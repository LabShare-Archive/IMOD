/*  IMOD VERSION 2.50
 *
 *  imod_io.c -- File I/O for imod.
 *
 *  The functions in imod_io provide an interface for load and saving imod
 *  models and images.  As models and images are loaded and save the global
 *  App structure is maintained.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.0  2002/09/27 20:15:24  rickg
Significant restructuring of the model io code
- io funtionality moved imod_menu_cb to this module
- added error reporting methods imodIOGetError*
- reverted LoadModel to orignally calling structure and added LoadModelFile
  to allow for cleaner handling of different cases especially w.r.t. freeing
  allocated variables and arguments
- still plenty left todo

Revision 3.1  2002/09/13 21:07:07  mast
Added argument to LoadModel so it can work from a filename passed in, and
removed old version of imod_io_image_reload

*/

// TODO:
//  * implement error codes for all functions
//
//  * review error handling
//
//  * decide whether this module should have access to the GUI (such as wprint)
//    this should only have access to the application data

#include <stdio.h>
#include <dia.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>
#include "mrcfiles.h"
#include "imod.h"
#include "xzap.h"
#include "imod_info.h"
#include "imodv.h"
#include "imod_io.h"

//  Module private functions
static void initModelData(Imod *newModel);
static char *datetime(void);
static void imod_undo_backup(void);
static void imod_finish_backup(void);
static int mapErrno(int errorCode);

char Statstring[128];  /* used in function show_staus(); */
char Inputstring[128];
char Imod_filename[256] = {0x00};

static char autosave_filename[266] = {0x00};
static char saved_filename[256] = {0x00};
static int last_checksum = -1;
static int lastError = IMOD_IO_SUCCESS;

extern int errno;

/* The VMS operating system doesn't allow filenames with the
 * charactor '#'
 */
#ifdef __vms
char *Imod_autosave_string = "_autosave";
#else
char *Imod_autosave_string = "#autosave#";
#endif

static char dummystring[] = "         ";

static char *datetime()
{
  time_t clock;
  char *string;
  clock = time(NULL);
  string = ctime(&clock);
  strncpy(dummystring, string + 11, 8);
  return dummystring;
}

int imod_model_changed(Imod *imodel)
{
  int checksum;

  checksum = imodChecksum(imodel);
  if (checksum == imodel->csum)
    return(0);
  return(1);
}

/* DNM: improved functions for making a backup (~) file: first one makes the
   backup and renames an older backup to ~~, then if the save is successful
   one finishes by deleting the ~~, or if unsuccessful, one undoes the moves
   so that both the current file and the backup are retained. */

/* Rename existing copy of saved file if it hasn't been done before for this
   file name */
static void imod_make_backup(char *filename)
{
  char *nfname1, *nfname2;

  if ((filename[0] != 0x00) && strcmp(filename, saved_filename)) {
    nfname1 = (char *)malloc(strlen(filename) + 2);
    nfname2 = (char *)malloc(strlen(filename) + 3);
    sprintf(nfname1, "%s~", filename);
    sprintf(nfname2, "%s~~", filename);
    rename(nfname1, nfname2);
    rename(filename, nfname1);
    free(nfname1);
    free(nfname2);
    strcpy(saved_filename, filename);
  }
}

/* undo the renaming of files to backup files if the save fails */
static void imod_undo_backup()
{
  char *nfname1, *nfname2;

  if (saved_filename[0]) {
    nfname1 = (char *)malloc(strlen(saved_filename) + 2);
    nfname2 = (char *)malloc(strlen(saved_filename) + 3);
    sprintf(nfname1, "%s~", saved_filename);
    sprintf(nfname2, "%s~~", saved_filename);
    rename(nfname1, saved_filename);
    rename(nfname2, nfname1);
    free(nfname1);
    free(nfname2);
    saved_filename[0] = 0x00;
  }
}

/* Or finish the making of a backup by cleaning up the ~~ file */
static void imod_finish_backup()
{
  char *nfname2;

  nfname2 = (char *)malloc(strlen(saved_filename) + 3);
  sprintf(nfname2, "%s~~", saved_filename);
  remove(nfname2);
  free(nfname2);
}

/* DNM: changed autosave functions to implement better cleanup of last autosave
   file when the nae changes, and to autosave only if the model is changed
   since the last autosave */

/* Clean up the autosave file if one was created */
void imod_cleanup_autosave(void)
{
  lastError = IMOD_IO_SUCCESS;

  if (autosave_filename[0])
    remove(autosave_filename);
  last_checksum = -1;
}

/* save autosave file */
/* DNM 6/15/01: add directory environment variable to allow save to local disk,
   and save even if in movie mode */
int imod_autosave(struct Mod_Model *mod)
{
  FILE *tfilep;
  int new_checksum;
  char *timestr;
  char *savedir = getenv("IMOD_AUTOSAVE_DIR");

  lastError = IMOD_IO_SUCCESS;
     
  if (!mod)
    return(-1);

  /*if (mod->mousemode == IMOD_MMOVIE)
    return(0); */

  new_checksum = imodChecksum(mod);
  if ((new_checksum == mod->csum) || (last_checksum == new_checksum))
    return(IMOD_IO_SUCCESS);

  imod_cleanup_autosave();

  if (savedir)
    sprintf(autosave_filename, "%s/%s%s", savedir, Imod_filename,
            Imod_autosave_string);
  else
    sprintf(autosave_filename, "%s%s", Imod_filename,
            Imod_autosave_string);
     
  remove(autosave_filename);
  tfilep = fopen(autosave_filename, "w");
  if (tfilep == NULL){
    wprint("Error: Can't open autosave file %s\n", autosave_filename);
    autosave_filename[0] = 0x00;
    return(-1);
  }


  if (imodWrite(Model, tfilep))
    wprint("Error: Autosave model file not saved. %s\n", 
           autosave_filename);
  else {
    timestr = datetime();
    wprint("Saved autosave file %s\n", timestr);
  }

  last_checksum = new_checksum;
  imod_draw_window();
  fclose(tfilep);
  return(IMOD_IO_SUCCESS);
}


int SaveModelQuit(Imod *mod)
{
  FILE *fout = NULL;
  int retval = 0;

  lastError = IMOD_IO_SUCCESS;
     
  /* DNM 8./4/01: Here and in next two functions, if imodv window is open,
     save the view if appropriate */
  if (!ImodvClosed)
    imodvAutoStoreView(Imodv);

  imod_make_backup(Imod_filename);

  fout = fopen(Imod_filename, "w+");

  if (fout == NULL){
    imod_undo_backup();
    return(mapErrno(errno));
  }

  retval = imodWrite(mod, fout);
  fclose(fout);

  imod_finish_backup();
  return(retval);
}


int SaveModel(struct Mod_Model *mod)
{
  FILE *fout = NULL;
  int retval;
  char *timestr;
     
  lastError = IMOD_IO_SUCCESS;

  if (!ImodvClosed)
    imodvAutoStoreView(Imodv);

  imod_make_backup(Imod_filename);

  fout = fopen(Imod_filename, "w+");

  if (fout == NULL){
    
    imod_undo_backup();
    retval = SaveasModel(mod);
    return(retval);
  }

  /* DNM: save the checksum and cleanup autosaves only if the save is
     successful, otherwise leave double backups and autosaves all over the
     the place */

  if (!imodWrite(mod, fout)){
    timestr = datetime();
    wprint("Done saving Model %s\n%s\n", timestr, Imod_filename);
    imod_finish_backup();
    mod->csum = imodChecksum(mod);
    imod_cleanup_autosave();
  }
  else
    wprint("Imod: Error Saving Model.");

  fclose(fout);
     

  return(0);
}


static void save_model(Widget w, XtPointer client, XtPointer call)
{
  Imod *mod = (Imod *)client;
  char *filename = (char *)call;
  FILE *fout = NULL;
  int retval = -1;

  lastError = IMOD_IO_SUCCESS;
     
  if ((filename == NULL) || (filename[0] == 0x00)) {
    wprint("No file selected. Model not saved.");
    XBell(imodDisplay(), 100);
    return;
  }

  if (!ImodvClosed)
    imodvAutoStoreView(Imodv);

  imod_make_backup(filename);
  fout = fopen(filename, "w");
  if (fout == NULL){
    wprint("Error: Couldn't open %s .  Model not saved.\n", filename);
    XBell(imodDisplay(), 100);
    imod_undo_backup();
    return;
  }

  mod->xmax = App->cvi->xsize;
  mod->ymax = App->cvi->ysize;
  mod->zmax = App->cvi->zsize;
     
  retval = imodWrite(mod, fout);
  fclose(fout);

  strcpy(Imod_filename, filename);
  free(filename); 
     
  if (!retval) {
    wprint("Done saving Model\n%s\n", Imod_filename);
    imod_finish_backup();
    mod->csum = imodChecksum(mod);
    imod_cleanup_autosave();
  }
  else {
    wprint("Imod: Error Saving Model.");
    XBell(imodDisplay(), 100);
  }
  MaintainModelName(mod);

}



int SaveasModel(struct Mod_Model *mod)
{
  lastError = IMOD_IO_SUCCESS;

  diaEasyFileAct("Model Save File", save_model, 
                 (XtPointer)mod);
  return(0);
}

/*  THIS IS NOT USED
    static void load_model(Widget w, XtPointer client, XtPointer call)
    {
    Imod **rmod = (Imod **)client;
    Imod *imod = NULL;
    char *filename = (char *)call;
    FILE *fout = NULL;

    if ((filename == NULL) || (filename[0] == '/0')) {
    wprint("No file selected for model load.\n");
    if (filename) free(filename);
    return;
    }

    imod = imodRead(filename);
    if (!imod){
    wprint("Error loading %s.\n", filename);
    return;
    }
    fclose (imod->file);
    free(filename);
    strcpy(Imod_filename, filename);
    *rmod = imod;
     
    imod->csum = imodChecksum(imod);
    if (!imod_autosave(imod)){
    filename = (char *)malloc(strlen(Imod_filename) +
    strlen(Imod_autosave_string) + 2);
    sprintf(filename, "%s%s", Imod_filename, Imod_autosave_string);
    remove(filename);
    if (filename)
    free(filename);
    }
    return;
    }
*/

Imod *LoadModel(FILE *mfin) {
  Imod *imod;
     
  lastError = IMOD_IO_SUCCESS;

  /* Added 2.00b7:
   * ifd files confuse imod library into thinking that
   * we have a model file.  So check for ifd file.
   * Also check for bad input.
   */
  if (mfin)
    if (imodImageFileDesc(mfin)) return(NULL);

  imod = (Imod *)malloc(sizeof(Imod));
  if (imod == NULL){
    wprint("IMOD Model Load: Not enough memory for model.");
    return(NULL);
  }
     
  imod->file = mfin;

  if (imodReadFile(imod)){
    if (imod)
      free(imod);
    return(NULL);
  }

  imod->csum = imodChecksum(imod);
  imod_cleanup_autosave();

  return(imod);
}


// LoadModelFile    attempts to load the specified file as IMOD model.
//
// Argumenents:
// filename     A character array pointer that specifies the file containing
//              the IMOD model.
//
// Returns:     a pointer to a newly allocated Imod object.
//
// If successful a pointer to a newly allocated model will be returned and the
// global variable Imod_filename will be copied from the requested string.
//
// If the load is unsucessful the return value is NULL, and the function cleans
// up after itself.  The failure reason can be accessed through the
// imod_io_error function.

Imod *LoadModelFile(char *filename) {
  FILE *fin;
  Imod *imod;
  
  lastError = IMOD_IO_SUCCESS;

  if (filename == NULL) {
    filename = dia_filename("Enter model file name to LOAD.");
    if (filename == NULL || filename[0] == '\0') {
      lastError = IMOD_IO_NO_FILE_SELECTED;
      show_status("File not selected. Model not loaded.");
      return((Imod *)NULL);
    }
    strcpy(Imod_filename, filename);
    free(filename);
    filename = NULL;
  }
  else {
    strcpy(Imod_filename, filename);
  }
  fin = fopen(Imod_filename, "r");

  if (fin == NULL) {
    lastError = mapErrno(errno);
    Imod_filename[0] = '\0';
    return((Imod *)NULL);
  }

  wprint("Loading... ");
  imod = LoadModel(fin);

  fclose(fin);
  imod->csum = imodChecksum(imod);
  return(imod);
}


/*************************************************************************/
/* Testing : future code */

#ifdef WHEN_IMAGE_IO_IS_ADDED

int SaveImage(struct ViewInfo *vi)
{
  char filename[256];
  FILE *fout = NULL;
  int retval = -1;
     
  /* get file name */
  filename[0] = 0x00;
  imod_getline("Enter filename for saved image."  , filename);
     
  /* open file */
  if (fout == NULL){
    show_status("Couldn't open File.");
    fprintf(stderr, "Imod: couldn't open %s\n",filename);
    return(-1);
  }
     
     
  retval = WriteImage(fout, vi, NULL);
  if (retval)
    show_status("Error saving image file.");
  else
    show_status("Done saving Image.");
     
  fclose( fout);
  return(retval);
}

int WriteImage(FILE *fout, struct ViewInfo *vi, struct LoadInfo *li)
{

  struct MRCheader hdata;
  double min, max, mean;
  int i, j, k;
  long xysize;
  int error= -1;
  int xmin, xmax, ymin, ymax, zmin, zmax;
  int xsize, xoff;

  xsize = li->xmax - li->xmin;

  mrc_head_new(&hdata, vi->xsize, vi->ysize, vi->zsize, 0);
  mrc_byte_mmm(&hdata, vi->idata);

  if (li){
    mrc_head_new(&hdata, xsize,
                 li->ymax - li->ymin, li->zmax - li->zmin, 0);
    xoff = li->xmin;
    xsize = li->xmax - li->xmin - 1;
    xmin = li->xmin;
    xmax = li->xmax;
    ymin = li->ymin;
    ymax = li->ymax;
    zmin = li->zmin;
    zmax = li->zmax;
  }else{
    xsize = vi->xsize;
    xmin = 0;
    xmax = vi->xsize - 1;
    ymin = 0;
    ymax = vi->ysize - 1;
    zmin = 0;
    zmax = vi->zsize - 1;
  }
	  
  mrc_head_label(&hdata, "Imod: Wrote sub image.   ");
  mrc_head_write(fout, &hdata);
     
  for (k = zmin; k <= zmax; k++){
	  
    for(j = ymin; j <= ymax; j++){
	       
      error = fwrite(&(vi->idata[k][(j * vi->xsize) + xmin]), 
                     1, xsize, fout);
    }
  }
  return(error);
}
#endif

unsigned char **imod_io_image_load(ImodImageFile *im,
                                   struct LoadInfo *li,
                                   void (*func)(char *))
{
  unsigned char **idata;
  struct MRCheader *mrchead;
  struct MRCheader savehdr;

  if (im->file == IIFILE_MRC){
    if (!im->fp)
      iiReopen(im);
    if (!im->fp) return NULL;
    mrchead = (struct MRCheader *)im->header;
    
    /* DNM: save and restore header after call to mrc_read_byte */
    savehdr = *mrchead;
    idata = (unsigned char **)mrc_read_byte(im->fp, mrchead, li, func);
    *mrchead = savehdr;
    
    return(idata);
  }
  
  {
    int i;
    int xsize, ysize, zsize, xysize;
    unsigned char **idata, *bdata;
    
    /* ignore load info for now. */
    xsize = im->nx;
    ysize = im->ny;
    zsize = im->nz;
    xysize = xsize * ysize;
    
    im->llx = 0;
    im->lly = 0;
    im->urx = xsize;
    im->ury = ysize;
    
    im->slope  = li->slope;
    im->offset = li->offset;
    im->imin   = li->imin;
    im->imax   = li->imax;
    im->axis   = li->axis;
    
    idata = (unsigned char **)malloc(zsize * sizeof(unsigned char *));
    if (!idata) return NULL;
    bdata = (unsigned char *)malloc(xysize * zsize * sizeof(unsigned char));
    if (!bdata) return NULL;
    for(i = 0; i < zsize; i++){
      idata[i] = bdata + (xysize * i);
      iiReadSectionByte(im, (char *)idata[i], i);
    }
    return(idata);
  }
}

/* DNM 9/12/02: deleted old version of imod_io_image_reload */
// openModel    opens a IMOD model specified by modeFilename
//
int openModel(char *modelFilename) {
  Imod *tmod;
  if (imod_model_changed(Model)){
    if (dia_ask("Save current model?")){
      if (SaveModel(App->cvi->imod)){
        return IMOD_IO_SAVE_ERROR;
      }
    }
  }
  imod_cleanup_autosave();

  /*	  mode = App->cvi->imod->mousemode; */
  tmod = (Imod *)LoadModelFile(modelFilename);

  if (tmod){
    initModelData(tmod);
  }
  else {
    return lastError;
  }

  return IMOD_IO_SUCCESS;
}


//  initModelData Initialize the internal model structures from a loaded model
//                file.
//
//  Description:
//  initModelData initializes the global ImodApp structure (App) with the new
//  model information and frees the old model data structures
//
//  FIXME this should really return a resut code, some of the functions called
//  appear that they can fail.
static void initModelData(Imod *newModel) {
	      
  /* DNM: for TrueColor, free colors of existing model and 
     allocate colors for the new one */
  if (App->rgba)
    free_object_colors(App->cvi->imod, 0, App->cvi->imod->objsize - 1);

  /* DNM: no longer causes a crash once we notify imodv of the
     new model */
  imodFree(App->cvi->imod);
	       
  Model = App->cvi->imod = newModel;
  App->cvi->black = App->cvi->imod->blacklevel;
  App->cvi->white = App->cvi->imod->whitelevel;

  /* DNM: unneeded with call to imod_cmap? */
  /* for (ob = 0; ob < newModel->objsize; ob++)
     if (App->depth <= 8)
     newModel->obj[ob].fgcolor = App->objbase - ob;
     else
     newModel->obj[ob].fgcolor = App->objbase + ob;
  */

  if (App->rgba)
    alloc_object_colors(newModel, 0, newModel->objsize - 1);
  else
    imod_cmap(App->cvi->imod);	  

  /* Notify imodv about the model */
  imodv_new_model(newModel);

  /* DNM: select the first color ramp; call xcramp_setlevels, 
     not xcramp_ramp, and set the sliders too */
  xcrampSelectIndex(App->cvi->cramp, 0);
  xcramp_setlevels(App->cvi->cramp, App->cvi->black,
                   App->cvi->white);
  imod_info_setbw(App->cvi->black, App->cvi->white);

  if (!App->cvi->fakeImage)
    ivwTransModel(App->cvi);
  /* DNM: check wild flags here, after any changes in model */
  ivwCheckWildFlag(newModel);
  /* DNM: model should never start out in model mode! */
  imod_set_mmode(IMOD_MMOVIE);

  /* DNM: needs to set object color of object 1 */
  imod_info_setobjcolor();
  imod_info_setocp();

  /* DNM: try eliminating this, since the setting of mode did it */
  /* imodDraw(App->cvi, IMOD_DRAW_MOD); */
  App->cvi->imod->xmax = App->cvi->xsize;
  App->cvi->imod->ymax = App->cvi->ysize;
  App->cvi->imod->zmax = App->cvi->zsize;

  MaintainModelName(App->cvi->imod);
}

//  createNewModel  create a new model data structure and optionally give the
//                  model a filename.
//
//  Description:
//  createNewModel first checks to see if the existing model needs to be saved
//  and cleaned up.  A new mode is then allocated, if the modelFilename
//  argument is non-NULL then the filename for the model is set.  Finally, the
//  App data structure is initialized for the new model.
int createNewModel(char *modelFilename) {
  int mode;
  int nChars;
  Iobj *obj;

  lastError = IMOD_IO_SUCCESS;

  if (imod_model_changed(Model)){
    if (dia_ask("Save old model?")){
      if (SaveModel(App->cvi->imod)){
        wprint("Error Saving Model. New model aborted.\n");
        lastError = IMOD_IO_NOMEM;
        return IMOD_IO_SAVE_ERROR;
      }
    } 
  }
  imod_cleanup_autosave();

  mode = App->cvi->imod->mousemode;
  /* DNM: for TrueColor, free colors of existing model and allocate
     a color for the new one */
  if (App->rgba)
    free_object_colors(App->cvi->imod, 0, 
                       App->cvi->imod->objsize - 1);
  imodFree(App->cvi->imod);
  
  //  Allocate the new model
  Model = imodNew();
  if(Model == NULL) {
    lastError = IMOD_IO_NOMEM;
    return lastError;
  }

  App->cvi->imod = Model;

  //  Copy the modelFilename into Imod_filename and then execute
  //  MaintainModelName to update the model structure and main window
  if(modelFilename != NULL) {
    nChars = strlen(modelFilename);
    if(nChars < sizeof(Imod_filename)) {
      strncpy(Imod_filename, modelFilename, nChars);
      Imod_filename[nChars] = '\0';
    }
    else {
      strncpy(Imod_filename, modelFilename, sizeof(Imod_filename) - 1);
      Imod_filename[sizeof(Imod_filename) - 1] = '\0';
    }

  }
  else {
    Imod_filename[0] = '\0';
  }
  MaintainModelName(App->cvi->imod);

  /* DNM: notify imodv of new model */
  imodv_new_model(Model);

  App->cvi->imod->xmax = App->cvi->xsize;
  App->cvi->imod->ymax = App->cvi->ysize;
  App->cvi->imod->zmax = App->cvi->zsize;
  imodNewObject(App->cvi->imod);

  /* DNM 5/16/02: if multiple image files, set time flag by default */
  obj = imodObjectGet(App->cvi->imod);
  if (App->cvi->nt)
    obj->flags |= IMOD_OBJFLAG_TIME;

  App->cvi->imod->mousemode = mode;
  imod_cmap(App->cvi->imod);
  if (App->rgba)
    alloc_object_colors(App->cvi->imod, 0, 0);

  /* Set the checksum to avoid save requests */
  App->cvi->imod->csum = imodChecksum(App->cvi->imod);
     
  imod_info_setobjcolor();
  imodDraw(App->cvi, IMOD_DRAW_MOD);

  imod_info_setocp();
  imod_object_edit_draw();    /* the setocp may not have called */
  ivwSetModelTrans(App->cvi);
  imod_cmap(App->cvi->imod);
  return IMOD_IO_SUCCESS;
}

// imodIOGetError   return the last error code
int imodIOGetError() {
  return lastError;
}

char *imodIOGetErrorString() {
  switch(lastError) {
  case IMOD_IO_SAVE_ERROR:
    return "Unable to save existing model";
  case IMOD_IO_DOES_NOT_EXIST:
    return "File does not exist";
  case IMOD_IO_NO_ACCESS_ERROR:
    return "Unable to access path or file, check permissions";
  case IMOD_IO_NO_FILE_SELECTED:
    return "File not selected";
  case IMOD_IO_NOMEM:
    return "Insufficient memory, try closing other programs";
  default:
    return "Unknown error";
  }
}
//  Map the errno codes to the IMOD_IO error codes
static int mapErrno(int errorCode) {
  switch(errorCode) {
  case ENOMEM:
    return IMOD_IO_NOMEM;

  case EACCES:
    return IMOD_IO_NO_ACCESS_ERROR;

  case ENOENT:
    return IMOD_IO_DOES_NOT_EXIST;

  default:
    return IMOD_IO_UNIMPLEMENTED_ERROR;
  }
  
}

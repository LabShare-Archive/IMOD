/*
 *  imod_io.c -- File I/O for imod.
 *
 *  The functions in imod_io provide an interface for load and saving imod
 *  models and images.  As models and images are loaded and save the global
 *  App structure is maintained.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

// TODO:
//  * implement error codes for all functions
//
//  * review error handling
//
//  * decide whether this module should have access to the GUI (such as wprint)
//    this should only have access to the application data

#include <sys/types.h>
#include <time.h>
#include <errno.h>
#include "imod_info.h"
#include <qfiledialog.h>
#include "imod.h"
#include "imod_display.h"
#include "xcramp.h"
#include "dia_qtutils.h"
#include "imod_info_cb.h"
#include "imodview.h"
#include "imodv.h"
#include "imod_io.h"
#include "imodv_views.h"
#include "preferences.h"

//  Module private functions
static void initModelData(Imod *newModel, bool keepBW);
static char *datetime(void);
static void imod_undo_backup(void);
static void imod_finish_backup(void);
static int mapErrno(int errorCode);

char Imod_filename[IMOD_FILENAME_SIZE] = {0x00};

static char autosave_filename[IMOD_FILENAME_SIZE + 10] = {0x00};
static char saved_filename[IMOD_FILENAME_SIZE] = {0x00};
static int last_checksum = -1;
static int lastError = IMOD_IO_SUCCESS;

static char *autosave_string = "#autosave#";

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
  QString qname, nfname1, nfname2;
  if ((filename[0] != 0x00) && strcmp(filename, saved_filename)) {
    QDir *curdir = new QDir();
    qname = QDir::convertSeparators(QString(filename));
    nfname1 = qname + "~";
    nfname2 = qname + "~~";
    curdir->remove(nfname2.latin1());
    curdir->rename(nfname1.latin1(), nfname2.latin1());
    curdir->rename(qname.latin1(), nfname1.latin1());
    strcpy(saved_filename, filename);
    delete curdir;
  }
}

/* undo the renaming of files to backup files if the save fails */
static void imod_undo_backup()
{
  QString qname, nfname1, nfname2;

  if (saved_filename[0]) {
    QDir *curdir = new QDir();
    qname = saved_filename;
    nfname1 = qname + "~";
    nfname2 = qname + "~~";
    curdir->remove(qname);
    curdir->rename(nfname1, qname);
    curdir->rename(nfname2, nfname1);
    saved_filename[0] = 0x00;
    delete curdir;
  }
}

/* Or finish the making of a backup by cleaning up the ~~ file */
static void imod_finish_backup()
{
  QDir *curdir = new QDir();
  QString nfname2 = QString(saved_filename) + "~~";
  curdir->remove(nfname2);
  delete curdir;
}

/* DNM: changed autosave functions to implement better cleanup of last autosave
   file when the nae changes, and to autosave only if the model is changed
   since the last autosave */

/* Clean up the autosave file if one was created */
void imod_cleanup_autosave(void)
{
  lastError = IMOD_IO_SUCCESS;

  if (autosave_filename[0]) {
    QDir *curdir = new QDir();
    curdir->remove(QString(autosave_filename));
    delete curdir;
  }
  last_checksum = -1;
}

/* save autosave file */
/* DNM 6/15/01: add directory environment variable to allow save to local disk,
   and save even if in movie mode */
int imod_autosave(struct Mod_Model *mod)
{
  FILE *tfilep;
  int new_checksum, i;
  char *timestr;
  char *convname;
  QString savedir = ImodPrefs->autosaveDir();

  lastError = IMOD_IO_SUCCESS;
     
  if (!mod)
    return(-1);

  /*if (mod->mousemode == IMOD_MMOVIE)
    return(0); */

  new_checksum = imodChecksum(mod);
  if ((new_checksum == mod->csum) || (last_checksum == new_checksum))
    return(IMOD_IO_SUCCESS);

  // Clean up with the existing name
  imod_cleanup_autosave();

  if (!savedir.isEmpty()) {
    /* Strip the path off the name */
    timestr = Imod_filename;
    for (i = 0; Imod_filename[i]; i++)
      if (Imod_filename[i] == '/')
        timestr = &(Imod_filename[i]) + 1;
        
    sprintf(autosave_filename, "%s/%s%s", savedir.latin1(), timestr, 
	    autosave_string);
      
  } else
    sprintf(autosave_filename, "%s%s", Imod_filename, autosave_string);
     
  // Then clean up with the new name
  imod_cleanup_autosave();
  convname = strdup
    ((QDir::convertSeparators(QString(autosave_filename))).latin1());

  tfilep = fopen(convname, "wb");
  if (tfilep == NULL){
    wprint("Error: Can't open autosave file %s\n", convname);
    autosave_filename[0] = 0x00;
    free(convname);
    return(-1);
  }


  if (imodWrite(Model, tfilep))
    wprint("Error: Autosave model file not saved. %s\n", 
           convname);
  else {
    timestr = datetime();
    wprint("Saved autosave file %s\n", timestr);
  }

  last_checksum = new_checksum;
  imod_draw_window();
  fclose(tfilep);
  free(convname);
  return(IMOD_IO_SUCCESS);
}

/* DNM 12/2/02: eliminated SaveModelQuit - SaveModel is fine and allows user
   to Save As if no model file is defined yet */


int SaveModel(struct Mod_Model *mod)
{
  FILE *fout = NULL;
  int retval;
  char *timestr;
     
  lastError = IMOD_IO_SUCCESS;

  /* DNM 8./4/01: -> 6/26/03: Here and in next functions, if imodv window is 
     closed, make sure Imodv->imod is set, and save the view if appropriate */
  if (ImodvClosed)
    Imodv->imod = mod;
  imodvAutoStoreView(Imodv);

  /* DNM 2/23/03: fopen crashes with empty filename in Windows, so just test 
     for that */
  if (Imod_filename[0] == 0x00) {
    retval = SaveasModel(mod);
    return(retval);
  }

  imod_make_backup(Imod_filename);

  fout = fopen((QDir::convertSeparators(QString(Imod_filename))).latin1(),
    "wb+");

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
    wprint("Done saving Model %s\n%s\n", timestr, (QDir::convertSeparators
        (QString(Imod_filename))).latin1());
    imod_finish_backup();
    mod->csum = imodChecksum(mod);
    imod_cleanup_autosave();
  }
  else {
    wprint("3dmod: Error Saving Model.");
    lastError = IMOD_IO_SAVE_ERROR;
  }

  fclose(fout);
     

  return(lastError);
}


/* DNM 12/2/02: made this call dia_filename, which returns directly, and moved
   code from save_model into here after filename is gotten */
int SaveasModel(struct Mod_Model *mod)
{
  QString qname;
  FILE *fout = NULL;
  int retval = -1;

  lastError = IMOD_IO_SUCCESS;
     
  qname = QFileDialog::getSaveFileName(QString::null, QString::null, 0, 0, 
                                       "Model Save File:");
  if (qname.isEmpty()) {
    // OLD NOTE ABOUT dia_filename
    /* this dialog doesn't return if no file selected, so this is a cancel
       and needs no message */
    /* wprint("\aNo file selected. Model not saved."); */
    lastError = IMOD_IO_SAVE_CANCEL;
    return IMOD_IO_SAVE_CANCEL;
  }

  if (ImodvClosed)
    Imodv->imod = mod;
  imodvAutoStoreView(Imodv);

  imod_make_backup((char *)qname.latin1());
  fout = fopen((QDir::convertSeparators(qname)).latin1(), "wb");
  if (fout == NULL){
    wprint("\aError: Couldn't open %s .  Model not saved.\n", qname.latin1());
    imod_undo_backup();
    lastError = mapErrno(errno);
    return lastError;
  }

  mod->xmax = App->cvi->xUnbinSize;
  mod->ymax = App->cvi->yUnbinSize;
  mod->zmax = App->cvi->zUnbinSize;
  mod->xybin = App->cvi->xybin;
  mod->zbin = App->cvi->zbin;
     
  retval = imodWrite(mod, fout);
  fclose(fout);

  strcpy(Imod_filename, qname.latin1());
     
  if (!retval) {
    wprint("Done saving Model\n%s\n", 
      (QDir::convertSeparators(qname)).latin1());
    imod_finish_backup();
    mod->csum = imodChecksum(mod);
    imod_cleanup_autosave();
  }
  else {
    wprint("\a3dmod: Error Saving Model.");
    lastError = IMOD_IO_SAVE_ERROR;
    return lastError;
  }
  MaintainModelName(mod);
  imodvSetCaption();

  return(0);
}


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
    wprint("3dmod Model Load: Not enough memory for model.");
    return(NULL);
  }
     
  imod->file = mfin;

  if (imodReadFile(imod)){
    if (imod)
      free(imod);
    return(NULL);
  }

  imodvViewsInitialize(imod);
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
  int nChars;
  QString qname;
  char *filter[] = {"Model files (*.*mod *.fid *.seed)"};
  
  lastError = IMOD_IO_SUCCESS;

  /* DNM 2/24/03: change to not destroy existing Imod_filename */
  if (filename == NULL) {
    qname = diaOpenFileName(NULL, "Select Model file to LOAD", 1, filter);
  
    if (qname.isEmpty()) {
      lastError = IMOD_IO_READ_CANCEL;
      /*  show_status("File not selected. Model not loaded."); */
      return((Imod *)NULL);
    }

  } else {
    qname = filename;
  }

  fin = fopen((QDir::convertSeparators(qname)).latin1(), "rb");

  if (fin == NULL) {
    lastError = mapErrno(errno);
    return((Imod *)NULL);
  }

  wprint("Loading... ");
  imod = LoadModel(fin);
  fclose(fin);

  /* DNM 2/24/03: catch error at this point, and translate it to a read 
    error if unidentified */
  /* DNM 9/12/03: eliminate checksum, protect from overrunning filename */
  if (imod) {
    nChars = strlen(qname.latin1());
    if (nChars >= IMOD_FILENAME_SIZE)
      nChars = IMOD_FILENAME_SIZE - 1;
    strncpy(Imod_filename, qname.latin1(), nChars);
    Imod_filename[nChars] = 0x00;
  } else {
    lastError = mapErrno(errno);
    if (lastError == IMOD_IO_UNIMPLEMENTED_ERROR)
      lastError = IMOD_IO_READ_ERROR;
  }
  return(imod);
}


/* DNM 9/12/02: deleted old version of imod_io_image_reload */
// openModel    opens a IMOD model specified by modeFilename
//
int openModel(char *modelFilename, bool keepBW) {
  Imod *tmod;
  int err;
  int answer;

  if (imod_model_changed(Model)){
    answer = dia_choice("Save current model?", "Yes", "No", "Cancel");
    if (answer == 1) {
      if ((err = SaveModel(App->cvi->imod))){
        return err;
      }
    } else if (answer != 2)
      return IMOD_IO_SAVE_CANCEL;
  }
  imod_cleanup_autosave();

  /*	  mode = App->cvi->imod->mousemode; */
  tmod = (Imod *)LoadModelFile(modelFilename);

  /* DNM 9/12/03: need to check checksum here, not in load model routines */
  if (tmod){
    initModelData(tmod, keepBW);
    tmod->csum = imodChecksum(tmod);
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
static void initModelData(Imod *newModel, bool keepBW) {
	      
  /* DNM 1/23/03: no longer free or allocate object colors */
  /* DNM: no longer causes a crash once we notify imodv of the
     new model */
  imodDelete(App->cvi->imod);
	       
  Model = App->cvi->imod = newModel;

  /* DNM 6/3/04: avoid two draws by keeping levels in the first place */
  if (!keepBW) {
    App->cvi->black = App->cvi->imod->blacklevel;
    App->cvi->white = App->cvi->imod->whitelevel;
  }

  /* DNM 6/3/04: removed commented out code on setting object colors */
  if (!App->rgba)
    imod_cmap(App->cvi->imod);	  

  /* set up model name and notify imodv about the model */
  MaintainModelName(App->cvi->imod);
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
  App->cvi->imod->xmax = App->cvi->xUnbinSize;
  App->cvi->imod->ymax = App->cvi->yUnbinSize;
  App->cvi->imod->zmax = App->cvi->zUnbinSize;
  App->cvi->imod->xybin = App->cvi->xybin;
  App->cvi->imod->zbin = App->cvi->zbin;

}

//  createNewModel  create a new model data structure and optionally give the
//                  model a filename.
//
//  Description:
//  createNewModel first checks to see if the existing model needs to be saved
//  and cleaned up.  A new model is then allocated, if the modelFilename
//  argument is non-NULL then the filename for the model is set.  Finally, the
//  App data structure is initialized for the new model.
int createNewModel(char *modelFilename) {
  int mode;
  int nChars;
  int err, answer;
  Iobj *obj;

  lastError = IMOD_IO_SUCCESS;

  if (imod_model_changed(Model)){
    answer = dia_choice("Save current model?", "Yes", "No", "Cancel");
    if (answer == 1) {

      /* DNM 12/2/02: it comes back with a valid error now, so give message
         only if not a cancel, and return actual error */
      if ((err = SaveModel(App->cvi->imod))){
        if (err != IMOD_IO_SAVE_CANCEL)
          wprint("\aError Saving Model. New model aborted.\n");
        lastError = err;
        return err;
      }
    } else if (answer != 2)
      return IMOD_IO_SAVE_CANCEL;
  }
  imod_cleanup_autosave();

  mode = App->cvi->imod->mousemode;

  /* DNM 1/23/03: no longer free or allocate object colors */
  imodDelete(App->cvi->imod);
  
  //  Allocate the new model
  Model = imodNew();
  if(Model == NULL) {
    lastError = IMOD_IO_NOMEM;
    return lastError;
  }

  App->cvi->imod = Model;
  imodvViewsInitialize(Model);

  //  Copy the modelFilename into Imod_filename and then execute
  //  MaintainModelName to update the model structure and main window
  if(modelFilename != NULL) {
    nChars = strlen(modelFilename);
    if (nChars >= IMOD_FILENAME_SIZE)
      nChars = IMOD_FILENAME_SIZE - 1;
    strncpy(Imod_filename, modelFilename, nChars);
    Imod_filename[nChars] = '\0';
  }
  else {
    Imod_filename[0] = '\0';
  }
  MaintainModelName(App->cvi->imod);

  /* DNM: notify imodv of new model */
  imodv_new_model(Model);

  App->cvi->imod->xmax = App->cvi->xUnbinSize;
  App->cvi->imod->ymax = App->cvi->yUnbinSize;
  App->cvi->imod->zmax = App->cvi->zUnbinSize;
  App->cvi->imod->xybin = App->cvi->xybin;
  App->cvi->imod->zbin = App->cvi->zbin;
  imodNewObject(App->cvi->imod);

  /* DNM 5/16/02: if multiple image files, set time flag by default */
  obj = imodObjectGet(App->cvi->imod);
  if (App->cvi->nt)
    obj->flags |= IMOD_OBJFLAG_TIME;

  App->cvi->imod->mousemode = mode;
  imod_cmap(App->cvi->imod);

  imod_info_setobjcolor();
  imodDraw(App->cvi, IMOD_DRAW_MOD);

  imod_info_setocp();
  ivwSetModelTrans(App->cvi);
  imod_cmap(App->cvi->imod);

  /* Set the checksum to avoid save requests */
  App->cvi->imod->csum = imodChecksum(App->cvi->imod);
     
  return IMOD_IO_SUCCESS;
}

/*
 * Routine to load or reload image into non-cache buffers
 */
unsigned char **imod_io_image_load(struct ViewInfo *vi)
{
  ImodImageFile *im = vi->image;
  struct LoadInfo *li = vi->li;
  unsigned char **idata;
  struct MRCheader *mrchead;
  struct MRCheader savehdr;
  int i;
  int pixsize;
  unsigned char *bdata;
  QString message;

  if (!im->fp)
    iiReopen(im);
  if (!im->fp)
    return NULL;

  /* Loading unbinned, non RGB MRC, file */
  if (im->file == IIFILE_MRC && !vi->rawImageStore && 
      vi->xybin * vi->zbin == 1 && li->mirrorFFT <= 0) {
    mrchead = (struct MRCheader *)im->header;

    imodStartAutoDumpCache();
    vi->loadingImage = 1;
    /* DNM: save and restore header after call to mrc_read_byte */
    savehdr = *mrchead;
    idata = mrc_read_byte(im->fp, mrchead, li, imod_imgcnt);
    *mrchead = savehdr;
    vi->loadingImage = 0;
    
    return(idata);
  }
  
  /* reading non-MRC or RGB files */

  /* print load status */
  wprint("Image size %d x %d, %d sections.\n", vi->xsize, vi->ysize,
         vi->zsize);
  
  /* A lot of stuff is already set and can be eliminated */

  pixsize = ivwGetPixelBytes(vi->rawImageStore);
    
  /* Just like the standard MRC load, this loads in native orientation
     and flips afterward if needed, so caller must be sure image axis is 3 */

  idata = mrcGetDataMemory(li, vi->xysize, vi->zsize, pixsize);
  if (!idata)
    return NULL;

  vi->loadingImage = 1;
  for (i = 0; i < vi->zsize; i++) {
    message.sprintf("Reading Image # %3.3d", i+1); 
    imod_imgcnt((char *)message.latin1());
    ivwReadBinnedSection(vi, (char *)idata[i], i + li->zmin);
  }
  imod_imgcnt("");
  vi->loadingImage = 0;
  return(idata);
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
    fprintf(stderr, "3dmod: couldn't open %s\n",filename);
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
	  
  mrc_head_label(&hdata, "3dmod: Wrote sub image.   ");
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

/*
$Log$
Revision 4.16  2005/03/20 19:55:36  mast
Eliminating duplicate functions

Revision 4.15  2004/11/04 17:01:31  mast
Changes for loading FFTs with internal mirroring

Revision 4.14  2004/10/22 22:16:18  mast
Added call to start cache dumping for mrc_read_byte data load

Revision 4.13  2004/09/10 02:31:03  mast
replaced long with int

Revision 4.12  2004/06/04 03:17:03  mast
Added argument to openModel to keep black/white level

Revision 4.11  2004/06/01 01:31:34  mast
Remove extern int errno statement - there is an include for it

Revision 4.10  2004/01/06 16:53:35  mast
Use proper test for when to use mrc_read_byte

Revision 4.9  2004/01/05 18:01:53  mast
Implemented full loading of non-cache images through ivwReadBinnedSection

Revision 4.8  2003/09/13 04:36:06  mast
Protected the global array with the model filename from being overrun.
Got the model checksum at the right points to prevent unneeded requests
to save the model; and eliminated unneeded computations of model checksum.

Revision 4.7  2003/06/27 19:26:36  mast
Changes to implement new view scheme when reading, saving, or making a
new model

Revision 4.6  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.5  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.4  2003/03/03 22:14:34  mast
cleanup

Revision 4.3  2003/02/28 18:33:44  mast
fix = to == in if statement

Revision 4.2  2003/02/27 19:37:26  mast
Changes for windows version in filename handling

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.9  2003/01/27 03:22:17  mast
Stripped model filename of directories when appending to an autosave dir

Revision 1.1.2.8  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.7  2003/01/23 20:07:45  mast
rely on imod_infor_setocp updating dialogs

Revision 1.1.2.6  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 1.1.2.5  2003/01/06 15:52:16  mast
changes for Qt version of slicer

Revision 1.1.2.4  2002/12/19 04:37:13  mast
Cleanup of unused global variables and defines

Revision 1.1.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.2  2002/12/17 18:41:49  mast
Changes to get imodv to set file name correctly in window title

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 4.1.2.2  2002/12/09 17:42:32  mast
remove include of zap

Revision 4.1.2.1  2002/12/05 16:30:00  mast
Add include of imod_object_edit.h

Revision 4.2  2002/12/03 16:08:28  mast
Switched SavasModel to using dia_filename so that there would be no callback
returns from file dialogs (thus preventing multiple file dialogs).  Refined
the error return protocols and minimized error messages if user cancelled;
also let user cancel as well as yes/no when asked whether to save current
model upon new model or model load.

Revision 4.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

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

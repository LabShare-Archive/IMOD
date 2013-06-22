/*
 *  imod_io.cpp -- File I/O for imod.
 *
 *  The functions in imod_io provide an interface for loading and saving imod
 *  models and images.  As models and images are loaded and saved the global
 *  App structure is maintained.
 *
 *  Original author: James Kremer
 *  Revised by: Rick Gaudette and David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
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
#include <qdir.h>
#include "info_setup.h"
#include "imod.h"
#include "display.h"
#include "xcramp.h"
#include "dia_qtutils.h"
#include "info_cb.h"
#include "imodview.h"
#include "imodplug.h"
#include "imodv.h"
#include "sslice.h"
#include "imod_io.h"
#include "mv_views.h"
#include "vertexbuffer.h"
#include "utilities.h"
#include "preferences.h"

//  Module private functions
static const char *datetime(void);
static void imod_undo_backup(void);
static void imod_finish_backup(void);
static void imod_make_backup(const char *filename);
static int mapErrno(int errorCode);
static void setSavedModelState(Imod *mod);
static void restoreSavedModelState(Imod *mod);
static int writeModel(Imod *mod, FILE *fout, QString qname);
static Imod *LoadModelFile(const char *filename) ;

char Imod_filename[IMOD_FILENAME_SIZE] = {0x00};

static char autosave_filename[IMOD_FILENAME_SIZE + 10] = {0x00};
static char saved_filename[IMOD_FILENAME_SIZE] = {0x00};
static int last_checksum = -1;
static int lastError = IMOD_IO_SUCCESS;

static const char *autosave_string = "#autosave#";

static char dummystring[] = "         ";

static const char *datetime()
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
  if (!imodel)
    return(0);

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
static void imod_make_backup(const char *filename)
{
  QString qname, nfname1, nfname2;
  if ((filename[0] != 0x00) && strcmp(filename, saved_filename)) {
    QDir *curdir = new QDir();
    qname = QDir::convertSeparators(QString(filename));
    nfname1 = qname + "~";
    nfname2 = qname + "~~";
    curdir->remove(LATIN1(nfname2));
    curdir->rename(LATIN1(nfname1), LATIN1(nfname2));
    curdir->rename(LATIN1(qname), LATIN1(nfname1));
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
int imod_autosave(Imod *mod)
{
  FILE *tfilep;
  int new_checksum, i;
  const char *timestr;
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
        
    sprintf(autosave_filename, "%s/%s%s", LATIN1(savedir), timestr, 
	    autosave_string);
      
  } else
    sprintf(autosave_filename, "%s%s", Imod_filename, autosave_string);
     
  // Then clean up with the new name
  imod_cleanup_autosave();
  convname = strdup
    (LATIN1(QDir::convertSeparators(QString(autosave_filename))));

  tfilep = fopen(convname, "wb");
  if (tfilep == NULL){
    wprint("Error: Can't open autosave file %s\n", convname);
    autosave_filename[0] = 0x00;
    free(convname);
    return(-1);
  }

  // Set variables for saved model and try to save
  setSavedModelState(Model);
  if (imodWrite(Model, tfilep))
    wprint("Error: Autosave model file not saved. %s\n", convname);
  else {
    timestr = datetime();
    wprint("Saved autosave file %s\n", timestr);
  }
  restoreSavedModelState(Model);

  // 9/28/11: get a new checksum, it may have just changed
  last_checksum = imodChecksum(mod);
  imod_draw_window();
  fclose(tfilep);
  free(convname);
  return(IMOD_IO_SUCCESS);
}

/*
 * Return name of valid file with saved model: either the main filename if it is
 * current, or the autosave file, doing an autosave if necessary
 */
char *currentSavedModelFile(void)
{
  Imod *mod = App->cvi->imod;
  int new_checksum = imodChecksum(mod);
  if (new_checksum == mod->csum && Imod_filename[0])
    return Imod_filename;
  if (new_checksum == last_checksum && autosave_filename[0])
    return autosave_filename;
  if (imod_autosave(mod) == IMOD_IO_SUCCESS)
    return autosave_filename;
  return NULL;
}

/* DNM 12/2/02: eliminated SaveModelQuit - SaveModel is fine and allows user
   to Save As if no model file is defined yet */

/*
 * SaveModel - saves the given model to the established filename in 
 * Imod_filename or calls SaveasModel if there is no name or there is an error
 * opening the file under that name
 */
int SaveModel(Imod *mod)
{
  FILE *fout = NULL;
  int retval;

  lastError = IMOD_IO_SUCCESS;
     
  /* DNM 2/23/03: fopen crashes with empty filename in Windows, so just test 
     for that */
  if (Imod_filename[0] == 0x00) {
    retval = SaveasModel(mod);
    return(retval);
  }

  imod_make_backup(Imod_filename);

  fout = fopen(LATIN1(QDir::convertSeparators(QString(Imod_filename))), "wb+");

  if (fout == NULL){
    imod_undo_backup();
    retval = SaveasModel(mod);
    return(retval);
  }

  retval = writeModel(mod, fout, QString(Imod_filename));

  // Need explicit call here to manage Reload Model; other places that change 
  // reloadable all call MaintainModelName
  if (!retval)
    ImodInfoWin->manageMenus();

  return(retval);
}


/* DNM 12/2/02: made this call dia_filename, which returns directly, and moved
   code from save_model into here after filename is gotten */

/*
 * SaveasModel saves the given model to a file after acquiring a name from user
 */
int SaveasModel(Imod *mod)
{
  QString qname;
  FILE *fout = NULL;
  int retval = -1;

  lastError = IMOD_IO_SUCCESS;
     
  qname = imodPlugGetSaveName(NULL, "Model Save File:");
  if (qname.isEmpty()) {
    // OLD NOTE ABOUT dia_filename
    /* this dialog doesn't return if no file selected, so this is a cancel
       and needs no message */
    /* wprint("\aNo file selected. Model not saved."); */
    lastError = IMOD_IO_SAVE_CANCEL;
    return IMOD_IO_SAVE_CANCEL;
  }

  imod_make_backup(LATIN1(qname));
  fout = fopen(LATIN1(QDir::convertSeparators(qname)), "wb");
  if (fout == NULL){
    wprint("\aError: Couldn't open %s .  Model not saved.\n", LATIN1(qname));
    imod_undo_backup();
    lastError = mapErrno(errno);
    return lastError;
  }

  retval = writeModel(mod, fout, qname);

  if (!retval) {
    setImod_filename(LATIN1(qname));
    MaintainModelName(mod);
    imodvSetCaption();
  }

  return(retval);
}

/*
 * Perform common functions pre and post writing of model for Save and Saveas
 */
static int writeModel(Imod *mod, FILE *fout, QString qname)
{
  const char *timestr;
  int retval;

  /* DNM 8/4/01: -> 6/26/03: If imodv window is 
     closed, make sure Imodv->imod is set, and save the view if appropriate */
  if (ImodvClosed)
    Imodv->imod = mod;
  imodvAutoStoreView(Imodv);

  // Save other state variables then write and close file
  setSavedModelState(mod);
  retval = imodWrite(mod, fout);
  fclose(fout);
  restoreSavedModelState(mod);

  // Issue messages and perform post tasks
  /* DNM: save the checksum and cleanup autosaves only if the save is
     successful, otherwise leave double backups and autosaves all over the
     the place */
  if (!retval) {
    timestr = datetime();
    wprint("Done saving model %s\n%s\n", timestr,
           LATIN1(QDir::convertSeparators(qname)));
    imod_finish_backup();
    mod->csum = imodChecksum(mod);
    if (imodDebug('C'))
      wprint("writeModel set checksum %d\n", mod->csum);
    imod_cleanup_autosave();
    App->cvi->reloadable = 1;
  } else {
    wprint("\aError saving model.");
    lastError = IMOD_IO_SAVE_ERROR;
    return lastError;
  }
  return(0);
}

/*
 * Set various items in the model before saving that are needed only in the 
 * saved model, not while running 3dmod.  This should be called before every
 * imodWrite.
 */ 
static void setSavedModelState(Imod *mod)
{

  // Do not save rotated models, no one knows what they are.  Save as flipped for now
  utilExchangeFlipRotation(mod, ROTATION_TO_FLIP);
  mod->blacklevel = App->cvi->black;
  mod->whitelevel = App->cvi->white;
  mod->xmax = App->cvi->xUnbinSize;
  mod->ymax = App->cvi->yUnbinSize;
  mod->zmax = App->cvi->zUnbinSize;
  mod->xybin = App->cvi->xybin;
  mod->zbin = App->cvi->zbin;
  setOrClearFlags(&mod->flags, IMODF_NEW_TO_3DMOD, 0);
}

// Restore the original maxes and get it back to rotated state if flipped
static void restoreSavedModelState(Imod *mod)
{
  mod->xmax = App->cvi->xsize;
  mod->ymax = App->cvi->ysize;
  mod->zmax = App->cvi->zsize;
  utilExchangeFlipRotation(mod, FLIP_TO_ROTATION);
}

/* 
 * LoadModel  allocates a model structure and reads in a model once the file
 * is opened on mfin.  Called by LoadModelFile and from imod on startup
 */
Imod *LoadModel(FILE *mfin)
{
  Imod *imod;
     
  lastError = IMOD_IO_SUCCESS;

  /* Added 2.00b7:
   * ifd files confuse imod library into thinking that
   * we have a model file.  So check for ifd file.
   * Also check for bad input.
   */
  if (mfin)
    if (imodImageFileDesc(mfin)) return(NULL);

  imod = imodNew();
  if (imod == NULL){
    wprint("3dmod Model Load: Not enough memory for model.");
    return(NULL);
  }
     
  imod->file = mfin;

  if (imodReadFile(imod)){
    if (imod) {
      if (imod->view)
        free(imod->view);
      free(imod);
    }
    return(NULL);
  }

  imodvViewsInitialize(imod);
  imod_cleanup_autosave();
  App->cvi->reloadable = 1;

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
// 10/14/05: Called only from openModel

static Imod *LoadModelFile(const char *filename) 
{
  FILE *fin;
  Imod *imod;
  QString qname;
  const char *filter[] = {"Model files (*.*mod *.*fid *.*seed)"};
  
  lastError = IMOD_IO_SUCCESS;

  /* DNM 2/24/03: change to not destroy existing Imod_filename */
  if (filename == NULL) {
    qname = utilOpenFileName(NULL, "Select Model file to LOAD", 1, filter);
  
    if (qname.isEmpty()) {
      lastError = IMOD_IO_READ_CANCEL;
      /*  show_status("File not selected. Model not loaded."); */
      return((Imod *)NULL);
    }

  } else {
    qname = filename;
  }

  fin = fopen(LATIN1(QDir::convertSeparators(qname)), "rb");

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
    setImod_filename(LATIN1(qname));
  } else {
    lastError = mapErrno(errno);
    if (lastError == IMOD_IO_UNIMPLEMENTED_ERROR)
      lastError = IMOD_IO_READ_ERROR;
  }
  return(imod);
}

/* DNM 9/12/02: deleted old version of imod_io_image_reload */

// openModel    opens a IMOD model specified by modelFilename after checking
//              whether the current model should be saved
// set keepBW to retain existing black/white values 
// set saveAs to call Saveas instead of Save if current model is being saved
//
int openModel(const char *modelFilename, bool keepBW, bool saveAs) 
{
  Imod *tmod;
  int err;
  int answer;

  if (imod_model_changed(Model)) {
    answer = dia_choice
      ((char *)(saveAs ? "The model has changed.  Save to new file before "
                "reloading?" : "Save current model?"), "Yes", "No", "Cancel");
    if (answer == 1) {
      if (saveAs) {
        if ((err = SaveasModel(App->cvi->imod)))
          return err;
      } else {
        if ((err = SaveModel(App->cvi->imod)))
          return err;
      }
      
    } else if (answer != 2)
      return IMOD_IO_SAVE_CANCEL;
  }
  imod_cleanup_autosave();

  tmod = LoadModelFile(modelFilename);

  if (tmod) {
    initReadInModelData(tmod, keepBW);
  } else {
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
//  called from openModel and initializeFlipAndModel
//
void initReadInModelData(Imod *newModel, bool keepBW) 
{
  ViewInfo *vi = App->cvi;

  /* DNM 1/23/03: no longer free or allocate object colors */
  /* DNM: no longer causes a crash once we notify imodv of the new model.
     10/13/05: but we have to invalidate imodv's model until all is ready */
  imodv_new_model(NULL);
  vbCleanupVBD(vi->imod);
  imodDelete(vi->imod);
	       
  Model = vi->imod = newModel;

  /* DNM 6/3/04: avoid two draws by keeping levels in the first place */
  if (!keepBW && !(newModel->flags & IMODF_NEW_TO_3DMOD)) {
    ivwSetBlackWhiteFromModel(vi);
  }

  /* DNM 6/3/04: removed commented out code on setting object colors */
  if (!App->rgba)
    imod_cmap(vi->imod);	  

  /* set up model name and make sure model is on */
  MaintainModelName(vi->imod);
  newModel->drawmode = 1;

  /* Scale model then notify imodv about the model.  5/3/11: Do this before setting
   black/white because that will cause a draw.  12/31/12: do it unconditionally for
   fake image too, that will handle setting up the trans and flipping */
  ivwTransModel(vi);
  utilExchangeFlipRotation(vi->imod, FLIP_TO_ROTATION);
  imodv_new_model(newModel);

  /* DNM: select the first color ramp; call xcramp_setlevels, 
     not xcramp_ramp, and set the sliders too */
  xcrampSelectIndex(vi->cramp, 0);
  xcramp_setlevels(vi->cramp, vi->black, vi->white);
  imod_info_setbw(vi->black, vi->white);

  /* DNM: check wild flags here, after any changes in model */
  ivwCheckWildFlag(newModel);
  vi->imod->ctime = vi->curTime;

  /* DNM: model should never start out in model mode! */
  imod_set_mmode(IMOD_MMOVIE);
  newModel->csum = imodChecksum(newModel);
  if (imodDebug('C'))
    wprint("initReadInModelData set checksum %d\n", newModel->csum);

  /* DNM: needs to set object color of object 1 */
  imod_info_setobjcolor();
  imod_info_setocp();
  slicerNewTime(true);
  
  if (!vi->doingInitialLoad)
    imodPlugCall(vi, 0, IMOD_REASON_NEWMODEL);
}

//  createNewModel  create a new model data structure and optionally give the
//                  model a filename.
//
//  Description:
//  createNewModel first checks to see if the existing model needs to be saved
//  and cleaned up.  A new model is then allocated, if the modelFilename
//  argument is non-NULL then the filename for the model is set.  Finally, the
//  App data structure is initialized for the new model.
//  Called from imod_menu and imod_client_message and imodview
//
int createNewModel(const char *modelFilename)
{
  ViewInfo *vi = App->cvi;
  int mode;
  int err, answer;

  lastError = IMOD_IO_SUCCESS;

  if (!vi->doingInitialLoad) {
    if (imod_model_changed(Model)){
      answer = dia_choice("Save current model?", "Yes", "No", "Cancel");
      if (answer == 1) {
        
        /* DNM 12/2/02: it comes back with a valid error now, so give message
           only if not a cancel, and return actual error */
        if ((err = SaveModel(vi->imod))){
          if (err != IMOD_IO_SAVE_CANCEL)
            wprint("\aError Saving Model. New model aborted.\n");
          lastError = err;
          return err;
        }
      } else if (answer != 2)
        return IMOD_IO_SAVE_CANCEL;
    }
    imod_cleanup_autosave();
  }

  mode = vi->doingInitialLoad ? IMOD_MMOVIE : vi->imod->mousemode;

  /* DNM 1/23/03: no longer free or allocate object colors */
  vbCleanupVBD(vi->imod);
  imodDelete(vi->imod);
  
  //  Allocate the new model
  Model = imodNew();
  if (Model == NULL) {
    lastError = IMOD_IO_NOMEM;
    return lastError;
  }

  vi->imod = Model;

  //  Copy the modelFilename into Imod_filename and then execute
  //  MaintainModelName to update the model structure and main window
  if (modelFilename != NULL && modelFilename[0] != 0x00)
    setImod_filename(modelFilename);
  else
    Imod_filename[0] = '\0';
  
  initNewModel(vi->imod);
  vi->reloadable = 0;
  MaintainModelName(vi->imod);

  // 12/30/12: Moved the setModelTrans up and converted to rotation before the draw
  // Also need to get the flipped state right before this call
  ivwFlipModel(vi);
  ivwSetModelTrans(vi);
  utilExchangeFlipRotation(vi->imod, FLIP_TO_ROTATION);

  vi->imod->mousemode = mode;
  imod_cmap(vi->imod);
  vi->imod->ctime = vi->curTime;

  imod_info_setobjcolor();
  if (!vi->doingInitialLoad)
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_SKIPMODV);

  imod_info_setocp();

  /* DNM: notify imodv of new model after scaling*/
  imodv_new_model(Model);
  slicerNewTime(true);
  if (!vi->doingInitialLoad)
    imodPlugCall(vi, 0, IMOD_REASON_NEWMODEL);

  /* Set the checksum to avoid save requests */
  vi->imod->csum = imodChecksum(vi->imod);
  if (imodDebug('C'))
    wprint("createNewModel set checksum %d\n", vi->imod->csum);
     
  return IMOD_IO_SUCCESS;
}

// Initializes a few items in a new model
void initNewModel(Imod *imod)
{
  Iobj *obj;
  imodvViewsInitialize(imod);
  imodNewObject(imod);

  /* DNM 5/16/02: if multiple image files, set time flag by default */
  obj = imodObjectGet(imod);
  if (App->cvi->numTimes)
    obj->flags |= IMOD_OBJFLAG_TIME;
  setModelScalesFromImage(imod, true);
}

// Set the pixel size and the zscale from the imge file header
void setModelScalesFromImage(Imod *imod, bool doZscale)
{
  ImodImageFile *image = App->cvi->image;
  /* 1/13/06: Set header pixel size and zscale */
  if (!App->cvi->fakeImage && image->xscale) {
    if (image->xscale != 1.0f) {
      imod->pixsize = image->xscale / 10.f;
      imod->units = IMOD_UNIT_NM;
    }

    // Round pixel-size based Z-scale to 3 digits, keep this consistent with
    // edit-model-header treatment of pixel ratio
    if (image->zscale && doZscale)
      imod->zscale = (float)
        (0.001 * floor(1000. * image->zscale / image->xscale + 0.5));
  }
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
  QString message;

  if (!im->fp)
    ivwReopen(im);
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

  idata = mrcGetDataMemory(li, (size_t)vi->xsize * (size_t)vi->ysize, 
                           vi->zsize, pixsize);
  if (!idata)
    return NULL;

  vi->loadingImage = 1;
  for (i = 0; i < vi->zsize; i++) {

    // Put \r at the end of this message, and send out a \n so that there is 
    // a clean start for beep colors in wprint
    message.sprintf("Reading Image # %3.3d\r", i+1); 
    imod_imgcnt(LATIN1(message));
    ivwReadBinnedSection(vi, (char *)idata[i], i + li->zmin);
  }

  imod_imgcnt("\n");
  vi->loadingImage = 0;
  return(idata);
}

/*
 * Copy given string into Imod_filename
 */
void setImod_filename(const char *name)
{
  int nChars = strlen(name);
  if (nChars >= IMOD_FILENAME_SIZE)
    nChars = IMOD_FILENAME_SIZE - 1;
  strncpy(Imod_filename, name, nChars);
  Imod_filename[nChars] = '\0';
}


// imodIOGetError   return the last error code
int imodIOGetError() 
{
  return lastError;
}

const char *imodIOGetErrorString()
{
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
static int mapErrno(int errorCode)
{
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

/* 10/14/05: Removed SaveImage and WriteImage (file version 4.19) */


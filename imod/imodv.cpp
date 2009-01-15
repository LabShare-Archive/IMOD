/*
 *  imodv.cpp -- The main imodv entry point for standalone or imod operation
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "imodv_window.h"
#include <qapplication.h>
#include <qdir.h>
#include <qimage.h>
#include <qpixmap.h>
#include "dia_qtutils.h"

#include "imodv.h"
#include "imodv_views.h"
#include "imodv_menu.h"
#include "imod.h"
#include "imodview.h"
#include "imod_info_cb.h"
#include "imod_object_edit.h"
#include "imod_display.h"
#include "imodv_gfx.h"
#include "imodv_stereo.h"
#include "imodv_modeled.h"
#include "preferences.h"
#include "control.h"
#include "imod_client_message.h"
#include "undoredo.h"
#include "imod_assistant.h"
#include "sslice.h"

#include "b3dicon.xpm"

// static declarations
static void usage(char *pname);
static int load_models(int n, char **fname, ImodvApp *a);
static int openWindow(ImodvApp *a);
static int getVisuals(ImodvApp *a);
static void initstruct(ImodView *vw, ImodvApp *a);
static int imodv_init(ImodvApp *a, struct Mod_Draw *md);


// GLOBAL VARIABLES for the imodv and draw structures
ImodvApp  ImodvStruct;
struct Mod_Draw Imodv_mdraw;
ImodvApp *Imodv = &ImodvStruct;

/* A global to indicate if the window is closed */
int ImodvClosed = 1;


#define DEFAULT_XSIZE  512
#define DEFAULT_YSIZE  512

static int onceOpened = 0;
static QRect lastGeom;

/* the default graphics rendering attributes for OpenGL */
static ImodGLRequest True24 = {1, 1, 24, 1, 1};
static ImodGLRequest True15 = {1, 1, 15, 1, 1};
static ImodGLRequest True12 = {1, 1, 12, 1, 1};
static ImodGLRequest True24nodep = {1, 1, 24, 0, 1};
static ImodGLRequest True15nodep = {1, 1, 15, 0, 1};
static ImodGLRequest True12nodep = {1, 1, 12, 0, 1};

/* List the attribute lists in order of priority */
static ImodGLRequest *OpenGLAttribList[] = {
  &True24, &True15, &True12, &True24nodep, &True15nodep, &True12nodep, 
  NULL
};

/*void __eprintf(void){return;} */

static void usage(char *pname)
{
  QString qstr;
  imodVersion(pname);
  imodCopyright();
  qstr = "options: all Qt options plus:\n";
  qstr += "\t-f               Open window to max size.\n";
  qstr += "\t-b color_name    Background color for rendering.\n";
  qstr += "\t-s width,height  Window size in pixels.\n";
  qstr += "\t-E <keys>        Open windows specifed by key letters (= hot keys).\n";
  qstr += "\t-D               Debug mode.\n";
  qstr += "\t-h               Print this help message.\n";
  imodPrintInfo(LATIN1(qstr));
  exit(3);
}

static char *blackString = "black";

/* DEFAULT INITIALIZATION OF THE STRUCTURES
   9/2/02: also called for model view initialization in imod */
static int imodv_init(ImodvApp *a, struct Mod_Draw *md)
{
  a->nm = 0;
  a->cm = 0;
  a->mod = NULL;
  a->imod = NULL;
  a->mat  = imodMatNew(3);
  a->rmat  = imodMatNew(3);
  a->dobj = imodObjectNew();
  a->obj = a->dobj;
  a->ob = 0;
  a->md = md;
  a->cnear = 0;
  a->cfar = 1000;
  a->fovy = 0;
  a->movie = 0;
  a->movieFrames = 0;
  a->movieSpeed = 36.;
  a->snap_fileno = 0;
  a->wpid = 0;
  a->stereo = IMODV_STEREO_OFF;
  a->clearAfterStereo = 0;
  a->plax = 5.0f;
  a->lightx = a->lighty = 0;
  a->winx = DEFAULT_XSIZE;
  a->winy = DEFAULT_YSIZE;
  a->rbgname = blackString;
  a->rbgcolor = new QColor(0, 0, 0);

  md->xorg = md->yorg = md->zorg = 0;
  md->xrot = md->yrot = md->zrot = 0;  /* current rotation. */
  md->zoom = 1.0f;
  md->arot = 10;
  md->atrans = 5.0f;
  md->azoom = 1.05f;
  md->azscale = 0.2;
  md->xrotm = 0; /* rotation movie */
  md->yrotm = 0;
  md->zrotm = 0;

  /* control flags */
  a->current_subset = 0;
  a->crosset    = 0;
  a->fullscreen = 0;
  a->drawall    = 0;
  a->moveall    = 1;
  a->alpha      = 0;
  a->drawExtraOnly = 0;
  imodViewDefault(&a->view);
  a->view.cnear = 0.0f;
  a->view.cfar  = 1.0f;
  a->doPick = 0;
  a->wPick = a->hPick = 5;

  a->lighting  = 1;
  a->depthcue  = 0;
  a->wireframe = 0;
  a->lowres = 0;
  a->drawClip = 0;
  a->drawLight = 0;
  a->linkToSlicer = 0;
  a->linkSlicerCenter = 1;
  a->boundBoxExtraObj = 0;
  a->curPointExtraObj = 0;

  // DNM 6/6/04: Get rid of stereo command initialization

  return(0);
}

// ADDITIONAL INITIALIZATION TO OPEN MODEL VIEW FROM IMOD
/* DNM 9/2/02: changed to call imodv_init for bulk of the initialization */
static void initstruct(ImodView *vw, ImodvApp *a)
{
  Ipoint imageMax;
  float binScale;

  imodv_init(a, &Imodv_mdraw);

  a->nm = 1;
  a->mod = (Imod **)malloc(sizeof(Imod *));
  a->mod[0] = vw->imod;
  a->imod = vw->imod;

  /* DNM 8/3/01: start with current object if defined */
  if (a->imod->cindex.object >= 0 && 
      a->imod->cindex.object < a->imod->objsize) {
    a->ob = a->imod->cindex.object;
    a->obj = &(a->imod->obj[a->ob]);
  }

  /* control flags */
  a->fullscreen = 0;

  a->standalone = 0;
  a->texMap  = 0;
  a->texTrans = 0;
  a->vi = vw;

  // Recompute scale and shift of all views to take care of binning - replaces
  // old method of setting up views
  imageMax.x = vw->xsize;
  imageMax.y = vw->ysize;
  imageMax.z = vw->zsize;
  binScale = ((float)vw->zbin) / ((float)vw->xybin);
  for (int i = 0; i < a->imod->viewsize; i++)
    imodViewDefaultScale(a->imod, &a->imod->view[i], &imageMax, binScale);
  return;
}


// GET THE VISUAL INFORMATION NEEDED TO OPEN GL WIDGETS
/* DMN 9/2/02: replaced old imodvSetCmap with this, which is accessed from
   both imodv and ximodv (model view startup in imod), and which gets the
   best visuals for double and single buffering then does the old work of
   imodvSetCmap to get color map if needed in color index mode */

static int getVisuals(ImodvApp *a)
{
  int i, colorDB, colorSB, colorDBst, colorSBst;
  int depthSB = -1;
  int depthDB = -1;
  int depthSBst = -1;
  int depthDBst = -1;
  ImodGLVisual *visual;
  
  // Keep track of what depth request is used to get the visuals selected
  // and of the actual depth bits that result
  a->enableDepthSB = -1;
  a->enableDepthDB = -1;
  a->enableDepthSBst = -1;
  a->enableDepthDBst = -1;
  
  // Loop through all requests, asking first for double buffer stereo, then
  // no stereo, then for single buffer stereo, then no stereo.
  // When one is found, the depth is set for that visual type
  for (i = 0; OpenGLAttribList[i] != NULL; i++) {
    if (depthDBst < 0) {
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual && visual->stereo) {
        a->enableDepthDBst = visual->depthEnabled;
        depthDBst = visual->depthBits;
        colorDBst = visual->colorBits;
      }
    }
    if (depthDB < 0) {
      OpenGLAttribList[i]->stereo = 0;
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual) {
        a->enableDepthDB = visual->depthEnabled;
        depthDB = visual->depthBits;
        colorDB = visual->colorBits;
      }
      OpenGLAttribList[i]->stereo = 1;
    }

    // It is almost certainly unnecessary to restore the table entries because
    // the find visual machinery stores a table of visuals, but do so anyway
    OpenGLAttribList[i]->doubleBuffer = 0;
    if (depthSBst < 0) {
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual) {
        a->enableDepthSBst = visual->depthEnabled;
        depthSBst = visual->depthBits;
        colorSBst = visual->colorBits;
      }
    }
    if (depthSB < 0) {
      OpenGLAttribList[i]->stereo = 0;
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual) {
        a->enableDepthSB = visual->depthEnabled;
        depthSB = visual->depthBits;
        colorSB = visual->colorBits;
      }
      OpenGLAttribList[i]->stereo = 1;
    }
    OpenGLAttribList[i]->doubleBuffer = 1;
    
    /* If got both, stop the loop (i.e., ignore lesser capability stereo 
       visuals */
    if (depthDB >= 0 && depthSB >= 0)
      break;
  }

  /* error if no visuals */
  if (depthDB < 0 && depthSB < 0 && depthDBst < 0 && depthSBst < 0)
    return 1;
  
  // If somehow there is stereo with depth and regular with not, just pretend
  // the non-stereo does not exist
  if (!depthDB && depthDBst > 0) {
    depthDB = -1;
    a->enableDepthDB = -1;
  }
  if (!depthSB && depthSBst > 0) {
    depthSB = -1;
    a->enableDepthSB = -1;
  }

  // Priority sequence will be DB, DB stereo, SB, SB stereo
  if (!depthDB || (!depthDBst && depthDB < 0) || 
      (!depthSB && depthDB < 0 && depthDBst < 0) ||
      (!depthSBst && depthDB < 0 && depthDBst < 0 && depthSB < 0))
    imodError(NULL, "3dmodv warning: using a visual with"
            " no depth buffer\n");

  if (depthDB < 0)
    imodError(NULL, "3dmodv warning: no %sdouble buffer visual available.\n",
              depthDBst >= 0 ? "non-stereo " : "");
  else if (Imod_debug)
    imodPrintStderr("DB visual: %d color bits, %d depth bits\n",
	   colorDB, depthDB);
  if (Imod_debug && depthDBst >= 0)
    imodPrintStderr("DB stereo visual: %d color bits, %d depth bits\n",
                    colorDBst, depthDBst);
  if (depthSB < 0)
    imodError(NULL, "3dmodv warning: no single buffer visual available.\n");
  else if (Imod_debug)
    imodPrintStderr("SB visual: %d color bits, %d depth bits\n",
	   colorSB, depthSB);
  if (Imod_debug && depthSBst >= 0)
    imodPrintStderr("SB stereo visual: %d color bits, %d depth bits\n",
                    colorSBst, depthSBst);

  // set to double buffer if visual exists
  a->db = depthDB >= 0 || depthDBst >= 0 ? 1 : 0;
  return 0;
}

// OPEN THE MAIN WINDOW
static int openWindow(ImodvApp *a)
{
  int newWidth = a->winx;
  int needy = a->winy;
  int xleft, ytop, newHeight;

  a->lighting = Imodv->imod->view->world & VIEW_WORLD_LIGHT;
  a->lowres = (Imodv->imod->view->world & VIEW_WORLD_LOWRES) ? 1 : 0;
  a->mainWin = new ImodvWindow(a);

  if (!a->mainWin)
    return 1;

  ImodvClosed = 0;
  imodvSetCaption();
  a->mainWin->setWindowIcon(*(a->iconPixmap));

  // If fullscreen, set that and show
  if (a->fullscreen) {
    a->mainWin->showMaximized();
    a->mainWin->show();
  } else {

    // If windows was open before, set size and position the same
    if (onceOpened) {
      newWidth = lastGeom.width();
      newHeight = lastGeom.height();
      xleft = lastGeom.x();
      ytop = lastGeom.y();
    } else {

      // Otherwise, set the size, allowing for menu based on the current size
      // Have to actually show it for the GL widget to fill the window
      a->mainWin->show();
      imod_info_input();
      newHeight = needy + a->mainWin->height() - a->winy;
      QPoint pos = a->mainWin->pos();
      xleft = pos.x();
      ytop = pos.y();
    }

    // Fix positions same as for zap window and set and draw
    diaLimitWindowSize(newWidth, newHeight);
    diaLimitWindowPos(newWidth, newHeight, xleft, ytop);

    if (Imod_debug)
      imodPrintStderr("Sizes: imodv %d %d, GL %d %d: "
                  "resize %d %d\nnew pos %d %d\n",
                  a->mainWin->width(), a->mainWin->height(),
                  a->mainWin->mCurGLw->width(), 
                  a->mainWin->mCurGLw->height(),
                  newWidth, newHeight, xleft, ytop);

    a->mainWin->resize(newWidth, newHeight);
    a->mainWin->move(xleft, ytop);
    a->mainWin->show();
    
    // 11/24/03: OS 10.3 needs to move after the show
#ifdef Q_OS_MACX
    a->mainWin->move(xleft, ytop);
#endif
  }
  return(0);
}

// OPEN THE MODELS IN IMODV
static int load_models(int n, char **fname, ImodvApp *a)
{
  int i, ob, co;
  Ipoint imageMax = {0., 0., 0.};
  Imod *mod;

  if (n < 1)
    return(0);
  a->mod = (Imod **)malloc(sizeof(Imod *) * n);
  a->nm = n;
  a->cm = 0;
  for(i = 0; i < n; i++){
    a->mod[i] = imodRead(LATIN1(QDir::convertSeparators(QString(fname[i]))));
    if (!a->mod[i]){
      imodError(NULL, "Error loading %s\n", fname[i]);
      return(-1);
    }
    mod = a->mod[i];


    /* DNM 6/20/01: find out max time and set current time */
    mod->tmax = 0;
    for (ob = 0; ob < mod->objsize; ob++)
      for (co = 0; co < mod->obj[ob].contsize; co++)
        if (mod->tmax < mod->obj[ob].cont[co].time)
          mod->tmax = mod->obj[ob].cont[co].time;
    mod->ctime = mod->tmax ? 1 : 0;

    /* DNM: changes for storage of object properties in view and 
       relying on default scaling.  Also, make sure every model has
       the view to use set up 
       6/26/03: switch to new method, just initialize views in each model */
    /* 7/17/03: trouble.  Restore default scaling of current view if exists */
    /* 10/16/05: Need to scale all views in case model saved from binned */
    for (int i = 0; i < mod->viewsize; i++)
      imodViewDefaultScale(mod, &mod->view[i], &imageMax, 1.);
  
    imodvViewsInitialize(mod);
  }

  a->imod = (a->mod[a->cm]);
  /* DNM 8/3/01: start with current object if defined */
  if (a->imod->cindex.object >= 0 && 
      a->imod->cindex.object < a->imod->objsize) {
    a->ob = a->imod->cindex.object;
    a->obj = &(a->imod->obj[a->ob]);
  }
   
  return(0);
}

// THE ENTRY POINT FOR STANDALONE IMODV
int imodv_main(int argc, char **argv)
{
  int i;
  int printID = 0;
  bool useStdin = false;
  ImodvApp *a = Imodv;
  char *windowKeys = NULL;
  a->standalone = 1;
  diaSetTitle("3dmodv");
  imodv_init(a, &Imodv_mdraw);

  // DNM 5/17/03: The Qt application and preferences are already gotten

  // Parse options
  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){

      case 'b':
	a->rbgname = strdup(argv[++i]);
	break;

      case 'D':
	Imod_debug = 1;
	break;

      case 'f':
	a->fullscreen = 1;
	break;

      case 's':
	sscanf(argv[++i], "%d%*c%d", &a->winx, &a->winy);
	break;

      case 'W':
	printID = 1;
	break;

      case 'E':
        windowKeys = strdup(argv[++i]);
        break;

      case 'L':
        useStdin = true;
        break;
        
      case 'h':
        usage(argv[0]);
        exit(1);
        break;

      default:
        if (strcmp("-modv", argv[i]) && 
            strcmp("-view", argv[i])) {
          imodError(NULL, "3dmodv error: illegal option %s\n", argv[i]);
          exit(1);
        }

      }
    } else
      break;
  }

  a->db        = 1;

  // Make a vi structure and initialize extra objects
  a->vi = (ImodView *)malloc(sizeof(ImodView));
  ivwInit(a->vi, true);

  // Make a color from the named color; fallback to black
  QString qstr = a->rbgname;
  a->rbgcolor->setNamedColor(a->rbgname);
  if (!a->rbgcolor->isValid())
    a->rbgcolor->setRgb(0, 0, 0);

  if (getVisuals(a) != 0) {
    imodError(NULL, "3dmodv error: Couldn't get rendering visual.\n");
    exit(3);
  }

  if (argc - i < 1)
    usage(argv[0]);

  /* DNM 1/29/03: already forked in imod, no need to do it here */
  
  if (load_models(argc - i, &(argv[i]), Imodv))
    exit(3);

  a->vi->imod = a->imod;
  a->iconPixmap = new QPixmap(QPixmap::fromImage(QImage(b3dicon)));

  if (openWindow(Imodv))
    exit(3);

  if (printID) {
    unsigned int winID = (unsigned int)a->mainWin->winId();
    imodPrintStderr("Window id = %u\n", winID);
  }
  if (printID || useStdin)
    ClipHandler = new ImodClipboard(useStdin);

  imodvOpenSelectedWindows(windowKeys);

  return qApp->exec();
}


// THE CALL TO OPEN THE MODEL VIEW WINDOW FROM IMOD
void imodv_open()
{
  ImodView *vw = App->cvi;
  Imodv = &ImodvStruct;
  ImodvStruct.md = &Imodv_mdraw;
  ImodvApp *a = Imodv;

  Imod *imod = vw->imod;

  /* mt model ? */
  if (!imod){
    wprint("Model View didn't open because "
           "there is no model loaded.\n");
    return;
  }

  /* DNM 6/26/03: eliminate test for 2 points, now that it can use image size
     to set scaling */

  /* check for already open? */
  if (!ImodvClosed){
    a->mainWin->raise();
    return;
  }

  initstruct(vw, a);
  a->iconPixmap = App->iconPixmap;

  if (getVisuals(a) != 0) {
    wprint("Couldn't get rendering visual for model view."
           "  Try running 3dmodv separately.\n");
    imodMatDelete(a->mat);
    imodMatDelete(a->rmat);
    return;
  }

  if (openWindow(a)) {
    wprint("Failed to open model view window.\n");
    return;
  }
    
}

// TO CLOSE FROM IMOD
void imodv_close()
{
  if (!ImodvClosed)
    Imodv->mainWin->close();
}

// TO DRAW THE MODEL FROM IMOD
void imodv_draw()
{
  if (!ImodvClosed)
    imodvDraw(Imodv);
  return;
}

/* DNM: a routine for imod to notify imodv of a change in model.
 Call it with NULL to disable access of model during change */
void imodv_new_model(Imod *mod)
{
  Ipoint imageMax;
  float binScale;

  if (ImodvClosed)
    return;

  Imodv->imod = mod;
  Imodv->mod[0] = mod;
  if (!mod)
    return;

  // Recompute scale and shift of all views (needed to take care of binning 
  // replaces earlier method of setting up views
  imageMax.x = Imodv->vi->xsize;
  imageMax.y = Imodv->vi->ysize;
  imageMax.z = Imodv->vi->zsize;
  binScale = ((float)Imodv->vi->zbin) / ((float)Imodv->vi->xybin);
  for (int i = 0; i < mod->viewsize; i++)
    imodViewDefaultScale(mod, &mod->view[i], &imageMax, binScale);

  imodvSelectModel(Imodv, 0);
}

int imodvLinkedToSlicer()
{
  if (ImodvClosed)
    return 0;
  return Imodv->linkToSlicer;
}

int imodvRotCenterLinked()
{
  if (ImodvClosed)
    return 0;
  return Imodv->linkSlicerCenter && Imodv->linkToSlicer ? 1 : 0;
}

int imodvStandalone()
{
  if (ImodvClosed)
    return 0;
  return Imodv->standalone;
}

void imodvNewModelAngles(Ipoint *rot)
{
  if (Imodv->linkToSlicer)
    setTopSlicerFromModelView(rot);
}

void imodvSetCaption()
{
  ImodvApp *a = Imodv;
  char *window_name;
  QString str;
  if (ImodvClosed)
    return;

  window_name = imodwEithername((char *)(a->standalone ? "3dmodv:" : 
                                 "3dmod Model View: "), a->imod->fileName, 1);
  if (window_name) {
    str = window_name;
    free(window_name);
  } 
  if (str.isEmpty())
    str = "3dmod Model View";

  a->mainWin->setWindowTitle(str);
}

// To call imodDraw if not in standalone mode
void imodvDrawImodImages(int skipDraw)
{
  if (Imodv->standalone)
    return;
  if (!skipDraw)
    imodDraw(Imodv->vi, IMOD_DRAW_MOD | IMOD_DRAW_SKIPMODV);
  imod_object_edit_draw();
  imod_info_setobjcolor();
}

// Inform other parts of program whether byte images exist in memory
int imodvByteImagesExist()
{
  if (Imodv->standalone)
    return 0;
  if (Imodv->vi->rawImageStore || Imodv->vi->fakeImage)
    return 0;
  return 1;
}

// Pass changes to undo if not in standalone mode
void imodvRegisterModelChg()
{
  //if (Imodv->standalone)
  //  return;
  Imodv->vi->undo->modelChange();
}

void imodvRegisterObjectChg(int object)
{
  if (/*Imodv->standalone ||*/ object >= Imodv->imod->objsize)
    return;
  Imodv->vi->undo->objectPropChg(object);
}

void imodvFinishChgUnit()
{
  //if (Imodv->standalone)
  //  return;
  Imodv->vi->undo->finishUnit();
}

// Quit imodv
void imodvQuit()
{
  ImodvApp *a = Imodv;
  ImodvClosed = 1;
  onceOpened = 1;
  lastGeom = ivwRestorableGeometry(a->mainWin);

  if (a->boundBoxExtraObj > 0)
    ivwFreeExtraObject(a->vi, a->boundBoxExtraObj);
  if (a->curPointExtraObj > 0)
    ivwFreeExtraObject(a->vi, a->curPointExtraObj);

  if (a->standalone && ClipHandler)
    ClipHandler->startDisconnect();
  stereoHWOff();
  imodvDialogManager.close();

  imodMatDelete(a->mat);
  imodMatDelete(a->rmat);
  delete a->rbgcolor;
  if (a->standalone) {
    // imod_info_input();   // This made it crash
    if (ImodPrefs) 
      ImodPrefs->saveSettings(1);
    if (ImodHelp)
      delete ImodHelp;
    if (ClipHandler)
      ClipHandler->waitForDisconnect();
    QApplication::exit(0);
  }
  return;
}

/*
$Log$
Revision 4.44  2008/12/17 17:49:13  mast
Make warning about no DB visual more nuanced

Revision 4.43  2008/12/15 21:22:43  mast
Make separate widgets for stereo so they are used only when stereo is on

Revision 4.42  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 4.41  2008/11/28 06:41:13  mast
Manage extra objects

Revision 4.40  2008/11/14 19:28:00  mast
Only warn on no depth buffer if actually using visual with none.

Revision 4.39  2008/10/02 22:43:51  mast
Call window constructor with stereo capabilities

Revision 4.38  2008/06/10 05:49:19  mast
Add flag for drawing the light vector

Revision 4.37  2008/05/27 05:48:41  mast
Changes for linking slicer center of rotation

Revision 4.36  2008/05/22 15:42:57  mast
Changed for extra object editability

Revision 4.35  2008/04/01 23:44:09  mast
initialized new flag

Revision 4.34  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.33  2007/11/30 06:51:50  mast
Changes for linking slicer to model view

Revision 4.32  2007/09/20 22:06:55  mast
Changes for visualizing clipping plane

Revision 4.31  2006/09/12 15:46:32  mast
Handled contour member renames

Revision 4.30  2006/07/03 19:52:05  mast
Request disconnect of message handler on exit

Revision 4.29  2006/06/19 05:29:14  mast
Added -L option to use stdin for messages; delete clipboard object on exit

Revision 4.28  2006/03/01 19:13:06  mast
Moved window size/position routines from xzap to dia_qtutils

Revision 4.27  2005/10/16 20:27:07  mast
INitialize scaling of all views in standalone mode too

Revision 4.26  2005/10/14 22:01:56  mast
Allow imod to disable access to model while it is being replaced

Revision 4.25  2005/10/13 20:07:25  mast
Scale all views upon startup or new model and provide bin scaling

Revision 4.24  2004/11/22 00:24:46  mast
Added deletion of ImodHelp on exit

Revision 4.23  2004/11/21 06:02:17  mast
Provided window opening by key letter and routines for undo calls

Revision 4.22  2004/11/12 01:20:55  mast
Fixed bug that made it impossible to turn off stored low res mode

Revision 4.21  2004/09/21 20:30:52  mast
Added call to synchronize object color change to info window

Revision 4.20  2004/07/07 19:25:29  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 4.19  2004/06/06 21:27:21  mast
Eliminated stereo-command related items

Revision 4.18  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.17  2004/03/30 18:57:19  mast
Did initial size-setting and coordinate limiting the same as for Zap window,
which made it work right under Windows

Revision 4.16  2003/12/30 06:32:16  mast
Make snap_fileno be part of imodvApp structure

Revision 4.15  2003/11/26 18:16:07  mast
Add function to determine if byte images exist

Revision 4.14  2003/11/25 01:14:36  mast
Repeat the window move after the show for Mac OS 10.3 when reopening window

Revision 4.13  2003/11/12 18:54:23  mast
Add ability to receive messages & save and restore window position from 3dmod

Revision 4.12  2003/11/04 04:41:32  mast
Initialize rotation speed

Revision 4.11  2003/11/01 18:12:17  mast
changed to put out virtually all error messages to a window

Revision 4.10  2003/07/17 14:41:32  mast
Go back to scaling and centering model when opening imodv

Revision 4.9  2003/06/27 20:04:47  mast
Changes for new scheme in which there is always a view 1: initialize
views when reading in a model; adjust scaling when opening model view
from imod

Revision 4.8  2003/05/18 22:58:33  mast
simplify creating icon pixmap

Revision 4.7  2003/05/18 22:06:37  mast
Changed to start QApplication before calling, and to create icon pixmap

Revision 4.6  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.5  2003/04/17 21:48:44  mast
simplify -imodv option processing

Revision 4.4  2003/03/26 23:22:00  mast
Set up to use preferences

Revision 4.3  2003/03/04 21:41:05  mast
Added function for refreshing imod windows from imodv

Revision 4.2  2003/02/27 17:42:38  mast
Remove include of unistd for windows

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.11  2003/01/29 17:50:58  mast
Fork now happens before imodv_main is called

Revision 1.1.2.10  2003/01/29 01:29:42  mast
add call for imod to close imodv

Revision 1.1.2.9  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.8  2003/01/01 19:12:31  mast
changes to start Qt application in standalone mode

Revision 1.1.2.7  2003/01/01 05:46:29  mast
changes for qt version of stereo

Revision 1.1.2.6  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.5  2002/12/17 22:28:20  mast
cleanup of unused variables and SGI errors

Revision 1.1.2.4  2002/12/17 18:42:22  mast
Qt version, incorporating ximodv startup code

Revision 1.1.2.3  2002/12/14 05:41:08  mast
Got qxt startup in the right place

Revision 1.1.2.2  2002/12/06 21:58:40  mast
*** empty log message ***

Revision 1.1.2.1  2002/12/05 16:28:37  mast
Open a qxt application

Revision 3.5  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.4  2002/11/30 06:07:22  mast
Corrected tables after addition of true-15 visual

Revision 3.3  2002/11/27 03:29:45  mast
Made it look for both single and double buffer visuals as long as they
are both RGB or both color index.  Added a true 15 visual, better than
pseudo 12 (present on O2).

Revision 3.2  2002/09/04 00:24:48  mast
Added CVS header.  Changed to getting visuals then passing them to GLw.

*/

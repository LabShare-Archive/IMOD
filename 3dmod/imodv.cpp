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
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "mv_window.h"
#include <qapplication.h>
#include <qdir.h>
#include <qimage.h>
#include <qpixmap.h>
#include "dia_qtutils.h"

#include "imodv.h"
#include "mv_views.h"
#include "mv_menu.h"
#include "imod.h"
#include "imodview.h"
#include "info_cb.h"
#include "imod_input.h"
#include "object_edit.h"
#include "display.h"
#include "mv_gfx.h"
#include "mv_image.h"
#include "mv_stereo.h"
#include "mv_modeled.h"
#include "preferences.h"
#include "control.h"
#include "client_message.h"
#include "undoredo.h"
#include "imod_assistant.h"
#include "sslice.h"
#include "vertexbuffer.h"
#include "utilities.h"

#include "b3dicon.xpm"

// static declarations
static void usage(char *pname);
static int load_models(int n, char **fname, ImodvApp *a);
static int openWindow(ImodvApp *a);
static int getVisuals(ImodvApp *a);
static void initstruct(ImodView *vw, ImodvApp *a);
static int imodv_init(ImodvApp *a);


// GLOBAL VARIABLES for the imodv and draw structures
ImodvApp  ImodvStruct;
ImodvApp *Imodv = &ImodvStruct;

/* A global to indicate if the window is closed */
int ImodvClosed = 1;


#define DEFAULT_XSIZE  512
#define DEFAULT_YSIZE  512

static int onceOpened = 0;
static QRect lastGeom;

/* the default graphics rendering attributes for OpenGL */
// The numbers are single/double buffer, rgba, colorBits, depthBits, stereo, alpha
static ImodGLRequest True24 = {1, 1, 24, 1, 1, 1};
static ImodGLRequest True15 = {1, 1, 15, 1, 1, 1};
static ImodGLRequest True12 = {1, 1, 12, 1, 1, 1};
static ImodGLRequest True24nodep = {1, 1, 24, 0, 1, 1};
static ImodGLRequest True15nodep = {1, 1, 15, 0, 1, 1};
static ImodGLRequest True12nodep = {1, 1, 12, 0, 1, 1};

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

static const char *blackString = "black";

/* DEFAULT INITIALIZATION OF THE STRUCTURES
   9/2/02: also called for model view initialization in imod */
static int imodv_init(ImodvApp *a)
{
  a->numMods = 0;
  a->curMod = 0;
  a->mod = NULL;
  a->imod = NULL;
  a->mat  = imodMatNew(3);
  a->rmat  = imodMatNew(3);
  a->obj = imodObjectNew();
  a->objNum = 0;
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
  if (!onceOpened)
    a->transBkgd = 0;
  a->alphaVisual = 0;
  a->plax = 5.0f;
  a->imageStereo = 0;
  a->imagesPerArea = 2;
  a->imageDeltaZ = 1;
  a->lightx = a->lighty = 0;
  a->winx = DEFAULT_XSIZE;
  a->winy = DEFAULT_YSIZE;
  a->rbgname = blackString;
  a->rbgcolor = new QColor(0, 0, 0);

  a->deltaRot = 10;
  a->xrotMovie = 0; /* rotation movie */
  a->yrotMovie = 0;
  a->zrotMovie = 0;

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
  a->invertZ = 0;
  a->drawClip = 0;
  a->drawLight = 0;
  a->linkToSlicer = 0;
  a->linkSlicerCenter = 1;
  a->boundBoxExtraObj = 0;
  a->curPointExtraObj = 0;
  a->vertBufOK = -2;
  a->primRestartOK = 0;
  a->vbManager = new VertBufManager;

  // DNM 6/6/04: Get rid of stereo command initialization

  return(0);
}

// ADDITIONAL INITIALIZATION TO OPEN MODEL VIEW FROM IMOD
/* DNM 9/2/02: changed to call imodv_init for bulk of the initialization */
static void initstruct(ImodView *vw, ImodvApp *a)
{
  Ipoint imageMax;
  float binScale;

  imodv_init(a);

  a->numMods = 1;
  a->mod = (Imod **)malloc(sizeof(Imod *));
  a->mod[0] = vw->imod;
  a->imod = vw->imod;

  /* DNM 8/3/01: start with current object if defined */
  if (a->imod->cindex.object >= 0 && 
      a->imod->cindex.object < a->imod->objsize) {
    a->objNum = a->imod->cindex.object;
    a->obj = &(a->imod->obj[a->objNum]);
  }

  /* control flags */
  a->fullscreen = 0;

  a->standalone = 0;
  a->texMap  = mvImageGetFlags() ? 1 : 0;
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
  int i, colorDB, colorDBal, colorSB, colorDBst, colorDBstAl, colorSBst;
  int depthSB = -1;
  int depthDB = -1;
  int depthDBal = -1;
  int depthSBst = -1;
  int depthDBst = -1;
  int depthDBstAl = -1;
  ImodGLVisual *visual;
  
  // Keep track of what depth request is used to get the visuals selected
  // and of the actual depth bits that result
  a->enableDepthSB = -1;
  a->enableDepthDB = -1;
  a->enableDepthDBal = -1;
  a->enableDepthSBst = -1;
  a->enableDepthDBst = -1;
  a->enableDepthDBstAl = -1;
  
  // Loop through all requests, asking first for double buffer stereo, then
  // no stereo, then for single buffer stereo, then no stereo.
  // When one is found, the depth is set for that visual type
  for (i = 0; OpenGLAttribList[i] != NULL; i++) {
    if (depthDBstAl < 0) {
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual && visual->stereo && visual->alpha) {
        a->enableDepthDBstAl = visual->depthEnabled;
        depthDBstAl = visual->depthBits;
        colorDBstAl = visual->colorBits;
      }
    }
    if (depthDBst < 0) {
      OpenGLAttribList[i]->alpha = 0;
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual && visual->stereo) {
        a->enableDepthDBst = visual->depthEnabled;
        depthDBst = visual->depthBits;
        colorDBst = visual->colorBits;
      }
      OpenGLAttribList[i]->alpha = 1;
    }
    OpenGLAttribList[i]->stereo = 0;
    if (depthDBal < 0) {
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual && visual->alpha) {
        a->enableDepthDBal = visual->depthEnabled;
        depthDBal = visual->depthBits;
        colorDBal = visual->colorBits;
      }
    }
    if (depthDB < 0) {
      OpenGLAttribList[i]->alpha = 0;
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual) {
        a->enableDepthDB = visual->depthEnabled;
        depthDB = visual->depthBits;
        colorDB = visual->colorBits;
      }
      OpenGLAttribList[i]->alpha = 1;
    }
    OpenGLAttribList[i]->stereo = 1;

    // There was a comment about not needing to restore table entries, but it seems like
    // you do need to if this ever happens more that once
    OpenGLAttribList[i]->doubleBuffer = 0;
    OpenGLAttribList[i]->alpha = 0;
    if (depthSBst < 0) {
      visual = imodFindGLVisual(*OpenGLAttribList[i]);
      if (visual && visual->stereo) {
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
    OpenGLAttribList[i]->alpha = 1;
    
    /* If got both, stop the loop (i.e., ignore lesser capability stereo 
       visuals */
    if (depthDB >= 0 && depthSB >= 0)
      break;
  }

  /* error if no visuals */
  if (depthDB < 0 && depthSB < 0 && depthDBst < 0 && depthSBst < 0 && depthDBal < 0 && 
      depthDBstAl < 0)
    return 1;
  
  // If somehow there is stereo with depth or alpha and regular with not, just pretend
  // the non-stereo does not exist; similarly for alpha and stereo
  if (!depthDB && (depthDBst > 0 || depthDBal > 0)) {
    depthDB = -1;
    a->enableDepthDB = -1;
  }
  if (!depthDBal && depthDBstAl > 0) {
    depthDBal = -1;
    a->enableDepthDBal = -1;
  }
  if (!depthSB && depthSBst > 0) {
    depthSB = -1;
    a->enableDepthSB = -1;
  }

  // Priority sequence will be DB, DB alpha, DB stereo, DB stereo alpha, SB, SB stereo
  if (!depthDB || (!depthDBal && depthDB < 0) || 
      (!depthDBst && depthDB < 0 && depthDBal < 0) || 
      (!depthDBstAl && depthDB < 0 && depthDBal < 0 && depthDBst < 0) || 
      (!depthSB && depthDB < 0 && depthDBst < 0 && depthDBal < 0 && depthDBstAl < 0) ||
      (!depthSBst && depthDB < 0 && depthDBst < 0 && depthSB < 0 && depthDBal < 0 && 
       depthDBstAl < 0 && depthSB < 0))
    imodError(NULL, "3dmodv warning: using a visual with no depth buffer\n");

  if (depthDB < 0)
    imodError(NULL, "3dmodv warning: no %sdouble buffer visual available.\n",
              (depthDBst >= 0 || depthDBal >= 0|| depthDBstAl >= 0) ? 
              "non-stereo, non-alpha " : "");
  else if (Imod_debug)
    imodPrintStderr("DB visual: %d color bits, %d depth bits\n", colorDB, depthDB);
  if (Imod_debug && depthDBst >= 0)
    imodPrintStderr("DB stereo visual: %d color bits, %d depth bits\n", colorDBst, 
                    depthDBst);
  if (Imod_debug && depthDBal >= 0)
    imodPrintStderr("DB alpha visual: %d color bits, %d depth bits\n", colorDBal, 
                    depthDBal);
  if (Imod_debug && depthDBstAl >= 0)
    imodPrintStderr("DB alpha stereo visual: %d color bits, %d depth bits\n",
                    colorDBstAl, depthDBstAl);
  if (depthSB < 0)
    imodError(NULL, "3dmodv warning: no single buffer visual available.\n");
  else if (Imod_debug)
    imodPrintStderr("SB visual: %d color bits, %d depth bits\n", colorSB, depthSB);
  if (Imod_debug && depthSBst >= 0)
    imodPrintStderr("SB stereo visual: %d color bits, %d depth bits\n",
                    colorSBst, depthSBst);

  // set to double buffer if visual exists
  a->dblBuf = (depthDB >= 0 || depthDBst >= 0 || depthDBal >= 0 || depthDBstAl >= 0) ? 
    1 : 0;
  return 0;
}

// OPEN THE MAIN WINDOW
static int openWindow(ImodvApp *a)
{
  int newWidth = a->winx;
  int needy = a->winy;
  int xleft, ytop, newHeight;

  // These may not matter...
  a->invertZ = (a->imod->view->world & VIEW_WORLD_INVERT_Z) ? 1 : 0;
  a->lighting = (a->imod->view->world & VIEW_WORLD_LIGHT) ? 1 : 0;
  a->lowres = (a->imod->view->world & VIEW_WORLD_LOWRES) ? 1 : 0;
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
  a->numMods = n;
  a->curMod = 0;
  for(i = 0; i < n; i++){
    a->mod[i] = imodRead(LATIN1(QDir::convertSeparators(QString(fname[i]))));
    if (!a->mod[i]){
      imodError(NULL, "Error loading %s\n", fname[i]);
      return(-1);
    }
    mod = a->mod[i];
    utilExchangeFlipRotation(mod, FLIP_TO_ROTATION);

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
    for (int j = 0; j < mod->viewsize; j++)
      imodViewDefaultScale(mod, &mod->view[j], &imageMax, 1.);
  
    imodvViewsInitialize(mod);
  }

  a->imod = (a->mod[a->curMod]);
  /* DNM 8/3/01: start with current object if defined */
  if (a->imod->cindex.object >= 0 && 
      a->imod->cindex.object < a->imod->objsize) {
    a->objNum = a->imod->cindex.object;
    a->obj = &(a->imod->obj[a->objNum]);
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
  imodv_init(a);

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

  a->dblBuf        = 1;

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
    ClipHandler = new ImodClipboard(useStdin, false);

  imodvOpenSelectedWindows(windowKeys);

#ifdef Q_OS_MACX
  a->mainWin->raise();
#endif
  return qApp->exec();
}


// THE CALL TO OPEN THE MODEL VIEW WINDOW FROM IMOD
void imodv_open()
{
  ImodView *vw = App->cvi;
  Imodv = &ImodvStruct;
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
  if (ImodvClosed)
    return;

  setModvDialogTitle(a->mainWin, (char *)(a->standalone ? "3dmodv:" :
                                          "3dmod Model View: "));
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
  if (Imodv->vi->rgbStore || Imodv->vi->fakeImage)
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
  vbCleanupVBD(Imodv->imod);
  mvImageCleanup();

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

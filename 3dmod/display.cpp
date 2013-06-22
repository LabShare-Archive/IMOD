/*  
 *  display.cpp -- Open the display and setup visual and colormaps.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qgl.h>
#include <qcursor.h>
#include <qbitmap.h>

#include "imod.h"
#include "b3dgfx.h"
#include "imod_input.h"
#include "info_cb.h"
#include "display.h"
#include "imodv.h"
#include "control.h"
#include "xcramp.h"
#include "preferences.h"
#include "dia_qtutils.h"
#include "scalebar.h"
#include "isosurface.h"

#ifdef Q_OS_MACX
#include "qcursor.mac.bits"
#include "qcursor_mask.mac.bits"
#else
#include "qcursor.bits"
#include "qcursor_mask.bits"
#endif

static void mapOneNamedColor(int index);
static void imodAssessVisual(int ind, int db, int rgba, int depth, int askStereo, 
                             int alpha);

// The numbers are single/double buffer, rgba, colorBits, depthBits, stereo, alpha
static ImodGLRequest qtPseudo12DB = {1, 0, 12, 0, 0, 0};
static ImodGLRequest qtPseudo12SB = {0, 0, 12, 0, 0, 0};
static ImodGLRequest qtPseudo8DB = {1, 0, 8, 0, 0, 0};
static ImodGLRequest qtPseudo8SB = {0, 0, 8, 0, 0, 0};
static ImodGLRequest qtTrue24DB = {1, 1, 24, 0, 0, 0};
static ImodGLRequest qtTrue24SB = {0, 1, 24, 0, 0, 0};
static ImodGLRequest qtTrue12DB = {1, 1, 12, 0, 0, 0};
static ImodGLRequest qtTrue12SB = {0, 1, 12, 0, 0, 0};

static ImodGLRequest *qtGLRequestList[] = {
  &qtTrue24DB, &qtTrue24SB, &qtPseudo12DB, &qtPseudo12SB,
  &qtPseudo8DB, &qtPseudo8SB, &qtTrue12DB, &qtTrue12SB,
  NULL
};


/* open up the display for imod.                               */
int imod_display_init(ImodApp *ap, char **argv)
{
  ap->wzoom   = 1;

  ap->depth = imodFindQGLFormat(ap, argv);

  // Set up the cursor for model mode
  QBitmap bmCursor = QBitmap::fromData(QSize(qcursor_width, qcursor_height),
                                       qcursor_bits);
  QBitmap bmMask = QBitmap::fromData(QSize(qcursor_width, qcursor_height),
                                     qcursor_mask_bits);
  ap->modelCursor = new QCursor(bmCursor, bmMask, qcursor_x_hot, 
                                qcursor_y_hot);
  diaSetTitle("3dmod");
  return(0);
}

/* sets the color index of the given object number. */
void imodSetObjectColor(int ob)
{
  Iobj *obj;

  /* check that ob is within range. */
  if (ob < 0)
    return;
  if (ob >= (int)App->cvi->imod->objsize)
    return;
     
  obj = &(App->cvi->imod->obj[ob]);

  if (App->rgba){
    glColor3f(obj->red, obj->green, obj->blue);
    return;
  }

  if (App->depth <= 8){
    obj->fgcolor = App->objbase - ob;
    b3dColorIndex(App->objbase - ob);
  }else{
    b3dColorIndex(ob + App->objbase);
    obj->fgcolor = App->objbase + ob;
  }
  return;
}


/* changes color of given pixel in color index mode */
int mapcolor(int color, int red, int green, int blue)
{
  if (App->rgba) 
    return 1;
  App->qColormap->setEntry(color, qRgb(red, green, blue));
  return(0);
}


/* setup the colors to be used. */
int imod_color_init(ImodApp *ap)
{
  ap->objbase      = RAMPMIN - 1;

  if (ap->cvi->imod){
    ivwSetBlackWhiteFromModel(ap->cvi);
  }
  
  if (ap->rgba){

    /* Set up ramp for rgba mode */
    ap->qColormap = NULL;
    ap->cvi->rampsize = 256;
    ap->cvi->rampbase = 0;
    ap->cvi->cramp = xcramp_allinit(ap->depth, ap->qColormap, 0, 255,
                                    ap->cvi->ushortStore);
    /*  imod_info_setbw(ap->cvi->black, ap->cvi->white);  NOT YET */
    xcramp_setlevels(ap->cvi->cramp, ap->cvi->black, ap->cvi->white);

    return 0;
  }

  /* Initialize the color map for color index mode, set one entry for
     it to show its true size */
  ap->qColormap = new QGLColormap();
  ap->qColormap->setEntry(0, qRgb(0, 0, 0));
  /* fprintf(stderr, "colormap size %d\n", ap->qColormap->size()); */
  if (ap->qColormap->size() <= 256)
    ap->depth = 8;

  if (ap->depth == 8){
    ap->cvi->rampbase = RAMPMIN;
    ap->cvi->rampsize = RAMPMAX + 1 - RAMPMIN;
    ap->cvi->cramp = xcramp_allinit(ap->depth, ap->qColormap, RAMPMIN, RAMPMAX, 0);
  }else{
    ap->objbase    = ap->base + 257;
    ap->cvi->rampsize = 256;
    ap->cvi->rampbase = ap->base;
    ap->cvi->cramp = xcramp_allinit(ap->depth, ap->qColormap, ap->base, ap->base + 255,
                                    0);
  }

  /* set colors for model objects and fixed colors */
  if (ap->cvi->imod){
    imod_cmap(ap->cvi->imod);
  } else {

    /* This is needed if there was no model somehow */
    mapNamedColors();
  }

  imod_info_setbw(ap->cvi->black, ap->cvi->white);
  xcramp_setlevels(ap->cvi->cramp, ap->cvi->black, ap->cvi->white);

  return(0);
}

/* Set the colormap for the given model. */
void imod_cmap(Imod *m)
{
  int i;
  int red,green,blue;
  if (App->rgba)
    return;
  for (i = 0; i < (int)m->objsize; i++){
    red   = (int)(m->obj[i].red * 255.0);
    green = (int)(m->obj[i].green * 255.0);
    blue  = (int)(m->obj[i].blue * 255.0);
    if (App->depth == 8){
      if ((App->objbase - i) > IMOD_MIN_INDEX)
	mapcolor(App->objbase - i, red, green, blue);
      m->obj[i].fgcolor = App->objbase - i;
    }
    else{
      mapcolor(i + App->objbase, red, green, blue);
      m->obj[i].fgcolor = App->objbase + i;
    }
  }
  mapNamedColors();
  imodDraw(App->cvi, IMOD_DRAW_COLORMAP);
  return;
}

// Map all the named colors.  The App members are set up in imod.cpp and
// the colors now come from preferences
void mapNamedColors()
{
  mapOneNamedColor(App->ghost);
  mapOneNamedColor(App->select);
  mapOneNamedColor(App->shadow);
  mapOneNamedColor(App->endpoint);
  mapOneNamedColor(App->bgnpoint);
  mapOneNamedColor(App->curpoint);
  mapOneNamedColor(App->foreground);
  mapOneNamedColor(App->background);
}

// Set a custom ghost color
void customGhostColor(int red, int green, int blue)
{
  mapcolor(App->ghost, red, green, blue); 
  b3dColorIndex(App->ghost);  
  
  /* if it's RGB, have to set the color here after the call */
  if (App->rgba)
    glColor3f(red/255., green/255., blue/255.);
}

// restore the ghost color after using it temporarily
void resetGhostColor(void)
{
  mapOneNamedColor(App->ghost);
}

// get one color from preferences and call mapcolor with it
static void mapOneNamedColor(int index)
{
  QColor qcol = ImodPrefs->namedColor(index);
  mapcolor(index, qcol.red(), qcol.green(), qcol.blue());
}

// Returns the IMOD_DRAW_MOD falg always, adds the IMOD_DRAW_XYZ flag and syncs
// xyzmouse to current model point if one is defined
static int rethink(ImodView *vw)
{
  Iobj   *obj;
  Icont  *cont;
  Ipoint *point;
  int     index;

  // DNM 1/24/03: replace use of Model with vw->imod and App->cvi with vw
     
  if ( (index = vw->imod->cindex.point) < 0){
    return(IMOD_DRAW_MOD);
  }

  cont = imodContourGet(vw->imod);
  if (cont == NULL){
    return(IMOD_DRAW_MOD);
  }
  if ((cont->pts == NULL) || ((int)cont->psize <= index)){
    return(IMOD_DRAW_MOD);
  }

  obj = imodObjectGet(vw->imod);
  if (iobjFlagTime(obj) && cont->time)
    ivwSetTime(vw, cont->time);

  point = &(cont->pts[index]);
  vw->xmouse = point->x;
  vw->ymouse = point->y;
  vw->zmouse = point->z;
  ivwBindMouse(vw);
  return(IMOD_DRAW_MOD|IMOD_DRAW_XYZ);
}

/* draw all windows in the display */
int imodDraw(ImodView *vw, int flag)
{
  int time, cx, cy, cz;
  static int lastz = -1, lastTime = -1;
  bool needModv = false;

  /*   SEE imod.h FOR DEFINITIVE LIST AND FULL DESCRIPTION:
   * IMOD_DRAW_IMAGE: image data or color map has changed; draw all images, 
   *                  clear caches
   * IMOD_DRAW_XYZ:   x,y,z position changed.
   * IMOD_DRAW_MOD:   model has changed.
   * IMOD_DRAW_SLICE: slice has changed in slicer
   * IMOD_DRAW_SKIPMODV: Skip drawing model view even though _MOD is set
   * IMOD_DRAW_RETHINK: Set current point from current model point if defined
   * IMOD_DRAW_NOSYNC:  Do not sync Zap window to current model point
   * IMOD_DRAW_COLORMAP: color index map has changed, do not combine with
   *                     other flags
   */

  /* Check for colormap change */
  if (flag & IMOD_DRAW_COLORMAP) {
    ivwControlListDraw(vw, IMOD_DRAW_COLORMAP);
    return 0;
  }

  /* Check for need to load colormap for colormapped images */
  ivwGetLocation(vw, &cx, &cy, &cz);
  ivwGetTime(vw, &time);
  if (vw->colormapImage && (cz != lastz || time != lastTime)) {
    if (vw->multiFileZ)
      xcramp_copyfalsemap(vw->imageList[cz + vw->li->zmin].colormap);
    else
      xcramp_copyfalsemap(&vw->image->colormap[768 * (cz + vw->li->zmin)]);
    xcramp_ramp(vw->cramp);
    lastz = cz;
    lastTime = time;
  }

  /* Check for black/white change on float, but only if image windows exist */
  if (vw->ctrlist && ilistSize(vw->ctrlist->list) && 
      imod_info_bwfloat(vw, cz, time) && App->rgba)
    flag |= IMOD_DRAW_IMAGE;            // DO WE NEED NOSYNC?

  if (flag & IMOD_DRAW_RETHINK)
    flag |= rethink(vw);

  if (flag & IMOD_DRAW_MOD)
    imod_info_setocp();

  /* DNM 11/24/02: deleted conditional on using controls, stopped drawing
     xyz window separately (it now has a control) */


  if (flag & (IMOD_DRAW_XYZ | IMOD_DRAW_MOD | IMOD_DRAW_IMAGE)) {
    imod_info_setxyz();
    needModv = imodvIsosurfaceUpdate(flag);
  }

  ivwControlListDraw(vw, flag);

  if (((flag & IMOD_DRAW_MOD) || 
       ((flag & IMOD_DRAW_IMAGE) && vw->ushortStore && Imodv->texMap) ||
       ((flag & IMOD_DRAW_XYZ) && (Imodv->texMap || Imodv->curPointExtraObj)) || needModv)
      && ! (flag & IMOD_DRAW_SKIPMODV))
    imodv_draw();

  scaleBarUpdate();
  return(0);
}


/*
 * 1/23/03: removed ivwGetImageType
 * DNM: this is not consistent with b3dgfx entry but is called by matchPoint
 */     



#define MAX_VISUALS 32
static ImodGLVisual glVisualTable[MAX_VISUALS];

static void imodAssessVisual(int ind, int db, int rgba, int depth, int askStereo, 
                             int alpha)
{
  int depthEnabled;
  QGLWidget *glw;
  GLint red, green, blue, depthBits;
  QGLFormat glFormat;
  GLboolean stereo;

  glFormat.setDoubleBuffer(db);
  glFormat.setRgba(rgba);
  glFormat.setDepth(depth > 0);
  glFormat.setAlpha(alpha > 0);
  glFormat.setStereo(askStereo > 0);
  glVisualTable[ind].dbRequested = db;
  glVisualTable[ind].rgbaRequested = rgba;
  glVisualTable[ind].depthEnabled = depth;
  glVisualTable[ind].validDirect = -1;
  glVisualTable[ind].colorBits = 0;
  glVisualTable[ind].depthBits = 0;
  glVisualTable[ind].doubleBuffer = 0;
  glVisualTable[ind].rgba = 0;
  glVisualTable[ind].alpha = 0;
  depthEnabled = 0;

  glw = new QGLWidget(glFormat);
  if (glw && glw->isValid()) {
    glw->makeCurrent();
    QGLFormat format = glw->format();

    // Record if it is direct, and actually doublebuffer, and alpha status
    glVisualTable[ind].validDirect = format.directRendering() ? 1 : 0;
    glVisualTable[ind].doubleBuffer = format.doubleBuffer() ? 1 : 0;
    glVisualTable[ind].alpha = format.alpha() ? 1 : 0;

    if (format.rgba()) {

      // rgba mode: get sum of red, green, blue
      glVisualTable[ind].rgba = 1;
      glGetIntegerv(GL_RED_BITS, &red);
      glGetIntegerv(GL_GREEN_BITS, &green);
      glGetIntegerv(GL_BLUE_BITS, &blue);
      glVisualTable[ind].colorBits = red + green + blue;
      glGetIntegerv(GL_ALPHA_BITS, &red);
    } else {

      // color index mode, get index size
      glVisualTable[ind].rgba = 0;
      glGetIntegerv(GL_INDEX_BITS, &red);
      glVisualTable[ind].colorBits = red;
    }       

    // Get stereo property
    glGetBooleanv(GL_STEREO, &stereo);
    glVisualTable[ind].stereo = (stereo == GL_TRUE) ? 1 : 0;
          
    // Get number of depth bits
    glGetIntegerv(GL_DEPTH_BITS, &depthBits);
    glVisualTable[ind].depthBits = depthBits;

    depthEnabled = format.depth() ? 1 : 0;
    delete glw;
  }
  if (Imod_debug)
    imodPrintStderr("for db %d rgb %d dep %d str %d al %d: dir %d db %d "
                    "rgb %d col %d dep %d ena %d str %d al %d\n", db, rgba,
                    depth, askStereo, alpha, glVisualTable[ind].validDirect,
                    glVisualTable[ind].doubleBuffer,  glVisualTable[ind].rgba,
                    glVisualTable[ind].colorBits, glVisualTable[ind].depthBits,
                    depthEnabled, glVisualTable[ind].stereo, glVisualTable[ind].alpha);
}

// Unused, for early experimentation
void imodAssessVisuals()
{
  int db, rgba, depth, ind;

  ind = 0;
  for (db = 0; db < 2; db++) {
    for (rgba = 0; rgba < 2; rgba++) {
      for (depth = 0; depth < 2; depth++) {
        imodAssessVisual(ind, db, rgba, depth, 0, 0);
        ind++;
      }
    }
  }
}


static int needToInitializeVTab = 1;

// Find the minimal visual that satisfies the request
ImodGLVisual *imodFindGLVisual(ImodGLRequest request)
{
  int i, ind;
  bool noDepthOK, depthOK;

  if (needToInitializeVTab) {
    for (i = 0; i < MAX_VISUALS; i++)
      glVisualTable[i].validDirect = -2;      
    needToInitializeVTab = 0;
  }

  // Index of the one requested without depth enabled or stereo: needed regardless
  ind = request.alpha * 16 + request.stereo * 8 + request.doubleBuffer * 4 +
    request.rgba * 2;
  if (glVisualTable[ind].validDirect == -2)
    imodAssessVisual(ind, request.doubleBuffer, request.rgba, 0, 0, request.alpha);

  // We are all set if we don't want depth or stereo and request is satisfied
  if (!request.depthBits && !request.stereo &&
      glVisualTable[ind].validDirect >= 0 &&
      request.colorBits <= glVisualTable[ind].colorBits)
    return &glVisualTable[ind];

  // Otherwise, need the one with depth enabled too
  if (glVisualTable[ind + 1].validDirect == -2)
    imodAssessVisual(ind + 1, request.doubleBuffer, request.rgba, 1, request.stereo,
                     request.alpha);

  // Evaluate overall suitability of each
  noDepthOK = glVisualTable[ind].validDirect >= 0 && 
    request.colorBits <= glVisualTable[ind].colorBits &&
    request.depthBits <= glVisualTable[ind].depthBits;
  depthOK = glVisualTable[ind + 1].validDirect >= 0 && 
    request.colorBits <= glVisualTable[ind + 1].colorBits &&
    request.depthBits <= glVisualTable[ind + 1].depthBits;

  // If both are OK, need to decide between them: 
  if (depthOK & noDepthOK) {

    // take the one with stereo if requested and only one has it
    if (request.stereo) {
      if (glVisualTable[ind].stereo && !glVisualTable[ind + 1].stereo)
        return &glVisualTable[ind];
      if (!glVisualTable[ind].stereo && glVisualTable[ind + 1].stereo)
        return &glVisualTable[ind + 1];
    }

    // take the one with alpha if requested and only one has it
    if (request.alpha) {
      if (glVisualTable[ind].alpha && !glVisualTable[ind + 1].alpha)
        return &glVisualTable[ind];
      if (!glVisualTable[ind].alpha && glVisualTable[ind + 1].alpha)
        return &glVisualTable[ind + 1];
    }

    // otherwise take the one with fewer
    // color bits; or if equal, then the one with fewer depth bits
    if (glVisualTable[ind].colorBits < glVisualTable[ind + 1].colorBits)
      return &glVisualTable[ind];
    else if (glVisualTable[ind].colorBits > glVisualTable[ind + 1].colorBits)
      return &glVisualTable[ind + 1];
    else if (glVisualTable[ind].depthBits <= glVisualTable[ind + 1].depthBits)
      return &glVisualTable[ind];
    else
      return &glVisualTable[ind + 1];

    // If only one OK, return one, the other, or neither
  } else if (depthOK)
    return &glVisualTable[ind + 1];
  else if (noDepthOK)
    return &glVisualTable[ind];
  else
    return NULL;
}

int imodFindQGLFormat(ImodApp *ap, char **argv)
{
  int i;
  ImodGLVisual *visual;

  for (i = 0; qtGLRequestList[i] != NULL; i++) {

    /* If want an rgb visual and this is not one, skip */
    if (ap->rgba > 0 && !qtGLRequestList[i]->rgba)
      continue;

    /* If want an ci visual and this is not one, skip */
    if (ap->rgba < 0 && qtGLRequestList[i]->rgba)
      continue;

    visual = imodFindGLVisual(*qtGLRequestList[i]);

    /* If it returns one, and it is rgb, make sure depth is at least 15 */
    if (visual && (!visual->rgba || visual->colorBits >= 15)) {
      ap->doublebuffer = visual->dbRequested;
      ap->rgba = visual->rgbaRequested;
      ap->qtEnableDepth = visual->depthEnabled;
      break;
    }
    visual = NULL;
  }
  if (!visual) {
    imodError(NULL, "%s: couldn't get appropriate GL format for Qt windows.\n"
              "Check whether your OpenGL drivers are properly installed and "
              "see if\n any other OpenGL-based programs run (on Linux, try "
              "glxgears and glxinfo)\nIf OpenGL drivers seem to be good, try "
              "setting an environment variable LIBGL_ALWAYS_INDIRECT to 1", argv[0]);
    exit(3);
  }
  return visual->colorBits;
}


/*  
 *  imod_display.c -- Open the display and setup visual and colormaps.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 * Log at end of file
 */

#include <qgl.h>
#include <qcursor.h>
#include <qbitmap.h>

#include "imod.h"
#include "b3dgfx.h"
#include "imod_input.h"
#include "imod_info_cb.h"
#include "imod_display.h"
#include "imodv.h"
#include "control.h"
#include "xcramp.h"
#include "preferences.h"
#include "dia_qtutils.h"
#include "scalebar.h"
#include "imodv_isosurface.h"

#ifdef Q_OS_MACX
#include "qcursor.mac.bits"
#include "qcursor_mask.mac.bits"
#else
#include "qcursor.bits"
#include "qcursor_mask.bits"
#endif

static void mapOneNamedColor(int index);

// The numbers are single/double buffer, rgba, colorBits, depthBits, stereo
static ImodGLRequest qtPseudo12DB = {1, 0, 12, 0, 0};
static ImodGLRequest qtPseudo12SB = {0, 0, 12, 0, 0};
static ImodGLRequest qtPseudo8DB = {1, 0, 8, 0, 0};
static ImodGLRequest qtPseudo8SB = {0, 0, 8, 0, 0};
static ImodGLRequest qtTrue24DB = {1, 1, 24, 0, 0};
static ImodGLRequest qtTrue24SB = {0, 1, 24, 0, 0};
static ImodGLRequest qtTrue12DB = {1, 1, 12, 0, 0};
static ImodGLRequest qtTrue12SB = {0, 1, 12, 0, 0};

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
    ap->cvi->black = ap->cvi->imod->blacklevel;
    ap->cvi->white = ap->cvi->imod->whitelevel;
  }
  
  if (ap->rgba){

    /* Set up ramp for rgba mode */
    ap->qColormap = NULL;
    ap->cvi->rampsize = 256;
    ap->cvi->rampbase = 0;
    ap->cvi->cramp = xcramp_allinit(ap->depth, ap->qColormap, 0, 255);
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
    ap->cvi->cramp = xcramp_allinit(ap->depth, ap->qColormap,
                                    RAMPMIN, RAMPMAX);
  }else{
    ap->objbase    = ap->base + 257;
    ap->cvi->rampsize = 256;
    ap->cvi->rampbase = ap->base;
    ap->cvi->cramp = xcramp_allinit(ap->depth, ap->qColormap,
                                    ap->base, ap->base + 255);
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


  if (flag & IMOD_DRAW_XYZ) {
    imod_info_setxyz();
    needModv = imodvIsosurfaceUpdate();
  }

  ivwControlListDraw(vw, flag);

  if (((flag & IMOD_DRAW_MOD) || 
       ((flag & IMOD_DRAW_XYZ) && (Imodv->texMap || Imodv->curPointExtraObj))||
       needModv) && ! (flag & IMOD_DRAW_SKIPMODV))
    imodv_draw();

  scaleBarUpdate();
  return(0);
}


/*
 * 1/23/03: removed ivwGetImageType
 * DNM: this is not consistent with b3dgfx entry but is called by matchPoint
 */     



#define MAX_VISUALS 8
static ImodGLVisual glVisualTable[MAX_VISUALS];

static void imodAssessVisual(int ind, int db, int rgba, int depth, 
                             int askStereo)
{
  int depthEnabled;
  QGLWidget *glw;
  GLint red, green, blue, depthBits;
  QGLFormat glFormat;
  GLboolean stereo;

  glFormat.setDoubleBuffer(db);
  glFormat.setRgba(rgba);
  glFormat.setDepth(depth > 0);
  glFormat.setStereo(askStereo > 0);
  glVisualTable[ind].dbRequested = db;
  glVisualTable[ind].rgbaRequested = rgba;
  glVisualTable[ind].depthEnabled = depth;
  glVisualTable[ind].validDirect = -1;
  glVisualTable[ind].colorBits = 0;
  glVisualTable[ind].depthBits = 0;
  glVisualTable[ind].doubleBuffer = 0;
  glVisualTable[ind].rgba = 0;
  depthEnabled = 0;

  glw = new QGLWidget(glFormat);
  if (glw && glw->isValid()) {
    glw->makeCurrent();
    QGLFormat format = glw->format();
    // Record if it is direct, and actually doublebuffer
    glVisualTable[ind].validDirect = 
      format.directRendering() ? 1 : 0;
    glVisualTable[ind].doubleBuffer = 
      format.doubleBuffer() ? 1 : 0;

    if (format.rgba()) {

      // rgba mode: get sum of red, green, blue
      glVisualTable[ind].rgba = 1;
      glGetIntegerv(GL_RED_BITS, &red);
      glGetIntegerv(GL_GREEN_BITS, &green);
      glGetIntegerv(GL_BLUE_BITS, &blue);
      glVisualTable[ind].colorBits = red + green + blue;
    } else {

      // color index mode, get index size
      glVisualTable[ind].rgba = 0;
      glGetIntegerv(GL_INDEX_BITS, &red);
      glVisualTable[ind].colorBits = red;

      // If pseudocolor is broken, disable this visual
#ifdef __sgi
      char *vendor = (char *)glGetString(GL_VENDOR);
      char *renderer = (char *)glGetString(GL_RENDERER);
      if (!strcmp(vendor, "SGI") && !strncmp(renderer, "VPRO", 4))
	glVisualTable[ind].validDirect = -1;
#endif

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
    imodPrintStderr("for db %d rgba %d dep %d stro %d: direct %d db %d "
                    "rgba %d color %d dep %d enab %d stro %d\n", db, rgba,
                    depth, askStereo, glVisualTable[ind].validDirect,
                    glVisualTable[ind].doubleBuffer,  glVisualTable[ind].rgba,
                    glVisualTable[ind].colorBits, glVisualTable[ind].depthBits,
                    depthEnabled, glVisualTable[ind].stereo);
}

// Unused, for early experimentation
void imodAssessVisuals()
{
  int db, rgba, depth, ind;

  ind = 0;
  for (db = 0; db < 2; db++) {
    for (rgba = 0; rgba < 2; rgba++) {
      for (depth = 0; depth < 2; depth++) {
        imodAssessVisual(ind, db, rgba, depth, 0);
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

  // Index of the one requested without depth enabled: needed regardless
  ind = request.doubleBuffer * 4 + request.rgba * 2;
  if (glVisualTable[ind].validDirect == -2)
    imodAssessVisual(ind, request.doubleBuffer, request.rgba, 0, 0);

  // We are all set if we don't want depth or stereo and request is satisfied
  if (!request.depthBits && !request.stereo &&
      glVisualTable[ind].validDirect >= 0 &&
      request.colorBits <= glVisualTable[ind].colorBits)
    return &glVisualTable[ind];

  // Otherwise, need the one with depth enabled too
  if (glVisualTable[ind + 1].validDirect == -2)
    imodAssessVisual(ind + 1, request.doubleBuffer, request.rgba, 1, 
                     request.stereo);

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
              "Make sure your display format is set for at least 15 bits "
              "of color\nAt least 24 bits of color is recommended for proper "
              "model viewing", argv[0]);
    exit(3);
  }
  return visual->colorBits;
}

/*

$Log$
Revision 4.27  2008/12/11 20:20:08  mast
Only skip the bwfloat if no image windows exists, not the rest of the draw

Revision 4.26  2008/12/01 15:39:31  mast
Do not call bwfloat if no images to display yet

Revision 4.25  2008/11/28 06:41:40  mast
Test on current point object in model view for model draw

Revision 4.24  2008/05/23 19:23:32  xiongq
Use multithreads to compute isosurface. Move the calling of imodvIsosurfaceUpdate() from imod_info_cb.cpp to imod_display.cpp.

Revision 4.23  2008/05/22 20:59:35  mast
Updated documentation of flags, referred to imod.h

Revision 4.22  2008/05/02 22:17:18  xiongq
add smoothing capability and adjust histgram acoording to intensity range of the input stack

Revision 4.21  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.20  2007/11/30 06:51:50  mast
Changes for linking slicer to model view

Revision 4.19  2007/11/16 23:11:52  mast
Added routines for redefining and restting ghost color

Revision 4.18  2006/09/12 15:40:24  mast
Handled contour member renames

Revision 4.17  2006/08/28 05:22:01  mast
Added colormap loading to redraw routine

Revision 4.16  2004/11/02 20:14:46  mast
Switch to get named colors from preferences for mapping

Revision 4.15  2004/07/11 18:22:39  mast
Do not set time when switching to contours with no time

Revision 4.14  2004/07/07 19:25:29  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 4.13  2004/06/09 14:11:17  mast
Suggested checking for display format when no GL visual available

Revision 4.12  2004/06/06 21:27:20  mast
Eliminated stereo-command related items

Revision 4.11  2004/06/05 23:46:38  mast
Set stereo in gl format when stereo requested - gets stereo visuals!

Revision 4.10  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.9  2004/04/27 14:53:13  mast
Use print info output for the assess visual debug output

Revision 4.8  2003/11/01 18:12:17  mast
changed to put out virtually all error messages to a window

Revision 4.7  2003/09/19 19:19:47  mast
Change required color depth for rgba visuals from 16 to 15

Revision 4.6  2003/05/18 22:59:13  mast
Remove icon-creating form here

Revision 4.5  2003/05/18 22:08:48  mast
Changes to add an application icon

Revision 4.4  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.3  2003/04/11 18:13:46  mast
Add small cursor for Mac

Revision 4.2  2003/03/03 22:10:55  mast
Make modeling cursor when open the display

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.12  2003/01/29 17:50:38  mast
New floating logic - float in imodDraw

Revision 1.1.2.11  2003/01/29 01:32:51  mast
changes for poor colormapping on SGI

Revision 1.1.2.10  2003/01/27 02:30:06  mast
Eliminate X includes

Revision 1.1.2.9  2003/01/26 23:22:16  mast
Qt version

Revision 1.1.2.8  2003/01/13 01:09:51  mast
got rid of cursor routine

Revision 1.1.2.7  2003/01/06 15:52:16  mast
changes for Qt version of slicer

Revision 1.1.2.6  2003/01/02 15:41:21  mast
add include of control.h

Revision 1.1.2.5  2003/01/01 05:41:31  mast
add stereo testing to qt visual selection

Revision 1.1.2.4  2002/12/30 06:38:49  mast
draw model view if image view is on

Revision 1.1.2.3  2002/12/17 18:39:12  mast
Implemented code for picking GL visuals for Qt

Revision 1.1.2.2  2002/12/14 17:53:13  mast
*** empty log message ***

Revision 1.1.2.1  2002/12/14 05:40:43  mast
new visual-assessing code

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/11/25 19:20:45  mast
In imodDraw, eliminated conditional on USE_IMOD_CONTROL and stopped drawing
xyz window separately (it is now in control list)

*/

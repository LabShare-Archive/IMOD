/*  IMOD VERSION 2.50
 *
 *  imod_display.c -- Open the display and setup visual and colormaps.
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
Log at end of file
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
#include "dia_qtutils.h"

#ifdef Q_OS_MACX
#include "qcursor.mac.bits"
#include "qcursor_mask.mac.bits"
#else
#include "qcursor.bits"
#include "qcursor_mask.bits"
#endif

char *ImodRes_SGIStereoCommand(void)
{
#ifdef __sgi
  return("/usr/gfx/setmon -n STR_TOP");
#else
  return(" ");
#endif
}
char *ImodRes_SGIRestoreCommand(void)
{
#ifdef __sgi
  return("/usr/gfx/setmon -n 1600x1024_72");
#else
  return(" ");
#endif
}

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
  QBitmap bmCursor(qcursor_width, qcursor_height, qcursor_bits, true);
  QBitmap bmMask(qcursor_width, qcursor_height, qcursor_mask_bits, true);
  ap->modelCursor = new QCursor(bmCursor, bmMask, qcursor_x_hot, 
                                qcursor_y_hot);

  diaSetTitle("Imod");
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


/* changes color of given pixel */
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
  /* Set up fixed indexes */
  ap->background   = IMOD_BACKGROUND;
  ap->foreground   = IMOD_FOREGROUND;
  ap->select       = IMOD_SELECT;
  ap->shadow       = IMOD_SHADOW;
  ap->endpoint     = IMOD_ENDPOINT;
  ap->bgnpoint     = IMOD_BGNPOINT;
  ap->curpoint     = IMOD_CURPOINT;
  ap->ghost        = IMOD_GHOST;
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
    mapcolor(ap->select,     255, 255,   0);
    mapcolor(ap->shadow,     128, 128,   0);
    mapcolor(ap->endpoint,   255,   0,   0);
    mapcolor(ap->bgnpoint,     0, 255,   0);
    mapcolor(ap->curpoint,   255,   0,   0);
    mapcolor(ap->foreground, 255, 255, 128);
    mapcolor(ap->background,  64,  64,  96);
    mapcolor(ap->ghost,       16, 16, 16);
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
  mapcolor(App->select,     255, 255,   0);
  mapcolor(App->shadow,     128, 128,   0);
  mapcolor(App->endpoint,   255,   0,   0);
  mapcolor(App->bgnpoint,     0, 255,   0);
  mapcolor(App->curpoint,   255,   0,   0);
  mapcolor(App->foreground, 255, 255, 128);
  mapcolor(App->background,  64,  64,  96);
  imodDraw(App->cvi, IMOD_DRAW_COLORMAP);
  return;
}


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
  if (iobjFlagTime(obj))
    ivwSetTime(vw, cont->type);

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

  /* 
   * IMOD_DRAW_IMAGE: image data or color map has changed; draw all images, 
   *                  clear caches
   * IMOD_DRAW_XYZ:   x,y,z position changed.
   * IMOD_DRAW_MOD:   model has changed.
   * IMOD_DRAW_SLICE: slice has changed in slicer
   * IMOD_DRAW_COLORMAP: color index map has changed, do not combine with
   *                     other flags
   */

  /* Check for colormap change */
  if (flag & IMOD_DRAW_COLORMAP) {
    ivwControlListDraw(vw, IMOD_DRAW_COLORMAP);
    return 0;
  }

  /* Check for black/white change on float */
  ivwGetLocation(vw, &cx, &cy, &cz);
  ivwGetTime(vw, &time);
  if (imod_info_bwfloat(vw, cz, time) && App->rgba)
    flag |= IMOD_DRAW_IMAGE;            // DO WE NEED NOSYNC?

  if (flag & IMOD_DRAW_RETHINK){
    flag |= rethink(vw);
  }

  if (flag & IMOD_DRAW_MOD){
    imod_info_setocp();
  }

  /* DNM 11/24/02: deleted conditional on using controls, stopped drawing
     xyz window separately (it now has a control) */

  ivwControlListDraw(vw, flag);

  if (flag & IMOD_DRAW_XYZ){
    imod_info_setxyz();
  }

  if (flag & IMOD_DRAW_MOD || (flag & IMOD_DRAW_XYZ && Imodv->texMap)) {
    imodv_draw();
  }



  return(0);
}


/*
 * 1/23/03: removed ivwGetImageType
 * DNM: this is not consistent with b3dgfx entry but is called by matchPoint
 */     



#define MAX_VISUALS 8
static ImodGLVisual glVisualTable[MAX_VISUALS];

static void imodAssessVisual(int ind, int db, int rgba, int depth)
{
  int depthEnabled;
  QGLWidget *glw;
  GLint red, green, blue, depthBits;
  QGLFormat glFormat;
  GLboolean stereo;

  glFormat.setDoubleBuffer(db);
  glFormat.setRgba(rgba);
  glFormat.setDepth(depth > 0);
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
    fprintf(stderr, "for db %d rgba %d depth %d: direct %d db %d "
            "rgba %d color %d depth %d enabled %d\n", db, rgba, depth, 
            glVisualTable[ind].validDirect,
            glVisualTable[ind].doubleBuffer,  glVisualTable[ind].rgba,
            glVisualTable[ind].colorBits, glVisualTable[ind].depthBits,
            depthEnabled);
}

void imodAssessVisuals()
{
  int db, rgba, depth, ind;

  ind = 0;
  for (db = 0; db < 2; db++) {
    for (rgba = 0; rgba < 2; rgba++) {
      for (depth = 0; depth < 2; depth++) {
        imodAssessVisual(ind, db, rgba, depth);
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
    imodAssessVisual(ind, request.doubleBuffer, request.rgba, 0);

  // We are all set if we don't want depth or stereo and request is satisfied
  if (!request.depthBits && !request.stereo &&
      glVisualTable[ind].validDirect >= 0 &&
      request.colorBits <= glVisualTable[ind].colorBits)
    return &glVisualTable[ind];

  // Otherwise, need the one with depth enabled too
  if (glVisualTable[ind + 1].validDirect == -2)
    imodAssessVisual(ind + 1, request.doubleBuffer, request.rgba, 1);

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

    /* If it returns one, and it is rgb, make sure depth is at least 16 */
    if (visual && (!visual->rgba || visual->colorBits >= 16)) {
      ap->doublebuffer = visual->dbRequested;
      ap->rgba = visual->rgbaRequested;
      ap->qtEnableDepth = visual->depthEnabled;
      break;
    }
    visual = NULL;
  }
  if (!visual) {
    fprintf(stderr, "%s: couldn't get appropriate GL format for Qt windows.\n",
            argv[0]);
    exit(-1);
  }
  return visual->colorBits;
}

/*
$Log$
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

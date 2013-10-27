/*
 *  model_draw.cpp -- Model draw, used only by the slicer window
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

#include "imod.h"
#include "display.h"
#include "b3dgfx.h"
#include "istore.h"
#include "sslice.h"
#include "finegrain.h"

static void imodDrawContourLines(ImodView * vi, Iobj *obj, int obnum, int co,
                                 GLenum mode);
static void imodDrawObjectSymbols(ImodView *vi, Iobj *obj);
static void imodDrawSpheres(ImodView *vi, Iobj *obj, float zscale);
static void imodDrawSymbol(Ipoint *point, int sym, int size, int flags, 
                           int linewidth);


void imodDrawModel(ImodView *vi, Imod *imod, int drawCurrent, float zscale)
{
  Iobj  *obj;
  Icont *cont;
  Ipoint *pnt;
  int imPtSize, modPtSize, backupSize, curSize;
  int ob, co, curob, curco, curpt;
  bool hasSpheres;

  if (imod->drawmode <= 0)
    return;

  imodGetIndex(imod, &curob, &curco, &curpt);
  for(ob = 0; ob < imod->objsize; ob++){
    obj = &imod->obj[ob];
    if (iobjOff(imod->obj[ob].flags))
      continue;

    imodSetObjectColor(ob);
    glLineWidth((GLfloat)obj->linewidth2);
    ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1);

    hasSpheres = iobjScat(obj->flags) || obj->pdrawsize;
    for (co = 0; co < obj->contsize && !hasSpheres; co++)
      hasSpheres = obj->cont[co].sizes != NULL;

    if (hasSpheres)
      imodDrawSpheres(vi, obj, zscale);
    
    if (!iobjScat(obj->flags)) {
      
      /* Draw all of the objects lines first. */
      for(co = 0; co < obj->contsize; co++){
	cont = &obj->cont[co];
	if (!cont->psize)
	  continue;
	
        /* DNM 2/21/03: don't draw contours from other times */
	if (ivwTimeMismatch(vi, 0, obj, cont))
          continue;

	/* todo: ghost contour ? */
	/* check ghostmode and surface ghostmode. */
	
	if (iobjOpen(obj->flags) || (cont->flags & ICONT_OPEN) ||
            (ob == curob && co == curco))
	  imodDrawContourLines(vi, obj, ob, co, GL_LINE_STRIP);
	else
	  imodDrawContourLines(vi, obj, ob, co, GL_LINE_LOOP);
      }
    }
    imodDrawObjectSymbols(vi, obj);
  }
     
  /* 7/3/07: switched to having slicer draw ends and current point symbol, but
     leave code in here just in case */
  if (!drawCurrent)
    return;

  /* draw ends of current contour. */
  /* draw current point symbol.    */
  obj = imodObjectGet(vi->imod);
  cont = imodContourGet(vi->imod);
  pnt = imodPointGet(vi->imod);
  utilCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);

  /* draw if current contour exists and it is not at wrong time */
  if (cont && cont->psize && !ivwTimeMismatch(vi, 0, obj, cont) && 
      vi->drawcursor) { 
    
    /* draw ends if more than one point */
    if (cont->psize > 1) {
      b3dColorIndex(App->bgnpoint);
      imodDrawSymbol(cont->pts, IOBJ_SYM_CIRCLE, modPtSize, 0, 
                     obj->linewidth2);
      b3dColorIndex(App->endpoint);
      imodDrawSymbol(&(cont->pts[cont->psize - 1]), IOBJ_SYM_CIRCLE, 
                     modPtSize, 0, obj->linewidth2);
    }

    /* draw current point in model mode */
    if (pnt && vi->imod->mousemode == IMOD_MMODEL) {
      b3dColorIndex(App->curpoint);
      curSize = modPtSize;
      if (cont->psize > 1 && 
          (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
        curSize = backupSize;
      imodDrawSymbol(pnt, IOBJ_SYM_CIRCLE, curSize, 0, obj->linewidth2);
    }
  }   
}

static void imodDrawContourLines(ImodView * vi, Iobj *obj, int obnum, int co,
                                 GLenum mode)
{
  Ipoint *point;
  Icont *cont = &obj->cont[co];
  DrawProps contProps, ptProps;
  int pt, lpt;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_LINE_COLOR | HANDLE_2DWIDTH;
  int scaleSizes = getSlicerThicknessScaling();
  bool selected = imodSelectionListQuery(vi, obnum, co) > -2;
     
  if (ifgGetValueSetupState())
    handleFlags |= HANDLE_VALUE1;

  point = cont->pts;
  lpt = cont->psize;
  nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                                   handleFlags, selected, scaleSizes);
  if (contProps.gap)
    return;

  utilEnableStipple(vi, cont);

  glBegin(GL_LINE_STRIP);
  for (pt = 0; pt < lpt; pt++, point++) {
    glVertex3fv((float *)point);
    ptProps.gap = 0;
    if (nextChange == pt)
      nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                       &ptProps, &stateFlags, &changeFlags,
                                       handleFlags, 0, scaleSizes);
    if (ptProps.gap) {
      glEnd();
      glBegin(GL_LINE_STRIP);
    }
  }
  if (mode == GL_LINE_LOOP && !ptProps.gap)
    glVertex3fv((float *)cont->pts);

  glEnd();
  utilDisableStipple(vi, cont);
}

static void imodDrawObjectSymbols(ImodView *vi, Iobj *obj)
{
  Icont  *cont;
  Ipoint *point;
  Ipoint vert;
  int co, pt, lpt;
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_LINE_COLOR | HANDLE_2DWIDTH;

  if (ifgGetValueSetupState())
    handleFlags |= HANDLE_VALUE1;

  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];

    if (ivwTimeMismatch(vi, 0, obj, cont))
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps,
                                     &stateFlags, handleFlags, 0);
    if (contProps.gap)
      continue;

    lpt = cont->psize;
    point = cont->pts;
    for (pt = 0; pt < lpt; pt++, point++){
      ptProps.gap = 0;
      if (nextChange == pt)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, 
                                         &changeFlags, handleFlags, 0);
      if (ptProps.symtype != IOBJ_SYM_NONE && 
          !(ptProps.gap && ptProps.valskip))
        imodDrawSymbol(point, ptProps.symtype, ptProps.symsize, 
                       ptProps.symflags, ptProps.linewidth2);
    }

    /* draw end markers for all kinds of contours */
    if (obj->symflags & IOBJ_SYMF_ENDS && lpt) {
      point = cont->pts;
      b3dColorIndex(App->bgnpoint);
      b3dLineWidth(contProps.linewidth2);

      for (pt = 0; pt < 2; pt++) {
        vert = *point;
        glBegin(GL_LINES);
        vert.x -= obj->symsize/2.;
        vert.y -= obj->symsize/2.;
	glVertex3fv((float *)&vert);
        vert.x += obj->symsize;
        vert.y += obj->symsize;
	glVertex3fv((float *)&vert);
        glEnd();

        glBegin(GL_LINES);
        vert.x -= obj->symsize;
	glVertex3fv((float *)&vert);
        vert.x += obj->symsize;
        vert.y -= obj->symsize;
	glVertex3fv((float *)&vert);
        glEnd();

        point = &(cont->pts[lpt - 1]);
        b3dColorIndex(App->endpoint);
      }
      
    }
  }
  return;
}

static void imodDrawSpheres(ImodView *vi, Iobj *obj, float zscale)
{
  Icont *cont;
  Ipoint *point;
  int pt, co;
  int stepRes;
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_LINE_COLOR;
  float zinv = 1.0f / zscale;
  GLdouble drawsize;
  GLuint listIndex;
#ifdef GLU_QUADRIC_HACK
  GLUquadricObj *qobj = gluNewQuadric();
#else
  static GLUquadricObj *qobj = NULL;
  if (!qobj)
    qobj = gluNewQuadric();
#endif

  /* The default is GLU_FILL, and it makes it dotty at the equator */
  /* But GLU_LINE does not give a circle on every slice */
  gluQuadricDrawStyle(qobj, GLU_FILL);

  /* Make a display list for the default size */
  drawsize = obj->pdrawsize / vi->xybin;
  if (drawsize) {
    stepRes = (int)(drawsize < 5 ? drawsize + 4 : 8);
    listIndex = glGenLists(1);
    glNewList(listIndex, GL_COMPILE);
    gluSphere(qobj, drawsize , stepRes * 2, stepRes);
    glEndList();
  }

  if (ifgGetValueSetupState())
    handleFlags |= HANDLE_VALUE1;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();



  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (!cont->psize)
      continue;
    if (ivwTimeMismatch(vi, 0, obj, cont))
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, 0);
    if (contProps.gap)
      continue;
    for (pt = 0, point = cont->pts; pt < cont->psize; pt++, point++) {
      ptProps.gap = 0;
      if (nextChange == pt)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, 
                                         &changeFlags, handleFlags, 0);
      drawsize = imodPointGetSize(obj, cont, pt)  / vi->xybin;
      if (!drawsize || (ptProps.gap && ptProps.valskip))
	continue;
      glPushMatrix();
      glTranslatef(point->x, point->y, point->z);

      // Undo the z-scaling that is applied later to get points in the right
      // place, to get round spheres
      glScalef(1.0f, 1.0f, zinv);

      /* Use the display list if default; otherwise individual size */
      if (drawsize == obj->pdrawsize)
	glCallList(listIndex);
      else {
	stepRes = (int)(drawsize < 5 ? drawsize + 4 : 8);
	gluSphere(qobj, drawsize , stepRes * 2, stepRes);
      }
      glPopMatrix();
    }
  }
  if (obj->pdrawsize)
    glDeleteLists(listIndex, 1);

  glPopMatrix();
#ifdef GLU_QUADRIC_HACK
  gluDeleteQuadric(qobj);
#endif
}

void imodDrawSymbol(Ipoint *point, int sym, int size, int flags, int linewidth)
{
  double inner, outer;
  int slices, loops;
  Ipoint vert;
#ifdef GLU_QUADRIC_HACK
  GLUquadricObj *qobj;
  if (sym == IOBJ_SYM_CIRCLE)
    qobj = gluNewQuadric();
#else
  static GLUquadricObj *qobj = NULL;
  if (!qobj)
    qobj = gluNewQuadric();
#endif

  switch(sym){
          
  case IOBJ_SYM_CIRCLE:
    outer = size;
    if (flags  & IOBJ_SYMF_FILL)
      inner = 0.0;
    else
      inner = outer - linewidth;
    slices = (int)(outer + 4);
    loops = 1;
    glPushMatrix();
    glTranslatef(point->x, point->y, point->z);
    gluDisk(qobj, inner, outer, slices, loops);
    glPopMatrix();
    break;
          
  case IOBJ_SYM_SQUARE:
    glBegin((flags & IOBJ_SYMF_FILL) ? GL_POLYGON : GL_LINE_LOOP);
    vert = *point;
    vert.x -= (size/2);
    vert.y -= (size/2);
    glVertex3fv((float *)&vert);
    vert.x += size;
    glVertex3fv((float *)&vert);
    vert.y += size;
    glVertex3fv((float *)&vert);
    vert.x -= size;
    glVertex3fv((float *)&vert);
    glEnd();
    break;
          
  case IOBJ_SYM_TRIANGLE:
    glBegin((flags & IOBJ_SYMF_FILL) ? GL_POLYGON : GL_LINE_LOOP);
    vert = *point;
    vert.y += size;
    glVertex3fv((float *)&vert);
    vert.x += size;
    vert.y -= (size + (size/2));
    glVertex3fv((float *)&vert);
    vert.x -= 2 * size;
    glVertex3fv((float *)&vert);
    glEnd();
    break;
          
  case IOBJ_SYM_STAR:
    break;

  case IOBJ_SYM_NONE:
    glBegin(GL_POINTS);
    glVertex3fv((float *)point);
    glEnd();
    break;
          

  default:
    break;
  }
     
#ifdef GLU_QUADRIC_HACK
  if (sym == IOBJ_SYM_CIRCLE)
    gluDeleteQuadric(qobj);
#endif
  return;
}


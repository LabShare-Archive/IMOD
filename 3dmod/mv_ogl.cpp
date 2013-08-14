/*
 *  mv_ogl.cpp -- OpenGL Drawing functions to draw models, etc in imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include "imod.h"
#include "vertexbuffer.h"
#include "imodv.h"
#include "mv_image.h"
#include "imod_edit.h"
#include "mv_gfx.h"
#include "mv_ogl.h"
#include "mv_objed.h"
#include "mv_input.h"
#include "mv_light.h"
#include "mv_modeled.h"
#include "mv_stereo.h"
#include "istore.h"
#include "finegrain.h"

#define DRAW_POINTS 1
#define DRAW_LINES  2
#define DRAW_FILL   3
#define DRAW_OBJECT -1
#define NO_NAME 0xffffffff

enum {SUBSET_NONE = 0, SUBSET_OBJ_ONLY, SUBSET_SURF_ONLY, SUBSET_SURF_OTHER, 
      SUBSET_CONT_ONLY, SUBSET_CONT_OTHER, SUBSET_PNT_OTHER};

static int begCount = 0;
void myGlEnd()
{
  imodPrintStderr("End %d\n", begCount--);
  glEnd();
  GLenum error = glGetError();
  if (error)
    imodPrintStderr("GL error: %s\n", gluErrorString(error));
}

void myGlVertex3f( GLfloat x, GLfloat y, GLfloat z )
{
  imodPrintStderr("Vertex %.1f %.1f %.1f\n", x, y, z);
  glVertex3f(x,y,z);
}

void myGlBegin( GLenum mode )
{
  imodPrintStderr("Begin mode %d  count %d\n", mode, begCount++);
  glBegin(mode);
}

// For debugging some calls
//#define glEnd myGlEnd
//#define glBegin myGlBegin
//#define glVertex3f myGlVertex3f

static void imodvDraw_spheres(Iobj *obj, double zscale, int style, 
                              int drawTrans);
static void imodvDraw_mesh(Imesh *mesh, int style, Iobj *obj, int drawTrans);
static void imodvDraw_filled_mesh(Imesh *mesh, double zscale, Iobj *obj, 
                                  int drawTrans);
static void imodvDrawScalarMesh(Imesh *mesh, double zscale, Iobj *obj, 
                                int drawTrans);
static void imodvDraw_contours(Iobj *obj, int mode, int drawTrans);
static void imodvPick_Contours(Iobj *obj, double zscale, int drawTrans);
static void imodvSetDepthCue(Imod *imod);
static void imodvSetViewbyModel(ImodvApp *a, Imod *imod);
static void setStereoProjection(ImodvApp *a);
static void imodvDraw_filled_contours(Iobj *obj, int drawTrans);
static void imodvDraw_object(Iobj *obj, Imod *imod, int drawTrans);
static int checkMeshDraw(Imesh *mesh, int checkTime, int resol);
static void imodvSetObject(Imod *imod, Iobj *obj, int style, int drawTrans);
static void set_curcontsurf(int ob, Imod* imod);
static void imodvUnsetObject(Iobj *obj);
static int clip_obj(Imod *imod, Iobj *obj, int flag);
static int skipNonCurrentSurface(Imesh *mesh, int *ip, Iobj *obj);
static void drawCurrentClipPlane(ImodvApp *a);

static int sCTime = -1;
static float sDepthShift;
static int sCursurf, sCurCont;
static int sThickCont, sThickObj, sObjBeingDrawn, sModBeingDrawn;

static void imodvSetViewbyModel(ImodvApp *a, Imod *imod)
{
  Iview *vw;
  GLint vp[4];
  double cdist;
  double xs, ys;       /* window scaling factors. */
  double zn, zf;
  double znear, zfar;  /* clip in z. */
  double fovytan;
  double rad;
  float scale = 1.0f;
  double kickFac = 3.;    /* Default amount to kick extreme planes out */
  double extraKick = 10.; /* Extra amount when kick checked */
  Iobj *xobj;
  Ipoint *curPnt;

  if (!a->imod) return;

  imodv_winset(a);
  vw = imod->view;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  if (vw->world & WORLD_KICKOUT_CLIPS)
    kickFac *= extraKick;

  if (a->doPick){
    glGetIntegerv(GL_VIEWPORT, vp);
    gluPickMatrix(a->xPick, a->yPick, a->wPick, a->hPick, vp);
  }
     
  rad = vw->rad;
  if (rad < 0.0f)
    rad *= -1.0f;

  // fovy is the angular field of view so take tangent of half of the angle
  fovytan = tan((double)vw->fovy * 0.0087266463);
  rad /= (1.0 + 3.1415927*fovytan);

  /* check for window area */
  if ((!Imodv->winx) || (!Imodv->winy)) return;
  xs = (double)a->winx;
  ys = (double)a->winy;

  // Set up current point now
  if (a->curPointExtraObj > 0) {
    xobj = ivwGetAnExtraObject(a->vi, a->curPointExtraObj);
    if (xobj) {
      vbCleanupVBD(xobj);
      curPnt = imodPointGet(imod);
      if (curPnt && imod->mousemode == IMOD_MMODEL) 
        xobj->cont[0].pts[0] = *curPnt;
      else {
        xobj->cont[0].pts[0].x = a->vi->xmouse;
        xobj->cont[0].pts[0].y = a->vi->ymouse;
        xobj->cont[0].pts[0].z = a->vi->zmouse;
      }
      scale = 0.5 * B3DMIN(xs, ys) / rad;
      imodPointSetSize(&xobj->cont[0], 0, xobj->pdrawsize / scale);
    }
  } 

  // Resume setting up scale
  if (xs < ys){
    scale = ys/xs;
    xs = rad;
    ys = xs * scale;
  }else{
    scale = xs/ys;
    ys = rad;
    xs = ys * scale;
  }

  /* DNM: this test makes L/R and T/B stereo come out too small */
  /*     if ((Imodv->stereo!= IMODV_STEREO_OFF) &&
         (Imodv->stereo!= IMODV_STEREO_HW) &&
         (Imodv->stereo!= -IMODV_STEREO_HW)){
         xs *= 2.0;
         ys *= 2.0;
         }
  */

  if (xs < ys){
    zfar  = ys;
    znear = -zfar;
  }else{
    zfar  = xs;
    znear = -zfar;
  }

  imodvSetDepthCue(imod);

  znear *= 5.;
  zfar *= 5.;

  // DNM 6/9/04: simplified a bit and kicked the clipping planes way out
  // if no clipping is selected
  // These are clipping plane locations relative to 0 in the middle
  cdist = zfar - znear;
  znear += cdist * vw->cnear;
  zfar  -= cdist * (1.0 - vw->cfar);

  // Kick the far plane out unconditionally, the near plane only for
  // parallel projection
  if (vw->cfar == 1.0f)
    zfar += cdist * kickFac;

  if (vw->fovy < 1.0f) {
    if (!vw->cnear)
      znear -= cdist * kickFac;
    glOrtho(-xs, xs, -ys, ys,
            znear + sDepthShift , zfar + sDepthShift);
    return;
  }
     
  // For perspective, the near value has to be radius over tangent of 
  // half-angle, so then get the shift that puts znear at that location
  // and adjust zfar also
  zn = rad/fovytan;
  cdist = zn - znear;
  zf = zfar + cdist;

  glFrustum(-xs, xs, -ys, ys, zn, zf); 

  /*
    imodPrintStderr("frustum xy = %g %g : znf = %g %g : cd = %g\n",
    xs, ys, zn, zf, cdist);
  */

  glTranslatef(0.0, 0.0, (float) (-cdist + sDepthShift));
  return;
}

static void imodvSetDepthCue(Imod *imod)
{
  Iview *vw = imod->view;
  Ipoint maxp, minp;
  float bgcolor[4];
  float fstart, fend;
  float drange;
  double zscale = imod->zscale;
  if (!Imodv->standalone)
    zscale = (imod->zscale * Imodv->vi->zbin) / Imodv->vi->xybin;
  if (imod->view->world & VIEW_WORLD_INVERT_Z)
    zscale = -zscale;
 
  sDepthShift = 0.0f;

  if (!(vw->world & VIEW_WORLD_DEPTH_CUE)){
    glDisable(GL_FOG);
    return;
  }
   

  /* We want drawn colors to blend into the background
   * so adjust fog color to the current background clear color.
   */
  glGetFloatv(GL_COLOR_CLEAR_VALUE, bgcolor);
  glFogfv(GL_FOG_COLOR, bgcolor);

  // DNM 11/16/04: Get model extent.  If there is none yet, set to image size
  // so depth cue will work correctly on image display
  imodGetBoundingBox(imod, &minp, &maxp);
  if (maxp.x == minp.x && maxp.y == minp.y && maxp.z == minp.z &&
      !Imodv->standalone) {
    minp.x = minp.y = minp.z = 0.;
    maxp.x = Imodv->vi->xsize;
    maxp.y = Imodv->vi->ysize;
    maxp.z = Imodv->vi->zsize;
  }

  maxp.z *= zscale;
  minp.z *= zscale;
  drange = (maxp.x - minp.x) * (maxp.x - minp.x) +
    (maxp.y - minp.y) * (maxp.y - minp.y) +
    (maxp.z - minp.z) * (maxp.z - minp.z);
  drange = (float)sqrt((double)drange);

  sDepthShift = 0.6 * drange;
   
  fstart = 1.2 * drange * vw->dcstart ;
  fend = 1.2 * drange * vw->dcend ;
   
  /*   imodPrintStderr("OGL Depth Cue start: %g       end: %g   range: %g\n",
       fstart, fend, drange); */

  glFogi(GL_FOG_MODE, GL_LINEAR);
  glFogf(GL_FOG_DENSITY, 1.0);
  glFogf(GL_FOG_START, fstart);
  glFogf(GL_FOG_END, fend);
  glEnable(GL_FOG);

  return;
}

static void imodvSetModelTrans(Imod *imod)
{
  Iview *vw;
  float zscale;
  if (!imod) return; 

  zscale = imod->zscale;
  if (!Imodv->standalone)
    zscale = (imod->zscale * Imodv->vi->zbin) / Imodv->vi->xybin;
  if (imod->view->world & VIEW_WORLD_INVERT_Z)
    zscale = -zscale;

  vw = imod->view;
  glMatrixMode(GL_MODELVIEW);

  glLoadIdentity();

  /* DNM: The last operation is a translation to get positive Z's if doing
     depth cueing */

  glTranslatef(0.0f, 0.0f, -sDepthShift);

  imodMatUniqueRotationPt(&vw->rot);

  glRotatef(vw->rot.x, 1.0f, 0.0f, 0.0f);
  glRotatef(vw->rot.y, 0.0f, 1.0f, 0.0f); 
  glRotatef(vw->rot.z, 0.0f, 0.0f, 1.0f); 
     
  glTranslatef(vw->trans.x, vw->trans.y, (vw->trans.z * zscale));
  glScalef(vw->scale.x, vw->scale.y, vw->scale.z * zscale);

  return;
}

// Set up the projection matrix properly for the current stereo mode
static void setStereoProjection(ImodvApp *a)
{
  float angle = a->plax * 0.5f;
  if (a->texMap && a->imageStereo)
    angle = 0.;

  glMatrixMode(GL_PROJECTION);

  switch (a->stereo){
  case -IMODV_STEREO_RL:
    glViewport(0, 0, a->winx, a->winy);
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(angle, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    break;

  case IMODV_STEREO_RL:
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(-angle, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    glViewport(a->winx, 0, a->winx, a->winy);
    break;

  case -IMODV_STEREO_TB:
    glViewport(0, -(imodvStereoVoffset() / 2), a->winx, a->winy);
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(angle, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    glScalef(1.0f, 0.5f, 0.5f);
    break;

  case IMODV_STEREO_TB:
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(-angle, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    glScalef(1.0f, 0.5f, 0.5f);
    glViewport(0, a->winy + imodvStereoVoffset() - (imodvStereoVoffset() / 2),
               a->winx, a->winy);
    break;

  case -IMODV_STEREO_HW:
    glRotatef(-angle, 0.0f, 1.0f, 0.0f);
#ifdef __sgi
    /* DNM: cut these values in half to get model at same zoom relative
       to the size of the window */
    glScalef(1.0f, 0.5f, 0.5f);
#endif
    break;

  case IMODV_STEREO_HW:
    glRotatef(angle, 0.0f, 1.0f, 0.0f);
#ifdef __sgi
    glScalef(1.0f, 0.5f, 0.5f);
#endif
    break;

  }
}

/*
 * Draw all models and image
 */
void imodvDraw_models(ImodvApp *a)
{
  int m, mstart, mend;

  /* DNM 11/30/02: change all glPushName(-1) to NO_NAME since it is supposed
     to be unsigned */
  glPushName(NO_NAME);
     
  // DNM 5/15/04: tell routine to draw non-transparent stuff, add modelTrans
  // call to get transformation matrix correct
  if (!a->standalone){
    imodvSetViewbyModel(a, a->imod);
    imodvSetModelTrans(a->imod);
    setStereoProjection(a);
    imodvDrawImage(a, 0);
  }

  imodvModelDrawRange(a, mstart, mend);
  for (m = mstart; m <= mend; m++){
    sModBeingDrawn = m;
    glLoadName(m);
    imodvDraw_model(a, a->mod[m]);
  }

  // DNM 5/14/04: now tell routine to draw transparent stuff
  if (!a->standalone){
    imodvSetViewbyModel(a, a->imod);
    imodvSetModelTrans(a->imod);
    setStereoProjection(a);
    imodvDrawImage(a, 1);
  }
     
  glPopName();

  // 3/2/12: flushes and finishes should not be needed when double buffering.
  // New practice, do a finish only here and only if single-buffer or picking
  if (!a->dblBuf || a->doPick)
    glFinish();
}

// Sets the values for current contour and surface and object being drawn,
// and object and contour number if contour is to be drawn thick
static void set_curcontsurf(int ob, Imod* imod)
{
  Iobj  *obj;
  sCurCont = -1;
  sCursurf = -1;
  sObjBeingDrawn = ob;
  if (ob >= 0)
    obj  = &(imod->obj[ob]);
  else
    obj = ivwGetAnExtraObject(Imodv->vi, -ob - 1);
  if (Imodv->current_subset && imod->cindex.object == ob) {
    sCurCont = imod->cindex.contour;
    if (sCurCont >= 0)
      sCursurf = imod->obj[imod->cindex.object].cont[sCurCont].surf;
  }
  
  // Set contour to thicken if flag set and either we are in the current object
  // or there is a non-empty selection list
  sThickCont = -1;
  sThickObj = -1;
  if (obj->flags & IMOD_OBJFLAG_THICK_CONT) {
    if (imod->cindex.object == ob || ilistSize(Imodv->vi->selectionList))
      sThickCont = imod->cindex.contour;
    if (imod->cindex.object == ob) 
      sThickObj = ob;
  }
}

bool imodvCheckThickerContour(int co)
{
  return ((co == sThickCont && sObjBeingDrawn == sThickObj )|| 
          (sThickCont >= 0 && imodSelectionListQuery
           (Imodv->vi, sObjBeingDrawn, co) > -2));
}

/*
 *  Draw this model.
 */
void imodvDraw_model(ImodvApp *a, Imod *imod)
{
  int ob = -1;
  Iobj *obj;
  int obstart, obend, drawTrans, loop, nloop;

  if (imod == NULL)
    return;

  if (imod->obj == NULL)
    return;

  /* DNM: move the call to "SetView" into the model loop in order to have
     it handle all models independently; this requires the additional
     steps for stereo display to happen inside this routine */

  imodvSetViewbyModel(a, imod);

  imodvSetModelTrans(imod);

  setStereoProjection(a);

  imodvSetLight(imod->view);

  sCTime = imod->ctime;
  glPushName(ob);

  nloop = a->vi->numExtraObj > 0 ? 2 : 1;

  /* DNM: draw objects without transparency first; then make the depth
     buffer read-only and draw objects with transparency. Anti-aliased
     lines seem to do well enough being drawn in the first round, so
     don't try to hold those until the second round
     In the first round, the drawing routines will set the temp flag if there
     is something to draw in the second round. */
  for (drawTrans = 0; drawTrans < 2; drawTrans++) {

    /* If displaying a current subset, set up object limits */
    obstart = 0;
    obend = imod->objsize;
    if ((a->current_subset == SUBSET_OBJ_ONLY || a->current_subset == SUBSET_SURF_ONLY || 
         a->current_subset == SUBSET_CONT_ONLY )&& imod->cindex.object >= 0) {
      obstart = imod->cindex.object;
      obend = obstart + 1;
    }

    // If drawing only extra objects, set up to skip all regular ones
    if (a->drawExtraOnly)
      obend = obstart - 1;

    // Loop on regular objects then on extra objects
    for (loop = 0; loop < nloop; loop++) {
      for (ob = obstart; ob < obend; ob++) {
        if (ob >= 0)
          obj = &(imod->obj[ob]);
        else {
          obj = ivwGetAnExtraObject(a->vi, -1 - ob);
          if (!obj || (!obj->contsize && !obj->meshsize) || 
              !(obj->flags & IMOD_OBJFLAG_EXTRA_MODV))
            continue;
        }
        if (!drawTrans)
          obj->flags &= ~IMOD_OBJFLAG_TEMPUSE;
        if (!iobjOff(obj->flags) && 
            (!drawTrans || (obj->flags & IMOD_OBJFLAG_TEMPUSE))) {
          set_curcontsurf(ob, imod);
          glLoadName(ob);
          clip_obj(imod, obj, 1);
          imodvDraw_object( obj , imod, drawTrans);
          clip_obj(imod, obj, 0);
          //glFinish();
        }
      }
      if (nloop == 2) {
        obstart = -a->vi->numExtraObj;
        obend = 0;
      }
    }
    glDepthMask(GL_FALSE); 
  }
  glDepthMask(GL_TRUE); 
  //glFinish();
  
  glPopName();
  if (a->drawClip && imod == a->imod)
    drawCurrentClipPlane(a);
  if (imod == a->imod) {
      ImodvCurModLight.x = Imodv_light_position[0];
      ImodvCurModLight.y = Imodv_light_position[1];
      ImodvCurModLight.z = Imodv_light_position[2];
  }
}

/*
 * Set up clipping planes for the given object
 */
static int clip_obj(Imod *imod, Iobj *obj, int flag)
{
  GLdouble params[4];
  int cpn = 0, clipSet, ip;
  IclipPlanes *clips = &obj->clips;
  GLint maxPlanes;
  int numSets = (clips->flags & (1 << 7)) ? 1 : 2;
  glGetIntegerv(GL_MAX_CLIP_PLANES, &maxPlanes);
  //imodPrintStderr("Max planes %d\n", maxPlanes);

  /* Loop through the two sets (object then global), loop through the planes in
     each set, and for each one that is on up to the limit, either turn on the
     plane or disable it based on the flag */
  for (clipSet = 0; clipSet < numSets; clipSet++) {
    for (ip = 0; ip < clips->count; ip++) {
      if ((clips->flags & (1 << ip)) && cpn < maxPlanes) {
        if (flag) {
          params[0] = clips->normal[ip].x;
          params[1] = clips->normal[ip].y;
          params[2] = clips->normal[ip].z;

          /* DNM 10/13/05: this parameter is evidently supposed to be the
             negative of this product for the plane equation Ax+By+Cz+D=0,
             which is why clip points are all maintained as negative values */
          params[3] = (clips->normal[ip].x * clips->point[ip].x) +
            (clips->normal[ip].y * clips->point[ip].y) +
            (clips->normal[ip].z * clips->point[ip].z);
          /* imodPrintStderr("clip set %d plane %d params = %g %g %g %g\n", 
            clipSet, ip, params[0], params[1], params[2], params[3]); */
          
          glClipPlane( GL_CLIP_PLANE0 + cpn, params); 
          glEnable(GL_CLIP_PLANE0 + cpn);
        } else {
          glDisable(GL_CLIP_PLANE0 + cpn);
        }
        cpn++;
      }
    }
    clips = &imod->view->clips;
  }
  /*if (flag)
    glFlush();*/
  return(0);
}

/*
 * In which any mode enabled by imodvSetObject should be disabled, including restoring
 * polygon mode to fill, needed for texture drawing
 */
static void imodvUnsetObject(Iobj *obj)
{
  light_off();
  glDisable(GL_BLEND);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  return;
}

/*
 * Set various OpenGL modes based upon drawing needs of given object
 */
static void imodvSetObject(Imod *imod, Iobj *obj, int style, int drawTrans)
{
  float red, green, blue, trans;
     
  trans = 1.0f - (obj->trans * 0.01f);
  switch(style){
  case 0:
    imodvUnsetObject(obj);
    /* DNM 11/30/01: need to return, not break */
    return;

  case DRAW_POINTS:
    glPointSize(obj->linewidth);
  case DRAW_LINES:
    if (IMOD_OBJFLAG_ANTI_ALIAS & obj->flags) {
      glEnable(GL_LINE_SMOOTH);
      drawTrans = 1;
    }
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    glLineWidth((float)obj->linewidth);
    glColor4f(obj->red, obj->green, obj->blue, trans);
    break;

  case DRAW_FILL:
    glLineWidth(obj->linewidth);
    glPointSize(obj->linewidth);

    if (Imodv->wireframe)
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    else
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    if (obj->flags & IMOD_OBJFLAG_TWO_SIDE)
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 1);
    else
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 0);
    if (imod->view->world & VIEW_WORLD_INVERT_Z)
      glFrontFace(GL_CW);
    else
      glFrontFace(GL_CCW);

    if (obj->flags & IMOD_OBJFLAG_FCOLOR){
      red   = (float)obj->fillred / 255.0f;
      green = (float)obj->fillgreen / 255.0f;
      blue  = (float)obj->fillblue / 255.0f;
      glColor4f(red, green, blue, trans);
    }else{
      glColor4f(obj->red, obj->green, obj->blue, trans);
    }

    if ((Imodv->lighting) && (!Imodv->wireframe)){
      light_on(obj, sModBeingDrawn);
    }else{
      light_off();
    }
    break;

    /* DNM: There was an entry for DRAW_OBJECT that recursively called
       this routine with the individual types; it was obsolete and was 
       called only from imodv_dlist which is unused */

  }

  /* DNM 11/11/00: The recommended glBlendFunc entries, used below, look
     horrible when there are overlapping back faces, because sometimes the
     backs are drawn first, and sometimes not.  One solution was to use
     (GL_SRC_ALPHA, GL_DST_ALPHA) with alpha planes, or (GL_SRC_ALPHA,
     GL_ONE) with no alpha planes, but this made things behind the 
     transparent surface too bright.  The other solution is to cull the
     back faces of the transparent object unless two-sided lighting is
     used.  This works regardless of alpha planes, so there is no need
     to request alpha */
  /* DNM 11/29/11: Now that there is alpha turned on just to get transparent backgrounds,
     and multisampling, tried glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, 
     GL_ONE_MINUS_DST_ALPHA, GL_ONE) as used in Chimera, with an alphaVisual.  Saw 
     NO difference in the display itself. */
  if (drawTrans){
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (!(obj->flags & IMOD_OBJFLAG_TWO_SIDE) && style == DRAW_FILL)
      glEnable(GL_CULL_FACE);
  }
  return;
}

/* A simple function to check whether the current mesh should be drawn, based
   on time, surface number, and resolution */
static int checkMeshDraw(Imesh *mesh, int checkTime, int resol)
{
  if ((checkTime) && (mesh->time) && (mesh->time != sCTime))
    return 0;
  if ((Imodv->current_subset == SUBSET_SURF_ONLY || Imodv->current_subset == 
       SUBSET_SURF_OTHER) && sCursurf >= 0 && mesh->surf > 0 && mesh->surf != sCursurf)
    return 0;
  if (imeshResol(mesh->flag) == resol)
    return 1;
  return 0;
}

/* And to check whether a contour should be drawn */
int imodvCheckContourDraw(Icont *cont, int co, int checkTime)
{
  if (!cont->psize) 
    return 0;
  if ((checkTime) && (cont->time) && (cont->time != sCTime))
    return 0;
  if ((Imodv->current_subset == SUBSET_SURF_ONLY || Imodv->current_subset == 
       SUBSET_SURF_OTHER) && sCursurf >= 0 && cont->surf != sCursurf)
    return 0;

  if ((Imodv->current_subset == SUBSET_CONT_ONLY || Imodv->current_subset == 
       SUBSET_CONT_OTHER || Imodv->current_subset == SUBSET_PNT_OTHER) && 
      sCurCont >= 0 && co != sCurCont &&
      imodSelectionListQuery(Imodv->vi, Imodv->imod->cindex.object, co) < 0)
    return 0;
  if (Imodv->current_subset == SUBSET_PNT_OTHER && co == sCurCont)
    return 2;
  return 1;
}

/*
 * Draw an object: determine the types of draws to be done and call routines
 */
static void imodvDraw_object(Iobj *obj, Imod *imod, int drawTrans)
{
  int co, resol, flagSave;
  double zscale;
  Imesh *mesh;
  int checkTime = (int)iobjTime(obj->flags);
  bool skipSpheres = (obj->flags & IMOD_OBJFLAG_PNT_NOMODV) && 
    iobjMesh(obj->flags);
  bool hasSpheres = (iobjScat(obj->flags) || 
                     (obj->pdrawsize > 0 && !skipSpheres)) && 
    obj->contsize > 0;

  if (!sCTime)
    checkTime = 0;

  if (!obj)
    return;

  if (iobjOff(obj->flags))
    return;

  if (!obj->contsize && !obj->meshsize)
    return;

  zscale = imod->zscale;
  if (!Imodv->standalone)
    zscale = (imod->zscale * Imodv->vi->zbin) / Imodv->vi->xybin;

  // Check for individual point sizes if not scattered and no sphere size
  if (!hasSpheres && !skipSpheres) {
    for (co = 0; co < obj->contsize; co++) {
      if (obj->cont[co].sizes) {
        hasSpheres = true;
        break;
      }
    }
  }

  /* DNM: Made sure there was an "imodvSetObject(obj, 0)" before each
     return from this, and before any repeated call to imodvSetObject.
     The IMOD_OBJFLAG_LINE has an inverted sense (off
     if line or fill outline is selected, so switched to using iobjLine
     instead of testing this flag to avoid confusion */

  if (hasSpheres){
    /* scattered points: if they are filled, draw as fill; then draw the
       lines on top if "Fill outline" is selected
       Also draw points non-scattered point objects as filled */
    if (iobjFill(obj->flags) || !iobjScat(obj->flags)){

      // If fill color for point flag is set, temporarily set fill color flag
      flagSave = obj->flags;
      if (obj->flags & IMOD_OBJFLAG_FCOLOR_PNT)
        obj->flags |= IMOD_OBJFLAG_FCOLOR;
      imodvSetObject(imod, obj, DRAW_FILL, drawTrans);
      imodvDraw_spheres(obj, zscale, DRAW_FILL, drawTrans );
      flagSave |= (obj->flags & IMOD_OBJFLAG_TEMPUSE);
      obj->flags = flagSave;
      if (iobjLine(obj->flags) && iobjFill(obj->flags)){
        imodvSetObject(imod, obj, 0, 0);
        imodvSetObject(imod, obj, DRAW_LINES, drawTrans);
        imodvDraw_spheres(obj, zscale, DRAW_LINES, drawTrans);
      }
    }else{
      /* or, just draw the lines if that is selected; otherwise draw
         points */
      if (iobjLine(obj->flags)){
        imodvSetObject(imod, obj, DRAW_LINES, drawTrans);
        imodvDraw_spheres(obj, zscale, DRAW_LINES, drawTrans );
      }else{
        imodvSetObject(imod, obj, DRAW_POINTS, drawTrans);
        imodvDraw_spheres(obj, zscale, DRAW_POINTS, drawTrans );
      }
    }
    imodvSetObject(imod, obj, 0, 0);
    if (iobjScat(obj->flags))
      return;
  }

  if (Imodv->doPick){
    imodvPick_Contours(obj, zscale, drawTrans);
    imodvSetObject(imod, obj, 0, 0);
    return;
  }

  // 9/7/05: removed redundant code     

  /*******************************************/
  /* Draw Mesh data instead of Contour data. */
  if (iobjMesh(obj->flags)){
          
    if (!obj->meshsize){
      return;
    }
    if (!obj->mesh)
      return;

    imodMeshNearestRes(obj->mesh, obj->meshsize, Imodv->lowres, &resol);

    /* Fill or fill outline: draw the filled mesh or scalar mesh */
    if (iobjFill(obj->flags)){
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();
      glScalef(1.0f, 1.0f, 1.0f/zscale);
      imodvSetObject(imod, obj, DRAW_FILL, drawTrans);
      for (co = 0; co < obj->meshsize; co++){
        mesh = &(obj->mesh[co]);
        if (checkMeshDraw(mesh, checkTime, resol)) {
          if (obj->flags & IMOD_OBJFLAG_SCALAR)
            imodvDrawScalarMesh (mesh, zscale, obj, drawTrans);
          else
            imodvDraw_filled_mesh (mesh, zscale, obj, drawTrans);
        }
      }
      glPopMatrix();
               
      /* Fill outline: draw the mesh lines as well */
      if (iobjLine(obj->flags)){
        imodvSetObject(imod, obj, 0, 0);
        imodvSetObject(imod, obj, DRAW_LINES, drawTrans);
        for(co = 0; co < obj->meshsize; co++) {
          mesh = &(obj->mesh[co]);
          if (checkMeshDraw(mesh, checkTime, resol))
            imodvDraw_mesh(mesh, DRAW_LINES, obj, drawTrans);
        }
        /*  imodvSetObject(imod, obj, 0); */
      }
    }else{
      /* Mesh Lines: draw lines in scalar or regular mode */
      if (iobjLine(obj->flags)){
        imodvSetObject(imod, obj, DRAW_LINES, drawTrans);
        for(co = 0; co < obj->meshsize; co++){
          mesh = &(obj->mesh[co]);
          if (checkMeshDraw(mesh, checkTime, resol)) {
            if (obj->flags & IMOD_OBJFLAG_SCALAR){
              imodvDrawScalarMesh(mesh, zscale, obj, drawTrans);
            }else{
              imodvDraw_mesh(mesh, DRAW_LINES, obj, drawTrans);
            }
          }
        }
      }else{
        /* Mesh Points: draw points in scalar or regular mode */
        imodvSetObject(imod, obj, DRAW_POINTS, drawTrans);
        for(co = 0; co < obj->meshsize; co++) {
          mesh = &(obj->mesh[co]);
          if (checkMeshDraw(mesh, checkTime, resol)) {
            if (obj->flags & IMOD_OBJFLAG_SCALAR){
              imodvDrawScalarMesh(mesh, zscale, obj, drawTrans);
            }else{
              /*  if (iobjLine(obj->flags))
                  imodvDraw_mesh(&(obj->mesh[co]),
                  DRAW_LINES, obj);
                  else */
              imodvDraw_mesh(mesh, DRAW_POINTS, obj, drawTrans);
            }
          }
        }
      }
    }
    imodvSetObject(imod, obj, 0, 0);
    return;
  }

  /* Closed contours with Fill: draw the fill, then draw the outside lines
     if Fill Outline is selected */
  if ((iobjClose(obj->flags)) && (iobjFill(obj->flags))){
    imodvSetObject(imod, obj, DRAW_FILL, drawTrans);
    /* We have to either turn off the light or set the normal to
       something, to have display be independent of the state of the
       last object. Lighting makes it look too bright.  Also turn off
       back-face culling in case of trans */
    light_off(); 
    glDisable(GL_CULL_FACE);
    /* glNormal3f(0., 0., 1.); */
    imodvDraw_filled_contours(obj, drawTrans);
    if (iobjLine(obj->flags)){
      imodvSetObject(imod, obj, 0, 0);
      imodvSetObject(imod, obj, DRAW_LINES, drawTrans);
      imodvDraw_contours(obj, GL_LINE_LOOP, drawTrans);
    }
  }else{
    /* Contours as lines or points; draw as open or closed lines */
    if (iobjLine(obj->flags)){
      imodvSetObject(imod, obj, DRAW_LINES, drawTrans);
      if (iobjClose(obj->flags)){
        imodvDraw_contours(obj, GL_LINE_LOOP, drawTrans );
      }else{
        imodvDraw_contours(obj, GL_LINE_STRIP, drawTrans );
      }
    }else{
      imodvSetObject(imod, obj, DRAW_POINTS, drawTrans);
      imodvDraw_contours(obj, GL_POINTS, drawTrans );
    }
  }

  imodvSetObject(imod, obj, 0, 0);
  return;
}




/****************************************************************************/
/* DRAW CONTOURS 
 */
//#define PICKPOINTS
static void imodvPick_Contours(Iobj *obj, double zscale, int drawTrans)
{
  int co, pt, npt;
  Icont *cont;
  Imesh *mesh;
  int i, li;
  Ipoint *vert;
  int    *list;
  int pmode = GL_POINTS;
  int doLines = 0;
  bool hasPolyNorm2;
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_MESH_COLOR | HANDLE_3DWIDTH;
  int checkTime = (int)iobjTime(obj->flags);
  if (!sCTime)
    checkTime = 0;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  // Skip drawing if trans state does not match draw state
  if ((obj->trans ? 1 : 0) != drawTrans) {
    obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    return;
  }


  // Make sure there is a polynorm2 mesh before doing mesh drawing, so it
  // can fall back to contour drawing for old meshes
  hasPolyNorm2 = false;
  for (co = 0; co < obj->meshsize && !hasPolyNorm2; co++) {
    mesh = &(obj->mesh[co]);
    for(i = 0; i < mesh->lsize; i++) {
      if (mesh->list[i] == IMOD_MESH_BGNPOLYNORM2) {
        hasPolyNorm2 = true;
        break;
      }
    }
  }

  // If there is mesh drawing, draw the vertex points or the triangles
  glPushName(NO_NAME);
  if (iobjMesh(obj->flags) && hasPolyNorm2) {
    for (co = 0; co < obj->meshsize; co++) {
      mesh = &(obj->mesh[co]);
      list = mesh->list;
      vert = mesh->vert;
      if (!mesh->lsize || !mesh->vsize)
        continue;

      // Load the name as a number past the last contour
      glLoadName(co + 1 + obj->contsize);
      glPushName(NO_NAME);
      
      // For ordinary meshes, better draw all the lines
      if (obj->contsize) {

        // LINE/FILL give same time and seem to perform same on big triangles
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); 
        for(i = 0; i < mesh->lsize; i++){
          switch(list[i]){
          case IMOD_MESH_BGNPOLYNORM2:
            i++;
            while (list[i] != IMOD_MESH_ENDPOLY) {
              li = list[i++];

              // The load name must occur outside begin-end sequence
              glLoadName(li);
              glBegin(GL_TRIANGLES);
              glVertex3f(vert[li].x, vert[li].y, vert[li].z);
              li = list[i++];
              glVertex3f(vert[li].x, vert[li].y, vert[li].z);
              li = list[i++];
              glVertex3f(vert[li].x, vert[li].y, vert[li].z);
              glEnd();
            }
            break;
          default:
            break;
          }
        }
      } else {

        // This is 5 times faster!  So do it for isosurfaces (no contours)
        for (li = 0; li < mesh->vsize; li += 2) {
          glLoadName(li);
          glBegin(GL_POINTS);
          glVertex3fv((GLfloat *)&(vert[li]));
          glEnd();
          
        }
      }
      glPopName();
    }
    glPopName();
    return;
  }


  if (iobjLine(obj->flags)){
    pmode = GL_LINES;
    doLines = 1;
  }

  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, 0);
    if (contProps.gap)
      continue;

    glLoadName(co);
          
    glPushName(NO_NAME);
    for (pt = 0; pt < cont->psize; pt++) {
      ptProps.gap = 0;
      if (nextChange == pt)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, 
                                         &changeFlags, handleFlags, 0);
      if (doLines){
        if (ptProps.gap)
          continue;
        npt = pt + 1;
        if (npt == cont->psize){
          if ((!(iobjClose(obj->flags))) || 
              (cont->flags & ICONT_OPEN))
            break;
          npt = 0;
        }
      } else if (ptProps.gap && ptProps.valskip)
        continue;
      glLoadName(pt);
      glBegin(pmode);
      glVertex3fv((GLfloat *)&(cont->pts[pt]));
      if (doLines) {
        glVertex3fv((GLfloat *)&(cont->pts[npt]));
      }
      glEnd();
    }
    glPopName();
  }

  glPopName();
}

static void imodvDraw_contours(Iobj *obj, int mode, int drawTrans)
{
  int co, pt, trans, thickAdd = 0;
  Icont *cont;
  int checkTime = (int)iobjTime(obj->flags);
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags, changeFlags2, cumInd, remInd, nextRemnant;
  int defTrans = obj->trans ? 1 : 0;
  bool vbOK;
  VertBufData *vbd;
  float red, green, blue;
  int handleCoFlgs = HANDLE_MESH_COLOR | HANDLE_TRANS;
  int handlePtFlgs = HANDLE_TRANS;
  if (!sCTime)
    checkTime = 0;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1)) {
    handleCoFlgs |= HANDLE_VALUE1;
    handlePtFlgs |= HANDLE_VALUE1;
  }

  vbCleanupMeshVBD(obj);

  vbOK = !(handleCoFlgs & HANDLE_VALUE1) && Imodv->vertBufOK > 0 && Imodv->primRestartOK
    && !Imodv->current_subset;
  if (!vbOK)
    vbCleanupContVBD(obj);

  // first time in, try to set up vertex buffer drawing if it is OK and this is not
  // the line draw of fill outline
  if (!drawTrans && vbOK) {
    pt = Imodv->vbManager->analyzeConts
      (obj, sObjBeingDrawn, sThickCont >= 0 ? 1 : 0, 
       (!Imodv->standalone && Imodv->vi->drawStipple) ? 1 : 0, checkTime ? sCTime : 0);
    if (pt != -1)
      imodTrace('b', "vbAnalyzeConts returned %d", pt);
  }

  vbd = (vbOK && obj->vertBufCont && obj->vertBufCont->vbObj) ? obj->vertBufCont : NULL;

  // First time in, if object has transparency, then check whether any 
  // contour or point stores set transparency to 0 and if not, skip
  if (!drawTrans && obj->trans) {
    if (!istoreTransStateMatches(obj->store, 0)) {
      for (co = 0; co < obj->contsize; co++)
        if (istoreTransStateMatches(obj->cont[co].store, 0))
          break;
      if (co >= obj->contsize) {
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
        return;
      }
    }
  }

  if (imodDebug('v'))
    imodPrintStderr("draw contours %s\n", drawTrans ? "trans" : "solid");

  // Do vertex buffer drawing
  if (vbd) {
    
    // set up to use the VBO
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, 3 * sizeof(GLfloat), BUFFER_OFFSET(0));
    glEnable(GL_PRIMITIVE_RESTART);
    b3dPrimitiveRestartIndex(RESTART_INDEX);

    // Draw default if it matches trans state
    if (vbd->numIndDefault) {
      if (drawTrans == defTrans) {
        imodTrace('v', "VB cont default, %s  %d %u %u", drawTrans ? "trans" : "solid", 
                  vbd->numIndDefault, vbd->vbObj, vbd->ebObj);
        glDrawElements(GL_LINE_STRIP, vbd->numIndDefault, GL_UNSIGNED_INT,
                       BUFFER_OFFSET(0));
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    }
    cumInd = vbd->numIndDefault;

    // Draw special sets, keeping cumulative index for offset
    for (pt = 0; pt < vbd->numSpecialSets; pt++) {
      Imodv->vbManager->unpackRGBT(vbd->rgbtSpecial[pt], red, green, blue, trans);
      if ((trans ? 1 : 0) == drawTrans) {
        imodTrace('v', "VB cont special %d, %s, %.2f %.2f %.2f %d", pt, 
                  drawTrans ? "trans" : "solid", red, green, blue, trans);
        ifgHandleColorTrans(obj, red, green, blue, trans);
        glDrawElements(GL_LINE_STRIP, vbd->numIndSpecial[pt], GL_UNSIGNED_INT,
                       BUFFER_OFFSET(cumInd * sizeof(GLuint)));
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      cumInd += vbd->numIndSpecial[pt];
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    b3dBindBuffer(GL_ARRAY_BUFFER, 0);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisable(GL_PRIMITIVE_RESTART);
    if (!vbd->numRemnant)
      return;
    nextRemnant = vbd->remnantIndList[0];
    remInd = 1;
  }

  for (co = 0; co < obj->contsize; co++) {

    // If doing remnant draw, skip from one remnant to next
    if (vbd) {
      if (nextRemnant < 0)
        return;
      if (co < nextRemnant)
        co = nextRemnant;
      if (remInd < vbd->numRemnant) {
        nextRemnant =  vbd->remnantIndList[remInd++];
        B3DCLAMP(nextRemnant, 0 , obj->contsize - 1);
      } else
        nextRemnant = -1;
    }
    cont = &(obj->cont[co]);
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleCoFlgs, 0);
    if (contProps.gap)
      continue;

    // Set thicker line if this is the current contour
    thickAdd = imodvCheckThickerContour(co) ? 2 : 0;
    
    pt = 0;
    if ((ptProps.trans ? 1 : 0) != drawTrans) {
      nextChange = ifgContTransMatch(obj, cont, &pt, drawTrans, 
                                     &contProps, &ptProps, &stateFlags,
                                     &changeFlags, handleCoFlgs);
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      if (ptProps.gap)
        pt++;
      if (pt >= cont->psize)
        ptProps.gap = 1;
      if (imodDebug('v'))
        imodPrintStderr("Skipped from start to %d\n", pt);
    }

    if (mode == GL_POINTS) {

      // Set up to do points
      glPointSize(ptProps.linewidth + thickAdd);
      glBegin(GL_POINTS);
      for (; pt < cont->psize; pt++) {
        ptProps.gap = 0;

        // For points, implement change before point is drawn
        if (nextChange == pt) {
          nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                           &ptProps, &stateFlags, 
                                           &changeFlags, handlePtFlgs, 0);

          // If trans state changes, seek point that restores it
          if ((ptProps.trans ? 1 : 0) != drawTrans) {
            glEnd();
            nextChange = ifgContTransMatch(obj, cont, &pt, drawTrans, 
                                           &contProps, &ptProps, &stateFlags,
                                           &changeFlags, handleCoFlgs);
            glBegin(GL_POINTS);
            obj->flags |= IMOD_OBJFLAG_TEMPUSE;
            if (pt >= cont->psize)
              break;
          }

          if (changeFlags & CHANGED_3DWIDTH) {
            glEnd();
            glPointSize((GLfloat)ptProps.linewidth + thickAdd);
            glBegin(GL_POINTS);
          }
          if (changeFlags & CHANGED_COLOR) {
            glEnd();
            ifgHandleColorTrans(obj, ptProps.red, ptProps.green,
                                ptProps.blue, ptProps.trans);
            glBegin(GL_POINTS);
          }
        }
        if (!ptProps.gap || !ptProps.valskip)
          glVertex3fv((GLfloat *)&(cont->pts[pt]));
      }
      glEnd();

    } else {

      // Set up to do lines
      if (!Imodv->standalone)
        utilEnableStipple(Imodv->vi, cont);
      glLineWidth(ptProps.linewidth + thickAdd);
      glBegin(GL_LINE_STRIP);
      for (; pt < cont->psize; pt++) {

        // Get change at point then add point so color changes during line
        ptProps.gap = 0;
        if (nextChange == pt) {
          nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                           &ptProps, &stateFlags, 
                                           &changeFlags, handlePtFlgs, 0);
          glVertex3fv((GLfloat *)&(cont->pts[pt]));

          // Skip ahead if trans state changed
          if ((ptProps.trans ? 1 : 0) != drawTrans) {
            if (imodDebug('v'))
              imodPrintStderr("Drew to %d\n", pt);
            glEnd();
            nextChange = ifgContTransMatch(obj, cont, &pt, drawTrans, 
                                           &contProps, &ptProps, &stateFlags,
                                           &changeFlags2, handlePtFlgs);
            obj->flags |= IMOD_OBJFLAG_TEMPUSE;
            if (imodDebug('v'))
              imodPrintStderr("Skipped to %d\n", pt);
            
            // Set gap if skipping to end so connector is not drawn
            if (pt >= cont->psize) {
              ptProps.gap = 1;
              break;
            }
            if ((changeFlags | changeFlags2) & CHANGED_3DWIDTH)
              glLineWidth((GLfloat)ptProps.linewidth + thickAdd);
            if ((changeFlags | changeFlags2) & CHANGED_COLOR)
              ifgHandleColorTrans(obj, ptProps.red, ptProps.green,
                                  ptProps.blue, ptProps.trans);
            glBegin(GL_LINE_STRIP);

          } else {
            if (changeFlags & CHANGED_3DWIDTH) {

              // Width change requires ending the strip and restarting it
              glEnd();
              glLineWidth((GLfloat)ptProps.linewidth + thickAdd);
              glBegin(GL_LINE_STRIP);
            }
           
            // So do color changes on nvidia/Linux
            if (changeFlags & CHANGED_COLOR) {
              glEnd();
              ifgHandleColorTrans(obj, ptProps.red, ptProps.green,
                                  ptProps.blue, ptProps.trans);
              glBegin(GL_LINE_STRIP);
            }
          }
        }
        glVertex3fv((GLfloat *)&(cont->pts[pt]));

        if (ptProps.gap) {
          glEnd();
          glBegin(GL_LINE_STRIP);
        }
      }
      if ((mode == GL_LINE_LOOP) && !(cont->flags & ICONT_OPEN) && 
          !ptProps.gap)
        glVertex3fv((GLfloat *)cont->pts);
      glEnd();
      if (!Imodv->standalone)
        utilDisableStipple(Imodv->vi, cont);
    }
  }
  return;
}  

// DMN 8/12/13: Removed TESS_HACK code before moving tesselator stuff to utilities.

#ifndef GLU_CALLBACK
#define GLU_CALLBACK GLvoid (*)()
#endif

/*
 * Draw filled contours with polygon tesselation
 */
static void imodvDraw_filled_contours(Iobj *obj, int drawTrans)
{
  Icont   *cont;
  int      co;
  DrawProps contProps, ptProps;
  int stateFlags;
  int handleFlags = ((obj->flags & IMOD_OBJFLAG_FCOLOR) 
                     ? HANDLE_MESH_FCOLOR : HANDLE_MESH_COLOR) | HANDLE_TRANS;
  int checkTime = (int)iobjTime(obj->flags);
  if (!sCTime)
    checkTime = 0;

  if (!obj->contsize)
    return;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  // Skip drawing first time in if object is trans and no contours become
  // solid
  if (!drawTrans && obj->trans && !istoreTransStateMatches(obj->store, 0)) {
    obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    return;
  }

  if (imodDebug('v'))
    imodPrintStderr("filled contours %s\n", drawTrans ? "trans" : "solid");

  setupFilledContTesselator();

  glPushName(NO_NAME);
  for(co = 0; co < obj->contsize ; co++){
    glLoadName(co);
    cont = &(obj->cont[co]);

    // 8/29/06: it was only checking time before (not even size)
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;

    ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                        handleFlags, 0);
    if (contProps.gap)
      continue;
    if ((contProps.trans ? 1 : 0) != drawTrans) {
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      continue;
    }
    drawFilledPolygon(cont);
  }
  glPopName();
  return;
}

#define MAX_QUALITY 5
#define MAX_LOOKUP 120
#define MAX_MEASURES 9
static int sSphereRes[MAX_LOOKUP][MAX_QUALITY];
static int sFirstSphere = 1;
static float sScaleSphere;
static int sQualitySphere;

int sphereResForSize(float drawsize)
{
  int pixsize = (int)(drawsize * sScaleSphere);
  B3DCLAMP(pixsize, 0, MAX_LOOKUP - 1);
  return sSphereRes[pixsize][sQualitySphere];
}

/***************************************************************************/
/* Draw point spheres. */
static void imodvDraw_spheres(Iobj *obj, double zscale, int style, 
                              int drawTrans)
{
  int co, pt, remInd, nextRemnant;
  Icont *cont;
  float z = zscale;
  int checkTime = (int)iobjTime(obj->flags);
  float drawsize;
  int i, j, k, mink, stepRes, xybin, contDraw, cumFanInd, cumQuadInd, trans;
  int defTrans = obj->trans ? 1 : 0;
  int contsize = obj->contsize;
  float diff, mindiff, red, green, blue;
  bool vbOK, outlineDraw, needThick;
  VertBufData *vbd;
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_3DWIDTH | HANDLE_TRANS |
    (style == DRAW_FILL && (obj->flags & IMOD_OBJFLAG_FCOLOR)
     ? HANDLE_MESH_FCOLOR : HANDLE_MESH_COLOR);
  float measuredSize[MAX_MEASURES] = 
    { 1.5, 2.5, 3.75, 7.5, 30., 40., 60., 80., 120.};
  int measuredRes[MAX_MEASURES][MAX_QUALITY] = {
    {0, 1, 2, 2, 2},
    {0, 1, 2, 3, 4},
    {0, 1, 2, 4, 6},
    {0, 2, 4, 6, 8},
    {0, 2, 5, 8, 10},
    {0, 2, 6, 8, 10},
    {0, 2, 8, 10, 12},
    {0, 2, 10, 12, 14},
    {0, 2, 12, 14, 16}
  };
  GLuint listIndex;
#ifdef GLU_QUADRIC_HACK
  GLUquadricObj *qobj = gluNewQuadric();
#else
  static GLUquadricObj *qobj = NULL;
  if (!qobj)
    qobj = gluNewQuadric();
#endif

  /* first time, build lookup tables for sphere resolution versus size and
     quality */
  if (sFirstSphere) {
    for (i = 0; i < MAX_QUALITY; i++)
      for (j = 0; j < MAX_LOOKUP; j++) {
        mindiff = 10000.;
        for (k = 0; k < MAX_MEASURES; k++) {
          diff = j - measuredSize[k];
          if (diff < 0.)
            diff = -diff;
          if (diff < mindiff) {
            mindiff = diff;
            mink = k;
          }
        }
        sSphereRes[j][i] = measuredRes[mink][i] + 2;
      }
    sFirstSphere = 0;
  }

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  xybin = 1;
  if (!Imodv->standalone)
    xybin = Imodv->vi->xybin;

  /* Take maximum of quality from world flag setting and from object */
  sQualitySphere = ((Imodv->mod[sModBeingDrawn]->view->world & WORLD_QUALITY_BITS) >> 
    WORLD_QUALITY_SHIFT) + 1;
  if (sQualitySphere <= obj->quality)
    sQualitySphere = obj->quality + 1;

  if (Imodv->lowres)
    sQualitySphere = 0;
  if (sQualitySphere >= MAX_QUALITY)
    sQualitySphere = MAX_QUALITY - 1;

  sScaleSphere = 0.5 * (Imodv->winx > Imodv->winy ? Imodv->winy : Imodv->winx) / 
    Imodv->mod[sModBeingDrawn]->view->rad;
  //printf("quality  %d  scale %f\n", sQualitySphere, sScaleSphere);

  if (!sCTime)
    checkTime = 0;

  // Doing an outline draw if drawing lines and the object has line/fill flags set
  outlineDraw = iobjFill(obj->flags) && iobjLine(obj->flags) && style == DRAW_LINES;

  // Some contours will be thickened if thickening is set up and we are not just
  // drawing a mesh without later outline draw
  needThick = sThickCont >= 0 && !(style == DRAW_FILL && !iobjLine(obj->flags));

  // TODO: handle subset changes
  vbOK = !(handleFlags & HANDLE_VALUE1) && Imodv->vertBufOK > 0 && Imodv->primRestartOK
    && !Imodv->current_subset;

  if (!vbOK)
    vbCleanupSphereVBD(obj);

  // first time in, try to set up vertex buffer drawing if it is OK and this is not
  // the line draw of fill outline
  if (!drawTrans && vbOK && !outlineDraw) {
    i = style == DRAW_FILL ? 1 : 0;
    if (style == DRAW_POINTS)
      i = -1;
    i = Imodv->vbManager->analyzeSpheres
      (obj, sObjBeingDrawn, z, xybin, sScaleSphere, sQualitySphere, i, 
       style == DRAW_FILL && (obj->flags & IMOD_OBJFLAG_FCOLOR) ? 1 : 0,
       needThick ? 1 : 0, checkTime ? sCTime : 0);
    if (i != -1)
      imodTrace('b', "vbAnalyzeSpheres returned %d", i);
  }

  // VBO cannot be used for pick draw or for any outline draw
  vbd = (vbOK && !Imodv->doPick && !outlineDraw && obj->vertBufSphere && 
         obj->vertBufSphere->vbObj) ? obj->vertBufSphere : NULL;

  // When drawing solids, if object has transparency, then check whether any 
  // contour or point stores set transparency to 0 and if not, skip
  if (!drawTrans && obj->trans) {
    if (!istoreTransStateMatches(obj->store, 0)) {
      for (co = 0; co < obj->contsize; co++)
        if (istoreTransStateMatches(obj->cont[co].store, 0))
          break;
      if (co >= obj->contsize) {
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
        return;
      }
    }
  }
  if (imodDebug('v'))
    imodPrintStderr("draw spheres %s\n", drawTrans ? "trans" : "solid");

  if (!qobj){
    qobj = gluNewQuadric();
  }

  switch(style){
  case DRAW_POINTS:
    gluQuadricDrawStyle(qobj, GLU_POINT);
    break;
  case DRAW_LINES:
    gluQuadricDrawStyle(qobj, GLU_LINE);
    break;
  default:
    gluQuadricDrawStyle(qobj, GLU_FILL);
    break;
  }

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();

  glScalef(1.0f, 1.0f, 1.0f/z);     
  glPushName(NO_NAME);

  // Do vertex buffer drawing
  if (vbd) {
    
    // set up to use the VBO
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
    if (style == DRAW_FILL) {
      glEnableClientState(GL_NORMAL_ARRAY);
      glNormalPointer(GL_FLOAT, 6 * sizeof(GLfloat), BUFFER_OFFSET(0));
    }
    glEnableClientState(GL_VERTEX_ARRAY);
    if (style == DRAW_FILL || outlineDraw)
      glVertexPointer(3, GL_FLOAT, 6 * sizeof(GLfloat), 
                      BUFFER_OFFSET(3 * sizeof(GLfloat)));
    else
      glVertexPointer(3, GL_FLOAT, 3 * sizeof(GLfloat), BUFFER_OFFSET(0));
    glEnable(GL_PRIMITIVE_RESTART);
    b3dPrimitiveRestartIndex(RESTART_INDEX);

    // Draw default if it matches trans state
    if (vbd->numFanIndDefault || vbd->numIndDefault) {
      if (drawTrans == defTrans) {
        imodTrace('v', "VB sphere default, %s  %d %u %u", drawTrans ? "trans" : "solid", 
                  vbd->numIndDefault, vbd->vbObj, vbd->ebObj);
        if (style == DRAW_FILL) {
          if (vbd->numIndDefault)
            glDrawElements(style == DRAW_POINTS ? GL_POINTS : GL_QUAD_STRIP, 
                           vbd->numIndDefault, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
          glDrawElements(style == DRAW_POINTS ? GL_POINTS : GL_TRIANGLE_FAN, 
                         vbd->numFanIndDefault, GL_UNSIGNED_INT, 
                         BUFFER_OFFSET(vbd->fanIndStart * sizeof(GLuint)));
        } else {
          glDrawElements(style == DRAW_POINTS ? GL_POINTS : GL_LINE_STRIP, 
                         vbd->numIndDefault, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
        }
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    }
    cumQuadInd = vbd->numIndDefault;
    cumFanInd = vbd->fanIndStart + vbd->numFanIndDefault;

    // Draw special sets, keeping cumulative indices for offset
    for (j = 0; j < vbd->numSpecialSets; j++) {
      Imodv->vbManager->unpackRGBT(vbd->rgbtSpecial[j], red, green, blue, trans);
      if ((trans ? 1 : 0) == drawTrans) {
        imodTrace('v', "VB sphere special %d, %s, %.2f %.2f %.2f %d  numquad %d ind %d"
                  " numfan %d ind %d", j, drawTrans ? "trans" : "solid", red, green, blue,
                  trans, vbd->numIndSpecial[j], cumQuadInd, vbd->numFanIndSpecial[j],
                  cumFanInd);
        ifgHandleColorTrans(obj, red, green, blue, trans);
        if (style == DRAW_FILL) {
          if (vbd->numIndSpecial[j])
            glDrawElements(style == DRAW_POINTS ? GL_POINTS : GL_QUAD_STRIP,
                           vbd->numIndSpecial[j], GL_UNSIGNED_INT,
                           BUFFER_OFFSET(cumQuadInd * sizeof(GLuint)));
          glDrawElements(style == DRAW_POINTS ? GL_POINTS : GL_TRIANGLE_FAN,
                         vbd->numFanIndSpecial[j], GL_UNSIGNED_INT,
                         BUFFER_OFFSET(cumFanInd * sizeof(GLuint)));
        } else {
          glDrawElements(style == DRAW_POINTS ? GL_POINTS : GL_LINE_STRIP,
                         vbd->numIndSpecial[j], GL_UNSIGNED_INT,
                         BUFFER_OFFSET(cumQuadInd * sizeof(GLuint)));
        }
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      cumQuadInd += vbd->numIndSpecial[j];
      cumFanInd += vbd->numFanIndSpecial[j];
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
    b3dBindBuffer(GL_ARRAY_BUFFER, 0);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisable(GL_PRIMITIVE_RESTART);

    // Short-circuit the loop if no remnants; otherwise set up remnant drawing
    if (!vbd->numRemnant)
      contsize = 0;
    else 
      nextRemnant = vbd->remnantIndList[0];
    remInd = 1;
    //imodTrace('b', "obj %d contsize %d  numrem %d nextrem %d remind %d", sObjBeingDrawn,
    //        contsize, vbd->numRemnant, nextRemnant, remInd);
  }

  /* DNM: Get a display list to draw the default size.  Helps a lot on PC,
     only a bit on the SGI */
  drawsize = obj->pdrawsize / xybin;
  stepRes = sphereResForSize(drawsize);
  //imodPrintStderr("drawsize  %f stepRes %d\n", drawsize, stepRes);
  listIndex = glGenLists(1);
  glNewList(listIndex, GL_COMPILE);
  gluSphere(qobj, drawsize , stepRes * 2, stepRes);
  glEndList();
  /*imodTrace('b', "obj %d contsize %d  nextrem %d remind %d", sObjBeingDrawn,
    contsize, nextRemnant, remInd);*/

  for (co = 0; co < contsize; co++) {

    // If doing remnant draw, skip from one remnant to next
    if (vbd) {
      if (nextRemnant < 0)
        break;
      if (co < nextRemnant)
        co = nextRemnant;
      if (remInd < vbd->numRemnant) {
        nextRemnant =  vbd->remnantIndList[remInd++];
        B3DCLAMP(nextRemnant, 0 , obj->contsize - 1);
      } else
        nextRemnant = -1;
    }

    cont = &(obj->cont[co]);
    glLoadName(co);
    contDraw = imodvCheckContourDraw(cont, co, checkTime);
    if (!contDraw)
      continue;

    // Skip contour if not scattered and no sizes
    if (!iobjScat(obj->flags) && !obj->pdrawsize && !cont->sizes)
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, 0);
    if (contProps.gap)
      continue;
    pt = 0;
    if ((ptProps.trans ? 1 : 0) != drawTrans) {
      nextChange = ifgContTransMatch(obj, cont, &pt, drawTrans, 
                                     &contProps, &ptProps, &stateFlags,
                                     &changeFlags, handleFlags);
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      if (imodDebug('v'))
        imodPrintStderr("Cont %d, Skipped from start to %d\n", co, pt);
   }

    // Set thicker line if this is the current contour, restore at end
    if (imodvCheckThickerContour(co))
      glLineWidth(obj->linewidth + 2);

    glPushName(NO_NAME);
    for (; pt < cont->psize; pt++) {
      if (nextChange == pt) {
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, 
                                         &changeFlags, handleFlags, 0);

        // If trans state changes, seek point that restores it
        if ((ptProps.trans ? 1 : 0) != drawTrans) {
          nextChange = ifgContTransMatch(obj, cont, &pt, drawTrans, 
                                         &contProps, &ptProps, &stateFlags,
                                         &changeFlags, handleFlags);
          obj->flags |= IMOD_OBJFLAG_TEMPUSE;
          if (pt >= cont->psize)
              break;
        }
      }        

      /* get the real point size, convert to number of pixels and
         look up step size based on current quality */
      drawsize = imodPointGetSize(obj, cont, pt);

      // Only draw zero-size points with scattered point objects
      // Skip points only if values are outside range, not if a line gap
      if ((!iobjScat(obj->flags) && !drawsize) || (ptProps.gap && ptProps.valskip) || 
          (contDraw > 1 && pt != Imodv->imod->cindex.point)) 
        continue;

      glLoadName(pt);
      glPushMatrix();
      glTranslatef(cont->pts[pt].x, cont->pts[pt].y, cont->pts[pt].z*z);
               
      if (drawsize == obj->pdrawsize)
        /* Use the display list if default size */
        glCallList(listIndex);
      else {
        drawsize /= xybin;
        stepRes = sphereResForSize(drawsize);
        gluSphere(qobj, drawsize , stepRes * 2, stepRes);
      }
      glPopMatrix();
    }
    glPopName();
    if (imodvCheckThickerContour(co))
      glLineWidth(obj->linewidth);
  }

  glDeleteLists(listIndex, 1);
  glPopMatrix();
  glPopName();
#ifdef GLU_QUADRIC_HACK
  gluDeleteQuadric(qobj);
#endif
  return;
}




/*****************************************************************************/
/*  Draw Mesh Data                                                           */
/*****************************************************************************/

static void imodvDraw_mesh(Imesh *mesh, int style, Iobj *obj, int drawTrans)
{
  int i, j;
  GLenum polyStyle, normStyle;
  DrawProps defProps, curProps;
  Ipoint *vert;
  int    *mlist;
  int nextChange, stateFlags, changeFlags, nextItemIndex;
  float red, green, blue;
  int trans, cumInd, remnantMatchesTrans;
  int handleFlags = HANDLE_MESH_COLOR | HANDLE_3DWIDTH | HANDLE_TRANS;
  float *firstPt;
  float firstRed, firstGreen, firstBlue;
  bool vbOK;
  VertBufData *vbd;
  Imesh meshCopy;
  int firstTrans, defTrans;
  int skipEnds = !((Imodv->current_subset == SUBSET_SURF_ONLY || Imodv->current_subset ==
                    SUBSET_SURF_OTHER) && sCursurf >= 0) || mesh->surf > 0 ? 1 : 0;

  if (!mesh || !mesh->lsize)
    return;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  switch(style){
  case DRAW_POINTS:
    polyStyle = GL_POINTS;
    normStyle   = GL_POINTS;
    break;
  case DRAW_LINES:
    polyStyle = GL_LINE_STRIP;
    normStyle   = GL_LINE_LOOP;
    break;
  }

  // Can use vertex buffer if no surface subset, not value, and it is not being used
  // for fill
  vbCleanupContVBD(obj);
  vbOK = skipEnds && !(handleFlags & HANDLE_VALUE1) && Imodv->vertBufOK > 0 && 
    !iobjFill(obj->flags);

  // Clean up unless it is being used for fill
  if (!vbOK && !iobjFill(obj->flags))
    vbCleanupVBD(mesh);

  ifgHandleSurfChange(obj, mesh->surf, &defProps, &curProps, &stateFlags, handleFlags);
  defTrans = defProps.trans ? 1 : 0;
  vert = mesh->vert;
  
  // first time in, try to set up vertex buffer drawing if it is OK
  if (!drawTrans && vbOK) {
    i = Imodv->vbManager->analyzeMesh(mesh, 1., 0, 0, &defProps);
    if (i != -1)
      imodTrace('b', "vbAnalyzeMesh returned %d", i);
  }
  vbd = vbOK ? mesh->vertBuf : NULL;
      
  // Do vertex buffer drawing
  if (vbd && vbd->vbObj) {

    // First time in, If everything is trans, set flag and skip out
    if (!drawTrans && defTrans && 
        Imodv->vbManager->checkAllTrans(obj, vbd, remnantMatchesTrans))
      return;
  }

  if (vbd && vbd->vbObj) {
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, 3 * sizeof(GLfloat), BUFFER_OFFSET(0));


    // These changes are needed to draw lines as triangles
    if (style == DRAW_LINES) {
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDisable(GL_CULL_FACE);
      light_off();
    }

    // Draw default if it matches trans state
    if (vbd->numIndDefault) {
      if (drawTrans == defTrans) {
        imodTrace('v', "VB default, %s  %d %u %u", drawTrans ? "trans" : "solid", 
                  vbd->numIndDefault, vbd->vbObj, vbd->ebObj);
        glDrawElements(style == DRAW_LINES ? GL_TRIANGLES : GL_POINTS, 
                       vbd->numIndDefault, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    }
    cumInd = vbd->numIndDefault;

    // Draw special sets, keeping cumulative index for offset
    for (j = 0; j < vbd->numSpecialSets; j++) {
      Imodv->vbManager->unpackRGBT(vbd->rgbtSpecial[j], red, green, blue, trans);
      if ((trans ? 1 : 0) == drawTrans) {
        imodTrace('v', "VB special %d, %s, %.2f %.2f %.2f %d", j, 
                  drawTrans ? "trans" : "solid", red, green, blue, trans);
        glColor4f(red, green, blue, 1.0f - (trans * 0.01f));
        glDrawElements(style == DRAW_LINES ? GL_TRIANGLES : GL_POINTS,
                       vbd->numIndSpecial[j], GL_UNSIGNED_INT,
                       BUFFER_OFFSET(cumInd * sizeof(GLuint)));
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      cumInd += vbd->numIndSpecial[j];
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    b3dBindBuffer(GL_ARRAY_BUFFER, 0);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

    // Set up for remnant drawing unless it is all trans the first time
    if (!(!drawTrans && defTrans && !remnantMatchesTrans)) {
      imodMeshCopy(mesh, &meshCopy);
      mesh = &meshCopy;
      mesh->list = vbd->remnantIndList;
      
      mesh->lsize = vbd->numRemnant;
      mesh->store = vbd->remnantStore;
      imodTrace('v', "Remnant %s", drawTrans ? "trans" : "solid");
      ifgHandleSurfChange(obj, mesh->surf, &defProps, &curProps, &stateFlags, 
                          handleFlags);
    } else {
      //glFlush();
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      return;
    }

    // Now do or continue with regular line by line drawing
  } else {

    // First time in, if the trans state does not match the draw state, and the 
    // storage list does not have a change to a matching state, return
    if (!drawTrans && defTrans && !istoreTransStateMatches(mesh->store, 0)) {
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      return;
    }
  }

  if (imodDebug('v'))
    imodPrintStderr("draw mesh lines %s\n", drawTrans ? "trans" : "solid");

  mlist = mesh->list;
  stateFlags = 0;
  nextItemIndex = nextChange = istoreFirstChangeIndex(mesh->store);

  for (i  = 0; i < mesh->lsize; i++) {
    switch(mlist[i]){
    case IMOD_MESH_BGNPOLY:
    case IMOD_MESH_BGNBIGPOLY:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      glBegin(polyStyle);
      while(mlist[++i] != IMOD_MESH_ENDPOLY){
        glVertex3fv( (float *)&(vert[mlist[i]]));
      }
      glEnd();
      break;

    case IMOD_MESH_BGNPOLYNORM:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      i++;
      while (mlist[i] != IMOD_MESH_ENDPOLY) {
        glBegin(normStyle);

        glVertex3fv( (float *)&(vert[mlist[++i]]));
        i+=2;
        glVertex3fv( (float *)&(vert[mlist[i]]));
        i+=2;
        glVertex3fv( (float *)&(vert[mlist[i++]]));
                    
        glEnd();
        /*
          glColor4ub(0xff, 0x00, 0x00, 0xff);
 
          if (i > 15)
          glColor4ub(0x00, 0x00, 0xff, 0xff);
        */

      }
      break;

    case IMOD_MESH_BGNPOLYNORM2:
      if (skipNonCurrentSurface(mesh, &i, obj)) {
        nextChange = nextItemIndex = istoreSkipToIndex(mesh->store, i);
        break;
      }
      i++;

      // Before starting loop, check if need to skip to matching trans state
      if ((curProps.trans ? 1 : 0) != drawTrans && 
          (nextChange < i || nextChange > i + 2)) {
        nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, drawTrans, &i,
                                                       skipEnds);
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
        if (imodDebug('v'))
          imodPrintStderr("Skipped from start to %d\n", i);
      }
      while (mlist[i] != IMOD_MESH_ENDPOLY) {
        if (nextChange < i || nextChange > i + 2) {
          glBegin(normStyle);

          // This does not require Z scaling because it is in the transformation matrix
          glVertex3fv( (float *)&(vert[mlist[i++]]));
          glVertex3fv( (float *)&(vert[mlist[i++]]));
          glVertex3fv( (float *)&(vert[mlist[i++]]));
          glEnd();
        } else {
          if (stateFlags || i == nextChange) {
            nextChange = ifgHandleMeshChange
              (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
               &stateFlags, &changeFlags, handleFlags);
            if ((curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              if (stateFlags)
                ifgHandleMeshChange(obj, mesh->store, &defProps, &curProps, 
                                    &nextItemIndex, 0, &stateFlags,
                                    &changeFlags, handleFlags);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, drawTrans,
                                                             &i, skipEnds);
              obj->flags |= IMOD_OBJFLAG_TEMPUSE;
              if (imodDebug('v'))
                imodPrintStderr("Skipping to %d\n", i);
              continue;
            }
          }

          if (style == DRAW_POINTS) {

            // Points might as well be drawn singly
            for (j = 0; j < 3; j++) {
              if (j && (stateFlags || i == nextChange))
                nextChange = ifgHandleMeshChange
                  (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
                   &stateFlags, &changeFlags, handleFlags);
              glBegin(normStyle);
              glVertex3fv( (float *)&(vert[mlist[i++]]));
              glEnd();
            }

          } else {

            glBegin(GL_LINE_STRIP);
            firstPt = (float *)&(vert[mlist[i++]]);
            glVertex3fv(firstPt);
            firstRed = curProps.red;
            firstGreen =curProps.green;
            firstBlue = curProps.blue;
            firstTrans = curProps.trans;
            
            for (j = 0; j < 2; j++) {
              changeFlags = 0;
              if (stateFlags || i == nextChange)
                nextChange = ifgHandleMeshChange
                  (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
                   &stateFlags, &changeFlags, HANDLE_MESH_COLOR);
              glVertex3fv( (float *)&(vert[mlist[i++]]));
              if (changeFlags & CHANGED_3DWIDTH) {
                glEnd();
                glLineWidth((GLfloat)curProps.linewidth);
                glBegin(GL_LINE_STRIP);
                glVertex3fv( (float *)&(vert[mlist[i - 1]]));
              }
            }

            // Reset color to first point
            if (firstRed != curProps.red || firstGreen != curProps.green ||
                firstBlue != curProps.blue || firstTrans != curProps.trans)
              ifgHandleColorTrans(obj, firstRed, firstGreen, firstBlue, 
                                   firstTrans);
            glVertex3fv(firstPt);
            glEnd();

            // If reset color, better set it back to match curprops
            if (firstRed != curProps.red || firstGreen != curProps.green ||
                firstBlue != curProps.blue || firstTrans != curProps.trans)
              ifgHandleColorTrans(obj, curProps.red, curProps.green, 
                                   curProps.blue, curProps.trans);
          }

          // Reset if not in default state and the next positive change will
          // not be in the next triangle
          if (stateFlags && (nextItemIndex < i || nextItemIndex > i + 2 || 
                             mlist[i] == IMOD_MESH_ENDPOLY)) {
            nextChange = ifgHandleMeshChange
              (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
               &stateFlags, &changeFlags, handleFlags);
            if (mlist[i] != IMOD_MESH_ENDPOLY && 
                (curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, drawTrans,
                                                             &i, skipEnds);
              obj->flags |= IMOD_OBJFLAG_TEMPUSE;
              if (imodDebug('v'))
                imodPrintStderr("Skipping to %d\n", i);
            }
          }
        }
      }
      break;

    case IMOD_MESH_BGNTRI:
    case IMOD_MESH_ENDTRI:
    case IMOD_MESH_SWAP:
      break;

    case IMOD_MESH_NORMAL:
      i++;
      break;

    case IMOD_MESH_END:
      return;

    default:
      if ((mlist[i] < mesh->vsize) && (mlist[i] > -1))
        glVertex3fv( (float *)&(vert[mlist[i]])  );
      break;
    }
  }
  return;
}

/* draws mesh with lighting model */
static void imodvDraw_filled_mesh(Imesh *mesh, double zscale, Iobj *obj, 
                                  int drawTrans)
{
  int i, j;
  float z = zscale;
  GLUtesselator *tobj;
  GLdouble v[3];
  DrawProps defProps, curProps;
  Ipoint *vert;
  int    *mlist;
  float red, green, blue;
  int trans, cumInd;
  bool vbOK;
  VertBufData *vbd;
  Imesh meshCopy;
  int nextChange, stateFlags, changeFlags, nextItemIndex, defTrans, remnantMatchesTrans;
  int handleFlags = ((obj->flags & IMOD_OBJFLAG_FCOLOR) ? HANDLE_MESH_FCOLOR :
    HANDLE_MESH_COLOR) | HANDLE_TRANS;
  // Skipends is 0 if current surface only is being drawn and it is not done with surface
  // numbers in mesh
  int skipEnds = !((Imodv->current_subset == SUBSET_SURF_ONLY || Imodv->current_subset ==
                    SUBSET_SURF_OTHER) && sCursurf >= 0) || mesh->surf > 0 ? 1 : 0;
 
  if (!mesh || !mesh->lsize)
    return;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  vbCleanupContVBD(obj);
  vbOK = skipEnds && !(handleFlags & HANDLE_VALUE1) && Imodv->vertBufOK > 0;

  if (!vbOK)
    vbCleanupVBD(mesh);

  /* Check to see if normals have magnitudes. */
  if (mesh->flag & IMESH_FLAG_NMAG)
    glDisable( GL_NORMALIZE );
  else
    glEnable( GL_NORMALIZE );

  ifgHandleSurfChange(obj, mesh->surf, &defProps, &curProps, &stateFlags, handleFlags);
  defTrans = defProps.trans ? 1 : 0;
  vert = mesh->vert;

  // first time in, try to set up vertex buffer drawing if it is OK
  if (!drawTrans && vbOK) {
    i = Imodv->vbManager->analyzeMesh(mesh, z, 1, obj->flags & IMOD_OBJFLAG_FCOLOR,
                                      &defProps);
    if (i != -1)
      imodTrace('b', "vbAnalyzeMesh returned %d", i);
  }
  vbd = vbOK ? mesh->vertBuf : NULL;

  // Do vertex buffer drawing
  if (vbd && vbd->vbObj) {

    // First time in, If everything is trans, set flag and skip out
    if (!drawTrans && defTrans && 
        Imodv->vbManager->checkAllTrans(obj, vbd, remnantMatchesTrans)) {
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      return;
    }

    // set up to use the VBO.  glInterleavedArrays is easier but apparently not used much
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_VERTEX_ARRAY);
    glNormalPointer(GL_FLOAT, 6 * sizeof(GLfloat), BUFFER_OFFSET(0));
    glVertexPointer(3, GL_FLOAT, 6 * sizeof(GLfloat), BUFFER_OFFSET(3 * sizeof(GLfloat)));
    
    // Draw default if it matches trans state
    if (vbd->numIndDefault) {
      if (drawTrans == defTrans) {
        imodTrace('v', "VB default, %s  %d %u %u", drawTrans ? "trans" : "solid", 
                  vbd->numIndDefault, vbd->vbObj, vbd->ebObj);
        glDrawElements(GL_TRIANGLES, vbd->numIndDefault, GL_UNSIGNED_INT,
                       BUFFER_OFFSET(0));
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    }
    cumInd = vbd->numIndDefault;

    // Draw special sets, keeping cumulative index for offset
    for (j = 0; j < vbd->numSpecialSets; j++) {
      Imodv->vbManager->unpackRGBT(vbd->rgbtSpecial[j], red, green, blue, trans);
      if ((trans ? 1 : 0) == drawTrans) {
        imodTrace('v', "VB special %d, %s, %.2f %.2f %.2f %d", j, 
                  drawTrans ? "trans" : "solid", red, green, blue, trans);
        ifgHandleColorTrans(obj, red, green, blue, trans);
        glDrawElements(GL_TRIANGLES, vbd->numIndSpecial[j], GL_UNSIGNED_INT,
                       BUFFER_OFFSET(cumInd * sizeof(GLuint)));
      } else
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      cumInd += vbd->numIndSpecial[j];
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
    b3dBindBuffer(GL_ARRAY_BUFFER, 0);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

    // Set up for remnant drawing unless it is all trans the first time
    if (!(!drawTrans && defTrans && !remnantMatchesTrans)) {
      imodMeshCopy(mesh, &meshCopy);
      mesh = &meshCopy;
      mesh->list = vbd->remnantIndList;
      
      mesh->lsize = vbd->numRemnant;
      mesh->store = vbd->remnantStore;
      imodTrace('v', "Remnant %s", drawTrans ? "trans" : "solid");
      ifgHandleSurfChange(obj, mesh->surf, &defProps, &curProps, &stateFlags, 
                          handleFlags);
    } else {
      //glFlush();
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      return;
    }

    // Now do or continue with old-style drawing element by element
  } else {

    // First time in, if the trans state does not match the draw state, and the 
    // storage list does not have a change to a matching state, return
    if (!drawTrans && defTrans && !istoreTransStateMatches(mesh->store, 0)) {
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      return;
    }
  }

  if (imodDebug('v'))
    imodPrintStderr("draw mesh %s\n", drawTrans ? "trans" : "solid");
  mlist = mesh->list;
  stateFlags = 0;
  nextItemIndex = nextChange = istoreFirstChangeIndex(mesh->store);

  for(i = 0; i < mesh->lsize; i++){
    switch(mlist[i]){

    case IMOD_MESH_BGNTRI:
      glBegin(GL_TRIANGLE_STRIP);
      break;
    case IMOD_MESH_ENDTRI:
      glEnd();
      break;

    case IMOD_MESH_BGNPOLY:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      glBegin(GL_POLYGON);
      break;

    case IMOD_MESH_NORMAL:
      i++;
      if ((mlist[i] < mesh->vsize) && (mlist[i] > -1)){
        glNormal3f(vert[mlist[i]].x,
                   vert[mlist[i]].y,
                   vert[mlist[i]].z);
      }
      break;

    case IMOD_MESH_BGNPOLYNORM:

      /* 6/19/01 note: using glVertex3fv with no z scaling increases
         speed by 5% on PC, 0.2% on SGI */
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      glBegin(GL_TRIANGLES);
      i++;
      while (mlist[i] != IMOD_MESH_ENDPOLY) {
        unsigned int li;
        glNormal3fv((float *)&(vert[mlist[i++]])); 
        li = mlist[i++];
        glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
                   

        glNormal3fv((float *)&(vert[mlist[i++]]));
        li = mlist[i++];
        glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);

        glNormal3fv((float *)&(vert[mlist[i++]]));
        li = mlist[i++];
        glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
        if ((i%512) == 0)
          glFinish();
      }
      glEnd();
      //glFlush();
      break;

    case IMOD_MESH_BGNPOLYNORM2:

      if (skipNonCurrentSurface(mesh, &i, obj)) {
        nextChange = nextItemIndex = istoreSkipToIndex(mesh->store, i);
        break;
      }
      glBegin(GL_TRIANGLES);
      i++;

      // Before starting loop, check if need to skip to matching trans state
      if ((curProps.trans ? 1 : 0) != drawTrans &&
          (nextChange < i || nextChange > i + 2)) {
        nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, drawTrans, &i,
                                                       skipEnds);
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
        if (imodDebug('v'))
          imodPrintStderr("Skipped from start to %d\n", i);
      }

      while (mlist[i] != IMOD_MESH_ENDPOLY) {
        unsigned int li;
        if (nextChange < i || nextChange > i + 2) {
          //imodPrintStderr("same %d  nextChange %d\n", i, nextChange);

          li = mlist[i++];
          glNormal3fv((float *)&(vert[li + 1])); 
          glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
          
          li = mlist[i++];
          glNormal3fv((float *)&(vert[li + 1])); 
          glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
          
          li = mlist[i++];
          glNormal3fv((float *)&(vert[li + 1])); 
          glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
        } else {

          // Isolate a triangle with changes from other triangles with an
          // End/begin pair regardless of type of change - needed on non-quadro
          // Nvidia in Linux, and Nvidia on a Mac
          glEnd();

          // Get the next change for the first point
          if (stateFlags || i == nextChange) {
            nextChange = ifgHandleMeshChange(obj, mesh->store, &defProps, &curProps,
                                             &nextItemIndex, i, &stateFlags, &changeFlags,
                                             handleFlags);

            // If trans state does not match draw state, return to default
            // and skip to next matching triangle
            if ((curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              if (stateFlags)
                ifgHandleMeshChange(obj, mesh->store, &defProps, &curProps,
                                    &nextItemIndex, 0, &stateFlags, &changeFlags, 
                                    handleFlags);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, drawTrans,
                                                             &i, skipEnds);
              obj->flags |= IMOD_OBJFLAG_TEMPUSE;
              glBegin(GL_TRIANGLES);
              if (imodDebug('v'))
                imodPrintStderr("Skipping to %d\n", i);
              continue;
            }
          }

          glBegin(GL_TRIANGLES);
          for (j = 0; j < 3; j++) {
            if (j && (stateFlags || i == nextChange))
              nextChange = ifgHandleMeshChange(obj, mesh->store, &defProps, &curProps,
                                               &nextItemIndex, i, &stateFlags,
                                               &changeFlags, handleFlags);
            li = mlist[i++];
            glNormal3fv((float *)&(vert[li + 1])); 
            glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
          }

          // Again, isolate this triangle from further ones.
          glEnd();

          /*imodPrintStderr("after changes %d  state %d  nextch %d nii %d\n",
            i - 3, stateFlags, nextChange, nextItemIndex); */
          // Reset if not in default state and the next positive change will
          // not be in the next triangle
          if (stateFlags && (nextItemIndex < i || nextItemIndex > i + 2 ||
                             mlist[i] == IMOD_MESH_ENDPOLY)) {
            /*imodPrintStderr("resetting, state %d\n", stateFlags); */
            nextChange = ifgHandleMeshChange(obj, mesh->store, &defProps, &curProps,
                                             &nextItemIndex, i, &stateFlags, &changeFlags,
                                             handleFlags);
            if (mlist[i] != IMOD_MESH_ENDPOLY && (curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, drawTrans,
                                                             &i, skipEnds);
              obj->flags |= IMOD_OBJFLAG_TEMPUSE;
              if (imodDebug('v'))
                imodPrintStderr("Skipping to %d\n", i);
            }
          }
          glBegin(GL_TRIANGLES);
        }
        
      }
      // istoreDump(mesh->store);
      glEnd();
      //glFlush();
      break;

    case IMOD_MESH_BGNBIGPOLY:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      tobj = gluNewTess();
      gluTessCallback(tobj, GLU_BEGIN, (GLU_CALLBACK)glBegin);
      gluTessCallback(tobj, GLU_VERTEX, (GLU_CALLBACK)glVertex3fv);
      gluTessCallback(tobj, GLU_END, (GLU_CALLBACK)glEnd);
      glPushMatrix();
      glScalef(1.0f, 1.0f, z);
      gluBeginPolygon(tobj);
      while(mlist[++i] != IMOD_MESH_ENDPOLY){
        v[0] = vert[mlist[i]].x;
        v[1] = vert[mlist[i]].y;
        v[2] = vert[mlist[i]].z;
        gluTessVertex(tobj, v, &(vert[mlist[i]]));
      }
      gluEndPolygon(tobj);
      gluDeleteTess(tobj);
      glPopMatrix();
      break;

    case IMOD_MESH_END:
      return;

    case IMOD_MESH_SWAP:
      imodPrintStderr("imodlib: old mesh\n");
      return;

    default:
      if ((mlist[i] < mesh->vsize) && (mlist[i] > -1)){
        glVertex3f(vert[mlist[i]].x,
                   vert[mlist[i]].y,
                   vert[mlist[i]].z * z);
      }
      break;
    }
    //glFlush();
  }
  return;
}

/*
 * SCALAR MESH DRAWING ROUTINES
 */


static void imodvDrawScalarMesh(Imesh *mesh, double zscale, 
                         Iobj *obj, int drawTrans)
{
  int i, j;
  float z = zscale;
  float mag;
  GLUtesselator *tobj;
  GLdouble v[3];
  Ipoint *n;
  unsigned char luv;
  int trans = (int)(2.55f * (100.0 - (float)obj->trans));

  int listInc, vertBase, normAdd;
  static unsigned char cmap[3][256];

  int useLight = Imodv->lighting;

  GLenum polyStyle;

  if ((!mesh) || (!mesh->lsize))
    return;

  // Skip drawing if trans state does not match draw state
  if ((obj->trans ? 1 : 0) != drawTrans) {
    obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    return;
  }
  if (imodDebug('v'))
    imodPrintStderr("draw scalar mesh %s\n", drawTrans ? "trans" : "solid");

  if (iobjFill(obj->flags)){
    polyStyle = GL_POLYGON;
  }else if (iobjLine(obj->flags)){
    polyStyle = GL_LINE_STRIP;
    zscale = 1.0;
  }else{
    polyStyle = GL_POINTS;
    zscale = 1.0;
  }

  ifgMakeValueMap(obj, cmap);

  /*
   * Loop through mesh data and draw it.
   */
  for(i = 0; i < mesh->lsize; i++){
    switch(mesh->list[i]){
    case IMOD_MESH_BGNTRI:
      glBegin(GL_TRIANGLE_STRIP);
      break;
    case IMOD_MESH_ENDTRI:
      glEnd();
      break;
    case IMOD_MESH_BGNPOLY:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      glBegin(GL_POLYGON);
      break;
    case IMOD_MESH_NORMAL:
      i++;
      if ((mesh->list[i] < mesh->vsize) && (mesh->list[i] > -1)){
        glNormal3f(mesh->vert[mesh->list[i]].x,
                   mesh->vert[mesh->list[i]].y,
                   mesh->vert[mesh->list[i]].z);
      }
      break;

    case IMOD_MESH_BGNPOLYNORM:
    case IMOD_MESH_BGNPOLYNORM2:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, &normAdd);
      i++;
      while(mesh->list[i] != IMOD_MESH_ENDPOLY){
        glBegin(polyStyle);
                    
        for (j = 0; j < 3; j++) {
          n = &mesh->vert[mesh->list[i] + normAdd];
          mag = 255.0 * sqrt
            ((n->x * n->x) + (n->y * n->y) + (n->z * n->z));
          luv = (unsigned char)mag; 

          if (useLight)
            light_adjust(obj, cmap[0][luv]/255.0f, 
                         cmap[1][luv]/255.0f,
                         cmap[2][luv]/255.0f, obj->trans);

          glColor4ub(cmap[0][luv], 
                     cmap[1][luv], 
                     cmap[2][luv],
                     trans);
          glNormal3f(n->x, n->y, n->z);
          n = &mesh->vert[mesh->list[i + vertBase]];
          glVertex3f(n->x, n->y, n->z * zscale);

          i += listInc;
        }
                    
        glEnd();
      }

      break;

    case IMOD_MESH_BGNBIGPOLY:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      tobj = gluNewTess();
      gluTessCallback(tobj, GLU_BEGIN, (GLU_CALLBACK)glBegin);
      gluTessCallback(tobj, GLU_VERTEX, (GLU_CALLBACK)glVertex3fv);
      gluTessCallback(tobj, GLU_END, (GLU_CALLBACK)glEnd);
      glPushMatrix();
      glScalef(1.0f, 1.0f, z);
      gluBeginPolygon(tobj);
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
        v[0] = mesh->vert[mesh->list[i]].x;
        v[1] = mesh->vert[mesh->list[i]].y;
        v[2] = mesh->vert[mesh->list[i]].z;
        gluTessVertex(tobj, v, &(mesh->vert[mesh->list[i]]));
      }
      gluEndPolygon(tobj);
      gluDeleteTess(tobj);
      glPopMatrix();
      break;

    case IMOD_MESH_END:
      return;

    case IMOD_MESH_SWAP:
      imodPrintStderr("imodlib: old mesh\n");
      return;

    default:
      if ((mesh->list[i] < mesh->vsize) && (mesh->list[i] > -1)){
        glVertex3f(mesh->vert[mesh->list[i]].x,
                   mesh->vert[mesh->list[i]].y,
                   mesh->vert[mesh->list[i]].z * z);
      }
      break;
    }
  }
  return;
}

// Check if surface subset and if so, see if mesh matches current surface
static int skipNonCurrentSurface(Imesh *mesh, int *ip, Iobj *obj)
{
  int co, pt, i, ind, listSkip, vertOffset, numTest, endCode, limTest = 9;
  float xx, yy, zz;
  Icont *cont;

  // Test if surface subset on, it's also OK if the mesh surface is greater
  // than zero because a match was already tested for in checkMeshDraw
  if (!((Imodv->current_subset == SUBSET_SURF_ONLY || Imodv->current_subset == 
         SUBSET_SURF_OTHER) && sCursurf >= 0) || mesh->surf > 0)
    return 0;

  // Set up indexes, offset and interval to check, ending code
  i = *ip + 1;
  listSkip = 1;
  vertOffset = 0;
  endCode = IMOD_MESH_ENDPOLY;
  
  if (mesh->list[i - 1] == IMOD_MESH_BGNPOLYNORM) {
    listSkip = 2;
    vertOffset = 1;
  }
  if (mesh->list[i - 1] == IMOD_MESH_BGNPOLY)
    endCode = IMOD_MESH_END;

  // Test up to the given limit of vertices in the mesh
  numTest = 0;
  while (mesh->list[i] != endCode && numTest < limTest) {
    ind = mesh->list[i + vertOffset];
    xx = mesh->vert[ind].x;
    yy = mesh->vert[ind].y;
    zz = mesh->vert[ind].z;
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      
      // If contour is not wild and Z is different, or if surface doesn't match
      // then skip the contour
      if (!cont->psize || (!(cont->flags & ICONT_WILD) && zz != cont->pts->z))
        continue;
      if (cont->surf != sCursurf)
        continue;

      // Return upon an exact match
      for (pt = 0; pt < cont->psize; pt++)
        if (cont->pts[pt].x == xx && cont->pts[pt].y == yy && 
            cont->pts[pt].z == zz)
          return 0;
    }
    numTest++;
    i += listSkip;
  }

  // Most efficient to loop to the end code here
  while (mesh->list[*ip] != endCode)
    (*ip)++;
    
  return 1;
}

void imodvSelectVisibleConts(ImodvApp *a, int &pickedOb, int &pickedCo)
{
  Imod *imod = a->imod;
  int ob = imod->cindex.object;
  int co, pt, checkTime;
  Iobj *obj;
  Icont *cont;
  int stateFlags, handleFlags = 0;
  DrawProps contProps, ptProps;
  Iindex cindex;
  int numSel = 0;
  int nPlanes;
  GLint maxPlanes;
  Iplane plane[2 * IMOD_CLIPSIZE];

  if (ob < 0)
    return;
  obj = &imod->obj[ob];
  checkTime = (int)iobjTime(obj->flags);

  sCTime = imod->ctime;
  if (!sCTime)
    checkTime = 0;
  if (iobjOff(obj->flags) || !iobjLine(obj->flags))
    return;

  glGetIntegerv(GL_MAX_CLIP_PLANES, &maxPlanes);
  nPlanes = 0;
  imodPlaneSetFromClips(&obj->clips, &imod->view->clips, plane, maxPlanes, 
                        &nPlanes);

  imodSelectionListClear(a->vi);

  set_curcontsurf(ob, imod);
  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;
  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    //imodPrintStderr("contour %d planes %d\n", co, nPlanes);
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;
    //imodPuts("passed draw check");
    ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, 0);
    if (contProps.gap)
      continue;
    //imodPuts("passed gap");

    // Check the clipping planes; if any point is visible, break and accept
    if (nPlanes > 0) {
      for (pt = 0; pt < cont->psize; pt++)
        if (imodPlanesClip(plane, nPlanes, &cont->pts[pt]))
          break;
      if (pt >= cont->psize)
        continue;
    }
    //imodPuts("passed clip");

    // The first time, just set the model index
    // The second time, add previous index to selection list
    // After the first time, add every index to the selection list
    if (numSel == 1)
      imodSelectionListAdd(a->vi, imod->cindex);
    cindex.contour = co;
    cindex.object = ob;
    cindex.point = -1;
    imod->cindex = cindex;
    if (numSel)
      imodSelectionListAdd(a->vi, cindex);
    numSel++;
    pickedCo = co;
    pickedOb = ob;
  }
}

/*
 * Draw the current clip plane if it is on
 */
static void drawCurrentClipPlane(ImodvApp *a)
{
  IclipPlanes *clips;
  double beta, alpha;
  Imat *mat = imodMatNew(3);
  Iview *vw = a->imod->view;
  float radfrac = 0.95f;
  Ipoint corner, xcorn, cen;
  int ind, ip;
  float vx[4], vy[4], vz[4];
  float zscale = a->imod->zscale ? a->imod->zscale : 1.;
  
  objedObject();
  if (!mat || !a->obj)
    return;
  clips = a->imod->editGlobalClip ? &a->imod->view->clips : &a->obj->clips;
  ip = clips->plane;
  if (!(clips->flags & (1 << ip)))
    return;

  clipCenterAndAngles(a, &clips->point[ip], &clips->normal[ip], &cen, alpha,
                      beta);
  imodMatRot(mat, -(float)(alpha / RADIANS_PER_DEGREE), b3dX);
  imodMatRot(mat, -(float)(beta / RADIANS_PER_DEGREE), b3dY);

  // Compute and draw 4 corner points.
  // It works best if you draw lines before plane
  glColor4ub(255, 0, 0, 255);
  glBegin(GL_LINE_LOOP);
  for (ind = 0; ind < 4; ind++) {
    corner.x = ((ind == 1 || ind == 2) ? -1 : 1) * radfrac * vw->rad;
    corner.y = ((ind / 2 == 1) ? -1 : 1) * radfrac * vw->rad;
    corner.z = 0.;
    imodMatTransform3D(mat, &corner, &xcorn);
    vx[ind] = cen.x + xcorn.x;
    vy[ind] = cen.y + xcorn.y;
    vz[ind] = cen.z + xcorn.z / zscale;
    glVertex3f(vx[ind], vy[ind], vz[ind]);
    //    imodPrintStderr("xcorn %.2f %.2f %.2f\n", xcorn.x, xcorn.y, xcorn.z);
  }
  glEnd();
  glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA); 
  glEnable(GL_BLEND); 

  glColor4ub(255, 0, 0, 96);
  glBegin(GL_POLYGON);

  for (ind = 0; ind < 4; ind++)
    glVertex3f(vx[ind], vy[ind], vz[ind]);
  glEnd();
  glBlendFunc(GL_ONE,  GL_ZERO);
  glDisable(GL_BLEND);

  imodMatDelete(mat);
}

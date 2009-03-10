/*
 *  imodv_ogl.c -- OpenGL Drawing functions to draw models, etc in imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include <qgl.h>
#include "imodv_image.h"
#include "imod.h"
#include "imodv.h"
#include "imod_edit.h"
#include "imodv_gfx.h"
#include "imodv_ogl.h"
#include "imodv_objed.h"
#include "imodv_input.h"
#include "imodv_light.h"
#include "imodv_stereo.h"
#include "istore.h"
#include "finegrain.h"

extern float Imodv_light_position[4];
extern Ipoint ImodvCurModLight;

#define DRAW_POINTS 1
#define DRAW_LINES  2
#define DRAW_FILL   3
#define DRAW_OBJECT -1
#define NO_NAME 0xffffffff

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
static int checkContourDraw(Icont *cont, int co, int checkTime);
static void imodvSetObject(Iobj *obj, int style, int drawTrans);
static void set_curcontsurf(int ob, Imod* imod);
static void imodvUnsetObject(Iobj *obj);
static int clip_obj(Imod *imod, Iobj *obj, int flag);
static int skipNonCurrentSurface(Imesh *mesh, int *ip, Iobj *obj);
static bool checkThickerContour(int co);
static void drawCurrentClipPlane(ImodvApp *a);

static int CTime = -1;
static float depthShift;
static int cursurf, curcont;
static int thickCont, thickObj, objBeingDrawn, modBeingDrawn;
static int curTessObj;

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
            znear + depthShift , zfar + depthShift);
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

  glTranslatef(0.0, 0.0, (float) (-cdist + depthShift));
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
 
  depthShift = 0.0f;

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

  depthShift = 0.6 * drange;
   
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

  vw = imod->view;
  glMatrixMode(GL_MODELVIEW);

  glLoadIdentity();

  /* DNM: The last operation is a translation to get positive Z's if doing
     depth cueing */

  glTranslatef(0.0f, 0.0f, -depthShift);

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
  glMatrixMode(GL_PROJECTION);

  switch (a->stereo){
  case -IMODV_STEREO_RL:
    glViewport(0, 0, a->winx, a->winy);
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    break;

  case IMODV_STEREO_RL:
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(-a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    glViewport(a->winx, 0, a->winx, a->winy);
    break;

  case -IMODV_STEREO_TB:
    glViewport(0, -(imodvStereoVoffset() / 2), a->winx, a->winy);
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    glScalef(1.0f, 0.5f, 0.5f);
    break;

  case IMODV_STEREO_TB:
    glTranslatef(a->winx/2, a->winy/2, 0.0f);
    glRotatef(-a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
    glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
    glScalef(1.0f, 0.5f, 0.5f);
    glViewport(0, a->winy + imodvStereoVoffset() - (imodvStereoVoffset() / 2),
               a->winx, a->winy);
    break;

  case -IMODV_STEREO_HW:
    glRotatef(-a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
#ifdef __sgi
    /* DNM: cut these values in half to get model at same zoom relative
       to the size of the window */
    glScalef(1.0f, 0.5f, 0.5f);
#endif
    break;

  case IMODV_STEREO_HW:
    glRotatef(a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
#ifdef __sgi
    glScalef(1.0f, 0.5f, 0.5f);
#endif
    break;

  }
}


void imodvDraw_models(ImodvApp *a)
{
  int m;

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

  switch (a->drawall){
  case 0:
    modBeingDrawn = a->cm;
    glLoadName(a->cm);
    imodvDraw_model(a, a->imod);
    break;
  case 2:
    modBeingDrawn = a->cm;
    imodvDraw_model(a, a->imod);
    m = a->cm + 1;
    if (m < a->nm){
      modBeingDrawn = m;
      glLoadName(m);
      imodvDraw_model(a, a->mod[m]);
    }
    break;

  case 1:
    imodvDraw_model(a, a->imod);
    m = a->cm - 1;
    if (m >= 0){
      modBeingDrawn = m;
      glLoadName(m);
      imodvDraw_model(a, a->mod[m]);
    }
    break;

  case 3:
    for (m = 0; m < a->nm; m++){
      modBeingDrawn = m;
      glLoadName(m);
      imodvDraw_model(a, a->mod[m]);
    }
    break;
  }

  // DNM 5/14/04: now tell routine to draw transparent stuff
  if (!a->standalone){
    imodvSetViewbyModel(a, a->imod);
    imodvSetModelTrans(a->imod);
    setStereoProjection(a);
    imodvDrawImage(a, 1);
  }
     
  glPopName();
  return;
}

// Sets the values for current contour and surface and object being drawn,
// and object and contour number if contour is to be drawn thick
static void set_curcontsurf(int ob, Imod* imod)
{
  Iobj  *obj;
  curcont = -1;
  cursurf = -1;
  objBeingDrawn = ob;
  if (ob >= 0)
    obj  = &(imod->obj[ob]);
  else
    obj = ivwGetAnExtraObject(Imodv->vi, -ob - 1);
  if (Imodv->current_subset && imod->cindex.object == ob) {
    curcont = imod->cindex.contour;
    if (curcont >= 0)
      cursurf = imod->obj[imod->cindex.object].cont[curcont].surf;
  }
  
  // Set contour to thicken if flag set and either we are in the current object
  // or there is a non-empty selection list
  thickCont = -1;
  thickObj = -1;
  if (obj->flags & IMOD_OBJFLAG_THICK_CONT) {
    if (imod->cindex.object == ob || ilistSize(Imodv->vi->selectionList))
      thickCont = imod->cindex.contour;
    if (imod->cindex.object == ob) 
      thickObj = ob;
  }
}

static bool checkThickerContour(int co)
{
  return ((co == thickCont && objBeingDrawn == thickObj )|| 
          (thickCont >= 0 && imodSelectionListQuery
           (Imodv->vi, objBeingDrawn, co) > -2));
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
     steps for stereo display to happen inside thsi routine */

  imodvSetViewbyModel(a, imod);

  imodvSetModelTrans(imod);

  setStereoProjection(a);

  imodvSetLight(imod->view);

  CTime = imod->ctime;
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
    if ((a->current_subset == 1 || a->current_subset == 2 || 
         a->current_subset == 4 )&& imod->cindex.object >= 0) {
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
          curTessObj = ob;
          clip_obj(imod, obj, 1);
          imodvDraw_object( obj , imod, drawTrans);
          clip_obj(imod, obj, 0);
          glFinish();
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
  glFinish();
  
  glPopName();
  if (a->drawClip && imod == a->imod)
    drawCurrentClipPlane(a);
  if (imod == a->imod) {
      ImodvCurModLight.x = Imodv_light_position[0];
      ImodvCurModLight.y = Imodv_light_position[1];
      ImodvCurModLight.z = Imodv_light_position[2];
  }
}

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
  if (flag)
    glFlush();
  return(0);
}

/* In which any mode enabled by imodvSetObject should be disabled */
static void imodvUnsetObject(Iobj *obj)
{
  light_off();
  glDisable(GL_BLEND);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_CULL_FACE);
  return;
}

static void imodvSetObject(Iobj *obj, int style, int drawTrans)
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

    if (obj->flags & IMOD_OBJFLAG_FCOLOR){
      red   = (float)obj->fillred / 255.0f;
      green = (float)obj->fillgreen / 255.0f;
      blue  = (float)obj->fillblue / 255.0f;
      glColor4f(red, green, blue, trans);
    }else{
      glColor4f(obj->red, obj->green, obj->blue, trans);
    }

    if ((Imodv->lighting) && (!Imodv->wireframe)){
      light_on(obj, modBeingDrawn);
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
  if (drawTrans){
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (!(obj->flags & IMOD_OBJFLAG_TWO_SIDE))
      glEnable(GL_CULL_FACE);
  }

  return;
}

/* A simple function to check whether the current mesh should be drawn, based
   on time, surface number, and resolution */
static int checkMeshDraw(Imesh *mesh, int checkTime, int resol)
{
  if ((checkTime) && (mesh->time) && (mesh->time != CTime))
    return 0;
  if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && mesh->surf > 0 && 
      mesh->surf != cursurf)
    return 0;
  if (imeshResol(mesh->flag) == resol)
    return 1;
  return 0;
}

/* And to check whether a contour should be drawn */
static int checkContourDraw(Icont *cont, int co, int checkTime)
{
    if (!cont->psize) 
      return 0;
    if ((checkTime) && (cont->time) && (cont->time != CTime))
      return 0;
    if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && 
        cont->surf != cursurf)
      return 0;

    if (Imodv->current_subset / 2 == 2 && curcont >= 0 && co != curcont &&
        imodSelectionListQuery(Imodv->vi, Imodv->imod->cindex.object, co) < 0)
      return 0;
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

  if (!CTime)
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
      imodvSetObject(obj, DRAW_FILL, drawTrans);
      imodvDraw_spheres(obj, zscale, DRAW_FILL, drawTrans );
      flagSave |= (obj->flags & IMOD_OBJFLAG_TEMPUSE);
      obj->flags = flagSave;
      if (iobjLine(obj->flags) && iobjFill(obj->flags)){
        imodvSetObject(obj, 0, 0);
        imodvSetObject(obj, DRAW_LINES, drawTrans);
        imodvDraw_spheres(obj, zscale, DRAW_LINES, drawTrans);
      }
    }else{
      /* or, just draw the lines if that is selected; otherwise draw
         points */
      if (iobjLine(obj->flags)){
        imodvSetObject(obj, DRAW_LINES, drawTrans);
        imodvDraw_spheres(obj, zscale, DRAW_LINES, drawTrans );
      }else{
        imodvSetObject(obj, DRAW_POINTS, drawTrans);
        imodvDraw_spheres(obj, zscale, DRAW_POINTS, drawTrans );
      }
    }
    imodvSetObject(obj, 0, 0);
    if (iobjScat(obj->flags))
      return;
  }

  if (Imodv->doPick){
    imodvPick_Contours(obj, zscale, drawTrans);
    imodvSetObject(obj, 0, 0);
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
      imodvSetObject(obj, DRAW_FILL, drawTrans);
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
        imodvSetObject(obj, 0, 0);
        imodvSetObject(obj, DRAW_LINES, drawTrans);
        for(co = 0; co < obj->meshsize; co++) {
          mesh = &(obj->mesh[co]);
          if (checkMeshDraw(mesh, checkTime, resol))
            imodvDraw_mesh(mesh, DRAW_LINES, obj, drawTrans);
        }
        /*  imodvSetObject(obj, 0); */
      }
    }else{
      /* Mesh Lines: draw lines in scalar or regular mode */
      if (iobjLine(obj->flags)){
        imodvSetObject(obj, DRAW_LINES, drawTrans);
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
        imodvSetObject(obj, DRAW_POINTS, drawTrans);
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
    imodvSetObject(obj, 0, 0);
    return;
  }

  /* Closed contours with Fill: draw the fill, then draw the outside lines
     if Fill Outline is selected */
  if ((iobjClose(obj->flags)) && (iobjFill(obj->flags))){
    imodvSetObject(obj, DRAW_FILL, drawTrans);
    /* We have to either turn off the light or set the normal to
       something, to have display be independent of the state of the
       last object. Lighting makes it look too bright.  Also turn off
       back-face culling in case of trans */
    light_off(); 
    glDisable(GL_CULL_FACE);
    /* glNormal3f(0., 0., 1.); */
    imodvDraw_filled_contours(obj, drawTrans);
    if (iobjLine(obj->flags)){
      imodvSetObject(obj, 0, 0);
      imodvSetObject(obj, DRAW_LINES, drawTrans);
      imodvDraw_contours(obj, GL_LINE_LOOP, drawTrans);
    }
  }else{
    /* Contours as lines or points; draw as open or closed lines */
    if (iobjLine(obj->flags)){
      imodvSetObject(obj, DRAW_LINES, drawTrans);
      if (iobjClose(obj->flags)){
        imodvDraw_contours(obj, GL_LINE_LOOP, drawTrans );
      }else{
        imodvDraw_contours(obj, GL_LINE_STRIP, drawTrans );
      }
    }else{
      imodvSetObject(obj, DRAW_POINTS, drawTrans);
      imodvDraw_contours(obj, GL_POINTS, drawTrans );
    }
  }

  imodvSetObject(obj, 0, 0);
  return;
}




/****************************************************************************/
/* DRAW CONTOURS 
 */
#define PICKPOINTS
static void imodvPick_Contours(Iobj *obj, double zscale, int drawTrans)
{
  int co, pt, npt;
  Icont *cont;
  Imesh *mesh;
  int i, li;
  float z = zscale;
  Ipoint *vert;
  int    *list;
  int pmode = GL_POINTS;
  int doLines = 0;
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_MESH_COLOR | HANDLE_3DWIDTH;
  int checkTime = (int)iobjTime(obj->flags);
  if (!CTime)
    checkTime = 0;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  // Skip drawing if trans state does not match draw state
  if ((obj->trans ? 1 : 0) != drawTrans) {
    obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    return;
  }

#ifdef PICKPOINTS
  /* pick points first */
  // DNM 6/25/05: What is the point?  Can't detect a difference in performance,
  // but leave it for now.
  glPushName(NO_NAME);
  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    if (!checkContourDraw(cont, co, checkTime))
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, 0);
    if (contProps.gap)
      continue;

    glLoadName(co);
    glPushName(NO_NAME);
    for (pt = 0; pt < cont->psize; pt++) {
      if (nextChange == pt)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, 
                                         &changeFlags, handleFlags, 0);
      glLoadName(pt);
      glBegin(GL_POINTS);
      glVertex3fv((GLfloat *)&(cont->pts[pt]));
      glEnd();
    }
    glPopName();
  }
  glPopName();
#endif    

  if (iobjLine(obj->flags)){
    pmode = GL_LINES;
    doLines = 1;
  }

  glPushName(NO_NAME);
  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    if (!checkContourDraw(cont, co, checkTime))
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
      }
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

  // If there are no contours but there are meshes, draw the triangles
  if (!obj->contsize && obj->meshsize) {
    for (co = 0; co < obj->meshsize; co++) {
      mesh = &(obj->mesh[co]);
      list = mesh->list;
      vert = mesh->vert;
      if (!mesh->lsize || !mesh->vsize)
        continue;

      // For sanity sake, make sure there is a polynorm2 mesh in there when 
      // using vertices only
      for(i = 0; i < mesh->lsize; i++)
        if (list[i] == IMOD_MESH_BGNPOLYNORM2)
          break;
      if (i == mesh->lsize)
        continue;

      glLoadName(co);
      glPushName(NO_NAME);
      /*  THIS WORKED ...
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);   // LINE/FILL give same time
      for(i = 0; i < mesh->lsize; i++){
        switch(list[i]){
        case IMOD_MESH_BGNPOLYNORM2:
          i++;
          while (list[i] != IMOD_MESH_ENDPOLY) {
            li = list[i++];

            // The load name must occur outside begin-end sequence
            glLoadName(li);
            glBegin(GL_TRIANGLES);
            glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
            li = list[i++];
            glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
            li = list[i++];
            glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
            glEnd();
          }
          glFlush();
          break;
        default:
          break;
        }
        } */

      // This is 5 times faster!  And fine for isosurfaces
      for (li = 0; li < mesh->vsize; li += 2) {
        glLoadName(li);
        glBegin(GL_POINTS);
        glVertex3fv((GLfloat *)&(vert[li]));
        glEnd();
        
      }
      glPopName();
    }
  }
  glPopName();
}

static void imodvDraw_contours(Iobj *obj, int mode, int drawTrans)
{
  int co, pt, thickAdd = 0;
  Icont *cont;
  int checkTime = (int)iobjTime(obj->flags);
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags, changeFlags2;
  int handleCoFlgs = HANDLE_MESH_COLOR | HANDLE_TRANS;
  int handlePtFlgs = HANDLE_TRANS;
  if (!CTime)
    checkTime = 0;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1)) {
    handleCoFlgs |= HANDLE_VALUE1;
    handlePtFlgs |= HANDLE_VALUE1;
  }

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

  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    if (!checkContourDraw(cont, co, checkTime))
      continue;

    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleCoFlgs, 0);
    if (contProps.gap)
      continue;

    // Set thicker line if this is the current contour
    thickAdd = checkThickerContour(co) ? 2 : 0;
    
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



static int tesserror;

static void myerror(GLenum error)
{
  tesserror = error;
  /*
    imodPrintStderr("gluError %d: %s\n", error,
    gluErrorString(error));
  */
  return;
}

static int newCount;
static int curTessCont;

/* DNM 12/12/00: The PC (mesa) crashes instead of giving an error callback
   when there are crossings that require a combined point - so implement this
   routine with a message that may get people to fix the contours.  However,
   the SGI gave a call on every contour insted of just the ones with crossings,
   so need to make this all conditional */

#ifdef TESS_HACK
#define MAX_CROSSES 20
static Ipoint newPoints[MAX_CROSSES];

static void myCombine( GLdouble coords[3], Ipoint *d[4],
                       GLfloat w[4], Ipoint **dataOut )
{
  if (newCount == MAX_CROSSES){
    imodPrintStderr ("This contour has over %d crossings - fix it!\n", 
            MAX_CROSSES);
    *dataOut = &(newPoints[0]);
    return;
  }
  if (!newCount)
    imodPrintStderr("Warning: contour %d of object %d crosses itself; this may "
           "cause crashes.\n", curTessCont + 1, curTessObj + 1);
  newPoints[newCount].x=coords[0];
  newPoints[newCount].y=coords[1];
  newPoints[newCount].z=coords[2];
  *dataOut = &(newPoints[newCount++]);
}
#endif

#ifndef GLU_CALLBACK
#define GLU_CALLBACK GLvoid (*)()
#endif

static void imodvDraw_filled_contours(Iobj *obj, int drawTrans)
{
  static GLUtesselator *tobj = NULL;
  GLdouble v[3];
  Icont   *cont;
  Ipoint *pts;
  int      co, pt;
  int psize;
  int ptstr, ptend;
  DrawProps contProps, ptProps;
  int stateFlags;
  int handleFlags = ((obj->flags & IMOD_OBJFLAG_FCOLOR) 
                     ? HANDLE_MESH_FCOLOR : HANDLE_MESH_COLOR) | HANDLE_TRANS;
  int checkTime = (int)iobjTime(obj->flags);
  if (!CTime)
    checkTime = 0;

  tesserror = 0;

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

  if (!tobj){
    tobj = gluNewTess();
    gluTessCallback(tobj, GLU_BEGIN, (GLU_CALLBACK)glBegin);
    gluTessCallback(tobj, GLU_VERTEX, (GLU_CALLBACK)glVertex3fv);
    gluTessCallback(tobj, GLU_END, (GLU_CALLBACK)glEnd);
    /*        gluTessCallback(tobj, GLU_EDGE_FLAG, glEdgeFlag); */
#ifdef TESS_HACK
    gluTessCallback(tobj, GLU_TESS_COMBINE, (GLU_CALLBACK)myCombine);
#endif
    gluTessCallback(tobj, GLU_ERROR, (GLU_CALLBACK)myerror);
  }

  glPushName(NO_NAME);
  for(co = 0; co < obj->contsize ; co++){
    glLoadName(co);
    cont = &(obj->cont[co]);

    // 8/29/06: it was only checking time before (not even size)
    if (!checkContourDraw(cont, co, checkTime))
      continue;

    ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                        handleFlags, 0);
    if (contProps.gap)
      continue;
    if ((contProps.trans ? 1 : 0) != drawTrans) {
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      continue;
    }

    tesserror = 0;
    ptend = cont->psize;
    ptstr = 0;
    pts = cont->pts;
    curTessCont = co;
    /* imodPrintStderr(".%d-%d", co, ptend);
       fflush(stdout); */
    newCount = 0;
    if (ptend){

#ifdef TESS_HACK
      /* Hack for PC (mesa), need to eliminate colinear points
         from the start and end of contour, just test for points at
         same X and Y levels */
      while (ptend > 2) {
        if (((pts[0].x == pts[ptend - 1].x) && 
             (pts[ptend - 1].x == pts[ptend - 2].x) ) ||
            ((pts[0].y == pts[ptend - 1].y) && 
             (pts[ptend - 1].y == pts[ptend - 2].y) ))
          ptend--;
        else
          break;
      }
      while (ptend - ptstr > 2) {
        if (((pts[ptstr].x == pts[ptend - 1].x) && 
             (pts[ptstr].x == pts[ptstr + 1].x) ) ||
            ((pts[ptstr].y == pts[ptend - 1].y) && 
             (pts[ptstr].y == pts[ptstr + 1].y) ))
          ptstr++;
        else
          break;
      }
#endif

      psize = ptend + 1;
      do {
        tesserror = 0;
        psize--;

        if (psize - ptstr < 3)
          break;

        gluTessBeginPolygon(tobj, NULL);
        gluTessBeginContour(tobj);
        for(pt = ptstr; pt < psize; pt++){
          v[0] = cont->pts[pt].x;
          v[1] = cont->pts[pt].y;
          v[2] = cont->pts[pt].z;
          gluTessVertex(tobj, v, &(cont->pts[pt]));
        }
        gluTessEndContour(tobj);
        gluTessEndPolygon(tobj);

        if ((!tesserror) && ((psize - ptend) > 3)){
          gluTessBeginPolygon(tobj, NULL);
          gluTessBeginContour(tobj);
          for(pt = psize; pt < ptend; pt++){
            v[0] = cont->pts[pt].x;
            v[1] = cont->pts[pt].y;
            v[2] = cont->pts[pt].z;
            gluTessVertex(tobj, v, &(cont->pts[pt]));
          }
          gluTessEndContour(tobj);
          gluTessEndPolygon(tobj);
        }

      }while(tesserror);
    }
  }
  glPopName();
  return;
}

#define MAX_QUALITY 5
#define MAX_LOOKUP 120
#define MAX_MEASURES 9
static int sphereRes[MAX_LOOKUP][MAX_QUALITY];
static float measuredSize[MAX_MEASURES] = 
  { 1.5, 2.5, 3.75, 7.5, 30., 40., 60., 80., 120.};
static int measuredRes[MAX_MEASURES][MAX_QUALITY] = {
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
static int firstSphere = 1;

/***************************************************************************/
/* Draw point spheres. */
static void imodvDraw_spheres(Iobj *obj, double zscale, int style, 
                              int drawTrans)
{
  int co, pt;
  Icont *cont;
  float z = zscale;
  int checkTime = (int)iobjTime(obj->flags);
  float drawsize;
  int pixsize, quality, i, j, k, mink, stepRes, xybin;
  float scale, diff, mindiff;
  DrawProps contProps, ptProps;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_3DWIDTH | HANDLE_TRANS |
    (style == DRAW_FILL && (obj->flags & IMOD_OBJFLAG_FCOLOR)
     ? HANDLE_MESH_FCOLOR : HANDLE_MESH_COLOR);
  GLuint listIndex;
#ifdef GLU_QUADRIC_HACK
  GLUquadricObj *qobj = gluNewQuadric();
#else
  static GLUquadricObj *qobj = NULL;
  if (!qobj)
    qobj = gluNewQuadric();
#endif

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  /* first time, build lookup tables for sphere resolution versus size and
     quality */
  if (firstSphere) {
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
        sphereRes[j][i] = measuredRes[mink][i] + 2;
      }
    firstSphere = 0;
  }

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

  xybin = 1;
  if (!Imodv->standalone)
    xybin = Imodv->vi->xybin;

  /* Take maximum of quality from world flag setting and from object */
  quality = ((Imodv->mod[modBeingDrawn]->view->world & WORLD_QUALITY_BITS) >> 
    WORLD_QUALITY_SHIFT) + 1;
  if (quality <= obj->quality)
    quality = obj->quality + 1;

  if (Imodv->lowres)
    quality = 0;
  if (quality >= MAX_QUALITY)
    quality = MAX_QUALITY - 1;

  scale = 0.5 * (Imodv->winx > Imodv->winy ? Imodv->winy : Imodv->winx) / 
    Imodv->mod[modBeingDrawn]->view->rad;

  if (!CTime)
    checkTime = 0;

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

  /* DNM: Get a display list to draw the default size.  Helps a lot on PC,
     only a bit on the SGI */
  drawsize = obj->pdrawsize / xybin;
  pixsize = (int)(drawsize * scale);
  if (pixsize >= MAX_LOOKUP)
    pixsize = MAX_LOOKUP - 1;
  stepRes = sphereRes[pixsize][quality];
  listIndex = glGenLists(1);
  glNewList(listIndex, GL_COMPILE);
  gluSphere(qobj, drawsize , stepRes * 2, stepRes);
  glEndList();

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    glLoadName(co);
    if (!checkContourDraw(cont, co, checkTime))
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
    if (checkThickerContour(co))
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
      drawsize = imodPointGetSize(obj, cont, pt) / xybin;

      // Only draw zero-size points with scattered point objects
      if (!iobjScat(obj->flags) && !drawsize) 
        continue;

      glLoadName(pt);
      glPushMatrix();
      glTranslatef(cont->pts[pt].x, cont->pts[pt].y,
                   cont->pts[pt].z*z);
               
      if (drawsize == obj->pdrawsize)
        /* Use the display list if default size */
        glCallList(listIndex);
      else {
        pixsize = (int)(drawsize * scale);
        if (pixsize >= MAX_LOOKUP)
          pixsize = MAX_LOOKUP - 1;
        stepRes = sphereRes[pixsize][quality];
        gluSphere(qobj, drawsize , stepRes * 2, stepRes);
      }
      glPopMatrix();
    }
    glPopName();
    if (checkThickerContour(co))
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
  int nextChange, stateFlags, changeFlags, nextItemIndex;
  int handleFlags = HANDLE_MESH_COLOR | HANDLE_3DWIDTH | HANDLE_TRANS;
  float *firstPt;
  float firstRed, firstGreen, firstBlue;
  int firstTrans, defTrans;

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
  case DRAW_FILL:   // Unused
    polyStyle = GL_POLYGON;
    normStyle   = GL_TRIANGLES;
    break;
  }

  if (!mesh)
    return;
  if (!mesh->lsize)
    return;

  ifgHandleSurfChange(obj, mesh->surf, &defProps, &curProps, &stateFlags,
                      handleFlags);
  defTrans = defProps.trans ? 1 : 0;

  // First time in, if the trans state does not match the draw state, and the 
  // storage list does not have a change to a matching state, return
  if (!drawTrans && defTrans &&
      !istoreTransStateMatches(mesh->store, 0)) {
    obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    return;
  }
  if (imodDebug('v'))
    imodPrintStderr("draw mesh lines %s\n", drawTrans ? "trans" : "solid");

  stateFlags = 0;
  nextItemIndex = nextChange = istoreFirstChangeIndex(mesh->store);

  for (i  = 0; i < mesh->lsize; i++) {
    switch(mesh->list[i]){
    case IMOD_MESH_BGNPOLY:
    case IMOD_MESH_BGNBIGPOLY:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      glBegin(polyStyle);
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
        glVertex3fv( (float *)&(mesh->vert[mesh->list[i]]));
      }
      glEnd();
      break;

    case IMOD_MESH_BGNPOLYNORM:
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      i++;
      while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
        glBegin(normStyle);

        glVertex3fv( (float *)&(mesh->vert[mesh->list[++i]]));
        i+=2;
        glVertex3fv( (float *)&(mesh->vert[mesh->list[i]]));
        i+=2;
        glVertex3fv( (float *)&(mesh->vert[mesh->list[i++]]));
                    
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
        nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, 
                                                       drawTrans, &i);
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
        if (imodDebug('v'))
          imodPrintStderr("Skipped from start to %d\n", i);
      }
      while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
        if (nextChange < i || nextChange > i + 2) {
          glBegin(normStyle);

          // Why oh why does this not require Z scaling?
          glVertex3fv( (float *)&(mesh->vert[mesh->list[i++]]));
          glVertex3fv( (float *)&(mesh->vert[mesh->list[i++]]));
          glVertex3fv( (float *)&(mesh->vert[mesh->list[i++]]));
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
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans,
                                                          drawTrans, &i);
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
              glVertex3fv( (float *)&(mesh->vert[mesh->list[i++]]));
              glEnd();
            }

          } else {

            glBegin(GL_LINE_STRIP);
            firstPt = (float *)&(mesh->vert[mesh->list[i++]]);
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
              glVertex3fv( (float *)&(mesh->vert[mesh->list[i++]]));
              if (changeFlags & CHANGED_3DWIDTH) {
                glEnd();
                glLineWidth((GLfloat)curProps.linewidth);
                glBegin(GL_LINE_STRIP);
                glVertex3fv( (float *)&(mesh->vert[mesh->list[i - 1]]));
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
                             mesh->list[i] == IMOD_MESH_ENDPOLY)) {
            nextChange = ifgHandleMeshChange
              (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
               &stateFlags, &changeFlags, handleFlags);
            if (mesh->list[i] != IMOD_MESH_ENDPOLY && 
                (curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans,
                                                          drawTrans, &i);
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
      if ((mesh->list[i] < mesh->vsize) && (mesh->list[i] > -1))
        glVertex3fv( (float *)&(mesh->vert[mesh->list[i]])  );
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
  int    *list;
  int nextChange, stateFlags, changeFlags, nextItemIndex, defTrans;
  int handleFlags = ((obj->flags & IMOD_OBJFLAG_FCOLOR) ? HANDLE_MESH_FCOLOR :
    HANDLE_MESH_COLOR) | HANDLE_TRANS;

  if (!mesh)
    return;
  if (!mesh->lsize)
    return;

  if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
    handleFlags |= HANDLE_VALUE1;

  /* Check to see if normals have magnitudes. */
  if (mesh->flag & IMESH_FLAG_NMAG)
    glDisable( GL_NORMALIZE );
  else
    glEnable( GL_NORMALIZE );

  vert = mesh->vert;
  list = mesh->list;

  ifgHandleSurfChange(obj, mesh->surf, &defProps, &curProps, &stateFlags,
                      handleFlags);
  defTrans = defProps.trans ? 1 : 0;

  // First time in, if the trans state does not match the draw state, and the 
  // storage list does not have a change to a matching state, return
  if (!drawTrans && defTrans &&
      !istoreTransStateMatches(mesh->store, 0)) {
    obj->flags |= IMOD_OBJFLAG_TEMPUSE;
    return;
  }
  if (imodDebug('v'))
    imodPrintStderr("draw mesh %s\n", drawTrans ? "trans" : "solid");

  stateFlags = 0;
  nextItemIndex = nextChange = istoreFirstChangeIndex(mesh->store);

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

      /* 6/19/01 note: using glVertex3fv with no z scaling increases
         speed by 5% on PC, 0.2% on SGI */
      if (skipNonCurrentSurface(mesh, &i, obj))
        break;
      glBegin(GL_TRIANGLES);
      i++;
      while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
        unsigned int li;
        glNormal3fv((float *)&(vert[list[i++]])); 
        li = list[i++];
        glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
                   

        glNormal3fv((float *)&(vert[list[i++]]));
        li = list[i++];
        glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);

        glNormal3fv((float *)&(vert[list[i++]]));
        li = list[i++];
        glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
        if ((i%512) == 0)
          glFinish();
      }
      glEnd();
      glFlush();
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
        nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans, 
                                                       drawTrans, &i);
        obj->flags |= IMOD_OBJFLAG_TEMPUSE;
        if (imodDebug('v'))
          imodPrintStderr("Skipped from start to %d\n", i);
      }

      while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
        unsigned int li;
        if (nextChange < i || nextChange > i + 2) {
          //imodPrintStderr("same %d  nextChange %d\n", i, nextChange);

          li = list[i++];
          glNormal3fv((float *)&(vert[li + 1])); 
          glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
          
          li = list[i++];
          glNormal3fv((float *)&(vert[li + 1])); 
          glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
          
          li = list[i++];
          glNormal3fv((float *)&(vert[li + 1])); 
          glVertex3f(vert[li].x, vert[li].y, vert[li].z * z);
        } else {

          // Isolate a triangle with changes from other triangles with an
          // End/begin pair regardless of type of change - needed on non-quadro
          // Nvidia in Linux, and Nvidia on a Mac
          glEnd();

          // Get the next change for the first point
          if (stateFlags || i == nextChange) {
            nextChange = ifgHandleMeshChange
              (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
               &stateFlags, &changeFlags, handleFlags);

            // If trans state does not match draw state, return to default
            // and skip to next matching triangle
            if ((curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              if (stateFlags)
                ifgHandleMeshChange(obj, mesh->store, &defProps, &curProps, 
                                    &nextItemIndex, 0, &stateFlags,
                                    &changeFlags, handleFlags);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans,
                                                          drawTrans, &i);
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
              nextChange = ifgHandleMeshChange
                (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
               &stateFlags, &changeFlags, handleFlags);
            li = list[i++];
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
                             list[i] == IMOD_MESH_ENDPOLY)) {
            /*imodPrintStderr("resetting, state %d\n", stateFlags); */
            nextChange = ifgHandleMeshChange
              (obj, mesh->store, &defProps, &curProps, &nextItemIndex, i,
               &stateFlags, &changeFlags, handleFlags);
            if (list[i] != IMOD_MESH_ENDPOLY && 
                (curProps.trans ? 1 : 0) != drawTrans) {
              if (imodDebug('v'))
                imodPrintStderr("Drew to %d\n", i);
              nextChange = nextItemIndex = ifgMeshTransMatch(mesh, defTrans,
                                                          drawTrans, &i);
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
      glFlush();
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
    glFlush();
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

  GLenum polyStyle, normStyle;

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
    normStyle = GL_TRIANGLES;
  }else if (iobjLine(obj->flags)){
    polyStyle = GL_LINE_STRIP;
    normStyle = GL_LINE_LOOP;
    zscale = 1.0;
  }else{
    polyStyle = GL_POINTS;
    normStyle = GL_POINTS;
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
  if (!(Imodv->current_subset / 2 == 1 && cursurf >= 0) || mesh->surf > 0)
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
      if (!(cont->flags & ICONT_WILD) && zz != cont->pts->z)
        continue;
      if (cont->surf != cursurf)
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
  int nextChange, stateFlags, handleFlags = 0;
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

  CTime = imod->ctime;
  if (!CTime)
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
    if (!checkContourDraw(cont, co, checkTime))
      continue;
    //imodPuts("passed draw check");
    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
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

/*

$Log$
Revision 4.45  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 4.44  2008/11/28 06:45:05  mast
Modify current point object at right point for it to be drawn

Revision 4.43  2008/10/02 22:46:41  mast
Change scaling for HW stereo only for SGI

Revision 4.42  2008/09/19 20:18:52  mast
Define GLU_CALLBACK to Linux/OSX 10.5 version if not defined in config file

Revision 4.41  2008/06/17 20:18:21  mast
Added ability to skip sphere drawing when drawing mesh of non-scattered obj

Revision 4.40  2008/06/10 05:56:29  mast
Changes for light drawing and making sure light is set for current model

Revision 4.39  2008/05/29 22:20:55  mast
When doing picking, put out vertices of mesh if no contours in object

Revision 4.38  2008/05/22 15:40:21  mast
Get object for clip plane, skip spheres if no contours

Revision 4.37  2008/04/01 23:43:16  mast
Fixed transparency when run from 3dmod, made it draw mesh if no contours,
and added flag for drawing extra objects only.

Revision 4.36  2008/03/05 20:06:45  mast
Added ability to draw stippled contours and extra objects

Revision 4.35  2007/11/30 06:51:50  mast
Changes for linking slicer to model view

Revision 4.34  2007/09/20 22:06:55  mast
Changes for visualizing clipping plane

Revision 4.33  2006/11/03 06:43:21  mast
Fixed problems with missing bits of contour line or point draw due to color
changes- needed to do glEnd/glBegin pairs

Revision 4.32  2006/09/12 15:44:07  mast
Handled mesh member renames

Revision 4.31  2006/09/01 20:47:51  mast
Fixed drawing of selected contours so it works with multiple objects

Revision 4.30  2006/08/31 23:27:45  mast
Changes for stored value display

Revision 4.29  2006/08/28 05:23:15  mast
Switched to using false color routines for getting false color map

Revision 4.28  2006/06/09 20:25:01  mast
Added glEnd/glBegin pairs to fix some gaps in mesh with color changes

Revision 4.27  2005/12/11 18:26:29  mast
Fixed perspective so the slider really does set the angular field of view
regardless of clipping plane setting, and changed kicking out of extreme
clipping planes so it is less by default and canbe increased by choice.

Revision 4.26  2005/11/19 16:59:25  mast
Fix fmod calls to use two floats for Intel compiler

Revision 4.25  2005/10/21 23:58:08  mast
Provide alternate define of GLU_CALLBACK for Mac for gcc 4

Revision 4.24  2005/10/13 20:11:59  mast
Handled model display correctly when data are read in binned, without
requiring a change in user-specified Z-scale.

Revision 4.23  2005/09/22 15:11:18  mast
Had to end and restart triangles for mesh triangles in Linux

Revision 4.22  2005/09/12 14:24:42  mast
Fixed problem with ending and starting triangles in mesh draw

Revision 4.21  2005/09/11 19:54:53  mast
Implemented drawing of fine-grained properties from new mesh structure

Revision 4.20  2005/06/26 19:38:36  mast
Added logic for fine-grained changes to line and sphere drawing

Revision 4.19  2005/06/20 22:20:29  mast
Pass transparency to light_adjust

Revision 4.18  2005/06/06 17:26:23  mast
Draw spheres with fillcolor if separate new flag is set

Revision 4.17  2004/11/02 16:22:26  mast
Fixed unsigned int mismatch to calling new skip function

Revision 4.16  2004/11/01 23:33:28  mast
Made current surface subset work with mesh dsplay

Revision 4.15  2004/09/21 20:14:51  mast
Implemented multiple clipping planes

Revision 4.14  2004/09/10 02:31:03  mast
replaced long with int

Revision 4.13  2004/06/10 00:33:28  mast
Disabled clipping planes, more or less, at 0 and 1000

Revision 4.12  2004/06/08 15:41:24  mast
Changes to get top/bottom stereo to work with monitor passthroughs

Revision 4.11  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.10  2004/05/16 20:19:35  mast
Switched to drawing solid image data before and transparent image data
after the model data; moved stereo setup of matrices to another routine;
fixed size computation on empty model so depth cure will work on image

Revision 4.9  2004/05/15 21:34:45  mast
Switched to drawing all image data after the model draw

Revision 4.8  2004/04/28 14:49:04  mast
Changed to be able to draw current contour thicker

Revision 4.7  2004/01/05 18:36:53  mast
Divide point sizes by binning if not in standalone mode

Revision 4.6  2003/06/27 19:31:00  mast
Changed to using object and global point quality flags

Revision 4.5  2003/04/17 19:02:59  mast
adding hack for GL-context dependent gluQuadric

Revision 4.4  2003/03/28 05:01:39  mast
Needed to remove include of glu.h for Mac

Revision 4.3  2003/03/03 22:15:08  mast
Added ability to display spheres for any points with size

Revision 4.2  2003/02/27 17:30:03  mast
Had to include qgl.h instead of GL/gl.h under windows

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.7  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.6  2002/12/30 06:44:21  mast
change position of imodv_image include

Revision 1.1.2.5  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.4  2002/12/17 22:28:21  mast
cleanup of unused variables and SGI errors

Revision 1.1.2.3  2002/12/17 19:25:56  mast
fixing braces after removing color index code

Revision 1.1.2.2  2002/12/17 17:44:18  mast
changes for Qt version

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.3  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.2  2002/09/03 19:36:05  mast
Eliminated initialized of scalar mesh whitelevel value because endian
problem is now fixed

Revision 3.1  2001/12/05 15:42:31  mast
Fixed problem with transparency when lighting both sides

*/

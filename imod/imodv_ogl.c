/*  IMOD VERSION 2.50
 *
 *  imodv_ogl.c -- OpenGL Drawing functions for imodv. No X code here!
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

    $Log$
*/

#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <imodel.h>
#include "imod.h"
#include "imodv.h"

#define DRAW_POINTS 1
#define DRAW_LINES  2
#define DRAW_FILL   3
#define DRAW_OBJECT -1

static int CTime = -1;

void imodvDraw_models(ImodvApp *a);
void imodvDraw_spheres(Iobj *obj, double zscale, int style);
void imodvDraw_filled_spheres(Iobj *obj, double zscale);
void imodvDraw_mesh(Imesh *mesh, int style);
void imodvDraw_filled_mesh(Imesh *mesh, double zscale);
void imodvDrawScalarMesh(Imesh *mesh, double zscale, Iobj *obj);
void imodvDrawImage(ImodvApp *a);
void imodvPick_Contours(Iobj *obj);
void light_adjust(Iobj *obj, float r, float g, float b);
void imodvSetDepthCue(Imod *imod);

static float depthShift;
static int cursurf, curcont;
static int curTessObj;

void imodvSetViewbyModel(ImodvApp *a, Imod *imod)
{
     Iview *vw;
     GLint vp[4];
     double ndist, cdist;
     double xs, ys;       /* window scaling factors. */
     double zn, zf;
     double znear, zfar;  /* clip in z. */
     double fovytan;
     double wedge;
     double rad;
     float scale = 1.0f;
     if (!a->imod) return;

     imodv_winset(a);
     vw = imod->view;
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();

     if (a->doPick){
	  glGetIntegerv(GL_VIEWPORT, vp);
	  gluPickMatrix(a->xPick, a->yPick, a->wPick, a->hPick, vp);
     }
     
     rad = vw->rad;
     if (rad < 0.0f)
	 rad *= -1.0f;

     fovytan = tan((((double)vw->fovy * 0.5) * 0.0087266463));
     rad /= (1.0 + 3.1415927*fovytan);

     /* check for window area */
     if ((!Imodv->winx) || (!Imodv->winy)) return;
     xs = (double)a->winx;
     ys = (double)a->winy;
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

     if (vw->fovy < 1.0f){
	  cdist = zfar - znear;
	  znear += cdist * vw->cnear;
	  zfar  -= cdist * (1.0 - vw->cfar);

	  glOrtho(-xs, xs, -ys, ys,
		  znear + depthShift , zfar + depthShift);
	  return;

     }
     
     ndist = fovytan * znear;

     if (fovytan)
	  cdist = rad/fovytan;
     else
	  cdist = rad;
     

     zn = znear + cdist;
     zf = zfar + cdist;
     {
	 float cres = zf - zn;
/*
	 printf("clipnf %g %g : sliders %g %g\n",
		zn, zf, vw->cnear, vw->cfar);
*/ 
	 zn += (vw->cnear * cres);
	 zf -= (1.0 - vw->cfar) * cres;

     }
     

     glFrustum(-xs, xs, -ys, ys, zn, zf); 

/*
     printf("frustum xy = %g %g : znf = %g %g : cd = %g\n",
	    xs, ys, zn, zf, cdist);
*/

     glTranslatef(0.0, 0.0, (float) (-cdist + depthShift));
     return;
}

void imodvSetDepthCue(Imod *imod)
{
   Iview *vw = imod->view;
   Ipoint maxp, minp;
   float bgcolor[4];
   int depthRange[3];
   float fstart, fend;
   float drange;
 
   depthShift = 0.0f;

   if (!(vw->world & VIEW_WORLD_DEPTH_CUE)){
       glDisable(GL_FOG);
       return;
   }
   
   /* Don't bother doing depth cue in colorindex mode. */
   if (Imodv->cindex) return;

   /* We want drawn colors to blend into the background
    * so adjust fog color to the current background clear color.
    */
   glGetFloatv(GL_COLOR_CLEAR_VALUE, bgcolor);
   glFogfv(GL_FOG_COLOR, bgcolor);

   imodGetBoundingBox(imod, &minp, &maxp);
   maxp.z *= imod->zscale;
   minp.z *= imod->zscale;
   drange = (maxp.x - minp.x) * (maxp.x - minp.x) +
            (maxp.y - minp.y) * (maxp.y - minp.y) +
            (maxp.z - minp.z) * (maxp.z - minp.z);
   drange = (float)sqrt((double)drange);

   depthShift = 0.6 * drange;
   
   fstart = 1.2 * drange * vw->dcstart ;
   fend = 1.2 * drange * vw->dcend ;
   
   /*   fprintf(stderr, "OGL Depth Cue start: %g       end: %g   range: %g\n",
	fstart, fend, drange); */

   glFogi(GL_FOG_MODE, GL_LINEAR);
   glFogf(GL_FOG_DENSITY, 1.0);
   glFogf(GL_FOG_START, fstart);
   glFogf(GL_FOG_END, fend);
   glEnable(GL_FOG);

   return;
}

void imodvSetModelTrans(Imod *imod)
{
     Iview *vw;
     int useTransMatrix = 0;
     if (!imod) return; 

     vw = imod->view;
     glMatrixMode(GL_MODELVIEW);

     glLoadIdentity();

     /* DNM: The last operation is a translation to get positive Z's if doing
	depth cueing */

     glTranslatef(0.0f, 0.0f, -depthShift);

     vw->rot.x = fmod(vw->rot.x, 360.0);
     vw->rot.y = fmod(vw->rot.y, 360.0);
     vw->rot.z = fmod(vw->rot.z, 360.0);

     glRotatef(vw->rot.x, 1.0f, 0.0f, 0.0f);
     glRotatef(vw->rot.y, 0.0f, 1.0f, 0.0f); 
     glRotatef(vw->rot.z, 0.0f, 0.0f, 1.0f); 
     
     glTranslatef(vw->trans.x, vw->trans.y, (vw->trans.z * imod->zscale));
     glScalef(vw->scale.x, vw->scale.y, vw->scale.z * imod->zscale);

     return;
}



void imodvDraw_models(ImodvApp *a)
{
     int m;

     if (!a->standalone){
          if (!a->texTrans) {
	       imodvSetViewbyModel(a, a->imod);
	       imodvDrawImage(a);
	  }
     }

     glPushName(-1);
     
     switch (a->drawall){
	case 0:
	  glLoadName(a->cm);
	  imodvDraw_model(a, a->imod);
	  break;
	case 2:
	  imodvDraw_model(a, a->imod);
	  m = a->cm + 1;
	  if (m < a->nm){
	       glLoadName(m);
	       imodvDraw_model(a, a->mod[m]);
	  }
	  break;

	case 1:
	  imodvDraw_model(a, a->imod);
	  m = a->cm - 1;
	  if (m >= 0){
	       glLoadName(m);
	       imodvDraw_model(a, a->mod[m]);
	  }
	  break;

	case 3:
	  for (m = 0; m < a->nm; m++){
	       glLoadName(m);
	       imodvDraw_model(a, a->mod[m]);
	  }
	  break;
     }

     if (!a->standalone){
          if (a->texTrans > 0) {
	       imodvSetViewbyModel(a, a->imod);
	       imodvDrawImage(a);
	  }
     }
     
     glPopName();
     return;
}

static void set_curcontsurf(int ob, Imod* imod)
{
     curcont = -1;
     cursurf = -1;
     if (Imodv->current_subset && imod->cindex.object == ob) {
	  curcont = imod->cindex.contour;
	  if (curcont >= 0)
	       cursurf = imod->obj[imod->cindex.object].cont[curcont].surf;
     }
}

/*
 *  Draw this model.
 */
void imodvDraw_model(ImodvApp *a, Imod *imod)
{
     int ob = -1;
     Iobj *obj;
     int trans = FALSE;
     float ftrans;
     int obstart, obend;

     if (imod == NULL)
	  return;

     if (imod->obj == NULL)
	  return;

     /* DNM: move the call to "SetView" into the model loop in order to have
	it handle all models independently; this requires the additional
	steps for stereo display to happen inside thsi routine */

     imodvSetViewbyModel(a, imod);

     imodvSetModelTrans(imod);

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
	  glViewport(0, 0, a->winx, a->winy);
	  glTranslatef(a->winx/2, a->winy/2, 0.0f);
	  glRotatef(a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
	  glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
	  break;

	case IMODV_STEREO_TB:
	  glTranslatef(a->winx/2, a->winy/2, 0.0f);
	  glRotatef(-a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
	  glTranslatef(-a->winx/2, -a->winy/2, 0.0f);
	  glViewport(0, a->winy, a->winx, a->winy);
	  break;

	case -IMODV_STEREO_HW:
	  glRotatef(-a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
	  /* DNM: cut these values in half to get model at same zoom relative
	     to the size of the window */
	  glScalef(1.0f, 0.5f, 0.5f);
	  break;

	case IMODV_STEREO_HW:
	  glRotatef(a->plax * 0.5f, 0.0f, 1.0f, 0.0f);
	  glScalef(1.0f, 0.5f, 0.5f);
	  break;

     }


     CTime = imod->ctime;
     glPushName(ob);

     /* If displaying a current subset, set up object limits */
     obstart = 0;
     obend = imod->objsize;
     if ((Imodv->current_subset == 1 || Imodv->current_subset == 2 || 
	  Imodv->current_subset == 4 )&& imod->cindex.object >= 0) {
	  obstart = imod->cindex.object;
	  obend = obstart + 1;
     }

     if (Imodv->cindex){
	  imodvMapModel(Imodv, imod);
	  for (ob = obstart; ob < obend; ob++){
	       set_curcontsurf(ob, imod);
	       obj = &(imod->obj[ob]);
	       glLoadName(ob);
	       clip_obj(obj, True, imod->zscale, Imodv->md->zoom);
	       glIndexi(Imodv->cstart + (ob * Imodv->cstep));
	       /* glIndexi(obj->fgcolor); */
	       curTessObj = ob;
	       imodvDraw_object( obj , imod);
	       clip_obj(obj, False, imod->zscale, Imodv->md->zoom);
	       glFinish();
	  }
     }else{
	  /* DNM: draw objects without transparency first; then make the depth
	     buffer read-only and draw objects with transparency. Anti-aliased
	     lines seem to do well enough being drawn in the first round, so
	     don't try to hold those until the second round */
	  for (ob = obstart; ob < obend; ob++){
	       set_curcontsurf(ob, imod);
	       obj = &(imod->obj[ob]);
	       glLoadName(ob);
	       curTessObj = ob;
	       if (obj->trans == 0){
		    clip_obj(obj, True, imod->zscale, Imodv->md->zoom);
		    imodvDraw_object( obj , imod);
		    clip_obj(obj, False, imod->zscale, Imodv->md->zoom);
		    glFinish();
	       }
	  }

	  for (ob = obstart; ob < obend; ob++){
	       set_curcontsurf(ob, imod);
	       glLoadName(ob);
	       curTessObj = ob;
	       obj = &(imod->obj[ob]);
	       trans = obj->trans;
	       if (obj->trans > 0){
		    glDepthMask(GL_FALSE); 
		    clip_obj(obj, True, imod->zscale, Imodv->md->zoom);
		    imodvDraw_object( obj , imod);
		    clip_obj(obj, False, imod->zscale, Imodv->md->zoom);
		    glDepthMask(GL_TRUE); 
		    glFinish();
	       }
	  }
     }
     
     glPopName();
     return;
}

/* In which any mode enabled by imodvSetObject should be disabled */
void imodvUnsetObject(Iobj *obj)
{
     light_off();
     glDisable(GL_BLEND);
     glDisable(GL_LINE_SMOOTH);
     glDisable(GL_CULL_FACE);
     return;
}

void imodvSetObject(Iobj *obj, int style)
{
     float red, green, blue, trans;
     unsigned char *ub;
     
     trans = 1.0f - (obj->trans * 0.01f);

     switch(style){
	case 0:
	  imodvUnsetObject(obj);
	  /* DNM 11/30/01: need to return, not break */
	  return;

	case DRAW_POINTS:
	  glPointSize(obj->linewidth);
	case DRAW_LINES:
	  if (IMOD_OBJFLAG_ANTI_ALIAS & obj->flags){
	       glEnable(GL_LINE_SMOOTH);
	       if (trans == 1.0f){
		    trans = 0.99f;
	       }
	  }

	  glLineWidth((float)obj->linewidth);
	  glColor4f(obj->red, obj->green, obj->blue,
		    (1.0f - (obj->trans * 0.01f)));
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
	       ub = ( unsigned char * )&(obj->mat1);
	       red   = (float)ub[0] / 255.0f;
	       green = (float)ub[1] / 255.0f;
	       blue  = (float)ub[2] / 255.0f;
	       glColor4f(red, green, blue, trans);
	  }else{
	       glColor4f(obj->red, obj->green, obj->blue, trans);
	  }
	  if (!Imodv->cindex){
	      /*if (obj->flags & IMOD_OBJFLAG_LIGHT){*/
	      if ((Imodv->lighting) && (!Imodv->wireframe)){
		    light_on(obj);
	       }else{
		    light_off();
	       }
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
     if (!Imodv->cindex){
	  if (trans < 1.0f){
	       glEnable(GL_BLEND);
	       glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	       if (!(obj->flags & IMOD_OBJFLAG_TWO_SIDE))
		   glEnable(GL_CULL_FACE);
	  }
     }

     return;
}

/* A simple function to check whether the current mesh should be drawn, based
 on time, surface number, and resolution */
static int check_mesh_draw(Imesh *mesh, int checkTime, int resol)
{
     if ((checkTime) && (mesh->type) && (mesh->type != CTime))
	  return 0;
     if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && 
	 mesh->pad != cursurf)
	  return 0;
     if (imeshResol(mesh->flag) == resol)
	  return 1;
     return 0;
}

void imodvDraw_object(Iobj *obj, Imod *imod)
{
     int co, resol;
     Icont *cont;
     double zscale;
     Imesh *mesh;
     int checkTime = (int)iobjTime(obj->flags);
     if (!CTime)
	  checkTime = 0;

     if (!obj)
	  return;

     if (iobjOff(obj->flags))
	  return;

     if (!obj->contsize)
	  return;

     /* DNM: Made sure there was an "imodvSetObject(obj, 0)" before each
	return from this, and before any repeated call to imodvSetObject.
	The IMOD_OBJFLAG_LINE has an inverted sense (off
	if line or fill outline is selected, so switched to using iobjLine
	instead of testing this flag to avoid confusion */

     if (iobjScat(obj->flags)){
          /* DNM: has to be the model being drawn, not the current one */
	  zscale = imod->zscale;
	  /* scattered points: if they are filled, draw as fill; then draw the
	     lines on top if "Fill outline" is selected */
	  if (iobjFill(obj->flags)){
	       imodvSetObject(obj, DRAW_FILL);
	       imodvDraw_spheres(obj, zscale, DRAW_FILL );
	       if (iobjLine(obj->flags)){
		    imodvSetObject(obj, 0);
		    imodvSetObject(obj, DRAW_LINES);
		    imodvDraw_spheres(obj, zscale, DRAW_LINES );
	       }
	  }else{
	       /* or, just draw the lines if that is selected; otherwise draw
		  points */
	       if (iobjLine(obj->flags)){
		    imodvSetObject(obj, DRAW_LINES);
		    imodvDraw_spheres(obj, zscale, DRAW_LINES );
	       }else{
		    imodvSetObject(obj, DRAW_POINTS);
		    imodvDraw_spheres(obj, zscale, DRAW_POINTS );
	       }
	  }
	  imodvSetObject(obj, 0);
	  return;
     }

     if (Imodv->doPick){
	  imodvPick_Contours(obj);
	  imodvSetObject(obj, 0);
	  return;
     }

     /* Open contours displayed as contours: draw lines if "lines" or "Fill
	Outline" selected, otherwise draw points.  This is redundant to code
	below and could be handled there */
     /*     if ((!iobjClose(obj->flags)) && (!iobjMesh(obj->flags))){
	  if (iobjLine(obj->flags)){
	       imodvSetObject(obj, DRAW_LINES);
	       imodvDraw_contours(obj, GL_LINE_STRIP );
	  }
	  else{
	       imodvSetObject(obj, DRAW_POINTS);
	       imodvDraw_contours(obj, GL_POINTS);
	  }
	  imodvSetObject(obj, 0);
	  return;
	  } */
     

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
	       glScalef(1.0f, 1.0f, 1.0f/imod->zscale);
	       imodvSetObject(obj, DRAW_FILL);
	       for (co = 0; co < obj->meshsize; co++){
		    mesh = &(obj->mesh[co]);
		    if (check_mesh_draw(mesh, checkTime, resol)) {
			 if (obj->flags & IMOD_OBJFLAG_SCALAR)
			      imodvDrawScalarMesh (mesh, imod->zscale, obj);
			 else
			      imodvDraw_filled_mesh (mesh, imod->zscale);
		    }
	       }
	       glPopMatrix();
	       
	       /* Fill outline: draw the mesh lines as well */
	       if (iobjLine(obj->flags)){
		    imodvSetObject(obj, 0);
		    imodvSetObject(obj, DRAW_LINES);
		    for(co = 0; co < obj->meshsize; co++) {
			 mesh = &(obj->mesh[co]);
			 if (check_mesh_draw(mesh, checkTime, resol))
			      imodvDraw_mesh(mesh, DRAW_LINES);
		    }
		    /*  imodvSetObject(obj, 0); */
	       }
	  }else{
	       /* Mesh Lines: draw lines in scalar or regular mode */
	       if (iobjLine(obj->flags)){
		    imodvSetObject(obj, DRAW_LINES);
		    for(co = 0; co < obj->meshsize; co++){
			 mesh = &(obj->mesh[co]);
			 if (check_mesh_draw(mesh, checkTime, resol)) {
			      if (obj->flags & IMOD_OBJFLAG_SCALAR){
				   imodvDrawScalarMesh 
					(mesh, imod->zscale, obj);
			      }else{
				   imodvDraw_mesh(mesh, DRAW_LINES);
			      }
			 }
		    }
	       }else{
		    /* Mesh Points: draw points in scalar or regular mode */
		    imodvSetObject(obj, DRAW_POINTS);
		    for(co = 0; co < obj->meshsize; co++) {
			 mesh = &(obj->mesh[co]);
			 if (check_mesh_draw(mesh, checkTime, resol)) {
			      if (obj->flags & IMOD_OBJFLAG_SCALAR){
				   imodvDrawScalarMesh
					(mesh, imod->zscale, obj);
			      }else{
				   /*  if (iobjLine(obj->flags))
					imodvDraw_mesh(&(obj->mesh[co]),
					DRAW_LINES);
					else */
				   imodvDraw_mesh(mesh, DRAW_POINTS);
			      }
			 }
		    }
	       }
	  }
	  imodvSetObject(obj, 0);
	  return;
     }

     /* Closed contours with Fill: draw the fill, then draw the outside lines
	if Fill Outline is selected */
     if ((iobjClose(obj->flags)) && (iobjFill(obj->flags))){
	  imodvSetObject(obj, DRAW_FILL);
	  /* We have to either turn off the light or set the normal to
	     something, to have display be independent of the state of the
	     last object. Lighting makes it look to bright */
	  light_off(); 
	  /* glNormal3f(0., 0., 1.); */
	  imodvDraw_filled_contours(obj);
	  if (iobjLine(obj->flags)){
	       imodvSetObject(obj, 0);
	       imodvSetObject(obj, DRAW_LINES);
	       imodvDraw_contours(obj, GL_LINE_LOOP);
	  }
     }else{
	  /* Contours as lines or points; draw as open or closed lines */
	  if (iobjLine(obj->flags)){
	       imodvSetObject(obj, DRAW_LINES);
	       if (iobjClose(obj->flags)){
		    imodvDraw_contours(obj, GL_LINE_LOOP );
	       }else{
		    imodvDraw_contours(obj, GL_LINE_STRIP );
	       }
	  }else{
	       imodvSetObject(obj, DRAW_POINTS);
	       imodvDraw_contours(obj, GL_POINTS );
	  }
     }

     imodvSetObject(obj, 0);
     return;
}


/****************************************************************************/
/* DRAW CONTOURS 
 */
#define PICKPOINTS
void imodvPick_Contours(Iobj *obj)
{
     int co, pt, npt;
     Icont *cont;
     int pmode = GL_POINTS;
     int doLines = False;
     int checkTime = (int)iobjTime(obj->flags);
     if (!CTime)
	  checkTime = 0;

#ifdef PICKPOINTS
     /* pick points first */
     glPushName(-1);
     for(co = 0; co < obj->contsize; co++){
	  cont = &(obj->cont[co]);
	  if (!cont->psize) continue;
	  if ((checkTime) && (cont->type) && (cont->type != CTime))
	       continue;
	  if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && 
	      cont->surf != cursurf)
	       continue;

	  if (Imodv->current_subset / 2 == 2 && curcont >= 0 && co != curcont)
	       continue;

	  glLoadName(co);
	  glPushName(-1);
	  for(pt = 0; pt < cont->psize; pt++){
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
	  doLines = True;
     }

     glPushName(-1);
     for(co = 0; co < obj->contsize; co++){
	  cont = &(obj->cont[co]);
	  if (!cont->psize) continue;
	  if ((checkTime) && (cont->type) && (cont->type != CTime))
	       continue;
	  
	  if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && 
	      cont->surf != cursurf)
	       continue;

	  if (Imodv->current_subset / 2 == 2 && curcont >= 0 && co != curcont)
	       continue;

	  glLoadName(co);
	  
	  glPushName(-1);
	  for(pt = 0; pt < cont->psize; pt++){
	       if (doLines){
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
	       if (doLines){
		    glVertex3fv((GLfloat *)&(cont->pts[npt]));
	       }
	       glEnd();
	  }
	  glPopName();
     }
     glPopName();
}

void imodvDraw_contours(Iobj *obj, int mode)
{
     int co, pt;
     Icont *cont;
     int checkTime = (int)iobjTime(obj->flags);
     if (!CTime)
	  checkTime = 0;

     for(co = 0; co < obj->contsize; co++){
	  cont = &(obj->cont[co]);
	  if (!cont->psize) continue;
	  if ((checkTime) && (cont->type) && (cont->type != CTime))
	       continue;

	  if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && 
	      cont->surf != cursurf)
	       continue;

	  if (Imodv->current_subset / 2 == 2 && curcont >= 0 && co != curcont)
	       continue;
#ifdef LINE_LOOP_HACK
	  if (mode == GL_LINE_LOOP)
	       glBegin(GL_LINE_STRIP);
#else
	  if ((mode == GL_LINE_LOOP) && (cont->flags & ICONT_OPEN))
	       glBegin(GL_LINE_STRIP);
#endif
	  else
	       glBegin(mode);

	  for(pt = 0; pt < cont->psize; pt++){
	       glVertex3fv((GLfloat *)&(cont->pts[pt]));
	  }
#ifdef LINE_LOOP_HACK
	  if ((mode == GL_LINE_LOOP) && !(cont->flags & ICONT_OPEN))
	       glVertex3fv((GLfloat *)cont->pts);
#endif
	  glEnd();
     }
     return;
}  



static int tesserror;

static void myerror(GLenum error)
{
     tesserror = error;
     /*
     fprintf(stderr, "gluError %d: %s\n", error,
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
	  printf ("This contour has over %d crossings - fix it!\n", 
		  MAX_CROSSES);
	  *dataOut = &(newPoints[0]);
	  return;
     }
     if (!newCount)
	  printf("Warning: contour %d of object %d crosses itself; this may "
		 "cause crashes.\n", curTessCont + 1, curTessObj + 1);
     newPoints[newCount].x=coords[0];
     newPoints[newCount].y=coords[1];
     newPoints[newCount].z=coords[2];
     *dataOut = &(newPoints[newCount++]);
}
#endif

void imodvDraw_filled_contours(Iobj *obj)
{
     static GLUtriangulatorObj *tobj = NULL;
     GLdouble v[3];
     Icont   *cont;
     int      co, pt;
     int psize;
     GLfloat color[4];
     int ptstr, ptend;
     Ipoint *pts;
     int checkTime = (int)iobjTime(obj->flags);
     if (!CTime)
	  checkTime = 0;

     tesserror = 0;

     if (!obj->contsize)
	  return;

     if (!tobj){
	  tobj = gluNewTess();
	  gluTessCallback(tobj, GLU_BEGIN, glBegin);
	  gluTessCallback(tobj, GLU_VERTEX, glVertex3fv);
	  gluTessCallback(tobj, GLU_END, glEnd);
/*	  gluTessCallback(tobj, GLU_EDGE_FLAG, glEdgeFlag); */
#ifdef TESS_HACK
	  gluTessCallback(tobj, GLU_TESS_COMBINE, myCombine);
#endif
	  gluTessCallback(tobj, GLU_ERROR, myerror);
     }

     glPushName(-1);
     for(co = 0; co < obj->contsize ; co++){
	  glLoadName(co);
	  cont = &(obj->cont[co]);
	  if ((checkTime) && (cont->type) && (cont->type != CTime))
	       continue;
	  tesserror = 0;
	  ptend = cont->psize;
	  ptstr = 0;
	  pts = cont->pts;
	  curTessCont = co;
	  /* printf(".%d-%d", co, ptend);
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
0, 1, 2, 2, 2,
0, 1, 2, 3, 4,
0, 1, 2, 4, 6,
0, 2, 4, 6, 8,
0, 2, 5, 8, 10,
0, 2, 6, 8, 10,
0, 2, 8, 10, 12,
0, 2, 10, 12, 14,
0, 2, 12, 14, 16
};
static int firstSphere = 1;

/***************************************************************************/
/* Draw point spheres. */
void imodvDraw_spheres(Iobj *obj, double zscale, int style)
{
     int co, pt;
     Icont *cont;
     static GLUquadricObj *qobj = NULL;
     float z = zscale;
     int checkTime = (int)iobjTime(obj->flags);
     float drawsize;
     int pixsize, quality, i, j, k, mink, stepRes;
     float scale, diff, mindiff;
     GLuint listIndex;

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
			      
     quality = Imodv->fastdraw + 1;
     if (Imodv->lowres)
	  quality = 0;
     if (quality >= MAX_QUALITY)
	  quality = MAX_QUALITY - 1;

     scale = 0.5 * (Imodv->winx > Imodv->winy ? Imodv->winy : Imodv->winx) / 
             Imodv->imod->view->rad;

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
     glPushName(-1);

     /* DNM: Get a display list to draw the default size.  Helps a lot on PC,
      only a bit on the SGI */
     drawsize = obj->pdrawsize;
     pixsize = drawsize * scale;
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
	  if (!cont->psize)
	       continue;
	  if ((checkTime) && (cont->type) && (cont->type != CTime))
	       continue;

	  if (Imodv->current_subset / 2 == 1 && cursurf >= 0 && 
	      cont->surf != cursurf)
	       continue;

	  if (Imodv->current_subset / 2 == 2 && curcont >= 0 && co != curcont)
	       continue;

	  glPushName(-1);
	  for(pt = 0; pt < cont->psize; pt++){
	       glLoadName(pt);
	       glPushMatrix();
	       glTranslatef(cont->pts[pt].x, cont->pts[pt].y,
			    cont->pts[pt].z*z);
	       
	       /* get the real point size, convert to number of pixels and
		  look up step size based on current quality */
	       drawsize = imodPointGetSize(obj, cont, pt);
	       if (drawsize == obj->pdrawsize)
		    /* Use the display list if default size */
		    glCallList(listIndex);
	       else {
		    pixsize = drawsize * scale;
		    if (pixsize >= MAX_LOOKUP)
			 pixsize = MAX_LOOKUP - 1;
		    stepRes = sphereRes[pixsize][quality];
		    gluSphere(qobj, drawsize , stepRes * 2, stepRes);
	       }
	       glPopMatrix();
	  }
	  glPopName();
     }
     glDeleteLists(listIndex, 1);
     glPopMatrix();
     glPopName();
     return;
}

/*****************************************************************************/
/*  Draw Mesh Data                                                           */
/*****************************************************************************/

void imodvDraw_mesh(Imesh *mesh, int style)
{
     unsigned long i, lsize;
     int first;
     GLenum polyStyle, normStyle;

     switch(style){
	case DRAW_POINTS:
	  polyStyle = GL_POINTS;
	  normStyle   = GL_POINTS;
	  break;
	case DRAW_LINES:
	  polyStyle = GL_LINE_STRIP;
	  normStyle   = GL_LINE_LOOP;
	  break;
	case DRAW_FILL:
	  polyStyle = GL_POLYGON;
	  normStyle   = GL_TRIANGLES;
	  break;
     }

     if (!mesh)
	  return;
     if (!mesh->lsize)
	  return;
     lsize = mesh->lsize;

     for(i  = 0; i < lsize; i++){
	  switch(mesh->list[i]){
	     case IMOD_MESH_BGNPOLY:
	     case IMOD_MESH_BGNBIGPOLY:
	       glBegin(polyStyle);
	       while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
		    glVertex3fv( (float *)&(mesh->vert[mesh->list[i]]));
	       }
	       glEnd();
	       break;

	     case IMOD_MESH_BGNPOLYNORM:
	       i++;
	       while(mesh->list[i] != IMOD_MESH_ENDPOLY){
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
void imodvDraw_filled_mesh(Imesh *mesh, double zscale)
{
     int i;
     float z = zscale;
     GLUtriangulatorObj *tobj;
     GLdouble v[3];
     Ipoint *vert;
     int    *list;

     if (!mesh)
	  return;
     if (!mesh->lsize)
	  return;

     /* Check to see of normals have magnitudes. */
     if (mesh->flag & IMESH_FLAG_NMAG)
	  glDisable( GL_NORMALIZE );
     else
	  glEnable( GL_NORMALIZE );

     vert = mesh->vert;
     list = mesh->list;

     for(i = 0; i < mesh->lsize; i++){
	  switch(mesh->list[i]){

	     case IMOD_MESH_BGNTRI:
	       glBegin(GL_TRIANGLE_STRIP);
	       break;
	     case IMOD_MESH_ENDTRI:
	       glEnd();
	       break;

	     case IMOD_MESH_BGNPOLY:
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
	       glBegin(GL_TRIANGLES);
	       while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
		   unsigned int li;
		   glNormal3fv((float *)&(vert[list[i++]])); 
		   li = list[i];
		   glVertex3f(vert[li].x,
			      vert[li].y,
			      vert[li].z * z);
		   
		   i++;
		   glNormal3fv((float *)&(vert[list[i++]]));
		   li = list[i];
		   glVertex3f(vert[li].x,
			      vert[li].y,
			      vert[li].z * z);
		   i++;
		   glNormal3fv((float *)&(vert[list[i++]]));
		   li = list[i];
		   glVertex3f(vert[li].x,
			      vert[li].y,
			      vert[li].z * z);
		   if ((i%512) == 0)
		       glFinish();
	       }
	       glEnd();
	       glFlush();
	       break;

	     case IMOD_MESH_BGNBIGPOLY:
	       tobj = gluNewTess();
	       gluTessCallback(tobj, GLU_BEGIN, glBegin);
	       gluTessCallback(tobj, GLU_VERTEX, glVertex3fv);
	       gluTessCallback(tobj, GLU_END, glEnd);
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
	       fprintf(stderr, "imodlib: old mesh\n");
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


static int ImodvRampData[] =
{ 
   15,
  255,    0,   90,    0,
  255,   45,   55,   20,
  255,  105,    0,   83,
  255,  175,    0,  162,
  255,  255,    0,  229,
  239,  255,    0,  240,
  191,  255,    0,  259,
   90,  255,   60,  305,
    0,  207,   78,  361,
    0,  191,  143,  383,
    0,  175,  177,  400,
   60,   96,  255,  469,
  120,   40,  255,  528,
  179,    0,  255,  569,
  255,    0,  255,  616,
};

static int load_cmap(unsigned char table[3][256], int *rampData)
{

     FILE *fin;
     char line[256];
     int nline;
     int *inramp;
     int i,l;
     float tabscl,terpfc,tabpos;
     int indtab;
     nline = *rampData;
     rampData++;
     inramp = (int *)malloc(sizeof(int) * nline * 4);

     for(i = 0, l = 0; i < nline; i++, l+=4){
	 inramp[l] = *rampData; rampData++;
	 inramp[l+1] = *rampData; rampData++;
	 inramp[l+2] = *rampData; rampData++;
	 inramp[l+3] = *rampData; rampData++;
      }

     tabscl = (inramp[(nline * 4) - 1] - inramp[3])/255.0;
     indtab = 0;
     for(i = 0; i < 256; i++){
          tabpos = i * tabscl + inramp[3];
          if (tabpos > inramp[((indtab+1) * 4) + 3]){
               indtab++;
               if (indtab > nline - 2)
                    indtab--;
	   }

          terpfc = (tabpos - inramp[(indtab * 4) + 3])/
               (inramp[((indtab+1) * 4) + 3] - inramp[(indtab * 4) + 3]);

          table[0][i] = (1 - terpfc) * inramp[(indtab * 4)] +
               terpfc * inramp [((indtab+1) * 4)];
          table[1][i] = (1 - terpfc) * inramp[(indtab * 4) + 1] +
               terpfc * inramp [((indtab+1) * 4) + 1];
          table[2][i] = (1 - terpfc) * inramp[(indtab * 4) + 2] +
               terpfc * inramp [((indtab+1) * 4) + 2];

      }
     return(0);
 }

static void mapfalsecolor(int gray, int *red, int *green, int *blue)
{
    static unsigned char cmap[3][256];
    static int first = 1;

    if (first){
	load_cmap(cmap, ImodvRampData);
	first = 0;
    }

    *red = cmap[0][gray];
    *green = cmap[1][gray];
    *blue = cmap[2][gray];
}
	

void imodvDrawScalarMesh(Imesh *mesh, double zscale, 
			       Iobj *obj)
{
     int i;
     float z = zscale;
     float oz = 1.0f/(z*z);
     GLUtriangulatorObj *tobj;
     GLdouble v[3];
     Ipoint *n;
     float  mag;
     float brightness = 0.0f, contrast = 1.0f;
     float red, green, blue;
     unsigned char luv;
     unsigned char *ub = (unsigned char *)&(obj->mat3);
     int trans = (2.55f * (100.0 - (float)obj->trans));

     int blacklevel = 0, whitelevel = 255;
     int rampsize, cmapReverse = 0;
     float slope, point;

     static unsigned char cmap[3][256];
     int falsecolor = False;
     int r,g,b;

     int useLight = Imodv->lighting;
     /*int useLight = (obj->flags & IMOD_OBJFLAG_LIGHT);*/

     GLenum polyStyle, normStyle;

     if ((!mesh) || (!mesh->lsize))
	  return;

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
	 
     /*
      * calculate the color ramp to use.
      */
     if (obj->flags & IMOD_OBJFLAG_MCOLOR)
	  falsecolor = True;

     /* DNM: this initialization was needed on the PC.  There are models
      running around with 0 in this spot, which made PC display regular mesh */
     if (!ub[1])
	  ub[1] = 255;
     blacklevel = ub[0];
     whitelevel = ub[1];
     brightness = ub[0] - 128;
     contrast =  0.05f*(float)(ub[1]+1);

     if (blacklevel > whitelevel){
	  cmapReverse = blacklevel;
	  blacklevel = whitelevel;
	  whitelevel = cmapReverse;
	  cmapReverse = 1;
     }
     rampsize = whitelevel - blacklevel;
     if (rampsize < 1) rampsize = 1;

     for (i = 0; i < blacklevel; i++)
	  cmap[0][i] = 0;
     for (i = whitelevel; i < 256; i++)
	  cmap[0][i] = 255;
     slope = 256.0 / (float)rampsize;
     for (i = blacklevel; i < whitelevel; i++){
	  point = (float)(i - blacklevel) * slope;
	  cmap[0][i] = point;
     }

     if (cmapReverse){
	  for(i = 0; i < 256; i++){
	       if (!cmap[0][i])
		    cmap[0][i] = 255;
	       else
		    if (cmap[0][i] > 127){
			 cmap[0][i] -= 128;
			 cmap[0][i] *= -1;
			 cmap[0][i] += 128;
		    }
		    else{
			 cmap[0][i] -= 128;
			 cmap[0][i] *= -1;
			 cmap[0][i] += 128;
		    }
	  }
     }
     
     if (falsecolor){
	 for(i = 0; i < 256; i++){
	     mapfalsecolor(cmap[0][i], &r, &g, &b);
	     cmap[0][i] = (unsigned char)r;
	     cmap[1][i] = (unsigned char)g;
	     cmap[2][i] = (unsigned char)b;
	 }
     }else{
	  red   = obj->red; green = obj->green; blue = obj->blue;
	  if (obj->flags & IMOD_OBJFLAG_FCOLOR){
	       ub    = (unsigned char *)&(obj->mat1);
	       red   = (float)ub[0] / 255.0f;
	       green = (float)ub[1] / 255.0f;
	       blue  = (float)ub[2] / 255.0f;
	  }
	  for(i = 0; i < 256; i++){
	       mag = (float)cmap[0][i];
	       cmap[0][i] = (unsigned char)(red   * mag);
	       cmap[1][i] = (unsigned char)(green * mag);
	       cmap[2][i] = (unsigned char)(blue  * mag);
	  }
     }

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
	       i++;
	       while(mesh->list[i] != IMOD_MESH_ENDPOLY){
		    glBegin(polyStyle);
		    
		    n = &mesh->vert[mesh->list[i++]];
		    mag = 255.0 * sqrt
			 ((n->x * n->x) + (n->y * n->y) + (n->z * n->z));
		    luv = mag; 
		    
		    if (useLight)
			light_adjust(obj, cmap[0][luv]/255.0f, 
				          cmap[1][luv]/255.0f,
				          cmap[2][luv]/255.0f);

		    glColor4ub(cmap[0][luv], 
			       cmap[1][luv], 
			       cmap[2][luv],
			       trans);
		    glNormal3f(n->x, n->y, n->z);
		    n = &mesh->vert[mesh->list[i++]];
		    glVertex3f(n->x, n->y, n->z * zscale);
		    
		    n = &(mesh->vert[mesh->list[i]]);i++;
		    mag = 255.0 * sqrt
			((n->x * n->x) + (n->y * n->y) + (n->z * n->z));
		    luv = mag; 

		    if (useLight)
			light_adjust(obj, cmap[0][luv]/255.0f, 
				          cmap[1][luv]/255.0f,
				          cmap[2][luv]/255.0f);
		    glColor4ub(cmap[0][luv], 
			       cmap[1][luv], 
			       cmap[2][luv],
			       trans);
		    glNormal3f(n->x, n->y, n->z);
		    n = &mesh->vert[mesh->list[i++]];
		    glVertex3f(n->x, n->y, n->z * zscale);

		    n = &(mesh->vert[mesh->list[i++]]);
		    mag = 255.0 * sqrt
			 ((n->x * n->x) + (n->y * n->y) + (n->z * n->z));
		    luv = mag; 
		    if (useLight)
			light_adjust(obj, cmap[0][luv]/255.0f,
				     cmap[1][luv]/255.0f,
				     cmap[2][luv]/255.0f);
		    glColor4ub(cmap[0][luv],
			       cmap[1][luv],
			       cmap[2][luv],
			       trans);
		    glNormal3f(n->x, n->y, n->z);
		    n = &mesh->vert[mesh->list[i++]];
		    glVertex3f(n->x, n->y, n->z * zscale);
		    
		    glEnd();
	       }

	       break;

	     case IMOD_MESH_BGNBIGPOLY:
	       tobj = gluNewTess();
	       gluTessCallback(tobj, GLU_BEGIN, glBegin);
	       gluTessCallback(tobj, GLU_VERTEX, glVertex3fv);
	       gluTessCallback(tobj, GLU_END, glEnd);
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
	       fprintf(stderr, "imodlib: old mesh\n");
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



/*  IMOD VERSION 2.50
 *
 *  imodv_input.c -- Input handlers for imodv.
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
    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/

#include <stdlib.h>
#ifndef NO_SYS_TIMES
#include <sys/times.h>
#ifdef FIXED_CLK_TCK
#define USE_CLK_TCK FIXED_CLK_TCK
#else
#define USE_CLK_TCK CLK_TCK
#endif
#endif
#include <Xm/Xm.h>
#include <X11/keysym.h>
#include <Xm/VirtKeys.h>
#include <X11/Xutil.h>
#include <math.h>
#include "imodv.h"
#include "imod.h"

/*
 *  Some defines for porting to VMS.
 */
#ifndef XK_KP_Up
#define XK_KP_Up XK_KP_8
#endif
#ifndef XK_KP_Down
#define XK_KP_Down XK_KP_2
#endif
#ifndef XK_KP_Left
#define XK_KP_Left XK_KP_4
#endif
#ifndef XK_KP_Right
#define XK_KP_Right XK_KP_6
#endif
#ifndef XK_KP_Next
#define XK_KP_Next XK_KP_9
#endif
#ifndef XK_KP_Prior
#define XK_KP_Prior XK_KP_3
#endif
#ifndef XK_KP_Begin
#define XK_KP_Begin XK_KP_5
#endif

void imodvSelect(ImodvApp *a);
int  imodvStepTime(ImodvApp *a, int tstep);
void imodv_fog_move(ImodvApp *a);
void light_moveby(int x, int y);
void imeSetViewData(int wi);
Boolean imodv_movie_wp(XtPointer client);
void imodvDepthCueEditDialog(ImodvApp *a, int state);

#define ROTATION_FACTOR 1.26

int ImodvClosed = True;
int ImodvBgColorOpen = False;

void ximodv_quit_cb(Widget w, XtPointer client, XtPointer call)
{
  ImodvApp *a = (ImodvApp *)client;
  ImodvClosed = True;

  stereoHWOff();
  object_edit_kill();
  imodvModelEditDialog(a, 0);
  imodvViewEditDialog(a, 0);
  imodv_control(a, 0);
  imodvMovieDialog(Imodv, 0);
  imodvObjectListDialog(Imodv, 0);
  imodvImageEditDialog(Imodv, 0);
  imodvStereoEditDialog(Imodv, 0);
  imodvDepthCueEditDialog(Imodv, 0);

  /* DNM: this is unused so far.  Each window was supposed to add its
     close callback to the list, that would have saved the above list */
  imodvCallCloseCB();

  glXMakeCurrent(Imodv->display, None, NULL);
  XtPopdown(a->topLevel);
  XtDestroyWidget(a->topLevel);
  a->topLevel = 0;
  imodMatDelete(a->mat);
  imodMatDelete(a->rmat);
  return;
}

void imodv_exit(ImodvApp *a)
{
  stereoHWOff();

  if (a->standalone){
    exit(0);
  }
  ximodv_quit_cb(a->topLevel, (XtPointer)a, (XtPointer)0);
  return;
}


static unsigned int imodv_query_pointer(ImodvApp *a, int *wx, int *wy)
{
  Window rootr, childr;
  int rx, ry;
  unsigned int maskr;

  if (a->db)
    XQueryPointer(a->display,
                  XtWindow(a->dgfx), &rootr, &childr,
                  &rx, &ry, wx, wy, &maskr);
  else
    XQueryPointer(a->display,
                  XtWindow(a->gfx), &rootr, &childr,
                  &rx, &ry, wx, wy, &maskr);
  return(maskr);
}


static int b2x = 0;
static int b2y = 0;

void imodv_input_cb(Widget w, XtPointer client, XtPointer call)
{
  ImodvApp *a = (ImodvApp *)client;
  GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
  KeySym keysym;
  int cview;
  int tstep = 1;
  int newval;
  int b2dx, b2dy;
  float elapsed;

  if (!Imodv->imod) return;

  switch(cbs->event->type){
  case KeyPress:
    keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
    if (cbs->event->xkey.state & ShiftMask)
      tstep = 10;
    switch(keysym){

    case XK_b:
      if (cbs->event->xkey.state & ShiftMask){

        /* DNM 12/1/02: call this so it can keep track of open/closed state */
        menu_bgcolor_cb(w, client, call);
      }else{
        imodv_setbuffer(a);
        imodvDraw(Imodv);
      }
      break;
               
      /* Kludge, add clip data to model/object later. */
    case XK_c: /* print the current clipping plane parameters */
      if (cbs->event->xkey.state & ShiftMask){
        imodv_control(a, 1);
      }else{
        if (a->obj){
          /* DNM 7/31/01 remove pixsize from D */
          printf("Current Object clip data = (A B C D) "
                 "= %g %g %g %g.\n",
                 a->obj->clip_normal.x,
                 a->obj->clip_normal.y,
                 a->obj->clip_normal.z / a->imod->zscale,
                 ((a->obj->clip_normal.x * 
                   a->obj->clip_point.x) +
                  (a->obj->clip_normal.y * 
                   a->obj->clip_point.y) +
                  (a->obj->clip_normal.z * 
                   a->obj->clip_point.z)));
        }
      }
      break;

    case XK_g: /* gooder (sic) */
      if (!(cbs->event->xkey.state & ShiftMask))
        Imodv->fastdraw++;
      else
        Imodv->fastdraw--;
      if (Imodv->fastdraw < 0)
        Imodv->fastdraw = 0;
      if (Imodv->fastdraw > 3)
        Imodv->fastdraw = 3;
      printf("Sphere draw quality %d\n", Imodv->fastdraw);
      imodvDraw(Imodv);
      break;

    case XK_minus:
      if (!(cbs->event->xkey.state & ShiftMask))
        imodv_zoomd(Imodv, 0.95238095);
      else
        imodv_zoomd(Imodv, 0.5);
      imodvDraw(Imodv);
      break;
    case XK_equal:
      if (!(cbs->event->xkey.state & ShiftMask))
        imodv_zoomd(Imodv, 1.05);
      else
        imodv_zoomd(Imodv, 2.0);
      imodvDraw(Imodv);
      break;

    case XK_s:
    case XK_S:
      if (cbs->event->xkey.state & ShiftMask)
        imodv_auto_snapshot(NULL, SnapShot_RGB);
      else if (cbs->event->xkey.state & ControlMask)
        imodv_auto_snapshot(NULL, SnapShot_TIF);
      else
        imodvStereoToggle();
      break;

      /* '[' and ']' adjust stereo */
    case XK_bracketleft:
      a->plax -= 0.5f;
      imodvDraw(a);
      break;

    case XK_bracketright:
      a->plax += 0.5f;
      imodvDraw(a);
      break;

    case XK_l:
      if (cbs->event->xkey.state & ShiftMask)
        imodvObjectListDialog(a, 1);
      else {
        a->plax *= -1.0f;
        imodvDraw(a);
      }
      break;

    case XK_comma:
      newval = (int)(a->md->arot / ROTATION_FACTOR + 0.5);
      if (newval == a->md->arot)
        newval--;
      if (!newval)
        newval = 1;
      imodvControlSetArot(a, newval);
      break;

    case XK_period:
      newval = (int)(a->md->arot * ROTATION_FACTOR + 0.5);
      if (newval == a->md->arot)
        newval++;
      imodvControlSetArot(a, newval);
      break;

    case XK_m:
      if (cbs->event->xkey.state & ShiftMask){
        imodvModelEditDialog(Imodv, 1);
      }else{
        imodvMovieDialog(Imodv, 1);
      }
      break;

    case XK_i:
      imodvImageEditDialog(Imodv, 1);
      break;

    case XK_v:
      imodvViewEditDialog(a, 1);
      break;

    case XK_1:
      imodvStepTime(a,-1);
      imodvDraw(a);
      break;
    case XK_2:
      imodvStepTime(a,1);
      imodvDraw(a);
      break;
    case XK_8:
      if (a->drawall)
        a->drawall = False;
      else
        a->drawall = 3;
      imeSetViewData(a->drawall);
      imodvDraw(a);
      break;

    case XK_9:
      imodvSelectModel(a, a->cm - 1);
      break;
               
    case XK_0:
      imodvSelectModel(a, a->cm + 1);
      break;

    case XK_Next:
      imodv_translated(a, 0, 0, tstep);
      break;
    case XK_Prior:
      imodv_translated(a, 0, 0, -tstep);
      break;
    case XK_Up:
      imodv_translated(a, 0, -tstep, 0);
      break;
    case XK_Down:
      imodv_translated(a, 0, tstep, 0);
      break;
    case XK_Right:
      imodv_translated(a, -tstep, 0, 0);
      break;
    case XK_Left:
      imodv_translated(a, tstep, 0, 0);
      break;
    case XK_KP_Begin:
      if (!a->movie){
        a->md->xrotm = a->md->yrotm = a->md->zrotm =0;
        a->movie = True;
      }else{
        a->movie = False;
        a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
      }
      break;
    case XK_KP_Prior:
      imodv_rotate_model(a,0, 0, a->md->arot);
      break;
    case XK_KP_Next:
      imodv_rotate_model(a,0, 0, -a->md->arot);
      break;
    case XK_KP_Up:
      imodv_rotate_model(a,-a->md->arot, 0, 0);
      break;
    case XK_KP_Down:
      imodv_rotate_model(a,a->md->arot, 0, 0);
      break;
    case XK_KP_Left:
      imodv_rotate_model(a,0, -a->md->arot, 0);
      break;
    case XK_KP_Right:
      imodv_rotate_model(a,0, a->md->arot, 0);
      break;
               
    case XK_o:
      if ((cbs->event->xkey.state & ShiftMask)){
        objed(Imodv);
      }else{
        /* output info */
        printf("Zoom = %g\n", a->md->zoom);
        if (a->imod->view->world & VIEW_WORLD_ON){
          printf("Transformation matrix:");
          for(tstep = 0; tstep < 16; tstep++){
            if (!(tstep%4))
              printf("\n");
            printf("%7.3f ", a->imod->view->mat[tstep]);
          }
          printf("\n");
        }
        printf("Trans (x,y,z) = (%g, %g, %g)\n",
               a->imod->view->trans.x,
               a->imod->view->trans.y,
               a->imod->view->trans.z);
        printf("Rotate (x,y,z) = (%g, %g, %g)\n",
               a->imod->view->rot.x,
               a->imod->view->rot.y,
               a->imod->view->rot.z);
        if (a->movieFrames) {
#ifdef NO_SYS_TIMES
          elapsed = (float)(a->movieCurrent - a->movieStart) / 
	    (float)CLOCKS_PER_SEC;
#else
          elapsed = (float)(a->movieCurrent - a->movieStart) / 
	    (float)USE_CLK_TCK;
#endif
#ifdef NO_SYS_TIMES
          printf("%d frames / %.3f CPU sec = %.3f FPS\n",
                 a->movieFrames, elapsed, 
                 a->movieFrames / elapsed);
#else
          printf("%d frames / %.3f sec = %.3f FPS\n", 
                 a->movieFrames, elapsed, 
                 a->movieFrames / elapsed);
#endif
        }
      }
      break;

    case XK_z:
      if (Imodv->texMap)
        Imodv->texMap = 0;
      else
        Imodv->texMap = 1;
      break;

    case XK_r:
      if (Imodv->lowres)
        Imodv->lowres = 0;
      else
        Imodv->lowres = 1;
      imodvMenuLowres(a->lowres);
      imodvDraw(a);
      break;


    case XK_F1:
               
      break;

    case XK_Escape:
      imodv_exit(a);
      break;
    case XK_q:
      imodv_exit(a);
      break;
    }
    break;

  case KeyRelease:
    break;
          
  case ButtonPress:
    switch(cbs->event->xbutton.button){
    case Button1:
      a->lmx = cbs->event->xbutton.x;
      a->lmy = cbs->event->xbutton.y;
      b2x = -10;
      b2y = -10;
      /* DNM: why draw here? */
      /* imodvDraw(a); */
      break;
    case Button2:
      b2x = a->lmx = cbs->event->xbutton.x;
      b2y = a->lmy = cbs->event->xbutton.y;
      /* DNM: why draw here? */
      /* imodvDraw(a); */
      break;

    case Button3:
      if (a->hidemenu) break;
      imodvSelect(a);
      break;
    }
    break;

  case ButtonRelease:
    /*      b2dx = b2x - cbs->event->xbutton.x;
            b2dy = b2y - cbs->event->xbutton.y; */
    /*      if ((b2x == cbs->event->xbutton.x) &&
            (b2y == cbs->event->xbutton.y)){ */
    /*      if (b2dx * b2dx + b2dy * b2dy < 17) {
            a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
            a->movie = False;
            break;
            } */
    if (cbs->event->xmotion.state & Button2Mask){
      if (!(cbs->event->xmotion.state & ShiftMask))
        imodv_rotate(a, True);
    }
    break;

  case MotionNotify:
    if (cbs->event->xmotion.state & Button1Mask){
      if (!(cbs->event->xmotion.state & ShiftMask))
        /*   DNM: disable this */
        /*           imodv_fog_move(a);
                     else */
        imodv_translate(a, cbs->event->xmotion.x, 
                        cbs->event->xmotion.y);
    }
    if (cbs->event->xmotion.state & Button2Mask){
      if (cbs->event->xmotion.state & ShiftMask)
        imodv_light_move(a);
      else 
        imodv_rotate(a, False);
    }
    a->lmx = cbs->event->xmotion.x;
    a->lmy = cbs->event->xmotion.y;
    break;

  default:
    break;
  }
}

/*
 *  Move the light.
 */
void imodv_light_move(ImodvApp *a)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);

  if ((maskr & Button2Mask) && (maskr & ShiftMask)){

    /*        a->lightx += 10 * (mx - a->lmx);
              a->lighty += 10 * (my - a->lmy);
              light_move(&(a->lightx), &(a->lighty));
    */
    light_moveby(10 * (mx - a->lmx),
                 10 * (my - a->lmy));
  }
  imodvDraw(a);
  return;
}


/* model coord transformation. */

void imodv_translate(ImodvApp *a, int x, int y)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  int dx, dy;
  float vx[4], vy[4], vz[4];
     
  dx = -(mx - a->lmx);
  dy = my - a->lmy;

  imodv_translated(a, dx, dy, 0);
  return;
}


void imodv_zoomd(ImodvApp *a, double zoom)
{
  int m;
  if (!a->imod) return;

  if (a->crosset){
    for(m = 0; m < a->nm; m++)
      a->mod[m]->view->rad /= zoom;
  }else{
    a->imod->view->rad /= zoom;
  }

  return;
}

void imodv_translated(ImodvApp *a, int x, int y, int z)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
    
  Imod *imod;
  Imat *mat = a->mat;
  Ipoint ipt, opt, spt;
  int m, mstrt, mend;
  float scrnscale;

  if ((maskr & ControlMask) || !a->moveall) {
    mstrt = a->cm;
    mend = mstrt + 1;
  } else {
    mstrt = 0;
    mend = a->nm;
  }

  /* DNM: changed to compute shift properly for each model, to take account
     of actual scale to window, and to shift by mouse move amount */

  for (m = mstrt; m < mend; m++) {
    imod = a->mod[m];
    imodMatId(mat);
    imodMatRot(mat, -(double)imod->view->rot.x, X);
    imodMatRot(mat, -(double)imod->view->rot.y, Y);
    imodMatRot(mat, -(double)imod->view->rot.z, Z);

    scrnscale = 0.5 * (a->winx > a->winy ? a->winy : a->winx) / 
      imod->view->rad;
    
    spt.x = 1.0f/scrnscale;
    spt.y = 1.0f/scrnscale;
    spt.z = 1.0f/scrnscale * 1.0f/a->mod[a->cm]->zscale;
    imodMatScale(mat, &spt);
    
    ipt.x = x;
    ipt.y = y;
    ipt.z = z;
    imodMatTransform(mat, &ipt, &opt);
    
    opt.x *= (1.0/ imod->view->scale.x);
    opt.y *= (1.0/ imod->view->scale.y);
    opt.z *= (1.0/ imod->view->scale.z);
    
    if (maskr & ControlMask){
      if (a->obj){
        if (a->obj->clip){
          a->obj->clip_point.x += opt.x;
          a->obj->clip_point.y += opt.y;
          a->obj->clip_point.z += opt.z;
        }
      }
    }else{ 
      imod->view->trans.x -= opt.x;
      imod->view->trans.y -= opt.y;
      imod->view->trans.z -= opt.z;
    }
  }
    
  imodvDraw(a);
  return;
}
    

/* Rotate the model by the actual angles x, y and z.
 * The angles are in 0.1 degree increments.
 * DNM: made this work properly.
 */
void imodv_rotate_model(ImodvApp *a, int x, int y, int z)
{
  imodv_compute_rotation(a, (float)x, (float)y, (float)z);
  imodvDraw(a);
  return;
}

void imodv_compute_rotation(ImodvApp *a, float x, float y, float z)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  int m, mstrt, mend;
  Imat *mat = a->mat;
  Imat *mato, *matp;
  double alpha, beta, gamma, gamrad;
  Ipoint normal;
  Ipoint scalePoint;
  Imod *imod;

  /* IF movieing, save the current increments as ones to movie on */
  if (a->movie){
    a->md->xrotm = (INT)x;
    a->md->yrotm = (INT)y;
    a->md->zrotm = (INT)z;
    /* DNM: new workproc approach, start it here and go on */
    if (!a->wpid) {
      a->wpid = XtAppAddWorkProc(a->context, 
                                 (XtWorkProc)imodv_movie_wp, 
                                 (XtPointer)a);
      a->movieFrames = 0;
      a->movieStart = imodv_sys_time();
    }
    /*  return; */
  }

  mato = imodMatNew(3);
  matp = imodMatNew(3);

  /* Compute a matrix that resolves X and Y rotations into rotation
     about a single axis */
  gamrad = atan2((double)y, (double)x);
  gamma = gamrad / 0.017453293;
  alpha = 0.1 * (x * cos(-gamrad) - y * sin(-gamrad));

  imodMatId(mat);
  imodMatRot(mat, -gamma, Z);
  imodMatRot(mat, alpha, X);
  imodMatRot(mat, gamma + (double)(0.1 * z), Z);

  if (!(maskr & ControlMask)){

    /* Regular rotation of one or all models */

    if (!a->moveall) {
      mstrt = a->cm;
      mend = mstrt + 1;
    } else {
      mstrt = 0;
      mend = a->nm;
    }

    for (m = mstrt; m < mend; m++) {
      imod = a->mod[m];

      /* Compute current rotation matrix */
      imodMatId(mato);
      imodMatRot(mato, (double)imod->view->rot.z, Z);
      imodMatRot(mato, (double)imod->view->rot.y, Y);
      imodMatRot(mato, (double)imod->view->rot.x, X);

      /* Multiply by the new rotation, then get back to 3 angles */
      imodMatMult(mato, mat, matp);
      imodMatGetNatAngles(matp, &alpha, &beta, &gamma);
      imod->view->rot.x = alpha;
      imod->view->rot.y = beta;
      imod->view->rot.z = gamma;
    }
          
  } else {

    /* Clipping plane rotation: apply to current model only */

    imod = a->imod;

    /* Find the normal in scaled model coordinates by scaling
       each of the components appropriately */
    scalePoint.x = a->obj->clip_normal.x / imod->view->scale.x;
    scalePoint.y = a->obj->clip_normal.y / imod->view->scale.y;
    scalePoint.z = a->obj->clip_normal.z / 
      (imod->view->scale.z * imod->zscale);

    /* get current rotation transform into viewing space */
    imodMatId(mato);
    imodMatRot(mato, (double)imod->view->rot.z, Z);
    imodMatRot(mato, (double)imod->view->rot.y, Y);
    imodMatRot(mato, (double)imod->view->rot.x, X);

    /* Get product of that with screen-oriented rotation */
    imodMatMult(mato, mat, matp);
    imodMatTransform(matp, &scalePoint, &normal);

    /* Back-transform normal by inverse of current transform */

    imodMatId(mato);
    imodMatRot(mato, -(double)imod->view->rot.x, X);
    imodMatRot(mato, -(double)imod->view->rot.y, Y);
    imodMatRot(mato, -(double)imod->view->rot.z, Z);
    imodMatTransform(mato, &normal, &scalePoint);

    /* Rescale components to get back to unscaled model normal */
    a->obj->clip_normal.x = scalePoint.x * imod->view->scale.x;
    a->obj->clip_normal.y = scalePoint.y * imod->view->scale.y;
    a->obj->clip_normal.z = scalePoint.z * 
      (imod->view->scale.z * imod->zscale);
    imodPointNormalize(&(a->obj->clip_normal));

  }

  imodMatDelete(mato);
  imodMatDelete(matp);
  return;
}


/* Rotate the model or clipping planes by the amount of cursor movement. */

#define MOUSE_TO_ANGLE 1.0f
#define MOUSE_TO_THROW 0.25f
#define MIN_SQUARE_TO_THROW 17

void imodv_rotate(ImodvApp *a, int throwFlag)
{
  int mx, my, idx, idy;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  float dx, dy;

  /* If movie on and not a Control rotation, then check the throw flag */

  if (a->movie && !(maskr & ControlMask)) {
    if (throwFlag) { 

      /* If throwing at end of movement, then turn off movie if
         movement is too small, otherwise set rotations to the total
         movement since the button was pressed */

      dx = (mx - b2x);
      dy = (my - b2y);
      if (dx * dx + dy * dy < MIN_SQUARE_TO_THROW) {
        a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
        a->movie = False;
        return;
      }

      idx = (int)(MOUSE_TO_THROW * dy + 0.5);
      idy = (int)(MOUSE_TO_THROW * dx + 0.5);
      if (!idx && !idy)
        a->movie = False;
      a->md->xrotm = idx;
      a->md->yrotm = idy;
      a->md->zrotm = 0;
      /* DNM: new workproc approach, start it here */
      if (a->movie && !a->wpid) {
        a->wpid = XtAppAddWorkProc(a->context, 
                                   (XtWorkProc)imodv_movie_wp, 
                                   (XtPointer)a);
        a->movieFrames = 0;
        a->movieStart = imodv_sys_time();
      }
    }
    return;
  }
       
  /* If the mouse button has been released, don't rotate. */
  if (!(maskr & Button2Mask))
    return; 

  /* Turn off movie for all rotation axis. DNM add movie flag too */
  a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
  a->movie = False;

  /* Get the total x and y movement. */
  dx = (mx - a->lmx);
  dy = (my - a->lmy);
  idx = (int)(MOUSE_TO_ANGLE * dy + 0.5);
  idy = (int)(MOUSE_TO_ANGLE * dx + 0.5);
  if ((!idx) && (!idy))
    return;
     
  imodv_rotate_model(a, idx, idy, 0);

  /* This is uneeded, since the rotate_model has a draw */
  /* imodvDraw(a); */
  return;
}



/*****************************************************************************/
/* Callbacks. */
static Ilist *closeCB = NULL;
static Ilist *drawCB = NULL;

void imodvAddCloseCB(void (*func)(ImodvApp *, XtPointer, int),
                     XtPointer client)
{
  ImodvCallBack cb;
  if (!closeCB){
    closeCB = ilistNew(sizeof(ImodvCallBack),4);
    if (!closeCB) return;
  }
  cb.func   = func;
  cb.client = client;
  ilistAppend(closeCB, &cb);
}

void imodvRemoveCloseCB(void (*func)(ImodvApp *, XtPointer, int),
                        XtPointer client)
{
  int index = 0;
  ImodvCallBack *cb;
  if (!closeCB) return;

  cb = (ImodvCallBack *)ilistFirst(closeCB);
  while(cb){
    if (((int)cb->func == (int)func) && 
        ((int)cb->client == (int)client)){
      ilistRemove(closeCB, index);
    }
    cb = (ImodvCallBack *)ilistNext(closeCB);
    index++;
  }
}

void imodvCallCloseCB(void)
{
  ImodvCallBack *cb;

  if (closeCB){
    cb = (ImodvCallBack *)ilistFirst(closeCB);
    while(cb){
      (*cb->func)(Imodv, cb->client, 0);
      cb = (ImodvCallBack *)ilistNext(closeCB);
    }
    ilistDelete(closeCB);
    closeCB = NULL;
  }
  if (drawCB){
    ilistDelete(drawCB);
    drawCB = NULL;
  }
}

/* Add a function to the draw callback list. */
void imodvAddDrawCB(void (*func)(ImodvApp *, XtPointer, int),
                    XtPointer client)
{
  ImodvCallBack cb;
  if (!drawCB){
    drawCB = ilistNew(sizeof(ImodvCallBack),4);
    if (!drawCB) return;
  }
  cb.func   = func;
  cb.client = client;
  ilistAppend(drawCB, &cb);
}

void imodvRemoveDrawCB(void (*func)(ImodvApp *, XtPointer, int),
                       XtPointer client)
{
  int index = 0;
  ImodvCallBack *cb;
  if (!drawCB) return;

  cb = (ImodvCallBack *)ilistFirst(drawCB);
  while(cb){

    if (((int)cb->func == (int)func) && 
        ((int)cb->client == (int)client)){
      /*             printf("listsize %d\n", drawCB->size);
                     printf("removed drawCB %x, %x, %d\n",
                     cb->func,cb->client,index);
                     ilistRemove(drawCB, index);
                     printf("listsize %d\n", drawCB->size);
      */
      ilistRemove(drawCB, index);
    }
    cb = (ImodvCallBack *)ilistNext(drawCB);
    index++;
  }
}

void imodvCallDrawCB(int reason)
{
  ImodvCallBack *cb;

  if (drawCB){
    if (drawCB->size){
      cb = (ImodvCallBack *)ilistFirst(drawCB);
      while(cb){
        (*cb->func)(Imodv, cb->client, reason);
        cb = (ImodvCallBack *)ilistNext(drawCB);
      }
    }
  }
  return;
}


/*****************************************************************************/
#define SELECT_BUFSIZE 4096
#define OBJCONTNAME(a,b) = ((a<<16)|(b))
/*#define HIT_DEBUG       */

void processHits (ImodvApp *a, GLint hits, GLuint buffer[])
{
  extern struct ViewInfo *XYZ_vi;
  unsigned int i, j;
  GLuint names, *ptr, *ptrstr;
  unsigned int z1, z2, zav, zmin;
  int tmo, tob, tco, tpt;
  int mo, ob, co, pt;

  if (!hits) return;
  /* If it overflowed, process what's there */
  if (hits == -1) hits = SELECT_BUFSIZE/3; 

  imodGetIndex(a->imod, &ob, &co, &pt);

#ifdef HIT_DEBUG
  printf ("hits = %d\n", hits);
#endif
  ptr = (GLuint *) buffer;
  ptrstr = ptr;
  pt = -1;

  for (i = 0; i < hits; i++) {    /* for each hit */
    if (ptr - ptrstr >= SELECT_BUFSIZE) break;
    names = *ptr; ptr++;
    if ((ptr - ptrstr) + names + 2 > SELECT_BUFSIZE) break;
          
    z1 = *ptr; ptr++;
    z2 = *ptr; ptr++;
    zav = z1/2 + z2/2;

#ifdef HIT_DEBUG
    printf (" # names = %d", names);
    printf (";  z1 = %u;", z1);
    printf (" z2 = %u; ", z2);
    printf ("   names are ");
#endif
                
    for (j = 0; j < names; j++) {   /*  for each name */
      switch(j){
      case 0:
        tmo = *ptr;
        break;
      case 1:
        tob = *ptr;
        break;
      case 2:
        tco = *ptr;
        break;
      case 3:
        tpt = *ptr;
        break;
      }



#ifdef HIT_DEBUG
      printf ("%d ", *ptr);
#endif
      ptr++;          
    }

    /* If it was a good hit (4 names) and its in front of any previous, take it */
    if ((names > 3 ) && ((i == 0) || (zav <= zmin))) { 
      zmin = zav;
      mo = tmo; ob = tob; co = tco; pt = tpt;
#ifdef HIT_DEBUG
      printf (" *");
#endif
    }
#ifdef HIT_DEBUG
    printf ("\n");
    printf ("   zav = %u; zmin = %u\n",zav, zmin);
#endif

  }

  if (pt == -1) return;
  a->cm = mo;
  a->imod = a->mod[mo];
  imodSetIndex(a->imod, ob, co, pt);     
  if (!a->standalone){
    imod_setxyzmouse();
  }
}

void imodvSelect(ImodvApp *a)
{
  static unsigned int buf[SELECT_BUFSIZE];
  GLint hits;
  int x, y;

  imodv_winset(a);
  imodv_query_pointer(a, &x, &y);
  glSelectBuffer(SELECT_BUFSIZE, buf);
  glRenderMode( GL_SELECT);

  a->xPick = x;
  a->yPick = a->winy - y;

  a->wPick = a->hPick = 10;
  a->doPick = True;
     
  glInitNames();
     
  imodvDraw(a);

  a->doPick = False;
  hits = glRenderMode( GL_RENDER );
  processHits(a, hits, buf);

}

int imodvStepTime(ImodvApp *a, int tstep)
{
  Iobj *obj;
  Icont *cont;
  int ob, co;

  if (!a->standalone){
    if (tstep > 0)
      inputNextTime(a->vi);
    if (tstep < 0)
      inputPrevTime(a->vi); 
    return 0;
  }

  for(;;){
    a->imod->ctime += tstep;
    if (a->imod->ctime < 0){ a->imod->ctime = 0; return 0; }
    if (a->imod->ctime > a->imod->tmax){
      a->imod->ctime = a->imod->tmax;
      return a->imod->tmax;
    }
        
    for(ob = 0 ; ob < a->imod->objsize; ob++){
      obj = &a->imod->obj[ob];
      if (!iobjTime(obj->flags)) continue;
      for(co = 0; co < obj->contsize; co++){
        if (obj->cont[co].type == a->imod->ctime)
          return(a->imod->ctime);
      }
    }
  }
}

/* DNM 5/21/01: returns real time, or CPU time time times function not
   available */
clock_t imodv_sys_time(void)
{
#ifndef NO_SYS_TIMES
  struct tms buf;
  return(times(&buf));
#else
  return(clock());
#endif
}

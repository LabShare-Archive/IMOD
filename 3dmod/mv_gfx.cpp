/*  
 *  mv_gfx.cpp -- Higher level drawing functions for imodv - contains 
 *                 functions to respond to initialize, resize, and paint calls
 *                 in the GL widget, and snapshot routines
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

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <errno.h>
#include <qdir.h>
#include "b3dgfx.h"
#include "mv_window.h"

#include "imod.h"
#include "preferences.h"
#include "b3dfile.h"
#include "imodv.h"
#include "sslice.h"
#include "scalebar.h"
#include "mv_control.h"
#include "mv_gfx.h"
#include "mv_ogl.h"
#include "mv_input.h"
#include "mv_light.h"
#include "mv_stereo.h"
#include "vertexbuffer.h"
#include "resizetool.h"

Ipoint ImodvCurModLight;

/* local functions */
static int imodv_snapshot(ImodvApp *a, QString fname);
static void imodv_clear(ImodvApp *a);
static void drawLightVector(ImodvApp *a);


/* 12/15/02: removed DisplayHasAlpha */


/*
 *  Set the current OpenGL rendering context to the window being used by Imodv.
 *  It generates errors to do this after entering selection mode
 */
int imodv_winset(ImodvApp *a)
{
  if (!a->doPick)
    a->mainWin->mCurGLw->makeCurrent();
  return(1);
}

/*
 *  A swap buffer command (should not be needed)
 */
void imodv_swapbuffers(ImodvApp *a)
{
  if (a->doPick) 
    return;
  a->mainWin->mCurGLw->makeCurrent();
  
  if (a->dblBuf)
    a->mainWin->mCurGLw->swapBuffers();
  glFlush();
}
          
// Clear the window - set context only if not in selection mode
static void imodv_clear(ImodvApp *a)
{
  imodv_winset(a);
  glClearColor(a->rbgcolor->red() / 256., a->rbgcolor->green() / 256.,
	       a->rbgcolor->blue() / 256., a->transBkgd ? 0.0 : 1.0);
  if (a->clearAfterStereo) {
    glDrawBuffer(a->dblBuf ? GL_BACK_RIGHT : GL_RIGHT);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDrawBuffer(a->dblBuf ? GL_BACK : GL_FRONT);
    a->clearAfterStereo = 0;
  }
     
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glFlush();
}

/*
 * Exchange double and single buffers (db >= 0)
 * OR switch between stereo and non stereo (db < 0) 
*/
void imodv_setbuffer(ImodvApp *a, int db, int stereo, int alpha)
{
  int useStereo, useDb = a->dblBuf;
  int inStereo = a->stereo == IMODV_STEREO_HW ? 1 : 0;
  int useAlpha = a->alphaVisual;

  // Skip if the requested change doesn't have a visual
  if (db >= 0) {
    if ((db && ((!inStereo && a->enableDepthSB < 0) || 
                (inStereo && a->enableDepthSBst < 0) || useAlpha)) ||
        (!db && ((!inStereo && a->enableDepthDB < 0 && a->enableDepthDBal < 0) || 
                 (inStereo && a->enableDepthDBst < 0 && a->enableDepthDBstAl < 0))))
      return;
    useStereo = inStereo;
    useDb = db;
  } else if (alpha >= 0) {
    if ((alpha && ((!inStereo && a->enableDepthDB < 0) || 
                   (inStereo && a->enableDepthDBst < 0))) ||
        (!alpha && ((!inStereo && a->enableDepthDBal < 0) ||
                    (inStereo && a->enableDepthDBstAl < 0) || !useDb)))
      return;
    useStereo = inStereo;
    useAlpha = alpha;
  } else {
    if ((a->dblBuf && ((!stereo && a->enableDepthDB < 0 && a->enableDepthDBal < 0) || 
                   (stereo && a->enableDepthDBst < 0 && a->enableDepthDBstAl < 0))) ||
        (!a->dblBuf && ((!stereo && a->enableDepthSB < 0) || 
                    (stereo && a->enableDepthSBst < 0 ))))
      return;
    useStereo = stereo;
  }
    
  imodv_clear(a);

  if (a->mainWin->setGLWidget(a, useDb, useStereo, useAlpha))
    return;

  // Only if it succeeds do we update the state items
  a->dblBuf = useDb;
  a->alphaVisual = useAlpha;
  a->mainWin->setCheckableItem(VVIEW_MENU_DB, useDb);
  imodvStereoUpdate();
  imodv_winset(a);
  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);
  glFlush();
  glFinish();
  glViewport(0, 0, a->winx, a->winy);
}

// Initialize the widget, whichever one is called
// These are the commands that were present - except for glClearIndex
void imodvInitializeGL()
{
  if (Imod_debug)
    imodPrintStderr("imodv Initialize GL\n");
  float rad = Imodv->imod->view->rad*1.5f;
  glClearColor(1.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
  glFogi(GL_FOG_MODE, GL_LINEAR);
  glFogf(GL_FOG_START,  0.0f);
  glFogf(GL_FOG_END, rad);
  if (!Imodv->dblBuf)
      glEnable(GL_LINE_SMOOTH);
  light_init();

}

// Resize of window
void imodvResizeGL(ImodvGL *GLw, int winx, int winy)
{
  ImodvApp *a = Imodv;
  if (Imod_debug)
    imodPrintStderr("imodv Resize GL %d %d\n", winx, winy);
  if (GLw != a->mainWin->mCurGLw)
    return;
  a->winx = winx;
  a->winy = winy;
  glViewport(0, 0, a->winx, a->winy); 
  if (a->mainWin->mResizeTool)
    a->mainWin->mResizeTool->newSize(a->winx, a->winy);
}  

// The central draw entry: just call to update through the paint routine
void imodvDraw(ImodvApp *a)
{
  int m, mstrt, mend, time;
  Ipoint center;
  float angles[3];

  // But first update all model angles if linked to slicer
  if (a->linkToSlicer && !getTopSlicerAngles(angles, &center, time)) {
    if (!a->moveall) {
      mstrt = a->curMod;
      mend = mstrt + 1;
    } else {
      mstrt = 0;
      mend = a->numMods;
    }

    for (m = mstrt; m < mend; m++) {
      a->mod[m]->view->rot.x = angles[b3dX];
      a->mod[m]->view->rot.y = angles[b3dY];
      a->mod[m]->view->rot.z = angles[b3dZ];
    }
  }

  a->mainWin->mCurGLw->updateGL();
}

// The paint routine called by the widget
void imodvPaintGL()
{
  ImodvApp *a = Imodv;
  static int drawcount = 0;
  int color;
  float scale;

  // First time in, find the OpenGL version and set vertBufOK to -1 or 1
  if (a->vertBufOK < -1) {
    a->glExtFlags = b3dInitializeGL();
    a->vertBufOK = (a->glExtFlags & B3DGLEXT_VERTBUF) ? 1 : -1;
    a->primRestartOK = (a->glExtFlags & B3DGLEXT_PRIM_RESTART) ? 1 : 0;
  }

  //if (Imod_debug)
  //  imodPrintStderr("drawing %d\n", drawcount);
  drawcount++;

  if (!a->imod)
    return;

  // Set context (if not done already for selection mode) then set selection
  // mode.  Is it really necessary to set context?
  imodv_winset(a);
  if (a->doPick) {
    glRenderMode( GL_SELECT);
    glInitNames();
  }
  // 6/6/04: deleted code for drawing whole model as dlist (unused)

  imodv_clear(a);

  switch (a->stereo){
  case IMODV_STEREO_RL:
    a->winx /= 2;
    a->stereo *= -1;
    imodvDraw_models(a);

    a->stereo *= -1;
    imodvDraw_models(a); 
    a->winx *= 2;
    glViewport(0, 0, a->winx, a->winy);
    break;

  case IMODV_STEREO_TB:
    a->winy /= 2;
    a->stereo *= -1;
    imodvDraw_models(a);

    a->stereo *= -1;
    imodvDraw_models(a);
    a->winy *= 2;
    glViewport(0, 0, a->winx, a->winy);
    break;

  case IMODV_STEREO_HW:

    // 6/8/04: The second clear may be superfluous, let stereo routine decide
    a->stereo *= -1;
    stereoDrawBuffer(a->dblBuf ? GL_BACK_RIGHT : GL_RIGHT);
    imodvStereoClear();
    imodvDraw_models(a);

    a->stereo *= -1;
    stereoDrawBuffer(a->dblBuf ? GL_BACK_LEFT : GL_LEFT);
    imodvStereoClear();
    imodvDraw_models(a);
    break;


  case IMODV_STEREO_OFF:
    imodvDraw_models(a);
    break;
  }

  a->vbManager->clearTempArrays();
  b3dResizeViewportXY(a->winx, a->winy);
  if (a->drawLight)
    drawLightVector(a);
  color = 0;
  if (!a->texMap && !a->rbgcolor->red() && !a->rbgcolor->blue() && !a->rbgcolor->green())
    color = -1;
  if (!a->texMap && a->rbgcolor->red() == 255 && a->rbgcolor->blue() == 255 && 
      a->rbgcolor->green() == 255)
    color = 1;
  scale = 0.5 * (a->winx > a->winy ? a->winy : a->winx) /
    a->imod->view->rad;
  a->scaleBarSize = scaleBarDraw(a->winx, a->winy, scale, color);

  // Cards with hidden stereo capability need this to avoid losing display
  // (In Qt 3 - take it out for Qt 4)
  /*#ifdef Q_OS_MACX
  imodv_swapbuffers(a);
  imodv_swapbuffers(a);
  #endif*/
  imodvControlSetView(a);
  imodvControlUpdate(a);
}


/*
 * Draw lighting vector  (when shift middle mouse down)
 */
static void drawLightVector(ImodvApp *a)
{
  Ipoint pnt, cen, an, ar;
  float winhalf = 0.5 * B3DMIN(a->winx, a->winy);
  float radfrac = winhalf * 0.9f;
  float del = 10.f;
  float smallVal = 1.e-4;
  double val, rpd = RADIANS_PER_DEGREE;
  Imat *mat = imodMatNew(3);
  GLboolean depthEnabled;
  if (!mat)
    return;

  // Disable depth test and enable at end
  depthEnabled = glIsEnabled(GL_DEPTH_TEST);
  if (depthEnabled)
    glDisable(GL_DEPTH_TEST);

  // Get the normal to light, normalize and draw from center
  pnt = ImodvCurModLight;
  imodPointNormalize(&pnt);
  glColor4ub(255, 0, 0, 255);
  glBegin(GL_LINE_STRIP);
  glVertex2f((float)(0.5 * a->winx), (float)(0.5 * a->winy));
  cen.x = pnt.x * radfrac + 0.5 * a->winx;
  cen.y = pnt.y * radfrac + 0.5 * a->winy;
  glVertex2f(cen.x, cen.y);
  glEnd();

  // Compute a rotation to bring normal to vertical, and get inverse matrix
  an.y = 0.0;
  if (pnt.x > smallVal || pnt.z > smallVal || pnt.x < -smallVal || 
      pnt.z < -smallVal)
    an.y = -atan2((double)pnt.x, (double)pnt.z);
  val = an.y;
  val = pnt.z * cos(val) - pnt.x * sin(val);
  an.x = 90. * rpd - atan2(val, (double)pnt.y);
  imodMatRot(mat, -an.x / rpd, b3dX);
  imodMatRot(mat, -an.y / rpd, b3dY);

  // Rotate each diagonal vector for drawing cross at end
  an.z = 0.;
  an.x = del;
  an.y = del;
  imodMatTransform3D(mat, &an, &ar);
  glBegin(GL_LINE_STRIP);
  glVertex2f(cen.x - ar.x, cen.y - ar.y);
  glVertex2f(cen.x + ar.x, cen.y + ar.y);
  glEnd();
  an.y = -del;
  imodMatTransform3D(mat, &an, &ar);
  glBegin(GL_LINE_STRIP);
  glVertex2f(cen.x - ar.x, cen.y - ar.y);
  glVertex2f(cen.x + ar.x, cen.y + ar.y);
  glEnd();
  imodMatDelete(mat);
  if (depthEnabled)
    glEnable(GL_DEPTH_TEST);
}


/****************************************************************************/
/****************************************************************************/
/**                        Image Snapshot Utilities                        **/
/****************************************************************************/
/****************************************************************************/

void imodvResetSnap()
{
  Imodv->snap_fileno = 0;
}

int imodv_auto_snapshot(QString fname, int format_type)
{
  ImodvApp *a = Imodv;
  QString sname;

  if (fname.isEmpty()) {

    /* DNM 6/4/03: add logic to avoid overwriting files */
    /* DNM 12/28/03: use common routine to get filename */
    fname = b3dGetSnapshotName("modv", format_type, 4, a->snap_fileno);
  }
  sname = b3dShortSnapName(fname);

  imodPrintStderr("3dmodv: Saving image to %s", LATIN1(sname));

  if (a->dblBuf)
    a->mainWin->mCurGLw->setBufferSwapAuto(false);
  glReadBuffer(a->dblBuf ? GL_BACK : GL_FRONT);

  if (format_type == SnapShot_TIF || 
      (format_type == SnapShot_RGB && ImodPrefs->snapFormat() != "RGB")) {
    imodvDraw(Imodv);
    b3dSetCurSize(Imodv->winx, Imodv->winy);
    if (format_type == SnapShot_TIF)
      b3dSnapshot_TIF(fname, 1, NULL, NULL, false);
    else
      b3dSnapshot_NonTIF(fname, 1, NULL, NULL);
    imodPrintStderr(".\n");
  } else {
    if (imodv_snapshot(Imodv, fname))
      imodPrintStderr(" : error writing file\n");
    else
      imodPrintStderr(".\n");
  }

  if (a->dblBuf) {
    imodv_swapbuffers(a);
    a->mainWin->mCurGLw->setBufferSwapAuto(true);
  }

  /* DNM 12/3/03: need to not free the inName since it is Qt's toLatin1 string */
  return(0);
}

// Save snapshot as SGI RGB file
static int imodv_snapshot(ImodvApp *a, QString fname)
{
  FILE *fout;
  unsigned char *pixels;
  int width, height;
  int i, xysize;
  GLint xoffset;
  char iname[80];
  char sep = QDir::separator().toLatin1();
  QString tailname = QDir::convertSeparators(fname);

  errno = 0;
  fout = fopen(LATIN1(tailname), "wb");
  if (!fout){
    QString qerr = "3dmodv: error opening file\n";
    if (errno)
      qerr +=  QString("System error: ") + QString(strerror(errno));
    imodPrintStderr(LATIN1(qerr));
    return(-1);
  }
  i = tailname.lastIndexOf(sep) + 1;
  tailname = tailname.right(tailname.length() - i);
  tailname.truncate(59);

  width = a->winx;
  height = a->winy;
  pixels = (unsigned char *)malloc(width * height * 3);  /* was * 3 * 4 */
  if (!pixels){
    fclose(fout);
    return(-1);
  }
  imodvDraw(a);

  /* Width Needs to be multiple of 4 for Octane with unsigned bytes */
  xoffset = (width % 4) / 2;
  width = 4 * (width / 4);  

  /*     glReadPixels(0, 0, width, height,  
         GL_RGB, GL_UNSIGNED_INT, pixels);
  */
  //  glReadBuffer(GL_FRONT);    /* DNM: have to read from front buffer */
  glReadPixels(xoffset, 0, width, height,  
               GL_RGB, GL_UNSIGNED_BYTE, pixels);
  glFlush();

 
  /* Create an SGI rgb file */

  iputshort(fout, 474);       /* MAGIC                */
  iputbyte (fout,   0);       /* STORAGE is VERBATIM  */
  iputbyte (fout,   1);       /* BPC is 1             */
  iputshort(fout,   3);       /* DIMENSION is 3       */
  iputshort(fout, width);     /* XSIZE                */
  iputshort(fout, height);    /* YSIZE                */
  iputshort(fout,   3);       /* ZSIZE                */
  iputlong (fout, 0l);        /* PIXMIN is 0          */
  iputlong (fout, 255l);      /* PIXMAX is 255        */
  iputlong (fout, 0);         /* DUMMY 4 bytes        */
  sprintf(iname, "%s, Created by 3dmodv.", LATIN1(tailname));
  fwrite(iname, 80, 1, fout); /* IMAGENAME            */
  iputlong (fout, 0);         /* COLORMAP is 0        */
  for(i=0; i<404; i++)        /* DUMMY 404 bytes      */
    iputbyte(fout,0);

  /* image data */
  /*
    for (i = 0, xysize = width * height; i < xysize; i++)
    iputbyte (fout, pixels[i*3]/65536);
    for (i = 0; i < xysize; i++)
    iputbyte (fout, pixels[(i*3)+1]/65536);
    for (i = 0; i < xysize; i++)
    iputbyte (fout, pixels[(i*3)+2]/65536);
  */
  for (i = 0, xysize = width * height; i < xysize; i++)
    iputbyte (fout, pixels[i*3]);
  for (i = 0; i < xysize; i++)
    iputbyte (fout, pixels[(i*3)+1]);
  for (i = 0; i < xysize; i++)
    iputbyte (fout, pixels[(i*3)+2]);

  free(pixels);
  fclose(fout);
  return(0);
}

/*  IMOD VERSION 2.7.9
 *
 *  imodv_gfx.c -- Higher level drawing functions for imodv - contains 
 *                 functions to respond to initialize, resize, and paint calls
 *                 in the GL widget, and snapshot routines
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

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <qdir.h>
#include "imodv_window.h"

#include "imod.h"
#include "b3dgfx.h"
#include "b3dfile.h"
#include "imodv.h"
#include "imodv_control.h"
#include "imodv_gfx.h"
#include "imodv_ogl.h"
#include "imodv_input.h"
#include "imodv_light.h"
#include "imodv_stereo.h"

/* local functions */
static int imodv_snapshot(ImodvApp *a, char *fname);
static void imodv_swapbuffers(ImodvApp *a);
static void imodv_clear(ImodvApp *a);


/* 12/15/02: removed DisplayHasAlpha */


/*
 *  Set the current OpenGL rendering context to the
 *  window being used by Imodv.
 */
int imodv_winset(ImodvApp *a)
{
  a->mainWin->mCurGLw->makeCurrent();
  return(1);
}

/*
 *  A swap buffer command (should not be needed)
 */
static void imodv_swapbuffers(ImodvApp *a)
{
  if (a->doPick) return;
  a->mainWin->mCurGLw->makeCurrent();
  
  if (a->db)
    a->mainWin->mCurGLw->swapBuffers();
  glFlush();
}
          
// Clear the window
static void imodv_clear(ImodvApp *a)
{
  a->mainWin->mCurGLw->makeCurrent();
  glClearColor(a->rbgcolor->red() / 256., a->rbgcolor->green() / 256.,
	       a->rbgcolor->blue() / 256., 1.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glFlush();
}

/* exchange double and single buffers */
void imodv_setbuffer(ImodvApp *a)
{
  /* DNM 9/2/02: skip if one of the visuals didn't exist */
  if (a->enableDepthSB < 0 || a->enableDepthDB < 0)
    return;

  // Also skip if we are in hardware stereo and the other buffer won't support
  if (a->stereo == IMODV_STEREO_HW && 
      (a->db && !a->stereoSB || !a->db && !a->stereoDB))
    return;

  imodv_clear(a);

  a->db = 1 - a->db;
  a->mainWin->setCheckableItem(VVIEW_MENU_DB, a->db);
  if (a->mainWin->setGLWidget(a->db))
    return;

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
    fprintf(stderr, "imodv Initialize GL\n");
  float rad = Imodv->imod->view->rad*1.5f;
  glClearColor(1.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
  glFogi(GL_FOG_MODE, GL_LINEAR);
  glFogf(GL_FOG_START,  0.0f);
  glFogf(GL_FOG_END, rad);
  if (!Imodv->db)
      glEnable(GL_LINE_SMOOTH);
  light_init();

}

// Resize of window
void imodvResizeGL(ImodvGL *GLw, int winx, int winy)
{
  ImodvApp *a = Imodv;
  if (Imod_debug)
    fprintf(stderr, "imodv Resize GL %d %d\n", winx, winy);
  if (GLw != a->mainWin->mCurGLw)
    return;
  a->winx = winx;
  a->winy = winy;
  glViewport(0, 0, a->winx, a->winy); 
}  

// The central draw entry: just call to update through the paint routine
void imodvDraw(ImodvApp *a)
{
  a->mainWin->mCurGLw->updateGL();
}

// The paint routine called by the widget
void imodvPaintGL()
{
  ImodvApp *a = Imodv;
  static int first = 1;
  static int drawcount = 0;

  //if (Imod_debug)
  //  fprintf(stderr, "drawing %d\n", drawcount++);

  if (!a->imod)
    return;

  if (!a->doPick)
    imodv_winset(a);

  if (a->dlist && a->update_dlist){
    if (first)
      first = 0;
    else
      glDeleteLists(1,1);
    glNewList(1, GL_COMPILE);
    imodvDraw_model(a, a->imod);
    glEndList();
    a->update_dlist = 0;
  }

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
    a->stereo *= -1;
    stereoDrawBuffer(GL_BACK_RIGHT);
    stereoClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    imodvDraw_models(a);

    a->stereo *= -1;
    stereoDrawBuffer(GL_BACK_LEFT);
    stereoClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    imodvDraw_models(a);
    break;


  case IMODV_STEREO_OFF:
    if (a->dlist)
      glCallList(1);
    else
      imodvDraw_models(a);
    break;
  }

  //  imodv_swapbuffers(a);
  imodvControlSetView(a);
  imodvControlUpdate(a);
  //  imodvCallDrawCB(IMODV_DRAWCB_UNKNOWN);
  return;
}


/****************************************************************************/
/****************************************************************************/
/**                        Image Snapshot Utilities                        **/
/****************************************************************************/
/****************************************************************************/

static int snap_fileno = 0;

void imodvResetSnap()
{
  snap_fileno = 0;
}

int imodv_auto_snapshot(char *inName, int format_type)
{
  ImodvApp *a = Imodv;
  char fname[32];
  char *usename = inName;
  char *fext = "rgb";

  if (format_type == SnapShot_TIF)
    fext = "tif";

  if (!inName) {
    usename = fname;
    if (snap_fileno < 10000)
      sprintf(fname, "modv%04d.%s", snap_fileno, fext);
    else
      sprintf(fname, "modv%d.%s", snap_fileno, fext);

    snap_fileno += 1;
  }

  printf("3dmodv: Saving image to %s", usename);
  fflush(stdout);


  if (a->db)
    a->mainWin->mCurGLw->setBufferSwapAuto(false);
  glReadBuffer(a->db ? GL_BACK : GL_FRONT);

  if (format_type == SnapShot_TIF) {
    imodvDraw(Imodv);
    b3dSetCurSize(Imodv->winx, Imodv->winy);
    b3dSnapshot_TIF(usename, 1, NULL, NULL);
    printf(".\n");
  } else {
    if (imodv_snapshot(Imodv, usename))
      printf(" : error writing file\n");
    else
      printf(".\n");
  }

  if (a->db) {
    imodv_swapbuffers(a);
    a->mainWin->mCurGLw->setBufferSwapAuto(true);
  }

  if (inName)
    free(inName);
  return(0);
}

static int imodv_snapshot(ImodvApp *a, char *fname)
{
  FILE *fout;
  unsigned char *pixels;
  int width, height;
  int i, xysize;
  GLint xoffset;
  char iname[80];

  fout = fopen((QDir::convertSeparators(QString(fname))).latin1(), "wb");
  if (!fout){
    perror("3dmodv: error opening file ");
    return(-1);
  }

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
  sprintf(iname, "%s, Created by 3dmodv.", fname);
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

/*
$Log$
Revision 4.3  2003/02/27 17:39:24  mast
Convert filenames with Qt routines

Revision 4.2  2003/02/21 23:21:41  mast
Open snapshot file in binary mode

Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.8  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.7  2003/01/01 19:12:31  mast
changes to start Qt application in standalone mode

Revision 1.1.2.6  2003/01/01 05:46:29  mast
changes for qt version of stereo

Revision 1.1.2.5  2002/12/23 04:57:07  mast
Defer swapping buffers when taking a snapshot

Revision 1.1.2.4  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.3  2002/12/17 22:28:21  mast
cleanup of unused variables and SGI errors

Revision 1.1.2.2  2002/12/17 17:39:52  mast
Qt version

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.4  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.3  2002/09/04 00:25:34  mast
Pass GLw the visuals that have been chosen already.  Rationalize single
buffer versus double code a bit.

Revision 3.2  2002/06/20 00:39:09  mast
Making GLw use that visual didn't work under Linux, remove the change

Revision 3.1  2002/06/20 00:26:58  mast
Force GLw to use the already chosen visual when getting a drawing area

*/

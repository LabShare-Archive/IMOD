
/*  IMOD VERSION 2.50
 *
 *  xzap.c -- The Zap Window.
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
    Revision 3.6  2002/10/22 22:41:47  mast
    Changed some debug messages for the expose timeouts

    Revision 3.5  2002/09/13 21:03:58  mast
    Changes to minimize crashes with Ti4600 when resizing window with R -
    elimination of interfering draws, and postpone of draw after expose events

    Revision 3.4  2002/07/28 22:58:42  mast
    Made I pop Info window to front and added a button to toolbar to do this

    Revision 3.3  2002/07/21 20:29:50  mast
    changed number of columns for section number to 4

    Revision 3.2  2002/01/28 16:53:59  mast
    Added section number to call to b3dDrawGreyScalePixelsHQ

    Revision 3.1  2001/12/17 18:52:40  mast
    Added hotkeys to do smoothing and next section in autocontouring

*/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/PushB.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <Xm/DialogS.h>
#include <X11/cursorfont.h>
#include <X11/IntrinsicP.h>
#include <math.h>

#ifdef REPORT_TIMES
#include <sys/times.h>
#include <time.h>
#endif

#include <diaP.h>
#include "imod.h"
#include "xzap.h"
#include "keypad.h"

#define zap_button_width  16
#define zap_button_height 16
/* #define XZAP_DEBUG */

static void zapClose(ZapWindow *zap);
static int zapDraw(ZapWindow *zap);

static void zapButton1(struct zapwin *zap, int x, int y);
static void zapButton2(struct zapwin *zap, int x, int y);
static void zapButton3(struct zapwin *zap, int x, int y);
static void zapB1Drag(struct zapwin *zap, int x, int y);
static void zapB2Drag(struct zapwin *zap, int x, int y);
static void zapB3Drag(struct zapwin *zap, int x, int y);
static int  zapDrawGraphics(struct zapwin *zap);
static void zapDrawModel(struct zapwin *zap);
static void zapDrawContour(struct zapwin *zap, int co, int ob);
static void zapDrawCurrentContour(struct zapwin *zap, int co, int ob);
static void zapDrawCurrentPoint(struct zapwin *zap, int undraw);
static int  zapDrawAuto(struct zapwin *zap);
static void zapDrawGhost(struct zapwin *zap);
static void zapDrawTools(ZapWindow *zap);
static void zapSetCursor(ZapWindow *zap, int mode);
static int  zapXpos(struct zapwin *zap, double x);
static int  zapYpos(struct zapwin *zap, double x);
static void zapGetixy(struct zapwin *zap, int mx, int my, float *x, float *y);
static int  zapPointVisable(struct zapwin *zap, Ipoint *pnt);
void zapAutoTranslate(struct zapwin *zap);
static void zapSyncImage(ZapWindow *win);
static void zap_quit_cb(Widget w, XtPointer client, XtPointer call);
static void zapPrintInfo(ZapWindow *zap);
static void zapResizeToFit(ZapWindow *zap);

static unsigned char lowres_bits[] = {
     0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0x0f, 0x0f, 0x0f, 0x0f,
     0x0f, 0x0f, 0x0f, 0x0f, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0,
     0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f};

static unsigned char highres_bits[] = {
     0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33, 0xcc, 0xcc, 0xcc, 0xcc,
     0x33, 0x33, 0x33, 0x33, 0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33,
     0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33};

static unsigned char zlock_bits[] = {
     0xf8, 0x0f, 0xfc, 0x1f, 0x1e, 0x3c, 0x0f, 0x78, 0x07, 0x70, 0x07, 0x70,
     0x07, 0x70, 0xff, 0x7f, 0xff, 0x7f, 0x0f, 0x70, 0xff, 0x73, 0xff, 0x7c,
     0x3f, 0x7f, 0xcf, 0x7f, 0x0f, 0x70, 0xff, 0x7f};

static unsigned char lock_bits[] = {
     0xf8, 0x0f, 0x0c, 0x18, 0x06, 0x30, 0x03, 0x60, 0x03, 0x60, 0x03, 0x60,
     0x03, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f, 0xff, 0x7f};

static unsigned char unlock_bits[] = {
     0xf8, 0x0f, 0x08, 0x18, 0x00, 0x30, 0x00, 0x60, 0x00, 0x60, 0x00, 0x60,
     0x00, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f, 0xff, 0x7f};

static unsigned char insertAfterBits[] = {
     0x00, 0x00, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0x80, 0x01,
     0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
     0x80, 0x01, 0x80, 0x01, 0xff, 0xff, 0xff, 0xff};

static unsigned char insertBeforeBits[] = {
     0xff, 0xff, 0xff, 0xff, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
     0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0xc0, 0x03,
     0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0x00, 0x00};

static unsigned char time_unlock_bits[] = {
     0xf8, 0x0f, 0x08, 0x18, 0x00, 0x30, 0x00, 0x60, 0x00, 0x60, 0x00, 0x60,
     0x00, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0x0f, 0x78, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f};

static unsigned char time_lock_bits[] = {
     0xf8, 0x0f, 0x0c, 0x18, 0x06, 0x30, 0x03, 0x60, 0x03, 0x60, 0x03, 0x60,
     0x03, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0x0f, 0x78, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f};

static unsigned char keepcenter_bits[] = {
   0xff, 0xff, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
   0x01, 0x80, 0x81, 0x81, 0x81, 0x81, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
   0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0xff, 0xff};

static unsigned char smartcenter_bits[] = {
   0xff, 0xff, 0x01, 0x80, 0x01, 0x80, 0xf9, 0x9f, 0x09, 0x90, 0x09, 0x90,
   0x09, 0x90, 0x09, 0x90, 0x09, 0x90, 0x09, 0x90, 0x09, 0x90, 0x09, 0x90,
   0xf9, 0x9f, 0x01, 0x80, 0x01, 0x80, 0xff, 0xff};


static  unsigned char test_bits[] = {
     0xff, 0xff, 0xff, 0xff, 0x03, 0x00, 0x00, 0xc0, 0x05, 0x00, 0x00, 0xa0,
     0x09, 0x00, 0x00, 0x90, 0x11, 0x00, 0x00, 0x88, 0x21, 0x00, 0x00, 0x84,
     0x41, 0x00, 0x00, 0x82, 0x81, 0x00, 0x00, 0x81, 0x01, 0x01, 0x80, 0x80,
     0x01, 0x02, 0x40, 0x80, 0x01, 0x04, 0x20, 0x80, 0x01, 0x08, 0x10, 0x80,
     0x01, 0x10, 0x08, 0x80, 0x01, 0x20, 0x04, 0x80, 0x01, 0x40, 0x02, 0x80,
     0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x40, 0x02, 0x80,
     0x01, 0x20, 0x04, 0x80, 0x01, 0x10, 0x08, 0x80, 0x01, 0x08, 0x10, 0x80,
     0x01, 0x04, 0x20, 0x80, 0x01, 0x02, 0x40, 0x80, 0xff, 0xff, 0xff, 0xff,
     0x01, 0x00, 0x00, 0x80, 0x01, 0x77, 0x77, 0x80, 0x01, 0x12, 0x21, 0x80,
     0x7d, 0x32, 0x27, 0xbf, 0x01, 0x12, 0x24, 0x80, 0x01, 0x72, 0x27, 0x80,
     0x01, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff};

#ifdef DRAW_GL
static GLXconfig glxZaPConfig [] = {
     { GLX_NORMAL, GLX_DOUBLE, TRUE},
     { GLX_OVERLAY, GLX_BUFSIZE, 2},
     { 0, 0, 0 }
};

String Zap_translations =
"<KeyDown>:   glxInput()\n\
 <BtnDown>:   glxInput()\n\
 <BtnMotion>: glxInput()\n\
 <Btn1Up>:    glxInput()\n\
";
#endif

#ifdef DRAW_X11

String Zap_translations =
"<KeyDown>: DrawingAreaInput()\n\
 <BtnDown>: DrawingAreaInput()\n\
 <Btn1Up>:  DrawingAreaInput()\n\
 <BtnMotion>: DrawingAreaInput()\n\
";
#endif

/* DNM 1/19/01: add KeyUp to allow insert key up to be detected */
#ifdef DRAW_OpenGL
String Zap_translations =
"<KeyDown>:   glwInput()\n\
 <KeyUp>:     glwInput()\n\
 <BtnDown>:   glwInput()\n\
 <BtnMotion>: glwInput()\n\
 <Btn1Up>:    glwInput()\n\
 <Btn2Up>:    glwInput()\n\
 <EnterNotify>: glwInput()\n\
";
#endif

static String KeyTranslations = "<KeyDown>: zapKeyInput()\n";
static void zap_keyinput(XKeyEvent *event, struct zapwin *zap);

/* DNM 1/19/01: add this to allow key to substitute for middle mouse button */
static int insertDown = 0;

/* DNM 3/9/01: add this to provide gloabl lock on movie snapshot use */
static int movieSnapLock = 0;

/* DNM: if there are problems with Mesa (?) that require use of GLw widgets 
   instead of GLwM, set traversals from text boxes and other input to a widget
   that performs key translations correctly (insertButton) */

#ifdef DRAW_GL
/**************************** stubs ***************************/
int imod_zap_input(Device dev, short val){return(0);}
void imod_zap_ortho(void){return;}
int imod_zap_clear(struct ViewInfo *vi){return(0);}
long Zap_window = -1;
/**************************************************************/
#endif

static void help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg
	  ("Imod Zap Help\n",
	   "---------------------------------------------------------------\n",
	   "The Tool Bar\n\n",
	   "\tThe Up and Down Arrows step the zoom factor up or down.\n",
	   "\tThe Zoom edit box shows the current zoom factor and allows ",
	   "one to type in a custom zoom.\n",
	   "\tThe checkerboard button toggles between fast rendering and ",
	   "slower but higher quality image rendering.\n",
	   "\tThe lock button can lock out movements within the Zap window ",
	   "and prevent centering on the current model point.\n",
	   "\tThe centering button toggles between having the image always ",
	   "centered ",
	   "on the current model point, and recentering the image only when ",
	   "the current point comes near an edge (the default).\n"
	   "\tThe modeling direction button toggles between inserting new ",
	   "points after the current point (when pointing up) or before ",
	   "the current point (when pointing down).\n"
	   "\tThe section edit box shows the current section and allows one ",
	   "to go directly to a section by typing in a number.\n",
	   "\tThe Info button (I) brings the Information Window to the "
	   "front and prints information about window and image size (see the "
	   "I Hot Key below).\n"
	   "\tIf multiple image files have been loaded into Imod, three "
	   "additional controls appear.  The Time Lock button will prevent "
	   "changes in other windows  from changing the time (image file) "
	   "displayed in this Zap window.  The left and right arrows will "
	   "step backward and forward in time.\n"
	   "\tPress the space bar to hide or show the tool bar.\n\n",
	   "---------------------------------------------------------------\n",
	   "\nHot Keys special to the Zap window\n\n"
	   "\ti toggles the modeling direction.\n",
	   "\tS or Ctrl-S saves the Zap window, or the area inside the "
	   "rubber band, into an RGB or TIFF file.\n"
	   "\tZ toggles auto section advance on and off.  When this is on, ",
	   "the section will change automatically after inserting a point if ",
	   "there was a section change between that point and the previous ",
	   "point.\n",
	   "\tb builds a contour when AutoContour window is open.\n",
	   "\ta advances to and fills next section when auto contouring.\n",
	   "\tu smooths a filled area when auto contouring.\n",
	   "\tB toggles the rubber band on and off.  The rubber band can be "
	   "used to select an area, then snapshot the area, resize the window "
	   "to that area, or find its coordinates.  The size of the band can "
	   "be adjusted by placing the pointer near an edge or corner and "
	   "dragging with the left mouse button.  The band can be moved as a "
	   "unit by placing the pointer near an edge and dragging with the "
	   "middle mouse button.\n",
	   "\tI prints information about the window and image size and "
	   "the coordinates of the image in the window.  If the rubber band "
	   "is on, the sizes and coordinates are relative to the rubber band "
	   "rather than the window.  The image "
	   "coordinates of the lower left and upper right corners, and of "
	   "the center, are printed in the Imod info window.  There is also "
	   "a fragment of a command line for extracting the image from the "
	   "stack with \"newst\".  This key also brings the Information "
	   "Window to the front of the display.\n",
	   "\tR resizes the window.  With the rubber band off, the window "
	   "changes, "
	   "if possible, to match the size of the entire image at the "
	   "current zoom.  With the rubber band on, it changes to the size "
	   "of the rubber band and the image is shifted to display the area "
	   "previously in the rubber band.\n",
	   "\tArrow keys and the keypad: In movie mode, the arrow keys and "
	   "the PageUp and PageDown keys move the current viewing point (the "
	   "small cross), while the keypad keys pan the image horizontally,"
	   " vertically, and diagonally.  In model mode, the arrow keys pan "
	   "the image, the numeric keypad arrows move the current model point "
	   "laterally, and the numeric keypad PageUp and PageDown keys move "
	   "the current model point in Z.\n"
	   "\tIns on the keypad: In movie mode, this key works the same as "
	   "the middle mouse button.  A single keystrike adds one point; "
	   "holding the key down allows points to be added continuously.\n"
	   "\tESC will close the Zap window.\n\n"
	   "For other keys, see Help - Hot Keys in the Imod Info Window.\n\n",
	   "---------------------------------------------------------------\n",
	   "\nMouse button function in movie mode\n\n",
	   "\tLeft Button Click: Select the current viewing point, marked by "
	   "a small cross.\n",
	   "\tLeft Button Drag: Pan the image if it is larger than the "
	   "window, or adjust the size of the rubber band."
	   "\n"
	   "\tMiddle Button: Start movie in forward direction, or stop movie."
	   "\n"
	   "\tMiddle Button Drag: Move the rubber band.\n",
	   "\tRight Button: Start movie in backward direction, or stop movie."
	   "\n\n"
	   "Mouse button function in model mode\n\n",
	   "\tLeft Button Click: Make the nearest model point be the current "
	   "model point.  If there is no point nearby, this detaches from the "
	   "current point and contour and selects a current viewing point "
	   "instead.\n",
	   "\tLeft Button Drag: Pan the image if it is larger than the "
	   "window, or adjust the size of the rubber band."
	   "\n"
	   "\tMiddle Button Click: Add one point to the current contour.\n"
	   "\tMiddle Button Drag: Continually add points to the current "
	   "contour as the mouse is moved, or move the rubber band.\n",
	   "\tRight Button Click: Modify the current model point to be at the "
	   "selected position.\n",
	   "\tRight Button Drag: Continually modify points as the mouse is "
	   "moved.  This only works when the current model point is in the "
	   "interior of the contour, not at its end.\n",
	   "\tCtrl - Right Button Click: Delete any points under the cursor "
	   "in the current contour.\n",
	   "\tCtrl - Right Button Drag: Continually delete points under the "
	   "cursor as the mouse is moved.  At the end, the current point is "
	   "set before the last deletion (or after, if modeling direction is "
	   "inverted.)\n",
	   NULL);
     return;
}


static void zapClose(ZapWindow *zap)
{
     if (!zap) return;

     b3dWinset(XtDisplay(zap->dialog), 0, 0);
     XtPopdown(zap->dialog);
     zap->popup = False;

#ifdef DRAW_X11
     if (zap->image)
	  XDestroyImage(zap->image);
#else
     if (zap->image)
	  free(zap->image);
#endif
     zap->ctrl  = 0;
     zap->image = NULL;
     zap->winx  = zap->winy = 0;
     if (movieSnapLock && zap->movieSnapCount)
	  movieSnapLock = 0;

     imod_info_input();     
     XtDestroyWidget(zap->dialog);
/*     zap->vi->zap = 0; */
     free(zap);
#ifdef XZAP_DEBUG
puts("Zap Killed.");
#endif

}

void zapClose_cb(ImodView *vi, void *client, int junk)
{
     zapClose((ZapWindow *)client);
}

/* DNM 3/8/01: check for whether to start a movie snapshot sequence, and 
   manage the lock that keeps more than one window from starting or stopping
   a snap sequence */
static void checkMovieSnap(ZapWindow *zap, int dir)
{
     int start, end;

     /* If the lock is set and this window is the owner, clear the lock */
     if (movieSnapLock && zap->movieSnapCount) {
	  zap->movieSnapCount = 0;
	  movieSnapLock = 0;
     }

     /* done if no movie, or if the lock is still set, or if no snapshots are
	desired.  I.E., don't let another window start a sequence if one
	was already going */
     if (!zap->vi->zmovie || movieSnapLock || !imcGetSnapshot(zap->vi))
	  return;
     
     /* Get start and end of loop, compute count */
     imcGetStartEnd(zap->vi, 2, &start, &end);
     zap->movieSnapCount = (end - start) / imcGetIncrement(zap->vi, 2) + 1;
     if (zap->movieSnapCount < 1)
	  zap->movieSnapCount = 1;

     /* double count for normal mode, leave as is for one-way */
     if (!imcGetLoopMode(zap->vi))
	  zap->movieSnapCount *= 2;

     /* Set to start or end depending on which button was hit */
     if (dir > 0)
	  zap->vi->zmouse = start;
     else
	  zap->vi->zmouse = end;

     /* set the lock and draw */
     movieSnapLock = 1;
     zapDraw_cb(zap->vi, zap, IMOD_DRAW_XYZ);
}

void zapDraw_cb(ImodView *vi, void *client, int drawflag)
{
     struct zapwin *zap = (struct zapwin *)client;
     int *limits;
     int limarr[4];

#ifdef XZAP_DEBUG
puts("Zap Draw");
#endif

     if (!zap) return;
     if ((!zap->popup) || (!zap->ginit)) return;
     
     zapSetCursor(zap, vi->imod->mousemode);

     zapDrawTools(zap);

     if (drawflag){
	  if (drawflag & IMOD_DRAW_SLICE)
	       zap->showslice = 1;

	  /* DNM: skip this, it is covered by the zapdraw call below and the
	     items that it sets are not needed by the flush or sync */
	  /* b3dWinset(XtDisplay(zap->gfx), zap->gfx, (XID)zap->context); */

	  if (drawflag & IMOD_DRAW_IMAGE){
	       b3dFlushImage(zap->image);
	  }
	  
	  if (!(drawflag & IMOD_DRAW_ACTIVE) && !(drawflag & IMOD_DRAW_NOSYNC))
	       zapSyncImage(zap);

	  /* DNM: replace multiple calls with one call to internal drawing 
	     routine */
	  if (zapDraw(zap))
	       return;

	  /* DNM 3/8/01: add autosnapshot when movieing */
	  if (imcGetSnapshot(zap->vi) && zap->vi->zmovie && 
	      zap->movieSnapCount) {
	       limits = NULL;
	       if (zap->rubberband) {
		    limits = limarr;
		    limarr[0] = zap->bandllx + 1;
		    limarr[1] = zap->winy - zap->bandury;
		    limarr[2] = zap->bandurx - 1 - zap->bandllx;
		    limarr[3] = zap->bandury - 1 - zap->bandlly;
	       }
	       if (imcGetSnapshot(zap->vi) == 1)
		    b3dAutoSnapshot("zap", SnapShot_RGB, limits);
	       else
		    b3dAutoSnapshot("zap", SnapShot_TIF, limits);
	       zap->movieSnapCount--;
	       /* When count expires, stop movie and clear the lock */
	       if(!zap->movieSnapCount) {
		    zap->vi->zmovie = 0;
		    movieSnapLock = 0;
	       }
	  }
     }
     return;
}

/*
 *  Sync the pan position to the current model point. 
 */
#define BORDER_FRAC  0.1
#define BORDER_MIN  50
#define BORDER_MAX  125

static void zapSyncImage(ZapWindow *win)
{
     int syncborder, wposition, wsize, tripshift;
     int trytrans, trydraws, tryborder, trystart;
     ImodView *vi = win->vi;
     if ((!win->lock) && (vi->imod->mousemode == IMOD_MMODEL)){
	  if (win->vi->imod->cindex.point >= 0){

	       /* If the keepcentered flag is set, just do a shift to center */
	       if (win->keepcentered)
	            tripshift = 1;
	       else {

		 /* Otherwise, look at each axis independently.  First see if
		    if the position is within the borders for shifting */
		 tripshift = 0;
		 wsize = win->winx;
		 wposition = zapXpos(win, (double)vi->xmouse);
		 syncborder = wsize * BORDER_FRAC;
		 if (syncborder < BORDER_MIN)
		      syncborder = BORDER_MIN;
		 if (syncborder > BORDER_MAX)
		      syncborder = BORDER_MAX;
		 if (wposition < syncborder || wposition > wsize - syncborder){

		      /* If close to a border, do ani mage offset computation
			 to see if the display would actually get moved if
			 this axis were centered on point */
		      trytrans = (vi->xsize * 0.5f) - vi->xmouse + 0.5f;
		      trydraws = win->xdrawsize;
		      tryborder = win->xborder;
		      trystart = win->xstart;
		      /* printf ("before %d %d %d %d\n", 
			      trydraws, win->xtrans, tryborder, trystart); */
		      b3dSetImageOffset(wsize, vi->xsize, win->zoom, &trydraws,
					&trytrans, &tryborder, &trystart);
		     /* printf ("after %d %d %d %d\n", 
			      trydraws, trytrans, tryborder, trystart); */
		      /* Can't use xtrans for a test, need to use the other
			 two values to see if change in display would occur */
		      if (tryborder != win->xborder || trystart != win->xstart)
			   tripshift += 1;

		 }

		 /* Same for Y axis */
		 wsize = win->winy;
		 wposition = zapYpos(win, (double)vi->ymouse);
		 syncborder = wsize * BORDER_FRAC;
		 if (syncborder < BORDER_MIN)
		     syncborder = BORDER_MIN;
		 if (syncborder > BORDER_MAX)
		      syncborder = BORDER_MAX;
		 if (wposition < syncborder || wposition > wsize - syncborder){
		      trytrans = (vi->ysize * 0.5f) - vi->ymouse + 0.5f;
		      trydraws = win->ydrawsize;
		      tryborder = win->yborder;
		      trystart = win->ystart;
		      b3dSetImageOffset(wsize, vi->ysize, win->zoom, &trydraws,
					&trytrans, &tryborder, &trystart);
		      if (tryborder != win->yborder || trystart != win->ystart)
			   tripshift += 2;
		 }
	    }

	    if (tripshift) {
	         /* fprintf(stderr, "tripshift %d\n",tripshift); */
	         win->xtrans = (vi->xsize * 0.5f) - vi->xmouse + 0.5f;
	         win->ytrans = (vi->ysize * 0.5f) - vi->ymouse + 0.5f;
	    }
	  }
     }
}

/* DNM 6/22/01: change it to return int so that it can return 1 if no drawing
   is done, to avoid double snapshots */
static int zapDraw(ZapWindow *zap)
{

     /* DNM 9/10/02: skip a draw if expose timeout is active or a resize
	was started and not finished yet */
#ifdef ZAP_EXPOSE_HACK
     if (zap->exposeTimeOut || zap->resizeSkipDraw) {
#ifdef XZAP_DEBUG
	  fprintf(stderr, "Skipping a draw because of expose timeout\n");
#endif
	  return;
     }
#endif

     b3dWinset(XtDisplay(zap->gfx), zap->gfx, (XID)zap->context);

     /* But this wait is also needed to prevent the crashes */
     glXWaitX();
     zapAutoTranslate(zap);

     /* DNM: this call returns 1 if further drawing can be skipped */
     if (zapDrawGraphics(zap))
	  return 1;

     zapDrawModel(zap);
     zapDrawCurrentPoint(zap, False);
     zapDrawAuto(zap);
     if (zap->rubberband) {
	  b3dColorIndex(App->endpoint);
	  b3dDrawRectangle(zap->bandllx, zap->winy - 1 - zap->bandury, 
			   zap->bandurx - zap->bandllx, 
			   zap->bandury - zap->bandlly);
     } 
     zapDrawTools(zap);
     b3dSwapBuffers();
     return 0;
}


static void zap_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     ivwDeleteControl(zap->vi, zap->ctrl);

#ifdef XZAP_DEBUG
     fprintf(stderr, "Zap Control deleted\n.");
#endif
     return;
}

static void setzoom(struct zapwin *zap, double amount, int ref)
{

     zap->zoom = b3dStepPixelZoom(zap->zoom, (int)amount);
     zapDrawTools(zap);
     return;
}

static void setview(struct zapwin *zap)
{
     b3dWinset(App->display, zap->gfx, zap->context);
}

void zapKeyInput(Widget w, XEvent *event, String par, Cardinal num)
{
     struct zapwin *zap;
     XtVaGetValues(w, XmNuserData, &zap, NULL);
     zap_keyinput((XKeyEvent *)event, zap);
     return;
}

void zap_resize_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     Dimension winx, winy;
     int bandmin = 4;

     ivwControlPriority(zap->vi, zap->ctrl);

#ifdef XZAP_DEBUG
     fprintf(stderr, "RESIZE: ");
#endif

     /* DNM 8/10/01: Needed on RH 7.1/GeForce3 to prevent garbage */
#ifdef ZAP_RESIZE_HACK
     zap->resizedraw2x = 1;
#endif
#ifdef ZAP_EXPOSE_HACK
     zap->resizeSkipDraw = 1;
#endif

     XtVaGetValues(zap->gfx, XmNwidth, &winx, XmNheight, &winy, NULL);

#ifdef XZAP_DEBUG
     fprintf(stderr, "Size = %d x %d :", winx, winy);
#endif

#ifdef XZAP_DEBUG
     fprintf(stderr, "Old Size = %d x %d :", zap->winx, zap->winy);
#endif
     zap->winx = winx;
     zap->winy = winy;
 

     if (zap->ginit){
#ifdef XZAP_DEBUG
	  fprintf(stderr, "Init!");
#endif
	  /* Make sure the rubber band stays legal, but keep it same size
	   if possible */
	  if (zap->rubberband) {
	       if (zap->bandurx >= winx) {
		    if (zap->bandurx + 1 - zap->bandllx > winx) {
			 zap->bandurx = winx - 1;
			 if (zap->bandllx > zap->bandurx - bandmin)
			      zap->bandllx = zap->bandurx - bandmin;
		    } else {
			 zap->bandllx -= zap->bandurx + 1 - winx;
			 zap->bandurx = winx - 1;
		    }
	       }
	       if (zap->bandury >= winy) {
		    if (zap->bandury + 1 - zap->bandlly > winy) {
			 zap->bandury = winy - 1;
			 if (zap->bandlly > zap->bandury - bandmin)
			      zap->bandlly = zap->bandury - bandmin;
		    } else {
			 zap->bandlly -= zap->bandury + 1 - winy;
			 zap->bandury = winy - 1;
		    }
	       }
	  }
	  b3dWinset(XtDisplay(zap->gfx), zap->gfx, zap->context);
	  b3dResizeViewport();
	  b3dFlushImage(zap->image);
	  zap->image =  b3dGetNewCIImage(zap->image, App->depth);
	  b3dBufferImage(zap->image);

	  /* DNM 9/10/02: this redraw is unneeded because expose events
	     are always generated, and it causes trouble on PCs */
	  /*fprintf(stderr, "drawing inside resize_cb...");
	  zapDraw(zap);
	     fprintf(stderr, "back\n"); */
	  /*  zapDrawTools(zap); */
     }
#ifdef XZAP_DEBUG
     fprintf(stderr, "\n");
#endif
     return;
}

void zap_ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     Dimension winx, winy;

     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     zap->winx = winx;
     zap->winy = winy;

#ifdef XZAP_DEBUG
     fprintf(stderr, "Ginit:");
#endif

     zap->context = b3dGetContext(zap->gfx);
     b3dWinset(XtDisplay(zap->gfx), zap->gfx, zap->context);
     b3dResizeViewport();

     zap->image   = b3dGetNewCIImage(zap->image, App->depth);
     b3dBufferImage(zap->image);
     b3dResizeViewport();
     return;
}

#ifdef DRAW_GL
void zap_overexpose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     return;
}
#endif

/* DNM 9/10/02: when the time out ends after the last expose event, finally 
   do the draws */
#ifdef ZAP_EXPOSE_HACK
void expose_to(XtPointer client_data, XtIntervalId *id)
{
     struct zapwin *zap = (struct zapwin *)client_data;
     zap->exposeTimeOut = (XtIntervalId)0;
     zap->resizeSkipDraw = 0;
#ifdef XZAP_DEBUG
     fprintf(stderr, "Drawing after expose timeout\n");
#endif
     zapDraw(zap);
     if (zap->resizedraw2x)
	  zapDraw(zap);
     zap->resizedraw2x = 0;
}
#endif     

void zap_expose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;
     Dimension winx, winy;
     int ox, oy;
     int inited = 0;
#ifdef REPORT_TIMES
     static clock_t lasttime, thistime;
     struct tms buf;
     float elapsed;
#endif
     unsigned int interval = 120;    /* The value from midas - could be less */

     /* This is a bust - there are multiple expose*/
     XExposeEvent *event = (XExposeEvent *)(cbs->event);
     if (event->count)
	  return;

#ifdef XZAP_DEBUG
     fprintf(stderr, "Expose:");
#endif

     /* Resize window */
     if (zap->ginit){
	  XtVaGetValues(zap->gfx, 
			XmNwidth, &winx, XmNheight, &winy, NULL);

#ifdef XZAP_DEBUG
	  fprintf(stderr, "Size = %d x %d :", winx, winy);
#endif
	  ox = zap->winx; oy = zap->winy;
	  zap->winx = winx;
	  zap->winy = winy;
	  b3dWinset(XtDisplay(zap->gfx), zap->gfx, zap->context);
	  
	  if ((winx != ox) || (winy != oy)){
#ifdef XZAP_DEBUG
	       fprintf(stderr, "Old Size = %d x %d :", zap->winx, zap->winy);
#endif
	       b3dFlushImage(zap->image);
	       zap->image =  b3dGetNewCIImage(zap->image, App->depth);
	       b3dBufferImage(zap->image);
	       /* 9/10/02:  THIS WAS NOT NEEDED */
	       /*  zapDraw(zap); */
	  }

	  b3dResizeViewport();

     }else{
	  zap_ginit_cb(w, client, call); 
	  zap->ginit = TRUE;
	  inited = 1;
     }

#ifdef REPORT_TIMES
     thistime = times(&buf);
     elapsed = 1000.*(float)(thistime - lasttime) / CLK_TCK;
     printf ("%6.1f\n", elapsed);
     lasttime = thistime;
#endif

     /* DNM 9/10/02: start a timeout after every expose event to avoid crashes
      with GeForce4 Ti4600 - possibly due to collisions with X's redraw of
      the window */
#ifdef ZAP_EXPOSE_HACK
     if (inited) {
#endif
	  zapDraw(zap);
	  if (zap->resizedraw2x)
	       zapDraw(zap);
	  zap->resizedraw2x = 0;
#ifdef ZAP_EXPOSE_HACK
     } else {
	  if (zap->exposeTimeOut) {
	       XtRemoveTimeOut(zap->exposeTimeOut);
#ifdef XZAP_DEBUG
	       fprintf(stderr, "Restarting expose timeout for %d", interval);
	  } else {
	       fprintf(stderr, "Starting an expose timeout for %d", interval);
#endif
	  }
	  zap->exposeTimeOut = XtAppAddTimeOut(Dia_context, interval, 
					       expose_to, (XtPointer)zap);
     }
#endif


#ifdef XZAP_DEBUG
     fprintf(stderr, "\n");
#endif
}


/*****************************************************************************/
/* zap tool bar callback functions.                                          */
static void hide_cb(struct zapwin *zap)
{
      Dimension width, height, border_width = 0;
      XtVaGetValues(zap->dialog, XmNwidth, &width, XmNheight, &height, NULL);

      ivwControlPriority(zap->vi, zap->ctrl);
      if (!zap->hide){
	   zap->hide = True;
	   XtVaSetValues(zap->gfx, XmNtopAttachment, XmATTACH_FORM, NULL);
	   XRaiseWindow(App->display, XtWindow(zap->gfx));
      }else{
	   zap->hide = False;
	   XtVaSetValues(zap->gfx, 
			 XmNtopAttachment, XmATTACH_WIDGET,
			 XmNtopWidget, zap->tools,
			 NULL);
	   XRaiseWindow(App->display, XtWindow(zap->tools));
      }
      return;
}

static void zoomup_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);
     setzoom(zap, 1.0, 1);
     zapDraw(zap);
     return;
}

static void zoomdown_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);     
     setzoom(zap, -1.0, 1);
     zapDraw(zap);
     return;
}

static void time_lock_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     int time;

     ivwGetTime(zap->vi, &time);
     if (zap->timeLock){
	 XtVaSetValues(w, XmNlabelPixmap, zap->timePix, NULL);
	 zap->timeLock = 0;
     }else{
	 zap->timeLock = time;
	 XtVaSetValues(w, XmNlabelPixmap, zap->timeLockPix, NULL);
     }

     zapDraw(zap);
}

static void lock_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     /* set lock = 2 to lock x,y and z.    */

     ivwControlPriority(zap->vi, zap->ctrl);
     if (zap->lock){
          zap->lock = 0;
	  XtVaSetValues(w, XmNlabelPixmap, zap->unlockpix, NULL);
	  b3dFlushImage(zap->image);
	  zapSyncImage(zap);
	  zapDraw(zap);
     }else{
	  zap->lock = 2;
	  XtVaSetValues(w, XmNlabelPixmap, zap->lockpix, NULL);
     }
     return;
}

/* DNM: 2.40 delete old version of lock_cb with 3 levels */


static void center_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);
     if (zap->keepcentered){
          zap->keepcentered = 0;
	  XtVaSetValues(w, XmNlabelPixmap, zap->smartcenpix, NULL);
     }else{
	  zap->keepcentered = 1;
	  XtVaSetValues(w, XmNlabelPixmap, zap->keepcenpix, NULL);
	  b3dFlushImage(zap->image);
	  zapSyncImage(zap);
	  zapDraw(zap);
     }
     return;
}

static void hq_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);
     if (zap->hqgfx){
	  zap->hqgfx = 0;
	  XtVaSetValues(w, XmNlabelPixmap, zap->lowrespix, NULL);
	  zapDraw(zap);
     }else{
	  zap->hqgfx = 1;
	  XtVaSetValues(w, XmNlabelPixmap, zap->highrespix, NULL);
	  zapDraw(zap);
     }

     return;
}

static void insert_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);
     if (zap->insertmode){
	  zap->insertmode = 0;
	  XtVaSetValues(w, XmNlabelPixmap, zap->insertAfterPix, NULL);
     }else{
	  zap->insertmode = 1;
	  XtVaSetValues(w, XmNlabelPixmap, zap->insertBeforePix, NULL);
     }
     zap->vi->insertmode = zap->insertmode;
     return;
}

/* DNm 7/28/02: new button to bring info window to top */
static void info_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);
     zapPrintInfo(zap);
}

static void seclabel_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     char *st = NULL;
     int sec;

     ivwControlPriority(zap->vi, zap->ctrl);
     st = XmTextGetString(w);
     sec = atoi(st);
     XtFree(st);
     if (zap->lock != 2)
	  zap->vi->zmouse = sec-1;
     zap->section = sec-1;
     ivwBindMouse(zap->vi);
     XmProcessTraversal(zap->gfx, XmTRAVERSE_CURRENT);
     imodDraw(zap->vi, IMOD_DRAW_XYZ);
}

static void zoomlabel_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     char *st = NULL;
     
     ivwControlPriority(zap->vi, zap->ctrl);
     st = XmTextGetString(w);
     zap->zoom = atof(st);
     XtFree(st);
     if (zap->zoom <= 0.01)
	  zap->zoom = 0.01;
     XmProcessTraversal(zap->gfx, XmTRAVERSE_CURRENT);
     zapDraw(zap);
}

static void timeprev_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     ivwControlPriority(zap->vi, zap->ctrl);
     if (zap->timeLock){
	 zap->timeLock--;
	 if (!zap->timeLock) zap->timeLock = 1;
	  zapDraw(zap);
     }else{
	 ivwControlPriority(zap->vi, zap->ctrl);
	 inputPrevTime(zap->vi);
     }
}

static void timenext_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;

     if (zap->timeLock){
	 zap->timeLock++;
	 if (zap->timeLock > ivwGetMaxTime(zap->vi))
	     zap->timeLock = ivwGetMaxTime(zap->vi);
	 zapDraw(zap);
     }else{
	 ivwControlPriority(zap->vi, zap->ctrl);
	 inputNextTime(zap->vi);
     }
}


/*****************************************************************************/

int imod_zap_open(struct ViewInfo *vi)
{
     XtTranslations transtable = XtParseTranslationTable(KeyTranslations);
     Widget form, frame;
     Widget tool_row, arrow, button;
     struct zapwin *zap;
     Atom   wmclose;
     Pixel  fg, bg;
     int    depth;
     char  *window_name;
     Screen *screen = XDefaultScreenOfDisplay(App->display);
     int time, tmax, len, maxlen;
     XmString xs1, xs2, tlbl;

/*
     if (vi->zap){
	  XRaiseWindow(App->display, XtWindow(vi->zap->dialog)); 
	  return(0);
     }
*/

     zap = (struct zapwin *)malloc(sizeof(struct zapwin));
     if (!zap) return(-1);

     vi->zap = NULL;

     zap->vi     = vi;
     zap->ctrl   = 0;
     /* DNM: setting max size of the topLevelShell didn't work on the PC, so
	let's just explicitly limit the size asked for in the image portion */
     zap->winx = WidthOfScreen(screen) - 20;
     zap->winy = HeightOfScreen(screen) - 76;
     if (vi->xsize < zap->winx)
	  zap->winx   = vi->xsize;
     if (vi->ysize < zap->winy)
	  zap->winy   = vi->ysize;
     /*     zap->xtrans = zap->vi->xsize / 2; */
/*     zap->ytrans = zap->vi->ysize / 2; */
     zap->xtrans = zap->ytrans = 0;
     zap->ztrans = 0;
     zap->hqgfx  = False;
     zap->hide   = False;
     zap->zoom   = 1.0;
     zap->data   = NULL;
     zap->image  = NULL;
     zap->ginit  = False;
     zap->lock   = False;
     zap->keepcentered = False;
     zap->context = 0;
     zap->insertmode = 0;
     zap->toolstart = 0;
     zap->showslice = 0;
     zap->timeLock = 0;
     zap->toolSection = -1;
     zap->toolZoom = 0.0f;
     zap->toolTime = 0;
     zap->twod = (!(vi->dim&4));
     zap->sectionStep = 0;
     zap->time = 0;
     zap->mousemode = 0;
     zap->rubberband = 0;
     zap->movieSnapCount = 0;
     zap->resizedraw2x = 0;
     zap->exposeTimeOut = (XtIntervalId)0;
     zap->resizeSkipDraw = 0;

     window_name = imodwfname("IMOD ZaP Window:");
     imod_info_input();

     zap->dialog = XtVaCreatePopupShell
	  ("ZaP", topLevelShellWidgetClass, App->toplevel,
	   XmNvisual,   App->visual,
	   XtNtitle,    window_name, 
/*	   XmNdeleteResponse, XmUNMAP, */
	   XmNuserData, (XtPointer)zap,
	   NULL);

     if (!zap->dialog){
	  free(zap);
	  vi->zap = NULL;
	  wprint("Error opening zap window.");
	  return(-1);
     }

#ifdef XZAP_DEBUG
puts("Got a zap dialog");
#endif

     if (window_name)
	  free(window_name);

     form = XtVaCreateManagedWidget
	  ("form", xmFormWidgetClass, zap->dialog,
	   NULL);

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass,  form,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNleftAttachment,   XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNshadowType, XmSHADOW_OUT,
	   NULL);
     zap->tools = frame;

     tool_row = XtVaCreateWidget
	  ("tools", xmRowColumnWidgetClass, frame,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     {
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonWidgetClass, tool_row,
		XmNarrowDirection, XmARROW_UP, 
		XmNuserData, (XtPointer)zap,
		NULL);
	  XtAddCallback(arrow, XmNhelpCallback, help_cb, (XtPointer)zap);
	  XtAddCallback(arrow, XmNarmCallback, zoomup_cb,
			(XtPointer)zap);
	  imodOverrideTranslations(arrow, transtable);

	  arrow = XtVaCreateManagedWidget
	       ("arrow_down", xmArrowButtonWidgetClass, tool_row,
		XmNarrowDirection, XmARROW_DOWN, 
		XmNuserData, (XtPointer)zap,
		NULL);
	  XtAddCallback(arrow, XmNarmCallback, zoomdown_cb,
			(XtPointer)zap);
	  imodOverrideTranslations(arrow, transtable);
	  XtVaGetValues(arrow,
			XmNforeground, &fg,
			XmNbackground, &bg,
			XmNdepth, &depth,
			NULL);

	  zap->zoomlabel = XtVaCreateManagedWidget
	       ("Zoom", xmTextWidgetClass, tool_row, 
		XmNcolumns, 4,
		XmNmarginHeight, 2,
		NULL);
	  XtAddCallback(zap->zoomlabel, XmNactivateCallback,
			zoomlabel_cb, (XtPointer)zap);

	  zap->lowrespix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), 
		(char *)lowres_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  zap->highrespix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)highres_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("HQ Graphics", xmPushButtonWidgetClass, tool_row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, zap->lowrespix,
		XmNindicatorOn, False,
		XmNuserData, (XtPointer)zap,
		XmNborderWidth, 0,
		XmNmarginWidth, 3, 
		NULL);
	  XtAddCallback(button, XmNactivateCallback, hq_cb, (XtPointer)zap);
	  imodOverrideTranslations(button, transtable);

	  zap->zlockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)zlock_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  zap->lockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)lock_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  zap->unlockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)unlock_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("tool_button", xmPushButtonWidgetClass, tool_row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, zap->unlockpix,
		XmNindicatorOn, False,
		XmNuserData, (XtPointer)zap,
		XmNmarginWidth, 3, 
		NULL);
	  XtAddCallback(button, XmNactivateCallback, lock_cb, (XtPointer)zap);
	  imodOverrideTranslations(button, transtable);
	  
	  zap->keepcenpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)keepcenter_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  zap->smartcenpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), 
		(char *)smartcenter_bits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("tool_button", xmPushButtonWidgetClass, tool_row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, zap->smartcenpix,
		XmNindicatorOn, False,
		XmNuserData, (XtPointer)zap,
		XmNmarginWidth, 3, 
		NULL);
	  XtAddCallback(button, XmNactivateCallback, center_cb,
			(XtPointer)zap);
	  imodOverrideTranslations(button, transtable);

	  zap->insertAfterPix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), 
		(char *)insertAfterBits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  zap->insertBeforePix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), 
		(char *)insertBeforeBits,
		zap_button_width, zap_button_height,
		fg, bg, depth);
	  zap->insertButton = XtVaCreateManagedWidget
	       ("tool_button", xmPushButtonWidgetClass, tool_row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, zap->insertAfterPix,
		XmNindicatorOn, False,
		XmNuserData, (XtPointer)zap,
		XmNmarginWidth, 3, 
		NULL);
	  XtAddCallback(zap->insertButton, XmNactivateCallback, insert_cb,
			(XtPointer)zap);
	  imodOverrideTranslations(zap->insertButton, transtable);
	  /* DNM: shouldn't this be insertBeforePix, not After? */
	  if (zap->insertmode){
	       XtVaSetValues(zap->insertButton, XmNlabelPixmap, 
			     zap->insertBeforePix, NULL);
	  }

	  button = XtVaCreateManagedWidget
	       ("section", xmLabelWidgetClass, tool_row, NULL);
	  
	  zap->seclabel = XtVaCreateManagedWidget
	       ("Section", xmTextWidgetClass, tool_row, 
		XmNcolumns, 4,
		XmNmarginHeight, 2,
		NULL);
	  XtAddCallback(zap->seclabel, XmNactivateCallback,
			seclabel_cb, (XtPointer)zap);

	  button = XtVaCreateManagedWidget
	       ("I", xmPushButtonWidgetClass, tool_row,
		XmNuserData, (XtPointer)zap, 
		XmNmarginWidth, 3, 
		NULL);
	  XtAddCallback(button, XmNactivateCallback, info_cb, (XtPointer)zap);
	  imodOverrideTranslations(button, transtable);

	  button = XtVaCreateManagedWidget
	       ("Help", xmPushButtonWidgetClass, tool_row,
		XmNuserData, (XtPointer)zap, 
		XmNmarginWidth, 3, 
		NULL);
	  XtAddCallback(button, XmNactivateCallback, help_cb, (XtPointer)zap);
	  imodOverrideTranslations(button, transtable);

	  /* Time data controls. */
	  if (vi->nt){

	       zap->timeLockPix = XCreatePixmapFromBitmapData
		    (App->display, XtWindow(App->toplevel), 
		     (char *)time_lock_bits,
		     zap_button_width, zap_button_height,
		     fg, bg, depth);
	       zap->timePix = XCreatePixmapFromBitmapData
		    (App->display, XtWindow(App->toplevel), 
		     (char *)time_unlock_bits,
		     zap_button_width, zap_button_height,
		     fg, bg, depth);
	       button = XtVaCreateManagedWidget
		    ("tool_button", xmPushButtonWidgetClass, tool_row,
		     XmNlabelType, XmPIXMAP,
		     XmNlabelPixmap, zap->timePix,
		     XmNindicatorOn, False,
		     XmNuserData, (XtPointer)zap,
		     XmNmarginWidth, 3, 
		     NULL);
	       XtAddCallback(button, XmNactivateCallback, 
			     time_lock_cb, (XtPointer)zap);
	       imodOverrideTranslations(button, transtable);
	       
	       XtVaCreateManagedWidget
		    ("4th D", xmLabelWidgetClass, tool_row, NULL);
	       arrow = XtVaCreateManagedWidget
		    ("arrow", xmArrowButtonWidgetClass, tool_row,
		     XmNarrowDirection, XmARROW_LEFT, 
		     XmNuserData, (XtPointer)zap,
		     NULL);
	       XtAddCallback(arrow, XmNhelpCallback, help_cb, (XtPointer)zap);
	       XtAddCallback(arrow, XmNarmCallback, timeprev_cb,
			     (XtPointer)zap);
	       imodOverrideTranslations(arrow, transtable);
	       
	       arrow = XtVaCreateManagedWidget
		    ("arrow_down", xmArrowButtonWidgetClass, tool_row,
		     XmNarrowDirection, XmARROW_RIGHT, 
		     XmNuserData, (XtPointer)zap,
		     NULL);
	       XtAddCallback(arrow, XmNarmCallback, timenext_cb,
			     (XtPointer)zap);
	       imodOverrideTranslations(arrow, transtable);

	       maxlen = -1;
	       for (time = 1; time < zap->vi->nt; time++) {
		    len = strlen(ivwGetTimeIndexLabel(zap->vi, time));
		    if (len > maxlen) {
			 maxlen = len;
			 tmax = time;
		    }
	       }

	       zap->timelabel = 
		    XtVaCreateManagedWidget
			 ("label", xmLabelWidgetClass, tool_row, NULL);
	       xs1 = XmStringCreateSimple
		    (ivwGetTimeIndexLabel(zap->vi, tmax));
	       xs2 = XmStringCreateSimple(" (999)");
	       tlbl = XmStringConcat(xs1, xs2);
	       XtVaSetValues(zap->timelabel, XmNlabelString,
			     tlbl, NULL);
	       XmStringFree(tlbl);
	       XmStringFree(xs1);
	       XmStringFree(xs2);
	       
	  }else{
	       zap->timelabel = 0;
	  }

     }
     XtManageChild(tool_row);
     XtManageChild(zap->tools);

     zap->gfx = XtVaCreateManagedWidget
	  ("gfx", B3dDrawingAreaWidgetClass, form,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNleftAttachment,   XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_WIDGET,
	   XmNtopWidget,        frame,
	   XmNwidth,            zap->winx,
	   XmNheight,           zap->winy,
	   XmNnavigationType,   XmNONE,
	   XmNtraversalOn,      True,
	   XmNtranslations,     XtParseTranslationTable (Zap_translations),
	   XmNcolormap,         App->cmapGL,
#ifdef DRAW_OpenGL
	   GLwNvisualInfo, App->visualinfoGL,
	   GLwNinstallBackground, False,
	   GLwNinstallColormap, False,
#endif
#ifdef DRAW_GL
	   GlxNglxConfig,       glxZaPConfig,
	   GlxNuseOverlay,      TRUE,
#endif
	   NULL);

     XtAddCallback(zap->gfx,B3dNexposeCallback, 
		   zap_expose_cb, (XtPointer)zap);
     XtAddCallback(zap->gfx,B3dNresizeCallback, 
		   zap_resize_cb, (XtPointer)zap);
     XtAddCallback(zap->gfx,B3dNinputCallback,  
		   zap_input_cb,  (XtPointer)zap);

#ifdef DRAW_GL
     XtAddCallback(zap->gfx, GlxNoverlayExposeCallback,
		   zap_overexpose_cb, (XtPointer)zap);
#endif

     XtVaSetValues(zap->gfx, XmNbackground, App->background, NULL);
     imodOverrideTranslations(zap->gfx,
			    XtParseTranslationTable(Zap_translations));

     wmclose = XmInternAtom( XtDisplay(zap->dialog),
			    "WM_DELETE_WINDOW", False);

     XmAddWMProtocolCallback(zap->dialog, wmclose, zap_quit_cb,
			     (caddr_t)zap);
     

/*     zap->cursor = XCreateFontCursor(App->display, XC_tcross); */
     zap->cursor = App->cursor_cross;


     zap->ctrl = ivwNewControl(vi, zapDraw_cb, zapClose_cb, 
			       (XtPointer)zap);
     /* DNM: this is the second call to this, which caused hanging when 
	imod_info_input tested on all events but dispatched only X events.
	With dispatching of all events, the call can be left here. */
     imod_info_input();
     XtPopup(zap->dialog, XtGrabNone);
     zap->popup = True;

#ifdef XZAP_DEBUG
puts("popup a zap dialog");
#endif

     /* DNM: set cursor after window created so it has model mode cursor if
	an existing window put us in model mode */
     zapSetCursor(zap, vi->imod->mousemode);
     return(0);
}

void zapGotoCurrentPoint(struct zapwin *zap)
{
     return;
}

void zapAutoTranslate(struct zapwin *zap)
{
     if (zap->lock == 2)
	  return;

     zap->section = (int)zap->vi->zmouse + 0.5f;
     
     zapDrawTools(zap);

     if (zap->lock)
	  return;

     return;
}

/* DNM: 2.40 deleted imod_zap_draw which was unused and confusing */


static void zapTranslate(struct zapwin *zap, int x, int y)
{
     ImodView *vw = zap->vi;
     zap->xtrans += x;
     if (zap->xtrans > vw->xsize)
	  zap->xtrans = vw->xsize;
     if (zap->xtrans < -vw->xsize)
	  zap->xtrans = - vw->xsize;
     zap->ytrans += y;
     if (zap->ytrans > vw->ysize)
	  zap->ytrans = vw->ysize;
     if (zap->ytrans < -vw->ysize)
	  zap->ytrans = - vw->ysize;
     zapDraw(zap);
     return;
}

/* DNM: change the key definitions that were #ifndef __sgi to #ifdef __vms
   since they seemed to work under Linux */

static void zap_keyinput(XKeyEvent *event, struct zapwin *zap)
{
     struct ViewInfo *vi = zap->vi;
     KeySym keysym;
     static int trans = 5;
     static Time insertTime = 0;
     int size;
     int *limits;
     int limarr[4];
     Window rootr, childr;
     int rx, ry, ix, iy;
     unsigned int maskr;

     ivwControlActive(vi, 0);
     if (imodPlugHandleKey(vi, event)) return;
     ivwControlActive(vi, 1);

     /* DNM: set global insertmode from this zap's mode to get it to work
	right with Delete key */
     vi->insertmode = zap->insertmode;
     keysym = XLookupKeysym(event, 0);

/*	fprintf(stderr, "Zapo got %x keysym\n", keysym); */


     switch(keysym){

	case XK_space:
	  hide_cb(zap);
	  break;
	
#ifdef __vms
	case XK_Up:
#endif  

	case XK_KP_Up:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, 0, trans);
	  else
	       inputPointMove(vi, 0, 1, 0);
	  break;
	
#ifdef __vms
	case XK_Down:
#endif
	case XK_KP_Down:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, 0, -trans);
	  else
	       inputPointMove(vi, 0, -1, 0);
	  break;

#ifdef __vms
	case XK_Right:
#endif
	case XK_KP_Right:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, trans, 0);
	  else
	       inputPointMove(vi, 1, 0, 0);
	  break;

#ifdef __vms
	case XK_Left:
#endif
	case XK_KP_Left:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, -trans, 0);
	  else
	       inputPointMove(vi, -1, 0, 0);
	  break;
	  
/* DNM: make semicolon and quote conditional; we sure don't need them */
#ifdef __vms
	case XK_semicolon:
#endif
	case XK_F35:
	case XK_KP_Next:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, trans, -trans);
	  else
	       inputPointMove(vi, 0, 0, -1);
	  break;
	  
#ifdef __vms
	case XK_quoteright:
	case XK_quoteleft:
#endif
	case XK_F29:
	case XK_KP_Prior:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, trans, trans);
	  else
	       inputPointMove(vi, 0, 0, 1);
	  break;
	  
	case XK_F27:
	case XK_KP_Home:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, -trans, trans);
	  break;

	case XK_F33:
	case XK_KP_End:
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       zapTranslate(zap, -trans, -trans);
	  break;
	  
	case XK_minus:
	  zap->zoom = b3dStepPixelZoom(zap->zoom, -1);
	  zapDraw(zap);
	  break;

	case XK_equal:
	  zap->zoom = b3dStepPixelZoom(zap->zoom, 1);
	  zapDraw(zap);
	  break;
	  
	  /* DNM: KP insert key, find mouse position, set flag, pass like 
	     middle mouse button */
	case XK_KP_Insert:
	  /* But skip out if in movie mode */
	  if (vi->imod->mousemode == IMOD_MMOVIE)
	       break;
	  XQueryPointer(XtDisplay(zap->gfx), XtWindow(zap->gfx),
			&rootr, &childr, &rx, &ry, &ix, &iy, &maskr);
	  insertDown = 1;
	  /* Use time since last event to determine whether to treat like
	     single click or drag */
	  rx = event->time - insertTime;
	  insertTime = event->time;
	  /* fprintf(stderr, " %d %d %d\n ", rx, ix, iy); */
	  if(rx > 250)
	       zapButton2(zap, ix, iy);
	  else
	       zapB2Drag(zap, ix, iy); 
	  zap->lmx = ix;
	  zap->lmy = iy;
	  break;

	/* DNM 12/13/01: add next and smooth hotkeys to autox */
	case XK_a:
	  autox_next(vi->ax);
	  break;

	case XK_u:
	  autox_smooth(vi->ax);
	  break;

	case XK_b:
	  if (event->state & ShiftMask) { 
	       if (zap->rubberband)
		    zap->rubberband = 0;
	       else {
		    zap->rubberband = 1;
		    size = zap->zoom * vi->xsize / 4;
		    if (size > zap->winx / 4)
			 size = zap->winx / 4;
		    zap->bandllx = zap->winx / 2 - size;
		    zap->bandurx = zap->winx / 2 + size;
		    size = zap->zoom * vi->ysize / 4;
		    if (size > zap->winy / 4)
			 size = zap->winy / 4;
		    zap->bandlly = zap->winy / 2 - size;
		    zap->bandury = zap->winy / 2 + size;
	       }
	       zapDraw(zap);
	  } else
	       autox_build(vi->ax);
	  break;
	  
	case XK_s:
	case XK_S:
	  if ((event->state & ShiftMask) || (event->state & ControlMask)){
	       b3dWinset(XtDisplay(zap->gfx), zap->gfx,
			 (XID)zap->context);
	       zapDraw(zap);
	       limits = NULL;
	       if (zap->rubberband) {
		    limits = limarr;
		    limarr[0] = zap->bandllx + 1;
		    limarr[1] = zap->winy - zap->bandury;
		    limarr[2] = zap->bandurx - 1 - zap->bandllx;
		    limarr[3] = zap->bandury - 1 - zap->bandlly;
	       }
	       if (event->state & ShiftMask)
		   b3dAutoSnapshot("zap", SnapShot_RGB, limits);
	       else if (event->state & ControlMask)
		   b3dAutoSnapshot("zap", SnapShot_TIF, limits);
	  }else
	       inputSaveModel(vi);
	  break;
	  
	case XK_Escape:
	  zap_quit_cb(zap->dialog, (XtPointer)zap, 
		      (XtPointer)zap);
	  break;

	case XK_r:
	  if (event->state & ShiftMask) 
	       zapResizeToFit(zap);
	  break;

	case XK_z:
	case XK_Z:
	  if (event->state & ShiftMask) { 
	       if(zap->sectionStep) {
		    zap->sectionStep = 0;
		    wprint("Auto-section advance turned OFF\n");
	       } else {
		    zap->sectionStep = 1;
		    wprint("Auto-section advance turned ON\n");
	       }
	       XBell(imodDisplay(), 100);
	  } else
	       imod_zap_open(vi);
	  break;

	case XK_i:
	case XK_I:
	  if (event->state & ShiftMask)
	       zapPrintInfo(zap);
	  else {
	       insert_cb(zap->insertButton, (XtPointer)zap, (XtPointer)0);
	       wprint("Toggled modeling direction\n");
	       XBell(imodDisplay(), 100);
	  }
	  break;

	  /*
	case XK_x:
	case XK_y:
	     {
		  Dimension width, height, neww, newh;
		  Position dx, dy;
		  int delta = 1;
		  XtVaGetValues(zap->dialog,
				XmNwidth, &width,
				XmNheight, &height,
				XmNx, &dx, XmNy, &dy,
				NULL);
		  if (event->state & ShiftMask)
		       delta = -1;
		  if (keysym == XK_x)
		       width += delta;
		  else
		       height += delta;
		  printf ("%d x %d\n", width, height);
		  XtConfigureWidget(zap->dialog, dx, dy, width, height, 0);
	     }
	  */

	case IMOD_XK_Prior:
	case IMOD_XK_Next:
	  /* These are defined already above if __vms */
#ifndef __vms
	case XK_Up:
	case XK_Down: 
	case XK_Right: 
	case XK_Left: 
#endif
	  if ((keysym == IMOD_XK_Prior || keysym == IMOD_XK_Next) &&
	      zap->lock == 2){
	       if (keysym == IMOD_XK_Prior)
		    zap->section--;
	       else
		    zap->section++;
	       if (zap->section < 0) zap->section = 0;
	       if (zap->section >= vi->zsize) 
		 zap->section = vi->zsize -1;
	       zapDraw(zap);
	       break;
	  } else if (vi->imod->mousemode != IMOD_MMOVIE &&
		     (keysym == XK_Up || keysym == XK_Down ||
		      keysym == XK_Right || keysym == XK_Left)) {
	       if (keysym == XK_Left)
		    zapTranslate(zap, -trans, 0);
	       if (keysym == XK_Right)
		    zapTranslate(zap, trans, 0);
	       if (keysym == XK_Down)
		    zapTranslate(zap, 0, -trans);
	       if (keysym == XK_Up)
		    zapTranslate(zap, 0, trans);
	       break;
	  }


	/* Prior and Next fall through to default if not locked */

	default:
	  ivwControlActive(vi, 0);
	  inputDefaultKeys(event, vi);
	  break;

     }
     return;
}

static int firstdrag = 0;
static int moveband = 0;
static int firstmx, firstmy;

void zap_input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct zapwin *zap = (struct zapwin *)client;
     struct ViewInfo *vi = zap->vi;
     int button1 = False, button2 = False, button3 = False;
     static Time but1downt = 0;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;
     /* static Time downtime; */

     ivwControlPriority(vi, zap->ctrl);
     switch(cbs->event->type){
	case KeyPress:
	  zap_keyinput((XKeyEvent *)cbs->event, zap);
	  /* downtime = ((XKeyEvent *)cbs->event)->time; */
	  break;

	case KeyRelease:
	  /* downtime -= ((XKeyEvent *)cbs->event)->time; */	  
	  /*  printf ("%d down\n", downtime); */
	  insertDown = 0;
	  break;
	  
	case EnterNotify:
	  XmProcessTraversal(zap->gfx, XmTRAVERSE_CURRENT);
	  break;

	case ButtonPress:
	  XmProcessTraversal(zap->gfx, XmTRAVERSE_CURRENT);
	  if (cbs->event->xbutton.state & Button1Mask)
	       button1 = True;
	  if (cbs->event->xbutton.state & Button2Mask)
	       button2 = True;
	  if (cbs->event->xbutton.state & Button3Mask)
	       button3 = True;

	  switch(cbs->event->xbutton.button){
	     case Button1:
	       but1downt = cbs->event->xbutton.time;
	       firstdrag = 1;
	       firstmx = cbs->event->xbutton.x;
	       firstmy = cbs->event->xbutton.y;
	       break;
	     case Button2:
	       if ((button1) || (button3))
		    break;
	       zapButton2(zap, cbs->event->xbutton.x, cbs->event->xbutton.y);
	       break;
	     case Button3:
	       if ((button1) || (button2))
		    break;
	       zapButton3(zap, cbs->event->xbutton.x, cbs->event->xbutton.y);
	       break;
	     default:
	       break;
	  }
	  zap->lmx = cbs->event->xbutton.x;
	  zap->lmy = cbs->event->xbutton.y;
	  break;

	case ButtonRelease:
	  if (cbs->event->xbutton.button == Button1){
	       firstdrag = 0;
	       if ((cbs->event->xbutton.time - but1downt) > 250) {
		    if (zap->hqgfxsave)
			 zapDraw(zap);
		    zap->hqgfxsave  = 0;
		    break;
	       }
	       zapButton1(zap, cbs->event->xbutton.x, cbs->event->xbutton.y);
	  }
	  if ((cbs->event->xbutton.button == Button2) && zap->rubberband && 
	      moveband){
	       moveband = 0;
	       if (zap->hqgfxsave)
		    zapDraw(zap);
	       zap->hqgfxsave  = 0;
	  }
	  break;

	case MotionNotify:
	  if (cbs->event->xmotion.state & Button1Mask)
	       button1 = True;
	  if (cbs->event->xmotion.state & Button2Mask)
	       button2 = True;
	  if (cbs->event->xmotion.state & Button3Mask)
	       button3 = True;
	  /* fprintf(stderr, "mb  %d|%d|%d\n", button1, button2, button3); */
	  if ( (button1) && (!button2) && (!button3)){
	       /* DNM: wait for a bit, but if we do not replace original 
		  lmx, lmy, there is a disconcerting lurch */
	      if ((cbs->event->xmotion.time - but1downt) > 250)
		   zapB1Drag(zap, cbs->event->xmotion.x,
			     cbs->event->xmotion.y);
	      /*  else
		   break; */
	  }
	  if ( (!button1) && (button2) && (!button3))
	       zapB2Drag(zap, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  if ( (!button1) && (!button2) && (button3))
	       zapB3Drag(zap, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  zap->lmx = cbs->event->xmotion.x;
	  zap->lmy = cbs->event->xmotion.y;
	  break;
	  
	default:
	  break;
     }
}


static void zapB2LookAhead(struct zapwin *zap)
{
    /*  look ahead for motion events and
     *       model them before drawing.
     */
    Imod   *imod = zap->vi->imod;
    Icont  *cont= imodContourGet(imod);
    Ipoint *lpt, cpt;
    float  ix, iy;
    double dist;
    XEvent event;
    Window rootr, childr;
    int rx, ry, wx, wy;
    unsigned int maskr;
    
    if (!cont){
	imodDraw(zap->vi, IMOD_DRAW_RETHINK);
	return;
    }
    XFlush(imodDisplay());
    
    /* If drawing because of insert key, update display before starting loop
       because no new points may be added.  For some reason, the NOSYNC flag
       isn't needed here but is needed within the loop */
    if (insertDown)
	 imodDraw(zap->vi, IMOD_DRAW_RETHINK);

    for (;;) {
	 /* Two cases: if insert key was down, loop until it is up, looking
	    at points and adding when distance is big enough */
	 if (insertDown) {
	      if (XCheckMaskEvent(imodDisplay(), KeyReleaseMask, &event)) {
		   /* If the key is up, clear falg and just return, display is
		      already up to date */
		   insertDown = 0;
		   return;
	      }
	      XQueryPointer(XtDisplay(zap->gfx), XtWindow(zap->gfx),
			    &rootr, &childr, &rx, &ry, &wx, &wy, &maskr);
	      zapGetixy(zap, wx, wy, &ix, &iy);
	 } else {
	      /* Otherwise, loop as long as there are unprocessed mouse
		 motion events */
	      if (!XCheckMaskEvent(imodDisplay(), ButtonMotionMask, &event))
		   break;
	      /* DNM 12/21/00: this was Button3Mask so this wasn't being
		 used */
	      if (!(event.xmotion.state & Button2Mask))
		   break;
	      /* Get coordinate of point we might add. */
	      zapGetixy(zap, event.xmotion.x, event.xmotion.y, &ix, &iy);
	 }

	 if (imod->cindex.point < 0)
	      break;

	 cpt.x = ix;
	 cpt.y = iy;
	 cpt.z = zap->section;
	 
	 /* See if we want to add this point. */
	 lpt = &(cont->pts[imod->cindex.point]);
	 if (zap->twod)
	      cpt.z = lpt->z;
	 dist = imodel_point_dist( lpt, &cpt);
	 if ( dist > imod->res) {
	      if (imod->cindex.point == cont->psize - 1){
		   if (zap->insertmode){
			imod->cindex.point--;
			InsertPoint(imod, &cpt, imod->cindex.point);
		   }else{
			NewPoint(imod, &cpt);
		   }
	      }
	      /* If drawing because of insert key, redraw each time a point is
		 added and set the no-resync flag */
	      if (insertDown)
		   imodDraw(zap->vi, IMOD_DRAW_IMAGE | IMOD_DRAW_NOSYNC);
	 }
    }
    imodDraw(zap->vi, IMOD_DRAW_RETHINK);
    return;
}


/* Attach to nearest point in model mode, or just modify the current 
   xmouse, ymouse values */

static void zapButton1(struct zapwin *zap, int x, int y)
{
     ImodView *vi   = zap->vi;
     Imod     *imod = vi->imod;
     Ipoint pnt, *spnt;
     Iindex index;
     int i, temp_distance;
     int distance = -1;
     float ix, iy;
     float selsize = IMOD_SELSIZE / zap->zoom;

     zapGetixy(zap, x, y, &ix, &iy);
     
     if (vi->ax)
	  if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       autox_fillmouse(vi, ix, iy);
	       return;
	  }
     
     if (vi->imod->mousemode == IMOD_MMODEL){
	  pnt.x = ix;
	  pnt.y = iy;
	  pnt.z = zap->section;
	  vi->xmouse = ix;
	  vi->ymouse = iy;
	  vi->imod->cindex.contour = -1;
	  vi->imod->cindex.point = -1;

	  for (i = 0; i < imod->objsize; i++){
	       index.object = i;
	       temp_distance = imod_obj_nearest
		   (&(vi->imod->obj[i]), &index , &pnt, selsize);
	       if (temp_distance == -1)
		    continue;
	       if (distance == -1 || distance > temp_distance){
		    distance      = temp_distance;
		    vi->imod->cindex.object  = index.object;
		    vi->imod->cindex.contour = index.contour;
		    vi->imod->cindex.point   = index.point;
		    spnt = imodPointGet(vi->imod);
		    if (spnt){
			 vi->xmouse = spnt->x;
			 vi->ymouse = spnt->y;
		    }
	       }
	  }

	  /* DNM: add the DRAW_XYZ flag to make it update info and Slicer */
	  imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
	  return;
     }

     vi->xmouse = ix;
     vi->ymouse = iy;

     imodDraw(vi, IMOD_DRAW_XYZ);
}

/* In model mode, add a model point, creating a new contour if necessary */

static void zapButton2(struct zapwin *zap, int x, int y)
{
     ImodView *vi = zap->vi;
     Iobj  *obj;
     Icont *cont;
     Ipoint point, *cpoint;
     int   pt;
     float ix, iy;
     float lastz;
     int time;
     int rcrit = 10;   /* Criterion for moving the whole band */
     int dxll, dxur,dyll, dyur;
     zapGetixy(zap, x, y, &ix, &iy);

     if (vi->ax){
	  if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       /* DNM 2/1/01: need to call with int */
	       autox_sethigh(vi, (int)ix, (int)iy);
	       return;
	  }
     }
     
     moveband = 0;
     /* If rubber band is on and within criterion distance of any edge, set
	flag to move whole band and return */
     if (zap->rubberband) {
	  dxll = x - zap->bandllx;
	  dxur = x - zap->bandurx;
	  dyll = y - zap->bandlly;
	  dyur = y - zap->bandury;
	  if ((dyll > 0 && dyur < 0 && (dxll < rcrit && dxll > -rcrit ||
					dxur < rcrit && dxur > -rcrit)) ||
	      (dxll > 0 && dxur < 0 && (dyll < rcrit && dyll > -rcrit ||
					dyur < rcrit && dyur > -rcrit))) {
	       moveband = 1;
	       return;
	  }
     }     

     if (vi->imod->mousemode == IMOD_MMODEL){
	  obj = imodObjectGet(vi->imod);
	  if (!obj)
	       return;
	  cont = imodContourGet(vi->imod);
	  point.x = ix;
	  point.y = iy;
	  point.z = zap->section;
	  if ((zap->twod)&&(cont)&&(cont->psize)){
	       point.z = cont->pts->z;
	  }
	  vi->xmouse = ix;
	  vi->ymouse = iy;

	  /* If there is no current contour, start a new one */
	  if (!cont){
	      vi->imod->cindex.contour = obj->contsize - 1;
	      NewContour(vi->imod);
	      cont = imodContourGet(vi->imod);
	      if (!cont)
		  return;
	      if (iobjFlagTime(obj)){
		  ivwGetTime(zap->vi, &time);
		  cont->type = time;
		  cont->flags |= ICONT_TYPEISTIME;
	      }
	  }

	  /* If contours are closed and Z has changed, start a new contour */
	  /* Also check for a change in time, if time data are being modeled */
	  if (iobjClose(obj->flags) && !(cont->flags & ICONT_WILD)){
	       cpoint = imodPointGet(vi->imod);
	       if (cpoint){
		   int cz,pz, contim;
		   cz = cpoint->z; pz = point.z;
		   ivwGetTime(zap->vi, &time);
		   contim = time;
		   if (iobjFlagTime(obj) && (cont->flags & ICONT_TYPEISTIME))
			contim = cont->type;

		   if (cz != pz || time != contim){
		        if (cont->psize == 1) {
			     wprint("\aStarted a new contour even though last "
				    "contour had only 1 pt.  Use open "
				    "contours to model across sections.\n");
			}
			NewContour(vi->imod);
			cont = imodContourGet(vi->imod);
			if (!cont)
			     return;
			if (iobjFlagTime(obj)){
			     cont->type = time;
			     cont->flags |= ICONT_TYPEISTIME;
			}
		   }

	       }
	   }

	  pt = vi->imod->cindex.point;
	  if (pt >= 0)
	       lastz = cont->pts[pt].z;
	  else
	       lastz = point.z;

	  /* Insert or add point depending on insertion mode and whether at end
	     of contour */
	  if ((cont->psize - 1) == pt){
	      if (zap->insertmode && cont->psize)
		  InsertPoint(vi->imod, &point, pt);
	      else
		  NewPoint(vi->imod, &point);
	  }else{
	      if (zap->insertmode)
		  InsertPoint(vi->imod, &point, pt);
	      else
		  InsertPoint(vi->imod, &point, pt + 1);
	  }

	  /* DNM: auto section advance is based on 
		  the direction of section change between last
		  and just-inserted points */
	  if (zap->sectionStep && point.z != lastz) {
	       if (point.z - lastz > 0.0)
		    vi->zmouse += 1.0;
	       else
		    vi->zmouse -= 1.0;

	       if (vi->zmouse < 0.0)
		    vi->zmouse = 0;
	       if (vi->zmouse > vi->zsize - 1)
		    vi->zmouse = vi->zsize - 1;
	       
	       imodDraw(vi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
	       imod_info_setocp();
	  }else
	       zapB2LookAhead(zap);
	  return;
     }
     imodMovieXYZT(vi, MOVIE_DEFAULT, MOVIE_DEFAULT, 1,
		   MOVIE_DEFAULT);
     checkMovieSnap(zap, 1);
}

/* Delete all points of current contour under the cursor */	
static void zapDelUnderCursor(struct zapwin *zap, int x, int y, Icont *cont)
{
     float ix, iy;
     float crit = 8./ zap->zoom;
     float critsq, dsq;
     int i;
     Ipoint *lpt;
     int deleted = 0;

     zapGetixy(zap, x, y, &ix, &iy);
     critsq = crit * crit;
     for (i = 0; i < cont->psize  && cont->psize > 1; ) {
	  lpt = &(cont->pts[i]);
	  if (floor((double)lpt->z + 0.5) == zap->section) {
	       dsq = (lpt->x - ix) * (lpt->x - ix) +
		    (lpt->y - iy) * (lpt->y - iy);
	       if (dsq <= critsq) {
		    imodPointDelete(cont, i);
		    zap->vi->imod->cindex.point = i + zap->insertmode - 1;
		    if (zap->vi->imod->cindex.point < 0)
			 zap->vi->imod->cindex.point = 0;
		    if (zap->vi->imod->cindex.point >= cont->psize)
			 zap->vi->imod->cindex.point = cont->psize - 1;
		    deleted = 1;
		    continue;
	       }
	  }
	  i++;
     }
     if (!deleted)
	  return;
     imodDraw(zap->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
     imod_info_setocp();
}

/* In model mode, modify current point; otherwise run movie */

static void zapButton3(struct zapwin *zap, int x, int y)
{
     ImodView *vi = zap->vi;
     Icont *cont;
     int   pt;
     float ix, iy;
     Window rootr, childr;
     int rx, ry, wx, wy;
     unsigned int maskr;

     zapGetixy(zap, x, y, &ix, &iy);

     if (vi->ax){
	  if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       /* DNM 2/1/01: need to call with int */
	       autox_setlow(vi, (int)ix, (int)iy);
	       return;
	  }
     }

     if (vi->imod->mousemode == IMOD_MMODEL){
	  cont = imodContourGet(vi->imod);
          pt   = vi->imod->cindex.point;
          if (!cont)
	       return;
	  if (pt < 0)
	       return;

	  /* If the control key is down, delete points under the cursor */
	  XQueryPointer(App->display, XtWindow(zap->gfx),
			&rootr, &childr,
			&rx, &ry, &wx, &wy, &maskr);
	  if (maskr & ControlMask) {
	       zapDelUnderCursor(zap, x, y, cont);
	       return; 
	  }

	  
	  if (!zapPointVisable(zap, &(cont->pts[pt])))
	       return;
	  cont->pts[pt].x = ix;
	  cont->pts[pt].y = iy;

          vi->xmouse  = ix;
          vi->ymouse  = iy;

	  imodDraw(vi, IMOD_DRAW_RETHINK);
	  return;
     }
     imodMovieXYZT(vi, MOVIE_DEFAULT, MOVIE_DEFAULT, -1,
		   MOVIE_DEFAULT);
     checkMovieSnap(zap, -1);
}

static void zapB1Drag(struct zapwin *zap, int x, int y)
{
     static int dragband;
     static int dragging[4];
     Window rootr, childr;
     int rx, ry, wx, wy;
     unsigned int maskr;
     int rubbercrit = 10;  /* Criterion distance for grabbing the band */
     int bandmin = 4;     /* Minimum size that the band can become */
     int i, dminsq, dist, distsq, dmin, dxll, dyll, dxur, dyur;
     int minedgex, minedgey;

     XQueryPointer(App->display, XtWindow(zap->gfx),
		   &rootr, &childr,
		   &rx, &ry, &wx, &wy, &maskr);

     if (!(maskr & Button1Mask))
	  return;

     if (zap->rubberband && firstdrag) {
	  /* First time if rubberbanding, analyze for whether close to a
	     corner or an edge */
	  dminsq = rubbercrit * rubbercrit;
	  minedgex = -1;
	  for (i = 0; i < 4; i++)
	       dragging[i] = 0;
	  dxll = firstmx - zap->bandllx;
	  dxur = firstmx - zap->bandurx;
	  dyll = firstmy - zap->bandlly;
	  dyur = firstmy - zap->bandury;

	  /* Find distance from each corner, keep track of a min */
	  distsq = dxll * dxll + dyll * dyll;
	  if (distsq < dminsq) {
	       dminsq = distsq;
	       minedgex = 0;
	       minedgey = 2;
	  }
	  distsq = dxur * dxur + dyll * dyll;
	  if (distsq < dminsq) {
	       dminsq = distsq;
	       minedgex = 1;
	       minedgey = 2;
	  }
	  distsq = dxll * dxll + dyur * dyur;
	  if (distsq < dminsq) {
	       dminsq = distsq;
	       minedgex = 0;
	       minedgey = 3;
	  }
	  distsq = dxur * dxur + dyur * dyur;
	  if (distsq < dminsq) {
	       dminsq = distsq;
	       minedgex = 1;
	       minedgey = 3;
	  }

	  /* If we are close to a corner, set up to drag the band */
	  if (minedgex >= 0) {
	       dragband = 1;
	       dragging[minedgex] = 1;
	       dragging[minedgey] = 1;
	  } else {
	       /* Otherwise look at each edge in turn */
	       dmin = rubbercrit;
	       dist = dxll > 0 ? dxll : -dxll;
	       if (dyll > 0 && dyur < 0 && dist < dmin){
		    dmin = dist;
		    minedgex = 0;
	       }
	       dist = dxur > 0 ? dxur : -dxur;
	       if (dyll > 0 && dyur < 0 && dist < dmin){
		    dmin = dist;
		    minedgex = 1;
	       }
	       dist = dyll > 0 ? dyll : -dyll;
	       if (dxll > 0 && dxur < 0 && dist < dmin){
		    dmin = dist;
		    minedgex = 2;
	       }
	       dist = dyur > 0 ? dyur : -dyur;
	       if (dxll > 0 && dxur < 0 && dist < dmin){
		    dmin = dist;
		    minedgex = 3;
	       }
	       if (minedgex < 0)
		    dragband = 0;
	       else {
		    dragging[minedgex] = 1;
		    dragband = 1;
	       }
	  }
     }
     firstdrag = 0;
     
     if (zap->rubberband && dragband) {
	  /* Move the rubber band */
	  if (dragging[0]) {
	       zap->bandllx += (x - zap->lmx);
	       if (zap->bandllx < 0)
		    zap->bandllx = 0;
	       if (zap->bandllx > zap->bandurx - bandmin)
		    zap->bandllx = zap->bandurx - bandmin;
	  }
	  if (dragging[1]) {
	       zap->bandurx += (x - zap->lmx);
	       if (zap->bandurx > zap->winx - 1)
		    zap->bandurx = zap->winx - 1;
	       if (zap->bandurx < zap->bandllx + bandmin)
		    zap->bandurx = zap->bandllx + bandmin;
	  }
	  if (dragging[2]) {
	       zap->bandlly += (y - zap->lmy);
	       if (zap->bandlly < 0)
		    zap->bandlly = 0;
	       if (zap->bandlly > zap->bandury - bandmin)
		    zap->bandlly = zap->bandury - bandmin;
	  }
	  if (dragging[3]) {
	       zap->bandury += (y - zap->lmy);
	       if (zap->bandury > zap->winy - 1)
		    zap->bandury = zap->winy - 1;
	       if (zap->bandury < zap->bandlly + bandmin)
		    zap->bandury = zap->bandlly + bandmin;
	  }

     } else {
	  /* Move the image */
	  zap->xtrans += (x - zap->lmx);
	  zap->ytrans -= (y - zap->lmy);
     }

     zap->hqgfxsave = zap->hqgfx;
     zap->hqgfx = 0;
     zapDraw(zap);
     zap->hqgfx = zap->hqgfxsave;
}

static void zapB2Drag(struct zapwin *zap, int x, int y)
{
     ImodView *vi = zap->vi;
     Iobj *obj;
     Icont *cont;
     Ipoint *lpt, cpt;
     float ix, iy;
     double dist;
     Window rootr, childr;
     int rx, ry, wx, wy;
     int pt;
     unsigned int maskr;
     int dx, dy;
     
     XQueryPointer(App->display, XtWindow(zap->gfx),
		   &rootr, &childr,
		   &rx, &ry, &wx, &wy, &maskr);

     if (vi->ax){
	  if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       zapGetixy(zap, x, y, &ix, &iy);
	       /* DNM 2/1/01: need to call with int */
	       autox_sethigh(vi, (int)ix, (int)iy);
	       return;
	  }
     }

     if (!(maskr & Button2Mask) && !insertDown)
	  return;

     if (zap->rubberband && moveband) {
	  /* Moving rubber band: get desired move and constrain it to keep
	     band in the window */
	  dx = x - zap->lmx;
	  if (zap->bandllx + dx < 0)
	       dx = -zap->bandllx;
	  if (zap->bandurx + dx > zap->winx - 1)
	       dx = zap->winx - 1 - zap->bandurx;
	  dy = y - zap->lmy;
	  if (zap->bandlly + dy < 0)
	       dy = -zap->bandlly;
	  if (zap->bandury + dy > zap->winy - 1)
	       dy = zap->winy - 1 - zap->bandury;
	  zap->bandllx += dx;
	  zap->bandurx += dx;
	  zap->bandlly += dy;
	  zap->bandury += dy;

	  zap->hqgfxsave = zap->hqgfx;
	  zap->hqgfx = 0;
	  zapDraw(zap);
	  zap->hqgfx = zap->hqgfxsave;
	  return;
     }

     if (vi->imod->mousemode == IMOD_MMOVIE)
	  return;

     if (vi->imod->cindex.point < 0)
	  return;

     zapGetixy(zap, wx, wy, &ix, &iy);
     zapGetixy(zap, x, y, &ix, &iy);

     cpt.x = ix;
     cpt.y = iy;
     cpt.z = zap->section;
     
     obj = imodObjectGet(vi->imod);
     if (!obj)
	  return;

     cont = imodContourGet(vi->imod);
     if (!cont)
	  return;

     lpt = &(cont->pts[vi->imod->cindex.point]);
     if (zap->twod)
	  cpt.z = lpt->z;

     dist = imodel_point_dist( lpt, &cpt);

     if ( dist > vi->imod->res){
	  pt = vi->imod->cindex.point;

	  /* Insert or add point depending on insertion mode and whether at end
	     of contour ; DNM made this work the same as single insert */
	  if ((cont->psize - 1) == pt){
	      if (zap->insertmode && cont->psize)
		  InsertPoint(vi->imod, &cpt, pt);
	      else
		  NewPoint(vi->imod, &cpt);
	  }else{
	      if (zap->insertmode)
		  InsertPoint(vi->imod, &cpt, pt);
	      else
		  InsertPoint(vi->imod, &cpt, pt + 1);
	  }

	  zapB2LookAhead(zap);
     }
}

static void zapB3Drag(struct zapwin *zap, int x, int y)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt;
  Ipoint pt;
  Window rootr, childr;
  int rx, ry, wx, wy;
  unsigned int maskr;
  float ix, iy;

  XQueryPointer(App->display, XtWindow(zap->gfx),
		&rootr, &childr,
		&rx, &ry, &wx, &wy, &maskr);
     
  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      zapGetixy(zap, x, y, &ix, &iy);
      /* DNM 2/1/01: need to call with int */
      autox_setlow(vi, (int)ix, (int)iy);
      return;
    }
  }

  if (!(maskr & Button3Mask))
    return;

  if (vi->imod->mousemode == IMOD_MMOVIE)
    return;
     
  if (vi->imod->cindex.point < 0)
    return;

  cont = imodContourGet(vi->imod);
  if (!cont)
    return;

  /* DNM 11/13/02: do not allow operation on scattered points */
  obj = imodObjectGet(vi->imod);
  if (iobjScat(obj->flags))
    return;

  if (maskr & ControlMask) {
    zapDelUnderCursor(zap, x, y, cont);
    return;
  }

  if (vi->imod->cindex.point == (cont->psize - 1))
    return;

  /* DNM 11/13/02: need to test for both next and current points to prevent
     strange moves between sections */
  if (!zapPointVisable(zap, &(cont->pts[vi->imod->cindex.point + 1])) ||
      !zapPointVisable(zap, &(cont->pts[vi->imod->cindex.point])))
    return;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  zapGetixy(zap, x, y, &(pt.x), &(pt.y));
  pt.z = lpt->z;
  if (imodel_point_dist(lpt, &pt) > vi->imod->res){
    ++vi->imod->cindex.point;
    lpt = &(cont->pts[vi->imod->cindex.point]);
    lpt->x = pt.x;
    lpt->y = pt.y;
    lpt->z = pt.z;
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
  }
  return;
}


/********************************************************
 * conversion functions between image and window cords. */

/* return x pos in window for given image x cord. */
static int zapXpos(struct zapwin *zap, double x)
{
     return( (int)(((x - zap->xstart) * zap->zoom) 
		   + zap->xborder));
}

/* return y pos in window for given image y cord. */
static int zapYpos(struct zapwin *zap, double y)
{
     return((int)(((y - zap->ystart) * zap->zoom)
		  + zap->yborder));
}

/* returns image cords in x,y, given mouse coords mx, my */
static void zapGetixy(struct zapwin *zap, int mx, int my, float *x, float *y)
{
     my = zap->winy - my;
     *x = ((float)(mx - zap->xborder) / zap->zoom)
	  + (float)zap->xstart;
     *y = ((float)(my - zap->yborder) / zap->zoom)
	  + (float)zap->ystart;
     return;
}

/* Prints window size and image coordinates in Info Window */
static void zapPrintInfo(ZapWindow *zap)
{
     float xl, xr, yb, yt;
     int ixl, ixr, iyb, iyt;
     int ixcen, iycen, ixofs, iyofs;
     XRaiseWindow(App->display, XtWindow(App->toplevel));
     if (zap->rubberband) {
	  zapGetixy(zap, zap->bandllx + 1, zap->bandlly + 1, &xl, &yt);
	  zapGetixy(zap, zap->bandurx - 1, zap->bandury - 1, &xr, &yb);
     } else {
	  zapGetixy(zap, 0, 0, &xl, &yt);
	  zapGetixy(zap, zap->winx, zap->winy, &xr, &yb);
     }
     ixl = xl + 0.5;
     ixr = xr - 0.5;
     iyb = yb + 0.5;
     iyt = yt - 0.5;
     wprint("(%d,%d) to (%d,%d); ", ixl + 1, iyb + 1, ixr + 1, iyt + 1);
     ixcen = (ixr + 1 + ixl)/2;
     iycen = (iyt + 1 + iyb)/2;
     ixofs = ixcen - zap->vi->xsize/2;
     iyofs = iycen - zap->vi->ysize/2;
     wprint("Center (%d,%d)\n", ixcen + 1, iycen + 1);
     wprint("To excise: newst -si %d,%d -of %d,%d\n", ixr + 1 - ixl, 
	    iyt + 1 - iyb, ixofs, iyofs);
     if (zap->rubberband) 
	  wprint("Rubberband: %d x %d; ", zap->bandurx - 1 - zap->bandllx, 
		 zap->bandury - 1 - zap->bandlly);
     else
	  wprint("Window: %d x %d;   ", zap->winx, zap->winy);
     wprint("Image: %d x %d\n", ixr + 1 - ixl, iyt + 1 - iyb);
}

/* Resize window to fit either whole image or part in rubber band */
static void zapResizeToFit(ZapWindow *zap)
{
     Dimension width, height, neww, newh, limw, limh;
     Position dx, dy, newdx, newdy;
     float xl, xr, yb, yt;
     XtVaGetValues(zap->dialog,
		   XmNwidth, &width,
		   XmNheight, &height,
		   XmNx, &dx, XmNy, &dy,
		   NULL);
     if (zap->rubberband) {
	  /* If rubberbanding, set size to size of band, and offset
	     image by difference between band and window center */
	  neww = zap->bandurx -1 - zap->bandllx + width - zap->winx;
	  newh = zap->bandury -1 - zap->bandlly + height - zap->winy;
	  zapGetixy(zap, zap->bandllx, zap->bandlly, &xl, &yt);
	  zapGetixy(zap, zap->bandurx, zap->bandury, &xr, &yb);
	  zap->xtrans = -(xr + xl - zap->vi->xsize) / 2;
	  zap->ytrans = -(yt + yb - zap->vi->ysize) / 2;
	  zap->rubberband = 0;
     } else {
	  /* Otherwise, make window the right size for the image */
	  neww = zap->zoom * zap->vi->xsize + width - zap->winx;
	  newh = zap->zoom * zap->vi->ysize + height - zap->winy;
     }
     limw = WidthOfScreen(XDefaultScreenOfDisplay(App->display));
     limh = HeightOfScreen(XDefaultScreenOfDisplay(App->display));
     if (neww > limw - 24)
	  neww = limw - 24;
     if (newh > limh - 44)
		    newh = limh - 44;
     newdx = dx + width / 2 - neww / 2;
     newdy = dy + height / 2 - newh / 2;
     if (newdx < 16)
	  newdx = 16;
     if (newdy < 36)
	  newdy = 36;
     if (newdx + neww > limw - 8)
	  newdx = limw - 8 - neww;
     if (newdy + newh > limh - 8)
	  newdy = limh - 8 - newh;

#ifdef XZAP_DEBUG
     fprintf(stderr, "configuring widget...");
#endif
#ifdef ZAP_EXPOSE_HACK
     imodMovieXYZT(zap->vi, 0, 0, 0, 0);
#endif
     XtConfigureWidget(zap->dialog, newdx, newdy, neww, newh, 0);
#ifdef XZAP_DEBUG
     fprintf(stderr, "back\n");
#endif
}
     
     

/****************************************************************************/
/* drawing routines.                                                        */

static int doingBWfloat = 0;

/* Draws the image.  Returns 1 if further drawing can be skipped */
static int zapDrawGraphics(struct zapwin *zap)
{
     XGCValues val;
     ImodView *vi = zap->vi;
     unsigned char *pixptr;
     Colorindex *data = (Colorindex *)zap->data;
     int i, j, x, y, z;
     int jsize, xlim;
     int xstop, ystop, ystep;
     int zoom = 1;
     int time;
     int xz, yz;
     unsigned char *imageData;
     int skipDraw = 0;

     ivwGetLocation(vi, &x, &y, &z);

     b3dSetCurPoint(x, y, zap->section);

#ifndef DRAW_GL
     zoom = zap->zoom;
#endif

     b3dSetImageOffset(zap->winx, vi->xsize, zap->zoom,
		       &zap->xdrawsize, &zap->xtrans, 
		       &zap->xborder, &zap->xstart);

     b3dSetImageOffset(zap->winy, vi->ysize, zap->zoom,
		       &zap->ydrawsize, &zap->ytrans, 
		       &zap->yborder, &zap->ystart);

     if (zap->timeLock) {
	  imageData = ivwGetZSectionTime(vi, zap->section, zap->timeLock);
	  time = zap->timeLock;
     } else{
	 /* flush if time is different. */
	  ivwGetTime(vi, &time);
	  if (time != zap->time){
	       b3dFlushImage(zap->image);
	       zap->time = time;
	  }
	 imageData = ivwGetZSection(vi, zap->section);
      }

     /* DNM: set sliders if doing float.  Set flag that the routine is
	being called, so that it won't be called again when it initiates a
	redraw.  If a redraw actually occurred, set the flag to skip drawing
	on the rest of this invocation and in the rest of zapdraw */

     if(!doingBWfloat) {
	  doingBWfloat = 1;
	  if (imod_info_bwfloat(vi, zap->section, time) && App->rgba)
	       skipDraw = 1;
	  doingBWfloat = 0;
     }

     if (!skipDraw) {

#ifdef DRAW_OpenGL
	  b3dDrawBoxout(zap->xborder, zap->yborder, 
			zap->xborder + ((float)zap->xdrawsize * zap->zoom),
			zap->yborder + ((float)zap->ydrawsize * zap->zoom));
#else
	  b3dDrawBoxout(zap->xborder - 1, zap->yborder - 1, 
			zap->xborder + ((float)zap->xdrawsize * zap->zoom),
			zap->yborder + ((float)zap->ydrawsize * zap->zoom));
#endif
	  b3dDrawGreyScalePixelsHQ(imageData,
				   vi->xsize, vi->ysize,
				   zap->xstart, zap->ystart,
				   zap->xborder, zap->yborder,
				   zap->xdrawsize, zap->ydrawsize,
				   zap->image,
				   vi->rampbase, 
				   zap->zoom, zap->zoom,
				   zap->hqgfx, zap->section);
     }
     return(skipDraw);
}

static void zapDrawModel(struct zapwin *zap)
{
     ImodView *vi = zap->vi;
     int ob, co;
     int surf = -1;
     Icont *cont = imodContourGet(vi->imod);

     if (vi->imod->drawmode <= 0)
	  return;

     zapDrawGhost(zap);

     if (cont)
	  surf = cont->surf;

     for(ob = 0; ob < vi->imod->objsize; ob++){
	  if (iobjOff(vi->imod->obj[ob].flags))
	       continue;
	  imodSetObjectColor(ob); 
	  b3dLineWidth(vi->imod->obj[ob].linewidth2); 

	  for(co = 0; co < vi->imod->obj[ob].contsize; co++){
	       if (ob == vi->imod->cindex.object){
		    if (co == vi->imod->cindex.contour){
			 zapDrawCurrentContour(zap, co, ob);
			 continue;
		    }
		    if (vi->ghostmode & IMOD_GHOST_SURFACE)
			 if (surf >= 0)
			      if (surf != vi->imod->obj[ob].cont[co].surf){
				   b3dColorIndex(App->ghost); 
				   zapDrawContour(zap, co, ob);
				   imodSetObjectColor(ob);
				   continue;
			      }
	       }

	       zapDrawContour(zap, co, ob); 
	  }
     }
     return;
}

void zapDrawSymbol(int mx, int my, 
		   unsigned char sym,
		   unsigned char size, 
		   unsigned char flags)
{
    
     switch (sym){
	case IOBJ_SYM_CIRCLE:
	  if (flags  & IOBJ_SYMF_FILL)
	       b3dDrawFilledCircle(mx, my, size);
	  else
	       b3dDrawCircle(mx, my, size);
	  break;
	case IOBJ_SYM_SQUARE:
	  if (flags  & IOBJ_SYMF_FILL)
	       b3dDrawFilledSquare(mx, my, size);
	  else
	       b3dDrawSquare(mx, my, size);
	  break;
	case IOBJ_SYM_TRIANGLE:
	  if (flags  & IOBJ_SYMF_FILL)
	       b3dDrawFilledTriangle(mx, my, size);
	  else
	       b3dDrawTriangle(mx, my, size);
	  break;
	case IOBJ_SYM_STAR:
	  break;
	case IOBJ_SYM_NONE:
	  b3dDrawPoint(mx, my);
	  break;

	default:
	  return;

     }
     return;
}

static void zapDrawCurrentContour(struct zapwin *zap, int co, int ob)
{
     ImodView *vi = zap->vi;
     Iobj  *obj  = &(vi->imod->obj[ob]);
     Icont *cont = &(vi->imod->obj[ob].cont[co]);
     Ipoint *point;
     int pt;
     int cz = zap->section;

     if (!cont->psize)
	  return;

     if (iobjClose(obj->flags)){
	  zapDrawContour(zap, co, ob);
	  return;
     }
     if (iobjScat(obj->flags)){
	  zapDrawContour(zap, co, ob);
	  return;
     }

     /* open contour */
     if (cont->psize > 1){
	  for(pt = 0; pt < cont->psize; pt++){
	       point = &(cont->pts[pt]);
	       if (zapPointVisable(zap, point)){
		    zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
				  zapYpos(zap, cont->pts[pt].y),
				  obj->symbol,
				  obj->symsize,
				  obj->symflags);
		    if (pt < (cont->psize - 1))
			 if (zapPointVisable(zap, &(cont->pts[pt+1])))
			      b3dDrawLine(zapXpos(zap, point->x),
					  zapYpos(zap, point->y),
					  zapXpos(zap, cont->pts[pt+1].x),
					  zapYpos(zap, cont->pts[pt+1].y));
	       }
	  }
	  
	  if (vi->drawcursor){
	       if ((zapPointVisable(zap, cont->pts)) && 
		   (zapPointVisable(zap, &(cont->pts[1]))))
		    b3dDrawCircle(zapXpos(zap, cont->pts[0].x),
				  zapYpos(zap, cont->pts[0].y), 3);
	       if ((zapPointVisable(zap, &(cont->pts[cont->psize - 1]))) &&
		    (!zapPointVisable(zap, &(cont->pts[cont->psize - 2]))))
		    b3dDrawCircle
			 (zapXpos(zap, cont->pts[cont->psize - 1].x),
			  zapYpos(zap, cont->pts[cont->psize - 1].y), 3);
	  }
     }else{
          /* DNM: I never stopped being confused by off-section display 
	     of single point, so got rid of it */
          if (zapPointVisable(zap, cont->pts))
	       zapDrawSymbol(zapXpos(zap, cont->pts[0].x),
			     zapYpos(zap, cont->pts[0].y),
			     obj->symbol,
			     obj->symsize,
			     obj->symflags);
     }

     if (obj->symflags & IOBJ_SYMF_ENDS){
	  if (zapPointVisable(zap, &(cont->pts[cont->psize-1]))){
	       b3dColorIndex(App->endpoint);
	       b3dDrawCross(zapXpos(zap, cont->pts[cont->psize-1].x),
			    zapYpos(zap, cont->pts[cont->psize-1].y), 
			    obj->symsize/2);
	  }
	  if (zapPointVisable(zap, cont->pts)){
	       b3dColorIndex(App->bgnpoint);
	       b3dDrawCross(zapXpos(zap, cont->pts->x),
			    zapYpos(zap, cont->pts->y),
			    obj->symsize/2);
	  }
	  imodSetObjectColor(ob);
     }
     return;
}

static void zapDrawContour(struct zapwin *zap, int co, int ob)
{
     ImodView *vi = zap->vi;
     float vert[3];
     Iobj  *obj  = &(vi->imod->obj[ob]);
     Icont *cont = &(vi->imod->obj[ob].cont[co]);
     Ipoint *point;
     int pt, npt = 0, ptsonsec;
     int curTime = vi->ct;
     float drawsize;

     if ((!cont) || (!cont->psize))
	  return;

     if (zap->timeLock) curTime = zap->timeLock;

     /* check for contours that contian time data. */
     /* Don't draw them if the time isn't right. */
     /* DNM 6/7/01: but draw contours with time 0 regardless of time */
     if (vi->nt){
	  if (iobjTime(obj->flags)){
	       if (cont->type && (curTime != cont->type))
		    return;
	  }
     }

     if (iobjClose(obj->flags)){
	  
	  if ((!(cont->flags & ICONT_WILD)) && 
	      (!zapPointVisable(zap, &(cont->pts[0])))){
	       return;
	  }
	  
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       if (!zapPointVisable(zap, &(cont->pts[pt])))
		    continue;
	       b3dVertex2i(zapXpos(zap, cont->pts[pt].x),
			   zapYpos(zap, cont->pts[pt].y));
	  }
	  
	  if (!(cont->flags & ICONT_OPEN))
	       if (!( (co == Model->cindex.contour) &&
		     (ob == Model->cindex.object ))){
		    point = &(cont->pts[0]);
		    if (zapPointVisable(zap, point)){
			 b3dVertex2i(zapXpos(zap, point->x),
				     zapYpos(zap, point->y));
		    }
	       }
	  b3dEndLine();

	  if (obj->symbol != IOBJ_SYM_NONE)
	       for (pt = 0; pt < cont->psize; pt++){
		    if (!zapPointVisable(zap, &(cont->pts[pt])))
			 continue;
		    zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
				  zapYpos(zap, cont->pts[pt].y),
				  obj->symbol,
				  obj->symsize,
				  obj->symflags);
	       }
     }

     if (iobjOpen(obj->flags)){
	  if ((!(cont->flags & ICONT_WILD)) && 
	      (!zapPointVisable(zap, &(cont->pts[0])))){
	       return;
	  }
	  
	  for(pt = 0; pt < cont->psize; pt++){
	       point = &(cont->pts[pt]);
	       if (zapPointVisable(zap, point)){
		     zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
				   zapYpos(zap, cont->pts[pt].y),
				   obj->symbol,
				   obj->symsize,
				   obj->symflags);
		     if (pt < (cont->psize - 1))
			  if (zapPointVisable(zap, &(cont->pts[pt+1])))
			       b3dDrawLine(zapXpos(zap, point->x),
					   zapYpos(zap, point->y),
					   zapXpos(zap, cont->pts[pt+1].x),
					   zapYpos(zap, cont->pts[pt+1].y));
		}
	  }
     }
     
     /* scattered contour */
     if (iobjScat(obj->flags)){
	  for (pt = 0; pt < cont->psize; pt++){
	       if (zapPointVisable(zap, &(cont->pts[pt]))){
		    zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
				  zapYpos(zap, cont->pts[pt].y),
				  obj->symbol,
				  obj->symsize,
				  obj->symflags);
	       }
	       drawsize = imodPointGetSize(obj, cont, pt);
	       if (drawsize > 0)
		    if (zapPointVisable(zap, &(cont->pts[pt]))){
			 /* DNM: make the product cast to int, not drawsize */
			 b3dDrawCircle(zapXpos(zap, cont->pts[pt].x),
				       zapYpos(zap, cont->pts[pt].y),
				       (int)(drawsize * zap->zoom));
			 if (drawsize > 3)
			      b3dDrawPlus(zapXpos(zap, cont->pts[pt].x), 
					  zapYpos(zap, cont->pts[pt].y), 3);
		    }else{
			if (drawsize > 1){
			    /* DNM: fixed this at last, but let size round
			       down so circles get smaller*/
			    /* draw a smaller circ if further away. */
			    vert[0] = (cont->pts[pt].z - zap->section) *
			         App->cvi->imod->zscale;
			    if (vert[0] < 0)
			        vert[0] *= -1.0f;
			
			    if (vert[0] < drawsize - 0.01){
			        vert[1] = sqrt((double)(drawsize * 
					   drawsize) - vert[0] * vert[0])
				           * zap->zoom;
				b3dDrawCircle(zapXpos(zap, cont->pts[pt].x),
					      zapYpos(zap, cont->pts[pt].y),
					      (int)vert[1]);
			    }
			}
		    }
	   }
      }
     
     if (obj->symflags & IOBJ_SYMF_ENDS){
	  if (zapPointVisable(zap, &(cont->pts[cont->psize-1]))){
	       b3dColorIndex(App->endpoint);
	       b3dDrawCross(zapXpos(zap, cont->pts[cont->psize-1].x),
			    zapYpos(zap, cont->pts[cont->psize-1].y), 
			    obj->symsize/2);
	  }
	  if (zapPointVisable(zap, cont->pts)){
	       b3dColorIndex(App->bgnpoint);
	       b3dDrawCross(zapXpos(zap, cont->pts->x),
			    zapYpos(zap, cont->pts->y),
			    obj->symsize/2);
	  }
	  imodSetObjectColor(ob);
     }
     
     return;
}


static void zapDrawCurrentPoint(struct zapwin *zap, int undraw)
{
     ImodView *vi = zap->vi;
     Iobj *obj = imodObjectGet(vi->imod);
     Icont *cont = imodContourGet(vi->imod);
     Ipoint *pnt = imodPointGet(vi->imod);
     int psize = 3;
     int x,y;
     int curTime = vi->ct;
     int contime;

     if (!vi->drawcursor) return;

     if (zap->timeLock)
	  curTime = zap->timeLock;
     contime = curTime;

     if ((App->cvi->imod->mousemode == IMOD_MMOVIE)||(!pnt)){
	  x = zapXpos(zap, (double)((int)vi->xmouse + 0.5));
	  y = zapYpos(zap, (double)((int)vi->ymouse + 0.5));
	  b3dColorIndex(App->foreground);
	  b3dDrawPlus(x, y, psize);
	  
     }else{
	  if ((cont) && (cont->psize) && (pnt)){

	       /* DNM 6/17/01: display off-time features as if off-section */
	       if (iobjTime(obj->flags) && (cont->flags & ICONT_TYPEISTIME) &&
		   cont->type)
		    contime = cont->type;
	       x = zapXpos(zap, pnt->x);
	       y = zapYpos(zap, pnt->y);
	       if (zapPointVisable(zap, pnt) && contime == curTime){
		    b3dColorIndex(App->foreground);
	       }else{
		    b3dColorIndex(App->shadow);
	       }
	       b3dDrawCircle(x, y, psize);
	  }
     }
     
     if (zap->showslice){
	  b3dColorIndex(App->foreground);
	  b3dDrawLine(x, y,
		      zapXpos(zap, zap->vi->slice.zx1+0.5f),
		      zapYpos(zap, zap->vi->slice.zy1+0.5f));
	  b3dDrawLine(x, y,
		      zapXpos(zap, zap->vi->slice.zx2+0.5f), 
		      zapYpos(zap, zap->vi->slice.zy2+0.5f));
	  zap->showslice = 0;
     }

     /* draw begin/end points for current contour */
     if (cont){
	  if (iobjTime(obj->flags) && (cont->flags & ICONT_TYPEISTIME) &&
	      cont->type)
	       contime = cont->type;
	  if (contime != curTime)
	       return;

	  if (cont->psize > 1){
	       if (zapPointVisable(zap, cont->pts)){
		    b3dColorIndex(App->bgnpoint);
		    b3dDrawCircle(zapXpos(zap, cont->pts->x),
				  zapYpos(zap, cont->pts->y), 2);
	       }
	       if (zapPointVisable(zap, &(cont->pts[cont->psize - 1]))){
		    b3dColorIndex(App->endpoint);
		    b3dDrawCircle(zapXpos(zap, cont->pts[cont->psize - 1].x),
				  zapYpos(zap, cont->pts[cont->psize - 1].y), 
				  2);
	       }
	  }
     }
     return;
}

static void zapDrawGhost(struct zapwin *zap)
{
     int ob, co, i;
     short red, green, blue;
     float vert[2];
     struct Mod_Object *obj;
     struct Mod_Contour *cont;
     Imod *mod = zap->vi->imod;
     int nextz, prevz, iz;

     if (!mod)
	  return;

     if ( !(zap->vi->ghostmode & IMOD_GHOST_SECTION))
	  return;
     
     obj = imodObjectGet(mod);
     if (!obj ) return;

     /* DNM: don't do scattered points - point size works for that */
     if(iobjScat(obj->flags))
	  return;

     red = (obj->red * 255.0) / 3.0;
     green = (obj->green * 255.0) / 3.0;
     blue = (obj->blue * 255.0) / 3.0;

#ifdef DRAW_GL
     glcompat(GLC_SLOWMAPCOLORS, TRUE);
     mapcolor(App->ghost, red, green, blue);
     mapcolor(App->ghost, red, green, blue);
     gflush();
     glcompat(GLC_SLOWMAPCOLORS, FALSE);
#endif

     b3dMapColor(App->ghost, red, green, blue); 
     b3dColorIndex(App->ghost);  

     /* DNM: if it's RGB, just have to set the color here */
     if (App->rgba)
	  glColor3f(red/255., green/255., blue/255.);

     /* DNM 6/16/01: need to be based on zap->section, not zmouse */
     nextz = zap->section + 1;
     prevz = zap->section - 1;
     
     for(co = 0; co < obj->contsize; co++){
	  cont = &(obj->cont[co]);

	  /* DNM: don't display wild contours, only coplanar ones */
	  /* By popular demand, display ghosts from lower and upper sections */
	  if (cont->pts && !(cont->flags & ICONT_WILD)) {
	       iz = floor((double)cont->pts->z + 0.5);
	       if ((iz == nextz && (zap->vi->ghostmode & IMOD_GHOST_PREVSEC))
		   || (iz == prevz && (zap->vi->ghostmode & IMOD_GHOST_NEXTSEC)
		       )){
		    b3dBeginLine();
		    for (i = 0; i < cont->psize; i++){
			 b3dVertex2i(zapXpos(zap, cont->pts[i].x),
				     zapYpos(zap, cont->pts[i].y));
		    }

		    /* DNM: connect back to start only if closed contour */
		    if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN))
			 b3dVertex2i(zapXpos(zap, cont->pts->x),
				     zapYpos(zap, cont->pts->y));
		    b3dEndLine();
	       }
	  }
     }
     return;
}


static int zapDrawAuto(struct zapwin *zap)
{
     ImodView *vi = zap->vi;
     unsigned long i, j;
     float vert[2];
     unsigned short cdat;
     int x, y;
     unsigned long pixel;
     unsigned long xsize,ysize;
     int rectsize;

     xsize = vi->xsize;
     ysize = vi->ysize;

     if (!vi->ax)
	  return(-1);

     if (!vi->ax->filled)
	  return(-1);

     if (vi->ax->cz != zap->section)
	  return(-1);

     cdat = App->endpoint;

#ifdef DRAW_GL
     rectzoom(zap->zoom, zap->zoom);
     for (j = 0; j < ysize; j++){
	  y = zapYpos(zap,j);
	  for(i = 0; i < xsize; i++){
	       x = zapXpos(zap,i);
	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK){
		    cdat = vi->rampbase;
		    rectwrite(x, y, x, y, &cdat);
		    continue;
	       }
	       
	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_FLOOD){
		    cdat = App->endpoint;
		    rectwrite(x, y, x, y, &cdat);
		    continue;
	       }

	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_WHITE){
		    cdat = vi->rampbase + 255;
		    rectwrite(x, y, x, y, &cdat); 
	       }
	  }
     }
#else

     /* DNM 8/11/01: make rectangle size be nearest integer and not 0 */
     rectsize = zap->zoom < 1 ? 1 : zap->zoom + 0.5;
     for (j = 0; j < ysize; j++){
	  y = zapYpos(zap,j);
	  for(i = 0; i < xsize; i++){
	       x = zapXpos(zap,i);
	       /*DNM 2/1/01: pick a dark and light color to work in rgb mode */
	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK){
		    pixel = App->ghost;
		    b3dColorIndex(pixel);
		    b3dDrawFilledRectangle(x, y, rectsize, rectsize);
		    continue;
	       }
	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_FLOOD){
		    pixel = App->endpoint;
		    b3dColorIndex(pixel);
		    b3dDrawFilledRectangle(x, y, rectsize, rectsize);
		    continue;
	       }
	       
	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_WHITE){
		    pixel = App->select;
		    b3dColorIndex(pixel);
		    b3dDrawFilledRectangle(x, y, rectsize, rectsize);
	       }

	  }
     }
#endif

     return(0);
}

static void zapDrawTools(ZapWindow *zap)
{
     char vals[16];

     if (zap->toolSection != zap->section){
	  zap->toolSection = zap->section;
	  sprintf(vals, "%3d", zap->section + 1);
	  XtVaSetValues(zap->seclabel, XmNvalue, vals, NULL);
     }
     
     if (zap->toolZoom != zap->zoom){
	  zap->toolZoom = zap->zoom;
	  sprintf(vals, "%3.2f", zap->zoom);
	  XtVaSetValues(zap->zoomlabel, XmNvalue, vals, NULL);
     }

     if (zap->vi->nt)
	  if (zap->timelabel){
	       XmString tlbl, xs1, xs2;
	       int time = zap->vi->ct;
	       if (zap->timeLock) time = zap->timeLock;
	       if (zap->toolTime != time){
		    zap->toolTime = time;
		    xs1 = XmStringCreateSimple
			 (ivwGetTimeIndexLabel(zap->vi, time));
		    sprintf(vals, " (%3d)", time);

		    xs2 = XmStringCreateSimple(vals);
		    tlbl = XmStringConcat(xs1, xs2);
		    XtVaSetValues(zap->timelabel, XmNlabelString,
				  tlbl, NULL);
		    XmStringFree(tlbl);
		    XmStringFree(xs1);
		    XmStringFree(xs2);
	       }
     }
     return;
}

static void zapSetCursor(ZapWindow *zap, int mode)
{
     if (zap->mousemode != mode){
	  if (mode == IMOD_MMODEL)
	       XDefineCursor(App->display, XtWindow(zap->gfx), zap->cursor);
	  else
	       XUndefineCursor(App->display, XtWindow(zap->gfx));
	  zap->mousemode = mode;
     }
     return;
}


static int zapPointVisable(struct zapwin *zap, Ipoint *pnt)
{
    int cz;

    if (zap->twod) return(1);

    if (pnt->z > 0)
	cz = pnt->z + 0.5f;
    else
	cz = pnt->z - 0.5f;
    
    if (cz == zap->section)
	return(1);
    
    return(0);
}

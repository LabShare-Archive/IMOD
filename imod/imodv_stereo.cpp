/*  IMOD VERSION 2.41
 *
 *  imodv_stereo.c -- Stereo view dialog for imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <math.h>
#include <qcombobox.h>
#include <qlayout.h>
#include <qslider.h>
#include <qapplication.h>
#include "multislider.h"
#include "dialog_frame.h"
#include "dia_qtutils.h"
#include "imodv_window.h"
#include "imodv.h"
#include "imod.h"
#include "imod_display.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_stereo.h"
#include "preferences.h"
#include "control.h"
#include "xzap.h"

#ifdef __sgi
#include <X11/Xlib.h>
#include <X11/extensions/SGIStereo.h>
#endif


static void stereoInit(void);
static void stereoSetUp(void);
static void stereoEnable(void);
static void stereoDisable(void);
     
static bool hardwareOK(void);


static struct{
  int       init;
  int       hw;
  ImodvStereo *dia;
  ImodvApp  *a;
  int       tbVoffset;
  int       cw;
  int       omode; /* the default mode when on */

  int   width, height;
  int    x, y;
  float       rad;

  char        *stereoCommand;
  char        *restoreCommand;
     

}imodvStereoData = {0, 0, 0, 0, 0};



static bool hardwareOK(void)
{
  return (Imodv->db && Imodv->stereoDB || !Imodv->db && Imodv->stereoSB);
}

void imodvStereoUpdate(void)
{
  if (imodvStereoData.dia)
    imodvStereoData.dia->update();
}

/*
 * Stereo lib modified code.      DNM: 5/2/98, switched to use the SGI values
 */
static void stereoEnable(void)
{
  if (imodvStereoData.stereoCommand)
    system(imodvStereoData.stereoCommand);
}

/* call to turn off stereo viewing */
static void stereoDisable(void)
{
  stereoHWOff();
}

// Set the draw buffer to the given mode
void stereoDrawBuffer(GLenum mode)
{

  // 6/8/04: This retains the essential calls needed to get stereo on the SGI

  /* sync with GL command stream before calling X (not essential) */
  glFinish();

#ifdef __sgi
  XSGISetStereoBuffer(Imodv->mainWin->x11Display(), 
                      Imodv->mainWin->mCurGLw->winId(),
		      mode == GL_BACK_LEFT ? STEREO_BUFFER_LEFT : 
                      STEREO_BUFFER_RIGHT);
  /* imodPrintStderr("mode %d  buffer %d\n", mode, mode == GL_BACK_LEFT ?
     STEREO_BUFFER_LEFT : STEREO_BUFFER_RIGHT); */

  // This used to be done, is not needed.  There was more buffer-switching
  // in the old X-Motif version but none of it happened.
  //if (mode == GL_BACK_RIGHT)
  // mode = GL_BACK;

  /* sync with X command stream before calling GL (essential) */
  XSync(Imodv->mainWin->x11Display(), False);
#endif

  glDrawBuffer(mode);
}

// Return an offset: sum of the offset due to window size and the user offset
// winy has been divided by 2 already by the caller
int imodvStereoVoffset(void)
{
  return (QApplication::desktop()->height() / 2 - Imodv->winy) +
    imodvStereoData.tbVoffset;
}

// Clear the current buffer, or not as appropriate
void imodvStereoClear()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

// DNM 12/16/02: removed unused  stereoMakeCurrent


/**************************************************************************/


static void stereoSetUp(void)
{

  static int width, height;
  int nx, nwidth, ny , nheight;

  static int x, y;
  float scalefac;
  int configured = 0;
     
  // DNM 12/16/02 deleted Stereo_FULL_SCREEN_HACK

  /* keep window in upper half of screen. */
  QRect oldGeom = ivwRestorableGeometry(Imodv->mainWin);
  width = oldGeom.width();
  height = oldGeom.height();
  x = oldGeom.x();
  y = oldGeom.y();

  if (Imodv->stereo == IMODV_STEREO_HW || Imodv->stereo == IMODV_STEREO_TB) {
    imodvStereoData.width = width;
    imodvStereoData.height = height;
    imodvStereoData.x = x;
    imodvStereoData.y = y;
    imodvStereoData.rad = Imodv->imod->view->rad;
    scalefac = 0.5 * (width > height ? height : width) /
      imodvStereoData.rad;
    ny = y;
    nheight = height;
    nx = x;
    nwidth = width;

    if (Imodv->stereo == IMODV_STEREO_HW) {
      /* DNM: move any window that will go below edge to the top, and
         cut the width and move windows left if necessary too */
      // These numbers are based on the flawed SGI geometry setting where
      // they are actually client window positions...  Will need to 
      // parameterize if it ever works elsewhere
      imodvStereoData.hw = 1;
      if (nheight > 450) 
        nheight = 450;
      if (y + nheight > 484){  
        ny = 34;
      }
      if (width > 1260) {
        nwidth = 1260;
        scalefac = (nwidth * scalefac) / width;
      }
      if (x + nwidth > 1270)
        nx = 1270 - nwidth;
      stereoEnable();
      Imodv->imod->view->rad = 0.5 * 
        (nwidth > nheight ? nheight : nwidth) / scalefac;

    } else {

      // For top/bottom, make the window as tall as possible
      nheight = QApplication::desktop()->height();
      zapLimitWindowSize(nwidth, nheight);
      zapLimitWindowPos(nwidth, nheight, nx, ny);
    }

    // Resize and move window as needed
    if ( (y != ny) || (height != nheight) || (x != nx) || 
         (width != nwidth)){
      Imodv->mainWin->resize(nwidth, nheight);
      Imodv->mainWin->move(nx, ny);
      configured = 1;
    }

    // Turning stereo off or switching to side-to-side: reset window
  } else if (imodvStereoData.omode == IMODV_STEREO_TB || imodvStereoData.hw) {
    if (imodvStereoData.hw) {
      stereoDisable();
      Imodv->imod->view->rad = imodvStereoData.rad;
    }
    Imodv->mainWin->resize(imodvStereoData.width, imodvStereoData.height);
    Imodv->mainWin->move(imodvStereoData.x, imodvStereoData.y);
    configured = 1;
  }

  // Now set old mode value
  if (Imodv->stereo != IMODV_STEREO_OFF)
    imodvStereoData.omode = Imodv->stereo;
  if (!configured) {
    imodvDraw(Imodv);
  }

}

static void stereoInit(void)
{
  char *envtmp;

  if (imodvStereoData.init)
    return;
  atexit(stereoHWOff);
  imodvStereoData.omode = IMODV_STEREO_HW;
  imodvStereoData.a = Imodv;
  imodvStereoData.init = 1;

  // Set up stereo commands: have default for SGI, then get from environment
#ifdef __sgi
  imodvStereoData.stereoCommand = "/usr/gfx/setmon -n STR_TOP";
  imodvStereoData.restoreCommand = "/usr/gfx/setmon -n 72HZ";
  //imodvStereoData.restoreCommand = "/usr/gfx/setmon -n 1600x1024_72";
#else
  imodvStereoData.stereoCommand = NULL;
  imodvStereoData.restoreCommand = NULL;
#endif
  envtmp = getenv("IMOD_STEREO_COMMAND");
  if (envtmp)
    imodvStereoData.stereoCommand = envtmp;
  envtmp = getenv("IMOD_STEREO_RESTORE");
  if (envtmp)
    imodvStereoData.restoreCommand = envtmp;
  if (Imod_debug)
    imodPrintStderr("Stereo enable and restore commands:\n%s\n%s\n",
                    imodvStereoData.stereoCommand,
                    imodvStereoData.restoreCommand);

  envtmp = getenv("IMOD_STEREO_TBOFFSET");
  if (envtmp) {
    imodvStereoData.tbVoffset = atoi(envtmp);
    imodvStereoData.omode = IMODV_STEREO_TB;
  }
}

void stereoHWOff(void)
{
  if (imodvStereoData.hw && imodvStereoData.restoreCommand)
    system(imodvStereoData.restoreCommand);
  imodvStereoData.hw = 0;
}

void imodvStereoToggle(void)
{
  stereoInit();

  if (Imodv->stereo != IMODV_STEREO_OFF){
    Imodv->stereo = IMODV_STEREO_OFF;
  }else{

    // If hardware is old mode and it is not OK, drop to RL
    Imodv->stereo = imodvStereoData.omode;
    if ((Imodv->stereo == IMODV_STEREO_HW) && !hardwareOK())
      Imodv->stereo = IMODV_STEREO_RL;
  }

  if(Imod_debug)
    imodPrintStderr("hardware OK %d, stereo %d\n", hardwareOK() ? 1 : 0,
                    Imodv->stereo);
  imodvStereoUpdate();
  stereoSetUp();
}

void imodvStereoEditDialog(ImodvApp *a, int state)
{
  if (!state){
    if (imodvStereoData.dia)
      imodvStereoData.dia->close();
    return;
  }

  stereoInit();

  if (imodvStereoData.dia){
    imodvStereoData.dia->raise();
    return;
  }

  imodvStereoData.dia = new ImodvStereo(imodvDialogManager.parent(IMODV_DIALOG),
                                        "stereo dialog");
  imodvStereoData.a = a;

  imodvDialogManager.add((QWidget *)imodvStereoData.dia, IMODV_DIALOG);
  imodvStereoUpdate();
  imodvStereoData.dia->show();
}

/****************************************************************************/
/*  Stereo Dialog class                                                     */

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close dialog box", "Open help window"};
static char *sliderLabels[] = {"Angle", "Vertical Offset",};

ImodvStereo::ImodvStereo(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, buttonLabels, buttonTips, true,
                ImodPrefs->getRoundedStyle(), "3dmodv Stereo", "", name)
{
  // Make combo box, with just the 2 software options
  mCtrlPressed = false;
  mComboBox = new QComboBox(this, "stereo combo");
  mLayout->addWidget(mComboBox);
  mComboBox->insertItem("Stereo Off", IMODV_STEREO_OFF);
  mComboBox->insertItem("Side by Side", IMODV_STEREO_RL);
  mComboBox->insertItem("Top / Bottom", IMODV_STEREO_TB);
  mComboBox->setFocusPolicy(NoFocus);
  connect(mComboBox, SIGNAL(activated(int)), this, SLOT(newOption(int)));

  // Make the slider with 1 decimal point
  mSlider = new MultiSlider(this, 2, sliderLabels, -100, 100, 1);
  mLayout->addLayout(mSlider->getLayout());
  mSlider->getSlider(0)->setPageStep(5);
  mSlider->setDecimals(1, 0);
  mSlider->setValue(1, imodvStereoData.tbVoffset);
  connect(mSlider, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
}

// User selects a new option
void ImodvStereo::newOption(int item)
{
  // DNM 9/23/04: eliminated switch since only action now is to assign value
  Imodv->stereo = item;
  stereoSetUp();
}

void ImodvStereo::sliderMoved(int which, int value, bool dragging)
{
  if (which)
    imodvStereoData.tbVoffset = value;
  else
    Imodv->plax = (float)(value / 10.);
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed))
    imodvDraw(Imodv);
}

void ImodvStereo::buttonPressed(int which)
{
  if (which)
    dia_vasmsg
    ("~~~~~~~~~~~~~~~~~~~~~~~~~\n"
     "   Stereo Dialog Help.\n"
     "~~~~~~~~~~~~~~~~~~~~~~~~~"
     "\n\n",
     "This window provides control over two or three kinds of stereo "
     "displays, depending on whether the graphics hardware supports stereo.  "
     "The combo box allows one to turn a particular kind of stereo on or off, "
     "the \"Angle\" slider allows one to adjust the angle of parallax between "
     "the two views, and the \"Vertical Offset\" slider allows one to adjust "
     "the offset between views in top/bottom stereo to bring the views "
     "into register when the monitor does two vertical scans per frame.\n\n",
     "\tSIDE-BY-SIDE: This mode is the default when no internal or external "
     "hardware seems to available.  Some people may find this useful if they "
     "can fuse the two images somehow.\n\n"
     "\tTOP/BOTTOM: This mode is designed to be used when the monitor can be "
     "set to display the top and bottom of the display in two successive "
     "scans.  When you activate this mode, the model view window will be "
     "stretched to the full height of the screen and the model will be shown "
     "in the top and bottom of this window.  After you switch the "
     "monitor to display two scans, adjust the vertical offset slider until "
     "the two images coincide.  Once you have determined this offset, you can "
     "define an environment variable IMOD_STEREO_TBOFFSET to have this value, "
     "and this offset will be applied automatically in the future.\n\n"
     "\tHARDWARE: This mode is available only if the display hardware "
     "supports left and right stereo buffers in OpenGL.  When the mode is "
     "activated, the command defined in the environment variable "
     "IMOD_STEREO_COMMAND is executed.  The two images are then drawn into "
     "the left and right buffers.  When the mode is turned off, the command "
     "defined in the environment variable IMOD_STEREO_RESTORE is executed.  "
     "This mode is known "
     "to work only on an SGI, where the proper specialized calls are made to "
     "use the stereo buffers, and the proper commands are built in to call "
     "setmon and change the monitor mode.\n\n"
     "Stereo is toggled with the \"s\" hot key.  The first time this is used, "
     "the program will go into Top/Bottom mode if IMOD_STEREO_TBOFFSET is "
     "defined, otherwise into Hardware mode if stereo buffers are available, "
     "otherwise into Side-by-side mode."
     , NULL);
  else
    close();
}

// Update the dialog box
void ImodvStereo::update()
{
  // Verify and send out the plax value
  if (Imodv->plax < -10.)
    Imodv->plax = -10.;
  if (Imodv->plax > 10.)
    Imodv->plax = 10.;

  mSlider->setValue(0, (int)floor(Imodv->plax * 10. + 0.5));

  // Adjust the number of combo items for status of hardware stereo
  if (hardwareOK() && mComboBox->count() == 3)
    mComboBox->insertItem("Hardware", IMODV_STEREO_HW);

  if (!hardwareOK() && mComboBox->count() == 4)
    mComboBox->removeItem(IMODV_STEREO_HW);

  // Set the combo box
  mComboBox->setCurrentItem(Imodv->stereo);
}

void ImodvStereo::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}
  
// Accept a close event and set dia to null
void ImodvStereo::closeEvent ( QCloseEvent * e )
{
  imodvDialogManager.remove((QWidget *)imodvStereoData.dia);
  imodvStereoData.dia = NULL;
  e->accept();
}

// Close on escape; watch for the hot slider key; pass on keypress
void ImodvStereo::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else {
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    imodvKeyPress(e);
  }
}

// pass on key release; watch for hot slider release
void ImodvStereo::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  imodvKeyRelease(e);
}


/*
$Log$
Revision 4.9  2004/09/24 18:15:31  mast
Made it fix window geometry if going from T/B to S/S.

Revision 4.8  2004/06/09 05:38:20  mast
Got top/bottom mode to work with double-scan setups, and really got SGI
to work by incorporating old SGI-specific calls.

Revision 4.7  2004/06/06 21:27:47  mast
Cleanup and changes to get it working on SGI

Revision 4.6  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.5  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.4  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.3  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.2  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.9  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.8  2003/01/18 00:58:37  mast
add tooltips to dialogframe call

Revision 1.1.2.7  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.6  2003/01/01 05:45:42  mast
Qt version

Revision 1.1.2.5  2002/12/18 04:49:31  mast
Don't require sgi-only stereo to be only standalone

Revision 1.1.2.4  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.3  2002/12/17 22:28:21  mast
cleanup of unused variables and SGI errors

Revision 1.1.2.2  2002/12/17 18:31:30  mast
preliminary changes for Qt

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.2  2002/12/01 16:51:34  mast
Changes to eliminate warnings on SGI

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/

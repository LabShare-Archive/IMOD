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
  int       cw;
  int       omode; /* the default mode when on */

  int   width, height;
  int    x, y;
  float       rad;

  char        *stereoCommand;
  char        *restoreCommand;
     

}imodvStereoData = {0, 0, 0, 0};



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

void stereoDrawBuffer(GLenum mode)
{
  // 6/6/04: Eliminated abstruse buffer-changing logic from SGI
  glDrawBuffer(mode);
}

// 6/6/04: Eliminated stereoClear as unneeded and detrimental


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
  if (Imodv->stereo == IMODV_STEREO_HW){
    imodvStereoData.hw = 1;
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
    /* DNM: move any window that will go below edge to the top, and
       cut the width and move windows left if necessary too */
    // These numbers are based on the flawed SGI geometry setting where
    // they are actually client window positions...  Will need to parameterize
    // if it ever works elsewhere
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
             
    /* DNM: set the zoom and enable the stereo before configuring,
       so that the redraw will be useful, then set flag so that
       the resize events can be skipped */

    Imodv->imod->view->rad = 0.5 * 
      (nwidth > nheight ? nheight : nwidth) / scalefac;
    stereoEnable();
    if ( (y != ny) || (height != nheight) || (x != nx) || 
         (width != nwidth)){
                 
      Imodv->mainWin->resize(nwidth, nheight);
      Imodv->mainWin->move(nx, ny);
      configured = 1;
    }
  }else{
    if (imodvStereoData.hw){
      Imodv->imod->view->rad = imodvStereoData.rad;
      stereoDisable();
      Imodv->mainWin->resize(imodvStereoData.width, imodvStereoData.height);
      Imodv->mainWin->move(imodvStereoData.x, imodvStereoData.y);
      configured = 1;
    }
  }

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
  imodvStereoData.restoreCommand = "/usr/gfx/setmon -n 1600x1024_72";
  //imodvStereoData.restoreCommand = "/usr/gfx/setmon -n 72HZ";
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
static char *sliderLabels[] = {"Angle"};

ImodvStereo::ImodvStereo(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "3dmodv Stereo", "",
                name)
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
  mSlider = new MultiSlider(this, 1, sliderLabels, -100, 100, 1);
  mLayout->addLayout(mSlider->getLayout());
  mSlider->getSlider(0)->setPageStep(5);
  connect(mSlider, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
}

// User selects a new option
void ImodvStereo::newOption(int item)
{
  switch (item) {

  case IMODV_STEREO_OFF:
    Imodv->stereo = IMODV_STEREO_OFF;
    break;

  case IMODV_STEREO_RL:
    Imodv->stereo = IMODV_STEREO_RL;
    imodvStereoData.omode = IMODV_STEREO_RL;
    break;

  case IMODV_STEREO_TB:
    imodvStereoData.omode = IMODV_STEREO_TB;
    Imodv->stereo = IMODV_STEREO_TB;
    break;

  case IMODV_STEREO_HW:
    imodvStereoData.omode = IMODV_STEREO_HW;
    Imodv->stereo = IMODV_STEREO_HW;
    break;

  }
  stereoSetUp();
}

void ImodvStereo::sliderMoved(int which, int value, bool dragging)
{
  Imodv->plax = (float)(value / 10.);
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed))
    imodvDraw(Imodv);
}

void ImodvStereo::buttonPressed(int which)
{
  if (which)
    dia_vasmsg
    ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
     "Stereo Edit Dialog Help.\n"
     "~~~~~~~~~~~~~~~~~~~~~~~~"
     "\n\n",
     "Manipulate stereo view.",
     NULL);
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

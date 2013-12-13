/*
 *  mv_stereo.cpp -- Stereo view dialog for imodv.
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

#include <math.h>
#include <qcombobox.h>
#include <qlayout.h>
#include <qslider.h>
#include <qapplication.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>
#include <QDesktopWidget>
#include <QCheckBox>
#include <QSpinBox>
#include "multislider.h"
#include "dialog_frame.h"
#include "dia_qtutils.h"
#include "mv_window.h"
#include "imodv.h"
#include "imod.h"
#include "display.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "mv_image.h"
#include "mv_stereo.h"
#include "preferences.h"
#include "control.h"

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
     

}imodvStereoData = {0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0., NULL, NULL};


// Check whether hardware is available for the current state of buffering
static bool hardwareOK(void)
{
  return ((Imodv->dblBuf && Imodv->enableDepthDBst >= 0) || 
          (!Imodv->dblBuf && Imodv->enableDepthSBst >= 0));
}

// External call to update the dialog
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
  imodv_setbuffer(Imodv, -1, 1, -1);
  if (imodvStereoData.stereoCommand)
    system(imodvStereoData.stereoCommand);
}

/* call to turn off stereo viewing */
static void stereoDisable(void)
{
  imodv_setbuffer(Imodv, -1, 0, -1);
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

/*
 * Sets up the screen geometry when entering or leaving stereo 
 * and makes calls to enable or disable hardware stereo
 */
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
#ifdef __sgi      
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
      Imodv->imod->view->rad = 0.5 * 
        (nwidth > nheight ? nheight : nwidth) / scalefac;
#endif
      stereoEnable();

    } else {

      // For top/bottom, make the window as tall as possible
      nheight = QApplication::desktop()->height();
      diaLimitWindowSize(nwidth, nheight);
      diaLimitWindowPos(nwidth, nheight, nx, ny);
    }

    // Resize and move window as needed
    if ( (y != ny) || (height != nheight) || (x != nx) || 
         (width != nwidth)){
      Imodv->mainWin->resize(nwidth, nheight);
      Imodv->mainWin->move(nx, ny);
      if (height != nheight || width != nwidth)
        configured = 1;
    }

    // Turning stereo off or switching to side-to-side: reset window
  } else if (imodvStereoData.omode == IMODV_STEREO_TB || imodvStereoData.hw) {
    if (imodvStereoData.hw) {
      stereoDisable();
      Imodv->imod->view->rad = imodvStereoData.rad;
      Imodv->clearAfterStereo = 1;
    }
    Imodv->mainWin->resize(imodvStereoData.width, imodvStereoData.height);
    Imodv->mainWin->move(imodvStereoData.x, imodvStereoData.y);
    if (width != imodvStereoData.width && height != imodvStereoData.height)
      configured = 1;
  }

  // Now set old mode value
  if (Imodv->stereo != IMODV_STEREO_OFF)
    imodvStereoData.omode = Imodv->stereo;
  if (!configured) {
    imodvDraw(Imodv);
  }

}

/*
 * One-time initialization of the structures and other items
 */
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

/*
 * Turn off hardware stereo - can be called at exit
 */
void stereoHWOff(void)
{
  if (imodvStereoData.hw && imodvStereoData.restoreCommand)
    system(imodvStereoData.restoreCommand);
  imodvStereoData.hw = 0;
}

// The external call to toggle the stereo from hot key
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

/*
 * Open the stereo dialog
 */
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
  adjustGeometryAndShow((QWidget *)imodvStereoData.dia, IMODV_DIALOG);
}

/****************************************************************************/
/*  Stereo Dialog class                                                     */

static const char *buttonLabels[] = {"Done", "Help"};
static const char *buttonTips[] = {"Close dialog box", "Open help window"};
static const char *sliderLabels[] = {"Angle", "Vertical Offset",};

ImodvStereo::ImodvStereo(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, buttonLabels, buttonTips, true,
                ImodPrefs->getRoundedStyle(), "3dmodv Stereo", "", name)
{
  QHBoxLayout *hLayout;

  // Make combo box, with just the 2 software options
  mCtrlPressed = false;
  mComboBox = new QComboBox(this);
  mLayout->addWidget(mComboBox);
  QStringList items;
  items << "Stereo Off" << "Side by Side" << "Top / Bottom";
  mComboBox->addItems(items);
  mComboBox->setFocusPolicy(Qt::NoFocus);
  connect(mComboBox, SIGNAL(activated(int)), this, SLOT(newOption(int)));

  // Make the slider with 1 decimal point
  mSlider = new MultiSlider(this, 2, sliderLabels, -100, 100, 1);
  mLayout->addLayout(mSlider->getLayout());
  mSlider->getSlider(0)->setPageStep(5);
  mSlider->setDecimals(1, 0);
  mSlider->setValue(1, imodvStereoData.tbVoffset);
  connect(mSlider, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));

  if (imodvByteImagesExist() != 0) {
    mImageBox = diaCheckBox("Show images as stereo pairs", this, mLayout);
    diaSetChecked(mImageBox, Imodv->imageStereo != 0);
    connect(mImageBox, SIGNAL(toggled(bool)), this, SLOT(imageToggled(bool)));

    hLayout = diaHBoxLayout(mLayout);
    mViewsAreaSpin = (QSpinBox *)diaLabeledSpin
      (0, 2, App->cvi->zsize, 1, "Tilted views per area", this, hLayout);
    diaSetSpinBox(mViewsAreaSpin, Imodv->imagesPerArea);
    connect(mViewsAreaSpin, SIGNAL(valueChanged(int)), this, 
            SLOT(viewsChanged(int)));

    hLayout = diaHBoxLayout(mLayout);
    mDeltaZspin = (QSpinBox *)diaLabeledSpin(0, 1, Imodv->imagesPerArea - 1, 
                                             1, "Sections between pairs", 
                                             this, hLayout);
    Imodv->imageDeltaZ = B3DMIN(Imodv->imageDeltaZ, Imodv->imagesPerArea - 1);
    diaSetSpinBox(mDeltaZspin, Imodv->imageDeltaZ);
    connect(mDeltaZspin, SIGNAL(valueChanged(int)), this, 
            SLOT(deltaChanged(int)));
  }

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
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvDraw(Imodv);
}

void ImodvStereo::imageToggled(bool state)
{
  Imodv->imageStereo = state ? 1 : 0;
  mvImageUpdate(Imodv);
  update();
  imodvDraw(Imodv);
}

void ImodvStereo::viewsChanged(int value)
{
  Imodv->imagesPerArea = value;
  Imodv->imageDeltaZ = B3DMIN(Imodv->imagesPerArea - 1, Imodv->imageDeltaZ);
  diaSetSpinMMVal(mDeltaZspin, 1, Imodv->imagesPerArea-1, Imodv->imageDeltaZ);
  imodvDraw(Imodv);
}
 
void ImodvStereo::deltaChanged(int value)
{
  Imodv->imageDeltaZ = value;
  imodvDraw(Imodv);
}

void ImodvStereo::buttonPressed(int which)
{
  if (which)
    imodShowHelpPage("modvStereo.html#TOP");
  else
    close();
}

// Update the dialog box including the combo box for whether hardware 
// is available currently
void ImodvStereo::update()
{
  // Verify and send out the plax value
  if (Imodv->plax < -10.)
    Imodv->plax = -10.;
  if (Imodv->plax > 10.)
    Imodv->plax = 10.;

  mSlider->setValue(0, (int)floor(Imodv->plax * 10. + 0.5));

  // Adjust the number of combo items for status of hardware stereo
  QStringList items;
  if (hardwareOK() && mComboBox->count() == 3)
    mComboBox->addItem("Hardware");

  if (!hardwareOK() && mComboBox->count() == 4)
    mComboBox->removeItem(IMODV_STEREO_HW);

  // Set the combo box
  mComboBox->setCurrentIndex(Imodv->stereo);

  if (imodvByteImagesExist() != 0) {
    mImageBox->setEnabled(Imodv->texMap != 0);
    mViewsAreaSpin->setEnabled(Imodv->texMap != 0);
    mDeltaZspin->setEnabled(Imodv->texMap != 0);
    mSlider->setEnabled(0, Imodv->texMap == 0 || !Imodv->imageStereo);
  }
}

void ImodvStereo::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
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
  if (utilCloseKey(e))
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


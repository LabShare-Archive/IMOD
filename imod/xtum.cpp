/*  IMOD VERSION 2.7.9
 *
 *  xtum.cpp -- The tumbler window
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include <string.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qspinbox.h>
#include <qtoolbutton.h>
#include <qpushbutton.h>
#include <qsignalmapper.h>
#include <qframe.h>
#include <qvbox.h>
#include <qhbox.h>
#include <qbitmap.h>
#include <qtooltip.h>
#include "multislider.h"
#include "preferences.h"
#include "dia_qtutils.h"
#include "control.h"

#include "sslice.h"
#include "xtum.h"

#include "imod.h"
#include "imod_input.h"
#include <imat.h>
#include "b3dgfx.h"

#include "lowres.bits"
#include "highres.bits"
#include "lock.bits"
#include "unlock.bits"

#define BM_WIDTH 16
#define BM_HEIGHT 16
#define XTUM_WIDTH 200
#define XTUM_HEIGHT 250
#define XTUM_SIZE_MIN 8
#define XTUM_SIZE_MAX 148
#define XTUM_SIZE_INC 8
#define XTUM_SIZE_INI 48
#define XTUM_MAX_ZOOM 12

#define AUTO_RAISE false


static void xtumClose_cb(ImodView *vi, void *client, int junk);
static void xtumDraw_cb(ImodView *vi, void *client, int drawflag);
static void xtumKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e);

static unsigned char *bitList[MAX_XTUM_TOGGLES][2] =
  { {lowres_bits, highres_bits},
    {unlock_bits, lock_bits}};

static QBitmap *bitmaps[MAX_XTUM_TOGGLES][2];
static int firstTime = 1;

/*
 * Callbacks from the controller 
 */
static void xtumClose_cb(ImodView *vi, void *client, int junk)
{
  TumblerStruct *xtum = (TumblerStruct *)client;
  xtum->dialog->close();
}

static void xtumKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e)
{
  TumblerStruct *xtum = (TumblerStruct *)client;
  if (e->key() != hotSliderKey())
    xtum->dialog->externalKeyEvent(e, released);
}

// Draw at new position if x/y/z changes, or draw if image changed
static void xtumDraw_cb(ImodView *vi, void *client, int drawflag)
{
  TumblerStruct *xtum = (TumblerStruct *)client;

  if (drawflag & IMOD_DRAW_COLORMAP) {
    xtum->dialog->mGLw->setColormap(*(App->qColormap));
    return;
  }

  if (!xtum->locked && (drawflag & IMOD_DRAW_XYZ)) {
    xtum->cx = (int)vi->xmouse;
    xtum->cy = (int)vi->ymouse;
    xtum->cz = (int)(vi->zmouse + 0.5);
    xtum->dialog->newData(xtum);
    xtum->dialog->draw(xtum);
    return;
  }

  if (drawflag & (IMOD_DRAW_IMAGE | IMOD_DRAW_ACTIVE)) {
    xtum->dialog->newData(xtum);
    xtum->dialog->draw(xtum);
    return;
  }
}


/*
 * Open the tumbler window
 */
int xtumOpen(struct ViewInfo *vi)
{
  TumblerStruct *xtum;
     
  xtum = (TumblerStruct *)malloc(sizeof(TumblerStruct));
  if (!xtum)
    return(-1);

  xtum->vi      = vi;
  xtum->zoom    = 1;
  xtum->image   = NULL;
  xtum->slice = xtum->stslice = xtum->bwslice = xtum->count = NULL;
  xtum->highres = 0;
  xtum->tstep = 2.;
  xtum->nx = xtum->ny = xtum->nz = XTUM_SIZE_INI;
  xtum->cx = (int)vi->xmouse;
  xtum->cy = (int)vi->ymouse;
  xtum->cz = (int)(vi->zmouse + 0.5);
  xtum->ms = (int)(sqrt((double)(xtum->nx * xtum->nx + xtum->ny * xtum->ny +
				xtum->nz * xtum->nz)) + 0.9);
  xtum->minval = 0;
  xtum->maxval = 255;
  xtum->locked = 0;
  xtum->bbox   = 0;
  xtum->stereo = 0;
  xtum->plax = 4.0f;
  xtum->alpha = 0.0;
  xtum->beta = 0.0;
  xtum->gamma = 0.0;
     
  xtum->dialog = new TumblerWindow(xtum, App->rgba, 
                                   App->doublebuffer, App->qtEnableDepth,
                                   imodDialogManager.parent(IMOD_IMAGE),
                                   "tumbler window");
  if (!xtum->dialog){
    free(xtum);
    wprint("Error opening tumbler window.");
    return(-1);
  }

  if (!App->rgba)
    xtum->dialog->mGLw->setColormap(*(App->qColormap));

  xtum->dialog->setCaption(imodCaption("3dmod Tumbler"));
  xtum->ctrl = ivwNewControl(vi, xtumDraw_cb, xtumClose_cb, xtumKey_cb,
                               (void *)xtum);
  imodDialogManager.add((QWidget *)xtum->dialog, IMOD_IMAGE);
  xtum->dialog->show();
     
  return(0);
}

/*
 * Constructor for the tumbler class 
 */
static char *xyzLabels[] = {"X", "Y", "Z"};
static char *sliderLabels[] = {"Black Threshold", "White Threshold"};

TumblerWindow::TumblerWindow(TumblerStruct *xtum, bool rgba,
            bool doubleBuffer, bool enableDepth, QWidget * parent,
            const char * name, WFlags f)
  : QMainWindow(parent, name, f)
{
  int j;
  QString str;
  mTum = xtum;

  // Make central vbox and top frame containing an hboxlayout
  QVBox *central = new QVBox(this, "central");
  setCentralWidget(central);
  QFrame * topFrame = new QFrame(central, "topFrame");
  topFrame->setFrameStyle(QFrame::Raised | QFrame::StyledPanel);

  QHBoxLayout *topLayout = new QHBoxLayout(topFrame, 4);
  QVBox *topVBox = new QVBox(topFrame, "topVBox");
  topLayout->addWidget(topVBox);
  
  QHBox *topHBox = new QHBox(topVBox, "topHBox");
  if (!(AUTO_RAISE))
      topHBox->setSpacing(4);

  // Add the toolbar widgets
  // Zoom spin box
  // If you try to make a spin box narrower, it makes the arrows tiny
  QLabel *label = new QLabel("Zoom", topHBox);
  mZoomBox = new QSpinBox(1, XTUM_MAX_ZOOM, 1, topHBox);
  mZoomBox->setValue(xtum->zoom);
  mZoomBox->setFocusPolicy(QWidget::ClickFocus);
  connect(mZoomBox, SIGNAL(valueChanged(int)), this, 
	  SLOT(zoomChanged(int)));
  QToolTip::add(mZoomBox, "Change zoom of display");

  // Make the 2 toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(topHBox);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < 2; j++)
    setupToggleButton(topHBox, toggleMapper, j);

  QToolTip::add(mToggleButs[0],
		"Toggle between low and high resolution.");
  QToolTip::add(mToggleButs[1], "Lock X/Y/Z position being displayed");

  // Help button
  mHelpButton = new QPushButton("Help", topHBox, "Help button");
  mHelpButton->setFocusPolicy(QWidget::NoFocus);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  setFontDependentWidths();

  // Spacer for the top row
  QHBox *topSpacer = new QHBox(topHBox);
  topHBox->setStretchFactor(topSpacer, 1);

  // Make second row for size spin boxes, and signal map them
  QHBox *botHBox = new QHBox(topVBox, "botHBox");
  QSignalMapper *sizeMapper = new QSignalMapper(botHBox);
  connect(sizeMapper, SIGNAL(mapped(int)), this, SLOT(sizeChanged(int)));

  // Make the spin boxes
  for (j = 0; j < 3; j++) {
    str = xyzLabels[j];
    label = new QLabel(str, botHBox);
    label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    mSizeBoxes[j] = new QSpinBox(XTUM_SIZE_MIN, XTUM_SIZE_MAX, XTUM_SIZE_INC,
				 botHBox);
    mSizeBoxes[j]->setValue(XTUM_SIZE_INI);
    mSizeBoxes[j]->setFocusPolicy(QWidget::ClickFocus);
    sizeMapper->setMapping(mSizeBoxes[j], j);
    connect(mSizeBoxes[j], SIGNAL(valueChanged(int)), sizeMapper, SLOT(map()));
    QToolTip::add(mSizeBoxes[j], "Change size of box in " + str);
  }

  // Spacer for the second row
  QHBox *botSpacer = new QHBox(botHBox);
  botHBox->setStretchFactor(botSpacer, 1);
  topVBox->setStretchFactor(botHBox, 1);

  // Add a vertical line
  QFrame *vertLine = new QFrame(topFrame);
  vertLine->setFrameStyle(QFrame::Sunken | QFrame::VLine);
  topLayout->addWidget(vertLine);

  // Threshold sliders
  mSliders = new MultiSlider(topFrame, 2, sliderLabels);
  topLayout->addLayout(mSliders->getLayout());
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
	  SLOT(thresholdChanged(int, int, bool)));
  mSliders->setValue(0, xtum->minval);
  mSliders->setValue(1, xtum->maxval);
  QToolTip::add((QWidget *)mSliders->getSlider(0), 
		"Level below which pixels will be set to black");
  QToolTip::add((QWidget *)mSliders->getSlider(1), 
		"Level above which pixels will be set to white");


  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new TumblerGL(xtum, glFormat, central);

  central->setStretchFactor(mGLw, 1);

  resize(XTUM_WIDTH, XTUM_HEIGHT);
  setFocusPolicy(QWidget::StrongFocus);
}

void TumblerWindow::setFontDependentWidths()
{
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

// Set up one toggle button, making bitmaps for the two state
void TumblerWindow::setupToggleButton(QHBox *toolBar, QSignalMapper *mapper,
				    int ind)
{
  if (firstTime) {
    bitmaps[ind][0] = new QBitmap(BM_WIDTH, BM_HEIGHT, bitList[ind][0], true);
    bitmaps[ind][1] = new QBitmap(BM_WIDTH, BM_HEIGHT, bitList[ind][1], true);
  }
  mToggleButs[ind] = new QToolButton(toolBar, "toolbar toggle");
  mToggleButs[ind]->setPixmap(*bitmaps[ind][0]);
  mToggleButs[ind]->setAutoRaise(AUTO_RAISE);
  mapper->setMapping(mToggleButs[ind],ind);
  connect(mToggleButs[ind], SIGNAL(clicked()), mapper, SLOT(map()));
  mToggleStates[ind] = 0;
}

// Respond to change in zoom spin box
void TumblerWindow::zoomChanged(int value)
{
  setFocus();
  mTum->zoom = value;
  setSlice(mTum);
  draw(mTum);
}

// Respond to change in threshold sliders
void TumblerWindow::thresholdChanged(int which, int value, bool dragging)
{
  // Forbid the sliders from crossing by asserting other value +/-1
  if (which) {
    if (value <= mTum->minval) {
      value = mTum->minval + 1;
      mSliders->setValue(1, value);
    }
    mTum->maxval = value;

  } else {
    if (value >= mTum->maxval) {
      value = mTum->maxval - 1;
      mSliders->setValue(0, value);
    }
    mTum->minval = value;
  }
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed)) {
    newData(mTum);
    draw(mTum);
  }
}

// Respond to change in size through spin boxes
void TumblerWindow::sizeChanged(int which)
{
  setFocus();
  if (!which)
    mTum->nx = mSizeBoxes[0]->value();
  else if (which == 1)
    mTum->ny = mSizeBoxes[1]->value();
  else
    mTum->nz = mSizeBoxes[2]->value();
  mTum->ms = (int)(sqrt((double)(mTum->nx * mTum->nx + mTum->ny * mTum->ny +
				mTum->nz * mTum->nz)) + 0.9);
  setSlice(mTum);
  draw(mTum);
}

void TumblerWindow::help()
{
  dia_vasmsg
    ("Tumbler Window Help\n",
     "---------------------------------------------------------\n",
     "The tumbler window displays a projected view of a rectangular portion "
     "of the image volume.  The subset is initially a small cube but the size "
     "can be changed with the X, Y and Z spin button controls.  The volume "
     "can be rotated with the keypad arrow keys just as in 3dmodv.\n\n"
     
     "\nToolbar Controls:\n",
     "\tThe zoom spin box lets you increase, decrease, or type in a zoom "
     "factor.\n",
     "\tThe checkerboard toggles between low resolution rendering with "
     "nearest-neighbor interpolation and high-resolution rendering with "
     "quadratic interpolation.\n",
     "\tThe lock button will lock the X/Y/Z position of the volume."
     "\tThe X, Y, and Z spin controls allow you to change the size of any "
     "dimension of the volume.\n"
     "\tThe Black and White threshold sliders allow you to cut out "
     "extraneous image material by setting all pixels in the volume with "
     "values below the black threshold to 0, and all pixels with values above "
     "the white threshold to 255.\n\n",
     "Hot Keys Specific to the Tumbler Window:\n",
     "\ts\tToggle stereo mode.\n",
     "\tb\tToggle display of bounding box.\n",
     "\t-/=\tDecrease/Increase zoom factor.\n",
     "\tF5/F6\tDecrease/Increase black threshold level.\n",
     "\tF7/F8\tDecrease/Increase white threshold level.\n",
     "\t,/.\tDecrease/Increase angular increment when rotating.\n"
     "\tS\tMake RGB snapshot of image in window.\n"
     "\t"CTRL_STRING"-S\tMake TIFF snapshot of image in window.\n"
     "\tKeypad up and down arrows rotate the volume around the X axis.\n",
     "\tKeypad left and right arrows rotate the volume around the Y axis.\n",
     "\tKeypad PgUp and PgDn rotate the volume around the Z axis.\n",
     "\tEscape\tClose window.\n\n",
     "All other keys that are common to image display windows perform their "
     "normal functions.",
     NULL);
}

// Respond to change in one of the toggle buttons
void TumblerWindow::toggleClicked(int index)
{
  int state = 1 - mToggleStates[index];
  mToggleStates[index] = state;
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
  if (!index) {

    // High res button toggled
    mTum->highres = state;
    setSlice(mTum);
    draw(mTum);
  } else {

    // Lock button toggled: draw if unlocking, set new position
    mTum->locked = state;
    if (!state) {
      mTum->cx = (int)mTum->vi->xmouse;
      mTum->cy = (int)mTum->vi->ymouse;
      mTum->cz = (int)(mTum->vi->zmouse + 0.5);
      newData(mTum);
      draw(mTum);
    }
  }
}


/*
 * Key handlers
 */
void TumblerWindow::externalKeyEvent ( QKeyEvent * e, int released)
{
  if (!released)
    keyPressEvent(e);
}

void TumblerWindow::keyPressEvent ( QKeyEvent * event)
{
  TumblerStruct *xtum = mTum;
  float tstep = xtum->tstep;
  int key = event->key();
  int shift = event->state() & Qt::ShiftButton;
  int ctrl = event->state() & Qt::ControlButton;
  int keypad = event->state() & Qt::Keypad;
  int dodraw = 1;
  int handled = 1;
  int newdata = 1;
  float xrot, yrot, zrot;
  
  if (inputTestMetaKey(event))
    return;

  inputConvertNumLock(key, keypad);

  if (key == hotSliderKey()) {
    mCtrlPressed = true;
    grabKeyboard();
    return;
  }

  switch(key){
  case Qt::Key_B:
    xtum->bbox = 1 - xtum->bbox;
    newdata = 0;
    break;
               
  case Qt::Key_S:
    if (shift || ctrl){
      
      // Snapshots
      draw(xtum);
      if (shift)
        b3dAutoSnapshot("tumbler", SnapShot_RGB, NULL);
      else
        b3dAutoSnapshot("tumbler", SnapShot_TIF, NULL);
      dodraw = 0;
    } else {

      // Toggle stereo
      xtum->stereo = 1 - xtum->stereo;
      setSlice(xtum);
      newdata = 0;
    }
    break;

  case Qt::Key_Comma:
    xtum->tstep /= sqrt(2.);
    if (xtum->tstep < 1.)
      xtum->tstep = 1.;
    break;

  case Qt::Key_Period:
    xtum->tstep *= sqrt(2.);
    if (xtum->tstep > 16.)
      xtum->tstep = 16.;
    break;

  case Qt::Key_Equal:
    if (xtum->zoom < XTUM_MAX_ZOOM) {
      xtum->zoom++;
      diaSetSpinBox(mZoomBox, xtum->zoom);
      setSlice(xtum);
      newdata = 0;
    } else
      dodraw = 0;
    break;
  case Qt::Key_Minus:
    if (xtum->zoom > 1) {
      xtum->zoom--;
      diaSetSpinBox(mZoomBox, xtum->zoom);
      setSlice(xtum);
      newdata = 0;
    } else
      dodraw = 0;
    break;
    
  case Qt::Key_Next:
  case Qt::Key_Prior:
  case Qt::Key_Up:
  case Qt::Key_Down:
  case Qt::Key_Right:
  case Qt::Key_Left:
    if (keypad) {

      // Someone with a better eye than mine may see that the directions
      // of X and Y need to be reversed
      xrot = (key == Key_Up ? tstep : 0.) - (key == Key_Down ? tstep : 0.); 
      yrot = (key == Key_Right ? tstep : 0.) - (key == Key_Left ? tstep : 0.);
      zrot = (key == Key_Prior ? tstep : 0.) - (key == Key_Next ? tstep : 0.);
      computeRotation(xrot, yrot, zrot);
    } else
      handled = 0;
    break;
    
  case Qt::Key_F5:
    if (xtum->minval >= 3) {
      xtum->minval -= 3;
      mSliders->setValue(0, xtum->minval);
    } else
      dodraw = 0;
    break;
  case Qt::Key_F6:
    if (xtum->minval < xtum->maxval - 3) {
      xtum->minval += 3;
      mSliders->setValue(0, xtum->minval);
    } else
      dodraw = 0;
    break;
  case Qt::Key_F7:
    if (xtum->maxval > xtum->minval + 3) {
      xtum->maxval -= 3;
      mSliders->setValue(1, xtum->maxval);
    } else
      dodraw = 0;
    break;
  case Qt::Key_F8:
    if (xtum->maxval <= 255 -  3) {
      xtum->maxval += 3;
      mSliders->setValue(1, xtum->maxval);
    } else
      dodraw = 0;
    break;

  case Qt::Key_Escape:
    close();
    dodraw = 0;
    return;
    
  default:
    handled = 0;
    break;
  }

  // If key not handled, call the default processor
  if (!handled) {
    inputQDefaultKeys(event, xtum->vi);
    return;
  }
  
  // Do the draws as needed
  if (dodraw && newdata)
    newData(xtum);
  if (dodraw)
    draw(xtum);
}

void TumblerWindow::keyReleaseEvent ( QKeyEvent * e )
{
  if (mCtrlPressed)
    releaseKeyboard();
  mCtrlPressed = false;
}

// Change the rotation angles by a viewer-centered rotation
void TumblerWindow::computeRotation(float x, float y, float z)
{
  Imat *mato, *matp, *mat;
  double alpha, beta, gamma;

  mat = imodMatNew(3);
  mato = imodMatNew(3);
  matp = imodMatNew(3);

  imodMatId(mat);
  imodMatRot(mat, (double)z, b3dZ);
  imodMatRot(mat, (double)y, b3dY);
  imodMatRot(mat, (double)x, b3dX);

  /* Compute current rotation matrix */
  imodMatId(mato);
  imodMatRot(mato, (double)mTum->gamma, b3dZ);
  imodMatRot(mato, (double)mTum->beta, b3dY);
  imodMatRot(mato, (double)mTum->alpha, b3dX);

  /* Multiply by the new rotation, then get back to 3 angles */
  imodMatMult(mato, mat, matp);
  imodMatGetNatAngles(matp, &alpha, &beta, &gamma);
  mTum->alpha = alpha;
  mTum->beta = beta;
  mTum->gamma = gamma;

  imodMatDelete(mat);
  imodMatDelete(mato);
  imodMatDelete(matp);
}

// Respond to close event.  Have to remove control first
void TumblerWindow::closeEvent ( QCloseEvent * e )
{
  TumblerStruct *xtum = mTum;
  ivwRemoveControl(xtum->vi, xtum->ctrl);
  imodDialogManager.remove((QWidget *)xtum->dialog);
  sliceFree(xtum->slice);
  sliceFree(xtum->stslice);
  sliceFree(xtum->bwslice);
  sliceFree(xtum->count);
  b3dFreeCIImage(xtum->image);
  free(xtum);
  e->accept();
}

/*
 * DATA FILLING AND DRAWING ROUTINES
 */

void TumblerWindow::newData(TumblerStruct *xtum)
{
  setSteps(xtum);
  fillSlice(xtum);
  return;
}

void TumblerWindow::draw( TumblerStruct *xtum)
{
  mGLw->updateGL();
}

// Get slices for storage based on needed size of display
void TumblerWindow::setSlice(TumblerStruct *xtum)
{
  int xsize, ysize;
  if (xtum->slice)
    sliceFree(xtum->slice);
  if (xtum->stslice)
    sliceFree(xtum->stslice);
  if (xtum->bwslice)
    sliceFree(xtum->bwslice);
  if (xtum->count)
    sliceFree(xtum->count);
  if (xtum->highres){
    xsize = ysize = xtum->ms * xtum->zoom;
    if (xtum->stereo && xsize > xtum->width / 2)
	xsize = xtum->width / 2;
  }else{
    xsize = ysize = xtum->ms;
  }

  xtum->slice   = sliceCreate(xsize, ysize, SLICE_MODE_SHORT);
  xtum->stslice = sliceCreate(xsize, ysize, SLICE_MODE_SHORT);
  xtum->bwslice = sliceCreate(xsize, ysize, SLICE_MODE_BYTE);
  xtum->count   = sliceCreate(xsize, ysize, SLICE_MODE_SHORT);
  newData(xtum);
  return;
}


/* Fill the slice data */
void TumblerWindow::fillSlice(TumblerStruct *xtum)
{
  Islice *tsl;
  Ipoint tx,ty,tz;
  int i, iz;
  unsigned char **imdata;
  int vmnullvalue;

  /* Set up image pointer tables */
  vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
  if (ivwSetupFastAccess(xtum->vi, &imdata, vmnullvalue, &i))
    return;

  fillASlice(xtum);

  if (xtum->stereo){
    tx.x = xtum->xstep.x; tx.y = xtum->xstep.y; tx.z = xtum->xstep.z;
    ty.x = xtum->ystep.x; ty.y = xtum->ystep.y; ty.z = xtum->ystep.z;
    tz.x = xtum->zstep.x; tz.y = xtum->zstep.y; tz.z = xtum->zstep.z;
    tsl = xtum->slice;

    xtum->xstep.x = xtum->sxstep.x;
    xtum->xstep.y = xtum->sxstep.y;
    xtum->xstep.z = xtum->sxstep.z;
    xtum->ystep.x = xtum->systep.x;
    xtum->ystep.y = xtum->systep.y;
    xtum->ystep.z = xtum->systep.z;
    xtum->zstep.x = xtum->szstep.x;
    xtum->zstep.y = xtum->szstep.y;
    xtum->zstep.z = xtum->szstep.z;
    xtum->slice = xtum->stslice;

    fillASlice(xtum);

    xtum->stslice = xtum->slice;
    xtum->slice = tsl;

    xtum->xstep.x = tx.x;
    xtum->xstep.y = tx.y;
    xtum->xstep.z = tx.z;
    xtum->ystep.x = ty.x;
    xtum->ystep.y = ty.y;
    xtum->ystep.z = ty.z;
    xtum->zstep.x = tz.x;
    xtum->zstep.y = tz.y;
    xtum->zstep.z = tz.z;
  }
  return;
}

/* Really fill the slice data for one slice */
void TumblerWindow::fillASlice(TumblerStruct *xtum)
{
  unsigned short *sdata=(unsigned short *)xtum->slice->data.s;
  short *ndata = xtum->count->data.s;
  int xysize = xtum->slice->xsize * xtum->slice->ysize;
  int n;
  float xs, ys, zs, xt, yt, zt;
  float x, y, z;
  float xsx, xsy, xsz;
  float ysx, ysy, ysz;
  float zsx, zsy, zsz;
  float xtz, ytz, ztz;
  int isize, jsize;
  int ksize, xsize, ysize, zsize;
  int xi, yi, zi;
  int i, j, k;
  float zoomfac;
  int xmin,ymin,zmin,xmax,ymax,zmax;
  int val;
  float dx, dy, dz;
  float x1, x2, y1, y2, z1, z2;
  float a, b, c, d, e, f;
  int pxi, nxi, pyi, nyi, pzi, nzi, cindex;
  int maxval = xtum->maxval;
  int minval = xtum->minval;
  float dsum;
  int nsum, shortcut, izoom, ilimshort, jlimshort, ishort, fillval, nmax;
  float zoom = xtum->zoom;

  if (xtum->slice->xsize <= 0)
    return;
  if (xtum->slice->ysize <= 0)
    return;

  zoomfac = xtum->highres ? zoom : 1.0;
  xmin = xtum->cx - xtum->nx/2;
  if (xmin < 0) xmin = 0;
  ymin = xtum->cy - xtum->ny/2;
  if (ymin < 0) ymin = 0;
  zmin = xtum->cz - xtum->nz/2;
  if (zmin < 0) zmin = 0;
  xmax = xtum->cx + xtum->nx/2;
  if (xmax >= xtum->vi->xsize)
    xmax = xtum->vi->xsize - 1;
  ymax = xtum->cy + xtum->ny/2;
  if (ymax >= xtum->vi->ysize)
    ymax = xtum->vi->ysize - 1;
  zmax = xtum->cz + xtum->nz/2;
  if (zmax >= xtum->vi->zsize)
    zmax = xtum->vi->zsize - 1;

  isize = xtum->slice->xsize;
  jsize = xtum->slice->ysize;
  ksize = xtum->ms;

  xsize = xtum->vi->xsize;
  ysize = xtum->vi->ysize;
  zsize = xtum->vi->zsize;

  xsx = xtum->xstep.x/zoomfac;
  xsy = xtum->xstep.y/zoomfac; 
  xsz = xtum->xstep.z/zoomfac;
     
  ysx = xtum->ystep.x/zoomfac; 
  ysy = xtum->ystep.y/zoomfac; 
  ysz = xtum->ystep.z/zoomfac;

  /* Steps in Z are independent of highres */
  zsx = xtum->zstep.x; 
  zsy = xtum->zstep.y;
  zsz = xtum->zstep.z;

  /* DNM 4/29/04: rewrote for compactness and clarity */
  xs = xtum->cx - 0.5f * (xsx * isize + ysx * jsize + zsx * xtum->ms);
  ys = xtum->cy - 0.5f * (xsy * isize + ysy * jsize + zsy * xtum->ms);
  zs = xtum->cz - 0.5f * (xsz * isize + ysz * jsize + zsz * xtum->ms);

  /* imodPrintStderr("xyzs %f %f %f\n", xs, ys, zs); */
  x = xs; y = ys; z = zs;

  shortcut = 0;
  izoom = (int)zoom;
  if (xtum->highres && (zoom == izoom) && (zoom > 1.0)) {
    shortcut = 1;
    ilimshort = izoom * ((isize - 1) / izoom - 1);
    jlimshort = izoom * ((jsize - 1) / izoom - 1);
  } else
    izoom = 1;

  for(n = 0; n < xysize; n++) {
    sdata[n] = 0;
    ndata[n] = 0;
  }

  for(k = 0; k < ksize; k++){
    xtz = x; ytz = y; ztz = z;
    for(j = 0, n = 0; j < jsize; j++){
      xt = x; yt = y; zt = z;
      cindex = j * isize;
      for(i = 0; i < isize; i++){

        /* DNM 2/25/03: eliminate floor as in slicer */
        xi = (int)x;
        yi = (int)y;
        zi = (int)(z + 0.5);

        if ((xi >= xmin) && (xi <= xmax) &&
            (yi >= ymin) && (yi <= ymax) &&
            (zi >= zmin) && (zi <= zmax)
            ){
          val = (*ivwFastGetValue)(xi, yi, zi);
                         
	  if (xtum->highres) {
	    dx = x - xi - 0.5;
	    dy = y - yi - 0.5;
	    dz = z - zi;
	    
	    pxi = xi - 1;
	    nxi = xi + 1;
	    pyi = yi - 1;
	    nyi = yi + 1;
	    pzi = zi - 1;
	    nzi = zi + 1;
	    
	    if (pxi < 0) pxi = 0;
	    if (nxi >= xsize) nxi = xi;
	    if (pyi < 0) pyi = 0;
	    if (nyi >= ysize) nyi = yi;
	    if (pzi < 0) pzi = 0;
	    if (nzi >= zsize) nzi = zi;
	    
	    
	    x1 = (*ivwFastGetValue)(pxi,  yi,  zi);
	    x2 = (*ivwFastGetValue)(nxi,  yi,  zi);
	    y1 = (*ivwFastGetValue)( xi, pyi,  zi);
	    y2 = (*ivwFastGetValue)( xi, nyi,  zi);
	    z1 = (*ivwFastGetValue)( xi,  yi, pzi);
	    z2 = (*ivwFastGetValue)( xi,  yi, nzi);
	    
	    a = (x1 + x2) * 0.5f - (float)val;
	    b = (y1 + y2) * 0.5f - (float)val;
	    c = (z1 + z2) * 0.5f - (float)val;
	    d = (x2 - x1) * 0.5f;
	    e = (y2 - y1) * 0.5f;
	    f = (z2 - z1) * 0.5f;
	    val = (int) ((a * dx * dx) + (b * dy * dy) + (c * dz * dz)
	      + (d * dx) + (e * dy) + (f * dz) + (float)val + 0.5);
	  }
          if (val > maxval)
            val = maxval;
          if (val < minval)
            val = minval;
          sdata[cindex + i] += (unsigned short)val;
          ndata[cindex + i]++;
        }
        x += xsx;
        y += xsy;
        z += xsz;

	/* If taking short cut, advance the source position to get to next
	 unzoomed pixel position */
        if (shortcut != 0 && ((i >= izoom && j % izoom == 0) ||
                              (i >= izoom - 1 && j % izoom != 0))
            && i < ilimshort && j >= izoom && j < jlimshort) {
          ishort = izoom - 1;
          if (j % izoom)
            ishort = ilimshort - izoom;
          x += xsx * ishort;
          y += xsy * ishort;
          z += xsz * ishort;
          i += ishort;
        }
      }
      x = xt + ysx;
      y = yt + ysy;
      z = zt + ysz;
    }
    x = xtz + zsx;
    y = ytz + zsy;
    z = ztz + zsz;
  }

  nmax = 0;
  nsum = 0;
  dsum = 0.;
  for (n = 0; n < xysize; n++) {
    if (nmax < ndata[n])
      nmax = ndata[n];
    nsum += ndata[n];
    dsum += sdata[n];
  }
  fillval = (int)(dsum / nsum + 0.5);

  for(n = 0; n < xysize; n++)
    sdata[n] += (nmax - ndata[n]) * fillval;

  xtum->fillval = fillval * nmax;

  if (shortcut)
    slicerCubicFillin(sdata, isize, jsize, izoom, ilimshort, jlimshort,
    0, 65535);

  return;
}


/* Scale the data in a slice */
void TumblerWindow::scaleData(TumblerStruct *xtum, unsigned short *sdata)
{
  unsigned int i;
  unsigned int xysize = xtum->slice->xsize * xtum->slice->ysize;
  float scale, offset;
   int min = 65535;
  int max = 0;
  int backoff = 32;
  
  /* Take the min and max of all pixels that are not pure fill pixels */
  /* (Using mean and standard deviation did not work with thresholding) */
  for (i = 0; i < xysize; i++) {
    if ((int)sdata[i] > max)
      max = sdata[i];
    if (sdata[i] != xtum->fillval && (int)sdata[i] < min)
      min = sdata[i];
  }

  if ((max - min) > 0) {
    scale = (256.0 - 2 * backoff) / (max - min);
    offset = -min + backoff / scale;

  } else {
    scale = 1.0f;
    offset = -min;
  }
     
  /* imodPrintStderr("min %d  max %d  scale %f  offset %f\n", min, max,
     scale, offset); */
  xtum->scale = scale;
  xtum->offset = offset;
  return;
}

/* Draw the image data; first regular then stereo */
void TumblerWindow::drawImage(TumblerStruct *xtum)
{
  unsigned short *sdata;
  int width = xtum->width;

  sdata = (unsigned short *)xtum->slice->data.s;

  if (xtum->stereo)
    width = xtum->width/2;
     
  drawSubArea(xtum, sdata, 0, width);

  if (xtum->stereo){
    sdata = (unsigned short *)xtum->stslice->data.s;
    drawSubArea(xtum, sdata, width, xtum->width);
  }
  return;
}

/* Draw one of the image arrays in a subarea of the window */
void TumblerWindow::drawSubArea(TumblerStruct *xtum, unsigned short *sdata,
				    int llx, int urx)
{
  float scale, offset;
  unsigned int i, xysize;
  unsigned char *data;
  float tf, tmax, tmin;
  int xo, yo;
  int zoom = xtum->zoom;

  xysize = xtum->slice->xsize * xtum->slice->ysize;
  data = xtum->bwslice->data.b;

  scaleData(xtum, sdata);

  tmax = 255.0f;
  tmin = 0.0f;
  scale  = xtum->scale;
  offset = xtum->offset;
     
  /* DNM: scale for depth 8 only if not rgba */
  if (App->depth == 8 && !App->rgba){
    scale *= xtum->vi->rampsize/256.0f;
    tmax = xtum->vi->rampbase + xtum->vi->rampsize;
    tmin = xtum->vi->rampbase;
    for(i = 0; i < xysize; i++){
      tf = ((sdata[i] + offset) * scale) + xtum->vi->rampbase;
      if (tf < tmin) tf = tmin;
      if (tf > tmax) tf = tmax;
      data[i] = (unsigned char)tf;
    }
  }else{
     
    for(i = 0; i < xysize; i++){
      tf = (sdata[i] + offset) * scale;
      if (tf < tmin) tf = tmin;
      if (tf > tmax) tf = tmax;
      data[i] = (unsigned char)tf;
    }
  }

  if (xtum->highres)
    zoom = 1;

  /* DNM 1/20/02: add slice argument to graphics calls; make it -1 to 
     prevent image re-use */
  
  b3dDrawGreyScalePixelsSubArea
    (xtum->image, ivwMakeLinePointers(xtum->vi, data, xtum->slice->xsize,
                                      xtum->slice->ysize, MRC_MODE_BYTE),
     xtum->slice->xsize, xtum->slice->ysize,
     0, 0, llx, 0, urx, xtum->height,
     xtum->vi->rampbase, zoom,
     &xo, &yo, -1);
}

/* Draw the bounding box */
void TumblerWindow::drawBBox(TumblerStruct *xtum)
{
  Imat *mat;
  Ipoint transp;
  int width = xtum->width;

  if (!xtum->bbox)
    return;

  mat = imodMatNew(3);
  b3dColorIndex(App->foreground);     
  if (xtum->stereo)
    width/=2;

  transp.x = transp.y = transp.z = xtum->zoom;
  imodMatScale(mat, &transp);      
  transp.x = ((float)width *0.5f);
  transp.y = ((float)xtum->height*0.5f);
  transp.z = 0;

  imodMatRot(mat, (double)xtum->gamma, 2);
  imodMatRot(mat, (double)xtum->beta,  1);
  imodMatRot(mat, (double)xtum->alpha, 0);
  imodMatTrans(mat, &transp);

  drawBoxLines(xtum, mat);

  if (xtum->stereo){
    imodMatId(mat);
    transp.x = transp.y = transp.z = xtum->zoom;
    imodMatScale(mat, &transp);
    imodMatRot(mat, (double)xtum->gamma, 2);
    imodMatRot(mat, (double)xtum->beta + xtum->plax,  1);
    imodMatRot(mat, (double)xtum->alpha, 0);
    transp.x = ((float)width * 1.5f);
    transp.y = ((float)xtum->height*0.5f);
    transp.z = 0;
    imodMatTrans(mat, &transp);

    drawBoxLines(xtum, mat);
  }
  imodMatDelete(mat);

  return;
}

/* Draw the actual lines of one bounding box */
void TumblerWindow::drawBoxLines(TumblerStruct *xtum, Imat *mat)
{
  Ipoint inp, outp1, outp2;
  int x2 = xtum->nx/2;
  int y2 = xtum->ny/2;
  int z2 = xtum->nz/2;

  inp.x = x2; inp.y = y2; inp.z = z2;
  imodMatTransform(mat, &inp, &outp1);
  inp.x = -x2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  /*     imodPrintStderr("line %d,%d to %d,%d\n",
         (int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);*/
  inp.x = x2; inp.y = -y2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.y = y2; inp.z = -z2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

  inp.x = -x2; inp.y = -y2; inp.z = -z2;
  imodMatTransform(mat, &inp, &outp1);
  inp.x = x2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.x = -x2; inp.y = y2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.y = -y2; inp.z = z2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

  inp.x = -x2; inp.y = y2; inp.z = -z2;
  imodMatTransform(mat, &inp, &outp1);
  inp.z = z2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.y = -y2;
  imodMatTransform(mat, &inp, &outp1);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.x = x2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

  inp.x = -x2; inp.y = y2; inp.z = -z2;
  imodMatTransform(mat, &inp, &outp1);
  inp.x = x2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.y = -y2;
  imodMatTransform(mat, &inp, &outp1);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
  inp.z = z2;
  imodMatTransform(mat, &inp, &outp2);
  b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

  return;
}

/* Set up the steps for walking through the data */
/* DNM 4/29/04: move calculation into a routine that can be called for both 
   cases */
void TumblerWindow::setSteps(TumblerStruct *xtum)
{
  setStepsPoints(xtum, &xtum->xstep, &xtum->ystep, &xtum->zstep, xtum->beta);

  if (xtum->stereo)
    setStepsPoints(xtum, &xtum->sxstep, &xtum->systep, &xtum->szstep, 
                   xtum->beta + xtum->plax);
}

/* Set up the steps for the given variables and beta rotation */
void TumblerWindow::setStepsPoints(TumblerStruct *xtum, Ipoint *xstep, 
                                   Ipoint *ystep, Ipoint *zstep, float beta)
{
  Imat *mat;
  Ipoint inp, outp, transp;

  inp.x = inp.y = inp.z = 0.0f;
  mat = imodMatNew(3);

  /* DNM 1/10/03: for whatever reason, gamma needs to be treated with a minus
     sign here to have rotations match those of the bounding box */
  transp.x = xtum->cx;
  transp.y = xtum->cy;
  transp.z = xtum->cz;
  imodMatTrans(mat, &transp);
  imodMatRot(mat, (double)xtum->alpha, 0);
  imodMatRot(mat, (double)beta,  1);
  imodMatRot(mat, -(double)xtum->gamma, 2);
  transp.x *= -1.0f; transp.y *= -1.0f; transp.z *= -1.0f;
  imodMatTrans(mat, &transp);
  imodMatTransform(mat, &inp, &outp);
  inp.x = 1.0f;
  imodMatTransform(mat, &inp, xstep);
  inp.y = 1.0f; inp.x = 0.0f;
  imodMatTransform(mat, &inp, ystep);
  inp.z = 1.0f; inp.y = 0.0f;
  imodMatTransform(mat, &inp, zstep);
  xstep->x -= outp.x; xstep->y -= outp.y; xstep->z -= outp.z;
  ystep->x -= outp.x; ystep->y -= outp.y; ystep->z -= outp.z;
  zstep->x -= outp.x; zstep->y -= outp.y; zstep->z -= outp.z;

  setSmallStepZero(xstep);
  setSmallStepZero(ystep);
  setSmallStepZero(zstep);
  /*  imodPrintStderr("xstep %g %g %g  ystep %g %g %g\n zstep %g %g %g\n", 
         xstep->x, xstep->y, xstep->z, ystep->x, ystep->y, ystep->z,
         zstep->x, zstep->y, zstep->z); */
  imodMatDelete(mat);
}

/* Set very small steps exactly zero to avoid lurching back and forth when
   rotating around an axis */
void TumblerWindow::setSmallStepZero(Ipoint *xstep)
{
  if (fabs((double)xstep->x) < 1.e-5)
    xstep->x = 0.;
  if (fabs((double)xstep->y) < 1.e-5)
    xstep->y = 0.;
  if (fabs((double)xstep->z) < 1.e-5)
    xstep->z = 0.;
}

/* Draw borders around the images */
void TumblerWindow::drawBorder(TumblerStruct *xtum)
{
  int cx, cy, gx, gy, sx, ty;
  cx = xtum->width/2;
  gx = (xtum->ms * xtum->zoom)/2;
  cy =  xtum->height/2;
  gy = (xtum->ms * xtum->zoom)/2;

  // Clear up top edge
  ty = cy + gy;
  if (ty > xtum->height - 2)
    ty = xtum->height - 2;
     
  b3dColorIndex(App->background);
     
     
  if (!xtum->stereo){
    b3dDrawBoxout(cx-gx, cy-gy, cx + gx, ty);
  }else{
    b3dDrawBoxout((cx/2)-gx, cy-gy, ((cx/2)*3)+gx ,ty);
    sx = (cx/2)-gx;
    if (sx >= -1) {
      b3dDrawFilledRectangle(cx - sx - 2, cy-gy, sx*2 + 4,  gy*2);
    }
  }
  return;
}


/*
 * THE GL WIDGET CLASS 
 */
TumblerGL::TumblerGL(TumblerStruct *xtum, QGLFormat format, QWidget * parent,
        const char * name)
  : QGLWidget(format, parent, name)
{
  mTum = xtum;
}

// Resize comes in: record size, set viewport, and get new arrays
void TumblerGL::resizeGL( int wdth, int hght )
{
  mTum->width = wdth;
  mTum->height = hght;
  b3dResizeViewportXY(wdth, hght);
  mTum->image   = b3dGetNewCIImageSize(mTum->image, App->depth, wdth, hght);
  if (!mTum->image) {
    wprint("\aInsufficient memory to run this Tumbler window.\n"
           "Please close it\n");
    return;
  }

  mTum->dialog->setSlice(mTum);
}

// Paint command: set up conditions and call drawing routines
void TumblerGL::paintGL()
{
  b3dSetCurSize(mTum->width, mTum->height);
  b3dColorIndex(mTum->vi->rampbase);

  mTum->dialog->drawImage(mTum);
  mTum->dialog->drawBBox(mTum);
  mTum->dialog->drawBorder(mTum);
}

/*
$Log$
Revision 4.18  2004/09/10 02:31:03  mast
replaced long with int

Revision 4.17  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.16  2004/05/03 02:34:20  mast
fix wobble due to truncation error near zero degrees

Revision 4.15  2004/03/12 19:27:43  mast
Fixed bug in Z size changes; it was taking Y value

Revision 4.14  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.13  2003/09/16 02:11:18  mast
Changed to access image data using new line pointers

Revision 4.12  2003/04/28 04:01:40  mast
Fx hot key text

Revision 4.11  2003/04/25 03:28:33  mast
Changes for name change to 3dmod

Revision 4.10  2003/04/18 20:16:39  mast
Rename meta test function

Revision 4.9  2003/04/18 20:06:21  mast
Reject the Ctrl (meta) key on the Mac

Revision 4.8  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.7  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.6  2003/03/26 17:15:30  mast
Adjust sizes for font changes

Revision 4.5  2003/03/24 17:56:46  mast
Register with dialogManager so it can be parked with info window

Revision 4.4  2003/03/13 01:20:08  mast
Convert numlock keypad keys so num lock can be on

Revision 4.3  2003/03/12 21:35:23  mast
Test if no CIImage is returned and give error message

Revision 4.2  2003/02/27 19:40:46  mast
remove slow floor() calls

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.3  2003/01/29 01:34:08  mast
implement colormaps

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/10 23:47:49  mast
Qt version and many general fixes and enhancements

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/01/28 16:53:10  mast
Added slice argument to calls to b3dDrawGreyScalePixelsSubArea

*/

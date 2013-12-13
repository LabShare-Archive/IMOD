/*  
 *  xtum.cpp -- The tumbler window
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
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
#include <qbitmap.h>
#include <qtooltip.h>
#include <qslider.h>
//Added by qt3to4:
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QCloseEvent>
#include "multislider.h"
#include "preferences.h"
#include "dia_qtutils.h"

#include "imod.h"
#include "sslice.h"
#include "xtum.h"
#include "control.h"
#include "pyramidcache.h"

#include "imod_input.h"
#include "b3dgfx.h"

#define XTUM_WIDTH 200
#define XTUM_HEIGHT 250
#define XTUM_SIZE_MIN 8.
#define XTUM_SIZE_MAX 148.
#define XTUM_SIZE_INC 8.
#define XTUM_SIZE_INI 48
#define XTUM_MAX_ZOOM 12.


static void xtumClose_cb(ImodView *vi, void *client, int junk);
static void xtumDraw_cb(ImodView *vi, void *client, int drawflag);
static void xtumKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e);

static const char *fileList[MAX_XTUM_TOGGLES][2] =
  { {":/images/lowres.png", ":/images/highres.png"},
    {":/images/unlock.png", ":/images/lock.png"}};

static QIcon *icons[MAX_XTUM_TOGGLES];
static int firstTime = 1;

static const char *toggleTips[] = {"Toggle between low and high resolution.",
                             "Lock X/Y/Z position being displayed"};

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
  xtum->closing = 0;
     
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

  xtum->dialog->setWindowTitle(imodCaption("3dmod Tumbler"));
  xtum->ctrl = ivwNewControl(vi, xtumDraw_cb, xtumClose_cb, xtumKey_cb, (void *)xtum);
  imodDialogManager.add((QWidget *)xtum->dialog, IMOD_IMAGE, TUMBLER_WINDOW_TYPE, 
                        xtum->ctrl);
  adjustGeometryAndShow((QWidget *)xtum->dialog, IMOD_IMAGE, false);
     
  return(0);
}

/*
 * Constructor for the tumbler class 
 */
static const char *xyzLabels[] = {"X", "Y", "Z"};
static const char *sliderLabels[] = {"Black Threshold", "White Threshold"};

TumblerWindow::TumblerWindow(TumblerStruct *xtum, bool rgba,
            bool doubleBuffer, bool enableDepth, QWidget * parent,
            const char * name, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  int j;
  QString str;
  mTum = xtum;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);

  if (firstTime) 
    utilFileListsToIcons(fileList, icons, MAX_XTUM_TOGGLES);
  firstTime = 0;
  
  // Make central vbox and top frame containing an hboxlayout
  QWidget *central = new QWidget(this);
  setCentralWidget(central);
  QVBoxLayout *cenlay = new QVBoxLayout(central);
  cenlay->setContentsMargins(0,0,0,0);
  cenlay->setSpacing(0);
  QFrame * topFrame = new QFrame(central);
  cenlay->addWidget(topFrame);
  topFrame->setFrameStyle(QFrame::Raised | QFrame::StyledPanel);

  QHBoxLayout *topLayout = new QHBoxLayout(topFrame);
  topLayout->setContentsMargins(2,2,2,2);
  topLayout->setSpacing(3);
  
  QVBoxLayout *topVBox = diaVBoxLayout(topLayout);
  topVBox->setSpacing(4);
  QHBoxLayout *topHBox = diaHBoxLayout(topVBox);
  topHBox->setContentsMargins(0,0,0,0);
  topHBox->setSpacing(3);

  // Add the toolbar widgets
  // Zoom spin box
  // If you try to make a spin box narrower, it makes the arrows tiny
  mZoomBox = (QSpinBox *)diaLabeledSpin(0, 1., XTUM_MAX_ZOOM, 1., "Zoom",
                                        topFrame, topHBox);
  mZoomBox->setValue(xtum->zoom);
  connect(mZoomBox, SIGNAL(valueChanged(int)), this, 
	  SLOT(zoomChanged(int)));
  mZoomBox->setToolTip("Change zoom of display");

  // Make the 2 toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(topFrame);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < 2; j++) {
    utilSetupToggleButton(topFrame, NULL, topHBox, toggleMapper, icons, 
                          toggleTips, mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));
  }

  // Help button
  mHelpButton = diaPushButton("Help", topFrame, topHBox);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  setFontDependentWidths();

  topHBox->addStretch();

  // Make second row for size spin boxes, and signal map them
  QHBoxLayout *botHBox = diaHBoxLayout(topVBox);
  botHBox->setContentsMargins(0,0,0,0);
  botHBox->setSpacing(3);
  QSignalMapper *sizeMapper = new QSignalMapper(topFrame);
  connect(sizeMapper, SIGNAL(mapped(int)), this, SLOT(sizeChanged(int)));

  // Make the spin boxes
  for (j = 0; j < 3; j++) {
    str = xyzLabels[j];
    mSizeBoxes[j] = (QSpinBox *)diaLabeledSpin(0, XTUM_SIZE_MIN, XTUM_SIZE_MAX,
                                               XTUM_SIZE_INC, LATIN1(str), 
                                               topFrame, botHBox);
    mSizeBoxes[j]->setValue(XTUM_SIZE_INI);
    sizeMapper->setMapping(mSizeBoxes[j], j);
    connect(mSizeBoxes[j], SIGNAL(valueChanged(int)), sizeMapper, SLOT(map()));
    mSizeBoxes[j]->setToolTip("Change size of box in " + str);
  }

  // Spacer for the second row
  botHBox->addStretch();

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
  mSliders->getSlider(0)->setToolTip(
		"Level below which pixels will be set to black");
  mSliders->getSlider(1)->setToolTip(
		"Level above which pixels will be set to white");


  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new TumblerGL(xtum, glFormat, central);
  cenlay->addWidget(mGLw);
  cenlay->setStretchFactor(mGLw, 1);

  resize(XTUM_WIDTH, XTUM_HEIGHT);
  setFocusPolicy(Qt::StrongFocus);
}

void TumblerWindow::setFontDependentWidths()
{
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

void TumblerWindow::changeEvent(QEvent *e)
{
  QMainWindow::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

// Respond to change in zoom spin box
void TumblerWindow::zoomChanged(int value)
{
  if (mTum->closing)
    return;
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
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)) {
    newData(mTum);
    draw(mTum);
  }
}

// Respond to change in size through spin boxes
void TumblerWindow::sizeChanged(int which)
{
  if (mTum->closing)
    return;
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
  imodShowHelpPage("tumbler.html#TOP");
}

// Respond to change in one of the toggle buttons
void TumblerWindow::toggleClicked(int index)
{
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state;
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
  int shift = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int dodraw = 1;
  int handled = 1;
  int newdata = 1;
  float xrot, yrot, zrot;
  
  if (inputTestMetaKey(event))
    return;

  if (utilCloseKey(event)) {
    close();
    return;
  }    

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
      b3dKeySnapshot("tumbler", shift, ctrl, NULL);
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

  case Qt::Key_Plus:
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
    
  case Qt::Key_PageDown:
  case Qt::Key_PageUp:
  case Qt::Key_Up:
  case Qt::Key_Down:
  case Qt::Key_Right:
  case Qt::Key_Left:
    if (keypad) {

      // Someone with a better eye than mine may see that the directions
      // of X and Y need to be reversed
      xrot = (key == Qt::Key_Up ? tstep : 0.) - 
        (key == Qt::Key_Down ? tstep : 0.); 
      yrot = (key == Qt::Key_Right ? tstep : 0.) - 
        (key == Qt::Key_Left ? tstep : 0.);
      zrot = (key == Qt::Key_PageUp ? tstep : 0.) - 
        (key == Qt::Key_PageDown ? tstep : 0.);
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
  xtum->closing = 1;
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
  xtum->bwslice = sliceCreate(xsize, ysize, xtum->vi->rawImageStore);
  xtum->count   = sliceCreate(xsize, ysize, SLICE_MODE_SHORT);
  newData(xtum);
  return;
}


/* Fill the slice data */
void TumblerWindow::fillSlice(TumblerStruct *xtum)
{
  Islice *tsl;
  Ipoint tx,ty,tz;
  int i;
  unsigned char **imdata;
  int vmnullvalue;

  /* Set up image pointer tables */
  vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
  if (xtum->vi->pyrCache) {
    if (ivwSetupFastTileAccess(xtum->vi, xtum->vi->pyrCache->getBaseIndex(), vmnullvalue,
                               i))
      return;
  } else {
    if (ivwSetupFastAccess(xtum->vi, &imdata, vmnullvalue, &i, xtum->vi->curTime))
      return;
  }

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
  unsigned short *sdata;
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
  unsigned char *bmap = NULL;
  sdata = (unsigned short *)xtum->slice->data.s;

  if (xtum->slice->xsize <= 0)
    return;
  if (xtum->slice->ysize <= 0)
    return;

  if (xtum->vi->ushortStore) {
    bmap = ivwUShortInRangeToByteMap(xtum->vi);
    if (!bmap)
      return;
  }

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
          if (bmap)
            val = bmap[B3DMAX(0, B3DMIN(65535, val))];
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
    slicerCubicFillin(sdata, isize, jsize, izoom, ilimshort, jlimshort, 0, 65535, 0);
  B3DFREE(bmap);
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
  b3dUInt16 *usdata;
  float tf, tmax, tmin;
  int xo, yo;
  int zoom = xtum->zoom;

  xysize = xtum->slice->xsize * xtum->slice->ysize;
  data = xtum->bwslice->data.b;
  usdata = xtum->bwslice->data.us;

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
  } else if (xtum->vi->ushortStore) {
    tmax = 65535.;
    scale *= 256.;
    for (i = 0; i < xysize; i++) {
      tf = (sdata[i] + offset) * scale;
      usdata[i] = (b3dUInt16)B3DMAX(tmin, B3DMIN(tmax, tf));
    }
  } else {
    for(i = 0; i < xysize; i++){
      tf = (sdata[i] + offset) * scale;
      data[i] = (unsigned char)B3DMAX(tmin, B3DMIN(tmax, tf));
    }
  }

  if (xtum->highres)
    zoom = 1;

  /* DNM 1/20/02: add slice argument to graphics calls; make it -1 to 
     prevent image re-use */
  
  b3dDrawGreyScalePixelsSubArea
    (xtum->image, ivwMakeLinePointers(xtum->vi, data, xtum->slice->xsize,
                                      xtum->slice->ysize, xtum->vi->rawImageStore),
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
TumblerGL::TumblerGL(TumblerStruct *xtum, QGLFormat format, QWidget * parent)
  : QGLWidget(format, parent)
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

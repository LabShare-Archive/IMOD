/* 
 *  rescale.cpp -- Reload image with new scaling factors.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *  $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qframe.h>
#include <qtooltip.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QGridLayout>
#include <QKeyEvent>

#include "rescale.h"
#include "imod.h"
#include "display.h"
#include "imod_io.h"
#include "cachefill.h"
#include "info_cb.h"
#include "info_setup.h"
#include "form_info.h"
#include "control.h"
#include "preferences.h"
#include "dia_qtutils.h"
#include "xcramp.h"

static struct iscaleDataStruct{
  ImageScaleWindow *dia;
  ImodView  *vi;
  float     min, max;
  int blackNew, whiteNew;
} iisData = {NULL, NULL, 0, 0, 0, 0};

#define BLACKNEW  32
#define WHITENEW  223

void imodImageScaleDialog(ImodView *vi)
{
     
  if (iisData.dia){
    iisData.dia->raise();
    return;
  }
  iisData.blackNew = BLACKNEW * (vi->ushortStore ? 256 : 1);
  iisData.whiteNew = WHITENEW * (vi->ushortStore ? 256 : 1);

  iisData.vi = vi;
  if ((!vi->li->smin) && (!vi->li->smax)){
    vi->li->smin = vi->hdr->amin;
    vi->li->smax = vi->hdr->amax;
  }

  iisData.dia = new ImageScaleWindow
    (imodDialogManager.parent(IMOD_DIALOG), "image scale");

  imodDialogManager.add((QWidget *)iisData.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)iisData.dia, IMOD_DIALOG);
}

void imodImageScaleUpdate(ImodView *vi)
{
  if (iisData.dia) {
    iisData.dia->showFileAndMMM();
    // iisData.dia->updateLimits();  // May not want!
  }
}

/* The window class */

static const char *buttonLabels[] = {"Apply", "Calc", "Done", "Help"};
static const char *buttonTips[] = 
  {"Reload the image data with the new limits", 
   "Calculate lower and upper limits based on black/white sliders", 
   "Close dialog box",
   "Open help dialog"};


ImageScaleWindow::ImageScaleWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 4, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  const char *uplow[] = {"Lower", "Upper"};
  QString str;
  QLabel *label;

  mFileLabel = diaLabel("File:", this, mLayout);
  mMMMLabel = diaLabel("Min:", this, mLayout);
  QFrame *frame = new QFrame(this);
  mLayout->addWidget(frame);
  frame->setFrameStyle(QFrame::HLine | QFrame::Sunken);

  diaLabel("Limits for linear scaling:", this, mLayout);

  QGridLayout *grid = new QGridLayout();
  mLayout->addLayout(grid);

  for (int i = 0; i < 2; i++) {
    str = uplow[i];
    label = new QLabel(str + " limit", this);
    grid->addWidget(label, i, 0);
    mEditBox[i] = new QLineEdit(this);
    grid->addWidget(mEditBox[i], i, 1);
    mEditBox[i]->setToolTip("Enter new " + str + " limit for rescaling");
    connect(mEditBox[i],  SIGNAL(returnPressed()), this, SLOT(setFocus()));
  }

  computeScale();
  showFileAndMMM();
  updateLimits();

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
  setWindowTitle(imodCaption("3dmod Image Scale"));
}

// Respond to button press: apply, revert, set base, done
void ImageScaleWindow::buttonPressed(int which)
{
  QString str;
  setFocus();

  switch (which) {
  case 0:  // Apply
    mTimerID =startTimer(10);
    if (!mTimerID)
      applyLimits();
    break;

  case 1:  // Calc
    computeScale();
    updateLimits();
    break;

  case 2: // Done
    close();
    break;

  case 3: // Help
    imodShowHelpPage("imageScale.html#TOP");
    break;
  }
}

void ImageScaleWindow::timerEvent(QTimerEvent *e)
{
  killTimer(mTimerID);
  applyLimits();
}

// Update the labels in the dialog
void ImageScaleWindow::updateLimits()
{
  QString str;
  str.sprintf("%g", iisData.min);
  mEditBox[0]->setText(str);
  str.sprintf("%g", iisData.max);
  mEditBox[1]->setText(str);
}

void ImageScaleWindow::showFileAndMMM()
{
  int cz;
  ImodView *vi = iisData.vi;
  QString str;
  if (vi->multiFileZ > 0) {
    if (vi->loadingImage)
      return;
    cz = (int)(vi->zmouse + 0.5 + vi->li->zmin);
    if (cz >= 0 && cz < vi->zsize && vi->image != &vi->imageList[cz]) {
        iiClose(vi->image);
        vi->hdr = vi->image = &vi->imageList[cz];
        ivwReopen(vi->image);
    }
  }
  str.sprintf("File: %s", vi->image->filename);
  mFileLabel->setText(str);
  str.sprintf("Min: %g    Max: %g    Mean: %g",  vi->hdr->amin,
          vi->image->amax, vi->image->amean);
  mMMMLabel->setText(str);
}

void ImageScaleWindow::computeScale()
{
  ImodView *vi = iisData.vi;
  float slidecur, rangecur, slidenew, rangenew;
  float smin = vi->image->smin;
  float smax = vi->image->smax;
  float kscale = mrcGetComplexScale();
  float minSign = 1.;
  float slideMax = vi->ushortStore ? 65535. : 255.;

  // Convert smin, smax to actual values used for scaling if complex
  if (vi->image->format == IIFORMAT_COMPLEX)
    mrcComplexSminSmax(smin, smax, &smin, &smax);
  slidecur = vi->white - vi->black;
  rangecur = smax - smin;
  slidenew = iisData.whiteNew - iisData.blackNew;
  rangenew = slidecur * rangecur / slidenew;
  iisData.min = smin + (slidenew * rangenew / slideMax) *
    (vi->black/slidecur - iisData.blackNew/slidenew );
  iisData.max = iisData.min + rangenew;

  // Convert back for complex values
  if (vi->image->format == IIFORMAT_COMPLEX) {
    iisData.max = (float)(exp((double)iisData.max) - 1.) / kscale;
    if (iisData.min < 0.) {
      minSign = -1.;
      iisData.min = -iisData.min;
    }
    iisData.min = (float)(exp((double)iisData.min) - 1.) / (kscale * minSign);
  }
}

void ImageScaleWindow::applyLimits()
{
  ImodView *vi = iisData.vi;
  int black = iisData.blackNew, white = iisData.whiteNew;
  int k;

  /* Don't do it if someone else is busy loading */
  if (vi->loadingImage)
    return;

  iisData.min = mEditBox[0]->text().toFloat();
  iisData.max = mEditBox[1]->text().toFloat();

  /* DNM: the min and max will take care of all the scaling needs, so no longer
     need the li-black and li->white to be anything but 0 and 255 */
  /* DNM 1/3/04: cleanup li->black, white, ivwSetScale, doubles to iiSetMM */
  vi->black = black;
  vi->white = white;
  vi->li->smin = iisData.min;
  vi->li->smax = iisData.max;
     
  iiSetMM(vi->image, vi->li->smin, vi->li->smax, vi->ushortStore ? 65535. : 255.);
  if (vi->ushortStore)
    ImodInfoWidget->setLHSliders(vi->rangeLow, vi->rangeHigh, vi->image->smin, 
                                 vi->image->smax, vi-> image->type == IITYPE_FLOAT);

  /* For multi-file sections, apply to all the files */
  if (vi->multiFileZ > 0)
    for (k = 0; k < vi->zsize; k++)
      iiSetMM(&vi->imageList[k + vi->li->zmin], vi->li->smin, vi->li->smax,
          vi->ushortStore ? 65535. : 255.);

  /* Flip data back first if it is flipped and is either non-cache or
     full cache flipped */
  int reflip = 0;
  if (vi->li->axis == 2 && (!vi->vmSize || vi->fullCacheFlipped)) { 
    ivwFlip(vi);
    reflip = 1;
  }

  if (vi->vmSize) {

    /* If cached data, flush the cache in all cases for the current time */
    ivwFlushCache(vi, vi->curTime);

    /* Then reload if we are supposed to keep cache full */
    if (vi->keepCacheFull) {
      if (imodCacheFill(vi)) {
        imodError(NULL, "3DMOD: Fatal error rereading image file\n");
        exit(3);
      }
    }
  } else {
          
    /* Uncached data: free memory and reload */
    mrcFreeDataMemory(vi->idata, vi->li->contig, vi->zsize);

    /* DNM 1/7/04:  no need to reopen and reread header, image_load does it */

    vi->idata = imod_io_image_load(vi);
    if (!vi->idata) {
      imodError(NULL, "3DMOD: Fatal error rereading image file\n");
      exit(3);
    }
          
    if (App->depth == 8)
      ivwScale(vi);
  }
  
  /* reflip the data if it was unflipped */
  if (reflip)
    ivwFlip(vi);

  /* DNM: clear any information for floating windows for this time */
  imod_info_float_clear(-vi->zsize, vi->curTime);
  imod_info_setbw(black, white);     
  xcramp_setlevels(vi->cramp,black,white);

  imodDraw(vi, IMOD_DRAW_IMAGE);
}

void ImageScaleWindow::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
}

// The window is closing, remove from manager
void ImageScaleWindow::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)iisData.dia);
  iisData.dia = NULL;
  e->accept();
}

// Close on escape, pass on keys
void ImageScaleWindow::keyPressEvent ( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void ImageScaleWindow::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

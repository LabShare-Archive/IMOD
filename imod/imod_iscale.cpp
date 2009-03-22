/*  IMOD VERSION 2.20
 *
 *  imod_iscale.c -- Reload image with new scaling factors.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

Log at end of file
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

#include "imod_iscale.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_io.h"
#include "imod_cachefill.h"
#include "imod_info_cb.h"
#include "control.h"
#include "preferences.h"
#include "dia_qtutils.h"
#include "xcramp.h"

struct{
  ImageScaleWindow *dia;
  ImodView  *vi;
  float     min, max;
}imodImageScaleData = {NULL, NULL, 0, 0};

#define BLACKNEW  32
#define WHITENEW  223

void imodImageScaleDialog(ImodView *vi)
{
     
  if (imodImageScaleData.dia){
    imodImageScaleData.dia->raise();
    return;
  }

  imodImageScaleData.vi = vi;
  if ((!vi->li->smin) && (!vi->li->smax)){
    vi->li->smin = vi->hdr->amin;
    vi->li->smax = vi->hdr->amax;
  }

  imodImageScaleData.dia = new ImageScaleWindow
    (imodDialogManager.parent(IMOD_DIALOG), "image scale");

  imodDialogManager.add((QWidget *)imodImageScaleData.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)imodImageScaleData.dia, IMOD_DIALOG);

  return;
}

void imodImageScaleUpdate(ImodView *vi)
{
  if (imodImageScaleData.dia) {
    imodImageScaleData.dia->showFileAndMMM();
    // imodImageScaleData.dia->updateLimits();  // May not want!
  }
}

/* The window class */

static char *buttonLabels[] = {"Apply", "Calc", "Done", "Help"};
static char *buttonTips[] = 
  {"Reload the image data with the new limits", 
   "Calculate lower and upper limits based on black/white sliders", 
   "Close dialog box",
   "Open help dialog"};


ImageScaleWindow::ImageScaleWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 4, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  char *uplow[] = {"Lower", "Upper"};
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
    imodShowHelpPage("imageScale.html");
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
  str.sprintf("%g", imodImageScaleData.min);
  mEditBox[0]->setText(str);
  str.sprintf("%g", imodImageScaleData.max);
  mEditBox[1]->setText(str);
}

void ImageScaleWindow::showFileAndMMM()
{
  int cz;
  ImodView *vi = imodImageScaleData.vi;
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
  ImodView *vi = imodImageScaleData.vi;
  float slidecur, rangecur, slidenew, rangenew;
  float smin = vi->image->smin;
  float smax = vi->image->smax;
  float kscale = mrcGetComplexScale();
  float minSign = 1.;

  // Convert smin, smax to actual values used for scaling if complex
  if (vi->image->format == IIFORMAT_COMPLEX)
    mrcComplexSminSmax(smin, smax, &smin, &smax);
  slidecur = vi->white - vi->black;
  rangecur = smax - smin;
  slidenew = WHITENEW - BLACKNEW;
  rangenew = slidecur * rangecur / slidenew;
  imodImageScaleData.min = smin + (slidenew * rangenew / 255.0f) *
    (vi->black/slidecur - BLACKNEW/slidenew );
  imodImageScaleData.max = imodImageScaleData.min + rangenew;

  // Convert back for complex values
  if (vi->image->format == IIFORMAT_COMPLEX) {
    imodImageScaleData.max = (float)(exp((double)imodImageScaleData.max) -
                                     1.) / kscale;
    if (imodImageScaleData.min < 0.) {
      minSign = -1.;
      imodImageScaleData.min = -imodImageScaleData.min;
    }
    imodImageScaleData.min = (float)(exp((double)imodImageScaleData.min) -
                                     1.) / (kscale * minSign);
  }
}

void ImageScaleWindow::applyLimits()
{
  ImodView *vi = imodImageScaleData.vi;
  int black = BLACKNEW, white = WHITENEW;
  int k;

  /* Don't do it if someone else is busy loading */
  if (vi->loadingImage)
    return;

  imodImageScaleData.min = mEditBox[0]->text().toFloat();
  imodImageScaleData.max = mEditBox[1]->text().toFloat();

  /* DNM: the min and max will take care of all the scaling needs, so no longer
     need the li-black and li->white to be anything but 0 and 255 */
  /* DNM 1/3/04: cleanup li->black, white, ivwSetScale, doubles to iiSetMM */
  vi->black = black;
  vi->white = white;
  vi->li->smin = imodImageScaleData.min;
  vi->li->smax = imodImageScaleData.max;
     
  iiSetMM(vi->image, vi->li->smin, vi->li->smax);

  /* For multi-file sections, apply to all the files */
  if (vi->multiFileZ > 0)
    for (k = 0; k < vi->zsize; k++)
      iiSetMM(&vi->imageList[k + vi->li->zmin], vi->li->smin, vi->li->smax);

  /* Flip data back first if it is flipped and is either non-cache or
     full cache flipped */
  int reflip = 0;
  if (vi->li->axis == 2 && (!vi->vmSize || vi->fullCacheFlipped)) { 
    ivwFlip(vi);
    reflip = 1;
  }

  if (vi->vmSize) {

    /* If cached data, flush the cache in all cases for the current time */
    ivwFlushCache(vi, vi->ct);

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
  imod_info_float_clear(-vi->zsize, vi->ct);
  imod_info_setbw(black, white);     
  xcramp_setlevels(vi->cramp,black,white);

  imodDraw(vi, IMOD_DRAW_IMAGE);
}

void ImageScaleWindow::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// The window is closing, remove from manager
void ImageScaleWindow::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)imodImageScaleData.dia);
  imodImageScaleData.dia = NULL;
  e->accept();
}

// Close on escape, pass on keys
void ImageScaleWindow::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void ImageScaleWindow::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

/*
$Log$
Revision 4.16  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.15  2006/08/24 21:30:27  mast
Fixed (?) test for multi file sections

Revision 4.14  2005/12/08 05:56:04  mast
Flush the cache only for the current time, not all files.

Revision 4.13  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.12  2004/07/07 19:25:29  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 4.11  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.10  2004/01/09 15:55:32  mast
Use image smin/msax instead of li values when updating

Revision 4.9  2004/01/08 06:42:45  mast
Fixed treatment of complex data

Revision 4.8  2004/01/05 18:41:38  mast
Changed to deal with cache full mode, added error exits, prevented it
from operating while loading is underway, and cleanup up a bit (vw to vi)

Revision 4.7  2003/12/30 06:40:10  mast
Changes for multi-file section display, to change all files when apply

Revision 4.6  2003/11/01 18:12:17  mast
changed to put out virtually all error messages to a window

Revision 4.5  2003/06/04 23:30:55  mast
Add timer to prevent crash if window closed before reload done

Revision 4.4  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.3  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.2  2003/03/03 22:14:34  mast
cleanup

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 19:57:06  mast
Qt version

Revision 3.1.2.1  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/

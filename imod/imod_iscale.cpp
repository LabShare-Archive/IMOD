/*  IMOD VERSION 2.20
 *
 *  imod_iscale.c -- Reload image with new scaling factors.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qframe.h>
#include <qtooltip.h>

#include "imod_iscale.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_io.h"
#include "imod_info_cb.h"
#include "control.h"
#include "dia_qtutils.h"
#include "xcramp.h"

struct{
  ImageScaleWindow *dia;
  ImodView  *vw;
  float     min, max;
}imodImageScaleData = {NULL, NULL, 0, 0};

#define BLACKNEW  32
#define WHITENEW  223

void imodImageScaleDialog(ImodView *vw)
{
     
  if (imodImageScaleData.dia){
    imodImageScaleData.dia->raise();
    return;
  }

  imodImageScaleData.vw = vw;
  if ((!vw->li->smin) && (!vw->li->smax)){
    vw->li->smin = vw->hdr->amin;
    vw->li->smax = vw->hdr->amax;
  }

  imodImageScaleData.dia = new ImageScaleWindow
    (imodDialogManager.parent(IMOD_DIALOG), "image scale");

  imodDialogManager.add((QWidget *)imodImageScaleData.dia, IMOD_DIALOG);

  return;
}

void imodImageScaleUpdate(ImodView *vw)
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
  : DialogFrame(parent, 4, buttonLabels, buttonTips, true, 
                " ", "", name)
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

  QGridLayout *grid = new QGridLayout(mLayout, 2, 2);

  for (int i = 0; i < 2; i++) {
    str = uplow[i];
    label = new QLabel(str + " limit", this);
    grid->addWidget(label, i, 0);
    mEditBox[i] = new QLineEdit(this);
    grid->addWidget(mEditBox[i], i, 1);
    QToolTip::add(mEditBox[i], "Enter new " + str + " limit for rescaling");
    connect(mEditBox[i],  SIGNAL(returnPressed()), this, SLOT(setFocus()));
  }

  computeScale();
  showFileAndMMM();
  updateLimits();

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
  setCaption(imodCaption("3dmod Image Scale"));
  show();
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
    dia_vasmsg
      ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
       "3dmod Image Scale \n"
       "~~~~~~~~~~~~~~~~~~~~~~~~"
       "\n\n",
       "The lower and upper limits of pixel values are used to scale",
       "the image pixel values linearly to the 8-bit display.  ",
       "When you first open the window, these values are set based upon"
       " the current position of the contrast sliders, so that if the "
       "image is reloaded, it will have the same apparent contrast with "
       "the sliders set to 32 and 223.\n\n",
       "The Apply button will reload the image with the given limits"
       " AND set the contrast sliders to 32 and 223.\n\n",
       "The Calc button will recalculate suggested lower and upper limits"
       " based upon new settings of the contrast sliders.\n\n",
       "Thus, you can either type in new lower and upper limits, or use"
       " the sliders and Calc button to set the limits.\n",
       NULL);
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
  ImodView *vw = imodImageScaleData.vw;
  QString str;
  if (vw->multiFileZ > 0) {
    cz = (int)(vw->zmouse + 0.5 + vw->li->zmin);
    if (cz >= 0 && cz < vw->zsize && vw->image != &vw->imageList[cz]) {
        iiClose(vw->image);
        vw->hdr = vw->image = &vw->imageList[cz];
        ivwReopen(vw->image);
    }
  }
  str.sprintf("File: %s", vw->image->filename);
  mFileLabel->setText(str);
  str.sprintf("Min: %g    Max: %g    Mean: %g",  vw->hdr->amin,
          vw->image->amax, vw->image->amean);
  mMMMLabel->setText(str);
}

void ImageScaleWindow::computeScale()
{
  ImodView *vw = imodImageScaleData.vw;
  float slidecur, rangecur, slidenew, rangenew;
  slidecur = vw->white - vw->black;
  rangecur = vw->li->smax - vw->li->smin;
  slidenew = WHITENEW - BLACKNEW;
  rangenew = slidecur * rangecur / slidenew;
  imodImageScaleData.min = (slidenew * rangenew / 255.0f) *
    ( (255.0f * vw->li->smin / (slidecur * rangecur)) +
      vw->black/slidecur - BLACKNEW/slidenew );
  imodImageScaleData.max = imodImageScaleData.min + rangenew;
}

void ImageScaleWindow::applyLimits()
{
  ImodView *vw = imodImageScaleData.vw;
  int black = BLACKNEW, white = WHITENEW;
  int k;

  imodImageScaleData.min = mEditBox[0]->text().toFloat();
  imodImageScaleData.max = mEditBox[1]->text().toFloat();

  /* DNM: the min and max will take care of all the scaling needs, so no longer
     need the li-black and li->white to be anything but 0 and 255 */

  vw->black = black;
  vw->li->black = 0;
  vw->white = white;
  vw->li->white = 255;
  vw->li->smin = imodImageScaleData.min;
  vw->li->smax = imodImageScaleData.max;
     
  iiSetMM(vw->image, (double)vw->li->smin, (double)vw->li->smax);
  ivwSetScale(vw);

  /* For multi-file sections, apply to all the files */
  if (vw->multiFileZ)
    for (k = 0; k < vw->zsize; k++)
      iiSetMM(&vw->imageList[k + vw->li->zmin], (double)vw->li->smin, 
                             (double)vw->li->smax);

  if (vw->vmSize){
    /* flush the image cache. */
    ivwFlushCache(vw);
  }else{
    /* flipped data will crash */
    int reflip = 0;
    if (vw->li->axis == 2){
      ivwFlip(vw);
      reflip = 1;
    }
          

    if (vw->li->contig){
      free(vw->idata[0]);
    }else{
      for (k = 0; k < vw->zsize; k++)
        free(vw->idata[k]);
    }
    free(vw->idata);

    /* DNM: got to reread header since it gets screwed up at end of read */
    if (vw->image->file == IIFILE_MRC){
      if (!vw->image->fp)
        iiReopen(vw->image);
      if (!vw->image->fp) return;
      mrc_head_read(vw->image->fp, 
                    (struct MRCheader *)vw->image->header);
    }
    vw->idata = imod_io_image_load
      (vw->image, vw->li, imod_imgcnt);

    if (!vw->idata){
      imodError(NULL, "3DMOD: Fatal Error. Image LOST!\n");
      exit(-1);
    }
          
    if (App->depth == 8)
      ivwScale(vw);
    if (reflip)
      ivwFlip(vw);
  }

  /* DNM: clear any information for floating windows for this time */
  imod_info_float_clear(-vw->zsize, vw->ct);
  imod_info_setbw(black, white);     
  xcramp_setlevels(vw->cramp,black,white);

  imodDraw(vw, IMOD_DRAW_IMAGE);
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

/*  IMOD VERSION 2.42
 *
 *  pixelview.c -- view numerical values of pixels in an image.
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
#include <qpushbutton.h>
#include <qlabel.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include "pixelview.h"
#include "imod.h"
#include "imod_display.h"
#include "control.h"
#include "preferences.h"
#include "imod_input.h"
#include "dia_qtutils.h"


static PixelView *PixelViewDialog = NULL;
static int ctrl;

static void pviewClose_cb(ImodView *vi, void *client, int drawflag)
{
  if( PixelViewDialog)
    PixelViewDialog->close();
}

static void pviewDraw_cb(ImodView *vi, void *client, int drawflag)
{
  if (PixelViewDialog && (drawflag & (IMOD_DRAW_XYZ | IMOD_DRAW_IMAGE)))
    PixelViewDialog->update();
}

int open_pixelview(struct ViewInfo *vi)
{

  if (PixelViewDialog){
    PixelViewDialog->raise();
    return(-1);
  }

  PixelViewDialog = new PixelView(imodDialogManager.parent(IMOD_IMAGE),
                                  "pixel view");

  PixelViewDialog->setCaption(imodCaption("3dmod Pixel View"));

  ctrl = ivwNewControl(vi, pviewDraw_cb, pviewClose_cb, NULL, (void *)0);
  imodDialogManager.add((QWidget *)PixelViewDialog, IMOD_IMAGE);

  // Adjust for the buttons that are too large if the current file is ints
  // The minimum size setting of the buttons will keep this from getting
  // too small
  if (vi->image->mode == MRC_MODE_BYTE || vi->image->mode == MRC_MODE_SHORT) {
    QSize hint = PixelViewDialog->sizeHint();
    PixelViewDialog->resize((int)(0.7 * hint.width()), hint.height());
  }

  PixelViewDialog->show();
  PixelViewDialog->update();

  return(0);
}

PixelView::PixelView(QWidget *parent, const char *name, WFlags fl)
  : QWidget(parent, name, fl)
{
  int i, j;
  QGridLayout *layout = new QGridLayout(this, PV_ROWS + 1, PV_COLS + 1, 
				       7, 5, "pixel view layout");
  // Add labels on left
  for (i = 0; i < PV_ROWS; i++) {
    mLeftLabels[i] = new QLabel("88888", this);
    mLeftLabels[i]->setAlignment(AlignRight | AlignVCenter);
    layout->addWidget(mLeftLabels[i], PV_ROWS - 1 - i, 0);
  }

  // Add labels on bottom
  for (i = 0; i < PV_COLS; i++) {
    mBotLabels[i] = new QLabel("8", this);
    mBotLabels[i]->setAlignment(AlignCenter);
    layout->addWidget(mBotLabels[i], PV_ROWS, i + 1);
  }
  QLabel *labXY = new QLabel("Y/X", this);
  labXY->setAlignment(AlignCenter);
  layout->addWidget(labXY, PV_ROWS, 0);

  // Make signal mapper
  QSignalMapper *mapper = new QSignalMapper(this);
  connect(mapper, SIGNAL(mapped(int)), this, SLOT(buttonPressed(int)));

  // Make the buttons - put them in array in order of right-handed coordinates
  for (i = 0; i < PV_ROWS; i++) {
    for (j = 0; j < PV_COLS; j++) {
      mButtons[i][j] = new QPushButton("8", this);
      mButtons[i][j]->setFocusPolicy(NoFocus);
      layout->addWidget(mButtons[i][j], PV_ROWS - 1 - i, j + 1);
      mapper->setMapping(mButtons[i][j], i * PV_COLS + j);
      connect(mButtons[i][j], SIGNAL(clicked()), mapper, SLOT(map()));
    }
  }
  setButtonWidths();

  // Get the default background color, initial minimum/maximum rows
  mGrayColor = mButtons[0][0]->paletteBackgroundColor();
  mMinRow = -1;
  mMaxRow = -1;
}

void PixelView::setButtonWidths()
{
  // Fixed widths do not work well
  // This at least lets them resize smaller
  int width = diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1.2, 
                                "-88888");
  for (int i = 0; i < PV_ROWS; i++)
    for (int j = 0; j < PV_COLS; j++)
      mButtons[i][j]->setMinimumWidth(width);
}

void PixelView::update()
{
  struct ViewInfo *vi = App->cvi;
  QString str;
  int i, j, x, y;
  float pixel;
  float minVal = 1.e38;
  float maxVal = -1.e38;
  int floats = -1;

  /* DNM 11/24/02: Bring window to the top */
  raise();

  // Reset the button colors from previous min/max
  if (mMinRow >= 0)
    mButtons[mMinRow][mMinCol]->setPaletteBackgroundColor(mGrayColor);
  if (mMaxRow >= 0)
    mButtons[mMaxRow][mMaxCol]->setPaletteBackgroundColor(mGrayColor);
  mMinRow = -1;
  mMaxRow = -1;

  for (i = 0; i < PV_COLS; i++) {
    /* DNM: take floor to avoid duplicating 1 at 0 */
    x = (int)floor((double)vi->xmouse) + i - (PV_COLS/2);
    
    // Update labels on bottom
    if (i == (PV_COLS / 2))
      str.sprintf("%5d*", x+1);
    else
      str.sprintf("%5d ", x+1);
    mBotLabels[i]->setText(str);

    // Update the buttons
    for(j = 0; j < PV_ROWS; j++){
      y = (int)floor((double)vi->ymouse) + j - (PV_ROWS/2);
      if ((x < 0) || (y < 0) || (x >= vi->xsize) || (y >= vi->ysize))
	str = "     x";
      else{
	pixel = ivwGetFileValue(vi, x, y, (int)(vi->zmouse + 0.5));

        /* First time after getting a pixel, see if floats are needed */
        if (floats < 0) {
          if (vi->image->mode == MRC_MODE_BYTE || 
              vi->image->mode == MRC_MODE_SHORT)
            floats = 0;
          else
            floats = 1;
        }

	if (floats)
	  str.sprintf("%9g", pixel);
	else
	  str.sprintf("%6d", (int)pixel);
	if (pixel < minVal) {
	  minVal = pixel;
	  mMinCol = i;
	  mMinRow = j;
	}
	if (pixel > maxVal) {
	  maxVal = pixel;
	  mMaxCol = i;
	  mMaxRow = j;
	}
      }
      mButtons[j][i]->setText(str);

      // do label on left for this row
      if (!i){
	if (j == (PV_COLS / 2))
	  str.sprintf("%5d*", y+1);
	else
	  str.sprintf("%5d ", y+1);
	mLeftLabels[j]->setText(str);
      }
    }
  }
  if (mMinRow >= 0)
    mButtons[mMinRow][mMinCol]->setPaletteBackgroundColor(QColor(0, 255, 255));
  if (mMaxRow >= 0)
    mButtons[mMaxRow][mMaxCol]->setPaletteBackgroundColor(QColor(255, 0, 128));
}

void PixelView::buttonPressed(int pos)
{
  int x,y;

  ivwControlPriority(App->cvi, ctrl);

  y = pos / PV_COLS - PV_COLS / 2;
  x = pos % PV_COLS - PV_ROWS / 2;
  App->cvi->xmouse += x;
  App->cvi->ymouse += y;
  ivwBindMouse(App->cvi);
  imodDraw(App->cvi, IMOD_DRAW_XYZ);   // Removed IMOD_DRAW_IMAGE
}


// Close event: just remove control from list and null pointer
void PixelView::closeEvent ( QCloseEvent * e )
{
  ivwRemoveControl(App->cvi, ctrl);
  imodDialogManager.remove((QWidget *)PixelViewDialog);
  PixelViewDialog = NULL;
  e->accept();
}

// Key press: look for arrow keys and pass directly to default input,
// pass on others to next window that cares
void PixelView::keyPressEvent ( QKeyEvent * e )
{
  int key = e->key();
  if (key == Qt::Key_Escape)
    close();

  else if (!(e->state() & Qt::Keypad) && 
	   (key == Qt::Key_Right || key == Qt::Key_Left || 
	    key == Qt::Key_Up || key == Qt::Key_Down))
    inputQDefaultKeys(e, App->cvi);
    
  else
    ivwControlKey(0, e);
}

void PixelView::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}


/*
$Log$
Revision 4.7  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.6  2003/12/31 05:32:07  mast
Identify whether floats or not after getting first pixel so file is set

Revision 4.5  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.4  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.3  2003/03/26 06:30:56  mast
adjusting to font changes

Revision 4.2  2003/03/24 17:56:46  mast
Register with dialogManager so it can be parked with info window

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2003/01/06 15:49:46  mast
Use imodCaption

Revision 1.1.2.2  2003/01/04 03:48:41  mast
Qt version

Revision 1.1.2.1  2003/01/02 15:45:09  mast
changes for new controller key callback

Revision 3.3.2.1  2002/12/12 01:22:09  mast
*** empty log message ***

Revision 3.4  2002/12/10 21:07:44  mast
Changed the flag that it tested on so that it would draw when time changed
and float option was on

Revision 3.3  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.2  2002/11/27 03:23:00  mast
Changed argument 3 of draw_cb and close_cb from long to int to avoid 
warnings

Revision 3.1  2002/11/25 19:24:49  mast
Made it add itself to list of controls to be redraw to prevent excessive
redraws; restructured calls for external drawing and closing accordingly,
and made it raise itself when it redraws

*/

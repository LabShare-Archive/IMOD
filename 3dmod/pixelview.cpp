/*  
 *  pixelview.cpp -- view numerical values of pixels in an image.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <qpushbutton.h>
#include <qlabel.h>
#include <qcheckbox.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include <qstylefactory.h>
#include <qtooltip.h>
//Added by qt3to4:
#include <QHBoxLayout>
#include <QCloseEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include <QVBoxLayout>
#include "pixelview.h"
#include "imod.h"
#include "info_cb.h"
#include "display.h"
#include "control.h"
#include "pyramidcache.h"
#include "preferences.h"
#include "imod_input.h"
#include "dia_qtutils.h"
#include "xzap.h"
#include "xxyz.h"
#include "sslice.h"

static int getAndConvertRGB(unsigned char **data, int x, int y, int &red,
                           int &green, int &blue);
static bool fileReadable(ImodView *vi, int iz);

static PixelView *PixelViewDialog = NULL;
static int ctrl;
static bool fromFile = false;
static bool lastReadable, lastGridReadable;
static bool gridFromFile = true;
static float lastMouseX = 0.;
static float lastMouseY = 0.;
static bool showButs = true;
static bool convertRGB = false;

static int getAndConvertRGB(unsigned char **data, int x, int y, int &red,
                           int &green, int &blue)
{
  red = data[y][3 * x];
  green = data[y][3 * x + 1];
  blue = data[y][3 * x + 2];
  return B3DNINT(0.3 * red + 0.59 * green + 0.11 * blue);
}

static bool fileReadable(ImodView *vi, int iz)
{
  ImodImageFile *image = vi->image;
  if (vi->multiFileZ && iz >= 0 && iz < vi->multiFileZ)
    image = &vi->imageList[iz];
  return (!vi->rgbStore && !vi->noReadableImage);
}

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

/* 
 * Open the pixel view window
 */
int open_pixelview(struct ViewInfo *vi)
{
  if (PixelViewDialog){
    PixelViewDialog->raise();
    return(-1);
  }

  PixelViewDialog = new PixelView(imodDialogManager.parent(IMOD_IMAGE),
                                  "pixel view");

  PixelViewDialog->setWindowTitle(imodCaption("3dmod Pixel View"));

  ctrl = ivwNewControl(vi, pviewDraw_cb, pviewClose_cb, NULL, (void *)0);
  imodDialogManager.add((QWidget *)PixelViewDialog, IMOD_IMAGE);

  // This takes care of showing/hiding. resizing, and updating
  PixelViewDialog->showButsToggled(showButs);
  adjustGeometryAndShow((QWidget *)PixelViewDialog, IMOD_IMAGE);
  pvNewMousePosition(vi, vi->xmouse, vi->ymouse, B3DNINT(vi->zmouse));
  zapPixelViewState(true);
  xyzPixelViewState(true);
  slicerPixelViewState(true);
  return(0);
}

/*
 * Display a new mouse position from whoever has one
 */
void pvNewMousePosition(ImodView *vi, float x, float y, int iz)
{
  int ix = (int)x;
  int iy = (int)y;
  int isFloat = 0;
  float value;
  int red, green, blue;
  bool readable;
  unsigned char **data;
  QString str;

  lastMouseX = x;
  lastMouseY = y;
  if (!PixelViewDialog)
    return;
  if (ix < 0 || iy < 0 || ix >= vi->xsize || iy >= vi->ysize || iz < 0 || 
      iz >= vi->zsize)
    return;
  readable = fileReadable(vi, iz);
  if (readable != lastReadable) {
    PixelViewDialog->mFileValBox->setEnabled(readable);
    lastReadable = readable;
  }
  if (fromFile && readable) {
    value = ivwGetFileValue(vi, ix, iy, iz);
    if (!(vi->image->mode == MRC_MODE_BYTE || 
          vi->image->mode == MRC_MODE_SHORT ||
          vi->image->mode == MRC_MODE_USHORT))
        isFloat = 1;
  } else if (vi->rgbStore) {
    ivwGetTime(vi, &red);
    data = ivwGetZSectionTime(vi, iz, red);
    value = getAndConvertRGB(data, ix, iy, red, green, blue);
  } else
    value = ivwGetValue(vi, ix, iy, iz);
  if (isFloat)
    str.sprintf("Mouse: %5d, %5d, %4d  Value: %9g", ix + 1, iy + 1, 
                iz + 1, value);
  else
    str.sprintf("Mouse: %5d, %5d, %4d  Value: %3d", ix + 1, iy + 1, 
                iz + 1, (int)value);
  PixelViewDialog->mMouseLabel->setText(str);
}

/*
 * The class constructor
 */
PixelView::PixelView(QWidget *parent, const char *name, Qt::WFlags fl)
  : QWidget(parent, fl)
{
  int i, j, iz = B3DNINT(App->cvi->zmouse);
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  QVBoxLayout *vBox = new QVBoxLayout(this);
  vBox->setSpacing(3);

  // Make the mouse report box
  QHBoxLayout *hBox = diaHBoxLayout(vBox);
  mMouseLabel = diaLabel(" ", this, hBox);
  hBox->addStretch();
  hBox->setSpacing(5);

  mFileValBox = diaCheckBox("File value", this, hBox);
  diaSetChecked(mFileValBox, fromFile);
  connect(mFileValBox, SIGNAL(toggled(bool)), this, 
          SLOT(fromFileToggled(bool)));
  mFileValBox->setToolTip("Show value from file, not byte value from memory"
                ", at mouse position");
  mFileValBox->setEnabled(fileReadable(App->cvi, iz));

  QCheckBox *gbox = diaCheckBox("Grid", this, hBox);
  diaSetChecked(gbox, showButs);
  connect(gbox, SIGNAL(toggled(bool)), this, SLOT(showButsToggled(bool)));
  gbox->setToolTip("Show buttons with values from file or memory)");

  hBox = diaHBoxLayout(vBox);
  mGridValBox = diaCheckBox("Grid value from file", this, hBox);
  diaSetChecked(mGridValBox, gridFromFile);
  connect(mGridValBox, SIGNAL(toggled(bool)), this, 
          SLOT(gridFileToggled(bool)));
  mGridValBox->setToolTip("Show value from file, not byte value from memory"
                ", in each button");
  mGridValBox->setEnabled(fileReadable(App->cvi, iz));

  mConvertBox = NULL;
  if (App->cvi->rgbStore) {
    mConvertBox = diaCheckBox("Convert RGB to gray scale", this, hBox);
    diaSetChecked(mConvertBox, convertRGB);
    connect(mConvertBox, SIGNAL(toggled(bool)), this, 
            SLOT(convertToggled(bool)));
    mConvertBox->setToolTip("Show luminance values instead of RGB triplets"
                  );
  }

  hBox->addStretch();
  hBox->setSpacing(5);
  mHelpButton = diaPushButton("Help", this, hBox);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(helpClicked()));

  // Make the grid
  QGridLayout *layout = new QGridLayout();
  layout->setSpacing(5);
  vBox->addLayout(layout);
  vBox->setContentsMargins(7,7,7,7);

  // Add labels on left
  for (i = 0; i < PV_ROWS; i++) {
    mLeftLabels[i] = new QLabel("88888", this);
    mLeftLabels[i]->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    layout->addWidget(mLeftLabels[i], PV_ROWS - 1 - i, 0);
  }

  // Add labels on bottom
  for (i = 0; i < PV_COLS; i++) {
    mBotLabels[i] = new QLabel("8", this);
    mBotLabels[i]->setAlignment(Qt::AlignCenter);
    layout->addWidget(mBotLabels[i], PV_ROWS, i + 1);
  }
  mLabXY = new QLabel("Y/X", this);
  mLabXY->setAlignment(Qt::AlignCenter);
  layout->addWidget(mLabXY, PV_ROWS, 0);

  // Make signal mapper
  QSignalMapper *mapper = new QSignalMapper(this);
  connect(mapper, SIGNAL(mapped(int)), this, SLOT(buttonPressed(int)));

  // The buttons look really bad in Aqua so use a fixed style here
#ifdef Q_OS_MACX
  QStyle *newStyle = QStyleFactory::create("Cleanlooks");
#else
  QStyle *newStyle = NULL;
#endif

  // Make the buttons - put them in array in order of right-handed coordinates
  for (i = 0; i < PV_ROWS; i++) {
    for (j = 0; j < PV_COLS; j++) {
      mButtons[i][j] = new QPushButton("8", this);
      mButtons[i][j]->setFocusPolicy(Qt::NoFocus);
      mButtons[i][j]->setAutoFillBackground(true);
      if (newStyle)
        mButtons[i][j]->setStyle(newStyle);
      layout->addWidget(mButtons[i][j], PV_ROWS - 1 - i, j + 1);
      mapper->setMapping(mButtons[i][j], i * PV_COLS + j);
      connect(mButtons[i][j], SIGNAL(clicked()), mapper, SLOT(map()));
    }
  }
  setButtonWidths();

  // Get the default background color, initial minimum/maximum rows
  QPalette palette = mButtons[0][0]->palette();
  mGrayColor = palette.color(mButtons[0][0]->backgroundRole());
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
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

void PixelView::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setButtonWidths();
}

/*
 * Routine to update the buttons with values
 */
void PixelView::update()
{
  ImodView *vi = App->cvi;
  QString str;
  int i, j, x, y, iz, xend, yend;
  float pixel;
  int red, green, blue;
  bool readable;
  unsigned char **data;
  float minVal = 1.e38;
  float maxVal = -1.e38;
  int floats = -1;

  /* DNM 11/24/02: Bring window to the top */
  raise();
  if (!showButs)
    return;

  // Reset the button colors from previous min/max
  if (mMinRow >= 0)
    diaSetWidgetColor(mButtons[mMinRow][mMinCol], mGrayColor);
  if (mMaxRow >= 0)
    diaSetWidgetColor(mButtons[mMaxRow][mMaxCol], mGrayColor);
  mMinRow = -1;
  mMaxRow = -1;

  iz = B3DNINT(vi->zmouse);
  readable = fileReadable(vi, iz);
  if (readable != lastGridReadable) {
    mGridValBox->setEnabled(readable);
    lastGridReadable = readable;
  }
  if (vi->rgbStore)
    data = ivwGetCurrentSection(vi);

  if (vi->imagePyramid) {
    x = (int)floor((double)vi->xmouse) - (PV_COLS/2);
    y = (int)floor((double)vi->ymouse) - (PV_ROWS/2);
    x = B3DMAX(0, x);
    y = B3DMAX(0, y);
    xend = B3DMIN(vi->xsize, x + PV_COLS);
    yend = B3DMIN(vi->ysize, y + PV_ROWS);
    vi->pyrCache->loadTilesContainingArea(vi->pyrCache->getBaseIndex(), x, y, xend - x, 
                                          yend - y, iz);
  }

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
      else {
        if (readable && gridFromFile)
          pixel = ivwGetFileValue(vi, x, y, iz);
        else if (vi->rgbStore)
          pixel = getAndConvertRGB(data, x, y, red, green, blue);
        else
          pixel = ivwGetValue(vi, x, y, iz);

        /* First time after getting a pixel, see if floats are needed */
        if (floats < 0 && readable) {
          if (vi->image->mode == MRC_MODE_BYTE || 
              vi->image->mode == MRC_MODE_SHORT ||
              vi->image->mode == MRC_MODE_USHORT)
            floats = 0;
          else
            floats = 1;
        }

	if (floats > 0)
	  str.sprintf("%9g", pixel);
        else if (vi->rgbStore && !convertRGB)
          str.sprintf("%3d,%3d,%3d", red, green, blue);
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
    diaSetWidgetColor(mButtons[mMinRow][mMinCol], QColor(0, 255, 255));
  if (mMaxRow >= 0)
    diaSetWidgetColor(mButtons[mMaxRow][mMaxCol], QColor(255, 0, 128));
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

void PixelView::fromFileToggled(bool state)
{
  fromFile = state;
}

void PixelView::gridFileToggled(bool state)
{
  gridFromFile = state;
  update();
}

void PixelView::convertToggled(bool state)
{
  convertRGB = state;
  update();
  adjustDialogSize();
}

void PixelView::helpClicked(void)
{
  imodShowHelpPage("pixelview.html#TOP");
}

// Show or hide the grid of buttons
void PixelView::showButsToggled(bool state)
{
  int i, j;
  showButs = state;
  for (i = 0; i < PV_ROWS; i++) {
    for (j = 0; j < PV_COLS; j++)
      diaShowWidget(mButtons[i][j], state);
    diaShowWidget(mLeftLabels[i], state);
  }
  for (j = 0; j < PV_COLS; j++)
    diaShowWidget(mBotLabels[j], state);
  diaShowWidget(mLabXY, state);
  diaShowWidget(mGridValBox, state);
  if (mConvertBox)
    diaShowWidget(mConvertBox, state);
  diaShowWidget(mHelpButton, state);

  adjustDialogSize();
  update();
}

// Adjust for the buttons that are too large if the current file is ints
// The minimum size setting of the buttons will keep this from getting
// too small
void PixelView::adjustDialogSize()
{
  int mode = App->cvi->image->mode;
  if (showButs && (mode == MRC_MODE_BYTE || mode == MRC_MODE_SHORT ||
                   mode == MRC_MODE_USHORT || 
                   (App->cvi->rgbStore && convertRGB))) {
    QSize hint = sizeHint();
    resize((int)(0.7 * hint.width()), hint.height());
  } else {
    imod_info_input();
    adjustSize();
  }
}

// Close event: just remove control from list and null pointer
void PixelView::closeEvent ( QCloseEvent * e )
{
  ivwRemoveControl(App->cvi, ctrl);
  imodDialogManager.remove((QWidget *)PixelViewDialog);
  PixelViewDialog = NULL;
  zapPixelViewState(false);
  xyzPixelViewState(false);
  slicerPixelViewState(false);
  e->accept();
}

// Key press: look for arrow keys and pass directly to default input,
// pass on others to next window that cares
void PixelView::keyPressEvent ( QKeyEvent * e )
{
  int key = e->key();
  if (utilCloseKey(e))
    close();

  else if (!(e->modifiers() & Qt::KeypadModifier) && 
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


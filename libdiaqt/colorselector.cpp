/* 
 *  colorselector.cpp       Implementation of color selector class
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */
#include <qlayout.h>
#include <qlabel.h>
#include <qcolordialog.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QKeyEvent>
#include <QCloseEvent>
#include "multislider.h"
#include "colorselector.h"

#define HOT_SLIDER_KEYUP 0
#define HOT_SLIDER_KEYDOWN 1
#define NO_HOT_SLIDER 2

static const char *sliderLabels[] = {"Red", "Green", "Blue"};
static const char *buttonLabels[] = {"Done", "Restore", "Qt Selector"};
static const char *buttonTips[] = 
  {"Close color selector",
   "Restore to starting color when selector was opened",
   "Select color with Qt color selector box"};

/*!
 * This class provides a color selector with a sample color panel, and three
 * sliders for adjusting red, green, and blue.  [label] is used to set a
 * label at the top of the panel, and the color is initialized with [red], 
 * [green], and [blue].  It manages the color of the
 * panel continuously during changes, and emits a signal for a new color
 * if the slider is clicked.  It will also emit signals during a drag if 
 * [hotFlag] is not 2; if the key given by [hotKey] is up when [hotFlag] is 0;
 * or if that key is down when [hotFlag] is 1.  [name] defaults to NULL, and
 * the window flags default to Qt::WDestructiveClose | Qt::WType_TopLevel.
 * ^     Signals emitted are:
 * ^ void newColor(int r, int g, int b);  -  When the color changes
 * ^ void done();   -  When the Done button is pressed
 * ^ void closing();  -  When the window is closing
 * ^ void keyPress(QKeyEvent *e);   -  When a key is pressed
 * ^ void keyRelease(QKeyEvent *e);  -  When a key is released
 * ^     In addition, there is one method:
 * ^ bool hotSliding();  -  Returns true is a slider is being dragged
 */
ColorSelector::ColorSelector(QWidget *parent, QString label, int red,
                             int green, int blue, int hotFlag, int hotKey,
                             bool rounded, const char *name, Qt::WFlags fl)
  : DialogFrame(parent, 3, 1, buttonLabels, buttonTips, false, rounded, "test",
                "test2", name, fl)
{
  QString str;

  mOriginalRGB[0] = mCurrentRGB[0] = red;
  mOriginalRGB[1] = mCurrentRGB[1] = green;
  mOriginalRGB[2] = mCurrentRGB[2] = blue;
  mCtrlPressed = false;
  mHotKey = hotKey;
  mHotFlag = hotFlag;
  mDragging = false;

  // Get the top label
  QLabel *topLabel = new QLabel(label, this);
  mLayout->addWidget(topLabel);
  
  // Make the color box a GL widget because palette background works poorly
  // on SGI
  mGLw = new ColorSelectorGL(&mCurrentRGB[0], this);
  mGLw->setFixedHeight(50);
  mLayout->addWidget(mGLw);
 
  // Get the sliders, connect them and initialize them to current color
  mSliders = new MultiSlider(this, 3, sliderLabels);
  mLayout->addLayout(mSliders->getLayout());
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderChanged(int, int, bool)));

  // Connect them: have to connect to release of Qt selector because the modal
  // box keeps the button from coming back up (maybe mixed X problem only)
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonClicked(int)));

  imposeColor(true, false);
}

ColorSelector::~ColorSelector()
{
}

void ColorSelector::buttonClicked(int which)
{
  if (which == 0)
    donePressed();
  else if (which == 1)
    restorePressed();
  else if (which == 2)
    qtSelectorPressed();
}

void ColorSelector::donePressed()
{
  emit done();
}

// Restore: restore the original color and send signal
void ColorSelector::restorePressed()
{
  for (int i = 0; i < 3; i++)
    mCurrentRGB[i] = mOriginalRGB[i];
  imposeColor(true, true);
}

// Open color selector, and take color if it is valid
void ColorSelector::qtSelectorPressed()
{
  QColor retColor = QColorDialog::getColor(QColor(mCurrentRGB[0],
                                                  mCurrentRGB[1],
                                                  mCurrentRGB[2]), this);
  if (!retColor.isValid())
    return;
  retColor.getRgb(&mCurrentRGB[0], &mCurrentRGB[1], &mCurrentRGB[2]);
  imposeColor(true, true);
}

// Slider changed: change the current value, update color box,
// and emit new color if not dragging or ctrl pressed
void ColorSelector::sliderChanged(int which, int value, bool dragging)
{
  mCurrentRGB[which] = value;
  imposeColor(false, 
	      !dragging || (mHotFlag == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
	      (mHotFlag == HOT_SLIDER_KEYUP && !mCtrlPressed));

  // Keep track of dragging state AFTER sending signal so first move of a
  // drag is still treated as non-drag and the last one resets the flag
  mDragging = dragging;
}

// Act on a new color
void ColorSelector::imposeColor(bool setSliders, bool emitSignal)
{
  if (setSliders)
    for (int i = 0; i < 3; i++)
      mSliders->setValue(i, mCurrentRGB[i]);

  mGLw->updateGL();
  if (emitSignal)
    emit newColor(mCurrentRGB[0], mCurrentRGB[1], mCurrentRGB[2]);
}

void ColorSelector::closeEvent ( QCloseEvent * e )
{
  emit closing();
  e->accept();
}

// watch for ctrl key; emit the key event to pass it on
void ColorSelector::keyPressEvent ( QKeyEvent * e )
{
  bool closing = e->key() == Qt::Key_Escape;
#ifdef Q_OS_MACX
  if (!closing)
    closing = e->key() == Qt::Key_W && (e->modifiers() ==Qt::ControlModifier);
#endif
  if (closing) {
    emit done();
  } else {
    
    if (mHotFlag != NO_HOT_SLIDER && e->key() == mHotKey) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    emit (keyPress(e));
  }

}


void ColorSelector::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == mHotKey) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  emit (keyRelease(e));
}

ColorSelectorGL::ColorSelectorGL(int *currentRGB, QWidget * parent,
				 const char * name)
  : QGLWidget(parent)
{
  mRGB = currentRGB;
  mFirstDraw = true;
}

void ColorSelectorGL::paintGL()
{
  glClearColor(mRGB[0] / 255., mRGB[1] / 255., mRGB[2] / 255., 0.);
  glClear(GL_COLOR_BUFFER_BIT);
  if (mFirstDraw) {
    mTimerID = startTimer(10);
    mFirstDraw = false;
  }
}

void ColorSelectorGL::timerEvent(QTimerEvent *e)
{
  killTimer(mTimerID);
  updateGL();
}

/*
$Log$
Revision 1.14  2010/04/01 02:57:37  mast
Added Apple-W for close on Mac

Revision 1.13  2009/03/20 00:16:23  mast
eliminated q3frame include

Revision 1.12  2009/01/15 16:30:26  mast
Qt 4 port

Revision 1.11  2007/08/26 06:55:59  mast
Documentation changes

Revision 1.10  2004/11/20 05:06:48  mast
Provide a way to determine if sliders are being dragged

Revision 1.9  2004/11/04 23:32:44  mast
Changes for rounded button style

Revision 1.8  2004/03/22 00:42:55  mast
Had to rename buttonPressed to buttonClicked

Revision 1.7  2004/01/22 19:14:30  mast
swicthed from actionPressed to actionClicked

Revision 1.6  2003/04/14 05:05:43  mast
add initial redraw

Revision 1.5  2003/03/24 17:43:24  mast
Changes in definitions of hotflags

Revision 1.4  2003/03/20 23:40:19  mast
Eliminate frame around GL widget to get it full width on SGI

Revision 1.3  2003/03/19 19:38:11  mast
Change the color panel to a GL widget

Revision 1.2  2003/02/10 20:51:22  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:35:46  mast
adding as library file

Revision 1.1.2.6  2003/01/23 19:55:42  mast
switch from button pressed to clicked

Revision 1.1.2.5  2003/01/18 00:58:37  mast
add tooltips to dialogframe call

Revision 1.1.2.4  2002/12/30 06:53:00  mast
eliminate unused variables

Revision 1.1.2.3  2002/12/30 06:36:21  mast
parameterizing the hot slider

Revision 1.1.2.2  2002/12/29 04:12:58  mast
Recoded to inherit dialog_frame

Revision 1.1.2.1  2002/12/27 01:19:47  mast
Initial creation

*/

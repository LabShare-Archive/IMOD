/*
 *  form_autox.cpp - Class for autocontour dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_autox.h"

#include <qvariant.h>
#include <qslider.h>
#include <qimage.h>
#include <qpixmap.h>
#include <qbuttongroup.h>

//Added by qt3to4:
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QCloseEvent>
#include <qtooltip.h>

#include "imod.h"
#include "control.h"
#include "autox.h"
#include "preferences.h"
#include "multislider.h"
#include "dia_qtutils.h"

/*
 *  Constructs a AutoxWindow as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
AutoxWindow::AutoxWindow(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
AutoxWindow::~AutoxWindow()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void AutoxWindow::languageChange()
{
  retranslateUi(this);
}

void AutoxWindow::init()
{
  mCtrlPressed = false;
  const char *labels[] = {"Threshold", "Resolution"};
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
    
  QVBoxLayout *layout = new QVBoxLayout(sliderFrame);
  layout->setContentsMargins(0,0,0,0);
  mSliders = new MultiSlider(sliderFrame, 2, labels, 0, 254);
  layout->addLayout(mSliders->getLayout());
  mSliders->setRange(1, 0, AUTOX_MAX_RESOLUTION);
  mSliders->setDecimals(1, 2);
    
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this,
          SLOT(sliderChanged(int, int, bool)));
  mSliders->getSlider(0)->setToolTip("Set threshold for high-contrast viewing");
  mSliders->getSlider(1)->setToolTip("Set resolution in pixels for making contours");

  contrastGroup = new QButtonGroup(this);
  contrastGroup->addButton(regularRadioButton, 0);
  contrastGroup->addButton(highRadioButton, 1);
  connect(contrastGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(contrastSelected(int)));

  setFontDependentWidths();
}

// Adjust button widths
void AutoxWindow::setFontDependentWidths()
{
  int width = diaSetButtonWidth(clearButton, ImodPrefs->getRoundedStyle(), 1.2, 
                                "Clear");
  fillButton->setFixedWidth(width);
  buildButton->setFixedWidth(width);
  nextButton->setFixedWidth(width);
  width = diaSetButtonWidth(expandButton, ImodPrefs->getRoundedStyle(), 1.2, 
                            "Expand");
  smoothButton->setFixedWidth(width);
  shrinkButton->setFixedWidth(width);
}

void AutoxWindow::contrastSelected( int which )
{
  autoxContrastSelected(which);
}

// Pass on slider if it is resolution slider, or if not dragging, or if hot slider enabled
void AutoxWindow::sliderChanged( int which, int value, bool dragging )
{
  if (which || !dragging || ImodPrefs->hotSliderActive(mCtrlPressed))
    autoxSlider(which, value);
}

// Pass on all other actions
void AutoxWindow::altMouse( bool state )
{
  autoxAltmouse(state ? 1: 0);
}

void AutoxWindow::followDiagonals( bool state )
{
  autoxFollowDiagonals(state ? 1 : 0);
}

void AutoxWindow::buildPressed()
{
  autoxBuild();
}

void AutoxWindow::clearPressed()
{
  autoxClear();
}

void AutoxWindow::fillPressed()
{
  autoxFill();
}

void AutoxWindow::nextPressed()
{
  autoxNext();
}

void AutoxWindow::expandPressed()
{
  autoxExpand();
}

void AutoxWindow::shrinkPressed()
{
  autoxShrink();
}

void AutoxWindow::smoothPressed()
{
  autoxSmooth();
}

void AutoxWindow::helpPressed()
{
  autoxHelp();
}

// Set initial states of all controls
void AutoxWindow::setStates( int contrast, int threshold, int resolution, 
			     int altMouse, int follow )
{
  diaSetGroup(contrastGroup, contrast);
  mSliders->setValue(0, threshold);
  mSliders->setValue(1, resolution);
  diaSetChecked(altMouseBox, altMouse);
  diaSetChecked(diagonalsBox, follow);
}

// Inform of window closing
void AutoxWindow::closeEvent( QCloseEvent * e )
{
  autoxClosing();
  e->accept();
}

// Close on Escape; watch for ctrl press and grab, or pass on keys
void AutoxWindow::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e)) {
    close();
  } else {
    
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    ivwControlKey(0, e);
  }
}

void AutoxWindow::keyReleaseEvent( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  ivwControlKey(1, e);
}


void AutoxWindow::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

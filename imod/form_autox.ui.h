/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/


void AutoxWindow::init()
{
    mCtrlPressed = false;
    char *labels[] = {"Threshold", "Resolution"};
    
    QVBoxLayout *layout = new QVBoxLayout(sliderFrame);
    mSliders = new MultiSlider(sliderFrame, 2, labels, 0, 254);
    layout->addLayout(mSliders->getLayout());
    mSliders->setRange(1, 0, AUTOX_MAX_RESOLUTION);
    mSliders->setDecimals(1, 2);
    
    connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this,
	    SLOT(sliderChanged(int, int, bool)));
    QToolTip::add(mSliders->getSlider(0), "Set threshold for high-contrast viewing");
    QToolTip::add(mSliders->getSlider(1), "Set resolution in pixels for making contours");
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
    if (e->key() == Qt::Key_Escape) {
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


void AutoxWindow::fontChange( const QFont & oldFont )
{
    setFontDependentWidths();
    QWidget::fontChange(oldFont);
}

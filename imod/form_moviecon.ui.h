/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// Initialize: make the sliders, set radio buttons
void MovieController::init()
{
    char *labels[] = {"Start", "End", "Increment"};
    
    QVBoxLayout *layout = new QVBoxLayout(sliderFrame);
    mSliders = new MultiSlider(sliderFrame, 3, labels, 1, 20);
    layout->addLayout(mSliders->getLayout());
    diaSetGroup(snapshotGroup, 0);
    diaSetGroup(extentGroup, 0);
    diaSetGroup(startHereGroup, 0);
    diaSetGroup(axisGroup, 2);
    connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this,
	    SLOT(sliderChanged(int, int, bool)));
    QToolTip::add(mSliders->getSlider(0), "Set starting section of movie range");
    QToolTip::add(mSliders->getSlider(1), "Set ending section of movie range");
    QToolTip::add(mSliders->getSlider(2), 
		  "Set spacing between sections shown during movie");
    setNonTifLabel();
}

void MovieController::setFontDependentWidths()
{
    rateLineEdit->setMaximumWidth(fontMetrics().width("8888.888"));
}

void MovieController::setNonTifLabel()
{
    rgbRadioButton->setText(ImodPrefs->snapFormat());
}

// Pass on actions directly to imod_moviecon
void MovieController::axisSelected( int which )
{
    imcAxisSelected(which);
}

void MovieController::sliderChanged( int which, int value, bool dragging )
{
    imcSliderChanged(which, value);
}

void MovieController::upPressed()
{
    imcIncrementRate(-1);
}

void MovieController::downPressed()
{
    imcIncrementRate(1);
}

void MovieController::rateEntered()
{
    float value = rateLineEdit->text().toFloat();
    imcRateEntered(value);
}

void MovieController::snapshotSelected( int which )
{
    imcSnapSelected(which);
}

void MovieController::extentSelected( int which )
{
    imcExtentSelected(which);
}

void MovieController::startHereSelected( int which )
{
    imcStartHereSelected(which);
}

void MovieController::donePressed()
{
    close();
}

void MovieController::helpPressed()
{
    imcHelp();
}

void MovieController::resetPressed()
{
    imcResetPressed();
}

// Routines for setting the state of the controls
void MovieController::enableTime( int enabled )
{
   timeRadioButton->setEnabled(enabled != 0); 
}

void MovieController::setActualRate( QString str )
{
    actualLabel->setText(str);
}

void MovieController::setRateBox( float value )
{
    QString str;
     if (value < 10.0)
          str.sprintf("%5.2f", value);
     else
          str.sprintf("%5.1f", value);
     rateLineEdit->setText(str);
}

void MovieController::setSliders( int start, int maxStart, int end, int minEnd,
				  int maxEnd, int increment,  int enable )
{
    mSliders->setRange(0, 1, maxStart);
    mSliders->setValue(0, start);
    mSliders->setRange(1, minEnd, maxEnd);
    mSliders->setValue(1, end);
    mSliders->setValue(2, increment);
    for (int i = 0; i < 3; i++) 
	mSliders->getSlider(i)->setEnabled(enable != 0);
}

// Close and key events
void MovieController::closeEvent( QCloseEvent * e )
{
    imcClosing();
    e->accept();
}

void MovieController::keyPressEvent( QKeyEvent * e )
{
    if (e->key() == Qt::Key_Escape)
	close();
    else
	ivwControlKey(0, e);
}

void MovieController::keyReleaseEvent( QKeyEvent * e )
{
    ivwControlKey(1, e);
}

void MovieController::fontChange( const QFont & oldFont )
{
    setFontDependentWidths();
}

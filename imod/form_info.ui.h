/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

/* Finicky layouts - here are some spacing and margin values
 * Image - model: spacing 4, margin 0
 * Inner vbox's all spacing 6
 * Outer Hboxes, spacing 6, margin 0
 * Slider grid spacing 5
 * vbox with slider and button, spacing 2
 * Mode group, spacing 2, margin 8 
 * bottom hbox, spacing 10
 * whole widget, spacing 4, margin 3
 */

void InfoControls::init()
{
    int i;
    mCtrlPressed = false;
    mBlackPressed = false;
    mWhitePressed = false;
    setShowPoint(true);
    
    // Set arrays of spin boxes
    mOCPBox[0] = objectSpinBox;
    mOCPBox[1] = contourSpinBox;
    mOCPBox[2] = pointSpinBox;
    mXYZBox[0] = xSpinBox;
    mXYZBox[1] = ySpinBox;
    mXYZBox[2] = zSpinBox;
    mOCPLabel[0] = maxObjectLabel;
    mOCPLabel[1] = maxContourLabel;
    mOCPLabel[2] = maxPointLabel;
    mXYZLabel[0] = maxXLabel;
    mXYZLabel[1] = maxYLabel;
    mXYZLabel[2] = maxZLabel;
    
    QToolTip::add(raiseButton, "Raise all 3dmod windows above other windows (hotkey "
		  CTRL_STRING"-R)");
    // Get signal mappers for the combo boxes
    QSignalMapper *ocpMapper = new QSignalMapper(this);
    QSignalMapper *xyzMapper = new QSignalMapper(this);
    connect(ocpMapper, SIGNAL(mapped(int)), this, SLOT(ocpChanged(int)));
    connect(xyzMapper, SIGNAL(mapped(int)), this, SLOT(xyzChanged(int)));
    
    // Set up mappings and initialize values to force updates
    for (i = 0; i < 3; i++) {
	ocpMapper->setMapping(mOCPBox[i], i);
	connect(mOCPBox[i], SIGNAL(valueChanged(int)), ocpMapper, SLOT(map()));
	mLastOCPval[i] = -2;
	mLastOCPmax[i] = -2;
	xyzMapper->setMapping(mXYZBox[i], i);
	connect(mXYZBox[i], SIGNAL(valueChanged(int)), xyzMapper, SLOT(map()));
	mLastXYZval[i] = -2;
	mLastXYZmax[i] = -2;
    }
    setFontDependentWidths();
    
    QIconSet iconSet;
    iconSet.setPixmap(QPixmap(pegged), QIconSet::Automatic, QIconSet::Normal,
		      QIconSet::On);
    iconSet.setPixmap(QPixmap(unpegged), QIconSet::Automatic, QIconSet::Normal,
		      QIconSet::Off);
    keepOnTopButton->setIconSet(iconSet);
    keepOnTopButton->setOn(false);
    QSize hint = keepOnTopButton->sizeHint();
    raiseButton->setFixedWidth(hint.width());
    keepOnTopButton->setFixedWidth(hint.width());
}

// Set a minimum width for spin boxes to keep arrows big
void InfoControls::setFontDependentWidths()
{
    int minWidth = fontMetrics().width("88888888");
    int minLabelWidth = fontMetrics().width("/ 8888");
    for (int i = 0; i < 3; i++) {
	mOCPBox[i]->setMinimumWidth(minWidth);
	mXYZBox[i]->setMinimumWidth(minWidth);
	mOCPLabel[i]->setMinimumWidth(minLabelWidth);
	mXYZLabel[i]->setMinimumWidth(minLabelWidth);
    }
}

// X, Y, or Z changed: pass on the whole array
void InfoControls::xyzChanged( int item )
{
    mLastXYZval[item] = mXYZBox[item]->value();
    imodInfoNewXYZ(mLastXYZval);
}

// Object, contour or point changed
void InfoControls::ocpChanged( int item )
{
    // The box has apparently already lost focus when the number gets here, so
    // that is not a way to determine if user edited
    // Determine if it was edited simply by whether it changed by more than one
    int value = mOCPBox[item]->value();
    int diff = mLastOCPval[item] - value;
    if (diff < 0)
	diff = -diff;
    // int edited = (diff == 1 || diff == mLastOCPmax[item]) ? 0 : 1;
    int edited = mShowPoint ? 0 : 1;
    
    // Interpret a change to 0 and wrap around as appropriate
    if (!value) {
	if (mLastOCPval[item] == 1)
	    value = mLastOCPmax[item];
	else
    	    value = 1;
	diaSetSpinBox(mOCPBox[item], value);
    }
    
    // If value really has changed, pass it on
    if (mLastOCPval[item] != value)
	imodInfoNewOCP(item, value, edited);
    mLastOCPval[item] = value;
}

// Manage the black slider
void InfoControls::blackChanged( int value )
{
    imodInfoNewBW(0, value, mBlackPressed ? 1 : 0);
}

void InfoControls::blackPressed()
{
    mBlackPressed = true;
}

void InfoControls::blackReleased()
{
    mBlackPressed = false;
    blackChanged(mDisplayedBlack);
}

void InfoControls::displayBlack( int value )
{
    mStr.sprintf("%d", value);
    blackLabel->setText(mStr);
    mDisplayedBlack = value;
}

// Manage the white slider
void InfoControls::whiteChanged( int value )
{
    imodInfoNewBW(1, value, mWhitePressed ? 1 : 0);
}

void InfoControls::whitePressed()
{
    mWhitePressed = true;
}

void InfoControls::whiteReleased()
{
    mWhitePressed = false;
    whiteChanged(mDisplayedWhite);
}

void InfoControls::displayWhite( int value )
{
    mStr.sprintf("%d", value);
    whiteLabel->setText(mStr);
    mDisplayedWhite = value;
}

// The buttons
void InfoControls::movieModelSelected( int item )
{
    imodInfoMMSelected(item);
}

void InfoControls::floatToggled( bool state )
{
    imodInfoFloat(state ? 1 : 0);
}

void InfoControls::raisePressed()
{
    inputRaiseWindows();
}

void InfoControls::showPointToggled( bool state )
{
    mShowPoint = state;
}

void InfoControls::keepOnTopToggled( bool state )
{
    ImodInfoWin->keepOnTop(state);
}

// ROUTINES FOR SETTING CONTROLS
// Float checkbox is disabled for state < 0
void InfoControls::setFloat( int state )
{
    floatCheckBox->setEnabled(state >= 0);
    diaSetChecked(floatCheckBox, state > 0);
}

// Set the sliders with new values
void InfoControls::setBWSliders( int black, int white )
{
    displayBlack(black);
    diaSetSlider(blackSlider, black);
    displayWhite(white);
    diaSetSlider(whiteSlider, white);
}

void InfoControls::setMovieModel( int which )
{
    diaSetGroup(modeGroup, which);
}

// Update the Object, Contour, Point
void InfoControls::updateOCP( int *newVal, int *maxVal )
{
    int i;
    for (i = 0; i < 3; i++) {
	if (mLastOCPmax[i] != maxVal[i]) {
	    if (maxVal[i] < 0)
		mStr = "    ";
	    else if (maxVal[i] >9999)
		mStr.sprintf("/%d", maxVal[i]);
	    else
		mStr.sprintf("/ %d", maxVal[i]);
	    mOCPLabel[i]->setText(mStr);
	    
	    // Block signals when changing the maximum to avoid signals for value changing
	    mOCPBox[i]->blockSignals(true);
	    mOCPBox[i]->setMaxValue(maxVal[i] > 0 ? maxVal[i] : 0);
	    mOCPBox[i]->blockSignals(false);
	}
	 
	// If value changed, set special value text appropriately
	if (mLastOCPval[i] != newVal[i]) {
	    mOCPBox[i]->setSpecialValueText(newVal[i] < 0 ? "   " : "--x--");
	    diaSetSpinBox(mOCPBox[i], newVal[i] > 0 ? newVal[i] : 0);
	}
	mLastOCPmax[i] = maxVal[i];
	mLastOCPval[i] = newVal[i];
    }
}

// Update the x/y/z boxes and labels.  Only set something if it has changed
void InfoControls::updateXYZ( int *newVal, int *maxVal )
{
    int i;
    for (i = 0; i < 3; i++) {
	if (maxVal[i] != mLastXYZval[i]) {
	    if (maxVal[i] >9999)
		mStr.sprintf("/%d", maxVal[i]);
	    else
		mStr.sprintf("/ %d", maxVal[i]);
	    mXYZLabel[i]->setText(mStr);
	    
	    // Block signals when changing the maximum to avoid signals for value changing
	    mXYZBox[i]->blockSignals(true);
	    mXYZBox[i]->setMaxValue(maxVal[i]);
	    mXYZBox[i]->blockSignals(false);
	}
	if (newVal[i] != mLastXYZval[i])
	    diaSetSpinBox(mXYZBox[i], newVal[i]);
	mLastXYZval[i] = newVal[i];
	mLastXYZmax[i] = maxVal[i];
    }
}

// Set object color into some widgets
void InfoControls::setObjectColor( QColor foreColor, QColor backColor )
{
    objectLabel->setPaletteForegroundColor(foreColor);
    objectLabel->setPaletteBackgroundColor(backColor);
    objectSpinBox->setPaletteForegroundColor(foreColor);
    objectSpinBox->setPaletteBackgroundColor(backColor);
    maxObjectLabel->setPaletteForegroundColor(foreColor);
    maxObjectLabel->setPaletteBackgroundColor(backColor);
}

// Set the model name into the label
void InfoControls::setModelName( char *name )
{
    mStr = name;
    modelLabel->setText(mStr);
}

// Set the image file name into the label
void InfoControls::setImageName( char *name )
{
    mStr = name;
    //imageLabel->setText(mStr);
}

// Set the show point toggle
void InfoControls::setShowPoint( int state )
{
    mShowPoint =  state != 0;
    diaSetChecked(showCheckBox, mShowPoint);
}



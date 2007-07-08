/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename functions or slots use
** Qt Designer which will update this file, preserving your code. Create an
** init() function in place of a constructor, and a destroy() function in
** place of a destructor.
*****************************************************************************/

void FineGrainForm::init()
{
    char *labels[] = {"Transparency"};
    int i;
    mLineSelector = NULL;
    mFillSelector = NULL;
    
    // Setup up the trans slider
    QVBoxLayout *layout = new QVBoxLayout(transSliderFrame);
    mTransSlider = new MultiSlider(transSliderFrame, 1, labels, 0, 100);
    layout->addLayout(mTransSlider->getLayout());
    connect(mTransSlider, SIGNAL(sliderChanged(int, int, bool)), this,
       SLOT(transSliderChanged(int, int, bool)));
    QToolTip::add(mTransSlider->getSlider(0), "Set transparency");
    
    // Initialize the arrays of pointers and flags
    mLastButs[0] = lastColorBut;
    mLastButs[1] = lastFillColorBut;
    mLastButs[2] = lastTransBut;
    mLastButs[3] = lastWidth2DBut;
    mLastButs[4] = lastWidth3DBut;
    mLastButs[5] = lastSymtypeBut;
    mLastButs[6] = lastSymsizeBut;
    mEndButs[0] = endColorBut;
    mEndButs[1] = endFillColorBut;
    mEndButs[2] = endTransBut;
    mEndButs[3] = endWidth2DBut;
    mEndButs[4] = endWidth3DBut;
    mEndButs[5] = endSymtypeBut;
    mEndButs[6] = endSymsizeBut;
    mClearButs[0] = clearColorBut;
    mClearButs[1] = clearFillColorBut;
    mClearButs[2] = clearTransBut;
    mClearButs[3] = clearWidth2DBut;
    mClearButs[4] = clearWidth3DBut;
    mClearButs[5] = clearSymtypeBut;
    mClearButs[6] = clearSymsizeBut;
    mDSlabels[0] = colorDSLabel;
    mDSlabels[1] = fillColorDSLabel;
    mDSlabels[2] = transDSLabel;
    mDSlabels[3] = width2DDSLabel;
    mDSlabels[4] = width3DDSLabel;
    mDSlabels[5] = symtypeDSLabel;
    mDSlabels[6] = symsizeDSLabel;
    mTypeValues[0] = GEN_STORE_COLOR;
    mTypeValues[1] = GEN_STORE_FCOLOR;
    mTypeValues[2] = GEN_STORE_TRANS;
    mTypeValues[3] = GEN_STORE_2DWIDTH;
    mTypeValues[4] = GEN_STORE_3DWIDTH;
    mTypeValues[5] = GEN_STORE_SYMTYPE;
    mTypeValues[6] = GEN_STORE_SYMSIZE;
    mChangeFlags[0] = CHANGED_COLOR;
    mChangeFlags[1] = CHANGED_FCOLOR;
    mChangeFlags[2] = CHANGED_TRANS;
    mChangeFlags[3] = CHANGED_2DWIDTH;
    mChangeFlags[4] = CHANGED_3DWIDTH;
    mChangeFlags[5] = CHANGED_SYMTYPE;
    mChangeFlags[6] = CHANGED_SYMSIZE;
    mSymTable[0] = IOBJ_SYM_NONE;
    mSymTable[1] = IOBJ_SYM_CIRCLE;
    mSymTable[2] = IOBJ_SYM_SQUARE;
    mSymTable[3] = IOBJ_SYM_TRIANGLE;
    mPtContSurf = 0;
    mLastRed = mLastGreen = mLastBlue = -1;
    mLastFillRed = mLastFillGreen = mLastFillBlue = -1;
    mLastTrans = mLast2Dwidth = mLast3Dwidth = mLastSymsize = -1;
    mLastSymType = -1;
    mCtrlPressed = false;
    
    // Get the signal mappers
    QSignalMapper *endMapper = new QSignalMapper(this);
    QSignalMapper *clearMapper = new QSignalMapper(this);
    connect(endMapper, SIGNAL(mapped(int)), this, SLOT(endClicked(int)));
    connect(clearMapper, SIGNAL(mapped(int)), this, SLOT(clearClicked(int)));
    
    // Connect to the mappers
    for (i = 0; i < 7; i++) {
        endMapper->setMapping(mEndButs[i], i);
        connect(mEndButs[i], SIGNAL(clicked()), endMapper, SLOT(map()));
        clearMapper->setMapping(mClearButs[i], i);
        connect(mClearButs[i], SIGNAL(clicked()), clearMapper, SLOT(map()));
    }
    setFontDependentWidths();
}

// Set up buttons etc to fixed width
void FineGrainForm::setFontDependentWidths()
{
    int i, cwid, swid, lwid, ewid, dswid;
    bool rounded = ImodPrefs->getRoundedStyle();
    dswid = diaSetButtonWidth(setColorBut, rounded, 1.3, "Set");
    setFillColorBut->setFixedWidth(dswid);
    cwid = diaSetButtonWidth(clearColorBut, rounded, 1.25, "Clear");
    lwid = diaSetButtonWidth(lastColorBut, rounded, 1.25, "Last");
    ewid = diaSetButtonWidth(endColorBut, rounded, 1.25, "End");
    dswid = (int)(1.1 * colorDSLabel->fontMetrics().width("D") + 0.5);
    colorDSLabel->setFixedWidth(dswid);
    for (i = 1; i < 7; i++) {
       mDSlabels[i]->setFixedWidth(dswid);
       mLastButs[i]->setFixedWidth(lwid);
       mEndButs[i]->setFixedWidth(ewid);
       mClearButs[i]->setFixedWidth(cwid);
    }
}

// Update the interface with the current properties and states
void FineGrainForm::update( int ptContSurf, bool enabled, DrawProps *props, int stateFlags, 
                            bool nextEnabled)
{
    int i;
    diaSetGroup(surfContPtGroup, ptContSurf);
    nextChangeButton->setEnabled(nextEnabled);
    for (i = 0; i < 7; i++) {
        bool changed = stateFlags & mChangeFlags[i];
        mEndButs[i]->setEnabled(!ptContSurf &&changed && enabled);
        mClearButs[i]->setEnabled(changed && enabled);
        mDSlabels[i]->setText((changed && enabled) ? "S" : "D");
    }
    lastColorBut->setEnabled(mLastRed >= 0 && enabled);
    lastFillColorBut->setEnabled(mLastFillRed >= 0 && enabled);
    lastTransBut->setEnabled(mLastTrans >= 0 && enabled);
    lastWidth2DBut->setEnabled(mLast2Dwidth >= 0 && enabled);
    lastWidth3DBut->setEnabled(mLast3Dwidth >= 0 && enabled);
    lastSymtypeBut->setEnabled(mLastSymType >= 0 && enabled);
    lastSymsizeBut->setEnabled(mLastSymsize >= 0 && enabled);
    mCurRed = (int)(255. * props->red);
    mCurGreen = (int)(255. * props->green);
    mCurBlue = (int)(255. * props->blue);
    colorFrame->setPaletteBackgroundColor(QColor(mCurRed, mCurGreen, mCurBlue));
    mCurFillRed = (int)(255. * props->fillRed);
    mCurFillGreen = (int)(255. * props->fillGreen);
    mCurFillBlue = (int)(255. * props->fillBlue);
    fillColorFrame->setPaletteBackgroundColor(QColor(mCurFillRed, mCurFillGreen,
                                                     mCurFillBlue));
    setColorBut->setEnabled(enabled);
    setFillColorBut->setEnabled(enabled);
    mTransSlider->getSlider(0)->setEnabled(enabled);
    mTransSlider->setValue(0, props->trans);
    diaSetSpinBox(width2DSpin, props->linewidth2);
    width2DSpin->setEnabled(enabled);
    diaSetSpinBox(width3DSpin, props->linewidth);
    width3DSpin->setEnabled(enabled);
    symbolComboBox->blockSignals(true);
    for (i = 0; i < 4; i++)
       if (props->symtype == mSymTable[i])
    symbolComboBox->setCurrentItem(i);
    symbolComboBox->blockSignals(false);
    symbolComboBox->setEnabled(enabled);
    diaSetChecked(fillCheckBox, (props->symflags & IOBJ_SYMF_FILL) != 0);
    fillCheckBox->setEnabled(enabled);
    diaSetSpinBox(symsizeSpin, props->symsize);
    symsizeSpin->setEnabled(enabled);
    diaSetChecked(gapCheckBox, props->gap);
    gapCheckBox->setText(QString(ptContSurf ? "Turn off drawing" : "Gap to next point"));
    gapCheckBox->setEnabled(enabled);
    diaSetSpinBox(connectSpin, props->connect);
    connectSpin->setEnabled(ptContSurf < 2 && enabled);
}

void FineGrainForm::ptContSurfSelected( int which )
{
    ifgPtContSurfSelected(which);
}

void FineGrainForm::nextChangeClicked()
{
    ifgGotoNextChange();
}

// Open color selectors for line and fill color
void FineGrainForm::setLineColor()
{
    if (mLineSelector) {
        mLineSelector->raise();
        return;
    }
  mLineSelector = new ColorSelector(this, "Fine grain line color", 
                                    mCurRed, mCurGreen,
                                mCurBlue, hotSliderFlag(),
                                hotSliderKey(), ImodPrefs->getRoundedStyle(),
                                "selector");
  connect(mLineSelector, SIGNAL(newColor(int, int, int)), this,
          SLOT(newLineColor(int, int, int)));
  connect(mLineSelector, SIGNAL(done()), this, SLOT(lineColorDone()));
  connect(mLineSelector, SIGNAL(closing()), this, SLOT(lineColorClosing()));
  connect(mLineSelector, SIGNAL(keyPress(QKeyEvent *)), this,
          SLOT(keyPressEvent(QKeyEvent *)));
  connect(mLineSelector, SIGNAL(keyRelease(QKeyEvent *)), this,
          SLOT(keyReleaseEvent(QKeyEvent *)));
  imodDialogManager.add((QWidget *)mLineSelector, IMOD_DIALOG);
  mLineSelector->setCaption(imodCaption("3dmod Line Color"));
  mLineSelector->show();
}

void FineGrainForm::setFillColor()
{
    if (mFillSelector) {
        mFillSelector->raise();
        return;
    }
  mFillSelector = new ColorSelector(this, "Fine grain fill color",
                                    mCurFillRed, mCurFillGreen,
                                mCurFillBlue, hotSliderFlag(),
                                hotSliderKey(), ImodPrefs->getRoundedStyle(),
                                "selector");
  connect(mFillSelector, SIGNAL(newColor(int, int, int)), this,
          SLOT(newFillColor(int, int, int)));
  connect(mFillSelector, SIGNAL(done()), this, SLOT(fillColorDone()));
  connect(mFillSelector, SIGNAL(closing()), this, SLOT(fillColorClosing()));
  connect(mFillSelector, SIGNAL(keyPress(QKeyEvent *)), this,
          SLOT(keyPressEvent(QKeyEvent *)));
  connect(mFillSelector, SIGNAL(keyRelease(QKeyEvent *)), this,
          SLOT(keyReleaseEvent(QKeyEvent *)));
  imodDialogManager.add((QWidget *)mFillSelector, IMOD_DIALOG);
  mFillSelector->setCaption(imodCaption("3dmod Fill Color"));
  mFillSelector->show();
}

// Slots for the color changes
void FineGrainForm::newLineColor( int red, int green, int blue )
{
    mLastRed = red;
    mLastGreen = green;
    mLastBlue = blue;
    ifgLineColorChanged(red, green, blue);
}

void FineGrainForm::newFillColor( int red, int green, int blue )
{
    mLastFillRed = red;
    mLastFillGreen = green;
    mLastFillBlue = blue;
    ifgFillColorChanged(red, green, blue);
}

void FineGrainForm::lineColorDone()
{
    if (mLineSelector)
        mLineSelector->close();
}

void FineGrainForm::fillColorDone()
{
    if (mFillSelector)
        mFillSelector->close();
}

void FineGrainForm::lineColorClosing()
{
    imodDialogManager.remove((QWidget *)mLineSelector);
    mLineSelector = NULL;
}

void FineGrainForm::fillColorClosing()
{
    imodDialogManager.remove((QWidget *)mFillSelector);
    mFillSelector = NULL;
}

// Slots for the last buttons, to set the last values
void FineGrainForm::lastLineColor()
{
    ifgLineColorChanged(mLastRed, mLastGreen, mLastBlue);
}

void FineGrainForm::lastFillColor()
{
    ifgFillColorChanged(mLastRed, mLastGreen, mLastBlue);
}

void FineGrainForm::lastTrans()
{
    ifgTransChanged(mLastTrans);
}

void FineGrainForm::last2DWidth()
{
    ifgWidth2DChanged(mLast2Dwidth);
}

void FineGrainForm::last3DWidth()
{
    ifgWidth3DChanged(mLast3Dwidth);
}

void FineGrainForm::lastSymtype()
{
    ifgSymtypeChanged(mLastSymType, mLastSymFill);
}

void FineGrainForm::lastSymsize()
{
    ifgSymsizeChanged(mLastSymsize);
}

// Slots for the mapped called from end and clear buttons
void FineGrainForm::endClicked( int which )
{
    ifgEndChange(mTypeValues[which]);
}

void FineGrainForm::clearClicked( int which )
{
    ifgClearChange(mTypeValues[which]);
}

// Slots for the individual changes
void FineGrainForm::transSliderChanged( int which, int value, bool dragging )
{
    if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)) {
        ifgTransChanged(value);
        mLastTrans = value;
    }
}

void FineGrainForm::width2DChanged( int value )
{
    setFocus();
    mLast2Dwidth = value;
    ifgWidth2DChanged(value);
}

void FineGrainForm::width3DChanged( int value )
{
    setFocus();
    mLast3Dwidth = value;
    ifgWidth3DChanged(value);
}

void FineGrainForm::symsizeChanged( int value )
{
    setFocus();
    mLastSymsize = value;
    ifgSymsizeChanged(value);
}

// Save fill state and symbol type if either is clicked, pass both on
void FineGrainForm::symtypeSelected( int value )
{
    mLastSymType = mSymTable[value];
    mLastSymFill = fillCheckBox->isChecked();
    ifgSymtypeChanged(mLastSymType, mLastSymFill);
}

void FineGrainForm::fillToggled( bool state )
{
    mLastSymType = mSymTable[symbolComboBox->currentItem()];
    mLastSymFill = state;
    ifgSymtypeChanged(mLastSymType, mLastSymFill);
}

void FineGrainForm::gapToggled( bool state )
{
    ifgGapChanged(state);
}

void FineGrainForm::connectChanged( int value )
{
    setFocus();
    ifgConnectChanged(value);
}

void FineGrainForm::helpClicked()
{
    ifgHelp();
}

void FineGrainForm::closeEvent( QCloseEvent *e )
{
    ifgClosing();
    lineColorDone();
    fillColorDone();
    e->accept();
}

void FineGrainForm::keyPressEvent( QKeyEvent *e )
{
  if (e->key() == Qt::Key_Escape) {
      close();
    } else if (e->key() == Qt::Key_D && 
               !(e->state() & (Qt::ShiftButton | Qt::ControlButton))) {
      ifgDump();
    } else {
      if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
        mCtrlPressed = true;
        grabKeyboard();
    }
      ivwControlKey(0, e);
    }
}

void FineGrainForm::keyReleaseEvent( QKeyEvent *e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  ivwControlKey(1, e);
}

void FineGrainForm::fontChange( const QFont & oldFont )
{
    setFontDependentWidths();
    QWidget::fontChange(oldFont);
}

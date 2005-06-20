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
    mSurfContPt = 2;
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

void FineGrainForm::setFontDependentWidths()
{
    int i, cwid, swid, lwid, ewid, dswid;
    bool rounded = ImodPrefs->getRoundedStyle();
    dswid = diaSetButtonWidth(setColorBut, rounded, 1.2, "Set");
    setFillColorBut->setFixedWidth(dswid);
    cwid = diaSetButtonWidth(clearColorBut, rounded, 1.2, "Clear");
    lwid = diaSetButtonWidth(lastColorBut, rounded, 1.2, "Last");
    ewid = diaSetButtonWidth(clearColorBut, rounded, 1.2, "End");
    dswid = (int)(1.1 * colorDSLabel->fontMetrics().width("D") + 0.5);
    colorDSLabel->setFixedWidth(dswid);
    for (i = 1; i < 7; i++) {
       mDSlabels[i]->setFixedWidth(dswid);
       mLastButs[i]->setFixedWidth(lwid);
       mEndButs[i]->setFixedWidth(ewid);
       mClearButs[i]->setFixedWidth(cwid);
    }
}



void FineGrainForm::update( int surfContPt, bool enabled, DrawProps *props, int stateFlags)
{
    int i;
    for (i = 0; i < 7; i++) {
        bool changed = stateFlags & mChangeFlags[i];
        mEndButs[i]->setEnabled((surfContPt == 2) &&changed && enabled);
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
    colorFrame->setPaletteBackgroundColor(QColor((int)(255. * props->red),
           (int)(255. * props->green), (int)(255. * props->blue)));
    fillColorFrame->setPaletteBackgroundColor(QColor((int)(255. * props->fillRed),
           (int)(255. * props->fillGreen), (int)(255. * props->fillBlue)));
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
    gapCheckBox->setEnabled((surfContPt == 2) && enabled);
    diaSetSpinBox(connectSpin, props->connect);
    connectSpin->setEnabled(enabled);
}

void FineGrainForm::surfContPtSelected( int which )
{

}

void FineGrainForm::setLineColor()
{

}


void FineGrainForm::setFillColor()
{

}

void FineGrainForm::lastLineColor()
{

}

void FineGrainForm::lastFillColor()
{

}

void FineGrainForm::lastTrans()
{

}

void FineGrainForm::last2DWidth()
{

}

void FineGrainForm::last3DWidth()
{

}

void FineGrainForm::lastSymtype()
{

}

void FineGrainForm::lastSymsize()
{

}


void FineGrainForm::endClicked( int which )
{

}


void FineGrainForm::clearClicked( int which )
{

}

void FineGrainForm::transSliderChanged( int which, int value, bool dragging )
{

}

void FineGrainForm::width2DChanged( int value )
{

}

void FineGrainForm::width3DChanged( int value )
{

}

void FineGrainForm::symsizeChanged( int value )
{

}

void FineGrainForm::symtypeSelected( int value )
{

}

void FineGrainForm::fillToggled( bool state )
{

}

void FineGrainForm::gapToggled( bool state )
{

}

void FineGrainForm::connectChanged( int value )
{

}



void FineGrainForm::helpClicked()
{

}


void FineGrainForm::closeEvent( QCloseEvent *e )
{
    e->accept();
}


void FineGrainForm::keyPressEvent( QKeyEvent *e )
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



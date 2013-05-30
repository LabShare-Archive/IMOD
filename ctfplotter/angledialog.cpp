/*
* angledialog.cpp - callbacks for the angle range dialog.
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*/
#include <QtGui>

#include "angledialog.h"
#include "plotter.h"
#include "myapp.h"
#include "b3dutil.h"
#include "ilist.h"
#include "parse_params.h" //for exitError()

AngleDialog::AngleDialog(MyApp *app, QWidget *parent): QDialog(parent)
{
  int width, i;
  mParamsOpen = true;
  mApp = app;
  setWindowTitle(tr("Angle Range & Tile Selection"));
  mDefocusLabel = new QLabel(tr("E&xpected defocus (um): "), this);
  mDefocusEdit = new QLineEdit("6.0", this);

  // Buddy means the accelerator key for the label sets focus to the text box
  // This is meaningless if there is no unique & included in the label!
  mDefocusLabel->setBuddy(mDefocusEdit);
  mDefocusEdit->setFixedWidth(fontMetrics().width("    99.99"));
  mDefocusEdit->setToolTip("Nominal defocus value in microns");

  mLowAngleLabel = new QLabel(tr("Starti&ng tilt angle: "), this);
  mLowAngleEdit = new QLineEdit(tr("-90.0"), this);
  mLowAngleLabel->setBuddy(mLowAngleEdit);
  mLowAngleEdit->setToolTip("Tilt angle at negative end of range to include in fit");

  mHighAngleLabel = new QLabel(tr("Endin&g tilt angle:  "), this);
  mHighAngleEdit = new QLineEdit(tr("90.0"), this);
  mHighAngleLabel->setBuddy(mHighAngleEdit);
  mHighAngleEdit->setToolTip("Tilt angle at positive end of range to include in fit");

  mStepUpButton = new QPushButton( tr("Step &Up"), this);
  mStepUpButton->setToolTip("Increase starting and ending angles by the step");
 
  mStepDownButton = new QPushButton( tr("Step &Down"), this);
  mStepDownButton->setToolTip("Decrease starting and ending angles by the step");
  width = (int)(1.35 * mStepDownButton->fontMetrics().width("Step Down") + 0.5);
  mStepDownButton->setFixedWidth(width);
  mStepUpButton->setFixedWidth(width);
 
  mAutofitButton = new QPushButton(mApp->getFitSingleViews() ? 
                                   tr("Autofit A&ll Single Views") : 
                                   tr("Autofit A&ll Steps"), this);
  mAutofitButton->setEnabled(false);
  mAutofitButton->setToolTip("Fit to all tilt angle ranges that fit within limits below");

  mRangeStepLabel = new QLabel(tr("Ste&p angle range by:  "), this);
  mRangeStepEdit = new QLineEdit(tr("90.0"), this);
  mRangeStepLabel->setBuddy(mRangeStepEdit);
  mRangeStepEdit->setToolTip("Amount to change starting and ending tilt angles with the"
                             " Step buttons");

  mAutoFromLabel = new QLabel(tr("Autof&it: "), this);
  mAutoFromEdit = new QLineEdit(tr("90.0"), this);
  mAutoFromLabel->setBuddy(mAutoFromEdit);
  mAutoFromEdit->setToolTip("Starting angle of range to cover with autofitting to "
                            "stepped ranges");
  width = mAutoFromEdit->fontMetrics().width("   -99.99");
  mAutoFromEdit->setFixedWidth(width);

  mAutoToLabel = new QLabel(tr("t&o"), this);
  mAutoToEdit = new QLineEdit(tr("90.0"), this);
  mAutoToEdit->setFixedWidth(width);
  mAutoToLabel->setBuddy(mAutoToEdit);
  mAutoToEdit->setToolTip("Ending angle of range to cover with autofitting to "
                            "stepped ranges");

  mFitSingleBox = new QCheckBox(tr("Fit each view separately"), this);
  mFitSingleBox->setToolTip("Fit every view in the range separately");
  mFitSingleBox->setChecked(mApp->getFitSingleViews());

  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );

  QFrame *line2 = new QFrame(this);
  line2->setFrameShape( QFrame::HLine );
  line2->setFrameShadow( QFrame::Sunken );

  QFrame *line3 = new QFrame(this);
  line3->setFrameShape( QFrame::HLine );
  line3->setFrameShadow( QFrame::Sunken );

  mTileParamButton = new QPushButton("+", this);
  width = mTileParamButton->fontMetrics().width(" + ");
  mTileParamButton->setFixedWidth(width);
  mTileParamButton->setFixedHeight(width);
  QLabel *tileParamLabel = new QLabel(tr("Tile parameters"), this);

  mDefTolLabel = new QLabel(tr("Center defocus tol (nm)"), this);
  mDefTolEdit = new QLineEdit(tr("200"), this);
  mDefTolLabel->setBuddy(mDefTolEdit);
  mDefTolEdit->setMinimumWidth(fontMetrics().width(" 9999"));
  mDefTolEdit->setToolTip("Maximum defocus difference for central tiles");

  mLeftTolLabel = new QLabel(tr("Left defocus tol (nm)"), this);
  mLeftTolEdit = new QLineEdit(tr("200"), this);
  mLeftTolLabel->setBuddy(mLeftTolEdit);
  mLeftTolEdit->setToolTip("Maximum defocus difference for non-central tiles on left");

  mRightTolLabel = new QLabel(tr("Right defocus tol (nm)"), this);
  mRightTolEdit = new QLineEdit(tr("200"), this);
  mRightTolLabel->setBuddy(mRightTolEdit);
  mRightTolEdit->setToolTip("Maximum defocus difference for non-central tiles on right");

  mTileSizeLabel = new QLabel(tr("Tile size:           "), this);
  mTileSizeEdit = new QLineEdit(tr("256"),this);
  mTileSizeLabel->setBuddy(mTileSizeEdit);
  mTileSizeEdit->setToolTip("Size of square, overlapping tiles to analyze");

  mAxisAngleLabel = new QLabel(tr("Tilt axis angle:   "), this);
  mAxisAngleEdit = new QLineEdit( tr("0.0"), this);
  mAxisAngleLabel->setBuddy(mAxisAngleEdit);
  mAxisAngleEdit->setToolTip("Angle of tilt axis from vertical");

  mDefocusGroup = new QGroupBox(tr("Which defocus to use"), this);
  QButtonGroup *butGroup = new QButtonGroup(this);
  mExpDefocusRadio = new QRadioButton(tr("Expected defocus"));
  mExpDefocusRadio->setToolTip("Use expected defocus for shifting spectra "
                              "from off-center tiles");
  mCurrDefocusRadio = new QRadioButton(tr("Current defocus estimate"));
  mCurrDefocusRadio->setToolTip("Use current defocus estimate for shifting "
                               "spectra from off-center tiles");
  if (mApp->getDefocusOption())
    mCurrDefocusRadio->setChecked(true);
  else
    mExpDefocusRadio->setChecked(true);
  butGroup->addButton(mExpDefocusRadio);
  butGroup->addButton(mCurrDefocusRadio);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(mExpDefocusRadio);
  vbox->addWidget(mCurrDefocusRadio);
  mDefocusGroup->setLayout(vbox);

  // This is what I usually do for more compact group boxes
  vbox->setSpacing(0);
  vbox->setContentsMargins(5, 2, 5, 5);

  mIfAllGroup = new QGroupBox(tr("Initial tiles to include"), this);
  butGroup = new QButtonGroup(this);
  mOnlyCenterRadio = new QRadioButton(tr("Only central tiles")); 
  mAllAtOnceRadio = new QRadioButton(tr("All tiles"));
  mOnlyCenterRadio->setToolTip("Compute curve initially only from central "
                              "tiles");
  mAllAtOnceRadio->setToolTip("Compute curve using all available tiles");
  if (mApp->getInitTileOption())
    mAllAtOnceRadio->setChecked(true);
  else
    mOnlyCenterRadio->setChecked(true);
  butGroup->addButton(mAllAtOnceRadio);
  butGroup->addButton(mOnlyCenterRadio);

  QVBoxLayout *vbox2 = new QVBoxLayout;
  vbox2->addWidget(mOnlyCenterRadio);
  vbox2->addWidget(mAllAtOnceRadio);
  mIfAllGroup->setLayout(vbox2);
  vbox2->setSpacing(0);
  vbox2->setContentsMargins(5, 2, 5, 5);

  mSaveButton = new QPushButton( tr("&Store Defocus in Table"), this);
  mSaveButton->setEnabled(true);
  mSaveButton->setToolTip("Add current defocus value to table below");

  mApplyButton = new QPushButton( tr("&Apply"), this);
  mApplyButton->setDefault(true);
  mApplyButton->setEnabled(false);
  mApplyButton->setToolTip("Compute power spectra with current settings");
 
  mCloseButton = new QPushButton( tr("&Close"), this);
  mCloseButton->setFixedWidth(3 * fontMetrics().width("Close"));
  mCloseButton->setToolTip("Close this dialog");

  mTable = new QTableWidget(0, 4, this);
  mTable->setSelectionBehavior(QAbstractItemView::SelectRows);
  mTable->setSelectionMode(QAbstractItemView::SingleSelection);
  mTable->setEditTriggers(QAbstractItemView::NoEditTriggers);
  QStringList headers;
  headers << "Start" << "End" << "Middle" << "Defocus";
  mTable->setHorizontalHeaderLabels(headers);
  width = (int)(1.66 * mTable->fontMetrics().width("Defocus"));
  for (i = 0; i < 4; i++)
    mTable->setColumnWidth(i, width);
  
  mDeleteButton = new QPushButton( tr("Delete Row"), this);
  mReturnButton = new QPushButton( tr("Set &Tilt Angles"), this);
  mToFileButton = new QPushButton( tr("Sa&ve to File"), this);
  mDeleteButton->setToolTip("Remove this row from the table");
  mReturnButton->setToolTip("Set starting and ending angles from this row and "
                            "recompute spectrum");
  mToFileButton->setToolTip("Save angle and defocus values in table to file");
  mTileParamButton->setFocusPolicy(Qt::NoFocus);
  mStepUpButton->setFocusPolicy(Qt::NoFocus);
  mStepDownButton->setFocusPolicy(Qt::NoFocus);
  mAutofitButton->setFocusPolicy(Qt::NoFocus);
  mFitSingleBox->setFocusPolicy(Qt::NoFocus);
  mDeleteButton->setFocusPolicy(Qt::NoFocus);
  mReturnButton->setFocusPolicy(Qt::NoFocus);
  mToFileButton->setFocusPolicy(Qt::NoFocus);
  mSaveButton->setFocusPolicy(Qt::NoFocus);

  connect(mDefocusEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mLowAngleEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mLowAngleEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mDefTolEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mTileSizeEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mAxisAngleEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mLeftTolEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mRightTolEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );

  connect(mExpDefocusRadio, SIGNAL(clicked()), this, SLOT(expDefocusChecked()));
  connect(mCurrDefocusRadio, SIGNAL(clicked()),this, SLOT(currDefocusChecked()));
  connect(mOnlyCenterRadio, SIGNAL(clicked()), this, SLOT(onlyCenterChecked()));
  connect(mAllAtOnceRadio, SIGNAL(clicked()), this, SLOT(allAtOnceChecked()) );
  
  connect(mTileParamButton, SIGNAL(clicked()), this, SLOT(tileParamsClicked()) );
  connect(mApplyButton, SIGNAL(clicked()), this, SLOT(applyClicked()) );
  connect(mStepUpButton, SIGNAL(clicked()), this, SLOT(stepUpClicked()) );
  connect(mAutofitButton, SIGNAL(clicked()), this, SLOT(autofitClicked()) );
  connect(mStepDownButton, SIGNAL(clicked()), this, SLOT(stepDownClicked()) );
  connect(mFitSingleBox, SIGNAL(toggled(bool)), this, SLOT(fitSingleToggled(bool)));
  connect(mCloseButton, SIGNAL(clicked()), this, SLOT(close()) );
  connect(mSaveButton, SIGNAL(clicked()), mApp, SLOT(saveCurrentDefocus()) );
  connect(mDeleteButton, SIGNAL(clicked()), this, SLOT(deleteClicked()) );
  connect(mReturnButton, SIGNAL(clicked()), this, SLOT(setAnglesClicked()) );
  connect(mToFileButton, SIGNAL(clicked()), mApp, SLOT(writeDefocusFile()) );
  connect(mTable, SIGNAL(cellDoubleClicked(int, int)), this, 
          SLOT(rowDoubleClicked(int, int)));

  QHBoxLayout *defHLayout = new QHBoxLayout;
  defHLayout->addWidget(mDefocusLabel);
  defHLayout->addWidget(mDefocusEdit);
  
  QHBoxLayout *defTolHLayout = new QHBoxLayout;
  defTolHLayout->addWidget(mDefTolLabel);
  defTolHLayout->addWidget(mDefTolEdit);
  
  QHBoxLayout *leftTolHLayout = new QHBoxLayout;
  leftTolHLayout->addWidget(mLeftTolLabel);
  leftTolHLayout->addWidget(mLeftTolEdit);
  
  QHBoxLayout *rightTolHLayout = new QHBoxLayout;
  rightTolHLayout->addWidget(mRightTolLabel);
  rightTolHLayout->addWidget(mRightTolEdit);

  QHBoxLayout *lAngleHLayout = new QHBoxLayout;
  lAngleHLayout->addWidget(mLowAngleLabel);
  lAngleHLayout->addWidget(mLowAngleEdit);

  QHBoxLayout *hAngleHLayout = new QHBoxLayout;
  hAngleHLayout->addWidget(mHighAngleLabel);
  hAngleHLayout->addWidget(mHighAngleEdit);

  QHBoxLayout *stepSizeHLayout = new QHBoxLayout;
  stepSizeHLayout->addWidget(mRangeStepLabel);
  stepSizeHLayout->addWidget(mRangeStepEdit);

  QHBoxLayout *stepButHLayout = new QHBoxLayout;
  stepButHLayout->addWidget(mStepUpButton);
  stepButHLayout->addWidget(mStepDownButton);

  QHBoxLayout *autoRangeHLayout = new QHBoxLayout;
  autoRangeHLayout->addWidget(mAutoFromLabel);
  autoRangeHLayout->addWidget(mAutoFromEdit);
  autoRangeHLayout->addWidget(mAutoToLabel);
  autoRangeHLayout->addWidget(mAutoToEdit);

  QHBoxLayout *tileParamHLayout = new QHBoxLayout;
  tileParamHLayout->addWidget(mTileParamButton);
  tileParamHLayout->addWidget(tileParamLabel);

  QHBoxLayout *tileSizeHLayout = new QHBoxLayout;
  tileSizeHLayout->addWidget(mTileSizeLabel);
  tileSizeHLayout->addWidget(mTileSizeEdit);

  QHBoxLayout *axisAngleHLayout = new QHBoxLayout;
  axisAngleHLayout->addWidget(mAxisAngleLabel);
  axisAngleHLayout->addWidget(mAxisAngleEdit);

  QHBoxLayout *bottomHLayout = new QHBoxLayout;
  bottomHLayout->addWidget(mCloseButton);

  QHBoxLayout *tabHLayout = new QHBoxLayout;
  tabHLayout->addWidget(mDeleteButton);
  tabHLayout->addWidget(mReturnButton);
  tabHLayout->addWidget(mToFileButton);

  QVBoxLayout *leftVLayout = new QVBoxLayout;
  leftVLayout->addLayout(lAngleHLayout);
  leftVLayout->addLayout(hAngleHLayout);
  leftVLayout->addWidget(line);
  leftVLayout->addLayout(stepSizeHLayout);
  leftVLayout->addLayout(stepButHLayout);
  leftVLayout->addWidget(line2);
  leftVLayout->addWidget(mAutofitButton);
  leftVLayout->addLayout(autoRangeHLayout);
  leftVLayout->addWidget(mFitSingleBox);
  leftVLayout->addWidget(line3);
  leftVLayout->addLayout(tileParamHLayout);
  leftVLayout->addLayout(defTolHLayout);
  leftVLayout->addLayout(leftTolHLayout);
  leftVLayout->addLayout(rightTolHLayout);

  QVBoxLayout *rightVLayout = new QVBoxLayout;
  rightVLayout->addLayout(defHLayout);
  rightVLayout->addWidget(mDefocusGroup);
  rightVLayout->addWidget(mIfAllGroup);
  rightVLayout->addWidget(mApplyButton);
  rightVLayout->addWidget(mSaveButton);
  rightVLayout->addStretch();
  rightVLayout->addLayout(tileSizeHLayout);
  rightVLayout->addLayout(axisAngleHLayout);

  QHBoxLayout *topHLayout = new QHBoxLayout;
  topHLayout->addLayout(leftVLayout);
  topHLayout->addLayout(rightVLayout);
  
  QVBoxLayout *mainVLayout = new QVBoxLayout(this);
  mainVLayout->addLayout(topHLayout);
  mainVLayout->addWidget(mTable);
  mainVLayout->setStretchFactor(mTable, 100);
  mainVLayout->addLayout(tabHLayout);
  mainVLayout->addLayout(bottomHLayout);
}

/* 
 * Show or hide the tile parameters and resize the window
 */
void AngleDialog::tileParamsClicked()
{
  int curWidth = width();
  mParamsOpen = !mParamsOpen;
  mTileParamButton->setText(mParamsOpen ? "-" : "+");
  mTileParamButton->setToolTip
    (QString(mParamsOpen ? "Hide" : "Show") +
     QString(" tile size, tolerance, and tilt axis angle entries"));

  mApp->showHideWidget(mDefTolLabel, mParamsOpen);
  mApp->showHideWidget(mLeftTolLabel, mParamsOpen);
  mApp->showHideWidget(mRightTolLabel, mParamsOpen);
  mApp->showHideWidget(mTileSizeLabel, mParamsOpen);
  mApp->showHideWidget(mAxisAngleLabel, mParamsOpen);
  mApp->showHideWidget(mDefTolEdit, mParamsOpen);
  mApp->showHideWidget(mLeftTolEdit, mParamsOpen);
  mApp->showHideWidget(mRightTolEdit, mParamsOpen);
  mApp->showHideWidget(mTileSizeEdit, mParamsOpen);
  mApp->showHideWidget(mAxisAngleEdit, mParamsOpen);
  QApplication::processEvents();
  QSize hint = sizeHint();
  QSize tabhint = mTable->sizeHint();
  int height = tabhint.height() - 6.5 * mTable->fontMetrics().height();
  height = hint.height() - B3DMAX(0, height);
  resize(mParamsOpen ? curWidth : (int)(0.85 * hint.width()), height);
}

void AngleDialog::applyClicked()
{
  anglesSet(0);
}

void AngleDialog::stepUpClicked()
{
  anglesSet(1);
}

void AngleDialog::stepDownClicked()
{
  anglesSet(-1);
}

void AngleDialog::fitSingleToggled(bool state)
{
  mApp->setFitSingleViews(state);
  mAutofitButton->setText(state ? tr("Autofit A&ll Single Views") :
                         tr("Autofit A&ll Steps"));
}


/*
 * This is called after Apply or general Enter on text fields, and by the step buttons
 * It fetches the angles and other parameters from the dialog and adjusts angles as needed
 */
void AngleDialog::anglesSet(int step)
{
  bool defOk;
  double defocus=mDefocusEdit->text().toDouble(&defOk);
  bool stepOk;
  double lowAngle, highAngle, rangeStep;
  double defTol, axisAngle, leftTol, rightTol;
  int tSize;

  bool anglesOk = getAnglesAndStep(step, lowAngle, highAngle, rangeStep, stepOk);

  bool tolOk = getTileTolerances(defTol, tSize, axisAngle, leftTol, rightTol);
  printf("lowAngle=%7.2f, highAngle=%7.2f\n", lowAngle, highAngle);
  fflush(stdout);
  if( defOk && anglesOk && tolOk && (!step || stepOk))
    emit angle(lowAngle, highAngle, defocus, 
        defTol, tSize, axisAngle, leftTol, rightTol);
  else {
    if (!defOk)
      QMessageBox::critical(NULL, "Ctfplotter: Bad character in entry", 
                            "There is a non-numeric character in the defocus entry");
    if (!anglesOk || (step && !stepOk))
      QMessageBox::critical(NULL, "Ctfplotter: Bad character in entry", "There is a "
                            "non-numeric character in one of the angle entries");
    if (!tolOk)
      QMessageBox::critical
        (NULL, "Ctfplotter: Bad character in entry",
         QString("There is a non-numeric character in one of the\n"
                 "tolerance entries or in the tilt axis angle entry.")
         + QString(mParamsOpen ? "" : "\n\nOpen Tile Parameters to see these entries"));
  }
}

/*
 * Get the starting and ending angle and range step from the text boxes; fix starting
 * and ending angles; step them both by doStep if nonzero (should be +1 or -1), return
 * false if either angle has bad characters; stepOk has the return value from getting step
 */
bool AngleDialog::getAnglesAndStep(int doStep, double &lowAngle, double &highAngle,
                                   double &rangeStep, bool &stepOk)
{
  float minAngle = mApp->getMinAngle();
  float maxAngle = mApp->getMaxAngle();
  QString str;
  bool lAngleOk;
  lowAngle=mLowAngleEdit->text().toDouble(&lAngleOk);
  bool hAngleOk;
  highAngle=mHighAngleEdit->text().toDouble(&hAngleOk);
  rangeStep = mRangeStepEdit->text().toDouble(&stepOk);

  // Check the angles and adjust them if necessary
  if (lAngleOk && hAngleOk && (doStep || lowAngle < minAngle || lowAngle > maxAngle ||
                               highAngle < minAngle || highAngle > maxAngle)) {
    B3DCLAMP(lowAngle, minAngle, maxAngle);
    B3DCLAMP(highAngle, minAngle, maxAngle);
    if (highAngle < lowAngle) {
      minAngle = lowAngle;
      lowAngle = highAngle;
      highAngle = minAngle;
    }
    if (doStep) {
      double diff = highAngle - lowAngle;
      highAngle += doStep * fabs(rangeStep);
      lowAngle += doStep * fabs(rangeStep);
      if (highAngle > maxAngle) {
        highAngle = maxAngle;
        lowAngle = maxAngle - diff;
      }
      if (lowAngle < minAngle) {
        lowAngle = minAngle;
        highAngle = minAngle + diff;
      }
      if (stepOk)
        mApp->setRangeStep(fabs(rangeStep));
    } 
    
    str.sprintf("%6.2f",lowAngle);
    mLowAngleEdit->setText(str);
    str.sprintf("%6.2f",highAngle);
    mHighAngleEdit->setText(str);
  }

  return lAngleOk && hAngleOk;
}

/*
 * Get the five tile parameters from the text boxes; return false if any have bad
 * characters
 */
bool AngleDialog::getTileTolerances(double &defTol, int &tSize, double &axisAngle,
                                   double &leftTol, double &rightTol)
{
  bool defTolOk;
  defTol=mDefTolEdit->text().toDouble(&defTolOk);
  bool tileSizeOk;
  tSize=mTileSizeEdit->text().toInt(&tileSizeOk);
  bool axisAngleOk;
  axisAngle=mAxisAngleEdit->text().toDouble(&axisAngleOk);
  bool leftTolOk;
  leftTol=mLeftTolEdit->text().toDouble(&leftTolOk);
  bool rightTolOk;
  rightTol=mRightTolEdit->text().toDouble(&rightTolOk);
  return defTolOk && tileSizeOk && axisAngleOk && leftTolOk && rightTolOk;
}

/*
 * Respond to autofit button being clicked: get the angles for the range, and the range
 * step, then get the from and to angles and correct them, then call autofit
 */
void AngleDialog::autofitClicked()
{
  int err;
  bool stepOk;
  double lowAngle, highAngle, rangeStep, rangeLow, rangeHigh;
  bool anglesOk = getAnglesAndStep(0, rangeLow, rangeHigh, rangeStep, stepOk);
  float minAngle = mApp->getMinAngle();
  float maxAngle = mApp->getMaxAngle();
  QString str;
  bool lAngleOk;
  lowAngle = mAutoFromEdit->text().toDouble(&lAngleOk);
  bool hAngleOk;
  highAngle = mAutoToEdit->text().toDouble(&hAngleOk);

  // Check the angles and adjust them if necessary
  if (lAngleOk && hAngleOk && (lowAngle < minAngle || lowAngle > maxAngle ||
                               highAngle < minAngle || highAngle > maxAngle || 
                               highAngle < lowAngle)) {
    B3DCLAMP(lowAngle, minAngle, maxAngle);
    B3DCLAMP(highAngle, minAngle, maxAngle);
    if (highAngle < lowAngle) {
      minAngle = lowAngle;
      lowAngle = highAngle;
      highAngle = minAngle;
    }
    
    str.sprintf("%.2f",lowAngle);
    mAutoFromEdit->setText(str);
    str.sprintf("%6.2f",highAngle);
    mAutoToEdit->setText(str);
  }

  if (lAngleOk && hAngleOk && anglesOk && stepOk) {
    err = mApp->autoFitToRanges((float)lowAngle, (float)highAngle,
                                 (float)(rangeHigh - rangeLow), (float)fabs(rangeStep),
                                 mApp->getDefocusOption() ? 3 : 1);
    if (err == 1)
      QMessageBox::critical
        (NULL, "Ctfplotter: Bad character in entry",
         QString("There is a non-numeric character in one of the\n"
                 "tolerance entries or in the tilt axis angle entry.")
         + QString(mParamsOpen ? "" : "\n\nOpen Tile Parameters to see these entries"));
    if (err == 2)
      QMessageBox::critical(NULL, "Ctfplotter: No autofit steps",
                             "Something is wrong with the entries for autofitting.\n"
                             "No tilt angle ranges could be defined for fitting over.");

  } else
    QMessageBox::critical(NULL, "Ctfplotter: Bad character in entry", "There is a "
                          "non-numeric character in one of the angle entries");
}

void AngleDialog::enableApplyButton(const QString &text)
{
  mApplyButton->setEnabled(!text.isEmpty());
  mAutofitButton->setEnabled(!text.isEmpty());
}

void AngleDialog::expDefocusChecked()
{
  emit defocusMethod(0);
}

void AngleDialog::currDefocusChecked()
{
  emit defocusMethod(1);
}

void AngleDialog::onlyCenterChecked()
{
  emit initialTileChoice(0);
}

void AngleDialog::allAtOnceChecked()
{
  emit initialTileChoice(1);
}

/*
 * Update all the entries in the table
 */
void AngleDialog::updateTable()
{
  Ilist *saved = mApp->getSavedList();
  SavedDefocus *item;
  QString str;  
  int row, j;
  for (row = 0; row < ilistSize(saved); row++) {
    
    // Add a row if needed
    if (row + 1 > mTable->rowCount()) {
      mTable->insertRow(row);
      mTable->setRowHeight(row, mTable->fontMetrics().height() + 3);
      for (j = 0; j < 4; j++) {
        QTableWidgetItem *witem = new QTableWidgetItem();
        witem->setTextAlignment(Qt::AlignRight);
        mTable->setItem(row, j, witem);
      }
    }
    
    // Load the data
    item = (SavedDefocus *)ilistItem(saved, row);
    str.sprintf("%.2f", item->lAngle);
    mTable->item(row,0)->setText(str);
    str.sprintf("%.2f", item->hAngle);
    mTable->item(row,1)->setText(str);
    str.sprintf("%.2f", (item->lAngle + item->hAngle) / 2.);
    mTable->item(row,2)->setText(str);
    str.sprintf("%.2f", item->defocus);
    mTable->item(row,3)->setText(str);
  }

  // Get rid of extra rows (hope it deletes the items)
  for (j = mTable->rowCount() - 1; j >= row; j--)
    mTable->removeRow(j);
  mDeleteButton->setEnabled(row > 0);
  mReturnButton->setEnabled(row > 0);
  mToFileButton->setEnabled(row > 0);
  if (mTable->currentRow() < 0 && mTable->rowCount())
    mTable->setCurrentCell(0, 0);
}

/* 
 * Delete Row clicked for table
 */
void AngleDialog::deleteClicked()
{
  int row = mTable->currentRow();
  Ilist *saved = mApp->getSavedList();
  if (row < 0 || row >= ilistSize(saved))
    return;
  ilistRemove(saved, row);
  updateTable();
  mApp->setSaveModified();
}

/* 
 * Set Angles clicked for table, set the angles from the current row
 */
void AngleDialog::setAnglesClicked()
{
  int row = mTable->currentRow();
  Ilist *saved = mApp->getSavedList();
  SavedDefocus *item;
  QString str;
  if (row < 0 || row >= ilistSize(saved))
    return;
  item = (SavedDefocus *)ilistItem(saved, row);
  str.sprintf("%.2f",item->lAngle);
  mLowAngleEdit->setText(str);
  str.sprintf("%.2f",item->hAngle);
  mHighAngleEdit->setText(str);
  qApp->processEvents();
  anglesSet(0);
}

void AngleDialog::rowDoubleClicked(int row, int column)
{
  setAnglesClicked();
}

void AngleDialog::closeEvent( QCloseEvent *e )
{
  mApp->mPlotter->mAngleDia = NULL;
  e->accept();
}

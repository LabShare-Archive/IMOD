/*
* fittingdialog.cpp - callbacks for the fitting range and parameters dialog.
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
#include <stdio.h>

#include "fittingdialog.h"
#include "myapp.h"
#define PRECISION 0.00005

FittingDialog::FittingDialog(MyApp *app, QWidget *parent) :QDialog(parent)
{
  mApp = app;
  float nDim=mApp->getDim()-1;
  float x1Idx1=mApp->getX1RangeLow()/nDim;
  float x1Idx2=mApp->getX1RangeHigh()/nDim;
  float x2Idx1=mApp->getX2RangeLow()/nDim;
  float x2Idx2=mApp->getX2RangeHigh()/nDim;
  char tmpStr[20];
  int which = mApp->getZeroFitMethod();

  setWindowTitle(tr("Fitting Range & Method"));

  // Set up the zero-fitting method radio group
  QGroupBox *zeroGrpBox = new QGroupBox(tr("Zero-finding method"), this);
  QButtonGroup *zeroButGroup = new QButtonGroup(this);
  QVBoxLayout *vbox=new QVBoxLayout;
  vbox->setSpacing(0);
  vbox->setContentsMargins(5, 2, 5, 5);
  QRadioButton *radio = new QRadioButton(tr("Fit to CTF-like curve"));
  zeroButGroup->addButton(radio, 0);
  vbox->addWidget(radio);
  radio->setToolTip("Fit to a function based on a CTF curve over the selected range");
  radio = new QRadioButton(tr("Fit polynomial to dip"));
  zeroButGroup->addButton(radio, 1);
  vbox->addWidget(radio);
  radio->setToolTip("Fit a polynomial over the selected range and take the minimum as "
                    "the zero");
  radio = new QRadioButton(tr("Find intersection of 2 curves"));
  zeroButGroup->addButton(radio, 2);
  vbox->addWidget(radio);
  radio->setToolTip("Fit curves or lines over two frequency ranges and take the zero as "
                    "their intersection");
  zeroGrpBox->setLayout(vbox);
  QAbstractButton *button = zeroButGroup->button(which);
  button->setChecked(true);

  // Checkbox for finding power and polynomial order spin button
  mPowerCheckBox = new QCheckBox(tr("Vary exponent of CTF function"), this);
  mPowerCheckBox->setChecked(mApp->getVaryCtfPowerInFit());
  mPowerCheckBox->setToolTip("Add a fifth parameter to the fit to vary the exponent of "
                             "the function being fit");

  QHBoxLayout *orderHbox = new QHBoxLayout;
  mOrderLabel = new QLabel(tr("Order of polynomial:"), this);
  orderHbox->addWidget(mOrderLabel);
  mOrderSpinBox = new QSpinBox(this);
  mOrderSpinBox->setRange(2, 6);
  mOrderSpinBox->setSingleStep(1);
  mOrderSpinBox->setValue(mApp->getPolynomialOrder());
  orderHbox->addWidget(mOrderSpinBox);
  mOrderSpinBox->setToolTip("Set the order for the polynomial fit, e.g., 3 for cubic");

  // FREQUENCY SCALE CHANGE: FOUR " / 2." AND %4.2f -> %4.3f

  // X1 start and end fields
  mX1_label_1=new QLabel(tr("X1 &Starts:"), this);
  sprintf(tmpStr, "%4.3f", x1Idx1 / 2.);
  mX1_edit_1=new QLineEdit(tmpStr, this);
  mX1_label_1->setBuddy(mX1_edit_1);
  mX1_edit_1->setToolTip("Starting frequency (in 1/pixel) of range for CTF-like, "
                         "polynomial, or first curve fit");

  //printf("??????????%f \n", x1Idx1);
  mX1_label_2=new QLabel(tr("X1 &Ends:"), this);
  sprintf(tmpStr, "%4.3f", x1Idx2 / 2.);
  mX1_edit_2=new QLineEdit(tmpStr, this);
  mX1_label_2->setBuddy(mX1_edit_1);
  mX1_edit_2->setToolTip("Ending frequency (in 1/pixel) of range for "
                         "first curve fit");

  // Set up the X1 fitting radio group
  mX1Group=new QGroupBox(tr("X1 fitting method"), this);
  QButtonGroup *x1ButGroup = new QButtonGroup(this);
  mX1LinearRadio=new QRadioButton(tr("Line"));
  mX1SimplexRadio=new QRadioButton(tr("Gaussian"));
  x1ButGroup->addButton(mX1LinearRadio, 0);
  x1ButGroup->addButton(mX1SimplexRadio, 1);
  mX1SimplexRadio->setChecked(true);

  vbox=new QVBoxLayout;
  vbox->addWidget(mX1LinearRadio);
  vbox->addWidget(mX1SimplexRadio);
  mX1Group->setLayout(vbox);
  vbox->setSpacing(0);
  vbox->setContentsMargins(5, 2, 5, 5);

  mX2_label_1=new QLabel(tr("X2 S&tarts:"), this);
  sprintf(tmpStr, "%4.3f", x2Idx1 / 2.);
  mX2_edit_1=new  QLineEdit(tmpStr, this);
  mX2_label_1->setBuddy(mX2_edit_1);
  mX2_edit_1->setToolTip("Starting frequency (in 1/pixel) of range for "
                         "second curve fit");

  mX2_label_2=new QLabel(tr("X2 E&nds:"), this);
  sprintf(tmpStr, "%4.3f", x2Idx2 / 2.);
  mX2_edit_2=new QLineEdit(tmpStr, this);
  mX2_label_2->setBuddy(mX2_edit_2);
  mX2_edit_2->setToolTip("Ending frequency (in 1/pixel) of range for CTF-like, "
                         "polynomial, or second curve fit");

  mX2Group=new QGroupBox(tr("X2 fitting method"), this);
  mX2LinearRadio=new QRadioButton(tr("Line"));
  mX2SimplexRadio=new QRadioButton(tr("Gaussian"));
  QButtonGroup *x2ButGroup = new QButtonGroup(this);
  x2ButGroup->addButton(mX2LinearRadio, 0);
  x2ButGroup->addButton(mX2SimplexRadio, 1);
  mX2SimplexRadio->setChecked(true);

  QVBoxLayout *vbox2=new QVBoxLayout;
  vbox2->addWidget(mX2LinearRadio);
  vbox2->addWidget(mX2SimplexRadio);
  mX2Group->setLayout(vbox2);
  vbox2->setSpacing(0);
  vbox2->setContentsMargins(5, 2, 5, 5);


  mApplyButton= new QPushButton( tr("&Apply"), this);
  mApplyButton->setDefault(true);
  mApplyButton->setEnabled(false);

  mCloseButton=new QPushButton( tr("&Close"), this);

  connect(mX1_edit_1, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mX1_edit_2, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mX2_edit_1, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(mX2_edit_2, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );

  connect(mApplyButton, SIGNAL(clicked()), this, SLOT(rangeSetted()) );
  connect(mCloseButton, SIGNAL(clicked()), this, SLOT(close()) );
  connect(mX1LinearRadio, SIGNAL(clicked()), this, SLOT(x1LinearChecked()) );
  connect(mX1SimplexRadio, SIGNAL(clicked()), this, SLOT(x1SimplexChecked()) );
  connect(mX2LinearRadio, SIGNAL(clicked()), this, SLOT(x2LinearChecked()) );
  connect(mX2SimplexRadio, SIGNAL(clicked()), this, SLOT(x2SimplexChecked()) );
  connect(zeroButGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(zeroMethodClicked(int)));
  connect(mPowerCheckBox, SIGNAL(clicked(bool)), this, 
          SLOT(fitPowerClicked(bool)));
  connect(mOrderSpinBox, SIGNAL(valueChanged(int)), this,
          SLOT(orderChanged(int)));

  QHBoxLayout *x1HLayout_1=new QHBoxLayout;
  x1HLayout_1->addWidget(mX1_label_1);
  x1HLayout_1->addWidget(mX1_edit_1);

  QHBoxLayout *x1HLayout_2=new QHBoxLayout;
  x1HLayout_2->addWidget(mX1_label_2);
  x1HLayout_2->addWidget(mX1_edit_2);

  QVBoxLayout *x1VLayout=new QVBoxLayout;
  x1VLayout->addLayout(x1HLayout_1);
  x1VLayout->addLayout(x1HLayout_2);

  QHBoxLayout *x1HLayout_3= new QHBoxLayout;
  x1HLayout_3->addLayout(x1VLayout);
  x1HLayout_3->addWidget(mX1Group);

  QHBoxLayout *x2HLayout_1=new QHBoxLayout;
  x2HLayout_1->addWidget(mX2_label_1);
  x2HLayout_1->addWidget(mX2_edit_1);

  QHBoxLayout *x2HLayout_2=new QHBoxLayout;
  x2HLayout_2->addWidget(mX2_label_2);
  x2HLayout_2->addWidget(mX2_edit_2);

  QVBoxLayout *x2VLayout=new QVBoxLayout;
  x2VLayout->addLayout(x2HLayout_1);
  x2VLayout->addLayout(x2HLayout_2);

  QHBoxLayout *x2HLayout_3= new QHBoxLayout;
  x2HLayout_3->addLayout(x2VLayout);
  x2HLayout_3->addWidget(mX2Group);


  QHBoxLayout *lowHLayout=new QHBoxLayout;
  lowHLayout->addWidget(mApplyButton);
  lowHLayout->addWidget(mCloseButton);

  QVBoxLayout *mainLayout=new QVBoxLayout(this);
  mainLayout->setMargin(11);
  mainLayout->setSpacing(6);
  mainLayout->addWidget(zeroGrpBox);
  mainLayout->addWidget(mPowerCheckBox);
  mainLayout->addLayout(orderHbox);
  mainLayout->addLayout(x1HLayout_3);
  mainLayout->addLayout(x2HLayout_3);
  mainLayout->addLayout(lowHLayout);
  manageWidgets(which);
}

void FittingDialog::rangeSetted()
{
  // FREQUENCY SCALE CHANGE: FOUR "2. * "
  bool x1_ok_1;
  double x1_1 = 2. * mX1_edit_1->text().toDouble(&x1_ok_1)+PRECISION;
  bool x1_ok_2;
  double x1_2 = 2. * mX1_edit_2->text().toDouble(&x1_ok_2)+PRECISION;

  bool x2_ok_1;
  double x2_1 = 2. * mX2_edit_1->text().toDouble(&x2_ok_1)+PRECISION;
  bool x2_ok_2;
  double x2_2 = 2.* mX2_edit_2->text().toDouble(&x2_ok_2)+PRECISION;
  int which = mApp->getZeroFitMethod();

  if( x1_ok_1 && x1_ok_2 && x1_1>=0.0 && x1_2>0.0 && x1_2<=1.0+PRECISION && 
      x2_ok_1 && x2_ok_2 && x2_1>=0.0 && x2_2>0.0 && 
      x2_2<=1.0+PRECISION && ((x2_1<x2_2 && x1_1<x1_2 && which == 2) ||
                              (x1_1<x2_2 && which != 2)))
    emit range(x1_1, x1_2, x2_1, x2_2); 
  else
    printf("Invalid range for x2 or x1 \n");
  mApplyButton->setEnabled(false);
}


void FittingDialog::enableApplyButton(const QString &text)
{
  mApplyButton->setEnabled(!text.isEmpty() );
}

// DNM 7/13/09: got rid of enableApplyButtonX1,2 because the emitted signal
// got there first and changed the selected method, so need to take care of
// the enables here

void FittingDialog::x1LinearChecked()
{
  if (mApp->getX1Method() != 0)
    mApplyButton->setEnabled(true);
  emit x1MethodChosen(0);
}

void FittingDialog::x1SimplexChecked()
{
  if (mApp->getX1Method() != 1)
    mApplyButton->setEnabled(true);
  emit x1MethodChosen(1);
}

void FittingDialog::x2LinearChecked()
{
  if (mApp->getX2Method() != 0)
    mApplyButton->setEnabled(true);
  emit x2MethodChosen(0);
}

void FittingDialog::x2SimplexChecked()
{
  if (mApp->getX2Method() != 1)
    mApplyButton->setEnabled(true);
  emit x2MethodChosen(1);
}

void FittingDialog::FittingDialog::zeroMethodClicked(int which)
{
  mApplyButton->setEnabled(true);
  manageWidgets(which);
  mApp->setZeroFitMethod(which);
}

// Refit automatically for simple changes like exponent and power
void FittingDialog::fitPowerClicked(bool state)
{
  mApp->setVaryCtfPowerInFit(state);
  rangeSetted();
}

void FittingDialog::orderChanged(int value)
{
  mApp->setPolynomialOrder(value);
  rangeSetted();
}

void FittingDialog::manageWidgets(int which)
{
  mApp->showHideWidget(mPowerCheckBox, which == 0);
  mApp->showHideWidget(mOrderLabel, which == 1);
  mApp->showHideWidget(mOrderSpinBox, which == 1);
  mApp->showHideWidget(mX1_label_2, which == 2);
  mApp->showHideWidget(mX2_label_1, which == 2);
  mApp->showHideWidget(mX1_edit_2, which == 2);
  mApp->showHideWidget(mX2_edit_1, which == 2);
  mApp->showHideWidget(mX1Group, which == 2);
  mApp->showHideWidget(mX1LinearRadio, which == 2);
  mApp->showHideWidget(mX1LinearRadio, which == 2);
  mApp->showHideWidget(mX2Group, which == 2);
  mApp->showHideWidget(mX2LinearRadio, which == 2);
  mApp->showHideWidget(mX2LinearRadio, which == 2);
  QApplication::processEvents();
  adjustSize();
}

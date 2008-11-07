/*
* angledialog.cpp - callbacks for the angle range dialog.
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
*
*  Log at end of file
*/
#include <qlabel.h>
#include <qlayout.h>
#include <qlineedit.h>
#include <qpushbutton.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qgroupbox.h>
#include <qerrormessage.h>


#include "angledialog.h"
#include "myapp.h"
#include "b3dutil.h"

AngleDialog::AngleDialog(QWidget *parent, const char *name):
  QDialog(parent,name)
{
  setCaption(tr("Set angle range and defocus"));
  defocusLabel=new QLabel(tr("Expected defocus (um): "), this);
  defocusEdit=new QLineEdit("6.0", this);
  defocusLabel->setBuddy(defocusEdit);

  defTolLabel=new QLabel(tr("Center defocus tol (nm): "), this);
  defTolEdit=new QLineEdit(tr("200"), this);
  defTolLabel->setBuddy(defTolEdit);

  leftTolLabel=new QLabel(tr("Left defocus tol (nm): "), this);
  leftTolEdit=new QLineEdit(tr("200"), this);
  leftTolLabel->setBuddy(leftTolEdit);

  rightTolLabel=new QLabel(tr("Right defocus tol (nm)"), this);
  rightTolEdit=new QLineEdit(tr("200"), this);
  rightTolLabel->setBuddy(rightTolEdit);

  lowAngleLabel=new QLabel(tr("Tilt angle starts: "), this);
  lowAngleEdit=new QLineEdit(tr("-90.0"), this);
  lowAngleLabel->setBuddy(lowAngleEdit);

  highAngleLabel=new QLabel(tr("Tilt angle ends:  "), this);
  highAngleEdit=new QLineEdit(tr("90.0"), this);
  highAngleLabel->setBuddy(highAngleEdit);

  tileSizeLabel=new QLabel(tr("Tile size:           "), this);
  tileSizeEdit=new QLineEdit(tr("256"),this);
  tileSizeLabel->setBuddy(tileSizeEdit);

  axisAngleLabel=new QLabel(tr("Tilt axis angle:   "), this);
  axisAngleEdit=new QLineEdit( tr("0.0"), this);
  axisAngleLabel->setBuddy(axisAngleEdit);

  defocusGroup=new QButtonGroup(1, QGroupBox::Horizontal, 
      tr("Which defocus to use"), this);
  expDefocusRadio=new QRadioButton(tr("Expected defocus"), defocusGroup);
  currDefocusRadio=new QRadioButton(tr("Current defocus estimate"), 
      defocusGroup);
  defocusGroup->setButton(0);

  ifAllGroup=new QButtonGroup(1, QGroupBox::Horizontal, 
      tr("Initial tiles to include"), this);
  onlyCenterRadio=new QRadioButton(tr("Only central tiles"), ifAllGroup); 
  allAtOnceRadio=new QRadioButton(tr("All tiles"), ifAllGroup);
  ifAllGroup->setButton(0);

  saveButton=new QPushButton( tr("&Store Current Defocus"), this);
  saveButton->setEnabled(true);

  applyButton=new QPushButton( tr("&Apply"), this);
  applyButton->setDefault(true);
  applyButton->setEnabled(false);
 
  closeButton=new QPushButton( tr("&Close"), this);

  connect(defocusEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(lowAngleEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(lowAngleEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(defTolEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(tileSizeEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(axisAngleEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(leftTolEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(rightTolEdit, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );

  connect(expDefocusRadio, SIGNAL(clicked()), this, SLOT(expDefocusChecked()) );
  connect(currDefocusRadio, SIGNAL(clicked()),this, SLOT(currDefocusChecked()));
  connect(onlyCenterRadio, SIGNAL(clicked()), this, SLOT(onlyCenterChecked()) );
  connect(allAtOnceRadio, SIGNAL(clicked()), this, SLOT(allAtOnceChecked()) );
  
  connect(applyButton, SIGNAL(clicked()), this, SLOT(angleSetted()) );
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()) );
  connect(saveButton, SIGNAL(clicked()), this, SLOT(saveCurrentDefocus()) );

  QHBoxLayout *defHLayout=new QHBoxLayout;
  defHLayout->addWidget(defocusLabel);
  defHLayout->addWidget(defocusEdit);
  
  QHBoxLayout *defTolHLayout=new QHBoxLayout;
  defTolHLayout->addWidget(defTolLabel);
  defTolHLayout->addWidget(defTolEdit);
  
  QHBoxLayout *leftTolHLayout=new QHBoxLayout;
  leftTolHLayout->addWidget(leftTolLabel);
  leftTolHLayout->addWidget(leftTolEdit);
  
  QHBoxLayout *rightTolHLayout=new QHBoxLayout;
  rightTolHLayout->addWidget(rightTolLabel);
  rightTolHLayout->addWidget(rightTolEdit);

  QHBoxLayout *lAngleHLayout=new QHBoxLayout;
  lAngleHLayout->addWidget(lowAngleLabel);
  lAngleHLayout->addWidget(lowAngleEdit);

  QHBoxLayout *hAngleHLayout=new QHBoxLayout;
  hAngleHLayout->addWidget(highAngleLabel);
  hAngleHLayout->addWidget(highAngleEdit);

  QHBoxLayout *tileSizeHLayout=new QHBoxLayout;
  tileSizeHLayout->addWidget(tileSizeLabel);
  tileSizeHLayout->addWidget(tileSizeEdit);

  QHBoxLayout *axisAngleHLayout=new QHBoxLayout;
  axisAngleHLayout->addWidget(axisAngleLabel);
  axisAngleHLayout->addWidget(axisAngleEdit);

  QHBoxLayout *bottomHLayout=new QHBoxLayout;
  bottomHLayout->addWidget(applyButton);
  bottomHLayout->addWidget(closeButton);

  QVBoxLayout *leftVLayout=new QVBoxLayout;
  leftVLayout->addLayout(defHLayout);
  leftVLayout->addLayout(defTolHLayout);
  leftVLayout->addLayout(leftTolHLayout);
  leftVLayout->addLayout(rightTolHLayout);
  leftVLayout->addLayout(lAngleHLayout);
  leftVLayout->addLayout(hAngleHLayout);
  leftVLayout->addLayout(tileSizeHLayout);
  leftVLayout->addLayout(axisAngleHLayout);

  QVBoxLayout *rightVLayout=new QVBoxLayout;
  rightVLayout->addWidget(defocusGroup);
  rightVLayout->addWidget(ifAllGroup);
  rightVLayout->addWidget(saveButton);

  QHBoxLayout *topHLayout=new QHBoxLayout;
  topHLayout->addLayout(leftVLayout);
  topHLayout->addLayout(rightVLayout);
  
  QVBoxLayout *mainVLayout=new QVBoxLayout(this);
  mainVLayout->addLayout(topHLayout);
  mainVLayout->addLayout(bottomHLayout);
}

void AngleDialog::angleSetted()
{
  bool defOk;
  double defocus=defocusEdit->text().toDouble(&defOk);
  bool lAngleOk;
  double lowAngle=lowAngleEdit->text().toDouble(&lAngleOk);
  bool hAngleOk;
  double highAngle=highAngleEdit->text().toDouble(&hAngleOk);
  bool defTolOk;
  double defTol=defTolEdit->text().toDouble(&defTolOk);
  bool tileSizeOk;
  int tSize=tileSizeEdit->text().toInt(&tileSizeOk);
  bool axisAngleOk;
  double axisAngle=axisAngleEdit->text().toDouble(&axisAngleOk);
  bool leftTolOk;
  double leftTol=leftTolEdit->text().toDouble(&leftTolOk);
  bool rightTolOk;
  double rightTol=rightTolEdit->text().toDouble(&rightTolOk);

  printf("lowAngle=%7.2f, highAngle=%7.2f\n", lowAngle, highAngle);

  if( defOk && lAngleOk && hAngleOk && defTolOk && tileSizeOk && axisAngleOk &&
      leftTolOk && rightTolOk)
    emit angle(lowAngle, highAngle, defocus, 
        defTol, tSize, axisAngle, leftTol, rightTol);
  else
    printf("Invalid defocus or angles\n");
}

void AngleDialog::saveCurrentDefocus(){
   char *defFn=((MyApp *)qApp)->getDefFn();
   FILE *fp;
   
   FILE *saveFp;
   saveFp=((MyApp *)qApp)->getSaveFp();
    if(!saveFp){
      imodBackupFile(defFn);
      fp=fopen(defFn,"w");
      ((MyApp *)qApp)->setSaveFp(fp);
    }else fp=fopen(defFn, "a");

   if(!fp){
      QErrorMessage* errorMessage = new QErrorMessage( this );
      errorMessage->message( "Can not open output file" );
      return;
   }

   int startingSlice=((MyApp *)qApp)->getStartingSliceNum();
   int endingSlice=((MyApp *)qApp)->getEndingSliceNum();
   double lAngle=((MyApp *)qApp)->getLowAngle();
   double hAngle=((MyApp *)qApp)->getHighAngle();
   double defocus=((MyApp *)qApp)->defocusFinder.getDefocus();
   fprintf(fp, "%d\t%d\t%5.2f\t%5.2f\t%6.0f\n", startingSlice, endingSlice,
       lAngle, hAngle, defocus*1000);
   fclose(fp); //flush output;
}

void AngleDialog::enableApplyButton(const QString &text)
{
  applyButton->setEnabled(!text.isEmpty());
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


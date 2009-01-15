/*
* rangedialog.cpp - callbacks for the fitting range dialog.
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*  Log at end of file
*/

#include <QtGui>

#include "rangedialog.h"
#include "myapp.h"
#define PRECISION 0.00005

  RangeDialog::RangeDialog(QWidget *parent) :QDialog(parent)
{
  float nDim=((MyApp *)qApp)->getDim()-1;
  float x1Idx1=((MyApp *)qApp)->getX1RangeLow()/nDim;
  float x1Idx2=((MyApp *)qApp)->getX1RangeHigh()/nDim;
  float x2Idx1=((MyApp *)qApp)->getX2RangeLow()/nDim;
  float x2Idx2=((MyApp *)qApp)->getX2RangeHigh()/nDim;
  char tmpStr[20];

  setWindowTitle(tr("Set X Range"));
  x1_label_1=new QLabel(tr("X1 &Starts:"), this);
  sprintf(tmpStr, "%4.2f", x1Idx1);
  x1_edit_1=new QLineEdit(tmpStr, this);
  x1_label_1->setBuddy(x1_edit_1);

  //printf("??????????%f \n", x1Idx1);
  x1_label_2=new QLabel(tr("X1 &Ends:"), this);
  sprintf(tmpStr, "%4.2f", x1Idx2);
  x1_edit_2=new QLineEdit(tmpStr, this);
  x1_label_2->setBuddy(x1_edit_1);

  x1Group=new QGroupBox(tr("X1 fitting method"), this);
  x1LinearRadio=new QRadioButton(tr("Line"));
  x1SimplexRadio=new QRadioButton(tr("Gaussian"));
  x1SimplexRadio->setChecked(true);

  QVBoxLayout *vbox=new QVBoxLayout;
  vbox->addWidget(x1LinearRadio);
  vbox->addWidget(x1SimplexRadio);
  x1Group->setLayout(vbox);

  x2_label_1=new QLabel(tr("X2 &Starts:"), this);
  sprintf(tmpStr, "%4.2f", x2Idx1);
  x2_edit_1=new  QLineEdit(tmpStr, this);
  x2_label_1->setBuddy(x2_edit_1);

  x2_label_2=new QLabel(tr("X2 &Ends:"), this);
  sprintf(tmpStr, "%4.2f", x2Idx2);
  x2_edit_2=new QLineEdit(tmpStr, this);
  x2_label_2->setBuddy(x2_edit_2);

  x2Group=new QGroupBox(tr("X2 fitting method"), this);
  x2LinearRadio=new QRadioButton(tr("Line"));
  x2SimplexRadio=new QRadioButton(tr("Gaussian"));
  x2SimplexRadio->setChecked(true);

  QVBoxLayout *vbox2=new QVBoxLayout;
  vbox2->addWidget(x2LinearRadio);
  vbox2->addWidget(x2SimplexRadio);
  x2Group->setLayout(vbox2);


  applyButton= new QPushButton( tr("&Apply"), this);
  applyButton->setDefault(true);
  applyButton->setEnabled(false);

  closeButton=new QPushButton( tr("&Close"), this);

  connect(x1_edit_1, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(x1_edit_2, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(x2_edit_1, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(x2_edit_2, SIGNAL(textChanged(const QString &)), this,
      SLOT(enableApplyButton(const QString &)) );
  connect(x1Group, SIGNAL(clicked(int )), this,
      SLOT(enableApplyButtonX1(int)) );
  connect(x2Group, SIGNAL(clicked(int )), this,
      SLOT(enableApplyButtonX2(int )) );

  connect(applyButton, SIGNAL(clicked()), this, SLOT(rangeSetted()) );
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()) );
  connect(x1LinearRadio, SIGNAL(clicked()), this, SLOT(x1LinearChecked()) );
  connect(x1SimplexRadio, SIGNAL(clicked()), this, SLOT(x1SimplexChecked()) );
  connect(x2LinearRadio, SIGNAL(clicked()), this, SLOT(x2LinearChecked()) );
  connect(x2SimplexRadio, SIGNAL(clicked()), this, SLOT(x2SimplexChecked()) );

  QHBoxLayout *x1HLayout_1=new QHBoxLayout;
  x1HLayout_1->addWidget(x1_label_1);
  x1HLayout_1->addWidget(x1_edit_1);

  QHBoxLayout *x1HLayout_2=new QHBoxLayout;
  x1HLayout_2->addWidget(x1_label_2);
  x1HLayout_2->addWidget(x1_edit_2);

  QVBoxLayout *x1VLayout=new QVBoxLayout;
  x1VLayout->addLayout(x1HLayout_1);
  x1VLayout->addLayout(x1HLayout_2);

  QHBoxLayout *x1HLayout_3= new QHBoxLayout;
  x1HLayout_3->addLayout(x1VLayout);
  x1HLayout_3->addWidget(x1Group);

  QHBoxLayout *x2HLayout_1=new QHBoxLayout;
  x2HLayout_1->addWidget(x2_label_1);
  x2HLayout_1->addWidget(x2_edit_1);

  QHBoxLayout *x2HLayout_2=new QHBoxLayout;
  x2HLayout_2->addWidget(x2_label_2);
  x2HLayout_2->addWidget(x2_edit_2);

  QVBoxLayout *x2VLayout=new QVBoxLayout;
  x2VLayout->addLayout(x2HLayout_1);
  x2VLayout->addLayout(x2HLayout_2);

  QHBoxLayout *x2HLayout_3= new QHBoxLayout;
  x2HLayout_3->addLayout(x2VLayout);
  x2HLayout_3->addWidget(x2Group);


  QHBoxLayout *lowHLayout=new QHBoxLayout;
  lowHLayout->addWidget(applyButton);
  lowHLayout->addWidget(closeButton);

  QVBoxLayout *mainLayout=new QVBoxLayout(this);
  mainLayout->setMargin(11);
  mainLayout->setSpacing(6);
  mainLayout->addLayout(x1HLayout_3);
  mainLayout->addLayout(x2HLayout_3);
  mainLayout->addLayout(lowHLayout);
}

void RangeDialog::rangeSetted()
{
  bool x1_ok_1;
  double x1_1=x1_edit_1->text().toDouble(&x1_ok_1)+PRECISION;
  bool x1_ok_2;
  double x1_2=x1_edit_2->text().toDouble(&x1_ok_2)+PRECISION;

  bool x2_ok_1;
  double x2_1=x2_edit_1->text().toDouble(&x2_ok_1)+PRECISION;
  bool x2_ok_2;
  double x2_2=x2_edit_2->text().toDouble(&x2_ok_2)+PRECISION;

  if( x1_ok_1 && x1_ok_2 && x1_1>=0.0 && x1_2>0.0 && x1_2<=1.0+PRECISION && 
      x1_1<x1_2 && x2_ok_1 && x2_ok_2 && x2_1>=0.0 && x2_2>0.0 && 
      x2_2<=1.0+PRECISION && x2_1<x2_2 )
    emit range(x1_1, x1_2, x2_1, x2_2); 
  else
    printf("Invalid range for x2 or x1 \n");
  applyButton->setEnabled(false);
}


void RangeDialog::enableApplyButton(const QString &text)
{
  applyButton->setEnabled(!text.isEmpty() );
}

void RangeDialog::enableApplyButtonX1(int id)
{
  if( id!=((MyApp *)qApp)->getX1Method() )
    applyButton->setEnabled(true);
}

void RangeDialog::enableApplyButtonX2(int id)
{
  if( id!=((MyApp *)qApp)->getX2Method() )
    applyButton->setEnabled(true);
}


void RangeDialog::x1LinearChecked()
{
  emit x1MethodChosen(0);
}

void RangeDialog::x1SimplexChecked()
{
  emit x1MethodChosen(1);
}

void RangeDialog::x2LinearChecked()
{
  emit x2MethodChosen(0);
}

void RangeDialog::x2SimplexChecked()
{
  emit x2MethodChosen(1);
}

/*

   $Log$
   Revision 1.3  2008/11/07 17:26:24  xiongq
   add the copyright heading

*/

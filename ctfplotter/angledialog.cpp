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
*  Log at end of file
*/
#include <QtGui>

#include "angledialog.h"
#include "plotter.h"
#include "myapp.h"
#include "b3dutil.h"
#include "ilist.h"
#include "parse_params.h" //for exitError()

AngleDialog::AngleDialog(QWidget *parent): QDialog(parent)
{
  int width, i;
  mApp = (MyApp *)qApp;
  setWindowTitle(tr("Angle Range & Tile Selection"));
  defocusLabel=new QLabel(tr("Expected defocus (um): "), this);
  defocusEdit=new QLineEdit("6.0", this);
  defocusLabel->setBuddy(defocusEdit);
  defocusEdit->setMinimumWidth(fontMetrics().width(" 99.99"));
  defocusEdit->setToolTip("Nominal defocus value in microns");

  lowAngleLabel=new QLabel(tr("Starting tilt angle: "), this);
  lowAngleEdit=new QLineEdit(tr("-90.0"), this);
  lowAngleLabel->setBuddy(lowAngleEdit);
  lowAngleEdit->setToolTip("Tilt angle at negative end of range to include in"
                           " fit");

  highAngleLabel=new QLabel(tr("Ending tilt angle:  "), this);
  highAngleEdit=new QLineEdit(tr("90.0"), this);
  highAngleLabel->setBuddy(highAngleEdit);
  highAngleEdit->setToolTip("Tilt angle at positive end of range to include in"
                           " fit");

  defTolLabel=new QLabel(tr("Center defocus tol (nm): "), this);
  defTolEdit=new QLineEdit(tr("200"), this);
  defTolLabel->setBuddy(defTolEdit);
  defTolEdit->setMinimumWidth(fontMetrics().width(" 9999"));
  defTolEdit->setToolTip("Maximum defocus difference for central tiles");

  leftTolLabel=new QLabel(tr("Left defocus tol (nm): "), this);
  leftTolEdit=new QLineEdit(tr("200"), this);
  leftTolLabel->setBuddy(leftTolEdit);
  leftTolEdit->setToolTip("Maximum defocus difference for non-central tiles "
                          "on left");

  rightTolLabel=new QLabel(tr("Right defocus tol (nm)"), this);
  rightTolEdit=new QLineEdit(tr("200"), this);
  rightTolLabel->setBuddy(rightTolEdit);
  rightTolEdit->setToolTip("Maximum defocus difference for non-central tiles "
                          "on right");

  tileSizeLabel=new QLabel(tr("Tile size:           "), this);
  tileSizeEdit=new QLineEdit(tr("256"),this);
  tileSizeLabel->setBuddy(tileSizeEdit);
  tileSizeEdit->setToolTip("Size of square, overlapping tiles to analyze");

  axisAngleLabel=new QLabel(tr("Tilt axis angle:   "), this);
  axisAngleEdit=new QLineEdit( tr("0.0"), this);
  axisAngleLabel->setBuddy(axisAngleEdit);
  axisAngleEdit->setToolTip("Angle of tilt axis from vertical");

  defocusGroup=new QGroupBox(tr("Which defocus to use"), this);
  QButtonGroup *butGroup = new QButtonGroup(this);
  expDefocusRadio=new QRadioButton(tr("Expected defocus"));
  expDefocusRadio->setToolTip("Use expected defocus for shifting spectra "
                              "from off-center tiles");
  currDefocusRadio=new QRadioButton(tr("Current defocus estimate"));
  currDefocusRadio->setToolTip("Use current defocus estimate for shifting "
                               "spectra from off-center tiles");
  expDefocusRadio->setChecked(true);
  butGroup->addButton(expDefocusRadio);
  butGroup->addButton(currDefocusRadio);

  QVBoxLayout *vbox=new QVBoxLayout;
  vbox->addWidget(expDefocusRadio);
  vbox->addWidget(currDefocusRadio);
  defocusGroup->setLayout(vbox);
  

  ifAllGroup=new QGroupBox(tr("Initial tiles to include"), this);
  butGroup = new QButtonGroup(this);
  onlyCenterRadio=new QRadioButton(tr("Only central tiles")); 
  allAtOnceRadio=new QRadioButton(tr("All tiles"));
  onlyCenterRadio->setToolTip("Compute curve initially only from central "
                              "tiles");
  allAtOnceRadio->setToolTip("Compute curve using all available tiles");
  onlyCenterRadio->setChecked(true);
  butGroup->addButton(allAtOnceRadio);
  butGroup->addButton(onlyCenterRadio);

  QVBoxLayout *vbox2=new QVBoxLayout;
  vbox2->addWidget(onlyCenterRadio);
  vbox2->addWidget(allAtOnceRadio);
  ifAllGroup->setLayout(vbox2);

  saveButton=new QPushButton( tr("&Store Defocus in Table"), this);
  saveButton->setEnabled(true);
  saveButton->setToolTip("Add current defocus value to table below");

  applyButton=new QPushButton( tr("&Apply"), this);
  applyButton->setDefault(true);
  applyButton->setEnabled(false);
  applyButton->setToolTip("Compute power spectra with current settings");
 
  closeButton=new QPushButton( tr("&Close"), this);
  closeButton->setFixedWidth(3 * fontMetrics().width("Close"));
  closeButton->setToolTip("Close this dialog");

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
  mReturnButton = new QPushButton( tr("Set Tilt Angles"), this);
  mToFileButton = new QPushButton( tr("Save to File"), this);
  mDeleteButton->setToolTip("Remove this row from the table");
  mReturnButton->setToolTip("Set starting and ending angles from this row and "
                            "recompute spectrum");
  mToFileButton->setToolTip("Save angle and defocus values in table to file");

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

  connect(expDefocusRadio, SIGNAL(clicked()), this, SLOT(expDefocusChecked()));
  connect(currDefocusRadio, SIGNAL(clicked()),this, SLOT(currDefocusChecked()));
  connect(onlyCenterRadio, SIGNAL(clicked()), this, SLOT(onlyCenterChecked()));
  connect(allAtOnceRadio, SIGNAL(clicked()), this, SLOT(allAtOnceChecked()) );
  
  connect(applyButton, SIGNAL(clicked()), this, SLOT(anglesSet()) );
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()) );
  connect(saveButton, SIGNAL(clicked()), mApp, SLOT(saveCurrentDefocus()) );
  connect(mDeleteButton, SIGNAL(clicked()), this, SLOT(deleteClicked()) );
  connect(mReturnButton, SIGNAL(clicked()), this, SLOT(setAnglesClicked()) );
  connect(mToFileButton, SIGNAL(clicked()), mApp, SLOT(writeDefocusFile()) );

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
  bottomHLayout->addWidget(closeButton);

  QHBoxLayout *tabHLayout=new QHBoxLayout;
  tabHLayout->addWidget(mDeleteButton);
  tabHLayout->addWidget(mReturnButton);
  tabHLayout->addWidget(mToFileButton);

  QVBoxLayout *leftVLayout=new QVBoxLayout;
  leftVLayout->addLayout(defHLayout);
  leftVLayout->addLayout(lAngleHLayout);
  leftVLayout->addLayout(hAngleHLayout);
  leftVLayout->addLayout(defTolHLayout);
  leftVLayout->addLayout(leftTolHLayout);
  leftVLayout->addLayout(rightTolHLayout);
  leftVLayout->addLayout(tileSizeHLayout);
  leftVLayout->addLayout(axisAngleHLayout);

  QVBoxLayout *rightVLayout=new QVBoxLayout;
  rightVLayout->addWidget(defocusGroup);
  rightVLayout->addWidget(ifAllGroup);
  rightVLayout->addWidget(applyButton);
  rightVLayout->addWidget(saveButton);

  QHBoxLayout *topHLayout=new QHBoxLayout;
  topHLayout->addLayout(leftVLayout);
  topHLayout->addLayout(rightVLayout);
  
  QVBoxLayout *mainVLayout=new QVBoxLayout(this);
  mainVLayout->addLayout(topHLayout);
  mainVLayout->addWidget(mTable);
  mainVLayout->addLayout(tabHLayout);
  mainVLayout->addLayout(bottomHLayout);

  // resize to eliminate stretched width and allow 4 lines in table
  QSize hint = sizeHint();
  QSize tabhint = mTable->sizeHint();
  width = tabhint.height() - 6.5 * mTable->fontMetrics().height();
  width = hint.height() - B3DMAX(0, width);
  resize((int)(0.85 * hint.width()), width);
}

void AngleDialog::anglesSet()
{
  float minAngle = mApp->getMinAngle();
  float maxAngle = mApp->getMaxAngle();
  QString str;
  bool defOk;
  double defocus=defocusEdit->text().toDouble(&defOk);
  bool lAngleOk;
  double lowAngle=lowAngleEdit->text().toDouble(&lAngleOk);
  bool hAngleOk;
  double highAngle=highAngleEdit->text().toDouble(&hAngleOk);

  // Check the angles and adjust them if necessary
  if (lAngleOk && hAngleOk && (lowAngle < minAngle || lowAngle > maxAngle ||
                               highAngle < minAngle || highAngle > maxAngle)) {
    lowAngle = B3DMIN(maxAngle, B3DMAX(minAngle, lowAngle));
    highAngle = B3DMIN(maxAngle, B3DMAX(minAngle, highAngle));
    if (highAngle < lowAngle) {
      minAngle = lowAngle;
      lowAngle = highAngle;
      highAngle = minAngle;
    }
    str.sprintf("%.2f",lowAngle);
    lowAngleEdit->setText(str);
    str.sprintf("%.2f",highAngle);
    highAngleEdit->setText(str);
  }
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
  lowAngleEdit->setText(str);
  str.sprintf("%.2f",item->hAngle);
  highAngleEdit->setText(str);
  qApp->processEvents();
  anglesSet();
}

void AngleDialog::closeEvent( QCloseEvent *e )
{
  mApp->mPlotter->aDialog = NULL;
  e->accept();
}

/*

   $Log$
   Revision 1.9  2010/03/14 19:14:17  mast
   Changes for adding table, using read-in tilt angles, etc

   Revision 1.8  2009/08/10 22:11:07  mast
   Reorganized to put important things at the top

   Revision 1.7  2009/01/15 16:31:36  mast
   Qt 4 port

   Revision 1.6  2008/11/07 17:04:27  xiongq
   add the copyright heading

*/

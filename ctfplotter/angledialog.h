/*
 * angledialog.h - Header for AngleDialog class
 *
 *  $Id$
 *
 */
#ifndef ANGLEDIALOG_H
#define ANGLEDIALOG_H

#include<QDialog>
class QLabel;
class QLineEdit;
class QPushButton;
class QGroupBox;
class QRadioButton;
class QTableWidget;
class QCheckBox;
class MyApp;

class AngleDialog :public QDialog
{
  Q_OBJECT
  public:
  AngleDialog(MyApp *app, QWidget *parent=0);
    void updateTable();
    bool getTileTolerances(double &defTol, int &tSize, double &axisAngle,
                           double &leftTol, double &rightTol);
    bool getAnglesAndStep(int doStep, double &lowAngle, double &highAngle,
                          double &rangeStep, bool &stepOk);
    void anglesSet(int step);
    QLineEdit *mDefocusEdit;
    QLineEdit *mLowAngleEdit;
    QLineEdit *mHighAngleEdit;
    QLineEdit *mDefTolEdit;
    QLineEdit *mLeftTolEdit;
    QLineEdit *mRightTolEdit;
    QLineEdit *mTileSizeEdit;
    QLineEdit *mAxisAngleEdit;
    QLineEdit *mRangeStepEdit;
    QLineEdit *mAutoFromEdit;
    QLineEdit *mAutoToEdit;
    
signals:
    void angle(double lAngle, double hAngle, double expDef, double defTol, 
        int tileSize, double axisAngle, double leftTol, double rightTol);
    void defocusMethod(int );
    void initialTileChoice(int );
public slots:
    void setAnglesClicked();
    void tileParamsClicked();
private slots:
    void applyClicked();
    void enableApplyButton(const QString &text);
    void allAtOnceChecked();
    void onlyCenterChecked();
    void currDefocusChecked();
    void expDefocusChecked();
    void deleteClicked();
    void stepUpClicked();
    void stepDownClicked();
    void autofitClicked();
    void rowDoubleClicked(int row, int column);
    void fitSingleToggled(bool state);

protected:
    void closeEvent( QCloseEvent * e );

  private:
    MyApp *mApp;
    QLabel *mDefocusLabel;
    QLabel *mLowAngleLabel;
    QLabel *mHighAngleLabel;
    QLabel *mDefTolLabel;
    QLabel *mLeftTolLabel;
    QLabel *mRightTolLabel;
    QLabel *mTileSizeLabel;
    QLabel *mAxisAngleLabel;
    QGroupBox *mIfAllGroup;
    QRadioButton *mAllAtOnceRadio;
    QRadioButton *mOnlyCenterRadio;
    QGroupBox *mDefocusGroup;
    QRadioButton *mCurrDefocusRadio;
    QRadioButton *mExpDefocusRadio;
    QTableWidget *mTable;
    QPushButton *mDeleteButton;
    QPushButton *mReturnButton;
    QPushButton *mToFileButton;
   
    QPushButton *mSaveButton; 
    QPushButton *mApplyButton;
    QPushButton *mCloseButton;
    QLabel *mRangeStepLabel;
    QLabel *mAutoFromLabel;
    QLabel *mAutoToLabel;
    QCheckBox *mFitSingleBox;
    QPushButton *mStepUpButton;
    QPushButton *mStepDownButton;
    QPushButton *mAutofitButton;
    QPushButton *mTileParamButton;
    bool mParamsOpen;
};
#endif

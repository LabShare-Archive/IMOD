#ifndef ANGLEDIALOG_H
#define ANGLEDIALOG_H

#include<qdialog.h>
class QLabel;
class QLineEdit;
class QPushButton;
class QButtonGroup;
class QRadioButton;

class AngleDialog :public QDialog
{
  Q_OBJECT
  public:
    AngleDialog(QWidget *parent=0, const char *name=0);
    QLineEdit *defocusEdit;
    QLineEdit *lowAngleEdit;
    QLineEdit *highAngleEdit;
    QLineEdit *defTolEdit;
    QLineEdit *leftTolEdit;
    QLineEdit *rightTolEdit;
    QLineEdit *tileSizeEdit;
    QLineEdit *axisAngleEdit;
signals:
    void angle(double lAngle, double hAngle, double expDef, double defTol, 
        int tileSize, double axisAngle, double leftTol, double rightTol);
    void defocusMethod(int );
    void initialTileChoice(int );
private slots:
    void angleSetted();
    void saveCurrentDefocus();
    void enableApplyButton(const QString &text);
    void allAtOnceChecked();
    void onlyCenterChecked();
    void currDefocusChecked();
    void expDefocusChecked();
  private:
    QLabel *defocusLabel;
    QLabel *lowAngleLabel;
    QLabel *highAngleLabel;
    QLabel *defTolLabel;
    QLabel *leftTolLabel;
    QLabel *rightTolLabel;
    QLabel *tileSizeLabel;
    QLabel *axisAngleLabel;
    QButtonGroup *ifAllGroup;
    QRadioButton *allAtOnceRadio;
    QRadioButton *onlyCenterRadio;
    QButtonGroup *defocusGroup;
    QRadioButton *currDefocusRadio;
    QRadioButton *expDefocusRadio;
   
    QPushButton *saveButton; 
    QPushButton *applyButton;
    QPushButton *closeButton;
};
#endif

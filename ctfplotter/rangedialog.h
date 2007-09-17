#ifndef RANGEDIALOG_H
#define RANGEDIALOG_H

#include<qdialog.h>

class QLabel;
class QLineEdit;
class QPushButton;
class QButtonGroup;
class QRadioButton;

class RangeDialog :public QDialog
{
  Q_OBJECT
  public:
    RangeDialog(QWidget *parent=0, const char *name=0);
signals:
    void range(double lowX, double highX, double, double );
    void x1MethodChosen(int );
    void x2MethodChosen(int);
private slots:
    void rangeSetted();
    void enableApplyButton(const QString &text);
    void x1LinearChecked();
    void x1SimplexChecked();
    void x2LinearChecked();
    void x2SimplexChecked();
  private:
      QLabel *x1_label_1;
      QLabel *x1_label_2;
      QButtonGroup *x1Group; 
      QRadioButton *x1LinearRadio;
      QRadioButton *x1SimplexRadio;
      QLabel *x2_label_1;
      QLabel *x2_label_2;
      QLineEdit *x1_edit_1;
      QLineEdit *x1_edit_2;
      QLineEdit *x2_edit_1;
      QLineEdit *x2_edit_2;
      QButtonGroup *x2Group;
      QRadioButton *x2LinearRadio;
      QRadioButton *x2SimplexRadio;
      
      QPushButton *applyButton;
      QPushButton *closeButton;
};
#endif

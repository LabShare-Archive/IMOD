#include "dialog_frame.h"
class QPushButton;
class QCheckBox;
class BeadFixer : public DialogFrame
{
  Q_OBJECT

 public:
  BeadFixer(QWidget *parent, const char *name = NULL);
  ~BeadFixer() {};

  public slots:
  void buttonPressed(int which);
  void nextGap();
  void openFile();
  void rereadFile() {reread(0);};
  void nextLocal() {reread(1);};
  void nextRes();
  void movePoint();
  void undoMove();
  void clearList();
  void onceToggled(bool state);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

 private:
  void reread(int which);
  int foundgap(int obj, int cont, int ipt, int before);
  void clearExtraObj();
  QPushButton *rereadBut;
  QPushButton *nextLocalBut;
  QPushButton *nextResBut;
  QPushButton *movePointBut;
  QPushButton *undoMoveBut;
  QPushButton *clearListBut;
};

#include "dialog_frame.h"
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
class QPushButton;
class QCheckBox;
class BeadFixer2 : public DialogFrame
{
  Q_OBJECT

 public:
  BeadFixer2(QWidget *parent, const char *name = NULL);
  ~BeadFixer2() {};
  void reread(int which);

  public slots:
  void buttonPressed(int which);
  void nextGap();
  void openFile();
  void rereadFile() {reread(0);};
  void nextLocal() {reread(1);};
  void nextRes();
  void backUp();
  void movePoint();
  void undoMove();
  void clearList();
  void onceToggled(bool state);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

 private:
  int foundgap(int obj, int cont, int ipt, int before);
  void clearExtraObj();
  QPushButton *rereadBut;
  QPushButton *nextLocalBut;
  QPushButton *nextResBut;
  QPushButton *backUpBut;
  QPushButton *movePointBut;
  QPushButton *undoMoveBut;
  QPushButton *clearListBut;
};

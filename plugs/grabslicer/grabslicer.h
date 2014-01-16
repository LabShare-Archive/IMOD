#include "dialog_frame.h"
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
#include <QtSql>
class QPushButton;
class QCheckBox;
class QTextEdit;
class QLineEdit;

class QButtonGroup;
class QLabel;
class QEvent;

#include "imodplugin.h"

class GrabSlicer : public DialogFrame
{
  Q_OBJECT

 public:
  GrabSlicer(QWidget *parent, const char *name = NULL);
  ~GrabSlicer() {};

  QLineEdit *tilt_id;
  QTextEdit *tilt_note;
  QLabel *lblTilt;
  int winValue;
  int SnapShot_format;

  void reread(int which);

  public slots:

  void buttonPressed(int which);
  void saveImage();
  void saveDatabase();
  void winSelected(int value);
  void imgtypeSelected(int value);
  void quitDatabase(QSqlDatabase db);


 protected:

  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );


 private:

  QButtonGroup *winGroup;
  QButtonGroup *imgGroup;
  QWidget *topBox;

  QPushButton *saveImgBut;
  QPushButton *saveDatabaseBut;


};

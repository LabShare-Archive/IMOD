#include "dialog_frame.h"

#define MAX_EDIT_BOXES 12

class ToolEdit;
class QGridLayout;
class QSignalMapper;
class LineTrack : public DialogFrame
{
  Q_OBJECT

 public:
  LineTrack(QWidget *parent, const char *name = NULL);
  ~LineTrack() {};
  void track(int copy);
  void undo();

  public slots:
  void buttonPressed(int which);
  void valueEntered(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

 private:
  void help();
  void makeEditRow(int row, char *label, int *iPtr, float *fPtr, bool isInt,
                   float min, float max, char *tip);
  QSignalMapper *mMapper;
  QGridLayout *mGrid;
  ToolEdit *mEdit[MAX_EDIT_BOXES];
  float mMinVal[MAX_EDIT_BOXES];
  float mMaxVal[MAX_EDIT_BOXES];
  int *mIntPtr[MAX_EDIT_BOXES];
  float *mFloatPtr[MAX_EDIT_BOXES];
  bool mIsInt[MAX_EDIT_BOXES];
};

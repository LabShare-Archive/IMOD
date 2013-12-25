#ifndef LINEGUI_H
#define LINEGUI_H

#include "dialog_frame.h"

#ifndef LINE_PLUGIN
#include "special_module.h"

class LineTrackModule : public SpecialModule
{
 public:
  LineTrackModule();
};
#endif

#ifdef F77FUNCAP
#define linetrack_ LINETRACK
#define conttrack_ CONTTRACK
#endif
extern "C" {
void linetrack_(unsigned char *image, int *nx, int *ny,
                float *points, int *csize, int *cpnt, int *cmax,
                int   *ksize,
                int   *knum,
                float *sigma,
                float *h,
                int   *ifdark,
                float *stepsize,
                float *redtol,
                int   *ifreplace,
                float *offset,
                int   *closecont,
                int   *iffail);

void conttrack_(unsigned char *image, int *nx, int *ny,
                float *points, int *csize, int *cpnt, float *p_copy, int *cmax,
                int   *ksize,
                int   *knum,
                float *sigma,
                float *h,
                int   *ifdark,
                float *stepsize,
                float *redtol,
                float *offset,
                int   *copytol,
                int   *copypool,
                int   *copyfit);
}

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
  void changeEvent(QEvent *e);

 private:
  void help();
  void makeEditRow(int row, const char *label, int *iPtr, float *fPtr, bool isInt,
                   float min, float max, const char *tip);
  void fillinValue(int row);
  QSignalMapper *mMapper;
  QGridLayout *mGrid;
  ToolEdit *mEdit[MAX_EDIT_BOXES];
  float mMinVal[MAX_EDIT_BOXES];
  float mMaxVal[MAX_EDIT_BOXES];
  int *mIntPtr[MAX_EDIT_BOXES];
  float *mFloatPtr[MAX_EDIT_BOXES];
  bool mIsInt[MAX_EDIT_BOXES];
};

#endif

//Added by qt3to4:
#include <QLabel>
#include <QKeyEvent>
#include <QCloseEvent>
/*  imodv_isosurface.h  -  declarations for imodv_isosurface.cpp
 *
 *  $Id$
 *
 * $Log$
 * Revision 4.8  2008/11/07 23:48:53  xiongq
 * seperate threshold for each stack
 *
 * Revision 4.7  2008/10/02 16:27:00  xiongq
 * add small piece filter, binning, and local XYZ functions
 *
 * Revision 4.6  2008/05/27 18:20:15  mast
 * Limited to 4 threads
 *
 *
 */

#ifndef IMODV_ISOSURFACE_H
#define IMODV_ISOSURFACE_H

typedef struct __imodv_struct ImodvApp;

#define IMODV_CENTER_VOLUME 1
#define IMODV_VIEW_BOX (1 << 1)
#define IMODV_VIEW_USER_MODEL (1 << 2)
#define IMODV_VIEW_ISOSURFACE (1 << 3)
#define IMODV_LINK_XYZ (1 << 4)
#define IMODV_DELETE_PIECES (1 << 5)


// Based on timing tests in May 2008
#define MAX_THREADS 4

/* Image Control functions. */
void imodvIsosurfaceEditDialog(ImodvApp *a, int state);
bool imodvIsosurfaceUpdate(void);

#include "dialog_frame.h"
#include <qspinbox.h>
#include <vector>

class MultiSlider;
class QCheckBox;
class QLabel;
class QSlider;
class QLineEdit;
//class QSpinBox;
class QPushButton;
class HistWidget;
struct ViewInfo;
struct Mod_Point;
struct Mod_Mesh;
class IsoThread;
class Surface_Pieces;

class ImodvIsosurface : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvIsosurface(struct ViewInfo *vi, QWidget *parent, const char *name = NULL) ;
  ~ImodvIsosurface();
  void updateCoords();
  void setBoundingBox();
  void setBoundingObj();
  void setViewCenter();
  int getCurrStackIdx();
  float fillVolumeArray();
  void  fillBinVolume();
  void setIsoObj();
  void smoothMesh(struct Mod_Mesh *, int);
  void filterMesh(bool);

  QCheckBox *mViewIso, *mViewModel, *mViewBoxing, *mCenterVolume;
  QCheckBox *mLinkXYZ, *mDeletePieces; 
  MultiSlider *mSliders;
  HistWidget *mHistPanel;
  MultiSlider *mHistSlider;
  QPushButton *mUseRubber;
  QSpinBox *mSmoothBox;
  QSpinBox *mBinningBox;
  QSpinBox *mPiecesBox;

  int mLocalX;
  int mLocalY;
  int mLocalZ;
  int mBoxObjNum;
  int mCurrTime;
  int mBoxOrigin[3];
  int mBoxEnds[3];
  int mBinBoxEnds[3];
  int mBoxSize[3];
  int mBinBoxSize[3];
  int mSubZEnds[MAX_THREADS+1];
  unsigned char *mVolume;
  unsigned char *mBinVolume;
  int mCurrStackIdx; // wihich stack is current;
  std::vector<float> mStackThresholds;
  float mThreshold; //threshold for the current stack;
  int mNThreads;
  int mInitNThreads;
  Surface_Pieces *mSurfPieces;

  public slots:
    void viewIsoToggled(bool state) ;
    void viewModelToggled(bool state) ;
    void viewBoxingToggled(bool state);
    void centerVolumeToggled(bool state);
    void deletePiecesToggled(bool);
    void linkXYZToggled(bool);
    void histChanged(int, int, bool );
    void iterNumChanged(int);
    void binningNumChanged(int);
    void sliderMoved(int which, int value, bool dragging);
    void buttonPressed(int which);
    void showRubberBandArea();
    void numOfTrianglesChanged(int);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  bool mCtrlPressed;
  struct ViewInfo *mIsoView;
  int mExtraObjNum;
  double mCurrMax;
  IsoThread *threads[MAX_THREADS];

  struct Mod_Mesh * mOrigMesh;
  struct Mod_Mesh * mFilteredMesh;
};

#endif

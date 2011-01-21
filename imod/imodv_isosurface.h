//Added by qt3to4:
#include <QLabel>
#include <QKeyEvent>
#include <QCloseEvent>
/*  imodv_isosurface.h  -  declarations for imodv_isosurface.cpp
 *
 *  $Id$
 *
 * $Log$
 * Revision 4.10  2010/03/30 02:22:31  mast
 * Added to thread limit comment
 *
 * Revision 4.9  2009/01/15 16:33:18  mast
 * Qt 4 port
 *
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
typedef unsigned int Index;
extern void smooth_vertex_positions(float *varray, Index nv,
    const Index *tarray, Index nt,
    float smoothing_factor, int smoothing_iterations);

// Based on timing tests in May 2008, confirmed on Nehalem March 2010
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
struct Mod_Contour;
class IsoThread;
class Surface_Pieces;
typedef struct {
  int ix, iy, iz;
} Point3D;


class ImodvIsosurface : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvIsosurface(struct ViewInfo *vi, QWidget *parent, const char *name = NULL) ;
  ~ImodvIsosurface();
  void updateCoords(bool setLocal);
  void setBoundingBox();
  void setBoundingObj();
  void setViewCenter();
  int getCurrStackIdx();
  void setIsoObj();
  void fillAndProcessVols(bool setThresh);
  void resizeToContours(bool draw);
  int getBinning();

  int mLocalX;
  int mLocalY;
  int mLocalZ;
  int mMaskObj;
  int mMaskCont;
  int mMaskPsize;
  int mCurrTime;
  int mBoxOrigin[3];
  int mBoxEnds[3];
  unsigned char *mVolume;
  int mCurrStackIdx; // wihich stack is current;
  std::vector<float> mStackThresholds;
  float mThreshold; //threshold for the current stack;
  std::vector<int> mStackOuterLims;
  int mOuterLimit;
  int mBinBoxSize[3];
  int mSubZEnds[MAX_THREADS+1];
  unsigned char *mBinVolume;
  int mNThreads;


  public slots:
    void viewIsoToggled(bool state) ;
    void viewModelToggled(bool state) ;
    void viewBoxingToggled(bool state);
    void centerVolumeToggled(bool state);
    void deletePiecesToggled(bool state);
    void linkXYZToggled(bool state);
    void closeFacesToggled(bool state);
    void histChanged(int, int, bool );
    void iterNumChanged(int);
    void binningNumChanged(int);
    void sliderMoved(int which, int value, bool dragging);
    void buttonPressed(int which);
    void showRubberBandArea();
    void numOfTrianglesChanged(int);
    void maskSelected(int which);
    void areaFromContClicked();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  float fillVolumeArray();
  void  fillBinVolume();
  void removeOuterPixels();
  void smoothMesh(struct Mod_Mesh *, int);
  void filterMesh(bool);
  bool allocArraysIfNeeded();
  int maskWithContour(struct Mod_Contour *inCont, int iz);
  void closeBoxFaces();
  void applyMask();
  void showDefinedArea(float x0, float x1, float y0, float y1, bool draw);
  int findClosestZ(int iz, int *listz, int zlsize, int **contatz, int *numatz, int zmin,
                   int &otherSide);
  void addToNeighborList(Point3D **neighp, int &numNeigh, int &maxNeigh,
                         int ix, int iy, int iz);
  void dumpVolume(char *filename);
  void setFontDependentWidths();

  bool mCtrlPressed;
  struct ViewInfo *mVi;
  int mExtraObjNum;
  int mCurrMax;
  IsoThread *threads[MAX_THREADS];

  struct Mod_Mesh * mOrigMesh;
  struct Mod_Mesh * mFilteredMesh;
  QCheckBox *mViewIso, *mViewModel, *mViewBoxing, *mCenterVolume;
  QCheckBox *mLinkXYZ, *mDeletePieces; 
  MultiSlider *mSliders;
  HistWidget *mHistPanel;
  MultiSlider *mHistSlider;
  QPushButton *mUseRubber, *mSizeContours;
  QSpinBox *mSmoothBox;
  QSpinBox *mBinningBox;
  QSpinBox *mPiecesBox;

  int mBoxObjNum;
  int mBinBoxEnds[3];
  int mBoxSize[3];

  unsigned char *mTrueBinVol;
  float mMedian;
  int mVolMin, mVolMax;
  int mInitNThreads;
  Surface_Pieces *mSurfPieces;

};

#endif

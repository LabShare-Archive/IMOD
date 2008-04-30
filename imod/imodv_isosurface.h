#ifndef IMODV_ISOSURFACE_H
#define IMODV_ISOSURFACE_H

typedef struct __imodv_struct ImodvApp;

#define IMODV_CENTER_VOLUME 1
#define IMODV_VIEW_BOX (1 << 1)
#define IMODV_VIEW_USER_MODEL (1 << 2)
#define IMODV_VIEW_ISOSURFACE (1 << 3)

/* Image Control functions. */
void imodvIsosurfaceEditDialog(ImodvApp *a, int state);
void imodvIsosurfaceUpdate(void);

#include "dialog_frame.h"
class MultiSlider;
class QCheckBox;
class QLabel;
class QSlider;
class QLineEdit;
class HistWidget;
struct ViewInfo;
struct Mod_Point;



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
  float fillVolumeArray();
  void setIsoObj();

  QCheckBox *mViewIso, *mViewModel, *mViewBoxing, *mCenterVolume;
  MultiSlider *mSliders;
  HistWidget *mHistPanel;
  MultiSlider *mHistSlider;

  int mBoxObjNum;
  int mCurrTime;
  int mBoxOrigin[3];
  int mBoxEnds[3];

  public slots:
    void viewIsoToggled(bool state) ;
    void viewModelToggled(bool state) ;
    void viewBoxingToggled(bool state);
    void centerVolumeToggled(bool state);
    void histChanged(int, int, bool );
    void sliderMoved(int which, int value, bool dragging);
    void buttonPressed(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  bool mCtrlPressed;
  struct ViewInfo *mIsoView;
  int mExtraObjNum;
  int mBoxSize[3];
  double mCurrMax;
  unsigned char *mVolume;
  float mThreshold;
};

#endif

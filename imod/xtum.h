/*   xtum.h  -  declarations for xtum.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.3  2003/01/29 01:52:41  mast
make gl widget public

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/10 23:42:45  mast
initial creation


*/
#ifndef XTUM_H
#define XTUM_H

#define MAX_XTUM_TOGGLES 2

#define NO_X_INCLUDES
#include <imodel.h>

#include <qmainwindow.h>
#include <qgl.h>

class QToolButton;
class QSignalMapper;
class QHBox;
class QSpinBox;
class MultiSlider;
class TumblerGL;
class TumblerWindow;

struct ViewInfo;
typedef struct b3d_ci_image B3dCIImage;
typedef struct MRCslice Islice;
typedef struct imodel_matrix Imat;

int xtumOpen(struct ViewInfo *vi);

typedef struct imod_xtum_struct
{
  struct ViewInfo *vi;

  TumblerWindow *dialog;
  B3dCIImage   *image;

  float  scale, offset;
  int    minval, maxval;
  float  alpha, beta, gamma;
  float  plax;
  float  tstep;
  Ipoint xstep, ystep, zstep;
  Ipoint sxstep, systep, szstep;

  Islice *slice;
  Islice *stslice;
  Islice *bwslice;
  Islice *count;

  int    exposed;       /* drawing area first exposure flag. */
  int    width, height; /* drawing area size. */
  int    zoom;
  int    cx, cy, cz;
  int    nx, ny, nz;
  int    ms;            /* max size, diameter of binding sphere. */
  int    highres;
  int    locked;
  int    stereo;
  int    bbox;
  int    fillval;
  int    ctrl;
} TumblerStruct;


class TumblerWindow : public QMainWindow
{
  Q_OBJECT

 public:
  TumblerWindow(TumblerStruct *xtum, bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, 
	    WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~TumblerWindow() {};
  void externalKeyEvent ( QKeyEvent * e, int released);
  void draw( TumblerStruct *xtum);
  void setSlice(TumblerStruct *xtum);
  void drawBorder(TumblerStruct *xtum);
  void drawImage(TumblerStruct *xtum);
  void drawBBox(TumblerStruct *xtum);
  void newData(TumblerStruct *xtum);

  TumblerGL *mGLw;

  public slots:
    void zoomChanged(int value);
  void thresholdChanged(int which, int value, bool dragging);
  void sizeChanged(int which);
  void help();
  void toggleClicked(int index);

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  
 private:
  void setupToggleButton(QHBox *toolBar, QSignalMapper *mapper, 
			 int index);
  void fillSlice(TumblerStruct *xtum);
  void fillASlice(TumblerStruct *xtum);
  void scaleData(TumblerStruct *xtum, unsigned short *sdata);
  void drawBoxLines(TumblerStruct *xtum, Imat *mat);
  void setSteps(TumblerStruct *xtum);
  void drawSubArea(TumblerStruct *xtum, unsigned short *sdata, int llx,
		       int urx);
  void computeRotation(float x, float y, float z);

  
  TumblerStruct *mTum;
  QToolButton *mToggleButs[MAX_XTUM_TOGGLES];
  int mToggleStates[MAX_XTUM_TOGGLES];
  bool mCtrlPressed;
  MultiSlider *mSliders;
  QSpinBox *mZoomBox;
  QSpinBox *mSizeBoxes[3];

};

class TumblerGL : public QGLWidget
{
  Q_OBJECT

 public:
  TumblerGL(TumblerStruct *xtum, QGLFormat format, QWidget * parent = 0,
        const char * name = 0);
  ~TumblerGL() {};
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );

 private:
  TumblerStruct *mTum;
};

#endif

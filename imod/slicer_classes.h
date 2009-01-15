//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QKeyEvent>
/*   slicer_classes.h  -  declarations for slicer_classes.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef SLICER_CLASSES_H
#define SLICER_CLASSES_H

#define MAX_SLICER_TOGGLES 6

enum {SLICER_TOGGLE_HIGHRES = 0, SLICER_TOGGLE_LOCK, SLICER_TOGGLE_CENTER, 
      SLICER_TOGGLE_SHIFTLOCK, SLICER_TOGGLE_FFT, SLICER_TOGGLE_TIMELOCK};

#include <qmainwindow.h>
#include <qspinbox.h>
#include <qgl.h>
#include <qstring.h>

class HotToolBar;
class QToolButton;
class QPushButton;
class ToolEdit;
class QLabel;
class QSignalMapper;
class QSlider;
class MultiSlider;
class QComboBox;
class QCheckBox;

typedef struct Super_slicer SlicerStruct;
class SlicerGL;
class SlicerCube;
class QDoubleSpinBox;
class HotToolBar;

class SlicerWindow : public QMainWindow
{
  Q_OBJECT

 public:
  SlicerWindow(SlicerStruct *slicer, float maxAngles[], QString timeLabel,
               bool rgba, bool doubleBuffer, bool enableDepth, 
               QWidget * parent = 0, Qt::WFlags f = Qt::Window) ;
  ~SlicerWindow() {};
  void setToggleState(int index, int state);
  void setZoomText(float zoom);
  void setModelThickness(float depth);
  void setImageThickness(int depth);
  void setAngles(float *angles);
  void showSaveAngleToolbar();

  SlicerGL *mGLw;
  SlicerCube *mCube;
  SlicerStruct *mSlicer;
  HotToolBar *mToolBar;
  HotToolBar *mToolBar2;
  HotToolBar *mTimeBar;
  HotToolBar *mSaveAngBar;
  QPushButton *mSetAngBut;
  QCheckBox *mAutoBox;
  QPushButton *mNewRowBut;
  QPushButton *mSaveAngBut;

  public slots:
    void zoomUp();
  void zoomDown();
  void help();
  void newZoom();
  void angleChanged(int which, int value, bool dragging);
  void toggleClicked(int index);
  void imageThicknessChanged(int depth);
  void modelThicknessChanged(double depth);
  void showslicePressed();
  void contourPressed();
  void setTimeLabel(QString label);
  void zScaleSelected(int item);
  void toolKeyPress(QKeyEvent *e) {keyPressEvent(e);};
  void toolKeyRelease(QKeyEvent *e) {keyReleaseEvent(e);};
  void timeBack();
  void timeForward();
  void saveAngClicked();
  void setAngClicked();
  void newRowClicked();
  void continuousToggled(bool state);
  void linkToggled(bool state);

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  void fontChange( const QFont & oldFont ) {setFontDependentWidths();};
  
 private:
  void setFontDependentWidths();
  
  QToolButton *mToggleButs[MAX_SLICER_TOGGLES];
  int mToggleStates[MAX_SLICER_TOGGLES];
  ToolEdit *mZoomEdit;
  QSpinBox *mImageBox;
  QDoubleSpinBox *mModelBox;
  MultiSlider *mSliders;
  QComboBox *mZscaleCombo;
  QLabel *mTimeLabel;
  QPushButton *mHelpButton;
  int mBreakBeforeAngBar;
};

class SlicerGL : public QGLWidget
{
  Q_OBJECT

 public:
  SlicerGL(SlicerStruct *slicer, QGLFormat format, QWidget * parent = 0);
  ~SlicerGL() {};
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mouseMoveEvent(QMouseEvent * e );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void timerEvent(QTimerEvent *e);

 private:

  SlicerStruct *mSlicer;
  bool mMousePressed;
  bool mFirstDraw;
  int mTimerID;
};

// The cube drawing class
class SlicerCube : public QGLWidget
{
  Q_OBJECT

 public:
  SlicerCube(SlicerStruct *slicer, QGLFormat format, QWidget * parent = 0);
  ~SlicerCube() {};
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );

 private:
  SlicerStruct *mSlicer;
};

#ifdef QT_THREAD_SUPPORT
#include <qthread.h>

class SlicerThread : public QThread
{
 public:
  SlicerThread(int jStart, int jLimit);
  ~SlicerThread() {};

 protected:
  void run();

 private:
  int mJstart, mJlimit;
};
#endif

void fillImageArray(SlicerStruct *ss, int panning, int meanOnly);

#endif     // SLICER_CLASSES_H

/*
$Log$
Revision 4.12  2008/11/29 22:10:30  mast
Added ability to link slicers

Revision 4.11  2007/06/15 21:19:54  mast
Added shift lock toolbar botton

Revision 4.10  2007/05/31 16:32:28  mast
Changes for slicer angle toolbar, classic setting and warning

Revision 4.9  2007/05/29 14:52:35  mast
Changes for new slicer mode and toolbar buttons

Revision 4.8  2006/10/12 19:02:55  mast
Added toolbar button for W function

Revision 4.7  2006/10/06 19:25:40  mast
Added thread definition

Revision 4.6  2006/09/12 15:36:09  mast
Added mouse move slot

Revision 4.5  2005/03/08 15:49:00  mast
Added enum for toolbar toggles

Revision 4.4  2004/08/12 17:14:11  mast
Made mSlicer public so angle reporting routine can access it

Revision 4.3  2003/12/16 23:54:22  mast
Move floatspinbox to libdiaqt

Revision 4.2  2003/03/26 17:15:31  mast
Adjust sizes for font changes

Revision 4.1  2003/02/10 20:41:56  mast
Merge Qt source

Revision 1.1.2.3  2003/01/30 00:53:49  mast
New timer logic for clean first image

Revision 1.1.2.2  2003/01/10 23:55:07  mast
make setAngle public, remove timer

Revision 1.1.2.1  2003/01/06 15:48:55  mast
initila creation

*/

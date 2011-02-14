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

class SlicerFuncs;
class SlicerGL;
class SlicerCube;
class QDoubleSpinBox;
class HotToolBar;

class SlicerWindow : public QMainWindow
{
  Q_OBJECT

 public:
  SlicerWindow(SlicerFuncs *funcs, float maxAngles[], QString timeLabel,
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
  SlicerFuncs *mFuncs;
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
  SlicerGL(SlicerFuncs *funcs, QGLFormat format, QWidget * parent = 0);
  ~SlicerGL() {};
  void setBufferSwapAuto(bool state) { setAutoBufferSwap(state); };
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mouseMoveEvent(QMouseEvent * e );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void timerEvent(QTimerEvent *e);

 private:

  SlicerFuncs *mFuncs;
  bool mMousePressed;
  bool mFirstDraw;
  int mTimerID;
};

// The cube drawing class
class SlicerCube : public QGLWidget
{
  Q_OBJECT

 public:
  SlicerCube(SlicerFuncs *funcs, QGLFormat format, QWidget * parent = 0);
  ~SlicerCube() {};
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );

 private:
  SlicerFuncs *mFuncs;
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

#endif     // SLICER_CLASSES_H

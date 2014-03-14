/*   slicer_classes.h  -  declarations for slicer_classes.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef SLICER_CLASSES_H
#define SLICER_CLASSES_H

#define MAX_SLICER_TOGGLES 9

enum {SLICER_TOGGLE_HIGHRES = 0, SLICER_TOGGLE_LOCK, SLICER_TOGGLE_CENTER, 
      SLICER_TOGGLE_BAND, SLICER_TOGGLE_ARROW, SLICER_TOGGLE_FFT, SLICER_TOGGLE_ZSCALE,
      SLICER_TOGGLE_TIMELOCK, SLICER_TOGGLE_SHIFTLOCK};
enum {SLICER_LIMIT_INVALID = 0, SLICER_LIMIT_TRUNCATE, SLICER_LIMIT_VALID};

//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QKeyEvent>

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
class QCheckBox;
class QAction;

class SlicerFuncs;
class SlicerGL;
class SlicerCube;
class QDoubleSpinBox;
class HotToolBar;
class RotationTool;
class HotWidget;

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
  void setViewAxisPosition(int amin, int amax, int current);
  void showSaveAngleToolbar();
  void setLowHighValidity(int which, int state);
  void enableLowHighButtons(int enable);
  void manageAutoLink(int newState);

  SlicerGL *mGLw;
  SlicerCube *mCube;
  SlicerFuncs *mFuncs;
  HotToolBar *mToolBar;
  HotToolBar *mToolBar2;
  HotWidget *mFreeBar2;
  HotToolBar *mTimeBar;
  HotToolBar *mSaveAngBar;
  QPushButton *mSetAngBut;
  QCheckBox *mAutoBox;
  QCheckBox *mLinkBox;
  QPushButton *mNewRowBut;
  QPushButton *mSaveAngBut;
  RotationTool *mRotationTool;

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
  void fillCachePressed();
  void setTimeLabel(int time, QString label);
  void toolKeyPress(QKeyEvent *e) {keyPressEvent(e);};
  void toolKeyRelease(QKeyEvent *e) {keyReleaseEvent(e);};
  void timeBack();
  void timeForward();
  void saveAngClicked();
  void setAngClicked();
  void newRowClicked();
  void continuousToggled(bool state);
  void linkToggled(bool state);
  void rotationClicked(int deltaX, int deltaY, int deltaZ);
  void stepSizeChanged(int delta);
  void shiftToggled(bool state);
  void lowHighClicked(int which);
  void contextMenuHit(int val);
  void toolbarMenuEvent(QContextMenuEvent *event);
  void freeBarClose();

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  void changeEvent(QEvent *e);
  
 private:
  void setFontDependentWidths();
  void buildToolBar2(bool freeBar);
  
  QToolButton *mToggleButs[MAX_SLICER_TOGGLES];
  int mToggleStates[MAX_SLICER_TOGGLES];
  ToolEdit *mZoomEdit;
  QSpinBox *mImageBox;
  QDoubleSpinBox *mModelBox;
  MultiSlider *mSliders;
  QLabel *mTimeNumLabel;
  QLabel *mTimeLabel;
  QPushButton *mHelpButton;
  QPushButton *mLowHighButtons[2];
  int mLowHighStates[2];
  QAction *mLowHighActions[2];
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
  void wheelEvent (QWheelEvent *e);

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

class HotWidget : public QWidget
{
  Q_OBJECT
 public:
  HotWidget( QWidget * parent = 0, Qt::WFlags fl = Qt::Window) 
    : QWidget(parent, fl) { };
  ~HotWidget() {}

 signals:
  void keyPress(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);
  void contextMenu(QContextMenuEvent *e);
  void closePressed();

 protected:
  void keyPressEvent ( QKeyEvent * e ) {emit keyPress(e);};
  void keyReleaseEvent ( QKeyEvent * e ) {emit keyRelease(e);};
  void contextMenuEvent(QContextMenuEvent *e) {emit contextMenu(e);};
  void closeEvent (QCloseEvent * e ) {emit closePressed() ; e->ignore();};
};


#endif     // SLICER_CLASSES_H

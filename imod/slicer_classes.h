/*   slicer_classes.h  -  declarations for slicer_classes.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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
#ifndef SLICER_CLASSES_H
#define SLICER_CLASSES_H

#define MAX_SLICER_TOGGLES 2

#include <qmainwindow.h>
#include <qspinbox.h>
#include <qtoolbar.h>
#include <qgl.h>

class QToolButton;
class QPushButton;
class ToolEdit;
class QLabel;
class QSignalMapper;
class QSlider;
class MultiSlider;
class QComboBox;

typedef struct Super_slicer SlicerStruct;
class SlicerGL;
class SlicerCube;
class FloatSpinBox;
class HotToolBar;

class SlicerWindow : public QMainWindow
{
  Q_OBJECT

 public:
  SlicerWindow(SlicerStruct *slicer, float maxAngles[], bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, 
	    WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~SlicerWindow() {};
  void setToggleState(int index, int state);
  void setZoomText(float zoom);
  void setModelThickness(float depth);
  void setImageThickness(int depth);
  void setAngles(float *angles);

  SlicerGL *mGLw;
  SlicerCube *mCube;
  HotToolBar *mToolBar;
  HotToolBar *mToolBar2;
  SlicerStruct *mSlicer;

  public slots:
    void zoomUp();
  void zoomDown();
  void help();
  void newZoom();
  void angleChanged(int which, int value, bool dragging);
  void toggleClicked(int index);
  void imageThicknessChanged(int depth);
  void modelThicknessChanged(int depth);
  void showslicePressed();
  void zScaleSelected(int item);
  void toolKeyPress(QKeyEvent *e) {keyPressEvent(e);};
  void toolKeyRelease(QKeyEvent *e) {keyReleaseEvent(e);};

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  void fontChange( const QFont & oldFont ) {setFontDependentWidths();};
  
 private:
  void setupToggleButton(QToolBar *toolBar, QSignalMapper *mapper, 
			 int index);
  void setFontDependentWidths();
  
  QToolButton *mToggleButs[MAX_SLICER_TOGGLES];
  int mToggleStates[MAX_SLICER_TOGGLES];
  ToolEdit *mZoomEdit;
  QSpinBox *mImageBox;
  FloatSpinBox *mModelBox;
  MultiSlider *mSliders;
  QComboBox *mZscaleCombo;
  QPushButton *mHelpButton;
};

class SlicerGL : public QGLWidget
{
  Q_OBJECT

 public:
  SlicerGL(SlicerStruct *slicer, QGLFormat format, QWidget * parent = 0,
        const char * name = 0);
  ~SlicerGL() {};
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );
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
  SlicerCube(SlicerStruct *slicer, QGLFormat format, 
	     QWidget * parent = 0, const char * name = 0);
  ~SlicerCube() {};
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );

 private:
  SlicerStruct *mSlicer;
};

// A toolbar class that will pass on keys
class HotToolBar : public QToolBar
{
  Q_OBJECT
 public:
  HotToolBar( QMainWindow * parent = 0, const char * name = 0) 
    : QToolBar(parent, name) { };
  ~HotToolBar() {}

 signals:
  void keyPress(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);

 protected:
  void keyPressEvent ( QKeyEvent * e ) {emit keyPress(e);};
  void keyReleaseEvent ( QKeyEvent * e ) {emit keyRelease(e);};
};

#endif     // SLICER_CLASSES_H

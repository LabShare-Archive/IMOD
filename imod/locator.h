/*   locator.h  -  declarations for locator.cpp
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef LOCATOR_H
#define LOCATOR_H
#include <qmainwindow.h>
#include <qgl.h>

class LocatorGL;
class QPushButton;
class QLabel;
typedef struct ViewInfo ImodView;
typedef struct b3d_ci_image B3dCIImage;

int locatorOpen(ImodView *vi);
void locatorScheduleDraw(ImodView *vi);

class LocatorWindow : public QMainWindow
{
  Q_OBJECT

 public:
  LocatorWindow(bool rgba, bool doubleBuffer, bool enableDepth, 
                QWidget * parent = 0, const char * name = 0, 
                WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~LocatorWindow() {};

  int mCtrl;
  QLabel *mZoomLabel;
  
 public slots:
  void help();
  void zoomUp();
  void zoomDown();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont ) {setFontDependentWidths();};

 private:
  void setFontDependentWidths();
  QPushButton *mHelpButton;
};

class LocatorGL : public QGLWidget
{
  Q_OBJECT

 public:
  LocatorGL(QGLFormat format, QWidget * parent = 0, const char * name = 0);
  ~LocatorGL() {};
  void drawIfNeeded(int drawflag);
  void scheduleDraw();
  void getImxy(int x, int y, float &imx, float &imy);
  ImodView *mVi;
  B3dCIImage *mImage;
  void changeSize(float factor);
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );
  void timerEvent(QTimerEvent * e );

 private:
  int mWinx, mWiny;
  double mZoom;
  int mLastAxis;
  int mLastSubRet;
  int mSection, mTime;
  int mLastX0, mLastY0;
  int mLastNx, mLastNy;
  int mBlack, mWhite;
  bool mFirstDraw;
  int mXborder, mYborder;
  int mTimerID;
  int mCursorSet;
  int mMouseX, mMouseY;
};

#endif

/*
$Log$
Revision 1.1  2007/08/13 16:05:25  mast
Added to program


*/

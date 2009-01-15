/*
 *  zap_classes.h -- Header file for ZaP mainwindow and GLwidget classes.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef ZAP_CLASSES_H
#define ZAP_CLASSES_H

#define NUM_TOOLBUTTONS 6
#define NUM_TIMEBUTTONS 1

#define ZAP_TOGGLE_RESOL 0
#define ZAP_TOGGLE_ZLOCK 1
#define ZAP_TOGGLE_CENTER 2
#define ZAP_TOGGLE_INSERT 3
#define ZAP_TOGGLE_RUBBER 4
#define ZAP_TOGGLE_TIMELOCK 5
#define MULTIZ_MAX_PANELS 20

#include <qmainwindow.h>
#include <qgl.h>
#include <qstring.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QWheelEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>

class QToolButton;
class ToolEdit;
class QLabel;
class HotToolBar;
class QSignalMapper;
class QSlider;
class QPushButton;
class QSpinBox;
class QAction;

struct zapwin;
class ZapGL;

class ZapWindow : public QMainWindow
{
  Q_OBJECT

 public:
  ZapWindow(struct zapwin *zap, QString timeLabel, bool panels, bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, Qt::WFlags f = Qt::Window) ;
  ~ZapWindow();
  void setToggleState(int index, int state);
  void setZoomText(float zoom);
  void setSectionText(int section);
  void setMaxZ(int maxZ);
  void setTimeLabel(QString label);
  void setSizeText(int winx, int winy);
  void setLowHighSectionState(int state);
  void setSizeAngleState();
  QString lowSection();
  QString highSection();

  ZapGL *mGLw;
  HotToolBar *mToolBar;
  HotToolBar *mToolBar2;
  HotToolBar *mPanelBar;
  struct zapwin *mZap;

  public slots:
    void zoomUp();
    void zoomDown();
    void help();
    void info();
    void newZoom();
    void newSection();
    void sliderChanged(int value);
    void secPressed();
    void secReleased();
    void timeBack();
    void timeForward();
    void toggleClicked(int index);
    void rowsChanged(int value);
    void columnsChanged(int value);
    void zStepChanged(int value);
    void drawCenterToggled(bool state);
    void drawOthersToggled(bool state);
    void toolKeyPress(QKeyEvent *e) {keyPressEvent(e);};
    void toolKeyRelease(QKeyEvent *e) {keyReleaseEvent(e);};
    void setLowSection();
    void setHighSection();

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void fontChange(const QFont &oldFont) {setFontDependentWidths();};
    void wheelEvent ( QWheelEvent * e);

 private:
    void setFontDependentWidths();

    QToolButton *mToggleButs[NUM_TOOLBUTTONS];
    int mToggleStates[NUM_TOOLBUTTONS];
    QAction *mToggleActs[NUM_TOOLBUTTONS];
    ToolEdit *mZoomEdit;
    ToolEdit *mSectionEdit;
    QLabel *mTimeLabel;
    QSlider *mSecSlider;
    QPushButton *mInfoButton;
    QPushButton *mHelpButton;
    QLabel *mSizeLabel;
    QLabel *mAngleLabel;
    int mSizeAngleState;
    bool mSecPressed;
    int mDisplayedSection;
    bool mCtrlPressed;
    QSpinBox *mRowSpin;
    QSpinBox *mColumnSpin;
    QSpinBox *mZstepSpin;
    QPushButton *mLowSectionButton;
    ToolEdit *mLowSectionEdit;
    QPushButton *mHighSectionButton;
    ToolEdit *mHighSectionEdit;
    QAction *mLowButtonAction;
    QAction *mHighButtonAction;
    QAction *mLowEditAction;
    QAction *mHighEditAction;
    QAction *mAngleAction;
    QAction *mSizeAction;
};

class ZapGL : public QGLWidget
{
  Q_OBJECT

 public:
  ZapGL(struct zapwin *zap, QGLFormat format, QWidget * parent = 0);
  ~ZapGL() {};
  void setBufferSwapAuto(bool state) { setAutoBufferSwap(state); };
  bool extraCursorInWindow() {return (mMousePressed || mMouseInWindow);};
  struct zapwin *mZap;
 
protected:
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );
  void timerEvent(QTimerEvent *e);
  void leaveEvent ( QEvent * e);
  void enterEvent ( QEvent * e);

 private:
  bool mMousePressed;
  int mMouseInWindow;
  bool mFirstDraw;
  int mTimerID;
};

/*

$Log$
Revision 4.15  2008/05/27 22:48:35  mast
Moved angle to separate label after Z slider

Revision 4.14  2008/02/05 19:59:23  sueh
bug# 1065 Added a low section button and edit field and a high section button
and edit field.

Revision 4.13  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.12  2008/01/11 18:12:55  mast
Fixed event handlers for wheel, dropped GL handler as not needed

Revision 4.11  2007/12/04 18:48:20  mast
Added event handlers

Revision 4.10  2007/05/31 16:23:10  mast
Changes for using hot toolbar

Revision 4.9  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 4.8  2006/04/01 23:43:15  mast
Added size output to toolbar

Revision 4.7  2006/01/26 18:45:20  mast
Enabled swapping control

Revision 4.6  2005/03/29 00:59:57  mast
Added 2nd toolbar

Revision 4.5  2004/05/07 22:16:07  mast
Fixed array dimension problems caused by new toolbutton

Revision 4.4  2004/05/05 17:32:00  mast
Added rubberband definition and made mZap public

Revision 4.3  2003/03/26 06:30:56  mast
adjusting to font changes

Revision 4.2  2003/03/07 15:49:11  mast
Put z section slider under hot slider control

Revision 4.1  2003/02/10 20:41:56  mast
Merge Qt source

Revision 1.1.2.10  2003/01/30 06:17:05  mast
Allow range of slider to change

Revision 1.1.2.9  2003/01/30 00:48:53  mast
new timer logic

Revision 1.1.2.8  2003/01/10 23:56:56  mast
move some declarations out of slots

Revision 1.1.2.7  2002/12/17 17:30:22  mast
Adding timer for redraws

Revision 1.1.2.6  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 1.1.2.5  2002/12/13 07:09:19  mast
GLMainWindow needed different name for mouse event processors

Revision 1.1.2.4  2002/12/13 06:06:29  mast
using new glmainwindow and mainglwidget classes

Revision 1.1.2.3  2002/12/12 01:25:23  mast
added Z slider

Revision 1.1.2.2  2002/12/09 23:24:12  mast
*** empty log message ***

Revision 1.1.2.1  2002/12/09 17:48:09  mast
Initial addition to source

*/

#endif     // ZAP_CLASSES_H

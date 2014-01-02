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
 */

#ifndef ZAP_CLASSES_H
#define ZAP_CLASSES_H

#define NUM_TOOLBUTTONS 8
#define NUM_TIMEBUTTONS 1

#define ZAP_TOGGLE_RESOL 0
#define ZAP_TOGGLE_ZLOCK 1
#define ZAP_TOGGLE_CENTER 2
#define ZAP_TOGGLE_INSERT 3
#define ZAP_TOGGLE_RUBBER 4
#define ZAP_TOGGLE_LASSO 5
#define ZAP_TOGGLE_ARROW 6
#define ZAP_TOGGLE_TIMELOCK 7
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
class ZapFuncs;

class ZapGL;

class ZapWindow : public QMainWindow
{
  Q_OBJECT

 public:
  ZapWindow(ZapFuncs *zap, QString timeLabel, bool panels, bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, Qt::WFlags f = Qt::Window) ;
  ~ZapWindow();
  void setToggleState(int index, int state);
  void setZoomText(float zoom);
  void setSectionText(int section);
  void setMaxZ(int maxZ);
  void setTimeLabel(int time, QString label);
  void setSizeText(int winx, int winy);
  void setLowHighSectionState(int state);
  void setSizeAngleState();
  QString lowSection();
  QString highSection();

  ZapGL *mGLw;
  HotToolBar *mToolBar;
  HotToolBar *mToolBar2;
  HotToolBar *mPanelBar;
  ZapFuncs *mZap;

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
    void contextMenuHit(int val);
    void toolbarMenuEvent(QContextMenuEvent *e);

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void changeEvent(QEvent *e);
    void wheelEvent ( QWheelEvent * e);

 private:
    void setFontDependentWidths();

    QToolButton *mToggleButs[NUM_TOOLBUTTONS];
    int mToggleStates[NUM_TOOLBUTTONS];
    QAction *mToggleActs[NUM_TOOLBUTTONS];
    ToolEdit *mZoomEdit;
    ToolEdit *mSectionEdit;
    QLabel *mTimeLabel;
    QLabel *mTimeNumLabel;
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
  ZapGL(ZapFuncs *zap, QGLFormat format, QWidget * parent = 0);
  ~ZapGL() {};
  void setBufferSwapAuto(bool state) { setAutoBufferSwap(state); };
  bool extraCursorInWindow() {return (mMousePressed || mMouseInWindow);};
  void scheduleRedraw(int interval);
  void cancelRedraw();
  ZapFuncs *mZap;
 
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


#endif     // ZAP_CLASSES_H

/*  IMOD VERSION 2.7.9
 *
 *  zap_classes.h -- Header file for ZaP mainwindow and GLwidget classes.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$
Log at end of file
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


#include <qmainwindow.h>
#include <qgl.h>

class QToolButton;
class ToolEdit;
class QLabel;
class QToolBar;
class QSignalMapper;
class QSlider;
class QPushButton;

struct zapwin;
class ZapGL;

class ZapWindow : public QMainWindow
{
  Q_OBJECT

 public:
  ZapWindow(struct zapwin *zap, QString timeLabel, bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, 
	    WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~ZapWindow();
  void setToggleState(int index, int state);
  void setZoomText(float zoom);
  void setSectionText(int section);
  void setMaxZ(int maxZ);
  void setTimeLabel(QString label);

  ZapGL *mGLw;
  QToolBar *mToolBar;
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

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void fontChange(const QFont &oldFont) {setFontDependentWidths();};

 private:
    void setupToggleButton(QToolBar *toolBar, QSignalMapper *mapper, 
                           int index);
    void setFontDependentWidths();

    QToolButton *mToggleButs[NUM_TOOLBUTTONS];
    int mToggleStates[NUM_TOOLBUTTONS];
    ToolEdit *mZoomEdit;
    ToolEdit *mSectionEdit;
    QLabel *mTimeLabel;
    QSlider *mSecSlider;
    QPushButton *mInfoButton;
    QPushButton *mHelpButton;
    bool mSecPressed;
    int mDisplayedSection;
    bool mCtrlPressed;
};

class ZapGL : public QGLWidget
{
  Q_OBJECT

 public:
  ZapGL(struct zapwin *zap, QGLFormat format, QWidget * parent = 0,
        const char * name = 0);
  ~ZapGL() {};
 
protected:
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );
  void timerEvent(QTimerEvent *e);

 private:
  struct zapwin *mZap;
  bool mMousePressed;
  bool mFirstDraw;
  int mTimerID;
};

/*
$Log$
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

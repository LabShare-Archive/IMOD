/*   xgraph.h  -  declarations for xgraph.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.1  2003/01/10 23:59:59  mast
Seems to have gotten lost in repository

Revision 1.1.2.1  2003/01/06 15:48:55  mast
initila creation

*/
#ifndef XGRAPH_H
#define XGRAPH_H

#define MAX_GRAPH_TOGGLES 2

#include <qmainwindow.h>
#include <qgl.h>

class QToolButton;
class QLabel;
class QSignalMapper;
class QHBox;
class GraphGL;
class GraphWindow;
struct ViewInfo;

int  xgraphOpen(struct ViewInfo *vi);

typedef struct imod_xgraph_struct
{
  struct ViewInfo *vi;
  GraphWindow   *dialog;
  int    width, height;
  float  zoom;
  float *data;
  int    dsize;
  int    cpt;
  float  cx, cy, cz; /* current location. */
  int    co, cc, cp; /* current object, contour, point */
  int    axis;
  int    locked;
  int    highres;
  float  offset;
  float  scale;
  float  min, max;
  int    start;
  int    ctrl;
} GraphStruct;


class GraphWindow : public QMainWindow
{
  Q_OBJECT

 public:
  GraphWindow(GraphStruct *graph, bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, 
	    WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~GraphWindow() {};
  void setToggleState(int index, int state);

  GraphGL *mGLw;

  public slots:
    void zoomUp();
  void zoomDown();
  void help();
  void toggleClicked(int index);
  void axisSelected(int item);
  void xgraphDraw(GraphStruct *xg);
  void xgraphDrawAxis(GraphStruct *xg);
  void xgraphDrawPlot(GraphStruct *xg);
  void xgraphFillData(GraphStruct *xg);
  void externalKeyEvent ( QKeyEvent * e, int released);

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  
 private:
  void setupToggleButton(QHBox *toolBar, QSignalMapper *mapper, 
			 int index);
  
  GraphStruct *mGraph;
  QToolButton *mToggleButs[MAX_GRAPH_TOGGLES];
  int mToggleStates[MAX_GRAPH_TOGGLES];
  QLabel *mPlabel1;
  QLabel *mPlabel2;
  QLabel *mPlabel3;
  QLabel *mVlabel1;
  QLabel *mVlabel2;
};

class GraphGL : public QGLWidget
{
  Q_OBJECT

 public:
  GraphGL(GraphStruct *graph, QGLFormat format, QWidget * parent = 0,
        const char * name = 0);
  ~GraphGL() {};
  void setxyz(GraphStruct *xg, int mx, int my);
 
protected:
  void initializeGL() {};
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );

 private:
  GraphStruct *mGraph;
};
#endif     // XGRAPH_H

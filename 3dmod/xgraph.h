/*   xgraph.h  -  declarations for xgraph.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  No more Log
 */                                                                           

#ifndef XGRAPH_H
#define XGRAPH_H

#define MAX_GRAPH_TOGGLES 2

#include <qmainwindow.h>
#include <qgl.h>
//Added by qt3to4:
#include <QLabel>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QCloseEvent>

class QToolButton;
class QLabel;
class QSignalMapper;
class QHBoxLayout;
class QSpinBox;
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
  int    allocSize;
  int    cpt;
  float  cx, cy, cz; /* current location. */
  int    co, cc, cp; /* current object, contour, point */
  int    axis;
  int    subStart;
  int    locked;
  int    highres;
  int    nlines;
  float  offset;
  float  scale;
  float  min, max;
  float  mean;
  int    start;
  int    end;
  int    ctrl;
  int    closing;
} GraphStruct;


class GraphWindow : public QMainWindow
{
  Q_OBJECT

 public:
  GraphWindow(GraphStruct *graph, bool rgba, 
            bool doubleBuffer, bool enableDepth, QWidget * parent = 0,
            const char * name = 0, Qt::WFlags f = Qt::Window) ;
  ~GraphWindow() {};
  void setToggleState(int index, int state);

  GraphGL *mGLw;

  public slots:
    void zoomUp();
  void zoomDown();
  void help();
  void toggleClicked(int index);
  void axisSelected(int item);
  void xgraphDraw();
  void xgraphDrawAxis(GraphStruct *xg);
  void xgraphDrawPlot(GraphStruct *xg);
  void xgraphFillData(GraphStruct *xg);
  void widthChanged(int value);
  void externalKeyEvent ( QKeyEvent * e, int released);

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  
 private:
  int allocDataArray(int dsize);
  
  GraphStruct *mGraph;
  QToolButton *mToggleButs[MAX_GRAPH_TOGGLES];
  int mToggleStates[MAX_GRAPH_TOGGLES];
  QLabel *mPlabel1;
  QLabel *mPlabel2;
  QLabel *mPlabel3;
  QLabel *mVlabel1;
  QLabel *mVlabel2;
  QLabel *mMeanLabel;
  QSpinBox *mWidthBox;
};

class GraphGL : public QGLWidget
{
  Q_OBJECT

 public:
  GraphGL(GraphStruct *graph, QGLFormat format, QWidget * parent = 0);
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

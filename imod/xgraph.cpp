/*  IMOD VERSION 2.40
 *
 *  xgraph.c -- Imod graph window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <math.h>
#include <stdlib.h>
#include <qlabel.h>
#include <qvbox.h>
#include <qapplication.h>
#include <qhbox.h>
#include <qlayout.h>
#include <qbitmap.h>
#include <qcombobox.h>
#include <qsignalmapper.h>
#include <qtoolbutton.h>
#include <qfont.h>
#include <qtooltip.h>
#include <qpushbutton.h>
#include "arrowbutton.h"
#include "xgraph.h"

#include "imod.h"
#include "imod_input.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "control.h"
#include "preferences.h"


#include "lowres.bits"
#include "highres.bits"
#include "lock.bits"
#include "unlock.bits"

#define BM_WIDTH 16
#define BM_HEIGHT 16
#define XGRAPH_WIDTH 320
#define XGRAPH_HEIGHT 160
#define AUTO_RAISE true


static void graphClose_cb(ImodView *vi, void *client, int junk);
static void graphDraw_cb(ImodView *vi, void *client, int drawflag);
static void graphKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e);

static unsigned char *bitList[MAX_GRAPH_TOGGLES][2] =
  { {lowres_bits, highres_bits},
    {unlock_bits, lock_bits}};

static QBitmap *bitmaps[MAX_GRAPH_TOGGLES][2];
static int firstTime = 1;

enum {GRAPH_XAXIS, GRAPH_YAXIS, GRAPH_ZAXIS, GRAPH_CONTOUR, GRAPH_HISTOGRAM};

// Open a graph dialog
int xgraphOpen(struct ViewInfo *vi)
{
  GraphStruct *xg;

  xg = (GraphStruct *)malloc (sizeof(GraphStruct));
  if (!xg)
    return(-1);

  xg->vi      = vi;
  xg->axis    = 0;
  xg->zoom    = 1.0;
  xg->data    = NULL;
  xg->dsize   = 0;
  xg->locked  = 0;
  xg->highres = 0;

  xg->dialog = new GraphWindow(xg, App->rgba, App->doublebuffer,
			       App->qtEnableDepth, 
                               imodDialogManager.parent(IMOD_IMAGE),
                               "graph window");
  if (!xg->dialog){
    free(xg);
    wprint("Error opening graph window.");
    return(-1);
  }

  if (!App->rgba)
    xg->dialog->mGLw->setColormap(*(App->qColormap));

  xg->dialog->setCaption(imodCaption("3dmod Graph"));

  xg->ctrl = ivwNewControl (xg->vi, graphDraw_cb, graphClose_cb, graphKey_cb,
			    (void *)xg);
  imodDialogManager.add((QWidget *)xg->dialog, IMOD_IMAGE);
  xg->dialog->show();

  return(0);
}

// The close signal back from the controller
static void graphClose_cb(ImodView *vi, void *client, int junk)
{
  GraphStruct *xg = (GraphStruct *)client;
  xg->dialog->close();
}

// The draw signal from the controller
static void graphDraw_cb(ImodView *vi, void *client, int drawflag)
{
  GraphStruct *xg = (GraphStruct *)client;

  if (!xg) return;

  if (drawflag & IMOD_DRAW_COLORMAP) {
    xg->dialog->mGLw->setColormap(*(App->qColormap));
    return;
  }

  if (drawflag & IMOD_DRAW_XYZ){
    if (!xg->locked){
      if ((xg->cx != xg->vi->xmouse) ||
          (xg->cy != xg->vi->ymouse) ||
          (xg->cz != xg->vi->zmouse)){
        xg->dialog->xgraphDraw(xg);
        return;
      }
    }
  }

  if (drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE)){
    xg->dialog->xgraphDraw(xg);
    return;
  }

  if ((drawflag & IMOD_DRAW_MOD) && xg->axis == GRAPH_CONTOUR){
    xg->dialog->xgraphDraw(xg);
    }
  return;
}

static void graphKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e)
{
  GraphStruct *xg = (GraphStruct *)client;
  xg->dialog->externalKeyEvent (e, released);
}

/*
 * IMPLEMENTATION OF THE GraphWindow CLASS
 *
 * Constructor to build the window
 */
GraphWindow::GraphWindow(GraphStruct *graph, bool rgba,
            bool doubleBuffer, bool enableDepth, QWidget * parent,
            const char * name, WFlags f)
  : QMainWindow(parent, name, f)
{
  int j;
  ArrowButton *arrow;
  mGraph = graph;

  // Make central vbox and top frame containing an hbox
  QVBox *central = new QVBox(this, "central");
  setCentralWidget(central);
  QFrame * topFrame = new QFrame(central, "topFrame");
  topFrame->setFrameStyle(QFrame::Raised | QFrame::StyledPanel);

  // Life lessons!  The frame needs a layout inside it; just putting a box
  // in it does not do the trick, and it asserts no size
  QHBoxLayout *topLayout = new QHBoxLayout(topFrame, 4);
  QHBox *topBox = new QHBox(topFrame, "topBox");
  topLayout->addWidget(topBox);
  if (!(AUTO_RAISE))
      topBox->setSpacing(4);

  // Add the toolbar widgets
  // Zoom arrows
  arrow = new ArrowButton(Qt::UpArrow, topBox, "zoomup button");
  arrow->setAutoRaise(AUTO_RAISE);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  QToolTip::add(arrow, "Increase scale along the pixel axis");
  arrow = new ArrowButton(Qt::DownArrow, topBox, "zoom down button");
  arrow->setAutoRaise(AUTO_RAISE);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  QToolTip::add(arrow, "Decrease scale along the pixel axis");

  // Make the 2 toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(topBox);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < 2; j++)
    setupToggleButton(topBox, toggleMapper, j);

  QToolTip::add(mToggleButs[0], "Display file values instead of scaled bytes");
  QToolTip::add(mToggleButs[1], "Lock X/Y/Z position being displayed");

  // The axis combo box
  QComboBox *axisCombo = new QComboBox(topBox, "axis combo");
  axisCombo->insertItem("X-axis", GRAPH_XAXIS);
  axisCombo->insertItem("Y-axis", GRAPH_YAXIS);
  axisCombo->insertItem("Z-axis", GRAPH_ZAXIS);
  axisCombo->insertItem("Contour", GRAPH_CONTOUR);
  axisCombo->insertItem("Histogram", GRAPH_HISTOGRAM);
  axisCombo->setFocusPolicy(NoFocus);
  connect(axisCombo, SIGNAL(activated(int)), this, SLOT(axisSelected(int)));
  
  QToolTip::add(axisCombo, "Select axis to graph");

  /*
  // Help button
  QPushButton *pbutton = new QPushButton("Help", topBox, "Help button");
  int width = (int)(1.2 * pbutton->fontMetrics().width("Help"));
  pbutton->setFixedWidth(width);
  pbutton->setFocusPolicy(QWidget::NoFocus);
  connect(pbutton, SIGNAL(pressed()), this, SLOT(help()));
  */

  QHBox *topSpacer = new QHBox(topBox);
  topBox->setStretchFactor(topSpacer, 1);

  // Now a frame to put a grid layout in
  QFrame *botFrame = new QFrame(central, "botFrame");
  QGridLayout *layout = new QGridLayout(botFrame, 2, 2);
  QVBox *leftBox = new QVBox(botFrame, "leftBox");
  layout->addWidget(leftBox, 0, 0);
  QVBox *spacer = new QVBox(botFrame, "spacer");
  layout->addWidget(spacer, 1, 0);
  QHBox *botBox = new QHBox(botFrame, "botBox");
  layout->addWidget(botBox, 1, 1);
  layout->setRowStretch(0, 1);
  layout->setColStretch(1, 1);

  // A frame for the graph widget, and a layout inside it, and the GL widget
  QFrame *graphFrame = new QFrame(botFrame, "graphFrame");
  graphFrame->setFrameStyle(QFrame::Sunken | QFrame::StyledPanel);
  layout->addWidget(graphFrame, 0, 1);
  QVBoxLayout *graphLayout = new QVBoxLayout(graphFrame);
  graphLayout->setMargin(3);
  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new GraphGL(graph, glFormat, graphFrame);
  graphLayout->addWidget(mGLw);

  // Get a bigger font for the labels
  float font_scale = 1.25;
  QFont newFont = QApplication::font();
  float pointSize = newFont.pointSizeFloat();
  if (pointSize > 0) {
    newFont.setPointSizeFloat(pointSize * font_scale);
  } else {
    int pixelSize = newFont.pixelSize();
    newFont.setPixelSize((int)floor(pixelSize * font_scale + 0.5));
  }

  // Get the labels, give them the bigger font
  mPlabel1 = new QLabel(botBox);
  mPlabel1->setFont(newFont);
  mPlabel1->setAlignment(Qt::AlignLeft | Qt::AlignTop);
  mPlabel2 = new QLabel(botBox);
  mPlabel2->setFont(newFont);
  mPlabel2->setAlignment(Qt::AlignCenter | Qt::AlignTop);
  mPlabel3 = new QLabel(botBox);
  mPlabel3->setFont(newFont);
  mPlabel3->setAlignment(Qt::AlignRight | Qt::AlignTop);

  mVlabel1 = new QLabel("-88888", leftBox);
  mVlabel1->setFont(newFont);
  mVlabel1->setAlignment(Qt::AlignRight | Qt::AlignTop);
  mVlabel2 = new QLabel(leftBox);
  mVlabel2->setFont(newFont);
  mVlabel2->setAlignment(Qt::AlignRight | Qt::AlignBottom);

  QSize hint = mVlabel1->sizeHint();
  leftBox->setMinimumWidth(hint.width() + 5);

  resize(XGRAPH_WIDTH, XGRAPH_HEIGHT);
  setFocusPolicy(QWidget::StrongFocus);
}

// Set up one toggle button, making bitmaps for the two state
void GraphWindow::setupToggleButton(QHBox *toolBar, QSignalMapper *mapper,
				    int ind)
{
  if (firstTime) {
    bitmaps[ind][0] = new QBitmap(BM_WIDTH, BM_HEIGHT, bitList[ind][0], true);
    bitmaps[ind][1] = new QBitmap(BM_WIDTH, BM_HEIGHT, bitList[ind][1], true);
  }
  mToggleButs[ind] = new QToolButton(toolBar, "toolbar toggle");
  mToggleButs[ind]->setPixmap(*bitmaps[ind][0]);
  mToggleButs[ind]->setAutoRaise(AUTO_RAISE);
  mapper->setMapping(mToggleButs[ind],ind);
  connect(mToggleButs[ind], SIGNAL(clicked()), mapper, SLOT(map()));
  mToggleStates[ind] = 0;
}

/* 
 * SLOTS FOR GRAPHWINDOW
 *
 * Zoom up and down
 */
void GraphWindow::zoomUp()
{
  mGraph->zoom = b3dStepPixelZoom(mGraph->zoom, 1);
  xgraphDraw(mGraph);
}

void GraphWindow::zoomDown()
{
  mGraph->zoom = b3dStepPixelZoom(mGraph->zoom, -1);
  xgraphDraw(mGraph);
}

void GraphWindow::help()
{

}

// Toggle button
void GraphWindow::toggleClicked(int index)
{
  int state = 1 - mToggleStates[index];
  mToggleStates[index] = state;
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
  if (!index) {

    // High res button toggled
    mGraph->highres = state;
    xgraphDraw(mGraph);
  } else {

    // Lock button toggled: draw if unlocking
    mGraph->locked = state;
    if (!state)
      xgraphDraw(mGraph);
  }
}

// Axis selection
void GraphWindow::axisSelected(int item)
{
  mGraph->axis = item;
  xgraphDraw(mGraph);
}

// For the program to set toggle states
void GraphWindow::setToggleState(int index, int state)
{
  mToggleStates[index] = state ? 1 : 0;
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
}

/*
 * EVENT RESPONSES
 *
 * Key press: close on Escape, zoom up or down on =/-, and pass on others
 */
void GraphWindow::keyPressEvent ( QKeyEvent * e )
{
  ivwControlPriority(mGraph->vi, mGraph->ctrl);
 int key = e->key();
  if (key == Qt::Key_Escape)
    close();

  else if (key == Qt::Key_Equal)
    zoomUp();

  else if (key == Qt::Key_Minus)
    zoomDown();

  else
    inputQDefaultKeys(e, mGraph->vi);
}

// Pass on a key press to event processor
void GraphWindow::externalKeyEvent ( QKeyEvent * e, int released)
{
  if (!released)
    keyPressEvent(e);
}

// When close event comes in, clean up and accept the event
void GraphWindow::closeEvent ( QCloseEvent * e )
{
  ivwRemoveControl(mGraph->vi, mGraph->ctrl);
  imodDialogManager.remove((QWidget *)this);
  if (mGraph->data)
    free(mGraph->data);
  free(mGraph);
  e->accept();
}

/*
 * DRAWING ROUTINES 
 *
 * The called drawing routine just calls an update on the GL widget
 */
void GraphWindow::xgraphDraw(GraphStruct *xg)
{
  mGLw->updateGL();
}

// Fill the data structure for drawing
void GraphWindow::xgraphFillData(GraphStruct *xg)
{
  int dsize, xsize;
  unsigned char **image = NULL;
  int cx, cy, cz, i, j;
  int co, cc, cp;
  Icont *cont;
  Ipoint *pt1, *pt2;
  int   pmax, pt;
  int curpt;
  float frac, totlen, curint;
  Ipoint scale;

  if (!xg->vi)
    return;

  if (! (xg->data))
    xg->dsize = 0;

  xg->cx = xg->vi->xmouse;
  cx = (int)xg->cx;
  xg->cy = xg->vi->ymouse;
  cy = (int)xg->cy;
  xg->cz = xg->vi->zmouse;
  cz = (int)(xg->cz + 0.5);

  switch(xg->axis){
  case GRAPH_XAXIS:
    image = ivwGetCurrentZSection(xg->vi);
    dsize = xg->vi->xsize;
    if (dsize > xg->dsize){
      if (xg->data)    free(xg->data);
      xg->data  = (float *)malloc(dsize * sizeof(float));
      if (!xg->data) return;
    }
    xg->dsize = dsize;
    for(i = 0; i < dsize; i++) 
      xg->data[i] = 0.0f;
    cz = (int)(xg->vi->zmouse + 0.5f);
    cy = (int)(xg->vi->ymouse + 0.5f);
    xg->cpt = (int)xg->cx;

    /* DNM: skip out if outside limits */
    if (cz < 0 || cz >= xg->vi->zsize || cy < 0 || cy >= xg->vi->ysize)
      break;
    if (xg->highres)
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetFileValue(xg->vi, i, cy, cz);
    else
      for(i = 0; i < dsize; i++)
        if (image)
          xg->data[i] = image[cy][i];
        else
          xg->data[i] = xg->vi->hdr->amean;
    break;


  case GRAPH_YAXIS:
    image = ivwGetCurrentZSection(xg->vi);
    dsize = xg->vi->ysize;
    if (dsize > xg->dsize){
      if (xg->data)    free(xg->data);
      xg->data  = (float *)malloc(dsize * sizeof(float));
      if (!xg->data) return;
    }
    xg->dsize = dsize;
    for(i = 0; i < dsize; i++)
      xg->data[i] = 0.0f;
    cz = (int)(xg->vi->zmouse + 0.5f);
    cx = (int)(xg->vi->xmouse + 0.5f);
    xg->cpt = (int)xg->cy;

    /* DNM: skip out if outside limits */
    if (cx < 0 || cx >= xg->vi->xsize || cz < 0 || cz >= xg->vi->zsize)
      break;
    if (xg->highres)
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetFileValue(xg->vi, cx, i, cz);
    else
      for(i = 0; i < dsize; i++){
        if (image)
          xg->data[i] = image[i][cx];
        else
          xg->data[i] = xg->vi->hdr->amean;
      }
    break;

  case GRAPH_ZAXIS:
    dsize = xg->vi->zsize;
    if (dsize > xg->dsize){
      if (xg->data)
        free(xg->data);
      xg->data  = (float *)malloc(dsize * sizeof(float));
      if (!xg->data) return;
    }
    xg->dsize = dsize;
    for(i = 0; i < dsize; i++) 
      xg->data[i] = 0.0f;
    cx = (int)(xg->vi->xmouse + 0.5f);
    cy = (int)(xg->cy + 0.5f);
    xg->cpt = (int)xg->cz;

    /* DNM: skip out if outside limits */
    if (cx < 0 || cx >= xg->vi->xsize || cy < 0 || cy >= xg->vi->ysize)
      break;
    if (xg->highres)
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetFileValue(xg->vi, cx, cy, i);
    else
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetValue(xg->vi, cx, cy, i);
    break;


    /* Contour : DNM got this working properly, and in 3D */
  case GRAPH_CONTOUR:
    co = xg->co = xg->vi->imod->cindex.object;
    cc = xg->cc = xg->vi->imod->cindex.contour;
    cp = xg->cp = xg->vi->imod->cindex.point;
    cont = imodContourGet(xg->vi->imod);
    if (!cont) return;
    if (cont->psize < 2) return;

    /* Get true 3D length, record where current point falls */

    totlen = 0;
    scale.x = 1.0;
    scale.y = 1.0;
    scale.z = 1.0;
    if (cp < 0) 
      cp = 0;
    if (cp >= (int)cont->psize) 
      cp = (int)cont->psize - 1;
    xg->cpt = 0;
    for (i = 1; i < (int)cont->psize; i++) {
      totlen += imodPoint3DScaleDistance(&cont->pts[i-1],
                                         &cont->pts[i], &scale);
      if (i == cp)
        xg->cpt = (int)(totlen + 0.5);
    }

    dsize = (int)(totlen + 1.0);
    if (dsize > xg->dsize){
      if (xg->data)    free(xg->data);
      xg->data  = (float *)malloc(dsize * sizeof(float));
      if (!xg->data) return;
    }
    xg->dsize = dsize;
    for(i = 0; i < dsize; i++)
      xg->data[i] = 0.0f;
    xg->cx = cont->pts[cp].x + 0.5f;
    xg->cy = cont->pts[cp].y + 0.5f;
    xg->cz = cont->pts[cp].z + 0.5f;
    cx = (int)(cont->pts->x + 0.5f);
    cy = (int)(cont->pts->y + 0.5f);
    cz = (int)(cont->pts->z + 0.5f);
    if (xg->highres)
      xg->data[0] = ivwGetFileValue(xg->vi, cx, cy, cz);
    else if (cx >= 0 && cx < xg->vi->xsize &&
             cy >= 0 && cy < xg->vi->ysize &&
             cz >= 0 && cz < xg->vi->zsize)
      xg->data[0] = ivwGetValue(xg->vi, cx, cy, cz);

    /* Advance through data points, finding nearest pixel along each line
       of contour */

    totlen = 0.0;
    pt2 = cont->pts;
    curint = 0.0;
    curpt = 1;
    for (i = 1; i < dsize; i++) {

      /* Advance as needed until i is inside the current interval */

      while (i > totlen + curint) {
        totlen += curint;
        pt1 = pt2;
        pt2 = &cont->pts[curpt++];
        curint = imodPoint3DScaleDistance(pt1, pt2, &scale);
	if (curpt >= (int)cont->psize)
	  break;
      }
      frac = 0;
      if (curint)
        frac = (i - totlen) / curint;
      cx = (int)(pt1->x + frac * (pt2->x - pt1->x) + 0.5);
      cy = (int)(pt1->y + frac * (pt2->y - pt1->y) + 0.5);
      cz = (int)(pt1->z + frac * (pt2->z - pt1->z) + 0.5);
      if (xg->highres)
        xg->data[i] = ivwGetFileValue(xg->vi, cx, cy, cz);
      else if (cx >= 0 && cx < xg->vi->xsize &&
               cy >= 0 && cy < xg->vi->ysize &&
               cz >= 0 && cz < xg->vi->zsize)
        xg->data[i] = ivwGetValue(xg->vi, cx, cy, cz);
    }

    break;

  case GRAPH_HISTOGRAM:
    image = ivwGetCurrentZSection(xg->vi);
    if (image){
      dsize = 256;
      if (xg->dsize < dsize){
        if (xg->data)
          free(xg->data);
        xg->data  = (float *)malloc(dsize * sizeof(float));
        if (!xg->data) 
          return;
      }
      xg->dsize = dsize;
      for(i = 0; i < dsize; i++)
        xg->data[i] = 0.0f;
      cz = (int)(xg->vi->zmouse + 0.5f);
      for (j = 0; j < xg->vi->ysize; j++) {
        for(i = 0; i < xg->vi->xsize; i++){
          pt = image[j][i];
          xg->data[pt] += 1.0f;
        }
      }
      xg->cpt = image[cy][cx];
    }
    break;

  default:
    break;
  }
  return;
}

// Set the text of the Axis labels
void GraphWindow::xgraphDrawAxis(GraphStruct *xg)
{
  QString str;

  // Output integers on the value axis unless we are in high res mode and 
  // the file is not byte or integer
  int floats = !xg->highres || xg->vi->image->mode == MRC_MODE_BYTE || 
    xg->vi->image->mode == MRC_MODE_SHORT ? 0 : 1;

  if (floats)
    str.sprintf("%9g", xg->min);
  else
    str.sprintf("%6d", (int)xg->min);
  mVlabel2->setText(str);

  if (floats)
    str.sprintf("%9g", xg->max);
  else
    str.sprintf("%6d", (int)xg->max);
  mVlabel1->setText(str);

  // Output pixel position axis
  str.sprintf("%d", xg->start);
  mPlabel1->setText(str);
  str.sprintf("%d", xg->cpt);
  mPlabel2->setText(str);
  str.sprintf("%d", xg->cpt + (xg->cpt-xg->start));
  mPlabel3->setText(str);
}

// Actually draw the data
void GraphWindow::xgraphDrawPlot(GraphStruct *xg)
{
  int spnt, epnt, i;
  int cpntx = xg->width/2;
  float min, max, yoffset, yscale;
  float zoom = xg->zoom;

  if (!xg->data)
    return;

  spnt = xg->cpt - (int)(cpntx / zoom);
  epnt = xg->cpt + (int)(cpntx / zoom);

  b3dColorIndex(App->foreground);
  min = max = xg->data[xg->cpt];
  for(i = spnt; i < epnt; i++){
    if (i < 0)
      continue;
    if (i >= xg->dsize - 2)
      break;
    if (xg->data[i] < min)
      min = xg->data[i];
    if (xg->data[i] > max)
      max = xg->data[i];
  }
  xg->min = min;
  xg->max = max;
  yoffset = min;
  if (max-min)
    yscale = xg->height/(max - min);
  else yscale = 1.0f;

  b3dBeginLine();
  for(i = spnt; (i <= epnt) && (i < xg->dsize); i++){
    if (i < 0)
      continue;
    b3dVertex2i((int)((i - spnt) * zoom),
                (int)((xg->data[i] - yoffset) * yscale));
  }
  b3dEndLine();

  b3dColorIndex(App->endpoint);
  b3dDrawLine((int)((xg->cpt - spnt) * zoom), 0,
              (int)((xg->cpt - spnt) * zoom), xg->height);

  xg->offset = yoffset;
  xg->scale  = yscale;
  xg->start  = spnt;
  return;
}


/*
 * The GL WIDGET CLASS: PAINT, RESIZE, MOUSE EVENTS
 */
GraphGL::GraphGL(GraphStruct *graph, QGLFormat format, QWidget * parent,
		 const char * name)
  : QGLWidget(format, parent, name)
{
  mGraph = graph;
}

void GraphGL::paintGL()
{
  GraphStruct *xg = mGraph;
  if (!xg->locked)
    xg->dialog->xgraphFillData(xg);

  if (!xg->data)
    return;
  b3dColorIndex(App->background);
  glClear(GL_COLOR_BUFFER_BIT);
  xg->dialog->xgraphDrawPlot(xg);
  xg->dialog->xgraphDrawAxis(xg);
}

void GraphGL::resizeGL( int wdth, int hght )
{
  mGraph->width  = wdth;
  mGraph->height = hght;
  b3dResizeViewportXY(wdth, hght);
}

void GraphGL::mousePressEvent(QMouseEvent * e )
{
  ivwControlPriority(mGraph->vi, mGraph->ctrl);
  if (e->stateAfter() & ImodPrefs->actualButton(1))
    setxyz(mGraph, e->x(), e->y());
}

// Routine to change the position on the current axis based upon a mouse click
void GraphGL::setxyz(GraphStruct *xg, int mx, int my)
{
  int ni = (int)((mx/xg->zoom) + xg->start);
  int x,y,z;

  ivwGetLocation(xg->vi, &x, &y, &z);

  switch(xg->axis){
  case GRAPH_XAXIS:
    x = ni;
    break;
  case GRAPH_YAXIS:
    y = ni;
    break;
  case GRAPH_ZAXIS:
    z = ni;
    break;

  case GRAPH_CONTOUR:
    return;
  case GRAPH_HISTOGRAM:
    return;
  }
  ivwSetLocation(xg->vi, x, y, z);
  ivwControlPriority(xg->vi, xg->ctrl);
  imodDraw(xg->vi, IMOD_DRAW_XYZ);
  return;
}

/*
    $Log$
    Revision 4.4  2003/04/25 03:28:33  mast
    Changes for name change to 3dmod

    Revision 4.3  2003/04/17 18:43:38  mast
    adding parent to window creation

    Revision 4.2  2003/03/24 17:56:46  mast
    Register with dialogManager so it can be parked with info window

    Revision 4.1  2003/02/10 20:29:02  mast
    autox.cpp

    Revision 1.1.2.4  2003/01/29 01:34:31  mast
    implement colormaps

    Revision 1.1.2.3  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 1.1.2.2  2003/01/10 23:41:28  mast
    clean up warnings

    Revision 1.1.2.1  2003/01/02 15:45:09  mast
    changes for new controller key callback

    Revision 3.1.2.1  2002/12/19 04:37:13  mast
    Cleanup of unused global variables and defines

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/

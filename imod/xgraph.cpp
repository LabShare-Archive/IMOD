/*  
 *  xgraph.c -- Imod graph window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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
#include <qspinbox.h>
#include <qtooltip.h>
#include <qpushbutton.h>
#include "arrowbutton.h"
#include "xgraph.h"

#include "imod.h"
#include "xzap.h"
#include "imod_input.h"
#include "imod_edit.h"
#include "imod_info_cb.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "control.h"
#include "preferences.h"
#include "win_support.h"

#include "lowres.bits"
#include "highres.bits"
#include "lock.bits"
#include "unlock.bits"

#define XGRAPH_WIDTH 320
#define XGRAPH_HEIGHT 160
#define AUTO_RAISE true


static void graphClose_cb(ImodView *vi, void *client, int junk);
static void graphDraw_cb(ImodView *vi, void *client, int drawflag);
static void graphKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e);
static void makeBoundaryPoint(Ipoint pt1, Ipoint pt2, int ix1, int ix2,
                              int iy1, int iy2, Ipoint *newpt);

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
  xg->allocSize = 0;
  xg->locked  = 0;
  xg->highres = 0;
  xg->nlines = 1;
  xg->mean = 0.;
  xg->closing = 0;

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
  imodDialogManager.add((QWidget *)xg->dialog, IMOD_IMAGE, GRAPH_WINDOW_TYPE);

  imod_info_input();
  QSize size = xg->dialog->sizeHint();
  xg->dialog->resize(size.width(), (int)(0.65 * size.width()));
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

  if (!xg) 
    return;

  if (drawflag & IMOD_DRAW_COLORMAP) {
    xg->dialog->mGLw->setColormap(*(App->qColormap));
    return;
  }

  if (drawflag & IMOD_DRAW_XYZ){
    if (!xg->locked){
      if ((xg->cx != xg->vi->xmouse) ||
          (xg->cy != xg->vi->ymouse) ||
          (xg->cz != xg->vi->zmouse)){
        xg->dialog->xgraphDraw();
        return;
      }
    }
  }

  if (drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE)){
    xg->dialog->xgraphDraw();
    return;
  }

  if ((drawflag & IMOD_DRAW_MOD) && xg->axis == GRAPH_CONTOUR){
    xg->dialog->xgraphDraw();
  }
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

  QLabel *label = new QLabel("Width", topBox);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  mWidthBox = new QSpinBox(1, 100, 1, topBox);
  mWidthBox->setValue(1);
  mWidthBox->setFocusPolicy(QWidget::ClickFocus);
  connect(mWidthBox, SIGNAL(valueChanged(int)), this, SLOT(widthChanged(int)));
  QToolTip::add(mWidthBox, "Set number of lines to average over");

  mMeanLabel = new QLabel(" 0.0000", topBox);
  
  // Help button
  QPushButton *pbutton = new QPushButton("Help", topBox, "Help button");
  int width = (int)(1.2 * pbutton->fontMetrics().width("Help"));
  pbutton->setFixedWidth(width);
  pbutton->setFocusPolicy(QWidget::NoFocus);
  connect(pbutton, SIGNAL(pressed()), this, SLOT(help()));

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
  xgraphDraw();
}

void GraphWindow::zoomDown()
{
  mGraph->zoom = b3dStepPixelZoom(mGraph->zoom, -1);
  xgraphDraw();
}

void GraphWindow::help()
{
  imodShowHelpPage("graph.html");
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
    xgraphDraw();
  } else {

    // Lock button toggled: draw if unlocking
    mGraph->locked = state;
    if (!state)
      xgraphDraw();
  }
}

// Axis selection
void GraphWindow::axisSelected(int item)
{
  mGraph->axis = item;
  mWidthBox->setEnabled(item != GRAPH_ZAXIS && item != GRAPH_HISTOGRAM);
  xgraphDraw();
}

// For the program to set toggle states
void GraphWindow::setToggleState(int index, int state)
{
  mToggleStates[index] = state ? 1 : 0;
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
}

// Width change
void GraphWindow::widthChanged(int value)
{
  if (mGraph->closing)
    return;
  mGraph->nlines = value;
  xgraphDraw();
  setFocus();
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
  mGraph->closing = 1;
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
void GraphWindow::xgraphDraw()
{
  mGLw->updateGL();
}

// Allocate or reallocate the data array to the correct size and zero it out
int GraphWindow::allocDataArray(int dsize)
{
  int i;
  if (dsize > mGraph->allocSize) {
    if (mGraph->data)
      free(mGraph->data);
    mGraph->data  = (float *)malloc(dsize * sizeof(float));
    if (!mGraph->data) 
      return 1;
    mGraph->allocSize = dsize;
  }
  mGraph->dsize = dsize;
  for (i = 0; i < dsize; i++)
    mGraph->data[i] = 0.0f;
  return 0;
}

// Fill the data structure for drawing
void GraphWindow::xgraphFillData(GraphStruct *xg)
{
  int dsize;
  unsigned char **image = NULL;
  int cx, cy, cz, i, j, jy, nlines;
  int ixStart, iyStart, nxUse, nyUse, ixEnd, iyEnd;
  int cp;
  Icont *cont;
  Ipoint *pt1, *pt2, *pts;
  int   pt;
  int curpt, vecpt, istr, iend, skipStart, skipEnd;
  float frac, totlen, curint, dx, dy, smin, smax;
  Ipoint scale, startPt, endPt;
  double sum, lensq;
  ImodView *vi = xg->vi;

  if (!vi)
    return;

  if (! (xg->data))
    xg->allocSize = 0;

  xg->cx = vi->xmouse;
  cx = (int)(xg->cx + 0.5);
  xg->cy = vi->ymouse;
  cy = (int)(xg->cy + 0.5);
  xg->cz = vi->zmouse;
  cz = (int)(xg->cz + 0.5);

  ixStart = 0;
  iyStart = 0;
  nxUse = vi->xsize;
  nyUse = vi->ysize;
  zapSubsetLimits(vi, ixStart, iyStart, nxUse, nyUse);

  switch(xg->axis){
  case GRAPH_XAXIS:
    image = ivwGetCurrentZSection(vi);
    dsize = nxUse;
    xg->subStart = ixStart;
    if (allocDataArray(dsize))
      return;

    /* DNM: skip out if outside limits */
    if (cz < 0 || cz >= vi->zsize || cy < 0 || cy >= vi->ysize)
      break;
    if (!image)
      break;

    if (cx < ixStart || cx > ixStart + nxUse - 1) {
      cx = xg->cx = vi->xmouse = 
        B3DMIN(ixStart + nxUse - 1, B3DMAX(ixStart, cx));
      imodDraw(vi, IMOD_DRAW_XYZ);
    }
    xg->cpt = cx;
    
    nlines = 0;
    for (j = 0; j < xg->nlines; j++) {
      jy = cy + j - (xg->nlines - 1) / 2;
      if (jy < 0 || jy >= vi->ysize)
        continue;
      nlines++;
      if (xg->highres)
        for(i = 0; i < dsize; i++)
          xg->data[i] += ivwGetFileValue(vi, i + ixStart, jy, cz);
      else
        for(i = 0; i < dsize; i++)
          xg->data[i] += image[jy][i + ixStart];
    }
    if (nlines > 1)
      for(i = 0; i < dsize; i++)
        xg->data[i] /= nlines;
    break;


  case GRAPH_YAXIS:
    image = ivwGetCurrentZSection(vi);
    dsize = nyUse;
    xg->subStart = iyStart;
    if (allocDataArray(dsize))
      return;

    /* DNM: skip out if outside limits */
    if (cx < 0 || cx >= vi->xsize || cz < 0 || cz >= vi->zsize)
      break;
    if (!image)
      break;

    if (cy < iyStart || cy > iyStart + nyUse - 1) {
      cy = xg->cy = vi->ymouse = 
        B3DMIN(iyStart + nyUse - 1, B3DMAX(iyStart, cy));
      imodDraw(vi, IMOD_DRAW_XYZ);
    }
    xg->cpt = cy;

    nlines = 0;
    for (j = 0; j < xg->nlines; j++) {
      jy = cx + j - (xg->nlines - 1) / 2;
      if (jy < 0 || jy >= vi->xsize)
        continue;
      nlines++;
      if (xg->highres)
        for (i = 0; i < dsize; i++)
          xg->data[i] += ivwGetFileValue(vi, jy, i + iyStart, cz);
      else
        for (i = 0; i < dsize; i++)
          xg->data[i] += image[i + iyStart][jy];
    }
    if (nlines > 1)
      for(i = 0; i < dsize; i++)
        xg->data[i] /= nlines;
    break;

  case GRAPH_ZAXIS:
    dsize = vi->zsize;
    if (allocDataArray(dsize))
      return;
    xg->subStart = 0;
    xg->cpt = cz;

    /* DNM: skip out if outside limits */
    if (cx < 0 || cx >= vi->xsize || cy < 0 || cy >= vi->ysize)
      break;
    if (xg->highres)
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetFileValue(vi, cx, cy, i);
    else
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetValue(vi, cx, cy, i);
    break;


    /* Contour : DNM got this working properly, and in 3D */
  case GRAPH_CONTOUR:
    cp = xg->cp = vi->imod->cindex.point;
    cont = imodContourGet(vi->imod);
    if (!cont)
      return;
    if (cont->psize < 2) 
      return;
    pts = cont->pts;

    // Analyze for whether contour crosses into and out of a subarea
    skipStart = 0;
    skipEnd = 0;
    totlen = 0.;
    scale.x = 1.0;
    scale.y = 1.0;
    scale.z = 1.0;
    if (nxUse < vi->xsize || nyUse < vi->ysize) {
      ixEnd = ixStart + nxUse - 1;
      iyEnd = iyStart + nyUse - 1;
      
      // Find first point inside box
      for (i = 0; i < cont->psize; i++) {
        if (pts[i].x >= ixStart && pts[i].x <= ixEnd && 
            pts[i].y >= iyStart && pts[i].y <= iyEnd) {
          if (!i)
            break;
          skipStart = i;
          makeBoundaryPoint(pts[i - 1], pts[i], ixStart, ixEnd, iyStart, iyEnd,
                            &startPt);
          totlen += imodPoint3DScaleDistance(&pts[i - 1], &startPt, &scale);
          break;
        }
        if (i)
          totlen += imodPoint3DScaleDistance(&pts[i - 1], &pts[i], &scale);
      }
      

      // If completely outside, skip
      if (i >= cont->psize)
        return;

      // Now search for first point outside box
      for (i++; i < cont->psize; i++) {
        if (!(pts[i].x >= ixStart && pts[i].x <= ixEnd && 
              pts[i].y >= iyStart && pts[i].y <= iyEnd)) {
          skipEnd = i;
          makeBoundaryPoint(pts[i], pts[i - 1], ixStart, ixEnd, iyStart, iyEnd,
                            &endPt);
          break;
        }
      }
    }

    /* Get true 3D length, record where current point falls */
    xg->subStart = (int)(totlen + 0.5);
    totlen = 0.;
    if (cp < 0) 
      cp = 0;
    if (cp >= (int)cont->psize) 
      cp = (int)cont->psize - 1;
    xg->cpt = xg->subStart;

    pt1 = cont->pts;
    if (skipStart)
      pt1 = &startPt;
    istr = skipStart ? skipStart : 1;
    iend = skipEnd ? skipEnd + 1 : cont->psize;
    for (i = istr; i < iend; i++) {
      pt2 = &cont->pts[i];
      if (i == skipEnd)
        pt2 = &endPt;
      totlen += imodPoint3DScaleDistance(pt1, pt2, &scale);
      if (i == cp)
        xg->cpt = (int)(totlen + 0.5) + xg->subStart;
      pt1 = pt2;
    }
    if (cp >= iend)
      xg->cpt = (int)(totlen + 0.5) + xg->subStart;

    dsize = (int)(totlen + 1.0);
    if (allocDataArray(dsize))
      return;
    xg->cx = cont->pts[cp].x + 0.5f;
    xg->cy = cont->pts[cp].y + 0.5f;
    xg->cz = cont->pts[cp].z + 0.5f;

    /* Advance through data points, finding nearest pixel along each line
       of contour */

    totlen = 0.0;
    curint = 0.0;
    curpt = istr;
    vecpt = 0;
    dx = 0.;
    dy = 0.;
    pt2 = cont->pts;
    if (skipStart)
      pt2 = &startPt;
    for (i = 0; i < dsize; i++) {

      /* Advance as needed until i is inside the current interval */

      while (i > totlen + curint || !curint) {
        totlen += curint;
        pt1 = pt2;
        pt2 = &cont->pts[curpt++];
        if (curpt == skipEnd + 1)
          pt2 = &endPt;
        curint = imodPoint3DScaleDistance(pt1, pt2, &scale);
	if (curpt >= iend)
	  break;
      }
      frac = 0;
      if (curint)
        frac = (i - totlen) / curint;

      // Compute delta for cross-averaging
      if (xg->nlines > 1 && curpt != vecpt) {
        dx = pt1->y - pt2->y;
        dy = pt2->x - pt1->x;
        lensq = dx * dx + dy * dy;
        if (lensq > 1.e-3) {
          lensq = sqrt(lensq);
          dx /= lensq;
          dy /= lensq;
        } else {
          dx = 0.;
          dy = 1.;
        }
        vecpt = curpt;
      }

      nlines = 0;
      for (jy = -(xg->nlines - 1) / 2 ; jy < xg->nlines - (xg->nlines - 1) / 2;
           jy++) {
        cx = (int)(pt1->x + frac * (pt2->x - pt1->x) + jy * dx + 0.5);
        cy = (int)(pt1->y + frac * (pt2->y - pt1->y) + jy * dy + 0.5);
        cz = (int)(pt1->z + frac * (pt2->z - pt1->z) + 0.5);
        if (cx >= 0 && cx < vi->xsize &&
            cy >= 0 && cy < vi->ysize &&
            cz >= 0 && cz < vi->zsize) {
          nlines++;
          if (xg->highres)
            xg->data[i] += ivwGetFileValue(vi, cx, cy, cz);
          else
            xg->data[i] += ivwGetValue(vi, cx, cy, cz);
        }
      }
      if (nlines)
        xg->data[i] /= nlines;
    }

    break;

  case GRAPH_HISTOGRAM:
    image = ivwGetCurrentZSection(vi);
    if (image){
      dsize = 256;
      sum = 0.;
      if (allocDataArray(dsize))
        return;
      xg->subStart = 0;
      cz = (int)(vi->zmouse + 0.5f);
      for (j = iyStart; j < iyStart + nyUse; j++) {
        for (i = ixStart; i < ixStart + nxUse; i++) {
          pt = image[j][i];
          xg->data[pt] += 1.0f;
          sum += pt;
        }
      }
      xg->cpt = image[cy][cx];

      // For high res, too painful to get file mean
      /*if (xg->highres) {
        sum = 0.;
        for (j = iyStart; j < iyStart + nyUse; j++)
          for (i = ixStart; i < ixStart + nxUse; i++)
            sum += ivwGetFileValue(vi, i, j, cz);
            } */

      // For high res, rescale the sum by the load in scaling, ignoring
      // truncation (ignoring outmin/outmax ...)
      if (xg->highres) {
        smin = vi->image->smin;
        smax = vi->image->smax;
        if (vi->multiFileZ > 0) {
          smin = vi->imageList[cz + vi->li->zmin].smin;
          smax = vi->imageList[cz + vi->li->zmin].smax;
        }
        if (smin != smax) 
          sum  = sum * (smax - smin) / 255. + smin;
      }
      xg->mean = sum / (nxUse * nyUse);
    }
    break;

  default:
    break;
  }

  if (xg->axis != GRAPH_HISTOGRAM) {
    sum = 0.;
    for (i = 0; i < dsize; i++)
      sum += xg->data[i];
    xg->mean = sum / dsize;
  }
}

// Set the text of the Axis labels
void GraphWindow::xgraphDrawAxis(GraphStruct *xg)
{
  QString str;

  // Output integers on the value axis unless we are in high res mode and 
  // the file is not byte or integer
  int mode = xg->vi->image->mode;
  int floats = !xg->highres || mode == MRC_MODE_BYTE || 
    mode == MRC_MODE_SHORT || mode == MRC_MODE_USHORT ? 0 : 1;

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
  str.sprintf(" %.5g", xg->mean);
  mMeanLabel->setText(str);
}

// Actually draw the data
void GraphWindow::xgraphDrawPlot(GraphStruct *xg)
{
  int spnt, epnt, i;
  int cpntx = xg->width/2;
  float min, max, yoffset, yscale, extra;
  float zoom = xg->zoom;

  if (!xg->data)
    return;

  spnt = xg->cpt - (int)(cpntx / zoom);
  epnt = xg->cpt + (int)(cpntx / zoom);

  b3dColorIndex(App->foreground);
  min = max = xg->data[xg->cpt - xg->subStart];
  for(i = spnt - xg->subStart; i < epnt - xg->subStart; i++){
    if (i < 0)
      continue;
    if (i >= xg->dsize - 2)
      break;
    if (xg->data[i] < min)
      min = xg->data[i];
    if (xg->data[i] > max)
      max = xg->data[i];
  }

  // Increase range a bit but keep integer values and keep a min of 0
  extra = 0.02 * (max - min);
  if (extra != (int)extra && (max == (int)max || min == (int)min))
    extra = extra > 0.2 ? (int)(extra + 1.) : 0;
  if (min != 0.)
    min -= extra;
  max += extra;

  xg->min = min;
  xg->max = max;
  yoffset = min;
  if (max-min)
    yscale = xg->height/(max - min);
  else yscale = 1.0f;

  b3dBeginLine();
  for (i = spnt - xg->subStart; (i <= epnt - xg->subStart) && (i < xg->dsize);
      i++) {
    if (i < 0)
      continue;
    b3dVertex2i((int)((i  + xg->subStart - spnt) * zoom),
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
  static bool drawing = false;

  if (drawing)
    return;
  drawing = true;
  if (!xg->locked)
    xg->dialog->xgraphFillData(xg);

  if (!xg->data)
    return;
  b3dColorIndex(App->background);
  glClear(GL_COLOR_BUFFER_BIT);
  xg->dialog->xgraphDrawPlot(xg);
  xg->dialog->xgraphDrawAxis(xg);
  drawing = false;
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

static void makeBoundaryPoint(Ipoint pt1, Ipoint pt2, int ix1, int ix2,
                              int iy1, int iy2, Ipoint *newpt)
{
  float t, tmax;
  // Find the maximum t parameter implied by X intersections, for t within
  // range of being in the segment
  tmax = 0.;
  if (fabs((double)(pt1.x - pt2.x)) > 1.e-4) {
    t = (ix1 - pt1.x) / (pt2.x - pt1.x);
    if (t >=0 && t < 1)
      tmax = t;
    t = (ix2 - pt1.x) / (pt2.x - pt1.x);
    if (t >=0 && t < 1)
      tmax = B3DMAX(t, tmax);
  }

  // Get maximum from t parameters of Y intersections too
  if (fabs((double)(pt1.y - pt2.y)) > 1.e-4) {
    t = (iy1 - pt1.y) / (pt2.y - pt1.y);
    if (t >=0 && t < 1)
      tmax = B3DMAX(t, tmax);
    t = (iy2 - pt1.y) / (pt2.y - pt1.y);
    if (t >=0 && t < 1)
      tmax = B3DMAX(t, tmax);
  }

  newpt->x = tmax * (pt2.x - pt1.x) + pt1.x;
  newpt->y = tmax * (pt2.y - pt1.y) + pt1.y;
  newpt->z = tmax * (pt2.z - pt1.z) + pt1.z;
}


/*
    $Log$
    Revision 4.9  2007/03/29 04:55:49  mast
    Fixed crash bug when closing window while focus is in edit/spinbox

    Revision 4.8  2006/08/25 14:31:51  mast
    SGI did not like bad declaration

    Revision 4.7  2006/08/24 21:33:19  mast
    Added averaging over multiple lines, mean output, rubberband control

    Revision 4.6  2005/11/11 23:04:29  mast
    Changes for unsigned integers

    Revision 4.5  2003/09/16 02:15:37  mast
    Changed to access image data using new line pointers

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

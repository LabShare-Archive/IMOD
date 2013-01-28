/*  
 *  xgraph.cp -- Imod graph window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <math.h>
#include <stdlib.h>
#include <qlabel.h>
#include <qapplication.h>
#include <qlayout.h>
#include <qbitmap.h>
#include <qcombobox.h>
#include <qsignalmapper.h>
#include <qtoolbutton.h>
#include <qfont.h>
#include <qspinbox.h>
#include <qtooltip.h>
#include <qpushbutton.h>
//Added by qt3to4:
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QGridLayout>
#include <QFrame>
#include <QMouseEvent>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "arrowbutton.h"
#include "xgraph.h"

#include "imod.h"
#include "xzap.h"
#include "imod_input.h"
#include "imod_edit.h"
#include "info_cb.h"
#include "display.h"
#include "pyramidcache.h"
#include "b3dgfx.h"
#include "control.h"
#include "preferences.h"
#include "dia_qtutils.h"

#define XGRAPH_WIDTH 320
#define XGRAPH_HEIGHT 160


static void graphClose_cb(ImodView *vi, void *client, int junk);
static void graphDraw_cb(ImodView *vi, void *client, int drawflag);
static void graphKey_cb(ImodView *vi, void *client, int released,
			QKeyEvent *e);
static void makeBoundaryPoint(Ipoint pt1, Ipoint pt2, int ix1, int ix2,
                              int iy1, int iy2, Ipoint *newpt);

static const char *fileList[MAX_GRAPH_TOGGLES][2] =
  { {":/images/lowres.png", ":/images/highres.png"},
    {":/images/unlock.png", ":/images/lock.png"}};

static QIcon *icons[MAX_GRAPH_TOGGLES];
static int firstTime = 1;
static const char *toggleTips[] = {"Display file values instead of scaled bytes",
                             "Lock X/Y/Z position being displayed"};

enum {GRAPH_XAXIS = 0, GRAPH_YAXIS, GRAPH_ZAXIS, GRAPH_CONTOUR, 
      GRAPH_HISTOGRAM};

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

  xg->dialog->setWindowTitle(imodCaption("3dmod Graph"));

  xg->ctrl = ivwNewControl (xg->vi, graphDraw_cb, graphClose_cb, graphKey_cb,
			    (void *)xg);
  imodDialogManager.add((QWidget *)xg->dialog, IMOD_IMAGE, GRAPH_WINDOW_TYPE, xg->ctrl);

  imod_info_input();
  QSize size = xg->dialog->sizeHint();
  xg->dialog->resize(size.width(), (int)(0.65 * size.width()));
  adjustGeometryAndShow((QWidget *)xg->dialog, IMOD_IMAGE, false);

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
            const char * name, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  int j;
  ArrowButton *arrow;
  mGraph = graph;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  setAnimated(false);

  if (firstTime) 
    utilFileListsToIcons(fileList, icons, MAX_GRAPH_TOGGLES);
  firstTime = 0;

  // Make central vbox and top frame containing an hbox
  QWidget *central = new QWidget(this);
  setCentralWidget(central);
  QVBoxLayout *cenlay = new QVBoxLayout(central);
  cenlay->setContentsMargins(0,0,0,0);
  cenlay->setSpacing(0);
  QFrame * topFrame = new QFrame(central);
  cenlay->addWidget(topFrame);
  topFrame->setFrameStyle(QFrame::Raised | QFrame::StyledPanel);

  // Life lessons!  The frame needs a layout inside it; just putting a box
  // in it does not do the trick, and it asserts no size
  QHBoxLayout *topLayout = new QHBoxLayout(topFrame);
  topLayout->setContentsMargins(2,2,2,2);
  topLayout->setSpacing(1);

  // Add the toolbar widgets
  // Zoom arrows
  arrow = new ArrowButton(Qt::UpArrow, topFrame);
  topLayout->addWidget(arrow);
  arrow->setAutoRaise(TB_AUTO_RAISE);
  arrow->setFocusPolicy(Qt::NoFocus);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  arrow->setToolTip("Increase scale along the pixel axis");
  arrow = new ArrowButton(Qt::DownArrow, topFrame);
  topLayout->addWidget(arrow);
  arrow->setAutoRaise(TB_AUTO_RAISE);
  arrow->setFocusPolicy(Qt::NoFocus);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  arrow->setToolTip("Decrease scale along the pixel axis");

  // Make the 2 toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(topFrame);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < 2; j++) {
    utilSetupToggleButton(topFrame, NULL, topLayout, toggleMapper, icons, 
                          toggleTips, mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));
  }

  mToggleButs[0]->setEnabled(App->cvi->noReadableImage == 0);

  // The axis combo box
  QComboBox *axisCombo = new QComboBox(topFrame);
  topLayout->addWidget(axisCombo);
  axisCombo->addItem("X-axis");
  axisCombo->addItem("Y-axis");
  axisCombo->addItem("Z-axis");
  axisCombo->addItem("Contour");
  if (!App->cvi->pyrCache)
    axisCombo->addItem("Histogram");
  axisCombo->setFocusPolicy(Qt::NoFocus);
  connect(axisCombo, SIGNAL(currentIndexChanged(int)), this, 
          SLOT(axisSelected(int)));
  axisCombo->setToolTip("Select axis to graph");

  mWidthBox = (QSpinBox *)diaLabeledSpin(0, 1., 100., 1., "Width", topFrame,
                                         topLayout);
  mWidthBox->setValue(1);
  connect(mWidthBox, SIGNAL(valueChanged(int)), this, SLOT(widthChanged(int)));
  mWidthBox->setToolTip("Set number of lines to average over");

  mMeanLabel = diaLabel(" 0.0000", topFrame, topLayout);
  
  // Help button
  topLayout->addStretch();
  QPushButton *pbutton = diaPushButton("Help", topFrame, topLayout);
  diaSetButtonWidth(pbutton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
  connect(pbutton, SIGNAL(pressed()), this, SLOT(help()));
  topLayout->addStretch();

  // Now a grid layout in
  QGridLayout *layout = new QGridLayout();
  cenlay->addLayout(layout);
  QVBoxLayout *leftBox = new QVBoxLayout();
  layout->addLayout(leftBox, 0, 0);
  QWidget *spacer = new QWidget(topFrame);
  layout->addWidget(spacer, 1, 0);
  QHBoxLayout *botBox = new QHBoxLayout();
  layout->addLayout(botBox, 1, 1);
  layout->setRowStretch(0, 1);
  layout->setColumnStretch(1, 1);

  // A frame for the graph widget, and a layout inside it, and the GL widget
  QFrame *graphFrame = new QFrame(central);
  graphFrame->setFrameStyle(QFrame::Sunken | QFrame::StyledPanel);
  layout->addWidget(graphFrame, 0, 1);
  QVBoxLayout *graphLayout = new QVBoxLayout(graphFrame);
  graphLayout->setContentsMargins(2,2,2,2);
  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new GraphGL(graph, glFormat, graphFrame);
  graphLayout->addWidget(mGLw);

  // Get a bigger font for the labels
  float font_scale = 1.25;
  QFont newFont = QApplication::font();
  float pointSize = newFont.pointSizeF();
  if (pointSize > 0) {
    newFont.setPointSizeF(pointSize * font_scale);
  } else {
    int pixelSize = newFont.pixelSize();
    newFont.setPixelSize((int)floor(pixelSize * font_scale + 0.5));
  }

  // Get the labels, give them the bigger font
  mPlabel1 = diaLabel(" ", central, botBox);
  mPlabel1->setFont(newFont);
  mPlabel1->setAlignment(Qt::AlignLeft | Qt::AlignTop);
  mPlabel2 = diaLabel(" ", central, botBox);
  mPlabel2->setFont(newFont);
  mPlabel2->setAlignment(Qt::AlignCenter | Qt::AlignTop);
  mPlabel3 = diaLabel(" ", central, botBox);
  mPlabel3->setFont(newFont);
  mPlabel3->setAlignment(Qt::AlignRight | Qt::AlignTop);

  mVlabel1 = diaLabel("-88888", central, leftBox);
  mVlabel1->setFont(newFont);
  mVlabel1->setAlignment(Qt::AlignRight | Qt::AlignTop);
  mVlabel2 = diaLabel(" ", central, leftBox);
  mVlabel2->setFont(newFont);
  mVlabel2->setAlignment(Qt::AlignRight | Qt::AlignBottom);

  QSize hint = mVlabel1->sizeHint();
  mVlabel1->setMinimumWidth(hint.width() + 5);

  resize(XGRAPH_WIDTH, XGRAPH_HEIGHT);
  setFocusPolicy(Qt::StrongFocus);
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
  imodShowHelpPage("graph.html#TOP");
}

// Toggle button
void GraphWindow::toggleClicked(int index)
{
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state;
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
  diaSetChecked(mToggleButs[index], state != 0);
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
  if (utilCloseKey(e))
    close();

  else if (key == Qt::Key_Equal || key == Qt::Key_Plus)
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
  b3dUInt16 **usimage;
  unsigned char *bmap;
  int cx, cy, cz, i, j, jy, nlines;
  int ixStart, iyStart, nxUse, nyUse, ixEnd, iyEnd;
  int cp;
  Icont *cont;
  Ipoint *pt1, *pt2, *pts;
  unsigned char **imdata;
  int   pt;
  int curpt, vecpt, istr, iend, skipStart, skipEnd;
  float frac, totlen, curint, dx, dy, smin, smax;
  Ipoint scale, startPt, endPt, pmin, pmax;
  double sum, lensq;
  ImodView *vi = xg->vi;
  int cacheInd = vi->pyrCache ? vi->pyrCache->getBaseIndex() : -1;

  if (!vi)
    return;

  if (! (xg->data))
    xg->allocSize = 0;

  xg->cx = vi->xmouse;
  cx = (int)xg->cx;
  xg->cy = vi->ymouse;
  cy = (int)xg->cy;
  xg->cz = vi->zmouse;
  cz = (int)(xg->cz + 0.5);

  ixStart = 0;
  iyStart = 0;
  nxUse = vi->xsize;
  nyUse = vi->ysize;
  zapSubsetLimits(vi, ixStart, iyStart, nxUse, nyUse);
  if (xg->axis != GRAPH_HISTOGRAM && !xg->highres && cacheInd < 0) {
    if (ivwSetupFastAccess(vi, &imdata, 0, &istr, vi->curTime))
      return;
  }

  switch(xg->axis){
  case GRAPH_XAXIS:
    dsize = nxUse;
    xg->subStart = ixStart;
    if (allocDataArray(dsize))
      return;

    /* DNM: skip out if outside limits */
    if (cz < 0 || cz >= vi->zsize || cy < 0 || cy >= vi->ysize)
      break;

    if (cx < ixStart || cx > ixStart + nxUse - 1) {
      cx = xg->cx = vi->xmouse = 
        B3DMIN(ixStart + nxUse - 1, B3DMAX(ixStart, cx));
      imodDraw(vi, IMOD_DRAW_XYZ);
    }
    xg->cpt = cx;

    // For tile cache, get the tiles needed from base cache and then set up fast access
    if (cacheInd >= 0 && !xg->highres) {
      vi->pyrCache->loadTilesContainingArea(cacheInd, ixStart, cy - xg->nlines / 2 - 1,
                                            dsize, xg->nlines + 3, cz);
      if (ivwSetupFastTileAccess(vi, cacheInd, 0, istr))
        return;
    }

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
          xg->data[i] += ivwFastGetValue(i + ixStart, jy, cz);
    }
    if (nlines > 1)
      for(i = 0; i < dsize; i++)
        xg->data[i] /= nlines;
    break;


  case GRAPH_YAXIS:
    dsize = nyUse;
    xg->subStart = iyStart;
    if (allocDataArray(dsize))
      return;

    /* DNM: skip out if outside limits */
    if (cx < 0 || cx >= vi->xsize || cz < 0 || cz >= vi->zsize)
      break;

    if (cy < iyStart || cy > iyStart + nyUse - 1) {
      cy = xg->cy = vi->ymouse = 
        B3DMIN(iyStart + nyUse - 1, B3DMAX(iyStart, cy));
      imodDraw(vi, IMOD_DRAW_XYZ);
    }
    xg->cpt = cy;

    // For tile cache, get the tiles needed from base cache and then set up fast access
    if (cacheInd >= 0 && !xg->highres) {
      vi->pyrCache->loadTilesContainingArea(cacheInd, cx - xg->nlines / 2 - 1, iyStart, 
                                            xg->nlines + 3, dsize, cz);
      if (ivwSetupFastTileAccess(vi, cacheInd, 0, istr))
        return;
    }

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
          xg->data[i] += ivwFastGetValue(jy, i + iyStart, cz);
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
    if (cacheInd >= 0 && !xg->highres) {
      if (ivwSetupFastTileAccess(vi, cacheInd, 0, istr))
        return;
    }
    if (xg->highres)
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwGetFileValue(vi, cx, cy, i);
    else
      for(i = 0; i < dsize; i++)
        xg->data[i] = ivwFastGetValue(cx, cy, i);
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

    // For tile cache, find limits of contour within area being used and load it if it
    // is planar
    if (cacheInd >= 0 && !xg->highres) {
      imodContourGetBBox(cont, &pmin, &pmax);
      if (B3DNINT(pmin.z) == B3DNINT(pmax.z)) {
        pmin.x = B3DMAX(pmin.x, ixStart) - xg->nlines / 2 - 1;
        pmin.x = B3DMAX(0, pmin.x);
        pmax.x = B3DMIN(pmax.x, ixStart + nxUse) + xg->nlines / 2 + 1;
        pmax.x = B3DMIN(pmax.x, vi->xsize - 1);
        pmin.y = B3DMAX(pmin.y, iyStart) - xg->nlines / 2 - 1;
        pmin.y = B3DMAX(0, pmin.y);
        pmax.y = B3DMIN(pmax.y, iyStart + nyUse) + xg->nlines / 2 + 1;
        pmax.y = B3DMIN(pmax.y, vi->ysize - 1);
        vi->pyrCache->loadTilesContainingArea(cacheInd, pmin.x, pmin.y, pmax.x + 1 - 
                                              pmin.x, pmax.y + 1 - pmin.y, 
                                              B3DNINT(pmin.z));
      }
      if (ivwSetupFastTileAccess(vi, cacheInd, 0, istr))
        return;
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
            xg->data[i] += ivwFastGetValue(cx, cy, cz);
        }
      }
      if (nlines)
        xg->data[i] /= nlines;
    }

    break;

  case GRAPH_HISTOGRAM:
    image = ivwGetCurrentZSection(vi);
    usimage = (b3dUInt16 **)image;
    if (image){
      dsize = 256;
      sum = 0.;
      if (allocDataArray(dsize))
        return;
      xg->subStart = 0;
      cz = (int)(vi->zmouse + 0.5f);
      if (vi->ushortStore) {
        bmap = ivwUShortInRangeToByteMap(vi);
        if (!bmap)
          return;
        for (j = iyStart; j < iyStart + nyUse; j++) {
          for (i = ixStart; i < ixStart + nxUse; i++) {
            pt = bmap[usimage[j][i]];
            xg->data[pt] += 1.0f;
            sum += pt;
          }
        }
        xg->cpt = bmap[usimage[cy][cx]];
        free(bmap);
      } else {
        for (j = iyStart; j < iyStart + nyUse; j++) {
          for (i = ixStart; i < ixStart + nxUse; i++) {
            pt = image[j][i];
            xg->data[pt] += 1.0f;
            sum += pt;
          }
        }
        xg->cpt = image[cy][cx];
      }

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
  str.sprintf("%d", (xg->start + xg->end) / 2);
  mPlabel2->setText(str);
  str.sprintf("%d", xg->end);
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

  // Give a histogram no more than full range if possible
  if (xg->axis == GRAPH_HISTOGRAM) {
    spnt = B3DMAX(0, spnt);
    epnt = B3DMIN(256, epnt);
    zoom = (float)xg->width / (epnt - spnt);
  }

  b3dColorIndex(App->foreground);
  max = -1.e37;
  min = 1.e37;
  for (i = spnt - xg->subStart; i < epnt - xg->subStart; i++) {
    if (i < 0)
      continue;
    if (i >= xg->dsize)
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
  xg->end    = epnt;
  return;
}


/*
 * The GL WIDGET CLASS: PAINT, RESIZE, MOUSE EVENTS
 */
GraphGL::GraphGL(GraphStruct *graph, QGLFormat format, QWidget * parent)
  : QGLWidget(format, parent)
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
  utilRaiseIfNeeded(mGraph->dialog, e);
  if (e->buttons() & ImodPrefs->actualButton(1))
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


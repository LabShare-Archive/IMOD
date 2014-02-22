/*
* plotter.cpp - callbacks for the main widget of ctfplotter.
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*/

#include <QtGui>

#include <cmath>

#include "fittingdialog.h"
#include "angledialog.h"
#include "plotter.h"
#include "myapp.h"
#include "ctfmain.h"

#include "b3dutil.h"

using namespace std;

Plotter::Plotter(MyApp *app, QWidget *parent) : QWidget(parent)
{
  int added = 0, width;
  mApp = app;
  setBackgroundRole(QPalette::Dark);
  setAutoFillBackground(true);
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  setFocusPolicy(Qt::StrongFocus);
  mRubberBandIsShown = false;
  mFittingDia=NULL;
  mAngleDia=NULL;

  mZoomInButton = new QToolButton(this);
  mZoomInButton->setIcon(QIcon(":/images/zoomin.png"));
  mZoomInButton->adjustSize();
  mZoomInButton->setToolTip("Zoom in");
  connect(mZoomInButton, SIGNAL(clicked()), this, SLOT(zoomIn()));

  mZoomOutButton = new QToolButton(this);
  mZoomOutButton->setIcon( QIcon(":/images/zoomout.png"));
  mZoomOutButton->adjustSize();
  mZoomOutButton->setToolTip("Zoom out");
  connect(mZoomOutButton, SIGNAL(clicked()), this, SLOT(zoomOut()));

  mPrintButton= new QToolButton(this);
  mPrintButton->setIcon( QIcon(":/images/printer.png") );
  mPrintButton->adjustSize();
  mPrintButton->setToolTip("Print");
  connect(mPrintButton, SIGNAL(clicked()), this, SLOT(printIt()) );

  mHelpButton= new QToolButton(this);
  mHelpButton->setIcon( QIcon(":/images/ctfhelp.png") );
  mHelpButton->adjustSize();
  mHelpButton->setToolTip("Open Ctfplotter help");
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(ctfHelp()) );

  mRangeButton = new QPushButton("Fitting", this);
  mRangeButton->setToolTip( "Open Fitting Ranges and Methods dialog");
  connect(mRangeButton, SIGNAL(clicked()), this, SLOT(rangeDiag()) );

  mAngleButton = new QPushButton("Angles", this);
  mAngleButton->setToolTip("Open Angle Range and Tile Selection dialog");
  connect(mAngleButton, SIGNAL(clicked()), this, SLOT(angleDiag()) );
#ifdef Q_OS_MACX  
  added = (int)(1.5 * mRangeButton->fontMetrics().height());
#endif
  width = (int)(1.2 * mAngleButton->fontMetrics().width("Angles") +0.5)+added;
  mAngleButton->setFixedWidth(width);
  mRangeButton->setFixedWidth(width);

  mTileButton=new QToolButton(this);
  mTileButton->setIcon(QIcon(":/images/moreTile.png") );
  mTileButton->adjustSize();
  mTileButton->setToolTip("Include all of the rest of the tiles");
  connect(mTileButton, SIGNAL(clicked()), mApp, SLOT(moreTileCenterIncluded()));

  mZeroLabel=new QLabel( tr("Z: NA       "), this);
  mZeroLabel->adjustSize();
  mDefocusLabel=new QLabel( tr("D: NA        "), this);
  mDefocusLabel->adjustSize();
  mDefoc2Label=new QLabel( tr("D2: NA       "), this);
  mDefoc2Label->adjustSize();
  mDefocAvgLabel=new QLabel( tr("D-avg: NA       "), this);
  mDefocAvgLabel->adjustSize();

  mPrinter= new QPrinter;
  mPrinter->setOrientation(QPrinter::Landscape);
  setPlotSettings(PlotSettings());
}

Plotter::~Plotter(){
  delete mPrinter;
}

void Plotter::setPlotSettings(const PlotSettings &settings)
{
  mZoomStack.clear();
  mZoomStack.append(settings);
  mCurZoom = 0;
  mZoomInButton->hide();
  mZoomOutButton->hide();
  refreshPixmap();
}

void Plotter::zoomOut()
{
  if (mCurZoom > 0) {
    --mCurZoom;
    mZoomOutButton->setEnabled(mCurZoom > 0);
    mZoomInButton->setEnabled(true);
    mZoomInButton->show();
    refreshPixmap();
  }
}

void Plotter::zoomIn()
{
  if (mCurZoom < mZoomStack.count() - 1) {
    ++mCurZoom;
    mZoomInButton->setEnabled(
                             mCurZoom < (int)mZoomStack.size() - 1);
    mZoomOutButton->setEnabled(true);
    mZoomOutButton->show();
    refreshPixmap();
  }
}

void Plotter::rangeDiag()
{
  if(!mFittingDia) {
    mFittingDia=new FittingDialog(mApp, this);
    connect(mFittingDia, SIGNAL( range(double, double, double, double) ), mApp,
            SLOT( rangeChanged(double, double, double, double)) );
    connect(mFittingDia, SIGNAL( x1MethodChosen(int) ), mApp,
            SLOT(setX1Method(int)) );
    connect(mFittingDia, SIGNAL( x2MethodChosen(int) ), mApp,
            SLOT(setX2Method(int)) );    
  }
  mFittingDia->show();
  mFittingDia->raise();
  mFittingDia->activateWindow();
}

/* 
 * Open the angle dialog, set it with current values
 */
void Plotter::angleDiag()
{
  QSize hint;
  bool newDia = !mAngleDia;
  if(!mAngleDia){
    mAngleDia=new AngleDialog(mApp, this);
    connect(mAngleDia, SIGNAL(angle(double,double,double,double,int,double,
                                  double,double) ), 
            mApp, SLOT(angleChanged(double,double,double,double,int,double,
                                    double,double)));
    connect(mAngleDia, SIGNAL( defocusMethod(int)), mApp, 
            SLOT( setDefOption(int)) );
    connect(mAngleDia, SIGNAL( initialTileChoice(int)), mApp,
            SLOT( setInitTileOption(int)) );
    double expDefocus=mApp->defocusFinder.getExpDefocus();
    double lowAngle=mApp->getLowAngle();
    double highAngle=mApp->getHighAngle();
    double defTol=mApp->getDefocusTol();
    int    tSize=mApp->getTileSize();
    double axisAngle=mApp->getAxisAngle();
    double leftTol=mApp->getLeftTol();
    double rightTol=mApp->getRightTol();
    char tmpStr[20];
    sprintf(tmpStr, "%6.2f", expDefocus); 
    mAngleDia->mDefocusEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.1f", lowAngle); 
    mAngleDia->mLowAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.1f", highAngle); 
    mAngleDia->mHighAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", B3DNINT(defTol)); 
    mAngleDia->mDefTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", tSize);
    mAngleDia->mTileSizeEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", axisAngle); 
    mAngleDia->mAxisAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", B3DNINT(leftTol)); 
    mAngleDia->mLeftTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", B3DNINT(rightTol)); 
    mAngleDia->mRightTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", mApp->getAutoFromAngle()); 
    mAngleDia->mAutoFromEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", mApp->getAutoToAngle()); 
    mAngleDia->mAutoToEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.1f", mApp->getRangeStep()); 
    mAngleDia->mRangeStepEdit->setText(tmpStr);
    
    mAngleDia->updateTable();
  }
  mAngleDia->show();
  mAngleDia->raise();
  mAngleDia->activateWindow();
  if (newDia)
    mAngleDia->tileParamsClicked();
}

void Plotter::printIt()
{
  QPrintDialog printDialog(mPrinter, this);
  printDialog.setWindowTitle("Printing plot");

  if ( printDialog.exec() ) {
    QPainter painter;
    if( !painter.begin( mPrinter ) ) return;
    painter.setWindow(0, 0, width(), height() );
    drawGrid(&painter, false);
    drawCurves(&painter);
  }
}

void Plotter::ctfHelp()
{
  ctfShowHelpPage("ctfHelp/ctfguide.html#TOP");
}

void Plotter::setCurveData(int id, const QVector<QPointF> &data)
{
    mCurveMap[id] = data;
   // int n=mCurveMap[id].size() / 2;
   // int i;
   // double min, max;

    //find and reset x, y ranges;
    /*if(n>1 && id==0){
      if( data[0]>data[2] ){
        max=data[0];
        min=data[2];
      }else{
        min=data[0];
        max=data[2];
      }
      for(i=2;i<n;i++){
        if( data[2*i]>max ) max=data[2*i];
        else if( data[2*i]<min ) min=data[2*i];
      }
      if( min<mZoomStack[0].minX ) mZoomStack[0].minX=min;
      if( max>mZoomStack[0].maxX ) mZoomStack[0].maxX=max; 

      if( data[1]>data[3] ){
        max=data[1];
        min=data[3];
      } else{
        min=data[1];
        max=data[3];
      }
      for(i=2;i<n;i++){
        if( data[2*i+1]>max ) max=data[2*i+1];
        else if( data[2*i+1]<min ) min=data[2*i+1];
      }
      if( min<mZoomStack[0].minY ) mZoomStack[0].minY=min;
      if( max>mZoomStack[0].maxY ) mZoomStack[0].maxY=max;
    }*/
    
    refreshPixmap();
}

void Plotter::clearCurve(int id)
{
  mCurveMap.remove(id);
  refreshPixmap();
}

QSize Plotter::minimumSizeHint() const
{
  return QSize(4 * Margin, 4 * Margin);
}

QSize Plotter::sizeHint() const
{
  return QSize(26 * Margin, 18 * Margin);
}

void Plotter::paintEvent(QPaintEvent *event)
{
  QStylePainter painter(this);
  painter.drawPixmap(0,0, mPixmap);


  if (mRubberBandIsShown) {
    //painter.setPen(palette().light().color());
    painter.setPen(Qt::white);
    painter.drawRect(mRubberBandRect.normalized().adjusted(0,0,-1,-1));

  }
  if (hasFocus()) {
    QStyleOptionFocusRect option;
    option.initFrom(this);
    //option.backgroundColor=palette().dark().color();
    option.backgroundColor=QColor(64,64,64);
    painter.drawPrimitive(QStyle::PE_FrameFocusRect, option);
  }
}

void Plotter::resizeEvent(QResizeEvent *)
{
  int x = width() - (mZoomInButton->width()
                     + mZoomOutButton->width() +mPrintButton->width() +
                     +mZeroLabel->width()+
                     mDefocusLabel->width() + mTileButton->width()+
                     mDefoc2Label->width() + mDefocAvgLabel->width() + 75);

  // Put text buttons on left
  mAngleButton->move(10, 5);
  mRangeButton->move(15 + mAngleButton->width(), 5);

  // Then lay out the labels and tool buttons at fixed distance from right edge
  mZeroLabel->move(x, 5);
  x += mZeroLabel->width()+5;
  mDefocusLabel->move(x, 5);
  x += mDefocusLabel->width()+5;
  mDefoc2Label->move(x, 5);
  x += mDefoc2Label->width()+5;
  mDefocAvgLabel->move(x, 5);
  x += mDefocAvgLabel->width()+5;
    
  mZoomInButton->move(x, 5);
  x += mZoomInButton->width() + 5;
  mZoomOutButton->move(x, 5);
  x += mZoomOutButton->width() + 5;
  mTileButton->move(x, 5);
  x += mTileButton->width() + 5;
  mPrintButton->move(x, 5);
  x += mPrintButton->width() + 5;
  mHelpButton->move(x, 5);
  refreshPixmap();
}

/*
 * Mouse events for the zooming rubberband
 */
void Plotter::mousePressEvent(QMouseEvent *event)
{
  QRect rect(Margin, Margin, width()-2*Margin, height()-2*Margin);
  if (event->button() ==Qt::LeftButton) {
    if(rect.contains(event->pos() ) ){
      mRubberBandIsShown = true;
      mRubberBandRect.setTopLeft(event->pos());
      mRubberBandRect.setBottomRight(event->pos());
      updateRubberBandRegion();
      setCursor(Qt::CrossCursor);
    }
  }
}

void Plotter::mouseMoveEvent(QMouseEvent *event)
{
  if (mRubberBandIsShown) {
    updateRubberBandRegion();
    mRubberBandRect.setBottomRight(event->pos());
    updateRubberBandRegion();
  }
}

void Plotter::mouseReleaseEvent(QMouseEvent *event)
{
  if (event->button() == Qt::LeftButton && mRubberBandIsShown) {
    mRubberBandIsShown = false;
    updateRubberBandRegion();
    unsetCursor();

    QRect rect = mRubberBandRect.normalized();
    if (rect.width() < 4 || rect.height() < 4)
      return;
    rect.translate(-Margin, -Margin);

    PlotSettings prevSettings = mZoomStack[mCurZoom];
    PlotSettings settings;
    double dx = prevSettings.spanX() / (width() - 2 * Margin);
    double dy = prevSettings.spanY() / (height() - 2 * Margin);
    settings.minX = prevSettings.minX + dx * rect.left();
    settings.maxX = prevSettings.minX + dx * rect.right();
    settings.minY = prevSettings.maxY - dy * rect.bottom();
    settings.maxY = prevSettings.maxY - dy * rect.top();
    settings.adjust();

    mZoomStack.resize(mCurZoom + 1);
    mZoomStack.append(settings);
    zoomIn();
  }
}

/*
 * Use double-click to define zeros
 */
void Plotter::mouseDoubleClickEvent(QMouseEvent *event)
{
  DefocusFinder *finder = &mApp->defocusFinder;
  PlotSettings settings=mZoomStack[mCurZoom];
  double dx=settings.spanX()/(width()-2*Margin);
  double zero=settings.minX + dx*(event->x()-Margin);
  double defocus, defoc2, defocAvg;

  // For a left button, redefine the first zero
  if (event->button() == Qt::LeftButton) {
    finder->setZero(zero);
    finder->findDefocus(&defocus);
    finder->setAvgDefocus(-1.);
    manageLabels(zero, defocus, 0., 0., 0);
    // If using ctf-like curve fit, replot with selected
    // defocus and all other parameters unchanged
    if (mApp->getZeroFitMethod() == 0)
      mApp->replotWithDefocus(defocus);
  } else {
    // Otherwise define the second zero and the average
    defocus = finder->getDefocus();
    if (defocus <= 0)
      return;
    defoc2 = finder->defocusFromSecondZero(zero);
    defocAvg = 0.5 * (defocus + defoc2);
    finder->setAvgDefocus(defocAvg);
    manageLabels(finder->getZero(), defocus, defoc2, defocAvg, 1);
  }
}

/*
 * Set the labels appropriately for the given values: set to NA if type = -1,
 * set first zero if type is 0 or 1, set second zero if type is 1
 */
void Plotter::manageLabels(double zero, double defocus, double def2,
                           double defAvg, int type)
{
  QString zeroString = "Z: NA ";
  QString defocusString = "D: NA ";
  QString defoc2String = "D2: NA ";
  QString defocAvgString = "D-avg: NA ";
  // FREQUENCY SCALE CHANGE: " / 2."
  if (type >= 0) {
    zeroString.sprintf("Z: %4.3f", zero / 2.);
    defocusString.sprintf("D%s: %4.2f", type ? "1" : "", defocus);
  }
  if (type > 0) {
    defoc2String.sprintf("D2: %4.2f", def2);
    defocAvgString.sprintf("D-avg: %4.2f", defAvg);
  }
  mZeroLabel->setText(zeroString);
  mDefocusLabel->setText(defocusString);
  mDefoc2Label->setText(defoc2String);
  mDefocAvgLabel->setText(defocAvgString);
}

/*
 * Process hot keys for zoom and scroll
 */
void Plotter::keyPressEvent(QKeyEvent *event)
{
  switch (event->key()) {
  case Qt::Key_Plus:
  case Qt::Key_Equal:
    zoomIn();
    break;
  case Qt::Key_Minus:
  case Qt::Key_Underscore:
    zoomOut();
    break;
  case Qt::Key_Left:
    mZoomStack[mCurZoom].scroll(-1, 0);
    refreshPixmap();
    break;
  case Qt::Key_Right:
    mZoomStack[mCurZoom].scroll(+1, 0);
    refreshPixmap();
    break;
  case Qt::Key_Down:
    mZoomStack[mCurZoom].scroll(0, -1);
    refreshPixmap();
    break;
  case Qt::Key_Up:
    mZoomStack[mCurZoom].scroll(0, +1);
    refreshPixmap();
    break;
  default:
    QWidget::keyPressEvent(event);
  }
}

/*
 * Use wheel to scroll up/down
 */
void Plotter::wheelEvent(QWheelEvent *event)
{
  int numDegrees = event->delta() / 8;
  int numTicks = numDegrees / 15;

  if (event->orientation() == Qt::Horizontal)
    mZoomStack[mCurZoom].scroll(numTicks, 0);
  else
    mZoomStack[mCurZoom].scroll(0, numTicks);
  refreshPixmap();
}

void Plotter::updateRubberBandRegion()
{
    QRect rect = mRubberBandRect.normalized();
    update(rect.left(), rect.top(), rect.width(), 1);
    update(rect.left(), rect.top(), 1, rect.height());
    update(rect.left(), rect.bottom(), rect.width(), 1);
    update(rect.right(), rect.top(), 1, rect.height());
}

void Plotter::refreshPixmap()
{
    mPixmap=QPixmap(size());
    //mPixmap.fill(this, 0, 0);
    mPixmap.fill(QColor(64,64,64));
    QPainter painter(&mPixmap);
    painter.initFrom(this);
    drawGrid(&painter, true);
    drawCurves(&painter);
    update();
}

/*
 * Draw the grid lines and labels
 */
void Plotter::drawGrid(QPainter *painter, bool onScreen)
{
  QRect rect(Margin, Margin, width() - 2 * Margin, height() - 2 * Margin);
  if(!rect.isValid()) return;
  PlotSettings settings = mZoomStack[mCurZoom];
  //QPen quiteDark = palette().dark().color().light();
  //QPen light = palette().light().color();
  //QPen quiteDark=QPen(Qt::white);
  QPen quiteDark;
  QPen light;
  if(onScreen){
    quiteDark=QPen(Qt::black);
    light=QPen(Qt::white);
  }else{
    quiteDark=QPen(QColor(64, 64, 64));
    light=QPen(Qt::black);
  }

  for (int i = 0; i <= settings.numXTicks; ++i) {
    int x = rect.left() + (i * (rect.width() - 1)
                           / settings.numXTicks);
    double label = settings.minX + (i * settings.spanX()
                                    / settings.numXTicks);
    painter->setPen(quiteDark);
    painter->drawLine(x, rect.top(), x, rect.bottom());
    painter->setPen(light);
    painter->drawLine(x, rect.bottom(), x, rect.bottom() + 5);
    // FREQUENCY SCALE CHANGE: " / 2."
    painter->drawText(x - 50, rect.bottom() + 5, 100, 15,
                      Qt::AlignHCenter | Qt::AlignTop,
                      QString::number(label / 2.));
  }
  painter->drawText(rect.left() + rect.width() / 2 - 100, rect.bottom() + 20, 200, 15,
                    Qt::AlignHCenter | Qt::AlignTop, "Frequency (1/pixel)");
  for (int j = 0; j <= settings.numYTicks; ++j) {
    int y = rect.bottom() - (j * (rect.height() - 1)
                             / settings.numYTicks);
    double label = settings.minY + (j * settings.spanY()
                                    / settings.numYTicks);
    painter->setPen(quiteDark);
    painter->drawLine(rect.left(), y, rect.right(), y);
    painter->setPen(light);
    painter->drawLine(rect.left() - 5, y, rect.left(), y);
    painter->drawText(rect.left() - Margin, y - 10,
                      Margin - 5, 20,
                      Qt::AlignRight | Qt::AlignVCenter,
                      QString::number(label));
  }
  painter->drawRect(rect.adjusted(0,0,-1,-1));
}

/*
 * Draw the curves
 */
void Plotter::drawCurves(QPainter *painter)
{
  static const QColor colorForIds[6] = {
    Qt::red, Qt::green, Qt::blue, Qt::cyan, Qt::magenta, Qt::yellow
  };
  PlotSettings settings = mZoomStack[mCurZoom];
  QRect rect(Margin, Margin,
             width() - 2 * Margin, height() - 2 * Margin);

  if(!rect.isValid()) return;

  painter->setClipRect(rect.adjusted(+1, +1, -1, -1) );
    
  QMapIterator<int, QVector<QPointF> > i(mCurveMap);
  while (i.hasNext() ) {
    i.next();
    int id = i.key();
    const QVector<QPointF> &data = i.value();
    QPolygonF polyline(data.count());

    for (int j = 0; j <data.count(); ++j) {
      double dx = data[j].x() - settings.minX;
      double dy = data[j].y() - settings.minY;
      double x = rect.left() + (dx * (rect.width() - 1)
                                / settings.spanX());
      double y = rect.bottom() - (dy * (rect.height() - 1)
                                  / settings.spanY());
      polyline[j]=QPointF(x,y);
    }
    painter->setPen(colorForIds[(uint)id % 6]);
    painter->drawPolyline(polyline);
  }
}

PlotSettings::PlotSettings()
{
  minX = 0.0;
  maxX = 1.0;
  numXTicks = 5;
  
  minY = -0.3;
  maxY = 20;
  numYTicks = 5;
}

void PlotSettings::scroll(int dx, int dy)
{
  double stepX = spanX() / numXTicks;
  minX += dx * stepX;
  maxX += dx * stepX;
  
  double stepY = spanY() / numYTicks;
  minY += dy * stepY;
  maxY += dy * stepY;
}

void PlotSettings::adjust()
{
  adjustAxis(minX, maxX, numXTicks);
  adjustAxis(minY, maxY, numYTicks);
}

void PlotSettings::adjustAxis(double &min, double &max,
                              int &numTicks)
{
  const int MinTicks = 4;
  double grossStep = (max - min) / MinTicks;
  double step = pow(10.0, floor(log10(grossStep)));
  
  if (5 * step < grossStep)
    step *= 5;
  else if (2 * step < grossStep)
    step *= 2;
  
  numTicks = (int)(ceil(max / step) - floor(min / step));
  min = floor(min / step) * step;
  max = ceil(max / step) * step;
}

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
*  Log at end of file
*/

#include <QtGui>

#include <cmath>

#include "rangedialog.h"
#include "angledialog.h"
#include "plotter.h"
#include "myapp.h"
#include "ctfmain.h"

#include "b3dutil.h"

using namespace std;

Plotter::Plotter(QWidget *parent) : QWidget(parent)
{
  mApp = (MyApp *)qApp;
  setBackgroundRole(QPalette::Dark);
  setAutoFillBackground(true);
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  setFocusPolicy(Qt::StrongFocus);
  rubberBandIsShown = false;
  rDialog=0;
  aDialog=0;

  zoomInButton = new QToolButton(this);
  zoomInButton->setIcon(QIcon(":/images/zoomin.png"));
  zoomInButton->adjustSize();
  zoomInButton->setToolTip("Zoom in");
  connect(zoomInButton, SIGNAL(clicked()), this, SLOT(zoomIn()));

  zoomOutButton = new QToolButton(this);
  zoomOutButton->setIcon( QIcon(":/images/zoomout.png"));
  zoomOutButton->adjustSize();
  zoomOutButton->setToolTip("Zoom out");
  connect(zoomOutButton, SIGNAL(clicked()), this, SLOT(zoomOut()));

  printButton= new QToolButton(this);
  printButton->setIcon( QIcon(":/images/printer.png") );
  printButton->adjustSize();
  printButton->setToolTip("Print");
  connect(printButton, SIGNAL(clicked()), this, SLOT(printIt()) );

  helpButton= new QToolButton(this);
  helpButton->setIcon( QIcon(":/images/ctfhelp.png") );
  helpButton->adjustSize();
  helpButton->setToolTip("Help");
  connect(helpButton, SIGNAL(clicked()), this, SLOT(ctfHelp()) );

  saveButton=new  QToolButton(this);
  saveButton->setIcon( QIcon(":/images/save.png") );
  saveButton->adjustSize();
  saveButton->setToolTip( "Save");
  connect(saveButton, SIGNAL(clicked()), this, SLOT(saveIt()) );
    
  rangeButton=new QToolButton(this);
  rangeButton->setIcon( QIcon(":/images/range.png") );
  rangeButton->adjustSize();
  rangeButton->setToolTip( "Set fitting ranges and methods");
  connect(rangeButton, SIGNAL(clicked()), this, SLOT(rangeDiag()) );

  angleButton=new QToolButton(this);
  angleButton->setIcon( QIcon(":/images/angle.png") );
  angleButton->adjustSize();
  angleButton->setToolTip("Set the angle range and expected defocus" );
  connect(angleButton, SIGNAL(clicked()), this, SLOT(angleDiag()) );

  tileButton=new QToolButton(this);
  tileButton->setIcon(QIcon(":/images/moreTile.png") );
  tileButton->adjustSize();
  tileButton->setToolTip("Include all of the rest of the tiles");
  connect(tileButton, SIGNAL(clicked()), qApp, SLOT(moreTileCenterIncluded()) );

  zeroLabel=new QLabel( tr("Z: NA       "), this);
  zeroLabel->adjustSize();
  defocusLabel=new QLabel( tr("D: NA        "), this);
  defocusLabel->adjustSize();
  defoc2Label=new QLabel( tr("D2: NA       "), this);
  defoc2Label->adjustSize();
  defocAvgLabel=new QLabel( tr("D-avg: NA       "), this);
  defocAvgLabel->adjustSize();

  printer= new QPrinter;
  printer->setOrientation(QPrinter::Landscape);
  setPlotSettings(PlotSettings());
}

Plotter::~Plotter(){
  delete printer;
}

void Plotter::setPlotSettings(const PlotSettings &settings)
{
  zoomStack.clear();
  zoomStack.append(settings);
  curZoom = 0;
  zoomInButton->hide();
  zoomOutButton->hide();
  refreshPixmap();
}

void Plotter::zoomOut()
{
  if (curZoom > 0) {
    --curZoom;
    zoomOutButton->setEnabled(curZoom > 0);
    zoomInButton->setEnabled(true);
    zoomInButton->show();
    refreshPixmap();
  }
}

void Plotter::zoomIn()
{
  if (curZoom < zoomStack.count() - 1) {
    ++curZoom;
    zoomInButton->setEnabled(
                             curZoom < (int)zoomStack.size() - 1);
    zoomOutButton->setEnabled(true);
    zoomOutButton->show();
    refreshPixmap();
  }
}

void Plotter::rangeDiag()
{
  if(!rDialog) {
    rDialog=new RangeDialog(this);
    connect(rDialog, SIGNAL( range(double, double, double, double) ), qApp,
            SLOT( rangeChanged(double, double, double, double)) );
    connect(rDialog, SIGNAL( x1MethodChosen(int) ), qApp,
            SLOT(setX1Method(int)) );
    connect(rDialog, SIGNAL( x2MethodChosen(int) ), qApp,
            SLOT(setX2Method(int)) );    
  }
  rDialog->show();
  rDialog->raise();
  rDialog->activateWindow();
}

/* 
 * Open the angle dialog, set it with current values
 */
void Plotter::angleDiag()
{
  if(!aDialog){
    aDialog=new AngleDialog(this);
    connect(aDialog, 
            SIGNAL(angle(double,double,double,double,int,double,double,double) ), 
            qApp, 
            SLOT(angleChanged(double,double,double,double,int,double,double,double)));
    connect(aDialog, SIGNAL( defocusMethod(int)), qApp, 
            SLOT( setDefOption(int)) );
    connect(aDialog, SIGNAL( initialTileChoice(int)), qApp,
            SLOT( setInitTileOption(int)) );
    double expDefocus=((MyApp *)qApp)->defocusFinder.getExpDefocus();
    double lowAngle=mApp->getLowAngle();
    double highAngle=mApp->getHighAngle();
    double defTol=mApp->getDefocusTol();
    int    tSize=mApp->getTileSize();
    double axisAngle=mApp->getAxisAngle();
    double leftTol=mApp->getLeftTol();
    double rightTol=mApp->getRightTol();
    char tmpStr[20];
    sprintf(tmpStr, "%6.2f", expDefocus); 
    aDialog->defocusEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.1f", lowAngle); 
    aDialog->lowAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.1f", highAngle); 
    aDialog->highAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", B3DNINT(defTol)); 
    aDialog->defTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", tSize);
    aDialog->tileSizeEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", axisAngle); 
    aDialog->axisAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", B3DNINT(leftTol)); 
    aDialog->leftTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", B3DNINT(rightTol)); 
    aDialog->rightTolEdit->setText(tmpStr);
  }
  aDialog->show();
  aDialog->raise();
  aDialog->activateWindow();
}

void Plotter::printIt()
{
  QPrintDialog printDialog(printer, this);
  printDialog.setWindowTitle("Printing plot");

  if ( printDialog.exec() ) {
    QPainter painter;
    if( !painter.begin( printer ) ) return;
    painter.setWindow(0, 0, width(), height() );
    drawGrid(&painter, false);
    drawCurves(&painter);
  }
}

void Plotter::ctfHelp()
{
  ctfShowHelpPage("ctfHelp/ctfguide.html");
}

void Plotter::saveIt()
{
    
  char *defFn=mApp->getDefFn();
  FILE *fp, *saveFp;

  saveFp=mApp->getSaveFp();
  if(!saveFp){
    imodBackupFile(defFn);
    fp=fopen(defFn,"w");
    mApp->setSaveFp(fp);
  }else fp=fopen(defFn, "a");

  if(!fp) {
    QErrorMessage* errorMessage = new QErrorMessage( this );
    errorMessage->showMessage( "Can not open output file" );
    return;
  }
  int startingSlice=mApp->getStartingSliceNum();
  int endingSlice=mApp->getEndingSliceNum();
  double lAngle=mApp->getLowAngle();
  double hAngle=mApp->getHighAngle();
  double defocus=mApp->defocusFinder.getAvgDefocus();
  if (defocus <= 0.)
    defocus=mApp->defocusFinder.getDefocus();
  fprintf(fp, "%d\t%d\t%5.2f\t%5.2f\t%6.0f\n", startingSlice, endingSlice,
          lAngle, hAngle, defocus*1000);

  //mApp->saveAllPs();

  fclose(fp);
    
}

void Plotter::setCurveData(int id, const QVector<QPointF> &data)
{
    curveMap[id] = data;
   // int n=curveMap[id].size() / 2;
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
      if( min<zoomStack[0].minX ) zoomStack[0].minX=min;
      if( max>zoomStack[0].maxX ) zoomStack[0].maxX=max; 

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
      if( min<zoomStack[0].minY ) zoomStack[0].minY=min;
      if( max>zoomStack[0].maxY ) zoomStack[0].maxY=max;
    }*/
    
    refreshPixmap();
}

void Plotter::clearCurve(int id)
{
  curveMap.remove(id);
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
  painter.drawPixmap(0,0, pixmap);


  if (rubberBandIsShown) {
    //painter.setPen(palette().light().color());
    painter.setPen(Qt::white);
    painter.drawRect(rubberBandRect.normalized().adjusted(0,0,-1,-1));

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
  int x = width() - (zoomInButton->width()
                     + zoomOutButton->width() +printButton->width() +
                     rangeButton->width()+zeroLabel->width()+
                     defocusLabel->width() + tileButton->width()+
                     angleButton->width()+ saveButton->width()+
                     defoc2Label->width() + defocAvgLabel->width() + 85);
  zeroLabel->move(x, 5);
  x += zeroLabel->width()+5;
  defocusLabel->move(x, 5);
  x += defocusLabel->width()+5;
  defoc2Label->move(x, 5);
  x += defoc2Label->width()+5;
  defocAvgLabel->move(x, 5);
  x += defocAvgLabel->width()+5;
    
  zoomInButton->move(x, 5);
  x += zoomInButton->width() + 5;
  zoomOutButton->move(x, 5);
  x += zoomOutButton->width() + 5;
  rangeButton->move(x, 5);
  x += rangeButton->width() + 5;
  angleButton->move(x, 5);
  x += angleButton->width() + 5;
  tileButton->move(x, 5);
  x += tileButton->width() + 5;
  saveButton->move(x, 5);
  x += saveButton->width() + 5;
  printButton->move(x, 5);
  x += printButton->width() + 5;
  helpButton->move(x, 5);
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
      rubberBandIsShown = true;
      rubberBandRect.setTopLeft(event->pos());
      rubberBandRect.setBottomRight(event->pos());
      updateRubberBandRegion();
      setCursor(Qt::CrossCursor);
    }
  }
}

void Plotter::mouseMoveEvent(QMouseEvent *event)
{
  if (rubberBandIsShown) {
    updateRubberBandRegion();
    rubberBandRect.setBottomRight(event->pos());
    updateRubberBandRegion();
  }
}

void Plotter::mouseReleaseEvent(QMouseEvent *event)
{
  if (event->button() == Qt::LeftButton && rubberBandIsShown) {
    rubberBandIsShown = false;
    updateRubberBandRegion();
    unsetCursor();

    QRect rect = rubberBandRect.normalized();
    if (rect.width() < 4 || rect.height() < 4)
      return;
    rect.translate(-Margin, -Margin);

    PlotSettings prevSettings = zoomStack[curZoom];
    PlotSettings settings;
    double dx = prevSettings.spanX() / (width() - 2 * Margin);
    double dy = prevSettings.spanY() / (height() - 2 * Margin);
    settings.minX = prevSettings.minX + dx * rect.left();
    settings.maxX = prevSettings.minX + dx * rect.right();
    settings.minY = prevSettings.maxY - dy * rect.bottom();
    settings.maxY = prevSettings.maxY - dy * rect.top();
    settings.adjust();

    zoomStack.resize(curZoom + 1);
    zoomStack.append(settings);
    zoomIn();
  }
}

/*
 * Use double-click to define zeros
 */
void Plotter::mouseDoubleClickEvent(QMouseEvent *event)
{
  DefocusFinder *finder = &mApp->defocusFinder;
  PlotSettings settings=zoomStack[curZoom];
  double dx=settings.spanX()/(width()-2*Margin);
  double zero=settings.minX + dx*(event->x()-Margin);
  double defocus, defoc2, defocAvg;

  // For a left button, redefine the first zero
  if (event->button() == Qt::LeftButton) {
    finder->setZero(zero);
    finder->findDefocus(&defocus);
    finder->setAvgDefocus(-1.);
    manageLabels(zero, defocus, 0., 0., 0);
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
  if (type >= 0) {
    zeroString.sprintf("Z: %4.3f", zero);
    defocusString.sprintf("D%s: %4.2f", type ? "1" : "", defocus);
  }
  if (type > 0) {
    defoc2String.sprintf("D2: %4.2f", def2);
    defocAvgString.sprintf("D-avg: %4.2f", defAvg);
  }
  zeroLabel->setText(zeroString);
  defocusLabel->setText(defocusString);
  defoc2Label->setText(defoc2String);
  defocAvgLabel->setText(defocAvgString);
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
    zoomStack[curZoom].scroll(-1, 0);
    refreshPixmap();
    break;
  case Qt::Key_Right:
    zoomStack[curZoom].scroll(+1, 0);
    refreshPixmap();
    break;
  case Qt::Key_Down:
    zoomStack[curZoom].scroll(0, -1);
    refreshPixmap();
    break;
  case Qt::Key_Up:
    zoomStack[curZoom].scroll(0, +1);
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
    zoomStack[curZoom].scroll(numTicks, 0);
  else
    zoomStack[curZoom].scroll(0, numTicks);
  refreshPixmap();
}

void Plotter::updateRubberBandRegion()
{
    QRect rect = rubberBandRect.normalized();
    update(rect.left(), rect.top(), rect.width(), 1);
    update(rect.left(), rect.top(), 1, rect.height());
    update(rect.left(), rect.bottom(), rect.width(), 1);
    update(rect.right(), rect.top(), 1, rect.height());
}

void Plotter::refreshPixmap()
{
    pixmap=QPixmap(size());
    //pixmap.fill(this, 0, 0);
    pixmap.fill(QColor(64,64,64));
    QPainter painter(&pixmap);
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
  QRect rect(Margin, Margin,
             width() - 2 * Margin, height() - 2 * Margin);
  if(!rect.isValid()) return;
  PlotSettings settings = zoomStack[curZoom];
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
    painter->drawText(x - 50, rect.bottom() + 5, 100, 15,
                      Qt::AlignHCenter | Qt::AlignTop,
                      QString::number(label));
  }
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
  PlotSettings settings = zoomStack[curZoom];
  QRect rect(Margin, Margin,
             width() - 2 * Margin, height() - 2 * Margin);

  if(!rect.isValid()) return;

  painter->setClipRect(rect.adjusted(+1, +1, -1, -1) );
    
  QMapIterator<int, QVector<QPointF> > i(curveMap);
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

/*

   $Log$
   Revision 1.11  2009/02/24 23:24:50  xiongq
   fix printing color

   Revision 1.10  2009/01/15 16:31:36  mast
   Qt 4 port

   Revision 1.9  2008/11/08 21:54:04  xiongq
   adjust plotter setting for initializaion

   Revision 1.8  2008/11/07 17:26:24  xiongq
   add the copyright heading

*/

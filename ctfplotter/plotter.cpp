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

#include <qapplication.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qpainter.h>
#include <qstyle.h>
#include <qtoolbutton.h>
#include <qprinter.h>
#include <qtooltip.h>
#include <qerrormessage.h>

#include <iostream>
#include <cmath>
using namespace std;

#include "rangedialog.h"
#include "angledialog.h"
#include "plotter.h"
#include "myapp.h"
#include "ctfmain.h"

#include "b3dutil.h"

Plotter::Plotter(QWidget *parent, const char *name, WFlags flags)
    : QWidget(parent, name, flags )
{
    setBackgroundMode(PaletteDark);
    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    setFocusPolicy(StrongFocus);
    rubberBandIsShown = false;
    rDialog=0;
    aDialog=0;

    zoomInButton = new QToolButton(this);
    zoomInButton->setIconSet(QPixmap::fromMimeSource("zoomin.png"));
    zoomInButton->adjustSize();
    QToolTip::add( zoomInButton, "Zoom in" );
    connect(zoomInButton, SIGNAL(clicked()), this, SLOT(zoomIn()));

    zoomOutButton = new QToolButton(this);
    zoomOutButton->setIconSet( QPixmap::fromMimeSource("zoomout.png"));
    zoomOutButton->adjustSize();
    QToolTip::add( zoomOutButton, "Zoom out" );
    connect(zoomOutButton, SIGNAL(clicked()), this, SLOT(zoomOut()));

    printButton= new QToolButton(this);
    printButton->setIconSet( QPixmap::fromMimeSource("printer.png") );
    printButton->adjustSize();
    QToolTip::add( printButton, "Print" );
    connect(printButton, SIGNAL(clicked()), this, SLOT(printIt()) );

    helpButton= new QToolButton(this);
    helpButton->setIconSet( QPixmap::fromMimeSource("ctfhelp.png") );
    helpButton->adjustSize();
    QToolTip::add( helpButton, "Help");
    connect(helpButton, SIGNAL(clicked()), this, SLOT(ctfHelp()) );

    saveButton=new  QToolButton(this);
    saveButton->setIconSet( QPixmap::fromMimeSource("save.png") );
    saveButton->adjustSize();
    QToolTip::add( saveButton, "Save" );
    connect(saveButton, SIGNAL(clicked()), this, SLOT(saveIt()) );
    
    rangeButton=new QToolButton(this);
    rangeButton->setIconSet( QPixmap::fromMimeSource("range.png") );
    rangeButton->adjustSize();
    QToolTip::add( rangeButton, "Set fitting ranges and methods" );
    connect(rangeButton, SIGNAL(clicked()), this, SLOT(rangeDiag()) );

    angleButton=new QToolButton(this);
    angleButton->setIconSet( QPixmap::fromMimeSource("angle.png") );
    angleButton->adjustSize();
    QToolTip::add( angleButton, "Set the angle range and expected defocus" );
    connect(angleButton, SIGNAL(clicked()), this, SLOT(angleDiag()) );

    tileButton=new QToolButton(this);
    tileButton->setIconSet(QPixmap::fromMimeSource("moreTile.png") );
    tileButton->adjustSize();
    QToolTip::add(tileButton, "Include all of the rest tiles" );
    connect(tileButton, SIGNAL(clicked()), qApp, SLOT(moreTileCenterIncluded()) );

    zeroLabel=new QLabel( tr("Z: NA     "), this);
    zeroLabel->adjustSize();
    defocusLabel=new QLabel( tr("D: NA     "), this);
    defocusLabel->adjustSize();

    printer= new QPrinter;
    setPlotSettings(PlotSettings());
}

Plotter::~Plotter(){
  delete printer;
}

void Plotter::setPlotSettings(const PlotSettings &settings)
{
    zoomStack.resize(1);
    zoomStack[0] = settings;
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
    if (curZoom < (int)zoomStack.size() - 1) {
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
  rDialog->setActiveWindow();
}

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
    double lowAngle=((MyApp *)qApp)->getLowAngle();
    double highAngle=((MyApp*)qApp)->getHighAngle();
    double defTol=((MyApp*)qApp)->getDefocusTol();
    int    tSize=((MyApp*)qApp)->getTileSize();
    double axisAngle=((MyApp*)qApp)->getAxisAngle();
    double leftTol=((MyApp*)qApp)->getLeftTol();
    double rightTol=((MyApp*)qApp)->getRightTol();
    char tmpStr[20];
    sprintf(tmpStr, "%6.2f", expDefocus); 
    aDialog->defocusEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", lowAngle); 
    aDialog->lowAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", highAngle); 
    aDialog->highAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", defTol); 
    aDialog->defTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6d", tSize);
    aDialog->tileSizeEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", axisAngle); 
    aDialog->axisAngleEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", leftTol); 
    aDialog->leftTolEdit->setText(tmpStr);
    sprintf(tmpStr, "%6.2f", rightTol); 
    aDialog->rightTolEdit->setText(tmpStr);
  }
  aDialog->show();
  aDialog->raise();
  aDialog->setActiveWindow();
}

void Plotter::printIt()
{
  if ( printer->setup( this ) ) {
    //QPainter painter(&pixmap, this);
    QPainter painter;
    if( !painter.begin( printer ) ) return;
    painter.setWindow(0, 0, width(), height() );
    drawGrid(&painter);
    drawCurves(&painter);
  }
}

void Plotter::ctfHelp()
{
  ctfShowHelpPage("ctfHelp/ctfguide.html");
}

void Plotter::saveIt()
{
    
    char *defFn=((MyApp *)qApp)->getDefFn();
    FILE *fp, *saveFp;

    saveFp=((MyApp *)qApp)->getSaveFp();
    if(!saveFp){
      imodBackupFile(defFn);
      fp=fopen(defFn,"w");
      ((MyApp *)qApp)->setSaveFp(fp);
    }else fp=fopen(defFn, "a");

    if(!fp) {
      QErrorMessage* errorMessage = new QErrorMessage( this );
      errorMessage->message( "Can not open output file" );
      return;
    }
    int startingSlice=((MyApp *)qApp)->getStartingSliceNum();
    int endingSlice=((MyApp *)qApp)->getEndingSliceNum();
    double lAngle=((MyApp *)qApp)->getLowAngle();
    double hAngle=((MyApp *)qApp)->getHighAngle();
    double defocus=((MyApp *)qApp)->defocusFinder.getDefocus();
    fprintf(fp, "%d\t%d\t%5.2f\t%5.2f\t%6.0f\n", startingSlice, endingSlice,
       lAngle, hAngle, defocus*1000);

    //((MyApp *)qApp)->saveAllPs();

    fclose(fp);
    
}

void Plotter::setCurveData(int id, const CurveData &data)
{
    curveMap[id] = data;
    int n=curveMap[id].size() / 2;
    int i;
    double min, max;

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
    curveMap.erase(id);
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
    QMemArray<QRect> rects = event->region().rects();
    for (int i = 0; i < (int)rects.size(); ++i)
        bitBlt(this, rects[i].topLeft(), &pixmap, rects[i]);

    QPainter painter(this);

    if (rubberBandIsShown) {
        painter.setPen(colorGroup().light());
        painter.drawRect(rubberBandRect.normalize());
    }
    if (hasFocus()) {
        style().drawPrimitive(QStyle::PE_FocusRect, &painter,
                              rect(), colorGroup(),
                              QStyle::Style_FocusAtBorder,
                              colorGroup().dark());
    }
}

void Plotter::resizeEvent(QResizeEvent *)
{
    int x = width() - (zoomInButton->width()
                       + zoomOutButton->width() +printButton->width() +
                       rangeButton->width()+zeroLabel->width()+
                       defocusLabel->width() + tileButton->width()+
                       angleButton->width()+ saveButton->width()+75);
    zeroLabel->move(x, 5);
    defocusLabel->move(x+zeroLabel->width()+5, 5);
    zoomInButton->move(x+zeroLabel->width()+defocusLabel->width()+10, 5);
    zoomOutButton->move(x +
        zeroLabel->width()+defocusLabel->width()+zoomInButton->width() + 15, 5);
    rangeButton->move(x+zeroLabel->width()+defocusLabel->width()+
        zoomInButton->width()+zoomOutButton->width()+20, 5);
    angleButton->move(x+zeroLabel->width()+defocusLabel->width()+
        zoomInButton->width()+zoomOutButton->width()+rangeButton->width()+25,5);
    tileButton->move(x+zeroLabel->width()+defocusLabel->width()+
        zoomInButton->width()+zoomOutButton->width()+
        rangeButton->width()+angleButton->width()+30, 5);
    saveButton->move(x+zeroLabel->width()+defocusLabel->width()+
        zoomInButton->width()+zoomOutButton->width()+
        rangeButton->width()+angleButton->width()+
        tileButton->width()+35, 5);
    printButton->move(x+zeroLabel->width()+defocusLabel->width()+
        zoomInButton->width()+zoomOutButton->width()+
        rangeButton->width()+angleButton->width()+
        tileButton->width()+saveButton->width()+40, 5);
    helpButton->move(x+zeroLabel->width()+defocusLabel->width()+
        zoomInButton->width()+zoomOutButton->width()+
        rangeButton->width()+angleButton->width()+
        tileButton->width()+saveButton->width() +
        printButton->width()+45, 5);
    refreshPixmap();
}

void Plotter::mousePressEvent(QMouseEvent *event)
{
    if (event->button() == LeftButton) {
        rubberBandIsShown = true;
        rubberBandRect.setTopLeft(event->pos());
        rubberBandRect.setBottomRight(event->pos());
        updateRubberBandRegion();
        setCursor(crossCursor);
    }
}

void Plotter::mouseMoveEvent(QMouseEvent *event)
{
    if (event->state() & LeftButton) {
        updateRubberBandRegion();
        rubberBandRect.setBottomRight(event->pos());
        updateRubberBandRegion();
    }
}

void Plotter::mouseReleaseEvent(QMouseEvent *event)
{
    if (event->button() == LeftButton) {
        rubberBandIsShown = false;
        updateRubberBandRegion();
        unsetCursor();

        QRect rect = rubberBandRect.normalize();
        if (rect.width() < 4 || rect.height() < 4)
            return;
        rect.moveBy(-Margin, -Margin);

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
        zoomStack.push_back(settings);
        zoomIn();
    }
}

void Plotter::mouseDoubleClickEvent(QMouseEvent *event)
{
  PlotSettings settings=zoomStack[curZoom];
  double dx=settings.spanX()/(width()-2*Margin);
  double zero=settings.minX + dx*(event->x()-Margin);
  ((MyApp*)qApp)->defocusFinder.setZero(zero);
  double defocus;
  ((MyApp*)qApp)->defocusFinder.findDefocus(&defocus);

  char zeroString[20]="Z: NA ";
  char defocusString[20]="D: NA ";
  sprintf(zeroString, "Z: %4.3f", zero);
  sprintf(defocusString, "D: %4.2f", defocus);
  zeroLabel->setText(zeroString);
  defocusLabel->setText(defocusString);
}

void Plotter::keyPressEvent(QKeyEvent *event)
{
    switch (event->key()) {
    case Key_Plus:
        zoomIn();
        break;
    case Key_Minus:
        zoomOut();
        break;
    case Key_Left:
        zoomStack[curZoom].scroll(-1, 0);
        refreshPixmap();
        break;
    case Key_Right:
        zoomStack[curZoom].scroll(+1, 0);
        refreshPixmap();
        break;
    case Key_Down:
        zoomStack[curZoom].scroll(0, -1);
        refreshPixmap();
        break;
    case Key_Up:
        zoomStack[curZoom].scroll(0, +1);
        refreshPixmap();
        break;
    default:
        QWidget::keyPressEvent(event);
    }
}

void Plotter::wheelEvent(QWheelEvent *event)
{
    int numDegrees = event->delta() / 8;
    int numTicks = numDegrees / 15;

    if (event->orientation() == Horizontal)
        zoomStack[curZoom].scroll(numTicks, 0);
    else
        zoomStack[curZoom].scroll(0, numTicks);
    refreshPixmap();
}

void Plotter::updateRubberBandRegion()
{
    QRect rect = rubberBandRect.normalize();
    update(rect.left(), rect.top(), rect.width(), 1);
    update(rect.left(), rect.top(), 1, rect.height());
    update(rect.left(), rect.bottom(), rect.width(), 1);
    update(rect.right(), rect.top(), 1, rect.height());
}

void Plotter::refreshPixmap()
{
    pixmap.resize(size());
    pixmap.fill(this, 0, 0);
    QPainter painter(&pixmap, this);
    drawGrid(&painter);
    drawCurves(&painter);
    update();
}

void Plotter::drawGrid(QPainter *painter)
{
    QRect rect(Margin, Margin,
               width() - 2 * Margin, height() - 2 * Margin);
    PlotSettings settings = zoomStack[curZoom];
    QPen quiteDark = colorGroup().dark().light();
    QPen light = colorGroup().light();

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
                          AlignHCenter | AlignTop,
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
                          AlignRight | AlignVCenter,
                          QString::number(label));
    }
    painter->drawRect(rect);
}

void Plotter::drawCurves(QPainter *painter)
{
    static const QColor colorForIds[6] = {
        red, green, blue, cyan, magenta, yellow
    };
    PlotSettings settings = zoomStack[curZoom];
    QRect rect(Margin, Margin,
               width() - 2 * Margin, height() - 2 * Margin);

    painter->setClipRect(rect.x() + 1, rect.y() + 1,
                         rect.width() - 2, rect.height() - 2);

    map<int, CurveData>::const_iterator it = curveMap.begin();
    while (it != curveMap.end()) {
        int id = (*it).first;
        const CurveData &data = (*it).second;
        int numPoints = 0;
        int maxPoints = data.size() / 2;
        QPointArray points(maxPoints);

        for (int i = 0; i < maxPoints; ++i) {
            double dx = data[2 * i] - settings.minX;
            double dy = data[2 * i + 1] - settings.minY;
            double x = rect.left() + (dx * (rect.width() - 1)
                                         / settings.spanX());
            double y = rect.bottom() - (dy * (rect.height() - 1)
                                           / settings.spanY());
            if (fabs(x) < 32768 && fabs(y) < 32768) {
                points[numPoints] = QPoint((int)x, (int)y);
                ++numPoints;
            }
        }
        points.truncate(numPoints);
        painter->setPen(colorForIds[(uint)id % 6]);
        painter->drawPolyline(points);
        ++it;
    }
}

PlotSettings::PlotSettings()
{
    minX = 0.0;
    maxX = 1.0;
    numXTicks = 5;

    minY = -0.3;
    maxY = 20.0;
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
*/

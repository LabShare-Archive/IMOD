#include <qwidget.h>
#include <qpainter.h>
#include <qpen.h>
#include <qcolor.h>

#include "histwidget.h"

HistWidget::HistWidget(QWidget *parent): QWidget(parent)
{
}

void HistWidget::setMinMax()
{
  minHist=hist[0];
  maxHist=hist[0];
  for(int i=1;i<256;i++){
     if(hist[i]>maxHist) maxHist=hist[i];
     if(hist[i]<minHist) minHist=hist[i];
   }
}

float HistWidget::computePercentile(float percentile)
{
  int i=0;
  float currPercent=0;
  
  while( i<256 && currPercent<percentile)
  {
    currPercent+=hist[i];
    i++;
  }
   
  return i+0.5;
}

void HistWidget::paintEvent(QPaintEvent *)
{
   QPainter painter(this);
   QColor black(0,0,0);
   QPen mypen(black);
   painter.setPen(mypen);
   int xRange=size().width();
   int yRange=size().height();
   float xCoord;
   int i;

   mypen.setWidth(xRange/255);
   for(i=0;i<256;i++)
   { 
     xCoord=xRange*i/256.0;
     painter.drawLine((int)xCoord,
         (int)(yRange*(maxHist-hist[i])/(maxHist-minHist) ), (int)xCoord, yRange);
   }
}


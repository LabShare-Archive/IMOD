/*
 *  histwidget.cpp - To draw the histogram widget for isosurface dialog
 *
 *  Author: Quanren Xiong   email: xiongq@colorado.edu
 *
 *  Copyright (C) 1995-2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <qwidget.h>
#include <qpainter.h>
#include <qpen.h>
#include <qcolor.h>
//Added by qt3to4:
#include <QPaintEvent>

#include "histwidget.h"

HistWidget::HistWidget(QWidget *parent): QWidget(parent)
{
}

void HistWidget::setHistMinMax()
{
  int i, maxbin1 = 0, maxbin2 = -1;
  float max2 = -1, max3 = -1;
  minHist=hist[0];
  maxHist=hist[0];
  for(i=1;i<256;i++){
    if(hist[i]>maxHist) {
      maxHist=hist[i];
      maxbin1 = i;
    }
    if(hist[i]<minHist) minHist=hist[i];
  }

  // Find the third highest bin and limit the max to somewhat higher than it
  for (i=0; i < 256; i++) {
    if (i != maxbin1 && hist[i] > max2) {
      max2 = hist[i];
      maxbin2 = i;
    }
  }
  for (i=0; i < 256; i++) {
    if (i != maxbin1 && i != maxbin2 && hist[i] > max3)
      max3 = hist[i];
  }
  if (max3 > 0 && maxHist > 1.1 * max3)
    maxHist = 1.1 * max3;
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
   QPen mypen(Qt::black);
   painter.setPen(mypen);
   int xRange=size().width();
   int yRange=size().height();
   float xCoord;
   int i;

   mypen.setWidth(xRange/(max-min+1));
   for(i=min;i<=max;i++)
   { 
     xCoord=xRange*(i-min)/(max-min);
     painter.drawLine((int)xCoord,
         (int)(yRange*(maxHist-hist[i])/(maxHist-minHist) ), (int)xCoord, yRange);
   }
}

/*

$Log$
Revision 4.3  2008/10/21 14:52:07  mast
Use third highest bin to limit the scaling


*/

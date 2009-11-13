

//############################################################

#include "ivector.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "_common_functions.h"
#include "imodplugin.h"
#include <vector>
using namespace std;

//############################################################

//------------------------
//-- Default constructor

Ivector::Ivector() {
  reset();
}

//------------------------
//-- Default constructor

Ivector::Ivector( Ipoint startPt, Ipoint endPt )
{
  setPt( &ptS, startPt.x,startPt.y,startPt.z);
  setPt (&ptE, endPt.x,endPt.y,endPt.z);
  
  vector.x = ptE.x - ptS.x;
  vector.y = ptE.y - ptS.y;
  vector.z = 0;
}

//------------------------
//-- Default constructor (sets start and end points the same)

Ivector::Ivector( Ipoint startPt ) {
  setStartAndEndPtsSame( startPt.x, startPt.y, startPt.z );
}

//------------------------
//-- Default constructor (sets start and end points the same)

Ivector::Ivector( float startX, float startY, float startZ ) {
  setStartAndEndPtsSame( startX, startY, startZ );
}

//------------------------
//-- Resets all point values to zero

void Ivector::reset() {
  setPt( &ptS, 0,0,0);
  setPt( &ptE, 0,0,0);
  setPt( &vector, 0,0,0);
}

//------------------------
//-- Sets the start and end points to the given value

void Ivector::setStartAndEndPtsSame( float x, float y, float z ) {
  setPt( &ptS ,   x,y,z);
  setPt( &ptE ,   x,y,z);
  setPt( &vector, 0,0,0);
}

//------------------------
//-- Sets the start point only (without chaging the other values)

void Ivector::setStartPtOnly( Ipoint startPt ) {
  setPt( &ptS   , startPt.x,startPt.y,startPt.z);
}

//------------------------
//-- Sets the end point value (and updates "vector")

void Ivector::setEndPt( Ipoint endPt ) {
  setPt (&ptE , endPt.x,endPt.y,endPt.z);
  
  vector.x = ptE.x - ptS.x;
  vector.y = ptE.y - ptS.y;
  vector.z = 0;
}

//------------------------
//-- Sets the "vector" value (and updates the end point)

void Ivector::setVector( Ipoint v ) {
  vector.x = v.x;
  vector.y = v.y;
  vector.z = 0;
  ptE.x  = ptS.x + vector.x;
    ptE.y  = ptS.y + vector.y;
    ptE.z  = ptS.z + vector.z;
}

//------------------------
//-- Recalculates "vector" based on position of start and end points
void Ivector::recalcVector() {
  vector.x = ptE.x - ptS.x;
  vector.y = ptE.y - ptS.y;
    vector.z = 0;
}

//------------------------
//-- Returns the Z value of the start point

int Ivector::getZVal() {
  return (int)ptS.z;
}

//------------------------
//-- Outputs the values of "pt", "ptE" and "vector"

/*
	ostream& operator<< (ostream &os, const Ivector i) {
  os << " pt=" << i.pt << ", ptE=" << i.ptE << ", &vector=" << i.vector;
  return os;
	}
 */
//------------------------
//-- Less than operation based on the vectors' Z values

bool operator< (const Ivector &a, const Ivector &b)	{
  return (a.ptS.z < b.ptS.z);
}


//############################################################

//------------------------
//-- Default constructor

SimpleTransform::SimpleTransform() {
  reset();
}

//------------------------
//-- Resets all values to zero

void SimpleTransform::reset() {
  scaleX = 1.0;
  scaleY = 1.0;
  rotate = 0.0;
  translateX = 0.0;
  translateY = 0.0;
}

//------------------------
//-- Takes a point and changes it by the tranform by using: (1)scale, (2)rotate, (3)translate.

void SimpleTransform::adjustPtByTransform (Ipoint *pt, Ipoint *rotatePt)
{
  point_scalePtAboutPt2D( pt, rotatePt, scaleX, scaleY );
  
  point_rotatePointAroundPoint2D ( pt, rotatePt, rotate*DEGS_TO_RADS );
  
  pt->x += translateX;
  pt->y += translateY;
}

//------------------------
//-- Takes a point and changes it by the INVERSE of the tranform by using: (1)-translate, (2)-rotate, (3)-scale.

void SimpleTransform::adjustPtByTransformInverse(Ipoint *pt, Ipoint *rotatePt)
{
  pt->x -= translateX;
  pt->y -= translateY;
  
  point_rotatePointAroundPoint2D ( pt, rotatePt, (-rotate)*DEGS_TO_RADS );
  
  point_scalePtAboutPt2D( pt, rotatePt, fDiv(1.0,scaleX), fDiv(1.0,scaleY) );
}


//------------------------
//-- Takes a point and changes it by the tranform by using: (1)rotate, (2)translate.

void SimpleTransform::adjustPtByTransformMinusScale (Ipoint *pt, Ipoint *rotatePt)
{
  point_rotatePointAroundPoint2D ( pt, rotatePt, rotate*DEGS_TO_RADS );
  
  pt->x += translateX;
  pt->y += translateY;
}

//------------------------
//-- Takes a point and changes it by the INVERSE of the tranform by using: (1)-translate, (2)-rotate

void SimpleTransform::adjustPtByTransformInverseMinusScale (Ipoint *pt, Ipoint *rotatePt)
{
  pt->x -= translateX;
  pt->y -= translateY;
  
  point_rotatePointAroundPoint2D ( pt, rotatePt, (-rotate)*DEGS_TO_RADS );
}


//------------------------
//-- Add SimpleTransform to existing tranform but ignores the effect of scale

void SimpleTransform::addTransformsIgnoreScale (SimpleTransform t)
{
  rotate += t.rotate;
  translateX += t.translateX;
  translateY += t.translateY;
    
  cout << "t.rotate=" << t.rotate << endl;  //%%%%%%%%%%%%%%%%%%
}








//------------------------
//-- Determines the transform values to fit a deformed square/rectangle
//-- NOTE: The final rectange won't necesary have right angled corners, hence edge values are averaged to produce a "best fit".

void SimpleTransform::calculateBestFitTransformUsingCorners( Ivector vectBL, Ivector vectTL, Ivector vectTR, Ivector vectBR )
{
  //## CALCULATE AVERAGE SCALE CHANGE IN X & Y USING LENGTH OF EACH SIDE BEFORE & AFTER TRANFORMATION:
  
  double changeLenBottom = fDiv( line_distBetweenPts2D( &vectBL.ptE, &vectBR.ptE ), line_distBetweenPts2D( &vectBL.ptS, &vectBR.ptS ) );
  double changeLenTop    = fDiv( line_distBetweenPts2D( &vectTL.ptE, &vectTR.ptE ), line_distBetweenPts2D( &vectTL.ptS, &vectTR.ptS ) );
  double changeLenLeft   = fDiv( line_distBetweenPts2D( &vectBL.ptE, &vectTL.ptE ), line_distBetweenPts2D( &vectBL.ptS, &vectTL.ptS ) );
  double changeLenRight  = fDiv( line_distBetweenPts2D( &vectBR.ptE, &vectTR.ptE ), line_distBetweenPts2D( &vectBR.ptS, &vectTR.ptS ) );
  
  scaleX = ( changeLenBottom + changeLenTop   ) / 2.0;
  scaleY = ( changeLenLeft   + changeLenRight ) / 2.0;
  
  //**********
  
  //## CALCULATE AVERAGE ROTATION:
  
  double changeAngleBottom = line_getAngle2D( &vectBL.ptE, &vectBR.ptE ) - line_getAngle2D( &vectBL.ptS, &vectBR.ptS );
  double changeAngleTop    = line_getAngle2D( &vectTL.ptE, &vectTR.ptE ) - line_getAngle2D( &vectTL.ptS, &vectTR.ptS );
  double changeAngleLeft   = line_getAngle2D( &vectBL.ptE, &vectTL.ptE ) - line_getAngle2D( &vectBL.ptS, &vectTL.ptS );
  double changeAngleRight  = line_getAngle2D( &vectBR.ptE, &vectTR.ptE ) - line_getAngle2D( &vectBR.ptS, &vectTR.ptS );
  
  rotate = ( changeAngleTop + changeAngleBottom + (changeAngleLeft) + (changeAngleRight) ) / 4.0;
  
  //**********
  
  //## CALCULATE TRANSLATION IN X & Y USING AVERAGE X & Y VALUES:
  
  double origAvgXVal = ( vectBL.ptS.x + vectBR.ptS.x + vectTL.ptS.x + vectTR.ptS.x ) / 4.0;  // middle/average x value (original)
  double origAvgYVal = ( vectBL.ptS.y + vectBR.ptS.y + vectTL.ptS.y + vectTR.ptS.y ) / 4.0;  // middle/average y value (original)
  double newAvgXVal = ( vectBL.ptE.x + vectBR.ptE.x + vectTL.ptE.x + vectTR.ptE.x ) / 4.0;  // middle/average x value (deformed)
  double newAvgYVal = ( vectBL.ptE.y + vectBR.ptE.y + vectTL.ptE.y + vectTR.ptE.y ) / 4.0;  // middle/average y value (deformed)
  
  translateX = newAvgXVal - origAvgXVal;
  translateY = newAvgYVal - origAvgYVal;
}

//------------------------
//-- Outputs the transform values

ostream& operator<< (ostream &os, const SimpleTransform t) {
  os << " scaleX=    " << t.scaleX << " scaleY=    " << t.scaleY << endl;
  os << " rotate=    " << t.rotate << endl;
  os << " translateX=" << t.translateX << " translateY=" << t.translateY << endl;
  return os;
}


//############################################################
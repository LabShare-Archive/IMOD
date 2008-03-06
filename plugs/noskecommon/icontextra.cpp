/*
 *  icontextra.c -- Special file for contour functions I need to use in my
 *                  special plugins and do not exist in icont.h/icont.cpp
 */

/*****************************************************************************
*   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
*   Bioscience at the University of Queensland (Australia)                  *
*****************************************************************************/

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.3  2008/03/05 10:29:00  tempuser
    Cleaned code

    Revision 1.2  2008/02/21 07:33:42  tempuser
    Changed DBL_MAX

    Revision 1.1  2008/01/24 01:25:31  tempuser
    *** empty log message ***

    Revision 0.0  2008/2/25 15:45:41  mast
    Made special module to be used in IMOD

*/

/* include needed Qt headers and imod headers */


#include "icontextra.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "_common_functions.h"
#include "imodplugin.h"
#include <vector>
using namespace std;

//############################################################


//----------------------------------------------------------------------------
//## SPECIAL DATA STRUCTURES USED IN FUNCTIONS:
//----------------------------------------------------------------------------


//## IcontPtr
  
  IcontPtr::IcontPtr( Icont* cont_ )
  {
    cont = imodContourDup( cont_ );
  }
  
  IcontPtr::IcontPtr()
  {
    cont = imodContourNew();
  }
  /*
  IcontPtr::~IcontPtr()    //-- GAVE ERRORS, SO USE deleteContour INSTEAD
  {
    deleteContour();
  }
  */ 
  
  void IcontPtr::deleteContour()
  {
    if( cont != NULL )
    {
      imodContourDelete( cont );
      cont = NULL;
      //wprint("CONTOUR DETELETD\n");   //%%%%
    }
    else
    {
      //wprint("CONTOUR ALREADY DEL\n");   //%%%%
    }
  }


//## PtConnection    // used especially for getIntersectingPolygon function

  PtConnection::PtConnection( Ipoint _intercept ) {
    intercept = _intercept;
    cont1Idx = -1;
    cont2Idx = -1;
    included = false;
  }
  
  PtConnection::PtConnection( Ipoint _intercept, int _cont1Idx ) {
    intercept = _intercept;
    cont1Idx  = _cont1Idx;
    cont2Idx = -1;
    included = false;
  }
  
  //-- Used to sort points by y value - using x as a tie-breaker.
  bool operator< (const PtConnection &a, const PtConnection &b) {                            
    if ( a.intercept.y == b.intercept.y )
      return ( a.intercept.x <= b.intercept.x );
    return (a.intercept.y < b.intercept.y);
  }
  
  //-- Used to eliminate duplicates
  bool operator== (const PtConnection lhs, const PtConnection rhs) {              
    return ( lhs.intercept.x == rhs.intercept.x );
  }


//## IdxAndFloat     // used especially for getIntersectingPolygon function

  IdxAndFloat::IdxAndFloat(int _idx, float _dist) {
    idx  = _idx;
    dist = _dist;
  }
  //-- Test if distance is "less than"
  bool operator<(const IdxAndFloat &a, const IdxAndFloat &b) {                                
    return (a.dist < b.dist);
  }


//## IdxToSort     // used especially for getIntersectingPolygon function
  
  IdxToSort::IdxToSort() {
    idx  = 0;
    float1 = 0;
    float2 = 0;
  }
  IdxToSort::IdxToSort( int _idx, float _float1 ) {
    idx  = _idx;
    float1 = _float1;
    float2 = 0;
  }
  IdxToSort::IdxToSort( int _idx, float _float1, float _float2 ) {
    idx  = _idx;
    float1 = _float1;
    float2 = _float2;
  }
  //-- Test if distance is "less than"
  bool operator<(const IdxToSort &a, const IdxToSort &b) {            
    if ( a.float1 == b.float1 )
      return ( a.float2 <= b.float2 );
    return (a.float1 < b.float1);
  }


//############################################################




//----------------------------------------------------------------------------
//## POINT RELATED FUNCTIONS:
//----------------------------------------------------------------------------




//------------------------
//-- Rotates the point by "theta" around the "center" in X and Y

void point_rotatePointAroundPoint2D( Ipoint *pt, Ipoint *center, float theta )
{
  pt->x -= center->x;      //|-- translate point (so center is at origin)
  pt->y -= center->y;      //|
  
  float newX = pt->x*cos(theta) - pt->y*sin(theta);   //|-- determine new position 
  float newY = pt->x*sin(theta) + pt->y*cos(theta);   //|   after rotation
  pt->x = newX;
  pt->y = newY;
  
  pt->x += center->x;      //|-- translate back point  
  pt->y += center->y;      //|   (so center returns to its original position)
}


//------------------------
//-- "Scales" the point by shifting it away from the center along each axis

void point_scalePtAboutPt( Ipoint *pt, Ipoint *center,
                           float scaleX, float scaleY, float scaleZ )
{
  pt->x -= center->x;      //|-- translate point (so center is at origin)
  pt->y -= center->y;      //|
  pt->z -= center->z;      //|

  pt->x *= scaleX;      // "scale"/shift point in x
  pt->y *= scaleY;      // "scale"/shift point in y
  pt->z *= scaleZ;      // "scale"/shift point in y

  pt->x += center->x;      //|-- translate back point
  pt->y += center->y;      //|   (so center returns to its original position)
  pt->z += center->z;      //|
}

//------------------------
//-- "Scales" the point by shifting it away from the center by
//-- scaleX in X and scaleY in Y

void point_scalePtAboutPt2D( Ipoint *pt, Ipoint *center, float scaleX, float scaleY )
{
  pt->x -= center->x;      //|-- translate point (so center is at origin)
  pt->y -= center->y;      //|
  
  pt->x *= scaleX;      // "scale"/shift point in x
  pt->y *= scaleY;      // "scale"/shift point in y
  
  pt->x += center->x;      //|-- translate back point 
  pt->y += center->y;      //|   (so center returns to its original position)
}


//------------------------
//-- Used to calculate MINDIST between a point and 2 edges.
//-- (but does NOT check wrap around)

float point_distToNearestEdge(float val, float min, float max)
{
  if    ( val < min )  return (min - val); 
  else if ( val > max )  return (max - val);
  else          return (0.0);
}

//------------------------
//-- Used to calculate MINDIST between a point and 2 edges.
//--(but does NOT check wrap around)

bool point_distToBBox2D(Ipoint *pt, Ipoint *ll, Ipoint *ur)
{
  float distX = point_distToNearestEdge( pt->x, ll->x, ur->x );
  float distY = point_distToNearestEdge( pt->y, ll->y, ur->y );
  return sqrt( distX*distX + distY*distY );
}


//------------------------
//-- Checks if point is inside the given bounding box

bool point_isInsideBBox(Ipoint *pt, Ipoint *ll, Ipoint *ur, bool ignoreZ)
{
  return ( ( ignoreZ || isBetween(ll->z,pt->z,ur->z) )
        && isBetween(ll->x,pt->x,ur->x)
           && isBetween(ll->y,pt->y,ur->y));
}



//------------------------
//-- Calculates a value "fracIntoKf" of the distance between "p1" and "p2" key values
//-- according to the catmull-rom spline algorithm.
//-- It does this by first calculating the gradients at these two key values.
//-- See: http://www.mvps.org/directx/articles/catmull/
//--
//-- NOTE: This function is used in smooth interpolation.
//--
//--     p0           p1            p2                    p3
//--      o------------o-----#------o---------------------o
//--                         |
//--                  desired point
//--
//-- ACTUAL EQUATION:
//--
//--  q(t) = 0.5 * [   ( -p0  + 3*p1 - 3*p2 + p3 ) * t^3
//--             + ( 2*p0 - 5*p1 + 4*p2 - p3 ) * t^2
//--             + ( -p0  + p2               ) * t
//--         + ( 2*p1                    )       ]


float getValCatmullRom( float fracIntoKf, float p0, float p1, float p2, float p3,
                        float tensileFract )
{
  //## CALCULATE "FRACTION INTO KEYFRAME" SQUARED AND CUBED:
  float fracIntoKf_2 = fracIntoKf * fracIntoKf;
  float fracIntoKf_3 = fracIntoKf * fracIntoKf * fracIntoKf;
  
  //## CALCULATE VALUES:
  //calculate gradient at previous keyframe.
  float gPrev =    tensileFract*(p1 - p0)/1.0
        + tensileFract*(p2 - p1)/1.0;
  
  //calculate gradient at current keyframe.
  float gCurr =    tensileFract*(p2 - p1)/1.0
        + tensileFract*(p3 - p2)/1.0;
  
  //calculate value for current frame.
  float curr =    p1*(2*fracIntoKf_3 - 3*fracIntoKf_2 + 1)
          + p2*(3*fracIntoKf_2 - 2*fracIntoKf_3)
          + gPrev*(fracIntoKf_3 - 2*fracIntoKf_2 + fracIntoKf)
          + gCurr*(fracIntoKf_3 - fracIntoKf_2);
  return curr;
  
  //
  return   0.5 *  ( ( 2 * p1 )
          + (-p0  + p2)          * fracIntoKf
          + (2*p0  - 5*p1 + 4*p2 - p3)    * fracIntoKf*fracIntoKf
          + (-p0  + 3*p1 - 3*p2 + p3)    * fracIntoKf*fracIntoKf*fracIntoKf );
  //
}


//------------------------
//-- Calculates point "fracIntoKf" of the distance between points "p1" and "p2"
//-- according to the catmull-rom spline algorithm.
//-- See: getValCatmullRom()

Ipoint getPtCatmullRom( float fracIntoKf, Ipoint p0, Ipoint p1, Ipoint p2,
                        Ipoint p3, float tensileFract )
{
  Ipoint returnPt;
  returnPt.x = getValCatmullRom( fracIntoKf,  p0.x, p1.x, p2.x,  p3.x, tensileFract );
  returnPt.y = getValCatmullRom( fracIntoKf,  p0.y, p1.y, p2.y,  p3.y, tensileFract );
  returnPt.z = getValCatmullRom( fracIntoKf,  p0.z, p1.z, p2.z,  p3.z, tensileFract );
  return (returnPt);
}








//############################################################



//----------------------------------------------------------------------------
//## LINE RELATED FUNCTIONS:
//----------------------------------------------------------------------------




//------------------------
//-- Calculates the angle (in degrees) of a line from the horizontal
//-- (the angle formed when a ray projected left of the first point)
//-- NOTE: Returned angle will be between -180 and 180 degrees.

float line_getAngle2D ( Ipoint *linept1, Ipoint *linept2 )
{
  float oppY = linept2->y - linept1->y;
  float adjX = linept2->x - linept1->x;
  
  return (float)(RADS_TO_DEGS * atan2( oppY , adjX ) );
}

//------------------------
//-- Returns the angle, between 0 and 360 degrees, of a line from the horizontal.

float line_getAngle2DPos ( Ipoint *linept1, Ipoint *linept2 )
{
  return (float)fMod( (line_getAngle2D(linept1,linept2)+360), 360 );
}



//------------------------
//-- Returns the center of the minmum bounding rectangle around a contour.
//-- NOTE: Is a bit less expensive than "cont_getCentroid"

Ipoint line_getPtHalfwayBetween(Ipoint *pt1, Ipoint *pt2)
{
  Ipoint pt;
  pt.x = avg( pt1->x, pt2->x );
  pt.y = avg( pt1->y, pt2->y );
  pt.z = avg( pt1->z, pt2->z );
  return ( pt );
}

//------------------------
//-- Finds a point some fraction of the distance between pt1 and pt2 (ignoring Z)
//-- NOTE: Can also be used to return a point BEYOND the line
//-- (if the fractBetweenPts value is > 1 or < 0)

Ipoint line_findPtFractBetweenPts2D( const Ipoint *pt1, const Ipoint *pt2,
                                     float fractBetweenPts )
{
  float diffX = pt2->x - pt1->x;
  float diffY = pt2->y - pt1->y;
  
  Ipoint pt;
  pt.x = (fractBetweenPts*diffX) + pt1->x;
  pt.y = (fractBetweenPts*diffY) + pt1->y;
  pt.z = pt1->z;
  return pt;
}

//------------------------
//-- Finds a point some fraction of the distance between pt1 and pt2
//-- NOTE: Can also be used to return a point BEYOND the line
//-- (if the fractBetweenPts value is > 1 or < 0)

Ipoint line_findPtFractBetweenPts( const Ipoint *pt1, const Ipoint *pt2,
                                   const float fractBetweenPts )
{
  float diffX = pt2->x - pt1->x;
  float diffY = pt2->y - pt1->y;
  float diffZ = pt2->z - pt1->z;
  
  Ipoint pt;
  pt.x = (fractBetweenPts*diffX) + pt1->x;
  pt.y = (fractBetweenPts*diffY) + pt1->y;
  pt.z = (fractBetweenPts*diffZ) + pt1->z;
  return pt;
}

//------------------------
//-- Calculates the distance SQUARED between two points in x and y

float line_sqDistBetweenPts2D( Ipoint *pt1, Ipoint *pt2 )
{
  float diffX = pt2->x - pt1->x;      // |-- calculate distance along x & y axis
  float diffY = pt2->y - pt1->y;      // |
  return ( diffX*diffX + diffY*diffY );    // calculate distance squared
}

//------------------------
//-- Calculates the distance between two points in 2D space (z values are ignored)

float line_distBetweenPts2D( Ipoint *pt1, Ipoint *pt2 )
{
  float diffX = pt2->x - pt1->x;        // |-- calculate distance along x & y axis
  float diffY = pt2->y - pt1->y;        // |
  return sqrt( diffX*diffX + diffY*diffY );    // calculate distance
}



//------------------------
//-- Returns true if the point lines on the line
//-- NOTE: This is done by calculating angles, which is NOT the most efficient way.

bool line_isPointOnLine( Ipoint *pt, Ipoint *lineStart, Ipoint *lineEnd )
{
  return ( isBetween( lineStart->x, pt->x, lineEnd->x )
           && isBetween( lineStart->y, pt->y, lineEnd->y )
      && ( line_getAngle2D(lineStart,lineEnd) == line_getAngle2D(lineStart,pt) ) ); 
}





//------------------------
//-- Computes the cross product of the two vectors defined by points
//-- {(x1,y1), (x2,y2)} and {(x1,y1), (x3,y3)}.
//-- (i.e. two lines connected with pt1 in the middle).
//--                        
//--                         NOTE: If the cross product is < 0 then 
//--      pt3 o_                   the line formed by {pt1-pt2},{pt2,pt3}
//--            -_                 (with pt2 in the middle) forms a "left turn"
//--              -_
//--   pt2 o---------o pt1 < "middle"

float line_crossProduct3Points( Ipoint *pt1, Ipoint *pt2, Ipoint *pt3)
{
  return ((pt2->x - pt1->x)*(pt3->y - pt1->y)) - ((pt2->y - pt1->y)*(pt3->x - pt1->x));
}


//------------------------
//-- Calculates the angle theta (in DEGREES) between two connected lines
//-- defined by the points: {(x1,y1), (x2,y2)} and {(x2,y2), (x3,y3)}.
//-- (i.e. two lines connected with pt2 in the middle).
//-- The result will be between 0 and 360.
//--
//--      pt3 o
//--         /
//--        / theta
//--   pt2 o---------o pt1

float line_angleFormed3Pts( Ipoint *pt1, Ipoint *pt2, Ipoint *pt3  )
{
  float line1Ang = line_getAngle2D ( pt1, pt2 );
  float line2Ang = line_getAngle2D ( pt3, pt2 );
  return fMod( line2Ang-line1Ang, 360.0 );
}


//------------------------
//-- Takes two lines (four coordinates) and returns true, plus point of
//-- intersection if the lines cross each other - otherwise returns false.

bool line_doLinesCrossAndWhere( Ipoint *line1pt1, Ipoint *line1pt2,
                                Ipoint *line2pt1, Ipoint *line2pt2, Ipoint *intercept )
{
  float a1 = line1pt2->y-line1pt1->y;
  float b1 = line1pt1->x-line1pt2->x;
  float c1 = line1pt2->x*line1pt1->y - line1pt1->x*line1pt2->y;
                                                //{ a1*x + b1*y + c1 = 0 is line 1 }
  float a2 = line2pt2->y-line2pt1->y;
  float b2 = line2pt1->x-line2pt2->x;
  float c2 = line2pt2->x*line2pt1->y - line2pt1->x*line2pt2->y;
                                                //{ a2*x + b2*y + c2 = 0 is line 2 }
  float denom = a1*b2 - a2*b1;
  
  if (denom == 0)
    return false;
    
  intercept->x = (b1*c2 - b2*c1) / denom;
  intercept->y = (a2*c1 - a1*c2) / denom;
  intercept->z = line1pt1->z;
  
  return ( isBetween(line1pt1->x, intercept->x, line1pt2->x)
           && isBetween(line2pt1->x, intercept->x, line2pt2->x)
           && isBetween(line1pt1->y, intercept->y, line1pt2->y)
           && isBetween(line2pt1->y, intercept->y, line2pt2->y) );
}







//------------------------
//-- Takes five points which form two 3-point lines:
//--    {p1-pMid-p2} AND {p3-pMid-p4}
//-- Returns true if the first line appears to "kiss" the second -
//-- if it touches the other line but does NOT pass through it (example below).
//-- Returns false if the first line goes through the other.
//--
//--
//--        p3  p4
//--         o  o
//--         | /
//--         |/
//--  o======o======o     <-- this is a "kiss" because the line from p1-pMid-p2
//-- p2    pMid     p1        does not pass through the other line.

bool line_isKiss( Ipoint *pMid, Ipoint *p1,Ipoint *p2,    Ipoint *p3, Ipoint *p4 )
{
  float ang1 = line_getAngle2D( pMid, p1 );  // angle each line relative to EAST
  float ang2 = line_getAngle2D( pMid, p2 );
  float ang3 = line_getAngle2D( pMid, p3 );
  float ang4 = line_getAngle2D( pMid, p4 );
  
  float relAng1 = 0;
  float relAng2 = fMod( ang2 - ang1, 360.0 );    //|-- calculates angle of each 
  float relAng3 = fMod( ang3 - ang1, 360.0 );    //|   line relative to the
  float relAng4 = fMod( ang4 - ang1, 360.0 );    //|   first line
  
  return (    (relAng3 < relAng2 && relAng4 < relAng2 )
       || (relAng3 > relAng2 && relAng4 > relAng2 ) );  // determines if lines kiss.
}

//------------------------
//-- Takes six points which form two 3-point lines:  {a1-a2-a3} AND {b1-b2-b3}
//-- Returns true if the first line appears to "kiss" the second

bool line_twoKiss( Ipoint *a1, Ipoint *a2, Ipoint *a3,
                   Ipoint *b1, Ipoint *b2, Ipoint *b3 )
{
  if (a2 != b2)
    return false;
  return line_isKiss( a2,  a1,a3,  b1,b3 );
}





//############################################################

//----------------------------------------------------------------------------
//## EXTRA CONTOUR FUNCTIONS:
//----------------------------------------------------------------------------



//------------------------
//-- Returns 1 if the two contours have an identical set of points
//-- (i.e. same number of points in same position)

int cont_isEqual( Icont *cont1, Icont *cont2 )
{
  if( psize(cont1) != psize(cont2) )
    return false;
  
  for (int i=0; i<psize(cont1) && i<<psize(cont2); i++)
    if( !imodPointIsEqual( getPt(cont1,i), getPt(cont2,i) )  )
      return false;
  return true;
}

//------------------------
//-- Returns 1 if the contour has a point with the same x and y
//-- value as the given point

int cont_doesPtExistInCont( Icont *cont, Ipoint *pt )
{
  for (int i=0; i<psize(cont); i++)
    if( ptsEqual(pt, getPt(cont,i))  )
      return 1;
  return 0;
}


//------------------------
//-- Returns the radius which would give a circle of equal area to
//-- the contour provided.
//-- WARNING: Contour must be enclosed (must not cross it's own path).

float cont_getRadius( Icont *c )
{
  float area = imodContourArea( c );
  return sqrt(area/PI);
}


//------------------------
//-- Finds the closest point in the given contour to the given point
//-- and returns the distance, plus the index value and the point itself.

void cont_findClosestPtInContToGivenPt( Ipoint *pt, Icont *cont, float *closestDist,
                                        Ipoint *closestPt, int *closestPtIdx )
{
  float closestDist2 = FLOAT_MAX;
  
  for(int i=0; i<psize( cont ); i++ )
  {
    float sqDistToPt = line_sqDistBetweenPts2D( getPt(cont,i) , pt );
    if( closestDist2 > sqDistToPt ) {
      closestDist2 = sqDistToPt;
      *closestPtIdx = i;
    }
  }
  *closestPt = *getPt(cont,*closestPtIdx);
  *closestDist = sqrt(closestDist2);
}






//------------------------
//-- Checks if two contours touch each other (or if one is contained in the other)
//-- First checks if minimum bounding rectangles around each overlap,
//-- then if contours cross, and finally if one is inside the other

bool cont_doContsTouch( Icont *cont1, Icont *cont2 )
{
  if ( isEmpty(cont1) || isEmpty(cont2)  )
    return false;
  
  Ipoint cont1ll, cont1ur, cont2ll, cont2ur;
  Icont *cs1 = imodel_contour_scan( cont1 );
  Icont *cs2 = imodel_contour_scan( cont2 );
  
  imodContourGetBBox( cont1, &cont1ll, &cont1ur );
  imodContourGetBBox( cont2, &cont2ll, &cont2ur );
  return ( imodel_scans_overlap( cs1,cont1ll,cont1ur, cs2,cont2ll,cont2ur) );
  
  imodContourDelete( cs1 );
  imodContourDelete( cs2 );
}



//------------------------
//-- Finds the closest distance between the given point and any point
//-- in the contour.
//-- Note that if the point is INSIDE the contour and returnZeroIfPointIsInside
//-- is true the function will return 0.

float cont_minDistPtAndContourPts2D(Ipoint *pt, Icont *cont, bool returnZeroIfPtInside)
{
  if( returnZeroIfPtInside && imodPointInsideCont( cont, pt ) )
    return (0);
    
  float closestDist2 = FLOAT_MAX;
  
  for(int i=0; i<psize( cont ); i++ )
    updateMin ( closestDist2, line_sqDistBetweenPts2D( getPt(cont,i), pt ) );
  
  return sqrt(closestDist2);
}

//------------------------
//-- Finds the closest distance between any two points (ignoring Z axis)...
//-- but note that this is NOT necessarily the closest distance between
//-- the lines formed by both contours.

float cont_minDistBetweenContPts2D( Icont *cont1, Icont *cont2, bool returnZeroIfTouch )
{
  if( returnZeroIfTouch && cont_doContsTouch( cont1, cont2) )
    return 0;
  
  float closestDist2 = FLOAT_MAX;
  
  for(int i=0; i<psize(cont1); i++ )
    for(int j=0; j<psize(cont2); j++ )
      updateMin(closestDist2,line_sqDistBetweenPts2D(getPt(cont1,i),getPt(cont2,j)));
  
  return sqrt(closestDist2);
}




//------------------------
//-- Reorder points so that the point at index "idxNewFirstPt" become the
//-- first point in the contour

void cont_reorderPtsToStartAtIdx( Icont *c, int idxNewFirstPt )
{
  if( idxNewFirstPt==0 )
    return;
  int nPts = psize( c );
  if( idxNewFirstPt < 0 || idxNewFirstPt > nPts ) {    //%%%%%% SHOULD NEVER HAPPEN
    wprint( "ERROR: cont_reorderPtsToStartAtIdx() - bad index" );
  }
  Icont copyC;
  Icont *copy = &copyC;
  copy = imodContourDup( c );
  
  deleteAllPts( c );
  //imodContourDefault( c );
  for (int i=idxNewFirstPt; i<(nPts + idxNewFirstPt); i++)
    imodPointAppend( c, getPt( copy, i % nPts ) );
}

//------------------------
//-- Removes any points in cont that fall within the circle defined.

int cont_removePointsInCircle( Icont *cont, Ipoint *center,
                               float radius, bool checkZEachPoint )
{
  int radiusSq = (radius*radius);
  int numRemovedPoints = 0;
  
  for(int p=0; p<psize(cont); p++ )
  {
    if( !checkZEachPoint || getPt(cont,p)->z == center->z )
    {
      float distSq = line_sqDistBetweenPts2D( center, getPt(cont,p) );
      if( distSq < radiusSq  )
      {
        imodPointDelete( cont, p );
        p--;
        numRemovedPoints++;
      }
    }
  }
  
  return (numRemovedPoints);
}




//------------------------
//-- Translates a contour by a given amout.

void cont_translate( Icont *cont, Ipoint *translate )
{
  for(int i=0; i<psize( cont ); i++)
  {
    getPt(cont,i)->x += translate->x;
    getPt(cont,i)->y += translate->y;
    getPt(cont,i)->z += translate->z;
  }
}

//------------------------
//-- Translates (moves) a contour by a given amount in x and y.

void cont_translate( Icont *cont, float x, float y )
{
  Ipoint translate;
  translate.x = x;
  translate.y = y;
  translate.z = 0;
  cont_translate( cont, &translate );
}

//------------------------
//-- Rotates a contour by a given angle (in DEGREES) around specified
//-- rotate point (ignores Z axis).

void cont_rotateAroundPoint2D( Icont *cont, Ipoint *center, float angle )
{
  float theta = angle * DEGS_TO_RADS;
  for(int i=0; i<psize( cont ); i++)
    point_rotatePointAroundPoint2D( getPt(cont,i), center, theta );
}

//------------------------
//-- Scales a contour about a given point by scaleX in X and scaleY in Y.

void cont_scaleAboutPtXY( Icont *cont, Ipoint *center, float scaleX, float scaleY )
{
  for(int i=0; i<psize( cont ); i++)
    point_scalePtAboutPt2D( getPt(cont,i), center, scaleX, scaleY );
}


//------------------------
//-- Scales a contour about a given point by the given factors in each dimension

void cont_scaleAboutPt3D( Icont *cont, Ipoint *center, float scaleX,
                          float scaleY, float scaleFactorZ )
{
  for(int i=0; i<psize(cont); i++)
    point_scalePtAboutPt( getPt(cont, i), center, scaleX, scaleY, scaleFactorZ );
}

//------------------------
//-- Returns the contour after it has been scaled outwards/inwards

void cont_scaleAboutPt( Icont *cont, Ipoint *pt, float scaleFactor, bool ignoreZ )
{
  float zOrig = getZ( cont );
  
  for(int i=0; i<psize(cont); i++)
    *getPt(cont,i) = line_findPtFractBetweenPts( pt, getPt(cont,i), scaleFactor );
  
  if(ignoreZ)
    changeZValue( cont, (int)zOrig );
}

//------------------------
//-- Stretches the contour by "stretchFactor" about "center" along
//-- the given "angle".

void cont_stretchAlongAngle( Icont *cont, Ipoint *center,
                             float angle, float stretchFactor )
{
  cont_rotateAroundPoint2D( cont, center, -angle );
  cont_scaleAboutPtXY( cont, center, stretchFactor, 1.0 );
  cont_rotateAroundPoint2D( cont, center, angle );
}




//---------------------------------
//-- Generates a contour representing a circle with given center,
//-- radius and number of points.

void cont_generateCircle( Icont *cont, float radius, int numPoints,
                          Ipoint center, bool addEndPt )
{
  Ipoint pt;
  pt.z = center.z;
  
  for (int i = 0; i < numPoints; i++) {
    float angle = 2 * PI * ( (float)i / (float)numPoints );
    pt.x = center.x + radius * cos(angle);
    pt.y = center.y + radius * sin(angle);
    imodPointAppend(cont, &pt);
  }
  if(addEndPt)
  {
    pt.x = center.x + radius;
    pt.y = center.y;
    imodPointAppend(cont, &pt);
  }
}






//------------------------
//-- Counts the number of times two (closed) contours cross each other.
//-- NOTE: This number should ALWAYS be even.

int cont_numTimesCountsCross( Icont *cont1, Icont *cont2 )
{
  int numTimesLinesCross = 0;
  for (int i=0; i<=psize(cont1);i++)
    for (int j=0; j<=psize(cont2);j++)
      if( imodPointIntersect( getPt(cont1,i), getPt(cont1,i+1),
                              getPt(cont2,j), getPt(cont2,j+1) ) )
        numTimesLinesCross++;
        
  return numTimesLinesCross;
}



//------------------------
//-- Returns true if the two (closed) contours cross paths.

bool cont_doCountoursCross( Icont *cont1, Icont *cont2,
                            bool cont1Closed=true, bool cont2Closed=true )
{
  Ipoint interceptPt;
  
  int cont1PtsToCheck = (cont1Closed) ? psize(cont1): psize(cont1)-1;
  int cont2PtsToCheck = (cont2Closed) ? psize(cont2): psize(cont2)-1;
  
  for (int i=0; i<cont1PtsToCheck;i++)
    for (int j=0; j<cont2PtsToCheck;j++)
      if( imodPointIntersect( getPt(cont1,i), getPt(cont1,i+1),
                              getPt(cont2,j), getPt(cont2,j+1) ) )
        return true;
  return false;
}

//------------------------
//-- Returns true if the two (closed) contours cross paths.

bool cont_doCountoursCrossAndWhere( Icont *cont1, Icont *cont2,
                                    bool cont1Closed, bool cont2Closed,
                                    int *pt1BeforeCross, int *pt2BeforeCross )
{
  Ipoint interceptPt;
  
  int cont1PtsToCheck = (cont1Closed) ? psize(cont1): psize(cont1)-1;
  int cont2PtsToCheck = (cont2Closed) ? psize(cont2): psize(cont2)-1;
  
  for (int i=0; i<cont1PtsToCheck;i++)
    for (int j=0; j<cont2PtsToCheck;j++)
      if(imodPointIntersect(getPt(cont1,i),getPt(cont1,i+1),
                            getPt(cont2,j),getPt(cont2,j+1)))
      {
        *pt1BeforeCross = i;
        *pt2BeforeCross = j;
        return true;
      }
  return false;
}

//------------------------
//-- Determines if point touches the given contour LINE
//-- (i.e: if it is on an existing point or between two consecutive contour points)

bool cont_doesPtTouchContLine( Ipoint *pt, Icont *cont )
{
  for (int i=0; i<psize(cont); i++)
    if( pt == getPt(cont,i) )
      return true;
  for (int i=0; i<psize(cont); i++)
    if( line_isPointOnLine( pt, getPt(cont,i), getPt(cont,i+1)  ) )
      return true;
      
  return false;
}





//------------------------
//-- Finds and returns the closest two points (ignoring Z axis) and returns
//-- their distance apart.

float cont_findClosestPts2D( Icont *cont1, Icont *cont2,
                             int *closestPtIdxInCont1, int *closestPtIdxInCont2 )
{
  float closestDist2 = FLOAT_MAX;
  
  for(int i=0; i<psize(cont1); i++ )
    for(int j=0; j<psize(cont2); j++ )
    {
      float dist2 = line_sqDistBetweenPts2D( getPt(cont1,i), getPt(cont2,j) );
      if ( dist2 < closestDist2 ) {
        closestDist2 = dist2;
        *closestPtIdxInCont1 = i;
        *closestPtIdxInCont2 = j;
      }
    }
  return sqrt(closestDist2);
}

//------------------------
//-- Reverses the order of the points

void cont_reversePts( Icont *c )
{
  imodel_contour_invert( c );
}



//------------------------
//-- Joins two contours together to form a longer contour.
//-- If matchClosestEnds is true it will join the two ends of cont1 and cont2
//-- which are closest together... otherwise it will simply add the points
//-- from cont2 onto the end of cont1

void cont_concat( Icont *contNew, Icont *cont1, Icont *cont2Orig, bool matchClosestEnds )
{
  imodContourDefault( contNew );
  Icont *cont2 = imodContourDup( cont2Orig );
  
  if(matchClosestEnds) {
    float distCont1EndToCont2Beg = imodPointDistance(getLastPt(cont1),getFirstPt(cont2));
    float distCont1EndToCont2End = imodPointDistance(getLastPt(cont1),getLastPt(cont2));
    
    if( distCont1EndToCont2End < distCont1EndToCont2Beg )
      cont_reversePts( cont2 );
  }
  
  for(int i=0; i<psize(cont1); i++ )
    imodPointAppend( contNew, getPt( cont1, i) );
  for(int i=0; i<psize(cont2); i++ )
    imodPointAppend( contNew, getPt( cont2, i) );
  
  imodContourDelete(cont2);
  imodContourUnique(contNew);
}


//------------------------
//-- Adds points to the using a very simple technique - to ensure
//-- no two consecutive points are > maxDist apart.

void cont_addPtsCrude( Icont *cont, float maxDist, bool closed )
{
  float sqMaxDist = SQ( maxDist );
  
  int extra = (closed) ? 0 : -1;
  
  for(int i=0; i<psize(cont)+extra; i++ )
  {
    Ipoint *currPoint = getPt( cont, i );
    Ipoint *nextPoint = getPt( cont, i+1 );
    
    float distToNextPt = line_distBetweenPts2D( currPoint, nextPoint );
    if( distToNextPt > maxDist )
    {
      Ipoint newPt = line_getPtHalfwayBetween( currPoint, nextPoint );
      imodPointAdd( cont, &newPt, i+1 );
      i--;
    }
  }
}

//------------------------
//-- Smooths the contour by finding any occurance where two consecutive
//-- points are > maxDist away from each other and adding a SINGLE extra
//-- point in the middle using catumull-rom spline.
//-- NOTE: At the end there may still be consecutive points > maxDist
//-- apart (but will be closer than originally)

void cont_addPtsSmoothIteration(Icont *cont, float maxDist,
                                float tensileFract, bool closed)
{
  float sqMaxDist = SQ( maxDist );
  
  if( psize(cont) < 5 )
    return;
  
  if(closed)
  {
    for(int i=1; i<(psize(cont))+1; i++ ) {
      float sqDistToNextPt = line_sqDistBetweenPts2D( getPt(cont,i), getPt(cont,i+1) );
      if ( sqDistToNextPt > sqMaxDist )
      {
        Ipoint newPt = getPtCatmullRom(0.5, *getPt(cont,i-1), *getPt(cont,i),
                                       *getPt(cont,i+1), *getPt(cont,i+2), tensileFract);
        int insertIdx = i % psize(cont);
        imodPointAdd( cont, &newPt, (insertIdx)+1 );
        i++;        // causes point just added to be skipped
      }
    }
  }
  else
  {
    imodPointAdd( cont, getFirstPt(cont), 0 );
    imodPointAppend( cont, getLastPt(cont) );
    
    for(int i=1; i<(psize(cont))-1; i++ ) {
      float sqDistToNextPt = line_sqDistBetweenPts2D( getPtNoWrap(cont,i),
                                                      getPtNoWrap(cont,i+1) );
      if ( sqDistToNextPt > sqMaxDist )
      {
        Ipoint newPt = getPtCatmullRom(0.5, *getPtNoWrap(cont,i-1), *getPtNoWrap(cont,i),
                                       *getPtNoWrap(cont,i+1), *getPtNoWrap(cont,i+2),
                                       tensileFract);
        int insertIdx = i % psize(cont);
        imodPointAdd( cont, &newPt, (insertIdx)+1 );
        i++;        // causes point just added to be skipped
      }
    }
    imodContourUnique( cont );
  }
}

//------------------------
//-- Smooths the contour by finding any occurance where two consecutive
//-- points are > maxDist away from each other and adding MULTIPLE
//-- extra points in the middle using catumull-rom spline.
//-- NOTE: Result will be that NO two points are > maxDist from each other.

void cont_addPtsSmooth( Icont *cont, float maxDist, float tensileFract, bool closed )
{
  while (true)
  {
    int numPtsBeforeSmoothIteration = psize(cont);
    cont_addPtsSmoothIteration( cont, maxDist, tensileFract, closed );
    int numPtsAdded = psize(cont) - numPtsBeforeSmoothIteration;
    if (numPtsAdded == 0)
      break;
  }
}

//------------------------
//-- Reduces the number of points in a contour using a VERY simple technique -
//-- by removing any point which is < minDist from the previous point.

void cont_reducePtsCrude( Icont *cont, float minDist, bool closed )
{
  float sqMinDist = SQ( minDist );
  
  int extra = (closed) ? 1 : 0;
  for(int i=1; i<psize(cont)+extra; i++ ) {
    float sqDistFromPrevPoint = line_sqDistBetweenPts2D(getPt(cont,i-1),getPt(cont,i));
    if ( sqDistFromPrevPoint < sqMinDist ) {
      imodPointDelete( cont, i );
      i--;
    }
  }
}

//------------------------
//-- Reduces the number of points in a contour by removing points
//-- which form an area < minArea when a triangle if formed with that point,
//-- the point before it, and the point after.
//-- Returns the number of points deleted.

int cont_reducePtsMinArea( Icont *cont, float minArea, bool closed )
{
  int numPointsDeleted = 0;
  
  int extra = (closed) ? 0 : -1;
  for(int i=1; i<psize(cont)+extra && psize(cont)>3; i++ )
  {
    Ipoint *p1 = getPt(cont,i-1);    //|-- construct a triangle with the current pt
    Ipoint *p2 = getPt(cont,i);      //|   plus the pt before and after it.
    Ipoint *p3 = getPt(cont,i+1);    //|
        
    if ( imodPointArea( p1, p2, p3 ) < minArea ) {  
      imodPointDelete( cont, i );
      numPointsDeleted++;
      i--;
    }
  }
  
  return (numPointsDeleted);
}




































//------------------------
//-- Will return true if the contour is a "simple polygon"
//-- (none of it's lines overlap) or false if it is complex
//-- (i.e. two or more lines intersect)

bool cont_isSimple( Icont *cont )
{
  for(int i=0; i<psize(cont); i++ )
    for(int j=i+2; j<psize(cont); j++ )
      if(imodPointIntersect(getPt(cont,i),getPt(cont,i+1),getPt(cont,j),getPt(cont,j+1))
         && !( i == 0 && j == psize(cont)-1 ) )
        return false;
        
  return true;
}


//------------------------
//-- Takes a contour and (if not already) makes it simple.
//-- It does this by determine points where the contour intersects itself,
//-- and deleting the SMALLER enclosed area (as computed by the fewest
//-- number of points) until no more self-intersections are found.
//-- NOTE: This function is still not perfect, but does the right
//-- thing 90% of the time.

void cont_makeSimple( Icont *cont )
{
  int numIntersects = 0;
  imodContourUnique( cont );
  
  findIntersect:            // LABEL (see "goto findIntersect")
  
  for(int i=0; i<psize(cont); i++ )
    for(int j=i+2; j<psize(cont); j++ )
    {
      if( i == 0 && j == psize(cont)-1 ) continue;
      
      Ipoint intersectPt;
      bool intersectionFound =
        line_doLinesCrossAndWhere( getPt(cont,i),getPt(cont,i+1),
                                   getPt(cont,j),getPt(cont,j+1),&intersectPt );
      
      if ( intersectionFound )
      {
        int numPtsToDelete = j-i;
        
                // if the region between the intersecting lines has more points:     
                // delete points in middle and add intersection point
        
        if ( numPtsToDelete < (psize(cont) / 2) )   
        {
          for (int p=0; p<(numPtsToDelete); p++)
            imodPointDelete( cont, i+1 );
          imodPointAdd(cont, &intersectPt, i+1);  
        }
                // else (if region on the ends has more points):  
                // delete points either end of intersecting pts and add intersection pt
        
        else
        {
          int altNumPtsToDelete = psize(cont)-(j+1);
          for (int p=0; p<(altNumPtsToDelete); p++)
            imodPointDelete( cont, j+1 );
          imodPointAdd(cont, &intersectPt, i+1);
          for (int p=0; p<(i+1); p++)
            imodPointDelete( cont, 0 );
        }
        
        numIntersects++;                      // |-- used to avoid rare cases where
        if(numIntersects>50) {                // |   problem occurs (not yet sure why)
          wprint( "ERROR: cont_makeSimple()\n" );
          return;
        }
        
        goto findIntersect;     // will go to beginning of double loop and
                                // see if there is another intersection.
      }
    }
    
  return;
}




//------------------------
//-- Takes a contour and breaks it into a series of simple contours
//-- (i.e. contours which don't overlap themselves) and returns these.
//--
//-- NOTE: If the given contour is already simple it will return itself.

int cont_breakIntoSimple( vector<IcontPtr> &conts, Icont *cont )
{
  conts.clear();
  conts.push_back( IcontPtr(cont) );
  
  if( cont_isSimple ( cont ) )
    return (conts.size());
  
  Ipoint intersectPt;
  Icont *newCont1 = imodContourNew();
  Icont *newCont2 = imodContourNew();
  
  for(int c=0; c<(int)conts.size(); c++ )
  {
    findIntersect:            // LABEL
    
    imodContourUnique( conts[c].cont );
    
    
    for(int i=0; i< psize(conts[c].cont); i++ )
    {
      for(int j=i+2; j< psize(conts[c].cont); j++ )
      {
        if( i == 0 && j == psize(conts[c].cont)-1 ) continue;
        
        
        if ( line_doLinesCrossAndWhere( getPt(conts[c].cont,i), getPt(conts[c].cont,i+1),
                                        getPt(conts[c].cont,j), getPt(conts[c].cont,j+1),
                                        &intersectPt ) )
        {
          imodPointAdd( conts[c].cont, &intersectPt, j+1);
          imodPointAdd( conts[c].cont, &intersectPt, i+1);
          
          cont_breakContourEitherSide(conts[c].cont,newCont1,newCont2,i+1,j+2,true);
          
          imodContourUnique( newCont1 );
          imodContourUnique( newCont2 );
          
          eraseContour( conts, c );
          conts.push_back( IcontPtr(newCont1) );
          conts.push_back( IcontPtr(newCont2) );
          
          goto findIntersect;     // will go to beginning of double loop 
                                  // and see if there is another intersection.
        }
      }
    }
    
  }
  
  imodContourDelete(newCont1);
  imodContourDelete(newCont2);
          
  return (conts.size());
}












//------------------------
//-- Determines if a contour is convex by checking that all the "turns"
//-- (the angle formed by the lines either side of each point)
//-- are in the same direction.

bool cont_isConvex( Icont *cont )
{
  if( psize(cont) < 3 )
    return false;
    
    
  int numRightTurns = 0;
  int numLeftTurns = 0;
  
  for (int i=1; i<(psize(cont)+1); i++ )
  {
    float crossProduct = line_crossProduct3Points(getPt(cont,i-1),
                                                  getPt(cont,i),getPt(cont,i+1)); 
      // calculates cross product using the line {i-1,i}
      // and line {i,i+1} to determine if a "left turn" is made
    
    if (crossProduct < 0)        // if turn is left: tally it
      numLeftTurns++;
    else if (crossProduct == 0)      // else if turn is straight: skip to next
      continue;
    else                // else (if turn is right): tally it
      numRightTurns++;
      
    if( numLeftTurns>0 && numRightTurns>0 )   // if NOT all turns are in same direction: 
      return false;                             // contour is not convex
  }
  
  return true;    // all turns are in the same direction: contour must be convex
}












//------------------------
//-- Computes a convex hull around a given set of points using
//-- "Graham Scan" (3-coins algorithm).
//-- This involves finding the lowest point (in y), sorting all points radially
//-- relative this point, then removing points which form a "left turn".

void cont_makeConvex( Icont *cont )
{
  if( psize(cont)<=3 || cont_isConvex(cont) )
    return;
    
//## FIND THE LOWEST POINT IN THE CONTOUR:
  
  int idxStartPt = 0;
  float lowestYVal   = FLOAT_MAX;
  for (int i=0; i< psize(cont); i++)  {
    if(getPt(cont,i)->y <= lowestYVal) // if this pt is below previously found lowest pt:
    {
             // if this point has same y value: use x as a tie-breaker
      if(getPt(cont,i)->y==lowestYVal && getPt(cont,i)->y > getPt(cont,idxStartPt)->y)   
        continue;
      lowestYVal = getPt(cont,i)->y;          // update as new lowest point.
      idxStartPt = i;
    }
  }
  
//## SORT POINTS RADIALLY FROM THE LOWEST POINT:
  
  vector<IdxToSort> idxAngles;
  for (int i=0; i< psize(cont); i++)
    if(i!=idxStartPt)
    {
                 // calculate the angle and distance from the lowest pt to this pt
      idxAngles.push_back(IdxToSort(i,
            line_getAngle2DPos(getPt(cont,idxStartPt),getPt(cont,i)), 
            FLOAT_MAX-imodPointDistance(getPt(cont,idxStartPt),getPt(cont,i)))); 
    }
  //std::sort( idxAngles.begin(), idxAngles.end() );                // sort angles
  idxAngles = vector_sort( idxAngles );
  
  Icont *contSorted;     // will store a list of pts sorted radially from the start pt
  imodPointAppend( contSorted, getPt(cont,idxStartPt) );
  for (int i=0; i<(int)idxAngles.size(); i++)
    imodPointAppend( contSorted, getPt(cont,idxAngles[i].idx ) );
  
//## ITERATE THROUGH LIST AND REMOVE ANY POINTS WHICH MAKE A LEFT TURN:
  
  for (int i=1; i<( psize(contSorted)); i++ )
  {
    int idx = (i+psize(contSorted))%psize(contSorted);  // make sure idx stays positive
       // use cross product form between the line P(i-1)-P(i) and line P(i)-P(i+1)
       // to determine if a "left turn" is made
    bool makesLeftTurn =
      (line_crossProduct3Points(getPt(contSorted,idx-1),getPt(contSorted,idx),
                                getPt(contSorted,idx+1)) < 0);
    if( makesLeftTurn ) {                         // if this point makes a left turn
      imodPointDelete( contSorted, idx );           // remove this point
      i=i-2;                                        // go back to the previous point
    }
  }
}






//------------------------
//-- Takes a single contours and breaks it into two contours at the
//-- specified index points (idxPt1 and idxPt2).
//--
//-- If shareEdge is false:
//--  the second contour will include the points between idxPt1 and idxPt2-1
//--  and the  first contour will include the pts between idxPt2 and idxPt1-1
//-- If shareEdge is set to true:
//--  both contours will include the pt at idxPt1 and idxPt2 and hence
//--  share that as a common edge

bool cont_breakContourEitherSide( Icont *cont, Icont *contBreak1, Icont *contBreak2,
                                  int idxPt1, int idxPt2, bool shareEdge=true )
{
  imodContourDefault( contBreak1 );
  imodContourDefault( contBreak2 );
  
  if( ( idxPt1 == idxPt2 )
      || ( !isBetweenAsc(0, idxPt1, psize(cont)-1) )
      || ( !isBetweenAsc(0, idxPt2, psize(cont)-1) ) ) {
    wprint( "ERROR: cont_breakContourEitherSide()" );
    return false;
  }
  
  if( !(idxPt1 < idxPt2) )          //|-- ensures that idxPt1 < idxPt2
    swapVals( idxPt1, idxPt2 );     //|
  
  if( shareEdge )
  {
    //## CREATE CONTOUR 1:
    for( int i=0; i<=idxPt1; i++ )
      imodPointAppend( contBreak1, getPt(cont,i) );
    for( int i=idxPt2; i<psize(cont); i++ )
      imodPointAppend( contBreak1, getPt(cont,i) );
      
    //## CREATE CONTOUR 2:
    for( int i=idxPt1; i<=idxPt2; i++ )
      imodPointAppend( contBreak2, getPt(cont,i) );
  }
  else
  {
    //## CREATE CONTOUR 1:
    for( int i=0; i<idxPt1; i++ )
      imodPointAppend( contBreak1, getPt(cont,i) );
    for( int i=idxPt2; i<psize(cont); i++ )
      imodPointAppend( contBreak1, getPt(cont,i) );
      
    //## CREATE CONTOUR 2:
    for( int i=idxPt1; i<idxPt2; i++ )
      imodPointAppend( contBreak2, getPt(cont,i) );
  }
  
  return true;
}




//------------------------
//-- Breaks a contour either side of the specified line.

bool cont_breakContourByLine( Icont *cont, Icont *contBreak1, Icont *contBreak2,
                              Ipoint *linePt1, Ipoint *linePt2,
                              Ipoint expectedPt,
                              bool useExpectedPtInsteadOfMaxAreaSmallerSide=false )
{
  imodContourDefault(contBreak1);
  imodContourDefault(contBreak2);
  
  Ipoint intercept;      // temp value for "line_doLinesCrossAndWhere" function
  
  vector<PtConnection> intercepts;  // stores a list of pts where the two contours
                                    // intersect, plus an index in cont where it crossed.
  
  for (int i=0; i<psize(cont);i++)                // for each point/line in cont:
    if(line_doLinesCrossAndWhere(getPt(cont,i),getPt(cont,i+1),
                                 linePt1,linePt2,&intercept ) )
    {                                             // if lines cross: add intercept point
      imodPointAdd( cont, &intercept, i+1 );
      intercepts.push_back( PtConnection(intercept, i+1) );
      i++;    // we want to skip the intersection point now.
    }
    
  int numIntercepts = (int)intercepts.size();
  
  if ( numIntercepts < 2 ) {      // if there are > 2 intercept points: return false
    return false;
  }
  else if( numIntercepts == 2 ) {    // if there are 2 intercept points:
                                     // break the contour between these points
    cont_breakContourEitherSide( cont,contBreak1,contBreak2,
                                 intercepts[0].cont1Idx,intercepts[1].cont1Idx,true );
    return true;
  }
  else                // else (if there are > 2 points):
  {                   // test sequential break pts to find which pair looks most likely
    
    intercepts = vector_sort( intercepts );
    
    Icont *contB1 = imodContourNew();
    Icont *contB2 = imodContourNew();
    
    float minDistToExpectedPt = FLOAT_MAX;
    float maxAreaOnSmallerSide = 0;
    
    for(int i=0; i<(int)intercepts.size()-1;i++)
    {
      Ipoint midWayPt = line_findPtFractBetweenPts2D( &intercepts[i].intercept, 
                                                      &intercepts[i+1].intercept, 0.5 );
      if ( !imodPointInsideCont( cont, &midWayPt ) ) {
        continue;
      }
      
      if( useExpectedPtInsteadOfMaxAreaSmallerSide )
      {
        float distToExpectedPt = imodPointDistance( &expectedPt, &midWayPt );
        
        if( distToExpectedPt < minDistToExpectedPt ) {
          minDistToExpectedPt = distToExpectedPt;
          cont_copyPoints( contBreak1, contB1, true );
          cont_copyPoints( contBreak2, contB2, true );
        }
      }
      else
      {
        cont_breakContourEitherSide(cont,contB1,contB2,
                                    intercepts[i].cont1Idx,intercepts[i+1].cont1Idx,true);
        float areaOnSmallerSide = MIN( ABS(imodContourArea(contB1)),
                                       ABS(imodContourArea(contB2))  );
        
        if( areaOnSmallerSide > maxAreaOnSmallerSide ) {
          maxAreaOnSmallerSide = areaOnSmallerSide;
          cont_copyPoints( contBreak1, contB1, true );
          cont_copyPoints( contBreak2, contB2, true );
        }
      }
    }
    
    imodContourDelete(contB1);
    imodContourDelete(contB2);
    
    return (psize(contBreak1) > 0 && psize(contBreak2) > 0 );
  }
  return true;
}


//------------------------
//-- Takes two contours and joins them together at the two points (one in each contour)
//-- which are closest together. This is used in a branching strategy.
//--  |      __     __     |      __     __          |
//--  |     /  \   /  \    |     /  \___/  \         |
//--  |     \__/   \__/    |     \__/ ^ \__/         | NOTE: will have point in 
//--  |                    |                         |       middle if
//--  | takes two contours | returns single contour  |       addPtInMiddle is true
//--  |                    | joined by a double line |

void cont_joinContsAtClosestApproach( Icont *newCont, Icont *cont1Orig,
                                      Icont *cont2Orig, bool addPtInMiddle=true )
{
  imodContourDefault( newCont );
  
  if( isEmpty(cont1Orig) || isEmpty(cont2Orig) )
    return;
    
  //## FIND CLOSEST POINTS IN BOTH CONTOURS:
  
  Icont *cont1 = imodContourDup(cont1Orig);
  Icont *cont2 = imodContourDup(cont2Orig);
  
  imodContourMakeDirection( cont1, IMOD_CONTOUR_CLOCKWISE );
  imodContourMakeDirection( cont2, IMOD_CONTOUR_CLOCKWISE );
  
  int closestPtInCont1, closestPtInCont2;
  cont_findClosestPts2D( cont1, cont2, &closestPtInCont1, &closestPtInCont2);
  
  Ipoint middlePt = line_findPtFractBetweenPts2D( getPt(cont1,closestPtInCont1),
                                                  getPt(cont2,closestPtInCont2), 0.5 );
  
  //## CONSTRUCT JOINED CONTOUR:
  
  cont_copyPoints( cont1, newCont, true );
  cont_reorderPtsToStartAtIdx( newCont, closestPtInCont1 );
  imodPointAppend( newCont, getPt(newCont,0) );
  
  if( addPtInMiddle )
    imodPointAppend( newCont, &middlePt );
  
  cont_reorderPtsToStartAtIdx( cont2, closestPtInCont2 );
  for(int i=0; i<psize(cont2); i++)
    imodPointAppend( newCont, getPt(cont2,i) );
  imodPointAppend( newCont, getPt(cont2,0) );
  
  if( addPtInMiddle )
    imodPointAppend( newCont, &middlePt );
  
  imodContourDelete(cont1);
  imodContourDelete(cont2);
  
}




//------------------------
//-- Takes a vector of contours and joines them all together in the place
//-- of closest approach to form a single big contour as shown in diagram:
//--  |                  __     |                   __     |
//--  |      __    __   /  \    |       __    __   /  \    |
//--  |     /  \  /  \  \__/    |      /  \__/  \__\__/    |
//--  |     \__/  \__/          |      \__/  \__/          |
//--  |                         |                          |
//--  | takes multiple contours | returns single contour   |
//--  |                         |  (joined with thin line) |

void cont_joinContsAtClosestApproachVector( Icont *newCont, vector<IcontPtr> conts,
                                            bool addPtInMiddle )
{
  imodContourDefault(newCont);
  
  if( (int)conts.size()==0 ) {
    wprint( "ERROR: cont_joinContsAtClosestApproach() - empty vector\n" );
    return;
  }
  else if( (int)conts.size()==1 ) {
    cont_copyPoints( conts[0].cont, newCont, true );
    return;
  }
  else if( (int)conts.size()==2 ) {
    cont_joinContsAtClosestApproach(newCont,conts[0].cont,conts[1].cont,addPtInMiddle);
    return;
  }
  else
  {
    while( (int)conts.size() > 1 )
    {
      float closestDist = FLOAT_MAX;
      int closestContIdx = 1;
      
      for( int i=1; i<(int)conts.size(); i++)  {
        float minDistThisCont = cont_minDistBetweenContPts2D(conts[0].cont,
                                                             conts[i].cont,false);
        if( minDistThisCont < closestDist ) {
          closestDist = minDistThisCont;
          closestContIdx = i;
        }
      }
      
      cont_joinContsAtClosestApproach(conts[0].cont,conts[0].cont,
                                      conts[closestContIdx].cont,addPtInMiddle);
      eraseContour( conts, closestContIdx );
    }
    cont_copyPoints( conts[0].cont, newCont, true );
    return;
  }
}




//------------------------
//-- Takes two contours, makes them convex and then returns a polygons
//-- representing the intersection (i.e. overlapping area) the two (convex)
//-- contours.
//-- NOTE: The intersection of two convex polygons is ALWAYS a (single)
//-- convex polygon or none at all.
//-- This diagram shows how it works:
//--
//-- |         INPUT         |  make both convex     | cont_getIntersectingConvexPolygon
//-- |         _________     |         _________     |
//-- |    ____/__       \    |    ____/__       \    |         __
//-- |   /  __|__|       |   |   /    |  |       |   |        |  |
//-- |   | /  |          | > |   |    |  |       | > |        |  |
//-- |   | |__|__        |   |   |    |  |       |   |        |  |
//-- |   \____|__|       |   |   \____|__|       |   |        |__|
//-- | cont1  \_________/    | cont1  \_________/    |
//-- |            cont2      |           cont2       |   returns single "intersection"
//--                                                      contour
//--                                (cont 1 has changed)

void cont_getIntersectingConvexPolygon( Icont *newCont,
                                        Icont *cont1Orig, Icont *cont2Orig )
{
  imodContourDefault(newCont);
  
  if( isEmpty(cont1Orig) || isEmpty(cont2Orig) )   // if either contours are empty:
    return;                                           // return empty set
  
  Icont *cont1 = imodContourDup(cont1Orig);
  Icont *cont2 = imodContourDup(cont2Orig);
  Icont *contInters = imodContourNew();    // stores final overlapping region
                                           // (the intersection of the two contours)
  
  
//## PREPARE CONTOURS AND DATA STRUCTURES:
  
  imodContourUnique( cont1 );
  imodContourUnique( cont2 );
  
  imodContourMakeDirection( cont1, IMOD_CONTOUR_CLOCKWISE );
  imodContourMakeDirection( cont2, IMOD_CONTOUR_CLOCKWISE );
  
  cont_makeConvex( cont1 );
  cont_makeConvex( cont2 );
  
  vector<PtConnection> intercepts; // stores a list of pts where the two contours
                                   // intersects, plus the index in both cont1P and
                                   // cont2P where this intersect point was added.
  Ipoint intercept;                // used in "line_doLinesCrossAndWhere"
  
      // setup "matrix of intersects" - is used to index a list of intercept pts
      // (in intercepts) which occur after EACH point in cont1 plus
      // the distance to that point  (and same for cont2)
        
  vector< vector<IdxAndFloat> > cont1Intercepts( psize(cont1) );    
  vector< vector<IdxAndFloat> > cont2Intercepts( psize(cont2) );
  
  const int NOT_INTERSECT_PT = -1;   // used to mark a pt which is not an intersection pt
  
      // use the z value show that none of these points are intersection points
      // (the are all original)
      // for intersection points the z value is set to an idx in the intercepts vector.
      
  changeZValue( cont1, NOT_INTERSECT_PT );          
  changeZValue( cont2, NOT_INTERSECT_PT );
  
//## FIND ALL INTERCEPTION POINTS WHERE CONTOURS CROSS AND 
//## ADD THEM TO THE LIST OF INTERCEPTS:
  
  for (int i=0; i<psize(cont1);i++)                // for each point/line in cont1:
    for (int j=0; j<psize(cont2);j++)                // for each point/line in cont2:
      if(line_doLinesCrossAndWhere( getPt(cont1,i), getPt(cont1,i+1),
                                    getPt(cont2,j), getPt(cont2,j+1), &intercept ) )
      {                      // if lines cross: add intercept point & calculate distances
        PtConnection newInt = PtConnection(intercept);
        if( vector_doesElementExistInVector( intercepts, newInt ) )
          continue;
        intercepts.push_back( newInt );
        
        cont1Intercepts[i].push_back(IdxAndFloat(int(intercepts.size())-1,
                           line_distBetweenPts2D( &intercept, getPt(cont1,i))));
        cont2Intercepts[j].push_back(IdxAndFloat(int(intercepts.size())-1,
                           line_distBetweenPts2D(&intercept,getPt(cont2,j))));
      }
      
//## IF CONTOURS NEVER CROSS: TEST IF ONE IS INSIDE THE OTHER, AND RETURN APPRIATE VALUE
  
  if( intercepts.size() == 0  )    // if contours don't cross paths at all:
  {
    if      (imodPointInsideCont(cont2, getPt(cont1,0)))  //(CASE 1) cont1 inside cont2
    {
      imodContourDelete(cont1);
      imodContourDelete(cont2);
      imodContourDelete(contInters);
      cont_copyPoints(cont1, newCont, true);
      return;
    }
    else if (imodPointInsideCont(cont1, getPt(cont2,0)))  //(CASE 2) cont2 inside cont1
    {
      imodContourDelete(cont1);
      imodContourDelete(cont2);
      imodContourDelete(contInters);
      cont_copyPoints(cont2, newCont, true);
      return;
    }
    else {               // (CASE 3) don't touch/overlap at all: return empty set
      imodContourDelete(cont1);
      imodContourDelete(cont2);
      imodContourDelete(contInters);
      cont_copyPoints(contInters, newCont, true);
      return;
    }
  }
  
//## FOR BOTH CONTOURS: IF MORE THAN ONE INTERCEPT POINTS OCCURS AFTER THE SINGLE POINT:
//## SORT THESE IN ORDER OF THEIR DISTANCE FROM THE POINT
        // (this ensure they are added to the contour in the correct order)
  
  for (int i=0; i<int(cont1Intercepts.size()); i++)    // for each point in cont1:
    if ( int(cont1Intercepts[i].size()) > 1 )       
      cont1Intercepts[i] = vector_sort( cont1Intercepts[i] );
      
  for (int i=0; i<int(cont2Intercepts.size()); i++)    // for each point in cont2:
    if ( int(cont2Intercepts[i].size()) > 1 )
      cont2Intercepts[i] = vector_sort( cont2Intercepts[i] );
      
//## FOR BOTH CONTOURS: CREATE A NEW VERSION WHEREBY THE INTERCEPTS POINT ARE ADDED,
//## AND AND MAP THE CONNECTION OF THESE POINTS BETWEEN CONTOURS
  
  Icont *cont1P = imodContourNew();   //|- a version of the contours where the 
  Icont *cont2P = imodContourNew();   //|  intercept pts have been ADDED as extra pts
  
  for (int i=0; i<int(cont1Intercepts.size()); i++) {  // for each point in cont1:
    imodPointAppend( cont1P, getPt(cont1,i) );            // add it to cont1P
    for (int j=0; j<int(cont1Intercepts[i].size()); j++ )
    {
      int interceptsIdx = cont1Intercepts[i][j].idx;
      Ipoint interceptPt = intercepts.at(interceptsIdx).intercept;
      interceptPt.z = interceptsIdx;
      imodPointAppend( cont1P, &interceptPt );             // add the intercept to cont1P
      intercepts.at(interceptsIdx).cont1Idx = psize(cont1P)-1; 
    }
  }
  
  for (int i=0; i<int(cont2Intercepts.size()); i++) {  // for each point in cont2:
    imodPointAppend( cont2P, getPt(cont2,i) );            // add it to cont2P
    for (int j=0; j<int(cont2Intercepts[i].size()); j++ )
    {
      int interceptsIdx = cont2Intercepts[i][j].idx;
      Ipoint interceptPt = intercepts.at(interceptsIdx).intercept;
      interceptPt.z = (int)interceptsIdx;
      imodPointAppend( cont2P, &interceptPt );             // add the intercept to cont2P
      intercepts.at(interceptsIdx).cont2Idx = psize(cont2P)-1;
    }
  }
  
//## TRAVERSE NEW CONTOURS AND CONNECTIONS BETWEEN INTERCEPT POINTS
//## TO GENERATE OVERLAPPING CONTOUR:
  
  Ipoint startPoint = intercepts[0].intercept;  // the starting point
  int currIdx = intercepts[0].cont1Idx;
  bool currentlyCont1 = true;         // indicates which contour we are traversing
                                      // (if true: then it's cont1, if false: cont2)
  
  while (true)
  {
    if( currentlyCont1 )      // if we are currently traversing cont1:
    {
                                // add this point (in cont1) to the overlapping polygon
      imodPointAppend( contInters, getPt(cont1P,currIdx));
      currIdx++;                        // go to next point in the cont1
      
      if( getPt( cont1P, currIdx)->z != -1 ) {        // if this is a intercept point:
        int nextIterceptIdx = (int)getPt(cont1P,currIdx)->z;
        currIdx = intercepts.at(nextIterceptIdx).cont2Idx;   
                // ensures a new contour is not started from
                // this intercept point (we've alread included it)
        currentlyCont1 = false;  // flip to cont2 (next iteration it will traverse cont2)
        
        // if we've come back around to our start point: overlapping polygon is complete
        if( intercepts.at(nextIterceptIdx).intercept == startPoint )  
          break;
      }
    }
    else                      // else (if we are currently traversing cont2):
    {
                                // add this point (in cont2) to the overlapping polygon
      imodPointAppend( contInters, getPt(cont2P,currIdx));
      currIdx++;                      // go to next point in cont2
      
      if( getPt( cont2P, currIdx)->z != -1 ) {        // if this is a intercept point:
        int nextIterceptIdx = (int)getPt(cont2P,currIdx)->z;
        currIdx = intercepts.at(nextIterceptIdx).cont1Idx;
                // ensures a new contour is not started from
                // this intercept point (we've alread included it)
        currentlyCont1 = true;                      // flip back to cont1
        // if we've come back around to our start point: overlapping polygon is complete
        if( intercepts.at(nextIterceptIdx).intercept == startPoint )
          break;
      }
    }
  }
  
  imodContourDelete(cont1);
  imodContourDelete(cont2);
  imodContourDelete(contInters);
  imodContourDelete(cont1P);
  imodContourDelete(cont2P);
  cont_copyPoints(contInters, newCont, true);
  return;
}








//------------------------
//-- Takes a single closed contour and breaks it into fragments either side 
//-- of any point which has a z value not equal to "zValue" with the point 
//-- inclusive. The broken contours are returned in "contSegs" after 
//-- setting all their points to zValue and deleting any fragments
//-- with only one point.

int cont_breakContByZValue( Icont *contOrig, vector<IcontPtr> &contSegs, int zValue )
{
  contSegs.clear();
  
  if( isEmpty(contOrig) )
    return 0;
  
  Icont *cont = imodContourDup(contOrig);
  
  imodContourUnique( cont );
  
  for(int i=0; i<psize(cont); i++)
    if( getPt(cont,i)->z != (float)zValue  )
    {
      cont_reorderPtsToStartAtIdx( cont, i );
      break;
    }
  
      contSegs.push_back( IcontPtr());
  for (int i=0; i<psize(cont)+1;i++)
  {
           // if this is an intersection point: add it, then start a new contour
    if( getPt(cont,i)->z != (float)zValue ) {    
      imodPointAppend( contSegs.back().cont, getPt(cont,i));
      contSegs.push_back( IcontPtr());
    }
    imodPointAppend( contSegs.back().cont, getPt(cont,i));
  }
  
  for (int i=0; i<(int)contSegs.size(); i++)
  {
    imodContourUnique( contSegs[i].cont );
    changeZValue( contSegs[i].cont,zValue );
    if(psize(contSegs[i].cont) <= 1 ) {
      eraseContour( contSegs, i );
      i--;
    }
  }
  
  imodContourDelete(cont);
  return (contSegs.size());
}


//------------------------
//-- Takes a single open contour and breaks it into fragments either
//-- side of any point which has a z value equal to "zValueToBreak"
//-- with the point not inclusive. The broken contours are returned
//-- in "contSegs" after deleting any fragments with only one point.

int cont_breakOpenContAtZValue( Icont *contOrig, vector<IcontPtr> &contSegs,
                                int zValueToBreak )
{
  contSegs.clear();
  
  if( isEmpty(contOrig) )
    return 0;
  
  Icont *cont = imodContourDup(contOrig);
  imodContourUnique( cont );
  
  contSegs.push_back( IcontPtr());
  for (int i=0; i<psize(cont);i++)
  {
            // if this is an intersection point: add it, then start a new contour
    if( getPt(cont,i)->z == (float)zValueToBreak ) {
      contSegs.push_back( IcontPtr());
    }
    else
      imodPointAppend( contSegs.back().cont, getPt(cont,i));
  }
  
  for (int i=0; i<(int)contSegs.size(); i++)
  {
    imodContourUnique( contSegs[i].cont );
    if(psize(contSegs[i].cont) <= 1 ) {
      eraseContour( contSegs, i );
      i--;
    }
  }
  
  imodContourDelete(cont);
  return (contSegs.size());
}


//------------------------
//-- Takes a single open contour and breaks it into fragments either side of
//-- a circle, by marking any points inside the circle and getting rid of them.

int cont_breakContByCircle( Icont *contOrig, vector<IcontPtr> &contSegs, Ipoint *center,
                            float radius )
{
  int radiusSq = (radius*radius);
  int numRemovedPoints = 0;
  
  int REMOVE_POINT_Z = -2;
  
  Icont *cont = imodContourDup(contOrig);
  
  for(int p=0; p<psize(cont); p++ )
  {
    if( getPt(cont,p)->z == center->z )
    {
      float distSq = line_sqDistBetweenPts2D( center, getPt(cont,p));
      if( distSq < radiusSq  )
      {
        getPt(cont, p)->z = REMOVE_POINT_Z;
        numRemovedPoints++;
      }
    }
  }
  
  cont_breakOpenContAtZValue( cont, contSegs, REMOVE_POINT_Z );
  
  imodContourDelete(cont);
  return (numRemovedPoints);
}







//------------------------
//-- Takes two contours and breaks both lines apart into pieces/segments
//-- in every place they intersect and returns the number of intercepts found.
//-- NOTE: If cont1 and cont2 are different slices no conts will be returned.

int cont_getIntersectingSegments( Icont *cont1Orig, Icont *cont2Orig,
                                             vector<IcontPtr> &cont1Seg,
                                             vector<IcontPtr> &cont2Seg  )
{
  cont1Seg.clear();
  cont2Seg.clear();
  
  if( psize(cont1Orig) <= 1 || psize(cont2Orig) <= 1 )  // if either contours are empty:
    return 0;                                               // return empty set
  
  Icont *cont1 = imodContourDup(cont1Orig);
  Icont *cont2 = imodContourDup(cont2Orig);
  
  imodContourUnique( cont1 );
  imodContourUnique( cont2 );
  
  int cont1ZVal = getZ(cont1);
  int cont2ZVal = getZ(cont2);
  
  if( cont1ZVal != cont2ZVal)
    wprint( "ERROR: cont_getIntersectingSegments() - different z vals\n" );
  
  vector<PtConnection> intercepts;   // stores a list of points where the two contours
                                     // intersect, plus the index in both cont1P and
                                     // cont2P where this intersect point was added.
  Ipoint intercept;     // fed into "line_doLinesCrossAndWhere" function.
  
      // create "matrix of intersects" - is used to index a list of intercept
      // points (in intercepts) which occur after EACH point in cont1 plus
      // the distance to that point (and same for cont2)
  
  vector< vector<IdxAndFloat> > cont1Intercepts( psize(cont1));
  vector< vector<IdxAndFloat> > cont2Intercepts( psize(cont2));
  
  const int INTERSECT_PT = -2;

//## FIND ALL INTERCEPTION POINTS WHERE CONTOURS CROSS AND
//## ADD THEM TO THE LIST OF INTERCEPTS:
  
  for (int i=0; i<psize(cont1);i++)                // for each point/line in cont1:
    for (int j=0; j<psize(cont2);j++)                // for each point/line in cont2:
      if( line_doLinesCrossAndWhere( getPt(cont1,i), getPt(cont1,i+1),
                                     getPt(cont2,j), getPt(cont2,j+1), &intercept ) )
                  // if lines cross: add intercept point & calculate distances
      {
        PtConnection newInt = PtConnection(intercept);
        if( vector_doesElementExistInVector( intercepts, newInt )
            || ptsEqual( getPt(cont1,i+1), &intercept)
            || ptsEqual( getPt(cont2,j+1), &intercept ) )
          continue;
        intercepts.push_back( newInt );
        
        cont1Intercepts[i].push_back(IdxAndFloat(int(intercepts.size())-1,
                                line_distBetweenPts2D( &intercept, getPt(cont1,i))));
        cont2Intercepts[j].push_back(IdxAndFloat(int(intercepts.size())-1,
                                line_distBetweenPts2D( &intercept, getPt(cont2,j))));
      }
  
//## IF CONTOURS NEVER CROSS: RETURN ZERO
  
  if( intercepts.size() == 0  )
  {
    imodContourDelete(cont1);
    imodContourDelete(cont2);
    return 0;
  }
  
  
  
//## FOR BOTH CONTOURS: IF MORE THAN ONE INTERCEPT POINT AFTER A SINGLE POINT:
//## SORT THESE IN ORDER OF THEIR DISTANCE FROM THE POINT
  
  for (int i=0; i<int(cont1Intercepts.size()); i++)    // for each point in cont1:
    if ( (int)cont1Intercepts[i].size() > 1 )
      cont1Intercepts[i] = vector_sort( cont1Intercepts[i] );
      
  for (int i=0; i<int(cont2Intercepts.size()); i++)    // for each point in cont2:
    if ( (int)cont2Intercepts[i].size() > 1 )
      cont2Intercepts[i] = vector_sort( cont2Intercepts[i] );
      
//## FOR BOTH CONTOURS: CREATE A NEW VERSION WHEREBY THE INTERCEPTS POINT ARE ADDED,
//## AND AND MAP THE CONNECTION OF THESE POINTS BETWEEN CONTOURS
  
  Icont *cont1P = imodContourNew();  //|- stores a version of the contours where the 
  Icont *cont2P = imodContourNew();  //|  intercept points have been ADDED as extra pts
  
  for (int i=0; i<int(cont1Intercepts.size()); i++) {  // for each point in cont1:
    imodPointAppend( cont1P, getPt(cont1,i));                // add it to cont1P
    for (int j=0; j<(int)cont1Intercepts[i].size(); j++ )
    {
      int interceptsIdx = cont1Intercepts[i][j].idx;
      Ipoint interceptPt = intercepts.at(interceptsIdx).intercept;
      interceptPt.z = (float)INTERSECT_PT;
      imodPointAppend( cont1P, &interceptPt );        // add the intercept to cont1P
      intercepts.at(interceptsIdx).cont1Idx = psize(cont1P)-1;
    }
  }
  
  for (int i=0; i<int(cont2Intercepts.size()); i++) {  // for each point in cont2:
    imodPointAppend( cont2P, getPt(cont2,i));                // add it to cont2P
    for (int j=0; j<(int)cont2Intercepts[i].size(); j++ )
    {
      int interceptsIdx = cont2Intercepts[i][j].idx;
      Ipoint interceptPt = intercepts.at(interceptsIdx).intercept;
      interceptPt.z = (float)INTERSECT_PT;
      imodPointAppend( cont2P, &interceptPt );         // add the intercept to cont2P
      intercepts.at(interceptsIdx).cont2Idx = psize(cont2P)-1;
    }
  }
  
//## CLEAN LINE SEGMENTS AND DELETE ANY EMPTY ONES:
  
  
  
  cont_breakContByZValue( cont1P, cont1Seg, cont1ZVal );
  cont_breakContByZValue( cont2P, cont2Seg, cont2ZVal );
  
  imodContourDelete(cont1);
  imodContourDelete(cont2);
  imodContourDelete(cont1P);
  imodContourDelete(cont2P);
  
  return ((int)cont1Seg.size());    // returns the number of intercept points
}





//------------------------
//-- Returns a vector of polygons representing the intersection
//-- (i.e. overlapping area) of two contours.
//-- WARNING: This function give correct result most of the time,
//-- but sometimes gives sligtly wrong polygons.
//--
//--   |           INPUT           |  cont_getIntersectingPolygons |
//--   |           _________       |                               |
//--   |      ____/__       \      |         __                    |
//--   |     /  __|__|       |     |        |__|                   |
//--   |     | /  |          |   > |                               |
//--   |     | |__|__        |     |         __                    |
//--   |     \____|__|       |     |        |__|                   |
//--   |   cont1  \_________/      |                               |
//--   |              cont2        |  returns 2 contour            |

int cont_getIntersectingPolygons(vector<IcontPtr> &finConts, Icont *cont1, Icont *cont2)
{
  finConts.clear();     // will store the final overlapping regions
                        // (the intersection of the two contours)
  
  if( isEmpty(cont1) || isEmpty(cont2) )   // if either contours is empty:
    return (finConts.size());                 // return empty set
  
//## BREAK CONTOURS INTO INTERSECTING SEGMENTS:
  
  vector<IcontPtr> cont1Seg;
  vector<IcontPtr> cont2Seg;
  int numIntersectPts =
    cont_getIntersectingSegments( cont1, cont2, cont1Seg, cont2Seg  );
  
//## IF CONTOURS NEVER CROSS: TEST IF ONE IS INSIDE THE OTHER,
//## AND RETURN APPROPRIATE VALUE
  
  if( numIntersectPts == 0  )   // if contours don't cross paths at all:
  {
    if      (imodPointInsideCont(cont2, getPt(cont1,0)))  //(CASE 1) cont1 inside cont2
      finConts.push_back( IcontPtr(cont1));
    else if (imodPointInsideCont(cont1, getPt(cont2,0)))  //(CASE 2) cont2 inside cont1
      finConts.push_back( IcontPtr(cont2));
    else                            //(CASE 3) don't touch/overlap at all
      finConts.empty();
    
    //deleteContours( cont1Seg );
    //deleteContours( cont2Seg );
    return (finConts.size());
  }
  
//## ERASE LINE SEGMENTS WHICH ARE NOT PART OF INTERSECTING AREA:
  
  if( numIntersectPts%2 ==1 ) {
    wprint( "WARNING: ODD NUMBER INTERSECTIONS !!!!\n" );
    
    for (int i=(int)cont1Seg.size()-1; i>=0; i--)    // for all intercept points
      if(imodPointInsideCont( cont2, getPt( cont1Seg[i].cont,1) ) )
        eraseContour(cont1Seg, i);
        
    for (int i=(int)cont2Seg.size()-1; i>=0; i--)    // for all intercept points
      if(imodPointInsideCont( cont1, getPt( cont2Seg[0].cont,1) ) )
        eraseContour(cont2Seg, i);
  }
  else    // delete every second segment (falling outside)
  {
    bool firstSegmentC1Inside = imodPointInsideCont(cont2, getPt(cont1Seg[0].cont,1));
    int offset = (firstSegmentC1Inside) ? 1 : 0 ;
    for(int i=(int)cont1Seg.size()-1; i>=0; i--)
      if( i>=offset && i%2 == offset )
        eraseContour( cont1Seg, i );
    
    bool firstSegmentC2Inside = imodPointInsideCont(cont1, getPt(cont2Seg[0].cont,1));
    offset = (firstSegmentC2Inside) ? 1 : 0 ;
    for(int i=(int)cont2Seg.size()-1; i>=0; i--)
      if( i>=offset && i%2 == offset )
        eraseContour( cont2Seg, i );
  }
  
  
//## JOIN CONT1 INTERSECTING SEGMENTS WITH TOUCHING INTERSECTING SEGMENTS IN CONT2
  
  for (int i=0; i<(int)cont1Seg.size(); i++)    // for all intercept points
  {
    for (int j=0; j<(int)cont2Seg.size(); j++)    // for all intercept points
    {
      if( ptsEqual(getLastPt(cont1Seg[i].cont),getLastPt(cont2Seg[j].cont))
          || ptsEqual(getLastPt(cont1Seg[i].cont),getFirstPt(cont2Seg[j].cont)))
      {
        Icont *newIntersectingCont = imodContourNew();
        cont_concat( newIntersectingCont, cont1Seg[i].cont, cont2Seg[j].cont, true );
        finConts.push_back( IcontPtr(newIntersectingCont));
        imodContourDelete(newIntersectingCont);
      }
    }
  }
  
//## JOIN ANY REMAINING/TOUCHING INTERSECTING SEGMENTS:
  
  Icont *tempCont = imodContourNew();
  for (unsigned i=0; i<finConts.size(); i++)
  {
    for (unsigned j=i+1; j<finConts.size(); j++)
    {
      if( ptsEqual( getLastPt( finConts[i].cont ), getLastPt( finConts[j].cont ) )
        || ptsEqual( getLastPt( finConts[i].cont ), getFirstPt( finConts[j].cont ) ) )
      {
        cont_copyPoints( finConts[i].cont, tempCont, true );
        cont_concat( finConts[i].cont, tempCont, finConts[j].cont, true );
        eraseContour( finConts, j );
        i--;
        break;
      }
    }
  }
  imodContourDelete( tempCont );
  
  deleteContours( cont1Seg );
  deleteContours( cont2Seg );
  return (finConts.size());
}



//------------------------
//-- Returns a vector of polygons representing the union (i.e. combined area)
//-- of two contours.
//-- NOTE: If the two contours intersect in > 2 places then multiple
//--       contours will be returned - one of them an "outer" contour
//--       and the other ones will actual be holes in the outer contour.
//--       (see "cont_getOuterUnionPolygon" for a diagram)
//--

int cont_getUnionPolygons( vector<IcontPtr> &finConts, Icont *cont1, Icont *cont2 )
{       
  finConts.clear();       // will store the final combined regions
                          // (the union of the two contours)
  
  
  if( isEmpty(cont1) || isEmpty(cont2) )   // if either contours is empty:
    return (finConts.size());                 // return empty set
  
  
//## BREAK CONTOURS INTO INTERSECTING SEGMENTS:
  
  vector<IcontPtr> cont1Seg;     // list of segments in cont1
  vector<IcontPtr> cont2Seg;     // list of segments in cont2
  int numIntersectPts = cont_getIntersectingSegments(cont1, cont2, cont1Seg, cont2Seg);
  
  wprint( "cont1Seg=%d cont1Seg=%d\n", cont1Seg.size(), cont2Seg.size());    //%%%%%%

    
//## IF CONTOURS NEVER CROSS: TEST IF ONE IS INSIDE THE OTHER,
//## AND RETURN APPROPRIATE VALUE
  
  if( numIntersectPts == 0  )    // if contours don't cross paths at all:
  {
    if      (imodPointInsideCont(cont2, getPt(cont1,0)))    //(CASE 1) cont1 inside cont2
      finConts.push_back( IcontPtr(cont1));
    else if (imodPointInsideCont(cont1, getPt(cont2,0)))    //(CASE 2) cont2 inside cont1
      finConts.push_back( IcontPtr(cont2));
    else                            // (CASE 3) don't touch/overlap at all
      finConts.empty();
    
    deleteContours(cont1Seg);
    deleteContours(cont2Seg);
    return (finConts.size());
  }
  
//## ERASE LINE SEGMENTS WHICH ARE NOT PART OF INTERSECTING AREA:
  
  if( numIntersectPts%2 == 1 )
  {
    wprint( "WARNING: ODD NUMBER INTERSECTIONS !!!! \n");
    for (int i=(int)cont1Seg.size()-1; i>=0; i--)    // for all intercept points
      if(imodPointInsideCont( cont2, getPt(cont1Seg[i].cont,1) ) )
        eraseContour( cont1Seg, i );
        
    for (int i=(int)cont2Seg.size()-1; i>=0; i--)    // for all intercept points
      if(imodPointInsideCont( cont1, getPt(cont2Seg[0].cont,1) ) )
        eraseContour( cont2Seg, i );
  }
  else    // delete every second segment (falling inside)
  {
    bool firstSegmentC1Inside = imodPointInsideCont(cont2, getPt(cont1Seg[0].cont,1));
    int offset = (firstSegmentC1Inside) ? 0 : 1 ;
    for(int i=(int)cont1Seg.size()-1; i>=0; i--)
      if( i>=offset && i%2 == offset )
        eraseContour( cont1Seg, i );
    
    bool firstSegmentC2Inside = imodPointInsideCont(cont1, getPt(cont2Seg[0].cont,1));
    offset = (firstSegmentC2Inside) ? 0 : 1 ;
    for(int i=(int)cont2Seg.size()-1; i>=0; i--)
      if( i>=offset && i%2 == offset )
        eraseContour( cont2Seg, i );
  }
  
  
  
//## JOIN CONT1 INTERSECTING SEGMENTS WITH TOUCHING INTERSECTING SEGMENTS IN CONT2
  
  Icont *newIntersectingCont = imodContourNew();
  for (int i=0; i<(int)cont1Seg.size(); i++)    // for all intercept points
  {
    for (int j=0; j<(int)cont2Seg.size(); j++)    // for all intercept points
    {
      if( ptsEqual(getLastPt(cont1Seg[i].cont), getLastPt(cont2Seg[j].cont))
        || ptsEqual(getLastPt(cont1Seg[i].cont), getFirstPt(cont2Seg[j].cont)))
      {
        cont_concat( newIntersectingCont, cont1Seg[i].cont, cont2Seg[j].cont, true );
        finConts.push_back( IcontPtr(newIntersectingCont));
      }
    }
  }
  imodContourDelete(newIntersectingCont);
  
//## JOIN ANY REMAINING/TOUCHING INTERSECTING SEGMENTS:
  
  Icont *tempCont = imodContourNew();
  for (int i=0; i<(int)finConts.size(); i++)
  {
    for (int j=i+1; j<(int)finConts.size(); j++)
    {
      if(  ptsEqual( getLastPt( finConts[i].cont ), getLastPt( finConts[j].cont ) )
        || ptsEqual( getLastPt( finConts[i].cont ), getFirstPt( finConts[j].cont ) ) )
      {
        cont_copyPoints( finConts[i].cont, tempCont, true );
        cont_concat( finConts[i].cont, tempCont, finConts[j].cont, true );
        eraseContour( finConts, j );
        i--;
        break;
      }
    }
  }
  imodContourDelete( tempCont );
  
  deleteContours( cont1Seg );
  deleteContours( cont2Seg );
  return (finConts.size());
}




//------------------------
//-- Returns true if c1 has a smaller area than c2

bool cont_smallerArea( IcontPtr c1, IcontPtr c2 ) {
  return ( ABS(imodContourArea( c1.cont )) < ABS(imodContourArea( c2.cont )));
}



//------------------------
//-- Returns a polygon representing the outer union (i.e. combined outer area)
//-- of two contours.
//-- NOTE: If the polygons don't overlap an empty polygon will be returned.
//-- NOTE: If the polygons intersect at > 2 places then the union area
//--       will consist of one OUTER polygon, and several smaller polygons
//--       inside this representing HOLES and this function will only
//--       return the largest (outer) polygon.
//--
//-- This diagram shows the difference between cont_getUnionPolygons
//-- AND cont_getOuterUnionPolygon
//--
//--  |          INPUT          |  cont_getUnionPolygons | cont_getOuterUnionPolygon |
//--  |          _________      |         _________      |         _________         |
//--  |     ____/__       \     |    ____/__       \     |    ____/         \        |
//--  |    /  __|__|       |    |   /    |__|       |    |   /               |       |
//--  |    | /  |          |  > |   |               |  > |   |               |       |
//--  |    | |__|__        |    |   |     __        |    |   |               |       |
//--  |    \____|__|       |    |   \____|__|       |    |   \____           |       |
//--  |  cont1  \_________/     |        \_________/     |        \_________/        |
//--  |             cont2       |  returns               |   returns only            |
//--                                 THREE CONTOURS             OUTER CONTOUR
//--                                 (including 2 holes)


void cont_getOuterUnionPolygon( Icont *newCont, Icont *cont1O, Icont *cont2O )
{
  vector<IcontPtr> joinedConts;
  cont_getUnionPolygons( joinedConts, cont1O, cont2O );
  
  if ( joinedConts.empty() ) {        // if contours don't touch: return empty contour
    imodContourDefault(newCont);
  }
  else if ( (int)joinedConts.size() == 1 ) {  // if is only one union polygon: return it
    wprint("copying\n");
    cont_copyPoints( joinedConts[0].cont, newCont, true );
  }
  else     // if there are > 1 union polygons: return the biggest one
  {
    float maxArea=0;
    int maxIdx=0;
    for(int i=0; i<(int)joinedConts.size(); i++) {
      float areaCont = imodContourArea(joinedConts[i].cont);
      if( areaCont > maxArea ) {
        maxArea = areaCont;
        maxIdx = i;
      }
    }
    IcontPtr contWithBiggestArea = joinedConts[maxIdx];
    cont_copyPoints( contWithBiggestArea.cont, newCont, true );
  }
  
  deleteContours( joinedConts );
  return;
}



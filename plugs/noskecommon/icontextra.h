#ifndef _ICONTEXTRA_H
#define _ICONTEXTRA_H

//############################################################

#include "_common_functions.h"
#include "imodplugin.h"
#include <vector>
using namespace std;


//############################################################

//-------------------------------
//## CONSTANTS

enum textalign  { TA_LEFT, TA_CENTER, TA_RIGHT };
enum textalignv { TV_TOP,  TV_CENTER, TV_BOTTOM, TV_LEADING };

const float DEFAULT_FONT_HEIGHT = 12.0f;
const float DEFAULT_FONT_SPACING = 10.0f;

//############################################################

//-------------------------------
//## SPECIAL DATA STRUCTURES USED IN FUCTIONS:



struct IcontPtr     // used as wrapper to store a pointer to a contour
{                   // so I can have vectors of "IcontPtr"
  
  Icont *cont;          // pointer to a contour
  float area;           // used in interoplator methods to stre and sort vector by area
  
  IcontPtr( Icont* cont_ );
  IcontPtr();
  //~IcontPtr();
  void deleteContour();
};


struct PtConnection // used especially for getIntersectingPolygon function
{  
  Ipoint intercept;    // stores intercept point
  int cont1Idx;        // stores index for cont1 where intercept point exists
  int cont2Idx;        // stores index for cont1 where intercept point exists
  bool included;       // wether pts included yet in generating intersecting polygon(s)
  
  PtConnection( Ipoint _intercept );
  PtConnection( Ipoint _intercept, int _cont1Idx );
};
bool operator<  (const PtConnection &a, const PtConnection &b);
bool operator== (const PtConnection lhs, const PtConnection rhs);


struct IdxAndFloat  // used especially for getIntersectingPolygon function
{
  int idx;            // stores an idx reference to the intercept point (in conn)
  float dist;         // stores distance from a pt on a contour to its interecpt pt
  
  IdxAndFloat(int _idx, float _dist);  
};
bool operator<(const IdxAndFloat &a, const IdxAndFloat &b);


struct IntAndInt    // used to connect point indexes between two contours
{
  int idx1;           // idx reference to a point in contour 1
  int idx2;           // idx reference to a point in contour 2
  
  IntAndInt(int _idx1, int _idx2);
};
bool operator<  (const IntAndInt &a, const IntAndInt &b);
bool operator== (const IntAndInt lhs, const IntAndInt rhs);

struct IdxToSort     // used especially for getIntersectingPolygon function
{  
  int idx;              // stores and idx reference to the intercept point (in conn)
  float float1;         // stores distance from a pt on a contour to its interecpt pt
  float float2;         // used as a tie breaker
  
  IdxToSort();
  IdxToSort( int _idx, float _float1 );
  IdxToSort( int _idx, float _float1, float _float2 );
};
bool operator<(const IdxToSort &a, const IdxToSort &b);

//############################################################


//-------------------------------
//## INLINE FUNCTION DECLARATIONS:                      (DEFINED AT THE END OF THIS FILE)

inline bool isContValid(Icont *cont);
inline bool isEmpty(Icont *cont);

inline bool isInterpolated(Icont *cont);
inline void setInterpolated(Icont *cont, int on);
inline bool isOpenFlag(Icont *cont);
inline void setOpenFlag(Icont *cont, int on);
inline bool isDeleteFlag(Icont *cont);
inline void setDeleteFlag(Icont *cont, int on);

inline int  psize(Icont *cont);
inline Ipoint *getPt(Icont *cont, int idx);
inline int getPtZInt(Icont *cont, int idx);
inline Ipoint *getPtNoWrap(Icont *cont, int idx);
inline Ipoint *getLastPt(Icont *cont );
inline Ipoint *getFirstPt(Icont *cont );
inline void setPt(Ipoint *pt, float x, float y, float z);
inline void copyPt(Ipoint *to, Ipoint *from);
inline Ipoint newPt( float x, float y, float z );
inline bool ptsEqual( Ipoint *pt1, Ipoint *pt2 );
inline bool ptsApproxEqual( Ipoint *pt1, Ipoint *pt2, float prec );
inline bool ptsEqualXYZ( Ipoint *pt1, Ipoint *pt2, float prec=0.001f );
inline void removePtsSize( Icont *cont );
inline void removePtSize( Icont *cont, int idx );
inline bool isDefaultSize( Iobj *obj, Icont *cont, int idx );

inline void printPt( Ipoint *pt );
inline void printCont( Icont *cont );
inline void printContPts( Icont *cont );
inline void deleteAllPts( Icont *cont );
inline float getZ( Icont *cont );
inline int getZInt( Icont *cont );
inline float getZRange( Icont *cont );
inline void setZValue( Icont *cont, int newZValue );
inline void cont_copyPts( Icont *from, Icont *to, bool clearToCont );
inline void deleteContours( vector<IcontPtr> &conts );
inline void eraseContour( vector<IcontPtr> &conts, int idx );

inline Icont* getCont( Iobj *obj, int idx );
inline Icont* getLastCont( Iobj *obj );
inline Icont* getFirstCont( Iobj *obj, bool createIfNotExists );
inline int csize( Iobj *obj );
inline int osize( Imod *imod );

inline Iobj* getObj( Imod *imod, int idx );
inline bool isObjClosed(Iobj *obj);
inline bool isContClosed(Iobj *obj, Icont *cont);
inline bool isObjectValidAndShown(Iobj *obj);
inline void setObjColor( Iobj *obj, int red, int green, int blue ); 

//-------------------------------
//## POINT RELATED FUNCTIONS:

void point_rotatePointAroundPoint2D( Ipoint *pt, Ipoint *center, float theta );
void point_scalePtAboutPt( Ipoint *pt, Ipoint *center, float scaleX, float scaleY, float scaleZ );
void point_scalePtAboutPt2D( Ipoint *pt, Ipoint *center, float scaleX, float scaleY );

float point_findAvgDistToPt( Icont *pts, Ipoint *startPt, Ipoint *scalePt );
float point_maxDiffInDistToPoint( Icont *pts, Ipoint *startPt, Ipoint *scalePt );
float point_stdDevDistToPoint( Icont *pts, Ipoint *startPt, Ipoint *scalePt );
Ipoint point_findCenterOfPts( Iobj *obj, Icont *pts, Ipoint *startPt, float &avgRadius,
                              float zScale, float &avgResidual, int maxIts );
Ipoint point_findCenterOfPtsX( Iobj *obj, Icont *pts, Ipoint *startPt, float avgRadius, float zScale, int maxIts );
Ipoint point_findOptimalZScaleAndCenterOfPts( Iobj *obj, Icont *pts, Ipoint *startPt, float &avgRadius, float zScale,
                                              float &bestZScale, float &bestAvgResidual, float startChangeZ,
                                              float accuracyZScale, int maxIts );

float getValCardinalSpline( float fract, float p0, float p1, float p2,  float p3, float tensileFract );
Ipoint getPtCardinalSpline( float fract, Ipoint p0, Ipoint p1, Ipoint p2,  Ipoint p3, float tensileFract ); 

//-------------------------------
//## MINIMUM BOUNDING RECTANGLE (MBR) RELATED FUNCTIONS:

void mbr_reset( Ipoint *ll, Ipoint *ur );
void mbr_addPt( Ipoint *pt, Ipoint *ll, Ipoint *ur );
float mbr_distToNearestEdge(float val, float min, float max);
float mbr_distToNearestEdge(float min1, float max1, float min2, float max2);
float mbr_distBetweenBBoxes2D(Ipoint *ll1, Ipoint *ur1, Ipoint *ll2, Ipoint *ur2);
float mbr_distBetweenBBoxes3D(Ipoint *ll1, Ipoint *ur1, Ipoint *ll2, Ipoint *ur2, float zScale); // NEW
bool mbr_distToBBox2D(Ipoint *pt, Ipoint *ll, Ipoint *ur);                    // NEW
bool mbr_isPtInsideBBox(Ipoint *pt, Ipoint *ll, Ipoint *ur);                  // NEW
bool mbr_isPtInsideBBox2D(Ipoint *pt, Ipoint *ll, Ipoint *ur);                // NEW
bool mbr_doEdgesOverlap(float min1, float max1, float min2, float max2);
bool mbr_doBBoxesOverlap2D(Ipoint *p1ll, Ipoint *p1ur, Ipoint *p2ll, Ipoint *p2ur);
bool mbr_isBBoxInsideBBox(Ipoint *ll1, Ipoint *ur1, Ipoint *ll2, Ipoint *ur2);

//-------------------------------
//## LINE RELATED FUNCTIONS:  

float line_getAngle2D ( Ipoint *linept1, Ipoint *linept2 );
float line_getAngle2DPos ( Ipoint *pt1, Ipoint *pt2 );

float line_getRadians2D ( Ipoint *linept1, Ipoint *linept2 );
float line_radiansFormed3Pts( Ipoint *pt1, Ipoint *pt2, Ipoint *pt3  );

Ipoint line_getPtHalfwayBetween(Ipoint *pt1, Ipoint *pt2);                    // NEW
Ipoint line_findPtFractBetweenPts2D( const Ipoint *pt1, const Ipoint *pt2, float fractBetweenPts );
Ipoint line_findPtFractBetweenPts( const Ipoint *pt1, const Ipoint *pt2, const float fractBetweenPts );

float line_sqDistBetweenPts2D( Ipoint *pt1, Ipoint *pt2 );
float line_distBetweenPts2D( Ipoint *pt1, Ipoint *pt2 );
bool line_isPointOnLine( Ipoint *pt, Ipoint *lineStart, Ipoint *lineEnd );

bool line_getLineEquation( Ipoint *pt1, Ipoint *pt2, float *gradient, float *offset );
float line_crossProduct3Points( Ipoint *pt1, Ipoint *pt2, Ipoint *pt3);
float line_angleFormed3Pts( Ipoint *pt1, Ipoint *pt2, Ipoint *pt3  );
Ipoint line_getPtRelativeToEnd( Ipoint *start, Ipoint *end, float distFromEnd, float angleFromStraight );                         //NEW
bool line_doLinesCrossAndWhere( Ipoint *line1pt1, Ipoint *line1pt2, Ipoint *line2pt1, Ipoint *line2pt2, Ipoint *intercept );
bool line_getInterceptWhereRayCross( Ipoint *line1pt1, Ipoint *line1pt2, Ipoint *line2pt1, Ipoint *line2pt2, Ipoint *intercept );
double line_getFractPtBetweenTwoRays( Ipoint *ray1pt1, Ipoint *ray1pt2, Ipoint *ray2pt1, Ipoint *ray2pt2, Ipoint *pt );
double line_getFractPtBetweenTwoLines( Ipoint *l1p1, Ipoint *l1p2, Ipoint *l2p1, Ipoint *l2p2, Ipoint *pt, bool checkOutsideNegative );
Ipoint point_findPtInQuad1InMatchingQuad2( Ipoint *pt, Ipoint *q1BL, Ipoint *q1TL, Ipoint *q1TR, Ipoint *q1BR, Ipoint *q2BL, Ipoint *q2TL, Ipoint *q2TR, Ipoint *q2BR );

bool line_isKiss( Ipoint *pMid, Ipoint *p1,Ipoint *p2,    Ipoint *p3, Ipoint *p4 );
bool line_twoKiss( Ipoint *a1, Ipoint *a2, Ipoint *a3,    Ipoint *b1, Ipoint *b2, Ipoint *b3 );


//-------------------------------
//## EXTRA CONTOUR FUNCTIONS:

int cont_generateDigitUsing16SegDisplay( Iobj *obj, char ch, int z );
int cont_generateDigitUsing7SegDisplay( Iobj *obj, int number, int z );
int cont_generateTextAsConts( Iobj *obj, string text, Ipoint pos, float fontSize, int textAlign=TA_LEFT, bool smallCaps=false );
int cont_generateTextAreaAsConts( Iobj *obj, string text, Ipoint pos, float fontSize, int alignHoriz=TA_LEFT, int alignVert=TV_LEADING, bool smallCaps=false, float lineSpacing=4 );
int cont_addTwoPointContourToObj( Iobj *obj, float p1x, float p1y, float p2x, float p2y, float z, int open=1 );
int cont_addTwoPointContourToObj( Iobj *obj, Ipoint p1, Ipoint p2, int open=1 );
int cont_addLineToObj( Iobj *obj, float x1, float y1, float z1, float x2, float y2, float z2, bool drawAllZ=false, bool interploated=false );		// NEW
int cont_addPtToObj( Iobj *obj, float x, float y, float z, bool drawAllZ=false );		// NEW

int  cont_isEqual( Icont *cont1, Icont *cont2 );
int  cont_doesPtExistInCont( Icont *cont, Ipoint *pt );

bool cont_getMBR( Icont *cont, Ipoint *ll, Ipoint *ur );
bool cont_getCenterOfMBR( Icont *cont, Ipoint *rpt );
bool cont_getCentroid( Icont *cont, Ipoint *rpt ); 
bool cont_insideCrude( Icont *cont1, Icont *cont2 );            // NEW

float cont_getRadius( Icont *c );
void  cont_findClosestPtInContToGivenPt( Ipoint *pt, Icont *cont, float *closestDist, Ipoint *closestPt, int *closestPtIdx );
void cont_findMinMaxAndAvgDistFromPt( Ipoint *pt, Icont *cont, float zScale, float &minDist,  float &maxDist,  float &avgDist );
bool  cont_doContsTouch( Icont *cont1, Icont *cont2 );
float cont_minDistPtAndContourPts2D( Ipoint *pt, Icont *cont, bool returnZeroIfPtInside );
float cont_minDistBetweenContPts2D( Icont *cont1, Icont *cont2, bool returnZeroIfTouch );
float cont_minDistBetweenContPts3D( Icont *cont1, Icont *cont2, float zScale, Ipoint *pt1, Ipoint *pt2 );
void  cont_reorderPtsToStartAtIdx( Icont *c, int idxNewFirstPt );
int   cont_removePointsInCircle( Icont *cont, Ipoint *center, float radius, bool checkZEachPoint );      //NEW

void cont_translate( Icont *cont, Ipoint *translate );
void cont_translate( Icont *cont, float x, float y );
void cont_rotateAroundPoint2D( Icont *cont, Ipoint *center, float angle );
void cont_scaleAboutPtXY( Icont *cont, Ipoint *center, float scaleX, float scaleY );
void cont_scaleAboutPt3D( Icont *cont, Ipoint *center, float scaleX, float scaleY, float scaleFactorZ );
void cont_scaleAboutPt( Icont *cont, Ipoint *pt, const float scaleFactor, bool ignoreZ );
void cont_stretchAlongAngle( Icont *cont, Ipoint *center, float angle, float stretchFactor );

void cont_generateCircle( Icont *cont, float radius, int numPoints, Ipoint center, bool addEndPt );
void cont_generateBox( Icont *cont, float llX, float llY, float width, float height, float z, bool repeatFirstPt=true );
void cont_generateBox( Icont *cont, Ipoint ll, Ipoint ur, bool repeatFirstPt=true );

int cont_numTimesCountsCross( Icont *cont1, Icont *cont2 );
bool cont_doCountoursCross( Icont *cont1, Icont *cont2, bool cont1Closed, bool cont2Closed );
bool cont_doCountoursCrossAndWhere( Icont *cont1, Icont *cont2, bool cont1Closed, bool cont2Closed, int *pt1BeforeCross, int *pt2BeforeCross );
bool cont_doesPtTouchContLine( Ipoint *pt, Icont *cont );

float cont_findClosestPts2D( Icont *cont1, Icont *cont2, int *closestPtIdxInCont1, int *closestPtIdxInCont2 );
void cont_reversePts( Icont *c );

void cont_addChamferPts( Icont *cont, Ipoint *ptPrev, Ipoint *ptCurr, Ipoint *ptNext, float distOffset, float minAngle );         //NEW
void cont_expandOpenCont( Icont *contOrig, Icont *contReturn, float thickness, float minAngleForChamfers, bool roundEnds );       //NEW
void cont_expandClosedCont( Icont *contOrig, Icont *innerCont, Icont *outerCont, float thickness, float minAngleForChamfers );    //NEW

void cont_concat( Icont *contNew, Icont *cont1, Icont *cont2, bool matchClosestEnds );    
int cont_addPtsCrude( Icont *cont, float maxDist, bool closed );           // MODIFIED
int cont_addPtsSmoothIteration( Icont *cont, float maxDist, float tensileFract, bool closed );
int cont_addPtsSmooth( Icont *cont, float maxDist, float tensileFract, bool closed, 
                       bool roundZOpenPts=true, bool addPtEveryZ=true );     
int cont_avgPtsPos( Icont *cont, float moveFract, float minDistToMove, bool closed, bool rescale ); // NEW
int cont_reducePtsCrude( Icont *cont, float minDist, bool closed );                    
int cont_reducePtsTol(Icont *cont, float tol);                              // NEW
int cont_reducePtsMinArea( Icont *cont, float minArea, bool closed );       // MODIFIED
int cont_removeRedundantPts( Icont *cont, bool removeStraightLinePts, bool closed, bool removePts );    // NEW

bool cont_isSimple( Icont *cont, bool closed=true );
bool cont_isSimpleSeg( Icont *cont, bool closed, int *ptCross );
void cont_makeSimple( Icont *cont );                            
int  cont_breakIntoSimple( vector<IcontPtr> &conts, Icont *cont );   
int  cont_killVertAndHorzSegments( Icont *cont );                           // NEW
void cont_nudgeAllPointsRandomly( Icont *cont );                            // NEW
int  cont_nudgeAnyPointsLyingOnOtherContour( Icont *cont1, Icont *cont2 );  // NEW
bool cont_isConvex( Icont *cont );   
int  cont_makeConvexCrude( Icont *cont );
int  cont_makeConvex( Icont *cont );
int  cont_markConvexPtsNegOne( Icont *cont );
void cont_calcConvexProperties( Icont *cont, bool closed, int *numConvexPts, float *convexLen, float *hullLen, float *hullArea );
bool cont_breakContourEitherSide( Icont *cont, Icont *contBreak1, Icont *contBreak2, int idxPt1, int idxPt2, bool shareEdge );      
bool cont_breakContourByLine( Icont *cont, Icont *contBreak1, Icont *contBreak2, Ipoint *linePt1, Ipoint *linePt2, Ipoint expectedPt, bool useExpectedPtInsteadOfMaxAreaSmallerSide );
int  cont_breakContourByContour( vector<IcontPtr> &contSegs, Icont *contO, Icont *contBreakO, float minDist );    // NEW
void cont_joinContsAtClosestApproach( Icont *newCont, Icont *cont1, Icont *cont2, bool addPtInMiddle );          
void cont_joinContsAtClosestApproachVector( Icont *newCont, vector<IcontPtr> conts, bool addPtInMiddle );          
void cont_getIntersectingConvexPolygon( Icont *newCont, Icont *cont1, Icont *cont2 );                    

int cont_breakContByZValue( Icont *contOrig, vector<IcontPtr> &contSegs, int zValue, bool removeOffSegments );  // NEW
int cont_breakOpenContAtZValue( Icont *contOrig, vector<IcontPtr> &contSegs, int zValueToBreak );               // NEW
int cont_breakContByCircle( Icont *contOrig, vector<IcontPtr> &contSegs, Ipoint *center, float radius );        // NEW
int cont_addPtsAtIntersection( Icont *cont1, Icont *cont2 );                                                    // NEW
int cont_addPtsAtIntersections( Icont *cont1, Icont *cont2, bool cont1Closed, bool cont2Closed, Icont *intercepts, bool clearIntercepts );					// VERY NEW
int cont_getIntersectingSegments( Icont *cont1, Icont *cont2, vector<IcontPtr> &cont1Segs, vector<IcontPtr> &cont2Segs  );   // MODIFY
int cont_getIntersectingSegmentsSafe( Icont *cont1, Icont *cont2, vector<IcontPtr> &cont1Segs, vector<IcontPtr> &cont2Segs  );   // MODIFY
int cont_getIntersectingPolygons( vector<IcontPtr> &finalConts, Icont *cont1, Icont *cont2 );                      
int cont_getUnionPolygons( vector<IcontPtr> &finalConts, Icont *cont1, Icont *cont2 );                        
bool cont_getOuterUnionPolygon( Icont *newCont, Icont *cont1O, Icont *cont2O );                    

Ipoint cont_getPtDistAlongLength( Icont *cont, float dist, bool closed, int startPt );
Ipoint cont_getPtFractAlongLength( Icont *cont, float fract, bool closed, int startPt );
vector<float> cont_getFractPtsAlongLength( Icont *cont, bool closed, int startPt );                                                    // NEW
int cont_addPtsFractsAlongLength( Icont *cont, Icont *contNew, vector<float> fractsAlongLen, bool closed, bool keepExistingPts, int startPt );      // NEW
//void cont_calcPtsizeInfo( Iobj *obj, Icont *cont, const float zScale, float &openLength, float &fullLength, float &openVol, float &fullVol, float &avgRadius, float &minRadius, float &maxRadius, float &minMidRadius, float &surfaceArea );
void cont_calcPtsizeInfo( Iobj *obj, Icont *cont, const float zScale, float pixelSize,
                     float &openLength, float &fullLength,
                     float &avgR, float &firstR, float &lastR,
                     float &minR, float &maxR, float &minMidR,
                     float &openVol, float &fullVol, float &surfaceArea );


//############################################################

//-------------------------------
//## INLINE FUNCTION DEFINITIONS:



//------------------------
//-- Returns true if the contour exists and has points.

inline bool isContValid(Icont *cont)
{
  return ( cont != NULL && imodContourGetMaxPoint(cont) > 0 );
}

//------------------------
//-- Returns true if the contour's interpolated flag is set.

inline bool isInterpolated(Icont *cont)
{
  return ( imodContourGetFlag( cont, ICONT_STIPPLED ) != 0 );
}

//------------------------
//-- Sets the contour's interpolated flag to on or off.

inline void setInterpolated(Icont *cont, int on)
{
  imodContourSetFlag( cont, ICONT_STIPPLED, on );
}

//------------------------
//-- Returns true if the contour's delete flag is set.

inline bool isOpenFlag(Icont *cont)
{
  return ( imodContourGetFlag( cont, ICONT_OPEN ) != 0 );
}

//------------------------
//-- Sets the contour's delete flag ("ICONT_TEMPUSE") to on or off.

inline void setOpenFlag(Icont *cont, int on )  
{
  imodContourSetFlag( cont, ICONT_OPEN, on );
}

//------------------------
//-- Returns true if the contour's delete flag is set.

inline bool isDeleteFlag(Icont *cont)
{
  return ( imodContourGetFlag( cont, (unsigned int)ICONT_TEMPUSE ) != 0 );
}

//------------------------
//-- Sets the contour's delete flag ("ICONT_TEMPUSE") to on or off.

inline void setDeleteFlag(Icont *cont, int on )  
{
  imodContourSetFlag( cont, ICONT_TEMPUSE, on );
}

//------------------------
//-- Returns true is the contour has no points or is marked for deletion.

inline bool isEmpty(Icont *cont)
{
  return ( imodContourGetMaxPoint( cont ) == 0 || isDeleteFlag( cont ) );
}

//------------------------
//-- Shorter function name for "imodContourGetMaxPoint()"

inline int  psize(Icont *cont)
{
  return imodContourGetMaxPoint( cont );
}

//------------------------
//-- Returns a pointer to the point at the given index in the contour...
//-- wrapping around to the first point if idx is >= imodContourGetMaxPoint().

inline Ipoint *getPt(Icont *cont, int idx)
{
  int contPts = imodContourGetMaxPoint( cont );
  idx = (idx+contPts) % contPts;
  return imodContourGetPoint(cont, idx);
  
  //Ipoint *pts = imodContourGetPoints( cont );
  //return &pts[ (idx+contPts) % contPts ];
}

//------------------------
//-- Returns the Z value of the point at the given index in the contour
//-- rounded to the nearest integer

inline int getPtZInt(Icont *cont, int idx)
{
  return int( getPt(cont, idx)->z + 0.5 );
}

//------------------------
//-- Returns a pointer to the point at the given index in the contour...
//-- but does not wrap around.

inline Ipoint *getPtNoWrap(Icont *cont, int idx)
{
  int contPts = imodContourGetMaxPoint( cont );
  keepWithinRange( idx, 0, contPts-1 );
  return imodContourGetPoint(cont, idx);
}

//------------------------
//-- Returns a pointer to the last point in the contour.

inline Ipoint *getLastPt(Icont *cont )
{
  if(!cont)
    return (NULL);
  int contPts = imodContourGetMaxPoint( cont );
  Ipoint *pts = imodContourGetPoints( cont );
  return &pts[ contPts-1 ];
}

//------------------------
//-- Returns a pointer to the first point in the contour.

inline Ipoint *getFirstPt(Icont *cont )
{
  if(!cont)
    return (NULL);
  Ipoint *pts = imodContourGetPoints( cont );
  return &pts[ 0 ];
}

//------------------------
//-- Used to set the three coordinates of a point in one call.

inline void setPt(Ipoint *pt, float x, float y, float z)
{
  pt->x = x;
  pt->y = y;
  pt->z = z;
}

//------------------------
//-- Used to set the three coordinates of a point in one call.

inline void copyPt(Ipoint *to, Ipoint *from)
{
  to->x = from->x;
  to->y = from->y;
  to->z = from->z;
}

//------------------------
//-- Used to create a new point in one call.

inline Ipoint newPt( float x, float y, float z )
{
  Ipoint pt;
  pt.x = x;
  pt.y = y;
  pt.z = z;
  return pt;
}


//------------------------
//-- Returns true if two points are equal... but unlike imodPointIsEqual
//-- only checks x and y values

inline bool ptsEqual( Ipoint *pt1, Ipoint *pt2 )
{
  return (pt1->x == pt2->x) && (pt1->y == pt2->y); 
}

//------------------------
//-- Returns true if two points are with plus or minus "prec"
//-- of each other in X and Y

inline bool ptsApproxEqual( Ipoint *pt1, Ipoint *pt2, float prec )
{
  return   (pt1->x >= pt2->x-prec && pt1->x <= pt2->x+prec )
        && (pt1->y >= pt2->y-prec && pt1->y <= pt2->y+prec ); 
}


//------------------------
//-- Returns true if two points are with plus or minus "prec"
//-- of each other in X, Y and Z

inline bool ptsEqualXYZ( Ipoint *pt1, Ipoint *pt2, float prec )
{
  return   ( fabs(pt1->x - pt2->x ) <= prec )
	      && ( fabs(pt1->y - pt2->y ) <= prec )
	      && ( fabs(pt1->z - pt2->z ) <= prec );
}


//------------------------
//-- Resets the point size and fine grain of all points in the current contour
//-- to the default.

inline void removeExtraInfo( Icont *cont )
{
  int openFlag = imodContourGetFlag( cont, ICONT_OPEN );
  int stippledFlag = imodContourGetFlag( cont, ICONT_STIPPLED );
  Icont *tempCont = imodContourDup( cont );
  imodContourDefault( cont );
  imodContourCopy( tempCont, cont );
  cont_copyPts( tempCont, cont, true );
  
  imodContourSetFlag( cont, ICONT_OPEN, openFlag );
  imodContourSetFlag( cont, ICONT_STIPPLED, stippledFlag );
  imodContourDelete( tempCont );
}


//------------------------
//-- Resets the point size and fine grain of all points in the current contour
//-- to the default.

inline void removePtsSize( Icont *cont )
{
  for (int p=0; p<psize(cont); p++)
    removePtSize( cont, p );
}

//------------------------
//-- Resets the point size and fine grain info of the given point in the current
//-- contour to the default.

inline void removePtSize( Icont *cont, int idx )
{
  Ipoint *pt = getPt(cont, idx);
  Ipoint newPt;
  setPt( &newPt, pt->x, pt->y, pt->z );
  imodPointAdd( cont, &newPt, idx );
  imodPointDelete( cont, idx+1 );
}


//------------------------
//-- Returns true if the size of the point equals the default sphere size for the object
//-- or returns false if different.

inline bool isDefaultSize( Iobj *obj, Icont *cont, int idx )
{
  float ptSize        = imodPointGetSize(obj, cont, idx);
  float objSphereSize = imodObjectGetValue(obj,IobjPointSize);
  return (ptSize == objSphereSize);
}


//------------------------
//-- Prints some simple information about point - used in debugging.

inline void printPt( Ipoint *pt )
{
  wprint("pt = %f,%f,%f\n", pt->x, pt->y, pt->z ); 
}


//------------------------
//-- Prints some simple information about the contour - used in debugging.

inline void printCont( Icont *cont )
{
  wprint("cont pts=%d z=%d \n", psize(cont), imodContourZValue(cont) ); 
}


//------------------------
//-- Prints all points in the contour.

inline void printContPts( Icont *cont )
{
  wprint("cont: " );
  for(int p=0; p<psize(cont); p++)
    wprint(" { %d,%d,%d }",
           (int)getPt(cont,p)->x, (int)getPt(cont,p)->y, (int)getPt(cont,p)->z );
  wprint("\n");
}


//------------------------
//-- Useful for deleting all points without calling "imodContourDefault",
//-- which will reset other contour properties.

inline void deleteAllPts( Icont *cont )
{
  int nPoints = psize( cont );
  for (int i=nPoints-1; i>=0; i--)
    imodPointDelete( cont, i );
}

//------------------------
//-- Shorter function name for "imodContourZValue()"

inline float getZ( Icont *cont )
{
  return (float)imodContourZValue( cont );
}

//------------------------
//-- Returns Z value of contour rounded to nearest integer

inline int getZInt( Icont *cont )
{
  return imodContourZValue( cont );
}

//------------------------
//-- Returns the span of the contour in Z (will be 0 if contour on one slice)

inline float getZRange( Icont *cont )
{
  if( isEmpty(cont) )
    return 0;
  
  float maxZ = INT_MIN;
  float minZ = INT_MAX;
  
  for(int p=0; p<psize(cont); p++)
  {
    float z = getPt(cont,p)->z;
    updateMin( minZ, z );
    updateMax( maxZ, z );
  }
  return (maxZ - minZ);
}


//------------------------
//-- Changes the z value of all the points to the given value.

inline void setZValue( Icont *cont, int newZValue )
{
  for (int i=0; i<psize( cont ); i++)
    getPt( cont, i )->z = (float)newZValue;
  imodContourSetFlag( cont, ICONT_WILD, 0 );
  //imodel_contour_check_wild(cont)
}

//------------------------
//-- Copies all the points from the first contour to the second.

inline void cont_copyPts( Icont *from, Icont *to, bool clearToCont )
{
  if(clearToCont)
    imodContourDefault(to);
  for (int i=0; i<psize(from); i++)
    imodPointAppend( to, getPt(from,i) );
}

//------------------------
//-- Deletes all contours in this array by calling
//-- imodContourDelete for any non-null contour pointers

inline void deleteContours( vector<IcontPtr> &conts )
{
  for (int iCont=0; iCont<(int)conts.size(); iCont++)
    conts[iCont].deleteContour();
}

//------------------------
//-- Erases the specified contours in this array by calling
//-- imodContourDelete and then removing it from the array

inline void eraseContour( vector<IcontPtr> &conts, int idx )
{
  conts[idx].deleteContour();
  conts.erase( conts.begin() + idx );
}



//------------------------
//-- Shorter function name for "imodObjectGetContour()"

inline Icont* getCont( Iobj *obj, int idx )
{
  return imodObjectGetContour(obj, idx);
}

//------------------------
//-- Returns the last contour in the object

inline Icont* getLastCont( Iobj *obj )
{
  if(!obj)
    return (NULL);
  return imodObjectGetContour(obj, csize(obj)-1 );
}


//------------------------
//-- Returns the first contour and creates it if doesn't already
//-- exist (so long as 'createIfNotExists' is true)

inline Icont* getFirstCont( Iobj *obj, bool createIfNotExists )
{
	if( createIfNotExists && imodObjectGetMaxContour(obj)==0 )
	{
		Icont *newCont = imodContourNew();    // malloc new contour and don't delele it
		imodObjectAddContour( obj, newCont );
		free(newCont); 
	}
  return imodObjectGetContour(obj, 0);
}


//------------------------
//-- Shorter function name for "imodObjectGetMaxContour()"

inline int csize( Iobj *obj )
{
  return imodObjectGetMaxContour(obj);
}

//------------------------
//-- Shorter function name for "imodGetMaxObject()"

inline int osize( Imod *imod )
{
  return imodGetMaxObject(imod);
}


//---------------------------------
//-- Returns the object at the specified index (or NULL if there is none).

inline Iobj* getObj( Imod *imod, int idx )
{
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  imodSetIndex( imod, idx, 0, 0);
  Iobj *obj  = imodObjectGet(imod);
  imodSetIndex( imod, objIdx, contIdx, ptIdx );
  
  return obj;
}


//---------------------------------
//-- Returns true if the object is closed.

inline bool isObjClosed(Iobj *obj)
{
  return (imodObjectGetValue(obj, IobjFlagClosed) == 1);
}

//---------------------------------
//-- Returns true if the object is valid and has it's draw flag on.

inline bool isContClosed(Iobj *obj, Icont *cont)
{
  bool objClosed = isObjClosed( obj );
  bool contClosed = !isOpenFlag( cont );
  return ( objClosed && contClosed );
}


//---------------------------------
//-- Returns true if the object is valid and has it's draw flag on.

inline bool isObjectValidAndShown(Iobj *obj)
{
        // imodObjectGetValue returns 1 when object is hidden
  int objHidden = (imodObjectGetValue(obj, IobjFlagDraw) );
  return ( obj != NULL && !objHidden );    
}

//---------------------------------
//-- Wrapper function for "imodObjectSetColor" but using integers between 0 & 255
//-- intead of a float beteen 0 & 1.
//-- Sets the objects color using red, green, blue integers with values between
//-- 0 and 255. I created this function because I had problems with
//-- "imodObjectSetColor( obj, red/255, green/255, blue/255 );" sometimes
//-- typecasting some colors incorrectly.

inline void setObjColor( Iobj *obj, int red, int green, int blue )
{
  float redF = (float)red / 255.0f;
  float greenF = (float)green / 255.0f;
  float blueF = (float)blue / 255.0f;
  
  imodObjectSetColor( obj, redF,greenF,blueF );
}




//############################################################

#endif


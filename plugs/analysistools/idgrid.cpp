
//############################################################

#include "idgrid.h"
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

DeformGridSlice::DeformGridSlice() {
  reset();
  contAroundEdge = imodContourNew();
}

//------------------------
//-- Default constructor

DeformGridSlice::DeformGridSlice( int colsX_, int rowsY_, float minX_, float minY_, float maxX_, float maxY_, int sliceNum_ ) {
  setupGrid( colsX_, rowsY_, minX_, minY_, maxX_, maxY_, sliceNum_ );
  contAroundEdge = imodContourNew();
}

//------------------------
//-- Resest all values to zero and clears the vector of points.

void DeformGridSlice::reset() {
  xPts = 0; yPts = 0; colsX = 0; rowsY = 0;
  sliceNum = 0;
  sectionBoundary = false;
  sectionQuality = 0.0;
  vect.clear();
  transform.reset();
  imodContourDefault( contAroundEdge );
  mbr_reset( &mbrLL, &mbrUR );
}

//------------------------
//-- Sets up the grid by calculating width and height values, and setting up the
//-- vector of start and end points in a uniformly spaced grid.

void DeformGridSlice::setupGrid( int colsX_, int rowsY_, float minX_, float minY_, float maxX_, float maxY_, int sliceNum_ )
{
  colsX = colsX_;
  rowsY = rowsY_;
  
  minX = minX_;
  minY = minY_;
  maxX = maxX_;
  maxY = maxY_;
  
  sliceNum = sliceNum_;
  
  xPts = colsX + 1;
  yPts = rowsY + 1;
  
  cellWidth  = (maxX-minX)  / (float)colsX;
  cellHeight = (maxY-minY)  / (float)rowsY;
  
  //cout << "cellWidth = " << cellWidth << endl;    //%%%%%%%
  
  vect.resize( xPts*yPts );
  
  for (int y=0; y<yPts; y++)
  	for (int x=0; x<xPts; x++)
      getVect(x,y).setStartAndEndPtsSame( x*cellWidth + minX, y*cellHeight + minX, sliceNum );
}

//------------------------
//-- Returns the vector at the given grid point

Ivector& DeformGridSlice::getVect( int x, int y )
{
  //assert( x < xPts && y < yPts );
  return vect[ y*yPts + x ];
}

//------------------------
//-- Returns the vector at the given grid point (and avoids out-of-bounds error for a row or colum which doesn't exist)

Ivector& DeformGridSlice::getVectSafe( int x, int y )
{
  if( x >= xPts )  x = xPts-1;
  if( y >= yPts )  y = yPts-1;
  return vect[ y*yPts + x ];
}

//------------------------
//-- Returns the grid X column index for the cell at the given point

int DeformGridSlice::idxX( Ipoint pt )
{
  return int( (pt.x - minX) / cellWidth );
}

//------------------------
//-- Returns the grid Y row index for the cell at the given point

int DeformGridSlice::idxY( Ipoint pt )
{
  return int( (pt.y - minY) / cellHeight );
}


//------------------------
//-- Takes a point and adjusts it using the calculated best-fit transform.
//-- NOTE: This gives a guess of where the transformed point might lie.

void DeformGridSlice::adjustPtByTransform( Ipoint *pt )
{
  Ipoint centerPt;
  setPt( &centerPt, (maxX-minX)/2.0, (maxY-minY)/2.0, sliceNum );
  transform.adjustPtByTransform( pt, &centerPt );
}

//------------------------
//-- Takes a point and adjusts it using the calculated best-fit transform.
//-- NOTE: This gives a guess of where the transformed point might lie.

Ipoint DeformGridSlice::adjustPtByTransformGet( Ipoint *pt )
{
  Ipoint adjustedPt;
  setPt( &adjustedPt, pt->x, pt->y, pt->z );
  adjustPtByTransform( &adjustedPt );
  return (adjustedPt);
}

//------------------------
//-- Takes a point and adjusts it using the INVERSE of the calculated best-fit transform.
//-- NOTE: This gives a guess of where the original point might lie.

void DeformGridSlice::adjustPtByTransformInverse( Ipoint *pt )
{
  Ipoint centerPt;
  setPt( &centerPt, (maxX-minX)/2.0, (maxY-minY)/2.0, sliceNum );
  transform.adjustPtByTransformInverse( pt, &centerPt );
}

//------------------------
//-- Takes a point within the original grid, and changes it to the cooresponding deformed point using the deformation grid.
//-- If the point is outside the grid it aborts and returns false (instead of true).

bool DeformGridSlice::adjustPtByGrid( Ipoint *pt )
{
  float ptZ = pt->z;
  
  if( !isBetweenAsc(minX, pt->x, maxX) || !isBetweenAsc(minY, pt->y, maxY) ) {
  	//cerr << "ERROR: adjustPtByGrid - point is outside limits" << endl;  //%%%%%%%%%%
  	return false;
  }
  
  float fX = (pt->x - minX) / cellWidth;
  float fY = (pt->y - minY) / cellHeight;
  int x = int(fX);
  int y = int(fY);
  float fractUp    = fY - (float)y;
  float fractRight = fX - (float)x;
  
  Ipoint ptBL = getVectSafe( x,   y   ).ptE;
  Ipoint ptBR = getVectSafe( x+1, y   ).ptE;
  Ipoint ptTR = getVectSafe( x+1, y+1 ).ptE;
  Ipoint ptTL = getVectSafe( x,   y+1 ).ptE;
  
  Ipoint partwayAlongBottom = line_findPtFractBetweenPts2D( &ptBL, &ptBR, fractRight );
  Ipoint partwayAlongTop    = line_findPtFractBetweenPts2D( &ptTL, &ptTR, fractRight );
  *pt = line_findPtFractBetweenPts2D( &partwayAlongBottom, &partwayAlongTop, fractUp );
  pt->z = ptZ;
  
  return true;
}

//------------------------
//-- Takes a point within the deformed grid, and makes a best estimate of the cooresponding point in the origonal grid.
//-- If the point is outside the deformation grid it aborts and returns false (instead of true).

bool DeformGridSlice::adjustPtByGridInverse( Ipoint *pt )
{
  if( mbr_isPtInsideBBox2D( pt, &mbrLL, &mbrUR ) )    // early elimination
  	return false;
  else if( !imodPointInsideCont( contAroundEdge, pt ) )
  	return false;
  
  //## GUESS WHICH DEFORMED CELL OUR POINT SHOULD BE IN:
  
  Ipoint estimatedPos;
  setPt( &estimatedPos, pt->x, pt->y, pt->z );
  adjustPtByTransformInverse( &estimatedPos );
  
  int gridX = idxX(estimatedPos);
  int gridY = idxY(estimatedPos);
  
  //## CHECK IF POINT IS IN PREDICTED CELL:
  
  Ipoint origPt;
  if ( isPointInDeformedCell( pt, gridX, gridY, &origPt ) ) {
  	setPt( pt, origPt.x, origPt.y, origPt.z);
  	return true;
  }
  
  //## CHECK IF POINT IS IN CELLS SURROUNDING PREDICTED CELL:
  for (int y=gridY-1; y<gridY+1; y++)
  {
  	for (int x=gridX-1; x<gridX+1; x++)  // for each cell: check if point is in that cell
      if( isPointInDeformedCell( pt, x, y, &origPt ) )
      {
        setPt( pt, origPt.x, origPt.y, origPt.z);
        return true;
      }
  }
    
  //## CHECK AGAINST ALL CELLS:
    
  for (int y=0; y<rowsY; y++)
  {
    for (int x=0; x<colsX; x++)  // for each cell: check if point is in that cell
      if( isPointInDeformedCell( pt, x, y, &origPt ) )
      {
        setPt( pt, origPt.x, origPt.y, origPt.z);
        return true;
      }
  }  
  
  cerr << "ERROR: DeformGridSlice::adjustPtByGridInverse() - point was determined to be inside edges, but can't find which cell" << endl;
  
  return true;
}



//------------------------
//-- ?

void DeformGridSlice::adjustGridByTransform( SimpleTransform t, bool origGrid, bool defGrid, bool inverse )
{
  Ipoint centerPt;
  setPt( &centerPt, (maxX-minX)/2.0, (maxY-minY)/2.0, sliceNum );
  
  if( inverse )
  {
  	if(origGrid){
      for (int i=0; i<(int)vect.size(); i++)
        t.adjustPtByTransformInverse( &vect[i].ptS, &centerPt );
  	}
  	if(defGrid){
      for (int i=0; i<(int)vect.size(); i++)
        t.adjustPtByTransformInverse( &vect[i].ptE, &centerPt );
  	}
  }
  else
  {
  	if(origGrid){
      for (int i=0; i<(int)vect.size(); i++)
        t.adjustPtByTransform( &vect[i].ptS, &centerPt );
  	}
  	if(defGrid){
      for (int i=0; i<(int)vect.size(); i++)
        t.adjustPtByTransform( &vect[i].ptE, &centerPt );
  	}
  }
}

//------------------------
//-- ?

void DeformGridSlice::adjustDeformedGridByTransform( SimpleTransform t )
{
  Ipoint centerPt;
  setPt( &centerPt, (maxX-minX)/2.0, (maxY-minY)/2.0, sliceNum );
  for (int i=0; i<(int)vect.size(); i++)
  {
  	t.adjustPtByTransform( &vect[i].ptE, &centerPt );
  	vect[i].recalcVector();
  }
}

//------------------------
//-- ?

void DeformGridSlice::adjustDeformedGridByTransformInverse( SimpleTransform t )
{
  Ipoint centerPt;
  setPt( &centerPt, (maxX-minX)/2.0, (maxY-minY)/2.0, sliceNum );
  for (int i=0; i<(int)vect.size(); i++)
  {
  	t.adjustPtByTransformInverse( &vect[i].ptE, &centerPt );
  	vect[i].recalcVector();
  }
}

//------------------------
//-- Returns true if the point is within the deformed cell at (x,y) and also
//-- returns a best estimate of cooresponding original point.
//-- Otherwise returns false.
//-- 
//-- NOTE: This is currently an estimate because of the way I calculate a points position within a deformed cell.
//--       I suspect that doing it properly will require a more powerful algebraic equation.   %%%%%%%%

bool DeformGridSlice::isPointInDeformedCell( Ipoint *pt, int x, int y, Ipoint *origPt )
{
  Icont *defCell = imodContourNew();
  getDeformedCellAsContour( defCell, x, y );	  // gets clockwise contour around deformed cell
  
  if( imodPointInsideCont( defCell, pt ) )
  {
  	//float fractRight = line_getFractPtBetweenTwoRays( defCell.pts[0],defCell.pts[1],defCell.pts[3],defCell.pts[2], pt );
  	//float fractUp    = line_getFractPtBetweenTwoRays( defCell.pts[0],defCell.pts[3],defCell.pts[1],defCell.pts[2], pt );
  	
    //cout << "NOT YET ADAPTED" << endl;      //%%%%%%%%%%
    
  	float fractUp       = line_getFractPtBetweenTwoLines( getPt(defCell,0), getPt(defCell,1), getPt(defCell,3), getPt(defCell,2), pt, true );
    float fractRight    = line_getFractPtBetweenTwoLines( getPt(defCell,0), getPt(defCell,3), getPt(defCell,1), getPt(defCell,2), pt, true );
    
    origPt->x = (x+fractRight) * cellWidth   + minX;
    origPt->y = (y+fractUp   ) * cellHeight  + minY;
    origPt->z = pt->z;
    imodContourDelete(defCell);
    return true;
  }
  
  imodContourDelete(defCell);
  return false;
}


//------------------------
//-- Returns a clockwise contour (starting from the bottom left corner) with 4 points representing the deformed cell
//-- at the given (x,y) coordinates of the grid.

void DeformGridSlice::getDeformedCellAsContour( Icont *deformedCell, int cellX, int cellY )
{
  imodContourDefault( deformedCell );
  
  if( !isBetweenAsc(0, cellX, colsX-1) || !isBetweenAsc(0, cellX, colsX-1) ) {
  	cerr << "ERROR: DeformGridSlice::getCellAsContour - bad input values" << endl;  	//%%%%%%%%
  	return;
  }
  
  imodPointAppend( deformedCell, &getVect(cellX  , cellY  ).ptE );  // bottom left  corner
  imodPointAppend( deformedCell, &getVect(cellX  , cellY+1).ptE );  // top    left  corner
  imodPointAppend( deformedCell, &getVect(cellX+1, cellY+1).ptE );  // top    right corner
  imodPointAppend( deformedCell, &getVect(cellX+1, cellY  ).ptE );  // bottom right corner
}

//------------------------
//-- Returns a clockwise contour (starting from the bottom left corner) with 4 points representing the deformed cell
//-- at the given a point in the original grid, or an empty contour if the point is outside the grid.

void DeformGridSlice::getDeformedCellAsContour( Icont *deformedCell, Ipoint ptOrig )
{
  return getDeformedCellAsContour( deformedCell, idxX(ptOrig), idxY(ptOrig) );
}


//------------------------
//-- Returns a clockwise contour (starting from the bottom left corner) with 4 points representing the original cell
//-- at the given a point in the original grid, or an empty contour if the point is outside the grid.

void DeformGridSlice::getCellAsContour( Icont *cell, Ipoint pt )
{
  imodContourDefault( cell );
  
  int cellX = idxX(pt);
  int cellY = idxY(pt);
  
  if( !isBetweenAsc(0, cellX, colsX-1) || !isBetweenAsc(0, cellX, colsX-1) )
  {
  	wprint( "ERROR: DeformGridSlice::getCellAsContour - bad input values" );  	//%%%%%%%%
  	return;
  }
  
  imodPointAppend( cell, &getVect(cellX  , cellY  ).ptS );  // bottom left  corner
  imodPointAppend( cell, &getVect(cellX  , cellY+1).ptS );  // top    left  corner
  imodPointAppend( cell, &getVect(cellX+1, cellY+1).ptS );  // top    right corner
  imodPointAppend( cell, &getVect(cellX+1, cellY  ).ptS );  // bottom right corner
}

//------------------------
//-- Counts the number of "bad" deformed cells, which are represented by cells which have overlapping edges (non-simple polygons).
//-- If a grid has "bad" cells it is likely to produce many problems/errors because > 1 point on the original grid will map to
//-- the same point on the deformed grid.

int DeformGridSlice::countBadDeformedCells()
{
  int numOverlappingCells = 0;
  
  Icont *deformedCell = imodContourNew();
  for (int y=0; y<rowsY; y++)
  {
    for (int x=0; x<colsX; x++)  // for each cell:
  	{
      getDeformedCellAsContour(deformedCell,x,y);
      if( !cont_isSimple( deformedCell ) )
        numOverlappingCells++;
  	}
  }
  
  imodContourDelete(deformedCell);
  return numOverlappingCells;
}

//------------------------
//-- Sets the end point of all grid vectors using the vector values at the four corners of the grid.
//-- It does this by simply interpolating all the values accross and down.

bool DeformGridSlice::setAllGridPointsUsingCornerVectors( vector<Ivector> vect, Ivector vectBL, Ivector vectTL, Ivector vectTR, Ivector vectBR, int zVal )
{
  if( isEmpty() )
  	return false;
  
  float cellFractX = 1.0 / colsX;
  float cellFractY = 1.0 / rowsY;
  
  for (int y=0; y<yPts; y++)  // for each point along Y:
  {
  	for (int x=0; x<xPts; x++)  // for each point along X:
  	{
      float weightY    = float(y) * cellFractY;
      float weightYi   = 1.0 - weightY;
      
      float weightX    = float(x) * cellFractX;
      float weightXi   = 1.0 - weightX;
      
      float totalX =  (weightXi) * (weightYi) * vectBL.vector.x
        +	(weightX ) * (weightYi) * vectBR.vector.x
        +	(weightX ) * (weightY ) * vectTR.vector.x
        +	(weightXi) * (weightY ) * vectTL.vector.x;
      
      float totalY =  (weightXi) * (weightYi) * vectBL.vector.y
        +	(weightX ) * (weightYi) * vectBR.vector.y
        +	(weightX ) * (weightY ) * vectTR.vector.y
        +	(weightXi) * (weightY ) * vectTL.vector.y;
      
      getVect(x,y).setVector( newPt( totalX, totalY, zVal ) );
  	}
  }
  return true;
}

//------------------------
//-- Sets the end point of grid vectors along the edge of the grid using the vector values at the four corners of the grid.
//-- It does this by simply interpolating all the values accross and down.

bool DeformGridSlice::setEdgeGridPointsUsingCornerVectors( vector<Ivector> vect, Ivector vectBL, Ivector vectTL, Ivector vectTR, Ivector vectBR, int zVal, bool setMiddlePtsToo )
{
  if( isEmpty() )
  	return false;
  
  float cellFractX = 1.0 / colsX;
  float cellFractY = 1.0 / rowsY;
  
  for (int x=0; x<xPts; x++)  // for each column along X:
  {
  	float weightX    = float(x) * cellFractX;
  	
  	getVect(x,0    ).setEndPt( line_findPtFractBetweenPts2D( &vectBL.ptE, &vectBR.ptE, weightX ) );  // set point on bottom
  	getVect(x,rowsY).setEndPt( line_findPtFractBetweenPts2D( &vectTL.ptE, &vectTR.ptE, weightX ) );  // set point on top
  }
  
  for (int y=0; y<yPts; y++)  // for each row along Y:
  {
  	float weightY    = float(y) * cellFractY;
  	getVect(0,    y).setEndPt( line_findPtFractBetweenPts2D( &vectBL.ptE, &vectTL.ptE, weightY ) );  // set point on left
  	getVect(colsX,y).setEndPt( line_findPtFractBetweenPts2D( &vectBR.ptE, &vectTR.ptE, weightY ) );  // set point on right
  }
  
  if(setMiddlePtsToo)
  {
  	for (int y=1; y<rowsY; y++)
  	{
      float weightY    = float(y) * cellFractY;
      Ipoint &endPtLeft  = getVect(0    ,y).ptE;
      Ipoint &endPtRight = getVect(colsX,y).ptE;
      for (int x=1; x<colsX; x++)
        getVect(x,y).setEndPt( line_findPtFractBetweenPts2D( &endPtLeft, &endPtRight, weightY ) );
  	}
  }
  return true;
}


//------------------------
//-- Generates a minimum bounding rectangle (defined by: minX, maxX, minY, maxY) around the defomed grid
//-- by examining every vector in the grid.

void DeformGridSlice::generateMBR()
{
  mbr_reset( &mbrLL, &mbrUR );
  for (int i=0; i<(yPts*xPts); i++)
  	mbr_addPt( &vect[i].ptE, &mbrLL, &mbrUR );
}

//------------------------
//-- Generates "contAroundEdge" - a clockwise contour around the edge of a grid starting at the bottom left corner.

void DeformGridSlice::generateContAroundEdge()
{
  imodContourDefault( contAroundEdge );
  for (int y=0; y<rowsY; y++)  imodPointAppend( contAroundEdge, &getVect(0,y).ptE );  	// up along left
  for (int x=0; x<colsX; x++)  imodPointAppend( contAroundEdge, &getVect(x,rowsY).ptE );  // right along top
  for (int y=rowsY; y>0; y--)  imodPointAppend( contAroundEdge, &getVect(colsX,y).ptE );  // down along right
  for (int x=colsX; x>0; x--)  imodPointAppend( contAroundEdge, &getVect(x,0).ptE );  	// left along bottom
}

//------------------------
//-- Returns "contAroundEdge" - see generateContAroundEdge()

void DeformGridSlice::getContourAroundEdge( Icont *cont )
{
  generateContAroundEdge();
  cont_copyPts( contAroundEdge, cont, true );
}

//------------------------
//-- Generates a clockwise contour around the four corners of the tranform

void DeformGridSlice::getContourAroundTranformCorners( Icont *transfRect )
{
  imodContourDefault( transfRect );
  
  Ipoint bl  = newPt(minX,minY,sliceNum);
  Ipoint blN = adjustPtByTransformGet( &bl );
  imodPointAppend( transfRect, &blN );  // bottom left  corner
  
  Ipoint tl = newPt(minX,maxY,sliceNum);
  Ipoint tlN = adjustPtByTransformGet( &tl );
  imodPointAppend( transfRect, &tlN );  // top    left  corner
  
  Ipoint tr = newPt(maxX,maxY,sliceNum);
  Ipoint trN = adjustPtByTransformGet( &tr );
  imodPointAppend( transfRect, &trN );  // top    right corner
  
  Ipoint br = newPt(maxX,minY,sliceNum);
  Ipoint brN = adjustPtByTransformGet( &br );
  imodPointAppend( transfRect, &brN );  // bottom right corner
}

//------------------------
//-- Returns all the vector points around the egde of the grid in as a vector of Ivectors in a clockwise
//-- order starting at the bottom left corner.

vector<Ivector> DeformGridSlice::getEdgePtsAsVector( bool includeConrnerPts )
{
  vector<Ivector> returnVects;
  int offset = (includeConrnerPts) ? 0 : 1;
  
  for (int y=0+offset; y<rowsY; y++)  returnVects.push_back( getVect(0,y    ) );  // up along left
  for (int x=0+offset; x<colsX; x++)  returnVects.push_back( getVect(x,rowsY) );  // right along top
  for (int y=rowsY+offset; y>0; y--)  returnVects.push_back( getVect(colsX,y) );  // down along right
  for (int x=colsX+offset; x>0; x--)  returnVects.push_back( getVect(x,    0) );  // left along bottom
  
  return returnVects;
}

//------------------------
//-- Uses the four corner points of the grid to caculate a best-fit transform.

void DeformGridSlice::calculateBestFitTransform()
{
  transform.calculateBestFitTransformUsingCorners( getVectBL(), getVectTL(), getVectTR(), getVectBR() );
}

//------------------------
//-- Updates "transform", "contAroundEdge" and "mbrDeformed".

void DeformGridSlice::updateGridTransformEdgeContAndMBR()
{
  calculateBestFitTransform();
  generateContAroundEdge();
  generateMBR();
}


//############################################################

//------------------------
//-- Default constructor.

DeformationGrid::DeformationGrid()
{
  reset();
}

//------------------------
//-- Resets the grid by setting all values to zero and clearing the array of slices.

void DeformationGrid::reset() {  	
  useDeformationGrid = false;
  colsX = 0;
  rowsY = 0;
  
  maxX = 0;
  maxY = 0;
  maxZ = 0;
  slices.clear();
  
  powerUsed = 3.0;
  enforceStraightEdges = false;
}

//------------------------
//-- Sets up the 3D grid by generating a uniform DeformGridSlice for each slice. 

void DeformationGrid::setupGrid( int colsX_, int rowsY_,
                                 float maxX_, float maxY_,
                                 int numSlices_, float _powerUsed ) {  	
  colsX = colsX_;
  rowsY = rowsY_;
  
  maxX = maxX_;
  maxY = maxY_;
  maxZ = numSlices_;
  
  powerUsed = _powerUsed;
  
  slices.resize( maxZ );
  for(int i=0; i<maxZ; i++) {
  	slices[i].setupGrid( colsX, rowsY, 0, 0, maxX, maxY, i );
  }
}

//------------------------
//-- Returns true if there are no slices yet.

bool DeformationGrid::isEmpty() {  	
  return ( slices.empty() );
}

//------------------------
//-- Inputs a vector of Ivectors representing deformation points and uses these to estimate the
//-- corners of the deformed grid slice should be, and returns these corners as a vector of
//-- four points in the order: bottom left, top left, top right, bottom right 

vector<Ivector> DeformationGrid::calcCornerPointsAfterDeform( vector<Ivector> vect, int zVal )
{
  int numVects = (int)vect.size();
  
  vector<Ivector> cnrVects;            	// used to store deformation vectors for the for corners of the model
  cnrVects.resize(4);
  cnrVects[CNR_BL] = Ivector( 0,    0,    zVal );    // bottom left  corner
  cnrVects[CNR_TL] = Ivector( 0,    maxY,	zVal );    // top    left  corner
  cnrVects[CNR_TR] = Ivector( maxX,	maxY,	zVal );    // top    right corner
  cnrVects[CNR_BR] = Ivector( maxX,	0,    zVal );    // bottom right corner
  
  if ( numVects == 1 )      // if there is only 1 displacement vector: use this to deform tomogram uniformly.
  {
  	for (int i=0; i<4; i++ )
      cnrVects[i].setVector( vect[0].vector );
  }
  else if ( numVects == 2 )    	// if there are 2 displacement vectors: use them to deform tomogram uniformly.
  {
  	float distOrig = line_distBetweenPts2D( &vect[0].ptS, &vect[1].ptS );
  	float distDefm = line_distBetweenPts2D( &vect[0].ptE, &vect[1].ptE );
  	float expand = fDiv( distDefm, distOrig );
  	
	  for (int i=0; i<4; i++ )	    // for each corner: calculate it's displacement
	  {
      float angle = line_angleFormed3Pts( &vect[0].ptS, &vect[1].ptS, &cnrVects[i].ptS );
      float dist  = line_distBetweenPts2D( &vect[1].ptS, &cnrVects[i].ptS );
      
      Ipoint defmPt = line_getPtRelativeToEnd( &vect[0].ptE, &vect[1].ptE, dist*expand, angle+180 );
      cnrVects[i].setEndPt( defmPt );
      //cout << " i=" << i <<  " angle=" << angle << " dist=" << dist << " cnrVects[i].pt=" << cnrVects[i].pt << " defmPt=" << defmPt << endl;                  //%%%%%%
	  }
  }
  else if ( numVects >= 3 )	    // if there are >3 displacement vectors: for each corner, use the closest and furthest vector to determine the corner's displacement.
  {	
	  for (int i=0; i<4; i++ )	    // for each corner:
	  {
      vector<IdxAndFloat> idxAndDist;        // list of vectors (vector indexes) and the distance to each one.
      idxAndDist.clear();
      
      for (int j=0; j<numVects; j++)
        idxAndDist.push_back( IdxAndFloat( j, line_distBetweenPts2D( &vect[j].ptS, &cnrVects[i].ptS ) ) );
      
      idxAndDist = vector_sort( idxAndDist );      // sorts the list based on the distance (from most near to most far)
      
      //cout << endl << endl << "AFTER SORTING: ... " << i << endl;      //%%%%%%
      //vector_printItems(idxAndDist);	            //%%%%%%
      
      Ivector &vect0 = vect.at( idxAndDist.front().idx );	  // closest  vector
      Ivector &vect1 = vect.at( idxAndDist.back().idx  );	  // furthest vector
      
      float distOrig = line_distBetweenPts2D( &vect0.ptS, &vect1.ptS );
      float distDefm = line_distBetweenPts2D( &vect0.ptE, &vect1.ptE );
      float expand = fDiv( distDefm, distOrig );
      
      float angle = line_angleFormed3Pts( &vect0.ptS, &vect1.ptS, &cnrVects[i].ptS );
      float dist  = line_distBetweenPts2D( &vect1.ptS, &cnrVects[i].ptS );
      
      Ipoint defmPt = line_getPtRelativeToEnd( &vect0.ptE, &vect1.ptE, dist*expand, angle+180 );
      cnrVects[i].setEndPt( defmPt );
	  }
  }
  
  return cnrVects;
}

//------------------------
//-- Inputs a vector of Ivectors representing deformation points and uses these to generate
//-- a deformation grid for the given boundary slice.

bool DeformationGrid::deformBoundarySliceUsingVectors( vector<Ivector> vect, int zVal, float powerUsed_ )
{
  //powerUsed = powerUsed_;
  int numVects = (int)vect.size();
  
  if( zVal >= (int)slices.size() || slices.at(zVal).vect.empty() )
  {
	  cout << "ERROR: deformBoundarySliceUsingVectors() - grid is not setup yet" << endl;
	  return false;
  }
  DeformGridSlice &ds = slices[zVal];
  //ds.sectionBoundary = true;    // make this a boundary slice (if not already)
  
  //## DETERMINE DISPLACEMENT FOR THE FOUR CORNERS OF THE MODEL:
  
  vector<Ivector> cnrVects = calcCornerPointsAfterDeform( vect, zVal );
  
  
  //## NORMALIZE ALL VECTORS:  
  
  vector<Ivector> cnrVectsO = cnrVects;
  
  for (int i=0; i<(int)vect.size(); i++)	    // adjusts all vectors so that they map within the starting square
	  vect[i].setEndPt( point_findPtInQuad1InMatchingQuad2( &vect[i].ptE,
                                                          &cnrVectsO[CNR_BL].ptE,  &cnrVectsO[CNR_TL].ptE,  &cnrVectsO[CNR_TR].ptE,  &cnrVectsO[CNR_BR].ptE,
                                                          &cnrVectsO[CNR_BL].ptS,  &cnrVectsO[CNR_TL].ptS,  &cnrVectsO[CNR_TR].ptS,  &cnrVectsO[CNR_BR].ptS  ) );
  
  for (int i=0; i<4; i++)
	  cnrVects[i].setVector( newPt(0,0,0) );
  
  
  //## DETERMINE THE DISPLACEMENT AT EACH POINT
  
  if( numVects == 1 )	        // if there is only 1 displacement vector: set all grid points to this value.
  {
	  for (int i=0; i<(int)ds.vect.size(); i++)
      ds.vect[i].setVector( vect[0].vector );
  }
  else if( numVects == 2 )	      // if there is only 2 vectors: set up grid using only corner vectors
  {
	  ds.setAllGridPointsUsingCornerVectors( vect, cnrVects[CNR_BL], cnrVects[CNR_TL], cnrVects[CNR_TR], cnrVects[CNR_BR], zVal );
  }
  else            // if there are > 3 vectors: set up grid using all vectors (including corner vectors)
  {
	  vect = vector_concat( vect, cnrVects );    // add corner vectors to list of displacement vectors.
	  
	  if(enforceStraightEdges)
	  {
      ds.setEdgeGridPointsUsingCornerVectors( vect, cnrVects[CNR_BL], cnrVects[CNR_TL], cnrVects[CNR_TR], cnrVects[CNR_BR], zVal, false );
      vector<Ivector> edgeVects = ds.getEdgePtsAsVector( false );
      vect = vector_concat( vect, edgeVects );    // add edge vectors to list of displacement vectors.
	  }
	  
	  for (int y=0; y<ds.yPts; y++)
      for (int x=0; x<ds.xPts; x++)  // for each point:
      {
        Ivector &currGridV = ds.getVect(x,y);  //vect[ y*ds.yPts + x ];
        
        vector<IdxAndFloat> idxAndDist;	    // list of vectors (vector indexes) and the distance to each one.
        idxAndDist.clear();
        
        bool matchingPtFound = false;
        
        for (int j=0; j<(int)vect.size(); j++)
        {
          float distance = line_distBetweenPts2D( &vect[j].ptS, &currGridV.ptS );
          idxAndDist.push_back( IdxAndFloat( j, distance ) );
          if( distance == 0 ) {
            currGridV.setEndPt( vect[j].ptE );
            matchingPtFound = true;
            break;
          }
        }
        if( matchingPtFound == true )  // a matching point was found for this gridpoint: go to next gridpoint
          continue;
        
        idxAndDist = vector_sort( idxAndDist );	  // sorts the list based on the distance (from most near to most far)
        
        //cout << endl << endl << "BEFORE WEIGHT APPLIED: ... " << i << endl;      //%%%%%%
        //vector_printItems(idxAndDist);	              //%%%%%%
        
        float totalWeight = 0;
        for (int j=0; j<(int)idxAndDist.size(); j++)
        {
          idxAndDist[j].dist = pow(idxAndDist[j].dist, powerUsed);
          idxAndDist[j].dist = fDiv( 1.0, idxAndDist[j].dist );
          totalWeight += idxAndDist[j].dist;
        }
        
        for (int j=0; j<(int)idxAndDist.size(); j++)
          idxAndDist[j].dist = fDiv( idxAndDist[j].dist, totalWeight );
        
        //cout << endl << endl << "AFTER WEIGHTING APPLIED: ... " << i << endl;      //%%%%%%
        //vector_printItems(idxAndDist);                //%%%%%%
        
        float totalX = 0;
        float totalY = 0;
        
        for (int j=0; j<(int)idxAndDist.size(); j++)
        {
          Ivector &currVect = vect.at( idxAndDist[j].idx );
          
          totalX += currVect.vector.x * idxAndDist[j].dist;
          totalY += currVect.vector.y * idxAndDist[j].dist;
        }
        
        currGridV.setVector( newPt( totalX, totalY, zVal ) );
      }
  }
  

  //## UN-NORMALIZE GRID:
    
  for (int i=0; i<(ds.yPts*ds.xPts); i++)
  {
    ds.vect[i].setEndPt(
        point_findPtInQuad1InMatchingQuad2( &ds.vect[i].ptE,
                &cnrVectsO[CNR_BL].ptS, &cnrVectsO[CNR_TL].ptS, &cnrVectsO[CNR_TR].ptS, &cnrVectsO[CNR_BR].ptS,
                &cnrVectsO[CNR_BL].ptE, &cnrVectsO[CNR_TL].ptE, &cnrVectsO[CNR_TR].ptE, &cnrVectsO[CNR_BR].ptE  ) );
  }
  
  ds.updateGridTransformEdgeContAndMBR();
    
  return true;
}

//------------------------
//-- Generates a deformation grid for the given (non-boundary) slice by finding the nearest top and/or bottom boundary slices
//-- and linearly interpolating between the two.
//-- If only one boundary slice is identified either above or below this slice, the vector values from this slice are simply copied.

bool DeformationGrid::deformNonBoundarySliceUsingInterpolation( int zVal )
{
  if ( zVal < 0 || zVal >= maxZ || slices[zVal].sectionBoundary ) {
	  cout << "ERROR: deformNonBoundarySliceUsingInterpolation - bad z value or not a section boundary" << endl;
	  return false;
  }
  
  DeformGridSlice &currSlice = slices[zVal];
  
  //## FIND NEAREST BOUNDARY SLICE ABOVE AND BELOW (IF THEY EXIST):
  
  int boundaryAboveZ = -1;
  int boundaryBelowZ = -1;
  
  bool isBoundaryAbove = false;
  bool isBoundaryBelow = false;
  
  for( int i=zVal+1; i<maxZ; i++ )
  {
	  if( slices[i].sectionBoundary )
    {
      boundaryAboveZ = i;
      isBoundaryAbove = true;
      break;
	  }
  }   
  for( int i=zVal-1; i>=0; i-- )
  {
    if( slices[i].sectionBoundary )
    {
      boundaryBelowZ = i;
      isBoundaryBelow = true;
      break;
    }
  }   
  //## SET VECTOR POINTS IN GIVEN NON-BOUNDARY SLICE:
  
  if( isBoundaryAbove && isBoundaryBelow )	    // if between 2 boundary slices:  calculate linear interpolation for each vector
  {
    float fractionUp = (float)(zVal-boundaryBelowZ) / (float)(boundaryAboveZ-boundaryBelowZ);
    
    for( int i=0; i<(int)currSlice.vect.size(); i++ )
    {
      Ipoint newEndPt = line_findPtFractBetweenPts2D( &slices[boundaryBelowZ].vect[i].ptE, &slices[boundaryAboveZ].vect[i].ptE, fractionUp );
      newEndPt.z = zVal;
      currSlice.vect[i].setEndPt( newEndPt );
    }
  }
  else if( isBoundaryAbove && !isBoundaryBelow )	        // if there is only 1 boundary slice above: make vectors same as slice above
  {
    for( int i=0; i<(int)currSlice.vect.size(); i++ )
      currSlice.vect[i].setVector( slices[boundaryAboveZ].vect[i].vector );
  }
  else if( !isBoundaryAbove && isBoundaryBelow )	        // if there is only 1 boundary slice below: make vectors same as slice below
  {
    for( int i=0; i<(int)currSlice.vect.size(); i++ )
      currSlice.vect[i].setVector( slices[boundaryBelowZ].vect[i].vector );
  }
  else if( !isBoundaryAbove && !isBoundaryBelow )    // if no boundary slices: set vectors to 0      (NOTE: this is unlikely)
  {
    Ipoint nullPt;
    setPt( &nullPt, 0,0,0 );
    for( int i=0; i<(int)currSlice.vect.size(); i++ )
      currSlice.vect[i].setVector( nullPt );
  }
  
  currSlice.updateGridTransformEdgeContAndMBR();
  
  return true;
}

//------------------------
//-- Inputs a vector of Ivectors representing deformation points and uses this to generate
//-- a deformation grids for each slice. The Ivectors are used to determine and set which slices are boundary slices.

bool DeformationGrid::deformEntireGridUsingVectors( vector<Ivector> vect, bool automaticallySelectSectionBoundaries, bool generateForAllSlices, bool applyCumulativeTransforms )
{
  //## SEPERATE VECTORS OUT INTO DIFFERENT SLICES:
  
  wprint( "DEFORMATION GRID:\n" );
  wprint( "  rows: %d \n", colsX );
  wprint( "  cols: %d \n", rowsY );
  wprint( "  power used: %f \n", powerUsed );
  wprint( "  enforce straight edges: %f:", enforceStraightEdges );
  
  vector< vector<Ivector> > sliceVect;
  sliceVect.resize( maxZ );
  
  for( int i=0; i<(int)vect.size(); i++ )
  {
	  int zVal = vect[i].getZVal();
	  if( zVal < 0 || zVal >= maxZ ) {
      cout << "ERROR: deformEntireGridUsingVectors - found vector out of range" << endl;
      continue;
	  }
	  sliceVect[ zVal ].push_back( vect[i] );
  }
  
  //## GENERATE DEFORMATION GRID FOR BOUNDARY GRIDS (AND LABEL SLICES AS BOUNDARY OR NON-BOUNDARY):
  
  for( int i=0; i<maxZ; i++ ) 
  {
	  if( !sliceVect[i].empty() ) {
      deformBoundarySliceUsingVectors( sliceVect[i], i, powerUsed );
      slices[i].sectionBoundary = true;    // label this as a boundary slice (if not already)
	  }
	  else {
      slices[i].sectionBoundary = false;    // label this as a non-boundary slice (if not already)
	  }
  }
  
  //## ADJUST BOUNDARY DEFORMATION GRIDS USING TRANSLATE AND ROTATE TRANSFORMS:
  
  if( applyCumulativeTransforms )
  {

    SimpleTransform cumulativeTransform;
    cumulativeTransform.scaleX = 1.0;
    cumulativeTransform.scaleY = 1.0;
    //bool sectionBondaryEncountered;
    
    
    for( int i=0; i<maxZ; i++ ) 
    {
      if ( slices[i].sectionBoundary == true )	  // if this is a section boundary
      {
        cumulativeTransform.addTransformsIgnoreScale( slices[i].transform );
        //slices[i].adjustGridByTransform( cumulativeTransform, false, true, true );
        slices[i].adjustDeformedGridByTransformInverse( cumulativeTransform );
        
        cout << endl << "SECTION " << i << endl;	        //%%%%%
        cout << " ... slice transform = "      << slices[i].transform;  //%%%%%
        cout << " ... cumulative transform = " << cumulativeTransform << endl;  //%%%%%
        
        if (i<maxZ-1) {
          slices[i+1].adjustDeformedGridByTransformInverse( cumulativeTransform );
          slices[i+1].sectionBoundary = true;
          i++;
        }
      }
    }
    
    for( int i=0; i<maxZ; i++ )
    {
      if ( slices[i].sectionBoundary == true )	  // if this is a section boundary
      {
        slices[i].updateGridTransformEdgeContAndMBR();
        cout << i << ", ";
      }
    }
    
  }
  //## GENERATE INTERPOLATED GRID FOR ALL NON-BOUNDARY GRIDS:
  
  if( generateForAllSlices )
  {
    for( int i=0; i<maxZ; i++ ) 
      if( slices[i].sectionBoundary == false )
      {
        deformNonBoundarySliceUsingInterpolation( i );
      }
  }
    
  return true;
}


//------------------------
//-- Adjusts a point using the grid - returns false if point is outside the deform grid

bool DeformationGrid::adjustPointByGrid( Ipoint *pt, bool inverse, bool adjustIfOutside )
{
  if ( pt->z < 0 || pt->z >= (int)slices.size() )
	  return false;
  
  //if(adjustIfOutside)
  //	cout << "WARNING: DeformationGrid::adjustPointByGrid - adjustIfOutside not implemented yet" << endl;	//%%%%%%%%%%%%%%%%%%%%
  
  if (inverse)
	  return slices[(int)pt->z].adjustPtByGridInverse( pt );
  else
	  return slices[(int)pt->z].adjustPtByGrid( pt );
}

//------------------------
//-- Determines if all deformation grid is "good" by checking each slices for "bad" cells - see DeformationGrid::countBadDeformedCells();

bool DeformationGrid::isGood(bool checkOnlySectionBoundaries)
{
  int totalBadCells = 0;
  
  cout << "CHECKING FOR BAD CELLS" << endl;
  
  for( int z=0; z<(int)slices.size(); z++ ) 
	  if( !checkOnlySectionBoundaries || slices[z].sectionBoundary ) 
	  {
      int badCells = slices[z].countBadDeformedCells();
      totalBadCells += badCells;
      
      if(badCells)
        cout << "  bad cells found on slice " << z << " * " << badCells << endl;
	  }
      
      cout << "  ... total bad cells = " << totalBadCells << endl;
  
  return ( totalBadCells==0 );
}



//------------------------
//-- Prints summary info about deoformation grid to cout 

void DeformationGrid::printInfo( bool includeSliceList )
{
  int numSectionBoundaries = 0;
  for (int i=0;i<(int)slices.size(); i++)
	  if( slices[i].sectionBoundary )
      numSectionBoundaries++;
  
  cout	            << endl << endl;
  cout << "DEFORMATION GRID SUMMARY:"	      << endl;
  cout 	          << endl;
  cout << "slices = "  << (int)slices.size()	<< endl;
  cout << "colsX  = "  << colsX    << endl;
  cout << "rowsY  = "  << rowsY    << endl;
  cout << "maxX   = "  << maxX    << endl;
  cout << "maxY   = "  << maxY    << endl;
  cout << "maxZ   = "  << maxZ    << endl;
  cout 	          << endl;
  cout << "powerUsed = "	<< powerUsed	  << endl;
  cout << "enforceStraightEdges = "	<< enforceStraightEdges << endl;
  cout 	          << endl;
  cout 	          << endl;
  cout << "number of section boundaries = "	<< numSectionBoundaries << endl;
  
  if( includeSliceList )
  {
	  cout << "slices: "        << endl;
	  cout << "\t" << "slice" << "\t" << "boundry" << "\t" << "quality" << "\t" << "numVects" << endl;
	  for (int i=0;i<(int)slices.size(); i++)
      cout << "\t" << slices[i].sliceNum << "\t" << slices[i].sectionBoundary << "\t" << slices[i].sectionQuality << "\t" << (int)slices[i].vect.size() << endl;
  }
}


//############################################################




//############################################################
#ifndef INC_IDGRID_H
#define INC_IDGRID_H

//############################################################

#include "_common_functions.h"
#include "imodplugin.h"
#include "icontextra.h"
#include "ivector.h"
#include <vector>
using namespace std;

//############################################################

enum cornerpoints { CNR_BL, CNR_TL, CNR_TR, CNR_BR };		// used to index corner points in the clockwise order: bottom left, top left, top right, bottom right

//############################################################

struct DeformGridSlice
{
//## DATA:
	
	int colsX;            // columns along X the grid is divided into
	int rowsY;            // row     along Y the grid is divided into
	
	int xPts;             // the number of points along X (colsX + 1)
	int yPts;             // the number of points along Y (rowsY + 1)
	
	float minX;	// |-- the minimum and maximum X & Y values over which the grid spans
	float minY;	// |
	float maxX;	// |
	float maxY;	// |
	
	float cellWidth;                // stores the width  of each cell
	float cellHeight;               // stores the height of each cell
	
	int sliceNum;                   // identifies the slice number (z value)
	bool sectionBoundary;           // wether or not this slice is on the edge of a section boundary
	float sectionQuality;           // crude measure used to indicate how good this section is.... which will affect how much it can be deformed.
	
	Icont  *contAroundEdge;         // stores a clockwise contour around the edge of the deformed grid
	
  Ipoint mbrLL;                   // a minimum bounding grid around the edge of the deformed grid.
  Ipoint mbrUR;                   // a minimum bounding grid around the edge of the deformed grid.
  //MBRect mbrDeformed;           // a minimum bounding grid around the edge of the deformed grid.
	
	vector<Ivector> vect;           // stores all the vectors in the grid
	
	SimpleTransform transform;      // used to store the linear transform which best approximates the effect of the deformation grid
	
	
//## METHODS:
	
	DeformGridSlice();																									//-- Default constructor
	DeformGridSlice( int colsX_, int rowsY_, float minX_, float minY_, float maxX_, float maxY_, int sliceNum_ );		//-- Default constructor
	
	void reset();																										//-- Resest all values to zero and clears the vector of points.
	void setupGrid( int colsX_, int rowsY_, float minX_, float minY_, float maxX_, float maxY_, int sliceNum_ );		//-- Sets up the grid by calculating width and height values, and setting up the vector of start and end points in a uniformly spaced grid.
	
	Ivector &getVect( int x, int y );			//-- Returns the vector at the given grid point
	Ivector &getVectSafe( int x, int y );	//-- Returns the vector at the given grid point (and avoids out-of-bounds error for a row or colum which doesn't exist)
	int idxX( Ipoint pt );                 //-- Returns the grid X column index for the cell at the given point
	int idxY( Ipoint pt );                 //-- Returns the grid Y row    index for the cell at the given point
	
	void adjustPtByTransform( Ipoint *pt );				//-- Takes a point and adjusts it using the calculated best-fit transform. //-- NOTE: This gives a guess of where the transformed point might lie.
	Ipoint adjustPtByTransformGet( Ipoint *pt );
	void adjustPtByTransformInverse( Ipoint *pt );    //-- Takes a point and adjusts it using the INVERSE of the calculated best-fit transform. //-- NOTE: This gives a guess of where the original point might lie.
	bool adjustPtByGrid( Ipoint *pt );								//-- Takes a point within the original grid, and changes it to the cooresponding deformed point using the deformation grid. If the point is outside the grid it aborts and returns false (instead of true).
	bool adjustPtByGridInverse( Ipoint *pt );					//-- Takes a point within the deformed grid, and makes a best estimate of the cooresponding point in the origonal grid. If the point is outside the deformation grid it aborts and returns false (instead of true).
	
	void adjustGridByTransform( SimpleTransform t, bool origGrid, bool defGrid, bool inverse );
	void adjustDeformedGridByTransform( SimpleTransform t );			//
	void adjustDeformedGridByTransformInverse( SimpleTransform t );		//
	
	bool isPointInDeformedCell( Ipoint *pt, int x, int y, Ipoint *origPt );	//-- Returns true if the point is within the deformed cell at (x,y) and also returns a best estimate of cooresponding original point. Otherwise returns false.
	
	void getDeformedCellAsContour( Icont *deformedCell, int cellX, int cellY );						//-- Returns a clockwise contour (starting from the bottom left corner) with 4 points representing the deformed cell at the given (x,y) coordinates of the grid.	
	void getDeformedCellAsContour( Icont *cont, Ipoint ptOrig );							//-- Returns a clockwise contour (starting from the bottom left corner) with 4 points representing the deformed cell at the given (x,y) coordinates of the grid.	
	void getCellAsContour( Icont *cell, Ipoint pt );										//-- Returns a clockwise contour (starting from the bottom left corner) with 4 points representing the original cell at the given a point in the grid, or an empty contour if the point is outside the grid.
	int countBadDeformedCells();												//-- Counts the number of "bad" deformed cells, which are represented by cells which have overlapping edges (non-simple polygons).
	bool setAllGridPointsUsingCornerVectors( vector<Ivector> vect, Ivector vectBL, Ivector vectTL, Ivector vectTR, Ivector vectBR, int zVal );		//-- Sets the end point of all grid vectors using the vector values at the four corners of the grid.
	bool setEdgeGridPointsUsingCornerVectors( vector<Ivector> vect, Ivector vectBL, Ivector vectTL, Ivector vectTR, Ivector vectBR, int zVal, bool setMiddlePtsToo=false );	//-- Sets the end point of grid vectors along the edge of the grid using the vector values at the four corners of the grid.
	
	void  generateMBR();				//-- Generates a minimum bounding rectangle (defined by: minX, maxX, minY, maxY) around the defomed grid by examining every vector in the grid.
	void  generateContAroundEdge();		//-- Generates "contAroundEdge" - a clockwise contour around the edge of a grid starting at the bottom left corner.
	void  getContourAroundEdge( Icont *contAroundEdge );		//-- Returns "contAroundEdge" - see generateContAroundEdge()
	
	void  getContourAroundTranformCorners( Icont *transfRect );		//-- Generates a clockwise contour around the four corners of the tranform
	
	vector<Ivector> getEdgePtsAsVector( bool includeConrnerPts );		//-- Returns all the vector points around the egde of the grid in as a vector of Ivectors in a clockwise order starting at the bottom left corner.
	
	void calculateBestFitTransform();									//-- Uses the four corner points of the grid to caculate a best-fit transform.
	void updateGridTransformEdgeContAndMBR();							//-- Updates "transform", "contAroundEdge" and "mbrDeformed".
	
	
	inline Ivector &getVectBL() { return getVect(0,		0    ); }			// bottom left  corner
	inline Ivector &getVectBR() { return getVect(colsX,	0    ); }			// bottom right corner
	inline Ivector &getVectTR() { return getVect(colsX,	rowsY); }			// top    right corner
	inline Ivector &getVectTL() { return getVect(0,		rowsY); }			// top    left  corner
	inline bool isEmpty()		{ return ( vect.empty() ); }				// returns true if the grid is not populated with vectors
};

//############################################################

struct DeformationGrid 						// Stores a Deformation Grid which spans over the model and contains a DeformGridSlice for each slice
{
//## DATA:
	
	bool useDeformationGrid;
	
	int colsX;		// columns along X each slice is divided into
	int rowsY;		// row     along Y each slice is divided into
	
	float maxX;		// the width  of the model
	float maxY;		// the height of the model
	int maxZ;     // the number of slices the model spans
	
	float powerUsed;				// the power used to weight deformation vectors
	bool enforceStraightEdges;		// causes all grid points on the edges to be distributed linearly between it's corner points
	
	vector<DeformGridSlice> slices;		// a vector of 2D DeformationGrids representing the slices of the mondel
	
//## METHODS:
	
	DeformationGrid();					//-- Default constructor
	
	void reset();                                         //-- Resets the grid by setting all values to zero and clearing the array of slices.
	void setupGrid( int colsX_, int rowsY_,
                  float maxX_, float maxY_,
                  int numSlices_, float _powerUsed );		//-- Sets up the 3D grid by generating a uniform DeformGridSlice for each slice. 
	bool isEmpty();                                       //-- Returns true if there are no slices yet.
	
	vector<Ivector> calcCornerPointsAfterDeform( vector<Ivector> vect, int zVal );							//-- Inputs a vector of Ivectors representing deformation points and uses these to estimate the corners of the deformed grid slice should be, and returns these corners as a vector of four points in the order: bottom left, bottom right, top right, top left
	bool deformBoundarySliceUsingVectors( vector<Ivector> vect, int zVal, float powerUsed_=2.0 );			//-- Inputs a vector of Ivectors representing deformation points and uses these to generate a deformation grid for the given boundary slice.
	bool deformNonBoundarySliceUsingInterpolation( int zVal );												//-- Generates a deformation grid for the given (non-boundary) slice by finding the nearest top and/or bottom boundary slices and linearly interpolating between the two.
	
	bool adjustPointByGrid( Ipoint *pt, bool inverse, bool adjustIfOutside = false );						//-- Adjusts a point using the grid - returns false if point is outside the deform grid
	
	bool deformEntireGridUsingVectors( vector<Ivector> vect, bool automaticallySelectSectionBoundaries, bool generateForAllSlices, bool applyCumulativeTransforms );			//-- Inputs a vector of Ivectors representing deformation points and uses this to generate a deformation grids for each slice. The Ivectors are used to determine and set which slices are boundary slices.
	bool isGood( bool checkOnlySectionBoundaries );															//-- Determines if all deformation grid is "good" by checking each slices for "bad" cells - see DeformationGrid::countBadDeformedCells();
	
	void printInfo( bool includeSliceList=true );															//-- Prints summary info about deoformation grid to cout
};

//############################################################

#endif
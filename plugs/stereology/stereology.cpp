/*
 *  stereology.cpp -- Special plugin for performing point counting stereology
 *
 */

/*****************************************************************************
 *   Copyright (C) 2011 by Andrew Noske - work done while at The National    *
 *   Center for Microscopy and Imaging Research at UCSD                      *
 *****************************************************************************/

/*  $Author$

 
 
 When this plugin is first launched (via: Special > Stereology) it starts by
 checking for any "STEREOLOGY" objects already setup in the model.
 If none are found it prompts the user to setup the grid and then setup and
 "Finalize" two or more different "categories". Once the user hits okay new 
 objects will be setup and named/labelled as per this example:
 
   > STEREOLOGY.GRID #grid_set=1#type=pts#
     > object label: STEREOLOGY.GRID #grid_set=1#type=pts#spacing=50,50,5#
                               x=10,1014#y=10,1014#z=10,190#(DONT_RENAME)#
	 
   > NO_CATEGORY.     #(STEREOLOGY)#grid_set=1#
   > Nucleus.         #(STEREOLOGY)#grid_set=1#
   > Mitochondrion.   #(STEREOLOGY)#grid_set=1#
   > Vesicle.         #(STEREOLOGY)#grid_set=1#
     > object labels: same as the name
 
 All of these objects are set to scattered points with a single contour.
 
 The "STEREOLOGY.GRID" object is the most important one - the label of this
 object contains/stores instructions about how the grid is setup. The object's 
 name can be used to overwrite one or more parameters values, but while users
 can easily delete or change an object name they cannot easily access/change
 object labels.
 As the user click "Next" to go to a new points, the ZAP window jumps to 
 center on the next "unchecked" position in the grid (left to right then down)
 and a point is added to the "STEREOLOGY.GRID" object. The number of points
 in "STEREOLOGY.GRID" is thus is equal to the the number of points the
 user has gone though.
 
 The "(STEREOLOGY)" objects each represent a "category" and each is tied
 to the "STEREOLOGY.GRID" by their "grid_set" value. As each point is selected
 the user can then use shortcut keys [1]-[9] or click the appropriate buttons
 to select what category each point is in, and it it will advance to the next.
 Each time a category is "assigned" to a point, that point is added to the
 corresponding object... hence the number of points in each object corresponds
 to the point in that category.
 
 By default, each point should be assigned one and ONLY one category,
 however when the user clicks "Finalize Categories" there is an option to "allow
 point to be assigned multiple categories". Another option presented during the 
 during this finalize step is "allow intercepts" whereby, the user can enter a
 "Intercept" mode and draw points wherever their surface of interest intercepts
 test lines. If either of these options are chosen the string "multi_cats" or
 "allow_intercepts" will be included in the label of the "STEREOLOGY.SETTINGS"
 object.
 
 NOTE: By default each object has is points represented with spheres, however it
 is possible to go "Edit > Object > Type" to change sphere size and the
 representation of points in each object/category, but please don't change
 the name!
 
 As for the grid display - the "Grid Setup" tab contains a couple of options
 to change the way the grid is drawn, but many more options can be found under
 "Options > Display Options".
 
*/

//############################################################

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <qvariant.h>
#include <qaction.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qcombobox.h>
#include <QButtonGroup>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>
#include <qclipboard.h>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QWheelEvent>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QScrollArea>
#include <QKeyEvent>
#include <QEvent>
#include <qtoolbutton.h>
#include <QStringList>
#include <QListView>
#include <qfile.h>
#include <QTextStream>
#include <QMenu>
#include <QTime>

#include <qcompleter.h>

#include <qfiledialog.h>
#include <sstream>      // for formatting string output
#include <fstream>      // for input/output of binary files

#include "../../3dmod/pegged.xpm"
#include "../../3dmod/unpegged.xpm"

#include "_common_functions.h"
#include "customdialog.h"
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "stereology.h"


//############################################################

static StereologyData plug = { 0, 0 };

//############################################################



//----------------------------------------------------------------------------
//
//          CLASS FUNCTION DEFINITIONS:
//
//----------------------------------------------------------------------------




//------------------------
//-- Default construtor -- used to set the x,y,z point position ("pos")
//-- and a number of categories (which should all default to off).

Spoint::Spoint( float xVal, float yVal, int zVal, int numCategories )
{
	pos.x = xVal; pos.y = yVal; pos.z = zVal;
	checked = false;
	catSel.resize( numCategories );
}


//------------------------
//-- Resets the stereology point by setting the point position ("pos")
//-- to origin and clearning the vector of categories.

void Spoint::reset()
{
	pos.x = 0; pos.y = 0; pos.z = 0;
	checked = false;
	catSel.clear();
}

//------------------------
//-- Used to determine if a specified category ("catIdx") is on or off.
//-- Returns true if the matching entry in the "catSel" is true, or
//-- false if off. If the give index does not exist, false is returned.

bool Spoint::isCatOn( int catIdx )
{
	if( catIdx < 0 || catIdx >= catSel.size() )
		return false;
	return catSel[catIdx];
}

//------------------------
//-- Used to set a specified category ("catIdx") to off or on to 
//-- match the input value ("turnOn").

void Spoint::setCatOn( int catIdx, bool turnOn )
{
	if( catIdx < 0 || catIdx >= catSel.size() )
		return;
	catSel[catIdx] = turnOn;
}

//------------------------
//-- Returns the total number of values in the "catSel" vector
//-- which are set to true (thus representing the number of
//-- categories applied to this stereology point).

int Spoint::numCatsOn()
{
	int catsOn = 0;
	for (int c=0; c<(int)catSel.size(); c++ )
		if( catSel[c] == true )
			catsOn++;
	return (catsOn);
}


//------------------------
//-- Returns true if the input stereology point (*spt) has the same
//-- checked value, and the "catSel" vector has the same values
//-- turned on or off. Otherwise returns false.
//-- Note that the point positions are not compared.

bool Spoint::hasSameState( Spoint *spt )
{
	for (int c=0; c<(int)catSel.size() && c<(int)spt->catSel.size(); c++ )
		if( catSel[c] != spt->catSel[c] )
			return false;
	
	if( checked != spt->checked )
		return false;
	
	return true;
}






//------------------------
//-- Default constructor

CategoryObj::CategoryObj()
{
	reset();
}

//------------------------
//-- Resets all values in "CategoryObj". Note that the objIdx
//-- value is set to -1 by default and is changed only when
//-- loaded from or saved to a IMOD object.

void CategoryObj::reset()
{
	objIdx       = -1;
	gridSetId    = "";
	objName      = "";
	categoryName = "";
	
	toolTip      = "";
	sphereSize   = -1;
	lineWidth    = 1;
	
	includeInResults = true;
	numPtsOn         = 0;
	changeOpt        = SEL_NA;
	selectOpt        = SEL_OFF;
}

//------------------------
//-- Sets the "categoryName" and also the "nameEntry.name" to the string
//-- specified ("newObjName"). If "newToolTip" is not empty it
//-- will set that too.

void CategoryObj::setNameAndTooltip( QString newCatName, QString newToolTip )
{
	categoryName   = newCatName;
	nameEntry.name = newCatName;
	
	if( !newToolTip.isEmpty() )
		toolTip = newToolTip;
}

//------------------------
//-- Returns true if the category's name ("categoryName") starts
//-- with a number (0-9).

bool CategoryObj::catNameStartsWithNumber()
{
	if( categoryName.length() <= 0 )
		return (false);
	
	return ( categoryName.at(0).isNumber() );	
						// NOTE: I believe "isNumber()" includes '-' and '.' 
}

//------------------------
//-- Returns the value of "numCounts" by multiplying the value
//-- in "numPtsOn" by the number at the start of the category's 
//-- name. For example if the value of "categoryName" is 
//-- "2_INTERSECTIONS" and "numPtsOn" is 50 it  will return 100. 
//-- Note that for any string starting with a non-number 
//-- it will return 0.

long CategoryObj::calcCountUsingName()
{
	if( !catNameStartsWithNumber() || numPtsOn == 0 )
		return 0;
	
	//## DETERMINE THE POSITION OF THE FIRST NON-NUMBER:
	
	int pos = 0;				// position to use
	for(pos=0; pos<categoryName.length(); pos++)
		if( !categoryName.at(pos).isNumber() )
			break;
	
	QString numberStr = categoryName.left(pos).trimmed();
	long multiplier = numberStr.toInt();
	
	return ( multiplier*numPtsOn );
}




//------------------------
//-- Default constructor

GridSetObj::GridSetObj()
{
	reset();
}


//------------------------
//-- Resets all values in "GridSetObj", which includes clearning the
//-- vector of categories ("catObj") and vector of stereology points
//-- ("pts"). Notice that the objIdx value is set to -1 by default
//-- and is changed only when the settings are loaded from or saved
//-- to a IMOD object.

void GridSetObj::reset()
{
	gridSetId = "";
	objIdx    = -1;
	gridType  = GD_CROSSHAIRS;
	
	xSpacing  = 100;
	ySpacing  = 100;
	zSpacing  = 10;
	xMin = xMax = yMin = yMax = zMin = zMax = -1;
	allowMultipleCats = false;
	allowIntercepts   = false;
	
	showSubRects = false;
	subRectsApplied = false;
	rXSpan = 400;
	rYSpan = 400;
	rXGap  = 600;
	rYGap  = 600;
	
	cols = rows = grids = ptsPerGrid = maxPts = numPtsChecked = -1;
	ptsPerRowSetup = ptsPerGridSetup = -1;
	currPtIdx = 0;
	countingStarted  = false;
	countingFinished = false;
	
	totPtsWithCatOn   = 0;
	totCatHits        = 0;
	gridTypeStr.clear();
	
	totCounts         = 0;
}


//------------------------
//-- This function is used to quickly verify that all values
//-- appear valid. This function is called after grid values are loaded
//-- to help catch any obvious errors such as a minimum value being
//-- greater than a maximum value, or any spacing value is <= 0.

bool GridSetObj::verifyVals()
{
	bool problems = objIdx < 0 || gridSetId=="" 
	|| xSpacing<=0.0f   || ySpacing<=0.0f   || zSpacing <=0
	|| xMin<0 || xMax<0 || yMin<0 || yMax<0 || zMin<0 || zMax<0
	|| xMin >= xMax     || yMin >= yMax     || zMin > zMax;
	return ( !problems );
	return true;
}

//------------------------
//-- Returns the current length of the "pts" vector.

long GridSetObj::ptsize()
{
	return (long)pts.size();
}


//------------------------
//-- Used to calculate the number of colums, rows, grids
//-- number of points per grid and total points over all grid.
//-- To calculate these values it uses the grid limits and grid
//-- spacing values.
//-- 
//-- INPUT:   xMax,xMin,xSpacing, yMax,yMin,ySpacing, zMax,zMin,zSpacing
//-- OUTPUTS: cols, rows, grids, ptsPerGrid, maxPts

bool GridSetObj::calcVals()
{
	if( xSpacing==0 || ySpacing == 0 || zSpacing == 0 )
		return false;
	
	cols   = (xMax - xMin) / xSpacing;
	rows   = (yMax - yMin) / ySpacing;
	grids  = ((zMax - zMin) / zSpacing) + 1;
	
	if( gridType == GD_CYCLOIDS || gridType == GD_CYCLOIDS2 || gridType == GD_CYCLOIDS3
		  || gridType == GD_UPDIAGONAL || xAlternatesOverRows() )
		cols -= 1;
	
	if( gridType == GD_CYCLOIDS2 || gridType == GD_CYCLOIDS3 )
		rows -= 1;
	
	ptsPerGrid = cols * rows;
	maxPts  = cols * rows * grids;
	
	if( ptsize() == 0 )
	{
		ptsPerRowSetup  = cols;
		ptsPerGridSetup = ptsPerGrid;
	}
	
	return true;
}


//------------------------
//-- Returns true if "gridType" is to GD_OFFPTS or GD_LINEPAIRS -
//-- the two types where alternate rows are offset in x.

bool GridSetObj::xAlternatesOverRows()
{
	return (gridType == GD_OFFPTS || gridType == GD_LINEPAIRS);
}

//------------------------
//-- Returns true if "gridType" is set to GD_RAND and thus
//-- the placement of points is random.

bool GridSetObj::allowRandomPts()
{
	return (gridType == GD_RAND);
}

//------------------------
//-- Returns true if "gridType" is set to GD_LINES, GD_CROSSHAIRS,
//-- GD_RAND or any other form of lines used to estimate volume 
//-- by point counting stereology.

bool GridSetObj::isVolumeGridType()
{
	return (   gridType == GD_LINES   || gridType == GD_CROSSHAIRS
					|| gridType == GD_ARROWS  || gridType == GD_OFFPTS
					|| gridType == GD_RAND  );
}

//------------------------
//-- Returns true if "gridType" is set to GD_VERTLINES, GD_LINEPAIRS,
//-- GD_CYCLOIDS or any other form of lines used to estimate 
//-- surface area density by counting the number of intersections
//-- along the line.

bool GridSetObj::isSurfaceAreaGridType()
{
	return (gridType == GD_VERTLINES    || gridType == GD_HORZLINES
			 || gridType == GD_UPDIAGONAL   || gridType == GD_DOWNDIAGONAL
			 || gridType == GD_LINEPAIRS    || gridType == GD_CYCLOIDS
			 || gridType == GD_CYCLOIDS2    || gridType == GD_CYCLOIDS3 );
}

//------------------------
//-- Returns true if "gridType" is set to GD_RECTS, GD_FORBIDSQ1
//-- of any other type of grid used to estimate a density count.

bool GridSetObj::isNumberDensityGridType()
{
	return (gridType == GD_RECTS || gridType == GD_FORBIDSQ1 ||
					gridType == GD_FORBIDSQ2 || gridType == GD_FORBIDSQ3 ||
					gridType == GD_OFF );
}

//------------------------
//-- Returns the length, in pixels, of the line extending from each point,
//-- as dictated by the value of "gridType". Note that if the gridType
//-- value is any kind of point, it will return 0.

float GridSetObj::lineLengthPerPtInPixels()
{
	switch (gridType)
	{
		case (GD_VERTLINES):			return ( ySpacing );
		case (GD_HORZLINES):			return ( xSpacing );
		case (GD_UPDIAGONAL):			return ( sqrt((xSpacing*xSpacing) + (ySpacing*ySpacing)) );
		case (GD_DOWNDIAGONAL):		return ( sqrt((xSpacing*xSpacing) + (ySpacing*ySpacing)) );
		case (GD_LINEPAIRS):			return ( xSpacing / 2.0f );
		
		case (GD_CYCLOIDS):
		case (GD_CYCLOIDS2):
		{
			Icont *cont = imodContourNew();
			genContourForPt( 0, cont );						// generate half a cycloid:
			float lengthInPix = imodContourLength( cont, false );
			free(cont);
			
			return (lengthInPix);
		}
		case (GD_CYCLOIDS3):			// http://en.wikipedia.org/wiki/Cycloid#Arc_length
		{
			float r = xSpacing / (PI * 2.0f);			// radius for cycloid
			return (8.0f * r);
		}
		
		case (GD_RECTS):
		{
			return ( xSpacing+ySpacing );
		}
		case (GD_FORBIDSQ1):
		case (GD_FORBIDSQ2):
		case (GD_FORBIDSQ3):
		{
			float sqLength = MIN( xSpacing, ySpacing );
			if( gridType == GD_FORBIDSQ1 )		sqLength /= 2.0f;
			if( gridType == GD_FORBIDSQ2 )		sqLength /= 4.0f;
			if( gridType == GD_FORBIDSQ3 )		sqLength /= 8.0f;
			return (2.0f*(sqLength + sqLength));
		}
	}
	
	return (0.0f);
}


//------------------------
//-- Generates a contour line which extends from the given stereology
//-- point ("ptIdx") and returns it via ("cont"). The type of line
//-- which extends will depend on the value of "gridType". Note that 
//-- any type of grid point (eg: GD_LINES) will return an empty contour.

void GridSetObj::genContourForPt( long ptIdx, Icont *cont )
{
	imodContourDefault( cont );
	
	Ipoint startPt = getPos( ptIdx );
	Ipoint endPt   = startPt;
	Ipoint pt      = startPt;
	
	setOpenFlag( cont, 1 );
	
	bool isStraightLine = true;		// if true, will make the line extend
																//  from "startPt" to "endPt"
	
	switch (gridType)
	{
		case (GD_VERTLINES):
		{
			endPt.y = startPt.y + ySpacing;
			break;
		}
		case (GD_HORZLINES):	
		{
			endPt.x = startPt.x + xSpacing;
			break;
		}
		case (GD_UPDIAGONAL):	
		{
			endPt.x = startPt.x + xSpacing;
			endPt.y = startPt.y + ySpacing;
			break;
		}
		case (GD_DOWNDIAGONAL):	
		{
			endPt.x = startPt.x + xSpacing;
			endPt.y = startPt.y + ySpacing;
			break;
		}
		case (GD_LINEPAIRS):	
		{
			endPt.x = startPt.x + (xSpacing / 2.0f);
			break;
		}
		
		case (GD_CYCLOIDS):
		case (GD_CYCLOIDS2):
		{
			float r = ySpacing / 4.0f;		// radius for cycloid
			float xStretch = 2.0f * fDiv( xSpacing, ySpacing ) / PI;			// ADJUST
			
			int xIdx = (cols > 0) ? ptIdx % cols : 0;
			bool flipY = (gridType==GD_CYCLOIDS2) && (xIdx%2==1);
			float quarterXSpacing = xSpacing / 4.0f;
			
			endPt.x = startPt.x + (0.5f * xSpacing);
			endPt.y = startPt.y + ((flipY) ? -(0.5f * ySpacing) : (0.5f * ySpacing));
			
			for(int i = 0; i <= ARCPOINTS; i++)					// generate half a cycloid:
			{
				float angle = PI * fDiv(i, ARCPOINTS) - (PI*0.5);
				pt.x = startPt.x + ((r * (angle - cos(angle)) ) * xStretch) + quarterXSpacing;
				pt.y = endPt.y   - (r *  (1.0f  - sin(angle)) );
				if( flipY )									// if we want to turn upside down
					pt.y = (endPt.y - pt.y) + endPt.y;
				imodPointAppendXYZ(cont, pt.x, pt.y, pt.z );
			}
			
			isStraightLine = false;
			break;
		}
		case (GD_CYCLOIDS3):			// http://en.wikipedia.org/wiki/Cycloid#Arc_length
		{
			float r = xSpacing / (PI * 2.0f);		// we want a "true" cycloid which
			float xStretch = 1.0f;									// is not distorted by y spacing value
			
			int xIdx = (cols > 0) ? ptIdx % cols : 0;
			bool flipY = (xIdx%2==1);
			float quarterXSpacing = xSpacing / 4.0f;
			
			endPt.y = startPt.y + ((flipY) ? -(2.0*r) : (2.0*r));
			
			for(int i=0; i<=ARCPOINTS*2; i++)					// generate a full cycloid:
			{
				float angle = PI * ( (float)i / (float)ARCPOINTS ) - (PI*0.5);
				
				pt.x = startPt.x + ((r * (angle - cos(angle)) ) * xStretch) + quarterXSpacing;
				pt.y = endPt.y   - (r *  (1.0f  - sin(angle)) );
				if( flipY )									// if we want to turn upside down
					pt.y = (endPt.y - pt.y) + endPt.y;
				imodPointAppendXYZ(cont, pt.x, pt.y, pt.z );
			}
			
			isStraightLine = false;
			break;
		}
		
		case (GD_RECTS):
		{
			endPt.x = startPt.x + (xSpacing / 2.0f);
			endPt.y = startPt.y + (ySpacing / 2.0f);
			
			imodPointAppendXYZ(cont, startPt.x, startPt.y, pt.z );
			imodPointAppendXYZ(cont, endPt.x,   startPt.y, pt.z );
			imodPointAppendXYZ(cont, endPt.x,   endPt.y,   pt.z );
			imodPointAppendXYZ(cont, startPt.x, endPt.y,   pt.z );
			setOpenFlag( cont, 0 );
			
			isStraightLine = false;
			break;
		}
		case (GD_FORBIDSQ1):
		case (GD_FORBIDSQ2):
		case (GD_FORBIDSQ3):
		{
			float sqLength = MIN( xSpacing, ySpacing );
			if( gridType == GD_FORBIDSQ1 )		sqLength /= 2.0f;
			if( gridType == GD_FORBIDSQ2 )		sqLength /= 4.0f;
			if( gridType == GD_FORBIDSQ3 )		sqLength /= 8.0f;
			
			imodPointAppendXYZ(cont, startPt.x, startPt.y, pt.z );
			imodPointAppendXYZ(cont, endPt.x,   startPt.y, pt.z );
			imodPointAppendXYZ(cont, endPt.x,   endPt.y,   pt.z );
			imodPointAppendXYZ(cont, startPt.x, endPt.y,   pt.z );
			setOpenFlag( cont, 0 );
			
			isStraightLine = false;
			break;
		}
	}
	
	if( isStraightLine )
	{
		imodPointAppend(cont, &startPt );
		imodPointAppend(cont, &endPt   );
	}
}



//------------------------
//-- Returns the area, in pixels, of any rectangles or squares extending
//-- from each point as dictated by the value of "gridType". Note that if
//-- the gridType value is any kind of point or line, it will return 0.

float GridSetObj::areaPerPtInPixels()
{
	switch (gridType)
	{
		case (GD_RECTS):
		{
			return ( xSpacing * ySpacing );
		}
		case (GD_FORBIDSQ1):
		case (GD_FORBIDSQ2):
		case (GD_FORBIDSQ3):
		{
			float sqLength = MIN( xSpacing, ySpacing );
			if( gridType == GD_FORBIDSQ1 )		sqLength /= 2.0f;
			if( gridType == GD_FORBIDSQ2 )		sqLength /= 4.0f;
			if( gridType == GD_FORBIDSQ3 )		sqLength /= 8.0f;
			return (sqLength * sqLength);
		}
	}
	
	return (0.0f);
}



//------------------------
//-- Returns the number of categories starting with numbers, and thus
//-- likely to represent "intersection" values... eg: "2_INTERSECTIONS"

int GridSetObj::numCatsStartingWithNumbers()
{
	int numCatsWithNumbersFront = 0;
	for(int i=0; i<(int)catObj.size(); i++)
		if( catObj[i].catNameStartsWithNumber() )
			numCatsWithNumbersFront++;
	return (numCatsWithNumbersFront);
}


//------------------------
//-- Returns true if the given point/position ("x","y","z") is inside 
//-- any 'subsample rectangle' or false if not - regardless of the 
//-- value of "showSubRects".
//-- The subsample rectangles start at the top corner ("xMin","yMax")
//-- of the grid limits and are sized and positioned according to the :
//-- values "rXSpan", "rYSpan", "rXGap" and "rYGap".
//-- If "includeLine" is on, points on any line (top, bottom, left or right)
//-- will be included as in.
//-- If "ignoreZ" is on, it will ignore the z value of the point
//-- and assume it falls on a slice containing a grid

bool GridSetObj::isPtInSubRect( float x, float y, int z,
																	  bool includeLine, bool ignoreZ )
{
	if( !ignoreZ && !isGridOnSlice(z) )
		return false;
	
	float rXSpacing = rXSpan + rXGap;
	float rYSpacing = rYSpan + rYGap;
	
	if( !includeLine )
	{
		float xRemain = fMod((x - xMin), rXSpacing);
		if( xRemain == 0 || xRemain >= rXSpan )
			return false;
		float yRemain = fMod((yMax - y), rYSpacing);
		if( yRemain == 0 || yRemain >= rYSpan )
			return false;
		return true;
	}
	
	return ( fMod((x - xMin), rXSpacing) <= rXSpan ) && 
	       ( fMod((yMax - y), rYSpacing) <= rYSpan );
}


//------------------------
//-- Returns true if the given stereology point ("x","y","z") and
//-- and structure (like a line or square) extending from it is completely
//-- inside the 'subsample rectangles' or false if not - regardless of the 
//-- value of "showSubRects".
//-- 
//-- The type of structure which may extend is determined by the vale
//-- of "gridLine". For example if set to "GD_LINEPAIRS", then 
//-- a line of appropriate length will be projected to the right
//-- of the specified point.
//-- 
//-- If "ignoreZ" is on, it will ignore the z value of the point
//-- and assume it falls on a slice containing a grid

bool GridSetObj::isSPtContainedInsideSubRect( float x, float y, int z, bool ignoreZ )
{
	if( !ignoreZ && !isGridOnSlice(z) )
		return false;
	
	//## IF GIVEN POINT IS OUTSIDE FORBIDDEN SQUARES:
	//## RETURN FALSE (REGARDLESS OF "gridType"):
	
	bool firstPtInside = isPtInSubRect(x,y,z,false,true);
	
	if( !firstPtInside )						// if first point is outside forbidden square:
		return false;										// return false
	
	float xEnd = x;
	float yEnd = y;
	
	//## FOR SELECT GRID TYPES CHECK IF A SECOND POINT IS INSIDE 
	//## FORBIDDEN SQUARES AND RETURN FALSE IF OUTSIDE:
	
	switch( gridType )
	{
		case( GD_UPDIAGONAL   ):
		case( GD_DOWNDIAGONAL ):
		{
			xEnd = x + (gridType == GD_DOWNDIAGONAL) ? -xSpacing : xSpacing;
			yEnd = y + ySpacing;
			return (isPtInSubRect(xEnd,yEnd,z,false,true));
		}
		
		case( GD_LINEPAIRS   ):
		{
			xEnd = x + (xSpacing * 0.5f);
			return (isPtInSubRect(xEnd,yEnd,z,false,true));
		}
		
		case( GD_CYCLOIDS ):
		case( GD_CYCLOIDS3 ):
		case( GD_RECTS ):
		{
			xEnd = x + (xSpacing * 0.5f);
			yEnd = y + (ySpacing * 0.5f);
			return (isPtInSubRect(xEnd,yEnd,z,false,true));
		}
		
		case( GD_CYCLOIDS2 ):
		{
			int xIdx = fDiv( (x - xMin), xSpacing );
			xEnd = x + (xSpacing * 0.5f);
			yEnd = y + (ySpacing * 0.5f);
			if( xIdx%2==0 )													// for every second column
				yEnd = y - (0.5f * ySpacing);
			return (isPtInSubRect(xEnd,yEnd,z,false,true));
		}
		
		case( GD_FORBIDSQ1 ):
		case( GD_FORBIDSQ2 ):
		case( GD_FORBIDSQ3 ):
		{
			float sqLength = MIN( xSpacing, ySpacing );
			if( gridType == GD_FORBIDSQ1 )		sqLength /= 2.0f;
			if( gridType == GD_FORBIDSQ2 )		sqLength /= 4.0f;
			if( gridType == GD_FORBIDSQ3 )		sqLength /= 8.0f;
			xEnd = x + sqLength;
			yEnd = y + sqLength;
			return (isPtInSubRect(xEnd,yEnd,z,false,true));
		}
	}
	
	return true;
}



//------------------------
//-- Calculates an Ipoint corresponding to the position
//-- in the grid, where a stereology point at the given 
//-- index ("ptIdx") in the "pts" vector should appear.
//-- Note that the "pts" vector does not need to be setup, 
//-- and the ptIdx is not checked to see if it's between
//-- 0 and maxPts-1.

Ipoint GridSetObj::calcPtPosAtIdx( long ptIdx )
{
	Ipoint pt;
	
	if( ptsPerGrid == 0 || cols == 0 )
		return (pt);
	
	int ptSlice  = ptIdx / ptsPerGrid;
	int ptRow    = (ptIdx - (ptsPerGrid*ptSlice) ) / cols;
	int ptCol    = ptIdx - (ptsPerGrid*ptSlice) - (cols*ptRow);
	
	pt.x =  ((ptCol+1) * xSpacing) + xMin;
	pt.y = -((ptRow+1) * ySpacing) + yMax;
	pt.z = (ptSlice * zSpacing) + zMin;
	
	if( gridType == GD_LINEPAIRS     && ptRow%2==1 )
		pt.x += (xSpacing / 4.0f);
	if( gridType == GD_OFFPTS     && ptRow%2==1 )
		pt.x += (xSpacing / 2.0f);
	
	return (pt);
}


//------------------------
//-- Returns a pointer to the stereology point (Spoint) at
//-- the given index ("ptIdx") in the "pts" vector. If
//-- the index does not exist, it returns NULL (to help 
//-- prevent out-of-bounds error).

Spoint *GridSetObj::getSPt( long ptIdx )
{
	if( ptIdx < 0 || ptIdx >= (long)pts.size() )
		return NULL;
	
	return &pts[ptIdx];
}

//------------------------
//-- Returns the (Ipoint) position of the stereology point at the
//-- given index ("ptIdx") in the "pts" vector. If this stereology
//-- point does not exist (possibly because the "pts" vector is not
//-- yet setup), it will instead call "calcPtPosAtIdx" to show where
//-- this point *should* be.

Ipoint GridSetObj::getPos( long ptIdx )
{
	if( ptIdx < 0 || ptIdx >= (long)pts.size() )
		return calcPtPosAtIdx(ptIdx);
	else
		return pts[ptIdx].pos;
}


//------------------------
//-- Takes in a row ("yIdx"), column ("y") and grid ("zIdx") index
//-- and returns a long representing the index within the "pts"
//-- vector where this point should exist.

long GridSetObj::getPtIdx( int xIdx, int yIdx, int zIdx )
{
	long ptIdx = (zIdx*ptsPerGrid) + (yIdx*cols) + xIdx;
	return ptIdx;
}

//------------------------
//-- Takes in a row ("yIdx"), column ("y") and grid ("zIdx") index
//-- and returns a pointer to the matching stereology pint (Spoint).
//-- If the point matching these index numbers does no exist
//-- NULL is returned.

Spoint *GridSetObj::getSPtByIdx( int xIdx, int yIdx, int zIdx )
{
	long ptIdx = (zIdx*ptsPerGrid) + (yIdx*cols) + xIdx;
	if(ptIdx < 0 || ptIdx >= (long)pts.size() )
		return NULL;
	
	return &pts[ptIdx];
}

//------------------------
//-- Inputs a point position ("pt") and tries to return a pointer 
//-- to the stereology point within the "pts" vector which matches
//-- this location. If no matching point is found, NULL is returned.
//-- 
//-- To speed up performance, this algorithm is smart enough to predict
//-- where the point should be within the vector and will check
//-- that first. If it can't find the point at the predicted 
//-- location and "checkAllPts" is true and it will search
//-- all points in the "pts" array. This exhaustive search is a bit
//-- slower, but is necessary if cases where "allowRandomPts()" is true.

Spoint *GridSetObj::getSPtByPt( Ipoint *pt, bool checkAllPts )
{
	//## DETERMINE WHERE POINT SHOULD BE (USING GRID SETTINGS) AND CHECK IF IT'S THERE:
	
	int xIdx = (int)fDiv(  (pt->x - xMin), xSpacing) - 1;
	int yIdx = (int)fDiv( -(pt->y - yMax), ySpacing) - 1;
	int zIdx = (int)fDiv(  (pt->z - zMin), zSpacing);
	
	Spoint *sptFirstGuess = getSPtByIdx(xIdx,yIdx,zIdx);
	
	if( sptFirstGuess != NULL
		 && ptsEqualXYZ( &sptFirstGuess->pos,pt,PT_PREC ) )	// if point matches: return it
	{
		return sptFirstGuess;												
	}
	
	
	//## TAKE A SECOND GUESS AS THE PREV POINT IN ORDER
	//## (USEFUL IF "gridType" = GD_LINEPAIR):
	
	Spoint *sptSecondGuess = getSPtByIdx(xIdx-1,yIdx,zIdx);
	if( sptSecondGuess != NULL
		 && ptsEqualXYZ( &sptSecondGuess->pos,pt,PT_PREC ) )	// if point matches: return it
	{
		return sptSecondGuess;
	}
	
	//## TAKE A THIRD GUESS AS THE NEXT POINT IN ORDER:
	
	Spoint *sptThirdGuess = getSPtByIdx(xIdx+1,yIdx,zIdx);
	if( sptThirdGuess != NULL
		 && ptsEqualXYZ( &sptThirdGuess->pos,pt,PT_PREC ) )	// if point matches: return it
	{
		return sptThirdGuess;
	}
	
	//## IF SPECIFIED: CHECK ALL POINTS FOR A MATCH
	
	if( checkAllPts )
	{
		for( long p=0; p<(long)pts.size(); p++ )
		{
			if( ptsEqualXYZ( &pts[p].pos,pt,PT_PREC ) )		// if match found: return it
				return &pts[p];
		}
	}
	
	return NULL;				// no match was found: return null
}

//------------------------
//-- Returns a pointer to the currently selected stereology point
//-- within the "pts" vector (as dictated by "currPtIdx").
//-- If "currPtIdx" doesn't index a real point it set to 0 
//-- and if the "pts" vector is empty NULL is returned to avoid
//-- an out of boundes error.

Spoint *GridSetObj::getCurrSPt()
{
	if( currPtIdx < 0 || currPtIdx >= pts.size() )
	{
		currPtIdx = 0;
		if( pts.size() == 0)
			return NULL;
	}
	
	return &pts[currPtIdx];
}




//------------------------
//-- Returns true if the given z value/section index ("sliceIdx")
//-- falls on a section which has a stereology grid as dictated
//-- by the values of "zMin", "zMax" and "zSpacing".
//-- Returns false if there is no grid on this section.

bool GridSetObj::isGridOnSlice( int sliceIdx )
{
	if( sliceIdx<zMin || sliceIdx>zMax )
		return false;
	
	return ((sliceIdx-zMin) % zSpacing)==0;
}


//------------------------
//-- Returns a vector of index positions, representing stereology points
//-- inside the "pts" vector which are within the circle described by
//-- the given "center" and "radius". Note that only stereology points
//-- on the same slice (given "center->z") are considered and if no points
//-- are inside the circle and empty vector will be returned.
//-- 
//-- If "allowRandomPts()" is false, this search is quite efficient,
//-- since the pts are setup in a uniform grid and a bounding box
//-- can be applied....... but if "allowRandomPts" is false, then all
//-- points must be checked exhaustively.

vector<long> GridSetObj::getIdxPtsInRadius( Ipoint *center, float radius )
{
	int zVal = (int)center->z;
	vector<long> ptIdxInRad;
	float radSq = radius*radius;
	
	if( !isGridOnSlice(zVal) || zSpacing==0 )
		return (ptIdxInRad);
	
	if( allowRandomPts() || subRectsApplied )
	{
		long psize   = (long)pts.size();
		for( long p=0; p<psize; p++ )
		{
			if( (int)pts[p].pos.z != zVal )
				continue;
			float distSq = line_sqDistBetweenPts2D( center, &pts[p].pos );
			if( distSq <= radSq )
				ptIdxInRad.push_back(p);
		}
	}
	else
	{
		int z = (zVal - zMin) / zSpacing;
		
		int yIdxMin = MAX( (int)ceil ( (yMax - (center->y + radius)) / ySpacing )-1, 0);
		int yIdxMax = MIN( (int)floor( (yMax - (center->y - radius)) / ySpacing )-1, rows-1);
		
		int xIdxMin = MAX( (int)ceil ( ((center->x - radius) - xMin) / xSpacing )-1, 0);
		int xIdxMax = MIN( (int)floor( ((center->x + radius) - xMin) / xSpacing )-1, cols-1);
		
		for(int y=yIdxMin; y<=yIdxMax; y++)								// for each row (from top):
		for(int x=xIdxMin; x<=xIdxMax; x++)								// for each column:
		{
			long ptIdx    = getPtIdx(x,y,z);
                        Ipoint tmpPt = getPos(ptIdx);
			float distSq = line_sqDistBetweenPts2D( center, &tmpPt );
			if( distSq <= radSq )
				ptIdxInRad.push_back(ptIdx);
		}
	}
	
	return (ptIdxInRad);
}


//------------------------
//-- Used to check if there are points inside the given circle, specified by
//-- "center" and "radius". If so, it will return true, and the index of the
//-- closest stereology point (within the "pts" vector) to the circles center
//-- is output via "closestPtIdx". If there are no points within the circle
//-- it returns false and "closetPtIdx" is set to -1.
//-- 
//-- This function is very similar in form to "getIdxPtsInRadius", except
//-- that it only returns one pt index (the pt closest to the middle) rather 
//-- than the index of all points within the circle.

bool GridSetObj::isPtInRadius( Ipoint *center, float radius, long *closestPtIdx )
{
	int zVal  = (int)center->z;
	bool ptFound = false;
	float minDistSq = radius*radius;
	*closestPtIdx   = -1;
	
	if( !isGridOnSlice(zVal) || zSpacing==0 )
		return (false);
	
	if( allowRandomPts() || subRectsApplied )
	{
		long psize   = (long)pts.size();
		for( long p=0; p<psize; p++ )
		{
			if( (int)pts[p].pos.z != zVal )
				continue;
			float distSq = line_sqDistBetweenPts2D( center, &pts[p].pos );
			if( distSq <= minDistSq )
			{
				minDistSq = distSq;
				*closestPtIdx = p;
				ptFound = true;
			}
		}
	}
	else
	{
		int z = (zVal - zMin) / zSpacing;
		
		int yIdxMin = MAX( (int)ceil ( (yMax - (center->y + radius)) / ySpacing )-1, 0);
		int yIdxMax = MIN( (int)floor( (yMax - (center->y - radius)) / ySpacing )-1, rows-1);
		
		int xIdxMin = MAX( (int)ceil ( ((center->x - radius) - xMin) / xSpacing )-1, 0);
		int xIdxMax = MIN( (int)floor( ((center->x + radius) - xMin) / xSpacing )-1, cols-1);
		
		if( xAlternatesOverRows() )
		{
			xIdxMax = MAX( xIdxMax+1, 0     );
			xIdxMin = MIN( xIdxMin-1, cols-1);
		}
		
		for(int y=yIdxMin; y<=yIdxMax; y++)								// for each row (from top):
		for(int x=xIdxMin; x<=xIdxMax; x++)								// for each column:
		{
			long ptIdx   = getPtIdx(x,y,z);
                        Ipoint tmpPt = getPos(ptIdx);
			float distSq = line_sqDistBetweenPts2D( center, &tmpPt );
			if( distSq <= minDistSq )
			{
				minDistSq = distSq;
				*closestPtIdx = ptIdx;
				ptFound = true;
			}
		}
	}
	
	return (ptFound);
}


//------------------------
//-- Used to find an point near the given point ("pt") which intersects
//-- a line segment extending from one of the stereology points.
//-- If no such point is "intercept point" is found within the specified
//-- distance ("maxDist"), it returns false. If a intercept point is found
//-- within range, it returns true and returns the position of the intercept
//-- point via "ptIntercept".

bool GridSetObj::getInterceptNearPt( Ipoint pt, Ipoint *ptIntercept, float maxDist )
{
	float radius = maxDist + MAX(ySpacing, xSpacing);
	vector<long> ptsInRange = getIdxPtsInRadius( &pt, radius );
	
	float minDistSq = FLOAT_MAX;
	
	Icont *cont = imodContourNew();
	
	for(long i=0; i<(long)ptsInRange.size(); i++)
	{
		genContourForPt( ptsInRange[i], cont );
		cont_addPtsCrude( cont, INTERCEPT_DIST, false );
		
		for(int p=0; p<(int)psize(cont); p++)
		{
			float distSq = line_sqDistBetweenPts2D( &pt, getPt(cont,p) );
			if( distSq < minDistSq )
			{
				minDistSq = distSq;
				copyPt( ptIntercept, getPt(cont,p) );
			}
		}
	}
	
	imodContourDelete( cont );
	//free(cont);
	
	float dist = sqrt(minDistSq);
	
	return ( dist <= maxDist );
}

//------------------------
//-- Is used to initialize the "pts" vector by first calling "calcValues"
//-- and (assuming the values are valid), populating "pts" with a total
//-- of "maxPts" stereology points, ordered along x (left to right) then
//-- y (top to bottom) and then z (lowest to highest grid) within
//-- the grid limits specified.
//-- 
//-- By default, all stereology points will have their "checked value"
//-- set to false, and will setup with their "catSel" vector
//-- the same size as the grids "catObj" vector (it's important these
//-- two match) with all "catSel" values false.
//-- 
//-- Note that if "allowRandomPts()" is true, the pts vector is cleared
//-- only but no points added.
//-- 
//-- If the input "showSubRectsIfOn" is on and "showSubRects" is on
//-- then any points/lines/squares (depending on "gridType") will be ommitted.
//-- 
//-- Returns true if the grid is setup (as expected, or false if "calcVals"
//-- call fails - suggesting that the grid contains invalid values.

bool GridSetObj::setupPts( bool applySubRectsIfOn )
{
	if (!calcVals())
		return false;
	
	int numCats = catObj.size();
	
	pts.clear();
	
	subRectsApplied = ( applySubRectsIfOn && showSubRects );
	
	if( gridType == GD_RAND )
		return true;
	
	//## DETERMINE IF AND HOW MUCH ALTERNATE ROWS SHOULD BE OFFSET IN X:
	
	bool offsetEverySecondRow = xAlternatesOverRows();
	float xOffsetSecondRow;
	if( gridType == GD_OFFPTS )
		xOffsetSecondRow = xSpacing / 2.0f;
	if( gridType == GD_LINEPAIRS )
		xOffsetSecondRow = xSpacing / 4.0f;
	
	
	//## FOR EACH POINT IN GRID: ADD TO THE "pts" ARRAY (IF APPLICABLE):
	
	for(int zVal=zMin; zVal<=zMax; zVal+=zSpacing)		// for each slice:
	for(int y=0; y<rows; y++)													// for each row (from top):
	for(int x=0; x<cols; x++)													// for each column:
	{
		float xVal =  ((float)(x+1) * xSpacing) + (float)xMin;
		float yVal = -((float)(y+1) * ySpacing) + (float)yMax;
		
		if( offsetEverySecondRow && y%2==1)
			xVal += xOffsetSecondRow;
		
		//## IF FORBIDDEN SQUARES APPLIED: CHECK IF THE POINT/LINE/SHAPE
		//## IS INSIDE THE SQUARES AND DON'T ADD IF IT GOES OUTSIDE:
		
		if( subRectsApplied && !isSPtContainedInsideSubRect(xVal,yVal,zVal,true) )
			continue;
		
		pts.push_back( Spoint(xVal,yVal,zVal,numCats) );		// add point to vector
	}
	
	
	//## IF FORBIDDEN SQUARES WERE APPLIED, CALCULATE THE VALUES
	//## OF "ptsPerRowSetup" and "ptsPerGridSetup" (FOR NAVIGATION):
	
	if( subRectsApplied && grids > 0 && ptsize() > 0 )
	{
		ptsPerGridSetup = pts.size() / grids;
		ptsPerRowSetup  = sqrt( (double)ptsize() );
		float yFirstPt  = pts[0].pos.y;
		for( long p=1; p<(long)pts.size(); p++ )
			if( pts[p].pos.y != yFirstPt )
			{
				ptsPerRowSetup = p;
				break;
			}
	}
	
	return true;
}

//------------------------
//-- Returns true if the size of the "pts" vector is equal to the 
//-- value in "maxPts" - which indicate (although not for certain)
//-- that grid points are setup according to current settings.
//-- To be sure this is correct you would call "calcVals" first.

bool GridSetObj::areMaxPtsSetup()
{
	return ( pts.size() > 0 && (pts.size() == maxPts || subRectsApplied ) );
}

//------------------------
//-- Returns the object index ("objIdx") of the category at the
//-- given position ("catIdx") within the "catObj" vector.
//-- If the "catIdx" doex not exist, it returns -2. By default,
//-- "objIdx" values are set to -1 unless loaded from or saved to
//-- an object in the IMOD model - in which case the "objIdx"
//-- should refer to this object's index.

int GridSetObj::getCatObjIdx( int catIdx )
{
	if( catIdx < 0 || catIdx >= catObj.size() )
	{
		cerr << "ERROR: getCatObjIdx() - catIdx " << catIdx << " not exists" << endl;
		return -2;
	}
	return ( catObj[catIdx].objIdx );
}



//------------------------
//-- Used to check if a point, at index "ptIdx", exists in the "pts"
//-- vector and contains a category matching the index "catIdx" in the points
//-- "catSel" vector. If true, it returns false, else returns false and
//-- will also output an error message to the console if "printError" is true.

bool GridSetObj::doesPtCatExists( long ptIdx, int catIdx, bool printError )
{
	if( ptIdx < 0 || catIdx < 0 )						// check ptIdx and catIdx are positive
	{
		if(printError)
			cerr << "ERROR: doesPtCatExists() - ptIdx or catIdx are < 0" << endl;
		return false;
	}
	if( ptIdx >= pts.size() )								// check ptIdx valid
	{
		if(printError)
			cerr << "ERROR: doesPtCatExists() - point does not exist" << endl;
		return false;
	}
	if( catIdx >= catObj.size() )						// check catIdx valid
	{
		if(printError)
			cerr << "ERROR: doesPtCatExists() - category does not exist" << endl;
		return false;
	}
	if( catIdx >= pts[ptIdx].catSel.size() )		// check catIdx existing in pts vector
	{
		if(printError)
			cerr << "ERROR: doesPtCatExists() - no matching category in this pt" << endl;
		return false;
	}
	return true;
}


//------------------------
//-- Used to set the value of "currPtIdx" to match the input value ("ptPtIdx").
//-- If "newPtIdx" is outside the range of points expected then it will be 
//-- automatically set to a valid value by either:
//--
//--   (A) if "wrap" is false: "snapping" it to the nearest valid point index 
//--                           (eg: 0 or maxPtx-1).
//--          --OR--
//--
//--   (B) if "wrap" is true: allowing it to wrap around to the other end
//--                          (such that a -1 value would be set to the last pt).

bool GridSetObj::setCurrPt( long newPtIdx, bool wrap )
{
	long psize = (countingStarted) ? (long)pts.size() : maxPts;
		
	if(wrap)
	{
		if( newPtIdx < 0 && currPtIdx != 0 )
			newPtIdx = 0;
		
		currPtIdx = intMod( newPtIdx, psize );
	}
	else
	{
		if( newPtIdx >= psize ) newPtIdx = psize-1;
		if( newPtIdx < 0 )			newPtIdx = 0;
		currPtIdx = newPtIdx;
	}
	
	return ( currPtIdx >= 0 && currPtIdx < psize );
}

//------------------------
//-- Used to change "currPtIdx" by a given offset ("changeAmount") while
//-- making sure "currPtIdx" stays valid by "wrapping around" or "snapping"
//-- to the nearest valid value if "wrap" is set to true or false, respectively.

bool GridSetObj::changeCurrPt( long changeAmount, bool wrap )
{
	return setCurrPt( currPtIdx + changeAmount, wrap );
}

//------------------------
//-- Returns a random (Ipoint) position within the limits of the grid.
//-- The input values "useSpacingX", "useSpacingY" and "useSpacingZ" can be
//-- set to true to ensure the value generated is snapped to gridlines
//-- along the respective axes. Note that if these are all false, the randomly
//-- generate point could be on any slice and any floating point X,Y values within
//-- the limits of the 3D bounding rectangle (xMin,xMax, yMin,yMax, zMin,yMax)
//-- and thus highly unlikely to fall on an intersection between gridlines.

Ipoint GridSetObj::genRandomPt( bool useSpacingX, bool useSpacingY, bool useSpacingZ )
{
	static bool randomSeedSet = false;
	if( randomSeedSet == false) {
		seedRandom();
		randomSeedSet = true;
	}
	Ipoint randPt;
	
	if(useSpacingX) randPt.x = (randIntInclusive(0,cols-1) * xSpacing) + xMin;
	else            randPt.x = randFlt( xMin, xMax );
	
	if(useSpacingY) randPt.y = -(randIntInclusive(0,rows-1) * ySpacing) + yMax;
	else            randPt.y = randFlt( yMin, yMax );
	
	if(useSpacingZ) randPt.z = (float)((randIntInclusive(0,grids-1) * zSpacing) + zMin);
	else            randPt.z = (float)randIntInclusive( zMin, zMax );
	
	return (randPt);
}


//------------------------
//-- Adds a specified number of points ("numRandPts") to the "pts" vector.
//-- The input values "useSpacingX", "useSpacingY" and "useSpacingZ" can be
//-- set to true to ensure the value generated is snapped to gridlines
//-- along the respective axes.
//-- 
//-- If "minDistFromOtherPts" is >= 0, then each new point added must be
//-- checked against EVERY other point (which can get nasty slow) to check
//-- that it's not within this distance (inclusive) of another point on the
//-- same slice. If a clashes are found, the algorithm will retry a total
//-- of "totalRetries" times to place points to meet this criterion.
//-- 
//-- The function returns the number of points added, which will typically
//-- be equal to "numRandPts", but less if too many clashes occur.
//-- 
//-- Finally, if "biasForSameGridAsLastRandPt" is between 0 and 1, each point  
//-- has a "biasForSameGridAsLastRandPt" chance that its Z value is changed
//-- to fall on the same slice as the last stereology point in the "pts" array
//-- (if it exists). This can help decrease the amount points "jump" between
//-- sections.... but if in doubt, leave this value as 0 - because if only a few
//-- points are generated it will make for an uneven distribution between slices.

long GridSetObj::addRandomPts( long numRandPts,
															 bool useSpacingX, bool useSpacingY, bool useSpacingZ,
															 float minDistFromOtherPts, long totalRetries,
															 float biasForSameZAsLastRandPt )
{
	float sqMinDist  = minDistFromOtherPts * minDistFromOtherPts;
	int numCats = catObj.size();
	
	long numRetries   = 0;		// used to track the number of "retries" when
														//  "clashing" points are found.
	long numPtsAdded  = 0;		// tallys the number of new (random) points added.
	
	
	for(long i=0; numPtsAdded<numRandPts && i<(numRandPts+totalRetries); i++)
	{
		//## GENERATE A NEW RANDOMLY GENERATED POSITION AND,
		//## IF DESIRED, BIAS THE Z VALUE OF THIS POSITION 
		//## TO THE LAST POINT IN THE "pts" VECTOR
		
		Ipoint randPt = genRandomPt(useSpacingX, useSpacingY, useSpacingZ);
		
		if( biasForSameZAsLastRandPt > 0.0f && pts.size() > 0 )
		{
			float randCooef = randFlt( 0.0f, 1.0f );
			if(randCooef <= biasForSameZAsLastRandPt)
				randPt.z = pts[ pts.size()-1 ].pos.z;
		}
		
		//## CHECK IF THIS RANDOMLY GENERATED POSITION IS WITHIN
		//## "minDistFromOtherPts" OF ANY OTHER POINT AND THUS
		//## REPRESENTS A "CLASH":
		
		bool clashFound = false;
		if( minDistFromOtherPts >= 0.0f )
		{
			for(long p=0; p<(long)pts.size(); p++)
			{
				if( pts[p].pos.z == randPt.z &&
					 line_sqDistBetweenPts2D( &randPt, &pts[p].pos ) <= sqMinDist)
				{
					clashFound = true;
					break;
				}
			}
		}
		
		//## IF NO CLASHES WERE FOUND: ADD THIS RANDOM
		//## POINT TO THE "pts" VECTOR:
		
		if( !clashFound )
		{
			pts.push_back( Spoint(randPt.x, randPt.y, randPt.z, numCats) );
			numPtsAdded++;
		}
	}
	
	return (numPtsAdded);
}


//------------------------
//-- Iterates through all points in the "pts" array and returns the number
//-- of points which have "checked" set as true, plus updates the value of
//-- "countingFinished" to true if counting has started (represented by
//-- "countingStarted") and all points appear checked. If not "countingFinished"
//-- is set to false.

long GridSetObj::countCheckedPts( bool earlyExit )
{
	numPtsChecked = 0;
	
	long psize   = (long)pts.size();
	for( long p=0; p<psize; p++ )
	{
		if( pts[p].checked == true ) {
			numPtsChecked++;
		}
		else if (earlyExit) {
			break;
		}
	}
	
	countingFinished = countingStarted && (numPtsChecked == psize);
	
	return (numPtsChecked);
}


//------------------------
//-- Used to advance "currPtIdx" to an unchecked point **.
//-- This function is quite versatile in that it can start
//-- searching either forwards (if "backwards" is left false)
//-- or backwards (if "backwards is true). If "startAtCurrPt"
//-- is true it will select the next or previous unchecked point
//-- just after/before the currently selected point..... or
//-- else if "startAtCurrPt" is false, it will find the very 
//-- first or very last unchecked point in the "pts" vector
//-- (dending on the value of "backwards").
//-- 
//-- Returns true if an unchecked point is found (and selected)
//-- or false if there are no unchecked points (and "currPtIdx
//-- was not changed).

bool GridSetObj::selectFirstUncheckedPt( bool startAtCurrPt, bool backwards )
{
	long psize   = (long)pts.size();
	
	if(!backwards)
	{
		for( long p=0; p<psize; p++ )
		{
			long ptIdx = (startAtCurrPt) ? (p+currPtIdx+1)%psize : p;
			if( pts[ptIdx].checked == false )
			{
				currPtIdx = ptIdx;
				return true;
			}	
		}
	}
	else
	{
		for( long p=psize-1; p>=0; p-- )
		{
			long ptIdx = (startAtCurrPt) ? (p+currPtIdx)%psize : p;
			if( pts[ptIdx].checked == false )
			{
				currPtIdx = ptIdx;
				return true;
			}
		}
	}
	
	return (false);
}


//------------------------
//-- Returns an integer representing the index of the grid
//-- which the currently selected point ("currSelIdx") falls on.
//-- A return value of 0 would suggest the point is on the very
//-- first grid, thus would have a z value equal to zMin.

int GridSetObj::currGrid()
{
	if( pts.size() > 0 && (allowRandomPts() || subRectsApplied) )
	{
		int zVal = getPos( currPtIdx ).z;
		if( isGridOnSlice(zVal) && zSpacing > 0 )
			return ( (zVal-zMin)/zSpacing );
		else {
			return -1;
		}
	}
	
	return ( (ptsPerGrid == 0) ? 0 : currPtIdx / ptsPerGrid );
}


//------------------------
//-- Used to validate whether all point values are valid. A point
//-- is deemed invalid if:
//-- 
//--   (A) it's a checked point with no categories on (easy to do)  OR
//--   (B) it's a point with >1 category on, in a grid
//--       where "allowMultipleCats" is off
//-- 
//-- Returns true if all points are valid, or false is any points
//-- are invalid. If points are invalid, it also returns a string
//-- representing all bad points via "badPtsStr", the index of the
//-- very first bad point via "firstBadPtIdx", and the number of points
//-- fitting (A) and (B) into "numCheckedPtsNoCat" and "numPtsMultiCat"
//-- respectively

bool GridSetObj::validatePtValues( QString *badPtsStr, long *firstBadPtIdx,
																   int *numCheckedPtsNoCat, int *numPtsMultiCat )
{
	*firstBadPtIdx = -1;
	int totCheckedPtsNoCat = 0;
	int totPtsMultiCat = 0;
	
	QString errStr = "";			// used to keep a list of bad points
	
	int  csize = (int)catObj.size();
	long psize = (long)pts.size();
	
	
	for( long p=0; p<psize; p++ )
	{
		Spoint *spt = &pts[p];
		int numCatsOnForPt = 0;		// number of categories applied to this point
		bool isProblem = false;
		
		for( int c=0; c<spt->catSel.size() && c<csize; c++ )
			if( spt->catSel[c] == true )
				numCatsOnForPt++;
		
		if( !allowMultipleCats && numCatsOnForPt > 1 )
		{
			errStr += "> pt " + QStr(p+1) + "\t- multiple categories \n";
			isProblem = true;
			totPtsMultiCat++;
		}
		
		if( spt->checked == true && numCatsOnForPt == 0 )
		{
			errStr += "> pt " + QStr(p+1) + "\t- is checked, but not assigned a category\n";
			isProblem = true;
			totCheckedPtsNoCat++;
		}
		
		if(isProblem && *firstBadPtIdx==-1)
			*firstBadPtIdx = p;
	}
	
	if( badPtsStr!=NULL )	          *badPtsStr = errStr;
	if( numCheckedPtsNoCat!=NULL )	*numCheckedPtsNoCat = totCheckedPtsNoCat;
	if( numPtsMultiCat!=NULL )			*numPtsMultiCat = totPtsMultiCat;
	
	return ( totPtsMultiCat == 0 && totCheckedPtsNoCat == 0 );
}

//------------------------
//-- Returns true if all point values are valid, or false if invalid points
//--- (checked points with no category or point with >1 category without
//-- "allowMultiCats" on). To do this it calls "validatePtValues()"
//-- 

bool GridSetObj::validateAllPtValues()
{
	QString badPtsStr      = "";
	long firstBadPtIdx     = 0;
	int numCheckedPtsNoCat = 0;
	int numPtsMultiCat     = 0;
	
	return ( validatePtValues(&badPtsStr, &firstBadPtIdx,
														&numCheckedPtsNoCat, &numPtsMultiCat ) );
}

//------------------------
//-- Calculates stereology results based on the values of points within
//-- the "pts" vector. If "includeAllCats" input is true, then all categores
//-- will be tallied towards results, but if off, then only categories
//-- (in the "catObj" vector) with their "includeInResults" turned on
//-- will be tallied. If "onlyIncludeChecked" input is true, points which
//-- are not checked will be omitted from the tallies... and any point
//-- with a z value of less than "minZVal" or greater than "maxZVal" is
//-- omitted too.
//-- 
//-- From all candidate points, this function sets and returns the total
//-- number of points with one or more categories turned on 
//-- ("totPtsWithCatOn"), and also sets and returns the number of total 
//-- categories turned on over these points ("totCatHits") via "catHits".
//-- Most importantly however, within each "included" category this  
//-- function will update its value of "numPtsOn" (the number of points
//-- with that category on) and and also calculate it's "volumeDensity"
//-- (Vv) value, as determined by:
//-- 
//--     catObj[X].volumeDensity =  catObj[X].numPtsOn / totPtsWithCatOn
//-- 
//--
//-- If "tallyCounts" is true, this function also sets "numCounts" for
//-- each category by using "calcCountUsingName()" to multiplying its
//-- "numPtsOn" value by any number at the start of its "categoryName"..
//-- For example: a category called "5.INERSECTIONS" with "numPtsOn"=4 will
//-- have it's "numCounts" set to 20. All calculated "numCounts" values
//-- are then added together into "totCounts".
//-- 
//-- 
//-- TIPS:
//--
//--   >  If you call "printResults()" immediately after "calcResults()"
//--      it will return a nicely formatted string of these results.
//--   >  To do a breakdown of points over different ranges of grids/section
//--      you can thus call calcResults() many times with different values
//--      of minZVal and maxZVal.

long GridSetObj::calcResults( bool includeAllCats, bool onlyIncludeChecked,
														  long *catHits, int minZVal, int maxZVal, bool tallyCounts )
{
	totPtsWithCatOn = 0;	// total points with at least one of the
												//  included categories applied to it
	totCatHits = 0;			  // total number of times included categories occur
												//  in any point. If "allowMultipleCats" is true
												//  this should be >= totPtsWithCatOn and if false
												//  it should be the same
	
	int  csize = (int)catObj.size();
	long psize = (long)pts.size();
	
	//## RESET NUMBER OF POINTS ON FOR EACH CATEGORY:
	
	for( int c=0; c<csize; c++ )
	{
		catObj[c].numPtsOn      = 0;
		catObj[c].volumeDensity = 0.0f;
		catObj[c].stDev         = 0.0f;
		catObj[c].numCounts     = 0;
	}
	
	//## GO THROUGH ALL STEREOLOGY POINTS TO TALLY THE NUMBER
	//## OF POINTS CHECKED AND ON FOR EACH CATEGORY:
	
	for( long p=0; p<psize; p++ )
	{
		Spoint *spt = &pts[p];
		int numCatsOnForPt = 0;		// number of (selected) categories applied to this point
		int z = spt->pos.z;
		
		if( z < minZVal || z > maxZVal )
			continue;
		
		if( onlyIncludeChecked && !spt->checked )
			continue;
		
		for( int c=0; c<spt->catSel.size() && c<csize; c++ )
		{
			if( !includeAllCats && !catObj[c].includeInResults )
				continue;
			
			if( spt->catSel[c] == true )
			{
				catObj[c].numPtsOn++;
				totCatHits++;
				numCatsOnForPt++;
			}
		}
		
		if(numCatsOnForPt > 0)
			totPtsWithCatOn++;
	}
	
	//## CALCULATE THE VOLUME DENSITY AND STANDARD DEVIATION 
	//## FOR EACH INCLUDED CATEGORY:
	
	for( int c=0; c<csize; c++ )
	{
		if( !includeAllCats && !catObj[c].includeInResults )
			continue;
		
		catObj[c].volumeDensity = fDiv( catObj[c].numPtsOn, totPtsWithCatOn );
		
		//see: http://en.wikipedia.org/wiki/Binomial_distribution#Normal_approximation :
		float topPart = catObj[c].volumeDensity * (1.0f - catObj[c].volumeDensity);
		catObj[c].stDev = sqrt( fDiv( topPart, totPtsWithCatOn ) );
	}
	
	
	//## FOR EACH INCLUDED CATEGORY, CHECK IF IT HAS A NUMBER AT THE START
	//## AND (IF SO) TALLY THE TOTAL NUMBER OF COUNTS/INTERSECTIONS:
	
	totCounts = 0;
	if(tallyCounts)
	{
		for( int c=0; c<(int)catObj.size(); c++ )
		{
			if( !includeAllCats && !catObj[c].includeInResults )
				continue;
			
			if( catObj[c].catNameStartsWithNumber() )
			{
				catObj[c].numCounts = catObj[c].calcCountUsingName();
				totCounts += catObj[c].numCounts;
			}
		}
	}
	
	//## OUTPUT RETURN VALUES:
	
	if( catHits!=NULL )
		*catHits = totCatHits;
	
	return (totPtsWithCatOn);
}


//------------------------
//-- Calculates the value of "numIntercepts" for each category by
//-- finding the matching object in the input model ("imod") and
//-- simply adding the total number of points accross all the
//-- objects contours. This function only applies if g->allowIntercepts
//-- is true.
//-- 
//-- I may later change this to validate that each point does
//-- actually fall on the line, or at least show an error message
//-- if it doesn't.

void GridSetObj::calcIntercepts( bool includeAllCats, Imod *imod,
																 int minZVal, int maxZVal )
{
	
	for( int i=0; i<(int)catObj.size(); i++ )
	{
		catObj[i].numIntercepts = 0;
		
		if( !includeAllCats && !catObj[i].includeInResults )
			continue;
		
		int objIdx = catObj[i].objIdx;
		if( objIdx < 0 || objIdx >= osize(imod) )
			continue;
		
		Iobj *obj = getObj(imod, objIdx);
		for( int c=0; c<csize(obj); c++ )
			catObj[i].numIntercepts += psize( getCont(obj, c) );
		
		cout << "calcIntercepts  catObj[i].numIntercepts =" <<  catObj[i].numIntercepts  << endl;
	}
}


//------------------------
//-- Returns a table-like string showing a list of categories
//-- and the values of "numPtsOn" as generated by the "calcResults()"
//-- method above. An example of output follows:
//-- 
//--    CAT #			CATEGORY NAME							# POINTS		FRACTION TOTAL
//--    1					NO_CATEGORY								159					0.574007
//--    2					Mitochondrion							45					0.162455
//--    3					Mature Insulin Granule		73					0.263538
//-- 
//-- 
//-- Note that if "includeAllCats" is true, only categories with
//-- "includeInResults" on are printed. If "transpose" is true
//-- the axis you see above will be flipped such that categories
//-- would be listed along the columns (not rows). To seperate
//-- each result "sepChar" is used... and in the example above
//-- was set to "\t" (tab), but can be set to "," to produce a 
//-- comma seperated value file. If "includeHeader" is false
//-- the first row (or column if "transpose" is true) containing
//-- "CAT#,CATEGORY NAME,...,etc" is omitted, else it is
//-- included as the default.

QString GridSetObj::printResults( bool includeAllCats, bool transpose, QString sepChar,
																  bool includeHeader, bool includeStDev )
{
	QString outStr = "";
	QString SEP = (QString)sepChar;
		
	if( !transpose )
	{
		if(includeHeader)
		{
			outStr += "CAT #"+SEP+"CATEGORY NAME"+SEP+"# POINTS"+SEP+"FRACTION TOTAL (Vv)";
			outStr += ((includeStDev) ? (SEP+"STDEV") : ("")) +"\n";
		}
		for( int c=0; c<(int)catObj.size(); c++ )
		{
			if( !includeAllCats && !catObj[c].includeInResults )
				continue;
			
			outStr += QStr(c) + SEP;
			outStr += catObj[c].categoryName + SEP;
			outStr += QStr( catObj[c].numPtsOn ) + SEP;
			outStr += QStr( catObj[c].volumeDensity );
			if(includeStDev)
				outStr += SEP + QStr( catObj[c].stDev );
			outStr += "\n";
		}
	}
	else
	{
		if(includeHeader)
			outStr += "CATEGORY NAME:" + SEP;
		for( int c=0; c<(int)catObj.size(); c++ )
			if( includeAllCats || catObj[c].includeInResults )
				outStr += catObj[c].categoryName + SEP;
		outStr += "\n";
		
		if(includeHeader)
			outStr += "# POINTS:" + SEP;
		for( int c=0; c<(int)catObj.size(); c++ )
			if( includeAllCats || catObj[c].includeInResults )
				outStr += QStr( catObj[c].numPtsOn ) + SEP;
		outStr += "\n";
		
		if(includeHeader)
			outStr += "FRACTION TOTAL (Vv):" + SEP;
		for( int c=0; c<(int)catObj.size(); c++ )
			if( includeAllCats || catObj[c].includeInResults )
				outStr += QStr( catObj[c].volumeDensity ) + SEP;
		
		if(includeStDev)
		{
			outStr += "\n";
			if(includeHeader)
				outStr += "STDEV:" + SEP;
			for( int c=0; c<(int)catObj.size(); c++ )
				if( includeAllCats || catObj[c].includeInResults )
					outStr += QStr( catObj[c].stDev ) + SEP;
		}
	}
	
	return (outStr);
}


//------------------------
//-- Returns a string which summarizes "surface area density" 
//-- in the form:
//-- 
//--    TOTAL COUNTS:			          80
//--    TOTAL LENGTHS:              100 
//--    LINE LENGTH:								13 nm
//--    TOTAL LINE LENGTH:          1300 nm
//--    SURFACE AREA DENSITY (Sv):	0.1231 nm^2/nm^3
//-- 
//-- To seperate result "sepChar" is used... and in the example above
//-- was set to "\t" (tab).
//-- 
//-- To generate these values you should first call "calcResults()" to
//-- make sure "numCounts" is already calculated. This function calculates 
//-- "totLineLength" multiplying the value of ("numPtsForCalc")
//-- by "lineLengthPerPtInPixels()" and ("pixelSize"). 
//-- 
//-- Finally the value of "surfAreaDensity" (Sv) is calculated using the
//-- general stereology formulae:
//-- 
//--     surfAreaDensity  =  2 * ( totCounts / totLineLength )
//-- 
//-- If ("shortVersion") is true, this function will return this 
//-- value in the form "0.34 nm^2/nm^3", rather than the longer
//-- string shown above.
//--
//-- If ("shortVersion") is false, the long string is returned... and
//-- if "allowIntercepts" is true this string will also include 
//-- a list of all categories showing their "numIntercepts" value
//-- which must be calculated beforehand using "calcIntercepts()".

QString GridSetObj::printSurfAreaDensityResults( QString sepChar, long numPtsForCalc,
																								 float pixelSize, QString units, 
																								 bool shortVersion )
{
	//## DETERMINE THE TOTAL LINE LENGTH:
	
	float lineLengthPerPt   = lineLengthPerPtInPixels() * pixelSize;
	float totLineLength     = (float)numPtsForCalc * lineLengthPerPt;
	
	//## CALCULATE THE FINAL SURFACE AREA DENSITY:
	
	float surfAreaDensity = 2.0f * fDiv( totCounts, totLineLength );
	QString densityStr = QStr(surfAreaDensity) + " " +units+ "^2/" +units+ "^3";
	
	if(shortVersion)
		return (densityStr);
	
	//## GENERATE OUTPUT STRING:
	
	QString outStr = "";
	QString SEP = (QString)sepChar;
	
	outStr += "SURFACE AREA ESTIMATIONS USING NUMBERED 'COUNTING' CATEGORIES: \n";
	outStr += "  TOTAL COUNTS:"      +SEP+ QStr( totCounts       ) + "\n";
	outStr += "  TOTAL LENGTHS:"     +SEP+ QStr( numPtsForCalc ) + "\n";
	outStr += "  LINE LENGTH:"       +SEP+ QStr( lineLengthPerPt ) + " " +units+ "\n";
	outStr += "  TOTAL LINE LENGTH:" +SEP+ QStr( totLineLength   ) + " " +units+ "\n";
	outStr += "  SURFACE AREA DENSITY (Sv):"  +SEP+ densityStr + "\n";
	if(units=="nm")
		outStr += "   . . . . . . . . . . . . . . . . = " +SEP+
		          QStr(surfAreaDensity * 1000.0f) + " um^2/um^3\n";
	
	if(allowIntercepts)
	{
		outStr += "\n";
		outStr += "CATEGORY-WISE BREAKDOWN: \n";
		outStr += "  CATEGORY" +SEP+ "# INTERCEPTS" +SEP+ "Sv (" +units+ "^2/" +units+ "^3)\n";
		for(int c=0;c<(int)catObj.size();c++)
		{
			outStr += "  > [" + QStr(c) + "] " + catObj[c].categoryName + SEP;
			outStr += QStr( catObj[c].numIntercepts ) + SEP;
			outStr += QStr( fDiv( (float)catObj[c].numIntercepts, totLineLength ) ) + "\n";
		}
	}
	
	return (outStr);
}

//------------------------
//-- Returns a string which summarizes "length density" results. This
//-- features is currently not implemented so the return string looks
//-- like this:
//-- 
//--    TOTAL COUNTS:			          80
//--    TOTAL LENGTHS:              100 
//--    LINE LENGTH:								13 nm
//--    TOTAL LINE LENGTH:          1300 nm
//--    NUMBER DENSITY (Nv):				Not yet implemented
//--    contact andrew if interested in this feature

QString GridSetObj::printLengthDensityResults( QString sepChar, long numPtsForCalc,
																							float pixelSize, QString units )
{
	//## DETERMINE THE TOTAL LINE LENGTH:
	
	float lineLengthPerPt   = lineLengthPerPtInPixels() * pixelSize;
	float totLineLength     = (float)numPtsForCalc * lineLengthPerPt;
	
	//## GENERATE OUTPUT STRING:
	
	QString outStr = "";
	QString SEP = (QString)sepChar;
	
	outStr += "TOTAL COUNTS:"      +SEP+ QStr( totCounts       ) + "\n";
	outStr += "TOTAL LENGTHS:"     +SEP+ QStr( numPtsForCalc   ) + "\n";
	outStr += "LINE LENGTH:"       +SEP+ QStr( lineLengthPerPt ) + " " + units + "\n";
	outStr += "TOTAL LINE LENGTH:" +SEP+ QStr( totLineLength   ) + " " + units + "\n";
	outStr += "LENGTH DENSITY (Lv): Not yet implemented\n";
	outStr += "contact andrew if interested in this feature\n";
	
	return (outStr);
}

//------------------------
//-- Returns a string which summarizes "number density" results. This
//-- feature is currently not implemented so the return string looks
//-- like this:
//-- 
//--    TOTAL COUNTS:			          80
//--    TOTAL LENGTHS:              100 
//--    LINE LENGTH:								13 nm
//--    TOTAL LINE LENGTH:          1300 nm
//--    SURFACE AREA DENSITY (Sv):	Not yet implemented
//--    contact Andrew if interested in this feature

QString GridSetObj::printNumberDensityResults( QString sepChar, float pixelSize,
																							 QString units, float zScale,
																							 int zMinUsed, int zMaxUsed )
{
	QString outStr = "";
	QString SEP = (QString)sepChar;
	
	//## DETERMINE DIMENSIONS OF SIDES IN UNITS:
	
	float sqXInUnits = 0.0f;			// |-- the length of sides in X and Y for each
	float sqYInUnits = 0.0f;			// |   stereology point (dending on "gridType")
	switch (gridType)
	{
		case (GD_RECTS):
		{
			sqXInUnits = xSpacing/2.0f * pixelSize;
			sqYInUnits = ySpacing/2.0f * pixelSize;
		}
		case (GD_FORBIDSQ1):
		case (GD_FORBIDSQ2):
		case (GD_FORBIDSQ3):
		{
			float sqLength = MIN( xSpacing, ySpacing );
			if( gridType == GD_FORBIDSQ1 )		sqLength /= 2.0f;
			if( gridType == GD_FORBIDSQ2 )		sqLength /= 4.0f;
			if( gridType == GD_FORBIDSQ3 )		sqLength /= 8.0f;
			sqXInUnits = sqLength * pixelSize;
			sqYInUnits = sqLength * pixelSize;
		}
	}
	
	float rectXInUnits = rXSpan * pixelSize;
	float rectYInUnits = rYSpan * pixelSize;
	
	
	//## CALCULATE NUMBER DENSITY:
	
	float zRangeUsed = zMaxUsed - zMinUsed;
	
	float areaPerPtInUnits   = areaPerPtInPixels() * ( pixelSize * pixelSize );
	long  sqsPerGrid         = ptsPerGridSetup;
	float areaPerGrid        = areaPerPtInUnits * sqsPerGrid;
	long  totalSquares       = sqsPerGrid  * (zMaxUsed - zMinUsed);
	float totalAreaSqs       = areaPerPtInUnits * totalSquares;
	float numDensitySqs      = fDiv( totCounts, totalAreaSqs );
	
	double areaSubRectInUnits = rXSpan * rYSpan     * ( pixelSize * pixelSize );
	int    numSubRectsPerGrid = ((int) fDiv((xMax - xMin), (rXSpan + rXGap))) *
	                            ((int) fDiv((yMax - yMin), (rYSpan + rYGap)));	
	double zSpanInUnits       = (zRangeUsed)        * (zScale * pixelSize);
	double volSubRectInUnits  = (areaSubRectInUnits * numSubRectsPerGrid) * zSpanInUnits;
	double numDensityRects    = fDiv( totCounts, totalAreaSqs );
	
	//## GENERATE OUTPUT STRING:
	
	outStr += "TOTAL COUNTS:" +SEP+ QStr( totCounts ) + "\n";
	
	if( isNumberDensityGridType() )
	{
		outStr += "--------\n";
		outStr += "FORBIDDEN SQUARES 2D:\n";
		outStr += " > SQUARES/GRID:" +SEP+ QStr( ptsPerGridSetup  ) +"\n";
		outStr += " > SQUARE DIMENSIONS:" +SEP+ QStr( sqXInUnits  ) + " x ";
		outStr += QStr( sqYInUnits ) +units+"\n";
		outStr += " > AREA/SQUARE:"  +SEP+ QStr( areaPerPtInUnits ) +" "+units+"^2\n";
		outStr += " > AREA/GRID:"    +SEP+ QStr( areaPerGrid      ) +" "+units+"^2\n";
		outStr += "\n";
		outStr += " OVER ALL GRIDS:";
		outStr += " > TOTAL SQUARES:"+SEP+ QStr( totalSquares ) +" "+units+"^2\n";
		outStr += " > TOTAL AREA:"   +SEP+ QStr( totalAreaSqs ) +" "+units+"^2\n";
		if(units=="nm")
			outStr += SEP + QStr( totalAreaSqs/(1000^2) ) + " um^2\n";
		outStr += "2D NUMBER DENSITY (Nv):";
		outStr += SEP + QStr( numDensitySqs ) + " surfs/" + units + "^2\n";
		if(units=="nm")
			outStr += SEP + QStr( numDensitySqs*(1000^2) ) + " surfs/um^2\n";
	}
	if( subRectsApplied )
	{
		outStr += "--------\n";
		outStr += "PHYSICAL DISSECTOR (USING SUBSAMPLE OPTION)\n";
		outStr += "FORBIDDEN RECTANGLES 3D:\n";
		outStr += " > NUMBER RECTS (IN X/Y):" +SEP+ QStr( numSubRectsPerGrid ) + "\n";
		outStr += " > SLICE RANGE:"      +SEP+ QStr( zSpanInUnits )       + " "+units+"\n";
		outStr += " > RECT DIMENSIONS:"  +SEP+ QStr( rectXInUnits ) + " x ";
		outStr += QStr( rectYInUnits ) +units+"\n";
		outStr += " > AREA EACH RECT:"   +SEP+ QStr( areaSubRectInUnits ) +" "+units+"^2\n";
		outStr += " > VOLUME ALL RECTS:" +SEP+ QStr( volSubRectInUnits  ) +" "+units+"^3\n";
		if(units=="nm")
			outStr += SEP + QStr( volSubRectInUnits/(1000^3) ) + " um^3\n";
		outStr += "3D NUMBER DENSITY (Nv):";
		outStr += SEP + QStr( numDensityRects ) + " surfs/" + units + "^3\n";
		if(units=="nm")
			outStr += SEP + QStr( numDensityRects*(1000^3) ) + " surfs/um^3\n";
	}
	
	return (outStr);
}


//------------------------
//-- If not already: sets up list of strings matching
//-- each of the enum "gridtype" values.

void GridSetObj::setupGridTypeStrings()
{
	if ( !gridTypeStr.empty() )
		return;
	
	gridTypeStr.resize( NUM_GRID_TYPES );
	gridTypeStr[ GD_LINES        ] = "lines";
	gridTypeStr[ GD_CROSSHAIRS   ] = "pts";
	gridTypeStr[ GD_ARROWS       ] = "pts2";
	gridTypeStr[ GD_OFFPTS       ] = "offpts";
	gridTypeStr[ GD_RAND         ] = "random";
	gridTypeStr[ SPACE1          ] = "----";
	gridTypeStr[ GD_VERTLINES    ] = "vert";
	gridTypeStr[ GD_HORZLINES    ] = "horz";
	gridTypeStr[ GD_UPDIAGONAL   ] = "diag_up";
	gridTypeStr[ GD_DOWNDIAGONAL ] = "diag_down";
	gridTypeStr[ GD_LINEPAIRS    ] = "linepairs";
	gridTypeStr[ GD_CYCLOIDS     ] = "cycloids";
	gridTypeStr[ GD_CYCLOIDS2    ] = "cycloids2";
	gridTypeStr[ GD_CYCLOIDS3    ] = "cycloids3";
	gridTypeStr[ SPACE2          ] = "----";
	gridTypeStr[ GD_RECTS        ] = "rects2";
	gridTypeStr[ GD_FORBIDSQ1    ] = "forbidsq2";
	gridTypeStr[ GD_FORBIDSQ2    ] = "forbidsq4";
	gridTypeStr[ GD_FORBIDSQ3    ] = "forbidsq8";
	gridTypeStr[ GD_OFF          ] = "hide";
}

//------------------------
//-- Inputs a string ("gridTypeAsString") and tries to match it 
//-- to a matching "gridtype" enum integer value.

int GridSetObj::getMatchingGridType( QString gridTypeAsString )
{
	setupGridTypeStrings();
	
	for( int i=0; i<NUM_GRID_TYPES; i++ )
		if( gridTypeAsString==gridTypeStr[i] )
			return i;
	
	return -1;
}

//------------------------
//-- Returns a string representation of the grids "gridType" value.
//-- For example "GD_RAND" returns "random".

QString GridSetObj::gridTypeAsString()
{
	setupGridTypeStrings();
	return gridTypeStr[ gridType ];
}

	
//------------------------
//-- Outputs a nicely formatted string representing the values of the grid
//-- and, if "includeCats" is true, it will also include the names and "objIdx"
//-- of each category in the "catObj" vector. An example of output is below:
//--
//--    GRID SET:
//--    gridSetId 	= '1'
//--    objIdx  	  = 1
//--    spacing 	  = x:50, y:50, z:30
//--    limits  	  = x:20-100, y:20-492, z:0-100
//--    pts in grid       = 9 cols * 9 rows * 4 grids = 324 points
//--    points setup 	    = 0
//--    CATEGORIES:
//--    > [1] 'NO_CATEGORY' 	objIdx = 2
//--    > [2] 'Mitochondrion' 	objIdx = 3
//--    > [3] 'Mature Insulin Granule' 	objIdx = 4
//-- 

QString GridSetObj::toString( bool includeCats )
{
	QString outStr;			// string to output
	
	int numCats = (int)catObj.size();
	
	outStr += "GRID SET:\n";
	outStr += " gridSetId \t= '" + gridSetId + "'\n";
	outStr += " objIdx  \t= " + QStr(objIdx) + "\n";
	outStr += " type    \t= " + gridTypeAsString() + "\n";
	outStr += " spacing \t= x:" + QStr(xSpacing) + ",";
	outStr +=             " y:" + QStr(ySpacing) + ",";
	outStr +=             " z:" + QStr(zSpacing) + "\n";
	outStr += " limits  \t= x:" + QStr(xMin) + "-" + QStr(xMax) + ",";
	outStr +=             " y:" + QStr(yMin) + "-" + QStr(yMax) + ",";
	outStr +=             " z:" + QStr(zMin) + "-" + QStr(zMax) + "\n";
	outStr += " pts in grid  \t= " + QStr( cols ) + " cols * " + QStr( cols ) + " rows * ";
	outStr += QStr( cols ) + " grids = "+ QStr( maxPts ) + " points\n";
	outStr += " points setup \t= " + QStr( ptsize() ) + "\n";
	if( showSubRects   ) {
		outStr += " subsample rectangles \t(true)";
		outStr += " \t span: " + QStr(rXSpan) + " x " + QStr(rYSpan);
		outStr += " \t gap: " + QStr(rXGap)  + " x " + QStr(rYGap) + "\n";
	}
	if( countingStarted   )	outStr += " counting started \t(true) \n";
	if( allowMultipleCats )	outStr += " multi_cats \t(true) \n";
	if( allowIntercepts   )	outStr += " allow_intercepts \t(true) \n";
	if( !includeCats      )	outStr += " categories   \t= " + QStr(numCats) + "\n";
	if( includeCats )
	{
		outStr += " CATEGORIES:\n";
		for( int c=0; c<numCats; c++ )
		{
			outStr += "  > [" + QStr(c) + "] '" + catObj[c].categoryName + "' ";
			outStr += "\tobjIdx = " + QStr( catObj[c].objIdx ) + "\n";
		}
	}
	
	return(outStr);
}


//----------------------------------------------------------------------------
//
//          MAPPED FUNCTIONS:
//
//----------------------------------------------------------------------------



//------------------------
//-- MAPPED FUNCTION: Called by the imod plugin load function

const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE + 
      IMOD_PLUG_MOUSE + IMOD_PLUG_EVENT;
    
  return("Stereology");
}

//------------------------
//-- MAPPED FUNCTION: Grab hotkey input. return 1 if we handle the key.

int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  int keyhandled = 1;
  
  if (!plug.view)          // if plugin window isn't open: don't grab keys
    return 0;
  
  int keysym  = event->key();            // key value (Key_A, Key_Space... etc)
  int ctrl    = event->modifiers() & Qt::ControlModifier;   // ctrl modifier
  int shift   = event->modifiers() & Qt::ShiftModifier;     // shift modifier
  
	plug.shiftDown = (shift != 0);
	
  switch(keysym)
  {
		case Qt::Key_QuoteLeft:																			// "~/`" key
		case Qt::Key_0:		plug.window->toggleCategory( 0 ); 		break;
		case Qt::Key_1:		plug.window->toggleCategory( 1 ); 		break;
		case Qt::Key_2:		plug.window->toggleCategory( 2 ); 		break;
		case Qt::Key_3:		plug.window->toggleCategory( 3 ); 		break;
		case Qt::Key_4:		plug.window->toggleCategory( 4 ); 		break;
		case Qt::Key_5:		plug.window->toggleCategory( 5 ); 		break;
		case Qt::Key_6:		plug.window->toggleCategory( 6 ); 		break;
		case Qt::Key_7:		plug.window->toggleCategory( 7 ); 		break;
		case Qt::Key_8:		plug.window->toggleCategory( 8 ); 		break;
		case Qt::Key_9:		plug.window->toggleCategory( 9 ); 		break;
		
		case Qt::Key_Left:	
		{
			if (shift)		plug.window->goToPrevUncheckedPt();
			else					plug.window->goToPrevPt();
			break;
		}
		case Qt::Key_Right:	
		{
			if (shift)		plug.window->goToNextUncheckedPt();
			else					plug.window->goToNextPt();
			break;
		}
		case Qt::Key_Up:
		{
			if (shift)		plug.window->goToPrevGrid();
			else					plug.window->goToPrevRow();
			break;
		}
		case Qt::Key_Down:
		{
			if (shift)		plug.window->goToNextGrid();
			else					plug.window->goToNextRow();
			break;
		}
		
		case Qt::Key_Comma:
		{
			if (shift)	plug.window->goToFirstUncheckedPt();
			else				plug.window->goToPrevGrid();
			break;
		}
		case Qt::Key_Period:
		{
			if (shift)	plug.window->goToLastUncheckedPt();
			else				plug.window->goToNextGrid();
			break;
		}
		
		case Qt::Key_Tab:								// tab: jumps to very first unchecked pt
			plug.window->goToFirstUncheckedPt();
			break;
		case Qt::Key_Return:
		case Qt::Key_Enter:							// return: jumps to next unchecked pt
			plug.window->goToNextUncheckedPt();
			break;
		
		case Qt::Key_Space:	
			plug.window->togglePtChecked();
			break;
		
			
		case Qt::Key_Q:
			plug.window->changePtPaintRadius( (shift) ? -5.0f : -1.0f );
			plug.window->drawExtraObject(true);
			break;
		
		case Qt::Key_W:
			plug.window->changePtPaintRadius( (shift) ? 5.0f : 1.0f );
			plug.window->drawExtraObject(true);
			break;
			
		//## PREVENT MODIFICATION OF "STEREOLOGY" OBJECTS:	
		
		case Qt::Key_Backspace:
		case Qt::Key_Delete:	
		{
			return (plug.window->isStereologyObjSelected(true)) ? 1 : 0;
		}	
		case Qt::Key_D:	
		{
			return (shift && plug.window->isStereologyObjSelected(true)) ? 1 : 0;
		}
		
		case Qt::Key_N:	
		{
			return (plug.window->isStereologyObjSelected(true)) ? 1 : 0;
		}
		
		case Qt::Key_B:	
		{
			plug.window->goToFirstInvalidPoint();
			break;
		}
		
    default:
      keyhandled = 0;
      break;
  }
  
  return keyhandled;
}


//------------------------
//-- MAPPED FUNCTION: Called when plugin window is started.
//-- Opens the plugin window for user interaction and initilizes data.
//-- See imodplug.h for a list of support functions.

void imodPlugExecute(ImodView *inImodView)
{

  if (plug.window) {      // if already open: bring window to front
    plug.window->raise();
    return;
  }
  
	
	ivwSetMovieModelMode( plug.view, IMOD_MMODEL );
	
  plug.view = inImodView;
	ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  ivwTrackMouseForPlugs(plug.view, 1);
	ivwEnableStipple( plug.view, 1 );     // enables the display of stippled lines
	
	
  //## INITIALIZE DATA:
	
  if( !plug.initialized )
  {
    plug.window->initValues();
    plug.window->loadSettings();
    plug.window->addGrid( true );		// adds a default grid
		
    plug.initialized = true;
  }
  
	//## SET UP EXTRA OBJECTS:
  
	plug.extraObjGrid     = ivwGetFreeExtraObjectNumber(plug.view);
	plug.extraObjRect2    = ivwGetFreeExtraObjectNumber(plug.view);
	plug.extraObjRect1    = ivwGetFreeExtraObjectNumber(plug.view);
	plug.extraObjSel      = ivwGetFreeExtraObjectNumber(plug.view);
  plug.extraObjBlack    = ivwGetFreeExtraObjectNumber(plug.view);
	plug.extraObjExtra    = ivwGetFreeExtraObjectNumber(plug.view);
  
	
  Iobj *xobjG = ivwGetAnExtraObject(plug.view, plug.extraObjGrid);
  setObjColor(xobjG, plug.gridColorR, plug.gridColorG, plug.gridColorB);
  imodObjectSetValue(xobjG, IobjLineWidth2, plug.gridLineThickness);
	imodObjectSetValue(xobjG, IobjFlagClosed, 0);
	
	Iobj *xobjR1 = ivwGetAnExtraObject(plug.view, plug.extraObjRect1);
  setObjColor(xobjR1, 200, 0, 0);		// dark red
  imodObjectSetValue(xobjR1, IobjLineWidth2, plug.gridLineThickness+1);
	imodObjectSetValue(xobjR1, IobjFlagClosed, 0);
	
	Iobj *xobjR2 = ivwGetAnExtraObject(plug.view, plug.extraObjRect2);
  setObjColor(xobjR2, 20, 240, 0);		// off green
  imodObjectSetValue(xobjR2, IobjLineWidth2, plug.gridLineThickness+1);
	imodObjectSetValue(xobjR2, IobjFlagClosed, 0);
	
	Iobj *xobjS = ivwGetAnExtraObject(plug.view, plug.extraObjSel);
  imodObjectSetColor(xobjS, 1, 1, 0);			// yellow
	imodObjectSetValue(xobjS, IobjLineWidth2, 2);
  imodObjectSetValue(xobjS, IobjFlagClosed, 0);
	
	Iobj *xobjB = ivwGetAnExtraObject(plug.view, plug.extraObjBlack);
  imodObjectSetColor(xobjB, 0, 0, 0);			// black
	imodObjectSetValue(xobjB, IobjLineWidth2, plug.gridLineThickness);
  imodObjectSetValue(xobjB, IobjFlagClosed, 0);
	
	Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  imodObjectSetColor(xobjX, 1, 1, 1);			// white
	imodObjectSetValue(xobjX, IobjLineWidth2, 2);
  imodObjectSetValue(xobjX, IobjFlagClosed, 0);
	
	
  //## CREATE THE PLUGIN WINDOW:
  
  plug.window  = new Stereology(imodDialogManager.parent(IMOD_DIALOG),"Stereology");
	plug.window->setWindowTitle("Stereology (*by SLASH*)");		// to help with our grant
	
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)plug.window, IMOD_DIALOG );
	
	plug.window->drawGridObject(true);
	
	
	//## CHECK IF ANY "STEREOLOGY" OBJECTS EXISTS IN MODEL AND IF 
	//## SO ALLOW USER THE OPTION TO LOAD THIS - IF NOT THEN TRY 
	//## AND LOAD DEFAULT CATEGORIES:
	
	bool gridLoaded = plug.window->loadGridFromImodModel( true );
	
	if( !gridLoaded )
		plug.window->loadDefaultCatNames();
}


//------------------------
//-- MAPPED FUNCTION: Process wheel events

int imodPlugEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
	if( plug.window == NULL )
    return (0);
  
  if (event->type() == QEvent::Wheel)
  {
    QWheelEvent *wheelEvent = static_cast<QWheelEvent*>(event);
    float scrollAmount    = wheelEvent->delta();
		
		if(plug.paintMode==PM_PAINT)
		{
			float multiplier   = 2.0f + (int)(plug.paintRadius / 100.0f);
			float changeAmount = multiplier*(scrollAmount / 100.0f);
			plug.window->changePtPaintRadius( changeAmount, plug.shiftDown );
			
			plug.window->drawExtraObject(true);
		}
	}
		
  return 0;
}

//------------------------
//-- MAPPED FUNCTION: Process a mouse event: An example of a circular cursor  
//-- with radius specified in image coordinates

/*
     Mouse event callback function to be defined by plugins with the
     IMOD_PLUG_MOUSE bit set.  ^
     This function can be used to override 3dmod mouse actions in the Zap 
     window.  [imx] and [imy] will contain the image position, and [but1], 
     [but2], and [but3] will indicate the state of the 3 buttons as mapped by 
     user preferences.  The return value should be the sum of two values: 
     ^  1 if the plugin handled the mouse event, indicating that no other action
     should be taken with the event itself by the 3dmod program.
     ^  2 if the specific calling window should draw itself, without issuing a
     general program redraw.  If this is not sufficient, the plugin should call
     ivwRedraw instead of setting this bit.
     ^  A zero return value indicates that 3dmod should process the event as usual.
     ^This function is called only when a mouse button is down, unless mouse
     tracking is enabled with ivwTrackMouseForPlugs.
    
    BUTTON KEY: (using my setup)
        LEFT   = but2 ********
        MIDDLE = but3
        RIGHT  = but1
*/

int imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx, float imy,
                  int but1, int but2, int but3)
{
  // if plugin is not open or imod isn't in "model mode": do nothing
  if( !plug.window || !ivwGetMovieModelMode(plug.view) )
    return (0);
  
	
	//## UPDATE MOUSE VALUES:
	
  int noZap = ivwGetTopZapMouse(plug.view, &plug.mouse); // returns 1 if no Zap window
  if(noZap == 1)
    return (2);
	
	
	//## REDRAW PAINT CIRCLE (IF EXISTS):
	
	plug.window->drawExtraObject( true );		// redraw 
	
	
	//## UPDATE BUTTON PRESSED VALUES:
	
  plug.but1Pressed  = (but1 == 1) && (plug.but1Down == 0);
  plug.but2Pressed  = (but2 == 1) && (plug.but2Down == 0);
  plug.but3Pressed  = (but3 == 1) && (plug.but3Down == 0);
  
  plug.but1Released = (but1 == 0) && (plug.but1Down == 1);
  plug.but2Released = (but2 == 0) && (plug.but2Down == 1);
  plug.but3Released = (but3 == 0) && (plug.but3Down == 1);
  
  plug.but1Down = but1;      // right mouse   (using my preferred settings)
  plug.but2Down = but2;      // left mouse    (using my preferred settings)
  plug.but3Down = but3;      // middle mosue  (using my preferred settings)
  
	
	bool isButtonDown = plug.but1Down || plug.but2Down || plug.but3Down;
	
	
	//## IF IN MOVIE MODE: LET IMOD HANDLE
	
	bool movieMode = (ivwGetMovieModelMode(plug.view) == 0);
	if( movieMode )
			return (0);															// let IMOD handle
	
	//## IF ANY BUTTON DOWN: REMOVE FOCUS FROM PLUGIN
	
	if( isButtonDown )
	{
		plug.window->clearFocus();	// most importantly we want to clear focus from spnSelPt
	}
	
	
	//## IF IN PAINT MODE:
	
	if( plug.paintMode==PM_PAINT && plug.window->getCurrGridSetObj()->countingStarted )
	{
		if( plug.but2Down )			// if button 2 (add pt) is down:
		{
			plug.window->executePaint();
			return (1);														// don't let IMOD handle
		}
		else if( plug.but3Down )			// if button 3 (move pt) is down:
		{
			plug.window->resetPaint();
			return (1);														// don't let IMOD handle
		}
	}
	
	
	//## IF IN PAINT INTERCEPT MODE:
	
	if(plug.paintMode==PM_INTERCEPTS && plug.window->getCurrGridSetObj()->countingStarted)
	{
		if( plug.but2Down )			// if button 2 (add pt) is down:
		{
			if( plug.but2Pressed ) 
				plug.window->executeIntercept();
			return (1);														// don't let IMOD handle
		}
		else if( plug.but3Down )			// if button 3 (move pt) is down:
		{
			plug.window->deleteIntercept();
			return (1);														// don't let IMOD handle
		}
	}
	
	
	//## IF NOT IN PAINT MODE AND A DRAWING BUTTON IS DOWN:
	//## CHECK IF A "STEREOLOGY" OBJECT IS SELECTED AND IF SO
	//## PREVENT USER FROM ADDING/MOVING POINTS
	
	if( plug.paintMode==PM_OFF && (plug.but2Down || plug.but3Down) )
	{
		bool stereologyObjSel = plug.window->isStereologyObjSelected(true);
											// checks if current object contains the string "STEREOLOGY"
		
		if( stereologyObjSel )			// if a "STEREOLOGY" object is selected:
			return (1);									// don't let IMOD handle
	}
	
	
	//## IF SELECT BUTTON PRESSED: CHECK IF GRID POINT SELECTED
	
	static bool newPtSel = false;
	
	if( plug.but1Pressed && !newPtSel )			// if button 1 pressed (pt select):
	{
		GridSetObj *g = plug.window->getCurrGridSetObj();
		float zapZoom = 1.0f;
		ivwGetTopZapZoom(plug.view, &zapZoom);
		float sc = fDiv( 1.0f, zapZoom);
		
		long newPtIdx = 0;
		bool ptFound = g->isPtInRadius( &plug.mouse, sc*12.0f, &newPtIdx );
		
		if( ptFound )												// if grid point found within 12 pixels of click:
		{
			newPtSel = true;														// flag that we've found a point
			g->setCurrPt( newPtIdx, false );						// set this point as current
			plug.window->updateCurrPtInGui(false,true);	// update GUI (without jumping)
			return (1);																	// don't let IMOD handle
		}
	}
	else if( plug.but1Down && newPtSel )		// if button 1 down after point selected:
	{
		return (1);															// don't let IMOD handle
	}
	else if( plug.but1Released )						// if button 1 released:
	{
		newPtSel = false;												// reset flag
	}
	
	
  return (0);			// let IMOD handle
}






//############################################################


//----------------------------------------------------------------------------
//
//          STEREOLOGY METHODS:
//
//----------------------------------------------------------------------------



//------------------------
//-- Convenience method, allowing you to create a new QAction. The new action
//-- triggers the method "member" (when clicked), is called "text", has the 
//-- status tip "tip" and is added to the specified "menu".
//-- Returns the newly created QAction

QAction *Stereology::addAction( QMenu *menu, const char *member,
                                   QString text, QString tip )
{
  QAction *newAction = new QAction(text, this);
  newAction->setToolTip( tip );
  newAction->setStatusTip( tip );
  connect( newAction, SIGNAL(triggered()), this, member );
  menu->addAction( newAction );
  
  return newAction;
}


//## WINDOW CLASS CONSTRUCTOR:

static const char *buttonLabels[] = {(char*)"Done", (char*)"Video", (char *)"Help"};
static const char *buttonTips[]   = {(char*)"Close this plugin window",
	                             (char*)"See SLASH help videos showing \n"
	                                    "how to use this plugin",
	                             (char*)"Open help window"};

Stereology::Stereology(QWidget *parent, const char *name) :
  DialogFrame(parent, 3, buttonLabels, buttonTips, true, "Stereology", "", name)
{
  const int LAY_MARGIN   = 3;
  const int LAY_SPACING  = 3;
  
  QFont smallFont;
  smallFont.setPixelSize(12);
  
	GridSetObj *g = getCurrGridSetObj();
  
	//## RECOLOR MIDDLE "Video" BUTTON:
	
	mButtons[1]->setStyleSheet("color: rgb(150, 180, 255);");
	mButtons[1]->setFlat( true );
	mButtons[1]->setCursor( Qt::PointingHandCursor );
	mButtons[2]->setCursor( Qt::PointingHandCursor );
	
	
//## CREATE TAB FOR GRID SETUP (SHOWN WHEN NO GRID IS LOADED):
	
	widGridSetup_tab0 = new QWidget(this);
  QVBoxLayout *layGridSetup_tab0 = new QVBoxLayout(widGridSetup_tab0);
  layGridSetup_tab0->setSpacing(LAY_SPACING);
  layGridSetup_tab0->setContentsMargins(10, 10, 5, 5);
  widGridSetup_tab0->setLayout( layGridSetup_tab0 );
	
	widGridScroll = new QWidget(widGridSetup_tab0);
  QVBoxLayout *layGridScroll = new QVBoxLayout(widGridScroll);
  layGridScroll->setSpacing(LAY_SPACING);
	layGridScroll->setContentsMargins(10, 10, 5, 5);
	widGridScroll->setLayout( layGridScroll );
	widGridScroll->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );
	
	widGridSetup = new QWidget(widGridSetup_tab0);
  QVBoxLayout *layGridSetup = new QVBoxLayout(widGridSetup);
  layGridSetup->setSpacing(LAY_SPACING);
  layGridSetup->setContentsMargins(0, 0, 0, 0);
  widGridSetup->setLayout( layGridSetup );
	widGridSetup->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );
	
	
	
	//** ROW 0:
	
	QWidget *widRow0 = new QWidget(widGridSetup);
	QHBoxLayout *layRow0 = new QHBoxLayout(widRow0);
	layRow0->setSpacing(LAY_SPACING);
  layRow0->setContentsMargins(0, 0, 0, 0);
	widRow0->setLayout( layRow0 );
	
	QLabel *lblGridDisplay = new QLabel("grid type: ", widRow0);
  lblGridDisplay->setFocusPolicy(Qt::NoFocus);
  lblGridDisplay->setToolTip( "Options for how the grid is dispayed");
  layRow0->addWidget(lblGridDisplay );
	
	cmbGridDisplayOpt = new QComboBox(widRow0);
  cmbGridDisplayOpt->setFocusPolicy(Qt::NoFocus);
  cmbGridDisplayOpt->addItem("lines");
  cmbGridDisplayOpt->addItem("crosshairs");
  cmbGridDisplayOpt->addItem("arrows");
	cmbGridDisplayOpt->addItem("off pts");
	cmbGridDisplayOpt->addItem("random pts");
	cmbGridDisplayOpt->insertSeparator(SPACE1);
	cmbGridDisplayOpt->addItem("vert lines");
	cmbGridDisplayOpt->addItem("horz lines");
	cmbGridDisplayOpt->addItem("diag up");
	cmbGridDisplayOpt->addItem("diag down");
	cmbGridDisplayOpt->addItem("line pairs");
	cmbGridDisplayOpt->addItem("cycloids");
	cmbGridDisplayOpt->addItem("cycloids alt");
	cmbGridDisplayOpt->addItem("cycloids long");
	cmbGridDisplayOpt->insertSeparator(SPACE2);
	cmbGridDisplayOpt->addItem("rectangles   1:2");
	cmbGridDisplayOpt->addItem("forbid sq   1:2");
	cmbGridDisplayOpt->addItem("forbid sq   1:4");
	cmbGridDisplayOpt->addItem("forbid sq   1:8");
	cmbGridDisplayOpt->addItem("none");
  cmbGridDisplayOpt->setCurrentIndex( plug.gridDisplayOpt );
  connect(cmbGridDisplayOpt,SIGNAL(activated(int)),this,SLOT(changeGridTypeCmb(int)));
  cmbGridDisplayOpt->setToolTip
	("Options for how the grid is dispayed."
	 "<ul>"
	 "<font color='#999999'><li>"
	 "<u>FOR VOLUME ESTIMATES</u>: (classifying each point)</li></font>"
	 "<li><b>lines</b> - shows horizontal and vertical grid lines</li>"
	 "<li><b>crosshairs</b> - shows a small crosshair over every stereology point</li>"													 
	 "<li><b>arrows</b> - shows a tiny arrow over every point</li>"
	 "<li><b>off pts</b> - shows points that are offset every second row</li>"
	 "<li><b>rand</b> - shows markers for lines on each side...<br>"
	 "<i>... (for non-publication estimates using random pts).</li>"
	 "<font color='#999999'><li>"
	 "<u>FOR SURFACE AREA ESTIMATES</u>: (counting # intersections per lines)</li></font>"
	 "<li><b>vert lines</b> - shows vertical grid lines only</li>"
	 "<li><b>horz lines</b> - shows horizontal grid lines only</li>"
	 "<li><b>diag up</b>   - shows diagonal lines that slope up (to the right)</li>"
	 "<li><b>diag down</b> - shows diagonal lines that slope down (to the right)</li>"
	 "<li><b>line pairs</b> - spaced out horz lines staggered on alternate rows<li>"													
	 "<li><b>cycloids</b> - evenly spaced cycloid lines</li>"
	 "<li><b>cycloids alt</b>  - as above, but with every second cycloid facing down</li>"
	 "<li><b>cycloids long</b> - continous cycloids good for length estimates too</li>"
	 "<font color='#999999'><li>"
	 "<u>FOR NUMBER ESTIMATES</u>: (counting surfaces inside boxes)</li></font>"
	 "<li><b>rectangles 1:2</b> - show rectangles with half the spacing of x and y</li>"
	 "<li><b>forbidden squares 1:2</b> -| show special 'forbidden squares' structures</li>"
	 "<li><b>forbidden squares 1:4</b> .| with sides 1/2, 1/4 and 1/8 minimum spacing</li>"
	 "<li><b>forbidden squares 1:8</b> .| nad used to estimate number of surfaces</li>"
	 "<li><b>none</b> - shows just markers and the boundary of limits specified</li>"
	 "</ul>" );
	layRow0->addWidget(cmbGridDisplayOpt );
	
	layGridSetup->addWidget(widRow0);
	
	
	
	//** ROW 1:
	
	QWidget *widRow1 = new QWidget(widGridSetup);
	QHBoxLayout *layRow1 = new QHBoxLayout(widRow1);
	layRow1->setSpacing(LAY_SPACING);
  layRow1->setContentsMargins(0, 0, 0, 0);
	widRow1->setLayout( layRow1 );
	
	QLabel *lblGridSp = new QLabel("grid spacing:", widRow1);
	layRow1->addWidget(lblGridSp);
	
	QSpacerItem *sp1 = new QSpacerItem(10,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layRow1->addItem(sp1);
	
	lblGridSpX = new QLabel("x:", widRow1);
	layRow1->addWidget(lblGridSpX);
	
	spnSpacingX = new QDoubleSpinBox(widRow1);	
  spnSpacingX->setRange(1.0f, 5000.0f);
	spnSpacingX->setSingleStep( 1.0f );
  spnSpacingX->setDecimals  ( 2 );
  spnSpacingX->setValue( g->xSpacing );
  connect(spnSpacingX,SIGNAL(valueChanged(double)),this,SLOT(changeGridDblSpn(double)));
  spnSpacingX->setToolTip( "The spacing between points (in pixels). \n"
													 "If 'keep square' is off, this value applies along \n"
													 "the x axis only and a seperate box can be used to \n"
													 "give a different spacing along y.");
	layRow1->addWidget(spnSpacingX);
	
	layGridSetup->addWidget(widRow1);
	
	//** ROW 2:
	
	QWidget *widRow2 = new QWidget(widGridSetup);
	QHBoxLayout *layRow2 = new QHBoxLayout(widRow2);
	layRow2->setSpacing(LAY_SPACING);
  layRow2->setContentsMargins(0, 0, 0, 0);
	widRow2->setLayout( layRow2 );
	
	QLabel *lblSp2 = new QLabel("   ", widRow2);
	layRow2->addWidget(lblSp2);
	
	chkSameXYSpacing = new QCheckBox("keep square", widRow2);
  chkSameXYSpacing->setChecked( plug.sameXYSpacing );
  connect(chkSameXYSpacing,SIGNAL(clicked()),this,SLOT(changeGridChk()));
  chkSameXYSpacing->setToolTip( "If on: the spacing of lines along x and y will\n"
															  "be the same. In stereology the grid used is\n"
															  "typically square so it's suggested you leave\n"
															  "this ticked unless good reason not too.\n\n"
															  "RECOMMENDED VALUE: true");
	layRow2->addWidget( chkSameXYSpacing );
	
	QSpacerItem *sp2 = new QSpacerItem(10,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layRow2->addItem(sp2);
	
	lblGridSpY = new QLabel("y:", widRow2);
	layRow2->addWidget(lblGridSpY);
	
	spnSpacingY = new QDoubleSpinBox(widRow2);
  spnSpacingY->setRange(1.0f, 5000.0f);
	spnSpacingY->setSingleStep( 1.0f );
  spnSpacingY->setDecimals  ( 2 );
  spnSpacingY->setValue( g->ySpacing );
  connect(spnSpacingY,SIGNAL(valueChanged(double)),this,SLOT(changeGridDblSpn(double)));
  spnSpacingY->setToolTip( "The spacing between points along y (in pixels).");
  layRow2->addWidget(spnSpacingY);
	
	layGridSetup->addWidget(widRow2);
	
	//** ROW 3:
	
	QWidget *widRow3 = new QWidget(widGridSetup);
	QHBoxLayout *layRow3 = new QHBoxLayout(widRow3);
	layRow3->setSpacing(LAY_SPACING);
  layRow3->setContentsMargins(0, 0, 0, 0);
	widRow3->setLayout( layRow3 );
	
	QLabel *lblSp3 = new QLabel("   ", widRow3);
	layRow3->addWidget(lblSp3);
	
	chkShowEverySlice = new QCheckBox("project grid on every slice", widGridSetup);
  chkShowEverySlice->setChecked( plug.showGridEverySlice );
  QObject::connect(chkShowEverySlice,SIGNAL(clicked()),this,SLOT(changeGridChk()));
  chkShowEverySlice->setToolTip
	  ( "If true: the same grid will be projected/shown on EVERY section - even \n"
		  "those outside the z limits\n This option is useful if you want to do \n"
		  "stereology by counting points on paper (writing down the numbesr onto a sheet)\n"
		  "rather than using the plugin to mark and record the classification of each\n"
		  "point (and let the plugin count for you). Projecting a the same grid accross \n"
		  "all section is takes a lot less overhead (fewer lines are drawn), but if you \n"
			"go ahead and start counting points this option is set false \n"
		  "and automatically disappears."
	    "If false: the grid will be applied to certain sections only, as defined \n"
			"by 'z spacing' and the min and max limits for z. \n" );
	layRow3->addWidget(chkShowEverySlice);
	QSpacerItem *sp3 = new QSpacerItem(10,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layRow3->addItem(sp3);
	
	
	layGridSetup->addWidget( widRow3 );
	
	
	//** ROW 4:
	
	widRowZSpace = new QWidget(widGridSetup);
	QHBoxLayout *layRow4 = new QHBoxLayout(widRowZSpace);
	layRow4->setSpacing(LAY_SPACING);
  layRow4->setContentsMargins(0, 0, 0, 0);
	widRowZSpace->setLayout( layRow4 );
	
	QSpacerItem *sp4 = new QSpacerItem(10,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layRow4->addItem(sp4);
	
	QLabel *lblGridZSp = new QLabel("z spacing:", widRowZSpace);
	layRow4->addWidget(lblGridZSp);
	
	spnSpacingZ = new QSpinBox(widRowZSpace);	
  spnSpacingZ->setRange(1, 5000);
  spnSpacingZ->setFocusPolicy(Qt::NoFocus);
  spnSpacingZ->setValue( g->zSpacing );
  QObject::connect(spnSpacingZ,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnSpacingZ->setToolTip
	  ( "The spacing between grids along z (in pixels).\n\n"
		  "WARNING: Always set this value carefully!\n\n"
		 	"In cases where consecutive sections represent different area,\n"
			"you can set this 1, so there will be a grid on every slice between \n"
		  "the min and max z limit (inclusive).\n\n"
		  "When working with single tomographic volume, however, you should \n"
		  "not normally set Z spacing smaller than X/Y spacing distance. If\n"
		  "the spacing is too narrow, the imaged structures (and thus point values) \n"
		  "are likely to be almost identical between grids and this means you are \n"
		  "not getting a good random sample!\n" );
	layRow4->addWidget(spnSpacingZ);
	
	layGridSetup->addWidget(widRowZSpace);
	
	
	
	//** ROW 5:
	
	widPtsRow = new QWidget(widGridSetup);
	QHBoxLayout *layPtsRow = new QHBoxLayout(widPtsRow);
	layPtsRow->setSpacing(LAY_SPACING);
  layPtsRow->setContentsMargins(10, 10, 5, 5);
	widPtsRow->setLayout( layPtsRow );
	widPtsRow->setStyleSheet("background-color: rgb(255, 230, 110);");
	
	lblTotalPts = new QLabel("<b>summary:</b><br>cols = ?<br>rows = ?<br>points = ?", widPtsRow);
	lblTotalPts->setTextFormat( Qt::RichText );
	lblTotalPts->setWordWrap(true);
	lblTotalPts->setToolTip( "This label dynamically updates to show how <br>"
													 "many total points will be in your final grids. <br><br>"
													 "As a rough rule, counting usually takes under <br>"
													 "<b>1 hrs per 1000 points</b>, and for resonable <br>"
													 "accuracy about <b>1000-2000 points</b> is a good <br>"
													 "number to count for each '<b>sample</b>'/dataset <br>"
													  "<i>(eg: a single mouse in a condition A)</i>." );
	layPtsRow->addWidget( lblTotalPts );
	
	
	layGridSetup->addWidget(widPtsRow );
	
	
	//** ROW 6:
	
	chkChangeDefLimits = new QCheckBox("change grid limits", widGridSetup);
  chkChangeDefLimits->setChecked( plug.changeDefLimits );
  QObject::connect(chkChangeDefLimits,SIGNAL(clicked()),this,SLOT(changeGridChk()));
  chkChangeDefLimits->setToolTip
	  ( "If on: the limits of the grid will be shifted so that they fall within \n"
		  "the values you specify below (the boxes which appear when this is ticked). \n"
		  "If off: the minimum x, y and z limits are set to the boundaries of the \n"
		  "model. These should be the same as the reconstruction (image volume) \n"
		  "used to create the model.");
	layGridSetup->addWidget(chkChangeDefLimits);
	
	//** ROW 7:
	///## GRID LIMITS:
	
	QWidget *widRow7 = new QWidget(widGridSetup);
	QHBoxLayout *layRow7 = new QHBoxLayout(widRow7);
	layRow7->setSpacing(LAY_SPACING);
  layRow7->setContentsMargins(0, 0, 0, 0);
	widRow7->setLayout( layRow7 );
	
	grpLimits = new QGroupBox("grid limits:", widRow7);
  grpLimits->setFocusPolicy(Qt::NoFocus);
  grpLimits->setToolTip
	  ( "Points are placed left to right and then top to bottom such that the first "
			"point each grid placed 'x spacing' pixels right of the minimum x limit and "
			"'y spacing' pixels below the maximum y limit. The last column is placed "
			"no further than the maximum x limit and last row is no lower than the "
			"minimum y limits. The very first point is placed on the slice matching the "
			"minimum z limit, and then spaced out by 'z spacing' with the last on or "
			"before the minimum z limit value. <br><br>"
		  "Think of these values as a <b>guard area</b>.");
  layLimits = new QGridLayout(grpLimits);
  layLimits->setSpacing(LAY_SPACING);
  layLimits->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
	
	//** X:
	
	const int LIMIT_SPIN_WIDTH = 70;
	
	QLabel *lblXMin = new QLabel(" x: ", grpLimits);
	layLimits->addWidget(lblXMin, 0, 0, 1, 1);
	
	spnLimitXMin = new QSpinBox(grpLimits);
  spnLimitXMin->setRange(-100, 1000000);
  spnLimitXMin->setValue( g->xMin );
	spnLimitXMin->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLimitXMin,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnLimitXMin->setToolTip( "Minimum limit in x (in pixels)");
  layLimits->addWidget(spnLimitXMin, 0, 1, 1, 1);
	
	QLabel *lblXMax = new QLabel(" - ", grpLimits);
	layLimits->addWidget(lblXMax, 0, 2, 1, 1);	
	
	spnLimitXMax = new QSpinBox(grpLimits);
  spnLimitXMax->setRange(-100, 1000000);
  spnLimitXMax->setValue( g->xMax );
	spnLimitXMax->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLimitXMax,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnLimitXMax->setToolTip( "Maximum limit in x (in pixels)");
  layLimits->addWidget(spnLimitXMax, 0, 3, 1, 1);
	
	//** Y:
	
	QLabel *lblYMin = new QLabel(" y: ", grpLimits);
	layLimits->addWidget(lblYMin, 1, 0, 1, 1);
	
	spnLimitYMin = new QSpinBox(grpLimits);
  spnLimitYMin->setRange(-100, 1000000);
  spnLimitYMin->setValue( g->yMin );
	spnLimitYMin->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLimitYMin,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnLimitYMin->setToolTip( "Minimum limit in y (in pixels)");
  layLimits->addWidget(spnLimitYMin, 1, 1, 1, 1);
	
	QLabel *lblYMax = new QLabel(" - ", grpLimits);
	layLimits->addWidget(lblYMax, 1, 2, 1, 1);	
	
	spnLimitYMax = new QSpinBox(grpLimits);
  spnLimitYMax->setRange(-100, 1000000);
  spnLimitYMax->setValue( g->yMax );
	spnLimitYMax->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLimitYMax,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnLimitYMax->setToolTip( "Maximum limit in y (in pixels)");
  layLimits->addWidget(spnLimitYMax, 1, 3, 1, 1);
	
	//** Z:
	
	QLabel *lblZMin = new QLabel(" z: ", grpLimits);
	layLimits->addWidget(lblZMin, 2, 0, 1, 1);
	
	spnLimitZMin = new QSpinBox(grpLimits);
  spnLimitZMin->setRange(-100, 1000000);
  spnLimitZMin->setValue( g->zMin+1 );
	spnLimitZMin->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLimitZMin,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnLimitZMin->setToolTip( "Minimum limit in z (in voxels). The first grid will be \n"
													  "placed on this slice" );
  layLimits->addWidget(spnLimitZMin, 2, 1, 1, 1);
	
	QLabel *lblZMax = new QLabel(" - ", grpLimits);
	layLimits->addWidget(lblZMax, 2, 2, 1, 1);	
	
	spnLimitZMax = new QSpinBox(grpLimits);
  spnLimitZMax->setRange(-100, 1000000);
  spnLimitZMax->setValue( g->zMax+1 );
	spnLimitZMax->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLimitZMax,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnLimitZMax->setToolTip( "Maximum limit in z (in voxels). There will be no grids \n"
													  "placed after this slice" );
  layLimits->addWidget(spnLimitZMax, 2, 3, 1, 1);
	
	layRow7->addWidget(grpLimits);
	
	QSpacerItem *sp5 = new QSpacerItem(0,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layRow7->addItem(sp5);
	
	layGridSetup->addWidget(widRow7);
	
	//** SPACER (LIMIT SIZE OF GROUP BOX):
	
	QSpacerItem *spV = new QSpacerItem(10,0,QSizePolicy::Expanding,QSizePolicy::Expanding);
	layGridSetup->addItem(spV);
		
	layGridScroll->addWidget( widGridSetup );
	
	
	
	//** ROW 8:
	
	chkSubsampleRect = new QCheckBox("show subsample rectangles", widGridSetup);
  chkSubsampleRect->setChecked( false );
  QObject::connect(chkSubsampleRect,SIGNAL(clicked()),this,SLOT(changeGridChk()));
  chkSubsampleRect->setToolTip
	  ( "If on: subsample rectangles or will be draw over your grid using \n"
	    "the sizes you specify. When you finalize the grid and categories, \n"
		  "you will get an option to automatically omit or black out points outside \n"
		  "these boxes thereby allowing you to subsample in XY. \n"
	    "This feature is handy if you have a gigantic image in X and Y but still \n"
	    "want to use a fine grid (without creating an unreasonable number of points).\n\n"
		  "By default, the subsample rectangles will be drawn as forbidden squares \n"
		  "but you can change this under 'Options > Display Options'" );
	layGridSetup->addWidget(chkSubsampleRect);
	
	//** ROW 9:
	///## GRID LIMITS:
	
	QWidget *widRow9 = new QWidget(widGridSetup);
	QHBoxLayout *layRow9 = new QHBoxLayout(widRow9);
	layRow9->setSpacing(LAY_SPACING);
  layRow9->setContentsMargins(0, 0, 0, 0);
	widRow9->setLayout( layRow9 );
	
	grpRects = new QGroupBox("rectangle size and spacing:", widRow9);
  grpRects->setFocusPolicy(Qt::NoFocus);
  grpRects->setToolTip( "Use these spin boxes to set the side length and gap size "
											  "for 'subsample rectangles' (used to subsample points) "
											  "along X and Y.");
  layRects = new QGridLayout(grpRects);
  layRects->setSpacing(LAY_SPACING);
  layRects->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
	
	//** SPAN:
	
	QLabel *lblXSp = new QLabel(" side: ", grpRects);
	lblXSp->setMaximumWidth(40);
	layRects->addWidget(lblXSp, 0, 0, 1, 1);
	
	spnRectXSpan = new QSpinBox(grpRects);
  spnRectXSpan->setRange(10, 1000000);
	spnRectXSpan->setSingleStep(10);
  spnRectXSpan->setValue( g->rXSpan );
	spnRectXSpan->setMaximumWidth(60);
  QObject::connect(spnRectXSpan,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnRectXSpan->setToolTip( "The size of the subsample rectangle along X (in pixels)");
  layRects->addWidget(spnRectXSpan, 0, 1, 1, 1);
	
	QLabel *lblYSp = new QLabel(" x ", grpRects);
	layRects->addWidget(lblYSp, 0, 2, 1, 1);	
	
	spnRectYSpan = new QSpinBox(grpRects);
  spnRectYSpan->setRange(10, 1000000);
	spnRectYSpan->setSingleStep(10);
  spnRectYSpan->setValue( g->rYSpan );
	spnRectYSpan->setMaximumWidth(60);
  QObject::connect(spnRectYSpan,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnRectYSpan->setToolTip( "The size of the subsample rectangle along Y (in pixels)");
  layRects->addWidget(spnRectYSpan, 0, 3, 1, 1);
	
	//** GAP:
	
	QLabel *lblXGap = new QLabel(" gap: ", grpRects);
	lblXGap->setMaximumWidth(40);
	layRects->addWidget(lblXGap, 1, 0, 1, 1);
	
	spnRectXGap = new QSpinBox(grpRects);
  spnRectXGap->setRange(10, 1000000);
	spnRectXGap->setSingleStep(10);
  spnRectXGap->setValue( g->rXGap );
	spnRectXGap->setMaximumWidth(60);
  QObject::connect(spnRectXGap,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnRectXGap->setToolTip( "The gap between subsample rectangles along X (in pixels)");
  layRects->addWidget(spnRectXGap, 1, 1, 1, 1);
	
	QLabel *lblYGap = new QLabel(" x ", grpRects);
	layRects->addWidget(lblYGap, 1, 2, 1, 1);	
	
	spnRectYGap = new QSpinBox(grpRects);
  spnRectYGap->setRange(10, 1000000);
	spnRectYGap->setSingleStep(10);
  spnRectYGap->setValue( g->rYGap );
	spnRectYGap->setMaximumWidth(60);
  QObject::connect(spnRectYGap,SIGNAL(valueChanged(int)),this,SLOT(changeGridSpn(int)));
  spnRectYGap->setToolTip( "The gap between subsample rectangles along Y (in pixels)");
  layRects->addWidget(spnRectYGap, 1, 3, 1, 1);
	
	//** KEEP LOCKED + DISPLAY:
	
	chkLockSqXY = new QCheckBox("lock x/y", widGridSetup);
  chkLockSqXY->setChecked( true );
  QObject::connect(chkLockSqXY,SIGNAL(clicked()),this,SLOT(changeGridChk()));
	layRects->addWidget(chkLockSqXY, 2, 0, 1, 3);
	
	layRow9->addWidget(grpRects);
	
	layGridSetup->addWidget(widRow9);
	
	
	
	//** ROW 10:
	//## GRID DISPLAY OPTIONS
	
	grpGridDisplayOpts = new QGroupBox("display options:", widGridSetup );
  grpGridDisplayOpts->setFocusPolicy(Qt::NoFocus);
  
  QGridLayout *layGridOpts = new QGridLayout(grpGridDisplayOpts);
  layGridOpts->setSpacing(LAY_SPACING);
  layGridOpts->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
	grpGridDisplayOpts->setLayout( layGridOpts );
	
	
	QLabel *lblSySize = new QLabel("symbol size: ", grpGridDisplayOpts);
  lblSySize->setFocusPolicy(Qt::NoFocus);
  lblSySize->setToolTip( "The size of crosshairs or line ends in pixels");
  layGridOpts->addWidget(lblSySize, 0, 0, 1, 1);
	
	spnGridSymbolSize = new QSpinBox(grpGridDisplayOpts);
	spnGridSymbolSize->setFocusPolicy(Qt::NoFocus);
  spnGridSymbolSize->setRange(1, 2000);
  spnGridSymbolSize->setValue( plug.gridSymbolSize );
	spnGridSymbolSize->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnGridSymbolSize,SIGNAL(valueChanged(int)),
									 this,SLOT(changeGridSpn(int)));
  spnGridSymbolSize->setToolTip
	  ( "The size (in pixel) of the grid points if 'grid setting'\n"
			"is set to either 'crosshairs' or 'arrows'." );
  layGridOpts->addWidget(spnGridSymbolSize, 0, 1, 1, 2);
	
	
	QLabel *lblLineW = new QLabel("line width: ", grpGridDisplayOpts);
  lblLineW->setFocusPolicy(Qt::NoFocus);
  lblLineW->setToolTip( "The thickness of the lines");
  layGridOpts->addWidget(lblLineW, 1, 0, 1, 1);
	
	spnLineWidth = new QSpinBox(grpGridDisplayOpts);
	spnLineWidth->setFocusPolicy(Qt::NoFocus);
  spnLineWidth->setRange(1, 50);
  spnLineWidth->setValue( plug.gridLineThickness );
	spnLineWidth->setMaximumWidth(LIMIT_SPIN_WIDTH);
  QObject::connect(spnLineWidth,SIGNAL(valueChanged(int)),
									 this,SLOT(changeGridSpn(int)));
  spnLineWidth->setToolTip( "The thickness of the grid lines (in pixel) \n"
													  "Default is 1, but may want to increase on \n"
													  "monitors with high resolution." );
  layGridOpts->addWidget(spnLineWidth, 1, 1, 1, 2);
	
	QColor gridColor = QColor( plug.gridColorR, plug.gridColorG, plug.gridColorB );
	colGridColor = new ColorButton( gridColor, this );
	QObject::connect(colGridColor,SIGNAL(released()),this,SLOT(changeGridChk()));
	colGridColor->setToolTip( "The color of the main grid lines." );
	layGridOpts->addWidget(colGridColor, 1, 2, 1, 1);
	
	
	chkShowDashes = new QCheckBox("show dashes", widGridSetup);
  chkShowDashes->setChecked( plug.showDashes );
  QObject::connect(chkShowDashes,SIGNAL(clicked()),this,SLOT(changeGridChk()));
  chkShowDashes->setToolTip( "If on, little dashes will be shown so you can \n"
														 "see the location of points when the grid type \n"
														 "is set to straight lines" );
	layGridOpts->addWidget(chkShowDashes, 2, 0, 1, 3);
	
	
	QLabel *lblRect = new QLabel("rect display: ", grpGridDisplayOpts);
  lblRect->setFocusPolicy(Qt::NoFocus);
  lblRect->setToolTip( "How to display/present subsample rectangles");
  layGridOpts->addWidget(lblRect, 3, 0, 1, 1);
	
	cmbSubRectDisplay = new QComboBox(grpGridDisplayOpts);
  cmbSubRectDisplay->setFocusPolicy(Qt::NoFocus);
  cmbSubRectDisplay->addItem("forbidden sqs");
  cmbSubRectDisplay->addItem("normal rects");
  cmbSubRectDisplay->addItem("dotted");
	cmbSubRectDisplay->addItem("grid");
  cmbSubRectDisplay->setCurrentIndex( plug.subRectDisplayOpt );
	connect(cmbSubRectDisplay,SIGNAL(activated(int)),this,SLOT(changeGridSpn(int)));
	layGridOpts->addWidget(cmbSubRectDisplay, 3, 1, 1, 2);
	
	
	layGridScroll->addWidget( grpGridDisplayOpts );
	
	
	//** PIN-TO-TOP BUTTON
	
	QToolButton *toolBut = new QToolButton(this);
  toolBut->setCheckable(true);
  toolBut->setFocusPolicy(Qt::NoFocus);
  QIcon iconSet;
  iconSet.addPixmap(QPixmap((const char **)pegged), QIcon::Normal, QIcon::On);
  iconSet.addPixmap(QPixmap((const char **)unpegged), QIcon::Normal, QIcon::Off);
  toolBut->setIcon(iconSet);
  toolBut->setChecked(false);
  QSize hint = toolBut->sizeHint();
  toolBut->setFixedWidth(hint.width());
  toolBut->setFixedHeight(hint.height());
  connect(toolBut, SIGNAL(toggled(bool)), this, SLOT(keepOnTop(bool)));
  toolBut->setToolTip("Keep stereology window on top");
	layGridOpts->addWidget(toolBut, 0, 2, 1, 1);
	
	
	//** PUT IN SCROLL AREA:
	
  scrollAreaGridSetup = new QScrollArea( this );
  scrollAreaGridSetup->setBackgroundRole(QPalette::Midlight);
  scrollAreaGridSetup->setWidget( widGridScroll );
	
	
	layGridSetup_tab0->addWidget( scrollAreaGridSetup );
	
	
	
	//** BUTTONS BELOW SCROLL AREA:
	
	QWidget *widGridBtnRow = new QWidget(widGridSetup);
	QHBoxLayout *layGridBtnRow = new QHBoxLayout(widGridBtnRow);
	layGridBtnRow->setSpacing(LAY_SPACING);
  layGridBtnRow->setContentsMargins(0, 0, 0, 0);
	widGridBtnRow->setLayout( layGridBtnRow );
	
	chkShowGridDisplayOpts = new QCheckBox("show display options", widGridBtnRow);
  chkShowGridDisplayOpts->setChecked( plug.changeDefLimits );
  QObject::connect(chkShowGridDisplayOpts,SIGNAL(clicked()),this,
									 SLOT(changeShowGridDisplayChk()));
  chkShowGridDisplayOpts->setToolTip
	  ( "Display a small number of options for modifying\n"
		  "the look and feel of the grid. For more options\n"
		  "click 'Options > More Settings'.");
	layGridBtnRow->addWidget(chkShowGridDisplayOpts);
	
	QSpacerItem *spGB = new QSpacerItem(10,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layGridBtnRow->addItem(spGB);
	
  btnStartStereology = new QPushButton("Finalize Grid", widGridBtnRow);
	setBold(btnStartStereology, true);
  connect(btnStartStereology, SIGNAL(clicked()), this, SLOT(finalizeGrid()));
  btnStartStereology->setToolTip("Once you've finished setting up and 'testing out'/n"
																 "your grid, click this to finalize the grid settings,\n"
																 "create categories, then start stereology by\n"
																 "counting/classifying points!        \n\n"
																 "All grid settings and categories are saved to a\n"
																 "new set of objects.");
  layGridBtnRow->addWidget(btnStartStereology);
	
	layGridSetup_tab0->addWidget( widGridBtnRow );
	
	
	
	
	
	
//## CREATE TAB FOR SELETING CATEGORIES:
	
	widCategories_tab1 = new QWidget(this);
	QVBoxLayout *layCat = new QVBoxLayout(widCategories_tab1);
	layCat->setSpacing(LAY_SPACING);
  layCat->setContentsMargins(10, 10, 5, 5);
	widCategories_tab1->setLayout( layCat );
	
  //## CREATE HEADER:
  
  QWidget *widHeader = new QWidget(widCategories_tab1);
  QHBoxLayout *layHeader = new QHBoxLayout( widHeader );
  layHeader->setSpacing(0);
  layHeader->setContentsMargins(10+2, 0, 0+2, 0);
  widHeader->setLayout( layHeader );
	
  QLabel *lblNum = new QLabel( "<i>Shortcut<i> &nbsp; " );
  lblNum->setToolTip( "....");
  lblNum->setFixedWidth(15+20+20+4+4);
  lblNum->setAlignment(Qt::AlignRight);
  lblNum->setFont( smallFont );
  
  QLabel *lblObjName = new QLabel( "Object Name" );
  lblObjName->setToolTip( ".");
  lblObjName->setMinimumWidth(200+4);
  lblObjName->setFont( smallFont );
  
  layHeader->addWidget( lblNum );
  layHeader->addWidget( lblObjName );
  
	layCat->addWidget(widHeader);
		
  //## CREATE WIDGET FOR LIST OF CATEGORIES:
  
  widList = new QWidget(this);
  layList = new QVBoxLayout(widList);
  layList->setSpacing(LAY_SPACING);
  layList->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  widList->setLayout( layList );
  
	//## CREATE SCROLL AREA FOR LIST AND BUTTONS:
	
  scrollArea = new QScrollArea( this );
  scrollArea->setBackgroundRole(QPalette::Midlight);
  scrollArea->setWidget( widList );
	
	layCat->addWidget(scrollArea);
	
	//## CREATE BUTTONS FOR ADDING TO CATEGORIES:
	
	widCatListOpns = new QWidget(widCategories_tab1);
	QHBoxLayout *layCatListOpns = new QHBoxLayout(widCatListOpns);
	layCatListOpns->setSpacing(LAY_SPACING);
  layCatListOpns->setContentsMargins(0, 0, 0, 0);
	widCatListOpns->setLayout( layCatListOpns );
	
	chkPtChecked = new QCheckBox("point checked", widCatListOpns);
  chkPtChecked->setChecked( false );
	chkPtChecked->setVisible( false );
	chkPtChecked->setToolTip( "Should be ticked for any point which you've \n"
													 "checked by clicking all the correct categories.");
	connect(chkPtChecked,SIGNAL(clicked()),this,SLOT(changePtCheckedClicked()));
	layCatListOpns->addWidget(chkPtChecked);
	
  btnAddCat = new QPushButton("+", widCatListOpns);
  connect(btnAddCat, SIGNAL(clicked()), this, SLOT(addCategory()));
	btnAddCat->setMaximumWidth(40);
  btnAddCat->setToolTip("Click to add another category.");
  layCatListOpns->addWidget(btnAddCat);
	
	btnDelCat = new QPushButton("-", widCatListOpns);
  connect(btnDelCat, SIGNAL(clicked()), this, SLOT(deleteCategory()));
	btnDelCat->setMaximumWidth(40);
  btnDelCat->setToolTip("Removes the last category.");
  layCatListOpns->addWidget(btnDelCat);
	
	btnCatOpts = new QPushButton(" ", widCatListOpns);
	btnCatOpts->setFixedSize(16,16);
  btnCatOpts->setToolTip("Load or save list of categories.");
  layCatListOpns->addWidget(btnCatOpts);
	
	QSpacerItem *spLO = new QSpacerItem(10,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layCatListOpns->addItem(spLO);
	
  btnStartCount = new QPushButton("Finalize Categories", widCatListOpns);
	setBold( btnStartCount, true );
  connect(btnStartCount, SIGNAL(clicked()), this, SLOT(startCounting()));
  btnStartCount->setToolTip("Start counting points.");
  layCatListOpns->addWidget(btnStartCount);
	
	layCat->addWidget( widCatListOpns );
  
	
	
//## CREATE TAB WIDGET:
	
	tabWidget = new QTabWidget();
	tabWidget->addTab( widGridSetup_tab0, "(1) Setup Grid" );
	tabWidget->addTab( widCategories_tab1, "(2) Classify Points" );
	QObject::connect(tabWidget,SIGNAL(currentChanged(int)),this,
									 SLOT(changeTabSelected(int)));
	
  //## CREATE ROW OF BUTTONS TO CHANGE POINTS:
  
  QWidget *widgetPtBtns = new QWidget(this);
  layPtBtns = new QHBoxLayout(widgetPtBtns);
  layPtBtns->setSpacing(LAY_SPACING);
  layPtBtns->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  widgetPtBtns->setLayout(layPtBtns);
  
	QLabel *lblPt = new QLabel( "Point:", widgetPtBtns );
	lblPt->setFixedWidth( 40 );
	lblPt->setMaximumWidth( 40 );
  lblPt->setTextFormat( Qt::RichText );
	lblPt->setToolTip( "Shows what point is selected\n\n"
										 "TIP: Use arrow keys or the first (pan) mouse button to\n"
										 "     quickly navigate the grid and select a particular point");
	layPtBtns->addWidget(lblPt);
	
	spnSelPt = new QSpinBox(widgetPtBtns);
  spnSelPt->setRange(1, 10000000);
  spnSelPt->setFocusPolicy(Qt::ClickFocus);		// allow user to change by typing numbers
  spnSelPt->setValue( g->currPtIdx );
  QObject::connect(spnSelPt,SIGNAL(valueChanged(int)),this,SLOT(changeCurrPtSpn(int)));
  spnSelPt->setToolTip( "Use this to select a point\n\n"
											  "TIP: Use the [left], [right], [up] and [down] arrow \n"
											  "     keys to quickly change points");
  layPtBtns->addWidget(spnSelPt);
	
	lblMaxPts = new QLabel( "/ -", widgetPtBtns );
  lblMaxPts->setTextFormat( Qt::RichText );
  lblMaxPts->setContentsMargins( 5,0,3,0 );
  lblMaxPts->setToolTip( "Total number of points in the grid you setup." );
	layPtBtns->addWidget(lblMaxPts);
	
	QSpacerItem *spPB = new QSpacerItem(0,0,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layPtBtns->addItem(spPB);
	
	btnPrev = new QPushButton("<", widgetPtBtns);
	btnPrev->setAutoRepeat( true );				// lets user hold button down
	btnPrev->setMaximumWidth( 40 );
  connect(btnPrev, SIGNAL(clicked()), this, SLOT(goToPrevUncheckedPt()));
  btnPrev->setToolTip( "Go to the closest unchecked point before the selected one.\n"
											 "You can also achieve this by pressing:   [shift]+[left]");
  layPtBtns->addWidget(btnPrev);
	
  btnNext = new QPushButton("Next >", widgetPtBtns);
	btnNext->setAutoRepeat( true );				// lets user hold button down
	btnNext->setMaximumWidth( 100 );
  connect(btnNext, SIGNAL(clicked()), this, SLOT(goToNextUncheckedPt()));
	btnNext->setToolTip( "Go to the next unchecked point after the current.  [enter]\n\n"
											 "You can also achieve this by pressing:   [shift]+[right]\n\n"
											 "TIP: Click 'Options > Check Progress' to see how many points\n"
											 "     are unchecked and estimated time remaning" );
  layPtBtns->addWidget(btnNext);
	
	
	//## CREATE ROW OF EXTRA BUTTONS TO CHANGE POINTS:
  
  QWidget *widgetExBtns = new QWidget(this);
  layExBtns = new QHBoxLayout(widgetExBtns);
  layExBtns->setSpacing(LAY_SPACING);
  layExBtns->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  widgetExBtns->setLayout(layExBtns);
  
	lblSelGrid = new QLabel( "Grid: -/-", widgetExBtns );
  lblSelGrid->setTextFormat( Qt::RichText );
	lblSelGrid->setFixedWidth( 120 );
  lblSelGrid->setToolTip( "Shows what grid is selected (numbered from bottom) \n\n"
												  "TIP: use [.] and [,] shortcut keys to change grid" );
	layExBtns->addWidget(lblSelGrid);
	
	QSpacerItem *spEB = new QSpacerItem(0,0,QSizePolicy::Expanding,QSizePolicy::Minimum);
	layExBtns->addItem(spEB);
		
  btnRandom = new QPushButton("Random Pt", widgetExBtns);
  connect(btnRandom, SIGNAL(clicked()), this, SLOT(goToRandomPos()));
  btnRandom->setToolTip( "Selects a new point in a random location using the \n"
												 "grid limits and z spacing specified.");
  layExBtns->addWidget(btnRandom);
  
	btnAddRandPts = new QPushButton("Random Pts+", widgetExBtns);
  connect(btnAddRandPts, SIGNAL(clicked()), this, SLOT(addRandomPts()));
  btnAddRandPts->setToolTip( "Allows you to add a number of random points \n"
														 "using the parameters you provide. Once added \n"
														 "you can then classify these points.");
	btnAddRandPts->setVisible( false );
  layExBtns->addWidget(btnAddRandPts);
	
  btnPaint = new QPushButton("Pt Painter", widgetExBtns);
	btnPaint->setCheckable( true );
  connect(btnPaint, SIGNAL(clicked()), this, SLOT(togglePaintBtn()));
  btnPaint->setToolTip( "When selected, can be used to paint multiple \n"
												 "points within the 'paint radius' with the \n"
												 "same category as the point selected." );
  btnPaint->setVisible(false);
	layExBtns->addWidget(btnPaint);
	
	btnIntercept = new QPushButton("Intercept", widgetExBtns);
	btnIntercept->setCheckable( true );
  connect(btnIntercept, SIGNAL(clicked()), this, SLOT(toggleInterceptBtn()));
  btnIntercept->setToolTip( "When selected, can be used to paint multiple \n"
											 "points within the 'paint radius' with the \n"
											 "same category as the point selected." );
  btnIntercept->setVisible(false);
	layExBtns->addWidget(btnIntercept);
	
  btnOptions = new QPushButton("Options", widgetExBtns);
	btnOptions->setMaximumWidth( 100 );
  btnOptions->setToolTip( "Contains options to check progress, analyse results, \n"
												  "change plugin settings and much more.");
  layExBtns->addWidget(btnOptions); 
	
	
	
  //## ADD THE WIDGES TO THE MAIN LAYOUT, GENERATE THE OBJECT LIST AND RESIZE: 
  
	mLayout->addWidget(tabWidget);
  mLayout->addWidget(widgetPtBtns);
  mLayout->addWidget(widgetExBtns);
  
  
  this->adjustSize();
  this->setMinimumWidth(300);
  
  
  //## CREATE "ACTIONS" CONTEXT MENU AND CONNECTIONS:
  
  QMenu *optionsMenu = new QMenu(this);
	addAction( optionsMenu, SLOT(setSpacingUsingUnits()),
						"Set/view grid spacing in units",
						"Allows you to view and/or set grid spacing in whatever metric \n"
						"unit is specified in the model header (typically 'nm')." );
	addAction( optionsMenu, SLOT(reloadGridFromImodModel()), "Load/reload grid from model",
						"Will search the currently open IMOD model for 'STEREOLOGY' objects<br>"
						"and if it find any, gives you options to load the grid settings,<br>"
						"categories and points from one of these grids." );
	addAction( optionsMenu, SLOT(startNewGrid()), "Start new grid (wizard)",
						"Allows you to start a new grid using a wizard to decide<br>"
						"what type of grid and method you want to use." );
	addAction( optionsMenu, SLOT(modifyExistingGridViaObjectLabels()),
						"Modify grid setup/labels",
						"Allows you to modify a grid which has already been finalized <br>"
						"by changing object labels.... something you should ONLY do if <br>"
						"you have read the documentation and know what you're doing." );
	optionsMenu->addSeparator();
	addAction( optionsMenu, SLOT(applyMask()), "Apply (contour) mask to points",
						"Allows you to change the classification of stereology\n"
						"points inside and/or outside a set of closed contours\n"
						"within a closed (non-stereology) object." );
	addAction( optionsMenu, SLOT(applyMasks()), "Apply batch masks",
						"Allows you to change the classification of all stereology \n"
						"points inside several (closed contour) objects at once. \n"
						"Unlike the option above, this function checks for contour 'holes' \n"
						"(contours inside contours), but has far fewer options and \n"
						"assumes that each point should have only one category." );
	addAction( optionsMenu, SLOT(applyIntercepts()), "Add intersection points",
						"Allows you to automatically add 'intersection' points where\n"
						"test lines intersect closed or open contours within your model. \n"
						"NOTE: Only applicable if you have lines projected." );
	addAction( optionsMenu, SLOT(applyRulesToChangePts()), "Change point values",
						"Allows you to apply a set of *rules* to change the classification\n"
						"of points which meet a set criteria. As one example: you could\n"
						"change only unchecked points on the current grid/section to be\n"
						"category 1 and checked." );
	addAction( optionsMenu, SLOT(validatePoints()), "Validate point values",
						"Will validate your grid points by finding any unchecked points\n"
						"without a category, or points with more than one category (in cases\n"
						"where the 'allow multiple categories' option was not checked)." );
	optionsMenu->addSeparator();
	optionsMenu->addSeparator();
	addAction( optionsMenu, SLOT(checkProgress()), "Check progress",
						"Quickly shows how many points you have checked \n"
						"and how many points fall in each category." );
	addAction( optionsMenu, SLOT(calculateResults()), "Calculate results!",
						"Calculates and outputs results from your selected point-counting\n"
						"stereology grids.\n\n"
						"Results can be output in various formats for easy copy and paste\n"
						"easily into a spreadsheet, and you can also choose to omit certain\n"
						"points and/or categories from calculation" );
	optionsMenu->addSeparator();
	addAction( optionsMenu, SLOT(gridSettings()), "Grid Settings (defaults)",
						"Lets you modify settings default values used to setup new grids." );
	addAction( optionsMenu, SLOT(displayOptions()), "Display Options",
						"Lets you modify settings associated with this plugin including default\n"
						"grid appearance options." );
  btnOptions->setMenu( optionsMenu );
  
	
	QMenu *catOptionsMenu = new QMenu(this);
	addAction( catOptionsMenu, SLOT(loadCatNamesViaDialog()), "Load categories from file",
						"Loads categories from a 'standard names and colors .csv' file." );
	addAction( catOptionsMenu, SLOT(loadDefaultCatNames()), "Load default categories",
						"Loads default categories from '<b>default_stereology_categories.csv</b>'<br>"
						"in imod's plugin directory." );
	catOptionsMenu->addSeparator();
	addAction( catOptionsMenu, SLOT(saveCatNamesViaDialog()), "Save categories to file",
						"Saves the categories above to a 'standard names and colors .csv'<br>"
						"file (prompted by a dialog box)." );
	addAction( catOptionsMenu, SLOT(saveCatNamesAsDefault()), "Save categories as default",
						"Saves the categories above to '<b>default_stereology_categories.csv</b>'<br>"
						"in imod's plugin directory, so they will become the default categories<br>"
						"each time you start a new grid." );
	catOptionsMenu->addSeparator();
	addAction( catOptionsMenu, SLOT(loadCatNamesForCounting()),
						"Setup categories for counting intersections",
						"Has options to setup categories in the form '0_INTERSECTIONS',\n"
						"'1_INTERSECTIONS','3_INTERSECTIONS' and so on." );
	btnCatOpts->setMenu( catOptionsMenu );
	
	
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
	
	
	//## CREATE COMPLETER:
	
  completer = new QCompleter( plug.wordList, this);
  completer->setCaseSensitivity( Qt::CaseInsensitive );
	
	
	//## UPDATE WIDGETS:
	
	updateGridFromGridGui(true, true, plug.jumpToGridOnUpdate);
}



//------------------------
//-- Used to help set "widList" to a size appropriate
//-- for the width of the dialog.

void Stereology::resizeEvent ( QResizeEvent * event )
{
   widList->setGeometry( 0, 0, this->width()-40, widList->height() );  
}




//############################################################
//## DRAWING METHOD:





//------------------------
//-- Accesses one of the extra objects "plug.extraObjGrid" and draws
//-- grid lines according to the values of "plug.gridDisplayOpt"
//-- and "plug.gridSymbolSize". By default these gridlines are red
//-- but the user can change this color.

bool Stereology::drawGridObject( bool redraw )
{
	GridSetObj *g = getCurrGridSetObj();
	
  //## CLEAR EXTRA OBJECTS:
  
	Iobj *xobjG = ivwGetAnExtraObject(plug.view, plug.extraObjGrid);
  
  if ( !plug.window || !xobjG )
    return 0;
  
	imodObjectSetValue(xobjG, IobjFlagExtraInModv, (plug.showGridInModelView)?1:0);
	imodObjectSetValue(xobjG, IobjLineWidth2, plug.gridLineThickness);
	setObjColor(xobjG, plug.gridColorR, plug.gridColorG, plug.gridColorB);
	ivwClearAnExtraObject( plug.view, plug.extraObjGrid );
  
  
  //## GET Z VALUE AND SIZE OF SCREEN PIXELS:
  
	float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
	int noZap = ivwGetTopZapZoom(plug.view, &zapZoom); 
	float sc = fDiv( 1.0f, zapZoom);   // tomogram distance for one screen pixel 
	
	float ix, iy;
	int iz;
	ivwGetTopZapCenter(plug.view, ix, iy, iz);
	
	bool isGridOnCurrSlice = g->isGridOnSlice( iz );
	
	
	//## IF ON GRID SETTINGS TAB: SHOW GRID LIMITS WITH STIPPLED LINES:
	
	if( tabWidget->currentIndex() == 0 || g->allowRandomPts() )
	{
		for (int z=g->zMin; z<=g->zMax; z+=g->zSpacing)
		{
			cont_addLineToObj( xobjG, g->xMin,g->yMin,z, g->xMin,g->yMax,z, false, true);
			cont_addLineToObj( xobjG, g->xMax,g->yMin,z, g->xMax,g->yMax,z, false, true);
			int nextXPos = (g->cols+1)*g->xSpacing + g->xMin;
			cont_addLineToObj( xobjG, nextXPos,g->yMax,z, nextXPos,g->yMax+100,z, false,false);
			
			cont_addLineToObj( xobjG, g->xMin,g->yMax,z, g->xMax,g->yMax,z, false, true);
			cont_addLineToObj( xobjG, g->xMin,g->yMin,z, g->xMax,g->yMin,z, false, true);
			int nextYPos = -(g->rows+1)*g->ySpacing + g->yMax;
			cont_addLineToObj( xobjG, g->xMin,nextYPos,z, g->xMin-100,nextYPos,z, false,false);
		}
	}
	
	
	//## IF DESIRED: DRAW GRID LABELS ALONG THE TOP AND LEFT SIDE:
	
	if( plug.gridLabels != GL_OFF || plug.gridDisplayOpt == GD_OFF )
	{
		float fontSize = 12 * sc;
		float off = fontSize * 2;
		
		Ipoint textPos;
		setPt( &textPos, (g->xMin - off), (g->yMax + off), g->zMin );
		
		cont_generateTextAreaAsConts( xobjG, "0", textPos, fontSize,
																 TA_RIGHT, TV_BOTTOM, true, 4 );
		
		//## FOR EACH COL: DRAW LABEL ALONG TOP:
		
		int xInterval = (int)MAX( fDiv(4*fontSize, g->xSpacing), 1.0f);
		for(int x=1; x<g->cols+1; x++)	// for each col: draw label along top
		{
			if( x % xInterval != 0 )
				continue;
			textPos.x = (x * g->xSpacing) + g->xMin;
			string text = toString( x );
			
			if( plug.gridLabels == GL_NUMS )
				text = toString( x );
			else if( plug.gridLabels == GL_LETTERS_NUMS )
				text = numberToAlphabetChars( x, true ).toStdString();
			
			cont_generateTextAreaAsConts( xobjG, text, textPos, fontSize,
																	 TA_CENTER, TV_BOTTOM, true, 4 );
		}
		
		setPt( &textPos, (g->xMin - off), 0, g->zMin );
		
		//## FOR EACH ROW: DRAW LABEL ALONG LEFT:
		
		int yInterval = (int)MAX( fDiv(4*fontSize, g->ySpacing), 1.0f);
		for(int y=1; y<g->rows+1; y++)	// for each row: draw label along left
		{
			if( y % yInterval != 0 )
				continue;
			textPos.y = -(y * g->ySpacing) + g->yMax;
			string text = toString( y );
			cont_generateTextAreaAsConts( xobjG, text, textPos, fontSize,
																	 TA_RIGHT, TV_CENTER, true, 4 );
		}
		
		//## LABEL ALL TEXT CONTOURS TO SHOW EVERY SLICE:
		
		for( int c=0; c<csize(xobjG); c++  )
			imodContourSetFlag( getCont(xobjG,c), ICONT_DRAW_ALLZ, 1 );
	}
	
	
	//## IF COUNTING NOT STARTED AND NO GRID ON CURRENT SLICE: SHOW TEXT
	
	if( g->countingStarted == false && !isGridOnCurrSlice )
	{
		float fontSize = 14 * sc;
		Ipoint textPos;
		setPt( &textPos, ix, iy, (float)iz );
		string text = "no grid on this slice";
		cont_generateTextAreaAsConts( xobjG, text, textPos, fontSize, TA_CENTER, TV_CENTER,
																  true, 4 );
	}
	
	
	
  //## DETERMINE HOW MANY CONTOURS MUST BE DRAWN AND IF TOO MANY
	//## SHOW A WARNING:
  
	float off = plug.gridSymbolSize;
	float halfXSpacing    = g->xSpacing / 2.0f;
	float quarterXSpacing = g->xSpacing / 4.0f;
	int zMin = g->zMin;
	int zMax = g->zMax;
	
	int firstContGridlines;
	bool showUniformGridAllSlices = plug.showGridEverySlice && !g->countingStarted;
	
	if(showUniformGridAllSlices)					// if we want to show gridlines every slice:
	{
		zMax = zMin;														// we only need to draw lines once
		firstContGridlines = csize( xobjG )-1;	// then from here on set contours visible
	}																					//  every slice
	
	
	int  dOpt        = plug.gridDisplayOpt;		// display option
	long approxConts = g->ptsPerGrid*2;				// appoximate number of contours
	
	
	bool needsDashes = plug.showDashes && ( dOpt>=GD_VERTLINES && dOpt<=GD_DOWNDIAGONAL );
	
	if      (dOpt==GD_RAND       )		approxConts = (g->cols + g->rows) * 2;
	else if (dOpt==GD_LINES      )		approxConts = g->cols + g->rows;
	else if (dOpt==GD_VERTLINES  )		approxConts = g->cols;
	else if (dOpt==GD_HORZLINES  )		approxConts = g->rows;
	else if (dOpt==GD_UPDIAGONAL )		approxConts = g->ptsPerGrid;
	else if (dOpt==GD_DOWNDIAGONAL )	approxConts = g->ptsPerGrid;
	else if (dOpt==GD_LINEPAIRS	 )		approxConts = g->ptsPerGrid * 4;
	else if (dOpt==GD_CYCLOIDS	 )		approxConts = g->ptsPerGrid * (ARCPOINTS+3);
	else if (dOpt==GD_CYCLOIDS2  )		approxConts = g->ptsPerGrid * (ARCPOINTS+3);
	else if (dOpt==GD_CYCLOIDS3  )		approxConts = g->ptsPerGrid * (2*ARCPOINTS+3);
	
	if(needsDashes)
		approxConts += g->ptsPerGrid;
	
	approxConts *= (showUniformGridAllSlices) ? 1 : g->grids;
	
	if( approxConts > plug.maxContsRend )
	{
		CustomDialog ds("Performance Warning", this, BS_NO_YES );
		ds.addHtmlLabel  ( "<b>WARNING</b>: Current grid settings requires ~<b>" + QStr(approxConts)
										 + "</b> contours <br>"
											 "to be drawn and this will take a long time<br>"
											 ".... possibly even crash 3dmod.");
		ds.setStylePrev ("background-color: rgb(255, 40, 40);");			// red
		
		ds.addHtmlLabel  ( "Do you want to render anyway?" );
		
		if( dOpt==GD_CROSSHAIRS || dOpt==GD_ARROWS )
		{
			ds.addHtmlLabel  ( "TIP: Changing the grid display to '<b>lines</b>' <br>"
												 "instead of '<b>crosshairs/arrows</b>' should drastically <br>"
												 "reduce the nubmer of required lines" );
		}
		
		ds.addLabel			( "----" );
		ds.addLineEditF ( "Don't show this warning unless\n"
										  "contours numbers exceed", 1000.0f, FLOAT_MAX,
										  &plug.maxContsRend, 0,
										  "Allows you to chose the maximum number of contours \n"
										  "which can be used to render grid lines before a warning \n"
										  "message appears which lets you decide if you want to \n"
										  "render this many lines. Drawing too many contours on \n"
										  "many contours on screen at once can crash the machine \n"
										  "and also it would be quite silly to attempt to classify \n"
										  "such huge number of points." );
		
		ds.exec();
		if( ds.wasCancelled() )
			return (false);
	}
	
	
	
	
	//## IF ALLOW RANDOM POINTS IS ON, DRAW ALL THESE RANDOMLY POSITIONED POINTS
	//## AS CROSSHAIRS (AND NO OTHER CROSSHAIRS):
	
	if( g->allowRandomPts() )
	{
		for(long p=0; p<g->ptsize(); p++)
		{
			Ipoint *pt = &g->pts[p].pos;
			cont_addLineToObj( xobjG, pt->x+off, pt->y, pt->z,  pt->x-off, pt->y, pt->z);
			cont_addLineToObj( xobjG, pt->x, pt->y+off, pt->z,  pt->x, pt->y-off, pt->z);
		}
	}
	
	
	//## DRAW GRIDS ON EACH SLICE:
	
	switch(plug.gridDisplayOpt)
	{		
		case( GD_LINES ):
		{
			for (int z=zMin; z<=zMax; z+=g->zSpacing)
			{
				for(int x=0; x<g->cols; x++)	// for each col: draw vertical line from left
				{
					float currX = ((float)(x+1) * g->xSpacing) + g->xMin;
					cont_addLineToObj( xobjG, currX,g->yMin,z, currX,g->yMax,z, false, false);
				}
				
				for(int y=0; y<g->rows; y++)	// for each row: draw horizontal line from top
				{
					float currY = -((float)(y+1) * g->ySpacing) + g->yMax;
					cont_addLineToObj( xobjG, g->xMin,currY,z, g->xMax,currY,z, false, false);
				}
			}
			break;
		}
		
		case( GD_CROSSHAIRS ):
		{
			if( g->allowRandomPts() )		// if allow random points on, extra crosshairs will
				break;										//  be confusing, so we'll skip drawing these.
			
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for each row
			for(int x=0; x<g->cols; x++)			// for each column
			{
				float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
				
				if ( g->showSubRects && !g->isPtInSubRect( cX, cY, z, false, true ) )
					continue;
				
				cont_addLineToObj( xobjG, cX+off,cY,z, cX-off,cY,z, false, false);
				cont_addLineToObj( xobjG, cX,cY+off,z, cX,cY-off,z, false, false);
			}}}
			break;
		}
		
		case( GD_ARROWS ):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing)
			for(int y=0; y<g->rows; y++)		// for each row
			for(int x=0; x<g->cols; x++)		// for each column
			{
				float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
						
				cont_addLineToObj( xobjG, cX+off,cY,z, cX,cY,z, false, false);
				cont_addLineToObj( xobjG, cX,cY+off,z, cX,cY,z, false, false);
			}
			break;
		}
		
		case( GD_OFFPTS ):
		{
			for (int z=zMin; z<=zMax; z+=g->zSpacing)
			{
				if( g->allowRandomPts() )		// if allow random points on, extra crosshairs will
					break;										//  be confusing, so we'll skip drawing these.
				
				for(int z=zMin; z<=zMax; z+=g->zSpacing) {
				for(int y=0; y<g->rows; y++) {		// for each row
				for(int x=0; x<g->cols; x++)			// for each column
				{
					float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
					float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
					
					if( y%2==1 )																				// if odd row (from top):
						cX += halfXSpacing;																	//  add quarter a length
					
					if ( g->showSubRects && !g->isPtInSubRect( cX, cY, z, false, true ) )
						continue;
					
					cont_addLineToObj( xobjG, cX+off,cY-off,z, cX-off,cY+off,z, false, false);
					cont_addLineToObj( xobjG, cX+off,cY+off,z, cX-off,cY-off,z, false, false);
				}}}
				break;
			}
			break;
		}
		
		case( GD_RAND ):
		{
			for (int z=zMin; z<=zMax; z+=g->zSpacing)
			{
				for(int x=0; x<g->cols; x++)	// for each col: draw vertical line marks
				{
					float currX = ((float)(x+1) * g->xSpacing) + g->xMin;
					cont_addLineToObj( xobjG, currX,g->yMax,z, currX,g->yMax+off,z, false, false);
					cont_addLineToObj( xobjG, currX,g->yMin,z, currX,g->yMin-off,z, false, false);
				}
				
				for(int y=0; y<g->rows; y++)	// for each row: draw horizontal line marks
				{
					float currY = -((float)(y+1) * g->ySpacing) + g->yMax;
					cont_addLineToObj( xobjG, g->xMin,currY,z, g->xMin-off,currY,z, false, false);
					cont_addLineToObj( xobjG, g->xMax,currY,z, g->xMax+off,currY,z, false, false);
				}
				
				if( !g->countingStarted )			// if counting not started: 
				{																// display rand pts to demonstrate
					for( int p=0; p<g->ptsPerGrid; p++ )
					{
						Ipoint pt = g->genRandomPt( false, false, true );
						cont_addLineToObj( xobjG, pt.x+off,pt.y,z, pt.x-off,pt.y,z );
						cont_addLineToObj( xobjG, pt.x,pt.y+off,z, pt.x,pt.y-off,z );
					}
				}
			}
			break;
		}
		
		case( GD_VERTLINES ):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing)
			for(int x=0; x<g->cols; x++)	// for each col: draw vertical line from left
			{
				float currX = ((float)(x) * g->xSpacing) + g->xMin;
				cont_addLineToObj( xobjG, currX,g->yMin,z, currX,g->yMax,z, false, false);
			}
			break;
		}
		
		case( GD_HORZLINES ):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing)
			for(int y=0; y<g->rows; y++)	// for each row: draw horizontal line from top
			{
				float currY = -((float)(y) * g->ySpacing) + g->yMax;
				cont_addLineToObj( xobjG, g->xMin,currY,z, g->xMax,currY,z, false, false);
			}
			break;
		}
		
		case( GD_UPDIAGONAL ):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for each row
			for(int x=0; x<g->cols; x++)			// for each column
			{
				float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
				float cXEnd =  cX + g->xSpacing;
				float cYEnd =  cY + g->ySpacing;
				
				if ( g->showSubRects && ( !g->isPtInSubRect(cX, cY, z,    true,true)
												       || !g->isPtInSubRect(cXEnd,cYEnd,z,true,true) ) )
					continue;
				
				cont_addLineToObj( xobjG, cX,cY,z, cXEnd,cYEnd,z, false, false);
			}}}
			break;
		}
		
		case( GD_DOWNDIAGONAL ):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for each row
			for(int x=0; x<g->cols; x++)			// for each column
			{
				float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
				float cXEnd =  cX - g->xSpacing;
				float cYEnd =  cY + g->ySpacing;
				
				if ( g->showSubRects && ( !g->isPtInSubRect(cX, cY, z,    true,true )
										           || !g->isPtInSubRect(cXEnd,cYEnd,z,true,true ) ) )
					continue;
				
				cont_addLineToObj( xobjG, cX,cY,z, cXEnd,cYEnd,z, false, false);
			}}}
			break;
		}
		
		case( GD_LINEPAIRS ):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing)
			{
				for(int y=0; y<g->rows; y++) {		// for each row:
				for(int x=0; x<g->cols; x++)			// for each column:
				{
					float cY = -((float)(y+1) * g->ySpacing) + g->yMax;	// height of line
					float cX = (float(x+1) * g->xSpacing) + g->xMin;		// where line starts
					
					if( y%2==1 )																				// if odd row (from top):
						cX += (halfXSpacing * 0.5f);												//  add quarter a length
					float cXEnd = cX + halfXSpacing;											// where line ends
					
					if ( g->showSubRects && ( !g->isPtInSubRect(cX, cY, z,  true,true)
												         || !g->isPtInSubRect(cXEnd,cY,z, true,true) ) )
						continue;
					
					cont_addLineToObj( xobjG, cX,cY,z, cXEnd,cY,z );						// horizontal line
					
					cont_addLineToObj( xobjG, cX,cY+off,z, cX,cY-off,z );				// |
					cont_addLineToObj( xobjG, cXEnd,cY+off,z, cXEnd,cY-off,z );	// |-- line ends
				}}
			}
			break;
		}
		
		case( GD_CYCLOIDS  ):		// see: http://en.wikipedia.org/wiki/Cycloid
		case( GD_CYCLOIDS2 ):
		{
			//## CALCULATE RADIUS FOR CYCLOID:
			
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for every row:
			for(int x=0; x<g->cols; x++)			// for every column:
			{
				bool flipY = (dOpt!=GD_CYCLOIDS) && (x%2==1);
				
				float cX    =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY    = -((float)(y+1) * g->ySpacing) + g->yMax;
				float cXEnd = cX + (0.5f * g->xSpacing);
				float cYEnd = cY + ((flipY) ? -(0.5f * g->ySpacing) : (0.5f * g->ySpacing));
				
				if ( g->showSubRects && ( !g->isPtInSubRect(cX, cY, z,    true,true)
															 || !g->isPtInSubRect(cXEnd,cYEnd,z,true,true) ) )
					continue;
				
				//** DRAW HALF CYCLOIDS:
				
				long ptIdx = g->getPtIdx( x, y, (int)fDiv((z-g->zMin),g->zSpacing) );
				Icont *xcont = imodContourNew();
				g->genContourForPt(ptIdx, xcont);
				imodObjectAddContour(xobjG, xcont);		
				
				free(xcont);
				
				cont_addLineToObj( xobjG, cX+off,cY,z, cX-off,cY,z );							// |-- line
				cont_addLineToObj( xobjG, cXEnd,cYEnd+off,z, cXEnd,cYEnd-off,z );	// |   ends
			}}}
			
			break;
		}
		case( GD_CYCLOIDS3 ):
		{
			//## CALCULATE RADIUS FOR CYCLOID:
			
			float r = g->xSpacing / (PI * 2.0f);		// we want a "true" cycloid which
			float xStretch = 1.0f;									// is not distorted by y spacing value
			float arcPointsDiv = (float)ARCPOINTS;
			
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for every row:
			for(int x=0; x<g->cols; x++)			// for every column:
			{
				bool flipY = (dOpt!=GD_CYCLOIDS) && (x%2==1);
				
				float cX    =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY    = -((float)(y+1) * g->ySpacing) + g->yMax;
				float cYEnd = cY + ((flipY) ? -(2.0*r) : (2.0*r));
				
				if ( g->showSubRects && !g->isPtInSubRect(cX,cY,z,true,true) )
					continue;
				
				//** DRAW FULL, ALTERNATING CYCLOIDS:
				
				Icont *xcont = imodContourNew();
				for(int i=0; i<=ARCPOINTS*2; i++)					// generate a full cycloid:
				{
					float angle = PI * ( (float)i / arcPointsDiv ) - (PI*0.5);
					
					float ptX = cX + ((r * (angle - cos(angle)) ) * xStretch) + quarterXSpacing;
					float ptY = cYEnd - (r *  (1.0f  - sin(angle)) );
					imodPointAppendXYZ(xcont, ptX, (flipY) ? (2.0f*cYEnd - ptY) : ptY, z);
				}
				imodObjectAddContour(xobjG, xcont);							 
				free(xcont);
				
				cont_addLineToObj( xobjG, cX+off,cY,z, cX-off,cY,z );		// line end
			}}}
			
			break;
		}
			
		case( GD_RECTS ):
		{			
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for each row
			for(int x=0; x<g->cols; x++)			// for each column
			{
				float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
				float cXEnd = cX + (g->xSpacing * 0.5f );
				float cYEnd = cY + (g->ySpacing * 0.5f );
				
				if ( g->showSubRects && ( !g->isPtInSubRect(cX, cY, z,    false,true)
															 || !g->isPtInSubRect(cXEnd,cYEnd,z,false,true) ) )
					continue;
				
				cont_addLineToObj( xobjG, cX,cYEnd,z,  cXEnd,cYEnd,z, false, false);	// top
				cont_addLineToObj( xobjG, cX,cY,z,     cXEnd,cY,z,    false, false);	// bottom
				cont_addLineToObj( xobjG, cX,cY,z,     cX,cYEnd,z,    false, false);	// left
				cont_addLineToObj( xobjG, cXEnd,cY,z,  cXEnd,cYEnd,z, false, false);	// right
			}}}
			break;
		}
		
		case( GD_FORBIDSQ1 ):
		case( GD_FORBIDSQ2 ):
		case( GD_FORBIDSQ3 ):
		{			
			float sqLength = MIN( g->xSpacing, g->ySpacing );
			if( dOpt==GD_FORBIDSQ1 ) sqLength /= 2.0f;
			if( dOpt==GD_FORBIDSQ2 ) sqLength /= 4.0f;
			if( dOpt==GD_FORBIDSQ3 ) sqLength /= 8.0f;
			float tip = MAX( sqLength / 2.0f, (float)plug.gridSymbolSize );
			
			for(int z=zMin; z<=zMax; z+=g->zSpacing) {
			for(int y=0; y<g->rows; y++) {		// for each row
			for(int x=0; x<g->cols; x++)			// for each column
			{
				float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
				float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
				float cXEnd = cX + sqLength;
				float cYEnd = cY + sqLength;
				
				if ( g->showSubRects && ( !g->isPtInSubRect(cX, cY, z,    true,true)
															 || !g->isPtInSubRect(cXEnd,cYEnd,z,true,true) ) )
					continue;
				
				cont_addLineToObj( xobjG, cX,cYEnd,z, cXEnd,cYEnd,z,  false,true);			// top
				cont_addLineToObj( xobjG, cX,cY,z,    cXEnd,cY,z,     false);						// bottom
				cont_addLineToObj( xobjG, cX,cY,z,    cX,cYEnd+tip,z, false);						// left
				cont_addLineToObj( xobjG, cXEnd,cY-tip,z, cXEnd,cYEnd,z,  false,true);	// right
			}}}
			break;
		}
	}
	
	//## IF APPROPRIATE: DRAW DASHES OVER EVERY POINT:
	
	if( needsDashes )
	{
		float dashHalf = MIN (off / 2.0f, 4.0f);		// half the desired dash length
		bool vert = (dOpt==GD_HORZLINES );					// is true if we want vertical dashes,
																								//  false if we want horizontal
		
		for(int z=zMin; z<=zMax; z+=g->zSpacing) {
		for(int y=0; y<g->rows; y++) {		// for each row
		for(int x=0; x<g->cols; x++)			// for each column
		{
			float cX =  ((float)(x+1) * g->xSpacing) + g->xMin;
			float cY = -((float)(y+1) * g->ySpacing) + g->yMax;
			
			if ( g->showSubRects && !g->isPtInSubRect(cX,cY,z, true,true) )
				continue;
			
			float x1 = (vert) ? cX            : cX + dashHalf;
			float x2 = (vert) ? cX            : cX - dashHalf;
			float y1 = (vert) ? cY + dashHalf : cY;
			float y2 = (vert) ? cY - dashHalf : cY;
			
			cont_addLineToObj( xobjG, x1,y1,z, x2,y2,z, false, false);
		}}}						
	}
	
	
	
	//## IF APPROPRIATE: SET GRID LINES TO SHOW ON ALL SLICES:
	
	if(showUniformGridAllSlices)					// if we want to show gridlines every slice:
	{
		for(int c=MAX(firstContGridlines,0); c<csize(xobjG); c++ )
			imodContourSetFlag( getCont(xobjG,c), ICONT_DRAW_ALLZ, 1 );
	}																				// draw these gridlines over every single slice
	
	
  if( redraw )
		ivwDraw( plug.view, IMOD_DRAW_ALL );
  return true;
}


//------------------------
//-- Accesses one of the extra objects "plug.extraObjRect1" and 
//-- plug.extraObjRect1 and draws subsample rectangles (which
//-- by default are the form of 'forbidden rectangles') over
//-- every grid.

bool Stereology::drawSubsampleRects( bool redraw )
{
	GridSetObj *g = getCurrGridSetObj();
	
  //## CLEAR EXTRA OBJECTS:
  
	Iobj *xobjR1 = ivwGetAnExtraObject(plug.view, plug.extraObjRect1);
  Iobj *xobjR2 = ivwGetAnExtraObject(plug.view, plug.extraObjRect2);
	
  if ( !plug.window || !xobjR1 )
    return 0;
  
	imodObjectSetValue(xobjR1, IobjFlagExtraInModv, (plug.showGridInModelView)?1:0);
	imodObjectSetValue(xobjR2, IobjFlagExtraInModv, (plug.showGridInModelView)?1:0);
	imodObjectSetValue(xobjR1, IobjLineWidth2, plug.gridLineThickness+1);
	imodObjectSetValue(xobjR2, IobjLineWidth2, plug.gridLineThickness+1);
	ivwClearAnExtraObject( plug.view, plug.extraObjRect1 );
  ivwClearAnExtraObject( plug.view, plug.extraObjRect2 );
  
	//## IF SUBSAMPLE AREA OFF: EXIT EARLY
	
	float rXSpacing = g->rXSpan + g->rXGap;
	float rYSpacing = g->rYSpan + g->rYGap;
	
	if( !g->showSubRects || rXSpacing <= 0 || rYSpacing <= 0 )
	{
		if( redraw )
			ivwDraw( plug.view, IMOD_DRAW_ALL );
		return true;
	}
	
	
  //## GET Z VALUE AND SIZE OF SCREEN PIXELS:
  
	float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
	float sc = fDiv( 1.0f, zapZoom);			// tomogram distance for one screen pixel 
	
	
  //## DETERMINE WHAT SECTIONS TO SHOW SUBSAMPLE RECTANGLES OVER:
  
	int zMin = g->zMin;
	int zMax = g->zMax;
	
	int firstContGridlines;
	bool showUniformGridAllSlices = plug.showGridEverySlice && !g->countingStarted;
	
	
	//## DRAW SUBSAMPLE RECTANGLES USING VALUES DICTATED BY:
	
	switch( plug.subRectDisplayOpt )
	{
		case(RD_FORBIDDENSQ):
		{
			float tip = MAX( (float)g->rYSpan / 2.0f, (float)plug.gridSymbolSize );
			tip       = MIN( (float)g->rYGap, tip );
			
			for(int   z=zMin; z<=zMax; z+=g->zSpacing) {
			for(float y=g->yMax - g->rYSpan; y>g->yMin; y-=rYSpacing) {		// for each row
			for(float x=g->xMin; x<g->xMax - g->rXSpan; x+=rXSpacing)			// for each column
			{
				float xEnd = x + g->rXSpan;
				float yEnd = y + g->rYSpan;
				
				cont_addLineToObj( xobjR2, x,yEnd,z,     xEnd,yEnd,z,  false,true);		// top
				cont_addLineToObj( xobjR1, x,y,z,        xEnd,y,z,     false);				// bottom
				cont_addLineToObj( xobjR1, x,y-sc,z,     xEnd,y-sc,z,  false);				// bottom
				cont_addLineToObj( xobjR1, x,y,z,        x,yEnd+tip,z, false);				// left
				cont_addLineToObj( xobjR1, x-sc,y,z,     x-sc,yEnd+tip,z, false);			// left
				cont_addLineToObj( xobjR2, xEnd,y-tip,z, xEnd,yEnd,z,  false,true);		// right
			}}}
			break;
		}
		
		case(RD_GRID):
		{
			for(int z=zMin; z<=zMax; z+=g->zSpacing)
			{
				for(float y=g->yMax; y>g->yMin; y-=rYSpacing) 	// for each row:
				{
					float y2 = y + g->rYSpan;
					cont_addLineToObj( xobjR2, g->xMin,y, z, g->xMax,y, z, false, true);
					cont_addLineToObj( xobjR2, g->xMin,y2,z, g->xMax,y2,z, false, true);
				}
				for(float x=g->xMin; x<g->xMax; x+=rXSpacing) 	// for each column:
				{
					float x2 = x + g->rXSpan;
					cont_addLineToObj( xobjR2, x,g->yMin,z, x,g->yMax,z,   false, true);
					cont_addLineToObj( xobjR2, x2,g->yMin,z, x2,g->yMax,z, false, true);
				}
			}
		}
		
		case(RD_NORMAL):
		case(RD_DOTTED):
		{
			float dotted = ( plug.subRectDisplayOpt == RD_DOTTED );
			
			for(int   z=zMin; z<=zMax; z+=g->zSpacing) {
			for(float y=g->yMax; y>g->yMin; y-=rYSpacing) {		// for each row
			for(float x=g->xMin; x<g->xMax; x+=rXSpacing)			// for each column
			{
				float xEnd = x + g->rXSpan;
				float yEnd = y - g->rYSpan;
				
				cont_addLineToObj( xobjR1, x,yEnd,z, xEnd,yEnd,z,  false, dotted);		// bottom
				cont_addLineToObj( xobjR1, x,y,z,    xEnd,y,z,     false, dotted);		// top
				cont_addLineToObj( xobjR1, x,y,z,    x,yEnd,z,     false, dotted);		// left
				cont_addLineToObj( xobjR1, xEnd,y,z, xEnd,yEnd,z,  false, dotted);		// right
			}}}
			break;
		}
	}
	
	
	//## IF APPROPRIATE: SET GRID LINES TO SHOW ON ALL SLICES:
	
	if(showUniformGridAllSlices)					// if we want to show gridlines every slice:
	{
		for(int c=0; c<csize(xobjR1); c++ )
			imodContourSetFlag( getCont(xobjR1,c), ICONT_DRAW_ALLZ, 1 );
	}																				// draw these gridlines over every single slice
	
	
  if( redraw )
		ivwDraw( plug.view, IMOD_DRAW_ALL );
  return true;
}



//------------------------
//-- Uses the extra object referenced by "plug.extraObjSel" to clear
//-- then redraw  a large yellow crosshair around the currently
//-- selected stereology point.

bool Stereology::drawSelObject( bool redraw )
{
	GridSetObj *g = getCurrGridSetObj();
	
  //## CLEAR EXTRA OBJECTS:
  
	Iobj *xobjS = ivwGetAnExtraObject(plug.view, plug.extraObjSel);
  
  if ( !plug.window || !xobjS )
    return false;
  
	imodObjectSetValue(xobjS, IobjFlagExtraInModv, 0);
  ivwClearAnExtraObject( plug.view, plug.extraObjSel );
  
	
  //## DRAW WHITE ARROWS AROUND SELECTED POINT:
	
	float zapZoom = 1.0f;
	ivwGetTopZapZoom(plug.view, &zapZoom);
	float sc = fDiv( 1.0f, zapZoom);
	
	float o1 = plug.gridSymbolSize;
	float o2 = (2.0f*o1) + (sc * 5.0f + 2.0f);
	
	Ipoint pt = g->getPos( g->currPtIdx );
	float x = pt.x;
	float y = pt.y;
	int z = (int)pt.z;
	
	switch(plug.selPtDisplay)
	{
			
		case( GD_RAND ):
			break;
			
		case( GD_LINES ):
			cont_addLineToObj( xobjS, x-o2, y, z,  x+o2, y,  z, false,false);
			cont_addLineToObj( xobjS, x, y-o2, z,  x, y+o2,  z, false,false);
			break;
			
		case( GD_CROSSHAIRS ):
			cont_addLineToObj( xobjS, x+o1, y, z,  x+o2, y,  z, false,false);
			cont_addLineToObj( xobjS, x-o1, y, z,  x-o2, y,  z, false,false);
			cont_addLineToObj( xobjS, x, y+o1, z,  x, y+o2,  z, false,false);
			cont_addLineToObj( xobjS, x, y-o1, z,  x, y-o2,  z, false,false);
			break;
			
		case( GD_ARROWS ):
			cont_addLineToObj( xobjS, x-o1, y-o2, z,  x, y-o1,  z, false,false);
			cont_addLineToObj( xobjS, x+o1, y-o2, z,  x, y-o1,  z, false,false);
			
			cont_addLineToObj( xobjS, x-o1, y+o2, z,  x, y+o1,  z, false,false);
			cont_addLineToObj( xobjS, x+o1, y+o2, z,  x, y+o1,  z, false,false);
			
			cont_addLineToObj( xobjS, x-o2, y-o1, z,  x-o1, y,  z, false,false);
			cont_addLineToObj( xobjS, x-o2, y+o1, z,  x-o1, y,  z, false,false);
			
			cont_addLineToObj( xobjS, x+o2, y-o1, z,  x+o1, y,  z, false,false);
			cont_addLineToObj( xobjS, x+o2, y+o1, z,  x+o1, y,  z, false,false);
			break;
			
		case( GD_VERTLINES ):
			cont_addLineToObj( xobjS, x, y+o1, z,  x, y-o1,  z, false,false);
			break;
			
		case( GD_HORZLINES ):
			cont_addLineToObj( xobjS, x-o1, y, z,  x+o1, y,  z, false,false);
			break;
	}
	
  if( redraw )
		ivwDraw( plug.view, 0 );
	
	return true;
}


//------------------------
//-- Uses the extra object referenced by "plug.extraObjBlack" to clear
//-- then draw black crosshairs over any stereology points which have
//-- been checked. Note that if "plug.blackVisitedPts" if false,
//-- the object is cleared but no crosshairs are drawn.

bool Stereology::drawBlackObject( bool redraw )
{
	GridSetObj *g = getCurrGridSetObj();
	
  //## CLEAR EXTRA OBJECTS:
  
	Iobj *xobjB = ivwGetAnExtraObject(plug.view, plug.extraObjBlack);
  
  if ( !plug.window || !xobjB )
    return false;
  
	imodObjectSetValue(xobjB, IobjFlagExtraInModv, (plug.showGridInModelView)?1:0);
	imodObjectSetValue(xobjB, IobjLineWidth2, plug.gridLineThickness);
  ivwClearAnExtraObject( plug.view, plug.extraObjBlack );
  
	
	if( !plug.blackVisitedPts || g->pts.size()==0 )
	{
		if( redraw )
			ivwDraw( plug.view, IMOD_DRAW_ALL );
		return false;
	}
	
  //## DRAW BLACK CROSSHAIRS ON POINTS WHICH HAVE BEEN CHECKED:
  
	float off = plug.gridSymbolSize;
	
	for(long p=0; p<g->ptsize(); p++)
	{
		if( g->getSPt(p)->checked==false )
			continue;
		Ipoint *pt = &g->getSPt(p)->pos;
		
		cont_addLineToObj( xobjB, pt->x+off,pt->y,pt->z, pt->x-off,pt->y,pt->z,false,false);
		cont_addLineToObj( xobjB, pt->x,pt->y+off,pt->z, pt->x,pt->y-off,pt->z,false,false);
	}
	
	
  if( redraw )
		ivwDraw( plug.view, IMOD_DRAW_ALL );
	
	return true;
}


//------------------------
//-- Uses the extra object referenced by "plug.extraObjExtra" to clear
//-- then draw the "Pt Painter" circle around the mouse. Note that this
//-- circle is only drawn if "plug.paintMode" is PM_PAINT.

bool Stereology::drawExtraObject( bool redraw )
{
	GridSetObj *g = getCurrGridSetObj();
	
  //## CLEAR EXTRA OBJECTS:
  
	Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  
  if ( !plug.window || !xobjX )
    return false;
  
	imodObjectSetValue(xobjX, IobjFlagExtraInModv, 0);		// don't show in model view
  ivwClearAnExtraObject( plug.view, plug.extraObjExtra );
  
	
	if( plug.paintMode == PM_OFF )
		return false;
	
	
  //## DRAW FORMAT PAINT CIRCLE:
  
	if(	plug.paintMode == PM_PAINT )
	{
		float radius = plug.paintRadius;
		
		Icont *xcont   = imodContourNew();    // primary closed contour used in extra object
		cont_generateCircle( xcont, radius, 100, plug.mouse, true );
		
		imodObjectAddContour(xobjX, xcont);
		free(xcont);
	}
	
	//## DRAW INTERCEPT TOOL:
  
	if(	plug.paintMode == PM_INTERCEPTS )
	{
		float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
		int noZap = ivwGetTopZapZoom(plug.view, &zapZoom); 
		float sc = fDiv( 1.0f, zapZoom);			// tomogram distance for one screen pixel 
		
		float off = 2.0f*plug.gridSymbolSize + 5.0f*sc;
		float x = plug.mouse.x;
		float y = plug.mouse.y;
		float z = plug.mouse.z;
		
		Ipoint pt;
		bool interceptFound = g->getInterceptNearPt( plug.mouse, &pt, INTERCEPT_SNAP );
		
		if( interceptFound )		// if intercept found near mouse:
		{													// show solid crosshair over intercept point...
			cont_addLineToObj( xobjX, pt.x+off,pt.y,pt.z, pt.x-off,pt.y,pt.z,false,false);
			cont_addLineToObj( xobjX, pt.x,pt.y+off,pt.z, pt.x,pt.y-off,pt.z,false,false);
			x = pt.x;				// |
			y = pt.y;				// |-- we want to snap the lines
		}
		
		if( plug.showBigIntercCH )
		{
			cont_addLineToObj( xobjX, g->xMin,y,z,  x-off,y,z, false,true);	// |-- draws big
			cont_addLineToObj( xobjX, g->xMax,y,z,  x+off,y,z, false,true);	// |   crosshair
			cont_addLineToObj( xobjX, x,g->yMin,z,  x,y-off,z, false,true); // |   with dotted
			cont_addLineToObj( xobjX, x,g->yMax,z,  x,y+off,z, false,true); // |   lines
		}
		
		//## DRAW TEXT TO SHOW WHAT CATEGORY IS SELECTED TO ADD
		//## INTERCEPT POINTS TO:
		
		{
			float fontSize = 12 * sc;
			Ipoint textPos;
			setPt( &textPos, x+fontSize, y+fontSize, z );
			string text = "cat : " + toString( plug.interceptCat );
			if( plug.interceptCat == -1 )
				text = "press a number...";
			cont_generateTextAreaAsConts( xobjX, text, textPos, fontSize, TA_LEFT, TV_BOTTOM,
																		true, 4 );
		}
		
	}
	
	
  if( redraw )
		ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
	
	return true;
}




//############################################################
//## PLUGIN SETTINGS METHOD:


//------------------------
//-- Used to initialize default values into StereologyData.

void Stereology::initValues()
{
	//## SAVED SETTINGS:
	
  plug.sameXYSpacing       = true;
  plug.showGridEverySlice  = false;
  plug.changeDefLimits     = false;
  
	plug.defSpacingX         = 50;
  plug.defSpacingY         = 50;
  plug.defSpacingZ         = 30;
	plug.limitDefaultInset   = 20;
	plug.showChangeDefOpt    = true;
	
	plug.showGridDisplayOpts = true;
	plug.showGridInModelView = true;
	plug.gridDisplayOpt      = GD_CROSSHAIRS;
	plug.gridSymbolSize      = 5;
	plug.gridColorR          = 255;		// |
	plug.gridColorG          = 0;			// |-- defaults to red
	plug.gridColorB          = 0;			// |
	plug.gridLineThickness   = 1;
	plug.gridLabels          = GL_OFF;
	plug.selPtDisplay        = GD_ARROWS;
	plug.showDashes          = true;
	
	plug.lockForbiddenSqXY    = true;
	plug.subRectDisplayOpt    = RD_FORBIDDENSQ;
	
	plug.autoProgress        = true;
	plug.centerOnEachPt      = true;
	
	plug.blackVisitedPts     = true;
	
	plug.paintRadius         = 30.0f;
	plug.paintMode           = PM_OFF;
	
	plug.resultsTranspose    = false;
	plug.resultsSepGrids     = false;
	plug.resultsSepCharIdx   = 0;
	plug.resultsOutputOpt    = 1;
	plug.resultsAvgAllGrids  = true;
	plug.resultsShowStDev    = false;
	
	plug.secsPerPt           = 3.0f;
	
	
	plug.showLoadingInConsole = true;
	plug.maxContsRend         = 100000;
	plug.numRandPtsToAdd			= 1000;
	plug.jumpToGridOnUpdate   = true;
	
	plug.targetNumPts         = 1000;
	plug.randomSymbols        = false;
	plug.agreeFinalizeDlg     = false;
	plug.showBigIntercCH      = false;
	
	plug.maskObjOpt						= RG_CURR;			// 0=current object only,  1=all, 2=custom
	plug.maskInverseOpt				= 0;						// 0=mask is inside contours, 1=outside
	plug.maskSectionsOpt			= RG_CURR;			// 0=current section only, 1=all, 2=custom
	plug.maskSelPtsOpt				= SEL_OFF;			// 0=all, 1=checked, 2=unchecked only
	plug.maskChangeToCheckedOpt = SEL_ON;			// 0=don't change, 1=checked, 2=unchecked
	
	plug.masksDefaultUse      = true;
	plug.masksDefaultCat      = 0;
	plug.masksDefaultChecked  = 1;
	plug.masksPrintChanges    = false;
	
	plug.masksUse[0]          = true;				// masksUse[NUM_MASKS]
	plug.masksUse[1]          = true;
	plug.masksUse[2]          = true;
	plug.masksUse[3]          = true;
	plug.masksUse[4]          = false;
	plug.masksObj[0]          = 1;						// masksObj[NUM_MASKS]
	plug.masksObj[1]          = 2;
	plug.masksObj[2]          = 3;
	plug.masksObj[3]          = 4;
	plug.masksObj[4]          = 5;
	plug.masksCat[0]          = 1;						// masksCat[NUM_MASKS]
	plug.masksCat[1]          = 4;
	plug.masksCat[2]          = 2;
	plug.masksCat[3]          = 3;
	plug.masksCat[4]          = 5;
	
	//## IF BIG IMAGE THEN INCREASE DEFAULT SIZE
	
	if( plug.xsize > 2000 && plug.ysize > 2000 )		// if big image:
	{
		plug.defSpacingX = 500;
		plug.defSpacingY = 500;
		plug.gridSymbolSize = 20;
	}
	
	
	//## TRANSIENT VALUES:
	
	plug.numMinutesWorked     = 0.0f;
  plug.defaultFilePath      = QString(getenv("IMOD_DIR"))
											        + "/lib/imodplug/default_stereology_categories.csv";
	plug.interceptCat         = -1;
	
	loadWordList();
}



//------------------------
//-- Loads most of the settings for Stereology from user preferences

void Stereology::loadSettings()
{
  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("Stereology", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS )
  {
    wprint("Stereology: Could not load saved values");
    int result = QMessageBox::information( this, "-- Documentation --",
                              "If this is your first time using 'Stereology' \n"
                              "we HIGHLY recommended you click 'Help' \n"
                              "(at bottom of the plugin) to learn how it works! \n\n"
                              "                                   -- Andrew Noske\n",
                              QMessageBox::Yes, QMessageBox::No );
    if( result == QMessageBox::Yes )
      helpPluginHelp();
  }
  else
	{
		plug.sameXYSpacing       = savedValues[0];
		plug.showGridEverySlice  = savedValues[1];
		plug.changeDefLimits     = savedValues[2];
		
		plug.defSpacingX         = savedValues[3];
		plug.defSpacingY         = savedValues[4];
		plug.defSpacingZ         = savedValues[5];
		plug.limitDefaultInset   = savedValues[6];
		plug.showChangeDefOpt    = savedValues[7];
		
		plug.showGridDisplayOpts = savedValues[8];
		plug.showGridInModelView = savedValues[9];
		plug.gridDisplayOpt      = savedValues[10];
		plug.gridSymbolSize      = savedValues[11];
		plug.gridColorR          = savedValues[12];
		plug.gridColorG          = savedValues[13];
		plug.gridColorB          = savedValues[14];
		plug.gridLineThickness   = savedValues[15];
		plug.gridLabels          = savedValues[16];
		plug.selPtDisplay        = savedValues[17];
		plug.showDashes          = savedValues[18];
		
		plug.lockForbiddenSqXY   = savedValues[19];
		plug.subRectDisplayOpt   = savedValues[20];
		
		plug.autoProgress        = savedValues[21];
		plug.centerOnEachPt      = savedValues[22];
		plug.blackVisitedPts     = savedValues[23];
		
		plug.paintRadius         = savedValues[24];
		plug.paintMode           = savedValues[25];
		
		plug.resultsTranspose    = savedValues[26];
		plug.resultsSepGrids     = savedValues[27];
		plug.resultsSepCharIdx   = savedValues[28];
		plug.resultsOutputOpt    = savedValues[29];
		plug.resultsAvgAllGrids  = savedValues[30];
		plug.resultsShowStDev    = savedValues[31];
		
		plug.secsPerPt           = savedValues[32];
		
		plug.showLoadingInConsole = savedValues[33];
		plug.maxContsRend         = savedValues[34];
		plug.numRandPtsToAdd      = savedValues[35];
		plug.jumpToGridOnUpdate   = savedValues[36];
		plug.targetNumPts         = savedValues[37];
		plug.randomSymbols        = savedValues[38];
		plug.agreeFinalizeDlg     = savedValues[39];
		plug.showBigIntercCH      = savedValues[40];
		
		plug.maskObjOpt						= savedValues[41];
		plug.maskInverseOpt				= savedValues[42];
		plug.maskSectionsOpt			= savedValues[43];
		plug.maskSelPtsOpt				= savedValues[44];
		plug.maskChangeToCheckedOpt = savedValues[45];
		
		plug.masksDefaultUse      = savedValues[46];
		plug.masksDefaultCat      = savedValues[47];
		plug.masksDefaultChecked  = savedValues[48];
		plug.masksPrintChanges    = savedValues[49];
		
		
		plug.masksUse[0]					= savedValues[50];			// masksUse[NUM_MASKS]
		plug.masksUse[1]					= savedValues[51];
		plug.masksUse[2]					= savedValues[52];
		plug.masksUse[3]					= savedValues[53];
		plug.masksUse[4]					= savedValues[54];
		plug.masksObj[0]					= savedValues[55];						// masksObj[NUM_MASKS]
		plug.masksObj[1]					= savedValues[56];
		plug.masksObj[2]					= savedValues[57];
		plug.masksObj[3]					= savedValues[58];
		plug.masksObj[4]					= savedValues[59];
		plug.masksCat[0]					= savedValues[60];						// masksCat[NUM_MASKS]
		plug.masksCat[1]					= savedValues[61];
		plug.masksCat[2]					= savedValues[62];
		plug.masksCat[3]					= savedValues[63];
		plug.masksCat[4]					= savedValues[64];
		
		if(plug.paintRadius <= 0)
			plug.paintRadius = 10.0f;
		if(getCurrGridSetObj() == NULL || getCurrGridSetObj()->countingStarted == false)
			plug.paintMode = false;
		if(plug.gridLineThickness<1 || plug.gridLineThickness>10)
			plug.gridLineThickness = 1;
	}
}


//------------------------
//-- Saves most of the settings within StereologyData in user preferences
//-- so they will load next time the Stereology plugin is started.

void Stereology::saveSettings()
{

	//## CHECK IF GRID SPACINGS ARE DIFFERENT FROM DEFAULTS - IF SO
	//## THEN GIVE THE USER THE OPTION TO CHANGE THESE TO THE NEW 
	//## DEFAULTS (FOR NEXT TIME):
	
	GridSetObj *g = getCurrGridSetObj();
	
	if( plug.showChangeDefOpt && g!=NULL &&
		             (   g->xSpacing != plug.defSpacingX
								  || g->ySpacing != plug.defSpacingY 
									|| g->zSpacing != plug.defSpacingZ ) )
	{
		CustomDialog ds("Change Defaults", this, BS_NO_YES );
		ds.addHtmlLabel ( "Would you like to set <b>" + QStr(g->xSpacing) + "x"
									+ QStr(g->ySpacing) + "</b> and z spacing=<b>" + QStr(g->zSpacing)
									+ "</b><br>as the new grid spacing defaults?" );
		ds.addHtmlLabel( "<font size='2.5'>NOTE: To avoid seeing this message again "
										"go to <b>Options > Grid Settings</b>.</fontsize>","" );
		ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
		ds.exec();
		if( !ds.wasCancelled() )
		{
			plug.defSpacingX = g->xSpacing;
			plug.defSpacingY = g->ySpacing;
			plug.defSpacingZ = g->zSpacing;
		}
	}
	
	
	
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]  = plug.sameXYSpacing;
  saveValues[1]  = plug.showGridEverySlice;
  saveValues[2]  = plug.changeDefLimits;
  
  saveValues[3]  = plug.defSpacingX;
  saveValues[4]  = plug.defSpacingY;
  saveValues[5]  = plug.defSpacingZ;
	saveValues[6]  = plug.limitDefaultInset;
	saveValues[7]  = plug.showChangeDefOpt;
	
	saveValues[8]  = plug.showGridDisplayOpts;
	saveValues[9]  = plug.showGridInModelView;
	saveValues[10]  = plug.gridDisplayOpt;
	saveValues[11] = plug.gridSymbolSize;
	saveValues[12] = plug.gridColorR;
	saveValues[13] = plug.gridColorG;
	saveValues[14] = plug.gridColorB;
	saveValues[15] = plug.gridLineThickness;
	saveValues[16] = plug.gridLabels;
	saveValues[17] = plug.selPtDisplay;
	saveValues[18] = plug.showDashes;
	
	saveValues[19] = plug.lockForbiddenSqXY;
	saveValues[20] = plug.subRectDisplayOpt;
	
	saveValues[21] = plug.autoProgress;
	saveValues[22] = plug.centerOnEachPt;
	saveValues[23] = plug.blackVisitedPts;
	
	saveValues[24] = plug.paintRadius;
	saveValues[25] = plug.paintMode;
	
	saveValues[26] = plug.resultsTranspose;
	saveValues[27] = plug.resultsSepGrids;
	saveValues[28] = plug.resultsSepCharIdx;
	saveValues[29] = plug.resultsOutputOpt;
	saveValues[30] = plug.resultsAvgAllGrids;
	saveValues[31] = plug.resultsShowStDev;
	
	saveValues[32] = plug.secsPerPt;
	
	saveValues[33] = plug.showLoadingInConsole;
	saveValues[34] = plug.maxContsRend;
	saveValues[35] = plug.numRandPtsToAdd;
	saveValues[36] = plug.jumpToGridOnUpdate;
	saveValues[37] = plug.targetNumPts;
	saveValues[38] = plug.randomSymbols;
	saveValues[39] = plug.agreeFinalizeDlg;
	saveValues[40] = plug.showBigIntercCH;
	
	saveValues[41] = plug.maskObjOpt;
	saveValues[42] = plug.maskInverseOpt;
	saveValues[43] = plug.maskSectionsOpt;
	saveValues[44] = plug.maskSelPtsOpt;
	saveValues[45] = plug.maskChangeToCheckedOpt;
	
	saveValues[46] = plug.masksDefaultUse;
	saveValues[47] = plug.masksDefaultCat;
	saveValues[48] = plug.masksDefaultChecked;
	saveValues[49] = plug.masksPrintChanges;
	
	saveValues[50] = plug.masksUse[0];			// masksUse[NUM_MASKS]
	saveValues[51] = plug.masksUse[1];
	saveValues[52] = plug.masksUse[2];
	saveValues[53] = plug.masksUse[3];
	saveValues[54] = plug.masksUse[4];
	saveValues[55] = plug.masksObj[0];			// masksObj[NUM_MASKS]
	saveValues[56] = plug.masksObj[1];
	saveValues[57] = plug.masksObj[2];
	saveValues[58] = plug.masksObj[3];
	saveValues[59] = plug.masksObj[4];
	saveValues[60] = plug.masksCat[0];			// masksCat[NUM_MASKS]
	saveValues[61] = plug.masksCat[1];
	saveValues[62] = plug.masksCat[2];
	saveValues[63] = plug.masksCat[3];
	saveValues[64] = plug.masksCat[4];
	
  prefSaveGenericSettings("Stereology",NUM_SAVED_VALS,saveValues);
}

//------------------------
//-- Change to flag to keep on top or run timer as for info window
void Stereology::keepOnTop(bool state)
{
#ifdef STAY_ON_TOP_HACK
  mStayOnTop = state;
  // Start or kill the timer
  if (state)  
    mTopTimerID = startTimer(200);
  else if (mTopTimerID) {
    killTimer(mTopTimerID);
    mTopTimerID = 0;
  }
#else
  Qt::WindowFlags flags = windowFlags();
  if (state)
    flags |= Qt::WindowStaysOnTopHint;
  else
    flags ^= Qt::WindowStaysOnTopHint;
  QPoint p2 = pos();
  setWindowFlags(flags);
  move(p2);
  show();
#endif
}



//------------------------
//-- Loads a list of words into "plug.wordList" representing common
//-- types of subcellular parts used to autocomplete the category
//-- names as the user types into the category line edit boxes.

void Stereology::loadWordList()
{
	if( plug.wordList.size()>0 )
		return;
	
	//## ADD WORDS FROM http://neurolex.org/wiki/Subcellular_Parts_Table TO WORDLIST
	
	plug.wordList
	
	// A:
	<< "Actin Filament"
	<< "Active Zone Cytomatrix" 
	<< "Active Zone Dense Projection" 
	<< "Active Zone Plasma Membrane" 
	<< "Age Associated"     // ??
	<< "Amorphous Vesicle" 
	<< "Autolysosome" 
	<< "Autophagosome" 
	<< "Axolemma" 
	
	// B:
	<< "Barr Body" 
	<< "Basal Body" 
	<< "Bunina Body" 
	
	// C:
	<< "Cajal Body" 
	<< "Cellular Inclusion" 
	<< "Cellular Membrane" 
	<< "Cellular Subcomponent" 
	<< "Centriole" 
	<< "Chromatin" 
	<< "Cilium" 
	<< "Classical Lewy Body" 
	<< "Clathrin Coat" 
	<< "Clathrin Coated Endocytic Vesicle" 
	<< "Coated Pit" 
	<< "Coated Tip" 
	<< "Condensed Chromatin" 
	<< "Contractile vacuole" 
	<< "Cortical Lewy Body" 
	<< "Cytoplasmic Vesicle" 
	<< "Cytoskeletal Element" 
	<< "Cytosol" 
	
	// D:
	<< "Dendritic Microtubule" 
	<< "Dense Body" 
	<< "Dense Core Vesicle" 
	<< "Docked Vesicle" 
	
	// E:
	<< "Early Endosome" 
	<< "Endocytic Vesicle" 
	<< "Endoplasmic Reticulum" 
	<< "Endosomal Membrane" 
	<< "Endosomal Subcomponent" 
	<< "Endosome" 
	<< "Extended Chromatin" 
	
	// F:
	<< "Fibrillary Inclusion" 
	<< "Flame-shaped Neurofibrillary Tangle" 
	<< "Free Ribosome" 
	
	// G:
	<< "Glial Cytoplasmic Inclusion" 
	<< "Glial Filament" 
	<< "Glial Inclusion" 
	<< "Glycogen Granule" 
	<< "Golgi Apparatus" 
	<< "Golgi Lamellae" 
	<< "Golgi Subcomponent" 
	<< "Golgi-associated Vesicle" 
	<< "Granular Vesicle" 
	
	// H:
	<< "Hyaline Inclusion" 
	
	// I:
	<< "Inter-Golgi Transport Vesicle" 
	<< "Interchromatin Granule" 
	<< "Intermediate Filament" 
	<< "Intermediate Filament"        // sao952483289
	<< "Intracellular Membrane" 
	
	// L:
	<< "Lamellar Body" 
	<< "Laminated Body" 
	<< "Large Vesicle" 
	<< "Late Endosome" 
	<< "Lewy Body" 
	<< "Lewy Body-like Hyaline Inclusion" 
	<< "Lipofuscin" 
	<< "Lumen Cargo" 
	<< "Lysosome" 
	<< "Lytic vacuole" 
	
	// M:
	<< "Membrane Bound Organelle" 
	<< "Membrane Bound Ribosome" 
	<< "Membrane Cargo" 
	<< "Microfilament"                // sao2006047981
	<< "Microtubule"                  // sao1846835077
	<< "Mitochondrial Adhaerens Complex" 
	<< "Mitochondrial Chromosome" 
	<< "Mitochondrial Matrix" 
	<< "Mitochondrial Membrane" 
	<< "Mitochondrial Membrane Inner" 
	<< "Mitochondrial Membrane Outer" 
	<< "Mitochondrial Subcomponent" 
	<< "Mitochondrion" 
	<< "Multivesicular Body" 
	
	// N:
	<< "Nematosome" 
	<< "Neurofibrillary Tangle" 
	<< "Neurofilament"                // sao1316272517
	<< "Neuromelanin" 
	<< "Neuronal Cytoplasmic Inclusion" 
	<< "Neurosecretory Vesicle" 
	<< "Neurotubule" 
	<< "Non Membrane Bound Organelle" 
	<< "Nuclear Body" 
	<< "Nuclear Inner Membrane" 
	<< "Nuclear Lamina" 
	<< "Nuclear Membrane" 
	<< "Nuclear Outer Membrane" 
	<< "Nuclear Pore" 
	<< "Nuclear Subcomponent" 
	<< "Nucleolus" 
	<< "Nucleolus-associated Heterochromatin" 
	<< "Nucleoplasm" 
	<< "Nucleus" 
	
	// O:
	<< "Organelle" 
	
	// P:
	<< "Peroxisome" 
	<< "Pick Body" 
	<< "Pigment" 
	<< "Pinocytic Vesicle" 
	<< "Plasma Membrane" 
	<< "Plasmalemmal precursor vesicle" 
	<< "Post-lysosomal Vacuole" 
	<< "Post-synaptic Component" 
	<< "Post-synaptic Density" 
	<< "Pre-synaptic Active Zone Component" 
	<< "Pre-synaptic Component" 
	<< "Pre-synaptic Dense Body" 
	<< "Pre-synaptic Grid" 
	<< "Pre-synaptic Ribbon" 
	<< "Primary Lysosome" 
	
	// R:
	<< "RER Membrane"
	<< "Ribosome"
	<< "Rough Endoplasmic Reticulum"
	
	// S:
	<< "SER Membrane"
	<< "SER Subcomponent"
	<< "Secondary Lysosome"
	<< "Skein-like Inclusion"
	<< "Skein-like inclusion"
	<< "Smooth Endoplasmic Reticulum" 
	<< "Smooth Membrane" 
	<< "Sorting Endosome" 
	<< "Spine Apparatus" 
	<< "Star-shaped Neurofibrillary Tangle" 
	<< "Storage vacuole" 
	<< "Subplasmalemmal Coating" 
	<< "Synaptic Component" 
	<< "Synaptic Vesicle" 
	
	// T:
	<< "Taxi body" 
	<< "Transport Vesicle" 
	<< "Tubular Endosome" 
	
	// V:
	<< "Vacuole" 
	<< "Vesicle" 
	<< "Vesicle Cargo" 
	<< "Vesicle Coat" 
	<< "Vesicle Membrane" 
	<< "Vesicle Other" 
	<< "Vesicle Subcomponent"
	
	//## EXTRA LABELS:
	<< "Axon"
	<< "Crinophagic Body"
	<< "Dendrite"
	<< "Golgi C1"
	<< "Golgi C2"
	<< "Golgi C3"
	<< "Golgi C4"
	<< "Golgi C5"
	<< "Golgi C6"
	<< "Golgi C7"
	<< "Golgi C8"
	<< "Immature Insulin Granule"
	<< "Mature Insulin Granule"
	<< "Golgi Trans-most Cisternae"
	
	<< "UNKNOWN"
	<< "GOLD FIDUCIALS"
	<< "POINT OF INTEREST"
	<< "RULER"
	<< "TOMOGRAM BOUNDARIES"
	
	//## STEREOLOGY-SPECIFIC LABELS:
	<< "STEREOLOGY"
	<< "STEREOLOGY_SETTINGS"
	<< "STEREOLOGY_GRID"
	<< "NO_CATEGORY"
	<< "NOTHING"
	<< "0_INTERSECTIONS"
	<< "1_INTERSECTIONS"
	<< "2_INTERSECTIONS"
	<< "3_INTERSECTIONS"
	<< "4_INTERSECTIONS"
	<< "5_INTERSECTIONS"
	<< "6_INTERSECTIONS"
	<< "7_INTERSECTIONS"
	<< "8_INTERSECTIONS"
	<< "9_INTERSECTIONS"
	<< "10_INTERSECTIONS";
	
}



//############################################################
//## LOAD/SAVE CATEGORIES NAMES FROM CSV FILE:



//------------------------
//-- Tries to open the file "default_stereology_categories.csv" where a default
//-- set of categories are entered, loads these values into the current grid
//-- and refreshes the GUI.

void Stereology::loadDefaultCatNames()
{
	if( getCurrGridSetObj()->countingStarted )
		return;
	
	int catsLoaded = loadCatNamesFromFile( plug.defaultFilePath, getCurrGridSetObj() );
	if( catsLoaded > 0 )
		updateItemListGuiFromCats();
}

//------------------------
//-- Opens a dialog where the user can select a "standard names and colors cvs file"
//-- to load a list of categories into the current grid.

void Stereology::loadCatNamesViaDialog()
{
	QString defName = plug.defaultFilePath;							// suggested file name
	QString filePath = QFileDialog::getOpenFileName( plug.window,"Load Categories .CSV",
																									 defName,"CSV file (*.csv)") ;
	if ( filePath==NULL )
		return;
	
	int catsLoaded = loadCatNamesFromFile( filePath, getCurrGridSetObj() );
	if( catsLoaded > 0 )
		updateItemListGuiFromCats();
}

//------------------------
//-- Opens a dialog where the user can generate any number
//-- of categories for counting in the form "0_INTERSECTIONS",
//-- "1_INTERSECTIONS" and so on.

void Stereology::loadCatNamesForCounting()
{
	static int maxIntersections = 9;
	static string label = "INTERSECTIONS";
	
	CustomDialog ds("Counting Grid", this, BS_NO_YES );
	ds.addHtmlLabel
	( "-----<br>"
	 "This dialog will help you generate categories in the form: <br>"
	 "'<b>0_INTERSECTIONS</b>', '<b>1_INTERSECTIONS</b>' .. and so on. <br>"
	 "Category names starting with a number are used as multipliers <br>"
	 "when calculating metrics such as 'surface area density' (Sv)" );
	ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
	
	ds.addSpinBox ( "max number to go to:", 1, 100, &maxIntersections, 1,
									"You shold enter here the maximum number of intersections  \n"
									"you'd expect based on the length of your line segments.  \n"
									"If you ever count more than 9 we'd recommend reducing \n"
									"the length of segments via the x and/or y spacing." );
	
	ds.addLineEdit( "label (to follow number):", &label,
									"Names will appear as '0_WHATEVER.YOU.ENTER'." );
	
	ds.exec();
	
	if( ds.wasCancelled() )
		return;
	
	setupCountingCategories( maxIntersections, label.c_str() );
}

//------------------------
//-- Saves the list of categores in the current grid to the file
//-- "default_stereology_categories.csv". First however, it shows
//-- an error dialog if there are no categories to save, or
//-- an "are you sure" dialog box to overwrite current values.

void Stereology::saveCatNamesAsDefault()
{
	updateCatsFromItemListGui();
	GridSetObj *g = getCurrGridSetObj();
	
	int numCats = (int)g->catObj.size();
	if( numCats==0 )
	{
		MsgBox( this, "...", "There are no categories to save sorry" );
		return;
	}
	
	if( MsgBoxYesNo( this,
									"Are you sure you want to overwrite the current defaults in "
									"'default_stereology_categories.csv' with these " +toString(numCats)+
									" categories") == false )
		return;
	
	saveCatNamesToFile( plug.defaultFilePath, g );
}

//------------------------
//-- Opens a dialog where the user can save the current list of categories
//-- into a "standard names and colors cvs file" (for future use).

void Stereology::saveCatNamesViaDialog()
{
	updateCatsFromItemListGui();
	GridSetObj *g = getCurrGridSetObj();
	
	int numCats = (int)g->catObj.size();
	if( numCats==0 )
	{
		MsgBox( this, "...", "Sorry, you must have at least one category entere to save" );
		return;
	}
	
	QString filePath = QFileDialog::getSaveFileName( plug.window,"Load Categories .CSV",
																									 "","CSV file (*.csv)") ;
	if ( filePath==NULL )
		return;
	
	saveCatNamesToFile( filePath, g );
}


//------------------------
//-- Loads values from a "standard names and colors cvs file" into the
//-- "catObj" vector in the specified GridSetObj object.
//--
//-- The format of this CSV (comma seperated value) file should be as follows:
//--     (1)Name, (2)Red, (3)Green, (4)Blue, (5)Hyperlink, (6)UniqueID,
//--     (7)Description, (8)Super Category, (9)Synonym(s), (blank),
//--     (11)Sphere Size, (12)Line Width
//-- 
//-- For more information refer to "stereology.html"
//-- 
//-- WARNING: If saved from Excel:Mac may only read one line. Make sure
//--          is saved as "Windows"

int Stereology::loadCatNamesFromFile( QString filePath, GridSetObj *g )
{
  //## TRY TO OPEN FILE:
  
  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
  {
    wprint("\aCould not open file: \n " + filePath.toLatin1() );
    return 0;
  }
  
  //## CLEAR NAME AND NAME LIST AND POPULAR NEW ENTRIES:
  
  g->catObj.clear();
  
  int linesRead = 0;
  QString line;
  while ( !file.atEnd() )
  {
    line = file.readLine(); // line of text excluding '\n'
    linesRead++;
    
    if( line.length() > 1 && line[0] != (QChar)'#' && line[0] != (QChar)',' )
    {
      QStringList list = line.split(",");
      
			CategoryObj newCatObj;
      NameEntry *nameE = &newCatObj.nameEntry;
      
      if( (int)list.size() >= 1 )   nameE->name          = list[0].trimmed();
      if( (int)list.size() >= 2 )   nameE->red           = list[1].trimmed();
      if( (int)list.size() >= 3 )   nameE->green         = list[2].trimmed();
      if( (int)list.size() >= 4 )   nameE->blue          = list[3].trimmed();
      if( (int)list.size() >= 5 )   nameE->hyperlink     = list[4].trimmed();
      if( (int)list.size() >= 6 )   nameE->identifier    = list[5].trimmed();
      if( (int)list.size() >= 7 )   nameE->description   = list[6].trimmed();
      if( (int)list.size() >= 8 )   nameE->superCat      = list[7].trimmed();
      if( (int)list.size() >= 9 )   nameE->synonyms      = list[8].trimmed();
      if( (int)list.size() >= 11)
			{
				if( !list[10].isEmpty() )
					newCatObj.sphereSize = list[10].trimmed().toInt();
			}
			if( (int)list.size() >= 12)
			{
				if( !list[11].isEmpty() )
					newCatObj.lineWidth = list[11].trimmed().toInt();
			}
			
			if( nameE->name.length() < 1 )
				continue;
			
			newCatObj.gridSetId    = g->gridSetId;
			newCatObj.categoryName = nameE->name;
			newCatObj.color        = QColor( nameE->red.toInt(),
													           	 nameE->green.toInt(),
															         nameE->blue.toInt() );
			
			QString colorStr  = "(" + nameE->red +","+ nameE->green + "," + nameE->blue + ")";
			colorStr += " <font color='" + newCatObj.color.name() + "'><b>|||||</b></font>";
			newCatObj.toolTip = "<b>Name:</b> " + newCatObj.categoryName + "<br>"
													"<b>Color:</b> " + colorStr + "<br>"
													"<b>Hyperlink:</b> <a href='" + nameE->hyperlink +
												  "'>" + nameE->hyperlink + "</a><br>"
			                    "<b>Description:</b> " + nameE->description + "<br>"
													"<b>Super-category:</b> " + nameE->superCat + "<br>"
													"<b>Synonyms:</b> " + nameE->synonyms + "<br><br>"
													"<b>Sphere size:</b> " + QStr(newCatObj.sphereSize) + "<br>"
													"<b>Line width:</b> " + QStr(newCatObj.lineWidth) + "<br>";
			
			g->catObj.push_back( newCatObj );
    }
  }
  file.close();				// close file
	
  //## SHOW OUTCOME:
  
  string filePathStr = qStringToString(filePath);
  wprint("");
  wprint("STEREOLOGY: %d names loaded from", (int)g->catObj.size() );
	wprint(" '%s' ", filePathStr.c_str() );
  
  if( linesRead == 0 )
  {
		QMessageBox::critical( this, "Loading Failed",
												   "WARNING: This file appears to be empty: \n" 
												   + filePath );
    return 0;
  }
  if( linesRead == 1 )
  {
    QMessageBox::critical( this, "Loading Failed",
												   "WARNING: Could only read one line in the file \n"
												   "  You may have to save it as a Windows .csv file" );
  }
	
  return (int)g->catObj.size();
}


//------------------------
//-- Save category values from the current grid to a 
//-- "standard names and colors cvs file". The format of
//-- this CSV (comma seperated value) file is described above.

bool Stereology::saveCatNamesToFile( QString filePath, GridSetObj *g )
{
  //## TRY TO OPEN FILE FOR WRITING:
  
  QFile file( filePath );
  if ( !file.open( QIODevice::WriteOnly | QIODevice::Text ) )
  {
    wprint("\aCould not open file: \n " + filePath.toLatin1() );
    return (false);
  }
  
  //## WRITE OUT LINES:
  
  QTextStream out(&file);
	out << "#LIST OF CATEGORIES FOR IMOD STEREOLOGY PLUGIN:\n";
  out << "#Name(*),Red(*),Green(*),Blue(*),Hyperlink,UniqueID,Description,";
	out << "Super Category,Synonym(s),Sphere Size(*):\n";
	
	for( int c=0; c<(int)g->catObj.size(); c++)
	{
		CategoryObj *catObj = &g->catObj[c];
		
		out << catObj->categoryName  << ",";
		out << catObj->color.red()   << ",";
		out << catObj->color.green() << ",";
		out << catObj->color.blue()  << ",";
		out << catObj->nameEntry.hyperlink   << ",";
		out << catObj->nameEntry.identifier  << ",";
		out << catObj->nameEntry.description << ",";
		out << catObj->nameEntry.superCat    << ",";
		out << catObj->nameEntry.synonyms    << ",,";
		out << catObj->sphereSize    << ",";
		out << catObj->lineWidth     << "\n";
	}
	
	file.flush();
  file.close();			// close file
	
  //## SHOW OUTCOME:
  
  string filePathStr = qStringToString(filePath);
  
  wprint("STEREOLOGY: %d categories saved to", (int)g->catObj.size() );
	wprint(" '%s'\n", filePathStr.c_str() );
  	
  return (true);
}




//############################################################
//## LOAD/SAVE GRIDS FROM IMOD MODEL:



//------------------------
//-- Wrapper method which calls "loadGridFromImodModel" and gives
//-- the user to reload a grid from the IMOD model.

void Stereology::reloadGridFromImodModel()
{
	loadGridFromImodModel( false );
}


//------------------------
//-- Allows user to load grids (and all their categories) from the IMOD
//-- model file. To do this it calls "loadAllGridsAndCatsFromImodObjs"
//-- and if grids were found and loaded successfully into "plug.gridList", 
//-- it presents a dialog where the user can see a list of these grids
//-- (although typically there will be no more than one) and opt to
//-- load one as the "current grid" - which he can then continue
//-- working on. If the user clicks okay, all loaded grids are moved from
//-- "plug.grid", and the grid the user selected become the current grid
//-- by setting "plug.currGridIdx" and by loading all points from the
//-- corresponding objects - which can be a bit slow if there are many points.
//-- 
//-- If not grids are found the function will show a popup
//-- if "repressPopupIfNoGrids" is true... or a subtle message
//-- the main window output if "repressPopupIfNoGrids" is off.
//-- 
//-- Returns false if no grid objects exist or user cancels loading, or
//-- returns true if a new grid loaded.

bool Stereology::loadGridFromImodModel( bool repressPopupIfNoGrids )
{
	int numGridsLoaded = loadAllGridsAndCatsFromImodObjs( true );
					// load the "meta data" for all grid settings and category objects
					//  in the imod model into "plug.gridList", without setting up points
					//  (as this can be slow).
	
	
	//## IF NO GRIDS WERE FOUND OR LOADED FROM IMOD MODEL, ADD
	//## A SINGLE GRID USING PLUGIN DEFAULTS AND EXIT EARLY:
	
	if( numGridsLoaded==0 )
	{
		if( repressPopupIfNoGrids )
		{
			wprint( "STEREOLOGY: No 'STEREOLOGY.GRID' objects found\n" );
		}
		else
		{
			MsgBox( this, "Whoops!",
						  "Sorry - no 'STEREOLOGY.GRID' objects were found in this model.\n\n"
							"Click the 'Help' button for more info." );
		}
		
		return (false);
	}
	
	//## IF DESIRED: PRINT GRIDS JUST LOADED INTO CONSOLE:
	
	if(plug.showLoadingInConsole)
	{
		cout <<endl<< "STEREOLOGY PLUGIN LOADED THESE GRIDS FROM THE MODEL:" <<endl;
		for( int i=0; i<(int)plug.gridList.size(); i++ )
		{
			plug.gridList[i].calcVals();
			cout << plug.gridList[i].toString( true ).toStdString() <<endl<<endl;
		}
	}
	
	
	//## GENERATE A CUSTOM DIALOG TO SHOW THE USER WHAT GRID/GRIDS
	//## WERE FOUND AND LET HIM CHOSE IF HE TO ONE OF THESE FROM THE MODEL:
	
	static bool loadPts = true;
	static int gridIdxToLoad = 0;
	
	if( gridIdxToLoad <= numGridsLoaded )
		gridIdxToLoad = numGridsLoaded-1;
	
	QString optionsStr;
	QString optToolTipStr;
	for( int i=0; i<(int)plug.gridList.size(); i++ )
	{
		GridSetObj *g = &plug.gridList[i];
		optionsStr += "grid set '" + g->gridSetId + "' ";
		optionsStr += "\t spacing: " + QStr(g->xSpacing) + ", " + QStr(g->ySpacing);
		optionsStr += ", " + QStr(g->zSpacing);
		g->calcVals();
		optToolTipStr += g->toString( true );
		
		if( i<(int)plug.gridList.size() )
		{
			optionsStr    += "|";
			optToolTipStr += "|";
		}
	}
	
	CustomDialog ds( (repressPopupIfNoGrids) ? "Load Grid from Model" : "Reload Grid",
									 this, BS_NO_YES);
	
	if(repressPopupIfNoGrids)
	{
		ds.addHtmlLabel("The following grids are setup in your model:",
		 	  						"Click 'yes' to load grid.. or click 'no' to keep \n"
		  							"the grid which is currently showing" );
		ds.setStylePrev("background-color: rgb(100, 255, 100);");			// light green
	}
	
  ds.addRadioGrp( "Select grid to load:", optionsStr,
								  &gridIdxToLoad, "Mouse over each to see more info", optToolTipStr );
	
	ds.addCheckBox( "Load point values", &loadPts,
								  "Almost always you'll want to load any point values, \n"
								  "but since loading point values is slow this option is \n"
								  "presented if you just want to view the grid, but not modify \n"
								  "or analyze results \n\n"
								  "RECOMMENDED VALUE: on" );
	
	ds.addHtmlLabel("Would you like to load selected grid?",
									"Click 'yes' to load grid.. or click 'no' to keep \n"
									"the grid which is currently showing" );
	ds.setStylePrev("background-color: rgb(100, 255, 100);");			// light green
	
	ds.exec();
	if( ds.wasCancelled() )
		return (false);
	
	
	
	//## LOAD POINTS FROM THE CHOOSEN GRID AND SHOW A SUMMARY OF
	//## HOW MANY POINTS WERE LOADED WITHIN A SMALL DIALOG:
	
	plug.grid        = plug.gridList;
	plug.currGridIdx = gridIdxToLoad;
	getCurrGridSetObj()->calcVals();
	updateGridGuiFromGrid( true );		// update the grid GUI
	
	if( loadPts )
	{
		long numContPtsLoaded  = 0;
		long numCheckedPts     = 0;
		long numUnmatchedPts   = 0;
		bool makeSureGridSetup = !getCurrGridSetObj()->allowRandomPts();
		
		loadGridPtsFromImodObjs( getCurrGridSetObj(), makeSureGridSetup, true, true,
														 &numContPtsLoaded, &numCheckedPts, &numUnmatchedPts );
		
		CustomDialog dsL( "Points Loaded:",this, BS_OKAY_ONLY);
		
		dsL.addHtmlLabel( "<b>" + QStr(numCheckedPts) + "</b> checked stereology points "
										 "were just loaded into the grid." );
		
		dsL.addHtmlLabel( "" );
		dsL.addHtmlLabel( "<b>OVER THE GIVEN OBJECTS:</b>." );
		
		dsL.addHtmlLabel( "> <b>" + QStr(numContPtsLoaded) + "</b> contour points were "
										  "matched to the grid." );
		
		dsL.addHtmlLabel( "> <b>" + QStr(numUnmatchedPts) + "</b> contour points failed "
										 "to load as they <br>could not be matched." );
		
		if(numUnmatchedPts>0)
		{
			dsL.addHtmlLabel( "<b>NOTE</b>: When contour points fail to load into  <br>"
											  "the grid it's because they fail to match the points <br>"
											  "specified by the grid's setting object. More info <br>"
											  "on this should be printed to the console");
			dsL.setStylePrev("background-color: rgb(255, 40, 40);");			// red
		}
		
		dsL.exec();
	}
	
	//## UPDATE TE GUI TO REFLECT THE CHOSEN GRID AND ITS CATEGORES
	//## LOADED FROM IMOD:
	
	updateGridGuiFromGrid( true );		// update the grid GUI
	updateItemListGuiFromCats();			// update the item list of categories (in the GUI)
	
	drawGridObject ( true );
	drawBlackObject( true );
	
	return (true);
}

//------------------------
//-- Attempts to load grids and all their categories from the IMOD
//-- model into the "plug.gridList" vector. Grid setting objects are identified
//-- as starting with "STEREOLOGY.GRID" and categories identified
//-- as containing "(STEREOLOGY)" in their name. Categories are assiged
//-- to grid objects based on a matching grid set id - eg: "grid_set=1".
//-- If a cateogry has no matching id... or if a setting object
//-- does not correctly set all variables in the format expected, errors
//-- are output to the console, and if "showPopupIfErrors" is on, a message box
//-- appears too.
//-- 
//-- When any grid setting or category is loaded successfully its "objIdx" 
//-- value is set to point to its matching IMOD object. Note that this
//-- function does NOT load or look at any points or contours - only the 
//-- names of objects are assessed.
//-- 
//-- When done, this function returns the number of (valid) grid setting objects
//-- loaded successfully into "plug.gridList". 

int Stereology::loadAllGridsAndCatsFromImodObjs( bool showPopupIfErrors )
{
  Imod *imod  = ivwGetModel(plug.view);
  
	
  //## SEARCH FOR "STEREOLOGY.GRID" OBJECTS AND ADD THEM TO "plug.gridList":
  
	QString outStr;
	int badObjectsFound = 0;
	
	plug.gridList.clear();
	
  for(int o=0; o<osize(imod); o++)
  {
    Iobj *obj  = getObj(imod,o);
		QString objName  = (QString)( imodObjectGetName(obj) );
		QString objLabel = getObjLabel(obj);
		QString objLabelAndName = objLabel + objName;
		
		//## IF "STEREOLOGY.GRID" OBJECT FOUND: EXTACT VARIABLE VALUES
		
		if( objLabel.startsWith( "STEREOLOGY.GRID", Qt::CaseSensitive ) ||
			  objName.startsWith ( "STEREOLOGY.GRID", Qt::CaseSensitive ) )
		{
			GridSetObj gridObj;
			gridObj.objIdx = o;
			
			QString errPrefix = "> Object " + QStr(o+1) + ": ";
			
			if( !objName.startsWith( "STEREOLOGY.GRID", Qt::CaseSensitive ) )
				outStr += errPrefix + "object name *should* start 'STEREOLOGY.GRID'\n";
			if( !objLabel.startsWith( "STEREOLOGY.GRID", Qt::CaseSensitive ) )
				outStr += errPrefix + "object label must start with 'STEREOLOGY.GRID'\n";
			
			QStringList varList = objLabelAndName.split('#');
			for(int i=1; i<varList.size(); i++)
			{
				QString varStr = varList[i];
				
				int posEqualSign = varStr.indexOf('=');
				
				QString varName = varStr.left(posEqualSign);
				QString args    = varStr.mid(posEqualSign+1);
				QStringList argsList = args.split(',');
				
				if( varStr == "multi_cats" )								// multi_cats
				{
					gridObj.allowMultipleCats = true;
				}
				else if( varStr == "allow_intercepts" )			// allow_intercepts
				{
					gridObj.allowIntercepts = true;
				}
				else if( varStr == "(DONT_RENAME)"									// (DONT_RENAME)
				      || varStr.startsWith( "STEREOLOGY.GRID" )			// STEREOLOGY.GRID 
							|| varStr == "" )															// blank
				{
					continue;
				}
				else if( varStr == "(DONT_RENAME)" )				// (DONT_RENAME)
				{
					continue;
				}
				
				else if( varName == "sub" )									// sub   (subsample rectangles)
				{
					if( argsList.size()==4 )
					{
						gridObj.rXSpan = argsList[0].toInt();
						gridObj.rYSpan = argsList[1].toInt();
						gridObj.rXGap  = argsList[2].toInt();
						gridObj.rYGap  = argsList[3].toInt();
						gridObj.showSubRects = true;
					}
					else
					{
						outStr += errPrefix + "'sq' (subsample rectangles) argument should ";
						outStr += "look like 'sq=600,600,1000,1000' \n";
						outStr += "          (x span, y span, x gap, y gap) \n";
					}
				}
				
				else if( varName == "type" )
				{
					if( argsList.size()==1 )
					{
						int gridType = gridObj.getMatchingGridType( argsList[0] );		
						
						if( gridType != -1 )								// returns -1 if no match
							gridObj.gridType = gridType;
						else
							outStr += errPrefix + "'type' is not recognized: '" + varStr + "'\n";
					}
					else
					{
						outStr += errPrefix + "'type' argument should look like 'type=pts' \n";
					}
				}
				
				else if(varName=="grid_set")								// grid_set=1
				{
					gridObj.gridSetId = args;
				}
				
				else if(varName=="spacing")									// grid_spacing=50,50,5
				{
					if( argsList.size()==3 ) {
						gridObj.xSpacing = argsList[0].toFloat();
						gridObj.ySpacing = argsList[1].toFloat();
						gridObj.zSpacing = argsList[2].toInt();
					}
					else {
						outStr += errPrefix + "'spacing' argument should look like '=50,50,5' ";
						outStr += " (xSpacing,ySpacing,zSpacing)\n";
					}
				}
				
				else if(varName=="xspacing")								// xspacing=1
				{
					if( argsList.size()==1 )
						gridObj.xSpacing = argsList[0].toFloat();
					else
						outStr += errPrefix + "'xspacing' should look like 'xspacing=25'\n";
				}
				else if(varName=="yspacing")								// yspacing=1
				{
					if( argsList.size()==1 )
						gridObj.ySpacing = argsList[0].toFloat();
					else
						outStr += errPrefix + "'yspacing' should look like 'yspacing=25'\n";
				}
				else if(varName=="zspacing")								// zspacing=1
				{
					if( argsList.size()==1 )
						gridObj.zSpacing = argsList[0].toInt();
					else
						outStr += errPrefix + "'zspacing' should look like 'zspacing=25'\n";
				}
				
				else if(varName=="x")												// x=0,1014
				{
					if( argsList.size()==2 ) {
						gridObj.xMin = argsList[0].toInt();
						gridObj.xMax = argsList[1].toInt();
						if( gridObj.xMin >= gridObj.xMax )
							outStr += errPrefix + "xMin should be < xMax\n";
					}
					else {
						outStr += errPrefix + "'x' argument should look like 'x=0,1014'\n";
					}
				}
				
				else if(varName=="y")												// y=0,1014
				{
					if( argsList.size()==2 ) {
						gridObj.yMin = argsList[0].toInt();
						gridObj.yMax = argsList[1].toInt();
						if( gridObj.yMin >= gridObj.yMax )
							outStr += errPrefix + "yMin should be < yMax\n";
					}
					else {
						outStr += errPrefix + "'y' argument should look like 'y=0,1014'\n";
					}
				}
				
				else if(varName=="z")												// z=10,190
				{
					if( argsList.size()==2 ) {
						gridObj.zMin = argsList[0].toInt();
						gridObj.zMax = argsList[1].toInt();
						if( gridObj.zMin > gridObj.zMax )
							outStr += errPrefix + "zMin should be <= zMax\n";
					}
					else {
						outStr += errPrefix + "'z' argument should look like 'z=10,190'\n";
					}
				}
				
				else																				// anything else!
				{	
					outStr += errPrefix + "the string '" + varName + "' was not recognized\n";
				}
				
			}
			
			//## VERIFY THAT ALL VARIABLES WERE SET AND SETUP POINTS:
			
			bool allValsValid = gridObj.verifyVals();
			
			if( !allValsValid )
			{
				outStr += errPrefix + "not all variables set correctly\n";
				outStr += "     ... this object could not be added\n";
				outStr += "     name:  '" + objName + "'\n";
				outStr += "     label: '" + objLabel + "'\n\n";
				badObjectsFound++;
			}
			else {
				outStr += errPrefix + " was verified\n";
				plug.gridList.push_back( gridObj );
			}
			
		}
	}
	
	
	//## SEARCH FOR "(STEREOLOGY)" OBJECTS AND ADD TO MATCHING GridSetObj:
  
	int totalCatObjsAdded = 0;
	
  for(int o=0; o<osize(imod); o++)
  {
    Iobj *obj  = getObj(imod,o);
		QString objName  = (QString)( imodObjectGetName(obj) );
		QString objLabel = getObjLabel(obj);
		QString objLabelAndName = objLabel + objName;
		
		//## IF "STEREOLOGY.GRID" OBJECT FOUND: EXTACT VARIABLE VALUES
		
		if( objLabel.contains( "(STEREOLOGY)", Qt::CaseSensitive ) || 
			  objName.contains ( "(STEREOLOGY)", Qt::CaseSensitive ) )
		{
			CategoryObj catObj;
			catObj.objIdx = o;
			
			QString errPrefix = "> Object " + QStr(o+1) + ": ";
			
			if( !objName.contains( "(STEREOLOGY)", Qt::CaseSensitive ) )
				outStr += errPrefix + "object name should contain '(STEREOLOGY)' \n";
			if( !objLabel.contains( "(STEREOLOGY)", Qt::CaseSensitive ) )
				outStr += errPrefix + "object label does not contain '(STEREOLOGY)' keyword  \n";
			
			//## GET CATEGORY NAME STRING:
			
			int posPeriod = objName.indexOf('.');
			int posHash   = objName.indexOf('#');
			if( posPeriod == -1 ) {
				outStr += errPrefix + "there should be a logical name then a period (.) ";
				outStr += "before '#(STEREOLOGY)#grid_set=1#'\n";
			}
			if( posPeriod == -1 && posHash > 1 ) {
				posPeriod = posHash;
			}
			
			catObj.objName  = objName;
			catObj.categoryName = (posPeriod==-1) ? objName
														                : objName.left( posPeriod ).trimmed();
			
			//## GET COLOR:
			
			float red, green, blue;
			imodObjectGetColor( obj, &red, &green, &blue );
			catObj.color.setRgb( red*255, green*255, blue*255 );
			
			//## GET THE "gridSetId" NUMBER:
			
			QStringList varList = objLabelAndName.split('#');
			for(int i=1; i<varList.size(); i++)
			{
				QString varStr = varList[i];
				if( varStr.startsWith("grid_set=") )
					catObj.gridSetId = varStr.mid( 9 );				// get chars after '='
			}
			
			//## ADD CATEGORY OBJECT TO MATCHING "gridSetId" OR PRINT ERROR:
			
			bool matchingGridSetFound = false;
			
			for(int g=0; g<plug.gridList.size(); g++)
			{
				if( plug.gridList[g].gridSetId == catObj.gridSetId )
				{
					plug.gridList[g].catObj.push_back( catObj );
					matchingGridSetFound = true;
					totalCatObjsAdded++;
					break;
				}
			}
			
			//## IF NO MATCH WAS FOUND: PRINT ERROR:
			
			if( !matchingGridSetFound )
			{
				outStr += errPrefix + "(grid_set=" + catObj.gridSetId + ") ";
				outStr += "couldn't be matched to a valid 'STEREOLOGY.GRID' object\n";
				outStr += "     name:  '" + objName + "'\n";
				outStr += "     label: '" + objLabel + "'\n\n";
			}
			
			if( !matchingGridSetFound || posPeriod == -1 )
			{
				badObjectsFound++;
			}
		}
	}
	
	
	//## IF BAD OBJECTS WERE FOUND: PRINT ERROR MESSAGES
	
	if( badObjectsFound>0 )
	{
		if( showPopupIfErrors )
			QMessageBox::critical(this, "ERROR",
														"Objects marked as 'STEREOLOGY' objects were detected "
														"in your model, but several errors were found with these "
														"objects. These errors will be printed to the console.");
		
		cerr <<endl<<endl;
		cerr << "STEREOLOGY PLUGIN FOUND THESE ERRORS IN YOUR MODEL:" <<endl;
		cerr << outStr.toStdString() <<endl;
	}
	else if( plug.showLoadingInConsole )
	{
		cerr << outStr.toStdString() <<endl;
	}
	
	return( plug.gridList.size() );
}



//------------------------
//-- Takes in a GridSetObj ("g"), and tries to load in the value of
//-- stereology points via the IMOD objects that the GridSetObj and
//-- all it's categories point to (via their "objIdx" variable).
//-- If "makeSureGridSetup" is true, the very first thing this function
//-- does is initalizes it's vector of points (g->pts) by calling
//-- "g->setupPts()". If grid points are setup, the next set involves
//-- searching the "STEREOLOGY.GRID" object and for every
//-- contour point which matches a stereology point, this point is
//-- set as checked. For each "(STEREOLOGY)" (category) object
//-- if a point is matched to a stereology point, the matching
//-- category is turned on for that point.
//-- 
//-- NOTE: If "g->allowRandomPts()" is true, set "makeSureGridSetup" to
//--       false, and instead of using grid settings to setup points
//--       the contour points in the "STEREOLOGY.GRID" object are
//--       use to populate the vector of stereology points (g->pts).
//-- 
//-- If "printResultsConsole" is on, the results of loading is output
//-- to the console (regardless of error), and if "showPopupIfErrors" is on,
//-- a popup message is shown if any of the objects are found to be invalid.
//-- 
//-- After done, this method returns the number of all contour points
//-- accross all objects which were successfully matched (and loaded) via
//-- "numContPtsLoaded", the number of checked points loaded/matched
//-- from the "STEREOLOGY.GRID" object is returned via "numCheckedPts"
//-- and the total contour points accross all object which didn't match
//-- any stereology point in the grid is returned via "numUnmatchedPts".
//-- 
//-- If all contour points that could have been matched were matched and
//-- loaded this method returns true, else (if "unmatched" contour points
//-- were found) it returns false.

bool Stereology::loadGridPtsFromImodObjs( GridSetObj *g, bool makeSureGridSetup,
																				 bool printResultsConsole, bool showPopupIfErrors,
																				 long *numContPtsLoaded, long *numCheckedPts,
																				 long *numUnmatchedPts )
{
  Imod *imod  = ivwGetModel(plug.view);
  
	QString outStr       = "";	// |-- keeps track of errors
	int numInvalidObjs   = 0;		// |
	
	*numContPtsLoaded = 0;	// returns the total contour points across all objects which
													//  match an actual stereology point in the grid
	*numCheckedPts   = 0;		// counts the number of checked points loaded into the grid
													//  via the "settings" object
	*numUnmatchedPts = 0;		// returns the total contour points across all objects which
													//  do not match an actual stereology point in the grid
	
	long catHits     = 0;		// counts the number of categories ticked over all points
	
	long badPtsInObj  = 0;	// used to tally the number contour points which don't match
													//  for one object at a time.
	long goodPtsInObj = 0;	// used to tally the number of matched points per object.
	
	string ptLdStr   = "";	// used to record how many points were loaded or not loaded
													//  for each object
	
	int numCats   = (int)g->catObj.size();		// number of categories
	
	
	//## IF GRID SETTING CONTAINS FORBIDDEN SQUARES, ASK USER IF HE
	//## WANTS TO APPLY THESE SQUARES:
	
	bool useSubRectsIfOn = false;
	
	if( g->showSubRects )
	{
		useSubRectsIfOn = MsgBoxYesNo( this, "This grid contains subsample rectangles..."
																				 "Would you like to omit points "
																				 "outside these squares?" );
	}
	
	//## SETUP/INITIALIZE GRID POINTS:
	
	if( makeSureGridSetup )
	{
		if( g->setupPts( useSubRectsIfOn ) == false )
			cerr << "ERROR: loadGridPtsFromImodObjs() - couldn't set up grid points" << endl;
	}
	
	
	//## IF POINTS ARE SETUP, USE "STEREOLOGY.GRID" OBJECT TO MARK WHICH ARE CHECKED:
	if( g->areMaxPtsSetup() )
	{
		if( isStereologyObj(g->objIdx) )
		{
			//if( g->pts.size() > 0 ) {
			//	cout<<"top most point: ";
			//	cout<<g->pts[0].pos.x<<","<<g->pts[0].pos.y<<","<<g->pts[0].pos.z<<endl;
			//}
			
			badPtsInObj  = 0;
			goodPtsInObj = 0;
			
			Iobj *obj = getObj(imod, g->objIdx);
			for( int c=0; c<csize(obj); c++ )
			{
				for( int p=0; p<psize( getCont(obj,c) ); p++)
				{
					Ipoint *pt  = getPt( getCont(obj,c), p );
					Spoint *spt = g->getSPtByPt( pt, true );
					
					if( spt!=NULL )
					{
						spt->checked = true;			// set as checked
						*numCheckedPts += 1;
						goodPtsInObj++;
					}
					else
					{
						badPtsInObj++;
					}
				}
			}
			
			long totPtsInObj   = goodPtsInObj + badPtsInObj;
			*numContPtsLoaded += goodPtsInObj;
			*numUnmatchedPts  += badPtsInObj;
			ptLdStr += " > obj " + toString(g->objIdx+1) + " (settings): \t";
			if(totPtsInObj==0)	ptLdStr += "contains no points";
			else								ptLdStr += toString( goodPtsInObj ) + " pts loaded\t";
			if(badPtsInObj>0)		ptLdStr += toString( badPtsInObj  ) + " pts failed to load";
			ptLdStr += "\n";
		}
		else
		{
			outStr += "> object " + QStr( g->objIdx+1 ) + "\n";
			numInvalidObjs++;
		}
		
	}
	//## ELSE (IF POINTS NOT SETUP): USE "STEREOLOGY.GRID" OBJECT TO ADD POINTS:
	else
	{
		outStr += "Points in grid set '" + g->gridSetId + "' are not setup, ";
		outStr += "so will try loading\n";
		if( isStereologyObj(g->objIdx) )
		{
			Iobj *obj = getObj(imod, g->objIdx);
			for( int c=0; c<csize(obj); c++ )
				for( int p=0; p<psize( getCont(obj,c) ); p++)
				{
					Ipoint *pt = getPt( getCont(obj,c), p );
					Spoint newSpt( pt->x, pt->y, pt->z, numCats );
					newSpt.checked = true;			// set as checked
					g->pts.push_back( newSpt );
					*numCheckedPts += 1;
				}
		}
		else
		{
			outStr += "> object " + QStr( g->objIdx+1 ) + "\n";
			numInvalidObjs++;
		}
	}
	
	//## FOR EACH CATEGORY: GO THROUGH ALL POINTS AND IF A MATCHING STEREOLOGY POINT
	//## IS FOUND IN THAT GRID, MARK THE CATEGORY AS ON FOR THAT STEREOLOGY POINT 
	
	int catsLoaded = 0;
	
	for(int j=0; j<numCats; j++)
	{
		CategoryObj *catObj = &g->catObj[j];
		
		if( isStereologyObj(catObj->objIdx) )
		{
			badPtsInObj  = 0;
			goodPtsInObj = 0;
			
			Iobj *obj = getObj(imod, catObj->objIdx);
			
			for( int c=0; c<csize(obj); c++ )
			{
				for( int p=0; p<psize( getCont(obj,c) ); p++)
				{
					Ipoint *pt = getPt( getCont(obj,c), p );
					Spoint *spt = g->getSPtByPt( pt, true );
					if( spt!=NULL )
					{
						spt->setCatOn(j,true);
						catHits++;
						goodPtsInObj++;
					}
					else
					{
						badPtsInObj++;
					}
				}
			}
			
			long totPtsInObj   = goodPtsInObj + badPtsInObj;
			*numContPtsLoaded += goodPtsInObj;
			*numUnmatchedPts  += badPtsInObj;
			ptLdStr += " > obj " + toString(catObj->objIdx+1);
			ptLdStr += " (category " + toString(j+1) + "): \t";
			if(totPtsInObj==0)	ptLdStr += "contains no points";
			else								ptLdStr += toString( goodPtsInObj ) + " pts loaded\t";
			if(badPtsInObj>0)		ptLdStr += toString( badPtsInObj  ) + " pts failed to load";
			ptLdStr += "\n";
		}
		else
		{
			outStr += "> object " + QStr( catObj->objIdx+1 ) + "\n";
			numInvalidObjs++;
		}
		
		if( *numContPtsLoaded > 0 )
			g->countingStarted = true;
	}
	
	
	//## IF DESIRED: PRINT RESULTS TO CONSOLE
		
	if( showPopupIfErrors && numInvalidObjs > 0 )
	{
		QMessageBox::critical(this, "ERROR",
							"The objects listed below no longer contain the keyword 'STEREOLOGY'\n\n"
							"WARNING: Never rename, edit or reorder 'STEREOLOGY' objects.\n\n"
							+ outStr );
	}
	
	if( printResultsConsole )
	{
		cout <<endl<<endl << "STEREOLOGY PLUGIN LOADED THESE POINTS:" << endl;
		cout << "gridSetId     = '" << g->gridSetId.toStdString() << "'" <<  endl;
		cout << "# checked points found  = " << *numCheckedPts << endl;
		cout << "# category hits  found  = " << catHits << endl;
		cout << "# contour points loaded = " << *numContPtsLoaded << endl;
		cout << "# contour points which could not be loaded = " << *numUnmatchedPts << endl;
		cout << ptLdStr << endl;
	}
	
	return (*numUnmatchedPts == 0);
}



//------------------------
//-- Takes the current grid and it's vector of categories, and uses this
//-- to setup IMOD objects - including one which represents the grid settings
//-- and one for each category.
//-- 
//-- All new objects are set as scattered point objects, and their labels
//-- are set as per the example shown at the top of this document...
//-- and copied here:
//-- 
//--  >  "STEREOLOGY.GRID  #grid_set=1#type=pts1#spacing=50,50,5#x=10,1014#y=10,1014
//--      #z=10,190#type=pts#(DONT_RENAME)#" 
//--         -- to represent grid settings
//--
//--  >  "NO_CATEGORY.     #(STEREOLOGY)#grid_set=1#"
//--         -- to represent a category (in this grid).
//-- 
//-- If the grid settings and/or any category was loaded from an object,
//-- then this object should be referenced by the "objIdx" variable which
//-- exists in both the GridSetObj and CategoryObj class and is otherwise
//-- set to -1 as the default value. If the value is -1 or points to an
//-- object which isn't a "STEREOLOGY" object a new object is created...
//-- however if it points to a valid stereology object then object is either
//-- ignored if "skipIfExists" is true, the name and colors changed if
//-- "overwriteNames" is true... or duplicated if both are false, (which
//-- you probably want to avoid.
//-- 
//-- This function does not alter contours or points in any ways - it's
//-- just used to create and/or set the name, colors and sphere size of
//-- object.

bool Stereology::setupImodObjsFromGridAndCats( bool skipIfExists, bool overwriteNames,
																			         int *numObjAdded, int *numObjChanged )
{
	GridSetObj *g = getCurrGridSetObj();
	Imod *imod  = ivwGetModel(plug.view);
	
	*numObjAdded = 0;
	*numObjChanged = 0;
	
	int sphOneSecOnly = (plug.showGridEverySlice || g->zSpacing < 10) ? 1 : 0;
				// if z spacing is small we should only want them showing on one section
	
	
	//## CREATE A NEW "STEREOLOGY.GRID" OBJECT:
	
	if( !(skipIfExists && isStereologyObj(g->objIdx)) )
	{
		Iobj *obj;				// object we wish to modify (and probably add first)
		if( overwriteNames && isStereologyObj(g->objIdx) )
		{
			obj = getObj(imod, g->objIdx);
			*numObjChanged += 1;
		}
		else
		{
			int error = imodNewObject(imod);			// create new object
			if(error == 1) {
				wprint("\aError creating object for 'STEREOLOGY.GRID'");
				return false;
			}
			g->objIdx = osize(imod)-1;
			obj = getObj(imod, g->objIdx);
			*numObjAdded += 1;
		}
		
		imodObjectSetValue(obj, IobjLineWidth, 2);
		imodObjectSetValue(obj, IobjFlagConnected, 0);		// scattered point object
		imodObjectSetValue(obj, IobjPointSize, 0);				// zero sphere size
		imodObjectSetValue(obj, IobjFlagPntOnSec, sphOneSecOnly);
		
		setObjColor( obj, 255, 255, 255 );			// white
		
		QString newObjName = "STEREOLOGY.GRID #grid_set=" + g->gridSetId;
		newObjName += "#type=" + g->gridTypeAsString() + "#";
		
		QString newObjLabel = "STEREOLOGY.GRID #grid_set=" + g->gridSetId;
		newObjLabel += "#type=" + g->gridTypeAsString();
		newObjLabel += "#spacing=" + QStr(g->xSpacing);
		newObjLabel += "," + QStr(g->ySpacing);
		newObjLabel += "," + QStr(g->zSpacing);
		newObjLabel += "#x=" + QStr(g->xMin) + "," + QStr(g->xMax);
		newObjLabel += "#y=" + QStr(g->yMin) + "," + QStr(g->yMax);
		newObjLabel += "#z=" + QStr(g->zMin) + "," + QStr(g->zMax);
		newObjLabel += (g->allowMultipleCats) ? "#multi_cats" : "";
		newObjLabel += (g->allowIntercepts)   ? "#allow_intercepts" : "";
		if( g->showSubRects ) {
			newObjLabel += "#sub=" + QStr(g->rXSpan) + "," + QStr(g->rYSpan);
			newObjLabel += "," + QStr(g->rXGap) + "," + QStr(g->rYGap);
		}
		newObjLabel += "#(DONT_RENAME)#";
		
		imodObjectSetName( obj, (char *)newObjName.toStdString().c_str() );
		
		if( setObjLabel( obj, newObjLabel )==false )
			cout << "Error setting label for object " << (g->objIdx+1) << endl;
	}
	
	
	//## CREATE A NEW OBJECT FOR EACH CATEGORY:
  
  for( int i=0; i<g->catObj.size(); i++ )
  {
		CategoryObj &catObj = g->catObj[i];
		
		if( skipIfExists && isStereologyObj(catObj.objIdx) )
			continue;
		
		Iobj *obj;				// object we wish to modify (and probably add first)
		if( overwriteNames && isStereologyObj(catObj.objIdx) )
		{
			obj = getObj(imod, catObj.objIdx);
			*numObjChanged += 1;
		}
		else
		{
			int error = imodNewObject(imod);			// add new object
			if(error == 1) {
				wprint("\aError creating new '(STEREOLOGY)' object");
				return false;
			}
			catObj.objIdx = osize(imod)-1;
			obj = getObj(imod, catObj.objIdx );
			*numObjAdded += 1;
		}
    
		imodObjectSetValue(obj, IobjFlagConnected, 0);					// scattered point object
		
		int sphereSize = catObj.sphereSize;
		if(sphereSize < 0)											// if sphere size not setup (still -1):
			sphereSize = plug.gridSymbolSize+i;		// increment sphere size starting
																						//  at "plug.gridSymbolSize"
    imodObjectSetValue(obj, IobjPointSize, sphereSize);		// point size (increments)
		imodObjectSetValue(obj, IobjLineWidth2, catObj.lineWidth);
		imodObjectSetValue(obj, IobjFlagPntOnSec, sphOneSecOnly);
		imodObjectSetValue(obj, IobjSymSize, sphereSize);
		if( plug.randomSymbols ) {
			int randInt = randIntInclusive(0,3);
			imodObjectSetValue(obj, IobjSymType, randInt);
			if( randInt!=0 )
				imodObjectSetValue(obj, IobjPointSize, 0);
		}
		
    setObjColor( obj, catObj.color.red(), catObj.color.green(), catObj.color.blue() );
		
		QString newObjName = catObj.categoryName.leftJustified(10, ' ');
		newObjName += ".  #(STEREOLOGY)#grid_set=" + catObj.gridSetId + "#";
		
		imodObjectSetName( obj, (char *)newObjName.toStdString().c_str() ); 
		
		if( setObjLabel( obj, newObjName )==false )
			cout << "Error setting label for object " << (catObj.objIdx+1) << endl;
  }
	
	return true;
}





//############################################################
//## LITTLE METHODS:



//------------------------
//-- Returns a pointer to the currently selected grid within the "plug.grid" vector
//-- according to the value of "plug.currGridIdx". If "plug.grid" is empty a grid will
//-- get added automatically and if "plug.currGridIdx" is an invalid value it will be
//-- set to 0 to prevent any errors.

GridSetObj *Stereology::getCurrGridSetObj()
{
	if( plug.grid.size()==0 )
	{
		addGrid( true );
	}
	
	if( plug.currGridIdx >= plug.grid.size() || plug.currGridIdx < 0 )
		plug.currGridIdx = 0;
	
	return ( &plug.grid[ plug.currGridIdx ] );
}

//------------------------
//-- Returns the object name of the matching object in the model file, or returns
//-- an empty string if the given object index ("objIdx") does not exists

QString Stereology::getObjName( int objIdx )
{
	Imod *imod  = ivwGetModel(plug.view);
	if( objIdx < 0 || objIdx >= osize(imod) )
		return "";
	
	Iobj *obj  = getObj(imod,objIdx);
	return (QString)( imodObjectGetName(obj) );
}

//------------------------
//-- Set's the label of the specified object ("obj") to the
//-- specified string ("newLabelStr"). Returns true if successful
//-- or false if there was a problem assigning the memory.

bool Stereology::setObjLabel( Iobj *obj, QString newLabelStr )
{	
	Ilabel *newLabel = imodObjectNewLabel( obj );
	return ( imodLabelName( newLabel, (char *)newLabelStr.toStdString().c_str()) != 2 );
}

//------------------------
//-- Returns the the label of the specified object ("obj") as a
//-- string. If the object has no label it returns an empty string instead.

QString Stereology::getObjLabel( Iobj *obj )
{
	Ilabel *label = imodObjectGetLabel(obj);
	if(label==NULL)
		return "";
	
	return (QString)( imodLabelNameGet(label) );
}

//------------------------
//-- Returns the the label of the specified object ("objIdx") as a
//-- string. If the object does not exist or has no label it returns
//-- an empty string instead.

QString Stereology::getObjLabel( int objIdx )
{
	Imod *imod  = ivwGetModel(plug.view);
	if( objIdx < 0 || objIdx >= osize(imod) )
		return "";
	
	return getObjLabel( getObj(imod,objIdx) );
}

//------------------------
//-- Inputs a distance in pixels ("distInPixels") and uses the model's pixel size
//-- and units (as specified in the model's header) to multiply this string
//-- by the pixel size and return a nicely formatted string in the form "15.3 nm",
//-- rounded of to the number of "decimals" specified

QString Stereology::distToUnitsStr( float distInPixels, int decimals )
{
	Imod *imod  = ivwGetModel(plug.view);
	float distInUnits = roundDecimals( distInPixels * imodGetPixelSize(imod), decimals);
	char *unitsChs = imodUnits(imod);
	
	return ( QStr(distInUnits) + " " + QString(unitsChs) );
}


//------------------------
//-- Takes a numbers of seconds "totSeconds" and formats a string in the form:
//-- "1 hrs 10 mins"... and is smart enough to *around* 2 significant figures.
//-- If the number of hours is zero, hrs is omitted and seconds are only
//-- included if the totSectons evaluates to <10 minutes

QString Stereology::formatApproxTime( float totSeconds )
{
	int hrs     = int( totSeconds / 3600 );
	totSeconds  = totSeconds - (hrs*3600);
	int mins    = int( totSeconds / 60 );
	int secs    = totSeconds - (mins*60);
	
	if( hrs >= 50 )				return ( QStr(hrs) + " hours" );
	else if(hrs >= 5 )		return ( QStr(hrs) + " hrs " + QStr(mins/30*30) + " mins" );
	else if(hrs >= 2 )		return ( QStr(hrs) + " hrs " + QStr(mins/10*10) + " mins" );
	else if(hrs >= 1 )		return ( QStr(hrs) + " hrs " + QStr(mins) + " mins" );
	else if(mins >= 10 )	return ( QStr(mins) + " mins" );
	else if(mins >= 1 )		return ( QStr(mins) + " mins " + QStr(secs/10*10) + " secs" );
	else									return ( QStr(totSeconds) + " seconds" );
}

//------------------------
//-- Takes a number and converts it to the matching character a-z
//-- where 1="a" and 26="z". For numbers more than 26 it uses recursion
//-- to add together characters such that 23="ab".

QString Stereology::numberToAlphabetChars( int number, bool caps )
{
	QString outString = "";
	if( number > 26 ) {
		int divisor = number / 26;
		outString += numberToAlphabetChars( divisor, caps );
		number -= (divisor*26);
	}
	outString += (char)((caps ? 65 : 97) + (number - 1));
	
	return (outString);
}





//############################################################
//## GRID SETUP METHODS:




//------------------------
//-- Adds a new grid to the "plug.grid" vector, using all the default grid settings
//-- (spacing etc) loaded by the plugin.
//-- If "checkUniqueGridId" is true, then the new grid's "gridSetId" value
//-- will be set to the first integer, starting at 1, which isn't already
//-- used in any "STEREOLOGY" object in the model. If false, this value
//-- defaults to "1" (as a string).

void Stereology::addGrid( bool checkUniqueGridId )
{
	int newGridSetId = 1;
	
	//## IF DESIRED: SEARCH FOR FIRST AVAILABLE "gridSetId" NUMBER
	//## NOT USED IN ANY OBJECT OF THE IMOD MODEL
	
	if( checkUniqueGridId )
	{
		Imod *imod  = ivwGetModel(plug.view);
		
		for(int i=1; i<100; i++)						// limit search to 100 only:
		{																		//  realistically would rarely have more than 2!
			QString searchStr = "grid_set=" + QStr(i);		// string we want to be unique
			
			bool clashFound = false;
			for(int o=0; o<osize(imod); o++)		// for each object: check if name contains this
			{
				Iobj *obj  = getObj(imod,o);
				QString objName = (QString)( imodObjectGetName(obj) );
				if( objName.contains( searchStr ) ) 
					clashFound = true;
			}
			if( !clashFound )		// if no clash found: 'i' must be unique
			{
				newGridSetId = i;
				break;
			}
		}
	}
	
	//## SETUP NEW GRID USING DEFAULT VALUES:
	
	GridSetObj newGrid;
	newGrid.gridSetId = QStr(newGridSetId);
	
	newGrid.xSpacing = plug.defSpacingX;
	newGrid.ySpacing = plug.defSpacingY;
	newGrid.zSpacing = plug.defSpacingZ;
	
	int LIM = (plug.changeDefLimits) ? plug.limitDefaultInset : 0;
	newGrid.xMin = LIM;
	newGrid.xMax = plug.xsize - LIM;
	newGrid.yMin = LIM;
	newGrid.yMax = plug.ysize - LIM;
	newGrid.zMin = 0;
	newGrid.zMax = plug.zsize - 1;
	
	newGrid.gridType = plug.gridDisplayOpt;
	
	newGrid.calcVals();
	
	plug.grid.push_back( newGrid );
}


//------------------------
//-- Updates the grid setup Gui using the currently selected grid.
//-- This function is called when a grid is loaded from IMOD
//-- objects. If "setCheckBoxesIntelligently" is on, then the values
//-- within the grid are used to guess wether the checkboxes "keep square"
//-- and "apply over multiple sections" should be on.

void Stereology::updateGridGuiFromGrid( bool setCheckBoxesIntelligently )
{
	plug.disableGuiUpdates = true;
	
	GridSetObj *g = getCurrGridSetObj();
	
	if( setCheckBoxesIntelligently )
	{
		plug.sameXYSpacing = ( g->xSpacing==g->ySpacing );
		chkSameXYSpacing->setChecked( plug.sameXYSpacing );
		
		plug.showGridEverySlice = false;
		chkShowEverySlice->setChecked( plug.showGridEverySlice );
		
		plug.changeDefLimits = true;
		chkChangeDefLimits->setChecked( plug.changeDefLimits );
		grpLimits->setVisible( plug.changeDefLimits );
		
		chkSubsampleRect->setChecked( g->showSubRects );
		grpRects->setVisible( g->showSubRects );
		
		plug.lockForbiddenSqXY = ( g->rXSpan==g->rYSpan && g->rXGap==g->rYGap );
		chkLockSqXY->setChecked( plug.lockForbiddenSqXY );
	}
	
	plug.gridDisplayOpt = g->gridType;
	cmbGridDisplayOpt->setCurrentIndex(  plug.gridDisplayOpt  );
	
	spnSpacingX->setValue(  g->xSpacing  );
	spnSpacingY->setValue(  g->ySpacing  );
	spnSpacingZ->setValue(  g->zSpacing  );
	
	spnLimitXMin->setValue(  g->xMin  );
	spnLimitXMax->setValue(  g->xMax  );
	spnLimitYMin->setValue(  g->yMin  );
	spnLimitYMax->setValue(  g->yMax  );
	spnLimitZMin->setValue(  g->zMin+1 );
	spnLimitZMax->setValue(  g->zMax+1 );
	
	spnRectXSpan->setValue(  g->rXSpan  );
	spnRectYSpan->setValue(  g->rYSpan  );
	spnRectXGap->setValue (  g->rXGap  );
	spnRectYGap->setValue (  g->rYGap  );
	
	plug.disableGuiUpdates = false;
	
	updateGuiToMatchCountingStarted();			// in case grid has been started
	
	updateGridFromGridGui( true, true, false );
}


//------------------------
//-- Updates the values of the current grid based on the values in the
//-- "Grid Setup" interface and does a redraw.
//-- Note that if "plug.disableGuiUpdates" is turned on or the current grid
//-- has already started counting only display changes are made and it returns 
//-- false, otherwise it updates almost all of the objects in the current 
//-- GridSetObj and returns true.
//-- 
//-- If "checkCheckboxes" is true, the checkboxes in the tab are checked
//-- and relevant elements are shown/hidden and a readjustment of size attempted.
//-- If "updatePtSelSpn" is true, the spin box showing the currently
//-- selected point is updated.

bool Stereology::updateGridFromGridGui( bool checkCheckboxes, bool updatePtSelSpn,
																			  bool jumpToGrid )
{
	if(plug.disableGuiUpdates)
		return (false);
	
	GridSetObj *g = getCurrGridSetObj();
	
	
	//## DISPLAY OPTIONS:
	
	plug.gridSymbolSize    = spnGridSymbolSize->value();
	plug.gridLineThickness = spnLineWidth->value();
	plug.gridColorR        = colGridColor->getColor().red();
	plug.gridColorB        = colGridColor->getColor().blue();
	plug.gridColorG        = colGridColor->getColor().green();
	plug.showDashes        = chkShowDashes->isChecked();
	plug.subRectDisplayOpt = cmbSubRectDisplay->currentIndex();
	
	plug.window->drawGridObject(false);
	plug.window->drawSubsampleRects(true);
	
	
	if( g->countingStarted )
		return (false);
	
	
	//## GET CHECKBOX VALUES AND SHOW/HIDE RELEVANT ELEMENTS
	
	if( checkCheckboxes )
	{
		plug.sameXYSpacing = chkSameXYSpacing->isChecked();
		spnSpacingY->setVisible( !plug.sameXYSpacing );
		lblGridSpX->setVisible( !plug.sameXYSpacing );
		lblGridSpY->setVisible( !plug.sameXYSpacing );
		
		plug.showGridEverySlice = chkShowEverySlice->isChecked();
		widRowZSpace->setVisible( !plug.showGridEverySlice );
		
		plug.changeDefLimits = chkChangeDefLimits->isChecked();
		grpLimits->setVisible( plug.changeDefLimits );
		
		g->showSubRects = chkSubsampleRect->isChecked();
		grpRects->setVisible( g->showSubRects  );
		
		plug.lockForbiddenSqXY = chkLockSqXY->isChecked();
		spnRectYSpan->setVisible( !plug.lockForbiddenSqXY );
		spnRectYGap->setVisible( !plug.lockForbiddenSqXY );
		
		widGridSetup->updateGeometry();
	}
	
	g->gridType = cmbGridDisplayOpt->currentIndex();
	
	//## GET ALL SPIN BOX VALUES:
	
	g->xSpacing = (float)spnSpacingX->value();
	g->ySpacing = (plug.sameXYSpacing) ? g->xSpacing : (float)spnSpacingY->value();
	g->zSpacing = (plug.showGridEverySlice) ? 1 : MAX( spnSpacingZ->value(), 1);
	
	if( plug.changeDefLimits )
	{
		g->xMin = spnLimitXMin->value();
		g->xMax = spnLimitXMax->value();
		g->yMin = spnLimitYMin->value();
		g->yMax = spnLimitYMax->value();
		g->zMin = spnLimitZMin->value() - 1;
		g->zMax = spnLimitZMax->value() - 1;
	}
	else
	{
		g->xMin = 0;
		g->xMax = plug.xsize;
		g->yMin = 0;
		g->yMax = plug.ysize;
		g->zMin = 0;
		g->zMax = plug.zsize - 1;
	}
	
	
	//## SUBSAMPLE RECTANGLE OPTIONS:
	
	g->rXSpan = spnRectXSpan->value();
	g->rYSpan = (plug.lockForbiddenSqXY) ? g->rXSpan : spnRectYSpan->value();
	g->rXGap  = spnRectXGap->value();
	g->rYGap  = (plug.lockForbiddenSqXY) ? g->rXGap : spnRectYGap->value();
	
	
	//## CALCULATE NEW GRID PARAMETERS AND UPDATE LABEL:
	
	g->calcVals();		// calculate any values which may have changed
	
	QString lblStr = "";
	lblStr += "<b>SUMMARY</b><br>";
	lblStr += "pts/grid:    <b>" + QStr(g->ptsPerGrid) + "</b>    (";
	lblStr += QStr(g->cols) + "*" + QStr(g->rows) + ")<br>";
	lblStr += "total grids: <b>" + QStr(g->grids) + "</b><br>";
	lblStr += "total pts:  <b>" + QStr(g->maxPts) + "</b>";
	
	if( g->maxPts > 100000 && !plug.showGridEverySlice )		// if above 100,000:
	{
		lblStr += "  (<u>ABORT!!</u>)";	
		widPtsRow->setStyleSheet("background-color: rgb(220, 0, 0);");	// dark red
	}
	else if( g->maxPts > 50000 )		// if between 50,000 and 100,000:
	{
		lblStr += "  (<u>dangerous!</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(255, 40, 40);");	// red
	}
	else if( g->maxPts > 10000 )		// if between 10,000 and 50,000:
	{
		lblStr += "  (<u>ambitious</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(0, 200, 0);");		// dark green
	}
	else if( g->maxPts > 5000 )		// if between 5,000 and 10,000:
	{
		lblStr += "  (<u>high</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(0, 255, 0);");		// darker green
	}
	else if( g->maxPts > 2000 )		// if between 2,000 and 5,000:
	{
		lblStr += "  (<u>great!</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(50, 255, 50);");	// green
	}
	else if( g->maxPts > 1000 )		// if between 1,000 and 2,000:
	{
		lblStr += "  (<u>good</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(100, 255, 100);");	// light green
	}
	else if ( g->maxPts > 500 )		// if between 500 and 1,000:
	{
		lblStr += "  (<u>not bad</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(255, 230, 110);");	// yellow
	}	
	else													// if less than 500:
	{
		lblStr += "  (<u>not enough!</u>)";
		widPtsRow->setStyleSheet("background-color: rgb(255, 40, 40);");		// red
	}	
	
	lblTotalPts->setText( lblStr );
	
	if(checkCheckboxes)
		scrollAreaGridSetup->updateGeometry();
	
	if(updatePtSelSpn)
		updateCurrPtInGui(false,false);
	
	if( jumpToGrid && !g->isGridOnSlice( edit_getZOfTopZap() ) )
	{
		Ipoint pt = g->getPos( g->currPtIdx );
		if( g->isGridOnSlice( (int)pt.z ) )
			edit_setZapLocation( pt.x, pt.y, pt.z, true );
		else
			edit_setZapLocation( 0, 0, g->zMin, true );
	}
	
	plug.window->drawGridObject(false);
	plug.window->drawSubsampleRects(true);
	
	return (true);
}



//------------------------
//-- Displays a short series of dialogs to let the user know that
//-- once a grid is "finalized" it cannot be altered and also
//-- asks the user if he wants to "use random points" instead of
//-- grid points, then takes the user to the second tab.
//-- This function is called when the user clicks "Finalize Grid"
//-- but in fact the user can still click back to the "Setup Grid"
//-- tab to make more changes. These options are only locked 
//-- after the user clicks "Finalize Categories".

void Stereology::finalizeGrid()
{
	GridSetObj *g = getCurrGridSetObj();
	
	//## GENERATE SUMMARY OF GRID:
  
	Imod *imod  = ivwGetModel(plug.view);
	Ipoint scalePt;
	setPt( &scalePt, 1,1, imodGetZScale(imod) );
	
	QString xSpUnits = distToUnitsStr( g->xSpacing, 3 );
	QString ySpUnits = distToUnitsStr( g->ySpacing, 3 );
	QString zSpUnits = distToUnitsStr( g->zSpacing * scalePt.z, 3 );
	
	QString sX = QStr(g->xMax-g->xMin) +" px | "+ distToUnitsStr((g->xMax-g->xMin),1);
	QString sY = QStr(g->yMax-g->yMin) +" px | "+ distToUnitsStr((g->yMax-g->yMin),1);
	QString sZ = QStr(g->zMax-g->zMin) +" px | "+ distToUnitsStr((g->zMax-g->zMin)*scalePt.z,1);
	
	float totEstimatedSecs = g->maxPts * plug.secsPerPt;
	QString estimatedTime  = formatApproxTime( totEstimatedSecs );
	
	float estDev        = (g->maxPts > 1) ? (1.0f / sqrt((double)g->maxPts)) : 1.0f;
	float estDevPercent = roundDecimals( estDev*100.0f, 2 );
	
	QString sumStr;			// summary string
	sumStr += "<b>SPACING BETWEEN POINTS</b>:<br>";
	sumStr += "> x spacing:   <b>" + QStr(g->xSpacing) + " px</b> (" + xSpUnits + ") <br>";
	sumStr += "> y spacing:   <b>" + QStr(g->ySpacing) + " px</b> (" + ySpUnits + ") <br>";
	sumStr += "> z spacing:   <b>" + QStr(g->zSpacing) + " px</b> (" + zSpUnits + ") <br>";
	sumStr += "<br>";
	sumStr += "<b>LIMITS FOR GRID</b>:<br>";
	sumStr += "> x range: " + QStr(g->xMin) + " - " + QStr(g->xMax) + " (" + sX + ")<br>";
	sumStr += "> y range: " + QStr(g->yMin) + " - " + QStr(g->yMax) + " (" + sY + ")<br>";
	sumStr += "> z range: " + QStr(g->zMin) + " - " + QStr(g->zMax) + " (" + sZ + ")<br>";
	sumStr += "<br>";
  sumStr += "<b>GRID POINTS (IMPORTANT)</b>:<br>";
	sumStr += "> # grid lines: <b>" + QStr(g->cols) + "</b> columns * <b>" + QStr(g->rows) + "</b> rows<br>";
	sumStr += "> # points/grid: " + QStr(g->ptsPerGrid) + "<br>";
	sumStr += "> # grids  (z): " + QStr(g->grids) + "<br>";
	sumStr += "------------------------------<br>";
	sumStr += "> # TOTAL POINTS: <b>" + QStr(g->maxPts) + "</b><br>";
	sumStr += "------------------------------";
	
	QString estStr;
	estStr += "Estimated time to classify:   <b>~" + estimatedTime + "</b> *<br>";
	estStr += "Estimated accuracy on counts: <b>" + (QString)PLUSMINUS_SIGN + QStr(estDevPercent) + "%</b> *"; 
	
	
	//## GET TO CONFIRM GRID SETTINGS VIA CUSTOM DIALOG:
	
	static bool agree = false;
	
	while( agree == false)
	{
		CustomDialog ds("Confirm Grid Settings",this);
		ds.addLabel    ("Your grid is setup as follows:\n" );
		
		ds.addHtmlLabel(sumStr,
										"Shown here is a summary of what you entered in the \n"
										"grid summary fields. Note that if you wanted your grid \n"
										"spacing in x / y a fixed number of units (eg: nm) you \n"
										"can click 'Options > Set Spacing Using Units'." );
		ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
		
		ds.addHtmlLabel(estStr,
										"Represents a time estimate to classify all <b>"
										+ QStr(g->maxPts) + "</b> points. "
										"This estimate is based on a speed of '<b>"
										+ QStr(plug.secsPerPt) + " seconds/point</b>'. "
										"Depending on the data and the number of classification "
										"you have, you may be able to go faster than this - especially "
										"if you have a small number of classifications and can make "
										"use of the '<b>Pt Painter</b>' and other tools." );
		ds.setStylePrev("background-color: rgb(100, 255, 100);");			// light green
		
		ds.addHtmlLabel("Click 'ok' if you are happy to 'lock in'<br>"
										"this grid.<br>",
										"WARNING: Once a grid is locked in and categories set \n"
										"the 'point counting' phase begins and it is no longer \n"
										"possible to change the grid setup!" );
		
		ds.addCheckBox( "I understand that once started\n"
									  "I can't modify the grid", &plug.agreeFinalizeDlg,
									  "Once you have setup categories and start counting points \n"
									  "you are unable to change the grid unless you create a new \n"
									  "grid and start from scratch" );
		ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
		
		ds.exec();
		if( ds.wasCancelled() )
			return;
		
		agree = plug.agreeFinalizeDlg;
		
		if( agree==false )
			MsgBox( this, "...", "You must click 'I understand' before \n"
						               "you continue to the next setp" );
		
	}
	
	
	//## GET TO DECIDE IF HE WANTS TO USE THE RANDOM POINTS METHOD:
	
	if( g->gridType==GD_RAND )
	{
		int pointProgression = 0;
		
		CustomDialog ds("Random Point", this, BS_NO_YES );
		ds.addLabel   ( "-----\n"
									 "WARNING:\n"
									 "  > You have chosen the 'random points' grid.      \n"
									 "   With this option selected the the x and y spacing \n"
									 "   for gridlines is mostly ignored (even if shown).  \n"
									 "   Instead, you will be prompted to generate randomly \n"
									 "   positioned points inside the 'grid limits' you have \n"
									 "   entered.  \n"
									 "   This is not a conventional method for stereology, \n"
									 "   and while the advantage is you can add as many   \n"
									 "   points as you want... the disadvantage is slower \n"
									 "   jumping between sections and it becomes non-trivial \n"
									 "   to reproduce or write up results." );
		ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
		
		ds.addHtmlLabel("Are you sure you want to use a 'random points' grid<br>"
										"and continue?" );
		
		ds.exec();
		if( ds.wasCancelled() )
			return;
	}
	
	
	//## IF NO CATEGORIES EXIST: GENERATE FIRST TWO:
	
	if( g->catObj.size() == 0 )
	{
		g->catObj.resize( 2 );
		
		g->catObj[0].gridSetId    = g->gridSetId;
		g->catObj[0].setNameAndTooltip ( "NO_CATEGORY",
																		"Choose this using shortcut key [~]or [0] \n"
																		"if a point doesn't match any of the \n"
																		"categories/classifications below" );
		g->catObj[0].color        = QColor(50,50,50);		// grey
		g->catObj[0].sphereSize   = 1;
		
		g->catObj[1].gridSetId    = g->gridSetId;
		g->catObj[1].setNameAndTooltip ( "Mitochondria",
																		 "Click to enter whatever color you want" );
		g->catObj[1].color        = QColor(0,255,0);			// green
		
		updateItemListGuiFromCats();
	}
	
	
	//## SELECT NEXT TAB AND SHOW USER MESSAGE:
	
	tabWidget->setCurrentIndex(1);
	
	MsgBox( this, "Next step: setup categories",
				 "In this next step you must choose at LEAST two categories which "
				 "your points can belong to. These cannot be easily changed later "
				 "so make sure you consult the 'Help' pages and chose your categories wisely");
	
	
	//## IF USER HAS ENTERED A "SURFACE AREA DENSITY" GRID TYPE, THEN
	//## ASK USER IF HE WANTS TO POPULATE NUMBERS OF INTERSECTIONS 
	//## INTO CATEGORIES LIST:
	
	if( g->isSurfaceAreaGridType() )
	{
		static int maxIntersections = 9;
		
		CustomDialog dsC("Counting Grid", this, BS_NO_YES );
		dsC.addHtmlLabel
		( "-----<br>"
		 "The grid type you have selected: ('<b>" + g->gridTypeAsString() + "</b>') <br>"
		 "is designed to estimate <b>surface area density</b> (<i>S<sub>V</sub></i>) <br>"
		 "by recording the number of times your surface <br>"
		 "intersects each line segment. Would you like <br>"
		 "us to setup categories accordingly using category <br>"
		 "names '<b>0_INTERSECTIONS</b>'-'<b>9_INTERSECTIONS</b>' ?" );
		dsC.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
		
		dsC.addSpinBox ( "max intersections / line segment:", 1, 100, &maxIntersections, 1,
									  "You shold enter here the maximum number of intersections  \n"
									  "you'd expect based on the length of your line segments.  \n"
									  "If you ever count more than 9 we'd recommend reducing \n"
									  "the length of segments via the x and/or y spacing." );
		
		dsC.exec();
		
		if( !dsC.wasCancelled() )
			setupCountingCategories( maxIntersections, "INTERSECTIONS" );
	}
}


//------------------------
//-- Has options for starting a new grid:

void Stereology::startNewGrid()
{
	static int typeOfMeasure = 0;													// "Volume (V)"
	static int gridOptVol = GD_LINES;											// default volume option
	static int gridOptSA  = GD_LINEPAIRS -GD_VERTLINES;		// default surface area option
	static int gridOptLen = GD_CYCLOIDS3 -GD_CYCLOIDS3;		// default volume option
	static int gridOptNum = GD_FORBIDSQ1 -GD_RECTS;				// default volume option
	
	static int volumeTypeOpt = 0;													// a tomographic or reconstructed
	static bool aimForTargetPts = true;
	static bool physicalDissector = false;
	
	const int MIN_WIDTH  = 400;
	const int MIN_HEIGHT = 400;
	
	int wizardStep = 1;
	
	
	while ( wizardStep >= 1 && wizardStep <= 3 )
	{
		
	//## WIZARD STEP 1/3: GET USER TO CHOSE WHAT HE WANTS TO MEASURE
			
	if( wizardStep == 1 )
	{	
		CustomDialog ds1("New Grid Wizard", this, BS_CUSTOM );
		
		ds1.addCustomButton("< Prev", BB_DISABLED );
		ds1.addCustomButton("Cancel", BB_REJECT   );
		ds1.addCustomButton("Next >", BB_ACCEPT, "Go to step 2");
		
		//ds1.setCustomButton( "< Prev", BB_ACCEPT, "", "Cancel", true, "Prev >", true );
		
		ds1.addHtmlLabel( "Step 1/3:" );
		ds1.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
		
		ds1.addRadioGrp( "What are you looking to estimate?",
										 "Volume (V) ...........  (point counting)|"
										 "Surface area (S) .....  (counting intersections)|"
										 "Length (L) ...........  (counting profiles)|"
										 "Number (N) ...........  (counting objects)",
										 &typeOfMeasure, "",
										 "With this option you'll be doing 'point counting stereology'.\n"
										 "You will first setup categories to represent different \n"
										 "classifications/volumes a point may fall into and then \n"
										 "assign each point a category using the matching shortcut key."
										 "|"
										 "With this options you'll be 'counting intersections'. \n"
										 "You will chose one of several grids which project test,\n"
										 "lines from each point and categories will be setup in a \n"
										 "way that makes it easy to count the number of lines \n"
										 "that cross your region of interest."
										 "|"
										 "This option isn't well supported so is recommended for \n "
										 "the experts only. This option only works by counting \n"
										 "transections or profiles using of very thin string-like' \n"
										 "structures - and in IMOD it's much easier to instead \n"
										 "draw open contours through (complete) surfaces you need a \n"
										 "length estimate of."
										 "|"
										 "With this option you'll be using 'forbidden squares' \n "
										 "where you must count only surfaces which are inside and/or \n"
										 "touch the box WITHOUT touching the forbidden (dotted) lines." );
		
		ds1.addVSpacer( 10 );
		ds1.setMinimumSize( MIN_WIDTH,MIN_HEIGHT );
		ds1.exec();
		
		if( ds1.wasCancelled() )
			return;
		
		if      ( ds1.customBtnClicked == 0 )		wizardStep--;			// "< Prev"
		else if ( ds1.customBtnClicked == 1 )		return;						// "Cancel"
		else if ( ds1.customBtnClicked == 2 )		wizardStep++;			// "Finish"
	}
	
	
	//## WIZARD STEP 2/3: SELECT GRID TYPE:
	
	if( wizardStep == 2)
	{
		CustomDialog ds2("New Grid Wizard", this, BS_CUSTOM );
		
		ds2.addCustomButton("< Prev", BB_ACCEPT, "Return to step 1");
		ds2.addCustomButton("Cancel", BB_REJECT );
		ds2.addCustomButton("Next >", BB_ACCEPT, "Go to final step");
		
		ds2.addHtmlLabel( "Step 2/3:" );
		ds2.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
		
		QString ind20 = nbsp(15);
		QString ind3  = nbsp(3);
		
		if( typeOfMeasure==0 )
		{
			ds2.addComboBox( "Select grid type:",
											 "lines *|"
											 "crosshairs *|"
											 "arrows|"
											 "offset pts|"
											 "random", &gridOptVol );
			
			ds2.addHtmlLabel
			( "<u>ABOUT EACH GRID</u>: <br>"
				"<b>&gt; lines *</b> - a uniform grid of lines <br>"
				+ind20+ "<i>(*recommended for a dense grid with many points)</i><br>"
				"<b>&gt; crosshairs *</b> - a uniform grid of points <br>"
				+ind20+ "<i>(*recommended as easiest to use and see points)</i><br>"
				"<b>&gt; arrows</b> - same as above (points just look different)  <br>"
				"<b>&gt; offset pts</b> - similar to others, except every second row is <br>"
				+ind20+ "offset to maximize distance between points  <br>"
				"<b>&gt; random</b> - very different from all the above - allows <br>"
				+ind20+ "any number of points to be randomly positioned <br>"
				+ind20+ "but isn't recommended if you want to publish <br>"
				+ind20+ "results although in many ways the most effective<br>"
				+ind20+ "one for unbiased sampling" );
			
			ds2.addHtmlLabel ( "-----<br>"
												"To estimate the volume using 'point counting stereology'<br>"
												"try and cover as much volume as possible with points  <br>"
												"spaced far apart in Z - else one grid to the next will  <br>"
												"be almost identical.<br>",
												"<b>TIP</b>: Most stereology experts put as few as 12-100 <br>"
												"points per grid (depending on it's size)... so long as the <br>"
												"total points add up to >500 for each specimen. <br>"
												"<b>TIP</b>: If have a huge image but want a fine grid <br>"
												"without a rediculous number of points try turning on <br>"
												"'show forbidden rects for subsample'" );
			ds2.setStylePrev( "background-color: rgb(200, 200, 200);");			// grey
		}
		else if( typeOfMeasure==1 )
		{
			ds2.addComboBox( "Select grid type:",
											 "vertical lines *|"
											 "horizontal lines"
											 "diagonal down|"
											 "diagonal down|"
											 "line pairs *|"
											 "cycloids *|"
											 "cycloids alt|"
											 "cycloids long|", &gridOptSA );
			
			ds2.addHtmlLabel
			( "<u>ABOUT EACH GRID</u>: <br>"
				"<b>&gt; vertical lines *</b> \t- uniformly spaced vertical lines <br>"
				+ind20+"<i>(*recommended as easy-to-use on small images)</i> <br>"
				"<b>&gt; horizontal lines</b> \t- uniformly spaced horizontal lines <br>"
				"<b>&gt; diagonal down</b> - lines go diagonally down to the right <br>"
				"<b>&gt; diagonal down</b> - lines go diagonally up to the right <br>"
				"<b>&gt; line pairs *</b> - a popular stereology grid for counting <br>"
				+ind20+"intersections using short staggered horizontal lines <br>"
				+ind20+"<i>(*recommended for large images)</i> <br>"
				"<b>&gt; cycloids *</b> - uses special 'cycloid' lines believed <br>"
				+ind20+"to minimize orientation bias, though in this <br>"
				+ind20+"grid cycloids are deformed by x/y spacing values<br>"
				+ind20+"<i>(*only if you know what you're doing!)</i><br>"
				"<b>&gt; cycloids alt</b> - as above, but with every second cycloid <br>"
				+ind20+"angled down <br>"
				"<b>&gt; cycloids long</b> - continous and non-deformed cycloids." );
			
			ds2.addHtmlLabel( "-----<br>"
												"To estimate the surface area per volume as easy-and-fast  <br>"
												"as possible, try and set the spacing (in x and y) so <br>"
												"that each line segment has anywhere between 0 and 3 <br>"
												"intersections with the object type you're interested in,<br>"
												"allowing you to quickly count and hit shortcut keys [0]-[3] <br>"
												"for each line. There are other methods to count, but this is <br>"
												"easiest" );
			ds2.setStylePrev( "background-color: rgb(200, 200, 200);");			// grey
		}
		else if( typeOfMeasure==2 )
		{
			ds2.addComboBox( "Select grid type:",
											 "cycloids long",&gridOptLen );
			
			ds2.addHtmlLabel
			( "<u>ABOUT EACH GRID</u>: <br>"
				"<b>&gt; cycloids long</b> - a grid of continous up and down cycloids <br>"
				+ind20+"which can be used in length estimates <br>"
				+ind20+"by counting number of intersections" );
			
			ds2.addHtmlLabel( "-----<br>"
												"<b>WARNING</b>: Calculating length estimates  <br>"
												"only works for long string like structures and <br>"
												"is not well supported by this plugin. Use at your <br>"
												"own risk or... if you have a huge volume relative <br>"
												"the structure's length, why not try measuring  <br>"
												"lengths by drawing an open contour through the middle <br>"
												"of each (completely captured) structures" );
			ds2.setStylePrev( "background-color: rgb(200, 200, 200);");			// grey
		}
		else if( typeOfMeasure==3 )
		{
			ds2.addComboBox( "Select grid type:",
											 "1:2 forbidden rectangles|"
											 "1:2 forbidden squares|"
											 "1:4 forbidden squares*|"
											 "1:8 forbidden squares", &gridOptNum );
			
			ds2.addHtmlLabel
			( "<u>ABOUT EACH GRID</u>: <br>"
				"<b>&gt; <b>1:2 rectangles</b> - uniformly spaced rectangles with width <br>"
				+ind3+"<b>rectangles</b> and height set to half x and y spacing <br>"
				"<b>&gt; <b>1:2 forbidden *</b> - shows forbidden squares with a <br>"
			  "</b>squares</b>"+ind20+"side length 1/2 the minimum side."
			  "<b>&gt; <b>1:4 \"\"\"\" </b> - side length 1/4 minimum side <br>"
			  "<b>&gt; <b>1:8 \"\"\"\" </b> - side length 1/8 minimum side <br>" );
			
			ds2.addHtmlLabel( "-----<br>"
												"Using 'forbidden squares/rectangles' you should count <br>"
												"(for each square) the number of surfaces which touch <br>"
												"the box (including the dotted lines) WITHOUT touching <br> "
											  "the forbidden (solid) lines.<br>"
												"Try to setup the spacing so that you have, on average,<br>"
												"around three surfaces per box.",
											  "<b>TIP</b>: Fobidden squares are designed to prevent "
											  "you counting the any surface more than once... but that said "
											  "it's good practise to space boxes far enough in X and Y "
												"the same surface doesn't touch multiple boxes." );
			ds2.setStylePrev( "background-color: rgb(200, 200, 200);");			// grey
			
			ds2.addCheckBox("Turn on custom physical dissector !", &physicalDissector,
										 "If true: the X and Y spacing will be adjusted so.\n" );
			
			ds2.addHtmlLabel ( "-----<br>"
												 "<b>TIP</b>: This option above turns on <br>"
												 "'subsample rectangles' which are displayed as <br>"
												 "forbidden retangles and set to any size. Click <br>"
												 "<a href='www.test.com'>here</b> "
												 "to learn more about physical dissector." );
		}
		
		ds2.addVSpacer( 10 );
		ds2.setMinimumSize( MIN_WIDTH,MIN_HEIGHT );
		ds2.exec();
		
		if( ds2.wasCancelled() )
			return;
		
		if      ( ds2.customBtnClicked == 0 )		wizardStep--;			// "< Prev"
		else if ( ds2.customBtnClicked == 1 )		return;						// "Cancel"
		else if ( ds2.customBtnClicked == 2 )		wizardStep++;			// "Next >"
	}
	
		
	//## WIZARD STEP 3/3: SELECT TARGET NUMBER OF POINTS:
	
	if( wizardStep == MT_NUM )
	{
		CustomDialog ds3("New Grid Wizard", this, BS_CUSTOM );
		
		ds3.addCustomButton("< Prev", BB_ACCEPT, "Return to step 2");
		ds3.addCustomButton("Cancel", BB_REJECT );
		ds3.addCustomButton("Finish", BB_ACCEPT, "Finish and generate this grid");
		
		ds3.addHtmlLabel( "Step 3/3:" );
		ds3.setStylePrev( "background-color: rgb(150, 150, 150);");			// grey
		
		ds3.addCheckBox("adjust grid spacing to give me ", &aimForTargetPts,
									 "If true: the X and Y spacing will be adjusted so.\n" );
		
		ds3.addSpinBox( "  ~ this many points:", 10, 1000000,
									  &plug.targetNumPts, 100,
									  "The number of points you'd like to have in your grid. \n"
									  "X and Y values will be changed accordingly." );
		
		ds3.addRadioGrp( "space grids apart:",
										"YES - recommended if you have a single continous \n"
										"         3D volume (eg: tomograms or reconstructions) |"
										"NO -  recommened if you have a collection of separate  \n"
										"         images (each from a  different area) |"
										"LEAVE ALONE - keep the current z spacing.",
										&volumeTypeOpt, "",
										"Select this if your image sections belong to \n"
										"a single volume, and we'll try and set z spacing so \n"
										"that it's greater or equal "
										"|"
										"Select this if you've put together or loade a large \n"
										"number of 2D images from different locations around \n"
										"one or more specimen / animal "
										"|"
										"Select this if you've already set your desired \n"
										"z spacing because you know what your doing " );
		
		ds3.addVSpacer( 10 );
		ds3.setMinimumSize( MIN_WIDTH,MIN_HEIGHT );
		ds3.exec();
		
		if( ds3.wasCancelled() )
			return;
		
		if      ( ds3.customBtnClicked == 0 )		wizardStep--;			// "< Prev"
		else if ( ds3.customBtnClicked == 1 )		return;						// "Cancel"
		else if ( ds3.customBtnClicked == 2 )		wizardStep++;			// "Finish"
	}}
	
	
	
	
	//## SETUP GRID TO MATCH WIZARD SETTINGS:
	
	
	plug.grid.clear();
	addGrid( true );
	GridSetObj *g = getCurrGridSetObj();
	
	addCategory();
	if( g->isSurfaceAreaGridType() )
		setupCountingCategories( 9, "INTERSECTIONS" );
	else if( g->isNumberDensityGridType() )
		setupCountingCategories( 9, "OCCURANCES" );
	else
		loadDefaultCatNames();

	
	
	
	//** MAKE SURE CORRECT GRID TYPE IS SELECTED:
	
	if     ( typeOfMeasure==MT_VOL )				g->gridType = gridOptNum + 0;
	else if( typeOfMeasure==MT_SA  )				g->gridType = gridOptNum + GD_VERTLINES;
	else if( typeOfMeasure==MT_LEN )				g->gridType = gridOptNum + GD_CYCLOIDS3;
	else if( typeOfMeasure==MT_NUM )				g->gridType = gridOptNum + GD_RECTS;
	
	
	//** SET GRID TO SHOW ON ALL SLICES OR NOT:
	
	plug.showGridEverySlice = (volumeTypeOpt==1);
	if( plug.showGridEverySlice )
		g->zSpacing = 1;
	
	//** IF SPECIFIED: TURN ON PHYSICAL DISSECTOR:
	
	if( physicalDissector && typeOfMeasure==2)
	{
		g->showSubRects = true;
		plug.subRectDisplayOpt = RD_FORBIDDENSQ;
		g->gridType = GD_OFF;
	}
	
	//** IF DESIRED, ADJUST GRID SPACING TO FIT THE DESIRED
	//** (TARGET) NUMBER OF POINTS:
	
	if( aimForTargetPts )
	{
		g->calcVals();
		
		float diffX = MAX(g->xMax - g->xMin, 1);
		float diffY = MAX(g->yMax - g->yMin, 1);
		float diffZ = MAX((g->zMax - g->zMin)+1, 1);
		
		float aspectXY = fDiv( diffX, diffY );
		float areaXY = diffX * diffY;			// 2D area of grid limits in pixels squared
		
		if( volumeTypeOpt==1 || volumeTypeOpt==2 || diffZ<=2.0f )
		{
			float numGrids = MAX(g->grids, 1);
			float targetPtsPerGrid = fDiv( (float)plug.targetNumPts, numGrids );
			
			float targetColsX = sqrt( targetPtsPerGrid * aspectXY );
			int newXSpacing   = ceil( fDiv(diffX, targetColsX+1.5f) );
			g->xSpacing = newXSpacing;
			g->ySpacing = newXSpacing;
		}
		else														// if we want to change z spacing
		{
			const float extraZStretch = 1.25f;	// will aim to set zSpacing "extraZStretch" x
																					//  greater than xSpacing/ySpacing
			
			Ipoint scalePt;
			setPt( &scalePt, 1,1, imodGetZScale( ivwGetModel(plug.view) ) );
			float aspectXZ = fDiv( diffX, (scalePt.z * diffZ / extraZStretch ) );
			
			float targetColsX = pow( (float)plug.targetNumPts*aspectXZ*aspectXY, (1.0f/3.0f) );
			int newXSpacing   = ceil( fDiv(diffX, targetColsX+1.5f) );
			
			g->xSpacing = newXSpacing;
			g->ySpacing = newXSpacing;
			g->zSpacing = (int)fDiv( newXSpacing, scalePt.z / extraZStretch);
		}
	}
	
	//** UPDATE GUI TO REFLECT CHANGES:
	
	updateGridGuiFromGrid( true );
	updateItemListGuiFromCats();
}



//------------------------
//-- Has options for modifying a grid's setting by modifying object
//-- labels and then reloading the grid. If the grid counting has not
//-- yet started then this object shows a message that the user
//-- can modify settings directly... but if the grid is finalized
//-- then a dialog appears showing the labels of all objects,
//-- which the user can then change and update.

void Stereology::modifyExistingGridViaObjectLabels()
{
	GridSetObj *g = getCurrGridSetObj();
	Imod *imod  = ivwGetModel(plug.view);
	
	//## IF GRID COUNTING NOT STARTED: SHOW MESSAGE:
	
	if( !g->countingStarted )
	{
		QMessageBox::information( this, "...",
														  "This option is used only as a last resort when you've "
														  "already finalized categories, but then decide you may "
														  "want to change the grid somehow... "
														  "for example: halve the spacing in X ane/or Y to make "
														  "a finer grid. \n\n"
														  "You have not started counting points yet, so you can "
														  "still change grid settings using the 'Grid Settings' "
														  "tab. Get this right the first time and you won't need "
														  "this option!"  );
		return;
	}
	
	//## COMPILE TWO VECTORS WITH OBJECT NAMES AND LABELS FOR EACH CATEGORY
	//## AND SHOW ERROR MESSAGE IF ANY OF THE "objIdx" VALUES ARE INVALID:
	
	static int action = 0;
	
	CustomDialog dsA("Modification options:", this );
	
	dsA.addRadioGrp( "choose an action:",
									"modify existing categories and/or grid settings|"
									"double the number of points|"
									"add new categories",
									&action, "",
									"Select this if you want to make custom modifications to \n"
									"grid settings or categories by changing their label values|"
									"Will quickly increase the number of points in your grid \n"
									"by ~2x by halving the grid spacing along x, y or z \n"
									"(depending on which option you choose) and updating the \n"
									"grid settings label for you."
									"Select this if you're realized you need/want additional \n"
									"categories (can add up to four at a time)" );
	
	dsA.exec();
	if( dsA.wasCancelled() )
		return;
	
	if( action==1  )
	{
		makeGridFiner();
		return;
	}
	else if( action==2  )
	{
		addExtraGridCategoriesToImodModel();
		return;
	}
	
	
	//## COMPILE TWO VECTORS WITH OBJECT NAMES AND LABELS FOR EACH CATEGORY
	//## AND SHOW ERROR MESSAGE IF ANY OF THE "objIdx" VALUES ARE INVALID:
	
	int invalidObjs = 0;
	
	string gridName  = getObjName(g->objIdx).toStdString();
	string gridLabel = getObjLabel(g->objIdx).toStdString();
	
	if( g->objIdx < 0 || g->objIdx >= osize(imod) )
	{
		MsgBox( "No matching object was found for the grid setting object... \n"
					  "maybe you deleted it? If so please reload the grid from the model." );
		return;
	}
	
	vector<string> catName;
	vector<string> catLabel;
	
	for(int c=0; c<(int)g->catObj.size(); c++)
	{
		int catObjIdx = g->catObj[c].objIdx;
		
		if( catObjIdx < 0 || catObjIdx >= osize(imod) )
		{
			MsgBox( "No matching object was found for one of your category objects... \n"
						  "maybe you deleted it? If so please reload the grid from the model." );
			return;
		}
		
		catName.push_back ( getObjName(catObjIdx).toStdString() );
		catLabel.push_back( getObjLabel(catObjIdx).toStdString() );
	}
	
	
	//## SHOW A CUSTOM DIALOG WITH A LIST OF ALL OBJECT LABELS:
	
	CustomDialog ds("Modify Grid via Object Labels:", this, BS_CUSTOM );
	
	ds.addCustomButton( "Cancel", BB_REJECT   );
	ds.addCustomButton( "Change and Reload", BB_ACCEPT,
											"Will apply all your changes by changing the name's and \n"
											"labels of objects... and then reload this grid from the \n"
											"model (so it should reflect your changes");
	
	ds.addHtmlLabel( "Object Labels for grid_id '<b>" + g->gridSetId + "</b>':" );
	ds.setStylePrev( "background-color: rgb(150, 150, 150);");			// grey
	
	ds.addHtmlLabel( "<b><u>Grid setting object:</u>:</b>" );
	
	ds.addLineEdit( "  object " + QStr(g->objIdx+1) + ":", 
								 &gridName,
								 "Represents the name of the grid setting's object.... \n"
								 "this should always start with 'STEREOLOGY.SETTINGS' \n"
								 "(to help you recognize it) and should be in the form:"
								 " > 'STEREOLOGY.GRID #grid_set=1#'   \n"
								 "You add extra variables such as #spacing=25,50,5# to overrule \n"
								 "values in the label, but the critical thing is that all the \n"
								 "values in the label are correct.");
	
	ds.addLineEdit( "       label:", 
									&gridLabel,
									"Represents the label of the grid setting's object.... \n"
									"this must always start with 'STEREOLOGY.SETTINGS' and \n"
									"be in the form: \n"
									" > STEREOLOGY.GRID #grid_set=1#type=pts#spacing=50,50,5#x=10,1014\n"
                  "   #y=10,1014#z=10,190#(DONT_RENAME)#");
	ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
	
	ds.addHtmlLabel("<b>WARNING</b>: to make a finer grid you can halve spacing values<br>"
									"pretty easily.... but please only change labels (yellow) if you<br>"
									"know what you're doing and have saved a backup! In cases where <br>"
									"parameters in the name are not recognized (or don't exist), labels<br>"
									"are used, so must contain all the correct keywords.<br>" );
	
	ds.addHtmlLabel( "<b><u>Category objects</u>:</b>" );
	
	for( int c=0; c<(int)catName.size() && c<(int)g->catObj.size(); c++ )
	{
		int catObjIdx = g->catObj[c].objIdx;
		
		ds.addLineEdit( "  object " + QStr(catObjIdx+1) + ":", 
										&catName[c],
										"Represents the label of a category object.... \n"
										"To help you recognize it as a cateogry object it should \n"
									  "always contain '(STEREOLOGY)' and be in the form: \n"
										" > 'CategoryName. #(STEREOLOGY)#grid_set=1#' ");
		
		ds.addLineEdit( "       label:", 
									 &catLabel[c],
									 "Represents the laebel of the grid setting's object.... \n"
									 "this should always start with 'STEREOLOGY.SETTINGS' and \n"
									 "be in the form: \n"
									 "  STEREOLOGY.GRID #grid_set=1#type=pts#spacing=50,50,5#x=10,1014\n"
									 "  #y=10,1014#z=10,190#(DONT_RENAME)#");
		
		ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
	}
	
	ds.exec();
	
	if( ds.wasCancelled() )
		return;
	
	
	//## CHANGE LABEL AND NAME VALUES:
	
	int namesChanged  = 0;
	int labelsChanged = 0;
	
	if( gridName != getObjName(g->objIdx).toStdString() )
	{
		imodObjectSetName( getObj(imod,g->objIdx), (char *)gridName.c_str() );
		namesChanged++;
	}
	if ( gridLabel != getObjLabel(g->objIdx).toStdString() )
	{
		setObjLabel( getObj(imod,g->objIdx), (QString)(gridLabel.c_str()) );
		labelsChanged++;
	}
	
	for( int c=0; c<(int)catName.size() && c<(int)g->catObj.size(); c++ )
	{
		int catObjIdx = g->catObj[c].objIdx;
		
		if( catName[c] != getObjName(catObjIdx).toStdString() )
		{
			imodObjectSetName( getObj(imod,catObjIdx), (char *)catName[c].c_str() );
			namesChanged++;
		}
		if ( catLabel[c] != getObjLabel(catObjIdx).toStdString() )
		{
			setObjLabel( getObj(imod,catObjIdx), (QString)(catLabel[c].c_str()) );
			labelsChanged++;
		}
	}
	
	
	
	//## SHOW RESULTS AND RELOAD:
	
	if( namesChanged==0 && labelsChanged==0 )
	{
		MsgBox( this, "...", "You made no changes, so no reload will occur" );
		return;
	}
	
	QMessageBox::information( this, "Summary of your changes:", "You have changed: \n"
													  " > " + QStr(namesChanged)  + " object names and \n"
													  " > " + QStr(labelsChanged) + " object labels \n\n"
													  "The grid will now reload, so I hope you \n"
													  "remembered to save!" );
	
	loadGridFromImodModel( false );
	
}



//------------------------
//-- Has options for adding categories to a grid which is already setup.

void Stereology::addExtraGridCategoriesToImodModel()
{
	GridSetObj *g = getCurrGridSetObj();
	Imod *imod  = ivwGetModel(plug.view);
	
	
	//## SHOW CUSTOM DIALOG WITH FIELDS TO ADD NEW CATEGORIES:
	
	const int MAX_NEW_OBJECTS = 4;
	string newCatName [MAX_NEW_OBJECTS];
	bool   addCat     [MAX_NEW_OBJECTS];
	
	for( int c=0; c<MAX_NEW_OBJECTS; c++ )
	{
		newCatName[c] = "";
		addCat[c]     = (c == 0);
	}
	
	CustomDialog ds("Add Extra Categories:", this, BS_CUSTOM );
	
	ds.addCustomButton( "Cancel", BB_REJECT   );
	ds.addCustomButton( "Add and Reload", BB_ACCEPT,
											"Will add the objects you've ticked and then reload this\n"
											"grid from the model (so it should reflect changes");
	
	for( int c=0; c<MAX_NEW_OBJECTS; c++ )
	{
		ds.addLineEdit( "   ... category name:", 
										 &newCatName[c], "Enter here the name of the new category");
		ds.addAutoCompletePrev( plug.wordList, false );
		
		ds.addCheckPrev( "add this new category:", 
											&addCat[c], CB_NONE, true,
										  "Allows you to add up to three extra categories at a time" );
		
		ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
	}
	
	ds.exec();
	
	if( ds.wasCancelled() )
		return;
	
	
	//## ADD NEW CATEGORIES TO CURRENT GRID:
	
	for( int c=0; c<MAX_NEW_OBJECTS; c++ )
	{
		if( addCat[c] )
		{
			addCategory();
			g->catObj.back().categoryName = (QString)(newCatName[c].c_str());
		}
	}
	
	int numObjsAdded   = 0;
	int numObjsChanged = 0;
	setupImodObjsFromGridAndCats( false, true, &numObjsAdded, &numObjsChanged );
	
	QMessageBox::information( this, "Summary of change:",
													 QStr(numObjsAdded)  + " new objects were added \n"
													 "We will now try and reload and hope for the best" );
	
	loadGridFromImodModel( false );
}


//------------------------
//-- Has options for doubling the number of points in the grid by halving
//-- the grid spacing along x, y or z.

void Stereology::makeGridFiner()
{
	GridSetObj *g = getCurrGridSetObj();
	Imod *imod  = ivwGetModel(plug.view);
	
	//## DETERMINE WHICH AXIS CAN AND CANNOT BE HALVED:
	
	int halfX = (g->xSpacing / 2);
	int halfY = (g->ySpacing / 2);
	int halfZ = (g->zSpacing / 2);
	
	bool canHalfX = 2.0f*halfX == g->xSpacing;
	bool canHalfY = 2.0f*halfY == g->ySpacing;
	bool canHalfZ = 2.0f*halfZ == g->zSpacing;
	
	QString xSpNew = (canHalfX) ? QStr(halfX) : " NOT APPLICABLE";
	QString ySpNew = (canHalfY) ? QStr(halfY) : " NOT APPLICABLE";
	QString zSpNew = (canHalfZ) ? QStr(halfZ) : " NOT APPLICABLE";
	
	//## IF NONE OF THE AXIS CAN BE HALVED: EXIT EARLY
	
	if (!canHalfX && !canHalfY && !canHalfZ)
	{
		MsgBox("Sorry, none of your spacings (x,y or z) \n"
					 "are evenly divisible by 2. Try the modify labels \n"
					 "option and read the help file and you should \n"
					 "be able to divide by some other amount instead \n"
					 "and change the grid setting label by hand");
		return;
	}
	
	//## SHOW CUSTOM DIALOG WITH OPTION TO HALVE X, Y OR Z SPACING:
	
	int axisToHalve = (canHalfX) ? 0 : (canHalfY) ? 1 : 2;
	
	CustomDialog ds("Increase grid fidelity:", this, BS_CUSTOM );
	
	ds.addCustomButton( "Cancel", BB_REJECT   );
	ds.addCustomButton( "Apply and Reload", BB_ACCEPT,
										 "Will apply your changes then reload this grid from\n"
										 "the model (so it should reflect changes");
	ds.addHtmlLabel ( "double the number of points by \n"
										"having the grid spacing along: ");
	ds.addRadioGrp( "",
									"x --> from " + QStr(g->xSpacing) + " to " + xSpNew + "|"
		 							"y --> from " + QStr(g->ySpacing) + " to " + ySpNew + "|"
		  						"z --> from " + QStr(g->zSpacing) + " to " + zSpNew,
									&axisToHalve, "",
									"select this to half the x spacing|"
									"select this to half the y spacing|"
									"select this to half the z spacing - meaning twice as many grids");
	
	ds.addHtmlLabel ( "<b>WARNING</b>: You cannot undo this operation! <br>"
									  "It's recommended you save before you hit okay." );
	ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	
	ds.exec();
	
	if( ds.wasCancelled() )
		return;
	
	//## CHECK HALVING SELECTED AXIS IS POSSIBLE:
	
	if(   (axisToHalve == 0 && !halfX )
		 || (axisToHalve == 1 && !halfY )
		 || (axisToHalve == 2 && !halfZ ) )
	{
		QMessageBox::information( this, "...",
														 "Sorry you cannot half the spacing along this axis - "
														 "it is not evenly divisible by two." );
		return;
	}
	
	//## ADD NEW CATEGORIES TO CURRENT GRID:
	
	if     ( axisToHalve == 0 )	{	g->xSpacing = halfX;	}
	else if( axisToHalve == 1 )	{	g->ySpacing = halfY;	}
	else if( axisToHalve == 2 )	{	g->zSpacing = halfZ;	}
	
	int numObjsAdded   = 0;
	int numObjsChanged = 0;
	setupImodObjsFromGridAndCats( false, true, &numObjsAdded, &numObjsChanged );
	
	if( numObjsChanged > 0 )
	{
		QMessageBox::information( this, "Summary of changes:",
		  											  "The grid setting object was changed. \n"
			  										  "We will now try and reload and hope for the best" );
	}
	else
	{
		QMessageBox::information( this, "Fail", "Was unable to make changes... \n"
														  "try changing labels manually instead");
	}
	
	loadGridFromImodModel( false );
}






//------------------------
//-- Called when the main tab ("tabWidget") is changed which basically redraws
//-- the grid.

void Stereology::changeTabSelected( int newValue )
{
	//tabWidget->currentIndex();
	drawGridObject( true );
}


//------------------------
//-- Called when "chkShowGridDisplayOpts" is clicked, which shows or
//-- hides a small grid display options group box as appropriate.

void Stereology::changeShowGridDisplayChk()
{
	plug.showGridDisplayOpts = chkShowGridDisplayOpts->isChecked();
	grpGridDisplayOpts->setVisible( plug.showGridDisplayOpts );
}


//------------------------
//-- Called when "cmbGridDisplayOpt" is changed, which registers and redraw 
//-- changes to how grid lines are drawn.

void Stereology::changeGridTypeCmb( int newValue )
{
	plug.gridDisplayOpt = cmbGridDisplayOpt->currentIndex();
	if(	!getCurrGridSetObj()->countingStarted )
		getCurrGridSetObj()->gridType = plug.gridDisplayOpt;
	updateGridFromGridGui(false, false, false);
}


//------------------------
//-- Called when either "spnSpacingX" or "spnSpacingY" are changed and
//-- updates/redraws the grid settings

void Stereology::changeGridDblSpn( double newVal )
{
	updateGridFromGridGui(false, true, false);
}

//------------------------
//-- Called when of the many integer spin boxes in the "Grid Setup" tab
//-- are changed and updates/redraws the grid settings.

void Stereology::changeGridSpn( int newVal )
{
	updateGridFromGridGui(false, true, plug.jumpToGridOnUpdate );
}

//------------------------
//-- Called when of the many check boxes in the "Grid Setup" tab
//-- are changed and updates/redraws the grid settings.

void Stereology::changeGridChk()
{
	updateGridFromGridGui(true, true, plug.jumpToGridOnUpdate);
}






//############################################################
//## CATEGORY SELECTION METHODS:







//------------------------
//-- Adds a new category to the current grid and refreshes the
//-- list in the GUI. The new category inherits the same "gridSetId"
//-- as the grid, and it's color is selected using a pin wheel of 
//-- about 10 different colors (see below).

void Stereology::addCategory()
{
	updateCatsFromItemListGui();
	
	GridSetObj *g = getCurrGridSetObj();
	
	CategoryObj newCatObj;
	newCatObj.objIdx    = -1;								// we don't know what object yet, 
	newCatObj.gridSetId = g->gridSetId;			// but we do know what grid set id
	
	int catIdxColorWheel = g->catObj.size() % 10;
	switch( catIdxColorWheel )
	{
		case(0): newCatObj.color = QColor(0,0,255);			break;			// green
		case(1): newCatObj.color = QColor(0,255,0);			break;			// blue
		case(2): newCatObj.color = QColor(255,0,255);		break;			// purple (magenta)
		case(3): newCatObj.color = QColor(255,255,0);		break;			// yellow
		case(4): newCatObj.color = QColor(0,255,255);		break;			// aqua (cyan)
		case(5): newCatObj.color = QColor(255,50,100);	break;			// peach
		case(6): newCatObj.color = QColor(255,130,0);		break;			// orange
		case(7): newCatObj.color = QColor(150,100,50);	break;			// brown
		case(8): newCatObj.color = QColor(0,100,0);			break;			// dark green
		case(9): newCatObj.color = QColor(0,0,50);			break;			// dark blue
	}
	
	g->catObj.push_back( newCatObj );
	
	updateItemListGuiFromCats();
}

//------------------------
//-- Removes the last category from the current grid and refreshes
//-- this list in the GUI. If there are two or less objects, an
//-- warning message appears and no categories are removed.

void Stereology::deleteCategory()
{	
	updateCatsFromItemListGui();
	
	GridSetObj *g = getCurrGridSetObj();
	if( g->catObj.size() <=2 )
	{
		MsgBox( this, "Try Again :)",
					 "Sorry, you need at LEAST two objects to do stereology");
		return;
	}
	g->catObj.resize( g->catObj.size() - 1 );
	
	updateItemListGuiFromCats();
}


//------------------------
//-- Clear categoies list and setup default categories for
//-- counting intersections

void Stereology::setupCountingCategories( int maxIntersections, QString labelStr )
{	
	GridSetObj *g = getCurrGridSetObj();
	
	if( maxIntersections < 1 )
	{
		MsgBox( this, "Try Again",
					 "Sorry, you need at LEAST two objects to do stereology");
		return;
	}
	
	g->catObj.clear();
	
	//## ADD FIRST OBJECTS:
	
	CategoryObj newCatObj;
	newCatObj.gridSetId = g->gridSetId;
	newCatObj.lineWidth  = 1;
	newCatObj.sphereSize = 1;
	newCatObj.color      = QColor(50,50,50);		// grey
	newCatObj.setNameAndTooltip( "0_" + labelStr,
															"Select this (by pressing [~] or [0] key) \n"
															"when the line segment you're looking at  \n"
															"has no membranes intersecting it." );
	
	g->catObj.push_back( newCatObj );
	
	
	//## ADD NEXT OBJECTS:
	
	for( int i=0; i<maxIntersections; i++)
	{
		CategoryObj newCatObj;
		newCatObj.gridSetId = g->gridSetId;			// but we do know what grid set id
		newCatObj.lineWidth  = (i%3) + 1;
		newCatObj.sphereSize = plug.gridSymbolSize + 2*(i%3);
		newCatObj.setNameAndTooltip( QStr(i+1) + "_" + labelStr,
																"Select this when the line segment you're \n"
																"looking at has " + QStr(i+1) + " intersections \n"
																"with your surface type of interest." );
		
		int catIdxColorWheel = (i / 3);
		switch( catIdxColorWheel )
		{
			case(0): newCatObj.color = QColor(0,0,255);			break;			// blue
			case(1): newCatObj.color = QColor(0,220,0);			break;			// green
			case(2): newCatObj.color = QColor(180,255,180);	break;			// light green
			case(3): newCatObj.color = QColor(255,130,0);		break;			// orange
		}
		
		g->catObj.push_back( newCatObj );
	}
	
	updateItemListGuiFromCats();
}


//------------------------
//-- This function is called when the user clicks the "Finalize Categories"
//-- button. If the user has entered less than two categories or any category,
//-- is missing a name it shows an error message and nothing happens.
//-- If it categories pass this test, however, the user is given the option to
//-- "Finalize Objects". If the user clicks yes, it will first check to 
//-- see if IMOD objects representing the current grid already exist. If this
//-- is the case the user is given an option to overwrite the existing
//-- names with any new values and/or colors. At this point the function calls
//-- "setupImodObjsFromGridAndCats()" which creates new objects and/or overwrites or
//-- ignores objects which already exist.
//-- 
//-- Finally, a dialog appears to show a summary of how many objects were
//-- created, renamed or left alone.... and the GUI is updated to match
//-- the fact that categories are "locked in" and counting commenced by setting
//-- the current grid's "countingStarted" value to true and calling
//-- "updateGuiToMatchCountingStarted()"

void Stereology::startCounting()
{	
	updateCatsFromItemListGui();
	GridSetObj *g = getCurrGridSetObj();
	
	//## IF ONE OR LESS CATEGORIES: SHOW ERROR:
	
	int numCats = (int)g->catObj.size();
	
	if( numCats==0 )
	{
		QMessageBox::warning(this, "Cannot Finalize Grid...",
												 "You should create at LEAST two objects to do stereology");
		return;
	}
	
	//## CHECK IF ANY CATEGORIES HAVE NO NAME: IF SO SHOW ERROR:
	
	int unnamedCats = 0;
	for(int c=0; c<numCats; c++)
		if( g->catObj[c].categoryName.isEmpty() || g->catObj[c].categoryName == "" )
			unnamedCats++;
	
	if( unnamedCats > 0 )
	{
		QMessageBox::warning(this, "Cannot Finalize Grid...",
												 "Some of your categories are missing names. Giving all "
												 "objects and categories is important for later reference.");
		return;
	}
	
	//## ASK USER IF HE WANTS TO CONTINUE:
	
	static bool multiCats = false;
	bool allowIntercepts = ( g->isSurfaceAreaGridType() );
	
	CustomDialog ds("Finalize Objects?", this);
	ds.addLabel   ( "Are you ready to 'lock in' these \n"
								  "categories and commence point counting?", true );
	
	ds.addCheckBox( "allow multiple categories per point", 
								 &multiCats,
								 "In most stereology studies you will want to leave this off as \n"
								 "categories are *usually* mutually exclusive.\n"
								 "\n"
								 "If off: the  plugin will prevent you from accidentally assigning \n"
								 "more than one category to a single point, by replacing existing \n"
								 "categories if you select a different category for any given point.\n"
								 "\n"
								 "If on: you will be allowed to turn on multiple categories per \n"
								 "point. Also, the 'auto progress' to the next point feature (when \n"
								 "you hit numbe shortcuts) will be disabled. An extra string \n"
								 "'#multi_cats#' will be added to your 'GRID.SETTING' object \n"
								 "to remember this setting after you save. Be warned that this \n"
								 "option makes point classification trickier!" );
	ds.setStylePrev("background-color: rgb(100, 100, 255);");			// light blue
	
	if( g->isSurfaceAreaGridType() || g->isNumberDensityGridType() )
	{
		ds.addCheckBox( "allow intercepts", 
									  &allowIntercepts,
									  "Shows a button called 'Intercepts' which allows you to \n"
									  "add category points wherever a surface intersects a line." );
		ds.setStylePrev("background-color: rgb(100, 100, 255);");			// light blue
	}
	
	
	ds.addLabel   ( "-----\n"
								 "NOTE:\n"
								 " > Once you hit 'Ok' it's not easily \n"
								 "   possible to change category names." );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	g->allowMultipleCats = multiCats;
	g->allowIntercepts   = allowIntercepts;
	
	//## CHECK IF OBJECTS ALREADY SETUP: AND IF SO GIVE USER OPTION TO OVERWRITE NAMES:
	
	bool skipObjsIfExist = false;
	bool overwriteNames  = false;
	
	bool gridSettingObjSetup = isStereologyObj( g->objIdx );
	
	int objsAlreadySetup = 0;
	for(int c=0; c<numCats; c++)
		if( isStereologyObj( g->catObj[c].objIdx ) )
			objsAlreadySetup++;
	
	if( gridSettingObjSetup || objsAlreadySetup > 0 )
	{
		static int overwriteNamesOpt = 0;
		
		CustomDialog dsC("Possible Object Name Conflicts",this);
		
		if( gridSettingObjSetup )
		{
			int objIdx = g->objIdx;
			dsC.addLabel    ("The following grid setting object already exists:" );
			dsC.addReadOnlyLineEdit( "object " + QStr(objIdx+1)+ ":", getObjName( objIdx ) );
			dsC.addLabel    ("");
		}
		
		if( objsAlreadySetup > 0 )
		{
			dsC.addLabel    ("The following 'stereology' object already exists:" );
			
			for(int c=0; c<numCats; c++)
			{
				if( isStereologyObj( g->catObj[c].objIdx ) )
				{
					int objIdx = g->catObj[c].objIdx;
					dsC.addReadOnlyLineEdit( "object " + QStr(objIdx+1)+ ":", getObjName( objIdx ) );
				}
			}
			dsC.addLabel    ("");
		}
		
		dsC.addRadioGrp( "choose action:",
									  "leave these names unchanged|"
									  "changes have been made - overwrite these names",
									  &overwriteNamesOpt, "",
									  "Select this if the names above match your current settings|"
									  "Select this if you have made changes to the grid or category \n"
									  "names and want the object names changed to the new values" );
		
		dsC.exec();
		if( dsC.wasCancelled() )
			return;
		
		overwriteNames  = (overwriteNamesOpt==1);
		skipObjsIfExist = !overwriteNames;
	}
	
	
	//## IF FORBIDDEN SQUARES ARE SHOWING: ASK USER IF HE WANTS TO OMIT THESE:
	
	bool useForbiddenSquares = false;
	
	if( g->showSubRects )
	{
		static int forbiddenSquaresAct = 1;
		
		CustomDialog dsF("Use Forbidden Rectangles to Subsample",this);
		
		dsF.addLabel    ("You have turned on 'forbidden rectangles' \n"
										 "and thus probably want to use these to subsample \n"
										 "the grid (reduce points)." );
		dsF.addRadioGrp( "choose action:",
									  "(A) leave rectangles on, but setup all points|"
									  "(B) setup only points inside these rectangles",
									  &forbiddenSquaresAct, "",
									  "Select this if you think you may want to classify all points|"
									  "Select this if you want to easily navigate and only classify \n"
									  "and/or count points/lines inside the forbidden rectangles \n"
										"(meaning less work)." );
		
		dsF.exec();
		if( dsF.wasCancelled() )
			return;
		
		useForbiddenSquares = (forbiddenSquaresAct==1);
	}
	
	
	//## SETUP IMOD OBJECTS FROM CATEGORIES AND GRID SETUP:
	
	int numObjAdded   = 0;
	int numObjChanged = 0;
	bool success = setupImodObjsFromGridAndCats( skipObjsIfExist, overwriteNames,
																			         &numObjAdded, &numObjChanged );
	
	if( success )
	{
		GridSetObj *g = getCurrGridSetObj();
		g->setupPts( useForbiddenSquares );
		g->countingStarted = true;
		
		updateGuiToMatchCountingStarted();
	}
	ivwDraw(plug.view, IMOD_DRAW_MOD);	// redraw window to reflect new number of objects
	
	
	//## PRINT RESULTS:
	
	if( numObjChanged > 0 )
	{
		QMessageBox::information(this, "Finalize grid",
														 QStr( numObjAdded ) + " objects were added to your model "
														 "and " + QStr(numObjChanged) + " objects were "
														 "changed. It's time now to start classifying points.");
	}
	else if( numObjAdded > 0 )
	{
		QMessageBox::information(this, "Finalize grid",
														 QStr( numObjAdded ) + " objects were added to your model, "
														 "you are now ready to start classifying points.");
	}
	else
	{
		QMessageBox::information(this, "Finalize grid",
														 "No extra objects were added... but you are now ready to "
														 "start classifying points.");
	}
	
	//## IF RANDOM POINTS MODE: PRINT MESSAGE AND REFRESH
	
	if( g->allowRandomPts() )
	{
		QMessageBox::information(this, "Random Points",
														 "You have chosen the random points option. \n"
														 "To add points to the grid area you will to   \n"
														 "click the 'Random Pts+' button.");
		drawGridObject( true );
	}
	
	updateItemListGuiFromCats();
}




//------------------------
//-- Shows/hides and enables/disables various GUI elements on the main 
//-- plugin interface based on the value of "countingStarted" of the 
//-- current GridSetObj. If counting has started, the list of categories
//-- becomes buttons (rather than text edit boxes), the grid setup
//-- tab becomes disables to prevent changes, and various buttons such
//-- as "Finalize Categories" and "Finalize Grid" disappear.

void Stereology::updateGuiToMatchCountingStarted()
{
	GridSetObj* g = getCurrGridSetObj();
	
	btnAddCat->setVisible     ( !g->countingStarted );
	btnDelCat->setVisible     ( !g->countingStarted );
	btnCatOpts->setVisible    ( !g->countingStarted );
	btnStartCount->setVisible ( !g->countingStarted );
	
	chkPtChecked->setVisible  ( g->countingStarted  );
	
	widGridSetup->setEnabled  ( !g->countingStarted );
	grpGridDisplayOpts->setEnabled( true );
	
	btnPaint->setVisible( g->countingStarted );
	btnIntercept->setVisible( g->countingStarted && g->allowIntercepts );
	
	btnRandom->setVisible( !g->countingStarted );
	btnAddRandPts->setVisible( g->countingStarted && g->allowRandomPts() );
	
	if( !g->countingStarted )
	{
		btnPaint->setChecked( false );
		btnIntercept->setChecked( false );
		plug.paintMode = PM_OFF;
	}
}







//------------------------
//-- Removes all "line elements" from the categories list
//-- (under the "Classify Points" tab)

void Stereology::removeAllLineItems()
{
	for( int i=0; i<(int)lineItem.size(); i++ )
	{
		lineItem[i].widLine->setVisible( false );
		layList->removeWidget( lineItem[i].widLine );
	}
}


//------------------------
//-- Refreshes/regenerates the main list of category names displayed in the plugin.
//-- it achieves this by enlarging/reducing the "lineItem" array (to match the
//-- current number of objects), initializing/setting up widgets in a table
//-- layout, and calling "refreshObjItem" for each lineItem

void Stereology::updateItemListGuiFromCats()
{
	//## IF THERE ARE NO GRIDS FOUND: HIDE ANY LINE ITEMS THAT MAY BE SHOWN AND RETURN:
	
	if( plug.grid.size() == 0 )
	{
		removeAllLineItems();
		return;
	}
	
	//## IF SELECTED GRID IS EMPTY: PROMPT USER TO ADD CATEGORIES:
	
	GridSetObj *g = getCurrGridSetObj();
	int numCats = g->catObj.size();			// number of categories in selected grid
	
	if( numCats == 0 )
	{
		removeAllLineItems();
		QMessageBox::warning( this, "Warning",
												 "This grid (grid_set='" + g->gridSetId +
												 "') does not have any\n"
												 "categories to chose from. Click '+' to add more." );
		return;
	}
	
	
	//## HIDE AND REMOVE ANY UNNECESSARY LINES:
	
	int prevSize = (int)lineItem.size();
	
	for( int i=numCats; i<(int)lineItem.size(); i++ )	// hide and remove superfluous lines
	{
		lineItem[i].widLine->setVisible( false );
		layList->removeWidget( lineItem[i].widLine );
	}
	
	//## IF GRID HAS CATEGORIES: INITIALIZE AND UPDATE LINE OBJECTS TO MATCH:	
	
  lineItem.resize( numCats );
  
	QFont smallFont;		smallFont.setPixelSize(10);
  QFont smallerFont;  smallerFont.setPixelSize(9);
	
  for(int i=0; (int)i<lineItem.size() && i<numCats; i++)
  {
    StereoLineItem &item = lineItem[i];
    CategoryObj &catObj = g->catObj[i];
		
    //## IF ITEM NOT SETUP: INITIALIZE GUI ELEMENTS:
    
    if( !item.setup )
    {
      item.widLine       = new QWidget( widList );
      item.layLine       = new QHBoxLayout( item.widLine );
      item.layLine->setSpacing(4);
      item.layLine->setContentsMargins(2, 2, 2, 2);
			item.widLine->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Expanding );
      item.widLine->setLayout( item.layLine );
			
      item.lblShortcut     = new QLabel();
      item.lblShortcut->setFixedWidth(20);
			if(i<10) {
				item.lblShortcut->setText( "[" + QStr(i) + "]" );
				item.lblShortcut->setToolTip( "Use this shortcut key to check this category");
			}
			else {
				item.lblShortcut->setText( "." + QStr(i+1) + "." );
				item.lblShortcut->setToolTip( "Unfortunately number keys don't go this high,\n"
																		 "so to check this category you'll have to push\n"
																		 "the button instead");
			}
			
      item.btnColor      = new ColorButton( catObj.color, item.widLine);
			connect(item.btnColor, SIGNAL(clicked()), this, SLOT(changeColorBtn()));
			
			item.btnCat = new QPushButton( item.widLine );
			item.btnCat->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Minimum );
			item.btnCat->setCheckable( true );
			connect(item.btnCat, SIGNAL(clicked()), this, SLOT(catToggleBtnPushed()));
			
			item.btnCat->setToolTip("Click to assign this category to the selected point");
			
			item.txtName = new QLineEdit();
			item.txtName->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Minimum );
			item.txtName->setCompleter(completer);
			
      item.layLine->addWidget( item.lblShortcut );
      item.layLine->addWidget( item.btnColor );
      item.layLine->addWidget( item.btnCat );
			item.layLine->addWidget( item.txtName );
			
      item.setup = true;
    }
    
		
    //## UPDATE GUI ELEMENTS TO MATCH CATEGORY OBJECTS:
    
    item.btnColor->setColor( catObj.color );
    item.btnCat->setText( catObj.categoryName );
		item.txtName->setText( catObj.categoryName );
		
		if( !catObj.nameEntry.name.isEmpty() && catObj.categoryName == catObj.nameEntry.name ) 
			item.btnColor->setToolTip( catObj.toolTip );
		
		item.btnCat->setVisible( g->countingStarted );
		item.txtName->setVisible( !g->countingStarted );	
  }
  
  //## MAKE NEW LINES VISIBLE AND RESIZE:
	
	for( int i=prevSize; i<(int)lineItem.size(); i++ )	// show and add all new lines
	{
		lineItem[i].widLine->setVisible( true );
		layList->addWidget( lineItem[i].widLine );
	}
	
	int LINE_HEIGHT = 30;
	widList->setFixedHeight( LINE_HEIGHT * (int)lineItem.size() + 4 );
	
	resizeEvent( NULL );
}


//------------------------
//-- Updates the "catObj" vector in the currently selected grid
//-- using the name and color values based on the values
//-- entered/changed by the user in the GUI where the categories
//-- are listed (under the "Classify Points" tab).

void Stereology::updateCatsFromItemListGui()
{
	GridSetObj *g = getCurrGridSetObj();
	int numCats = g->catObj.size();			// number of categories in selected grid
	
  for(int i=0; (int)i<lineItem.size() && i<numCats; i++)
  {
    StereoLineItem &item = lineItem[i];
    CategoryObj &catObj = g->catObj[i];
    
		catObj.color        = item.btnColor->getColor();
		catObj.categoryName = item.txtName->text();
  }
}







//############################################################
//## POINT SELECTION/CHANGE METHODS:



//------------------------
//-- Presents the user with a number of options for generating
//-- a new set of random points. Once the user clicks okay,
//-- these values are added to the "pts" vector in the currently
//-- selected grid.

void Stereology::addRandomPts()
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( !g->allowRandomPts() )
	{
		MsgBox( this, "...", "This only works if the grid type is set to 'rand pts'" );
		return;
	}
	
	//## GET USER INPUT VIA A CUSTOM DIALOG:
	
	static bool checkPtsForDist = false;
	static float minDistFromOtherPtsSpn = 1.0f;
	static int totalRetries = 0;
	
	static bool useSpacingX  = false;
	static bool useSpacingY  = false;
	static bool useSpacingZ  = false;
	static bool biasPtsSameZ = false;
	static float biasCoefForSameZAsLastRandPt = 0.0f;
	
	
	CustomDialog ds("Add Random Pts", this);
	
	ds.addLabel   ( "--- NUMBER OF POINTS TO ADD: ---", true );
	
	ds.addSpinBox ( "number of points to add:", 1, 1000000, &plug.numRandPtsToAdd, 100,
								  "The number of random points to add (within the grid limits) \n"
								  "when you click 'Okay'. \n"
								  "TIP: It's better to make this number too low than too high \n"
								  "(i.e. more than you can classify in one sitting), as \n"
								  "it's easy to click this again to more add random points later." );
	
	ds.addLineEditF( "", 0.0f, 1000.0f, &minDistFromOtherPtsSpn, 6,
									"The minimum distance, in pixels, you want two points placed. \n"
									"NOTE: If you turn this on and set it to 0 no two points \n"
									"should be on exactly the same spot.", " pixels apart" );
	
	ds.addCheckPrev("ensure pts are ", &checkPtsForDist, CB_NONE, true,
									"If true: each new point added is checked against EVERY \n"
									"other point to check that it's not within this distance \n"
									"in pixels of another point on the same slice. If a randomly \n"
									"generated point 'clashes' the algorithm won't add this point and \n"
									"tries again, up to the number of attempts shown below.\n" );
	
	ds.addSpinBox ( "    ... # of retry attempts if clash:", 1, 5000, &totalRetries, 10,
								 "If the above option is on, and a randomly generated point \n"
								 "is within the specified distance of an existing point... \n"
								 "the algorithm will retry this many times to find a new \n"
								 "point which satisfies your criterion before giving up." );
	
	ds.beginGroupBox( "Restrictions for point placement:", false,
									 "Select below options to limit or 'snap' points using \n"
									 "the spacing along each axis." );
	ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
	
	ds.addCheckBox( "use X spacing", 
									&useSpacingX,
									"If true: randomly generated points will be placed \n"
									"on one of the vertical gridlines (along X)");
	
	ds.addCheckBox( "use Y spacing", 
								 &useSpacingY,
								 "If true: randomly generated points will be placed \n"
								 "on one of the horizonal gridlines (along Y)");
	
	ds.addCheckBox( "use Z spacing", 
								 &useSpacingZ,
								 "If true: randomly generateds point will be placed \n"
								 "on one of the slices dictated by the grid's z spacing \n"
								 "value (starting at zMax)");
	
	ds.addLineEditF( "", 0.0f, 1.0f, &biasCoefForSameZAsLastRandPt, 6,
									 "Set between 0 and 1. Mouse over previous tooptip for more info." );
	
	ds.addCheckPrev("chance pts has same z as prev:", &biasPtsSameZ, CB_NONE, true,
									"If true, the z value of each point will have this chance \n"
									"(a value between 0 and 1) that it's z value is changed to \n"
									"fall on the same slice as the last stereology point added.\n"
									"This can help decrease the amount points 'jump' between \n"
									"sections.... but if in doubt, leave this value as 0 - because \n"
									"if only a few points are generated it will make for an \n"
									"uneven distribution between slices." );

	
	ds.endGroupBox ();
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	
	//## IF X AND Y SPACING WERE SET: SHOW WARNING
	
	if( useSpacingX && useSpacingY )
	{
		CustomDialog dsR("Random Pts", this);
		
		dsR.addHtmlLabel ( "<b>NOTE</b>: If both X and Y spacing are on <br>"
										 "it becomes likely multiple points wil be <br>"
										 "placed exactly the same spot." );
		dsR.setStylePrev("background-color: rgb(255, 40, 40);");			// red
		dsR.addLabel ( "Are you sure you want to continue?" );
		
		dsR.exec();
		if( dsR.wasCancelled() )
			return;
	}
	
	
	//## GENERATE RANDOM POINTS:
	
	float minDistFromOtherPts      = (checkPtsForDist) ? minDistFromOtherPtsSpn : -1.0f;
	float biasForSameZAsLastRandPt = (biasPtsSameZ) ? biasCoefForSameZAsLastRandPt : -1.0f;
	
	long numPtsAdded = g->addRandomPts( plug.numRandPtsToAdd,
																		  useSpacingX, useSpacingY, useSpacingZ,
																		  minDistFromOtherPts, totalRetries,
																		  biasForSameZAsLastRandPt );
	
	
	if( numPtsAdded != plug.numRandPtsToAdd )
	{
		MsgBox( this, "Results", "Only '" + QStr(numPtsAdded) + "' points were added. \n"
					                   "Try changing your restictions to add more." );
	}
	
	drawGridObject(true);
	updateCurrPtInGui( true, true );
	
}



//------------------------
//-- Jumpts to a random grid and random XY location within the limits
//-- This function is called by the "Random Pts" button... and this 
//-- button is typically made invisible when point counting has started

void Stereology::goToRandomPos()
{
	GridSetObj* g =getCurrGridSetObj();
	
	if( !g->countingStarted )
	{
		Ipoint randPt = g->genRandomPt( false, false, true );
		
		edit_setZapLocation( randPt.x, randPt.y, randPt.z, true );
		ivwSetTopZapCenter( plug.view, randPt.x, randPt.y, randPt.z, true );
	}
	else
	{
		Ipoint randPt = g->genRandomPt( true, true, true );
		
		edit_setZapLocation( randPt.x, randPt.y, randPt.z, true );
		ivwSetTopZapCenter( plug.view, randPt.x, randPt.y, randPt.z, true );
	}
}


//------------------------
//-- A group of functions which iterate the currently selected point
//-- to the next or previous, column, row or grid (see: "jumpToPt()").

void Stereology::goToNextPt()		{		jumpToPt( 1 );		}
void Stereology::goToPrevPt()		{		jumpToPt( -1 );	}
void Stereology::goToNextRow()	{		jumpToPt( getCurrGridSetObj()->ptsPerRowSetup );	 }
void Stereology::goToPrevRow()	{		jumpToPt( -getCurrGridSetObj()->ptsPerRowSetup );	 }
void Stereology::goToNextGrid()	{		jumpToPt( getCurrGridSetObj()->ptsPerGridSetup );	 }
void Stereology::goToPrevGrid()	{		jumpToPt( -getCurrGridSetObj()->ptsPerGridSetup ); }

//------------------------
//-- Changes the currently selected point to the specified value, 
//-- making sure the value is still valid. The ZAP jumps to the 
//-- this point and the GUI updates to reflect the change. 

void Stereology::jumpToPt( int changeAmount )
{
	getCurrGridSetObj()->changeCurrPt( changeAmount, true );
	updateCurrPtInGui( true, true );
}


//------------------------
//-- Finds and then selects/jumps to the next, previous, first
//-- or last unchecked point.

void Stereology::goToNextUncheckedPt()	{		jumpToUncheckedPt( true,  false  );	}
void Stereology::goToPrevUncheckedPt()	{		jumpToUncheckedPt( true,  true   );	}
void Stereology::goToFirstUncheckedPt()	{		jumpToUncheckedPt( false,  true  );	}
void Stereology::goToLastUncheckedPt()	{		jumpToUncheckedPt( false,  false );	}

//------------------------
//-- Searches for and selects an unchecked point. If "startAtCurrPt"
//-- is true it will select the next or previous unchecked point
//-- just after or before the currently selected point, or else,
//-- if "startAtCurrPt" is false, it will find the very first
//-- or very last unchecked point in the "pts" vector (dending
//-- on the value of "backwards"). The function searches forwards 
//-- or backwards if the value of "backwards"
//-- is false or true respectively.
//-- 
//-- If an unchecked point is found and updated, the ZAP jumps to the 
//-- this point and the GUI updates to reflect the change.
//-- If no unchecked points exist, a mesage box pops up.

void Stereology::jumpToUncheckedPt( bool startAtCurrPt, bool backwards )
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( g->countingStarted )
	{
		bool uncheckedPtFound = g->selectFirstUncheckedPt(startAtCurrPt, backwards);
		if( uncheckedPtFound )
		{
			updateCurrPtInGui( true, true );
		}
		else
		{
			MsgBox( this, "Grids Finished!", g->allowRandomPts() ?
						        "No unchecked point found \n"
						        "Click 'Random Pts+' to add more." : 
										"All points have been checked! \n"
						        "Click 'Options > Results' to see results." );
		}
	}
	else
	{
		g->changeCurrPt( (backwards) ? -1 : 1, false );
		updateCurrPtInGui( true, true );
	}
}





//------------------------
//-- Called when the "spnSelPt" spin box is changed. the ZAP jumps  
//-- to the this point and the GUI updates to reflect the change.

void Stereology::changeCurrPtSpn( int newPtNum )
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( g->currPtIdx != newPtNum-1 )
	{
		g->setCurrPt( newPtNum-1, false );
		updateCurrPtInGui(true, false);
	}
	
	if( plug.disableGuiUpdates==false && g->countingStarted && g->allowRandomPts() )
	{
		if( spnSelPt->value() != g->currPtIdx+1 )
		{
			plug.disableGuiUpdates = true;
			spnSelPt->setValue( g->currPtIdx+1 );
			plug.disableGuiUpdates = false;
		}
	}
	
}

//------------------------
//-- Updates the elements in the "Classify Points" tab to reflect
//-- the values of the currently selected point. If "jumptToPt"
//-- is on, the ZAP jumpts to this point, and if "updateSpinner"
//-- is on, the value of "spnSelPt" is updated to match.

bool Stereology::updateCurrPtInGui( bool jumpToPt, bool updateSpinner )
{
	GridSetObj *g = getCurrGridSetObj();
	
	long psize = (g->countingStarted) ? g->ptsize() : g->maxPts;
	
	
	if(g->currPtIdx < 0)					g->currPtIdx = psize;
	if(g->currPtIdx >= psize)			g->currPtIdx = 0;
	
	if(psize == 0)
	{
		lblMaxPts->setText( "<b> N/A </b>" );
		return false;
	}
	
	Ipoint pt = g->getPos( g->currPtIdx );
	
	if(jumpToPt)
	{
		edit_setZapLocation( pt.x, pt.y, pt.z, true );
		if(plug.centerOnEachPt)
			ivwSetTopZapCenter( plug.view, (float)pt.x, (float)pt.y, pt.z, true );
	}
	
	if(updateSpinner)
		spnSelPt->setValue( g->currPtIdx+1 );
	
	lblMaxPts->setText( "/ " + QStr(psize) );
	lblSelGrid->setText( "Grid:  " + QStr(g->currGrid()+1) + " / " + QStr(g->grids) );
	
	if( g->countingStarted )
		updateCatsSelectedGui();
	
	
	drawSelObject( !plug.blackVisitedPts );
	
	if( plug.blackVisitedPts )
		drawBlackObject(true);
}


//------------------------
//-- Updates the category toggle buttons to reflect the values
//-- of the currently selected point.

bool Stereology::updateCatsSelectedGui()
{
	GridSetObj *g = getCurrGridSetObj();
	Spoint *spt = g->getCurrSPt();
	for( int i=0; i<spt->catSel.size() && i<lineItem.size(); i++ )
		lineItem[i].btnCat->setChecked( spt->isCatOn(i) );
	chkPtChecked->setChecked( spt->checked );
}






//------------------------
//-- Change "plug.paintRadius" by "value" and keep between sensible limits.
//-- NOTE that if "slowDown" is true value is reduced to 1/5 it's value.

void Stereology::changePtPaintRadius( float value, bool slowDown )
{
  if(slowDown)
    value *= 0.2;
  
  plug.paintRadius += value;            // linear
	
	if( plug.paintRadius < 3.0f    ) plug.paintRadius = 3.0f;
	if( plug.paintRadius > 2000.0f ) plug.paintRadius = 2000.0f;
}


//------------------------
//-- Callback for when one of the color buttons in the "Classify Points"
//-- is clicked and a new color set. First this function identifies
//-- which button was clicked, updates the color for that cateogry and
//-- then, if this category matches an existing IMOD object, the
//-- color of the object is updated to match.

void Stereology::changeColorBtn()
{
  GridSetObj *g = getCurrGridSetObj();
	
	if( g->catObj.size() != lineItem.size() )		// if sizes don't match: exit early
		return;
	
	//## DETERMINE THE INDEX OF THE COLOR BUTTON PRESSED:
	
	int  btnIdx = -1;						// will be set to the index of the button clicked
	
	for (int i=0; i<(int)lineItem.size(); i++)
		if( QObject::sender() == lineItem[i].btnColor )
		{
			btnIdx = i;
			break;
		}
	if(btnIdx == -1) 		// if couldn't find valid index: exit early
		return;
			
	//## DOUBLE CHECK THIS BUTTON MATCHES A VALID CATEGORY:
	
	if( btnIdx < 0 || btnIdx >= g->catObj.size() )		// double check this category exists
		return;
	CategoryObj &catObj = g->catObj[btnIdx];
	
	//## UPDATE THE COLOR OF THE CATEGORY AND, IF A MATCHING OBJECT EXISTS,
	//## UPDATE THE COLOR OF THIS OBJECT TOO:
	
	if( catObj.color != lineItem[btnIdx].btnColor->getColor() )
	{
		catObj.color = lineItem[btnIdx].btnColor->getColor();
		
		if( g->countingStarted && isCatObjValid(btnIdx, false) )
		{
			Imod *imod  = ivwGetModel(plug.view);
			Iobj *obj   = getObj(imod, catObj.objIdx);
			setObjColor( obj, catObj.color.red(), catObj.color.green(), catObj.color.blue() );
			ivwDraw( plug.view, IMOD_DRAW_MOD );
		}
	}
}


//------------------------
//-- Called when the space bar is clicked, and is used to toggle
//-- the value of "chkPtChecked".

void Stereology::togglePtChecked()
{
	if( getCurrGridSetObj()->countingStarted )
	{
		chkPtChecked->setChecked( !chkPtChecked->isChecked() );
		changePtCheckedClicked();
	}
}


//------------------------
//-- Called when "chkPtChecked" is clicked - updates the value
//-- of the currently selected point to match.

void Stereology::changePtCheckedClicked()
{
  GridSetObj *g = getCurrGridSetObj();
	Spoint *spt = g->getCurrSPt();
	if(spt==NULL)	return;
	changePtCheckedAndUpdateObj( spt, chkPtChecked->isChecked() );
	drawBlackObject( true );
}


//------------------------
//-- Called when one of the shortcut keys [1]-[9] is clicked.
//-- If "plug.paintMode" is off (PM_OFF), then the matching category
//-- is toggled on/off for the currently selected point.
//-- If "plug.paintMode" is PM_PAINT, then the matching category
//-- is turned on for all the points inside the paint circle.

void Stereology::toggleCategory( int catIdx )
{
	if( tabWidget->currentIndex() == 0 || spnSelPt->hasFocus() )
		return;
	
	
  GridSetObj *g = getCurrGridSetObj();
	
	if( catIdx >= g->catObj.size() )
		return;
	
	
	//## IF IN "PT PAINT" MODE THEN TURN ON THIS CATEGORY TO 
	//## ALL POINTS INSIDE THE PAINT CIRCLE:
	
	if( plug.paintMode==PM_PAINT )
	{
		if( g->isGridOnSlice( edit_getZOfTopZap() ) )
		{
			int numPtsChanged = 0;
			int currPtChanged = false;
			
			vector<long> ptIdxs = g->getIdxPtsInRadius( &plug.mouse, plug.paintRadius );
			
			for(int i=0; i<(int)ptIdxs.size(); i++)
			{
				long ptIdx = ptIdxs[i];
				Spoint *spt = g->getSPt( ptIdx );
				bool catOn = spt->isCatOn( catIdx );
				if( catOn==false )
				{
					updatePtCat( ptIdx, catIdx, true, false, false );	
											// turn on but don't redraw or autoprogress
					numPtsChanged++;
					if( ptIdx==g->currPtIdx )	currPtChanged = true;
				}
			}
			
			if(numPtsChanged)
			{	
				drawBlackObject(true);			// includes a redraw all
				if(currPtChanged)
					updateCatsSelectedGui();
			}
		}
	}
	
	//## IF IN "INTERCEPTS" MODE THEN UPDATE THE VALUE OF 
	//## "plug.interceptCat" AND ADD POINT (IF APPROPRIATE):
	
	if( plug.paintMode==PM_INTERCEPTS )
	{
		plug.interceptCat = catIdx;
		
		int objIdx = g->getCatObjIdx(catIdx);
		if( isStereologyObj(objIdx) )
		{
			Imod *imod  = ivwGetModel(plug.view);
			imodSetIndex(imod, objIdx, 0, -1);
			executeIntercept();
		}
	}
	
	
	//## IF IN NORMAL MODE THEN TOGGLE THIS CATEGORY FOR 
	//## THE CURRENTLY SELECTED STEREOLOGY POINT:
	
	if( plug.paintMode==PM_OFF )
	{
		Spoint *spt = g->getCurrSPt();
		if(spt==NULL)	return;
		bool catOn = spt->isCatOn( catIdx );
		updatePtCat( g->currPtIdx, catIdx, !catOn, true );
	}
}



//------------------------
//-- Called when one of the category toggle button is clicked.
//-- First it needs to identify which button was clicked, and
//-- then the relevant category is turned on or off.

void Stereology::catToggleBtnPushed()
{
  GridSetObj *g = getCurrGridSetObj();
	
	//## CHECK THE NUMBER OF LINE ITEMS AND CATEGORIES MATCH:
	
	if( lineItem.size() != g->catObj.size() )
	{
		cerr << "ERROR: toggleButtonPushed() - # line items don't match categories" << endl;
		return;
	}
	
	//## ACCESS CORRESPONDING "Spoint" AND MAKE SURE IT'S INITALIZED WITH
	//## THE RIGHT NUMBER OF CATEGORIES:
	
	Spoint *spt = g->getCurrSPt();
	
	if(spt == NULL)
	{
		QMessageBox::warning( this, "ERROR", "ERROR: This point not setup yet!");
		return;
	}
	if( spt->catSel.size() != g->catObj.size() )
	{
		spt->catSel.resize( g->catObj.size() );
	}
	
	//## GO THROUGH LINE ITEMS TO DETERMINE WHICH WAS CHANGED:
	
	int catIdxChanged = -1;
	bool catOn = false;
	
	for (int i=0; i<lineItem.size() && i<spt->catSel.size(); i++)
	{
		bool isCatBtnChecked = lineItem[i].btnCat->isChecked();
		if( isCatBtnChecked != spt->catSel[i] )
		{
			catIdxChanged = i;
			catOn    = isCatBtnChecked;
			break;
		}
	}
	
	if( catIdxChanged == -1 )		// if no change found (which shouldn't happen): 
		return;												// do nothing
	
	
	Imod *imod  = ivwGetModel(plug.view);
	
	updatePtCat( g->currPtIdx, catIdxChanged, catOn );
}


//------------------------
//-- Used to keep track of the number of minutes the user has worked
//-- by updating the value of "plug.numMinutesWorked" and reseting
//-- "plug.timeLastClick". The value of "plug.numMinutesWorked" is only 
//-- increased (by the appropriate fraction) if less than a minute
//-- has elapsed since the last time this function was called - meaning
//-- that if you click once then two minutes later, no change occurs.

void Stereology::updateTimeWorked()
{
	if( plug.timeLastClick.isNull() )
	{
		plug.timeLastClick.start();
		return;
	}
	
	int msecsElapsed = plug.timeLastClick.elapsed();
	if( msecsElapsed <= 60000 )
		plug.numMinutesWorked += ((float)msecsElapsed / (60.0f*1000.0f));
	
	plug.timeLastClick.restart();	
}


//------------------------
//-- Called whenever the category ("catIdx") of a point, specified
//-- by "ptIdx" is to be turned on or off ("turnOn").
//-- 
//-- This function contains a certain amoung of logic: if this
//-- category is turned on and "allowMultipleCats" is off it will 
//-- turn off the value of any other categories, set the point to
//-- checked and, if "plug.autoProgress" is on, advances to the
//-- next point. For any categories turned off or on, contours points 
//-- are added or removed by calling "changePtCatAndUpdateObj()".

bool Stereology::updatePtCat( long ptIdx, int catIdx, bool turnOn, 
														  bool redraw, bool updateGui )
{
	GridSetObj *g = getCurrGridSetObj();
	
	//## GET CORRESPONDING POINT AND CHECK IF CHANGE NEEDED:
	
	if( !g->doesPtCatExists(ptIdx,catIdx) )
		return false;
	
	Spoint *spt = g->getSPt( ptIdx );				// get the stereology point we want to change
	if( spt->isCatOn(catIdx) == turnOn )			// if no change needed: early exit
		return true;
	
	
	updateTimeWorked();		// update the number of minutes worked
	
	//## IF TURNED ON AND CATEGORIES ARE MUTUALLY EXCLUSIVE:
	//## SET POINT AS CHECKED AND TURN OFF ANY OTHER CATEGORIES:
	
	if( turnOn && g->allowMultipleCats==false )
	{
		changePtCheckedAndUpdateObj( spt, true );
		//spt->checked = true;
		
		for(int c=0; c<(int)spt->catSel.size(); c++ )
			if( c!=catIdx && spt->isCatOn( c ) )
				changePtCatAndUpdateObj( spt, c, false );
	}
	
	
	//## IF TURNED OFF, SET POINT AS UNCHECKED IF ALL OTHER VALUES ARE OFF:
	
	if( !turnOn )
	{
		bool onValFound = false;
		for(int c=0; c<(int)spt->catSel.size(); c++ )
			if( spt->isCatOn( c ) )
				onValFound = true;
		
		if( !onValFound )
			changePtCheckedAndUpdateObj( spt, false );
		
		//spt->checked = false;
	}
	
	//## UPDATE THE VALUE OF THIS POINT:
	
	changePtCatAndUpdateObj( spt, catIdx, turnOn );
	
	
	//## IF AUTOPROGRESS ON AND CHECKED: GO TO NEXT POINT:
	
	if( updateGui )
	{
		if( plug.autoProgress && g->allowMultipleCats==false && spt->checked )
		{
			goToNextPt();
			return true;
		}
		
		updateCatsSelectedGui();
	}
	
	if( redraw )
		ivwDraw( plug.view, IMOD_DRAW_ALL );
	
	return true;
}



//------------------------
//-- Changes the selected point ("spt"), to turn the selected category
//-- ("catIdx") to on or off ("turnOn") without any logic, except that
//-- it will also either add (if "turnOn" is true) or else remove the
//-- matching contour point from the matching category's IMOD object (assuming
//-- the category in the current grid has a matching IMOD object).

bool Stereology::changePtCatAndUpdateObj( Spoint *spt, int catIdx, bool turnOn )
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( !isCatObjValid(catIdx) )
		return false;
	
	if( spt->isCatOn(catIdx) == turnOn )			// if no change needed: early exit
		return true;
	
	spt->setCatOn(catIdx, turnOn);						// turn this category on or off
	
	int objIdx = g->getCatObjIdx(catIdx);
	return addOrRemoveSinglePtFromObj( objIdx, spt->pos, turnOn );
}


//------------------------
//-- Used to changed the value of "checked" for the specified point
//-- "spt" in the currently selected grid to "turnCheckedOn". If the
//-- value already matches, it returns true and no changed is made.
//-- If the value needs changing, a point is either added or removed
//-- from the matching IMOD "STEREOLOGY.GRID" object. Returns
//-- true if the change was made successfully, or else false.

bool Stereology::changePtCheckedAndUpdateObj( Spoint *spt, bool turnCheckedOn )
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( spt->checked == turnCheckedOn )			// if no change needed: early exit
		return true;
	
	spt->checked = turnCheckedOn;						// change checked value
	
	return addOrRemoveSinglePtFromObj( g->objIdx, spt->pos, turnCheckedOn );
				// if "turnCheckedOn" is true: point will be added to end of first contour
				// if false: removes first occurance of point in object
}


//------------------------
//-- Used to add or remove a point ("newPt") from the specified
//-- object (at index "objIdx") in the current IMOD model.
//-- If "turnOn" is true, the point will be added to the very end
//-- of the first contour in the object.
//-- If "turnOn" is false, this algorithm will search through every
//-- contour and every point and removes the first point which
//-- matches the point position specified. NOTE: Contours are 
//-- searched from first to last, but points in each contour are
//-- searched from last to first.
//-- 
//-- Returns false if the specified object does not exist, or
//-- true if it does exist (and thus a point was either added,
//-- removed, or in some cases a matching point may not exist,
//-- thus no change made to the object, but it will still
//-- return true).

bool Stereology::addOrRemoveSinglePtFromObj( int objIdx, Ipoint newPt, bool turnOn )
{
	Imod *imod    = ivwGetModel(plug.view);
	if( objIdx < 0 || objIdx >= osize(imod) )		// if object idx is invalid: return false
		return false;
	
	Iobj *obj = getObj(imod, objIdx);
	
	//## IF WE WANT TO TURN ON: ADD THE POINT TO THE END OF THE FIRST CONTOUR:
	//## ELSE REMOVE THE FIRST MATCHING POINT FROM THE OBJECT:
	
	if( turnOn )		// if we want to turn on: add point to first contour
	{
		Icont *cont = getFirstCont(obj,true);				// gets the first contour
																								//  (and creates it if necessary)
		imodPointAppend( cont, &newPt );						// add point to end
	}
	else						// else: find and delete first matching point in any contour
	{
		for(int c=0; c<csize(obj); c++)			// for each contour:
		{
			Icont *cont = getCont(obj,c);
			for(long p=psize(cont)-1; p>=0; p-- )		// search points from end:
			{
				Ipoint *pt = getPt(cont,p);
				if( ptsEqualXYZ( &newPt,pt,PT_PREC ) )
				{
					imodPointDelete( cont, p );
					return true;
				}
			}
		}
	}
	
	return true;
}


//------------------------
//-- Returns true if the specified category ("catIdx") in the 
//-- currently selected grid is attached to a valid IMOD object,  
//-- as determined by a valid "objIdx" value. If the "catIdx"
//-- or "objIdx" is invalid and "printError" is on, the error
//-- is printed to the terminal console.

bool Stereology::isCatObjValid( int catIdx, bool printError )
{
	Imod *imod    = ivwGetModel(plug.view);
	GridSetObj *g = getCurrGridSetObj();
	
	if( catIdx < 0 || catIdx >= g->catObj.size() )		// check catIdx valid
	{
		if(printError)
			cerr << "ERROR: isCatObjValid() - category does not exist" << endl;
		return false;
	}
	
	int objIdx = g->getCatObjIdx(catIdx);
	
	if( objIdx < 0 || objIdx >= osize(imod) )
	{
		if(printError)
			cerr << "ERROR: isCatObjValid() - matching object does not exist" << endl;
		return false;
	}
	return true;
}



//------------------------
//-- Return true if the object at the specified index "objIdx" 
//-- is a valid object and contains the word "STEREOLOGY" in its
//-- label and/or object name, and thus should represent a
//-- "STEREOLOGY" object - either a category  or a grid
//-- setting object.

bool Stereology::isStereologyObj( int objIdx )
{
	Imod *imod    = ivwGetModel(plug.view);
	if( objIdx < 0 || objIdx >= osize(imod) )
		return false;
	
	Iobj       *obj = getObj(imod, objIdx);
	QString objName = (QString)imodObjectGetName(obj);
	QString objLabel = getObjLabel(obj);
	
	return ( objLabel.contains( "STEREOLOGY" ) || objName.contains( "STEREOLOGY" )  );
}

//------------------------
//-- Returns true if the currently selected IMOD object
//-- contains the word "STEREOLOGY" (in either its label or
//-- object name) - thus should represent a stereology object,
//-- or false if it doesn't.
//-- 
//-- If "showErrorIfTrue" is true and the object IS a stereology
//-- object, it will also display a popup message showing the
//-- text in "customMsg"... or will show a default message
//-- "Never try to directly modify a 'STEREOLOGY' object"
//-- if no "customMsg" is given.

bool Stereology::isStereologyObjSelected( bool showErrorIfTrue, const char *customMsg )
{
	Imod *imod    = ivwGetModel(plug.view);
	int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	static bool popupShown = false;
	
	if( isStereologyObj( objIdx ) )
	{
		if(showErrorIfTrue)		
		{
			if(!popupShown)
			{
				QMessageBox::warning( this, "WARNING",
															"Never try to directly modify a 'STEREOLOGY' object");
				popupShown = true;
			}
			
			if( customMsg==NULL )
				wprint( "\a'STEREOLOGY' contours should not be modified\n" );
			else
				wprint( customMsg );
		}
		return true;
	}
	
	return false;
}

//------------------------
//-- Jumps to the first invalid point in the current grid.
//-- Returns true there is an invalid point, or returns 
//-- false and shows a message if all points are valid.

bool Stereology::goToFirstInvalidPoint()
{
	//## SEARCH FOR BAD POINTS:
	
	GridSetObj *g = getCurrGridSetObj();
	long firstBadPtIdx = -1;
	QString badPtsStr;
	int numCheckedPtsNoCat;
	int numPtsMultiCat;
	bool pointsValid = g->validatePtValues( &badPtsStr, &firstBadPtIdx, 
																				 &numCheckedPtsNoCat, &numPtsMultiCat );
	
	//## IF POINTS INVALID POINTS FOUND: SELECT AND JUMP TO THE FIRST OF THEM
	
	if( firstBadPtIdx != -1 )
	{
		g->setCurrPt( firstBadPtIdx, false );
		updateCurrPtInGui( true, true );
		return true;
	}
	else
	{
		MsgBox( this, "...", "All points appear valid" );
		return false;
	}
}




//------------------------
//-- Toggles the "Pt Painter" tool, by changing the value
//-- of "plug.paintMode" between PM_PAINT and PM_OFF.

void Stereology::togglePaintBtn()
{
	plug.paintMode = ( btnPaint->isChecked() ) ? PM_PAINT : PM_OFF;
	btnIntercept->setChecked( false );
}

//------------------------
//-- Toggles the "Pt Painter" tool, by changing the value
//-- of "plug.paintMode" between PM_INTERCEPT and PM_OFF.

void Stereology::toggleInterceptBtn()
{
	plug.paintMode = ( btnIntercept->isChecked() ) ? PM_INTERCEPTS : PM_OFF;
	btnPaint->setChecked( false );
}


//------------------------
//-- Used to "paint" all stereology points within the "paint circle"
//-- as the same value as the currently selected point.
//-- This function is called when "Pt Painter" is on ("plug.paintMode"  
//-- is PM_PAINT) and the user holds down the second mouse button.

void Stereology::executePaint()
{
  GridSetObj *g = getCurrGridSetObj();
	
	if( !g->countingStarted )
		return;
	
	Spoint *currSPt = g->getCurrSPt();
	
	if( currSPt==NULL )
		return;
	
	
	
	if( g->isGridOnSlice( edit_getZOfTopZap() ) )
	{
		int numPtsChanged = 0;
		
		vector<long> ptIdxs = g->getIdxPtsInRadius( &plug.mouse, plug.paintRadius );
		
		for(int i=0; i<(int)ptIdxs.size(); i++)
		{
			long ptIdx = ptIdxs[i];
			Spoint *spt = g->getSPt( ptIdx );
			if( ptIdx!=g->currPtIdx )
			{
				int ptChanged = 0;			// is changed to 1 if point is changed
				for(int c=0; c<currSPt->catSel.size() && c<spt->catSel.size(); c++)
				{
					if( spt->isCatOn(c) != currSPt->isCatOn(c) )
					{
						changePtCatAndUpdateObj( spt, c, currSPt->isCatOn(c) );
						ptChanged = 1;
					}
				}
				if( spt->checked != currSPt->checked )
				{
					spt->checked = currSPt->checked;
					ptChanged = 1;
				}
				numPtsChanged += ptChanged;
			}
		}
		
		if(numPtsChanged)
		{	
			drawBlackObject(true);			// includes a redraw all
		}
	}
}


	
	
//------------------------
//-- Used to reset all the values of any stereology points within
//-- the point circle by turning "checked" all "catSel" values to false.
//-- This function is called when "Pt Painter" is on ("plug.paintMode"  
//-- is PM_PAINT) and the user holds down the third mouse button.

void Stereology::resetPaint()
{
  GridSetObj *g = getCurrGridSetObj();
	
	if( !g->countingStarted )
		return;
		
	if( g->isGridOnSlice( edit_getZOfTopZap() ) )
	{
		int numPtsChanged = 0;
		int currPtChanged = false;
		
		vector<long> ptIdxs = g->getIdxPtsInRadius( &plug.mouse, plug.paintRadius );
		
		for(int i=0; i<(int)ptIdxs.size(); i++)
		{
			long ptIdx  = ptIdxs[i];
			Spoint *spt = g->getSPt( ptIdx );
			
			int ptChanged = 0;			// is changed to 1 if point is changed
			for(int c=0;c<spt->catSel.size(); c++)
			{
				if( spt->isCatOn(c) )
				{
					changePtCatAndUpdateObj( spt, c, false );
					ptChanged = 1;
				}
			}
			if( spt->checked == true )
			{
				spt->checked = false;
				ptChanged = 1;
			}
			numPtsChanged += ptChanged;
		}
		
		if(numPtsChanged)
		{	
			drawBlackObject(true);			// includes a redraw all
			if(currPtChanged)
				updateCatsSelectedGui();
		}
	}
}


//------------------------
//-- Used to add an intercept point somewhere along the nearest line segment.
//-- 
//-- This function is called when "plug.paintMode"==PM_INTERCEPT
//-- and the user presses down the second mouse button.

void Stereology::executeIntercept()
{
  GridSetObj *g = getCurrGridSetObj();
	
	if( !g->countingStarted )
		return;
	
	if( plug.interceptCat < 0 || plug.interceptCat >= (int)g->catObj.size() )
	{
		QMessageBox::warning( this, "...",
												  "Press a number key matching the category you "
												  "wish to create intercept points for" );
		return;
	}
	
	int objIdx = g->getCatObjIdx(plug.interceptCat);
	if( !isStereologyObj(objIdx) )
	{
		QMessageBox::critical( this, "...",
										 		   "The category you selected does not seem to have "
												   "a matching object" );
		return;
	}
	
	//## FIND NEAREST INTERCEPT POINT ON THE LINE
	//## AND EXIT EARLY IF NO POINT IS IN RANGE:
	
	Ipoint pt;
	bool interceptFound = g->getInterceptNearPt( plug.mouse, &pt, INTERCEPT_SNAP );
	
	if( !interceptFound )		// if no intercept found near mouse: exit early
		return;
	
	//## IF INTERCEPT IS FOUND ADD IT TO THE FIRST CONTOUR:
	
	addOrRemoveSinglePtFromObj( objIdx, pt, false );		// if point was already here
																											//  delete it...
	addOrRemoveSinglePtFromObj( objIdx, pt, true  );		// add point to this spot
	
}

//------------------------
//-- Used to delete points added using "executeIntercept()".
//-- This function is called when "plug.paintMode"==PM_INTERCEPT 
//-- and the user holds down the third mouse button.

void Stereology::deleteIntercept()
{
  GridSetObj *g = getCurrGridSetObj();
	
	if( !g->countingStarted )
		return;
	
	//## FIND NEAREST INTERCEPT POINT ON THE LINE
	//## AND EXIT EARLY IF NO POINT IS IN RANGE:
	
	Ipoint pt;
	bool interceptFound = g->getInterceptNearPt( plug.mouse, &pt, INTERCEPT_SNAP );
	
	if( !interceptFound )		// if no intercept found near mouse: exit early
		return;
	
	//## FOR EACH STEREOLOGY CATEGORY OBJECT IN GRID,
	//## DELETE ANY POINTS AT THIS POINT:
	
	for( int c=0; c<(int)g->catObj.size(); c++ )
	{
		int objIdx = g->getCatObjIdx(c);
		if( isStereologyObj(objIdx) )
			addOrRemoveSinglePtFromObj( objIdx, pt, false );
	}
	
}






//############################################################
//## PLUGIN SETTINGS AND HELP METHODS:



//------------------------
//-- Brings up a dialog which allows the user to view and set the x, y and z 
//-- spacing of gridlines using the metric units units set in the model's header. 
//-- After the user clicks okay, any changes are converted back to pixels by
//-- using the pixel size. If the pixel size and/or units are not setup, a warning
//-- is displayed to the user.... and if the grid is already setup the user can
//-- view the spacing in metric units, but not modify the values.

void Stereology::setSpacingUsingUnits()
{
	//## GET PIXEL SIZE AND WORK OUT IF UNITS ARE SETUP:
	
	GridSetObj *g = getCurrGridSetObj();
	
	Imod *imod  = ivwGetModel(plug.view);
	float pixelSize = imodGetPixelSize(imod);
	char *unitsChs = imodUnits(imod);
	QString unitsStr = unitsChs;
	Ipoint scalePt;
	setPt( &scalePt, 1,1, imodGetZScale(imod) );
	
	bool unitsNotSetup = (unitsStr == "pixels" || unitsStr == "" || pixelSize == 1.0f);
	
	static bool changeXUnits = false;
	static bool changeYUnits = false;
	static bool changeZUnits = false;
	
	if( g->countingStarted )
	{
		changeXUnits = false;
		changeYUnits = false;
		changeZUnits = false;
	}
	
	//## CALCULATE SPACING IN UNITS AND SHOW ERROR IF ANY BAD VALUES:
	
	float xUnits = g->xSpacing * pixelSize;
	float yUnits = g->ySpacing * pixelSize;
	float zUnits = g->zSpacing * pixelSize * scalePt.z;
	
	if( pixelSize <= 0 || scalePt.z <= 0 || xUnits <= 0 || yUnits <= 0 || zUnits <= 0 )
	{
		QMessageBox::critical(this, "ERROR", "Bad unit values detected... \n"
																"Please go to 'Edit > Model > Header' and fix this");
		return;
	}
	
	
	//## GET USER INPUT VIA A CUSTOM DIALOG:
	
	CustomDialog ds("Change Grid Spacing Using Units", this);
	
	ds.addLabel   ( "--- SELECT UNITS TO CHANGE: ---", true );
	
	if( unitsNotSetup )			// if units don't appear setup: display warning message
	{
		ds.addHtmlLabel ( "<b>WARNING</b>: Pixel size and/or units do not appear to <br>"
											" &nbsp;  be setup yet. Go to 'Edit > Model > Header' to fix this.",
											"All MRC files *SHOULD* have a pixel size and metric \n"
											"units (eg: 'angstrom' or 'nm') in their header \n"
											"(use the 'header your.mrc' command to see these) and \n"
											"you should copy these units into the 'units' field under \n"
											"'Menubar > Edit > Model > Header'. Without this you can only\n"
											"measure distances in 'pixels' and not real world distances." );
		ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	}
	if( g->countingStarted )
	{
		ds.addHtmlLabel ( "<b>NOTE</b>: Since this grid is already finalized,<br>"
										  " &nbsp; &nbsp; none of these values can be modified." );
		ds.setStylePrev("background-color: rgb(255, 230, 110);");			// light yellow
	}
	
	ds.addLineEditF( "", 0.01f, 100000.0f, &xUnits, 6,
									"The spacing between lines in x in real world distance.\n"
									"This value will be converted to pixels (or as close as possible"
									"with two decimal points).", " " + unitsStr);
	
	ds.addCheckPrev("change X spacing to:", &changeXUnits, CB_NONE, true, 
									"If true, this value will be converted to a \n"
									"pixel value (or as close as possible) representing \n"
									"the spacing between vertical lines." );
	
	ds.addLineEditF( "", 0.01f, 100000.0f, &yUnits, 6,
									"The spacing between lines in x in real world distance.\n"
									"This value will be converted to pixels (or as close as possible"
									"with two decimal points).", " " + unitsStr);
	
	ds.addCheckPrev( "change Y spacing to:", &changeYUnits, CB_NONE, true, 
								 "If true, this value will be converted to a \n"
								 "pixel value (or as close as possible) representing \n"
								 "the spacing between horizontal lines.");
	
	ds.addLineEditF( "", 0.01f, 100000.0f, &zUnits, 6,
									"The spacing between grids in Z in real world distance.\n"
									"This value will be rounded down to the nearest pixels and\n"
									"must be at least one pixel.", " " + unitsStr);
	
	ds.addCheckPrev( "change Z spacing to:", &changeZUnits, CB_NONE, true, 
								 "If true, this value will be rounded down to the nearest \n"
								 "pixel value and spacing between grid in Z updated. \n"
								 "Note that grids must be spaced at LEAST 1 pixel apart and\n"
								 "this unit distance in affected not just by 'Pixel Size'\n"
								 "but also by 'Z scale' (under 'Edit > Model > Header')");
	
	ds.setEnabledAll( !g->countingStarted );		// disable everything if grid finalized
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	if( g->countingStarted )
		return;
	
	//## VALIDATE USER INPUT:
	
	if( xUnits <= 0 || yUnits <= 0 || zUnits <= 0 )
	{
		QMessageBox::warning(this, "BAD INPUT", "WARNING: Some of your input values\n"
												                    "   were negative or not numbers, so will\n"
												                    "   be ignored.");
		return;
	}
	if( !changeXUnits && !changeYUnits && !changeZUnits && !g->countingStarted )
	{
		QMessageBox::warning(this, "NO INPUT", "No changes made, as you didn't\n"
												                   "tick any of the checkboxes.");
	}
	
	//## CHANGE SPACING AS REQUESTED:
	
	if( changeXUnits )	spnSpacingX->setValue(MAX(xUnits / pixelSize, 0.01f));
	if( changeYUnits )	spnSpacingY->setValue(MAX(yUnits / pixelSize, 0.01f));
	if( changeZUnits )	spnSpacingZ->setValue(MAX((int)(zUnits/(pixelSize*scalePt.z)),1));
	
	if( changeXUnits || changeYUnits || changeZUnits )
		updateGridFromGridGui( true, true, plug.jumpToGridOnUpdate );
}



//------------------------
//-- Shows a dialog where the user can choose one or more objects with
//-- closed contours to serve as a mask. The "MASK TO APPLY" options
//-- also allow the user to chose wether the mask should be inverted
//-- such that ONLY objects outside these contours are selected.... and
//-- wether to apply the mask over multiple sections/grids.
//-- The user also specifies "CHANGE TO MAKE", so that selected
//-- point can become checked/unchecked and or have certain categories
//-- turned on or off. Once the user clicks okay, each point is the grid
//-- is checked against criteria and changed if needed and after changes 
//-- are applied a message box says how many points have been changed.
//-- This function does not support undo, and the user is warned such.

void Stereology::applyMask()
{
	GridSetObj *g = getCurrGridSetObj();
	int nCatObjs  = (int)g->catObj.size();
	
	//## IF POINT COUNTING NOT STARTED: EXIT EARLY:
	
	if( g->countingStarted==false )
	{
		MsgBox( this, "...",
					 "Sorry - you can't apply a mask to points until you have finalized "
					 "categories and started counting/categorizing points!");
		return;
	}
	
	
	//## IF USER HAS NOT SELECTED A CLOSED CONTOUR OBJECT: EXIT
	
	Imod *imod      = ivwGetModel(plug.view);
	Iobj *obj       = imodObjectGet(imod);		// currently selected object
	
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	if( isStereologyObj(objIdx) || !isObjClosed(obj) )
	{
		MsgBox( this, "...",
					 "To apply a mask you must first select a 'non-stereology' object "
					 "with closed contours" );
		return;
	}
	
	
	
	//## GET TO SELECT CONTOUR INPUT AND CATEGORIES TO APPLY VIA A CUSTOM DIALOG:
	
	int  currZ = edit_getZOfTopZap();
	if( currZ == -1 )
		currZ = 0;
		
	CustomDialog ds("Apply Mask", this);
	
	ds.addLabel   ( "--- MASK TO APPLY ---", true );
	
	ds.addComboBox( "using contours from:",
									"object " + QStr(objIdx+1) + " only|"
									"all closed objects|"
									"let me enter a range", &plug.maskObjOpt,
									"Specifies which object(s) you want to use to create the mask. \n"
								  "If you select 'let me enter a range' you can enter a range in \n"
								  "the next window. Any objects which are not closed objects \n"
								  "or are 'STEREOLOGY' objects will automatically be skipped." );
	
	ds.addRadioGrp( "select points:",
								  "INSIDE closed contours|"
								  "OUTSIDE closed contours", &plug.maskInverseOpt,
								  "The first option means only points inside contours will be changed.\n"
								  "The secton option 'inverses' this mask and means only stereology \n"
								  "points outside closed contours will be changed. Note also that \n"
									"any open contours will get ignored (no used in the masked). \n\n"
									"WARNING: If a grid has no closed contours and you have ticked \n"
									"'OUSIDE' then ALL the points on this grid are inside the mask!" );
	
	ds.addComboBox( "and apply over:",
								  "section " + QStr(currZ+1) + " only|"
								  "all sections (all grids)|"
								  "let me enter a range", &plug.maskSectionsOpt,
								  "Specifies which sections you want to to change. \n"
								  "By default it will change only the current grid. \n"
								  "If you select 'let me enter a range' you can enter \n"
								  "a range of sections in the next window." );
	
	ds.addComboBox( "and change only:",
								  "all points (in mask)|"
								  "checked points|"
								  "unchecked points", &plug.maskSelPtsOpt,
								  "By default, this is set to 'unchecked points', meaning that \n"
								  "any points you have already checked will be unaltered. \n"
								  "In some rare cases however, you can opt that only checked points \n"
								  "or both checked and unchecked points are selected for change." );
	
	
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "--- CHANGE TO MAKE: ---", true );
	
	ds.addLabel   ( "for points selected by this mask:" );
	
	ds.addComboBox( "set these points as:",
								 "don't change|"
								 "checked|"
								 "unchecked|"
								 "toggle", &plug.maskChangeToCheckedOpt,
								 "All selected points inside the mask will be changed to show \n"
								 "this value" );
	
	ds.beginGroupBox( "and set these categories:", false,
									  "Select below which categories you would like to \n"
									  "turn on or off for any selected stereology points \n\n"
									  "NOTE: Unless you have enabled multiple points per grid \n"
									  "  you should only select ONE object to modify at a time." );
	ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
	for( int c=0; c<nCatObjs; c++ )
	{
		CategoryObj *catObj = &g->catObj[c];
		QString catName = "[" + QStr(c) + "] " + catObj->categoryName;
		ds.addComboBox( catName,
									 "-|"
									 "turn ON|"
									 "turn off|"
									 "toggle", &catObj->changeOpt,
									 "All selected points inside the mask will be changed to show \n"
									 "this value" );
	}
	ds.endGroupBox();
	
	
	ds.addHtmlLabel ( "<b>WARNING</b>: You cannot undo this operation! <br>"
									 "It's recommended you save before you hit okay.",
									 "Due to the way this STEREOLOGY plugin has been fitted to IMOD \n"
									 "(a program not originally designed for stereology), it was \n"
									 "difficult to include any undo functionality, therefore we \n"
									 "suggest you hit save before any major changes... and if you \n"
									 "need to revert then you can try to reload this model." );
	ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	
	ds.setMinimumWidth( 200 );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	
	//## IF SPECIFIED: ALLOW USER TO SET RANGE FOR OBJECTS AND/OR SECTIONS:
	
	static int minObjRange = objIdx+1;
	static int maxObjRange = objIdx+1;
	
	static int minZRange = 1;
	static int maxZRange = plug.zsize;
	
	
	if( plug.maskObjOpt == RG_CUSTOM || plug.maskSectionsOpt == RG_CUSTOM )
	{
		if( minZRange > maxZRange )												  minZRange = maxZRange;
		if( maxObjRange > osize(imod) || maxObjRange < 1 )	maxObjRange = osize(imod);
		if( minObjRange > maxObjRange || minObjRange < 1 )	minObjRange = maxObjRange;
		
		CustomDialog dsR("Set Mask Range", this);
		
		if( plug.maskObjOpt == RG_CUSTOM )
		{
			dsR.addLabel( "" );
			dsR.addLabel( "Use only closed contours in these objects:", true );
			
			dsR.addMinMaxSpinBoxPair( "> objects: ", " - ", 1, osize(imod),
															  &minObjRange, &maxObjRange, 1,
															  "And CLOSED contours in this range will be used \n"
															  "to generate the mask to change stereology points" );
		}
		
		if( plug.maskSectionsOpt == RG_CUSTOM )
		{
			dsR.addLabel( "" );
			dsR.addLabel( "Only change grids between these sections:", true );
			
			dsR.addMinMaxSpinBoxPair( "> sections: ", " - ", 1, plug.zsize,
															  &minZRange, &maxZRange, 1,
															  "The mask and changing of stereology point will \n"
															  "only apply to grids between these section numbers");
		}
		
		dsR.exec();
		if( dsR.wasCancelled() )
			return;
	}
	
	
	//## DETERMINE FINAL RANGE OF OBJECTS AND SECTIONS FOR MASK:
	
	int minObjIdx = objIdx;			// |-- final range of objects
	int maxObjIdx = objIdx;			// |   to consider
	
	if( plug.maskObjOpt==RG_ALL )	{
		minObjIdx = 0;
		maxObjIdx = osize(imod)-1;
	}
	else if( plug. maskObjOpt==RG_CUSTOM )	{
		minObjIdx = minObjRange - 1;
		maxObjIdx = maxObjRange - 1;
	}
	
	keepWithinRange( minObjIdx, 0,         osize(imod)-1 );
	keepWithinRange( maxObjIdx, minObjIdx, osize(imod)-1 );
	
	
	int minZ = currZ;						// |-- final range of sections
	int maxZ = currZ;						// |   to consider
	
	if( plug.maskSectionsOpt==RG_ALL )	{
		minZ = 0;
		maxZ = plug.zsize;
	}
	else if( plug.maskSectionsOpt==RG_CUSTOM )	{
		minZ = minZRange - 1;
		maxZ = maxZRange - 1;
	}	
	
	
	//## FOR EACH STEREOLOGY POINT: CHECK IF IN Z RANGE, AND IF SO GO OVER ALL MASK
	//## OBJECTS TO DETERMINE IF POINT IS INSIDE MASK REGION AND SHOULD BE CHANGED:
	
	long nPtsInMask  = 0;
	long nPtsChanged = 0;
	long nSPts = g->ptsize();
	
	for( long i=0; i<nSPts; i++ )
	{
		Spoint *spt = g->getSPt(i);
		Ipoint *pt  = &spt->pos;
		int z = (int)pt->z;
		
		//## IF POINT DOESN'T MEET REQUIREMENTS: REJECT EARLY
		
		if( z < minZ || z > maxZ )									// if point outside z range:
			continue;																		// exit early
		
		if(  (plug.maskSelPtsOpt==SEL_OFF &&  spt->checked)
		  || (plug.maskSelPtsOpt==SEL_ON  && !spt->checked) )
			continue;
													// if we want only "unchecked" or "checked" points and this
													//  point doesn't match, it is disqualified
		
		//## FOR EACH MASK OBJECT, CHECK IF POINT IS INSIDE ANY CLOSED CONTOUR:
		
		bool ptIsInCont = false;							// is this point inside any closed contour
		
		for(int o=minObjIdx; o<=maxObjIdx && o<osize(imod) && (!ptIsInCont); o++)
		{
			Iobj *obj = getObj(imod,o);
			
			if( isStereologyObj(o) || !isObjClosed(obj) )		// if obj is "STEREOLOGY" or open:
				continue;																				// exit early
			
			for(int c=0; c<csize(obj) && (!ptIsInCont); c++)
			{
				Icont *cont = getCont(obj,c);
				if( getZInt(cont) != z || !isContClosed(obj,cont) )	// if cont open or diff z:
					continue;																						// exit early
				
				if( imodPointInsideCont( cont, pt ) )			// if points is inside contour
					ptIsInCont = true;												// flag as inside and exit 2 loops
			}
		}
		
		bool ptIsInMask = ( plug.maskInverseOpt==0 && ptIsInCont )
									 || ( plug.maskInverseOpt==1 && !ptIsInCont  );
		
		
		//## IF POINT IS INSIDE MASK: CHANGE IT AS APPROPRIATE:
		
		if( ptIsInMask )
		{
			nPtsInMask++;
			bool ptChanged = false;
			
			//## CHANGED CHECKED VALUE (IF NEEDED):
			
			if( plug.maskChangeToCheckedOpt==SEL_ON && !spt->checked )	
			{
				changePtCheckedAndUpdateObj( spt, true );						// want to make checked
				ptChanged = true;
			}
			else if( plug.maskChangeToCheckedOpt==SEL_OFF && spt->checked )	
			{
				changePtCheckedAndUpdateObj( spt, false );					// want to make unchecked
				ptChanged = true;
			}
			else if( plug.maskChangeToCheckedOpt==SEL_TOGGLE )		// want to toggle
			{
				changePtCheckedAndUpdateObj( spt, !spt->checked );
				ptChanged = true;
			}
			
			
			//## FOR EACH CATEGORY: CHANGE ON/OFF STAGE (IF NEEDED):
			
			for( int c=0; c<(int)spt->catSel.size() && c<nCatObjs; c++ )
			{
				int changeOpt = g->catObj[c].changeOpt;
				
				if( changeOpt==SEL_ON && !spt->isCatOn(c) )				// want to turn category on
				{
					changePtCatAndUpdateObj(spt, c, true );
					ptChanged = true;
				}
				else if( changeOpt==SEL_OFF && spt->isCatOn(c) )	// want to turn category off
				{
					changePtCatAndUpdateObj(spt, c, false );
					ptChanged = true;
				}
				else if( changeOpt==SEL_TOGGLE )									// want to toggle category
				{
					changePtCatAndUpdateObj(spt, c, !spt->isCatOn(c) );
					ptChanged = true;
				}
			}
			
			if(ptChanged)
				nPtsChanged++;
		}
	}
	
	//## OUTPUT RESULTS:
	
	MsgBox( this, "Mask results",
				  "A total of " + QStr(nPtsChanged) + " points were changed" );
	
	
	updateCatsSelectedGui();		// update GUI
	drawBlackObject(true);			// redraw in case some objects were checked/unchecked
}

//------------------------
//-- Shows a dialog where the user can choose multiple mask operations over
//-- all sections. Unlike "Apply Masks" there are fewer options.
//-- This function does not support undo, and the user is warned such.

void Stereology::applyMasks()
{
	GridSetObj *g = getCurrGridSetObj();
	int nCatObjs  = (int)g->catObj.size();
	Imod *imod      = ivwGetModel(plug.view);
	int nObjs       = osize(imod);
	
	//## IF POINT COUNTING NOT STARTED: EXIT EARLY:
	
	if( g->countingStarted==false )
	{
		MsgBox( this, "...",
					 "Sorry - you can't apply a mask to points until you have finalized "
					 "categories and started counting/categorizing points!");
		return;
	}
	
	//## GET USER TO SELECT MASK OBJECTS AND CATEGORIES TO APPLY VIA A CUSTOM DIALOG:
		
	QString catOptions = "";
	for(int c=0; c<nCatObjs; c++)
	{
		if(c>0) catOptions += "|";
		catOptions += "[" + QStr(c)+ "] " + g->catObj[c].categoryName;
	}
	
	CustomDialog ds("Apply Batch Masks", this);
	
	for(int i=0; i<NUM_MASKS; i++)
	{
		ds.addCheckBox( "Rule #" + QStr(i+1) + ":  -------------------------------",
									  &plug.masksUse[i], "Untick if you don't want to use this rule" );
		ds.addSpinBox ( "   > if point is in object: ", 1, nObjs, &plug.masksObj[i], 1,
									  "Choose which closed contour object you want to use as a mask" );
		ds.addComboBox( "   > flip on category:", catOptions, &plug.masksCat[i],
									  "Choose which category which be on (all others will will turn off)" );
	}
	
	ds.addLabel( "===================================");
	ds.addCheckBox( "Set all other points to", 
								 &plug.masksDefaultUse, "" );
	ds.addComboBox( "  ... category:", catOptions, &plug.masksDefaultCat,
								  "Choose which category which be on (all others will will turn off)" );
	ds.addComboBox( "  ... set as:", "unchanged|checked|unchecked",
								  &plug.masksDefaultChecked );
	
	ds.addLabel( "===================================");
	ds.addCheckBox( "Write changed points to console", &plug.masksPrintChanges, "" );
	
	ds.setMinimumWidth( 200 );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	
	//## FOR EACH STEREOLOGY POINT: CHECK IF IN Z RANGE, AND IF SO GO OVER ALL MASK
	//## OBJECTS TO DETERMINE IF POINT IS INSIDE MASK REGION AND SHOULD BE CHANGED:
	
	long nPtsInMask  = 0;
	long nPtsChanged = 0;
	long nSPts = g->ptsize();
	
	if( plug.masksPrintChanges )	cout << "LIST OF CHANGED POINTS:";
	
	for( long i=0; i<nSPts; i++ )
	{
		Spoint *spt = g->getSPt(i);
		Ipoint *pt  = &spt->pos;
		int z = (int)pt->z;
		
		int newCategory = -1;
		
		//## FOR EACH MASK OBJECT, CHECK IF POINT IS INSIDE ANY CLOSED CONTOUR:
		
		for(int m=0; m<NUM_MASKS; m++)
		{
			if( plug.masksUse[m]==false )
				continue;
			
			int objIdx = plug.masksObj[m]-1;
			Iobj *obj  = getObj(imod, objIdx );
						
			bool ptIsInCont = false;							// is this point inside a closed contour
			
			for(int c=0; c<csize(obj); c++)
			{
				Icont *cont = getCont(obj,c);
				if( getZInt(cont) != z || !isContClosed(obj,cont) )	// if cont open or diff z:
					continue;																						// exit early
				
				if( imodPointInsideCont( cont, pt ) )			// if points is inside contour
					ptIsInCont = !ptIsInCont;									// toggle value (might be in a hole)
			}
			
			if(ptIsInCont)
				newCategory = plug.masksCat[m];		// this is the category we want on
		}
		
		//## DETERMINE IF WAS IN MASK AND UPDATE CHECKED VALUE:
		
		if( newCategory >= 0 ) {
			changePtCheckedAndUpdateObj( spt, true );
			nPtsInMask++;
		}
		
		if( newCategory < 0 && plug.masksDefaultUse  )	// if not in mask and we 
		{																								//  want to use default cat:
			newCategory = plug.masksDefaultCat;									// mark for default category
			if(plug.masksDefaultChecked==1)		changePtCheckedAndUpdateObj( spt, true  );
			if(plug.masksDefaultChecked==2)		changePtCheckedAndUpdateObj( spt, false );	
		}
		
		//## IF POINT IS INSIDE MASK: CHANGE IT AS APPROPRIATE:
		
		if( newCategory >= 0 )
		{
			//## FOR EACH CATEGORY: CHANGE ON/OFF STAGE (IF NEEDED):
			
			bool ptChanged = false;
			
			for( int c=0; c<(int)spt->catSel.size() && c<nCatObjs; c++ )
			{
				bool newCatVal = (c==newCategory);
				
				if( spt->isCatOn(c) != newCatVal )			// if the current category is wrong
				{
					changePtCatAndUpdateObj(spt, c, c==newCategory);
					
					if( plug.masksPrintChanges ) {		// if we want to print changes:
						if(ptChanged==false)	cout << endl << "Pt #" << i+1 << ": \t";
						else									cout << ", ";
						cout << "cat:" << c << "->" << ((newCatVal) ? "ON" : "off" );
					}
					ptChanged = true;
				}
			}
			
			if(ptChanged)
				nPtsChanged++;
		}
	}
	
	//## OUTPUT RESULTS:
	
	if( plug.masksPrintChanges )	cout << endl;
	MsgBox( this, "Mask results",
				 "A total of " + QStr(nPtsInMask ) + " pts were inside a mask\n"
				 "A total of " + QStr(nPtsChanged) + " pts were changed category\n");
	
	updateCatsSelectedGui();		// update GUI
	drawBlackObject(true);			// redraw in case some objects were checked/unchecked
}


//------------------------
//-- Shows a dialog where the user can choose one or more objects with
//-- closed contours to serve as a mask.

void Stereology::applyIntercepts()
{
	GridSetObj *g = getCurrGridSetObj();
	int nCatObjs  = (int)g->catObj.size();
	
	//## IF POINT COUNTING NOT STARTED, EXIT EARLY:
	
	if( g->countingStarted==false )
	{
		MsgBox( this, "...",
					 "Sorry - you can't apply a mask to points until you have finalized "
					 "categories and started counting/categorizing points!");
		return;
	}
	
	//## IF NO GRID LINES, EXIT EARLY:
	
	if( g->isVolumeGridType()==false )
	{
		MsgBox( this, "...",
					 "Sorry - you can only apply intersections to grid types which project lines. "
					 "Perhaps you wanted to click the option above.");
		return;
	}
	
	//## IF ALLOW INTERCEPTS IN NOT ENABLED, SHOW WARNING:
	
	if( g->allowIntercepts==false )
	{
		if( !MsgBoxYesNo( this,
										  "WARNING: It appears you did not turn on the \n "
										  "  'allow intercepts' option when you clicked \n"
										  "  'Finalize Categories' and so this function have  \n"
										  "  unexpected results... \n"
										  "Are you sure you want to continue?") )
			return;
	}
	
	//## IF USER HAS NOT SELECTED A CLOSED CONTOUR OBJECT: EXIT
	
	Imod *imod      = ivwGetModel(plug.view);
	Iobj *obj       = imodObjectGet(imod);		// currently selected object
	
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	if( isStereologyObj(objIdx) )
	{
		MsgBox( this, "...",
					 "To apply a mask you must first select a 'non-stereology' object \n"
					 "with closed or open contours" );
		return;
	}
	
	
	//## GET TO SELECT CONTOUR INPUT AND CATEGORIES TO APPLY VIA A CUSTOM DIALOG:
	
	int  currZ = edit_getZOfTopZap();
	if( currZ == -1 )
		currZ = 0;
	
	static int maskObjOpt      = RG_CURR;		// 0=current object only,  1=all, 2=custom
	static int maskSectionsOpt = RG_CURR;		// 0=current section only, 1=all, 2=custom
								// NOTE: Most of these use the enums (see: ptchange & rangeopts)
	
	
	CustomDialog ds("Apply Mask", this);
	
	ds.addLabel   ( "--- INTERSECTS TO APPLY ---", true );
	
	ds.addComboBox( "using contours from:",
								 "object " + QStr(objIdx+1) + " only|"
								 "all non-stereology objects|"
								 "let me enter a range", &maskObjOpt,
								 "Specifies which object(s) you want to use to create the mask. \n"
								 "If you select 'let me enter a range' you can enter a range in \n"
								 "the next window. Any objects which are marked as 'STEREOLOGY' \n"
								 "objects (with this keyword in their name or label) will \n"
								 "automatically be skipped." );
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "--- CHANGE TO MAKE: ---", true );
	
	ds.addSpinBox ( "add points to category:", 0, (int)g->catObj.size()-1,
								 &plug.interceptCat, 1,
								 "Chose which category you want to add points to" );
	
	ds.addHtmlLabel ( "<b>WARNING</b>: You cannot undo this operation! <br>"
									 "It's recommended you save before you hit okay." );
	ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	//## DETERMINE WHAT OBJECT MACHES THE CHOSEN CATEGORY AND
	//## EXIT EARLY IF IT'S NOT A VALID OBJECT:
	
	int catObjIdx = g->getCatObjIdx(plug.interceptCat);
	if( !isStereologyObj(catObjIdx) )
	{
		QMessageBox::critical( this, "...",
													"The category you selected does not seem to have "
													"a matching object" );
		return;
	}
	
	//## IF SPECIFIED: ALLOW USER TO SET RANGE FOR OBJECTS AND/OR SECTIONS:
	
	static int minObjRange = objIdx+1;
	static int maxObjRange = objIdx+1;
	
	static int minZRange = 1;
	static int maxZRange = plug.zsize;
	
	if( maskObjOpt == RG_CUSTOM || maskSectionsOpt == RG_CUSTOM )
	{
		if( minZRange > maxZRange )												  minZRange = maxZRange;
		if( maxObjRange > osize(imod) || maxObjRange < 1 )	maxObjRange = osize(imod) - 1;
		if( minObjRange > maxObjRange || minObjRange < 1 )	minObjRange = maxObjRange;
		
		CustomDialog dsR("Set Range", this);
		
		if( maskObjOpt == RG_CUSTOM )
		{
			dsR.addLabel( "" );
			dsR.addLabel( "Use only closed contours in these objects:", true );
			
			dsR.addMinMaxSpinBoxPair( "> objects: ", " - ", 1, osize(imod),
															 &minObjRange, &maxObjRange, 1,
															 "And CLOSED contours in this range will be used \n"
															 "to generate the mask to change stereology points" );
		}
		
		if( maskSectionsOpt == RG_CUSTOM )
		{
			dsR.addLabel( "" );
			dsR.addLabel( "Only change grids between these sections:", true );
			
			dsR.addMinMaxSpinBoxPair( "> sections: ", " - ", 1, plug.zsize,
															 &minZRange, &maxZRange, 1,
															 "The mask and changing of stereology point will \n"
															 "only apply to grids between these section numbers");
		}
		
		dsR.exec();
		if( dsR.wasCancelled() )
			return;
	}
	
	
	//## DETERMINE FINAL RANGE OF OBJECTS AND SECTIONS FOR MASK:
	
	int minObjIdx = (maskSectionsOpt==RG_CURR) ? objIdx : 0;
	int maxObjIdx = (maskSectionsOpt==RG_CURR) ? objIdx : osize(imod)-1;
	
	int minZ = (maskObjOpt==RG_CURR) ? currZ : 0;
	int maxZ = (maskObjOpt==RG_CURR) ? currZ : plug.zsize;
	
	if(maskSectionsOpt==RG_CUSTOM)
	{
		minObjIdx = minObjRange - 1;
		maxObjIdx = minObjRange - 1;
	}
	if(maskObjOpt==RG_CUSTOM)
	{
		minZ = minZRange - 1;
		maxZ = maxZRange - 1;
	}
	
	keepWithinRange( minObjIdx, 0,         osize(imod)-1 );
	keepWithinRange( maxObjIdx, minObjIdx, osize(imod)-1 );
	
	
	//## FOR EACH TEST LINE, CHECK IF IN Z RANGE, AND IF SO GO OVER ALL MASK
	//## OBJECTS/CONTOURS TO DETERMINE INTERCEPTS WITH THIS LINE AND ADD THEM
	//## TO "intercepts" CONTOUR:
	
	long nPtsInMask  = 0;
	long nLinesWithIntercepts = 0;
	long nPtsAdded   = 0;
	long nSPts = g->ptsize();
	
	Icont *intercepts = imodContourNew();		// will store a list of intercept points
	Icont *testLine   = imodContourNew();		// used to generate each test line
																					//  (extending from stereology point)
	
	for( long i=0; i<nSPts; i++ )
	{
		Spoint *spt = g->getSPt(i);
		Ipoint *pt  = &spt->pos;
		int z = (int)pt->z;
		
		//## IF POINT DOESN'T MEET REQUIREMENTS: REJECT EARLY
		
		if( z < minZ || z > maxZ )									// if point outside z range:
			continue;																		// exit early
		
		//## FOR CONTOUR IN A MASK OBJECT ADD ANY INTERSECTIONS WITH THIS
		//## TEST LINE TO "intercepts" CONTOUR:
		
		int nInterceptsThisLine = 0;
		
		g->genContourForPt(i, testLine);
		
		for(int o=minObjIdx; o<=maxObjIdx && o<osize(imod); o++)
		{
			Iobj *obj = getObj(imod,o);
			
			if( isStereologyObj(o) )				// if obj is "STEREOLOGY":
				continue;												// exit early
			
			for(int c=0; c<csize(obj); c++)
			{
				Icont *cont = getCont(obj,c);
				
				if( getZInt(cont) != z )				// if cont diff z:
					continue;												// exit early
				
				bool contClosed = isContClosed(obj,cont);
				
				nInterceptsThisLine += cont_addPtsAtIntersections( testLine, cont,
																													 false, contClosed,
																													 intercepts, false );
			}
		}
		
		if( nInterceptsThisLine > 0 )
			nLinesWithIntercepts++;
	}
	
	
	
	//## FOR EACH INTERCEPT FOUND (AS LISTED IN "intercepts") ADD
	//## AN INTERCEPT POINT THE SPECIFIED CATEGORY:
	
	for( int p=0; p<psize(intercepts); p++)
	{
		Ipoint *pt = getPt( intercepts, p );
		
		//## FIND NEAREST INTERCEPT POINT ON THE LINE USING A SNAP SETTING
		
		Ipoint ptSnap;
		bool interceptFound = g->getInterceptNearPt( *pt, &ptSnap, INTERCEPT_SNAP );
		
		if( !interceptFound )		// if no intercept found near mouse: skip
			continue;
		
		//## IF INTERCEPT IS FOUND ADD IT TO THE FIRST CONTOUR:
		
		addOrRemoveSinglePtFromObj( catObjIdx, ptSnap, false );	// if pt already here delete
		addOrRemoveSinglePtFromObj( catObjIdx, ptSnap, true  );	// add point to this spot
		
		nPtsAdded++;
	}
	
	
	imodContourDelete( testLine );
	imodContourDelete( intercepts );
	
	//## OUTPUT RESULTS:
	
	MsgBox( this, "Intercept results",
				  "A total of " + QStr(nPtsAdded) + " intercept points were added \n"
				  "A total of " + QStr(nLinesWithIntercepts) + " test lines intercepted \n"
				  "(one or more) contours" );
	
	updateCatsSelectedGui();		// update GUI
	drawBlackObject(true);			// redraw in case some objects were checked/unchecked
}




//------------------------
//-- Shows a dialog where the user can apply a set of "rules" to
//-- one or more grids. These "rules" allow the user to specify a 
//-- "SELECTION CRITERIA" to select only points which are 
//-- checked/unchecked and/or have certain categories on or off. 
//-- The user also specifies "CHANGE TO MAKE", so that selected
//-- point can become checked/unchecked and or have certain categories
//-- turned on or off. After changes are applied a message box
//-- says how many points have been changed.
//-- This function does not support undo, and the user is warned such.

void Stereology::applyRulesToChangePts()
{
	GridSetObj *g = getCurrGridSetObj();
	int nCatObjs  = (int)g->catObj.size();
	
	//## IF POINT COUNTING NOT STARTED: EXIT EARLY:
	
	if( g->countingStarted==false )
	{
		MsgBox( this, "...",
					 "Sorry - you can't apply a change rules to points until you have finalized "
					 "categories and started counting/categorizing points!");
		return;
	}
	
	//## IF THE TOP ZAP IS NOT SHOWING A GRID: EXIT EARLY:

	int  currZ = edit_getZOfTopZap();
	
	if( currZ<0 || !g->isGridOnSlice(currZ) )
	{
		MsgBox( this, "...",
					 "To apply rules, you should first set your top ZAP window to "
					 "view the grid (or one of the grids) you wish to modify.");
		return;
	}
	
	
	//## GET TO SELECT CONTOUR INPUT AND CATEGORIES TO APPLY VIA A CUSTOM DIALOG:
	
	static int minZLimitSet = 1;
	static int maxZLimitSet = plug.zsize;
	static bool currSectOnly   = true;
	
	static int maskSelPtsOpt   = SEL_OFF;		// 0=all, 1=checked, 2=unchecked only
	static int changeToCheckedOpt = SEL_ON;	// 0=don't change, 1=checked, 2=unchecked
	
	if( currSectOnly && currZ >= 0 )
	{
		minZLimitSet = currZ+1;
		maxZLimitSet = currZ+1;
	}
	
	CustomDialog ds("Change Point Values Using Rules", this);
	
	ds.addLabel   ( "... select points in the" );
	
	
	ds.addLabel   ( "    --- GRID RANGE: ---", true );
	
	ds.addCheckBox( "current section only", 
									&currSectOnly,
									"If true: rules will be applied to points on the current \n"
									"view/grid only.\n"
									"If false: rules will be applied to all points between the \n"
									"minimum and maximum section number below (inclusive)." );
	
	ds.addMinMaxSpinBoxPair( "   (OR) range of sections:", " - ", 1, plug.zsize,
													 &minZLimitSet, &maxZLimitSet, 1,
													 "If the checkbox above is unchecked... then only \n"
													 "grids between these two section numbers (inclusive) \n"
													 "will be included" );
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "... and which meet these" );
	
	ds.addLabel   ( "    --- SELECTION CRITERIA: ---", true );
	
	ds.addComboBox( "point must be:",
								 "checked or unchecked|"
								 "checked|"
								 "unchecked", &maskSelPtsOpt,
								 "By default, this is set to 'unchecked points', meaning that \n"
								 "any points you have already checked will be unaltered. \n"
								 "In some rare cases however, you can opt that only checked points \n"
								 "or both checked and unchecked points are selected for change." );
	
	
	ds.beginGroupBox( "points must have:", false, "" );
	//ds.setStylePrev("background-color: rgb(0, 255, 0);");				// green
	ds.setStylePrev("background-color: rgb(255, 230, 90);");			// yellowish
	for( int c=0; c<nCatObjs; c++ )
	{
		CategoryObj *catObj = &g->catObj[c];
		QString catName = "[" + QStr(c) + "] " + catObj->categoryName;
		ds.addComboBox( catName,
									 "-|"
									 "on|"
									 "off", &catObj->selectOpt,
									 "If set to 'on' or 'off' - only points with this category \n"
									 "on or off are candiates for alteration" );
	}
	ds.endGroupBox();
	
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "... and then apply" );
	ds.addLabel   ( "    --- CHANGES TO MAKE: ---", true );
		
	ds.addComboBox( "set these points as:",
								 "don't change|"
								 "checked|"
								 "unchecked|"
								 "toggle", &changeToCheckedOpt,
								 "All selected points will be changed to show this value" );
	
	ds.beginGroupBox( "set categories as:", false, "" );
	ds.setStylePrev("background-color: rgb(255, 230, 110);");			// yellowish
	for( int c=0; c<nCatObjs; c++ )
	{
		CategoryObj *catObj = &g->catObj[c];
		QString catName = "[" + QStr(c) + "] " + catObj->categoryName;
		ds.addComboBox( catName,
									 "-|"
									 "turn ON|"
									 "turn off|"
									 "toggle", &catObj->changeOpt,
									 "All selected points inside will be changed to show this value" );
	}
	ds.endGroupBox();
	
	ds.addHtmlLabel ( "<b>WARNING</b>: You cannot undo this operation! <br>"
									 "It's recommended you save before you hit okay.",
									 "Due to the way this STEREOLOGY plugin has been fitted to IMOD \n"
									 "(a program not originally designed for stereology), it was \n"
									 "difficult to include any undo functionality, therefore we \n"
									 "suggest you hit save before any major changes... and if you \n"
									 "need to revert then you can try to reload this model." );
	ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	
	ds.setMinimumWidth( 200 );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	
	//## DETERMINE FINAL RANGE OF SECTIONS:
	
	int minZ = (currSectOnly) ? currZ : minZLimitSet-1;
	int maxZ = (currSectOnly) ? currZ : maxZLimitSet-1;
	
	if( minZ < 0 || maxZ >= plug.zsize || minZ > maxZ )
	{
		MsgBox( this, "...",
					 "Bad range of z values given - changes have been aborted.");
		return;
	}
	
	
	
	//## FOR EACH STEREOLOGY POINT: CHECK IF IN Z RANGE, AND IF SO GO OVER ALL MASK  
	//## OBJECTS TO DETERMINE IF POINT SHOULD BE CHANGED:
	
	long nPtsInMask  = 0;
	long nPtsChanged = 0;
	long nSPts = g->ptsize();
	
	for( long i=0; i<nSPts; i++ )
	{
		Spoint *spt = g->getSPt(i);
		int z = (int)spt->pos.z;
		
		//## IF POINT DOESN'T MEET "CHECKED" REQUIREMENTS: REJECT EARLY
		
		if( z < minZ || z > maxZ )									// if point outside z range:
			continue;																		// exit early
		
		if(   (maskSelPtsOpt==SEL_OFF &&  spt->checked)
			 || (maskSelPtsOpt==SEL_ON  && !spt->checked) )
			continue;
															// if we want only "unchecked" or "checked" points and this
															//  point doesn't match, it is disqualified
		
		
		//## FOR EACH CATEGORY, CHECK IF POINT IS REJECTED:
		
		bool rejectPt = false;							// is this point inside any closed contour
		
		for( int c=0; c<(int)spt->catSel.size() && c<nCatObjs && (!rejectPt); c++ )
		{
			int selectOpt = g->catObj[c].selectOpt;
			if(   ( selectOpt==SEL_ON  && !spt->isCatOn(c) )
				 || ( selectOpt==SEL_OFF && spt->isCatOn(c) ) )
			{
				rejectPt = true;
				break;
			}
		}
		
		if( rejectPt )
			continue;
		
		
		//## CHANGED CHECKED VALUE (IF NEEDED):
		
		bool ptChanged = false;
		
		if( changeToCheckedOpt==SEL_ON && !spt->checked )					// want to make checked
		{
			changePtCheckedAndUpdateObj( spt, true );
			ptChanged = true;
		}
		else if( changeToCheckedOpt==SEL_OFF && spt->checked )		// want to make unchecked
		{
			changePtCheckedAndUpdateObj( spt, false );
			ptChanged = true;
		}
		else if( changeToCheckedOpt==SEL_TOGGLE )									// want to toggle
		{
			changePtCheckedAndUpdateObj( spt, !spt->checked );
			ptChanged = true;
		}
		
		
		//## FOR EACH CATEGORY: CHANGE ON/OFF STAGE (IF NEEDED):
		
		for( int c=0; c<(int)spt->catSel.size() && c<nCatObjs; c++ )
		{
			int changeOpt = g->catObj[c].changeOpt;
			
			if( changeOpt==SEL_ON && !spt->isCatOn(c) )				// want to turn category on
			{
				changePtCatAndUpdateObj(spt, c, true );
				ptChanged = true;
			}
			else if( changeOpt==SEL_OFF && spt->isCatOn(c) )	// want to turn category off
			{
				changePtCatAndUpdateObj(spt, c, false );
				ptChanged = true;
			}
			else if( changeOpt==SEL_TOGGLE )									// want to toggle category
			{
				changePtCatAndUpdateObj(spt, c, !spt->isCatOn(c) );
				ptChanged = true;
			}
		}
		
		if(ptChanged)
			nPtsChanged++;
	}
	
	
	//## OUTPUT RESULTS:
	
	MsgBox( this, "Mask results",
				 "A total of " + QStr(nPtsChanged) + " points were changed" );
	
	
	updateCatsSelectedGui();		// update GUI
	drawBlackObject(true);			// redraw in case some objects were checked/unchecked
}




//------------------------
//-- Brings up a dialog which calculates and displays the users "progress"
//-- in terms of how many of the total points he has checked, including
//-- a progress bar to show the percentage of points completed, and
//-- an estimate of how long it will take to segment the remaining points.

void Stereology::checkProgress()
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( !g->countingStarted )
	{
		MsgBox("You cannot check progress until you have finalized the grid, "
					 "finalized categories and started counting.");
		return;
	}
	
	long psize = g->ptsize();
	
	//## COUNT NUMBER OF CHECKED POINTS:
	
	long ptsChecked = g->countCheckedPts(false);	// counts the number of checked points
	long ptsUnchecked = psize - ptsChecked;
	float fractDone   = (ptsChecked==0) ? 1 : (float)ptsChecked / (float)psize;
	float percentProg = roundDecimals( fractDone*100.0f, 1 );
	
	QString progressStr;
	
	if( !g->countingFinished )
	{
		progressStr += "# points checked: &nbsp; &nbsp; <b>" + QStr(ptsChecked) + "</b> / ";
		progressStr += QStr(psize) + " &nbsp; (<b>"+ QStr(percentProg) + "%</b>)<br>";
		progressStr += "# points unchecked: &nbsp;<b>" + QStr(ptsUnchecked) + "</b>";
	}
	else
	{
		progressStr += "# points checked:  <b>all " + QStr(psize) + "</b> points!<br>";
		progressStr += "... you are now ready for '<i>Options > Calculate Results</i>'  ";
	}
	
	
	float totEstimatedSecs = ptsUnchecked * plug.secsPerPt;
	QString estTimeLeft    = formatApproxTime( totEstimatedSecs );
	
	float totSecsWorked = (float)(int)(plug.numMinutesWorked * 60.0f);
	QString approxTimeWorked = formatApproxTime( totSecsWorked );
	float averageSecsPerPt   = (ptsChecked==0) ? 0 : fDiv( totSecsWorked, ptsChecked );
	averageSecsPerPt = (float)((int)(averageSecsPerPt*100.0f)) / 100.0f;
	
	//## COUNT NUMBER OF POINTS IN EACH CATEGORY:
	
	long totCatHits = 0;
	long totPtsWithAnyCatOn = g->calcResults(true,true,&totCatHits,0,MAX_INT);		
														// tallies the number of points in each category and
														//  returns the total # of points with any category on
	
	//## OUTPUT PROGRESS RESULTS IN A CUSTOM DIALOG:
	
	CustomDialog ds("Summary of Progress", this, BS_OKAY_ONLY );
	
	ds.addLabel   ( "--- NUMBER OF POINTS DONE: ---", true );
	
	ds.addHtmlLabel  ( progressStr, "Shows how much progress you've made" );
	ds.setStylePrev("background-color: rgb(255, 230, 110);");			// light yellow
	if( g->countingFinished )
		ds.setStylePrev("background-color: rgb(0, 255, 0);");				// green
	
	ds.addHtmlLabel  ( "Approx. time spend: &nbsp; <b>~" + approxTimeWorked + "</b>",
										"This represents ~how much time you have spend<br>"
										"<b>classifying points during this 3dmod session</b>. <br>"
										"This value will only update during each minute interval<br>"
										"in which you classify a point so time you may have spend<br>"
										"doing anything else shouldn't be included. Assuming you<br>"
										"have just started this dataset, you are averaging: <br><br>"
										" &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; "
										"~<b>" + QStr(averageSecsPerPt) + " seconds/point</b>" );
	
	if( !g->countingFinished )
	{
		ds.addHtmlLabel  ( "Estimated time remaining: &nbsp; <b>~" + estTimeLeft + "</b>",
											 "This is based on the estimate of <br>"
											 " &nbsp; &nbsp; <b>" + QStr(plug.secsPerPt) +
											 " seconds/point</b>. <br><br>"
											 "TIP: If it needs adjusting can change this estimated "
											 "     rate via '<b>Options > Settings</b>'" );
		ds.setStylePrev("background-color: rgb(100, 255, 100);");			// light green
	}
	
	ds.beginGroupBox( "Points / Category:", false );
	
	ds.addLabel( "# Pts \tPercent \tCategory", false,
							 "Shows the number of points in each category \n"
							 "and the percental of checked points in this category." );
	ds.setStylePrev("background-color: rgb(200, 200, 200);");			// grey
	for( int c=0; c<(int)g->catObj.size(); c++ )
	{
		long numPtsOn = g->catObj[c].numPtsOn;
		float fractOfChecked = (ptsChecked==0) ? 0 : ( (float)numPtsOn / (float)ptsChecked);
		float percent = roundDecimals( fractOfChecked*100.0f, 1 );
		QString catName = "[" + QStr(c) + "] " + g->catObj[c].categoryName;
		ds.addLabel( QStr(numPtsOn) + " \t" + QStr(percent) + "% \t" + catName );
	}
	
	ds.endGroupBox();
	
	ds.addProgressBar( "Points done:", percentProg, 200, false );
	
	ds.exec();
}



//------------------------
//-- Does a check over the current grid and brings up a dialog to
//-- show a list of invalid points within the current grid where an
//-- invalid points is either:
//--   (A) a checked point with no categories on (easy to do)  OR
//--   (B) a point with >1 category on, in a grid
//--       where "allowMultipleCats" is off
//--
//-- If all points are valid, a green "no errors were found" message
//-- is shown. If not all points are valid, it prints a summary of
//-- these errors, a list of bad points in a text area, and the very
//-- first invalid point is selected in the ZAP.

void Stereology::validatePoints()
{
  GridSetObj *g = getCurrGridSetObj();
	
	//## SEARCH FOR BAD POINTS:
	
	long firstBadPtIdx;
	QString badPtsStr;
	int numCheckedPtsNoCat;
	int numPtsMultiCat;
	bool pointsValid = g->validatePtValues( &badPtsStr, &firstBadPtIdx, 
																				  &numCheckedPtsNoCat, &numPtsMultiCat );
								// tallies and returns a list and text representation of bad points
	
	
	//## IF POINTS INVALID POINTS FOUND: SELECT AND JUMP TO THE FIRST OF THEM
	
	if( !pointsValid && firstBadPtIdx != -1 )
	{
		g->setCurrPt( firstBadPtIdx, false );
		updateCurrPtInGui( true, true );
	}
	
	
	//## OUTPUT GRID VALIDATION RESULTS IN CUSTOM DIALOG:
	
	CustomDialog ds("Grid Validation", this, BS_OKAY_ONLY );
	
	ds.addLabel   ( "--- POINT VALIDATION RESULTS: ---", true );
	
	if( pointsValid )
	{
		ds.addHtmlLabel( "No errors were found with your points!  :-)" );
		ds.setStylePrev("background-color: rgb(100, 255, 100);");			// light green
	}
	else
	{
		ds.addHtmlLabel( "Several errors were found:" );
		ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
		
		ds.addHtmlLabel( "# points with > 1 category: <b>"
										 + QStr(numPtsMultiCat) + "</b>" );
		ds.addHtmlLabel( "# checked but with no category: <b>"
										 + QStr(numCheckedPtsNoCat) + "</b>" );
		
		ds.addLabel   ( "" );
		ds.addLabel   ( "--- LIST OF BAD POINTS: ---" , true,
									 "Belows is the list of all bad points \n"
									 "detected. You should visit all of these \n"
									 "and fix them before running analysis" );
		
		ds.addReadOnlyTextEdit ( badPtsStr, false, 120, 
														"Shows the list of all bad points detected" );
	}
	
	ds.exec();
}


//------------------------
//-- Bring up a dialog with a number of options for generating a
//-- table of results based on the number of points in all, or 
//-- a subset of categories within the current grid.
//-- 
//-- Some of these options include:
//--  > the ability to omit certain categories
//--  > the ability to limit results to a range of sections/grids
//--  > the option to print a seperate count per each grid.
//--  > an option to "tranpose" the final table
//--
//-- .. and in terms of output, the user can output results to either
//-- the clipboard, console or text file. Whichever of these 
//-- options is selected, the next dialog which appears contains
//-- a copy of the table in a text area (ready to copy and paste)
//-- and a crude bar graph showing the amount of points in 
//-- each of the selected categories.

void Stereology::calculateResults()
{
	GridSetObj *g = getCurrGridSetObj();
	
	if( !g->countingStarted )
	{
		QMessageBox::warning( this, "...",
												  "You cannot calculate results until you have finalized the "
												  "grid, finalized categories and started counting.");
		return;
	}
	
	
	//## COUNT THE NUMBER OF CHECKED POINTS:
	
	long ptsChecked = g->countCheckedPts(false);	// counts the number of checked points
	long psize = g->ptsize();
	int csize = (int)g->catObj.size();
	int numCountingCats = g->numCatsStartingWithNumbers();
	Imod *imod  = ivwGetModel(plug.view);
	float pixelSize = imodGetPixelSize(imod);
	char *unitsChs = imodUnits(imod);
	QString unitsStr = unitsChs;
	Ipoint scalePt;
	setPt( &scalePt, 1,1, imodGetZScale(imod) );
	
	static int showCalcType = -1;
	if( showCalcType == -1 && g->isSurfaceAreaGridType()    )		showCalcType = MT_SA;
	if( showCalcType == -1 && g->isNumberDensityGridType() )		showCalcType = MT_NUM;
	if( showCalcType == -1 || numCountingCats == 0          )		showCalcType = MT_VOL;
	
	
	//## IF PIXEL SIZE AND/OR UNITS NOT SET: SHOW ERROR MESSAGE:
	
	bool unitsNotSetup = (unitsStr == "pixels" || unitsStr == "" || pixelSize == 1.0f);
	
	if( unitsNotSetup )			// if units don't appear setup: display warning message
	{
		QMessageBox::warning( this, "Missing Pixel Size Info",
										"WARNING: Pixels size and/or units do not appear to "
										"be setup yet, thus making it impossible to estimate "
										"surface area density (Sv) or length density (Lv). \n"
										"Go to 'Edit > Model > Header' to fix this." );
	}
	
	//## GET USER INPUT FROM CUSTOM DIALOG:
	
	CustomDialog dsI("Calculate Stereology Results", this);
	
	if( ptsChecked != psize )
	{
		float fractDone   = fDiv( ptsChecked, psize );
		float percentProg = roundDecimals( fractDone*100.0f, 1 );
		
		dsI.addHtmlLabel  ( "WARNING: You have only checked <br>\t" + QStr(ptsChecked) +
											 " of " + QStr(psize) + " points (<b>"+ QStr(percentProg) +
											 "</b>%)<br>Ideally all points should be checked",
											 "It is advised you check all points before running this \n"
											 "Only checking certain points can bias results and \n"
											 "make it hard to reproduce / explain why not all \n"
											 "points were checked" );
		dsI.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	}
	
	if( !g->validateAllPtValues() )
	{
		dsI.addHtmlLabel  ( "WARNING: Some of your point values appear invalid. <br>"
											  "Please run 'Options > Validate point values' <br>"
											  "to fix this" );
		dsI.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	}
	
	dsI.addLabel   ( "--- CATEGORIES TO INCLUDE ---", true );
	
	dsI.beginGroupBox( "include these categories only:", false,
									  "Only checked items will be added together to count the total \n"
									  "and caculate a percentage breakdown. \n"
									  "Unchecked items will be ignored." );
	
	for( int c=0; c<csize; c++ )
	{
		QString catName = "[" + QStr(c) + "] " + g->catObj[c].categoryName;
		dsI.addCheckBox( catName, &g->catObj[c].includeInResults,
									  "If true: this category will be included in the total count." );
	}
	dsI.endGroupBox();
	
	dsI.addLabel   ( "" );
	dsI.addLabel   ( "--- TABLE OUTPUT OPTIONS ---", true );
	
	dsI.addCheckBox( "transpose (output categories along columns not rows)", 
									&plug.resultsTranspose,
									"Use this option to transpose the final results table.\n"
									"If unchecked: results output with each category along a row:\n"
									"\n"
									"  CATEGORY NAME \t# POINTS \tFRACTION TOTAL\n"
									"  Mitochondria \t10 \t0.25\n"
									"  Cytoplasm    \t30 \t0.75\n"
									"\n"
	                "If checked: results will be output one column per category: \n"
									"\n"
									"  CATEGORY NAME:  \tMitochondria \tCytoplasm \n"
									"  # POINTS:       \t10 \t30 \n"
									"  FRACTION TOTAL:  \t0.25 \t0.75");
	
	dsI.addCheckBox( "include a separate breakdown for each grid", 
									&plug.resultsSepGrids,
									"If true: extra options appear which let you specify a \n"
									"minimum and maximum section number and show results \n"
									"for each grid seperately." );
	
	dsI.addComboBox( "output measurement:",
									"volume density (Vv) only|"
									"surface area density (Sv)|"
									"length density (Lv)|"
									"number density (Nv)", &showCalcType,
									"Pick what type of result you'd like to generate:\n"
									"> Volume Density (Vv): \n"
									"     calculates volume density estimate (representing a\n"
									"     fraction of the included volume) for each category using \n"
									"     the basic formulae: \n"
									"        volumeDensity[x] = numPtsOn[x] / totPtsWithCatOn \n"
									"\n"
									"> Surface Area Density (Sv): \n"
									"     calculates surface area density (eg: nm^2/nm^3) \n"
									"     using numbered 'INTERSECTION' categories and the \n"
									"     total length of test lines using the formule: \n"
									"        Sv = 2 * TOTAL COUNT / TOTAL LINE LENGTH \n"
									"\n"
									"> Length Density (Lv): \n"
									"     not currently working sorry. \n"
									"\n"
									"> Number Density (Nv): \n"
									"     calculates the number density (number surfaces / nm^3 \n"
									"     using the total counts (using numbered categories) and \n"
									"     the total volume within forbidden squares with the formulae:\n"
									"        Nv = ( totCounts / totVolForbiddenSquares )" );
	dsI.setStylePrev("background-color: rgb(100, 100, 255);");			// light blue
	
	dsI.addComboBox( "output results to:",
									"window only|"
									"window + clipboard|"
									"window + console|"
									"window + save file", &plug.resultsOutputOpt,
									"Options for outputting final results:\n"
									"> window only        - is printed to a text area in the next\n"
									"                       dialog/window\n"
									"> window + clipboard - is output to text area, and also copied\n"
									"                       into the clipboard - ready to immediately\n"
									"                       paste with [ctrl]+[v]\n"
									"> window + console   - is output to text area, and also the\n"
									"                       console/terminal window used to launch imod\n"
									"> window + save file - is output to text area, and a save dialog\n"
									"                       appears to save the output to a text file" );
	
	dsI.addComboBox( "seperate values with:",
								 "tab|"
								 ",|"
								 ",tab|"
								 ";|"
								 "pipe|"
								 "enter", &plug.resultsSepCharIdx,
								 "Allows you to chose what string to seperate values in the table...\n"
								 "By default this is a tab, which should be easy to cut\n"
								 "and paste into most spreadsheet programs (including Windows Excel)\n"
								 "but you may also use a comma so you can create a CSV \n"
								 "(comma seperated value) file." );
	
	dsI.addCheckBox( "output standard deviation", 
									&plug.resultsShowStDev,
									"Outputs a standard deviation for each category  \n"
									"based on the formulae for the standard deviation   \n"
									"of a binomial distribution assuming normal   \n"
									"approximation: \n"
									"   o = sqrt(P(P-1)/n)" );
	
	dsI.addLabel   ( "" );
	dsI.addLabel   ( "--- RANGE SETTINGS ---", true );
	
	dsI.addCheckBox( "average points over all grid/sections *", 
									&plug.resultsAvgAllGrids,
									"If false: extra options appear which let you specify a \n"
									"minimum and maximum section number and/or breakdown \n"
									"grids into seperate regions." );
	
	dsI.exec();
	if( dsI.wasCancelled() )
		return;
  
	
	QString sepC;										// separation string (used to seperate output)
	if     ( plug.resultsSepCharIdx==0 )	sepC = "\t";
	else if( plug.resultsSepCharIdx==1 )	sepC = ",";
	else if( plug.resultsSepCharIdx==1 )	sepC = ",\t";
	else if( plug.resultsSepCharIdx==2 )	sepC = ";";
	else if( plug.resultsSepCharIdx==3 )	sepC = "|";
	else if( plug.resultsSepCharIdx==4 )	sepC = "\n";
	
	
	bool includeSACount  = ( showCalcType == MT_SA );
	bool includeLenCount = ( showCalcType == MT_LEN );
	bool includeNumCount = ( showCalcType == MT_NUM );
	
	
	
	//## SHOW DIALOG IF SPECIAL MEASUREMENT TYPE SELECTED:
	
	if( includeSACount )
	{
		CustomDialog dsT("Stereology Measurement", this, BS_OKAY_ONLY);
		dsT.addHtmlLabel
		 ( "You have chosen to output: '<b>Surface Area Density (Sv)</b>' <br>"
			 "Which is done using this formuale: <br>"
			 "<br>  &nbsp; &nbsp;  &nbsp; &nbsp; "
			 "<b>Sv = 2 * TOTAL COUNT / TOTAL LINE LENGTH</b><br><br>"
			 "For it to work correctly, make sure you've set the <br>"
			 "correct pixel size (under '<b>Edit > Model > Header'</b>) and <br>"
			 "your objects used to count the number of intersections <br>"
			 "must be labelled <b><i>'0_INTERSECTIONS', '1_INTERSECTIONS'</i></b>, <br>"
			 "etc to be tallied correctly." );
		dsT.exec();
	}
	
	
	//## PROMPT USER FOR OPTIONS TO SEPERATE OUTPUT:
	
	static int minZLimitSet = 1;
	static int maxZLimitSet = plug.zsize;
	static string sliceRangeStr = "";
	vector<int> rangesMin;
	vector<int> rangesMax;
	
	if( !plug.resultsAvgAllGrids )
	{
		CustomDialog dsL("Stereology Results - Limits", this);
		
		dsL.addCheckBox( "only include only grids between", 
									 &plug.sameXYSpacing );
		
		dsL.addMinMaxSpinBoxPair( "section ", " - ", 1, plug.zsize,
														 &minZLimitSet, &maxZLimitSet, 1,
														 "If checked, only grids between these section numbers \n"
														 "will be included in the total counts" );
		
		dsL.addLineEdit( "cluster grids between these values:", 
										&sliceRangeStr,
										"Allows you to specify a certain range of sections, for example: \n"
										" > '1-10,11-20,21-30'\n\n"
										"Where each range will be clustered together and results for each \n"
										"cluster shown seperately in the results table. \n"
										"These values must always be within the min and max values \n"
										"above.");
		
		dsL.exec();
		if( dsL.wasCancelled() )
			return;
		
		if( sliceRangeStr.length() > 1 )
		{
			QString sliceRangeQStr = sliceRangeStr.c_str();
			QStringList rangeList  = sliceRangeQStr.split(',');
			for(int i=1; i<rangeList.size(); i++)
			{
				QStringList minMaxList = rangeList[i].split('-');
				
				if( (int)minMaxList.size() == 1 )
				{
					int min = MAX( minMaxList[0].trimmed().toInt(), minZLimitSet ) - 1;
					rangesMin.push_back( min );
					rangesMax.push_back( min );
				}
				else if( (int)minMaxList.size() == 2 )
				{
					int min = MAX( minMaxList[0].trimmed().toInt(), minZLimitSet ) - 1;
					int max = MIN( minMaxList[0].trimmed().toInt(), maxZLimitSet ) - 1;
					rangesMin.push_back( min );
					rangesMax.push_back( max );
				}
				else
				{
					wprint("\aCluster list not in correct format\n");
				}
			}
		}
	}
	
	int minZVal = (plug.resultsAvgAllGrids) ? 0              : minZLimitSet;
	int maxZVal = (plug.resultsAvgAllGrids) ? plug.zsize - 1 : maxZLimitSet;
	
	int gridsIncluded = 0;
	for( int z=minZVal; z<maxZVal; z++ )
		if( g->isGridOnSlice(z) )
			gridsIncluded++;
	
	
	//## COUNT NUMBER OF POINTS IN EACH CATEGORY:
	
	long totCatHits = 0;
	long totPtsInResults = g->calcResults(false,true,&totCatHits,minZVal,maxZVal,true);	
															// tallies the number of points in each category,
															//  ignoring categories which are not included above
	
	int numCatsOnForAnalysis = 0;
	for( int c=0; c<csize; c++ )
		if ( g->catObj[c].includeInResults==true )
			numCatsOnForAnalysis++;
	
	float fractPtsOfTotal   = fDiv( totPtsInResults, psize );
	float percentPtsOfTotal = roundDecimals( fractPtsOfTotal*100.0f, 2 );
	
	QString fileNameStr;
	if( imodGetFilename(imod)!=NULL )
		fileNameStr = (QString)imodGetFilename(imod);		// get file name
	else if( fileNameStr.isEmpty() )
		fileNameStr = "unknown/unsaved";
	
	
	//## GENERATE RESULTS INTO STRINGS OF DESIRED FORMAT:
	
	QString warningStr;
	
	if( g->allowMultipleCats==false && totCatHits!=totPtsInResults )
	{
		warningStr += "WARNING: There appear to be checked points \n";
		warningStr += " without any category.";
	}
	
	QString outStr = "STEREOLOGY RESULTS:\n\n";		// main output string
	
	outStr += "file name: .............................. " + sepC + fileNameStr + "\n";
	outStr += "grid type:"  + sepC + g->gridTypeAsString() + "\n";
	outStr += "# grids included in results:" + sepC + QStr(gridsIncluded);
	outStr += sepC + "(of " + QStr(g->grids) + ")";
	outStr += sepC + "sections:" + sepC + QStr(minZVal+1) + "-" + QStr(maxZVal+1) + "\n";	
	outStr += "# categories included in results:" + sepC + QStr(numCatsOnForAnalysis);
	outStr += sepC + "(of " + QStr(csize) + ")\n";
	outStr += "# total pts included in results:" + sepC + QStr(totPtsInResults);
	outStr += sepC + "(of " + QStr(psize) + ")\n";
	outStr += "\n";
	
	//## NOW OUTPUT ANY SPECIAL MEASUREMENTS REQUESTED:
	
	if( g->allowIntercepts )
	{
		g->calcIntercepts( true,ivwGetModel(plug.view),minZVal,maxZVal );
	}
	
	if( includeSACount )
	{
		outStr += g->printSurfAreaDensityResults( sepC, totPtsInResults,
																						  pixelSize, unitsStr, false );
		outStr += "\n";
	}
	if( includeLenCount )
	{
		outStr += g->printLengthDensityResults( sepC, totPtsInResults,
																					  pixelSize, unitsStr );
		outStr += "\n";
	}
	if( includeNumCount )
	{
		outStr += g->printNumberDensityResults( sepC, pixelSize, unitsStr,
																					  scalePt.z, minZVal, maxZVal );
		outStr += "\n";
	}
	
	
	outStr += g->printResults( false, plug.resultsTranspose, sepC, true,
														 plug.resultsShowStDev );
	
	//## IF CLUSTERED OPTION SET: OUTPUT RESULTS WITHIN EACH RANGE
	
	bool needToRegenOrigResults = false;
	if( !plug.resultsAvgAllGrids && (int)rangesMin.size() > 0 )
	{
		outStr += "\n\nCLUSTERED BREAKDOWN USING GRID RANGES:\n\n";
		
		for( int i=0; i<(int)rangesMin.size() && i<(int)rangesMax.size(); i++ )
		{
			int minZ = rangesMin[i];
			int maxZ = rangesMax[i];
			g->calcResults(false,true,0,minZ,maxZ);		// recaulate results in this range.
			needToRegenOrigResults = true;
			
			outStr += "\n\n> Z RANGE (sections):" + sepC;
			outStr += QStr(minZ+1) + "to" + QStr(maxZ+1) + "\n";
			outStr += g->printResults( false, plug.resultsTranspose, sepC, true,
																 plug.resultsShowStDev );
		}
	}
	
	//## IF GRID BREAKDOWN OPTION SET: SEPERATELY CALCULATE AND PRINT
	//## RESULTS FROM EACH GRID
	
	if( plug.resultsSepGrids )
	{
		outStr += "\n\nGRID-WISE POINT BREAKDOWN:\n\n";
		
		vector< vector<QString> > results;		// a two dimensional vector of strings
																					//  to store the table we want to print
		results.resize( g->grids + 2 );
		
		results[0].push_back( "GRID #(SECTION):" );
		results[0].push_back( "CATEGORY_NAME:" );
		for( int c=1; c<numCatsOnForAnalysis; c++ )
			results[0].push_back( "" );
		
		results[1].push_back( "..." );
		for( int c=0; c<(int)g->catObj.size(); c++ )
			if( g->catObj[c].includeInResults )
				results[1].push_back( g->catObj[c].categoryName );
		
		//## FOR EACH GRID: POPULATE RESULTS INTO A NEW ROW IN THE "results" TABLE
		
		for(int i=0; i<g->grids && i+2<(int)results.size(); i++)
		{
			int z = g->zMin + (i*g->zSpacing);
			g->calcResults(false,true,0,z,z);				// recaculate results for this section only
			needToRegenOrigResults = true;
			
			results[i+2].push_back( QStr(i+1) + "(" + QStr(z+1) + ")" );
			for( int c=0; c<(int)g->catObj.size(); c++ )
				if( g->catObj[c].includeInResults )
					results[i+2].push_back( QStr(g->catObj[c].numPtsOn) );
		}
		
		//## WRITE OUT THE TABLE AS A SINGLE STRING, AND
		//## TRANSPOSE (FLIP X AND Y) IF NECESSARY:
		
		if( plug.resultsTranspose )
		{
			for( int i=0; i<(int)results.size(); i++ )
			{
				for( int j=0; j<(int)results[i].size(); j++)
					outStr += results[i][j] + ((j==results[i].size()-1) ? "" : sepC);
				outStr += "\n";
			}
		}
		else
		{
			int maxCols = numCatsOnForAnalysis + 2;
			for( int j=0; j<maxCols; j++)
			{
				for( int i=0; i<(int)results.size(); i++ )
					if( j < results[i].size() )
						outStr += results[i][j] + ((i==(int)results.size()-1) ? "" : sepC);
				outStr += "\n";
			}
		}
	}
	
	if(needToRegenOrigResults)
		g->calcResults(false,true,&totCatHits,minZVal,maxZVal);		// make sure we have 
																															//  orig values for dialog
	
	
	//## IF DESIRED: OUTPUT A COPY OF THE RESULTS TO CLIPBOARD OR CONSOLE
	
	if( plug.resultsOutputOpt==1 )									// "plugin + clipboard"
	{
		QApplication::clipboard()->setText(outStr);
	}
	else if( plug.resultsOutputOpt==2 )							// "plugin + console"
	{
		cout <<endl<<endl << outStr.toStdString() << endl;
	}
	bool saveFile = (plug.resultsOutputOpt==3);			// "plugin + save file"
	
	
	//## OUTPUT PROGRESS RESULTS IN A CUSTOM DIALOG:
	
	CustomDialog ds("Stereology Results", this, (saveFile) ? BS_NO_YES : BS_OKAY_ONLY );
	
	if( !warningStr.isEmpty() )
	{
		ds.addHtmlLabel  ( warningStr, "Click 'Options > Validate grid' to see more info" );
		ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	}
	
	ds.addLabel   ( "--- TOTAL POINT COUNT: ---", true );
	
	ds.addHtmlLabel(
		  "# points included in results: &nbsp; &nbsp; <b>" + QStr(totPtsInResults) +
		  "</b> &nbsp; &nbsp; &nbsp; (<b>" + QStr(percentPtsOfTotal) + "%</b> of total)",
		  "Show how many of the total points were included in the results." );
	
	
	ds.beginGroupBox( "Category breakdown:", false );
	
	//ds.setStylePrev("background-color: rgb(200, 200, 200);");			// grey
	
	for( int c=0; c<(int)g->catObj.size(); c++ )
	{
		long numPtsOn = g->catObj[c].numPtsOn;
		float fractOfChecked = fDiv( numPtsOn, totPtsInResults );
		float percent = roundDecimals( fractOfChecked*100.0f, 1 );
		QString catName = "[" + QStr(c) + "] " + g->catObj[c].categoryName;
		
		ds.addPercentBar( catName, QStr(percent)+"%", fractOfChecked, 250,
										  g->catObj[c].color, "", QFrame::NoFrame );
	}
	
	ds.endGroupBox();
	
	ds.addLabel   ( "" );
	
	ds.addLabel   ( (plug.resultsOutputOpt==1) ?
								 "--- TABLE COPIED TO CLIPBOARD: ---" :
								 "--- RESULTS TABLE ---" , true,
								 "Belows is the results table in ASCII form \n"
								 "ready to copy and paste into your favourite program" );
	
	ds.addReadOnlyTextEdit ( outStr, false, 120, 
													(plug.resultsOutputOpt==1) ?
													"This has already been copied (to your clipboard).\n"
													"Use [ctrl]+[v] to paste into a spreadsheet or text editor." :
													"Select to copy and paste"  );
	
	if( saveFile )
	{
		ds.addHtmlLabel  ( "Would you like to save this to file?",
											 "If you 'Yes' below, a dialog will open to save the \n"
											 "text above into a comma seperated value (csv) file." );
		ds.setStylePrev("background-color: rgb(0, 255, 0);");				// green
	}
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	
	//## IF SPECIFIED, SHOW A SAVE DIALOG TO SAVE RESULTS TO A .CSV FILE:
	
	if( saveFile )
	{
		QString defName = fileNameStr + "_stereology_results.csv";		// suggested file name
		QString filePath = QFileDialog::getSaveFileName( plug.window,"Save Results",
																								     defName,"CSV file (*.csv)") ;
		if ( filePath==NULL )
			return;
		
		ofstream out_file( filePath.toLatin1() );                //open/create text file
		if (out_file.fail())
		{
			QMessageBox::critical(this,"ERROR","ERROR: Could not create and/or open file");
		}
		else
		{
			out_file << outStr.replace(sepC,",").toStdString();
			out_file.close();                    //close text file
		}
	}
	
}


//------------------------
//-- Brings up a dialog where the user can change default values used
//-- to generate new grid

void Stereology::gridSettings()
{	
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("More Settings", this);
	
	ds.addLabel   ( "--- DEFAULT SETTINGS FOR NEW GRID ---", true );
	
	ds.addCheckBox( "keep square (keep y spacing same as x spacing)", 
								 &plug.sameXYSpacing,
								 "If true: then by default, the 'keep square' box will be ticked \n"
								 "and the x and y spacing will be kept the same." );
	ds.addCheckBox( "project grid on every slice", 
								 &plug.showGridEverySlice,
								 "If true: the same grid will be projected/shown on EVERY section \n"
								 "... even those outside the z limits This option is useful if you \n"
								 "want to do stereology by counting points on paper rather than using \n"
								 "the plugin to mark and record the classification of each point \n"
								 "(and let the plugin count for you). Projecting a the same grid \n"
								 "accross all section is takes a lot less overhead (fewer lines are \n"
								 "drawn) - so a good idea if you have very dense points.... but if \n"
								 "you go ahead and start counting points this option is set false \n"
								 "and automatically disappears."
								 "If false: the grid will be applied to certain sections only, \n"
								 "as defined by 'z spacing' and the min and max limits for z. \n" );
	ds.addCheckBox( "change grid limits", 
								 &plug.changeDefLimits,
								 "If true: then by default, grid limits will be applied \n"
								 "when the plugin first loads." );
	
	ds.addDblSpinBoxF("default X spacing:", 0.01, 5000, &plug.defSpacingX, 2, 0.01,
                    "The default value for x spacing (when the plugin loads).");
	ds.addDblSpinBoxF("default Y spacing:", 0.01, 5000, &plug.defSpacingY, 2, 0.01,
                    "The default value for y spacing (when the plugin loads).");
	ds.addSpinBox ( "default Z spacing:", 1, 5000, &plug.defSpacingZ, 1,
								 "The default value for z spacing (when the plugin loads)." );
	
	ds.addSpinBox ( "default inset when applying grid limits:", 1, 5000,
								 &plug.limitDefaultInset, 1,
								 "The default inset to apply to x and y grid limits\n"
								 "when the model loads" );
	
	ds.addCheckBox( "show the option to change default grid\n"
								  "settings when I close this plugin",
								  &plug.showChangeDefOpt,
								  "NOTE: If left off, then a yes/no dialog appears "
								  "when you close the Stereology plugin and/or 3dmod "
								  "if your current x,y or z spacing is different from "
								  "the default values above." );
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "--- CATEGORY SELECTION SETTINGS ---", true );
	
  ds.addCheckBox( "auto progress points", 
								 &plug.autoProgress,
								 "If true, IMOD will automatically jump to the next (unchecked)\n"
								 "point whenever you select a category (by pressing the shortcut\n"
								 "number keys or pushing the toggle button). \n\n"
								 "Auto progress doesn't apply when the 'allow multiple categories'\n"
								 "option is on however, since you may need to turn on multilpe\n"
								 "categories for each point.");
	
	ds.addCheckBox( "center ZAP on each point", 
								 &plug.centerOnEachPt,
								 "If true, the ZAP window will move so the current stereology point\n"
								 "is in the center of the screen as you progress through points.");
	
	ds.addLineEditF( "estimated speed", 0.01f, 10000.0f,
									&plug.secsPerPt, 6,
									"This value represents the approximate number of seconds <br>"
									"it takes you to classify/check each point, and is used by <br>"
									"'<b>Options > Progress' to help estimate how much time <br>"
									"you have left to finish your grid(s). <br>"
									"By default this value is set to '3 secs/point' (~1200 points/hr),<br>"
									"but unless you have complex images you'll probably average<br>"
									"<b>faster</b> (eg: 2 secs/point)..... especially if you master<br>"
									"the '<b>Pt Paint</b>' tool and can make good use of options<br>"
									"like 'Change point values' and 'Apply mask to points'.<br>"
									"<br>"
									"<i>DEFAULT VALUE: 3 seconds/point</i>", " secs/point");
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "--- OTHER SETTINGS ---", true );
	
	ds.addCheckBox( "show loading info in console", 
								 &plug.showLoadingInConsole,
								 "If true, information will be printed out to the \n"
								 "console when a grids are loaded from IMOD models... \n"
								 "even if there's no error.");
	
	ds.addCheckBox( "jump to grid on update",
								 &plug.jumpToGridOnUpdate,
								 "If true, then when the plugin first loads and/or you \n"
								 "make changes to a grid's setting, it will automatically \n"
								 "make sure your ZAP changes to see a section with a grid on \n"
								 "it. If false, you may sometimes see a grid which the section \n"
								 "is not applied to (depending on the z spacing).");
	
	ds.addCheckBox( "add random symbols to new objects",
								 &plug.randomSymbols,
								 "If true, then instead of all objects getting a sphere size \n"
								 "newly created objects (by this plugin) will be given a \n"
								 "random symbols shape - a circle, square or triangle - which \n"
								 "will appear a fixed size regardless of zoom.");
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
	
	//## CHECK NEW GRID SETTINGS ARE VALID:
	
	if( plug.secsPerPt <= 0.0f )
		plug.secsPerPt = 3.0f;
}


//------------------------
//-- Brings up a dialog where the user can change grid display/appearance
//-- options..... plugin values/settings which do not fit well onto the
//-- main plugin interface.

void Stereology::displayOptions()
{
	QColor gridObjColor    = QColor( plug.gridColorR, plug.gridColorG, plug.gridColorB );
	QColor gridObjColorNew = gridObjColor;
	
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("Display Options", this);
	
	ds.addLabel   ( "--- GRID LINE APPEARANCE ---", true );
	
  ds.addCheckBox( "show grid in model view", 
								 &plug.showGridInModelView,
								 "Will show the grid in the Model View. \n"
								 "WARNING: This may reduce performance, but \n"
								 "   only while the Model View window is open.");
	
	ds.addComboBox( "grid display:",
								 "lines|"
								 "crosshairs|"
								 "arrows|"
								 "offset pts|"
								 "---|"
								 "random pts|"
								 "vert lines|"
								 "horz lines|"
								 "up diagonal|"
								 "down diagonal|"
								 "line pairs|"
								 "cycloids|"
								 "cycloids alt|"
								 "cycloids long|"
								 "---|"
								 "rectangles 1:2|"
								 "forbidden sq 1:2|"
								 "forbidden sq 1:4|"
								 "forbidden sq 1:8|"
								 "none", &plug.gridDisplayOpt,
								 "Dictates how the grid is displayed\n\n"
								 "TIP: While classifying points we recommend 'crosshairs' \n"
								 "but in cases where there are a huge number of points \n"
								 "the 'lines' options makes for quicker rendering. Once \n"
								 "you've started counting points you cannot easily change \n"
								 "the grid type, but you can use this to change how it's \n"
								 "DISPLAYED." );
	
	ds.addSpinBox ( "grid symbol size", 1, 2000,
								 &plug.gridSymbolSize, 1,
								 "The size (in pixel) of the grid points if 'grid setting'\n"
								 "is set to either 'crosshairs' or 'arrows'.");
	
	ds.addColorSel( "grid color", &gridObjColorNew,
								 "The color to use for the grid lines" );
	
	ds.addSpinBox ( "grid line thickness", 1, 50,
								 &plug.gridLineThickness, 1,
								 "The width (in pixel) of the grid lines. In most cases we\n"
								 "recommend you set this to 1, but on certain screens pixels\n"
								 "are so small you may need to bump this to 2 to see the lines!");
	
	ds.addComboBox( "grid labels:",
								 "off|"
								 "numbers (1,1)|"
								 "traditional (a,1)",	 &plug.gridLabels,
								 "Options for presenting labels for the grid lines \n"
								 "along X and Y. If on, labels will appear along the \n"
								 "top and bottom of the grid limits." );
	
	ds.addLabel   ( "" );
	ds.addLabel   ( "--- OTHER DISPLAY OPTIONS ---", true );
	
	ds.addComboBox( "selected point display:",
								 "lines|"
								 "crosshair|"
								 "arrows|"
								 "off|"
								 "vert line|"
								 "horz line",	 &plug.selPtDisplay,
								 "Dictates how the currently selected stereology point is highlighed. \n"
								 "By default it this is set to the 'arrows' option whereby the \n"
								 "selected point is highlighed with four nice big yellow arrows \n"
								 "pointing to it." );
	
	ds.addCheckBox( "black out visited points", 
								 &plug.blackVisitedPts,
								 "Will draw black lines over any point which\n"
								 "has been checked.");
	
	ds.addLineEditF ( "Don't show this warning unless\n"
									 "contours numbers exceed", 1000.0f, FLOAT_MAX,
									 &plug.maxContsRend, 0,
									 "Allows you to chose the maximum number of contours \n"
									 "which can be used to render grid lines before a warning \n"
									 "message appears which lets you decide if you want to \n"
									 "render this many lines. Drawing too many contours on \n"
									 "many contours on screen at once can crash the machine \n"
									 "and also it would be quite silly to attempt to classify \n"
									 "such huge number of points." );
	
	ds.addCheckBox( "show big crosshair for 'Intercept' tool", 
								  &plug.showBigIntercCH,
								  "Will draw long dotted lines when you are using the \n"
								  "'intercept' tool to mark where surfaces intersect \n"
								  "lines and can help you see the snap action better.");
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
	
	//## APPLY ANY NECESSARY CHANGES TO GRID APPEARANCE (LINE THICKNESS, COLOR, ETC):
	
	if( gridObjColorNew != gridObjColor )
	{
		plug.gridColorR = gridObjColorNew.red();
		plug.gridColorG = gridObjColorNew.green();
		plug.gridColorB = gridObjColorNew.blue();
		colGridColor->setColor( gridObjColorNew );
	}
	
	//if(cmbGridDisplayOpt->currentIndex() != plug.gridDisplayOpt)
	//	cmbGridDisplayOpt->setCurrentIndex( plug.gridDisplayOpt );
	
	if(spnGridSymbolSize->value() != plug.gridSymbolSize)
		spnGridSymbolSize->setValue( plug.gridSymbolSize );
	if( plug.maxContsRend < 1000 )
		plug.maxContsRend = 1000;
	
	drawGridObject     ( false );
	drawSubsampleRects ( false );
	drawSelObject      ( false );
	drawBlackObject    ( true );
}



//------------------------
//-- Callback for the buttons at the bottom

void Stereology::buttonPressed(int which)
{
  if      (which==0)
    close();
  else if (which==1)
		openUrl( "http://www.slashsegmentation.com/tools/imod/stereology-plugin" );
	else if (which==2)
    helpPluginHelp();
}


//############################################################
//## PROTECTED SLOTS:


//------------------------
//-- Displays a (html) help page with information about the plugin

void Stereology::helpPluginHelp()
{
  imodShowHelpPage("../plughelp/stereology.html#TOP");
}

//------------------------
//-- Displays a html help page with information about "Stereology" (and what it is)

void Stereology::helpNamingHelp()
{
  imodShowHelpPage("../plughelp/stereology_help.html#TOP");
}

//------------------------
//-- Window closing event handler - removes this pluging from the imod dialog manager

void Stereology::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)plug.window);
  ivwFreeExtraObject(plug.view, plug.extraObjGrid);
	ivwFreeExtraObject(plug.view, plug.extraObjRect1);
  ivwFreeExtraObject(plug.view, plug.extraObjRect2);
	ivwFreeExtraObject(plug.view, plug.extraObjSel);
	ivwFreeExtraObject(plug.view, plug.extraObjBlack);
  ivwFreeExtraObject(plug.view, plug.extraObjExtra);
  ivwTrackMouseForPlugs(plug.view, 0);
  
  plug.window->saveSettings();
  
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  
  plug.view = NULL;
  plug.window = NULL;
  e->accept();
}


//------------------------
//-- Key press event handler - closes on escape or passes on event to "ivwControlKey"

void Stereology::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

//------------------------
//-- Key release event hander - passes on event to "ivwControlKey"

void Stereology::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}
























//----------------------------------------------------------------------------
//
//          EDITING FUNCTIONS:
//
//----------------------------------------------------------------------------



//------------------------
//-- Gets the slice value of the top Zap window or returns -1 if no Zap

int edit_getZOfTopZap()
{
  int currSlice = -1;
  int noZap = ivwGetTopZapZslice(plug.view, &currSlice);   // gets current slice
  if (noZap == 1)   // if no top ZAP window:
    return (-1);
  return (currSlice);
}


//------------------------
//-- Sets the top ZAP window to focus on the selected point and slice.

int edit_setZapLocation( float x, int y, int z, bool redraw )
{
  ivwSetLocation( plug.view, x, y, z );
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  return z;
}



//------------------------
//-- Adds a new contour to the specified object

int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo )
{
  Icont *newCont = imodContourDup( cont );    // malloc new contour and don't delele it
  int numConts = csize(obj);
  if(enableUndo)
    undoContourAdditionCO( plug.view, numConts );    // REGISTER UNDO
  int newContPos = imodObjectAddContour( obj, newCont );
  free(newCont);
  return newContPos;
}

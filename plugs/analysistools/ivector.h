#ifndef INC_IVECTOR_H
#define INC_IVECTOR_H

//############################################################

#include "_common_functions.h"
#include "imodplugin.h"
#include "icontextra.h"
#include <vector>
using namespace std;

//############################################################

struct Ivector
{
//## DATA:
	
	Ipoint ptS;           // the start point of the vector     ( ptS  = ptE - vector )
	Ipoint ptE;           // the end point of the vector       ( ptE  = ptS + vector )
	Ipoint vector;        // displacement of end point relative to start point 
                        //                                   ( vector = ptE - ptS )
	
//## METHODS:
	
	Ivector();                                            //-- Default constructor
	Ivector( Ipoint startPt, Ipoint endPt );              //-- Default constructor
	Ivector( Ipoint startPt );                            //-- Default constructor (sets start and end points the same)
  Ivector( float startX, float startY, float startZ );	//-- Default constructor (sets start and end points the same)
	
	void reset();                                             //-- Resets all point values to zero
	void setStartAndEndPtsSame( float x, float y, float z );	//-- Sets the start and end points to the given value
  void setStartPtOnly( Ipoint startPt );                    //-- Sets the start point only (without chaging the other values)
	void setEndPt( Ipoint endPt );                            //-- Sets the end point value (and updates "vector")
	void setVector( Ipoint v );                               //-- Sets the "vector" value (and updates the end point)
	void recalcVector();                                      //-- Recalculates "vector" based on position of start and end points
	int  getZVal();                                           //-- Returns the Z value of the start point
}; 

//ostream& operator<< (ostream &os, const Ivector i);				//-- Outputs the values of "pt", "ptEnd" and "vector"
bool operator< (const Ivector &a, const Ivector &b);        //-- Less than operation based on the vectors' Z values

//############################################################

struct SimpleTransform
{
//## DATA:
	
	double scaleX;        // the stretch factor along X  (where 1.0 = no change)
	double scaleY;        // the stretch factor along Y  (where 1.0 = no change)
	double rotate;        // the rotation angle in DEGREES
                        //  (measured anti-clockwise from a ray horizontally left)
	double translateX;		// the move/translation distance along X
	double translateY;		// the move/translation distance along Y
	
//## METHODS:
	
	SimpleTransform();                                                //-- Default constructor
	void reset();                                                     //-- Resets all values to zero
	void adjustPtByTransform (Ipoint *pt, Ipoint *rotatePt);					//-- Takes a point and changes it by the tranform by using: (1)scale, (2)rotate, (3)translate.
	void adjustPtByTransformInverse (Ipoint *pt, Ipoint *rotatePt);		//-- Takes a point and changes it by the INVERSE of the tranform by using: (1)-translate, (2)-rotate, (3)-scale.
	
	void adjustPtByTransformMinusScale (Ipoint *pt, Ipoint *rotatePt);            //-- Takes a point and changes it by the tranform by using: (1)scale, (2)rotate, (3)translate.
	void adjustPtByTransformInverseMinusScale (Ipoint *pt, Ipoint *rotatePt);			//-- Takes a point and changes it by the tranform by using: (1)scale, (2)rotate, (3)translate.
	
	void addTransformsIgnoreScale (SimpleTransform t);
	

	void calculateBestFitTransformUsingCorners( Ivector vectBL, Ivector vectTL, Ivector vectTR, Ivector vectBR );		//-- Determines the transform values to fit a deformed square/rectangle //-- NOTE: The final rectange won't necesary have right angled corners, hence edge values are averaged to produce a "best fit".
};

ostream& operator<< (ostream &os, const SimpleTransform t);				//-- Outputs transform values

//############################################################

#endif
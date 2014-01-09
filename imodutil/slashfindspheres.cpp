/*
 *  slashfindspheres.cpp -- used to find likely matches for some input spheres.
 *  
 *  Author:     Andrew Noske
 *  Revised by: David Mastronarde (once finished)   email: mast@colorado.edu
 *  
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *  
 * $Id: slashfindspheres.cpp
 */

//############################################################

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "iimage.h"
#include "sliceproc.h"
#include "imodel.h"

#include <algorithm>
#include <vector>
#include <list>
#include <limits.h>
#include <math.h>

using namespace std;


//############################################################
//## STRUCTURE DECLARATIONS:

class Sphere;
class RingValue;
class ConcentricRings;
class PixTemplate;
class Filter;
class LoadedSlice;
class SliceBuffer;


//############################################################
//## CONSTANTS:

enum weightstrategy	{	WS_NONE, WS_FALLOFF, WS_HALFDOWN }; 
									// the strategy used to calculate weights based on distance from center
enum filtertype     { FT_MEDIAN, FT_SOBEL, FT_PREWITT, FT_SCALE, FT_THRESHOLD };
									// types of filters which can be applied to slices as they are loaded

const float FLOAT_MAX		  = (float) 3.40282e38;			// largest positive value float
const float FLOAT_MIN_POS	= (float) 1.17549e-38;		// smallest positive value float
const float FLOAT_MIN		  = (float)-3.4027e35;			// largest negative value float

const int MAX_CHARS       = 4096;				// number of characters in file path and labels

const float DEFAULT       = -1.0f;			// |-- starting value for certain input values
const int   DEFAULT_INT   = -1;					// |   so we can tell if user has changed them

const float DEFAULT_CUTOFF = 0.85f;		// default cutoff range to use if none specified by
																			//  user and not enough spheres were compared
const int MAX_COMPARES = 500;		      // max number of "cross comparions" between the 
                                      //  templates around input spheres in order
                                      //  to generate a estimated "good cutoff"

const float NULL_FLOAT = -999; //MIN_FLOAT;	// represents a pixel not on the image
	

//############################################################
//## MAIN PROGRAM VARIABLES:

static vector<int> objList;				// objects containing spheres/scattered points to
                                  //  use as input for finding new spheres
static vector<int> extraObjList;	// a list of "extra" scattered points objects which we
                                  //  don't want out output spheres to overlap

static float gutterXY = 4.0f;			// number of pixels to extend each radius for template
static float gutterZ  = DEFAULT;	// number of pixels to extend each radius in Z
static float cutoff   = DEFAULT;	// cut off for cross-correlation (where 1=perfect)
static float valRange = DEFAULT;  // represents a range in pixel values by which to scale
																	//  cross-correlation, if left as -1 this value gets
																	//  set using the min and max pixel values in the image

static float histFirstBin = 4.0f;	// the bin size for the first histogram ring
static float histBinSize  = 2.0f;	// the bin size/radius increase for ring values

static int   maxSpan       = 30;		// maximum allowed span for vesicle
static int   maxSpheresOut = 10000;	// maximum number of spheres written out 

static bool  ignoreZ    = false;	// if true, then analyzes 2D only and gutter z ignored
static bool  allowDup   = false;	// if true, then allows output spheres to overlap/
                                  //  duplicate input spheres
static int   splitLevel = 5;			// the number of new objects to split values over
static int   origColor  = false;	// if true, uses same color as existing objects
                                  //  instead of heatmap colors
static int   multiPts   = false;	// if true, each new object will get only a single
                                  //  contour with all points, versus 1 point per contour

static int   Z_BUFFER = 30;				// maximum number of slices in memory at once

static int   writeImages = 0;			// if 1: then will output an MRC with filtering applied
																	//  if 2: also outputs MRC shoing all cutoff values
static int   printLevel  = 2;			// amount of information we want to print out between
																	//  0 and 5 where 0=no info, 1=basic and 5=huge

static float testRadius = 3.0f;		// "test radius" use to do "early rejection" of any
                                  //  pixel that has values inside "testRadius"
                                  //  on the same slice fall outside expected values

//############################################################
//## FUNCTION DECLARATIONS:

void populateTemplate( PixTemplate &pxT, Sphere sp, MrcHeader &inHead,
											float gutterInXY, float gutterInZ,
											bool setWeights, float zScale );
int getMinMaxAndAvgConfidence( vector<Sphere> &sp, float &minC,float &maxC,float &avgC, 
															 float minToInclude=0.0f, bool ignoreOneAndAbove=true );
int addImodObjectSpheresToVector( Imod *imod, int objIdx, vector<Sphere> &sphere,
																 bool printErrs, bool mustBeInImage, MrcHeader &inHead, 
																 int gutterInXY, int gutterInZ, float zScale );
int addSpheresToModel( Imod *imod, vector<Sphere> &sphere, int numNewObjs,
											 float absMinCutoff, int origObjIdx, bool addDefPrefix,
											 bool scaleMaxCutoff, bool scaleMinCutoff, bool useObjColor,
											 bool multiPtsPerCont );
float getAvgValInSquareArea( MrcHeader &inHead, int x, int y, int z, float sideLength );

bool compareSphereSimilarity (const Sphere &a, const Sphere &b);
int  getMaxNumRings( const vector<ConcentricRings> &hist );
void calcAvgOfHistograms( const vector<ConcentricRings> &hist,
												  ConcentricRings &avgHist, bool useCurrentNumberOfRings );
bool deleteOverlappingSpheres( vector<Sphere> &sphere, float zScale,
                               vector<Sphere> &extraSphere, bool useExtraSpheres,
                               bool markForDeleteOnly );


inline float calcCorrelation( float val1, float val2, float valRange );
inline float calcDist3D( float x1, float y1, float z1,
												 float x2, float y2, float z2, float zScale );
inline float calcWeight( float dist, float radius, float gutter,
												 float centerW=1.0f, float innerW=1.0f, float outerW=1.0f,
												 weightstrategy wtStrategy=WS_NONE, float falloffRate=1.0f );
inline Iobj* getAndSetObj( Imod *imod, int objIdx );

inline bool isDigit( char chr );
inline int  getDigitVal( char chr );
inline bool keepInLimits( int &value, int min, int max );

inline int processIntList( char *arg, vector<int> &vect, int pos=0,
													bool allowNeg=true, bool allowRange=false, 
													char sepCh1=',', char sepCh2=';', bool stopOnOtherChars=true );
inline int processTwoPositiveInts( char *arg, int &int1, int &int2 );
inline int processPositiveIntList( char *arg, vector<int> &vect, bool acceptRange );
inline int processFilterList( char *arg, vector<Filter> &filt );


bool writeOutMrcFileAfterFilters( MrcHeader &inHead, char outfile[MAX_CHARS],
																  vector<Filter> &filt);
bool writeOutMrcFileWithCutoffVals( MrcHeader &inHead, char outfile[MAX_CHARS],
																	  vector<Filter> &filt, int zMin, int zMax,
																	  int maxXYRad, int maxZRad, float zScale );

//############################################################
//## STRUCTURES:

//******************************
//** Stores a single sphere with a center (x,y,z), radius (r) and an association
//** to an object index (objIdx).

class Sphere
{
public:
	float x;		// |
	float y;		// |-- x,y,z coordinates of center
	float z;		// |
	float r;						// radius
	
	float confidence;		// confidence of match (1=perfect, 0=all pixels way off)
	int objIdx;					// identifies the object this sphere comes from or is matched to
	
	Sphere()
	{
		set( -1, -1, -1, -1, -1 );
	}
	
	Sphere( float xCenter, float yCenter, float zCenter, float radius, int objectIdx )
	{
		set( xCenter, yCenter, zCenter, radius, objectIdx );
	}
	
	void set( float xCenter, float yCenter, float zCenter, float radius, int objectIdx )
	{
		x = xCenter;
		y = yCenter;
		z = zCenter;
		r = radius;
		objIdx = objIdx = objectIdx;
	}
	
	bool operator< (const Sphere &rhs) const
	{
		if( z != rhs.z )			return ( z < rhs.z );
		if( y != rhs.y )			return ( y < rhs.y );
		if( x != rhs.x )			return ( x < rhs.x );
		return false;
	}
	
	void print()
	{
		printf("(%2.0f, %2.0f, %2.0f, r=%2.2f, c=%4.4f) \n",x,y,z,r,confidence);
	}
};


//******************************
//** Stores a number of intensity measurements (mean, min, max, stDev, etc)
//** representing the pixel values within a fixed 3D area between
//** "innerRadius" and "outerRadius" of a central point

class RingValue
{
public:
	float innerRadius;		// the inner radius of this concentric sphere
	float outerRadius;		// the outer radius of this concentric sphere
	
	float numPx;					// the number of pixels in this range
	
	float meanVal;				// the average pixel value in this range 
	float minVal;					// the minimum """""""""""""""""""""""""
	float maxVal;					// the maximum """""""""""""""""""""""""
	float stDev;					// the standard deviaton of pixelsi this range
	
	float minAllowedMean;	// the minimum mean value allowed to be a candidate "match"
	float maxAllowedMean;	// the maximum mean value allowed to be a candidate "match"
	
	float binWeight;			// the weight to apply to this concentric sphere
	
	RingValue()	{	reset(); }
	
	void reset()
	{
		innerRadius = outerRadius = numPx = meanVal = 0.0f;
		minAllowedMean = minVal = FLOAT_MAX;
		maxAllowedMean = maxVal = FLOAT_MIN;
		stDev  = -1.0f;
		binWeight = 1.0f;
	}
	
	bool calMinAndMaxAllowedMean()
	{
		minAllowedMean = meanVal - stDev;
		maxAllowedMean = meanVal + stDev;
		return (stDev>=0.0f);
	}

};


//******************************
//** Stores a group of concentric RingValue's representing rings
//** of increasing radius from a fixed point, which each
//** RingValue storing intensity value for pixels inside that
//** non-overlapping region.

class ConcentricRings
{
public:
	vector<RingValue> ring;				// a series of concentric rings with increasing size
	
	float ringFirstBinSize;				// the radius of the first ring
	float ringBinSize;						// the increase of every radius after that
	float sphereRad;							// the radius of the sphere representing the
																//  "edge" of the compartment (not extended radius)
	float maxRad;									// the radius beyond which we don't care about pixels
	
	RingValue *getRing( int idx )
	{
		if( idx < 0 || idx >= ring.size() )
			return NULL;
		return &ring[idx];
	}
	
	RingValue *getRingByDistance( float dist, float incFirstRingSize=0.0f )
	{
		if( ring.size()==0 )
			return NULL;
		
		float distBeyondFirstRing = dist - (ring[0].outerRadius + incFirstRingSize);
		if( distBeyondFirstRing <= 0 )
			return &ring[0];
		
		int predIdx = (distBeyondFirstRing / ringBinSize) + 1;
		if( predIdx >= 0 && predIdx < ring.size() )
			return &ring[predIdx];
		
		return NULL;
	}
	
	int setupRings( float binSize, float firstBinSize, float sphereRadius,
								  float maxRadius )
	{
		if( binSize <= 0 || firstBinSize < 0 )
			return -1;
		
		ringFirstBinSize = firstBinSize;
		ringBinSize      = binSize;
		sphereRad        = sphereRadius;
		maxRad           = maxRadius;
		
		int numBins = ceil((maxRad - firstBinSize) / binSize) + 1;
		ring.resize( numBins );
		
		ring[0].innerRadius = 0.0f;
		ring[0].outerRadius = firstBinSize;
		for(int i=1; i<ring.size(); i++)
		{
			ring[i].innerRadius = (i-1)*binSize + firstBinSize;
			ring[i].outerRadius = ring[i].innerRadius + binSize;
		}
		
		return numBins;
	}
	
	bool setupWeights( float firstBinWeight=1.0f, float innerWeight=1.0f,
										 weightstrategy wtStrategy=WS_FALLOFF )
	{
		if( ring.size()==0 )
			return false;
		
		float gutter = maxRad - sphereRad;
		
		ring[0].binWeight = firstBinWeight;
		for(int i=1; i<ring.size(); i++)
		{
			float avgRad = (ring[i].outerRadius + ring[i].outerRadius) / 2.0f;
			ring[i].binWeight = calcWeight( avgRad, sphereRad, gutter,
												 	            innerWeight, innerWeight, 1.0f,
												              wtStrategy, 1.0f );
		}
		return true;
	}
	
	void setupWeightsSameValue( float newWeight=1.0f )
	{
		for(int i=1; i<ring.size(); i++)
			ring[i].binWeight = newWeight;
	}
	
	void print( float valueRange=255, float minValue=0, bool showStDev=false,
						  bool showAllVals=false )
	{
		static const float CHAR_WIDTH = 80;
		printf("Num rings: %d\n", ring.size() );
		
		for(int i=0; i<ring.size(); i++)
		{
			int meanBar  = ((ring[i].meanVal - minValue) / valueRange) * CHAR_WIDTH;
			int stDevBar = ((ring[i].stDev   - minValue) / valueRange) * CHAR_WIDTH * 2.0f;
			printf(" %2.0f-%2.0f mean=%3.1f stDev=%4.2f numPx=%2.0f     \t|",
						 ring[i].innerRadius,ring[i].outerRadius,
						 ring[i].meanVal, ring[i].stDev, ring[i].numPx );
			for(int j=0; j<meanBar;  j++)	printf("=");
			if(showStDev)
				for(int k=0; k<stDevBar; k++)	printf(".");
			printf("|");
			if(sphereRad > ring[i].innerRadius && sphereRad <= ring[i].outerRadius )
				printf("   <** radius=%0.2f", sphereRad);
			printf("\n");
			if(showAllVals)
			{
				int minBar  = ((ring[i].minVal - minValue      ) / valueRange) * CHAR_WIDTH;
				int maxBar  = ((ring[i].maxVal - ring[i].minVal) / valueRange) * CHAR_WIDTH;
				printf("min/max=%2.0f-%2.0f allowedM=%1.0f-%1.0f binW=%1.2f\t|",
							 ring[i].minVal,ring[i].maxVal,
							 ring[i].minAllowedMean,ring[i].maxAllowedMean,ring[i].binWeight );
				for(int j=0; j<minBar; j++) printf(" ");
				printf("^");
				for(int j=0; j<maxBar; j++) printf(" ");
				printf("^\n");
			}
		}
	}
	
	void getMinMaxAllowedInRange( float& minOut, float &maxOut,
															  float radStart, float radEnd )
	{
		minOut = maxOut = 0.0f;
		int totNumPx = 0;
		for( int i=0; i<ring.size(); i++)
		{
			if( (ring[i].innerRadius < radStart && ring[i].outerRadius < radStart)
			 || (ring[i].innerRadius > radEnd   && ring[i].outerRadius > radStart) )
				continue;
			minOut   += (ring[i].minAllowedMean * ring[i].numPx);
			maxOut   += (ring[i].maxAllowedMean * ring[i].numPx);
			totNumPx += ring[i].numPx;
		}
		if(totNumPx)
		{
			minOut /= totNumPx;
			maxOut /= totNumPx;
		}
	}
};

//******************************
//** PixTemplate represents a 3D region of pixels and pixel weights (for each pixel)
//** which are stored and can be compared against PixTemplate instances.
//** In addition to the vector of pixel values and size, PixTemplate also stores
//** a sphere (center) which represents the radius and center of the image in the
//** pixels, plus a second sphere (sphereOrig) representing the sphere's position
//** in the origional volume. When two PixTemplates are compared uwing
//** "calcCorrelationOn", they are both aligned using their centers.

class PixTemplate
{
public:
	vector<float> px;				// stores a 3D cluster of pixel values
	vector<float> weight;		// stores a weight to be applied to each pixel where
	                        //  a weight of 0 is ignored
	
	float totalWeight;			// the sum of all values in the weight vector
	
	Sphere center;					// stores the center of a sphere relative to the template
	Sphere origSphere;			// keeps a record of the sphere's position in the image
	
	ConcentricRings ringHist;		// stores a series of concentric rings which radiate
															//  from the center of this sphere/template
	
	int nx;		// |-- the size of the template along each dimension
	int ny;		// |   
	int nz;		// |
	
	int totXY;  // the number of pixels on each z level (stored for faster calculation)
	int totPx;  // the total nubmer of pixels in the template
	
	PixTemplate()			{  reset(); }
	
	void reset()
	{
		totalWeight = 0.0f;
		nx = ny = nz = 0;
		totXY = totPx = 0;
	}
	
	void setSize( const int sizeX, const int sizeY, const int sizeZ )
	{
		nx = sizeX;
		ny = sizeY;
		nz = sizeZ;
		totXY = nx * ny;
		totPx = nx * ny * nz;
		
		px.resize(totPx);
	}
	
	inline int getPtIdx( int x, int y, int z )		// returns index to the point at (x,y,z)
	{																							//  relative to the template's corner
		return (z*totXY + y*nx + x);
	}
	
	inline bool isValidPx( int x, int y, int z )	// returns true if the given pixel exists
	{
		return (x<nx && y<ny && z<nx && x>=0 && y>=0 && z>=0);
	}
	
	bool setOrigSphere( Sphere &originalSphere )
	{
		origSphere = originalSphere;
	}
	
	bool setSphere( float centerX, float centerY, float centerZ, float radius, int objIdx )
	{
		center.set(centerX, centerY, centerZ, radius, objIdx);
	}
	
	bool clearPxValues()
	{
		for(int p=0; p<px.size(); p++)
			px[p] = NULL_FLOAT;
	}
	
	bool setWeightsUsingSphere( Sphere sp, float gutter, float zScale, 
													    float innerWeight=1.0f, float falloffRate=1.0f )
	{
		float radius = sp.r;
		
		if( weight.size() != px.size() )
			weight.resize( px.size() );
		
		float fullRad = radius + gutter;
		totalWeight   = 0.0f;
		
		for(int z=0; z<nz; z++)
		for(int y=0; y<ny; y++)
		for(int x=0; x<nx; x++)
		{
			int   idx   = getPtIdx(x,y,z);
			float dist  = calcDist3D( (float)x,(float)y,(float)z, sp.x,sp.y,sp.z, zScale );
			weight[idx] = calcWeight( dist, radius, gutter,
															  1.5f*innerWeight, innerWeight, 1.0f,
															  WS_FALLOFF, falloffRate );
		}
		
		return true;
	}
	
	float setWeightsForValidPixels( float onWeight=1.0f, float offWeight=0.0f )
	{
		if( weight.size() != px.size() )
			weight.resize( px.size() );
		
		float totOn = 0.0f;
		for(int i=0; i<px.size(); i++)	{
			if (px[i] == NULL_FLOAT) {
				weight[i] = offWeight;
			}
			else {
				weight[i] = onWeight;
				totOn++;
			}
		}
		return ( px.size() ) ? 0.0f : totOn / px.size();
	}
	
	
	bool calcHistogramRings( float binSize, float firstBinSize, float zScale,
													 float maxRadius=-1.0f,
													 float firstBinWeight=1.0f, float innerWeight=1.0f )
	{
		if( binSize <= 0 || px.size() <= 0 || px.size() != totPx )		// error checking
			return false;
		
		if( firstBinSize <= 0 )
			firstBinSize = binSize;
		if( maxRadius <= 0 || maxRadius > 0.5f*(float)nx )
			maxRadius = 0.5f*(float)nx;
		
		ringHist.setupRings  ( binSize, firstBinSize, center.r, maxRadius );
		ringHist.setupWeights( firstBinWeight, innerWeight, WS_NONE );
		
		//## CALCULATE AVERAGE, MIN AND MAX:
		
		for(int z=0; z<nz; z++)
		for(int y=0; y<ny; y++)
		for(int x=0; x<nx; x++)
		{
			int   idx   = getPtIdx(x,y,z);
			float dist  = calcDist3D( x,y,z, center.x,center.y,center.z, zScale );
			
			RingValue *spv = ringHist.getRingByDistance( dist );
			if(spv==NULL)
				continue;
			
			float value = px[idx];
			spv->numPx++;
			spv->meanVal += value;												// use this to sum all values
			spv->minVal   = min( spv->minVal, value );
			spv->maxVal   = max( spv->maxVal, value );
		}
		for(int i=0; i<ringHist.ring.size(); i++ )
		{
			RingValue *spv = ringHist.getRing(i);
			if( spv==NULL || spv->numPx <= 0.0f )
				continue;
			spv->meanVal = spv->meanVal / spv->numPx;		// then calculate mean
		}
		
		//## CALCULATE STANDARD DEVIATION:
		
		for(int z=0; z<nz; z++)
		for(int y=0; y<ny; y++)
		for(int x=0; x<nx; x++)
		{
			int   idx   = getPtIdx(x,y,z);
			float dist  = calcDist3D( x,y,z, center.x,center.y,center.z, zScale );
			
			RingValue *spv = ringHist.getRingByDistance( dist );
			if(spv==NULL)
				continue;
			
			float diff   = px[idx] - spv->meanVal;
			spv->stDev  += diff*diff;											// use this to sum difference squared
		}
		for(int i=0; i<ringHist.ring.size(); i++ )
		{
			RingValue *spv = ringHist.getRing(i);
			if( spv==NULL || spv->numPx <= 0.0f )
				continue;
			spv->stDev = sqrt(spv->stDev / spv->numPx);	// then calculate final standard dev
			spv->calMinAndMaxAllowedMean();							// calc "allowed" range for mean which
			                                            //  is the average + and - stdev 
		}
	}
	
	
	
	float calcCorrelationOn( PixTemplate &test, bool multiplyWeights )
	{
		int offX = (int)center.x - (int)test.center.x;	// |-- offset to get from the
		int offY = (int)center.y - (int)test.center.y;	// |   center of the test template to 
		int offZ = (int)center.z - (int)test.center.z;	// |   the center of this template
		
		bool weightsExist     = (weight.size()      == px.size()     );
		bool testWeightsExist = (test.weight.size() == test.px.size());
		
		if(multiplyWeights && !(weightsExist && testWeightsExist) )
			multiplyWeights = false;
		
		long totPixelsCompared  = 0;
		double sumWeights       = 0.0f;
		double sumComparison    = 0.0f;
		
		int minX = max( 0,  offX    );
		int maxX = min( nx, nx-offX );
		int minY = max( 0,  offY    );
		int maxY = min( ny, ny-offY );
		int minZ = max( 0,  offZ    );
		int maxZ = min( nz, nz-offZ );
		
		//printf("calcCorrelationOn -> offX=%d,offY=%d,offZ=%d \n",offX,offY,offZ);		 //%%%%
		//printf(" s.x=%f,%f,%f \n",center.x,center.y,center.z);											 //%%%%
		//printf(" t.x=%f,%f,%f \n",test.center.x,test.center.y,test.center.z);				 //%%%%
		//printf(" min/max X:%d-%d,Y:%d-%d,Z:%d-%d\n",minX,maxX,minY,maxY,minZ,maxZ);	 //%%%%
		//printf(" multiplyWeights=%d\n",multiplyWeights?1:0 );		                     //%%%%
		
		for(int z=minZ; z<maxZ; z++ )
		{
			int tZ = z-offZ;	//if( tZ < 0 || tZ >= test.nz )	continue;
			
			for(int y=minY; y<maxY; y++)
			{
				int tY = y-offY;	//if( tY < 0 || tY >= test.ny )	continue;
				
				for(int x=minX; x<maxX; x++)
				{
					int tX = x-offX;	//if( tX < 0 || tX >= test.nx )	continue;
					
					int idx  = getPtIdx(x,y,z);
					int idxT = test.getPtIdx(tX,tY,tZ);
					
					float pxWeight = 1.0f;
					if( multiplyWeights   ) pxWeight = weight[idx] * test.weight[idxT];
					else if( weightsExist ) pxWeight = weight[idx];
					
					sumWeights      += pxWeight;
					float similarity = calcCorrelation( px[idx], test.px[idxT], valRange );
					sumComparison   += ((similarity*similarity) * pxWeight);
				}
			}
		}
		
		if( sumWeights <= 0.0f )
			return -1.0f;
		
		float finalCorrelation = (float)(sumComparison / sumWeights);
		return (finalCorrelation);
	}
	
	
	void print( bool printWeight=false )
	{
		for(int z=0; z<nz; z++)
		for(int y=0; y<ny; y++)
		for(int x=0; x<nx; x++)
		{
			if(y==0 && x==0)	printf("-------slice %d---------\n", z);
			int idx = getPtIdx(x,y,z);
			if(printWeight)
				(weight[idx]==0) ? printf("-") : printf("%1.2f", weight[idx]);
			else
				(px[idx]==0) ? printf("-") : printf("%1.0f", px[idx]);
			(x==nx-1)	? printf("\n") : printf(",\t");
		}
	}
};


//******************************
//** This structure represents an image filter and the parameters of the filter.
//** See 'filtertype' for the types of filters supported.
//** Within this program the user can enter '-F m3,s,p' to create an ordered
//** vector of filters which will get applied to each image slice.

struct Filter
{
	filtertype filterType;
	int sizeXY;
	float min;
	float max;
	
	Filter()	{  reset();  }
	
	Filter( filtertype filtType, int pixSizeXY )
	{ 
		filterType = filtType;
		sizeXY     = pixSizeXY;
	}
	
	void reset()
	{
		filterType = FT_MEDIAN;
		sizeXY     = 3;
		min        = 0.0f;
		max        = 255.0f;
	}
	
	void print()
	{
		switch(filterType)
		{
			case(FT_MEDIAN):		printf("Median,   size=%d\n", sizeXY);						break;
			case(FT_SOBEL):			printf("Sobel    (edge detection)\n");						break;
			case(FT_PREWITT):		printf("Prewitt  (edge detection)\n");						break;
			case(FT_SCALE):		  printf("Adjust,    min=%d max=%d\n", min, max);		break;
			case(FT_THRESHOLD):	printf("Threshold, min=%d max=%d\n", min, max);		break;
		}
	}
};


//******************************
//** LoadedSlice represents a single slice of an MRC volume which can 
//** be loaded into memory and have a small number of filters applied.

struct LoadedSlice
{
	Islice *sl;				// slice data
	int zVal;					// location of slice in volume
	
	LoadedSlice()
	{
		sl   = NULL;
		zVal = INT_MAX;
	}
	
	~LoadedSlice()
	{
		if(sl != NULL)
			sliceFree(sl);
	}
		
	void clear()
	{
		if(sl != NULL)
			sliceFree(sl);
		sl = NULL;
	}
	
	bool isLoaded()
	{
		return (sl!=NULL);
	}
	
	inline float &px( Islice *s, int x, int y )
	{
		return s->data.f[ y*s->xsize + x ];
	}
	
	inline float &px( int x, int y )
	{
		return sl->data.f[ y*sl->xsize + x ];
	}
	
	inline float &pxS( int x, int y )
	{
		if(x <  0)          x=0;
		if(x >= sl->xsize ) x=sl->xsize-1;
		if(y <  0)          y=0;
		if(y >= sl->ysize ) y=sl->ysize-1;
		return sl->data.f[ y*sl->xsize + x ];
	}
	
	void updateMinMaxAndMean()
	{
		int totalPx = sl->xsize * sl->ysize;
		if(totalPx<=0)	return;
		sl->min  = FLOAT_MAX;
		sl->max  = FLOAT_MIN;
		sl->mean = 0.0f;
		
		for(int i=0; i<totalPx; i++)
    {
			float value = (sl->data.f) ? sl->data.f[i] : sl->data.b[i];
			sl->min   = min( value, sl->min);
			sl->max   = max( value, sl->max);
			sl->mean += value;
		}
		sl->mean /= (float)totalPx;
	}
	
	void runEdgeDetect(int center)
	{
		Islice *sout = sliceCreate(sl->xsize, sl->ysize, SLICE_MODE_FLOAT);
		int xmax = sl->xsize - 1;
		int ymax = sl->ysize - 1;
		
		for(int y=1; y<ymax; y++)
		for(int x=1; x<xmax; x++)
		{
			float sumHorz  =   px(x-1,y-1) + center*px(x,y-1) + px(x+1,y-1)
			                 - px(x-1,y+1) - center*px(x,y+1) - px(x+1,y+1);
			float sumVert  =   px(x+1,y+1) + center*px(x+1,y) + px(x+1,y-1)
			                 - px(x-1,y+1) - center*px(x,y+1) - px(x+1,y+1);
			px(sout,x,y) = sqrt( sumHorz*sumHorz + sumVert*sumVert );
		}
		
		for(int y=1; y<ymax; y++)
		{
			px(sout,0,   y) = px(sout,1,      y);
			px(sout,xmax,y) = px(sout,xmax-1, y);
		}
		for(int x=1; x<xmax; x++)
		{
			px(sout,x,0   ) = px(sout,x, 1     );
			px(sout,x,ymax) = px(sout,x, ymax-1);
		}
		
		int totalPx = sl->xsize * sl->ysize;
		for(int i=0; i<totalPx; i++)
      sl->data.f[i] = sout->data.f[i];
		sliceFree(sout);
	}
	
	void runMedianFilter(int span=3)
	{
		if(span <= 0)
			return;
		
		Islice *sout = sliceCreate(sl->xsize, sl->ysize, SLICE_MODE_FLOAT);
		
		int xspan = span;
		int yspan = span;
		int xhalfspan = (xspan - 1) / 2;
		int yhalfspan = (yspan - 1) / 2;
		
		int numPx = xspan * yspan;		// number of pixels in neighborhood (should be odd)
		int half  = numPx / 2;				// half way point where we'll find median value
		
		vector<float> values;										// stores values in pixel neighborhood
		values.resize( numPx );			
		
		for(int y=0; y<sl->ysize; y++)
		for(int x=0; x<sl->xsize; x++)
		{
			for(int i=0; i<numPx; i++)
			{
				int yoff  = i/xspan - yhalfspan;
				int xoff  = i%xspan - xhalfspan;
				values[i] = pxS(x+xoff, y+yoff);
			}
			sort( values.begin(), values.end() );
			px(sout,x,y) = values[half];
		}
		
		int totalPx = sl->xsize * sl->ysize;
		for(int i=0; i<totalPx; i++)
      sl->data.f[i] = sout->data.f[i];
		sliceFree(sout);
	}
	
	void threshold(float min, float max)
	{
		printf("TODO");
	}
	
	
	void scale(float min, float max)
	{
		printf("TODO");
	}
	
	bool floatToByte( float min, float max )
	{
		float range = max - min;
		if(range<=0)
			return false;
		
		int totalPx = sl->xsize * sl->ysize;
		for(int i=0; i<totalPx; i++)
		{
			float fract = ( sl->data.f[i] - min ) / range;
			if(fract > 1.0f)	fract = 1.0f;
			if(fract < 0.0f)	fract = 0.0f;
      sl->data.b[i] = (fract * 255.0f);
		}
	}
	
	void print()
	{
		printf("slice: %d  \t", zVal);
		if(sl)
			printf("min=%0.1f max=%0.1f mean=%0.2f \n", sl->min, sl->max, sl->mean);
		else
			printf("not loaded\n");
	}
};

//******************************
//** SliceBuffer is a class which can load and unload a fixed number of slices
//** as they are requested. If more than the maximum number of slices allowed
//** in memory (maxSlices) are already in memory and a slice not in memory
//** is requested, it will unload the slice furthest away in distance.
//** Each slice request (getSlice) is also accompanied with a vector of
//** filters which will be applied to that slice if it needs loading.

class SliceBuffer
{
public:
	vector<LoadedSlice> sl;				// an array of slices which acts as a "buffer"
	long totSliceLoads;						// tallies the total number of times a new slice
																//  is loaded into the buffer.
	long totSliceRepeats;					// tallies the number of times slice is requested
																//  that is already laoded in buffer.
	
	
	int maxSlicesInMem;						// the maximum number of slices we're allowed to 
																//  have in memory at once
	int numSlicesLoaded;          // used to keep track of how many slices are loaded
	                              //  and should never go above maxSlicesInMem
	
	SliceBuffer()																 { init();                               }
	SliceBuffer( int numSlices, int maxSlices )	 { init(); setSize(numSlices,maxSlices); }
	
	~SliceBuffer()
	{
		clearAllSlices();
	}
	
	void init()
	{
		totSliceLoads  = totSliceRepeats = 0;
		maxSlicesInMem = numSlicesLoaded = 0;
	}
	
	void setSize( int numSlices, int maxSlicesToKeepInMem )
	{
		sl.resize( numSlices );
		numSlicesLoaded = 0;
		for(int z=0; z<sl.size(); z++) {
			sl[z].zVal       = z;
			numSlicesLoaded += sl[z].isLoaded() ? 1 : 0;
		}
		maxSlicesInMem = maxSlicesToKeepInMem;
	}
	
	void clearAllSlices()
	{
		for(vector<LoadedSlice>::iterator it=sl.begin(); it!=sl.end(); it++)
			it->clear();
		numSlicesLoaded = 0;
	}
	
	LoadedSlice *loadSlice( MrcHeader *inHead, int z, vector<Filter> &filt )
	{
		if(printLevel >= 8)
			printf( "LOADING SLICE %d \n", z );
		
		sl[z].sl   = sliceReadFloat( inHead, z );		// load slice:
		for(int f=0; f<filt.size(); f++)						// for each filter: apply it to slice
		{
			switch( filt[f].filterType )
			{
				case(FT_MEDIAN):	  sl[z].runMedianFilter( filt[f].sizeXY );			break;
				case(FT_SOBEL):		  sl[z].runEdgeDetect( 1 );										break;
				case(FT_PREWITT):	  sl[z].runEdgeDetect( 2 );										break;
				case(FT_SCALE):		  sl[z].scale( filt[f].min, filt[f].max );			break;
				case(FT_THRESHOLD):	sl[z].threshold( filt[f].min, filt[f].max );	break;
			}
		}
		totSliceLoads++;
		numSlicesLoaded++;
	}
	
	LoadedSlice *getSlice( MrcHeader *inHead, int z, vector<Filter> &filt )
	{
		if( sl.size()==0				)	return NULL;		// error checking
		if( z<0 || z>=sl.size() ) return NULL;		// error checking
		
		//## IF SLICE IS ALREADY LOADED: RETURN THAT SLICE
		if( sl[z].sl != NULL ) {
			totSliceRepeats++;
			return &sl[z];
		}
		
		//## IF NOT LOADED AND BEYOND MAX QUOTA MET: FIND AND FREE FURTHEST SLICE
		while ( numSlicesLoaded >= maxSlicesInMem )
		{
			int furthestZ = z;
			for(int i=max(z,(int)sl.size()-z-1); i>0; i--)
			{
				if( z-i >= 0        && sl[z-i].isLoaded() )	{	furthestZ = z-i; break;	}
				if( z+i < sl.size() && sl[z+i].isLoaded() )	{	furthestZ = z+i; break; }
			}
			if(printLevel >= 11)	printf( "UNLOADING SLICE %d \n", furthestZ );
			if(furthestZ==z)			printf( "ERROR: LoadedSlice\n" );		// error checking
			sl[furthestZ].clear();
			numSlicesLoaded--;
		}
		
		//## LOAD NEW SLICE:
		loadSlice( inHead, z, filt );
		return &sl[z];
	}
};

//############################################################

class ColorGradient
{
public:
  struct ColorPoint  // internal class used to store colors at different points in the gradient
  {
    float r,g,b;      // red, green and blue values of our color
    float val;        // the position of our color along the gradient (between 0 and 1)
    ColorPoint(float red, float green, float blue, float value)
      : r(red), g(green), b(blue), val(value) {}
  };
  vector<ColorPoint> color;      // an array of color points in ascending value
  
  //-- default constructor
  ColorGradient()  {  createDefaultHeatMapGradient();  }
  
  //-- places a 5 color heapmap gradient into the "color" vector
  void createDefaultHeatMapGradient()
  {
    color.clear();
    color.push_back( ColorPoint(0, 0, 1,   0.0f ) );      // blue
    color.push_back( ColorPoint(0, 1, 1,   0.25f) );      // cyan
    color.push_back( ColorPoint(0, 1, 0,   0.5f ) );      // green
    color.push_back( ColorPoint(1, 1, 0,   0.75f) );      // yellow
    color.push_back( ColorPoint(1, 0, 0,   1.0f ) );      // red
  }
  
  //-- inputs a (value) between 0 and 1 and outputs the (red), (green) and (blue)
  //-- values representing that position in the gradient. Returns 
  void getColorAtValue( const float value, float &red, float &green, float &blue )
  {
    if(color.size()==0)
      return;
    
    for(int i=0; i<color.size(); i++)
    {
      ColorPoint &currC = color[i];
      if( value < currC.val )
      {
        ColorPoint &prevC  = color[ max(0,i-1) ];
        float valueDiff    = (prevC.val - currC.val);
        float fractBetween = (valueDiff==0) ? 0 : (value - currC.val) / valueDiff;
        red   = (prevC.r - currC.r)*fractBetween + currC.r;
        green = (prevC.g - currC.g)*fractBetween + currC.g;
        blue  = (prevC.b - currC.b)*fractBetween + currC.b;
        return;
      }
    }
    red   = color.back().r;
    green = color.back().g;
    blue  = color.back().b;
    return;
  }
};

//############################################################


//## GLOBAL VARIABLES:

vector<Sphere>      spheresIn;			// stores the spheres we wish to input
vector<Sphere>      spheresExtra;		// stores all spheres we can't overlap
vector<Sphere>      spheresOut;			// stores output spheres until ready to write model

vector<PixTemplate> pixTemplate;		// stores a template of pixels for each input sphere
vector<Filter>      filter;					// stores a list of filter to apply to each slice
                                    //  when it's loaded

PixTemplate currPixTemplate;        // pixel template used on each pixel
SliceBuffer sliceBuffer;            // used to keep multiple slices loaded into memory
																		//  at once (trying to minimize # loads)

//------------------------------

static void usage(void)
{
  printf("\n");
	printf("Inputs spheres from one or more scattered point objects and outputs\n");
	printf("new spheres with similar pixel values (useful for small vesicles).\n");
  printf("\n");
	printf("Usage: slashfindspheres [options] <image> <input_model> <output_model>\n");
  printf("Options:\n");
  printf("     -o #    list of scattered pt objects to use as input (eg: '1,4-5')\n");
	printf("             or enter '-o 0' for 'no object' approach... if omitted the \n");
	printf("             program uses the 1st scattered pt object in the model.\n");
	printf("     -e #    list of extra scattered pt objects we don't want to overlap.\n");
	printf("     -G #    # of pixels to use as a gutter around each sphere radius\n");
	printf("             when generating template to match                (default: 2)\n");
	printf("     -Z #    # of pixels to use as a gutter in Z     (default:   G/zScale)\n");
  printf("     -C #    cut off for cross-correlation as a percentage   (default: 80)\n");
	printf("     -R #    a range of pixel values to scale the curoff by               \n");
  printf("     -M #    maximum allowed pixels for a template to span   (default: 30)\n");
	printf("     -B #    the number of Z slices in the 'slice buffer     (default: 30)\n");
	printf("     -N #    maximum number of spheres added             (default: 10,000)\n");
	printf("\n");
	printf("     -x #,#  limits for x\n");
	printf("     -y #,#  limits for y\n");
	printf("     -z #,#  limits for z (starting at 0 for first slice)\n");
	printf("\n");
	printf("     -i      ignore the z axis (looks at everything in 2D only)\n");
	printf("     -d      allow new spheres to duplicate/overlap input spheres\n");
	printf("     -t #    test radius used to to quickly skip unlikely pixel values or\n");
	printf("             set to 0 to check every pixel in detail       (default:  3) \n");
	printf("     -s #    number new objects to split new spheres over  (default: 10)\n");
	printf("     -m      only create a single contour (with multiple points) per \n");
	printf("             object (by default is only 1 points per contour)        \n");
	printf("     -c      use original color of objects instead of heatmap colors \n");
	printf("     -w #    write out intermediate mrc files to help see results\n");
	printf("     -p #    print level where # is 0-10 -> 0 prints almost nothing\n");
  printf("             and 5 prints the most info during processing  (default:  2)\n");
	printf("     -F keys ordered list of image filters to apply in the form 'm5,s'\n");
	printf("             where m5=median size 5, s=sobel, etc (see man page for all)'\n");
	printf("             (default is 'm3' but you can enter '-F 0' for no filters)\n");
	printf("\n");
	printf("NOTE: This program is part of the 'SLASH' initiative and it features a \n");
	printf("      video tutorial here: \n");
	printf(" > http://www.slashsegmentation/methodology/auto/find-round-compartments\n");
}


//------------------------------
//-- Main function - takes in user entered arguments, then starts processing
//-- input object into a vector of spheres (spheresIn), then processes
//-- these into a vector of pixel templates (pixTexmplate), then processes
//-- each pixel in the image against the template to generate a vector of 
//-- output spheres (spheresOut) based on their level of "confidence",
//-- then finally eleminates duplicate spheres and then outputs a new copy
//-- of the input model file with the remaining output spheres appended
//-- into one or more new objects with heatmap colors from most to least
//-- confident between 1 and a determined "cutoff" value.

int main(int argc, char *argv[])
{
  FILE *inImage;						// input image file (1st compulsory argument)
  int i=0;									// tracks number of arguments
	
	Imod *imod = NULL;				// model file which is input, modified then written out
  MrcHeader inHead;					// header data for (input) image (nx, ny, nz, etc)
	int inSliceMode;
  IloadInfo li;							// loading information for image (smin, smax)	
	
	
	//## VERIFY CORRECT NUMBER OF ARGUMENTS:
	
	printf("\n\n");
	char *progname = imodProgName(argv[0]);
	
  if (argc <= 3)
	{
    imodVersion(progname);
    imodCopyright();
    usage();
    exit(3);
  }
  b3dSetStoreError(-1);		// make library error output to stderr go to stdout
  
	//## PROCESS OPTIONAL ARGUMENTS:
	
	int xmin = DEFAULT_INT;					// |-- the lowest and highest pixel to scan along
	int xmax = DEFAULT_INT;					// |   for matches each dimension.....
	int ymin = DEFAULT_INT;					// |   if left at DEFAULT_INT, then each will be
	int ymax = DEFAULT_INT;					// |   set to the edges of the input image
	int zmin = DEFAULT_INT;					// |   
	int zmax = DEFAULT_INT;					// |
	
	int rangeErr = 0;								// number of errors found in ranges
	bool useDefFilters = true;
	
	
  mrc_init_li(&li, NULL);
  for (i = 1; i < argc; i++)
	{
    if (argv[i][0] == '-')
      switch (argv[i][1])
			{
				case 'o':			processPositiveIntList(argv[++i], objList,      true);		break;
				case 'e':			processPositiveIntList(argv[++i], extraObjList, true);		break;	
				case 'G':			gutterXY      = atof(argv[++i]);													break;
				case 'Z':			gutterZ       = atof(argv[++i]);													break;
				case 'C':			cutoff        = atof(argv[++i]);													break;
				case 'R':			valRange      = atof(argv[++i]);													break;
				case 'M':			maxSpan       = atoi(argv[++i]);													break;
				case 'B':			Z_BUFFER      = atoi(argv[++i]);													break;
				case 'N':			maxSpheresOut = atoi(argv[++i]);													break;
				
				case 'x':			rangeErr += processTwoPositiveInts(argv[++i],xmin,xmax);	break;
				case 'y':			rangeErr += processTwoPositiveInts(argv[++i],ymin,ymax);	break;
				case 'z':			rangeErr += processTwoPositiveInts(argv[++i],zmin,zmax);	break;
				
				case 'i':			ignoreZ       = true;																			break;
				case 'd':			allowDup      = true;																			break;
				case 't':			testRadius    = atof(argv[++i]);													break;
				case 's':			splitLevel    = atoi(argv[++i]);													break;
				case 'm':     multiPts      = true;																			break;
				case 'c':     origColor     = true;																			break;
				case 'w':			writeImages   = atoi(argv[++i]);													break;
				case 'p':			printLevel    = atoi(argv[++i]);													break;
				case 'F':			processFilterList(argv[++i], filter);
											useDefFilters=false;		                  								break;
				
				default:			usage();			exit(3);																		break;
      }
    else
      break;
  }
	
	
	//## VERIFY OPTIONAL ARGUMENTS:
	
	if( cutoff > 1.0f )						// if cuttoff was entered as a percentage
		cutoff = cutoff / 100.0f;
	
	if( maxSpan <= 0 )
	{
    fprintf(stderr, "ERROR: Bad optional argument values detected.\n");
    exit(3);
	}
	
	if( maxSpan > 50 )
	{
		fprintf(stderr, "WARNING: You have set a large 'maximum span' value and processing "
					         "may take a very long time (increases exponentially).\n" );
	}
		
	if(useDefFilters)				// if no filters were specified: make default
	{
		filter.clear();
		filter.push_back( Filter(FT_MEDIAN,3) );
	}
	
	for(int i=0; i<objList.size(); i++)
	{
		objList[i]--;
		if( objList.size() > 1 && objList[i] < 0 )
			fprintf(stderr, "ERROR: you have entered an object number of 0 or less.\n" );
	}
	
	for(int i=0; i<extraObjList.size(); i++)
	{
		extraObjList[i]--;
		if( extraObjList[i] < 0 )
			fprintf(stderr, "ERROR: you have entered an extra object number of 0 or less.\n" );
	}
	
	if(gutterZ > 10 || gutterZ > 2*gutterXY)
	{
		fprintf(stderr, "WARNING: gutterZ value of %0.2f seems very high.\n", gutterZ);
	}
	
	
	//## OPEN INPUT MRC FILE (FOR READING):
	
  inImage = iiFOpen(argv[argc-3], "rb");
  if (!inImage)
	{
    fprintf(stderr, "ERROR: Opening image file %s.\n", argv[argc-3]);
    exit(3);
  }
	
	
  //## READ IN IMAGE HEADER AND IMAGE DATA:
	
  if (mrc_head_read(inImage, &inHead))
	{
    fprintf(stderr, "ERROR: Reading input file header");
    exit(3);
  }
  if (li.smin == li.smax)
	{
    li.smin = inHead.amin;
    li.smax = inHead.amax;
  }
  mrc_fix_li(&li, inHead.nx, inHead.ny, inHead.nz);
	inSliceMode = sliceModeIfReal(inHead.mode);
	long iTotXY = inHead.nx * inHead.nz;		// total pixels per slice
	
	
	//## DETERMINE X,Y,Z LIMITS FOR PIXELS TO CHECK:
	
	bool imageSubset = ( xmin!=DEFAULT_INT || xmax!=DEFAULT_INT 
										|| ymin!=DEFAULT_INT || ymax!=DEFAULT_INT
										|| zmin!=DEFAULT_INT || zmax!=DEFAULT_INT );
	
	if( xmin==DEFAULT_INT )		xmin = 0;
	if( xmax==DEFAULT_INT )		xmax = inHead.nx-1;
	if( ymin==DEFAULT_INT )		ymin = 0;
	if( ymax==DEFAULT_INT )		ymax = inHead.ny-1;
	if( zmin==DEFAULT_INT )		zmin = 0;
	if( zmax==DEFAULT_INT )		zmax = inHead.nz-1;
	
	if( keepInLimits( xmin, 0, inHead.nx-1 ) || keepInLimits( xmax, xmin, inHead.nx-1 )
	 || keepInLimits( ymin, 0, inHead.ny-1 ) || keepInLimits( ymax, ymin, inHead.ny-1 )
	 || keepInLimits( zmin, 0, inHead.nz-1 ) || keepInLimits( zmax, zmin, inHead.nz-1 ) )
	{
		printf( "WARNING: Bad x,y or z ranges were entered. These values should be in \n");
		printf( "         the form '-x 0,500 -y 100-3000, -z 0-5' within image limits.\n\n");
	}
	
	
	//## DETERMINE APPROPIATE MINIMUM, MAXIMUM AND VALUE RANGE AFTER FILTERS APPLIED
	
	sliceBuffer.setSize(inHead.nz,Z_BUFFER);
	
	float minVal = inHead.amin;						// |-- min and max value for scaling 
	float maxVal = inHead.amax;						// |
	int middleSlice = (zmin+zmax) / 2;		// middle-most slice
	
	bool useSampleValRange = (filter.size() > 0 && valRange == DEFAULT );
	
	if( useSampleValRange )				// if we're applying filters and no custom range is set:
	{
		LoadedSlice *loadSl = sliceBuffer.getSlice(&inHead,middleSlice,filter);
		loadSl->updateMinMaxAndMean();		// calculate sample range using the middle slice
		
		minVal = loadSl->sl->min;
		maxVal = loadSl->sl->max;
		valRange = maxVal - minVal;
		
		if(valRange<=10.0f)
			fprintf(stderr, "ERROR: Filtering causes poor input range\n");
	}
	if(valRange == DEFAULT || valRange <= 0 )
	{
		valRange = (inHead.amax - inHead.amin);
		if(valRange<=0)
			valRange = 50;
	}
	
	
	//## PRINT OUT INFORMATION ABOUT OUR IMAGE AND FILTERS:
	
	if(printLevel >= 1)
	{
		printf("FILTERS TO APPLY:\n"  );
		for(int i=0; i<filter.size(); i++)
		{
			printf("   > #%d -> ", i+1);
			filter[i].print();
		}
		if(filter.size()==0)	printf("   none\n", valRange );
		if(useDefFilters)			printf("   (default filter options)\n", valRange );
		printf("\n"  );
		
		printf("PROPERTIES OF IMAGE: \n"  );
		printf("  image size:     %d x %d x %d \n", inHead.nx, inHead.ny, inHead.nz );
		printf("  pixel  min/max:  %4.1f - %4.1f \n", inHead.amin, inHead.amax );
		if(useSampleValRange)
		{
			printf("  sample min/max:  %4.1f - %4.1f ... (sampled from slice %d)\n",
						 minVal, maxVal, middleSlice+1 );
		}
		printf("  value range for computing correlation:  %4.2f \n\n", valRange );
		if(imageSubset)
		{
			printf("  specified image subset:   x:%d-%d  y:%d-%d  z:%d-%d\n\n",
						 xmin,xmax, ymin,ymax, zmin,zmax );
		}
	}
		
	//## OPEN INPUT IMOD FILE (FOR READING):
	
	imod = imodRead(argv[argc-2]);
  if (!imod){
    fprintf(stderr, "ERROR: Problem reading imod model %s\n", argv[argc-2]);
    exit(3);
  }
	float zScale = imodGetZScale(imod);
	if(zScale<=0)	zScale = 1.0f;
	if( gutterZ == DEFAULT )
		gutterZ = gutterXY / zScale;
	
	if(printLevel >= 4)
	{
		printf("MODEL AND GUTTER PROPERTIES:\n"  );
		printf("  z scale:        %0.4f \n",   zScale   );
		printf("  gutterXY:       %0.4f \n",   gutterXY );
		printf("  gutterZ:        %0.4f \n",   gutterZ  );
		printf("  Z_BUFFER:       %d    \n\n", Z_BUFFER );
	}
	
	//## IF NO OBJECTS WERE SPECIFIED: FIND FIRST SCATTERED POINT OBJECT
	
	bool useNoObjectApproach = (objList.size()==1) && (objList[0] < 0);
	bool noCustomObjsEntered = (objList.size()==0);
	
	if( noCustomObjsEntered )
	{
		int firstScatPtObj = -1;
		for( int o=0; o<imodGetMaxObject(imod); o++ )
		{
			Iobj *obj = getAndSetObj( imod, o );
			if( obj && imodObjectGetValue(obj,IobjFlagConnected)==0 )	// if scattered pt obj:
			{
				firstScatPtObj = o;
				break;
			}
		}
		if( firstScatPtObj >= 0 )			objList.push_back(firstScatPtObj);
		else													useNoObjectApproach = true;
	}
	
	//## PRINT OBJECT INFO:
	
	if(printLevel>=1)
	{
		printf("OBJECT INPUT PROPERTIES:\n"  );
		if( noCustomObjsEntered ) {
			if( useNoObjectApproach )	
				printf("  no objects were input, and no scattered pt objects exist in model\n");
			else
				printf("  no objects were input, so will use first scattered pt object...\n");
		}
		
		if(useNoObjectApproach) {
			printf("  ... will use 'no object approach'.\n\n");
		}
		else {
			printf("  object(s) to use as input: ");
			for(int i=0; i<objList.size(); i++)	{
				if (i==objList.size()-1)  printf("%d\n",  objList[i]+1);
				else                      printf("%d,",   objList[i]+1);
			}
		}
		if(extraObjList.size()>0)	{
			printf("  extra object(s) to avoid overlapping: ");
			for(int i=0; i<extraObjList.size(); i++) {
				if (i==extraObjList.size()-1)  printf("%d\n",  extraObjList[i]+1);
				else                           printf("%d,",   extraObjList[i]+1);
			}
		}
	}
	
	//## POPULATE VECTOR OF SPHERES AND SORT BY Z (TO HELP REDUCE # BUFFER LOADS):
	
	for( int o=0; o<objList.size() && !useNoObjectApproach; o++ )
	{
		addImodObjectSpheresToVector( imod, objList[o], spheresIn, true, true, inHead,
																  gutterXY, gutterZ, zScale );
	}	
	sort( spheresIn.begin(), spheresIn.end() );		// sorts our spheres by z (and y then x)
	
	
	//## CREATE TEMPLATE FOR EACH SPHERE:
	
	pixTemplate.resize( spheresIn.size() );
	
	for(int i=0; i<spheresIn.size(); i++)		// for each sphere:
	{
		Sphere &sp         = spheresIn[i];
		PixTemplate &pxT   = pixTemplate[i];
		
		populateTemplate( pxT, sp, inHead, gutterXY, gutterZ, true, zScale );
	}
	
	
	if( printLevel >= 10 && pixTemplate.size() > 0 )
	{
		printf("\nFIRST SPHERE'S VALUES:\n\n");
		pixTemplate[0].print(false);
		printf("\nFIRST SPHERE'S WEIGHTS:\n\n");
		pixTemplate[0].print(true);
	}
	
	
	//## DETERMINE THEN PRINT LARGEST SIZE OF ANY TEMPLATE:
	
	int maxXYSpan = 0;		// the span of the largest pixel template in X and Y
	int maxZSpan  = 0;		// the span of the largest pixel template in Z 
	
	for(int i=0; i<pixTemplate.size(); i++)		// for each sphere:
	{
		if(maxXYSpan < pixTemplate[i].nx)		maxXYSpan = pixTemplate[i].nx;
		if(maxXYSpan < pixTemplate[i].ny)		maxXYSpan = pixTemplate[i].ny;
		if(maxZSpan  < pixTemplate[i].nz)		maxZSpan  = pixTemplate[i].nz;
	}
	int maxXYRad = maxXYSpan / 2;
	int maxZRad  = maxZSpan  / 2;
	
	if(printLevel >= 1)
	{
		fprintf(stdout, "\nTEMPLATE INFORMATION: \n"  );
		fprintf(stdout, "  number templates/spheres:   %d \n", (int)pixTemplate.size() );
		fprintf(stdout, "  max radius in XY:     %d \n", maxXYRad );
		fprintf(stdout, "  max radius in Z:      %d \n", maxZRad );
		fprintf(stdout, "\n\n"  );
	}
	
	
	//## FOR EACH TEMPLATE, COMPUTE RING HISTOGRAM AND COPY INTO VECTOR OF HISTOGRAMS:
	
	vector<ConcentricRings> allRings;
	for(int i=0; i<pixTemplate.size(); i++)		// for each sphere:
	{
		pixTemplate[i].calcHistogramRings( histBinSize,histFirstBin,zScale );
		allRings.push_back( pixTemplate[i].ringHist );
	}
	
	
	//## FIND AVERAGE AND MAXIMUM NUMBER OF BINS PER HISTOGRAMS:
	
	int   maxNumBins      = 0;
	float maxExtendedRad  = 0.0f;
	float avgRad          = 0.0f;
	for(int i=0; i<allRings.size(); i++)		// for each sphere:
	{
		maxNumBins     = max( maxNumBins,     (int)allRings[i].ring.size() );
		maxExtendedRad = max( maxExtendedRad, allRings[i].maxRad );
		avgRad         += (float)allRings[i].sphereRad;
	}
	
	
	//## DETERMINE AN "AVERAGED" HISTOGRAM:
	
	ConcentricRings avgHist;			// in this histgram we averages all rings without
																//  shifting any
	ConcentricRings scaledHist;		// in this histogram, the first bin size of each
																//  histogram is stretched so the original spheres
																//  will effectively line up with the largest
	
	avgHist.setupRings( histBinSize,histFirstBin, avgRad,maxExtendedRad );
	
	avgHist.setupWeightsSameValue( 0.0f );
	calcAvgOfHistograms( allRings, avgHist, false );
	
	//scaledHist.setupRings( histBinSize,histFirstBin, avgRad,maxExtendedRad );
	//calcAvgOfAdjustedHistograms( allRings,scaledHist, false );
	//TODO: Finish a scaled histogram
	
	if(printLevel >= 2)
	{
		printf("AVERAGED HISTOGRAM: (averaged over %d spheres)\n", allRings.size() );
		avgHist.print( valRange, minVal, true, true );	printf("\n");
		
		for(int i=0; i<allRings.size() && (printLevel >= 3); i++)	{
			printf("Histogram %d\n",i+1);
			allRings[i].print( valRange, minVal, true, (printLevel>=4) );
		}
		printf("\n");
	}
	
	
	//## COMPARE SPHERE TEMPLATES TO EACH OTHER TO ESTIMATE A GOOD CUTOFF:
	
	float estGoodCutoff    = 0.0f;	// will be set to an "estimated" good cutoff
	                                //  using cross-comparision of spheres	
	int totComp            = 0;			// total times a tempate is compared to another
	
	float minCompCorr      = 1.0f;
	float maxCompCorr      = 0.0f;
	float avgCompCorr      = 0.0f;
	
	for(int i=0;   i<pixTemplate.size(); i++)		// for each template:
	for(int j=i+1; j<pixTemplate.size(); j++)		// for every other template:
	{
		if(   pixTemplate[i].origSphere.objIdx
			 != pixTemplate[j].origSphere.objIdx )			// if from different objects:
			continue;																			// don't compare
		
		float correlation = pixTemplate[i].calcCorrelationOn( pixTemplate[j], true );
		minCompCorr  = min( minCompCorr, correlation );
		maxCompCorr  = max( maxCompCorr, correlation );
		avgCompCorr += correlation;
		totComp++;
		
		if(totComp>=MAX_COMPARES)
			break;
	}
	avgCompCorr = ( totComp==0 ) ? 0.0f : avgCompCorr/(float)totComp;
	
	bool enoughCompsForCutoff  = (totComp >= 4);
	bool wasCustomCutoffGiven  = (cutoff != DEFAULT_CUTOFF && cutoff > 0 );
	
	float adjMulti          = (totComp >= 10) ? 1.5f : 2.0f;
	estGoodCutoff           = 1.0f - (adjMulti * (1.0f-avgCompCorr));	// estimated cutoff
	float adjustedMinCutoff = 1.0f - (1.25f    * (1.0f-minCompCorr));
	if( estGoodCutoff > adjustedMinCutoff )
		estGoodCutoff = (estGoodCutoff + adjustedMinCutoff) / 2.0f;
	
	if( wasCustomCutoffGiven == false )
		cutoff = (enoughCompsForCutoff) ? estGoodCutoff : DEFAULT_CUTOFF;
	
	
	//## PRINT RESULT OF COMPARISION AND FINAL CUTOFF VALUE:
	
	if( printLevel >= 1 )
	{
		printf( "CUTOFF INFO:\n" );
		if( totComp )
		{
			printf("  calculated values (using comparison of input spheres):\n"          );
			printf("    number comparisons:      %d    \n",       totComp                 );
			printf("    average correlation:     %0.4f \n",       avgCompCorr             );
			printf("    min and max correlation: %0.4f-%0.4f \n", minCompCorr,maxCompCorr );
			printf("    estimated 'good cutoff': %0.4f \n",       estGoodCutoff           );
		}
		if( !enoughCompsForCutoff )
		{
			printf("  WARNING: you have not entered enough spheres to do a good \n");
			printf("           cross comparision of values... aim for 5-10 input \n");
			printf("           spheres for better results next time. \n");
		}
		
		printf("\n  cutoff value: %0.5f      *", cutoff );
		if( wasCustomCutoffGiven )					printf("(as specified by you)\n");
		else if( cutoff==DEFAULT_CUTOFF )		printf("(default value)\n");				
		else																printf("(as determined using your spheres)\n");
		printf("\n");
	}
	
	
  //## DETERMINE MIN AND MAX ALLOWED VALUES FOR "EARLY ELIMINATION":
	
	float minAllowedMean = 0.0f;							// |-- the allowable range for mean values
	float maxAllowedMean = 0.0f;							// |   in the test radius area
	
	bool useEarlyElim    = (testRadius>=1);		// says if we will use early elimination
	
	if( useEarlyElim && avgHist.ring.size() > 0 )
	{
		avgHist.getMinMaxAllowedInRange( minAllowedMean, maxAllowedMean, 0.0f, testRadius);
		
		if(printLevel>=3)
		{
			printf("EARLY ELIMINATION INFO\n");
			printf("  min/max allowed mean = %0.2f-%0.2f \n", minAllowedMean,maxAllowedMean );
			printf("  test radius          = %1.0f\n\n",testRadius);
		}
	}
	
	long numEarlyElim    = 0;               // tallies the total # of early eliminations
	long totPxProc       = 0;								// tallies the total # of pixels tested
	
	
	
	//## FOR EACH PIXEL IN IMAGE PERFORM CROSS CORRELATION AGAINST TEMPLATES:
	
	Sphere currSp;										// represents our current point
	
	for(int z=zmin; z<=zmax; z++)			// for each slice:
	{
		if(printLevel>=1)	printf("\nPROCESSING SLICE %d: ", z+1);
		float bestConfThisSlice = 0.0f;
		
		for(int y=ymin; y<=ymax; y++)			// |-- for each pixel:
		for(int x=xmin; x<=xmax; x++)			// |
		{
			if(x==0 && y%10==0) { printf("."); fflush(stdout); }
			
			//## CHECK FOR EARLY ELIMINATION:
			totPxProc++;
			if (useEarlyElim)
			{
				float avgV = getAvgValInSquareArea( inHead, x,y,z, testRadius*2.0f );
				if( avgV < minAllowedMean || avgV > maxAllowedMean )	{
					numEarlyElim++;
					continue;
				}
			}
			
			//## POPULATE TEMPLATE FOR PIXELS AND CHECK AGAINST ALL OTHER TEMPLATES:
			currSp.set( (float)x, (float)y, (float)z, 0.0f, -1);
			currSp.confidence = 0.0f;
			
			populateTemplate( currPixTemplate, currSp, inHead,
											  maxXYRad, maxZRad, false, zScale );
			
			float fractValidPx = currPixTemplate.setWeightsForValidPixels(1,0);
			
			for(int i=0; i<pixTemplate.size(); i++)
			{
				float correlation = pixTemplate[i].calcCorrelationOn( currPixTemplate, true );
				if(currSp.confidence < correlation)
				{
					currSp.confidence = correlation;
					currSp.r          = pixTemplate[i].origSphere.r;
					currSp.objIdx     = pixTemplate[i].origSphere.objIdx;
				}
			}
			
			if( currSp.confidence >= cutoff )
				spheresOut.push_back( currSp );
			
			if( bestConfThisSlice < currSp.confidence && currSp.confidence < 1.0f )
				bestConfThisSlice = currSp.confidence;
		}
		printf(" best match = %f", bestConfThisSlice);
	}
	printf("\n\n");
	
	
	//## CREATE A VECTOR OF ALL INPUT AND EXTRA SPHERES WHICH WE DON'T WANT
	//## ANY OUTPUT SPHERES TO OVERLAP:
	
	for( int o=0; o<objList.size() && !useNoObjectApproach; o++ )	{
		addImodObjectSpheresToVector(imod,objList[o],spheresExtra,false,false,inHead,
																 0,0,zScale );
	}
	for( int o=0; o<extraObjList.size(); o++ ) {
		addImodObjectSpheresToVector(imod,extraObjList[o],spheresExtra,false,false,inHead,
																 0,0,zScale );
	}
	
	
	//## SORT SPHERES BY CORRELATION AND ELIMIATE THOSE THAT OVERLAP:	
	
	sort( spheresOut.begin(), spheresOut.end(), compareSphereSimilarity );
	reverse( spheresOut.begin(), spheresOut.end() );
	
	deleteOverlappingSpheres( spheresOut, zScale, spheresExtra, !allowDup, false );
	
	if( spheresOut.size()==0 )
	{
		printf("Sorry... no spheres found\n\n");
	}
	
	
	//## ADD REMAINING SPHERES TO NEW OBJECT(S) IN THE MODEL:
	
	if( spheresOut.size() > maxSpheresOut )	{
		printf("NOTE: The # of spheres remaining is > the max number of allowed.\n");
		printf("      %d spheres will be reduced to %d\n\n",spheresOut.size(),maxSpheresOut);
		spheresOut.resize( maxSpheresOut );
	}
	
	
	int numNewObjects = splitLevel;
	int spheresAdded  = 0;
	
	if( !useNoObjectApproach && objList.size() > 0 )
	{
		for(int o=0; o<objList.size(); o++)
		{
			addSpheresToModel( imod, spheresOut, numNewObjects, cutoff, objList[o],
											   true, true, true, origColor, multiPts );
		}
	}
	
	
	//## OPEN/CREATE OUTPUT IMOD FILE (FOR WRITING):
	
	imodBackupFile(argv[argc-1]);
  if (imodOpenFile(argv[argc-1], "wb", imod)) {
    fprintf(stderr, "ERROR: Fatal error opening new model %s\n", argv[argc-1]);
    exit (1);
  }
  imodWriteFile(imod);
	
	
	//## IF DESIRED: WRITE OUT MRC FILES SHOWING INTERMEDIATE RESULTS:
	
	char outfile1[MAX_CHARS];
	char outfile2[MAX_CHARS];
	if(writeImages>=1)
	{
		char *outmod = argv[argc-1];			// input model file
		strcpy (outfile1, outmod );
		strncat(outfile1, "_intermediate1.mrc", 18);
		
		printf("\nINTERMEDIATE FILE GENERATION (SHOWING FILTERS):\n", outfile1);
		printf("  Writing MRC file to: '%s'\n", outfile1);
		
		//writeOutMrcFileAfterFilters( inHead, outfile, filter );
		
		if(writeImages>=2)
		{
			sliceBuffer.clearAllSlices();
			strcpy (outfile2, outmod );
			strncat(outfile2, "_intermediate2.mrc", 18);
			
			printf("\nINTERMEDIATE FILE GENERATION (SHOWING CUTOFF):\n", outfile2);
			printf("  Writing MRC file to: '%s'\n", outfile2);
			printf("  WARNING: This operation is VERY slow!\n", outfile2);
			writeOutMrcFileWithCutoffVals( inHead, outfile2, filter,
																		 zmin, zmax, maxXYRad, maxZRad, zScale );
		}
	}
	
	sliceBuffer.clearAllSlices();
	
	
	//## CLOSE FILES AND OUTPUT RESULTS:
	
	float percEarlyElim = (totPxProc) ? (numEarlyElim/(float)totPxProc)*100.0f : 0.0f;
	
	iiFClose(inImage);
	
	fprintf(stdout, "\nPERFORMANCE SUMMARY:\n"  );
	fprintf(stdout, "  # slice loads:       %d\n", sliceBuffer.totSliceLoads   );
	fprintf(stdout, "  # reloads avoided:   %d\n", sliceBuffer.totSliceRepeats );
	fprintf(stdout, "  # early elimination: %0.1f%% of %d pixels\n", percEarlyElim, (int)totPxProc );
	fprintf(stdout, "\n\n"  );
	
	fprintf(stdout, "RESULTS SUMMARY:\n" );
	fprintf(stdout, "  Finished writing '%s'\n", argv[argc-1]      );
	fprintf(stdout, "  # spheres input:  %d\n", spheresIn.size()   );
	fprintf(stdout, "  # spheres added:  %d\n", spheresAdded       );
	fprintf(stdout, "  # new objects:    %d\n", numNewObjects      );
	fprintf(stdout, "\n\n"  );
	
	if(printLevel>=2)
	{
		fprintf(stdout, "TO SEE RESULTS ENTER:\n" );
		fprintf(stdout, "  3dmod -C 1 %s", argv[argc-3]     );
		if(writeImages>=1)	fprintf(stdout, " %s", outfile1 );
		if(writeImages>=2)	fprintf(stdout, " %s", outfile2 );
		fprintf(stdout, " %s\n\n", argv[argc-1] );
	}
	
  return 0;			// return success so that next operation continues
}

//------------------------------
//-- Inputs a PixTemplate (pxT) and populates it with a 3D array of pixels
//-- within a minimum bounding rectange around the given sphere (sp) with
//-- the radius extended by (gutterInXY) along the X and Y axis, and extended
//-- by (gutterInZ) along the z axis. Note that the pixels come from the 
//-- image specified by (inHead) and are loaded using the global "sliceBuffer".
//-- Results are output to "pxT" and the middle-most pixel (in X,Y,Z) will be
//-- the center of "sp" and it's final dimensions will be:
//--    ~2*(sp.r+gutterInXY)   *   2*(sp.r+gutterInXY)  *   2*(sp.r+gutterInZ)
//-- 
//-- An exception to this is if "ignoreZ" is true, in which case it will only
//-- be 1 pixel in Z.
//-- 
//-- @ pxT        - the pixel template we want to add/output pixel values too
//-- @ sp         - a sphere representing the area around which to load pixels
//-- @ inHead     - specifies the MRC file from which we wish to load values
//-- @ gutterInXY - a distance added to the "sp" radius in XY
//-- @ gutterInZ  - a distance added to the "sp" radius in Z
//-- @ setWeights - if true: the weights in pxT will be populated based on the
//--                sphere's radius and whatever global "weightstrategy" is set
//-- @ zScale     - factor by which to stretch the sp radius in Z

void populateTemplate( PixTemplate &pxT, Sphere sp, MrcHeader &inHead,
											 float gutterInXY, float gutterInZ,
											 bool setWeights, float zScale )
{
	int fullRadXY = sp.r + gutterInXY;
	int fullRadZ  = (ignoreZ) ? 0 : ceil(sp.r/zScale + gutterInZ);
	
	int centerX = sp.x;
	int centerY = sp.y;
	int centerZ = sp.z;
	
	int nX = 2*fullRadXY + 1;
	int nY = 2*fullRadXY + 1;
	int nZ = 2*fullRadZ  + 1;
	
	int minX = max(centerX - fullRadXY, 0          );
	int maxX = min(centerX + fullRadXY, inHead.nx-1);
	int minY = max(centerY - fullRadXY, 0          );
	int maxY = min(centerY + fullRadXY, inHead.ny-1);
	int minZ = max(centerZ - fullRadZ,  0          );
	int maxZ = min(centerZ + fullRadZ,  inHead.nz-1);
	
	int offX = centerX - fullRadXY;
	int offY = centerY - fullRadXY;
	int offZ = centerZ - fullRadZ;
	
	pxT.setOrigSphere( sp );
	pxT.setSphere( fullRadXY, fullRadXY, fullRadZ, sp.r, sp.objIdx );
	pxT.setSize( nX, nY, nZ );
	pxT.clearPxValues();				// we should clear values in case some existed here
	
	if(setWeights)
		pxT.setWeightsUsingSphere( pxT.center, gutterXY, zScale, 1.0f, 1.0f );
	
	for(int z=minZ; z<=maxZ; z++ )		// for each slice in sphere:
	{
		int tZ = z-offZ;
		LoadedSlice *lSlice = sliceBuffer.getSlice(&inHead,z,filter);	// load slice
		float *slData       = lSlice->sl->data.f;									// slice data as floats
		
		for(int y=minY; y<=maxY; y++)
		for(int x=minX; x<=maxX; x++)
		{
			int tIdx = pxT.getPtIdx(x-offX,y-offY,tZ);			// position in teamplate
			int iIdx = y*inHead.nx + x;											// position in image
						
			pxT.px[ tIdx ] = slData[iIdx];														// copy pixel value
		}
	}
}


//------------------------------
//-- Determines the average pixel value of all pixels within a square
//-- area of a given side length (sideLength) centered on the given pixel
//-- (x,y,z) in the given mrc file (inHead).

float getAvgValInSquareArea( MrcHeader &inHead, int x, int y, int z, float sideLength )
{
	if(z < 0 || z >= inHead.nz)
		return -1.0f;
	
	LoadedSlice *lSlice = sliceBuffer.getSlice(&inHead,z,filter);		// load slice
	float *slData       = lSlice->sl->data.f;									// slice data as floats
	
	int rad = ceil(sideLength / 2.0f);
	
	int minX = max(x - rad, 0          );
	int maxX = min(x + rad, inHead.nx-1);
	int minY = max(y - rad, 0          );
	int maxY = min(y + rad, inHead.ny-1);
	
	float numPx = 0.0f;
	float avgV  = 0.0f;
	
	for(y=minY; y<=maxY; y++)																
	for(x=minX; x<=maxX; x++)
	{
		int iIdx = y*inHead.nx + x;											// position in image
		avgV    += slData[iIdx];
		numPx++;
	}
	
	return (numPx>0.0f) ? (avgV / numPx) : 0.0f; 
}


//------------------------------
//-- Used to sort spheres by their confidence level (in ascending order).

bool compareSphereSimilarity (const Sphere &a, const Sphere &b)
{
  return ( a.confidence < b.confidence );
}

//------------------------------
//-- Inputs a vector of "ConcentricRings" (hist) and returns the maximum
//-- number of rings in any of these histograms.

int getMaxNumRings( const vector<ConcentricRings> &hist )
{
	int maxRings = 0;
	for( int i=0; i<hist.size(); i++ )
		maxRings = max( maxRings, (int)hist[i].ring.size() );
	return ( maxRings );
}


//------------------------------
//-- Inputs a vector of "ConcentricRings" (hist) and returns the maximum
//-- radius in any concentric ring's "sphereRad" value.

float getMaxRadius( const vector<ConcentricRings> &hist )
{
	float maxRadius = 0.0f;
	for( int i=0; i<hist.size(); i++ )
		maxRadius = max( maxRadius, hist[i].sphereRad );
	return ( maxRadius );
}


//------------------------------
//-- Inputs a vector of "ConcentricRings" (hist) and outputs a single
//-- ConcentricRing (avgHist) which represents a histogram with the size
//-- of the largest histogram (maximum number or rings), but where all ring
//-- values are averaged over all histograms with that ring, and weighted
//-- according to the number of histograms that have that ring.
//-- 
//-- As an example, if we have three histograms in "hist" with 5, 6 and 4
//-- rings respectively and (for simplicity) all ring weights are 1. If the values
//-- of their 5th ring are 120, 130 and N/A respectively (remembering that our third
//-- histogram has only 4 rings) then the 5th ring of avgHist will get given
//-- a meanVal=125 ({120+130}/2) and weight=1 ({1+1}/2).
//-- Note that "stDev" is also averaged, while "minVal" and "maxVal" are also
//-- updated to reflect the minimum/maximum over all ConcentricRings.
//-- 
//-- If (useCurrentNumberOfRings) is true then avgHist will not be resized, so
//-- you should make sure you've given it the desired number of rings you
//-- want averaged.

void calcAvgOfHistograms( const vector<ConcentricRings> &hist,
													ConcentricRings &avgHist, bool useCurrentNumberOfRings )
{
	if( useCurrentNumberOfRings )
		avgHist.ring.resize( getMaxNumRings( hist ) );
	
	
	for( int b=0; b<avgHist.ring.size(); b++ )
	{
		avgHist.ring[b].reset();
		avgHist.ring[b].binWeight = 0.0f;
		float numHistsWithThisBin = 0.0f;
		
		for( int i=0; i<hist.size(); i++ )
		{
			if( b>=hist[i].ring.size() || hist[i].ring[b].binWeight == 0.0f )
				continue;
			
			avgHist.ring[b].innerRadius += hist[i].ring[b].innerRadius;
			avgHist.ring[b].outerRadius += hist[i].ring[b].outerRadius;
			
			float weight = hist[i].ring[b].binWeight;
			avgHist.ring[b].numPx     += hist[i].ring[b].numPx   * weight;
			avgHist.ring[b].meanVal   += hist[i].ring[b].meanVal * weight;
			avgHist.ring[b].stDev     += hist[i].ring[b].stDev   * weight;
			avgHist.ring[b].minVal     = min( hist[i].ring[b].minVal, avgHist.ring[b].minVal );
			avgHist.ring[b].maxVal     = max( hist[i].ring[b].maxVal, avgHist.ring[b].maxVal );
			avgHist.ring[b].minAllowedMean     = min( hist[i].ring[b].minAllowedMean,
																								avgHist.ring[b].minAllowedMean );
			avgHist.ring[b].maxAllowedMean     = max( hist[i].ring[b].maxAllowedMean,
																								avgHist.ring[b].maxAllowedMean );
			
			avgHist.ring[b].binWeight += weight;
			numHistsWithThisBin       += 1.0f;
		}
		
		if( numHistsWithThisBin > 0.0f && avgHist.ring[b].binWeight > 0.0f )
		{
			avgHist.ring[b].innerRadius /= numHistsWithThisBin;
			avgHist.ring[b].outerRadius /= numHistsWithThisBin;
			
			float totWeight = avgHist.ring[b].binWeight;
			avgHist.ring[b].numPx      /= totWeight;
			avgHist.ring[b].meanVal    /= totWeight;
			avgHist.ring[b].stDev      /= totWeight;
			avgHist.ring[b].binWeight  /= numHistsWithThisBin;
		}
	}

}

//------------------------------
//-- 
//-- 

void calcAvgOfAdjustedHistograms( const vector<ConcentricRings> &hist,
													        ConcentricRings &avgHist, bool resetVals )
{
	int    maxNumRings = getMaxNumRings( hist );
	float  maxRadius   = getMaxRadius  ( hist );
	
	avgHist.ring.resize( getMaxNumRings( hist ) );
	avgHist.sphereRad  = maxRadius;
	
	for( int i=0; i<hist.size(); i++ )
	{
		float radiusDiff = maxRadius - hist[i].sphereRad;
		
		/*float binDiff = ()
		{
			
		}
		
		for( int i=0; i<hist.size(); i++ )
		{
			maxRad         = max( maxRad,         hist[i].sphereRad );
			maxExtendedRad = max( maxExtendedRad, hist[i].maxRad    );
		}*/
	
	}
	
	
}

//------------------------------





//############################################################




struct CellOfIndexes
{
	list<long> idx;
};


class MinimumGridOfIndexes
{
public:
	float xmin, ymin, zmin;				// an offset representing the minimum point of the grid
	float xmax, ymax, zmax;				// the maximum allowed point (before min offset applied)
	float xspan, yspan, zspan;		// the size of the grid along each dimension
	
	float xside, yside, zside;		// size of a grid cell along each dimension
	int nx, ny, nz;
	long totalXY;
	long totalCells;
	
	vector<CellOfIndexes> cell;
	
	
	MinimumGridOfIndexes()	{	reset();	}
	
	void reset()
	{
		cell.clear();
		xmin  = ymin  = zmin  = FLOAT_MAX;
		xmax  = ymax  = zmax  = FLOAT_MIN;
		xspan = yspan = zspan = 0.0f;
		xside = yside = zside = 0.0f;
		nx    =  ny   = nz    = 0;
		totalCells = totalXY  = 0;
	}
	
	void increaseBoundsToIncludePt(float x, float y, float z)
	{
		xmin = min( xmin, x );
		xmax = max( xmax, x );
		ymin = min( ymin, y );
		ymax = max( ymax, y );
		zmin = min( zmin, z );
		zmax = max( zmax, z );
		
		xspan = xmax - xmin;
		yspan = ymax - ymin;
		zspan = zmax - zmin;
		
		if(xspan==0)	xspan = 1.0f;		// -- must avoid divide by 1 error
		if(yspan==0)	yspan = 1.0f;		//
		if(zspan==0)	zspan = 1.0f;		//
	}
	
	void setupGrid( int numSidesX, int numSidesY, int numSidesZ, bool initCellVector )
	{
		nx = numSidesX;
		ny = numSidesY;
		nz = numSidesZ;
		
		xside = xspan / (float)nx;
		yside = yspan / (float)ny;
		zside = zspan / (float)nz;
		
		calcGridSize( initCellVector );
	}
	
	void setupGridWithSideLengths( float sideLenX, float sideLenY, float sideLenZ,
																 bool initCellVector )
	{
		xside = sideLenX;
		yside = sideLenY;
		zside = sideLenZ;
		
		nx = ceil( xspan / xside );
		ny = ceil( yspan / yside );
		nz = ceil( zspan / zside );
		
		calcGridSize( initCellVector );
	}
	
	long calcGridSize( bool initCellVector )
	{
		if(nx <= 0)	nx=1;
		if(ny <= 0)	ny=1;
		if(nz <= 0)	nz=1;
		
		totalXY    = nx*ny;
		totalCells = nx*ny*nz;
		if(initCellVector)
			cell.resize( totalCells );
		return totalCells;
	}
	
	inline int getXIdx( float x )			{   return floor((x - xmin) / xside);   }
	inline int getYIdx( float y )			{   return floor((y - ymin) / yside);   }
	inline int getZIdx( float z )			{   return floor((z - zmin) / zside);   }
	
	inline long getIdx( int xIdx, int yIdx, int zIdx )
	{
		return ( zIdx*totalXY + yIdx*nx + xIdx );
	}
	
	inline long getCellIdxOfPoint( float x, float y, float z )
	{
		int xIdx = getXIdx(x);
		int yIdx = getYIdx(y);
		int zIdx = getZIdx(z);
		
		if(xIdx < 0 || xIdx >= nx)	return -1;
		if(yIdx < 0 || yIdx >= ny)	return -1;
		if(zIdx < 0 || zIdx >= nz)	return -1;
		
		return ( zIdx*totalXY + yIdx*nx + xIdx );
	}
	
	
	bool addIndexToGrid( float x, float y, float z, long newIdxToAdd )
	{
		long cIdx = getCellIdxOfPoint( x,y,z );
		if(cIdx < 0)
		{
			printf("  ...! Failed to add point (%3f,%3f,%3f)",x,y,z);		// ERROR
			return false;
		}
		
		cell[cIdx].idx.push_back( newIdxToAdd );
		return true;
	}
	
	void print( int maxX=INT_MAX )
	{
		printf("MinimumGridOfIndexes... %dx%dx%d\n", nx,ny,nz);
		maxX = min(maxX, nx);
		for(int z=0; z<nz;   z++)
		for(int y=0; y<ny;   y++)
		for(int x=0; x<maxX; x++)
		{
			if(y==0 && x==0)		printf("---z=%d---\n", z);
			long cIdx       = getIdx(x,y,z);
			int numIndexes  = cell[ cIdx ].idx.size();
			(x==maxX-1) ? printf("%d\n",numIndexes) : printf("%d, ", numIndexes);
		}
	}
};



//------------------------------
//-- Takes a vector of spheres (sphere) and alters the vector by deleting
//-- spheres which touch each other in 3D space. For each sphere it will
//-- and spheres further ahead in the vector which overlap with it - hence
//-- the spheres should be pre-ordered in descending priority (the ones you
//-- most want to keep first). If (useExtraSpheres) is true, then any sphere in
//-- "sphere" which overlaps with a sphere in (extraSphere) is deleted before
//-- processing the sphere vector - otherwise extraSphere is ignored completely.
//--
//-- NOTE: In order to compare spheres efficiently, this function first builds
//--       a "MinimumGridOfIndexes", representing a uniform grid where spheres
//--       are placed so that only spheres in nearby grid cells need to be
//--       be compared. Without this grid this function would take O(n^2)
//--       operations.
//-- 
//-- @ sphere      - the vector of spheres you want altered
//-- @ zScale      - the factor by which to stretch the sphere along z
//-- @ extraSphere - a vector of spheres which you don't want any sphere to overlap with
//-- @ useExtraSpheres   - if false: the extraSphere vector is ignored
//-- @ markForDeleteOnly - if true: instead of deleteing spheres, they will just
//--                       have their "confidence" set to -1.

bool deleteOverlappingSpheres( vector<Sphere> &sphere, float zScale,
															 vector<Sphere> &extraSphere, bool useExtraSpheres,
															 bool markForDeleteOnly )
{
	if( zScale <= 0.0f || sphere.size() == 0 )
		return false;
	
	
	MinimumGridOfIndexes grid;
	
	int numSpheresOrig = sphere.size();
	int targetNumCells = numSpheresOrig / 10;			// ~ten spheres per cell would be nice.
	
	float maxRadius = 0.0f;
	
	for(long i=0; i<sphere.size(); i++)
	{
		grid.increaseBoundsToIncludePt( sphere[i].x, sphere[i].y, sphere[i].z );
		maxRadius  = max( maxRadius,  sphere[i].r );
	}
	
	if(maxRadius <= 0.0f)
	{
		fprintf(stderr, "ERROR: maxRadius was 0.\n");
		maxRadius = 1.0f;
	}
	
	float zSizeAdjusted = (grid.zspan+1.0f)*zScale;
	float maxDiameter   = 2.0f*maxRadius;
	float maxDiameterZ  = maxDiameter / zScale;
	float minSideLength = maxDiameter;
	
	float gridVolume  = grid.xspan * grid.yspan * zSizeAdjusted;
	float sideLen     = pow( gridVolume / (float)targetNumCells, 1.0f/3.0f );
	
	grid.setupGridWithSideLengths( sideLen, sideLen, sideLen/zScale, false );
	grid.calcGridSize( true );
	
	if( grid.totalCells > numSpheresOrig || grid.totalCells*1000 < numSpheresOrig )
	{
		(grid.totalCells > numSpheresOrig)
		?	printf("WARNING: total cells is greater than number of spheres....\n")
		: printf("WARNING: too few cells....\n");
		
		if( minSideLength > zSizeAdjusted )
		{
			printf("NOTE: grid.zspan is small, so using area metric.\n");
			float gridArea = grid.xspan * grid.yspan;
			sideLen        = pow( gridArea / (float)targetNumCells, 0.5f );
		}
		if( sideLen < minSideLength )
		{
			fprintf(stderr, "ERROR: sideLen too great so setting to minSideLength.\n");
			sideLen = minSideLength;
		}
	}
	
	grid.setupGridWithSideLengths( sideLen, sideLen, sideLen/zScale, false );
	grid.calcGridSize( true );
	
	
	//## POPULATE GRID CELLS WITH SPHERE INDEXES:
	
	for(long i=0; i<sphere.size(); i++)
		grid.addIndexToGrid( sphere[i].x, sphere[i].y, sphere[i].z, i );
	
	
	//## GO THROUGH ALL SPHERES IN ORDER, STARTING WITH THE "EXTRA SPHERES",
	//## AND FLAG SPHERES IN THE "SPHERES" VECTOR FOR DELETING:
	
	int cellGutterX = ceil( maxDiameter  / grid.xside );	// |-- number of side lengths
	int cellGutterY = ceil( maxDiameter  / grid.yside );	// |   needed to safely include
	int cellGutterZ = ceil( maxDiameterZ / grid.zside );	// |   the diameter
	
	long numSpheresFlaggedForDelete = 0;		// tallies the # of sphere we mark for deletion
	list<long>::iterator it;								// iterator for sphere indexes in each cell
	long numExtraSpheres = (useExtraSpheres) ? (long)extraSphere.size() : 0;
	
	for(long i=-numExtraSpheres; i<(long)sphere.size(); i++)		// for ALL spheres:
	{
		//## GET NEXT  "CURRENT SPHERE" TO CHECK OTHER SPHERES AGAINST:
		
		Sphere *currSp;						// the current sphere to check other spheres against
		
		if(i>=0) {												// if i is positives:
			currSp = &sphere[i];							// use the matching "sphere"
		}
		else {														// (else) if i is negative:
			int eIdx = i+numExtraSpheres;			// |
			eIdx    %= numExtraSpheres;				// |-- use the matching "extraSphere"
			currSp = &extraSphere[ eIdx ];		// |
		}
		
		if( currSp->confidence < 0.0f )		// if sphere already flagged for delete: skip it
			continue;
		
		//## DETERMINE RANGE OF CELLS TO CHECK:
		
		int xIdx = grid.getXIdx( currSp->x );		// |-- determine the x,y,z cell in the
		int yIdx = grid.getYIdx( currSp->y );		// |   grid our sphere is centered in.
		int zIdx = grid.getZIdx( currSp->z );		// |
		
		int xmin = max( xIdx - cellGutterX, 0         );		// |-- the range of grid
		int xmax = min( xIdx + cellGutterX, grid.nx-1 );		// |   cells along each
		int ymin = max( yIdx - cellGutterY, 0         );		// |   dimension which our
		int ymax = min( yIdx + cellGutterY, grid.ny-1 );		// |   sphere potentially
		int zmin = max( zIdx - cellGutterZ, 0         );		// |   spans.
		int zmax = min( zIdx + cellGutterZ, grid.nz-1 );		// |
		
		//## FOR EACH CELL IN RANGE:
		
		for( int z=zmin; z<=zmax; z++ )				// |-- for each cell in range:
		for( int y=ymin; y<=ymax; y++ )				// |
		for( int x=xmin; x<=xmax; x++ )				// |
		{
			long cIdx = grid.getIdx( x,y,z );									// determine the cell index
			CellOfIndexes &cell = grid.cell[cIdx];						// the current cell to search
			
			//## FOR EACH SPHERE INDEX IN THIS CELL, CHECK IF IT OVERLAPS OUR CURRENT SPHERE:
			
			for(it = cell.idx.begin(); it!=cell.idx.end(); )	// for each sphere index:
			{
				long otherSpIdx = *it;
				if( otherSpIdx == i || otherSpIdx < 0 )		// if itself or an already flagged for
				{																					//  elimination then we skip it
					it++;
					continue;														
				}
				
				Sphere &otherSp = sphere[ otherSpIdx ];
				
				float distBetweenCenters = calcDist3D( currSp->x,  currSp->y,  currSp->z,
																               otherSp.x, otherSp.y, otherSp.z, zScale );
				float combinedRad        = currSp->r + otherSp.r;
				bool  doSpheresOverlap   = distBetweenCenters <= combinedRad;
				
				if( doSpheresOverlap ) {				// if spheres overlap:
					*it                  = -1;			// flag this sphere for deletion
					otherSp.confidence   = -1.0f;		// set it's confidence to -1
					it = cell.idx.erase(it);				// remove this index from the cell
					numSpheresFlaggedForDelete++;		// increase tally
				}
				else {
					it++;
				}

			}
		}
	}
	
	//## REORDER SPHERES IN DESCENDING CONFIDENCE:
	
	sort( spheresOut.begin(), spheresOut.end(), compareSphereSimilarity );
	reverse( spheresOut.begin(), spheresOut.end() );
	
	
	//## REMOVE ALL SPHERES TAGGED FOR DELETION:
	
	if( markForDeleteOnly==false )
	{
		while(sphere.size() > 0 && sphere.back().confidence <= 0.0f )
			sphere.pop_back();
	}
	else
	{
		printf(" > NOTE: spheres were flagged for delete only \n" );
	}
	
	//## PRINT RESULTS:
	
	float deletedPercent = (numSpheresFlaggedForDelete * 100.0f) / (float)numSpheresOrig;
	long spheresLeft     = numSpheresOrig - numSpheresFlaggedForDelete;
	
	printf("\n");
	printf("GRID SUMMARY: \n");
	printf(" > dimensions:   %5.0f x %5.0f x %5.0f \n", grid.xspan,grid.yspan,grid.zspan );
	printf(" > side lengths: %4.2f x %4.2f x %4.2f \n", grid.xside,grid.yside,grid.zside );
	printf(" > grid size:    %d x %d x %d cells\n", grid.nx, grid.ny, grid.nz );
	printf(" > total cells:  %d \n", grid.totalCells );
	printf("\n");
	printf(" > max diameter:      %4.2f \n", maxDiameter );
	printf(" > xside / diameter:  %4.2f \n", grid.xside / maxDiameter );
	printf(" > spheres / cell:    %4f \n\n", (float)numSpheresOrig / grid.totalCells );
	//grid.print();
	
	printf(" > # (overlapping) spheres deleted: %d of %d (%4.2f%%) \n",
				 numSpheresFlaggedForDelete, numSpheresOrig, deletedPercent );
	printf(" > # spheres left:                  %d\n", spheresLeft );
	printf("\n");
		
	return true;
}


//------------------------------
//-- Inputs a vector of spheres (sphere) and outputs the minimum (minC),
//-- maximum (maxC) and average (avgC) confidence for all spheres
//-- which have a confidence above the specified minimum (minToInclude)
//-- confidence value. If (ignoreOneAndAbove) is true then values >= 1 are 
//-- skipped too. The function itself returns the number of values included
//-- in the calculation

int getMinMaxAndAvgConfidence( vector<Sphere> &sp, float &minC,float &maxC,float &avgC, 
										           float minToInclude, bool ignoreOneAndAbove )
{
	minC = FLOAT_MAX;
	maxC = FLOAT_MIN;
	avgC = 0.0f;
	int numValsIncluded = 0;
	
	for(int i=0; i<sp.size(); i++)
	{
		float conf = sp[i].confidence;
		if( conf<minToInclude || (ignoreOneAndAbove && conf >= 1.0f))
			continue;
		
		minC  = min( minC, conf );
		maxC  = max( maxC, conf );
		avgC += conf;
		numValsIncluded++;
	}
	
	if(numValsIncluded)
		avgC /= (float)numValsIncluded;
	
	return numValsIncluded;
}


//------------------------------
//-- Inputs an IMOD model file (imod) and the index of the desired object to use
//-- (objIdx), and then outputs all valid spheres in that object into the
//-- specified sphere vector (sphere). Returns -1 if the object does not
//-- exist or outputs the number of valid spheres added.
//-- 
//-- @ imod          - the model file in which spheres exist
//-- @ objIdx        - the index of the object to check
//-- @ sphere        - the sphere vector to add spheres too
//-- @ mustBeInImage - if true: any sphere not completely contained in the limits
//--                   is excluded
//-- @ inHead        - the image to use for limits if "mustBeInImage" is true

int addImodObjectSpheresToVector( Imod *imod, int objIdx, vector<Sphere> &sphere,
														      bool printErrs, bool mustBeInImage, MrcHeader &inHead, 
																  int gutterInXY, int gutterInZ, float zScale )
{
	Iobj *obj = getAndSetObj( imod, objIdx );
	if( obj==NULL )	{
		fprintf(stderr, "ERROR: Object %d does not exist\n", objIdx);
		return (-1);
	}
	
	float objSphereSize = imodObjectGetValue(obj,IobjPointSize);
	if( objSphereSize == 0  )
	{
		fprintf(stderr, "WARNING: Object %d has no default sphere size\n", objIdx);
	}
	
	int numPtsZeroRad  = 0;
	int numPtsTooBig   = 0;
	int numPtsOffImage = 0;
	int numPtsAdded    = 0;
	
	for(int c=0; c<imodObjectGetMaxContour(obj); c++ )
	{
		Icont *cont = imodObjectGetContour( obj, c );
		for(int p=0; p<imodContourGetMaxPoint(cont); p++ )
		{
			Ipoint *pt = imodContourGetPoint( cont, p);
			float ptSize = imodPointGetSize( obj, cont, p );
			
			//## CHECK FOR UNDESIRABLE POINTS:
			if( ptSize<=0 )
			{
				numPtsZeroRad++;
				continue;
			}
			if( ceil(ptSize+gutterInXY) > maxSpan )
			{
				numPtsTooBig++;
				continue;
			}
			
			//## CHECK IF OUTSIDE IMAGE:
			if(mustBeInImage)
			{
				int fullRadXY = ptSize + gutterInXY;
				int fullRadZ  = (ignoreZ) ? 0 : ceil(ptSize/zScale + gutterInZ);
				if(   ( floor(pt->x - fullRadXY) < 0 || ceil(pt->x + fullRadXY) >= inHead.nx )
					 || ( floor(pt->y - fullRadXY) < 0 || ceil(pt->y + fullRadXY) >= inHead.ny )
					 || ( floor(pt->z - fullRadZ ) < 0 || ceil(pt->z + fullRadZ ) >= inHead.nz ) )
				{
					numPtsOffImage++;
					continue;
				}
			}
			
			//## ADD POINT TO VECTOR OF SPHERES:
			sphere.push_back( Sphere(pt->x, pt->y, pt->z, ptSize, objIdx) );
			numPtsAdded++;
		}
	}
	
	int totBadPoints = numPtsZeroRad + numPtsTooBig + numPtsOffImage;
	if( totBadPoints && printErrs )
	{
		fprintf(stderr, "WARNING: %d points could not be included\n", totBadPoints);
		fprintf(stderr, " > %d had a zero radius\n", numPtsZeroRad);
		fprintf(stderr, " > %d had an extended radius > that allowed\n", numPtsTooBig);
		fprintf(stderr, " > %d had an extended radius outside the image\n\n",numPtsOffImage);
	}	
	
	return (numPtsAdded);
}

//------------------------------
//-- Inputs a vector of spheres (sphere) and uses them to generate
//-- a specified number of new objects (numNewObjs) in the specified
//-- model file (imod). Spheres are seperated into different objects
//-- based on their confidence values. Each object gets it's own
//-- range of descending cutoff values which collectively span between 
//-- 1 and the specified "min cutoff" value (absMinCutoff). Each sphere is
//-- placed into the appropriate object, except sphere with a < min cutoff
//-- confidence - these are ignored. The function itself returns the number
//-- of spheres added, or -1 if the input is bad.
//-- 
//-- By default, new objects get colored using a 5-color heatmap gradient,
//-- and their name includes their cutoff range, prefixed by (prefix)
//-- string plus "... findspheres," if (addDefPrefix) is true.
//-- As an example, if object "origObjIdx" is "Vesicle", numNewObjs is 5,
//-- and cutoff is 0.5, the objects will be:
//-- 
//--   > "Vesicle... findspheres,cutoff:1.0-0.9"   -> red
//--   > "Vesicle... findspheres,cutoff:0.9-0.8"   -> yellow
//--   > "Vesicle... findspheres,cutoff:0.8-0.7"   -> green
//--   > "Vesicle... findspheres,cutoff:0.7-0.6"   -> cyan
//--   > "Vesicle... findspheres,cutoff:0.6-0.5"   -> blue
//-- 
//-- If (scaleMaxCutoff) is true, the first object min range will be adjusted to 
//-- include the largest confidence value. In the example above, if the max 
//-- confidence of any sphere is  0.75, the objects above become:
//--   "1.0-0.7", "0.7-0.65", "0.65-0.6", "0.6-0.55", "0.55-0.5"
//-- 
//-- @ imod          - the model file to add objects to
//-- @ sphere        - the spheres to write out to the imod model
//-- @ numNewObjs    - the number of new objects we want to write out
//-- @ absMinCutoff  - min threshold for a sphere's confidence to be included
//-- @ origObjIdx    - the origional object on which spheres were determined
//-- @ addDefPrefix  - if true: will add "  .findspheres," to the prefix above
//-- @ scaleMax      - if true: the first object's cutoff will be adjusted to
//--                   include the largest confidence value
//-- @ scaleMin      - if true: min cutoff will get changed to to the smallest
//--                   confidence which is > the "absMinCutoff" specified

int addSpheresToModel( Imod *imod, vector<Sphere> &sphere, int numNewObjs,
											 float absMinCutoff, int origObjIdx, bool addDefPrefix,
											 bool scaleMaxCutoff, bool scaleMinCutoff, bool useObjColor,
											 bool multiPtsPerCont )
{
	if(sphere.size()==0 || numNewObjs <= 0)
		return -1;
	
	int numSpheresAdded   = 0;		// tallies the total spheres added to imod
	int numSpheresOverOne = 0;		// tallies the total spheres with a confidence > 1
	
	//## DETERMINE THE VALUES FOR MIN AND MAXIMUM CUTOFF:
	
	float minC, maxC, avgC;
	int numInRange = getMinMaxAndAvgConfidence(sphere, minC,maxC,avgC, absMinCutoff, true);
							// gets the minimum and maxiumum confidence values for any sphere
	            // with a confidence below 1 and >= absMinCutoff
	
	bool goodRange = numInRange > 2 && (maxC-minC) > 0.01f;
	
	float maxCutoff = (scaleMaxCutoff && goodRange) ? maxC : 1.0f;
	float minCutoff = (scaleMinCutoff && goodRange) ? minC : absMinCutoff;
	
	float confidenceSpan = (maxCutoff-minCutoff) / (float)numNewObjs;
	ColorGradient heatMapGradient;			// used to create a nice array of different colors
	float red   = 1.0f;		// |
	float green = 0.0f;		// |-- default color is red
	float blue  = 0.0f;		// |
	
	
	//## PRINT CUTOFF INFO (IF DESIRED)
	
	if(printLevel>=3)
	{
		printf("GENERATING SPHERES WHICH MATCH OBJECT %d:\n", origObjIdx+1 );
		if(printLevel>=4)
		{
			printf(" > maxCutoff=%5.4f, minCutoff=%5.4f\n", maxCutoff,minCutoff);
			printf(" > numNewObjs=%d, confidenceSpan=%2.5f \n",numNewObjs, confidenceSpan );
			printf(" > minC=%0.3f, maxC=%0.3f, avgC=%0.4f  ... numInRange=%d\n",
						   minC,maxC,avgC, numInRange);
		}
	}
	
	//## SETUP PREFIX FOR NEW OBJECTS:
	
	int defSphereSize = 10;						// default sphere size for all objects
	char prefix[1024] = "";						// prefix to add to the start of every object
	if( addDefPrefix )
	{
		Iobj *objOrig = getAndSetObj( imod, origObjIdx );
		if(objOrig)	
		{
			sprintf( prefix, "%s... findspheres", imodObjectGetName( objOrig ) );
			defSphereSize = imodObjectGetValue(objOrig, IobjPointSize);
			if(useObjColor)
				imodObjectGetColor( objOrig, &red, &green, &blue );			// update color
		}
		else
		{
			sprintf( prefix,   "... findspheres" );
		}
	}
	
	//## CREATE NEW OBJECTS AND POPULATE THEM WITH SPHERES:
	
	char newObjName[1024];
	char newObjLabel[MAX_CHARS];
	sprintf(newObjLabel, "Generated by slashfindspheres. ");
	sprintf(newObjLabel, "Confidence were min=%f, max%f, avg=%f with absMinCutoff=%f",
					minC, maxC, avgC, absMinCutoff );
	
	for(int o=0; o<numNewObjs; o++)
	{
		//## CREATE NEW OBJECT:
		
		int fail = imodNewObject( imod );
		if( fail == 1 )	{
			fprintf(stderr, "ERROR: Failed at adding new object \n" );
			exit (1);
		}
		int objIdx = imodGetMaxObject(imod)-1;
		imodSetIndex(imod, objIdx, 0, 0);
		Iobj *objNew = imodObjectGet(imod);
		
		float oMinCutoff = maxCutoff  - (o+1.0f)*confidenceSpan;	// |-- cutoff range for
		float oMaxCutoff = oMinCutoff + confidenceSpan;						// |   the current object
		if(o==0)	oMaxCutoff = 1.0f;		// our first confidence starts at 1 regardless
		
		//## SET OBJECT'S NAME AND OTHER PROPERTIES:
		
		newObjName[0] = 0;
		if(o==0)	{
			Ilabel *label = imodObjectNewLabel( objNew );
			imodLabelName( label, newObjLabel );
			sprintf(newObjName, "%s, cutoff:%0.3f-%0.3f ... maxC=%0.5f", prefix,
							oMinCutoff, oMaxCutoff, maxC);
		}
		else {
			sprintf(newObjName, "%s, cutoff:%0.3f-%0.3f", prefix, oMinCutoff, oMaxCutoff);
		}
		
		imodObjectSetName(objNew, newObjName);					// set object's name
		
		imodObjectSetValue(objNew, IobjLineWidth, (oMinCutoff>0.9f)?2:1);	// line thickness
		imodObjectSetValue(objNew, IobjFlagConnected, 0);						// scattered point object
		imodObjectSetValue(objNew, IobjPointSize, defSphereSize);		// object sphere size
		imodObjectSetColor(objNew, red, green, blue );							// default color
		
		if( !useObjColor && numNewObjs > 1 )	// if multiple objects: 
		{																				//  set object color using heatmap gradient
			float gradientVal = 1.0f - ((float)o/(numNewObjs-1));
			heatMapGradient.getColorAtValue( gradientVal, red, green, blue );
			imodObjectSetColor( objNew, red, green, blue );
		}
		
		//## CREATE A NEW CONTOUR AND ALL SPHERES IN THE CUTOFF RANGE FOR THIS OBJECT:
		
		Icont *newCont = imodContourNew();
		int numSpheresInObject = 0;
		for(int i=0; i<sphere.size() && i<maxSpheresOut; i++)
		{
			Sphere &sp = sphere[i];
			
			if( sphere[i].objIdx != origObjIdx )		// if doesn't match object index: skip
				continue;
			
			if( sp.confidence >= oMinCutoff && sp.confidence < oMaxCutoff )	// if in range:
			{
				if(multiPtsPerCont)
				{
					imodPointAppendXYZ(newCont, sp.x+0.5f, sp.y+0.5f, sp.z );			  // add point
					imodPointSetSize( newCont, numSpheresInObject, sphere[i].r );
				}
				else
				{
					newCont = imodContourNew();
					imodPointAppendXYZ(newCont, sp.x+0.5f, sp.y+0.5f, sp.z );				// add point
					imodPointSetSize( newCont, 0, sphere[i].r );
					imodObjectAddContour( objNew, newCont );                        // add contour
					free(newCont);																									// start new
				}
				
				numSpheresInObject++;
				numSpheresAdded++;
			}
			else if ( i==0 && sp.confidence > 1.0f  )			// if over one: tally this
			{
				numSpheresOverOne++;
			}
		}
		if(multiPtsPerCont)
		{
			imodObjectAddContour( objNew, newCont );												// add last contour
			free(newCont);
		}
		
		//## PRINT RESULTS (IF APPLICABLE):
		if(printLevel>=3)
		{
			printf("(%d) Obj #%d \t", o+1, objIdx+1);
			printf("'%s' \t %d spheres\n", newObjName, numSpheresInObject );
		}
	}
	
	//## PRINT ANY WARNINGS:
	
	if(numSpheresOverOne)
	{
		printf("WARNING: %d spheres with a >=1 confidence were found\n", numSpheresOverOne);
		printf("         these spheres were not added\n");
	}
	if(printLevel>=3)
		printf("\n\n");
	
	return (numSpheresAdded);
}

//############################################################
//## INLINE FUNCTIONS:


//------------------------------
//-- Returns a value between 0 and 1 which represents the "similarity" between
//-- two float values (val1) and (val2). Identical values will return 1 and
//-- values >= (valRange) apart will return 0.
//-- 
//-- Note that (valRange) argument represents a resonable range we might
//-- expect our floats to be spaced apart. For instance if pixels in an image
//-- are all between 0 and 255 (byte range) then you might set valRange to
//-- 256 and if your pixel grey values are 50 and 60 it would return 0.96 ->
//--   correlation = 1 - (absDiff / valRange)
//--               = 1 - (10/256) = 0.96

inline float calcCorrelation( float val1, float val2, float valRange )
{
	float absDiff    = fabs(val1-val2);			// absolute difference between values
	float relDiff    = absDiff / valRange;	// relative difference against range
	float similarity = 1.0f - relDiff;			// "similarity" measure where 1 is identical
	return (similarity>0) ? similarity : 0.0f;	// don't return less than 0
}

//------------------------------
//-- Returns the distance between two points (x1,y1,z1) and (x2,y2,z2) in 3D
//-- after the z axis has been multipled by the given (zScale)
//-- NOTE: Same result as "imodPoint3DScaleDistance()", but doesn't need Ipoints

inline float calcDist3D( float x1, float y1, float z1,
												 float x2, float y2, float z2, float zScale )
{
	float xDiff = x2 - x1;
	float yDiff = y2 - y1;
	float zDiff = (z2 - z1) * zScale;
	
	return sqrt( xDiff*xDiff + yDiff*yDiff + zDiff*zDiff );
}

//------------------------------
//-- Calculates and returns an appropriate "weighting" for a point which is a 
//-- specified distance (dist) from the center of a sphere with a given (radius)
//-- and a special (gutter) distance around the sphere to form an "extended radius"
//-- beyond which 0 is returned.
//-- 
//-- If the point is:
//--   > within a pixel of the center -> it returns (centerWeight)
//--   > inside the sphere            -> it returns (innerWeight)
//--   > outside the extended radius  -> it returns 0
//--   > outside the sphere but inside the extended radius
//--                                  -> uses a function of (outerWeight) depending on
//--                                     the type of (wtStrategy) applied (see below)

inline float calcWeight( float dist, float radius, float gutter,
												 float centerWeight, float innerWeight, float outerWeight,
												 weightstrategy wtStrategy, float falloffRate )
{
	float extendedRadius = radius + gutter;		// represents our "extended radius"
	
	if( dist <= 0.5f )								// if center-most pixel:
	{
		return (centerWeight);
	}
	else if( dist <= radius )					// if inside radius:
	{
		return (innerWeight);
	}
	else if (dist <= extendedRadius)	// if between radius and extendedRadius:
	{
		float fractDist = 0.5f;						// fractional distance between radius (1) 
		if (gutter!=0.0f)									//  and extendedRadius (0)
			fractDist = (extendedRadius - dist) / gutter;
		
		switch (wtStrategy)
		{
			case (WS_NONE):					// WS_NONE     -> will just return (outerWeight)
			{
				return (outerWeight);	
			}
			case (WS_FALLOFF):			// WS_FALLOFF  -> falls off by using exponential funtion
			{												//                (fractDist^falloffRate) as it gets
				if(falloffRate > 0)		//                further away from sphere
					return (outerWeight * pow(fractDist,falloffRate));
				return (0.0f);
			}
			case (WS_HALFDOWN):			// WS_HALFDOWN -> returns (outerWeight) if less than
			{												//                halfway from radius to extended radius
				if(fractDist>=0.5)    //                and drops linearly to 0 after that
					return (outerWeight);
				return (outerWeight*2.0f*fractDist);
			}
		}
	}
	else											// if outside extendedRadius:
	{
		return (0.0f);
	}
	
	return (-1.0f);
}

//------------------------------
//-- Returns the object at the given index (objIdx) in the given model (imod)
//-- by calling "imodSetIdex" or returns NULL if that object index does not exist.

inline Iobj* getAndSetObj( Imod *imod, int objIdx )
{
	if( objIdx < 0 || objIdx >= imodGetMaxObject(imod)  )
		return NULL;
	imodSetIndex( imod, objIdx, 0, 0);
	return imodObjectGet(imod);
}


//------------------------------
//-- Returns true if the input character (chr) is a number (between '0' and '9') 

inline bool isDigit( char chr )
{
	return (chr >= '0' && chr <='9');
}

//------------------------------
//-- Returns an integer representing the digit value (0-9) of a character

inline int getDigitVal( char chr )
{
	return ( (int)chr - (int)'0' );
}

//------------------------------
//-- Keeps a given integer (value) within the specified (min) and (max) values
//-- inclusive. Teturns true if the value had to be changed, or false if it was
//-- already inside the limits

inline bool keepInLimits( int &value, int min, int max )
{
	if( value < min ) 	{  value = min;	return true;  }
	if( value > max ) 	{  value = max;	return true;  }
	return false;
}

//------------------------------
//-- Inputs a cstring (arg) representing a list of integers in a form such as
//-- "1,-2,5-7,9", plus a starting position (pos) and returns a vector (vect)
//-- of these integers (eg: "1,-2,5,6,7,9") from the starting point forwards.
//-- Will returns -1 if there was an error (empty arg list or bad pos value)
//-- or else the final position of (pos) as it iterates forwards and either
//-- gets to the end or encounters a character which it does not recognize
//-- (if stopOnOtherChars is true). If the function returns the string length
//-- you can assume it processed all the way with no unexpected values.
//-- 
//-- @ arg        = input array of characters
//-- @ vect       = vector of integers to output
//-- @ pos        = index within "arg" to start processing characters
//-- @ allowNeg   = if true: allows negative ints by searching for '-' before digits
//-- @ allowRange = if true: allows ranges of number by searching for '-' between 
//--                numbers that are no seperated by a separator charachter (eg: 1-3)
//--                and will expand each range to include each number (ie: 1,2,3)
//-- @ sep        = the seperator character to look for (typically ',')
//-- @ sep2       = an alternate separator character (typically ';', but can be set to
//--                the same value as 'sep' if needed
//-- @ stopOnOtherChars = if true, then the funtion will exit early as soon as it
//--                encounters a character which is not "0-9","sep","sep2" or "-",
//--                otherwise this "unrecognized" characters will simply be ignored
//--                and the string gets processed to the end.

inline int processIntList( char *arg, vector<int> &vect, int pos,
													 bool allowNeg, bool allowRange, 
													 char sep,  char sep2, bool stopOnOtherChars )
{
	if(arg==NULL || strlen(arg)==0 || pos<0 || pos>=strlen(arg))
		return -1;		// error checking
	
	vect.clear();
	
	int  len = strlen(arg);
	
	int  number     = 0;				// tracks the current integer value as digits are found
	bool numFound   = false;		// set to true whenever last valid character is a digit
	bool rangeFound = false;		// set to true when a range character is found (eg: 1-5)
	int  sign       = 1;				// gets set to -1 if our number is negative (eg: -1,-4)
	
	for(; pos<len; pos++)
	{
		char &ch = arg[pos];
		
		if(!numFound && allowNeg && ch=='-')					// if negative sign: flag it
		{
			sign *= -1;
		}
		else if(numFound && allowRange && ch=='-')		// if range sign: add num & flag
		{
			if(numFound)
				vect.push_back( sign*number );
			number        = 0;
			numFound      = false;
			rangeFound    = true;		// flag
			sign          = 1;
		}
		else if(numFound && (ch==sep || ch==sep2))		// if seperator ch: add number
		{
			if(rangeFound && vect.size()>0)										// if range was started:
			{
				for(int j=vect.back()+1; j<number; j++)						// add all numbers starting  
					vect.push_back( j );														//  from prev number found
			}
			vect.push_back( sign*number );										// add number just found
			number      = 0;
			numFound    = false;
			rangeFound  = false;
			 sign       = 1;
		}
		else if(isDigit(ch))													// if characer is digit: update tally
		{
			number   *= 10;
			number   += getDigitVal(ch);
			numFound  = true;
		}
		else																					// if none of above:
		{
			if(stopOnOtherChars)														// will exit early if desired
				break;
		}
	}
	if(numFound)
		vect.push_back( sign*number );
	
	return pos;
}


//------------------------------
//-- Inputs a cstring (arg) representing a list of positive integers in the form
//-- "1,2,5-7,9" and returns a vector (vect) of these integers (eg: "1,2,5,6,7,9").
//-- If "acceptRange" is true, then ranges of integers (eg: 5-7) will be expanded
//-- to their full range (eg: 5,6,7), otherwise it will be treated the same as
//-- a comma. Any characters which are not numbers (0-9) commas or dashes are ignored.
//-- Returns the number of integers found or -1 if the input string is null.

inline int processPositiveIntList( char *arg, vector<int> &vect, bool acceptRange )
{
	if(arg==NULL)	return -1;		// error checking
	
	processIntList(arg, vect, 0, false, true, ',', ';', false);
	return vect.size();
}

//------------------------------
//-- Inputs a cstring (arg) in the form "10,20" and returns two positive integers 
//-- (int1) and (int2) which should be separated by a single comma in the input string.
//-- Returns 0 if successful or 1 if the wrong number of comma seperated integers
//-- were found, or input was null.

inline int processTwoPositiveInts( char *arg, int &int1, int &int2 )
{	
	if(arg==NULL)	return 1;		// error checking
	
	vector<int> intVect;
	processIntList( arg, intVect, 0, false, false, ',', ';', false );
	
	if(intVect.size() >= 1)		int1 = intVect[0];
	if(intVect.size() >= 2)		int2 = intVect[1];
	
	return (intVect.size()==2 ? 0 : 1 );
}


//------------------------------
//-- Inputs a string (args) in the form "m5,s,a0:150" and processes/outputs this as a 
//-- vector of Filters (filt). If any of the parameters are unrecognize it will
//-- print out error messages, but will still process the rest. The function will
//-- return the number of valid filters found or -1 if args is empty. 

inline int processFilterList( char *arg, vector<Filter> &filt )
{
	if(arg==NULL || strlen(arg)==0)	return -1;		// error checking
	
	filt.clear();
	if(arg[0]=='0')						// if the string reads '0' we don't want any filters
		return 0;
	
	int len = strlen(arg);
	Filter newFilter;
	
	vector<int> vectInts;
	
	int nextInt = 0;
	int errors  = 0;
	
	for(int i=0; i<len; i++ )
	{
		newFilter.reset();
		
		vectInts.clear();
		int newPos = 0;
		if(i+1<len)
			newPos = processIntList(arg, vectInts, i+1, true, false, ':', ';', true);
		
		switch( arg[i] )
		{
			case('m'):
			{
				newFilter.filterType = FT_MEDIAN;					// MEDIAN FILTER
				if( vectInts.size()==1 )
				{
					newFilter.sizeXY = vectInts[0];
					i = newPos;
				}
				if( newFilter.sizeXY <= 1 || newFilter.sizeXY % 2==0 )
				{
					fprintf(stderr,"ERROR: median filter should be 'm#' where # is odd and >=3\n");
					errors++;
				}
				else
				{
					filt.push_back( newFilter );
				}
				break;
			}
			case('s'):
			{
				newFilter.filterType = FT_SOBEL;					// SOBEL (EDGE DETECTION) FILTER
				filt.push_back( newFilter );
				break;
			}
			case('p'):
			{
				newFilter.filterType = FT_PREWITT;				// PERWITT (EDGE DETECTION) FILTER
				filt.push_back( newFilter );
				break;
			}
			case(','):
			{
				break;
			}
			default:
			{
				errors++;
				break;
			}
		}
	}
	
	if(errors)
	{
		fprintf(stderr, "ERROR: your filter argument '%s' has errors\n", arg);
		fprintf(stderr, "       %d of these characters were unrecognized...\n", errors);
		fprintf(stderr, "       see the slashfindspheres man page for syntax.\n\n");
	}
	
	return (filt.size());
}



//------------------------------
//-- Outputs an MRC by taking an input MRC file (inHead) then applies the
//-- selcted filters (filt). If "outputCuttoffVals" is on, then the 
//-- pxTemplate vector is applied to selected slices (zMin, zMax)
//-- to show the cutoff value at each pixel multipled by 255.

bool writeOutMrcFileAfterFilters( MrcHeader &inHead, char outfile[MAX_CHARS],
										              vector<Filter> &filt)
{
	char newLabel[MAX_CHARS];
	imodBackupFile(outfile);
	FILE *gfout = iiFOpen(outfile, "wb");
	if (gfout == NULL) {
		fprintf(stderr, "ERROR: Could not open %s", outfile);
		return false;
	}
	
	int rangeZ = max(inHead.nz, 1);
	
	MrcHeader outHead;
	mrc_head_new(&outHead, inHead.nx, inHead.ny, rangeZ, MRC_MODE_BYTE);
	mrc_head_label_cp(&outHead, &inHead);
	sprintf(newLabel, "slashfindspheres: intermediate file");
	mrc_head_label(&outHead, newLabel);
	
	outHead.amin  = FLOAT_MAX;
	outHead.amax  = FLOAT_MIN;
	outHead.amean = 0;
		
	for(int z=0; z<rangeZ; z++)
	{
		printf(" .");	fflush(stdout);
		
		LoadedSlice *loadSl = sliceBuffer.getSlice(&inHead,z,filt);	  // loads and applies
		Islice *sl          = loadSl->sl;															//  filters to slice
		
		loadSl->updateMinMaxAndMean();																// updates values
		if(printLevel>=8)
			printf("levels after convert: "); loadSl->print();
		
		sliceNewMode( sl, MRC_MODE_BYTE );		// converts (modified) float values to bytes 
		
		outHead.amin   = min(outHead.amin, sl->min);		// |-- updates min, max and
		outHead.amax   = max(outHead.amax, sl->max);		// |   average levels over
		outHead.amean += sl->mean / outHead.nz;					// |   entire image (all slices)
		
		if (mrc_write_slice(sl->data.b, gfout, &outHead, z, 'Z'))	// writes bytes of slice
			fprintf(stderr, "ERROR: Writing slice at Z=%d", z);     //  out to our file
	}
	outHead.amean = outHead.amean / (float)rangeZ;
	
	if (mrc_head_write(gfout, &outHead))								// write final header to file
		fprintf(stderr, "ERROR: Writing header to final output file");
	iiFClose(gfout);
	
	printf("done\n\n");
	return true;
}

//------------------------------
//-- Outputs an MRC by taking an input MRC file (inHead) then applies the
//-- selcted filters (filt). If "outputCuttoffVals" is on, then the 
//-- pxTemplate vector is applied to selected slices (zMin, zMax)
//-- to show the cutoff value at each pixel multipled by 255.

bool writeOutMrcFileWithCutoffVals( MrcHeader &inHead, char outfile[MAX_CHARS],
																	  vector<Filter> &filt, int zMin, int zMax,
																	  int maxXYRad, int maxZRad, float zScale )
{
	char newLabel[MAX_CHARS];
	imodBackupFile(outfile);
	FILE *gfout = iiFOpen(outfile, "wb");
	if (gfout == NULL) {
		fprintf(stderr, "ERROR: Could not open %s", outfile);
		return false;
	}
	
	int rangeZ = max(inHead.nz, 1);
	
	MrcHeader outHead;
	mrc_head_new(&outHead, inHead.nx, inHead.ny, rangeZ, MRC_MODE_BYTE);
	mrc_head_label_cp(&outHead, &inHead);
	sprintf(newLabel, "slashfindspheres: intermediate file");
	mrc_head_label(&outHead, newLabel);
	
	outHead.amin  = FLOAT_MAX;
	outHead.amax  = FLOAT_MIN;
	outHead.amean = 0;
	
	LoadedSlice cutoffSlice;				// slice to store cutoff slice in
	cutoffSlice.sl = sliceCreate(inHead.nx, inHead.ny, MRC_MODE_FLOAT);
	
	PixTemplate currPixTemp;        // pixel template used on each pixel
	Sphere currSp;									// represents our current point
	
	
	int xmin = inHead.nx-1;
	int ymin = inHead.ny-1;
	
	for(int z=0; z<rangeZ; z++)
	{
		LoadedSlice *loadSl = sliceBuffer.getSlice(&inHead,z,filt);	  // loads and applies
		Islice *sl          = loadSl->sl;															//  filters to slice
		
		bool applyCutoff = z>=zMin && z<=zMax;
		
		printf(" .");	fflush(stdout);
		
		if(applyCutoff)
		{
			sliceNewMode( cutoffSlice.sl, MRC_MODE_FLOAT );
			cutoffSlice.zVal = z;			
			
			//## CALCULATE CUTOFF VALUES ON SLICE:
			
			float *cutSlData     = cutoffSlice.sl->data.f;								  	// slice data
			
			for(int y=0; y<=ymin; y++)					
			for(int x=0; x<=xmin; x++)
			{
				currSp.set( (float)x, (float)y, (float)z, 0.0f, -1);
				currSp.confidence = 0.0f;
				
				populateTemplate( currPixTemp, currSp, inHead,
												  maxXYRad, maxZRad, false, zScale );			// gives seg fault and not sure why %%%%%%%
				float fractValidPx = currPixTemp.setWeightsForValidPixels(1,0);
				
				for(int i=0; i<pixTemplate.size(); i++)
				{
					float correlation = pixTemplate[i].calcCorrelationOn( currPixTemp, true );
					if(currSp.confidence < correlation)
						currSp.confidence = correlation;
				}
				
				int iIdx  = y*inHead.nx + x;											// position in image
				float val = currSp.confidence * 255.0f;
				cutSlData[iIdx] = val;
				
				if(x==0 && (y+1)%100==0) { printf("."); fflush(stdout); }
			}
			
			//## UPDATE AND OUTPUT SLICE MIN/MAX/MEAN VALUES:
			
			cutoffSlice.updateMinMaxAndMean();																// updates values
			if(printLevel>=8)
				printf("levels after convert to cutoff: ");
			cutoffSlice.print();
			
			sliceNewMode( cutoffSlice.sl, MRC_MODE_BYTE );	// converts float values to bytes
			
			outHead.amin   = min(outHead.amin, cutoffSlice.sl->min);
			outHead.amax   = max(outHead.amax, cutoffSlice.sl->max);
			outHead.amean += cutoffSlice.sl->mean / outHead.nz;
			
			if (mrc_write_slice(cutoffSlice.sl->data.b, gfout, &outHead, z, 'Z'))
				fprintf(stderr, "ERROR: Writing slice at Z=%d", z);
		}
		else
		{
			loadSl->updateMinMaxAndMean();																// updates values
			if(printLevel>=8)
				printf("levels after convert: "); loadSl->print();
			
			sliceNewMode( sl, MRC_MODE_BYTE );		// converts (modified) float values to bytes
			
			outHead.amin   = min(outHead.amin, sl->min);		// |-- updates min, max and
			outHead.amax   = max(outHead.amax, sl->max);		// |   average levels over
			outHead.amean += sl->mean / outHead.nz;					// |   entire image (all slices)
			
			if (mrc_write_slice(sl->data.b, gfout, &outHead, z, 'Z'))	// writes bytes of slice
				fprintf(stderr, "ERROR: Writing slice at Z=%d", z);     //  out to our file
			
			sliceNewMode( sl, MRC_MODE_FLOAT );		// must change back to floats
		}
	}
	outHead.amean = outHead.amean / (float)rangeZ;
	
	if (mrc_head_write(gfout, &outHead))								// write final header to file
		fprintf(stderr, "ERROR: Writing header to final output file");
	iiFClose(gfout);
	
	printf("done\n\n");
	return true;
}


//############################################################


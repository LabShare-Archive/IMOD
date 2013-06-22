/*
 *  slashmasksort.cpp -- used sort contour or points into new objects based on if
 *                       they fall inside one of the specified mask objects
 *  
 *  Author:     Andrew Noske
 *  Revised by: David Mastronarde (once finished)   email: mast@colorado.edu
 *  
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *  
 * $Id: slashmasksort.cpp
 */

//############################################################

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "imodel.h"

#include <algorithm>
#include <vector>
#include <list>
#include <limits.h>
#include <math.h>

using namespace std;

//############################################################
//## CONSTANTS:

const int MAX_CHARS     = 4096;				// number of characters in file path and labels

const int IS_OUTSIDE    = -1;					// signifies a point outside all mask contours
const int OUT_OF_RANGE  = -2;					// signifies a point outside the z range
const int NO_POINTS     = -3;					// signifies a contour with no points

//############################################################
//## MAIN PROGRAM VARIABLES:

static vector<int> objList;						// the list of objects we want to "sort/split"
static vector<int> objMaskList;			  // the list of closed objects to use as a mask
                                      //  and "set of boundaries" for sorting conts

static bool  splitPoints   = false;		// if true, individual points (not just contours)
                                      //  may be split into different objects
static bool  cutContsOpen  = false;		// if true: split contours will all be
                                      //  turned into open contours

static bool  insertInPlace = false;		// if true, new objects will be added just after 
																			//  each mask object
static bool  deleteOld     = false;		// if true, original objects (in objList)
                                      //  will be deleted
static bool  keepOutside   = false;   // if true, contours/points outside all masks
                                      //  will get added to a seperate "OUTSIDE" object
static bool  firstPtOnly   = false;   // if true, will test only test the first point
                                      //  if each contour to see what mask contour/object
                                      //  it may fall inside
static bool  newColors     = false;   // if true, new objects get different colors
                                      //  instead of the same color as original object
static bool  useObjNums    = false;   // if true, will use object numbers instead of 
																			//  object names to label new objects

//############################################################
//## CLASS DECLARATIONS:

class ClosedCont;
class ContourListZ;

//############################################################
//## FUNCTION DECLARATIONS:

bool addContPtsToObj( Iobj *obj, Icont *cont, int startPt, int endPt, bool cutOpen );
bool addContToObj( Iobj *obj, Icont *cont );
int determineWhatObjIdPointIsIn( Ipoint *pt,  vector<ContourListZ> &contList );
int determineWhatObjIdContIsIn ( Icont *cont, vector<ContourListZ> &contList,
															   bool testFirstPtOnly );


inline Iobj* getAndSetObj( Imod *imod, int objIdx );
inline Iobj* addNewObj   ( Imod *imod );

inline int processIntList( char *arg, vector<int> &vect, int pos=0,
													bool allowNeg=true, bool allowRange=false, 
													char sepCh1=',', char sepCh2=';', bool stopOnOtherChars=true );
inline int processPositiveIntList( char *arg, vector<int> &vect, bool acceptRange );


//############################################################
//## STRUCTURES:

//** ClosedCont - stores a closed contour, plus the object index 
//** and object id it belongs to

class ClosedCont
{
public:
	Icont *cont;		// a contour
	int objIdx;			// the object index this contour comes from
	int objId;      // an object ID
	
	ClosedCont()	{
		cont   = NULL;
		objIdx = -1;
		objId  = -1;
	}
};


//** ContourListZ - stores a vector of "ClosedCont" objects which all belong
//** to the same z level

class ContourListZ
{
public:
	vector<ClosedCont> cont;			// a vector of contours at this z level
	float zVal;										// the z value of these contours
	
	ContourListZ() {
		zVal = -1;
	}
	
	void print()	{
		printf( "ContourListZ: z=%d, number contours=%d\n",zVal,cont.size() );
	}
};





//############################################################


//## GLOBAL VARIABLES:

vector<ContourListZ>   contList;		// stores all mask contours at each z level


//------------------------------

static void usage(void)
{
  printf("\n");
	printf("Sorts/splits contours into a set of new objects based on whether its \n");
	printf("points fall inside any closed contours in the specified 'mask' object(s).\n");
	printf("\n");
	printf("Usage: slashmasksort -o <list> -m <list> [opts] <input_mod> <output_mod>\n");
  printf("Options:\n");
  printf("   -o <lst>  list of input objects (eg: '1,4-5') containing contours/points\n");
	printf("             that you wish to sort/split into new objects \n");
	printf("   -m <lst>  list of mask objects which must be closed contour and are used\n");
	printf("             as masks/boundaries to sort the main list of input objects\n");
	printf("   -s        test each point and allow contours to be split into separate \n");
	printf("             objects - by default only the first point of each contour is \n");
	printf("             tested and whole contours are kept intact)\n");
	printf("   -C        split contour will be marked as open (if -s is on)\n");
	printf("   -i        insert new objects just after mask objects they belong to\n");
	printf("   -d        delete all objects in the '-o' list after they are split\n");
	printf("   -k        keep contours/points outside mask objects and put them \n");
	printf("             into a separate object \n");
	printf("   -f        only test the first contour point of each '-o' contour \n");
	printf("             to see what mask object it might be inside  \n");
	printf("   -c        give new objects new colors (else will stay same colors)\n");
	printf("   -n        use object numbers instead of names to make new object names\n");
	printf("\n");
}


//------------------------------
//-- Main function - takes in user entered arguments, then starts processing
//-- input object into a vector of contours ...

int main(int argc, char *argv[])
{
  int i=0;									// tracks number of arguments
	Imod *imod = NULL;				// model file which is input, modified then written out
	
	
	//## VERIFY CORRECT NUMBER OF ARGUMENTS:
	
	printf("\n\n");
	char *progname = imodProgName(argv[0]);
	
  if (argc < 7)
	{
    imodVersion(progname);
    imodCopyright();
    usage();
    exit(3);
  }
  b3dSetStoreError(-1);		// make library error output to stderr go to stdout
  
	//## PROCESS OPTIONAL ARGUMENTS:
	
  for (i = 1; i < argc; i++)
	{
    if (argv[i][0] == '-')
      switch (argv[i][1])
			{
				case 'o':			processPositiveIntList(argv[++i], objList,      true);		break;
				case 'm':			processPositiveIntList(argv[++i], objMaskList,  true);		break;
				
				case 's':			splitPoints   = true;																			break;
				case 'C':			cutContsOpen  = true;																			break;
				case 'i':			insertInPlace = true;																			break;
				case 'd':     deleteOld     = true;																			break;
				case 'k':     keepOutside   = true;																			break;
				case 'f':     firstPtOnly   = true;																			break;
				case 'c':     newColors     = true;																			break;
				case 'n':     useObjNums    = true;																			break;
				
				default:			usage();			exit(3);																		break;
      }
    else
      break;
  }
	
	
	//## OPEN INPUT IMOD FILE:
	
	imod = imodRead(argv[argc-2]);
  if (!imod){
    fprintf(stderr, "ERROR: Problem reading imod model %s\n", argv[argc-2]);
    exit(3);
  }
	int numObjectsAtStart = imodGetMaxObject(imod);
	
	
	//## PROCESS OBJECT LISTS AND CHECK FOR ERRORS:
	
	int badObjects = 0;
	int numSplitObjs = (int)objList.size();
	int numMaskObjs  = (int)objMaskList.size();
	int numNewObjs   = numSplitObjs * numMaskObjs;
	
	if( numSplitObjs==0 )							// if no input objects specified
	{
		fprintf(stderr, "ERROR: You have not entered any objects to split (-o)\n" );
		exit(3);
	}
	if( numMaskObjs==0 )							// if no input objects specified
	{
		fprintf(stderr, "ERROR: You have not entered any closed objects to use \n");
		fprintf(stderr, "       as masks for splitting contours (-b)\n" );
		exit(3);
	}
	
	for(int i=0; i<numSplitObjs; i++)
	{
		for(int j=0; j<numMaskObjs; j++)
			if( objList[i]==objMaskList[j] )
				printf("WARNING: Object %d appears as both a split and mask object.\n", i );
	}
	
	for(int i=0; i<numSplitObjs; i++)
	{
		objList[i]--;
		if( objList[i] < 0 || objList[i] >= numObjectsAtStart )
			badObjects++;
	}
	for(int i=0; i<numMaskObjs; i++)
	{
		objMaskList[i]--;
		if( objMaskList[i] < 0 || objMaskList[i] >= numObjectsAtStart )
			badObjects++;
	}
	
	if( badObjects )
	{
		fprintf(stderr, "ERROR: You have entered %d bad object numbers.\n", badObjects );
		fprintf(stderr, "       All objects should be between 0 and the highest object \n");
		fprintf(stderr, "       number in your model (%d).\n", numObjectsAtStart );
		exit(3);
	}
	
	
	//## DETERMINE MINIMUM AND MAXIMUM Z VALUES IN MODEL:
	
	Ipoint minPt, maxPt;
	imodGetBoundingBox(imod, &minPt, &maxPt);
	int minz = minPt.z;
	int maxz = maxPt.z;
	
	printf("MODEL SUMMARY:\n" );
	printf("  min z:  %d\n", minz );
	printf("  max z:  %d\n", maxz );
	printf("  # of objects to split:  %d\n", numSplitObjs );
	printf("  # of mask objects:      %d\n\n", numMaskObjs );
	
	//## CREATE A SET OF NEW OBJECTS (TO THE END OF THE IMOD MODEL)
	//## TO SPLIT CONTOURS INTO:
	
	vector< vector<int> > newObjIdx;			// will store a list of object indexes to show
	newObjIdx.resize( numSplitObjs );		  //  where each "split object", "mask object"
	                                      //  pair maps to a new object
	
	char splitObjName[1024];
	char maskObjName [1024];
	char newObjName  [1024];
	float red, green, blue;
	
	for(int s=0; s<numSplitObjs; s++)		// for each object to split
	{
		Iobj *objS = getAndSetObj( imod, objList[s] );
		sprintf( splitObjName, "%s", imodObjectGetName(objS) );
		if( useObjNums || strlen(splitObjName) == 0 )
			sprintf( splitObjName, "object %d", objList[s]+1 );
		
		for(int m=0; m<numMaskObjs; m++)		// for each mask object to split.
		{
			Iobj *objNew = addNewObj(imod);							// create a new object in the model
			imodObjectGetColor( objNew, &red, &green, &blue );		// get object colors
			
			Iobj *objM = getAndSetObj( imod, objMaskList[m] );	// our mask object
			Iobj *objDup = imodObjectDup( objS );		// duplicate our split object
			imodObjectCopy( objDup, objNew );				// copy properties (color, name etc)
																							//  and contours of objS to our new object
			
			while( imodObjectGetMaxContour(objNew) > 0 )		// remove all contours from objNew
				imodObjectRemoveContour(objNew, 0);
			
			if(newColors)																			// if we want new colors:
				imodObjectSetColor(objNew, red, green, blue);			// use orig assigned colors
			
			sprintf( maskObjName, "%s", imodObjectGetName(objM) );
			if( useObjNums || strlen(maskObjName) == 0 )
				sprintf( maskObjName, "object %d", objMaskList[m]+1 );
			
			sprintf( newObjName, "%s... masked by '%s'",
							 splitObjName, maskObjName );
			imodObjectSetName(objNew, newObjName);					// set object's name
			
			newObjIdx[s].push_back( imodGetMaxObject(imod)-1 );
		}
		
		if(keepOutside)		// if keep inside is on, add an extra object on the end
		{
			Iobj *objNew = addNewObj(imod);				// create new object
			imodObjectCopy( objS, objNew );				// copy properties (color, name etc)
			sprintf( newObjName, "%s... masked OUTSIDE",	 imodObjectGetName(objS) );
			imodObjectSetName(objNew, newObjName);					// set object's name
			newObjIdx[s].push_back( imodGetMaxObject(imod)-1 );
		}
	}
	
	int numObjectsAfterSplit = imodGetMaxObject(imod);
	
	
	//## POPULATE A VECTOR OF CONTOURS (contList) REPRESENTING BOUNDARIES TO
	//## TEST AGAINST AT EACH Z VALUE:
	
	contList.resize( maxz+1 );
	for( int z=0; z<contList.size(); z++ )
		contList[z].zVal = z;
	
	int numMaskConts  = 0;
	int numBadConts       = 0;
	ClosedCont newClosedCont;
	
	for( int b=0; b<objMaskList.size(); b++ )
	{
		int objIdx = objMaskList[b];
		Iobj *obj  = getAndSetObj( imod, objIdx );
		if( obj==NULL )	{
			fprintf(stderr, "ERROR: Object %d does not exist\n", objIdx);
			continue;
		}
		
		for(int c=0; c<imodObjectGetMaxContour(obj); c++ )
		{
			Icont *cont = imodObjectGetContour( obj, c );
			int z = imodContourZValue(cont);
			
			if(z < 0 || z > maxz) {
				numBadConts++;
				continue;
			}
			
			newClosedCont.cont   = cont;				// copy pointer to mask contour
			newClosedCont.objIdx = objIdx;
			newClosedCont.objId  = b;
			
			contList[z].cont.push_back( newClosedCont );
			
			numMaskConts++;
		}
	}
	
	//## FOR OBJECT IN INPUT LIST, TEST ALL IT'S CONTOUR AND/OR POINT AGAINST 
	//## THE contList VECTOR TO FIND WHICH BOUNDARY OBJECT IT BELONG IN
	//## AND THEN ADD IT TO THE CORRESPONDING NEW OBJECT:
	
	int numContsSorted = 0;
	int numContsSplit  = 0;
	
	for( int s=0; s<objList.size(); s++ )
	{
		Iobj *objS = getAndSetObj( imod, objList[s] );
		if( objS==NULL )	{
			fprintf(stderr, "ERROR: Object %d does not exist\n", objList[s]+1);
			continue;
		}
		
		if(splitPoints)		// if we want to consider points individually:
		{
			for(int c=0; c<imodObjectGetMaxContour(objS); c++ )
			{
				Icont *cont = imodObjectGetContour( objS, c );
				int numPts  = imodContourGetMaxPoint(cont);
				if(numPts==0)
					continue;
				int lastObjIdx     = -1;
				int firstPtThisObj = 0;
				
				for(int p=0; p<numPts; p++)
				{
					Ipoint *pt    = imodContourGetPoint(cont, p);
					int objId     = determineWhatObjIdPointIsIn( pt, contList );
					int objIdxAdd = -1;			// object index to add to
					if     (objId>=0)														objIdxAdd = newObjIdx[s][objId];
					else if(objId==IS_OUTSIDE && keepOutside)		objIdxAdd = newObjIdx[s].back();
					else																				objIdxAdd = -1;
					
					if(lastObjIdx != objIdxAdd)						// if end of a set found:
					{
						if( lastObjIdx >= 0 )
						{
							Iobj *objAdd  = getAndSetObj( imod, lastObjIdx );
							if(objAdd == NULL)
								fprintf(stderr, "ERROR: Object %d does not exist\n", lastObjIdx+1);
							addContPtsToObj( objAdd, cont, firstPtThisObj, p-1, cutContsOpen );	
																																		// add prev set
						}
						firstPtThisObj = p;
						
						numContsSplit++;
					}
					
					if(p == numPts-1 && objIdxAdd >= 0)		// if last point and this set not added:
					{
						Iobj *objAdd  = getAndSetObj( imod, objIdxAdd );
						if(objAdd == NULL)
							fprintf(stderr, "ERROR: Object %d does not exist\n", objIdxAdd+1);
						addContPtsToObj( objAdd, cont, firstPtThisObj, numPts-1, cutContsOpen );	
																																		// add final set
						
						numContsSplit++;
					}
					
					
					lastObjIdx = objIdxAdd;
				}
				
				
				numContsSorted++;
			}
		}
		
		
		else					// else (if we want whole contour moved):
		{
			for(int c=0; c<imodObjectGetMaxContour(objS); c++ )
			{
				Icont *cont   = imodObjectGetContour( objS, c );
				int objId     = determineWhatObjIdContIsIn( cont, contList, firstPtOnly );
				int objIdxAdd = -1;			// object index to add to
				if     (objId>=0)														objIdxAdd = newObjIdx[s][objId];
				else if(objId==IS_OUTSIDE && keepOutside)		objIdxAdd = newObjIdx[s].back();
				else																				objIdxAdd = -1;
				
				if(objIdxAdd >= 0)
				{
					Iobj *objAdd  = getAndSetObj( imod, objIdxAdd );
					addContToObj( objAdd, cont );
				}
				
				numContsSorted++;
			}
		}
	}	
	
	
	//## OUTPUT DETALIED RESULTS:
	
	fprintf(stdout, "OBJECT OUTPUT:\n" );
	for(int s=0; s<numSplitObjs; s++)
	{
		int numContsSplit = imodObjectGetMaxContour( getAndSetObj( imod, objList[s] ) );
		for(int m=0; m<numMaskObjs;  m++)
		{
			Iobj *obj     = getAndSetObj( imod, newObjIdx[s][m] );
			int numConts  = imodObjectGetMaxContour(obj);
			float percent = (numContsSplit==0) ? -1 : (numConts * 100) / numContsSplit;
			printf("  > \"%s\"", imodObjectGetName(obj));
			printf("\t%d conts \t(%0.1f%%)\n", imodObjectGetMaxContour(obj), percent );
			if(percent==-1)	printf("   ... were no contours in split obj\n)" );
		}
		if(numSplitObjs>1) printf( "\n" );
	}
	
	
	//## IF APPLICABLE: SORT THE NEW OBJECTS SO THAT THEY EACH OCCUR JUST
	//## AFTER THE MASK OBJECT THEY BELONG TO:
	
	if(insertInPlace)
	{
		for(int s=0; s<numSplitObjs; s++)		// |-- for each new object added:
		for(int m=0; m<numMaskObjs;  m++)		// |
		{
			int currObjIdx = newObjIdx[s][m];				// position of current objects
																							//  (which are in ascending order)
			int maskObjIdx = objMaskList[m];				// position of mask object
			int destObjIdx = maskObjIdx + 1+s;			// we want to move it to after
																							//  the orig mask object (in order)
			
			if(destObjIdx < numObjectsAfterSplit)
			{
				imodMoveObject( imod, currObjIdx, destObjIdx );		// move the object
				newObjIdx[s][m] = destObjIdx;											// update position
				
				for(int i=0; i<numMaskObjs; i++)			// for each mask object:
					if( objMaskList[i] > maskObjIdx )			// if it falls after the masked object
						objMaskList[i]++;											// update obj idx to reflect changes
				for(int j=0; j<numSplitObjs; j++)			// for each (orig) split object:
					if( objList[j] > maskObjIdx )			    // if it falls after the masked object
						objList[j]++;													// update obj idx to reflect changes
			}
		}
	}
	
	//## IF APPLICABLE: DELETE ORIGINAL OBJECTS:
	
	if(deleteOld)
	{
	  sort   ( objList.begin(), objList.end() );		// |-- sort the objects in
		reverse( objList.begin(), objList.end() );		// |   reverse order
		for( int s=0; s>=0; s++ )
			if( s==0 || objList[s]!=objList[s-1] )				// don't delete same idx twice
				imodDeleteObject( imod, objList[s] );
	}
	
	
	//## OPEN/CREATE OUTPUT IMOD FILE (FOR WRITING):
	
	imodBackupFile(argv[argc-1]);
  if (imodOpenFile(argv[argc-1], "wb", imod)) {
    fprintf(stderr, "ERROR: Fatal error opening new model %s\n", argv[argc-1]);
    exit (1);
  }
  imodWriteFile(imod);
	
	
	//## OUTPUT RESULTS SUMMARY:
	
	printf( "\n" );
	printf( "RESULTS:\n" );
	printf( "  tot conts sorted:  %d\n", numContsSorted );
	if(splitPoints)
		printf( "  tot split events:  %d\n", numContsSplit );
	printf( "\n\n"  );
	
  return 0;			// return success so that next operation continues
}



//############################################################
//## FUNCTIONS:


//------------------------------
//-- Adds a series of contours points, from index "startPt" to "endPt" in the
//-- given contour (cont), as a new contour to the specified object (obj).
//-- To do this it actually first duplicates the entire contour then deletes
//-- all points either side of "startPt" and "endPt" so that any point sizes
//-- and other fine grain info is preserved.
//-- 
//-- @ obj         - the object to add the new contour/points to
//-- @ cont        - the contour to duplicate points from
//-- @ startPt     - the index of the first point in (cont) we wish to copy
//-- @ endPt       - the index of the last  point in (cont) we wish to copy
//-- @ cutOpen     - if true, new contour will be marked as open

bool addContPtsToObj( Iobj *obj, Icont *cont, int startPt, int endPt, bool cutOpen )
{
	int numPts = imodContourGetMaxPoint(cont);
	if( endPt   >= numPts)		endPt   = numPts - 1;
	if( startPt <  0     )		startPt = 0;
	if( numPts  == 0     )		return false;
	if( startPt > endPt  )
	{
		fprintf(stderr, "ERROR: addContPtsToObj - bad start and end values\n");
		return false;
	}
	
	Icont *contNew = imodContourDup( cont );		// duplicate input contour
	
	if(cutOpen)
		imodContourSetFlag( contNew, ICONT_OPEN, 1 );
	
	for(int p=numPts-1;  p>endPt; p--)					// delete all points after endPt
		imodPointDelete( contNew, p );
	for(int p=startPt-1; p>=0; p--)							// delete for all points before startPt
		imodPointDelete( contNew, p );
	
	int result = imodObjectAddContour( obj, contNew );		// add new contour to object
	free(contNew);
	
	if(result == -1)
	{
		fprintf(stderr, "ERROR: addContPtsToObj - problem adding new contour\n");
		return false;
	}
	return true;			// success
}

//------------------------------
//-- Duplicates the given contour (cont) and adds it to the given object (obj)
//-- such that all the pixel size info etc is preserved.
//-- 
//-- @ obj     - the object to add the new contour to
//-- @ cont    - the contour we wish to copy/duplicate
/*
bool addContToObj( Iobj *obj, Icont *cont )
{
	int numPts = imodContourGetMaxPoint(cont);
	if( numPts==0 )			// if no points: don't bother
		return false;
	
	Icont *contNew = imodContourDup( cont );
	int result = imodObjectAddContour( obj, contNew );
	free(contNew);
	
	if(result == -1)
	{
		fprintf(stderr, "ERROR: addContToObj - problem adding new contour\n");
		return false;
	}
	
	return true;
}*/


//------------------------------
//-- Duplicates the given contour (cont) and adds it to the given object (obj)
//-- such that all the pixel size info etc is preserved.
//-- 
//-- @ obj     - the object to add the new contour to
//-- @ cont    - the contour we wish to copy/duplicate

bool addContToObj( Iobj *obj, Icont *cont )
{
	int numPts = imodContourGetMaxPoint(cont);
	if( numPts==0 )			// if no points: don't bother
		return false;
	
	Icont *contNew = imodContourDup( cont );
	if(contNew==NULL)
		fprintf(stderr, "ERROR: contour too big to copy\n");
	
	int result = imodObjectAddContour( obj, contNew );
	free(contNew);
	
	if(result == -1)
	{
		fprintf(stderr, "ERROR: addContToObj - problem adding new contour\n");
		return false;
	}
	
	return true;
}


//------------------------------
//-- Inputs a point (pt) and z sorted list of closed contours and object ids (contList)
//-- and returns the ID of the mask object for the first contour which the point
//-- falls inside, on the same z plane. If the point doesn't fall in any contours
//-- inside contList it returns -2 (IS_OUTSIDE).
//-- 
//-- @ pt        - the point to check
//-- @ contList  - a z sorted list of close contours and object ids to test if (pt)
//--               falls inside the area of any of these on the same slice.

int determineWhatObjIdPointIsIn( Ipoint *pt, vector<ContourListZ> &contList )
{
	int z = pt->z;
		
	if( z<0 || z>=contList.size() )
		return OUT_OF_RANGE;						// returns -1
	
	for( int i=0; i<contList[z].cont.size(); i++ )		// for each contour on the same slice:
	{
		if( imodPointInsideCont( contList[z].cont[i].cont, pt  ) )
			return ( contList[z].cont[i].objId );
	}
	return IS_OUTSIDE;								// returns -2
}


//------------------------------
//-- Inputs a conour (cont) and z sorted list of closed contours and object ids 
//-- (contList) and returns the ID of the mask object for the first closed contour  
//-- which contains one of cont's points on the same z plane. If (testFirstPtOnly) 
//-- is on then only the first point in the contour is checked (instead of them all).
//-- If none of the contour points fall in any contours inside contList it
//-- returns -2 (IS_OUTSIDE).
//-- 
//-- @ cont      - the contour we wish to test
//-- @ contList  - a z sorted list of close contours and object ids to test if (cont)
//--               falls inside the area of any of these on the same slice.


int determineWhatObjIdContIsIn( Icont *cont, vector<ContourListZ> &contList,
															  bool testFirstPtOnly )
{
	if(cont==NULL)
		return NO_POINTS;
	
	int z      = imodContourZValue(cont);
	int numPts = imodContourGetMaxPoint(cont);
	if( numPts==0 )
		return NO_POINTS;
	
	if( z<0 || z>=contList.size() )
		return OUT_OF_RANGE;						// returns -1
	
	int ptsToCheck = (testFirstPtOnly) ? 1 : numPts;
	
	for(int p=0; p<ptsToCheck; p++)
	{
		Ipoint *pt = imodContourGetPoint(cont, p);
		for(int i=0; i<contList[z].cont.size(); i++)
		{
			if( imodPointInsideCont( contList[z].cont[i].cont, pt  ) == 1 )
				return ( contList[z].cont[i].objId );
		}
	}
	return IS_OUTSIDE;								// returns -2
}









//############################################################
//## INLINE FUNCTIONS:



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
//-- Adds a new object to the given model (imod), sets that as the new position
//-- and returns a pointer to the the new object. If there's a problem adding
//-- the new model it will print an error message and exit.

inline Iobj* addNewObj( Imod *imod )
{
	int fail = imodNewObject( imod );
	if( fail == 1 )	{
		fprintf(stderr, "ERROR: Failed at adding new object \n" );
		exit (1);
	}
	int objIdx = imodGetMaxObject(imod)-1;
	imodSetIndex(imod, objIdx, 0, 0);
	return ( imodObjectGet(imod) );
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
		else if(ch >= '0' && ch <='9')								// if characer is digit: update tally
		{
			number   *= 10;
			number   += (int)ch - (int)'0';
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



//############################################################

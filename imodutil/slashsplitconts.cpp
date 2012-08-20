/*
 *  slashsplitconts.cpp -- splits contours into a specified number of points.
 *  
 *  Author:     Andrew Noske
 *  Revised by: David Mastronarde (once finished)   email: mast@colorado.edu
 *  
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *  
 * $Id: slashsplitconts.cpp
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
//## MAIN PROGRAM VARIABLES:

static vector<int> objList;						// the list of objects we want to "split"
static vector<int> objNewList;			  // the list of objects which duplicate "objList"

static int pointsPerCont  = 1;			  // the number of points per contour you 
																			//  wish to have in your new contours
static bool overwriteObjs = false;		// if true: will change existing objects
                                      //  instead of adding new ones
static bool duplLastPts  = false;		  // if true: last point will duplicate
                                      //  first point of next contour split
static bool cutContsOpen = false;		  // if true: split contours will all be
                                      //  turned into open contours

//############################################################
//## FUNCTION DECLARATIONS:

bool addContPtsToObj( Iobj *obj, Icont *cont, int startPt, int endPt, bool cutOpen );
bool addContToObj( Iobj *obj, Icont *cont );
int  pruneContPts( Icont *cont, int startPt, int endPt );

inline Iobj* getAndSetObj( Imod *imod, int objIdx );
inline Iobj* addNewObj   ( Imod *imod );

inline int processIntList( char *arg, vector<int> &vect, int pos=0,
													bool allowNeg=true, bool allowRange=false, 
													char sepCh1=',', char sepCh2=';', bool stopOnOtherChars=true );
inline int processPositiveIntList( char *arg, vector<int> &vect, bool acceptRange );


//############################################################



//------------------------------

static void usage(void)
{
  printf("\n");
	printf("Inputs one or more objects then splits all contours in those objects \n");
	printf("so they have no more than a specified number of points.\n");
  printf("\n");
	printf("Usage: slashsplitconts -o <list> [options] <input_model> <output_model>\n");
  printf("Options:\n");
  printf("   -o <lst>  list of objects (eg: '1,4-5') containing contours/points \n");
	printf("             that you wish to split into new objects \n");
	printf("   -p #      the new number of points you want to have per contour \n");
	printf("   -w        overwrite listed objects instead of adding new ones \n");
	printf("   -d        duplicate last point so first and last pts touch \n");
	printf("   -C        mark contours which are split/cut as 'open contours' \n");
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
	
  if (argc < 5)
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
				
				case 'p':			pointsPerCont = atoi(argv[++i]);													break;
				case 'w':			overwriteObjs = true;																			break;
				case 'd':			duplLastPts   = true;																			break;
				case 'C':			cutContsOpen  = true;																			break;
				
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
	
	
	//## PROCESS OBJECT LIST AND CHECK FOR ERRORS:
	
	int numObjs = (int)objList.size();
	
	if( numObjs==0 )							    // if no input objects specified
	{
		fprintf(stderr, "ERROR: You have not entered any objects to split (-o)\n" );
		exit(3);
	}
	for(int i=0; i<numObjs; i++)
	{
		objList[i]--;
		if( objList[i] < 0 || objList[i] >= numObjectsAtStart )
		{
			fprintf(stderr, "ERROR: You have entered bad object numbers.\n" );
			fprintf(stderr, "       All objects should be between 0 and the highest object.\n");
			exit(3);
		}
	}
	
	if( pointsPerCont<=0 )				    // if bad "pointsPerCont" value
	{
		fprintf(stderr, "ERROR: You've entered a bad '-p' value - it would be >=1. \n" );
		exit(3);
	}
	
	//## CREATE A NEW SET OF NEW DUPLICATE OBJECTS TO THE END OF THE IMOD MODEL:
	
	objNewList.resize( objList.size() );			// new objects idx  corresponding to each orig object
	for(int o=0; o<numObjs; o++)			// for each object to split
	{
		Iobj *obj    = getAndSetObj( imod, objList[o] );
		if( obj==NULL )	{
			fprintf(stderr, "ERROR: Object %d does not exist\n", objList[o]);
			continue;
		}
		Iobj *objNew = addNewObj(imod);							// create a new object in the model
		Iobj *objDup = imodObjectDup( obj );		    // duplicate our split object
		imodObjectCopy( objDup, objNew );				    // copy everything to new object
		
		objNewList[o] = imodGetMaxObject(imod)-1;		// update to new object number
		
		if( overwriteObjs )													// if "overwriteObjs":
		{
			objNew = getAndSetObj( imod, objList[o] );	// |-- effectively swaps the
			swap( objList[o], objNewList[o] );					// |   new object into old position
		}
		
		while( imodObjectGetMaxContour(objNew) > 0 )		// remove all contours from objNew
			imodObjectRemoveContour(objNew, 0);
	}
				// NOTE: you can't duplicate an contour within an object into the same object,
	      //       hence we instead must duplicate the whole object, remove points
	      //       and then duplicate from the old to the new.
	
	
	//## PROCESS CONTOURS IN EACH OBJECT AND SPLIT INTO THE APPROPRIATE NUMBER
	//## OF NEW CONTOURS:
	
	int numContsChecked = 0;
	int numContsSplit   = 0;
	int numSplits       = 0;
	
	
	for( int o=0; o<objList.size(); o++ )				// for each object in our object list:
	{
		Iobj *objS         = getAndSetObj( imod, objList[o]    );
		Iobj *objN         = getAndSetObj( imod, objNewList[o] );
		int numContsBefore = imodObjectGetMaxContour(objS);
		
		//## DUPLICATE CONTS WITH THE CORRECT NUMBER OF POINTS TO THE
		//## END OF THE OBJECT:
		
		for(int c=0; c<numContsBefore; c++ )
		{
			Icont *cont = imodObjectGetContour( objS, c );
			int numPts  = imodContourGetMaxPoint(cont);
						
			numContsChecked++;
			
			if(numPts<=pointsPerCont)		// if contour already has few enough points:
			{
				addContToObj(objN,cont);		// |-- duplicate it to the end of the object 
				continue;									  // |   and continue to next contour
			}
			
			int newContsRequired = ceil(float(numPts) / float(pointsPerCont));
			                        // the number of new contours required
			
			for(int i=0; i<newContsRequired; i++)				// for each required contour:
			{
				int startPt = i*pointsPerCont;					  	// |-- determine range of points
				int endPt   = startPt+pointsPerCont-1;      // |
				if(duplLastPts)	endPt++;
				addContPtsToObj(objN, cont, startPt, endPt, cutContsOpen );
				                         // duplicate and cut out all but these points
			}
			
			numContsSplit++;
			numSplits += newContsRequired - 1;
		}
	}
	
	
	//## IF SPECIFIED: DELETE ALL "ORIGINAL" OBJECTS WHICH ARE,
	//## IN THIS CASE, AT THE END OF THE IMOD MODEL:
	
	if( overwriteObjs )													// if "overwriteObjs":
	{
		for(int o=imodGetMaxObject(imod)-1; o>=numObjectsAtStart; o-- )
			imodDeleteObject( imod, o );
	}
	
	
	//## OPEN/CREATE OUTPUT IMOD FILE (FOR WRITING):
	
	imodBackupFile(argv[argc-1]);
  if (imodOpenFile(argv[argc-1], "wb", imod)) {
    fprintf(stderr, "ERROR: Fatal error opening new model %s\n", argv[argc-1]);
    exit (1);
  }
  imodWriteFile(imod);
	
	
	//## OUTPUT RESULTS SUMMARY:
	
	float percentSplit = -1;
	if (numContsChecked>0)
		percentSplit = numContsSplit*100.0f / float(numContsChecked);
	
	printf( "\n" );
	printf( "RESULTS:\n" );
	printf( "  desired pts/cont:      %d           \n", pointsPerCont   );
	printf( "  tot contours checked:  %d           \n", numContsChecked );
	printf( "  tot contours split:    %d  (%0.1f%%)\n", numContsSplit, percentSplit );
	printf( "  tot split events:      %d           \n", numSplits       );
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
//-- Takes a contour (con) and deletes all points except those between
//-- a given "startPt" and "endPt" index and returns the number of points
//-- removed.
//-- 
//-- @ cont        - the contour to remove points from
//-- @ startPt     - the index of the first point in (cont) we wish to remain
//-- @ endPt       - the index of the last  point in (cont) we wish to remain

int pruneContPts( Icont *cont, int startPt, int endPt )
{
	int numPts = imodContourGetMaxPoint(cont);
	if( endPt   >= numPts)		endPt = numPts - 1;
	if( startPt <  0     )		startPt = 0;
	
	for(int p=numPts-1;  p>endPt; p--)
		imodPointDelete( cont, p );
	for(int p=startPt-1; p>=0; p--)
		imodPointDelete( cont, p );
	
	return (numPts - imodContourGetMaxPoint(cont));
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


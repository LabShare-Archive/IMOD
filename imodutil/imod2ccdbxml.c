/*
 *  imod2ccdbxml - convert IMOD model to "CCDB Annotation XML" ("ccdbXML" for short)
 *  
 *  Author:     Andrew Noske - based on specs from Willy Wong
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *  
 *  Specs for ccdbXML: http://DOES_NOT_EXIST_YET.COM  
 *  
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

//############################################################

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "imodel.h"

//## GLOBAL VARIABLES AND CONSTANTS:

static int tabLevel;
static int printAllObjs   = 0;
static int flipY          = 0;
static int printPtsSep    = 0;
static int useObjLabels   = 0;
static int includeEmpty   = 0;

//## FUNCTION DECLARATION:

int imod_to_ccdbxml(Imod *imod, FILE *fout);
static void printObject(Imod *imod, int ob, FILE *fout);
static void getXYZForPoint(Imod *imod, Ipoint pt, float *x, float *y, float *z );
static void printXmlSafeUniqueObjectName(Imod *imod, int ob, FILE *fout );

//############################################################

//------------------------
//-- Usage function - used to write out a small set of instructions in the console
//-- window when the user types "imod2ccdbxml -h" or gets an argument wrong.

static void usage(int error)
{
  printf("\nConverts an imod model to the CCDB Annotation XML File format.\n");
	printf("\   see: .\n");
  printf("Usage: imod2ccdbxml [options] <imod_model_file.mod> <output_file.wml>\n");
	printf("Options:\n");
	printf("       -a    output all objects (by default those switched off are omitted)\n");
	printf("       -f    flips Y values to mach ccdbxml's default coordinate systems\n");
	printf("             (by default header tags are added to represent imod's system).\n");
	printf("       -p    output points using individual <POINT> tags instead of more \n");
	printf("             efficient <LINESTRING_2D> and <LINESTRING_3D> tags\n");
	printf("       -l    use object label values to populate an <ONTO_URI> tag for \n" );
	printf("             each of its contour, these labels can be set via 3dmod: \n" );
	printf("             'Special > Name Wizard > Selection > Generate Labels'\n" );
	printf("       -e    include empty contours \n" );
	exit(error);
}


//------------------------
//-- Main function
//-- Reads command line input, opens new file to write out to, prints any
//-- file errors and then calls "imod_to_ccdbxml" function on the chosen model file.

int main( int argc, char *argv[])
{
  int i;
  FILE *fout;
  Imod *imod;
	
  if (argc < 2) {
    imodVersion(argv[0]);
    imodCopyright();
    usage(0);
  }
	
  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-')
      switch (argv[i][1]){	
			case 'a':
				printAllObjs = 1;
				break;
			
			case 'f':
				flipY = 1;
				break;
			
			case 'p':
				printPtsSep = 1;
				break;
					
			case 'l':
				useObjLabels = 1;
				break;
					
			case 'e':
				includeEmpty = 1;
				break;
					
      default:
        usage(-1);
        break;

      }
    else
      break;
  }

  if (argc - i != 2)
    usage(-1);

  fout = fopen( argv[i + 1] , "w");
  if (fout == NULL){
    printf("Couldn't open output file %s.\n", argv[i + 1]);
    exit(10);
  }
	
  imod = imodRead(argv[i]);
  if (!imod){
    fprintf(stderr, "%s: Error reading imod model %s\n", argv[0], 
            argv[i]);
    exit(3);
  }
     
  imod_to_ccdbxml(imod, fout);

  fclose(fout);
  exit(0);
}

//------------------------
//-- Takes an imod model and outputs the contour in "CCDB Annotation XML" format by
//-- calling "printObject" on each object.
//-- For more information about CCDB Annotation XML format see:
//-- > http://DOES_NOT_EXIST_YET.COM                               (official specs)   OR
//-- > https://confluence.crbs.ucsd.edu/display/SLASH/CCDB+Annotation+XML (author's notes)

int imod_to_ccdbxml(Imod *imod, FILE *fout)
{
	int ob;
  fprintf(fout,"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
  fprintf(fout,"<file type=\"CCDBAnnotationSchema\" file_version=\"1.0\">\n");
															// change this line to mach current version!
	fprintf(fout,"<ANNOTATION>\n");
	fprintf(fout,"\n");
	fprintf(fout,"  <RESOURCES dataset_id=\"unknown\" filepath=\"unknown\">\n");
	if(!flipY)
	{
		fprintf(fout,"  <COORDINATE_ORIGIN x=\"left\" y=\"bottom\" z=\"bottom\"/>\n");
		fprintf(fout,"  <COORDINATE_DIRECTION x=\"right\" y=\"up\" z=\"up\"/>\n");
	}
	fprintf(fout,"\n");
  {
    for(ob = 0; ob < imod->objsize; ob++)
      printObject(imod, ob, fout);
  }
  fprintf(fout,"\n");
	fprintf(fout,"</ANNOTATION>\n");
	fprintf(fout,"</file>\n");
  return 0;
}


//------------------------
//-- Prints out to file the ccdbXML text for that object by
//-- printing each set of contour points.

static void printObject(Imod *imod, int ob, FILE *fout)
{
  int c, p;
  Iobj *obj = &imod->obj[ob];
	Icont *cont;
	int lastuse = -1;
	int objScattered = iobjScat(obj->flags) ? 1 : 0;
	
	//## IF OBJECT TURNE OFF, EXIT EARLY
	
	if ( !printAllObjs && iobjOff(obj->flags) )		// if object off, and "-a" not entered:
    return;																				// exit early
	
	
  //## PRINT OBJECT'S DATA:
	
	fprintf(fout,"\n");
	fprintf(fout,"\n");
	fprintf(fout,"<!-- #DATA FOR OBJECT ");							// print comment
	printXmlSafeUniqueObjectName( imod, ob, fout );
	fprintf(fout,"-->\n");
	fprintf(fout,"\n");
	
	fprintf(fout,"<GEOMETRY user_name=\"guest\""
							" modified_time=\"1307058082\""
							" program=\"IMOD\">\n");
	
	int isObjClosed = (imodObjectGetValue(obj, IobjFlagClosed) == 1) ? 1 : 0;
	
	//## FOR EACH NON-EMPTY CONTOUR: PRINT OUT AS "POLYGON":
	
	for(c = 0; c < obj->contsize; c++)
	{
    cont = &(obj->cont[c]);
		
    if (!cont || (!includeEmpty && cont->psize == 0 ) )		// if contour is empty:
      continue;																							// skip this contour
		
		//## FOR EACH POINT: PRINT OUT POINT COORDINATES:
		
		fprintf(fout,"  <POLYGON>\n");
		
		
		float x,y,z;
		
		if(printPtsSep)
		{
			for (p = 0; p < cont->psize; p++)
			{
				//float x = cont->pts[p].x;
				//float y = cont->pts[p].y;
				//float z = cont->pts[p].z;
				//if( flipY )
				//	y = (float)imod->ymax - y;
				getXYZForPoint( imod, cont->pts[p], &x, &y, &z );
				
				fprintf(fout,"    <POINT>%g,%g,%g</POINT>\n", x, y, z );
			}
		}
		else
		{
			int isContClosed = 0;
			if ( (imodObjectGetValue(obj, IobjFlagClosed) == 1)
				&& (imodContourGetFlag( cont, ICONT_OPEN )  == 0) )
				isContClosed = 1;
			
			/*bool zChanges = false;
			for (p = 0; p < cont->psize-1; p++)
			{
				if ( cont->pts[p].z < cont->pts[p+1].z - 0.001
					|| cont->pts[p].z > cont->pts[p+1].z + 0.001 )
				{
					zChanges = false;
					break;
				}
			}*/
			
			if(isContClosed)
			{
				if( cont->psize > 0)
					fprintf(fout,"    <Z_VALUE>%g</Z_VALUE>\n", cont->pts[0].z );
				fprintf(fout,"    <LINESTRING_2D>" );
				for (p = 0; p < cont->psize-1; p++)
				{
					if(p>0)
						fprintf(fout,",");
					getXYZForPoint( imod, cont->pts[p], &x, &y, &z );
					fprintf(fout,"%g %g", x, y );
				}
				fprintf(fout,"</LINESTRING_2D>\n" );
			}
			else
			{
				fprintf(fout,"    <LINESTRING_3D>" );
				for (p = 0; p < cont->psize-1; p++)
				{
					if(p>0)
						fprintf(fout,",");
					getXYZForPoint( imod, cont->pts[p], &x, &y, &z );
					fprintf(fout,"%g %g %g", x, y, z );
				}
				fprintf(fout,"</LINESTRING_3D>\n" );
			}				
		}
		
		fprintf(fout,"  </POLYGON>\n", ob);
  }
	
	
  fprintf(fout,"</GEOMETRY>\n", ob);
	fprintf(fout,"\n");
  return;
}

//------------------------
//-- Returns the values of x, y and z for the given point.
//-- Note that the y value will be flipped if "flipY" is true.

static void getXYZForPoint(Imod *imod, Ipoint pt, float *x, float *y, float *z )
{
	*x = pt.x;
	*y = pt.y;
	*z = pt.z;
	if( flipY )
		*y = (float)imod->ymax - pt.y;
}


//------------------------
//-- Outputs to file [fout] a unique name for the given object [ob] in the form:
//--   "obj1_Object_Name"
//-- Note that the object number is included (to ensure uniquness) and the user
//-- assigned object name is also printed, but using an underscore (_) to replace
//-- any whitespaces, brackets and several other characters which we don't
//-- want allow in the name.

static void printXmlSafeUniqueObjectName(Imod *imod, int ob, FILE *fout )
{
	int i;
	Iobj *obj = &imod->obj[ob];
	
	fprintf( fout,"obj%d_", (ob+1) );		// write out object number as "obj1_"
	
	for(i = 0; i < IOBJ_STRSIZE; i++)		// write out object's name as "Object_Name"
	{
		char c = obj->name[i];							// get character
		if( c == 0x00 )
			break;
		else if( c==' ' || c=='\t' || c=='\n' )					// whitespace chars
			fprintf( fout,"_" );
		else if( c=='<' )						// open brackets
			fprintf( fout,"]" );
		else if( c=='>' )						// close brackets
			fprintf( fout,">" );
		else if( c=='\'' || c=='"' )																// quote symbols
			fprintf( fout,"`" );
		else if( c=='#' || c=='.' || c==',' || c=='\\' || c==':' )	// other
			fprintf( fout,"_" );
		else
		  fprintf( fout,"%c", obj->name[i] );
	}
}



/*
$Log: imod2ccdbxml.c,v $
Revision 1.0  2011/06/03 21:37:14  anoske
Modified file from "imod2vrml.c" to output ccdbXML format
*/

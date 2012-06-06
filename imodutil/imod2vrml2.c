/*
 *  imod2vrml2 - convert IMOD model to VRML version 2
 *  
 *  Author:     Andrew Noske - modified from James Kremer's imod2vrml
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *  
 *  Info on VRML2: http://www.andrewnoske.com/wiki/index.php?title=VRML
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

static int  numObjs     = 0;		// tallies the number of objects turned on
static int  numVertices = 0;		// tallies the number of vertex points added
static int  numFaces    = 0;		// tallies the number of faces added
static int  numSpheres  = 0;		// tallies the number of spheres printed

static int lowRes         = 0;		// if we want to use low res version
static int printAllObjs   = 0;		// print all objects (not just those on)
static int rotateModel    = 0;		// "rotate" (flip Y and Z axis)
static int dontNameGeoms  = 0;		// don't assign names to child geometries
static int printNormals   = 0;		// print normals
static int showContours   = 0;		// show all contour lines

//## FUNCTION DECLARATION:

void imod_to_vrml2(Imod *imod, FILE *fout);

static void printObject(Imod *imod, int ob, FILE *fout);
static void printMaterial(Imod *imod, int ob, int *lastuse, int usefill, FILE *fout);
static void printVrmlSafeUniqueObjectName(Imod *imod, int ob, FILE *fout );
static void printScatContours(Imod *imod, int ob, FILE *fout);
static void printMesh(Imod *imod, int ob, FILE *fout);
static void printMeshPoints(Imesh *mesh, float zscale, FILE *fout);

//############################################################

//------------------------
//-- Usage function - used to write out a small set of instructions in the console
//-- window when the user types "imod2vrml2 -h" or gets an argument wrong.

static void usage(int error)
{
  printf("\nConverts an imod model to the VRML2.0 (Virtual Reality Modeling "
         "Language) file format.\n");
  printf("Usage: imod2vrml2 [options] <imod_model_file.mod> <output_file.wml>\n");
	printf("Options:\n");
  printf("       -l    output low-resolution meshes (if any exist).\n");
	printf("       -a    output all objects (by default those switched off are omitted)\n");
	printf("       -r    groups all objects together in an 'imod_model' object and\n");
	printf("             rotates this such that the Z axis become up in Y\n");
	printf("       -g    don't assign names to child geometries (by default they get \n");
	printf("             DEF names in the form 'obj1_mesh' or 'cont2_pt1').\n");
	printf("       -n    output normals (off by default as most 3d programs which \n");
	printf("             import VRML2 can generate their own normals if missing).\n");
	printf("       -c    show all contour lines.\n");
	
  exit(error);
}


//------------------------
//-- Main function
//-- Reads command line input, opens new file to write out to, prints any
//-- file errors and then calls "imod_to_vrml2" function on the chosen model file.

int main( int argc, char *argv[])
{
  int i;
  FILE *fout;
  Imod *imod;
  char *progname = imodProgName(argv[0]);

  if (argc < 2) {
    imodVersion(progname);
    imodCopyright();
    usage(0);
  }

  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-')
      switch (argv[i][1]){
      case 'l':
        lowRes = 1;
        break;
					
			case 'a':
				printAllObjs = 1;
				break;
			
			case 'r':
				rotateModel = 1;
				break;
			
			case 'g':
				dontNameGeoms = 1;
				break;
				
			case 'n':
				printNormals = 1;
				break;
					
			case 'c':
				showContours = 1;
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
	
  imod = imodRead(argv[i]);
  if (!imod){
    fprintf(stderr, "%s: Error reading imod model %s\n", argv[0], 
            argv[i]);
    exit(3);
  }
	
	//## OPEN/CREATE .OBJ FILE FOR WRITING:
	
	fout = fopen( argv[i + 1] , "w");
  if (fout == NULL){
    printf("Couldn't open output file %s.\n", argv[i + 1]);
    exit(10);
  }
	
  imod_to_vrml2(imod, fout);

  fclose(fout);
	
	fprintf(stdout, "Finished writing '%s' \n", argv[i] );
	fprintf(stdout, "  # objects on: %d\n", numObjs     );	
	fprintf(stdout, "  # spheres:    %d\n", numSpheres  );
	fprintf(stdout, "  # vertices:   %d\n", numVertices );
	fprintf(stdout, "  # faces:      %d\n", numFaces    );
  exit(0);
}

//------------------------
//-- Takes an imod model and outputs the meshes into a VRML2 file by printing
//-- then calling "printObject" on each object.
//-- For more information about VRML2 format see:
//-- > http://graphcomp.com/info/specs/sgi/vrml/spec/        (official specs)   OR
//-- > http://www.andrewnoske.com/wiki/index.php?title=VRML  (author's notes)

void imod_to_vrml2(Imod *imod, FILE *fout)
{
	int ob;
  fprintf(fout,"#VRML V2.0 utf8\n");
  fprintf(fout,"#Generated by IMOD\n\n");
	fprintf(fout,"DEF imod_model Transform {\n");
	if(rotateModel)
	  fprintf(fout,"  rotation -1 0 0 1.570796   # rotate by -90 degrees in Z \n");
	fprintf(fout,"  children [\n");
  {
    for(ob = 0; ob < imod->objsize; ob++)
      printObject(imod, ob, fout);
  }
  fprintf(fout,"\n");
	fprintf(fout,"  ]\n");
	fprintf(fout,"}\n");
}

//------------------------
//-- Outputs to file [fout] a unique name for the given object [ob] in the form:
//--   "obj1_Object_Name"
//-- Note that the object number is included (to ensure uniquness) and the user
//-- assigned object name is also printed, but using an underscore (_) to replace
//-- any whitespaces, brackets and several other characters which VRML2.0 format
//-- does not allow in a single name.
//-- These dangerous characters include:    \t \n \\ # c . : () [] {} ' " + & ; |

static void printVrmlSafeUniqueObjectName(Imod *imod, int ob, FILE *fout )
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
		else if( c=='(' || c=='[' || c=='{' )												// open brackets
			fprintf( fout,"<" );
		else if( c==')' || c==']' || c=='}' )												// close brackets
			fprintf( fout,">" );
		else if( c=='\'' || c=='"' )																// quote symbols
			fprintf( fout,"`" );
		else if( c=='#' || c=='.' || c==',' || c=='\\' || c==':' )	// other
			fprintf( fout,"_" );
		else if( c=='+' || c=='&' || c==';' || c=='|' )			// symbols cinema 4D rejects
			fprintf( fout,"_" );
		else
		  fprintf( fout,"%c", obj->name[i] );
	}
}

//------------------------
//-- Prints out to file the VRML text for that object by first printing
//-- the material by calling "printMaterial" function, then 
//-- calling a seperate function to "printScatContours" or "printMesh"
//-- depending on the way this appeared in code.

static void printObject(Imod *imod, int ob, FILE *fout)
{
  int co;
  Iobj *obj = &imod->obj[ob];
  int lastuse = -1;
	int objScattered = iobjScat(obj->flags) ? 1 : 0;
  int hasSpheres   = (iobjScat(obj->flags) || obj->pdrawsize) ? 1 : 0;
	if ( !printAllObjs && iobjOff(obj->flags) )		// if object off, and "-a" not entered:
    return;																				// exit early
	
	// See if object has spheres to draw
  for (co = 0; co < obj->contsize && !hasSpheres; co++)
    if (obj->cont[co].sizes)
      hasSpheres = 1;
	
	
	//## PRINT OBJECT'S MATERIAL IN AN EMPTY SHAPE:
	
	printMaterial(imod, ob, &lastuse, obj->flags & IMOD_OBJFLAG_FCOLOR, fout);
	
	
  //## PRINT OBJECT'S DATA:
	
	fprintf(fout,"\n");
	fprintf(fout,"#DATA FOR OBJECT %d:\n", ob+1);
  fprintf(fout,"DEF ");
	printVrmlSafeUniqueObjectName( imod, ob, fout );
	fprintf(fout," Transform {\n");
	fprintf(fout,"  children [\n");
	
	if( !objScattered )
	{
    if (iobjMesh(obj->flags))
      printMesh(imod, ob, fout);
	}
  if (hasSpheres) {
    printScatContours(imod, ob, fout);
  }
	
  fprintf(fout,"  ]\n", ob);
	fprintf(fout,"}\n", ob);
	
  numObjs++;
}


//------------------------
//-- Prints out the VRML2 code representing the material properties of
//-- the specified object ("ob"), matching values as closely as possible.
//-- Things like color and transparency are maintained, but other
//-- values like specular color are not quite perfectly translated.

static void printMaterial(Imod *imod, int ob, int *lastuse, int usefill, FILE *fout)
{
	Iobj *obj = &imod->obj[ob];
  float ambient   = ((int)obj->ambient)   / 255.;
	float diffuse   = ((int)obj->diffuse)   / 255.;
  float specular  = ((int)obj->specular)  / 255.;
  float shininess = ((int)obj->shininess) / 255.;
  float r = obj->red;
  float g = obj->green;
  float b = obj->blue;
  if (*lastuse == usefill)
    return;
  
  if (usefill) {
    r = (float)obj->fillred   / 255.;
    g = (float)obj->fillgreen / 255.;
    b = (float)obj->fillblue  / 255.;
  }
  *lastuse = usefill;
	
  if (shininess < 0.01)
    shininess= 0.01;
	
  //## PRINT EMPTY SHAPE WITH "appearance DEF" TAG SO OTHER SHAPES
	//## CAN REFERENCE THIS TAG:
	
	fprintf(fout,"\n");
	fprintf(fout,"#MATERIAL FOR OBJECT %d:\n", (ob+1) );
  fprintf(fout,"Shape {\n");
  fprintf(fout,"  appearance DEF MAT_");
	printVrmlSafeUniqueObjectName(imod, ob, fout );
	fprintf(fout," Appearance {\n");
	fprintf(fout,"    material Material {\n");
	fprintf(fout,"      ambientIntensity %g \n", ambient );		// "ambientColor" in VRML1
  fprintf(fout,"      diffuseColor %g %g %g\n",  r*diffuse, g*diffuse, b*diffuse);
  fprintf(fout,"      specularColor %g %g %g\n", r*specular, g*specular, b*specular);
  fprintf(fout,"      emissiveColor 0 0 0\n");
  fprintf(fout,"      shininess %g\n", shininess);
  fprintf(fout,"      transparency %g\n", obj->trans/100.0f);
  fprintf(fout,"    }\n");
	fprintf(fout,"  }\n");
	fprintf(fout,"}\n");
}


//------------------------
//-- Prints a scattered point object (in VRML2 format) by printing a sphere for 
//-- every point in the object which has a radius.

static void printScatContours(Imod *imod, int ob, FILE *fout)
{
	Iobj *obj = &imod->obj[ob];
  Icont *cont;
  int hasSpheres = (iobjScat(obj->flags) || obj->pdrawsize) ? 1 : 0;
  int c, p;
  float zscale = imod->zscale;
  float radius;
  
	fprintf(fout,"    DEF obj%d_SPHERES Group {    # GROUP OF POINTS:\n", ob+1);
	fprintf(fout,"      children [\n\n");	
	
  //## FOR EACH CONTOUR: PRINT EVERY POINT WHICH HAS A SPHERE SIZE:
	
	for(c = 0; c < obj->contsize; c++){
    cont = &(obj->cont[c]);
		
    if (!cont || !cont->psize								// if contour is empty
				|| (!hasSpheres && !cont->sizes))		// or has no sphere sizes:
      continue;																// skip this contour
		
		for (p = 0; p < cont->psize; p++)
		{
      radius = imodPointGetSize(obj, cont, p);
      if (radius > 0.)		// if point radius is non zero: output new sphere
      {
				fprintf(fout,"        DEF obj%d_cont%d_pt%d Transform {\n", ob+1, c+1, p+1 );
				fprintf(fout,"          translation %g %g %g \n",
											      cont->pts[p].x, cont->pts[p].y, (cont->pts[p].z * zscale) );
				if(rotateModel)
					fprintf(fout,"          rotation 1 0 0 1.570796 \n");		// rotate 90 degs
			  fprintf(fout,"          children [ Shape {\n");
				fprintf(fout,"              appearance USE MAT_");
				printVrmlSafeUniqueObjectName(imod, ob, fout );
				fprintf(fout,"\n              ");
				if( dontNameGeoms )
					fprintf(fout,"geometry Sphere { radius %f } } ]\n", radius);
				else
					fprintf(fout,"geometry DEF cont%d_pt%d Sphere { radius %f } } ]\n", 
																									ob+1, c+1, p+1, radius);
				fprintf(fout,"        }\n", radius);
				numSpheres++;
			}
    }
  }
	fprintf(fout,"      ]\n");	
	fprintf(fout,"    }\n");	
}


//------------------------
//-- Prints out the mesh generated over the given object ("ob") in
//-- VRML2 format which involves first outputting the list of all points
//-- using the "printAllPoints" function, then connecting points into triangles.

static void printMesh(Imod *imod, int ob, FILE *fout)
{
  Iobj *obj = &imod->obj[ob];
	int objFill = iobjFill(obj->flags);		// object has been set to show faces
	int objLine = iobjLine(obj->flags);		// object has been set to show lines
  Imesh *mesh;
  int m, c, p, i, j;
  float zscale = imod->zscale;
  int *ilist;
  int lsize;
  int index, ind, poly;
  Ipoint norm;
  int lastuse = -1;
  int resol;
  int listInc, vertBase, normAdd;
	int objClosed, contClosed;
	
	
  imodMeshNearestRes(obj->mesh, obj->meshsize, lowRes, &resol);
  
	
	
	//## FOR EACH MESH (TYPICALLY JUST ONE) PRINT MESH OR POINTS: 
	
  for(m = 0; m < obj->meshsize; m++)
	{
    if (imeshResol(obj->mesh[m].flag) != resol)
      continue;
		
    poly = 0;
    mesh = &obj->mesh[m];
    ilist = (int *)malloc(sizeof(int) * mesh->lsize);
    if (!ilist) {
      printf("Error allocating memory\n");
      exit(1);
    }
		
		
		//## IF OBJECT'S MESH ARE DISPLAYED: GENERATE "IndexFaceSet"
		
		if( objFill )
		{
			//## PRINT START OF NEW SHAPE WITH "IndexFaceSet" GEOMETRY:
			
			fprintf(fout,"    Shape {  #MESH \n");							// start of new Shape
			fprintf(fout,"      appearance USE MAT_" );
			printVrmlSafeUniqueObjectName(imod, ob, fout );
			fprintf(fout,"\n      ");
			if( dontNameGeoms )
				fprintf(fout,"geometry IndexedFaceSet {\n");				// start of new IndexFaceSet
			else
				fprintf(fout,"geometry DEF obj%d_mesh%d IndexedFaceSet {\n", ob+1, 1 );
			fprintf(fout,"        ccw FALSE \n");
			fprintf(fout,"        solid FALSE \n");
			fprintf(fout,"        creaseAngle 1.56207 \n");
			
			//## OUTPUT POINTS:
			printMeshPoints( mesh, zscale, fout );
			
			//## OUTPUT NORMALS (IF SPECIFIED):
			if (printNormals)
			{
				fprintf(fout,"        normal Normal {\n");
				fprintf(fout,"          vector [   # list of all normals for each point\n");
				for(i = 1; i < mesh->vsize; i+=2){
					norm = mesh->vert[i];
					imodPointNormalize(&norm);
					fprintf(fout,"            %.3f %.3f %.3f,\n", norm.x, norm.y, norm.z );
				}
				fprintf(fout,"          ]\n");
				fprintf(fout,"        }\n");
			}
			
			//## OUTPUT ALL MESHES TOGETHER:
			
			fprintf(fout,"        coordIndex [   # connect triangles\n");
			for(i = 0; i < mesh->lsize; i++)
			{
				if (mesh->list[i] == IMOD_MESH_END)
					break;
				if ( !imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, &normAdd) )
					continue;
				
				lsize = 0;		// copy the indices to the new index array numbered from 0:
				i++;
				while (mesh->list[i] != IMOD_MESH_ENDPOLY)
				{
					ilist[lsize++] = mesh->list[i+vertBase]/2;			i += listInc;
					ilist[lsize++] = mesh->list[i+vertBase]/2;			i += listInc;
					ilist[lsize++] = mesh->list[i+vertBase]/2;			i += listInc;
				}
				
				if (!lsize)
					continue;
				
				//## OUTPUT FACESET:   (if showing surface, put out face set)
				
				for(index = 0; index < lsize / 3; index++)
				{
					ind = 3 * index;
					fprintf(fout,"          %d,%d,%d,-1,\n", 
									ilist[ind+2], ilist[ind+1], ilist[ind]);
					numFaces++;
				}
			}
			fprintf(fout,"        ]\n");
			
			//## PRINT END OF SHAPE:
			
			fprintf(fout,"      }\n");		// end of IndexFaceSet
			fprintf(fout,"    }\n");			// end of Shape
		}
		
		
		//## IF OBJECT RENDERED AS POINTS: GENERATE "PointSet"
		
		if( !objFill )
		{
			//## PRINT START OF NEW SHAPE WITH "IndexFaceSet" GEOMETRY:
			
			fprintf(fout,"    Shape {\n");							// start of new Shape
			fprintf(fout,"      appearance USE MAT_" );
			printVrmlSafeUniqueObjectName(imod, ob, fout );
			fprintf(fout,"\n      ");
			if( dontNameGeoms )
				fprintf(fout,"geometry PointSet {\n");				// start of new PointSet
			else
				fprintf(fout,"geometry DEF obj%d_all_points PointSet {\n", ob+1, 1 );
			
			//## OUTPUT POINTS:
			printMeshPoints( mesh, zscale, fout );
			
			fprintf(fout,"      }\n");		// end of PointSet
			fprintf(fout,"    }\n");			// end of Shape
			
    }
		
		
		//## FREE ANY MEMORY USED:
    if (ilist) 
      free(ilist);
  }
	
	
	//## IF OBJECT'S CONTOURS ARE DISPLAYED: GENERATE "IndexLineSet"
	
	if (showContours)
	{
		objClosed = (imodObjectGetValue(obj,IobjFlagClosed)==1) ? 1 : 0;
		for(c = 0; c < obj->contsize; c++)
		{
			Icont *cont = &obj->cont[c];
			if(cont->psize == 0)
				continue;
			
			//## PRINT START OF NEW SHAPE WITH "IndexFaceSet" GEOMETRY:
			
			fprintf(fout,"    Shape {  #CONTOUR \n");							// start of new Shape
			fprintf(fout,"      appearance USE MAT_" );
			printVrmlSafeUniqueObjectName(imod, ob, fout );
			fprintf(fout,"\n      ");
			if( dontNameGeoms )
				fprintf(fout,"geometry IndexedLineSet {\n");				// start of new IndexFaceSet
			else
				fprintf(fout,"geometry DEF obj%d_cont%d IndexedLineSet {\n", ob+1, c+1 );
			
			//## PRINT POINTS:
			
			contClosed = (objClosed || imodContourGetFlag(cont,ICONT_OPEN) != 0) ?1:0;
			
			fprintf(fout,"        coord Coordinate {\n");
			fprintf(fout,"          point [   # contour points\n");
			for(p = 0; p < cont->psize; p++)
			{
				fprintf(fout,"            %.5g %.5g %.5g,\n",
								cont->pts[p].x, cont->pts[p].y, (cont->pts[p].z * zscale) );
				
				numVertices++;
			}
			fprintf(fout,"          ]\n");
			fprintf(fout,"        }\n");
			fprintf(fout,"        coordIndex [     # connect points\n");
			fprintf(fout,"          \n");
			for(p = 0; p < cont->psize; p++)
				fprintf(fout,"%d,", p);
			if(contClosed)
				fprintf(fout,"0");
			fprintf(fout,"        ]\n");
			fprintf(fout,"      }\n");		// end of IndexFaceSet
			fprintf(fout,"    }\n");			// end of Shape
		}
		
	}
	
	
}

//------------------------
//-- Prints a list of all points in the specified mesh in VRML2 format
//-- as an point array in the form:
//-- "coord Coordinate { point [ 0 0 0, 0 0 5, ... ] )".

static void printMeshPoints(Imesh *mesh, float zscale, FILE *fout)
{
	int i;
	fprintf(fout,"        coord Coordinate {\n");
	fprintf(fout,"          point [   # list of all points in mesh\n");
	for(i = 0; i < mesh->vsize; i+=2)
	{
		fprintf(fout,"            %.5g %.5g %.5g,\n",
						mesh->vert[i].x, mesh->vert[i].y, (mesh->vert[i].z * zscale) );
		
		numVertices++;
	}
	fprintf(fout,"          ]\n");
	fprintf(fout,"        }\n");
}


/*
$Log: imod2vrml2.c,v $
Revision 1.0  2011/05/29 21:37:14  noske
Modified file from "imod2vrml.c" to output VRML2.0 format
*/

/*
 *  imod2obj.cpp - convert IMOD model to (wavefront) OBJ file
 *  
 *  Author:     Andrew Noske
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Info on OBJ: http://www.andrewnoske.com/wiki/index.php?title=OBJ_file_format
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id: imod2obj.cpp
 *  Log at end
 */

//############################################################

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "imodel.h"

//## GLOBAL VARIABLES:

static int  numObjs     = 0;		// tallies the number of objects turned on
static int  numVertices = 0;		// tallies the number of vertex points added
static int  numFaces    = 0;		// tallies the number of faces added
static int  numNormals  = 0;		// tallies the number of normals added
static int  numSpheres  = 0;		// tallies the number of spheres printed

static int lowRes         = 0;		// if we want to use low res version
static int printAllObjs   = 0;		// print all objects (not just those on)
static int rotateModel    = 0;		// "rotate" (flip Y and Z axis)
static int printMatFile   = 0;		// generate a .mtl file
static int printNormals   = 0;		// print normals
static int flipNormals    = 0;    // flip normals
static int sphereSegments = 8;		// number of segments per sphere
static int useIcosahedrons= 0;		// draw isohedrons instead of "standard" sphere meshes
static int onlyScatSpheres= 0;	  // only print spheres for scattered objects

// TODO: Add labels...


//## FUNCTION DECLARATION:

void imod_to_obj(Imod *imod, FILE *fout, char *outFileName);
static void printObject(Imod *imod, int ob, FILE *fout);
static void printObjSafeUniqueObjectName(Imod *imod, int ob, FILE *fout );
static void printMesh(Imod *imod, int ob, FILE *fout);
static void printScatContours(Imod *imod, int ob, FILE *fout);
static void printSphere(Ipoint pt, float radius, int segments, FILE *fout);
static void printSphereAsIcosahedron(Ipoint pt, float radius, FILE *fout);

void imod_to_mtl(Imod *imod, FILE *fout);
static void printMaterial(Imod *imod, int ob, FILE *fout);

//############################################################

//------------------------
//-- Usage function - used to write out a small set of instructions in the console
//-- window when the user types "imod2obj -h" or gets an argument wrong.

static void usage(int error)
{
  printf("\nConverts an imod model to OBJ (wavefront object) file format and\n");
	printf("   (if specified) a matching MTL (material template library) file.\n\n");
  printf("Usage: imod2obj [options] <input.mod> <output.obj> [output.mtl]\n");
	printf("Options:\n");
  printf("       -l     output low-resolution meshes (if any exist).\n");
	printf("       -a     output all objects (by default any switched off are omitted)\n");
	printf("       -r     rotates the model by 90 pitch by effectively flipping the \n");
	printf("              Y and Z axis - a better orientiation making movies\n");
	printf("       -n     output normals (off by default as most 3d programs which \n");
	printf("              import OBJ can generate their own normals if missing).\n");
	printf("       -f     flips all normals.\n");
	printf("       -o     only print spheres on scattered objects.\n");
	printf("       -s #   number of segments to render per sphere (default is 8) \n");
	printf("       -i     print spheres as icosahedrons (20 triangles/sphere) instead \n");
	
  exit(error);
}


//------------------------
//-- Main function
//-- Reads command line input, opens new file to write out to, prints any
//-- file errors and then calls "imod_to_obj" function on the chosen model file.
//-- If a third filename argument is specified it will call "imod_to_mtl" too.

int main( int argc, char *argv[])
{
  int i;
  FILE *fout, *fmout;
  Imod *imod;
  char *progname = imodProgName(argv[0]);
	char *matFileName;
	
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
			
			case 'i':
				useIcosahedrons = 1;
				break;
				
			case 's':
				sphereSegments = atof(argv[++i]);
				break;
				
			case 'n':
				printNormals = 1;
				break;
				
			case 'f':
				flipNormals = 1;
				break;
				
			case 'o':
				onlyScatSpheres = 1;
				break;
				
      default:
        usage(-1);
        break;

      }
    else
      break;
  }

  if (argc - i != 2 && argc - i != 3)
    usage(-1);
	
	//## OPEN IMOD FILE FOR READING:
	imod = imodRead(argv[i]);
  if (!imod){
    fprintf(stderr, "%s: Error reading imod model %s\n", argv[0], argv[i]);
    exit(3);
  }
	
	//## OPEN/CREATE .OBJ FILE FOR WRITING:
	
  fout = fopen( argv[i+1] , "w");
  if (fout == NULL){
    printf("Couldn't open output file %s.\n", argv[i+1]);
    exit(10);
  }
	
	if(argc - i == 3)
	{
		printMatFile = 1;
		matFileName = argv[i+2];
	}
	
	
  imod_to_obj(imod, fout, matFileName);
	
  fclose(fout);
	
	
	//## OPEN/CREATE .MTL FILE FOR WRITING (IF APPLICABLE):
	
	if(printMatFile)
	{
		fmout = fopen( matFileName , "w");
		if (fmout == NULL){
			printf("Couldn't open material output file %s.\n", matFileName);
			exit(10);
		}
		imod_to_mtl(imod, fmout);
	}
	
	fprintf(stdout, "Finished writing '%s' \n", argv[i] );
	fprintf(stdout, "  # objects on: %d\n", numObjs     );
	fprintf(stdout, "  # spheres:    %d\n", numSpheres  );
	fprintf(stdout, "  # vertices:   %d\n", numVertices );
	fprintf(stdout, "  # faces:      %d\n", numFaces    );
	fprintf(stdout, "  mtl file generated: %s\n", (printMatFile) ? "yes" : "no" );
	
  exit(0);
}

//------------------------
//-- Takes an imod model and outputs the meshes into a OBJ file by printing
//-- then calling "printObject" on each object.
//-- For more information about OBJ format see:
//-- > http://www.fileformat.info/format/wavefrontobj/egff.htm   (official specs)   OR
//-- > http://www.andrewnoske.com/wiki/index.php?title=OBJ_file_format  (author's notes)

void imod_to_obj(Imod *imod, FILE *fout, char *matFileName)
{
	int ob;
  fprintf(fout,"# WaveFront *.obj file (generated from an IMOD model by imod2obj)\n");
	
	if(printMatFile)
	{
		fprintf(fout,"# Material values are stored in this .mtl file:\n");
		fprintf(fout,"mtllib %s\n", matFileName);
	}
	
  fprintf(fout,"\n\n");
	for(ob = 0; ob < imod->objsize; ob++)
	{
    printObject(imod, ob, fout);
		fprintf(fout,"\n\n");
  }
	fprintf(fout,"\n\n");
	fprintf(fout,"# For more info on OBJ file format see:\n");
	fprintf(fout,"#  http://www.andrewnoske.com/wiki/index.php?title=OBJ_file_format\n");
}

//------------------------
//-- Outputs to file [fout] a unique name for the given object [ob] in the form:
//--   "obj1_Object_Name"
//-- Note that the object number is included (to ensure uniquness) and the user
//-- assigned object name is also printed, but using an underscore (_) to replace
//-- any whitespaces, brackets and several other characters which OBJ format
//-- does not allow in a single name.
//-- These dangerous characters include:    \t \n \\ # c . : () [] {} ' " + & ; |

static void printObjSafeUniqueObjectName(Imod *imod, int ob, FILE *fout )
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
		  fprintf( fout,"%c", c );
	}
}

//------------------------
//-- Prints out to file the OBJ text for that object by first printing
//-- the material by calling "printMaterial" function, then 
//-- calling a seperate function to "printScatContours" or "printMesh"
//-- depending on the way this appeared in code.

static void printObject(Imod *imod, int ob, FILE *fout)
{
  int co;
  Iobj *obj = &imod->obj[ob];
	int objScattered = iobjScat(obj->flags) ? 1 : 0;
  int hasSpheres   = (iobjScat(obj->flags) || obj->pdrawsize) ? 1 : 0;
	if ( !printAllObjs && iobjOff(obj->flags) )		// if object off, and "-a" not entered:
    return;																				// exit early
	
	// See if object has spheres to draw
  for (co = 0; co < obj->contsize && !hasSpheres; co++)
    if (obj->cont[co].sizes)
      hasSpheres = 1;
	
	
  //## PRINT OBJECT'S DATA:
	
	fprintf(fout,"\n");
	fprintf(fout,"g ");
	printObjSafeUniqueObjectName( imod, ob, fout );
	fprintf(fout,"\n");
	
	if(printMatFile)
	{
		fprintf(fout,"usemtl ");
		printObjSafeUniqueObjectName( imod, ob, fout );
		fprintf(fout,"\n");
	}
	fprintf(fout,"\n");
	
	if( !objScattered )
	{
    if (iobjMesh(obj->flags))
      printMesh(imod, ob, fout);
	}
  if (hasSpheres) {
		if( objScattered || !onlyScatSpheres )
			printScatContours(imod, ob, fout);
  }
	
  numObjs++;
}





//------------------------
//-- Prints out the mesh generated over the given object ("ob") in
//-- OBJ format which involves first outputting the list of all points
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
  Ipoint vert, norm;
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
		
		//## OUTPUT VERTEXES (POINTS):
		int fVert = numVertices;					// first vertex in this group
		for(i = 0; i < mesh->vsize; i+=2)
		{
			vert = mesh->vert[i];
			if(rotateModel)
				fprintf(fout,"v %.5g %.5g %.5g\n",	vert.x, (vert.z * zscale), vert.y );
			else
				fprintf(fout,"v %.5g %.5g %.5g\n",	vert.x, vert.y, (vert.z * zscale) );
			numVertices++;
		}
		
		//## OUTPUT NORMALS (IF SPECIFIED):
		int fNorm = numNormals;					// first normal in this group
		if (printNormals)
		{
			fprintf(fout,"\n");
			for(i = 1; i < mesh->vsize; i+=2){
				norm = mesh->vert[i];
				imodPointNormalize(&norm);
				if(flipNormals)
					fprintf(fout,"vn %.3f %.3f %.3f\n", norm.x, norm.y, norm.z );
				else
					fprintf(fout,"vn %.3f %.3f %.3f\n", -norm.x, -norm.y, -norm.z );
				numNormals++;
			}
		}
		
		//## OUPUT FACES (TRIANGLES):
		fprintf(fout,"\n");
		for(i = 0; i < mesh->lsize; i++)
		{
			if (mesh->list[i] == IMOD_MESH_END)
				break;
			if ( !imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, &normAdd) )
				continue;
			
			// Copy the indices to the new index array numbered from 0
			lsize = 0;
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
				if(printNormals)
				{
					fprintf(fout,"f %d//%d %d//%d %d//%d\n", 
									ilist[ind+2]+fVert+1, ilist[ind+2]+fNorm+1, 
									ilist[ind+1]+fVert+1, ilist[ind+1]+fNorm+1,
									ilist[ind]  +fVert+1, ilist[ind]  +fNorm+1 );
				}
				else
				{
					fprintf(fout,"f %d %d %d\n", 
											  ilist[ind+2]+fVert+1, ilist[ind+1]+fVert+1, ilist[ind]+fVert+1);
				}
				numFaces++;
			}
		}
		
		//## FREE ANY MEMORY USED:
    if (ilist) 
      free(ilist);
  }
}


//------------------------
//-- Prints a scattered point object (in OBJ format) by printing a sphere for 
//-- every point in the object which has a radius.

static void printScatContours(Imod *imod, int ob, FILE *fout)
{
	Iobj *obj = &imod->obj[ob];
  Icont *cont;
  int hasSpheres = (iobjScat(obj->flags) || obj->pdrawsize) ? 1 : 0;
  int c, p;
  float zscale = imod->zscale;
  float radius;
  Ipoint pt;
	
	fprintf(fout,"# obj%d SPHERES:\n\n", ob+1);
	
  //## FOR EACH CONTOUR: PRINT EVERY POINT WHICH HAS A SPHERE SIZE:
	
	for(c = 0; c < obj->contsize; c++){
    cont = &(obj->cont[c]);
				
    if (!cont || !cont->psize								// if contour is empty
				|| (!hasSpheres && !cont->sizes))		// or has no sphere sizes:
      continue;																// skip this contour
		
		for (p = 0; p < cont->psize; p++)
		{
			fprintf(fout,"#   cont %d pt %d\n", c+1, p+1 );
			fprintf(fout,"g obj_%d_cont_%d_pt_%d\n", ob+1, c+1, p+1 );
			
			pt.x = cont->pts[p].x;
			pt.y = cont->pts[p].y;
			pt.z = cont->pts[p].z * zscale;
			
			if(rotateModel)
			{
				pt.y = cont->pts[p].z * zscale;
				pt.z = cont->pts[p].y;
			}
			
      radius = imodPointGetSize(obj, cont, p);
			
			if( useIcosahedrons )
				printSphereAsIcosahedron(pt, radius, fout);
			else
				printSphere( pt, radius, sphereSegments, fout );
			
			numSpheres++;
    }
  }
	fprintf(fout,"\n");	
}


//------------------------
//-- Prints a sphere as a "standard sphere" triangular mesh with the specified
//-- number of segments. This standard sphere works a lot like longditude and
//-- latitude whereby the top and bottom point connect with triangles to
//-- their adjacent points, while all other points connect together with
//-- rectangles.

static void printSphere(Ipoint pt, float radius, int segments, FILE *fout)
{
	int p, s, i, j;
	
	if(segments < 4)
		return;
	
	float DEGS_TO_RAD = 3.14159f/180.0f;
	
	int nPitch = segments / 2 + 1;
	float pitchInc = (180. / (float)nPitch  ) * DEGS_TO_RAD;
	float segInc   = (360. / (float)segments) * DEGS_TO_RAD;
	
	//## PRINT VERTEXES:
	
	fprintf(fout,"v %.5g %.5g %.5g\n", pt.x, pt.y+radius, pt.z );		// top vertex
	fprintf(fout,"v %.5g %.5g %.5g\n", pt.x, pt.y-radius, pt.z );		// bottom vertex
	numVertices += 2;
	
	int fVert = numVertices;		// index of first intermediate vertex
	for(p=1; p<nPitch; p++)			// generate all "intermediate vertices":
	{
		float out = fabs( radius * sin( (float)p * pitchInc ) );
		float y   = radius * cos( (float)p * pitchInc );
		
		for(s=0; s<segments; s++)
		{
			float x = out * cos( (float)s * segInc );
			float z = out * sin( (float)s * segInc );
			
			fprintf(fout,"v %.5g %.5g %.5g\n", x+pt.x, y+pt.y, z+pt.z );
			numVertices++;
		}
	}
	fprintf(fout,"\n");
	
	
	//## PRINT NORMALS:
	
	long fNorm;				// index of first intermediate normal
	if(printNormals)
	{
		fprintf(fout,"vn 0.0 1.0 0.0\n"  );			// top normal
		fprintf(fout,"vn 0.0 -1.0 0.0\n" );		  // bottom normal
		numNormals += 2;
		
		fNorm = numNormals;
		for(p=1; p<nPitch; p++)			// generate all "intermediate normals":
		{
			float outN = fabs(1.0f * sin( (float)p * pitchInc ));
			float yN   = cos( (float)p * pitchInc );
			
			for(s=0; s<segments; s++)
			{
				float xN = outN * cos( (float)s * segInc );
				float zN = outN * sin( (float)s * segInc );
				
				if(zN > -0.001f && zN < 0.001f)	zN = 0;
				if(xN > -0.001f && xN < 0.001f)	xN = 0;
				
				fprintf(fout,"vn %.5g %.5g %.5g\n", xN, yN, zN );
				numNormals++;
			}
		}
		fprintf(fout,"\n");
	}
	
	
	
	//## PRINT SQUARE FACES BETWEEN INTERMEDIATE POINTS:
	
	for(p=1; p<nPitch-1; p++)
	{
		for(s=0; s<segments; s++)
		{
			i = p*segments + s;
			j = (s==segments-1) ? i-segments : i;
			
			if(printNormals)
			{
				fprintf(fout,"f %d//%d %d//%d %d//%d %d//%d\n", 
								(i+1-segments)+fVert, (i+1-segments)+fNorm, 
								(j+2-segments)+fVert, (j+2-segments)+fNorm, 
								(j+2)+fVert,          (j+2)+fNorm, 
								(i+1)+fVert,          (i+1)+fNorm );
			}
			else
			{
				fprintf(fout,"f %d %d %d %d\n", 
								(i+1-segments)+fVert, (j+2-segments)+fVert, (j+2)+fVert, (i+1)+fVert );
			}
			numFaces++;
		}
	}
	
	//## PRINT TRIANGLE FACES CONNECTING TO TOP AND BOTTOM VERTEX:
	
	int offLastVerts  = fVert + (segments * (nPitch-2) );
	int offLastNorms  = fNorm + (segments * (nPitch-2) );
	for(s=0; s<segments; s++)
	{
		j = (s==segments-1) ? -1 : s;
		if(printNormals)
		{
			fprintf(fout,"f %d//%d %d//%d %d//%d\n",
							fVert-1,      fNorm-1,
							(j+2)+fVert,  (j+2)+fNorm,
							(s+1)+fVert,  (s+1)+fNorm );
			fprintf(fout,"f %d//%d %d//%d %d//%d\n",
							fVert,               fNorm,
							(s+1)+offLastVerts,  (s+1)+offLastNorms,
							(j+2)+offLastVerts,  (j+2)+offLastNorms );
		}
		else
		{
			fprintf(fout,"f %d %d %d\n", fVert-1, (j+2)+fVert,        (s+1)+fVert        );
			fprintf(fout,"f %d %d %d\n", fVert,   (s+1)+offLastVerts, (j+2)+offLastVerts );
		}
		numFaces = numFaces + 2;
	}
	
	fprintf(fout,"\n");	
}


//------------------------
//-- Prints a sphere as an isohedron - a regular polyedron with 20 identical
//-- equilateral triangular faces, 30 edges and 12 vertices.
//-- Code was modified from:
//-- http://www.csee.umbc.edu/~squire/reference/polyhedra.shtml#icosahedron

static void printSphereAsIcosahedron(Ipoint pt, float radius, FILE *fout)
{
	int i;
	Ipoint v[12];
	
	v[0].x = 0.000;			v[0].y = 1.000;			v[0].z = 0.000;			// top-most point
	v[1].x = 0.894;			v[1].y =  0.447; 		v[1].z = 0.000;
	v[2].x = 0.276;			v[2].y =  0.447;		v[2].z = 0.851;
	v[3].x = -0.724;		v[3].y =  0.447;		v[3].z = 0.526;
	v[4].x = -0.724;		v[4].y =  0.447;		v[4].z = -0.526;
	v[5].x = 0.276;			v[5].y =  0.447;		v[5].z = -0.851;
	v[6].x = 0.724;			v[6].y = -0.447;		v[6].z = 0.526;
	v[7].x = -0.276;		v[7].y = -0.447;		v[7].z = 0.851;
	v[8].x = -0.894;		v[8].y = -0.447;		v[8].z = 0.000;
	v[9].x = -0.276;		v[9].y = -0.447;		v[9].z = -0.851;
  v[10].x= 0.724;			v[10].y= -0.447;		v[10].z= -0.526;
  v[11].x= 0.000;			v[11].y= -1.000;		v[11].z= 0.000;			// bottom-most point
	
	//## PRINT VERTEXES:
	
	for(i=0; i<12; i++)
	{
		fprintf(fout,"v %.5g %.5g %.5g\n",
						v[i].x*radius+pt.x,
						v[i].y*radius+pt.y,
						v[i].z*radius+pt.z );
		numVertices++;
	}
	fprintf(fout,"\n");	
	
	//## PRINT FACES:
	
	fprintf(fout,"f -12 -10 -11\n");		// |-- top-most triangles
  fprintf(fout,"f -12 -9 -10\n");			// |
  fprintf(fout,"f -12 -8 -9\n");			// |
  fprintf(fout,"f -12 -7 -8\n");			// | 
  fprintf(fout,"f -12 -11 -7\n");			// |
  fprintf(fout,"f -1 -6 -5\n");			// |-- bottom-most triangles
  fprintf(fout,"f -1 -5 -4\n");			// |
  fprintf(fout,"f -1 -4 -3\n");			// |
  fprintf(fout,"f -1 -3 -2\n");			// |
  fprintf(fout,"f -1 -2 -6\n");			// |
  fprintf(fout,"f -11 -10 -6\n");			// |-- downwards pointing
  fprintf(fout,"f -10 -9 -5\n");			// |   triangles
  fprintf(fout,"f -9 -8 -4\n");				// |
  fprintf(fout,"f -8 -7 -3\n");				// |
  fprintf(fout,"f -7 -11 -2\n");			// |
  fprintf(fout,"f -6 -10 -5\n");		// |-- upwards pointing
  fprintf(fout,"f -5 -9 -4\n");			// |   triangles
  fprintf(fout,"f -4 -8 -3\n");			// |
  fprintf(fout,"f -3 -7 -2\n");			// |
  fprintf(fout,"f -2 -11 -6\n");		// |
	numFaces = numFaces + 20;
	
	fprintf(fout,"\n");
}



//------------------------
//-- Takes an imod model and outputs the meshes into a Wavefront's MTL
//-- (Material Template Library) file by calling "printMaterial" on each object.
//-- For more information about MTL format see:
//-- > http://en.wikipedia.org/wiki/Material_Template_Library   (official specs)

void imod_to_mtl(Imod *imod, FILE *fout)
{
	int ob;
  fprintf(fout,"# WaveFront *.mtl file (generated from an IMOD model by imod2obj)\n");
	
  fprintf(fout,"\n\n");
	for(ob = 0; ob < imod->objsize; ob++)
    printMaterial(imod, ob, fout);
	fprintf(fout,"\n\n");
	fprintf(fout,"# For more info on MTL file format see:\n");
	fprintf(fout,"#  http://en.wikipedia.org/wiki/Material_Template_Library");
}

//------------------------
//-- Prints out the Material Template Library code representing the 
//-- material properties of the specified object ("ob").

static void printMaterial(Imod *imod, int ob, FILE *fout)
{
	Iobj *obj = &imod->obj[ob];
  float ambient   = ((int)obj->ambient)   / 255.;
	float diffuse   = ((int)obj->diffuse)   / 255.;
  float specular  = ((int)obj->specular)  / 255.;
  float shininess = ((int)obj->shininess) / 255.;
  float r = (float)obj->red;
  float g = (float)obj->green;
  float b = (float)obj->blue;
	
  if (shininess < 0.01)
    shininess= 0.01;
	
  //## PRINT MATERIAL 
	//## CAN REFERENCE THIS TAG:
	
	fprintf(fout,"\n");
	fprintf(fout,"#MATERIAL FOR OBJECT %d:\n", (ob+1) );
  fprintf(fout,"newmtl ");
	printObjSafeUniqueObjectName(imod, ob, fout );
	fprintf(fout,"\n");
	fprintf(fout,"Ka %g %g %g\n", r*ambient, g*ambient, b*ambient);
  fprintf(fout,"Kd %g %g %g\n", r*diffuse, g*diffuse, b*diffuse);
  fprintf(fout,"Ks %g %g %g\n", r*specular, g*specular, b*specular);
  fprintf(fout,"Ns %g\n", shininess*1000.);
  fprintf(fout,"d %g\n",  obj->trans/100.0f);		//|-- transparency
	fprintf(fout,"Tr %g\n", obj->trans/100.0f);		//|
  fprintf(fout,"\n");
}



/*
 $Log: imod2obj.cpp,v $
 Revision 1.0  2011/12/22 21:37:14  noske
 Modified file from "imod2vrml.c" to output OBJ format
 */

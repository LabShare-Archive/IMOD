/*  IMOD VERSION 2.6.2
 *
 *  imodfillin.c --  Fills in missing contours based on mesh data
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

/*  $Author$

    $Date$

    $Revision$

    $Log$
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <imodel.h>

int *parselist (char *line, int *nlist);
void fillin_from_mesh(Imod *imod, int ob, int newobj, int zinc, float tol);
#define DEFAULT_TOL 0.25

static void imodfillin_usage(char *name, int retcode)
{
     printf("usage: %s [options] <input model> <output model>\n", name);
     printf("options:\n");

     printf("\t-e\tPlace new contours in existing objects.\n");
     printf("\t-n\tPlace new contours in new objects (default).\n");
     printf("\t-o list\tFill in the given list of objects only (ranges allowed).\n");
     printf("\t-i #\tFill in only gaps bigger than the given Z increment.\n");
     printf("\t-R #\tTolerance (maximum error) for point reduction"
	    " (default %.2f).\n", DEFAULT_TOL);
     exit(retcode);
}

main( int argc, char *argv[])
{
     int  i, c, ob;
     int  zinc = 1;
     int  newobj = TRUE;
     float tol = DEFAULT_TOL;
     int  obj_list_size = 0;
     int  *obj_list;
     Imod *model;
     char backname[257];
     int  obsave, cosave, ptsave;
     int doit;
     int origsize;
     int anymesh = 0;

     char *options = "o:i:R:en";
     extern char *optarg;
     extern int optind;
     
     if (argc == 1){
	  imodVersion(argv[0]);
	  imodCopyright();
	  imodfillin_usage(argv[0], 0);
     }

     while ((c = getopt(argc, argv, options)) != EOF)
	  switch (c){
	       
	     case 'i':
	       zinc = atoi(optarg);
	       if (zinc < 1)
		    zinc = 1;
	       break;
	       
	     case 'R':
	       tol = atof(optarg);
	       if (tol <= 0.001)
		    tol = 0.001;
	       break;
	       
	     case 'o':
	       obj_list = parselist(optarg, &obj_list_size);
	       if (!obj_list) {
	         fprintf(stderr, "%s: Error parsing object list\n", argv[0]);
		 exit(-1);
	       }
	       break;
	       
	     case 'e':
	       newobj = 0;
	       break;

	     case 'n':
	       newobj = 1;
	       break;

	     case '?':
	     default:
	       imodfillin_usage(argv[0], -1);
	       break;
	       
	  }

     if (optind >= argc - 1)
	  imodfillin_usage(argv[0], -1);

    model = imodRead(argv[argc - 2]);
    if (!model) {
      fprintf(stderr, "%s: Fatal error reading model %s\n", argv[0], 
	      argv[argc - 2]);
      exit(1);
    }

    imodGetIndex(model, &obsave, &cosave, &ptsave);

    /* Loop on objects, finding ones with meshes that are either on the list
     or consist of closed contours */
    origsize = model->objsize;
    for (ob = 0; ob < origsize; ob++) {
	 doit = model->obj[ob].meshsize;
	 if (doit)
	      anymesh = 1;
	 if (doit && obj_list_size) {
	      doit = 0;
	      for (i = 0; i < obj_list_size; i++)
		   if (ob + 1 == obj_list[i]) doit = 1;
	 } else if (doit) 
	      doit = iobjClose(model->obj[ob].flags);

	 if (doit) {
	      printf("Examining object %d\n", ob + 1);
	      fillin_from_mesh(model, ob, newobj, zinc, tol);
	 }
    }    

    if (!anymesh)
	 printf ("No objects with meshes found; be sure to run imodmesh "
		 "with the -s flag\n");

    imodSetIndex(model, obsave, cosave, ptsave);

    sprintf(backname, "%s~", argv[argc - 1]);
    rename (argv[argc - 1], backname);
    if (imodOpenFile(argv[argc - 1], "w", model)) {
      fprintf(stderr, "%s: Fatal error opening new model %s\n", argv[0],
	      argv[argc - 1]);
      exit (1);
    }
    imodWriteFile(model);
    exit(0);
}

void fillin_from_mesh(Imod *imod, int ob, int newobj, int zinc, float tol)
{
     Iobj *obj = &imod->obj[ob];
     Iobj *destobj = obj;   /* Destination object */
     int resol;         /* resolution for highest res mesh */
     int me, i, j;      /* indices */
     int *listp;        /* pointer to normal-vertex indices */
     int ninpoly;       /* # of vertexes in polygon */
     Ipoint *vertp;     /* pointer to normal-vertex list */
     float zmax, zmin;  /* max and min z in polygon */
     int ntriang;       /* # of triangles in polygon */
     int itri, jtri;    /* triangle indexes */
     Ipoint ptadd;      /* Point to add */
     int zadd;          /* Z value at which to add contour */
     int firstv;        /* index of first vertex in polygon */
     int ind, ind1, ind2, jnd1, jnd2, jnd3, indv, jndv, done;
     Icont *cont;       /* contour being added to */
     int coadd;         /* number of that contour */
     float z1, z2;      /* z values of two candidate vertices */
     float frac;        /* interpolation fraction */
     float red, green, blue;

     imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
     for (me = 0; me < obj->meshsize; me++) {
	  if (imeshResol(obj->mesh[me].flag) != resol) 
	       continue;
     
	  listp = obj->mesh[me].list;
	  vertp = obj->mesh[me].vert;
	  i = 0;
	  while (i < obj->mesh[me].lsize) {
	       if (listp[i++] == IMOD_MESH_BGNPOLYNORM) {
		    
		    /* Find min and max Z in polygon, first vertex index and
		       number of vertices */
		    zmin = vertp[listp[i + 1]].z;
		    zmax = zmin;
		    firstv = i + 1;   /* first vertex index */
		    ninpoly = 0;
		    while (listp[i++] != IMOD_MESH_ENDPOLY) {
			 ind = listp[i++];
			 if (vertp[ind].z < zmin)
			      zmin = vertp[ind].z;
			 if (vertp[ind].z > zmax)
			      zmax = vertp[ind].z;
			 ninpoly++;
		    }
		    
		    
		    /* If difference in Z is not > than desired increment, 
		       skip */
		    if (zmax - zmin <= zinc) 
			 continue;
		    
		    /* See if need to make a new object now */
		    if (newobj) {
			 imodNewObject(imod);
			 obj = &imod->obj[ob];
			 destobj = &imod->obj[imod->objsize - 1];
			 /* Copy properties, then restore new color and clear 
			    out data */
			 red = destobj->red;
			 green = destobj->green;
			 blue = destobj->blue;
			 imodObjectCopy(obj, destobj);
			 destobj->red = red;
			 destobj->green = green;
			 destobj->blue = blue;
			 destobj->cont = NULL;
			 destobj->mesh = NULL;
			 destobj->contsize = 0;
			 destobj->meshsize = 0;
			 destobj->store = NULL;
			 printf("Adding new contours to new object %d\n", 
				imod->objsize);
			 newobj = 0;
		    }
		    
		    zadd = floor((double)(zmin + zinc + 0.001));
		    ntriang = ninpoly / 3;
		    while (zadd < zmax) {
			 cont = imodContourNew();
			 coadd = imodObjectAddContour(destobj, cont);
			 if (coadd < 0) {
			      fprintf(stderr, "Fatal error: cannot get new "
				      "contouror add it to object");
			      exit(-1);
			 }
			 imodContourDelete(cont);
			 cont = &destobj->cont[coadd];
			 cont->surf = obj->mesh[me].pad;
			 cont->type = obj->mesh[me].type;
			 
			 /* loop on triangles */
			 for (itri = 0; itri < ntriang; itri++) {
			      indv = firstv + itri * 6;
			      
			      /* Look at the three pairs of vertices in 
				 triangle, see if any bracket zadd */
			      for (j = 0; j < 3; j++) {
				   ind1 = listp[indv + j * 2];
				   ind2 = listp[indv + (j * 2 + 2) % 6];
				   z1 = vertp[ind1].z;
				   z2 = vertp[ind2].z;
				   if (!((z1 > zadd && z2 < zadd) ||
					 (z1 < zadd && z2 > zadd)))
					continue;
				   
				   /* If it brackets, look back to see that 
				      this pair of vertices hasn't been done 
				      already */
				   done = 0;
				   for (jtri = itri - 1; jtri >= 0; jtri--) {
					jndv = firstv + jtri * 6;
					jnd1 = listp[jndv];
					jnd2 = listp[jndv + 2];
					jnd3 = listp[jndv + 4];
					if (ind1 == jnd1 && ind2 == jnd2 ||
					    ind2 == jnd1 && ind1 == jnd2 ||
					    ind1 == jnd2 && ind2 == jnd3 ||
					    ind2 == jnd2 && ind1 == jnd3 ||
					    ind1 == jnd3 && ind2 == jnd1 ||
					    ind2 == jnd3 && ind1 == jnd1) {
					     done = 1;
					     break;
					}
				   }
				   if (done)
					continue;
				   
				   /* It passes the test, add interpolated 
				      point */
				   frac = (zadd - z1) / (z2 - z1);
				   ptadd.z = zadd;
				   ptadd.x = (1. - frac) * vertp[ind1].x +
					frac * vertp[ind2].x;
				   ptadd.y = (1. - frac) * vertp[ind1].y +
					frac * vertp[ind2].y;
				   imodPointAppend(cont, &ptadd);
			      }
			 }
			 imodContourReduce(cont, tol);
			 zadd += zinc;
		    }
	       }
	  }
     }
}
     

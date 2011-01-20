/*
 *  imodfillin.c --  Fills in missing contours based on mesh data
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"

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

int main( int argc, char *argv[])
{
  int  i, c, ob;
  int  zinc = 1;
  int  newobj = TRUE;
  float tol = DEFAULT_TOL;
  int  obj_list_size = 0;
  int  *obj_list;
  Imod *model;
  int  obsave, cosave, ptsave;
  int doit;
  int origsize;
  int anymesh = 0;
  char *progname = imodProgName(argv[0]);

  if (argc == 1){
    imodVersion(progname);
    imodCopyright();
    imodfillin_usage(progname, 0);
  }

  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
               
      case 'i':
        zinc = atoi(argv[++i]);
        if (zinc < 1)
          zinc = 1;
        break;
               
      case 'R':
        tol = atof(argv[++i]);
        if (tol <= 0.001)
          tol = 0.001;
        break;
               
      case 'o':
        obj_list = parselist(argv[++i], &obj_list_size);
        if (!obj_list) {
          fprintf(stderr, "%s: Error parsing object list\n", progname);
          exit(3);
        }
        break;
               
      case 'e':
        newobj = 0;
        break;

      case 'n':
        newobj = 1;
        break;

      case '?':
        imodfillin_usage(progname, -1);
        break;

      default:
        fprintf(stderr, "%s: unknown option %s\n", progname, argv[i]);
        imodfillin_usage(progname, -1);
        break;
               
      }
    } else
      break;
  }

  if (i >= argc - 1)
    imodfillin_usage(progname, -1);

  model = imodRead(argv[argc - 2]);
  if (!model) {
    fprintf(stderr, "%s: Fatal error reading model %s\n", progname, 
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
  imodObjviewComplete(model);

  imodBackupFile(argv[argc - 1]);
  if (imodOpenFile(argv[argc - 1], "wb", model)) {
    fprintf(stderr, "%s: Fatal error opening new model %s\n", progname,
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
  int me, i, ind;      /* indices */
  int *listp;        /* pointer to normal-vertex indices */
  int ninpoly;       /* # of vertexes in polygon */
  Ipoint *vertp;     /* pointer to normal-vertex list */
  float zmax, zmin;  /* max and min z in polygon */
  int ntriang;       /* # of triangles in polygon */
  int zadd;          /* Z value at which to add contour */
  int firstv;        /* index of first vertex in polygon */
  Icont *cont;       /* contour being added to */
  int coadd;         /* number of that contour */
  float red, green, blue;
  int listInc, vertBase, normAdd;

  imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
  for (me = 0; me < obj->meshsize; me++) {
    if (imeshResol(obj->mesh[me].flag) != resol) 
      continue;
     
    listp = obj->mesh[me].list;
    vertp = obj->mesh[me].vert;
    i = 0;
    while (i < obj->mesh[me].lsize) {
      if (imodMeshPolyNormFactors(listp[i++], &listInc, &vertBase, &normAdd)) {
                    
        /* Find min and max Z in polygon, first vertex index and
           number of vertices */
        firstv = i + vertBase;   /* first vertex index */
        zmin = vertp[listp[firstv]].z;
        zmax = zmin;
        ninpoly = 0;
        while (listp[i] != IMOD_MESH_ENDPOLY) {
          ind = listp[i + vertBase];
          i += listInc;
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
          destobj->meshParam = NULL;
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
                    "contour or add it to object");
            exit(3);
          }
          free(cont);
          cont = &destobj->cont[coadd];
          cont->surf = obj->mesh[me].surf;
          cont->time = obj->mesh[me].time;
                         
          imodMeshInterpCont(listp, vertp, ntriang, firstv, listInc, zadd, cont);
          imodContourReduce(cont, tol);
          zadd += zinc;
        }
      }
    }
  }
}

     
/*

  $Log$
  Revision 3.13  2008/04/04 21:21:28  mast
  Free contour after adding to object

  Revision 3.12  2006/09/12 15:02:42  mast
  rename mesh members, null out meshparam

  Revision 3.11  2006/06/26 14:48:49  mast
  Added b3dutil include for parselist

  Revision 3.10  2005/09/11 19:22:11  mast
  Changes for new style of mesh

  Revision 3.9  2005/02/11 01:42:33  mast
  Warning cleanup: implicit declarations, main return type, parentheses, etc.

  Revision 3.8  2004/11/05 19:05:29  mast
  Include local files with quotes, not brackets

  Revision 3.7  2004/07/07 19:25:30  mast
  Changed exit(-1) to exit(3) for Cygwin

  Revision 3.6  2003/10/26 14:46:41  mast
  fixed problem in eliminating getopt

  Revision 3.5  2003/10/24 03:05:23  mast
  open as binary, strip program name and/or use routine for backup file

  Revision 3.4  2003/08/01 00:16:14  mast
  Complete object views when saving model, since there may be new objects

  Revision 3.3  2003/02/21 23:15:33  mast
  Open new file as wb

  Revision 3.2  2002/06/21 00:26:03  mast
  Needed to swap points from first triangle of a strip if they were out of
  order relative to the following points of the strip

  Revision 3.1  2001/12/17 18:53:55  mast
  First version of program

*/

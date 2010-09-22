/*
 *  $Id$
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  Log at end
 */

#include <stdio.h>
#include <string.h>
#include "parse_params.h"
#include "imodel.h"

#define DEFAULT_SCALE 10.0

Imod *imod_from_patches(FILE *fin, float scale, int clipSize, char *name,
                        int noflip, int ignoreZero, int countLines);
static void usage(char *prog)
{
  imodVersion(prog);
  imodCopyright();
  printf("Usage: %s [options] patch_file output_model\n", prog);
  printf("Options:\n");
  printf("\t-s #\tScale vectors by given value (default %.1f)\n",
         DEFAULT_SCALE);
  printf("\t-f\tDo NOT flip the Y and Z coordinates\n");
  printf("\t-n name\tAdd given name to model object\n");
  printf("\t-c #\tSet up clipping planes enclosing area of given size\n");
  printf("\t-z\tIgnore zero values when using SD to limit stored "
         "maximum value\n");
  printf("\t-l\Use all lines in file rather than getting line count from first"
         " line\n");
  exit(1);
}

int main( int argc, char *argv[])
{
  int i;
  FILE *fin, *fout;
  Imod *Model;
  float scale = 10.0;
  int clipSize = 0;
  int noflip = 0;
  int ignoreZero = 0;
  int countLines = 0;
  char *name = NULL;

  /* This name is hard-coded because of the script wrapper needed in Vista */
  char progname[] = "patch2imod";

  if (argc < 2)
    usage(progname);

  setExitPrefix("ERROR: patch2imod - ");
  
  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
        
      case 's':
        sscanf(argv[++i], "%f", &scale);
        break;

      case 'n':
        name = strdup(argv[++i]);
        break;

      case 'c':
        clipSize = atoi(argv[++i]);
        break;

      case 'f':
        noflip = 1;
        break;

      case 'z':
        ignoreZero = 1;
        break;

      case 'l':
        countLines = 1;
        break;

      default:
        exitError("Illegal argument %s\n", argv[i]);
        break;
      }
    } else
      break;
  }
  if (i > (argc - 2)) {
    printf("ERROR: patch2imod - Wrong # of arguments\n");
    usage(progname);
  }

  fin = fopen(argv[i++], "r");
  if (!fin)
    exitError("Couldn't open %s\n", argv[--i]);

  if (imodBackupFile(argv[i])) {
    exitError("Renaming existing output file "
            "to %s~\n", argv[i]);
    exit(1);
  }
  fout = fopen(argv[i], "wb");
  if (!fout)
    exitError("Could not open %s\n", argv[i]);
  Model = (Imod *)imod_from_patches(fin, scale, clipSize, name, noflip, 
                                    ignoreZero, countLines);
     
  imodWrite(Model, fout);

  imodDelete(Model);
  exit(0);
}





#define MAXLINE 128

Imod *imod_from_patches(FILE *fin, float scale, int clipSize, char *name,
                        int noflip, int ignoreZero, int countLines)
{
  int len;
  int i, npatch, nread;
     
  char line[MAXLINE];
  Imod *mod;
  Iobj *obj;
  Istore store, store2;
  Ipoint *pts;
  int ix, iy, iz, itmp;
  int xmin, ymin, zmin, xmax, ymax, zmax;
  float dx, dy, dz, xx, yy, value, value2, valmin, valmax, tmp;
  double valsum, sumsq, valsd, sdmax;
  int residuals = 0;
  int nvals = 0;
  int dzvary = 0;

  if (countLines) {
    npatch = 0;
    while (1) {
      ix = fgetline(fin,line,MAXLINE);
      if (ix > 2)
        npatch++;
      else
        break;
    }
    if (npatch < 1)
      exitError("No usable lines in the file");
    rewind(fin);
  } else {
    fgetline(fin,line,MAXLINE);
    sscanf(line, "%d", &npatch);
    if (npatch < 1)
      exitError("Implausible number of patches = %d.", npatch);

    if (strstr(line, "residuals") != NULL)
      residuals = 1;
  }

  mod = imodNew();     
  if (!mod)
    exitError("Could not get new model");

  if (imodNewObject(mod))
    exitError("Could not get new object");
  obj = mod->obj;
  obj->contsize = npatch;
  obj->cont = imodContoursNew(npatch);
  if (!obj->cont)
    exitError("Could not get contour array");
  obj->flags |= IMOD_OBJFLAG_OPEN;
  if (!residuals && !noflip)
    mod->flags |= IMODF_FLIPYZ;
  mod->pixsize = scale;
  xmin = ymin= zmin = 1000000;
  xmax = ymax = zmax = -1000000;
  valmin = 1.e30;
  valmax = -valmin;
  dz = 0.;
  store.type = GEN_STORE_VALUE1;
  store.flags = GEN_STORE_FLOAT << 2;
  store2.type = GEN_STORE_VALUE2;
  store2.flags = GEN_STORE_FLOAT << 2;

  for (i = 0; i < npatch; i++) {
    pts = (Ipoint *)malloc(2 * sizeof(Ipoint));
    if (!pts)
      exitError("Could not get new point array");

    obj->cont[i].pts = pts;
    obj->cont[i].psize = 2;
    len = fgetline(fin,line, MAXLINE);
    if (len < 3)
      exitError("Error reading file at line %d.", i + 1);

    /* DNM 7/26/02: read in residuals as real coordinates, without a 
       flip */
    if (residuals) {
      nread = sscanf(line, "%f %f %d %f %f", &xx, &yy, &iz, &dx, &dy);
    } else {
      /* DNM 11/15/01: have to handle either with commas or without,
     depending on whether it was produced by patchcorr3d or 
     patchcrawl3d */
      if (strchr(line, ','))
        nread = sscanf(line, "%d %d %d %f, %f, %f", &ix, &iz, &iy, &dx,
           &dz, &dy);
      else
        nread = sscanf(line, "%d %d %d %f %f %f %f %f", &ix, &iz, &iy, &dx,
                       &dz, &dy, &value, &value2);
      if (noflip) {
        itmp = iy;
        iy = iz;
        iz = itmp;
        tmp =dy;
        dy = dz;
        dz = tmp;
      }
      xx = ix;
      yy = iy;
    }

    pts[0].x = xx;
    pts[0].y = yy;
    pts[0].z = iz;
    pts[1].x = xx + scale * dx;
    pts[1].y = yy + scale * dy;
    pts[1].z = iz + scale * dz;
    xmin = B3DMIN(xmin, xx);
    ymin = B3DMIN(ymin, yy);
    zmin = B3DMIN(zmin, iz);
    xmax = B3DMAX(xmax, xx);
    ymax = B3DMAX(ymax, yy);
    zmax = B3DMAX(zmax, iz);
    if (dz != 0.)
      dzvary = 1;

    if (nread > 6) {
      valmin = B3DMIN(valmin, value);
      valmax = B3DMAX(valmax, value);
      if (value || !ignoreZero) {
        nvals++;
        valsum += value;
        sumsq += value * value;
      }
      store.index.i = i;
      store.value.f = value;
      if (istoreInsert(&obj->store, &store))
        exitError("Could not add general storage item");
    }
    if (nread > 7) {
      store2.index.i = i;
      store2.value.f = value2;
      if (istoreInsert(&obj->store, &store2))
        exitError("Could not add general storage item");
    }
  }
     
  if (residuals) {
    obj->symflags = IOBJ_SYMF_ENDS;
    obj->symbol = IOBJ_SYM_NONE;
    obj->symsize = 10;
  } else if (clipSize) {
    imodViewModelNew(mod);
    for (i = 0; i <  mod->viewsize; i++)
      mod->view[i].world |= WORLD_MOVE_ALL_CLIP;
    obj->clips.count = 4;
    obj->clips.flags = 0;
    obj->clips.normal[0].x = 1.;
    obj->clips.normal[1].x = -1.;
    obj->clips.normal[2].x = 0.;
    obj->clips.normal[3].x = 0.;
    obj->clips.normal[0].y = 0.;
    obj->clips.normal[1].y = 0.;
    obj->clips.normal[2].y = 1.;
    obj->clips.normal[3].y = -1.;
    for (i = 0; i < 4; i++) {
      obj->clips.normal[i].z = 0.;
      obj->clips.point[i].x = -0.5 * (xmax + xmin);
      obj->clips.point[i].y = -0.5 * (ymax + ymin);
      obj->clips.point[i].z = -0.5 * (zmax + zmin);
      if (obj->clips.normal[i].x)
        obj->clips.point[i].x += obj->clips.normal[i].x * clipSize / 2;
      if (obj->clips.normal[i].y)
        obj->clips.point[i].y += obj->clips.normal[i].y * clipSize / 2;
    }      
  }

  mod->xmax = xmax + xmin;
  mod->ymax = ymax + ymin;
  mod->zmax = zmax + zmin;
  if (dzvary)
    obj->symbol = IOBJ_SYM_CIRCLE;

  /* Set current thicken contour flag to aid deleting patches */
  obj->flags |= IMOD_OBJFLAG_THICK_CONT | IMOD_OBJFLAG_MCOLOR;
  if (nvals) {
    if (nvals > 10) {
      valsd = (sumsq - valsum * valsum / nvals) / (nvals - 1);
      if (valsd > 0) {
        sdmax = valsum / nvals + 10. * sqrt(valsd);
        valmax = B3DMIN(valmax, sdmax);
      }
    }

    if (istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, valmin, valmax))
      exitError("Could not add general storage item");
  }

  if (name)
    imodObjectSetName(obj, name);

  return(mod);
     
}

/*

$Log$
Revision 3.14  2009/09/08 23:21:46  mast
Added option to ignore zero values when computing limit to maximum

Revision 3.13  2009/09/03 04:44:11  mast
Added ability to store another column in general value 2

Revision 3.12  2007/12/14 23:59:34  mast
Hard-coded name

Revision 3.11  2006/10/03 14:37:30  mast
Added option to prevent flipping of vector model.

Revision 3.10  2006/08/31 20:57:16  mast
Added value encoding and name and clipping plane options

Revision 3.9  2006/08/28 14:28:25  mast
removed internal fgetline

Revision 3.8  2005/03/20 19:56:05  mast
Eliminating duplicate functions

Revision 3.7  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.6  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.5  2004/04/28 05:29:31  mast
Set flag to draw current contour thicker

Revision 3.4  2003/10/24 03:05:24  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.3  2002/12/23 21:32:55  mast
Fixed exit status and made residual model have end-markers set

Revision 3.2  2002/07/27 23:50:47  mast
Eliminated line for test output

Revision 3.1  2002/07/27 06:00:43  mast
Added ability to convert a residual listing from Tiltalign

*/

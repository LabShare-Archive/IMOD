/*
 *  $Id$
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 */

#include <stdio.h>
#include <string.h>
#include "parse_params.h"
#include "imodel.h"
#include "warpfiles.h"

#define P2I_NO_FLIP          1
#define P2I_IGNORE_ZERO      2
#define P2I_COUNT_LINES      4
#define P2I_DISPLAY_VALUES   8
#define P2I_READ_WARP       16
#define P2I_TIMES_FOR_Z     32


#define DEFAULT_SCALE 10.0

static Imod *imod_from_patches(FILE *fin, float scale, int clipSize, char *name, 
                               int flags);

static void usage(char *prog)
{
  imodVersion(prog);
  imodCopyright();
  printf("Usage: %s [options] patch_file output_model\n", prog);
  printf("Options:\n");
  printf("\t-s #\tScale vectors by given value (default %.1f)\n", DEFAULT_SCALE);
  printf("\t-f\tDo NOT flip the Y and Z coordinates\n");
  printf("\t-n name\tAdd given name to model object\n");
  printf("\t-c #\tSet up clipping planes enclosing area of given size\n");
  printf("\t-d #\tSet flag to display values in false color in 3dmod\n");
  printf("\t-z\tIgnore zero values when using SD to limit stored maximum value\n");
  printf("\t-l\tUse all lines in file rather than getting line count from first line\n");
  printf("\t-w\tRead input file as a warping transformation file\n");
  printf("\t-t\tGive each contour a time equal to its Z value plus 1\n");

  exit(1);
}

int main( int argc, char *argv[])
{
  int i;
  FILE *fin = NULL, *fout;
  Imod *Model;
  float scale = 10.0;
  int clipSize = 0;
  int flags = 0;
  int nxWarp, nyWarp, nzWarp, warpFlags, version, indWarp, ibin;
  float warpPixel;
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
        flags |= P2I_NO_FLIP;
        break;

      case 'z':
        flags |= P2I_IGNORE_ZERO;
        break;

      case 'l':
        flags |= P2I_COUNT_LINES;
        break;

      case 'd':
        flags |= P2I_DISPLAY_VALUES;
        break;

      case 'w':
        flags |= P2I_READ_WARP;
        break;

      case 't':
        flags |= P2I_TIMES_FOR_Z;
        break;

      default:
        exitError("Illegal argument %s", argv[i]);
        break;
      }
    } else
      break;
  }
  if (i > (argc - 2)) {
    printf("ERROR: patch2imod - Wrong # of arguments");
    usage(progname);
  }

  if (flags & P2I_READ_WARP) {
    indWarp = readWarpFile(argv[i++], &nxWarp, &nyWarp, &nzWarp, &ibin, &warpPixel,
                           &version, &warpFlags);
    if (indWarp < 0)
      exitError("Reading %s as a warping file", argv[--i]);
                           
  } else {

    fin = fopen(argv[i++], "r");
    if (!fin)
      exitError("Couldn't open %s", argv[--i]);
  }

  if (imodBackupFile(argv[i])) {
    exitError("Renaming existing output file to %s~", argv[i]);
    exit(1);
  }
  fout = fopen(argv[i], "wb");
  if (!fout)
    exitError("Could not open %s", argv[i]);
  Model = (Imod *)imod_from_patches(fin, scale, clipSize, name, flags);
     
  imodWrite(Model, fout);

  imodDelete(Model);
  exit(0);
}


#define MAXLINE 128

static Imod *imod_from_patches(FILE *fin, float scale, int clipSize, char *name,
                               int flags)
{
  int noflip = flags & P2I_NO_FLIP;
  int ignoreZero = flags & P2I_IGNORE_ZERO;
  int countLines = flags & P2I_COUNT_LINES;
  int displayValues = flags & P2I_DISPLAY_VALUES;
  int readWarp = flags & P2I_READ_WARP;
  int timesForZ = flags & P2I_TIMES_FOR_Z;
  int len, i;
     
  char line[MAXLINE];
  Imod *mod;
  Iobj *obj;
  Istore store, store2;
  Ipoint *pts;
  int ix, iy, iz, itmp;
  int nxWarp, nyWarp, ifControl, nxGrid, nyGrid, numControl, maxProd, izWarp;
  float xStart, yStart, xInterval, yInterval;
  int xmin, ymin, zmin, xmax, ymax, zmax;
  float dx, dy, dz, xx, yy, value, value2, valmin, valmax, tmp;
  double valsum, sumsq, valsd, sdmax;
  float *xVector, *yVector, *xControl, *yControl;
  float *dxGrid = NULL, *dyGrid = NULL;
  int residuals = 0;
  int nvals = 0;
  int dzvary = 0;
  int npatch = 0;
  int nzWarp = 1;
  int nread = 0;
  int contBase = 0;

  if (readWarp) {
    getWarpFileSize(&nxWarp, &nyWarp, &nzWarp, &ifControl);
    maxProd = 0;
    for (iz = 0; iz < nzWarp; iz++) {
      if (ifControl)
        ix = getNumWarpPoints(iz, &numControl);
      else
        ix = getWarpGridSize(iz, &nxGrid, &nyGrid, &numControl);
      if (ix)
        exitError("Getting number of points in warping file at section %d", iz);
      npatch += numControl;
      maxProd = B3DMAX(maxProd, numControl);
    }
    if (!ifControl) {
      dxGrid = B3DMALLOC(float, maxProd);
      dyGrid = B3DMALLOC(float, maxProd);
      if (!dxGrid || !dyGrid)
        exitError("getting memory for warping grids");
    }
  } else if (countLines) {
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
  if (!residuals && !noflip && !readWarp)
    mod->flags |= IMODF_FLIPYZ;
  if (timesForZ)
    obj->flags |= IMOD_OBJFLAG_TIME;
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

  for (izWarp = 0; izWarp < nzWarp; izWarp++) {
    if (readWarp) {
      iz = izWarp;
      if (ifControl) {
        if (getNumWarpPoints(iz, &npatch) || 
            getWarpPointArrays(iz, &xControl, &yControl, &xVector, &yVector))
          exitError("Getting number of control points or arrays for section %d", iz);
      } else {
        if (getWarpGrid(iz, &nxGrid, &nyGrid, &xStart, &yStart, &xInterval, &yInterval,
                        dxGrid, dyGrid, 0))
          exitError("Getting warp grid for section %d", iz);
        npatch = nxGrid * nyGrid;
      }
    }

    for (i = 0; i < npatch; i++) {
      pts = (Ipoint *)malloc(2 * sizeof(Ipoint));
      if (!pts)
        exitError("Could not get new point array");

      obj->cont[i + contBase].pts = pts;
      obj->cont[i + contBase].psize = 2;
      if (timesForZ)
        obj->cont[i + contBase].time = B3DMAX(1, iz + 1);
      if (readWarp) {
        if (ifControl) {
          xx = xControl[i];
          yy = yControl[i];
          dx = -xVector[i];
          dy = -yVector[i];
        } else {
          ix = i % nxGrid;
          iy = i / nxGrid;
          xx = xStart + ix * xInterval;
          yy = yStart + iy * yInterval;
          dx = -dxGrid[i];
          dy = -dyGrid[i];
        }
      } else {

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
            nread = sscanf(line, "%d %d %d %f, %f, %f", &ix, &iz, &iy, &dx, &dz, &dy);
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
      xmax = B3DMAX(xmax, xx + 1.);
      ymax = B3DMAX(ymax, yy + 1.);
      zmax = B3DMAX(zmax, iz + 1.);
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
    contBase += npatch;
  }
     
  if (residuals) {
    obj->symflags = IOBJ_SYMF_ARROW;
    obj->symbol = IOBJ_SYM_NONE;
    obj->symsize = 7;
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
  if (displayValues)
    obj->flags |= IMOD_OBJFLAG_USE_VALUE;
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

  B3DFREE(dxGrid);
  B3DFREE(dyGrid);
  return(mod);
     
}

/*
 *  point2model.c - Converts simple point file to model
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"
#include "mrcfiles.h"
#include "parse_params.h"

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  Imod *imod;
  Iobj *obj;
  Ipoint point;
  Istore store;
  FILE *infp;
  char line[1024];
  int open = 0, zsort = 0, scat = 0, numPerCont = 0, fromZero = 0;
  int err, nvals, nread, ob, co, lineNum, after, needcont, i, numOffset, linelen;
  int numPts = 0, numConts = 0, numObjs = 0;
  int sphere = 0, circle = 0;
  float tst1, tst2, xx, yy, zz, value;
  int numColors = 0;
  int numNames = 0;
  int hasValues = 0;
  int contValues = 0;
  int *red, *green, *blue;
  int directScale = 0;
  float xscale = 1., yscale = 1., zscale = 1., xtrans = 0., ytrans = 0., ztrans = 0.;
  char **names = NULL;

  char *progname = imodProgName(argv[0]);
  char *filename, *imagename;
  MrcHeader hdata;
  IrefImage *ref;
  FILE *fpimage = NULL;
  char *errString;
  int numOptArgs, numNonOptArgs;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 point2model  */
  int numOptions = 15;
  const char *options[] = {
    "input:InputFile:FN:", "output:OutputFile:FN:", "open:OpenContours:B:",
    "scat:ScatteredPoints:B:", "number:PointsPerContour:I:", "planar:PlanarContours:B:",
    "zero:NumberedFromZero:B:", "values:ValuesInLastColumn:I:", "circle:CircleSize:I:",
    "sphere:SphereRadius:I:", "color:ColorOfObject:ITM:", "name:NameOfObject:CHM:",
    "image:ImageForCoordinates:FN:", "pixel:PixelSpacingOfImage:FT:",
    "origin:OriginOfImage:FT:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        2, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input and output files */
  if (PipGetInOutFile("InputFile", 0, &filename))
      exitError("No input file specified");
  infp = fopen(filename, "r");
  if (!infp)
    exitError("Error opening input file %s", filename);
  free(filename);
  if (PipGetInOutFile("OutputFile", 1, &filename))
      exitError("No output file specified");

  if (!PipGetString("ImageForCoordinates", &imagename)) {
    fpimage = fopen(imagename, "rb");
    if (!fpimage)
      exitError("Could not open image file for coordinates: %s", imagename);
    if (mrc_head_read(fpimage, &hdata))
      exitError("Reading header from %s", imagename);
    free(imagename);
  }

  directScale = 2 - PipGetThreeFloats("PixelSpacingOfImage", &xscale, &yscale, &zscale)
    - PipGetThreeFloats("OriginOfImage", &xtrans, &ytrans, &ztrans);
  if (directScale && fpimage)
    exitError("You cannot use -image together with -pixel or -origin");

  err = PipGetInteger("PointsPerContour", &numPerCont);
  err = PipGetInteger("SphereRadius", &sphere);
  err = PipGetInteger("CircleSize", &circle);
  err = PipGetBoolean("OpenContours", &open);
  err = PipGetBoolean("ScatteredPoints", &scat);
  err = PipGetBoolean("PlanarContours", &zsort);
  err = PipGetInteger("ValuesInLastColumn", &hasValues);
  B3DCLAMP(hasValues, -1, 1);
  if (hasValues < 0) {
    hasValues = 1;
    contValues = 1;
  }
  err = PipGetBoolean("NumberedFromZero", &fromZero);
  numOffset = 1 - fromZero;
  if (numPerCont < 0) 
    exitError("Number of points per contour must be positive or zero");
  if (open + scat > 1)
    exitError("Only one of -open or -scat may be entered");

  // Get colors
  err = PipNumberOfEntries("ColorOfObject", &numColors);
  if (numColors) {
    red = (int *)malloc(numColors * sizeof(int));
    green = (int *)malloc(numColors * sizeof(int));
    blue = (int *)malloc(numColors * sizeof(int));
    if (!red || !green || !blue)
      exitError("Allocating memory for colors");
    for (co = 0; co < numColors; co++)
      err = PipGetThreeIntegers("ColorOfObject", &red[co], &green[co], 
                                &blue[co]);
  }

  // Get names
  err = PipNumberOfEntries("NameOfObject", &numNames);
  if (numNames) {
    names = (char **)malloc(numNames * sizeof(char *));
    if (!names)
      exitError("Allocating memory for names");
    for (co = 0; co < numNames; co++)
      err = PipGetString("NameOfObject", &names[co]);
  }

  PipDone();

  // Read first line of file, figure out how many values
  if (fgetline(infp, line, 1024) <= 0)
    exitError("Reading beginning of file");
      
  nvals = sscanf(line, "%f %f %f %f %f %f", &tst1, &tst2, &xx, &yy, &zz, &value);
  nvals -= hasValues;
  if (nvals < 3)
    exitError("There must be at least %d values per line", 3 + hasValues);
  nvals = B3DMIN(nvals, 5);
  if (numPerCont && nvals > 3) {
    exitError("The point file has contour numbers and the -number option "
              "cannot be used"); 
  }
  if (zsort && nvals > 3)
    exitError("The point file has contour numbers and the -planar option "
              "cannot be used"); 

  rewind(infp);
  
  imod = imodNew();
  if (!imod)
    exitError("Failed to get model structure");

  // Set the image reference scaling
  if (fpimage) {
    imodSetRefImage(imod, &hdata);
    fclose(fpimage);
  } else if (directScale) {
    imod->refImage = (IrefImage *)malloc(sizeof(IrefImage));
    if (!imod->refImage)
      exitError("Allocating IrefImage structure");
    ref = imod->refImage;
    ref->ctrans.x = xtrans;
    ref->ctrans.y = ytrans;
    ref->ctrans.z = ztrans;
    ref->cscale.x = xscale;
    ref->cscale.y = yscale;
    ref->cscale.z = zscale;
    ref->oscale.x = ref->oscale.y = ref->oscale.z = 1.;
    ref->orot.x = ref->orot.y = ref->orot.z = 0.;
    ref->crot.x = ref->crot.y = ref->crot.z = 0.;
    ref->otrans.x = ref->otrans.y = ref->otrans.z = 0.;
  }

  ob = 0;
  co = 0;
  lineNum = 0;
  imod->xmax = 0.;
  imod->ymax = 0.;
  imod->zmax = 0.;
  store.type = GEN_STORE_VALUE1;
  store.flags = GEN_STORE_FLOAT << 2;

  // To do: error check contour and object #'s, and they are numbered from 1.
  while (1) {

    // get line, done on EOF, skip blank line
    linelen = fgetline(infp, line, 1024);
    if (linelen < 0)
      break;
    if (linelen == 0) 
      continue;
 
    if (nvals == 3) {
      nread = sscanf(line, "%f %f %f %f", &xx, &yy, &zz, &value);
    } else if (nvals == 4) {
      nread = sscanf(line, "%d %f %f %f %f", &co, &xx, &yy, &zz, &value);
      co -= numOffset;
    } else {
      nread = sscanf(line, "%d %d %f %f %f %f", &ob, &co, &xx, &yy, &zz, &value);
      co -= numOffset;
      ob -= numOffset;
    }
    lineNum++;

    // Skip line with no values
    if (nread <= 0)
      continue;
    if (B3DMIN(5 + hasValues, nread) != nvals + hasValues &&
        !(contValues && nread == nvals)) 
      exitError("Every line should have %d entries; line %d has %d",
                nvals + hasValues, lineNum, nread);

    if (ob < 0 || co < 0)
      exitError("Illegal object or contour number (object %d, contour %d at line %d", 
                ob + numOffset, co + numOffset, lineNum);

    // Add objects if needed to get to the current object
    if (ob >= imod->objsize) {
      for (i = imod->objsize; i <= ob; i++) {
        if (imodNewObject(imod))
          exitError("Failed to add object to model");
        if (open)
          imod->obj[i].flags |= IMOD_OBJFLAG_OPEN;
        if (scat)
          imod->obj[i].flags |= IMOD_OBJFLAG_SCAT | IMOD_OBJFLAG_OPEN;
        numObjs++;
        imod->obj[i].pdrawsize = B3DMAX(0, sphere);
        if (circle > 0) {
          imod->obj[i].symsize = circle;
          imod->obj[i].symbol = IOBJ_SYM_CIRCLE;
        }
        if (i < numColors) {
          imod->obj[i].red = red[i] / 255.;
          imod->obj[i].green = green[i] / 255.;
          imod->obj[i].blue = blue[i] / 255.;
        }
        if (i < numNames) {
          strncpy(imod->obj[i].name, names[i], IOBJ_STRSIZE - 1);
          imod->obj[i].name[IOBJ_STRSIZE - 1] = 0x00;
        }
      }
    }

    // Determine if a contour is needed: either the contour number is too high
    // or the number limit is reached or there is a change in Z
    needcont = 0;
    obj = &imod->obj[ob];
    if (co >= obj->contsize)
      needcont = 1;
    else if ((numPerCont && obj->cont[co].psize >= numPerCont) ||
             (zsort && B3DNINT(obj->cont[co].pts[0].z) != B3DNINT(zz))) {
      co++;
      needcont = 1;
    }

    if (needcont) {
      imodSetIndex(imod, ob, -1, -1);
      for (i = obj->contsize; i<= co; i++) {
        if (imodNewContour(imod))
          exitError("Failed to add contour to model");
        numConts++;
      }
    }

    point.x = xx;
    point.y = yy;
    point.z = zz;
    imod->xmax = B3DMAX(imod->xmax, B3DNINT(xx + 10.));
    imod->ymax = B3DMAX(imod->ymax, B3DNINT(yy + 10.));
    imod->zmax = B3DMAX(imod->zmax, B3DNINT(zz + 1.));
    if (!imodPointAppend(&obj->cont[co], &point))
      exitError("Failed to add point to contour");
    numPts++;

    // take care of value for contour or point, only add one per contour
    if (hasValues) {
      store.value.f = value;
      err = 0;
      if (contValues && istoreLookup(obj->store, co, &after) < 0) {
        if (nread < nvals + 1)
          exitError("The first point for a contour must have a value entry; line %d "
                    "has only %d entries", lineNum, nread);
        store.index.i = co;
        err = istoreInsert(&obj->store, &store);
      } else {
        store.index.i = obj->cont[co].psize - 1;
        err = istoreInsert(&obj->cont[co].store, &store);
      }
      if (err)
        exitError("Failed to add general value");
    }
  }

  // Get the object min/max values set up
  if (hasValues) {
    for (ob = 0; ob < imod->objsize; ob++) {
      if (imod->obj[ob].contsize && istoreFindAddMinMax1(&imod->obj[ob]))
        exitError("Adding min/max values to object");
    }
  }

  // Attach image file's size as the max values
  if (fpimage) {
    imod->xmax = hdata.nx;
    imod->ymax = hdata.ny;
    imod->zmax = hdata.nz;
  }

  fclose(infp);
  if (imodBackupFile(filename)) 
    printf("Warning: %s - Failed to make old version of %s be a backup file\n",
           progname, filename);

  imod->file = fopen(filename, "wb");
  if (!imod->file)
    exitError("Opening new model file %s", filename);
  if (imodWriteFile(imod))
    exitError("Writing model file %s", filename);
  free(filename);
  printf("Model created with %d objects, %d contours, %d points\n", numObjs,
         numConts, numPts);
  exit(0);
}

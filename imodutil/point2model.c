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
 * Log at end of file
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
  FILE *infp;
  char line[1024];
  int open = 0, zsort = 0, scat = 0, numPerCont = 0, fromZero = 0;
  int err, nvals, nread, ob, co, lineNum, needcont, i, numOffset, linelen;
  int numPts = 0, numConts = 0, numObjs = 0;
  int sphere = 0, circle = 0;
  float tst1, tst2, xx, yy, zz;
  int numColors = 0;
  int numNames = 0;
  int *red, *green, *blue;
  char **names = NULL;

  char *progname = imodProgName(argv[0]);
  char *filename, *imagename;
  MrcHeader hdata;
  FILE *fpimage = NULL;
  char *errString;
  int numOptArgs, numNonOptArgs;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 point2model  */
  int numOptions = 11;
  char *options[] = {
    "input:InputFile:FN:", "output:OutputFile:FN:", "open:OpenContours:B:",
    "scat:ScatteredPoints:B:", "number:PointsPerContour:B:",
    "planar:PlanarContours:B:", "zero:NumberedFromZero:B:",
    "circle:CircleSize:I:", "sphere:SphereRadius:I:",
    "color:ColorOfObject:ITM:", "image:ImageForCoordinates:FN:"};

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

  err = PipGetInteger("PointsPerContour", &numPerCont);
  err = PipGetInteger("SphereRadius", &sphere);
  err = PipGetInteger("CircleSize", &circle);
  err = PipGetBoolean("OpenContours", &open);
  err = PipGetBoolean("ScatteredPoints", &scat);
  err = PipGetBoolean("PlanarContours", &zsort);
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
      
  nvals = sscanf(line, "%f %f %f %f %f", &tst1, &tst2, &xx, &yy, &zz);
  if (nvals < 3)
    exitError("There must be at least 3 values per line");
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

  if (fpimage) {
    imodSetRefImage(imod, &hdata);
    fclose(fpimage);
  }

  ob = 0;
  co = 0;
  lineNum = 0;
  imod->xmax = 0.;
  imod->ymax = 0.;
  imod->zmax = 0.;

  // To do: error check contour and object #'s, and they are numbered from 1.
  while (1) {

    // get line, done on EOF, skip blank line
    linelen = fgetline(infp, line, 1024);
    if (linelen < 0)
      break;
    if (linelen == 0) 
      continue;
 
    if (nvals == 3)
      nread = sscanf(line, "%f %f %f", &xx, &yy, &zz);
    else if (nvals == 4) {
      nread = sscanf(line, "%d %f %f %f", &co, &xx, &yy, &zz);
      co -= numOffset;
    } else {
      nread = sscanf(line, "%d %d %f %f %f", &ob, &co, &xx, &yy, &zz);
      co -= numOffset;
      ob -= numOffset;
    }
    lineNum++;

    // Skip line with no values
    if (nread <= 0)
      continue;
    if (B3DMIN(5, nread) != nvals) 
      exitError("Every line should have %d entries; line %d has %d",
                nvals, lineNum, nread);

    if (ob < 0 || co < 0)
      exitError("Illegal object or contour number (object %d, contour %d at "
                "line %d", ob + numOffset, co + numOffset, lineNum);

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


/*

$Log$
Revision 3.5  2009/09/22 14:35:19  mast
Fixed warning statement

Revision 3.4  2009/09/21 18:34:07  mast
Fixed problem with reading stopping at a blank line, and added options for
setting color, sizes, and reference coordinates.

Revision 3.3  2009/02/16 06:38:40  mast
Fixed initialization of maxes

Revision 3.2  2008/01/28 19:42:12  mast
Switched from close to fclose

Revision 3.1  2007/10/18 22:17:10  mast
Added to package


*/

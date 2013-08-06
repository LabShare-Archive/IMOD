/*
 *  clonevolume.c - clone multiple copies of an input volume into an
 *                  existing volume one at specified points / orientations. 
 *
 *  Author: John Heumann   email: heumannj@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>

#include "b3dutil.h"
#include "imodel.h"
#include "mrcfiles.h"
#include "parse_params.h"

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

typedef struct bndBox3D {
  float xMin;
  float xMax;
  float yMin;
  float yMax;
  float zMin;
  float zMax;
} bndBox3D;

typedef Ipoint corners3D[8];

/* Forward declarations */
bndBox3D *boundingBoxOfTransformedCorners(
  corners3D corners, Imat *xform, bndBox3D *bbox);

bndBox3D *intersect(bndBox3D *bb1, bndBox3D *bb2, bndBox3D *bbOut);

int isInside(Ipoint *point, bndBox3D *box, float tol);

Ipoint *getCenter(bndBox3D *bbox, Ipoint *center);

bndBox3D *getBoundingBoxOfVolume(MrcHeader *header, bndBox3D *bbox);

Ipoint *getCornersOfVolume(MrcHeader *header, corners3D corners);

Imat **allocateMat3DArray(int nMats);

Imat **reallocateMat3DArray(Imat **oldArray, int oldNMats, int newNMats);

Imat **freeMat3DArray(Imat **array, int nMats);

float trilinearInterpolation(float **inVol, MrcHeader *inHeader, 
                             float x, float y, float z);

/* 
 * Main entry
 */
#define lineSz 1024
int main( int argc, char *argv[])
{
  FILE *coordFP = NULL, *inFP = NULL, *intoFP = NULL, *outFP = NULL;
  char line[lineSz];
  char msg[256];
  char *progname = imodProgName(argv[0]);
  char *inFile, *intoFile, *outFile, *coordFile, *listString;
  int iSlice, numOptArgs, numNonOptArgs, inMode, intoMode;
  int *contourList = NULL, numContours = 0;
  int nClones = 0, maxClones = 100, iClone, ix, iy;
  float alpha, xMin, xMax, yMin, yMax, zMin, zMax;
  Imat **xform = NULL;
  bndBox3D inBBox, intoBBox, *outBBox = NULL;
  MrcHeader inHeader, intoHeader, outHeader;
  corners3D inCorners;
  float **inVol = NULL, minGray = FLT_MAX, maxGray = -FLT_MIN, meanGray = 0.0;
  Islice *slice = NULL;
  Ipoint inCenter;

  int numOptions = 9;
  const char *options[] = {
    "at:AtPoints:FN:", "x:XRange:IP:", "y:YRange:IP:", "z:ZRange:IP:",
    "input:InputFile:FN:", "output:OutputFile:FN:",  "into:IntoFile:FN:",
    "contours:ContourNumbers:LI:", "alpha:AlphaTransparency:F:"};
  const char *UsageString =
    "Usage: clonevolume [options] -into target -at locations inputVol outputVol";
  /* Parse parameters */
  PipSetUsageString(UsageString);
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 4, 1, 
                        1, &numOptArgs, &numNonOptArgs, 
                        imodUsageHeader);
  if (PipGetInOutFile((char *)"InputFile", 0, &inFile))
    exitError("No input file specified");
  if (PipGetString("AtPoints", &coordFile))
    exitError("No location/orientation file specified");
  if (PipGetInOutFile("OutputFile", 1, &outFile))
    exitError("No into file specified");
  if (PipGetString("IntoFile", &intoFile))
    exitError("No output file specified");
  if (PipGetTwoFloats("XRange", &xMin, &xMax)) {
    xMin = 0.0F;
    xMax = FLT_MAX;
  }
  if (PipGetTwoFloats("YRange", &yMin, &yMax)) {
    yMin = 0.0F;
    yMax = FLT_MAX;
  }
  if (PipGetTwoFloats("ZRange", &zMin, &zMax)) {
    zMin = 0.0F;
    zMax = FLT_MAX;
  }
  if (!PipGetString("ContourNumbers", &listString)) {
    contourList = parselist(listString, &numContours);
    free(listString);
    if (!contourList)
      exitError("Bad entry in list of contour numbers");
  }
  if (PipGetFloat("AlphaTransparency", &alpha))
    alpha = 0.0;
  if (alpha < 0.0 || alpha > 1.0)
    exitError("Transparency must be between 0 and 1");
  PipDone();

  /* Open the csv location/orientation file and skip the header line */
  coordFP = fopen(coordFile, "r");
  if (!coordFP) {
    sprintf(msg, "Error opening location/orientation file:\n%s", coordFile);
    exitError(msg);
  }
  free(coordFile);
  if (fgets(line, lineSz, coordFP) == NULL) {
    sprintf(msg, "Error reading location/orientation file :\n%s", coordFile);
    exitError(msg);
  }

  /* Open the input file */
  if ((inFP = fopen(inFile, "rb")) == 0) {
    sprintf(msg, "Error opening input file :\n%s", inFile);
    exitError(msg);
  }
  if (mrc_head_read(inFP, &inHeader)) {
    sprintf(msg, "Error reading header of input file :\n%s", inFile);
    exitError(msg);
  }
  inMode = sliceModeIfReal(inHeader.mode);
  if (inMode < 0)
    exitError("Input Volume must be  byte, short integer, or real");

  /* Open the into file */
  if ((intoFP = fopen(intoFile, "rb")) == 0) {
    sprintf(msg, "Error opening into file :\n%s", intoFile);
    exitError(msg);
  }
  if (mrc_head_read(intoFP, &intoHeader)) {
    sprintf(msg, "Error reading header of into file :\n%s", intoFile);
    exitError(msg);
  }
  intoMode = sliceModeIfReal(intoHeader.mode);
  if (intoMode < 0)
    exitError("Into volume must be  byte, short integer, or real");

  /* Copy the output header from the into */
  outHeader = intoHeader;

  /* Open the output file */
  if ((outFP = fopen(outFile, "wb")) == 0) {
    sprintf(msg, "Error opening output file :\n%s", outFile);
    exitError(msg);
  }
   
  xform = allocateMat3DArray(maxClones);
  outBBox = (bndBox3D *)malloc(maxClones * sizeof(bndBox3D));
  if (!xform || !outBBox)
    exitError("Allocation failed: out of memory");
  
  /* Get corners, bounding box, and center of the input volume */
  getCornersOfVolume(&inHeader, inCorners);
  getBoundingBoxOfVolume(&inHeader, &inBBox);
  getCenter(&inBBox, &inCenter);

  /* Get the bounding box of the into volume */
  getBoundingBoxOfVolume(&intoHeader, &intoBBox);

  /* Loop over points in the coordinate file */
  while (fgets(line, lineSz, coordFP)) {
    int contour, contourOk, inRange;
    float x, y, z, xAngle, yAngle, zAngle;

    if (sscanf(line, "%d,%g,%g,%g,%g,%g,%g", &contour, &x, &y, &z, 
               &xAngle, &yAngle, &zAngle) != 7)
      exitError("Error parsing location/orientation file");
    contourOk = (numContours == 0 || 
                 numberInList(contour, contourList, numContours, 0));
    inRange = (x >= xMin && x <= xMax && y >= yMin && y <= yMax && 
               z >= zMin && z <= zMax);
    if (contourOk && inRange) {
      Ipoint offset;
      bndBox3D tmpBBox;

      /* Dynamically increase storage when needed */
      if (nClones == maxClones) {
        int oldMaxClones = maxClones;
        maxClones *= 2;
        outBBox = (bndBox3D *)realloc(
	  (void *)outBBox, maxClones * sizeof(bndBox3D));
        xform = reallocateMat3DArray(xform, oldMaxClones, maxClones);
      }

      /* Construct the transformation for this point */
      /* First just the rotation                     */
      imodMatId(xform[nClones]);
      imodMatRot(xform[nClones], zAngle, b3dZ);
      imodMatRot(xform[nClones], yAngle, b3dY);
      imodMatRot(xform[nClones], xAngle, b3dX);
      /* Add the translation, adjusting for the fact that we're using */
      /* corner rather than center relative reference coordinates.    */
      imodMatTransform(xform[nClones], &inCenter, &offset);
      offset.x = -offset.x + x;
      offset.y = -offset.y + y;
      offset.z = -offset.z + z;
      imodMatTrans(xform[nClones], &offset);

      /* Find the bounding box of the "input" transformed to "into" */
      boundingBoxOfTransformedCorners(inCorners, xform[nClones], &tmpBBox);
      intersect(&tmpBBox, &intoBBox, outBBox + nClones);

      /* Invert the transformation matrix for later use */
      xform[nClones] = imodMatInverse(xform[nClones]);
      nClones += 1;
    }
  }

  /* Read the input volume as floating point */
  inVol = (float **)malloc(inHeader.nz * sizeof(float *));
  if (!inVol)
    exitError("Allocation failed: out of memory");
  for (iSlice = 0; iSlice < inHeader.nz; iSlice++) {
    slice = sliceReadFloat(&inHeader, iSlice);
    if (!slice)
      exitError("Error reading input volume");
    inVol[iSlice] = slice->data.f;
    free(slice); /* This frees the slice but not the data pointed to */
  }
  fclose(inFP);

  slice = sliceCreate(intoHeader.nx, intoHeader.ny, intoMode);
  if (!slice)
    exitError("Allocation failed: out of memory");
  /* Process the "into" volume slice-by-slice */
  for (iSlice = 0; iSlice < intoHeader.nz; iSlice++) {
    mrc_read_slice(slice->data.b, intoFP, &intoHeader, iSlice, 'Z');
    if (!slice->data.b)
      exitError("Error reading into volume slice");
    /* Loop over all the clone locations */
    for (iClone = 0; iClone < nClones; iClone++) {
      /* If cloned volume intersects the current slice... */
      if (iSlice >= outBBox[iClone].zMin && iSlice <= outBBox[iClone].zMax) {
        Ival oldVal = {0, 0, 0}, newVal = {0, 0, 0};
        /* Process the points in the clone's bounding box */
        for (ix = (int)ceil(outBBox[iClone].xMin);
             ix <= (int)floor(outBBox[iClone].xMax); ix++) {
          for (iy = (int)ceil(outBBox[iClone].yMin); 
	       iy <= (int)floor(outBBox[iClone].yMax); iy++) {
            Ipoint inPt, outPt;
            outPt.x = (float)ix;
            outPt.y = (float)iy;
            outPt.z = (float)iSlice;
            /* Tranform output/into coords back to the input */
            imodMatTransform3D(xform[iClone], &outPt, &inPt);

            /* If in range, combine current value with interpolated input. */
            /* Tolerance of a little more than 1 is used for the in range  */
            /* test to ensure that both previous and next points in each   */
            /* dimension are within input vol for trilinear interpolation. */
            if (isInside(&inPt, &inBBox, 1.0001)) {
              float inVal;
              if (sliceGetVal(slice, ix, iy, oldVal))
                exitError("Error retrieving value from slice");
              inVal = trilinearInterpolation(
                inVol, &inHeader, inPt.x, inPt.y, inPt.z);
              newVal[0] = alpha * oldVal[0] + (1.0 - alpha) * inVal;
              if (slicePutVal(slice, ix, iy, newVal))
                exitError("Error setting value in slice");
            }
          }
        }
      }
    }
    /* Get and process slice pixel statistics */
    if (sliceMMM(slice))
      exitError("Slice contains no data");
    meanGray += slice->mean;
    maxGray = max(slice->max, maxGray);
    minGray = min(slice->min, minGray);
    printf("\rWriting slice %d...", iSlice);
    fflush(stdout);
    /* Write the output slice  */
    if (mrc_write_slice(slice->data.b, outFP, &outHeader, iSlice, 'Z') < 0)
      exitError("Error writing output slice");
  }  
  printf("\n");
  free(slice->data.b);
  free(slice);

  /* Update and write output header */
  outHeader.amin = minGray;
  outHeader.amax = maxGray;
  outHeader.amean = meanGray / outHeader.nz;
  mrc_head_label(&outHeader, "clonevolume: Copy in subvolumes");
  if (mrc_head_write(outFP, &outHeader)) 
    exitError("Error writing output file header");
  
  /* Clean up. For documentation only since we're about to exit */
  fclose(outFP);
  fclose(intoFP);
  for (iSlice = 0; iSlice < inHeader.nz; iSlice++) {
    free(inVol[iSlice]);
  }
  free(inVol);
  xform = freeMat3DArray(xform, maxClones);
  free(outBBox);

  printf("Finished!\n");
  exit(0);
}

/**********************************************************************/

bndBox3D *boundingBoxOfTransformedCorners(
 corners3D corners, Imat *xform, bndBox3D *bbox) {
   
  int i;
  corners3D newCorners;

  for (i = 0; i < 8; i++) 
    imodMatTransform3D(xform, &(corners[i]), &newCorners[i]);

  bbox->xMin = bbox->xMax = newCorners[0].x;
  bbox->yMin = bbox->yMax = newCorners[0].y;
  bbox->zMin = bbox->zMax = newCorners[0].z;

  for (i = 1; i < 8; i++) {
    bbox->xMin = min(bbox->xMin, newCorners[i].x);
    bbox->xMax = max(bbox->xMax, newCorners[i].x);
    bbox->yMin = min(bbox->yMin, newCorners[i].y);
    bbox->yMax = max(bbox->yMax, newCorners[i].y);
    bbox->zMin = min(bbox->zMin, newCorners[i].z);
    bbox->zMax = max(bbox->zMax, newCorners[i].z);
  }

  return bbox;
}

/**********************************************************************/

bndBox3D *intersect(bndBox3D *bb1, bndBox3D *bb2, bndBox3D *bbOut) 
{
  bbOut->xMin = max(bb1->xMin, bb2->xMin);
  bbOut->xMax = min(bb1->xMax, bb2->xMax);
  bbOut->yMin = max(bb1->yMin, bb2->yMin);
  bbOut->yMax = min(bb1->yMax, bb2->yMax);
  bbOut->zMin = max(bb1->zMin, bb2->zMin);
  bbOut->zMax = min(bb1->zMax, bb2->zMax);

  return bbOut;
}

/**********************************************************************/

int isInside(Ipoint *point, bndBox3D *box, float tol)
{
  return (point->x >= box->xMin + tol && point->x <= box->xMax - tol &&
          point->y >= box->yMin + tol && point->y <= box->yMax - tol &&
          point->z >= box->zMin + tol && point->z <= box->zMax - tol);
}

/**********************************************************************/

Ipoint *getCenter(bndBox3D *bbox, Ipoint *center)
{
  center->x = 0.5 * (bbox->xMin + bbox->xMax);
  center->y = 0.5 * (bbox->yMin + bbox->yMax);
  center->z = 0.5 * (bbox->zMin + bbox->zMax);

  return center;
}

/**********************************************************************/

bndBox3D *getBoundingBoxOfVolume(MrcHeader *hdr, bndBox3D *bbox)
{
  bbox->xMin = bbox->yMin = 0;
  bbox->zMin = -0.5;
  bbox->xMax = hdr->nx;
  bbox->yMax = hdr->ny;
  bbox->zMax = hdr->nz - 0.5;

  return bbox;
}

/**********************************************************************/

Ipoint *getCornersOfVolume(MrcHeader *hdr, corners3D corners)
{
  const float x1 = hdr->nx;
  const float y1 = hdr->ny;
  const float z1 = hdr->nz - 0.5;


  /* Construct 8 points at the corners of the given volume */
  corners[0].x = 0;  corners[0].y = 0;  corners[0].z = -0.5;
  corners[1].x = 0;  corners[1].y = 0;  corners[1].z = z1;
  corners[2].x = 0;  corners[2].y = y1; corners[2].z = -0.5;
  corners[3].x = 0;  corners[3].y = y1; corners[3].z = z1;
  corners[4].x = x1; corners[4].y = 0;  corners[4].z = -0.5;
  corners[5].x = x1; corners[5].y = 0;  corners[5].z = z1;
  corners[6].x = x1; corners[6].y = y1; corners[6].z = -0.5;
  corners[7].x = x1; corners[7].y = y1; corners[7].z = z1;

  return (Ipoint *)corners;
}

/**********************************************************************/

Imat **allocateMat3DArray(int nMats)
{
  Imat **newArray = (Imat **)calloc(nMats, sizeof(void *));
  int i;

  if (!newArray)
    exitError("Allocation failed: out of memory");
  for (i = 0; i < nMats; i++) {
    newArray[i] = imodMatNew(3);
    if (!newArray[i])
      exitError("Allocation failed: out of memory");
  }
  
  return newArray;
}

/**********************************************************************/

Imat **reallocateMat3DArray(Imat **oldArray, int oldNMats, int newNMats) 
{
  Imat **newArray;
  int i;

  if (newNMats == oldNMats)
    return oldArray;

  for (i = oldNMats; i >= newNMats; i--)
    imodMatDelete(oldArray[i]);

  if (newNMats < oldNMats) {
    newArray = (Imat **)realloc(oldArray, newNMats * sizeof(void *));
    if (!newArray)
      exitError("Reallocation failed");
  }
  else {
    newArray = (Imat **)calloc(newNMats, sizeof(void *));
    memcpy(newArray, oldArray, oldNMats * sizeof(void *));
    for (i = oldNMats; i < newNMats; i++) {
      newArray[i] = imodMatNew(3);
      if (!newArray[i])
        exitError("Allocation failed: out of memory");
    }
  }

  if (oldArray != newArray)
    free(oldArray);

  return newArray;
}

/**********************************************************************/

Imat **freeMat3DArray(Imat **array, int nMats)
{
  int i;

  for (i = 0; i < nMats; i++)
    imodMatDelete(array[i]);
  free(array);

  return NULL;
}

/**********************************************************************/

float trilinearInterpolation(float **inVol, MrcHeader *inHeader, 
                             float x, float y, float z)
{
  float temp, newVal;

  const int ix = (int)x;
  const int iy = (int)y;
  int iz = (int)z;
  
  /* dx, dy, and dz will each be >= 0 and strictly less than 1 */
  const float dx = x - ix;
  const float dy = y - iy;
  const float dz = z - iz;

  /* Set up terms for up to tri-linear interpolation */
  const float d11 = (1.0 - dx) * (1.0 - dy);
  const float d12 = (1.0 - dx) * dy;
  const float d21 = dx * (1.0 - dy);
  const float d22 = dx * dy;

  const int ibase = ix + iy * inHeader->nx;
  newVal = d11 * inVol[iz][ibase];   /* d11 is always non-zero */
  if (d12 != 0)
    newVal += d12 * inVol[iz][ibase + inHeader->nx];
  if (d21 != 0)
    newVal += d21 * inVol[iz][ibase + 1];
  if (d22 != 0)
    newVal += d22 * inVol[iz++][ibase + inHeader->nx + 1];
  newVal *= (1.0 - dz);              /* 1 - dz is always non-zero */
  if (dz != 0) {
    temp = d11 * inVol[iz][ibase];   /* d11 is always non-zero */
    if (d12 != 0)
      temp += d12 * inVol[iz][ibase + inHeader->nx];
    if (d21 != 0)
      temp += d21 * inVol[iz][ibase + 1];
    if (d22 != 0)
      temp += d22 * inVol[iz][ibase + inHeader->nx + 1];
    newVal += dz * temp;
  }

  return newVal;
}

/**********************************************************************/

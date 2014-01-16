/*
 *  ctfphaseflip.cpp  -  CTF correction of tilted images
 *
 *  Author: Quanren Xiong
 *
 *  Copyright (C) 2007-2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <limits>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "b3dutil.h"
#include "iimage.h"
#include "sliceproc.h"
#include "ilist.h"
#include "ctfutils.h"
#include "cfft.h"
#include "parse_params.h"

#define MIN_ANGLE 1.0e-6  //tilt Angle less than this is treated as 0.0;
#define MY_PI 3.1415926
#define UNUSED_DEFOCUS -2000000.0
#define MAXLINE 160

using namespace std;

int main(int argc, char *argv[])
{
  int numOptArgs, numNonOptArgs;

  // Fallbacks from   ../manpages/autodoc2man 2 1 ctfphaseflip
  int numOptions = 18;
  const char *options[] = {
    "input:InputStack:FN:", "output:OutputFileName:FN:", "angleFn:AngleFile:FN:",
    "invert:InvertTiltAngles:B:", "defFn:DefocusFile:FN:", "xform:TransformFile:FN:",
    "defTol:DefocusTol:I:", "maxWidth:MaximumStripWidth:I:",
    "iWidth:InterpolationWidth:I:", "pixelSize:PixelSize:F:", "volt:Voltage:I:",
    "cs:SphericalAberration:F:", "ampContrast:AmplitudeContrast:F:",
    "views:StartingEndingViews:IP:", "totalViews:TotalViews:IP:",
    "boundary:BoundaryInfoFile:FN:", "aAngle:AxisAngle:F:", "param:Parameter:PF:"};

  char *stackFn, *angleFn, *outFn, *defFn, *xformFn = NULL;
  char *boundFn = NULL;
  int volt, iWidth, defocusTol, ii, ierr;
  float pixelSize, cs, ampContrast, stripDefocus;
  int startingView, endingView, startingTotal, endingTotal, defVersion;
  bool isSingleRun = false;
  int invertAngles = 0;
  int maxStripWidth = 256;
  double angleSign;
  float minAngle, maxAngle;
  float *tiltAngles = NULL;
  float *xShifts = NULL;
  Ilist *defocusList;
  char line[MAXLINE];
  char *progname = imodProgName(argv[0]);

  PipReadOrParseOptions(argc, argv, options, numOptions, progname,
                        1, 0, 0, &numOptArgs, &numNonOptArgs, NULL);

  if (PipGetString("InputStack", &stackFn))
    exitError("No stack specified");
  if (PipGetString("AngleFile", &angleFn)) {
    angleFn = NULL;
    printf("No angle file is specified, tilt angle is assumed to be 0.0\n");
  }
  if (PipGetString("DefocusFile", &defFn))
    exitError("No defocus file is specified");
  //if (PipGetFloat("AxisAngle", &tiltAxisAngle) )
  //  exitError("No AxisAngle specified");
  if (PipGetInteger("DefocusTol", &defocusTol))
    exitError("No DefocusTol specified");
  if (PipGetInteger("InterpolationWidth", &iWidth))
    exitError("No interpolationWidth specified");
  if (PipGetFloat("PixelSize", &pixelSize))
    exitError("No PixelSize specified");
  if (PipGetInteger("Voltage", &volt))
    exitError("Voltage is not specified");
  if (PipGetFloat("SphericalAberration", &cs))
    exitError("SphericalAberration is not specified");
  cs = B3DMAX(0.01, cs);
  if (PipGetFloat("AmplitudeContrast", &ampContrast))
    exitError("No AmplitudeContrast is specified");
  if (PipGetTwoIntegers("TotalViews", &startingTotal, &endingTotal))
    isSingleRun = true; // TotalViews is not specified;
  if (PipGetString("OutputFileName", &outFn))
    exitError("OutputFileName is not specified");
  PipGetString("BoundaryInfoFile", &boundFn);
  PipGetString("TransformFile", &xformFn);
  PipGetInteger("MaximumStripWidth", &maxStripWidth);
  PipGetBoolean("InvertTiltAngles", &invertAngles);
  angleSign = invertAngles ? -1. : 1.;

  printf("stackFn = %s, angleFn=%s,  invertAngles=%d\n", stackFn, angleFn, invertAngles);
  printf("volt=%d Kv, interpolationWidth=%d pixels, defocusTol=%d nm \n",
         volt, iWidth, defocusTol);
  printf("pixelSize=%f nm, cs=%f mm, ampContrast=%f \n", pixelSize, cs, ampContrast);

  FILE *fpStack;
  if ((fpStack = iiFOpen(stackFn, "rb")) == 0)
    exitError("could not open input file %s", stackFn);
  defocusList = readDefocusFile(defFn, defVersion);
  if (!ilistSize(defocusList))
    exitError("The defocus file %s is non-existent or empty - did you save in ctfplotter?"
              , defFn);

  FILE *foutput;

  MrcHeader header;
  MrcHeader outHeader;
  int sliceMode;
  /* read header */
  if (mrc_head_read(fpStack, &header))
    exitError("reading header of input file %s",  stackFn);

  if (mrc_head_read(fpStack, &outHeader))
    exitError("reading header of input file %s",  stackFn);

  xShifts = B3DMALLOC(float, header.nz);
  if (!xShifts)
    exitError("Allocating array for X shifts");
  for (ii = 0; ii < header.nz; ii++)
    xShifts[ii] = 0.;

  if (xformFn) {
    float dum1, dum2, dum3, dum4;
    FILE *fpXF = fopen(xformFn, "r");
    if (!fpXF)
      exitError("Opening transform file %s", xformFn);
    for (ii = 0; ii < header.nz; ii++) {
      ierr = fgetline(fpXF, line, MAXLINE);
      if (ierr == -1)
        exitError("Reading transform file %s", xformFn);
      if (ierr == -2)
        exitError("End of file after reading %d transforms; not enough transforms in "
                  "file", ii);
      ierr = sscanf(line, "%f %f %f %f %f", &dum1, &dum2, &dum3, &dum4, &xShifts[ii]);
      if (ierr <= 0) {
        ii--;
        continue;
      }
    }
  }

  startingView = 1;
  endingView = header.nz;
  PipGetTwoIntegers("StartingEndingViews", &startingView, &endingView);
  sliceMode = sliceModeIfReal(header.mode);
  if (sliceMode < 0)
    exitError("File mode is %d; only byte, short integer, or real allowed", header.mode);

  //The number of slices this run deals with;
  int currNz = endingView - startingView + 1;
  if (isSingleRun) {
    outHeader.nz = currNz;
    outHeader.mz = currNz;
  } else {
    outHeader.nz = endingTotal - startingTotal + 1;
    outHeader.mz = endingTotal - startingTotal + 1;
  }
  outHeader.zlen = header.zlen * outHeader.nz / header.nz;
  mrcInitOutputHeader(&outHeader);
  mrc_head_label(&outHeader, "ctfPhaseFlip: CTF correction "
                 "with phase flipping only");

  if ((startingView == -1 && endingView == -1) || isSingleRun) {
    if (!isSingleRun && b3dOutputFileType() == IIFILE_TIFF)
      exitError("Cannot do parallel writing to a TIFF output file");
    imodBackupFile(outFn);
    foutput = iiFOpen(outFn, "wb");
  } else
    foutput = iiFOpen(outFn, "r+b");
  if (!foutput)
    exitError("fopen() failed to open %s", outFn);

  if (startingView == -1 && endingView == -1 && !isSingleRun) {
    if (mrc_head_write(foutput, &outHeader))
      exitError("Error when write out header");
    iiFClose(fpStack);
    iiFClose(foutput);
    return 0;
  }


  int err = parWrtInitialize(boundFn, header.nx, header.ny);
  if (err)
    exitError("Initializing parallel writing with boundary info file %s "
              "(error %d)", boundFn, err);

  int nx = header.nx;
  int nyfile = header.ny;
  int nz = header.nz;
  float currAngle;
  Islice *currSlice;
  int stripPixelNum, interPixelNum;
  int k, row, column, i;
  int stripIdx, fy, fyy,  fx, ny, yoff;
  int dir = 0;
  int idir = 1;
  float WL, C1, C2, f2, ctf, freq_scalex, freq_scaley;
  float waveAberration;

  //Get the tilt angles and detected defocus for each slice;
  SavedDefocus *item;
  float *defocus = (float *)malloc(nz * sizeof(float));
  if (!defocus)
    exitError("Allocating memory for defocus array");

  if (angleFn)
    tiltAngles = readTiltAngles(angleFn, nz, angleSign, minAngle, maxAngle);

  // Check the defocus list if there is more than one value
  if (ilistSize(defocusList) > 1 && 
      checkAndFixDefocusList(defocusList, tiltAngles, nz, defVersion))
    printf("WARNING: ctfphaseflip - View numbers in defocus file are not all "
           "consistent with the angular ranges\n");

  //sets to UNUSED_DEFOCUS since ctfplotter saves defocus with that value
  // when defocus is not computed.
  for (k = 0; k < nz; k++)
    defocus[k] = UNUSED_DEFOCUS;

  // Process the defocus values; slice numbers are now numbered from 0
  for (i = 0; i < ilistSize(defocusList); i++) {
    item = (SavedDefocus *)ilistItem(defocusList, i);
    k = (item->startingSlice + item->endingSlice) / 2;
    if (k < 0 || k >= nz)
      exitError("View numbers in defocus file are out of range");

    // They are already in microns
    defocus[k] = item->defocus;
    //printf("beginNum=%d endNum=%d k=%d defocus=%f\n", beginNum, endNum, k,
    //defocus[k]);
  }

  //defocus interpolation;
  int first = -1, second = 0;
  for (k = 0; k < nz; k++) {
    if (defocus[k] == UNUSED_DEFOCUS)
      continue;
    second = k;
    if (first == -1) {
      for (row = 0; row < second; row++)
        defocus[row] = defocus[second];
    } else {
      for (row = first + 1; row < second; row++)
        defocus[row] = ((row - first) * defocus[second] + (second - row) *
                        defocus[first]) / (float)(second - first);
    }
    first = second;
  }

  for (k = nz - 1; k >= 0; k--)
    if (defocus[k] == UNUSED_DEFOCUS)
      defocus[k] = defocus[second];
    else
      break;

  for (k = 0; k < nz; k++)
    printf("defocus[%d] = %f microns\n", k , defocus[k]);

  WL = 12.41 / sqrt(volt * (volt + 1022.0)); //wavelength;
  C1 = MY_PI * WL;
  C2 = -C1 * cs * 1000000.0 * WL * WL / 2.0;


  int stripDist[2];
  float *restoredArray;
  double meanSum = 0.0;
  double amin = 0.1 * numeric_limits<double>::max();
  double amax = -amin;

  Islice *outSlice;

  // Pad the extent in Y if necessary
  ny = niceFrame(nyfile, 2, 19);
  yoff = (ny - nyfile) / 2;
  int currK = 0;
  if (!isSingleRun)
    currK = startingView - startingTotal;
  fflush(stdout);

  for (k = startingView; k <= endingView; k++) {
    if (tiltAngles) {
      currAngle = tiltAngles[k - 1];
      printf("Slice %d, tilt angle is %f degrees. \n", k, currAngle);
    } else {
      currAngle = 0.0;
      printf("No angle is specified, set to 0.0\n");
    }

    if (defocus[k - 1] == UNUSED_DEFOCUS)
      exitError("specified defocus is wrong for slice %d", k);

    currSlice = sliceCreate(nx, nyfile, sliceMode);
    outSlice = sliceCreate(nx, nyfile, SLICE_MODE_FLOAT);
    if (!currSlice || !outSlice)
      exitError("creating outslice or currSlice");
    restoredArray = outSlice->data.f;

    //startingView starts at 1, the API starts 0;
    if (mrc_read_slice(currSlice->data.b, fpStack, &header, k - 1, 'Z'))
      exitError("reading slice");
    printf("Slice %d of stack %s is included\n", k, stackFn);

    //convert slice to floats
    if (sliceMode != SLICE_MODE_FLOAT)
      if (sliceNewMode(currSlice, SLICE_MODE_FLOAT) < 0)
        exitError("converting slice to float");

    currAngle = currAngle * MY_PI / 180.0;
    if (fabs(currAngle) > MIN_ANGLE) {
      stripPixelNum = fabs(defocusTol / tan(currAngle)) / pixelSize;
    } else {
      stripPixelNum = nx;
    }
    if (stripPixelNum > nx)
      stripPixelNum = nx;

    stripPixelNum = niceFrame(stripPixelNum, 2 , 19);
    B3DCLAMP(stripPixelNum, 128, maxStripWidth);

    interPixelNum = iWidth;

    //interPixelNum must be less than stripPixelNum/2;
    if (interPixelNum >= stripPixelNum / 2)
      exitError("interPixelNum is bigger than stripPixleNum/2 ");

    printf("stripPixelNum=%d interPixelNum=%d \n", stripPixelNum,
           interPixelNum);
    //Allocate 2 strips, even and odd strip;
    int stripXdim = stripPixelNum + 2;
    float *strip = (float *)malloc(2 * ny * stripXdim * sizeof(float));
    bool finished = false;
    float *curStrip, *lastStrip;
    int stripBegin;
    int stripEnd;
    int stripStride, halfStrip, stripMid;
    halfStrip = stripPixelNum / 2;

    // convert pixelSize to Angstroms from nm
    freq_scalex = 1.0 / (pixelSize * 10.0 * stripPixelNum);
    freq_scaley = 1.0 / (pixelSize * 10.0 * ny);
    stripIdx = 0;
    while (!finished) {

      if (stripIdx * interPixelNum + stripPixelNum - 1 < nx) {
        stripBegin = stripIdx * interPixelNum;
        stripEnd = stripBegin + stripPixelNum - 1;
        stripStride = interPixelNum;
      } else {
        stripStride = nx - (stripPixelNum + 1) / 2 - (stripBegin + stripEnd) / 2;
        stripBegin = nx - stripPixelNum;
        stripEnd = nx - 1;
        finished = true;
      }
      stripMid = (stripBegin + stripEnd) / 2;
      curStrip = strip + (stripIdx % 2) * ny * stripXdim;
      //printf("stripIdx=%d stripBegin=%d stripEnd=%d \n",
      //stripIdx, stripBegin, stripEnd);

      stripDefocus = defocus[k - 1] * 1000.0 - (nx / 2 + xShifts[k - 1] - stripMid) *
                     tan(currAngle) * pixelSize; //in nm;
      //printf("defocus is %6.1f\n", stripDefocus);

      sliceTaperInPad(currSlice->data.f, SLICE_MODE_FLOAT, nx, stripBegin,
                      stripEnd, 0, nyfile - 1, curStrip,
                      stripPixelNum + 2, stripPixelNum, ny, 9, 9);
      todfft(curStrip, &stripPixelNum, &ny, &dir);

      //fipping the phase;
      for (fy = 0; fy < ny; fy++) {
        fyy = fy;
        if (fy > ny / 2)
          fyy -= ny;
        for (fx = 0; fx < stripXdim / 2; fx++) {
          //convert defocus to Angstroms
          f2 = fx * fx * freq_scalex * freq_scalex + fyy * fyy * 
            freq_scaley * freq_scaley;
          waveAberration = (C1 * stripDefocus * 10.0 + C2 * f2) * f2;
          ctf = -(sqrt(1 - ampContrast * ampContrast)) * sin(waveAberration)
                - ampContrast * cos(waveAberration);
          if (ctf >= 0) {
            curStrip[fy * stripXdim + 2 * fx] *= -1.;
            curStrip[fy * stripXdim + 2 * fx + 1] *= -1.;
          }
        }
      }
      //inverse FFT;
      todfft(curStrip, &stripPixelNum, &ny, &idir);

      if (stripIdx == 0) { //The starting strip needs special handling;
        for (row = 0; row < nyfile; row++)
          for (column = 0; column < halfStrip; column++)
            restoredArray[row * nx + column] =
              curStrip[(row + yoff) * stripXdim + column];
        //printf("column=1 ... %d \n", halfStrip);
      } else {
        for (row = 0; row < nyfile; row++)
          for (column = stripMid - stripStride + 1; column < stripMid + 1; column++) {
            stripDist[0] = column - stripMid + stripStride - 1;
            stripDist[1] = stripMid + 1 - column;
            restoredArray[row * nx + column] =
              (stripDist[0] * curStrip[(row + yoff) * stripXdim +
                                       halfStrip - stripDist[1]] +
               stripDist[1] * lastStrip[(row + yoff) * stripXdim +
                                        halfStrip + stripDist[0]])
              / (float)stripStride;
          }
        // printf("column=%d ... %d \n", stripMid-
        // stripStride+1+1, stripMid+1 );
      }

      if (finished) { //last strip
        //printf("finished: starting column=%d \n", stripMid+1 );

        for (row = 0; row < nyfile; row++)
          for (column = stripMid + 1; column < nx; column++)
            restoredArray[row * nx + column]  =
              curStrip[(row + yoff) * stripXdim + halfStrip + column - stripMid - 1];
        //printf("column=%d ... %d \n", stripMid+1+1, nx);
      }

      //printf("stripIdx=%d\n", stripIdx);
      stripIdx++;
      lastStrip = curStrip;
    }//while strip loop

    free(strip);
    sliceMMM(outSlice);
    if (outSlice->min < amin)
      amin = outSlice->min;
    if (outSlice->max > amax)
      amax = outSlice->max;
    meanSum += outSlice->mean;
    if (sliceMode != SLICE_MODE_FLOAT)
      if (sliceNewMode(outSlice, sliceMode) < 0)
        exitError("converting slice to original mode");

    if (parallelWriteSlice(outSlice->data.b, foutput, &outHeader, currK))
      exitError("Writing slice %d error", currK);
    currK++;
    sliceFree(currSlice);
    sliceFree(outSlice);
    fflush(stdout);
  }//k slice

  if (isSingleRun) {
    outHeader.amin = amin;
    outHeader.amax = amax;
    outHeader.amean = meanSum / (double)currNz;
    if (mrc_head_write(foutput, &outHeader))
      exitError("Writing slice header error");
  } else {//for collectmmm
    printf("min, max, mean, # pixels= %f  %f  %f %d \n",
           amin, amax, meanSum / (double)currNz, nx * ny * currNz);
  }
  iiFClose(foutput);
  iiFClose(fpStack);
  free(defocus);
}

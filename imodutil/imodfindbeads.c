/*
 *  imodfindbeads.c -- Find gold particles in images
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "b3dutil.h"
#include "mrcfiles.h"
#include "mrcslice.h"
#include "sliceproc.h"
#include "parse_params.h"
#include "cfft.h"

// Structure to hold peaks
typedef struct peak_list_entry {
  float xcen, ycen;
  int iz;
  float ccc;
  float integral;
  float peak;
  float cenmean;
  float annmean;
  float median;
} PeakEntry;

// Some limits for arrays
#define MAX_BINS 10000
#define MAX_GROUPS 10
#define KERNEL_MAXSIZE 7
#define MAX_AREAS  1000

// Local functions
static int comparePeaks(const void *p1, const void *p2);
static Islice *readSliceAsFloat(FILE *infp, MrcHeader *inhead, int sliceMode,
                                int iz);
static float templateCCCoefficient(float *array, int nxdim, int nx, int ny, 
                                   float *template, int nxtdim, int nxt, 
                                   int nyt, int xoffset, int yoffset);
static int pointInsideBoundary(Iobj *obj, Ipoint *pnt);
static void kernelHistoPL(PeakEntry *peakList, float *element, int numPeaks,
                            int skipZeroCCC, float *select, float selMin, 
                            float selMax, float *bins, int numBins,
                            float firstVal, float lastVal, float h,
                            int verbose);
static int findHistoDipPL(PeakEntry *peakList, int numPeaks, int minGuess,
                            float *kernHist, float *regHist, float *histDip,
                            float *peakBelow, float *peakAbove, char *vkeys);
static void printArray(float *filtBead, int nxdim, int nx, int ny);
static void selectedMinMax(PeakEntry *peakList, float *element, int numPeaks,
                           float *select, float selMin, float selMax,
                           float *minVal, float *maxVal, int *ninRange);
static int pointInsideArea(Iobj *obj, int *list, int nlist, float xcen, 
                           float ycen);
static void makeAreaContList(Iobj *obj, int iz, int *list, int *nlist);

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  char *filename;
  char *outModel;
  char *progname = imodProgName(argv[0]);
  int numOptArgs, numNonOptArgs;
  Imod *refmod = NULL;
  Imod *imod;
  Imod *areaMod = NULL;
  Iobj *obj;
  Icont *cont;
  Ipoint pnt;
  FILE *infp;
  FILE *outfp = NULL;
  MrcHeader inhead, outhead;
  float *filtSlice, *corrSlice, *fullBead, *oneBead, *filtBead, *splitBead;
  float *writeSlice;
  Islice *sl, *sclsl;
  PeakEntry *peakList;
  Istore store;
  int *listStart;
  int *zlist;
  int boundObj = -1;
  int numGuess = 0;
  float threshold = -2.;
  float peakThresh = 0.;
  float center = 2.;
  int lightBeads = 0;
  int linear = 0;
  int measureToUse = 1;
  int remakeModelBead = 0;
  float annulusPctile = -1.;
  float minRelativePeak = 0.1f;
  float minSpacing = 1.;
  int forward = 0, inverse = 1;
  float scaledSize = 8.;
  float beadSize, scaleFactor, xOffset, yOffset, xscale, yscale;
  float amat[2][2];
  float regHist[MAX_BINS], kernHist[MAX_BINS];
  float ctf[8193];
  int boxSize, boxScaled;
  int numRefPts = 0, numPeakPts, numPeakMatch, numPeaksLeft, numBelowMin;
  int numMatched = 0, numUnmatched = 0;
  float minInterp = 1.4f;
  int minInGroup = 100;
  float kernelSigma = 0.85;
  float annUseMin, annUseMax, maxBalRange = 4.;
  float sigma1 = 0., sigma2 = 0., radius1 = 0., radius2 = 0.;
  int izst, iznd, nytmp, nxpad, nypad, nxpdim, listSize, numPeaks, npass;
  int nxin, nyin, nxout, nyout, nzout, diff, mindiff, range, size, nxtmp;
  float rCenter, rInner, rOuter, zscale, xtmp, ytmp, xBeadOfs, yBeadOfs;
  float beadCenOfs, val, tsum, cval, cx, cy, xcen, ycen, integral, ccc;
  int ipass, numEliminated, numStart, iz, iy, ix, i, ixst, ixnd, iyst, iynd;
  int ind, ixofs, iyofs, j, jdir, jstr, jend, nsum, loaded, sliceMode, minsize;
  float minDist, critsq, dx, dy, distsq, cccMin, cccMax, matchCrit, peakMax;
  float peakAbove, peakBelow, error, lastPeak, errMin, histDip, meanAbove;
  float sdAbove, dxbin, ctfDelta, cenmean, annmean, median;
  int numToSave, lowerInd, ninHist, ninSel, igr, ndat, verbose;
  int maxSec, numRuns, numZperRun, runsAddingOne, irun, indz;
  float cumStart, target, cumul, annMin, annMax, lowerLim, upperLim, threshUse;
  float selPeakMin, selPeakMax, selSlope, selIntcp, modeSlope, modeIntcp;
  float selectPctile = 0.90;
  float annMidval[MAX_GROUPS], annPctPeak[MAX_GROUPS], annDip[MAX_GROUPS];
  float annPeakAbove[MAX_GROUPS], dip, kernel[KERNEL_MAXSIZE * KERNEL_MAXSIZE];
  int numGroups = 4;
  int areaConts[MAX_AREAS];
  int numAreaCont, numObjOrig;
  double sum, sumsq;
  char *vkeys = NULL;
  float *meanMedPtr;
  int ob, co, pt;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 imodfindbeads  */
  int numOptions = 32;
  char *options[] = {
    "input:InputImageFile:FN:", "output:OutputModelFile:FN:",
    "filtered:FilteredImageFile:FN:", "area:AreaModel:FN:",
    "add:AddToModel:FN:", "ref:ReferenceModel:FN:",
    "boundary:BoundaryObject:I:", "size:BeadSize:F:", "light:LightBeads:B:",
    "scaled:ScaledSize:F:", "interpmin:MinInterpolationFactor:F:",
    "linear:LinearInterpolation:B:", "center:CenterWeight:F:",
    "box:BoxSizeScaled:I:", "threshold:ThresholdForAveraging:F:",
    "store:StorageThreshold:F:", "bkgd:BackgroundGroups:F:",
    "annulus:AnnulusPercentile:F:", "peakmin:MinRelativeStrength:F:",
    "spacing:MinSpacing:F:", "sections:SectionsToDo:LI:",
    "maxsec:MaxSectionsPerAnalysis:I:", "remake:RemakeModelBead:B:",
    "guess:MinGuessNumBeads:I:", "measure:MeasureToUse:I:",
    "kernel:KernelSigma:F:", "rad1:FilterRadius1:F:",
    "rad2:FilterRadius2:F:", "sig1:FilterSigma1:F:", "sig2:FilterSigma2:F:",
    "verbose:VerboseKeys:CH:", "param:ParameterFile:PF:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        3, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input and output files */
  if (PipGetInOutFile("InputImageFile", 0, &filename))
      exitError("No input image file specified");
  infp = fopen(filename, "rb");
  if (!infp)
    exitError("Opening input image file %s", filename);
  free(filename);
  if (mrc_head_read(infp, &inhead))
    exitError("Reading header of image file %s", filename);

  // Check if it is the correct data type and set slice type
  sliceMode = sliceModeIfReal(inhead.mode);
  if (sliceMode < 0)
    exitError("File mode is %d; only byte, short, float allowed", 
              inhead.mode);

  if (PipGetInOutFile("OutputModelFile", 1, &outModel))
    exitError("No output model file specified");

  // Read reference model
  if (!PipGetString("ReferenceModel", &filename)) {
    refmod = imodRead(filename);
    if (!refmod)
      exitError("Reading reference model %s", filename);
    free(filename);

    if (!PipGetInteger("BoundaryObject", &boundObj)) {
      if (boundObj < 1 || boundObj > refmod->objsize)
        exitError("Boundary object number %d is out of bounds (model has %d "
                  "objects)", boundObj, refmod->objsize);
    }
  }

  // Read area model
  if (!PipGetString("AreaModel", &filename)) {
    areaMod = imodRead(filename);
    if (!areaMod)
      exitError("Reading area model %s", filename);
    free(filename);
    if (!areaMod->objsize || !areaMod->obj[0].contsize)
      exitError("No contours in object 1 of area model");
  }

  // Read existing model
  if (!PipGetString("AddToModel", &filename)) {
    imod = imodRead(filename);
    if (!imod)
      exitError("Reading model to append to: %s", filename);
    free(filename);
  } else {

    // Or create a model
    imod = imodNew();
    if (!imod)
      exitError("Creating output model");
  }
  numObjOrig  = imod->objsize;

  // Get other parameters
  PipGetFloat("ScaledSize", &scaledSize);
    // exitError("You must enter a scaled size for the filtering");
  if (PipGetFloat("BeadSize", &beadSize))
    exitError("You must enter a bead size");
  PipGetBoolean("LightBeads", &lightBeads);
  PipGetBoolean("LinearInterpolation", &linear);
  PipGetBoolean("RemakeModelBead", &remakeModelBead);
  PipGetFloat("ThresholdForAveraging", &threshold);
  PipGetFloat("StorageThreshold", &peakThresh);
  PipGetFloat("CenterWeight", &center);
  PipGetInteger("BackgroundGroups", &numGroups);
  PipGetFloat("MinInterpolationFactor", &minInterp);
  PipGetFloat("MinSpacing", &minSpacing);
  PipGetInteger("MinGuessNumBeads", &numGuess);
  PipGetInteger("MeasureToUse", &measureToUse);
  PipGetFloat("AnnulusPercentile", &annulusPctile);
  PipGetFloat("KernelSigma", &kernelSigma);
  PipGetFloat("FilterRadius1", &radius1);
  PipGetFloat("FilterRadius2", &radius2);
  PipGetFloat("FilterSigma1", &sigma1);
  PipGetFloat("FilterSigma2", &sigma2);
  PipGetString("VerboseKeys", &vkeys);
  numGroups = B3DMIN(MAX_GROUPS, numGroups);
  if (lightBeads && annulusPctile >= 0.)
    annulusPctile = 1. - annulusPctile;
  median = 0.;

  // If doing automatic thresholds and no peak limit entered, drop it to
  // allow more histogram to be built
  if (PipGetFloat("MinRelativeStrength", &minRelativePeak) && threshold < 0.)
    minRelativePeak /= 2.;

  // Get radii for the integral
  rCenter = B3DMAX(1., 0.34 * beadSize);
  rInner = B3DMAX(rCenter + 1., 0.5 * beadSize + 1.);
  rOuter = rInner + 2.;
  minDist = minSpacing * beadSize;
  matchCrit = B3DMAX(0.2 * beadSize, 2.);

  // Make default box size and list of sections to do
  boxScaled = 3 * scaledSize + 2;
  PipGetInteger("BoxSizeScaled", &boxScaled);
  if (!PipGetString("SectionsToDo", &filename)) {
    zlist = parselist(filename, &nzout);
    free(filename);
  } else {
    nzout = inhead.nz;
    zlist = (int *)malloc(nzout * sizeof(int));
    if (zlist)
      for (i = 0; i < nzout; i++)
        zlist[i] = i;
  }
  if (!zlist)
    exitError("Failed to get memory for list of sections");
  for (i = 0; i < nzout; i++)
    if (zlist[i] < 0 || zlist[i] >= inhead.nz)
      exitError("Section # %d is out of range", zlist[i]);

  // Figure out division into separate runs
  maxSec = nzout;
  PipGetInteger("MaxSectionsPerAnalysis", &maxSec);
  if (maxSec <= 0)
    exitError("Maximum number of sections per analysis must be positive");
  numRuns = (nzout + maxSec - 1) / maxSec;
  numZperRun = nzout / numRuns;
  runsAddingOne = nzout % numRuns;
  if (numRuns > 1 && refmod)
    exitError("All sections must be analyzed together when comparing with "
              "reference model");

  // Get size for scaled slices
  nxin = inhead.nx;
  nyin = inhead.ny;
  scaleFactor = beadSize / scaledSize;
  scaledSobel(NULL, nxin, nyin, scaleFactor, minInterp, linear, -1., NULL, 
              &nxout, &nyout, &xOffset, &yOffset);

  if (vkeys)
    printf("nxout %d  nyout %d xOffset %f yOffset %f scaleFactor %f\n", 
           nxout, nyout, xOffset, yOffset, scaleFactor);

  // Open file for filtered images
  if (!PipGetString("FilteredImageFile", &filename)) {
    if (imodBackupFile(filename))
      printf("WARNING: %s - Error renaming existing image file %s\n", progname,
             filename);
    outfp = fopen(filename, "wb");
    if (!outfp)
      exitError("Opening output image file %s", filename);
    free(filename);

    mrc_head_new(&outhead, nxout, nyout, nzout, MRC_MODE_FLOAT);
    mrc_head_label_cp(&inhead, &outhead);
    mrc_head_label(&outhead, "imodfindbeads: Scaled and Sobel filtered");
    xscale = yscale = zscale = 1.;
    if (inhead.mx && inhead.xlen)
      xscale = inhead.xlen / (float)inhead.mx;
    if (inhead.my && inhead.ylen)
      yscale = inhead.ylen / (float)inhead.my;
    if (inhead.mz && inhead.zlen)
      zscale = inhead.zlen / (float)inhead.mz;

    // Set scale and origin in new header to display the match input data
    outhead.xlen = nxout * xscale * scaleFactor;
    outhead.ylen = nyout * yscale * scaleFactor;
    outhead.zlen = nzout * zscale;
    outhead.xorg -= xOffset * xscale;
    outhead.yorg -= yOffset * yscale;
    outhead.zorg -= zlist[0] * zscale;
    outhead.amin = 1.e30;
    outhead.amax = -1.e30;
    outhead.amean = 0.;
  }

  imodBackupFile(outModel);
  imod->file = fopen(outModel, "wb");
  if (!imod->file)
    exitError("Opening output model %s", outModel);
  free(outModel);

  // Get the full box size, find the size that best matches the specified 
  // scaled size and revise it
  boxSize = (int)(boxScaled * scaleFactor);
  mindiff = 100000;
  range = (int)(3. * scaleFactor);
  for (size = boxSize + range; size >= boxSize - range; size--) {
    if (size <= 3)
      break;
    scaledSobel(NULL, size, size, scaleFactor, minInterp, linear, -1., NULL,
                &nxtmp, &nytmp, &xtmp, &ytmp);
    diff = nxtmp > boxScaled ? nxtmp - boxScaled : boxScaled - nxtmp;
    if (diff < mindiff) {
      mindiff = diff;
      minsize = size;
    }
  }
  boxSize = minsize;
  scaledSobel(NULL, boxSize, boxSize, scaleFactor, minInterp, linear, -1., 
              NULL, &boxScaled, &nytmp, &xtmp, &ytmp);

  // Need padded size for arrays being transformed
  nxpad = niceFrame(nxout, 2, 19);
  nypad = niceFrame(nyout, 2, 19);
  nxpdim = nxpad + 2;
  if (vkeys)
    printf("nxpad %d  nypad %d boxSize %d boxScaled %d\n", nxpad, nypad,
           boxSize, boxScaled);
  printf("Scaling down by %.2f for Sobel filter; box size = %d, scaled size "
         "= %d\n", scaleFactor, boxSize, boxScaled);

  // Get memory for filtered slice, synthetic bead and scaled bead
  filtSlice = (float *)malloc(nxout * nyout * sizeof(float));
  corrSlice = (float *)malloc(nxpdim * nypad * sizeof(float));
  fullBead = (float *)malloc(boxSize * boxSize * sizeof(float));
  oneBead = (float *)malloc(boxSize * boxSize * sizeof(float));
  filtBead = (float *)malloc(boxScaled * boxScaled * sizeof(float));
  splitBead = (float *)malloc(nxpdim * nypad * sizeof(float));
  if (!center)
    writeSlice = (float *)malloc(nxout * nyout * sizeof(float));

  if (!filtSlice || !fullBead  || !filtBead || !splitBead || !corrSlice || 
      !oneBead || (!center && !writeSlice))
    exitError("Failed to get memory for an image array");

  listStart = (int *)malloc((nzout + 2) * sizeof(int));
  if (!listStart)
    exitError("Failed to get memory for working array");
  
  izst = 0;
  for (irun = 0; irun < numRuns; irun++) {
    iznd = izst + numZperRun + (irun < runsAddingOne ? 1 : 0);
    printf("\nAnalyzing group of sections starting with %d, ending with %d\n",
           zlist[izst], zlist[iznd - 1]);

    // Construct a bead
    if (!irun || remakeModelBead)
      makeModelBead(boxSize, beadSize, fullBead);
    npass = threshold != 0. ? 2 : 1;
    listSize = 0;

    // Loop on one or two passes
    for (ipass = 1; ipass <= npass; ipass++) {
      numPeaks = 0;
      numEliminated = 0;
      numBelowMin = 0;
      peakMax = -1.e30;
    
      // Scale down and filter the bead
      //printArray(fullBead, boxSize, boxSize, boxSize);
      scaledSobel(fullBead, boxSize, boxSize, scaleFactor, minInterp, linear, 
                  center, filtBead, &nxtmp, &nytmp, &xBeadOfs, &yBeadOfs);
      beadCenOfs = (boxSize / 2. -  xBeadOfs) / scaleFactor - boxScaled / 2;
      //printArray(filtBead, boxScaled, boxScaled, boxScaled);

      // Split it into 4 corners of the big array and take the FFT
      sliceSplitFill(filtBead, boxScaled, boxScaled, splitBead, nxpdim, nxpad,
                     nypad, 0, 0.);
      todfft(splitBead, &nxpad, &nypad, &forward);
      XCorrSetCTF(sigma1, sigma2, radius1, radius2, ctf, nxpad, nypad, 
                  &ctfDelta);
      if (ctfDelta)
        XCorrFilterPart(splitBead, splitBead, nxpad, nypad, ctf, ctfDelta);

      // Loop on images
      for (indz = izst; indz < iznd; indz++) {
        iz = zlist[indz];
        listStart[indz - izst] = numPeaks;
        numAreaCont = 0;
        if (areaMod)
          makeAreaContList(&areaMod->obj[0], iz, areaConts, &numAreaCont);
      
        // Create a slice and read into it as floats
        sl = readSliceAsFloat(infp, &inhead, sliceMode, iz);

        // Do kernel filtering
        if (kernelSigma > 0.) {
          scaledGaussianKernel(&kernel[0], &ndat, KERNEL_MAXSIZE, kernelSigma);
          sclsl = slice_mat_filter(sl, kernel, ndat);
          if (!sclsl)
            exitError("Failed to get memory for kernel filtered slice");
          sliceFree(sl);
          sl = sclsl;
        }

        // Filter it, write it on last pass if requested
        scaledSobel(sl->data.f, nxin, nyin, scaleFactor, minInterp, linear,
                    center, filtSlice, &nxtmp, &nytmp,  &xtmp, &ytmp);
        // Pad into array and correlate it
        sliceTaperOutPad(filtSlice, SLICE_MODE_FLOAT, nxout, nyout, corrSlice, 
                         nxpdim, nxpad, nypad, 0, 0.);
        //printArray( corrSlice, nxpdim, nxpad, nypad);
        todfft(corrSlice, &nxpad, &nypad, &forward);
        conjugateProduct(corrSlice, splitBead, nxpad, nypad);
        todfft(corrSlice, &nxpad, &nypad, &inverse);
        //printArray( corrSlice, nxpdim, nxpad, nypad);
      

        if (outfp && ipass == npass) {
          if (center)
            writeSlice = filtSlice;
          else {
            for (iy = 0; iy < nyout; iy++)
              for (ix = 0; ix < nxout; ix++)
                writeSlice[ix + nxout * iy] = 
                  corrSlice[ix + nxpdim * iy - (nxpad - nxout) / 2];
          }
          if (mrc_write_slice(writeSlice, outfp, &outhead, iz - izst, 'z'))
            exitError("Writing filtered image for section %d", iz);
          for (iy = 0; iy < nyout; iy++) {
            tsum = 0.;
            for (ix = 0; ix < nxout; ix++) {
              val = writeSlice[ix + iy * nxout];
              tsum += val;
              outhead.amin = B3DMIN(outhead.amin, val);
              outhead.amax = B3DMAX(outhead.amax, val);
            }
            outhead.amean += tsum / ((float)nxout * nyout);
          }
        }
            

        // Search for all peaks in the correlation
        ixst = (nxpad - nxout) / 2 + 1;
        ixnd = ixst + nxout - 2;
        iyst = (nypad - nyout) / 2 + 1;
        iynd = iyst + nyout - 2;
        for (iy = iyst; iy < iynd; iy++) {
          for (ix = ixst; ix < ixnd; ix++) {
            ind = ix + iy * (nxpdim);
            cval = corrSlice[ind];
            if (corrSlice[ind - 1] < cval && corrSlice[ind + 1] <= cval &&
                corrSlice[ind - nxpdim] < cval && 
                corrSlice[ind + nxpdim] <= cval &&
                corrSlice[ind - 1 - nxpdim] < cval && 
                corrSlice[ind + 1 + nxpdim] < cval && 
                corrSlice[ind + 1 - nxpdim] < cval && 
                corrSlice[ind - 1 + nxpdim] < cval) {
              cx = parabolicFitPosition(corrSlice[ind - 1], cval, 
                                        corrSlice[ind + 1]);
              cy = parabolicFitPosition(corrSlice[ind - nxpdim], cval, 
                                        corrSlice[ind + nxpdim]);

              // integer offset in scaled, filtered image
              ixofs = ix - (nxpad - nxout) / 2;
              iyofs = iy - (nypad - nyout) / 2;

              // Center of feature in full original image
              xcen = (ixofs + cx + beadCenOfs) * scaleFactor + xOffset;
              ycen = (iyofs + cy + beadCenOfs) * scaleFactor + yOffset;
              if (numAreaCont && !pointInsideArea(&areaMod->obj[0], areaConts,
                                                  numAreaCont, xcen, ycen))
                continue;

              // First validate the peak by polarity of density in full image
              integral = (float)beadIntegral
                (sl->data.f, nxin, nxin, nyin, rCenter, rInner, rOuter, xcen,
                 ycen, &cenmean, &annmean, kernHist, annulusPctile, &median);
              if (! lightBeads)
                integral = -integral;
              if (integral > 0.) {

                // Good, then get a CCC and add to list
                ccc = templateCCCoefficient(filtSlice, nxout, nxout, nyout,
                                            filtBead, boxScaled, boxScaled, 
                                            boxScaled, ixofs - boxScaled / 2,
                                            iyofs - boxScaled / 2);
                /*if (ipass == npass)
                  printf("peak ix %d iy %d xcen %.2f ycen %.2f integral %f  peak %f ccc "
                  "%.3f\n", ix, iy, xcen, ycen, integral, cval, ccc);*/
                if (ccc > 0.) {
                  if (numPeaks >= listSize) {
                    if (!listSize)
                      peakList = (PeakEntry *)malloc(10000 *sizeof(PeakEntry));
                    else
                      peakList = (PeakEntry *)realloc
                        (peakList, (listSize + 10000) * sizeof(PeakEntry));
                    if (!peakList)
                      exitError("Failed to get memory for peak list");
                    listSize += 10000;
                  }
                  peakList[numPeaks].xcen = xcen;
                  peakList[numPeaks].ycen = ycen;
                  peakList[numPeaks].iz = iz;
                  peakList[numPeaks].ccc = ccc;
                  peakList[numPeaks].peak = cval;
                  if (measureToUse == 1)
                    peakList[numPeaks].peak = integral;
                  if (measureToUse == 2)
                    peakList[numPeaks].peak = 
                      (float)sqrt((double)B3DMAX(cval * integral, 0.));
                  peakList[numPeaks].cenmean = cenmean;
                  peakList[numPeaks].annmean = annmean;
                  peakList[numPeaks].integral = integral;
                  peakList[numPeaks].median = median;
                  peakMax = B3DMAX(peakMax, peakList[numPeaks].peak);
                  numPeaks++;
                }
              }
            }
          }
        }

        sliceFree(sl);
      }
      listStart[iznd - izst] = numPeaks;

      // Adjust for a trend in peak strength with background mean
      // Start with histogram of annular means and set up groups based on this
      meanMedPtr = &peakList[0].annmean;
      if (annulusPctile >= 0.)
        meanMedPtr = &peakList[0].median;
      selectedMinMax(peakList, meanMedPtr, numPeaks, NULL, 0., 0., 
                     &annMin, &annMax, &ninHist);
      kernelHistoPL(peakList, meanMedPtr, numPeaks, 1, NULL, 0., 0., 
                      regHist, MAX_BINS, annMin, annMax, 0., 0);
      if (vkeys)
        printf("min %f max %f ninhist %d\n", annMin, annMax, ninHist);
      numGroups = B3DMIN(numGroups, ninHist / minInGroup);
      dxbin = (annMax - annMin) / MAX_BINS;
      if (numGroups > 1) {
        lowerInd = 0;
        cumStart = 0.;
        ndat = 0;

        // For each group, find upper limit as place where hist reaches target
        for (igr = 0; igr < numGroups; igr++) {
          target = ((igr + 1.) * ninHist) / numGroups;
          sum = 0.;
          ind = lowerInd; 
          cumul = cumStart;
          while (ind < MAX_BINS && (cumul + regHist[ind] < target || 
                                    ind == MAX_BINS - 1)) {
            sum += ((ind + 0.5) * dxbin + annMin) * regHist[ind];
            cumul += regHist[ind++];
          }

          // Get a selected histogram of the peaks
          lowerLim = lowerInd * dxbin + annMin;
          upperLim = ind * dxbin + annMin;
          selectedMinMax(peakList, &peakList[0].peak, numPeaks, 
                         meanMedPtr, lowerLim, upperLim, &selPeakMin,
                         &selPeakMax, &ninSel);
          selPeakMin = minRelativePeak * selPeakMax;
          verbose = 0;
          if (vkeys && strchr(vkeys, 'P')) {
            verbose = 1;
            printf("Peak:  selected data set %d\n", igr + 1);
          } else if (vkeys && strchr(vkeys, 'H'))
            verbose = 2;
          kernelHistoPL(peakList, &peakList[0].peak, numPeaks, 1, 
                          meanMedPtr, lowerLim, upperLim,
                          kernHist, MAX_BINS, selPeakMin, selPeakMax, 
                          0.1 * selPeakMax, verbose);

          if (scanHistogram(kernHist, MAX_BINS, selPeakMin, selPeakMax,
                            selPeakMin, selPeakMax, 1, &dip, &peakBelow,
                            &peakAbove)) {
          
            if (vkeys)
              printf("No histogram dip: annular mean %.2f to %.2f  peaks %.2f "
                     "to %.2f\n", lowerLim, upperLim, selPeakMin, selPeakMax);
          } else {
            annMidval[ndat] = sum / (cumul - cumStart);
            if (!ndat)
              annUseMin = 2. * annMidval[ndat] - upperLim;
            annUseMax = 2. * annMidval[ndat] - lowerLim;
            annDip[ndat] = dip;
            annPeakAbove[ndat] = peakAbove;
            if (vkeys)
              printf("%.2f  %.2f  %.2f %.2f %.2f %.2f %.2f\n", lowerLim,
                     upperLim, annMidval[ndat], selPeakMin, selPeakMax, dip,
                     peakAbove);
            ndat++;
          }
          lowerInd = ind;
          cumStart = cumul;

          /*
          // Find the needed percentile point in here
          ind = 0;
          cumul = 0;
          target = selectPctile * ninSel;
          while (ind < MAX_BINS && (cumul + kernHist[ind] < target || 
          ind == MAX_BINS - 1))
          cumul += kernHist[ind++];
          annPctPeak[igr] = ind * (selPeakMax - selPeakMin) / MAX_BINS +
          selPeakMin; */

        }

        if (ndat > 1) {
          // Fit a line to the points and scale the peaks, find new max
          lsFit(annMidval, annPeakAbove, ndat, &modeSlope, &modeIntcp, &xtmp);
          lsFit(annMidval, annDip, ndat, &selSlope, &selIntcp, &xtmp);
          printf("Dips found in %d groups based on bkg mean, means %f to %f\n",
                 ndat, annMidval[0], annMidval[ndat - 1]);
          if (vkeys)
            printf(" Fit of mode vs background has slope %f, intcp %f\n"
                   " Fit of dip vs background has slope %f, intcp %f\n",
                   modeSlope, modeIntcp, selSlope, selIntcp);

          if (!lightBeads && selSlope > 0. || lightBeads && selSlope < 0.) {
            if (lightBeads) {
              val = ((annUseMin * selSlope + selIntcp) / maxBalRange - 
                     selIntcp) / selSlope;
              annUseMax = B3DMIN(val, annUseMax);
            } else {
              val = ((annUseMax * selSlope + selIntcp) / maxBalRange - 
                     selIntcp) / selSlope;
              annUseMin = B3DMAX(val, annUseMin);
            }
            if (vkeys)
              printf("Adjusting for background limited to %.2f to %.2f\n",
                     annUseMin, annUseMax);
            peakMax = -1.e30;
            for (j = 0; j < numPeaks; j++) {
              if (!peakList[j].ccc)
                continue;
              val = *(&peakList[j].peak + (meanMedPtr - &peakList[0].peak));
              val = B3DMAX(annUseMin, B3DMIN(annUseMax, val));
              peakList[j].peak /= (val * selSlope + selIntcp);
              peakMax = B3DMAX(peakMax, peakList[j].peak);
            }
          }
        }
      }

      // Normalize peak values and eliminate ones below minimum: zero out ccc
      for (j = 0; j < numPeaks; j++) {
        peakList[j].peak /= peakMax;
        if (peakList[j].ccc && peakList[j].peak < minRelativePeak) {
          peakList[j].ccc = 0.;
          numBelowMin++;
        }
      }

      critsq = minDist * minDist;

      // Eliminate peaks that are too close by zeroing out ccc
      for (indz = 0; indz < iznd - izst; indz++) {
        for (i = listStart[indz]; i < listStart[indz + 1]; i++) {
          if (!peakList[i].ccc)
            continue;
          xcen = peakList[i].xcen;
          ycen = peakList[i].ycen;
          jstr = i - 1;
          jend = listStart[indz];
          for (jdir = -1; jdir <= 1 && peakList[i].ccc; jdir += 2) {
            for (j = jstr; j * jdir <= jend * jdir; j += jdir) {
              dy = peakList[j].ycen - ycen;
              if (peakList[j].ccc && jdir * dy > minDist)
                break;
              if (!peakList[j].ccc)
                continue;
              dx = peakList[j].xcen - xcen;
              if (dx >= -minDist && dx <= minDist) {
                distsq = dx * dx + dy * dy;
                if (distsq <= critsq) {
                  numEliminated++;

                  // Found a peak too close
                  // Eliminate current peak and break out of loop if it is 
                  // weaker
                  if (peakList[j].ccc > peakList[i].ccc) {
                    peakList[i].ccc = 0.;
                    break;
                  } else

                    // Otherwise eliminate the other peak and continue
                    peakList[j].ccc = 0.;
                }
              }
            }
            jstr = i + 1;
            jend = listStart[indz + 1] - 1;
          }
        }
      }

      if (npass > 1) 
        printf("Pass %d: ", ipass);
      numPeaksLeft = numPeaks - numEliminated - numBelowMin;
      printf("%d peaks found. %d eliminated as too weak, %d too close, %d "
             "remaining\n", numPeaks, numBelowMin, numEliminated,numPeaksLeft);

      // If doing two passes, now average the beads
      if (ipass < npass) {
        numStart = 0;
        threshUse = threshold;
        if (threshold > 1.) {
          if (numPeaks > 1) 
            qsort(&peakList[0], numPeaks, sizeof(PeakEntry), comparePeaks);
          numStart = B3DMAX(0, numPeaks - (int)threshold);
        } else if (threshold < 0.) {

          // Negative threshold: find dip
          if (findHistoDipPL(peakList, numPeaks, numGuess * nzout, kernHist,
                               regHist, &histDip, &peakBelow, &peakAbove,NULL))
            exitError("Failed to find dip in smoothed histogram of peaks");

          // Set original automatic threshold at 1/4 way from dip to peak, or
          // -threshold as the fraction above dip to take
          threshUse = 0.75 * histDip + 0.25 * peakAbove;
          if (threshold >= -1.) {
            if (numPeaks > 1) 
              qsort(&peakList[0], numPeaks, sizeof(PeakEntry), comparePeaks);
            for (j = 0; j < numPeaks; j++)
              if (peakList[j].ccc && peakList[j].peak >= histDip)
                break;
            numStart = numPeaks + B3DNINT(threshold * (numPeaks - j));
            numStart = B3DMAX(0, B3DMIN(numPeaks - 1, numStart));
            threshUse = peakList[numStart].peak;
          }
          printf("Threshold for averaging set to %.3f\n", threshUse);
        }

        for (i = 0; i < boxSize * boxSize; i++)
          fullBead[i] = 0.;
        nsum = 0;
        amat[0][0] = amat[1][1] = 1.;
        amat[0][1] = amat[1][0] = 0.;
      
        // Loop through images again
        for (indz = izst; indz < iznd; indz++) {
          iz = zlist[indz];
          loaded = 0;
          for (j = numStart; j < numPeaks; j++) {
            if (peakList[j].ccc && peakList[j].iz == iz && 
                (threshUse > 1. || peakList[j].peak >= threshUse)) {
              if (!loaded)
                sl = readSliceAsFloat(infp, &inhead, sliceMode, iz);
              loaded = 1;
            
              // Interpolate the bead into center of array, add it to sum
              cubinterp(sl->data.f, oneBead, nxin, nyin, boxSize, boxSize, 
                        amat, peakList[j].xcen, peakList[j].ycen, 0., 0., 1.,
                        inhead.amean, linear);
              nsum++;
              for (i = 0; i < boxSize * boxSize; i++)
                fullBead[i] += oneBead[i];
            }
          }
          if (loaded)
            sliceFree(sl);
        }
        if (nsum)
          for (i = 0; i < boxSize * boxSize; i++)
            fullBead[i] /= nsum;
      }
    }

    ixst = izst;
    izst = iznd;
    if (!numPeaksLeft)
      continue;
  
    // Determine automatic threshold for putting points out
    threshUse = peakThresh;
    if (peakThresh <= 0.) {
      threshUse = -10000.;
      histDip = -1.;
      if (!findHistoDipPL(peakList, numPeaks, numGuess * nzout, kernHist,
                            regHist, &histDip, &peakBelow, &peakAbove,
                            vkeys)) {
        dxbin = 1. / MAX_BINS;
    
        // Count points above threshold
        jstr = histDip / dxbin;
        nsum = 0;
        sum = 0.;
        sumsq = 0.;
        for (j = jstr; j < MAX_BINS; j++) {
          nsum += B3DNINT(regHist[j]);
          val = (j + 0.5) * dxbin;
          sum += regHist[j] * val;
          sumsq += regHist[j] * val * val;
        }

        // estimate mean and SD for points above threshold
        if (!peakThresh) {
          sdAbove = 0.;
          meanAbove = sum / B3DMAX(1, nsum);
          if (nsum > 1) {
            val = (sumsq - nsum * meanAbove * meanAbove) / (nsum - 1.);
            sdAbove = (float)sqrt((double)(B3DMAX(val, 0.)));
          }
          jend = (meanAbove - 5. * sdAbove) / dxbin;
          jdir = 0;
          for (j = jstr - 1; j >= 0; j--) {
            jdir += B3DNINT(regHist[j]);
            if (jdir > nsum || (j < jend && jdir > nsum / 10))
              break;
          }
    
          threshUse = j * dxbin;
          printf("%d peaks are above threshold of %.3f\n"
                 "%d more peaks %s stored in model down to value of %.3f\n"
                 , nsum, histDip, jdir, numObjOrig ? "would be" : "being",
                 threshUse);
        } else {

          // Or find threshold given a relative number
          jend = -B3DNINT(peakThresh * nsum);
          jdir = 0;
          for (j = MAX_BINS - 1; j >= 0; j--) {
            jdir += B3DNINT(regHist[j]);
            if (jdir >= jend)
              break;
          }
          threshUse = B3DMAX(0., j * dxbin);
          printf("%d peaks are above histogram dip at %.3f\n"
                 "%d total peaks % stored in model down to value of %.3f\n"
                 , nsum, histDip, jdir, numObjOrig ? "would be" : "being",
                 threshUse);
        }

      } else 
        printf("Failed to find dip in histogram\n");
    }

    // Eliminate duplicates from original objects by brute force
    numEliminated = 0;
    for (ob = 0; ob < numObjOrig; ob++) {
      if (iobjClose(imod->obj[ob].flags))
        continue;
      for (indz = ixst; indz < iznd; indz++) {
        iz = zlist[indz];
        
        for (co = 0; co < imod->obj[ob].contsize; co++) {
          cont = &imod->obj[ob].cont[co];
          for (pt = 0; pt < cont->psize; pt++) {
            if (B3DNINT(cont->pts[pt].z) != iz)
              continue;
            xcen = cont->pts[pt].x;
            ycen = cont->pts[pt].y;
            for (i = listStart[indz]; i < listStart[indz + 1]; i++) {
              if (!peakList[i].ccc)
                continue;
              dy = peakList[i].ycen - ycen;
              if (dy < -minDist || dy > minDist)
                continue;
              dx = peakList[i].xcen - xcen;
              if (dx < -minDist || dx > minDist)
                continue;
              distsq = dx * dx + dy * dy;
              if (distsq <= critsq) {
                numEliminated++;
                peakList[i].ccc = 0.;
              }
            }
          }
        }
      }
    }
    if (numEliminated)
      printf("%d peaks were eliminated as too close to existing "
             "points in model\n", numEliminated);

    // Count points to be stored
    numToSave = 0;
    for (i = 0; i < numPeaks; i++)
      if (peakList[i].ccc && (peakList[i].peak >= threshUse))
        numToSave++;
    if (peakThresh >0)
      printf("%d peaks above threshold of %.3f are being stored in model\n",
             numToSave, peakThresh);

    if (imodNewObject(imod))
      exitError("Creating new object in model");
    obj = imodObjectGet(imod);
    obj->flags |= IMOD_OBJFLAG_OPEN | IMOD_OBJFLAG_USE_VALUE;
    obj->matflags2 |= MATFLAGS2_CONSTANT | MATFLAGS2_SKIP_LOW;
    obj->symbol = IOBJ_SYM_CIRCLE;
    obj->symsize = 7;
    obj->contsize = numToSave;
    obj->cont = imodContoursNew(obj->contsize);
    if (!obj->cont) 
      exitError("Creating contours in model");
    ix = 0;
    cccMin = 10000.;
    cccMax = -10000.;
    for (i = 0; i < numPeaks; i++) {
      if (!peakList[i].ccc || (peakList[i].peak < threshUse))
        continue;
      pnt.x = peakList[i].xcen;
      pnt.y = peakList[i].ycen;
      pnt.z = peakList[i].iz;
      if (!imodPointAppend(&obj->cont[ix], &pnt))
        exitError("Adding point to contour");

      store.value.f = peakList[i].peak;
      store.flags = GEN_STORE_FLOAT << 2;
      store.type = GEN_STORE_VALUE1;
      store.index.i = ix++;
      if (istoreInsert(&obj->store, &store))
        exitError("Could not add general storage item");
      cccMin = B3DMIN(cccMin, peakList[i].peak);
      cccMax = B3DMAX(cccMax, peakList[i].peak);
    }
    if (istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, cccMin, cccMax))
      exitError("Could not add general storage item");
    obj->valwhite = 255;
    obj->valblack = 0;
    if (threshUse > -9999. && histDip > cccMin)
      obj->valblack = (unsigned char)B3DNINT(255. * (histDip - cccMin) / 
                                             (cccMax - cccMin));

  }

  // Finish output file
  if (outfp) {
    outhead.amean /= outhead.nz;
    if (mrc_head_write(outfp, &outhead))
      exitError("Writing header to output image file");
    fclose(outfp);
  }
  fclose(infp);

  // Finish model
  if (imod->objsize) {
    imod->xmax = nxin;
    imod->ymax = nyin;
    imod->zmax = inhead.nz;
    
    imodSetRefImage(imod, &inhead);
    
    imodWriteFile(imod);
    fclose(imod->file);
  }

  if (!refmod)
    exit(0);
    
  // Compare reference model to the peak model
  // First count # of actual points
  obj = NULL;
  if (boundObj >= 0)
    obj = &refmod->obj[boundObj-1];
  for (ob = 0; ob < refmod->objsize; ob++) {
    if (ob == boundObj)
      continue;
    for (co = 0; co < refmod->obj[ob].contsize; co++) {
      cont = &refmod->obj[ob].cont[co];
      for (pt = 0; pt < cont->psize; pt++) {
        if (pointInsideBoundary(obj, &cont->pts[pt]))
          numRefPts++;
      }
    }
  }
            
  // Next look at each peak inside boundary and look for match
  qsort(&peakList[0], numPeaks, sizeof(PeakEntry), comparePeaks);
  for (i = 0; i < numPeaks; i++) {
    peakList[i].integral = -1.;
    if (peakList[i].ccc) {
      pnt.x = peakList[i].xcen;
      pnt.y = peakList[i].ycen;
      pnt.z = peakList[i].iz;
      if (pointInsideBoundary(obj, &pnt)) {
        peakList[i].integral = 0;
        for (ob = 0; ob < refmod->objsize && !peakList[i].integral; ob++) {
          if (ob == boundObj)
            continue;
          for (co = 0; co < refmod->obj[ob].contsize && !peakList[i].integral;
               co++) {
            cont = &refmod->obj[ob].cont[co];
            for (pt = 0; pt < cont->psize; pt++) {
              if (B3DNINT(cont->pts[pt].z) == peakList[i].iz && 
                  imodPointDistance(&cont->pts[pt], &pnt) < matchCrit) {
                peakList[i].integral = 1.;
                break;
              }
            }
          }
        }
        if (vkeys && strchr(vkeys, 'g'))
          printf("Peak: %d %.4f %.4f %f %f %f %f\n", 
                 B3DNINT(peakList[i].integral),
                 peakList[i].peak, peakList[i].peak * peakList[i].ccc,
                 peakList[i].cenmean - peakList[i].annmean, 
                 peakList[i].cenmean, peakList[i].annmean, peakList[i].median);
      }
    }
  }
  
  // Walk backwards through list and count up not matched and matched and
  // find place with minimum total error
  errMin = numRefPts + 1;
  lastPeak = 1.0;
  for (i = numPeaks - 1; i >= 0; i--) {
    if (peakList[i].integral < 0.)
      continue;
    error = numRefPts + numUnmatched - numMatched;
    if (lastPeak >= histDip && peakList[i].peak < histDip)
      printf("Threshold criterion of %.3f has error %d, %d fn and %d fp\n",
             histDip, B3DNINT(error), numRefPts - numMatched, numUnmatched);
    if (error < errMin) {
      errMin = error;
      peakAbove = lastPeak;
      peakBelow = peakList[i].peak;
      numPeakPts = numMatched + numUnmatched;
      numPeakMatch = numMatched;
    }
    lastPeak = peakList[i].peak;
    if (peakList[i].integral > 0.)
      numMatched++;
    else
      numUnmatched++;
  }

  printf("Minimum error %d with a criterion of %.4f (%.4f to %.4f):\n"
         "%d unmatched of %d actual points, %.1f%% false negative\n"
         "%d unmatched peak points, %.1f%% false positive\n", 
         numRefPts + numPeakPts - 2 * numPeakMatch,
         0.5 * (peakAbove + peakBelow), peakBelow, peakAbove, 
         numRefPts - numPeakMatch, numRefPts,
         100. * (numRefPts - numPeakMatch) / numRefPts, 
         numPeakPts - numPeakMatch,
         100. * (numPeakPts - numPeakMatch) / numRefPts);

  exit(0);
}

/*
 * Tests if pnt is inside one of the contours of obj; returns 1 if it is inside
 * 1 or if there are no contours on the point's section
 */
static int pointInsideBoundary(Iobj *obj, Ipoint *pnt)
{
  int co;
  int inside = 1;
  if (!obj)
    return 1;
  for (co = 0; co < obj->contsize; co++) {
    if (obj->cont[co].psize && 
        B3DNINT(obj->cont[co].pts[0].z) == B3DNINT(pnt->z)) {
      inside = imodPointInsideCont(&obj->cont[co], pnt);
      if (inside)
        return 1;
    }
  }
  return inside;
}

/*
 * Comparison function for qsort on peak strengths
 */
static int comparePeaks(const void *p1, const void *p2)
{
  PeakEntry *peak1 = (PeakEntry *)p1;
  PeakEntry *peak2 = (PeakEntry *)p2;
  if (!peak1->ccc && peak2->ccc)
    return -1;
  else if (peak1->ccc && !peak2->ccc)
    return 1;
  else if (peak1->peak < peak2->peak)
    return -1;
  else if (peak1->peak > peak2->peak)
    return 1;
  return 0;
}

/* NOTES ON COORDINATES:
   original_image_coord = reduced_image_coord * scaleFactor + xOffset

   The bead is centered in the original model, regardless of whether it is
   odd or even.  
   Original bead center = boxSize / 2.

   original_bead_coord = reduced_bead_coord * scaleFactor + xBeadOfs
   Reduced bead center = (boxSize / 2. - xBeadOfs)  / scaleFactor

   With a centered bead in split image, peak = shift of bead to feature
   peak position = center of feature in reduced image
   If bead is offset positively from center, the bead needs to be shifted less
   to match a feature, so peak position is reduced
   feature_center = peak_position + bead_offset

   The boxScaled / 2 pixel is the one that ends up at the origin in the split
   filled image.  This is / 2 not / 2.  !
   Coordinate in reduced bead at origin = boxScale / 2
   Offset of bead from being centered in split image = 
   bead_offset = (boxSize / 2. - xBeadOfs)  / scaleFactor - boxScale / 2

*/   


/*
 * Read a slice at iz in fle and convert it to a floating slice, then taper it
 * by 64 pixels at fill edges
 */
Islice *readSliceAsFloat(FILE *infp, MrcHeader *inhead, int sliceMode, int iz)
{
  Islice *sl;

  sl = sliceReadFloat(inhead, iz);
  if (!sl)
    exitError("Creating slice for or reading section %d", iz);
  
  // Taper fill areas very slowly to avoid big gradients there
  if (sliceTaperAtFill(sl, 64, 0))
    exitError("Getting memory for tapering edges");
  return sl;
}

/*
 * Computes CCC in real space between the image in template, size nxt by nyt
 * and X dimension nxtdim, and a position in the image in array, size nx by ny
 * and X dimension nxdim.  xoffset, yoffset is the offset from pixels in the
 * template to pixels in the array.
 */
static float templateCCCoefficient(float *array, int nxdim, int nx, int ny, 
                                   float *template, int nxtdim, int nxt, 
                                   int nyt, int xoffset, int yoffset)
{
  double asum, bsum, csum, asumsq, bsumsq, ccc, aval, bval;
  int ixst, ixnd, iyst, iynd, ix, iy, nsum;
  ixst = B3DMAX(0, xoffset);
  ixnd = B3DMIN(nx - 1, nxt + xoffset - 1);
  iyst = B3DMAX(0, yoffset);
  iynd = B3DMIN(ny - 1, nyt + yoffset - 1);
  nsum = (ixnd + 1 - ixst) * (iynd + 1 - iyst);
  asum = asumsq = bsum = bsumsq = csum = 0.;
  /*printf("xoffset %d  yoffset %d ixst %d  ixnd %d iyst %d iynd %d\n",
    xoffset, yoffset, ixst, ixnd,iyst,iynd); */
  if (nsum < 16)
    return 0.;
  for (iy = iyst; iy < iynd; iy++) {
    for (ix = ixst; ix < ixnd; ix++) {
      aval = array[ix + iy * nxdim];
      bval = template[ix - xoffset + (iy - yoffset) * nxtdim];
      asum += aval;
      asumsq += aval * aval;
      bsum += bval;
      bsumsq += bval * bval;
      csum += aval * bval;
    }
  }
  ccc = (nsum * asumsq - asum * asum) * (nsum * bsumsq - bsum * bsum);
  //ccc = (nsum * bsumsq - bsum * bsum) * (nsum * bsumsq - bsum * bsum);
  if (ccc <= 0.)
    return 0.;
  ccc = (nsum * csum - asum * bsum) / sqrt(ccc);
  return ccc;
}

/*
 * Computes a standard or kernel histogram from the peaks in peaklist; element
 * is pointer to measure to use in first peak; select if non-null is used to
 * select on a different member of the peak entry, in wihch case selMin and
 * selMax specify min and max of range to select.  The histogram is placed in 
 * bins, and occupies numBins between firstVal and lastVal.  Peaks with zero 
 * ccc are skipped if skipZeroCCC is set.  h is the kernel width, or 0 for a 
 * standard binned histogram.
 */
static void kernelHistoPL(PeakEntry *peakList, float *element, int numPeaks,
                            int skipZeroCCC, float *select, float selMin, 
                            float selMax, float *bins, int numBins,
                            float firstVal, float lastVal, float h,
                            int verbose)
{
  float dxbin, delta, val;
  int i, j, ist, ind;
  dxbin = (lastVal - firstVal) / numBins;
  for (i = 0; i < numBins; i++)
    bins[i] = 0.;
  for (j = 0; j < numPeaks; j++) {
    if (skipZeroCCC && !peakList[j].ccc)
      continue;
    if (select) {
      val = *(&peakList[j].peak + (select - &peakList[0].peak));
      if (val < selMin || val >= selMax)
        continue;
    }
    val = *(&peakList[j].peak + (element - &peakList[0].peak));

    if (verbose == 1)
      printf("Peak: %.4f\n", val);
    if (h) {
      ist = (int)ceil((double)(val - h - firstVal) / dxbin);
      ind = (int)floor((double)(val + h - firstVal) / dxbin);
      ist = B3DMAX(0, ist);
      ind = B3DMIN(numBins - 1, ind);
      for (i = ist; i <= ind; i++) {
        delta = (val - firstVal - i * dxbin) / h;
        bins[i] += (float)pow(1. - delta * delta, 3.);
      }
    } else {
      ist = (int)floor((double)(val - firstVal) / dxbin);
      if (ist >= 0 && ist < numBins)
        bins[ist]++;
      else if (ist == numBins && val - lastVal < 0.001 * dxbin)
        bins[numBins - 1]++;
    }
  }   
  if (verbose == 2)
    for (i = 0; i < numBins; i++)
      printf("bin: %.4f %f\n", firstVal + i * dxbin, bins[i]);
}

/*
 * Finds a histogram dip by starting with a high smoothing and dropping to
 * lower one.
 */
static int findHistoDipPL(PeakEntry *peakList, int numPeaks, int minGuess,
                          float *kernHist, float *regHist, float *histDip,
                          float *peakBelow, float *peakAbove, char *vkeys)
{
  float coarseH = 0.2f;
  float fineH = 0.05f;
  int numCut = 4;
  float fracGuess = 0.5f;
  int i, ncum, numCrit, verbose;
  float upperLim = 1.0;

  // Build a regular histogram first and use minGuess to find safe upper limit
  verbose = (vkeys != NULL && strchr(vkeys, 'p')) ? 1 : 0;
  kernelHistoPL(peakList, &peakList[0].peak, numPeaks, 1, NULL, 0., 0.,
                  regHist, MAX_BINS, 0., 1., 0., verbose);
  if (minGuess) {
    numCrit = B3DMAX(1, B3DNINT(minGuess * fracGuess));
    ncum = 0;
    for (i = MAX_BINS - 1; i > 10; i--) {
      ncum += B3DNINT(regHist[i]);
      if (ncum >= numCrit)
        break;
    }
    upperLim = i / (MAX_BINS - 1.);
  }
  
  // Seek a kernel width that gives two peaks in histogram
  for (i = 0; i < numCut; i++) {
    verbose = 0;
    if (vkeys && (strchr(vkeys, 'e') || (strchr(vkeys, 'i') && !i)))
      verbose = 2;
    kernelHistoPL(peakList, &peakList[0].peak, numPeaks, 1, NULL, 0., 0.,
                    kernHist, MAX_BINS, 0., 1., coarseH, verbose);

    // Cut H if it fails or if the top peak is at 1.0
    if (!scanHistogram(kernHist, MAX_BINS, 0., 1., 0., upperLim, 1, histDip,
                       peakBelow, peakAbove) && *peakAbove < 0.999) 
      break;
    coarseH *= 0.707;
  }
  if (i == numCut)
    return 1;
  
  printf("Histogram smoothed with H = %.3f has dip at %.3f, peaks at %.3f"
         " and %.3f\n", coarseH, *histDip, *peakBelow, *peakAbove);
  
  verbose = (vkeys != NULL && strchr(vkeys, 'f')) ? 2 : 0;
  kernelHistoPL(peakList, &peakList[0].peak, numPeaks, 1, NULL, 0., 0.,
                  kernHist, MAX_BINS, 0., 1., fineH, verbose);
  scanHistogram(kernHist, MAX_BINS, 0., 1., 0.5 * (*histDip + *peakBelow),
                0.5 * (*histDip + *peakAbove), 0, histDip,
                peakBelow, peakAbove);
  printf("Histogram smoothed with H = %.3f has lowest dip at %.3f\n",
         fineH, *histDip);
  return 0;
}


static void printArray(float *filtBead, int nxdim, int nx, int ny)
{
  int ix, iy;
  printf("%d x %d array:\n", nx, ny);
  for (iy = 0; iy < ny; iy++) {
    for (ix = 0; ix < nx; ix++)
      printf("%.2f ", filtBead[ix + iy * nxdim]);
    printf("\n");
  }
}

/*
 * Computes the min and max and number of peaks, possibly selecting on some
 * value being in a given range.  element is the pointer to the peak measure
 * to be assessed in the first peak; select can be NULL or a pointer to the
 * element in the first peak to use for selection, and selMin and selMax
 * are minimum and maximum values for the selection range.  min and max and
 * number in range are returne din minVal, maxVal, and ninRange.
 */
static void selectedMinMax(PeakEntry *peakList, float *element, int numPeaks,
                           float *select, float selMin, float selMax,
                           float *minVal, float *maxVal, int *ninRange)
{
  float dxbin, delta, val;
  int i, j, ist, ind;
  *minVal = 1.e30;
  *maxVal = -1.e30;
  *ninRange = 0;
  for (j = 0; j < numPeaks; j++) {
    if (!peakList[j].ccc)
      continue;
    if (select) {
      val = *(&peakList[j].peak + (select - &peakList[0].peak));
      if (val < selMin || val >= selMax)
        continue;
    }
    val = *(&peakList[j].peak + (element - &peakList[0].peak));
    *minVal = B3DMIN(*minVal, val);
    *maxVal = B3DMAX(*maxVal, val);
    (*ninRange)++;
  }
}

/*
 * Make a list of the contours in obj at Z value iz; rteurn them in list and
 * number of values in nlist
 */
static void makeAreaContList(Iobj *obj, int iz, int *list, int *nlist)
{
  int co, dzmin, izmin, zco, dz;
  izmin = -999;
  dzmin = 100000;
  for (co = 0; co < obj->contsize; co++) {
    if (!obj->cont[co].psize)
      continue;
    zco = B3DNINT(obj->cont[co].pts[0].z);
    dz = B3DMAX(iz - zco, zco - iz);
    if (dz < dzmin) {
      dzmin = dz;
      izmin = zco;
    }
  }

  if (izmin == -999)
    exitError("No contours in object 1 of area model");
  *nlist = 0;
  for (co = 0; co < obj->contsize; co++) {
    if (!obj->cont[co].psize)
      continue;
    zco = B3DNINT(obj->cont[co].pts[0].z);
    if (zco == izmin) {
      if (*nlist == MAX_AREAS - 1)
        exitError("Too many contours on one section in area model for array");
      list[(*nlist)++] = co;
    }
  }
}

/* 
 * Test for whether the point xcen, ycen is inside any of the contours in obj
 * that are listed in list (nlist values there)
 */
static int pointInsideArea(Iobj *obj, int *list, int nlist, float xcen, 
                           float ycen)
{
  int i;
  Ipoint pnt;
  pnt.x = xcen;
  pnt.y = ycen;
  pnt.z = 0.;
  for (i = 0; i < nlist; i++) {
    if (imodPointInsideCont(&obj->cont[list[i]], &pnt))
      return 1;
  }
  return 0;
}

/*

$Log$
Revision 3.5  2008/12/01 15:44:39  mast
Pulled out more library functions, made kernel filtering be default

Revision 3.4  2008/11/12 03:48:19  mast
Pulled out the scan histogram function for library

Revision 3.3  2008/11/02 13:43:48  mast
Switched to float-slice reading function

Revision 3.2  2008/06/22 05:04:26  mast
Make sure valblack is not based on dip below the minimum value output

Revision 3.1  2008/06/19 23:26:50  mast
Added to package


*/

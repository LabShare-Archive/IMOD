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
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <vector>
#include "imodel.h"
#include "b3dutil.h"
#include "iimage.h"
#include "sliceproc.h"
#include "parse_params.h"
#include "cfft.h"
#include "imodfindbeads.h"


// Local functions
static int comparePeaks(const void *p1, const void *p2);

// Main: just construct object and call its main
int main( int argc, char *argv[])
{
  FindBeads find;
  find.main(argc, argv);
  exit(0);
}

/* 
 * Constructor: initialize variables with defaults
 */
FindBeads::FindBeads()
{
  mAverageFallback = 0;
  mStorageFallback = 0;
  mFiltSlice = NULL;
  mCorrSlice = NULL;
  mFullBead = NULL;
  mOneBead = NULL;
  mSplitBead = NULL;
  mWriteSlice = NULL;
  mThreshold = -2.;
  mPeakThresh = 0.;
  mCenterWeight = 2.;
  mLightBeads = 0;
  mAnnulusPctile = -1.;
  mMinRelativePeak = 0.1f;
  mMinSpacing = 1.;
  mScaledSize = 8.;
  mNumGroups = 4;
  mDumpType = 1;
  mVkeys = NULL;
  mMinInterp = 1.4f;
  mLinearInterp = 0;
  mDumpFp = NULL;
  mAlignXshift = NULL;
  mAlignYshift = NULL;
  mFiltBead = NULL;
  mAreaMod = NULL;
  mMeasureToUse = 1;
  mExcludeAreas = 0;
  mAdjustSizes = 0;
  mProfiling = false;
  mWallStart = mWallLast = wallTime();
  mMinSizeForAdjust = 5.;         // Minimum bead size to apply adjustment for
  mMaxAdjustFactor = 1.6;         // Maximum factor to change the bead size by
  mMinSizeChangeForRedo = 1.05;  // Minimum factor of change for adding another pass
}

/* 
 * Main entry: option processing, main loop, and final output
 */
void FindBeads::main(int argc, char *argv[])
{
  char *filename;
  char *outModel;
  //char *progname = imodProgName(argv[0]);
  const char *progname = "imodfindbeads";
  int numOptArgs, numNonOptArgs;
  Imod *refmod = NULL;
  Imod *imod;
  Iobj *obj;
  Icont *cont;
  Ipoint pnt;
  FILE *outfp = NULL;
  MrcHeader outhead;
  Islice *sl, *sclsl;
  Istore store;
  int *listStart;
  int boundObj = -1;
  int numGuess = 0;
  int remakeModelBead = 0;
  int forward = 0, inverse = 1;
  float xscale, yscale;
  float ctf[8193];
  char line[MAX_LINE];
  int numRefPts = 0, numPeakPts, numPeakMatch, numPeaksLeft, numBelowMin;
  int numMatched = 0, numUnmatched = 0;
  float kernelSigma = 0.85;
  float sigma1 = 0., sigma2 = 0., radius1 = 0., radius2 = 0.;
  int izStart, izEnd, nytmp, npass, binning;
  int nzout, nxtmp;
  float zscale, xtmp, ytmp, xBeadOfs, yBeadOfs;
  float val, tsum, xcen, ycen;
  int ipass, numEliminated, iz, iy, ix, i, ixst;
  int cacheLimitMB = 768;
  int j, jdir, jstr, jend, sliceMode, maxZperRun;
  float critsq, dx, dy, distsq, cccMin, cccMax, sizeOrig;
  float peakAbove, peakBelow, error, lastPeak, errMin, histDip;
  float ctfDelta;
  int numToSave, ndat;
  int maxSec, numRuns, numZperRun, runsAddingOne, irun, indz;
  float threshUse;
  //float annPctPeak[MAX_GROUPS], selectPctile = 0.90;
  float kernel[KERNEL_MAXSIZE * KERNEL_MAXSIZE];
  int ob, co, pt;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 imodfindbeads  */
  int numOptions = 39;
  const char *options[] = {
    "input:InputImageFile:FN:", "output:OutputModelFile:FN:",
    "filtered:FilteredImageFile:FN:", "area:AreaModel:FN:",
    "exclude:ExcludeInsideAreas:B:", "query:QueryAreaOnSection:I:",
    "prexf:PrealignTransformFile:FN:", "imagebinned:ImagesAreBinned:I:",
    "add:AddToModel:FN:", "ref:ReferenceModel:FN:", "boundary:BoundaryObject:I:",
    "size:BeadSize:F:", "light:LightBeads:B:", "scaled:ScaledSize:F:",
    "adjust:AdjustSizes:B:", "interpmin:MinInterpolationFactor:F:",
    "linear:LinearInterpolation:I:", "center:CenterWeight:F:", "box:BoxSizeScaled:I:",
    "threshold:ThresholdForAveraging:F:", "store:StorageThreshold:F:",
    "fallback:FallbackThresholds:IP:", "bkgd:BackgroundGroups:F:",
    "annulus:AnnulusPercentile:F:", "peakmin:MinRelativeStrength:F:",
    "spacing:MinSpacing:F:", "sections:SectionsToDo:LI:",
    "maxsec:MaxSectionsPerAnalysis:I:", "remake:RemakeModelBead:B:",
    "guess:MinGuessNumBeads:I:", "measure:MeasureToUse:I:", "kernel:KernelSigma:F:",
    "rad1:FilterRadius1:F:", "rad2:FilterRadius2:F:", "sig1:FilterSigma1:F:",
    "sig2:FilterSigma2:F:", "verbose:VerboseKeys:CH:", "dump:DumpHistogramFile:FN:",
    "param:ParameterFile:PF:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        3, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input file */
  if (PipGetInOutFile("InputImageFile", 0, &filename))
    exitError("No input image file specified");
  mInFp = iiFOpen(filename, "rb");
  if (!mInFp)
    exitError("Opening input image file %s", filename);
  free(filename);
  if (mrc_head_read(mInFp, &mInHead))
    exitError("Reading header of image file %s", filename);
  xscale = yscale = zscale = 1.;
  if (mInHead.mx && mInHead.xlen)
    xscale = mInHead.xlen / (float)mInHead.mx;
  if (mInHead.my && mInHead.ylen)
    yscale = mInHead.ylen / (float)mInHead.my;
  if (mInHead.mz && mInHead.zlen)
    zscale = mInHead.zlen / (float)mInHead.mz;

  // Check if it is the correct data type and set slice type
  sliceMode = sliceModeIfReal(mInHead.mode);
  if (sliceMode < 0)
    exitError("File mode is %d; only byte, short, float allowed", mInHead.mode);
  mNxIn = mInHead.nx;
  mNyIn = mInHead.ny;

  // Read area model
  if (!PipGetString("AreaModel", &filename)) {
    mAreaMod = imodRead(filename);
    if (!mAreaMod)
      exitError("Reading area model %s", filename);
    free(filename);
    if (!mAreaMod->objsize || !mAreaMod->obj[0].contsize)
      exitError("No contours in object 1 of area model");
    PipGetBoolean("ExcludeInsideAreas", &mExcludeAreas);
  }

  // Check if this is a run just to find the area
  if (!PipGetInteger("QueryAreaOnSection", &iz)) {
    if (!mAreaMod)
      exitError("You must enter an area model to use -query");
    areaContListCheckErr(iz);
    mBeadSize = 0.;
    for (co = 0; co < mNumAreaCont; co++)
      mBeadSize += imodContourArea(&mAreaMod->obj[0].cont[mAreaConts[co]]);
    if (mExcludeAreas)
      mBeadSize = mInHead.nx * mInHead.ny - mBeadSize;

    // Autofidseed is looking for 'Area (megapixels)' on any line
    printf("Area (megapixels) included in analysis = %.3f", mBeadSize * 1.e-6);
    exit(0);
  }

  // Get output file
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
    if (xscale != 1.0f) {
      imod->pixsize = xscale / 10.f;
      imod->units = IMOD_UNIT_NM;
    }
  }
  mNumObjOrig  = imod->objsize;

  // Set up dump file
  if (!PipGetString("DumpHistogramFile", &filename)) {
    imodBackupFile(filename);
    mDumpFp = fopen(filename, "w");
    if (!mDumpFp)
      exitError("Failed to open file for histograms, %s", filename);
    free(filename);
  }

  // Get other parameters
  PipGetFloat("ScaledSize", &mScaledSize);
  // exitError("You must enter a scaled size for the filtering");
  if (PipGetFloat("BeadSize", &mBeadSize))
    exitError("You must enter a bead size");
  PipGetBoolean("AdjustSizes", &mAdjustSizes);
  PipGetBoolean("LightBeads", &mLightBeads);
  PipGetInteger("LinearInterpolation", &mLinearInterp);
  PipGetBoolean("RemakeModelBead", &remakeModelBead);
  PipGetFloat("ThresholdForAveraging", &mThreshold);
  PipGetFloat("StorageThreshold", &mPeakThresh);
  PipGetTwoIntegers("FallbackThresholds", &mAverageFallback, &mStorageFallback);
  PipGetFloat("CenterWeight", &mCenterWeight);
  PipGetInteger("BackgroundGroups", &mNumGroups);
  PipGetFloat("MinInterpolationFactor", &mMinInterp);
  PipGetFloat("MinSpacing", &mMinSpacing);
  PipGetInteger("MinGuessNumBeads", &numGuess);
  PipGetInteger("MeasureToUse", &mMeasureToUse);
  PipGetFloat("AnnulusPercentile", &mAnnulusPctile);
  PipGetFloat("KernelSigma", &kernelSigma);
  PipGetFloat("FilterRadius1", &radius1);
  PipGetFloat("FilterRadius2", &radius2);
  PipGetFloat("FilterSigma1", &sigma1);
  PipGetFloat("FilterSigma2", &sigma2);
  PipGetString("VerboseKeys", &mVkeys);
  mNumGroups = B3DMIN(MAX_GROUPS, mNumGroups);
  if (mLightBeads && mAnnulusPctile >= 0.)
    mAnnulusPctile = 1. - mAnnulusPctile;

  if (mAdjustSizes && mBeadSize < mMinSizeForAdjust) {
    mAdjustSizes = 0;
    printf("WARNING: imodfindbeads - Bead size is below limit for finding diameter; "
           "no adjustment will be done\n");
  }
  sizeOrig = mBeadSize;

  // If doing automatic thresholds and no peak limit entered, drop it to
  // allow more histogram to be built
  if (PipGetFloat("MinRelativeStrength", &mMinRelativePeak) && mThreshold < 0.)
    mMinRelativePeak /= 2.;

  // Make default box size and list of sections to do
  mBoxScaledOrig = B3DNINT(3 * mScaledSize + 4);
  PipGetInteger("BoxSizeScaled", &mBoxScaledOrig);
  if (!PipGetString("SectionsToDo", &filename)) {
    mZlist = parselist(filename, &nzout);
    if (!mZlist)
      exitError("Bad entry in list of sections to do");
    free(filename);
  } else {
    nzout = mInHead.nz;
    mZlist = B3DMALLOC(int, nzout);
    if (mZlist)
      for (i = 0; i < nzout; i++)
        mZlist[i] = i;
  }
  if (!mZlist)
    exitError("Failed to get memory for list of sections");
  for (i = 0; i < nzout; i++)
    if (mZlist[i] < 0 || mZlist[i] >= mInHead.nz)
      exitError("Section # %d is out of range", mZlist[i]);

  // Get shifts if option is entered
  if (!PipGetString("PrealignTransformFile", &filename)){
    outfp = fopen(filename, "r");
    if (!outfp)
      exitError("Failed to open prealignment transform file %s\n", filename);
    free(filename);
    mAlignXshift = B3DMALLOC(float, mInHead.nz);
    mAlignYshift = B3DMALLOC(float, mInHead.nz);
    binning = 1;
    PipGetInteger("ImagesAreBinned", &binning);
    if (binning <= 0)
      exitError("Binning entry must be positive");
    if (!mAlignXshift || !mAlignYshift)
      exitError("Allocating arrays for shifts");
    for (i = 0; i < mInHead.nz; i++)
      mAlignXshift[i] = mAlignYshift[i] = 0.;
    for (i = 0; i < mInHead.nz; i++) {
      ix = fgetline(outfp, line, MAX_LINE);
      if (ix == -1)
        exitError("Reading prealignment transform file %s\n", filename);
      if (ix == -2)
        break;
      sscanf(line, "%f %f %f %f %f %f", &xtmp, &ytmp, &xcen, &ycen, &mAlignXshift[i],
             &mAlignYshift[i]);
      mAlignXshift[i] /= binning;
      mAlignYshift[i] /= binning;
      if (ix < 0)
        break;
    }
    fclose(outfp);
    outfp = NULL;
  }

  // Figure out division into separate runs and set up for caching read slices
  maxSec = nzout;
  PipGetInteger("MaxSectionsPerAnalysis", &maxSec);
  if (maxSec <= 0)
    exitError("Maximum number of sections per analysis must be positive");
  numRuns = (nzout + maxSec - 1) / maxSec;
  numZperRun = nzout / numRuns;
  runsAddingOne = nzout % numRuns;
  maxZperRun = numZperRun + (runsAddingOne ? 1 : 0);
  mUseSliceCache = maxZperRun * mNxIn * mNyIn * 4 / 1.e6 < cacheLimitMB;
  if (mUseSliceCache) {
    mCachedSlices = B3DMALLOC(Islice *, maxZperRun);
    if (!mCachedSlices)
      exitError("Getting memory for silly little array");
  }

  if (numRuns > 1 && refmod)
    exitError("All sections must be analyzed together when comparing with "
              "reference model");

  // Open file for filtered images
  if (!PipGetString("FilteredImageFile", &filename)) {
    if (imodBackupFile(filename))
      printf("WARNING: %s - Error renaming existing image file %s\n", progname,
             filename);
    outfp = iiFOpen(filename, "wb");
    if (!outfp)
      exitError("Opening output image file %s", filename);
    free(filename);
  }

  imodBackupFile(outModel);
  imod->file = fopen(outModel, "wb");
  if (!imod->file)
    exitError("Opening output model %s", outModel);
  free(outModel);

  setupSizeDependentVars();

  listStart = B3DMALLOC(int, nzout + 2);
  if (!listStart)
    exitError("Failed to get memory for working array");

  profile("Finished startup tasks");
  izStart = 0;
  for (irun = 0; irun < numRuns; irun++) {
    izEnd = izStart + numZperRun + (irun < runsAddingOne ? 1 : 0);
    printf("\nAnalyzing group of sections starting with %d, ending with %d\n",
           mZlist[izStart], mZlist[izEnd - 1]);
    mMinGuess = numGuess * (izEnd - izStart);

    // Construct a bead
    if (!irun || remakeModelBead)
      makeModelBead(mBoxSize, mBeadSize, mFullBead);
    npass = mThreshold != 0. ? 2 : 1;
    mListSize = 0;

    // Loop on one or two passes
    for (ipass = 1; ipass <= npass; ipass++) {
      mNumPeaks = 0;
      numEliminated = 0;
      numBelowMin = 0;
      mPeakMax = -1.e30;

      // On the last pass, set up the output file
      if (outfp && ipass == npass) {
        mrc_head_new(&outhead, mNxOut, mNyOut, nzout, MRC_MODE_FLOAT);
        mrc_head_label_cp(&mInHead, &outhead);
        mrc_head_label(&outhead, "imodfindbeads: Scaled and Sobel filtered");

        // Set scale and origin in new header to display the match input data
        outhead.xlen = mNxOut * xscale * mScaleFactor;
        outhead.ylen = mNyOut * yscale * mScaleFactor;
        outhead.zlen = nzout * zscale;
        outhead.xorg -= mXoffset * xscale;
        outhead.yorg -= mYoffset * yscale;
        outhead.zorg -= mZlist[0] * zscale;
        outhead.amin = 1.e30;
        outhead.amax = -1.e30;
        outhead.amean = 0.;
      }
    
      // Scale down and filter the bead
      //printArray(mFullBead, mBoxSize, mBoxSize, mBoxSize);
      scaledSobel(mFullBead, mBoxSize, mBoxSize, mScaleFactor, mMinInterp, mLinearInterp, 
                  mCenterWeight, mFiltBead, &nxtmp, &nytmp, &xBeadOfs, &yBeadOfs);
      mBeadCenOfs = (mBoxSize / 2. -  xBeadOfs) / mScaleFactor - mBoxScaled / 2;
      //printArray(mFiltBead, mBoxScaled, mBoxScaled, mBoxScaled);

      // Split it into 4 corners of the big array and take the FFT
      sliceSplitFill(mFiltBead, mBoxScaled, mBoxScaled, mSplitBead, mNxpDim, mNxPad,
                     mNyPad, 0, 0.);
      todfft(mSplitBead, &mNxPad, &mNyPad, &forward);
      XCorrSetCTF(sigma1, sigma2, radius1, radius2, ctf, mNxPad, mNyPad, 
                  &ctfDelta);
      if (ctfDelta)
        XCorrFilterPart(mSplitBead, mSplitBead, mNxPad, mNyPad, ctf, ctfDelta);
      profile("Set up the bead");

      // Loop on images
      for (indz = izStart; indz < izEnd; indz++) {
        iz = mZlist[indz];
        listStart[indz - izStart] = mNumPeaks;
        mNumAreaCont = 0;
        if (mAreaMod)
          areaContListCheckErr(iz);
      
        // Create a slice and read into it as floats
        if (ipass == 1 || !mUseSliceCache) {
          sl = readSliceAsFloat(iz);
          profile("Read slice");
          if (mUseSliceCache)
            mCachedSlices[indz - izStart] = sl;
        } else
          sl = mCachedSlices[indz - izStart];
 
        // Do kernel filtering
        if (kernelSigma > 0.) {
          scaledGaussianKernel(&kernel[0], &ndat, KERNEL_MAXSIZE, kernelSigma);
          sclsl = slice_mat_filter(sl, kernel, ndat);
          if (!sclsl)
            exitError("Failed to get memory for kernel filtered slice");
          if (!mUseSliceCache)
            sliceFree(sl);
          sl = sclsl;
          profile("kernel filtered");
        }

        // Filter it, write it on last pass if requested
        scaledSobel(sl->data.f, mNxIn, mNyIn, mScaleFactor, mMinInterp, mLinearInterp,
                    mCenterWeight, mFiltSlice, &nxtmp, &nytmp,  &xtmp, &ytmp);
        profile("Sobel filtered");

        // Pad into array and correlate it
        sliceTaperOutPad(mFiltSlice, SLICE_MODE_FLOAT, mNxOut, mNyOut, mCorrSlice, 
                         mNxpDim, mNxPad, mNyPad, 0, 0.);
        //printArray( mCorrSlice, mNxpDim, mNxPad, mNyPad);
        todfft(mCorrSlice, &mNxPad, &mNyPad, &forward);
        conjugateProduct(mCorrSlice, mSplitBead, mNxPad, mNyPad);
        todfft(mCorrSlice, &mNxPad, &mNyPad, &inverse);
        //printArray( mCorrSlice, mNxpDim, mNxPad, mNyPad);
        profile("Correlated");
      
        // Write slice on last pass
        if (outfp && ipass == npass) {
          if (mCenterWeight)
            mWriteSlice = mFiltSlice;
          else {
            for (iy = 0; iy < mNyOut; iy++)
              for (ix = 0; ix < mNxOut; ix++)
                mWriteSlice[ix + mNxOut * iy] = 
                  mCorrSlice[ix + mNxpDim * iy - (mNxPad - mNxOut) / 2];
          }
          if (mrc_write_slice(mWriteSlice, outfp, &outhead, indz - izStart, 'z'))
            exitError("Writing filtered image for section %d", iz);
          for (iy = 0; iy < mNyOut; iy++) {
            tsum = 0.;
            for (ix = 0; ix < mNxOut; ix++) {
              val = mWriteSlice[ix + iy * mNxOut];
              tsum += val;
              outhead.amin = B3DMIN(outhead.amin, val);
              outhead.amax = B3DMAX(outhead.amax, val);
            }
            outhead.amean += tsum / ((float)mNxOut * mNyOut);
          }
        }
            
        // Search for all peaks in the correlation
        searchCorrelationPeaks(sl, iz);
        profile("found peaks");

        if (!mUseSliceCache || kernelSigma > 0.)
          sliceFree(sl);
      }
      listStart[izEnd - izStart] = mNumPeaks;

      // Determine a scaling of peaks by background intensities
      analyzeBackgroundGroups();
      profile("Analyzed groups");

      // Normalize peak values and eliminate ones below minimum: zero out ccc
      for (j = 0; j < mNumPeaks; j++) {
        mPeakList[j].peak /= mPeakMax;
        if (mPeakList[j].ccc && mPeakList[j].peak < mMinRelativePeak) {
          mPeakList[j].ccc = 0.;
          numBelowMin++;
        }
      }

      critsq = mMinDist * mMinDist;

      // Eliminate peaks that are too close by zeroing out ccc
      for (indz = 0; indz < izEnd - izStart; indz++) {
        for (i = listStart[indz]; i < listStart[indz + 1]; i++) {
          if (!mPeakList[i].ccc)
            continue;
          xcen = mPeakList[i].xcen;
          ycen = mPeakList[i].ycen;
          jstr = i - 1;
          jend = listStart[indz];
          for (jdir = -1; jdir <= 1 && mPeakList[i].ccc; jdir += 2) {
            for (j = jstr; j * jdir <= jend * jdir; j += jdir) {
              dy = mPeakList[j].ycen - ycen;
              if (mPeakList[j].ccc && jdir * dy > mMinDist)
                break;
              if (!mPeakList[j].ccc)
                continue;
              dx = mPeakList[j].xcen - xcen;
              if (dx >= -mMinDist && dx <= mMinDist) {
                distsq = dx * dx + dy * dy;
                if (distsq <= critsq) {
                  numEliminated++;

                  // Found a peak too close
                  // Eliminate current peak and break out of loop if it is 
                  // weaker
                  if (mPeakList[j].ccc > mPeakList[i].ccc) {
                    mPeakList[i].ccc = 0.;
                    break;
                  } else

                    // Otherwise eliminate the other peak and continue
                    mPeakList[j].ccc = 0.;
                }
              }
            }
            jstr = i + 1;
            jend = listStart[indz + 1] - 1;
          }
        }
      }
      profile("Eliminated close points");

      if (npass > 1) 
        printf("Pass %d: ", ipass);
      numPeaksLeft = mNumPeaks - numEliminated - numBelowMin;
      printf("%d peaks found. %d eliminated as too weak, %d too close, %d "
             "remaining\n", mNumPeaks, numBelowMin, numEliminated, numPeaksLeft);

      // If doing two passes, now average the beads
      if (ipass < npass) {
        averageBeads(izStart, izEnd);
        if (mAdjustSizes && ipass == 1 && B3DMAX(sizeOrig, mBeadSize) / 
            B3DMIN(sizeOrig, mBeadSize) >= mMinSizeChangeForRedo) {
          printf("Adding another pass because size changed more than %.0f%%\n",
                 100. * (mMinSizeChangeForRedo - 1.));
          npass++;
        }
        profile("Averaged");
      }
    }
    
    // Done with the cached slices now
    if (mUseSliceCache)
      for (indz = izStart; indz < izEnd; indz++)
        sliceFree(mCachedSlices[indz - izStart]);

    // After first set of sections, stop adjusting sizes
    mAdjustSizes = 0;

    ixst = izStart;
    izStart = izEnd;
    if (!numPeaksLeft)
      continue;
  
    // Determine automatic threshold for putting points out
    threshUse = mPeakThresh;
    if (mPeakThresh <= 0.) {
      threshUse = findStorageThreshold(histDip);
    }

    // Eliminate duplicates from original objects by brute force
    numEliminated = 0;
    for (ob = 0; ob < mNumObjOrig; ob++) {
      if (iobjClose(imod->obj[ob].flags))
        continue;
      for (indz = ixst; indz < izEnd; indz++) {
        iz = mZlist[indz];
        
        for (co = 0; co < imod->obj[ob].contsize; co++) {
          cont = &imod->obj[ob].cont[co];
          for (pt = 0; pt < cont->psize; pt++) {
            if (B3DNINT(cont->pts[pt].z) != iz)
              continue;
            xcen = cont->pts[pt].x;
            ycen = cont->pts[pt].y;
            for (i = listStart[indz]; i < listStart[indz + 1]; i++) {
              if (!mPeakList[i].ccc)
                continue;
              dy = mPeakList[i].ycen - ycen;
              if (dy < -mMinDist || dy > mMinDist)
                continue;
              dx = mPeakList[i].xcen - xcen;
              if (dx < -mMinDist || dx > mMinDist)
                continue;
              distsq = dx * dx + dy * dy;
              if (distsq <= critsq) {
                numEliminated++;
                mPeakList[i].ccc = 0.;
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
    for (i = 0; i < mNumPeaks; i++)
      if (mPeakList[i].ccc && (mPeakList[i].peak >= threshUse))
        numToSave++;
    if (mPeakThresh >0)
      printf("%d peaks above threshold of %.3f are being stored in model\n",
             numToSave, mPeakThresh);
    if (!numToSave)
      exitError("There are no peaks available for saving");

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
    for (i = 0; i < mNumPeaks; i++) {
      if (!mPeakList[i].ccc || (mPeakList[i].peak < threshUse))
        continue;
      pnt.x = mPeakList[i].xcen;
      pnt.y = mPeakList[i].ycen;
      pnt.z = mPeakList[i].iz;
      if (!imodPointAppend(&obj->cont[ix], &pnt))
        exitError("Adding point to contour");

      store.value.f = mPeakList[i].peak;
      store.flags = GEN_STORE_FLOAT << 2;
      store.type = GEN_STORE_VALUE1;
      store.index.i = ix++;
      if (istoreInsert(&obj->store, &store))
        exitError("Could not add general storage item");
      cccMin = B3DMIN(cccMin, mPeakList[i].peak);
      cccMax = B3DMAX(cccMax, mPeakList[i].peak);
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
    iiFClose(outfp);
  }
  iiFClose(mInFp);
  if (mDumpFp)
    fclose(mDumpFp);

  // Finish model
  if (imod->objsize) {
    imod->xmax = mNxIn;
    imod->ymax = mNyIn;
    imod->zmax = mInHead.nz;
    
    imodSetRefImage(imod, &mInHead);
    
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
  qsort(&mPeakList[0], mNumPeaks, sizeof(PeakEntry), comparePeaks);
  for (i = 0; i < mNumPeaks; i++) {
    mPeakList[i].integral = -1.;
    if (mPeakList[i].ccc) {
      pnt.x = mPeakList[i].xcen;
      pnt.y = mPeakList[i].ycen;
      pnt.z = mPeakList[i].iz;
      if (pointInsideBoundary(obj, &pnt)) {
        mPeakList[i].integral = 0;
        for (ob = 0; ob < refmod->objsize && !mPeakList[i].integral; ob++) {
          if (ob == boundObj)
            continue;
          for (co = 0; co < refmod->obj[ob].contsize && !mPeakList[i].integral;
               co++) {
            cont = &refmod->obj[ob].cont[co];
            for (pt = 0; pt < cont->psize; pt++) {
              if (B3DNINT(cont->pts[pt].z) == mPeakList[i].iz && 
                  imodPointDistance(&cont->pts[pt], &pnt) < mMatchCrit) {
                mPeakList[i].integral = 1.;
                break;
              }
            }
          }
        }
        if (mVkeys && strchr(mVkeys, 'g'))
          printf("Peak: %d %.4f %.4f %f %f %f %f\n", 
                 B3DNINT(mPeakList[i].integral),
                 mPeakList[i].peak, mPeakList[i].peak * mPeakList[i].ccc,
                 mPeakList[i].cenmean - mPeakList[i].annmean, 
                 mPeakList[i].cenmean, mPeakList[i].annmean, mPeakList[i].median);
      }
    }
  }
  
  // Walk backwards through list and count up not matched and matched and
  // find place with minimum total error
  // TODO: histDip could be -1 from above with fallback?
  errMin = numRefPts + 1;
  lastPeak = 1.0;
  for (i = mNumPeaks - 1; i >= 0; i--) {
    if (mPeakList[i].integral < 0.)
      continue;
    error = numRefPts + numUnmatched - numMatched;
    if (lastPeak >= histDip && mPeakList[i].peak < histDip)
      printf("Threshold criterion of %.3f has error %d, %d fn and %d fp\n",
             histDip, B3DNINT(error), numRefPts - numMatched, numUnmatched);
    if (error < errMin) {
      errMin = error;
      peakAbove = lastPeak;
      peakBelow = mPeakList[i].peak;
      numPeakPts = numMatched + numUnmatched;
      numPeakMatch = numMatched;
    }
    lastPeak = mPeakList[i].peak;
    if (mPeakList[i].integral > 0.)
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
 * Computes the sizes for integrals and sobel filtering and allocates arrays that depend
 * on bead size
 */
void FindBeads::setupSizeDependentVars()
{
  int diff, mindiff, range, size, nxtmp, nytmp, minsize;
  float xtmp, ytmp;

  // Get radii for the integral
  mRadCenter = B3DMAX(1., 0.34 * mBeadSize);
  mRadInner = B3DMAX(mRadCenter + 1., 0.5 * mBeadSize + B3DMAX(1., 0.1 * mBeadSize));
  mRadOuter = mRadInner + B3DMAX(2., 0.2 * mBeadSize);
  mMinDist = mMinSpacing * mBeadSize;
  mMatchCrit = B3DMAX(0.2 * mBeadSize, 2.);

  // Get size for scaled slices
  mScaleFactor = mBeadSize / mScaledSize;
  scaledSobel(NULL, mNxIn, mNyIn, mScaleFactor, mMinInterp, mLinearInterp, -1., NULL, 
              &mNxOut, &mNyOut, &mXoffset, &mYoffset);

  if (mVkeys)
    printf("mNxOut %d  mNyOut %d mXoffset %f mYoffset %f mScaleFactor %f\n", 
           mNxOut, mNyOut, mXoffset, mYoffset, mScaleFactor);

  // Get the full box size, find the size that best matches the specified 
  // scaled size and revise it
  mBoxScaled = mBoxScaledOrig;
  mBoxSize = 2 * (B3DNINT(mBoxScaled * mScaleFactor) / 2);
  mindiff = 100000;
  range = (int)(2. * mScaleFactor);
  for (size = mBoxSize + 2 * range; size >= mBoxSize - 2 * range; size -= 2) {
    if (size <= 3)
      break;
    scaledSobel(NULL, size, size, mScaleFactor, mMinInterp, mLinearInterp, -1., NULL,
                &nxtmp, &nytmp, &xtmp, &ytmp);
    diff = nxtmp > mBoxScaled ? nxtmp - mBoxScaled : mBoxScaled - nxtmp;
    if (diff < mindiff) {
      mindiff = diff;
      minsize = size;
    }
  }
  mBoxSize = minsize;
  scaledSobel(NULL, mBoxSize, mBoxSize, mScaleFactor, mMinInterp, mLinearInterp, -1., 
              NULL, &mBoxScaled, &nytmp, &xtmp, &ytmp);

  // Need padded size for arrays being transformed
  mNxPad = niceFrame(mNxOut, 2, 19);
  mNyPad = niceFrame(mNyOut, 2, 19);
  mNxpDim = mNxPad + 2;
  if (mVkeys)
    printf("mNxPad %d  mNyPad %d mBoxSize %d boxScaled %d\n", mNxPad, mNyPad,
           mBoxSize, mBoxScaled);
  printf("Scaling down by %.2f for Sobel filter; box size = %d, scaled size "
         "= %d\n", mScaleFactor, mBoxSize, mBoxScaled);

  // Get memory for filtered slice, synthetic bead and scaled bead
  B3DFREE(mFiltBead)
  B3DFREE(mFiltSlice);
  B3DFREE(mCorrSlice);
  B3DFREE(mFullBead);
  B3DFREE(mOneBead);
  B3DFREE(mSplitBead);
  mFiltBead = B3DMALLOC(float, mBoxScaled * mBoxScaled);
  mFiltSlice = B3DMALLOC(float, mNxOut * mNyOut);
  mCorrSlice = B3DMALLOC(float, mNxpDim * mNyPad);
  mFullBead = B3DMALLOC(float, mBoxSize * mBoxSize);
  mOneBead = B3DMALLOC(float, mBoxSize * mBoxSize);
  mSplitBead = B3DMALLOC(float, mNxpDim * mNyPad);
  if (!mCenterWeight) {
    B3DFREE(mWriteSlice);
    mWriteSlice = B3DMALLOC(float, mNxOut * mNyOut);
  }

  if (!mFiltBead || !mFiltSlice || !mFullBead || !mSplitBead || !mCorrSlice || 
      !mOneBead || (!mCenterWeight && !mWriteSlice))
    exitError("Failed to get memory for an image array");
}

/*
 * Examines each point to see if it is a correlation peak, gets peak strength, ccc, 
 * integral and annulus properties and stores peaks
 */
void FindBeads::searchCorrelationPeaks(Islice *sl, int iz)
{
  int iy, ix, ixst, ixnd, iyst, iynd, ind, ixofs, iyofs, co;
  float cval, cx, cy, xcen, ycen, integral, ccc, cenmean, annmean, median;

  // Limit range by prealignment shift
  ixst = (mNxPad - mNxOut) / 2 + 1;
  ixnd = ixst + mNxOut - 2;
  iyst = (mNyPad - mNyOut) / 2 + 1;
  iynd = iyst + mNyOut - 2;
  if (mAlignXshift) {
    ixst += (int)ceil(B3DMAX(0., mAlignXshift[iz]) / mScaleFactor);
    ixnd -= (int)ceil(B3DMAX(0., -mAlignXshift[iz]) / mScaleFactor);
    iyst += (int)ceil(B3DMAX(0., mAlignYshift[iz]) / mScaleFactor);
    iynd -= (int)ceil(B3DMAX(0., -mAlignYshift[iz]) / mScaleFactor);
  }
  for (iy = iyst; iy < iynd; iy++) {
    for (ix = ixst; ix < ixnd; ix++) {
      ind = ix + iy * (mNxpDim);
      cval = mCorrSlice[ind];
      if (mCorrSlice[ind - 1] < cval && mCorrSlice[ind + 1] <= cval &&
          mCorrSlice[ind - mNxpDim] < cval && mCorrSlice[ind + mNxpDim] <= cval &&
          mCorrSlice[ind - 1 - mNxpDim] < cval && mCorrSlice[ind + 1 + mNxpDim] < cval && 
          mCorrSlice[ind + 1 - mNxpDim] < cval && mCorrSlice[ind - 1 + mNxpDim] < cval) {
        cx = parabolicFitPosition(mCorrSlice[ind - 1], cval, mCorrSlice[ind + 1]);
        cy = parabolicFitPosition(mCorrSlice[ind - mNxpDim], cval,
                                  mCorrSlice[ind + mNxpDim]);

        // integer offset in scaled, filtered image
        ixofs = ix - (mNxPad - mNxOut) / 2;
        iyofs = iy - (mNyPad - mNyOut) / 2;

        // Center of feature in full original image
        xcen = (ixofs + cx + mBeadCenOfs) * mScaleFactor + mXoffset;
        ycen = (iyofs + cy + mBeadCenOfs) * mScaleFactor + mYoffset;
        if (mNumAreaCont) {
          co = imodPointInsideArea(&mAreaMod->obj[0], mAreaConts, mNumAreaCont, xcen,
                                   ycen);
          if ((co < 0 && !mExcludeAreas) || (co >= 0 && mExcludeAreas))
            continue;
        }

        // First validate the peak by polarity of density in full image
        integral = (float)beadIntegral(sl->data.f, mNxIn, mNxIn, mNyIn, mRadCenter, 
                                       mRadInner, mRadOuter, xcen, ycen, &cenmean,
                                       &annmean, mKernHist, mAnnulusPctile, &median);
        if (! mLightBeads)
          integral = -integral;
        if (integral > 0.) {

          // Good, then get a CCC and add to list
          ccc = templateCCCoefficient(mFiltSlice, mNxOut, mNxOut, mNyOut, mFiltBead,
                                      mBoxScaled, mBoxScaled, mBoxScaled,
                                      ixofs - mBoxScaled / 2, iyofs - mBoxScaled / 2);
          //if (ipass == npass)
          /* printf("peak ix %d iy %d xcen %.2f ycen %.2f integral %f  peak %f ccc "
             "%.3f\n", ix, iy, xcen, ycen, integral, cval, ccc);*/
          if (ccc > 0.) {
            if (mNumPeaks >= mListSize) {
              if (!mListSize)
                mPeakList = B3DMALLOC(PeakEntry, 10000);
              else
                B3DREALLOC(mPeakList, PeakEntry, mListSize + 10000);
              if (!mPeakList)
                exitError("Failed to get memory for peak list");
              mListSize += 10000;
            }
            mPeakList[mNumPeaks].xcen = xcen;
            mPeakList[mNumPeaks].ycen = ycen;
            mPeakList[mNumPeaks].iz = iz;
            mPeakList[mNumPeaks].ccc = ccc;
            mPeakList[mNumPeaks].peak = cval;
            if (mMeasureToUse == 1)
              mPeakList[mNumPeaks].peak = integral;
            if (mMeasureToUse == 2)
              mPeakList[mNumPeaks].peak =
                (float)sqrt((double)B3DMAX(cval * integral, 0.));
            mPeakList[mNumPeaks].cenmean = cenmean;
            mPeakList[mNumPeaks].annmean = annmean;
            mPeakList[mNumPeaks].integral = integral;
            mPeakList[mNumPeaks].median = median;
            mPeakMax = B3DMAX(mPeakMax, mPeakList[mNumPeaks].peak);
            mNumPeaks++;
          }
        }
      }
    }
  }
}

/*
 * Divides points into multiple groups if possible based on annulus medians and tries to 
 * find a dip in each histogram, then fits a line to the dips and uses this to adjust 
 * all the peak strengths to a common basis
 */
void FindBeads::analyzeBackgroundGroups()
{
  float *meanMedPtr;
  float annMin, annMax, cumStart, cumul, target;
  int verbose, ninHist, lowerInd, ndat, igr, ninSel, ind, j;
  int minInGroup = 100;
  float maxBalRange = 4.;
  float lowerLim, upperLim, peakAbove, peakBelow, dxbin, val;
  double sum;
  float selPeakMin, selPeakMax, selSlope, selIntcp, modeSlope, modeIntcp, xtmp;
  float annMidval[MAX_GROUPS], annDip[MAX_GROUPS], annPeakAbove[MAX_GROUPS];
  float annUseMin, annUseMax, dip;

  // Adjust for a trend in peak strength with background mean
  // Start with histogram of annular means and set up groups based on this
  meanMedPtr = &mPeakList[0].annmean;
  if (mAnnulusPctile >= 0.)
    meanMedPtr = &mPeakList[0].median;
  selectedMinMax(meanMedPtr, NULL, 0., 0., annMin, annMax, ninHist);
  kernelHistoPL(meanMedPtr, 1, NULL, 0., 0., 
                mRegHist, MAX_BINS, annMin, annMax, 0., 0, NULL);
  if (mVkeys)
    printf("min %f max %f ninhist %d\n", annMin, annMax, ninHist);
  mNumGroups = B3DMIN(mNumGroups, ninHist / minInGroup);
  dxbin = (annMax - annMin) / MAX_BINS;
  if (mNumGroups > 1) {
    lowerInd = 0;
    cumStart = 0.;
    ndat = 0;

    // For each group, find upper limit as place where hist reaches target
    for (igr = 0; igr < mNumGroups; igr++) {
      target = ((igr + 1.) * ninHist) / mNumGroups;
      sum = 0.;
      ind = lowerInd; 
      cumul = cumStart;
      while (ind < MAX_BINS && (cumul + mRegHist[ind] < target || 
                                ind == MAX_BINS - 1)) {
        sum += ((ind + 0.5) * dxbin + annMin) * mRegHist[ind];
        cumul += mRegHist[ind++];
      }

      // Get a selected histogram of the peaks
      lowerLim = lowerInd * dxbin + annMin;
      upperLim = ind * dxbin + annMin;
      selectedMinMax(&mPeakList[0].peak, meanMedPtr, lowerLim, upperLim, selPeakMin,
                     selPeakMax, ninSel);
      selPeakMin = mMinRelativePeak * selPeakMax;
      verbose = 0;
      if (mVkeys && strchr(mVkeys, 'P')) {
        verbose = 1;
        printf("Peak:  selected data set %d\n", igr + 1);
      } else if (mVkeys && strchr(mVkeys, 'H'))
        verbose = 2;
      kernelHistoPL(&mPeakList[0].peak, 1, meanMedPtr, lowerLim,
                    upperLim, mKernHist, MAX_BINS, selPeakMin, selPeakMax, 
                    0.1 * selPeakMax, verbose, mDumpFp);
      if (mDumpFp)
        printf("Type %2d:  Selected histogram for group %d\n", mDumpType++, igr);

      if (scanHistogram(mKernHist, MAX_BINS, selPeakMin, selPeakMax,
                        selPeakMin, selPeakMax, 1, &dip, &peakBelow,
                        &peakAbove)) {
          
        if (mVkeys)
          printf("No histogram dip: annular mean %.2f to %.2f  peaks %.2f "
                 "to %.2f\n", lowerLim, upperLim, selPeakMin, selPeakMax);
      } else {
        annMidval[ndat] = sum / (cumul - cumStart);
        if (!ndat)
          annUseMin = 2. * annMidval[ndat] - upperLim;
        annUseMax = 2. * annMidval[ndat] - lowerLim;
        annDip[ndat] = dip;
        annPeakAbove[ndat] = peakAbove;
        if (mVkeys)
          printf("%.2f  %.2f  %.2f %.2f %.2f %.2f %.2f\n", lowerLim, upperLim,
                 annMidval[ndat], selPeakMin, selPeakMax, dip, peakAbove);
        ndat++;
      }
      lowerInd = ind;
      cumStart = cumul;

      /*
      // Find the needed percentile point in here
      ind = 0;
      cumul = 0;
      target = selectPctile * ninSel;
      while (ind < MAX_BINS && (cumul + mKernHist[ind] < target || 
      ind == MAX_BINS - 1))
      cumul += mKernHist[ind++];
      annPctPeak[igr] = ind * (selPeakMax - selPeakMin) / MAX_BINS +
      selPeakMin; */

    }

    if (ndat > 1) {
      // Fit a line to the points and scale the peaks, find new max
      lsFit(annMidval, annPeakAbove, ndat, &modeSlope, &modeIntcp, &xtmp);
      lsFit(annMidval, annDip, ndat, &selSlope, &selIntcp, &xtmp);
      printf("Dips found in %d groups based on bkg mean, means %.5g to %.5g\n",
             ndat, annMidval[0], annMidval[ndat - 1]);
      if (mVkeys)
        printf(" Fit of mode vs background has slope %f, intcp %f\n"
               " Fit of dip vs background has slope %f, intcp %f\n",
               modeSlope, modeIntcp, selSlope, selIntcp);

      if ((!mLightBeads && selSlope > 0.) || (mLightBeads && selSlope < 0.)) {
        if (mLightBeads) {
          val = ((annUseMin * selSlope + selIntcp) / maxBalRange - 
                 selIntcp) / selSlope;
          annUseMax = B3DMIN(val, annUseMax);
        } else {
          val = ((annUseMax * selSlope + selIntcp) / maxBalRange - 
                 selIntcp) / selSlope;
          annUseMin = B3DMAX(val, annUseMin);
        }
        if (mVkeys)
          printf("Adjusting for background limited to %.2f to %.2f\n",
                 annUseMin, annUseMax);
        mPeakMax = -1.e30;
        for (j = 0; j < mNumPeaks; j++) {
          if (!mPeakList[j].ccc)
            continue;
          val = *(&mPeakList[j].peak + (meanMedPtr - &mPeakList[0].peak));
          val = B3DMAX(annUseMin, B3DMIN(annUseMax, val));
          mPeakList[j].peak /= (val * selSlope + selIntcp);
          mPeakMax = B3DMAX(mPeakMax, mPeakList[j].peak);
        }
      }
    }
  }
}

/*
 * Finds the threshold for averaging and then average the beads above threshold
 */
void FindBeads::averageBeads(int izStart, int izEnd)
{
  int numStart, i, j, nsum, loaded, iz, indz, oldBox, offset;
  float threshUse, histDip, peakAbove, peakBelow, newDiam;
  float amat[2][2];
  float *saveBead;
  Islice *sl;
  //std::vector<float> diameters;

  numStart = 0;
  threshUse = mThreshold;
  if (mThreshold > 1.) {
    if (mNumPeaks > 1) 
      qsort(&mPeakList[0], mNumPeaks, sizeof(PeakEntry), comparePeaks);
    numStart = B3DMAX(0, mNumPeaks - (int)mThreshold);
  } else if (mThreshold < 0.) {

    // Negative threshold: find dip
    if (mDumpFp)
      printf("Dumping histograms for finding threshold for averaging:\n");
    if (findHistoDipPL(histDip, peakBelow, peakAbove, NULL)) {
      if (mAverageFallback <= 0 || mNumPeaks < 2)
        exitError("Failed to find dip in smoothed histogram of peaks");
      printf("Failed to find dip in histogram; using fallback threshold of %d for "
             "averaging\n", mAverageFallback);
      qsort(&mPeakList[0], mNumPeaks, sizeof(PeakEntry), comparePeaks);
      numStart = B3DMAX(0, mNumPeaks - mAverageFallback);
      threshUse = 2.;
    } else {
            
      // Set original automatic threshold at 1/4 way from dip to peak, or
      // -mThreshold as the fraction above dip to take
      threshUse = 0.75 * histDip + 0.25 * peakAbove;
      if (mThreshold >= -1.) {
        if (mNumPeaks > 1) 
          qsort(&mPeakList[0], mNumPeaks, sizeof(PeakEntry), comparePeaks);
        for (j = 0; j < mNumPeaks; j++)
          if (mPeakList[j].ccc && mPeakList[j].peak >= histDip)
            break;
        numStart = mNumPeaks + B3DNINT(mThreshold * (mNumPeaks - j));
        numStart = B3DMAX(0, B3DMIN(mNumPeaks - 1, numStart));
        threshUse = mPeakList[numStart].peak;
      }
      printf("Threshold for averaging set to %.3f\n", threshUse);
    }
  }
  profile("Found threshold");

  for (i = 0; i < mBoxSize * mBoxSize; i++)
    mFullBead[i] = 0.;
  nsum = 0;
  amat[0][0] = amat[1][1] = 1.;
  amat[0][1] = amat[1][0] = 0.;
    
  // Loop through images again
  for (indz = izStart; indz < izEnd; indz++) {
    iz = mZlist[indz];
    loaded = 0;
    for (j = numStart; j < mNumPeaks; j++) {
      if (mPeakList[j].ccc && mPeakList[j].iz == iz && 
          (threshUse > 1. || mPeakList[j].peak >= threshUse)) {
        if (!loaded) {
          if (mUseSliceCache)
            sl = mCachedSlices[indz - izStart];
          else
            sl = readSliceAsFloat(iz);
        }
        loaded = 1;
          
        // Interpolate the bead into center of array, add it to sum
        cubinterp(sl->data.f, mOneBead, mNxIn, mNyIn, mBoxSize, mBoxSize, 
                  amat, mPeakList[j].xcen, mPeakList[j].ycen, 0., 0., 1.,
                  mInHead.amean, B3DMAX(0, mLinearInterp));
        nsum++;
        for (i = 0; i < mBoxSize * mBoxSize; i++)
          mFullBead[i] += mOneBead[i];
        //diameters.push_back(extractDiameter(mOneBead));
      }
    }
    if (loaded && !mUseSliceCache)
      sliceFree(sl);
  }
  if (nsum) {
    for (i = 0; i < mBoxSize * mBoxSize; i++)
      mFullBead[i] /= nsum;

    newDiam = extractDiameter(mFullBead);
    printf("Diameter of average bead at zero-crossing = %.2f\n", newDiam);
    if (mAdjustSizes) {
      if (B3DMAX(newDiam, mBeadSize) / B3DMIN(newDiam, mBeadSize) > mMaxAdjustFactor) {
        printf("WARNING: imodfindbeads - Measured bead diameter differs too much "
               "from specified size to be plausible, so bead size will not be "
               "adjusted\n");
      } else {

        // autofindseed is looking for "Adjusted parameters" at start of line
        printf("Adjusted parameters based on a new bead size of %.2f\n", newDiam);
        
        // Save the bead, remake all the arrays, and copy average into new array with 
        // trimming or padding
        oldBox = mBoxSize;
        saveBead = B3DMALLOC(float, oldBox * oldBox);
        if (!saveBead)
          exitError("Memory error making array to resize bead average");
        memcpy(saveBead, mFullBead, oldBox * oldBox * sizeof(float));
        mBeadSize = newDiam;
        setupSizeDependentVars();
        if (oldBox >= mBoxSize) {
          offset = (oldBox - mBoxSize) / 2;
          for (j = 0; j < mBoxSize; j++)
            for (i = 0; i < mBoxSize; i++)
              mFullBead[i + j * mBoxSize] = saveBead[i + offset + (j + offset) *oldBox];
        } else {
          sliceTaperOutPad(saveBead, SLICE_MODE_FLOAT, oldBox, oldBox, mFullBead,
                           mBoxSize, mBoxSize, mBoxSize, 0, 0.);
        }
        free(saveBead);
      }
    }
    /* float median;
       rsSortFloats(diameters.data(), diameters.size());
       rsMedianOfSorted(diameters.data(), diameters.size(), &median);
       printf("median of individual diameters = %.2f\n", median); */
  }
}

/*
 * Finds the diameter of an average or single bead
 */
float FindBeads::extractDiameter(float *oneBead)
{
  float edge = sliceEdgeMean(mFullBead, mBoxSize, 0, mBoxSize - 1, 0, mBoxSize - 1);
  float polarity = (mLightBeads ? 1. : -1.);
  float diams[100], avgden[100], xx[100], yy[100];
  float grad, maxGrad = 0.;
  int i, ifirst = (int)(0.2  * mBeadSize), ndat = 0;
  int indMax, nfit, ind, idir, idiff;
  float slope, intcp, ro, zeroCross;
    
  // Find mean in a series of rings; loop on all pixels that could be in a ring and test
  // each one against inner and outer radii
  for (i = ifirst; i < B3DNINT(0.9 * mBeadSize); i++) {
    int jx,jy, nsum = 0;
    float bsum = 0., bsumsq = 0.;
    float rad = i;
    for (jy = mBoxSize / 2 - i - 2; jy <= mBoxSize / 2 + i + 2; jy++) {
      for (jx = mBoxSize / 2 - i - 2; jx <= mBoxSize / 2 + i + 2; jx++) {
        float dx = jx - (mBoxSize - 1) / 2.;
        float dy = jy - (mBoxSize - 1) / 2.;
        float radsq = dx * dx + dy *dy;
        if (radsq >= rad * rad && radsq <= (rad + 1.) * (rad + 1.)) {
          radsq = polarity * (mFullBead[jx + mBoxSize * jy] - edge);
          nsum++;
          bsum += radsq;
          bsumsq += radsq * radsq;
        }
      }
    }
    diams[ndat] = 2. * rad + 1.;
    avgden[ndat] = bsum / nsum;

    // Keep track of the point with the maximum gradient prior to it
    if (ndat) {
      grad = avgden[ndat - 1] - avgden[ndat];
      if (grad > maxGrad) {
        maxGrad = grad;
        indMax = ndat;
      }
    }
    if (mVkeys)
      printf("%.2f  %.2f  %.2f  %.2f\n", diams[ndat], avgden[ndat], grad, maxGrad);
    ndat++;
  }
  
  // Fit to ones around maximum gradient with a gradient nearly as big
  nfit = 2;
  xx[0] = diams[indMax];
  yy[0] = avgden[indMax];
  xx[1] = diams[indMax - 1];
  yy[1] = avgden[indMax - 1];
  for (idir = -1; idir <= 1; idir += 2) {
    for (idiff = 1; idiff < 100; idiff++) {
      ind = indMax + idir * idiff;
      if (ind < 1 || ind > ndat)
        break;
      grad = avgden[ind - 1] - avgden[ind];
      if (grad < 0.75 * maxGrad)
        break;
      if (idir < 0)
        ind--;
      xx[nfit] = diams[ind];
      yy[nfit] = avgden[ind];
    }
  }
  lsFit(xx, yy, nfit, &slope, &intcp, &ro);
  zeroCross = -intcp / slope;
  return zeroCross;
  
  // Forget this, it gives too big a diameter for cryo with overshoots
  // Fit up to 5 points past the zero crossing
  /* float baseSlope, baseIntcp;
  indFit = 0;
  for (i = 1; i < ndat; i++) {
    if (diams[i] >= zeroCross) {
      indFit = i;
      break;
    }
  }
  printf("Zerocross %.2f  nfit %d\n", zeroCross, nfit);
  nfit = B3DMIN(5, ndat - indFit);
  if (indFit < 0 || nfit < 3)
    return zeroCross;
  lsFit(&diams[indFit], &avgden[indFit], nfit, &baseSlope, &baseIntcp, &ro);
  return -(intcp - baseIntcp) / (slope - baseSlope); */
}

/*
 * Finds the threshold for storing peaks in the model
 */
float FindBeads::findStorageThreshold(float &histDip)
{
  float threshUse = -10000.;
  float peakBelow, peakAbove, val, dxbin, meanAbove, sdAbove;
  int nsum, j, jstr, jend, jdir, numStart;
  double sum, sumsq;

  histDip = -1.;
  if (mDumpFp)
    printf("Dumping histograms for finding threshold for output points:\n");
  if (!findHistoDipPL(histDip, peakBelow, peakAbove, mVkeys)) {
    dxbin = 1. / MAX_BINS;
    
    // Count points above threshold
    jstr = histDip / dxbin;
    nsum = 0;
    sum = 0.;
    sumsq = 0.;
    for (j = jstr; j < MAX_BINS; j++) {
      nsum += B3DNINT(mRegHist[j]);
      val = (j + 0.5) * dxbin;
      sum += mRegHist[j] * val;
      sumsq += mRegHist[j] * val * val;
    }

    // estimate mean and SD for points above threshold
    if (!mPeakThresh) {
      sdAbove = 0.;
      meanAbove = sum / B3DMAX(1, nsum);
      if (nsum > 1) {
        val = (sumsq - nsum * meanAbove * meanAbove) / (nsum - 1.);
        sdAbove = (float)sqrt((double)(B3DMAX(val, 0.)));
      }
      jend = (meanAbove - 5. * sdAbove) / dxbin;
      jdir = 0;
      for (j = jstr - 1; j >= 0; j--) {
        jdir += B3DNINT(mRegHist[j]);
        if (jdir > nsum || (j < jend && jdir > nsum / 10))
          break;
      }
    
      threshUse = j * dxbin;

      // autofidseed wants to see " peaks are above" or "using fallback"
      // on the next to last line and 'total peaks being' on the last line
      printf("%d peaks are above threshold of %.3f\n"
             "%d more peaks %s stored in model down to value of %.3f\n", nsum, histDip,
             jdir, mNumObjOrig ? "would be" : "being", threshUse);
    } else {

      // Or find threshold given a relative number
      jend = -B3DNINT(mPeakThresh * nsum);
      jdir = 0;
      for (j = MAX_BINS - 1; j >= 0; j--) {
        jdir += B3DNINT(mRegHist[j]);
        if (jdir >= jend)
          break;
      }
      threshUse = B3DMAX(0., j * dxbin);
      printf("%d peaks are above histogram dip at %.3f\n"
             "%d total peaks %s stored in model down to value of %.3f\n", nsum, histDip,
             jdir, mNumObjOrig ? "would be" : "being", threshUse);
    }

  } else if (mStorageFallback > 0 && mNumPeaks > 1) {
    qsort(&mPeakList[0], mNumPeaks, sizeof(PeakEntry), comparePeaks);
    numStart = B3DMAX(0, mNumPeaks - mStorageFallback);
    threshUse = mPeakList[numStart].peak;
    printf("Failed to find dip in histogram, using fallback threshold for storing "
           "points\n");
    printf("%d total peaks being stored in model down to value of %.3f\n", 
           mNumPeaks - numStart, threshUse);

  } else {

    // Autofidseed wants to see 'Failed to find dip' on the last line of output
    printf("Failed to find dip in histogram\n");
  }
  return threshUse;
}

/*
 * Calls makeAreaContList for a section and checks and responds to errors
 */
void FindBeads::areaContListCheckErr(int iz)
{
  int ix;
  ix = makeAreaContList(&mAreaMod->obj[0], iz, mAreaConts, &mNumAreaCont, MAX_AREAS);
  if (ix < 0)
    exitError("Too many contours on one section in area model for array (limit %d)", 
              MAX_AREAS);
  if (ix > 0)
    exitError("No contours in object 1 of area model");
}

/*
 * Tests if pnt is inside one of the contours of obj; returns 1 if it is inside
 * 1 or if there are no contours on the point's section
 */
int FindBeads::pointInsideBoundary(Iobj *obj, Ipoint *pnt)
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
   original_image_coord = reduced_image_coord * mScaleFactor + mXoffset

   The bead is centered in the original model, regardless of whether it is
   odd or even.  
   Original bead center = mBoxSize / 2.

   original_bead_coord = reduced_bead_coord * mScaleFactor + xBeadOfs
   Reduced bead center = (mBoxSize / 2. - xBeadOfs)  / mScaleFactor

   With a centered bead in split image, peak = shift of bead to feature
   peak position = center of feature in reduced image
   If bead is offset positively from center, the bead needs to be shifted less
   to match a feature, so peak position is reduced
   feature_center = peak_position + bead_offset

   The mBoxScaled / 2 pixel is the one that ends up at the origin in the split
   filled image.  This is / 2 not / 2.  !
   Coordinate in reduced bead at origin = boxScale / 2
   Offset of bead from being centered in split image = 
   bead_offset = (mBoxSize / 2. - xBeadOfs)  / mScaleFactor - boxScale / 2

*/   


/*
 * Read a slice at iz in fle and convert it to a floating slice, then taper it
 * by 64 pixels at fill edges
 */
Islice *FindBeads::readSliceAsFloat(int iz)
{
  Islice *sl;

  sl = sliceReadFloat(&mInHead, iz);
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
float FindBeads::templateCCCoefficient(float *array, int nxdim, int nx, int ny, 
                                       float *templateIm, int nxtdim, int nxt, 
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
      bval = templateIm[ix - xoffset + (iy - yoffset) * nxtdim];
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
void FindBeads::kernelHistoPL(float *element, int skipZeroCCC, float *select,
                              float selMin, float selMax, float *bins, int numBins, 
                              float firstVal, float lastVal, float h, int verbose,
                              FILE *dumpFp)
{
  float dxbin, delta, val;
  int i, j, ist, ind;
  dxbin = (lastVal - firstVal) / numBins;
  for (i = 0; i < numBins; i++)
    bins[i] = 0.;
  for (j = 0; j < mNumPeaks; j++) {
    if (skipZeroCCC && !mPeakList[j].ccc)
      continue;
    if (select) {
      val = *(&mPeakList[j].peak + (select - &mPeakList[0].peak));
      if (val < selMin || val >= selMax)
        continue;
    }
    val = *(&mPeakList[j].peak + (element - &mPeakList[0].peak));

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
  if (dumpFp)
    for (i = 0; i < numBins; i++)
      fprintf(dumpFp, "%2d %.4f %f\n", mDumpType, firstVal + i * dxbin, bins[i]);
}

/*
 * Finds a histogram dip by starting with a high smoothing and dropping to
 * lower one.
 */
int FindBeads::findHistoDipPL(float &histDip, float &peakBelow,
                              float &peakAbove, char *vkeys)
{
  float coarseH = 0.2f;
  float fineH = 0.05f;
  int numCut = 4;
  float fracGuess = 0.5f;
  int i, ncum, numCrit, verbose;
  float upperLim = 1.0;

  // Build a regular histogram first and use minGuess to find safe upper limit
  verbose = (vkeys != NULL && strchr(vkeys, 'p')) ? 1 : 0;
  kernelHistoPL(&mPeakList[0].peak, 1, NULL, 0., 0.,
                mRegHist, MAX_BINS, 0., 1., 0., verbose, mDumpFp);
  if (mDumpFp)
    printf("Type %2d:  Regular histogram\n", mDumpType++);

  if (mMinGuess) {
    numCrit = B3DMAX(1, B3DNINT(mMinGuess * fracGuess));
    ncum = 0;
    for (i = MAX_BINS - 1; i > 10; i--) {
      ncum += B3DNINT(mRegHist[i]);
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
    kernelHistoPL(&mPeakList[0].peak, 1, NULL, 0., 0.,
                  mKernHist, MAX_BINS, 0., 1., coarseH, verbose, mDumpFp);
    if (mDumpFp)
      printf("Type %2d:  Kernel histogram with H = %.3f\n", mDumpType++, coarseH);

    // Cut H if it fails or if the top peak is at 1.0
    if (!scanHistogram(mKernHist, MAX_BINS, 0., 1., 0., upperLim, 1, &histDip,
                       &peakBelow, &peakAbove) && peakAbove < 0.999) 
      break;
    coarseH *= 0.707;
  }
  if (i == numCut)
    return 1;
  
  printf("Histogram smoothed with H = %.3f has dip at %.3f, peaks at %.3f"
         " and %.3f\n", coarseH, histDip, peakBelow, peakAbove);
  
  verbose = (vkeys != NULL && strchr(vkeys, 'f')) ? 2 : 0;
  kernelHistoPL(&mPeakList[0].peak, 1, NULL, 0., 0.,
                mKernHist, MAX_BINS, 0., 1., fineH, verbose, mDumpFp);
  if (mDumpFp)
    printf("Type %2d:  Kernel histogram with fine H = %.3f\n", mDumpType++, fineH);
  scanHistogram(mKernHist, MAX_BINS, 0., 1., 0.5 * (histDip + peakBelow),
                0.5 * (histDip + peakAbove), 0, &histDip, &peakBelow, &peakAbove);
  printf("Histogram smoothed with H = %.3f has lowest dip at %.3f\n", fineH, histDip);
  return 0;
}


void FindBeads::printArray(float *filtBead, int nxdim, int nx, int ny)
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
 * number in range are returned in minVal, maxVal, and ninRange.
 */
void FindBeads::selectedMinMax(float *element,
                               float *select, float selMin, float selMax,
                               float &minVal, float &maxVal, int &ninRange)
{
  float val;
  int j;
  minVal = 1.e30;
  maxVal = -1.e30;
  ninRange = 0;
  for (j = 0; j < mNumPeaks; j++) {
    if (!mPeakList[j].ccc)
      continue;
    if (select) {
      val = *(&mPeakList[j].peak + (select - &mPeakList[0].peak));
      if (val < selMin || val >= selMax)
        continue;
    }
    val = *(&mPeakList[j].peak + (element - &mPeakList[0].peak));
    minVal = B3DMIN(minVal, val);
    maxVal = B3DMAX(maxVal, val);
    ninRange++;
  }
}

void FindBeads::profile(const char *format, ...)
{
  if (!mProfiling)
    return;
  va_list args;
  double now = wallTime();
  printf("%10.3f %5.3f  ", now - mWallStart, now - mWallLast);
  mWallLast = now;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
  printf("\n");
}

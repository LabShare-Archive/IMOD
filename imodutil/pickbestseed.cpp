/*
 *  pickbestseed.c -- Pick beads for tracking seed
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
#include <string.h>
#include <math.h>
#include <vector>
#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"
#include <iostream>
using namespace std;
#include "pickbestseed.h"

#define PI 3.141593
#define MAXLINE 1000
#define DEG2RAD 0.0174532925

#define SET_CONTROL_FLOAT(a,b) case a: b = yy ; cout << #b << " set to " << yy << endl ; break
#define SET_CONTROL_INT(a,b) case a: b = B3DNINT(yy) ; cout << #b << " set to " << B3DNINT(yy) << endl ; break

/*
 * The shortest main in the West
 */
int main( int argc, char *argv[])
{
  PickSeeds pick;
  pick.main(argc, argv);
  exit(0);
}

/*
 * Constructor: just initialize parameters and options 
 */
PickSeeds::PickSeeds()
{
  mNumAreaCont = 0;
  mAreaMod = NULL;
  mAppendToSeed = 0;
  mPhase = 1;
  mExcludeAreas = 0;
  mVerbose = 0;
  mOverlapTarget = -1.;
  mDensPlotName = NULL;
  mHighestTilt = 0.;
  mNumWsum = 0;
  mWsumMean = 0.;
  mNumOuterMAD = 0;
  mOuterMADMean = 0.;
  
  // Parameters
  // 1: Deviation between points as fraction of bead diameter for tracks to be close
  mCloseDiamFrac = 0.5;
  // 2: Multiple of target spacing at which to exclude points from further searches
  mExcludeFac = 0.75;
  // 3: Width of rings for finding points when filling gaps, as fraction of target spacing
  mRingSpacingFac = 0.25;
  // 4: Number of rings to search
  mNumRings = 4;
  // 5: Maximum # of bead diameters separation for points to be considered clustered
  mClusterCrit = 2.;
  // 7: Scaling for both mElongForOverlap and mEdgeOutlierCrit
  mElongCritScaling = 1.;
  // 12: Criterion for edge SD values or elongations to be considered outliers
  mEdgeOutlierCrit = 2.24;
  // 16: Absolute threshold for elongation to be considered overlap
  mElongForOverlap = 3.2;
  // 17: Option flags, 1 = divide edgeSD by outerMAD, 2 = equalize over wsum
  mOptionFlags = 0;
  // 18: Angle for rotating edgeSdmean vs. edgeSDsd
  mEdgeComboAngle = -59.;
  // 19: Angle for rotating elongMean vs. elongSD
  mElongComboAngle = -67.;
  // 20: Angle for rotating normalized edgeSD and elongation values for combining
  mNormComboAngle = 45.;
}

/*
 * The real main procedure
 */
void PickSeeds::main(int argc, char *argv[])
{
  // Parameters
  // 6: Fraction of points that must be close in two tracks for them to be considered same
  float critFracClose = 0.6;
  // 8: Maximum fraction of target density at which to add points in initial phase
  float minDensityFac1 = 0.9;
  // 9: Higher fraction of target at which to add points in more desperate searches
  float minDensityFac2 = 1.1;
  // 10: Scaling from desired spacing to H for kernel density computation
  float spacingToHfac = 1.3;
  // 11: Scaling from desired spacing to density grid spacing
  float spacingToGridFac = 0.2;
  // 13: Ratio of minority to majority for using higher density factor
  float surfRatioUseDens2 = 0.65;
  // 14: Fraction of nominal spacing allowed for initial addition of points
  float initAddSpacingFac = 0.85;
  // 15: Fraction of spacing for adding best half of points on next phase
  float halfAddSpacingFac = 0.7;

  // Defaults for options
  int useClusters = 0;
  int useOverlaps = 0;
  int twoSurf = 0;
  int phaseAsSurf = 0;
  float rotation = 0.;
  int noBeefUp = 0;
  int xBorder = 0, yBorder = 0;
  int boundForCount = 0;

  // Indices for the weights, and their default values.
  enum {WGT_COMPLETE = 0, WGT_NUM_MODELS, WGT_RESIDUAL, WGT_DEVIATION};
  float weights[4] = {1., 1., 1., 1.};

  char *filename;
  char *outName;
  char *elongName = NULL;
  char *progname = imodProgName(argv[0]);
  int numOptArgs, numNonOptArgs;
  int maxConts, i, nxImage, nyImage, izMiddle, targetNumber, ifDensity, ifNumber, j, loop;
  int len, co, surfNumber, numDomains, maxGridPerDom, ix, iy, jx, jy, ind, maxTrackLen;
  float targetDensity, totalArea, targetSpacing, surfDensity, surfSpacing,kernelH;
  float gridSpacing, xx, yy, dzmin, dz, fracClose, meanDev, compSum, resSum;
  int imin, ncum, idom, neigh, neighCo, neighMod, coMod, ndev, botSum, topSum, ob;
  int useDens2, majorNumber, numPhase, phase, overlapThresh, topBot, surf;
  float resid, sdMean, sdMed, sdSD, termDens, bestResForPos;
  float clusterThresh, majorDensity, majorSpacing, majorKernelH, valmin, valmax;
  float eloMean, eloMed, eloSD, outerMAD, outerBkgd, wsum;
  int numModels, numClust, numOver, numIgnore, numBaseCand, numContForCount = 0;
  int numInSurf[30], numInside[30], numOutside[30], totInside, totOutside;
  float *edgeTmp;
  bool lookMore;
  int *trackList;
  Imod *imod;
  Imod *baseMod;
  Iobj *obj;
  Icont *cont;
  Candidate candid;
  Candidate *candp;
  GridDomain *domp;
  char line[MAXLINE];
  Istore store;
  FILE *fp;
  const int MAX_COLORS = 10;
  unsigned char rgba[MAX_COLORS][4] = 
    {{255,0,255,0}, {255,255,0,0}, {0,255,255,0}, {255,0,0,0}, {0,0,255,0},
     {255,128,0,0}, {153,102,229,0}, {51,51,204,0}, {229,153,102,0}, {153,51,51,0}};
  const int MAX_COLORS2 = 7;
  unsigned char rgba2[MAX_COLORS2][4] = 
    {{255,0,255,0}, {0,255,0,0}, {255,255,0,0}, {100,100,0,0}, 
     {255,0,0,0}, {85,170,255,0}, {255,128,0,0}};
  const char *fileOptName[2] = {"ElongationFile", "SurfaceFile"};
    
  // Fallbacks from    ../manpages/autodoc2man 2 1 pickbestseed
  int numOptions = 27;
  const char *options[] = {
    "tracked:TrackedModel:FNM:", "surface:SurfaceFile:FNM:",
    "resid:ElongationFile:FNM:", "output:OutputSeedModel:FN:",
    "append:AppendToSeedModel:B:", "size:BeadSize:F:", "image:ImageSizeXandY:IP:",
    "border:BordersInXandY:IP:", "middle:MiddleZvalue:I:", "two:TwoSurfaces:B:",
    "boundary:BoundaryModel:FN:", "exclude:ExcludeInsideAreas:B:",
    "counting:BoundaryForCounting:B:", "number:TargetNumberOfBeads:I:",
    "density:TargetDensityOfBeads:F:", "nobeef:LimitMajorityToTarget:B:",
    "elongated:ElongatedPointsAllowed:I:", "cluster:ClusteredPointsAllowed:I:",
    "lower:LowerTargetForClustered:F:", "rotation:RotationAngle:F:",
    "highest:HighestTiltAngle:F:", "weights:WeightsForScore:FA:",
    "control:ControlValue:FPM:", "phase:PhaseOutput:B:",
    "root:DensityOutputRootname:CH:", "candidate:CandidateModel:FN:",
    "verbose:VerboseOutput:I:"};

  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        9, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  // Read the tracked models, get maximum number of contours (They should all match...)
  if (PipNumberOfEntries("TrackedModel", &numModels))
    exitError("At least one tracked model must be entered");
  if (numModels > MAX_MODELS)
    exitError("At most %d tracked models can be entered", MAX_MODELS);
  maxConts = 0;
  for (i = 0; i < numModels; i++) {
    PipGetString("TrackedModel", &filename);
    mTrackMods[i] = imodRead(filename);
    if (!mTrackMods[i])
      exitError("Reading tracked model %s", filename);
    if (!mTrackMods[i]->objsize || !mTrackMods[i]->obj[0].contsize)
      exitError("No contours in tracked model %s", filename);
    maxConts = B3DMAX(maxConts, mTrackMods[i]->obj[0].contsize);
    free(filename);
  }

  // Get other options
  if (PipGetInteger("MiddleZvalue", &izMiddle) || PipGetFloat("BeadSize", &mBeadSize) ||
      PipGetTwoIntegers("ImageSizeXandY", &nxImage, &nyImage) ||
      PipGetString("OutputSeedModel", &outName))
    exitError("Middle Z value, bead size, image size, and output filename must be "
              "entered");

  PipGetBoolean("TwoSurfaces", &twoSurf);
  PipGetBoolean("PhaseOutput", &phaseAsSurf);
  PipGetBoolean("AppendToSeedModel", &mAppendToSeed);
  PipGetBoolean("BoundaryForCounting", &boundForCount);
  PipGetInteger("VerboseOutput", &mVerbose);
  PipGetString("DensityOutputRootname", &mDensPlotName);
  PipGetString("CandidateModel", &elongName);
  PipGetFloat("RotationAngle", &rotation);
  PipGetFloat("HighestTiltAngle", &mHighestTilt);
  PipGetInteger("ClusteredPointsAllowed", &useClusters);
  B3DCLAMP(useClusters, 0, 4);
  if (!PipGetInteger("ElongatedPointsAllowed", &useOverlaps)) {
    if (useClusters > 1)
      exitError("You cannot enter both -elongated and -cluster with a value > 1");
    B3DCLAMP(useOverlaps, 0, 3);
  } else if (useClusters) {
    useOverlaps = useClusters - 1;
    useClusters = 1;
  }

  PipGetFloat("LowerTargetForClustered", &mOverlapTarget);
  PipGetBoolean("LimitMajorityToTarget", &noBeefUp);
  PipGetTwoIntegers("BordersInXandY", &xBorder, &yBorder);
  iy = 4;
  ix = PipGetFloatArray("WeightsForScore", weights, &iy, 4);
  if (ix < 0 || iy != 4)
    exitError("You must enter exactly 4 values for weights");
  weights[WGT_NUM_MODELS] /= B3DMAX(numModels - 1., 1.);

  // Allow any parameter to be set
  PipNumberOfEntries("ControlValue", &ncum);
  for (i = 0; i < ncum; i++) {
    PipGetTwoFloats("ControlValue", &xx, &yy);
    switch (B3DNINT(xx)) {
      SET_CONTROL_FLOAT(1, mCloseDiamFrac);
      SET_CONTROL_FLOAT(2, mExcludeFac);
      SET_CONTROL_FLOAT(3, mRingSpacingFac);
      SET_CONTROL_INT(4, mNumRings);
      SET_CONTROL_FLOAT(5, mClusterCrit);
      SET_CONTROL_FLOAT(6, critFracClose);
      SET_CONTROL_FLOAT(7, mElongCritScaling);
      SET_CONTROL_FLOAT(8, minDensityFac1);
      SET_CONTROL_FLOAT(9, minDensityFac2);
      SET_CONTROL_FLOAT(10, spacingToHfac);
      SET_CONTROL_FLOAT(11, spacingToGridFac);
      SET_CONTROL_FLOAT(12, mEdgeOutlierCrit);
      SET_CONTROL_FLOAT(13, surfRatioUseDens2);
      SET_CONTROL_FLOAT(14, initAddSpacingFac);
      SET_CONTROL_FLOAT(15, halfAddSpacingFac);
      SET_CONTROL_FLOAT(16, mElongForOverlap);
      SET_CONTROL_INT(17, mOptionFlags);
      SET_CONTROL_FLOAT(18, mEdgeComboAngle);
      SET_CONTROL_FLOAT(19, mElongComboAngle);
      SET_CONTROL_FLOAT(20, mNormComboAngle);
    default: break;
    }
  }

  // Multiply both elongation criteria, whether entered or not, by the criterion scaling
  mElongForOverlap *= mElongCritScaling;
  mEdgeOutlierCrit *= mElongCritScaling;
   
  ifDensity = 1 - PipGetFloat("TargetDensityOfBeads", &targetDensity);
  ifNumber = 1 - PipGetInteger("TargetNumberOfBeads", &targetNumber);
  if (ifNumber + ifDensity != 1)
    exitError("Target number or density must be entered, not both");
  mAreaXmin = xBorder;
  mAreaXmax = nxImage - xBorder;
  mAreaYmin = yBorder;
  mAreaYmax = nyImage - yBorder;
  totalArea = (nxImage - 2 * xBorder) * (nyImage - 2 * yBorder);

  // Read base model to append to
  if (mAppendToSeed) {
    baseMod = imodRead(outName);
    if (!baseMod)
      exitError("Reading existing seed model %s", outName);
  }

  // Read area model if any and get the area
  if (!PipGetString("BoundaryModel", &filename)) {
    mAreaMod = imodRead(filename);
    if (!mAreaMod)
      exitError("Reading boundary model %s", filename);
    free(filename);
    PipGetBoolean("ExcludeInsideAreas", &mExcludeAreas);
    if (!mAreaMod->objsize || !mAreaMod->obj[0].contsize)
      exitError("No contours in object 1 of boundary model");
    if (makeAreaContList(&mAreaMod->obj[0], izMiddle, mAreaConts, &mNumAreaCont, 
                         MAX_AREAS))
      exitError("Too many contours on one section in boundary model for array (limit "
                "is %d)", MAX_AREAS);

    if (boundForCount) {
      numContForCount = mNumAreaCont;
      mNumAreaCont = 0;
    } else {

      // Get the area by converting each contour to scan contour, clipping each segment to
      // be within borders, and adding up segment lengths
      if (!mExcludeAreas)
        totalArea = 0.;
      for (co = 0; co < mNumAreaCont; co++) {
        cont = imodel_contour_scan(&mAreaMod->obj[0].cont[mAreaConts[co]]);
        if (!cont)
          exitError("Creating scan contour from boundary contour");
        for (i = 0; i < cont->psize; i += 2) {
          xx = B3DMAX(mAreaXmin, cont->pts[i].x);
          yy = B3DMIN(mAreaXmax, cont->pts[i + 1].x);
          totalArea += B3DMAX(0, yy - xx) * (mExcludeAreas ? -1 : 1);
        }
        imodContourDelete(cont);
      }
    }
  }

  printf("Total area = %.2f megapixels\n", totalArea / 1.e6);

  mTracks = B3DMALLOC(TrackData, maxConts);
  trackList = B3DMALLOC(int, maxConts);
  edgeTmp = B3DMALLOC(float, maxConts);
  if (!mTracks || !trackList || !edgeTmp)
    exitError("Allocating arrays for track data");
  for (i = 0; i < maxConts; i++) {
    mTracks[i].model = -1;
    mTracks[i].domain = -1;
    mTracks[i].candIndex = -1;
  }
  
  // Read in the residual elongation and top/bottom data
  numIgnore = 0;
  for (loop = 0; loop <= twoSurf; loop++) {
    for (i = 0; i < numModels; i++) {
      if (PipGetString(fileOptName[loop], &filename) != 0) {
        if (loop) {
          numIgnore++;
          continue;
        }
        exitError("%d files must be entered with the %s option", numModels, 
                  fileOptName[loop]);
      }
      fp = fopen(filename, "r");
      if (!fp)
        exitError("Opening file %s", filename);

      while(1) {
        len = fgetline(fp, line, MAXLINE);
        if (!len)
          continue;
        if (len == -1)
          exitError("Reading file %s", filename);
        if (len == -2)
          break;
        if (loop) {
          sscanf(line, "%d %d %d %d", &ix, &co, &iy, &topBot);
        } else {
          outerMAD = outerBkgd = wsum = 0.;
          sscanf(line, "%d %d %f %f %f %f %f %f %f %f", &ix, &co, &resid, &sdMean,
                 &sdMed, &sdSD, &eloMean, &eloMed, &eloSD, /* &outerMAD, &outerBkgd, */
                 &wsum);
          if (resid < 0. || sdMean < 0.)
            continue;
        }
        if (co < 1 || co > mTrackMods[i]->obj[0].contsize)
          exitError("Contour number (%d) out of range in %s", co, filename);
        co--;
        if (loop) {
          mTracks[co].topBot = topBot;
        } else {
          mTracks[co].model = i;
          mTracks[co].topBot = 0;
          mTracks[co].residual = resid;
          mTracks[co].edgeSDmean = sdMean;
          mTracks[co].edgeSDmedian = sdMed;
          mTracks[co].edgeSDsd = sdSD;
          mTracks[co].elongMean = eloMean;
          mTracks[co].elongMedian = eloMed;
          mTracks[co].elongSD = eloSD;
          /* mTracks[co].outerMAD = outerMAD;
             mTracks[co].outerBkgd = outerBkgd; */
          mTracks[co].wsumMean = wsum; 
          if (wsum > 0) {
            mNumWsum++;
            mWsumMean += wsum;
          }
          if (outerMAD > 0) {
            mNumOuterMAD++;
            mOuterMADMean += outerMAD;
          }
        }
        if (len < 0)
          break;
      }
      free(filename);
      fclose(fp);
    }
  }
  if (numIgnore == numModels)
    exitError("Surface information must be provided for at least one model "
              "if -twosurf is given");
  mOuterMADMean /= B3DMAX(1, mNumOuterMAD);
  mWsumMean /= B3DMAX(1, mNumWsum);
  if (mVerbose)
    PRINT4(mNumWsum, mWsumMean, mNumOuterMAD, mOuterMADMean);

  // Set up the domains; first get the density in number per square pixel
  // Also set up the lower target for overlapped beads if it was a density
  if (ifNumber) {
    targetDensity = targetNumber / totalArea;
  } else {
    targetDensity /= 1.e6;
    targetNumber = targetDensity * totalArea;
    if (mOverlapTarget > 0)
      mOverlapTarget *= totalArea / 1.e6;
  }
  targetSpacing = sqrt(2. / (targetDensity * sqrt(3.)));
  if (mOverlapTarget < 0)
    mOverlapTarget = targetNumber;

  // Base the domain sizes on the lower density if doing two surfaces, and grid spacing on
  // higher density
  surfDensity = targetDensity / (1. + twoSurf);
  surfNumber = (targetNumber + twoSurf) / (1 + twoSurf);
  surfSpacing = sqrt(2. / (surfDensity * sqrt(3.)));
  kernelH = spacingToHfac * surfSpacing;
  gridSpacing = spacingToGridFac * targetSpacing;
  mNumXgrid = B3DMAX(1., ceil(nxImage / gridSpacing));
  mDelXgrid = nxImage / (mNumXgrid + 1.);
  mNumYgrid = B3DMAX(1., ceil(nyImage / gridSpacing));
  mDelYgrid = nyImage / (mNumYgrid + 1.);
  if (mVerbose)
    PRINT4(kernelH, gridSpacing, mNumXgrid, mNumYgrid);
  if (mVerbose)
    PRINT3(surfDensity * 1.e6, surfSpacing, surfNumber);

  // Divide image area into domains and set up array of domains
  mNumXdomains = (int)B3DMAX(1., 0.4999 * nxImage / kernelH);
  mNumYdomains = (int)B3DMAX(1., 0.4999 * nyImage / kernelH);
  numDomains = mNumXdomains * mNumYdomains;
  mDelXdomain = ((float)nxImage) / mNumXdomains;
  mDelYdomain = ((float)nyImage) / mNumYdomains;
  if (mVerbose)
    PRINT4(mNumXdomains, mNumYdomains, mDelXdomain, mDelXdomain);
  mGridPoints = B3DMALLOC(GridPoint, mNumXgrid * mNumYgrid);
  mDomains = B3DMALLOC(GridDomain, numDomains);
  if (!mDomains || !mGridPoints)
    exitError("Allocating arrays for domains or grid points");
  maxGridPerDom = (1 + (int)(mDelXdomain + 1.) / mDelXgrid) * 
    (1 + (int)(mDelYdomain + 1.) / mDelYgrid);

  // Initialize domains and make list of neighbors to loop over
  for (i = 0; i < numDomains; i++) {
    domp = &mDomains[i];
    domp->numCandidates = 0;
    domp->numAccepted = 0;
    domp->numGridPts = 0;
    domp->numTracks = 0;
    domp->gridPtInd = B3DMALLOC(int, maxGridPerDom);
    if (!domp->gridPtInd)
      exitError("Allocating arrays for grid point lists");
    domp->numNeighbors = 0;
    ix = i % mNumXdomains;
    iy = i / mNumXdomains;
    for (jx = -1; jx <= 1; jx++)
      for (jy = -1; jy <= 1; jy++)
        if (ix + jx >= 0 && ix + jx < mNumXdomains && 
            iy + jy >= 0 && iy + jy < mNumYdomains)
          domp->neighbors[domp->numNeighbors++] = ix + jx + mNumXdomains * (iy + jy);
  }

  // Initialize grid points and make lists in domains
  for (ix = 0; ix < mNumXgrid; ix++) {
    xx = mDelXgrid * (ix + 1);
    for (iy = 0; iy < mNumYgrid; iy++) {
      yy = mDelYgrid * (iy + 1);
      i = ix + iy * mNumXgrid;
      mGridPoints[i].x = xx;
      mGridPoints[i].y = yy;
      mGridPoints[i].domain = -1;
      if ((ind = domainIndex(xx, yy)) >= 0 && 
          xx >= mAreaXmin && xx <= mAreaXmax && yy >= mAreaYmin && yy <= mAreaYmax &&
          (!mNumAreaCont || 
           ((imodPointInsideArea(&mAreaMod->obj[0], mAreaConts, mNumAreaCont, xx, yy)
             >= 0) ? 0 : 1) == mExcludeAreas)) {
        mDomains[ind].gridPtInd[mDomains[ind].numGridPts++] = i;
        mGridPoints[i].domain = ind;
      }
    }
  }

  computeAreaFracs(kernelH);

  // Get domains for the tracks and accumulate the number of tracks per domain
  maxTrackLen = 0;
  for (co = 0; co < maxConts; co++) {
    if (mTracks[co].model >= 0) {
      cont = &mTrackMods[mTracks[co].model]->obj[0].cont[co];
      maxTrackLen = B3DMAX(maxTrackLen, cont->psize);

      // Find nearest Z in track to middle Z
      dzmin = 10000.;
      for (i = 0; i < cont->psize; i++) {
        dz = fabs(cont->pts[i].z - izMiddle);
        if (dz < dzmin) {
          dzmin = dz;
          imin =  i;
        }
      }
      if ((ind = domainIndex(cont->pts[imin].x, cont->pts[imin].y)) >= 0) {
        mTracks[co].domain = ind;
        mDomains[ind].numTracks++;
        mTracks[co].midPos = cont->pts[imin];
      }
    }
  }

  // Make a list of the tracks by domain
  ncum = 0;
  for (i = 0; i < numDomains; i++) {
    mDomains[i].trackStartInd = ncum;
    ncum += mDomains[i].numTracks;
    mDomains[i].numTracks = 0;
  }
  for (co = 0; co < maxConts; co++) {
    ind = mTracks[co].domain;
    if (ind >= 0)
      trackList[mDomains[ind].trackStartInd + mDomains[ind].numTracks++] = co;
  }

  // Identify common tracks and build list of candidates
  for (co = 0; co < maxConts; co++) {
    ind = mTracks[co].domain;
    if (ind < 0 || mTracks[co].candIndex >= 0)
      continue;
    candid.domain = ind;
    coMod = mTracks[co].model;
    for (i = 0; i < numModels; i++)
      candid.contours[i] = -1;
    candid.contours[coMod] = co;
    mTracks[co].candIndex = mCandidates.size();
    candid.overlapped = 0;
    candid.overlapTmp = 0;
    candid.clustered = 0;
    candid.accepted = 0;
    candid.topBot = 0;
    candid.meanDeviation = 0.;
    topSum = mTracks[co].topBot == 2 ? 1 : 0;
    botSum = mTracks[co].topBot == 1 ? 1 : 0;
    ncum = 1;
    bestResForPos = 1.e30;
    if (B3DNINT(mTracks[co].midPos.z) == izMiddle) {
      bestResForPos = mTracks[co].residual;
      candid.pos = mTracks[co].midPos;
    }

    // Loop on all tracks in neighborhood 
    for (neigh = 0; neigh < mDomains[ind].numNeighbors && ncum < numModels; neigh++) {
      idom = mDomains[ind].neighbors[neigh];
      for (i = 0; i < mDomains[idom].numTracks && ncum < numModels; i++) {
        neighCo = trackList[mDomains[idom].trackStartInd + i];
        neighMod = mTracks[neighCo].model;
        if (neighMod == coMod || mTracks[neighCo].candIndex >= 0 || 
            candid.contours[neighMod] >= 0 || mTracks[neighCo].domain < 0)
          continue;
        getTrackDeviation(co, coMod, neighCo, neighMod, meanDev, fracClose);

        // If it matches, add it to the candidate and mark it with index; save deviation
        if (fracClose > critFracClose) {
          candid.contours[neighMod] = neighCo;
          mTracks[neighCo].candIndex = mCandidates.size();
          topSum += mTracks[neighCo].topBot == 2 ? 1 : 0;
          botSum += mTracks[neighCo].topBot == 1 ? 1 : 0;
          ncum++;
          if (B3DNINT(mTracks[neighCo].midPos.z) == izMiddle && 
              bestResForPos > mTracks[neighCo].residual) {
            bestResForPos =  mTracks[neighCo].residual;
            candid.pos = mTracks[neighCo].midPos;
          }
        }
      }
    }

    // Get mean deviation between all pairs if there is more than one track
    if (ncum > 1) {
      for (i = 0; i < numModels - 1; i++) {
        for (j = i + 1; j < numModels; j++) {
          if (candid.contours[i] >= 0 && candid.contours[j] >= 0) {
            getTrackDeviation(candid.contours[i], i, candid.contours[j], j, meanDev,
                              fracClose);
            candid.meanDeviation += meanDev;
          }
        }
      }
      candid.meanDeviation /= ncum * (ncum - 1) / 2;
    }
          
    if (twoSurf && topSum > botSum)
      candid.topBot = 1;
     
    // Get the true position on the middle section or nearest if necessary if it wasn't
    // set from the contour with lowest residual
    if (bestResForPos > 1.e29) {
      for (i = 0; i < numModels; i++) {
        j = candid.contours[(co + i) % numModels];
        if (j >= 0 && B3DNINT(mTracks[j].midPos.z) == izMiddle) {
          candid.pos = mTracks[co].midPos;
          break;
        }
      }
      if (i >= numModels)
        candid.pos = mTracks[co].midPos;
    }
 
    // Now check if it is inside the borders and abort the whole set of tracks if out
    if (candid.pos.x < xBorder || candid.pos.x >= nxImage - xBorder ||
        candid.pos.y < yBorder || candid.pos.y >= nyImage - yBorder) {
      for (i = 0; i < numModels; i++) {
        if (candid.contours[i] >= 0) {
          mTracks[candid.contours[i]].candIndex = -1;
          mTracks[candid.contours[i]].domain = -1;
        }
      }
    } else {

      // Or add the candidate for real
      mCandidates.push_back(candid);
      mDomains[ind].numCandidates++;
    }
  }

  // Make the candidate list organized by domain
  mNumCandidates = mCandidates.size();
  mCandidList = B3DMALLOC(int, mNumCandidates);
  mAcceptList = B3DMALLOC(int, mNumCandidates);
  mRankIndex = B3DMALLOC(int, mNumCandidates);
  if (!mCandidList || !mAcceptList || !mRankIndex)
    exitError("Allocating arrays for candidates");

  ncum = 0;
  for (i = 0; i < numDomains; i++) {
    mDomains[i].candStartInd = ncum;
    ncum += mDomains[i].numCandidates;
    mDomains[i].numCandidates = 0;
  }

  for (i = 0; i < mNumCandidates; i++) {
    ind = (&mCandidates[i])->domain;
    if (ind >= 0)
      mCandidList[mDomains[ind].candStartInd + mDomains[ind].numCandidates++] = i;
  }

  // Identify elongated points first using all points
  analyzeElongation(maxConts, 0, edgeTmp);

  // Identify clustered points as ones with near neighbors by looking at all pairs
  mCosRot = cos(PI * rotation / 180.);
  mSinRot = sin(PI * rotation / 180.);
  mCosTilt = cos(PI * mHighestTilt / 180.);
  
  for (co = 0; co < mNumCandidates; co++) {
    candp = &mCandidates[co];
    ind = candp->domain;
    for (neigh = 0; neigh < mDomains[ind].numNeighbors; neigh++) {
      idom = mDomains[ind].neighbors[neigh];
      for (i = 0; i < mDomains[idom].numCandidates; i++) {
        neighCo = mCandidList[mDomains[idom].candStartInd + i];
        if (neighCo != co && candp->topBot == (&mCandidates[neighCo])->topBot) {
          if (beadsAreClustered(candp, &mCandidates[neighCo])) {
            candp->clustered = 1;
            (&mCandidates[neighCo])->clustered = 1;
          }
        }
      }
    }
  }

  // Analyze for elongated points again after excluding all the clustered ones which
  // could skew the distribution
  analyzeElongation(maxConts, 1, edgeTmp);

  // Report the results
  numClust = 0;
  numOver = 0;
  for (co = 0; co < mNumCandidates; co++) {
    candp = &mCandidates[co];
    if (candp->overlapped)
      numOver++;
    else if (candp->clustered)
      numClust++;
    if (mVerbose)
      printf("%4d %4d %4d %4d  at %4.0f %4.0f  overlap %d clustered %d\n", co + 1, 
             candp->contours[0] + 1, candp->contours[1] + 1, candp->contours[2] + 1,
             candp->pos.x + 1., candp->pos.y + 1., candp->overlapped, candp->clustered);
  }
  printf("%d candidate points, including %d clustered and %d elongated\n", mNumCandidates,
         numClust, numOver);
  if (!mNumCandidates)
    exitError("No candidate points have been identified");

  // Score the candidates, put scores into separate array for ranking
  // A low score is good
  for (ind = 0; ind < mNumCandidates; ind++) {
    candp = &mCandidates[ind];
    ncum = 0;
    compSum = 0.;
    resSum = 0.;
    botSum = 0;
    topSum = 0;
    for (i = 0; i < numModels; i++) {
      co = candp->contours[i];
      if (co >= 0) {
        ncum++;
        compSum += 1. - ((float)mTrackMods[mTracks[co].model]->obj[0].cont[co].psize) /
          maxTrackLen;
        resSum += mTracks[co].residual;
        topSum += mTracks[co].topBot == 2 ? 1 : 0;
        botSum += mTracks[co].topBot == 1 ? 1 : 0;
      }
    }
    ndev = ncum * (ncum - 1) / 2;
    edgeTmp[ind] = (weights[WGT_COMPLETE] * compSum / ncum + 
                    weights[WGT_RESIDUAL] * resSum / ncum +
                    weights[WGT_NUM_MODELS] * (numModels - ncum) + 
                    weights[WGT_DEVIATION] * candp->meanDeviation) /
      (weights[WGT_COMPLETE] + weights[WGT_RESIDUAL] + weights[WGT_NUM_MODELS] + 
       (ndev ? weights[WGT_DEVIATION] : 0.));
    candp->score = edgeTmp[ind];
    mRankIndex[ind] = ind;
    candp->overlapped = (unsigned char)((100 * (int)candp->overlapped) / ncum);
    if (mVerbose)
      printf("%d  tb %d %d -> %d  inc %.3f  res %.3f  miss %d  dev %.3f  score %.f\n",
             ind, botSum, topSum, candp->topBot, compSum / ncum, resSum / ncum,
             numModels - ncum, candp->meanDeviation, candp->score);
  }

  rsSortIndexedFloats(edgeTmp, mRankIndex, mNumCandidates);

  for (ind = 0; ind < 3; ind++)
    mNumAccepted[ind] = 0;

  // If appending, go through each existing point and find it in candidate list and
  // accept the candidate
  if (mAppendToSeed) {
    for (ob = 0; ob < baseMod->objsize; ob++) {
      for (co = 0; co < baseMod->obj[ob].contsize; co++) {
        cont = &baseMod->obj[ob].cont[co];
        if (!cont->psize)
          continue;
        i = cont->psize / 2;
        ind = domainIndex(cont->pts[i].x, cont->pts[i].y);
        if (ind < 0)
          continue;
        lookMore = true;
        for (neigh = 0; neigh < mDomains[ind].numNeighbors && lookMore; neigh++) {
          idom = mDomains[ind].neighbors[neigh];
          for (i = 0; i < mDomains[idom].numCandidates && lookMore; i++) {
            neighCo = mCandidList[mDomains[idom].candStartInd + i];
            candp = &mCandidates[neighCo];
            if (candp->accepted)
              continue;
            for (j = 0; j < numModels && lookMore; j++) {
              if (candp->contours[j] >= 0) {
                getTrackDeviation(candp->contours[j], j, 0, 0, meanDev, fracClose, cont);
                if (fracClose > 0.) {
                  lookMore = false;
                  acceptCandidate(neighCo, candp->topBot);
                }
              }
            }
          }
        }
      }
    }
    mPhase++;
  }

  // Phase 1:
  // For each surface, go through points in order by ranking and and accept them
  // if they are on that surface, not in cluster, and not too close to existing point
  numBaseCand = mNumAccepted[2];
  for (topBot = 0; topBot <= twoSurf; topBot++) {
    addBestSpacedPoints(topBot, surfNumber, mNumCandidates, initAddSpacingFac * 
                        surfSpacing);
    addBestSpacedPoints(topBot, surfNumber, mNumCandidates / 2, halfAddSpacingFac * 
                        surfSpacing);
  }
  outputNumAccepted(twoSurf, 0);
  mPhase++;

  // Phase 2:
  // Now go through the gap filling routine for each surface separately
  // In this phase, also allow a higher density threshold for termination
  useDens2 = 0;
  if (twoSurf) {
    useDens2 = -1;
    for (topBot = 0; topBot <= twoSurf; topBot++)
      if (mNumAccepted[topBot] < surfRatioUseDens2 * mNumAccepted[1 - topBot])
        useDens2 = topBot;
  }
  for (topBot = 0; topBot <= twoSurf; topBot++) {
    termDens = surfDensity * minDensityFac1;
    if (topBot == useDens2)
      termDens = surfDensity * minDensityFac2;
    computeDensities(topBot, kernelH);
    addPointsInGaps(topBot, surfNumber, termDens, surfSpacing, 0., 0, kernelH, 2);
    addPointsInGaps(topBot, surfNumber, termDens, surfSpacing, 0., 0, kernelH, mNumRings);
  }
  outputNumAccepted(twoSurf, 0);
  mPhase++;

  // Phase 3:
  // If two surfaces, now try to beef up the majority surface to make up for deficiency in
  // the minority.  
  // Stick with the lower density factor for termination
  majorDensity = surfDensity;
  if (twoSurf && mNumAccepted[2] < targetNumber) {
    topBot = (mNumAccepted[1] > mNumAccepted[0]) ? 1 : 0;
    
    // Revise target number to make up the difference, and do a round of initial adding
    // at that number, then do the gap filling but base the density analysis on
    // the full set of points in order to fill in gaps of the other surface
    majorNumber = noBeefUp ? surfNumber : (targetNumber - mNumAccepted[1-topBot]);
    majorDensity = majorNumber / totalArea;
    majorSpacing = sqrt(2. / (majorDensity * sqrt(3.)));
    addBestSpacedPoints(topBot, majorNumber, mNumCandidates, initAddSpacingFac * 
                        majorSpacing);
    majorKernelH = targetSpacing;
    computeAreaFracs(majorKernelH);
    computeDensities(2, majorKernelH);
    addPointsInGaps(topBot, majorNumber, targetDensity * minDensityFac1, targetSpacing,
                    0., 0, majorKernelH, 2);
    addPointsInGaps(topBot, majorNumber, targetDensity * minDensityFac1, targetSpacing,
                    0., 0, majorKernelH, mNumRings);
    outputNumAccepted(twoSurf, 0);
  }
  mPhase++;

  // Phase 4 [5, 6, 7, 8]:
  // Now we are going to use a higher density factor, then 
  // add in clustered points, and overlapped points if selected
  numPhase = useClusters + useOverlaps + 1;
  if (mVerbose)
    PRINT3(numPhase, useClusters, useOverlaps);
  for (phase = 0; phase < numPhase && mNumAccepted[2] < targetNumber; phase++) {

    // Do the minority surface first
    topBot = 0;
    if (twoSurf)
      topBot = (mNumAccepted[1] < mNumAccepted[0]) ? 1 : 0;
    overlapThresh = phase ? ((phase - useClusters) * 100) / 3 : 0;
    clusterThresh = (phase && useClusters) ? majorDensity : 0.;
    if (mVerbose)
      PRINT3(phase, clusterThresh, overlapThresh);
    for (surf = 0; surf < twoSurf + 1 && mNumAccepted[2] < targetNumber; surf++) {
      if (surf) {

        // Revise targets every time when doing majority surface
        majorNumber = noBeefUp ? surfNumber : (targetNumber - mNumAccepted[1-topBot]);
        majorDensity = majorNumber / totalArea;
        majorSpacing = sqrt(2. / (majorDensity * sqrt(3.)));
        majorKernelH = spacingToHfac * majorSpacing;
        computeAreaFracs(majorKernelH);
        computeDensities(topBot, majorKernelH);
        addPointsInGaps(topBot, majorNumber, majorDensity * minDensityFac2, majorSpacing,
                        clusterThresh, overlapThresh, majorKernelH, mNumRings);
        
      } else {

        // Return to original targets for minority surface
        computeAreaFracs(kernelH);
        computeDensities(topBot, kernelH);
        addPointsInGaps(topBot, surfNumber, surfDensity * minDensityFac2, surfSpacing,
                        clusterThresh, overlapThresh, kernelH, mNumRings);
      }
      topBot = 1 - topBot;
    }
    outputNumAccepted(twoSurf, 0);
    mPhase++;
  }
 
  outputNumAccepted(twoSurf, 1);

  // Compose the model
  valmin = 1.e20;
  valmax = -valmin;
  if (mAppendToSeed) {
    imod = baseMod;
    obj = &imod->obj[0];
    co = obj->contsize;
    if (mNumAccepted[2] > numBaseCand) {
      ind = co + mNumAccepted[2] - numBaseCand;
      cont = imodContoursNew(ind);
      if (!cont)
        exitError("Allocating new array of contours");
      for (i = 0; i < co; i++)
        imodContourCopy(&obj->cont[i], &cont[i]);
      obj->cont = cont;
      obj->contsize = ind;
    }
    if (istoreGetMinMax(obj->store, obj->contsize, GEN_STORE_MINMAX1, &xx, &yy)) {
      valmin = xx;
      valmax = yy;
    }

  } else {

    // clear out object of one model if not appending
    imod = mTrackMods[numModels / 2];
    obj = clearModelAllocateConts(imod, mNumAccepted[2]);
    co = 0;
  }
  
  // Add candidates as contours, with scores
  store.type = GEN_STORE_VALUE1;
  store.flags = GEN_STORE_FLOAT << 2;
  for (idom = 0; idom < numDomains; idom++) {
    for (i = 0; i < mDomains[idom].numAccepted; i++) {
      ind = mAcceptList[mDomains[idom].candStartInd + i];
      candp = &mCandidates[ind];

      // Skip ones that were accepted because they matched ones in base model
      if (mAppendToSeed && candp->accepted == 1)
        continue;
      if (!imodPointAppend(&obj->cont[co], &candp->pos))
        exitError("Adding point to new model");

      // Need to take inverse of the score so that worse ones are "below threshold"
      // This seems to spread them out as well as taking a log does, too
      store.value.f = 1. / (candp->score + candp->clustered + candp->overlapped / 100.);
      store.index.i = co;
      if (istoreInsert(&obj->store, &store))
        exitError("Could not add general storage item");
      valmin = B3DMIN(valmin, store.value.f);
      valmax = B3DMAX(valmax, store.value.f);
      if (phaseAsSurf)
        obj->cont[co].surf = (twoSurf+1) * (candp->accepted - 1 - mAppendToSeed) + 
          twoSurf * candp->topBot;
      else if (twoSurf)
        obj->cont[co].surf = candp->topBot;
      co++;
    }
  }
  if (istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, valmin, valmax))
    exitError("Could not add general storage item");
  
  // Add colors for the surfaces
  ncum = twoSurf;
  if (phaseAsSurf)
    ncum = (twoSurf + 1) * mPhase + twoSurf - 1;
  obj->surfsize = ncum;
  if (!mAppendToSeed) {
    addSurfaceColors(obj, ncum, rgba, MAX_COLORS);
  }

  // Turn off low flag set originally by imodfindbeads and high one just in case
  obj->matflags2 &= ~(MATFLAGS2_SKIP_LOW | MATFLAGS2_SKIP_HIGH);

  if (imodBackupFile(outName))
    printf("WARNING: pickbestseed - Could not rename existing output file to %s~",
           outName);
  fp = fopen(outName, "wb");
  if (!fp)
    exitError("Opening file for output model");
  imodWrite(imod, fp);

  if (elongName) {
    for (i = 0; i < 8; i++)
      numInSurf[i] = numInside[i] = numOutside[i] = 0;
    totInside = totOutside = 0;
    imodBackupFile(elongName);
    imod = mTrackMods[numModels / 2];
    imod->cindex.object = 0;
    imod->cindex.contour = 0;
    imod->cindex.point = 0;
    obj = clearModelAllocateConts(imod, mNumCandidates);
    for (co = 0; co < mNumCandidates; co++) {
      candp = &mCandidates[co];
      if (!imodPointAppend(&obj->cont[co], &candp->pos))
        exitError("Adding point to new model");
      obj->cont[co].surf = 4 * candp->clustered + (int)ceil(candp->overlapped / 33.4);
      numInSurf[obj->cont[co].surf]++;

      // Make current contour the first elongated non clustered one
      if (candp->overlapped && !candp->clustered && !imod->cindex.contour)
        imod->cindex.contour = co;
      if (numContForCount) {
        ind = imodPointInsideArea(&mAreaMod->obj[0], mAreaConts, numContForCount, 
                                  candp->pos.x, candp->pos.y);
        if (ind >= 0) {
          numInside[obj->cont[co].surf]++;
          if (candp->overlapped)
            totInside++;
        } else {
          numOutside[obj->cont[co].surf]++;
          if (candp->overlapped)
            totOutside++;
        }
      }
    }
    obj->surfsize = 7;
    addSurfaceColors(obj, 7, rgba2, MAX_COLORS2);
    obj->matflags2 &= ~(MATFLAGS2_SKIP_LOW | MATFLAGS2_SKIP_HIGH);
    obj->red = .1;
    obj->green = .5;
    obj->blue = .1;
    obj->linewidth2 = 2;

    // Add labels so user can see what is what
    if (!obj->label)
      obj->label = imodLabelNew();
    imodLabelItemAdd(obj->label, "Not clust. or elong.", 0);
    imodLabelItemAdd(obj->label, "Elongated 1", 1);
    imodLabelItemAdd(obj->label, "Elongated 2", 2);
    imodLabelItemAdd(obj->label, "Elongated 3", 3);
    imodLabelItemAdd(obj->label, "Clustered", 4);
    imodLabelItemAdd(obj->label, "Clust., Elong. 1", 5);
    imodLabelItemAdd(obj->label, "Clust., Elong. 2", 6);
    imodLabelItemAdd(obj->label, "Clust., Elong. 3", 7);

    fp = fopen(elongName, "wb");
    if (!fp)
      exitError("Opening file for output model");
    imodWrite(imod, fp);

    printf("Number in candidate model surfaces:");
    for (i = 0; i < 8; i++)
      printf("   %d", numInSurf[i]);
    printf("\n");
    if (numContForCount) {
      printf("    Inside/Outside:");
      for (i = 0; i < 8; i++)
        printf(" %d/%d", numInside[i], numOutside[i]);
      printf("    Total elongated: %d + %d = %d\n", totInside, totOutside, 
             totInside + totOutside);
    }
  }
  exit(0);
}

Iobj *PickSeeds::clearModelAllocateConts(Imod *imod, int numCont)
{
  Iobj *obj = &imod->obj[0];
  imodContoursDelete(obj->cont, obj->contsize);
  obj->cont = imodContoursNew(numCont);
  if (!obj->cont)
    exitError("Allocating new array of contours");
  obj->contsize = numCont;
  if (obj->store)
    ilistDelete(obj->store);
  obj->store = NULL;
  return obj;
}

void PickSeeds::addSurfaceColors(Iobj *obj, int ncum, unsigned char rgba[][4], 
                                 int maxColors)
{
  Istore store;
  int i, j, ind;
  store.type = GEN_STORE_COLOR;
  store.flags = (GEN_STORE_BYTE << 2) | GEN_STORE_SURFACE;
  for (i = 0; i < ncum; i++) {
    store.index.i = i + 1;
    ind = i % maxColors;
    for (j = 0; j < 4; j++)
      store.value.b[j] = rgba[ind][j];
    if (istoreInsert(&obj->store, &store))
      exitError("Could not add general storage item");
  }
}

/*
 * Go through points in order of increasing score and add them to the given side as long 
 * as their distance from other points on that side is bigger than minSpacing
 */
void PickSeeds::addBestSpacedPoints(int topBot, int targNum, int numCandidates, 
                                   float minSpacing)
{
  int j, ind, neigh, idom, i, co;
  bool tooClose;
  Candidate *candp;
  for (j = 0 ; j < numCandidates; j++) {
    candp = &mCandidates[mRankIndex[j]];
    if (candp->overlapped || candp->clustered || candp->topBot != topBot)
      continue;
    tooClose = false;
    ind = candp->domain;
    for (neigh = 0; neigh < mDomains[ind].numNeighbors && !tooClose; neigh++) {
      idom = mDomains[ind].neighbors[neigh];
      for (i = 0; i < mDomains[idom].numAccepted; i++) {
        co = mAcceptList[mDomains[idom].candStartInd + i];
        if ((&mCandidates[co])->topBot == topBot && 
            imodPointDistance(&((&mCandidates[co])->pos), &candp->pos) < minSpacing) {
          tooClose = true;
          break;
        }
      }
    }
    if (!tooClose) {
      acceptCandidate(mRankIndex[j], topBot);
      if (mNumAccepted[topBot] >= targNum)
        break;
    }
  }
}

/*
 * Look around all the places where density is lowest for points to add
 */
void PickSeeds::addPointsInGaps(int topBot, int targNum, float termDens, 
                                float targSpacing, float clusterThresh, int overlapThresh,
                                float H, int numRings)
{
  std::vector<int> candInRing, ringNum;
  Candidate *cand;
  int i, ind, indMin, ixdel, iydel, ixmid, iymid, ixlo, ixhi, iylo, iyhi;
  int ix, iy, gpt, idom, iring, neigh;
  float densMin, excludeDist, excludeSq, dx, dy, dist, delr, minScore, score, distMin; 
  float gridXcen, gridYcen;
  bool tooClose;

  for (i = 0; i < mNumXgrid * mNumYgrid; i++)
    mGridPoints[i].exclude = 0;
  outputDensities(topBot);

  // Loop until desired number is achieved
  while (mNumAccepted[topBot] < targNum) {

    // Find lowest density non-excluded spot
    indMin = -1;
    densMin = 1.e20;
    candInRing.clear();
    ringNum.clear();
    for (i = 0; i < mNumXgrid * mNumYgrid; i++) {
      if (mGridPoints[i].domain >= 0 && !mGridPoints[i].exclude && 
          mGridPoints[i].density < densMin) {
        densMin = mGridPoints[i].density;
        indMin = i;
      }
    }

    // Break out of loop if none found
    if (indMin < 0 || densMin > termDens) {
      if (mVerbose > 1 && indMin < 0)
        printf("tb %d - all areas excluded in density search\n", topBot);
      else if (mVerbose > 1)
        printf("tb %d - min density %.2f above limit %.2f\n", topBot, 1.e6 * densMin,
               1.e6 * termDens);
      break;
    }

    // Exclude all grid points in the neighborhood from future searches
    gridXcen = mGridPoints[indMin].x;
    gridYcen = mGridPoints[indMin].y;
    excludeDist = mExcludeFac * targSpacing;
    excludeSq = excludeDist * excludeDist;
    ixdel = (int)ceil(excludeDist / mDelXgrid);
    iydel = (int)ceil(excludeDist / mDelYgrid);
    ixmid = indMin % mNumXgrid;
    iymid = indMin / mNumXgrid;
    ixlo = B3DMAX(0, ixmid - ixdel);
    ixhi = B3DMIN(mNumXgrid - 1, ixmid + ixdel);
    iylo = B3DMAX(0, iymid - iydel);
    iyhi = B3DMIN(mNumYgrid - 1, iymid + iydel);
    for (iy = iylo; iy <= iyhi; iy++) {
      for (ix = ixlo; ix <= ixhi; ix++) {
        gpt = ix + iy * mNumXgrid;
        if (mGridPoints[gpt].domain >= 0) {
          dx = gridXcen - mGridPoints[gpt].x;
          dy = gridYcen - mGridPoints[gpt].y;
          dist = dx * dx + dy * dy;
          if (dist < excludeSq)
            mGridPoints[gpt].exclude = 1;
        }
      }
    }

    // Look for points in rings: first define a range of domains to loop in
    delr = targSpacing * mRingSpacingFac;
    excludeDist = numRings * delr;
    excludeSq = excludeDist * excludeDist;
    ixlo = (int)((gridXcen - excludeDist) / mDelXdomain);
    ixhi = (int)((gridXcen + excludeDist) / mDelXdomain);
    ixlo = B3DMAX(0, ixlo);
    ixhi = B3DMIN(mNumXdomains - 1, ixhi);
    iylo = (int)((gridYcen - excludeDist) / mDelYdomain);
    iyhi = (int)((gridYcen + excludeDist) / mDelYdomain);
    iylo = B3DMAX(0, iylo);
    iyhi = B3DMIN(mNumYdomains - 1, iyhi);

    // Then look at all candidates and make lists of ring numbers
    for (iy = iylo; iy <= iyhi; iy++) {
      for (ix = ixlo; ix <= ixhi; ix++) {
        idom = ix + iy * mNumXdomains;
        for (i = 0; i < mDomains[idom].numCandidates; i++) {
          ind = mCandidList[mDomains[idom].candStartInd + i];
          cand = &mCandidates[ind];
          if (!cand->accepted && (cand->overlapped <= overlapThresh) && 
              (!cand->overlapped || mNumAccepted[2] < mOverlapTarget) &&
              (topBot > 1 || cand->topBot == topBot) &&
              (!cand->clustered || (densMin < clusterThresh && 
                                    mNumAccepted[2] < mOverlapTarget))) {
            dx = gridXcen - cand->pos.x;
            dy = gridYcen - cand->pos.y;
            dist = dx * dx + dy * dy;
            if (dist < excludeSq) {
              iring = sqrt(dist) / delr;
              candInRing.push_back(ind);
              ringNum.push_back(B3DMIN(numRings - 1, iring));
            }
          }
        }
      }
    }
    
    // Process the rings from middle outward
    for (iring = 0; iring < numRings; iring++) {
      indMin = -1;
      minScore = 1.e20;
      for (ind = 0; ind < candInRing.size(); ind++) {
        if (iring == ringNum[ind]) {
          
          // Find distance to nearest accepted point and determine if one is just
          // too close
          cand = &mCandidates[candInRing[ind]];
          idom = cand->domain;
          distMin = 1.e20;
          tooClose = false;
          for (neigh = 0; neigh < mDomains[idom].numNeighbors; neigh++) {
            for (i = 0; i < mDomains[neigh].numAccepted; i++) {
              ix = mAcceptList[mDomains[neigh].candStartInd + i];
              if (cand->topBot == (&mCandidates[ix])->topBot) {
                dist = imodPointDistance(&((&mCandidates[ix])->pos), &(cand->pos));
                distMin = B3DMIN(dist, distMin);
                tooClose = beadsAreClustered(cand, &mCandidates[ix]);
              }
            }
          }

          // Penalize clustered and overlapped ones in the ring so others are given
          // priority if any still exist
          // Adjust score by dividing by distance: a very close point gets big score
          score = cand->score + cand->clustered + cand->overlapped / 100.;
          if (distMin < 1.e19)
            score /= distMin;
          if (score < minScore && !tooClose) {
            minScore = score;
            indMin = candInRing[ind];
          }
        }
      }

      // If a point was found in ring, accept it, update densities, break out of ring loop
      if (indMin >= 0) {
        if (mVerbose) {
          cand = &mCandidates[indMin];
          printf("tb %d  density %.2f, add %d at %.0f, %.0f in ring %d, adjusted "
                 "score %f\n", topBot, densMin * 1.e6, indMin, cand->pos.x, cand->pos.y,
                 iring,minScore);
        }
        acceptCandidate(indMin, cand->topBot);
        reviseDensities(indMin, H);
        break;
      }
    }
    if (indMin < 0 && mVerbose > 1)
      printf("tb %d  min density %.2f at %.0f, %.0f, no point found\n", topBot, 
             densMin * 1.e6, gridXcen, gridYcen);
  }
  outputDensities(topBot);

  // This was used to validate that the adjusted densities are the same as the ones
  // computed from scratch
  /* computeDensities(topBot, H);
     outputDensities(topBot); */
}

/* 
 * Mark a candidate as accepted and add it to the list, increment counts
 */
void PickSeeds::acceptCandidate(int index, int topBot)
{
  Candidate *cand = &mCandidates[index];
  int idom = cand->domain;
  cand->accepted = mPhase;
  mAcceptList[mDomains[idom].candStartInd + mDomains[idom].numAccepted++] = index;
  mNumAccepted[topBot]++;
  mNumAccepted[2]++;
}

/*
 * Return domain index for a given x, y
 */
int PickSeeds::domainIndex(float x, float y)
{
  int ix = x / mDelXdomain;
  int iy = y / mDelYdomain;
  if (ix < 0 || ix >= mNumXdomains || iy < 0 || iy >= mNumYdomains)
    return -1;
  return ix + iy * mNumXdomains;
}

/*
 * Compare two tracks to see if they are close to each other, return the mean deviation
 * between them and the fraction of points that are close
 */
void PickSeeds::getTrackDeviation(int co1, int mod1, int co2, int mod2, float &meanDev,
                                  float &fracClose, Icont *baseCont)
{
  Icont *cont1 = &mTrackMods[mod1]->obj[0].cont[co1];
  Icont *cont2 = baseCont;
  float dx, dy, dev;
  int pt1 = 0, pt2 = 0;
  int nsum = 0, nclose = 0;
  float devsum = 0.;
  if (!cont2)
    cont2 = &mTrackMods[mod2]->obj[0].cont[co2];
  while (pt1 < cont1->psize && pt2 < cont2->psize) {
    if (B3DNINT(cont1->pts[pt1].z) < B3DNINT(cont2->pts[pt2].z)) {
      pt1++;
    } else if (B3DNINT(cont1->pts[pt1].z) > B3DNINT(cont2->pts[pt2].z)) {
      pt2++;
    } else {
      dx = cont1->pts[pt1].x - cont2->pts[pt2].x;
      dy = cont1->pts[pt1].y - cont2->pts[pt2].y;
      dev = sqrt(dx * dx + dy * dy);
      devsum += dev;
      nsum++;
      if (dev < mCloseDiamFrac * mBeadSize)
        nclose++;
      pt1++;
      pt2++;
    }
  }
  fracClose = nsum ? ((float)nclose / nsum) : 0;
  meanDev = nsum ? devsum / nsum : 0.;
}

/*
 * Analyze the edge SD and elongation values for all the tracks that have gone into 
 * candidates and mark outliers as overlapped
 */
void PickSeeds::analyzeElongation(int maxConts, int which, float *edgeSDs)
{
  static float *elongs = NULL, *elongTmp, *wsums, *elongOutlie;
  static int *sortInd, *contInd;
  int co, numSet = 0, numEdge = 0, ind, includeStart, includeEnd, testStart, testEnd;
  float edgeSlope, edgeIntcp, elongSlope, elongIntcp, ro, seSlope, seIntcp, se, tmp, tmp2;
  int numElongOut = 0, numElongAbs = 0, numTot = 0;
  int numInGroup, minForGroups = 50;
  int minToTest = 10;
  bool testByGroup;
  double cosElong = cos(DEG2RAD * mElongComboAngle);
  double sinElong = sin(DEG2RAD * mElongComboAngle);
  double cosEdge = cos(DEG2RAD * mEdgeComboAngle);
  double sinEdge = sin(DEG2RAD * mEdgeComboAngle);
  double cosNorm = cos(DEG2RAD * mNormComboAngle);
  double sinNorm = sin(DEG2RAD * mNormComboAngle);

  if (!elongs) {
    elongs = B3DMALLOC(float, maxConts);
    elongTmp = B3DMALLOC(float, maxConts);
    elongOutlie = B3DMALLOC(float, maxConts);
    wsums = B3DMALLOC(float, maxConts);
    sortInd = B3DMALLOC(int, maxConts);
    contInd = B3DMALLOC(int, maxConts);
    if (!elongs || !elongTmp || !wsums || !sortInd || !contInd || !elongOutlie)
      exitError("Allocating arrays for analyzing elongation");
  }

  // Load the data arrays for all contours of all candidates
  for (co = 0; co < maxConts; co++) {
    if (mTracks[co].candIndex >= 0 && !(&mCandidates[mTracks[co].candIndex])->clustered) {
      edgeSDs[numEdge] = cosEdge * mTracks[co].edgeSDmean + 
        sinEdge * mTracks[co].edgeSDsd;
      elongs[numEdge] = cosElong * mTracks[co].elongMean + sinElong * mTracks[co].elongSD;
      contInd[numEdge] = co;
      sortInd[numEdge] = numEdge++;
    }
  }
  if (!numEdge)
    return;

  // Fit each measure versus the wsum and replace with the residual of the fit (doesn't
  // help)
  if (mNumWsum && (mOptionFlags & OPTION_FIT_TO_WSUM)) {
    lsFitPred(wsums, edgeSDs, numEdge, &edgeSlope, &edgeIntcp, &ro, &seIntcp, &seSlope,
              &se, 0., &tmp, &tmp2);
    if (mVerbose)
      printf("EdgeSD vs wsum: slope %f sb %f  intcp %f sa %f  ro %f\n", edgeSlope, 
             seSlope, edgeIntcp, seIntcp, ro);
    lsFitPred(wsums, elongs, numEdge, &elongSlope, &elongIntcp, &ro, &seIntcp, &seSlope,
              &se, 0., &tmp, &tmp2);
    if (mVerbose)
      printf("elong vs wsum: slope %f sb %f  intcp %f sa %f  ro %f\n", elongSlope, 
             seSlope, elongIntcp, seIntcp, ro);
    for (co = 0; co < numEdge; co++) {
      edgeSDs[co] -= wsums[co] * edgeSlope + edgeIntcp;
      elongs[co] -= wsums[co] * elongSlope + elongIntcp;
    }
  }

  // Normalize the edgeSD's to the same SD as the elongs and combine
  avgSD(edgeSDs, numEdge, &edgeIntcp, &edgeSlope, &ro);
  avgSD(elongs, numEdge, &elongIntcp, &elongSlope, &ro);
  for (co = 0; co < numEdge; co++) 
    elongs[co] = sinNorm * edgeSDs[co] * elongSlope / edgeSlope + cosNorm * elongs[co];

  includeStart = 0;
  includeEnd = numEdge;
  testStart = 0;
  testEnd = numEdge;
  testByGroup = mNumWsum && (mOptionFlags & OPTION_WSUM_GROUPS) && 
    numEdge >= minForGroups;
  if (testByGroup) {
    numInGroup = B3DMAX(minForGroups, (numEdge + 3) / 4);
    includeEnd = numInGroup - minToTest;
    rsSortIndexedFloats(wsums, sortInd, numEdge);
  }

  do {

    if (testByGroup) {
      
      // Advance end from last time by minToTest, limit to numEdge, and get start from
      // that, and set test end at half of test group size past middle of group to
      // analyze, except when analysis goes to end
      includeEnd = B3DMIN(includeEnd + minToTest, numEdge);
      includeStart = includeEnd - numInGroup;
      testEnd = (includeStart + includeEnd + minToTest) / 2;
      if (includeEnd == numEdge)
        testEnd = numEdge;
    }

    // Load the data
    for (ind = includeStart; ind < includeEnd; ind++)
      elongTmp[ind - includeStart] = elongs[sortInd[ind]];

    // Analyze for outliers, mark as overlapped if it is an outlier on either measure or
    // exceeds the absolute elongation
    rsMadMedianOutliers(elongTmp, includeEnd - includeStart, mEdgeOutlierCrit, 
                        elongOutlie);
    for (ind = testStart - includeStart; ind < testEnd - includeStart; ind++) {
      co = contInd[sortInd[ind + includeStart]];
      if (elongOutlie[ind] > 0. || mTracks[co].elongMedian >= mElongForOverlap) {
        if (which)
          (&mCandidates[mTracks[co].candIndex])->overlapTmp++;
        else
          (&mCandidates[mTracks[co].candIndex])->overlapped++;
        numTot++;
        if (elongOutlie[ind] > 0.)
          numElongOut++;
        if (mTracks[co].elongMedian >= mElongForOverlap)
          numElongAbs++;
      }
    }
    testStart = testEnd;
  } while (testEnd < numEdge);

  // Count the candidates marked as overlapped and set the overlap to the max on the 
  // second round
  for (co = 0; co< mNumCandidates; co++) {
    if (which)
      (&mCandidates[co])->overlapped = 
        B3DMAX((&mCandidates[co])->overlapped, (&mCandidates[co])->overlapTmp);
    if ((&mCandidates[co])->overlapped)
      numSet++;
  }
  
  if (mVerbose) {
    printf("%d beads identified as elongated after round %d\n", numSet, which + 1);
    PRINT3(numTot, numElongOut, numElongAbs);
  }
}

/*
 * Test whether two beads are "clustered", i.e., too close to each other after considering
 * tilt foreshortening at highest tilt
 */
bool PickSeeds::beadsAreClustered(Candidate *cand1, Candidate *cand2)
{
  float dx, dy, dxtmp;

  // Rotate the vector so the tilt axis is vertical and foreshorten X by highest
  // tilt angle to determine worst-case separation
  dx = cand1->pos.x - cand2->pos.x;
  dy = cand1->pos.y - cand2->pos.y;
  if (mHighestTilt) {
    dxtmp = dx * mCosRot + dy * mSinRot;
    dy = -dx * mSinRot + dy * mCosRot;
    dx = mCosTilt * dxtmp;
  }
  return (sqrt(dx * dx + dy * dy) < mClusterCrit * mBeadSize);
}

/*
 * Compute a weighted sum of area actually present around a grid point inside boundaries
 * This can be used as a denominator to get density from the kernel sum of beads around
 * a grid point
 */ 
void PickSeeds::computeAreaFracs(float H)
{
  int idom, gpt, ring, numInside, numSamp, samp;
  float xcen, ycen, xx, yy, delr, areaSum, rr, angle, weight, maxSum;
  int numRings = 20;
  delr = H / numRings;
  maxSum = 0.;
  for (ring = 0; ring < numRings; ring++) {
    rr = (ring + 0.5) * delr;
    weight = pow(1. - (rr / H) * (rr / H), 3.);
    maxSum += PI * (2. * ring + delr) * delr * weight;
  }
  
  // Sample small sectors in small rings to see if they are inside area being considered
  for (idom = 0; idom < mNumXdomains * mNumYdomains; idom++) {
    for (gpt = 0; gpt < mDomains[idom].numGridPts; gpt++) {
      areaSum = 0.;
      xcen = mGridPoints[gpt].x;
      ycen = mGridPoints[gpt].y;
      for (ring = 0; ring < numRings; ring++) {
        rr = (ring + 0.5) * delr;
        numSamp = B3DNINT(2. * PI * rr / delr);
        numInside = 0;
        for (samp = 0; samp < numSamp; samp++) {
          angle = 2. * PI * samp / numSamp;
          xx = xcen + rr * cos(angle);
          yy = ycen + rr * sin(angle);
          if (xx >= mAreaXmin && xx <= mAreaXmax && yy >= mAreaYmin && yy <= mAreaYmax) {
            if (mNumAreaCont)

              // This adds 1 if point is inside and exclude = 0, or if the point is 
              // outside and exclude = 1
              numInside += (imodPointInsideArea(&mAreaMod->obj[0], mAreaConts, 
                                                mNumAreaCont, xx, yy) >= 0) ? 
                1 - mExcludeAreas : mExcludeAreas;
            else 
              numInside++;
          }
        }
        weight = pow(1. - (rr / H) * (rr / H), 3.);
        areaSum += PI * (2. * ring + delr) * delr * (weight * numInside) / numSamp;
      }
      mGridPoints[mDomains[idom].gridPtInd[gpt]].areaFrac = areaSum /maxSum;
    }
  }
}

/*
 * Determine the density at all grid points from scratch
 */
void PickSeeds::computeDensities(int topBot, float H)
{
  Candidate *cand;
  int gpt, dom, ind, neigh, cpt;
  float dsum, dx, dy, dist;
  float hsqr = H * H;
  float densfac = 4. / (PI * hsqr);
  for (gpt = 0; gpt < mNumXgrid * mNumYgrid; gpt++) {
    dom = mGridPoints[gpt].domain;
    if (dom < 0)
      continue;
    dsum = 0.;
    for (neigh = 0; neigh < mDomains[dom].numNeighbors; neigh++) {
      ind = mDomains[dom].neighbors[neigh];
      for (cpt = 0; cpt < mDomains[ind].numAccepted; cpt++) {
        cand = &mCandidates[mAcceptList[mDomains[ind].candStartInd + cpt]];
        if (topBot > 1 || cand->topBot == topBot) {
          dx = cand->pos.x - mGridPoints[gpt].x;
          dy = cand->pos.y - mGridPoints[gpt].y;
          dist = dx * dx + dy * dy;
          if (dist < hsqr)
            dsum += pow(1. - dist / hsqr, 3.);
        }
      }
    }
    mGridPoints[gpt].density = densfac * dsum / mGridPoints[gpt].areaFrac;
  }
}

/*
 * When a candidate is accepted, add its contribution to the density around nearby grid
 * points
 */
void PickSeeds::reviseDensities(int acceptNew, float H)
{
  Candidate *cand;
  int ixlo, ixhi, iylo, iyhi, iy, ix, gpt;
  float dx, dy, dist;
  float hsqr = H * H;
  float densfac = 4. / (PI * hsqr);
  cand = &mCandidates[acceptNew];
  ixlo = B3DMAX(0, (int)((cand->pos.x - H) / mDelXgrid) - 1);
  ixhi = B3DMIN(mNumXgrid - 1, (int)ceil((cand->pos.x + H) / mDelXgrid) - 1);
  iylo = B3DMAX(0, (int)((cand->pos.y - H) / mDelYgrid) - 1);
  iyhi = B3DMIN(mNumYgrid - 1, (int)ceil((cand->pos.y + H) / mDelYgrid) - 1);
  for (iy = iylo; iy <= iyhi; iy++) {
    for (ix = ixlo; ix <= ixhi; ix++) {
      gpt = ix + iy * mNumXgrid;
      if (mGridPoints[gpt].domain >= 0) {
        dx = cand->pos.x - mGridPoints[gpt].x;
        dy = cand->pos.y - mGridPoints[gpt].y;
        dist = dx * dx + dy * dy;
        if (dist < hsqr)
          mGridPoints[gpt].density += densfac * pow(1. - dist/hsqr, 3.) / 
            mGridPoints[gpt].areaFrac;
      }
    }
  }
}

/*
 * Put out densities in a gnuplot format
 */
void PickSeeds::outputDensities(int topBot)
{
  static int lastPhase = -1;
  static int sequence;
  int ix, iy, ind;
  char *fname;
  FILE *fp;
  if (!mDensPlotName)
    return;
  if (mPhase != lastPhase) {
    sequence = 1;
    lastPhase = mPhase;
  } else
    sequence++;
  fname = B3DMALLOC(char, 30 + strlen(mDensPlotName));
  if (!fname)
    return;
  sprintf(fname, "%s-%d-%d-%d.dat", mDensPlotName, mPhase, topBot, sequence);
  fp = fopen(fname, "w");
  if (!fp)
    return;
  
  for (iy = 0; iy < mNumYgrid; iy++) {
    for (ix = 0; ix < mNumXgrid; ix++) {
      ind = ix + iy * mNumXgrid;
      fprintf(fp, "%f %f %f\n", mGridPoints[ind].x, mGridPoints[ind].y, 
             1.e6 * mGridPoints[ind].density);
    }
    fprintf(fp, "\n");
  }
  fclose(fp);
  free(fname);
}

/*
 * Output number of points accepted total and on each surface
 */
void PickSeeds::outputNumAccepted(int twoSurf, int final)
{
  if (final)
    printf("Final:   ");
  else
    printf("Phase %d: ", mPhase - mAppendToSeed);
  printf("total points accepted = %d", mNumAccepted[2]);
  if (twoSurf)
    printf("   -   on bottom = %d , on top = %d\n", mNumAccepted[0], mNumAccepted[1]);
  else
    printf("\n");
}


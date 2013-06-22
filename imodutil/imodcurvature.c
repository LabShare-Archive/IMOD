/*
 *  imodcurvature.c - encodes radius of curvature into model display
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"

static void loadWindowPoints(Iobj *obj, Icont *cont, int cenPt, float window, 
                             float sample, int needFull, float zscale, 
                             float *xx, float *yy, float *zz, int *numPts);
static int findConnectedContours(Iobj *obj, Icont *cont, int zdir, 
                                 Ilist *list);
static int encodeCurvature(Imod *mod, int obnum, float rCritLo, float rCritHi,
                           float window, float sample, float fitCrit,
                           int pointSize, float symZoom, int numCol,
                           unsigned char *red, unsigned char *green, 
                           unsigned char *blue, int storeVals, float zrange,
                           float zscale, int rotateWild, float cylSearch,
                           int verbose, int testCo, int TestPt, int printMean);
static int farthestPoint(float *xx, float *yy, int numPts, int icen);
static int fitCylinder(float *xx3, float *yy3, float *zz3, float *xxr,
                       float *yyr, int numPts, float delt, float *rad,
                       float *alpha, float *gamma, float *rmsErr,
                       Ipoint *axisPt1, Ipoint *axisPt2, int verbose);

/* Structures for keeping track of connected contours and the near contours
   to be used for a spherical fit */
typedef struct {
  int co;
  int z;
  int checked;
} ConnectedCont;

typedef struct {
  Icont *minCont;
  int minPt;
  int z;
  float cumz;
} NearCont;
  
/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  Imod *model;
  Iobj *obj;
  Icont *cont;
  Ipoint *pts;
  Ipoint point;
  float xx[10000], yy[10000];
  float xcen, ycen, rad, rmsErr, dum;
  int co, ob, i, newnum, ierr, numColUse, colorInd, numObj = 0;
  int testcurve = 0;
  int testCo = -1;
  int testPt = -1;
  int divColors = 0;
  int numColors = 0;
  int pointSize = 0;
  int storeVals = 0;
  int storeKappa = 0;
  int printMean = 0;
  int red, green, blue;
  unsigned char cmap[3][256];
  float fitCrit = 0.;
  float sample = 2.;
  int verbose = 0;
  int palette = 0;
  int rotateWild = 0;
  float zrange = 0;
  float cylSearch = 0.;
  float rCritLo, rCritHi, window;
  float symZoom = 0.;
  int *objList;
  int *rampData;
  char *listString;
  char *progname = imodProgName(argv[0]);
  char *filename;
  char *errString;
  int numOptArgs, numNonOptArgs;
  
  /* Fallbacks from    ../manpages/autodoc2man 2 1 imodcurvature  */
  int numOptions = 21;
  const char *options[] = {
    "in:InputFile:FN:", ":OutputFile:FN:", "wl:WindowLength:F:",
    "zr:ZRangeToFit:F:", "cy:CylinderSearchAngle:F:",
    "rc:RadiusCriterion:FP:", "fc:FitCriterion:F:", "ob:ObjectsToDo:LI:",
    "ro:RotateToXYPlane:B:", "st:StoreValues:B:", "kappa:KappaValues:B:",
    "ps:PointSize:B:", "sy:SymbolZoom:F:", "co:Color:ITM:",
    "di:DivideRange:B:", "pa:UsePalette:FN:", "sa:SampleSpacing:F:",
    "me:MeanStored:I:", "ve:Verbose:B:", "tc:TestCircleFits:B:",
    "ts:TestSphereFits:IP:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        3, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input and output files */
  if (PipGetInOutFile("InputFile", 0, &filename))
      exitError("No input file specified\n");

  model = imodRead(filename);
  if (!model) 
    exitError("Reading model %s\n", filename);
  free(filename);

  if (PipGetInOutFile("OutputFile", 1, &filename))
      exitError("No output file specified\n");

  /* Process options */
  PipGetBoolean("TestCircleFits", &testcurve);
  PipGetBoolean("RotateToXYPlane", &rotateWild);
  PipGetFloat("CylinderSearchAngle", &cylSearch);
  PipGetInteger("MeanStored", &printMean);
  
  if (!testcurve) {

    /* Criteria */
    if (PipGetTwoFloats("RadiusCriterion", &rCritLo, &rCritHi))
      exitError("You must enter low and high radius criteria");
    if (PipGetFloat("WindowLength", &window))
      window = rCritLo + rCritHi;
    PipGetFloat("FitCriterion", &fitCrit);
    PipGetFloat("SampleSpacing", &sample);
    PipGetFloat("ZRangeToFit", &zrange);
    if (zrange && rotateWild)
      exitError("You cannot do rotation of contours with fitting to spheres");
    if (cylSearch && !zrange) 
      exitError("You must enter a Z range to do cylinder fitting");
    
    /* Color options */
    PipGetBoolean("DivideRange", &divColors);
    palette = 1 - PipGetString("UsePalette", &listString);
    PipNumberOfEntries("Color", &numColors);
    if (numColors > 256) 
      exitError("You cannot enter more than 256 colors");
    if (palette) {
      if (numColors)
        exitError("You cannot enter both -co (colors) and -pa "
                  "(palette)");
      numColors = 256;
      divColors = 1;
    }

    if (numColors) {
      if (palette) {
        if (strstr("inverted", listString)) {
          rampData = cmapInvertedRamp();
          cmapConvertRamp(rampData, cmap);
        } else if (strstr("standard", listString)) {
          rampData = cmapStandardRamp();
          cmapConvertRamp(rampData, cmap);
        } else {
          if (cmapReadConvert(listString, cmap))
            exitError("Reading color map table");
          free(listString);
        }
      } else
        for (i = 0; i < numColors; i++) {
          PipGetThreeIntegers("co", &red, &green, &blue);
          cmap[0][i] = red;
          cmap[1][i] = green;
          cmap[2][i] = blue;
        }
    }
    
    /* Point size and object list */
    PipGetBoolean("PointSize", &pointSize);
    PipGetFloat("SymbolZoom", &symZoom);
    PipGetBoolean("StoreValues", &storeVals);
    PipGetBoolean("KappaValues", &storeKappa);
    if (storeKappa)
      storeVals = -1;
    if (!pointSize && !numColors && !symZoom && !storeVals)
      exitError("You must enter either -ps, -sy, -st or color options for "
                "point or color encoding");
    
    if (!PipGetString("ObjectsToDo", &listString)) {
      objList = parselist(listString, &numObj);
      free(listString);
      if (!objList)
        exitError("Bad entry in list of objects to do");
    }
    PipGetBoolean("Verbose", &verbose);
    PipGetTwoIntegers("TestSphereFits", &testCo, &testPt);
    if (testCo > 0 && testPt > 0 && zrange) {
      imodNewObject(model);
      imodNewContour(model);
      obj = imodObjectGet(model);
      obj->flags |= IMOD_OBJFLAG_SCAT | IMOD_OBJFLAG_OPEN;
      obj->pdrawsize = 1;
      testCo--;
      testPt--;
    } else {
      testCo = -1;
      testPt = -1;
    }
    
    /* Loop on the objects */
    imodObjviewComplete(model);
    colorInd = 0;
    for (ob = 0; ob < model->objsize; ob++) {
      obj = &model->obj[ob];
      ierr = 1;
      if (numObj) {
        ierr = 0;
        for (i = 0; i < numObj; i++)
          if (objList[i] - 1 == ob)
            ierr = 1;
      }
      if (!ierr || iobjScat(obj->flags))
        continue;

      if (zrange && !obj->mesh) {
        printf("Skipping object %d because it has no mesh\n", ob + 1);
        continue;
      }
      
      numColUse = numColors ? 1 : 0;
      if (numColors && divColors) {
        colorInd = 0;
        numColUse = numColors;
      }

      printf("Doing object %d...\n", ob + 1);
      if (encodeCurvature(model, ob, rCritLo, rCritHi, window, sample,
                          fitCrit, pointSize, symZoom, numColUse, 
                          &cmap[0][colorInd], &cmap[1][colorInd], 
                          &cmap[2][colorInd], storeVals, zrange,
                          model->zscale, rotateWild, cylSearch, verbose,
                          testCo, testPt, printMean))
        exitError("Error allocating memory in curvature routine");

      if (numColors && !divColors && colorInd < numColors - 1)
        colorInd++;
    }

  } else {

    /* Do test for circle fitting */
    imodNewObject(model);

    obj = &model->obj[0];
    imodNewContour(model);
  
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      pts = cont->pts;
      if (cont->psize < 3)
        continue;
      point.z = pts->z;
      if (circleThrough3Pts(pts[0].x, pts[0].y, pts[cont->psize / 2].x,
                            pts[cont->psize / 2].y, pts[cont->psize - 1].x,
                            pts[cont->psize - 1].y, &rad, &xcen, &ycen)) {
        printf("Contour %d: failed to fit to 3 points\n", co + 1);
      } else {
        printf("Contour %d: circle through 3 points (%.2f,%.2f) radius %.2f\n",
               co + 1, xcen, ycen, rad);
        for (i = 0; i < cont->psize; i++) {
          xx[i] = pts[i].x;
          yy[i] = pts[i].y;
        }
        
        fitSphere(xx, yy, NULL, cont->psize, &rad, &xcen, &ycen, &dum,
                  &rmsErr);
        printf("  Fit to %d points (%.2f,%.2f) radius %.2f  error %f\n",
               cont->psize, xcen, ycen, rad, rmsErr);
        point.x = xcen;
        point.y = ycen;
        newnum = imodNewPoint(model, &point);
        cont = imodContourGet(model);
        if (newnum > 0 && cont)
          imodPointSetSize(cont, newnum - 1, rad);
      }
    }
  }

  imodBackupFile(filename);
  if (imodOpenFile(filename, "wb", model))
    exitError("Opening new model %s\n", filename);
  imodWriteFile(model);
  printf("Objects must be remeshed to see changes in surface display\n");
  exit(0);
}

/*
 * Compute the curvature for all points in all contours of an object
 * with the multitude of parameters
 */
int encodeCurvature(Imod *model, int obnum, float rCritLo, float rCritHi,
                    float window, float sample, float fitCrit,
                    int pointSize, float symZoom, int numCol,
                    unsigned char *red, unsigned char *green, 
                    unsigned char *blue, int storeVals, float zrange,
                    float zscale, int rotateWild, float cylSearch, int verbose,
                    int testCo, int testPt, int printMean)
{
  Iobj *obj = &model->obj[obnum];
  Icont *cont;
  Icont *cont2;
  Icont *minCont;
  Icont *rcont;
  int co, pt, indCol, maxSamp, activeSym, numStore, obNumStore;
  float *xx, *yy, *zz = NULL;
  float *xxrot, *yyrot;
  float xcen, ycen, zcen, rad, rmsErr, dval, frac, radsto;
  float minDist, dist, cumz, subWind, valMin = 1.e30, valMax = -1.e30;
  int cenPt, meetsCrit, activeCol, numPts, minPt, zhalf, pt2, zdir, icheck;
  int numMidSlice, delz, j, newnum, maxZ, iud, minDiff, diffUp, diffDown;
  int diff, activeVal, rotated, needFull, numContDone, numPtDone, ifDidCont;
  Istore store;
  Ilist *list;
  Ipoint scale, *cenPoint, point, norm, axisPt1, axisPt2;
  double alpha, beta, radsum, obRadsum, radsq, obRadsq;
  float alphas, gammas, avgsto, sdsto;
  double dtor = 0.017453293;
  ConnectedCont conCont;
  ConnectedCont *conItem;
  NearCont *nearOnes;
  NearCont *nearPt;
  int numUpDown[2];
  Iobjview *obv;
  Imat *mat;

  scale.x = 1.;
  scale.y = 1.;
  scale.z = zscale;
  needFull = iobjOpen(obj->flags) ? -1 : 1;
  numContDone = 0;
  numPtDone = 0;
  obRadsum = 0.;
  obRadsq = 0.;
  obNumStore = 0;

  /* Make the arrays generously large */
  maxSamp = (int)(window / sample + 10.);
  if (zrange) {
    maxZ = (int)(zrange / zscale + 2.);
    maxSamp *= maxZ;
    zz = (float *)malloc(maxSamp * sizeof(float));
    nearOnes = (NearCont *)malloc(sizeof(NearCont) * maxZ);
    if (!zz || !nearOnes)
      return 1;
  }
  xx = (float *)malloc(maxSamp * sizeof(float));
  yy = (float *)malloc(maxSamp * sizeof(float));
  if (!xx || !yy)
    return 1;

  if (cylSearch) {
    xxrot = (float *)malloc(maxSamp * sizeof(float));
    yyrot = (float *)malloc(maxSamp * sizeof(float));
    if (!xxrot || !yyrot)
      return 1;
  }

  /* Loop on contours */
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    ifDidCont = 0;
    numStore = 0;
    radsum = 0.;
    radsq = 0.;

    if (cont->psize < 3 || (testCo >= 0 && testCo != co))
      continue;

    rotated = 0;
    rcont = cont;
    if (rotateWild) {

      /* If we are rotating wild contours, check all types and try to fit 
         plane to the contour */
      imodel_contour_check_wild(cont);
      if (cont->flags & ICONT_WILD) {
        if (imodContourFitPlane(cont, &scale, &norm, &dval, &alpha, &beta))
          continue;
        rcont = imodContourDup(cont);
        mat = imodMatNew(3);
        if (!rcont || !mat)
          return 1;
        
        /* Compose matrix and scale points.  There should be no need to scale 
           Z back after the rotation */
        imodMatId(mat);
        imodMatScale(mat, &scale);
        imodMatRot(mat, beta / dtor, b3dY);
        imodMatRot(mat, alpha / dtor, b3dX);
        for (pt = 0; pt < cont->psize; pt++)
          imodMatTransform(mat, &cont->pts[pt], &rcont->pts[pt]);
        rotated = 1;
      }
    } else {

      /* Otherwise skip wild open contours */
      if (iobjOpen(obj->flags)) {
        imodel_contour_check_wild(cont);
        if (cont->flags & ICONT_WILD)
          continue;
      }
    }

    if (verbose)
      printf("Doing contour %d...\n", co + 1);

    /* Clear an existing color if encoding color, clear symbols if those */
    if (numCol)
      istoreClearRange(cont->store, GEN_STORE_COLOR, 0, cont->psize - 1);
    if (symZoom) {
      istoreClearRange(cont->store, GEN_STORE_SYMSIZE, 0, cont->psize - 1);
      istoreClearRange(cont->store, GEN_STORE_SYMTYPE, 0, cont->psize - 1);
    }
    if (storeVals)
      istoreClearRange(cont->store, GEN_STORE_VALUE1, 0, cont->psize - 1);

    /* If doing spheres, get a list of connected contours in the z range */
    if (zrange) {
      zhalf = (int)(0.5 * zrange / zscale);
      list = ilistNew(sizeof(ConnectedCont), 2 * zhalf + 1);
      if (!list)
        return 1;
      conCont.co = co;
      conCont.z = imodContourZValue(cont);
      ilistAppend(list, &conCont);
      for (zdir = -1; zdir <= 1; zdir += 2) {
        icheck = 0;
        while (icheck < ilistSize(list)) {
          conItem = (ConnectedCont *)ilistItem(list, icheck++);
          if (zdir * (conItem->z - conCont.z) >= zhalf)
            conItem->checked = 1;
          if (icheck > 1 && conItem->checked)
            continue;
          conItem->checked = 1;
          findConnectedContours(obj, &obj->cont[conItem->co], zdir, list);
        }
      }
      
      if (verbose || testPt >= 0) {
        printf("contour %d   %d connected:\n", co + 1, ilistSize(list));
        for (j = 0; j < ilistSize(list); j++) {
          conItem = (ConnectedCont *)ilistItem(list, j);
          printf("%d %d %d\n", conItem->co + 1, conItem->z, conItem->checked);
        }
      }
      if (ilistSize(list) == 1)
        continue;
    }          

    /* Loop on points */
    activeCol = 0;
    activeSym = 0;
    activeVal = 0;
    for (cenPt = 0; cenPt < cont->psize; cenPt++) {

      if (testPt >= 0 && cenPt != testPt)
        continue;

      numPts = 0;
      meetsCrit = 0;
      loadWindowPoints(obj, rcont, cenPt, window, sample, needFull, zscale, xx,
                       yy, zz, &numPts);
      if (numPts > 2) {

        /* Fit to circle and test against criteria */
        if (circleThrough3Pts(xx[0], yy[0], xx[numPts / 2], yy[numPts / 2],
                               xx[numPts - 1], yy[numPts - 1], 
                               &rad, &xcen, &ycen)) {
          printf("Failed to find circle through 3 points for contour %d "
                 "pt %d\n", co + 1, cenPt + 1);
        } else {

          /* For spheres, load points from adjacent sections */
          meetsCrit = 1;
          if (zrange) {
            numMidSlice = numPts;
            for (zdir = -1; zdir <= 1; zdir += 2) {
              iud = (zdir + 1) / 2;
              numUpDown[iud] = 0;
              
              /* Start loop with the central point & cumulative z dist of 0 */
              cumz = 0.;
              cenPoint = &cont->pts[cenPt];
              for (delz = 1; delz <= zhalf; delz++) {
                minDist = 1.e20;

                /* For each Z level, find closest point on connected conts */
                for (j = 1; j < ilistSize(list); j++) {
                  conItem = (ConnectedCont *)ilistItem(list, j);
                  if (conItem->z != conCont.z + delz * zdir)
                    continue;
                  cont2 = &obj->cont[conItem->co];
                  for (pt2 = 0; pt2 < cont2->psize; pt2++) {
                    dist = imodPoint3DScaleDistance(cenPoint, &cont2->pts[pt2],
                                                    &scale);
                    if (dist < minDist) {
                      minDist = dist;
                      minCont = cont2;
                      minPt = pt2;
                    }
                  }
                }
                
                /* If got no points, continue to next Z, otherwise add minimum 
                   distance to cumulative, break the half-loop if
                   it is bigger than the window size */
                if (minDist >= 1.e20)
                  continue;
                cumz += minDist;
                if (cumz > window)
                  break;
                
                /* Save the nearest contour data */
                j = iud + 2 * numUpDown[iud];
                numUpDown[iud]++;
                nearOnes[j].cumz = cumz;
                nearOnes[j].minPt = minPt;
                nearOnes[j].minCont = minCont;
                nearOnes[j].z = conCont.z + delz * zdir;

                /* Set up center point for the next round */
                cenPoint = &minCont->pts[minPt];
              }
            }

            /* Require at least one contour on each side */
            if (numUpDown[0] && numUpDown[1]) {

              /* Balance contour extent in Z on each side */
              diffUp = nearOnes[1 + 2 *(numUpDown[1] - 1)].z - conCont.z;
              diffDown = conCont.z - nearOnes[2 * (numUpDown[0] - 1)].z;
              /*printf("numUpDown %d %d  diffDown diffUp %d  %d\n",
                numUpDown[0], numUpDown[1], diffDown, diffUp); */
              if (diffUp > diffDown) {
                iud = 1;
              } else {
                iud = 0;
                diffDown = diffUp;
              }
              minDiff = 10000;
              for (j = numUpDown[iud] - 1; j >= 0; j--) {
                diffUp = nearOnes[iud + 2 * j].z - conCont.z;
                if (diffUp < 0)
                  diffUp = -diffUp;
                diff = diffUp - diffDown;
                if (diff < 0)
                  diff = -diff;

                /* Find contour whose difference in Z from middle is closest
                   to the difference on the other side, set it as last one */
                if (diff < minDiff) {
                  minDiff = diff;
                  /*if (numUpDown[iud] != j + 1)
                    printf("Trimming %d to %d\n", iud, j + 1); */
                  numUpDown[iud] = j + 1;
                }
              }
                
              /* Loop on the selected contours now */
              for (iud = 0; iud < 2; iud++) {
                for (j = 0; j < numUpDown[iud]; j++) {
                  
                  /* Set window to remaining distance, get points */
                  nearPt = &nearOnes[iud + 2 * j];
                  subWind = (float)sqrt(window * window - 
                                        nearPt->cumz * nearPt->cumz);
                  loadWindowPoints(obj, nearPt->minCont, nearPt->minPt,
                                   subWind, sample, 0, zscale, xx, yy, zz, 
                                   &numPts);
                  /* printf("delz %.0f subwind %f  nummid %d numpts %d\n",
                     nearPt->cumz, subWind, numMidSlice, numPts); */
                }
              }
            }

            /* Require at least 3 points off the midslice on each side */
            zcen = conCont.z * zscale;
            if (numPts < numMidSlice + 6)
              meetsCrit = 0;
          }
          
          if (meetsCrit) {
            if (cylSearch) {
              j = 0;
              if (testPt >= 0)
                j = verbose ? 2 : 1;
              fitCylinder(xx, yy, zz, xxrot, yyrot, numPts, cylSearch, &rad,
                          &alphas, &gammas, &rmsErr, &axisPt1, &axisPt2, j);
              if (verbose)
                  printf("Center pt %3d  num= %3d  alpha %6.1f  gamma %6.1f  "
                         "radius %8.1f  err %6.1f\n", cenPt + 1, numPts,
                         alphas, gammas, rad,rmsErr);
            } else {
              fitSphere(xx, yy, zz, numPts, &rad, &xcen, &ycen, &zcen, 
                        &rmsErr);
              if (verbose || testPt >= 0) {
                if (zrange)
                  printf("Center pt %d  num= %d  (%.2f,%.2f,%.2f)  radius %.2f  "
                         "err %f\n", cenPt + 1, numPts, xcen,ycen,zcen,rad,
                         rmsErr);
                else
                  printf("Center pt %d  num= %d  (%.2f,%.2f)  radius %.2f  "
                         "err %f\n", cenPt + 1, numPts, xcen,ycen,rad,rmsErr);
              }
            }
          
            if (rad < rCritLo || rad > rCritHi || 
                (fitCrit > 0. && rmsErr > rad * fitCrit))
              meetsCrit = 0;

            /* Create the test points */
            if (testPt >= 0) {
              for (j = 0; j < numPts; j++) {
                point.x = xx[j];
                point.y = yy[j];
                point.z = zz[j] / zscale;
                imodNewPoint(model, &point);
              }
              cont2 = imodContourGet(model);
              if (cylSearch) {
                for (j = 0; j < 3; j++) {
                  frac = j / 2.;
                  point.x = frac * axisPt2.x + (1-frac) * axisPt1.x;
                  point.y = frac * axisPt2.y + (1-frac) * axisPt1.y;
                  point.z = (frac * axisPt2.z + (1-frac) * axisPt1.z) / zscale;
                  newnum = imodNewPoint(model, &point);
                  if (newnum > 0 && cont2)
                    imodPointSetSize(cont2, newnum - 1, rad);
                }
              } else {
                point.x = xcen;
                point.y = ycen;
                point.z = zcen / zscale;
                newnum = imodNewPoint(model, &point);
                if (newnum > 0 && cont2)
                  imodPointSetSize(cont2, newnum - 1, rad);
              }
            }
          }
        }
      }

      if (meetsCrit) {
        numPtDone++;
        ifDidCont++;
      }

      /* For points, set size or clear an existing size */
      if (pointSize) {
        if (meetsCrit) 
          imodPointSetSize(cont, cenPt, rad);
        else if (cont->sizes)
          imodPointSetSize(cont, cenPt, 0.);
      }

      /* For symbols, set radius with zoom */
      if (meetsCrit && symZoom) {
        store.value.i = (int)(rad * symZoom + 0.5);
        store.flags = 0;
        store.type = GEN_STORE_SYMSIZE;
        store.index.i = cenPt;
        istoreInsertChange(&cont->store, &store);
        store.value.i = IOBJ_SYM_CIRCLE;
        store.type = GEN_STORE_SYMTYPE;
        istoreInsertChange(&cont->store, &store);
        activeSym = 1;
      }

      /* For colors, find color index and add a change */
      if (meetsCrit && numCol) {
        indCol = (int)(numCol * (rad - rCritLo) / (rCritHi - rCritLo));
        indCol = B3DMIN((numCol - 1), indCol);
        store.value.b[0] = red[indCol];
        store.value.b[1] = green[indCol];
        store.value.b[2] = blue[indCol];
        store.flags = GEN_STORE_BYTE << 2;
        store.type = GEN_STORE_COLOR;
        store.index.i = cenPt;
        istoreInsertChange(&cont->store, &store);
        activeCol = 1;
      }

      /* For values, simply store them and keep track of min/max*/
      if (meetsCrit && storeVals) {
        radsto = rad * model->pixsize;
        if (storeVals < 0) {
          radsto = 1. / radsto;
          if (model->pixsize > 0.05)
            radsto *= 1000.;
        }
        store.value.f = radsto;
        store.flags = GEN_STORE_FLOAT << 2;
        store.type = GEN_STORE_VALUE1;
        store.index.i = cenPt;
        istoreInsertChange(&cont->store, &store);
        valMin = B3DMIN(valMin, radsto);
        valMax = B3DMAX(valMax, radsto);
        activeVal = 1;
        numStore = numStore + 1;
        radsum = radsum + radsto;
        radsq = radsq + radsto * radsto;
      }

      /* Otherwise end the change */
      if (!meetsCrit && activeCol) {
        istoreEndChange(cont->store, GEN_STORE_COLOR, cenPt);
        activeCol = 0;
      }
      if (!meetsCrit && activeSym) {
        istoreEndChange(cont->store, GEN_STORE_SYMSIZE, cenPt);
        istoreEndChange(cont->store, GEN_STORE_SYMTYPE, cenPt);
        activeSym = 0;
      }
      if (!meetsCrit && activeVal) {
        istoreEndChange(cont->store, GEN_STORE_VALUE1, cenPt);
        activeVal = 0;
      }

    }

    if (ifDidCont)
      numContDone++;

    if (zrange)
      ilistDelete(list);
    if (rotated) {
      imodMatDelete(mat);
      imodContourDelete(rcont);
    }

    obRadsum += radsum;
    obRadsq += radsq;
    obNumStore += numStore;
    if (numStore && printMean > 1) {
      sumsToAvgSDdbl(radsum, radsq, numStore, 1, &avgsto, &sdsto);
      printf("Contour %d, stored value mean and SD: %f  %f\n", co + 1, avgsto, 
             sdsto);
    }
  }

  if (obNumStore && printMean > 0) {
    sumsToAvgSDdbl(obRadsum, obRadsq, obNumStore, 1, &avgsto, &sdsto);
    printf("Object %d, stored value mean and SD: %f  %f\n", obnum + 1, avgsto, 
           sdsto);
  }

  if (zrange) {
    free(nearOnes);
    free(zz);
  }

  /* Store the min/max and set flags in the real object views */
  if (storeVals && valMin <= valMax) {
    if (verbose)
      printf("Storing value min %f  max %f\n", valMin, valMax);
    istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, valMin, valMax);
    obj->flags |= IMOD_OBJFLAG_MCOLOR | IMOD_OBJFLAG_USE_VALUE;
    if (!obj->valblack && !obj->valwhite)
      obj->valwhite = 255;
    for (j = 1 ; j < model->viewsize; j++) {
      obv = &model->view[j].objview[obnum];
      obv->flags |= IMOD_OBJFLAG_MCOLOR | IMOD_OBJFLAG_USE_VALUE;
      if (!obv->valblack && !obv->valwhite)
        obv->valwhite = 255;
    }
  }

  printf("Curvature within criteria encoded for %d points in %d contours\n",
         numPtDone, numContDone);
  free(xx);
  free(yy);
  if (cylSearch) {
    free(xxrot);
    free(yyrot);
  }
  return 0;
}

/*
 * Add a set of points at the given sample spacing from the contour, equally
 * distributed around the center point cenPt.  If needFull is set to 1 or -1,
 * return with no points if the full or half window length cannot be provided,
 * respectively.
 */
void loadWindowPoints(Iobj *obj, Icont *cont, int cenPt, float window, 
                      float sample, int needFull, float zscale, float *xx,
                      float *yy, float *zz, int *numPts)
{
  Ipoint *pts = cont->pts;
  int lastPt, ptBefore, ptAfter, nextPt, first, numContPts;
  float length, lenBefore, lenAfter, skip,segment, winActual;

  /* If contour has less than 3 points, just load center point unless full load
     is requested */
  if (cont->psize < 3) {
    if (needFull)
      return;
    if (zz)
      zz[*numPts] = pts[cenPt].z * zscale;
    xx[*numPts] = pts[cenPt].x;
    yy[(*numPts)++] = pts[cenPt].y;
    return;
  }

  /* Find previous point at half-window distance */
  numContPts = 1;
  lenBefore = 0.;
  ptBefore = cenPt;
  lastPt = cenPt;
  while (lenBefore < window / 2.) {
    ptBefore--;
    if (ptBefore < 0) {
      if (iobjOpen(obj->flags)) {
        ptBefore = 0;
        break;
      }
      ptBefore = cont->psize - 1;
    }
    if (ptBefore == cenPt)
      break;
    lenBefore += imodPointDistance(&pts[ptBefore], &pts[lastPt]);
    lastPt = ptBefore;
    numContPts++;
  }
  if (lenBefore < window / 2. && needFull > 0)
    return;

  /* Find next point at half-window distance */
  lenAfter = 0.;
  ptAfter = cenPt;
  lastPt = cenPt;
  while (lenAfter < window / 2.) {
    ptAfter++;
    if (ptAfter >= cont->psize) {
      if (iobjOpen(obj->flags)) {
        ptAfter--;
        break;
      }
      ptAfter = 0;
    }
    if (ptAfter == ptBefore)
      break;
    lenAfter += imodPointDistance(&pts[ptAfter], &pts[lastPt]);
    lastPt = ptAfter;
    numContPts++;
  }

  /* Skip if there is not enough length with needFull set, or if there are
     only two actual points which will give no curvature */
  if ((lenAfter < window / 2. && needFull > 0) || (numContPts < 3 && needFull) 
      || (lenBefore + lenAfter < window / 2. && needFull < 0))
    return;

  /* Load the sampled points into the arrays */
  skip = B3DMAX(0., lenBefore - window / 2.);
  length = 0.;
  lastPt = ptBefore;
  nextPt = (lastPt + 1) % cont->psize;
  segment = imodPointDistance(&pts[nextPt], &pts[lastPt]);
  first = 1;
  winActual = B3DMIN(lenBefore, window / 2.) + B3DMIN(lenAfter, window / 2.);
  while (length < winActual - sample / 2.) {
          
    /* If skip distance is past segment, advance to next segment */
    while (skip > segment) {

      /* But if at end of open contour, either return if the next desired 
         point is less than half the sample length past the last point, or
         add the last point in the contour */
      if (nextPt == cont->psize - 1 && iobjOpen(obj->flags)) {
        if (skip - segment < sample / 2.)
          return;
        skip = segment;
        break;
      }
      skip -= segment;
      lastPt = nextPt;
      nextPt = (lastPt + 1) % cont->psize;
      segment = imodPointDistance(&pts[nextPt], &pts[lastPt]);
    }
          
    /* Get interpolated point at skip distance.  Adjust for zero segment */
    if (segment < 1.e-5)
      segment = skip = 1.;
    if (zz)
      zz[*numPts] = pts[cenPt].z * zscale;
    xx[*numPts] = pts[lastPt].x + (pts[nextPt].x - pts[lastPt].x) * 
      skip / segment;
    yy[(*numPts)++] = pts[lastPt].y + (pts[nextPt].y - pts[lastPt].y) * 
      skip / segment;
          
    /* Add to skip and length */
    skip += sample;
    if (!first)
      length += sample;
    first = 0;
  }
}

/*
 * Find contours connected by a mesh to the given contour, in the given
 * z direction (-1 or +1), and add ConnectedCont entries to the list
 */
int findConnectedContours(Iobj *obj, Icont *cont, int zdir, Ilist *list)
{
  int numAdded = 0;
  int numOrig = ilistSize(list);
  ConnectedCont conCont;
  ConnectedCont *conItem;
  Ipoint *p1, *p2, *p3;
  Icont *cont2;
  Imesh *mesh;
  float zother, xx, yy, zz;
  int izOther, me, pt, i, j, k, pt2, co, found, onList, resol;
  int listInc, vertBase, normAdd, indStart;
  
  imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);

  /* Loop on points until one is found in a mesh */
  found = 0;
  for (pt = 0; pt < cont->psize && !found; pt++) {
    xx = cont->pts[pt].x;
    yy = cont->pts[pt].y;
    zz = cont->pts[pt].z;

    for (me = 0; me < obj->meshsize; me++) {
      mesh = &obj->mesh[me];
      if (!mesh || !mesh->lsize || 
          imeshResol(mesh->flag) != resol)
        continue;

      for (i = 0; i < mesh->lsize; i++){
        if (imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, 
                                    &normAdd)) {
          i++;
          indStart = i;
          while (mesh->list[i] != IMOD_MESH_ENDPOLY){
          
            p1 = &(mesh->vert[mesh->list[i + vertBase]]);
            i+=listInc;
            p2 = &(mesh->vert[mesh->list[i + vertBase]]);
            i+=listInc;
            p3 = &(mesh->vert[mesh->list[i + vertBase]]);
            i+=listInc;

            /* Find an exactly matching point */
            if ((xx == p1->x && yy == p1->y && zz == p1->z) ||
                (xx == p2->x && yy == p2->y && zz == p2->z) ||
                (xx == p3->x && yy == p3->y && zz == p3->z)) {

              if (zdir > 0) {
                zother = B3DMAX(p1->z, B3DMAX(p2->z, p3->z));
                if (zother > zz) 
                  found = 1;
              } else {
                zother = B3DMIN(p1->z, B3DMIN(p2->z, p3->z));
                if (zother < zz) 
                  found = 1;
              }
              
              /* Matching point was not in a polygon in the right direction
                 so break this polygon loop and keep looking */
              if (!found)
                break;

              /* Now look through this polygon for all the points at the other
                 Z value and then look for them in contours */
              izOther = (int)floor(zother + 0.5);
              if (izOther == (int)floor(zz + 0.5))
                izOther += zdir > 0 ? 1 : -1;
              j = indStart;
              while (mesh->list[j] != IMOD_MESH_ENDPOLY) {
                p1 = &(mesh->vert[mesh->list[j + vertBase]]);
                j += listInc;
                if (floor(p1->z + 0.5) != izOther)
                  continue;

                /* Look first in contours already on list */
                onList = 0;
                for (k = 0; k < numAdded && !onList; k++) {
                  conItem = (ConnectedCont *)ilistItem(list, k + numOrig);
                  cont2 = &obj->cont[conItem->co];
                  for (pt2 = 0; pt2 < cont2->psize; pt2++) {
                    if (cont2->pts[pt2].x == p1->x && 
                        cont2->pts[pt2].y == p1->y &&
                        cont2->pts[pt2].z == p1->z) {
                      onList = 1;
                      break;
                    }
                  }
                }

                if (onList)
                  continue;
                
                /* Then look at all other contours */
                onList = 0;
                for (co = 0; co < obj->contsize && !onList; co++) {
                  cont2 = &obj->cont[co];
                  /* Is this worth it? */
                  if (imodContourZValue(cont2) != izOther)
                    continue;
                  for (pt2 = 0; pt2 < cont2->psize; pt2++) {
                    if (cont2->pts[pt2].x == p1->x && 
                        cont2->pts[pt2].y == p1->y &&
                        cont2->pts[pt2].z == p1->z) {
                      onList = 1;
                      conCont.co = co;
                      conCont.z = izOther;
                      conCont.checked = 0;
                      ilistAppend(list, &conCont);  /* ERROR?  */
                      numAdded++;
                      break;
                    }
                  }
                }
              }

              /* Finished with points in polygon, return number conts found */
              return numAdded;
            }
          }
        }
      }
    }
  }

  /* Fell out of loop, no polygon or contours was found */
  return 0;
}

static int fitCylinder(float *xx3, float *yy3, float *zz3, float *xxr,
                       float *yyr, int numPts, float delt, float *rad,
                       float *alpha, float *gamma, float *rmsErr, 
                       Ipoint *axisPt1, Ipoint *axisPt2, int verbose)
{
  double gammat, alphat, delGamma, delAlpha;
  int numAlpha, numGamma, ig, ia, i, ifar1, ifar2, ifar3;
  Ipoint pt, rpt, minpt, maxpt;
  Imat *mat;
  float xcent, ycent, zcent, rmst, radt, area, areabase, areamax;
  double dtor = 0.017453293;
 
  /* Set up search angles: round up number to search in alpha to even */
  numAlpha = 2 * ((B3DNINT(180. / delt) + 1) / 2);
  delAlpha = 180. / numAlpha;
  *rmsErr = 1.e35;
  *rad = -1.;
  mat = imodMatNew(3);
  if (!mat)
    return 1;
  for (ia = 0; ia < numAlpha; ia++) {
    alphat = -90. + ia * delAlpha;
    numGamma = 2 *((B3DNINT(sin(fabs(dtor * alphat)) * 180. / delt) + 1) / 2);
    numGamma = B3DMAX(1, numGamma);
    delGamma = 180. / numGamma;
    for (ig = 0; ig < numGamma; ig++) {
      gammat = ig * delGamma;

      /* Get transformation and rotate points, effectively project down axis*/
      imodMatId(mat);
      imodMatRot(mat, gammat, b3dZ);
      imodMatRot(mat, alphat, b3dX);

      minpt.z = 1.e30;
      maxpt.z = -1.e30;
      for (i = 0; i < numPts; i++) {
        pt.x = xx3[i];
        pt.y = yy3[i];
        pt.z = zz3[i];
        imodMatTransform3D(mat, &pt, &rpt);
        xxr[i] = rpt.x;
        yyr[i] = rpt.y;
        if (verbose > 1)
          printf("%d %f %f\n", 100 *ia + ig, rpt.x, rpt.y);
        minpt.z = B3DMIN(minpt.z, rpt.z);
        maxpt.z = B3DMAX(maxpt.z, rpt.z);
      }

      /* To get 3 starting points, find point most distant from the first,
         then point most distant from that, then point that makes the largest
         triangle */
      ifar1 = farthestPoint(xxr, yyr, numPts, 1);
      ifar2 = farthestPoint(xxr, yyr, numPts, ifar1);
      areabase = (yyr[ifar2] + yyr[ifar1]) * (xxr[ifar2] - xxr[ifar1]);
      areamax = -1.;
      for (i = 0; i < numPts; i++) {
        if (i == ifar1 || i == ifar2)
          continue;
        area = areabase + (yyr[i] + yyr[ifar2]) * (xxr[i] - xxr[ifar2]) +
          (yyr[ifar1] + yyr[i]) * (xxr[ifar1] - xxr[i]);
        if (area > areamax) {
          areamax = area;
          ifar3 = i;
        }
      }

      /* forget this angle if the area is too small or the 3 point fit fails */
      if (areamax < 1.)
        continue;
      if (verbose > 1)
        printf("Fitting to (%f %f)  (%f %f)  (%f %f)\n", xxr[ifar1], 
        yyr[ifar1], xxr[ifar2], yyr[ifar2], xxr[ifar3], yyr[ifar3]);
      if (circleThrough3Pts(xxr[ifar1], yyr[ifar1], xxr[ifar2], yyr[ifar2], 
                              xxr[ifar3], yyr[ifar3], &radt, &xcent, &ycent))
        continue;
      if (verbose > 1)
        printf("Starting at rad %f  xcen %f  ycen %f\n", radt, xcent, ycent);
      fitSphere(xxr, yyr, NULL, numPts, &radt, &xcent, &ycent, &zcent, &rmst);
      if (verbose) 
        printf("%d  alf %5.1f gam %5.1f  rad %10.1f  err %10.3f %s\n", 
               100 *ia + ig, alphat,
               gammat, radt, rmst, rmst < *rmsErr ? "*" : " ");
      if (rmst < *rmsErr) {
        *rmsErr = rmst;
        *alpha = alphat;
        *gamma = gammat;
        *rad = radt;

        /* Back-transform axis points at min and max Z */
        imodMatId(mat);
        imodMatRot(mat, -alphat, b3dX);
        imodMatRot(mat, -gammat, b3dZ);
        minpt.x = maxpt.x = xcent;
        minpt.y = maxpt.y = ycent;
        imodMatTransform3D(mat, &minpt, axisPt1);
        imodMatTransform3D(mat, &maxpt, axisPt2);
      }
    }
  
  }
  imodMatDelete(mat);
  return 0;
}

static int farthestPoint(float *xx, float *yy, int numPts, int icen)
{
  float dx, dy, dist, distmax;
  int i, ifar;
  distmax = -1.;
   for (i = 0; i < numPts; i++) {
     dx = xx[i] - xx[icen];
     dy = yy[i] - yy[icen];
     dist = dx * dx + dy * dy;
     if (dist > distmax) {
       distmax = dist;
       ifar = i;
     }
   }
   return ifar;
}
   

/*

$Log$
Revision 3.14  2008/11/14 20:04:37  mast
Added option for output of mean stored values

Revision 3.13  2008/06/08 21:34:35  mast
Implemented cylinder fitting with inefficient search, fixed bug in deciding
if there are enough points in window.

Revision 3.12  2007/10/18 20:17:43  mast
Fixed log indentation

Revision 3.11  2007/10/01 15:39:10  mast
Moved circlefit to library for ease of sharing with SerialEM
  
Revision 3.10  2006/11/05 00:59:45  mast
Fixed treatment of ends of open contours
  
Revision 3.9  2006/11/02 22:04:09  mast
Fixed a precision problem in the 3 point fits and added summary output
  
Revision 3.8  2006/10/14 20:00:49  mast
Added option to rotate to plane, and let open contour fits use half window
length to fit all the way to end
  
Revision 3.7  2006/09/20 23:04:28  mast
Added callback for copyright to read/parse function call
  
Revision 3.6  2006/08/31 23:13:38  mast
Added value storage
  
Revision 3.5  2006/08/27 23:49:26  mast
Switched palette argument to allow a file, fixed initialization bug
  
Revision 3.4  2006/06/26 14:48:48  mast
Added b3dutil include for parselist
  
Revision 3.3  2006/06/18 19:37:11  mast
Changed for new amoeba function type
  
Revision 3.2  2006/06/14 14:21:01  mast
Needed to eliminate ;;
  
Revision 3.1  2006/06/14 04:28:43  mast
Initial creation

*/

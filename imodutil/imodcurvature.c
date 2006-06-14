/*
 *  imodcurvature.c - encodes radius of curvature into model display
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "parse_params.h"

static void loadWindowPoints(Iobj *obj, Icont *cont, int cenPt, float window, 
                             float sample, int needFull, float zscale, 
                             float *xx, float *yy, float *zz, int *numPts);
static int findConnectedContours(Iobj *obj, Icont *cont, int zdir, 
                                 Ilist *list);
static int circleThrough3Pts(float x1, float y1, float x2, float y2, float x3, 
                             float y3, float *rad, float *xc, float *yc);
static int fitSphere(float *xpt, float *ypt, float *zpt, int numPts,
                     float *rad, float *xcen, float *ycen, float *zcen,
                     float *rmsErr);
static float circleErr(float *y);
static float sphereErr(float *y);
static int encodeCurvature(Imod *mod, Iobj *obj, float rCritLo, float rCritHi,
                           float window, float sample, float fitCrit,
                           int pointSize, float symZoom, int numCol,
                           int *red, int *green, int *blue, float zrange,
                           float zscale, int verbose, int testCo, int TestPt);
static int load_cmap(int *red, int *green, int *blue);

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
  int co, ob, i, newnum, ierr, numColUse, colorInd, numObj;
  int testcurve = 0;
  int testCo = -1;
  int testPt = -1;
  int divColors = 0;
  int numColors = 0;
  int pointSize = 0;
  int *red = &co;
  int *green = &co;
  int *blue = &co;
  float fitCrit = 0.;
  float sample = 2.;
  int verbose = 0;
  int palette = 0;
  float zrange = 0;
  float rCritLo, rCritHi, window;
  float symZoom = 0.;
  int *objList;
  char *listString;
  char *progname = imodProgName(argv[0]);
  char *filename;
  char *errString;
  int numOptArgs, numNonOptArgs;
  
  /* Fallbacks from    ../manpages/autodoc2man 2 1 imodcurvature  */
  int numOptions = 16;
  char *options[] = {
    "in:InputFile:FN:", ":OutputFile:FN:", "wl:WindowLength:FP:",
    "zr:ZRangeToFit:F:", "rc:RadiusCriterion:FP:", "fc:FitCriterion:F:",
    "ob:ObjectsToDo:LI:", "ps:PointSize:B:", "sy:SymbolZoom:F:",
    "co:Color:ITM:", "di:DivideRange:B:", "pa:UsePalette:B:",
    "sa:SampleSpacing:F:", "ve:Verbose:B:", "tc:TestCircleFits:B:",
    "ts:TestSphereFits:IP:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        3, 1, 1, &numOptArgs, &numNonOptArgs);

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
  
  if (!testcurve) {

    /* Criteria */
    if (PipGetTwoFloats("RadiusCriterion", &rCritLo, &rCritHi))
      exitError("You must enter low and high radius criteria");
    if (PipGetFloat("WindowLength", &window))
      window = rCritLo + rCritHi;
    PipGetFloat("FitCriterion", &fitCrit);
    PipGetFloat("SampleSpacing", &sample);
    PipGetFloat("ZRangeToFit", &zrange);
    
    /* Color options */
    PipGetBoolean("DivideRange", &divColors);
    PipGetBoolean("UsePalette", &palette);
    PipNumberOfEntries("Color", &numColors);
    if (palette) {
      if (numColors)
        exitError("You cannot enter both -co (colors) and -pa "
                  "(palette)");
      numColors = 256;
      divColors = 1;
    }

    if (numColors) {
      red = (int *)malloc(numColors * sizeof(int));
      green = (int *)malloc(numColors * sizeof(int));
      blue = (int *)malloc(numColors * sizeof(int));
      if (palette)
        load_cmap(red, green, blue);
      else
        for (i = 0; i < numColors; i++)
          PipGetThreeIntegers("co", &red[i], &green[i], &blue[i]);
    }
    
    /* Point size and object list */
    PipGetBoolean("PointSize", &pointSize);
    PipGetFloat("SymbolZoom", &symZoom);
    if (!pointSize && !numColors && !symZoom)
      exitError("You must enter either -ps, -sy, or -co options for point "
                "or color encoding");
    
    if (!PipGetString("ObjectsToDo", &listString)) {
      objList = parselist(listString, &numObj);
      free(listString);
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
      if (encodeCurvature(model, obj, rCritLo, rCritHi, window, sample,
                          fitCrit, pointSize, symZoom, numColUse, 
                          &red[colorInd], &green[colorInd], &blue[colorInd],
                          zrange, model->zscale, verbose, testCo, testPt))
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
  exit(0);
}

/*
 * Compute the curvature for all points in all contours of an object
 * with the multitude of parameters
 */
int encodeCurvature(Imod *model, Iobj *obj, float rCritLo, float rCritHi, 
                    float window,
                    float sample, float fitCrit, int pointSize, float symZoom,
                    int numCol, int *red, int *green, int *blue, float zrange,
                    float zscale, int verbose, int testCo, int testPt)
{
  Icont *cont;
  Icont *cont2;
  Icont *minCont;
  int co, pt, indCol, maxSamp, activeSym;
  float *xx, *yy, *zz = NULL;
  float xcen, ycen, zcen, rad, rmsErr;
  float minDist, dist, cumz, subWind;
  int cenPt, meetsCrit, activeCol, numPts, minPt, zhalf, pt2, zdir, icheck;
  int numMidSlice, delz, j, newnum, maxZ, iud, minDiff, diffUp, diffDown;
  int diff;
  Istore store;
  Ilist *list;
  Ipoint scale, *cenPoint, point;
  ConnectedCont conCont;
  ConnectedCont *conItem;
  NearCont *nearOnes;
  NearCont *nearPt;
  int numUpDown[2];

  /* Make the arrays generously large */
  maxSamp = (int)(window / sample + 10.);
  if (zrange) {
    maxZ = (int)(zrange / zscale + 2.);
    maxSamp *= maxZ;
    zz = (float *)malloc(maxSamp * sizeof(float));
    scale.x = 1.;
    scale.y = 1.;
    scale.z = zscale;
    nearOnes = (NearCont *)malloc(sizeof(NearCont) * maxZ);
    if (!zz || !nearOnes)
      return 1;
  }
  xx = (float *)malloc(maxSamp * sizeof(float));
  yy = (float *)malloc(maxSamp * sizeof(float));
  if (!xx || !yy)
    return 1;

  /* Loop on contours */
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];

    /* Skip wild open contours */
    if (iobjOpen(obj->flags)) {
      imodel_contour_check_wild(cont);
      if (cont->flags & ICONT_WILD)
        continue;
    }

    if (cont->psize < 3 || (testCo >= 0 && testCo != co))
      continue;
    if (verbose)
      printf("Doing contour %d...\n", co + 1);

    /* Clear an existing color if encoding color, clear symbols if those */
    if (numCol)
      istoreClearRange(cont->store, GEN_STORE_COLOR, 0, cont->psize - 1);
    if (symZoom) {
      istoreClearRange(cont->store, GEN_STORE_SYMSIZE, 0, cont->psize - 1);
      istoreClearRange(cont->store, GEN_STORE_SYMTYPE, 0, cont->psize - 1);
    }

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
    for (cenPt = 0; cenPt < cont->psize; cenPt++) {

      if (testPt >= 0 && cenPt != testPt)
        continue;

      numPts = 0;
      meetsCrit = 0;
      loadWindowPoints(obj, cont, cenPt, window, sample, 1, zscale, xx, yy, zz,
                       &numPts);
      if (numPts) {

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
              /* printf("numUpDown %d %d  diffDown diffUp %d  %d\n",
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
                }
              }
            }

            /* Require at least 3 points off the midslice on each side */
            zcen = conCont.z * zscale;
            if (numPts < numMidSlice + 6)
              meetsCrit = 0;
          }
          
          if (meetsCrit) {
            fitSphere(xx, yy, zz, numPts, &rad, &xcen, &ycen, &zcen, &rmsErr);
            if (verbose || testPt >= 0) {
              if (zrange)
                printf("Center pt %d  num= %d  (%.2f,%.2f,%.2f)  radius %.2f  "
                       "err %f\n", cenPt + 1, numPts, xcen,ycen,zcen,rad,
                       rmsErr);
              else
                printf("Center pt %d  num= %d  (%.2f,%.2f)  radius %.2f  "
                       "err %f\n", cenPt + 1, numPts, xcen,ycen,rad,rmsErr);
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
              point.x = xcen;
              point.y = ycen;
              point.z = zcen / zscale;
              newnum = imodNewPoint(model, &point);
              cont2 = imodContourGet(model);
              if (newnum > 0 && cont2)
                imodPointSetSize(cont2, newnum - 1, rad);
            }
          }
        }
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

    }

    if (zrange)
      ilistDelete(list);
  }

  if (zrange) {
    free(nearOnes);
    free(zz);
  }

  free(xx);
  free(yy);
  return 0;
}

/*
 * Add a set of points at the given sample spacing from the contour, equally
 * distributed around the center point cenPt.  If needFull is set to 1,
 * return with no points if the full window length cannot be provided.
 */
void loadWindowPoints(Iobj *obj, Icont *cont, int cenPt, float window, 
                      float sample, int needFull, float zscale, float *xx,
                      float *yy, float *zz, int *numPts)
{
  Ipoint *pts = cont->pts;;
  int lastPt, ptBefore, ptAfter, nextPt;
  float length, lenBefore, lenAfter, skip,segment;

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
  lenBefore = 0.;
  ptBefore = cenPt;
  lastPt = cenPt;
  while (lenBefore < window / 2.) {
    ptBefore--;
    if (ptBefore < 0) {
      if (iobjOpen(obj->flags))
        break;
      ptBefore = cont->psize - 1;
    }
    if (ptBefore == cenPt)
      break;
    lenBefore += imodPointDistance(&pts[ptBefore], &pts[lastPt]);
    lastPt = ptBefore;
  }
  if (lenBefore < window / 2. && needFull)
    return;

  /* Find next point at half-window distance */
  lenAfter = 0.;
  ptAfter = cenPt;
  lastPt = cenPt;
  while (lenAfter < window / 2.) {
    ptAfter++;
    if (ptAfter >= cont->psize) {
      if (iobjOpen(obj->flags))
        break;
      ptAfter = 0;
    }
    if (ptAfter == ptBefore)
      break;
    lenAfter += imodPointDistance(&pts[ptAfter], &pts[lastPt]);
    lastPt = ptAfter;
  }
  if (lenAfter < window / 2. && needFull)
    return;

  /* Load the sampled points into the arrays */
  skip = lenBefore - window / 2.;
  length = 0.;
  lastPt = ptBefore;
  nextPt = (lastPt + 1) % cont->psize;
  segment = imodPointDistance(&pts[nextPt], &pts[lastPt]);
  while (length < window - sample / 2.) {
          
    /* If skip distance is past segment, advance to next segment */
    while (skip > segment) {
      skip -= segment;
      lastPt = nextPt;
      nextPt = (lastPt + 1) % cont->psize;
      segment = imodPointDistance(&pts[nextPt], &pts[lastPt]);
    }
          
    /* Get interpolated point at skip distance */
    if (zz)
      zz[*numPts] = pts[cenPt].z * zscale;
    xx[*numPts] = pts[lastPt].x + (pts[nextPt].x - pts[lastPt].x) * 
      skip / segment;
    yy[(*numPts)++] = pts[lastPt].y + (pts[nextPt].y - pts[lastPt].y) * 
      skip / segment;
          
    /* Add to skip and length */
    skip += sample;
    if (*numPts > 1)
      length += sample;
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


/* Determinant of 3x3 matrix */
#define determ3(a1,a2,a3,b1,b2,b3,c1,c2,c3) ((a1)*(b2)*(c3) - (a1)*(b3)*(c2) +\
  (a2)*(b3)*(c1) - (a2)*(b1)*(c3) + (a3)*(b1)*(c2) - (a3)*(b2)*(c1))

/*
 * Compute the radius rad and center xc, yc for a circle through the 3 given
 * points
*/
int circleThrough3Pts(float x1, float y1, float x2, float y2, float x3, 
                      float y3, float *rad, float *xc, float *yc)
{
  double a, d, e, f, x1sq, sq1, sq2, sq3;
  double absa, rsq;
  sq1 = x1 * x1 + y1 * y1;
  sq2 = x2 * x2 + y2 * y2;
  sq3 = x3 * x3 + y3 * y3;
  /* printf("x,y,sq: %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n",
     x1, y1, sq1, x2, y2, sq2, x3, y3, sq3); */

  a = determ3(x1, y1, 1.f, x2, y2, 1.f, x3, y3, 1.f);
  d = -determ3(sq1, y1, 1.f, sq2, y2, 1.f, sq3, y3, 1.f);
  e = determ3(sq1, x1, 1.f, sq2, x2, 1.f, sq3, x3, 1.f);
  f = -determ3(sq1, x1, y1, sq2, x2, y2, sq3, x3, y3);
  /* printf("a, d, e, f: %f  %f  %f  %f\n", a, d, e, f); */
  absa = fabs((double)a);
  if (absa < 1.e-15 * fabs((double)d) || absa < 1.e-15 * fabs((double)e) ||
      absa < 1.e-15 * fabs((double)f))
    return 1;
  *xc = -0.5f * d / a;  
  *yc = -0.5f * e / a;
  rsq = (d * d + e * e) / (4. * a * a) - f / a;
  if (rsq < 0.)
    return 1;
  *rad = (float)sqrt(rsq);
  return 0;
}


/* Static variables to make the data accessible to the error function */
static float *xp;
static float *yp;
static float *zp;
static int numpt;
#define MAXVAR 5

/* 
 * Fit a circle or sphere to a set of points.
 * Inputs: X and Y coordinates in arrays xpt, ypt; zpt is NULL for a circle fit
 * or has the array of Z coordinate; number of points in numPts.  
 * Outputs: radius in rad, center coordinate in xcen, ycen, (zcen),
 * and RMS error in rmsErr.
 */
int fitSphere(float *xpt, float *ypt, float *zpt, int numPts, float *rad, 
              float *xcen, float *ycen, float *zcen, float *rmsErr)
{
  float pp[MAXVAR + 1][MAXVAR + 1], yy[MAXVAR + 1];
  float ptmp[MAXVAR], ptol[MAXVAR], a[MAXVAR];
  int iter, jmin, i, j, nvar;
  float errmin, ftol1, ftol2, delfac, ptol1, ptol2;
  float da[MAXVAR] = {2., 2., 2., 2.};
  float (*funk)(float *) = circleErr;
  
  delfac = 2.;
  ftol2 = 5.e-4;
  ftol1 = 1.e-5;
  ptol2 = 0.1;
  ptol1 = .002;
  nvar = 3;

  xp = xpt;
  yp = ypt;
  numpt = numPts;
  a[0] = *rad;
  a[1] = *xcen;
  a[2] = *ycen;

  if (zpt) {
    zp = zpt;
    a[3] = *zcen;
    funk = sphereErr;
    nvar = 4;
  }

  errmin = funk(a);
  if (errmin > 0.) {
    amoebaInit(&pp[0][0], yy, MAXVAR + 1, nvar, delfac, ptol2, a, da, funk,
               ptol);
    amoeba(&pp[0][0], yy, MAXVAR + 1, nvar, ftol2, funk, &iter, ptol, &jmin);
    for (i = 0; i < nvar; i++)
      a[i] = pp[i][jmin];

    amoebaInit(&pp[0][0], yy, MAXVAR + 1, nvar, delfac, ptol1, a, da, funk,
               ptol);
    amoeba(&pp[0][0], yy, MAXVAR + 1, nvar, ftol1, funk, &iter, ptol, &jmin);
    for (i = 0; i < nvar; i++)
      a[i] = pp[i][jmin];
    errmin = funk(a);
  }

  *rad  = a[0];
  *xcen  = a[1];
  *ycen  = a[2];
  if (zpt)
    *zcen = a[3];
  *rmsErr = (float)sqrt((double)errmin);
  return 0;
}

/* Function to compute the error of the circle fit for given vector y */
static float circleErr(float *y)
{
  float xcen, ycen, rad;
  double delx, dely, delrad, err;
  int i;

  rad = y[0];
  xcen = y[1];
  ycen = y[2];
  err = 0.;
  for (i = 0; i < numpt; i++) {
    delx = xp[i] - xcen;
    dely = yp[i] - ycen;
    delrad = sqrt(delx * delx + dely * dely) - rad;
    err += delrad * delrad;
  }

  return (float)(err / numpt);
}

/* Function to compute the error of the sphere fit for given vector y */
static float sphereErr(float *y)
{
  float xcen, ycen, zcen, rad;
  double delx, dely, delz, delrad, err;
  int i;

  rad = y[0];
  xcen = y[1];
  ycen = y[2];
  zcen = y[3];
  err = 0.;
  for (i = 0; i < numpt; i++) {
    delx = xp[i] - xcen;
    dely = yp[i] - ycen;
    delz = zp[i] - zcen;
    delrad = sqrt(delx * delx + dely * dely + delz * delz) - rad;
    err += delrad * delrad;
  }

  return (float)(err / numpt);
}


/* Create a 256-color false color map - simplified from imodv_ogl.cpp */
static int load_cmap(int *red, int *green, int *blue)
{
  int rampData[] =
    { 
      15,
      255,    0,   90,    0,
      255,   45,   55,   20,
      255,  105,    0,   83,
      255,  175,    0,  162,
      255,  255,    0,  229,
      239,  255,    0,  240,
      191,  255,    0,  259,
      90,  255,   60,  305,
      0,  207,   78,  361,
      0,  191,  143,  383,
    0,  175,  177,  400,
      60,   96,  255,  469,
    120,   40,  255,  528,
      179,    0,  255,  569,
      255,    0,  255,  616,
    };

  int nline;
  int *inramp;
  int i,l;
  float tabscl,terpfc,tabpos;
  int indtab;
  nline = *rampData;
  inramp = rampData + 1;

  tabscl = (inramp[(nline * 4) - 1] - inramp[3])/255.0;
  indtab = 0;
  for(i = 0; i < 256; i++){
    tabpos = i * tabscl + inramp[3];
    if (tabpos > inramp[((indtab+1) * 4) + 3]){
      indtab++;
      if (indtab > nline - 2)
        indtab--;
    }

    terpfc = (tabpos - inramp[(indtab * 4) + 3])/
      (inramp[((indtab+1) * 4) + 3] - inramp[(indtab * 4) + 3]);

    red[i] = (unsigned char)((1 - terpfc) * inramp[(indtab * 4)] +
      terpfc * inramp [((indtab+1) * 4)]);
    green[i] = (unsigned char)((1 - terpfc) * inramp[(indtab * 4) + 1] +
      terpfc * inramp [((indtab+1) * 4) + 1]);
    blue[i] = (unsigned char)((1 - terpfc) * inramp[(indtab * 4) + 2] +
      terpfc * inramp [((indtab+1) * 4) + 2]);

  }
  return(0);
}

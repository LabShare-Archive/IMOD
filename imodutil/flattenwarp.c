/*
 *  flattenwarp.c - Computes warping transforms to flatten a volume
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2008-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end of file
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifndef _WIN32
#include <sys/types.h>
#include <unistd.h>
#endif
#include "imodel.h"
#include "mkmesh.h"
#include "b3dutil.h"
#include "parse_params.h"
#include "lsqr.h"
#include "sparselsqr.h"
#include "lapackc.h"

/* Structure for storing contour data */
typedef struct {
  Icont *cont;
  int yval;
  Icont *cont2;
  float xmin;
  float xmax;
  Icont *cdup;
} ContData;

/* Structure for storing warping data */
typedef struct {
  float xpos;
  float ypos;
  float aa;
  float bb;
  float dx;
  float dy;
  float dz;
} WarpData;

/* Static declarations */
static int interpolateCont(Icont *cont, float xval, float *zval);
static int interpolateCont2(Icont *cont, float xval, float window,
                            float *zval);
static int indInArray(int *indWarp, int numXloc, int numYloc, int i, int j);
static void reportLsqr(char *dd, int istop, int itndone, double anorm);
static int prepareTPS(int numPoints, Ipoint **fitPts, double **yvec, 
                      double **lmat, int **ipiv, double **work);
static void fitTPS(Ipoint *fitPts, int numPoints, float lambda, Ipoint *tpsScl,
                   double *yvec, double *lmat, int *ipiv, double *work, 
                   int lwork);
static float evaluateTPS(float x, float y, Ipoint *fitPts, int numPoints,
                         Ipoint *tpsScl, double *yvec);
static void setTPSscaling(Ipoint *fitPts, int numPoints, Ipoint *tpsScl);
static void setupWarpGrid
(float xmin, float xmax, float ymin, float ymax, int xyBinning, int zBinning,
 Imod *model, float *xSpacing, float *ySpacing, int *numXloc, int *numYloc,
 float *xcen, float *ycen, float *zcen, WarpData **warps, int **indWarp);
static void adjustAndMeshObj(Iobj *obj, float lambda, Ipoint *scale,
                             int showcont, char *prefix);
static void finishOutputModel(char *midfile, Imod *midmod, Imod *model, 
                              Ipoint *scale, int flipped, int numLambdas,
                              int restore);
static void  rotateModel(Imod *imod, int dir);

/* Useful macros */
#define INDWARP(a,b) indInArray(indWarp, numXloc, numYloc, (a), (b))
#define NEWOBJECT if (imodNewObject(midmod)) \
  exitError("Error creating new object in middle contour model");
#define NEWCONTOUR(a)  a = imodContourNew(); \
      if (!a) \
        exitError("Allocating new contour");

/* Error message defines */
#define APPEND_ERROR "Adding point to contour for output model"
#define ADDCONT_ERROR "Error adding new contour to model for middle contours"

#define MAX_LAMBDAS 100

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  Imod *model, *midmod;
  Icont *cont, *ncont, *cdup, *cont1 = NULL, *cont2 = NULL;
  Iobj *obj;
  char *progname = imodProgName(argv[0]);
  char *filename;
  int numOptArgs, numNonOptArgs;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 flattenwarp  */
  int numOptions = 12;
  const char *options[] = {
    "input:InputFile:FN:", "output:OutputFile:FN:",
    "patch:PatchOutputFile:FN:", "middle:MiddleContourFile:FN:",
    "binning:BinningOfTomogram:IP:", "one:OneSurface:B:",
    "flip:FlipOption:I:", "spacing:WarpSpacingXandY:FP:",
    "lambda:LambdaForSmoothing:FA:", "show:ShowContours:B:",
    "restore:RestoreOrientation:B:", ":PID:B:"};

  /* Maximum locations to output.  It could be 200000, but limit it to keep
     linear equation system from getting truly enormous */ 
  int maxLocations = 50000;
  float xSpacing = 0, ySpacing = 0;
  float xSpaceFac = 4.5f, ySpaceFac = 3, resampleXYfac = 0.67f;
  float resampWindowFac = 1.5f;
  int xyBinning = 1, zBinning = 1;
  int oneSurface = 0;
  int flipyz = -1, flipped = 0, restoreOrientation = 0;
  int PID = 0;
  int scattered = 0, showcont = 0;
  int dropOutliers = 1;
  float critMADN = 2.5;
  int minNumScat = 6;
  float scatSpaceFac = 0.33f;
  float fracOmit = 0.0;
  Ilist *clist = ilistNew(sizeof(ContData), 1000);
  char *patchfile = NULL;
  char *midfile = NULL;
  char *namePrefix[3];
  char *topPrefix = "Top, ";
  char *botPrefix = "Bottom, ";
  char *midPrefix = "Middle, ";
  char *emptyString = "";
  ContData cdata;
  ContData *cdptr, *cdptr2;
  WarpData *warps;
  FILE *fp;
  Ipoint minpt, maxpt, tpsScl, ptadd, axisvec, scale = {1., 1., 10.};
  Imat *mat = imodMatNew(3);
  float *mdata = mat->data;
  float xp[9], yp[9], zp[9], valRow[5];
  int icolRow[5];
  int *indWarp;
  float lambda[MAX_LAMBDAS];
  double *lmat, *yvec;
  int numLambdas = 0;
  int *iwrk, *ia, *ja;
  float *rwrk, *sumEntries;
  double *xx, *uu1, *uu2, *ww, *vv;
  double *work, *scatVec[2];
  int numScat[2], numScatFit[2];
  Ipoint *fitPts, *scatPts[2], scatScl[2];
  Icont *scanCont[2];
  float meanZscat[2];
  float *scx, *scy, *bx, *by;
  int *ipiv;
  float xmin, xmax, ymin, ymax, zval, zval2, frac, xcen, ycen, zcen,xloc, yloc;
  int iyval, i, j, planar, found, co, ob, pt, minint, numXloc;
  int numYloc, numLoc, indc, ind00, ind01, ind02, ind10, ind11, ind12;
  int ind20, ind21, ind22, ndat, ix, iy, delind,ind2, indmin, ind, numTied;
  int maxRows, maxVals, numRows, numInRow, err, itnlim, itndone, istop;
  int maxPoints;
  float dx, dy, dist, distmin, dxScale, dyScale, dxyScale, a11, a12, a21, a22;
  double zsum, axis, stretch, cosphi, sinphi, cosphisq, sinphisq, angle;
  float coef, aa, bb, cc, zmid, alpha, localYspace, resampX, windowX;
  float medianDev, MADN;
  double atol, btol, conlim, anorm, acond, rnorm, arnorm, xnorm;
  int numPoints, lwork, numBound, numObj, loop;
  Ipoint *ptp;
  float boundArea, scmin, scmax, xleft, xright, xcenpts, ycenpts, pad;

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        2, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input and output files */
  if (PipGetInOutFile("InputFile", 0, &filename))
    exitError("No input file specified");

  model = imodRead(filename);
  if (!model) 
    exitError("Reading model %s", filename);
  free(filename);

  err = PipGetFloatArray("LambdaForSmoothing", &lambda[0], &numLambdas, 
                         MAX_LAMBDAS);
  if (PipGetInOutFile("OutputFile", 1, &filename) && numLambdas < 2)
    exitError("No output file specified");

  /* Process options */
  PipGetTwoFloats("WarpSpacingXandY", &xSpacing, &ySpacing);
  PipGetTwoIntegers("BinningOfTomogram", &xyBinning, &zBinning);
  PipGetBoolean("OneSurface", &oneSurface);
  PipGetInteger("FlipOption", &flipyz);
  PipGetString("PatchOutputFile", &patchfile);
  PipGetString("MiddleContourFile", &midfile);
  PipGetBoolean("ShowContour", &showcont);
  PipGetBoolean("RestoreOrientation", &restoreOrientation);
  dropOutliers = 1 - PipGetFloat("CriterionForOutliers", &critMADN);
  PipGetBoolean("PID", &PID);
  if (PID) {
    fprintf(stderr, "Shell PID: %d\n", getpid());
    fflush(stderr);
  }

  if (numLambdas > 1 && midfile == NULL)
    exitError("You must enter an output model file for smoothed contours if "
              "entering multiple lambdas");

  if (!clist)
    exitError("Getting memory for list of contours");

  /* Start output model now before splitting out on type of model */
  if (midfile) {
    midmod = imodNew();
    if (!midmod)
        exitError("Error creating new model for middle contours");
  }
  
  /* Determine if scattered point objects are present */
  if (iobjScat(model->obj[0].flags)) {

    /* THE MODEL HAS SCATTERED POINTS */

    scattered = 1;
    numObj = model->objsize;
    if (numObj > 2)
      exitError("A model with scattered points must have either one or "
                "two objects");
    if (numObj > 1 && !iobjScat(model->obj[1].flags))
      exitError("The first object is scattered points but the second is "
                "not");
    if (!numLambdas)
      exitError("You must specify a lambda for smoothing with scattered "
                "points");
    
    /* Manage the flip/rotation state of model: first flip to native coords */
    if (model->flags & IMODF_FLIPYZ)
      imodFlipYZ(model);

    /* Then rotate if necessary unless user directs otherwise */
    if ((flipyz < 0 && model->zmax > model->ymax) || flipyz > 0) {
      if (flipyz == 1) {
        imodFlipYZ(model);
        flipped = 1;
      } else {
        rotateModel(model, -1);
        flipped = 2;
      }
    }
    
    /* Count points in each object and get bounding box for min/max */
    numPoints = 0;
    maxPoints = 0;
    ymin = xmin = -1.e20;
    ymax = xmax = -ymin;
    for (ob = 0; ob < numObj; ob++) {
      imodObjectGetBBox(&model->obj[ob], &minpt, &maxpt);
      xmin = B3DMAX(xmin, minpt.x);
      xmax = B3DMIN(xmax, maxpt.x);
      ymin = B3DMAX(ymin, minpt.y);
      ymax = B3DMIN(ymax, maxpt.y);
      numScat[ob] = 0;
      for (co = 0; co < model->obj[ob].contsize; co++)
        numScat[ob] += model->obj[ob].cont[co].psize;
      numPoints += numScat[ob];
      maxPoints = B3DMAX(maxPoints, numScat[ob]);
      if (numScat[ob] < minNumScat)
        exitError("There must be at least %d points in each scattered point "
                  "object", minNumScat);
    }

    /* Copy points into arrays for convex bound */
    boundArea = 0.;
    bx = (float *)malloc(maxPoints * sizeof(float));
    by = (float *)malloc(maxPoints * sizeof(float));
    for (ob = 0; ob < numObj; ob++) {
      scx = (float *)malloc(numScat[ob] * sizeof(float));
      scy = (float *)malloc(numScat[ob] * sizeof(float));
      if (!scx || !scy || !bx || !by)
        exitError("Allocating arrays for boundary contour analysis");
      i = 0;
      meanZscat[ob] = 0.;
      for (co = 0; co < model->obj[ob].contsize; co++) {
        cont = &model->obj[ob].cont[co];
        for (pt = 0; pt < cont->psize; pt++) {
          scx[i] = cont->pts[pt].x;
          scy[i++] = cont->pts[pt].y;
          meanZscat[ob] += cont->pts[pt].z / numScat[ob];
        }
      }

      /* Get convex boundary and convert to a scan contour */
      pad = 0.5 * sqrt((double)(xmax - xmin) * (ymax - ymin) / numScat[ob]);
      convexBound(scx, scy, numScat[ob], fracOmit, pad, bx, by, &numBound,
                  &xcenpts, &ycenpts, numScat[ob]);
      NEWCONTOUR(cont);
      cont->pts = (Ipoint *)malloc(numBound * sizeof(Ipoint));
      if (!cont->pts) 
        exitError("Allocating points for boundary contour");
      for (i = 0; i < numBound; i++) {
        cont->pts[i].x = bx[i];
        cont->pts[i].y = by[i];
        cont->pts[i].z = 0.;
      }
      cont->psize = numBound;
      scanCont[ob] = imodel_contour_scan(cont);
      if (!scanCont[ob])
        exitError("Getting scan contour from boundary contour");
      boundArea += imodContourArea(cont);
      free(scx);
      free(scy);
      /* Leave bx, by for outlier detection */
      imodContourDelete(cont);
    }
    
    if (xSpacing && ySpacing) {
      if ((1. + (ymax - ymin) / ySpacing) * (1. + (xmax - xmin) / xSpacing) 
          >= maxLocations)
        exitError("Your spacings between grid points are too small for "
                  "warpvol");
      found = 0;
    } else {

      /* Set spacing if not specified */
      xSpacing = ySpacing = (float)(scatSpaceFac *sqrt(boundArea / numPoints));
      found = 1;
      /* printf("%f  %f %f  %f  %f\n", xSpacing, xmin, xmax, ymin, ymax); */
      if ((1. + (ymax - ymin) / ySpacing) * (1. + (xmax - xmin) / xSpacing) 
          >= maxLocations) {
        xSpacing = (float)sqrt((ymax + 2. * ySpacing - ymin) * 
                               (xmax+ ySpacing - xmin) / maxLocations);
        ySpacing = xSpacing;
        printf("Setting grid spacing to %.1f based on maximum allowed grid "
               "points\n", xSpacing);
      } else
        printf("Setting grid spacing to %.1f based on mean distance between "
               "points\n", xSpacing);
    }
      

    setupWarpGrid(xmin, xmax, ymin,ymax, xyBinning, zBinning, model, &xSpacing,
                  &ySpacing, &numXloc, &numYloc, &xcen, &ycen, &zcen, &warps,
                  &indWarp);
    if (numXloc < 3 || numYloc < 3)
      exitError("The number of grid positions is too small; %s", 
                found ? "there are too few data points" : 
                "try reducing the grid spacing");

    /* Set up the warp positions and determine whether each is feasible */

    numLoc = 0;
    for (j = 0; j < numYloc; j++) {
      yloc = ymin + j * ySpacing;
      xleft = -1.e20;
      xright = 1.e20;
      iyval = B3DNINT(yloc);
      for (ob = 0; ob < numObj; ob++) {
        
        /* Find extent of scan contours at this Y value */
        scmin = 1.e20;
        scmax = -1.e20;
        for (pt = 0; pt < scanCont[ob]->psize; pt++)
          if (B3DNINT(scanCont[ob]->pts[pt].y) == iyval) {
            scmin = B3DMIN(scmin, scanCont[ob]->pts[pt].x);
            scmax = B3DMAX(scmax, scanCont[ob]->pts[pt].x);
          }
        
        /* AND the scan contour extents if there are two */
        xleft = B3DMAX(xleft, scmin);
        xright = B3DMIN(xright, scmax);
      }
      for (i = 0; i < numXloc; i++) {
        xloc = xmin + i * xSpacing;
        if (xloc >= xleft && xloc <= xright) {
          warps[numLoc].xpos = xloc * xyBinning - xcen;
          warps[numLoc].ypos = yloc * xyBinning - ycen;
          
          warps[numLoc].aa = -999.;
          indWarp[i + j * numXloc] = numLoc++;
        } else
          indWarp[i + j * numXloc] = -1;
      }
    }

    /* If doing output model, duplicate the point objects */
    if (midfile) {
      for (ob = 0; ob < numObj; ob++) {
        NEWOBJECT;
        obj = imodObjectDup(&model->obj[ob]);
        if (!obj)
          exitError("Duplicating scattered point object for output model");
        obj->flags |= IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |
          IMOD_OBJFLAG_FILL;
        if (numObj == 1)
          sprintf(obj->name, "Bead positions");
        else
          sprintf(obj->name, "%s bead positions", 
                  meanZscat[ob] > meanZscat[1-ob] ? "Top" : "Bottom");
          
        imodObjectCopy(obj, &midmod->obj[ob]);
        free(obj);
      }
      namePrefix[0] = numObj > 1 ? midPrefix : emptyString;
      if (numObj > 1) {
        namePrefix[1] = meanZscat[0] > meanZscat[1] ? topPrefix : botPrefix;
        namePrefix[2] = meanZscat[0] > meanZscat[1] ? botPrefix : topPrefix;
      }

      /* And make all the other objects */
      for (ob = 0; ob < numLambdas * (1 + 2 * (numObj - 1)); ob++)
        NEWOBJECT;
      midmod->cindex.object = numObj;
    }

    /* Now loop on TPS fits */
    for (indc = 0; indc < numLambdas; indc++) {
      for (ob = 0; ob < numObj; ob++) {

        /* Get the arrays for this number of points */
        lwork = prepareTPS(numScat[ob], &scatPts[ob], &scatVec[ob], &lmat,
                           &ipiv, &work);
        
        /* Load the points into fit point array */
        j = 0;
        for (co = 0; co < model->obj[ob].contsize; co++) {
          cont = &model->obj[ob].cont[co];
          for (pt = 0; pt < cont->psize; pt++)
            scatPts[ob][j++] = cont->pts[pt];
        }
        setTPSscaling(scatPts[ob], numScat[ob], &scatScl[ob]);
      
        printf("Fitting thin plate spline to obj %d, log lambda = %.2f:", 
               ob + 1, lambda[indc]);

        /* If doing outliers, loop twice, eliminate outliers from array the
           first time */
        numScatFit[ob] = numScat[ob];
        for (loop = 0; loop <= dropOutliers; loop++) {
          fitTPS(scatPts[ob], numScatFit[ob], lambda[indc], &scatScl[ob], 
                 scatVec[ob], lmat, ipiv, work, lwork);
          alpha = 0.;
          for (j = 0; j < numScat[ob]; j++) {
            zval = evaluateTPS(scatPts[ob][j].x, scatPts[ob][j].y, scatPts[ob],
                               numScatFit[ob], &scatScl[ob], scatVec[ob]);
            bx[j] = zval - scatPts[ob][j].z; 
            alpha += fabs(zval - scatPts[ob][j].z); 
          }
          printf(" Mean deviation = %.3f\n", alpha / numScat[ob]);

          rsMedian(bx, numScatFit[ob], by, &medianDev);
          rsMADN(bx, numScatFit[ob], medianDev, by, &MADN);
          /*printf("MADN %.3f\n", MADN); */
          if (!loop && dropOutliers) {
            for (j = numScat[ob] - 1; j >= 0; j--) {
              if (fabs(bx[j] - medianDev) / MADN > critMADN) {
                for (i = j + 1; i < numScatFit[ob]; i++)
                  scatPts[ob][i-1] = scatPts[ob][i];
                numScatFit[ob]--;
              }
            }
            printf("             Dropped %4d of %5d points as outliers:", 
                   numScat[ob] - numScatFit[ob], numScat[ob]);
          }
        }
        free(lmat);
        free(ipiv);
        free(work);
      }

      if (midfile)
        obj = &midmod->obj[indc+numObj];

      /* Get the Z value at the warp positions */
      zsum = 0.;
      for (j = 0; j < numYloc; j++) {
        yloc = ymin + j * ySpacing;
        if (midfile) {
          NEWCONTOUR(cont);
          if (numObj > 1) {
            NEWCONTOUR(cont1);
            NEWCONTOUR(cont2);
          }
        }
        for (i = 0; i < numXloc; i++) {
          xloc = xmin + i * xSpacing;
          ind = indWarp[i + j * numXloc];
          if (ind >= 0) {
            zval = 0.;
            for (ob = 0; ob < numObj; ob++) {
              zval2 = evaluateTPS(xloc, yloc, scatPts[ob], numScatFit[ob], 
                                  &scatScl[ob], scatVec[ob]);
              zval += zval2 / numObj;
              if (cont1 && !ob && !imodPointAppendXYZ(cont1, xloc, yloc,zval2))
                exitError(APPEND_ERROR);
              if (cont2 && ob && !imodPointAppendXYZ(cont2, xloc, yloc,zval2))
                exitError(APPEND_ERROR);
            }
            warps[ind].dz = zBinning * zval;
            zsum += warps[ind].dz;
            if (midfile && !imodPointAppendXYZ(cont, xloc, yloc, zval))
                exitError(APPEND_ERROR);
          }
        }

        /* Add contours to objects */
        if (midfile) {
          if (cont->psize && imodObjectAddContour(obj, cont) < 0)
            exitError(ADDCONT_ERROR);
          free(cont);
          if (numObj > 1) {
            if (cont1->psize && imodObjectAddContour
                (&midmod->obj[numObj+indc+numLambdas], cont1) < 0)
              exitError(ADDCONT_ERROR);
            free(cont1);
            if (cont2->psize && imodObjectAddContour
                (&midmod->obj[numObj+indc+2*numLambdas], cont2) < 0)
              exitError(ADDCONT_ERROR);
            free(cont2);
          }
        }
      }
      if (midfile) {
        for (ob = 0; ob < 1 + 2 * (numObj - 1); ob++) {
          obj = &midmod->obj[numObj+indc+ob*numLambdas];
          adjustAndMeshObj(obj, lambda[indc], &scale, showcont,namePrefix[ob]);
          if (!indc)
            obj->flags &= ~IMOD_OBJFLAG_OFF;
        }
      }
      for (ob = 0; ob < numObj; ob++) {
        free(scatPts[ob]);
        free(scatVec[ob]);
      }
    }

    /* Close up the output model file now and exit if multiple lambdas */
    finishOutputModel(midfile, midmod, model, &scale, flipped, numLambdas,
                      restoreOrientation);

  } else {

    /* THE MODEL HAS BOUNDARY CONTOURS */

    /* Flip model so that z is depth unless user directs otherwise */
    if ((flipyz < 0 && model->zmax > model->ymax) || flipyz > 0) {
      imodFlipYZ(model);
      flipped = 1;
    }

    ymin = 1.e20;
    ymax = -ymin;
    /* Make list of contours to be used */
    for (ob = 0; ob < model->objsize; ob++) {
      for (co = 0; co < model->obj[ob].contsize; co++) {
        cont = &model->obj[ob].cont[co];
        if (cont->psize < 2)
          continue;
        
        /* Make sure contour is planar and get x min and max */
        iyval = B3DNINT(cont->pts[0].y);
        planar = 1;
        xmin = cont->pts[0].x;
        xmax = xmin;
        for (pt = 1; pt < cont->psize; pt++) {
          if (B3DNINT(cont->pts[pt].y) !=iyval)
            planar = 0;
          xmin = B3DMIN(xmin, cont->pts[pt].x);
          xmax = B3DMAX(xmax, cont->pts[pt].x);
        }
        if (!planar) {
          printf ("\nWARNING: Obj %d, cont %d, at Y = %d, is not planar and is"
                  " being ignored\n\n", ob + 1, co + 1, iyval + 1);
          continue;
        }

        /* Sort in X and remove points at same X */
        imodel_contour_sortx(cont, 0, cont->psize - 1);
        i = 0;
        while (i < cont->psize - 1) {
          if (cont->pts[i+1].x - cont->pts[i].x < 1.e-3) {
            cont->pts[i].z = (cont->pts[i].z + cont->pts[i+1].z) / 2.;
            imodPointDelete(cont, i + 1);
          } else
            i++;
        }

        /* Check for other contour at this Y value */
        found = 0;
        for (i = 0; i < ilistSize(clist); i++) {
          cdptr = (ContData *)ilistItem(clist, i);
          if (cdptr->yval == iyval) {
            if (cdptr->cont2)
              exitError("There seem to be 3 contours at Y = %d", iyval + 1);
            if (oneSurface)
              exitError("You specified one surface and there seem to be 2 "
                        "contours at Y = %d", iyval + 1);
            if (xmin >= cdptr->xmax || xmax <= cdptr->xmin)
              exitError("Two contours at Y = %d do not overlap enough in X", 
                        iyval + 1);
            found = 1;
            cdptr->xmin = B3DMAX(xmin, cdptr->xmin);
            cdptr->xmax = B3DMIN(xmax, cdptr->xmax);
            cdptr->cont2 = cont;
            break;
          }
        }

        if (!found) {
          cdata.xmin = xmin;
          cdata.xmax = xmax;
          cdata.cont = cont;
          cdata.cont2 = NULL;
          cdata.yval =iyval;
          cdata.cdup = NULL;
          if (ilistAppend(clist, &cdata))
            exitError("Adding contour to list");
          ymin = B3DMIN(ymin, iyval);
        ymax = B3DMAX(ymax, iyval);
        }
      }
    }

    if (ilistSize(clist) < 2) 
      exitError("You must enter contours at more than one Y level");
    
    /* Sort contours in Y */
    for (i = 0; i < ilistSize(clist) - 1; i++) {
      cdptr = (ContData *)ilistItem(clist, i);
      for (j = i + 1; j < ilistSize(clist); j++) {
        cdptr2 = (ContData *)ilistItem(clist, j);
        if (cdptr->yval > cdptr2->yval) {
          cdata = *cdptr;
          *cdptr = *cdptr2;
          *cdptr2 = cdata;
        }
      }
    }
    
    /* Resample the contours at something close to the local Y spacing and
       reduce two contours to one */
    xmin = 1.e20;
    xmax = -xmin;
    numPoints = 0;
    for (i = 0; i < ilistSize(clist); i++) {
      cdptr = (ContData *)ilistItem(clist, i);
      xmin = B3DMIN(xmin, cdptr->xmin);
      xmax = B3DMAX(xmax, cdptr->xmax);
      
      /* Get Y spacing from 4 adjacent contours, set X sample & window size */
      iy = B3DMAX(i - 2, 0);
      j = B3DMIN(iy + 4, ilistSize(clist) - 1);
      iy = B3DMAX(j - 4, 0);
      cdptr2 = (ContData *)ilistItem(clist, j);
      localYspace = cdptr2->yval;
      cdptr2 = (ContData *)ilistItem(clist, iy);
      localYspace = (localYspace - cdptr2->yval) / (j - iy);
      numBound = (int)ceil((cdptr->xmax - cdptr->xmin) / 
                            (resampleXYfac * localYspace)) + 1;
      resampX = (cdptr->xmax - cdptr->xmin) / (numBound - 1);
      windowX = resampX * resampWindowFac;
      //printf("%d %d %d  %f  %f  %d\n", iy, j, cdptr2->yval, localYspace, resampX, numBound);
      
      /* Make a new contour */
      NEWCONTOUR(ncont);
      
      for (j = 0; j < numBound; j++) {
        ptadd.x = B3DMIN(cdptr->xmin + j * resampX, cdptr->xmax);
        ptadd.y = cdptr->yval;
        interpolateCont2(cdptr->cont, ptadd.x, windowX, &ptadd.z);
        if (cdptr->cont2) {
          interpolateCont2(cdptr->cont2, ptadd.x, windowX, &zval);
          ptadd.z = (ptadd.z + zval) / 2.;
        }
        if (!imodPointAppend(ncont, &ptadd))
          exitError("Adding point to average contour");
      }
      cdptr->cont = ncont;
      numPoints += numBound;
    }

    /* Output middle contour file if desired */
    if (midfile) {
      NEWOBJECT;
      obj = &midmod->obj[0];
      for (i = 0; i < ilistSize(clist); i++) {
        cdptr = (ContData *)ilistItem(clist, i);
        cdup = imodContourDup(cdptr->cont);
        if (!cdup)
          exitError("Duplicating contour");
        if (imodObjectAddContour(obj, cdup) < 0)
          exitError(ADDCONT_ERROR);
        free(cdup);
      }
      adjustAndMeshObj(obj, -999., &scale, showcont, "Original positions");
    }

    /* Now do smoothing if there are lambdas */
    if (numLambdas) {
      
      /* Get the arrays for this number of points */
      lwork = prepareTPS(numPoints, &fitPts, &yvec, &lmat, &ipiv, &work);
      
      /* Load the points from contours into fit point array */
      j = 0;
      for (i = 0; i < ilistSize(clist); i++) {
        cdptr = (ContData *)ilistItem(clist, i);
        for (pt = 0; pt < cdptr->cont->psize; pt++)
          fitPts[j++] = cdptr->cont->pts[pt];
      }
      setTPSscaling(fitPts, numPoints, &tpsScl);
      for (indc = 0; indc < numLambdas; indc++) {
        printf("Fitting thin plate spline, n = %d, log lambda = %.2f:", 
               numPoints, lambda[indc]);
        fitTPS(fitPts, numPoints, lambda[indc], &tpsScl, yvec, lmat, ipiv,
               work, lwork);
        if (midfile) {
          NEWOBJECT;
          obj = &midmod->obj[indc+1];
        }

        /* Make duplicate contours with predicted data */
        alpha = 0.;
        for (i = 0; i < ilistSize(clist); i++) {
          cdptr = (ContData *)ilistItem(clist, i);
          if (cdptr->cdup)
            free(cdptr->cdup);
          cdptr->cdup = imodContourDup(cdptr->cont);
          if (!cdptr->cdup)
            exitError("Duplicating contour");
          for (pt = 0; pt < cdptr->cont->psize; pt++) {
            ptp = &cdptr->cont->pts[pt];
            zval = evaluateTPS(ptp->x, ptp->y, fitPts, numPoints, &tpsScl,
                               yvec);
            cdptr->cdup->pts[pt].z = zval;
            alpha += fabs((double)(zval - ptp->z));
            /* Last time, replace Z value in original data too */
            if (indc == numLambdas - 1)
              ptp->z = zval;
          }
          
          /* Add contour to model if writing model */
          if (midfile && imodObjectAddContour(obj, cdptr->cdup) < 0)
            exitError(ADDCONT_ERROR);
        }
        printf(" Mean deviation = %.3f\n", alpha / numPoints);
        if (midfile) {
          adjustAndMeshObj(obj, lambda[indc], &scale, showcont, "");
          if (!indc)
            obj->flags &= ~IMOD_OBJFLAG_OFF;
          midmod->cindex.object = 1;
        }
      }
      free(work);
      free(lmat);
      free(ipiv);
    }

    /* Close up the output model file now and exit if multiple lambdas */
    finishOutputModel(midfile, midmod, model, &scale, flipped, numLambdas,
                      restoreOrientation);

    if (xSpacing && ySpacing) {
      if ((1. + (ymax - ymin) / ySpacing) * (1. + (xmax - xmin) / xSpacing) 
          >= maxLocations)
        exitError("Your spacings between grid points are too small for "
                  "warpvol");
    } else {
      
      /* Find minimum interval in Y */
      minint = 10000000;
      for (i = 0; i < ilistSize(clist) - 1; i++) {
        cdptr = (ContData *)ilistItem(clist, i);
        cdptr2 = (ContData *)ilistItem(clist, i + 1);
        iyval = cdptr2->yval - cdptr->yval;
        minint = B3DMIN(minint, iyval);
      }
      
      /* Start at "ideal" spacing factors and work down to factors of 1 */
      for (i = 20; i >= 0; i--) {
        xSpacing = minint / (1. + i * (xSpaceFac - 1.) / 20.);
        ySpacing = minint / (1. + i * (ySpaceFac - 1.) / 20.);
        if ((1. + (ymax - ymin) / ySpacing) * (1. + (xmax - xmin) / xSpacing)
            < maxLocations)
          break;
      }
      
      if (i < 0)
        exitError("The minimum spacing between contours (%d) in Y is too "
                  "small", minint);
      printf("Minimum spacing between contours is %d\nSetting target spacings"
             " in X and Y to %.1f and %1.f\n", minint, xSpacing, ySpacing);
    }
    
    setupWarpGrid(xmin, xmax, ymin,ymax, xyBinning, zBinning, model, &xSpacing,
                  &ySpacing, &numXloc, &numYloc, &xcen, &ycen, &zcen, &warps,
                  &indWarp);
    if (numXloc < 3 || numYloc < 3)
      exitError("The number of positions is too small; reduce spacing or "
                "increase range of contours");
    cdptr = (ContData *)ilistItem(clist, 0);
    cdptr2 = (ContData *)ilistItem(clist, 1);
    indc = 1;
    zsum = 0.;
    numLoc = 0;
    for (j = 0; j < numYloc; j++) {
      yloc = ymin + j * ySpacing;
      
      /* Advance to next contour pair if necessary */
      while (yloc > cdptr2->yval && indc < ilistSize(clist)) {
        cdptr = cdptr2;
        cdptr2 = (ContData *)ilistItem(clist, indc++);
      }
      
      /* Compute inverse transform, so keep sign of z */
      for (i = 0; i < numXloc; i++) {
        xloc = xmin + i * xSpacing;
        if (interpolateCont(cdptr->cont, xloc, &zval) >= 0 && 
            interpolateCont(cdptr2->cont, xloc, &zval2) >= 0) {
          warps[numLoc].xpos = xloc * xyBinning - xcen;
          warps[numLoc].ypos = yloc * xyBinning - ycen;
          
          /* Get the value from the TPS if it exists */
          if (numLambdas) {
            warps[numLoc].dz = zBinning * 
              evaluateTPS(xloc, yloc, fitPts, numPoints, &tpsScl, yvec);
          } else {
            frac = (yloc - cdptr->yval) / (cdptr2->yval - cdptr->yval);
            warps[numLoc].dz = zBinning * ((1. - frac) * zval + frac *zval2);
          }
          zsum += warps[numLoc].dz;
          warps[numLoc].aa = -999.;
          indWarp[i + j * numXloc] = numLoc++;
        } else
          indWarp[i + j * numXloc] = -1;
      }
    }  
  }

  /* DONE WITH BOTH KINDS OF MODELS - PROCESS WARP ARRAY FOR WARPING */

  zsum /= numLoc;
  printf("Mean Z height is %.1f\n", zsum / zBinning);
  zmid = (oneSurface || scattered) ? zsum : zcen;
  for (i = 0; i < numLoc; i++)
    warps[i].dz -= zmid;

  /* At each position, fit a plane to up to 9 points to get normal */
  for (j = 0; j < numYloc; j++) {
    for (i = 0; i < numXloc; i++) {
      ind = indWarp[i + j * numXloc];
      if (ind < 0)
        continue;
      ndat = 0;
      for (iy = j - 1; iy <= j + 1; iy++) {
        for (ix = i - 1; ix <= i + 1; ix++) {
          ind2 = INDWARP(ix, iy);
          if (ind2 >= 0) {
            xp[ndat] = warps[ind2].xpos;
            yp[ndat] = warps[ind2].ypos;
            zp[ndat++] = warps[ind2].dz;
          }
        }
      }
      /* Fit a plane to the data and save the normal vector */
      if (ndat >= 5) {
        lsFit2(xp, yp, zp, ndat, &aa, &bb, &cc);
        warps[ind].aa = -aa;
        warps[ind].bb = -bb;
      }
    }
  }

  /* Fill in normals from closest transformation */
  for (j = 0; j < numYloc; j++) {
    for (i = 0; i < numXloc; i++) {
      ind = indWarp[i + j * numXloc];
      if (ind < 0 || warps[ind].aa > -998.)
        continue;
      distmin = 1.e30;
      indmin = -1;
      delind = 2 + B3DMAX(ySpacing / xSpacing, xSpacing / ySpacing);
      while (indmin < 0) {
        for (iy = j - delind; iy <= j + delind; iy++) {
          for (ix = i - delind; ix <= i + delind; ix++) {
            ind2 = INDWARP(ix, iy);
            if (ind2 < 0 || warps[ind2].aa < -998.)
              continue;
            dx = warps[ind].xpos -  warps[ind2].xpos;
            dy = warps[ind].ypos -  warps[ind2].ypos;
            dist = dx * dx + dy * dy;
            if (dist < distmin) {
              distmin = dist;
              indmin = ind2;
            }
          }
        }
        delind *= 2;
      }
      warps[ind].aa = warps[indmin].aa;
      warps[ind].bb = warps[indmin].bb;
    }
  }
  
  /* Get array for lsqr and pointers to subarrays */
  maxRows = 3 * numLoc + 20;
  maxVals = maxRows * 4;
  iwrk = (int *)malloc((2 + maxRows + 2 * maxVals) * sizeof(int));
  uu1 = (double *)malloc(maxRows * sizeof(double));
  uu2 = (double *)malloc(maxRows * sizeof(double));
  vv = (double *)malloc(maxRows * sizeof(double));
  ww = (double *)malloc(maxRows * sizeof(double));
  xx = (double *)malloc((numLoc + 20) * sizeof(double));
  sumEntries = (float *)malloc((numLoc + 20) * sizeof(float));
  if (!iwrk || !uu1 || !uu2 || !vv || !ww || !xx || !sumEntries)
    exitError("Failed to get memory for arrays");
  iwrk[0] = maxRows + 2;
  iwrk[1] = 2 + maxRows + maxVals;
  ia = iwrk + 2;
  ja = iwrk + iwrk[0];
  rwrk = (float *)iwrk + iwrk[1];
  ia[0] = 1;

  /* Set up and solve equations for dx, dy */
  numRows = 0;
  dxScale = 1. / (xyBinning * xSpacing);
  dyScale = 1. / (xyBinning * ySpacing);
  dxyScale = 1. / (xyBinning * sqrt(xSpacing * ySpacing));
  numTied = 0;
  for (j = 0; j < numYloc; j++) {
    for (i = 0; i < numXloc; i++) {
      ind11 = indWarp[i + j * numXloc];
      if (ind11 < 0)
        continue;
      numInRow = 0;
      ind21 = INDWARP(i + 1, j);
      ind12 = INDWARP(i, j + 1);
      ind22 = INDWARP(i + 1, j + 1);
      err = 0;
      if (ind12 >= 0 && ind21 >= 0 && ind22 >= 0) {

        /* If this is the lower left corner of a rectangle, get the average
           normal and derive a stretch transformation from rotating normal
           to vertical */
        aa = (warps[ind11].aa + warps[ind21].aa + warps[ind12].aa + 
              warps[ind22].aa) / 4.;
        bb = (warps[ind11].bb + warps[ind21].bb + warps[ind12].bb + 
              warps[ind22].bb) / 4.;
        stretch = sqrt(1. + aa * aa + bb * bb);
        axis = 0.;
        if (aa || bb)
          axis = atan2(bb, aa);
        cosphi = cos(axis);
        sinphi = sin(axis);
        cosphisq = cosphi * cosphi;
        sinphisq = sinphi * sinphi;
        a11 = stretch * cosphisq + sinphisq;
        a12 = a21 = (stretch - 1.) * cosphi * sinphi;
        a22 = stretch * sinphisq + cosphisq;

        addValueToRow(-dxScale, ind11+1, valRow, icolRow, &numInRow);
        addValueToRow(dxScale, ind21+1, valRow, icolRow, &numInRow);
        uu1[numRows] = a11 - 1.;
        uu2[numRows] = a21;
        err += addRowToMatrix(valRow, icolRow, numInRow, rwrk, ia, ja,
                              &numRows, maxRows, maxVals);
        numInRow = 0;
        addValueToRow(-dyScale, ind11+1, valRow, icolRow, &numInRow);
        addValueToRow(dyScale, ind12+1, valRow, icolRow, &numInRow);
        uu1[numRows] = a12;
        uu2[numRows] = a22 - 1.;
        err += addRowToMatrix(valRow, icolRow, numInRow, rwrk, ia, ja,
                              &numRows, maxRows, maxVals);
        numInRow = 0;
        addValueToRow(-dxyScale, ind11+1, valRow, icolRow, &numInRow);
        addValueToRow(dxyScale, ind22+1, valRow, icolRow, &numInRow);
        uu1[numRows] = ((a11 - 1.) * xSpacing + a12 * ySpacing) * dxyScale *
          xyBinning;
        uu2[numRows] = ((a22 - 1.) * ySpacing + a21 * xSpacing) * dxyScale *
          xyBinning;
        err += addRowToMatrix(valRow, icolRow, numInRow, rwrk, ia, ja,
                              &numRows, maxRows, maxVals);

      } else {

        ind02 = INDWARP(i - 1, j + 1);
        ind01 = INDWARP(i - 1, j);
        ind00 = INDWARP(i - 1, j - 1);
        ind10 = INDWARP(i, j - 1);
        ind20 = INDWARP(i + 1, j - 1);

        if (!(ind01 >= 0 && ind02 >= 0 && ind12 >=0) && 
            !(ind01 >= 0 && ind00 >= 0 && ind10 >=0) &&
            !(ind10 >= 0 && ind20 >= 0 && ind21 >=0)) {

          /* If this is not part of any full rectangle then need to tie to
             adjacent point(s) */
          coef = 1. / ((ind01 >= 0 ? 1. : 0.) + (ind10 >= 0 ? 1. : 0.) + 
                       (ind21 >= 0 ? 1. : 0.) + (ind12 >= 0 ? 1. : 0.));
          addValueToRow(-1., ind11+1, valRow, icolRow, &numInRow);
          if (ind01 >= 0)
            addValueToRow(coef, ind01+1, valRow, icolRow, &numInRow);
          if (ind10 >= 0)
            addValueToRow(coef, ind10+1, valRow, icolRow, &numInRow);
          if (ind21 >= 0)
            addValueToRow(coef, ind21+1, valRow, icolRow, &numInRow);
          if (ind12 >= 0)
            addValueToRow(coef, ind12+1, valRow, icolRow, &numInRow);
          uu1[numRows] = 0.;
          uu2[numRows] = 0.;
          err += addRowToMatrix(valRow, icolRow, numInRow, rwrk, ia, ja,
                                &numRows, maxRows, maxVals);
          numTied++;
        }
      }
      if (err)
        exitError("Failed to make arrays big enough (numVals %d maxVals %d "
                  "numRows %d maxRows %d)", ia[numRows], maxVals, numRows, 
                  maxRows);
    }
  }  
  
  /* Set the center point zero */
  ind = indWarp[numXloc / 2 + (numYloc / 2) * numXloc];
  if (ind < 0) {
    for (delind = 1; delind <= B3DMAX(numXloc / 2, numYloc / 2) && ind < 0;
         delind++)
      for (j = numYloc / 2 - delind; j <=  numYloc / 2 + delind && ind < 0;
           j++)
        for (i = numXloc / 2 - delind; i <=  numXloc / 2 + delind && ind < 0;
             i++)
          ind = INDWARP(i, j);
  }
            
  numInRow = 0;
  addValueToRow(1., ind + 1, valRow, icolRow, &numInRow);
  uu1[numRows] = 0;
  uu2[numRows] = 0;
  if (addRowToMatrix(valRow, icolRow, numInRow, rwrk, ia, ja, &numRows, 
                     maxRows, maxVals))
    exitError("Failed to make arrays big enough");

  printf("%d rows of data, %d variables (%d tied to neighbors)\n", numRows, 
         numLoc, numTied);
  
  /* Normalize columns, then solve for dx and dy */
  normalizeColumns(rwrk, ia, ja, numLoc, numRows, sumEntries);
  itnlim = numLoc;
  atol = 0.;
  btol = 0.;
  conlim = 1.e7;
  lsqr(numRows, numLoc, sparseProd, 0., iwrk, uu1, vv, ww, xx, NULL, atol,
       btol, conlim, itnlim, NULL, &istop, &itndone, &anorm, 
       &acond, &rnorm, &arnorm, &xnorm);
  reportLsqr("dx", istop, itndone, acond);

  /* Store the negative for an inverse transform */
  for (i = 0; i < numLoc; i++)
    warps[i].dx = -xx[i] / sumEntries[i];
  lsqr(numRows, numLoc, sparseProd, 0., iwrk, uu2, vv, ww, xx, NULL, atol,
       btol, conlim, itnlim, NULL, &istop, &itndone, &anorm, 
       &acond, &rnorm, &arnorm, &xnorm);
  reportLsqr("dy", istop, itndone, acond);
  for (i = 0; i < numLoc; i++)
    warps[i].dy = -xx[i] / sumEntries[i];

  imodBackupFile(filename);
  fp = fopen(filename, "w");
  if (!fp)
    exitError("Opening output file %s", filename);
  fprintf(fp, "%d %d 1 %.2f %.2f 0. %.4f %.4f 1.\n", numXloc, numYloc,
          xmin * xyBinning - xcen, ymin * xyBinning - ycen, 
          xSpacing * xyBinning, ySpacing * xyBinning);
  for (i = 0; i < numLoc; i++) {

    /* Get the matrix that rotates the normal to vertical */
    angle = 180. * acos(1. / sqrt(1. + warps[i].aa * warps[i].aa + 
                                  warps[i].bb * warps[i].bb)) / 3.14159;
    axisvec.z = 0.;
    axisvec.x = warps[i].bb;
    axisvec.y = -warps[i].aa;
    imodMatId(mat);
    imodMatRotateVector(mat, -angle, &axisvec);

    /* Then apply this inverse transform to the center position and get
       the actual shift needed to get to the displaced point */
    axisvec.x = warps[i].xpos;
    axisvec.y = warps[i].ypos;
    axisvec.z = 0.;
    imodMatTransform3D(mat, &axisvec, &ptadd);

    fprintf(fp, "%.2f %.2f 0.\n%.5f %.5f %.5f %.2f\n%.5f %.5f %.5f %.2f\n"
            "%.5f %.5f %.5f %.2f\n", warps[i].xpos, warps[i].ypos, 
            mdata[0], mdata[4], mdata[8], warps[i].xpos + warps[i].dx -ptadd.x,
            mdata[1], mdata[5], mdata[9], warps[i].ypos + warps[i].dy -ptadd.y,
            mdata[2], mdata[6], mdata[10], warps[i].dz - ptadd.z);
  }

  fclose(fp);
  printf("%d warping transformations written\n", numLoc);

  if (patchfile) {
    imodBackupFile(patchfile);
    fp = fopen(patchfile, "w");
    if (!fp)
      exitError("Opening patch output file %s", patchfile);
    fprintf(fp, "%d positions\n", numLoc);
    for (i = 0; i < numLoc; i++)
      fprintf(fp, "%d %d %d %.2f %.2f %.2f\n", B3DNINT(warps[i].xpos + xcen),
              B3DNINT(warps[i].ypos + ycen), B3DNINT(zcen),
              warps[i].dx, warps[i].dy, warps[i].dz);
    fclose(fp);
  }
  exit(0);
}

/* The original contour interpolation routine interpolates between the 
   appropriate pair of points or returns -1 for X out of range.  It is still
   used to get a zval from the smoothed contour data */
static int interpolateCont(Icont *cont, float xval, float *zval)
{
  int pt;
  float frac;
  if (xval < cont->pts[0].x || xval > cont->pts[cont->psize - 1].x)
    return -1;
  for (pt = 1; pt < cont->psize - 1; pt++) {
    if (cont->pts[pt].x > xval)
      break;
  }
  frac = (xval - cont->pts[pt-1].x) / (cont->pts[pt].x - cont->pts[pt-1].x);
  *zval = frac * cont->pts[pt].z + (1. - frac) * cont->pts[pt-1].z;
  return pt - 1;
}

/* New contour interpolation method simply interpolates if there are only a
   few values within the window around x, or finds a smoothed value by
   fitting a quadratic to nearby points */
static int interpolateCont2(Icont *cont, float xval, float window, float *zval)
{
  int i, pt, numPts, firstPt, ptAfter;
  static int maxPts = 0;
  static float *xx, *yy, *zz;
  float frac, aa, bb;

  if (xval < cont->pts[0].x || xval > cont->pts[cont->psize - 1].x)
    return -1;
  numPts = 0;
  ptAfter = -1;
  
  /* Count points in the window around xval, keep track of first one after */
  for (pt = 0; pt < cont->psize; pt++) {
    if (ptAfter < 0 && cont->pts[pt].x > xval)
      ptAfter = pt;
    if (cont->pts[pt].x > xval + window / 2.)
      break;
    if (cont->pts[pt].x >= xval - window / 2.)
      numPts++;
  }

  /* If there are fewer than 4 points in the window, just interpolate */
  if (numPts <= 3) {
    if (ptAfter < 0)
      ptAfter = cont->psize - 1;
    pt = ptAfter;
    frac = (xval - cont->pts[pt-1].x) / (cont->pts[pt].x - cont->pts[pt-1].x);
    *zval = frac * cont->pts[pt].z + (1. - frac) * cont->pts[pt-1].z;
    return pt - 1;
  }
    
  /* Allocate arrays if needed */
  firstPt = pt - numPts;
  if (numPts > maxPts) {
    if (maxPts) {
      free(xx);
      free(yy);
      free(zz);
    }
    maxPts = numPts;
    xx = (float *)malloc(maxPts * sizeof(float));
    yy = (float *)malloc(maxPts * sizeof(float));
    zz = (float *)malloc(maxPts * sizeof(float));
    if (!xx || !yy || !zz)
      exitError("Allocating memory for line fits");
  }

  /* Load the arrays */
  for (i = 0; i < numPts; i++) {
    pt = i + firstPt;
    xx[i] = cont->pts[pt].x - xval;
    yy[i] = xx[i] * xx[i];
    zz[i] = cont->pts[pt].z;
  }

  /* Quadratic fit with xval at origin gives z as the constant term */
  lsFit2(xx, yy, zz, numPts, &aa, &bb, zval);
  return ptAfter - 1;
}

/* Return the index in the warp array of the given position, or -1 if the 
   position is outside the range */
static int indInArray(int *indWarp, int numXloc, int numYloc, int i, int j)
{
  if (i < 0 || i >= numXloc || j < 0 || j >= numYloc)
    return -1;
  return indWarp[i + j * numXloc];
}

static void reportLsqr(char *dd, int istop, int itndone, double acond)
{
  printf("Solution for %s: condition # %.4f, %d iterations\n", dd, acond,
         itndone);
  if (istop == 4)
    printf("The system appears to be ill conditioned\n");
  if (istop == 5)
    printf("The iteration limit was reached\n");
}

/* Get number of positions in warp grid, refine spacing, allocate array */
static void setupWarpGrid
(float xmin, float xmax, float ymin, float ymax, int xyBinning, int zBinning,
 Imod *model, float *xSpacing, float *ySpacing, int *numXloc, int *numYloc,
 float *xcen, float *ycen, float *zcen, WarpData **warps, int **indWarp)
{
  *numXloc = (int)(1. + (xmax - xmin) / *xSpacing);
  *numYloc = (int)(1. + (ymax - ymin) / *ySpacing);
  *xSpacing = (xmax - xmin) / (*numXloc - 1);
  *ySpacing = (ymax - ymin) / (*numYloc - 1);

  *xcen = xyBinning * model->xmax / 2.;
  *ycen = xyBinning * model->ymax / 2.;
  *zcen = zBinning * model->zmax / 2.;

  *warps = (WarpData *)malloc(*numXloc * *numYloc * sizeof(WarpData));
  *indWarp = (int *)malloc(*numXloc * *numYloc * sizeof(int));
  if (!*warps || !*indWarp)
    exitError("Getting memory for warping data");
}

/* Allocate arrays for thin plate spline solution and return size of work */
static int prepareTPS(int numPoints, Ipoint **fitPts, double **yvec, 
                      double **lmat, int **ipiv, double **work)
{
  char uplo = 'U';
  double query;
  int info, one = 1, lwork = -1;
  int pp3 = numPoints + 3;
  *fitPts = (Ipoint *)malloc(numPoints * sizeof(Ipoint));
  *lmat = (double *)malloc(pp3 * pp3 * sizeof(double));
  *yvec = (double *)malloc(pp3 * sizeof(double));
  *ipiv = (int *)malloc(pp3 * sizeof(int));
  dsysv(&uplo, &pp3, &one, *lmat, &pp3, *ipiv, *yvec, &pp3, &query, &lwork, 
        &info, 1);
  if (info)
      exitError("Error %d from dsysv workspace query", info);
  lwork = B3DNINT(query);
  *work = (double *)malloc(lwork * sizeof(double));
  
  if (!*fitPts || !*lmat || !*yvec || !*ipiv || !*work)
    exitError("Allocating memory for smoothing matrix");
  return lwork;
}

/* Solve for the thin plate spline */
static void fitTPS(Ipoint *fitPts, int numPoints, float lambda, Ipoint *tpsScl,
                   double *yvec, double *lmat, int *ipiv, double *work, 
                   int lwork)
{
  int row, i, info, col, one = 1, pp3 = numPoints + 3;
  Ipoint *ptp, *pt2p;
  double rr, uval, dx, dy, alpha = 0.;
  char uplo = 'U';
  lambda = (float)pow(10., (double)lambda);
  for (row = 0; row < numPoints; row++) {
    ptp = &fitPts[row];

    /* Load the row at the far end of the matrix */
    lmat[row * pp3 + pp3 - 3] = 1.;
    lmat[row * pp3 + pp3 - 2] = ptp->x * tpsScl->x;
    lmat[row * pp3 + pp3 - 1] = ptp->y * tpsScl->y;
    lmat[pp3 * (pp3-3) + row] = 1.;
    lmat[pp3 * (pp3-2) + row] = ptp->x * tpsScl->x;
    lmat[pp3 * (pp3-1) + row] = ptp->y * tpsScl->y;
    yvec[row] = ptp->z * tpsScl->z;

    for (col = row + 1; col < numPoints; col++) {
      
      /* Get r12, add to alpha, get U and put in matrix */
      pt2p = &fitPts[col];
      dx = (ptp->x - pt2p->x) * tpsScl->x;
      dy = (ptp->y - pt2p->y) * tpsScl->y;
      rr = sqrt(dx * dx + dy * dy);
      alpha += rr;
      uval = 0.;
      if (rr > 0.) 
        uval = rr * rr * log(rr);
      lmat[row * pp3 + col] = uval;
      lmat[col * pp3 + row] = uval;
    }
  }
  
  /* Fill zeros and diagonal */
  for (row = pp3 - 3; row < pp3; row++) {
    yvec[row] = 0.;
    for (col = pp3 - 3; col < pp3; col++)
      lmat[row * pp3 + col] = 0.;
  }
  alpha *= 2. / (numPoints * numPoints);
      
  for (i = 0; i < numPoints; i++)
    lmat[i * pp3 + i] = lambda * alpha * alpha / numPoints;

  /* for (i = 0; i < pp3; i++) {
     for (j = 0; j < pp3; j++) {
     printf("%13.5g ", lmat[i*pp3 + j]);
     if ((j%5) == 4)
     printf("\n");
     }
     printf("\n");
     } */
      
  /* Solve it */
  dsysv(&uplo, &pp3, &one, lmat, &pp3, ipiv, yvec, &pp3, work, &lwork, 
        &info, 1);
  if (info)
    exitError("Error %d from dsysv", info);
}

/* Evaluate the spline at the given position */
static float evaluateTPS(float x, float y, Ipoint *fitPts, int numPoints,
                         Ipoint *tpsScl, double *yvec)
{
  int row;
  double spsum, rr, uval, dx, dy;
  Ipoint *ptp;
  spsum = yvec[numPoints] + yvec[numPoints+1] * x * tpsScl->x 
    + yvec[numPoints+2] * y * tpsScl->y;
  for (row = 0; row < numPoints; row++) {
    ptp = &fitPts[row];
    dx = (x - ptp->x) * tpsScl->x;
    dy = (y - ptp->y) * tpsScl->y;
    rr = sqrt(dx * dx + dy * dy);
    uval = 0.;
    if (rr > 0.) 
      uval = rr * rr * log(rr);
    spsum += yvec[row] * uval;
  }
  return ((float)spsum / tpsScl->z);
}

/* Find the bounds of the points and set the scaling factors to scale data to
   a range of 1 on all axes */
static void setTPSscaling(Ipoint *fitPts, int numPoints, Ipoint *tpsScl)
{
  Icont *cont;
  Ipoint minpt, maxpt;
  NEWCONTOUR(cont);
  cont->psize = numPoints;
  cont->pts = fitPts;
  imodContourGetBBox(cont, &minpt, &maxpt);
  tpsScl->x = 1. / (maxpt.x - minpt.x);
  tpsScl->y = 1. / (maxpt.y - minpt.y);
  tpsScl->z = 1. / (maxpt.z - minpt.z);
  free(cont);
}

/* Rotate a model and associated meshes by -90 or 90 depending on the sign of 
   dir */
static void  rotateModel(Imod *imod, int dir)
{
  Iobj  *obj;
  Icont *cont;
  Imesh *mesh;
  float tmp;
  int    ob, co, pt;

  for(ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
        tmp = cont->pts[pt].y;
        if (dir < 0) {
          cont->pts[pt].y = cont->pts[pt].z;
          cont->pts[pt].z = imod->ymax - tmp - 1.;
        } else {
          cont->pts[pt].y = imod->zmax - cont->pts[pt].z - 1.;
          cont->pts[pt].z = tmp;
        }
      }
    }
    for(co = 0; co < obj->meshsize; co++){
      mesh = &(obj->mesh[co]);
      for(pt = 0; pt < mesh->vsize; pt++){
        tmp = mesh->vert[pt].y;
        if (dir < 0) {
          mesh->vert[pt].y = mesh->vert[pt].z;
          mesh->vert[pt].z = imod->ymax - tmp - 1.;
        } else {
          mesh->vert[pt].y = imod->zmax - mesh->vert[pt].z - 1.;
          mesh->vert[pt].z = tmp;
        }
      }
    }
  }

  tmp = imod->ymax;
  imod->ymax = imod->zmax;
  imod->zmax = tmp;
}

/* Set object properties and mesh the contours */
static void adjustAndMeshObj(Iobj *obj, float lambda, Ipoint *scale,
                             int showcont, char *prefix)
{
  if (lambda > -998.)
    sprintf(obj->name, "%sLog lambda %.2f", prefix, lambda);
  else
    sprintf(obj->name, "%s", prefix);

  imodObjectSetValue(obj, IobjFlagClosed, 0);
  obj->flags |= IMOD_OBJFLAG_TWO_SIDE;
  obj->meshParam = imeshParamsNew();
  if (obj->meshParam) {
    obj->meshParam->flags |= IMESH_MK_SKIP;
    analyzePrepSkinObj(obj, 0, scale, NULL);
    if (lambda > -999. && !showcont) 
      obj->flags |= IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |
        IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_OFF;

    /* Tone down the lighting to allow bumps to show up better */
    obj->ambient = 116;
    obj->diffuse = 130;
    obj->specular = 68;
  }
}

/* Set properties of output model and save it */
static void finishOutputModel(char *midfile, Imod *midmod, Imod *model, 
                              Ipoint *scale, int flipped, int numLambdas,
                              int restore)
{
  FILE *fout;
  if (!midfile)
    return;
  midmod->xmax = model->xmax;
  midmod->ymax = model->ymax;
  midmod->zmax = model->zmax;
  midmod->flags = model->flags;
  midmod->refImage = model->refImage;
  midmod->zscale = scale->z;
  imodBackupFile(midfile);

  /* Restore to flip/rotation state of input model if requested */
  if (restore) {
    if (flipped == 1)
      imodFlipYZ(midmod);
    else if (flipped == 2)
      rotateModel(midmod, 1);
    if (iobjScat(model->obj->flags) && (model->flags & IMODF_FLIPYZ))
      imodFlipYZ(midmod);
  } else {

    /* Otherwise just toggle the flipped flag if appropriate */
    if (flipped == 1 || (flipped != 1 && iobjScat(model->obj->flags) &&
                         (model->flags & IMODF_FLIPYZ))) {
      if (model->flags & IMODF_FLIPYZ)
        model->flags &= ~IMODF_FLIPYZ;
      else 
        model->flags |= IMODF_FLIPYZ;
    }
  }
  
  fout = fopen(midfile, "wb");
  if (!fout)
    exitError("Error opening file for middle contour model: %s", midmod);
  if (imodWrite(midmod, fout))
    exitError("Error writing middle contour model");
  fclose(fout);
  if (numLambdas > 1)
    exit(0);
}

/*
 *  flattenwarp.c - Computes warping transforms to flatten a volume
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
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
#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"
#include "lsqr.h"
#include "sparselsqr.h"

typedef struct {
  Icont *cont;
  int yval;
  int averaged;
  float xmin;
  float xmax;
} ContData;

typedef struct {
  float xpos;
  float ypos;
  float aa;
  float bb;
  float dx;
  float dy;
  float dz;
} WarpData;

static int interpolateCont(Icont *cont, float xval, float *zval);
static int indInArray(int *indWarp, int numXloc, int numYloc, int i, int j);
static void reportLsqr(char *dd, int istop, int itndone, double anorm);

#define INDWARP(a,b) indInArray(indWarp, numXloc, numYloc, (a), (b))

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  Imod *model;
  Icont *cont, *ncont;
  char *progname = imodProgName(argv[0]);
  char *filename;
  int numOptArgs, numNonOptArgs;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 flattenwarp  */
  int numOptions = 7;
  char *options[] = {
    "input:InputFile:FN:", "output:OutputFile:FN:", 
    "patch:PatchOutputFile:FN:", "binning:BinningOfTomogram:I:", 
    "one:OneSurface:B:", "flipyz:FlipYandZ:I:", 
    "spacing:WarpSpacingXandY:FP:"};

  /* Maximum locations to output.  It could be 200000, but limit it to keep
     linear equation system from getting truly enormous */ 
  int maxLocations = 50000;
  float xSpacing = 0, ySpacing = 0;
  float xSpaceFac = 4.5f, ySpaceFac = 3;
  int binning = 1;
  int oneSurface = 0;
  int flipyz = -1;
  int PID = 0;
  Ilist *clist = ilistNew(sizeof(ContData), 1000);
  char *strtmp;
  char *patchfile = NULL;
  ContData cdata;
  ContData *cdptr, *cdptr2;
  WarpData *warps;
  FILE *fp;
  Ipoint ptadd, axisvec;
  Imat *mat = imodMatNew(3);
  float *mdata = mat->data;
  float xp[9], yp[9], zp[9], valRow[5];
  int icolRow[5];
  int *indWarp;
  int *iwrk, *ia, *ja;
  float *rwrk, *sumEntries;
  double *xx, *uu1, *uu2, *ww, *vv;
  float xmin, xmax, ymin, ymax, zval, zval2, frac, xcen, ycen, zcen,xloc, yloc;
  int iyval, i, j, planar, found, co, ob, pt, ptl, ptm, minint, numXloc;
  int numYloc, numLoc, indc, ind00, ind01, ind02, ind10, ind11, ind12;
  int ind20, ind21, ind22, ndat, ix, iy, delind,ind2, indmin, ind, numTied;
  int maxRows, maxVals, numRows, numInRow, err, itnlim, itndone, istop;
  float dx, dy, dist, distmin, dxScale, dyScale, dxyScale, a11, a12, a21, a22;
  double zsum, axis, stretch, cosphi, sinphi, cosphisq, sinphisq, angle;
  float coef, aa, bb, cc, zmid;
  double atol, btol, conlim, anorm, acond, rnorm, arnorm, xnorm;

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

  if (PipGetInOutFile("OutputFile", 1, &filename))
    exitError("No output file specified");

  /* Process options */
  PipGetTwoFloats("WarpSpacingXandY", &xSpacing, &ySpacing);
  PipGetInteger("BinningOfTomogram", &binning);
  PipGetBoolean("OneSurface", &oneSurface);
  PipGetInteger("FlipYandZ", &flipyz);
  PipGetString("PatchOutputFile", &patchfile);
  PipGetBoolean("PID", &PID);
  if (PID) {
    fprintf(stderr, "Shell PID: %d\n", getpid());
    fflush(stderr);
  }

  /* Flip model so that z is depth unless user directs otherwise */
  if ((flipyz < 0 && model->zmax > model->ymax) || flipyz > 0)
    imodFlipYZ(model);

  if (!clist)
    exitError("Getting memory for list of contours");

  /* Make list of contours to be used */
  ymin = 1.e20;
  ymax = -ymin;
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
      imodel_contour_sortx(cont, 0, cont->psize - 1);

      /* Check for other contour at this Y value */
      found = 0;
      for (i = 0; i < ilistSize(clist); i++) {
        cdptr = (ContData *)ilistItem(clist, i);
        if (cdptr->yval == iyval) {
          if (cdptr->averaged)
            exitError("There seem to be 3 contours at Y = %d", iyval + 1);
          if (oneSurface)
            exitError("You specified one surface and there seem to be 2 "
                      "contours at Y = %d", iyval + 1);

          /* Make a new contour with points in the middle */
          ncont = imodContourNew();
          if (!ncont)
            exitError("Getting memory for contour");
          if (xmin >= cdptr->xmax || xmax <= cdptr->xmin)
            exitError("Two contours at Y = %d do not overlap enough in X", 
                      iyval + 1);

          /* Get starting point in contour starting later */
          if (xmin < cdptr->xmin) {
            ptl = 0;
            ptm = interpolateCont(cont, cdptr->xmin, &zval) + 1;
          } else {
            ptm = 0;
            ptl = interpolateCont(cdptr->cont, xmin, &zval) + 1;
          }

          /* Add points until one of them runs out */
          ptadd.y = iyval;
          do {
            if (cont->pts[ptm].x < cdptr->cont->pts[ptl].x) {
              interpolateCont(cdptr->cont, cont->pts[ptm].x, &zval);
              ptadd.x = cont->pts[ptm].x;
              ptadd.z = 0.5f * (zval + cont->pts[ptm++].z);
            } else {
              interpolateCont(cont, cdptr->cont->pts[ptl].x, &zval);
              ptadd.x = cdptr->cont->pts[ptl].x;
              ptadd.z = 0.5f * (zval + cdptr->cont->pts[ptl++].z);
            } 
            if (!imodPointAppend(ncont, &ptadd))
              exitError("Adding point to average contour");
          } while (ptl < cdptr->cont->psize && ptm < cont->psize);

          found = 1;
          cdptr->xmin = B3DMAX(xmin, cdptr->xmin);
          cdptr->xmax = B3DMIN(xmax, cdptr->xmax);
          cdptr->cont = ncont;
          cdptr->averaged = 1;
          break;
        }
      }

      if (!found) {
        cdata.xmin = xmin;
        cdata.xmax = xmax;
        cdata.cont = cont;
        cdata.averaged = 0;
        cdata.yval =iyval;
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

  xmin = 1.e20;
  xmax = -xmin;
  for (i = 0; i < ilistSize(clist) - 1; i++) {
    cdptr = (ContData *)ilistItem(clist, i);
    xmin = B3DMIN(xmin, cdptr->xmin);
    xmax = B3DMAX(xmax, cdptr->xmax);
  }

  if (xSpacing && ySpacing) {
    if ((1. + (ymax - ymin) / ySpacing) * (1. + (xmax - xmin) / xSpacing) 
        >= maxLocations)
      exitError("Your spacings between grid points are too small for warpvol");
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
      if ((1. + (ymax - ymin) / ySpacing) * (1. + (xmax - xmin) / xSpacing) < 
          maxLocations)
        break;
    }
    
    if (i < 0)
      exitError("The minimum spacing between contours (%d) in Y is too small",
                minint);
    printf("Minimum spacing between contours is %d\nSetting target spacings"
           " in X and Y to %.1f and %1.f\n", minint, xSpacing, ySpacing);
  }
    
  numXloc = (int)(1. + (xmax - xmin) / xSpacing);
  numYloc = (int)(1. + (ymax - ymin) / ySpacing);
  if (numXloc < 3 || numYloc < 3)
    exitError("The number of positions is too small; reduce spacing or "
              "increase range of contours");
  xSpacing = (xmax - xmin) / (numXloc - 1);
  ySpacing = (ymax - ymin) / (numYloc - 1);

  xcen = binning * model->xmax / 2.;
  ycen = binning * model->ymax / 2.;
  zcen = binning * model->zmax / 2.;

  warps = (WarpData *)malloc(numXloc * numYloc * sizeof(WarpData));
  indWarp = (int *)malloc(numXloc * numYloc * sizeof(int));
  if (!warps || !indWarp)
    exitError("Getting memory for warping data");
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
        warps[numLoc].xpos = xloc * binning - xcen;
        warps[numLoc].ypos = yloc * binning - ycen;
        frac = (yloc - cdptr->yval) / (cdptr2->yval - cdptr->yval);
        warps[numLoc].dz = binning * ((1. - frac) * zval + frac *zval2);
        zsum += warps[numLoc].dz;
        warps[numLoc].aa = -999.;
        indWarp[i + j * numXloc] = numLoc++;
      } else
        indWarp[i + j * numXloc] = -1;
    }
  }  

  zsum /= numLoc;
  printf("Mean Z height is %.1f\n", zsum / binning);
  zmid = oneSurface ? zsum : zcen;
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
  dxScale = 1. / (binning * xSpacing);
  dyScale = 1. / (binning * ySpacing);
  dxyScale = 1. / (binning * sqrt(xSpacing * ySpacing));
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
           to vetical */
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
          binning;
        uu2[numRows] = ((a22 - 1.) * ySpacing + a21 * xSpacing) * dxyScale *
          binning;
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
  btol = 0.01;
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
          xmin * binning - xcen, ymin * binning - ycen, xSpacing * binning,
          ySpacing * binning);
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


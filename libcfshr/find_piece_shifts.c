/*  find_piece_shifts.c : finds piece shifts by iteration
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2010 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end of file
 */
#include <math.h>
#include <string.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define findpieceshifts FINDPIECESHIFTS
#else
#define findpieceshifts findpieceshifts_
#endif

static void findLowestThree(float val, int ind, float *lowest, float *second, 
                            float *third);

/*!
 * Solves for positions of overlapping pieces by adjusting them iteratively to
 * minimize the difference between measured displacement between pairs of
 * pieces and the difference in their positions. ^
 * [ivarpc]: index from included variables (pieces) to overall piece number ^
 * [nvar]: Number of variables ^
 * [indvar]: index from piece number to variable number; must be set to less
 * than the minimum variable number (0 for C, 1 for Fortran) for all pieces
 * potentially connected to the ones included as variables. ^
 * [ixpclist, iypclist]: X and Y piece coordinates. ^
 * [dxedge, dyedge]: the measured difference from the nominal displacement 
 * between pieces at an edge ^
 * [idir]: 1 if those values are the amount that the upper piece is displaced
 * from being in register with the lower; -1 if they are the amount that the
 * upper piece needs to be shifted to be in register. ^
 * [pieceLower, pieceUpper]: index of pieces below and above an edge ^
 * [ifskipEdge]: A number indicating whether the edge should be skipped ^
 * [edgeStep]: Stride between X and Y edges at the same edge index in the edge
 * arrays [dxedge, dyedge, pieceLower, pieceUpper, ifskipEdge].  If the arrays
 * are accessed in Fortran as array(edge,ixy), then this value is the first 
 * dimension of the array.  If they are accessed in C as array[edge][ixy] or
 * array[2*edge+ixy] then the value is 1. ^
 * [dxyvar]: array into which the piece shifts in X and Y are returned for
 * each piece in the variable list^
 * [varStep]: Stride between X and Y shifts for a given piece in [dxyvar]; as
 * for [edgeStep], this is either 1 or the long dimension of the array. ^
 * [edgeLower, edgeUpper]: edge index of edge below or edge above a given piece
 * in X or Y. ^
 * [pcStep]: Stride between X and Y edges at the same piece in those arrays;
 * again, this is either 1 or the long dimension of the array ^
 * [work]: Array for temporary storage, must be at least 20.25 times the number
 * of variables plus 1. ^
 * [fort]: A nonzero value indicates that the indexes in 
 * [ivarpc, indvar, pieceLower, pieceUpper, edgeLower, edgeUpper] are numbered
 * from 1 instead 0. ^
 * [leaveInd]: Absolute edge index in edge arrays of edge to leave out ^
 * [skipCrit]: An edge is skipped if [ifskipEdge] is >= this value ^
 * [critMaxMove]: Terminate if the maximum move of any piece in X or Y is less
 * than this criterion. ^
 * [robustCrit]: Use robust fitting to eliminate or down-weight outlying 
 * edge displacements; multiply outlier criterion by given factor. ^
 * [critMoveDiff]: Terminate if the average move changes by less than this
 * amount between samples ^
 * [maxIter]: Maximum number of iterations ^
 * [numAvgForTest]: Number of iterations over which to average the average  ^
 * moves in X and Y for the test based on [critMoveDiff]  ^
 * [intervalForTest]: Number of iterations between such tests  ^
 * [numIter] is returned with the number of iteratation. ^
 * [wErrMean] and [wErrMax] are returned with the mean and maximum of weighted
 * errors. ^
 * Returns 1 if the strides are not all 1 or all different from 1. ^
 * Called from Fortran with identical arguments and name.
 */
int findPieceShifts
(int *ivarpc, int nvar, int *indvar, int *ixpclist, int *iypclist, 
 float *dxedge, float *dyedge, int idir, int *pieceLower, int *pieceUpper, 
 int *ifskipEdge, int edgeStep, float *dxyvar, int varStep, int *edgeLower,
 int *edgeUpper, int pcStep, int *work, int fort, int leaveInd, int skipCrit,
 float robustCrit, float critMaxMove, float critMoveDiff, int maxIter,
 int numAvgForTest, int intervalForTest, int *numIter, float *wErrMean, 
 float *wErrMax)
{
  int minxpc, minypc, maxxpc, maxypc, imin, numOnList, listInd, iedge, ipc;
  int numNeigh,isign,iter,nay,list, nsum, i, ivar, xyStep, ind, ixy, j;
  int lowup, neighpc,neighvar, numMedian, didWeights, computeWeights;
  int weightInterval, numEdge[2], debug;
  int edgeMedCrit = 6;
  float edgeTrMeanX[2], edgeTrMeanY[2], edgeDevMed[2], edgeDevMADN[2];
  float distmin,dist,xmovemax,ymovemax, xsum, ysum, dx, dy, ex, ey;
  float erx[4], ery[4], errd[4], xlow, xsec, xthr, ylow, ysec, ythr;
  float elow, esec, ethr, uu, wsum, wgt, MAD;
  float Ktune = 2.f * 4.685f * 0.6745f * robustCrit;
  float medThresh = 2.f * robustCrit;
  double sumxmove,sumymove,dxsum,dysum,errsum, errmax, medSum;
  float xmoveAvg, ymoveAvg, xmoveLast, ymoveLast;
  int bigint = 100000000;

  /* Set pointers to temporary arrays from the work array */
  int *ivarToList = work;
  int *listToVar = ivarToList + nvar;
  int *neighInd = listToVar + nvar;
  int *neighList = neighInd + nvar + 1;
  float *dxyEdge = (float *)(neighList + 4 * nvar);
  float *neighWgt = (float *)(dxyEdge + 8 * nvar);
  unsigned char *edgeDir = (unsigned char *)(neighWgt + 4 * nvar);
  unsigned char *placed = edgeDir + 4 * nvar;

  /* Set the xy stride parameter */
  if (edgeStep == 1 && pcStep == 1 && varStep == 1)
    xyStep = 2;
  else if (edgeStep > 1 && pcStep > 1 && varStep > 1)
    xyStep = 1;
  else
    return 1;
  if (fort)
    fort = 1;

  /* If doing robust, get median edge displacement and median deviation */
  if (robustCrit > 0.) {
    for (ixy = 0; ixy < 2; ixy++) {
      nsum = 0;

      /* Find the X or Y edges and save them in array */
      for (ivar = 0; ivar < nvar; ivar++) {
        ipc = ivarpc[ivar] - fort;
        iedge = edgeUpper[xyStep*ipc + pcStep*ixy] - fort;
        ind = xyStep*iedge + edgeStep*ixy;
        if (iedge >= 0 && ifskipEdge[ind] < skipCrit && 
            ind != leaveInd - fort) {
          dxyEdge[nsum] = -idir * dxedge[ind];
          dxyEdge[nsum++ + nvar] = -idir * dyedge[ind];
        }
      }
      numEdge[ixy] = nsum;

      /* If there are enough, get a trimmed mean vector, the deviations from 
         the mean, and the median and MADN of those deviations */
      if (nsum >= edgeMedCrit) {
        rsTrimmedMean(dxyEdge, nsum, 0.2f, &dxyEdge[2 * nvar], 
                      &edgeTrMeanX[ixy]);
        rsTrimmedMean(&dxyEdge[nvar], nsum, 0.2f, &dxyEdge[2 * nvar], 
                      &edgeTrMeanY[ixy]);
        for (i = 0; i < nsum; i++) {
          ex = dxyEdge[i] - edgeTrMeanX[ixy];
          ey = dxyEdge[i+nvar] - edgeTrMeanY[ixy];
          dxyEdge[i + 2 * nvar] = (float)sqrt((double)ex * ex + ey * ey);
          /* printf("%d %.2f,%.2f  %.2f,%.2f  %.3f\n", i, dxyEdge[i],
             dxyEdge[i+nvar], ex, ey, dxyEdge[i + 2 * nvar]); */
        }
        rsMedian(&dxyEdge[2 * nvar], nsum, dxyEdge, &edgeDevMed[ixy]);
        rsMADN(&dxyEdge[2 * nvar], nsum, edgeDevMed[ixy], dxyEdge,
                &edgeDevMADN[ixy]);
        /*printf("%d %s edges: trMean %.2f, %.2f, median dev %.3f MADN %.3f\n",
               nsum, ixy ?"Y":"X",  edgeTrMeanX[ixy],  edgeTrMeanY[ixy], 
               edgeDevMed[ixy], edgeDevMADN[ixy]); */
      } 
    }
  }

  /* Initialize stuff */
  for (ivar = 0; ivar < nvar; ivar++) {
    placed[ivar] = 0;
    ivarToList[ivar] = -1;
  }
  numNeigh = 0;
  numOnList = 0;

  /* Set up initial placement of pieces, loop on possibly unconnected sets */
  for (;;) {

    /* find min/max coordinates of the unplaced pieces and the piece nearest
       the middle of this */
    minxpc = bigint;
    minypc = bigint;
    maxxpc = -bigint;
    maxypc = -bigint;
    for (ivar = 0; ivar < nvar; ivar++) {
      if (!placed[ivar]) {
        minxpc = B3DMIN(minxpc, ixpclist[ivarpc[ivar]-fort]);
        maxxpc = B3DMAX(maxxpc, ixpclist[ivarpc[ivar]-fort]);
        minypc = B3DMIN(minypc, iypclist[ivarpc[ivar]-fort]);
        maxypc = B3DMAX(maxypc, iypclist[ivarpc[ivar]-fort]);
      }
    }
    /* printf("min/max %d %d %d %d\n", minxpc,maxxpc, minypc,maxypc); */
    if (minxpc == bigint)
      break;

    distmin = 1.e30;
    for (ivar = 0; ivar < nvar; ivar++) {
      if (!placed[ivar]) {
        dx = ixpclist[ivarpc[ivar]-fort] - 0.5 * (maxxpc + minxpc);
        dy = iypclist[ivarpc[ivar]-fort] - 0.5 * (maxypc + minypc);
        dist = dx * dx + dy * dy;
        if (dist < distmin) {
          distmin = dist;
          imin = ivar;
        }
      }
    }
    /* printf("INITIAL PIECE %d\n", imin); */

    /* Add this one to the list of variables then start searching */
    ivarToList[imin] = numOnList;
    listToVar[numOnList] = imin;
    listInd = numOnList++;
    while (listInd < numOnList) {
      xsum = 0.;
      ysum = 0.;
      nsum = 0;

      /* Look at a piece and its edges for neighbors */
      ipc = ivarpc[listToVar[listInd]]-fort;
      neighInd[listInd] = numNeigh;
      /* printf("Considering piece %d\n", ipc); */
      for (ixy = 0; ixy < 2; ixy++) {
        for (lowup = 0; lowup < 2; lowup++) {
          isign = 2 * lowup - 1;
          if (lowup)
            iedge = edgeUpper[xyStep*ipc + pcStep*ixy] - fort;
          else
            iedge = edgeLower[xyStep*ipc + pcStep*ixy] - fort;
          ind = xyStep*iedge + edgeStep*ixy;
          if (iedge >= 0 && ifskipEdge[ind] < skipCrit && 
              ind != leaveInd - fort) {
            if (lowup) 
              neighpc = pieceUpper[ind] - fort;
            else
              neighpc = pieceLower[ind] - fort;
            neighvar = indvar[neighpc] - fort;
            if (neighvar < 0)
              continue;
            /* printf("Neighbor pc %d var %d across edge %d  %d\n", 
               neighpc,neighvar,iedge, ixy); */
            /* If neighbor is not on var list yet, add it */
            if (ivarToList[neighvar] < 0) {
              /* printf("Add neighbor to var list\n"); */
              listToVar[numOnList] = neighvar;
              ivarToList[neighvar] = numOnList++;
            }

            /* Add neighbor to neighbor list and also the edge shift with the
               right polarity */
            nay = ivarToList[neighvar];
            ex = -idir * isign * dxedge[ind];
            dxyEdge[2 * numNeigh] = ex;
            ey = -idir * isign * dyedge[ind];
            dxyEdge[2 * numNeigh + 1] = ey;
            neighWgt[numNeigh] = 1.;
            edgeDir[numNeigh] = lowup + 2 * ixy;
            /* printf("dxyEdge %.2f %.2f %d %d %d %2.f %.2f\n", ex,ey, idir,
               isign, ind, dxedge[ind], dyedge[ind]); */
            neighList[numNeigh++] = nay;
             
            /* if the neighbor is placed, add estimated piece shift to sum */
            /* dxyvar is indexed as in C for convenience and then rearranged
               to correct indexing for the return */
            if (placed[neighvar]) {
              xsum += dxyvar[2*nay] - ex;
              ysum += dxyvar[2*nay + 1] - ey;
              nsum++;
            }
          }
        }
      }
         
      /* Get average piece shift and place this piece */
      if (nsum) {
        xsum /= nsum;
        ysum /= nsum;
      }
      dxyvar[2 * listInd] = xsum;
      dxyvar[2 * listInd + 1] = ysum;
      placed[listToVar[listInd]] = 1;
      /* printf("Placed piece at %f %f\n", xsum, ysum); */
      listInd++;
    }
  }
  neighInd[nvar] = numNeigh;
  /* printf("Set up %d neighbors\n", numNeigh);
  printf("Initial positions\n");
  for (list = 0; list < nvar; list++) {
    printf("%6.0f %6.0f  ",dxyvar[2 * list],dxyvar[2 * list+1]);
    if (list % 5 == 4 || list == nvar - 1) 
      printf("\n");
      } */

  /* Iterate */
  xmoveLast = 1.e10;
  xmoveAvg = 0.;
  ymoveLast = 1.e10;
  ymoveAvg = 0.;
  MAD = 0.;
  weightInterval = B3DMAX(1, B3DMIN(nvar / 2, maxIter / 10));
  computeWeights = 0;
  didWeights = 0;
  for (iter = 1; iter <= maxIter; iter++) {
    sumxmove = 0.;
    sumymove = 0.;
    xmovemax = 0.;
    ymovemax = 0.;
    dxsum = 0.;
    dysum = 0.;
    if (robustCrit > 0. && !(iter % weightInterval))
      computeWeights = 1;

    /* This is needed only for debug output */
    errsum = 0.;
    errmax = 0.;
    /*for (list = 0; list < nvar; list++) {
      for (i = neighInd[list]; i < neighInd[list+1]; i++) {
        nay = neighList[i];
        ex = dxyvar[2 * nay] - dxyvar[2 * list] - dxyEdge[2 * i];
        ey = dxyvar[2 * nay + 1] - dxyvar[2 * list + 1] - dxyEdge[2 * i + 1];
        ex = ex*ex+ey*ey;
        errsum+=ex;
        errmax = B3DMAX(errmax, ex);
      }
    }
    printf("Iteration %d errmax = %.2f\n", iter, sqrt(errmax)); */

    if (computeWeights) {
      numMedian = 0;
      medSum = 0.;
      for (list = 0; list < nvar; list++) {
        xsum = 0.;
        ysum = 0.;
        debug = 0;
        for (i = neighInd[list]; i < neighInd[list+1]; i++) {
          j = i - neighInd[list];
          nay = neighList[i];
          erx[j] = dxyvar[2 * nay] - dxyvar[2 * list] - dxyEdge[2 * i];
          ery[j] = dxyvar[2 * nay + 1] - dxyvar[2 * list + 1] - dxyEdge[2*i+1];
          /*if (fabs(fabs(dxyEdge[2 * i]) - 24.5) < 0.2 && 
              (fabs(fabs(dxyEdge[2 * i+1]) - 105) <0.2) )
              debug = 1; */
          findLowestThree(erx[j], j, &xlow, &xsec, &xthr);
          findLowestThree(ery[j], j, &ylow, &ysec, &ythr);
          xsum += erx[j];
          ysum += ery[j];
        }
        nsum = B3DMAX(1, neighInd[list+1] - neighInd[list]);
        if (nsum >= 3) {

          /* Get the median into xsec, ysec if 4 points */
          if (nsum > 3) {
            xsec = (xsec + xthr) / 2.;
            ysec = (ysec + ythr) / 2.;
          }

          /* Find deviation from median and get a median deviation */
          for (j = 0; j < nsum; j++) {
            ex = erx[j] - xsec;
            ey = ery[j] - ysec;
            errd[j] = (float)sqrt((double)(ex * ex + ey * ey));
            findLowestThree(errd[j], j, &elow, &esec, &ethr);
          }
          if (nsum > 3)
            esec = (esec + ethr) / 2.;
          medSum += esec;
          numMedian++;

          /* Get the bisquare weighting factor and compute a weight */
          if (MAD > 0.) {
            /*if (debug)
              printf("  %d  %.2f %.2f    %.3f\n", nsum, xsec, ysec, esec); */
            esec = B3DMAX(MAD, esec);
            for (j = 0; j < nsum; j++) {
              uu = (errd[j] - medThresh * MAD) / (Ktune * esec);
              wgt = 0.;
              if (uu < 1.)
                wgt = uu <= 0 ? 1.f : (1. - uu * uu) * (1. - uu * uu);
              /*if (debug)
                printf("%d  %.2f %.2f    %.3f  %.4f  %.4f\n", j, erx[j],ery[j],
                errd[j], uu, wgt); */
              neighWgt[neighInd[list] + j] = wgt;
            }
          }
        } else if (nsum == 2 && B3DMIN(numEdge[0], numEdge[1]) > edgeMedCrit &&
                   B3DMIN(edgeDevMADN[0], edgeDevMADN[1]) > 1.e-6) {
          
          /* For two edges, get their deviation from the median edge vector */
          for (j = 0; j < 2; j++) {
            i = neighInd[list] + j;
            ixy = edgeDir[i] / 2;
            isign = (edgeDir[i] % 2) ? 1 : -1;
            ex = dxyEdge[2*i] - isign * edgeTrMeanX[ixy];
            ey = dxyEdge[2*i+1] - isign * edgeTrMeanY[ixy];
            esec = (float)sqrt((double)(ex * ex + ey * ey));
            uu = (esec - edgeDevMed[ixy]) / (edgeDevMADN[ixy] *  4.685f);
            neighWgt[i] = 0.;
            if (uu < 1.)
              neighWgt[i] = uu <= 0. ? 1. : (1. - uu * uu) * (1. - uu * uu);
            /* if (debug)
               printf("Two: %d  %d  %.2f %.2f  %.3f  %.4f  %.4f\n", j, ixy, 
               ex, ey, esec, uu, neighWgt[i]); */
          }
          if (B3DMAX(neighWgt[i-1], neighWgt[i]) < 1.e-2)
            neighWgt[i-1] = neighWgt[i] = 1.;
        }
      }
      if (MAD > 0.) {
        didWeights = 1;
        computeWeights = 0;
      }
      
      MAD = 0.;
      if (numMedian)
        MAD = (float)B3DMAX(1.e-5, medSum / numMedian);
      /* printf("Average median dev = %.2f\n", MAD); */
    }

    /* Loop on pieces, adjusting each one by weighted average error in edges */
    for (list = 0; list < nvar; list++) {
      xsum = 0.;
      ysum = 0.;
      wsum = 0.;
      for (i = neighInd[list]; i < neighInd[list+1]; i++) {
        nay = neighList[i];
        wgt = neighWgt[i];
        ex = dxyvar[2 * nay] - dxyvar[2 * list] - dxyEdge[2 * i];
        ey = dxyvar[2 * nay + 1] - dxyvar[2 * list + 1] - dxyEdge[2 * i + 1];
        xsum += ex * wgt;
        ysum += ey * wgt;
        wsum += wgt;
      }
      if (wsum > 1.e-6) {
        xsum /= wsum;
        ysum /= wsum;
      }
      dxyvar[2 * list] += xsum;
      dxyvar[2 * list + 1] += ysum;
      dxsum += dxyvar[2 * list];
      dysum += dxyvar[2 * list + 1];
      sumxmove += fabs((double)xsum);
      sumymove += fabs((double)ysum);
      xmovemax = B3DMAX(xmovemax, fabs((double)xsum));
      ymovemax = B3DMAX(ymovemax, fabs((double)ysum));
    }


    /* Shift to zero mean */
    ex = dxsum / nvar;
    ey = dysum / nvar;
    for (list = 0; list < nvar; list++) {
      dxyvar[2 * list] -= ex;
      dxyvar[2 * list + 1] -= ey;
    }

    /* stop if change was low */
    if (xmovemax < critMaxMove && ymovemax < critMaxMove) {
      if (robustCrit <= 0. || didWeights)
        break;
      computeWeights = 1;
    }
    /*  printf("iter %d  err %.7f  xmove %.5f  ymove %.5f\n", iter,
        sqrt(errsum/numNeigh), sumxmove / nvar, sumymove / nvar); */
    /* Average the mean moves over some iterations, and test for a change
       in it periodically */
    if (iter % intervalForTest >= intervalForTest - numAvgForTest) {
      xmoveAvg += sumxmove / nvar;
      ymoveAvg += sumymove / nvar;
    }
    if (iter % intervalForTest == intervalForTest - 1) {
      xmoveAvg /= numAvgForTest;
      ymoveAvg /= numAvgForTest;
      if (xmoveLast - xmoveAvg < critMoveDiff && 
          ymoveLast - ymoveAvg < critMoveDiff) {
        if (robustCrit <= 0. || didWeights)
          break;
        computeWeights = 1;
      }
      xmoveLast = xmoveAvg;
      xmoveAvg = 0.;
      ymoveLast = ymoveAvg;
      ymoveAvg = 0.;
    }
  }

  // Compute weighted error mean and max
  errsum = 0.;
  errmax = 0.;
  nsum = 0;
  for (list = 0; list < nvar; list++) {
    for (i = neighInd[list]; i < neighInd[list+1]; i++) {
      nay = neighList[i];
      ex = dxyvar[2 * nay] - dxyvar[2 * list] - dxyEdge[2 * i];
      ey = dxyvar[2 * nay + 1] - dxyvar[2 * list + 1] - dxyEdge[2 * i + 1];
      ex = (float)sqrt((double)ex*ex+ey*ey) * neighWgt[i];
      errsum += ex;
      errmax = B3DMAX(errmax, ex);
      nsum++;
    }
  }
  *wErrMax = errmax;
  *wErrMean = errsum / B3DMAX(1, nsum);

  /*printf("Final positions after %d iterations\n", iter);
  for (list = 0; list < nvar; list++) {
    printf("%6.0f %6.0f  ",dxyvar[2 * list],dxyvar[2 * list+1]);
    if (list % 5 == 4 || list == nvar - 1) 
      printf("\n");
      } */
  /* Rearrange the data */
  for (ivar = 0; ivar < 2 * nvar; ivar++)
    dxyEdge[ivar] = dxyvar[ivar];
  for (ivar = 0; ivar < nvar; ivar++) {
    dxyvar[xyStep * ivar] = dxyEdge[2 * ivarToList[ivar]];
    dxyvar[xyStep * ivar + varStep] = dxyEdge[2 * ivarToList[ivar] + 1];
  }

  *numIter = iter;
  /* printf("%d iterations\n", iter);
  fflush(stdout); */
  return 0;
}

int findpieceshifts
(int *ivarpc, int *nvar, int *indvar, int *ixpclist, int *iypclist, 
 float *dxedge, float *dyedge, int *idir, int *pieceLower, int *pieceUpper, 
 int *ifskipEdge, int *edgeStep, float *dxyvar, int *varStep, int *edgeLower,
 int *edgeUpper, int *pcStep, int *work, int *fort, int *leaveInd, 
 int *skipCrit, float *robustCrit, float *critMaxMove, float *critMoveDiff,
 int *maxIter, int *numAvgForTest, int *intervalForTest, int *numIter,
 float *wErrMean, float *wErrMax)
{
  return findPieceShifts
    (ivarpc, *nvar, indvar, ixpclist, iypclist, dxedge, dyedge, *idir,
     pieceLower, pieceUpper, ifskipEdge, *edgeStep, dxyvar, *varStep, 
     edgeLower, edgeUpper, *pcStep, work, *fort, *leaveInd, *skipCrit,
     *robustCrit, *critMaxMove, *critMoveDiff, *maxIter, *numAvgForTest,
     *intervalForTest, numIter, wErrMean, wErrMax);
}

void findLowestThree(float val, int ind, float *lowest, float *second, 
                     float *third)
{
  if (!ind) {
    *lowest = val;
  } else if (val < *lowest) {
    if (ind > 1)
      *third = *second;
    *second = *lowest;
    *lowest = val;
    return;
  } else if (ind <= 1 || val < *second) {
    if (ind > 1)
      *third = *second;
    *second = val;
  } else if (ind <= 2 || val < *third)
    *third = val;
}

/*

$Log$
Revision 1.3  2010/12/28 18:06:28  mast
Added robust fitting

Revision 1.2  2010/06/19 23:41:42  mast
Removed one debug line

Revision 1.1  2010/06/19 23:24:09  mast
Added to library


*/

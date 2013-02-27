/*  piecefuncs.c : Functions for working with piece coordinates
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define checklist CHECKLIST
#define adjustpieceoverlap ADJUSTPIECEOVERLAP
#else
#define checklist checklist_
#define adjustpieceoverlap adjustpieceoverlap_
#endif

/*!
 * Examines a list of [npclist] piece coordinates in [pclist] for one dimension (x or y)
 * at a spacing of [stride] between successive values, applies a reduction factor
 * [redfac], and given the frame size [nframe], it finds the minimum coordinate
 * [minpiece], the # of pieces [npieces], and the overlap [noverlap]. It also checks 
 * that ALL coordinates on the list are multiples of frame size minus overlap.  Upon 
 * failure, it returns the line number at which that  check fails, and also return -1 
 * in [npieces].
 */
int checkPieceList(int *pclist, int stride, int npclist, int redfac, int nframe,
                   int *minpiece, int *npieces, int *noverlap)
{
  int i, j, mindiff, ipc, jpc, naddback, novertot, idiff, maxpiece;
  int *coords = NULL;
  mindiff = 100000;
  *minpiece = 100000;
  maxpiece = -100000;
  /* get min and max of piece coordinates and minimum non-zero difference */
  if (npclist > 1) 
    coords = B3DMALLOC(int, npclist);
  if (coords) {

    /* New way: sort the coordinates and then find minimum diff in one pass */
    for (i = 0; i < npclist; i++)
      coords[i] = pclist[i * stride];
    rsSortInts(coords, npclist);
    *minpiece = redfac * coords[0];
    maxpiece = redfac * coords[npclist - 1];
    for (i = 1; i < npclist; i++) {
      idiff = coords[i] - coords[i - 1];
      if (idiff)
        mindiff = B3DMIN(mindiff, redfac * idiff);
    }
        
  } else {

    /* The old way: pairwise comparisons */
    for (i = 0; i < npclist; i++) {
      ipc = redfac * pclist[i * stride];
      *minpiece = B3DMIN(*minpiece, ipc);
      maxpiece = B3DMAX(maxpiece, ipc);
      for (j = 0; j < npclist; j++) {
        jpc = redfac * pclist[j * stride];
        idiff = ipc - jpc;
        if (idiff < 0)
          idiff = -idiff;
        if (idiff)
          mindiff = B3DMIN(mindiff, idiff);
      }
    }  
  }

  /* now check and make sure all differences are multiples of minimum
     but if there were no non-zero differences, return 1 piece, 0 overlap */
  if (mindiff == 100000) {
    *npieces = 1;
    *noverlap = 0;
  } else {
    if (mindiff > 1.1 * nframe) {
           
      /* If difference is much bigger than frame size, find out how many
         frame sizes to add back to difference to make it positive, then
         divide positive difference by this # of times to get overlap
         THIS WILL FAIL UNDER EXTREME CONDITIONS */
           
      naddback = 1;
      novertot = nframe - mindiff;
      while (novertot < 0) {
        novertot = novertot + nframe;
        naddback++;
      }
      *noverlap = novertot / naddback;
      mindiff = nframe - *noverlap;
    }

    *npieces = 0;
    for (i = 0; i < npclist; i++) {
      idiff = redfac * pclist[i * stride] - *minpiece;
      if (idiff % mindiff) {
        *npieces = -1;
        return (i + 1);
      }
      *npieces = B3DMAX(*npieces, idiff / mindiff + 1);
    }
    *noverlap = nframe - mindiff;
  }
  return 0;
}

/*!
 * Fortran wrapper for @checkPieceList which assumes the [stride] is 1.  Upon error,
 * it prints a message, and still returns -1 in [npieces].
 */
void checklist(int *pclist, int *npclist, int *redfac, int *nframe, int *minpiece,
               int *npieces, int *noverlap)
{
  int retval = checkPieceList(pclist, 1, *npclist, *redfac, *nframe, minpiece, npieces, 
                              noverlap);
  if (retval) {
    printf("Piece coordinates not regularly spaced apart or corrupted at line %d\n",
           retval);
    fflush(stdout);
  }
}

/*!
 * Adjusts a list of [npclist] piece coordinates in [pclist] for one dimension (x or y),
 * at a spacing of [stride] between successive values, changing the overlap from 
 * [noverlap] to [newOverlap], given the frame size [nframe] and the minimum coordinate
 * [minpiece] (which is retained).
 */
void adjustPieceOverlap(int *pclist, int stride, int npclist, int nframe, int minpiece,
                        int noverlap, int newOverlap)
{
  int ind, i;
  for (i = 0; i < npclist; i++) {
    ind = (pclist[i * stride] - minpiece) / (nframe - noverlap);
    pclist[i * stride] = ind * (nframe - newOverlap) + minpiece;
  }
}

/*! Fortran wrapper for @adjustPieceOverlap that assumes a stride of 1. */
void adjustpieceoverlap(int *pclist, int *npclist, int *nframe, int *minpiece,
                        int *noverlap, int *newOverlap)
                        {
  adjustPieceOverlap(pclist, 1, *npclist, *nframe, *minpiece, *noverlap, *newOverlap);
}

/*
 * taperatfill.c - Taper a slice at fill areas on edges
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */

#include <stdlib.h>
#include "cfsemshare.h"
#include "mrcslice.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define taperatfill TAPERATFILL
#else
#define taperatfill taperatfill_
#endif

#define PLIST_CHUNK 1000
/*!
 * Analyzes for fill areas at the edge of the slice [sl], finds the borders 
 * between actual image and fill areas, and tapers the image intensities down 
 * to the fill value over [ntaper] pixels.  If [inside] is 0 the image 
 * intensities at the edge are extended into the fill area, while if [inside] 
 * is 1 then pixels inside the image are attenuated.  Returns -1 for memory
 * allocation errors.
 */
int sliceTaperAtFill(Islice *sl, int ntaper, int inside)
{
  struct pixdist {
    unsigned short int x, y;
    unsigned short dist;
    signed char dx, dy;
  };

  Ival val;
  float fracs[128], fillpart[128];
  float fillval, lastval;
  int len, longest;
  int i, ix, iy, xnext, ynext, xp, yp, lastint, interval, found;
  int dir, tapdir, col, row, ncol, ind, xstart, ystart, ndiv;
  int dist;
  struct pixdist *plist;
  int pllimit = PLIST_CHUNK;
  int plsize = 0;
  unsigned char *bitmap;
  int xsize = sl->xsize;
  int ysize = sl->ysize;
  int bmsize = (xsize * ysize + 7) / 8;
  int dxout[4] = {0, 1, 0, -1};
  int dyout[4] = {-1, 0, 1, 0};
  int dxnext[4] = {1, 0, -1, 0};
  int dynext[4] = {0, 1, 0, -1};

  inside = inside != 0 ? 1 : 0;

  /* find longest string of repeated values along the edge */
  longest = 0;
  for (iy = 0; iy < ysize; iy += ysize - 1) {
    len = 0;
    sliceGetVal(sl, 0, iy, val);
    lastval = val[0];
    for (ix = 1; ix < xsize; ix++) {
      sliceGetVal(sl, ix, iy, val);
      if (val[0] == lastval) {
        /* same as last, add to count, see if this is a new
           best count*/
        len++;
        if (len > longest) {
          longest = len;
          fillval = lastval;
        }
      } else {
        /* different, reset count and set new lastval */
        len = 0;
        lastval = val[0];
      }
    }
  }         
  for (ix = 0; ix < xsize; ix += xsize - 1) {
    len = 0;
    sliceGetVal(sl, ix, 0, val);
    lastval = val[0];
    for (iy = 1; iy < ysize; iy++) {
      sliceGetVal(sl, ix, iy, val);
      if (val[0] == lastval) {
        /* same as last, add to count, see if this is a new
           best count */
        len++;
        if (len > longest) {
          longest = len;
          fillval = lastval;
        }
      } else {
        /* different, reset count and set new lastval */
        len = 0;
        lastval = val[0];
      }
    }
  }         

  /* If length below a criterion (what?) , return without error */
  if (longest < 10)
    return 0;

  /* set the slice mean so that sliceGetValue will return this outside the
     edges */
  sl->mean = fillval;

  /* look from left edge along a series of lines to find the first point
     that is not a fill point */
  lastint = 0;
  found = 0;
  for (ndiv = 2; ndiv <= ysize; ndiv++) {
    interval = (ysize + 2) / ndiv;
    if (interval == lastint)
      continue;
    lastint = interval;
    for (iy = interval; iy < ysize; iy += interval) {
      for (ix = 0; ix < xsize; ix++) {
        sliceGetVal(sl, ix, iy, val);
        if (val[0] != fillval) {
          found = 1;
          break;
        }
      }
      if (found)
        break;
    }
    if (found)
      break;
  }

  if (!found)
    return 0;

  /* Get initial chunk of pixel list and bitmap */
  plist = (struct pixdist *)malloc(PLIST_CHUNK * sizeof(struct pixdist));
  if (plist)
    bitmap = (unsigned char *)malloc(bmsize);
  if (!plist || !bitmap) {
    if (plist)
      free(plist);
    return (-1);
  }

  /* clear bitmap */
  for (i = 0; i < bmsize; i++)
    bitmap[i] = 0;

  dir = 3;
  xstart = ix;
  ystart = iy;
  tapdir = 1 - 2 * inside;

  do {
    ncol = 1;
    xnext = ix + dxout[dir] + dxnext[dir];
    ynext = iy + dyout[dir] + dynext[dir];
    sliceGetVal(sl, xnext, ynext, val);
    if (val[0] != fillval) {
      /* case 1: inside corner */
      ix = xnext;
      iy = ynext;
      dir = (dir + 3) % 4;
      if (inside)
        ncol = ntaper + 1;
    } else {
      xnext = ix + dxnext[dir];
      ynext = iy + dynext[dir];
      sliceGetVal(sl, xnext, ynext, val);
      if (val[0] != fillval) {
        /* case 2: straight edge to next pixel */
        ix = xnext;
        iy = ynext;
      } else {
        /* case 3: outside corner, pixel stays the same */
        dir = (dir + 1) % 4;
        if (!inside)
          ncol = ntaper + 1;
      }
    }
      
    /* If outside pixel is outside the data, nothing to add to lists */
    xp = ix + dxout[dir];
    yp = iy + dyout[dir];
    if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
      continue;

    /* Loop on all the pixels to mark */
    for (col = 0; col < ncol; col++) {
      for (row = 1 - inside; row <= ntaper - inside; row++) {
        xp = ix + tapdir * row * dxout[dir] - col * dxnext[dir];
        yp = iy + tapdir * row * dyout[dir] - col * dynext[dir];

        /* skip if pixel outside area */
        if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
          continue;

        /* skip marking outside pixels for inside taper or
           inside pixels for outside taper */
        sliceGetVal(sl, xp, yp, val);
        if ((inside && val[0] == fillval) || 
            (!inside && val[0] != fillval))
          continue;

        dist = col * col + row * row;
        ind = xsize * yp + xp;
        if (bitmap[ind / 8] & (1 << (ind % 8))) {

          /* If the pixel is already marked, find it on the
             list and see if it's closer this time */
          for (i = plsize - 1; i >= 0; i--) {
            if (plist[i].x == xp && plist[i].y == yp) {
              if (plist[i].dist > dist) {
                plist[i].dist = dist;
                plist[i].dx = (signed char)(ix - xp);
                plist[i].dy = (signed char)(iy - yp);
              }
              break;
            }
          }
        } else {

          /* Otherwise, mark pixel in bitmap and make a new 
             entry on the list */
          bitmap[ind / 8] |= (1 << (ind % 8));
          if (plsize >= pllimit) {
            pllimit += PLIST_CHUNK;
            plist = (struct pixdist *) realloc(plist,
                                               pllimit * sizeof(struct pixdist));
            if (!plist) {
              free (bitmap);
              return (-1);
            }
          }
          plist[plsize].x = xp;
          plist[plsize].y = yp;
          plist[plsize].dist = dist;
          plist[plsize].dx = (signed char)(ix - xp);
          plist[plsize++].dy = (signed char)(iy - yp);
        }
      }
    }

  } while (ix != xstart || iy != ystart || dir != 3);

  /* make tables of fractions and amounts to add of fillval */
  for (i = 1; i <= ntaper; i++) {
    dist = inside ? i : ntaper + 1 - i;
    fracs[i] = (float)dist / (ntaper + 1);
    fillpart[i] = (1. - fracs[i]) * fillval;
  }

  /* Process the pixels on the list */
  for (i = 0; i < plsize; i++) {
    ind = sqrt((double)plist[i].dist) + inside;
    if (ind > ntaper)
      continue;
    ix = plist[i].x;
    iy = plist[i].y;
    if (inside) {
      xp = ix;
      yp = iy;
    } else {
      xp = ix + plist[i].dx;
      yp = iy + plist[i].dy;
    }
    sliceGetVal(sl, xp, yp, val);
    /*  val[0] = fracs[plist[i].dist] * val[0] + fillpart[plist[i].dist]; */
    val[0] = fracs[ind] * val[0] + fillpart[ind];
    slicePutVal(sl, ix, iy, val);
  }

  free(plist);
  free(bitmap);
  return 0;
}

/*! 
 * Fortran wrapper to @sliceTaperAtFill for tapering a real*4 image in [array]
 * of size [nx] by [ny].  Returns -1 for memory error.
 */
int taperatfill(float *array, int *nx, int *ny, int *ntaper, int *inside)
{
  Islice slice;
  sliceInit(&slice, *nx, *ny, SLICE_MODE_FLOAT, array);
  return sliceTaperAtFill(&slice, *ntaper, *inside);
}

/*

$Log$

*/

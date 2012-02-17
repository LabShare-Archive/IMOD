/*
 *  autocont.c -- Routines for for autocontouring used by imodauto and 3dmod/autox
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <math.h>
#include <string.h>
#include "imodel.h"

static void autoPatchFillOutside(unsigned char *data, int *xlist, int *ylist, 
                                 int listsize, int xsize, int xmin, int xmax, int ymin,
                                 int ymax, int x, int y);
static int nay8(int i, int j);

/*!
 * Marks the area outside of the "flooded" region of pixels in [data], ones marked with 
 * AUTOX_FLOOD, with the patch flag, AUTOX_PATCH, then makes all unmarked pixels be part 
 * of the flooded region, thus filling in the interior of the flood. The size of the 
 * data array is given in [size] and [ysize]; [xlist] and [ylist] are temporary arrays
 * with size given by [listsize], which should be at least 4 * ([xsize] + [ysize]) 
 */
void imodAutoPatch(unsigned char *data, int *xlist, int *ylist, int listsize, int xsize,
                   int ysize)
{
  int i, x, y;
  int xysize;
  int xmax = -1;
  int xmin = xsize;
  int ymax = -1;
  int ymin = ysize;

  /* get min and max of flooded area */
  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      if (data[x + y * xsize] & AUTOX_FLOOD) {
        if (x < xmin)
          xmin = x;
        if (x > xmax)
          xmax = x;
        if (y < ymin)
          ymin = y;
        if (y > ymax)
          ymax = y;
      }

  /* Start a patch from every point along the four sides, because there
     may be isolated patches */
  for (x = xmin; x <= xmax; x++) {
    autoPatchFillOutside(data, xlist, ylist, listsize, xsize, xmin, xmax, ymin, ymax, x,
                         ymin);
    autoPatchFillOutside(data, xlist, ylist, listsize, xsize, xmin, xmax, ymin, ymax, x,
                         ymax);
  }
  for (y = ymin; y <= ymax; y++) {
    autoPatchFillOutside(data, xlist, ylist, listsize, xsize, xmin, xmax, ymin, ymax,
                         xmin, y);
    autoPatchFillOutside(data, xlist, ylist, listsize, xsize, xmin, xmax, ymin, ymax,
                         xmax, y);
  }

  xysize = xsize * ysize;

  /* Mark everything now not in a patch as in the flood */
  for (y = ymin; y <= ymax; y++)
    for (x = xmin; x <= xmax; x++) {
      i = x + y * xsize;
      if (!(data[i] & (AUTOX_FLOOD | AUTOX_PATCH)))
        data[i] |= AUTOX_FLOOD;
    }
     
  /* Clear the patch flags */
  for ( i = 0; i < xysize; i++)
    if (data[i] & AUTOX_PATCH)
      data[i] &= ~AUTOX_PATCH;
}

/* To build a patch (add points to existing patch) from a single point */
static void autoPatchFillOutside(unsigned char *data, int *xlist, int *ylist, 
                                 int listsize, int xsize, int xmin, int xmax, int ymin,
                                 int ymax, int x, int y)
{
  int ringnext = 0;
  int ringfree = 1;
  int pixind;
  unsigned char neighflag;
 
  /* Don't even start if this point is a patch or a flood */
  pixind = x + y * xsize;
  if (data[pixind] & (AUTOX_FLOOD | AUTOX_PATCH))
    return;

  /* initialize the ring buffer */
  xlist[0] = x;
  ylist[0] = y;
  data[pixind] |= (AUTOX_CHECK | AUTOX_PATCH);
  neighflag  = AUTOX_FLOOD | AUTOX_CHECK | AUTOX_PATCH;

  while (ringnext != ringfree) {

    /* the next point on list got there by being neither patch nor
       flood, so it needs no checking or marking */
    x = xlist[ringnext];
    y = ylist[ringnext];
    pixind = x + y * xsize;

    /* add each of four neighbors on list if coordinate is legal
       and they are not already on list or in flood or patch. 
       Mark each as on list and in patch */
    if (x > xmin && !(data[pixind - 1] & neighflag)) {
      xlist[ringfree] = x - 1;
      ylist[ringfree++] = y;
      ringfree %= listsize;
      data[pixind - 1] |= (AUTOX_CHECK | AUTOX_PATCH);
    }
    if (x < xmax && !(data[pixind + 1] & neighflag)) {
      xlist[ringfree] = x + 1;
      ylist[ringfree++] = y;
      ringfree %= listsize;
      data[pixind + 1] |= (AUTOX_CHECK | AUTOX_PATCH);
    }
    if (y > ymin && !(data[pixind - xsize] & neighflag)) {
      xlist[ringfree] = x;
      ylist[ringfree++] = y - 1;
      ringfree %= listsize;
      data[pixind - xsize] |= (AUTOX_CHECK | AUTOX_PATCH);
    }
    if (y < ymax && !(data[pixind + xsize] & neighflag)) {
      xlist[ringfree] = x;
      ylist[ringfree++] = y + 1;
      ringfree %= listsize;
      data[pixind + xsize] |= (AUTOX_CHECK | AUTOX_PATCH);
    }
          
    /* Take point off list, advance next pointer */
    data[pixind] &= ~AUTOX_CHECK;
    ringnext++;
    ringnext %= listsize;
  }
}

/* Testing the eight neighbors of a point: if the point is in the flood, counts up the 
   neighbors that are also in flood */
static unsigned char *sData;
static int sXsize;
static int sYsize;
static int nay8(int i, int j)
{
  int n, m, k = 0;
  int x, y;

  if (!(sData[i + (j * sXsize)] & AUTOX_FLOOD))
    return(0);
     
  for (n = -1; n <= 1; n++) {
    y = n + j;
    for(m = -1; m <= 1 ; m++) {
      x = m + i;
      if ((x >= 0) && (y >= 0) && (x < sXsize) && (y < sYsize))
        if (sData[x + (y * sXsize)] & AUTOX_FLOOD)
          k++; 
    }
  }
  return(k-1);
}

/*!
 * Shrinks the area in [data] marked with the flag AUTOX_FLOOD by eliminating every
 * point with fewer than 7 neighbors in the flood.  [imax] and [jmax] are the X and Y 
 * sizes of [data].
 */
void imodAutoShrink(unsigned char *data, int imax, int jmax)
{
  int i, j;
  sData = data;
  sXsize = imax;
  sYsize = jmax;
     
  /* DNM: tried testing on fill flag before checking neighbors and it
     didn't work. */
  for (j = 0; j < jmax; j++) {
    for (i = 0; i < imax; i++) {
      if (nay8(i, j) < 7)
        data[i + (j * imax)] |= AUTOX_CHECK;
    }
  }

  /* DNM: clear check flag after use, not before */
  for (j = 0; j < jmax; j++) {
    for (i = 0; i < imax; i++) {
      if (data[i + (j * imax)] & AUTOX_CHECK)
        data[i + (j * imax)] &= ~(AUTOX_FLOOD | AUTOX_CHECK);
    }
  }
}

/*!
 * Expands the area in [data] marked with the flag AUTOX_FLOOD or AUTOX_PATCH by adding
 * all 8 neighbors around each marked by, marking it with AUTOX_FLOOD.
 * [imax] and [jmax] are the X and Y sizes of [data].
 */
void imodAutoExpand(unsigned char *data, int imax, int jmax)
{
  int i, j, m, n, x, y;
     
  for (j = 0; j < jmax; j++) {
    for (i = 0; i < imax; i++) {
      if (!(data[i + (j * imax)] & AUTOX_FILL)) 
        continue;

      for (m = -1; m <= 1; m++) {
        y = j + m;
        if ((y < 0) || (y >= jmax)) 
          continue;
        for (n = -1; n <= 1; n++) {
          x = n + i;
          if ((x == i) && (y == j)) 
            continue;
          if ((x < 0) || (x >= imax)) 
            continue;
          data[x + (y * imax)] |= AUTOX_CHECK;
        }
      }
    }
  }

  /* DNM: clear check flag in this loop, not before use */
  for (i = 0; i < imax * jmax; i++) {
    if (data[i] & AUTOX_CHECK) {
      data[i] |= AUTOX_FLOOD;
      data[i] &= ~AUTOX_CHECK;
    }
  }
}



#define RIGHT_EDGE   16
#define TOP_EDGE     (RIGHT_EDGE << 1)
#define LEFT_EDGE    (RIGHT_EDGE << 2)
#define BOTTOM_EDGE  (RIGHT_EDGE << 3)
#define ANY_EDGE    (RIGHT_EDGE | TOP_EDGE | LEFT_EDGE | BOTTOM_EDGE)

/*!
 * Forms contours around marked points in an array. ^
 * [data] is an array of flags marking the image points. ^
 * [imdata] is the corresponding actual image array, which is used to compute
 * interpolated positions for the edges between marked and unmarked points.
 * If is NULL, no interpolation is done and contours will follow horizontal,
 * vertical, and 45 degree diagonal lines. ^
 * [xsize] and [ysize] specify the X and Y dimensions of the array. ^
 * [z] is the Z value to assign to the contours. ^
 * [testmask] is the value to AND with the image flag points to select them.
 * The mask should use the first four bits only. ^
 * If [diagonal] is non-zero, then pixels that touch only at corners will be
 * contained within the same contour. ^
 * [threshold] is the threshold to use for interpolating the edge position
 * between pixels.  If it is less than 0 and [imdata] is defined, then an 
 * effective threshold will be computed from the mean value of pixels on
 * both sides of the edges. ^
 * [reverse] should be set non-zero if marked pixels have lower values than
 * unmarked ones. ^
 * The number of contours created is returned in [ncont]. ^
 * The function returns a pointer to an array of contours, or NULL for error.
 */
/* DNM 1/17/01: changed to handle edges of image better, added testmask
   argument so imod/autox can use it 
   DNM 1/25/01: implemented new algorithm to walk around edges and build
   contours in order, instead of just putting edge points into a contour and
   trying to sort them out afterwards 
   DNM 5/25/08: implemented interpolation and threhsold computation. */
Icont *imodContoursFromImagePoints(unsigned char *data, unsigned char **imdata,
                                   int xsize, int ysize, int z, 
                                   unsigned char testmask, int diagonal,
                                   float threshold, int reverse, int *ncont)
{
  Ipoint point;
  Icont *contarr = NULL;
  Icont *cont;
  int i, j, itst, jtst;
  int side;
  int xs, ys;
  int edgemask[4] = {RIGHT_EDGE, TOP_EDGE, LEFT_EDGE, BOTTOM_EDGE};
  int nextx[4] = {0, -1, 0, 1};
  int nexty[4] = {1, 0, -1, 0};
  int cornerx[4] = {1, -1, -1, 1};
  int cornery[4] = {1, 1, -1, -1};
  /* float delx[4] = {0.95, 0.5, 0.05, 0.5};
     float dely[4] = {0.5, 0.95, 0.5, 0.05}; */
  int otherx[4] = {1, 0, -1, 0};
  int othery[4] = {0, 1, 0, -1};
  float frac;
  int found, iedge, ixst, iyst, iedgest, nsum, nayx, nayy, polarity, diff = 0;
  double edgeSum;

  xs = xsize - 1;
  ys = ysize - 1;
  point.z = z;
  *ncont = 0;
  nsum = 0;
  edgeSum = 0.;
  polarity = reverse ? -1 : 1;
  if (!imdata)
    threshold = -1.;

  /* Go through all points including the edges of the image area, and 
     mark all the edges of defined area.  Compute a  */

  for (j = 0; j < ysize; j++)
    for(i = 0; i < xsize; i++) {
      if (data[i + (j * xsize)] & testmask) {

        /* Mark a side if on edge of image, or if next pixel over
           is not in the set */
        side = 0;
        if (i == xs)
          side |= RIGHT_EDGE;
        else if (!(data[(i + 1) + (j * xsize)] & testmask)) {
          side |= RIGHT_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j][i + 1];
            nsum++;
          }
        }
        if (j == ys)
          side |= TOP_EDGE;
        else if (!(data[i + ((j + 1) * xsize)] & testmask)) {
          side |= TOP_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j + 1][i];
            nsum++;
          }
        }
        if (!i)
          side |= LEFT_EDGE;
        else if (!(data[(i - 1) + (j * xsize)] & testmask)) {
          side |= LEFT_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j][i - 1];
            nsum++;
          }
        }
        if (!j)
          side |= BOTTOM_EDGE;
        else if (!(data[i + ((j - 1) * xsize)] & testmask)) {
          side |= BOTTOM_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j - 1][i];
            nsum++;
          }
        }
        data[i +(j * xsize)] |= side;
        /* if (side) printf("i %d  j %d  side %d  data %d\n",
           i, j, side, data[i +(j * xsize)]); */
      }
    }

  if (nsum) {
    threshold = 0.5 * edgeSum / nsum;
    /* printf("Derived threshold %.2f\n", threshold); */
  }

  found = 1;
  while (found) {
    found = 0;
    for (j = 0; j < ysize && !found; j++)
      for(i = 0; i < xsize; i++){
        if (!(data[i + (j * xsize)] & ANY_EDGE))
          continue;
            
        /* find lowest edge */
        for (iedge = 0; iedge < 4; iedge++)
          if (data[i + j * xsize] & edgemask[iedge])
            break;

        /* Start a new contour */
        if (*ncont) {
          cont = (Icont *) realloc(contarr, (*ncont + 1) *
                                   sizeof(Icont));
        } else {
          cont = (Icont *)malloc(sizeof(Icont));
        }
        if (!cont) {
          b3dError(stderr, "imodContoursFromImagePoints: "
                    "failure to allocate memory for contours");
          return (contarr);
        } else {
          (*ncont)++;
          contarr = cont;
        }
        cont = &contarr[*ncont - 1];
        imodContourDefault(cont);

        /* keep track of starting place and stop when reach 
           it again */
        iedgest = iedge;
        ixst = i;
        iyst = j;
        /* printf ("starting %d %d %d\n", ixst, iyst, iedge); */
        while (!cont->psize || i != ixst || j != iyst || iedge != iedgest) {

          if (!diagonal) {
            /* If no diagonals, look for next edge first 
               around corner on same pixel */
            if (data[i + j * xsize] & edgemask[(iedge + 1) % 4]) {
              iedge = (iedge + 1) % 4;
            } else if (data[i + nextx[iedge] + (j + nexty[iedge]) * xsize] &
                       edgemask[iedge]) {
              /* same edge, next pixel */
              i += nextx[iedge];
              j += nexty[iedge];
            } else {
              /* pixel on an inside corner - it's got to
                 be, but put in check for testing */
              i += cornerx[iedge];
              j += cornery[iedge];
              iedge = (iedge + 3) % 4;
              if (!(data[i + j * xsize] & edgemask[iedge]))
                printf("no edge around corner at i %d, j %d, edge %d\n", i, j, iedge);
            }
          } else {
            /* If diagonals, look for next edge first on pixel 
               around inside corner if it's legal */
            itst = i + cornerx[iedge];
            jtst = j + cornery[iedge];
            if (itst >= 0 && itst < xsize && jtst >= 0 && jtst < ysize && 
                (data[itst + jtst * xsize] & edgemask[(iedge + 3) % 4])){
              i = itst;
              j = jtst;
              iedge = (iedge + 3) % 4;
            } else {
              itst = i + nextx[iedge];
              jtst = j + nexty[iedge];
              if (itst >= 0 && itst < xsize && jtst >= 0 && jtst < ysize && 
                  data[i + nextx[iedge] + (j + nexty[iedge]) * xsize] & edgemask[iedge]) {
                /* then same edge, next pixel */
                i += nextx[iedge];
                j += nexty[iedge];
              } else {
                /* go around corner on this pixel - the
                   edge has to be there, but put in check
                   for testing */
                iedge = (iedge + 1) % 4;
                if (!(data[i + j * xsize] & edgemask[iedge]))
                  printf("no edge around corner at i %d, j %d, edge %d\n", i, j, iedge);
              }
            }
          }
          
          frac = 0.45;
          nayx = i + otherx[iedge];
          nayy = j + othery[iedge];
          if (threshold > 0 && nayx >= 0 && nayx < xsize && nayy >= 0 && nayy < ysize) {
            diff = imdata[nayy][nayx] - imdata[j][i];
            if (polarity * diff < 0) {
              frac = (threshold - imdata[j][i]) / diff;
              frac = B3DMIN(0.99, frac);
              frac = B3DMAX(0.01, frac);
            }
          }
          point.x = i + 0.5 + frac * otherx[iedge];
          point.y = j + 0.5 + frac * othery[iedge];

          /* add the point and clear the edge */
          /* point.x = i + delx[iedge];
             point.y = j + dely[iedge]; */
          imodPointAdd(cont, &point, cont->psize);
          data[i + j * xsize] &= ~edgemask[iedge];
          /* printf ("at %d %d %d, adding %f %f   diff %d  frac %.2f\n", 
             i, j, iedge, point.x, point.y, diff, frac); */

        }
        found = 1;
        break;
      }
  }
  return(contarr);
}


/*
 * cubinterp.c : cubic or linear interpolation of an image
 * Translated from a fortran routine
 * written by David Mastronarde, April 2000.
 * 
 * $Id$
 */
#include <math.h>
#include <stdlib.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "warpfiles.h"

/*!
 * Corrects for a distortion field and then applies a linear transformation, or applies 
 * a linear transformation then a warping field, using cubic or linear interpolation.
 * 
 * ^ [array]   - The input image array
 * ^ [bray]    - The output image array
 * ^ [nxa,nya] - The dimensions of [array]
 * ^ [nxb,nyb] - The dimensions of [bray]
 * ^ [amat]    - A 2x2 matrix to specify rotation,scaling,skewing
 * ^ [xc,yc]   - The coordinates of the center of ARRAY
 * ^ [xt,yt]   - The translation to add in the linear transformation
 * ^ [scale]   - A multiplicative scale factor for the intensities
 * ^ [dmean]   - Mean intensity of image, or value to fill edges with
 * ^ [linear]  - Set greater than zero to do linear interpolation, or less than zero for
 * nearest neighbor interpolation
 * ^ [linFirst] - Set non-zero to do linear transformation before warping
 * ^ [dxGrid, dyGrid] - Grid of distortion displacements in X & Y;
 * each value is the shift to get from a desired position in the
 * undistorted or warped image to the corresponding position in the original image
 * ^ [ixgDim]  - The first dimension of [dxGrid] and [dyGrid]
 * ^ [xGridStrt, yGridStrt] - Coordinates at which grid starts in output image
 * ^ [xGridIntrv, yGridIntrv] - Spacing between grid points in X & Y
 * ^ [nxGrid, nyGrid] - Number of grid points in X & Y
 * 
 * The linear transformation is done as:
 * ^ Xo = a11(Xi - Xc) + a12(Yi - Yc) + NXB/2. + XT
 * ^ Yo = a21(Xi - Xc) + a22(Yi - Yc) + NYB/2. + YT
 * ^  where X coordinates run from 0 at the left edge of the first pixel
 * to [nxa] at the right edge of the last pixel, etc. ^
 * When calling from C, indices in [amat] are transposed: a11 = amat\[0\]\[0\],
 * a12 = amat\[1\]\[0\], a21 = amat\[0\]\[1\], a22 = amat\[1\]\[1\]  ^
 * To call from Fortran, use arguments of the same type and in the same order.
 * Indices in [amat] are in logical order: a11 = amat(1,1), a12 = amat(1,2), 
 * etc.  ^
 * Both undistorting and warping are parallelized with OpenMP.
 */
void warpInterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
                float amat[2][2], float xc, float yc, float xt, float yt, float scale,
                float dmean, int linear, int linFirst, float *dxGrid, float *dyGrid,
                int ixgDim, int nxGrid, int nyGrid, float xGridStrt, float yGridStrt,
                float xGridIntrv, float yGridIntrv)
{
  float xcen,ycen,xco,yco,denom,a11,a12,a22,a21,dyo,xbase,ybase;
  float xp,yp,dx,dy,v2,v4, ox, oy, xstep, ystep;
  float dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3,x,y;
  int iy,ix,ixp,ixpp1,iyp,iypp1,ixpm1,iypm1,ixpm2, iypm2, allIn, index, i, j;
  size_t ixbase,llnxa,ind,indpnxa,indmnxa,indpnxa2;
  int numThreads, ixgStart, ixgEnd, iygStart, iygEnd;
  float *buf;
  float fx, fy, gridfy, xbox, ybox, xmap[2][2], ymap[2][2], xlim[2], ylim[2];
  int ixgrid, iygrid, indy[2], indx[2], ixlim[2], iylim[2];


  /* Calc inverse transformation */
  xcen = nxb / 2. + xt + 0.5;
  ycen = nyb / 2. + yt + 0.5;
  xco = xc + 0.5;
  yco = yc + 0.5;
  denom = amat[0][0] * amat[1][1] - amat[1][0] * amat[0][1];
  a11 =  amat[1][1] / denom;
  a12 = -amat[1][0] / denom;
  a21 = -amat[0][1] / denom;
  a22 =  amat[0][0] / denom;
  llnxa = (size_t)nxa;

  /* Limit the number of threads  TODO: SEE IF NEEDED */
  numThreads = B3DNINT(0.04 * sqrt((double)nxb * nyb));
  numThreads = numOMPthreads(numThreads);
  
  if (!linFirst) {

    /* UNDISTORT FOLLOWED BY LINEAR TRANSFORM */
    /* Use 1-based pixel coordinates to match fortran version of undistinterp */
    /* loop over output image */
#pragma omp parallel for num_threads(numThreads) \
  shared(a11,a12,a21,a22,xco,yco,xcen,ycen,array,bray,nxa,nya,nxb,nyb,scale,\
         dmean,linear, dxGrid, dyGrid, ixgDim, nxGrid, xGridStrt, xGridIntrv, nyGrid, \
         yGridStrt, yGridIntrv)                                         \
  private(iy,dyo,xbase,ybase)                                           \
  private(xp,yp,dx,dy,v1,v2,v3,v4)                                      \
  private(dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1)                      \
  private(ix,ixp,ixpp1,iyp,iypp1,ixpm1,iypm1,ixpm2,iypm2,ixbase)
    for (iy = 1; iy <= nyb; iy++) {
      ixbase = ((size_t)iy - 1) * nxb - 1;
      dyo = iy - ycen;
      xbase = a12 * dyo +xco - a11 * xcen;
      ybase = a22 * dyo + yco - a21 * xcen;

      if (linear > 0) {

        /* linear interpolation */
        for (ix = 1; ix <= nxb; ix++) {
          xp = a11*ix + xbase;
          yp = a21*ix + ybase;
          interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid, nyGrid, xGridStrt, 
                           yGridStrt, xGridIntrv, yGridIntrv, &dx, &dy);
          xp = xp + dx;
          yp = yp + dy;
          ixp = xp;
          iyp = yp;
          bray[ix + ixbase] = dmean;
          if (ixp >= 1 && ixp < nxa && iyp >= 1 && iyp < nya) {
            dx = xp - ixp;
            dy = yp - iyp;
            ixpm1 = ixp - 1;
            iypm1 = iyp - 1;
            bray[ix + ixbase] = 
              (1. - dy) * ((1. - dx) * array[ixpm1 + iypm1*nxa] +
                           dx * array[ixp + iypm1*nxa]) +
              dy * ((1. - dx) * array[ixpm1 + iyp*nxa] +
                    dx * array[ixp + iyp*nxa]);
          }
        }
      } else if (linear < 0) {

        /* Nearest neighbor */
        for (ix = 1; ix <= nxb; ix++) {
          xp = a11*ix + xbase;
          yp = a21*ix + ybase;
          interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid, nyGrid, xGridStrt, 
                           yGridStrt, xGridIntrv, yGridIntrv, &dx, &dy);
          ixp = xp + dx + 0.5;
          iyp = yp + dy + 0.5;
          if (ixp >= 1 && ixp <= nxa && iyp >= 1 && iyp <= nya)
            bray[ix + ixbase] = array[ixp - 1 + (iyp - 1) * nxa];
          else
            bray[ix + ixbase] = dmean;
        }
      } else {
           
        /* cubic interpolation */
        for (ix = 1; ix <= nxb; ix++) {
          xp = a11*ix + xbase;
          yp = a21*ix + ybase;
          interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid, nyGrid, xGridStrt, 
                           yGridStrt, xGridIntrv, yGridIntrv, &dx, &dy);
          xp = xp + dx;
          yp = yp + dy;
          ixp = xp;
          iyp = yp;
          bray[ix + ixbase] = dmean;
          if (ixp >= 2 && ixp < nxa - 1 && iyp >= 2 && iyp < nya - 1) {

            dx = xp - ixp;
            dy = yp - iyp;
            ixpp1 = ixp + 1;
            ixpm1 = ixp - 1;
            iypp1 = iyp + 1;
            iypm1 = iyp - 1;
            ixpm2 = ixp - 2;
            iypm2 = iyp - 2;
            dxm1 = dx-1.;
            dxdxm1=dx*dxm1;
            fx1=-dxm1*dxdxm1;
            fx4=dx*dxdxm1;
            fx2=1+dx*dx*(dx-2.);
            fx3=dx*(1.-dxdxm1);
              
            dym1 = dy-1.;
            dydym1=dy*dym1;
              
            v1=fx1*array[ixpm2+iypm2*nxa]+fx2*array[ixpm1+iypm2*nxa]+
              fx3*array[ixp+iypm2*nxa]+fx4*array[ixpp1+iypm2*nxa];
            v2=fx1*array[ixpm2+iypm1*nxa]+fx2*array[ixpm1+iypm1*nxa]+
              fx3*array[ixp+iypm1*nxa]+fx4*array[ixpp1+iypm1*nxa];
            v3=fx1*array[ixpm2+iyp*nxa]+fx2*array[ixpm1+iyp*nxa]+
              fx3*array[ixp+iyp*nxa]+fx4*array[ixpp1+iyp*nxa];
            v4=fx1*array[ixpm2+iypp1*nxa]+fx2*array[ixpm1+iypp1*nxa]+
              fx3*array[ixp+iypp1*nxa]+fx4*array[ixpp1+iypp1*nxa];
            bray[ix + ixbase] = -dym1*dydym1*v1+(1.+dy*dy*(dy-2.))*v2+
              dy*(1.-dydym1)*v3 +dy*dydym1*v4;
          }
        }
      }
    }

  } else {

    /* WARPING AFTER LINEAR TRANSFORMATION */
    /* Subtract 1 to work with 0-based coordinates now to match what midas does */
    xco -= 1.;
    yco -= 1.;
    xcen -= 1.;
    ycen -= 1.;
    ox = xco - a11 * xcen - a12 * ycen;
    oy = yco - a21 * xcen - a22 * ycen;

    /* Determine indexes of grid points ending starting and ending grid intervals */
    ixgStart = (int)floor((double)(-xGridStrt / xGridIntrv)) + 1;
    ixgStart = B3DMAX(0, ixgStart);
    ixgEnd = (int)ceil((nxb - 1. - xGridStrt) / xGridIntrv);
    ixgEnd = B3DMIN(nxGrid, ixgEnd);
    iygStart = (int)floor((double)(-yGridStrt / yGridIntrv)) + 1;
    iygStart = B3DMAX(0, iygStart);
    iygEnd = (int)ceil((nyb - 1. - yGridStrt) / yGridIntrv);
    iygEnd = B3DMIN(nyGrid, iygEnd);
    /* fprintf(stderr, "ixgs,e %d %d  y %d %d\n", ixgStart, ixgEnd, iygStart, iygEnd);*/

#pragma omp parallel for num_threads(numThreads) \
  shared(array,bray,nxa,nya,nxb,nyb,scale, dmean,linear, dxGrid, dyGrid, ixgDim, \
         nxGrid, xGridStrt, xGridIntrv, nyGrid, yGridStrt, yGridIntrv, a11,a12,a21, \
         a22,ox, oy, llnxa, ixgStart, ixgEnd, iygStart, iygEnd)                \
  private(j, x, y, buf, i, ix, iy, index, fx, fy, iygrid, ixgrid, indx, indy, xlim, \
          ylim, ixlim, iylim, allIn, xmap, ymap, xbox, ybox, gridfy,    \
          xstep, ystep, dyo,xbase,ybase, xp,yp,dx,dy,v1,v2,v3,v4,    \
          dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,                      \
          ixp,iyp,ixbase, ind,indpnxa,indmnxa,indpnxa2)
    for (iygrid = iygStart; iygrid <= iygEnd; iygrid++) {

      /* Get indexes of grid points in Y on low and high side of block, and y 
         coordinates */
      indy[0] = B3DMAX(0, iygrid - 1);
      indy[1] = B3DMIN(nyGrid - 1, iygrid);
      ylim[0] = yGridStrt + yGridIntrv * (iygrid - 1);
      ylim[1] = yGridStrt + yGridIntrv * iygrid;
      iylim[0] = (int)ceil((double)ylim[0]);
      if (iygrid == iygStart) {
        iylim[0] = 0;
        ylim[0] = B3DMIN(0., ylim[0]);
      }
      iylim[1] = (int)ceil((double)ylim[1]) - 1;
      if (iygrid == iygEnd) {
        iylim[1] = nyb - 1;
        ylim[1] = B3DMAX(nyb - 0.999, ylim[1]);
      }
      /* fprintf(stderr, "Y: %d %f %f  %d %d\n", iygrid, ylim[0], ylim[1], iylim[0],
         iylim[1]); */

      /* Loop on X blocks, get indexes and limiting coordinates in X */
      for (ixgrid = ixgStart; ixgrid <= ixgEnd; ixgrid++) {
        indx[0] = B3DMAX(0, ixgrid - 1);
        indx[1] = B3DMIN(nxGrid - 1, ixgrid);
        xlim[0] = xGridStrt + xGridIntrv * (ixgrid - 1);
        xlim[1] = xGridStrt + xGridIntrv * ixgrid;
        ixlim[0] = (int)ceil((double)xlim[0]);
        if (ixgrid == ixgStart) {
          ixlim[0] = 0;
          xlim[0] = B3DMIN(0., xlim[0]);
        }
        ixlim[1] = (int)ceil((double)xlim[1]) - 1;
        if (ixgrid == ixgEnd) {
          ixlim[1] = nxb - 1;
          xlim[1] = B3DMAX(nxb - 0.999, xlim[1]);
        }
      
        /* Evaluate mapping of each corner point and see if inside */
        allIn = 1;
        /* fprintf(stderr, "block %d %d:", ixgrid, iygrid); */
        for (iy = 0; iy < 2; iy++) {
          for (ix = 0; ix < 2; ix++) {
            index = indx[ix] + ixgDim * indy[iy];
            x = xlim[ix] + dxGrid[index];
            y = ylim[iy] + dyGrid[index];
            /* printf("  %.1f, %.1f", dxGrid[index], dyGrid[index]);*/
            xmap[ix][iy] = x * a11 + y * a12 + ox;
            ymap[ix][iy] = x * a21 + y * a22 + oy;
            /* if (iygrid == 0)
               fprintf(stderr, "iyg %d ix %d iy %d %f %f\n", iygrid, ix, iy, xmap[ix][iy],
               ymap[ix][iy]); */
            if (xmap[ix][iy] < 1. || xmap[ix][iy] >= nxa - 2 || ymap[ix][iy] < 1. || 
                ymap[ix][iy] >= nya - 2)
              allIn = 0;
          }
        }
        /* fprintf(stderr, " %d\n", allIn);
           puts(""); */
        xbox = xlim[1] - xlim[0];
        ybox = ylim[1] - ylim[0];

        /* Loop on lines, set up start coordinate and steps */
        for (j = iylim[0]; j <= iylim[1]; j++) {
          ixbase = ((size_t)j) * nxb;
          gridfy = (j - ylim[0]) / ybox;
          xstep = ((1. - gridfy) * (xmap[1][0] - xmap[0][0]) + 
                   gridfy * (xmap[1][1] - xmap[0][1])) / xbox;
          ystep = ((1. - gridfy) * (ymap[1][0] - ymap[0][0]) + 
                   gridfy * (ymap[1][1] - ymap[0][1])) / xbox;
          x = (1. - gridfy) * xmap[0][0] + gridfy * xmap[0][1] + 
            xstep * (ixlim[0] - xlim[0]);
          y = (1. - gridfy) * ymap[0][0] + gridfy * ymap[0][1] +
            ystep * (ixlim[0] - xlim[0]);
          /* if (iygrid == 0 && ixgrid == 5)
            fprintf(stderr, "gridfy %f  x,y %f %f  steps %f %f\n", gridfy, x, y, xstep,
            ystep); */

          /* Loop across lines in different cases */
          buf = &bray[ixlim[0] + (size_t)j * nxb];
          if (linear) {

            /* Linear or nearest neighbor interpolation */
            if (allIn) {
              if (linear > 0) {
                for (i = 0; i <= ixlim[1] - ixlim[0]; i++){
                  xp = x + i * xstep;
                  yp = y + i * ystep;
                  ixp = (int)xp; 
                  iyp = (int)yp;
                  fx = xp - ixp;
                  fy = yp - iyp;
                  ind = ixp + (iyp * llnxa);
                  *buf++ = ((1. - fy) * ((1. - fx) *array[ind] +
                                         fx * array[ind + 1]) +
                            fy * ((1. - fx) *array[ind + llnxa] +
                                  fx * array[ind + llnxa + 1]));
                }
              } else {
                for (i = 0; i <= ixlim[1] - ixlim[0]; i++){
                  xp = x + i * xstep;
                  yp = y + i * ystep;
                  ixp = (int)(xp + 0.5); 
                  iyp = (int)(yp + 0.5);
                  ind = ixp + (iyp * llnxa);
                  *buf++ = array[ind];
                }
              }
            } else {
              for (i = 0; i <= ixlim[1] - ixlim[0]; i++){
                xp = x + i * xstep;
                yp = y + i * ystep;
                if (linear > 0) {
                  ixp = (int)xp; 
                  iyp = (int)yp;
                  if (ixp >= 0 && ixp < nxa - 1  && iyp >= 0 && iyp < nya - 1) {
                    fx = xp - ixp;
                    fy = yp - iyp;
                    ind = ixp + (iyp * llnxa);
                    *buf++ = ((1. - fy) * ((1. - fx) *array[ind] +
                                           fx * array[ind + 1]) +
                              fy * ((1. - fx) *array[ind + llnxa] +
                                    fx * array[ind + llnxa + 1]));
                  } else
                    *buf++ = dmean;
                } else {
                  ixp = (int)(xp + 0.5); 
                  iyp = (int)(yp + 0.5);
                  if (ixp >= 0 && ixp < nxa  && iyp >= 0 && iyp < nya)
                    *buf++ = array[ixp + (iyp * llnxa)];
                  else
                    *buf++ = dmean;
                }
              }
            }  
          } else {

            /* Cubic */
            for (i = 0; i <= ixlim[1] - ixlim[0]; i++){
              xp = x + i * xstep;
              yp = y + i * ystep;
              ixp = xp;
              iyp = yp;
              if (allIn || (ixp >= 1 && ixp < nxa - 2 && iyp >= 1 && iyp < nya - 2)) {
                
                dx = xp - ixp;
                dy = yp - iyp;
                dxm1 = dx-1.;
                dxdxm1=dx*dxm1;
                fx1=-dxm1*dxdxm1;
                fx4=dx*dxdxm1;
                fx2=1+dx*dx*(dx-2.);
                fx3=dx*(1.-dxdxm1);
                
                dym1 = dy-1.;
                dydym1=dy*dym1;
                ind = ixp + iyp * llnxa;
                indmnxa = ind - llnxa;
                indpnxa = ind + llnxa;
                indpnxa2 = ind + 2 * llnxa;
                v1 = fx1*array[indmnxa-1] + fx2*array[indmnxa] + 
                  fx3*array[indmnxa+1] + fx4*array[indmnxa+2];
                v2 = fx1*array[ind-1] + fx2*array[ind] + 
                  fx3*array[ind+1] + fx4*array[ind+2];
                v3 = fx1*array[indpnxa-1] + fx2*array[indpnxa] + 
                  fx3*array[indpnxa+1] + fx4*array[indpnxa+2];
                v4 = fx1*array[indpnxa2-1] + fx2*array[indpnxa2] + 
                  fx3*array[indpnxa2+1] + fx4*array[indpnxa2+2];
                
                *buf++ = -dym1*dydym1*v1 + (1. + dy*dy*(dy - 2.))*v2 +
                  dy*(1. - dydym1)*v3 +dy*dydym1*v4;
              } else
                *buf++ = dmean;
            }
          }

        }
      }
    }
  }
}

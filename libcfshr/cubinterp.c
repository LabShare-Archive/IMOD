/*
 * cubinterp.c : cubic or linear interpolation of an image
 * Translated from a fortran routine
 * written by David Mastronarde, April 2000.
 * 
 * $Id$
 * Log at end
 */
#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define cubinterpfwrap CUBINTERP
#else
#define cubinterpfwrap cubinterp_
#endif

/*!
 * Applies a linear transformation to an image using cubic or linear 
 * interpolation.  It eliminates all range tests from the inner loop, but 
 * falls back from cubic to quadratic interpolation (which range tests) around
 * the edges of the input image area.  ^
 *   [array] - The input image array  ^
 *   [bray] - The output image array  ^
 *   [nxa],[nya] - The dimensions of [array]  ^
 *   [nxb],[nyb] - The dimensions of [bray]  ^
 *   [amat] - A 2x2 matrix to specify rotation, scaling, and skewing  ^
 *   [xc],[yc] - The coordinates of the center of [array]  ^
 *   [xt],[yt] - The translation to add to the final image. The
 * center of the output array is taken as [nxb] / 2., [nyb] / 2.  ^
 *   [scale] - A multiplicative scale factor for the intensities  ^
 *   [dmean] - Mean intensity of image or other value with which to fill 
 * empty image area  ^
 *   [linear] - Set non-zero to do linear interpolation  ^
 * The coordinate transformation from (Xi, Yi) in the input image to the
 * (Xo, Yo) in the output image is given by:  ^
 *     Xo = a11(Xi - Xc) + a12(Yi - Yc) + nxb/2. + xt  ^
 *     Yo = a21(Xi - Xc) + a22(Yi - Yc) + nyb/2. + yt  ^
 * where Xi is a coordinate running from 0 at the left edge of the first pixel
 * to [nxa] at the right edge of the last pixel in X, and similarly for Y.  ^
 * When calling from C, indices in [amat] are transposed: a11 = amat\[0\]\[0\],
 * a12 = amat\[1\]\[0\], a21 = amat\[0\]\[1\], a22 = amat\[1\]\[1\]  ^
 * To call from Fortran, use arguments of the same type and in the same order.
 * Indices in [amat] are in logical order: a11 = amat(1,1), a12 = amat(1,2), 
 * etc.  ^
 * This routine is now parallelized with OpenMP.  The allowed threads is
 * proportional to the square root of output image area, falling to 1 at about 
 * 32x32.
 */   
void cubinterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
               float amat[2][2], float xc, float yc, float xt, float yt, 
               float scale, float dmean, int linear)
{
  float xcen,ycen,xco,yco,denom,a11,a12,a22,a21,dyo,xbase,ybase;
  float xst,xnd,xlft,xrt,xp,yp,dennew,dx,dy,v2,v4,v6,v8,v5,a,b,c,d;
  float dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3,vmin,vmax;
  int iy,ix,ixp,ixpp1,iyp,iypp1,ixpm1,iypm1,linefb;
  int ixnd,ixst,ixfbst,ixfbnd,iqst,iqnd,ifall,ind,indpnxa,indmnxa,indpnxa2;

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

  /* loop over output image */
  for (iy = 1; iy <= nyb; iy++) {
    dyo = iy - ycen;
    xbase = a12 * dyo +xco - a11 * xcen;
    ybase = a22 * dyo + yco - a21 * xcen;
    xst = 1;
    xnd = nxb;
    linefb = 0;
    if (fabs((double)a11) > 1.e-10) {
      xlft = (2.01 - xbase) / a11;
      xrt = (nxa - 1.01 - xbase) / a11;
      xst = B3DMAX(xst, B3DMIN(xlft, xrt));
      xnd = B3DMIN(xnd, B3DMAX(xlft, xrt));
    } else if (xbase < 2. || xbase >= nxa-1.) {
      xst = nxb;
      xnd = 1;
      if (xbase >= 0.5 || xbase <= nxa+0.5)
        linefb = 1;
    }
    if (fabs((double)a21) > 1.e-10) {
      xlft = (2.01 - ybase) / a21;
      xrt = (nya - 1.01 - ybase) / a21;
      xst = B3DMAX(xst, B3DMIN(xlft, xrt));
      xnd = B3DMIN(xnd, B3DMAX(xlft, xrt));
    } else if (ybase < 2. || ybase >= nya - 1.) {
      xst = nxb;
      xnd = 1;
      if (ybase >= 0.5 || ybase <= nya + 0.5) 
        linefb = 1;
    }
     
    /* truncate the ending value down and the starting value up but do not
       pay any attention to xst bigger than nxb + 1 */
    ixnd = xnd;
    ixst = nxb + 1 - (int)(nxb + 1 - B3DMIN(xst, nxb + 1.));
     
    /* if they're crossed, set them up so fill will do whole line */
    if (ixst > ixnd) {
      ixst = nxb / 2;
      ixnd = ixst - 1;
    }
     
    /* set up fallback region limits depending on whether doing 2 pixels
       or whole line */
    if (linefb == 0) {
      ixfbst = B3DMAX(1, ixst - 2);
      ixfbnd = B3DMIN(nxb, ixnd + 2);
    } else {
      ixfbst = 1;
      ixfbnd = nxb;
    }
     
    /* do fill outside of fallback */
    for (ix = 1; ix <= ixfbst - 1 ; ix++)
      bray[ix + (iy - 1) * nxb - 1] = dmean;
    for (ix = ixfbnd + 1; ix <= nxb; ix++)
      bray[ix + (iy - 1) * nxb - 1] = dmean;
     
    /* do fallback to quadratic with tests */
    iqst = ixfbst;
    iqnd = ixst - 1;
    for (ifall = 1; ifall <= 2; ifall++) {
      if (!linear) {

        /* Do quadratic interpolation */
        for (ix = iqst; ix <= iqnd; ix++) {
          xp = a11 * ix + xbase;
          yp = a21 * ix + ybase;
          ixp = B3DNINT(xp);
          iyp = B3DNINT(yp);
          dennew = dmean;
          if (ixp  >=  1 && ixp  <=  nxa && iyp  >=  1 && iyp  <=  nya) {
            dx = xp - ixp;
            dy = yp - iyp;
            ixpp1 = ixp + 1;
            ixpm1 = ixp - 1;
            iypp1 = iyp + 1;
            iypm1 = iyp - 1;
            if (ixpm1  <  1) 
              ixpm1 = 1;
            if (iypm1  <  1)
              iypm1 = 1;
            if (ixpp1 > nxa)
              ixpp1 = nxa;
            if (iypp1 > nya)
              iypp1 = nya;

      /* set up terms for quadratic interpolation */
            v2 = array[ixp + (iypm1 - 1) * nxa - 1];
            v4 = array[ixpm1 + (iyp - 1) * nxa - 1];
            v5 = array[ixp + (iyp - 1) * nxa - 1];
            v6 = array[ixpp1 + (iyp - 1) * nxa - 1];
            v8 = array[ixp + (iypp1 - 1) * nxa - 1];
            vmax = B3DMAX(v2,v4);
            vmax = B3DMAX(vmax,v5);
            vmax = B3DMAX(vmax,v6);
            vmax = B3DMAX(vmax,v8);
            vmin = B3DMIN(v2,v4);
            vmin = B3DMIN(vmin,v5);
            vmin = B3DMIN(vmin,v6);
            vmin = B3DMIN(vmin,v8);
     
            a = (v6 + v4) * .5f - v5;
            b = (v8 + v2) * .5f - v5;
            c = (v6 - v4) * .5f;
            d = (v8 - v2) * .5f;
            
            dennew = scale*(a*dx*dx + b*dy*dy + c*dx + d*dy + v5);
            if (dennew>vmax) 
              dennew = vmax;
            if (dennew < vmin)
              dennew = vmin;
          }
          bray[ix + (iy - 1) * nxb - 1] = dennew;
        }    

      } else {
  
  /* fallback to linear */
        for (ix = iqst; ix <= iqnd; ix++) {
          xp = a11 * ix + xbase;
          yp = a21 * ix + ybase;
          ixp = xp;
          iyp = yp;
          dennew = dmean;
          if (ixp  >=  1 && ixp  <  nxa && iyp  >=  1 && iyp  <  nya) {
            dx = xp - ixp;
            dy = yp - iyp;
            ind = ixp + (iyp - 1) * nxa - 1;
            dennew = (1. - dy) * ((1. - dx) * array[ind] + dx * array[ind+1]) +
              dy * ((1. - dx) * array[ind+nxa] + dx * array[ind+nxa+1]);
            
          }
          bray[ix +(iy - 1) * nxb - 1] = dennew;
     }
      }
      iqst = ixnd + 1;
      iqnd = ixfbnd;
    }

    if (!linear) {

      /* Do cubic interpolation on the central region */
      for (ix = ixst; ix <= ixnd; ix++) {
        xp = a11 * ix + xbase;
        yp = a21 * ix + ybase;
        ixp = xp;
        iyp = yp;
        dx = xp - ixp;
        dy = yp - iyp;
       
        dxm1 = dx - 1.;
        dxdxm1 = dx * dxm1;
        fx1 = -dxm1 * dxdxm1;
        fx4 = dx * dxdxm1;
        fx2 = 1 + dx * dx * (dx - 2.);
        fx3 = dx * (1. - dxdxm1);
       
        dym1 = dy - 1.;
        dydym1 = dy * dym1;
        ind = ixp + (iyp - 1) * nxa - 1;
        indmnxa = ind - nxa;
        indpnxa = ind + nxa;
        indpnxa2 = ind + 2 * nxa;
        v1 = fx1*array[indmnxa-1] + fx2*array[indmnxa] + 
          fx3*array[indmnxa+1] + fx4*array[indmnxa+2];
        v2 = fx1*array[ind-1] + fx2*array[ind] + 
          fx3*array[ind+1] + fx4*array[ind+2];
        v3 = fx1*array[indpnxa-1] + fx2*array[indpnxa] + 
          fx3*array[indpnxa+1] + fx4*array[indpnxa+2];
        v4 = fx1*array[indpnxa2-1] + fx2*array[indpnxa2] + 
          fx3*array[indpnxa2+1] + fx4*array[indpnxa2+2];
       
        bray[ix+(iy-1)*nxb-1] = -dym1*dydym1*v1 + (1. + dy*dy*(dy - 2.))*v2 +
          dy*(1. - dydym1)*v3 +dy*dydym1*v4;
  
      }
      
    } else {
       
      /* do linear interpolation */
      for (ix = ixst; ix <= ixnd; ix++) {
        xp = a11 * ix + xbase;
        yp = a21 * ix + ybase;
        ixp = xp;
        iyp = yp;
        dx = xp - ixp;
        dy = yp - iyp;
        ind = ixp + (iyp - 1) * nxa - 1;
        bray[ix+(iy-1)*nxb-1] = (1. - dy) *
          ((1. - dx) * array[ind] + dx * array[ind+1]) +
          dy * ((1. - dx) * array[ind+nxa] + dx * array[ind+nxa+1]);
      }
    }
  }
}

void cubinterpfwrap(float *array, float *bray, int *nxa, int *nya, int *nxb,
                    int *nyb, float amat[2][2], float *xc, float *yc,
                    float *xt, float *yt, float *scale, float *dmean, 
                    int *linear)
{
  cubinterp(array, bray, *nxa, *nya, *nxb, *nyb, amat, *xc, *yc, *xt, *yt, 
            *scale, *dmean, *linear);
}

/*

$Log$
Revision 1.3  2007/10/12 21:00:01  mast
Made it rotate around center of image when input center is specified as nx/2.

Revision 1.2  2007/09/20 15:42:32  mast
Documentation fixes

Revision 1.1  2007/09/20 02:42:37  mast
Added C translation to new library

Revision 3.3  2006/03/27 19:50:47  mast
Fixed problem with starting X value too large for integer truncations

Revision 3.2  2003/03/14 02:08:05  mast
Added a linear interpolation option and implemented implicit none

Revision 3.1  2002/07/31 01:09:22  mast
Changed margins for computing xlft and xrt from 2.0 and 1.001 to
2.01 and 1.01 because of crash on SGI, probably due to rounding
error

*/

/*
 * odfft.c - 1D FFTs
 *
 * Original author: David Agard.
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 *
 * Original header comments:
 *
 *  Performs NY 1-D FFT'S in place. Uses Lynn Ten Eyck's FFT routines.
 *  Thus only restrictions are NX MUST be EVEN & max prime factor =19.
 *
 *  Origin is at first point in each strip
 *  A Normalization factor is applied during ALL transformations
 *
 *  For Real/complex or Complex/real:
 *      Only the unique portion of the Transform is written
 *      DIMENSION ARRAY(NX+2,NY)
 *
 *  For Complex/complex
 *      The entire Transform is written
 *      COMPLEX ARRAY((NX+2)/2,NY)
 *
 *
 *  IDIR =  0 Foward   (Real --> Complex)     exp(+2PIirs)
 *  IDIR =  1 Reverse  (Complex --> Real)     exp(-2PIirs)
 *  IDIR = -1 Foward   (Complex --> Complex)  exp(+2PIirs)
 *  IDIR = -2 Reverse  (Complex --> Complex)  exp(-2PIirs)
 *  IDIR = -3 Reverse  (Real --> Complex)     exp(-2PIirs)
 *
 *
 *  Version 1.00    Apr 19 1982     DAA
 *  Version 1.01    Nov 23 1982     DAA
 *  Version 1.02    Jul 28 1983     DAA
 *  Version 1.03    Jan 18 1984     DAA
 *
 * However, when IMOD was switched  FFTW for most platforms, the conjugate was moved from
 * the forward to the inverse real transform and removed from the forward complex 
 * transform to be consistent with FFTW; the inverse complex requires a conjugate both
 * before and after to be a true inverse instead of the conjugate of 1; this change
 * means that a second conjugate is required when doing todfft for 3D inverse.
 */

/*  $Id$ */

#include "cfft.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*!
 * Performs [nyp] 1-D FFT'S in place.  For real to complex or complex to real transforms,
 * the data are organized as [nyp] lines of data of length [nxp] in the real space image,
 * which must be contained in a float array whose X dimension is [nxp] + 2.  For complex
 * to complex transforms, [nxp] is the number of complex elements in X, and the data must
 * be in an array whose X dimension is 2 * [nxp]. The origin of the transform
 * is at the first point on each line.  The direction of the transform is 
 * determined by [idirp]: ^
 *     0 Forward  (Real --> Complex)     exp(-2PIirs) ^
 *     1 Reverse  (Complex --> Real)     exp(+2PIirs) ^
 *    -1 Forward  (Complex --> Complex)  exp(-2PIirs) ^
 *    -2 Reverse  (Complex --> Complex)  exp(+2PIirs) ^
 * For Real/complex or Complex/real, only the unique portion of the transform
 * is written to the array (positive frequencies).  For Complex/complex, the
 * entire transform is written.  ^
 * Can be called from either C or Fortran by this name.
 */
void odfft(float *array, int *nxp, int *nyp, int *idirp)
{
  int idim[6];
  int nxo2, nx2, nxt, nxt1, j, nxp2, nxm1, index, iy;
  float onevol;
  int nx = *nxp;
  int ny = *nyp;
  int idir = *idirp;

  nxo2 = nx/2;
  if (!(2*nxo2 == nx || idir < 0)) {
    printf("ERROR: odfft - nx= %d must be even with IMOD FFT routines\n", nx);
    exit(1);
  }
  onevol = sqrt((double)(1.0/nx));
  switch (idir) {
    /*
      c********      complex transforms come here       ***********
    */


    /*  set up for first dimension of transform */
  case -2:
  case -1:
    nx2 = nx*2;
    nxt = nx2*ny;
    nxt1 = nxt - 1;
    idim[1] = nxt;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nx2;

    /* DNM: switch conjugate between forward and reverse and move scaling and conjugate
       to before transform */
    /*   take complex conjugate to do reverse & scale by 1/volume */
    if (idir == -2) {
      for (j = 0; j < nxt1; j += 2) {
        array[j] = array[j]*onevol;
        array[j+1] = -array[j+1]*onevol;
      }
    } 
    cmplft(array, &array[1], nx, idim);

    if (idir == -2) {

      /* True inverse requires another conjugate here */
      for (j = 0; j < nxt1; j += 2) {
        array[j+1] = -array[j+1];
      }
    } else {

      /*   scale by 1/volume */
      for (j = 0; j < nxt; j++)
        array[j] = array[j]*onevol;
    }

    return;

    /*
     *      foward real transform comes here       ******************
     */

    /*  set up for transform */

  case 0:
    nxp2 = nx + 2;
    nxt = nxp2*ny;
    idim[1] = nxt;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nxp2;

    realft(array, &array[1], nxo2, idim);

    /*    normalize data */
    /* DNM: do not take conjugate here to match FFTW, take conjugate before inverse */
    for (j = 0; j < nxt; j++)
      array[j] = array[j]*onevol;

    return;

    /*
     *      inverse hermite transform comes here       ****************/

    /*  set up for transform */

  case 1:
    nxp2 = nx + 2;
    nxt = nxp2*ny;
    nxt1 = nxt - 1;
    nxm1 = nx - 1;
    idim[1] = nxt;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nxp2;

    
    /* normalize data and take complex conjugate
     * before reverse transform */
    for (j = 0; j < nxt1; j += 2) {
      array[j] = array[j]*onevol;
      array[j + 1] = -array[j + 1]*onevol;
    }

    /*  change data storage mode */
    index = 1;
    for (iy = 0; iy < ny; iy++) {
      array[index] = array[nxm1 + index];
      index = index + nxp2;
    }

    hermft(array, &array[1], nxo2, idim);

    return;

  default:
    printf("ERROR: odfft - idir = %d is an illegal option\n", idir);
    exit(1);
  }
}

/*!
 * Function to call @odfft from C with arguments passed by value
 */
void odfftc(float *array, int nx, int ny, int idir)
{
  odfft(array, &nx, &ny, &idir);
}

/*!
 * Returns 1 if FFTW is being used, 0 if IMOD FFT library is being used
 */
int usingFFTW(void)
{
  return 0;
}

/*!
 * Returns the highest allowed prime factor, 19, for IMOD FFTs, or the highest desirable
 * prime factor, 13, for FFTW.
 */
int niceFFTlimit(void)
{
  return 19;
}

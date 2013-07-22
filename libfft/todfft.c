/*
 * todfft.c - 2D FFTs
 *
 * Original author: David Agard.
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 *
 * Original header comments:
 *
 *   TWO-DIMENSIONAL FOURIER TRANSFORM SUBROUTINE FOR IMAGE
 *   PROCESSING. DOES BOTH FORWARD & INVERSE TRANSFORMS
 *   USES LYNN TENEYCK'S MIXED-RADIX ROUTINES
 *   THUS THE ONLY RESTRICTION IS THAT THE IMAGE SIZE BE
 *   AN EVEN NUMBER AND HAVE NO PRIME FACTORS LARGER THAN 19!!
 *
 *   IDIR =  0   FORWARD  TRANSFORM :  exp(+2PIirs)
 *   IDIR =  1   INVERSE TRANSFORM  :  exp(-2PIirs)
 *   IDIR = -1   INVERSE TRANSFORM BUT NO COMPLEX CONJUGATE
 *
 *   DATA SET UP AS NY ROWS OF NX NUMBERS
 *   NOTE NX,NY ALWAYS REFER TO THE REAL-SPACE IMAGE SIZE
 *
 *   NX,NY IMAGE IS TRANSFORMED IN-PLACE INTO NY STRIPS OF
 *   NX/2 + 1 COMPLEX FOURIER COEFFICIENTS
 *   THE ORIGIN IS LOCATED AT THE FIRST POINT!!!
 *
 *   ARRAY MUST BE DIMENSIONED TO ALLOW FOR EXTRA STRIP OF COMPLEX
 *   NUMBERS ON OUTPUT.
 *   THUS FOR A 300X400 TRANSFORM, ARRAY MUST BE DIMENSIONED:
 *   REAL ARRAY(302,400)
 *
 *   A NORMALIZATION FACTOR IS APPLIED DURING BOTH  TRANSFORMATIONS
 *
 *   VERSION 1.00    OCT 11 1981     DAA
 *   VERSION 1.02    APR 19 1982     DAA
 *   VERSION 1.03    NOV 23 1982     DAA
 *
 * However, when IMOD was switched  FFTW for most platforms, the conjugate was removed
 * from the forward and applied before the inverse to make this 2D FFT match FFTW.
 * Also removed the -1 option to reduce confusion.
 */

#include "cfft.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*!
 * Performs a two-dimensional FFT in place in [array].  The data are organized
 * as [nyp] rows of [nxp] values in the real space image, which must be 
 * contained in a float array whose X dimension is [nxp] + 2.  The direction 
 * of the transform is determined by [idirp]: ^
 *     0        forward  transform :  exp(-2PIirs) ^
 *     1 or -1  inverse transform  :  exp(+2PIirs) ^
 * The origin of the transform is at the first point.  The Y coordinate of the
 * transform progresses from Y = 0 on the first line, to Y = ([nyp] - 1 ) / 2 on
 * the middle line, then from -[nyp] / 2 on the next line up, to -1 on the 
 * last line.  ^
 * Can be called from either C or Fortran by this name.
 */
void todfft(float *array, int *nxp, int *nyp, int *idirp)
{
  int idim[6];   /* Make it 6 so that indexes 1 to 5 work */
  int nxo2, nxt, nxt1, j, nxp2, nxm1, index, iy;
  float onevol;
  int nx = *nxp;
  int ny = *nyp;
  int idir = *idirp;

  nxo2 = nx/2;
  if (2*nxo2 != nx) {
    printf("ERROR: todfft - nx= %d must be even with IMOD FFT routines\n", nx);
    exit(1);
  }
  nxp2 = nx + 2;
  nxt = nxp2*ny;
  nxt1 = nxt - 1;
  onevol = sqrt((double)(1.0/(nx*ny)));
  if (idir == 0) {
    /*
      c********      forward transforms come here       ******************
      c
      c
      c  set up for first dimension of transform
    */
    idim[1] = nxp2*ny;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nxp2;

    realft(array, &(array[1]), nxo2, idim);
    /*
      c  set up for second dimension of transform
    */
    idim[2] = nxp2;
    idim[4] = idim[2];
    idim[5] = 2;

    cmplft(array, &(array[1]), ny, idim);
    /* Scale.  DNM: Eliminated conjugate for consistency with FFTW */
    for (j = 0; j < nxt1; j += 2) {
      array[j] = array[j]*onevol;
      array[j+1] = array[j+1]*onevol;
    }
  
    return;
  }
  /*
    c**********        inverse transform     *******************
    c
    c
    c  set up for first dimension of transform
  */
  nxm1 = nx - 1;
  idim[1] = nxp2*ny;
  idim[2] = nxp2;
  idim[3] = idim[1];
  idim[4] = idim[2];
  idim[5] = 2;
  /*
    c   take complex conjugate to do inverse & scale by 1/volume
    * DNM: do this here since it is no longer done on forward
  */
  for (j = 0; j < nxt1; j += 2) {
    array[j] = array[j]*onevol;
    array[j+1] = -array[j+1]*onevol;
  }

  cmplft(array, &(array[1]), ny, idim);
  /*
    c  set up for second dimension of transform
  */
  idim[2] = 2;
  idim[4] = idim[1];
  idim[5] = nxp2;
  /*
    c    change data storage mode complex conjugate done by hermft
  */
  index = 1;
  for (iy = 0; iy < ny; iy++) {
    array[index] = array[nxm1 + index];
    index = index + nxp2;
  }

  hermft(array, &(array[1]), nxo2, idim);

  return;
}

/*!
 * Function to call @todfft from C with arguments passed by value
 */
void todfftc(float *array, int nx, int ny, int idir)
{
  todfft(array, &nx, &ny, &idir);
}

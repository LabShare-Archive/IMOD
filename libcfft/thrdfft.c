/*
 * Three-dimensional FFT routine, does forward and inverse transforms
 * idir = 0   forward transform
 * idir = 1   inverse transform (for sake of completeness)
 * idir = -1  inverse transform with no complex conjugate in 2D FFTs
 *    The latter is apparently needed for proper phases
 *
 * Data are organized as nx rows by ny columns by nz slices in the
 * real-space image, in an array dimensioned to nx + 2 in X to allow for
 * extra complex numbers in X dimension.
 * nx,ny,nz image is transformed in place into 
 * nx/2 + 1, ny, nz complex numbers
 *
 * brray is for working storage and must be dimensioned to at least nx + 2 * nz
 */

/*  $Author$
    
$Date$

$Revision$

$Log$
*/

#include "cfft.h"
#include <math.h>
void thrdfft(float *array, float *brray, int *nxp, int *nyp, int *nzp,
             int *idirp)
{
  int nxo2, i, j, k, nxp2, ibase;
  int nx = *nxp;
  int ny = *nyp;
  int nz = *nzp;
  int idir = *idirp;
  int oddir = idir ? -2 : -1;

  nxp2 = nx + 2;
  nxo2 = nxp2 / 2;

  /* Forward transform: first do 2D fft of each plane */
  if (!idir)
    for (j = 0; j < nz; j++)
      todfft(&array[nxp2 * ny * j], nxp, nyp, idirp);

  /* Now do one-D transforms in Z */
  for (j = 0; j < ny; j++) {
    
    /* Copy plane, do one-D complex FFTs, and copy plane back */
    for (k = 0; k < nz; k++) {
      ibase = j * nxp2 + k * ny * nxp2;
      for (i = 0; i < nxo2; i++) {
        brray[2 * (k + i * nz)] = array[ibase + 2 * i];
        brray[2 * (k + i * nz) + 1] = array[ibase + 2 * i + 1];
      }
    }

    odfft(brray, nzp, &nxo2, &oddir);

    for (k = 0; k < nz; k++) {
      ibase = j * nxp2 + k * ny * nxp2;
      for (i = 0; i < nxo2; i++) {
        array[ibase + 2 * i] = brray[2 * (k + i * nz)];
        array[ibase + 2 * i + 1] = brray[2 * (k + i * nz) + 1];
      }
    }
  }

  /* Reverse transform: do reverse 2D FFT's */
  if (idir)
    for (j = 0; j < nz; j++)
      todfft(&array[nxp2 * ny * j], nxp, nyp, idirp);
  return;
}


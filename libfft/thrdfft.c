/*
 * thrdfft.c - 3D FFTs
 *
 * Original Author: David Mastronarde
 * Translated to C: David Mastronarde
 * This code is released under the General Public License.
 *
 * $Id$
 */

#include "cfft.h"
#include <math.h>

/*!
 * Performs a three-dimensional FFT in place on data in [array].
 * Data are organized as [nzp] slices each consisting of [nyp] rows of [nxp]
 * values in the real-space image, which must be contained in a float array
 * whose X dimension is [nxp] + 2.  [brray] is for working storage and must be
 * dimensioned to at least ([nxp] + 2) * [nzp] when using IMOD FFT routines but is not
 * needed with FFTW.  The direction of the transform is determined by [idirp]: ^
 *     0         forward transform ^
 *     1 or -1   inverse transform ^
 * The origin of the transform is at the first point.  The Y coordinate of the
 * transform progresses from Y = 0 on the first line, to Y = ([nyp] - 1) / 2 on
 * the middle line, then from -[nyp] / 2 on the next line up, to -1 on the 
 * last line.  Similarly the Z coordinate progresses from Z = 0 on the first
 * slice to Z = ([nzp] - 1) / 2, then from -[nzp] / 2 to -1.  ^
 * Can be called from either C or Fortran by this name.
 */
void thrdfft(float *array, float *brray, int *nxp, int *nyp, int *nzp,
             int *idirp)
{
  int nxo2, i, j, k, nxp2, ibase;
  int nx = *nxp;
  int ny = *nyp;
  int nz = *nzp;
  int idir = *idirp;
  int oddir = idir ? -2 : -1;
  int back = 1;

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
      todfft(&array[nxp2 * ny * j], nxp, nyp, &back);
  return;
}

/*!
 * Function to call @thrdfft from C with arguments passed by value
 */
void thrdfftc(float *array, float *brray, int nx, int ny, int nz, int idir)
{
  thrdfft(array, brray, &nx, &ny, &nz, &idir);
}


/*
 * thrdfft.c - 3D FFTs
 *
 * Original Author: David Mastronarde
 * Translated to C: David 
 * This code is released under the General Public License.
 *
 * $Id$
 * Log at end of file
 */

#include "cfft.h"
#include <math.h>

/*!
 * Performs a three-dimensional FFT in place on data in [array].
 * Data are organized as [nzp] slices each consisting of [nyp] rows of [nxp]
 * values in the real-space image, which must be contained in a float array
 * whose X dimension is [nxp] + 2.  [brray] is for working storage and must be
 * dimensioned to at least ([nxp] + 2) * [nzp].  The direction of the 
 * transform is determined by [idirp]: ^
 *     0   forward transform ^
 *     1   inverse transform (for sake of completeness) ^
 *    -1   inverse transform with no complex conjugate in 2D FFTs ^
 *         The latter is needed for proper phases and should be used. ^
 * The origin of the transform is at the first point.  The Y coordinate of the
 * transform progresses from Y = 0 on the first line, to Y = [nyp] / 2 - 1 on
 * the middle line, then from -[nyp] / 2 on the next line up, to -1 on the 
 * last line.  Similarly the Z coordinate progresses from Z = 0 on the first
 * slice to Z = [nzp] / 2 - 1, then from -[nzp] / 2 to -1.  ^
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

/*
$Log$
Revision 1.3  2007/10/12 18:33:01  mast
Fix documentation to describe X dimension of array

Revision 1.2  2006/11/03 17:28:38  mast
Added documentation, author and license statements

Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/

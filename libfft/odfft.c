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
 */
/*  $Id$

$Log$
Revision 1.6  2007/10/12 19:45:17  mast
Fix documentation to describe X dimension of array

Revision 1.4  2006/11/03 17:28:38  mast
Added documentation, author and license statements

Revision 1.3  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 1.2  2004/10/25 16:37:13  mast
Fixed test for even-ness of size

Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/

#include "cfft.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*!
 * Performs [nyp] 1-D FFT'S in place.  The data are organized as [nyp] lines of
 * data of length [nxp] in the real space image, which must be contained in a
 * float array whose X dimension is [nxp] + 2.  The origin of the transform
 * is at the first point on each line.  The direction of the transform is 
 * determined by [idirp]: ^
 *     0 Forward  (Real --> Complex)     exp(+2PIirs) ^
 *     1 Reverse  (Complex --> Real)     exp(-2PIirs) ^
 *    -1 Forward  (Complex --> Complex)  exp(+2PIirs) ^
 *    -2 Reverse  (Complex --> Complex)  exp(-2PIirs) ^
 *    -3 Reverse  (Real --> Complex)     exp(-2PIirs) ^
 * For Real/complex or Complex/real, only the unique portion of the transform
 * is written to the array  (positive frequencies).  For Complex/complex, the
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
    printf("ERROR: odfft - nx= %d must be even\n", nx);
    exit(1);
  }
  onevol = sqrt((double)(1.0/nx));
  /*        goto (30,10,10,30,40) idir + 4 */
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

    cmplft(array, &array[1], nx, idim);

    if (idir != -1) {

      /*   scale by 1/volume */

      /*        do 100 j = 1,nxt */
      for (j = 0; j < nxt; j++)
        array[j] = array[j]*onevol;
      return;
    }

    /*   take complex conjugate to do foward & scale by 1/volume */

    /*      do 150 j = 1,nxt1,2 */
    for (j = 0; j < nxt1; j += 2) {
      array[j] = array[j]*onevol;
      array[j+1] = -array[j+1]*onevol;
    }
    return;

    /*
     *      foward real transform comes here       ******************
     */

    /*  set up for transform */

  case -3:
  case 0:
    nxp2 = nx + 2;
    nxt = nxp2*ny;
    nxt1 = nxt - 1;
    idim[1] = nxt;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nxp2;

    realft(array, &array[1], nxo2, idim);

    if (idir != -3) {

      /*    normalize data and take complex conjugate for 
       * true foward transform */

      /*        do 200 j = 1,nxt1,2 */
      for (j = 0; j < nxt1; j += 2) {
        array[j] = array[j]*onevol;
        array[j + 1] = -array[j + 1]*onevol;
      }
      return;
    }
    /*    normalize data */

    /*35      do 250 j = 1,nxt */
    for (j = 0; j < nxt; j++)
      array[j] = array[j]*onevol;

    return;

    /*
     *      inverse hermite transform comes here       ****************/

    /*  set up for transform */

  case 1:
    nxp2 = nx + 2;
    nxt = nxp2*ny;
    nxm1 = nx - 1;
    idim[1] = nxt;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nxp2;

    /*   normalize data */

    /*        do 300 j = 1,nxt */
    for (j = 0; j < nxt; j++)
      array[j] = onevol*array[j];

    /*  change data storage mode */

    index = 1;
    /*        do 350 iy = 1,ny */
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

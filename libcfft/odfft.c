/*
  C*ODFFT.FOR*******************************************************
  C
  C   Performs NY 1-D FFT'S in place. Uses Lynn Ten Eyck's FFT routines.
  C   Thus only restrictions are NX MUST be EVEN & max prime factor =19.
  C
  C   Origin is at first point in each strip
  C   A Normalization factor is applied during ALL transformations
  C
  C   For Real/complex or Complex/real:
  C       Only the unique portion of the Transform is written
  C       DIMENSION ARRAY(NX+2,NY)
  C
  C   For Complex/complex
  C       The entire Transform is written
  C       COMPLEX ARRAY(NX,NY)
  C
  C
  C   IDIR =  0 Foward   (Real --> Complex)     exp(+2PIirs)
  C   IDIR =  1 Reverse  (Complex --> Real)     exp(-2PIirs)
  C   IDIR = -1 Foward   (Complex --> Complex)  exp(+2PIirs)
  C   IDIR = -2 Reverse  (Complex --> Complex)  exp(-2PIirs)
  C   IDIR = -3 Reverse  (Real --> Complex)     exp(-2PIirs)
  C
  C
  C   Version 1.00    Apr 19 1982     DAA
  C   Version 1.01    Nov 23 1982     DAA
  C   Version 1.02    Jul 28 1983     DAA
  C   Version 1.03    Jan 18 1984     DAA
  C
*/
/*  $Author$
    
$Date$

$Revision$

$Log$
Revision 1.2  2004/10/25 16:37:13  mast
Fixed test for even-ness of size

Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/

#include "cfft.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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
    printf("ERROR: odfft - nx= %d must be even!!!\n", nx);
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

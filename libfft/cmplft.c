/*  cmplft.c -   complex finite discrete fourier transform
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 */

/*
C     COMPLEX FINITE DISCRETE FOURIER TRANSFORM
C     TRANSFORMS ONE DIMENSION OF MULTI-DIMENSIONAL DATA
C     MODIFIED BY L. F. TEN EYCK FROM A ONE-DIMENSIONAL VERSION WRITTEN
C     BY G. T. SANDE, 1969.
C
C     THIS PROGRAM CALCULATES THE TRANSFORM
C               (X(T) + I*Y(T))*(COS(2*PI*T/N) - I*SIN(2*PI*T/N))
C
C     INDEXING -- THE ARRANGEMENT OF THE MULTI-DIMENSIONAL DATA IS
C     SPECIFIED BY THE INTEGER ARRAY D, THE VALUES OF WHICH ARE USED AS
C     CONTROL PARAMETERS IN DO LOOPS.  WHEN IT IS DESIRED TO COVER ALL
C     ELEMENTS OF THE DATA FOR WHICH THE SUBSCRIPT BEING TRANSFORMED HAS
C     THE VALUE I0, THE FOLLOWING IS USED.
C
C               I1 = (I0 - 1)*D(2) + 1
C               DO 100 I2 = I1, D(1), D(3)
C               I3 = I2 + D(4) - 1
C               DO 100 I = I2, I3, D(5)
C                  .
C                  .
C           100 CONTINUE
C
C     WITH THIS INDEXING IT IS POSSIBLE TO USE A NUMBER OF ARRANGEMENTS
C     OF THE DATA, INCLUDING NORMAL FORTRAN COMPLEX NUMBERS (D(5) = 2)
C     OR SEPARATE STORAGE OF REAL AND IMAGINARY PARTS.
*/
/*  $Id$

$Log$
Revision 1.2  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/

#include "cfft.h"
#include <stdlib.h>
#include <stdio.h>

void cmplft (float *x, float *y, int n, int *d)
{
     int error;
     int pmax, psym, twogrp;
     int factor[16], sym[16], unsym[16];
     /*
       c     pmax is the largest prime factor that will be tolerated by this
       c     program.
       c     twogrp is the largest power of two that is treated as a special
       c     case.
     */
     pmax = 19;
     twogrp = 8;

     if (n <= 1)
	  return;
     srfp (n, pmax, twogrp, factor, sym, &psym, unsym, &error);

     if (error) {
	  printf ("invalid number of points for cmplft.  n = %d\n", n);
	  exit(1);
     }
     mdftkd (n, factor, d, x, y);
     diprp (n, sym, psym, unsym, d, x, y);

     return;
}

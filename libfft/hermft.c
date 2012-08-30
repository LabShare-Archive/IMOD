/*  hermft.c -   hermitian symmetric fourier transform
 * Original author: Lynn Ten Eyck
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 */

/*
C     HERMITIAN SYMMETRIC FOURIER TRANSFORM
C
C     GIVEN THE UNIQUE TERMS OF A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH
C     2N THIS SUBROUTINE CALCULATES THE 2N REAL NUMBERS WHICH ARE ITS
C     FOURIER TRANSFORM.  THE EVEN NUMBERED ELEMENTS OF THE TRANSFORM
C     (0, 2, 4, . . ., 2N-2) ARE RETURNED IN X AND THE ODD NUMBERED
C     ELEMENTS (1, 3, 5, . . ., 2N-1) IN Y.
C
C     A FINITE HERMITIAN SEQUENCE OF LENGTH 2N CONTAINS N + 1 UNIQUE
C     REAL NUMBERS AND N - 1 UNIQUE IMAGINARY NUMBERS.  FOR CONVENIENCE
C     THE REAL VALUE FOR X(N) IS STORED AT Y(0).
*/
/*  $Id$

$Log$
Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/

#include "cfft.h"
#include <math.h>

void hermft(float *x, float *y, int n, int *dim)
{
     float a, b, c, d, e, f, co, si, two_n, twopi;
     double angle ;
     int nt, d2, d3, d4, d5;
     int i, j, k, nover2, i0, i1, i2;
     int k1;

     twopi = 6.2831853;
     two_n = 2*n;

     nt = dim[1];
     d2 = dim[2];
     d3 = dim[3];
     d4 = dim[4] - 1;
     d5 = dim[5];

     /*      do 100 i0 = 1, nt, d3 */
     for (i0 = 1; i0 <= nt; i0 += d3) {
	  i1 = i0 + d4;
	  /*      do 100 i = i0, i1, d5 */
	  for (i = i0 - 1; i < i1; i+= d5) {
	       a = x[i];
	       b = y[i];
	       x[i] = a + b;
	       y[i] = a - b;
	  }
     }

     nover2 = n/2 + 1;
     if (nover2 < 2) 
	  return;
     /*      do 400 i0 = 2, nover2 */
     for (i0 = 2; i0 <= nover2; i0++) {
	  angle = twopi*(i0-1)/two_n;
	  co = cos(angle);
	  si = sin(angle);
	  k = (n + 2 - 2*i0)*d2;
	  k1 = (i0 - 1)*d2 + 1;
	  /*      do 300 i1 = k1, nt, d3 */
	  for (i1 = k1; i1 <= nt; i1 += d3) {
	       i2 = i1 + d4;
	       /*      do 200 i = i1, i2, d5 */
	       for (i = i1 - 1; i < i2; i += d5) {
		    j = i + k;
		    a = x[i] + x[j];
		    b = x[i] - x[j];
		    c = y[i] + y[j];
		    d = y[i] - y[j];
		    e = b*co + c*si;
		    f = b*si - c*co;
		    x[i] = a + f;
		    x[j] = a - f;
		    y[i] = e + d;
		    y[j] = e - d;
	       }
	  }
     }
     cmplft (x, y, n, dim);
     return;
}

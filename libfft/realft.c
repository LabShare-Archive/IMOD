/*  realft.c -   real fourier transform
 * Original author: Lynn Ten Eyck
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 */

/*
C     REAL FOURIER TRANSFORM
C
C     GIVEN A REAL SEQUENCE OF LENGTH 2N THIS SUBROUTINE CALCULATES THE
C     UNIQUE PART OF THE FOURIER TRANSFORM.  THE FOURIER TRANSFORM HAS
C     N + 1 UNIQUE REAL PARTS AND N - 1 UNIQUE IMAGINARY PARTS.  SINCE
C     THE REAL PART AT X(N) IS FREQUENTLY OF INTEREST, THIS SUBROUTINE
C     STORES IT AT X(N) RATHER THAN IN Y(0).  THEREFORE X AND Y MUST BE
C     OF LENGTH N + 1 INSTEAD OF N.  NOTE THAT THIS STORAGE ARRANGEMENT
C     IS DIFFERENT FROM THAT EMPLOYED BY THE HERMITIAN FOURIER TRANSFORM
C     SUBROUTINE.
C
C     FOR CONVENIENCE THE DATA IS PRESENTED IN TWO PARTS, THE FIRST
C     CONTAINING THE EVEN NUMBERED REAL TERMS AND THE SECOND CONTAINING
C     THE ODD NUMBERED TERMS (NUMBERING STARTING AT 0).  ON RETURN THE
C     REAL PART OF THE TRANSFORM REPLACES THE EVEN TERMS AND THE
C     IMAGINARY PART OF THE TRANSFORM REPLACES THE ODD TERMS.
*/
/*  $Id$

$Log$
Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/

#include "cfft.h"
#include <math.h>

void realft (float *even, float *odd, int n, int *dim)
{
     float a, b, c, d, e, f, co, si, twopi, two_n;
     double angle;
     int nt, d2, d3, d4, d5;
     int i, j, k, l, nover2, i0, i1, i2;

     twopi = 6.2831853;
     two_n = 2*n;

     cmplft (even, odd, n, dim);

     nt = dim[1];
     d2 = dim[2];
     d3 = dim[3];
     d4 = dim[4] - 1;
     d5 = dim[5];
     nover2 = n/2 + 1;

     if (nover2 >= 2) {
	  /*     do 300 i = 2, nover2 */
	  for (i = 2; i <= nover2; i++) {
	       angle = twopi*(i-1)/two_n;
	       co = cos(angle);
	       si = sin(angle);
	       i0 = (i - 1)*d2 + 1;
	       j = (n + 2 - 2*i)*d2;
	       /*      do 200 i1 = i0, nt, d3 */
	       for (i1 = i0; i1 <= nt; i1 += d3) {
		    i2 = i1 + d4;
		    /*      do 100 k = i1, i2, d5 */
		    for (k = i1 - 1; k < i2; k += d5) {
			 l = k + j;
			 a = (even[l] + even[k])/2.0;
			 c = (even[l] - even[k])/2.0;
			 b = (odd[l] + odd[k])/2.0;
			 d = (odd[l] - odd[k])/2.0;
			 e = c*si + b*co;
			 f = c*co - b*si;
			 even[k] = a + e;
			 even[l] = a - e;
			 odd[k] = f - d;
			 odd[l] = f + d;
		    }
	       }
	  }
     }
     if (n < 1)
	  return;
     j = n*d2;
     /*      do 500 i1 = 1, nt, d3 */
     for (i1 = 1; i1 <= nt; i1 += d3) {
	  i2 = i1 + d4;
	  /*      do 500 k = i1, i2, d5 */
	  for (k = i1 - 1; k < i2; k += d5) {
	       l = k + j;
	       even[l] = even[k] - odd[k];
	       odd[l] = 0.0;
	       even[k] = even[k] + odd[k];
	       odd[k] = 0.0;
	  }
     }
     return;
}

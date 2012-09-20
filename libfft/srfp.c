/*  srfp.c -   symmetrized reordering factoring programme
 * Original author: Lynn Ten Eyck
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 */
/*  $Id$

$Log$
Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/
/* USE FORTRAN INDEXING TO AVOID REWRITING INDEXING LOGIC */

#include "cfft.h"
#include <stdio.h>

void srfp (int pts, int pmax, int twogrp, int *factor, int *sym, int *psym,
	   int *unsym, int *error)
{
     int pp[15], qq [8];
     int f,j,jj,n,nest,p,ptwo,q,r;

     nest=14;

     n=pts;
     *psym=1;
     f=2;
     p=0;
     q=0;
     /*  100 continue */
     while (n > 1) {
	  /*      if (n <= 1) go to 500 */
	  for (j = f; j <= pmax; j++) {
	       if (n == (n/j)*j)
		    break;
	  }
	  if (j > pmax) {
	       printf("largest factor exceeds %d.  n = %d.\n", pmax, pts);
	       *error=1;
	       return;
	  }
	  if (2*p+q >= nest) {
	       printf("factor count exceeds %d.  n = %d.\n", nest, pts);
	       *error=1;
	       return;
	  }

	  f=j;
	  n=n/f;
	  if (n != (n/f)*f) {
	       q=q+1;
	       qq[q]=f;
	  } else {
	       n=n/f;
	       p=p+1;
	       pp[p]=f;
	       *psym=*psym*f;
	  }
     }
     /*      go to 100
	     500  continue */

     r=1;
     if (q == 0) r=0;
     if (p >= 1) {
	  /*    do 600 j=1,p */
	  for (j = 1; j <= p; j++) {
	       jj=p+1-j;
	       sym[j]=pp[jj];
	       factor[j]=pp[jj];
	       jj=p+q+j;
	       factor[jj]=pp[j];
	       jj=p+r+j;
	       sym[jj]=pp[j];
	  }
     }
     if (q >= 1) {
	  /*     do 800 j=1,q */
	  for (j = 1; j <= q; j++) {
	       jj=p+j;
	       unsym[j]=qq[j];
	       factor[jj]=qq[j];
	  }
	  sym[p+1]=pts / (*psym * *psym);
     }
     jj=2*p+q;
     factor[jj+1]=0;
     ptwo=1;
     j=0;
     while (factor[j+1] != 0) {
	  j=j+1;
	  /*      if (factor[j].eq.0) go to 1200 */
	  if (factor[j] != 2)
	       continue;
	  ptwo=ptwo*2;
	  factor[j]=1;
	  if (ptwo < twogrp && factor[j+1] == 2)
	       continue;
	  factor[j]=ptwo;
	  ptwo=1;
     }

     if (p == 0) r=0;
     jj=2*p+r;
     sym[jj+1]=0;
     if (q <= 1) q=0;
     unsym[q+1]=0;
     *error=0;
     return;
}

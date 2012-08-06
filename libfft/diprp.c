/* diprp.c -  double in place reordering programme
 * Original author: Lynn Ten Eyck
 * Translated to C: David Mastronarde
 * This translation is released under the General Public License.
 */
/*  $Id$

$Log$
Revision 1.1  2004/10/24 21:18:39  mast
Added C version of library to package

*/
#define al u[1]
#define bs s[2]
#define bl u[2]
#define cs s[3]
#define cl u[3]
#define ds s[4]
#define dl u[4]
#define es s[5]
#define el u[5]
#define fs s[6]
#define fl u[6]
#define gs s[7]
#define gl u[7]
#define hs s[8]
#define hl u[8]
#define is s[9]
#define il u[9]
#define js s[10]
#define jl u[10]
#define ks s[11]
#define kl u[11]
#define ls s[12]
#define ll u[12]
#define ms s[13]
#define ml u[13]
#define ns s[14]
#define nl u[14]

#include "cfft.h"

void diprp (int pts, int *sym, int psym, int *unsym, int *dim,
	    float *x, float *y)
{
     float t;
     int onemod;
     int modulo [15];
     int dk,jj,kk,lk,mods,mult,nest,punsym,test;
     int nt, sep, delta, p, p0, p1, p2, p3, p4, p5, size;

     int s [15], u [15];
     int a,b,c,d,e,f,g,h,i,j,k,l,m,n;
     /*      int bs,cs,ds,es,fs,gs,hs,is,js,ks,ls,ms,ns;
	     int al,bl,cl,dl,el,fl,gl,hl,il,jl,kl,ll,ml,nl; */
     /*      equivalence           (al,u(1)),(bs,s(2)),(bl,u(2))
	     equivalence (cs,s(3)),(cl,u(3)),(ds,s(4)),(dl,u(4))
	     equivalence (es,s(5)),(el,u(5)),(fs,s(6)),(fl,u(6))
	     equivalence (gs,s(7)),(gl,u(7)),(hs,s(8)),(hl,u(8))
	     equivalence (is,s(9)),(il,u(9)),(js,s(10)),(jl,u(10))
	     equivalence (ks,s(11)),(kl,u(11)),(ls,s(12)),(ll,u(12))
	     equivalence (ms,s(13)),(ml,u(13)),(ns,s(14)),(nl,u(14)) */


     nest=14;

     nt = dim[1];
     sep = dim[2];
     p2 = dim[3];
     size = dim[4] - 1;
     p4 = dim[5];
     if (sym[1] != 0) {
	  for (j = 1; j <= nest; j++) { 
	       /*     do 100 j=1,nest */
	       u[j]=1;
	       s[j]=1;
	  }
	  n=pts;
	  /*      do 200 j=1,nest */
	  for (j = 1; j <= nest; j++) {
	       if (sym[j] == 0) 
		    break;
	       jj=nest+1-j;
	       u[jj]=n;
	       s[jj]=n/sym[j];
	       n=n/sym[j];
	  }

	  jj=0;
	  /*      do 400 a=1,al
		  do 400 b=a,bl,bs
		  do 400 c=b,cl,cs
		  do 400 d=c,dl,ds
		  do 400 e=d,el,es
		  do 400 f=e,fl,fs
		  do 400 g=f,gl,gs
		  do 400 h=g,hl,hs
		  do 400 i=h,il,is
		  do 400 j=i,jl,js
		  do 400 k=j,kl,ks
		  do 400 l=k,ll,ls
		  do 400 m=l,ml,ms
		  do 400 n=m,nl,ns */
	  for (a = 1; a <= al; a++)
	       for (b = a; b <= bl; b += bs)
		    for (c = b; c <= cl; c += cs)
			 for (d = c; d <= dl; d += ds)
			      for (e = d; e <= el; e += es)
				   for (f = e; f <= fl; f += fs)
					for (g = f; g <= gl; g += gs)
					     for (h = g; h <= hl; h += hs)
						  for (i = h; i <= il; i += is)
						       for (j = i; j <= jl; j += js)
							    for (k = j; k <= kl; k += ks)
								 for (l = k; l <= ll; l += ls)
								      for (m = l; m <= ml; m += ms)
									   for (n = m; n <= nl; n += ns) {
										jj=jj+1;
										if (jj >= n)
										     continue;
										delta = (n-jj)*sep;
										p1 = (jj-1)*sep + 1;
										/*      do 350 p0 = p1, nt, p2 */
										for (p0 = p1; p0 <= nt; p0 += p2) {
										     p3 = p0 + size;
										     /*      do 350 p = p0, p3, p4 */
										     for (p = p0 - 1; p < p3; p += p4) {
											  p5 = p + delta;
											  t = x[p];
											  x[p] = x[p5];
											  x[p5] = t;
											  t = y[p];
											  y[p] = y[p5];
											  y[p5] = t;
										     }
										}
									   }
     }

     if (unsym[1] == 0)
	  return;
     punsym=pts/(psym*psym);
     mult=punsym/unsym[1];
     test=(unsym[1]*unsym[2]-1)*mult*psym;
     lk=mult;
     dk=mult;
     /*      do 600 k=2,nest */
     for (k = 2; k <= nest; k++) {
	  if (unsym[k] == 0) 
	       break;
	  lk=lk*unsym[k-1];
	  dk=dk/unsym[k];
	  u[k]=(lk-dk)*psym;
	  mods=k;
     }
     onemod=mods < 3;
     if (!onemod) {
	  for (j = 3; j <= mods; j++) {
	       /*      do 800 j=3,mods */
	       jj=mods+3-j;
	       modulo[jj]=u[j];
	  }
     }
     modulo[2]=u[2];
     jl=(punsym-3)*psym;
     ms=punsym*psym;

     /*     do 1800 j=psym,jl,psym */
     for (j = psym; j <= jl; j += psym) {
	  k=j;

	  /*  1000 continue */
	  do {
	       k=k*mult;
	       if (!onemod)
		    /*      do 1100 i=3,mods */
		    for (i = 3; i <= mods; i++)
			 k=k-(k/modulo[i])*modulo[i];

	       if (k < test)
		    k=k-(k/modulo[2])*modulo[2];
	       else
		    k=k-(k/modulo[2])*modulo[2]+modulo[2];
	       /*      if (k.lt.j) go to 1000 */
	  } while (k < j);
	  
	  if (k != j) {
	       delta = (k-j)*sep;
	       /*      do 1600 l=1,psym
		       do 1500 m=l,pts,ms */
	       for (l = 1; l <= psym; l++) {
		    for (m = l; m <= pts; m += ms) {
			 p1 = (m+j-1)*sep + 1;
			 /*      do 1500 p0 = p1, nt, p2 */
			 for (p0 = p1; p0 <= nt; p0 += p2) {
			      p3 = p0 + size;
			      /*      do 1500 jj = p0, p3, p4 */
			      for (jj = p0 - 1; jj < p3; jj += p4) {
				   kk = jj + delta;
				   t=x[jj];
					x[jj]=x[kk];
					x[kk]=t;
					t=y[jj];
					y[jj]=y[kk];
					y[kk]=t;
			      }
			 }
		    }
	       }
	  }
     }
     return;
}

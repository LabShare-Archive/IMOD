/* subroutine GAUSSJ
   inverts matrix A of order N, (dimensions NP)
   replaces M columns of B (dimensions NP by MP) with solution vectors
   originally from Cooley and Lohnes - Multivariate Procedures for the
   Behavioral Sciences, Wiley, 1962 (!).
   12/25/90: converted to fortran 77, indented, put in message
   for singularity, verified basically same program as GAUSSJ from
   Press et al 1986 for Gauss-Jordan elimination, changed arguments,
   removed determinant calculation and augmented treatment of B
   10/29/99: translated to C.  Failed to keep Fortran ordering of input data.
   
*/
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.1.2.1  2002/12/05 03:13:02  mast
    New Qt version

    Revision 3.2  2002/08/19 04:42:59  mast
    Made it test for zero before subtracting rows from each other.

    Revision 3.1  2002/08/01 00:29:01  mast
    Increased array sizes to 10000

*/

#define MSIZ 10000
int gaussj(float *a, int n, int np, float *b, int m, int mp)
{
     int index[MSIZ][2];
     float pivot[MSIZ];
     int ipivot[MSIZ];
     int irow, icolum, i, j, k, l, l1;
     float amax, t, abstmp, pivotmp;

     for (j = 0; j < n; j++)
	  ipivot[j]=0;
     for (i = 0; i < n; i++) {
	  amax=0.;
	  for (j = 0; j < n; j++) {
	       if(ipivot[j] != 1) {
				   
		    for (k = 0; k < n; k++) {
			 if(ipivot[k] == 0) {
			      abstmp = a[j*np+k];
			      if (abstmp < 0)
				   abstmp = -abstmp;
			      if(amax < abstmp){
				   irow=j;
				   icolum=k;
				   amax=abstmp;
			      }

			 } else if(ipivot[k] > 1) {
			      /* write(*,*) 'Singular matrix' */
			      return 1;
			 }
		    }
	       }
	  }
	  ipivot[icolum]=ipivot[icolum]+1;
	  if(irow != icolum) {
	       for (l = 0; l < n; l++) {
		    t=a[irow*np+l];
		    a[irow*np+l]=a[icolum*np+l];
		    a[icolum*np+l]=t;
	       }
	       for (l = 0; l < m; l++) {
		    t=b[irow*mp+l];
		    b[irow*mp+l]=b[icolum*mp+l];
		    b[icolum*mp+l]=t;
	       }
	  }
	  index[i][0]=irow;
	  index[i][1]=icolum;
	  pivotmp=a[icolum*np+icolum];
	  /*	  if(abs(pivotmp) < 1.e-30) write(*,*) 'small pivot',pivotmp */
	  pivot[i]=pivotmp;
	  a[icolum*np+icolum]=1.;
	  /*	    worried about that step! */
	  for (l = 0; l < n; l++) 
	       a[icolum*np+l]=a[icolum*np+l]/pivotmp;
	  for (l = 0; l < m; l++)
	       b[icolum*mp+l]=b[icolum*mp+l]/pivotmp;
	  for (l1 = 0; l1 < n; l1++) {
	       t=a[l1*np+icolum];
	       if (t != 0. && l1 != icolum) {
		    a[l1*np+icolum]=0.;
		    for (l = 0; l < n; l++) 
			 a[l1*np+l]=a[l1*np+l]-a[icolum*np+l]*t;
		    for (l = 0; l < m; l++)
			 b[l1*mp+l]=b[l1*mp+l]-b[icolum*mp+l]*t;
	       }
	  }
     }
     for (i = 0; i < n; i++) {
	  l=n-1-i;
	  if(index[l][0] != index[l][1]) {
	       irow=index[l][0];
	       icolum=index[l][1];
	       for (k = 0; k < n; k++) {
		    t=a[k*np+irow];
		    a[k*np+irow]=a[k*np+icolum];
		    a[k*np+icolum]=t;
	       }
	  }
     }
     return 0;
}

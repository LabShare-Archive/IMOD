c	  ICLAVGSD computes the min DMIN, max DMAX, mean AVG, and standard
c	  deviation SD from data in ARRAY, dimensioned NX by NY, for X indices
c	  from IX0 to IX1 and Y indices from IY0 to IY1 (numbered from 1).  It
c	  also returns the sum in SUM8 and sum of squares in SUMSQ8 which are
c	  real*8.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2005/05/26 04:40:45  mast
c	  Switched to accumult sums as double and return them that way too
c	
c	  Revision 3.1  2002/08/18 23:01:39  mast
c	  Added to library after discovering 2 versions in four programs
c	
c
	subroutine iclavgsd(array,nx,ny,ix0,ix1,iy0,iy1,dmin,dmax,sum8
     &	    ,sumsq8,avg,sd)
	implicit none
	real*4 array(*)
	integer*4 nx,ny,ix0,ix1,iy0,iy1
	real*4 dmin,dmax,avg,sd,den
	real*8 sum8,sumsq8,smtm,smtmsq
        integer(kind=8) i, iy, ibas, nsum
        
	sum8=0.
	sumsq8=0.
	dmin=1.e10
	dmax=-1.e10
	do iy=iy0,iy1
	  smtm=0.
	  smtmsq=0.
	  ibas=(iy-1)*nx
	  do i=ix0+ibas,ix1+ibas
	    den=array(i)
	    smtm=smtm+den
	    smtmsq=smtmsq+den**2
	    dmin=min(dmin,den)
 	    dmax=max(dmax,den)
	  enddo
	  sum8=sum8+smtm
	  sumsq8=sumsq8+smtmsq
	enddo
	nsum=int(ix1+1-ix0, kind=8)*(iy1+1-iy0)
	avg=sum8/nsum
	sd=sqrt((sumsq8-sum8**2/nsum)/(nsum-1))
	return
	end

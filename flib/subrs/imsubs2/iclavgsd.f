c	  ICLAVGSD computes the min DMIN, max DMAX, mean AVG, and standard
c	  deviation SD from data in ARRAY, dimensioned NX by NY, for X indices
c	  from IX0 to IX1 and Y indices from IY0 to IY1 (numbered from 1).  It
c	  also returns the sum in SUM and sum of squares in SUMSQ
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine iclavgsd(array,nx,ny,ix0,ix1,iy0,iy1,dmin,dmax,sum
     &	    ,sumsq,avg,sd)
	implicit none
	real*4 array(*)
	integer*4 nx,ny,iy,ibas,i,nsum,ix0,ix1,iy0,iy1
	real*4 dmin,dmax,sum,sumsq,avg,sd,smtm,smtmsq,den
	sum=0.
	sumsq=0.
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
	  sum=sum+smtm
	  sumsq=sumsq+smtmsq
	enddo
	nsum=(ix1+1-ix0)*(iy1+1-iy0)
	avg=sum/nsum
	sd=sqrt((sumsq-sum**2/nsum)/(nsum-1))
	return
	end

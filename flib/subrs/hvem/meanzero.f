c	  MEANZERO will shift the contents of ARRAY to a zero mean.  NXDIM
c	  specifies the X dimension of ARRAY, and NX and NY are the size of
c	  the data in ARRAY.

	subroutine meanzero(array,nxdim,nx,ny)
	real*4 array(nxdim,ny)
	sum=0.
	do iy=1,ny
	  tsum=0.
	  do ix=1,nx
	    tsum=tsum+array(ix,iy)
	  enddo
	  sum=sum+tsum
	enddo
	dmean=sum/(nx*ny)
c	  
	do iy=1,ny
	  do ix=1,nx
	    array(ix,iy)=array(ix,iy)-dmean
	  enddo
	enddo
	return
	end


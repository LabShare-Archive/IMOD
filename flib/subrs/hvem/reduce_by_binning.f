c	  REDUCE_BY_BINNING will reduce an array in size by summing pixels
c	  (binning) by the factor NBIN.  ARRAY has the input, with dimension
c	  NX by NY, while BRRAY receives the output, with dimensions returned
c	  in NXR and NYR.  BRRAY can be the same as ARRAY, and NXR, NYR can
c	  be the same variables as NX, NY
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine reduce_by_binning(array,nx,ny,nbin,brray,nxr,nyr)
	implicit none
	real*4 array(*),brray(*)
	integer*4 nx,ny,nxr,nyr,nbin
	integer*4 ixofset,iyofset,iyout,ixout,ixin,iyin,ixbase
	integer*4 linebase,ixoutbase,nxrtmp,nyrtmp
	real*4 sum,binsq
c	  
c	  get reduced size and split any remainder equally to left and right
c
	nxrtmp=nx/nbin
	nyrtmp=ny/nbin
	ixofset=mod(nx,nbin)/2
	iyofset=mod(ny,nbin)/2
	binsq=nbin**2

	do iyout=1,nyrtmp
	  linebase=nx*(nbin*(iyout-1)+iyofset)+ixofset
	  ixoutbase=nxrtmp*(iyout-1)
	  do ixout=1,nxrtmp
	    ixbase=linebase
	    sum=0.
	    do iyin=1,nbin
	      do ixin=1,nbin
		sum=sum+array(ixbase+ixin)
	      enddo
	      ixbase=ixbase+nx
	    enddo
	    brray(ixoutbase+ixout)=sum/binsq
	    linebase=linebase+nbin
	  enddo
	enddo
	nxr=nxrtmp
	nyr=nyrtmp
	return
	end

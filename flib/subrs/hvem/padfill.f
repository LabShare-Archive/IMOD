c	  PADFILL pads an image, dimensions NXBOX by NYBOX in ARRAY,
c	  into the center of a larger array.  The padded image in BRRAY will
c	  have size NX by NY while the BRRAY will be dimensioned NXDIM by NY
c	  The overall mean of the input image will be used for padding if
c	  MEANEDGE is zero; otherwise the mean of the edge will be used.
c	  BRRAY can be the same as ARRAY
c
	subroutine padfill(array,nxbox,nybox,brray,nxdim,nx,ny,meanedge)
	real*4 array(nxbox,nybox),brray(nxdim,ny)
c
	if(nxbox.ne.nx.or.nybox.ne.ny)then
	  call iclden(array,nxbox,nybox,1,nxbox,1,nybox,dmin,dmax,dmean)
	  fill=dmean
	  bias=dmean
	  if(meanedge.ne.0)then
c
c	      find mean of edge of box
c
	    sum=0.
	    do ix=1,nxbox
	      sum=sum+array(ix,1)+array(ix,nybox)
	    enddo
	    do iy=2,nybox-1
	      sum=sum+array(1,iy)+array(nxbox,iy)
	    enddo
	    edge=sum/(2*nxbox+2*(nybox-1))
c
c	      fill with edge value and subtract a bias that would produce
c	      a mean of zero
c	      
	    fill=edge
	    bias=edge+(dmean-edge)*float(nxbox*nybox)/float(nx*ny)
c	      
	  endif
	endif
c
c	  move array into brray, splitting it into the 4 corners of brray
c
	ixlo=nx/2-nxbox/2
	ixhi=ixlo+nxbox
	iylo=ny/2-nybox/2
	iyhi=iylo+nybox
	do iy=ny,1,-1
	  if(iy.le.iylo.or.iy.gt.iyhi)then
	    do ix=nx,1,-1
	      brray(ix,iy)=fill-bias
	    enddo
	  else
	    do ix=nx,ixhi+1,-1
	      brray(ix,iy)=fill-bias
	    enddo
	    do ix=ixhi,ixlo+1,-1
	      brray(ix,iy)=array(ix-ixlo,iy-iylo)-bias
	    enddo
	    do ix=ixlo,1,-1
	      brray(ix,iy)=fill-bias
	    enddo
	  endif
	enddo
	return
	end

c	  TAPEROUTPAD pads an image, dimensions NXBOX by NYBOX in ARRAY,
c	  into the center of a larger array.  The padded image in BRRAY will
c	  have size NX by NY while the BRRAY will be dimensioned NXDIM by NY
c	  The values of the image at its edge will be tapered out either to the
c	  supplied mean value DMEANIN, if IFMEAN is nonzero, or to the mean at
c	  the pre-existing edge of the image, if IFMEAN is 0.
c
	subroutine taperoutpad(array,nxbox,nybox,brray,nxdim,nx,ny,
     &	    ifmean,dmeanin)
	real*4 array(nxbox,nybox),brray(nxdim,ny)

	if(ifmean.ne.0)then
	  dmean=dmeanin
	else
	  sum=0.
	  do ix=1,nxbox
	    sum=sum+array(ix,1)+array(ix,nybox)
	  enddo
	  do iy=2,nybox-1
	    sum=sum+array(1,iy)+array(nxbox,iy)
	  enddo
	  dmean=sum/(2*(nxbox+nybox-2))
	endif

	ixlo=(nx-nxbox)/2
	ixhi=ixlo+nxbox
	iylo=(ny-nybox)/2
	iyhi=iylo+nybox
	do iy=nybox,1,-1
	  do ix=nxbox,1,-1
	    brray(ix+ixlo,iy+iylo)=array(ix,iy)
	  enddo
	enddo
	if(nxbox.ne.nx.or.nybox.ne.ny)then
	  nxtop=nx+1
	  nytop=ny+1
c	    
c	    if there is a mismatch between left and right, add a column on 
c	    right; similarly for bottom versus top, add a row on top
c
	  if(nx-ixhi.gt.ixlo)then
	    nxtop=nx
	    do iy=1,ny
	      brray(nx,iy)=dmean
	    enddo
	  endif
	  if(ny-iyhi.gt.iylo)then
	    nytop=ny
	    do ix=1,nx
	      brray(ix,ny)=dmean
	    enddo
	  endif
c
	  do iy=iylo+1,iyhi
	    edgel=brray(ixlo+1,iy)
	    edger=brray(ixhi,iy)
	    do ix=1,ixlo
	      wedge=(ix-1.)/ixlo
	      wmean=1.-wedge
	      brray(ix,iy)=wmean*dmean+wedge*edgel
	      brray(nxtop-ix,iy)=wmean*dmean+wedge*edger
	    enddo
	  enddo
	  do iy=1,iylo
	    wedge=(iy-1.)/iylo
	    prodmean=(1.-wedge)*dmean
	    iytoplin=nytop-iy
	    do ix=1,nx
	      brray(ix,iy)=prodmean+wedge*brray(ix,iylo+1)
	      brray(ix,iytoplin)=prodmean+wedge*brray(ix,iyhi)
	    enddo
	  enddo
	endif
	return
	end



c	  TAPERINPAD pads an image, dimensions NXBOX by NYBOX in
c	  ARRAY, into the center of a larger array.  The padded image in BRRAY
c	  will have size NX by NY while the BRRAY will be dimensioned
c	  NXDIM by NY.  The values of the image at its edge will be
c	  tapered down to the mean value at the edge, over a width of the
c	  original image area equal to NXTAP or NYTAP.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine taperinpad(array,nxbox,nybox,brray,nxdim,nx,
     &	    ny, nxtap,nytap)
	real*4 array(nxbox,nybox),brray(nxdim,ny)
c
	sum=0.
	do ix=1,nxbox
	  sum=sum+array(ix,1)+array(ix,nybox)
	enddo
	do iy=2,nybox-1
	  sum=sum+array(1,iy)+array(nxbox,iy)
	enddo
	nsum=(2*(nxbox+nybox-2))
	dmean=sum/nsum
c	  
	ixlo=nx/2-nxbox/2
	ixhi=ixlo+nxbox
	iylo=ny/2-nybox/2
	iyhi=iylo+nybox
	do iy=nybox,1,-1
	  do ix=nxbox,1,-1
	    brray(ix+ixlo,iy+iylo)=array(ix,iy)
	  enddo
	enddo
	if(nxbox.ne.nx.or.nybox.ne.ny)then
	  do iy=iylo+1,iyhi
	    do ix=1,ixlo
	      brray(ix,iy)=dmean
	      brray(nx+1-ix,iy)=dmean
	    enddo
	  enddo
	  do iy=1,iylo
	    iytoplin=ny+1-iy
	    do ix=1,nx
	      brray(ix,iy)=dmean
	      brray(ix,iytoplin)=dmean
	    enddo
	  enddo
	endif
c
	do iy=1,(nybox+1)/2
	  fracy=1.
	  if(iy.le.nytap)fracy=iy/(nytap+1.)
	  do ix=1,(nxbox+1)/2
	    fracx=1.
	    if(ix.le.nxtap)fracx=ix/(nxtap+1.)
	    fmin=min(fracx,fracy)
	    if(fmin.lt.1.)then
	      ix1=ix+ixlo
	      ix2=ixhi+1-ix
	      iy1=iy+iylo
	      iy2=iyhi+1-iy
c		
c		DNM 4/28/02: for odd box sizes, deflect middle pixel to edge
c		to keep it from being attenuated twice
c
	      if(ix1.eq.ix2)ix2=1
	      if(iy1.eq.iy2)iy2=1
	      brray(ix1,iy1)=fmin*(brray(ix1,iy1)-dmean)+dmean
	      brray(ix1,iy2)=fmin*(brray(ix1,iy2)-dmean)+dmean
	      brray(ix2,iy1)=fmin*(brray(ix2,iy1)-dmean)+dmean
	      brray(ix2,iy2)=fmin*(brray(ix2,iy2)-dmean)+dmean
	    endif
	  enddo
	enddo
	return
	end


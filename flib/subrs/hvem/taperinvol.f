c	  TAPERINVOL pads a volume, dimensions NXBOX by NYBOX by NZBOX in
c	  ARRAY, into the center of a larger array.  The padded image in BRRAY
c	  will have size NX by NY by NZ while BRRAY is assumed to be
c	  dimensioned NXDIM by NY by NZ.  The values of the image at its edge
c	  will be tapered down to the mean value at the edge, over a width of
c	  the original image area equal to NXTAP, NYTAP, and NZTAP.
c
	subroutine taperinvol(array,nxbox,nybox,nzbox,brray,nxdim,nx,
     &	    ny, nz,nxtap,nytap,nztap)
	real*4 array(nxbox,nybox,nzbox),brray(nxdim,ny,nz)
c
	sum=0.
	do iy=1,nybox
	  do ix=1,nxbox
	    sum=sum+array(ix,iy,1)+array(ix,iy,nzbox)
	  enddo
	enddo
	nsum=2*nxbox*nybox
	do iz=2,nzbox-1
	  do ix=1,nxbox
	    sum=sum+array(ix,1,iz)+array(ix,nybox,iz)
	  enddo
	  do iy=2,nybox-1
	    sum=sum+array(1,iy,iz)+array(nxbox,iy,iz)
	  enddo
	  nsum=nsum+(2*(nxbox+nybox-2))
	enddo
	dmean=sum/nsum
c	  
	ixlo=(nx-nxbox)/2
	ixhi=ixlo+nxbox
	iylo=(ny-nybox)/2
	iyhi=iylo+nybox
	izlo=(nz-nzbox)/2
	izhi=izlo+nzbox
	do iz=nzbox,1,-1
	  do iy=nybox,1,-1
	    do ix=nxbox,1,-1
	      brray(ix+ixlo,iy+iylo,iz+izlo)=array(ix,iy,iz)
	    enddo
	  enddo
	enddo

	if(nxbox.ne.nx.or.nybox.ne.ny.or.nzbox.ne.nz)then
	  do iz=izlo+1,izhi
	    nxtop=nx+1
	    nytop=ny+1
	    nztop=nz+1
	
c	    
c	    if there is a mismatch between left and right, add a column on 
c	    right; similarly for bottom versus top, add a row on top
c
	    if(nx-ixhi.gt.ixlo)then
	      nxtop=nx
	      do iy=1,ny
		brray(nx,iy,iz)=dmean
	      enddo
	    endif
	    if(ny-iyhi.gt.iylo)then
	      nytop=ny
	      do ix=1,nx
		brray(ix,ny,iz)=dmean
	      enddo
	    endif
c
	    do iy=iylo+1,iyhi
	      do ix=1,ixlo
		brray(ix,iy,iz)=dmean
		brray(nxtop-ix,iy,iz)=dmean
	      enddo
	    enddo
	    do iy=1,iylo
	      iytoplin=nytop-iy
	      do ix=1,nx
		brray(ix,iy,iz)=dmean
		brray(ix,iytoplin,iz)=dmean
	      enddo
	    enddo
	  enddo
c	    
	  if(nz-izhi.gt.izlo)then
	    nztop=nz
	    do iy=1,ny
	      do ix=1,nx
		brray(ix,iy,nz)=dmean
	      enddo
	    enddo
	  endif
	  do iz=1,izlo
	    iztoplin=nztop-iz
	    do iy=1,ny
	      do ix=1,nx
		brray(ix,iy,iz)=dmean
		brray(ix,iy,iztoplin)=dmean
	      enddo
	    enddo
	  enddo
	endif
c
c	  do 8 pixels at once, using symmetry.  if the box was odd in a 
c	  dimension, deflect the middle pixel out to the edge to keep it from
c	  being attenuated twice
c
	do iz=1,(nzbox+1)/2
	  fracz=1.
	  if(iz.le.nztap)fracz=iz/(nztap+1.)
	  iz1=iz+izlo
	  iz2=izhi+1-iz
	  if(iz2.eq.iz1)iz2=nz
	  do iy=1,(nybox+1)/2
	    fracy=1.
	    if(iy.le.nytap)fracy=iy/(nytap+1.)
	    iy1=iy+iylo
	    iy2=iyhi+1-iy
	    if(iy2.eq.iy1)iy2=ny
	    nxlim=(nxbox+1)/2
	    if(fracy.eq.1..and.fracz.eq.1.)nxlim=nxtap
	    do ix=1,nxlim
	      fracx=1.
	      if(ix.le.nxtap)fracx=ix/(nxtap+1.)
	      fmin=min(fracx,fracy,fracz)
	      ix1=ix+ixlo
	      ix2=ixhi+1-ix
	      if(ix2.eq.ix1)ix2=nx
	      brray(ix1,iy1,iz1)=fmin*(brray(ix1,iy1,iz1)-dmean)+dmean
	      brray(ix1,iy2,iz1)=fmin*(brray(ix1,iy2,iz1)-dmean)+dmean
	      brray(ix2,iy1,iz1)=fmin*(brray(ix2,iy1,iz1)-dmean)+dmean
	      brray(ix2,iy2,iz1)=fmin*(brray(ix2,iy2,iz1)-dmean)+dmean
	      brray(ix1,iy1,iz2)=fmin*(brray(ix1,iy1,iz2)-dmean)+dmean
	      brray(ix1,iy2,iz2)=fmin*(brray(ix1,iy2,iz2)-dmean)+dmean
	      brray(ix2,iy1,iz2)=fmin*(brray(ix2,iy1,iz2)-dmean)+dmean
	      brray(ix2,iy2,iz2)=fmin*(brray(ix2,iy2,iz2)-dmean)+dmean
	    enddo
	  enddo
	enddo
	return
	end

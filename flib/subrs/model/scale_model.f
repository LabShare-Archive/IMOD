c	  SCALE_MODEL will undo or redo the scaling from image index 
c	  coordinates to model coordinates performed by imodel_fwrap.
c	  Set IDIR to 0 to undo the scaling, for working in index coordinates
c	  set IDIR to 1 to redo the scaling before saving the model back out
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine scale_model(idir)
	implicit none
	include 'model.inc'
	integer getimodhead,getimodscales
	integer*4 ierr, ierr2, ifflip, idir, i
	real*4 ximscale, yimscale, zimscale,xyscal,zscale,xofs,yofs,zofs
c
	ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	ierr2 = getimodscales(ximscale, yimscale, zimscale)
	if (ierr .ne. 0 .or. ierr2 .ne. 0) then
	  print *,'ERROR: SCALE_MODEL: getting model header'
	  call exit(1)
	endif
	if (idir .eq. 0) then
c	  
c	    shift the data to index coordinates before working with it
c	    
	  do i=1,n_point
	    p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
	    p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
	    p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
	  enddo
	else
c	  
c	    shift the data back for saving
c	    
	  do i=1,n_point
	    p_coord(1,i)=ximscale*p_coord(1,i)+xofs
	    p_coord(2,i)=yimscale*p_coord(2,i)+yofs
	    p_coord(3,i)=zimscale*p_coord(3,i)+zofs
	  enddo
	endif
	return
	end

c	  maggradfield.f contains routines for computing distortions due to 
c	  mag gradients
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  
c	  MakeMagGradField  makes a distortion field array with the shifts
c	  from the mag gradient
c	  
c	  AddMagGradField  adds the distortions from mag gradients to those
c	  in an existing distortion field
c	  
c	  The following arguments apply to both subroutines:
c	  
c	  gradDx, gradDy  are the arrays returned with the X and Y shifts
c	  at each location
c	  idfDx, idfDy are arrays with an image distortion field (for
c	  AddMagGradField) or used for temporary storage (for MakeMagGradField)
c	  All of these are dimensioned lmGrid x lmGrid
c	  imageNx and imageNy are the image size that gradients are being 
c	  applied to.
c	  ixGridStrt, iyGridStrt are coordinates as which grid starts
c	  xGridIntrv, yGridIntrv are the spacing between grid points in X & Y
c	  nxGrid, nyGrid are the number of grid points in X & Y
c	  xcen, ycen is the center coordinate for the mag and rotation changes
c	  pixelSize is the pixel size in Angstroms
c	  axisRot is the rotation of the tilt axis from the vertical
c	  tilt is the tilt angle
c	  dmagPerUm is the percent change in magnification per micron of Z
c	  rotPerUm is the rotation in degrees per micron of Z height
c	  
	subroutine MakeMagGradField(idfDx, idfDy, gradDx, gradDy,
     &	    lmGrid, imageNx, imageNy, ixGridStrt, xGridIntrv, nxGrid,
     &	    iyGridStrt, yGridIntrv, nyGrid, xcen, ycen, pixelSize,
     &	    axisRot, tilt, dmagPerUm, rotPerUm)
	implicit none
	integer*4  imageNx, imageNy, lmGrid
	integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
	real*4 xGridIntrv, yGridIntrv, pixelSize
	real*4 axisRot, tilt, dmagPerUm, rotPerUm, xcen, ycen
	real*4 idfDx(lmGrid, lmGrid), idfDy(lmGrid, lmGrid)
	real*4 gradDx(lmGrid, lmGrid), gradDy(lmGrid, lmGrid)
	integer*4 i, j
c	  
c	  Set up a grid that has maximum resolution for the array size
c	  
	nxGrid = lmGrid
	nyGrid = lmGrid
	ixGridStrt = 1
	iyGridStrt = 1
	xGridIntrv = (imageNx - 1.) / (lmGrid - 1.)
	yGridIntrv = (imageNy - 1.) / (lmGrid - 1.)
	do i = 1, lmGrid
	  do j = 1, lmGrid
	    idfDx(i, j) = 0.
	    idfDy(i, j) = 0.
	  enddo
	enddo
	call AddMagGradField(idfDx, idfDy, gradDx, gradDy,
     &	    lmGrid, imageNx, imageNy, ixGridStrt, xGridIntrv, nxGrid,
     &	    iyGridStrt, yGridIntrv, nyGrid, xcen, ycen, pixelSize,
     &	    axisRot, tilt, dmagPerUm, rotPerUm)
	return
	end


	subroutine AddMagGradField(idfDx, idfDy, gradDx, gradDy,
     &	    lmGrid, imageNx, imageNy, ixGridStrt, xGridIntrv, nxGrid,
     &	    iyGridStrt, yGridIntrv, nyGrid, xcen, ycen, pixelSize,
     &	    axisRot, tilt, dmagPerUm, rotPerUm)
	implicit none
	integer*4  imageNx, imageNy, lmGrid
	integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
	real*4 xGridIntrv, yGridIntrv, pixelSize
	real*4 axisRot, tilt, dmagPerUm, rotPerUm, xcen, ycen
	real*4 idfDx(lmGrid, lmGrid), idfDy(lmGrid, lmGrid)
	real*4 gradDx(lmGrid, lmGrid), gradDy(lmGrid, lmGrid)
	integer*4 ix, iy
	real*4 dx, dy, xx, yy, dx2, dy2, cosphi, sinphi, tantheta, xrel, yrel
c
	do ix = 1, nxGrid
	  do iy = 1, nyGrid
	    xx = ixGridStrt + (ix - 1) * xGridIntrv
	    yy = iyGridStrt + (iy - 1) * yGridIntrv
c	      
c	      Get the shift due to the mag gradient,
c	      Then look up the distortion field at this point and add the
c	      two shifts to get the total field
c
	    call magGradientShift(xx, yy, imageNx, imageNy, xcen, ycen,
     &		pixelSize, axisRot, tilt, dmagPerUm, rotPerUm, dx, dy)
	    call interpolateGrid(xx + dx, yy + dy, idfDx, idfDy, lmGrid,
     &		nxGrid, nyGrid, ixGridStrt, xGridIntrv, iyGridStrt,
     &		yGridIntrv, dx2, dy2)
	    gradDx(ix, iy) = dx + dx2
	    gradDy(ix, iy) = dy + dy2
	  enddo
	enddo
	return
	end


c	  MagGradientShift computes the shifts from a mag gradient at one point
c	  
c	  xx, yy is the coordinate of the point
c	  imageNx and imageNy are the image size that gradients are being 
c	  applied to.
c	  xcen, ycen is the center coordinate for the mag and rotation changes
c	  pixelSize is the pixel size in Angstroms
c	  axisRot is the rotation of the tilt axis from the vertical
c	  tilt is the tilt angle
c	  dmagPerUm is the percent change in magnification per micron of Z
c	  rotPerUm is the rotation in degrees per micron of Z height
c	  dx, dy are returned with the shifts
c	  
	subroutine magGradientShift(xx, yy, imageNx, imageNy, xcen, ycen,
     &	    pixelSize, axisRot, tilt, dmagPerUm, rotPerUm, dx, dy)
	implicit none
	integer*4  imageNx, imageNy
	real*4 pixelSize
	real*4 axisRot, tilt, dmagPerUm, rotPerUm, xcen, ycen
	real*4 dx, dy, xx, yy, cosphi, sinphi, tantheta, xrel, yrel
	real*4 zh, sinrz, cosrz, gmag
	real*4 cosd, sind, tand
c
c	  get trig values for the rotation to axis and tilt angle
c
	cosphi = cosd(axisRot) * pixelSize / 10000.
	sinphi = sind(axisRot) * pixelSize / 10000.
	tantheta = tand(tilt)
c	      
c	      compute the location of this point relative to the mag center
c	      and its vertical height and thus rotation and mag
c
	xrel = xx - xcen
	yrel = yy - ycen
	zh = tantheta * (xrel * cosphi + yrel * sinphi)
	sinrz = sind(rotPerUm * zh)
	cosrz = cosd(rotPerUm * zh)
	gmag = (1. + 0.01 * dmagPerUm * zh)
c	  
c	  have to mag around the center of this picture so do transform
c	  relative to that to get dx, dy
c	  
	xrel = xx - imageNx / 2.
	yrel = yy - imageNy / 2.
	dx = (xrel * cosrz - yrel * sinrz) * gmag + imageNx / 2. - xx
	dy = (xrel * sinrz + yrel * cosrz) * gmag + imageNy / 2. - yy
	return
	end

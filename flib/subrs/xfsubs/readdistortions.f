c	  This file contains modules for reading distortion field files
c	  or mag gradient files
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 1.1  2003/12/27 19:23:55  mast
c	  Addition to library
c	
c	  readDistortions will read a distortion field from a file
c	  idfFile  specifies name of file with field
c	  fieldDx and fieldDy are arrays in which to place the grid, 
c	  dimensioned to lmGrid x lmGrid
c	  ifdNx and idfNy are the image size from which field was measured
c	  idfBinning is the binning of those images
c	  pixelIdf is their pixel size in Angstroms
c	  ixGridStrt, iyGridStrt are coordinates as which grid starts
c	  xGridIntrv, yGridIntrv are the spacing between grid points in X & Y
c	  nxGrid, nyGrid are the number of grid points in X & Y
c
	subroutine readDistortions(idfFile, fieldDx, fieldDy, lmGrid, idfNx,
     &	    idfNy, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &	    iyGridStrt, yGridIntrv, nyGrid)
	implicit none
	character*(*) idfFile
	integer*4  idfBinning, idfNx, idfNy, lmGrid
	integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
	real*4 xGridIntrv, yGridIntrv, pixelIdf
	real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
c	  
	integer*4 idfVersion, i, j
c	  
	call dopen(14, idfFile, 'ro', 'f')
	read(14, *) idfVersion
	if (idfVersion .eq. 1) then
	  read(14, *)idfNx, idfNy, idfBinning, pixelIdf
	  read(14, *)ixGridStrt, xGridIntrv, nxGrid, iyGridStrt,
     &	      yGridIntrv, nyGrid
	  if (nxGrid .gt. lmGrid .or. nyGrid .gt. lmGrid) then
	    print *
	    print *,'ERROR: readDistortions - too many grid points for arrays'
	    call exit(1)
	  endif
	  do j = 1, nyGrid
	    read(14, *)(fieldDx(i, j), fieldDy(i,j), i = 1, nxGrid)
	  enddo
	else
	  print *
	  print *,'ERROR: readDistortions - version',idfVersion,
     &	      ' of idf file not recognized'
	  call exit(1)
	endif
	close(14)
	return
	end


c	  readMagGradients will read from a file with mag gradients and tilt
c	  angles for each view, as prepared by extractmagrad
c	  magFile  specifies name of file with gradient information
c	  maxVals is the maximum number of values that are allowed 
c	  pixelSize is the pixel size in Angstroms
c	  axisRot is the rotation of the tilt axis from vertical
c	  tilt is an array to receive tilt angles
c	  dmagPerUm is an array to receive percent mag per micron
c	  rotPerUm is an array to receive degrees of rotation per micron
c	  numVals is returned with the number of values
c
	subroutine readMagGradients(magFile, maxVals, pixelSize,
     &	    axisRot, tilt, dmagPerUm, rotPerUm, numVals)
	implicit none
	character*(*) magFile
	integer*4 magVersion, numVals, maxVals, i
	real*4 pixelSize, axisRot, tilt(*), dmagPerUm(*), rotPerUm(*)
c	  
	call dopen(14, magFile, 'ro', 'f')
	read(14, *) magVersion
	if (magVersion .eq. 1) then
	  read(14, *)numVals, pixelSize, axisRot
	  if (numVals .gt. maxVals) then
	    print *
	    print *,'ERROR: readMagGradients - too many values for arrays'
	    call exit(1)
	  endif
	  do i = 1, numVals
	    read(14, *) tilt(i), dmagPerUm(i), rotPerUm(i)
	  enddo

	else
	  print *
	  print *,'ERROR: readMagGradients - version',magVersion,
     &	      ' of gradient file not recognized'
	  call exit(1)
	endif
	close(14)
	return
	end

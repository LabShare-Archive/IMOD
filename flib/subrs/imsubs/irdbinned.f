c	  IRDBINNED reads a specified portion of a section and can bin the
c	  image by an arbitrary amount.
c
c	  call irdBinned(imunit, iz, array, ixdim, iydim,
c     &	    ixst, iyst, nbin, nxbin, nybin, temp, lenTemp, ierr)
c
c	  IMUNIT - image file unit number
c	  IZ     - section #, numbered from 0
c	  ARRAY  - array in which to return image
c	  IXDIM  - X dimension of array (set to NXBIN for one-D array)
c	  IYDIM  - Y dimension of array
c	  IXST   - starting unbinned X coordinate, numbered from 0.
c	  IYST   - starting unbinned Y coordinate, numbered from 0.
c	  NBIN   - factor to bin by, or 1 for no binning
c	  NXBIN  - binned size of image in X
c	  NYBIN  - binned size of image in Y
c	  TEMP   - temporary real array for reading unbinned data into
c	  LENTEMP - size of TEMP array
c	  IERR   - return value, 0 if succeeded or 1 if failed
c
c	  Both IXST and IYST can be negative as long as they are bigger than
c	  -NBIN.  This allows one to offset the binned data so as to maintain
c	  centering.  With an offset, the first binned row or column is based
c	  on fewer than NBIN rows or columns of unbinned data.  In addition,
c	  the last row or column can also be based on fewer than NBIN rows or
c	  columns of unbinned data, but the calling routine is responsible
c	  for not setting NXBIN or NYBIN too high and requesting a completely
c	  empty binned column.  The temporary array may be as small as needed
c	  to fit one binned pixel, but access will be more efficient if it is
c	  at least as big as the binning times the unbinned X dimension.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 1.1  2003/12/17 22:11:44  mast
c	  Initial creation
c	
c
	subroutine irdBinned(imunit, iz, array, ixdim, iydim,
     &	    ixst, iyst, nbin, nxbin, nybin, temp, lenTemp, ierr)
        use imsubs
	implicit none
	integer*4 imunit, ixdim, iydim, ix0, ix1, iy0, iy1, nbin, nx, ny
	integer*4 lenTemp, nxbin, ixoffset, nybin, iyoffset, ierr, iz
	integer*4 ixst,iyst
	real*4 array(ixdim, iydim), temp(lenTemp)
	integer*4 ixb, nxLoad, nyLoad, maxLineLoad, maxColLoad, loadYoffset
	integer*4 iyStart, iyDone, nBinLines, loadXoffset, ixStart, ixDone
	integer*4 nBinCols, iyb, iFastStrt, iFastEnd, ixnd, iynd, ix, iy
	integer*4 iCheckStrt, iCheckEnd, iCheck, nsum
	real*4 sum, binsq
c
	ixb = lstream(imunit)
	nx = ncrs(1, ixb)
	ny = ncrs(2, ixb)
	ierr = 1
c	  
c	  convert input starting coordinate to actual start and offset
c	  a negative start becomes an offset, and start then becomes 0
c	  
	ix1 = min(nx, ixst + nxbin * nbin) - 1
	ixOffset = min(0, ixst)
	ix0 = max(0, ixst)
c
	iy1 = min(ny, iyst + nybin * nbin) - 1
	iyOffset = min(0, iyst)
	iy0 = max(0, iyst)
c	  
c	  if binning 1, use regular routines
c
	if (nbin .eq. 1) then
	  call imposn(imunit, iz, 0)
	  if (ixdim .eq. nx .and. nxbin .eq. nx .and. nybin .eq. ny) then
	    call irdsec(imunit, array, *99)
	  else
	    call irdpas(imunit, array, ixdim, iydim, ix0, ix1, iy0, iy1, *99)
	  endif
	  ierr = 0
	  return
	endif
c	  
c	  figure out how many columns and lines can be done at once in temp
c
	if (lenTemp .lt. nbin**2) then
	  print *
	  print *,'ERROR: IRDBINNED - BINNING TOO LARGE FOR TEMPORARY ARRAY'
	  return
	endif
	binsq = nbin**2
	nxLoad = ix1 + 1 - ix0
	maxLineLoad = lenTemp / nxLoad
	maxColLoad = nxLoad
	if (maxLineLoad .lt. nbin) then
	  maxLineLoad = nbin
	  maxColLoad = lenTemp / nbin
	endif

	loadYoffset = iyoffset
	iyStart = iy0
	iyDone = 0
	do while (iyStart .le. iy1)
c	    
c	    if loading is limited by max lines, then load to a bin boundary
c	    otherwise load the rest of the lines
c
	  if (maxLineLoad .lt. iy1 + 1 - iyStart) then
	    nyLoad = nbin * ((maxLineLoad - loadYoffset) / nbin) + loadYoffset
	  else
	    nyLoad = iy1 + 1 - iyStart
	  endif
c	    
c	    this will do a partially empty row at end if there is one
c
	  nBinLines = (nyLoad - loadYoffset + nbin - 1) / nbin
	  loadXoffset = ixoffset
	  ixStart = ix0
	  ixDone = 0
	  do while (ixStart .le. ix1)
c	      
c	      similarly, load X to a bin boundary or to the end of the line
c
	    if (maxColLoad .lt. ix1 + 1 - ixStart) then
	      nxLoad = nbin * ((maxColLoad - loadXoffset) / nbin) + loadXoffset
	    else
	      nxLoad = ix1 + 1 - ixStart
	    endif
	    nBinCols = (nxLoad - loadXoffset + nbin - 1) / nbin
c	    
c	      read in data
c	      
c	    print *,'loading',nxLoad, nyLoad, ixStart,
c     &          ixStart + nxLoad - 1, iyStart, iyStart + nyLoad - 1
	    call imposn(imunit, iz, 0)
	    call irdpas(imunit, temp, nxLoad, nyLoad, ixStart,
     &		ixStart + nxLoad - 1, iyStart, iyStart + nyLoad - 1, *99)
	    do iyb = 1, nBinLines
c		
c		find extent in X that can be done without checks
c
	      if ((iyb - 1) * nbin + loadYoffset .lt. 0 .or.
     &		  iyb * nbin + loadYoffset .gt. nyLoad) then
		iFastStrt = nBinCols + 1
		iFastEnd = nBinCols
	      else
		iFastStrt = 1
		iFastEnd = nBinCols
		if (loadXoffset .ne. 0) iFastStrt = 2
		if (nBinCols * nbin + loadXoffset .gt. nxLoad)
     &		    iFastEnd = nBinCols - 1
	      endif
c		  
c		  quick sums without checks
c
	      do ixb = iFastStrt, iFastEnd
		sum = 0.
		iynd = iyb * nbin + loadYoffset
		ixnd = ixb * nbin + loadXoffset
		do iy = iynd + 1 - nbin, iynd
		  do ix = ixnd + 1 - nbin, ixnd
		    sum = sum + temp(ix + (iy - 1) * nxLoad)
		  enddo
		enddo
		array(ixDone + ixb, iyDone + iyb) = sum / binsq
	      enddo

	      iCheckStrt = 1
	      iCheckEnd = iFastStrt - 1
	      do iCheck = 1, 2
		do ixb = iCheckStrt, iCheckEnd
c		    
c		    sums with checks
c		      
		  sum = 0.
		  nsum = 0
		  iynd = iyb * nbin + loadYoffset
		  ixnd = ixb * nbin + loadXoffset
		  do iy = iynd + 1 - nbin, iynd
		    do ix = ixnd + 1 - nbin, ixnd
		      if (ix .ge. 1 .and. ix .le. nxLoad .and.
     &			  iy .ge. 1 .and. iy .le. nyLoad) then
			sum = sum + temp(ix + (iy - 1) * nxLoad)
			nsum = nsum + 1
		      endif
		    enddo
		  enddo
		  array(ixDone + ixb, iyDone + iyb) = sum / nsum
		enddo
		iCheckStrt = iFastEnd + 1
		iCheckEnd = nBinCols
	      enddo
	    enddo	    

	    ixDone = ixDone + nBinCols
	    ixStart = ixStart + nxLoad
	    loadXoffset = 0
	  enddo
	  iyDone  = iyDone + nBinLines
	  iyStart = iyStart + nyLoad
	  loadYoffset = 0
	enddo
	ierr =0
99	return 
	end

****	  SUMDENSITY
c	  
c	  SUMDENSITY will sum the density within contours of selected IMOD 
c	  model objects.  Pixels within a specified distance of the boundary
c	  contour can be excluded.  A threshold can be set so that only pixels
c	  above threshold are summed, and the threshold value subtracted from
c	  the pixel values in the sum.  Data are reported by surface if there
c	  are surfaces defined in the model contours.
c	  
c	  See man page for more details
c       
c	  David Mastronarde 11/10/98
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2004/09/21 22:21:13  mast
c	  Removed a ; for SGI
c	
c	  Revision 3.1  2004/09/10 05:56:49  mast
c	  Initial addition to package
c	
c	  
	implicit none
	integer imsiz,limflag,limsurf,limobj
	parameter (imsiz=4100, limflag = 1024, limobj = 1000, limsurf = 10000)
	real*4 array(imsiz*imsiz)
	integer*4 nxyz(3),mxyz(3),nx,ny,nz
	equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
	character*120 infile,outfile,modelfile
	include 'model.inc'
	common /bigcom/array
	logical readw_or_imod
	integer*4 iSurface(max_obj_num),iobjflag(limflag),iobjdo(limobj)
	integer*4 listsurf(limsurf),numInBorder(limsurf)
	integer*4 numAboveThresh(limsurf),isurf
	real*4 sumAbove(limsurf), sumTemp
        character*120 line
c	  
	integer*4 mode,i,j,nobjdo,iunitOut,numTot,numInTemp,numAboveTemp
	integer*4 iobj,imodobj,imodobj2,imodcont,ifdo,iconThresh,numSurf
	real*4 dmin,dmax,dmean,sumTot,avgAbove,absSum,absThresh
	integer*4 ierr,ierr2,irefObject,ifReverse,ifVerbose,nThreshIn
	integer*4 getimodhead,getimodscales,getimodflags,getimodsurfaces
	real*4 xyscal,zscale,xofs,yofs,zofs,ximscale, yimscale, zimscale
	integer*4 ifflip,getimodobjsize
	real*4 border, polarity
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger,PipGetBoolean
	integer*4 PipGetString,PipGetFloat
	integer*4 PipGetNonOptionArg, PipGetInOutFile, PipPrintHelp
	integer numOptions
	parameter (numOptions = 12)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'image:ImageFile:FN:@output:OutputFile:FN:@'//
     &      'model:ModelFile:FN:@objects:ObjectsToDo:LI:@'//
     &      'border:BorderSize:F:@absolute:AbsoluteThreshold:F:@'//
     &      'contrast:ContrastThreshold:I:@'//
     &      'reference:ReferenceObject:I:@reverse:ReversePolarity:B:@'//
     &      'verbose:VerboseOutput:B:@param:ParameterFile:PF:@'//
     &      'help:usage:B:'
c	  
c	  Set all defaults here
c
	infile = ' '
	outfile = ' '
	modelfile = ' '

	nobjdo=0
	border=0.
	irefObject = -1
	polarity = 1.
	ifReverse = 0
	ifVerbose = 0
c	  
c	  Pip startup: set error, parse options, do help output
c
	call PipReadOrParseOptions(options, numOptions, 'sumdensity',
     &	    'ERROR: SUMDENSITY - ', .false., 2, 2, 1, numOptArg,
     &	    numNonOptArg)
c	    
c	  Get the files; output file is optional
c
	if (PipGetInOutFile('ImageFile', 1, ' ', infile)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
	if (PipGetInOutFile('ModelFile', 2, ' ', modelfile)
     &	    .ne. 0) call errorexit('NO MODEL FILE SPECIFIED')
	ierr = PipGetInOutFile('OutputFile', 3, ' ', outfile)
c	  
c	  open image file
c
	call imopen(1,infile,'old')
	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
c
c	  open model file and get header info
c
	if(.not.readw_or_imod(modelfile))
     &	    call errorexit ('READING MODEL FILE')

	ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	ierr2 = getimodscales(ximscale, yimscale, zimscale)
	if (ierr .ne. 0 .or. ierr2 .ne. 0)
     &	    call errorexit('getting model header')
	if (getimodflags(iobjflag,limflag) .ne. 0)
     &	    call errorexit('getting object types')
	if (getimodsurfaces(iSurface) .ne. 0)
     &	    call errorexit('getting surface numbers')
c	  
c	  Do standard scaling of model to index coords
c
	do i=1,n_point
	  p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
	  p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
	  p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
	enddo
c
	nThreshIn = 0
	if (PipGetFloat('AbsoluteThreshold', absThresh) .eq. 0)
     &	    nThreshIn  = nThreshIn + 1
	if (PipGetInteger('ContrastThreshold', iconThresh) .eq. 0) then
	  nThreshIn  = nThreshIn + 1
	  absThresh = iconThresh * (dmax - dmin) / 255. + dmin
	endif
	if (PipGetInteger('ReferenceObject', irefObject) .eq. 0) then
     	  nThreshIn  = nThreshIn + 1
	  if (irefObject .le. 0 .or. irefObject .gt. getimodobjsize())
     &	      call errorexit(
     &	      'ILLEGAL OBJECT NUMBER FOR REFERENCE OBJECT')
	  if (mod(iobjflag(irefObject), 4) .ne. 0) call errorexit(
     &	      'REFERENCE OBJECT IS NOT A CLOSED CONTOUR OBJECT')
	endif
	if (nThreshIn .eq. 0) call errorexit(
     &	    'A THRESHOLD HAS NOT BEEN SPECIFIED')
	if (nThreshIn .gt. 1) call errorexit(
     &	    'A THRESHOLD WAS SPECIFIED IN MORE THAN ONE WAY')
	
	ierr = PipGetFloat('BorderSize', border)
	ierr = PipGetBoolean('ReversePolarity', ifReverse)
	if (ifReverse .ne. 0) polarity = -1.
	if (PipGetString('ObjectsToDo', line) .eq. 0)
     &	    call parselist(line, iobjdo,nobjdo)
	ierr = PipGetBoolean('VerboseOutput', ifVerbose)
	call PipDone()
c	  
	iunitOut = 6
	if (outfile .ne. ' ') then
	  iunitOut = 7
	  call dopen(7, outfile, 'new', 'f')
	endif
c	  
c	  compute the reference areas if object defined
c
	if (irefObject .gt. 0) then
	  numTot = 0
	  sumTot = 0.
	  do iobj = 1,max_mod_obj
	    if (npt_in_obj(iobj).gt.2 .and. 256-obj_color(2,iobj) .eq.
     &		irefObject) then
	      call sum_inside_contour(array, imsiz, nx, ny, nz, iobj, border,
     &		  dmin - dmax, 1, numInTemp, numAboveTemp, absSum,
     &		  avgAbove)
	      numTot = numTot + numInTemp
	      sumTot = sumTot + absSum
	    endif
	  enddo
	  if (numTot .eq. 0) call errorexit('REFERENCE OBJECT DID'//
     &	      ' NOT HAVE USEFUL CONTOURS FOR GETTING THRESHOLD')
	  absThresh = sumTot / numTot

	  write(*,'(a,i7,a,g15.6)')' Threshold computed from',numTot,
     &	      ' pixels is',absThresh
	endif
c	  
c	  loop on objects
c
	do imodobj = 1, getimodobjsize()
c	    
c	    do object if no list, or on list, and if closed contour
c	    exclude reference object
c
	  ifdo = 1
	  if (nobjdo .gt. 0) then
	    ifdo = 0
	    do i = 1, nobjdo
	      if (iobjdo(i) .eq. imodobj) ifdo = 1
	    enddo
	  endif
	  if (mod(iobjflag(imodobj), 4) .ne. 0 .or. imodobj .eq. irefObject)
     &	      ifdo = 0
c
	  if (ifdo .ne. 0) then
c	      
c	      look for valid contours and make a list of surface numbers
c
	    numSurf = 0
	    do iobj = 1, max_mod_obj
	      if (npt_in_obj(iobj) .gt. 2 .and.
     &		  256 - obj_color(2,iobj) .eq. imodobj) then
		isurf = 0
		do i = 1, numSurf
		  if (iSurface(iobj) .eq. listsurf(i)) isurf = i
		enddo
		if (isurf .eq. 0) then
		  numSurf = numSurf + 1
		  if (numSurf .gt. limsurf) call errorexit
     &		      ('TOO MANY SURFACES FOR ARRAYS')
		  listsurf(numSurf) = iSurface(iobj)
		  numInBorder(numSurf) = 0
		  numAboveThresh(numSurf) = 0
		  sumAbove(numSurf) = 0
		endif
	      endif
	    enddo
c	      
c	      order surface list
c
	    do i = 1, numSurf - 1
	      do j = i + 1, numSurf
		if (listsurf(i) . gt. listsurf(j)) then
		  isurf = listsurf(i)
		  listsurf(i) = listsurf(j)
		  listsurf(j) = isurf
		endif
	      enddo
	    enddo    
c	      
c	      Do contours and add results for surfaces
c	      
	    do iobj = 1, max_mod_obj
	      if (npt_in_obj(iobj) .gt. 2 .and.
     &		  256 - obj_color(2,iobj) .eq. imodobj) then
		call objtocont(iobj,obj_color,imodobj2,imodcont)
		isurf = 0
		do i = 1, numSurf
		  if (iSurface(iobj) .eq. listsurf(i)) isurf = i
		enddo
		call sum_inside_contour(array, imsiz, nx, ny, nz, iobj, border,
     &		    absThresh, polarity, numInTemp, numAboveTemp, absSum,
     &		    avgAbove)
		numInBorder(isurf) = numInBorder(isurf) +numInTemp
		numAboveThresh(isurf) = numAboveThresh(isurf) + numAboveTemp
		sumAbove(isurf) = sumAbove(isurf) + avgAbove * numAboveTemp
		if (ifVerbose .ne. 0) call print_result(iunitOut, mode,
     &		    imodobj, 'cont', imodcont, numInTemp,numAboveTemp,
     &		    avgAbove * numAboveTemp)
 	      endif
	    enddo	    
	    numInTemp = 0
	    numAboveTemp = 0
	    sumTemp = 0.
	    do isurf = 1, numSurf
	      numInTemp = numInTemp + numInBorder(isurf)
	      numAboveTemp = numAboveTemp + numAboveThresh(isurf)
	      sumTemp = sumTemp + sumAbove(isurf)
	      call print_result(iunitOut,mode, imodobj,'surf',listsurf(isurf),
     &		  numInBorder(isurf), numAboveThresh(isurf), sumAbove(isurf))
	    enddo
	    call print_result(iunitOut, mode, imodobj,'ALL ',numSurf,
     &		numInTemp, numAboveTemp, sumTemp)
	  endif
	enddo

	call exit(0)
99	call errorexit('READING FILE')
	end


	subroutine print_result(iunitOut, mode, imodobj, typeText, icont,
     &	    numIn, numAbove, sumAbove)
	implicit none
	integer*4 iunitOut, mode, imodobj, icont, numIn, numAbove
	real*4 sumAbove, avgAbove
	character*(*) typeText
	if (numIn .eq. 0) return
	avgAbove = 0.
	if (numAbove .gt. 0) avgAbove = sumAbove / numAbove
	if (mode .eq. 2) then
	  write(iunitOut, 102)imodobj,typeText, icont, numIn,
     &	      numAbove, (100. * numAbove) / numIn, sumAbove, avgAbove
102	  format(' obj',i5,2x,a,i6,2i10,f7.2,2e15.6)
	else
	  write(iunitOut, 103)imodobj,typeText, icont, numIn,
     &	      numAbove, (100. * numAbove) / numIn, sumAbove, avgAbove
103	  format(' obj',i5,2x,a,i6,2i10,f7.2,f12.0,f10.2)
	endif
	return
	end


c	  SUM_INSIDE_CONTOUR computes the sum of pixels inside a contour,
c	  excluding a border region
c	  Input parameters:
c	  ARRAY is the array for reading image, dimensioned to IMSIZ**2
c	  NX, NY, Nz are dimensionf of the file
c	  IOBJ is the "object" number
c	  BORDER is the size of the exclusion zone
c	  ABSTHRESH is the threshold for counting pixels
c	  POLARITY is 1 to count bright pixels, or -1 for dark pixels
c	  Returned parameters:
c	  numInBorder  is the number of pixels inside the borders
c	  numAboveThresh is the number of pixels above threshold
c	  absSum is the absolute sum of the pixels above threshold
c	  avgAbove is the average of the those pixels after subtracting
c	  the threshold
c
	subroutine sum_inside_contour(array, imsiz, nx, ny, nz, iobj, border,
     &	    absThresh, polarity, numInBorder, numAboveThresh, absSum,
     &	    avgAbove)
	implicit none
	integer limpts
	parameter (limpts=10000)
	integer*4 imsiz, iobj, numInBorder, numAboveThresh, nx, ny, nz
	real*4 array(*), absThresh, absSum, avgAbove, border,polarity
	real*4 cx(limpts),cy(limpts),sumAbove
	include 'model.inc'
	integer*4 ibase, ix, iy, iz, ipt, i, ixst, ixnd, iyst, iynd, nxload
	integer*4 nyload, ninobj, ilas
	logical inside, interior
	real*4 xmin, xmax, ymin, ymax, dx, dy, xpix, ypix, t, val, borderSq

	ninobj = npt_in_obj(iobj)
	if (ninobj .gt. limpts) call errorexit(
     &	    'TOO MANY POINTS IN CONTOURS FOR ARRAYS')
	borderSq = border**2
	numInBorder = 0
	numAboveThresh = 0
	absSum = 0.
	avgAbove = 0.
c	  
c	  copy contour into arrays and get min/max
c
	xmin = 10000000.
	xmax = -xmin
	ymin = xmin
	ymax = xmax
	ibase = ibase_obj(iobj)
	iz = nint(p_coord(3, object(ibase + 1)))
	if (iz .lt. 0 .or. iz .ge. nz) call errorexit
     &	    ('CONTOUR Z VALUE OUT OF RANGE OF IMAGE FILE')
	do i = 1, ninobj
	  ipt = object(ibase + i)
	  cx(i) = p_coord(1, ipt)
	  cy(i) = p_coord(2, ipt)
	  xmin = min(xmin, cx(i))
	  xmax = max(xmax, cx(i))
	  ymin = min(ymin, cy(i))
	  ymax = max(ymax, cy(i))
	enddo
c	  
c	  adjust min/max to be inside borders of image enough for expansion
c
	xmin = max(xmin, 1.)
	xmax = min(xmax, nx - 3.)
	ymin = max(ymin, 1.)
	ymax = min(ymax, ny - 3.)
	if (xmin.ge.xmax .or. ymin.ge.ymax) return
c	  
c	  get boundaries of load, a bit bigger
c
	ixst = nint(xmin - 1.)
	ixnd = nint(xmax + 2.)
	iyst = nint(ymin - 1.)
	iynd = nint(ymax + 2.)
	nxload = ixnd + 1 - ixst
	nyload = iynd + 1 - iyst
	if (nxload * nyload .gt. imsiz**2) call
     &	    errorexit('CONTOUR TOO BIG FOR IMAGE ARRAY')
c	  
c	  load image
c
	call imposn(1, iz, 0)
	call irdpas(1, array, nxload, nyload, ixst, ixnd, iyst, iynd, *99)
c	  
c	  loop on pixels.  Use coordinates of pixel centers
c
	do iy = 0, nyload - 1
	  do ix = 0, nxload - 1
	    xpix = ixst + 0.5 + ix
	    ypix = iyst + 0.5 + iy
c	      
c	      test if inside the contour
c
	    if (inside(cx, cy, ninobj, xpix, ypix)) then
	      ilas = ninobj
	      i = 1
	      interior = .true.
c		
c		compute distance from each line segment of contour and make
c		sure pixel is not in the border region
c
	      do while (i .le. ninobj .and. interior)
		dx = cx(i) - cx(ilas)
		dy = cy(i) - cy(ilas)
		t = 0.
		if (dx .ne. 0. .or. dy .ne. 0.) t = max(0., min(1.,
     &		    ((xpix - cx(ilas)) * dx + (ypix - cy(ilas)) * dy) /
     &		    (dx**2 + dy**2)))
		dx = xpix - (cx(ilas) + t * dx)
		dy = ypix - (cy(ilas) + t * dy)
		interior = dx**2 + dy**2 .gt. borderSq
		ilas = i
		i = i + 1
	      enddo
c		
c		If pixel is in interior, count it, and see if it is above the
c		threshold, if so add to sums
c
	      if (interior) then
		numInBorder = numInBorder + 1
		val = array(1 + ix + iy * nxload)
		if (polarity * (val - absThresh) .gt. 0) then
		  numAboveThresh = numAboveThresh +1
		  absSum = absSum + val
		  sumAbove = sumAbove + polarity * (val - absThresh)

		endif
	      endif
	    endif
	  enddo
	enddo

	if (numAboveThresh .gt. 0) avgAbove = sumAbove / numAboveThresh

	return
99	call errorexit('READING IMAGE FILE')
	end




	logical function typeonlist(itype,ityplist,ntyplist)
	implicit none
	integer*4 ityplist(*),itype,ntyplist,i
	typeonlist=.true.
	if(ntyplist.eq.1.and.ityplist(1).eq.-999)return
	do i=1,ntyplist
	  if(itype.eq.ityplist(i))return
	enddo
	typeonlist=.false.
	return
	end
	
	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: SUMDENSITY - ',message
	call exit(1)
	end


c	  
	implicit none
	integer maxImDim, maxGrids, iGridDim, msiz, maxVars, matLim
	integer maxData, lenRwrk, lenIwrk, maxSect
	parameter (maxImDim = 2100)
	parameter (maxGrids = 20, maxSect = 200)
	parameter (iGridDim = 256)
	parameter (msiz = 800)
	parameter (maxVars = iGridDim * iGridDim + 50)
	parameter (matLim = maxImDim * maxImDim / msiz)
	parameter (maxData = maxImDim * maxImDim / 22)
	parameter (lenRwrk = maxData * 9)
	parameter (lenIwrk = maxData * 10 + 10)
	integer*4 nxyz(3),mxyz(3),nx,ny,nz
	equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
	character*120 infile,tempfile, strFile
	character*320 comString
	character*12 endvStr, ypadStr
	character*80 concat,outRoot,outfile,xffile, stFile, patchFile, quietStr
	logical exist

	real*4 array(maxImDim * maxImDim), brray(maxImDim * maxImDim)
	common /bigarr/array, brray

	integer*4 nxgrid(maxGrids),nygrid(maxGrids)
     &	    ,iGridStrt(2,maxGrids)
     &	    ,iGridOfs(2,maxGrids)
	real*4 dxGrid(iGridDim,iGridDim,maxGrids)
     &	    ,dyGrid(iGridDim,iGridDim,maxGrids)
     &	    ,ddenGrid(iGridDim,iGridDim),sdGrid(iGridDim,iGridDim)
	real*4 fieldDx(iGridDim, iGridDim), fieldDy(iGridDim, iGridDim)
	real*4 lastFieldX(iGridDim, iGridDim), lastFieldY(iGridDim, iGridDim)
	equivalence (fieldDx, ddenGrid), (fieldDy, sdGrid)
	integer*4 ixydispl(2), noverlap(2), intGridXY(2), iboxXY(2),indentXY(2)
	integer*4 listPairs(maxGrids * 2)
	real*4 delXY(2), globDelXY(2, maxGrids), fracDelXY(2, maxGrids)
	integer*4 integDelXY(2, maxGrids)
	integer*4 iFieldStrt(2), nPtField(2), nPtData(2, maxGrids)
	real*4 fieldIntrv(2), dataIntrv(2, maxGrids)
	real*4 xform(2, 3, maxSect)
c	  
c	  arrays for matrix inversion regression, reuse big array for data
c
	real*4 x(msiz, matLim)
	real*8 sx(msiz), xm(msiz), sd(msiz), ss(msiz,msiz) , b(msiz), b1(msiz)
	equivalence (x, array)
c	  
c	  working arrays, reuse brray, and set up convenient access to the
c	  components of iwrk, ia and ja arrays
c
	real*4 rwrk(lenRwrk)
	integer*4 iwrk(lenIwrk), ja(lenRwrk), ia(maxData + 1)
	equivalence (iwrk, brray), (ja, brray(maxData + 3)), (ia, brray(2))
	equivalence (rwrk, brray(lenIwrk + 10))
c
	double precision uu(maxData), vv(maxVars), ww(maxVars), xx(maxVars)
	external sparseProd
	double precision anorm, acond, rnorm, arnorm, xnorm, se(4)
	double precision atol, btol, conlim, bfac
	integer*4 itnlim, istop, itndone
c	  
	real*4 shiftXY(2, maxData)
	real*4 solMultr(maxVars, 2)
	real*4 solLsqr(maxVars, 2)
	real*4 sumEntries(maxVars)
	real*4 valRow(maxVars)
	integer*4 icolRow(maxVars)
c
	integer*4 intGrid, mode, iboxsize, indent, izRef, izShift, nxfRead
	integer*4 izLower, izUpper, i, ixy, lenYpad, lenEndv, ierr, iPair
	integer*4 intField, intData, nPairList, numPairs, j, iAngle
	real*4 dmin, dmax, dmean, dum
	logical doMultr, done, useOldXf, saveXfs, makePatch
	integer*4 numVars, numRows, numTotField, iter, numIter, numTied
	integer*4 ix, iy, ixd, iyd, numInRow, idist, minDist, icol, numNeigh
	real*4 xData, yData, xShift, yShift, xGrid, yGrid, errsum, error
	integer*4 icolNeigh(10), intscan, nfitSmooth, nSkipSmooth,norderSmooth
	real*4 radius2, sigma1, sigma2, sdcritSmooth, devcritSmooth
	real*4 sourceTol, const, rsq, fval, gridIntrv, colSumCrit, rsq1
	real*4 cosang, sinang, axisPos, sum, errorCrit, relax
	real*4 val,  aa(2, 3), xOut, yOut, errorLast(maxGrids, 2), relaxInitial
	real*4 bb(2, 3), dmag, theta, smag, str, phi, smagMean, alpha
	real*4 ssqr, solSsqr, solNeg, solPos, pixelSize, resSum, resMax
	real*4 fieldChange
	integer*4 iBinning, maxZ
	real*4 cosd, sind, acosd, asind
	integer*4 lnblnk

	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger,PipGetBoolean
	integer*4 PipGetString,PipGetFloat, PipGetTwoIntegers
	integer*4 PipGetInOutFile, PipGetLogical
	integer numOptions
	parameter (numOptions = 16)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@'//
     &      'output:OutputRoot:FN:@'//
     &	    'pairs:PairsToAnalyze:LI:@'//
     &	    'field:FieldSpacing:I:@'//
     &	    'data:DataSpacing:I:@'//
     &	    'grid:GridSpacing:I:@'//
     &	    'box:BoxSize:I:@'//
     &	    'indent:GridIndent:I:@'//
     &	    'iterations:Iterations:I:@'//
     &	    'multr:SolveWithMultr:B:@'//
     &	    'usexf:UseOldTransforms:B:@'//
     &	    'redirect:RedirectOutput:CH:@'//
     &	    'strfile:StretchFile:FN:@'//
     &	    'patch:PatchOutput:B:@'//
     &	    'binning:ImageBinning:I:@'//
     &	    'help:usage:B:'
c	  
c	  set defaults
c	  
	iboxsize = 24
	indent = 4
	intgrid = 16
	radius2 = 0.25
	sigma1 = 0.03
	sigma2 = 0.25
	intdata = 20
	intfield = 40
	intscan = 6
	sdcritSmooth=2.
	devcritSmooth=2.
	norderSmooth=2
	nfitSmooth = 5
	nSkipSmooth = 1
	doMultr = .false.
	numIter = 10
	errorCrit = 1.e-4
	sourceTol = 0.1
	colSumCrit = 0.2
	relaxInitial = 0.67
	atol = 0.
	btol = 0.01
	conlim = 1.e7
	useOldXf = .false.
	xffile = ' '
	strFile = ' '
	makePatch = .false.
	iBinning = 2
	fieldChange = 100000.
	quietStr = '> /dev/null'

	call PipReadOrParseOptions(options, numOptions, 'finddistort',
     &	    'ERROR: FINDDISTORT - ', .false., 2, 1, 1, numOptArg,
     &	    numNonOptArg)

	if (PipGetInOutFile('InputFile', 1, ' ', infile)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
	if (PipGetInOutFile('OutputRoot', 2, ' ', outRoot)
     &	    .ne. 0) call errorexit('NO OUTPUT ROOT NAME SPECIFIED')

        CALL IMOPEN(1,INFILE,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
	IF (NX * NY .GT. maxImDim * maxImDim) call errorexit(
     &	    'IMAGE TOO LARGE FOR ARRAYS')

	if (nz .gt. maxGrids * 2) call errorexit(
     &	    'TOO MANY IMAGES FOR ARRAYS')
	call irtdel(1, valRow)
	pixelSize = valRow(1)
c	  
c	  Set up default pair list
c
	do i = 1, nz
	  listPairs(i) = i - 1
	enddo
	nPairList = 2 * (nz / 2)
c	ierr = PipGetString('TransformFile', xffile)
	ierr = PipGetLogical('UseOldTransforms', useOldXf)
	saveXfs = .not.useOldXf
c	saveXfs = xffile .ne. ' ' .and. .not.useOldXf
c	if (useOldXf .and. xffile .eq. ' ') call errorexit(
c     &	    'TRANSFORM FILE MUST BE SPECIFIED TO USE OLD TRANSFORMS')
c	  
c	  get pairs and check their validity
c
	if (PipGetString('PairsToAnalyze', comString) .eq. 0)
     &	    call parselist(comString, listPairs, nPairList)
	numPairs = nPairList / 2
	if (numPairs .gt. maxGrids) call errorexit(
     &	    'TOO MANY PAIRS OF IMAGES FOR ARRAYS')
	maxZ = 0
	do i = 1, numPairs * 2
	  if (listPairs(i) .lt. 0 .or. listPairs(i) .gt. nz - 1)
     &	      call errorexit('SECTION NUMBER OUT OF RANGE')
	  if (mod(i, 2) .eq. 1 .and. listPairs(i + 1) .ne. listPairs(i) + 1)
     &	      call errorexit(
     &	      'FOR NOW, PAIRS MUST BE ADJACENT SECTIONS IN FILE')
	  maxZ = max(maxZ, listPairs(i))
	enddo

	ierr = PipGetString('StretchFile', strFile)
	ierr = PipGetLogical('PatchOutput', makePatch)
	ierr = PipGetInteger('FieldSpacing', intField)
	ierr = PipGetInteger('GridIndent', indent)
	ierr = PipGetInteger('GridSpacing', intGrid)
	ierr = PipGetInteger('DataSpacing', intData)
	ierr = PipGetInteger('Iterations', numIter)
	ierr = PipGetLogical('SolveWithMultr', doMultr)
	ierr = PipGetInteger('ImageBinning', iBinning)
	ierr = PipGetInteger('BoxSize', iBoxsize)
	ierr = PipGetString('RedirectOutput', quietStr)
	call PipDone()

c	tempfile = temp_filename(infile, ' ', 'xf')
	tempfile = concat(outRoot, '.tmpxf')
	xffile = concat(outRoot, '.rawxf')
	outfile = concat(outRoot, '.nosidf')
	call int_iwrite(ypadStr, ny / 2, lenYpad)
	if (maxSect .lt. nz) call errorexit(
     &	    'TOO MANY SECTIONS FOR TRANSFORM ARRAY')

	iter = 1
	done = .false.
	do while ( .not.done)
c	  
c	  set up transforms and get from file if using old ones
c
	  do i = 1, nz
	    call xfunit(xform(1, 1, i), 1.)
	  enddo
	  if (useOldXf) then
	    call dopen(3, xffile, 'ro', 'f')
	    call xfrdall(3, xform, nxfRead, *98)
	    if (nxfRead .gt. maxSect) call errorexit(
     &		'TOO MANY TRANSFORMS IN FILE FOR ARRAY')
	    close(3)
	  endif
c	    
c	    undistort images after the first time
c	    
	  stFile = infile
	  if (iter .gt. 1) then
	    stFile = concat(outRoot, '.udst')
	    call int_iwrite(endvStr, maxZ, lenEndv)
	    write(comString, 102)'newstack -sec 0-', endvStr(1:lenEndv),
     &		' -image ', iBinning, ' -dist ', outfile(1:lnblnk(outfile)), 
     &		infile(1:lnblnk(infile)), stFile(1:lnblnk(stFile)),
     &		quietStr(1:lnblnk(quietStr))
102	    format(a,a,a,i3,a,a,1x,a,1x,a,1x,a)
c	    print *,comString
c	    
c		check for result
c	    
	      call system(comString)
	      inquire(file=stFile,exist=exist)
	      if (.not.exist) call errorexit(
     &		  'NEWSTACK FAILED TO MAKE UNDISTORTED STACK')
	  endif

	  errsum = 0.
	  do iPair = 1, numPairs
c	    
c	      make the tiltxcorr command string
c
	    izShift = listPairs(2 * iPair)
	    izRef = listPairs(2 * iPair - 1)
	    if (useOldXf) then
	      delXY(1) = xform(1, 3, izShift + 1)
	      delXY(2) = xform(2, 3, izShift + 1)
	    else
	      call int_iwrite(endvStr, izShift + 1, lenEndv)
	      write(comString, 101)
     &		  'tiltxcorr -first 0. -inc 0. -rot 0. -radius2 ', radius2,
     &		  ' -sigma1 ', sigma1, ' -sigma2 ', sigma2, ' -pad ',
     &		  nx / 2, ',', ypadStr(1:lenYpad), ' -views ',
     &		  izRef + 1, ',', endvStr(1:lenEndv),
     &		  stFile(1:lnblnk(stFile)), tempfile(1:lnblnk(tempfile)),
     &		  quietStr(1:lnblnk(quietStr))
101	      format(a,f5.2,a,f5.2,a,f5.2,a,i5,a,a,a,i4,a,a,1x,a,1x,a,1x,a)

c	    print *,comString
c	    
c		check for and get result
c	    
	      call system(comString)
	      inquire(file=tempfile,exist=exist)
	      if (.not.exist) call errorexit(
     &		  'TILTXCORR FAILED TO MAKE TRANSFORM FILE')
	    
	      open(3, file=tempfile, status='OLD', form='FORMATTED')
	
	      do i = 1, izShift + 1
		read(3, *) dum, dum, dum, dum, delXY(1), delXY(2)
	      enddo
	      close(3)
	      if (saveXfs) then
		xform(1, 3, izShift + 1) = delXY(1)
		xform(2, 3, izShift + 1) = delXY(2)
	      endif
	    endif
c
c	      determine grid parameters, make the "short" dimension be the
c	      one with less overlap
c	      swap the images and invert deltas to make upper and lower fit
c	      the model of upper to right or above
c	      
	    ixy = 1
	    if (abs(delXY(1)) .lt. abs(delXY(2))) ixy = 2
	    
	    if (delXY(ixy) .ge. 0.) then
	      izLower = izRef
	      izUpper = izShift
	    else
	      izLower = izShift
	      izUpper = izRef
	      delXY(1) = -delXY(1)
	      delXY(2) = -delXY(2)
	    endif

	    if (iter. eq. 1) then
	      print *,'pair',izLower, izUpper,', deltas',delXY(1), delXY(2)
c	    
c		set overlap in short dimension, set displacement (error) zero
c		in that dimension, and use measured displacement in long
c		dimension 
c
	      noverlap(ixy) = nxyz(ixy) - nint(delXY(ixy))
	      ixydispl(ixy) = 0
	      ixydispl(3 - ixy) = nint(delXY(3 - ixy))
	      
	      do i = 1, 2
		indentXY(i) = indent
		intgridXY(i) = intgrid
		iboxXY(i) = iboxsize
	      enddo

	      call setgridchars(nxyz, noverlap, iboxXY, indentXY, intgridXY,
     &		  ixy, ixydispl(1), ixydispl(2), nxGrid(iPair), 
     &		  nyGrid(iPair), iGridStrt(1, iPair), iGridOfs(1, iPair))
c	  print *,'Grid start',iGridStrt(1, iPair), iGridStrt(2, iPair),
c     &	      '  offset', iGridOfs(1, iPair), iGridOfs(2, iPair)

c	      
c		Set up the sampling grid for this pair
c	      
	      nPtData(1, iPair) = ((nxGrid(iPair) - 1) * intGrid + intData - 1)
     &		/ intData + 1
	      dataIntrv(1, iPair) = ((nxGrid(iPair) - 1.) * intGrid) /
     &		  nPtData(1, iPair)
	      nPtData(2, iPair) = ((nyGrid(iPair) - 1) * intGrid + intData - 1)
     &		  / intData + 1
	      dataIntrv(2, iPair) = ((nyGrid(iPair) - 1.) * intGrid) /
     &		  nPtData(2, iPair)
c	    
c		read in the sections
c		
	      call imposn(1, izLower, 0)
	      call irdsec(1, array, *99)
	      call imposn(1, izUpper, 0)
	      call irdsec(1, brray, *99)
c	    
c		get and smooth the edge function
c		
	      call findedgefunc(array,brray,nx,ny, iGridStrt(1, iPair),
     &		  iGridStrt(2, iPair),iGridOfs(1, iPair),iGridOfs(2, iPair),
     &		  nxGrid(iPair),nyGrid(iPair), intGrid,intGrid,iboxsize,
     &		  iboxsize,intscan, dxGrid(1,1,iPair), dyGrid(1,1,iPair),
     &		  sdgrid, ddengrid, iGridDim, iGridDim)
c	    
c	  print *,'edge function', iPair
c	  do j = 1, nyGrid(iPair)
c	    write(*, '(6(f7.1,f6.1))')(dxGrid(i,j,iPair),dyGrid(i,j,iPair),
c     &		i = 1, nxGrid(iPair))
c	  enddo
c
	      call smoothgrid(dxGrid(1,1,iPair), dyGrid(1,1,iPair), sdgrid,
     &		  ddengrid, iGridDim, iGridDim, nxGrid(iPair),
     &		  nyGrid(iPair), sdcritSmooth, devcritSmooth, nfitSmooth,
     &		  nfitSmooth, norderSmooth, nskipSmooth,nskipSmooth)
c	    
c	  print *,'smoothed', iPair
c	  do j = 1, nyGrid(iPair)
c	    write(*, '(6(f7.1,f6.1))')(dxGrid(i,j,iPair),dyGrid(i,j,iPair),
c     &		i = 1, nxGrid(iPair))
c	  enddo
c
	    else
c	      
c		after first time, get shift from previous time and sum the
c		change
c		
	      shiftXY(1, iPair) = delXY(1) - globDelXY(1, iPair)
	      shiftXY(2, iPair) = delXY(2) - globDelXY(2, iPair)
	      errsum = errsum + shiftXY(1, iPair)**2 + shiftXY(2, iPair)**2
	    endif
c	    
c	      save the starting global offset, its integer and fractional
c	      components
c	      
	    do ixy = 1 ,2
	      globDelXY(ixy, iPair) = delXY(ixy)
	      integDelXY(ixy, iPair) = iGridStrt(ixy, iPair) -
     &		  iGridOfs(ixy, iPair)
	      fracDelXY(ixy, iPair) = delXY(ixy) - integDelXY(ixy, iPair)
	    enddo
	  enddo
c	    
c	    save transforms if flag set
c
	  if (saveXfs) then
	    call dopen(3, xffile, 'new', 'f')
	    do i = 1, nz
	      call xfwrite(3, xform(1,1, i), *97)
	    enddo  
	    close(3)
	  endif
c	    
c	    Fix some things the first time through
c
	  if (iter .eq. 1) then
c	    
c	      set up not to use old ones and save them under a different name
c
	    saveXfs = .true.
	    useOldXf = .false.
	    xffile = concat(outRoot, '.udxf')

c
c	      The edge function gives the amount that a pixel moves when going
c	      from lower to upper piece, so it would have same sign as df in
c	      the upper piece.  The coordinates are the center of the box in
c	      lower  piece.
c	  
c	      Set up the grid for the distortion field.  The minimum
c	      indentation is the maximum of half the grid spacing and the
c	      indents for the edge function, which are the minimum of the
c	      start and offset values 
c
	    do ixy = 1, 2
	      iFieldStrt(ixy) = intField / 2
	      do  iPair = 1, numPairs
		iFieldStrt(ixy) = max(iFieldStrt(ixy),
     &		    min(iGridStrt(ixy, iPair), iGridOfs(ixy, iPair)))
	      enddo
	      nPtField(ixy) = (nxyz(ixy) - 2 * iFieldStrt(ixy) + intField - 1)
     &		  / intField + 1
	      fieldIntrv(ixy) = (nxyz(ixy) - 2. * iFieldStrt(ixy)) /
     &		  (nPtField(ixy) - 1.)
	    enddo
	  
	    numTotField = nPtField(1) * nPtField(2)
c	      numVars = numTotField + numPairs
	    numVars = numTotField
	    if (nPtField(1) .gt. iGridDim .or. nPtField(2) .gt. iGridDim .or.
     &		numVars .gt. maxVars) call errorexit(
     &		'TOO MANY VARIABLES TO SOLVE FOR TO FIT IN ARRAYS')
	    if (doMultr .and. numVars .ge. msiz) call errorexit(
     &		'TOO MANY VARIABLES TO DO MULTR SOLUTION')
	    print *,'Solving for',nPtField(1),' by', nPtField(2),' =',
     &		numTotField, ' field positions'
c	    
c	      initialize distortion field
c
	    do j = 1, nPtField(2)
	      do i = 1, nPtField(1)
		fieldDx(i, j) = 0.
		fieldDy(i, j) = 0.
	      enddo	
	    enddo
	  else
	    error = sqrt(errsum / numPairs)
	    write(*,'(a, (10f7.2))')'Shift changes:',
     &		((shiftXY(ixy,i),ixy=1,2),i=1,numPairs)
	    print *,'iteration', iter, '   mean change in shift', error
	  endif
c
	  ia(1) = 1
	  iwrk(1) = maxData + 2
	  gridIntrv = intGrid
c	    
c	    load the data array
c
	  numRows = 0
	  do i = 1, numVars
	    sumEntries(i) = 0.
	  enddo
	  do iPair = 1, numPairs
	    do ixd = 1, nPtData(1, iPair)
	      xData = iGridStrt(1, iPair) + (ixd - 1) * dataIntrv(1, iPair)
	      do iyd = 1, nPtData(2, iPair)
		yData = iGridStrt(2, iPair) + (iyd - 1) * dataIntrv(2, iPair)
		numInRow = 0
c		  
c		  get the shift values and adjust them by adding the fractional
c		  shift values
c		  
		call interpolateGrid(xData, yData, dxGrid(1,1,iPair),
     &		    dyGrid(1,1,iPair), iGridDim, nxGrid(iPair),
     &		    nyGrid(iPair), iGridStrt(1, iPair), gridIntrv,
     &		    iGridStrt(2, iPair), gridIntrv, xShift, yShift)
		xShift = xShift + fracDelXY(1, iPair)
		yShift = yShift + fracDelXY(2, iPair)
c		print *,'at',xData,yData,' shift=',xShift,yShift
c		  
c		  Add the coefficients for the position in the lower piece
c
		call findCoefficients(xData, yData, fieldDx, fieldDy, iGridDim,
     &		    iGridDim, nPtField, iFieldStrt, fieldIntrv, -1., valRow,
     &		    icolRow, numInRow, sourceTol)
c		  
c		  Get position in upper piece, add negative coefficients
c		  
		call findCoefficients(xData - globDelXY(1, iPair),
     &		    yData - globDelXY(2, iPair), fieldDx, fieldDy, iGridDim,
     &		    iGridDim, nPtField, iFieldStrt, fieldIntrv, 1., valRow,
     &		    icolRow, numInRow, sourceTol)
c		  
c		  Add dummy variable for the pair and add the data row for
c		  x shift
c		  
c		call addValueToRow(1., numTotField + iPair,
c     &		    valRow, icolRow, numInRow)
		call addDataRow(valRow, icolRow, numInRow, xShift, yShift,
     &		    rwrk, ia, ja, numRows, shiftXY, maxData, sumEntries,
     &		    1, x, msiz, matLim, numVars, doMultr)
	      enddo
	    enddo
	  enddo
c	    
c	    Add row to make the mean field be zero
c	    
	  do i = 1, numTotField
	    valRow(i) = 1.
	    icolRow(i) = i
	  enddo
	  call addDataRow(valRow, icolRow, numTotField, 0., 0., 
     &	      rwrk, ia, ja, numRows, shiftXY, maxData, sumEntries,
     &	      0, x, msiz, matLim, numVars, doMultr)
c
c	    Data are loaded.
c	    Scan for unused variables and tie them to nearest neighbors
c	    
	  numTied = 0
	  do ix = 1, nPtField(1)
	    do iy = 1, nPtField(2)
	      icol = ix + (iy - 1) * nPtField(1)
	      if (sumEntries(icol) .lt. colSumCrit) then
c		  
c		  If variable has no data, start a row with -1 for it
c		  
		numInRow = 0
		call addValueToRow(-1., icol, valRow, icolRow, numInRow)
		minDist = 100000
		do ixd = -4, 4
		  do iyd = -4, 4
		    icol = ix + ixd + (iy + iyd - 1) * nPtField(1)
		    idist = ixd**2 + iyd**2
c		   
c		      add to list of nearest neighbors, flush list if a new
c		      minimum distance is found
c
		    if (sumEntries(icol) .ge. colSumCrit .and.
     &			idist .le. minDist) then
		      if (idist .lt. minDist) numNeigh = 0
		      numNeigh = numNeigh + 1
		      icolNeigh(numNeigh) = icol
		      minDist = idist
		    endif
		  enddo
		enddo
		if (numNeigh .eq. 0) call errorexit(
     &		    'NO NEAR NEIGHBORS WITH DATA FOR A POINT IN FIELD')
c		  
c		  set up coefficients to make the orphan variable be the
c		  average of the neighbors
c
		do i = 1, numNeigh
		  call addValueToRow(1. / numNeigh, icolNeigh(i),
     &		      valRow, icolRow, numInRow)
		enddo
		call addDataRow(valRow, icolRow, numInRow, 0., 0.,
     &		    rwrk, ia, ja, numRows, shiftXY, maxData, sumEntries,
     &		    0, x, msiz, matLim, numVars, doMultr)
		do i = 1, numInRow
		  iColRow(i) = iColRow(i) + 1
		enddo
		call addDataRow(valRow, icolRow, numInRow, 0., 0.,
     &		    rwrk, ia, ja, numRows, shiftXY, maxData, sumEntries,
     &		    0, x, msiz, matLim, numVars, doMultr)
		numTied = numTied + 1
	      endif
	    enddo
	  enddo
	  if (numTied .gt. 0)print *,
     &	      'Data loaded,',numTied,' variables tied to others'
	  print *,numRows,' rows of data,',ia(numRows + 1) -1,' entries'
c	      
c	      Normalize the sparse matrix data
c	      
	  do icol = 1, numVars
	    sum = 0.
	    do j = 1, ia(numRows + 1) - 1
	      if (ja(j) .eq. icol) sum = sum + rwrk(j)**2
	    enddo
	    sum = sqrt(sum)
	    sumEntries(icol) = sum
	    do j = 1, ia(numRows + 1) - 1
	      if (ja(j) .eq. icol) rwrk(j) = rwrk(j) / sum
	    enddo
	  enddo
c		
c	    
c	    solve equations for X then Y shifts 
c
	  if (.not.doMultr) then
	    do ixy = 1, 2
	      rsq1 = acond
	      do i = 1, numRows
		uu(i) = shiftXY(ixy, i)
	      enddo
	      itnlim = numVars
	      call lsqr(numRows, numVars, sparseProd, 0., .false., lenIwrk,
     &		  lenRwrk, iwrk, rwrk, uu, vv, ww, xx, se, atol, btol, conlim,
     &		  itnlim, -1, istop, itndone, anorm, acond, rnorm,
     &		  arnorm, xnorm)
	      
	      do i = 1, numVars
		solLsqr(i, ixy) = xx(i) / sumEntries(i)
	      enddo
	    enddo
	    print *,'condition #',rsq1,acond
	  endif
c	      
	  if (doMultr) then
	    do ixy = 1, 2
c		
	      rsq1 = rsq
	      do i = 1, numRows
		x(numVars + 1, i) = shiftXY(ixy, i)
c		if(ixy.eq.1)  write(*,'(8f9.5)')(x(j,i),j=1,numVars)
c		if (ixy.eq.1) write(*,'(f12.5)')shiftXY(ixy, i)
	      enddo
	      call multrd(x, msiz, numVars + 1, numRows, sx, ss, xm,
     &		  sd, b, b1, rsq, fval)
	      do i = 1, numVars
		solMultr(i, ixy) = b1(i)
		solLsqr(i, ixy) = b1(i)
	      enddo
	    enddo
	    write(*,'(a,2f9.6)')' Rsq in multr: ',rsq1, rsq
	  endif
c	    
c	    get mean and max residual
c	    
	  resSum = 0.
	  resMax = 0.
	  do i = 1, numRows
	    rsq = 0.
	    do ixy = 1, 2
	      sum = 0.
	      do j = ia(i), ia(i + 1) - 1
		icol = ja(j)
		sum = sum + solLsqr(icol, ixy) * rwrk(j) * sumEntries(icol)
	      enddo
	      rsq = rsq + (sum - shiftXY(ixy, i))**2
	    enddo
	    rsq = sqrt(rsq)
	    resSum = resSum +rsq
	    resMax = max(resMax, rsq)
	  enddo
	  write(*,'(a,2f7.3)')' Mean and max residual in fit: ',
     &	      resSum / numRows, resMax
	  
c	    
c	    fill distortion field grid with solution
c	    
	  resSum = 0.
	  resMax = 0.
	  do ix = 1, nPtField(1)
	    do iy = 1, nPtField(2)
	      icol = ix + (iy - 1) * nPtField(1)
	      lastFieldX(ix, iy) = fieldDx(ix, iy)
	      lastFieldY(ix, iy) = fieldDy(ix, iy)
	      fieldDx(ix, iy) = solLsqr(icol, 1)
	      fieldDy(ix, iy) = solLsqr(icol, 2)
	    enddo	  
	  enddo
c	  
c	    unstretch the field by solving for stretch
c
	  numRows = 0
	  do ix = 1, nPtField(1)
	    do iy = 1, nPtField(2)
	      numRows = numRows + 1
	      xData = iFieldStrt(1) + (ix - 1) * fieldIntrv(1) - nx / 2.
	      yData = iFieldStrt(2) + (iy - 1) * fieldIntrv(2) - ny / 2.
	      x(1, numRows) = xData + fieldDx(ix, iy)
	      x(2, numRows) = yData + fieldDy(ix, iy)
	      x(4, numRows) = xData
	      x(5, numRows) = yData
	    enddo
	  enddo
	  do ixy = 1, 2
	    do i = 1, numRows
	      x(3, i) = x(3 + ixy, i)
	    enddo
	    call multrd(x, msiz, 3, numRows, sx, ss, xm,
     &		sd, b, b1, rsq, fval)
	    aa(ixy, 1) = b1(1)
	    aa(ixy, 2) = b1(2)
	    aa(ixy, 3) = 0.
	  enddo
	  
	  done = iter. ge. numIter .or. (iter .gt. 1 .and.
     &	      error .le. errorCrit .and. fieldChange .le. errorCrit)
	  iter = iter + 1

	  if (.not.done) then
c	      
c	      Transform the vectors
c	      
	    j = 0
	    do ix = 1, nPtField(1)
	      do iy = 1, nPtField(2)
		j = j + 1
		fieldDx(ix, iy) = aa(1,1)*x(1, j) + aa(1,2) * x(2, j) - x(4, j)
		fieldDy(ix, iy) = aa(2,1)*x(1, j) + aa(2,2) * x(2, j) - x(5, j)
		rsq = sqrt((fieldDx(ix, iy) - lastFieldX(ix, iy))**2 +
     &		    (fieldDy(ix, iy) - lastFieldY(ix, iy))**2)
		resSum = resSum + rsq
		resMax = max(resMax, rsq)
	      enddo
	    enddo
	    fieldChange = resSum / numTotField
	    write(*,'(a,2f9.4)')' Mean and max change in field: ',
     &	      fieldChange, resMax
c	  
c	      write the no stretch idf file
c	      
	    call dopen(1, outfile, 'new', 'f')
	    write(1,'(i5)')1
	    write(1,'(2i7,i3,f10.4)')nx, ny, iBinning, pixelSize
	    write(1,'(2(i6,f8.2,i6))')(iFieldStrt(i), fieldIntrv(i),
     &		nPtField(i), i = 1,2)
	    do iy = 1, nPtField(2)
	      write(1,'(8f8.2)')(fieldDx(ix, iy), fieldDy(ix, iy), ix = 1,
     &		  nPtField(1))
	    enddo
	    close(1)
	  endif
	enddo

	outfile = concat(outRoot, '.idf')
	write(*,'(a,4f10.6)')' De-stretch transformation:',
     &	    ((aa(i,j),j=1,2),i=1,2)

	if (strFile .ne. ' ') then
	  call dopen(1, strFile, 'ro', 'f')
	  call xfrdall(1, xform, nxfRead, *98)
	  if (nxfRead .gt. maxSect) call errorexit(
     &	      'TOO MANY TRANSFORMS IN FILE FOR ARRAY')
	  call amat_to_rotmagstr(xform(1,1,nxfRead), theta, smag, str, phi)
	  smagMean = smag * sqrt(str)
	  call rotmagstr_to_amat(theta, smag / smagMean, str, phi, bb)

	  write(*,110)theta, smag, str, phi, smagMean, smag/smagMean, str
110	  format('Pair has rotation =',f8.2,', mag =',f7.4,
     &	      ', stretch =',f7.4,' on',f7.1,' deg axis',/,
     &	      'Mean mag =',f7.4,', using mag =',f7.4,' and stretch =',
     &	      f7.4)
	  write(*,'(a,4f10.6)')' No-mag transformation:',
     &	      ((bb(i,j),j=1,2),i=1,2)

	  theta = sign(acosd(0.5 * (bb(1, 1) + bb(2, 2))), theta)
	  alpha = 0.
	  if (abs(bb(2,1) - bb(1,2)) .gt. abs(bb(2,2) - bb(1,1)))
     &	      alpha = asind((bb(2,2) - bb(1,1)) / (bb(2,1) - bb(1,2)))
	  write(*, 113) theta, alpha
113	  format('True rotation =',f8.2,', delta theta =',f7.2)
	  dmag = 0.
	  if (abs(bb(2,1) + bb(1,2)) .gt. 1.e-10) then
	    ssqr = ((bb(2,1) - bb(1,2)) * cosd(alpha) - 2. * sind(theta)) /
     &		((bb(2,1) - bb(1,2)) * cosd(alpha) + 2. * sind(theta))
	    if (ssqr .lt. 0) then
	      print *,'WARNING: delta mag squared is negative:', ssqr
	      ssqr = 0.
	    endif
	    solSsqr = 2. * sqrt(ssqr)
	    bfac = 4. * sind(theta) /(bb(2,1) + bb(1,2))
	    solNeg = -bfac - sqrt(bfac**2 + 4)
	    solPos = -bfac + sqrt(bfac**2 + 4)
	    write(*, 111) -solSsqr, solSsqr, solNeg, solPos
111	    format('Delta mag from square:',2f10.4,',  quadratic:',
     &		2f12.4)
	    if (abs(solNeg + solSsqr) .lt. abs(solSsqr - solPos)) then
	      dmag = 0.5 * (solNeg - solSsqr)
	      ssqr = abs(solNeg + solSsqr)
	    else
	      dmag = 0.5 * (solPos + solSsqr)
	      ssqr = abs(solPos - solSsqr)
	    endif

	    write(*, 112)dmag, ssqr
112	    format('Use delta mag =',f7.4,
     &		', difference between estimates =', f7.4)
	  else
	    print *,'Delta mag appears to be 0 from coefficients'
	  endif
	  
c	    
c	    convert dmag and dtheta to a transformation matrix
c	    
	  call rotmag_to_amat(0., alpha, 1., dmag, bb)
	  write(*,'(a,4f10.6)')' Stretch transformation:',
     &	      ((bb(i,j),j=1,2),i=1,2)

	  call xfmult(aa, bb, xform(1,1,2))
	  call xfcopy(xform(1,1,2), aa)
c	    
c	    check that it regenerates the no-mag transformation
c
	  call xfinvert(bb, xform(1,1,1))
	  xform(1,1,2) = cosd(theta)
	  xform(1,2,2) = -sind(theta)
	  xform(2,2,2) = cosd(theta)
	  xform(2,1,2) = sind(theta)
	  call xfmult(xform(1,1,1), xform(1,1,2), xform(1,1,3))
	  call xfmult(xform(1,1,3), bb, xform(1,1,1))
	  write(*,'(a,4f10.6)')' Implied no-mag transformation:',
     &	      ((xform(i,j,1),j=1,2),i=1,2)
	endif
c	  
c	  Transform the vectors
c	  
	j = 0
	do ix = 1, nPtField(1)
	  do iy = 1, nPtField(2)
	    j = j + 1
	    fieldDx(ix, iy) = aa(1,1) * x(1, j) + aa(1,2) * x(2, j) - x(4, j)
	    fieldDy(ix, iy) = aa(2,1) * x(1, j) + aa(2,2) * x(2, j) - x(5, j)
	  enddo
	enddo
c	  
	if (makePatch) then
	  patchFile = concat(outRoot, '.patch')
	  call dopen(1, patchFile, 'new', 'f')
	  write(1,'(i7,a)')numTotField,' residuals'
	  j = 0
	  do iy = 1, nPtField(2)
	    do ix = 1, nPtField(1)
	      j = j + 1
	      xData = iFieldStrt(1) + (ix - 1) * fieldIntrv(1)
	      yData = iFieldStrt(2) + (iy - 1) * fieldIntrv(2)
	      write(1,103)xData, yData, 0, fieldDx(ix, iy), fieldDy(ix, iy)
103	      format(2f8.2,i6,2f8.2)
	    enddo
	  enddo
	  close(1)
	endif
c	  
c	  check it
c
	numRows = 0
	do ix = 1, nPtField(1)
	  do iy = 1, nPtField(2)
	    numRows = numRows + 1
	    xData = iFieldStrt(1) + (ix - 1) * fieldIntrv(1) - nx / 2.
	    yData = iFieldStrt(2) + (iy - 1) * fieldIntrv(2) - ny / 2.
	    x(4, numRows) = xData + fieldDx(ix, iy)
	    x(5, numRows) = yData + fieldDy(ix, iy)
	    x(1, numRows) = xData
	    x(2, numRows) = yData
	  enddo
	enddo
	do ixy = 1, 2
	  do i = 1, numRows
	    x(3, i) = x(3 + ixy, i)
	  enddo
	  call multrd(x, msiz, 3, numRows, sx, ss, xm,
     &		sd, b, b1, rsq, fval)
	  aa(ixy, 1) = b1(1)
	  aa(ixy, 2) = b1(2)
	  aa(ixy, 3) = 0.
	enddo
	write(*,'(a,4f10.6)')' Embedded transformation:',
     &	    ((aa(i,j),j=1,2),i=1,2)
c	  
c	  write the idf file
c
	call dopen(1, outfile, 'new', 'f')
	write(1,'(i5)')1
	write(1,'(2i7,i3,f10.4)')nx, ny, iBinning, pixelSize
	write(1,'(2(i6,f8.2,i6))')(iFieldStrt(i), fieldIntrv(i), nPtField(i),
     &	    i = 1,2)
	do iy = 1, nPtField(2)
	  write(1,'(8f8.2)')(fieldDx(ix, iy), fieldDy(ix, iy), ix = 1,
     &	      nPtField(1))
	enddo
	close(1)
c
	call exit(0)
97	call errorexit('WRITING TRANSFORMS TO FILE')
98	call errorexit('READING TRANSFORMS FROM FILE')
99	call errorexit('READING FILE')
	end


	subroutine findCoefficients(x, y, fieldDx, fieldDy, ixgDim, iygDim,
     &	    nPtField, iFieldStrt, fieldIntrv, sign, valRow, icolRow,
     &	    numInRow, sourceTol)
	integer*4 ixgDim, iygDim, nPtField(2), iFieldStrt(2), icolRow(*)
	real*4 x, y, fieldDx(ixgDim, iygDim), fieldDy(ixgDim, iygDim)
	real*4 fieldIntrv(2), sign, valRow(*), val, sourceTol
	integer*4 numInRow
	logical done
	real*4 xlast, ylast, xnew, ynew, dx, dy, xgrid,ygrid,fx(2),fy(2)
	integer*4 ixg,iyg, ix, iy, icol, iter
c	  
c	  first adjust the given coordinates to the original coordinate in
c	  the undistorted image that was mapped to this location
c	  
	iter = 1
	xlast = x
	ylast = y
	done = .false.
	do while (iter .lt. 10 .and. .not.done)
	  call interpolateGrid(xlast, ylast, fieldDx, fieldDy, ixgDim,
     &	      nPtField(1), nPtField(2), iFieldStrt(1), fieldIntrv(1),
     &	      iFieldStrt(2), fieldIntrv(2), dx, dy)
	  xnew = x - dx
	  ynew = y - dy
	  done = abs(xnew - xlast) .lt. sourceTol .and. abs(ynew - ylast) .lt.
     &	      sourceTol
	  xlast = xnew
	  ylast = ynew
	  iter = iter + 1
	enddo	
c	  
c	  Determine the coefficients in the field grid of this location
c
	xgrid = 1. + (xnew - iFieldStrt(1)) / fieldIntrv(1)
	ixg=xgrid
	ixg=max(1,min(nPtField(1)-1,ixg))
	fx(2)=max(0.,min(1.,xgrid-ixg))
	fx(1)=1.-fx(2)
	ygrid = 1. + (ynew - iFieldStrt(2)) / fieldIntrv(2)
	iyg=ygrid
	iyg=max(1,min(nPtField(2)-1,iyg))
	fy(2)=max(0.,min(1.,ygrid-iyg))
	fy(1)=1.-fy(2)
c	  
c	  Find the variable number and add to row if coefficient is non-zero
c	  
	do ix = 1, 2
	  do iy = 1, 2
	    val = sign * fx(ix) * fy(iy)
	    if (val .ne. 0) then
	      icol = ixg + ix - 1 + (iyg + iy - 2) * nPtField(1)
	      call addValueToRow(val, icol, valRow, icolRow, numInRow)
	    endif
	  enddo
	enddo
	return
	end


	subroutine addDataRow(valRow, icolRow, numInRow, xShift, yShift,
     &	    rwrk, ia, ja, numRows, shiftXY, maxData, sumEntries,
     &	    incEntries, x, msiz, matLim, numVars, doMultr)
	implicit none
	integer*4 icolRow(*), numInRow, ia(*), ja(*), numRows, maxData
	real*4 valRow(*), xShift, yShift, rwrk(*), shiftXY(2, *)
	integer*4 incEntries, msiz, matLim, numVars
	real*4 x(msiz, matLim), sumEntries(*)
	logical doMultr
	integer*4 i, ind, icol
c	  
c	  check for legality
c	  
	if (doMultr .and. numRows .ge. matLim) call errorexit(
     &	    'TOO MANY DATA POINTS TO SOLVE WITH MULTR')
	if (numRows .ge. maxData) call errorexit(
     &	    'TOO MANY DATA POINTS FOR ARRAYS')
c	  
c	  increment number of rows, save shifts, get index from ia
c
	numRows = numRows + 1
c	print *,'row', numRows, ' shift', xShift, yShift
c	print *,(icolRow(i), valRow(i), i = 1, numInRow)
	shiftXY(1, numRows) = xShift
	shiftXY(2, numRows) = yShift
	ind = ia(numRows)
	if (doMultr) then
	  do i = 1, numVars
	    x(i, numRows) = 0.
	  enddo
	endif
c	  
c	  put all items in arrays
c
	do i = 1, numInRow
	  rwrk(ind) = valRow(i)
	  icol = icolRow(i)
	  ja(ind) = icol
	  ind = ind + 1
	  sumEntries(icol) = sumEntries(icol) + incEntries * abs(valRow(i))
	  if (doMultr) x(icol, numRows) = valRow(i)
	enddo
c	
c	  save the index back in ia
c	  
	ia(numRows + 1) = ind
	return
	end


	subroutine addValueToRow(val, icol, valRow, icolRow, numInRow)
	implicit none
	real*4 val, valRow(*)
	integer*4 numInRow, icolRow(*), icol
	numInRow = numInRow + 1
	valRow(numInRow) = val
	icolRow(numInRow) = icol
	return
	end


	subroutine sparseProd(mode, m, n, x, y, leniw, lenrw, iw, rw)
	implicit none
	integer*4 mode, m, n, leniw, lenrw, iw(leniw)
	double precision x(n), y(m)
	real*4 rw(lenrw)
	integer*4 icol, irow, jaofs, ind
c	  
*	  If mode = 1, compute  y = y + A*x.
*	  If mode = 2, compute  x = x + A(transpose)*y.
c	  First number in iw is the offset to the JA values, the column
c	  number of each data value
c	  Then IW contains the I values, the starting index for each row
*
	jaofs = iw(1)
	if (mode .eq. 1) then
	  do irow = 1, m
	    do ind = iw(irow + 1), iw(irow + 2) - 1
	      icol = iw(jaofs + ind)
	      y(irow) = y(irow) + rw(ind) * x(icol)
	    enddo
	  enddo
	else
	  do irow = 1, m
	    do ind = iw(irow + 1), iw(irow + 2) - 1
	      icol = iw(jaofs + ind)
	      x(icol) = x(icol) + rw(ind) * y(irow)
	    enddo
	  enddo
	endif
	return
	end

	subroutine errorexit(message)
	implicit none
	character*(*) message
	print *
	print *,'ERROR: FINDDISTORT - ',message
	call exit(1)
	end

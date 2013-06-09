! XFTOXG takes a list of transformations (f) from each section to
! the previous one, and computes a list of xforms (g) to apply to each
! section to obtain a single consistent set of alignments.
!
! See man page for details.
!
! David Mastronarde 1988; hybrid method 7/26/01
!
! $Id$
!
program xftoxg
  implicit none
  integer LIMSEC
  parameter (LIMSEC = 100000)
  real*4 f(2,3,LIMSEC), g(2,3,LIMSEC), nat(2,3,LIMSEC) , gAvg(2,3), gcen(2,3)
  real*4 natAvg(2,3), gcenInv(2,3), prod(2,3), slope(2,3,10), intcp(2,3), natProd(2,3)
  real*4 x(LIMSEC), y(LIMSEC), slopeTmp(10)
  integer*4 igroup(LIMSEC), nControl(LIMSEC)
  character*320 inFile, outFile, errString
  real*4, allocatable :: dxGrid(:,:), dyGrid(:,:), dxCum(:,:,:), dyCum(:,:,:)
  real*4, allocatable :: dxProd(:,:), dyProd(:,:), gWarp(:,:,:)
  !
  integer*4 nhybrid, ifShift, iorder, nlist, kl, i, ilist, klLow, klHigh, nx, ny
  integer*4 j, ipow, numFit, ierr, numGroups, numInFirst, iorderUse, ibin, indFitCenter
  integer*4 irefSec, indWarpInput, indWarpOutput, nxGrid, nyGrid, nxGrTmp, nyGrTmp, iflags
  real*4 xStart, yStart, xInterval, yInterval, xStrTmp, yStrTmp, xIntTmp, yIntTmp
  logical*4 warping, control, robustLinear
  real*4 deltaAngle, angDiff, bint, angleRange, pixelSize, xcen, ycen, xEnd, yEnd
  integer*4 numCols, numRows, maxRobIter
  real*4, allocatable :: rMat(:,:), rSDs(:), rWork(:), rMeans(:), bSolve(:,:), cSolve(:)
  real*4 scaleKfactor, robChangeMax, robOscillMax, fracZeroWgt
  integer*4 robustRegress, multRegress
  integer*4 readCheckWarpFile, getNumWarpPoints, getLinearTransform
  integer*4 getGridParameters, getWarpGrid, setWarpGrid, setGridSizeToMake
  integer*4 setLinearTransform, writeWarpFile, multiplyWarpings, newWarpFile
  integer*4 clearWarpFile, separateLinearTransform, expandAndExtrapGrid
  integer*4 gridSizeFromSpacing, setCurrentWarpFile
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetFloat, PipGetLogical, PipGetThreeFloats
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  xftoxg
  !
  integer numOptions
  parameter (numOptions = 8)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@goutput:GOutputFile:FN:@nfit:NumberToFit:I:@'// &
      'ref:ReferenceSection:I:@order:OrderOfPolynomialFit:I:@'// &
      'mixed:HybridFits:I:@range:RangeOfAnglesInAverage:F:@help:usage:B:'
  !
  ! defaults
  !
  ifShift = 7
  nhybrid = 0
  iorder = 1
  irefSec = 0
  angleRange = 999.
  robustLinear = .false.
  maxRobIter = 200
  scaleKfactor = 1.
  robChangeMax = 0.02
  robOscillMax = 0.04
  fracZeroWgt = 0.2
  !
  ! initialize
  !
  call PipReadOrParseOptions(options, numOptions, 'xftoxg', &
      'ERROR: XFTOXG - ', .true., 1, 1, 1, numOptArg, numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0

  !
  ! get input parameters
  !
  if (pipInput) then
    if (PipGetInteger('ReferenceSection', irefSec) == 0) then
      if (irefSec < 1) call exitError('REFERENCE SECTION NUMBER MUST BE POSITIVE')
      ifShift = 0
    endif
    ierr = PipGetInteger('NumberToFit', ifShift)
    if (ifShift < 0) call exitError('A negative value for nfit is not allowed')
    if (irefSec > 0 .and. ifShift > 0) call exitError( &
        'A REFERENCE SECTION CAN ONLY BE USED WITH GLOBAL ALIGNMENT')

    ierr = PipGetInteger('OrderOfPolynomialFit', iorder)
    ierr = PipGetInteger('HybridFits', nhybrid)

    if (nhybrid .ne. 0) then
      if (ifShift == 0) call exitError( &
          'You cannot use hybrid and global alignment together')
      if (nhybrid < 0) call exitError( &
          'A negative value for hybrid alignment is not allowed')
      nhybrid = max(2, min(4, nhybrid))
    endif

    ierr = PipGetFloat('RangeOfAnglesInAverage', angleRange)
    ierr = PipGetLogical('RobustFit', robustLinear)
    ierr = PipGetFloat('KFactorScaling', scaleKfactor)
    ierr = PipGetInteger('MaximumIterations', maxRobIter)
    ierr = PipGetThreeFloats('IterationParams', fracZeroWgt, robChangeMax, robOscillMax)

  else
    
    print *,'Enter 0 to align all sections to a single' &
        //' average central alignment;'
    print *,'   or 1 to align to an average alignment that shifts based'
    print *,'         on a polynomial fit to the whole stack;'
    print *, '   or N to align each section to an average '// &
          'alignment based'
    write(*,'(1x,a,/,a,$)') &
        '         on a polynomial fit to the nearest N sections,', &
        '   or -1 or -N for a hybrid of central and shifting '// &
        'alignments: '
    read(*,*) ifShift
    !
    if (ifShift < 0) then
      write(*,'(1x,a,/,a,$)') 'Enter # of parameters to do central' &
          //' alignment on (2 for translation only,', &
          ' 3 for translation/rotation; 4 for trans/rot/mag: '
      read(5,*) nhybrid
      ifShift = abs(ifShift)
      nhybrid = max(2, min(4, nhybrid))
    endif
    !
    if (ifShift > 0) then
      write(*,'(1x,a,$)') &
          'Order of polynomial to fit (1 for linear): '
      read(*,*) iorder
    endif
  endif
  if (ifShift > 1) iorder = min(ifShift - 1, iorder)
  iorder = max(1, min(10, iorder))
  !
  if (PipGetInOutFile('InputFile', 1, 'Input file of f transforms', &
      inFile) .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
  if (PipGetInOutFile('GOutputFile', 2, 'Output file of g transforms', &
      outFile) .ne. 0) then
    i = len_trim(inFile)
    if (inFile(i - 1:i) .ne. 'xf') call exitError &
        ('NO OUTPUT FILE SPECIFIED AND INPUT FILENAME DOES NOT END IN XF')
    outFile = inFile
    outFile(i:i) = 'g'
  endif
  !
  ! Determine if there is warping
  indWarpInput = readCheckWarpFile(inFile, 0, 1, nx, ny, nlist, ibin, &
      pixelSize, iflags, errString)
  if (indWarpInput < -1) call exitError(errString)
  warping = indWarpInput >= 0
  if (.not. warping) then
    !
    ! Regular: open files, read the whole list of f's
    !
    call dopen(1, inFile, 'old', 'f')
    call dopen(2, outFile, 'new', 'f')
    call xfrdall2(1, f, nlist, LIMSEC, ierr)
    if (ierr == 2) call exitError('READING TRANSFORM FILE')
    if (ierr == 1) call exitError('TOO MANY TRANSFORMS IN FILE FOR ARRAYS')
  endif
  if (nlist > LIMSEC) call exitError('TOO MANY TRANSFORMS FOR ARRAYS')
  if (irefSec > nlist) call exitError( &
      'REFERENCE SECTION NUMBER TOO LARGE FOR NUMBER OF TRANSFORMS')
  !
  if (ifShift > 0 .and. nlist < 3) then
    write(*,'(/,a)') 'WARNING: XFTOXG - COMPUTING A GLOBAL ALIGNMENT SINCE THERE '// &
        'ARE FEWER THAN 3 TRANSFORMS'
    ifShift = 0
  endif
  !
  ! Set up for linear robust fits
  if (ifShift > 0) then
    numCols = iorder + 4
    numRows = nlist
    if  (ifShift > 1) numRows = ifShift
    allocate(rMat(numRows, numCols + 2), bSolve(iorder, 4), cSolve(4),  &
        rMeans(numCols), rSDs(numCols), rWork(numCols**2 + 2 * numRows), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR ROBUST REGRESSION')
  endif
  if (warping) then
    !
    ! warping
    write(*,'(a,a)') 'Old warping file opened: ', trim(inFile)
    control = mod(iflags / 2, 2) .ne. 0
    xcen = nx / 2.
    ycen = ny / 2.
    !
    ! Get the linear transforms and determine smallest spacing needed
    xStart = nx
    yStart = ny
    xEnd = 0.
    yEnd = 0.
    xInterval = nx
    yInterval = ny
    nxGrid = 0
    nyGrid = 0
    do kl = 1, nlist
      nControl(kl) = 4
      if (control) then
        if (getNumWarpPoints(kl, nControl(kl)) .ne. 0) &
            call exitError('GETTING NUMBER OF CONTROL POINTS')
      endif
      ! IS THIS NEEDED?
      if (nControl(kl) > 2) then
        if (separateLinearTransform(kl) .ne. 0) call exitError &
            ('SEPARATING OUT THE LINEAR TRANSFORM FROM THE WARPING')
      endif
      if (getLinearTransform(kl, f(1, 1, kl)) .ne. 0) call exitError &
          ('GETTING LINEAR TRANSFORM FROM WARP FILE')
      if (nControl(kl) >= 4) then
        if (control) then
          if (gridSizeFromSpacing(kl, -1., -1., 1) .ne. 0) call exitError &
              ('SETTING GRID SIZE FROM SPACING OF CONTROL POINTS')
        endif
        if (getGridParameters(kl, nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp, &
            yIntTmp) .ne. 0) call exitError('GETTING GRID PARAMETERS')
        ! print *,nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp, yIntTmp
        xStart = min(xStart, xStrTmp)
        xInterval = min(xInterval, xIntTmp)
        xEnd = max(xEnd, (nxGrTmp - 1) * xInterval)
        yStart = min(yStart, yStrTmp)
        yInterval = min(yInterval, yIntTmp)
        yEnd = max(yEnd, (nyGrTmp - 1) * yInterval)
      endif
    enddo
    ! print *,xStart, xEnd, xInterval, yStart, yEnd, yInterval
    if (xStart <= 0. .or. xEnd >= nx .or. yStart <= 0. .or. yEnd >= ny) &
        call exitError('CANNOT WORK WITH GRIDS THAT EXTEND OUTSIDE THE DEFINED '// &
        'IMAGE AREA')
    !
    ! Figure out the grid size and interval that fits in the range, but make it
    ! fill the range
    xStart = min(xStart, xInterval / 2.)
    xEnd = max(xEnd, nx - xInterval / 2.)
    yStart = min(yStart, yInterval / 2.)
    yEnd = max(yEnd, ny - yInterval / 2.)
    nxGrid = max(2., (xEnd - xStart) / xInterval + 1.05)
    xInterval = (xEnd - xStart) / (nxGrid - 1)
    nyGrid = max(2., (yEnd - yStart) / yInterval + 1.05)
    yInterval = (yEnd - yStart) / (nyGrid - 1)
    ! print *,xStart, xEnd, xInterval, yStart, yEnd, yInterval, nxGrid, nyGrid
    !
    if (control) then
      !
      ! Then set all the parameters for control point grids
      do kl = 1, nlist
        ierr = setGridSizeToMake(kl, nxGrid, nyGrid, xStart, yStart, xInterval, &
            yInterval)
      enddo
    endif
    !
    ! Allocate arrays
    allocate(dxGrid(nxGrid, nyGrid), dyGrid(nxGrid, nyGrid),  &
        dxCum(nxGrid, nyGrid, nlist), dyCum(nxGrid, nyGrid, nlist),  &
        dxProd(nxGrid, nyGrid), dyProd(nxGrid, nyGrid), gWarp(2, 3, nlist), &
        stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR WARPING GRIDS')
    !
    ! Get the cumulative transforms over the whole list, put cumulative linear in g
    call cumulativeWarp(1, nlist, g)
    !
    ! Find the mean grid and take its inverse, leave in dxGrid, dyGrid
    dxProd = 0.
    dyProd = 0.
    do kl = 1, nlist
      call xfCopy(g(1, 1, kl), gWarp(1, 1, kl))
      do j = 1, nyGrid
        do i = 1, nxGrid
          dxProd(i, j) = dxProd(i, j) + dxCum(i, j, kl) / nlist
          dyProd(i, j) = dyProd(i, j) + dyCum(i, j, kl) / nlist
        enddo
      enddo
    enddo
    ! write(*,'(f7.2,9f8.2)') ((dxProd(i, j), dyProd(i, j), i=1, nxGrid), j=1, nyGrid)

    call invertWarpGrid(dxProd, dyProd, nxGrid, nxGrid, nyGrid, xStart, yStart, &
        xInterval, yInterval, g(1, 1, 1), xcen, ycen, dxGrid, dyGrid, prod)

    ! Or take the inverse at the reference section
    if (irefSec > 0) call invertWarpGrid(dxCum(1, 1, irefSec), dyCum(1, 1, irefSec), &
        nxGrid, nxGrid, nyGrid, xStart, yStart, &
        xInterval, yInterval, g(1, 1, irefSec), xcen, ycen, dxGrid, dyGrid, prod)
    
    !
    ! Start a new warp file
    if (ifShift < 2) ierr = clearWarpFile(indWarpInput)
    iflags = 1
    indWarpOutput = newWarpFile(nx, ny, ibin, pixelSize, iflags)
    if (indWarpOutput < 0) call exitError('OPENING A NEW WARPING FILE')
  endif

  if (.not. warping .or. ifShift > 1) then
    !
    ! Regular transforms: compute g's to align all sections to the first
    ! Do this for warping with local fits instead of trusting the g computed above
    call xfUnit(g(1, 1, 1), 1.)
    do i = 2, nlist
      call xfMult(f(1, 1, i), g(1, 1, i - 1), g(1, 1, i))
      ! call xfwrite(6, g(1, 1, i))
    enddo
  endif
  !
  ! Usual treatment of regular transforms now that we have cumulative g in each case
  ! Convert to "natural" transforms
  !
  deltaAngle = 0.
  do kl = 1, nlist
    call amat_to_rotmag(g(1, 1, kl), nat(1, 1, kl), nat(1, 2, kl) , nat(2, 1, kl), &
        nat(2, 2, kl))
    nat(1, 3, kl) = g(1, 3, kl)
    nat(2, 3, kl) = g(2, 3, kl)
    !
    ! if rotation angle differs greatly from last one, adjust the
    ! DELTANG value appropriately so that angles change in continuous
    ! increments around + or - 180 degrees
    !
    if (kl > 1) then
      angDiff = nat(1, 1, kl - 1) - (nat(1, 1, kl) + deltaAngle)
      if (abs(angDiff) > 180.) deltaAngle = deltaAngle + sign(360., angDiff)
    endif
    nat(1, 1, kl) = nat(1, 1, kl) + deltaAngle
  enddo
  !
  ! average natural g's, convert back to xform, take inverse of average;
  ! ginv is used if no linear fits, natav is used in hybrid cases
  ! First get group with restricted rotation range
  !
  call groupRotations(nat, 1, nlist, angleRange, igroup, numGroups, &
      numInFirst)
  ! print *,numGroups, ' groups, first has', numInFirst
  ! print *,(igroup(kl), kl=1, nlist)
  call xfunit(natAvg, 0.)
  do kl = 1, nlist
    if (igroup(kl) == 1) &
        call xflincom(natAvg, 1., nat(1, 1, kl), 1. / numInFirst, natAvg)
  enddo
  call rotmag_to_amat(natAvg(1, 1), natAvg(1, 2), natAvg(2, 1), natAvg(2, 2), gAvg)
  gAvg(1, 3) = natAvg(1, 3)
  gAvg(2, 3) = natAvg(2, 3)
  call xfInvert(gAvg, gcenInv)
  !
  ! If doing reference section, just invert its transform
  !
  if (irefSec > 0) call xfInvert(g(1, 1, irefSec), gcenInv)
  !
  ! DNM 1/24/04: fixed bug in hybrid 3 or 4:
  ! do not convert natav to be the inverse!
  !
  ! loop over each section
  !
  do ilist = 1, nlist
    if (ifShift > 0) then
      ! if doing line fits, set up section limits for this fit
      if (ifShift == 1) then
        klLow = 1                              !ifshift = 1: take all sections
        klHigh = nlist
      else
        klLow = max(1, ilist - ifShift / 2)         !>1: take N sections centered
        klHigh = min(nlist, klLow + ifShift - 1)      !on this one, or offset
        klLow = max(1, min(klLow, klHigh + 1 - ifShift))
      endif
      if (ifShift > 1 .or. (ifShift == 1 .and. ilist == 1)) then
        !
        ! do line fit to each component of the natural parameters
        ! first time only if doing all sections, or each time for N
        !
        call groupRotations(nat, klLow, klHigh, angleRange, igroup, numGroups, numInFirst)
        if (numInFirst < 2) call exitError('Only 1 point in fit; '// &
            'increase NumberToFit or RangeOfAngles')
        iorderUse = max(1, min(iorder, numInFirst - 2))
        indFitCenter = (klHigh + klLow) / 2
        ierr = 1
        if (robustLinear .and. numInFirst > 3) then

          ! Do the robust fitting if called for and there are enough points
          if (maxRobIter < 0) print *,'Robust fitting for rot/mag of section',ilist
          call robustFitToNat(1, 2, ierr)
          if (ierr == 0) then
            if (maxRobIter < 0) print *,'Robust fitting for shifts of section',ilist
            call robustFitToNat(3, 3, ierr)
            if (ierr .ne. 0) write(*,101)'shifts',ilist
          else
            write(*,101)'rot/mag',ilist
          endif
101       format('Robust fitting to ',a,' failed for section',i5, &
              ', falling back to regular fit')
        endif

        ! Otherwise, or if robust failed, do regular linear fit
        if (ierr .ne. 0) then 
          do i = 1, 2
            do j = 1, 3
              numFit = 0
              do kl = klLow, klHigh
                if (igroup(kl) == 1) then
                  numFit = numFit + 1
                  x(numFit) = kl - indFitCenter
                  y(numFit) = nat(i, j, kl)
                endif
              enddo
              !
              call polyfit(x, y, numFit, iorderUse, slopeTmp, bint)
              do ipow = 1, iorderUse
                slope(i, j, ipow) = slopeTmp(ipow)
              enddo
              intcp(i, j) = bint
            enddo
          enddo
        endif
      endif
      if (ifShift == 1 .and. ilist == 1) then
        print *,'constants and coefficients of fit to', numFit , ' natural parameters'
        call xfwrite(6, intcp,*97)
        do ipow = 1, iorderUse
          call xfwrite(6, slope(1, 1, ipow),*99)
99      enddo
      endif
      !
      ! calculate the g transform at this position along the linear fit
      ! and take its inverse
      !
      call xfCopy(intcp, natProd)
      do ipow = 1, iorderUse
        call xflincom(natProd, 1., slope(1, 1, ipow), float(ilist - indFitCenter)**ipow, &
            natProd)
      enddo
      !
      ! for hybrid method, restore global average for translations,
      ! restore rotation if nhybrid is 3 or 4; restore overall mag
      ! if nhybrid is 4
      !
      if (nhybrid > 0) then
        natProd(1, 3) = natAvg(1, 3)
        natProd(2, 3) = natAvg(2, 3)
        if (nhybrid > 2) natProd(1, 1) = natAvg(1, 1)
        if (nhybrid > 3) natProd(2, 1) = natAvg(2, 1)
      endif
      !
      call rotmag_to_amat(natProd(1, 1), natProd(1, 2) &
          , natProd(2, 1), natProd(2, 2), gcen)
      gcen(1, 3) = natProd(1, 3)
      gcen(2, 3) = natProd(2, 3)
      call xfInvert(gcen, gcenInv)
    endif
    !
    ! multiply this g by the inverse of the grand average or the locally
    ! fitted average: generates a xform to the common central place or
    ! to the locally fitted center.  This stuff about the linearly fitted
    ! center position is pretty ad hoc and hokey, but it seems to give
    ! good results in the final images.
    !
    if (warping) then
      !
      ! If there are warpings, multiply cumulative warp by inverse warp based on
      ! this transform and the inverse average warp in the global case

      if (ifShift > 0) then

        ! For local fits, get a cumulative warp over the fit range
        if (ifShift > 1) then
          if (setCurrentWarpFile(indWarpInput) .ne. 0)  &
              call exitError('SWITCHING BACK TO INPUT WARP FILE')
          call cumulativeWarp(klLow, klHigh, gWarp)
        endif

        ! For all fits, now fit to the cumulative warps, put the fitted values in dxProd,
        ! and invert that, assuming the current linear forward transform there
        call fitWarpComponent(dxCum, dxProd)
        call fitWarpComponent(dyCum, dyProd)
        call invertWarpGrid(dxProd, dyProd, nxGrid, nxGrid, nyGrid, xStart, yStart, &
            xInterval, yInterval, gcen, xcen, ycen, dxGrid, dyGrid, gcenInv)
      endif
      if (irefSec > 0) then
        !
        ! For a reference section, set to unit transform and warping (did verify that
        ! product warp was <= 0.001)
        call xfunit(prod, 1.)
        dxProd(1:nxGrid, 1:nyGrid) = 0.
        dyProd(1:nxGrid, 1:nyGrid) = 0.
      else
        !
        ! Otherwise multiply the local cumulative warp plus the true global G times
        ! the fitted local warp plus the true fitted center.  Not perfect but very close.
        ! The alternative would be to do the fit with the local cumulative gWarp's
        if (multiplyWarpings(dxCum(1,1, ilist), dyCum(1,1, ilist), nxGrid, nxGrid, &
            nyGrid, xStart, yStart, xInterval, yInterval, g(1, 1, ilist), xcen, ycen, &
            dxGrid, dyGrid, nxGrid, nxGrid, nyGrid, xStart, yStart, xInterval, &
            yInterval, gcenInv, dxProd, dyProd, prod, 0) .ne. 0) &
            call exitError ('MULTIPLYING TWO WARPINGS TOGETHER FOR FINAL WARPING')
      endif
      if (setCurrentWarpFile(indWarpOutput) .ne. 0)  &
          call exitError('SWITCHING TO OUTPUT WARP FILE')
      if (setLinearTransform(ilist, prod) .ne. 0 .or. &
          setWarpGrid(ilist, nxGrid, nyGrid, xStart, yStart, &
          xInterval, yInterval, dxProd, dyProd, nxGrid) .ne. 0) &
          call exitError('STORING FINAL WARPING')

    else
      call xfMult(g(1, 1, ilist), gcenInv, prod)
      call xfwrite(2, prod,*97)
    endif
  enddo
  if (warping) then
    if (writeWarpFile(outFile, 0) .ne. 0) call exitError('WRITING NEW WARP FILE')
    write(*,'(a,a)') 'New warping file written: ', trim(outFile)
  else
    close(2)
  endif
  call exit(0)
97 call exitError('WRITING FILE')

CONTAINS

  ! Form cumulative product of warping transforms to align to the first section in a range
  !
  subroutine cumulativeWarp(klStart, klEnd, gcw)
    integer*4 klStart, klEnd
    real*4 gcw(2, 3, nlist)
    integer*4 getWarpGrid, expandAndExtrapGrid, multiplyWarpings

    ! initialize first one to unit transform
    call xfunit(gcw(1, 1, klStart), 1.)
    dxCum(1:nxGrid, 1:nyGrid, klStart) = 0.                              !ARRAY OPERATIONS
    dyCum(1:nxGrid, 1:nyGrid, klStart) = 0.
    do kl = klStart + 1, klEnd
      if (nControl(kl) > 3) then
        if (getWarpGrid(kl, nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp, yIntTmp, &
            dxGrid, dyGrid, nxGrid) .ne. 0) call exitError('GETTING WARP GRID')
        ! print *,'raw grid', nxGrTmp, nyGrTmp
        ! write(*,'(f7.2,9f8.2)') ((dxGrid(i, j), dyGrid(i, j), i=1, nxGrid), j=1, nyGrid)
        if (.not.control) then
          if (expandAndExtrapGrid(dxGrid, dyGrid, nxGrid, nyGrid, nxGrTmp, nyGrTmp, &
              xStrTmp, yStrTmp, xIntTmp, yIntTmp, xStart, yStart, xEnd, yEnd, 0, nx, &
              0, ny) .ne. 0) call exitError('EXPANDING A WARP GRID')
        endif
      else
        nxGrTmp = nxGrid
        nyGrTmp = nyGrid
        dxGrid = 0.
        dyGrid = 0.
      endif
      if (multiplyWarpings(dxGrid, dyGrid, nxGrid, nxGrTmp, nyGrTmp, xStrTmp, &
          yStrTmp, xIntTmp, yIntTmp, f(1, 1, kl), xcen, ycen, &
          dxCum(1, 1, kl - 1), dyCum(1, 1, kl - 1), nxGrid, nxGrid, nyGrid, &
          xStart, yStart, xInterval, yInterval, gcw(1, 1, kl - 1), &
          dxCum(1, 1, kl), dyCum(1, 1, kl), gcw(1, 1, kl), 0) .ne. 0) &
          call exitError ('MULTIPLYING TWO WARPINGS TOGETHER FOR CUMULATIVE WARPING')
      ! call xfwrite(6, g(1, 1, kl))
      ! print *,'cumulative', nxGrid, nyGrid
      ! write(*,'(f7.2,9f8.2)') ((dxCum(i, j, kl), dyCum(i, j, kl), i=1, nxGrid), j=1, nyGrid)
    enddo
    return
  end subroutine cumulativeWarp


  ! Do the polynomial fit to the x or y component of set of cumulative warping transforms
  ! and place the fitted position at "ilist" in the dxyProd array
  ! 
  subroutine fitWarpComponent(dxyCum, dxyProd)
    real*4 dxyCum(nxGrid, nyGrid, nlist), dxyProd(nxGrid, nyGrid)
    do i = 1, nxGrid
      do j = 1, nyGrid
        numFit = 0
        do kl = klLow, klHigh
          numFit = numFit + 1
          x(numFit) = kl - indFitCenter
          y(numFit) = dxyCum(i, j, kl)
        enddo
        call polyfit(x, y, numFit, iorder, slopeTmp, bint)
        do ipow = 1, iorder
          bint = bint + slopeTmp(ipow) * (ilist - indFitCenter)**ipow
        enddo
        dxyProd(i, j) = bint
      enddo
    enddo
    return 
  end subroutine fitWarpComponent

  
  ! Do a robust fit to either the geometric parameters or the shifts over current range
  !
  subroutine robustFitToNat(jstart, jend, iret)
    integer*4 jstart, jend, iret
    real*4 fitScale(2, 3) /1., 100., 1., 100., 1., 1./
    real*4 fitAdd(2, 3) /0., -1., 0., 0., 0., 0./
    integer*4 indCol, numBcol, numIter, maxZeroWgt
    integer*4 robustRegress
    iret = 0

    ! Load the data matrix, scaling mag and dmag
    numFit = 0
    do kl = klLow, klHigh
      if (igroup(kl) == 1) then
        numFit = numFit + 1
        indCol = 1
        do ipow = 1, iorderUse
          rMat(numFit, indCol) = float(kl - indFitCenter)**ipow
          indCol = indCol + 1
        enddo
        do i = 1, 2
          do j = jstart, jend
            rMat(numFit, indCol) = (nat(i, j, kl) + fitAdd(i, j)) * fitScale(i, j)
            indCol = indCol + 1
          enddo
        enddo
        rmat(numFit, indCol) = 1.
      endif
    enddo

    ! Do the regression
    maxZeroWgt = max(1, nint(fracZeroWgt * numFit))
    numBcol = 2 * (jend + 1 - jstart)
    iret = robustRegress(rMat, numRows, 0, iorderUse, numFit, numBcol, bSolve, iorder, &
        cSolve, rMeans, rSDs, rWork, scaleKfactor * 4.685, numIter, maxRobIter, &
        maxZeroWgt, robChangeMax, robOscillMax)
    if (iret .ne. 0) return
    
    ! unpack the result
    indCol = 1
    do i = 1, 2
      do j = jstart, jend
        do ipow = 1, iorderUse
          slope(i, j, ipow) = bSolve(ipow, indCol) / fitScale(i, j)
        enddo
        intcp(i, j) = cSolve(indCol) / fitScale(i, j) - fitAdd(i, j)
        indCol = indCol + 1
      enddo
    enddo
    
    return
  end subroutine robustFitToNat


end program

!
! groupRotations finds groups of sections whose rotation angles are
! all within the range given by RANGE.  The angles are assumed to be
! the first element of the transforms in F.  Only sections from KSTART
! to KEND are considered.  IGROUP is returned with a group number for
! each section; numGroups with the number of groups, and numInFirst
! with the number of sections in the first (i.e., largest) group.
!
subroutine groupRotations(f, kStart, kEnd, range, igroup, numGroups, numInFirst)
  implicit none
  real*4 f(2,3,*), range
  integer*4 igroup(*), kStart, kEnd, numGroups, numInFirst
  real*4 diff, angleMin, angleMax, angleDiff
  integer*4 numFree, i, j, numInGroup, maxInGroup, iMax
  !
  numFree = kEnd + 1 - kStart
  numGroups = 0
  angleMin = 1.e10
  angleMax = -1.e10
  do i = kStart, kEnd
    igroup(i) = 0
    angleMin = min(angleMin, f(1, 1, i))
    angleMax = max(angleMax, f(1, 1, i))
  enddo
  !
  ! If the angles all fit within the range, we are all set
  !
  if (angleMax - angleMin <= range) then
    do i = kStart, kEnd
      igroup(i) = 1
    enddo
    numGroups = 1
    numInFirst = numFree
    return
  endif
  !
  do while (numFree > 0)
    !
    ! Find biggest group of angles that fit within range
    !
    i = kStart
    maxInGroup = 0
    do while (i <= kEnd .and. maxInGroup < numFree)
      if (igroup(i) == 0) then
        !
        ! For each ungrouped item, count up number of items that would
        ! fit within a range starting at this item
        !
        numInGroup = 0
        do j = kStart, kEnd
          if (igroup(j) == 0) then
            diff = angleDiff(f(1, 1, i), f(1, 1, j))
            if (diff >= 0. .and. diff <= range) &
                numInGroup = numInGroup + 1
          endif
        enddo
        if (numInGroup > maxInGroup) then
          maxInGroup = numInGroup
          iMax = i
        endif
      endif
      i = i + 1
    enddo
    !
    ! Now assign the angles within range to the group
    !
    numGroups = numGroups + 1
    do j = kStart, kEnd
      if (igroup(j) == 0) then
        diff = angleDiff(f(1, 1, iMax), f(1, 1, j))
        if (diff >= 0. .and. diff <= range) igroup(j) = numGroups
      endif
    enddo
    if (numGroups == 1) numInFirst = maxInGroup
    numFree = numFree - maxInGroup
  enddo
  return
end subroutine groupRotations

real*4 function angleDiff(ang1, ang2)
  real*4 ang1, ang2
  angleDiff = ang2 - ang1
  do while (angleDiff > 180.)
    angleDiff = angleDiff - 360.
  enddo
  do while (angleDiff <= -180.)
    angleDiff = angleDiff + 360.
  enddo
  return
end function angleDiff

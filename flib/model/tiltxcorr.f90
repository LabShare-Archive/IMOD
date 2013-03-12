! * * * * * * TILTXCORR * * * * * *
!
! TILTXCORR uses cross-correlation to find an initial translational
! alignment between successive images of a tilt series.  For a given
! pair of images, it stretches the image with the larger tilt angle
! perpendicular to the tilt axis, by an amount equal to the ratio of
! the cosines of the two tilt angles (cosine stretch) .  The stretched
! image is correlated with the other image, and the position of the
! peak of the correlation indicates the relative shift between the
! images.  There are options to use only a subset of the image, to
! pad the image with a border before correlating, and to taper the
! image intensities down to the average level over some boundary
! region.  The latter feature is particularly important for getting
! reliable correlation peaks.  The program also has an option to
! correlate each image with the sum of already-aligned images at lower
! tilts, a method developed by Christian Renkin.
!
! For further details, see the man page.
!
! $Id$
!
program tiltxcorr
  implicit none
  integer IDIM, IDIM2, LIMPATCH, LIMBOUND
  parameter (IDIM = 4300, IDIM2 = IDIM * IDIM)
  parameter (LIMPATCH = 100000, LIMBOUND = 1000)
  include 'smallmodel.inc90'
  integer*4 nx, ny, nz
  !
  integer*4 nxyz(3), mxyz(3), nxyzs(3)
  real*4 title(20), delta(3), origin(3)
  real*4 ctfp(8193), sumArray(IDIM2), crray(IDIM2)
  real*4 array(IDIM2), brray(IDIM2)
  real*4, allocatable :: tmpArray(:)
  !
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  common /bigarr/ array, sumArray, brray, crray
  !
  character*320 inFile, plFile, imFileOut, xfFileOut
  character*1000 listString
  real*4 fs(2,3), fsInv(2,3), fUnit(2,3)
  character*9 dat
  character*8 tim
  character*80 titlech
  character*70 titleStr
  character*10 filterText/' '/
  character*15 binRedText/' Binning is'/

  real*4, allocatable :: f(:,:,:), tilt(:), dxPreali(:), dyPreali(:)
  integer*4, allocatable :: ixPcList(:), iyPcList(:), izPcList(:)
  integer*4, allocatable :: listz(:), listSkip(:)
  real*4 patchCenX(LIMPATCH), patchCenY(LIMPATCH)
  real*4, allocatable :: xmodel(:,:), ymodel(:,:), xbound(:), ybound(:)
  real*4, allocatable :: xtfsBound(:), ytfsBound(:)
  integer*4, allocatable :: iobjFlags(:)
  integer*4 iobjBound(LIMBOUND), indBound(LIMBOUND)
  integer*4 numInBound(LIMBOUND)
  real*4 xtfsBmin(LIMBOUND), xtfsBmax(LIMBOUND), ytfsBmin(LIMBOUND)
  real*4 ytfsBmax(LIMBOUND)
  real*4 dmin2, dmax2, dmean2, dmean3, rotAngle, deltaCTF, radExclude, cosStrMaxTilt
  integer*4 i, numPcList, numViews, minXpiece, numXpieces, nxOverlap, minYpiece, numBest
  integer*4 numYpieces, nyOverlap, ifImOut, nxPad, nyPad, ifExclude, mode, indBest
  integer*4 nxTrim, nyTrim, nxUse, nyUse, nxBorder, nyBorder, nxTaper, nyTaper
  integer*4 izStart, izEnd, kk, nzOut, izLast, izCur, idir, izTmp, imagesBinned
  real*4 dmeanSum, dmax, dmin, stretch, streak, xpeak, ypeak, unstretchDx, unstretchDy
  real*4 dmean, radius1, radius2, sigma1, sigma2, tiltAtMin, cosView
  integer*4 iv, iview, kti, isOut, ierr, ivStart, ivEnd, loopDir, ifReadXfs
  integer*4 iloop, numLoops, minTilt, ifAbsStretch, ivRef, ifLeaveAxis, ivRefBase
  integer*4 nbinning, maxBinSize, nxUseBin, nyUseBin, ifCumulate, ifNoStretch, maxTrackGap
  integer*4 ixStart, ixEnd, iyStart, iyEnd, ivCur, maxBinning, iz, ivSkip
  integer*4 ixBoxCur, iyBoxCur, ixBoxRef, iyBoxRef, lenContour, minContOverlap
  integer*4 ixCenStart, ixCenEnd, iyCenStart, iyCenEnd, iter, numIter
  integer*4 lapTotal, lapRemainder, j, ivBase, lapBase, numCont, nxPatch, nyPatch
  real*4 xBoxOfs, yBoxOffset, cosPhi, sinPhi, x0, y0, xshift, yshift
  real*4 useMin, useMax, useMean, cumXshift, cumYshift, cumXrot, xAdjust
  real*4 angleOffset, cumXcenter, cumYcenter, xModOffset, yModOffset
  real*4 xpeakCum, ypeakCum, xpeakTmp, ypeakTmp, xpeakFrac, ypeakFrac, yOverlap
  real*4 peakFracTol, xFromCen, yFromCen, cenx, ceny, baseTilt, xOverlap, yval
  integer*4 numPatches, numXpatch, numYpatch, ind, ixBoxStart, iyBoxStart
  integer*4 ixBoxForAdj, iyBoxForAdj, ipatch, numPoints, numBound, iobj, lenTemp
  integer*4 ipnt, ipt, numInside, iobjSeed, imodObj, imodCont, ix, iy, iAntiFiltType
  integer*4 limitShiftX, limitShiftY, numSkip, lastNotSkipped, lenConts, nFillTaper
  real*4 critInside, cosRatio, peakVal, peakLast, xpeakLast, yPeakLast
  real*4 boundXmin, boundXmax, boundYmin, boundYmax, fracXover, fracYover
  real*4 fracOverMax, critNonBlank, fillTaperFrac
  real*8 wallMask, wallTime, wallStart, wallInterp, wallfft

  logical*4 tracking, verbose, breaking, taperCur, taperRef, reverseOrder
  integer*4 niceFrame, newImod, putImodMaxes, putModelName, numberInList, taperAtFill
  logical inside
  real*4 cosd, sind

  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean, PipGetLogical
  integer*4 PipGetString, PipGetFloat, PipGetTwoIntegers, PipGetTwoFloats
  integer*4 PipGetInOutFile, ifPip
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  tiltxcorr
  !
  integer numOptions
  parameter (numOptions = 45)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@piece:PieceListFile:FN:@output:OutputFile:FN:@'// &
      'rotation:RotationAngle:F:@first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'// &
      'tiltfile:TiltFile:FN:@angles:TiltAngles:FAM:@offset:AngleOffset:F:@'// &
      'reverse:ReverseOrder:B:@radius1:FilterRadius1:F:@radius2:FilterRadius2:F:@'// &
      'sigma1:FilterSigma1:F:@sigma2:FilterSigma2:F:@exclude:ExcludeCentralPeak:B:@'// &
      'shift:ShiftLimitsXandY:IP:@border:BordersInXandY:IP:@xminmax:XMinAndMax:IP:@'// &
      'yminmax:YMinAndMax:IP:@boundary:BoundaryModel:FN:@objbound:BoundaryObject:I:@'// &
      'binning:BinningToApply:I:@antialias:AntialiasFilter:I:@'// &
      'leaveaxis:LeaveTiltAxisShifted:B:@pad:PadsInXandY:IP:@taper:TapersInXandY:IP:@'// &
      'views:StartingEndingViews:IP:@skip:SkipViews:LI:@break:BreakAtViews:LI:@'// &
      'cumulative:CumulativeCorrelation:B:@absstretch:AbsoluteCosineStretch:B:@'// &
      'nostretch:NoCosineStretch:B:@iterate:IterateCorrelations:I:@'// &
      'size:SizeOfPatchesXandY:IP:@number:NumberOfPatchesXandY:IP:@'// &
      'overlap:OverlapOfPatchesXandY:IP:@seed:SeedModel:FN:@objseed:SeedObject:I:@'// &
      'length:LengthAndOverlap:IP:@prexf:PrealignmentTransformFile:FN:@'// &
      'imagebinned:ImagesAreBinned:I:@test:TestOutput:FN:@verbose:VerboseOutput:B:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  ! set defaults here where not dependent on image size
  !
  ifImOut = 0
  ifExclude = 0
  nxTrim = 0
  nyTrim = 0
  sigma1 = 0.
  sigma2 = 0.
  radius1 = 0.
  radius2 = 0.
  rotAngle = 0.
  imFileOut = ' '
  xfFileOut = ' '
  ifCumulate = 0
  ifNoStretch = 0
  ifAbsStretch = 0
  ifLeaveAxis = 0
  angleOffset = 0.
  maxBinSize = 1180
  maxBinning = 8
  cosStrMaxTilt = 82.
  nbinning = 0
  ifPip = 0
  numIter = 1
  peakFracTol = 0.015
  lenContour = 0
  minContOverlap = 0
  tracking = .false.
  verbose = .false.
  critInside = 0.75
  limitShiftX = 1000000
  limitShiftY = 1000000
  fracXover = 0.33
  fracYover = 0.33
  fracOverMax = 0.8
  wallMask = 0.
  wallInterp = 0.
  wallfft = 0.
  numSkip = 0
  breaking = .false.
  imagesBinned = 1
  ifReadXfs = 0
  critNonBlank = 0.7
  fillTaperFrac = 0.1
  maxTrackGap = 5
  lenTemp = 1000000
  iAntiFiltType = 0
  reverseOrder = .false.
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'tiltxcorr', &
      'ERROR: TILTXCORR - ', .true., 3, 1, 1, numOptArg, &
      numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0

  if (PipGetInOutFile('InputFile', 1, 'Image input file', inFile) &
      .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
  call imopen(1, inFile, 'RO')
  call irdhdr(1, nxyz, mxyz, mode, dmin2, dmax2, dmean2)
  izStart = 1
  izEnd = nz

  allocate(f(2, 3, nz), tilt(nz), ixPcList(nz), iyPcList(nz), izPcList(nz), listz(nz), &
      listSkip(nz), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR VIEWS')
  !
  if (pipinput) then
    ifPip = 1
    plFile = ' '
    ierr = PipGetString('PieceListFile', plFile)
    ierr = PipGetLogical('VerboseOutput', verbose)
  else
    write(*,'(1x,a,$)') 'Piece list file if there is one,'// &
        ' otherwise Return: '
    read(*,101) plFile
101 format(a)
  endif
  call read_piece_list2(plFile, ixPcList, iyPcList, izPcList, numPcList, nz)
  !
  ! if no pieces, set up mocklist
  !
  if (numPcList == 0) then
    do i = 1, nz
      ixPcList(i) = 0
      iyPcList(i) = 0
      izPcList(i) = i - 1
    enddo
    numPcList = nz
  endif
  if (numPcList .ne. nz) then
    write(*,'(/,a,i5,a,i5)') 'ERROR: TILTXCORR - Piece list should have an '// &
        'entry for each image; nz =', nz, ', # in piece list =', numPcList
    call exit(1)
  endif
  call fill_listz(izPcList, numPcList, listz, numViews)
  call checkList(ixPcList, numPcList, 1, nx, minXpiece , numXpieces, nxOverlap)
  call checkList(iyPcList, numPcList, 1, ny, minYpiece , numYpieces, nyOverlap)
  if (numXpieces * numYpieces > 1) call exitError( &
      'Program will not work with montages; '// &
      'blend images into single frames first')

  if (numViews .ne. nz .or. listz(1) .ne. 0 .or. listz(numViews) .ne. nz - 1) then
    write(*,'(/,a,i5)') 'ERROR: TILTXCORR - The piece list should specify', &
        ' all Z values from 0 to' , nz - 1
    call exit(1)
  endif
  !
  ! Get the output file
  if (PipGetInOutFile('OutputFile', 2, 'Output file for transforms', &
      xfFileOut) .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
  !
  call get_tilt_angles(numViews, 3, tilt, nz, ifPip)
  if (numViews .ne. nz) then
    write(*,'(/,a,i5,a,i5,a)') 'ERROR: TILTXCORR - There must be a tilt angle for' &
        //' each image: nz =', nz, ', but there are', numViews, ' tilt angles'
    call exit(1)
  endif
  !
  ! DNM 4/28/02: figure out binning now, and fix this to send setctf
  ! approximately correct nx and ny instead of nxpad and nypad which
  ! don't yet exist
  ! DNM 7/11/03: better fix is to wait to get ctf until pad size is known
  ! DNM 11/1/5/03: and wait to get binning so big padding can be used
  !
  if (pipinput) then
    if (PipGetString('TestOutput', imFileOut) == 0) then
      call imopen(3, imFileOut, 'NEW')
      call itrhdr(3, 1)
      ifImOut = 1
    endif
    ierr = PipGetFloat('RotationAngle', rotAngle)
    ierr = PipGetFloat('FilterRadius1', radius1)
    ierr = PipGetFloat('FilterRadius2', radius2)
    ierr = PipGetFloat('FilterSigma1', sigma1)
    ierr = PipGetFloat('FilterSigma2', sigma2)
    ierr = PipGetBoolean('ExcludeCentralPeak', ifExclude)
    ierr = PipGetTwoIntegers('BordersInXandY', nxTrim, nyTrim)
    ierr = PipGetTwoIntegers('ShiftLimitsXandY', limitShiftX, limitShiftY)
    ierr = PipGetInteger('IterateCorrelations', numIter)
    numIter = max(1, min(6, numIter))
    ierr = PipGetTwoIntegers('LengthAndOverlap', lenContour, minContOverlap)
    if (lenContour > 0) lenContour = max(3, lenContour)
    minContOverlap = min(max(1, minContOverlap), lenContour - 2)
    ixStart = nxTrim
    ixEnd = nx - 1 - nxTrim
    iyStart = nyTrim
    iyEnd = ny - 1 - nyTrim
    ierr = PipGetTwoIntegers('XMinAndMax', ixStart, ixEnd)
    ierr = PipGetTwoIntegers('YMinAndMax', iyStart, iyEnd)
    if (PipGetInteger('BinningToApply', nbinning) == 0) then
      if (nbinning <= 0 .or. nbinning > maxBinning) call exitError &
          ('THE ENTERED VALUE FOR BINNING IS OUT OF RANGE')
    endif
    ierr = PipGetInteger('AntialiasFilter', iAntiFiltType)
    if (nbinning == 1 .or. iAntiFiltType == 1) iAntiFiltType = 0
    if (iAntiFiltType > 0) then
      lenTemp = 10000000
      binRedText = ' Reduction is'
    endif
    lenTemp = min(lenTemp, nx * ny)
    allocate(tmpArray(lenTemp), stat = ierr)
    call memoryError(ierr, "TEMPORARY ARRAY FOR READING DATA")

    ierr = PipGetLogical('ReverseOrder', reverseOrder)
    ierr = PipGetBoolean('CumulativeCorrelation', ifCumulate)
    ierr = PipGetBoolean('NoCosineStretch', ifNoStretch)
    ierr = PipGetBoolean('AbsoluteCosineStretch', ifAbsStretch)
    ierr = PipGetBoolean('LeaveTiltAxisShifted', ifLeaveAxis)
    ierr = PipGetFloat('AngleOffset', angleOffset)
    do iv = 1, numViews
      tilt(iv) = tilt(iv) + angleOffset
    enddo

    ! Get skip or breaking list
    ierr = PipGetString('SkipViews', listString)
    iz = PipGetString('BreakAtViews', listString)
    if (iz + ierr == 0) call exitError( &
        'YOU CANNOT BOTH SKIP VIEWS AND BREAK AT VIEWS')
    if (ierr + iz == 1) then
      call parselist2(listString, listSkip, numSkip, nz)
      breaking = iz == 0
    endif
  else
    write(*,'(1x,a,$)') 'Rotation angle FROM vertical TO the tilt axis: '
    read(5,*) rotAngle

    print *,'Enter filter parameters to filter the correlation, or / for no filter'
    write(6, 1100)
1100 format(' Sigma1, Sigma2, Radius1, Radius2: ',$)
    read(5,*) sigma1, sigma2, radius1, radius2
    !
    write(*,'(1x,a,$)') '1 to exclude central correlation peak due' &
        //' to fixed pattern noise, 0 not to: '
    read(5,*) ifExclude
    !
    write(*,'(1x,a,$)') 'Amounts to trim off each side in X and Y (/ for 0,0):'
    read(5,*) nxTrim, nyTrim
    ixStart = nxTrim
    ixEnd = nx - 1 - nxTrim
    iyStart = nyTrim
    iyEnd = ny - 1 - nyTrim
  endif

  radExclude = 0.
  if (ifExclude == 1) radExclude = 1.1

  if (ixStart < 0 .or. iyStart < 0 .or. ixEnd >= nx .or. iyEnd >= ny .or. &
      ixEnd - ixStart < 24 .or. iyEnd - iyStart < 24) call exitError( &
      'Impossible amount to trim by or incorrect coordinates')

  nxUse = ixEnd + 1 - ixStart
  nyUse = iyEnd + 1 - iyStart
  cosPhi = cosd(rotAngle)
  sinPhi = sind(rotAngle)
  numBound = 0
  if (pipinput) then
    !
    ! Now check if boundary model and load it in
    if (PipGetString('BoundaryModel', inFile) == 0) then
      iobjSeed = 0
      ierr = PipGetInteger('BoundaryObject', iobjSeed)
      call getModelAndFlags('OPENING BOUNDARY MODEL FILE')
      numPoints = 0
      do iobj = 1, max_mod_obj
        call objToCont(iobj, obj_color, imodObj, imodCont)
        !
        ! Use specified object, or any object with closed contours
        if ((iobjSeed > 0 .and. imodObj == iobjSeed) .or. &
            (iobjSeed == 0 .and. iobjFlags(imodObj) == 0) .and. &
            npt_in_obj(iobj) > 2) then
          ipnt = abs(object(1 + ibase_obj(iobj)))
          iv = nint(p_coord(3, ipnt)) + 1
          if (iv >= 1 .and. iv <= numViews) then
            numBound = numBound + 1
            if (numBound > LIMBOUND) call exitError( &
                'TOO MANY BOUNDARY CONTOURS FOR ARRAYS')
            iobjBound(numBound) = iobj
            numInBound(numBound) = npt_in_obj(iobj)
            indBound(numBound) = numPoints
            numPoints = numPoints + npt_in_obj(iobj)
          endif
        endif
      enddo
      if (numBound == 0) call exitError( &
          'NO QUALIFYING BOUNDARY CONTOURS FOUND IN MODEL')
      !
      ! Allocate point array and copy points to arrays
      allocate(xbound(numPoints), ybound(numPoints), stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR BOUNDARY CONTOURS')
      boundXmin = 1.e10
      boundXmax = -1.e10
      boundYmin = 1.e10
      boundYmax = -1.e10
      do ix = 1, numBound
        iobj = iobjBound(ix)
        ipnt = abs(object(1 + ibase_obj(iobj)))
        iv = nint(p_coord(3, ipnt)) + 1
        do ipt = 1, npt_in_obj(iobj)
          ipnt = abs(object(ipt + ibase_obj(iobj)))
          call adjustCoord(tilt(iv), 0., p_coord(1, ipnt) - nx / 2., &
              p_coord(2, ipnt) - ny / 2., x0, y0)
          xbound(indBound(ix) + ipt) = x0 + nx / 2.
          ybound(indBound(ix) + ipt) = y0 + ny / 2.
          boundXmin = min(boundXmin, x0 + nx / 2.)
          boundXmax = max(boundXmax, x0 + nx / 2.)
          boundYmin = min(boundYmin, y0 + ny / 2.)
          boundYmax = max(boundYmax, y0 + ny / 2.)
        enddo
      enddo
      deallocate(iobjFlags)
    endif
    !
    ! Get the view range and the minimum tilt view - needed for evaluating patches
    ierr = PipGetTwoIntegers('StartingEndingViews', izStart, izEnd)
    call findMinimumTiltView()
    !
    ! Now check if doing patches and set up regular grid of them
    if (PipGetTwoIntegers('SizeOfPatchesXandY', nxPatch, nyPatch) == 0) &
        then
      if (ifCumulate .ne. 0) call exitError( &
          'YOU CANNOT USE CUMULATIVE CORRELATION WITH PATCH TRACKING')
      if (breaking) call exitError('YOU CANNOT BREAK AT VIEWS WITH PATCH TRACKING')
      tracking = .true.
      if (nxPatch > nxUse .or. nyPatch > nyUse) &
          call exitError('PATCHES DO NOT FIT WITHIN TRIMMED AREA OF IMAGE')
      ierr = PipGetTwoIntegers('NumberOfPatchesXandY', numXpatch, numYpatch)
      ix = PipGetTwoFloats('OverlapOfPatchesXandY', fracXover, fracYover)
      iobjSeed = PipGetString('SeedModel', inFile)
      if (ierr + iobjSeed + ix < 2) call exitError('YOU MUST ENTER'// &
          ' ONLY ONE OF THE -number, -overlap, OR -seed OPTIONS')
      if (iobjSeed .ne. 0) then
        !
        ! Specify regular array of patches, either by number
        if (ierr == 0) then
          if (numXpatch < 1 .or. numYpatch < 1) call exitError( &
              'NUMBER OF PATCHES MUST BE POSITIVE')
        else
          !
          ! Or by overlap factors
          if (fracXover > fracOverMax .or. fracYover > fracOverMax) &
              call exitError('FRACTIONAL OVERLAP BETWEEN PATCHES IS TOO HIGH')
          xOverlap = fracXover * nxPatch
          yOverlap = fracYover * nyPatch
          numXpatch = max(1, nint((nxUse - xOverlap) / (nxPatch - xOverlap)))
          numYpatch = max(1, nint((nyUse - yOverlap) / (nyPatch - yOverlap)))
        endif
        numPatches = numXpatch * numYpatch
        if (numPatches > LIMPATCH) call exitError('TOO MANY PATCHES FOR ARRAYS')
        xOverlap = (numXpatch * nxPatch - nxUse) / max(1., numXpatch - 1.)
        yOverlap = (numYpatch * nyPatch - nyUse) / max(1., numYpatch - 1.)
        do j = 1, numYpatch
          yval = iyStart + (j - 1) * (nyPatch - yOverlap) + 0.5 * nyPatch
          if (numYpatch == 1) yval = (iyEnd + 1 + iyStart) / 2.
          do i = 1, numXpatch
            ind = i + (j - 1) * numXpatch
            patchCenX(ind) = ixStart + (i - 1) * (nxPatch - xOverlap) + &
                0.5 * nxPatch
            if (numXpatch == 1) patchCenX(ind) = (ixEnd + 1 + ixStart) / 2.
            patchCenY(ind) = yval
          enddo
        enddo
      else
        !
        ! Or get a seed model to specify patches
        ierr = PipGetInteger('SeedObject', iobjSeed)
        call getModelAndFlags('OPENING SEED MODEL FILE')
        numPatches = 0
        do iobj = 1, max_mod_obj
          call objToCont(iobj, obj_color, imodObj, imodCont)
          !
          ! Use specified object, or any object with scattered points
          if ((iobjSeed > 0 .and. imodObj == iobjSeed) .or. &
              (iobjSeed == 0 .and. iobjFlags(imodObj) == 2)) then
            do ipt = 1, npt_in_obj(iobj)
              ipnt = abs(object(ipt + ibase_obj(iobj)))
              iv = nint(p_coord(3, ipnt)) + 1
              !
              ! Get point down to zero degrees and make sure patch fits
              if (iv >= 1 .and. iv <= numViews) then
                call adjustCoord(tilt(iv), 0., p_coord(1, ipnt) - nx / 2., &
                    p_coord(2, ipnt) - ny / 2., x0, y0)
                x0 = x0 + nx / 2.
                y0 = y0 + ny / 2.
                if (nint(x0) - nxPatch / 2 >= ixStart .and. &
                    nint(x0) + nxPatch / 2 <= ixEnd .and. &
                    nint(y0) - nyPatch / 2 >= iyStart .and. &
                    nint(y0) + nyPatch / 2 <= iyEnd) then

                  numPatches = numPatches + 1
                  if (numPatches > LIMPATCH) call exitError( &
                      'TOO MANY POINTS IN SEED MODEL FOR ARRAYS')
                  patchCenX(numPatches) = x0
                  patchCenY(numPatches) = y0
                endif
              endif
            enddo
          endif
        enddo
        if (numPatches == 0) call exitError('NO QUALIFYING POINTS FOUND' &
            //' IN SEED MODEL; SPECIFY OBJECT OR MAKE IT SCATTERED POINTS')
        deallocate(iobjFlags)
      endif
      !
      ! Now eliminate patches outside boundary model
      if (numBound > 0) then
        !
        ! Evaluate each patch for fraction inside boundary
        ind = 0
        do ipatch = 1, numPatches
          numInside = 0
          do ix = 1, 32
            x0 = patchCenX(ipatch) + nxPatch * (ix - 16.5) / 32.
            do iy = 1, 32
              y0 = patchCenY(ipatch) + nyPatch * (iy - 16.5) / 32.
              do iv = 1, numBound
                if (inside(xbound(indBound(iv) + 1), ybound(indBound(iv) + 1), &
                    numInBound(iv), x0, y0)) then
                  numInside = numInside + 1
                  exit
                endif
              enddo
            enddo
          enddo
          ! print *, patchCenX(ipatch), patchCenY(ipatch), numInside / 1024.
          !
          ! If enough points are inside, keep the patch
          if (numInside / 1024. >= critInside) then
            ind = ind + 1
            patchCenX(ind) = patchCenX(ipatch)
            patchCenY(ind) = patchCenY(ipatch)
          endif
        enddo
        numPatches = ind
        if (ind == 0) call exitError( &
            'NO PATCHES ARE SUFFICIENTLY INSIDE THE BOUNDARY CONTOUR(S)')
      endif
      !
      ! Get prealign transforms, then eliminate patches that have too much blank area
      ! on starting view
      ifReadXfs = 1 - PipGetString('PrealignmentTransformFile', plFile)
      if (ifReadXfs .ne. 0) then
        ierr = PipGetInteger('ImagesAreBinned', imagesBinned)
        allocate(dxPreali(nz), dyPreali(nz), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR PREALIGN TRANSFORMS')
        call dopen(3, plFile, 'ro', 'f')
        call xfrdall2(3, f, iv, nz, ierr)
        if (ierr == 2) call exitError('READING TRANSFORM FILE')
        if (iv .ne. nz) call exitError( &
            'NOT ENOUGH TRANSFORMS IN PREALIGN TRANSFORM FILE')
        dxPreali(1:nz) = f(1, 3, 1:nz) / imagesBinned
        dyPreali(1:nz) = f(2, 3, 1:nz) / imagesBinned
        !
        ! Scan around the minimum tilt for the view with the most patches left
        numBest = 0
        do iv = minTilt - 2, minTilt + 2
          if (iv >= izStart .and. iv <= izEnd .and. &
              (breaking .or. numberInList(iv, listSkip, numSkip, 0) == 0)) then
            call markUsablePatches(iv, ind)
            if (verbose) print *,'Usable patches for view: ', iv, ind
            if (ind > numBest .or. (ind == numBest .and. &
                abs(iv - minTilt) < abs(indBest - minTilt))) then
              numBest = ind
              indBest = iv
            endif
          endif
        enddo
        !
        ! Mark them for real and copy the usable ones down
        minTilt = indBest
        call markUsablePatches(minTilt, ind)
        ind = 0
        do ipatch = 1, numPatches
          if (tmpArray(ipatch) > 0) then
            ind = ind + 1
            patchCenX(ind) = patchCenX(ipatch)
            patchCenY(ind) = patchCenY(ipatch)
          endif
        enddo
        numPatches = ind
        if (ind == 0) call exitError( &
            'NO PATCHES HAVE SUFFICIENT IMAGE DATA NEAR THE MINIMUM TILT VIEW')
      endif

      nxUse = nxPatch
      nyUse = nyPatch
    elseif (numBound > 0) then
      !
      ! For ordinary correlation with boundary model, adjust ixst etc
      if (boundXmin - 2. > ixStart) ixStart = boundXmin - 2.
      if (boundXmax + 2. < ixEnd) ixEnd = ceiling(boundXmax + 2.)
      if (boundYmin - 2. > iyStart) iyStart = boundYmin - 2.
      if (boundYmax + 2. < iyEnd) iyEnd = ceiling(boundYmax + 2.)
      nxUse = ixEnd + 1 - ixStart
      nyUse = iyEnd + 1 - iyStart
      if (nxUse < 24 .or. nyUse < 24) call exitError( &
          'REGION INSIDE BOUNDARY IS TOO SMALL')
      allocate(xtfsBound(numPoints), ytfsBound(numPoints), stat = ierr)
      call memoryError(ierr, 'TFS BOUNDARY ARRAYS')
      write(*,'(a,2i7,a,2i7)') 'The area loaded will be X:', ixStart, ixEnd, ' Y:', &
          iyStart, iyEnd
    endif
  endif
  !
  ! Set up one patch if no tracking
  if (tracking) then
    print *,numPatches, ' patches will be tracked'
  else
    numPatches = 1
    patchCenX(1) = (ixEnd + 1 + ixStart) / 2.
    patchCenY(1) = (iyEnd + 1 + iyStart) / 2.
  endif
  !
  ! OK, back to main image operations
  ! determine padding
  !
  nxBorder = max(5, min(20, nint(0.05 * nxUse)))
  nyBorder = max(5, min(20, nint(0.05 * nyUse)))
  if (pipinput) then
    ierr = PipGetTwoIntegers('PadsInXandY', nxBorder, nyBorder)
  else
    write(*,'(1x,a,2i4,a,$)') 'Amounts to pad images on each side ' &
        //'in X and Y (/ for', nxBorder, nyBorder, '): '
    read(*,*) nxBorder, nyBorder
  endif
  !
  ! get a binning based on the padded size so that large padding is
  ! possible
  !
  if (nbinning == 0) then
    nbinning = (max(nxUse + 2 * nxBorder, nyUse + 2 * nyBorder) + maxBinSize-1) /  &
        maxBinSize
    !
    ! If the binning is bigger than 4, find the minimum binning needed
    ! to keep the used image within bounds
    if (nbinning > 4) then
      nbinning = 0
      i = 4
      do while (i <= maxBinning .and. nbinning == 0)
        if ((niceFrame((nxUse + 2 * nxBorder) / i, 2, 19) + 2) * &
            niceFrame((nyUse + 2 * nyBorder) / i, 2, 19) < IDIM2) nbinning = i
        i = i + 1
      enddo
      if (nbinning == 0) call exitError('IMAGE AREA TOO'// &
          ' LARGE FOR ARRAYS; INCREASE THE BORDER TO TRIM OFF')
    endif
  else if ((niceFrame((nxUse + 40) / nbinning, 2, 19) + 2) * &
      niceFrame((nyUse + 40) / nbinning, 2, 19) > IDIM2) then
    call exitError('IMAGE AREA TOO'// &
        ' LARGE; INCREASE THE BINNING OR THE BORDER TO TRIM OFF')
  endif

  nxUseBin = nxUse / nbinning
  nyUseBin = nyUse / nbinning

  nxPad = niceFrame((nxUse + 2 * nxBorder) / nbinning, 2, 19)
  nyPad = niceFrame((nyUse + 2 * nyBorder) / nbinning, 2, 19)
  if ((nxPad + 2) * nyPad > IDIM2) call exitError( &
      'PADDED IMAGE TOO BIG, TRY LESS PADDING')

  write(*,'(/,a,i3,a,i5,a,i5)') binRedText, nbinning, &
      ';  padded, reduced size is', nxPad, ' by', nyPad
  !
  ! Now that padded size exists, get the filter ctf
  !
  call setCtfwSR(sigma1, sigma2, radius1, radius2, ctfp, nxPad, nyPad, deltaCTF)
  !
  ! Set up tapering
  !
  nxTaper = max(5, min(100, nint(0.1 * nxUse)))
  nyTaper = max(5, min(100, nint(0.1 * nyUse)))
  if (pipinput) then
    ierr = PipGetTwoIntegers('TapersInXandY', nxTaper, nyTaper)
  else
    write(*,'(1x,a,2i4,a,$)') 'Widths over which to taper images' &
        //' in X and Y (/ for', nxTaper, nyTaper, '): '
    read(*,*) nxTaper, nyTaper
  endif
  nxTaper = nxTaper / nbinning
  nyTaper = nyTaper / nbinning
  limitShiftX = (limitShiftX  + nbinning / 2) / nbinning
  limitShiftY = (limitShiftY  + nbinning / 2) / nbinning
  if (limitShiftX <= 0 .or. limitShiftY <= 0) call exitError( &
      'SHIFT LIMITS MUST BE POSITIVE')
  !
  ! Get view range in old sequential input (needed earlier when pip)
  if (.not. pipinput) then
    write(*,'(1x,a,$)') 'Starting and ending views to do (first is 1), or / for all: '
    read(*,*) izStart, izEnd
    call findMinimumTiltView()
  endif
  !
  ! Get max tilt angle and check for appropriateness of cosine stretch
  useMax = 0.
  do iv = izStart, izEnd
    useMax = max(useMax, abs(tilt(iv)))
  enddo
  if (ifNoStretch == 0 .and. useMax > cosStrMaxTilt) call exitError &
      ('MAXIMUM TILT ANGLE IS TOO HIGH TO USE COSINE STRETCHING')
  !
  do kk = 1, nz
    call xfunit(f(1, 1, kk), 1.0)
  enddo
  !
  if (tracking) then
    nFillTaper = min(nxPatch / (4 * nbinning), nyPatch / (4 * nbinning), 100, max(10, &
        nint(fillTaperFrac * (nxPatch + nyPatch) / 2.) / nbinning))
    allocate(xmodel(numPatches, izStart:izEnd), ymodel(numPatches, izStart:izEnd), &
        stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR TRACKED POINTS')
    xmodel = -1.e10                         ! Vector operations
    ymodel = -1.e10
    if (lenContour <= 0) lenContour = izEnd + 1 - izStart
    numCont = (izEnd - izStart) / (lenContour - minContOverlap) + 1
    if (numCont > max_obj_num) call exitError('TOO MANY CONTOURS FOR MODEL ARRAYS')
    if (numCont * lenContour > max_pt) call exitError( &
        'TOO MANY TOTAL POINTS FOR MODEL ARRAYS')
  endif
  !
  if (imFileOut .ne. ' ') then
    nzOut = izEnd - izStart
    if (ifImOut .ne. 0) nzOut = nzOut * 3
    call ialsiz_sam_cel(3, nxPad, nyPad, nzOut)
    dmeanSum = 0.
    dmax = -1.e10
    dmin = 1.e10
    nzOut = 0
  endif
  !
  ! get centered starting and ending coordinates to which box offsets
  ! will be added for loading
  !
  ixCenStart = (nx - nxUse) / 2
  ixCenEnd = ixCenStart + nxUse - 1
  iyCenStart = (ny - nyUse) / 2
  iyCenEnd = iyCenStart + nyUse - 1
  !
  ! Report axis offset if leaving axis at box
  xBoxOfs = (ixEnd + 1 + ixStart - nx) / 2.
  yBoxOffset = (iyEnd + 1 + iyStart - ny) / 2.
  if (ifLeaveAxis .ne. 0) write(*,'(/,a,f8.1,a)') &
      ' The tilt axis is being left at a shift of', &
      xBoxOfs * cosPhi + yBoxOffset * sinPhi, ' pixels from center'

  ! print *,xBoxOfs, yBoxOfs, ixstCen, ixndCen, iystCen, iyndCen
  !
  ! set up for one forward loop through data - modified by case below
  !
  numLoops = 1
  loopDir = 1
  !
  ! set up for first or only loop
  !
  ! print *,mintilt, izstart, izend
  if (minTilt >= izEnd) then
    ivStart = izEnd - 1
    ivEnd = izStart
    loopDir = -1
  else if (minTilt < izEnd .and. minTilt > izStart) then
    ivStart = minTilt + 1
    ivEnd = izEnd
    numLoops = 2
  else
    ivStart = izStart + 1
    ivEnd = izEnd
  endif

  do iloop = 1, numLoops
    do i = 1, nxUseBin * nyUseBin
      sumArray(i) = 0.
    enddo
    unstretchDx = 0.
    unstretchDy = 0.
    xpeak = 0.
    ypeak = 0.
    cumXshift = 0.
    cumYshift = 0.
    lastNotSkipped = minTilt
    call xfunit(fUnit, 1.0)

    do iview = ivStart, ivEnd, loopDir

      ivCur = iview
      ivRef = iview - loopDir
      !
      ! Test for skipping of this view, or of skipping of previous view when going
      ! backwards and breaking
      ivSkip = iview
      if (breaking .and. loopDir < 0) ivSkip = iview + 1
      !
      ! If view is on skip/break list, copy the previous cumulative transform
      if (numberInList(ivSkip, listSkip, numSkip, 0) .ne. 0) then
        call xfcopy(f(1, 1, ivRef), f(1, 1, ivCur))
        cycle
      endif
      !
      ! Align to last one not skipped unless breaking alignment, then revise last
      if (numSkip > 0 .and. .not. breaking) ivRef = lastNotSkipped
      lastNotSkipped = iview
      cosView = cosd(tilt(iview))
      !
      ! get the stretch - if its less than 1., invert everything
      ! unless doing cumulative or tracking, where it has to do in order
      ! DNM 9/24/09: It seems like all kinds of things may not work if this
      ! inversion ever happens...  9/23/11: basic correlation works but not tracking
      idir = 1
      stretch = 1.
      if (ifNoStretch == 0 .and. abs(cosView) > 0.01) then
        stretch = cosd(tilt(ivRef)) / cosView
        if (ifCumulate .ne. 0 .and. ifAbsStretch .ne. 0) &
            stretch = cosd(tilt(minTilt)) / cosView
      endif
      if (stretch < 1. .and. ifCumulate == 0 .and. .not. tracking) then
        idir = -1
        stretch = 1. / stretch
        izTmp = ivCur
        ivCur = ivRef
        ivRef = izTmp
        cosView = cosd(tilt(ivCur))
      endif
      if (verbose) print *,'idir, stretch, ivRef, ivCur', idir, stretch, ivRef, ivCur
      !
      ! Loop on the patches
      ivRefBase  = ivRef
      do ipatch = 1, numPatches
        ivRef = ivRefBase
        !
        ! Get box offset, first by tilt-foreshortened center position
        ! rounded to nearest integer
        cenx = patchCenX(ipatch) - nx / 2.
        ceny = patchCenY(ipatch) - ny / 2.
        call adjustCoord(0., tilt(ivRef), cenx, ceny, x0, y0)
        ixBoxRef = max(-ixCenStart, min(nx - 1 - ixCenEnd, nint(x0)))
        iyBoxRef = max(-iyCenStart, min(ny - 1 - iyCenEnd, nint(y0)))
        baseTilt = 0.
        if (iview == ivStart) then
          ixBoxStart = ixBoxRef
          iyBoxStart = iyBoxRef
        endif
        !
        ! If tracking, initialize model position on first time or get  reference
        ! box from model point
        if (tracking) then
          if (iloop == 1 .and. iview == ivStart) then
            xmodel(ipatch, ivRef) = ixBoxRef + ixCenStart + nxUse / 2.
            ymodel(ipatch, ivRef) = iyBoxRef + iyCenStart + nyUse / 2.
          else
            !
            ! Back up reference to last point that was tracked unless it is too
            ! far back
            do while (xmodel(ipatch, ivRef) < -1.e9 .and. &
                abs(ivRef - ivCur) <= maxTrackGap)
              ivRef = ivRef - loopDir
            enddo
            if (xmodel(ipatch, ivRef) < -1.e9) then
              if (verbose) print *,'Stopping tracking of patch #', ipatch
              cycle
            endif
            if (ivRef .ne. ivRefBase) then
              if (verbose) print *,'Patch ', ipatch, ' tracking to reference', ivRef
              stretch = 1.
              if (ifNoStretch == 0 .and. abs(cosView) > 0.01) &
                  stretch = cosd(tilt(ivRef)) / cosView
            endif
            !
            ! Center the reference box on the model point
            x0 = xmodel(ipatch, ivRef) - (ixCenStart + nxUse / 2.)
            y0 = ymodel(ipatch, ivRef) - (iyCenStart + nyUse / 2.)
            ixBoxRef = max(-ixCenStart, min(nx - 1 - ixCenEnd, nint(x0)))
            iyBoxRef = max(-iyCenStart, min(ny - 1 - iyCenEnd, nint(y0)))
            cenx = ixBoxRef
            ceny = iyBoxRef
            baseTilt = tilt(ivRef)
          endif
        endif
        !
        ! Now get box offset of current view by adjusting either the zero
        ! degree position (so this will match old behavior of program)
        ! or the box offset on reference view
        call adjustCoord(baseTilt, tilt(ivCur), cenx, ceny, x0, y0)
        ixBoxCur = max(-ixCenStart, min(nx - 1 - ixCenEnd, nint(x0)))
        iyBoxCur = max(-iyCenStart, min(ny - 1 - iyCenEnd, nint(y0)))
        if (verbose) print *,'Box offsets', ipatch, ixBoxRef, iyBoxRef, ixBoxCur, iyBoxCur
        !
        ! Now that reference and stretch is fixed, get a limited cosine ratio to
        ! use below, get the transforms and look up the Z's in file
        cosRatio = abs(cosView) / max(abs(cosView), abs(cosd(tilt(ivRef))), 1.e-6)
        call rotmagstr_to_amat(0., 1., stretch, rotAngle, fs)
        fs(1, 3) = 0.
        fs(2, 3) = 0.
        call xfinvert(fs, fsInv)
        do i = 1, nz
          if (izPcList(i) + 1 == ivRef) izLast = i - 1
          if (izPcList(i) + 1 == ivCur) izCur = i - 1
        enddo
        ! print *,izlast, izcur, stretch
        !
        ! If tracking and prexg's are available, evaluate need for tapering and
        ! skip the patch if it has below criterion image data
        taperRef = .false.
        taperCur = .false.
        if (ifReadXfs .ne. 0) then
          ix = min(nx - 2, nx + nint(dxPreali(ivRef)) - 2, ixCenStart + ixBoxRef +  &
              nxPatch) - max(2, nint(dxPreali(ivRef)) + 2, ixCenStart + ixBoxRef)
          iy = min(ny - 2, ny + nint(dyPreali(ivRef)) - 2, iyCenStart + iyBoxRef +  &
              nyPatch) - max(2, nint(dyPreali(ivRef)) + 2, iyCenStart + iyBoxRef)
          taperRef = ix < nxPatch .or. iy < nyPatch
          ix = min(nx - 2, nx + nint(dxPreali(ivCur)) - 2, ixCenStart + ixBoxCur +  &
              nxPatch) - max(2, nint(dxPreali(ivCur)) + 2, ixCenStart + ixBoxCur)
          iy = min(ny - 2, ny + nint(dyPreali(ivCur)) - 2, iyCenStart + iyBoxCur +  &
              nyPatch) - max(2, nint(dyPreali(ivCur)) + 2, iyCenStart + iyBoxCur)
          taperCur = ix < nxPatch .or. iy < nyPatch
          if (ix < 0 .or. iy < 0 .or. &
              ix * iy < critNonBlank * nxPatch * nyPatch) then
            if (verbose) print *,'Skipping tracking of patch', ipatch
            cycle
          endif
          if (verbose .and. taperRef) print *,'Tapering reference patch #', ipatch
          if (verbose .and. taperCur) print *,'Tapering current patch #', ipatch

        endif
        xpeakFrac = 0.
        ypeakFrac = 0.
        !
        ! If correlating inside boundary contour, transform contours
        if (.not. tracking .and. numBound > 0) then
          do ix = 1, numBound
            xtfsBmin(ix) = 1.e10
            xtfsBmax(ix) = -1.e10
            ytfsBmin(ix) = 1.e10
            ytfsBmax(ix) = -1.e10
            do j = 1, numInBound(ix)
              ind = j + indBound(ix)
              call adjustCoord(0., tilt(ivCur), xbound(ind) - nx / 2., &
                  ybound(ind) - ny / 2., x0, y0)
              xtfsBound(ind) = (x0 + nx / 2. - (ixCenStart + ixBoxCur)) / nbinning
              ytfsBound(ind) = (y0 + ny / 2. - (iyCenStart + iyBoxCur)) / nbinning
              xtfsBmin(ix) = min(xtfsBmin(ix), xtfsBound(ind))
              ytfsBmin(ix) = min(ytfsBmin(ix), ytfsBound(ind))
              xtfsBmax(ix) = max(xtfsBmax(ix), xtfsBound(ind))
              ytfsBmax(ix) = max(ytfsBmax(ix), ytfsBound(ind))
            enddo
            ! print *,ix, xtfsBmin(ix), xtfsBmax(ix), ytfsBmin(ix), ytfsBmax(ix)
          enddo
        endif
        !
        do iter = 1, numIter
          !
          ! get "current" into array, stretch into brray, pad it
          !
          call readBinnedOrReduced(1, izCur, array, ixCenStart + ixBoxCur, &
              iyCenStart + iyBoxCur, taperCur)
          !
          ! 7/11/03: We have to feed the interpolation the right mean or
          ! it will create a bad edge mean for padding
          !
          call iclden(array, nxUseBin, nyUseBin, 1, nxUseBin, 1, nyUseBin, &
              useMin, useMax, useMean)
          if (.not. tracking .and. numBound > 0) then
            wallStart = wallTime()
            call maskOutsideBoundaries(array, nxUseBin, nyUseBin)
            wallMask = wallMask + wallTime() - wallStart
          endif
          wallStart = wallTime()
          call cubInterp(array, brray, nxUseBin, nyUseBin, nxUseBin, nyUseBin, fs, &
              nxUseBin / 2., nyUseBin / 2., xpeakFrac, ypeakFrac , 1., useMean, 0)
          wallInterp = wallInterp + wallTime() - wallStart
          call taperInPad(brray, nxUseBin, nyUseBin, brray, nxPad + 2, nxPad, &
              nyPad, nxTaper, nyTaper)
          !
          call meanZero(brray, nxPad + 2, nxPad, nyPad)
          !
          ! get "last" into array, just pad it there
          !
          call readBinnedOrReduced(1, izLast, array, ixCenStart + ixBoxRef, &
              iyCenStart + iyBoxRef, taperRef)
          
          if (ifCumulate .ne. 0) then
            if (iter == 1) then
              !
              ! if accumulating, transform image by last shift, add it to
              ! sum array, then taper and pad into array
              !
              if (ifAbsStretch == 0) then
                call xfunit(fUnit, 1.0)
              else
                unstretchDx = xpeak
                unstretchDy = ypeak
              endif
              if (verbose) print *,'cumulating usdx,usdy,xpeak,ypeak',  &
                  unstretchDx, unstretchDy, xpeak, ypeak
              call iclden(array, nxUseBin, nyUseBin, 1, nxUseBin, 1, nyUseBin, &
                  useMin, useMax, useMean)
              call cubInterp(array, crray, nxUseBin, nyUseBin, nxUseBin, nyUseBin, &
                  fUnit, nxUseBin / 2., nyUseBin / 2., unstretchDx, unstretchDy , 1., &
                  useMean, 0)
              do i = 1, nxUseBin * nyUseBin
                sumArray(i) = sumArray(i) + crray(i)
              enddo
            endif
            call taperInPad(sumArray, nxUseBin, nyUseBin, array, nxPad + 2, &
                nxPad, nyPad, nxTaper, nyTaper)
          else
            call taperInPad(array, nxUseBin, nyUseBin, array, nxPad + 2, &
                nxPad, nyPad, nxTaper, nyTaper)
          endif
          if (ifImOut .ne. 0) then
            do isOut = 1, 2
              if (isOut == 1) then
                call irepak(crray, array, nxPad + 2, nyPad, &
                    0, nxPad-1, 0, nyPad-1)
              else
                call irepak(crray, brray, nxPad + 2, nyPad, &
                    0, nxPad-1, 0, nyPad-1)
              endif
              if (mode .ne. 2) then
                call isetdn(crray, nxPad, nyPad, mode, 1, nxPad, 1, nyPad, dmin2, &
                    dmax2, dmean3)
              else
                call iclden(crray, nxPad, nyPad, 1, nxPad, 1, nyPad, dmin2, dmax2, &
                    dmean3)
              endif
              call iwrsec(3, crray)
              nzOut = nzOut + 1
              !
              dmax = max(dmax, dmax2)
              dmin = min(dmin, dmin2)
              dmeanSum = dmeanSum + dmean3
            enddo
          endif
          call meanZero(array, nxPad + 2, nxPad, nyPad)

          !
          ! print *,'taking fft'
          wallStart = wallTime()
          call todfft(array, nxPad, nyPad, 0)
          call todfft(brray, nxPad, nyPad, 0)
          !
          ! multiply array by complex conjugate of brray, put back in array
          !
          ! print *,'multiplying arrays'
          call conjugateProduct(array, brray, nxPad, nyPad)
          !
          if (deltaCTF .ne. 0.) call filterPart(array, array, nxPad, nyPad, ctfp, &
              deltaCTF)
          ! print *,'taking back fft'
          call todfft(array, nxPad, nyPad, 1)
          wallfft = wallfft + wallTime() - wallStart
          !
          ! the factor here determines the balance between accepting a
          ! spurious peak and rejecting a real peak because it falls in
          ! the streak. The spurious peak could be as far as 0.5 out but
          ! is much more likely to be within 0.25 of the maximum
          ! displacement
          !
          streak = 0.25 * (stretch - 1.0) * nxUse
          call peakFind(array, nxPad + 2, nyPad, xpeakTmp, ypeakTmp, peakVal, &
              radExclude, rotAngle, streak, limitShiftX, limitShiftY)
          xpeakCum = xpeakTmp + xpeakFrac
          xpeakFrac = xpeakCum - nint(xpeakCum)
          ypeakCum = ypeakTmp + ypeakFrac
          ypeakFrac = ypeakCum - nint(ypeakCum)
          if (verbose) write(*,'(i3,2f8.2,g15.6,4f8.2)') iter, xpeakCum, ypeakCum, &
              peakVal, xpeakTmp - nint(xpeakTmp), ypeakTmp - nint(ypeakTmp), &
              xpeakFrac, ypeakFrac
          !
          ! Skip out and restore last peak if the peak strength is less
          if (iter > 1 .and. peakVal < peakLast) then
            xpeakCum = xpeakLast
            ypeakCum = yPeakLast
            peakVal = peakLast
            exit
          endif
          xpeakLast = xpeakCum
          yPeakLast = ypeakCum
          peakLast = peakVal
          !
          ! Skip out if we are close to 0 interpolation
          ! (gfortran did not like exit on the if statement)
          if (sqrt((xpeakTmp - nint(xpeakTmp))**2 + &
              (ypeakTmp - nint(ypeakTmp))**2) < peakFracTol) then
            exit
          endif
        enddo
        xpeak = xpeakCum
        ypeak = ypeakCum
        !
        ! DNM 5/2/02: only destretch the shift if the current view was
        ! stretched.  Also, put the right sign out in the standard output
        !
        if (idir > 0) then
          call xfapply(fsInv, 0., 0., xpeak, ypeak, unstretchDx, unstretchDy)
        else
          unstretchDx = xpeak
          unstretchDy = ypeak
        endif
        if (verbose) print *,'peak usdx,usdy,xpeak,ypeak',  &
            unstretchDx, unstretchDy, xpeak, ypeak
        !
        ! compensate for the box offsets of reference and current view,
        ! where the reference is the starting view if accumulating
        !
        ixBoxForAdj = ixBoxRef
        iyBoxForAdj = iyBoxRef
        if (ifCumulate .ne. 0) then
          ixBoxForAdj = ixBoxStart
          iyBoxForAdj = iyBoxStart
        endif

        ! ivRef = iview - loopDir
        ! if (ifcumulate .ne. 0) ivRef = ivStart - loopDir
        xshift = idir * nbinning * unstretchDx + ixBoxForAdj - ixBoxCur
        yshift = idir * nbinning * unstretchDy + iyBoxForAdj - iyBoxCur
        if (tracking) then
          !
          ! compensate the shift that is to be accumulated for the
          ! difference between the center of the box and the model point
          ! on reference view
          cumXcenter = xmodel(ipatch, ivRef)
          cumYcenter = ymodel(ipatch, ivRef)
          xModOffset = cumXcenter - (ixBoxRef + ixCenStart + nxUse / 2.)
          yModOffset = cumYcenter - (iyBoxRef + iyCenStart + nyUse / 2.)
          cumXrot = xModOffset * cosPhi + yModOffset * sinPhi
          xAdjust = cumXrot *  (1. - cosRatio)
          !
          ! Subtract adjusted shift to get new model point position
          xmodel(ipatch, iview) = cumXcenter - (xshift + xAdjust * cosPhi)
          ymodel(ipatch, iview) = cumYcenter - (yshift + xAdjust * sinPhi)
        endif
        !
        ! if not leaving axis at center of box, compute amount that
        ! current view must be shifted across tilt axis to be lined up at
        ! center, then rotate that to get shifts in X and Y
        ! This is not due to cosine stretch, it is due to difference in
        ! the box offsets
        if (ifLeaveAxis == 0) then
          xBoxOfs = (ixBoxCur - ixBoxForAdj) * cosPhi + &
              (iyBoxCur - iyBoxForAdj) * sinPhi
          xshift = xshift + xBoxOfs * cosPhi
          yshift = yshift + xBoxOfs * sinPhi
        endif
        !
        ! If not cumulative, adjust the shift to bring the tilt axis
        ! to the true center from the center of last view
        ! If last view was shifted to right to line up with center,
        ! then the true center is to the left of the center of that view
        ! and the tilt axis (and this view) must be shifted to the left
        ! But base this on the cumulative shift of the center of the image
        ! not of the box if axis not at center: so add on the cumulative
        ! amount that would have been added due to box offsets in block
        ! above
        if (ifCumulate == 0 .and. ifNoStretch == 0) then
          xFromCen = cumXshift
          yFromCen = cumYshift
          if (ifLeaveAxis .ne. 0) then
            xFromCen = xFromCen + (ixBoxRef - ixBoxStart)
            yFromCen = yFromCen + (iyBoxRef - iyBoxStart)
          endif
          cumXrot = xFromCen * cosPhi + yFromCen * sinPhi
          xAdjust = cumXrot *  (cosRatio - 1.)
          xshift = xshift + xAdjust * cosPhi
          yshift = yshift + xAdjust * sinPhi
          !
          ! Add to cumulative shift and report and save the absolute shift
          !
          cumXshift = cumXshift + xshift
          cumYshift = cumYshift + yshift
          xshift = cumXshift
          yshift = cumYshift
        endif

        if (.not.tracking) write(*,111) 'View', iview, ', shifts', xshift, &
            yshift, '      peak', peakVal
111     format(a,i4,a,2f10.2,a,g15.6)
        !
        ! DNM 10/22/03: Only do flush for large stacks because of problem
        ! inside shell scripts in Windows/Intel
        !
        if (izEnd - izStart > 2) call flush(6)
        f(1, 3, iview) = xshift
        f(2, 3, iview) = yshift
        call xfcopy(fs, fUnit)
        if (imFileOut .ne. ' ') then
          call packCorr(crray, array, nxPad + 2, nyPad)
          if (mode .ne. 2) then
            call isetdn(crray, nxPad, nyPad, mode, 1, nxPad, 1, nyPad, dmin2, &
                dmax2, dmean3)
          else
            call iclden(crray, nxPad, nyPad, 1, nxPad, 1, nyPad, dmin2, dmax2, &
                dmean3)
          endif
          call iwrsec(3, crray)
          nzOut = nzOut + 1
          if (verbose) print *,'Correlation output at Z = ', nzOut
          !
          dmax = max(dmax, dmax2)
          dmin = min(dmin, dmin2)
          dmeanSum = dmeanSum + dmean3
        endif
      enddo
      if (tracking) write(*,111) 'View', iview, ' processed'
    enddo
    !
    ! set up for second loop
    !
    ivStart = minTilt - 1
    ivEnd = izStart
    loopDir = -1
  enddo
  !
  if (imFileOut .ne. ' ') then
    call ialsiz_sam_cel(3, nxPad, nyPad, nzOut)
    dmean = dmeanSum / nzOut
    call b3ddate(dat)
    call time(tim)
    !
    if (deltaCTF .ne. 0.) filterText = ', filtered'
    titleStr = 'TILTXCORR: stack cosine stretch/correlated '// filterText
    !
    write(titlech, 1500) titleStr, dat, tim
    read(titlech, '(20a4)') (title(kti), kti = 1, 20)
1500 format(a54,2x,a9,2x,a8)

    call iwrhdr(3, title, 1, dmin, dmax, dmean)
    call imclose(3)
  endif
  !
  ! Now adjust transforms of skipped ones so they are always the same as the one below
  if (.not. breaking) then
    lastNotSkipped = -1
    do iv = 1, numViews
      if (numberInList(iv, listSkip, numSkip, 0) == 0) then
        lastNotSkipped = iv
      else if (lastNotSkipped > 0) then
        call xfcopy(f(1, 1, lastNotSkipped), f(1, 1, iv))
      endif
    enddo
  endif
  !
  ! Normal output
  if (.not. tracking) then
    call dopen(1, xfFileOut, 'new', 'f')
    !
    ! If leaving axis at an offset box, output G transforms directly so that
    ! the material in box at zero tilt will stay in box
    if (ifLeaveAxis .ne. 0) then
      do iv = 1, nz
        call xfwrite(1, f(1, 1, iv))
      enddo
    else
      !
      ! Anticipate what xftoxg will do, namely get to transforms with zero
      ! mean, and shift tilt axis to be at middle in that case
      !
      iloop = 1
      cumXshift = 10.
      do while (iloop <= 10 .and. (cumXshift > 0.1 .or. cumYshift > 0.1) &
          .and. ifNoStretch == 0)
        iloop = iloop + 1
        cumXshift = 0.
        cumYshift = 0.
        do iv = 1, nz
          cumXshift = cumXshift - f(1, 3, iv) / nz
          cumYshift = cumYshift - f(2, 3, iv) / nz
        enddo
        ! print *,iloop, ', mean shift', cumXshift, cumYshift
        !
        ! rotate the average shift to tilt axis vertical, adjust the X shift
        ! to keep tilt axis in center and apply shift for all views
        !
        cumXrot = cumXshift * cosPhi + cumYshift * sinPhi
        yshift = -cumXshift * sinPhi + cumYshift * cosPhi
        do iv = 1, nz
          xAdjust = cumXrot * cosd(tilt(iv))
          f(1, 3, iv) = f(1, 3, iv) + xAdjust * cosPhi - yshift * sinPhi
          f(2, 3, iv) = f(2, 3, iv) + xAdjust * sinPhi + yshift * cosPhi
        enddo
      enddo
      !
      ! convert from g to f transforms by taking differences
      !
      do iv = nz, 2, -1
        if (numberInList(iv, listSkip, numSkip, 0) == 0) then
          f(1, 3, iv) = f(1, 3, iv) - f(1, 3, iv - 1)
          f(2, 3, iv) = f(2, 3, iv) - f(2, 3, iv - 1)
        else
          call xfunit(f(1, 1, iv), 1.)
        endif
      enddo
      call xfunit(f(1, 1, 1), 1.)
      do iv = 1, nz
        call xfwrite(1, f(1, 1, iv),*96)
      enddo
    endif
    close(1)
  else
    !
    ! Write Model of tracked points
    ierr = newImod()
    max_mod_obj = 0
    n_point = 0
    !
    ! Go through patches and put points in model structure
    do ipatch = 1, numPatches
      !
      ! Count model points in this patch and divide them up
      ipnt = 0
      ivBase = -1
      do iz = izStart, izEnd
        if (xmodel(ipatch, iz) > -1.e9) then
          ipnt = ipnt + 1
          if (ivBase < 0) ivBase = iz
        endif
      enddo
      lenConts = min(lenContour, ipnt)
      numCont = (ipnt - 1) / (lenConts - minContOverlap) + 1
      lapTotal = numCont * lenConts - ipnt
      lapBase = lapTotal / max(numCont - 1, 1)
      lapRemainder = mod(lapTotal, max(numCont - 1, 1))
      !
      ! Loop through each contour starting at base position
      do i = 1, numCont
        max_mod_obj = max_mod_obj + 1
        obj_color(1, max_mod_obj) = 1
        obj_color(2, max_mod_obj) = 255
        ibase_obj(max_mod_obj) = n_point
        npt_in_obj(max_mod_obj) = lenConts
        iv = ivBase
        ipnt = 0
        do j = 1, lenConts
          do while (xmodel(ipatch, iv) < -1.e9 .and. iv < izEnd)
            iv = iv + 1
          enddo
          n_point = n_point + 1
          p_coord(1, n_point) = xmodel(ipatch, iv)
          p_coord(2, n_point) = ymodel(ipatch, iv)
          do iz = 1, nz
            if (izPcList(iz) + 1 == iv) p_coord(3, n_point) = iz - 1
          enddo
          object(n_point) = n_point
          pt_label(n_point) = 0
          ipnt = ipnt + 1
          !
          ! If hit point for start of overlap, record position
          if (i > lapRemainder .and. ipnt == lenConts + 1 - lapBase .or. &
              i <= lapRemainder .and. ipnt == lenConts - lapBase) ivBase = iv
          iv = iv + 1
        enddo
      enddo
    enddo
    !
    ! Set model properties: open contours, thicken current contour, etc.
    ierr = putModelName('Patch Tracking Model')
    call putImodFlag(1, 1)
    call putImodFlag(1, 10)
    call putSymType(1, 0)
    call putSymSize(1, 7)
    call putLineWidth(1, 2)
    call putObjColor(1, 255, 0, 255)
    call putImodZscale(max(1., min(10., (0.3 * (nx + ny)) / nz)))
    call irtdel(1, delta)
    call irtorg(1, origin(1), origin(2), origin(3))
    call putImageRef(delta, origin)
    ierr = putImodMaxes(nx, ny, nz)
    call scaleModelToImage(1, 1)
    call write_wmod(xfFileOut)
  endif
  call imclose(1)
  !
  ! write(*,'(3(a,f9.3))') 'interpolation', wallinterp, '  fft', wallfft, &
  ! '  masking', wallmask
  write(6, 500)
500 format(' PROGRAM EXECUTED TO END.')
  call exit(0)
96 call exitError('ERROR WRITING TRANSFORMS TO FILE')

CONTAINS

  ! Adjusts centered coordinates XFROM, YFROM from an image at TILTFROM
  ! to an image at TILTTO by rotating to the tilt axis vertical,
  ! adjusting the X coordinate by the ratio of cosines, and rotating back
  !
  subroutine adjustCoord(tiltFrom, tiltTo, xfrom, yfrom, xto, yto)
    implicit none
    real*4 tiltFrom, tiltTo, xfrom, yfrom, xto, yto, xrot, yrot, cosTo, cosFrom
    real*4 tmpRatio
    real*4 tiltToLast/0./, tiltFromLast/0./, cosToLast/1./, cosFromLast/1./
    save tiltToLast, tiltFromLast, cosToLast, cosFromLast
    real*4 cosd
    !
    if (tiltFrom == tiltFromLast) then
      cosFrom = cosFromLast
    else
      cosFrom = cosd(tiltFrom)
      cosFromLast = cosFrom
      tiltFromLast = tiltFrom
    endif
    if (tiltTo == tiltToLast) then
      cosTo = cosToLast
    else
      cosTo = cosd(tiltTo)
      cosToLast = cosTo
      tiltToLast = tiltTo
    endif
    tmpRatio = abs(cosTo) / max(abs(cosFrom), 1.e-6)
    !
    xrot = xfrom * cosPhi + yfrom * sinPhi
    yrot = -xfrom * sinPhi + yfrom * cosPhi
    xrot = xrot * tmpRatio
    xto = xrot * cosPhi - yrot * sinPhi
    yto = xrot * sinPhi + yrot * cosPhi
  end subroutine adjustCoord


  ! Loads a boundary or seed model, scales it, allocates iobjFlags to the
  ! needed size and gets the flags.
  ! iobjFlags must be deallocated after use
  !
  subroutine getModelAndFlags(errMess)
    implicit none
    character*(*) errMess
    logical exist, readSmallMod
    integer*4 getImodObjSize, getImodFlags, numObjTot
    !
    exist = readSmallMod(inFile)
    if (.not.exist) call exitError(errMess)
    numObjTot = getImodObjSize()
    allocate(iobjFlags(numObjTot), stat = ierr)
    call memoryError(ierr, 'OBJECT FLAG ARRAY')
    ierr = getImodFlags(iobjFlags, numObjTot)
    call scale_model(0)
    return
  end subroutine getModelAndFlags


  ! Masks out the area outside of all boundary contours with the mean value
  !
  subroutine maskOutsideBoundaries(arrayMOB, nxMOB, nyMOB)
    implicit none
    integer*4 nxMOB, nyMOB
    real*4 arrayMOB(nxMOB, nyMOB)
    logical outside
    integer*4 taperAtFill
    !
    !$OMP PARALLEL do &
    !$OMP& SHARED(nxMOB, nyMOB, numBound, xtfsBmin, xtfsBmax, ytfsBmin, ytfsBmax) &
    !$OMP& SHARED(arrayMOB, numInBound, useMean, xtfsBound, ytfsBound) &
    !$OMP& PRIVATE(iy, ix, x0, y0, j, outside)
    do iy = 1, nyMOB
      y0 = iy - 0.5
      do ix = 1, nxMOB
        x0 = ix - 0.5
        outside = .true.
        do j = 1, numBound
          if (x0 >= xtfsBmin(j) .and. x0 <= xtfsBmax(j) .and. &
              y0 >= ytfsBmin(j) .and. y0 <= ytfsBmax(j)) then
            if (inside(xtfsBound(indBound(j) + 1), ytfsBound(indBound(j) + 1), &
                numInBound(j), x0, y0)) then
              outside = .false.
              exit
            endif
          endif
        enddo
        if (outside) arrayMOB(ix, iy) = useMean
      enddo
    enddo
    !$OMP end PARALLEL do
    !
    ! Taper if only one boundary
    if (numBound == 1) then
      if (taperAtFill(arrayMOB, nxMOB, nyMOB, 16, 1) .ne. 0) &
          call exitError('GETTING MEMORY FOR TAPERING INSIDE BOUNDARY')
    endif
    return
  end subroutine maskOutsideBoundaries


  ! find minimum tilt view that is not skipped and is in the range being done
  !
  subroutine findMinimumTiltView()
    izStart = max(1, min(nz, izStart))
    izEnd = max(izStart, min(nz, izEnd))
    tiltAtMin = 10000.
    do iv = izStart, izEnd
      if ((breaking .or. numberInList(iv, listSkip, numSkip, 0) == 0) .and. &
          (abs(tilt(iv)) < abs(tiltAtMin) .or.  &
          (reverseOrder .and. abs(tilt(iv)) <= abs(tiltAtMin)))) then
        minTilt = iv
        tiltAtMin = tilt(iv)
      endif
    enddo
    if (tiltAtMin > 9999.) call exitError( &
        'ALL VIEWS IN THE RANGE BEING ALIGNED ARE IN THE LIST TO SKIP')
  end subroutine findMinimumTiltView


  ! Count patches that have usable image area on the given view, return the number
  ! and mark them in tmprray
  !
  subroutine markUsablePatches(indTilt, numUsable)
    integer*4 indTilt, numUsable
    numUsable = 0
    do ipatch = 1, numPatches
      cenx = patchCenX(ipatch) - nx / 2.
      ceny = patchCenY(ipatch) - ny / 2.
      call adjustCoord(0., tilt(indTilt), cenx, ceny, x0, y0)
      ix = min(nx - 1., nx + dxPreali(indTilt), x0 + nx / 2. + nxPatch / 2.) - &
          max(0., dxPreali(indTilt), x0 + nx / 2. - nxPatch / 2.)
      iy = min(ny - 1., ny + dyPreali(indTilt), y0 + ny / 2. + nyPatch / 2.) - &
          max(0., dyPreali(indTilt), y0 + ny / 2. - nyPatch / 2.)
      tmpArray(ipatch) = -1.
      if (ix > 0 .and. iy > 0 .and. &
          ix * iy > critNonBlank * nxPatch * nyPatch) then
        numUsable = numUsable + 1
        tmpArray(ipatch) = 1.
      endif
    enddo
    return
  end subroutine markUsablePatches


  ! readBinnedOrReduced reads an area with the given starting coordinates by binning or
  ! reduction and tapers it if requested
  !
  subroutine readBinnedOrReduced(imUnit, izRead, arrRead, ixStartRead, iyStartRead, &
      taperRead)
    integer*4 imUnit, izRead, ixStartRead, iyStartRead
    real*4 arrRead(*)
    logical*4 taperRead
    integer*4 taperAtFill

    if (iAntiFiltType > 0) then
      call irdReduced(imUnit, izRead, arrRead, nxUseBin, float(ixStartRead), &
          float(iyStartRead), float(nbinning), nxUseBin, nyUseBin, iAntiFiltType - 1, &
          tmpArray, lenTemp, ierr)
      if (ierr == 1) call exitError('ANTIALIAS FILTER TYPE OUT OF RANGE')
      if (ierr > 0) then
        write(listString, '(a,i2,a)') 'CALLING irdReduced TO READ IMAGE (ERROR CODE', &
            ierr, ')'
        call exitError(listString)
      endif
    else
      call irdBinned(imUnit, izRead, arrRead, nxUseBin, nyUseBin, ixStartRead, &
              iyStartRead, nbinning, nxUseBin, nyUseBin, tmpArray, lenTemp, ierr)
    endif
    if (ierr .ne. 0) call exitError('READING IMAGE FILE')
    if (taperRead) then
      if (taperAtFill(arrRead, nxUseBin, nyUseBin, nFillTaper, 1) .ne. 0)  &
          call exitError ('GETTING MEMORY FOR TAPERING FROM FILL AREA IN PATCH')
    endif
    return
  end subroutine readBinnedOrReduced
end program tiltxcorr


! PEAKFIND finds the coordinates of the absolute peak, XPEAK, YPEAK
! in the array ARRAY, which is dimensioned to nx+2 by ny.  It fits
! a parabola in to the three points around the peak in X or Y and gets
! a much better estimate of peak location.
!
subroutine peakFind(array, nxPlus, nyCorr, xpeak, ypeak, peak, radExclude, &
    rotAngle, streak, limitShiftX, limitShiftY)
  implicit none
  integer*4 nxPlus, nyCorr
  real*4 xpeak, ypeak, radExclude, rotAngle, streak
  real*4 array(nxPlus,nyCorr)
  integer*4 nxCorr, ix, iy, idx, idy, lower, ixPeak, iyPeak, limitShiftX, limitShiftY
  real*4 peak, xrot, yrot, cx, y1, y2, y3, cy, cosTheta, sinTheta
  real*4 cosd, sind
  integer*4 indmap
  real*8 parabolicFitPosition
  !
  nxCorr = nxPlus - 2
  !
  ! find peak
  !
  cosTheta = cosd(-rotAngle)
  sinTheta = sind(-rotAngle)
  peak = -1.e30
  xpeak = 0.
  ypeak = 0.
  ixPeak = -1
  do iy = 1, nyCorr
    do ix = 1, nxCorr
      if (array(ix, iy) > peak) then
        !
        ! first check if within limits
        idx = ix - 1
        idy = iy - 1
        if (idx > nxCorr / 2) idx = idx - nxCorr
        if (idy > nyCorr / 2) idy = idy - nyCorr
        if (abs(idx) <= limitShiftX .and. abs(idy) <= limitShiftY) then
          !
          ! Then check if it outside the exclusion region
          xrot = idx * cosTheta - idy * sinTheta
          yrot = idx * sinTheta + idy * cosTheta
          if (abs(yrot) >= radExclude .or. abs(xrot) >= streak + radExclude) then
            !
            ! next check that point is actually a local peak
            !
            lower = 0
            do idx = -1, 1
              do idy = -1, 1
                if (array(ix, iy) < array(indmap(ix + idx, nxCorr), &
                    indmap(iy + idy, nyCorr))) lower = 1
              enddo
            enddo
            if (lower == 0) then
              peak = array(ix, iy)
              ixPeak = ix
              iyPeak = iy
            endif
          endif
        endif
      endif
    enddo
  enddo
  ! print *,ixpeak, iypeak
  !
  if (ixPeak > 0) then
    !
    ! simply fit a parabola to the two adjacent points in X or Y
    !
    y1 = array(indmap(ixPeak - 1, nxCorr), iyPeak)
    y2 = peak
    y3 = array(indmap(ixPeak + 1, nxCorr), iyPeak)
    cx = parabolicFitPosition(y1, y2, y3)
    ! print *,'X', y1, y2, y3, cx
    y1 = array(ixPeak, indmap(iyPeak - 1, nyCorr))
    y3 = array(ixPeak, indmap(iyPeak + 1, nyCorr))
    cy = parabolicFitPosition(y1, y2, y3)
    ! print *,'Y', y1, y2, y3, cy
    !
    ! return adjusted pixel coordinate minus 1
    !
    xpeak = ixPeak + cx - 1.
    ypeak = iyPeak + cy - 1.
  endif
  ! print *,xpeak, ypeak
  if (xpeak > nxCorr / 2) xpeak = xpeak - nxCorr
  if (ypeak > nyCorr / 2) ypeak = ypeak - nyCorr
  return
end subroutine peakFind



subroutine packCorr(crray, array, nxPadPlus, nyPad)
  implicit none
  integer*4 nxPadPlus, nyPad
  real*4 array(nxPadPlus,nyPad), crray(*)
  integer*4 nxPad, iout, ixIn, iyIn, ix, iy

  nxPad = nxPadPlus - 2
  iout = 1
  ixIn = nxPad / 2 + 1
  iyIn = nyPad / 2 + 1
  do iy = 1, nyPad
    do ix = 1, nxPad
      crray(iout) = array(ixIn, iyIn)
      iout = iout + 1
      ixIn = ixIn + 1
      if (ixIn > nxPad) ixIn = 1
    enddo
    iyIn = iyIn + 1
    if (iyIn > nyPad) iyIn = 1
  enddo
  return
end subroutine packCorr

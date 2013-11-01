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
  integer LIMBOUND, LIMPEAKS
  parameter (LIMBOUND = 1000, LIMPEAKS = 50)
  include 'smallmodel.inc90'
  integer*4 nx, ny, nz
  !
  integer*4 nxyz(3), mxyz(3), nxyzs(3)
  real*4 title(20), delta(3), origin(3)
  real*4 ctfp(8193), ctfUB(8193)
  real*4, allocatable :: sumArray(:), crray(:), array(:), brray(:)
  real*4, allocatable :: tmpArray(:), ubArray(:), ubBrray(:)
  logical*4 inStreak(LIMPEAKS)
  !
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  !
  character*320 inFile, plFile, imFileOut, xfFileOut
  character*1000 listString
  real*4 fs(2,3), fsInv(2,3), fUnit(2,3), prexfInv(2,3)
  character*9 dat
  character*8 tim
  character*80 titlech
  character*70 titleStr
  character*10 filterText/' '/
  character*15 binRedText/' Binning is'/

  real*4, allocatable :: f(:,:,:), tilt(:), dxPreali(:), dyPreali(:), fPreali(:,:,:)
  integer*4, allocatable :: ixPcList(:), iyPcList(:), izPcList(:)
  integer*4, allocatable :: listz(:), listSkip(:), listMagViews(:)
  real*4, allocatable :: patchCenX(:), patchCenY(:), patchCenXall(:), patchCenYall(:)
  real*4, allocatable :: xControl(:), yControl(:), xVector(:), yVector(:)
  real*4, allocatable :: xmodel(:,:), ymodel(:,:), xbound(:), ybound(:)
  real*4, allocatable :: xtfsBound(:), ytfsBound(:)
  integer*4, allocatable :: iobjFlags(:), ifBoundOnView(:)
  integer*4 iobjBound(LIMBOUND), indBound(LIMBOUND)
  integer*4 numInBound(LIMBOUND)
  real*4 xtfsBmin(LIMBOUND), xtfsBmax(LIMBOUND), ytfsBmin(LIMBOUND)
  real*4 ytfsBmax(LIMBOUND)
  real*4 dmin2, dmax2, dmean2, dmean3, rotAngle, deltaCTF, cosStrMaxTilt
  integer*4 i, numPcList, numViews, minXpiece, numXpieces, nxOverlap, minYpiece, numBest
  integer*4 numYpieces, nyOverlap, ifImOut, nxPad, nyPad, ifExclude, mode, indBest
  integer*4 nxTrim, nyTrim, nxUse, nyUse, nxBorder, nyBorder, nxTaper, nyTaper
  integer*4 izStart, izEnd, kk, nzOut, izLast, izCur, idir, izTmp, imagesBinned
  real*4 dmeanSum, dmax, dmin, stretch, xpeak, ypeak, unstretchDx, unstretchDy
  real*4 dmean, radius1, radius2, sigma1, sigma2, tiltAtMin, cosView
  integer*4 iv, iview, kti, isOut, ierr, ivStart, ivEnd, loopDir, ifReadXfs
  integer*4 iloop, numLoops, minTilt, ifAbsStretch, ivRef, ifLeaveAxis, ivRefBase
  integer*4 nbinning, maxBinSize, nxUseBin, nyUseBin, ifCumulate, ifNoStretch, maxTrackGap
  integer*4 ixStart, ixEnd, iyStart, iyEnd, ivCur, maxBinning, iz, ivSkip
  integer*4 ixBoxCur, iyBoxCur, ixBoxRef, iyBoxRef, lenContour, minContOverlap
  integer*4 ixCenStart, ixCenEnd, iyCenStart, iyCenEnd, iter, numIter, ivBound
  integer*4 lapTotal, lapRemainder, j, ivBase, lapBase, numCont, nxPatch, nyPatch
  real*4 xBoxOfs, yBoxOffset, cosPhi, sinPhi, x0, y0, xshift, yshift
  real*4 useMin, useMax, useMean, cumXshift, cumYshift, cumXrot, xAdjust
  real*4 angleOffset, cumXcenter, cumYcenter, xModOffset, yModOffset
  real*4 xpeakCum, ypeakCum, xpeakTmp, ypeakTmp, xpeakFrac, ypeakFrac, yOverlap
  real*4 peakFracTol, xFromCen, yFromCen, cenx, ceny, baseTilt, xOverlap, yval
  integer*4 numPatches, numXpatch, numYpatch, ind, ixBoxStart, iyBoxStart, numPatchesAll
  integer*4 ixBoxForAdj, iyBoxForAdj, ipatch, numPoints, numBound, iobj, lenTemp
  integer*4 ipnt, ipt, numInside, iobjSeed, imodObj, imodCont, ix, iy, iAntiFiltType
  integer*4 limitShiftX, limitShiftY, numSkip, lastNotSkipped, lenConts, nFillTaper
  integer*4 numControl, numBoundAll, idim2
  integer*4 nxUnali, nyUnali, numAllViews, ivPairOffset, ifFindWarp, limitedBinSize
  integer*4 nxUBpad, nyUBpad, nxUBtaper, magView
  real*4 critInside, cosRatio, peakVal, peakLast, xpeakLast, yPeakLast
  real*4 boundXmin, boundXmax, boundYmin, boundYmax, fracXover, fracYover
  real*4 fracOverMax, critNonBlank, fillTaperFrac, deltaUBctf, radExclude
  integer*4 nyUBtaper, limitUBshiftX, limitUBshiftY, niceLimit, ifEllipse, maxXcorrPeaks
  real*4 binWidthRatioCrit, peak2ToPeak3Crit, centralPeakMaxWidth, ubWidthRatioCrit
  integer*4 ifUBpeakIsSharp, numCuts, numMagViews
  real*4 cosRotAngle, sinRotAngle, overlapCrit, overlapPower, foundMag, stepMag, searchMin
  real*4 searchMax, brackets(14)
  real*8 wallMask, wallStart, wallInterp, wallfft, wallPeak

  logical*4 tracking, verbose, breaking, taperCur, taperRef, reverseOrder, evalCCC
  logical*4 refViewOut, rawAlignedPair, addToWarps, curViewOut, limitingShift
  logical*4 searchedForMag, searchMag
  integer*4 niceFrame, newImod, putImodMaxes, putModelName, numberInList, taperAtFill
  integer*4 newWarpFile, setWarpPoints, writeWarpFile, setLinearTransform, readWarpFile
  integer*4 separateLinearTransform, niceFFTlimit, minimize1D
  logical inside
  real*4 cosd, sind

  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean, PipGetLogical, PipNumberOfEntries
  integer*4 PipGetString, PipGetFloat, PipGetTwoIntegers, PipGetTwoFloats
  integer*4 PipGetInOutFile, ifPip, PipGetThreeFloats
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  tiltxcorr
  !
  integer numOptions
  parameter (numOptions = 55)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@piece:PieceListFile:FN:@output:OutputFile:FN:@'// &
      'rotation:RotationAngle:F:@first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'// &
      'tiltfile:TiltFile:FN:@angles:TiltAngles:FAM:@offset:AngleOffset:F:@'// &
      'reverse:ReverseOrder:B:@radius1:FilterRadius1:F:@radius2:FilterRadius2:F:@'// &
      'sigma1:FilterSigma1:F:@sigma2:FilterSigma2:F:@exclude:ExcludeCentralPeak:B:@'// &
      'central:CentralPeakExclusionCriteria:FT:@shift:ShiftLimitsXandY:IP:@'// &
      'rect:RectangularLimits:B:@ccc:CorrelationCoefficient:B:@'// &
      'border:BordersInXandY:IP:@xminmax:XMinAndMax:IP:@yminmax:YMinAndMax:IP:@'// &
      'boundary:BoundaryModel:FN:@objbound:BoundaryObject:I:@'// &
      'binning:BinningToApply:I:@antialias:AntialiasFilter:I:@'// &
      'leaveaxis:LeaveTiltAxisShifted:B:@pad:PadsInXandY:IP:@taper:TapersInXandY:IP:@'// &
      'views:StartingEndingViews:IP:@skip:SkipViews:LI:@break:BreakAtViews:LI:@'// &
      'cumulative:CumulativeCorrelation:B:@absstretch:AbsoluteCosineStretch:B:@'// &
      'nostretch:NoCosineStretch:B:@iterate:IterateCorrelations:I:@'// &
      'search:SearchMagChanges:B:@changes:ViewsWithMagChanges:LI:@'// &
      'mag:MagnificationLimits:FP:@size:SizeOfPatchesXandY:IP:@'// &
      'number:NumberOfPatchesXandY:IP:@overlap:OverlapOfPatchesXandY:IP:@'// &
      'seed:SeedModel:FN:@objseed:SeedObject:I:@length:LengthAndOverlap:IP:@'// &
      'prexf:PrealignmentTransformFile:FN:@imagebinned:ImagesAreBinned:I:@'// &
      'unali:UnalignedSizeXandY:IP:@warp:FindWarpTransforms:I:@'// &
      'pair:RawAndAlignedPair:IP:@append:AppendToWarpFile:B:@test:TestOutput:FN:@'// &
      'verbose:VerboseOutput:B:@param:ParameterFile:PF:@help:usage:B:'
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
  limitingShift = .false.
  fracXover = 0.33
  fracYover = 0.33
  fracOverMax = 0.8
  wallMask = 0.
  wallInterp = 0.
  wallfft = 0.
  wallPeak = 0.
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
  ifFindWarp = 0
  rawAlignedPair = .false.
  addToWarps = .false.
  ivPairOffset = 0
  evalCCC = .false.
  maxXcorrPeaks = 10
  binWidthRatioCrit = 1.05
  peak2ToPeak3Crit = 3.
  centralPeakMaxWidth = 3.0
  ubWidthRatioCrit = 1.6
  radExclude = 0.3
  overlapCrit = 0.125
  overlapPower = 6
  ifEllipse = 1
  searchMag = .false.
  searchMin = 0.9
  searchMax = 1.1
  limitedBinSize = 4300**2
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
      listSkip(nz), listMagViews(nz), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR VIEWS')
  listMagViews = (/(i, i = 1, nz)/)
  numMagViews = nz
  !
  if (pipinput) then
    ifPip = 1
    plFile = ' '
    ierr = PipGetString('PieceListFile', plFile)
    ierr = PipGetLogical('VerboseOutput', verbose)
    ierr = PipGetInteger('FindWarpTransforms', ifFindWarp)
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
  numAllViews = numViews
  if (ifFindWarp .ne. 0) then
    !
    ! Make sure warping has no tilt angles
    ierr = PipNumberOfEntries('TiltAngles', nz)
    if (nz > 0 .or. PipGetFloat('FirstTiltAngle', xpeak) == 0 .or.  &
        PipGetFloat('TiltIncrement', xpeak) == 0 .or.  &
        PipGetString('TiltFile', listString) == 0) call exitError( &
        'YOU CANNOT ENTER TILT ANGLES WHEN FINDING WARP TRANSFORMS')
    nz = numViews
    tilt(1:nz) = 0.
    !
    ! See if doing an aligned pair.  Make gray area criterion less because only the
    ! current image will lose area
    rawAlignedPair = PipGetTwoIntegers('RawAndAlignedPair', iv, numAllViews) == 0
    if (rawAlignedPair) then
      if (numAllViews < 2 .or. iv < 2 .or. iv > numAllViews) call exitError( &
          'IN RawAndAlignedPair ENTRY, TOTAL'// &
          ' VIEWS IS LESS THAN 2 OR VIEW NUMBER IS OUT OF RANGE')
      ivPairOffset = iv - 2
      ierr = PipGetLogical('AppendToWarpFile', addToWarps)
      critNonBlank = 0.5
    endif
  else
    call get_tilt_angles(numViews, 3, tilt, nz, ifPip)
    if (numViews .ne. nz) then
      write(*,'(/,a,i5,a,i5,a)') 'ERROR: TILTXCORR - There must be a tilt angle for' &
          //' each image: nz =', nz, ', but there are', numViews, ' tilt angles'
      call exit(1)
    endif
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
    limitingShift = PipGetTwoIntegers('ShiftLimitsXandY', limitShiftX, limitShiftY) == 0
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

    ierr = PipGetLogical('CorrelationCoefficient', evalCCC)
    ierr = PipGetLogical('ReverseOrder', reverseOrder)
    ierr = PipGetBoolean('CumulativeCorrelation', ifCumulate)
    ierr = PipGetBoolean('NoCosineStretch', ifNoStretch)
    ierr = PipGetBoolean('AbsoluteCosineStretch', ifAbsStretch)
    ierr = PipGetBoolean('LeaveTiltAxisShifted', ifLeaveAxis)
    ierr = PipGetThreeFloats('CentralPeakExclusionCriteria', peak2ToPeak3Crit, &
        centralPeakMaxWidth, ubWidthRatioCrit)
    ierr = PipGetBoolean('RectangularLimits', iv)
    ifEllipse = 1 - iv
    ierr = PipGetLogical('SearchMagChanges', searchMag) 
    if (searchMag .and. PipGetString('ViewsWithMagChanges', listString) == 0) &
        call parselist2(listString, listMagViews, numMagViews, nz)
    ierr = PipGetTwoFloats('MagnificationLimits', searchMin, searchMax)
    if (searchMin >= searchMax - 0.01) call exitError( &
        'LIMITS FOR MAGNIFICATION SEARCH ARE OUT OF ORDER OR TOO CLOSE TO EACH OTHER') 
    if (searchMag .and. (ifCumulate > 0 .or. ifLeaveAxis > 0)) call exitError( &
        'MAG CHANGE CANNOT BE SEARCHED WITH -cumulative OR -leave OPTIONS')
    ierr = PipGetFloat('AngleOffset', angleOffset)
    do iv = 1, numViews
      tilt(iv) = tilt(iv) + angleOffset
    enddo

    ! Get skip or breaking list
    ierr = PipGetString('SkipViews', listString)
    iz = PipGetString('BreakAtViews', listString)
    if (iz + ierr == 0) call exitError('YOU CANNOT BOTH SKIP VIEWS AND BREAK AT VIEWS')
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
          if (iv >= 1 .and. iv <= numAllViews) then
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
      allocate(xbound(numPoints), ybound(numPoints), ifBoundOnView(numAllViews),  &
          stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR BOUNDARY CONTOURS')
      boundXmin = 1.e10
      boundXmax = -1.e10
      boundYmin = 1.e10
      boundYmax = -1.e10
      ifBoundOnView = 0           ! Vector
      do ix = 1, numBound
        iobj = iobjBound(ix)
        ipnt = abs(object(1 + ibase_obj(iobj)))
        iv = nint(p_coord(3, ipnt)) + 1
        ifBoundOnView(iv) = 1
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
    numBoundAll = numBound
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
      if (searchMag) call exitError( &
          'YOU CANNOT SEARCH FOR A MAG CHANGE WITH PATCH TRACKING')
      if (breaking .and. ifFindWarp == 0)  &
          call exitError('YOU CANNOT BREAK AT VIEWS WITH PATCH TRACKING')
      tracking = .true.
      if (ifFindWarp .ne. 0 .and. reverseOrder) call exitError( &
          'YOU CANNOT FIND WARP TRANSFORMS IN REVERSE ORDER')
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
        numPatchesAll = numXpatch * numYpatch
        ix = numPatchesAll + 10
        allocate(patchCenX(ix), patchCenY(ix), patchCenXall(ix), patchCenYall(ix), &
            stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR PATCH CENTERS')
        xOverlap = (numXpatch * nxPatch - nxUse) / max(1., numXpatch - 1.)
        yOverlap = (numYpatch * nyPatch - nyUse) / max(1., numYpatch - 1.)
        do j = 1, numYpatch
          yval = iyStart + (j - 1) * (nyPatch - yOverlap) + 0.5 * nyPatch
          if (numYpatch == 1) yval = (iyEnd + 1 + iyStart) / 2.
          do i = 1, numXpatch
            ind = i + (j - 1) * numXpatch
            patchCenXall(ind) = ixStart + (i - 1) * (nxPatch - xOverlap) + &
                0.5 * nxPatch
            if (numXpatch == 1) patchCenXall(ind) = (ixEnd + 1 + ixStart) / 2.
            patchCenYall(ind) = yval
          enddo
        enddo
      else
        !
        ! Or get a seed model to specify patches
        ierr = PipGetInteger('SeedObject', iobjSeed)
        call getModelAndFlags('OPENING SEED MODEL FILE')
        
        ! Loop twice, first time to get count and allocate, second time to save
        do iloop = 1, 2
          numPatchesAll = 0
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

                    numPatchesAll = numPatchesAll + 1
                    if (iloop > 1) then
                      patchCenXall(numPatchesAll) = x0
                      patchCenYall(numPatchesAll) = y0
                    endif
                  endif
                endif
              enddo
            endif
          enddo
          if (numPatchesAll == 0) call exitError('NO QUALIFYING POINTS FOUND' &
              //' IN SEED MODEL; SPECIFY OBJECT OR MAKE IT SCATTERED POINTS')
          if (iloop == 1) then
            ix = numPatchesAll + 10
            allocate(patchCenXall(ix), patchCenYall(ix), patchCenX(ix), patchCenY(ix), &
                stat = ierr)
            call memoryError(ierr, 'ARRAYS FOR PATCH CENTERS')
          endif
        enddo

        deallocate(iobjFlags)
      endif
      if (ifFindWarp .ne. 0) then
        allocate(xControl(ix), yControl(ix), xVector(ix), yVector(ix), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR WARP POINTS')
      endif
      !
      ! Now eliminate patches outside boundary model or just copy centers
      ! This is redone on every view for warping if there is more than one contour
      call makePatchListInsideBoundary()
      if (ifFindWarp .ne. 0 .and. numPatches < 3) call exitError( 'THERE ARE TOO FEW'// &
          ' PATCHES INSIDE THE BOUNDARY CONTOUR(S) TO DEFINE CONTROL POINTS')
      if (numPatches == 0) call exitError( &
          'NO PATCHES ARE SUFFICIENTLY INSIDE THE BOUNDARY CONTOUR(S)')
      !
      ! Get prealign transforms, then eliminate patches that have too much blank area
      ! on starting view
      ifReadXfs = 1 - PipGetString('PrealignmentTransformFile', plFile)
      if (rawAlignedPair .and. ifReadXfs == 0) call exitError( &
          'PREALIGNMENT F TRANSFORMS MUST BE ENTERED TO DO RAW-ALIGNED PAIR')
      if (ifReadXfs .ne. 0) then
        ierr = PipGetInteger('ImagesAreBinned', imagesBinned)
        if (rawAlignedPair .and. imagesBinned > 1) call exitError( &
            'YOU CANNOT DO A RAW AND ALIGNED PAIR WITH BINNED INPUT')
        allocate(fPreali(2, 3, numAllViews), dxPreali(numAllViews),  &
            dyPreali(numAllViews), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR PREALIGN TRANSFORMS')
        call checkForWarpFile(plFile)
        call dopen(3, plFile, 'ro', 'f')
        call xfrdall2(3, fPreali, iv, numAllViews, ierr)
        if (ierr == 2) call exitError('READING TRANSFORM FILE')
        if (iv .ne. numAllViews) call exitError( &
            'NOT ENOUGH TRANSFORMS IN PREALIGN TRANSFORM FILE')
        dxPreali(1:numAllViews) = fPreali(1, 3, 1:numAllViews) / imagesBinned
        dyPreali(1:numAllViews) = fPreali(2, 3, 1:numAllViews) / imagesBinned
        !
        ! Get unaligned size if it differs, adjust for binning
        if (PipGetTwoIntegers('UnalignedSizeXandY', nxUnali, nyUnali) == 0) then
          nxUnali = nxUnali / imagesBinned
          nyUnali = nyUnali / imagesBinned
          if (rawAlignedPair) call exitError( &
              'INPUT FILE WITH RAW AND ALIGNED PAIR MUST BE SAME SIZE AS UNALIGNED STACK')
        else
          nxUnali = nx
          nyUnali = ny
        endif
        if (ifFindWarp == 0) then
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
      endif

      nxUse = nxPatch
      nyUse = nyPatch
      
      ! Start the warping file
      if (ifFindWarp .ne. 0) then
        call irtdel(1, delta)
        if (addToWarps) then
          ierr = readWarpFile(xfFileOut, ix, iy, iz, ind, xpeakTmp, ipatch, iv)
          if (ierr < 0) call exitError('READING OUTPUT FILE AS EXISTING WARP FILE')
          if (ix .ne. nx .or. iy .ne. ny .or.  &
              abs(xpeakTmp - delta(1)) > 1.e-4 * delta(1)) call exitError( &
              'EXISTING WARP FILE DOES NOT MATCH IN X OR Y IMAGE SIZE OR PIXEL SIZE')
          if (ind .ne. 1 .or. iv .ne. 3) call exitError( &
              'BINNING ENTRY OR FLAGS IN EXISTING WARP FILE ARE INVALID')
        else
          ierr = newWarpFile(nx, ny, 1, delta(1), 3)
          if (ierr < 0) call exitError(' &
              &FAILURE TO ALLOCATE NEW WARP STRUCTURE IN LIBRARY')
        endif
      endif

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
    if (ifFindWarp .ne. 0)  &
        call exitError('YOU MUST SPECIFY A PATCH SIZE TO FIND WARP TRANSFORMS')
    numPatches = 1
    allocate(patchCenX(10), patchCenY(10), stat=ierr)
    call memoryError(ierr, 'TINY ARRAY FOR PATCH CENTERS')
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
  niceLimit = niceFFTlimit()
  if (nbinning == 0) then
    nbinning = (max(nxUse + 2 * nxBorder, nyUse + 2 * nyBorder) + maxBinSize-1) /  &
        maxBinSize
    !
    ! If the binning is bigger than 4, find the minimum binning needed to keep the used
    ! image within advisable bounds, up to a maximum binning, then stick with that
    if (nbinning > 4) then
      nbinning = 0
      i = 4
      do while (i <= maxBinning .and. nbinning == 0)
        if ((niceFrame((nxUse + 2 * nxBorder) / i, 2, niceLimit) + 2) * &
            niceFrame((nyUse + 2 * nyBorder) / i, 2, niceLimit) < limitedBinSize)  &
            nbinning = i
        i = i + 1
      enddo
      if (nbinning == 0) nbinning = maxBinning
    endif
  endif

  nxUseBin = nxUse / nbinning
  nyUseBin = nyUse / nbinning

  nxPad = niceFrame((nxUse + 2 * nxBorder) / nbinning, 2, niceLimit)
  nyPad = niceFrame((nyUse + 2 * nyBorder) / nbinning, 2, niceLimit)
  idim2 = (nxPad + 2) * nyPad + 16
  allocate(sumarray(idim2), crray(idim2), array(idim2), brray(idim2), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR IMAGES')

  write(*,'(/,a,i3,a,i5,a,i5)') binRedText, nbinning, &
      ';  padded, reduced size is', nxPad, ' by', nyPad
  !
  ! Now that padded size exists, get the filter ctf.  If doing CCC's, get the array for
  ! that and take square root of filter to apply it to both images
  !
  call setCtfwSR(sigma1, sigma2, radius1, radius2, ctfp, nxPad, nyPad, deltaCTF)
  if (evalCCC) ctfp(:) = sqrt(ctfp(:))
  !
  ! Set up tapering, save unbinned values for taper and shift limit
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
  nxUBtaper = nxTaper
  nyUBtaper = nyTaper
  limitUBshiftX = limitShiftX
  limitUBshiftY = limitShiftY
  nxTaper = nxTaper / nbinning
  nyTaper = nyTaper / nbinning
  limitShiftX = (limitShiftX  + nbinning / 2) / nbinning
  limitShiftY = (limitShiftY  + nbinning / 2) / nbinning
  if (limitShiftX <= 0 .or. limitShiftY <= 0) call exitError( &
      'SHIFT LIMITS MUST BE POSITIVE AND AT LEAST 1 PIXEL WHEN DIVIDED BY BINNING')
  !
  ! If excluding central peak, set up parameters for that
  if (ifExclude > 0) then
    nxUBpad = niceFrame(nxUse + 2 * nxBorder, 2, niceLimit)
    nyUBpad = niceFrame(nyUse + 2 * nyBorder, 2, niceLimit)
    call setCtfwSR(sigma1 / nbinning, sigma2 / nbinning, radius1 / nbinning, 0.75, &
        ctfUB, nxUBpad, nyUBpad, deltaUBctf)
    if (nbinning > 1) then
      kk = nyUBpad * (nxUBpad + 2) + 16
      allocate(ubArray(kk), ubBrray(kk), stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR UNBINNED CORRELATIONS')
    endif
    cosRotAngle = cosd(-rotAngle)
    sinRotAngle = sind(-rotAngle)
  endif
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
        f(1:2, 3, ivCur) = f(1:2, 3, ivRef)
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
      ! If doing warp and there is more than one boundary contour or the contour needs
      ! to be transformed, get contour(s) from nearest Z to this and get new set of
      ! patch centers
      if (ifFindWarp .ne. 0 .and. (numBoundAll > 1 .or.  &
          (numBoundAll > 0 .and. rawAlignedPair))) then
        !
        ! Find nearest view with boundaries
        ind = numAllViews + 10
        do iv = 1, numAllViews
          if (ifBoundOnView(iv) > 0 .and. abs(ivCur + ivPairOffset - iv) < ind) then
            ind = abs(ivCur + ivPairOffset - iv)
            ivBound = iv
          endif
        enddo
        !
        ! Redo the indices and repack the points into boundary arrays, transforming them
        ! if needed
        numBound = 0
        numPoints = 0
        do ix = 1, numBoundAll
          iobj = iobjBound(ix)
          ipnt = abs(object(1 + ibase_obj(iobj)))
          if (nint(p_coord(3, ipnt)) + 1 .eq. ivBound) then
            numBound = numBound + 1
            indBound(numBound) = numPoints
            numInBound(numBound) = npt_in_obj(iobj)
            do ipt = 1, npt_in_obj(iobj)
              ipnt = abs(object(ipt + ibase_obj(iobj)))
              numPoints = numPoints + 1
              xbound(numPoints) = p_coord(1, ipnt)
              ybound(numPoints) = p_coord(2, ipnt)
              if (rawAlignedPair) call xfapply(fPreali(1, 1, ivCur + ivPairOffset),  &
                  nx / 2., ny / 2., p_coord(1, ipnt), p_coord(2, ipnt), &
                  xbound(numPoints), ybound(numPoints))
            enddo
          endif
        enddo
        !
        ! Get the patch centers based on the boundaries
        call makePatchListInsideBoundary()
      endif
      !
      ! Get inverse of prexf transform when doing pairs, for evaluating taper
      if (rawAlignedPair) call xfinvert(fPreali(1, 1, ivCur + ivPairOffset), prexfInv)
      !
      ! Loop on the patches
      ivRefBase  = ivRef
      numControl = 0
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
          if ((iloop == 1 .and. iview == ivStart) .or. ifFindWarp .ne. 0) then
            xmodel(ipatch, ivRef) = ixBoxRef + ixCenStart + nxUse / 2.
            ymodel(ipatch, ivRef) = iyBoxRef + iyCenStart + nyUse / 2.
            if (ifFindWarp .ne. 0) then
              xmodel(ipatch, ivCur) = xmodel(ipatch, ivRef)
              ymodel(ipatch, ivCur) = ymodel(ipatch, ivRef)
            endif
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
          if (rawAlignedPair) then
            taperRef = .false.
            refViewOut = .false.
            call evaluatePairPatch(nx, ny, nxPatch, nyPatch, ixCenStart + ixBoxCur, &
                iyCenStart + iyBoxCur, prexfInv, critNonBlank, taperCur, curViewOut)
          else
            call usablePatchExtent(nx, nxUnali, nxPatch, dxPreali(ivRef),  &
                ixCenStart + ixBoxRef, ix)
            call usablePatchExtent(ny, nyUnali, nyPatch, dyPreali(ivRef),  &
                iyCenStart + iyBoxRef, iy)
            taperRef = ix < nxPatch .or. iy < nyPatch
            refViewOut = ix < 0 .or. iy < 0 .or.  &
                ix * iy < critNonBlank * nxPatch *  nyPatch
            call usablePatchExtent(nx, nxUnali, nxPatch, dxPreali(ivCur),  &
                ixCenStart + ixBoxCur, ix)
            call usablePatchExtent(ny, nyUnali, nyPatch, dyPreali(ivCur),  &
                iyCenStart + iyBoxCur, iy)
            taperCur = ix < nxPatch .or. iy < nyPatch
            curViewOut = ix < 0 .or. iy < 0 .or. &
                ix * iy < critNonBlank * nxPatch * nyPatch
          endif
          if (curViewOut) then
            if (verbose) print *,'Skipping tracking of patch', ipatch
            if (ifFindWarp .ne. 0) xmodel(ipatch, ivRef) = -1.e10
            cycle
          endif

          ! If finding warp, skip if no reference view
          if (ifFindWarp .ne. 0 .and. refViewOut) then
            if (verbose) print *,'Skipping tracking of patch; no reference', ipatch
            xmodel(ipatch, ivRef) = -1.e10
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
        ifUBpeakIsSharp = -1
        ! 
        ! Is it time to search for the mag change?
        magView = max(ivCur, ivRef)
        searchedForMag = searchMag .and.  &
            numberInList(magView, listMagViews, numMagViews, 0)
        if (searchedForMag) then

          ! Swap the search limits if the reference is the mag view; we are finding the
          ! inverse of its mag change
          if (ivRef == magView) then
            peakval = 1. / searchMin
            searchMin = 1. / searchMax
            searchMax = peakval
          endif

          ! Set up the step size and initial mag
          stepMag = min(0.03, (searchMax - searchMin) / 3.)
          foundMag = searchMin - stepMag
          numCuts = -1
          if (verbose) print *,'Searching for best magnification change'
          do while (.true.)

            ! Get the transformation incorporating the mag change, and correlate with CCCs
            call rotmagstr_to_amat(0., foundMag, stretch, rotAngle, fs)
            if (nbinning > 1) then
              call correlateAndFindPeaks(.true., ubArray, ubBrray, 0)
            else
              call correlateAndFindPeaks(.true., array, brray, 0)
            endif
            if (verbose) print *,'Mag = ',foundMag, '  CCC = ', peakval

            ! Find out the next step; check if out of range or if step size is small
            ierr = minimize1D(foundMag, -peakVal, stepMag,  &
                nint((searchMax - searchMin) / stepMag) + 2, numCuts, brackets, foundMag)
            if (ierr > 0) then
              write(*,'(a,i4,a,/,a)')'Search for magnification change at view', magView, &
                  ' reached allowed limits',  &
                  '  without finding a minimum - no mag change will be introduced'
              foundMag = 1.
              searchedForMag = .false.
              exit
            endif
            if (stepMag / 2**numCuts < 0.00025) then

              ! Done: get the minimum
              foundMag = brackets(2)
              exit
            endif
          enddo

          ! Revise the transform again and go on
          call rotmagstr_to_amat(0., foundMag, stretch, rotAngle, fs)
        endif

        ! Now start iterations of regular correlation alignment
        do iter = 1, numIter
          if (nbinning > 1) then
            call correlateAndFindPeaks(evalCCC, ubArray, ubBrray, ifImOut)
          else
            call correlateAndFindPeaks(evalCCC, array, brray, ifImOut)
          endif
          xpeakCum = xpeakTmp + xpeakFrac
          xpeakFrac = xpeakCum - nint(xpeakCum)
          ypeakCum = ypeakTmp + ypeakFrac
          ypeakFrac = ypeakCum - nint(ypeakCum)
          if (verbose) write(*,'(i3,2f8.2,g15.6,4f8.2)') iter, xpeakCum, ypeakCum, &
              peakVal, xpeakTmp - nint(xpeakTmp), ypeakTmp - nint(ypeakTmp), &
              xpeakFrac, ypeakFrac

          ! If accumulating and the correlation failed, assign it the last shift
          if (peakVal < -1.e29 .and. ifCumulate .ne. 0) then
            xpeakCum = xpeak
            ypeakCum = ypeak
          endif
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
        !
        ! After this possible destretch, unconditionally adjust for a mag change
        ! and invert the mag change and scale shift if it applies to reference
        if (searchedForMag) then
          unstretchDx = unstretchDx - ((foundMag - 1.) * ixBoxRef) / nbinning
          unstretchDy = unstretchDy - ((foundMag - 1.) * iyBoxRef) / nbinning
          if (magView == ivRef) then
            foundMag = 1. / foundMag
            unstretchDx = unstretchDx * foundMag
            unstretchDy = unstretchDy * foundMag
          endif
          f(1, 1, magView) = foundMag
          f(2, 2, magView) = foundMag
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
          if (ifFindWarp .ne. 0) then
            !
            ! Only add control point if there was a peak found within limits
            if (peakVal > -1.e29) then
              numControl = numControl + 1
              xControl(numControl) = xmodel(ipatch, ivCur)
              yControl(numControl) = ymodel(ipatch, ivCur)
              xVector(numControl) = -nbinning * xpeak
              yVector(numControl) = -nbinning * ypeak
            endif
          else
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
      if (ifFindWarp .ne. 0) then
        if (verbose) print *,'View',ivCur,numControl,' warp points'
        ierr = 0
        iv = ivCur + ivPairOffset
        if (numControl > 2) then
          ierr = setWarpPoints(iv, numControl, xControl, yControl, xVector, yVector)
          if (ierr .ne. 0) call exitError('SETTING WARP POINTS FOR THE CURRENT VIEW')
          if (rawAlignedPair)  &
              ierr = setLinearTransform(iv, fPreali(1, 1, iv), 2)
          if (ierr .ne. 0) call exitError('SETTING LINEAR TRANSFORM IN WARP FILE')
          if (ifFindWarp > 0) ierr = separateLinearTransform(iv)
          if (ierr .ne. 0) call exitError( &
              'SEPARATING LINEAR COMPONENT FROM WARP TRANSFORM')
        else if (numControl > 0) then
          call xfunit(fs)
          if (rawAlignedPair) call xfcopy(fPreali(1, 1, iv), fs)
          do i = 1, numControl
            fs(1, 3) = fs(1, 3) - xVector(i) / numControl
            fs(2, 3) = fs(2, 3) - yVector(i) / numControl
          enddo
          ierr = setLinearTransform(iv, fs, 2)
          if (ierr .ne. 0) call exitError('SETTING LINEAR TRANSFORM IN WARP FILE')
        endif
      endif
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
        f(1:2, 3, iv) = f(1:2, 3, lastNotSkipped)
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
  else if (ifFindWarp .ne. 0) then
    !
    ! Warp output
    ierr = writeWarpFile(xfFileOut, 0)
    if (ierr < 0) call exitError('WRITING WARP FILE')
    if (ierr > 0) write(*, '(/,a,/)') &
        'WARNING: TILTXCORR - FAILED TO MAKE BACKUP OF EXISTING OUTPUT WARPING FILE'
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
  ! write(*,'(4(a,f9.3))') 'interpolation', wallinterp, '  fft', wallfft, &
  !    '  peak', wallPeak, '  masking', wallmask
  write(6, 500)
500 format(' PROGRAM EXECUTED TO END.')
  call exit(0)
96 call exitError('ERROR WRITING TRANSFORMS TO FILE')

CONTAINS

  subroutine correlateAndFindPeaks(useCCC, ubArray, ubBrray, ifDoImOut)
    logical*4 useCCC
    real*4 ubArray(*), ubBrray(*)
    real*4 xpeakList(LIMPEAKS), ypeakList(LIMPEAKS), peakList(LIMPEAKS), widths(LIMPEAKS)
    real*4 widthMins(LIMPEAKS), xtemp, ytemp
    integer*4 indPeakSort(LIMPEAKS), ifDoImOut
    real*4 ubXpeaks(2), ubYpeaks(2), ubPeakList(2), overlap, streak, wgtCCC, xrot, yrot
    integer*4 limitXlo, limitXhi, limitYlo, limitYhi, indPeak, nsum, nxCCTrimA, nyCCTrimA
    integer*4 numXcorrPeaks, indFirstOut, indSecondOut, nxInterp, nyInterp
    integer*4 nxCCTrimB, nyCCTrimB

    real *8 cccMax, ccc
    real*8 wallTime, CCCoefficientTwoPads
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
    nxInterp = nxUseBin
    nyInterp = nyUseBin
    if (searchedForMag) then
      !
      ! Get new limits for interpolated output when there is a mag down, so that
      ! tapering happens inside true interpolated image area and correlations take account
      ! of this area too.  Use the OUTER of two limits to accommodate stretched area
      call xfApply(fs, nxUseBin / 2., nyUseBin / 2., float(nxUseBin), 0., xrot, yrot)
      call xfApply(fs, nxUseBin / 2., nyUseBin / 2., float(nxUseBin), float(nyUseBin),  &
          xtemp, ytemp)
      nxInterp = min(nxUseBin, 2 * int(max(xrot, xtemp) - nxUseBin / 2.))
      nyInterp = min(nyUseBin, 2 * int(max(nyUseBin / 2. - yrot, ytemp - nyUseBin / 2.)))
    endif
    call cubInterp(array, brray, nxUseBin, nyUseBin, nxUseBin, nyUseBin, fs, &
        nxUseBin / 2., nyUseBin / 2., xpeakFrac, ypeakFrac , 1., useMean, 0)
    wallInterp = wallInterp + wallTime() - wallStart
    call taperInPadEx(brray, nxUseBin, (nxUseBin - nxInterp) / 2,  &
        (nxUseBin + nxInterp) / 2 - 1, (nyUseBin - nyInterp) / 2,  &
        (nyUseBin + nyInterp) / 2 - 1, brray, nxPad + 2, nxPad, nyPad, nxTaper, nyTaper)
    !
    call meanZero(brray, nxPad + 2, nxPad, nyPad)
    !
    ! get "last" into array, just pad it there
    !
    call readBinnedOrReduced(1, izLast, array, ixCenStart + ixBoxRef, &
        iyCenStart + iyBoxRef, taperRef)

    limitXlo = -limitShiftX
    limitXhi = limitShiftX
    limitYlo = -limitShiftY
    limitYhi = limitShiftY
    if (ifCumulate .ne. 0) then
      limitXlo = xpeak - limitShiftX
      limitXhi = xpeak + limitShiftX
      limitYlo = ypeak - limitShiftY
      limitYhi = ypeak + limitShiftY
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
        call cubInterp(array, crray, nxUseBin, nyUseBin, nxUseBin, nyUseBin, fUnit, &
            nxUseBin / 2., nyUseBin / 2., unstretchDx, unstretchDy , 1., useMean, 0)
        do i = 1, nxUseBin * nyUseBin
          sumArray(i) = sumArray(i) + crray(i)
        enddo
      endif
      call taperInPad(sumArray, nxUseBin, nyUseBin, array, nxPad + 2, nxPad, nyPad, &
          nxTaper, nyTaper)
    else
      call taperInPad(array, nxUseBin, nyUseBin, array, nxPad + 2, nxPad, nyPad, &
          nxTaper, nyTaper)
    endif
    if (ifDoImOut .ne. 0) then
      do isOut = 1, 2
        if (isOut == 1) then
          call irepak(crray, array, nxPad + 2, nyPad, 0, nxPad-1, 0, nyPad-1)
        else
          call irepak(crray, brray, nxPad + 2, nyPad, 0, nxPad-1, 0, nyPad-1)
        endif
        if (mode .ne. 2) then
          call isetdn(crray, nxPad, nyPad, mode, 1, nxPad, 1, nyPad, dmin2, dmax2, dmean3)
        else
          call iclden(crray, nxPad, nyPad, 1, nxPad, 1, nyPad, dmin2, dmax2, dmean3)
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
    if (useCCC .and. deltaCTF == 0)  &
        crray(1:(nxPad + 2) * nyPad) = array(1:(nxPad + 2) * nyPad)
    call todfft(array, nxPad, nyPad, 0)
    call todfft(brray, nxPad, nyPad, 0)
    !
    if (deltaCTF .ne. 0.) then
      if (useCCC) then
        call filterPart(array, array, nxPad, nyPad, ctfp, deltaCTF)
        call filterPart(brray, brray, nxPad, nyPad, ctfp, deltaCTF)
        crray(1:(nxPad + 2) * nyPad) = array(1:(nxPad + 2) * nyPad)
      else
        call filterPart(array, array, nxPad, nyPad, ctfp, deltaCTF)
      endif
    endif
    !
    ! multiply array by complex conjugate of brray, put back in array
    !
    ! print *,'multiplying arrays'
    call conjugateProduct(array, brray, nxPad, nyPad)
    ! print *,'taking back fft'
    call todfft(array, nxPad, nyPad, 1)
    wallfft = wallfft + wallTime() - wallStart

    if (useCCC) then
      if (deltaCTF .ne. 0.) call todfft(crray, nxPad, nyPad, 1)
      call todfft(brray, nxPad, nyPad, 1)
    endif
    wallStart = wallTime()
    if (limitingShift)  &
        call setPeakFindLimits(limitXlo, limitXhi, limitYlo, limitYhi, ifEllipse)
    call xcorrPeakFindWidth(array, nxPad + 2, nyPad, xpeakList, ypeakList, &
        peakList, widths, widthMins, maxXcorrPeaks)
    !
    ! Get the true number of peaks and start an index to them
    do i = 1, maxXcorrPeaks
      if (peakList(i) > -0.9e30) numXcorrPeaks = maxXcorrPeaks
      indPeakSort(i) = i
    enddo
    wallPeak = wallPeak + wallTime() - wallStart
    cccMax = -10.
    indPeak = 1
    if (useCCC) then
      !
      ! Evaluate real-space correlation coefficient, using half of tapered region
      nxCCTrimA = (nxPad - nxInterp) / 2 + nxTaper / 2
      nyCCTrimA = (nyPad - nyInterp) / 2 + nyTaper / 2
      nxCCTrimB = (nxPad - nxUseBin) / 2 + nxTaper / 2
      nyCCTrimB = (nyPad - nyUseBin) / 2 + nyTaper / 2
      do i = 1, numXcorrPeaks
        ccc = CCCoefficientTwoPads(crray, brray, nxPad + 2, nxPad, nyPad, xpeakList(i), &
            ypeakList(i), nxCCTrimA, nyCCTrimA, nxCCTrimB, nyCCTrimB, 25, nsum)
        !
        ! Peaks with less than 1/8 overlap were ignored; this is a steep function
        ! to downweight them instead
        overlap = float(nsum) / ((nxPad - 2 * nxCCTrimA) * (nyPad - 2 * nyCCTrimA))
        wgtCCC = ccc * 1. / (1. +  &
            max(0.1, min(10., overlapCrit / overlap)) ** overlapPower)
        if (verbose) write(*,'(i3,a,2f7.1,a,e14.7,a,f6.3,a,2f8.5,a,2f8.2)') &
            i,' at ', xpeakList(i), ypeakList(i), ' peak =',peakList(i), ' ov = ', &
            overlap, ' cc =',ccc,wgtCCC,' width&Min =',widths(i),widthMins(i)
        if (wgtCCC > cccMax) then
          cccMax = wgtCCC
          indPeak = i
          if (i > 1 .and. verbose) print *,'Highest raw peak superceded!'
        endif
        peakList(i) = wgtCCC
      enddo
      !
      ! Sort the CCC's and reverse the index fo clarity below
      call rsSortIndexedFloats(peakList, indPeakSort, numXcorrPeaks)
      do i = 1, numXcorrPeaks / 2
        j = indPeakSort(i)
        indPeakSort(i) = indPeakSort(numXcorrPeaks + 1 - i)
        indPeakSort(numXcorrPeaks + 1 - i) = j
      enddo
    endif
    !
    ! If excluding central peaks, first determine if each peak is in the streak
    if (ifExclude > 0 .and. numXcorrPeaks > 1) then

      ! The bad peaks could really be anywhere in this extent although it is quite
      ! implausible for it to be all the way out at the end
      streak = 0.5 * (stretch - 1.0) * min(nxUse / max(0.01, abs(cosRotAngle)),  &
          nyUse / max(0.01, abs(sinRotAngle)))

      ! Determine if each peak is in or out of the streak, adjusting for the center
      ! if the box extraction is different, and keep track of the
      ! first and second one outside the streak
      indFirstOut = -1
      indSecondOut = -1
      do i = 1, numXcorrPeaks
        ind = indPeakSort(i)
        xpeakTmp = xpeakList(ind) - float(ixBoxCur - ixBoxRef) / nbinning
        ypeakTmp = ypeakList(ind) - float(iyBoxCur - iyBoxRef) / nbinning
        xrot = xpeakTmp * cosRotAngle - ypeakTmp * sinRotAngle
        yrot = xpeakTmp * sinRotAngle + ypeakTmp * cosRotAngle
        inStreak(ind) = abs(yrot) < radExclude .and. abs(xrot) < streak + radExclude
        if (.not. inStreak(ind)) then
          if (indFirstOut < 0) then
            indFirstOut = ind
          else if (indSecondOut < 0) then
            indSecondOut = ind
          endif
        endif
      enddo

      ! If the first peak is in the streak, and its minimum peak width is at least
      ! less than the mean width of the first non-streak peak, and 
      ! the third peak is sufficiently weaker than the second, need to get the 
      ! unbinned, unstretched correlation
      ind = indPeakSort(1)
      if (indFirstOut > 0 .and. inStreak(ind) .and. &
          widths(max(1, indFirstOut)) / widthMins(ind) > binWidthRatioCrit .and.  &
          (indSecondOut < 0 .or.  &
          peakList(max(1, indFirstOut)) / peakList(max(1, indSecondOut)) >  &
          peak2ToPeak3Crit)) then
        if (ifUBpeakIsSharp < 0) then
          if (verbose) &
              print *,'Evaluating first and second peak with unbinned correlation'

          ! Load both images unbinned from reference limits, taper, and take the
          ! correlation with high-pass filter only
          call irdbinned(1, izLast, ubArray, nxUse, nyUse, ixCenStart + ixBoxRef, &
              iyCenStart + iyBoxRef, 1, nxUse, nyUse, tmpArray, lenTemp, ierr)
          if (ierr .ne. 0) call exitError('READING IMAGE FILE')
          call taperInPad(ubArray, nxUse, nyUse, ubArray, nxUBpad + 2, nxUBpad, &
              nyUBpad, nxUBtaper, nyUBtaper)
          call meanZero(ubArray, nxUBpad + 2, nxUBpad, nyUBpad)
          call irdbinned(1, izCur, ubBrray, nxUse, nyUse, ixCenStart + ixBoxRef, &
              iyCenStart + iyBoxRef, 1, nxUse, nyUse, tmpArray, lenTemp, ierr)
          if (ierr .ne. 0) call exitError('READING IMAGE FILE')
          call taperInPad(ubBrray, nxUse, nyUse, ubBrray, nxUBpad + 2, nxUBpad, &
              nyUBpad, nxUBtaper, nyUBtaper)
          call meanZero(ubBrray, nxUBpad + 2, nxUBpad, nyUBpad)
          call todfft(ubArray, nxUBpad, nyUBpad, 0)
          call todfft(ubBrray, nxUBpad, nyUBpad, 0)
          call conjugateProduct(ubArray, ubBrray, nxUBpad, nyUBpad)
          if (deltaUBctf .ne. 0.) &
              call filterpart(ubArray, ubArray, nxUBpad, nyUBpad, ctfUB, deltaUBctf)
          call todfft(ubArray, nxUBpad, nyUBpad, 1)

          ! It's not clear if these limits should be applied...
          if (limitingShift)  &
              call setPeakFindLimits(-limitUBshiftX, limitUBshiftX, -limitUBshiftY, &
              limitUBshiftY, ifEllipse)
          call xcorrPeakFindWidth(ubArray, nxUBpad + 2, nyUBpad, ubXpeaks, ubYpeaks, &
              ubPeakList, widths, widthMins, 2)
          if (verbose) write(*,'(i2,2f9.2,e15.7,f8.2)') &
              (i,ubXpeaks(i), ubYpeaks(i),ubPeakList(i), widths(i),i=1,2)

          ! Accept the second peak if the first is still at origin and is narrow
          ! enough and if the width ratio is big enough
          ifUBpeakIsSharp = 0
          if (ubPeakList(2) > 0 .and. abs(ubXpeaks(1)) < 0.1 .and.  &
              abs(ubYpeaks(1)) < 0.1 .and. widths(1) <= centralPeakMaxWidth .and.  &
              widths(2) / widths(1) > ubWidthRatioCrit) then
            if (verbose) print *,'Rejecting peak at 0,0!'
            ifUBpeakIsSharp = 1
          endif
        endif
        if (ifUBpeakIsSharp > 0) indPeak = indFirstOut
      endif
    endif
    !
    ! Done with all picking of substitute peaks, now proceed with final values 
    xpeakTmp = xpeakList(indPeak)
    ypeakTmp = ypeakList(indPeak)
    peakVal = peakList(indPeak)
    return
  end subroutine correlateAndFindPeaks


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
    logical outside, inside
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
      ix = min(nx - 1., (nx + nxUnali) / 2. + dxPreali(indTilt),  &
          x0 + nx / 2. + nxPatch / 2.) - &
          max(0., (nx - nxUnali) / 2. + dxPreali(indTilt), x0 + nx / 2. - nxPatch / 2.)
      iy = min(ny - 1., (ny + nyUnali) / 2. + dyPreali(indTilt), &
          y0 + ny / 2. + nyPatch / 2.) - &
          max(0., (ny - nyUnali) / 2. + dyPreali(indTilt), y0 + ny / 2. - nyPatch / 2.)
      tmpArray(ipatch) = -1.
      if (ix > 0 .and. iy > 0 .and. &
          ix * iy > critNonBlank * nxPatch * nyPatch) then
        numUsable = numUsable + 1
        tmpArray(ipatch) = 1.
      endif
    enddo
    return
  end subroutine markUsablePatches


  ! makePatchListInsideBoundary copies the whole patch list from "all" to working arrays
  ! if there are no boundary contours, or just the ones inside the boundary contours
  !
  subroutine makePatchListInsideBoundary()
    logical inside
    numPatches = 0
    do ipatch = 1, numPatchesAll
      
      numInside = 0
      if (numBound > 0) then
        
        do ix = 1, 32
          x0 = patchCenXall(ipatch) + nxPatch * (ix - 16.5) / 32.
          do iy = 1, 32
            y0 = patchCenYall(ipatch) + nyPatch * (iy - 16.5) / 32.
            do iv = 1, numBound
              if (inside(xbound(indBound(iv) + 1), ybound(indBound(iv) + 1), &
                  numInBound(iv), x0, y0)) then
                numInside = numInside + 1
                exit
              endif
            enddo
          enddo
        enddo
        ! print *, patchCenXall(ipatch), patchCenYall(ipatch), numInside / 1024.
      endif
      !
      ! If there is no boundary or enough points are inside, keep the patch
      if (numBound == 0 .or. numInside / 1024. >= critInside) then
        numPatches = numPatches + 1
        patchCenX(numPatches) = patchCenXall(ipatch)
        patchCenY(numPatches) = patchCenYall(ipatch)
      endif
    enddo
    return
  end subroutine makePatchListInsideBoundary


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


! Determine the extent of usable area in a patch in one dimension, note this is 
! 1 or 2 pixels more stringent than in markUsablePatches
!
subroutine usablePatchExtent(nx, nxUnali, nxPatch, dx, ixStart, nxUsable)
  implicit none
  integer*4 nx, nxUnali, nxPatch, ixStart, nxUsable
  real*4 dx
  nxUsable = min(nx - 2, nint((nx + nxUnali) / 2. + dx) - 2, ixStart + nxPatch) - &
      max(2, nint((nx - nxUnali) / 2. + dx) + 2, ixStart)
  return
end subroutine usablePatchExtent


! Determine if a patch is sufficiently in area with image when there is a general
! transformation aplied to the image
!
subroutine evaluatePairPatch(nx, ny, nxPatch, nyPatch, ixStart, iyStart, prexfInv, &
    critNonBlank, taperCur, curViewOut)
  implicit none
  integer*4 nx, ny, nxPatch, nyPatch, ixStart, iyStart, numInside, ix, iy, numTest
  real*4 prexfInv(2, 3), critNonBlank
  real*4 xtmp, ytmp, area, xLoLf, xLoRt, xUpLf, xUpRt, yLoLf, yLoRt, yUpLf, yUpRt
  logical*4 taperCur, curViewOut
  !
  ! back-transform the 4 corners of the patch
  numTest = 10
  xtmp = ixStart
  ytmp = iyStart
  call xfapply(prexfInv, nx / 2., ny / 2., xtmp, ytmp, xLoLf, yLoLf)
  call xfapply(prexfInv, nx / 2., ny / 2., xtmp + nxPatch, ytmp, xLoRt, yLoRt)
  call xfapply(prexfInv, nx / 2., ny / 2., xtmp, ytmp + nyPatch, xUpLf, yUpLf)
  call xfapply(prexfInv, nx / 2., ny / 2., xtmp + nxPatch, ytmp + nyPatch, xUpRt, yUpRt)

  ! If they are all in, there is no tapering.  If all out, the patch is out
  taperCur = xLoLf < 1. .or. xUpLf < 1. .or. xLoRt > nx - 1. .or. xUpRt > nx - 1. .or.  &
      yLoLf < 1. .or. yUpLf < 1. .or. yLoRt > ny - 1. .or. yUpRt > ny - 1. 
  curViewOut = .false.
  if (.not. taperCur) return
  curViewOut = max(xLoRt, xUpRt) <= 0 .or. min(xLoLf, xUpLf) >= nx .or. &
      max(yLoRt, yUpRt) <= 0 .or. min(yLoLf, yUpLf) >= ny
  if (curViewOut) return

  ! Get intersection with the raw image area and compute the area from the vertices
  xLoLf = max(1., xLoLf)
  xUpLf = max(1., xUpLf)
  yLoLf = max(1., yLoLf)
  yLoRt = max(1., yLoRt)
  xLoRt = min(nx - 1., xLoRt)
  xUpRt = min(nx - 1., xUpRt)
  yUpLf = min(ny - 1., yUpLf)
  yUpRt = min(ny - 1., yUpRt)
  area = ((xLoLf * yLoRt - yLoLf * xLoRt) + (xLoRt * yUpRt - yLoRt * xUpRt) + &
      (xUpRt * yUpLf - yUpRt * xUpLf) + (xUpLf * yLoLf - yUpLf * xLoLf)) / 2.
  curViewOut = area < critNonBlank * nxPatch * nyPatch
  return
end subroutine evaluatePairPatch

! Routine to check for a warp file and exit with error if so
!
subroutine checkForWarpFile(xfInFile)
  character*(*) xfInFile
  character*240 strntmp
  integer*4 ierr, idx, idy, itmp, i, jj
  real*4 deltac
  integer*4 readCheckWarpFile
  ierr = readCheckWarpFile(xfinfile, 0, 0, idx, idy, itmp, i, deltac, jj, strntmp)
  if (ierr .ge. 0) call exitError( &
      'THE PREALIGN TRANSFORM FILE CONTAINS WARPING TRANSFORMS')
  if (ierr .ne. -1) then
    write(*, '(/,a)')'ERROR: A PROBLEM OCCURRED TESTING WHETHER THE INITIAL'// &
        ' TRANSFORM FILE HAD WARPINGS'
    call exitError(strntmp)
  endif
  return
end subroutine checkForWarpFile

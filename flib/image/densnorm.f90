! DENSNORM
!
! Densnorm can normalize images by dividing by the incident
! illumination, an operation usually referred to as mass normalization.
! It can take the log of normalized data, or simply take the log of the
! input data.  It can also output weighting factors that can be supplied
! to Tilt to provide a relative normalization of the data, thus
! avoiding the need to write a normalized file.
!
! See man page for details
!
! $Id$
!
program densnorm
  implicit none
  integer IDIM, MAXDOSE, MAXPIECE
  parameter (IDIM = 20000000, MAXDOSE = 4000, MAXPIECE = 100000)
  real*4 doses(MAXDOSE), refDoses(MAXDOSE)
  real*4 array(IDIM)
  integer*4 ixPiece(MAXPIECE), iyPiece(MAXPIECE), izPiece(MAXPIECE)
  integer*4 izpcInput(MAXPIECE), izpcSrc(MAXPIECE)
  integer*4 nxyz(3), mxyz(3), nx, ny, nz, nxyz2(3)
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  integer*4 i, j, maxLines, numChunks, iChunk, numLines, ierr, ierr2, ierr3
  integer*4 ifRefMean, ifRefDose, ifLog, ifPack, ndose, maxzInput, npcInput
  integer*4 iunit, nzsrc, mode2, maxzSrc, npcSrc, nrefDose, maxzRef, npcRef
  integer*4 modeIn, modeOut, ind, iz, cosPower
  real*4 refMean, refDose, dmin, dmax, dmean, dmin2, dmax2, dmean2
  real*4 tmin, tmax, tmean, sum, scale, rangeFrac, doseFac, valmin, baseLog
  real*4 addback, resubval
  real*8 dsum8
  character*320 inputFile, outfile, expFile, tiltFile, wgtFile, refFile, otherFile
  character*80 titlech
  character dat * 9, tim * 8, relText * 8, logText * 11
  logical*4 relative, reverse, ignoreExp, subtracted, resubtract, divideby2
  real*4 cosd
  common /bigarr/ array

  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetString, PipGetInteger, PipGetFloat, PipGetLogical
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  densnorm
  !
  integer numOptions
  parameter (numOptions = 21)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputImageFile:FN:@output:OutputImageFile:FN:@'// &
      'weight:WeightOutputFile:FN:@images:ImagesWithExposures:FN:@'// &
      'rifile:ReferenceImageFile:FN:@rimean:MeanOfReferenceImage:F:@'// &
      'riexp:ExposureOfReferenceImage:F:@'// &
      'subtracted:SubtractedIntegers:B:@resub:Resubtract:B:@'// &
      'divide:DivideBy2:B:@log:LogOfOutput:F:@mode:ModeOfOutput:I:@'// &
      'scale:ScalingFactor:F:@reverse:ReverseContrast:B:@'// &
      'ignore:IgnoreExposures:B:@expfile:ExposureFile:FN:@'// &
      'tiltfile:TiltFile:FN:@power:CosinePowerInverse:I:@'// &
      'minlog:MinimumLogFactor:F:@param:ParameterFile:PF:@help:usage:B:'
  !
  ! Defaults
  inputFile = ' '
  outfile = ' '
  expFile = ' '
  tiltFile = ' '
  wgtFile = ' '
  refFile = ' '
  otherFile = ' '
  cosPower = 1.
  ignoreExp = .false.
  reverse = .false.
  relative = .true.
  rangeFrac = 0.001
  baseLog = 0.
  subtracted = .false.
  resubtract = .false.
  divideby2 = .false.
  !
  ! Pip startup: set error, parse options, do help output
  call PipReadOrParseOptions(options, numOptions, 'densnorm', &
      'ERROR: DENSNORM - ', .false., 2, 1, 1, numOptArg, numNonOptArg)
  !
  ! Get file options, check for consistency
  ierr = PipGetInOutFile('InputImageFile', 1, ' ', inputFile)
  ierr = PipGetInOutFile('OutputImageFile', 2, ' ', outfile)
  if (inputFile == ' ' .and. outfile .ne. ' ') call exitError('YOU MUST'// &
      'SPECIFY AN INPUT IMAGE FILE IF YOU HAVE AN OUTPUT IMAGE FILE')
  ierr = PipGetString('WeightOutputFile', wgtFile)
  ierr = 1 - PipGetString('ExposureFile', expFile)
  ierr2 = 1 - PipGetString('TiltFile', tiltFile)
  ierr3 = 1 - PipGetString('ImagesWithExposures', otherFile)
  if (outfile == ' ' .and. wgtFile == ' ') call exitError( &
      'YOU MUST SPECIFY EITHER AN IMAGE OR WEIGHTING OUTPUT FILE')
  !
  if (ierr + ierr2 + ierr3 > 1) call exitError('YOU SHOULD ENTER'// &
      'ONLY ONE OF TILT FILE, EXPOSURE FILE, OR IMAGE FILE WITH EXPOSURES')
  !
  ! Get reference image options
  ierr = 1 - PipGetString('ReferenceImageFile', refFile)
  ifRefMean = 1 - PipGetFloat('MeanOfReferenceImage', refMean)
  ifRefDose = 1 - PipGetFloat('ExposureOfReferenceImage', refDose)
  if (ierr + ifRefMean + ifRefDose > 0 .and. ierr2 > 1) &
      call exitError('IT MAKES NO SENSE TO ENTER ANY REFERENCE '// &
      'INFORMATION WITH A TILT FILE')
  if (ierr + ifRefMean == 0 .and. ifRefDose == 1) call exitError( &
      'CANNOT USE A REFERENCE EXPOSURE WITHOUT A REFERENCE IMAGE OR MEAN')
  if (ierr + ifRefMean == 2) call exitError( &
      'IT MAKES NO SENSE TO ENTER BOTH A REFERENCE IMAGE AND A MEAN')
  !
  ! Other options than can be gotten at this time
  ifLog = 1 - PipGetFloat('LogOfOutput', baseLog)
  ierr = PipGetLogical('IgnoreExposures', ignoreExp)
  ierr = PipGetInteger('CosinePowerInverse', cosPower)
  ierr = PipGetLogical('ReverseContrast', reverse)
  ierr = PipGetFloat('MinimumLogFactor', rangeFrac)
  ierr = PipGetLogical('SubtractedIntegers', subtracted)
  ierr = PipGetLogical('Resubtract', resubtract)
  ierr = PipGetLogical('DivideBy2', divideby2)
  ifPack = 1
  ndose = 0
  if (inputFile .ne. ' ') then
    !
    ! Open and read the header of the input file to figure out if it is
    ! a montage; otherwise doses should be packed for existing Z values
    call imopen(1, inputFile, 'RO')
    call irdhdr(1, nxyz, mxyz, modeIn, dmin, dmax, dmean)
    call getHeaderPiecesDoses(1, array, IDIM * 4, nz, 0, doses,  MAXDOSE, ndose, &
        ixPiece, iyPiece, izpcInput, MAXPIECE, maxzInput, npcInput)
    if (npcInput .ne. 0) ifPack = 0
    modeOut = modeIn
    ierr = PipGetInteger('ModeOfOutput', modeOut)
  endif
  !
  ! Get exposures from somewhere: exposure file
  if (expFile .ne. ' ') then
    call dopen(1, expFile, 'ro', 'f')
    ndose = 0
10  read(1, *,end = 11, err = 95) doses(ndose + 1)
    ndose = ndose + 1
    goto 10
11  close(1)
    !
    ! Tilt file
  elseif (tiltFile .ne. ' ') then
    call dopen(1, tiltFile, 'ro', 'f')
    ndose = 0
20  read(1, *,end = 21, err = 96) doses(ndose + 1)
    ndose = ndose + 1
    goto 20
21  close(1)
    do i = 1, ndose
      doses(i) = 1. / cosd(doses(i))**(1. / cosPower)
    enddo
    !
    ! Image file unless ignoring
  elseif (.not. ignoreExp) then
    if (otherFile .ne. ' ') then
      call imopen(2, inputFile, 'RO')
      call irdhdr(2, nxyz2, mxyz, mode2, dmin2, dmax2, dmean2)
      iunit = 2
      nzsrc = nxyz2(3)
    else
      if (inputFile == ' ') call exitError( &
          'YOU MUST SPECIFY A SOURCE FOR THE NORMALIZING DATA')
      iunit = 1
      nzsrc = nz
    endif
    call getHeaderPiecesDoses(iunit, array, IDIM * 4, nzsrc, 0, doses, MAXDOSE, ndose, &
        ixPiece, iyPiece, izpcSrc, MAXPIECE, maxzSrc, npcSrc)
    if (iunit == 2) call imclose(2)
  elseif (ignoreExp) then
    !
    ! Ignore means ignore!
    ndose = 0
  endif
  !
  ! Test for bad doses
  do i = 1, ndose
    if (doses(i) == 0) call exitError( &
        'SOME DOSES ARE ZERO; USE -ignore TO IGNORE DOSES IN IMAGE FILE HEADER')
  enddo
  !
  ! Deal with doses if they exist
  if (ndose > 0) then
    !
    ! Are there the right number of doses?
    if (inputFile .ne. ' ' .and. (npcInput == 0 .and. ndose .ne. nz) .or. &
        (npcInput > 0 .and. ndose .ne. maxzInput)) call exitError( &
        'NUMBER OF EXPOSURES DOES NOT APPEAR TO MATCH NUMBER OF IMAGES')
    !
    ! Now evaluate whether there is reference data
    if (refFile .ne. ' ') then
      call imopen(2, refFile, 'RO')
      call irdhdr(2, nxyz2, mxyz, mode2, dmin2, dmax2, dmean2)
      if (subtracted .and. mode2 .ne. 1 .and. mode2 .ne. 2) call exitError( &
          'REFERENCE IMAGE FILE IS NOT THE RIGHT MODE TO CONTAIN '// &
          'SUBTRACTED VALUES')
      if (ifRefDose == 0) then
        !
        ! Get doses from reference image
        call getHeaderPiecesDoses(iunit, array, IDIM * 4, nxyz2(3), 1, refDoses, &
            MAXDOSE, nrefDose, ixPiece, iyPiece, izPiece, MAXPIECE, maxzRef, npcRef)
        if (nrefDose == 0) call exitError( &
            'REFERENCE IMAGE FILE HAS NO EXPOSURE DOSE VALUES')
        ifRefDose = 1
        refDose = refDoses(1)
      endif
      !
      ! Get mean of reference image
      dsum8 = 0.
      do j = 1, nxyz2(2)
        call irdlin(2, array, *99)
        do i = 1, nxyz2(1)
          dsum8 = dsum8 + array(i)
        enddo
      enddo
      refMean = (dsum8 / nxyz2(1)) / nxyz2(2)
      if (subtracted) refMean = refMean + 32768
      ifRefMean = 1
      call imclose(2)
    endif
    !
    ! Determine if the scaling is relative
    if (ifRefMean == 1 .and. ifRefDose == 1) relative = .false.
    !
    ! If doing output, check state of subtractions
    if (outfile .ne. ' ') then
      if (subtracted .and. modeIn .ne. 1 .and. modeIn .ne. 2) call exitError( &
          'INPUT IMAGE FILE IS NOT THE RIGHT MODE TO CONTAIN '// &
          'SUBTRACTED VALUES')
      if (resubtract .and. divideby2) call exitError( &
          'YOU CANNOT ENTER BOTH -resubtract AND -divideby2')
      if ((resubtract .or. divideby2) .and. .not.subtracted) call exitError( &
          'YOU CANNOT ENTER -resubtract OR -divideby2 UNLESS DATA WERE '// &
          'SUBTRACTED')
      if ((resubtract .or. divideby2) .and. (modeOut .ne. 1 .or. &
          .not.relative .or. ifLog .ne. 0)) call exitError( &
          'YOU CANNOT ENTER -resubtract OR -divideby2 UNLESS DOING '// &
          'RELATIVE NORMALIZATIONS WITH OUTPUT MODE OF 1 AND NO LOG')
      if (subtracted .and. baseLog > 30000) call exitError( &
          'YOU SHOULD ENTER "-log 0" WITH -subtracted')
    endif
    !
    ! Make the weights
    if (relative) then
      !
      ! Relative weights have a mean of zero
      sum = 0.
      do i = 1, ndose
        doses(i) = 1. / doses(i)
        sum = sum + doses(i) / ndose
      enddo
      do i = 1, ndose
        doses(i) = doses(i) / sum
      enddo
    else
      !
      ! Absolute weights
      do i = 1, ndose
        doses(i) = refDose / (doses(i) * refMean)
      enddo
    endif
    !
    ! Write to file if called for
    if (wgtFile .ne. ' ') then
      call dopen(1, wgtFile, 'new', 'f')
      write(1, '(f17.10)') (doses(i), i = 1, ndose)
      close(1)
    endif

  else
    !
    ! No doses are available: require log output, make unit factors
    if (outfile == ' ' .or. ifLog == 0) call exitError &
        ('YOU MUST ENTER SOME KIND OF EXPOSURE DATA OR PRODUCE LOG OUTPUT')
    ndose = nz
    if (npcInput > 0) ndose = maxzInput
    do i = 1, ndose
      doses(i) = 1.
    enddo
  endif
  !
  ! Done if not producing output
  if (outfile == ' ') then
    call imclose(1)
    call exit(0)
  endif
  !
  call imopen(2, outfile, 'new')
  call itrhdr(2, 1)
  call ialmod(2, modeOut)
  !
  ! Set scaling to 1 for float output, then to 25000 for attenuations,
  ! then to 5000 for logs
  scale = 1.
  if (PipGetFloat('ScalingFactor', scale) .ne. 0 .and. modeOut .ne. 2 &
      .and. (.not. relative .or. ifLog .ne. 0)) then
    scale = 25000.
    if (ifLog .ne. 0) scale = 5000.
    if (modeOut == 6) scale = scale * 2.
    if (modeOut == 0) scale = scale / 100.
    if (.not. relative .and. (modeOut == 6 .or. modeOut == 0)) then
      scale = -scale
      reverse = .false.
    endif
    print *,'Output will be scaled by default factor of', scale
  endif
  if (reverse) scale = -scale
  addback = 0.
  resubval = 0.
  if (subtracted) addback = 32768.
  if (divideby2) scale = scale * 0.5
  if (resubtract) resubval = 32768.
  call PipDone()
  !
  ! Process the image file in chunks
  maxLines = IDIM / nx
  numChunks = (ny + maxLines - 1) / maxLines
  dsum8 = 0.
  dmin2 = 1.e37
  dmax2 = -dmin2
  call imposn(1, 0, 0)
  do iz = 1, nz
    ind = izpcInput(iz) + 1
    if (ind < 1 .or. ind > ndose) then
      write(*,'(/,a,i6,a,i6,a)') 'ERROR: DENSNORM - SECTION', iz, ' HAS Z'// &
          ' VALUE OF', ind - 1, ', OUT OF RANGE OF NORMALIZATION FACTORS'
      call exit(1)
    endif
    !
    ! Get the scaling and the scaled minimum value for logs
    doseFac = doses(ind)
    valmin = rangeFrac * (dmax - dmin) * doseFac
    do iChunk = 1, numChunks
      numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
      call irdsecl(1, array, numLines, *100)
      !
      ! Log scaling (relative or absolute)
      if (ifLog .ne. 0) then
        do i = 1 , nx * numLines
          array(i) = scale * alog10(max(valmin, (array(i) + addback + &
              baseLog) * doseFac))
        enddo
        !
        ! Relative linear scaling
      elseif (relative) then
        do i = 1 , nx * numLines
          array(i) = (array(i) + addback) * scale * doseFac - resubval
        enddo
      else
        !
        ! Absolute linear scaling
        do i = 1 , nx * numLines
          array(i) = scale * ((array(i) + addback) * doseFac - 1.)
        enddo
      endif
      call iclden(array, nx, numLines, 1, nx, 1, numLines, tmin, tmax, tmean)
      dsum8 = dsum8 + tmean * numLines
      dmin2 = min(dmin2, tmin)
      dmax2 = max(dmax2, tmax)
      call iwrsecl(2, array, numLines)
    enddo
  enddo
  !
  dmean = (dsum8 / ny) / nz
  relText = 'Absolute'
  if (relative) relText = 'Relative'
  logText = 'linear'
  if (ifLog .ne. 0) logText = 'logarithmic'
  !
  call b3ddate(dat)
  call time(tim)
  write(titlech, 3000) relText, logText, dat, tim
3000 format('DENSNORM: ', a, ' density normalization, ', a, t57, a9, 2x, a8)
  call iwrhdrc(2, titlech, 1, dmin2, dmax2, dmean)
  call imclose(1)
  call imclose(2)
  call exit(0)

95 call exitError('READING EXPOSURE FILE')
96 call exitError('READING TILT FILE')
99 call exitError('READING REFERENCE IMAGE FILE')
100 call exitError('READING INPUT IMAGE FILE')
end program densnorm


! Gets piece list and doses from the header of unit IUNIT, with ARRAY
! as scratch array with MAXARR bytes, NZ dimension of file, IFPACK to
! pack dose data down for montage with missing images, NDOSE doses
! returned in DOSES of size MAXDOSE, NPIECE piece coordinates returned
! IXPIECE, IYPIECE, IZPIECE of size MAXPIECE, and MAXZ being the highest
! Z index numbered from 1
!
subroutine getHeaderPiecesDoses(iunit, array, maxarr, nz, ifPack, doses, &
    MAXDOSE, ndose, ixPiece, iyPiece, izPiece, MAXPIECE, maxz, npiece)
  implicit none
  integer*4 iunit, maxarr, nz, ndose, MAXDOSE, MAXPIECE, maxz
  integer*4 npiece, ixPiece(*), iyPiece(*), izPiece(*), ifPack
  real*4 array(*), doses(*)
  integer*4 i, nbExtra, nbyte, iflags, ndoseOut

  call irtnbsym(iunit, nbExtra)
  if (nbExtra > maxarr) call exitError( &
      'ARRAYS NOT LARGE ENOUGH FOR EXTRA HEADER DATA')
  call irtsym(iunit, nbExtra, array)
  call irtsymtyp(iunit, nbyte, iflags)
  call get_extra_header_pieces(array, nbExtra, nbyte, iflags, nz, &
      ixPiece, iyPiece, izPiece, npiece, MAXPIECE)
  if (npiece == 0) then
    do i = 1, nz
      izPiece(i) = i - 1
    enddo
    maxz = nz
  else
    maxz = 0
    do i = 1, npiece
      maxz = max(maxz, izPiece(i) + 1)
    enddo
  endif
  if (maxz > MAXDOSE) call exitError('Z VALUES TOO HIGH FOR ARRAYS')
  !
  ! set up a marker value for empty slots
  do i = 1, maxz
    doses(i) = -999.
  enddo
  call get_extra_header_items(array, nbExtra, nbyte, iflags, nz, 6, &
      doses, array, ndose, MAXDOSE, izPiece)
  if (ndose == 0 .or. ifPack == 0) return
  !
  ! pack the values down
  ndoseOut = 0
  do i = 1, ndose
    if (doses(i) .ne. -999.) then
      ndoseOut = ndoseOut + 1
      doses(ndoseOut) = doses(i)
    endif
  enddo
  ndose = ndoseOut
  return
end subroutine getHeaderPiecesDoses

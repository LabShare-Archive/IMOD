!*************HEADER.F**********************************************
!
! A SMALL PROGRAM TO READ THE HEADER ON AN IMAGE FILE
!
!************************************************************************
!
! $Id$
!
program header
  implicit none
  integer maxextra, ntypes
  parameter (maxextra = 2000000, ntypes = 6)
  real*4 array(maxextra/4)
  integer*4 NXYZ(3), MXYZ(3), mode, nbsym, numInt, numReal, ierr, i, j
  integer*4 labels(20,10), nlabel
  character*4 feichar
  logical nbytes_and_flags
  !
  CHARACTER*320 FILIN
  !
  character*17 typeName(ntypes) /'Tilt angles', 'Piece coordinates', &
      'Stage positions', 'Magnifications', 'Intensities', 'Exposure doses'/
  character*25 extractCom(ntypes) /'extracttilts', 'extractpieces', &
      'extracttilts -stage', 'extracttilts -mag', 'extracttilts -int', &
      'extracttilts -exp'/
  integer*4 numInputFiles, nfilein, ifBrief, iBinning, iflags, ifImod
  logical*4 doSize, doMode, doMin, doMax, doMean, silent, doPixel, doOrigin
  real*4 DMIN, DMAX, DMEAN, pixel, tiltaxis, delta(3)
  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetLogical, PipGetString, PipGetNonOptionArg, PipGetInteger
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  header
  !
  integer numOptions
  parameter (numOptions = 10)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FNM:@size:Size:B:@mode:Mode:B:@'// &
      'pixel:PixelSize:B:@origin:Origin:B:@minimum:Minimum:B:@'// &
      'maximum:Maximum:B:@mean:Mean:B:@brief:Brief:B:@help:usage:B:'
  !
  ! defaults
  !
  doSize = .false.
  doMode = .false.
  doMin = .false.
  doMax = .false.
  doMean = .false.
  doPixel = .false.
  doOrigin = .false.
  nfilein = 1
  ifBrief = 0
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  ! But turn off the entry printing first!
  call PipEnableEntryOutput(0)
  call PipReadOrParseOptions(options, numOptions, 'header', &
      'ERROR: HEADER - ', .true., 1, 2, 0, numOptArg, &
      numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0
  !
  if (pipinput) then
    ierr = PipGetLogical('Size', doSize)
    ierr = PipGetLogical('Mode', doMode)
    ierr = PipGetLogical('Max', doMax)
    ierr = PipGetLogical('Min', doMin)
    ierr = PipGetLogical('Mean', doMean)
    ierr = PipGetLogical('PixelSize', doPixel)
    ierr = PipGetLogical('Origin', doOrigin)
    ierr = PipGetInteger('Brief', ifBrief)
    call PipNumberOfEntries('InputFile', numInputFiles)
    nfilein = numInputFiles + numNonOptArg
    if (nfilein == 0) &
        call exitError('No input file specified')
  endif

  silent = doSize .or. doMode .or. doMin .or. doMax .or. doMean .or. &
      doPixel .or. doOrigin
  if (silent) call ialprt(.false.)
  call ialbrief(ifBrief)

  do i = 1, nfilein
    !
    ! get the next filename
    !
    if (pipinput) then
      if (i <= numInputFiles) then
        ierr = PipGetString('InputFile', filin)
      else
        ierr = PipGetNonOptionArg(i - numInputFiles, filin)
      endif
    else
      write(*,'(1x,a,$)') 'Name of input file: '
      READ(5, '(a)') FILIN
    endif
    !
    CALL IMOPEN(1, FILIN, 'RO')
    CALL IRDHDR(1, NXYZ, MXYZ, MODE, DMIN, DMAX, DMEAN)
    !
    if (silent) then
      if (doSize) write(*, '(3i8)') (nxyz(j), j = 1, 3)
      if (doMode) write(*, '(i4)') mode
      if (doPixel) then
        call irtdel(1, delta)
        write(*, '(3g11.4)') (delta(j), j = 1, 3)
      endif
      if (doOrigin) then
        call irtorg(1, delta(1), delta(2), delta(3))
        write(*, '(3g11.4)') (delta(j), j = 1, 3)
      endif
      if (doMin) write(*, '(g13.5)') dmin
      if (doMax) write(*, '(g13.5)') dmax
      if (doMean) write(*, '(g13.5)') dmean
    else
      call irtnbsym(1, nbsym)
      if (nbsym > 0 .and. nbsym <= maxextra) then
        call irtsym(1, nbsym, array)
        call irtsymtyp(1, numInt, numReal)
        if (.not. nbytes_and_flags(numInt, numReal) .and. numReal >= 12) then
          tiltaxis = array(numInt + 11)
          if (tiltaxis >= -360. .and. tiltaxis <= 360.) then
            if (tiltaxis < -180.) tiltaxis = tiltaxis + 360.
            if (tiltaxis > 180.) tiltaxis = tiltaxis - 360.
            call irtlab(1, labels, nlabel)
            write(feichar, '(a4)') labels(1, 1)
            if (feichar == 'Fei ') then
              write(*,101) - tiltaxis, ' (Corrected sign)'
            else
              write(*,101) tiltaxis
            endif
101         format(10x,'Tilt axis rotation angle = ', f7.1, a)
          endif
          pixel = array(numInt + 12) * 1.e9
          call irtdel(1, delta)
          call irtImodFlags(1, iflags, ifImod)
          if (pixel > 0.01 .and. pixel < 10000 .and. iand(iflags, 2) .eq. 0) then
            do j = 3, 1, -1
              iBinning = nint(delta(j))
              if (abs(delta(j) - iBinning) > 1.e-6 .or. iBinning <= 0 .or. &
                  iBinning > 4) then
                iBinning = 0
                exit
              endif
            enddo
            if (iBinning == 1) then
              write(*,102) pixel
            else if (iBinning > 1 .and. iBinning < 5) then
              write(*,102) pixel * iBinning, ' (Assumed binning of', iBinning, ')'
            else
              write(*,104) pixel
            endif
102         format(10x,'Pixel size in nanometers =', g11.4, a, i2, a)
104         format(10x,'Original pixel size in nanometers =', g11.4)
          endif
        endif
        if (nbytes_and_flags(numInt, numReal) .and. .not.silent .and. ifBrief == 0) then
          write(*,'(/,a)') 'Extended header from SerialEM contains:'
          do j = 1, ntypes
            if (mod(numReal / 2**(j - 1), 2) .ne. 0) &
                write(*,103) typeName(j), trim(extractCom(j))
103         format(2x, a, ' - Extract with "',a,'"')
          enddo
        endif
      endif
    endif

    CALL IMCLOSE(1)
  enddo
  !
  call exit(0)
END program header

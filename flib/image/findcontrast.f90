! * * * * * * FINDCONTRAST * * * * * *
!
! FINDCONTRAST finds the black and white contrast settings that are
! used when converting an image file to bytes with newstack or mrcbyte.
! It computes a histogram of pixel values within a selected volume,
! which it uses to determine the contrast settings that would truncate
! the values of a specified, small number of pixels in the volume.
!
! See man page for details
!
! David Mastronarde, 1/1/00
!
! $Id$
!
program findcontrast
  implicit none
  integer IDIM, LIMDEN
  parameter (IDIM = 10000 * 500)
  parameter (LIMDEN = 1000000)
  !
  integer*4 nxyz(3), mxyz(3), nx, ny, nz
  real*4 array(IDIM)
  integer*4 ihist(-LIMDEN:LIMDEN)
  real*4 dmin, dmax, dmean, areaFac, histScale, realLow, realHigh
  integer*4 mode, iyLow, iyHigh, ixLow, ixHigh, izLow, izHigh, numTruncLo, numTruncHi
  integer*4 nxTot, nyTot
  integer*4 maxLines, numChunks, ivalMin, ivalMax, iz, ichunk, iyEnd, iyChunk, ival
  integer*4 numTrunc, indLow, indHi, iconLow, iconHigh, i, numYlines, ierr, izLim, iylim
  logical*4 flipped, oldFlip
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  character*320 filename

  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetTwoIntegers, PipGetLogical
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  findcontrast
  !
  integer numOptions
  parameter (numOptions = 8)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@slices:SlicesMinAndMax:IP:@xminmax:XMinAndMax:IP:@'// &
      'yminmax:YMinAndMax:IP:@flipyz:FlipYandZ:B:@oldflip:OldFlipping:B:@'// &
      'truncate:TruncateBlackAndWhite:IP:@help:usage:B:'
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'findcontrast', &
      'ERROR: FINDCONTRAST - ', .true., 1, 1, 0, numOptArg, numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0

  if (PipGetInOutFile('InputFile', 1, 'Name of image file', filename) .ne. 0) &
      call exitError('NO INPUT FILE SPECIFIED')
  !
  ! Open image file
  !
  call imopen(1, filename, 'RO')
  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
  !
  histScale = 1.
  !
  ! For non-integer mode, set up a scaling that should fill 1/5 of the histogram
  ! at most, but allow the rest of the range in case the min and max are in error
  if (mode .ne. 1 .and. mode .ne. 6 .and. mode .ne. 0) then
    histScale = (LIMDEN / 5) / max(abs(dmin), abs(dmax), 1.e-10)
  endif

  if (nx > IDIM) call exitError('IMAGES TOO LARGE IN X FOR ARRAYS')

  flipped = .not.pipinput
  oldFlip = .false.
  if (pipinput) then
    ierr = PipGetLogical('FlipYandZ', flipped)
    ierr = PipGetLogical('OldFlipping', oldFlip)
  endif
  !
  ! Set up default limits
  !
  ixLow = nx / 10
  ixHigh = nx - 1 - ixLow
  !
  if (flipped) then
    izLim = ny
    iylim = nz
  else
    izLim = nz
    iylim = ny
  endif
  izLow = 1
  izHigh = izLim
  iyLow = iylim / 10
  iyHigh = iylim - 1 - iyLow
  !
  if (pipinput) then
    ierr = PipGetTwoIntegers('SlicesMinAndMax', izLow, izHigh)
    ierr = PipGetTwoIntegers('XMinAndMax', ixLow, ixHigh)
    ierr = PipGetTwoIntegers('YMinAndMax', iyLow, iyHigh)
  else
    write(*,'(1x,a,/,a,$)') 'First and last slice (Imod section #'// &
        ' in flipped volume)', '  to include in analysis: '
    read(5,*) izLow, izHigh
    write(*,'(1x,a,/,a,4i7,a,$)') 'Lower & upper X, lower & upper' &
        //' Y (in flipped volume) to include in analysis', &
        '  (/ for ', ixLow, ixHigh, iyLow, iyHigh, '): '
    read(5,*) ixLow, ixHigh, iyLow, iyHigh
  endif
  if (izLow <= 0 .or. izHigh > izLim .or. izLow > izHigh) call exitError( &
      'SLICE NUMBERS OUTSIDE RANGE OF IMAGE FILE')
  izLow = izLow - 1
  izHigh = izHigh - 1
  !
  if (ixLow < 0 .or. ixHigh >= nx .or. ixLow >= ixHigh .or. &
      iyLow < 0 .or. iyHigh >= iylim .or. iyLow > iyHigh) call exitError( &
      'X OR Y VALUES OUTSIDE RANGE OF VOLUME')
  !
  areaFac = max(1., nx * iylim * 1.e-6)
  numTruncLo = areaFac * (izHigh + 1 - izLow)
  numTruncHi = numTruncLo
  if (pipinput) then
    ierr = PipGetTwoIntegers('TruncateBlackAndWhite', numTruncLo, numTruncHi)
  else
    write(*,'(1x,a,/,a,2i8,a,$)') 'Maximum numbers of pixels to '// &
        'truncate at black and white in analyzed volume', &
        '  (/ for ', numTruncLo, numTruncHi, '): '
    read(5,*) numTruncLo, numTruncHi
  endif
  !
  ! Flip coordinates
  !
  if (flipped) then
    ierr = iyLow
    iyEnd = iyHigh
    if (oldFlip) then
      iyLow = izLow
      iyHigh = izHigh
    else
      iyLow = ny - 1 - izHigh
      iyHigh = ny - 1 - izLow
    endif
    izLow = ierr
    izHigh = iyEnd
  endif
  write(*,'(3(a,2i6))') 'Analyzing X:', ixLow, ixHigh, '  Y:', iyLow, iyHigh, '  Z:', &
      izLow, izHigh
  !
  do i = -LIMDEN, LIMDEN
    ihist(i) = 0
  enddo
  nxTot = ixHigh + 1 - ixLow
  nyTot = iyHigh + 1 - iyLow
  maxLines = IDIM / nxTot
  numChunks = (nyTot + maxLines - 1) / maxLines

  ivalMin = LIMDEN
  ivalMax = -LIMDEN
  do iz = izLow, izHigh
    iyChunk = iyLow
    do ichunk = 1, numChunks
      iyEnd = min(iyHigh, iyChunk + maxLines - 1)
      numYlines = iyEnd + 1 - iyChunk
      call imposn(1, iz, 0)
      call irdpas(1, array, nxTot, numYlines, ixLow, ixHigh, iyChunk, iyEnd)
      do i = 1, nxTot * numYlines
        ival = nint(histScale * array(i))
        ival = max(-LIMDEN, min(LIMDEN, ival))
        ihist(ival) = ihist(ival) + 1
        ivalMin = min(ivalMin, ival)
        ivalMax = max(ivalMax, ival)
      enddo
      iyChunk = iyEnd + 1
    enddo
  enddo
  !
  numTrunc = 0
  indLow = ivalMin
  do while(numTrunc <= numTruncLo .and. indLow < ivalMax)
    numTrunc = numTrunc + ihist(indLow)
    indLow = indLow + 1
  enddo
  !
  numTrunc = 0
  indHi = ivalMax
  do while(numTrunc <= numTruncHi .and. indHi > ivalMin)
    numTrunc = numTrunc + ihist(indHi)
    indHi = indHi - 1
  enddo
  ! write(*,'(i7,9i8)') (ihist(i), i=ivalMin, ivalMax)
  realLow = indLow / histScale
  realHigh = indHi / histScale
  iconLow = 255 * (realLow - dmin) / (dmax - dmin)
  iconHigh = 255 * (realHigh - dmin) / (dmax - dmin) + 0.99
  if (iconLow < 0 .or. iconHigh > 255) call exitError('THE FILE MINIMUM OR MAXIMUM '// &
      'IS TOO FAR OFF TO ALLOW CONTRAST SCALING; USE alterheader WITH mmm OPTION'// &
      ' TO FIX MIN/MAX')
  write(*,101) ivalMin / histScale, ivalMax / histScale, realLow, realHigh, iconLow,  &
      iconHigh
101 format('Min and max densities in the analyzed volume are', g13.5,' and',g13.5,/, &
      'Min and max densities with truncation are', g13.5,' and',g13.5,/, &
      'Implied black and white contrast levels are' ,i4,' and',i4)
  call exit(0)
end program findcontrast


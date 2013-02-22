! BINVOL  bins an image file down by potentially different amounts in X, Y, and Z
!
! $Id$
!
program binvol
  implicit none
  integer*4 nx, ny, nz, nxBin, nyBin, nzBin
  !
  integer*4 nxyz(3), mxyz(3), nxyzBin(3), nxyzst(3), mode
  real*4 dmin, dmax, dmean, cell(6), delta(3), xOrigin, yOrigin, zOrigin
  real*4, allocatable :: array(:), filtWeight(:)
  integer*4, allocatable :: inputStarts(:), inputEnds(:), iringPos(:)
  real*4 dmin2, dmax2, dmean2, zWeight, halfOffset
  real*8 dmeanSum, pixSum
  integer*4 izOut, iredFac(3), iredAll, i, inz, ierr, isec, maxLines, inBase
  integer*4 numChunks, iy, numLines, numBinLines, ibinX, ibinY, ibinZ, iChunk
  integer*4 limit, ifiltType, numExtra, izCenOffset, numZbefore, linesFilt
  integer*4 numSliceUsedIn, numSliceSummed, limDim, izCen, iringStart, iringEnd
  integer*4 lastZfinished, iyBinned, ioutBase, lastZadded, numZafter
  logical*4 fullOutSlices, spreadZ, verbose
  data nxyzst/0, 0, 0/
  !
  equivalence (nx, nxyz), (nxBin, nxyzBin), (ibinX, iredFac)
  common //nx, ny, nz, nxBin, nyBin, nzBin, ibinX, ibinY, ibinZ
  !
  character*320 bigFile, outFile
  character*9 dat
  character*8 tim
  character*80 titlech
  integer*4 selectZoomFilter
  real*8 zoomFiltValue
  !
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetLogical
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  binvol
  !
  integer numOptions
  parameter (numOptions = 11)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@binning:BinningFactor:I:@'// &
      'xbinning:XBinningFactor:I:@ybinning:YBinningFactor:I:@'// &
      'zbinning:ZBinningFactor:I:@antialias:AntialiasZFilter:I:@'// &
      'spread:SpreadSlicesInZ:B:@memory:MemoryLimit:I:@verbose:VerboseOutput:B:@'// &
      'help:usage:B:'
  !
  iredAll = 2
  limit = 1000
  ifiltType = 0
  spreadZ = .false.
  !
  call PipReadOrParseOptions(options, numOptions, 'binvol', &
      'ERROR: BINVOL - ', .false., 2, 1, 1, numOptArg, numNonOptArg)
  if (PipGetInOutFile('InputFile', 1, ' ', bigFile) .ne. 0)  &
      call exitError('NO INPUT FILE SPECIFIED')
  if (PipGetInOutFile('OutputFile', 2, ' ', outFile) .ne. 0)  &
      call exitError('NO OUTPUT FILE SPECIFIED')
  !
  ierr = PipGetInteger('BinningFactor', iredAll)
  ibinX = iredAll
  ibinY = iredAll
  ibinZ = iredAll
  ierr = PipGetInteger('XBinningFactor', ibinX)
  ierr = PipGetInteger('YBinningFactor', ibinY)
  ierr = PipGetInteger('ZBinningFactor', ibinZ)
  ierr = PipGetInteger('MemoryLimit', limit)
  if (limit < 1 .or. limit > 8000)  &
      call exitError('Memory limit must be between 1 and 8000')
  limDim = limit * 1024 * 256
  ierr = PipGetInteger('AntialiasZFilter', ifiltType)
  if (ifiltType == 1 .or. ifiltType > 6)  &
      call exitError('Antialias filter type must be between 2 and 6')
  ierr = PipGetLogical('VerboseOutput', verbose)
  ierr = PipGetLogical('SpreadSlicesInZ', spreadZ)
  if (spreadZ .and. ifiltType <= 0) &
      call exitError('Spreading in Z can be used only with antialias filtering')

  !
  ! Open image files.
  !
  call imopen(1, bigFile, 'RO')
  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
  if (nz == 1) ibinZ = 1
  !
  call imopen(3, outFile, 'NEW')
  !
  call itrhdr(3, 1)
  !
  call irtcel(1, cell)
  do i = 1, 3
    if (iredFac(i) < 1) call exitError('Binning factor must be at least 1')
    nxyzBin(i) = nxyz(i) / iredFac(i)
    if (nxyzBin(i) < 1) call exitError('Binning factor too large')
  enddo
  !
  ! Set up parameters of slices to add into each slice, etc
  if (ifiltType > 0) then
    if (selectZoomFilter(ifiltType - 1, 1. / ibinZ, linesFilt) .ne. 0) &
        call exitError('Setting up the antialias filter')
    allocate(filtWeight(-linesFilt:linesFilt), stat = ierr)
    call memoryError(ierr, 'array for filter weights')
    izCenOffset = ibinZ / 2
    halfOffset = 0.5 * (mod(ibinZ, 2) - 1)

    ! If the input Z size is not an exact multiple of the binning we can always spread
    ! to another output slice
    numExtra = mod(nz, ibinZ)
    if (spreadZ .and. numExtra > 0) then
      izCenOffset = numExtra / 2
      halfOffset = 0.5 * (mod(numExtra, 2) - 1)
      nzBin = nzBin + 1

      ! Add to the origin the distance from center of usual binned pixel to starting pixel
      call irtdel(1, delta)
      call irtorg(1, xOrigin, yOrigin, zOrigin)
      zOrigin = zOrigin + delta(3) * (ibinZ / 2. - (izCenOffset + halfOffset))
      call ialorg(3, xOrigin, yOrigin, zOrigin)
    endif
    !
    ! Get the weights, figure out the number of slices before and after the center
    numZbefore = -1
    do i = -linesFilt, linesFilt
      filtWeight(i) = zoomFiltValue(i - halfOffset)
      if (numZbefore < 0 .and. filtWeight(i) .ne. 0.) numZbefore = -i
      if (filtWeight(i) .ne. 0.) numZafter = i
    enddo
  else
    
    ! Ordinary binning
    izCenOffset = 0
    numZbefore = 0
    numZafter = ibinZ - 1
    halfOffset = 0.
  endif
  cell(1:3) = iredFac(1:3) * nxyzBin(1:3) * (cell(1:3) / mxyz(1:3))
  !
  ! Determine how many slices a particular input slice is used in
  numSliceUsedIn = 1
  numSliceSummed = 1 + numZbefore + numZafter
  do i = 1, numSliceSummed + 10
    if (i * ibinZ - numZbefore > numZafter) then
      exit
    else
      numSliceUsedIn = numSliceUsedIn + 1
    endif
  enddo

  ! First try holding all output slices that take data from one input slice
  fullOutSlices = .false.
  if (float(nxBin) * nybin * numSliceUsedIn < limDim) then
    inBase = nxBin * nyBin * numSliceUsedIn
    maxLines = ibinY * (((limDim - inBase) / nx) / ibinY)
    fullOutSlices = maxLines > 0
    maxLines = min(maxLines, ny)
  endif
  if (.not. fullOutSlices) then

    ! If that fails, process a strip at a time, reading all input for it sequentially
    ! and producing output
    maxLines = ibinY * ((limDim / (nx + (nxBin + ibinY - 1) / ibinY)) / ibinY - 1)
    if (maxLines < 1) call exitError( &
        'INPUT IMAGES TOO LARGE WITH GIVEN BINNING FACTORS AND MEMORY LIMIT')
    maxLines = min(maxLines, ny)
    inBase = nxBin * (maxLines / ibinY)
  endif
  allocate(array(nx * maxLines + inBase), stat = ierr)
  call memoryError(ierr, 'LARGE ARRAY FOR DATA')

  call ialsiz(3, nxyzBin, nxyzst)
  call ialsam(3, nxyzBin)
  call ialcel(3, cell)

  dmeanSum = 0.
  dmax = -1.e30
  dmin = 1.e30
  pixSum = 0.
  if (verbose) print *,'fullOutSlices',fullOutSlices,' num before, after', numZbefore, &
      numZafter, ' num used in, cen off, half', numSliceUsedIn, izCenOffset, halfOffset

  numChunks = (ny + maxLines - 1) / maxLines
  if (verbose) print *,numChunks, ' chunks, maximum lines', maxLines
  if (fullOutSlices) then
    !
    ! If holding full output slices, allocate and set up arrays for output slices
    allocate(inputStarts(0:nzBin), inputEnds(0:nzBin), iringPos(0:nzBin), stat=ierr)
    call memoryError(ierr, 'ARRAYS FOR OUTPUT SLICE NEEDS')
    do izOut = 0, nzBin - 1
      izCen = izOut * ibinZ + izCenOffset
      inputStarts(izOut) = max(0, izCen - numZbefore)
      inputEnds(izOut) = min(izCen + numZafter, nz - 1)
      iringPos(izOut) = 0
    enddo
    !
    ! Ring buffer
    iringStart = 0
    iringEnd = 0
    lastZfinished = -1
    !
    ! Loop on input slices and on strips in slices
    do inz = 0, nz - 1
      iy = 0
      iyBinned = 0
      do iChunk = 1, numChunks
        numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
        numBinLines = numLines / ibinY
        call imposn(1, inz, iy)
        call irdsecl(1, array(inBase + 1), numLines, *99)
        !
        ! Loop on output slices that need this input
        do izOut = lastZfinished + 1, nzBin - 1
          if (inz > inputEnds(izOut)) then
            exit
          endif
          if (inz >= inputStarts(izOut)) then
            !
            ! If slice is not in ring yet, assign and zero it
            if (iringPos(izOut) == 0) then
              iringEnd = iringEnd + 1
              if (iringEnd > numSliceUsedIn) iringEnd = 1
              if (iringEnd == iringStart) call exitError('PROBLEM WITH RING BUFFER')
              ioutBase = (iringEnd - 1) * nxBin * nyBin + 1
              array(ioutBase : ioutBase + nxBin * nyBin - 1) = 0.
              iringPos(izOut) = iringEnd
              if (iringStart == 0) iringStart = 1
              if (verbose) print *,'Assigning ',izOUt,' to slot ',iringEnd
            endif
            !
            ! Get weighting and add the strip in
            izCen = izOut * ibinZ + izCenOffset
            call sliceWeighting(inz, zWeight)
            ioutBase = (iringPos(izOut) - 1) * nxBin * nyBin + nxBin * iyBinned + 1
            if (verbose) print *,'Adding',inz,' into',izOut,', weight',zweight, ioutBase
            call add_into_array(array(inBase + 1), nx, numLines, array(ioutBase), nxBin, &
                numBinLines, iredFac, zWeight)
          endif
          lastZadded = izOut
        enddo

        ! Advance input and output lines for next chunk
        iy = iy + numLines
        iyBinned = iyBinned + numBinLines
      enddo
      !
      ! Loop again on output slices and see if any are done
      ! If so, write the slice, advance ring start
      do izOut = lastZfinished + 1, lastZadded
        if (inz == inputEnds(izOut)) then
          ioutBase = (iringPos(izOut) - 1) * nxBin * nyBin + 1
          call iclden(array(ioutBase), nxBin, nyBin, 1, nxBin, 1, nyBin, dmin2, dmax2, &
            dmean2)
          call iwrsec(3, array(ioutBase))
          if (verbose) print *,'Wrote ',izOut,ioutBase
          !
          dmax = max(dmax, dmax2)
          dmin = min(dmin, dmin2)
          dmeanSum = dmeanSum + dmean2 * nyBin * nxBin
          pixSum = pixSum + nyBin * nxBin
          lastZfinished = izOut
          iringStart = iringStart + 1
        endif
      enddo
    enddo
  else
    !
    ! When output slices can't all be held for an input slice
    ! Loop on output slices then on chunks in slice
    do izOut = 0, nzBin - 1
      iy = 0
      do iChunk = 1, numChunks
        numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
        numBinLines = numLines / ibinY
        array(1:inBase) = 0.
        izCen = izOut * ibinZ + izCenOffset
        do inz = max(0, izCen - numZbefore), min(izCen + numZafter, nz - 1)
          call imposn(1, inz, iy)
          call irdsecl(1, array(inBase + 1), numLines, *99)
          call sliceWeighting(inz, zWeight)
          call add_into_array(array(inBase + 1), nx, numLines, array, nxBin, &
              numBinLines, iredFac, zWeight)
        enddo
        iy = iy + numLines

        call iclden(array, nxBin, numBinLines, 1, nxBin, 1, numBinLines, dmin2, dmax2, &
            dmean2)
        call iwrsecl(3, array, numBinLines)
        !
        dmax = max(dmax, dmax2)
        dmin = min(dmin, dmin2)
        dmeanSum = dmeanSum + dmean2 * numBinLines * nxBin
        pixSum = pixSum + numBinLines * nxBin
      enddo
    enddo
  endif
  !
  dmean = dmeanSum / pixSum
  call b3ddate(dat)
  call time(tim)
  !
  write(titlech, 1500) ibinX, ibinY, ibinZ, dat, tim
1500 format('BINVOL: Volume binned down by factors', 3i4, t57,a9,2X,a8)

  call iwrhdrc(3, titlech, 1, dmin, dmax, dmean)
  call imclose(3)
  call imclose(1)
  !
  write(6, 500)
500 format(' PROGRAM EXECUTED TO END.')
  call exit(0)
99 call exitError('READING IMAGE')

CONTAINS

  subroutine sliceWeighting(izIn, weight)
    integer*4 jj, izIn, jstart, jend
    real*4 weight
    if (ifiltType <= 0) then
      weight = 1. / ibinZ
      return
    endif
    weight = 0.
    jstart = izIn - izCen
    jend = jstart
    if (izIn == 0) jstart = -numZbefore
    if (izIn == nz - 1) jend = numZafter
    do jj = jstart, jend
      weight = weight + filtWeight(jj)
    enddo
    return
  end subroutine sliceWeighting

end program binvol


subroutine add_into_array(array, nx, ny, brray, nxBin, nyBin, iredFac, zWeight)
  implicit none
  integer*4 iredFac(3), ixBin, iyBin, ix, iy, nx, ny, nxBin, nyBin
  real*4 array(nx,ny), brray(nxBin,nyBin), denom, zWeight
  denom = iredFac(1) * iredFac(2) / zWeight

  if (iredFac(1) * iredFac(2) == 1) then
    !
    ! Parallelizing this did more harm than good
    do iyBin = 1, nyBin
      do ixBin = 1, nxBin
        brray(ixBin, iyBin) = brray(ixBin, iyBin) + array(ixBin, iyBin) * zWeight
      enddo
    enddo
  else
    !$OMP PARALLEL DO &
    !$OMP SHARED(nx, ny, nxBin, nyBin, denom, zWeight, array, brray, iredFac) &
    !$OMP PRIVATE(ixBin, iyBin, ix, iy) &
    !$OMP DEFAULT(NONE)
    do iyBin = 1, nyBin
      do ixBin = 1, nxBin
        do ix = (ixBin - 1) * iredFac(1) + 1, ixBin * iredFac(1)
          do iy = (iyBin - 1) * iredFac(2) + 1, iyBin * iredFac(2)
            brray(ixBin, iyBin) = brray(ixBin, iyBin) + array(ix, iy) / denom
          enddo
        enddo
      enddo
    enddo
  endif
  return
end subroutine add_into_array

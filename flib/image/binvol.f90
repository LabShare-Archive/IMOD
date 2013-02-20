! BINVOL  bins an image file down by equal amounts in X, Y, and Z
!
! $Id$
!
program binvol
  implicit none
  integer idim
  parameter (idim = 3500 * 3500)
  integer*4 nx, ny, nz, nxBin, nyBin, nzBin
  !
  integer*4 nxyz(3), mxyz(3), nxyzBin(3), nxyzst(3), mode
  real*4 array(idim), dmin, dmax, dmean, cell(6)
  real*4 dmin2, dmax2, dmean2
  real*8 dmeanSum, pixSum
  integer*4 izOut, iredFac(3), iredAll, i, inz, ierr, isec, maxLines, inBase
  integer*4 numChunks, iy, numLines, numBinLines, ibinX, ibinY, ibinZ, iChunk
  integer*4 limit, ifLimSet
  data nxyzst/0, 0, 0/
  !
  equivalence (nx, nxyz), (nxBin, nxyzBin), (ibinX, iredFac)
  common //nx, ny, nz, nxBin, nyBin, nzBin, ibinX, ibinY, ibinZ
  !
  character*320 bigFile, outFile
  character*9 dat
  character*8 tim
  character*80 titlech
  common / bigarr / array
  !
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  binvol
  !
  integer numOptions
  parameter (numOptions = 8)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@'// &
      'binning:BinningFactor:I:@xbinning:XBinningFactor:I:@'// &
      'ybinning:YBinningFactor:I:@zbinning:ZBinningFactor:I:@'// &
      'test:TestLimit:I:@help:usage:B:'
  !
  iredAll = 2
  limit = idim
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
  ifLimSet = 1 - PipGetInteger('TestLimit', limit)
  limit = min(limit, idim)
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
    cell(i) = iredFac(i) * nxyzBin(i) * (cell(i) / mxyz(i))
  enddo
  !
  maxLines = ibinY * ((limit / (nx + (nxBin + ibinY - 1) / ibinY)) / ibinY - 1)
  if (maxLines < 1) call exitError('INPUT' &
      //' IMAGES TOO LARGE FOR ARRAYS WITH GIVEN BINNING FACTORS')
  call ialsiz(3, nxyzBin, nxyzst)
  call ialsam(3, nxyzBin)
  call ialcel(3, cell)

  dmeanSum = 0.
  dmax = -1.e30
  dmin = 1.e30
  pixSum = 0.

  inBase = nxBin * (maxLines / ibinY)
  numChunks = (ny + maxLines - 1) / maxLines
  if (ifLimSet .ne. 0) print *,numChunks, ' chunks, maximum lines', maxLines, inBase
  do izOut = 0, nzBin - 1
    iy = 0
    do iChunk = 1, numChunks
      numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
      numBinLines = numLines / ibinY
      do i = 1, inBase
        array(i) = 0.
      enddo
      isec = izOut * ibinZ
      do inz = 1, ibinZ
        call imposn(1, isec, iy)
        isec = isec + 1
        call irdsecl(1, array(inBase + 1), numLines, *99)

        call add_into_array(array(inBase + 1), nx, numLines, &
            array, nxBin, numBinLines, iredFac)
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
  !
  dmean = dmeanSum / pixSum
  call date(dat)
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
end program binvol


subroutine add_into_array(array, nx, ny, brray, nxBin, nyBin, iredFac)
  implicit none
  integer*4 iredFac(3), ixBin, iyBin, ix, iy, nx, ny, nxBin, nyBin
  real*4 array(nx,ny), brray(nxBin,nyBin), denom
  denom = iredFac(1) * iredFac(2) * iredFac(3)
  do iyBin = 1, nyBin
    do ixBin = 1, nxBin
      do ix = (ixBin - 1) * iredFac(1) + 1, ixBin * iredFac(1)
        do iy = (iyBin - 1) * iredFac(2) + 1, iyBin * iredFac(2)
          brray(ixBin, iyBin) = brray(ixBin, iyBin) + array(ix, iy) / denom
        enddo
      enddo
    enddo
  enddo
  return
end subroutine add_into_array

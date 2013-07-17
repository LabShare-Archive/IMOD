! *FFTRANS.F**************************************************************
! *
! This program will do 2- or 3-dimensional FFT's in either direction. The real-space
! origin is at (1, 1) and the origin of reciprocal space is at (1, NY/2+1). 
! The FT of an image NX, NY is NX/2+1, NY complex values. Transforms are done using
! FFTW, in which case the only restriction applied here is that NX is even, or with
! Lynn ten Eyck's subroutines (in IMOD), which allow arbitrary-sized images having a
! largest prime factor of 19.
!
! See man page for more details
! *
! DNM modified to do in "memory" rather than use giantfft as long as
! the image will actually fit into the dimensioned array.  Also, changed
! to properly package arrays in memory and use iwrsec to write sections;
! this is much faster and saves lots of disk access.
!
! Version  1.00   18.10.81        DAA             FOR VAX         *
! Version  1.01   27.MAY.82       DAA             FOR VAX         *
! Version  1.02   10.JUNE.82      DAA             FOR VAX         *
! Version  1.03   23.JULY.82      DAA             FOR VAX         *
! Version  1.04   01.OCTOBER.82   DAA             FOR VAX         *
! Version  1.05   08.November.82  DAA             FOR VAX         *
! Update   1.05   18.November.82  DAA             FOR VAX         *
! Revision 1.06   27.November.84  RH              FOR VAX         *
! Bitmodes 1.07   09.August.88    DNM             FOR uVAX        *
! Bug fix  1.07   31.December.88  DNM             FOR uVAX        *
! Ported to unix  07.December.94  DNM             FOR SGI         *
! *
!************************************************************************
!
! $Id$
!
program fftrans
  implicit none
  integer*4 nxyzr(3), mxyzr(3), nxyzf(3), nxyzst(3)
  character dat*9, tim*8
  character*80 titlech

  logical do3d, quiet, verbose
  integer*4 nx, ny, nz, iforward, iback, mode, nzMinus1, nxMinus1
  integer*4 nyMinus1, nxOver2, nxOver2Plus1
  integer*4 nxPlus2, isec, index, nxReal, iy, indBase, ierr, modeOut
  equivalence (nx, nxyzr(1)), (ny, nxyzr(2)), (nz, nxyzr(3))
  real*4 zero, dmin, dmax, dmean, tmin, tmax, tmean, cell(6)
  character*320 fileIn, fileOut
  real*4, allocatable :: array(:), tmpArray(:)
  data iforward/0/, iback/1/, zero/0.0/, nxyzst/3*0/
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetLogical, PipGetInteger
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  fftrans
  !
  integer numOptions
  parameter (numOptions = 6)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@:3dfft:B:@'// &
      'mode:Mode:I:@quiet:Quiet:B:@help:usage:B:'
  !
  do3d = .false.
  modeOut = 2
  quiet = .false.
  !
  call PipReadOrParseOptions(options, numOptions, 'fftrans', 'ERROR: FFTRANS - ', &
      .true., 2, 1, 1, numOptArg, numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0
  if (pipInput) then
    ierr = PipGetLogical('Quiet', quiet)
    ierr = PipGetInteger('Mode', modeOut)
    if (quiet) call ialprt(.false.)
  endif
  verbose = .not.quiet
  !
  if (verbose) write(6, 1000)
1000 format(/,' FFTRANS: Fourier Transform Program  V1.10',/)
  !
  if (PipGetInOutFile('InputFile', 1, 'Name of input file', fileIn) .ne. 0) &
      call exitError('NO INPUT FILE SPECIFIED')
  !
  if (PipGetInOutFile('OutputFile', 2, 'Name of output file', fileOut) .ne. 0) &
      call exitError('NO OUTPUT FILE SPECIFIED')
  call imopen(1, fileIn, 'RO')
  call imopen(2, fileOut, 'NEW')
  call b3dDate(dat)
  call time(tim)
  !
  ! Read input header & decide which direction to go
  !
  call irdhdr(1, nxyzr, mxyzr, mode, dmin, dmax, dmean)
  call itrhdr(2, 1)
  !
  ! DNM 12/19/98: just set this to the true buffer size
  nzMinus1 = nz - 1
  tmin =  1.e37
  tmax = -tmin
  tmean = 0.0

  if (nz > 1) then
    if (pipInput) ierr = PipGetLogical('3dfft', do3d)
    if (.not.do3d .and. verbose) write(6, 1200) nz
1200 format(/' Each of the ',i4,' sections are SEPARATELY transformed',/)
  endif

  nxPlus2 = nx + 2
  if (mode == 3 .or. mode == 4) nxPlus2 = 2 * nx
  index = nxPlus2 * ny
  isec = nxPlus2
  if (do3d) then
    index = nxPlus2 * ny * nz
    isec = nxPlus2 * nz
  endif
  allocate(array(index), tmpArray(isec), stat = ierr)
  call memoryError(ierr, 'ARRAY FOR IMAGE DATA')

  if (.not.(mode == 3 .or. mode == 4)) then
    !
    ! Here for forward transform
    !
    if (mod(nx, 2) .ne. 0) call exitError('IMAGE SIZE IN X MUST BE EVEN')
    nxMinus1 = nx - 1
    nyMinus1 = ny - 1
    nxOver2 = nx / 2
    nxOver2Plus1 = nxOver2 + 1
    nxPlus2 = nx + 2
    nxyzf(1) = nxOver2Plus1
    nxyzf(2) = ny
    nxyzf(3) = nz

    call ialmod(2, 4)
    call ialsiz(2, nxyzf, nxyzst)
    if (quiet) print *,'FFTRANS: Computing forward Fourier transform'
    !
    write(titlech, 1500) dat, tim
1500 format('FFTRANS: Forward Fourier Transform',t57,a9, 2x,a8)
    call iwrhdrc(2, titlech, 1, zero, zero, zero)
    if (.not.do3d) then
      !
      ! Loop over all sections & write out with shifted origin
      !
      do isec = 0, nzMinus1
        call irdpas(1, array, nxPlus2, ny, 0, nxMinus1, 0, nyMinus1, *99)
        call todfft(array, nx, ny, iforward)
        ! DNM: a speed trick: swap the lines in memory and use iwrsec
        call wrapFFTslice(array, tmpArray, nxOver2Plus1, ny, 0)
        call iwrsec(2, array)
        call iclcdn(array, nxOver2Plus1, ny, 1, nxOver2Plus1, 1, ny, dmin, dmax, &
            dmean)
        tmin = min(tmin, dmin)
        tmax = max(tmax, dmax)
        tmean = tmean + dmean
        if (nz > 1 .and. verbose) write(6, 1600) isec, dmin, dmax, dmean
      enddo
1600  format(' Min,Max,Mean for section ',i4,' are: ',3g13.5)
    else
      !
      ! 3D forward FFT
      !
      do isec = 0, nzMinus1
        call irdpas(1, array(isec * nxPlus2 * ny + 1), nxPlus2, ny, 0, nxMinus1, 0,  &
            nyMinus1, *99)
      enddo
      call thrdfft(array, tmpArray, nx, ny, nz, iforward)
      index = nxPlus2 * ny * ((nz + 1) / 2) + 1
      do isec = 0, nzMinus1
        if (isec == nz / 2) index = 1
        call wrapFFTslice(array(index), tmpArray, nxOver2Plus1, ny, 0)
        call iwrsec(2, array(index))
        call iclcdn(array(index), nxOver2Plus1, ny, 1, nxOver2Plus1, 1, ny, dmin, dmax, &
            dmean)
        tmin = min(tmin, dmin)
        tmax = max(tmax, dmax)
        tmean = tmean + dmean
        index = index + nxPlus2 * ny
      enddo
    endif
  else
    !
    ! Here for inverse transform
    !
    nxReal = (nx - 1) * 2
    nxPlus2 = nxReal + 2
    nxyzf(1) = nxReal
    nxyzf(2) = ny
    nxyzf(3) = nz
    index = nxPlus2 * ((ny + 1) / 2) + 1
    call ialmod(2, modeOut)
    call ialsiz(2, nxyzf, nxyzst)
    if (quiet) print *,'FFTRANS: Computing inverse Fourier transform'
    !
    write(titlech, 1700) dat, tim
1700 format('FFTRANS: Inverse Fourier Transform',t57,a9, 2x,a8)
    call iwrhdrc(2, titlech, 1, zero, zero, zero)
    if (.not.do3d) then
      !
      ! Loop over all sections , shift origin on reading in
      !
      do isec = 0, nzMinus1
        do iy = 1, ny
          if (iy == ny / 2 + 1) index = 1
          call irdlin(1, array(index),*99)
          index = index + nxPlus2
        enddo
        call todfft(array, nxReal, ny, iback)
        call iclden(array, nxPlus2, ny, 1, nxReal, 1, ny, dmin, dmax, dmean)
        ! DNM: moved the ICLDEN up, switched to a repack and iwrsec
        call irepak(array, array, nxPlus2, ny, 0, nxReal - 1, 0, ny - 1)
        call iwrsec(2, array)
        tmin = min(tmin, dmin)
        tmax = max(tmax, dmax)
        tmean = tmean + dmean
        if (nz > 1 .and. verbose) write(6, 1600) isec, dmin, dmax, dmean
      enddo
    else
      !
      ! backward 3D transform: reorder sections as well as Y on read-in
      !
      indBase = nxPlus2 * ny * ((nz + 1) / 2)
      do isec = 0, nzMinus1
        if (isec == nz / 2) indBase = 0
        do iy = 1, ny
          if (iy == ny / 2 + 1) index = 1
          call irdlin(1, array(index + indBase),*99)
          index = index + nxPlus2
        enddo
        indBase = indBase + nxPlus2 * ny
      enddo
      !
      ! Call 3d routine 
      !
      call thrdfft(array, tmpArray, nxReal, ny, nz, 1)
      do isec = 0, nzMinus1
        index = isec * nxPlus2 * ny + 1
        call iclden(array(index), nxPlus2, ny, 1, nxReal, 1, ny, dmin, dmax, dmean)
        call irepak(array(index), array(index), nxPlus2, ny, 0, nxReal - 1, 0, ny - 1)
        call iwrsec(2, array(index))
        tmin = min(tmin, dmin)
        tmax = max(tmax, dmax)
        tmean = tmean + dmean
      enddo
    endif
  endif
  !
  ! Adjust MX value but adjust cell to retain the pixel size
  if (nx == mxyzr(1) .and. ny == mxyzr(2) .and. nz == mxyzr(3)) then
    call ialsam(2, nxyzf)
    call irtcel(1, cell)
    cell(1) = (cell(1) * nxyzf(1)) / nx
    call ialcel(2, cell)
  endif
  tmean = tmean / nz
  if (verbose) write(6, 1800) tmin, tmax, tmean
1800 format(/,' Overall Min,Max,Mean values are: ',3g13.5)
  call iwrhdrc(2, titlech, -1, tmin, tmax, tmean)
  call imclose(1)
  call imclose(2)
  call exit(0)
  !
99 call exitError('READING FILE')
  call exit(1)
end program fftrans

! * * * * * FILLTOMO * * * * * *
!
! FILLTOMO improves a combined tomogram from a two-axis
! tilt series by replacing pixels in locations where the "matching"
! tomogram had no data with the values from the tomogram that was
! matched to.  It determines a linear scaling between the latter
! tomogram and the combined tomogram so that the intensities will
! match as well as possible.
!
! See man page for details
!
! David Mastronarde, November 1995
!
! $Id$
!
program filltomo
  implicit none
  integer maxSample, limThreads
  parameter (maxSample = 100000, limThreads = 8)
  !
  integer*4 nxyz(3), mxyz(3), nxyzst(3)
  real*4, allocatable :: array(:), brray(:)
  !
  character*320 inFile, outFile
  !
  integer*4 nxIn, nyIn, nzIn, nxOut, nyOut, nzOut, nx, ny, nz, mode
    real*4 cxyzIn(3), cxyzOut(3), cenXin, cenYin, cenZin, cenXout, cenYout, cenZout
  integer*4 nxyzIn(3), nxyzOut(3), idim
  common /xyz/ nxIn, nyIn, nzIn, nxOut, nyOut, nzOut, cenXin, cenYin, cenZin, cenXout, &
      cenYout, cenZout, nx, ny, nz
  equivalence (nxyzIn(1), nxIn), (nxyzOut(1), nxOut)
  equivalence (cxyzIn(1), cenXin), (cxyzOut(1), cenXout)
  equivalence (nx, nxyz)
  real*4 minv(3,3), title(20)
  real*4 avg(2), sd(2), dminThread, dmaxThread
  integer*4, allocatable :: ixSrcStart(:), ixSrcEnd(:), ixMatStart(:), ixMatEnd(:)
  character dat*9, tim*8
  character*80 titlech
  integer*4 i, nLeftFill, nRightFill, nBotFill, nTopFill, iunit, idelSamp, numSampX
  integer*4 ixStart, numSampY, iyStart, j, iz, izSrc, ibinning, numFillOnSec
  integer*4 numSampZ, izStart, ndat, jz, jy, iy, jx, ix, numSecRead, ind, kti
  real*4 sem, scale, facAdd, zcen, ycen, xcen, xp, yp, zp, val, dminOut, dmaxOut, dmeanOut
  real*4 dmin, dmax, dmean
  integer*4 indBase, numFillThread, numThreads
  integer (kind = 8) numPtFilled
  logical pipinput, doFill, doMatTests
  integer*4  numOptArg, numNonOptArg, ierr, ifSrcLimits, ifMatLimits
  integer*4 PipGetInOutFile, PipGetString, PipGetTwoIntegers, PipGetInteger
  integer*4 numOMPthreads, b3dOMPthreadNum
  ! real*8 wallTime, wallStart, wallTot
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  filltomo
  integer numOptions
  parameter (numOptions = 15)
  character*(40 * numOptions) options(1)
  options(1) = &
      'matched:MatchedToTomogram:FN:@fill:FillTomogram:FN:@'// &
      'source:SourceTomogram:CH:@inverse:InverseTransformFile:FN:@'// &
      'xfill:LeftRightFill:IP:@yfill:BottomTopFill:IP:@:ImagesAreBinned:I:@'// &
      'sraw:SourceRawStackSize:IT:@sxform:SourceStackTransforms:FN:@'// &
      'sboundary:SourceBoundaryModel:FN:@mraw:MatchedToRawStackSize:IT:@'// &
      'mxform:MatchedToStackTransforms:FN:@mboundary:MatchedToBoundaryModel:FN:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  call PipReadOrParseOptions(options, numOptions, 'filltomo', 'ERROR: FILLTOMO - ', &
      .true., 3, 1, 1, numOptArg, numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0
  if (PipGetInOutFile('FillTomogram', 2, 'Name of tomogram file to fill', inFile)  &
      .ne. 0) call exitError('NO TOMOGRAM FILE TO FILL SPECIFIED')
  call imopen(1, inFile, 'old')
  call irdhdr(1, nxyzOut, mxyz, mode, dminOut, dmaxOut, dmeanOut)

  if (PipGetInOutFile('MatchedToTomogram', 2, 'Name of tomogram that was matched TO', &
      inFile) .ne. 0) call exitError('NO MATCHED-TO TOMOGRAM FILE SPECIFIED')
  call imopen(2, inFile, 'ro')
  call irdhdr(2, nxyz, mxyz, mode, dmin, dmax, dmean)
  if (nx .ne. nxOut .or. ny .ne. nyOut .or. nz .ne. nzOut) call exitError( &
      'DIMENSIONS DO NOT MATCH BETWEEN MATCHED-TO FILE AND FILE BEING FILLED')
  idim = nx * ny
  allocate(array(idim), brray(max(idim, 2 * maxSample)), stat = i)
  call memoryError(i, 'ARRAYS FOR IMAGE')
  !
  if (.not. pipinput) print *,'Enter either the X, Y and Z dimensions of the tomogram ' &
      , 'that was', 'transformed to match, or the name of that file'
  if (pipinput) then
    if (PipGetString('SourceTomogram', inFile).ne. 0)  &
        call exitError('NO SOURCE TOMOGRAM FILE ENTERED')
    call ialprt(.false.)
    call imopen(3, inFile, 'ro')
    call irdhdr(3, nxyzIn, mxyz, mode, dmin, dmax, dmean)
  else
    call get_nxyz(.false., ' ', 'FILLTOMO', 5, nxyzIn)
  endif
  !
  if (pipinput) then
    if (PipGetString('InverseTransformFile', inFile) .ne. 0)  &
        call exitError('NO INVERSE TRANSFORM FILE ENTERED')
  else
    print *,'Enter name of file containing inverse transformation used by MATCHVOL'
    read(5, '(a)') inFile
  endif
  call dopen(1, inFile, 'ro', 'f')
  read(1,*) ((minv(i, j), j = 1, 3), cxyzIn(i), i = 1, 3)
  close(1)
  do i = 1, 3
    cxyzIn(i) = cxyzIn(i) + nxyzIn(i) / 2.
    cxyzOut(i) = nxyzOut(i) / 2.
  enddo
  !
  nLeftFill = 0
  nRightFill = 0
  nBotFill = 0
  nTopFill = 0
  ibinning = 1
  if (pipinput) then
    ierr = PipGetTwoIntegers('LeftRightFill', nLeftFill, nRightFill)
    ierr = PipGetTwoIntegers('BottomTopFill', nBotFill, nTopFill)
    ierr = PipGetInteger('ImagesAreBinned', ibinning)
  else
    write(*,'(1x,a,/,a,$)') '#s of pixels on left, right, bottom, and top ', &
      '(Y in flipped tomogram) to fill regardless: '
    read(5,*) nLeftFill, nRightFill, nBotFill, nTopFill
  endif
  !
  ! Make arrays for the limits in source and matched volumes
  allocate(ixSrcStart(nzIn), ixSrcEnd(nzIn), ixMatStart(nz), ixMatEnd(nz), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR BOUNDARIES IN X')
  call getBoundaryLimits(pipinput, ibinning, 'Source', nxyzIn, 3, ixSrcStart, ixSrcEnd, &
      ifSrcLimits)
  call getBoundaryLimits(pipinput, ibinning, 'MatchedTo', nxyzOut, 2, ixMatStart, &
      ixMatEnd, ifMatLimits)
  call PipDone()
  !
  ! sample each volume to find mean and SD
  do iunit = 1, 2
    idelSamp = ((float(nx * ny) * nz) / (8. *maxSample))**.3333 + 1.
    numSampX = (nx / 2) / idelSamp + 1
    ixStart = nx / 2 - 0.5 * numSampX * idelSamp
    ixStart = max(0, ixStart)
    numSampY = (ny / 2) / idelSamp + 1
    iyStart = ny / 2 - 0.5 * numSampY * idelSamp
    iyStart = max(0, iyStart)
    numSampZ = (nz / 2) / idelSamp + 1
    izStart = nz / 2 - 0.5 * numSampZ * idelSamp
    izStart = max(0, izStart)
    ndat = 0
    do jz = 1, numSampZ
      iz = izStart + (jz - 1) * idelSamp
      do jy = 1, numSampY
        iy = iyStart + (jy - 1) * idelSamp
        call imposn(iunit, iz, iy)
        call irdlin(iunit, array,*99)
        do jx = 1, numSampX
          ix = 1 + ixStart + (jx - 1) * idelSamp
          ndat = ndat + 1
          brray(ndat) = array(ix)
        enddo
      enddo
    enddo
    call avgsd(brray, ndat, avg(iunit), sd(iunit), sem)
    write(6, 103) iunit, avg(iunit), sd(iunit)
103 format(' Volume',i2,': mean =',f12.4,',  SD =',f12.4)
  enddo
  !
  ! scale second volume to match first
  !
  scale = sd(1) / sd(2)
  facAdd = avg(1) - avg(2) * scale
  !
  doMatTests = ifMatLimits > 0 .or. nLeftFill > 0 .or. nRightFill > 0 .or. &
      nBotFill > 0 .or. nTopFill > 0
  numThreads = numOMPthreads(limThreads)
  numSecRead = 0
  numPtFilled = 0
  ! wallTot = 0.
  do iz = 0, nz - 1
    zcen = iz - cenZout
    call imposn(1, iz, 0)
    call imposn(2, iz, 0)
    call irdsec(1, array, *99)
    call irdsec(2, brray, *99)
    numFillOnSec = 0
    ! wallStart = wallTime()
    
    !$OMP PARALLEL NUM_THREADS(numThreads) &
    !$OMP SHARED(nx, ny, nz, minv, cenXin, cenYin, cenZin, nxIn, nyIn, nzIn) &
    !$OMP SHARED(numFillOnSec, ifSrcLimits, ixSrcStart, ixSrcEnd, doMatTests) &
    !$OMP SHARED(ixMatEnd, nLeftFill, nRightFill, nBotFill, nTopFill, scale, array) &
    !$OMP SHARED(iz, zcen, brray, ixMatStart, cenZout, cenYout, cenXout, facAdd) &
    !$OMP SHARED(dminOut, dmaxOut)&
    !$OMP PRIVATE(iy, ycen, indBase, ix, xcen, xp, yp, zp, doFill, izSrc) &
    !$OMP PRIVATE(ind, val, numFillThread, dminThread, dmaxThread) &
    !$OMP DEFAULT(NONE)
    dminThread = 1.e37
    dmaxThread = -1.e37
    numFillThread = 0
    !$OMP DO
    do iy = 0, ny - 1
      ycen = iy - cenYout
      indBase = 1 + iy * nx
      do ix = 0, nx - 1
        xcen = ix - cenXout

        ! Get the coordinate in the source volume and fill if it is out of bounds
        xp = minv(1, 1) * xcen + minv(1, 2) * ycen + minv(1, 3) * zcen + cenXin
        yp = minv(2, 1) * xcen + minv(2, 2) * ycen + minv(2, 3) * zcen + cenYin
        zp = minv(3, 1) * xcen + minv(3, 2) * ycen + minv(3, 3) * zcen + cenZin
        doFill = xp < 0. .or. xp > nxIn - 1 .or. yp < 0. .or. yp > nyIn - 1 .or.  &
            zp < 0. .or. zp > nzIn - 1

        ! Then if NOT filling, check against the boundary limits in source volume
        if (ifSrcLimits > 0 .and. .not. doFill) then
          izSrc = zp + 1.
          doFill = xp < ixSrcStart(izSrc) .or. xp > ixSrcEnd(izSrc)
        endif

        ! Then if filling, only fill if inside the usable boundaries in matched volume
        ! But fill regardless outside the specified borders
        if (doMatTests) then
          doFill = (doFill .and. ix >= ixMatStart(iz + 1) .and.  &
              ix <= ixMatEnd(iz + 1)) .or. ix < nLeftFill .or. nx - ix <= nRightFill &
              .or. iz < nBotFill .or. nz - iz <= nTopFill
        endif
        if (doFill) then
          numFillThread = numFillThread + 1
          ind = indBase + ix 
          val = scale * brray(ind) + facAdd
          array(ind) = val
          dminThread = min(dminThread, val)
          dmaxThread = max(dmaxThread, val)
        endif
      enddo
    enddo
    !$OMP END DO
    !$OMP CRITICAL
    numFillOnSec = numFillOnSec + numFillThread
    dminOut = min(dminOut, dminThread)
    dmaxOut = max(dmaxOut, dmaxThread)
    !$OMP END CRITICAL
    
    !$OMP END PARALLEL

    ! wallTot = wallTot + wallTime() - wallStart
    if (numFillOnSec > 0) then
      call imposn(1, iz, 0)
      call iwrsec(1, array)
      numSecRead = numSecRead + 1
      numPtFilled = numPtFilled + numFillOnSec
    endif
  enddo
  ! write(*,'(a,f8.3)')'Parallel section time', wallTot
  call b3ddate(dat)
  call time(tim)
  write(titlech, 3000) dat, tim
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
  call iwrhdr(1, title, 1, dminOut, dmaxOut, dmeanOut)
  call imclose(1)
  write(*,'(i15,a,i7,a)') numPtFilled, ' points replaced on', numSecRead, ' sections'
  call exit(0)
3000 format ( 'FILLTOMO: Replacing parts of dual-axis tomogram',t57, a9,2x, a8)
99 call exitError('READING FILE')
end program filltomo



subroutine getBoundaryLimits(pipinput, ibinning, optPrefix, nxyz, imUnit, ixStart, &
    ixEnd, icontUse)
  implicit none
  logical pipinput
  character*(*) optPrefix
  character*320 inFile
  integer*4 nxyz(3), ixStart(*), ixEnd(*), imUnit, ibinning
  integer LIMVERT, LIMCONT
  parameter (LIMVERT=100000, LIMCONT=1000)
  real*4 xvert(LIMVERT), yvert(LIMVERT), zcont(LIMCONT)
  integer*4 indVert(LIMCONT), numVert(LIMCONT)
  real*4, allocatable :: flist(:,:,:)
  logical inside
  integer*4 idiff, mindiff, ifModel, ifRaw, ifXform, numCont, icont, i, istart, icontUse
  integer*4 numApply, ix, iz, numFlist, nxRaw, nyRaw, nzRaw, ifFlip, ierr, indv
  real*4 xll, yll, xcen, ycen, xtrans, ytrans
  integer*4 PipGetString, PipGetThreeIntegers

  icontUse = 0
  if (pipinput) then
    ifModel = 1 - PipGetString(trim(optPrefix)//'BoundaryModel', inFile)
    ifRaw = 1 - PipGetThreeIntegers(trim(optPrefix)//'RawStackSize', nxRaw, nyRaw, nzRaw)
    ifXform = 1 - PipGetString(trim(optPrefix)//'StackTransforms', inFile)
    if (ifXform > 0 .and. ifRaw == 0) call exitError( &
        'A RAW STACK SIZE MUST BE ENTERED TO USE A STACK TRANSFORM FILE')
    if (ifModel + ifXform > 1) call exitError('YOU CANNOT ENTER BOTH A BOUNDARY MODEL'// &
        ' AND RAW STACK TRANSFORMS FOR THE SAME TOMOGRAM')

    ! Process model file if one given
    if (ifModel > 0) then
      call get_region_contours(inFile, 'FILLTOMO', xvert, yvert, numVert, indVert, &
          zcont,  numCont, ifFlip, LIMCONT, LIMVERT, imUnit)
      if (numCont == 0)  &
          call exitError('THE '//trim(optPrefix)//' BOUNDARY MODEL HAS NO CONTOURS')
      
      ! Find contour closest to middle
      mindiff = 100000000
      do icont = 1, numCont
        idiff = abs(zcont(icont) - nxyz(2) / 2)
        if (idiff < mindiff) then
          minDiff = idiff
          icontUse = icont
        endif
      enddo
    endif

    ! Or get the transforms, and apply it to raw size to make a contour
    if (ifXform > 0) then
      allocate(flist(2, 3, nzRaw), stat=icont)
      call memoryError(icont, 'ARRAY FOR STACK TRANSFORMS')
      call dopen(12, inFile, 'ro', 'f')
      call xfrdall2(12, flist, numFlist, nzRaw, icont)
      if (icont > 1) call exitError('READING '//trim(optPrefix)//' STACK TRANSFORM FILE')

      ! Adjust sizes for binning, zero out transform shifts
      nxRaw = nxRaw / ibinning
      nyRaw = nyRaw / ibinning
      nzRaw = nzRaw / ibinning
      flist(1:2, 3, 1:numFlist) = 0.

      ! Average over the assumed middle of the tilt series
      numApply = max(1, min(10, numFlist / 6))
      xvert(1:4) = 0.
      yvert(1:4) = 0.
      indVert(1) = 1
      icontUse = 1
      numVert(1) = 4
      xll = (nxyz(1) - nxRaw) / 2.
      yll = (nxyz(3) - nyRaw) / 2.
      xcen = nxyz(1) / 2.
      ycen = nxyz(3) / 2.
      
      ! Apply to each corner
      istart = max(1, (numFlist - numApply) / 2)
      do icont = istart, istart + numApply - 1
        i = max(1, min(numFlist, icont))
        call xfapply(flist(1, 1, i), xcen, ycen, xll, yll, xtrans, ytrans)
        xvert(1) = xvert(1) + xtrans / numApply
        yvert(1) = yvert(1) + ytrans / numApply
        call xfapply(flist(1, 1, i), xcen, ycen, xll + nxRaw, yll, xtrans, ytrans)
        xvert(2) = xvert(2) + xtrans / numApply
        yvert(2) = yvert(2) + ytrans / numApply
        call xfapply(flist(1, 1, i), xcen, ycen, xll + nxRaw, yll + nyRaw, xtrans, ytrans)
        xvert(3) = xvert(3) + xtrans / numApply
        yvert(3) = yvert(3) + ytrans / numApply
        call xfapply(flist(1, 1, i), xcen, ycen, xll, yll + nyRaw, xtrans, ytrans)
        xvert(4) = xvert(4) + xtrans / numApply
        yvert(4) = yvert(4) + ytrans / numApply
      enddo
      deallocate(flist, stat = ierr)
    endif
  endif

  if (icontUse == 0) then
    do iz = 1, nxyz(3)
      ixStart(iz) = 0
      ixEnd(iz) = nxyz(1) - 1
    enddo
  else
    indv = indVert(icontUse)
    do iz = 1, nxyz(3)

      ! Find first pixel inside the contour from the left
      do ix = 0, nxyz(1) - 1
        ixStart(iz) = nxyz(1) + 1
        if (inside(xvert(indv), yvert(indv), numVert(icontUse), ix + 0.5, iz - 0.5)) then
          ixStart(iz) = ix
          exit
        endif
      enddo

      ! If none was found, set other limit, otherwise find first pixel inside from right
      if (ixStart(iz) > nxyz(1)) then
        ixEnd(iz) = 0
      else
        do ix = nxyz(1) - 1, ixStart(iz), -1
          if (inside(xvert(indv), yvert(indv), numVert(icontUse), ix + 0.5, iz - 0.5)) &
              then
            ixEnd(iz) = ix
            exit
          endif
        enddo
      endif
    enddo
    ! write(*,'(5(2x,3i4))')(iz,ixstart(iz),ixend(iz),iz=1,nxyz(3))
  endif
  return
end subroutine getBoundaryLimits

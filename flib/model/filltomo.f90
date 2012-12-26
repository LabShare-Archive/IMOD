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
  integer maxSample
  parameter (maxSample = 100000)
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
  real*4 avg(2), sd(2)
  character dat*9, tim*8
  character*80 titlech
  integer*4 i, nLeftFill, nRightFill, nBotFill, nTopFill, iunit, idelSamp, numSampX
  integer*4 ixStart, numSampY, iyStart, j, iz
  integer*4 numSampZ, izStart, ndat, jz, jy, iy, jx, ix, numSecRead, ifReadIn, ind, kti
  real*4 sem, scale, facAdd, zcen, ycen, xcen, xp, yp, zp, val, dminOut, dmaxOut, dmeanOut
  real*4 dmin, dmax, dmean
  real*8 numPtFilled
  !
  call setExitPrefix('ERROR: FILLTOMO - ')
  write(*,'(1x,a,$)') 'Name of combined tomogram file: '
  read(5, '(a)') inFile
  call imopen(1, inFile, 'old')
  call irdhdr(1, nxyzOut, mxyz, mode, dminOut, dmaxOut, dmeanOut)
  write(*,'(1x,a,$)') 'Name of tomogram that was matched TO: '
  read(5, '(a)') inFile
  call imopen(2, inFile, 'ro')
  call irdhdr(2, nxyz, mxyz, mode, dmin, dmax, dmean)
  if (nx .ne. nxOut .or. ny .ne. nyOut .or. nz .ne. nzOut) &
      call exitError ('File sizes do not match')
  idim = nx * ny
  allocate(array(idim), brray(max(idim, 2 * maxSample)), stat = i)
  call memoryError(i, 'ARRAYS FOR IMAGE')
  !
  print *,'Enter either the X, Y and Z dimensions of the tomogram ' &
      , 'that was', 'transformed to match, or the name of that file'
  call get_nxyz(.false., ' ', 'FILLTOMO', 5, nxyzIn)
  !
  print *,'Enter name of file containing inverse transformation ', &
      'used by MATCHVOL'
  read(5, '(a)') inFile
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
  write(*,'(1x,a,/,a,$)') '#s of pixels on left, right, bottom,'// &
      ' and top ', &
      '(Y in flipped tomogram) to fill regardless: '
  read(5,*) nLeftFill, nRightFill, nBotFill, nTopFill
  !
  ! sample each volume to find mean and SD
  !
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
  numSecRead = 0
  numPtFilled = 0
  do iz = 0, nz - 1
    ifReadIn = 0
    zcen = iz - cenZout
    do iy = 0, ny - 1
      ycen = iy - cenYout
      do ix = 0, nx - 1
        xcen = ix - cenXout
        xp = minv(1, 1) * xcen + minv(1, 2) * ycen + minv(1, 3) * zcen + cenXin
        yp = minv(2, 1) * xcen + minv(2, 2) * ycen + minv(2, 3) * zcen + cenYin
        zp = minv(3, 1) * xcen + minv(3, 2) * ycen + minv(3, 3) * zcen + cenZin
        if (xp < 0. .or. xp > nxIn - 1 .or. yp < 0. .or. yp > nyIn - 1 &
            .or. zp < 0. .or. zp > nzIn - 1 &
            .or. ix < nLeftFill .or. nx - ix <= nRightFill &
            .or. iz < nBotFill .or. nz - iz <= nTopFill) then
          if (ifReadIn == 0) then
            call imposn(1, iz, 0)
            call irdsec(1, array,*99)
            call imposn(2, iz, 0)
            call irdsec(2, brray,*99)
            ifReadIn = 1
            numSecRead = numSecRead + 1
          endif
          ind = 1 + ix + iy * nx
          val = scale * brray(ind) + facAdd
          array(ind) = val
          dminOut = min(dminOut, val)
          dmaxOut = max(dmaxOut, val)
          numPtFilled = numPtFilled + 1
        endif
      enddo
    enddo
    if (ifReadIn .ne. 0) then
      call imposn(1, iz, 0)
      call iwrsec(1, array)
    endif
  enddo
  call b3ddate(dat)
  call time(tim)
  write(titlech, 3000) dat, tim
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
  call iwrhdr(1, title, 1, dminOut, dmaxOut, dmeanOut)
  call imclose(1)
  write(*,'(f15.0,a,i7,a)') numPtFilled, ' points replaced on', numSecRead, ' sections'
  call exit(0)
3000 format ( 'FILLTOMO: Replacing parts of combined tomogram',t57, a9,2x, a8)
99 call exitError('READING FILE')
end program filltomo

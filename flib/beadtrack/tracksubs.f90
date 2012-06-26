module cgPixels
  implicit none
  integer*4, allocatable :: idxIn(:), idyin(:), idxEdge(:), idyEdge(:)
  real*4, allocatable :: edgePixels(:)
  integer*4 numInside, numEdge, iPolarity
  logical*4 edgeMedian
end module cgPixels

!
!
! nextPos computes the projected position of a point from positions on
! nearby views in the absence of a tilt alignment
! IOBJ is the object #
! ipNear is the point number of the nearest point in Z
! IDIR is the direction (+1 / -1)
! izNext is the Z value of the view to project to
! TILT is array of tilt angles
! numFit and minFit are maximum and minimum number of points to fit
! iaxTilt is 1 if tilt axis is near 0 degrees
! tiltMin is minimum tilt angle for fitting to sin / cosine
! XNEXT, YNEXT is the projected position
!
subroutine nextPos(iobj, ipNear, idir, izNext, tilt, numFit, minFit, &
    iaxTilt, tiltMin, xnext, ynext)
  implicit none
  include 'smallmodel.inc90'
  integer idim
  parameter (idim = 50)
  real*4 tilt(*)
  real*4 xx(idim), yy(idim), zz(idim), b1(2)
  integer*4 iobj, ipNear, idir, izNext, numFit, minFit, iaxTilt
  real*4 tiltMin, xnext, ynext
  integer*4 ibase, numInObj, mfit, ipEnd, indx, indy, iptNear, ip, ipt, ipb, ipp
  integer*4 iPast, iBefore, i
  real*4 xsum, ysum, slope, bint, ro, const, rsq, fra, thetaNext, xtmp, theta
  real*4 cosd, sind

  ibase = ibase_obj(iobj)
  numInObj = npt_in_obj(iobj)
  mfit = 0
  ipEnd = 1
  if (idir == - 1) ipEnd = numInObj
  if (iaxTilt <= 1) then
    indx = 1
    indy = 2
  else
    indx = 2
    indy = 1
  endif
  !
  ! set pointer past the view if possible
  !
  iptNear = object(ibase + ipNear)
  ip = ipNear
  do while(idir * (nint(p_coord(3, object(ibase + ip))) - izNext) >= 0 &
      .and. idir * (ip - ipEnd) > 0)
    ip = ip - idir
  enddo
  if (idir * (nint(p_coord(3, object(ibase + ip))) - izNext) == - 1) then
    !
    ! if point is adjacent in that direction, then starting from there,
    ! load numFit points
    !
    do while(idir * (ip - ipEnd) >= 0 .and. mfit < numFit)
      ipt = object(ibase + ip)
      mfit = mfit + 1
      if (mfit > idim) call errorExit( &
          'TOO MANY POINTS IN NON-TILT ALIGNMENT FITS FOR ARRAYS', 0)
      xx(mfit) = p_coord(indx, ipt)
      yy(mfit) = p_coord(indy, ipt)
      zz(mfit) = p_coord(3, ipt)
      ip = ip - idir
    enddo
  elseif (nint(p_coord(3, iptNear)) == izNext) then
    !
    ! otherwise, if there is already a point on the section, just take it
    !
    mfit = 1
    xx(mfit) = p_coord(indx, iptNear)
    yy(mfit) = p_coord(indy, iptNear)
    zz(mfit) = izNext
  else
    !
    ! otherwise, set pointers to points past and before view and get
    ! points from both directions
    !
    iPast = ipNear
    if (p_coord(3, object(ibase + ipNear)) < izNext) iPast = ipNear + 1
    iBefore = iPast - 1
    !
    ! starting from there, load numFit nearest points
    !
    do while((iBefore > 0 .or. iPast <= numInObj) .and. mfit < numFit)
      if (iBefore > 0) ipb = object(ibase + iBefore)
      if (iPast <= numInObj) ipp = object(ibase + iPast)
      !
      ! take the only one that's legal
      !
      if (iBefore <= 0) then
        ipt = ipp
        iPast = iPast + 1
      elseif (iPast > numInObj) then
        ipt = ipb
        iBefore = iBefore - 1
      else
        !
        ! or if both are legal, take the one closest to target view
        !
        if (abs(p_coord(3, ipb) - izNext) < &
            abs(p_coord(3, ipp) - izNext)) then
          ipt = ipb
          iBefore = iBefore - 1
        else
          ipt = ipp
          iPast = iPast + 1
        endif
      endif
      mfit = mfit + 1
      if (mfit > idim) call errorExit( &
          'TOO MANY POINTS IN NON-TILT ALIGNMENT FITS FOR ARRAYS', 0)
      xx(mfit) = p_coord(indx, ipt)
      yy(mfit) = p_coord(indy, ipt)
      zz(mfit) = p_coord(3, ipt)
    enddo
  endif
  !
  if (mfit < minFit) then
    xsum = 0.
    ysum = 0.
    do i = 1, mfit
      xsum = xsum + xx(i)
      ysum = ysum + yy(i)
    enddo
    xnext = xsum / mfit
    ynext = ysum / mfit
    ! print *,'average:', iobj, mfit, xnext, ynext
  else
    call lsfit(zz, xx, mfit, slope, bint, ro)
    xnext = izNext * slope + bint
    if (iaxTilt == 0 .or. abs(tilt(izNext + 1)) < tiltMin) then
      call lsfit(zz, yy, mfit, slope, bint, ro)
      ynext = izNext * slope + bint
      ! print *,'linear fit:', iobj, mfit, xnext, ynext
    else
      do i = 1, mfit
        theta = tilt(nint(zz(i)) + 1)
        xx(i) = cosd(theta)
        zz(i) = sind(theta)
      enddo
      call lsfit2(xx, zz, yy, mfit, b1(1), b1(2), const)
      thetaNext = tilt(izNext + 1)
      ynext = b1(1) * cosd(thetaNext) + b1(2) * sind(thetaNext) + const
      ! print *,'sin/cos fit:', iobj, mfit, xnext, ynext
    endif
  endif
  if (iaxTilt > 1) then
    xtmp = xnext
    xnext = ynext
    ynext = xtmp
  endif
  return
end subroutine nextPos





! find_PIECE takes a list of nPclist piece coordinates in
! I[XYZ]PCLIST, the piece dimensions nx and NY, and index coordinates
! in the montaged image IND[XYZ], and finds the piece that those
! coordinates are in (IPCZ) and the coordinates IPCX, IPCY of the point
! in that piece
!
subroutine find_piece(ixPclist, iyPclist, izPclist, nPclist, nx, &
    ny, nxBox, nyBox, xnext, ynext, izNext, ix0, ix1, iy0, iy1, ipcz)
  !
  integer*4 ixPclist(*), iyPclist(*), izPclist(*)
  !
  indx0 = nint(xnext) - nxBox / 2
  indx1 = indx0 + nxBox - 1
  indy0 = nint(ynext) - nyBox / 2
  indy1 = indy0 + nyBox - 1
  ipcz = -1
  do ipc = 1, nPclist
    if (izNext == izPclist(ipc) .and. &
        indx0 >= ixPclist(ipc) .and. indx1 < ixPclist(ipc) + nx .and. &
        indy0 >= iyPclist(ipc) .and. indy1 < iyPclist(ipc) + ny) then
      ipcz = ipc - 1
      ix0 = indx0 - ixPclist(ipc)
      ix1 = indx1 - ixPclist(ipc)
      iy0 = indy0 - iyPclist(ipc)
      iy1 = indy1 - iyPclist(ipc)
      return
    endif
  enddo
  return
end subroutine find_piece




SUBROUTINE QDshift(array, bray, nx, NY, xt, yt)
  dimension array(nx, ny), bray(nx, ny)
  !
  ! Loop over output image
  !
  dx = -xt
  dy = -yt
  dxsq = dx**2
  dysq = dy**2
  DO IYP = 1, NY
    DO ixp = 1, nx
      !
      ! Do quadratic interpolation
      !
      ixpp1 = ixp + 1
      ixpm1 = ixp - 1
      iypp1 = IYP + 1
      iypm1 = IYP - 1
      IF (ixpm1 < 1) ixpm1 = 1
      IF (iypm1 < 1) iypm1 = 1
      IF (ixpp1 > nx) ixpp1 = nx
      IF (iypp1 > NY) iypp1 = NY
      !
      ! Set up terms for quadratic interpolation
      !
      v2 = array(ixp, iypm1)
      v4 = array(ixpm1, IYP)
      v5 = array(ixp, IYP)
      v6 = array(ixpp1, IYP)
      v8 = array(ixp, iypp1)
      !
      a = (v6 + v4) * .5 - v5
      b = (v8 + v2) * .5 - v5
      c = (v6 - v4) * .5
      d = (v8 - v2) * .5
      !

      bray(ixp, iyp) = (a * DXsq + b * DYsq + c * DX + d * DY + v5)
      !
    enddo
  enddo
  !
  RETURN
END SUBROUTINE QDshift



subroutine setMeanZero(array, nxDim, nyDim, nx, ny)
  real*4 array(nxDim,nyDim)
  sum = 0.
  do iy = 1, ny
    sumt = 0.
    do ix = 1, nx
      sumt = sumt + array(ix, iy)
    enddo
    sum = sum + sumt
  enddo
  avg = sum / (nx * ny)
  do iy = 1, ny
    do ix = 1, nx
      array(ix, iy) = array(ix, iy) - avg
    enddo
  enddo
  return
end subroutine setMeanZero


! peakFind finds the coordinates of the the absolute peak, XPEAK, YPEAK
! in the array array, which is dimensioned to nx + 2 by ny.
!
subroutine peakFind(array, nxPlus, nyRot, xpeak, ypeak, peak)
  real*4 array(nxPlus,nyRot)
  nxRot = nxPlus - 2
  !
  ! find peak
  !
  peak = -1.e30
  do iy = 1, nyRot
    do ix = 1, nxRot
      if (array(ix, iy) > peak) then
        peak = array(ix, iy)
        ixPeak = ix
        iyPeak = iy
      endif
    enddo
  enddo
  ! print *,ixPeak, iyPeak
  !
  ! return adjusted pixel coordinate minus 1
  !
  xpeak = ixPeak - 1.
  ypeak = iyPeak - 1.
  if (xpeak > nxRot / 2) xpeak = xpeak - nxRot
  if (ypeak > nyRot / 2) ypeak = ypeak - nyRot
  ! print *,xpeak, ypeak
  return
end subroutine peakFind


subroutine edgeForCG(boxTmp, nxBox, nyBox, ixcen, iycen, edge, ierr)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox)
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, nsum, i, ierr
  real*4 sum, edge
  sum = 0.
  nsum = 0
  edge = 0.
  ierr = 1
  !
  ! find edge mean - require half the points to be present
  !
  if (edgeMedian) then
    do i = 1, numEdge
      ix = ixcen + idxEdge(i)
      iy = iycen + idyEdge(i)
      if (ix >= 1 .and. ix <= nxBox .and. iy >= 1 .and. iy <= nyBox) then
        nsum = nsum + 1
        edgePixels(nsum) = boxTmp(ix, iy)
      endif
    enddo
  else
    do i = 1, numEdge
      ix = ixcen + idxEdge(i)
      iy = iycen + idyEdge(i)
      if (ix >= 1 .and. ix <= nxBox .and. iy >= 1 .and. iy <= nyBox) then
        sum = sum + boxTmp(ix, iy)
        nsum = nsum + 1
      endif
    enddo
  endif
  if (nsum < numEdge / 2) return
  ierr = 0
  if (edgeMedian) then
    call rsSortFloats(edgePixels, nsum)
    call rsMedianOfSorted(edgePixels, nsum, edge)
  else
    edge = sum / nsum
  endif
  return
end subroutine edgeForCG


subroutine calcCG(boxTmp, nxBox, nyBox, xpeak, ypeak, wsum)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox), xpeak, ypeak, wsum
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, ixBest, iyBest, nsum, i
  real*4 best, sum4, sum, xsum, ysum, weight, edge
  !
  ixcen = nxBox / 2 + nint(xpeak)
  iycen = nyBox / 2 + nint(ypeak)
  !
  ! look around, find most extreme 4 points as center
  !
  if (ixcen >= 2 .and. ixcen <= nxBox - 2 .and. &
      iycen >= 2 .and. iycen <= nyBox - 2) then
    best = 0.
    do iy = iycen - 1, iycen + 1
      do ix = ixcen - 1, ixcen + 1
        sum4 = (boxTmp(ix, iy) + boxTmp(ix + 1, iy) + &
            boxTmp(ix, iy + 1) + boxTmp(ix + 1, iy + 1))
        if (iPolarity * sum4 > iPolarity * best .or. best == 0.) then
          ixBest = ix
          iyBest = iy
          best = sum4
        endif
      enddo
    enddo
    ixcen = ixBest
    iycen = iyBest
  endif
  xsum = 0.
  ysum = 0.
  wsum = 0.
  !
  ! find edge mean - require half the points to be present
  call edgeForCG(boxTmp, nxBox, nyBox, ixcen, iycen, edge, i)
  if (i .ne. 0) return
  !
  ! subtract edge and get weighted sum of pixel coordinates for POSITIVE
  ! pixels
  !
  do i = 1, numInside
    ix = ixcen + idxIn(i)
    iy = iycen + idyin(i)
    if (ix >= 1 .and. ix <= nxBox .and. iy >= 1 .and. iy <= nyBox) then
      weight = boxTmp(ix, iy) - edge
      if (iPolarity * weight > 0.) then
        xsum = xsum + ix * weight
        ysum = ysum + iy * weight
        wsum = wsum + weight
      endif
    endif
  enddo
  if (wsum == 0.) return
  xpeak = xsum / wsum - 0.5 - nxBox / 2
  ypeak = ysum / wsum - 0.5 - nyBox / 2
  wsum = wsum * iPolarity
  return
end subroutine calcCG

subroutine wsumForSobelPeak(boxTmp, nxBox, nyBox, xpeak, ypeak, wsum)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox), xpeak, ypeak, wsum
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, i
  real*4 weight, edge
  !
  ixcen = nxBox / 2 + nint(xpeak)
  iycen = nyBox / 2 + nint(ypeak)
  wsum = 0.
  !
  ! find edge mean - require half the points to be present
  call edgeForCG(boxTmp, nxBox, nyBox, ixcen, iycen, edge, i)
  if (i .ne. 0) return
  !
  ! subtract edge and get weight sum for POSITIVE pixels
  !
  do i = 1, numInside
    ix = ixcen + idxIn(i)
    iy = iycen + idyin(i)
    if (ix >= 1 .and. ix <= nxBox .and. iy >= 1 .and. iy <= nyBox) then
      weight = boxTmp(ix, iy) - edge
      if (iPolarity * weight > 0.) wsum = wsum + weight
    endif
  enddo
  wsum = wsum * iPolarity
  return
end subroutine wsumForSobelPeak



subroutine rescue(boxTmp, nxBox, nyBox, xpeak, ypeak, radMax, relaxCrit, wsum)
  implicit none
  real*4 boxTmp(*), xpeak, ypeak, radMax, relaxCrit, wsum
  integer*4 nxBox, nyBox, idx, idy
  real*4 rad, radInc, radSq, radOutSq, wBest, distSq, xtmp, ytmp, wtmp
  wsum = 0.
  rad = 0.
  radInc = 1.5
  do while(rad <= radMax .and. wsum == 0.)
    radSq = rad**2
    radOutSq = min(rad + radInc, radMax)**2
    wBest = 0.
    do idy = -nyBox / 2, nyBox / 2
      do idx = -nxBox / 2, nxBox / 2
        distSq = idx**2 + idy**2
        if (distSq > radSq .and. distSq <= radOutSq) then
          xtmp = idx
          ytmp = idy
          call calcCG(boxTmp, nxBox, nyBox, xtmp, ytmp, wtmp)
          if (wtmp > wBest) then
            wBest = wtmp
            xpeak = xtmp
            ypeak = ytmp
          endif
        endif
      enddo
    enddo
    rad = rad + radInc
    if (wBest >= relaxCrit) wsum = wBest
  enddo
  return
end subroutine rescue

subroutine rescueFromSobel(boxTmp, nxBox, nyBox, xpeak, ypeak, sobelXpeaks, &
    sobelYpeaks, sobelPeaks, sobelWsums, maxPeaks, radMax, relaxCrit, wsum)
  implicit none
  real*4 boxTmp(*), xpeak, ypeak, sobelXpeaks(*), sobelYpeaks(*), sobelPeaks(*)
  real*4 sobelWsums(*), radMax, relaxCrit, wsum
  integer*4 nxBox, nyBox, maxPeaks, ipeak
  real*4 rad, radInc, radSq, radOutSq, wBest, distSq
  wsum = 0.
  rad = 0.
  radInc = 1.5
  do while(rad <= radMax .and. wsum == 0.)
    radSq = rad**2
    radOutSq = min(rad + radInc, radMax)**2
    wBest = 0.
    do ipeak = 1, maxPeaks
      if (sobelPeaks(ipeak) < - 1.e29) exit
      distSq = sobelXpeaks(ipeak)**2 + sobelYpeaks(ipeak)**2
      call checkSobelPeak(ipeak, boxTmp, nxBox, nyBox, sobelXpeaks, sobelYpeaks, &
          sobelPeaks, sobelWsums, maxPeaks)
      if (distSq > radSq .and. distSq <= radOutSq .and. sobelWsums(ipeak) > wBest) then
        wBest = sobelWsums(ipeak)
        xpeak = sobelXpeaks(ipeak)
        ypeak = sobelYpeaks(ipeak)
      endif
    enddo
    rad = rad + radInc
    if (wBest >= relaxCrit) wsum = wBest
  enddo
  return
end subroutine rescueFromSobel

subroutine checkSobelPeak(indPeak, boxTmp, nxBox, nyBox, xpeaks, yPeaks, peaks, wsums, &
    maxPeaks)
  implicit none
  real*4 boxTmp(*), xpeaks(*), yPeaks(*), peaks(*), wsums(*)
  integer*4 nxBox, nyBox, maxPeaks, indPeak
  real*4 xtmp, ytmp
  if (wsums(indPeak) >= 0.) return
  if (peaks(indPeak) < - 1.e29) then
    wsums(indPeak) = 0.
    return
  endif
  call wsumForSobelPeak(boxTmp, nxBox, nyBox, xpeaks(indPeak), yPeaks(indPeak), &
      wsums(indPeak))
  return
end subroutine checkSobelPeak



subroutine add_point(iobj, ipNear, xpos, ypos, izNext)
  include 'smallmodel.inc90'
  logical failed
  !
  call object_mover(iobj, failed)
  if (failed) call errorExit('insufficient object space', 0)
  ibase = ibase_obj(iobj)
  n_point = n_point + 1
  p_coord(1, n_point) = xpos
  p_coord(2, n_point) = ypos
  p_coord(3, n_point) = izNext
  pt_label(n_point) = 0
  npt_in_obj(iobj) = npt_in_obj(iobj) + 1
  ipAdd = ipNear
  if (ipAdd == 0) then
    ipAdd = npt_in_obj(iobj)
    if (ipAdd == 1) then
      ibase_obj(iobj) = ibase_free
      ibase = ibase_free
    endif
  elseif (nint(p_coord(3, object(ibase + ipAdd))) < izNext) then
    ipAdd = ipAdd + 1
  endif
  do j = ibase + npt_in_obj(iobj), ibase + ipAdd + 1, -1
    object(j) = object(j - 1)
  enddo
  object(ibase + ipAdd) = n_point
  ibase_free = ibase_free + 1
  if (ipNear .ne. 0) ipNear = ipAdd
  return
end subroutine add_point


! readRawScale takes information about the desired image box ix0, ix1,
! iy0, iy1 from image file of dimensions nx, ny, and a size nxRaw, nyRaw
! of a larger box to be used for local scaling, and the target mean
! and sd in targetAvg and targetSD.  It reads data into the array rawTmp,
! scales them, and places the desired box of data into boxTmp

subroutine readRawScale(rawTmp, boxTmp, nx, ny, nxRaw, nyRaw, &
    nxBox, nyBox, ix0, ix1, iy0, iy1, targetAvg, targetSD)
  real*4 rawTmp(*), boxTmp(*)
  !
  ixr0 = max(0, ix0 - (nxRaw - nxBox) / 2)
  ixr1 = min(nx - 1, ixr0 + nxRaw - 1)
  ixr0 = max(0, ixr1 + 1 - nxRaw)
  nxRead = ixr1 + 1 - ixr0
  iyr0 = max(0, iy0 - (nyRaw - nyBox) / 2)
  iyr1 = min(ny - 1, iyr0 + nyRaw - 1)
  iyr0 = max(0, iyr1 + 1 - nyRaw)
  nyRead = iyr1 + 1 - iyr0
  call irdpas(1, rawTmp, nxRead, nyRead, ixr0, ixr1, iyr0, iyr1,*99)
  call avgsd(rawTmp, nxRead * nyRead, avg, sd, SEM)
  call irepak(boxTmp, rawTmp, nxRead, nyRead, ix0 - ixr0, ix1 - ixr0, &
      iy0 - iyr0, iy1 - iyr0)
  !
  ! Ad hoc protection against zero, should be improved
  !
  if (sd < 0.0001) sd = 0.0001
  dscal = targetSD / sd
  dadd = targetAvg - avg * dscal
  do i = 1, nxBox * nyBox
    boxTmp(i) = boxTmp(i) * dscal + dadd
  enddo
  return
99 print *,'ERROR READING IMAGE FROM FILE'
  call exit(1)
end subroutine readRawScale

! SETSIZ_SAM_CEL modifies the header size, sampling and cell size
! for the MRC file open on unit IUNIT, given dimensions nx, NY, NZ.
! It preserves the pixel size in Unit 1
!
subroutine setsiz_sam_cel(iunit, nx, ny, nz)
  integer*4 nxyz(3), nxyzst(3)
  real*4 cell(6), delta(3)
  data cell/0., 0., 0., 90., 90., 90./
  data nxyzst/0, 0, 0/
  !
  call irtdel(1, delta)
  nxyz(1) = nx
  nxyz(2) = ny
  nxyz(3) = nz
  cell(1) = nx * delta(1)
  cell(2) = ny * delta(2)
  cell(3) = nz * delta(3)
  call ialsiz(iunit, nxyz, nxyzst)
  call ialsam(iunit, nxyz)
  call ialcel(iunit, cell)
  return
end subroutine setsiz_sam_cel

! splitPack splits array into the 4 corners of brray
! array is dimensioned nxDim by NY, data are nx by NY and pack into brray
!
subroutine splitPack(array, nxDim, nx, ny, brray)
  implicit none
  integer*4 nx, nxDim, ny, ixnew, iyNew, ix, iy
  real*4 array(nxDim,ny), brray(nx,ny)
  do iy = 1, ny
    do ix = 1, nx
      ixnew = mod(ix + nx / 2 - 1, nx) + 1
      iyNew = mod(iy + ny / 2 - 1, ny) + 1
      brray(ixnew, iyNew) = array(ix, iy)
    enddo
  enddo
  return
end subroutine splitPack


! countMissing counts the number of points missing from object IOBJ
! nviewAll is the total number of views, numExlude excluded views are
! in izExclude, MISSING is a logical array to use, and the number
! or missing views and the list of views are returned in numListZ and
! LISTZ
!
subroutine countMissing(iobj, nviewAll, izExclude, numExlude, missing, &
    listz, numListZ)
  implicit none
  include 'smallmodel.inc90'
  logical missing(0: * )
  integer*4 izExclude(*), listz(*), iobj, nviewAll, numExlude, numListZ
  integer*4 numInObj, i, ibase, ip, iz
  !
  numInObj = npt_in_obj(iobj)
  numListZ = 0
  if (numInObj == 0) return
  ibase = ibase_obj(iobj)
  do i = 1, nviewAll
    missing(i) = .true.
  enddo
  do i = 1, numExlude
    missing(izExclude(i)) = .false.
  enddo
  do ip = 1, numInObj
    iz = nint(p_coord(3, object(ibase + ip))) + 1
    missing(iz) = .false.
  enddo
  numListZ = 0
  do i = 1, nviewAll
    if (missing(i)) then
      numListZ = numListZ + 1
      listz(numListZ) = i
    endif
  enddo
  return
end subroutine countMissing

logical function itemOnList(item, list, nlist)
  implicit none
  integer*4 list(*), item, nlist, i
  itemOnList = .true.
  do i = 1, nlist
    if (item == list(i)) return
  enddo
  itemOnList = .false.
  return
end function itemOnList

! FINDXF_WO_OUTLIERS calls FINDXF to get a 2d transformation,
! and eliminates outlying position - pairs from the solution
!
! XR is the data matrix, numData is the full amount of data
! XCEN, YCEN should contain the center coordinates that have been
! subtracted from the X and Y point data.
! ifTrans and ifRoTrans should be 0 to get a general linear transform
! ifTrans should be 1 to solve for translation only
! ifRoTrans should be 1 to solve for rotations and translations, or
! 2 to solve for magnification also
! maxDrop is the maximum number of points to drop as outliers
! critProb and critAbs are the criterion probabilities for considering
! a point an outlier
! If the maximum residual is below elimMin nothing will be eliminated
! numDrop is number of points dropped, point numbers returned in IDROP
! f is the 2 by 3 matrix transformation computed
! devAvg, devSD, devMax are mean, SD, and maximum deviations
! ipntMax give the point number at which the maximum occurred
!
subroutine findxf_wo_outliers(xr, msizeXR, numData, xcen, ycen, ifTrans, &
    ifRoTrans, maxDrop, critProb, critAbs, elimMin, idrop, numDrop, &
    f, devAvg, devSD, devMax, ipntMax)
  implicit none
  integer*4 idrop(*)
  real*4 xr(msizeXR,*)
  integer*4 ifTrans, ifRoTrans
  real*4 f(2,3), xcen, ycen
  integer*4 numData, maxDrop, numDrop, ipntMax, msizeXR
  real*4 critProb, critAbs, elimMin, devAvg, devSD, devMax
  integer*4 i, j, lastDrop, itmp, nkeep, jdrop, isaveBase, icolDev, indexCol
  real*4 probPerPt, absPerPt, sigmaFromAvg, sigmaFromSD, sigma, z, prob, xx, yy
  real*4 erfcc, gprob
  gprob(z) = 1. - 0.5 * erfcc(z / 1.414214)
  isaveBase = 13
  icolDev = 13
  indexCol = 19
  !
  ! get probability per single point from the overall criterion prob
  !
  probPerPt = (1. - critProb)**(1. / numData)
  absPerPt = (1. - critAbs)**(1. / numData)
  !
  ! copy the data into far columns
  !
  do i = 1, numData
    do j = 1, 5
      xr(j + isaveBase, i) = xr(j, i)
    enddo
  enddo

  call findxf(xr, msizeXR, 4, numData, xcen, ycen, ifTrans, ifRoTrans, 1, f, devAvg, &
      devSD, devMax, ipntMax)
  numDrop = 0
  if (maxDrop == 0 .or. devMax < elimMin) return
  !
  ! Sort the residuals and keep index back to initial values
  !
  lastDrop = 0
  do i = 1, numData
    idrop(i) = i
  enddo
  do i = 1, numData - 1
    do j = i + 1, numData
      if (xr(icolDev, idrop(i)) > xr(icolDev, idrop(j))) then
        itmp = idrop(i)
        idrop(i) = idrop(j)
        idrop(j) = itmp
      endif
    enddo
  enddo
  !
  ! load the data in this order, save index in xr
  !
  do i = 1, numData
    do j = 1, 5
      xr(j, i) = xr(j + isaveBase, idrop(i))
    enddo
    xr(indexCol, i) = idrop(i)
  enddo
  !
  ! Drop successively more points: get mean and S.d. of the remaining
  ! points and check how many of the points pass the criterion
  ! for outliers.
  !
  do jdrop = 1, maxDrop + 1
    call findxf(xr, msizeXR, 4, numData - jdrop, xcen, ycen, ifTrans, ifRoTrans, 1, f, devAvg, &
        devSD, devMax, ipntMax)
    !
    ! get deviations for points out of fit
    !
    do i = numData + 1 - jdrop, numData
      call xfApply(f, 0., 0., xr(1, i), xr(2, i), xx, yy)
      xr(icolDev, i) = sqrt((xx - xr(1, i))**2 + (yy - xr(2, i))**2)
    enddo
    !
    ! estimate the sigma for the error distribution as the maximum of
    ! the values implied by the mean and the SD of the deviations
    !
    sigmaFromAvg = devAvg / sqrt(8. / 3.14159)
    sigmaFromSD = devSD / sqrt(3. - 8. / 3.14159)
    sigma = max(sigmaFromAvg, sigmaFromSD, 1.e-10)

    nkeep = 0
    do j = numData - jdrop + 1, numData
      z = xr(icolDev, j) / sigma
      prob = 2 * (gprob(z) - 0.5) - sqrt(2. / 3.14159) * z * exp(-z**2 / 2.)
      if (prob < probPerPt) nkeep = nkeep + 1
      if (prob >= absPerPt) numDrop = min(maxDrop, max(numDrop, numData + 1 - j))
    enddo
    !
    ! If all points are outliers, this is a candidate for a set to drop
    ! When only the first point is kept, and all the rest of the points
    ! were outliers on the previous round, then this is a safe place to
    ! draw the line between good data and outliers.  In this case, set
    ! numDrop; and at end take the biggest numDrop that fits these criteria
    !
    if (nkeep == 0) lastDrop = jdrop
    if (nkeep == 1 .and. lastDrop == jdrop - 1 .and. lastDrop > 0) &
        numDrop = lastDrop
    ! print *,'drop', jdrop, ', keep', nkeep, ', lastdrop', lastDrop, &
    ! ',  ndrop =', numDrop
  enddo
  !
  ! when finish loop, need to redo with right amount of data and restore
  ! data
  !
  do i = 1, numDrop
    idrop(i) = nint(xr(indexCol, numData + i - numDrop))
  enddo
  call findxf(xr, msizeXR, 4, numData - numDrop, xcen, ycen, ifTrans, ifRoTrans, 1, f, devAvg, &
      devSD, devMax, ipntMax)
  ipntMax = nint(xr(indexCol, ipntMax))
  do i = 1, numData
    do j = 1, 5
      xr(j, i) = xr(j + isaveBase, i)
    enddo
  enddo
  return
end subroutine findxf_wo_outliers

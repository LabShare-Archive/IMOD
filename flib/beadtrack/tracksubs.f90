module cgPixels
  implicit none
  integer*4, allocatable :: idxIn(:), idyin(:), idxEdge(:), idyEdge(:)
  real*4, allocatable :: edgePixels(:), elongSmooth(:,:), outerPixels(:)
  logical, allocatable :: elongMask(:,:)
  integer*4, allocatable :: ixElong(:), iyElong(:), idxOuter(:), idyOuter(:)
  real*4 elongKernel(49), outerKernel(81)
  integer*4 numInside, numEdge, iPolarity, kernDimElong, numOuter, kernDimOuter
  logical*4 edgeMedian, getEdgeSD
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
! maxFit and minFit are maximum and minimum number of points to fit
! axisRot is tilt axis rotation angle
! tiltMin is minimum tilt angle for fitting to sin / cosine
! izExclude is list of numExclude view numbers to exclude (numbered from 1)
! XNEXT, YNEXT is the projected position
!
subroutine nextPos(iobj, ipNear, idir, izNext, tilt, maxFit, minFit, axisRot, tiltMin, &
    izExclude, numExclude, xnext, ynext)
  implicit none
  include 'smallmodel.inc90'
  real*4 tilt(*), yrot(maxFit + 2), b1(2)
  real*4 xx(maxFit + 2), yy(maxFit + 2), zz(maxFit + 2), xrot(maxFit + 2)
  integer*4 izExclude(*), numExclude, iobj, ipNear, idir, izNext, maxFit, minFit
  real*4 tiltMin, xnext, ynext, axisRot, cosRot, sinRot
  integer*4 ibase, numInObj, mfit, ipEnd, iptNear, ip, ipt, ipb, ipp
  integer*4 iPast, iBefore, i, iz
  real*4 xsum, ysum, slope, bint, ro, const, rsq, fra, thetaNext, xtmp, ytmp, theta
  real*4 cosd, sind
  logical itemOnList

  ibase = ibase_obj(iobj)
  numInObj = npt_in_obj(iobj)
  cosRot = cosd(axisRot)
  sinRot = sind(axisRot)
  mfit = 0
  ipEnd = 1
  if (idir == - 1) ipEnd = numInObj
  !
  ! set pointer past the view if possible
  !
  iptNear = object(ibase + ipNear)
  ip = ipNear
  do ipt = ipNear, ipEnd, -idir
    iz = nint(p_coord(3, object(ibase + ipt)))
    if (idir * (iz - izNext) < 0)  &
        exit
    if (.not. itemOnList(iz + 1, izExclude, numExclude)) ip = ipt
  enddo
  if (idir * (nint(p_coord(3, object(ibase + ip))) - izNext) == - 1) then
    !
    ! if point is adjacent in that direction, then starting from there,
    ! load maxFit points
    !
    do while(idir * (ip - ipEnd) >= 0 .and. mfit < maxFit)
      ipt = object(ibase + ip)
      if (.not. itemOnList(nint(p_coord(3, ipt)) + 1, izExclude, numExclude)) then
        mfit = mfit + 1
        xx(mfit) = p_coord(1, ipt)
        yy(mfit) = p_coord(2, ipt)
        zz(mfit) = p_coord(3, ipt)
      endif
      ip = ip - idir
    enddo
  elseif (nint(p_coord(3, iptNear)) == izNext) then
    !
    ! otherwise, if there is already a point on the section, just take it
    !
    mfit = 1
    xx(mfit) = p_coord(1, iptNear)
    yy(mfit) = p_coord(2, iptNear)
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
    ! starting from there, load maxFit nearest points
    !
    do while((iBefore > 0 .or. iPast <= numInObj) .and. mfit < maxFit)
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
      if (.not. itemOnList(nint(p_coord(3, ipt)) + 1, izExclude, numExclude)) then
        mfit = mfit + 1
        xx(mfit) = p_coord(1, ipt)
        yy(mfit) = p_coord(2, ipt)
        zz(mfit) = p_coord(3, ipt)
      endif
    enddo
  endif
  !
  ! Just average the points if there are not enough
  if (mfit < minFit) then
    !
    ! If there are no points at all, take the nearest even though it is on excluded view
    if (mfit == 0) then
      mfit = 1
      xnext = p_coord(1, iptNear)
      ynext = p_coord(2, iptNear)
    else
      xsum = 0.
      ysum = 0.
      do i = 1, mfit
        xsum = xsum + xx(i)
        ysum = ysum + yy(i)
      enddo
      xnext = xsum / mfit
      ynext = ysum / mfit
      !print *,'average:', iobj, mfit, xnext, ynext
    endif
  else if (mfit >= minFit + 2 .and. abs(tilt(izNext + 1)) >= tiltMin) then
    !
    ! Or, if there are enough for sine-cosine fit and angle is big enough, rotate the
    ! points so the tilt axis is along Y, fit Y to Z and X to cos/sin, and rotate
    ! the result back.  This may never happen unless there are too few points for
    ! tiltalign, but it was tested July 2012 and gave possibly even better results
    do i = 1, mfit
      xrot(i) = cosRot * xx(i) + sinRot * yy(i)
      yrot(i) = -sinRot * xx(i) + cosRot * yy(i)
      theta = tilt(nint(zz(i)) + 1)
      xx(i) = cosd(theta)
      yy(i) = sind(theta)
    enddo
    call lsfit(zz, yrot, mfit, slope, bint, ro)
    ytmp = izNext * slope + bint
    call lsfit2(xx, yy, xrot, mfit, b1(1), b1(2), const)
    thetaNext = tilt(izNext + 1)
    xtmp = b1(1) * cosd(thetaNext) + b1(2) * sind(thetaNext) + const
    xnext = cosRot * xtmp - sinRot * ytmp
    ynext = sinRot * xtmp + cosRot * ytmp
    !print *,'sin/cos fit:', iobj, mfit, xnext, ynext
  else
    !
    ! Or just do straight line fits to both components
    call lsfit(zz, xx, mfit, slope, bint, ro)
    xnext = izNext * slope + bint
    call lsfit(zz, yy, mfit, slope, bint, ro)
    ynext = izNext * slope + bint
    !print *,'linear fit:', iobj, mfit, xnext, ynext
  endif
  return
end subroutine nextPos





! findPIECE takes a list of nPclist piece coordinates in
! I[XYZ]PCLIST, the piece dimensions nx and NY, and index coordinates
! in the montaged image IND[XYZ], and finds the piece that those
! coordinates are in (IPCZ) and the coordinates IPCX, IPCY of the point
! in that piece
!
subroutine findPiece(ixPclist, iyPclist, izPclist, nPclist, nx, ny, nxBox, nyBox, &
    xnext, ynext, izNext, ix0, ix1, iy0, iy1, ipcz, ifXfs, prexf, needTaper)
  implicit none
  integer*4 ixPclist(*), iyPclist(*), izPclist(*)
  integer*4 nPclist, nx, ny, nxBox, nyBox,izNext, ix0, ix1, iy0, iy1, ipcz, ifXfs
  real*4 xnext, ynext, prexf(2, 3, *), critNonBlank
  logical*4 needTaper
  integer*4 indx0, indx1, indy0, indy1, ipc, indGood0, indGood1, nxGood, nyGood
  !
  critNonBlank = 0.75
  indx0 = nint(xnext) - nxBox / 2
  indx1 = indx0 + nxBox - 1
  indy0 = nint(ynext) - nyBox / 2
  indy1 = indy0 + nyBox - 1
  ipcz = -1
  needTaper = .false.
  if (ifXfs .ne. 0) then
    indGood0 = max(indx0, 0, nint(prexf(1, 3, izNext + 1)))
    indGood1 = min(indx1, nx - 1 , nx + nint(prexf(1, 3, izNext + 1)) - 1)
    nxGood = max(0, indGood1 + 1 - indGood0)
    indGood0 = max(indy0, 0, nint(prexf(2, 3, izNext + 1)))
    indGood1 = min(indy1, ny - 1 , ny + nint(prexf(2, 3, izNext + 1)) - 1)
    nyGood = max(0, indGood1 + 1 - indGood0)
    if (nxGood * nyGood < critNonBlank * nxBox * nyBox) return
    needTaper = nxGood < nxBox .or. nyGood < nyBox
  endif
  !
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
end subroutine findPiece


! Does a simple image shift with quadratic interpolation
!
SUBROUTINE QDshift(array, bray, nx, ny, xt, yt)
  implicit none
  integer*4 nx, ny
  real*4 array(nx, ny), bray(nx, ny), xt, yt
  real*4 dx, dy, dxsq, dysq, a, b, c, d, e, v2, v4, v6, v8, v5
  integer*4 iyp, ixp, ixpp1, ixpm1, iypp1, iypm1
  !
  ! Loop over output image
  !
  dx = -xt
  dy = -yt
  dxsq = dx**2
  dysq = dy**2
  do iyp = 1, ny
    do ixp = 1, nx
      !
      ! do quadratic interpolation
      !
      ixpp1 = min(nx, ixp + 1)
      ixpm1 = max(1, ixp - 1)
      iypp1 = min(ny, iyp + 1)
      iypm1 = max(1, iyp - 1)
      !
      ! set up terms for quadratic interpolation
      !
      v2 = array(ixp, iypm1)
      v4 = array(ixpm1, iyp)
      v5 = array(ixp, iyp)
      v6 = array(ixpp1, iyp)
      v8 = array(ixp, iypp1)
      !
      a = (v6 + v4) * .5 - v5
      b = (v8 + v2) * .5 - v5
      c = (v6 - v4) * .5
      d = (v8 - v2) * .5
      !

      bray(ixp, iyp) = (a * dxsq + b * dysq + c * dx + d * dy + v5)
      !
    enddo
  enddo
  !
  RETURN
END SUBROUTINE QDshift


! peakFind finds the coordinates of the the absolute peak, XPEAK, YPEAK
! in the array array, which is dimensioned to nx + 2 by ny.
! This does no interpolation; it is used for the original correlation peak that is
! always corrected to a centroid and not for the Sobel correlation peak, 
!
subroutine peakFind(array, nxPlus, nyRot, xpeak, ypeak, peak)
  implicit none
  integer*4 nxPlus, nyRot
  real*4 array(nxPlus,nyRot), xpeak, ypeak, peak
  integer*4 nxRot, ix, iy, ixPeak, iyPeak
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


! Finds the mean or median of edge pixels for taking the centroid, depending on whether
! edgeMedian is set, and finds the SD of the edge pixels if getEdgeSD is set
!
subroutine edgeForCG(boxTmp, nxBox, nyBox, ixcen, iycen, edge, edgeSD, ierr)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox)
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, nsum, i, ierr
  real*4 sum, edge, sumsq, edgeSD
  sum = 0.
  nsum = 0
  edge = 0.
  ierr = 1
  !
  ! find edge mean - require half the points to be present
  !
  if (edgeMedian .or. getEdgeSD) then
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
  if (getEdgeSD) call avgsd(edgePixels, nsum, edge, edgeSD, sum)
  if (edgeMedian) then
    call rsFastMedianInPlace(edgePixels, nsum, edge)
  else if (.not. getEdgeSD) then
    edge = sum / nsum
  endif
  return
end subroutine edgeForCG


! Adjust the center position for computing a centroid or a elongation by finding the
! strongest set of 4 pixels within 1 pixel of the original position xcen, ycen.
! Returns the integer position to use as the center in ixcen, iycen and the best sum
! of 4 pixels in best.
!
subroutine bestCenterForCG(boxTmp, nxBox, nyBox, xpeak, ypeak, ixcen, iycen, best)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox), xpeak, ypeak
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, ixBest, iyBest
  real*4 best, sum4
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
  return
end subroutine bestCenterForCG


! Calculates the centroid of a bead around the position xpeak, ypeak, revises the position
! in those variables, and returns the sum of pixels (times polarity) in wsum and the
! Sd of edge pixles in edgeSD (if getEdgeSD is set)
!
subroutine calcCG(boxTmp, nxBox, nyBox, xpeak, ypeak, wsum, edgeSD)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox), xpeak, ypeak, wsum
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, nsum, i
  real*4 sum, xsum, ysum, weight, edge, edgeSD, bestSum, posSum
  !
  call bestCenterForCG(boxTmp, nxBox, nyBox, xpeak, ypeak, ixcen, iycen, bestSum)
  xsum = 0.
  ysum = 0.
  wsum = 0.
  edgeSD = 0.
  !
  ! find edge mean - require half the points to be present
  call edgeForCG(boxTmp, nxBox, nyBox, ixcen, iycen, edge, edgeSD, i)
  if (i .ne. 0) return
  !
  ! subtract edge and get weighted sum of pixel coordinates for POSITIVE
  ! pixels
  !
  posSum = 0.
  do i = 1, numInside
    ix = ixcen + idxIn(i)
    iy = iycen + idyin(i)
    if (ix >= 1 .and. ix <= nxBox .and. iy >= 1 .and. iy <= nyBox) then
      weight = boxTmp(ix, iy) - edge
      if (iPolarity * weight > 0.) then
        xsum = xsum + ix * weight
        ysum = ysum + iy * weight
        posSum = posSum + weight
      endif
      wsum = wsum + weight
    endif
  enddo
  if (posSum == 0.) return
  xpeak = xsum / posSum - 0.5 - nxBox / 2
  ypeak = ysum / posSum - 0.5 - nyBox / 2
  wsum = max(0., wsum * iPolarity)
  return
end subroutine calcCG


! Computes a sum of central pixels above edge pixels around the position in xpeak, ypeak,
! without adjusting the center position for computation or computing a centroid position
! for revising xpeak, ypeak.  Also returns the edge SD if appropriate.
!
subroutine wsumForSobelPeak(boxTmp, nxBox, nyBox, xpeak, ypeak, wsum, edgeSD)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox,nyBox), xpeak, ypeak, wsum, edgeSD
  integer*4 nxBox, nyBox, ixcen, iycen, iy, ix, i
  real*4 edge
  !
  ixcen = nxBox / 2 + nint(xpeak)
  iycen = nyBox / 2 + nint(ypeak)
  wsum = 0.
  edgeSD = 0.
  !
  ! find edge mean - require half the points to be present
  call edgeForCG(boxTmp, nxBox, nyBox, ixcen, iycen, edge, edgeSD, i)
  if (i .ne. 0) return
  !
  ! subtract edge and get weight sum for ALL pixels
  !
  do i = 1, numInside
    ix = ixcen + idxIn(i)
    iy = iycen + idyin(i)
    if (ix >= 1 .and. ix <= nxBox .and. iy >= 1 .and. iy <= nyBox) then
      wsum = wsum + boxTmp(ix, iy) - edge
    endif
  enddo
  wsum = max(0., wsum * iPolarity)
  return
end subroutine wsumForSobelPeak


! RESCUE searches in progressively wider rings from the center of the box outward for
! a peak in the CG above the relaxed criterion relaxCrit.  The biggest peak in a ring
! is taken and the search is terminated when one is found.  Otherwise wsum and edgeSd
! are returned with 0.
!
subroutine rescue(boxTmp, nxBox, nyBox, xpeak, ypeak, radMax, relaxCrit, wsum, edgeSD)
  use cgPixels
  implicit none
  real*4 boxTmp(*), xpeak, ypeak, radMax, relaxCrit, wsum, edgeSD
  integer*4 nxBox, nyBox, idx, idy
  real*4 rad, radInc, radSq, radOutSq, wBest, distSq, xtmp, ytmp, wtmp, sdtmp, sdBest
  wsum = 0.
  edgeSD = 0.
  if (relaxCrit <= 0.) return
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
          call calcCG(boxTmp, nxBox, nyBox, xtmp, ytmp, wtmp, sdtmp)
          if (wtmp > wBest) then
            wBest = wtmp
            xpeak = xtmp
            ypeak = ytmp
            if (getEdgeSD) sdBest = sdtmp
          endif
        endif
      enddo
    enddo
    rad = rad + radInc
    if (wBest >= relaxCrit) then
      wsum = wBest
      if (getEdgeSD) edgeSD = sdBest
    endif
  enddo
  return
end subroutine rescue

! Not used, did not help.
!
subroutine rescueFromSobel(boxTmp, nxBox, nyBox, xpeak, ypeak, sobelXpeaks, &
    sobelYpeaks, sobelPeaks, sobelWsums, sobelEdgeSD, maxPeaks, radMax, relaxCrit, wsum, &
    edgeSD)
  use cgPixels
  implicit none
  real*4 boxTmp(*), xpeak, ypeak, sobelXpeaks(*), sobelYpeaks(*), sobelPeaks(*)
  real*4 sobelWsums(*), sobelEdgeSD(*), radMax, relaxCrit, wsum, edgeSD
  integer*4 nxBox, nyBox, maxPeaks, ipeak
  real*4 rad, radInc, radSq, radOutSq, wBest, distSq, sdBest
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
          sobelPeaks, sobelWsums, sobelEdgeSD, maxPeaks)
      if (distSq > radSq .and. distSq <= radOutSq .and. sobelWsums(ipeak) > wBest) then
        wBest = sobelWsums(ipeak)
        xpeak = sobelXpeaks(ipeak)
        ypeak = sobelYpeaks(ipeak)
        sdBest = sobelEdgeSD(ipeak)
      endif
    enddo
    rad = rad + radInc
    if (wBest >= relaxCrit) then
      wsum = wBest
      if (getEdgeSD) edgeSD = sdBest
    endif
  enddo
  return
end subroutine rescueFromSobel


! checkSobelPeak makes sure the wsum has been computed for the indPeak peak in the
! list, returns 0 if there is no peak or comutes it if necessary
!
subroutine checkSobelPeak(indPeak, boxTmp, nxBox, nyBox, xpeaks, yPeaks, peaks, wsums, &
    edgeSDs, maxPeaks)
  implicit none
  real*4 boxTmp(*), xpeaks(*), yPeaks(*), peaks(*), wsums(*), edgeSDs(*)
  integer*4 nxBox, nyBox, maxPeaks, indPeak
  real*4 xtmp, ytmp
  if (wsums(indPeak) >= 0.) return
  if (peaks(indPeak) < - 1.e29) then
    wsums(indPeak) = 0.
    return
  endif
  call wsumForSobelPeak(boxTmp, nxBox, nyBox, xpeaks(indPeak), yPeaks(indPeak), &
      wsums(indPeak), edgeSDs(indPeak))
  return
end subroutine checkSobelPeak


! calcElongation calculates a measure of elongation for the portion of density that
! is at least a certain fraction above the edge intensity.  Data are in boxTmp, 
! dimensioned nxBox x nyBox, and the nomin
!
subroutine calcElongation(boxTmp, nxBox, nyBox, xpeak, ypeak, elongation)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox, nyBox), xpeak, ypeak, elongation
  integer*4 nxBox, nyBox
  integer*4 i, ixcen, iycen, numPos, ix, iy, idir, indCheck
  integer*4 idelx(4)/-1, 1, 0, 0/, idely(4)/0, 0, -1, 1/
  real*4 xmean, ymean, dx, dy, edge, edgeSD, thresh, threshFrac, root, bestSum
  real*8 dxsum, dysum, dxsqsum, dysqsum, dxysum
  logical*4 edgeSDsave, edgeMedianSave
  !
  threshFrac = 0.20
  !
  ! Smooth the data then find an adjusted center from the smoothed data
  call applyKernelFilter(boxTmp, elongSmooth, nxBox, nxBox, nyBox, elongKernel, &
      kernDimElong)
  call bestCenterForCG(elongSmooth, nxBox, nyBox, xpeak, ypeak, ixcen, iycen, bestSum)
  elongation = -1.
  !
  ! Get an edge median regardless of normal setting
  edgeSDsave = getEdgeSD
  edgeMedianSave = edgeMedian
  getEdgeSD = .false.
  edgeMedian = .true.
  call edgeForCG(elongSmooth, nxBox, nyBox, ixcen, iycen, edge, edgeSD, i)
  getEdgeSD = edgeSDsave
  edgeMedian = edgeMedianSave
  if (i .ne. 0 .or. ixcen <= 0 .or. ixcen > nxBox .or. iycen <= 0 .or. iycen > nyBox)  &
      return
  !
  ! Get threshold value and start a list of points to check with the center point
  thresh = edge + threshFrac * (bestSum / 4. - edge)
  numPos = 1
  ixElong(1) = ixcen
  iyElong(1) = iycen
  indCheck = 1
  elongMask = .false.
  elongMask(ixcen, iycen) = .true.
  dxsum = 0; dysum = 0
  !
  ! Make a list of pixels above the threshold by checking the four neighbors of each point
  ! on list, adding to list and setting a mask as each is found
  do while (indCheck <= numPos)
    do i = 1, 4
      ix = max(1, min(nxBox, ixElong(indCheck) + idelx(i)))
      iy = max(1, min(nyBox, iyElong(indCheck) + idely(i)))
      if (.not. elongMask(ix, iy) .and. iPolarity * (elongSmooth(ix, iy) - thresh) > 0) &
          then
        elongMask(ix, iy) = .true.
        numPos = numPos + 1
        ixElong(numPos) = ix
        iyElong(numPos) = iy
        dxsum = dxsum + ix
        dysum = dysum + iy
      endif
    enddo
    indCheck = indCheck + 1
  enddo
  if (numPos < 4) return

  ! Get the means and moments and apply the equation for elongation
  xmean = dxsum / numPos
  ymean = dysum / numPos
  dxsqsum = 0; dysqsum = 0; dxysum = 0
  do i = 1, numPos
    dxsqsum = dxsqsum + (ixElong(i) - xmean)**2
    dysqsum = dysqsum + (iyElong(i) - ymean)**2
    dxysum = dxysum + (ixElong(i) - xmean) * (iyElong(i) - ymean)
  enddo

  root = sqrt(4. * dxysum**2 + (dxsqsum - dysqsum)**2)
  elongation = (dxsqsum + dysqsum + root) / (dxsqsum + dysqsum - root)

  ! This is supposedly the axis angle but it seemed flaky.  Trying to find a long axis
  ! by looking at points above threshold was problematic
  ! angle = atan2(2. * dxysum, dxsqsum - dysqsum)
  return
end subroutine calcElongation


! calcOuterMAD computes a mean and a median absolute deviation in in an outer region
! of the box.  This did not trun out to be useful
!
subroutine calcOuterMAD(boxTmp, nxBox, nyBox, xpeak, ypeak, outerMAD, background)
  use cgPixels
  implicit none
  real*4 boxTmp(nxBox, nyBox), xpeak, ypeak, outerMAD, rmedian, background
  integer*4 nxBox, nyBox
  integer*4 i, ixcen, iycen, ix, iy, nsum
 
  call applyKernelFilter(boxTmp, elongSmooth, nxBox, nxBox, nyBox, outerKernel, &
      kernDimOuter)
  nsum = 0
  ixcen = nint(xpeak)
  iycen = nint(ypeak)
  do i = 1, numOuter
    ix = ixcen + idxOuter(i)
    iy = iycen + idyOuter(i)
    if (ix >= 3 .and. ix <= nxBox - 2  .and. iy >= 3 .and. iy <= nyBox - 3) then
      nsum = nsum + 1
      outerPixels(nsum) = boxTmp(ix, iy)
    endif
  enddo
  outerMAD = 0.
  if (nsum < 11) return
  !
  call rsFastMedianInPlace(outerPixels, nsum, background)

  nsum = 0
  do i = 1, numOuter
    ix = ixcen + idxOuter(i)
    iy = iycen + idyOuter(i)
    if (ix >= 3 .and. ix <= nxBox - 2  .and. iy >= 3 .and. iy <= nyBox - 3) then
      nsum = nsum + 1
      outerPixels(nsum) = elongSmooth(ix, iy)
    endif
  enddo
  call rsFastMedianInPlace(outerPixels, nsum, rmedian)
  outerPixels(1:nsum) = abs(outerPixels(1:nsum) - rmedian)
  call rsFastMedianInPlace(outerPixels, nsum, outerMAD)
  return
end subroutine calcOuterMAD


! Adds a point to the output model
!
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


! adjustXYZinAreas shifts XYZ values of the different local areas so that points shared
! between two areas have the same mean position in the two areas
!
subroutine adjustXYZinAreas(iobjLists, listSize, indObjList, ninObjList, xyzAll, &
    nobjLists, xyzObj, numObj)
  implicit none
  integer*4 iobjLists(*), indObjList(*), ninObjList(*)
  real*4 xyzAll(3, *), xyzObj(3, *)
  integer*4 numObj, nobjLists, listSize
  integer*4 numPrevAreas(listSize + numObj)
  integer*4 indStartInPrevList(listSize + numObj)
  integer*4, allocatable :: listPrevInds(:) 
  real*4 dxyz(3), sumDxyz(3), dxyzLast(3), dxyzAvg(3), dxyzMax(3), avgXyz(3)
  integer*4 loop, indFree, iseq, iobj, indObj, jseq, jndList, jobj, iter, maxIter
  integer*4 numInErr, numInSum, i, ind, j, numAvgForTest, intervalForTest
  integer*4 ibaseOrigObj, indList, jndObj
  real*4 critMaxMove, critMoveDiff
  real*8 error

  maxIter = 1000
  critMaxMove = .01
  critMoveDiff = 0.001
  intervalForTest = 20
  numAvgForTest = 10
  
  ! Go through points twice, first time just counting how big the list array needs to be
  ! then allocate it, then second time, fill the list of indices
  ibaseOrigObj = indObjList(nobjLists) + ninObjList(nobjLists)
  ninObjList(nobjLists + 1) = numObj
  indObjList(nobjLists + 1) = ibaseOrigObj
  numPrevAreas = 0
  do loop = 1, 2
    indFree = 1
    do iseq = 1, nobjLists + 1
      do indList = 1, ninObjList(iseq)
        indObj = indObjList(iseq) + indList - 1
        indStartInPrevList(indObj) = indFree
        if (iseq <= nobjLists) then
          iobj = iobjLists(indObj)
          !
          ! Skip all points with no solved xyz
          if (xyzAll(1, indObj) == 0. .and. xyzAll(1, indObj) == 0. .and.  &
              xyzAll(1, indObj) == 0.)  &
              cycle
        else 
          iobj = indList
        endif
        do jseq = 1, iseq - 1
          do jndList = 1, ninObjList(jseq)
            jndObj = indObjList(jseq) + jndList - 1
            jobj = iobjLists(jndObj)
            if (jobj == iobj .and. (xyzAll(1, jndObj) .ne. 0. .or.  &
                xyzAll(1, jndObj) .ne. 0. .or. xyzAll(1, jndObj) .ne. 0.) )then
              if (loop > 1) then
                listPrevInds(indFree) = jndObj
                numPrevAreas(indObj) = numPrevAreas(indObj) + 1
              endif
              indFree = indFree + 1
            endif
          enddo
        enddo
        if (loop == 2) then
         ! write(*,'(a,i3,a,3i5,4x,(8i5))')'area',iseq,'  object',iobj,indStartInPrevList(indObj),numPrevAreas(indObj), &
         ! (listPrevInds(i+indStartInPrevList(indObj)-1), i = 1,numPrevAreas(indObj))
        endif
      enddo
    enddo
    if (loop == 1) then
      allocate(listPrevInds(indFree), stat=iseq)
      call memoryError(iseq, 'ARRAY FOR INDICES IN PREVIOUS AREAS')
    endif
  enddo
  !
  ! Iterate until changes become small.  The termination logic is adopted from
  ! libcfshr/find_piece_shifts.c
  dxyzLast = 0.
  dxyzAvg = 0.
  do iter = 1, maxIter
    error = 0.
    numInErr = 0
    sumDxyz = 0.
    dxyzMax = 0.
    do iseq = 2, nobjLists

      ! For each area in turn, add up all shifts relative to same points in previous areas
      dxyz = 0.
      numInSum = 0
      do indList = 1, ninObjList(iseq)
        indObj = indObjList(iseq) + indList - 1
        !if (numPrevAreas(indObj) > 0) print *,indList,iobjLists(indObj), indObj, xyzAll(1:3, indObj)
        do i = 1, numPrevAreas(indObj)
          ind = listPrevInds(indStartInPrevList(indObj) + i - 1)
          !print *,iobjLists(ind), ind, xyzAll(1:3, ind)
          dxyz(1:3) = dxyz(1:3) + (xyzAll(1:3, indObj) - xyzAll(1:3, ind))
        enddo
        numInSum = numInSum + numPrevAreas(indObj)
      enddo

      ! Get the mean amount of shift needed, and accumulate the mean and maximum
      ! absolute value of the shift for each dimension
      dxyz = dxyz / max(1, numInSum)
      sumDxyz = sumDxyz + abs(dxyz)
      do i = 1, 3
        dxyzMax(i) = max(dxyzMax(i), abs(dxyz(i)))
      enddo

      ! Subtract the shift from each xyz in the current area
      do indList = 1, ninObjList(iseq)
        indObj = indObjList(iseq) + indList - 1
        if (xyzAll(1, indObj) .ne. 0. .or. xyzAll(1, indObj) .ne. 0. .or.  &
            xyzAll(1, indObj) .ne. 0.)  &
            xyzAll(1:3, indObj) = xyzAll(1:3, indObj) - dxyz(1:3)
      enddo
    enddo

    ! Compute error as mean difference from the average position
    error = 0.
    do iobj = 1, numObj
      avgXyz = 0.
      numInSum = numPrevAreas(ibaseOrigObj + iobj - 1)
      do i = 1, numInSum
        ind = listPrevInds(indStartInPrevList(ibaseOrigObj + iobj - 1) + i - 1)
        avgXyz(1:3) = avgXyz(1:3) + xyzAll(1:3, ind) / numInSum
      enddo
      
      do i = 1, numInSum
        ind = listPrevInds(indStartInPrevList(ibaseOrigObj + iobj - 1) + i - 1)
        do j = 1,3
          error = error + (avgXyz(j) - xyzAll(j, ind))**2
        enddo
      enddo
      numInErr = numInErr + numInSum
    enddo

    error = sqrt(error / max(1, numInErr))
    write(*,'(i4,7f10.3)')iter, error, dxyzMax, dxyzAvg

    ! If the maximum move is ever less than this criterion in all dimensions, finished
    if (dxyzMax(1) < critMaxMove .and. dxyzMax(2) < critMaxMove .and.  &
        dxyzMax(3) < critMaxMove) &
      exit

    ! Periodically accumulate the average move over several iterations
    if (mod(iter, intervalForTest) >= intervalForTest - numAvgForTest)  &
        dxyzAvg = dxyzAvg + sumDxyz / (nobjLists - 1)

    ! Then test whether the average move has fallen by less than this criterion
    if (mod(iter, intervalForTest) == intervalForTest - 1) then
      dxyzAvg = dxyzAvg / numAvgForTest;
      if (dxyzLast(1) - dxyzAvg(1) < critMoveDiff .and. dxyzLast(2) - dxyzAvg(2) <  &
          critMoveDiff .and. dxyzLast(3) - dxyzAvg(3) < critMoveDiff)  &
          exit
      dxyzLast = dxyzAvg
      dxyzAvg = 0.
    endif

  enddo

  ! Average each bead into the array of one per object
  xyzObj(1:3, 1:numObj) = 0.
  do iobj = 1, numObj
    numInSum = numPrevAreas(ibaseOrigObj + iobj - 1)
    do i = 1, numInSum
      ind = listPrevInds(indStartInPrevList(ibaseOrigObj + iobj - 1) + i - 1)
      xyzObj(1:3, iobj) = xyzObj(1:3, iobj) + xyzAll(1:3, ind) / numInSum
    enddo
  enddo

end subroutine adjustXYZinAreas

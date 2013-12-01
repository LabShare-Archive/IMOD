! ****   solveXyzd obtains estimates for the underlying (real) x, y, z
! coordinates of each point and for the delta X and Y of each view,
! for the given values of tilt, rotation, mag and compression.
!
! The equations relating the parameters are:
! _    xproj = a * x + b * y + c * z + dx(view)
! _    yproj = d * x + e * y + f * z + dy(view)
! where a = mag * cos (tilt) * cos (rot)
! _     b = - mag * sin (rot)
! _     c = mag * comp * sin (tilt) * cos (rot)
! where d = mag * cos (tilt) * sin (rot)
! _     e = mag * cos (rot)
! _     f = mag * comp * sin (tilt) * sin (rot)
!
! 5/18/12: Replaced old solve_xyzd and init_dxy routines
! 8/17/12: Restored old solve_xyzd and init_dxy routines for large numbers of points
!
! $Id$
!
! sprod should be allocated to at least (3 * npt - 1) * (3 * npt - 2) / 2
! where npt = min(numRealpt, maxRealForDirectInit)
!
subroutine solveXyzd(xx, yy, isecView, irealStart, nview, numRealPt, tilt, rot, gmag, &
    comp, xyz, dxy, rotInc, sprod, error, ierr)
  use arraymaxes
  implicit none
  real*4 xx(*), yy(*), tilt(*), rot(*), gmag(*), comp(*) , xyz(3,*), dxy(2,*), rotInc
  integer*4 isecView(*), irealStart(*), nview, numRealPt, ierr
  real*8 error
  real*4 valRow(3*numRealPt), baseRow(3*numRealPt, 2)
  real*8 sprod(*), sx(3*numRealPt), xml(3*numRealPt), sdl(3*numRealPt), bl(3*numRealPt)
  integer*4 indOnView(3*numRealPt), indqk(4), j
  logical bigBase(2), bigRow
  real*4 ad(2), be(2), cf(2), fac, xybar(2), xyzcen(3)
  integer*4 numCols, numRows, iv, jpt, i, ixy, icol, ipt, numOnView
  real*8       wallStart, wallTime, addTime
  integer*4 packedIndex

  ! If the overall data size is large or the number of points exceeds the limit, use
  ! the old iterative method
  if (numRealPt * nview > 10000 .or. numRealPt > maxRealForDirectInit) then
    wallStart = wallTime()
    ierr = 0
    ipt = max(95, 90 + int(sqrt(float(numRealPt))))
    call solveIteratively(ipt)
    !write(*,'(a,f9.3, a, f20.5)') 'Iterative time', 1000. * (wallTime() - wallStart), &
    !'   error', error
    !call flush(6)
    return
  endif
  !
  ! Load data matrix, loop on views then real points projecting on each
  numCols = 3 * numRealPt - 2
  numRows = 0
  ierr = 1
  addTime = 0
  if (numRealPt < 2) then
    ierr = 0
    xyz(1:3, 1) = 0.
    return
  endif
  sx(1:numCols) = 0.
  sprod(1: (numCols + 1) * numCols / 2) = 0.
  do iv = 1, nview
    !
    ! Set up ad, be, cf, and get indexes to points on view and get xybar
    call geometricCoeffsAndIndices(iv)
    !
    ! Set up the terms for the dxy: if last point is not on view, subtract
    ! a / n etc from the columns for all points on view; if last point is on
    ! view, add a / n to the columns for all points not in the view
    do ixy = 1, 2
      bigBase(ixy) = .false.
      baseRow(1:numCols, ixy) = 0.
      do ipt = 1, numRealPt - 1
        if (indOnView(ipt) > 0 .and. indOnView(numRealPt) == 0 .or. &
            indOnView(ipt) == 0 .and. indOnView(numRealPt) > 0) then
          fac = 1.
          if (indOnView(numRealPt) == 0) fac = -1.
          icol = 3 * ipt - 2
          baseRow(icol, ixy) = fac * ad(ixy) / numOnView
          baseRow(icol + 1, ixy)  = fac * be(ixy) / numOnView
          baseRow(icol + 2, ixy)  = fac * cf(ixy) / numOnView
          !
          ! Keep track of whether this vector is non - zero
          bigBase(ixy) = .true.
        endif
      enddo
    enddo
    !
    ! Loop on points again and set up the equations for their projections
    do jpt = 1, numRealPt
      if (indOnView(jpt) > 0) then
        do ixy = 1, 2
          valRow(1:numCols) = baseRow(1:numCols, ixy)
          bigRow = .false.
          if (jpt < numRealPt) then
            icol = 3 * jpt - 2
            valRow(icol) = valRow(icol) + ad(ixy)
            valRow(icol + 1) = valRow(icol + 1) + be(ixy)
            valRow(icol + 2) = valRow(icol + 2) + cf(ixy)
          else
            !
            ! Last point is negative sum of all the rest, and makes a big row of data
            do ipt = 1, numRealPt - 1
              icol = 3 * ipt - 2
              valRow(icol) = valRow(icol) - ad(ixy)
              valRow(icol + 1) = valRow(icol + 1) - be(ixy)
              valRow(icol + 2) = valRow(icol + 2) - cf(ixy)
            enddo
            bigRow = .true.
          endif
          !
          ! Get the projection position and put it in the last column
          ! Adjust that position by the mean of points on view
          if (ixy == 1) then
            valRow(numCols) = xx(indOnView(jpt)) - xybar(1)
          else
            valRow(numCols) = yy(indOnView(jpt)) - xybar(2)
          endif
          ! write(*, '(3(2f9.4,f8.4))') (valRow(i), i = 1, numCols)
          !
          ! Row is done.  Accumulate it
          wallStart = wallTime()
          if (bigRow .or. bigBase(ixy)) then
            call addRowToSums(valRow, numCols, sx, sprod)
          else
            !
            ! Just handle non - zero columns if there are a few, it is a lot quicker
            indqk(1) = icol
            indqk(2) = icol + 1
            indqk(3) = icol + 2
            indqk(4) = numCols
            do j = 1, 4
              sx(indqk(j)) = sx(indqk(j)) + valRow(indqk(j))
              do i = 1, j
                icol = packedIndex(indqk(i), indqk(j))
                sprod(icol) = sprod(icol) + valRow(indqk(i)) * valRow(indqk(j))
              enddo
            enddo
          endif
          addTime = addTime + wallTime() - wallStart
          numRows = numRows + 1
        enddo
      endif
    enddo
  enddo
  call flush(6)
  wallStart = wallTime()

  call solvePackedSums(sx, sprod, numCols, numRows, xml, sdl, bl, ierr)
  ! print *,'lapack', ierr
  ! write(*, '(3(2f9.4,f8.4))') (sdl(i), i = 1, numCols)
  ! write(*, '(3(2f9.4,f8.4))') (bl(i), i = 1, numCols - 1)
  !write(*,'(a,f9.3, a, f10.3)') 'solution time', 1000. * (wallTime() - wallStart), &
  ! '   addRow time', 1000. * addTime
  !
  ! retrieve solution
  xyz(1:3, numRealPt) = 0.;
  do i = 1, numRealPt - 1
    do ixy = 1, 3
      icol = 3 * (i - 1) + ixy
      xyz(ixy, i) = bl(icol)
      xyz(ixy, numRealPt) = xyz(ixy, numRealPt) - xyz(ixy, i)
    enddo
  enddo
  !
  ! compute total error, equals F reported by funct
  error = 0.
  do iv = 1, nview
    call geometricCoeffsAndIndices(iv)
    xyzcen = 0.
    do jpt = 1, numRealPt
      if (indOnView(jpt) > 0) xyzcen = xyzcen + xyz(1:3, jpt)
    enddo
    do ixy = 1, 2
      dxy(ixy, iv) = xybar(ixy) - (ad(ixy) * xyzcen(1) + be(ixy) * xyzcen(2) + &
          cf(ixy) * xyzcen(3)) / numOnView
    enddo
    do jpt = 1, numRealPt
      i = indOnView(jpt)
      if (i > 0) error = error + &
          (ad(1) * xyz(1, jpt) + be(1) * xyz(2, jpt) + cf(1) * xyz(3, jpt) + &
          dxy(1, iv) - xx(i))**2 + &
          (ad(2) * xyz(1, jpt) + be(2) * xyz(2, jpt) + cf(2) * xyz(3, jpt) + &
          dxy(2, iv) - yy(i))**2
    enddo
  enddo
  !write(*,'(a,f7.1,a,f15.5)') 'Rotinc =', rotInc * 180 / 3.14159, ',  ERROR = ', error
  !call flush(6)
  return

CONTAINS
  !
  ! Sets up the ad, be, cf for a view, counts points on view, gets indices to them
  ! and computes the mean projection position
  !
  subroutine geometricCoeffsAndIndices(iv)
    implicit none
    integer*4 iv
    real*4 cosTheta, cSinTheta, gCosPhi, gSinPhi
    cosTheta = cos(tilt(iv))
    cSinTheta = comp(iv) * sin(tilt(iv))
    gCosPhi = gmag(iv) * cos(rot(iv) + rotInc)
    gSinPhi = gmag(iv) * sin(rot(iv) + rotInc)
    !
    ad(1) = cosTheta * gCosPhi
    be(1) = -gSinPhi
    cf(1) = cSinTheta * gCosPhi
    ad(2) = cosTheta * gSinPhi
    be(2) = gCosPhi
    cf(2) = cSinTheta * gSinPhi
    !
    ! Count  and get indices and get the mean projection coordinate
    numOnView = 0
    xybar = 0.
    do jpt = 1, numRealPt
      indOnView(jpt) = 0
      do i = irealStart(jpt), irealStart(jpt + 1) - 1
        if (iv == isecView(i)) then
          indOnView(jpt) = i
          numOnView = numOnView + 1
          xybar(1) = xybar(1) + xx(i)
          xybar(2) = xybar(2) + yy(i)
        endif
      enddo
    enddo
    xybar = xybar  / numOnView
  end subroutine geometricCoeffsAndIndices


  ! solveIteratively implements the original interative solution method
  ! try either with initial dxy solved to equalize centroids section-to- section, or
  ! with dxy 0.  Find which way gives lowest error somewhere along the line, and redo it
  ! that way to do just the best number of iterations.
  !
  subroutine solveIteratively(maxSolve)
    integer*4 maxSolve
    real*4 errList(maxSolve + 5)
    integer*4 itry, iminTilt, isolMinInit, isolMin, isolve, iv, ipt, nsum
    real*4 errMinInit, errMin, cgx, cgy

    !
    ! Get the view at minimum tilt and get the CG of the points on that view
    !
    iminTilt = 1
    do iv = 1, nview
      if (abs(tilt(iv)) < abs(tilt(iminTilt))) iminTilt = iv
    enddo
    cgx = 0.
    cgy = 0.
    nsum = 0
    do ipt = 1, numRealPt
      do i = irealStart(ipt), irealStart(ipt + 1) - 1
        if (isecView(i) == iminTilt) then
          cgx = cgx + xx(i)
          cgy = cgy + yy(i)
          nsum = nsum + 1
        endif
      enddo
    enddo
    cgx = cgx / nsum
    cgy = cgy / nsum
    
    ! initial trial with call to INIT_DXY
    call init_dxy(xx, yy, isecView, irealStart, nview, numRealPt, iminTilt, dxy, cgx, cgy)
    !
    do itry = 1, 2
      !
      ! second time through, save minimum error and iteration # from
      ! first trial that used call to init_dxy
      !
      if (itry == 2) then
        isolMinInit = isolMin
        errMinInit = errMin
      endif
      !
      call solve_xyzd_iter(xx, yy, isecView, irealStart, nview, numRealPt, tilt, rot, &
          gmag, comp, rotInc, xyz, dxy, maxSolve, error, errList, isolve)
      !
      ! find iteration with minimum error
      !
      errMin = 1.e30
      do i = 1, isolve-1
        if (errList(i) < errMin) then
          isolMin = i
          errMin = errList(i)
        endif
      enddo
      !write(*,'(2i4,f15.7,i4,4f10.6)'),itry, isolve, errMin, isolMin, dxy(1, iminTilt), &
      !    dxy(2, iminTilt), cgx, cgy
      !
      ! set dxy to 0 for second try, or leave at zero for final setup
      !
      do iv = 1, nview
        dxy(1, iv) = cgx
        dxy(2, iv) = cgy
      enddo
    enddo
    !
    if (errMinInit < errMin) then
      isolMin = isolMinInit
      call init_dxy(xx, yy, isecView, irealStart, nview, numRealPt, iminTilt, dxy, cgx, &
          cgy)
      !print *, 'DXY set to equalize centroids gave best initialization'
    else
      !print *, 'DXY set to zero gave best initialization'
    endif
    !
    call solve_xyzd_iter(xx, yy, isecView, irealStart, nview, numRealPt, tilt, rot, &
        gmag, comp, rotInc, xyz, dxy, isolMin, error, errList, isolve)
  end subroutine solveIteratively

end subroutine solveXyzd

! Accumulate sums and sums of cross - products from a row of data
!
subroutine addRowToSums(row, m, sx, sprod)
  implicit none
  real*4 row(*)
  real*8 sx(*), sprod(*)
  integer*4 m, j, ind
  integer*4 packedIndex
  sx(1:m) = sx(1:m) + row(1:m)
  do j = 1, m
    ind = packedIndex(1, j)
    sprod(ind:ind + j - 1) = sprod(ind:ind + j - 1) + row(1:j) * row(j)
  enddo
  return
end subroutine addRowToSums

! A subroutine for calling the linear solver, just in case this is useful elsewhere
!
subroutine solvePackedSums(sx, ss, mp, nrows, xm, sd, b, ierr)
  implicit none
  real*8 sx(*), ss(*), xm(*), sd(*), b(*)
  integer*4 mp, nrows, i, j, ierr, m, ind
  integer*4 packedIndex
  !
  ! First get the means and SDs
  m = mp - 1
  do i = 1, mp
    xm(i) = sx(i) / nrows
    sd(i) = sqrt((ss(packedIndex(i, i)) - sx(i)**2 / nrows) / (nrows - 1.))
  enddo

  !
  ! What we have now is a raw sum of squares and cross - products
  ! Scale the matrix by the sd's; this scales the RHS (b) variable in the last column
  ! This is what multrd does, and is not the same as multr does, which is to convert
  ! the matrix to true correlation coefficients, because when there is no constant
  ! term the solution involves raw sums of squares instead of deviations from mean
  do j = 1, mp
    ind = packedIndex(1, j)
    do i = 1, j
      ss(ind + i - 1) = ss(ind + i - 1) / (sd(i) * sd(j))
    enddo
  enddo
  !
  ! Now we can call lapack with this matrix.  Since it is not a covariance matrix,
  ! can't use the positive - definite routine, have to use the symmetric matrix one
  ind = packedIndex(1, mp)
  call dspsv('U', m, 1, ss, b, ss(ind), m, ierr)
  if (ierr .ne. 0) return
  !
  ! scale and return b
  do i = 1, m
    b(i) = ss(ind + i - 1) * sd(mp) / sd(i)
  enddo
  return
end subroutine solvePackedSums

! Silly function because statement functions are now obsolete
integer*4 function packedIndex(i, j)
  integer*4 i, j
  packedIndex = i + (j - 1) * j / 2
  return
end function packedIndex

! ****   solve_xyzd_iter obtains estimates for the underlying (real) x, y, z
! coordinates of each point and for the delta X and Y of each view,
! for the given values of tilt, rotation, mag and compression.
! It takes advantage of the linear relationship between the projection
! x and y coordinates and the x, y, z and dx and dy.  The routine takes
! in incoming values of dx and dy, and, for each real point, uses
! regression equations to solve for the x, y, z coordinates of that
! point.  then it uses the collection of x, y, z coordinates to compute
! the dx and dy values for each section.  This process is repeated up
! to NSOLVE times, or until the biggest change in dx or dy becomes
! tiny.  The procedure is not guaranteed to converge for large data
! sets and is useful only for quickly getting good initial estimates of
! the (x, y, z) .
!
! The equations relating the parameters are:
! _    xproj = a*x + b*y + c*z + dx(view)
! _    xproj = d*x + e*y + f*z + dy(view)
! where a = mag * cos (tilt) * cos (rot)
! _     b = - mag * sin (rot)
! _     c = mag * comp * sin (tilt) * cos (rot)
! where d = mag * cos (tilt) * sin (rot)
! _     e = mag * cos (rot)
! _     f = mag * comp * sin (tilt) * sin (rot)
!
subroutine solve_xyzd_iter(xx, yy, isecView, irealStart, nview, numRealPt, tilt, rot, &
    gmag, comp, rotInc, xyz, dxy, maxSolve, error, errList, isolve)
  implicit none
  !
  real*4 xx(*), yy(*), tilt(*), rot(*), gmag(*), comp(*), xyz(3,*), dxy(2,*), rotInc
  integer*4 isecView(*), irealStart(*)
  double precision error
  !
  real*4 bvec(3)
  real*4 a(nview), b(nview), c(nview), asq(nview), bsq(nview), csq(nview)
  real*4 axb(nview), axc(nview), bxc(nview), d(nview), e(nview), f(nview)
  !
  real*4 errList(*), dxSum(nview), dySum(nview), dxsqSum(nview), dysqSum(nview)
  integer*4 nsum(nview)

  real*4 delDxyLast(2,nview), dxyLast(2,nview)
  real*4 am11, am12, am13, am21, am22, am23, am31, am32, am33, bsum, bv1, bv2, bv3
  real*4 cosTheta, csinTheta, del1, del2, delDx, delDy, delNew, det, diffMax, dxLast
  real*4 dxNew, dxTmp, dyLast, dyNew, dyTmp, errDiff, gcosPhi, gsinPhi, xpr, ypr
  integer*4 i, isolve, iv, iwatch, jpt, jumpBase, jumpFast, kpt, maxSolve, ncycSkip
  integer*4 ncycSkipLim, numRealPt, nview
  !
  ! precompute the a-f and relevant cross-products for the regression
  !
  iwatch = 0
  ! if (iwatch==0) iwatch=1
  do i = 1, nview
    cosTheta = cos(tilt(i))
    csinTheta = comp(i) * sin(tilt(i))
    gcosPhi = gmag(i) * cos(rot(i) + rotInc)
    gsinPhi = gmag(i) * sin(rot(i) + rotInc)
    !
    a(i) = cosTheta * gcosPhi
    b(i) = -gsinPhi
    c(i) = csinTheta * gcosPhi
    d(i) = cosTheta * gsinPhi
    e(i) = gcosPhi
    f(i) = csinTheta * gsinPhi
  enddo
    !
    ! the square and cross product terms for the x and y paired
    ! observations are always added together - so just combine them here
  call regressionCrossProducts(nview, a, b, c, d, e, f, asq, bsq, csq, axb, axc, bxc)
  !
  ! start looping until maxSolve times, or diffMax gets small, or error
  ! really blows up a lot from first iteration
  !
  isolve = 1
  diffMax = 1.
  jumpFast = 0
  jumpBase = 3
  ncycSkipLim = 5
  do while(isolve <= maxSolve .and. diffMax > 1.e-7 .and. &
      (isolve <= 3 .or. errList(max(1, isolve-1)) <= 1000. *errList(1)))
    !
    ! zero arrays for sums of xd, dy and squares for each view
    !
    do iv = 1, nview
      dxSum(iv) = 0.
      dySum(iv) = 0.
      dxsqSum(iv) = 0.
      dysqSum(iv) = 0.
      nsum(iv) = 0
    enddo
    !
    ! loop on each real point
    !
    do jpt = 1, numRealPt
      if (jpt < numRealPt) then
        call oneXYZbyRegression(a, b, c, d, e, f, asq, bsq, csq, axb, axc, bxc, dxy, xx, &
            yy, irealStart(jpt), irealStart(jpt + 1) - 1, isecView, bvec)
      else
        !
        ! get coordinates of last point as minus sum of all other points
        !
        do i = 1, 3
          bsum = 0.
          do kpt = 1, numRealPt - 1
            bsum = bsum - xyz(i, kpt)
          enddo
          bvec(i) = bsum
        enddo
      endif
      !
      ! store new x, y, z
      !
      do i = 1, 3
        xyz(i, jpt) = bvec(i)
      enddo
      !
      ! for each view that point is projected into, add its contribution
      ! to the dx and dy differences in that view
      !
      do i = irealStart(jpt), irealStart(jpt + 1) - 1
        iv = isecView(i)
        dxTmp = xx(i) - (a(iv) * bvec(1) + b(iv) * bvec(2) + c(iv) * bvec(3))
        dyTmp = yy(i) - (d(iv) * bvec(1) + e(iv) * bvec(2) + f(iv) * bvec(3))
        dxSum(iv) = dxSum(iv) + dxTmp
        dxsqSum(iv) = dxsqSum(iv) + dxTmp**2
        dySum(iv) = dySum(iv) + dyTmp
        dysqSum(iv) = dysqSum(iv) + dyTmp**2
        nsum(iv) = nsum(iv) + 1
      enddo
      !
    enddo
    !
    ! set new dx and dy equal to the average dx and dy for each view,
    ! and compute total error using the sums of squares gotten above
    !
    error = 0.
    diffMax = 0.
    do iv = 1, nview
      dxNew = dxSum(iv) / nsum(iv)
      dyNew = dySum(iv) / nsum(iv)
      delDx = dxNew - dxy(1, iv)
      delDy = dyNew - dxy(2, iv)
      diffMax = max(diffMax, abs(delDx), abs(delDy))
      dxy(1, iv) = dxNew
      dxy(2, iv) = dyNew
      !
      ! save info to make possible big jumps in dx and dy next time round
      !
      if (jumpFast == 1) then
        delDxyLast(1, iv) = delDx
        delDxyLast(2, iv) = delDy
        dxyLast(1, iv) = dxy(1, iv)
        dxyLast(2, iv) = dxy(2, iv)
      endif
      error = error + dxsqSum(iv) + dysqSum(iv) &
          - nsum(iv) * (dxy(1, iv)**2 + dxy(2, iv)**2)
    enddo
    !
    ! stick error on list, calculate difference from last error
    !
    errList(isolve) = error
    if (isolve > 1) errDiff = abs(errList(isolve) - errList(isolve-1)) &
        / max(errList(isolve), errList(isolve-1))
    isolve = isolve + 1
    !
    ! try to make big jumps in dx or dy; even this stuff doesn't make big
    ! data sets converge adequately
    !
    if (jumpFast == 2) then
      do iv = 1, nview
        do i = 1, 2
          ncycSkip = 1
          del2 = dxy(i, iv) - dxyLast(i, iv)
          del1 = delDxyLast(i, iv)
          if (del2 * del1 > 0 .and. abs(del2) < abs(del1)) then
            ncycSkip = ncycSkipLim
            if (abs(del1 - del2) > abs(del1) / &
                ncycSkipLim) ncycSkip = del1 / (del1 - del2)
          endif
          delNew = ncycSkip * del1 + (del2 - del1) * ncycSkip * (ncycSkip + 1) / 2
          dxy(i, iv) = dxyLast(i, iv) + delNew
        enddo
      enddo
    endif
    !
    if (isolve > jumpBase) jumpFast = mod(jumpFast + 1, 3)
    !
    if (iwatch > 0) then
      delDx = dxy(1, iwatch) - dxLast
      delDy = dxy(2, iwatch) - dyLast
      write(*,'(2i4,4f10.4,f20.5)') iwatch, isolve, dxy(1, iwatch) &
          , delDx, dxy(2, iwatch), delDy, error
      dxLast = dxy(1, iwatch)
      dyLast = dxy(2, iwatch)
    endif
  enddo
  !
  ! after convergence, recompute error de novo: this may give more
  ! accurate double precision result, or may be superfluous

  error = 0.
  do jpt = 1, numRealPt
    do i = irealStart(jpt), irealStart(jpt + 1) - 1
      iv = isecView(i)
      error = error + (a(iv) * xyz(1, jpt) + b(iv) * xyz(2, jpt) &
          + c(iv) * xyz(3, jpt) + dxy(1, iv) - xx(i))**2 &
          + (d(iv) * xyz(1, jpt) + e(iv) * xyz(2, jpt) &
          + f(iv) * xyz(3, jpt) + dxy(2, iv) - yy(i))**2
    enddo
  enddo
  !
  errList(isolve) = error
  ! write(*,'(4f20.5)') (errList(i), i=1, isolve)
  if (iwatch > 0) iwatch = mod(iwatch, nview) + 1
  return
end subroutine solve_xyzd_iter



! ***    init_dxy sets the initial values of dx and dy for each view:
! it sets dx and dy to 0 for the view with minimum tilt (IMINTILT),
! then sets the dx and dy of each view so that the points shared
! between adjacent views have the same center of gravity.
!
subroutine init_dxy(xx, yy, isecView, irealStart, nview, numRealPt, iminTilt, dxy, cgx, &
    cgy)
  implicit none
  real*4 xx(*), yy(*), dxy(2,*), cgx, cgy
  integer*4 isecView(*), irealStart(*), nview, numRealPt, iminTilt
  real*4 dxSum, dySum
  integer*4 ivStart, ivEnd, idir, iv, nsum, ipt, inOne, inTwo, i
  !
  ! consider each pair of views from the one with min tilt out to ends
  !
  dxy(1, iminTilt) = cgx
  dxy(2, iminTilt) = cgy
  ivStart = iminTilt
  ivEnd = 2
  do idir = -1, 1, 2
    do iv = ivStart, ivEnd, idir
      !
      ! find each real point in both views, add up disparities
      !
      dxSum = 0.
      dySum = 0.
      nsum = 0
      do ipt = 1, numRealPt
        inOne = 0
        inTwo = 0
        do i = irealStart(ipt), irealStart(ipt + 1) - 1
          if (isecView(i) == iv + idir) inOne = i
          if (isecView(i) == iv) inTwo = i
        enddo
        if (inOne * inTwo > 0) then
          dxSum = dxSum + xx(inTwo) - xx(inOne)
          dySum = dySum + yy(inTwo) - yy(inOne)
          nsum = nsum + 1
        endif
      enddo
      !
      ! adjust this section's dx, dy by the disparity
      !
      dxy(1, iv + idir) = dxy(1, iv) + dxSum / nsum
      dxy(2, iv + idir) = dxy(2, iv) + dySum / nsum
    enddo
    ivEnd = nview - 1
  enddo
  return
end subroutine init_dxy


! regressionCrossProducts takes the 6 factors of x, y, z and computes the cross-products
! and squares needed for quick regression
!
subroutine regressionCrossProducts(nview, a, b, c, d, e, f, asq, bsq, csq, axb, axc, bxc)
  implicit none
  integer*4 nview, i
  real*4 a(*), b(*), c(*), d(*), e(*), f(*), asq(*), bsq(*), csq(*), axb(*), axc(*),bxc(*)
  do i = 1, nview
    asq(i) = a(i)**2 + d(i)**2
    bsq(i) = b(i)**2 + e(i)**2
    csq(i) = c(i)**2 + f(i)**2
    axb(i) = a(i) * b(i) + d(i) * e(i)
    axc(i) = a(i) * c(i) + d(i) * f(i)
    bxc(i) = b(i) * c(i) + e(i) * f(i)
  enddo
  return
end subroutine regressionCrossProducts


! oneXYZbyRegression solves for the x, y, z coordinates of a single fiducial given the
! limits of its projection points, the set of all projection points, and the 6 factors and
! cross-products 
!
subroutine oneXYZbyRegression(a, b, c, d, e, f, asq, bsq, csq, axb, axc, bxc, dxy, xx, &
    yy, indStart, indEnd, isecView, bvec)
  implicit none
  integer*4 isecView(*), indStart, indEnd, i, iv
  real*4 a(*), b(*), c(*), d(*), e(*), f(*), asq(*), bsq(*), csq(*), axb(*), axc(*),bxc(*)
  real*4 dxy(2, *), bvec(3), xx(*), yy(*), ypr
  real*4 am11, am12, am13, am21, am22, am23, am31, am32, am33, det, bv1, bv2, bv3, xpr
  !
  ! zero the matrix elements for sums of squares and cross-products
  am11 = 0.
  am12 = 0.
  am13 = 0.
  am22 = 0.
  am23 = 0.
  am33 = 0.
  bv1 = 0.
  bv2 = 0.
  bv3 = 0.
  !
  ! for each projection of the real point, add terms to matrix
  do i = indStart, indEnd
    iv = isecView(i)
    xpr = xx(i) - dxy(1, iv)
    ypr = yy(i) - dxy(2, iv)
    am11 = am11 + asq(iv)
    am22 = am22 + bsq(iv)
    am33 = am33 + csq(iv)
    am12 = am12 + axb(iv)
    am13 = am13 + axc(iv)
    am23 = am23 + bxc(iv)
    bv1 = bv1 + a(iv) * xpr + d(iv) * ypr
    bv2 = bv2 + b(iv) * xpr + e(iv) * ypr
    bv3 = bv3 + c(iv) * xpr + f(iv) * ypr
  enddo
  !
  ! fill out lower triangle of matrix
  am31 = am13
  am21 = am12
  am32 = am23
  !
  ! solve it
  det = am11 * (am22 * am33 - am23 * am32) - am12 * (am21 * am33 - am23 * am31) &
      + am13 * (am21 * am32 - am22 * am31)
  bvec(1) = (bv1 * (am22 * am33 - am23 * am32) - am12 * (bv2 * am33 - am23 * bv3) &
      + am13 * (bv1 * am32 - am22 * bv3)) / det
  bvec(2) = (am11 * (bv2 * am33 - am23 * bv3) - bv1 * (am21 * am33 - am23 * am31) &
      + am13 * (am21 * bv3 - bv2 * am31)) / det
  bvec(3) = (am11 * (am22 * bv3 - bv2 * am32) - am12 * (am21 * bv3 - bv2 * am31) &
      + bv1 * (am21 * am32 - am22 * am31)) / det
  return
end subroutine oneXYZbyRegression

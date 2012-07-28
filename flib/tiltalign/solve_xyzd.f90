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
! 5 / 18 / 12: Replaced old solve_xyzd and init_dxy routines
!
! $Id$
!
! sprod should be allocated to at least (3 * numRealPt - 1) * (3 * numRealPt - 2) / 2
!
subroutine solveXyzd(xx, yy, isecView, irealStart, nview, numRealPt, tilt, rot, gmag, &
    comp, xyz, dxy, rotInc, sprod, error, ierr)
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
  ! print *,'Loaded matrix', numCols, numRows
  wallStart = wallTime()

  call solvePackedSums(sx, sprod, numCols, numRows, xml, sdl, bl, ierr)
  ! print *,'lapack', ierr
  ! write(*, '(3(2f9.4,f8.4))') (sdl(i), i = 1, numCols)
  ! write(*, '(3(2f9.4,f8.4))') (bl(i), i = 1, numCols - 1)
  ! write(*,'(a,f7.3, a, f7.3)') 'solution time', 1000. * (wallTime() - wallStart), &
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
  ! write(*,'(a,f7.1,a,f15.5)') 'Rotinc =', rotInc * 180 / 3.14159, ',  ERROR = ', error
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
    xm(i) = xm(i) / nrows
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

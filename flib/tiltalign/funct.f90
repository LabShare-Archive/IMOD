! ****   FUNCT performs the necessary tasks required by the METRO routine for
! conjugate gradient minimization.  For the current values of the
! all alignment variables such as rotation, tilt, and mag (which are
! obtained, whether they are fixed values or variables, by calling the
! routine REMAP_PARAMS), and the current values of the real - space (x, y,
! z) coordinates of the points (which are kept in the list VAR, after
! the "geometric" variables on that list), it computes the projection
! coordinates of each point, the best displacement dx, dy, for each
! view; the residuals for each point (computed minus measured
! projection coordinate), and the error term, which is the sum of the
! squares of the residuals.  It then computes the derivative of the
! error with respect to each variable and returns these derivatives in
! the array GRAD.  The derivatives with respect to the geometric
! variables are obtained from the following relations:
! _    xproj = a * x + b * y + c * z + dx(view)
! _    yproj = d * x + e * y + f * z + dy(view)
! where the six terms are a product of distortion, X - axis tilt, Y - axis
! tilt, projection, and image rotation matrices.  The latest version
! takes the derivative of one of component matrices and forms the
! product with that derivative and the rest of the matrices.
!
! The derivatives with respect to the coordinate variables are obtained
! from expressions relating the projection coordinates to the real -
! space (x, y, z) coordinates of all but the last point.  These
! expressions incorporate the constraint that the centroid of the
! (x, y, z) points is (0, 0, 0) and the fact that the total error will be a
! minimum when the centroid of the measured projection points in one
! view matches the centroid of the projection of the real - space
! points.  ANGLES ARE EXPECTED TO BE RADIANS.
!
! $Id$
!
module functVars
  implicit none
  !
  ! a, b, etc are the quantities in the above equations for each view
  ! aOverN is a over n (# of points in that view)
  ! aprime is derivative of a with respect to tilt angle
  !
  logical*1, allocatable :: realInView(:,:)
  real*4, allocatable :: xbar(:), ybar(:), xproj(:), yproj(:)
  real*4, allocatable :: xcen(:), ycen(:), zcen(:)
  real*4, allocatable :: a(:), b(:), c(:), d(:), e(:), f(:)
  real*4, allocatable :: aOverN(:), bOverN(:), cOverN(:), dOverN(:), eOverN(:), fOverN(:)
  real*4, allocatable :: cbeta(:), sbeta(:), calf(:), salf(:)
  real*4, allocatable :: cgam(:), sgam(:), cdel(:), sdel(:), xmag(:)
  !
  integer*2, allocatable :: indvReal(:,:)
  integer*4, allocatable :: nptInView(:), indvProj(:,:)
  real*4, allocatable :: coefx(:), coefy(:), resProd(:,:)
  real*4, allocatable :: dmat(:,:), xtmat(:,:), ytmat(:,:), rmat(:,:)
  real*4, allocatable :: beamInv(:,:), beamMat(:,:)

end module functVars

subroutine allocateFunctVars(ierr)
  use functVars
  use arrayMaxes
  implicit none
  integer*4 ierr, ms
  ms = maxView
  allocate(realInView(maxReal, maxView), xbar(ms), ybar(ms), xproj(maxProjPt), &
      yproj(maxProjPt), xcen(ms), ycen(ms), zcen(ms), a(ms), b(ms), c(ms), d(ms), e(ms), &
      f(ms), aOverN(ms), bOverN(ms), cOverN(ms), dOverN(ms), eOverN(ms), fOverN(ms), &
      cbeta(ms), sbeta(ms), calf(ms), salf(ms), cgam(ms), sgam(ms), cdel(ms), sdel(ms), &
      xmag(ms), indvReal( maxReal, maxView), nptInView(maxView), indvProj(maxReal, &
      maxView), coefx(3 * maxReal), coefy(3 * maxReal), resProd(6, maxProjPt), &
      dmat(9, ms), xtmat(9, ms), ytmat(9, ms), rmat(4, ms), beamInv(9, ms), &
      beamMat(6, ms), stat = ierr)
  return
end subroutine allocateFunctVars

subroutine funct(nvarSearch, var, ferror, grad)
  !
  use alivar
  use functVars
  implicit none
  !
  real*4 grad(*), var(*), ferror
  integer*4 nvarSearch
  !
  double precision error, gradSum, errorMin
  !
  real*4 dermat(9)
  real*4 projMat(4), umat(9)
  !
  integer*4 numProjPt, iv, jpt, i, nvmat, icoordBas, kxlas, kylas, kzlas
  integer*4 kz, kx, ky, ipt, ivar, iptInV, jx, jy, jz, iy, ix, kpt, jj, istrType
  real*4 afac, bfac, cfac, dfac, efac, ffac, xpxRlast, xpyRlast, xpzRlast, ypxRlast
  real*4 ypyRlast, ypzRlast, valAdd, cosPSkew, sinPSkew, cosBeam, sinBeam
  real*4 cos2rot, sin2rot
  character*2 star
  save errorMin
  real*8 gradientSum
  !
  ! Stretch type: 1 for dmag = stretch on X axis, skew = X axis rotation
  ! 2 for dmag = stretch on X axis, skew = +Y - axis and - X - axis rotation
  ! 3 for dmag = +Y - axis and - X - axis stretch, skew = +Y - axis and - X - axis
  ! rotation
  ! Since the X - stretch frequently occurs because of thinning, the third
  ! formulation would often affect mag inappropriately.
  ! Neither the second nor the third seemed to reduce solved rotation
  !
  istrType = 1
  !
  ! first time in, precompute the mean projection coords in each view
  ! and build indexes to the points in each view.
  !
  numProjPt = irealStr(nrealPt + 1) - 1
  if (firstFunct) then
    xbar(1:nview) = 0.
    ybar(1:nview) = 0.
    if (.not. robustWeights) weight(1:numProjPt) = 1.
    nptInView(1:nview) = 0
    realInView(1:nrealPt, 1:nview) = .false.
    !
    do jpt = 1, nrealPt
      do i = irealStr(jpt), irealStr(jpt + 1) - 1
        iv = isecView(i)
        nptInView(iv) = nptInView(iv) + 1
        indvReal(nptInView(iv), iv) = jpt
        indvProj(nptInView(iv), iv) = i
        realInView(jpt, iv) = .true.
        xbar(iv) = xbar(iv) + xx(i)
        ybar(iv) = ybar(iv) + yy(i)
      enddo
    enddo
    !
    do iv = 1, nview
      xbar(iv) = xbar(iv) / nptInView(iv)
      ybar(iv) = ybar(iv) / nptInView(iv)
    enddo
    firstFunct = .false.
!!$      call remap_params(var)
!!$      do iv = 1, nview
!!$      write(*,113) rot(iv), mapRot(iv), linRot(iv), frcRot(iv), &
!!$                     tilt(iv), mapTilt(iv), linTilt(iv), frcTilt(iv), &
!!$                     gmag(iv), mapGmag(iv), linGmag(iv), frcGmag(iv)
!!$      113     format(f11.7,2i4,f6.3,f11.7,2i4,f6.3,f11.7,2i4,f6.3)
!!$      enddo
!!$      print *,nvarSearch
!!$      write(*,'(7f11.7)') (var(i), i = 1, nvarSearch)
    errorMin = 1.e37
  endif
  !
  ! precompute the a - f and items related to them.  Store the component
  ! matrices to use for computing gradient coefficients
  !
  call remap_params(var)
  !
  call fill_proj_matrix(projStrRot, projSkew, projMat, cosPSkew, &
      sinPSkew, cos2rot, sin2rot)
  call fill_beam_matrices(beamTilt, beamInv, beamMat, cosBeam, sinBeam)
  do i = 1, nview
    xmag(i) = gmag(i) + dmag(i)
    call fill_dist_matrix(gmag(i), dmag(i), skew(i), comp(i), &
        istrType, dmat(1, i), cdel(i), sdel(i))
    call fill_xtilt_matrix(alf(i), ifAnyAlf, xtmat(1, i), calf(i), &
        salf(i))
    call fill_ytilt_matrix(tilt(i), ytmat(1, i), cbeta(i), sbeta(i))
    call fill_rot_matrix(rot(i), rmat(1, i), cgam(i), sgam(i))
    call matrix_to_coef(dmat(1, i), xtmat(1, i), beamInv, ytmat(1, i), &
        beamMat, projMat, rmat(1, i), a(i), b(i), c(i), d(i), e(i), f(i))
  enddo
  !
  do i = 1, nview
    aOverN(i) = -a(i) / nptInView(i)
    bOverN(i) = -b(i) / nptInView(i)
    cOverN(i) = -c(i) / nptInView(i)
    dOverN(i) = -d(i) / nptInView(i)
    eOverN(i) = -e(i) / nptInView(i)
    fOverN(i) = -f(i) / nptInView(i)
  enddo
  !
  ! s0 = secnds(0.)
  nvmat = 3 * (nrealPt - 1)                       !# of x, y, z variables
  icoordBas = nvarSearch - nvmat                  !offset to x, y, z's
  kzlas = icoordBas + nrealPt * 3                 !indexes of x, y, z of last point
  kylas = kzlas - 1
  kxlas = kylas - 1
  !
  ! get xproj and yproj: for now, these will be projected points minus
  ! the dx, dy values
  ! compute the coordinates of the last point: minus the sum of the rest
  !
  do iv = 1, nview
    xcen(iv) = 0.
    ycen(iv) = 0.
    zcen(iv) = 0.
  enddo
  var(kxlas) = 0.
  var(kylas) = 0.
  var(kzlas) = 0.
  !
  do jpt = 1, nrealPt
    kz = icoordBas + jpt * 3
    ky = kz - 1
    kx = ky - 1
    !
    if (jpt < nrealPt) then                  !accumulate last point coords
      var(kxlas) = var(kxlas) - var(kx)
      var(kylas) = var(kylas) - var(ky)
      var(kzlas) = var(kzlas) - var(kz)
    endif
    !
    xyz(1, jpt) = var(kx)                      !unpack the coordinates
    xyz(2, jpt) = var(ky)
    xyz(3, jpt) = var(kz)
    !
    do i = irealStr(jpt), irealStr(jpt + 1) - 1
      iv = isecView(i)
      xproj(i) = a(iv) * var(kx) + b(iv) * var(ky) + c(iv) * var(kz)
      yproj(i) = d(iv) * var(kx) + e(iv) * var(ky) + f(iv) * var(kz)
      xcen(iv) = xcen(iv) + var(kx)
      ycen(iv) = ycen(iv) + var(ky)
      zcen(iv) = zcen(iv) + var(kz)
    enddo
  enddo
  !
  ! get xcen, ycen, zcen scaled, and get the dx and dy
  !
  do iv = 1, nview
    xcen(iv) = xcen(iv) / nptInView(iv)
    ycen(iv) = ycen(iv) / nptInView(iv)
    zcen(iv) = zcen(iv) / nptInView(iv)
    dxy(1, iv) = xbar(iv) &
        - a(iv) * xcen(iv) - b(iv) * ycen(iv) - c(iv) * zcen(iv)
    dxy(2, iv) = ybar(iv) &
        - d(iv) * xcen(iv) - e(iv) * ycen(iv) - f(iv) * zcen(iv)
  enddo
  !
  ! adjust xproj&yproj by dxy, get residuals and errors
  !
  error = 0.
  do i = 1, numProjPt
    iv = isecView(i)
    xproj(i) = xproj(i) + dxy(1, iv)
    yproj(i) = yproj(i) + dxy(2, iv)
    xresid(i) = xproj(i) - xx(i)
    yresid(i) = yproj(i) - yy(i)
    error = error + (xresid(i)**2 + yresid(i)**2) * weight(i)
  enddo
  !
  ! precompute products needed for gradients
  !
  do iv = 1, nview
    do iptInV = 1, nptInView(iv)
      ipt = indvProj(iptInV, iv)
      jpt = indvReal(iptInV, iv)
      resProd(1, ipt) = 2. * (xyz(1, jpt) - xcen(iv)) * xresid(ipt) * weight(ipt)
      resProd(2, ipt) = 2. * (xyz(2, jpt) - ycen(iv)) * xresid(ipt) * weight(ipt)
      resProd(3, ipt) = 2. * (xyz(3, jpt) - zcen(iv)) * xresid(ipt) * weight(ipt)
      resProd(4, ipt) = 2. * (xyz(1, jpt) - xcen(iv)) * yresid(ipt) * weight(ipt)
      resProd(5, ipt) = 2. * (xyz(2, jpt) - ycen(iv)) * yresid(ipt) * weight(ipt)
      resProd(6, ipt) = 2. * (xyz(3, jpt) - zcen(iv)) * yresid(ipt) * weight(ipt)
    enddo
  enddo

  ! Keep track of minimum error for trace output
  ferror = error
  star = ''
  if (error < errorMin) then
    errorMin = error
    star = ' *'
  endif
  ! write(*,'(f55.15,a)') error,star
  !
  ! compute derivatives of error w / r to search parameters
  ! first clear out all the gradients
  !
  do ivar = 1, nvarSearch
    grad(ivar) = 0.
  enddo
  !
  ! loop on views: consider each of the parameters
  !
  ivar = 0
  do iv = 1, nview
    !
    ! rotation: add gradient for this view to any variables that it is
    ! mapped to
    ! These equations are valid as long as rotation is the last operation
    !
    gradSum = 0.
    if (mapRot(iv) > 0) then
      do iptInV = 1, nptInView(iv)
        ipt = indvProj(iptInV, iv)
        gradSum = gradSum + 2. * weight(ipt) * &
            ((ybar(iv) - yproj(ipt)) * xresid(ipt) &
            + (xproj(ipt) - xbar(iv)) * yresid(ipt))
      enddo
      grad(mapRot(iv)) = grad(mapRot(iv)) + frcRot(iv) * gradSum
      if (linRot(iv) > 0) grad(linRot(iv)) = grad(linRot(iv)) + &
          (1. - frcRot(iv)) * gradSum
    endif
    !
    ! tilt: add gradient for this tilt angle to the variable it is mapped
    ! from, if any
    !
    if (mapTilt(iv) .ne. 0) then
      call zero_matrix(dermat, 9)
      dermat(1) = -sbeta(iv)
      dermat(3) = cbeta(iv)
      dermat(7) = -cbeta(iv)
      dermat(9) = -sbeta(iv)

      call matrix_to_coef(dmat(1, iv), xtmat(1, iv), beamInv, dermat, beamMat, &
          projMat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      grad(mapTilt(iv)) = grad(mapTilt(iv)) + frcTilt(iv) * gradSum
      if (linTilt(iv) > 0) grad(linTilt(iv)) = grad(linTilt(iv)) + &
          (1. - frcTilt(iv)) * gradSum
      !
    endif
    !
    ! mag: add gradient for this view to the variable it is mapped from
    !
    if (mapGmag(iv) > 0) then
      call zero_matrix(dermat, 9)
      if (istrType == 1) then
        dermat(1) = cdel(iv)
        dermat(4) = sdel(iv)
        dermat(5) = 1.
      else
        dermat(1) = cdel(iv)
        dermat(2) = -sdel(iv)
        dermat(4) = -sdel(iv)
        dermat(5) = cdel(iv)
      endif
      dermat(9) = comp(iv)
      call matrix_to_coef(dermat, xtmat(1, iv), beamInv, ytmat(1, iv), beamMat, &
          projMat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      ! write(*,'(i4,3f9.5,f16.10)') iv, gmag(iv), dmag(iv), skew(iv), gradSum
      grad(mapGmag(iv)) = grad(mapGmag(iv)) + frcGmag(iv) * gradSum
      if (linGmag(iv) > 0) grad(linGmag(iv)) = grad(linGmag(iv)) + &
          (1. - frcGmag(iv)) * gradSum
    endif
    !
    ! comp: add gradient for this view to the variable it is mapped from
    ! These equation are valid as long as the there is nothing else in
    ! the final column of the distortion matrix
    !
    if (mapComp(iv) > 0) then
      gradSum = 0.
      cfac = c(iv) / comp(iv)
      ffac = f(iv) / comp(iv)
      do iptInV = 1, nptInView(iv)
        ipt = indvProj(iptInV, iv)
        gradSum = gradSum +  cfac * resProd(3, ipt) + ffac * resProd(6, ipt)
      enddo
      grad(mapComp(iv)) = grad(mapComp(iv)) + frcComp(iv) * gradSum
      if (linComp(iv) > 0) grad(linComp(iv)) = grad(linComp(iv)) + &
          (1. - frcComp(iv)) * gradSum
    endif
    !
    ! dmag: add gradient for this view to the variable it is mapped from
    !
    if (mapDmag(iv) > 0) then
      call zero_matrix(dermat, 9)
      if (istrType == 1) then
        dermat(1) = cdel(iv)
        dermat(4) = sdel(iv)
      else if (istrType == 2) then
        dermat(1) = cdel(iv)
        dermat(4) = -sdel(iv)
      else
        dermat(1) = -cdel(iv)
        dermat(2) = -sdel(iv)
        dermat(4) = sdel(iv)
        dermat(5) = cdel(iv)
      endif
      call matrix_to_coef(dermat, xtmat(1, iv), beamInv, ytmat(1, iv), beamMat, &
          projMat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      !
      ! if this parameter maps to the dummy dmag, then need to subtract
      ! the fraction times the gradient sum from gradient of every real
      ! variable
      !
      if (mapDmag(iv) == mapDumDmag .or. linDmag(iv) == mapDumDmag) &
          then
        if (mapDmag(iv) == mapDumDmag) then
          valAdd = dumDmagFac * frcDmag(iv) * gradSum
          if (linDmag(iv) > 0) grad(linDmag(iv)) = &
              grad(linDmag(iv)) + (1. - frcDmag(iv)) * gradSum
        else
          valAdd = dumDmagFac * (1. - frcDmag(iv)) * gradSum
          grad(mapDmag(iv)) = grad(mapDmag(iv)) + frcDmag(iv) * gradSum
        endif
        do jj = mapDmagStart, mapDumDmag - 1
          grad(jj) = grad(jj) + valAdd
        enddo
      else
        grad(mapDmag(iv)) = grad(mapDmag(iv)) + frcDmag(iv) * gradSum
        if (linDmag(iv) > 0) grad(linDmag(iv)) = grad(linDmag(iv)) + &
            (1. - frcDmag(iv)) * gradSum
      endif
    endif
    !
    ! skew: add gradient for this view to the variable it is mapped from
    !
    if (mapSkew(iv) > 0) then
      call zero_matrix(dermat, 9)
      if (istrType == 1) then
        dermat(1) = -xmag(iv) * sdel(iv)
        dermat(4) = xmag(iv) * cdel(iv)
      else if (istrType == 2) then
        dermat(1) = -xmag(iv) * sdel(iv)
        dermat(2) = -gmag(iv) * cdel(iv)
        dermat(4) = -xmag(iv) * cdel(iv)
        dermat(5) = -gmag(iv) * sdel(iv)
      else
        dermat(1) = -(gmag(iv) - dmag(iv)) * sdel(iv)
        dermat(2) = -xmag(iv) * cdel(iv)
        dermat(4) = -(gmag(iv) - dmag(iv)) * cdel(iv)
        dermat(5) = -xmag(iv) * sdel(iv)
      endif
      call matrix_to_coef(dermat, xtmat(1, iv), beamInv, ytmat(1, iv), beamMat, &
          projMat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      grad(mapSkew(iv)) = grad(mapSkew(iv)) + frcSkew(iv) * gradSum
      if (linSkew(iv) > 0) grad(linSkew(iv)) = grad(linSkew(iv)) + &
          (1. - frcSkew(iv)) * gradSum
      !
    endif
    !
    ! alpha: add gradient for this view to the variable it is mapped from
    !
    if (mapAlf(iv) > 0) then
      call zero_matrix(dermat, 9)
      dermat(5) = -salf(iv)
      dermat(6) = -calf(iv)
      dermat(8) = calf(iv)
      dermat(9) = -salf(iv)
      call matrix_to_coef(dmat(1, iv), dermat, beamInv, ytmat(1, iv), beamMat, &
          projMat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      ! write(*,'(3i4,f7.4,f16.10)') iv, mapAlf(iv), linAlf(iv) &
      ! , frcAlf(iv), gradSum
      grad(mapAlf(iv)) = grad(mapAlf(iv)) + frcAlf(iv) * gradSum
      if (linAlf(iv) > 0) grad(linAlf(iv)) = grad(linAlf(iv)) + &
          (1. - frcAlf(iv)) * gradSum
    endif
  enddo
  !
  ! projection skew: do gradient
  !
  if (mapProjStretch > 0) then
    !
    dermat(1) = -sinPSkew + cosPSkew * sin2rot
    dermat(2) = -cosPSkew * cos2rot
    dermat(3) = dermat(2)
    dermat(4) = -sinPSkew - cosPSkew * sin2rot
    do iv = 1 , nview
      call matrix_to_coef(dmat(1, iv), xtmat(1, iv), beamInv, ytmat(1, iv), &
          beamMat, dermat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      grad(mapProjStretch) = grad(mapProjStretch) + gradSum
    enddo
  endif
  !
  ! beam tilt: the derivative is of the product of beamInv, ytmat and
  ! beamMat, so pass two unit matrices and derivative
  !
  if (mapBeamTilt > 0) then
    dermat(1) = 0.
    call zero_matrix(umat, 9)
    umat(1) = 1.
    umat(5) = 1.
    umat(9) = 1.
    do iv = 1, nview
      dermat(2) = -cosBeam * sbeta(iv)
      dermat(3) = -sinBeam * sbeta(iv)
      dermat(4) = cosBeam * sbeta(iv)
      dermat(5) = 2. * cosBeam * sinBeam * (cbeta(iv) - 1.)
      dermat(6) = (cosBeam**2 - sinBeam**2) * (1 - cbeta(iv))
      call matrix_to_coef(dmat(1, iv), xtmat(1, iv), umat, umat, &
          dermat, projMat, rmat(1, iv), afac, bfac, cfac, dfac, efac, ffac)
      gradSum = gradientSum(indvProj(1, iv), nptInView(iv), resProd, &
          afac, bfac, cfac, dfac, efac, ffac)
      grad(mapBeamTilt) = grad(mapBeamTilt) + gradSum
    enddo
  endif
  !
  ! loop on points, get derivatives w / r to x, y, or z
  !
  if (xyzFixed) return
  do jpt = 1, nrealPt
    jz = 3 * jpt
    jy = jz - 1
    jx = jy - 1
    iy = 0
    !
    ! for each projection of the real point, find how that point
    ! contributes to the derivative w / r to each of the x, y, z
    !
    do i = irealStr(jpt), irealStr(jpt + 1) - 1
      iv = isecView(i)
      iy = iy + 2
      ix = iy - 1
      !
      ! the relation between the projection (x, y) and the set of (x, y, z)
      ! contains the term dxy, which is actually a sum of the (x, y, z) .
      ! There is a 3 by 3 matrix of possibilities: the first set of 3
      ! possibilities is whether this real point is the last one, or
      ! whether the last point is projected in this view or not.
      !
      if (jpt == nrealPt) then
        xpxRlast = -a(iv)
        xpyRlast = -b(iv)
        xpzRlast = -c(iv)
        ypxRlast = -d(iv)
        ypyRlast = -e(iv)
        ypzRlast = -f(iv)
      elseif (realInView(nrealPt, iv)) then
        xpxRlast = 0.
        xpyRlast = 0.
        xpzRlast = 0.
        ypxRlast = 0.
        ypyRlast = 0.
        ypzRlast = 0.
      else
        xpxRlast = -aOverN(iv)
        xpyRlast = -bOverN(iv)
        xpzRlast = -cOverN(iv)
        ypxRlast = -dOverN(iv)
        ypyRlast = -eOverN(iv)
        ypzRlast = -fOverN(iv)
      endif
      !
      ! The second set of three possibilities is whether the point whose
      ! coordinate that we are taking the derivative w / r to is the same
      ! as the real point whose projections are being considered
      ! (kpt == jpt), and whether the former point is or is not projected
      ! in the view being considered.
      !
      do kpt = 1, nrealPt - 1
        kz = 3 * kpt
        ky = kz - 1
        kx = ky - 1
        !
        if (kpt == jpt) then
          coefx(kx) = a(iv) + xpxRlast
          coefx(ky) = b(iv) + xpyRlast
          coefx(kz) = c(iv) + xpzRlast
          coefy(kx) = d(iv) + ypxRlast
          coefy(ky) = e(iv) + ypyRlast
          coefy(kz) = f(iv) + ypzRlast
        elseif (realInView(kpt, iv)) then
          coefx(kx) = xpxRlast
          coefx(ky) = xpyRlast
          coefx(kz) = xpzRlast
          coefy(kx) = ypxRlast
          coefy(ky) = ypyRlast
          coefy(kz) = ypzRlast
        else
          coefx(kx) = aOverN(iv) + xpxRlast
          coefx(ky) = bOverN(iv) + xpyRlast
          coefx(kz) = cOverN(iv) + xpzRlast
          coefy(kx) = dOverN(iv) + ypxRlast
          coefy(ky) = eOverN(iv) + ypyRlast
          coefy(kz) = fOverN(iv) + ypzRlast
        endif
        !
      enddo
      !
      ! The coefficients directly yield derivatives
      !
      do ivar = 1, nvmat
        grad(ivar + icoordBas) = grad(ivar + icoordBas) + &
            2. * weight(i) * (xresid(i) * coefx(ivar) + yresid(i) * coefy(ivar))
      enddo
      !
    enddo
  enddo
  !
  ! write(*,'(i4,2f16.10)') (i, var(i), grad(i), i = 1, nvarSearch)
  return
end subroutine funct


! GRADIENTSUM forms the standard gradient sum over the points
! in a view for the given factors AFAC - FFAC
! indvProj has indices from point in view to residual products in resProd
! nptInView is the number of points in the VIEW
!
real*8 function gradientSum(indvProj, nptInView, resProd, &
    afac, bfac, cfac, dfac, efac, ffac)
  implicit none
  integer*4 indvProj(*), nptInView, iptInV, ipt
  real*4 resProd(6,*), afac, bfac, cfac, dfac, efac, ffac
  real*8 gradSum
  gradSum = 0.
  do iptInV = 1, nptInView
    ipt = indvProj(iptInV)
    gradSum = gradSum + afac * resProd(1, ipt) + bfac * resProd(2, ipt) &
        + cfac * resProd(3, ipt) + dfac * resProd(4, ipt) + &
        efac * resProd(5, ipt) + ffac * resProd(6, ipt)
  enddo
  gradientSum = gradSum
  return
end function gradientSum


! ***    REMAP_PARAMS returns the complete set of geometric variables based
! on the current values of the search parameters.

subroutine remap_params(varList)
  use alivar
  implicit none
  !
  real*4 varList(*)
  real*4 sum, varSave
  integer*4 i
  !
  ! 4 / 10 / 05: eliminated global rotation, it is just like the others now
  ! so extra arguments to map_one_var were removed
  !
  call map_one_var(varList, rot, mapRot, frcRot, linRot, fixedRot, &
      nview, glbRot, incrRot)
  !
  do i = 1, nview
    if (mapTilt(i) > 0) then
      if (linTilt(i) > 0) then
        tilt(i) = frcTilt(i) * varList(mapTilt(i)) + (1. - frcTilt(i))* &
            varList(linTilt(i)) + tiltInc(i)
      elseif (linTilt(i) == - 1) then
        tilt(i) = frcTilt(i) * varList(mapTilt(i)) + (1. - frcTilt(i))* &
            fixedTilt + tiltInc(i)
      elseif (linTilt(i) == - 2) then
        tilt(i) = frcTilt(i) * varList(mapTilt(i)) + (1. - frcTilt(i))* &
            fixedTilt2 + tiltInc(i)
      else
        tilt(i) = varList(mapTilt(i)) + tiltInc(i)
      endif
    endif
  enddo
  !
  call map_one_var(varList, gmag, mapGmag, frcGmag, linGmag, fixedGmag, &
      nview, glbGmag, incrGmag)
  !
  call map_one_var(varList, comp, mapComp, frcComp, linComp, fixedComp, &
      nview, glbGmag, 0)
  !
  call map_one_var(varList, skew, mapSkew, frcSkew, linSkew, fixedSkew, &
      nview, glbSkew, incrSkew)
  !
  if (mapDumDmag > mapDmagStart) then
    !
    ! if there are any dmag variables, the dummy variable is some factor
    ! times the sum of the real variables.  Save that position on
    ! varList, put the value there, and compose all of the view
    ! parameters as usual
    !
    sum = 0.
    do i = mapDmagStart, mapDumDmag - 1
      sum = sum + varList(i)
    enddo
    varSave = varList(mapDumDmag)
    varList(mapDumDmag) = dumDmagFac * sum
  endif
  !
  call map_one_var(varList, dmag, mapDmag, frcDmag, linDmag, fixedDmag, &
      nview, glbDmag, incrDmag)
  if (mapDumDmag > mapDmagStart) varList(mapDumDmag) = varSave
  !
  if (ifAnyAlf .ne. 0) call map_one_var(varList, alf, mapAlf, frcAlf, &
      linAlf, fixedAlf, nview, glbAlf, incrAlf)
  !
  if (mapProjStretch > 0) then
    ! projStretch = varList(mapProjStretch)
    projSkew = varList(mapProjStretch)
  endif
  if (mapBeamTilt > 0) beamTilt = varList(mapBeamTilt)
  return
end subroutine remap_params



subroutine map_one_var(varList, val, map, frc, lin, fixed, nview, glb, &
    incr)
  implicit none
  real*4 varList(*), val(*), frc(*), glb(*), fixed
  integer*4 map(*), lin(*), nview, incr
  integer*4 i
  do i = 1, nview
    if (map(i) > 0) then
      if (lin(i) > 0) then
        val(i) = frc(i) * varList(map(i)) + (1. - frc(i)) * varList(lin(i))
      elseif (lin(i) < 0) then
        val(i) = frc(i) * varList(map(i)) + (1. - frc(i)) * fixed
      else
        val(i) = varList(map(i))
      endif
      if (incr .ne. 0) val(i) = val(i) + glb(i)
    endif
  enddo
  return
end subroutine map_one_var


! MATRIX_TO_COEF takes the distortion matrix DIST, x - axis tilt matrix
! XTILT, Y axis tilt matrix yTilt, projection stretch matrix projStr,
! and rotation matrix ROT, and computes the 6 components of the 2x3
! product in A, B, C, D, E, F
!
subroutine matrix_to_coef(dist, xtilt, beamInv, yTilt, beamMat, &
    projStr, rot, a, b, c, d, e, f)
  implicit none
  real*4 dist(*), xtilt(*), yTilt(*), rot(*), projStr(*), beamInv(*)
  real*4 beamMat(*), a, b, c, d, e, f
  real*8 tmp(9)
  integer*4 i

  do i = 1, 9
    tmp(i) = dist(i)
  enddo
  call mat_product(tmp, 3, 3, xtilt, 3, 3)
  call mat_product(tmp, 3, 3, beamInv, 3, 3)
  call mat_product(tmp, 3, 3, yTilt, 3, 3)
  call mat_product(tmp, 3, 3, beamMat, 2, 3)
  call mat_product(tmp, 2, 3, rot, 2, 2)
  call mat_product(tmp, 2, 3, projStr, 2, 2)
  a = tmp(1)
  b = tmp(2)
  c = tmp(3)
  d = tmp(4)
  e = tmp(5)
  f = tmp(6)
  return
end subroutine matrix_to_coef


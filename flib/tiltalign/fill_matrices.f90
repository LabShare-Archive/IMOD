! fill_matrices has routines for setting up a matrix for each of the
! individual changes.  It also contains a routine for converting alpha,
! tilt, and rotation for beam tilt.
!
! $Id$
!
!
! ZERO_MATRIX fills a matrix with zeros
!
subroutine zero_matrix(rmat, n)
  implicit none
  real*4 rmat(*)
  integer*4 n, i
  do i = 1, n
    rmat(i) = 0.
  enddo
  return
end subroutine zero_matrix

! MAT_PRODUCT takes the product of RMAT, a nmRows x nmCols matrix,
! and PROD, a npRows x npCols matrix applied first, and places the
! resulting nmRows x npCols matrix back into PROD
! Matrices must be organized to progress across rows
!
subroutine mat_product(prod, npRows, npCols, rmat, nmRows, nmCols)
  implicit none
  integer*4 npRows, npCols, nmRows, nmCols
  real*8 prod(npCols, npRows)
  real*4 rmat(nmCols, nmRows)
  real*8 tmp(3,3)
  integer*4 irow, icol, i
  do irow = 1, nmRows
    do icol = 1, npCols
      tmp(icol, irow) = 0.
      do i = 1, nmCols
        tmp(icol, irow) = tmp(icol, irow) + rmat(i, irow) * prod(icol, i)
      enddo
    enddo
  enddo
  do irow = 1, nmRows
    do icol = 1, npCols
      prod(icol, irow) = tmp(icol, irow)
    enddo
  enddo
  return
end subroutine mat_product

! CONVERT_FOR_BEAMTILT converts the given alpha, tilt angle, and
! rotation for the beam tilt angle.  alpha, tilt, and rot are assumed to
! be in degrees while beamTilt must be in radians

subroutine convert_for_beamtilt(alpha, tilt, rot, beamTilt, ifAnyAlpha)
  implicit none
  real*4 alpha, tilt, rot, beamTilt
  integer*4 ifAnyAlpha, i, j
  real*4 beamMat(9), beamInv(9), dmat(9), xtmat(9), ytmat(9), rmat(9), angles(3)
  real*4 cosAlpha, sinAlpha, cosBeta, sinBeta, cosBeam, sinBeam, cosTmp, sinTmp
  real*8 pmat(9)
  real*4 dtor/0.0174532/
  !
  ! Get a matrix that includes X tilt, beam tilt, tilt, and rotation, then
  ! derive 3 rotation angles from that
  call fill_beam_matrices(beamTilt, beamInv, beamMat, cosBeam, sinBeam)
  beamMat(7) = 0.
  beamMat(8) = sinBeam
  beamMat(9) = cosBeam
  call fill_xtilt_matrix(alpha * dtor, ifAnyAlpha, xtmat, cosAlpha, sinAlpha)
  call fill_ytilt_matrix(tilt * dtor, ytmat, cosBeta, sinBeta)
  call zero_matrix(rmat, 9)
  call fill_rot_matrix(rot * dtor, rmat, cosTmp, sinTmp)
  rmat(3) = 0.
  rmat(4) = sinTmp
  rmat(5) = cosTmp
  rmat(9) = 1.
  do i = 1, 9
    pmat(i) = xtmat(i)
  enddo
  call mat_product(pmat, 3, 3, beamInv, 3, 3)
  call mat_product(pmat, 3, 3, ytmat, 3, 3)
  call mat_product(pmat, 3, 3, beamMat, 3, 3)
  call mat_product(pmat, 3, 3, rmat, 3, 3)
  !
  ! transpose the matrix to go into the stock routines
  !
  do i = 1, 3
    do j = 1, 3
      xtmat(i + 3 * (j - 1)) = pmat(j + 3 * (i - 1))
    enddo
  enddo
  call inv_matrix(xtmat, ytmat)
  call icalc_angles(angles, ytmat)
  ! write(*,'(i4,3(2f9.2,3x))') iv, rot, -angles(3), tilt, &
  ! - angles(2), alpha, -angles(1)
  !
  ! replace the angles - then assume beam tilt 0 below
  !
  rot = -angles(3)
  tilt = -angles(2)
  alpha = -angles(1)
  return
end subroutine convert_for_beamtilt


! ROUTINES FOR FILLING MATRICES: they all fill a matrix of a defined
! geometry for the particular transformation, take angles in radians,
! and return cosine and sine of the relevant angle

! FILL_DIST_MATRIX fills a 3x3 matrix DMAT for in - plane distortions,
! given mag GMAG, x - stretch DMAG, X - axis rotation angle SKEW,
! compression COMP, and stretch type istrType
!
subroutine fill_dist_matrix(gmag, dmag, skew, comp, istrType, &
    dmat, cosDelta, sinDelta)
  implicit none
  real*4 gmag, dmag, skew, comp, dmat(*), cosDelta, sinDelta, xmag
  integer*4 istrType

  xmag = gmag + dmag
  cosDelta = cos(skew)
  sinDelta = sin(skew)
  call zero_matrix(dmat, 9)
  if (istrType == 1) then
    dmat(1) = xmag * cosDelta
    dmat(4) = xmag * sinDelta
    dmat(5) = gmag
  else if (istrType == 2) then
    dmat(1) = xmag * cosDelta
    dmat(2) = -xmag * sinDelta
    dmat(4) = -gmag * sinDelta
    dmat(5) = gmag * cosDelta
  else
    dmat(1) = (gmag - dmag) * cosDelta
    dmat(2) = -xmag * sinDelta
    dmat(4) = -(gmag - dmag) * sinDelta
    dmat(5) = xmag * cosDelta
  endif
  dmat(9) = gmag * comp
  return
end subroutine fill_dist_matrix

! FILL_XTILT_MATRIX fills 3x3 matrix XTMAT given X - axis tilt angle alpha,
! if ifAnyAlpha is not zero; otherwise it assumes alpha = 0
!
subroutine fill_xtilt_matrix(alpha, ifAnyAlpha, xtmat, cosAlpha, sinAlpha)
  implicit none
  real*4 alpha, xtmat(*), cosAlpha, sinAlpha
  integer*4 ifAnyAlpha
  !
  call zero_matrix(xtmat, 9)
  xtmat(1) = 1.
  xtmat(5) = 1.
  xtmat(9) = 1.
  if (ifAnyAlpha .ne. 0) then
    cosAlpha = cos(alpha)
    sinAlpha = sin(alpha)
    xtmat(5) = cosAlpha
    xtmat(6) = -sinAlpha
    xtmat(8) = sinAlpha
    xtmat(9) = cosAlpha
  else
    cosAlpha = 1.
    sinAlpha = 0.
  endif
  return
end subroutine fill_xtilt_matrix

! FILL_BEAM_MATRICES fills 3x3 matrix beamInv and 2x3 matrix beamMat
! given beam inclination angle beamTilt
!
subroutine fill_beam_matrices(beamTilt, beamInv, beamMat, cosBeam, sinBeam)
  implicit none
  real*4 beamMat(*), beamInv(*), cosBeam, sinBeam, beamTilt
  !
  call zero_matrix(beamInv, 9)
  call zero_matrix(beamMat, 6)

  beamInv(1) = 1.
  beamMat(1) = 1.
  cosBeam = cos(beamTilt)
  sinBeam = sin(beamTilt)
  beamInv(5) = cosBeam
  beamInv(6) = sinBeam
  beamInv(8) = -sinBeam
  beamInv(9) = cosBeam
  beamMat(5) = cosBeam
  beamMat(6) = -sinBeam
  return
end subroutine fill_beam_matrices

! FILL_YTILT_MATRIX fills 3x3 matrix YTMAT given tilt angle TILT
!
subroutine fill_ytilt_matrix(tilt, ytmat, cosBeta, sinBeta)
  implicit none
  real*4 tilt, ytmat(*), cosBeta, sinBeta
  !
  call zero_matrix(ytmat, 9)
  cosBeta = cos(tilt)
  sinBeta = sin(tilt)
  ytmat(1) = cosBeta
  ytmat(3) = sinBeta
  ytmat(5) = 1.
  ytmat(7) = -sinBeta
  ytmat(9) = cosBeta
  return
end subroutine fill_ytilt_matrix

! FILL_PROJ_MATRIX fills 2x2 matrix projMat for projection skew
! given skew projSkew and the approximate rotation angle of the axes,
! projStrRot
!
subroutine fill_proj_matrix(projStrRot, projSkew, projMat, &
    cosPSkew, sinPSkew, cos2rot, sin2rot)
  implicit none
  real * 4 projStrRot, projSkew, projMat(*), cosPSkew, sinPSkew, cos2rot
  real*4 sin2rot
  cosPSkew = cos(projSkew)
  sinPSkew = sin(projSkew)
  cos2rot = cos(2 * projStrRot)
  sin2rot = sin(2 * projStrRot)
  projMat(1) = cosPSkew + sinPSkew * sin2rot
  projMat(2) = -sinPSkew * cos2rot
  projMat(3) = projMat(2)
  projMat(4) = cosPSkew - sinPSkew * sin2rot
  return
end subroutine fill_proj_matrix

! FILL_ROT_MATRIX fills 2x2 matrix RMAT given rotation angle ROT
!
subroutine fill_rot_matrix(rot, rmat, cosGamma, sinGamma)
  implicit none
  real*4 rot, rmat(*), cosGamma, sinGamma
  cosGamma = cos(rot)
  sinGamma = sin(rot)
  rmat(1) = cosGamma
  rmat(2) = -sinGamma
  rmat(3) = sinGamma
  rmat(4) = cosGamma
  return
end subroutine fill_rot_matrix


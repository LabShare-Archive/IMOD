! TAPERINVOL pads a volume, dimensions NXBOX by NYBOX by NZBOX in
! ARRAY, into the center of a larger array.  The padded image in BRRAY
! will have size NX by NY by NZ while BRRAY is assumed to be
! dimensioned NXDIM by NY by NZ.  The values of the image at its edge
! will be tapered down to the mean value at the edge, over a width of
! the original image area equal to NXTAP, NYTAP, and NZTAP.
!
subroutine taperInVol(array, nxBox, nyBox, nzBox, brray, nxDim, nx, ny, nz, nxTaper, &
    nyTaper, nzTaper)
  implicit none 
  real*4 array(nxBox,nyBox,nzBox), brray(nxDim,ny,nz)
  integer*4 nxBox, nyBox, nzBox, nxDim, nx, ny, nz, nxTaper, nyTaper, nzTaper
  real*4 sum, tsum, dmean, fracX, fracY, fracZ, fmin
  integer*4 ix, iy, iz, ixLow, ixHigh, iyLow, iyHigh, izLow, izHigh, nxTop, nyTop, nzTop
  integer*4 ix1, ix2, iy1, iy2, iz1, iz2, nsum, iyTopLine,  izTopLine, nxLimit
  !
  ! Get the edge mean
  sum = 0.
  do iy = 1, nyBox
    tsum = 0.
    do ix = 1, nxBox
      tsum = tsum + array(ix, iy, 1) + array(ix, iy, nzBox)
    enddo
    sum = sum + tsum
  enddo
  nsum = 2 * nxBox * nyBox
  do iz = 2, nzBox - 1
    tsum = 0.
    do ix = 1, nxBox
      tsum = tsum + array(ix, 1, iz) + array(ix, nyBox, iz)
    enddo
    do iy = 2, nyBox - 1
      tsum = tsum + array(1, iy, iz) + array(nxBox, iy, iz)
    enddo
    sum = sum + tsum
    nsum = nsum + (2 * (nxBox + nyBox - 2))
  enddo
  dmean = sum / nsum
  !
  ixLow = (nx - nxBox) / 2
  ixHigh = ixLow + nxBox
  iyLow = (ny - nyBox) / 2
  iyHigh = iyLow + nyBox
  izLow = (nz - nzBox) / 2
  izHigh = izLow + nzBox
  do iz = nzBox, 1, -1
    do iy = nyBox, 1, -1
      do ix = nxBox, 1, -1
        brray(ix + ixLow, iy + iyLow, iz + izLow) = array(ix, iy, iz)
      enddo
    enddo
  enddo

  if (nxBox .ne. nx .or. nyBox .ne. ny .or. nzBox .ne. nz) then
    do iz = izLow + 1, izHigh
      nxTop = nx + 1
      nyTop = ny + 1
      nzTop = nz + 1

      !
      ! if there is a mismatch between left and right, add a column on
      ! right; similarly for bottom versus top, add a row on top
      !
      if (nx - ixHigh > ixLow) then
        nxTop = nx
        do iy = 1, ny
          brray(nx, iy, iz) = dmean
        enddo
      endif
      if (ny - iyHigh > iyLow) then
        nyTop = ny
        do ix = 1, nx
          brray(ix, ny, iz) = dmean
        enddo
      endif
      !
      do iy = iyLow + 1, iyHigh
        do ix = 1, ixLow
          brray(ix, iy, iz) = dmean
          brray(nxTop - ix, iy, iz) = dmean
        enddo
      enddo
      do iy = 1, iyLow
        iyTopLine = nyTop - iy
        do ix = 1, nx
          brray(ix, iy, iz) = dmean
          brray(ix, iyTopLine, iz) = dmean
        enddo
      enddo
    enddo
    !
    if (nz - izHigh > izLow) then
      nzTop = nz
      do iy = 1, ny
        do ix = 1, nx
          brray(ix, iy, nz) = dmean
        enddo
      enddo
    endif
    do iz = 1, izLow
      izTopLine = nzTop - iz
      do iy = 1, ny
        do ix = 1, nx
          brray(ix, iy, iz) = dmean
          brray(ix, iy, izTopLine) = dmean
        enddo
      enddo
    enddo
  endif
  !
  ! do 8 pixels at once, using symmetry.  if the box was odd in a
  ! dimension, deflect the middle pixel out to the edge to keep it from
  ! being attenuated twice
  !
  do iz = 1, (nzBox + 1) / 2
    fracZ = 1.
    if (iz <= nzTaper) fracZ = iz / (nzTaper + 1.)
    iz1 = iz + izLow
    iz2 = izHigh + 1 - iz
    if (iz2 == iz1) iz2 = nz
    do iy = 1, (nyBox + 1) / 2
      fracY = 1.
      if (iy <= nyTaper) fracY = iy / (nyTaper + 1.)
      iy1 = iy + iyLow
      iy2 = iyHigh + 1 - iy
      if (iy2 == iy1) iy2 = ny
      nxLimit = (nxBox + 1) / 2
      if (fracY == 1. .and. fracZ == 1.) nxLimit = nxTaper
      do ix = 1, nxLimit
        fracX = 1.
        if (ix <= nxTaper) fracX = ix / (nxTaper + 1.)
        fmin = min(fracX, fracY, fracZ)
        ix1 = ix + ixLow
        ix2 = ixHigh + 1 - ix
        if (ix2 == ix1) ix2 = nx
        if (fmin < 1.) then
          brray(ix1, iy1, iz1) = fmin * (brray(ix1, iy1, iz1) - dmean) + dmean
          brray(ix1, iy2, iz1) = fmin * (brray(ix1, iy2, iz1) - dmean) + dmean
          brray(ix2, iy1, iz1) = fmin * (brray(ix2, iy1, iz1) - dmean) + dmean
          brray(ix2, iy2, iz1) = fmin * (brray(ix2, iy2, iz1) - dmean) + dmean
          brray(ix1, iy1, iz2) = fmin * (brray(ix1, iy1, iz2) - dmean) + dmean
          brray(ix1, iy2, iz2) = fmin * (brray(ix1, iy2, iz2) - dmean) + dmean
          brray(ix2, iy1, iz2) = fmin * (brray(ix2, iy1, iz2) - dmean) + dmean
          brray(ix2, iy2, iz2) = fmin * (brray(ix2, iy2, iz2) - dmean) + dmean
        endif
      enddo
    enddo
  enddo
  return
end subroutine taperInVol

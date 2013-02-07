! SHUFFLER manages the input sections in the big array ARRAY.
! IZWANT specifies the piece # that is wanted (numbered from 1, not 0) .
! The routine looks to see if it is already in memory; if not, it loads
! the section, replacing the frame that has not been used in the
! longest time, and returns the index of the frame's start in INDEX
!
! $Id$
!
subroutine shuffler(izWant, index)
  !
  use blendvars
  implicit none
  !
  ! real*4 array(*)
  integer*4 minUsed, i, index, izWant, ioldest, indRead, j, magUse, iangUse
  real*4 xcen, ycen, amat(2,2)
  !
  ! if the section's entry in memIndex exists, return index
  !
  i = memIndex(izWant)
  if (i > 0) then
    index = (i - 1) * npixIn + 1
    juseCount = juseCount + 1
    lastUsed(i) = juseCount
    return
  endif
  !
  ! Otherwise look for oldest used or unused slot
  !
  minUsed = juseCount + 1
  do i = 1, maxLoad
    if (minUsed > lastUsed(i)) then
      minUsed = lastUsed(i)
      ioldest = i
    endif
  enddo
  !
  ! Assign to that slot, clear out previous allocation there
  !
  index = (ioldest - 1) * npixIn + 1
  juseCount = juseCount + 1
  if (izMemList(ioldest) > 0) memIndex(izMemList(ioldest)) = -1
  lastUsed(ioldest) = juseCount
  izMemList(ioldest) = izWant
  memIndex(izWant) = ioldest
  ! print *,'reading section', izwant-1, '  index', index
  call imposn(1, izWant - 1, 0)                 !yes izwant starts at 1
  !
  indRead = index
  if (doFields .and. doingEdgeFunc) indRead = maxLoad * npixIn + 1
  call irdsec(1, array(indRead),*99)
  !
  if (doFields) then
    if (doMagGrad) then
      magUse = min(ilistz, numMagGrad)
      iangUse = min(ilistz, numAngles)
      !
      ! Get center of tilt as center of frame or montage
      !
      if (focusAdjusted) then
        xcen = nxin / 2.
        ycen = nyin / 2.
      else
        xcen = (minXpiece + nxPieces * (nxin - nXoverlap) + nXoverlap) / &
            2. - ixPcList(izWant)
        ycen = (minYpiece + nyPieces * (nyin - nyOverlap) + nyOverlap) / &
            2. - iyPcList(izWant)
      endif
      !
      ! add mag gradient to distortion or make field up
      !
      if (undistort) then
        call addMagGradField(distDx, distDy, fieldDx(1, 1, ioldest), &
            fieldDy(1, 1, ioldest), lmField, nxin, nyin, nxField, nyField, xFieldStrt, &
            yFieldStrt, xFieldIntrv, yFieldIntrv, xcen, ycen, pixelMagGrad, axisRot, &
            tiltAngles(iangUse), dmagPerUm(magUse), rotPerUm(magUse))
      else
        call makeMagGradField(distDx, distDy, fieldDx(1, 1, ioldest), &
            fieldDy(1, 1, ioldest), lmField, nxin, nyin, nxField, nyField, xFieldStrt, &
            yFieldStrt, xFieldIntrv, yFieldIntrv, xcen, ycen, pixelMagGrad, axisRot, &
            tiltAngles(iangUse), dmagPerUm(magUse), rotPerUm(magUse))
      endif
    else
      !
      ! Just copy field if distortion only
      !
      do i = 1, nxField
        do j = 1, nyField
          fieldDx(i, j, ioldest) = distDx(i, j)
          fieldDy(i, j, ioldest) = distDy(i, j)
        enddo
      enddo
    endif
    !
    ! Undistort the piece for computing edge functions
    !
    if (doingEdgeFunc) then
      amat(1, 1) = 1.
      amat(2, 2) = 1.
      amat(1, 2) = 0.
      amat(2, 1) = 0.
      call warpInterp(array(indRead), array(index), nxin, nyin, nxin, nyin, amat, &
          nxin / 2. , nyin / 2., 0., 0., 1., dfill, 1, 0, fieldDx(1, 1, ioldest), &
          fieldDy(1, 1, ioldest), lmField, nxField, nyField, xFieldStrt, yFieldStrt, &
          xFieldIntrv, yFieldIntrv)
    endif
  endif
  return
99 call exitError('READING FILE')
end subroutine shuffler


! CLEARSHUFFLE initializes or clears the memory allocation
!
subroutine clearShuffle()
  use blendvars
  implicit none
  integer*4 i

  juseCount = 0
  do i = 1, memLim
    izMemList(i) = -1
    lastUsed(i) = 0
  enddo
  do i = 1, limNpc
    memIndex(i) = -1
  enddo
  return
end subroutine clearShuffle

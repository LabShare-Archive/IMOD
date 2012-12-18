! Symbol drawing functions, now relying on postscript capabilities
!
! $Id$
!
subroutine psSymbol(x, y, itype)
  use psparams
  integer*4 index(21)
  real*4 vec(3, 44)
  real*4 ringFrac /.45/
  data index/1, 1, 5, 5, 9, 9, 12, 16, -1, -1, 20, 20, 23, 29, 43, -2, -1, -1, 41, 43, 41/
  integer*4 ifFill(21), ifCircle(21)
  real*4 xvec(4), yvec(4)
  data ifFill /0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0/
  data ifCircle /0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1/
  ! square, diamond, triangle, X, cross, up tri, U, S, -, |
  data vec /-.5, -.5, -1., .5, -.5, 0., .5, .5, 0., -.5, .5, 1. &
      , -.5, 0., -1., 0., -.625, 0., .5, 0., 0., 0., .625, 1. &
      , -.5, -.289, -1., .5, -.289, 0., 0., .577, 1. &
      , -.4, -.4, -1., .4, .4, 0., -.4, .4, -1., .4, -.4, 1. &
      , -.5, 0., -1., .5, 0., 0., 0., -.5, -1., 0., .5, 1. &
      , -.5, .289, -1., .5, .289, 0., 0., -.577, 1. &
      , -.38, .5, -1., -.38, -.38, 0., -.26, -.5, 0., .26, -.5, 0., .38, -.38, 0., &
      .38, .5, 1. &
      , .38, .38, -1., .26, .5, 0., -.26, .5, 0., -.38, .38, 0., -.38, .12, 0. &
      , -.26, 0., 0., .26, 0., 0., .38, -.12, 0., .38, -.38, 0., .26, -.5, 0. &
      , -.26, -.5, 0., -.38, -.38, 1. &
      , -.5, 0, -1., .5, 0, 1.,  0, -.5, -1., 0, .5, 1./
  character*4 dummy, dumm2
  if (itype == 0 .or. itype > 21) return
  if (itype < 0) then
    iscale = nint(107. * symScale)
    write(dummy, '(i4)') - itype
    numChars = alog10(float(-itype)) + 1.0001
    dumm2 = ' '
    dumm2(1:numChars) = dummy(5 - numChars:4)
    call psWriteText(x + .016, y - .018, dumm2(1:numChars), iscale, 0, 0)
    return
  endif
  ind = index(itype)
  scaleVec = symScale
  if (ifFill(itype) .ne. 0) scaleVec = symScale + (nThick - 1) / unitsPerInch
  indVec = 0
  go to (20, 20, 20, 20, 20, 20, 10, 10, 30, 30, 20, 20, 10, 10, 10, 50, 40, 40 &
      , 10, 10, 10), itype
  !
  ! arbitrary path
  !
10 xx = x + symScale * vec(1, ind)
  yy = y + symScale * vec(2, ind)
  instruct = nint(vec(3, ind))
  if (instruct < 0) call psMoveAbs(xx, yy)
  if (instruct >= 0) call psVectAbs(xx, yy)
  if (instruct > 0) then
    if (ifCircle(itype) .ne. 0) go to 30
    return
  endif
  ind = ind + 1
  go to 10
  !
  ! closed path
  !
20 indVec = indVec + 1
  xvec(indVec) = x + scaleVec * vec(1, ind)
  yvec(indVec) = y + scaleVec * vec(2, ind)
  instruct = nint(vec(3, ind))
  ind = ind + 1
  if (instruct <= 0) go to 20
  if (indVec == 3) call psTriangle(xvec, yvec, ifFill(itype))
  if (indVec == 4) call psQuadrangle(xvec, yvec, ifFill(itype))
  if (ifCircle(itype) .ne. 0) go to 30
  return
  !
  ! circle, open or filled
  !
30 call psCircle(x, y, scaleVec / 2., ifFill(itype))
  return
  !
  ! Dot
  !
40 call psCircle(x, y, max(2, nThick) / unitsPerInch, 1)
  if (ifCircle(itype) .ne. 0) go to 30
  return
  !
  ! Ring, select a thickness to cover given fraction
  !
50 nthkSave = nThick
  ringScale = 0.5 * scaleVec
  nThick = nint(ringFrac * ringScale * unitsPerInch) + 1
  call psSetup(nThick, c1, c2, c3, 0)
  call psCircle(x, y, (1 - ringFrac / 2.) * ringScale, 0)
  call psSetup(nthkSave, c1, c2, c3, 0)
  return
end subroutine psSymbol

subroutine psSymSize(size)
  use psparams
  symScale = size
  return
end subroutine psSymSize

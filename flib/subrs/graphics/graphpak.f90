! An interface between DNMs old graphics calls (first for a Megatek board in
! a NOVA 1220 then a Parallax graphics device in a microVax) and Qt graphics
!
! 12/27/90: added ability to change color
! 3/18/91: made it return and not turn on flag if p_start fails,
! made grfopn call erase if parallax is not on yet, to turn it on
!
! $Id$
!
subroutine erase(ix)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  data ifPlaxOn/0/
  data ifReverse/0/
  integer*4 p_start
  if (ifPlaxOn < 0) return
  if (ifPlaxOn == 0) then
    if ( p_start() == -1) return
    ifPlaxOn = 1
    icolor = 241
  endif
  ! call p_zoom(1, 1)
  ! call p_pan(0, 1023)
  call plax_erase()
  if (ifReverse .ne. 0) then
    call p_clt8(0, 255, 255, 255)
    call p_clt8(241, 0, 0, 0)
  else
    call p_clt8(0, 0, 0, 0)
    call p_clt8(241, 255, 255, 255)
  endif
  call p_clt8(250, 255, 0, 0)
  call p_clt8(251, 0, 255, 0)
  call p_clt8(252, 0, 0, 255)
  call p_clt8(253, 255, 255, 0)
  call p_clt8(254, 255, 0, 255)
  call p_clt8(255, 0, 255, 255)
  call p_box(0, 0, 0, 1279, 1023)
  ixCur = 0
  iyCur = 0
  call p_b_flush
  return
  !
  entry plxoff
  if (ifPlaxOn <= 0) return
  call p_box(0, 0, 0, 1279, 1023)
  call p_end
  ifPlaxOn = 0
  return
  !
  entry updat(ix)
  if (ifPlaxOn <= 0) return
  call p_b_flush
  return
  !
  entry chgcol(ix)
  icolor = ix
  return
end subroutine erase
!
subroutine grfopn(ifOff)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  ! if ifOff is 0, want it on, so bump from -1 to 0 or leave as is
  if (ifOff == 0) then
    ifPlaxOn = max(0, ifPlaxOn)
    if (ifPlaxOn == 0) call erase(1)
  else
    ! otherwise, want it off; turn it off, mark flag as -1
    call plxoff
    ifPlaxOn = -1
  endif
  return
end subroutine grfopn

subroutine reverseGraphContrast(ival)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  ifReverse = ival
  return
end subroutine reverseGraphContrast

subroutine ma(ix, iy)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  ixCur = ix
  iyCur = iy
  return
  !
  entry mi(ix, iy)
  ixCur = ixCur + ix
  iyCur = iyCur + iy
  return
  !
  entry va(ix, iy)
  if (ifPlaxOn <= 0) return
  call p_vect(icolor, ixCur, iyCur, ix, iy)
  ixCur = ix
  iyCur = iy
  return
  !
  entry vi(ix, iy)
  if (ifPlaxOn <= 0) return
  call p_vect(icolor, ixCur, iyCur, ixCur + ix, iyCur + iy)
  ixCur = ixCur + ix
  iyCur = iyCur + iy
  return
  !
  entry pa(ix, iy)
  if (ifPlaxOn <= 0) return
  ixCur = ix
  iyCur = iy
  call p_circ(icolor, 1, ixCur, iyCur)
  return
  !
  entry pi(ix, iy)
  if (ifPlaxOn <= 0) return
  ixCur = ixCur + ix
  iyCur = iyCur + iy
  call p_circ(icolor, 1, ixCur, iyCur)
  return
end subroutine ma
!
subroutine dspnts(jx, jy, np)
  integer*4 jx(*), jy(*)
  do i = 1, np
    call pa(jx(i), jy(i))
  enddo
  call updat(1)
  return
end subroutine dspnts
!
subroutine dslins(jx, jy, np)
  integer*4 jx(*), jy(*)
  do i = 1, np
    call va(jx(i), jy(i))
  enddo
  call updat(1)
  return
end subroutine dslins
!
subroutine scpnt(ix, iy, itype)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  integer*2 ivec(12)
  character*6 dummy
  if (ifPlaxOn <= 0) return
  ! size was 5 for Parallax - set to 8 for X windows
  isize = 8
  isizem1 = isize - 1
  if (itype < 0) then
    iscale = nint(2 * (isize + 3) / 2.5)
    write(dummy, '(i6)') - itype
    call plax_next_text_align(5)
    call p_sctext(1, iscale, iscale, icolor, ix, iy, adjustl(dummy))
    return
  endif
  if (itype > 0) itype = mod(itype-1, 19) + 1
  go to(1, 2, 3, 3, 5, 5, 7, 8, 9, 10, 5, 5, 13, 14, 15, 9, 10, 18, 19) itype
  return
1 call p_boxo(icolor, ix - isize, iy - isize, ix + isize, iy + isize)
  call p_boxo(icolor, ix - isizem1, iy - isizem1, ix + isizem1, iy + isizem1)
  return
2 call p_box(icolor, ix - isize, iy - isize, ix + isize, iy + isize)
  return
3 len = nint(1.414 * isize)
  ivec(1) = ix + len
  ivec(3) = ix
  ivec(5) = ix - len
  ivec(7) = ix
  ivec(2) = iy
  ivec(4) = iy + len
  ivec(6) = iy
  ivec(8) = iy - len
  if (itype == 4) then
    call p_poly(icolor, 4, ivec)
  else
    call p_polyo(icolor, 4, ivec)
  endif
  return

5 ivec(1) = ix - isize
  ivec(3) = ix + isize
  ivec(5) = ix
  lenY = nint(1.732 * isize)
  if (itype < 7) then
    iyBot = iy - lenY / 3
    ivec(2) = iyBot
    ivec(4) = iyBot
    ivec(6) = iyBot + lenY
  else
    iyTop = iy + lenY / 3
    ivec(2) = iyTop
    ivec(4) = iyTop
    ivec(6) = iyTop - lenY
  endif
  if (mod(itype, 2) == 0) then
    call p_poly(icolor, 3, ivec)
  else
    call p_polyo(icolor, 3, ivec)
  endif
  return
7 call p_vect(icolor, ix - isizem1, iy - isizem1, ix + isizem1, iy + isizem1)
  call p_vect(icolor, ix + isizem1, iy - isizem1, ix - isizem1, iy + isizem1)
  return
8 call p_vect(icolor, ix, iy - isize, ix, iy + isize)
  call p_vect(icolor, ix - isize, iy, ix + isize, iy)
  return
9 call p_circo(icolor, isize, ix, iy)
  call p_circo(icolor, isizem1, ix, iy)
  return
10 call p_circ(icolor, isize, ix, iy)
  return
13 len38 = nint(0.76 * isize)
  len20 = nint(0.32 * isize)
  len32 = nint(0.56 * isize)
  call p_vect(icolor, ix - len38, iy + isize, ix - len38, iy - len32)
  call p_vect(icolor, ix - len38, iy - len32, ix - len20, iy - isize)
  call p_vect(icolor, ix - len20, iy - isize, ix + len20, iy - isize)
  call p_vect(icolor, ix + len20, iy - isize, ix + len38, iy - len32)
  call p_vect(icolor, ix + len38, iy - len32, ix + len38, iy + isize)
  return
14 len38 = nint(0.76 * isize)
  call p_vect(icolor, ix - len38, iy - isize, ix + len38, iy - isize)
  call p_vect(icolor, ix + len38, iy - isize, ix + len38, iy)
  call p_vect(icolor, ix + len38, iy, ix - len38, iy)
  call p_vect(icolor, ix - len38, iy, ix - len38, iy + isize)
  call p_vect(icolor, ix - len38, iy + isize, ix + len38, iy + isize)
  return
15 call p_circo(icolor, isize, ix, iy)
  call p_vect(icolor, ix, iy - isize, ix, iy + isize)
  return
18 call p_circ(icolor, isize / 3, ix, iy)
  return
19 call p_vect(icolor, ix - isize, iy, ix + isize, iy)
  return
end subroutine scpnt

subroutine dsgrd(ix0, iy0, idx, idy, numIntervals)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  if (ifPlaxOn <= 0) return
  tickSize = 5.
  axisAngle = atan2(float(idy), float(idx))
  ixTick = -tickSize * sin(axisAngle)
  iyTick = tickSize * cos(axisAngle)
  do i = 0, numIntervals
    ixCen = ix0 + i * idx
    iyCen = iy0 + i * idy
    call p_vect(icolor, ixCen - ixTick, iyCen - iyTick, ixCen + ixTick, &
        iyCen + iyTick)
  enddo
  call p_vect(icolor, ix0, iy0, ixCen, iyCen)
  call updat(1)
  return
end subroutine dsgrd

subroutine label(number, numChars)
  common /smplgr/ ifPlaxOn, ixCur, iyCur, icolor, ifReverse
  character*8 dummy, dum2
  if (ifPlaxOn <= 0) return
  dum2 = ' '
  write(dummy, '(i8)') number
  dum2(1:numChars) = dummy(9 - numChars:8)
  call p_sctext(1, 8, 8, icolor, ixCur, iyCur, dum2)
  ! call updat(1)
  return
end subroutine label

subroutine xyset(ix)
  entry setno(ix)
  entry penup(ix)
  entry pendn(ix)
  entry grfwat(ix)
  entry xyma(ix, iy)
  entry xyva(ix, iy)
  entry xypa(ix, iy)
  entry xymi(ix, iy)
  entry xyvi(ix, iy)
  entry xypi(ix, iy)
  entry xypnt(ix)
  entry xyspd(ix, iy)
  entry symbl(ix, iy, iz)
  entry xygrd(ix, iy, iz, ia, ib)
  entry logax(ix, iy, rx, ry, rz, iz)
  return
end subroutine xyset

subroutine fortgetarg(i, string)
  character*(*) string
  call getarg(i, string)
  return
end subroutine fortgetarg

integer*4 function fortiargc()
  fortiargc = iargc()
  return
end function fortiargc


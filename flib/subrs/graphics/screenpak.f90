! An interface between DNMs old-style graphics calls and the qtplax Qt graphics module
!
! 12/27/90: added ability to change color
! 3/18/91: made it return and not turn on flag if p_start fails,
! made grfopn call erase if parallax is not on yet, to turn it on
!
! $Id$
!
module scrnvars
  integer*4 ifPlaxOn/0/, ixCur, iyCur, icolor, ifReverse/0/
end module scrnvars

! Initializes graphics if needed, clears screen and restores default colors
subroutine scrnErase(ix)
  use scrnvars
  integer*4 plax_open
  if (ifPlaxOn < 0) return
  if (ifPlaxOn == 0) then
    if ( plax_open() == -1) return
    ifPlaxOn = 1
    icolor = 241
  endif
  call plax_erase()
  if (ifReverse .ne. 0) then
    call plax_mapcolor(0, 255, 255, 255)
    call plax_mapcolor(241, 0, 0, 0)
  else
    call plax_mapcolor(0, 0, 0, 0)
    call plax_mapcolor(241, 255, 255, 255)
  endif
  call plax_mapcolor(250, 255, 0, 0)
  call plax_mapcolor(251, 0, 255, 0)
  call plax_mapcolor(252, 0, 0, 255)
  call plax_mapcolor(253, 255, 255, 0)
  call plax_mapcolor(254, 255, 0, 255)
  call plax_mapcolor(255, 0, 255, 255)
  call plax_box(0, 0, 0, 1279, 1023)
  ixCur = 0
  iyCur = 0
  call plax_flush()
  return
end subroutine scrnErase

! Closes the graphics window
subroutine scrnClose()
  use scrnvars
  if (ifPlaxOn <= 0) return
  call plax_box(0, 0, 0, 1279, 1023)
  call plax_close()
  ifPlaxOn = 0
  return
end subroutine scrnClose

! Change the current color index for calls that rely on it
subroutine scrnChangeColor(ix)
  use scrnvars
  icolor = ix
  return
end subroutine scrnChangeColor

! Flush; i.e. force a draw
subroutine scrnUpdate(ix)
  use scrnvars
  if (ifPlaxOn <= 0) return
  call plax_flush()
  return
end subroutine scrnUpdate

! Open graphics window or mark it as not to be opened
subroutine scrnOpen(ifOff)
  use scrnvars
  ! if ifOff is 0, want it on, so bump from -1 to 0 or leave as is
  if (ifOff == 0) then
    ifPlaxOn = max(0, ifPlaxOn)
    if (ifPlaxOn == 0) call scrnErase(1)
  else
    ! otherwise, want it off; turn it off, mark flag as -1
    call scrnClose()
    ifPlaxOn = -1
  endif
  return
end subroutine scrnOpen

! Set the reverse contrast flag
subroutine reverseGraphContrast(ival)
  use scrnvars
  ifReverse = ival
  return
end subroutine reverseGraphContrast

! Move current point to absolute position
subroutine scrnMoveAbs(ix, iy)
  use scrnvars
  ixCur = ix
  iyCur = iy
  return
end subroutine scrnMoveAbs

! Move current point incrementally
subroutine scrnMoveInc(ix, iy)
  use scrnvars
  ixCur = ixCur + ix
  iyCur = iyCur + iy
  return
end subroutine scrnMoveInc

! Draw vector to absolute position
subroutine scrnVectAbs(ix, iy)
  use scrnvars
  if (ifPlaxOn <= 0) return
  call plax_vect(icolor, ixCur, iyCur, ix, iy)
  ixCur = ix
  iyCur = iy
  return
end subroutine scrnVectAbs

! Draw vector incrementally
subroutine scrnVectInc(ix, iy)
  use scrnvars
  if (ifPlaxOn <= 0) return
  call plax_vect(icolor, ixCur, iyCur, ixCur + ix, iyCur + iy)
  ixCur = ixCur + ix
  iyCur = iyCur + iy
  return
end subroutine scrnVectInc

! Draw a point at absolute position
subroutine scrnPointAbs(ix, iy)
  use scrnvars
  if (ifPlaxOn <= 0) return
  ixCur = ix
  iyCur = iy
  call plax_circ(icolor, 1, ixCur, iyCur)
  return
end subroutine scrnPointAbs

! Draw a point at incremental position
subroutine scrnPointInc(ix, iy)
  use scrnvars
  if (ifPlaxOn <= 0) return
  ixCur = ixCur + ix
  iyCur = iyCur + iy
  call plax_circ(icolor, 1, ixCur, iyCur)
  return
end subroutine scrnPointInc


! Draw a set of points (Unused)
subroutine scrnPoints(jx, jy, np)
  integer*4 jx(*), jy(*)
  do i = 1, np
    call scrnPointAbs(jx(i), jy(i))
  enddo
  call scrnUpdate(1)
  return
end subroutine scrnPoints

! Draw a set of vectors (Unused)
subroutine scrnLines(jx, jy, np)
  integer*4 jx(*), jy(*)
  do i = 1, np
    call scrnVectAbs(jx(i), jy(i))
  enddo
  call scrnUpdate(1)
  return
end subroutine scrnLines

! Draw a symbol at the absolute position of the given type
subroutine scrnSymbol(ix, iy, itype)
  use scrnvars
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
    call plax_sctext(1, iscale, iscale, icolor, ix, iy, adjustl(dummy))
    return
  endif
  if (itype > 0) itype = mod(itype-1, 19) + 1
  go to(1, 2, 3, 3, 5, 5, 7, 8, 9, 10, 5, 5, 13, 14, 15, 9, 10, 18, 19) itype
  return
1 call plax_boxo(icolor, ix - isize, iy - isize, ix + isize, iy + isize)
  call plax_boxo(icolor, ix - isizem1, iy - isizem1, ix + isizem1, iy + isizem1)
  return
2 call plax_box(icolor, ix - isize, iy - isize, ix + isize, iy + isize)
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
    call plax_poly(icolor, 4, ivec)
  else
    call plax_polyo(icolor, 4, ivec)
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
    call plax_poly(icolor, 3, ivec)
  else
    call plax_polyo(icolor, 3, ivec)
  endif
  return
7 call plax_vect(icolor, ix - isizem1, iy - isizem1, ix + isizem1, iy + isizem1)
  call plax_vect(icolor, ix + isizem1, iy - isizem1, ix - isizem1, iy + isizem1)
  return
8 call plax_vect(icolor, ix, iy - isize, ix, iy + isize)
  call plax_vect(icolor, ix - isize, iy, ix + isize, iy)
  return
9 call plax_circo(icolor, isize, ix, iy)
  call plax_circo(icolor, isizem1, ix, iy)
  return
10 call plax_circ(icolor, isize, ix, iy)
  return
13 len38 = nint(0.76 * isize)
  len20 = nint(0.32 * isize)
  len32 = nint(0.56 * isize)
  call plax_vect(icolor, ix - len38, iy + isize, ix - len38, iy - len32)
  call plax_vect(icolor, ix - len38, iy - len32, ix - len20, iy - isize)
  call plax_vect(icolor, ix - len20, iy - isize, ix + len20, iy - isize)
  call plax_vect(icolor, ix + len20, iy - isize, ix + len38, iy - len32)
  call plax_vect(icolor, ix + len38, iy - len32, ix + len38, iy + isize)
  return
14 len38 = nint(0.76 * isize)
  call plax_vect(icolor, ix - len38, iy - isize, ix + len38, iy - isize)
  call plax_vect(icolor, ix + len38, iy - isize, ix + len38, iy)
  call plax_vect(icolor, ix + len38, iy, ix - len38, iy)
  call plax_vect(icolor, ix - len38, iy, ix - len38, iy + isize)
  call plax_vect(icolor, ix - len38, iy + isize, ix + len38, iy + isize)
  return
15 call plax_circo(icolor, isize, ix, iy)
  call plax_vect(icolor, ix, iy - isize, ix, iy + isize)
  return
18 call plax_circ(icolor, isize / 3, ix, iy)
  return
19 call plax_vect(icolor, ix - isize, iy, ix + isize, iy)
  return
end subroutine scrnSymbol

! Draw a grid at the given starting position and interval
subroutine scrnGridLine(ix0, iy0, idx, idy, numIntervals)
  use scrnvars
  if (ifPlaxOn <= 0) return
  tickSize = 5.
  axisAngle = atan2(float(idy), float(idx))
  ixTick = -tickSize * sin(axisAngle)
  iyTick = tickSize * cos(axisAngle)
  do i = 0, numIntervals
    ixCen = ix0 + i * idx
    iyCen = iy0 + i * idy
    call plax_vect(icolor, ixCen - ixTick, iyCen - iyTick, ixCen + ixTick, &
        iyCen + iyTick)
  enddo
  call plax_vect(icolor, ix0, iy0, ixCen, iyCen)
  call scrnUpdate(1)
  return
end subroutine scrnGridLine

! Print one integer at the current position
subroutine scrnlabel(number, numChars)
  use scrnvars
  character*8 dummy, dum2
  if (ifPlaxOn <= 0) return
  dum2 = ' '
  write(dummy, '(i8)') number
  dum2(1:numChars) = dummy(9 - numChars:8)
  call plax_sctext(1, 8, 8, icolor, ixCur, iyCur, dum2)
  ! call scrnUpdate(1)
  return
end subroutine scrnLabel

! Needed for qtplax to access the argument vector
subroutine fortgetarg(i, string)
  character*(*) string
  call getarg(i, string)
  return
end subroutine fortgetarg

integer*4 function fortiargc()
  fortiargc = iargc()
  return
end function fortiargc


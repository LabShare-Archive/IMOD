! SCRNAXES receives the range of X and Y values, determines "nice" limits
! of X and Y that contain those values, draws axes with labels on a graphics device, and
! returns scaling factors to the calling program in xScale, xAdd, yScale, yAdd.
! Also returns the starting data values in XLO, YLO and 1/10 of the data range in DX and
! DY, even when it draws axes with 8 or 12 divisions
!
!  $Id$
!
subroutine scrnAxes(xMin, xMax, yMin, yMax, xScale, xAdd, yScale, yAdd, dx, xlo, dy, ylo)
  implicit none
  real*4 xMin, xMax, yMin, yMax, xScale, xAdd, yScale, yAdd, dx, xlo, dy, ylo
  integer*4 itextSize, leftX, nintNeededX, maxLenX, nintNeededY, maxLenY, i, numXdiv
  integer*4 numYdiv, iyBot, ixDelta, ixRange, iyDelta, iyRange
  real*4 dxDiv, dyDiv
  character*8 xformat, yformat
  character*25 label

  itextSize = 8
  iyBot = 80
  call scrnErase(-1)
  call axsub(xMin, xMax, dx, xlo, 'X', numXdiv, dxDiv)
  call formatForAxis(dxDiv, xlo, numXdiv, xformat, nintNeededX, maxLenX)
  call axsub(yMin, yMax, dy, ylo, 'Y', numYdiv, dyDiv)
  call formatForAxis(dyDiv, ylo, numYdiv, yformat, nintNeededY, maxLenY)
  leftX = 15 + max(70, maxLenY * 18)
  ixDelta = 920 / numXdiv
  ixRange = numXdiv * ixDelta
  iyDelta = 920 / numYdiv
  iyRange = numYdiv * iyDelta
  do i = 0, numXdiv / 2
    call plax_next_text_align(1)
    if (nintNeededX > 0) then
      write(label, fmt = xformat, err = 10) nint(xlo + i * 2 * dxDiv)
    else
      write(label, fmt = xformat, err = 10) xlo + i * 2 * dxDiv
    endif
10  call plax_sctext(1, itextSize, itextSize, 241, leftX + i * 2 * ixDelta, iyBot - 15,  &
        adjustl(label))
  enddo

  do i = 0, numYdiv / 2
    call plax_next_text_align(2)
    if (nintNeededY > 0) then
      write(label, fmt = yformat, err = 20) nint(ylo + i * 2 * dyDiv)
    else
      write(label, fmt = yformat, err = 20) ylo + i * 2 * dyDiv
    endif
20  call plax_sctext(1, itextSize, itextSize, 241, leftX - 15, iyBot + i * 2 * iyDelta, &
        label)
  enddo
  call scrnGridLine(leftX, iyBot, ixDelta, 0, numXdiv)
  call scrnGridLine(leftX + ixRange, iyBot, 0, iyDelta, numYdiv)
  call scrnGridLine(leftX + ixRange, iyBot + iyRange, -ixDelta, 0, numXdiv)
  call scrnGridLine(leftX, iyBot + iyRange, 0, -iyDelta, numYdiv)
  call scrnUpdate
  xScale = ixDelta / dxDiv
  yScale = iyDelta / dyDiv
  xAdd = leftX - xlo * xScale
  yAdd = iyBot - ylo * yScale
  return
end subroutine scrnAxes


! Run the routine to determine scaling and print the scaling on the terminal
!
subroutine axsub(xMin, xMax, dx, xlo, name, numDiv, dxDiv)
  implicit none
  real*4 xMin, xMax, dx, xlo, xhi, dxDiv
  integer*4 numDiv
  character name
  call scaleMultiDiv(xMin, xMax, dx, xlo, numDiv, dxDiv)
  xhi = 10. *dx + xlo
  write(*,30) name, xlo, xhi, dxDiv
30 format(1x,a2,' from',f12.5,' to',f12.5,' at',f11.4,' /div.')
  return
end subroutine axsub


! Create a format for drawing axis labels for an axis starting at XLO and with numDiv
! divisions of DX, assuming a label at every other tick mark. Returns format in FRMT,
! nintNeeded nonzero if it is an integer format, and the maximum string length in maxLen
!
subroutine formatForAxis(dx, xlo, numDiv, frmt, nintNeeded, maxLen)
  implicit none
  real*4 dx, xlo, dxpow
  character*(*) frmt
  character*25 label
  integer*4 ipow, ndec, maxLen, ilab, nintNeeded, numDiv, last
  logical allEndZero

  ! Determine needed decimal places from the dx value
  do ipow = 0, 9
    ndec = ipow
    dxpow = dx * 10**ipow
    if (abs(nint(dxpow) - dxpow) < dxpow * 1.e-4) then
      exit
    endif
  enddo

  ! Detremine if integer output is OK and set up preliminary format
  if (ndec == 0 .and. xlo < 9.e8 .and. xlo + numDiv * dx < 9.e8) then
    nintNeeded = 1
    frmt = '(i23)'
  else
    nintNeeded = 0
    write(frmt, '(a,i1,a)') '(f23.', ndec, ')'
  endif

  ! print the actual labels to find their maximum lengths, and to see if there is a
  ! zero on the ends of all of them
  maxLen = 0
  allEndZero = nintNeeded == 0
  do ilab = 0, numDiv / 2
    if (nintNeeded > 0) then
      write(label, '(i23)') nint(xlo + 2 * ilab * dx)
    else
      write(label, fmt = frmt, err = 10) xlo + 2 * ilab * dx
      last = len_trim(label)
      if (label(last:last) .ne. '0') allEndZero = .false.
    endif
10  maxLen = max(maxLen, len_trim(adjustl(label)))
  enddo

  ! If there is a zero, drop it and drop the length
  if (allEndZero .and. ndec > 0) then
    ndec = ndec - 1
    maxLen = maxLen - 1
  endif

  ! Make the real format
  if (maxLen < 10) then
    if (nintNeeded > 0) then
      write(frmt, '(a,i1,a)') '(i', maxLen, ')'
    else
      write(frmt, '(a,i1,a,i1,a)') '(f', maxLen, '.', ndec, ')'
    endif
  else
    write(frmt, '(a,i2,a,i1,a)') 'f', maxLen, '.', ndec, ')'
  endif
  return
end subroutine formatForAxis

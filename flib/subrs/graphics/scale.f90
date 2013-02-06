!  $Id$
!
! SCALE takes the minimum and maximum data values and finds "nice"
! axis limits that contain them, as specified by the lower limit XLO
! and an increment DX, 1/10 of the axis length.
!
subroutine scale(xmin, xmax, dx, xlo)
  implicit none
  real*4 goodRange(10), xmin, xmax, dx, xlo
  data goodRange/1., 1.5, 2., 2.5, 3., 4., 5., 6., 8., 10./
  integer*4 indRange
  call scaleCommon(xmin, xmax, goodRange, 10, dx, xlo, indRange)
  return
end subroutine scale


! scaleMultiDiv also takes the minimum and maximum data values and finds "nice"
! axis limits that contain them, as specified by the lower limit XLO and an increment
! DX, 1/10 of the axis length.  However, it provides one more data range, 1.2, and it
! returns an optimal number of axis divisions in numDiv, which can be 8, 10, or 12, and
! the size of those divisions in dxDiv.
!
subroutine scaleMultiDiv(xmin, xmax, dx, xlo, numDiv, dxDiv)
  implicit none
  real*4 goodRange(11), xmin, xmax, dx, xlo, dxDiv
  data goodRange/1., 1.2, 1.5, 2., 2.5, 3., 4., 5., 6., 8., 10./
  integer*4 numGoodDivs(11), indRange, numDiv
  data numGoodDivs/10, 12, 10, 10, 10, 10, 8, 10, 12, 8, 10/
  call scaleCommon(xmin, xmax, goodRange, 11, dx, xlo, indRange)
  numDiv = numGoodDivs(indRange)
  dxDiv = (10. * dx) / numDiv
  return
end subroutine scaleMultiDiv


! The common scale routine takes the list of good data ranges and returns the index of
! the selected range
subroutine scaleCommon(xmin, xmax, goodRange, numRange, dx, xlo, indRange)
  implicit none
  real*4 goodRange(*), xmin, xmax, dx, xlo
  integer*4 numRange, indRange, loop, logExp
  real*4 xloLast, dxLast, dxLog, fract
  xlo = xmin
  xloLast = 1.e30
  dxLast = 1.e30
  do loop = 1, 100
    if (xmax <= xlo) xmax = xlo + 1.
    dxLog = alog10(xmax - xlo)
    logExp = dxLog
    if (dxLog < 0.) logExp = logExp - 1
    fract = (xmax - xlo) / 10.**(logExp)
    do indRange = 1, numRange
      if (fract <= goodRange(indRange)) then
        exit
      endif
    enddo
    dx = goodRange(indRange) * 10.**(logExp - 1)
    xlo = dx * ifix(xmin / dx)
    if (xlo > xmin) xlo = xlo - dx
    if (xlo == xloLast .and. dx == dxLast .or. xlo + 10. * dx >= xmax) then
      do while (xlo >= dx .and. xlo + 9. * dx > xmax)
        xlo = xlo -dx
      enddo
      return
    endif
    xloLast = xlo
    dxLast = dx
  enddo
  return
end subroutine scaleCommon

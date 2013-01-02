! psGridLine and psLogGrid draw grid lines with equally spaced or logarithmically spaced
! ticks.
!
subroutine psGridLine(xStart, yStart, xRange, yRange, nTicks, tickSize)
  real*4 x(abs(nTicks) + 1), y(abs(nTicks) + 1)
  if (nTicks == 0) return
  numTicks = iabs(nTicks) + 1
  do  i = 1, numTicks
    x(i) = xStart + (i - 1) * xRange / (numTicks - 1)
    y(i) = yStart + (i - 1) * yRange / (numTicks - 1)
  enddo
  call psGridBase(x, y, xRange, nTicks, numTicks, tickSize)
  return
end subroutine psGridLine

subroutine psLogGrid(xStart, yStart, xRange, yRange, tickVals, nTicks, tickSize)
  real*4 tickVals(*), x(abs(nTicks) + 1), y(abs(nTicks) + 1)
  numTicks = iabs(nTicks)
  do i = 1, numTicks
    alogTick = alog10(tickVals(i) / tickVals(1))
    x(i) = xStart + xRange * alogTick
    y(i) = yStart + yRange * alogTick
  enddo
  call psGridBase(x, y, xRange, nTicks, numTicks, tickSize)
  return
end subroutine psLogGrid

subroutine psGridBase(x, y, xRange, nTicks, numTicks, tickSize)
  real*4 x(*), y(*)
  ifHalf = 0
  if (nTicks < 0) ifHalf = 1
  dx = tickSize
  dy = 0.
  if (xRange .ne. 0.) then
    dy = tickSize
    dx = 0.
  endif
  do it = 1, numTicks
    xx = x(it)
    yy = y(it)
    if (it == 1) call psMoveAbs(xx, yy)
    call psVectAbs(xx, yy)
    call psMoveAbs(xx + dx, yy + dy)
    if (ifHalf .ne. 0) call psVectAbs(xx, yy)
    if (ifHalf == 0) call psVectAbs(xx - dx, yy - dy)
    call psMoveAbs(xx, yy)
  enddo
  return
end subroutine psGridBase

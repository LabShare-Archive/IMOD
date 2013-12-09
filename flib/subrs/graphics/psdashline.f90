! subroutine will draw one line after requesting parameters for line
! slope and intercept and starting and ending position, line thickness,
! and distances to dash on and off.
! call parameters define transformation between plotting area and user
! units: x (in inches) = xscale*(x in user units - xlo) +xAdd
!
subroutine psDashInteractive(xscale, xlo, xAdd, yScale, ylo, yAdd)
  write(*,'('' slope, intercept, start and end x (user units), '// &
      'thickness (- to switch x&y), '') ')
  write(*,'('' dash on and off in inches (0, 0 no dash) : '', $) ')
  read(5,*) b, a, xStart, xEnd, lineThick, dashOn, dashOff
  call psDashedLine(xscale, xlo, xAdd, yScale, ylo, yAdd, b, a, xStart, xEnd, lineThick, &
      dashOn, dashOff)
  return
end subroutine psDashInteractive

! This draws a dashed line given all the parameters 
subroutine psDashedLine(xscale, xlo, xAdd, yScale, ylo, yAdd, b, a, xStart, xEnd, &
    lineThick, dashOn, dashOff)
  dconv = 1. / (xscale * sqrt(1. +(b * yScale / xscale)**2))
  if (lineThick < 0) dconv = 1. / (yScale * sqrt(1. +(b * xscale / yScale)**2))
  dashOnConv = dconv * dashOn
  dashOffConv = dconv * dashOff
  call psSetup(iabs(lineThick), c1, c2, c3, 0)
  xFrom = xStart
80 xTo = xEnd
  if (dashOnConv > 0) xTo = amin1(xEnd, xFrom + dashOnConv)
  yFrom = a + b * xFrom
  yTo = a + b * xTo
  xPlotFrom = xFrom
  xPlotTo = xTo
  if (lineThick < 0) then
    xPlotFrom = yFrom
    xPlotTo = yTo
    yFrom = xFrom
    yTo = xTo
  endif
  call psMoveAbs(xscale * (xPlotFrom - xlo) + xAdd, yScale * (yFrom - ylo) + yAdd)
  call psVectAbs(xscale * (xPlotTo - xlo) + xAdd, yScale * (yTo - ylo) + yAdd)
  xFrom = xTo + dashOffConv
  if (xFrom < xEnd) go to 80
90 continue
  return
end subroutine psDashedLine

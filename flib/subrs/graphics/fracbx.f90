! psFracBox produces little boxes that are filled up to the fraction "FRAC", at the
! location X, Y in inches.
!
subroutine psFracBox(x, y, frac)
  use psparams
  common /bxparm/ boxWidth, boxHeight, boxTick
  data boxWidth, boxHeight, boxTick/.1, .4, .05/
  save / bxparm/
  xlf = trnc(x - boxWidth / 2.)
  xrt = trnc(x + boxWidth / 2.)
  ylo = trnc(y - boxHeight / 2.)
  yhi = trnc(y + boxHeight / 2.)
  call psMoveAbs(xlf, ylo)
  call psVectAbs(xrt, ylo)
  call psVectAbs(xrt, yhi)
  call psMoveAbs(xlf, ylo)
  call psVectAbs(xlf, yhi)
  call psVectAbs(xrt, yhi)
  call psMoveAbs(xlf, y)
  call psVectAbs(xlf - boxTick, y)
  call psMoveAbs(xrt, y)
  call psVectAbs(xrt + boxTick, y)
  numLines = (xrt - xlf) * unitsPerInch
  yLineLo = ylo + extraLen
  yLineHi = yLineLo + frac * ((yhi - unitLen) - yLineLo)
  nthkSave = nThick
  nThick = 1
  do i = 1, numLines
    xx = xlf + i * unitLen
    call psMoveAbs(xx, yLineLo)
    call psVectAbs(xx, yLineHi)
  end do
  nThick = nthkSave
  return
end subroutine psFracBox

subroutine psFracBoxParams(wdth, height, tick)
  common /bxparm/ boxWidth, boxHeight, boxTick
  if (wdth .ne. 0.) boxWidth = wdth
  if (height .ne. 0.) boxHeight = height
  if (tick .ne. 0.) boxTick = tick
  return
end subroutine psFracBoxParams


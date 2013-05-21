! * * * * BSPLT * * * * *
!
! subroutine BSPLT allows one to plot one variable against another,
! with graphical display on either a graphics device, if available, or
! on the terminal (ANSI standard/VT100 class), and hard copy output via
! postscript plotting. See man page for details.
!
! $Id$
!
subroutine bsplt(namex, xx, yy, igroupNum, numPoints, isymbol, numGroups, irecx, irecy,  &
    colx, coly, ifLogX, ifLogY)
  use plotvars
  integer*4 irecx(*), irecy(*), namex(*), igroupNum(*), isymbol(*), numPoints, numGroups
  real*4 colx(*), coly(*), xx(*), yy(*)
  real*4 xllStock(4), yllStock(4), xtick(310), ytick(310)
  character*80 filename
  character*1 hiSymb(19)
  logical byGroup, doConnect, doPlot
  integer*4 iabsNumXtick, iabsNumYtick, iabsPlot, icl, icolorUse, idx, idy, ifBox, ifLogX
  integer*4 ifLogY, ifNewPage, ifNonZero, ifPlot, ifPlotConnect, ifTerm, ifType
  integer*4 igrpLast, il, irow, isymThick, isymbTemp, ithickGrid, ithickTukey, iunOut
  integer*4 iv, ix, iy, j, jcol, nn
  integer*4 numLines, numXticks, numYticks
  real*4 aa, alog10, bb, c1, c2, c3, connectAdjust, cutDist, defScale, denom
  real*4 distTot, dx, dy, err, errLen, fracBoxHeight, fracBoxTick, fracBoxWidth
  real*4 fracCut, p10, p25, p50, p75, p90, radic, rnumer, rr, rx
  real*4 rxAdj, rxLast, ry, ryAdj, ryLast, sa, sb, se, sx, sxsq, sxy, sy, symWidth, sysq
  real*4 term, tickLeft, tickRight, tickSize, tukeyAdjust, tukeyGap, tukeyTick, tukeyWidth
  real*4 unitsPerInch, widthInch, x, xGridOfs, xLinMax, xLinMin, xLowerLeft, xadd, xcen
  real*4 xcut, xhigh, xleft, xlow, xmax, xmin, xrange, xright, xscale, xtmp, xtrunc
  real*4 y10, y25, y50, y75, y90, yGridOfs, yLinMax, yLinMin, yLowerLeft, yMinus
  real*4 yPlus, yadd, ycut, yhigh, ylow, ymax, ymin, yrange, yscale, ytrunc
  data hiSymb/'-', '|', '<', '>', '/', '\\', 'X', '+', 'O', '@', 'v', '^' &
      , '3', '4', '5', '6', '7', '8', '9'/
  data xllStock, yllStock/0., 0., 5.2, 5.2, 5.2, 0., 5.2, 0./
  data ifTerm/0/, ifType/0/, ifPlot/0/
  save ifTerm, ifType, ifPlot
  !
  ! Get the range including error bars
  call minmax(xx, numPoints, xmin, xmax)
  call minmax(yy, numPoints, ymin, ymax)
  if (igenPltType < 0) then
    ifNonZero = 0
    do i = 1, numPoints
      ymin = min(ymin, yy(i) - colx(i))
      ymax = max(ymax, yy(i) + colx(i))
      if (colx(i) .ne. 0.) ifNonZero = 1
    enddo
  endif
  ifTerm = 0
  if (ifNoTerm == 0) then
    write(*,'('' 0 for graphics plot, 1 for plot on terminal: '',$)')
    read(5,*) ifTerm
  endif
  if (ifTerm > 0) then
    call chrout(27)
    call chrout(ichar('['))
    call chrout(ichar('2'))
    call chrout(ichar('J'))
    call setpos(21, 0)
  endif
  !
  ! Draw the axes then the data, not by group
  call scrnAxes(xmin, xmax, ymin, ymax, xscale, xadd, yscale, yadd, dx, xlow, dy, ylow)
  if (ifTerm == 0) then
    call plax_drawing_scale(xscale, xadd, yscale, yadd);
    igrpLast = -1
    do icl = 1, numColors
      call plax_mapcolor(icl + 100, icolors(2, icl), icolors(3, icl), icolors(4, icl))
    enddo
    do i = 1, numPoints
      igroup = igroupNum(i)
      if (igroup .ne. igrpLast .and. numColors > 0) then
        icolorUse = 241
        call lookupColorIndex(1, igroup, icl)
        if (icl > 0) icolorUse = 100 + icl
        call scrnChangeColor(icolorUse)
      endif
      igrpLast = igroup
      ix = xscale * xx(i) + xadd
      iy = yscale * yy(i) + yadd
      isymbTemp = isymbol(igroup)
      !
      ! if the symbol type is 0, draw connected lines instead
      if (igenPltType < 0 .or. isymbTemp == 0 .or. ifConnect .ne. 0) then
        if (i > 1 .and. igroupNum(i) == igroupNum(max(1, i - 1))) then
          call scrnVectAbs(ix, iy)
        else
          call scrnMoveAbs(ix, iy)
        endif
      endif
      !
      ! Draw symbols and error bars
      if (isymbTemp < 0) isymbTemp = -i
      call scrnSymbol(ix, iy, isymbTemp)
      if (igenPltType < 0) then
        idy = yscale * colx(i)
        call scrnMoveAbs(ix, iy + idy)
        call scrnVectAbs(ix, iy - idy)
        call scrnMoveAbs(ix, iy)
      endif
    enddo
    if (numColors > 0) call scrnChangeColor(241)
    !
    ! Output symbols and keys on right
    do i = 1, min(numKeys, numGroups)
      icolorUse = 241
      call lookupColorIndex(1, i, icl)
      if (icl > 0) icolorUse = 100 + icl
      call scrnChangeColor(icolorUse)
      ix = (xlow + 10. * dx) * xscale + xadd + 30
      iy = (ylow + (10.5 - i) * dy) * yscale + yadd
      if (isymbol(i) > 0) call scrnSymbol(ix, iy, isymbol(i))
      call plax_next_text_align(14)
      call plax_sctext(1, 8, 8, icolorUse, ix + 20, iy, keys(i))
    enddo
    if (numColors > 0 .and. numKeys > 0) call scrnChangeColor(241)
    if (xaxisLabel .ne. ' ') then
      call plax_next_text_align(3)
      call plax_sctext(1, 8, 8, 241, nint((xlow + 5. * dx) * xscale + xadd), 2,xaxisLabel)
    endif
    numKeys = 0
    xaxisLabel = ' '
    call scrnUpdate(1)
  elseif (ifTerm > 0) then
    do i = 1, numPoints
      jcol = 7.9999 * (xx(i) - xlow) / dx
      if (igenPltType < 0) then
        irow = 2.09995 * (10. -(yy(i) + colx(i) - ylow) / dy)
        call setpos(irow, jcol)
        call chrout(ichar('-'))
        irow = 2.09995 * (10. -(yy(i) - colx(i) - ylow) / dy)
        call setpos(irow, jcol)
        call chrout(ichar('-'))
      endif
      irow = 2.09995 * (10. -(yy(i) - ylow) / dy)
      call setpos(irow, jcol)
      call chrout(ichar(hiSymb(max(1, isymbol(igroupNum(i))))))
    enddo
    call setpos(22, 65)
    do i = 1, 12
      call chrout(ichar(hiSymb(i)))
    enddo
  endif

  write(*,'(1x,a,$)') 'draw n lines (n+1), type (1), to file (-1), connect '// &
      '(-thick-1), no (0): '
  read(5,*) ifType
  numLines = 0
  ifPlotConnect = 0
  if (ifType > 1) numLines = ifType - 1
  if (ifType < -1) ifPlotConnect = -1 - ifType
  ! if not doing errors, default is to connect all, and addition of
  ! 100 indicates connect by group; but if doing errors, default is
  ! to do by group only, and addition of 100 is needed to specify
  ! connect all
  byGroup = (ifPlotConnect > 100 .and. igenPltType >= 0) .or. &
      (ifPlotConnect > 0 .and. ifPlotConnect < 100 .and. igenPltType < 0)
  ifPlotConnect = mod(ifPlotConnect, 100)
  if (iabs(ifType) > 1) ifType = 0
  iunOut = 6
  if (ifType < 0) then
    iunOut = 8
    write(*,*) 'enter output file name'
    call flnam(filename, 0, '0')
    open(8, file = filename, err = 26, status = 'unknown')
25  read(8, '(a4)', end = 26) filename
    go to 25
  endif
26  write(*,'('' plot # (6-10), - # to set new x/y range, or no plot (0): '',$)')
  read(5,*) ifPlot
  iabsPlot = iabs(ifPlot)
  doPlot = iabsPlot > 5
  if (ifPlot == 0 .and. ifType == 0) go to 60
  defScale = 1.
  if (doPlot) then
    call psSetup(1, widthInch, c2, c3, 0)
    defScale = 0.74 * widthInch / 7.5
    iabsPlot = iabsPlot - 5
  endif
  numXticks = 10
  numYticks = 10
  xGridOfs = 0.
  yGridOfs = 0.
  xhigh = xlow + 10. *dx
  yhigh = ylow + 10. *dy
  xrange = 4.7 * defScale
  yrange = 4.7 * defScale
  symWidth = .1
  errLen = 0.8 * symWidth
  tickSize = 0.05
  isymThick = 1
  ithickGrid = 1
  ifBox = 0
  if (.not. doPlot) go to 60
  !
  ! Get new data range, tick definition and thicknesses if negative entry
  if (ifPlot < 0) then
    write(*,101) xmin, xmax, ymin, ymax
101 format(' min and max values of x:',2f10.3,',  y:',2f10.3)
    if (ifLogX .ne. 0) then
      xLinMin = 10.**xmin
      xLinMax = 10.**xmax
      write(*,235) xLinMin, xLinMax
235   format(' linear limits of x:',2f10.3)
    endif
    if (ifLogY .ne. 0) then
      yLinMin = 10.**ymin
      yLinMax = 10.**ymax
      write(*,236) yLinMin, yLinMax
236   format(' linear limits of y:',2f10.3)
    endif
    write(*,'('' lower and upper limits of x, of y, # ticks x and y: '',$)')
    read(5,*) xlow, xhigh, ylow, yhigh, numXticks, numYticks
    if (doPlot) then
      write(*,'('' tick and symbol size, grid and symbol thickness, 1 for box: '',$)')
      read(5,*) tickSize, symWidth, ithickGrid, isymThick, ifBox
      if (igenPltType < 0 .and. ifNonZero .ne. 0) then
        write(*,'(1x,a,$)') 'Length of ticks at ends of error bars (inches): '
        read(5,*) errLen
      endif
    endif
  endif
  ! 
  ! For +/- 10 entry, get size/position and possible grid offsets
  if (iabsPlot >= 5) then
    write(*,'('' X and Y size, lower left X and Y (- to offset grids): '',$)')
    read(5,*) xrange, yrange, xLowerLeft, yLowerLeft
    if (xLowerLeft < 0.) then
      write(*,'('' grid offset in x: '',$)')
      read(5,*) xGridOfs
      xLowerLeft = -xLowerLeft
    endif
    if (yLowerLeft < 0.) then
      write(*,'('' grid offset in y: '',$)')
      read(5,*) yGridOfs
      yLowerLeft = -yLowerLeft
    endif
    if (isymbol(1) == -2) then
      fracBoxWidth = 0.
      fracBoxHeight = 0.
      fracBoxTick = 0.
      write(*,'(1x,a,$)') 'fraction box width, height, tick size: '
      read(5,*) fracBoxWidth, fracBoxHeight, fracBoxTick
      call psFracBoxParams(fracBoxWidth, fracBoxHeight, fracBoxTick)
    endif
  else
    xLowerLeft = xllStock(iabsPlot) * defScale
    yLowerLeft = yllStock(iabsPlot) * defScale
  endif
  !
  ! Box parameters for tukey box plots, ask about new page
  if (igenPltType == 2) then
    tukeyWidth = 0.4
    tukeyTick = 0.1
    tukeyGap = 0.
    ithickTukey = 1
    write(*,'(1x,a,$)') 'Tukey box width, tick size, tick-symbol gap, line thickness: '
    read(5,*) tukeyWidth, tukeyTick, tukeyGap, ithickTukey
  endif
  iabsNumXtick = iabs(numXticks)
  iabsNumYtick = iabs(numYticks)
  call psSymSize(symWidth)
  call psSetup(ithickGrid, c1, c2, c3, 0)
  write(*,'('' new page (0 or 1)?: '',$)')
  read(5,*) ifNewPage
  if (ifNewPage .ne. 0) call psFrame()
  xadd = xLowerLeft + 0.1
  yadd = yLowerLeft + 0.1
  ! 
  ! Draw the axes
  if (ifLogX == 0 .or. ifPlot > 0) then
    xscale = xrange / (xhigh - xlow)
    call psGridLine(xadd, yadd - yGridOfs, xrange, 0., numXticks, tickSize)
    if (ifBox .ne. 0) call psGridLine(xadd, yadd + yrange + yGridOfs, xrange, 0., &
        numXticks, -tickSize)
  else
    xlow = alog10(xlow)
    xhigh = alog10(xhigh)
    write(*,'(i3,'' x ticks: '',$)') iabsNumXtick
    read(5,*) (xtick(i), i = 1, iabsNumXtick)
    xscale = xrange / (xhigh - xlow)
    call psLogGrid(xadd, yadd - yGridOfs, xscale, 0., xtick, numXticks, tickSize)
    if (ifBox .ne. 0) call psLogGrid(xadd, yadd + yrange + yGridOfs, xscale, 0., xtick, &
        numXticks, -tickSize)
  endif
  if (ifLogY == 0 .or. ifPlot >= 0) then
    yscale = yrange / (yhigh - ylow)
    call psGridLine(xadd - xGridOfs, yadd, 0., yrange, numYticks, tickSize)
    if (ifBox .ne. 0) call psGridLine(xadd + xrange + xGridOfs, yadd, 0., yrange, &
          numYticks, -tickSize)
  else
    ylow = alog10(ylow)
    yhigh = alog10(yhigh)
    write(*,'(i3,'' y ticks: '',$)') iabsNumYtick
    read(5,*) (ytick(i), i = 1, iabsNumYtick)
    yscale = yrange / (yhigh - ylow)
    call psLogGrid(xadd - xGridOfs, yadd, 0., yscale, ytick, numYticks, tickSize)
    if (ifBox .ne. 0) call psLogGrid(xadd + xrange + xGridOfs, yadd, 0., yscale, ytick, &
        numYticks, -tickSize)
  endif
  call psSetup(isymThick, c1, unitsPerInch, c3, 0)
  ! 
  ! Came here if no plots, so need to test for plotting in this section
  ! Which does the line fits and tukey box plots
60 do igroup = 1, numGroups
    sx = 0.
    sy = 0.
    sxsq = 0.
    sysq = 0.
    sxy = 0.
    nn = 0
    if (doPlot .and. numColors > 0) then
      call lookupColorIndex(1, igroup, icl)
      if (icl > 0) then
        call psSetColor(icolors(2, icl), icolors(3, icl), icolors(4, icl))
      else
        call psSetColor(0, 0, 0)
      endif
    endif
    do i = 1, numPoints
      if (igroupNum(i) .ne. igroup) &
          cycle
      if (ifType .ne. 0) then
        if (igenPltType == 0) then
          write(iunOut, 103) igroup, namex(i), irecx(i), xx(i), colx(i), irecy(i), &
              yy(i), coly(i)
        elseif (igenPltType > 0) then
          write(iunOut, 203) igroup, xx(i), yy(i)
        else
          write(iunOut, 203) igroup, xx(i), yy(i), colx(i)
        endif
      endif
103   format(i3,2i7,2f10.3,i10,2f10.3)
203   format(i3,3f10.3)
      sx = sx + xx(i)
      sy = sy + yy(i)
      sxy = sxy + xx(i) * yy(i)
      sxsq = sxsq + xx(i)**2
      sysq = sysq + yy(i)**2
      nn = nn + 1
      if (igenPltType == 2) colx(nn) = yy(i)
    enddo
    if (nn <= 1)  &
        cycle
    rnumer = nn * sxy - sx * sy
    denom = nn * sxsq - sx**2
    radic = denom * (nn * sysq - sy**2)
    if (radic > 0.) then
      rr = rnumer / sqrt(radic)
      aa = (sy * sxsq - sx * sxy) / denom
      bb = rnumer / denom
      se = 0.
      term = (sysq - aa * sy - bb * sxy)
      if (nn > 2 .and. term >= 0.) se = sqrt(term / (nn - 2))
      sa = se * sqrt(1. / nn + (sx**2 / nn) / denom)
      sb = se / sqrt(denom / nn)
      write(*,105) igroup, nn, rr, aa, bb, sa, sb
105   format('grp',i3,', n=',i4,', r=',f6.3,', a=',f10.3,', b=' &
          ,f10.3,', sa=',f9.3,', sb=',f9.3)
    endif
    if (igenPltType == 2 .and. nn >= 2 .and. doPlot) then
      !
      ! order values in colx
      !
      do i = 1, nn - 1
        do j = i + 1, nn
          if (colx(j) < colx(i)) then
            xtmp = colx(i)
            colx(i) = colx(j)
            colx(j) = xtmp
          endif
        enddo
      enddo
      !
      ! get percentiles and draw box
      !
      call psSetup(ithickTukey, c1, c2, c3, 0)
      tukeyAdjust = (ithickTukey - 1) / unitsPerInch
      p10 = pctile(colx, nn, 0.10)
      p25 = pctile(colx, nn, 0.25)
      p50 = pctile(colx, nn, 0.50)
      p75 = pctile(colx, nn, 0.75)
      p90 = pctile(colx, nn, 0.90)
      write(*,106) igroup, nn, p10, p25, p50, p75, p90
106   format(' grp',i3,', n=',i4,',  10,25,50,75,90%s:',5f9.3)
      y10 = yscale * (p10 - ylow) + yadd - tukeyAdjust
      y25 = yscale * (p25 - ylow) + yadd - tukeyAdjust
      y50 = yscale * (p50 - ylow) + yadd - tukeyAdjust
      y75 = yscale * (p75 - ylow) + yadd - tukeyAdjust
      y90 = yscale * (p90 - ylow) + yadd - tukeyAdjust
      rx = xscale * (sx / nn - xlow) + xadd
      xcen = rx - tukeyAdjust
      xleft = xcen - tukeyWidth / 2.
      xright = xcen + tukeyWidth / 2.
      tickLeft = xcen - tukeyTick / 2.
      tickRight = xcen + tukeyTick / 2.
      call psMoveAbs(xleft, y50)
      call psVectAbs(xright, y50)
      call psMoveAbs(xleft, y25)
      call psVectAbs(xright, y25)
      call psVectAbs(xright, y75)
      call psVectAbs(xleft, y75)
      call psVectAbs(xleft, y25)
      call psMoveAbs(xcen, y25)
      call psVectAbs(xcen, y10)
      call psMoveAbs(xcen, y75)
      call psVectAbs(xcen, y90)
      call psMoveAbs(tickLeft, y10)
      call psVectAbs(tickRight, y10)
      call psMoveAbs(tickLeft, y90)
      call psVectAbs(tickRight, y90)
      call psSetup(isymThick, c1, c2, c3, 0)
      !
      ! plot points outside 10/90 plus gap
      !
      do i = 1, nn
        ytrunc = max(ylow, min(yhigh, colx(i)))
        ry = yscale * (ytrunc - ylow) + yadd
        if (ry < y10 - tukeyGap .or. ry > y90 + tukeyGap) &
            call psSymbol(rx, ry, isymbol(igroup))
      enddo
    endif
  enddo
  if (doPlot .and. numColors > 0) call psSetColor(0, 0, 0)

  if (.not. doPlot) return
  !
  ! Now do non-tukey plotting
  if (igenPltType .ne. 2) then
    connectAdjust = (ifPlotConnect - 1) / unitsPerInch
    igrpLast = -1
    rxLast = -1000.
    ryLast = -1000.
    do i = 1, numPoints
      xtrunc = max(xlow, min(xhigh, xx(i)))
      ytrunc = max(ylow, min(yhigh, yy(i)))
      doConnect = (ifPlotConnect > 0) .and. (i > 1) .and.  &
          (.not.byGroup .or. igroupNum(i) == igrpLast)
      if (numColors > 0 .and. igroupNum(i) .ne. igrpLast) then
        call lookupColorIndex(1, igroupNum(i), icl)
        if (icl > 0) then
          call psSetColor(icolors(2, icl), icolors(3, icl), icolors(4, icl))
        else
          call psSetColor(0, 0, 0)
        endif
      endif
      igrpLast = igroupNum(i)
      rx = xscale * (xtrunc - xlow) + xadd
      ry = yscale * (ytrunc - ylow) + yadd
      rxAdj = rx - connectAdjust
      ryAdj = ry - connectAdjust
      if (doConnect) then
        call psSetup(ifPlotConnect, c1, c2, c3, 0)
        cutDist = symConnectGap * symWidth
        fracCut = 0.
        distTot = sqrt((rxAdj - rxLast)**2 + (ryAdj - ryLast)**2)
        if (isymbTemp .ne. 0 .and. distTot > 1.e-4) fracCut = cutDist / distTot
        if (fracCut <= 0.45) then
          xcut = fracCut * (rxAdj - rxLast)
          ycut = fracCut * (ryAdj - ryLast)
          call psMoveAbs(rxLast + xcut, ryLast + ycut)
          call psVectAbs(rxAdj - xcut, ryAdj - ycut)
        endif
        call psSetup(isymThick, c1, c2, c3, 0)
      endif
      rxLast = rxAdj
      ryLast = ryAdj
      isymbTemp = isymbol(igroupNum(i))
      if (isymbTemp == -2) then
        call psFracBox(rx, ry, colx(i))
      else
        if (isymbTemp < 0) isymbTemp = -i
        call psSymbol(rx, ry, isymbTemp)
      endif
      if (igenPltType < 0 .and. ifNonZero .ne. 0) then
        yPlus = yscale * (max(ylow, min(yhigh, yy(i) + colx(i))) - ylow) + yadd - &
            connectAdjust
        yMinus = yscale * (max(ylow, min(yhigh, yy(i) - colx(i))) - ylow) + yadd - &
            connectAdjust
        if (ifPlotConnect > 0) call psSetup(ifPlotConnect, c1, c2, c3, 0)
        call psMoveAbs(rxAdj - errLen / 2., yMinus)
        call psVectAbs(rxAdj + errLen / 2., yMinus)
        call psMoveAbs(rxAdj - errLen / 2., yPlus)
        call psVectAbs(rxAdj + errLen / 2., yPlus)
        call psMoveAbs(rxAdj, yMinus)
        call psVectAbs(rxAdj, yPlus)
        if (doConnect) call psSetup(isymThick, c1, c2, c3, 0)
      endif
    enddo
    if (numColors > 0) call psSetColor(0, 0, 0)
  endif
  !
  ! Finally draw extra things
  do il = 1, numLines
    call psDashInteractive(xscale, xlow, xadd, yscale, ylow, yadd)
  enddo
  if (ifPlot < 0 .and. doPlot) then
    call label_axis(xadd, yadd - yGridOfs, xrange, xscale, abs(numXticks), xtick, &
        ifLogX, 0)
    call label_axis(xadd - xGridOfs, yadd, yrange, yscale, abs(numYticks), ytick, &
        ifLogY, 1)
    call psMiscItems(xscale, xlow, xadd, xrange, yscale, ylow, yadd, yrange)
  endif
  return
end subroutine bsplt

subroutine gnplt(xx, yy, igroupNum, numPoints, isymbol, numGroups, ifLogX, ifLogY)
  use plotvars
  implicit none
  real*4 xx(*), yy(*), colx(1), coly(1)
  integer*4 igroupNum(*), isymbol(*), numPoints, numGroups, ifLogX, ifLogY
  integer*4 irecx(1), irecy(1), namex(1)
  igenPltType = 1
  call bsplt(namex, xx, yy, igroupNum, numPoints, isymbol, numGroups, irecx, irecy, &
      colx, coly, ifLogX, ifLogY)
  return
end subroutine gnplt

subroutine errplt(xx, yy, igroupNum, numPoints, isymbol, numGroups, colx, ifLogX, ifLogY)
  use plotvars
  implicit none
  real*4 xx(*), yy(*), colx(*), coly(1)
  integer*4 igroupNum(*), isymbol(*), numPoints, numGroups, ifLogX, ifLogY
  integer*4 irecx(1), irecy(1), namex(1)
  igenPltType = -1
  call bsplt(namex, xx, yy, igroupNum, numPoints, isymbol, numGroups, irecx, irecy, &
      colx, coly, ifLogX, ifLogY)
  return
end subroutine errplt

subroutine boxplt(xx, yy, igroupNum, numPoints, isymbol, numGroups, colx, ifLogX, ifLogY)
  use plotvars
  implicit none
  real*4 xx(*), yy(*), colx(*), coly(1)
  integer*4 igroupNum(*), isymbol(*), numPoints, numGroups, ifLogX, ifLogY
  integer*4 irecx(1), irecy(1), namex(1)
  igenPltType = 2
  call bsplt(namex, xx, yy, igroupNum, numPoints, isymbol, numGroups, irecx, irecy, &
      colx, coly, ifLogX, ifLogY)
  return
end subroutine boxplt


subroutine setpos(irow, jcol)
  implicit none
  integer*4 irow, jcol, idig1, idig2
  call chrout(27)
  call chrout(ichar('['))
  idig1 = (irow + 1) / 10
  if (idig1 > 0) call chrout(idig1 + 48)
  call chrout(48 + (irow + 1 - 10 * idig1))
  call chrout(ichar(';'))
  idig2 = (jcol + 1) / 100
  idig1 = (jcol + 1 - 100 * idig2) / 10
  if (idig2 > 0) call chrout(idig2 + 48)
  if (idig1 > 0 .or. idig2 > 0) call chrout(idig1 + 48)
  call chrout(48 + (jcol + 1 - 10 * idig1 - 100 * idig2))
  call chrout(ichar('H'))
  return
end subroutine setpos



real*4 function pctile(x, n, p)
  implicit none
  real*4 x(*), p, v, f
  integer*4 n, iv
  v = n * p + 0.5
  v = max(1., min(float(n), v))
  iv = min(v, n - 1.)
  f = v - iv
  pctile = (1. -f) * x(iv) + f * x(iv + 1)
  ! write(*,'(f6.0,f9.3)') 100.*p, pctile
  return
end function pctile

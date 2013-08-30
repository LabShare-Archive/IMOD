! * * * * GENHSTPLT * * * * *
!
! GENHSTPLT is a general-purpose interface to the BSHST and BSPLT
! histogram and 2-dimensional data plotting routines.  See the
! documentation of those routines for instructions on operating them.
! See man page for details.
!
! $Id$
!
program genhstplt
  call plax_initialize('genhstplt')
  call exit(0)
end program genhstplt

subroutine realGraphicsMain()
  use plotvars
  implicit none
  integer MAX_ROWS, MAX_AVERAGES, MAX_GROUPS
  parameter (MAX_ROWS = 1000000, MAX_AVERAGES = 50000, MAX_GROUPS = 100)
  real*4 dmat(MAX_ROWS * 10), xx(MAX_ROWS), zz(MAX_ROWS), yy(MAX_ROWS)
  integer*4 iXgroup(MAX_ROWS), itype(MAX_ROWS), itypeGroup(MAX_GROUPS, MAX_GROUPS)
  integer*4 iSymbol(MAX_GROUPS), numTypeInGrp(MAX_GROUPS), igroupAvg(MAX_AVERAGES)
  integer*4 numInAvg(MAX_AVERAGES), numComboField
  real*4 avgX(MAX_AVERAGES), avgY(MAX_AVERAGES), groupSdY(MAX_ROWS)
  real*4 selectMin(MAX_GROUPS), selectMax(MAX_GROUPS), comboField(MAX_GROUPS)
  integer*4 icolSelect(MAX_GROUPS), ifSelExclude(MAX_GROUPS)
  integer*4 i, iGroup, icolDenom, icolDivide, icolIn, icolWithNum, icolWithSD
  integer*4 ifLogX, ifLogXin, ifLogY, ifOneTypePerGrp, ifRetain
  integer*4 ifTerm, ifTypes, ifail, igroupStart, inGroup, iopt, numLinCombo
  integer*4 isel, ix, j, jEnd, jStart, k, newCol, nfields, nnVal, numAvgGroups, numAvgTot
  integer*4 numCol, numData, numGroups, numInGroup, numSelect, numSkip
  integer*4 numTypes, numXvals, nxTmp, numColOrig, ifReverse, ifNegOptShown
  real*4 quotientHiLim, quotientLoLim, sdVal, selVal
  real*4 tCrit, tValue, SEMval, baseLog, err, errBarFac
  character*1024 name
  !
  ifTerm = -1
  ifTypes = 0
  numCol = -1
  numSkip = 0
  ifLogXin = 0
  numXvals = 0
  baseLog = 0
  numSelect = 0
  ifReverse = 1
  ifNegOptShown = 0
5 continue
  if (ifNoTerm == 0) then
    write(*,'(1x,a,$)') &
        '0 for Plax screen plots, 1 for terminal only, -1 for always screen [-1]: '
    read(5,*) ifTerm
    if (ifTerm < 0) ifNoTerm = 1
  endif
  if (ifNoTerm .ne. 0) ifTerm = 0
  !
  call reverseGraphContrast(ifReverse)
  call scrnOpen(ifTerm)
  !
  write(*,'(1x,a,/,a,$)') '1 if there are types in first column, 0 if there are '// &
      'no types,', &
      ' -1 to make one column be X and make separate types for other columns [0]: '
  read(5,*) ifTypes
  !
  write(*,'(1x,a,/,a,$)') 'Number of columns of data values, 0 if # is on line '// &
      'before data,', ' or -1 if there is one line per row of data [-1]: '
  read(5,*) numCol
  !
  numSkip = 0
  write(*,'(1x,a,$)') 'Number of lines to skip at start of file [0]: '
  read(5,*,err = 10) numSkip
  !
10 call flnam(name, 1, '0')
  close(1)
  open(1, file = name, status = 'old', err = 10)
  !
  do i = 1, numSkip
    read(1, '(a)') name
  enddo
  if (numCol < 0) then
    read(1, '(a)') name
    call frefor2(name, avgX, itype, nfields, MAX_AVERAGES)
    numCol = 0
    do i = 1, nfields
      if (itype(i) > 0) numCol = i
    enddo
    if (numCol == 0 .or. (ifTypes > 0 .and. numCol == 1)) then
      print *,'There are not enough numeric values at start of first line of data,'// &
          ' try again'
      go to 5
    endif
    if (ifTypes > 0) numCol = numCol - 1
    rewind(1)
    do i = 1, numSkip
      read(1, '(a)') name
    enddo
    write(*,'(i5,a)') numCol, ' columns of data'
  endif
  if (numCol == 0) read(1,*) numCol
  numColOrig = 0
  !
  numData = 0
12 i = numData + 1
  jStart = (i - 1) * numCol + 1
  jEnd = i * numCol
  if (ifTypes <= 0) then
    read(1,*,end = 18) (dmat(j), j = jStart, jEnd)
    itype(i) = 1
  else
    read(1,*,end = 18) itype(i), (dmat(j), j = jStart, jEnd)
  endif
  numData = i
  go to 12
18 write(*,'(i5,a)') numData, ' lines of data'
  if (ifTypes < 0) &
      call convertColsToTypes(dmat, dmat, numCol, numData, itype, numXvals, ifTypes, &
      numColOrig)
  !
20 if (ifTypes .ne. 0) then
    write(*,'('' # of groups (neg. if one Type/group): '',$)')
    read(5,*) numGroups
    ifOneTypePerGrp = 0
    if (numGroups < 0) then
      numGroups = -numGroups
      ifOneTypePerGrp = 1
      numTypes = 1
    endif
    do j = 1, numGroups
      write(*,*) 'for group #', j
      if (ifOneTypePerGrp .ne. 0) then
        write(*,'('' Type # and symbol #: '',$)')
        read(5,*) itypeGroup(1, j), iSymbol(j)
      else
        write(*,'('' # of Types and symbol #: '',$)')
        read(5,*) numTypes, iSymbol(j)
        write(*,'('' Types: '',$)')
        read(5,*) (itypeGroup(i, j), i = 1, numTypes)
      endif
      numTypeInGrp(j) = numTypes
    enddo
  else
    numGroups = 1
    itypeGroup(1, 1) = 1
    numTypeInGrp(1) = 1
    write(*,'(1x,a,$)') 'Symbol #: '
    read(5,*) iSymbol(1)
  endif
  !
30 ifRetain = 0
31 if (numCol > 1) then
    write(*,'('' Column number: '',$)')
    read(5,*) newCol
  else
    newCol = 1
  endif
  if (newCol <= 0 .or. newCol > numCol) go to 20
  numLinCombo = 0
  !
32 if (ifRetain == 0 .and. numXvals > 0) then
    ifLogY = ifLogX
    do i = 1, numXvals
      yy(i) = xx(i)
    enddo
  endif
  !
  write(*,'(1x,a,$)') '1 or 2 to take log or sqr root '// &
      '(-1 if it already is log), base to add: '
  read(5,*) ifLogXin, baseLog
  !
  ifLogX = ifLogXin
  numXvals = 0
  do iGroup = 1, numGroups
    do k = 1, numData
      inGroup = 0
      do j = 1, numTypeInGrp(iGroup)
        if (itype(k) == itypeGroup(j, iGroup)) inGroup = 1
      enddo
      !
      ! check that it passes all selections too
      !
      do isel = 1, numSelect
        selVal = dmat((k - 1) * numCol + icolSelect(isel))
        if ((ifSelExclude(isel) .ne. 0) .neqv. (selVal < selectMin(isel) .or. &
            selVal > selectMax(isel))) inGroup = 0
      enddo
      if (inGroup > 0) then
        numXvals = numXvals + 1
        iXgroup(numXvals) = iGroup
        if (numLinCombo > 0) then
          xx(numXvals) = 0
          do j = 1, numLinCombo
            xx(numXvals) = xx(numXvals) + avgX(j) * dmat((k - 1) * numCol + numInAvg(j))
          enddo
        else
          xx(numXvals) = dmat((k - 1) * numCol + newCol)
        endif
        if (ifLogX > 0) then
          if (ifLogX == 2) xx(numXvals) = sqrt(xx(numXvals) + baseLog)
          if (ifLogX .ne. 2) xx(numXvals) = alog10(xx(numXvals) + baseLog)
        endif
      endif
    enddo
  enddo
  !
  if (ifLogX == 2) ifLogX = 0
  ifLogX = iabs(ifLogX)
  call gnhst(xx, iXgroup, numXvals, iSymbol, numGroups, ifLogX)
  !
  if (ifNegOptShown == 0) then
    ifNegOptShown = 1
    write(*,103)
103 format('Control options also available:',/, &
        '  -2 to enter X axis label and keys for symbols',/, &
        '  -3 to reverse display contrast',/, &
        '  -4 to enter colors for groups in screen display and postscript plots',/, &
        '  -5 to set indexes of text strings for each color in postscript plots ',/, &
        '  -6 to set gap size when connecting symbols',/, &
        '  -8 to wait until window closes then exit')
  endif
50 write(*,104)
104 format(' Enter 1 for new column or 14 for new column to replace Y and retain X,',/, &
      '       2 for plot of this column versus previous column or 17 with connectors,' &
      ,/, &
      '       3 for X/Y plot of averages of groups of points', &
      ' or 11 with column ratios',/, &
      '       4 to define new groups/symbols,', '   5 to open a new file,',/, &
      '       6 or 7 to plot metacode file on screen or printer,', &
      '   8 to exit program,',/, &
      '       9 for Tukey box plots,', &
      '   10 for X/Y plot with error bars using S.D.''s,',/, &
      '       12 to set columns to select on, 13 for X/Y plot dividing Y by a', &
      ' column')
  if (ifTypes == 0 .and. numCol > 2) then
    write(*, 105)
105 format('       15 to make a separate type from each column,   16 for ordinal ', &
        'column,',/,'       18 for new column as linear combination')
  elseif (numColOrig > 0) then
    write(*, 107)
107 format('       15 to restore columns from types,   16 for ordinal column,',/, &
        '       18 for new column as linear combination')
  else
    write(*, 106)
106 format('       16 for ordinal column,   18 for new column as linear combination')
  endif
  read(5,*) iopt
  if (iopt == -123) go to 99
  if (iopt == -8) then
    call plax_wait_for_close()
    go to 99
  endif
  if (iopt == -2) go to 1155
  if (iopt == -3) then
    ifReverse = 1 - ifReverse
    call reverseGraphContrast(ifReverse)
    go to 50
  endif
  if (iopt == -4) go to 1158
  if (iopt == -5) go to 1159
  if (iopt == -6) then
    write(*,'(1x,a,$)') 'Size of connecting line gap as fraction of symbol width: '
    read(5,*) symConnectGap
    go to 50
  endif
  if (iopt == 209) iopt = 7
  if (iopt <= 0 .or. iopt > 18) go to 50
  go to(30, 60, 70, 20, 5, 90, 90, 99, 110, 70, 70, 130, 1130, 1140, 1150, 1160, 60,  &
      1180) iopt
  !
60 ifConnect = 0
  if (iopt == 17) ifConnect = 1
  call gnplt(yy, xx, iXgroup, numXvals, iSymbol, numGroups, ifLogY, ifLogX)
  go to 50
1140 ifRetain = 1
  go to 31
  !
70 write(*,'(1x,a,/,a,$)') 'For the error bars, enter the # of '// &
      'S.D.''s, or - the # of S.E.M.''s,', &
      '   or a large # for confidence limits at that % level: '
  read(5,*) errBarFac
  if (iopt == 10) go to 120
  if (iopt == 11) then
    write(*,'(1x,a,$)') 'Column to divide current column by: '
    read(5,*) icolDenom
  endif
  numAvgTot = 0
  igroupStart = 1
  !
  do iGroup = 1, numGroups
    if (numGroups > 1) write(*,*) ' Define groupings of points for group #', iGroup
    !
    numInGroup = 0
    do i = igroupStart, numXvals
      if (iXgroup(i) .ne. iGroup) exit
      numInGroup = numInGroup + 1
    enddo
    if (iopt == 11) then
      ix = 0
      do k = 1, numData
        inGroup = 0
        do j = 1, numTypeInGrp(iGroup)
          if (itype(k) == itypeGroup(j, iGroup)) inGroup = 1
        enddo
        if (inGroup > 0) then
          ix = ix + 1
          zz(ix) = dmat((k - 1) * numCol + icolDenom)
        endif
      enddo
    endif
    !
    if (numInGroup > 0) then
      if (iopt == 11) then
        call grupnt3(yy(igroupStart), xx(igroupStart), zz, numInGroup,  &
            avgX(numAvgTot + 1), avgY(numAvgTot + 1), groupSdY(numAvgTot + 1), numInAvg, &
            numAvgGroups)
      else
        call grupnt(yy(igroupStart), xx(igroupStart), numInGroup, avgX(numAvgTot + 1), &
            avgY(numAvgTot + 1), groupSdY(numAvgTot + 1), numInAvg, numAvgGroups)
      endif
      !
      do i = 1, numAvgGroups
        SEMval = groupSdY(numAvgTot + i) / sqrt(float(numInAvg(i)))
        if (errBarFac > 30. .and. numInAvg(i) > 1) then
          ifail = 0
          ! tCrit=g01caf(dble((1.+0.01*errBarFac) /2.), numInAvg(i) -1, ifail)
          tCrit = tValue((1. +0.01 * errBarFac) / 2., numInAvg(i) - 1)
          groupSdY(numAvgTot + i) = tCrit * SEMval
        elseif (errBarFac >= 0) then
          groupSdY(numAvgTot + i) = errBarFac * groupSdY(numAvgTot + i)
        elseif (numInAvg(i) > 1) then
          groupSdY(numAvgTot + i) = -errBarFac * SEMval
        endif
        igroupAvg(numAvgTot + i) = iGroup
      enddo
      !
      numAvgTot = numAvgTot + numAvgGroups
      igroupStart = igroupStart + numInGroup
    endif
  enddo
  call errplt(avgX, avgY, igroupAvg, numAvgTot, iSymbol, numGroups, groupSdY, ifLogY, &
      ifLogX)
  go to 50
  !
110 groupSdY(1) = -9999.
  print *,'Enter an X value for each group, or / to use X', &
      ' values in previous column'
  read(5,*) (groupSdY(i), i = 1, numGroups)
  if (groupSdY(1) .ne. -9999.) then
    do i = 1, numXvals
      yy(i) = groupSdY(iXgroup(i))
    enddo
  endif
  call boxplt(yy, xx, iXgroup, numXvals, iSymbol, numGroups, groupSdY, ifLogY, ifLogX)
  go to 50
  !
120 if (errBarFac >= 0 .and. errBarFac <= 30.) then
    write(*,'(1x,a,$)') 'Column number with S.D.''s: '
    read(5,*) icolWithSD
  else
    write(*,'(1x,a,$)') 'Column numbers with S.D.''s and n''s: '
    read(5,*) icolWithSD, icolWithNum
  endif
  ix = 0
  do iGroup = 1, numGroups
    do k = 1, numData
      inGroup = 0
      do j = 1, numTypeInGrp(iGroup)
        if (itype(k) == itypeGroup(j, iGroup)) inGroup = 1
      enddo
      if (inGroup > 0) then
        ix = ix + 1
        sdVal = dmat((k - 1) * numCol + icolWithSD)
        groupSdY(ix) = errBarFac * sdVal
        if (errBarFac < 0. .or. errBarFac > 30.) then
          nnVal = dmat((k - 1) * numCol + icolWithNum)
          SEMval = sdVal / sqrt(float(nnVal))
          if (nnVal <= 1) then
            groupSdY(ix) = 0.
          elseif (errBarFac < 0.) then
            groupSdY(ix) = -errBarFac * SEMval
          else
            ifail = 0
            ! tCrit=g01caf(dble((1.+0.01*errBarFac) /2.), nnVal-1, ifail)
            tCrit = tValue((1. +0.01 * errBarFac) / 2., nnVal - 1)
            groupSdY(ix) = tCrit * SEMval
          endif
        endif
      endif
    enddo
  enddo
  call errplt(yy, xx, iXgroup, numXvals, iSymbol, numGroups, groupSdY, ifLogY, ifLogX)
  go to 50
  !
90 continue
  call pltout(7 - iopt)
  go to 50
  !
130 write(*,'(1x,a,$)') &
      'New column to select on, or 0 to clear selections: '
  read(5,*) icolin
  if (icolin <= 0) then
    numSelect = 0
    go to 50
  endif
  numSelect = numSelect + 1
  icolSelect(numSelect) = icolin
  write(*,'(1x,a,/,a,$)') 'Minimum and maximum values for that ' &
      //'column, and 0 to include or ', &
      '  1 to exclude values in that range: '
  read(5,*) selectMin(numSelect), selectMax(numSelect), ifSelExclude(numSelect)
  go to 50
  !
1130 write(*,'(1x,a,$)') 'Column number to divide by: '
  read(5,*) icolDivide
  if (icolDivide < 1 .or. icolDivide > numCol) go to 50
  write(*,'(1x,a,$)') 'Lower and upper limits for quotient (0,0 for none): '
  read(5,*) quotientLoLim, quotientHiLim
  nxTmp = 0
  do iGroup = 1, numGroups
    do k = 1, numData
      inGroup = 0
      do j = 1, numTypeInGrp(iGroup)
        if (itype(k) == itypeGroup(j, iGroup)) inGroup = 1
      enddo
      !
      ! check that it passes all selections too
      !
      do isel = 1, numSelect
        selVal = dmat((k - 1) * numCol + icolSelect(isel))
        if ((ifSelExclude(isel) .ne. 0) .neqv. (selVal < selectMin(isel) .or. &
            selVal > selectMax(isel))) inGroup = 0
      enddo
      if (inGroup > 0) then
        nxTmp = nxTmp + 1
        zz(nxTmp) = 0.
        if (abs(dmat((k - 1) * numCol + icolDivide)) > 1.e-10 * abs(xx(nxTmp))) then
          zz(nxTmp) = xx(nxTmp) / dmat((k - 1) * numCol + icolDivide)
          if (quotientLoLim .ne. 0. .or. quotientHiLim .ne. 0) &
              zz(nxTmp) = max(quotientLoLim, min(quotientHiLim, zz(nxTmp)))
        endif
      endif
    enddo
  enddo
  call gnplt(yy, zz, iXgroup, numXvals, iSymbol, numGroups, ifLogY, ifLogX)
  go to 50
  !
1150 if (numColOrig > 0) then 
    call convertTypesToCols(dmat, dmat, numCol, numData, ifTypes, numColOrig)
    numXvals = 0
    go to 20
  else
    if (ifTypes > 0 .or. numCol < 2) go to 50
    call convertColsToTypes(dmat, dmat, numCol, numData, itype, numXvals, ifTypes, &
        numColOrig)
    if (ifTypes > 0) go to 20
    go to 50
  endif
  !
1155 write(*,'(1x,a,$)') 'X axis label (Return for none): '
  read(5, '(a)') xaxisLabel
  write(*,'(1x,a,$)') 'Number of key strings for next graph: '
  read(5,*) numKeys
  numKeys = max(0, min(LIM_KEYS, numKeys))
  if (numKeys == 0) go to 50
  write(*,'(a,i2,a)') 'Enter the', numKeys, ' strings one per line:'
  do i = 1, numKeys
    read(*, '(a)') keys(i)
  enddo
  go to 50
1158 write(*,'(1x,a,$)') 'Number of groups to define color for: '
  read(5,*) numColors
  numColors = max(0, min(LIM_COLORS, numColors))
  if (numColors == 0) go to 50
  print *,'For each group, enter group #, red, green, blue values (0-255)'
  read(5,*) ((icolors(j, i), j = 1, 4), i = 1, numColors)
  do i = 1, numColors
    icolors(5:6, i) = -1
    if (icolors(1, i) > 0 .and. icolors(1, i) <= numGroups) &
        icolors(5, i) = iSymbol(icolors(1, i))
  enddo
  go to 50
1159 if (numColors == 0) go to 50
  write(*,'(1x,a,/,a,$)')'For each defined color, enter index of text string to ' // &
      'apply it to,', '   or 0 for none:'
  read(5,*) (icolors(6, i), i = 1, numColors)
  go to 50
  !
1160 ifLogX = 0
  j = 0
  do i = 1, numXvals
    if (i > 1 .and. iXgroup(max(i - 1, 1)) .ne. iXgroup(i)) j = 0
    j = j + 1
    xx(i) = j
  enddo
  go to 50
  !
1180 print *,'Enter series of coefficient,column pairs in one line'
  read(5, '(a)') name
  call frefor3(name, comboField, numInAvg, 0, numComboField, MAX_GROUPS)
  if (mod(numComboField, 2) > 0) then
    print *,'You must enter an even number of values'
    go to 50
  endif
  numLinCombo = numComboField / 2
  do i = 1, numLinCombo
    avgX(i) = comboField(2 * i - 1)
    numInAvg(i) = nint(comboField(2 * i))
    if (numInAvg(i) < 1 .or. numInAvg(i) > numCol) then
      print *,'Column number out of range:', numInAvg(i)
      go to 50
    endif
  enddo
  write(*,'(1x,a,$)') '1 to replace existing Y and retain X, 0 to roll Y into X: '
  read(5,*) ifRetain
  go to 32
  !
99 call scrnClose()
  call psExit()
end subroutine realGraphicsMain


! given a list of numToGroup points (xx, yy), it asks how many contiguous
! non-overlapping groups (numAvgGrps) to divide into, and number of points in
! each group (numInAvg), orders the points by x value, finds the mean x value
! (avgX), and the mean and sd y value (avgY and groupSdY)
!
subroutine grupnt3(xx, yy, zz, numToGroup, avgX, avgY, groupSdY, numInAvg, numAvgGrps)
  implicit none
  real*4 xx(*), avgX(*), groupSdY(*), yy(*), avgY(*), zz(*)
  integer*4 ind(numToGroup), numInAvg(*), numAvgGrps
  real*4 avgYinGrp, avgZ, sdyGrp, sumYsq, thresh, threshDelta, xsum, ysum, zcum, zsum
  integer*4 i, igrp, ii, itmp, j, ncum, ntmp, numToGroup, in

  zsum = 0.
  do i = 1, numToGroup
    ind(i) = i
    zsum = zsum + zz(i)
  enddo
  ! build an index to order the points by xx
  do i = 1, numToGroup - 1
    do j = i + 1, numToGroup
      if (xx(ind(i)) > xx(ind(j))) then
        itmp = ind(i)
        ind(i) = ind(j)
        ind(j) = itmp
      endif
    enddo
  enddo
  write(*,*) numToGroup, ' points'
  write(*,'('' number of groups: '',$)')
  read(*,*) numAvgGrps
  threshDelta = zsum / max(1, numAvgGrps)
  thresh = threshDelta
  ncum = 0
  igrp = 1
  zcum = 0.
  do ii = 1, numToGroup
    i = ind(ii)
    if (zcum + zz(i) >= thresh) then
      thresh = thresh + threshDelta
      numInAvg(igrp) = ii - ncum
      ncum = ii
      igrp = igrp + 1
    endif
    zcum = zcum + zz(i)
  enddo
  if (igrp == numAvgGrps) numInAvg(igrp) = numToGroup - ncum
  write(*,'(1x,a,$)') '# of points in each group (/ for equal denominators): '
  read(5,*) (numInAvg(i), i = 1, numAvgGrps)
  ii = 1
  do igrp = 1, numAvgGrps
    xsum = 0.
    ysum = 0.
    zsum = 0.
    sumYsq = 0.
    ntmp = numInAvg(igrp)
    do in = 1, ntmp
      i = ind(ii)
      ii = ii + 1
      xsum = xsum + xx(i)
      ysum = ysum + yy(i)
      zsum = zsum + zz(i)
      sumYsq = sumYsq + yy(i)**2
    enddo
    avgX(igrp) = xsum / ntmp
    avgYinGrp = ysum / ntmp
    avgZ = zsum / ntmp
    avgY(igrp) = avgYinGrp / avgZ
    sdyGrp = 0.
    if (ntmp > 1) sdyGrp = sqrt((sumYsq - ntmp * avgYinGrp**2) / (ntmp - 1.))
    groupSdY(igrp) = sdyGrp / avgZ
    write(*,101) avgX(igrp), avgYinGrp, sdyGrp, avgZ, avgY(igrp), groupSdY(igrp), numInAvg(igrp)
101 format(6f10.4,i5)
  enddo
  return
end subroutine grupnt3

subroutine convertColsToTypes(dmatIn, dmatOut, numCol, numData, itype, numXvals, &
    ifTypes, numColOrig)
  implicit none
  real*4 dmatIn(numCol, numData), dmatOut(2, numData, numCol)
  real*4 tmpMat(numCol, numData)
  integer*4 numCol, numData, itype(numData, numCol), ixCol, i, j, numXvals, ifTypes
  integer*4 numColOrig
  write(*,'(1x,a,$)') 'Enter column number to become X (first column), or 0 '// &
      'to abort: '
  read(5,*) ixCol
  ifTypes = 0
  if (ixCol <= 0 .or. ixCol > numCol) return
  tmpMat = dmatIn
  do j = 1, numCol
    dmatOut(1, 1:numData, j) = tmpMat(ixCol, 1:numData)
    dmatOut(2, 1:numData, j) = tmpMat(j, 1:numData)
    itype(1:numData, j) = j
  enddo
  numData = numData * numCol
  numColOrig = numCol
  numCol = 2
  numXvals = 0
  ifTypes = 1
  write(*,'(a,i4)')'Data are now in two columns with types from 1 to', numColOrig
  return
end subroutine convertColsToTypes

subroutine convertTypesToCols(dmatIn, dmatOut, numCol, numData, ifTypes, numColOrig)
  implicit none
  real*4 dmatOut(numColOrig, numData / numColOrig)
  real*4 dmatIn(2, numData / numColOrig, numColOrig)
  real*4 tmpMat(2, numData / numColOrig, numColOrig)
  integer*4 numCol, numData, ixCol, i, j, ifTypes
  integer*4 numColOrig
  tmpMat = dmatIn
  numData = numData / numColOrig
  numCol = numColOrig
  do j = 1, numCol
    dmatOut(j, 1:numData) = tmpMat(2, 1:numData, j)
  enddo
  numColOrig = 0
  ifTypes = 0
  write(*,'(a,i4,a)')'Data have been restored to the original', numCol, ' columns'
  return
end subroutine convertTypesToCols

! INPUT_VARS gets the specifications for the geometric variables
! rotation, tilt, mag, compression, X stretch, skew, and X axis tilt.
! It fills the VAR array and the different variable and mapping arrays.
!
! $Id$
!
subroutine input_vars(var, varName, inputAlf, numVarSearch, numVarAngles, &
    numVarScaled, minTiltInd, numCompSearch, ifLocal, mapTiltStart, mapAlfStart, &
    mapAlfEnd, ifBTSearch, tiltOrig, tiltAdd, pipinput, numInView, &
    ninThresh, rotEntered)
  use alivar
  use mapsep
  implicit none
  integer*4 inputAlf, numVarSearch, numVarAngles, numVarScaled, minTiltInd, numCompSearch
  integer*4 ifLocal, mapTiltStart, mapAlfStart, ifBTSearch, numInView(*)
  integer*4 ninThresh, mapAlfEnd
  logical pipinput
  character*8 varName(*)
  real*4 var(*), tiltOrig(*), tiltAdd, rotEntered
  integer*4, allocatable, save :: mapList(:), mapListRot(:), mapListTilt(:)
  integer*4, allocatable, save ::mapListMag(:), mapListXtilt(:), mapListDist(:,:)
  real*4 groupSize(size(tilt))
  integer*4 nmapDefRot, nRanSpecRot, nmapSpecRot(MAXGRP)
  integer*4 ivSpecStrRot(MAXGRP), ivSpecEndRot(MAXGRP)
  integer*4 nmapDefTilt, nRanSpecTilt, nmapSpecTilt(MAXGRP)
  integer*4 ivSpecStrTilt(MAXGRP), ivSpecEndTilt(MAXGRP)
  integer*4 nmapDefMag, nRanSpecMag, nmapSpecMag(MAXGRP)
  integer*4 ivSpecStrMag(MAXGRP), ivSpecEndMag(MAXGRP)
  integer*4 nmapDefXtilt, nRanSpecXtilt, nmapSpecXtilt(MAXGRP)
  integer*4 ivSpecStrXtilt(MAXGRP), ivSpecEndXtilt(MAXGRP)
  integer*4 nmapDefDist(2), nRanSpecDist(2), nmapSpecDist(MAXGRP,2)
  integer*4 ivSpecStrDist(MAXGRP,2), ivSpecEndDist(MAXGRP,2)
  !
  character*8 dump(8)
  character*10 distmp, distText(3)
  data distText/'distortion', 'X stretch ', 'axis skew '/
  character*17 lintmp, linText(2)
  data linText/'3 block, 4 linear', '3 linear, 4 block'/
  character*8 distOptTmp, distOptText(2)
  data distOptText /'XStretch', 'Skew'/
  character*4 rotText
  character*28 localText
  real*4 dtor/0.0174532/
  !
  real*4 powerTilt, powerComp, powerMag, powerSkew, powerDmag, power
  real*4 poweRrot, powerAlf, rotStart, defRot
  real*4 tiltMin, origDev, fixdum
  integer*4 i, ioptRot, iref1, iflin, ioptTilt, nviewFix, ig
  integer*4 iref2, ioptMag, irefComp, ioptComp, iffix, iv, jv, ioptDel
  integer*4 irefDmag, nvarTmp, ivdum, idist, ioptDist(2), ioptAlf, nviewFixIn
  integer*4 irefTilt, numDmagVar, ivl, ivh, lenOpt
  integer*4 irefTiltIn
  integer*4 nearest_view, ifpip, mapFix, ierr
  character*1024 listString
  save ioptRot, ivSpecEndXtilt, ioptTilt, nviewFixIn, irefTiltIn
  save nmapDefRot, nRanSpecRot, nmapSpecRot, ivSpecStrRot, ivSpecEndRot
  save nmapDefTilt, nRanSpecTilt, nmapSpecTilt, ivSpecStrTilt, ivSpecEndTilt
  save nmapDefMag, nRanSpecMag, nmapSpecMag, ivSpecStrMag, ivSpecEndMag
  save nmapDefXtilt, nRanSpecXtilt, nmapSpecXtilt, ivSpecStrXtilt
  save nmapDefDist, nRanSpecDist, nmapSpecDist, ivSpecStrDist, ivSpecEndDist
  save ioptMag, ioptDel, ioptDist, ioptAlf
  !
  integer*4 PipGetInteger, PipNumberOfEntries
  integer*4 PipGetString, PipGetFloat, PipGetBoolean
  character*40 PrependLocal
  !
  powerTilt = 1.
  powerComp = 1.
  powerMag = 0.
  powerSkew = 0.
  powerDmag = 1.5
  poweRrot = 0.
  powerAlf = 0.
  iffix = 0
  !
  tiltAdd = 0.
  numVarSearch = 0
  ifpip = 0
  if (ifLocal == 0) then
    call allocateMapsep(ierr)
    call memoryError(ierr, 'ARRAYS FOR MAPSEP')
    allocate(mapList(maxView), mapListRot(maxView), mapListTilt(maxView), &
        mapListMag(maxView), mapListXtilt(maxView), mapListDist(maxView, 2), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR INPUT_VARS')
  endif

  ! print *,nview, ' views'
  ! print *,(numInView(i), i = 1, nview)
  if (pipinput) ifpip = 1
  if (pipinput .and. ifLocal == 0) then
    call inputSeparateGroups(ngsep, nsepInGrp, ivsep, listString)
    do ig = 1, ngsep
      call mapSeparateGroup(ivsep(1, ig), nsepInGrp(ig), mapFileToView, nfileViews)
    enddo
    rotStart = 0.
    ierr = PipGetFloat('RotationAngle', rotStart)
  endif
  !
  if (pipinput) then
    if (ifLocal < 2) then
      ifRotFix = 0
      ierr = PipGetInteger('RotationFixedView', ifRotFix)
      ioptRot = 0
      ierr = PipGetInteger(PrependLocal('RotOption', ifLocal), ioptRot)
    endif
  else
    if (ifLocal == 0) then
      !
      ! set up the first search variable as a global rotation angle
      ! and the rest as delta rotation angles of each view after first
      !
      write(*,'(1x,a,$)') &
          'Initial angle of rotation in projection plane: '
      read(5,*) rotStart
      !
      write(*,'(1x,a,/,a,/,a,$)') 'Enter 0 to find all rotations,'// &
          ' -1 to find a single global rotation,', &
          ' -2 to fix all rotations at the initial angle', &
          ' or a positive # to fix one view at the initial angle: '

      read(5,*) ifRotFix
      !
      ! transpose to new option list
      !
      ioptRot = 0
      if (ifRotFix >= 0) ioptRot = 1
      if (ifRotFix == - 1) ioptRot = -1
    elseif (ifLocal == 1) then
      !
      ! local entry already had the option list
      !
      print *,'Enter -1 to solve for a single incremental rotation'
      print *,'      0 to fix all rotations at global value'
      print *,'      1 to give each view an independent incremental' &
          //' rotation'
      write(*,'(1x,a,/,a,$)') '      2 to specify other mappings of' &
          //' rotation variables', &
          '       3 or 4 for automatic mapping of groups of views ' &
          //'(3 linear, 4 block): '
      read(5,*) ioptRot
    endif
  endif
  !
  ! 4 / 10 / 04: Eliminated global rotation variable, simplified treatment
  ! of rotation
  !
  if (ifLocal == 0) then
    rotEntered = rotStart
    rotStart = dtor * rotStart
  else
    rotStart = 0.
  endif
  !
  if (ifRotFix > 0) ifRotFix = nearest_view(ifRotFix)
  !
  ! set up appropriate mapping list or read it in
  ! set the meaning of ifRotFix =  0 for a one global rotation variable
  ! and the others incremental to it, + for one variable fixed,
  ! - 1 for all fixed, -2 for single variable, -3 otherwise
  !
  iref1 = ifRotFix
  if (xyzFixed .and. ioptRot > 0) iref1 = 0
  iflin = 0
  defRot = rotStart
  !
  ! All one variable - set default to true angle
  !
  if (ioptRot < 0) then
    do i = 1, nview
      mapList(i) = 1
    enddo
    ifRotFix = -2
    !
    ! All fixed - also set default to angle, set reference to 1
    !
  elseif (ioptRot == 0) then
    do i = 1, nview
      mapList(i) = 0
    enddo
    ifRotFix = -1
    iref1 = 1
    !
    ! All separate variables
    !
  elseif (ioptRot == 1) then
    do i = 1, nview
      mapList(i) = i
    enddo
    !
    ! Specified mapping
    ! 5 / 2 / 02: changed irotfix (?) to iref1 and made output conditional
    !
  elseif (ioptRot == 2) then
    mapFix = 0
    if (iref1 > 0) mapFix = mapViewToFile(iref1)
    call GetMapList('rotation', 'Rot', mapFix, defRot, ifpip, ifLocal, &
        nview, mapList, mapListRot)
    !
    ! automap
    !
  else
    power = 0.
    if (ioptRot == 3) then
      iflin = 1
      power = poweRrot
    endif
    call setGrpSize(tilt, nview, power, groupSize)
    call automap(nview, mapList, groupSize, mapFileToView, nfileViews &
        , ifpip, 1, PrependLocal('RotDefaultGrouping', ifLocal), &
        PrependLocal('RotNondefaultGroup', ifLocal), numInView, ninThresh, &
        ifLocal, nmapDefRot, nRanSpecRot, nmapSpecRot, ivSpecStrRot, &
        ivSpecEndRot)
    if (.not.pipinput) write(6, 111) (mapList(i), i = 1, nview)
  endif
  !
  ! Make sure ifRotFix = 0 means what it is supposed to
  !
  if (ifLocal > 0 .and. ifRotFix == 0) ifRotFix = -3
  rotText = 'rot '
  !
  ! analyze map list
  !
  call analyze_maps(rot, mapRot, linRot, frcRot, fixedRot, fixdum, &
      iflin, mapList, nview, iref1, 0, defRot, rotText, var, varName, &
      numVarSearch, mapViewToFile)
  ! write(6, 111) (mapRot(i), i = 1, nview)
  !
  if (.not. pipinput .and. ifLocal == 0) then
    !
    ! Get list of views to treat separately in automapping
    !
    write(*,'(1x,a,/,a,$)') 'If you are going to automap '// &
        'variables,' &
        //' enter the number of sets of views to treat separately' &
        //' from the main set of views (otherwise enter 0): '
    read(5,*) ngsep
    do ig = 1, ngsep
      write(*,'(1x,a,i3,a,$)') 'List of views in set', ig, &
          ' (ranges OK): '
      call rdlist(5, ivsep(1, ig), nsepInGrp(ig))
      !
      ! check legality and trim ones not in included views
      !
      call mapSeparateGroup(ivsep(1, ig), nsepInGrp(ig), mapFileToView, &
          nfileViews)
      ! print *,(ivsep(i, ig), i = 1, nsepInGrp(ig))
    enddo
  endif
  if (ifLocal == 0) then
    !
    ! get initial tilt angles for all views, convert to radians
    ! save adjusted angles in tiltOrig, then map as radians into tilt
    !
    call get_tilt_angles(nfileViews, 3, tiltOrig, maxView, ifpip)
    if (pipinput) then
      tiltAdd = 0.
      ierr = PipGetFloat('AngleOffset', tiltAdd)
    else
      write(*,'(1x,a,$)') &
          'Angle offset, i.e. amount to add to all angles: '
      read(5,*) tiltAdd
    endif
    do i = 1, nfileViews
      tiltOrig(i) = tiltOrig(i) + tiltAdd
      if (mapFileToView(i) .ne. 0) tilt(mapFileToView(i)) = tiltOrig(i) * dtor
    enddo
  endif
  !
  ! For tilt, set up default on increments and maps
  ! also find section closest to zero: it will have mag set to 1.0
  ! This needs to be done fresh on each local area since views can change
  mapTiltStart = numVarSearch + 1
  tiltMin = 100.
  do i = 1, nview
    tiltInc(i) = 0.
    mapTilt(i) = 0
    mapList(i) = 0
    origDev = abs(tilt(i) - tiltAdd * dtor)
    if (tiltMin > origDev) then
      tiltMin = origDev
      minTiltInd = i
    endif
  enddo
  !
  ! get type of mapping
  !
  if (ifLocal <= 1) then
    if (pipinput) then
      ioptTilt = 0
      ierr = PipGetInteger(PrependLocal('TiltOption', ifLocal), ioptTilt)
    else
      print *,'Enter 0 to have all tilt angles fixed'
      print *,'      1 to vary all except one for a specified view'
      print *,'      2 to vary all except for the view at minimum tilt'
      print *,'      3 to vary all except for a specified view and the' &
          //' one at minimum tilt'
      write(*,'(1x,a,/,a,$)') '      4 to specify other combinations'// &
          ' of variable, mapped and fixed', &
          '       5-8 for automatic mapping of groups of views '// &
          '(5 & 7 linear with 1 or 2', '       fixed tilts, 6 & 8 '// &
          'block with 1 or 2 fixed tilts): '
      read(5,*) ioptTilt
    endif
  endif
  !
  ! set up mapList appropriately or get whole map
  !
  iflin = 0
  nviewFix = 0
  if (ioptTilt >= 1 .and. ioptTilt <= 3) then
    do i = 1, nview
      mapList(i) = i
    enddo
    if (ioptTilt > 1) mapList(minTiltInd) = 0
    if (ioptTilt .ne. 2) then
      if (ifLocal <= 1) then
        if (pipinput) then
          if (PipGetInteger(PrependLocal('TiltFixedView', ifLocal), &
              nviewFixIn) .ne. 0) call errorExit('YOU MUST ENTER A'// &
              ' FIXED VIEW WITH THIS TILT OPTION', 0)
        else
          write(*,'(1x,a,$)') 'Number of view to fix tilt angle for: '
          read(5,*) nviewFixIn
        endif
      endif
      nviewFix = nearest_view(nviewFixIn)
      mapList(nviewFix) = 0
    endif
  elseif (ioptTilt == 4) then
    if (pipinput .or. ifLocal > 1) then
      call GetMapList('tilt', 'Tilt', 0, 0., ifpip, ifLocal, nview, &
          mapList, mapListTilt)
    else
      print *,'For each view, enter 0 to fix the tilt angle', &
          '   or the # of a tilt variable to map its tilt angle to'
      read(5,*) (mapList(i), i = 1, nview)
    endif
  elseif (ioptTilt >= 5) then
    if (ioptTilt > 6) then
      if (ifLocal <= 1) then
        if (pipinput) then
          if (PipGetInteger(PrependLocal('TiltSecondFixedView', ifLocal), &
              nviewFixIn) .ne. 0) call errorExit('YOU MUST ENTER A'// &
              ' SECOND FIXED VIEW WITH THIS TILT OPTION', 0)
        else
          write(*,'(1x,a,$)') 'Second view to fix tilt angle for: '
          read(5,*) nviewFixIn
        endif
      endif
      nviewFix = nearest_view(nviewFixIn)
    endif
    !
    ! automap: get map, then set variable # 0 for ones mapped to
    ! minimum tilt
    !
    power = 0.
    if (ioptTilt == 5 .or. ioptTilt == 7) then
      iflin = 1
      power = powerTilt
    endif
    call setGrpSize(tilt, nview, power, groupSize)
    call automap(nview, mapList, groupSize, mapFileToView, nfileViews &
        , ifpip, 1, PrependLocal('TiltDefaultGrouping', ifLocal), &
        PrependLocal('TiltNondefaultGroup', ifLocal), numInView, ninThresh, &
        ifLocal, nmapDefTilt, nRanSpecTilt, nmapSpecTilt, ivSpecStrTilt, &
        ivSpecEndTilt)
    if (.not.pipinput) write(6, 111) (mapList(i), i = 1, nview)
111 format(/,' Variable mapping list:',(1x,25i3))
  endif
  !
  ! analyze map list
  !
  iref1 = minTiltInd
  iref2 = nviewFix
  if (xyzFixed .and. ioptTilt .ne. 0) then
    iref1 = 0
    iref2 = 0
  endif
  call analyze_maps(tilt, mapTilt, linTilt, frcTilt, fixedTilt, &
      fixedTilt2, iflin, mapList, nview, iref1, iref2, -999., &
      'tilt', var, varName, numVarSearch, mapViewToFile)
  !
  ! set tiltInc so it will give back the right tilt for whatever
  ! mapping or interpolation scheme is used
  !
  do iv = 1, nview
    tiltInc(iv) = 0.
    if (mapTilt(iv) .ne. 0) then
      if (linTilt(iv) > 0) then
        tiltInc(iv) = tilt(iv) - (frcTilt(iv) * var(mapTilt(iv)) + &
            (1. - frcTilt(iv)) * var(linTilt(iv)))
      elseif (linTilt(iv) == - 1) then
        tiltInc(iv) = tilt(iv) - (frcTilt(iv) * var(mapTilt(iv)) + &
            (1. - frcTilt(iv)) * fixedTilt)
      elseif (linTilt(iv) == - 2) then
        tiltInc(iv) = tilt(iv) - (frcTilt(iv) * var(mapTilt(iv)) + &
            (1. - frcTilt(iv)) * fixedTilt2)
      else
        tiltInc(iv) = tilt(iv) - var(mapTilt(iv))
      endif
    endif
  enddo
  numVarAngles = numVarSearch
  !
  ! get reference view to fix magnification at 1.0
  !
  if (ifLocal <= 1) then
    irefTiltIn = mapViewToFile(minTiltInd)
    if (pipinput) then
      ierr = PipGetInteger(PrependLocal('MagReferenceView', ifLocal), &
          irefTiltIn)
    else
      print *, &
          'Enter # of reference view to fix at magnification of 1.0'
      write(*,'(1x,a,i3,a,$)') &
          '   [default =', irefTiltIn, ', view at minimum tilt]: '
      read(5,*) irefTiltIn
    endif
  endif
  irefTilt = nearest_view(irefTiltIn)
  !
  ! get type of mag variable set up
  !
  if (ifLocal <= 1) then
    if (pipinput) then
      ioptMag = 0
      ierr = PipGetInteger(PrependLocal('MagOption', ifLocal), ioptMag)
    else
      print *,'Enter 0 to fix all magnifications at 1.0'
      print *,'      1 to give each view an independent magnification'
      write(*,'(1x,a,/,a,$)') '      2 to specify other mappings of'// &
          ' magnification variables', &
          '       3 or 4 for automatic mapping of groups of views '// &
          '(3 linear, 4 block): '
      read(5,*) ioptMag
    endif
  endif
  !
  ! set up appropriate mapping list or read it in
  !
  iflin = 0
  if (ioptMag <= 0) then
    do i = 1, nview
      mapList(i) = 0
    enddo
  elseif (ioptMag == 1) then
    do i = 1, nview
      mapList(i) = i
    enddo
  elseif (ioptMag == 2) then
    call GetMapList('magnification', 'Mag', mapViewToFile(irefTilt), &
        1.0, ifpip, ifLocal, nview, mapList, mapListMag)
  else
    power = 0.
    if (ioptMag == 3) then
      iflin = 1
      power = powerMag
    endif
    call setGrpSize(tilt, nview, power, groupSize)
    call automap(nview, mapList, groupSize, mapFileToView, nfileViews &
        , ifpip, 1, PrependLocal('MagDefaultGrouping', ifLocal), &
        PrependLocal('MagNondefaultGroup', ifLocal), numInView, ninThresh, &
        ifLocal, nmapDefMag, nRanSpecMag, nmapSpecMag, ivSpecStrMag, &
        ivSpecEndMag)
    if (.not.pipinput) write(6, 111) (mapList(i), i = 1, nview)
  endif
  !
  ! analyze map list
  !
  iref1 = irefTilt
  if (xyzFixed .and. ioptMag .ne. 0) iref1 = 0
  call analyze_maps(gmag, mapGmag, linGmag, frcGmag, fixedGmag, fixdum, &
      iflin, mapList, nview, iref1, 0, 1., 'mag ', var, varName, &
      numVarSearch, mapViewToFile)
  !
  ! set up compression variables if desired: the ones at fixed zero tilt
  ! and at least one other one must have compression of 1.0
  ! get reference view to fix compression at 1.0
  !
  irefComp = 0
  if (ifLocal == 0) then
    if (pipinput) then
      ioptComp = 0
      irefComp = 1
      ierr = PipGetInteger('CompReferenceView', irefComp)
      ierr = PipGetInteger('CompOption', ioptComp)
      if (ioptComp == 0) irefComp = 0
    else
      write(*,'(1x,a,/,a,$)') 'Enter # of reference view to fix at'// &
          ' compression of 1.0,', '   or 0 for no compression: '
      read(5,*) irefComp
    endif
  endif
  iflin = 0
  if (irefComp <= 0) then
    do i = 1, nview
      mapList(i) = 0
    enddo
    irefComp = 1
  else
    irefComp = nearest_view(irefComp)
    !
    ! get type of comp variable set up
    !
    if (.not.pipinput) then
      print *,'Enter 1 to give each view an independent compression'
      write(*,'(1x,a,/,a,$)') '      2 to specify other mappings of' &
          //' compression variables', &
          '       3 or 4 for automatic mapping of groups of views '// &
          '(3 linear, 4 block): '
      read(5,*) ioptComp
    endif
    !
    ! set up appropriate mapping list or read it in
    !
    if (ioptComp == 1) then
      do i = 1, nview
        mapList(i) = i
      enddo
    elseif (ioptComp == 2) then
      call GetMapList('compression', 'Comp', mapViewToFile(irefComp), &
          1., ifpip, 0, nview, mapList, mapList)
    else
      power = 0.
      if (ioptComp == 3) then
        iflin = 1
        power = powerComp
      endif
      call setGrpSize(tilt, nview, power, groupSize)
      call automap(nview, mapList, groupSize, mapFileToView, nfileViews , ifpip, 1, &
          PrependLocal('CompDefaultGrouping', ifLocal), &
          PrependLocal('CompNondefaultGroup', ifLocal), numInView, ninThresh, &
          ifLocal, nmapDefXtilt, nRanSpecXtilt, nmapSpecXtilt, ivSpecStrXtilt, &
          ivSpecEndXtilt)
      ! if (.not.pipinput)
      write(6, 111) (mapList(i), i = 1, nview)
    endif
  endif
  !
  ! if view has tilt fixed at zero, see if mapped to any other
  ! view not fixed at zero tilt; if not, need to fix at 1.0
  !
  do iv = 1, nview
    if (tilt(iv) == 0. .and. mapTilt(iv) == 0) then
      iffix = 1
      do jv = 1, nview
        if (mapList(iv) == mapList(jv) .and. &
            (tilt(jv) .ne. 0. .or. mapTilt(jv) .ne. 0)) iffix = 0
      enddo
      if (iffix == 1) mapList(iv) = mapList(irefComp)
    endif
  enddo
  !
  ! analyze map list
  !
  nvarTmp = numVarSearch
  call analyze_maps(comp, mapComp, linComp, frcComp, fixedComp, fixdum, &
      iflin, mapList, nview, irefComp, 0, 1., 'comp', var, varName, &
      numVarSearch, mapViewToFile)
  !
  numCompSearch = numVarSearch - nvarTmp
  ! get type of distortion variable set up
  !
  if (ifLocal <= 1) then
    if (pipinput) then
      ioptDist(1) = 0
      ioptDist(2) = 0
      ierr = PipGetInteger(PrependLocal('XStretchOption', ifLocal), &
          ioptDist(1))
      ierr = PipGetInteger(PrependLocal('SkewOption', ifLocal), &
          ioptDist(2))
      if (ioptDist(1) > 0 .or. ioptDist(2) > 0) ioptDel = 2
    else
      print *,'Enter 0 to fix all lateral distortions at 0.0'
      print *,'      1 to solve for distortion with the same '// &
          'mappings for stretch and skew'
      write(*,'(1x,a,$)') '      2 to find distortion using '// &
          'different mappings for stretch and skew: '
      read(5,*) ioptDel
    endif
  endif
  !
  ! get reference and dummy views for distortion: take reference on the
  ! same side of minimum as the mag reference, but avoid the minimum tilt
  !
  irefDmag = max(1, nview / 4)
  ivdum = max(1, 3 * nview / 4)
  if (irefDmag == minTiltInd .or. &
      (irefTilt > minTiltInd .and. ivdum .ne. minTiltInd)) then
    irefDmag = ivdum
    ivdum = max(1, nview / 4)
  endif
  !
  power = powerDmag
  do idist = 1, 2
    iflin = 0
    if (ioptDel <= 0 .or. (pipinput .and. ioptDist(idist) <= 0)) then
      do i = 1, nview
        mapList(i) = 0
      enddo
    else
      distmp = distText(idist * min(1, ioptDel - 1) + 1)
      lintmp = linText(idist)
      distOptTmp = distOptText(idist)
      lenOpt = len_trim(distOptTmp)
      if (idist == 1 .or. ioptDel > 1) then
        if (.not. pipinput .and. ifLocal <= 1) then
          print *,'Enter 1 to give each view an independent ', distmp
          write(*,'(1x,a,/,a,$)') '      2 to specify other mappings' &
              //' of '//distmp//' variables', &
              '       3 or 4 for automatic mapping of groups of '// &
              'views ('//lintmp//'): '
          read(5,*) ioptDist(idist)
        endif
        !
        ! set up appropriate mapping list or read it in
        !
        if (ioptDist(idist) <= 1) then
          do i = 1, nview
            mapList(i) = i
          enddo
        elseif (ioptDist(idist) == 2) then
          call GetMapList(distmp, distOptTmp, mapViewToFile(irefTilt), &
              0., ifpip, ifLocal, nview, mapList, mapListDist(1, idist))
        else
          if (idist == 1) then
            if (ioptDist(idist) > 3) iflin = 1
            if (ioptDist(idist) < 3) power = 1.
          else
            if (ioptDist(idist) == 3) then
              iflin = 1
            else
              power = 1.
            endif
          endif
          call setGrpSize(tilt, nview, power, groupSize)
          call automap(nview, mapList, groupSize, mapFileToView, nfileViews, &
              ifpip, 1, PrependLocal(distOptTmp(1:lenOpt)//'DefaultGrouping', ifLocal), &
              PrependLocal(distOptTmp(1:lenOpt)//'NondefaultGroup', ifLocal), &
              numInView, ninThresh, ifLocal, nmapDefDist(idist), nRanSpecDist(idist), &
              nmapSpecDist(1, idist), ivSpecStrDist(1, idist), &
              ivSpecEndDist(1, idist))
          if (.not.pipinput) write(6, 111) (mapList(i), i = 1, nview)
        endif
      endif
    endif
    iref1 = irefDmag
    if (xyzFixed .and. ioptDist(idist) .ne. 0) iref1 = 0
    if (idist == 1) then
      !
      ! analyze map list
      !
      mapDmagStart = numVarSearch + 1
      call analyze_maps(dmag, mapDmag, linDmag, frcDmag, fixedDmag, &
          fixdum, iflin, mapList, nview, iref1, 0, 0., 'dmag', var, &
          varName, numVarSearch, mapViewToFile)
      mapDumDmag = mapDmagStart - 2
      !
      ! provided there are at least two dmag variables, turn
      ! one into a dummy - try for one 1 / 4 of the way through the views
      !
      numDmagVar = numVarSearch + 1 - mapDmagStart
      ! if (numDmagVar > 1 .and. ifLocal <= 1) then
      if (numDmagVar > 1 .and. ifLocal <= - 1) then
        ivl = ivdum
        ivh = ivl + 1
        do while(ivl > 0 .and. ivh <= nview .and. &
            mapDumDmag < mapDmagStart)
          if (ivl > 0) then
            if (mapDmag(ivl) .ne. 0) mapDumDmag = mapDmag(ivl)
            ivl = ivl - 1
          endif
          if (ivh <= nview .and. mapDumDmag < mapDmagStart) then
            if (mapDmag(ivh) .ne. 0) mapDumDmag = mapDmag(ivh)
            ivh = ivh + 1
          endif
        enddo
        !
        ! pack down the variable list in case they mean anything
        !
        do i = mapDumDmag, numVarSearch - 1
          var(i) = var(i + 1)
          varName(i) = varName(i + 1)
        enddo
        !
        ! if a variable is mapped to the dummy, change its mapping
        ! to the endpoint; if it mapped past there, drop it by one to
        ! pack the real variables down
        !
        do i = 1, nview
          if (mapDmag(i) == mapDumDmag) then
            mapDmag(i) = numVarSearch
          elseif (mapDmag(i) > mapDumDmag) then
            mapDmag(i) = mapDmag(i) - 1
          endif
          if (linDmag(i) == mapDumDmag) then
            linDmag(i) = numVarSearch
          elseif (linDmag(i) > mapDumDmag) then
            linDmag(i) = linDmag(i) - 1
          endif
        enddo
        !
        ! now set the dummy variable to the end of list and eliminate
        ! that from the list
        !
        mapDumDmag = numVarSearch
        ! dumDmagFac = 1. / (numDmagVar - 1.)
        dumDmagFac = -1.
        numVarSearch = numVarSearch - 1
      endif
      numVarScaled = numVarSearch
    else
      call analyze_maps(skew, mapSkew, linSkew, frcSkew, fixedSkew, &
          fixdum, iflin, mapList, nview, iref1, 0, 0., 'skew', var, &
          varName, numVarSearch, mapViewToFile)
    endif
    power = powerSkew
  enddo
  !
  ! get type of alpha variable set up
  !
  if (ifLocal <= 1) then
    ioptAlf = 0
    if (pipinput) then
      ierr = PipGetInteger(PrependLocal('XTiltOption', ifLocal), ioptAlf)
    else if (inputAlf .ne. 0) then
      print *,'Enter 0 to fix all X-axis tilts at 0.0'
      print *,'      1 to give each view an independent X-axis tilt'
      write(*,'(1x,a,/,a,$)') '      2 to specify other mappings of' &
          //' X-axis tilt variables', &
          '       3 or 4 for automatic mapping of groups of views ' &
          //'(3 linear, 4 block): '
      read(5,*) ioptAlf
    endif
  endif
  !
  ! set up appropriate mapping list or read it in
  !
  ifAnyAlf = ioptAlf
  iflin = 0
  if (ioptAlf <= 0) then
    do i = 1, nview
      mapList(i) = 0
    enddo
  elseif (ioptAlf == 1) then
    do i = 1, nview
      mapList(i) = i
    enddo
  elseif (ioptAlf == 2) then
    call GetMapList('X-axis tilt', 'XTilt', mapViewToFile(irefTilt), &
        0., ifpip, ifLocal, nview, mapList, mapListXtilt)
  else
    power = 0.
    if (ioptAlf == 3) then
      iflin = 1
      power = powerAlf
    endif
    call setGrpSize(tilt, nview, power, groupSize)
    call automap(nview, mapList, groupSize, mapFileToView, nfileViews &
        , ifpip, 1, PrependLocal('XTiltDefaultGrouping', ifLocal), &
        PrependLocal('XTiltNondefaultGroup', ifLocal), numInView, ninThresh, &
        ifLocal, nmapDefXtilt, nRanSpecXtilt, nmapSpecXtilt, ivSpecStrXtilt, &
        ivSpecEndXtilt)
    if (.not.pipinput) write(6, 111) (mapList(i), i = 1, nview)
  endif
  !
  ! analyze map list - fix reference at minimum tilt
  !
  mapAlfStart = numVarSearch + 1
  call analyze_maps(alf, mapAlf, linAlf, frcAlf, fixedAlf, fixdum, iflin, &
      mapList, nview, minTiltInd, 0, 0., 'Xtlt', var, varName, numVarSearch, &
      mapViewToFile)
  mapAlfEnd = numVarSearch
  !
  ! Add projection stretch variable if desired.
  !
  mapProjStretch = 0
  mapBeamTilt = 0
  ifBTSearch = 0
  if (ifLocal == 0) then
    projStrRot = defRot
    projSkew = 0.
    beamTilt = 0.
    if (pipinput) then
      ierr = PipGetBoolean('ProjectionStretch', mapProjStretch)
      ierr = PipGetInteger('BeamTiltOption', ifBTSearch)
      ierr = PipGetFloat('FixedOrInitialBeamTilt', beamTilt)
      beamTilt = beamTilt * dtor
    endif
    if (mapProjStretch > 0) then
      mapProjStretch = numVarSearch + 1
      numVarSearch = numVarSearch + 1
      ! varName(mapProjStretch) = 'projstr '
      varName(mapProjStretch) = 'projskew'
      var(mapProjStretch) = 0.
      ! var(mapProjStretch + 1) = 0.
    endif
    !
    ! If beam tilt option is 1, set up variable and set search back to 0
    !
    if (ifBTSearch == 1) then
      numVarSearch = numVarSearch + 1
      mapBeamTilt = numVarSearch
      varName(mapBeamTilt) = 'beamtilt'
      var(mapBeamTilt) = beamTilt
      ifBTSearch = 0
    endif
  endif
  !
  if (ifLocal > 1) return
  if (.not.xyzFixed .and. numCompSearch .ne. 0 .and. &
      (ioptTilt == 1 .or. (ioptTilt > 1 .and. nviewFix == 0))) &
      write(*,109)
109 format(/,'WARNING: ONLY ONE TILT ANGLE IS FIXED -',/, &
      'WARNING: THERE SHOULD BE TWO FIXED TILT ANGLES WHEN DOING ', &
      'COMPRESSION')
  if (ioptAlf .ne. 0 .and. ifRotFix .ne. - 1 .and. ifRotFix .ne. - 2 .and. &
      mapAlfEnd > mapAlfStart) write(*,110)
110 format(/,'WARNING: YOU ARE ATTEMPTING TO SOLVE FOR X-AXIS TILTS AND ', &
      'ROTATION ANGLE -',/,'WARNING: RESULTS WILL BE VERY UNRELIABLE; TRY', &
      ' SOLVING FOR JUST ONE ROTATION')
  if (ioptDist(2) .ne. 0 .and. ifRotFix .ne. - 1 .and. ifRotFix .ne. - 2 .and. &
      (ifBTSearch .ne. 0 .or. mapBeamTilt .ne. 0)) write(*,112)
112 format(/,'WARNING: YOU ARE TRYING TO SOLVE FOR BEAM TILT, SKEW, AND ', &
      'ROTATION ANGLE -',/,'WARNING: THIS IS ALMOST IMPOSSIBLE; TRY', &
      ' SOLVING FOR JUST ONE ROTATION')
  localText = ' '
  if (ifLocal == 1) localText = ' for lower left local area'
  if (inputAlf == 0 .and. ifAnyAlf == 0) then
    write(*,101) localText
101 format(/,24x,'Variable mappings',a,/,' View  Rotation     Tilt', &
        '  (+ incr.)      Mag       Comp      Dmag      Skew')
  else
    write(*,1101) localText
1101 format(/,24x,'Variable mappings',a,/,' View Rotation    Tilt ', &
        ' (+ incr.)   Mag      Comp     Dmag      Skew    X-tilt')
  endif
  do iv = 1, nview
    do i = 1, 8
      dump(i) = '  fixed '
    enddo
    dump(3) = '        '
    call setDumpName(mapRot(iv), linRot(iv), -1, varName, dump(1))
    if (mapTilt(iv) > 0) then
      call setDumpName(mapTilt(iv), linTilt(iv), -1, varName, dump(2))
      if (abs(tiltInc(iv)) > 5.e-3 .and. &
          (ifLocal == 0 .or. incrTilt == 0)) &
          write(dump(3), '(a2,f6.2)') '+ ', tiltInc(iv) / dtor
    endif
    call setDumpName(mapGmag(iv), linGmag(iv), -1, varName, dump(4))
    call setDumpName(mapComp(iv), linComp(iv), -1, varName, dump(5))
    call setDumpName(mapDmag(iv), linDmag(iv), mapDumDmag, varName, &
        dump(6))
    call setDumpName(mapSkew(iv), linSkew(iv), -1, varName, dump(7))
    call setDumpName(mapAlf(iv), linAlf(iv), -1, varName, dump(8))
    if (inputAlf == 0 .and. ifAnyAlf == 0) then
      write(*,'(i4,3x,a8,3x,a8,1x,a8,3x,a8,3x,a8,3x,a8,3x,a8)') &
          mapViewToFile(iv), (dump(i), i = 1, 7)
    else
      write(*,'(i4,2x,a8,2x,a8,a7,2x,a8,1x,a8,1x,a8,2x,a8,2x,a8)') &
          mapViewToFile(iv), (dump(i), i = 1, 8)
    endif
  enddo
  write(*,*)
  return
end subroutine input_vars

subroutine setDumpName(map, lin, mapdum, varName, dump)
  implicit none
  character*8 varName(*), dump
  integer*4 map, mapdum, lin
  if (map == 0) return
  if (map == mapdum) then
    if (lin == 0) then
      dump = 'dummy'
    elseif (lin < 0) then
      dump = 'dum+fix'
    else
      dump = 'dum+'//varName(lin) (1:1) //varName(lin) (6:8)
    endif
  else
    if (lin == 0) then
      dump = varName(map)
    elseif (lin < 0) then
      dump = varName(map) (1:1) //varName(map) (6:8) //'+fix'
    elseif (lin == mapdum) then
      dump = varName(map) (1:1) //varName(map) (6:8) //'+dum'
    else
      dump = varName(map) (1:1) //varName(map) (6:8) //'+'// &
          varName(lin) (6:8)
    endif
  endif
  return
end subroutine setDumpName


! Reloads the values of a variable vlocal for local fits from the global
! values in GLB.  MAP is the mapping from variable index to view for
! this variable, FRC is the fraction for linear fits, NVIEW is number
! of views, mapStart and mapEnd are the start and end of variable range
! for this parameter, VAR is the master variable list which is also
! initialized, FIXED is to filled with the fixed value where the map is
! 0, INCR indicates that the  variable is being done incrementally, and
! mapLocalToAll is the mapping from the current set of views to the
! global views.
!
subroutine reload_vars(glb, vlocal, map, frc, nview, &
    mapStart, mapEnd, var, fixed, incr, mapLocalToAll)
  implicit none
  integer*4 map(*), nview, mapStart, mapEnd, incr, mapLocalToAll(*)
  real*4 glb(*), frc(*), var(*), vlocal(*), fixed
  !
  integer*4 i, m, nsum
  real*4 sum
  !
  ! first just reload all of the local parameters from the global
  ! fixed will have the right value for any case where there is ONE
  ! fixed parameter
  !
  do i = 1, nview
    m = mapLocalToAll(i)
    vlocal(i) = glb(m)
    if (map(i) == 0) fixed = glb(m)
  enddo
  !
  ! if doing incremental, set all the var's to 0 and return
  !
  if (incr .ne. 0) then
    do m = mapStart, mapEnd
      var(m) = 0.
    enddo
    fixed = 0.
    return
  endif
  !
  ! next set the value of each variable as the average value of all
  ! the parameters that map solely to it
  !
  do m = mapStart, mapEnd
    sum = 0.
    nsum = 0
    do i = 1, nview
      if (map(i) == m .and. frc(i) == 1.) then
        sum = sum + glb(mapLocalToAll(i))
        nsum = nsum + 1
      endif
    enddo
    if (nsum == 0) then
      print *
      print *,'ERROR: TILTALIGN - Nothing mapped to variable', m
      call exit(1)
    endif
    var(m) = sum / nsum
  enddo
  return
end subroutine reload_vars


! Counts the number of points in each view, given list of nrealPt point
! numbers in listReal, starting index of each real point in irealStr,
! and array of view numbers for all the points in isecView.  Returns
! the count in numInView
!
subroutine countNumInView(listReal, nrealPt, irealStr, isecView, nview, &
    numInView)
  implicit none
  integer*4 listReal(*), nrealPt, irealStr(*), isecView(*), numInView(*)
  integer*4 nview, i, j, ist, ind
  !
  do i = 1, nview
    numInView(i) = 0
  enddo
  do j = 1, nrealPt
    ist = irealStr(listReal(j))
    ind = irealStr(listReal(j) + 1) - 1
    do i = ist, ind
      numInView(isecView(i)) = numInView(isecView(i)) + 1
    enddo
  enddo
  return
end subroutine countNumInView


! Takes a variable from a local solution with some omitted views and
! expands the variable to the original global views, filling in the
! empty spots by interpolation, or copying at the ends.  VAR is the
! variable array, dimensioned VAR(IDIM,*), and INDEX is the index for
! the first dimension; nvLocal is the local # of views, NVIEW is the
! global # of views, mapa2l is the mapping from all to local views
!
subroutine expandLocalToAll(var, idim, index, nvLocal, nview, mapa2l)
  implicit none
  integer*4 idim, index, nview, mapa2l(*)
  real*4 var(idim,*)
  integer*4 ioutLast, iout, ioutNext, nvLocal
  real*4 varLast, f
  !
  if (nvLocal == nview) return
  ioutLast = -1
  do iout = nview, 1, -1
    if (mapa2l(iout) > 0) then
      varLast = var(index, mapa2l(iout))
      var(index, iout) = varLast
      ioutLast = iout
    else
      !
      ! Find the next one down
      ioutNext = iout - 1
      do while (ioutNext > 1 .and. mapa2l(max(1, ioutNext)) == 0)
        ioutNext = ioutNext - 1
      enddo
      !
      ! If there is no next one (at the bottom), use the last one
      ! If there is no last one, use the next one
      ! if there are both, interpolate
      if (ioutNext == 0 .or. mapa2l(max(1, ioutNext)) == 0) then
        var(index, iout) = varLast
      else if (ioutLast < 1) then
        var(index, iout) = var(index, mapa2l(ioutNext))
      else
        f = float(iout - ioutNext) / float(ioutLast - ioutNext)
        var(index, iout) = f * varLast + (1 - f) * &
            var(index, mapa2l(ioutNext))
        ! print *,iout, ioutLast, ioutNext, mapa2l(ioutNext), mapa2l(ioutNext), &
        ! varLast, var(index, mapa2l(ioutNext)), var(index, iout)
      endif
    endif
  enddo
  return
end subroutine expandLocalToAll

! Finds the nearest existing view to the given file view number
!
integer*4 function nearest_view(iview)
  use alivar
  implicit none
  integer*4 iview, ivdel
  if (iview > nfileViews .or. iview <= 0) then
    print *,'View', iview, ' is beyond the known range of the file'
    call exit(1)
  endif
  nearest_view = mapFileToView(iview)
  ivdel = 1
  do while(nearest_view == 0)
    if (iview - ivdel > 0) then
      if (mapFileToView(iview - ivdel) > 0) &
          nearest_view = mapFileToView(iview - ivdel)
    endif
    if (iview + ivdel <= nfileViews) then
      if (mapFileToView(iview + ivdel) > 0) &
          nearest_view = mapFileToView(iview + ivdel)
    endif
    ivdel = ivdel + 1
  enddo
  return
end function nearest_view

character*40 function PrependLocal(string, ifLocal)
  implicit none
  character*(*) string
  integer*4 ifLocal
  PrependLocal = string
  if (ifLocal .ne. 0) PrependLocal = 'Local'//trim(string)
  return
end function PrependLocal


! Gets a specific mapping list for the variable named varName.  OPTION
! is a base name for the option, IREF specifies which is the reference
! view if nonzero, FIXVAL is the value there, IFPIP and NVIEW
! have the usual meanings, mapList is returned with the mappings.
! If ifLocal is 0 or 1 the map list is copied into mapSave; if it is 2
! then mapList is simply copied from mapSave.
!
subroutine GetMapList(varName, option, iref, fixval, ifpip, ifLocal, &
    nview, mapList, mapSave)
  implicit none
  character*(*) varName, option
  integer*4 mapList(*), mapSave(*)
  integer*4 ifLocal, ifpip, nview, i, iref, len, numEntry, numTot, numGot
  real*4 fixval
  character*40 mapOption
  character*40 PrependLocal
  integer*4 PipNumberOfEntries, PipGetIntegerArray

  if (ifLocal > 1) then
    do i = 1, nview
      mapList(i) = mapSave(i)
    enddo
    return
  endif
  !
  if (ifpip == 0) then
    len = len_trim(varName)
    print *,'For each view, enter a ', varName(1:len), ' variable #'
    if (iref > 0) write(*,'(a,a,a,f4.1,a,i4)') '   to fix ', &
        varName(1:len), ' of a view at' &
        , fixval, ' give it the same variable # as view', iref
    read(5,*) (mapList(i), i = 1, nview)
  else
    mapOption = PrependLocal(trim(option)//'Mapping', ifLocal)
    numEntry = 0
    len = PipNumberOfEntries(mapOption, numEntry)
    numTot = 0
    do i = 1, numEntry
      numGot = 0
      len = PipGetIntegerArray(mapOption, mapList(numTot + 1), &
          numGot, nview - numTot)
      numTot = numTot + numGot
    enddo
    if (numTot < nview) call errorExit( &
        'NOT ENOUGH MAPPING VALUES ENTERED WITH '//mapOption, 0)
  endif
  do i = 1, nview
    mapSave(i) = mapList(i)
  enddo
  return
end subroutine GetMapList

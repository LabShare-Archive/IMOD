! MAP_VARS.F  -  Routines for variable mapping
!
! ANALYZE_MAPS
! automap
! inputGroupings
! makeMapList
! groupList
! groupMinMax
! setGrpSize
! mapSeparateGroup
!
! $Id$
!
!
! ANALYZE_MAPS takes a map list and other information about how to set
! up a mapping and allocates variables, and mappings for getting
! values from variables
! GMAG is the variable array, one per view
! MAPGMAP is filled with mappings to solution variables in VAR
! linGmag is filled with mapping to second variable for linear mapping
! fracGmag is filled with the fractions for linear mapping
! fixedGmag is the fixed value that is ramped to in linear mappings
! fixedGmag2 is a second fixed value to be ramped to for second ref
! IFLIN is 1 for linear mapping, 0 for block
! mapList is the list of relative numbers to indicate grouping
! NVIEW is the # of views in the solution
! irefTilt is the reference view to be set to zero (0 if none)
! IREF2 is a second reference view to be set to zero (0 if none)
! DEFVAL is a default value to fill the array with, if not - 999.
! NAME is a 4 character string with variable name
! VAR is the solution variable array for minimization
! varName is an array of 8 - character strings filled with map names
! numVarSearch is input with an returns number of variables in VAR
! mapViewToFile is mapping from solution views to file views
!
subroutine analyze_maps(gmag, mapGmag, linGmag, fracGmag, fixedGmag, &
    fixedGmag2, iflin, mapList, nview, irefTilt, iref2, &
    defval, name, var, varName, numVarSearch, mapViewToFile)
  use arrayMaxes
  implicit none
  integer*4 mapGmag(*), mapList(*), linGmag(*), mapViewToFile(*)
  integer*4 mapVarNum(maxView)
  real*4 gmag(*), var(*), fracGmag(*)
  character*(*) varName(*), name
  real*4 fixedGmag, fixedGmag2, defval
  integer*4 iflin, nview, irefTilt, iref2, numVarSearch
  !
  integer*4 numMagSearch, iv, mapref1, mapref2, imap, it, listMax, ivmin, ivmax
  integer*4 numInGroup, nextMinView, nextMaxView, numInNext, nadd, ivadd, ivfix, linval
  integer*4 ivvar, lastAdded, iadd
  !
  numMagSearch = 0
  do iv = 1, nview
    if (defval .ne. - 999.) gmag(iv) = defval
  enddo
  mapref1 = -999
  if (irefTilt > 0) mapref1 = mapList(irefTilt)
  mapref2 = -999
  if (iref2 > 0) mapref2 = mapList(iref2)
  if (iflin <= 0) then
    do iv = 1, nview
      mapGmag(iv) = 0
      linGmag(iv) = 0
      fracGmag(iv) = 1.
      if (mapList(iv) .ne. mapref1 .and. mapList(iv) .ne. mapref2) then
        !
        ! if value is same as for minimum tilt, all set for fixed mag
        ! otherwise see if this var # is already encountered
        !
        imap = 0
        do it = 1, numMagSearch
          if (mapList(iv) == mapVarNum(it)) imap = it
        enddo
        if (imap == 0) then
          !
          ! if not, need to add another mag variable
          !
          numMagSearch = numMagSearch + 1
          mapVarNum(numMagSearch) = mapList(iv)
          var(numVarSearch + numMagSearch) = gmag(iv)
          write(varName(numVarSearch + numMagSearch), '(a4,i4)') name, &
              mapViewToFile(iv)
          imap = numMagSearch
        endif
        !
        ! now map the mag to the variable
        !
        mapGmag(iv) = numVarSearch + imap
      endif
    enddo
  else
    !
    ! LINEAR MAPPING OPTION
    !
    listMax = 0
    do iv = 1, nview
      listMax = max(listMax, mapList(iv))
    enddo
    !
    ! go through the variable groupings
    !
    do imap = 1, listMax
      call groupMinMax(mapList, nview, imap, ivmin, ivmax, numInGroup)
      !
      ! if any in this group:
      !
      if (numInGroup > 0) then
        call groupMinMax(mapList, nview, imap + 1, nextMinView, nextMaxView, &
            numInNext)
        !
        ! if any in next group, the min view in that group is the
        ! endpoint for interpolation, otherwise the max view in this
        ! group is the endpoint
        !
        nadd = 2
        ivadd = ivmin
        ivfix = 0
        linval = -1
        if (numInNext == 0) then
          nextMinView = ivmax
          !
          ! ending and only two in group, add only one
          !
          if (numInGroup <= 2) nadd = 1
          !
          ! but if this one is fixed group, add end to stack unless
          ! only 2, in which case add nothing
          !
          if (imap == mapref1 .or. imap == mapref2) then
            mapGmag(ivmin) = 0
            ivadd = nextMinView
            ivfix = ivmin
            ivvar = nextMinView
            if (numInGroup <= 2) nadd = 0
            if (imap == mapref2) linval = -2
          endif
        else
          !
          ! if continuing and this one is fixed, add next start to list
          !
          if (imap == mapref1 .or. imap == mapref2) then
            mapGmag(ivmin) = 0
            ivadd = nextMinView
            !
            ! but if next one is fixed also, use this one's end instead
            !
            if (imap + 1 == mapref1 .or. imap + 1 == mapref2) ivadd = ivmax
            nadd = 1
            ivfix = ivmin
            ivvar = ivadd
            if (imap == mapref2) linval = -2
            !
            ! if continuing and next one is fixed, add this start to list
            !
          elseif (imap + 1 == mapref1 .or. imap + 1 == mapref2) then
            nadd = 1
            ivfix = nextMinView
            ivvar = ivmin
            if (imap + 1 == mapref2) linval = -2
          endif
        endif
        !
        ! add the starting and ending views for interpolation as
        ! variables, if they are not already on the list
        !
        do iadd = 1, nadd
          if (numMagSearch == 0 .or. lastAdded .ne. ivadd) then
            numMagSearch = numMagSearch + 1
            var(numVarSearch + numMagSearch) = gmag(ivadd)
            write(varName(numVarSearch + numMagSearch), '(a4,i4)') name, &
                mapViewToFile(ivadd)
            lastAdded = ivadd
            mapGmag(ivadd) = numVarSearch + numMagSearch
            linGmag(ivadd) = 0
            fracGmag(ivadd) = 1.
          endif
          ivadd = nextMinView
        enddo
        !
        !
        ! now go through rest of views (excluding endpoints), and for
        ! each one in the group, map to next to last variable and set
        ! fraction to the fraction of the distance in view number from
        ! the starting to the ending point
        !
        if (ivfix == 0 .and. (numInNext > 0 .or. numInGroup > 2)) then
          do iv = ivmin + 1, ivmax
            if (iv .ne. nextMinView .and. mapList(iv) == imap) then
              mapGmag(iv) = mapGmag(ivmin)
              linGmag(iv) = mapGmag(nextMinView)
              fracGmag(iv) = float(nextMinView - iv) / float(nextMinView - ivmin)
            endif
          enddo
        elseif (ivfix == 0) then
          !
          ! ending, not fixed, and only two in group: map 2nd to first
          !
          mapGmag(ivmax) = mapGmag(ivmin)
          linGmag(ivmax) = 0
          fracGmag(ivmax) = 1.
        elseif (numInNext == 0 .and. numInGroup <= 2) then
          !
          ! ending in a fixed group of 2
          !
          mapGmag(ivmax) = 0
        else
          !
          ! linear ramp between fixed and variable
          !
          do iv = ivmin, ivmax
            if (iv .ne. ivfix .and. iv .ne. ivvar .and. mapList(iv) &
                == imap) then
              mapGmag(iv) = mapGmag(ivvar)
              linGmag(iv) = linval
              fracGmag(iv) = float(ivfix - iv) / float(ivfix - ivvar)
            endif
          enddo
        endif
        !
        ! set the fixed value that is being ramped to
        !
        if (imap == mapref1) fixedGmag = gmag(ivfix)
        if (imap == mapref2) fixedGmag2 = gmag(ivfix)
      endif
    enddo


  endif
  numVarSearch = numVarSearch + numMagSearch
  return
end subroutine analyze_maps


subroutine automap(nview, mapList, groupSize, mapFileToView, nFileViews, &
    ifpip, ifRequired, defaultOption, nonDefaultOption, numInView, ninThresh, &
    ifLocal, nmapDef, nRanSpecIn, ivSpecStrIn, ivSpecEndIn, nmapSpecIn)
  ! module needed for MAXGRP definition
  use arrayMaxes
  implicit none
  integer*4 mapList(*), nview, nFileViews, ifpip, ifRequired, ninThresh
  real*4 groupSize(*)
  integer*4 mapFileToView(*), numInView(*)
  character*(*) defaultOption, nonDefaultOption
  integer*4 ivSpecStrIn(MAXGRP), ivSpecEndIn(MAXGRP), nmapSpecIn(MAXGRP)
  integer*4 nmapDef, nRanSpecIn, ifLocal
  !
  if (ifLocal <= 1) call inputGroupings(nFileViews, ifpip, ifRequired, &
      defaultOption, nonDefaultOption, nmapDef, ivSpecStrIn, &
      ivSpecEndIn, nmapSpecIn, nRanSpecIn, MAXGRP)
  call makeMapList(nview, mapList, groupSize, mapFileToView, nFileViews, &
      nmapDef, ivSpecStrIn, ivSpecEndIn, nmapSpecIn, nRanSpecIn, numInView, &
      ninThresh)
  return
end subroutine automap


! Input the separate groups and check that not every view is in each one
!
subroutine inputSeparateGroups(ngsep, nsepInGrp, ivsep, listString)
  use alivar
  implicit none
  integer*4 ngsep, nsepInGrp(*), ivsep(maxView, *)
  character*(*) listString
  integer*4 ig, iv, i, ierr, PipGetString, PipNumberOfEntries

  ierr = PipNumberOfEntries('SeparateGroup', ngsep)
  if (ngsep > MAXGRP) call errorExit('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
  do ig = 1, ngsep
    ierr = PipGetString('SeparateGroup', listString)
    call parseList2(listString, ivsep(1, ig), nsepInGrp(ig), maxView)

    ! Check each view to see if it is in the group
    do iv = 1, nview
      ierr = 0
      do i = 1, nsepInGrp(ig)
        if (ivsep(i, ig) == iv) then
          ierr = 1
          exit
        endif
      enddo
      
      ! We get here with a 1 if it is in, so if there is ever a view not in the group, 
      ! the group is OK
      if (ierr == 0) then
        exit
      endif
    enddo
    if (ierr > 0) call errorExit('THIS ENTRY FOR A SEPARATE GROUP CONTAINS ALL VIEWS: ' &
        // trim(listString), 0)
  enddo
  return
end subroutine inputSeparateGroups


! Inputs the groupings for one variable
!
subroutine inputGroupings(nFileViews, ifpip, ifRequired, defaultOption, &
    nonDefaultOption, nmapDef, ivSpecStrIn, ivSpecEndIn, nmapSpecIn, nRanSpecIn, MAXGRP)
  implicit none
  integer*4 nFileViews, ifpip, ifRequired, nmapDef, MAXGRP, nRanSpecIn
  character*(*) defaultOption, nonDefaultOption
  integer*4 ivSpecStrIn(*), ivSpecEndIn(*), nmapSpecIn(*)
  !
  integer*4 iran, ivstr, ivend, ierr
  integer*4 PipGetInteger, PipNumberOfEntries, PipGetThreeIntegers
  !
  nRanSpecIn = 0
  if (ifpip == 0) then
    print *,'Enter the negative of a group size to NOT treat ', &
        'any views separately here.'
    write(*,'(1x,a,$)') 'Default group size, # of ranges'// &
        ' with special group sizes: '
    read(5,*) nmapDef, nRanSpecIn
  else
    if (PipGetInteger(defaultOption, nmapDef) > 0) then
      if (ifRequired == 0) return
      print *
      print *,'ERROR: AUTOMAP - OPTION ', trim(defaultOption), &
          ' MUST BE ENTERED'
      call exit(1)
    endif
    ierr = PipNumberOfEntries(nonDefaultOption, nRanSpecIn)
  endif
  !
  if (nRanSpecIn > MAXGRP) then
    print *,'ERROR: AUTOMAP - TOO MANY NONDEFAULT GROUPINGS FOR ARRAYS'
    call exit(1)
  endif
  !
  do iran = 1, nRanSpecIn
    if (ifpip == 0) then
      write(*,'(1x,a,i3,a,$)') 'Starting and ending views in range', &
          iran, ', group size: '
      read(5,*) ivstr, ivend, nmapSpecIn(iran)
    else
      ierr = PipGetThreeIntegers(nonDefaultOption, ivstr, ivend, &
          nmapSpecIn(iran))
    endif
    if (ivstr > ivend) then
      print *,'ERROR: AUTOMAP - Start past end of range:', ivstr, ivend
      call exit(1)
    endif
    if (ivstr < 0 .or. ivstr > nFileViews .or. ivend < 0 .or. &
        ivend > nFileViews) then
      print *,'ERROR: AUTOMAP - View number not in file: ', ivstr, ivend
      call exit(1)
    endif
    ivSpecStrIn(iran) = ivstr
    ivSpecEndIn(iran) = ivend
  enddo
  return
end subroutine inputGroupings

! Makes a mapping list, consisting of group number for each view returned
! in mapList.  NVIEW is the current number of views, groupSize is an array
! of relative group sizes for those views, mapFileToView is a mapping
! from file views to current views, nFileViews is the number if file
! views.  nmapDef is the default group size, nRanSpecIn is the number of
! special ranges, ivSpecStrIn and ivSpecEndIn are arrays with the
! starting and ending file views in each range, nmapSpecIn is and array
! with the group size in each range.  numInView is an array with the number
! of points in each view and ninThresh has a threshold number required
! for counting a view toward the number needed to form a group.  If
! ninThresh is zero, the contents of numInView are ignored.

subroutine makeMapList(nview, mapList, groupSize, mapFileToView, nFileViews, &
    nmapDef, ivSpecStrIn, ivSpecEndIn, nmapSpecIn, nRanSpecIn, numInView, &
    ninThresh)
  use mapSep

  implicit none
  integer*4 mapList(*), nview, nFileViews, nmapDef, nRanSpecIn, numInView(*)
  real*4 groupSize(*)
  integer*4 mapFileToView(*), ninThresh
  integer*4 ivSpecStrIn(*), ivSpecEndIn(*), nmapSpecIn(*)
  integer*4 ivSpecStr(MAXGRP), ivSpecEnd(MAXGRP), nmapSpec(MAXGRP)
  integer*4 inran(maxView)
  !
  integer*4 nranSpec, iran, ivstr, ivend, nran, ir, ivar
  integer*4 ninRange, iv, ig, ifsep, jj, ii, ierr
  !
  ! First process special ranges
  !
  nranSpec = 0
  do iran = 1, nRanSpecIn
    ivstr = ivSpecStrIn(iran)
    ivend = ivSpecEndIn(iran)
    !
    ! convert and trim nonexistent views from range
    !
    do while(ivstr <= ivend .and. mapFileToView(ivstr) == 0)
      ivstr = ivstr + 1
    enddo
    do while(ivstr <= ivend .and. mapFileToView(ivend) == 0)
      ivend = ivend - 1
    enddo
    !
    ! if there is still a range, add it to list
    !
    if (ivstr <= ivend) then
      nranSpec = nranSpec + 1
      ivSpecStr(nranSpec) = mapFileToView(ivstr)
      ivSpecEnd(nranSpec) = mapFileToView(ivend)
      nmapSpec(nranSpec) = nmapSpecIn(iran)
      ! print *,ivSpecStr(nranSpec), ivSpecEnd(nranSpec)
    endif
  enddo
  !
  ! build list of all uninterrupted ranges
  !
  nran = nranSpec + 1
  ivSpecStr(nran) = 1
  ivSpecEnd(nran) = nview
  nmapSpec(nran) = nmapDef
  do iran = 1, nranSpec
    ir = nranSpec + 1
    !
    ! for each special range, scan rest of ranges and see if they overlap
    !
    do while (ir <= nran)
      if (.not.(ivSpecStr(iran) > ivSpecEnd(ir) .or. &
          ivSpecEnd(iran) < ivSpecStr(ir))) then
        !
        ! overlap: then look at three cases
        !
        if (ivSpecStr(iran) <= ivSpecStr(ir) .and. &
            ivSpecEnd(iran) >= ivSpecEnd(ir)) then
          !
          ! CASE #1: complete overlap: wipe out the range from the list,
          ! move rest of list down
          !
          nran = nran - 1
          do ii = ir, nran
            ivSpecStr(ii) = ivSpecStr(ii + 1)
            ivSpecEnd(ii) = ivSpecEnd(ii + 1)
            nmapSpec(ii) = nmapSpec(ii + 1)
          enddo
        elseif (ivSpecStr(ir) < ivSpecStr(iran) .and. &
            ivSpecEnd(ir) > ivSpecEnd(iran)) then
          !
          ! CASE #2: interior subset; need to split range in 2
          !
          nran = nran + 1
          ivSpecEnd(nran) = ivSpecEnd(ir)
          ivSpecStr(nran) = ivSpecEnd(iran) + 1
          nmapSpec(nran) = nmapSpec(ir)
          ivSpecEnd(ir) = ivSpecStr(iran) - 1
        else
          !
          ! CASE #3, subset at one end, need to truncate range
          !
          if (ivSpecStr(iran) > ivSpecStr(ir)) then
            ivSpecEnd(ir) = ivSpecStr(iran) - 1
          else
            ivSpecStr(ir) = ivSpecEnd(iran) + 1
          endif
        endif
      endif
      ir = ir + 1
    enddo
    ! write(*,'(4i4)') (i, ivSpecStr(i), ivSpecEnd(i), nmapSpec(i), i = 1, &
    ! nran)
  enddo
  ivar = 1
  !
  ! for each range, make list of views in range; exclude the special
  ! ones unless nmap is negative
  !
  do iran = 1, nran
    ninRange = 0
    do iv = ivSpecStr(iran), ivSpecEnd(iran)
      ifsep = 0
      if (nmapSpec(iran) > 0) then
        do ig = 1, ngsep
          do jj = 1, nSepInGrp(ig)
            if (iv == ivsep(jj, ig)) ifsep = 1
          enddo
        enddo
      endif
      if (ifsep == 0) then
        ninRange = ninRange + 1
        inran(ninRange) = iv
      endif
    enddo
    !
    ! get the ones in range grouped; then group each special set
    !
    call groupList(inran, ninRange, abs(nmapSpec(iran)), groupSize, numInView, &
        ninThresh, ivar, mapList)
    if (nmapSpec(iran) > 0) then
      do ig = 1, ngsep
        ninRange = 0
        do jj = 1, nSepInGrp(ig)
          if (ivsep(jj, ig) >= ivSpecStr(iran) .and. &
              ivsep(jj, ig) <= ivSpecEnd(iran)) then
            ninRange = ninRange + 1
            inran(ninRange) = ivsep(jj, ig)
          endif
        enddo
        call groupList(inran, ninRange, nmapSpec(iran), groupSize, numInView, &
            ninThresh, ivar, mapList)
      enddo
    endif
  enddo
  ! write(*,'(26i3)') (mapList(i), i = 1, nview)
  return
end subroutine makeMapList


! Determines groupings for views in a range.  INRAN has the list of
! views in the range, ninRange is the number of views in the list.  NMAP
! is the average group size, and groupSize is an array with relative group
! sizes for all views.  numInView has the number of points in each view,
! and ninThresh is the threshold number required to count a view toward
! the total of views in a group.  IVAR comes in with the first free group
! number and is returned at two past the last group number to avoid
! connections. mapList is an array indexed by view number and is filled
! with the group numbers.

subroutine groupList(inran, ninRange, nmap, groupSize, numInView, ninThresh, ivar, &
    mapList)
  implicit none
  integer*4 inran(*), mapList(*), numInView(*)
  real*4 groupSize(*)
  integer*4 ninRange, nmap, ivar, ninThresh
  !
  integer*4 nsets, isetStr, isetEnd, lastMap, iset, ninVar, i, iran, ifFew, nextRan
  real*4 setSum, setcum, setTarg, cumNext
  !
  if (ninRange == 0) return
  !
  ! First determine if there are any views below theshold - this triggers
  ! a priority toward having groups include a minimum # of views above
  ! threshold, rather than a priority of maintaining a maximum group size
  ifFew = 0
  if (ninThresh > 0) then
    do iran = 1, ninRange
      if (numInView(inran(iran)) < ninThresh) ifFew = 1
    enddo
  endif
  !
  ! compute the expected number of sets, taking into account the relative
  ! groupings
  setSum = 0.
  if (ifFew == 0) then
    !
    ! Count all views from start to end of range
    do i = inran(1), inran(ninRange)
      setSum = setSum + 1. / (groupSize(i) * nmap)
    enddo
  else
    !
    ! Or count only views in range and above threshold for # of points
    do iran = 1, ninRange
      i = inran(iran)
      if (numInView(i) >= ninThresh) setSum = setSum + 1. / (groupSize(i) * nmap)
    enddo
  endif
  nsets = max(1, nint(setSum))

  ! if (ifFew .ne. 0) print *,'setsum', setSum, '  nsets', nsets
  isetStr = inran(1)
  isetEnd = isetStr - 1
  lastMap = isetStr
  setcum = 0.
  iran = 0
  cumNext = 0.
  do iset = 1, nsets
    !
    ! find the ending point that is at or before the next target sum
    ! for a set
    !
    setTarg = (iset * setSum) / nsets
    if (ifFew == 0) then
      !
      ! Simple case: advance as long as the next one will be below or
      ! just at the target
      do while(isetEnd < inran(ninRange) .and. &
          setcum + 1. / (groupSize(isetEnd + 1) * nmap) <= setTarg + 0.01)
        isetEnd = isetEnd + 1
        setcum = setcum + 1. / (groupSize(isetEnd) * nmap)
      enddo
    else
      ! print *,'set', iset, '  settarg', setTarg
      !
      ! Complex case: index on view in list and advance as long as cumul
      ! for next above threshold on list is not past target
      do while (iran < ninRange .and. cumNext <= setTarg + 0.01)
        !
        ! Advance index to next, skipping ones below threshold
        iran = iran + 1
        do while (iran < ninRange)
          if (numInView(inran(iran)) >= ninThresh) exit
          iran = iran + 1
        enddo
        !
        ! Set view number of end, and get cumulative number
        isetEnd = inran(iran)
        setcum = setcum + 1. / (groupSize(isetEnd) * nmap)
        !
        ! if not at end, get next eligible one
        if (iran < ninRange) then
          nextRan = iran + 1
          do while (nextRan < ninRange)
            if (numInView(inran(nextRan)) >= ninThresh) exit
            nextRan = nextRan + 1
          enddo
          !
          ! If it is at end, set group to the end; otherwise get cum
          if (numInView(inran(nextRan)) >= ninThresh) then
            cumNext = setcum + 1. / (groupSize(inran(nextRan)) * nmap)
          else
            iran = ninRange
            isetEnd = inran(iran)
          endif
        endif
      enddo
      ! print *,'isetstr', isetStr, '  isetend', isetEnd, '  setcum', setcum, &
      ! '  cumnext', cumNext
    endif
    !
    ! If it's a valid set, assign the group numbers
    ! Make sure the last set goes to the end (shouldn't be needed)
    if (isetEnd >= isetStr) then
      if (iset == nsets) isetEnd = inran(ninRange)
      ninVar = 0
      do i = 1, ninRange
        if (inran(i) >= isetStr .and. inran(i) <= isetEnd) then
          !
          ! if it's a long way since last entry, skip a group number
          !
          if (inran(i) - lastMap > nmap + 1) ivar = ivar + 1
          ninVar = ninVar + 1
          mapList(inran(i)) = ivar
          lastMap = inran(i)
        endif
      enddo
      if (ninVar > 0) ivar = ivar + 1
      isetStr = isetEnd + 1
    endif
  enddo
  !
  ! skip a group number at the end to prevent connections between sets
  !
  ivar = ivar + 1
  return
end subroutine groupList




subroutine groupMinMax(mapList, nview, imap, ivmin, ivmax, numInGroup)
  implicit none
  integer*4 mapList(*), nview, imap, ivmin, ivmax, numInGroup
  integer*4 iv
  ivmin = 0
  ivmax = 0
  numInGroup = 0
  do iv = 1, nview
    if (mapList(iv) == imap) then
      if (ivmin == 0) ivmin = iv
      ivmax = iv
      numInGroup = numInGroup + 1
    endif
  enddo
  return
end subroutine groupMinMax


! Computes relative group sizes for each view; group size is proportional
! to cosine of the tilt angle to the given power.

subroutine setGrpSize(tilt, nview, power, groupSize)
  implicit none
  real*4 groupSize(*), tilt(*), power
  integer*4 nview, i
  real*4 sum, angMax
  !
  ! 12 / 13 / 08: Originally meant the maximum angle to be 65 but it was not
  ! in radians so it never had any effect.  So make it high so grouping
  ! will be same for anything but 180 degree data
  angMax = 80. * 3.14159 / 180.
  if (power == 0.) then
    do i = 1, nview
      groupSize(i) = 1.
    enddo
  else
    sum = 0.
    do i = 1, nview
      groupSize(i) = cos(min(abs(tilt(i)), angMax))**power
      sum = sum + groupSize(i)
    enddo
    do i = 1, nview
      groupSize(i) = nview * groupSize(i) / sum
    enddo
  endif
  return
end subroutine setGrpSize

!
! Remap the view numbers in a separate group from file views
! to internal views
!
subroutine mapSeparateGroup(ivSep, nSepInGrp, mapFileToView, &
    nFileViews)
  implicit none
  integer*4 ivSep(*), nSepInGrp, mapFileToView(*), nFileViews, i, j
  !
  i = 1
  do while(i <= nSepInGrp)
    if (ivsep(i) <= 0 .or. ivsep(i) > nFileViews) call errorExit( &
        'View in separate group is outside known range of image file', 0)
    ivsep(i) = mapFileToView(ivsep(i))
    if (ivsep(i) == 0) then
      nSepInGrp = nSepInGrp - 1
      do j = i, nSepInGrp
        ivsep(j) = ivsep(j + 1)
      enddo
    else
      i = i + 1
    endif
  enddo
end subroutine mapSeparateGroup

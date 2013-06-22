! * * * * CLIPMODEL * * * * *
!
! CLIPMODEL will clip out a portion of a model and output either a
! new model file, or a simple list of point coordinates.  It can either
! include only points within a defined rectangular block of 3-
! dimensional coordinate space, or exclude points within such a block.
! This process of inclusion or exclusion may be invoked repeatedly.
! The program can also eliminate an entire IMOD object
!
! See man page for details
!
! $Id$
!
! David Mastronarde 1/11/90
!
program clipmodel
  implicit none
  include 'model.inc90'
  integer LIMZ, LIMOBJ
  parameter (LIMZ = 100000, LIMOBJ = 100000)
  integer*4 listz(LIMZ), iobjElim(LIMOBJ), iflags(LIMOBJ), inclExclList(LIMOBJ)
  logical exist, readw_or_imod, inside, lastInside, getModelObjectRange, doValues
  character*320 modelFile
  character*10240 listString
  character*2 inOrEx
  character*7 pixelMicrons
  integer*4, allocatable :: ipntToDelete(:)
  integer*4 i, ibase, icl, ierr, ifCut, ifExclude, ifFlip, ifKeepAll, ifLoop, ifPoint
  integer*4 ifStartCut, ifTreat, il, imodObj, inList, indY, indZ, iobj
  integer*4 iobjTest, ipt, iptEndBest, iptInside, iptStart, iptStartBest, ixMax, ixMin
  integer*4 ixx, iyMax, iyMin, iyy, izMax, izMin, izz, jpnt, lpnt, modFlag
  integer*4 nlistz, nonEmptyNew, nonEmptyOld, numInObj, numObjElim, numObjTot, numPtsTot
  integer*4 numPtsTotOld, numRemEnd, numRemStart, numLoops, loop, noActions, ixyzSum
  integer*4 ifValRange, ioptValues, iptOut, iptDelSize, numToDelete, maxNumPts
  real*4 cutSum, defScale, volume, xmax, xmin, xofs, xx, valRangeMin, valRangeMax
  real*4 xyScale, ymax, ymin, yofs, yy, zFullScale, zScale, zmax, zmin, zofs, zz, gsval
  logical*4 keepEmpty, updateMM
  integer*4 getImodFlags, getImodHead, getImodScales, getImodObjSize, findAddMinMax1Value
  integer*4 getObjValueThresh, getContValue, getPointValue, deleteImodPoint
  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger,PipGetBoolean, PipNumberOfEntries, PipGetLogical
  integer*4 PipGetFloat, PipGetString, PipGetTwoIntegers, PipGetTwoFloats
  integer*4 PipGetInOutFile, PipGetIntegerArray

  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  clipmodel
  integer numOptions
  parameter (numOptions = 16)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@point:PointOutput:I:@'// &
      'xminmax:XMinAndMax:FPM:@yminmax:YMinAndMax:FPM:@zminmax:ZMinAndMax:FPM:@'// &
      'exclude:ExcludeOrInclude:IA:@longest:LongestContourSegment:B:@'// &
      'keep:KeepEmptyContours:B:@objects:ObjectList:LI:@ends:ClipFromStartAndEnd:IP:@'// &
      'values:ValuesInOrOutOfRange:I:@range:RangeForValues:FP:@'// &
      'update:UpdateObjectMinMax:B:@param:ParameterFile:PF:@help:usage:B:'
  ifPoint = 0
  ifKeepAll = 1
  keepEmpty = .false.
  ifValRange = 0
  ioptValues = 0
  iptDelSize = 0
  updateMM = .false.

  ! Pip startup: set error, parse options, check help, set flag if used
  call PipReadOrParseOptions(options, numOptions, 'clipmodel', 'ERROR: CLIPMODEL - ', &
      .true., 1, 1, 1, numOptArg, numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0
       
  call imodPartialMode(1)
  if (PipGetInOutFile('InputFile', 1, 'Name of input model file', modelFile) .ne. 0)  &
      call exitError('NO INPUT FILE SPECIFIED')

  exist = readw_or_imod(modelFile)
  if (.not.exist) call exitError('OPENING MODEL FILE')
  !
  if (PipGetInOutFile('OutputFile', 2, 'Name of output model or point file', modelFile)  &
      .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
  !
  if (pipinput) then
    ierr = PipGetInteger('PointOutput', ifPoint)
    i = 1 - ifKeepAll
    ierr = PipGetBoolean('LongestContourSegment', i)
    ifKeepAll = 1 - i
    ierr = PipGetLogical('KeepEmptyContours', keepEmpty)
    ierr = PipGetInteger('ValuesInOrOutOfRange', ioptValues)
    ifValRange = 1 - PipGetTwoFloats('RangeForValues', valRangeMin, valRangeMax)
    ierr = PipGetLogical('UpdateObjectMinMax', updateMM)
    if (updateMM .and. keepEmpty) call exitError('YOU CANNOT USE -update WITH -keep')
  else
    write(*,'(1x,a,$)') '0 for model output, 1 for point file'// &
        ' output (-1 for corner points also): '
    read(*,*) ifPoint
    !
    write(*,'(1x,a,/,a,$)') '0 to retain only longest included'// &
        ' segment,', '  or 1 to retain all points within limits: '
    read(5,*) ifKeepAll
  endif
  !
  do i = 1, LIMOBJ
    iflags(i) = 0
  enddo
  ierr = getImodFlags(iflags, LIMOBJ)
  if (ierr .ne. 0) print *,'Error getting object types, assuming all are closed contours'
  numObjTot = getImodObjSize()
  pixelMicrons = 'microns'
  defScale = 1.e6
  !
  ! DNM 7/7/02: get image scales, and switch to exiting on error
  !
  ierr = getImodHead(xyScale, zScale, xofs, yofs, zofs, ifFlip)
  if (ierr .ne. 0) call exitError('GETTING MODEL HEADER')
  if (abs(xyScale - defScale) / defScale < 1.e-5) then
    xyScale = 1.
    pixelMicrons = 'pixels'
  endif
  zFullScale = xyScale * zScale
  if (ifFlip == 0) then
    indY = 2
    indZ = 3
  else
    indZ = 2
    indY = 3
  endif
  numLoops = 1000000
  if (pipInput) then
    numLoops = 0
    ierr = PipNumberOfEntries('XMinAndMax', ixMin)
    ierr = PipNumberOfEntries('YMinAndMax', iyMin)
    ierr = PipNumberOfEntries('ZMinAndMax', izMin)
    ixyzSum = ixMin + iyMin + izMin
    noActions = PipGetIntegerArray('ExcludeOrInclude', inclExclList, numLoops, LIMOBJ)
    iobj = 1 - PipGetString('ObjectList', listString)
    if (iobj > 0) call parseList2(listString, iobjElim, numObjElim, LIMOBJ)
    ifCut = 1 - PipGetTwoIntegers('ClipFromStartAndEnd', numRemStart, numRemEnd)
    if (iobj > 0 .and. (noActions == 0 .or. ixyzSum > 0)) call exitError( &
        'YOU CANNOT ENTER AN OBJECT LIST WITH COORDINATE INCLUSION/EXCLUSIONS')
    if (ifCut > 0 .and. iobj == 0) call exitError( &
        'YOU MUST ENTER AN OBJECT LIST WHEN CLIPPING FROM STARTS AND ENDS')
    if (abs(ioptValues) > 1 .and. (noActions == 0 .or. ixMin + iyMin + izMin > 0))  &
        call exitError('YOU CANNOT CLIP POINTS BY VALUE TOGETHER WITH COORDINATE '// &
        'INCLUSION/EXCLUSIONS')
    if (ifCut > 0 .and. abs(ioptValues) > 1) call exitError( &
        'YOU CANNOT CLIP POINTS BY VALUE WHEN CLIPPING FROM STARTS AND ENDS')
    if (noActions .ne. 0 .and. iobj == 0 .and. ioptValues == 0 .and. ixyzSum == 0)  &
        call exitError('YOU MUST SPECIFY SOMETHING TO DO')
    if (noActions .ne. 0 .and. iobj == 0) then
      numLoops = 1
      inclExclList(1) = 0
      noActions = 0
    endif
    if (noActions .ne. 0) then
      numLoops = 1
      inclExclList(1) = -1
      if (ifCut > 0) inclExclList(1) = -2
    else
      if (ixMin > 0 .and. ixMin .ne. numLoops) call exitError('THE NUMBER OF -xminmax'// &
          ' ENTRIES MUST MATCH THE NUMBER OF OPERATIONS ENTERED WITH -exclude')
      if (iyMin > 0 .and. iyMin .ne. numLoops) call exitError('THE NUMBER OF -yminmax'// &
          ' ENTRIES MUST MATCH THE NUMBER OF OPERATIONS ENTERED WITH -exclude')
      if (izMin > 0 .and. izMin .ne. numLoops) call exitError('THE NUMBER OF -zminmax'// &
          ' ENTRIES MUST MATCH THE NUMBER OF OPERATIONS ENTERED WITH -exclude')
      if (ifPoint < 0 .and. (ixMin .ne. numLoops .or. iyMin .ne. numLoops)) &
          call exitError('If you want corner points, then you'// &
          ' MUST enter actual min and max X & Y')
    endif
  endif

  !
  do loop = 1, numLoops
    xmin = -1.e9
    xmax = 1.e9
    ymin = -1.e9
    ymax = 1.e9
    zmin = -1.e9
    zmax = 1.e9
    if (pipinput) then
      ifExclude = inclExclList(loop)
    else
      write(*,'(1x,a,/,a,$)') 'Enter 0 to include or 1 to exclude'// &
          ' points in a specified coordinate block,', '   or -1 or -2' &
          //' to exclude or shorten particular objects: '
      read(*,*) ifExclude
    endif
    if (ifExclude >= 0) then
      inOrEx = 'in'
      if (ifExclude .ne. 0) inOrEx = 'ex'
      !
      if (pipInput) then
        ierr = PipGetTwoFloats('XMinAndMax', xmin, xmax)
        ierr = PipGetTwoFloats('YMinAndMax', ymin, ymax)
        ierr = PipGetTwoFloats('ZMinAndMax', zmin, zmax)
      else
18      write(*,'(1x,a,$)') 'Minimum and maximum X and Y to ' &
            //inOrEx//'clude (/ for no limits): '
        read(*,*) xmin, xmax, ymin, ymax
        if (ifPoint < 0 .and. ymax > 9.e8) then
          print *,'If you want corner points, then you', &
              ' MUST enter actual min and max X & Y'
          go to 18
        endif
        write(*,'(1x,a,$)') &
            'Minimum and maximum Z to '//inOrEx//'clude (/ for no limits): '
        read(*,*) zmin, zmax
      endif
      if (xmax - xmin < 9.e8 .and. ymax - ymin < 9.e8 .and. zmax - zmin < 9.e8) then
        volume = (zmax + 1 - zmin) * (xmax + 1 - xmin) * (ymax + 1 - ymin) * zFullScale* &
            xyScale**2
        write(*,105) volume, pixelMicrons
105     format(/,' Selected volume is',g15.6,' cubic ',a)
      endif

      ! Increase Z limits a bit in case there are slightly off Z values
      zmin = zmin - 0.005
      zmax = zmax + 0.005

    else if (.not. pipinput) then
      if (ifExclude < -1) then
        write(*,'(1x,a,$)') 'Number of points to remove from start,' &
            //' # to remove from end: '
        read(5,*) numRemStart, numRemEnd
        print *,'Enter list of object numbers for contours to shorten (ranges OK)'
      else
        print *,'Enter list of numbers of objects to eliminate (ranges OK)'
      endif
      call rdlist2(5, iobjElim, numObjElim, LIMOBJ)
    endif
    !
    ! look at each object, find longest contiguous segment within area
    numPtsTot = 0
    numPtsTotOld = 0
    cutSum = 0.
    nonEmptyOld = 0
    nonEmptyNew = 0
    do imodObj = 1, numObjTot
      
      if (.not.getModelObjectRange(imodObj, imodObj)) then
        write(*,'(/,a,i6)') 'ERROR: CLIPMODEL - LOADING DATA FOR OBJECT #', imodObj
        call exit(1)
      endif
      call scale_model(0)
      !
      maxNumPts = 0
      do iobj = 1, max_mod_obj
        numPtsTotOld = numPtsTotOld + npt_in_obj(iobj)
        maxNumPts = max(maxNumPts, npt_in_obj(iobj))
      enddo

      ! Maintain array for keeping track of deleted points
      if (maxNumPts > iptDelSize) then
        if (iptDelSize == 0) deallocate(ipntToDelete, stat=ierr)
        iptDelSize = maxNumPts + 1000
        allocate(ipntToDelete(iptDelSize), stat=ierr)
        call memoryError(ierr, 'ALLOCATING ARRAY FOR DELETED POINT LIST')
      endif

      ! If doing values and no range was entered, get the object threshold
      ! If there is not one, skip the object
      doValues = ioptValues .ne. 0
      if (doValues .and. ifValRange == 0) then
        doValues = getObjValueThresh(imodObj, valRangeMax) == 0
        valRangeMin = -1.e37
      endif
      !
      ! Loop on the contours
      do iobj = 1, max_mod_obj
        numInObj = npt_in_obj(iobj)
        numToDelete = 0
        if (numInObj > 0) nonEmptyOld = nonEmptyOld + 1
        if (doValues .and. mod(abs(ioptValues), 2) > 0) then
          !
          ! If doing values, get the contour value and eliminate it by just zeroing it
          ! But also set up to delete the contour data
          if (getContValue(imodObj, iobj, gsval) .eq. 0) then
            inside = gsval >= valRangeMin .and. gsval <= valRangeMax
            if (ioptValues < 0) inside = .not. inside
            if (inside) then
              call addToDeletionList(1, numInObj)
              npt_in_obj(iobj) = 0
              numInObj = 0
            endif
          endif
        endif
        !
        ! Then do other operations if that did not delete contour
        if (numInObj > 0) then
          iptOut = 0
          ibase = ibase_obj(iobj)
          if (doValues .and. abs(ioptValues) > 1) then
            !
            ! point clipping by value, repack object array with retained points
            do ipt = 1, numInObj
              if (getPointValue(imodObj, iobj, ipt, gsval) .eq. 0) then
                inside = gsval >= valRangeMin .and. gsval <= valRangeMax
                if (ioptValues < 0) inside = .not. inside
                if (inside) then
                  call addToDeletionList(ipt, ipt)
                else
                  iptOut = iptOut + 1
                  object(ibase + iptOut) = object(ibase + ipt) 
                endif
              endif
            enddo
            npt_in_obj(iobj) = iptOut

          else if (ifExclude >= 0) then
            ! imodObj=256-obj_color(2, iobj)
            if (ifKeepAll == 0 .and. mod(iflags(imodObj), 4) .ne. 2) then
              !
              ! keeping only longest segment inside limits: can be used for
              ! open or closed contours but not scattered points
              ! Loop and find largest segment
              lastInside = .false.
              iptStartBest = 1
              iptEndBest = 0
              do ipt = 1, numInObj
                jpnt = abs(object(ipt + ibase))
                xx = p_coord(1, jpnt)
                yy = p_coord(indY, jpnt)
                zz = p_coord(indZ, jpnt)
                ! this could be done with .xor. but it would take you longer
                ! to figure out what was going on
                inside = (xx >= xmin .and. xx <= xmax) .and. &
                    (yy >= ymin .and. yy <= ymax) .and. &
                    (zz >= zmin .and. zz <= zmax)
                if (ifExclude .ne. 0) inside = .not.inside
                if (inside) iptInside = ipt
                if (inside .and. .not.lastInside) then
                  iptStart = ipt
                endif
                if (lastInside .and. .not.inside .or. &
                    ipt == numInObj .and. inside) then
                  if (iptEndBest - iptStartBest < iptInside - iptStart) then
                    iptEndBest = iptInside
                    iptStartBest = iptStart
                  endif
                endif
                lastInside = inside
              enddo
              !
              ! Add to deletion list
              call addToDeletionList(1, iptStartBest - 1)
              call addToDeletionList(iptEndBest + 1, numInObj)
              !
              ! truncate object and point base to new start
              npt_in_obj(iobj) = iptEndBest + 1 - iptStartBest
              ibase_obj(iobj) = ibase_obj(iobj) + iptStartBest - 1
            else
              !
              ! keeping all points inside limits
              ! repack the object array with just the points being retained
              ifCut = 0
              ifStartCut = 0
              modFlag = mod(iflags(imodObj), 4)
              do ipt = 1, numInObj
                jpnt = abs(object(ipt + ibase))
                xx = p_coord(1, jpnt)
                yy = p_coord(indY, jpnt)
                zz = p_coord(indZ, jpnt)
                ! this could be done with .xor. but it would take you longer
                ! to figure out what was going on
                inside = (xx >= xmin .and. xx <= xmax) .and. &
                    (yy >= ymin .and. yy <= ymax) .and. &
                    (zz >= zmin .and. zz <= zmax)
                if (ifExclude .ne. 0) inside = .not.inside
                if (inside) then
                  if (ifCut .ne. 0 .and. modFlag .ne. 2) then
                    if (iptOut == 0) then
                      !
                      ! if last point was cut out and we are still at the
                      ! start, then mark the start as cut
                      !
                      ifStartCut = 1
                    else
                      !
                      ! otherwise, compute distance between this & last point
                      !
                      cutSum = cutSum + sqrt( &
                          (xyScale * (p_coord(1, jpnt) - p_coord(1, lpnt)))**2 + &
                          (xyScale * (p_coord(indY, jpnt) - &
                          p_coord(indY, lpnt)))**2 + &
                          (zFullScale * (p_coord(indZ, jpnt) - p_coord(indZ, lpnt)))**2)
                    endif
                  endif
                  ifCut = 0
                  iptOut = iptOut + 1
                  object(ibase + iptOut) = object(ibase + ipt) 
                  lpnt = jpnt
                else
                  ifCut = 1
                  call addToDeletionList(ipt, ipt)
                endif
              enddo
              npt_in_obj(iobj) = iptOut
              !
              ! If there are any points left, and either the start was cut
              ! or the last point was cut, and this is a closed contour,
              ! then add distance between endpoints as a cut edge
              if (iptOut > 0 .and. (ifStartCut > 0 .or. ifCut .ne. 0) .and. &
                  modFlag == 0) then
                jpnt = abs(object(1 + ibase))
                cutSum = cutSum + sqrt( &
                    (xyScale * (p_coord(1, jpnt) - p_coord(1, lpnt)))**2 + &
                    (xyScale * (p_coord(indY, jpnt) - p_coord(indY, lpnt)))**2 + &
                    (zFullScale * (p_coord(indZ, jpnt) - p_coord(indZ, lpnt)))**2)
              endif
            endif
            !
            ! if getting rid of certain color, check for color
          else
            iobjTest = 256 - obj_color(2, iobj)
            if (obj_color(1, iobj) == 0) iobjTest = -iobjTest
            ifTreat = 0
            do icl = 1, numObjElim
              if (iobjTest == iobjElim(icl)) ifTreat = 1
            enddo
            if (ifTreat == 1) then
              if (ifExclude == -1) then
                !
                ! Getting rid of whole object, point deletion should be gratuitous
                npt_in_obj(iobj) = 0
                call addToDeletionList(1, numInObj)
              else
                !
                ! Clipping ends, do point deletion and move object base
                call addToDeletionList(1, numRemStart)
                call addToDeletionList(numInObj + 1 - numRemEnd, numInObj)
                npt_in_obj(iobj) = max(0, numInObj - numRemStart - numRemEnd)
                ibase_obj(iobj) = ibase_obj(iobj) + numRemStart
              endif
            endif
          endif
          numPtsTot = numPtsTot + npt_in_obj(iobj)
          if (npt_in_obj(iobj) == 0) then
            n_object = n_object - 1
          else
            nonEmptyNew = nonEmptyNew + 1
          endif
        endif
        !
        ! Delete points from model in inverse order
        do ipt = numToDelete, 1, -1
          ierr = deleteImodPoint(imodObj, iobj, ipntToDelete(ipt))
          if (ierr .ne. 0) call exitError('Deleting point from IMOD model')
        enddo
      enddo
      if (.not. keepEmpty) call removeEmptyContours(imodObj)
      if (updateMM) then
        if (findAddMinMax1Value(imodObj) .ne. 0)  &
            call exitError('Adding min/max to object in IMOD model')
      endif
      call scale_model(1)
      call putModelObjects()
    enddo
    !
    ! write out result, offer to go back for more
    !
    write(*,102) nonEmptyOld, nonEmptyNew, numPtsTotOld, numPtsTot
102 format(/,' Number of non-empty contours reduced from',i7,' to', &
        i8,/, ' Number of points reduced from',i8,' to',i8,/)
    !
    if (cutSum > 0.) then
      write(*,103) cutSum, pixelMicrons, cutSum * zFullScale, pixelMicrons
103   format(' Total length of cut edges is',f15.5,' ',a,/, &
          ' Approximate surface area of cut edges is',f18.8, &
          ' square ',a,/)
    endif
    !
    if (.not. pipinput) then
      write(*,'(1x,a,$)') '0 to write out results, 1 to enter new'// &
          ' block to include or exclude: '
      read(*,*) ifLoop
      if (ifLoop == 0) then
        exit
      endif
    endif
  enddo
  !
  ! now either just write model, or write point file
  !
  if (ifPoint == 0) then
    n_point = -1
    call write_wmod(modelFile)
    print *,'CLIPPED MODEL WRITTEN'
  else
    call dopen(1, modelFile, 'new', 'f')
    if (.not.getModelObjectRange(1, numObjTot))  &
        call exitError(' RELOADING DATA TO MAKE POINT OUTPUT')
    !
    ! This should be done before point output
    call scale_model(0)
    ixMin = nint(xmin)
    iyMin = nint(ymin)
    izMin = nint(zmin)
    ixMax = nint(xmax)
    iyMax = nint(ymax)
    izMax = nint(zmax)
    nlistz = 0
    !
    ! loop through objects again
    !
    do iobj = 1, max_mod_obj
      ibase = ibase_obj(iobj)
      do ipt = 1, npt_in_obj(iobj)
        jpnt = abs(object(ipt + ibase))
        ixx = max(ixMin, min(ixMax, nint(p_coord(1, jpnt))))
        iyy = max(iyMin, min(iyMax, nint(p_coord(2, jpnt))))
        izz = max(izMin, min(izMax, nint(p_coord(3, jpnt))))
        !
        ! if need corners, see if z coord is on list of z's
        !
        if (ifPoint < 0) then
          inList = 0
          do il = 1, nlistz
            if (izz == listz(il)) inList = il
          enddo
          !
          ! if not, add to list of z's and write corner
          !
          if (inList == 0) then
            nlistz = nlistz + 1
            listz(nlistz) = izz
            write(1, 101) ixMin, iyMin, izz
            write(1, 101) ixMax, iyMax, izz
          endif
        endif
        write(1, 101) ixx, iyy, izz
101     format(3i8)
      enddo
    enddo
    close(1)
    print *, 'POINT LIST WRITTEN'
  endif
  call exit(0)

CONTAINS
  
  ! Add a range of point numbers to the list of points to delete
  subroutine addToDeletionList(istart, iend)
    integer*4 istart, iend, iptmp
    do iptmp = istart, iend
      numToDelete = numToDelete + 1
      ipntToDelete(numToDelete) = iptmp
    enddo
    return
  end subroutine addToDeletionList

end program clipmodel

subroutine removeEmptyContours(loadedObj)
  implicit none
  include 'model.inc90'
  integer*4 imodObj(max_mod_obj), imodCont(max_mod_obj), iobjToDelete(max_mod_obj)
  integer*4 numKeep, numDelete, iobj, idel, deleteImodCont, loadedObj
  numKeep = 0
  numDelete = 0
  if (loadedObj > 0) then
    do iobj = 1, max_mod_obj
      call objToCont(iobj, obj_color, imodObj(iobj), imodCont(iobj))
    enddo
  else 
    do iobj = 1, max_mod_obj
      imodObj(iobj) = loadedObj
      imodCont(iobj) = iobj
    enddo
  endif
  do iobj = 1, max_mod_obj
    if (npt_in_obj(iobj) > 0) then
      numKeep = numKeep + 1
      npt_in_obj(numKeep) = npt_in_obj(iobj)
      ibase_obj(numKeep) = ibase_obj(iobj)
      obj_color(1, numKeep) = obj_color(1, iobj)
      obj_color(2, numKeep) = obj_color(2, iobj)
    else
      numDelete = numDelete + 1
      iobjToDelete(numDelete) = iobj
    endif
  enddo
  do idel = numDelete, 1, -1
    iobj = iobjToDelete(idel)
    if (deleteImodCont(imodObj(iobj), imodCont(iobj)) .ne. 0)  &
        call exitError('Deleting contour from IMOD model')
  enddo
  max_mod_obj = numKeep
  return
end subroutine removeEmptyContours

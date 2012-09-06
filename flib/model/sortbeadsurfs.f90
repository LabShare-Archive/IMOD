! SORTBEADSURFS
!
! This program is used to sort beads into two surfaces for a bead model
! from findbeads3d, or to apply X axis tilt, shift coordinates, and
! combine objects for a model from tiltalign.  It is a preprocessor for
! using such models in flattenwarp.
!
! $Id$
!
program sortbeadsurfs
  implicit none
  include 'smallmodel.inc90'
  character*320 inFile, outFile, textOutput
  integer*4 numInGroup(2), jRed(2), jGreen(2), jBlue(2)
  logical*4 exist, readSmallMod, already, majority, invertZ, rescale
  logical*4 oneSurface, checkGroups
  integer*4 ifFlip, local, ierr, i, j, ix, iy, indY, indZ, maxx, maxy, maxz
  real*4 xmax, xmin, ymin, ymax, deltaX, deltaY, xlo, xhi, ylo, yhi
  real*4 epsX, epsY, dx, dy, cosa, sina, xtilt, scaleFac, outlierCrit, contValue
  integer*4 numAreaX, numAreaY, numPoints, isize, minNum, iRed, iGreen, iBlue
  integer*4 imodObj, newNx, newNy, icolorObj, icolor, numColors, iobj, iyStart, iyEnd
  integer*4 nxUB, nyUB, ixTrim0, ixTrim1, iyTrim0, iyTrim1, ibinningRec, ifPickLocal
  integer*4 ibinningPreali, numParams, ifUseValues, imodCont, minLocalSize, minLocalNum
  integer*4 lastNumX, lastNumY, numTryX, numTryY, localTry
  integer*4 getImodMaxes, getScatSize, deleteIObj, putImodMaxes, getObjColor
  real*4 sind, cosd
  integer*4 PipNumberOfEntries, PipGetTwoFloats, setSurfSortParam
  integer*4 surfaceSort, getContValue, getPointValue

  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetFloat, PipGetTwoIntegers
  integer*4 PipGetInOutFile, PipGetLogical, PipGetString
  real*4 xyz(3,max_pt), xyzFit(3,max_pt), values(max_pt), outlie(max_pt), tiltDum
  integer*4 igroup(max_pt), igrpSort(max_pt), imodObjOrig(max_pt)
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  sortbeadsurfs
  !
  integer numOptions
  parameter (numOptions = 21)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@text:TextFileWithSurfaces:FN:@'// &
      'flipyz:FlipYandZ:I:@subarea:SubareaSize:I:@majority:MajorityObjectOnly:B:@'// &
      'xaxis:XAxisTilt:F:@invert:InvertZAxis:B:@already:AlreadySorted:B:@'// &
      'one:OneSurface:B:@aligned:AlignedSizeXandY:IP:@xtrim:XTrimStartAndEnd:IP:@'// &
      'ytrim:YTrimStartAndEnd:IP:@prebin:PrealignedBinning:I:@'// &
      'recbin:ReconstructionBinning:I:@rescale:RescaleByBinnings:B:@'// &
      'check:CheckExistingGroups:I:@values:ValuesToRestrainSorting:B:@'// &
      'outlier:OutlierCriterionDeviation:F:@set:SetSurfaceSortParam:FPM:@help:usage:B:'

  ifFlip = -1
  local = 0
  newNx = 0
  newNy = 0
  xtilt = 0.
  ibinningRec = 1
  ibinningPreali = 1
  ixTrim0 = 0
  ixTrim1 = 0
  iyTrim0 = 0
  iyTrim1 = 0
  invertZ = .false.
  already = .false.
  majority = .false.
  rescale = .false.
  checkGroups = .false.
  oneSurface = .false.
  textOutput = ''
  ifUseValues = 0
  outlierCrit = 2.24

  call PipReadOrParseOptions(options, numOptions, 'sortbeadsurfs', &
      'ERROR: SORTBEADSURFS - ', .false., 1, 1, 1, numOptArg, &
      numNonOptArg)

  if (PipGetInOutFile('InputFile', 1, ' ', inFile) &
      .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
  if (PipGetInOutFile('OutputFile', 2, ' ', outFile) &
      .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
  ierr = PipGetInteger('FlipYandZ', ifFlip)
  ierr = PipGetInteger('SubareaSize', local)
  ifPickLocal = 1 - PipGetTwoIntegers('PickAreasMinNumAndSize', minLocalNum, minLocalSize)
  if (local > 0 .and. ifPickLocal > 0)  &
      call exitError('YOU CANNOT ENTER BOTH -subarea AND -pick')
  ierr = PipGetFloat('XAxisTilt', xtilt)
  ierr = PipGetFloat('OutlierCriterionDeviation', outlierCrit)
  ierr = PipGetLogical('AlreadySorted', already)
  ierr = PipGetLogical('OneSurface', oneSurface)
  ierr = PipGetLogical('MajorityObjectOnly', majority)
  ierr = PipGetInteger('ValuesToRestrainSorting', ifUseValues)
  ierr = PipGetString('TextFileWithSurfaces', textOutput)
  ierr = PipGetTwoIntegers('AlignedSizeXandY', newNx, newNy)
  ierr = PipGetInteger('PrealignedBinning', ibinningPreali)
  ierr = PipGetInteger('ReconstructionBinning', ibinningRec)
  ierr = PipGetLogical('RescaleByBinnings', rescale)
  ierr = PipGetLogical('InvertZAxis', invertZ)
  ierr = PipGetLogical('CheckExistingGroups', checkGroups)
  ierr = PipGetTwoIntegers('XTrimStartAndEnd', ixTrim0, ixTrim1)
  ierr = PipGetTwoIntegers('YTrimStartAndEnd', iyTrim0, iyTrim1)
  ierr = PipNumberOfEntries('SetSurfaceSortParam', numParams)
  do i = 1, numParams
    ierr = PipGetTwoFloats('SetSurfaceSortParam', epsX, epsY)
    if (setSurfSortParam(nint(epsX), epsY) .ne. 0) call exitError( &
        'IMPROPER PARAMETER SETTING FOR SURFACE SORT ROUTINE')
  enddo
  !
  ! Get the model and its size
  exist = readSmallMod(inFile)
  if (.not.exist) call exitError('READING MODEL FILE')
  call scale_model(0)
  ierr = getImodMaxes(maxx, maxy, maxz)
  !
  ! Flip coordinates if appropriate given ymax and zmax, or if user says to
  indY = 2
  indZ = 3
  if ((ifFlip < 0 .and. maxz > maxy) .or. ifFlip > 0) then
    indY = 3
    indZ = 2
    ierr = maxy
    maxy = maxz
    maxz = ierr
    if (xtilt .ne. 0) call exitError('X axis tilt cannot be applied to a flipped model')
    if (newNx .ne. 0 .or. newNy .ne. 0 .or. ibinningPreali > 1 .or. &
        ibinningRec > 1 .or. rescale .or. invertZ .or. ixTrim0 > 0 .or. &
        ixTrim1 > 0 .or. iyTrim0 > 0 .or. iyTrim1 > 0) &
        call exitError('You cannot do size adjustments or Z '// &
        'inversion with a flipped model')
  endif
  !
  ! Find the X/Y range of the data and copy into a separate array
  ! Rotate by the tilt angle not the negative of it.
  xmin = 1.e20
  xmax = -xmin
  ymin = xmin
  ymax = xmax
  cosa = cosd(xtilt)
  sina = sind(xtilt)
  i = 0
  do iobj = 1, max_mod_obj
    call objToCont(iobj, obj_color, imodObj, imodCont)
    if (ifUseValues .ne. 0) then
      if (getContValue(imodObj, imodCont, contValue) .ne. 0) contValue = 0.
    endif
    do j = 1, npt_in_obj(iobj)
      i = i + 1
      iy = object(ibase_obj(iobj) + j)
      xyz(1, i) = p_coord(1, iy)
      if (xtilt .ne. 0) then
        dy = p_coord(indY, iy) - maxy / 2.
        xyz(2, i) = cosa * dy - sina * p_coord(indZ, iy) + maxy / 2.
        xyz(3, i) = sina * dy + cosa * p_coord(indZ, iy)
      else
        xyz(2, i) = p_coord(indY, iy)
        xyz(3, i) = p_coord(indZ, iy)
      endif
      if (invertZ) xyz(3, i) = maxz - 1 - xyz(3, i)
      xmin = min(xmin, xyz(1, i))
      ymin = min(ymin, xyz(2, i))
      xmax = max(xmax, xyz(1, i))
      ymax = max(ymax, xyz(2, i))
      igroup(i) = -1
      imodObjOrig(i) = imodObj
      if (ifUseValues .ne. 0) then
        if (getPointValue(imodObj, imodCont, j, values(i)) .ne. 0) values(i) = contValue
      endif
    enddo
  enddo
  n_point = i

  ! Do outlier analysis of values if they were obtained
  if (ifUseValues .ne. 0) &
      call rsMadMedianOutliers(values, n_point, outlierCrit, outlie)
  !
  ! If points are already sorted, look at the object colors to deduce the sorting,
  ! unless the one surface option is given
  if (oneSurface) then
    do i = 1, n_point
      igroup(i) = 1
    enddo

  else if (already) then
    numColors = 0
    do iobj = 1, max_mod_obj
      if (npt_in_obj(iobj) > 0) then
        imodObj = 256 - obj_color(2, iobj)
        ierr = getObjColor(imodObj, iRed, iGreen, iBlue)
        icolorObj = 0
        do icolor = 1, numColors
          if (iRed == jRed(icolor) .and. iGreen == jGreen(icolor) .and. &
              iBlue == jBlue(icolor)) icolorObj = icolor
        enddo
        if (icolorObj == 0) then
          if (numColors > 1) call exitError( &
              'There are more than two colors among the various objects')
          numColors = numColors + 1
          icolorObj = numColors
          jRed(icolorObj) = iRed
          jGreen(icolorObj) = iGreen
          jBlue(icolorObj) = iBlue
        endif
        do ix = 1, npt_in_obj(iobj)
          i = object(ibase_obj(iobj) + ix)
          igroup(i) = icolorObj
        enddo
      endif
    enddo
  else
    !
    ! To do sorting, first set up subareas
    if (ifPickLocal > 0) then
      lastNumX = 1
      lastNumY = 1
      
      ! For picking area size, loop from biggest possible size down to minimum
      LOCAL_LOOP: do localTry = nint(max(xmax - xmin, ymax - ymin)), minLocalSize, -1
        numTryX = max(1, nint((xmax - xmin) / localTry))
        numTryY = max(1, nint((ymax - ymin) / localTry))

        ! Skip to next size down if it gives numbers that have already been assessed
        if ((numTryX == lastNumX .and. numTryY == lastNumY) .or.  &
            numTryX * numTryY < 2) cycle

        ! Finish with last good size if the area is now too small
        if ((xmax - xmin) / numTryX < minLocalSize .or.  &
            (ymax - ymin) / numTryY < minLocalSize) then
          exit LOCAL_LOOP
        endif

        ! Otherwise count up the number in each area and finish if it falls below min
        do ix = 1, numTryX
          call subareaLimits(xmin, xmax, numTryX, ix, xlo, xhi)
          do iy = 1, numTryY
            call subareaLimits(ymin, ymax, numTryY, iy, ylo, yhi)
            numPoints = 0
            do i = 1, n_point
              if (xyz(1, i) >= xlo .and. xyz(1, i) <= xhi .and.  &
                  xyz(2, i) >= ylo .and. xyz(2, i) <= yhi) numPoints = numPoints + 1
            enddo
            if (numPoints < minLocalNum) then
              exit LOCAL_LOOP
            endif
          enddo
        enddo

        ! This is a good area size so set the local value from it
        local = localTry
        lastNumX = numTryX
        lastNumY = numTryY
      enddo LOCAL_LOOP

      if (local > 0) then
        write(*,'(a,i7,a,i4,a,i4,a)')'Picked a subarea size of',local, &
            ' to divide area into',lastNumX,' by',lastNumY,' subareas'
      else
        print *,'Analyzing entire area; no subarea size fits the constraints'
      endif
    endif

    ! Set up subareas with picked or entered local size
    numAreaX = 1
    numAreaY = 1
    if (local > 0) then
      numAreaX = max(1, nint((xmax - xmin) / local))
      numAreaY = max(1, nint((ymax - ymin) / local))
    endif
    minNum = n_point + 1
    !
    ! Loop on the areas
    do ix = 1, numAreaX
      call subareaLimits(xmin, xmax, numAreaX, ix, xlo, xhi)
      do iy = 1, numAreaY
        call subareaLimits(ymin, ymax, numAreaY, iy, ylo, yhi)
        numPoints = 0
        do i = 1, n_point
          !
          ! If a point is in area and hasn't been done yet, copy it over
          ! and mark as in the fitting group
          if (igroup(i) < 0 .and. xyz(1, i) >= xlo .and. xyz(1, i) <= &
              xhi .and. xyz(2, i) >= ylo .and. xyz(2, i) <= yhi) then
            numPoints = numPoints + 1
            xyzFit(1:3, numPoints) = xyz(1:3, i)
            igrpSort(numPoints) = 0
            if (ifUseValues * outlie(i) > 0) igrpSort(numPoints) = -1
            igroup(i) = 0
          endif
        enddo
        if (local > 0 .and. .not.checkGroups) write(*,'(a,2i3,a,i6,a)') &
            'Subarea', ix, iy, ' has', numPoints, ' points'
        minNum = min(minNum, numPoints)
        !
        ! Do the fits to find surfaces
        if (numPoints > 1) then
          ierr =  surfaceSort(xyzFit, numPoints, abs(ifUseValues), igrpSort)
          if (ierr .ne. 0) call exitError('ALLOCATING MEMORY IN surfaceSort')
        endif
        !
        ! Copy the group numbers back
        j = 0
        do i = 1, n_point
          if (igroup(i) == 0) then
            j = j + 1
            igroup(i) = igrpSort(j)
          endif
        enddo
      enddo
    enddo
  endif
  !
  ! Count numbers in groups
  do iy = 1, 2
    numInGroup(iy) = 0
    do i = 1, n_point
      if (igroup(i) == iy) numInGroup(iy) = numInGroup(iy) + 1
    enddo
    if (.not. checkGroups) write(*,'(a,i2,a,i6,a)') 'Group', iy, ' has', &
        numInGroup(iy), ' points'
  enddo
  !
  ! Set up for majority group if desired, or to avoid empty object
  iyStart = 1
  iyEnd = 2
  if (majority .or. numInGroup(2) == 0) then
    if (numInGroup(1) >= numInGroup(2)) iyEnd = 1
    if (numInGroup(1) < numInGroup(2)) iyStart = 2
  endif
  !
  ! Set up to shift the data if size changing and set up scaling
  dx = 0.
  dy = 0.
  nxUB = maxx * ibinningPreali
  nyUB = maxy * ibinningPreali
  scaleFac = 1.
  if (newNx > 0 .and. newNy > 0) then
    dx = (newNx - nxUB) / 2.
    dy = (newNy - nyUB) / 2.
    nxUB = newNx
    nyUB = newNy
  endif
  if (ixTrim0 > 0 .and. ixTrim1 > 0) then
    dx = dx - (ixTrim0 - 1) * ibinningRec
    nxUB = (ixTrim1 + 1 - ixTrim0) * ibinningRec
  endif
  if (iyTrim0 > 0 .and. iyTrim1 > 0) then
    dy = dy - (iyTrim0 - 1) * ibinningRec
    nyUB = (iyTrim1 + 1 - iyTrim0) * ibinningRec
  endif
  dx = dx / ibinningPreali
  dy = dy / ibinningPreali
  if (rescale) then
    scaleFac = float(ibinningPreali) / ibinningRec
    nxUB = nint(float(nxUB) / ibinningRec)
    nyUB = nint(float(nyUB) / ibinningRec)
  else
    nxUB = nint(float(nxUB) / ibinningPreali)
    nyUB = nint(float(nyUB) / ibinningPreali)
  endif
  ierr = putImodMaxes(nxUB, nyUB, maxz)
  !
  ! Set object properties
  isize = 3
  ierr = getScatSize(1, isize)
  isize = max(3, isize)
  ierr = deleteIObj()
  call putScatSize(1, isize)
  call putImodFlag(1, 2)
  call putObjColor(1, 0, 255, 0)
  if (iyStart .ne. iyEnd) then
    call putScatSize(2, isize)
    call putImodFlag(2, 2)
    call putObjColor(2, 255, 0, 255)
  endif

  ! Output text file if requested
  if (textOutput .ne. '') then
    call dopen(2, textOutput, 'new', 'f')
    i = 0
    do iobj = 1, max_mod_obj
      call objToCont(iobj, obj_color, imodObj, imodCont)
      do j = 1, npt_in_obj(iobj)
        i = i + 1
        write(2,'(3i8,i3)')imodObj, imodCont, j, igroup(i)
      enddo
    enddo
    close(2)
    
  endif
  !
  ! then rebuild the model, make it one point per contour
  ix = 0
  do iy = iyStart, iyEnd
    do i = 1, n_point
      if (igroup(i) == iy) then
        ix = ix + 1
        ibase_obj(ix) = ix - 1
        npt_in_obj(ix) = 1
        obj_color(1, ix) = 1
        obj_color(2, ix) = 255 - (iy - iyStart)
        p_coord(1, ix) = scaleFac * (xyz(1, i) + dx)
        p_coord(indY, ix) = scaleFac * (xyz(2, i) + dy)
        p_coord(indZ, ix) = scaleFac * xyz(3, i)
      endif
    enddo
  enddo
  max_mod_obj = ix
  n_point = ix
  call scale_model(1)
  call write_wmod(outFile)
  if (checkGroups) then
    ierr = n_point
    do iy = 1, 2
      ix = 0
      do i = 1, n_point
        if ((iy == 1 .and. igroup(i) .ne. imodObjOrig(i)) .or. &
            (iy == 2 .and. igroup(i) == imodObjOrig(i))) ix = ix + 1
      enddo
      ierr = min(ierr, ix)
    enddo
    if (ierr > 0) write(*,'(/,a,a,a,i5,a,i5,a)') &
        'ERROR: sortbeadsurfs - ', trim(inFile), ' has ', ierr, ' of ', &
        n_point , ' points in the wrong group'
    call exit(ierr)
  endif
  if (local > 0 .and. minNum <= 4) then
    print *,'Some areas have very few points - you should rerun this '// &
        'with larger subareas'
  else if (local > 0 .and. minNum <= 10) then
    print *, 'If points are '// &
        'sorted incorrectly, try rerunning with larger subareas'
  endif
  !
  call exit(0)
end program

subroutine subareaLimits(xmin, xmax, numAreaX, ix, xlo, xhi)
  implicit none
  real*4 xmin, xmax, xlo, xhi, epsX, deltaX
  integer*4 ix, numAreaX
  epsX = 0.0001 * (xmax - xmin)
  deltaX = (xmax - xmin) / numAreaX
  xlo = xmin + (ix - 1.) * deltaX - epsX
  xhi = xmin + ix * deltaX + epsX
  return
end subroutine subareaLimits

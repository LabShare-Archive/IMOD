! ***    INPUT_MODEL reads in the model data, reads the header info from an
! image file, sorts out the model points that are to be included in
! the analysis, and converts the coordinates to "index" coordinates
! with the origin at the center of the section
!
! $Id$
!
subroutine input_model(imodObj, imodCont, numProjPt,  iwhichOut, xcen, ycen, xdelt, &
    listz, maxZlist, modelFile, residualFile, pointFile, iuAngle, iuXtilt, pipinput)
  !
  use alivar
  implicit none
  real*4 xcen, ycen
  integer*4 imodObj(*), imodCont(*), listz(*)
  integer*4 numProjPt, maxZlist
  integer*4 iwhichOut, iuAngle, iuXtilt
  character*(*) modelFile, pointFile, residualFile
  !
  character*320 soluFile, angleFile
  logical exist, readSmallMod, pipinput
  integer getImodHead, getImodScales, getImodMaxes, numberInList, getModelName
  !
  include 'smallmodel.inc90'
  !
  integer*4 nxyz(3), mxyz(3)
  real*4 delta(3)
  !
  integer*4 numInZlist, iobject, numInObj, ipt, iz, i, j, itmp, minZval, ifSpecify
  integer*4 ninOutList, ivstr, ivend, ivinc, ifOnList, ibase, nprojTmp
  integer*4 inlist, mode, ierr, ifFlip, ifpip, ierr2, ierr3
  real*4 xorig, yorig, zorig, xdelt, ydelt, dmin, dmax, dmean
  real*4 xyScale, zscale, xofs, yofs, zofs
  logical residualOut, resBothOut, gotDot
  character*1024 listString
  integer*4 PipGetThreeIntegers
  integer*4 PipGetString, PipGetTwoIntegers, PipGetTwoFloats
  !
  ! read model in
  !
  if (pipinput) then
    ifpip = 1
    if (PipGetString('ModelFile', modelFile) .ne. 0) call errorExit( &
        'NO INPUT FIDUCIAL MODEL FILE ENTERED', 0)
  else
    ifpip = 0
    write(*,'(1x,a,$)') 'Model file: '
    read(5, '(a)') modelFile
  endif
  exist = readSmallMod(modelFile)
  if (.not.exist) call errorExit('READING MODEL FILE', 0)
  ierr = getModelName(listString)
  patchTrackModel = listString == 'Patch Tracking Model'
  !
  ! get dimensions of file and header info on origin and delta
  !
  if (pipinput) then
    modelFile = ' '
    ierr = PipGetString('ImageFile', modelFile)
  else
    print *,'Image file with matching header info'// &
        ' (Return to enter nx,ny directly)'
    read(5, '(a)') modelFile
  endif
  !
  if (modelFile == ' ') then
    !
    ! DNM 7 / 28 / 02: get defaults from  model header, including z size
    !
    ierr = getImodHead(xyScale, zscale, xofs, yofs, zofs, ifFlip)
    ierr = getImodScales(delta(1), delta(2), delta(3))
    ierr = getImodMaxes(nxyz(1), nxyz(2), nxyz(3))
    xorig = -xofs
    yorig = -yofs
    zorig = -zofs
    if (pipinput) then
      ierr = PipGetTwoIntegers('ImageSizeXandY', nxyz(1), nxyz(2))
      ierr = PipGetTwoFloats('ImageOriginXandY', xorig, yorig)
      ierr = PipGetTwoFloats('ImagePixelSizeXandY', delta(1), delta(2))
    else
      write(*,'(1x,a,$)') &
          'nx, ny, x origin, y origin, x delta, y delta: '
      read(5,*) nxyz(1), nxyz(2), xorig, yorig, delta(1), delta(2)
    endif
  else
    call ialprt(.false.)                    !don't want to see header!
    call imopen(1, modelFile, 'ro')
    call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
    call irtorg(1, xorig, yorig, zorig)
    call irtdel(1, delta)
    call imclose(1)
  endif
  xdelt = delta(1)
  ydelt = delta(2)
  xcen = nxyz(1) / 2.
  ycen = nxyz(2) / 2.
  !
  ! scan to get list of z values
  !
  numInZlist = 0
  do iobject = 1, max_mod_obj
    numInObj = npt_in_obj(iobject)
    if (numInObj > 1) then
      do ipt = 1, numInObj
        iz = nint((p_coord(3, object(ipt + ibase_obj(iobject))) +  zorig) / delta(3))
        if (numberInList(iz, listz, numInZlist, 0) == 0) then
          numInZlist = numInZlist + 1
          if (numInZlist > maxZlist) &
              call errorExit('TOO MANY Z VALUES IN MODEL FOR TEMPORARY ARRAYS')
          listz(numInZlist) = iz
        endif
      enddo
    endif
  enddo
  if (n_point == 0) call errorExit('THE FIDUCIAL MODEL IS EMPTY', 0)
  if (numInZlist == 0) call errorExit( &
      'THE FIDUCIAL MODEL HAS NO CONTOURS WITH MORE THAN ONE POINT', 0)
  if (numInZlist == 1) call errorExit( &
      'THE FIDUCIAL MODEL HAS POINTS ON ONLY ONE VIEW', 0)
  !
  ! order list of z values
  !
  do i = 1, numInZlist - 1
    do j = i + 1, numInZlist
      if (listz(i) > listz(j)) then
        itmp = listz(i)
        listz(i) = listz(j)
        listz(j) = itmp
      endif
    enddo
  enddo
  !
  ! set minimum z value if any are negative, and set number of views
  ! in file to be maximum of number acquired from model or file or
  ! range of z values
  !
  minZval = min(0, listz(1))
  !
  ! DNM 7 / 12 / 02: now that we convert to index coordinates, we
  ! use the minimum just computed.
  ! # of file views should be the size of file, but make it big
  ! enough to hold all z values actually found
  !
  nfileviews = max(listz(numInZlist) - minZval + 1, nxyz(3))
  !
  ! get name of file for output model
  !
  residualOut = .false.
  if (pipinput) then
    modelFile = ' '
    residualFile = ' '
    if (PipGetString('OutputModelAndResidual', modelFile) == 0) then
      resBothOut = .true.
    else
      resBothOut = .false.
      ierr = PipGetString('OutputModelFile', modelFile)
      ierr = PipGetString('OutputResidualFile', residualFile)
    endif
  else
    residualFile = ' '
    write(*,'(1x,a,/,a)') 'Enter file name for output model of'// &
        ' solved X-Y-Z coordinates,', ' or a name containing .res'// &
        'for a list of residuals,', ' or a name without an '// &
        'extension for both outputs, or Return for neither'
    read(5, '(a)') modelFile
    !
    ! 7 / 26 / 02: if modelFile contains .res, output residuals
    ! 12 / 20 / 02: if there is no extension, output both residual and
    ! model
    !
    resBothOut = modelFile .ne. ' '
    gotDot = .false.
    do i = 1, len_trim(modelFile) - 2
      if (gotDot .and. modelFile(i:i + 2) == 'res') residualOut = .true.
      if (gotDot .and. modelFile(i:i + 2) == 'mod') resBothOut = .false.
      if (modelFile(i:i) == '.')  gotDot = .true.
    enddo
  endif
  !
  if (residualOut .or. resBothOut) then
    if (residualOut) then
      residualFile = modelFile
    else
      residualFile = trim(modelFile)//'.resid'
    endif
    !
    ! manage the model file name now; attach extension if model
    ! wanted, or null it out if not
    !
    if (residualOut) then
      modelFile = ' '
    else
      modelFile = trim(modelFile)//'.3dmod'
    endif
  endif
  !
  ! get name of files for angle list output and 3 - D point output
  !
  iuAngle = 0
  angleFile = ' '
  pointFile = ' '
  if (pipinput) then
    ierr = PipGetString('OutputFidXYZFile', pointFile)
    ierr = PipGetString('OutputTiltFile', angleFile)
  else
    write(*,'(1x,a)') 'Enter file name for ASCII list of'// &
        ' solved X-Y-Z coordinates (Return for none)'
    read(5, '(a)') pointFile
    !
    write(*,'(1x,a)') 'Enter name of output file for list of'// &
        ' solved tilt angles (Return for none)'
    read(5, '(a)') angleFile
  endif
  if (angleFile .ne. ' ') then
    call dopen(9, angleFile, 'new', 'f')
    iuAngle = 9
  endif
  !
  ! if iuXtilt comes in non - zero, ask about file for x axis tilts
  !
  angleFile = ' '
  if (pipinput) then
    ierr = PipGetString('OutputXAxisTiltFile', angleFile)
  else if (iuXtilt .ne. 0) then
    iuXtilt = 0
    write(*,'(1x,a)') 'Enter name of output file for list of'// &
        ' X-axis tilt angles (Return for none)'
    read(5, '(a)') angleFile
  endif
  if (angleFile .ne. ' ') then
    call dopen(10, angleFile, 'new', 'f')
    iuXtilt = 10
  endif
  !
  ! open output file to put transforms into
  !
  if (pipinput) then
    soluFile = ' '
    ierr = PipgetString('OutputTransformFile', soluFile)
    iwhichOut = 1
  else
    write(*,'(1x,a,$)') 'Output file for solutions/transforms: '
    read(5, '(a)') soluFile
    !
    write(*,'(1x,a,$)') '1 to put only xforms in file,' &
        //' -1 to put only solutions, 0 both: '
    read(5,*) iwhichOut
  endif
  call dopen(7, soluFile, 'new', 'f')
  !
  ! find out which z values to include; end up with a list of z values
  !
  ninOutList = 0
  if (pipinput) then
    ifSpecify = 0
    ierr = PipGetThreeIntegers('IncludeStartEndInc', ivstr, ivend, ivinc)
    ierr2 = PipGetString('IncludeList', listString)
    ierr3 = PipGetString('ExcludeList', listString)
    if (ierr + ierr2 + ierr3 < 2) call errorExit( &
        'YOU MAY ENTER ONLY ONE OF IncludeStartEndInc, '// &
        'IncludeList, OR ExcludeList', 0)
    if (ierr == 0) ifSpecify = 1
    if (ierr2 + ierr3 == 1) then
      call parseList(listString, imodObj, ninOutList)
      if (ierr3 == 0) ifSpecify = 3
    endif
  else
    !
    write(*,'(1x,a,i3,a,/,a,/,a,/,a,$)') 'Enter 0 to include all', numInZlist, &
        ' views with points' &
        , '    or 1 to enter start, end, and increment view numbers' &
        , '    or 2 to specify a list of views to include' &
        , '    or 3 to specify a list of views to exclude: '
    read(5,*) ifSpecify
    !
    if (ifSpecify > 0) then
      if (ifSpecify == 1) then
        write(*,'(1x,a,$)') &
            'Start, end, and increment of views to include: '
        read(5,*) ivstr, ivend, ivinc
      elseif (ifSpecify == 2) then
        write(*,'(1x,a,$)') 'Enter views to include (ranges ok): '
        call rdlist(5, imodObj, ninOutList)
      else
        write(*,'(1x,a,$)') 'Enter views to exclude (ranges ok): '
        call rdlist(5, imodObj, ninOutList)
      endif
    endif
  endif
  if (ifSpecify == 1) then
    ninOutList = 1 + (ivend - ivstr) / ivinc
    do i = 1, ninOutList
      imodObj(i) = ivstr + (i - 1) * ivinc
    enddo
  endif
  !
  ! go through list of z values in model and make sure they are on
  ! an include list or not on an exclude list
  !
  if (ninOutList > 0) then
    i = 1
    do while(i <= numInZlist)
      ifOnList = 0
      do j = 1, ninOutList
        if (listz(i) + 1 == imodObj(j)) ifOnList = 1
      enddo
      !
      ! remove points on exclude list or not on include list
      !
      if (ifSpecify <= 2 .neqv. ifOnList == 1) then
        numInZlist = numInZlist - 1
        do j = i, numInZlist
          listz(j) = listz(j + 1)
        enddo
      else
        i = i + 1
      endif
    enddo
  endif
  !
  ! Count the number of points in valid objects on selected views
  numProjPt = 0
  nrealpt = 0
  imodObj(1:numInZlist) = 0
  do iobject = 1, max_mod_obj
    numInObj = npt_in_obj(iobject)
    ibase = ibase_obj(iobject)
    if (numInObj > 1) then
      !
      ! First determine if the object is usable at all: if it has at least 2 points
      inlist = 0
      do ipt = 1, numInObj
        if (numberInList(nint((p_coord(3, object(ipt + ibase)) + zorig) / delta(3)), &
            listz, numInZlist, 0) .ne. 0) then
          inlist = inlist + 1
          if (inlist > 1) exit
        endif
      enddo
      !
      ! If so, then count the points on valid views, and keep track of those views
      if (inlist > 1) then
        nRealPt = nRealPt + 1
        do ipt = 1, numInObj
          iz = nint((p_coord(3, object(ipt + ibase)) + zorig) / delta(3))
          do i = 1, numInZlist
            if (iz == listz(i)) then
              numProjPt = numProjPt + 1
              imodObj(i) = imodObj(i) + 1
              exit
            endif
          enddo
        enddo
      endif
    endif
  enddo
  !
  ! Trim the Z list if that excluded any views
  inlist = 0
  do i = 1, numInZlist
    if (imodObj(i) > 0) then
      inlist = inlist + 1
      listz(inlist) = listz(i)
    endif
  enddo
  numInZlist = inlist
  if (numInZlist < 2) call errorExit( &
      'THE FIDUCIAL MODEL HAS USABLE POINTS ON ONLY ONE VIEW', 0)
  nview = numInZlist
  !
  ! Do some big allocations

  call allocateAlivar(numProjPt, nview, nrealpt, ierr)
  call memoryError(ierr, 'ARRAYS IN ALIVAR')
  call allocateFunctVars(ierr)
  call memoryError(ierr, 'ARRAYS FOR FUNCT')
  !
  ! go through model finding objects with more than one point in the
  ! proper z range, and convert to index coordinates, origin at center
  !
  numProjPt = 0
  nrealpt = 0
  do iobject = 1, max_mod_obj                  !loop on objects
    numInObj = npt_in_obj(iobject)
    ibase = ibase_obj(iobject)
    if (numInObj > 1) then                     !non - empty object
      nprojTmp = numProjPt                      !save number at start of object
      do ipt = 1, numInObj                       !loop on points
        !
        ! find out if the z coordinate of this point is on the list
        !
        iz = nint((p_coord(3, object(ipt + ibase)) + zorig) / delta(3))
        inlist = 0
        do i = 1, numInZlist
          if (iz == listz(i)) inlist = i
        enddo
        if (inlist > 0) then
          !
          ! if so, tentatively add index coordinates to list but first
          ! check for two points on same view
          !
          do i = numProjPt + 1, nprojTmp
            if (inlist == isecView(i)) then
              !
              ! Find the other point
              do j = 1, ipt - 1
                if (nint((p_coord(3, object(j + ibase)) + zorig) / delta(3)) == iz) &
                    inlist = j
              enddo
              call objToCont(iobject, obj_color, ibase, numInObj)
              write(*,'(/,a,i5,a,i5,a,i5,a,i5,a,i4)') &
                  'ERROR: TILTALIGN - TWO POINTS (#', inlist, ' AND', ipt, &
                  ') ON VIEW', iz + 1, ' IN CONTOUR', numInObj, ' OF OBJECT', ibase
              call exit(1)
            endif
          enddo
          nprojTmp = nprojTmp + 1
          if (nprojTmp > maxprojpt) call errorExit( &
              'TOO MANY PROJECTION POINTS FOR ARRAYS', 0)
          xx(nprojTmp) = (p_coord(1, object(ipt + ibase)) + xorig) / xdelt - xcen
          yy(nprojTmp) = (p_coord(2, object(ipt + ibase)) + yorig) / ydelt - ycen
          isecView(nprojTmp) = inlist
        endif
      enddo
      !
      ! if there are at least 2 points, take it as a real point
      !
      if (nprojTmp - numProjPt >= 2) then
        nrealpt = nrealpt + 1
        if (nrealpt > maxreal) call errorExit('TOO MANY FIDUCIAL POINTS FOR ARRAYS', 0) 
        irealstr(nrealpt) = numProjPt + 1
        call objToCont(iobject, obj_color, imodObj(nrealpt), imodCont(nrealpt))
        numProjPt = nprojTmp
        npt_in_obj(iobject) = 1
      else
        npt_in_obj(iobject) = 0
      endif
    else
      npt_in_obj(iobject) = 0
    endif
  enddo
  irealstr(nrealpt + 1) = numProjPt + 1           !Needed to get number in real sometimes
  !
  ! For a patch tracking model, find which contours belong to an original full track and
  ! make lists and maps back and forth
  if (patchTrackModel) then
    call analyzePatchTracks(numProjPt)
  endif
  !
  ! shift listz to be a view list, and make the map of file to view
  ! listz will be copied into mapviewtofile
  mapFileToView(:) = 0
  do i = 1, numInZlist
    listz(i) = listz(i) - minZval + 1
    mapFileToView(listz(i)) = i
  enddo
  return
end subroutine input_model



! WRITE_XYZ_MODEL will write out a model containing only a single point
! per object with the solved XYZ coordinates, to file modelFile, if
! that string is non-blank.  IGROUP is an array with the group number
! if the points have been assigned to surfaces.
!
subroutine write_xyz_model(modelFile, xyz, igroup, nrealpt)
  !
  implicit none
  real*4 xyz(3,*)
  integer*4 igroup(*), nrealpt
  character*(*) modelFile
  include 'smallmodel.inc90'
  real*4 xyzmax, xImScale, yImScale, zImScale
  integer*4, allocatable :: imodObjSize(:), mapImodObj(:), numGroup1(:)
  integer getImodScales, clearImodObjStore
  integer*4 imodObj, isize, ierr, newObj, maxImodObj, mapInd
  !
  integer*4 ireal, iobject, ipt, i
  !
  if (modelFile == ' ') return
  !
  ! get a scattered point size (simplified 1/30/03)
  !
  xyzmax = 0.
  do ireal = 1, nrealpt
    do i = 1, 3
      xyzmax = max(xyzmax, abs(xyz(i, ireal)))
    enddo
  enddo
  isize = max(3., xyzmax / 100.)
  !
  ! Count the contours per object so that objects can be reused if all
  ! their points are in group 2 and get moved elsewhere
  maxImodObj = 0
  do iobject = 1, max_mod_obj
    maxImodObj = max(maxImodObj, 256 - obj_color(2, iobject))
  enddo
  allocate(imodObjSize(maxImodObj * 2), mapImodObj(maxImodObj * 2), &
      numGroup1(maxImodObj), stat = ierr)
  if (ierr .ne. 0) call errorExit('ALLOCATING ARRAYS FOR WRITING MODEL', 0)
  imodObjSize = 0
  mapImodObj = 0
  numGroup1 = 0
  ireal = 0
  do iobject = 1, max_mod_obj
    if (npt_in_obj(iobject) > 0) then
      imodObj = 256 - obj_color(2, iobject)
      imodObjSize(imodObj) = imodObjSize(imodObj) + 1
      ireal = ireal + 1
      if (igroup(ireal) == 1) numGroup1(imodObj) = numGroup1(imodObj) + 1
    endif
  enddo
  !
  ! loop on model objects that are non - zero, stuff point coords into
  ! first point of object (now only point), and set color by group
  !
  ! get scaling factors, might as well apply each one to each coordinate
  ! 12 / 11 / 08: No longer invert Z.  Model now fits exactly on tomogram
  ! opened either with - Y or without, except for X - axis tilt
  ! (invert Z so that model can be visualized on tomogram by shifting)
  !
  ierr = getImodScales(xImScale, yImScale, zImScale)
  ireal = 0
  do iobject = 1, max_mod_obj
    if (npt_in_obj(iobject) > 0 .and. ireal < nrealpt) then
      ipt = object(ibase_obj(iobject) + 1)
      ireal = ireal + 1
      p_coord(1, ipt) = xyz(1, ireal) * xImScale
      p_coord(2, ipt) = xyz(2, ireal) * yImScale
      p_coord(3, ipt) = xyz(3, ireal) * zImScale
      !
      ! DNM 5 / 15 / 02: only change color if there is a group
      ! assignment, but then put it in the object this one maps to
      ! If no map yet, find first free object
      ! Also, define as scattered and put sizes out
      !
      imodObj = 256 - obj_color(2, iobject)
      if (igroup(ireal) .ne. 0) then
        imodObjSize(imodObj) = imodObjSize(imodObj) - 1
        !
        ! Multiply by 2 to get index to mapping for group 1 or 2 in object
        ! If already mapped, fine
        ! If in group 1, map to self and set to green
        ! If in group 2, map to self if there are no group 1's in object,
        ! otherwise map to first empty object, and set to magenta
        mapInd = 2 * (imodObj - 1) + igroup(ireal)
        if (mapImodObj(mapInd) .ne. 0) then
          imodObj = mapImodObj(mapInd)
        else if (igroup(ireal) == 1) then
          mapImodObj(mapInd) = imodObj
          call putObjColor(imodObj, 0, 255, 0)
        else
          if (numGroup1(imodObj) > 0) then
            do newObj = 1, maxImodObj * 2 - 1
              if (imodObjSize(newObj) == 0) exit
            enddo
            imodObj = newObj
          endif
          mapImodObj(mapInd) = imodObj
          call putObjColor(imodObj, 255, 0, 255)
        endif
        obj_color(2, iobject) = 256 - imodObj
        imodObjSize(imodObj) = imodObjSize(imodObj) + 1
      endif
      call putImodFlag(imodObj, 2)
      call putScatSize(imodObj, isize)
      ierr = clearImodObjStore(imodObj);
    endif
  enddo
  call putImodZscale(1.)
  call putImodRotation(0., 0., 0.)
  !
  n_object = ireal
  call write_wmod(modelFile)
  close(20)
  deallocate(imodObjSize, mapImodObj, numGroup1)
  return
end subroutine write_xyz_model

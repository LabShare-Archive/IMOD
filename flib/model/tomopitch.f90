! TOMOPITCH  analyes simple models of the boundaries of the section
! in slices from a tomogram and recommends how much to change
! tilt angles to make the section flat, how much to shift the tilt
! axis in Z to produce centered slices, and how thick to make the
! slices.  It can also recommend how much X-axis tilt is needed to
! make the section flat in the orthogonal direction as well.  It can
! also be used with a model drawn on a whole tomogram, possibly binned
! down.
!
! See manpage for more details.
!
! David Mastronarde, January 2000
! 5/20/01: Added analysis of single file with multiple time indexes
!
! $Id$
!
program tomopitch
  implicit none
  include 'smallmodel.inc90'
  integer LIMTOT
  parameter (LIMTOT = 500)
  !
  character*320 inFile
  !
  logical readSmallMod, useTimes
  integer getImodHead, getImodMaxes, getImodTimes, getImodScales
  real*4 xcen(LIMTOT), ycen(LIMTOT), thickMid(LIMTOT), ySamp(LIMTOT)
  integer*4 ifUse(LIMTOT), itimes(max_obj_num)
  integer*4 iobjVert(2), iobjHoriz(LIMTOT), indHoriz(LIMTOT)
  real*4 yVert(2), zSpanVert(2), zmean(LIMTOT), ymean(LIMTOT)
  character*120 message
  !
  integer*4 numPatch, numFiles, ifile, ipBase, ierr, ifFlip, i, iobj, j, indy, indz
  integer*4 maxx, maxy, maxz, minTime, maxTime, numObjects, iobj1, iobj2
  real*4 border, deltaY, ySample, xyScale, zScale, xOffset, yOffset, zOffset
  real*4 xImScale, yImScale, zImScale, yMiddle, scaleFac
  real*4 yval, zVal, zSpan, diffMin, xend, yEnd, zEnd, yLine, zline
  real*4 alphaAdd, thetaAdd, shiftAdd, alphaOld, thetaOld, shiftOld
  integer*4 ifNoAlpha, ifNoTheta, ifNoShift, numLinePairs
  integer*4 numVertical, numHoriz, ip1, ip2, imodObj, imodCont
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg, PipNumberOfEntries
  integer*4 PipGetString, PipGetFloat
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  tomopitch
  !
  integer numOptions
  parameter (numOptions = 9)
  character*(40 * numOptions) options(1)
  options(1) = &
      'model:InputFile:FNM:@extra:ExtraThickness:F:@'// &
      'spacing:SpacingInY:F:@scale:ScaleFactor:F:@'// &
      'angle:AngleOffsetOld:F:@zshift:ZShiftOld:F:@'// &
      'xtilt:XAxisTiltOld:F:@param:ParameterFile:PF:@help:usage:B:'

  numPatch = 2
  scaleFac = 1.
  deltaY = 0.
  border = 2.
  ifNoAlpha = 1
  ifNoTheta = 1
  ifNoShift = 1
  !
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'tomopitch', &
      'ERROR: TOMOPITCH - ', .true., 1, 0, 0, numOptArg, numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0
  if (pipInput) then
    ierr = PipNumberOfEntries('ModelFile', numFiles)
    if (numFiles == 0) call exitError('ERROR: TOMOPITCH - NO MODEL FILE ENTERED')
    ierr = PipGetFloat('SpacingInY', deltaY)
    ierr = PipGetFloat('ExtraThickness', border)
    ierr = PipGetFloat('ScaleFactor', scaleFac)
    ifNoAlpha = PipGetFloat('XAxisTiltOld', alphaOld)
    ifNoTheta = PipGetFloat('AngleOffsetOld', thetaOld)
    ifNoShift = PipGetFloat('ZShiftOld', shiftOld)
  else
    write(*,'(1x,a,$)') 'Additional thickness to add outside model lines: '
    read(5,*) border
    !
    write(*,'(1x,a,$)') 'For analysis of X-axis tilt, enter '// &
        'distance between sample tomograms: '
    read(5,*) deltaY
    !
    write(*,'(1x,a,$)') 'Number of model files: '
    read(5,*) numFiles
  endif
  if (deltaY > 0. .and. numFiles > 0) &
      write(*,'(/,a,/)') ' MODEL FILES OR TIMES MUST '// &
      'OCCUR IN ORDER OF INCREASING SAMPLE COORDINATE'
  if (deltaY < 0. .and. numFiles > 0) &
      write(*,'(/,a,/)') ' MODEL FILES OR TIMES MUST '// &
      'OCCUR IN ORDER OF DECREASING SAMPLE COORDINATE'

  ipBase = 1
  ySample = -deltaY * (numFiles - 1.) / 2.
  numVertical = 0
  numHoriz = 0
  numLinePairs = 0
  indy = 2
  indz = 3

  ifile = 1
  useTimes = .false.

  ! Loop on the files; this turns into a loop on times if times are discovered
  do while(ifile <= numFiles)
    
    ! First time or if no times and not drawing in whole tomo, read a file
    if (.not.useTimes .and. numVertical == 0 .and. numHoriz == 0) then
      if (pipInput) then
        ierr = PipGetString('ModelFile', inFile)
      else
        write(*,'(1x,a,i2,a,$)') 'Name of model file #', ifile, ': '
        read(5, '(a)') inFile
      endif
      if (.not.readSmallMod(inFile)) then
        inquire(file = inFile, exist = useTimes)
        if (.not. useTimes) then
          write(*,'(/,a,a,a)') 'ERROR: TOMOPITCH - MODEL FILE ', &
              trim(inFile), ' DOES NOT EXIST - DID YOU SAVE IT FROM 3dmod?'
        else
          write(*,'(/,a,a)') 'ERROR: TOMOPITCH - READING MODEL FILE ', trim(inFile)
        endif
        call exit(1)
      endif
      ierr = getImodHead(xyScale, zScale, xOffset, yOffset, zOffset, ifFlip)
      ierr = getImodMaxes(maxx, maxy, maxz)
      ierr = getImodScales(xImScale, yImScale, zImScale)
      ! print *,xOffset, yOffset, zOffset, ifFlip
      ! print *,maxx, maxy, maxz
      !
      ! convert to centered index coordinates - maxes are correct
      do i = 1, n_point
        p_coord(1, i) = (p_coord(1, i) - xOffset) / xImScale - maxx / 2
        p_coord(2, i) = (p_coord(2, i) - yOffset) / yImScale - maxy / 2
        p_coord(3, i) = (p_coord(3, i) - zOffset) / zImScale - maxz / 2
        ! write(*,'(3f7.1)') (p_coord(j, i), j=1, 3)
      enddo
      !
      ! if there is one file, see if there are multiple times
      if (numFiles == 1) then
        ierr = getImodTimes(itimes)
        minTime = 100000
        maxTime = 0
        do iobj = 1, max_mod_obj
          if (npt_in_obj(iobj) > 0) then
            minTime = min(minTime, itimes(iobj))
            maxTime = max(maxTime, itimes(iobj))
          endif
        enddo
        if (maxTime > 0) then
          useTimes = .true.
          numFiles = maxTime
          !
          ! DNM 11/11/03: adjust ySample here, not below
          ySample = -deltaY * (numFiles - 1.) / 2.
          if (minTime == 0) then
            write(*,'(/,a,/,a)') 'ERROR: TOMOPITCH - THE MODEL FILE HAS '// &
                'MULTIPLE TIMES BUT HAS SOME CONTOURS WITH', 'ERROR: (line 2):    '// &
                'NO TIME INDEX. THESE CONTOURS SHOULD BE ELIMINATED'
            call exit(1)
          endif
        else
          !
          ! now check for whether there are multiple lines in one model
          ! 2/3/05: DO NOT swap y and Z if y is the long dimension,
          ! data are already flipped back so Z is between-slice dimension
          !
          do iobj = 1, max_mod_obj
            call objToCont(iobj, obj_color, imodObj, imodCont)
            if (npt_in_obj(iobj) > 2) then
              write(*,'(/,a,i5,a,i3,a)') 'ERROR: TOMOPITCH - contour', &
                  imodCont, ' in object', imodObj, ' has more than 2 points'
              call exit(1)
            endif
            if (npt_in_obj(iobj) > 1) then
              ip1 = object(ibase_obj(iobj) + 1)
              ip2 = object(ibase_obj(iobj) + npt_in_obj(iobj))
              yval = 0.5 * (p_coord(indy, ip1) + p_coord(indy, ip2))
              zVal = 0.5 * (p_coord(indz, ip1) + p_coord(indz, ip2))
              zSpan = abs(p_coord(indz, ip1) - p_coord(indz, ip2))
              if (abs(p_coord(1, ip1) - p_coord(1, ip2)) < &
                  0.2 * zSpan) &
                  then
                numVertical = numVertical + 1
                if (numVertical <= 2) then
                  iobjVert(numVertical) = iobj
                  yVert(numVertical) = yval
                  zSpanVert(numVertical) = zSpan
                endif
              else if (zSpan < &
                  0.2 * abs(p_coord(1, ip1) - p_coord(1, ip2))) then
                numHoriz = numHoriz + 1
                if (numHoriz > LIMTOT) call exitError('TOO MANY LINES FOR ARRAYS')
                iobjHoriz(numHoriz) = iobj
                zmean(numHoriz) = zVal
                ymean(numHoriz) = yval
                indHoriz(numHoriz) = numHoriz
              else
                write(*, '(/,a,i5,a,i3,a)') 'ERROR: TOMOPITCH - contour', &
                    imodCont, ' in object', imodObj, ' is too diagonal to analyze'
                call exit(1)
              endif
            endif
          enddo
          !
          ! Now require 2 horiz & 2 vert, or even number of horiz lines
          !
          if (numVertical > 0 .and. numVertical .ne. 2 .and. &
              numHoriz .ne. 2) call exitError('TO USE CROSSED LINES'// &
              ', YOU MUST HAVE 2 HORIZONTAL AND 2 VERTICAL LINES')
          if (mod(numHoriz, 2) .ne. 0) call exitError('YOU MUST HAVE AN EVEN NUMBER '// &
              'OF LINES IN A SINGLE MODEL FILE')
          if (numVertical > 0) then
            !
            ! for crossed lines, set up for 3 "files" and handle below
            ! adjust horizontal index to  match top & bottom
            !
            numFiles = 3
            if ((yVert(1) > yVert(2) .and. ymean(1) < ymean(2)) .or. &
                (yVert(1) < yVert(2) .and. ymean(1) > ymean(2))) then
              indHoriz(1) = 2
              indHoriz(2) = 1
            endif
            if (abs(zmean(1) - zmean(2)) > 0.3 * &
                min(zSpanVert(1), zSpanVert(2))) call exitError('DISTANCE BETWEEN '// &
                'HORIZONTAL LINES MUST BE LESS THAN 0.3 TIMES LENGTH OF VERTICAL ONES')

          else if (numHoriz > 2) then
            !
            ! for multiple horizontal lines, sort them by Z and make sure
            ! they make sense
            !
            numFiles = numHoriz / 2
            do i = 1, numHoriz - 1
              do j = i + 1, numHoriz
                if (zmean(indHoriz(i)) > zmean(indHoriz(j))) then
                  iobj = indHoriz(i)
                  indHoriz(i) = indHoriz(j)
                  indHoriz(j) = iobj
                endif
              enddo
            enddo

            do i = 1, numFiles
              j = 2 * i
              diffMin = 1.e10
              if (i > 1) diffMin = min(diffMin, &
                  zmean(indHoriz(j - 1)) - zmean(indHoriz(j - 2)))
              if (i < numFiles) diffMin = min(diffMin, &
                  zmean(indHoriz(j + 1)) - zmean(indHoriz(j)))
              if (zmean(indHoriz(j)) - zmean(indHoriz(j - 1)) > &
                  0.3 * diffMin) &
                  call exitError('THE SPACING BETWEEN TWO LINES OF A PAIR MUST BE '// &
                  'LESS THAN 0.3 TIMES THE SPACING BETWEEN PAIRS')
            enddo
          else
            !
            ! otherwise, need to restore indexes
            !
            indy = 2
            indz = 3
          endif
        endif
      endif
    endif
    if (numFiles * numPatch > LIMTOT) call exitError( &
        'ERROR: TOMOPITCH - TOO MANY TOTAL PATCHES FOR ARRAYS')
    !
    ! now get the first 2 contours in model or time or next pair
    ! of horizontal lines, or make up lines in crossed case
    !
    if (numVertical > 0 .and. ifile > 1) then
      !
      ! shift the horizontal lines to the endpoints of the vertical ones
      !
      ySample = 0.
      do i = 1, 2
        !
        ! get appropriate endpoint of line
        !
        ip1 = object(ibase_obj(iobjVert(i)) + 1)
        ip2 = object(ibase_obj(iobjVert(i)) + npt_in_obj(iobjVert(i)))
        if ((ifile == 2 .and. p_coord(indz, ip1) > p_coord(indz, ip2)) &
            .or. (ifile == 3 .and. &
            p_coord(indz, ip1) < p_coord(indz, ip2))) ip1 = ip2
        xend = p_coord(1, ip1)
        yEnd = p_coord(indy, ip1)
        zEnd = p_coord(indz, ip1)
        ! print *,i, iobjVert(i), ip1, ip2, xend, yEnd, zEnd
        !
        ! solve for y and z coordinate of existing horizontal line at
        ! that X value, and shift Y and Z of endpoints to move that
        ! point to the desired endpoint
        !
        iobj = iobjHoriz(indHoriz(i))
        ip1 = object(ibase_obj(iobj) + 1)
        ip2 = object(ibase_obj(iobj) + npt_in_obj(iobj))
        yLine = p_coord(indy, ip1) + (xend - p_coord(1, ip1)) * &
            (p_coord(indy, ip2) - p_coord(indy, ip1)) / &
            (p_coord(1, ip2) - p_coord(1, ip1))
        zline = p_coord(indz, ip1) + (xend - p_coord(1, ip1)) * &
            (p_coord(indz, ip2) - p_coord(indz, ip1)) / &
            (p_coord(1, ip2) - p_coord(1, ip1))
        p_coord(indy, ip1) = p_coord(indy, ip1) + yEnd - yLine
        p_coord(indy, ip2) = p_coord(indy, ip2) + yEnd - yLine
        p_coord(indz, ip1) = p_coord(indz, ip1) + zEnd - zline
        p_coord(indz, ip2) = p_coord(indz, ip2) + zEnd - zline
        ySample = ySample + 0.5 * zEnd
      enddo
      iobj1 = iobjHoriz(1)
      iobj2 = iobjHoriz(2)
      numObjects = 2
      write(message, '(a,f7.0)') 'lines moved to Y =', ySample + maxz / 2.

    else if (numHoriz > 2 .or. numVertical > 0) then
      !
      ! get pair of horizontal lines
      !
      numObjects = 2
      ip1 = indHoriz(2 * ifile - 1)
      ip2 = indHoriz(2 * ifile)
      iobj1 = iobjHoriz(ip1)
      iobj2 = iobjHoriz(ip2)
      ySample = (zmean(ip1) + zmean(ip2)) / 2.
      write(message, '(a,f7.0)') 'line pair at Y =', ySample + maxz / 2.

    else if (useTimes) then
      !
      ! get lines at next time
      !
      numObjects = 0
      do iobj = 1, max_mod_obj
        if (npt_in_obj(iobj) > 0 .and. &
            itimes(iobj) == ifile .and. numObjects <= 2) then
          numObjects = numObjects + 1
          if (numObjects == 1) iobj1 = iobj
          if (numObjects == 2) iobj2 = iobj
        endif
      enddo
      if (numObjects > 2) then
        write(*, '(/,a,a,i3)') 'ERROR: TOMOPITCH - THERE ARE MORE THAN TWO ', &
            'CONTOURS AT TIME INDEX', ifile
        call exit(1)
      endif
      write(message, '(a,i3)') 'time index', ifile
    else
      !
      ! or just get first two object
      !
      numObjects = min(2, max_mod_obj)
      iobj1 = 1
      iobj2 = 2
      message = inFile
    endif

    if (numObjects > 0 .or. .not.useTimes) then
      if (numObjects < 2) then
        write(*, '(/,a,i3,a)') 'ERROR: TOMOPITCH - MODEL OR TIME', ifile, &
            ' DOES NOT HAVE ENOUGH CONTOURS'
        call exit(1)
      endif
      if (npt_in_obj(iobj1) < 2 .or. npt_in_obj(iobj2) < 2) then
        write(*, '(/,a,a,i3)') 'ERROR: TOMOPITCH - THERE ARE FEWER THAN TWO POINTS ', &
            'IN ONE OF THE FIRST TWO CONTOURS OF MODEL OR TIME', ifile
        call exit(1)
      endif


      call addLinePair(message, iobj1, iobj2, scaleFac * ySample, &
          border, scaleFac, indy, maxx, xcen, ycen, thickMid, &
          ifUse, ySamp, numPatch, ipBase)
      numLinePairs = numLinePairs + 1
    endif
    ySample = ySample + deltaY
    ifile = ifile + 1
  enddo
  !
  ! DNM 6/25/02: need to make the ySamp values symmetric around zero
  ! i.e., assume that the samples are symmetric in data set
  ! 11/11/03: remove adjustment, which must have broken the 3-model case
  !
  message = 'all files'
  if (useTimes) message = 'all time indexes'
  if (numHoriz > 2 .or. numVertical > 0) then
    message = 'all line pairs'
    deltaY = 1.
  endif
  alphaAdd = 0.
  if (numFiles == 1 .or. (useTimes .and. numLinePairs < 2)) deltaY = 0.
  call analyzeSpots(trim(message), xcen, ycen, &
      thickMid, ifUse, ipBase-1, ySamp, deltaY, alphaAdd, thetaAdd, shiftAdd)
  if (ifNoAlpha == 0 .or. ifNoTheta == 0 .or. ifNoShift == 0) &
      print *
  if (ifNoAlpha == 0) write(*,102) &
      'X axis tilt - ', alphaOld, alphaAdd, alphaOld + alphaAdd
  if (ifNoTheta == 0) write(*,102) &
      'Angle offset -', thetaOld, thetaAdd, thetaOld + thetaAdd
  if (ifNoShift == 0) write(*,101) &
      'Z shift -     ', shiftOld, shiftAdd, shiftOld + shiftAdd
101 format(1x, a, ' Original:', f8.1,'   Added:',f8.1,'   Total:',f8.1)
102 format(1x, a, ' Original:', f8.2,'   Added:',f8.2,'   Total:',f8.2)
  call exit(0)
end program tomopitch


subroutine addLinePair(message, iobj1, iobj2, ySample, border, &
    scaleFac, indy, maxx, &
    xcen, ycen, thickMid, ifUse, ySamp, numPatch, ipBase)
  implicit none
  include 'smallmodel.inc90'
  character*(*) message
  integer*4 iobj1, iobj2, numPatch, ipBase, maxx, indy, ifUse(*)
  real*4 ySample, border, scaleFac, xcen(*), ycen(*), thickMid(*), ySamp(*)
  integer*4 ibotTop(2)
  real*4 slope(2), bintcp(2), xLeft(2), xRight(2)
  integer*4 ip1, ip2, ibt, line
  real*4 x1, y1, x2, y2, xlo, xhi, yll, ylr, yul, yur, alphaAdd, thetaAdd, shiftAdd

  ibotTop(1) = iobj1
  ibotTop(2) = iobj2
  if (p_coord(indy, object(ibase_obj(iobj1) + 1)) + &
      p_coord(indy, object(ibase_obj(iobj1) + npt_in_obj(iobj1))) > &
      p_coord(indy, object(ibase_obj(iobj2) + 1)) + &
      p_coord(indy, object(ibase_obj(iobj2) + npt_in_obj(iobj2)))) then
    ibotTop(1) = iobj2
    ibotTop(2) = iobj1
  endif

  do line = 1, 2
    ibt = ibotTop(line)
    ip1 = object(ibase_obj(ibt) + 1)
    ip2 = object(ibase_obj(ibt) + npt_in_obj(ibt))
    x1 = scaleFac * p_coord(1, ip1)
    y1 = scaleFac * p_coord(indy, ip1)
    x2 = scaleFac * p_coord(1, ip2)
    y2 = scaleFac * p_coord(indy, ip2)
    if (abs(x2 - x1) < 0.1 * abs(y1 - y2)) then
      write(*, '(/,a,2f7.0,a,2f7.0,a)') 'ERROR: TOMOPITCH - LINE FROM', x1, y1, ' TO', &
          x2, y2, ' IS TOO STEEP TO USE'
      call exit(1)
    endif
    slope(line) = (y2 - y1) / (x2 - x1)
    bintcp(line) = y2 - slope(line) * x2
    xLeft(line) = min(x1, x2)
    xRight(line) = max(x1, x2)
  enddo

  xlo = min(xLeft(1), xLeft(2), -0.45 * scaleFac * maxx)
  xhi = max(xRight(1), xRight(2), 0.45 * scaleFac * maxx)
  if (xRight(1) - xLeft(1) < 0.1 * (xhi - xlo) .or. &
      xRight(2) - xLeft(2) < 0.1 * (xhi - xlo)) then
    line = 2
    if (xRight(1) - xLeft(1) < 0.1 * (xhi - xlo)) line = 1
    write(*, '(/,a,f7.0,a,f7.0,a)') 'ERROR: TOMOPITCH - THE LINE GOING FROM', &
        xLeft(line), ' TO', xRight(line), ' IS TOO SHORT TO USE'
    call exit(1)
  endif

  yll = xlo * slope(1) + bintcp(1)
  ylr = xhi * slope(1) + bintcp(1)
  yul = xlo * slope(2) + bintcp(2)
  yur = xhi * slope(2) + bintcp(2)
  xcen(ipBase) = xlo
  ycen(ipBase) = (yll + yul) / 2.
  thickMid(ipBase) = 2 * border + yul - yll
  ySamp(ipBase) = ySample
  xcen(ipBase + 1) = xhi
  ycen(ipBase + 1) = (ylr + yur) / 2.
  thickMid(ipBase + 1) = 2 * border + yur - ylr
  ySamp(ipBase + 1) = ySample
  ifUse(ipBase) = 1
  ifUse(ipBase + 1) = 1
  !
  call analyzeSpots(trim(message), xcen(ipBase), &
      ycen(ipBase), thickMid(ipBase), ifUse(ipBase), numPatch, &
      ySamp(ipBase), 0., alphaAdd, thetaAdd, shiftAdd)
  ipBase = ipBase + numPatch
  return
end subroutine addLinePair


subroutine analyzeSpots(fileLabel, xcen, ycen, &
    thickMid, ifUse, numSpots, ySamp, doXtilt, alpha, thetaAdd, shiftAdd)
  implicit none
  character*(*) fileLabel
  real*4 xcen(*), ycen(*), thickMid(*), ySamp(*)
  integer*4 ifUse(*), numSpots
  real*4 xx(1000), yy(1000), zz(1000), doXtilt, thetaAdd, shiftAdd
  !
  integer*4 nd, i
  real*4 angle, slope, bintcp, ro, a, b, c, alpha, theta, cosTheta, sinTheta
  real*4 cosAplha, sinAlpha, zp
  real*4 atand, cosd, sind
  !
  write (6, 101) fileLabel
101 format(//,' Analysis of positions from ',a,':')
  if (numSpots > 1 .and. thickMid(1) < 0. .or. thickMid(2) < 0) &
      call exitError('LINES CROSS WHEN EXTRAPOLATED TO THE FULL RANGE IN X')
  call findshift('unrotated', ycen, thickMid, ifUse, numSpots, shiftAdd)
  nd = 0
  do i = 1, numSpots
    if (ifUse(i) .ne. 0) then
      nd = nd + 1
      xx(nd) = xcen(i)
      yy(nd) = ycen(i)
    endif
  enddo
  call lsfit(xx, yy, nd, slope, bintcp, ro)
  angle = atand(slope)
  do i = 1, numSpots
    yy(i) = xcen(i) * sind(-angle) + ycen(i) * cosd(-angle)
  enddo
  write(6, 102) slope, angle
102 format(' slope =',f8.4,': to make level, add',f6.1, ' to total angle offset')
  call findshift(' rotated ', yy, thickMid, ifUse, numSpots, shiftAdd)
  thetaAdd = angle
  !
  if (doXtilt == 0. .or. numSpots <= 2) return
  !
  nd = 0
  do i = 1, numSpots
    if (ifUse(i) .ne. 0) then
      nd = nd + 1
      xx(nd) = xcen(i)
      zz(nd) = ycen(i)
      yy(nd) = ySamp(i)
    endif
  enddo
  call lsfit2(xx, yy, zz, nd, a, b, c)
  theta = -atand(a)
  cosTheta = cosd(theta)
  sinTheta = sind(theta)
  alpha = atand(b / (cosTheta - a * sinTheta))
  cosAplha = cosd(alpha)
  sinAlpha = sind(alpha)
  do i = 1, numSpots
    zp = xcen(i) * sinTheta + ycen(i) * cosTheta
    yy(i) = -ySamp(i) * sinAlpha + zp * cosAplha
  enddo
!!$    alpha=atand(b)
!!$    slope=a/(cosd(alpha) -b*sind(alpha))
!!$    theta=-atand(slope)
!!$    cosTheta=cosd(theta)
!!$    sinTheta=sind(theta)
!!$    cosAplha=cosd(alpha)
!!$    sinAlpha=sind(alpha)
!!$    do i=1, numSpots
!!$    zp=ycen(i)*cosAplha-ySamp(i)*sinAlpha
!!$    yy(i) =xcen(i)*sinTheta+zp*cosTheta
!!$    enddo
  write(6, 103) alpha, -theta
103 format(/' Pitch between samples can be corrected with ', &
      'an added X-axis tilt of',f7.2,/,' In this case, to make ', &
      'level, add',f6.1,' to total angle offset')
  thetaAdd = -theta
  call findshift('x-tilted ', yy, thickMid, ifUse, numSpots, shiftAdd)
  return
end subroutine analyzeSpots

subroutine findshift(rotLabel, ycen, thick, ifUse, numSpots, shift)
  implicit none
  character*(*) rotLabel
  real*4 ycen(*), thick(*), shiftAdd
  integer*4 ifUse(*), numSpots
  !
  real*4 bot, top, realThick, shift
  integer*4 i, ithick
  integer*4 niceFrame
  !
  bot = 1.e10
  top = -1.e10
  do i = 1, numSpots
    if (ifUse(i) .ne. 0) then
      bot = min(bot, ycen(i) - thick(i) / 2)
      top = max(top, ycen(i) + thick(i) / 2)
    endif
  enddo
  realThick = top - bot
  shift = -0.5 * (top + bot)
  ithick = 2 * (int(realThick / 2. +0.99))
  ithick = niceFrame(ithick, 2, 19)
  write(6, 101) rotLabel, shift, realThick, ithick
101 format(1x,a,' lines imply added Z shift of',f7.1,'; thickness of', &
      f6.1,', set to',i5)
  return
end subroutine findshift

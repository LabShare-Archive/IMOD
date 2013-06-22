! * * * * * * XYZPROJ * * * * * *
!
! This program will compute projections of a 3-dimensional block of an
! image file at a series of tilts around either the X, the Y or the Z
! axis.  The block may be any arbitrary subset of the image file.
!
! See man page for details.
!
! $Id$
!
program xyzproj
  implicit none
  integer LIMPIX, LIMSTACK, LIMPROJ, LIMRAY
  !
  ! 9/16/09: Increased stack size increases page faulting and CPU time
  parameter (LIMSTACK = 40000000, LIMPIX = 40000, &
      LIMPROJ = 1440, LIMRAY = 180 * 40000)
  real*4 array(LIMSTACK)
  character*320 inFile, outFile
  character*1 xyz
  integer*4 nxyzIn(3), mxyzIn(3), nxyzOut(3), nxyzst(3), mapScales(3,3)
  real*4 cell(6), title(20), delta(3)
  data nxyzst/0, 0, 0/, cell/0., 0., 0., 90., 90., 90./
  data mapScales/2, 1, 2, 1, 2, 1, 1, 3, 1/
  integer*4 nxIn, nyin, nzIn, nxout, nyOut, nzOut
  common / nxyz / nxIn, nyin, nzIn, nxout, nyOut, nzOut
  equivalence (nxyzIn(1), nxIn), (nxyzOut(1), nxout)
  integer*4 nrayInc(LIMRAY), nrayMax(0:LIMPROJ)
  real*4 xrayStr(LIMRAY), yrayStr(LIMRAY), pixtmp(LIMPIX)
  real*4 cosAng(0:LIMPROJ), sinAng(0:LIMPROJ), tiltAngles(LIMPROJ)
  integer*4 ixBoxLo(LIMPROJ), iyBoxLo(LIMPROJ), ixBoxHi(LIMPROJ)
  integer*4 ix0, ix1, iy0, iy1, iz0, iz1, modeIn, iyBoxHi(LIMPROJ)
  integer*4 nxBlock, nyBlock, nzBlock, idirz, nxSlice, nySlice, lenLoad
  integer*4 ixLoad0, ixLoad1, iyLoad0, iyLoad1, loadXlo, loadXhi
  integer*4 load0, loadDir, modeOut, ifRayScale, loadYlo, loadYhi
  real*4 dmin, dmax, dmean, tiltStart, tiltEnd, tiltInc, scaleAdd, scaleFac
  real*4 fill, dmin2, dmax2, dmean2, dsum, angle, projMiddle
  integer*4 istackDel, LIMSLICES, numLoads, ioutBase, iproj, ixOut, indAxis
  integer*4 iyOutStr, numSlices, load1, invertAng, nview
  integer*4 izSec, indStack, irayBase, nraypts, iray, ipix
  real*4 xray, yray, dx, dy, v2, v4, v5, v6, v8, vmin, vmax, a, b, c, d
  real*4 sumTmp, rayFac, rayAdd, tmin, tmax, tmean, tiltMax
  integer*4 ixr, iyr, ixy, ixy4, ixy6, ixy8, ixy2, indOut, kti, i, ind, iload, ierr
  integer*4 ixdir, ixoStart, ixoEnd
  logical*4 commonLine, fullImage
  real*4 sind, cosd
  ! real*8 readtime, readstart, readdone, walltime, transtime, projtime
  common / bigarr / array, xrayStr, yrayStr, nrayInc
  !
  ! 7/7/00 CER: remove the encode's; titlech is the temp space
  !
  character*80 titlech
  character dat * 9, tim * 8
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetTwoFloats, PipGetFloat, PipNumberOfEntries
  integer*4 PipGetString, PipGetTwoIntegers, PipGetThreeFloats
  integer*4 PipGetInOutFile, PipGetBoolean, PipGetLogical
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  xyzproj
  !
  integer numOptions
  parameter (numOptions = 18)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@'// &
      'axis:AxisToTiltAround:CH:@xminmax:XMinAndMax:IP:@'// &
      'yminmax:YMinAndMax:IP:@zminmax:ZMinAndMax:IP:@'// &
      'angles:StartEndIncAngle:FT:@mode:ModeToOutput:I:@'// &
      'width:WidthToOutput:I:@addmult:AddThenMultiply:FP:@'// &
      'fill:FillValue:F:@constant:ConstantScaling:B:@'// &
      'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'// &
      'tiltfile:TiltFile:FN:@tangles:TiltAngles:FAM:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  tiltStart = 0.
  tiltEnd = 0.
  tiltInc = 1.
  scaleAdd = 0.
  scaleFac = 1.
  ifRayScale = 1
  commonLine = .false.
  ! transtime = 0.
  ! readtime = 0.
  ! projtime = 0.
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'xyzproj', &
      'ERROR: XYZPROJ - ', .true., 3, 1, 1, numOptArg, &
      numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0

  if (PipGetInOutFile('InputFile', 1, 'Name of input file', inFile) &
      .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
  !
  call imopen(1, inFile, 'ro')
  call irdhdr(1, nxyzIn, mxyzIn, modeIn, dmin, dmax, dmean)
  !
  if (PipGetInOutFile('OutputFile', 2, 'Name of output file', outFile) &
      .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
  !
  ix0 = 0
  ix1 = nxIn - 1
  iy0 = 0
  iy1 = nyin - 1
  iz0 = 0
  iz1 = nzIn - 1
  if (pipInput) then
    ierr = PipGetTwoIntegers('XMinAndMax', ix0, ix1)
    ierr = PipGetTwoIntegers('YMinAndMax', iy0, iy1)
    ierr = PipGetTwoIntegers('ZMinAndMax', iz0, iz1)
  else
    write(*,'(1x,a,/,a,$)') &
        'Enter index coordinates of block: ix0,ix1,iy0,iy1,iz0,iz1' &
        , ' (or / for whole volume): '
    read(*,*) ix0, ix1, iy0, iy1, iz0, iz1
  endif
  ix0 = max(0, ix0)
  ix1 = min(ix1, nxIn - 1)
  iy0 = max(0, iy0)
  iy1 = min(iy1, nyin - 1)
  iz0 = max(0, min(iz0, nzIn - 1))
  iz1 = max(0, min(iz1, nzIn - 1))
  if (ix0 > ix1 .or. iy0 > iy1) call exitError('No volume specified')
  !
  if (pipInput) then
    ierr = PipNumberOfEntries('TiltAngles', ixLoad0)
    ierr = PipNumberOfEntries('TiltFile', ixLoad1)
    ierr = PipNumberOfEntries('FirstTiltAngle', iyLoad0)
    ierr = PipGetLogical('FullAreaAtTilt', fullImage)
    if (ixLoad0 + ixLoad1 + iyLoad0 > 0) then
      commonLine = .true.
      nview = nzIn
      call get_tilt_angles(nview, 3, tiltAngles, LIMPROJ, 1)
      if (nview .ne. nzIn) call exitError( &
          'THERE MUST BE A TILT ANGLE FOR EACH VIEW')
      xyz = 'Z'
    endif

    if (PipGetString('AxisToTiltAround', xyz) .ne. 0 .and. .not.commonLine) &
        call exitError( 'YOU MUST ENTER AN AXIS TO TILT AROUND')
    if (commonLine .and. xyz .ne. 'Z' .and. xyz .ne. 'z') call exitError( &
        'YOU CAN ENTER TILT ANGLES ONLY FOR PROJECTIONS AROUND THE Z AXIS')
    ierr = PipGetThreeFloats('StartEndIncAngle', tiltStart, tiltEnd, tiltInc)
  else
    write(*,'(1x,a,$)') 'Axis to project around (enter X, Y or Z): '
    read(*,'(a)') xyz
    !
    write(*,'(1x,a,$)') 'Starting, ending, increment tilt angles: '
    read(*,*) tiltStart, tiltEnd, tiltInc
  endif
  do while (tiltStart > 180.)
    tiltStart = tiltStart - 360.
  enddo
  do while (tiltStart <= -180.)
    tiltStart = tiltStart + 360.
  enddo
  do while (tiltEnd > 180.)
    tiltEnd = tiltEnd-360.
  enddo
  do while (tiltEnd <= -180.)
    tiltEnd = tiltEnd + 360.
  enddo
  if (tiltInc >= 0 .and. tiltEnd < tiltStart) tiltEnd = tiltEnd + 360.
  if (tiltInc < 0 .and. tiltEnd > tiltStart) tiltEnd = tiltEnd-360.
  nzOut = 1
  if (tiltInc .ne. 0.) nzOut = (tiltEnd - tiltStart) / tiltInc + 1
  if (nzOut > LIMPROJ) call exitError('TOO MANY PROJECTIONS FOR ARRAYS')
  !
  nxBlock = ix1 + 1 - ix0
  nyBlock = iy1 + 1 - iy0
  if (iz1 >= iz0) then
    idirz = 1
    nzBlock = iz1 + 1 - iz0
  else
    idirz = -1
    nzBlock = iz0 + 1 - iz1
  endif
  !
  ! set up size of slices within which to project sets of lines,
  ! total # of slices, and other parameters for the 3 cases
  !
  invertAng = 1
  if (xyz == 'x' .or. xyz == 'X') then          !tilt around X
    nxSlice = nzBlock
    nySlice = nyBlock
    nxout = nyBlock
    nyOut = nxBlock
    lenLoad = nyBlock
    load0 = ix0
    loadDir = 1
    indAxis = 1
  elseif (xyz == 'y' .or. xyz == 'Y') then      !tilt around Y
    nxSlice = nxBlock
    nySlice = nzBlock
    nxout = nxBlock
    nyOut = nyBlock
    lenLoad = nxBlock
    load0 = iy0
    loadDir = 1
    invertAng = -1
    indAxis = 2
  elseif (xyz == 'z' .or. xyz == 'Z') then      !tilt around Z
    nxSlice = nxBlock
    nySlice = nyBlock
    nxout = nxBlock
    nyOut = nzBlock
    lenLoad = 0
    load0 = iz0
    loadDir = idirz
    indAxis = 3
  else
    call exitError('YOU MUST ENTER ONE OF X, Y, Z, x, y, or z FOR AXIS')
  endif
  !
  modeOut = 1
  if (modeIn == 2) modeOut = 2
  fill = dmean
  ixLoad0 = ix0
  ixLoad1 = ix1
  iyLoad0 = iy0
  iyLoad1 = iy1
  if (commonLine) then
    nxout = 0
    ixLoad0 = nxIn
    ixLoad1 = 0
    iyLoad0 = nyin
    iyLoad1 = 0
    tiltMax = 0.;
    do izSec = iz0, iz1
      tiltMax = max(tiltMax, abs(tiltAngles(izSec + 1)))
    enddo
    projMiddle = invertAng * (tiltStart + nzOut * tiltInc / 2.)
    do iproj = 0, nzOut - 1
      angle = invertAng * (tiltStart + iproj * tiltInc)
      call commonLineBox(ix0, ix1, iy0, iy1, nxIn, nyin, angle, tiltMax, &
          projMiddle, ixBoxLo(iproj), iyBoxLo(iproj), ixBoxHi(iproj), &
          iyBoxHi(iproj), loadXlo, loadXhi, loadYlo, loadYhi, fullImage)
      nxout = max(nxout, ixBoxHi(iproj) + 1 - ixBoxLo(iproj))
      ixLoad0 = min(ixLoad0, loadXlo)
      ixLoad1 = max(ixLoad1, loadXhi)
      iyLoad0 = min(iyLoad0, loadYlo)
      iyLoad1 = max(iyLoad1, loadYhi)
    enddo
    nxSlice = ixLoad1 + 1 - ixLoad0
    nySlice = iyLoad1 + 1 - iyLoad0
  endif
  !
  if (pipInput) then
    ierr = PipGetInteger('WidthToOutput', nxout)
    ierr = PipGetInteger('ModeToOutput', modeOut)
    ierr = PipGetTwoFloats('AddThenMultiply', scaleAdd, scaleFac)
    ierr = PipGetFloat('FillValue', fill)
    ixr = 0
    ierr = PipGetBoolean('ConstantScaling', ixr)
    ifRayScale = 1 - ixr
  else
    write(*,'(1x,a,i5,a,$)') 'Width of output image [/ for', nxout, ']: '
    read(*,*) nxout
    !
    write(*,'(1x,a,i2,a,$)') 'Output data mode [/ ', modeOut, ']: '
    read(*,*) modeOut
    !
    ! write(*,'(1x,a,$)') '0 to scale by 1/(vertical thickness),'// &
    ! ' or 1 to scale by 1/(ray length): '
    ! read(*,*) ifRayScale
    !
    write(*,'(1x,a,$)') 'Additional scaling factors to add '// &
        'then multiply by [/ for 0,1]: '
    read(*,*) scaleAdd, scaleFac
    !
    write(*,'(1x,a,/,a,f10.2,a,$)') &
        'Value to fill parts of output not projected to', &
        '   (before scaling, if any) [/ for mean=', dmean, ']: '
    read(*,*) fill
  endif

  fill = (fill + scaleAdd) * scaleFac
  !
  ! set up output file and header
  !
  !
  call imopen(2, outFile, 'new')
  call itrhdr(2, 1)
  call ialmod(2, modeOut)
  call ialsiz(2, nxyzOut, nxyzst)
  call ialsam(2, nxyzOut)
  call irtdel(1, delta)
  do i = 1, 3
    cell(i) = nxyzOut(i) * delta(mapScales(i, indAxis))
  enddo
  call ialCel(2, cell)
  call time(tim)
  call date(dat)
  !
  write(titlech, 301) ix0, ix1, iy0, iy1, iz0, iz1, xyz, dat, tim
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
301 format('XYZPROJ: x',2i5,', y',2i5,', z ',2i5,' about ',a1,t57,a9,2X,a8)
  dmax2 = -1.e20
  dmin2 = 1.e20
  dsum = 0.
  !
  ! set up stack loading with slices
  !
  istackDel = nxSlice * nySlice
  LIMSLICES = min(LIMPIX, LIMSTACK / (istackDel + max(nxout, lenLoad)))
  if (commonLine) LIMSLICES = 1
  if (LIMSLICES < 1) call exitError('IMAGES TOO LARGE FOR STACK')
  if (nxout * nzOut > LIMRAY) call exitError( &
      'TOO MANY PROJECTIONS FOR OUTPUT THIS WIDE')
  numLoads = (nyOut + LIMSLICES - 1) / LIMSLICES
  ioutBase = 1 + LIMSLICES * istackDel
  !
  ! analyse the number of points on each ray in each projection
  !
  do iproj = 0, nzOut - 1
    angle = invertAng * (tiltStart + iproj * tiltInc)
    sinAng(iproj) = sind(angle)
    cosAng(iproj) = cosd(angle)
    irayBase = iproj * nxout
    if (.not.commonLine) call set_projection_rays(sinAng(iproj), &
        cosAng(iproj), nxSlice, nySlice, nxout, xrayStr(irayBase + 1), &
        yrayStr(irayBase + 1), nrayInc(irayBase + 1), nrayMax(iproj))
  enddo
  !
  ! loop on loads of several to many slices at once
  !
  iyOutStr = 0                                !starting Y output line
  do iload = 1, numLoads
    numSlices = min(LIMSLICES, nyOut - iyOutStr)
    load1 = load0 + loadDir * (numSlices - 1)
    !
    ! load the slices one of 3 different ways: for X, get vertical
    ! rectangles of sections in input file, transpose into slice array
    !
    if (xyz == 'x' .or. xyz == 'X') then
      do izSec = iz0, iz1, idirz
        call imposn(1, izSec, 0)
        call irdpas(1, array(ioutBase), numSlices, nyBlock, load0, &
            load1, iy0, iy1,*99)
        call xtransp(array(ioutBase), idirz * (izSec - iz0) + 1, &
            array, nxSlice, nySlice, numSlices)
      enddo
      !
      ! for Y, get horizontal rectangles of sections in input file,
      ! transpose differently into slice array
      !
    elseif (xyz == 'y' .or. xyz == 'Y') then
      do izSec = iz0, iz1, idirz
        ! readstart = walltime()
        call imposn(1, izSec, 0)
        call irdpas(1, array(ioutBase), nxBlock, numSlices, ix0, ix1, &
            load0, load1,*99)
        ! readdone = walltime()
        ! readtime = readtime + readdone - readstart
        call ytransp(array(ioutBase), idirz * (izSec - iz0) + 1, &
            array, nxSlice, nySlice, numSlices)
        ! transtime =  transtime +  walltime() - readdone
      enddo
      !
      ! for Z, just get slices directly from sections in file
      !
    else
      indStack = 1
      do izSec = load0, load1, loadDir
        call imposn(1, izSec, 0)
        call irdpas(1, array(indStack), nxSlice, nySlice, ixLoad0, ixLoad1, &
            iyLoad0, iyLoad1,*99)
        indStack = indStack + istackDel
      enddo
    endif
    load0 = load0 + loadDir * numSlices
    !
    ! loop on different projection views
    !
    ! readdone = walltime()
    do iproj = 0, nzOut - 1
      irayBase = iproj * nxout
      if (commonLine) then
        angle = invertAng * (tiltStart + iproj * tiltInc)
        call commonLineRays(ix0, ix1, iy0, iy1, nxIn, nyin, ixLoad0, &
            iyLoad0, nxout, angle, tiltAngles(load0 + 1), ixBoxLo(iproj), &
            iyBoxLo(iproj), ixBoxHi(iproj), iyBoxHi(iproj), &
            xrayStr(irayBase + 1), yrayStr(irayBase + 1), nrayInc(irayBase + 1), &
            nxSlice, nySlice, fullImage)
        nrayMax(iproj) = nrayInc(irayBase + 1)
      endif
      !
      ! Set X-independent part of scaling
      if (ifRayScale == 0) then
        rayFac = scaleFac / nySlice
      else
        rayFac = scaleFac / nrayMax(iproj)
      endif
      !
      ! set the output lines for this view to the fill value
      rayAdd = fill
      if (ifRayScale == 0) rayAdd = (fill * nrayMax(iproj)) / nySlice
      do ind = ioutBase, ioutBase + numSlices * nxout - 1
        array(ind) = rayAdd
      enddo
      !
      ! for projections at an angle, process multiple slices at once
      if (sinAng(iproj) .ne. 0.) then
        !
        ! loop on pixels along line
        do ixOut = 1, nxout
          nraypts = nrayInc(ixOut + irayBase)
          ! print *,nraypts
          if (nraypts > 0) then
            !
            ! if block along ray, clear temporary array for output pixels
            do ipix = 1, numSlices
              pixtmp(ipix) = 0.
            enddo
            !
            ! move along ray, computing at each point indexes and factors
            ! for quadratic interpolation
            do iray = 0, nraypts - 1
              xray = xrayStr(ixOut + irayBase) + iray * sinAng(iproj)
              yray = yrayStr(ixOut + irayBase) + iray * cosAng(iproj)
              ixr = nint(xray)
              iyr = nint(yray)
              dx = xray - ixr
              dy = yray - iyr
              ixy = ixr + (iyr - 1) * nxSlice
              ixy4 = ixy - 1
              ixy6 = ixy + 1
              ixy8 = ixy + nxSlice
              ixy2 = ixy - nxSlice
              !
              ! loop through pixels in different slices, do quadratic
              ! interpolation limited by values of surrounding pixels
              do ipix = 1, numSlices
                v2 = array(ixy2)
                v4 = array(ixy4)
                v5 = array(ixy)
                v6 = array(ixy6)
                v8 = array(ixy8)
                vmax = max(v2, v4, v5, v6, v8)
                vmin = min(v2, v4, v5, v6, v8)
                !
                a = (v6 + v4) * .5 - v5
                b = (v8 + v2) * .5 - v5
                c = (v6 - v4) * .5
                d = (v8 - v2) * .5
                !
                pixtmp(ipix) = pixtmp(ipix) + max(vmin, min(vmax, &
                    (a * dx * dx + b * dy * dy + c * dx + d * dy + v5)))
                !
                ixy = ixy + istackDel           !increment subscripts for
                ixy2 = ixy2 + istackDel         !next slice
                ixy4 = ixy4 + istackDel
                ixy6 = ixy6 + istackDel
                ixy8 = ixy8 + istackDel
              enddo
            enddo
            !
            ! set up scaling and put pixels out in different lines
            rayAdd = scaleAdd * scaleFac + rayFac * (nrayMax(iproj) - nraypts) * &
                (fill / scaleFac - scaleAdd)
            !
            indOut = ixOut + ioutBase-1
            do ipix = 1, numSlices
              array(indOut) = rayFac * pixtmp(ipix) + rayAdd
              indOut = indOut + nxout
            enddo
          endif
        enddo

      else
        !
        ! simple case of straight projection
        ! find limits of X to loop on
        ixoStart = 0
        do ixOut = 1, nxout
          if (ixoStart == 0 .and. nrayInc(ixOut + irayBase) > 0) &
              ixoStart = ixOut
          if (nrayInc(ixOut + irayBase) > 0) ixoEnd = ixOut
        enddo
        !
        ! Get starting positions in X, Y, and directions
        ixdir = 1
        ixr = nint(xrayStr(ixoStart + irayBase))
        if (xrayStr(ixoEnd + irayBase) < ixr) ixdir = -1
        iyr = nint(yrayStr(ixoStart + irayBase))
        ixy8 = sign(1., cosAng(iproj))
        nraypts = nrayInc(ixoStart + irayBase)
        rayAdd = scaleAdd * scaleFac + rayFac * (nrayMax(iproj) - nraypts) * &
            (fill / scaleFac - scaleAdd)
        !
        ! loop on slices; clear the array segment for the slice
        do ipix = 1, numSlices
          indOut = nxout * (ipix - 1) + ioutBase - 1
          do ixOut = ixoStart, ixoEnd
            array(indOut + ixOut) = 0.
          enddo

          ! Loop on levels in Y (the ray points) and set starting index
          do iray = 0, nraypts - 1
            ixy = ixr + (iyr - 1) * nxSlice + iray * nxSlice * ixy8 + &
                (ipix - 1) * istackDel
            !
            ! Add line into array
            do ixOut = ixoStart, ixoEnd
              array(indOut + ixOut) = array(indOut + ixOut) + array(ixy)
              ixy = ixy + ixdir
            enddo
          enddo
          !
          ! Scale the data
          do ixOut = ixoStart, ixoEnd
            array(indOut + ixOut) = array(indOut + ixOut) * rayFac + rayAdd
          enddo
        enddo
      endif
      !
      ! get min, max, mean; output the lines to proper section
      !
      call iclden(array(ioutBase), nxout, numSlices, 1, nxout, 1, &
          numSlices, tmin, tmax, tmean)
      dmin2 = (min(dmin2, tmin))
      dmax2 = (max(dmax2, tmax))
      dsum = dsum + tmean * numSlices * nxout
      call imposn(2, iproj, iyOutStr)
      call iwrsecl(2, array(ioutBase), numSlices)
    enddo
    iyOutStr = iyOutStr + numSlices
    ! projtime = projtime + walltime() - readdone
  enddo
  !
  ! finish up file header
  !
  dmean2 = dsum / (nxout * nyOut * nzOut)
  call iwrhdr(2, title, 1, dmin2, dmax2, dmean2)
  call imclose(2)
  call imclose (1)
  ! print *,'read time', readtime, '  transpose time', transtime, '
  ! project time', projtime
  call exit(0)
99 call exitError('READING FILE')
end program xyzproj
!
!
! Subroutines to transpose the pieces of sections read from input file
! into the array of slices
!
subroutine xtransp(array, icolm, brray, nxsl, nysl, nsl)
  real*4 array(nsl,nysl), brray(nxsl,nysl,nsl)
  do iy = 1, nysl
    do ix = 1, nsl
      brray(icolm, iy, ix) = array(ix, iy)
    enddo
  enddo
  return
end subroutine xtransp

subroutine ytransp(array, irow, brray, nxsl, nysl, nsl)
  real*4 array(nxsl,nsl), brray(nxsl,nysl,nsl)
  do iy = 1, nsl
    do ix = 1, nxsl
      brray(ix, irow, iy) = array(ix, iy)
    enddo
  enddo
  return
end subroutine ytransp

! Compute the needed box for common line projections that fits within
! the given area and within the image at the given projection angle
! and when tilt-foreshortened at the maximum tilt angle.  return the
! coordinates defining the limits of the projection box in rotated
! space, and the coordinates that need loading
!
subroutine commonLineBox(ix0, ix1, iy0, iy1, nxIn, nyin, projAng, &
    tiltMax, projMiddle, ixlo, iylo, ixhi, iyhi, loadXlo, loadXhi, &
    loadYlo, loadYhi, fullImage)
  implicit none
  integer*4 ix0, ix1, iy0, iy1, nxIn, nyin
  integer*4 ixlo, iylo, ixhi, iyhi, loadXlo, loadXhi, loadYlo, loadYhi
  logical*4 fullImage
  real*4 projAng, tiltMax, projMiddle
  !
  real*4 cosRot, sinRot, xcen, ycen, x0, y0, x1, y1, xstep, ystep
  real*4 cosAng, sinAng
  integer*4 maxSteps, istep
  real*4 xll, xul, xlr, xur, yll, yul, ylr, yur, rotAng, axisy, yloTFS, yhiTFS
  real*4 xllr, xulRot, xlrRot, xurRot, yllRot, yulRot, ylrRot, yurRot
  real*4 xllTFS, xulTFS, xlrTFS, xurTFS, yllTFS, yulTFS, ylrTFS, yurt
  real*4 cosd, sind
  !
  ! Determine rotation of area so that it is within 45 deg, so that the
  ! box that is projected is oriented as closely as possible to the
  ! specified box at the middle angle
  rotAng = projAng
  cosAng = projMiddle
  do while (abs(cosAng) > 45.01)
    rotAng = rotAng - sign(90., cosAng)
    cosAng = cosAng - sign(90., cosAng)
  enddo
  ! print *,'Proj angle', projAng, '  Reduced angle', rotAng
  !
  ! Incoming and outgoing coordinates are all numbered from 0
  cosRot = cosd(rotAng)
  sinRot = -sind(rotAng)
  cosAng = cosd(projAng)
  sinAng = sind(projAng)
  xcen = (ix1 + ix0) / 2.
  ycen = (iy1 + iy0) / 2.
  !
  ! Set up to loop on area, stepping size down symmetrically until rotated
  ! box fits within the image
  x0 = ix0
  y0 = iy0
  x1 = ix1
  y1 = iy1
  maxSteps = max(ix1 + 1 - ix0, iy1 + 1 - iy0) / 2
  xstep = (ix1 + 1 - ix0) / maxSteps
  ystep = (iy1 + 1 - iy0) / maxSteps
  do istep = 1, maxSteps
    !
    ! Rotate the box by negative of angle and see if it fits
    call rotatePoint(cosRot, sinRot, x0, y0, xll, yll)
    call rotatePoint(cosRot, sinRot, x0, y1, xul, yul)
    call rotatePoint(cosRot, sinRot, x1, y0, xlr, ylr)
    call rotatePoint(cosRot, sinRot, x1, y1, xur, yur)
    !
    ! Back-rotate these boundaries by the original, unconstrained angle
    ! and get the coordinates defining limits of projection box
    call rotatePoint(cosAng, sinAng, xll, yll, xllr, yllRot)
    call rotatePoint(cosAng, sinAng, xul, yul, xulRot, yulRot)
    call rotatePoint(cosAng, sinAng, xlr, ylr, xlrRot, ylrRot)
    call rotatePoint(cosAng, sinAng, xur, yur, xurRot, yurRot)
    ixlo = ceiling(min(xllr, xulRot, xlrRot, xurRot))
    ixhi = floor(max(xllr, xulRot, xlrRot, xurRot))
    iylo = ceiling(min(yllRot, yulRot, ylrRot, yurRot))
    iyhi = floor(max(yllRot, yulRot, ylrRot, yurRot))
    !
    ! tilt-foreshorten the Y limits of projection box and recompute corners
    axisy = -(nxIn / 2. - xcen) * sinAng + (nyin / 2. - ycen) * cosAng + &
        ycen
    if (fullImage) then
      yloTFS = iylo
      yhiTFS = iyhi
    else
      yloTFS = nint((iylo - axisy) * cosd(tiltMax) + axisy)
      yhiTFS = nint((iyhi - axisy) * cosd(tiltMax) + axisy)
    endif
    call rotatePoint(cosAng, -sinAng, float(ixlo), yloTFS, xllTFS, yllTFS)
    call rotatePoint(cosAng, -sinAng, float(ixlo), yhiTFS, xulTFS, yulTFS)
    call rotatePoint(cosAng, -sinAng, float(ixhi), yloTFS, xlrTFS, ylrTFS)
    call rotatePoint(cosAng, -sinAng, float(ixhi), yhiTFS, xurTFS, yurt)
    !
    ! Get limits of the box needing loading - allow for quadratic
    ! interpolation with the margins here, no extra margin is needed
    loadXlo = floor(min(xll, xul, xlr, xur, xllTFS, xulTFS, xlrTFS, xurTFS)) - 2
    loadXhi = ceiling(max(xll, xul, xlr, xur, xllTFS, xulTFS, xlrTFS, xurTFS)) + 2
    loadYlo = floor(min(yll, yul, ylr, yur, yllTFS, yulTFS, ylrTFS, yurt)) - 2
    loadYhi = ceiling(max(yll, yul, ylr, yur, yllTFS, yulTFS, ylrTFS, yurt)) + 2
    !
    ! Test and see if this fits; if not reduce box
    if (loadXlo >= 0 .and. loadXhi < nxIn .and. &
        loadYlo >= 0 .and. loadYhi < nyin) exit
    x0 = x0 + xstep
    x1 = x1 - xstep
    y0 = y0 + ystep
    y1 = y1 - ystep
    if (istep > maxSteps - 10) call exitError( &
        'CANNOT FIND ROTATED BOX THAT FITS WITHIN IMAGE')
  enddo
  ! print *,istep, ' steps, new box:', x0, x1, y0, y1
  ! print *,'Projection box limits:', ixlo, ixhi, iylo, iyhi
  ! print *,'load limits:', loadXlo, loadXhi, loadYlo, loadYhi
  return

contains
  subroutine rotatePoint(cosa, sina, xin, yin, xout, yout)
    implicit none
    real*4 xin, yin, xout, yout, cosa, sina
    xout = (xin - xcen) * cosa - (yin - ycen) * sina + xcen
    yout = (xin - xcen) * sina + (yin - ycen) * cosa + ycen
  end subroutine rotatePoint

end subroutine commonLineBox


! Compute the ray parameters for a common line projection given the
! original coordinate limits, the start of loaded data, and coordinates
! defining the projection box computed before
!
subroutine commonLineRays(ix0, ix1, iy0, iy1, nxIn, nyin, loadXst, &
    loadYst, nxout, projAng, tilt, inIxlo, iylo, inIxhi, iyhi, xrayStr, &
    yrayStr, nrayInc, nxSlice, nySlice, fullImage)
  implicit none
  integer*4 ix0, ix1, iy0, iy1, nxIn, nyin, loadXst, loadYst, nxout
  integer*4 inIxlo, iylo, inIxhi, iyhi, nrayInc(*)
  real*4 projAng, tilt, xrayStr(*), yrayStr(*)
  integer*4 nxSlice, nySlice
  logical*4 fullImage
  !
  integer*4 iyloTFS, iyhiTFS, ix, i, ixlo, ixhi
  real*4 cosRot, sinRot, xcen, ycen, axisy, xend, yend
  real*4 cosd, sind
  !
  cosRot = cosd(projAng)
  sinRot = -sind(projAng)
  xcen = (ix1 + ix0) / 2.
  ycen = (iy1 + iy0) / 2.
  !
  ! Trim the X limits if they are bigger than nxout
  ixlo = inIxlo
  ixhi = inIxhi
  if (ixhi + 1 - ixlo > nxout) then
    ix = ixhi + 1 - ixlo - nxout
    ixlo = ixlo + ix / 2
    ixhi = ixhi - (ix - ix / 2)
  endif
  !
  ! Rotate the center of the image to determine the Y value of tilt axis
  ! then tilt-foreshorten the lower and upper Y limits
  axisy = (nxIn / 2. - xcen) * sinRot + (nyin / 2. - ycen) * cosRot + ycen
  if (fullImage) then
    iyloTFS = iylo
    iyhiTFS = iyhi
  else
    iyloTFS = nint((iylo - axisy) * cosd(tilt) + axisy)
    iyhiTFS = nint((iyhi - axisy) * cosd(tilt) + axisy)
  endif
  !
  ! Rotate the bottom line of this box to get ray starts, adjust by load
  ! Also, incoming coordinates are numbered from 0 but we need array index
  ! coordinates numbered from 1, so add 1 at this stage
  do ix = ixlo, ixhi
    i = ix + 1 - ixlo
    xrayStr(i) = (ix - xcen) * cosRot - (iyloTFS - ycen) * sinRot + xcen + &
        1. - loadXst
    yrayStr(i) = (ix - xcen) * sinRot + (iyloTFS - ycen) * cosRot + ycen + &
        1. - loadYst
    nrayInc(i) = iyhiTFS + 1 - iyloTFS
!!$        xend = xrayStr(i) - (nrayInc(i) -1)*sinRot
!!$        yend = yrayStr(i) + (nrayInc(i) -1)*cosRot
!!$        if (xrayStr(i) < 1.5 .or. yrayStr(i) < 1.5 .or. xrayStr(i) > &
!!$            nxSlice - 0.5 .or.yrayStr(i) > nySlice - 0.5) print *,'OOPS', &
!!$            projAng, tilt, axisy, ix, xrayStr(i), yrayStr(i), iylo, iyloTFS, &
!!$            iyhi, iyhiTFS
!!$        if (xend < 1.5 .or. yend < 1.5 .or. xend > &
!!$            nxSlice - 0.5 .or.yend > nySlice - 0.5) print *,'OOPSend', &
!!$            projAng, tilt, axisy, ix, xend, yend, iylo, iyloTFS, iyhi, iyhiTFS
  enddo
  do i = ixhi + 1 - ixlo, nxout
    nrayInc(i) = 0
  enddo
  return
end subroutine commonLineRays

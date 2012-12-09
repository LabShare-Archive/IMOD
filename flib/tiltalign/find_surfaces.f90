! !
! Find_surfaces will analyze a set of [numRealPt] points, with coordinates
! in [xyz] (dimensioned to (3,*)), correlate the Z with the X and Y
! coordinates of those points, and determine the angles that the points
! would have to be tilted first around the X axis then around the Y axis
! in order for them to lie parallel to the X - Y plane.   If [numSurface] is
! 1, it will simply fit a plane to all of the points and base its
! estimates on that fit.  If [numSurface] is 2, it will attempt to divide
! the points into two groups occupying two surfaces, then provide
! separate estimates of the new tilt angle based on the slope of the
! plane through either set of points, or on the average slope.  The
! array [igroup] is returned with a value for each point of 1 if on lower
! surface, 2 if on upper, but only if 2 - surface analysis is done.  All
! outputs are printed for units 6 through [iunit], so set [iunit] to 6
! for output to standard out only, to 7 for output to a file on unit 7,
! or < 6 for no output.  If [ifComp] is non - zero, it assumes data were
! obtained at a single tilt angle [tiltMax] and at zero tilt and will
! estimate the true tilt angle of the section, returned in [tiltNew].
! [tiltAdd] should be set to an existing change in tilt angles so that
! the total tilt angle change can be output.  The values in [znew] and [znewInput], the
! actual and input amounts to shift the tilt axis in Z, and in [imageBinned] allow it to
! report on the unbinned thickness between fiducials and shift needed to
! center them.
! !
! $Id$
!
subroutine find_surfaces(xyz, numRealPt, numSurface, tiltMax, &
    iunit2, tiltNew, igroup, ifComp, tiltAdd, znew, znewInput, imageBinned)
  implicit none
  real*4 xyz(3,*), tiltMax, tiltNew, tiltAdd, bintcpMinus, bintcp, znew, znewInput
  integer*4 numRealPt, numSurface, iunit2, ifComp, imageBinned, maxReal
  real*4, allocatable :: xx(:), yy(:), zz(:), zrot(:)
  integer*4, allocatable :: icheck(:)
  integer*4 igroup(*)
  integer*4 i, iun, numIter, iter, ipt, numPntMinus, numPntPlus, iterLim, limCheck
  real*4 aSlope, bSlope, alpha, slope, resid, truePlus, slopeAvg, alphaAvg, theta
  real*4 sinTheta, cosAlpha, sinAlpha, zmin, zmax, zp, zmiddle, aslopeMinus
  real*4 slopeMinus, residMinus, err, resMax, res, errNew, thick, trueMinus
  real*4 shiftInc, shiftTot, botExtreme, topExtreme, cosTheta, bslopeMinus, alphaMinus
  integer*4 iptMax, ifCheck, numCheck
  logical changed

  integer*4 surfaceSort
  real*4 cosd, sind, atand
  !
  ! first fit a line to all of the points to get starting angle
  !
  maxReal = numRealPt + 10
  allocate( xx(maxReal), yy(maxReal), zz(maxReal), zrot(maxReal), &
      icheck(maxReal / 2), stat = iter)
  if (iter .ne. 0) then
    write(*,'(/,a)') 'ERROR: find_surfaces - failure to allocate memory for arrays'
    call exit(1)
  endif
  do i = 1, numRealPt
    xx(i) = xyz(1, i)
    yy(i) = xyz(2, i)
    zz(i) = xyz(3, i)
    igroup(i) = 0
  enddo
  call lsfit2Resid(xx, yy, zz, numRealPt, aSlope, bSlope, bintcp, alpha, slope, &
      resid, botExtreme, topExtreme)

  do iun = 6, iunit2
    write(iun, '(/,a,//,a,//,a,/,a,f8.4)') ' SURFACE ANALYSIS:', &
        ' The following parameters are appropriate if fiducials' &
        //' are all on one surface', &
        ' Fit of one plane to all fiducials:', &
        ' Adjusted slope =', slope
  enddo
  !
  ! if there are supposed to be 2 surfaces, enter an iterative procedure
  ! that rotates points based on the current average slope (initially
  ! the slope of the fit to all points, later the average of slopes for
  ! the top and bottom surfaces), finds the midpoint of the range of
  ! rotated Z values, assigns points to the lower or upper surface based
  ! on this midpoint, and fits lines to lower and upper surface points.
  !
  if (numSurface > 1) then
    call calcTiltNew(slope, tiltMax, iunit2, truePlus, ifComp, tiltAdd)
    do iun = 6, iunit2
      write(iun, '(a,/,a,/)') ' The following '// &
          'parameters are provided to indicate the consistency', &
          ' of the fit when fiducials are on two surfaces'
    enddo

    if (surfaceSort(xyz, numRealPt, 0, igroup) .ne. 0) then
      write(*,'(/,a)') 'ERROR: find_surfaces - Allocating memory in surfaceSort'
      call exit(1)
    endif

    call twoSurfaceFits(xyz, igroup, numRealPt, xx, yy, zz, numPntMinus, aslopeMinus, &
        bslopeMinus, bintcpMinus, alphaMinus, slopeMinus, residMinus, numPntPlus, &
        aSlope, bSlope, bintcp, alpha, slope, resid, slopeAvg, alphaAvg, botExtreme, &
        topExtreme)

    ! write(*,101) numPntMinus, bintcpMinus, slopeMinus, romi
    ! write(*,101) numPntPlus, bintcp, slope, ro
    !
    call twoSurfaceFits(xyz, igroup, numRealPt, xx, yy, zz, numPntMinus, aslopeMinus, &
        bslopeMinus, bintcpMinus, alphaMinus, slopeMinus, residMinus, numPntPlus, &
        aSlope, bSlope, bintcp, alpha, slope, resid, slopeAvg, alphaAvg, botExtreme, &
        topExtreme)
    !
    if (numPntMinus > 1) then
      do iun = 6, iunit2
        write(iun, 101) 'bottom', numPntMinus, bintcpMinus, slopeMinus, residMinus, &
            -alphaMinus
101     format(' Fit of one plane to points on ',a,' surface:',/, &
            ' # of points = ',i6,/,' Z axis intercept =',f8.2,/, &
            ' Adjusted slope =' ,f10.4,/,' Mean residual =',f11.3,/, &
            ' X axis tilt needed =',f6.2)
      enddo
      call calcTiltNew(slopeMinus, tiltMax, iunit2, trueMinus, ifComp, tiltAdd)
    endif
  else
    numPntPlus = numRealPt
  endif
  if (numPntPlus > 1) then
    do iun = 6, iunit2
      write(iun, 101) 'top', numPntPlus, bintcp, slope, resid, -alpha
    enddo
    call calcTiltNew(slope, tiltMax, iunit2, truePlus, ifComp, tiltAdd)
  endif
  if (numSurface == 1) then
    tiltNew = truePlus
  else
    !
    ! but just return distance between intercepts
    !
    thick = bintcp - bintcpMinus
    do iun = 6, iunit2
      print *,'The following parameters combine the last two '// &
          'results and are', 'appropriate if fiducials are on'// &
          ' two surfaces'
      write(iun, 102) thick, slopeAvg, -alphaAvg
102   format(/,' Thickness at Z intercepts =',f11.2,/, &
          ' Average adjusted slope =',f14.4,/, &
          ' Average X axis tilt needed =',f10.2)
    enddo
    call calcTiltNew(slopeAvg, tiltMax, iunit2, tiltNew, ifComp, tiltAdd)
  endif
  !
  ! Get the unbinned thickness and shifts needed to center the gold
  ! The direction is opposite to expected because the positive Z points
  ! come out on the bottom of the tomogram, presumably due to rotation
  thick = imageBinned * (topExtreme - botExtreme)
  shiftTot = imageBinned * (topExtreme + botExtreme) / 2.
  shiftInc = shiftTot - imageBinned * znew
  shiftTot = shiftInc + imageBinned * znewInput
  do iun = 6, iunit2
    write(iun, 103) thick, shiftInc, shiftTot
103 format(' Unbinned thickness needed to contain centers of all ', &
        'fiducials =', f13.0,/, &
        ' Incremental unbinned shift needed to center range of fi', &
        'ducials in Z =',f8.1,/, &
        ' Total unbinned shift needed to center range of fiducial', &
        's in Z =',f14.1)
  enddo
  deallocate( xx, yy, zz, zrot, icheck, stat = iter)
  return
end subroutine find_surfaces


subroutine twoSurfaceFits(xyz, igroup, numRealPt, xx, yy, zz, numPntMinus, aslopeMinus, &
    bslopeMinus, bintcpMinus, alphaMinus, slopeMinus, residMinus, numPntPlus, aSlope, &
    bSlope, bintcp, alpha, slope, resid, slopeAvg, alphaAvg, botExtreme, topExtreme)
  implicit none
  real*4 xyz(3,*), xx(*), yy(*), zz(*), aslopeMinus, bslopeMinus, bintcpMinus, alphaMinus
  real*4 residMinus, aSlope, bSlope, bintcp, alpha, slope, resid, slopeAvg, alphaAvg
  real*4 devMin, devMax, botExtreme, topExtreme, slopeMinus
  integer*4 igroup(*), numPntMinus, numRealPt, numPntPlus
  integer*4 ipt
  real*4 xxMinus, yyMinus, zzMinus
  !
  ! first fit a plane to points in the first group
  !
  numPntMinus = 0
  do ipt = 1, numRealPt
    if (igroup(ipt) == 1) then
      numPntMinus = numPntMinus + 1
      xx(numPntMinus) = xyz(1, ipt)
      yy(numPntMinus) = xyz(2, ipt)
      zz(numPntMinus) = xyz(3, ipt)
    endif
  enddo
  if (numPntMinus > 1) then
    call lsfit2Resid(xx, yy, zz, numPntMinus, aslopeMinus, bslopeMinus, bintcpMinus, &
        alphaMinus, slopeMinus, residMinus, devMin, devMax)
    botExtreme = bintcpMinus + devMin
  else
    xxMinus = xx(1)
    yyMinus = yy(1)
    zzMinus = zz(1)
  endif
  !
  ! next fit a plane to points in the second group
  !
  numPntPlus = 0
  do ipt = 1, numRealPt
    if (igroup(ipt) == 2) then
      numPntPlus = numPntPlus + 1
      xx(numPntPlus) = xyz(1, ipt)
      yy(numPntPlus) = xyz(2, ipt)
      zz(numPntPlus) = xyz(3, ipt)
    endif
  enddo
  if (numPntPlus > 1) then
    call lsfit2Resid(xx, yy, zz, numPntPlus, aSlope, &
        bSlope, bintcp, alpha, slope, resid, devMin, devMax)
    topExtreme = bintcp + devMax
  endif
  !
  ! get slope and intercept of each line, even if only 1 point
  !
  if (numPntMinus == 1) then
    slopeMinus = slope
    bintcpMinus = zzMinus - yyMinus * bSlope - xxMinus * aSlope
    alphaMinus = alpha
    botExtreme = bintcpMinus
  endif
  if (numPntPlus == 1) then
    slope = slopeMinus
    bintcp = zz(1) - yy(1) * bslopeMinus - xx(1) * aslopeMinus
    alpha = alphaMinus
    topExtreme = bintcp
  endif
  slopeAvg = (numPntPlus * slope + numPntMinus * slopeMinus) / numRealPt
  alphaAvg = (numPntPlus * alpha + numPntMinus * alphaMinus) / numRealPt
  return
end subroutine twoSurfaceFits

! DNM 5 / 3 / 02: give the new maximum tilt angle only for compression
! solutions
!
subroutine calcTiltNew(slopeMinus, tiltMax, iunit2, trueMinus, ifComp, &
    tiltAdd)
  implicit none
  real*4 slopeMinus, tiltMax, trueMinus, tiltAdd
  integer*4 iunit2, ifComp
  real*4 cosNew, globalDelta
  integer*4 iun
  real*4 atand, acosd, cosd, sind

  cosNew = slopeMinus * sind(tiltMax) + cosd(tiltMax)
  globalDelta = atand(-slopeMinus)
  do iun = 6, iunit2
    write(iun, 102) globalDelta, globalDelta + tiltAdd
102 format(' Incremental tilt angle change =',f7.2,/, &
        ' Total tilt angle change =',f13.2)
    if (ifComp .ne. 0) then
      if (abs(cosNew) <= 1.) then
        trueMinus = sign(acosd(cosNew), tiltMax)
        write(iun, 103) tiltMax, trueMinus
103     format('     or, change a fixed maximum tilt angle from' &
            ,f8.2,' to',f8.2,/)
      else
        write(iun, '(a,/)') '      or. . . But cannot derive '// &
            'implied tilt angle: |cos| > 1'
      endif
    else
      write(iun,*)
    endif
  enddo
  return
end subroutine calcTiltNew


subroutine lsfit2Resid(x, y, z, n, a, b, c, alpha, slope, resid, devMin, devMax)
  implicit none
  real*4 x(*), y(*), z(*), a, b, c, alpha, slope, resid, ro, devMin, devMax, dev
  integer*4 n, i
  real*4 sind, cosd, atand

  if (n > 2) then
    call lsfit2(x, y, z, n, a, b, c)
  elseif (n > 1) then
    b = 0.
    call lsfit(x, z, n, a, c, ro)
  else
    a = 0.
    b = 0.
    c = z(1)
  endif
  resid = 0.
  devMin = 1.e10
  devMax = -1.e10
  do i = 1, n
    dev = z(i) - (x(i) * a + y(i) * b + c)
    resid = resid + abs(dev)
    devMin = min(devMin, dev)
    devMax = max(devMax, dev)
  enddo
  resid = resid / n
  alpha = atand(b)
  slope = a / (cosd(alpha) - b * sind(alpha))
  return
end subroutine lsfit2Resid

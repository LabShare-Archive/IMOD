! * * * * * * REDUCECONT * * * * * * *
!
! This program will reduce the number of points in model contours to
! the minimum consistent with a defined maximum change in the model,
! the tolerance value.  Each of the original points in the model will
! be within the tolerance distance of the line segments connecting the
! final, reduced set of points.  Some smoothing is also done, and a
! point will be replaced by a smoothed point if the smoothed point is
! within the tolerance distance from the original point.
!
! See man page for more details.
!
! $Id$
!
program reducecont
  implicit none
  include 'model.inc90'
  !
  real*4, allocatable :: xt(:), yt(:), xx(:), yy(:)
  integer*4, allocatable :: idum(:), jdum(:), iobjDo(:), iflags(:)
  !
  character*320 inFile, outFile
  integer*4 numFit, iorder, ierr, numObjDo, numObjTot, iflag, indy, indz, ifFlipped
  real*4 tol, xyScale, zScale, xOffset, yOffset, zOffset, spacing
  integer*4 newTotal, numPtOld, numContTot, imodObj, ifOnList, icheck, i, iobj, numInObj
  integer*4 ipnt, ibase, izCont, numContInObj, numBefore, numAfter, limFlags, limPath/0/
  logical readw_or_imod, coplanar, getModelObjectRange
  integer getImodHead, getImodFlags, getImodObjSize, numberInList
  !
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetTwoIntegers
  integer*4 PipGetFloat, PipGetString
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  reducecont
  integer numOptions
  parameter (numOptions = 7)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@objects:ObjectsToReduce:LI:@'// &
      'tolerance:Tolerance:F:@smoothing:SmoothingPointsAndOrder:IP:@'// &
      'spaced:SpacedPointsTarget:F:@help:usage:B:'
  !
  numFit = 5
  iorder = 2
  tol = 0.25
  numObjDo = 0
  numContTot = 0
  spacing = 0.
  !
  ! Pip startup: set error, parse options, check help
  !
  call PipReadOrParseOptions(options, numOptions, 'reducecont', 'ERROR: REDUCECONT - ', &
      .false., 2, 1, 1, numOptArg, numNonOptArg)
  !
  if (PipGetInOutFile('InputFile', 1, ' ', inFile) .ne. 0) &
      call exitError('NO INPUT FILE SPECIFIED')
  !
  if (PipGetInOutFile('OutputFile', 2, ' ', outFile) .ne. 0) &
      call exitError('NO OUTPUT FILE SPECIFIED')
  !
  ierr = PipGetFloat('Tolerance', tol)
  i = PipGetTwoIntegers('SmoothingPointsAndOrder', numFit, iorder)
  if (tol <= 0) call exitError('ZERO TOLERANCE WILL HAVE NO EFFECT')
  if (numFit > 100 .or. iorder > 5) call exitError( &
      'NUMBERS OF POINTS OR ORDER FOR SMOOTHING TOO HIGH')
  if (PipGetFloat('SpacedPointsTarget', spacing) == 0 .and. (ierr == 0 .or. i == 0)) &
      call exitError('YOU CANNOT ENTER -spacing WITH -tolerance OR -smoothing')
  !
  call imodPartialMode(1)
  if (.not.readw_or_imod(inFile)) call exitError('READING MODEL')
  !
  numObjTot = getImodObjSize()
  limFlags = numObjTot + 20
  allocate(iobjDo(limFlags), iflags(limFlags), stat = ierr)
  call memoryError(ierr, 'ARRAYS PER OBJECT')
  if (PipGetString('ObjectsToReduce', inFile) == 0) &
      call parselist2(inFile, iobjDo, numObjDo, limFlags)

  call PipDone()
  ierr = getImodHead(xyScale, zScale, xOffset, yOffset, zOffset, ifFlipped)
  !
  ! Get object flags
  !
  iflags(:) = 0
  ierr = getImodFlags(iflags, limFlags)
  if (ierr .ne. 0) print *,'Error getting object types, assuming', &
      ' all are closed contours'

  ! print *,n_point, ' points,', max_mod_obj, ' contours in input file'
  !
  indy = 2
  indz = 3
  if (ifFlipped .ne. 0) then
    indy = 3
    indz = 2
  endif
  newTotal = 0
  numPtOld = 0
  !
  ! loop on model objects and decide whether to load
  !
  do imodObj = 1, numObjTot
    !
    ! is IMOD object on list
    ifOnList = numberInList(imodObj, iobjDo, numObjDo, 1)
    !
    ! require not scattered points
    iflag = mod(iflags(imodObj), 4)
    if (ifOnList == 1 .and. iflag < 2) then
      numContInObj = 0
      numBefore = 0
      numAfter = 0
      !
      if (.not.getModelObjectRange(imodObj, imodObj)) then
        write(outFile, '(a,i5)') 'LOADING DATA FOR OBJECT #', imodObj
        call exitError(outFile)
      endif
      call scale_model(0)
      !
      ! Check allocation sizes
      numInObj = maxval(npt_in_obj(1:max_mod_obj))
      if (numInObj > limPath - 10) then
        if (limPath > 0) deallocate(xt, yt, xx, yy, idum, jdum, stat = ierr)
        limPath = numInObj + 10
        allocate(xt(limPath), yt(limPath), xx(limPath), yy(limPath), idum(limPath), &
            jdum(limPath), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR CONTOURS')
      endif
      !
      ! Loop on contours, which are in this object
      !
      do iobj = 1, max_mod_obj
        numInObj = npt_in_obj(iobj)
        ibase = ibase_obj(iobj)
        coplanar = .true.
        i = 2
        izCont = nint(p_coord(indz, ibase + 1))
        do while (i <= numInObj .and. coplanar .and. spacing <= 0)
          coplanar = nint(p_coord(indz, object(i + ibase))) == izCont
          i = i + 1
        enddo

        if (numInObj > 2 .and. coplanar) then
          numBefore = numBefore + numInObj
          numContInObj = numContInObj + 1
          do i = 1, numInObj
            ipnt = object(i + ibase)
            xx(i) = p_coord(1, ipnt)
            yy(i) = p_coord(indy, ipnt)
            xt(i) = p_coord(indz, ipnt)
          enddo
          if (spacing <= 0) then
            call reducepts(xx, yy, numInObj, iorder, numFit, tol, xt, yt, idum, jdum)
          else
            call spacePoints(xx, yy, xt, yt, idum, jdum, numInObj, spacing)
          endif
          do i = 1, numInObj
            ipnt = object(i + ibase)
            p_coord(1, ipnt) = xx(i)
            p_coord(indy, ipnt) = yy(i)
            if (spacing > 0) p_coord(indz, ipnt) = xt(i)
          enddo
          ! print *,imodobj, iobj, ' reduced from', npt_in_obj(iobj), ' to', &
          ! ninobj
          npt_in_obj(iobj) = numInObj
          numAfter = numAfter + numInObj
        endif
      enddo
      numPtOld = numPtOld + numBefore
      newTotal = newTotal + numAfter
      numContTot = numContTot + numContInObj
      !
      call scale_model(1)
      call putModelObjects()
      if (numContInObj > 0) &
          write(*,101) imodObj, numContInObj, numBefore, numAfter
101   format('Object',i4,':',i7,' contours reduced from',i8,' to',i8, ' points')
    endif
  enddo
  !
  n_point = -1
  call write_wmod(outFile)
  write(*,102) numContTot, numPtOld, newTotal
102 format('     Total:',i7,' contours reduced from',i8,' to',i8, ' points')
  call exit(0)
end program


! REDUCEPTS reduces a set of points by smoothing and selecting a
! minimal subset of the points.  Each of the original points will be
! within a tolerance TOL of the segments defined by the new set of
! points.
! XX, YY are coordinates of the points
! NPTS is the number of points
! INORDER is the order of a polynomial to be fit to successive sets of
! points for smoothing; INFIT is the number of points to fit.  No
! smoothing is used if either value is zero.
! TOL is the tolerance
! XS, YS are scratch real*4 arrays, NEXTPT and MINSEG are scratch
! integer*4 arrays which should be at least as large as the original
! number of points
! The reduced set of points and number of points are returned in
! XX, YY and NPTS.
!
subroutine reducepts(xx, yy, numPts, inOrder, inFit, tol, xs, ys, nextPt, minSeg)
  implicit none
  integer LIMFIT, LIMORD
  parameter (LIMFIT = 30, LIMORD = 10)
  real*4 xx(*), yy(*), xs(*), ys(*), tol
  integer*4 nextPt(*), minSeg(*), numPts, inOrder, inFit
  real*4 xt(LIMFIT), yt(LIMFIT), slope(LIMORD)
  real*4 errLim, dx, dy, dlen, xmid, ymid, sinTheta, cosTheta, ycen, bintcp, tolSq
  integer*4 numFit, iorder, icen, ist, ind, in, io, i, numOutLim, left, irt, ifOut, its
  real*4 x1, y1, x2, y2, dx0, dy0, denom, distSq, tmin
  integer*4 numOut, npo, ipo, jcen

  errLim = 1.e-6
  !
  numFit = min(inFit, LIMFIT)
  iorder = min(inOrder, LIMORD)
  do icen = 1, numPts
    !
    ! copy data into xs, ys arrays
    !
    xs(icen) = xx(icen)
    ys(icen) = yy(icen)
    if (iorder > 0 .and. numFit > iorder .and. numFit <= numPts .and. &
        icen > 1 .and. icen < numPts) then
      !
      ! try to smooth data if iorder/nfit are set
      !
      ist = max(1, icen - numFit / 2)
      ind = min(numPts, ist + numFit - 1)
      ist = ind + 1 - numFit
      dx = xx(ind) - xx(ist)
      dy = yy(ind) - yy(ist)
      dlen = sqrt(dx**2 + dy**2)
      xmid = 0.5 * (xx(ind) + xx(ist))
      ymid = 0.5 * (yy(ind) + yy(ist))
      if (dlen > 1.) then
        !
        ! if the segment spanned by nfit points is long enough, rotate
        ! it about its midpoint so that it is horizontal
        !
        sinTheta = -dy / dlen
        cosTheta = dx / dlen
        do in = ist, ind
          io = in + 1 - ist
          xt(io) = cosTheta * (xx(in) - xmid) - sinTheta * (yy(in) - ymid)
          yt(io) = sinTheta * (xx(in) - xmid) + cosTheta * (yy(in) - ymid)
          if (in == icen) jcen = io
        enddo
        !
        ! fit to rotated points
        !
        call localPolyFit(xt, yt, numFit, iorder, slope, bintcp)
        ycen = bintcp
        do i = 1, iorder
          ycen = ycen + slope(i) * xt(jcen)**i
        enddo
        if (abs(ycen - yt(jcen)) < tol) then
          !
          ! if the fitted point is within tolerance of the actual value
          ! replace the actual value in the xs, ys arrays with the
          ! back-rotated fitted position
          !
          xs(icen) = cosTheta * xt(jcen) + sinTheta * ycen + xmid
          ys(icen) = -sinTheta * xt(jcen) + cosTheta * ycen + ymid
        endif
      endif
    endif
  enddo
  !
  ! moving from right to left, look at possible segments from a given
  ! point going to right.  A segment is possible if all intervening
  ! points are within TOL of the segment.  From the given point, find
  ! the possible segment that involves the fewest segments to get to
  ! the right end.  Keep track of the endpoint of the best segment in
  ! NEXTPT and the total number of segments in MINSEG
  !
  minSeg(numPts) = 0
  minSeg(numPts - 1) = 1
  nextPt(numPts) = numPts + 1
  nextPt(numPts - 1) = numPts
  tolSq = tol**2
  !
  ! Stop looking at longer segments after finding numOutLim segments that
  ! are not possible
  !
  numOutLim = 3
  do left = numPts - 2, 1, -1
    !
    ! set left edge of segment
    !
    minSeg(left) = minSeg(left + 1) + 1
    nextPt(left) = left + 1
    irt = left + 2
    x1 = xs(left)
    y1 = ys(left)
    numOut = 0
    do while(irt <= numPts .and. numOut < numOutLim)
      if (1 + minSeg(irt) < minSeg(left)) then
        !
        ! look at a segment only if it will give a better path: set
        ! right edge
        !
        ifOut = 0
        x2 = xs(irt)
        y2 = ys(irt)
        dx = x2 - x1
        dy = y2 - y1
        denom = dx**2 + dy**2
        its = left + 1
        do while(its < irt .and. ifOut == 0)
          !
          ! check distance of points from segment until one falls out
          !
          dx0 = xx(its) - x1
          dy0 = yy(its) - y1
          if (denom < 1.e-5) then
            distSq = dx0**2 + dy0**2
          else
            tmin = (dx0 * dx + dy0 * dy) / denom
            distSq = (tmin * dx - dx0)**2 + (tmin * dy - dy0)**2
          endif
          if (distSq >= tolSq) ifOut = 1
          its = its + 1
        enddo
        !
        ! if its an OK segment, set it up as new minimum
        !
        if (ifOut == 0) then
          minSeg(left) = 1 + minSeg(irt)
          nextPt(left) = irt
        else
          numOut = numOut + 1
        endif
      endif
      irt = irt + 1
    enddo
  enddo
  !
  ! when we get to the left edge, the minimal path is available by
  ! following the chain of NEXTPT values
  !
  npo = 1
  ipo = 1
  do while(nextPt(ipo) <= numPts)
    ipo = nextPt(ipo)
    npo = npo + 1
    xx(npo) = xs(ipo)
    yy(npo) = ys(ipo)
  enddo
  ! print *,npts, npo
  numPts = npo
  return
end subroutine reducepts

subroutine spacePoints(xx, yy, zz, xt, yt, zt, numInObj, targetSpace)
  implicit none
  real*4 xx(*), yy(*), zz(*), xt(*), yt(*), zt(*), spacing, targetSpace
  integer*4 numInObj
  real*8 totalDist
  real*4 distTarget, frac, segDist
  integer*4 i, iseg, numSeg
  !
  ! Compute total distance and cumulative distance to each point
  totalDist = 0.
  do i = 1, numInObj - 1
    totalDist = totalDist + sqrt((xx(i) - xx(i + 1))**2 + (yy(i) - yy(i + 1))**2 +  &
        (zz(i) - zz(i + 1))**2)
  enddo
  !
  ! Determine number of segments and ideal spacing from the target, limit to existing
  ! number of segments
  numSeg = min(nint(totalDist / targetSpace), numInObj - 1)
  spacing = totalDist / numSeg
  !
  ! Walk along contour find point past each target position, and interpolate from previous
  i = 1
  totalDist = 0.
  do iseg = 2, numSeg
    distTarget = (iseg - 1) * spacing
    do while (totalDist < distTarget)
      segDist = sqrt((xx(i) - xx(i + 1))**2 + (yy(i) - yy(i + 1))**2 +  &
          (zz(i) - zz(i + 1))**2)
      totalDist = totalDist + segDist
      i = i + 1
    enddo
    frac = max(0., min(1., (totalDist - distTarget) / max(.01, segDist)))
    xt(iseg) = frac * xx(i - 1) + (1. - frac) * xx(i)
    yt(iseg) = frac * yy(i - 1) + (1. - frac) * yy(i)
    zt(iseg) = frac * zz(i - 1) + (1. - frac) * zz(i)
  enddo
  !
  ! Copy the last point over, then resize contour and copy all points back from temp
  xt(numSeg + 1) = xx(numInObj)
  yt(numSeg + 1) = yy(numInObj)
  zt(numSeg + 1) = zz(numInObj)
  numInObj = numSeg + 1
  xx(2:numInObj) = xt(2:numInObj)
  yy(2:numInObj) = yt(2:numInObj)
  zz(2:numInObj) = zt(2:numInObj)
  return
end subroutine spacePoints

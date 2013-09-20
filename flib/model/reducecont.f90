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
  integer LIMPATH, LIMFLAGS
  include 'model.inc90'
  parameter (LIMPATH = 50000, LIMFLAGS = 5000)
  !
  real*4 xt(LIMPATH), yt(LIMPATH), xx(LIMPATH), yy(LIMPATH)
  integer*4 idum(LIMPATH), jdum(LIMPATH), iobjDo(LIMFLAGS), iflags(LIMFLAGS)
  !
  character*120 inFile, outFile
  integer*4 numFit, iorder, ierr, numObjDo, numObjTot, iflag, indy, indz, ifflip
  real*4 tol, xyScale, zScale, xOffset, yOffset, zOffset
  integer*4 newTotal, numPtOld, numContTot, imodObj, ifOnList, icheck, i, iobj, numInObj
  integer*4 ipnt, ibase, izCont, numContInObj, numBefore, numAfter
  logical readw_or_imod, coplanar, getModelObjectRange
  integer getImodHead, getImodFlags, getImodObjSize
  !
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetTwoIntegers
  integer*4 PipGetFloat, PipGetString
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  reducecont
  !
  integer numOptions
  parameter (numOptions = 6)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@'// &
      'objects:ObjectsToReduce:LI:@tolerance:Tolerance:F:@'// &
      'smoothing:SmoothingPointsAndOrder:IP:@help:usage:B:'
  !
  numFit = 5
  iorder = 2
  tol = 0.25
  numObjDo = 0
  numContTot = 0
  !
  ! Pip startup: set error, parse options, check help
  !
  call PipReadOrParseOptions(options, numOptions, 'reducecont', &
      'ERROR: REDUCECONT - ', .false., 2, 1, 1, numOptArg, &
      numNonOptArg)
  !
  if (PipGetInOutFile('InputFile', 1, ' ', inFile) .ne. 0) &
      call exitError('NO INPUT FILE SPECIFIED')
  !
  if (PipGetInOutFile('OutputFile', 2, ' ', outFile) .ne. 0) &
      call exitError('NO OUTPUT FILE SPECIFIED')
  !
  ierr = PipGetFloat('Tolerance', tol)
  ierr = PipGetTwoIntegers('SmoothingPointsAndOrder', numFit, iorder)
  if (tol <= 0) call exitError('ZERO TOLERANCE WILL HAVE NO EFFECT')
  if (numFit > 100 .or. iorder > 5) call exitError( &
      'NUMBERS OF POINTS OR ORDER FOR SMOOTHING TOO HIGH')
  !
  call imodPartialMode(1)
  if (.not.readw_or_imod(inFile)) call exitError('READING MODEL')
  !
  if (PipGetString('ObjectsToReduce', inFile) == 0) &
      call parselist(inFile, iobjDo, numObjDo)
  if (numObjDo > LIMFLAGS) call exitError( &
      'TOO MANY OBJECTS IN LIST FOR ARRAYS')

  call PipDone()
  ierr = getImodHead(xyScale, zScale, xOffset, yOffset, zOffset, ifflip)
  !
  ! Get object flags
  !
  do i = 1, LIMFLAGS
    iflags(i) = 0
  enddo
  ierr = getImodFlags(iflags, LIMFLAGS)
  if (ierr .ne. 0) print *,'Error getting object types, assuming', &
      ' all are closed contours'
  numObjTot = getImodObjSize()

  ! print *,n_point, ' points,', max_mod_obj, ' contours in input file'
  !
  indy = 2
  indz = 3
  if (ifflip .ne. 0) then
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
    !
    ifOnList = 0
    if (numObjDo == 0) then
      ifOnList = 1
    else
      do icheck = 1, numObjDo
        if (imodObj == iobjDo(icheck)) ifOnList = 1
      enddo
    endif
    !
    ! require not scattered points
    !
    iflag = 0
    if (imodObj <= LIMFLAGS) iflag = mod(iflags(imodObj), 4)
    if (ifOnList == 1 .and. iflag < 2) then
      numContInObj = 0
      numBefore = 0
      numAfter = 0
      !
      if (.not.getModelObjectRange(imodObj, imodObj)) then
        print *
        print *, 'ERROR: REDUCECONT - LOADING DATA FOR OBJECT #', imodObj
        call exit(1)
      endif
      call scale_model(0)
      !
      ! Loop on contours, which are in this object
      !
      do iobj = 1, max_mod_obj
        numInObj = npt_in_obj(iobj)
        ibase = ibase_obj(iobj)
        coplanar = .true.
        i = 2
        izCont = nint(p_coord(indz, ibase + 1))
        do while(i <= numInObj .and. coplanar)
          coplanar = nint(p_coord(indz, object(i + ibase))) == izCont
          i = i + 1
        enddo

        if (numInObj > 2 .and. numInObj <= LIMPATH .and. coplanar) then
          numBefore = numBefore + numInObj
          numContInObj = numContInObj + 1
          do i = 1, numInObj
            ipnt = object(i + ibase)
            xx(i) = p_coord(1, ipnt)
            yy(i) = p_coord(indy, ipnt)
          enddo
          call reducepts(xx, yy, numInObj, iorder, numFit, tol, xt, yt, idum, jdum)
          do i = 1, numInObj
            ipnt = object(i + ibase)
            p_coord(1, ipnt) = xx(i)
            p_coord(indy, ipnt) = yy(i)
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
101   format('Object',i4,':',i7,' contours reduced from',i8,' to',i8, &
          ' points')
    endif
  enddo
  !
  n_point = -1
  call write_wmod(outFile)
  write(*,102) numContTot, numPtOld, newTotal
102 format('     Total:',i7,' contours reduced from',i8,' to',i8, &
      ' points')
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
subroutine reducepts(xx, yy, numPts, inOrder, inFit, tol, xs, ys, nextPt, &
    minSeg)
  parameter (LIMFIT = 30, LIMORD = 10)
  real*4 xx(*), yy(*), xs(*), ys(*)
  integer*4 nextPt(*), minSeg(*)
  real*4 xt(LIMFIT), yt(LIMFIT), slope(LIMORD)
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
  ! Stop looking at longer segments after finding NOUTLIM segments that
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

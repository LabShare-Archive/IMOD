! ***    PROC_MODEL processes the model data,
! sorts out the model points that are to be included in
! the analysis, and converts the coordinates to "index" coordinates
! with the origin at the center of the section
!
! $Id$
!
subroutine proc_model(xcen, ycen, xdelt, ydelt, xorig, yorig, scaleXY, &
    nviewTot, minInView, izcur, nzLocal, listObj, ninList, numInView, &
    ivOrig, xx, yy, isecView, maxProjPt, maxReal, irealStr, iobjAli, &
    nview, nprojpt, nRealPt, izExclude, numExclude)
  !
  implicit none
  include 'smallmodel.inc90'
  real*4 xx(*), yy(*)
  integer*4 isecView(*), irealStr(*), listObj(*)
  integer*4 numInView(*), ivOrig(*), iobjAli(*), izExclude(*), numExclude
  real*4 xcen, ycen, xdelt, ydelt, xorig, yorig, scaleXY
  integer*4 nviewTot, minInView, izcur, nzLocal, ninList, nview, nprojpt
  integer*4 nRealPt, maxProjPt, maxReal
  integer*4 izst, iznd, izStrtMin, izEndMax, i, l, iobject, ibase, iz, ipt
  integer*4 numTry, itry, izStrtTry, izEndTry
  integer*4 needDo, numInObj, numLegal, maxView, minView, minViewsPresent
  real*4 adjNpnts, adjMaxpt
  logical itemOnList
  minViewsPresent = 3
  !
  ! find z limits based on nzLocal
  !
  if (nzLocal == 0 .or. nzLocal >= nviewTot) then
    izst = 1
    iznd = nviewTot
  else
    !
    ! count number of points on each view
    !
    izStrtMin = max(1, izcur - nzLocal)
    izEndMax = min(nviewTot, izcur + nzLocal)
    do i = izStrtMin, izEndMax
      numInView(i) = 0
    enddo
    do l = 1, ninList
      iobject = listObj(l)
      ibase = ibase_obj(iobject)
      do ipt = 1, npt_in_obj(iobject)
        iz = nint(p_coord(3, object(ipt + ibase))) + 1
        if (iz >= izStrtMin .and. iz <= izEndMax .and.  &
            .not. itemOnList(iz, izExclude, numExclude))  &
            numInView(iz) = numInView(iz) + 1
      enddo
    enddo
    !
    ! Force the current view to be included if it has points
    if (numInView(izcur) > 0) izStrtMin = max(1, izcur + 1 - nzLocal)
    !
    ! find placement of the window of views that maximizes an adjusted
    ! total number of points, where views within the range that would
    ! be equally spaced around the current one are given a 10% premium
    !
    numTry = izEndMax + 2 - izStrtMin - nzLocal
    ! print *,izcur, nzLocal, izStrtMin, izEndMax, numTry
    adjMaxpt = -1
    do itry = 1, numTry
      adjNpnts = 0
      izStrtTry = izStrtMin + itry - 1
      izEndTry = min(nviewTot, izStrtTry + nzLocal - 1)
      do i = izStrtTry, izEndTry
        adjNpnts = adjNpnts + numInView(i)
        if (abs(i - izcur) < nzLocal / 2) &
            adjNpnts = adjNpnts + 0.1 * numInView(i)
      enddo
      ! print *,izStrtTry, izEndTry, adjNpnts
      if (adjNpnts > adjMaxpt) then
        adjMaxpt = adjNpnts
        izst = izStrtTry
        iznd = izEndTry
      endif
    enddo
  endif
  !
  ! scan to get number of points on each view; eliminate consideration
  ! of views with not enough points, and objects with less than 2 points
  ! on the legal views; reiterate until all counted points come up above
  ! the minimum
  !
  do i = 1, nviewTot
    ivOrig(i) = 0
    numInView(i) = 0
  enddo
  do i = izst, iznd
    ivOrig(i) = 2 * minInView
  enddo
  needDo = 1
  !
  do while(needDo == 1)
    do i = izst, iznd
      numInView(i) = 0
    enddo
    do l = 1, ninList
      iobject = listObj(l)
      numInObj = npt_in_obj(iobject)
      if (numInObj >= minViewsPresent) then
        ibase = ibase_obj(iobject)
        numLegal = 0
        do ipt = 1, numInObj
          iz = nint(p_coord(3, object(ipt + ibase))) + 1
          if (ivOrig(iz) >= minInView .and. .not. itemOnList(iz, izExclude, numExclude)) &
              numLegal = numLegal + 1
        enddo
        if (numLegal >= minViewsPresent) then
          do ipt = 1, numInObj
            iz = nint(p_coord(3, object(ipt + ibase))) + 1
            if (ivOrig(iz) >= minInView .and.  &
                .not. itemOnList(iz, izExclude, numExclude)) &
                numInView(iz) = numInView(iz) + 1
          enddo
        endif
      endif
    enddo
    needDo = 0
    do i = izst, iznd
      if (numInView(i) < minInView .and. numInView(i) > 0) needDo = 1
      ivOrig(i) = numInView(i)
    enddo
  enddo
  !
  ! find number of views with enough points, build cross - indexes
  !
  nview = 0
  maxView = 0
  minView = nviewTot
  do i = izst, iznd
    if (numInView(i) .ne. 0) then
      nview = nview + 1
      numInView(i) = nview
      ivOrig(nview) = i
      minView = min(minView, i)
      maxView = max(maxView, i)
    endif
  enddo
  ! print *,'views', minView, ' to', maxView
  !
  ! go through model finding objects with more than one point in the
  ! proper z range, and convert to index coordinates, origin at center
  !
  nprojpt = 0
  nRealPt = 0
  do l = 1, ninList
    iobject = listObj(l)
    numInObj = npt_in_obj(iobject)
    ibase = ibase_obj(iobject)
    if (numInObj >= minViewsPresent) then
      numLegal = 0
      do ipt = 1, numInObj
        iz = nint(p_coord(3, object(ipt + ibase))) + 1
        if (numInView(iz) > 0) numLegal = numLegal + 1
      enddo
      if (numLegal >= minViewsPresent) then
        nRealPt = nRealPt + 1
        if (nRealPt > maxReal) call errorExit( &
            'TOO MANY FIDUCIALS FOR ARRAYS' , 0)
        irealStr(nRealPt) = nprojpt + 1
        iobjAli(nRealPt) = iobject
        do ipt = 1, numInObj                     !loop on points
          !
          ! find out if the z coordinate of this point is on the list
          !
          iz = nint(p_coord(3, object(ipt + ibase))) + 1
          if (numInView(iz) > 0) then
            !
            ! if so, add index coordinates to list
            !
            nprojpt = nprojpt + 1
            if (nprojpt > maxProjPt) call errorExit( &
                'TOO MANY PROJECTION POINTS FOR ARRAYS', 0)
            xx(nprojpt) = ((p_coord(1, object(ipt + ibase)) + xorig) / &
                xdelt - xcen) / scaleXY
            yy(nprojpt) = ((p_coord(2, object(ipt + ibase)) + yorig) / &
                ydelt - ycen) / scaleXY
            isecView(nprojpt) = numInView(iz)
          endif
        enddo
      endif
    endif
  enddo
  irealStr(nRealPt + 1) = nprojpt + 1             !for convenient looping
  return
end subroutine proc_model

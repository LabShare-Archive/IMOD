! Routines related to robust fitting
!
! $Id$
!
! setupWeightGroups:
! Sets up weighting groups, consisting of sets of views divided into rings if possible
!  maxRings = maximum number of rings allowed
!  minRes = minimum number of residuals in the group
!  minTiltView = view number at minimum tilt
!  ierr = return error value, 0 for success or 1 for failure
!
subroutine setupWeightGroups(maxRings, minRes, minTiltView, ierr)
  use alivar
  implicit none
  integer*4 irealRingList(maxReal), ierr, maxRings, minRes, minTiltView
  integer*4 maxViewsForRings(maxWgtRings) /1, 10, 8, 6, 5, 5, 4, 4, 3, 3/
  real*4 distReal(maxReal)
  integer*4 i, nring, nrealPerRing, neededViews, numViews, numViewGroups, numExtra
  integer*4 iexStart, ngrpBeforeEx, indProj, indGroup, indView, ivbase, igroup
  integer*4 irbase, ninGroup, ninRing, iring, iv, ind, ireal, iproj
  integer*4 numPts

  ! Get distances from center get sorted indexes to them
  do i = 1, nrealPt
    distReal(i) = sqrt(xyz(1, i)**2 + xyz(2, i)**2)
    irealRingList(i) = i
  enddo
  call rsSortIndexedFloats(distReal, irealRingList, nrealPt)

  ! Loop from largest number of rings down, first evaluate plausibility if
  ! all points are present
  NUM_RING_LOOP:  do nring = maxRings, 1, -1
    nrealPerRing = nrealPt / nring
    if (nrealPerRing == 0) cycle
    neededViews = max(1, minRes / nrealPerRing)
    if (neededViews > maxViewsForRings(nring) .and. nring > 1) then
      !print *, 'rejecting ', nring, ' rings out of hand'
      cycle
    endif
    !
    ! Try to set up groups of views of increasing sizes until one works
    NUM_VIEW_LOOP:  do numViews = neededViews, nview
      numViewGroups = nview / numViews
      numExtra = mod(nview, numViews)
      iexStart = max(1, minTiltView - numExtra / 2)
      ngrpBeforeEx = (iexStart - 1) / numViews
      indProj = 1
      ivbase = 0
      indGroup = 1
      indView = 1
      !
      ! Loop on the groups of views
      GROUP_LOOP:  do igroup = 1, numViewGroups
        irbase = 0
        ninGroup = numViews
        if (igroup > ngrpBeforeEx .and. igroup <= ngrpBeforeEx + numExtra) &
            ninGroup = ninGroup + 1
        !
        ! loop on the rings in views
        RING_LOOP:  do iring = 1, nring
          ninRing = nrealPerRing
          if (iring > nring - mod(nrealPt, nring)) ninRing = ninRing + 1
          !
          ! This is one weight group, set the starting view index of it
          ivStartWgtGroup(indGroup) = indView
          indGroup = indGroup + 1
          !
          ! loop on the views in the group; for each one, set the starting index
          ! in the projection list
          ! print *,'group', igroup, ivbase + 1, ivbase + ninGroup
          do iv = ivbase + 1, ivbase + ninGroup
            ! print *,'view in group', iv, indView, indProj
            ipStartWgtView(indView) = indProj
            indView = indView + 1
            !
            ! Loop on the real points in the ring, and for each one on the given view,
            ! add its projection point index to the list
            do ind = irbase + 1, irbase + ninRing
              ireal = irealRingList(ind)
              do iproj = irealStr(ireal), irealStr(ireal + 1) - 1
                if (isecView(iproj) == iv) then
                  ! print *,ireal, iproj, iv
                  indProjWgtList(indProj) = iproj
                  indProj = indProj + 1
                endif
              enddo
            enddo
          enddo
          !
          ! After each ring, increase the ring base index
          irbase = irbase + ninRing
          !
          ! But if there are too few in this group, make view groups bigger if
          ! possible for this number of rings; otherwise go on to try fewer rings;
          ! but if there is only one ring and it is up to all views, push on
          numPts = indProj - ipStartWgtView(ivStartWgtGroup(indGroup - 1))
          if (numPts < minres) then
            ierr = 1
            ! print *,'group too small', ivbase
            if (numViews >= maxViewsForRings(nring) .and. nring > 1) &
                exit NUM_VIEW_LOOP
            if (nring > 1 .or. numViews < nview) exit GROUP_LOOP
          endif
        enddo RING_LOOP
        !
        ! After each view group, increase the view base number
        ivbase = ivbase + ninGroup
        ierr = 0
      enddo GROUP_LOOP
      !
      ! If we got here with err 0, this setup fits constraints, finalize index lists
      if (ierr == 0) then
        ipStartWgtView(indView) = indProj
        ivStartWgtGroup(indGroup) = indView
        numWgtGroups = nring * numViewGroups
        write(*,'(/,a,i5,a,i4,a,i3,a)')'Starting robust fitting with', numWgtGroups, &
            ' weight groups:', numViewGroups, ' view groups in', nring, ' rings'
        !write(*,'(13i6)') (ivStartWgtGroup(i), i = 1, numWgtGroups + 1)
        !write(*,'(13i6)') (ipStartWgtView(i), i = 1, indView)
        return
      endif
    enddo NUM_VIEW_LOOP
  enddo NUM_RING_LOOP
  ierr = 1
  return
end subroutine setupWeightGroups


! computeWeights: 
! Computes the weights given the current set of residuals and the weighting groups.
! distRes and work are temp arrays that need to be at least as big as the biggest view
! group, and iwork needs to be as big as number of points on view
!
subroutine computeWeights(distRes, work, iwork)
  use alivar
  implicit none
  real*4 distRes(*), work(*), dev, rmedian, rMADN, adjMedian, tooManyZeroDelta
  integer*4 iwork(*), igroup, i, ind, ninGroup, numViews, indv, ninView, numLow, maxSmall
  integer*4 minNonZero

  minNonZero = 4
  tooManyZeroDelta = .02
  !
  ! Loop on the groups of views
  do igroup = 1, numWgtGroups
    ninGroup = 0
    numViews = ivStartWgtGroup(igroup + 1) - ivStartWgtGroup(igroup)
    !print *,'group', igroup, numViews, ivStartWgtGroup(igroup)
    !
    ! loop on the views in the group and get the residuals, divide by the smoothed
    ! median residual
    do indv = ivStartWgtGroup(igroup), ivStartWgtGroup(igroup + 1) - 1
      ninView = ipStartWgtView(indv + 1) - ipStartWgtView(indv)
      do i = 1, ninView
        ind = indProjWgtList(i + ipStartWgtView(indv) - 1)
        distRes(ninGroup + i) = sqrt(xresid(ind)**2 + yresid(ind)**2) /  &
            viewMedianRes(isecView(ind))
      enddo
      ninGroup = ninGroup + ninView
    enddo
    !
    ! Get overall median and MADN and compute weights
    call rsMedian(distRes, ninGroup, work, rmedian)
    call rsMADN(distRes, ninGroup, rmedian, work, rMADN)
    ninGroup = 0
    do indv = ivStartWgtGroup(igroup), ivStartWgtGroup(igroup + 1) - 1
      ninView = ipStartWgtView(indv + 1) - ipStartWgtView(indv)
      call weightsForView(rmedian)
      maxSmall = ninView * smallWgtMaxFrac
      if (numLow >  maxSmall) then
        !
        ! If there are too many small weights, then find the first one that must be
        ! given a weight above threshold and adjust the median to accomplish that
        do i = 1, ninView
          iwork(i) = i
        enddo
        call rsSortIndexedFloats(distRes(ninGroup + 1), iwork, ninView)
        !print *,numLow,' small weights, below limit of ', maxSmall
        !print *,' median', rmedian,'  MADN', rMADN
        !write(*, '(7f11.3)')(distRes(ninGroup + i), i = 1, ninView)
        !write(*, '(7i11)')(iwork(i), i = 1, ninView)
        !write(*, '(7f11.3)')(distRes(ninGroup + iwork(i)), i = 1, ninView)
        dev = sqrt(1. - sqrt(1.1 * smallWgtThreshold))
        adjMedian = distRes(ninGroup + iwork(ninView -  maxSmall)) - dev * kfacRobust * &
            rMADN
        call weightsForView(adjMedian)
        if (numLow >  maxSmall) print *,'WARNING: median adjustment for small weights bad'
        !print *,'Adjust median to ', adjMedian,' now there are ', numLow
      endif
      ninGroup = ninGroup + ninView
    enddo
  enddo
  !
  ! Now look at each real point and make sure it has enough non-zero weights, or
  ! specifically points above the delta value; and if not, add delta to all weights
  do ind = 1, nRealPt
    numLow = 0
    do i = irealStr(ind), irealStr(ind + 1) - 1
      if (weight(i) > tooManyZeroDelta) numLow = numLow + 1
    enddo
    if (numLow < minNonZero) then
    do i = irealStr(ind), irealStr(ind + 1) - 1
      weight(i) = min(1., weight(i) + tooManyZeroDelta)
    enddo
      
    endif
  enddo
  return

CONTAINS

  subroutine weightsForView(viewMedian)
    real*4 viewMedian
    numLow = 0
    do i = 1, ninView
      ind = indProjWgtList(i + ipStartWgtView(indv) - 1)
      dev = (distRes(ninGroup + i) - viewMedian) / (kfacRobust * rMADN)
      if (dev <= 0.) then
        weight(ind) = 1.
      else if (dev >= 1.) then
        weight(ind) = 0.
      else
        weight(ind) = (1. - dev**2)**2
      endif
      if (weight(ind) < smallWgtThreshold) numLow = numLow + 1
    enddo
    return
  end subroutine weightsForView

end subroutine computeWeights

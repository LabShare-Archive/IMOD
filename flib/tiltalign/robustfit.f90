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
! For ordinary weighting per point, the arrays mean the following:
!  ivStartWgtGroup has the starting view index of the weight group
!    these view indexes increment through all the groups
!  ipStartWgtView has the starting projection point index for each view subset in each
!    group.  These indexes also increment through all the groups
!  indProjWgtList has the true 2D point index for each projection point index
!
subroutine setupWeightGroups(maxRings, minRes, minTiltView, ierr)
  use alivar
  implicit none
  integer*4 irealRingList(maxReal), ierr, maxRings, minRes, minTiltView
  integer*4 maxViewsForRings(maxWgtRings) /1, 10, 8, 6, 5, 5, 4, 4, 3, 3/
  real*4 distReal(maxReal)
  integer*4 i, nring, nrealPerRing, neededViews, numViews, numViewGroups, numExtra
  integer*4 iexStart, ngrpBeforeEx, indProj, indGroup, indView, ivbase, igroup
  integer*4 irbase, ninGroup, ninRing, iring, iv, ind, ireal, iproj, nrealForViews
  integer*4 numPts

  nrealForViews = nrealPt
  if (patchTrackModel) nrealForViews = numFullTracksUsed

  ! Get distances from center and get sorted indexes to them
  do i = 1, nrealPt
    distReal(i) = sqrt(xyz(1, i)**2 + xyz(2, i)**2)
    irealRingList(i) = i
  enddo
  call rsSortIndexedFloats(distReal, irealRingList, nrealPt)

  ! Loop from largest number of rings down, first evaluate plausibility if
  ! all points are present.  Here use the number of full tracks for patch tracking
  NUM_RING_LOOP:  do nring = maxRings, 1, -1
    ierr = 1
    nrealPerRing = nrealForViews / nring
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
        ! loop on the rings in views; here base the number on the total number of real
        ! points not full tracks, because each will be considered for whether they have
        ! points in the view
        RING_LOOP:  do iring = 1, nring
          ninRing = nrealPt / nring
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
subroutine computeWeights(indAllReal, distRes, work, iwork)
  use alivar
  implicit none
  real*4 distRes(*), work(*), dev, rmedian, rMADN, adjMedian, tooManyZeroDelta
  integer*4 indAllReal(*)
  integer*4 iwork(*), igroup, i, ind, ninGroup, numViews, indv, ninView, numLow, maxSmall
  integer*4 minNonZero, j
  logical*4 wholeTrack

  wholeTrack = patchTrackModel .and. robustByTrack
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
        if (wholeTrack) then
          !
          ! recompute mean residual for track
          trackResid(ind) = 0.
          do j = irealStr(ind), irealStr(ind + 1) - 1
            trackResid(ind) = trackResid(ind) + sqrt(xresid(j)**2 + yresid(j)**2)
          enddo
          trackResid(ind) = trackResid(ind) / (irealStr(ind + 1) - irealStr(ind))
          distRes(ninGroup + i) = trackResid(ind) /  &
              viewMedianRes(itrackGroup(indAllReal(ind)))
        else
          distRes(ninGroup + i) = sqrt(xresid(ind)**2 + yresid(ind)**2) /  &
              viewMedianRes(isecView(ind))
        endif
      enddo
      ninGroup = ninGroup + ninView
    enddo
    !
    ! Get overall median and MADN and compute weights
    call rsFastMedian(distRes, ninGroup, work, rmedian)
    call rsFastMADN(distRes, ninGroup, rmedian, work, rMADN)
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
  if (wholeTrack) return
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

  ! Compute the weights for one view in a weight group, or for tracks in a track group
  subroutine weightsForView(viewMedian)
    real*4 viewMedian, trackWgt
    numLow = 0
    if (wholeTrack) then
      !
      ! For whole track, loop on track, get one weight for each, make sure it is not 0,
      ! and assign it to all projection points
      do i = 1, ninView
        ind = indProjWgtList(i + ipStartWgtView(indv) - 1)
        dev = (distRes(ninGroup + i) - viewMedian) / (kfacRobust * rMADN)
        if (dev <= 0.) then
          trackWgt = 1.
        else if (dev >= 1.) then
          trackWgt = tooManyZeroDelta
        else
          trackWgt = max(tooManyZeroDelta, (1. - dev**2)**2)
        endif
        if (trackWgt < smallWgtThreshold) numLow = numLow + 1
        weight(irealStr(ind) : irealStr(ind + 1) - 1) = trackWgt
        !write(*,'(i4,2f9.4,f9.4)')ind,scaleXY* &
        !    trackResid(ind), distRes(ninGroup + i), trackWgt
      enddo
    else
      !
      ! Otherwise, loop on projection points in the view-weight group
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
    endif
    return
  end subroutine weightsForView

end subroutine computeWeights


! Sets up weighting that is the same for all points in a contour with patch track data
!  maxRings = maximum number of rings allowed
!  minRes = minimum number of tracks in the group
!  ierr = return error value, 0 for success or 1 for failure
!
! For weighting per contour, the arrays mean the following:
!  ivStartWgtGroup has the starting track group index of the weight group
!    these track group indexes increment through all the weight groups
!  ipStartWgtView has the starting real point index for each track group subset in each
!    weight group.  These indexes also increment through all the weight groups
!  indProjWgtList has the true real point number for each real point index
!
subroutine setupTrackWeightGroups(maxRings, minres, indAllReal, ierr)
  use alivar
  implicit none
  integer*4  ierr, maxRings, minRes, indAllReal(*)
  integer*4 irealRingList(maxReal), listTrack(numTrackGroups)
  real*4 distReal(maxReal)
  integer*4 i, nring, numViewGroups, indProj, indGroup, indView, igroup, ind, iv, numPairs
  integer*4 irbase, ninRing, iring, ireal, numPts, maxCombine, numCombine, ninList
  integer*4 numberInList

  ! Get distances from center and get sorted indexes to them
  do i = 1, nrealPt
    distReal(i) = sqrt(xyz(1, i)**2 + xyz(2, i)**2)
    irealRingList(i) = i
  enddo
  call rsSortIndexedFloats(distReal, irealRingList, nrealPt)
  ! print *,'numFullTracksUsed,minres,ntg',numFullTracksUsed, minres, numTrackGroups

  ! Evaluate possibility of rings before any track group combine
  NUM_RING_LOOP:  do nring = maxRings, 1, -1
    ierr = 1
    maxCombine = max(1, 2 * (numTrackGroups / 2))
    if (nring > 1) then
      if (numFullTracksUsed / nring < minres) cycle
      maxCombine = 1
    endif
    ! print *,'nring,maxCombine',nring,maxCombine

    ! Combine track groups until we find a set that works with given # of rings
    numViewGroups = numTrackGroups
    NUM_TGROUP_LOOP:  do numCombine = 1, maxCombine
      if (numCombine > 1) then
        if (mod(numCombine, 2) > 0) cycle
        numViewGroups = maxCombine / numCombine
      endif
      ! print *,'numCombine,numViewGroups', numCombine,numViewGroups
      indGroup = 1
      indProj = 1
      indView = 1

      GROUP_LOOP:  do igroup = 1, numViewGroups
        !
        ! Make list of track groups in the view group
        if (numCombine == 1) then
          ninList = 1
          listTrack(1) = igroup
        else

          ! Combine by pairs.  If there are leftover pairs of groups, add them to last one
          numPairs = numCombine / 2
          if (igroup == numViewGroups) &
              numPairs = numPairs + mod(maxCombine / 2, numPairs)
          do i = 1, numPairs
            listTrack(2 * i - 1) = (numCombine / 2) * (igroup - 1) + i
            listTrack(2 * i) = numTrackGroups + 1 - listTrack(2 * i - 1)
          enddo
          ninList = 2 * numPairs

          ! Add an odd one in the middle to the last group
          if (igroup == numViewGroups .and. mod(numTrackGroups, 2) > 0) then
            ninList = ninList + 1
            listTrack(ninList) = (numTrackGroups + 1) / 2
          endif
        endif
        ! write(*,'(a,i3,a,12i4)')'view group',igroup,'  track groups', &
        !    (listTrack(i),i=1,ninList)
        !
        ! Loop on the rings, count up tracks actually in there
        irbase = 0
        RING_LOOP:  do iring = 1, nring
          ninRing = nrealPt / nring
          if (iring > nring - mod(nrealPt, nring)) ninRing = ninRing + 1

          ! This starts a weight group, save starting track group index
          ivStartWgtGroup(indGroup) = indView
          indGroup = indGroup + 1
          !
          ! Loop on the track groups in the weight group, for each one, set the starting
          ! index in the "projection" list
          do iv = 1, ninList
            ipStartWgtView(indView) = indProj
            indView = indView + 1
            ! Loop on the tracks in the ring, find ones in this track group
            do ind = irbase + 1, irbase + ninRing
              ireal = irealRingList(ind)
              if (itrackGroup(indAllReal(ireal)) == listTrack(iv)) then
                indProjWgtList(indProj) = ireal
                indProj = indProj + 1
              endif
            enddo
          enddo

          ! After each ring, increase the ring base index
          irbase = irbase + ninRing
          numPts = indProj - ipStartWgtView(ivStartWgtGroup(indGroup - 1))
          if (numPts < minres) then
            ierr = 1
            exit GROUP_LOOP
          endif
        enddo RING_LOOP
        ierr = 0
      enddo GROUP_LOOP
      !
      ! If we got here with err 0, this setup fits constraints, finalize index lists
      if (ierr == 0) then
        ipStartWgtView(indView) = indProj
        ivStartWgtGroup(indGroup) = indView
        numWgtGroups = nring * numViewGroups
        write(*,'(/,a,i5,a,i4,a,i3,a)')'Starting robust fitting with', numWgtGroups, &
            ' weight groups:', numViewGroups, ' track groups in', nring, ' rings'
        return
      endif
    enddo NUM_TGROUP_LOOP
  enddo NUM_RING_LOOP
  ierr = 1
  return
end subroutine setupTrackWeightGroups

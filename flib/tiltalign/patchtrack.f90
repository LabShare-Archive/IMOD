! Routines for dealing with patch track data
!
! $Id$
!
! analyzePatchTracks:
! Sort the tracks into groups corresponding to an original full track and set up various
! mappings
!
subroutine analyzePatchTracks(numProjPt)
  use alivar
  implicit none
  integer*4, allocatable :: ninTemp(:)
  real*4, allocatable :: midpoints(:)
  real*4 avgLen, diffMax
  integer*4 numProjPt, len, maxLen, numPeaks
  integer*4 lastRealDone, numOnList, listCur, indMin, indMax, idir
  integer*4 ireal, minRview, maxRview, i, j, ierr

  len = nrealPt + 4
  allocate(mapRealToTrack(len), mapTrackToReal(len), itrackGroup(len), midpoints(len), &
      trackResid(len), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR PATCH TRACK MAPS')
  !
  ! Build up mapRealToTrack.  Use mapTrackToReal for list of ones to process, and
  ! mapFileToView for flag of which ends need checking
  numFullPatchTracks = 0
  mapRealToTrack(:) = 0
  lastRealDone = 0
  do while (lastRealDone < nrealPt)
    !
    ! Find the next point that is not mapped yet
    do while (lastRealDone < nrealPt) 
      lastRealDone = lastRealDone + 1
      if (mapRealToTrack(lastRealDone) == 0) then
        exit
      endif
    enddo
    if (mapRealToTrack(lastRealDone) > 0) then
      exit
    endif
    !
    ! Start a new track and the list with the next real point, both ends need checking
    numFullPatchTracks = numFullPatchTracks + 1
    mapRealToTrack(lastRealDone) = numFullPatchTracks
    mapTrackToReal(1) = lastRealDone
    mapFileToView(1) = 3
    numOnList = 1
    listCur = 1
    !
    ! process next point on list: find first and last view
    do while (listCur <= numOnList)
      minRview = 100000
      maxRview = -100000
      ireal = mapTrackToReal(listCur)
      do i = irealstr(ireal), irealStr(ireal + 1) - 1
        if (isecView(i) < minRview) then
          indMin = i
          minRview = isecView(i)
        endif
        if (isecView(i) > maxRview) then
          indMax = i
          maxRview = isecView(i)
        endif
      enddo
      !
      ! Check higher end first, then substitute lower end and check it
      do idir = 1, 2
        if ((idir == 1 .and. mapFileToView(listCur) > 1) .or.  &
            (idir == 2 .and. mod(mapFileToView(listCur), 2) > 0)) then
          LOOP_REAL: do i = lastRealDone + 1, nrealPt
            if (mapRealToTrack(i) == 0) then
              do j = irealStr(i), irealStr(i + 1) - 1
                if (isecView(j) == maxRview .and. abs(xx(indMax) - xx(j)) < 0.1 .and. &
                    abs(yy(indMax) - yy(j)) < 0.1) then
                  !
                  ! Found the adjacent one in the track, add it to the list
                  mapRealToTrack(i) = numFullPatchTracks
                  numOnList = numOnList + 1
                  mapFileToView(numOnList) = 3 - idir
                  mapTrackToReal(numOnList) = i
                  exit LOOP_REAL
                endif
              enddo
            endif
          enddo LOOP_REAL
        endif

        ! Set up for other direction
        maxRview = minRview
        indMax = indMin
      enddo
      ! 
      ! Done with this one, advance on list
      listCur = listCur + 1
    enddo
  enddo
  !
  ! Now find out how many in each track and set up indexes
  allocate(indFullTrack(numFullPatchTracks + 4), ninTemp(numFullPatchTracks), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR PATCH TRACK INDICES')
  ninTemp(:) = 0
  avgLen = 0
  maxLen = 0
  do j = 1, nrealPt
    i = mapRealToTrack(j)
    ninTemp(i) = ninTemp(i) + 1
    len = isecView(irealStr(j + 1) - 1) + 1 - isecView(irealStr(j))
    midpoints(j) = (isecView(irealStr(j)) + isecView(irealStr(j + 1) - 1)) / 2.
    avgLen = avgLen + len
    maxLen = max(maxLen, len)
  enddo
  avgLen = avgLen / nrealPt
  indFullTrack(1) = 1
  do j = 1, numFullPatchTracks
    indFullTrack(j + 1) = indFullTrack(j) + ninTemp(j)
  enddo
  !
  ! Then populate the map using the indices
  ninTemp(:) = 0
  do j = 1, nrealPt
    i = mapRealToTrack(j)
    mapTrackToReal(indFullTrack(i) + ninTemp(i)) = j
    ninTemp(i) = ninTemp(i) + 1
  enddo
  !
  ! Get a kernel histogram of the midpoints and find peaks in it
  call kernelHistogram(midpoints, nrealPt, dmag, nview, 0.5, nview + 0.5, avgLen / 8., 0)
  numPeaks = 0
  linAlf(1) = 0.
  do i = 2, nview - 1
    !
    ! If point is a peak, check whether it is within 1/4 of maximum length of last one,
    ! and if so take the stronger of the two
    if (dmag(i) > dmag(i - 1) .and. dmag(i) > dmag(i + 1)) then
      if (numPeaks > 0 .and. i - linAlf(max(1, numPeaks)) < maxLen / 4.) then
        if (dmag(i) > dmag(linAlf(numPeaks))) linAlf(numPeaks) = i
      else
        numPeaks = numPeaks + 1
        linAlf(numPeaks) = i
      endif
    endif
  enddo
  !
  ! Put each track in a group based on which peak its midpoint is nearest
  do j = 1, nrealPt
    diffMax = 1.e10
    do i = 1, numPeaks
      if (diffMax > abs(midpoints(j) - linAlf(i))) then
        diffMax = abs(midpoints(j) - linAlf(i))
        idir = i
      endif
    enddo
    itrackGroup(j) = idir
  enddo
  numTrackGroups = numPeaks

  deallocate(ninTemp, midpoints)

  !print *,numFullPatchTracks,' full tracks'
  !do j = 1, numFullPatchTracks
  ! write(*,'(15i5)')j,(mapTrackToReal(i), i = indFullTrack(j), indFullTrack(j + 1) - 1)
  !enddo
  numFullTracksUsed = numFullPatchTracks
  return
end subroutine analyzePatchTracks


! loadPatchSubset:
! Selects a subset of tracks to avoid trying to solve for a global solution with a huge
! number of tracks.  All tracks in a full track are used.  numTarget is the target
! number of full tracks to select, and minInView is the minimum number of points in each 
! view.
!
subroutine loadPatchSubset(allxx, allyy, nAllProjPt, nprojPt, indAllReal, nAllRealPt,  &
    iallRealStr, iallSecv, numInView, numTarget, minInView)
  use alivar
  implicit none 
  real*4 allxx(*), allyy(*)
  integer*4 indAllReal(*), iallRealStr(*), iallSecv(*), nAllProjPt, nAllRealPt, numTarget
  integer*4 numInView(*), minInView, nprojPt
  logical*1 usedFullTrack(numFullPatchTracks), usedReal(nrealPt)
  real*4 gapSize(numFullPatchTracks)
  integer*4 indGapStart(numFullPatchTracks), indSort(nrealPt)
  integer*4 numUsedFull, lenTwiceAvg, ireal, lenVeryLong, num, lastUsed, numGaps, ifull
  integer*4 isort, numFill, i, iv, iseg, midpoint, idiff, idir, j
  real*4 density
  !
  ! First save all the data
  nAllRealPt = nrealPt
  nAllProjPt = nprojPt
  allxx(1:nprojPt) = xx(1:nprojPt)
  allyy(1:nprojPt) = yy(1:nprojPt)
  iallRealStr(1:nrealPt + 1) = irealStr(1:nrealPt + 1)
  iallSecv(1:nprojPt) = isecView(1:nprojPt)
  usedFullTrack(:) = .false.
  usedReal(:) = .false.
  !
  nrealPt = 0
  nprojPt = 0
  numUsedFull = 0
  density = float(numTarget) / numFullPatchTracks
  !
  ! Include any track that is 2x longer than average of at least 0.8 * full set of views
  lenTwiceAvg = nint((2. * nAllProjPt) / nAllRealPt)
  lenVeryLong = nint(0.8 * nview)
  ! print *,'Adding long tracks if any'
  do ireal = 1, nAllRealPt
    num = iallRealStr(ireal + 1) - iallRealStr(ireal) 
    if (.not. usedReal(ireal) .and. (num >= lenTwiceAvg .or. num >= lenVeryLong)) then
      call addFullTrack(mapRealToTrack(ireal))
    endif
  enddo
  !
  ! Make a list of all gaps between used tracks and sort it
  lastUsed = 1
  numGaps = 0
  do ifull = 2, numFullPatchTracks
    if (usedFullTrack(ifull) .or. ifull == numFullPatchTracks) then
      numGaps = numGaps + 1
      indGapStart(numGaps) = lastUsed
      indSort(numGaps) = numGaps
      gapSize(numGaps) = ifull - lastUsed
      lastUsed = ifull
    endif
  enddo
  if (numGaps > 1) call rsSortIndexedFloats(gapSize, indSort, numGaps)
  ! print *,'Number of gaps', numgaps
  !
  ! Fill in gaps from the biggest down, using evenly spaced selections to achieve
  ! desired density
  isort = numGaps
  do while (isort >= 1 .and. numUsedFull < numTarget)
    numFill = nint(density * gapSize(indSort(isort))) - 1
    if (numFill == 0) then
      exit
    endif
    do i = 1, numFill
      ifull = nint(i / density) + indGapStart(indSort(isort))
      if (.not. usedFullTrack(ifull)) call addFullTrack(ifull)
    enddo
    isort = isort - 1
  enddo
  ! print *,'Checking enough in each view'
  !
  ! Now the worry is that there may not be a minimum number per section
  indSort = (/(i, i = 1, nrealPt)/)
  call countNumInView(indSort, nrealPt, irealStr, isecView, nview, numInView)
  do iv = 1, nview
    if (numInView(iv) < minInView) then
      numFill = minInView - numInView(iv)
      print *,'Need to add',numFill,' for view',iv
      !
      ! Go to the midpoint of equally spaced segments and find nearest not used with
      ! point on view
      do iseg = 1, numFill
        midpoint = nint(((iseg - 0.5) * numFullPatchTracks) / numFill)
        ! print *,'Finding nearest to', midpoint
        FIND_NEAREST: do idiff = 0, numFullPatchTracks
          do idir = -1, 1, 2
            ifull = midpoint + idir * idiff
            if (ifull >= 1 .and. ifull <= numFullPatchTracks) then
              if (.not. usedFullTrack(ifull)) then
                do j = indFullTrack(ifull), indFullTrack(ifull + 1) - 1
                  ireal = mapTrackToReal(j)
                  do i = iallRealStr(ireal), iallRealStr(ireal + 1) - 1
                    if (iallSecv(i) == iv) then
                      call addFullTrack(ifull)
                      exit FIND_NEAREST
                    endif
                  enddo
                enddo
              endif
            endif
          enddo
        enddo FIND_NEAREST
      enddo
      indSort = (/(i, i = 1, nrealPt)/)
      call countNumInView(indSort, nrealPt, irealStr, isecView, nview, numInView)
    endif
  enddo
  write(*,'(a,i5,a,i5,a,i5,a,i5,a)')'Selected', numUsedFull,' of',numFullPatchTracks,&
      ' full tracks,', nrealPt,' of', nAllRealPt,' track segments'
  irealStr(nrealPt + 1) = nprojPt + 1
  numFullTracksUsed = numUsedFull
  return

CONTAINS

  ! Adds all the individual tracks in a full track
  !
  subroutine addFullTrack(indFull)
    integer*4 indReal, j, i, indFull
    usedFullTrack(indFull) = .true.
    numUsedFull = numUsedFull + 1
    ! print *,'Adding full track ',indFull
    do j = indFullTrack(indFull), indFullTrack(indFull + 1) - 1
      indReal = mapTrackToReal(j)
      nrealPt = nrealPt + 1
      indAllReal(nrealPt) = indReal
      irealStr(nrealPt) = nprojPt + 1
      usedReal(nrealPt) = .true.
      do i = iallRealStr(indReal), iallRealStr(indReal + 1) - 1
        nprojPt = nprojPt + 1
        xx(nprojPt) = allxx(i)
        yy(nprojPt) = allyy(i)
        isecView(nprojPt) = iallSecv(i)
      enddo
    enddo
  end subroutine addFullTrack

end subroutine loadPatchSubset


! Restores the data arrays from doing a subset of tracks and solves for XYZ for the
! remainder of the real points
!
subroutine restoreFromPatchSample(allxx, allyy, nAllProjPt, nprojPt, allXYZ, indAllReal, &
    nAllRealPt, iallRealStr, iallSecv, afac, bfac, cfac, dfac, efac, ffac)
  use alivar
  implicit none
  real*4 allxx(*), allyy(*), afac(*), bfac(*), cfac(*), dfac(*), efac(*), ffac(*)
  real*4 allXYZ(3,*)
  integer*4 indAllReal(*), iallRealStr(*), iallSecv(*), nAllProjPt, nAllRealPt, nprojPt
  real*4 asq(nview), bsq(nview), csq(nview), axb(nview), axc(nview), bxc(nview)
  logical*1 inSample(nAllRealPt)
  integer*4 i, ind
  !
  ! Copy the xyz's into the larger array and mark them in the sample array
  print *,nAllProjPt, nprojPt, nAllRealPt, nrealPt
  inSample(:) = .false.
  do i = 1, nrealPt
    ind = indAllReal(i)
    inSample(ind) = .true.
    allXYZ(:, ind) = xyz(:,i)
  enddo
  !
  ! Get the cross-products then solve for xyz for the rest of the points
  call regressionCrossProducts(nview, afac, bfac, cfac, dfac, efac, ffac, asq, bsq, csq, &
      axb, axc, bxc)
  do i = 1, nAllRealPt
    if (.not.inSample(i)) then
      call oneXYZbyRegression(afac, bfac, cfac, dfac, efac, ffac, asq, bsq, csq, axb, &
        axc, bxc, dxy, allxx, allyy, iallRealStr(i), iallRealStr(i + 1) - 1, iallSecv, &
        allXYZ(1, i))
    endif
    indAllReal(i) = i
  enddo
  !
  ! Restore all the data
  nrealPt = nAllRealPt
  nprojPt = nAllProjPt
  xyz(:,1:nrealPt) = allXYZ(:,1:nrealPt)
  xx(1:nprojPt) = allxx(1:nprojPt)
  yy(1:nprojPt) = allyy(1:nprojPt)
  irealStr(1:nrealPt + 1) = iallRealStr(1:nrealPt + 1)
  isecView(1:nprojPt) = iallSecv(1:nprojPt)
  return
end subroutine restoreFromPatchSample

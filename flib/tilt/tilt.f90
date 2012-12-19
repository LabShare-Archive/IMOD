! -----------------------------------------------------------------
!
! TILT......A program for reconstructing a three-dimensional object
! from a series of two-dimensional projections.
! The projections are assumed to arise from rotation about
! a fixed tilt axis.
!
! See man page for details
!
! $Id$
!
program tilt
  use tiltvars
  implicit none
  integer*4 nxyztmp(3), nxyzst(3)
  data nxyzst/0., 0., 0./
  character*20 radtxt1/'Radial weighting'/
  character*18 radtxt2/'   function'/
  !
  integer*4 interhsave, nsliceout, nslice, nxprj2
  integer*4 inloadstr, inloadend, lastready, lastcalc, nextfreevs
  integer*4 lvsstart, lvsend, nvsinring, ni, loadlimit, LSLICEout
  integer*4 lsstart, lsend, lslice, needstart, needend, itryend, ibaseSIRT
  integer*4 itry, ifenough, laststart, lastend, i, lsmin, lsmax, nextReadFree
  integer*4 iv, nalready, lsProjEnd, lReadStart, lReadEnd, nReadInRing
  real*4 dmin, dmax, ycenfix, abssal, tanalpha, dmin4, dmax4, dmin5, dmax5, dmin6, dmax6
  real*4 valmin, vslcen, vycenfix, riBot, riTop, tmax, tmean, tmin
  integer*4 lriMin, lriMax, loadstart, loadEnd, lri, isirt, ierr, j, k
  integer*4 ibase, lstart, nv, ISTART, NL, iyload, ix, ioffset
  integer*4 iringstart, mode, needGpuStart, needGpuEnd, keepOnGpu, numLoadGpu
  real*4 unscmin, unscmax, recscale, recflevl, DMEAN, pixelTot
  real*4 rpfill, curmean, firstmean, vertSum, numVertSum, edgeFillOrig
  real*4 composeFill
  integer*4 iset, mapEnd, nz5, lfillStart, lfillEnd
  real*8 dtot8
  logical*4 shiftedGpuLoad, composedOne, truncations, extremes
  integer*4 gpuLoadProj, gpuReprojOneSlice, parWrtSetCurrent
  real*8 walltime, tstart, dsum, dpix
  !
  interhsave = 20
  nsliceout = 0
  DTOT8 = 0.
  DMIN = 1.E30
  DMAX = -1.E30
  DMIN4 = 1.E30
  DMAX4 = -1.E30
  DMIN5 = 1.E30
  DMAX5 = -1.E30
  DMIN6 = 1.E30
  DMAX6 = -1.E30
  nz5 = 0
  debug = .false.
  !
  ! Open files and read control data
  CALL INPUT
  NSLICE = (isliceEnd - isliceStart) / idelSlice + 1
  valmin = 1.e-3 * (dmaxIn - dminIn)
  edgeFill = dmeanIn
  if (ifLog .ne. 0) edgeFill = alog10(max(valmin, dmeanIn + baseForLog))
  edgeFill = edgeFill * zeroWeight
  edgeFillOrig = edgeFill
  mapEnd = indOutSlice + isliceSizeBP - 1

  if (debug)  print *,'iflog=', ifLog, ' scale=', outScale, '  edgefill=', edgeFill
  composeFill = edgeFill * numViews
  !
  ! recompute items not in common
  NXPRJ2 = nxProj + 2 + numPad
  !
  ! initialize variables for loaded slices
  inloadstr = 0
  inloadend = 0
  lastready = 0
  lastcalc = 0
  !
  ! initialize variables for ring buffer of vertical slices: # in the ring, next free
  ! position, starting and ending slice number of slices in ring
  nvsinring = 0
  nextfreevs = 1
  lvsstart = -1
  lvsend = -1
  !
  ! initialize similar variables for ring buffer of read-in slices
  nReadInRing = 0
  nextReadFree = 1
  lreadStart = -1
  lreadEnd = -1
  lfillStart = -1
  ycenfix = ycenOut
  abssal = abs(sinAlpha(1))
  composedOne = ifAlpha >= 0
  numVertSum = 0
  vertSum = 0.
  !
  ! Calculate and report stack loading
  ! The stack is constructed as follows:
  ! (Radial weighting function, 1 or two planes) --->
  ! (current output plane) --->
  ! (vertical reconstructed planes for new X-axis tilting) --->
  ! (plane(s) read in from rec for SIRT) --->
  ! (working reprojection plane for SIRT)
  ! (first slice, first view) ....(first slice, last view) --->
  ! ..................................................--->
  ! (last slice, first view ) ....(last slice, last view)
  ! (Extra space for another set of slices, if cosine stretching)
  ! After each view two additional elements are inserted for
  ! the transform.
  !
  NI = numPlanes * inPlaneSize
  if (.not. recReproj) then
    WRITE(6, 900) maxStack, radtxt1, indOutSlice - 1, radtxt2, isliceSizeBP
    if (ifAlpha < 0) write(6, 902) numVertNeeded, numVertNeeded * ithickBP * iwidth
    if (numSIRTiter > 0) then
      if (indWorkPlane - ireadBase > 0) &
          write(6, 904) max(1, numReadNeed), indWorkPlane - ireadBase
      write(6, 907) inPlaneSize
    endif
    write(6, 903) numPlanes, NI
    if (ipExtraSize .ne. 0) write(6, 901) ipExtraSize
    !
    ! Allocate maskEdges array and prepare fixed maskEdges
    allocate(ixUnmaskedSE(2, ithickBP), stat = ierr)
    if (ierr .ne. 0) call exitError('ALLOCATING MASK ARRAY')
    if (ifAlpha == 0 .or. .not.maskEdges) call maskprep(isliceStart)
  endif
  if (debug) print *,'slicen', centerSlice, ', imap', indOutSlice, ', nbase', indLoadBase
  if (ifAlpha >= 0) then
    loadlimit = isliceEnd
  else
    loadlimit = centerSlice + (isliceEnd - centerSlice) * cosAlpha(1) +  &
        yOffset * sinAlpha(1) + 0.5 * ithickOut * abssal + 2.
  endif
  !
  ! Main loop over slices perpendicular to tilt axis
  ! ------------------------------------------------
  LSLICEout = isliceStart
  DO while (LSLICEout <= isliceEnd)
    !
    ! get limits for slices that are needed: the slice itself for regular
    ! work, or required vertical slices for new-style X tilting
    !
    if (debug) print *,'working on', lsliceout
    if (ifAlpha >= 0) then
      lsstart = lsliceout
      lsend = lsliceout
    else
      tanalpha = sinAlpha(1) / cosAlpha(1)
      lsmin = centerSlice + (lsliceout - centerSlice) * cosAlpha(1) +  &
          yOffset * sinAlpha(1) - 0.5 * ithickOut * abssal - 1.
      lsmax = centerSlice + (lsliceout - centerSlice) * cosAlpha(1) +  &
          yOffset * sinAlpha(1) + 0.5 * ithickOut * abssal + 2.
      if (debug) print *,'need slices', lsmin, lsmax
      lsmin = max(1, lsmin)
      lsmax = min(nyProj, lsmax)
      lsstart = lsmin
      if (lsmin >= lvsstart .and. lsmin <= lvsend) lsstart = lvsend + 1
      lsend = lsmax
    endif
    !
    ! loop on needed vertical slices
    !
    if (debug) print *,'looping to get', lsstart, lsend

    do lslice = lsstart, lsend
      shiftedGpuLoad = .false.
      !
      ! Load stack with as many lines from projections as will
      ! fit into the remaining space.
      !
      ! Enter loading procedures unless the load is already set for the
      ! current slice
      !
      if (inloadstr == 0 .or. lslice > lastready) then
        needstart = 0
        needend = 0
        itryend = 0
        lastready = 0
        itry = lslice
        ifenough = 0
        !
        ! loop on successive output slices to find what input slices are
        ! needed for them; until all slices would be loaded or there
        ! would be no more room
        do while (itry > 0 .and. itry <= nyProj .and. ifenough == 0 .and. &
            itry <= loadlimit .and. itryend - needstart + 1 <= numPlanes)
          laststart = neededStarts(itry - indNeededBase)
          lastend = neededEnds(itry - indNeededBase)
          !
          ! values here must work in single-slice case too
          ! if this is the first time, set needstart
          ! if this load still fits, set needend and lastready
          !
          itryend = lastend
          if (needstart == 0) needstart = laststart
          if (itryend - needstart + 1 <= numPlanes) then
            needend = itryend
            lastready = itry
          else
            ifenough = 1
          endif
          itry = itry + idelSlice
        enddo
        if (debug) print *,'itryend,needstart,needend,lastready', itryend, &
            needstart, needend, lastready
        if (needend == 0) call exitError &
            ('INSUFFICIENT STACK SPACE TO DO A SINGLE SLICE')
        !
        ! if some are already loaded, need to shift them down
        !
        nalready = max(0, inloadend - needstart + 1)
        if (inloadend == 0) nalready = 0
        ioffset = (needstart - inloadstr) * inPlaneSize
        do i = indLoadBase, indLoadBase + nalready * inPlaneSize-1
          array(i) = array(i + ioffset)
        enddo
        if (nalready .ne. 0 .and. debug) print *,'shifting', needstart, inloadend
        !
        ! If it is also time to load something on GPU, shift existing
        ! data if appropriate and enable copy of filtered data
        if (numGpuPlanes > 0) then
          if (loadGpuStart <= 0 .or. neededEnds(lsliceOut - indNeededBase) > &
              loadGpuEnd) call shiftGpuSetupCopy()
        endif
        !
        ! load the planes in one plane high if stretching
        !
        IBASE = indLoadBase + nalready * inPlaneSize + ipExtraSize
        lstart = needstart + nalready
        !
        if (debug) print *,'loading', lstart, needend
        if (.not. recReproj) then
          DO NV = 1, numViews
            ISTART = IBASE
            DO NL = lstart, needend, idelSlice
              ! Position to read from projection NV at record NL
              iyload = max(0, min(nyProj - 1, nl - 1))
              CALL IMPOSN(1, mapUsedView(NV) - 1, iyload)
              CALL IRDLIN(1, array(ISTART),*999)
              ! Take log if requested
              ! 3/31/04: limit values to .001 time dynamic range
              if (ifLog .ne. 0) then
                do ix = istart, istart + nxProj - 1
                  array(ix) = alog10(max(valmin, array(ix) + baseForLog))
                enddo
              else
                do ix = istart, istart + nxProj - 1
                  array(ix) = array(ix) * exposeWeight(nv)
                enddo
              endif
              !
              ! pad with taper between start and end of line
              call taperEndToStart(istart)
              !
              ISTART = ISTART + inPlaneSize
            enddo
            IBASE = IBASE + NXPRJ2
          enddo
          !
        else
          !
          ! Load reconstruction for projections
          DO NL = lstart, needend, idelSlice
            call imposn(3, nl - 1, 0)
            call irdpas(3, array(ibase), maxXload + 1 - minXload, &
                ithickReproj, minXload - 1, maxXload - 1, minYreproj - 1, &
                maxYreproj - 1, *999)
            !
            ! Undo the scaling that was used to write, and apply a
            ! scaling that will make the data close for projecting
            array(ibase:ibase + isliceSizeBP - 1) = (array(ibase:ibase + isliceSizeBP -  &
                1) / outScale - outAdd) / filterScale
            ibase = ibase + inPlaneSize
          enddo
        endif
        if (.not.recReproj .and. numSIRTiter <= 0) then
          IBASE = indLoadBase + nalready * inPlaneSize
          DO  NL = lstart, needend, idelSlice
            call transform(ibase, nl, 1)
            ibase = ibase + inPlaneSize
          enddo
        endif
        inloadstr = needstart
        inloadend = needend
        indLoadEnd = indLoadBase + inPlaneSize * ((inloadend - inloadstr) / idelSlice +  &
            1) - 1
      end if
      !
      ! Stack is full.  Now check if GPU needs to be loaded
      if (numGpuPlanes > 0) then
        if (loadGpuStart <= 0 .or. neededEnds(lsliceOut - indNeededBase) > &
            loadGpuEnd) then
          !
          ! Load as much as possible.  If the shift fails the first time
          ! it will set loadGpuStart to 0, call again to recompute for
          ! full load
          if (.not. shiftedGpuLoad) call shiftGpuSetupCopy()
          if (.not. shiftedGpuLoad) call shiftGpuSetupCopy()
          ibase = indLoadBase + (needGpuStart + keepOnGpu - inloadstr) * inPlaneSize
          if (debug) write(*,'(a,i4,a,i4,a,i4,i10)') 'Loading GPU, #', &
              numLoadGpu, '  lstart', needGpuStart + keepOnGpu, &
              ' start pos base', keepOnGpu + 1, ibase - indLoadBase
          tstart = walltime()
          if (gpuLoadProj(array(ibase), numLoadGpu, needGpuStart + keepOnGpu, &
              keepOnGpu + 1) == &
              0) then
            loadGpuStart = needGpuStart
            loadGpuEnd = needGpuEnd
          else
            loadGpuStart = 0
          endif
          if (debug) write(*,'(a,f8.4)') 'Loading time', walltime() - tstart
        endif
      endif
      !
      ISTART = indLoadBase + inPlaneSize * (lslice - inloadstr) / idelSlice
      if (.not.recReproj) then
        !
        ! Backprojection: Process all views for current slice
        ! If new-style X tilt, set  the Y center based on the slice
        ! number, and adjust the y offset slightly
        !
        if (ifAlpha < 0) ycenOut = ycenfix + (1. / cosAlpha(1) - 1.) * yOffset &
            - nint(tanalpha * (lslice - centerSlice))
        if (numSIRTiter == 0) then
          !
          ! print *,indLoadBase, lslice, inloadstr
          ! print *,'projecting', lslice, ' at', istart, ', ycen =', ycenOut
          CALL PROJECT(ISTART, lslice)
          if (maskEdges) call maskSlice(indOutSlice, ithickBP)
          !
          ! move vertical slice into ring buffer, adjust ring variables
          !
          if (ifAlpha < 0) then
            ! print *,'moving slice to ring position', nextfreevs
            ioffset = isliceSizeBP + (nextfreevs - 1) * ithickBP * iwidth
            do i = indOutSlice, indOutSlice + ithickBP * iwidth - 1
              array(i + ioffset) = array(i)
            enddo
            call manageRing(numVertNeeded, nvsinring, nextfreevs, lvsstart, &
                lvsend, lslice)
          endif
        else
          !
          ! for SIRT, either do the zero iteration here, load single slice
          ! into read-in slice spot, or decompose vertical slice from read-
          ! in slices into spot in  vertical slice ring buffer.
          ! Descale them by output scaling only
          ! Set up for starting from read-in data
          recscale = filterScale * numViews
          iset = 1
          rpfill = dmeanIn
          if (sirtFromZero) then
            !
            ! Doing zero iteration: set up location to place slice
            ibaseSIRT = ireadBase
            if (ifAlpha < 0) then
              ibaseSIRT = indOutSlice + isliceSizeBP + (nextfreevs - 1) * ithickBP * &
                  iwidth
              call manageRing(numVertNeeded, nvsinring, nextfreevs, lvsstart, &
                  lvsend, lslice)
            endif
            !
            ! Copy and filter the lines needed by filter set 1
            do iv = 1, numViews
              j = indWorkPlane + (iv - 1) * nxprj2
              k = istart + (iv - 1) * nxprj2
              do i = 0, nxprj2 - 3
                array(j + i) = array(k + i)
              enddo
            enddo
            call transform(indWorkPlane, lslice, 1)
            edgeFill = edgeFillOrig
            !
            ! Backproject and maskEdges/taper edges
            call project(indWorkPlane, lslice)
            if (maskEdges) call maskSlice(indOutSlice, ithickBP)
            array(ibaseSIRT:ibaseSIRT + isliceSizeBP - 1) = array(indOutSlice:mapEnd) / &
                recscale
            iset = 2
            if (ifOutSirtRec == 3) then
              print *,'writing zero iteration slice', lslice
              call writeInternalSlice(5, ibaseSIRT)
            endif

          else if (ifAlpha == 0) then
            !
            ! Read in single slice
            call imposn(3, lslice - 1, 0)
            call irdsec(3, array(ireadBase), *999)
            dsum = 0.
            dpix = 0.
            do i = 0, ithickOut * iwidth - 1
              array(i + ireadBase) = (array(i + ireadBase) / outScale - outAdd)
              dsum = dsum + array(i + ireadBase)
              dpix = dpix + 1.
            enddo
            if (debug) print *,'Loaded mean= ', dsum / dpix, '   pmean= ', dmeanIn
            ibaseSIRT = ireadBase
          else
            !
            ! Vertical slices: read file directly if it is vertical slices
            ibaseSIRT = indOutSlice + isliceSizeBP + (nextfreevs - 1) * ithickBP * iwidth
            if (vertSirtInput) then
              if (debug) print *,'loading vertical slice', lslice
              call imposn(3, lslice - 1, 0)
              call irdsec(3, array(ibaseSIRT), *999)
            else
              !
              ! Decompose vertical slice from read-in slices
              ! First see if necessary slices are loaded in ring
              vslcen = lslice - centerSlice
              vycenfix = (ithickBP / 2 + 0.5) - nint(tanalpha * (lslice - centerSlice)) &
                  + yOffset / cosAlpha(1)
              riBot = centerSlice + vslcen * cosAlpha(1) + (1 - vycenfix) * sinAlpha(1)
              riTop = riBot + (ithickBP - 1) * sinAlpha(1)
              lriMin = max(1, floor(min(riBot, riTop)))
              lriMax = min(nyProj, ceiling(max(riBot, riTop)))

              loadstart = lriMin
              if (lriMin >= lReadStart .and. lriMin <= lReadEnd) loadStart = lReadEnd + 1
              loadEnd = lriMax
              !
              ! Read into ring and manage the ring pointers
              if (debug .and. loadEnd >= loadStart) &
                  print *,'reading into ring', loadstart, loadEnd
              do lri = loadstart, loadEnd
                call imposn(3, lri - 1, 0)
                ioffset = ireadBase + (nextReadFree - 1) * ithickOut * iwidth
                call irdsec(3, array(ioffset), *999)
                do i = 0, ithickOut * iwidth - 1
                  array(i + ioffset) = (array(i + ioffset) / outScale - outAdd)
                enddo
                call manageRing(numReadNeed, nreadInRing, nextReadFree, lReadStart, &
                    lreadEnd, lri)
              enddo
              iringstart = 1
              if (nreadInRing == numReadNeed) iringstart = nextReadFree
              call decompose(lslice, lReadStart, lreadEnd, iringstart, ibaseSIRT)
            endif
            if (ifOutSirtRec == 4) then
              print *,'writing decomposed slice', lslice
              call writeInternalSlice(5, ibaseSIRT)
            endif
            call manageRing(numVertNeeded, nvsinring, nextfreevs, lvsstart, lvsend, &
                lslice)
          endif
          !
          ! Ready to iterate.  First get the starting slice mean
          call iclden(array(ibaseSIRT), iwidth, ithickBP, 1, iwidth, 1, &
              ithickBP, unscmin, unscmax, firstmean)
          ! It seems to give less edge artifacts using the mean all the time
          ! if (sirtFromZero) rpfill = firstmean
          rpfill = firstmean
          !
          do isirt = 1, numSIRTiter
            ierr = 1
            tstart = walltime()

            if (useGPU) then
              ierr = gpuReprojOneSlice(array(ibaseSIRT), array(indWorkPlane), &
                  sinReproj, cosReproj, ycenOut, numViews, rpfill)
            endif
            if (ierr .ne. 0) then
              do iv = 1, numViews
                i = (iv - 1) * iwidth + 1
                j = indWorkPlane + (iv - 1) * nxprj2
                ! call reproject(array(ibaseSIRT), iwidth, ithickBP, iwidth, &
                ! sinReproj(iv), cosReproj(iv), xRayStart(i), yRayStart(i), &
                ! numPixInRay(i), maxRayPixels(iv), dmeanIn, array(j), 1, 1)
                call reprojOneAngle(array(ibaseSIRT), array(j), 1, 1, 1, &
                    cosReproj(iv), -sinReproj(iv), 1., 0., cosReproj(iv), &
                    iwidth, ithickBP, iwidth * ithickBP, iwidth, 1, 1, 0., 0., &
                    iwidth / 2 + 0.5, ycenOut, iwidth / 2 + 0.5, &
                    centerSlice, 0, 0., 0., rpfill)
              enddo
            endif
            if (debug) write(*,'(a,f8.4)') 'Reproj time = ', walltime() - tstart
            if (isirt == numSIRTiter .and. ifOutSirtProj == 1) &
                call writeInternalSlice(4, indWorkPlane)
            !
            ! Subtract input projection lines from result.  No scaling
            ! needed here; these intensities should be in register
            ! Taper ends
            ! dsum = 0.
            do iv = 1, numViews
              j = indWorkPlane + (iv - 1) * nxprj2
              k = istart + (iv - 1) * nxprj2
              do i = 0, iwidth - 1
                array(j + i) = array(j + i) - array(k + i)
              enddo
              call taperEndToStart(j)
            enddo
            if (isirt == numSIRTiter .and. ifOutSirtProj == 2) &
                call writeInternalSlice(4, indWorkPlane)
            !
            ! filter the working difference lines and get a rough value for
            ! the edgeFill.  Trying to get this better did not prevent edge
            ! artifacts - the masking was needed
            call transform(indWorkPlane, lslice, iset)
            dsum = 0.
            do iv = 1, numViews
              j = indWorkPlane + (iv - 1) * nxprj2
              dsum = dsum + array(j + iwidth + numPad / 2 - 1)
            enddo
            edgeFill = dsum / numViews
            ! print *,'   (for diff bp) edgefill =', edgeFill
            !
            ! Backproject the difference
            call project(indWorkPlane, lslice)
            if (isirt == numSIRTiter .and. ifOutSirtRec == 1) then
              array(indOutSlice:mapEnd) = (array(indOutSlice:mapEnd)  + outAdd * &
                  recscale) * (outScale / recscale)
              print *,'writing bp difference', lslice
              call writeInternalSlice(5, indOutSlice)
              array(indOutSlice:mapEnd) = array(indOutSlice:mapEnd) /  &
                  (outScale / recscale) - outAdd * recscale
            endif
            !
            ! Accumulate report values
            if (iterForReport > 0) call sampleForReport(array(indOutSlice), &
                lslice, ithickBP, isirt, outScale / recscale, outAdd * recscale)
            !
            ! Subtract from input slice
            ! outScale to account for difference between ordinary scaling
            ! for output by 1 / numViews and the fact that input slice was
            ! descaled by 1/filterScale
            i = ibaseSIRT + isliceSizeBP - 1
            if (isignConstraint == 0) then
              array(ibaseSIRT:i) = array(ibaseSIRT:i) - array(indOutSlice:mapEnd) / &
                  recscale
            else if (isignConstraint < 0) then
              array(ibaseSIRT:i) = min(0., array(ibaseSIRT:i) - &
                  array(indOutSlice:mapEnd) / recscale)
            else
              array(ibaseSIRT:i) = max(0., array(ibaseSIRT:i) - &
                  array(indOutSlice:mapEnd) / recscale)
            endif
            if (maskEdges) call maskSlice(ibaseSIRT, ithickBP)
            if (isirt == numSIRTiter - 2 .and. ifOutSirtRec == 2) then
              print *,'writing internal slice', lslice
              call writeInternalSlice(5, ibaseSIRT)
            endif
            if (isirt == numSIRTiter .and. saveVertSlices .and. &
                lslice >= isliceStart .and. lslice <= isliceEnd) then
              if (debug) print *,'writing vertical slice', lslice
              ierr = parWrtSetCurrent(2)
              call iclden(array(ibaseSIRT), iwidth, ithickBP, 1, iwidth, 1, ithickBP, &
                  tmin, tmax, tmean)
              dmin6 = min(dmin6, tmin)
              dmax6 = max(dmax6, tmax)
              call parWrtSec(6, array(ibaseSIRT))
              ierr = parWrtSetCurrent(1)
            endif
            !
            ! Adjust fill value by change in mean
            ! Accumulate mean of starting vertical slices until one output
            ! slice has been composed, and set composeFill from mean
            if (isirt < numSIRTiter .or. ifAlpha < 0) then
              call iclden(array(ibaseSIRT), iwidth, ithickBP, 1, iwidth, 1, &
                  ithickBP, unscmin, unscmax, curmean)
              !
              ! Doing it the same way with restarts as with going from 0 seems
              ! to prevent low frequency artifacts
              ! if (sirtFromZero) then
              rpfill = curmean
              ! else
              ! rpfill = dmeanIn + curmean - firstmean
              ! endif
              if (isirt == numSIRTiter .and. .not. composedOne) then
                vertSum = vertSum + curmean
                numVertSum = numVertSum + 1
              endif
            endif
            if (numVertSum > 0) composeFill = vertSum / numVertSum
          enddo
        endif
      endif
    enddo
    !
    if (.not.recReproj) then
      if (ifAlpha < 0) then
        !
        ! interpolate output slice from vertical slices
        !
        iringstart = 1
        if (nvsinring == numVertNeeded) iringstart = nextfreevs
        if (nvsinring > 0) then
          !
          ! If there is anything in ring to use, then we can compose an
          ! output slice, first dumping any deferred fill slices
          call dumpFillSlices()
          ! print *,'composing', lsliceout, ' from', lvsstart, lvsend, iringstart
          call compose(lsliceout, lvsstart, lvsend, 1, iringstart, composeFill)
          composedOne = .true.
        else
          !
          ! But if there is nothing there, keep track of a range of slices
          ! that need to be filled - after the composeFill value is set
          if (lfillStart < 0) lfillStart = lsliceOut
          lfillEnd = lsliceOut
        endif
      endif
      !
      ! Dump slice unless there are fill slices being held
      if (lfillStart < 0) call dumpUpdateHeader(lsliceOut)
      lsliceOut = lsliceOut + idelSlice
    else
      !
      ! REPROJECT all ready slices to minimize file mangling
      lsProjEnd = lastReady
      call reprojectRec(lsliceOut, lsProjEnd, inloadstr, inloadend, DMIN, &
          DMAX, DTOT8)
      lsliceOut = lsProjEnd + 1
    endif
  enddo
  !
  ! End of main loop
  !-----------------
  !
  call dumpFillSlices()
  ! Close files
  CALL IMCLOSE(1)
  pixelTot = float(NSLICE) * iwidth * ithickOut
  if (reprojBP .or. recReproj) pixelTot = float(NSLICE) * iwidth * numReproj
  DMEAN = DTOT8 / pixelTot
  !
  ! get numbers used to compute scaling report, and then fix min/max if
  ! necessary
  unscmin = dmin / outScale - outAdd
  unscmax = dmax / outScale - outAdd
  truncations = .false.
  extremes = .false.
  if (newMode == 0) then
    truncations = dmin < 0. .or. dmax > 256.
    dmin = max(0., dmin)
    dmax = min(255., dmax)
  else if (newMode == 1) then
    extremes = useGPU .and. max(abs(dmin), abs(dmax)) > effectiveScale * 3.e5
    truncations = dmin < -32768. .or. dmax > 32768.
    dmin = max(-32768., dmin)
    dmax = min(32767., dmax)
  endif
  if (minTotSlice <= 0) then
    if (perpendicular .and. interhsave > 0 .and. .not.(reprojBP .or. recReproj)) then
      nxyztmp(3) = nslice
      call ialsiz(2, nxyztmp, nxyzst)
    endif
    CALL IWRHDR(2, title, 1, DMIN, DMAX, DMEAN)
    if (.not.(reprojBP .or. recReproj)) then
      WRITE(6, 930) 'reconstruction'
    else
      WRITE(6, 930) 'reprojection'
    endif
    CALL IRDHDR(2, nxyztmp, nxyzst, MODE, DMIN, DMAX, DMEAN)
    if (saveVertSlices) call iwrhdr(6, title, 1, dmin6, dmax6, (dmin6 + dmax6) / 2.)
  else
    write(*,'(a,3g15.7,f15.0)') 'Min, max, mean, # pixels=', dmin, dmax, dmean, pixelTot
  endif
  CALL IMCLOSE(2)
  if (saveVertSlices) call imclose(6)
  if (.not.(reprojBP .or. recReproj .or. numSIRTiter > 0)) then
    recscale = numViews * 235. / (unscmax - unscmin)
    recflevl = (10. *(unscmax - unscmin) / 235. -unscmin) / numViews
    write(6, 905) 'bytes (10-245)', recflevl, recscale
    recscale = numViews * 30000. / (unscmax - unscmin)
    recflevl = (-15000. *(unscmax - unscmin) / 30000. -unscmin) / numViews
    write(6, 905) '-15000 to 15000', recflevl, recscale
    WRITE(6, 910) NSLICE
  endif
  if (extremes) write(6, 911)
  if (truncations .and. .not. extremes) write(6, 912)
  if (useGPU) call gpuDone()
  if (iterForReport > 0) then
    do i = 1, max(1, numSIRTiter)
      if (reportVals(3, i) > 0) write(6, 908) i + iterForReport - 1, &
          isliceStart, isliceEnd, reportVals(1, i) / reportVals(3, i), &
          reportVals(2, i) / reportVals(3, i)
    enddo
  endif
  if (ifOutSirtProj > 0) then
    call iwrhdr(4, title, 1, dmin4, dmax4, (dmin4 + dmax4) / 2.)
    call imclose(4)
  endif
  if (ifOutSirtRec > 0) then
    call iwrhdr(5, title, 1, dmin5, dmax5, (dmin5 + dmax5) / 2.)
    call imclose(5)
  endif
  call exit(0)
999 WRITE(6, 920) mapUsedView(NV), nL
  call exit(1)
  !
  !
900 FORMAT(//,' STACK LOADING' &
      /,' -------------' &
      //,' Total stack size           ',I11,/ &
      /,1x,a20,'         ',I9/,a,//, &
      ' Output slice                 ',I9,/)
901 format(' Stretching buffer            ',I9,/)
902 format(1x,i4,  ' Untilted slices         ',I9,/)
903 format(1X,I4,' Transposed projections  ',I9,/)
904 format(1X,I4,' Slice(s) for SIRT       ',I9,/)
907 format(' Reprojection lines for SIRT  ',i9,/)
905 format(/,' To scale output to ',a,', use SCALE to add',f12.3, &
      ' and scale by',f12.5)
908 format(/,'Iter ', i4.3, ', slices', 2i6, ', diff rec mean&sd:', 2f15.3)
910 FORMAT(//' Reconstruction of',I5,' slices complete.', &
      //,1X,78('-'))
911 format(/,'WARNING: TILT - EXTREMELY LARGE VALUES OCCURRED AND VALUES ', &
      'WERE TRUNCATED WHEN OUTPUT TO FILE; THERE COULD BE ERRORS IN GPU ', &
      'COMPUTATION.  RUN gputilttest',/)
912 format(/,'WARNING: TILT - SOME VALUES WERE TRUNCATED WHEN OUTPUT TO THE', &
      ' FILE; CHECK THE OUTPUT SCALING FACTOR',/)
920 FORMAT(//' ERROR: TILT -  reading in view',I3,' for slice' &
      ,I5,/)
930 FORMAT(//' Header on ',a,' file' / &
      ' --------------------------------'//)

CONTAINS
  !
  ! Determine parameters for loading GPU and make call to shift existing
  ! data if any is to be retained
  subroutine shiftGpuSetupCopy()
    integer*4 gpuShiftProj
    needGpuStart = neededStarts(lsliceOut - indNeededBase)
    needGpuEnd = min(needGpuStart + numGpuPlanes - 1,  needend)
    keepOnGpu = 0
    if (loadGpuStart > 0) keepOnGpu = loadGpuEnd + 1 - needGpuStart
    numLoadGpu = needGpuEnd + 1 - needGpuStart - keepOnGpu
    if (debug) write(*,'(a,i4,a,i4,a,i4)') 'Shifting GPU, #', numLoadGpu, &
        '  lstart', needGpuStart + keepOnGpu, ' start pos', keepOnGpu + 1
    tstart = walltime()
    if (gpuShiftProj(numLoadGpu, needGpuStart + keepOnGpu, keepOnGpu + 1) &
        == 0) then
      shiftedGpuLoad = .true.
    else
      loadGpuStart = 0
    endif
    if (debug) write(*,'(a,f8.4)') 'Shifting time', walltime() - tstart
    return
  end subroutine shiftGpuSetupCopy

  ! For writing two kinds of test output
  !
  subroutine writeInternalSlice(isUnit, iwriteStart)
    integer*4 isUnit, iwriteStart
    real*4 istmin, istmax, istmean
    if (isUnit == 4) then
      do iv = 1, numViews
        j = iwriteStart + (iv - 1) * nxprj2
        call imposn(4, iv - 1, lslice - isliceStart)
        call iwrlin(4, array(j))
      enddo
      call iclden(array(iwriteStart), nxprj2, iv, 1, iwidth, 1, numViews, &
          istmin, istmax, istmean)
      dmin4 = min(dmin4, istmin)
      dmax4 = max(dmax4, istmax)
    else
      call iwrsec(5, array(iwriteStart))
      call iclden(array(iwriteStart), iwidth, ithickBP, 1, iwidth, 1, ithickBP, &
          istmin, istmax, istmean)
      dmin5 = min(dmin5, istmin)
      dmax5 = max(dmax5, istmax)
      nz5 = nz5 + 1
      call ialsiz_sam_cel(5, iwidth, ithickBP, nz5)
    endif
    return
  end subroutine writeInternalSlice

  ! Dump a slice and update the header periodically if appropriate
  !
  subroutine dumpUpdateHeader(lsliceDump)
    integer*4 lsliceDump
    !
    ! Write out current slice
    CALL DUMP(LSLICEDump, DMIN, DMAX, DTOT8)
    ! DNM 10/22/03:  Can't use flush in Windows/Intel because of sample.com
    ! call flush(6)
    !
    ! write out header periodically, restore writing position
    if (perpendicular .and. interhsave > 0 .and. .not.reprojBP .and. &
        minTotSlice <= 0) then
      nsliceout = nsliceout + 1
      nxyztmp(1) = iwidth
      nxyztmp(2) = ithickOut
      nxyztmp(3) = nsliceout
      if (mod(nsliceout, interhsave) == 1) then
        call ialsiz(2, nxyztmp, nxyzst)
        DMEAN = DTOT8 / (float(NSLICEout) * iwidth * ithickOut)
        CALL IWRHDR(2, title, -1, DMIN, DMAX, DMEAN)
        call parWrtPosn(2, nsliceout, 0)
      endif
    endif
  end subroutine dumpUpdateHeader

  ! If there are fill slices that haven't been output yet, dump them now
  ! and turn off signal that they exist
  subroutine dumpFillSlices()
    if (lfillStart >= 0) then
      do i = lfillStart, lfillEnd, idelSlice
        array(indOutSlice:indOutSlice + iwidth * ithickOut - 1) = composeFill
        call dumpUpdateHeader(i)
      enddo
      lfillStart = -1
    endif
  end subroutine dumpFillSlices

  ! END OF MAIN PROGRAM UNIT
END program tilt


! Taper intensities across pad region from end of line to start
!
subroutine taperEndToStart(istart)
  use tiltvars
  implicit none
  real*4 xsum, stmean, endmean, f
  integer*4 ipad, nsum, ix, istart
  !
  nsum = 0
  xsum = 0.
  do ix = istart, istart + min(2, nxProj - 1)
    nsum = nsum + 1
    xsum = xsum + array(ix)
  enddo
  stmean = xsum / nsum
  if (nsum == 0) print *,'stmean bogus'
  nsum = 0
  xsum = 0.
  do ix = istart + max(0, nxProj - 3), istart + nxProj - 1
    nsum = nsum + 1
    xsum = xsum + array(ix)
  enddo
  if (nsum == 0) print *,'ndmean bogus'
  endmean = xsum / nsum
  do ipad = 1, numPad
    f = ipad / (numPad + 1.)
    array(istart + nxProj + ipad-1) = f * stmean + (1. -f) * endmean
  enddo
  return
end subroutine taperEndToStart

subroutine manageRing(numVertNeeded, nvsinring, nextfreevs, lvsstart, &
    lvsend, lslice)
  implicit none
  integer*4 numVertNeeded, nvsinring, nextfreevs, lvsstart, lvsend, lslice
  if (nvsinring < numVertNeeded) then
    if (nvsinring == 0) lvsstart = lslice
    nvsinring = nvsinring + 1
  else
    lvsstart = lvsstart + 1
  endif
  lvsend = lslice
  nextfreevs = nextfreevs + 1
  if (nextfreevs > numVertNeeded) nextfreevs = 1
  return
end subroutine manageRing



! --------------------------------------------------------------------
SUBROUTINE RADWT(IRMAXin, IFALLin, ifilterSet)
  ! -----------------------------
  !
  ! Set Radial Transform weighting
  ! Linear ramp plus Gaussian fall off
  use tiltvars
  implicit none
  integer*4 IRMAXin, IFALLin, ifilterSet
  integer*4 nxprj2, IEND, irmax, ifall, iv, iw, ibase, i
  real*4 stretch, avgint, atten, sumint, wsum, z, arg, sirtFrac
  real*4 diffmin, diff, attensum
  real*4, allocatable :: wgtAtten(:)
  !
  allocate(wgtAtten(limView), stat = i)
  call memoryError(i, 'ARRAY FOR WEIGHTS PER VIEW')
  sirtFrac = 0.99
  nxprj2 = nxProj + 2 + numPad
  IEND = NXPRJ2 / 2
  stretch = float(nxProj + numPad) / nxProj
  irmax = nint(irmaxin * stretch)
  ifall = nint(ifallin * stretch)
  avgint = 1.
  attensum = 0.
  zeroWeight = 0.
  if (numTiltIncWgt > 0 .and. numWgtAngles > 1) then
    avgint = (wgtAngles(numWgtAngles) - wgtAngles(1)) / (numWgtAngles - 1)
    if (debug) write(6, 401)
401 format(/' View  Angle Weighting')
  endif
  !
  ! Set up the attenuations for the weighting angles
  do iv = 1, numWgtAngles
    atten = 1.
    if (numTiltIncWgt > 0. .and. numWgtAngles > 1) then
      sumint = 0
      wsum = 0.
      do iw = 1, numTiltIncWgt
        if (iv - iw > 0) then
          wsum = wsum + tiltIncWgts(iw)
          sumint = sumint + tiltIncWgts(iw) * (wgtAngles(iv + 1 - iw) - &
              wgtAngles(iv - iw))
        endif
        if (iv + iw <= numViews) then
          wsum = wsum + tiltIncWgts(iw)
          sumint = sumint + tiltIncWgts(iw) * (wgtAngles(iv + iw) - &
              wgtAngles(iv + iw - 1))
        endif
      enddo
      atten = atten * (sumint / wsum) / avgint
    endif
    wgtAtten(iv) = atten
  enddo
  !
  ! Set up linear ramp
  do iv = 1, numViews
    !
    ! Get weighting from nearest weighting angle
    atten = 1.
    if (numTiltIncWgt > 0 .and. numWgtAngles > 1) then
      diffmin = 1.e10
      do iw = 1, numWgtAngles
        diff = abs(angles(iv) - wgtAngles(iw))
        if (diff < diffmin) then
          diffmin = diff
          atten = wgtAtten(iw)
        endif
      enddo
      if (debug) write(6, 402) iv, angles(iv), atten
402   format(i4,f8.2,f10.5)
    endif
    !
    ! Take negative if subtracting
    if (numViewSubtract > 0) then
      do i = 1, numViewSubtract
        if (iviewSubtract(i) == 0 .or. mapUsedView(iv) == iviewSubtract(i)) &
            atten = -atten
      enddo
    endif
    !
    attensum = attensum + atten
    ibase = (iv - 1 + (ifilterSet - 1) * numViews) * nxprj2
    DO  I = 1, min(IRMAX, iend)
      ! This was the basic filter
      ! ARRAY(ibase+2*I-1) =atten*(I-1)
      ! This is the mixture of the basic filter and a flat filter with
      ! a scaling that would give approximately the same output magnitude
      array(ibase + 2 * I - 1) = atten * ((1. -flatFrac) * (I - 1) + flatFrac * &
          filterScale)
      !
      ! This 0.2 is what Kak and Slaney's weighting function gives at 0
      if (i == 1) array(ibase + 2 * I - 1) = atten * 0.2
      !
      ! This is the SIRT filter, which divides the error equally among
      ! the pixels on a ray.
      if (flatFrac > 1) array(ibase + 2 * I - 1) = sirtFrac * atten * &
          filterScale / (ithickBP / cosBeta(iv))
      !
      ! And just the value and compute the mean zero weighting
      array(ibase + 2 * I) = array(ibase + 2 * I - 1)
      if (i == 1) zeroWeight = zeroWeight + array(ibase + 2 * I - 1) / numViews
    enddo
  enddo
  if (debug) print *,'Mean weighting factor', attensum / numViews
  !
  ! Set up Gaussian
  DO I = IRMAX + 1, IEND
    ARG = FLOAT(I - IRMAX) / FLOAT(IFALL)
    atten = EXP(-ARG * ARG)
    ibase = 0
    do iv = 1, numViews
      Z = atten * array(ibase + 2 * irmax)
      array(ibase + 2 * I - 1) = z
      array(ibase + 2 * I) = Z
      ibase = ibase + nxprj2
    enddo
  enddo
  RETURN
END SUBROUTINE RADWT
!
!
! ---------------------------------------------------------------------
SUBROUTINE MASKPREP(lslice)
  ! ----------------
  !
  ! This subroutine prepares the limits of the slice width to be computed
  ! if masking is used
  use tiltvars
  implicit none
  real*4 radlft, radrt, y, yy, ycenuse
  integer*4 i, ixlft, ixrt, lslice
  !
  ! Compute left and right edges of unmasked area
  if (maskEdges) then
    !
    ! Adjust the Y center for alpha tilt (already adjusted for ifAlpha < 0)
    ycenuse = ycenOut
    if (ifAlpha > 0) ycenuse = ycenOut + (1. / cosAlpha(1) - 1.) * yOffset &
        - nint((lslice - centerSlice) * sinAlpha(1) / cosAlpha(1))
    !
    ! Get square of radius of arcs of edge of input data from input center
    radlft = (xcenIn + axisXoffset - 1)**2
    radrt = (nxProj - xcenIn - axisXoffset)**2
    DO I = 1, ithickBP
      Y = I - YCENuse
      YY = min(Y * Y, radlft)
      !
      ! get distance of X coordinate from center, subtract from or add to
      ! center and round up on left, down on right, plus added maskEdges pixels
      ixlft = xcenOut + 1. -sqrt(radlft - yy)
      ixUnmaskedSE(1, i) = max(1, ixlft + numExtraMaskPix)
      YY = min(Y * Y, radrt)
      ixrt = xcenOut + sqrt(radrt - yy)
      ixUnmaskedSE(2, i) = min(iwidth, ixrt - numExtraMaskPix)
    enddo
    !-------------------------------------------------
    ! If no maskEdges
  ELSE
    DO I = 1, ithickBP
      ixUnmaskedSE(1, i) = 1
      ixUnmaskedSE(2, i) = iwidth
    enddo
  END IF
  RETURN
END SUBROUTINE MASKPREP
!
!
! ---------------------------------------------------------------------
SUBROUTINE TRANSFORM(ibase, lslice, ifilterSet)
  ! ----------------------------
  !
  ! This subroutine applies a one-dimensional Fourier transform to
  ! all views corresponding to a given slice, applies the radial
  ! weighting function and then applies an inverse Fourier transform.
  !
  use tiltvars
  implicit none
  integer*4 nxprj2, istart, index, indrad, nv, i, ibfrom, ibto, ixp
  integer*4 ixpp1, ixpm1, ixpp2, ibase, lslice, ifilterSet
  real*4 x, xp, dx, dxm1, v4, v5, v6, a, c, dennew, dxdxm1, diffmax
  real*4 fx1, fx2, fx3, fx4
  real*8 walltime, tstart
  integer*4 gpuFilterLines

  NXPRJ2 = nxProj + 2 + numPad
  istart = ibase + ipExtraSize
  tstart = walltime()
  index = 1
  if (useGPU) then
    index = gpuFilterLines(array(ISTART), lslice, ifilterSet)
  endif
  if (index .ne. 0) then
    !
    ! Apply forward Fourier transform
    CALL ODFFT(array(ISTART), nxProj + numPad, numViews, 0)
    !
    ! Apply Radial weighting
    INDEX = ISTART
    indrad = 1 + (ifilterSet - 1) * NXPRJ2 * numViews
    v4 = 0.
    v5 = 0.
    DO  NV = 1, numViews
      DO I = 1, NXPRJ2
        v4 = v4 + array(INDEX)
        v5 = v5 + array(indrad)
        array(INDEX) = array(INDEX) * array(indrad)
        indrad = indrad + 1
        INDEX = INDEX + 1
      enddo
    enddo
    !
    ! Apply inverse transform
    CALL ODFFT(array(ISTART), nxProj + numPad, numViews, 1)
  endif
  if (debug) write(*,'(a,f8.4)') 'Filter time', walltime() - tstart
  if (ipExtraSize == 0) return
  !
  ! do cosine stretch and move down one plane
  ! Use cubic interpolation a la cubinterp
  !
  ! print *,'istart, ibase', istart, ibase
  do nv = 1, numViews
    ibfrom = istart + (nv - 1) * nxprj2 - 1
    ibto = ibase + indStretchLine(nv) - 1
    ! print *,nv, ibfrom, ibto, nxStretched(nv)
    diffmax = 0.
    if (interpOrdStretch == 1) then
      !
      ! linear interpolation
      !
      do i = 1, nxStretched(nv)
        x = i / float(interpFacStretch) + stretchOffset(nv)
        xp = min(max(1., x * cosBeta(nv)), float(nxProj))
        IXP = XP
        DX = XP - IXP
        ixp = ixp + ibfrom
        IXPP1 = min(IXP + 1, nxProj + ibfrom)
        dxm1 = dx - 1.
        array(ibto + i) = -dxm1 * array(ixp) + dx * array(ixpp1)
      enddo
    else if (interpOrdStretch == 2) then
      !
      ! quadratic
      !
      do i = 1, nxStretched(nv)
        x = i / float(interpFacStretch) + stretchOffset(nv)
        xp = min(max(1., x * cosBeta(nv)), float(nxProj))
        IXP = nint(XP)
        DX = XP - IXP
        ixp = ixp + ibfrom
        IXPP1 = min(IXP + 1, nxProj + ibfrom)
        IXPM1 = max(IXP - 1, 1 + ibfrom)
        V4 = array(IXPM1)
        V5 = array(IXP)
        V6 = array(IXPP1)
        !
        A = (V6 + V4) * .5 - V5
        C = (V6 - V4) * .5
        !
        dennew = A * DX * DX + C * DX + V5
        ! dennew=min(dennew, max(v4, v5, v6))
        ! dennew=max(dennew, min(v4, v5, v6))
        array(ibto + i) = dennew
      enddo
    else
      !
      ! cubic
      !
      do i = 1, nxStretched(nv)
        x = i / float(interpFacStretch) + stretchOffset(nv)
        xp = min(max(1., x * cosBeta(nv)), float(nxProj))
        IXP = XP
        DX = XP - IXP
        ixp = ixp + ibfrom
        IXPP1 = min(IXP + 1, nxProj + ibfrom)
        IXPM1 = max(IXP - 1, 1 + ibfrom)
        ixpp2 = min(ixp + 2, nxProj + ibfrom)

        dxm1 = dx - 1.
        dxdxm1 = dx * dxm1
        fx1 = -dxm1 * dxdxm1
        fx4 = dx * dxdxm1
        fx2 = 1 + dx**2 * (dx - 2.)
        fx3 = dx * (1. -dxdxm1)
        dennew = fx1 * array(ixpm1) + fx2 * array(ixp) + &
            fx3 * array(ixpp1) + fx4 * array(ixpp2)
        ! dennew=min(dennew, max(array(ixpm1), array(ixp), array(ixpp1), &
        ! array(ixpp2)))
        ! dennew=max(dennew, min(array(ixpm1), array(ixp), array(ixpp1), &
        ! array(ixpp2)))
        array(ibto + i) = dennew
      enddo
    endif
  enddo

  RETURN
END SUBROUTINE TRANSFORM


! ---------------------------------------------------------------------
SUBROUTINE PROJECT(ISTART, lslice)
  ! --------------------------
  !
  ! This subroutine assembles one reconstructed slice perpendicular
  ! to the tilt axis, using a back projection method.
  !
  use tiltvars
  implicit none
  integer*4 jstrt(3), jend(3)
  real*8 xproj8, tstart
  integer*4 nxprj2, ipdel, IPOINT, iv, INDEX, i, j
  real*4 CBETA, SBETA, zz, zpart, yy, yproj, YFRAC, omyfrac
  integer*4 jPROJ, jlft, jrt, iproj, ip1, ip2, ind, ipbase, ifytest
  integer*4 jtstlft, jtstrt, ISTART, lslice, jregion
  real*4 xlft, xrt, x, xfrac, omxfrac, zbot, ztop, xproj, ytol
  integer*4 gpubpnox, gpubpxtilt, gpubplocal
  real*8 walltime
  !
  ! A note on the ubiquitous ytol: It is needed to keep artifacts from
  ! building up at ends of data set through SIRT, from reprojection of the
  ! line between real data and fill, or backprojection of an edge in the
  ! projection difference.  2.05 was sufficient for X-axis tilt cases but
  ! 3.05 was needed for local alignments, thus it is set to 3.05 everywhere
  ! (Here, reprojection routines, and in GPU routines)
  ytol = 3.05
  !
  ! Determine maskEdges extent if it is variable
  if (ifAlpha .ne. 0 .and. maskEdges) call maskprep(lslice)
  NXPRJ2 = nxProj + 2 + numPad
  tstart = walltime()
  !
  ! GPU backprojection
  if (useGPU) then
    ind = 1
    if (ifAlpha <= 0 .and. nxWarp == 0) then
      ind = gpubpnox(array(indOutSlice), array(istart), sinBeta, cosBeta, nxProj, &
          xcenIn + axisXoffset, xcenOut, ycenOut, edgeFill)
    else if (nxWarp == 0 .and. loadGpuStart > 0) then
      ind = gpubpxtilt(array(indOutSlice), sinBeta, cosBeta, sinAlpha, cosAlpha, xzfac, &
          yzfac, nxProj, nyProj, xcenIn + axisXoffset, xcenOut, ycenOut, lslice, &
          centerSlice, edgeFill)
    else if (loadGpuStart > 0) then
      ind = gpubplocal(array(indOutSlice), lslice, nxWarp, nyWarp, ixStartWarp, &
          iyStartWarp, idelXwarp, idelYwarp, nxProj, xcenOut, xcenIn, axisXoffset, &
          ycenOut, centerSlice, edgeFill)
    endif
    if (ind == 0) then
      if (debug) write(*, '(a,f8.4)') 'GPU backprojection time', &
          walltime() - tstart
      return
    endif
  endif
  !
  ! CPU backprojection: clear out the slice
  DO I = 0, isliceSizeBP - 1
    array(indOutSlice + I) = 0.
  enddo
  ipdel = idelSlice * inPlaneSize
  !
  if (nxWarp == 0) then
    !
    ! Loop over all views
    IPOINT = ISTART - 1
    DO iv = 1, numViews
      !
      ! Set view angle
      CBETA = cosBeta(iv)
      SBETA = sinBeta(iv)
      !
      ! Loop over all points in output slice
      INDEX = indOutSlice
      !
      DO I = 1, ithickBP
        ZZ = (I - ycenOut) * compress(iv)
        if (ifAlpha <= 0) then
          zPART = zz * SBETA + xcenIn + axisXoffset
        else
          !
          ! If x-axis tilting, find interpolation factor between the
          ! slices
          !
          yy = lslice - centerSlice
          zpart = yy * sinAlpha(iv) * sbeta + zz * (cosAlpha(iv) * sbeta + xzfac(iv)) + &
              xcenIn + axisXoffset
          yproj = yy * cosAlpha(iv) - zz * (sinAlpha(iv) - yzfac(iv)) + centerSlice
          !
          ! if inside the tolerance, clamp it to the endpoints
          if (yproj >= 1. - ytol .and. yproj <= nyProj + ytol) &
              yproj = max(1., min(float(nyProj), yproj))
          jPROJ = YPROJ
          jproj = min(nyProj - 1, jproj)
          YFRAC = YPROJ - JPROJ
          omyfrac = 1. -yfrac
        endif
        !
        ! compute left and right limits that come from legal data
        !
        x = cbeta
        if (abs(cbeta) < 0.001) x = sign(0.001, cbeta)
        xlft = (1. -zpart) / x + xcenOut
        xrt = (nxProj - zpart) / x + xcenOut
        if (xrt < xlft) then
          x = xlft
          xlft = xrt
          xrt = x
        endif
        jlft = xlft
        if (jlft < xlft) jlft = jlft + 1
        jlft = max(jlft, ixUnmaskedSE(1, i))
        jrt = xrt
        if (jrt == xrt) jrt = jrt - 1
        jrt = min(jrt, ixUnmaskedSE(2, i))
        !
        ! If the limits are now crossed, just skip to full fill at end
        if (jlft <= jrt) then
          !
          ! set up starting index and projection position
          !
          do ind = index + ixUnmaskedSE(1, i) - 1, index + jlft - 2
            array(ind) = array(ind) + edgeFill
          enddo
          index = index + (jlft - 1)
          x = jlft - xcenOut
          if (interpFacStretch .ne. 0) then
            !
            ! Computation with prestretched data
            !
            XPROJ8 = interpFacStretch * (zPART / CBETA + X - stretchOffset(iv))
            IPROJ = XPROJ8
            XFRAC = XPROJ8 - IPROJ
            iproj = iproj + ipoint + indStretchLine(iv)
            omxfrac = 1. -xfrac
            if (ifAlpha <= 0) then
              !
              ! interpolation in simple case of no x-axis tilt
              !
              DO ind = index, index + jrt - jlft
                array(IND) = array(IND) + &
                    omxfrac * array(IPROJ) + XFRAC * array(IPROJ + 1)
                iproj = iproj + interpFacStretch
              enddo
              index = index + jrt + 1 - jlft
            else
              !
              ! If x-axis tilting, interpolate from two lines
              !
              ip1 = iproj + (jproj - lslice) * ipdel
              ip2 = ip1 + ipdel
              if (yproj >= 1. .and. yproj <= nyProj .and. &
                  ip1 >= indLoadBase .and. ip2 >= indLoadBase .and. ip1 < indLoadEnd &
                  .and. ip2 < indLoadEnd) then

                DO ind = index, index + jrt - jlft
                  array(IND) = array(IND) + &
                      omxfrac * (omyfrac * array(IP1) + yFRAC * array(IP2)) + &
                      xfrac * (omyfrac * array(IP1 + 1) + yFRAC * array(IP2 + 1))
                  ip1 = ip1 + interpFacStretch
                  ip2 = ip2 + interpFacStretch
                enddo
              else
                do ind = index, index + jrt + 1 - jlft - 1
                  array(ind) = array(ind) + edgeFill
                enddo
              endif
              index = index + jrt + 1 - jlft
            endif
          else
            !
            ! Computation direct from projection data
            !
            XPROJ8 = zPART + X * CBETA
            if (ifAlpha <= 0) then
              !
              ! interpolation in simple case of no x-axis tilt
              !
              call bpsumnox(array, index, ipoint, jrt + 1 - jlft, xproj8, cbeta)
            else
              !
              ! If x-axis tilting
              !
              IPROJ = XPROJ8
              ipbase = ipoint + (jproj - lslice) * ipdel
              ip1 = ipbase + iproj
              ip2 = ip1 + ipdel
              if (yproj >= 1. .and. yproj <= nyProj .and. &
                  ip1 >= indLoadBase .and. ip2 >= indLoadBase .and. ip1 < indLoadEnd &
                  .and. ip2 < indLoadEnd) then

                call bpsumxtilt(array, index, ipbase, ipdel, jrt + 1 - jlft, &
                    xproj8, cbeta, yfrac, omyfrac)
              else
                do ind = index, index + jrt + 1 - jlft - 1
                  array(ind) = array(ind) + edgeFill
                enddo
                index = index + jrt + 1 - jlft
              endif
            endif
          endif
        else
          jrt = 0
        endif
        do ind = index, index + iwidth - jrt - 1
          array(ind) = array(ind) + edgeFill
        enddo
        index = index + iwidth - jrt
      enddo
      !
      !-------------------------------------------
      !
      ! End of projection loop
      if (interpFacStretch == 0) IPOINT = IPOINT + NXPRJ2
    enddo
  else
    !
    ! LOCAL ALIGNMENTS
    !
    ! Loop over all views
    IPOINT = ISTART - 1
    DO IV = 1, numViews
      !
      ! precompute the factors for getting xproj and yproj all the
      ! way across the slice
      !
      ifytest = 0
      zbot = (1 - ycenOut) * compress(iv)
      ztop = (ithickBP - ycenOut) * compress(iv)
      DO J = 1, iwidth
        !
        ! get the fixed and z-dependent component of the
        ! projection coordinates

        call localProjFactors(j, lslice, iv, xprojfs(j),  xprojzs(j), &
            yprojfs(j), yprojzs(j))
        !
        ! see if any y testing is needed in the inner loop by checking
        ! yproj at top and bottom in Z
        !
        yproj = yprojfs(j) + yprojzs(j) * zbot
        jPROJ = YPROJ
        ip1 = ipoint + (jproj - lslice) * ipdel + 1
        ip2 = ip1 + ipdel
        if (ip1 <= indLoadBase .or. ip2 <= indLoadBase .or. ip1 >= indLoadEnd &
            .or. ip2 >= indLoadEnd .or. jproj < 1 .or. jproj >= nyProj) &
            ifytest = 1
        yproj = yprojfs(j) + yprojzs(j) * ztop
        jPROJ = YPROJ
        ip1 = ipoint + (jproj - lslice) * ipdel + 1
        ip2 = ip1 + ipdel
        if (ip1 <= indLoadBase .or. ip2 <= indLoadBase .or. ip1 >= indLoadEnd &
            .or. ip2 >= indLoadEnd .or. jproj < 1 .or. jproj >= nyProj) &
            ifytest = 1
      enddo
      !
      ! walk in from each end until xproj is safely within bounds
      ! to define region where no x checking is needed
      !
      jtstlft = 0
      j = 1
      do while(jtstlft == 0 .and. j < iwidth)
        if (min(xprojfs(j) + zbot * xprojzs(j), &
            xprojfs(j) + ztop * xprojzs(j)) >= 1) jtstlft = j
        j = j + 1
      enddo
      if (jtstlft == 0) jtstlft = iwidth
      !
      jtstrt = 0
      j = iwidth
      do while(jtstrt == 0 .and. j > 1)
        if (max(xprojfs(j) + zbot * xprojzs(j), &
            xprojfs(j) + ztop * xprojzs(j)) < nxProj) jtstrt = j
        j = j - 1
      enddo
      if (jtstrt == 0) jtstrt = 1
      if (jtstrt < jtstlft) then
        jtstrt = iwidth / 2
        jtstlft = jtstrt + 1
      endif
      !
      INDEX = indOutSlice
      !
      ! loop over the slice, outer loop on z levels
      !
      DO I = 1, ithickBP
        ZZ = (I - ycenOut) * compress(iv)
        jlft = max(jtstlft, ixUnmaskedSE(1, i))
        jrt = min(jtstrt, ixUnmaskedSE(2, i))
        index = index + ixUnmaskedSE(1, i) - 1
        !
        ! set up to do inner loop in three regions of X
        !
        jstrt(1) = ixUnmaskedSE(1, i)
        jend(1) = jlft - 1
        jstrt(2) = jlft
        jend(2) = jrt
        jstrt(3) = jrt + 1
        jend(3) = ixUnmaskedSE(2, i)
        do jregion = 1, 3
          if (jregion .ne. 2 .or. ifytest == 1) then
            !
            ! loop involving full testing - either left or right
            ! sides needing x testing, or anywhere if y testing
            ! needed
            !
            do j = jstrt(jregion), jend(jregion)
              xproj = xprojfs(j) + zz * xprojzs(j)
              yproj = yprojfs(j) + zz * yprojzs(j)
              if (yproj >= 1. - ytol .and. yproj <= nyProj + ytol) &
                  yproj = max(1., min(float(nyProj), yproj))
              if (xproj >= 1 .and. xproj <= nxProj .and. &
                  yproj >= 1. .and. yproj <= nyProj) then
                !
                IPROJ = XPROJ
                iproj = min(nxProj - 1, iproj)
                XFRAC = XPROJ - IPROJ
                jPROJ = YPROJ
                jproj = min(nyProj - 1, jproj)
                YFRAC = YPROJ - JPROJ
                !
                ip1 = ipoint + (jproj - lslice) * ipdel + iproj
                ip2 = ip1 + ipdel
                if (ip1 >= indLoadBase .and. ip2 >= indLoadBase .and. &
                    ip1 < indLoadEnd .and. ip2 < indLoadEnd) then
                  array(INDEX) = array(INDEX) + &
                      (1. -yfrac) * ((1. -XFRAC) * array(IP1) &
                      + XFRAC * array(IP1 + 1)) + &
                      yfrac * ((1. -XFRAC) * array(IP2) &
                      + XFRAC * array(IP2 + 1))
                else
                  array(INDEX) = array(INDEX) + edgeFill
                endif
              else
                array(INDEX) = array(INDEX) + edgeFill
              endif
              index = index + 1
            enddo
            !
            ! loop for no x-testing and no y testing
            !
          else
            call bpsumlocal(array, index, zz, xprojfs, xprojzs, yprojfs, &
                yprojzs, ipoint, ipdel, lslice, jstrt(jregion), &
                jend(jregion))
          endif
        enddo
        index = index + iwidth - ixUnmaskedSE(2, i)
      enddo
      !-------------------------------------------
      !
      ! End of projection loop
      IPOINT = IPOINT + NXPRJ2
    enddo
  endif
  if (debug) write(*, '(a,f8.4)') 'CPU backprojection time', &
      walltime() - tstart
  RETURN
END SUBROUTINE PROJECT
!
!-------------------------------------------------------------------------
!
! COMPOSE will interpolate the output slice LSLICEOUT from vertical
! slices in the ring buffer, where LVSSTART and LVSEND are the starting
! and ending slices in the ring buffer, IDIR is the direction of
! reconstruction, and IRINGSTART is the position of LVSSTART in the
! ring buffer.
!
subroutine compose(lsliceout, lvsstart, lvsend, idir, iringstart, composeFill)
  use tiltvars
  implicit none
  integer*4 lsliceout, lvsstart, lvsend, idir, iringstart
  real*4 composeFill
  integer*4 ind1(4), ind2(4), ind3(4), ind4(4)
  real*4 tanalpha, vertcen, cenj, cenl, vsl, vycen, fx, vy, fy, f22, f23, f32, f33
  integer*4 ivsl, ifmiss, i, lvsl, iring, ibase, ivy, indcen, jnd5, jnd2, j, k
  real*4 fx1, fx2, fx3, fx4, fy1, fy2, fy3, fy4, v1, v2, v3, v4, f5, f2, f8, f4, f6
  integer*4 jnd8, jnd4, jnd6, nfill
  !
  ! 12/12/09: stopped reading base here, read on output; eliminate zeroing
  !
  tanalpha = sinAlpha(1) / cosAlpha(1)
  vertcen = ithickBP / 2 + 0.5
  fx = 0.
  fy = 0.
  nfill = 0
  !
  ! loop on lines of data
  !
  do j = 1, ithickOut
    cenj = j - (ithickOut / 2 + 0.5) - yOffset
    cenl = lsliceout - centerSlice
    !
    ! calculate slice number and y position in vertical slices
    !
    vsl = cenl * cosAlpha(1) - cenj * sinAlpha(1) + centerSlice
    vycen = cenl * sinAlpha(1) + cenj * cosAlpha(1)
    ivsl = vsl
    fx = vsl - ivsl
    ifmiss = 0
    !
    ! for each of 4 slices needed for cubic interpolation, initialize
    ! data indexes at zero then see if slice exists in ring
    !
    do i = 1, 4
      ind1(i) = 0
      ind2(i) = 0
      ind3(i) = 0
      ind4(i) = 0
      lvsl = ivsl + i - 2
      if (idir * (lvsl - lvsstart) >= 0 .and. idir * (lvsend - lvsl) >= 0) then
        !
        ! if slice exists, get base index for the slice, compute the
        ! y index in the slice, then set the 4 data indexes if they
        ! are within the slice
        !
        iring = idir * (lvsl - lvsstart) + iringstart
        if (iring > numVertNeeded) iring = iring - numVertNeeded
        ibase = indOutSlice + isliceSizeBP + (iring - 1) * ithickBP * iwidth
        vy = vycen + vertcen - nint(tanalpha * (lvsl - centerSlice)) +  &
            yOffset / cosAlpha(1)
        ivy = vy
        fy = vy - ivy
        if (ivy - 1 >= 1 .and. ivy - 1 <= ithickBP) ind1(i) = ibase + iwidth * (ivy - 2)
        if (ivy >= 1 .and. ivy <= ithickBP) ind2(i) = ibase + iwidth * (ivy - 1)
        if (ivy + 1 >= 1 .and. ivy + 1 <= ithickBP) ind3(i) = ibase + iwidth * ivy
        if (ivy + 2 >= 1 .and. ivy + 2 <= ithickBP) ind4(i) = ibase + iwidth * (ivy + 1)
      endif
      if (ind1(i) == 0 .or. ind2(i) == 0 .or. ind3(i) == 0 .or. &
          ind4(i) == 0) ifmiss = 1
    enddo
    ibase = indOutSlice + (j - 1) * iwidth - 1
    if (interpOrdXtilt > 2 .and. ifmiss == 0) then
      !
      ! cubic interpolation if selected, and no data missing
      !
      fx1 = 2. *fx**2 - fx**3 - fx
      fx2 = fx**3 - 2. *fx**2 + 1
      fx3 = fx**2 + fx - fx**3
      fx4 = fx**3 - fx**2
      fy1 = 2. *fy**2 - fy**3 - fy
      fy2 = fy**3 - 2. *fy**2 + 1
      fy3 = fy**2 + fy - fy**3
      fy4 = fy**3 - fy**2
      do i = 1, iwidth
        v1 = fx1 * array(ind1(1)) + fx2 * array(ind1(2)) + &
            fx3 * array(ind1(3)) + fx4 * array(ind1(4))
        v2 = fx1 * array(ind2(1)) + fx2 * array(ind2(2)) + &
            fx3 * array(ind2(3)) + fx4 * array(ind2(4))
        v3 = fx1 * array(ind3(1)) + fx2 * array(ind3(2)) + &
            fx3 * array(ind3(3)) + fx4 * array(ind3(4))
        v4 = fx1 * array(ind4(1)) + fx2 * array(ind4(2)) + &
            fx3 * array(ind4(3)) + fx4 * array(ind4(4))
        array(ibase + i) = fy1 * v1 + fy2 * v2 + fy3 * v3 + fy4 * v4
        do k = 1, 4
          ind1(k) = ind1(k) + 1
          ind2(k) = ind2(k) + 1
          ind3(k) = ind3(k) + 1
          ind4(k) = ind4(k) + 1
        enddo
      enddo
    elseif (interpOrdXtilt == 2 .and. ifmiss == 0) then
      !
      ! quadratic interpolation if selected, and no data missing
      ! shift to next column or row if fractions > 0.5
      !
      indcen = 2
      if (fx > 0.5) then
        indcen = 3
        fx = fx - 1.
      endif
      if (fy <= 0.5) then
        jnd5 = ind2(indcen)
        jnd2 = ind1(indcen)
        jnd8 = ind3(indcen)
        jnd4 = ind2(indcen - 1)
        jnd6 = ind2(indcen + 1)
      else
        fy = fy - 1.
        jnd5 = ind3(indcen)
        jnd2 = ind2(indcen)
        jnd8 = ind4(indcen)
        jnd4 = ind3(indcen - 1)
        jnd6 = ind3(indcen + 1)
      endif
      !
      ! get coefficients and do the interpolation
      !
      f5 = 1. -fx**2 - fy**2
      f2 = (fy**2 - fy) / 2.
      f8 = f2 + fy
      f4 = (fx**2 - fx) / 2.
      f6 = f4 + fx
      do i = 1, iwidth
        array(ibase + i) = f5 * array(jnd5) + f2 * array(jnd2) + &
            f4 * array(jnd4) + f6 * array(jnd6) + f8 * array(jnd8)
        jnd5 = jnd5 + 1
        jnd2 = jnd2 + 1
        jnd4 = jnd4 + 1
        jnd6 = jnd6 + 1
        jnd8 = jnd8 + 1
      enddo
    else
      !
      ! linear interpolation
      !
      ! print *,j, ind2(2), ind2(3), ind3(2), ind3(3)
      if (ind2(2) == 0 .or. ind2(3) == 0 .or. ind3(2) == 0 .or. &
          ind3(3) == 0) then
        !
        ! if there is a problem, see if it can be rescued by shifting
        ! center back to left or below
        !
        if (fx < 0.02 .and. ind2(1) .ne. 0 .and. ind3(1) .ne. 0 .and. &
            ind2(2) .ne. 0 .and. ind3(2) .ne. 0) then
          fx = fx + 1
          ind2(3) = ind2(2)
          ind2(2) = ind2(1)
          ind3(3) = ind3(2)
          ind3(2) = ind3(1)
        elseif (fy < 0.02 .and. ind1(2) .ne. 0 .and. ind1(3) .ne. 0 .and. &
            ind2(2) .ne. 0 .and. ind3(2) .ne. 0) then
          fy = fy + 1
          ind3(2) = ind2(2)
          ind2(2) = ind1(2)
          ind3(3) = ind2(3)
          ind2(3) = ind1(3)
        endif
      endif
      !
      ! do linear interpolation if conditions are right, otherwise fill
      !
      if (ind2(2) .ne. 0 .and. ind2(3) .ne. 0 .and. ind3(2) .ne. 0 .and. &
          ind3(3) .ne. 0) then
        f22 = (1. -fy) * (1. -fx)
        f23 = (1. -fy) * fx
        f32 = fy * (1. -fx)
        f33 = fy * fx
        do i = 1, iwidth
          array(ibase + i) = f22 * array(ind2(2)) + &
              f23 * array(ind2(3)) + f32 * array(ind3(2)) + f33 * array(ind3(3))
          ind2(2) = ind2(2) + 1
          ind2(3) = ind2(3) + 1
          ind3(2) = ind3(2) + 1
          ind3(3) = ind3(3) + 1
        enddo
      else
        ! print *,'filling', j
        do i = 1, iwidth
          array(i + ibase) = composeFill
        enddo
        nfill = nfill + 1
      endif
    endif
  enddo
  ! if (nfill .ne. 0) print *,nfill, ' lines filled, edgefill =', composeFill
  return
end subroutine compose

!
! DECOMPOSE will interpolate a vertical slice LSLICE from input slices
! in the read-in ring buffer, where lReadStart and lReadEnd are the
! starting and ending slices in the ring buffer, IRINGSTART is the
! position of lReadStart in the ring buffer, and ibaseSIRT is the index
! in stack at which to place the slice.
!
subroutine decompose(lslice, lReadStart, lreadEnd, iringstart, &
    ibaseSIRT)
  use tiltvars
  implicit none
  integer*4 lslice, lReadStart, lreadEnd, iringstart, ibaseSIRT
  real*4 tanalpha, outcen, cenl, vslcen, vycen, outsl, outj, fx, fy
  real*4 f11, f12, f21, f22
  integer*4 ibasev, ibase1, ibase2, j, ioutsl, jout, i, iring

  tanalpha = sinAlpha(1) / cosAlpha(1)
  outcen = ithickOut / 2 + 0.5
  cenl = lslice - centerSlice
  vslcen = cenl
  !
  ! loop on lines of data
  do j = 1, ithickBP
    ibasev = ibaseSIRT + (j - 1) * iwidth
    !
    ! calculate slice number and y position in input slices
    !
    vycen = j - (ithickBP / 2 + 0.5 - nint(tanalpha * cenl) + yOffset / cosAlpha(1))
    outsl = centerSlice + vslcen * cosAlpha(1) + vycen * sinAlpha(1)
    outj = outcen + yOffset - vslcen * sinAlpha(1) + vycen * cosAlpha(1)
    ! print *,j, vycen, outsl, outj
    ! if (outsl >= lreadStart - 0.5 .and. outsl <= lreadEnd + 0.5 &
    ! .and. outj >= 0.5 .and.outj <= ithickOut + 0.5) then
    !
    ! For a legal position, get interpolation integers and fractions,
    ! adjust if within half pixel of end
    ioutsl = outsl
    fx = outsl - ioutsl
    if (ioutsl < lreadStart) then
      ioutsl = lreadStart
      fx = 0.
    else if (ioutsl > lreadEnd - 1) then
      ioutsl = lreadEnd - 1
      fx = 1.
    endif
    jout = outj
    fy = outj - jout
    if (jout < 1) then
      jout = 1
      fy = 0.
    else if (jout > ithickOut - 1) then
      jout = ithickOut - 1
      fy = 1.
    endif
    !
    ! Get slice indexes in ring
    iring = ioutsl - lreadStart + iringstart
    if (iring > numReadNeed) iring = iring - numReadNeed
    ibase1 = ireadBase + (iring - 1) * ithickOut * iwidth + &
        (jout - 1) * iwidth
    iring = ioutsl + 1 - lreadStart + iringstart
    if (iring > numReadNeed) iring = iring - numReadNeed
    ibase2 = ireadBase + (iring - 1) * ithickOut * iwidth + &
        (jout - 1) * iwidth
    !
    ! Interpolate line
    f11 = (1. -fy) * (1. -fx)
    f12 = (1. -fy) * fx
    f21 = fy * (1. -fx)
    f22 = fy * fx
    do i = 0, iwidth - 1
      array(ibasev + i) = f11 * array(ibase1 + i) + f12 * array(ibase2 + i) + &
          f21 * array(ibase1 + i + iwidth) + f22 * array(ibase2 + i + iwidth)
    enddo
    ! else
    !
    ! Otherwise fill line
    ! do i = 0, iwidth - 1
    ! array(ibasev+i) = dmeanIn
    ! enddo
    ! endif
  enddo
  return
end subroutine decompose

!
!-------------------------------------------------------------------------
SUBROUTINE DUMP(LSLICE, DMIN, DMAX, DTOT8)
  ! --------------------------------------
  !
  use tiltvars
  implicit none
  integer*4 lslice, nparextra, iend, index, i, j, imapOut
  real*4 DMIN, DMAX, fill
  real*8 dtot8, dtmp8
  !
  ! If adding to a base rec, read in each line and add scaled values
  imapOut = indOutSlice
  if (numSIRTiter > 0 .and. ifAlpha >= 0) imapOut = ireadBase
  if (readBaseRec) then
    index = imapOut
    call imposn(3, lslice - 1, 0)
    if (iterForReport > 0) call sampleForReport(array(imapOut), &
        lslice, ithickOut, 1, outScale, outAdd)
    do j = 1, ithickOut
      call irdlin(3, projLine)
      if (recSubtraction) then
        !
        ! SIRT subtraction from base with possible sign constraints
        if (isignConstraint == 0) then
          array(index:index + iwidth - 1) = projLine(1:iwidth) / outScale - outAdd - &
              array(index:index + iwidth - 1)
        else if (isignConstraint < 0) then
          array(index:index + iwidth - 1) = min(0., projLine(1:iwidth) / outScale - &
              outAdd - array(index:index + iwidth - 1))
        else
          array(index:index + iwidth - 1) = max(0., projLine(1:iwidth) / outScale - &
              outAdd - array(index:index + iwidth - 1))
        endif
      else
        !
        ! Generic addition of the scaled base data
        array(index:index + iwidth - 1) = array(index:index + iwidth - 1) + &
            projLine(1:iwidth) / baseOutScale - baseOutAdd
      endif
      index = index + iwidth
    enddo
  endif
  !
  nparextra = 100
  IEND = IMAPOUT + ithickOut * iwidth - 1
  !
  ! outScale
  ! DNM simplified and fixed bug in getting min/max/mean
  dtmp8 = 0.
  !
  ! DNM 9/23/04: incorporate reprojBP option
  !
  if (reprojBP) then
    !--------------outScale
    DO I = IMAPOUT, IEND
      array(I) = (array(I) + outAdd) * outScale
    enddo
    !
    ! Fill value assumes edge fill value
    !
    fill = (edgeFill + outAdd) * outScale
    do j = 1, numReproj
      i = (j - 1) * iwidth + 1
      call reproject(array(imapOut), iwidth, ithickBP, iwidth, sinReproj(j), &
          cosReproj(j), xRayStart(i), yRayStart(i), &
          numPixInRay(i), maxRayPixels(j), fill, projLine, 0, 0)
      do i = 1, iwidth
        DMIN = AMIN1(projLine(I), DMIN)
        DMAX = AMAX1(projLine(I), DMAX)
        DTmp8 = DTmp8 + projLine(I)
      enddo
      i = (lslice - isliceStart) / idelSlice
      if (minTotSlice > 0) i = lslice - minTotSlice
      call parWrtPosn(2, j - 1, i)
      call parWrtLin(2, projLine)
    enddo
    dtot8 = dtot8 + dtmp8
    return
  endif
  !
  !--------------outScale and get min / max / sum
  DO I = IMAPOUT, IEND
    array(I) = (array(I) + outAdd) * outScale
    DMIN = AMIN1(array(I), DMIN)
    DMAX = AMAX1(array(I), DMAX)
    DTmp8 = DTmp8 + array(I)
    !
  enddo
  dtot8 = dtot8 + dtmp8
  !
  ! Dump slice
  if (perpendicular) then
    ! ....slices correspond to sections of map
    CALL parWrtSEC(2, array(IMAPOUT))
  ELSE
    ! ....slices must be properly stored
    ! Take each line of array and place it in the correct section
    ! of the map.
    INDEX = IMAPOUT
    DO J = 1, ithickOut
      CALL IMPOSN(2, J - 1, (LSLICE - isliceStart) / idelSlice)
      CALL IWRLIN(2, array(INDEX))
      !
      ! DNM 2/29/01: partially demangle the parallel output by writing
      ! up to 100 lines at a time in this plane
      !
      if (mod((LSLICE - isliceStart) / idelSlice, nparextra) == 0) then
        do i = 1, min(nparextra - 1, (isliceEnd - lslice) / idelSlice)
          CALL IWRLIN(2, array(INDEX))
        enddo
      endif
      INDEX = INDEX + iwidth
    END DO
  END IF
  !
  RETURN
END SUBROUTINE DUMP

! maskEdges out (blur) the edges of the slice at the given index
!
subroutine maskSlice(ibaseMask, ithickMask)
  use tiltvars
  implicit none
  integer*4 ibaseMask, ithickMask, i, j, idir, limit, lr, nsum, nsmooth
  integer*4 ntaper, index, ibase
  real*4 sum, edgeMean, frac
  !
  nsmooth = 10
  ntaper = 10
  idir = -1
  limit = 1
  do lr = 1, 2
    !
    ! find mean along edge
    sum = 0.
    do j = 1, ithickMask
      sum = sum + array(ibaseMask + (j - 1) * iwidth + ixUnmaskedSE(lr, j) - 1)
    enddo
    edgeMean = sum / ithickMask
    !
    ! For each line, sum progressively more pixels along edge out to a
    ! limit
    do j = 1, ithickMask
      ibase = ibaseMask + (j - 1) * iwidth - 1
      index = ixUnmaskedSE(lr, j) + idir
      sum = array(ibase + ixUnmaskedSE(lr, j))
      nsum = 1
      do i = 1, nsmooth
        if (idir * (limit - index) < 0) exit
        if (j + i <= ithickMask) then
          nsum = nsum + 1
          sum = sum + array(ibase + i * iwidth + ixUnmaskedSE(lr, j + i))
        endif
        if (j - i >= 1) then
          nsum = nsum + 1
          sum = sum + array(ibase - i * iwidth + ixUnmaskedSE(lr, j - i))
        endif
        !
        ! Taper partway to mean over this smoothing distance
        frac = i / (ntaper + nsmooth + 1.)
        array(ibase + index) = (1. - frac) * sum / nsum + frac * edgeMean
        index = index + idir
      enddo
      !
      ! Then taper rest of way down to mean over more pixels, then fill
      ! with mean
      do i = 1, ntaper
        if (idir * (limit - index) < 0) exit
        frac = (i + nsmooth) / (ntaper + nsmooth + 1.)
        array(ibase + index) = (1. - frac) * sum / nsum + frac * edgeMean
        index = index + idir
      enddo
      do i = index, limit, idir
        array(ibase + i) = edgeMean
      enddo
    enddo
    !
    ! Set up for other direction
    limit = iwidth
    idir = 1
  enddo
  return
end subroutine maskSlice

! The input routine, gets input and sets up almost all parameters
! -----------------------------------------------------------------
SUBROUTINE INPUT()
  ! ----------------

  use tiltvars
  implicit none
  integer limnum
  parameter (limnum = 100)
  !
  integer*4 MPXYZ(3), NOXYZ(3), nrxyz(3), nsxyz(3), maxNeeds(limnum)
  real*4 outilt(3), cell(6), dtor
  data outilt/90., 0., 0./
  data cell/0., 0., 0., 90., 90., 90./
  DATA DTOR/0.0174532/
  CHARACTER DAT * 9, TIM * 8
  real*4 delta(3)
  character*80 titlech
  !
  Character*1024 card
  CHARACTER*320 FILIN, FILOUT, recfile, basefile, boundfile, vertBoundFile, vertOutFile
  integer*4 nfields, inum(limnum)
  real*4 XNUM(limnum)
  !
  integer*4, allocatable :: ivexcl(:), ivreprj(:)
  real*4, allocatable :: packLocal(:,:), angReproj(:)
  integer*4 mode, newangles, iftiltfile, nvuse, nvexcl, numNeedEval
  real*4 delang, compfac, globalpha, xoffset, scalelocal, rrmax, rfall, xoffAdj
  integer*4 irmax, ifall, ncompress, nxfull, nyfull, ixsubset, iysubset
  integer*4 kti, indbase, ipos, idtype, lens
  integer*4 nd1, nd2, nv, nslice, indi, i, iex, nvorig, iv
  real*4 vd1, vd2, dtheta, theta, thetanv
  integer*4 nxprj2, ninp, nexclist, j, ind, nument
  integer*4 npadtmp, nprpad, ifZfac, localZfacs
  integer*4 ifThickIn, ifSliceIn, ifWidthIn, imageBinned, ifSubsetIn, ierr
  real*4 pixelLocal, dmint, dmaxt, dmeant, frac, origx, origy, origz
  real*4 gpuMemoryFrac, gpuMemory
  integer*4 nViewsReproj, iwideReproj, k, ind1, ind2, ifExpWeight
  integer*4 minMemory, nGPU, iactGpuFailOption, iactGpuFailEnviron
  integer*4 ifGpuByEnviron, indDelta, ifexit
  logical*4 adjustOrigin, projModel, readw_or_imod
  integer*4 niceframe, parWrtInitialize, gpuAvailable, imodGetEnv, parWrtSetCurrent
  integer*4 gpuAllocArrays, allocateArray, gpuLoadLocals, gpuLoadFilter
  real*8 wallTime, wallstart
  !
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean, PipGetLogical, PipGetTwoFloats
  integer*4 PipGetString, PipGetFloat, PipGetTwoIntegers, PipGetFloatArray
  integer*4 PipGetInOutFile, PipGetIntegerArray, PipNumberOfEntries
  integer*4 PipGetThreeFloats
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  tilt
  !
  integer numOptions
  parameter (numOptions = 62)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputProjections:FN:@output:OutputFile:FN:@'// &
      'recfile:RecFileToReproject:FN:@:ProjectModel:FN:@'// &
      ':BaseRecFile:FN:@:ActionIfGPUFails:IP:@:AdjustOrigin:B:@'// &
      ':ANGLES:FAM:@:BaseNumViews:I:@:BoundaryInfoFile:FN:@'// &
      ':COMPFRACTION:F:@:COMPRESS:FAM:@:ConstrainSign:I:@:COSINTERP:IA:@'// &
      ':DENSWEIGHT:FA:@:DONE:B:@:EXCLUDELIST2:LIM:@'// &
      ':FlatFilterFraction:F:@:FBPINTERP:I:@:FULLIMAGE:IP:@'// &
      ':IMAGEBINNED:I:@:INCLUDE:LIM:@:LOCALFILE:FN:@:LOCALSCALE:F:@'// &
      ':LOG:F:@:MASK:I:@:MinMaxMean:IT:@:MODE:I:@:OFFSET:FA:@'// &
      ':PARALLEL:B:@:PERPENDICULAR:B:@:RADIAL:FP:@:REPLICATE:FPM:@'// &
      ':REPROJECT:FAM:@:SCALE:FP:@:SHIFT:FA:@:SIRTIterations:I:@'// &
      ':SIRTSubtraction:B:@:SLICE:FA:@:StartingIteration:I:@'// &
      ':SUBSETSTART:IP:@:SubtractFromBase:LI:@:THICKNESS:I:@'// &
      ':TILTFILE:FN:@:TITLE:CH:@:TOTALSLICES:IP:@:UseGPU:I:@'// &
      ':ViewsToReproject:LI:@:WeightAngleFile:FN:@:WeightFile:FN:@'// &
      ':WIDTH:I:@:XAXISTILT:F:@xminmax:XMinAndMaxReproj:IP:@'// &
      ':XTILTFILE:FN:@:XTILTINTERP:I:@yminmax:YMinAndMaxReproj:IP:@'// &
      ':ZFACTORFILE:FN:@zminmax:ZMinAndMaxReproj:IP:@'// &
      'debug:DebugOutput:B:@internal:InternalSIRTSlices:IP:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  recReproj = .false.
  nViewsReproj = 0
  useGPU = .false.
  nGPU = -1;
  numGpuPlanes = 0
  numSIRTiter = 0
  sirtFromZero = .false.
  recSubtraction = .false.
  projSubtraction = .false.
  vertSirtInput = .false.
  saveVertSlices = .false.
  flatFrac = 0.
  iterForReport = 0
  !
  ! Minimum array size to allocate, desired number of slices to allocate
  ! for if it exceeds that minimum size
  minMemory = 20000000
  numNeedEval = 10
  gpuMemoryFrac = 0.8
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipSetSpecialFlags(1, 1, 1, 2, 0)
  call PipReadOrParseOptions(options, numOptions, 'tilt', &
      'ERROR: TILT - ', .false., 0, 1, 1, numOptArg, numNonOptArg)

  if (PipGetInOutFile('InputProjections', 1, ' ', filin) .ne. 0) &
      call exitError('NO INPUT FILE WITH PROJECTIONS SPECIFIED')
  if (PipGetInOutFile('OutputFile', 2, ' ', filout) .ne. 0) &
      call exitError('NO OUTPUT FILE SPECIFIED')
  !
  ! Allocate array a little bit for temp use
  limReproj = 100000
  allocate(array(limReproj), angReproj(limReproj), stat = ierr)
  call memoryError(ierr, 'SMALL TEMPORARY ARRAYS')
  ierr = PipGetLogical('DebugOutput', debug)
  !
  ! Open input projection file
  CALL IMOPEN(1, FILIN, 'RO')
  CALL IRDHDR(1, nprojXyz, MPXYZ, MODE, dminIn, dmaxIn, dmeanIn)
  call irtdat(1, idtype, lens, nd1, nd2, vd1, vd2)
  numViews = nprojXyz(3)
  limView = numViews + 10
  allocate(sinBeta(limView), cosBeta(limView), sinAlpha(limView), cosAlpha(limView),  &
      alpha(limView), angles(limView), xzfac(limView), yzfac(limView),  &
      compress(limView), nxStretched(limView), indStretchLine(limView),  &
      stretchOffset(limView), exposeWeight(limView), mapUsedView(limView), &
      iviewSubtract(limView), wgtAngles(limView), ivexcl(limView), &
      ivreprj(limView), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR VARIABLES PER VIEW')
  !
  ! The approximate implicit scaling caused by the default radial filter
  filterScale = nprojXyz(1) / 2.2
  !
  newangles = 0
  iftiltfile = 0
  !
  ! Get model file to project
  projModel = PipGetString('ProjectModel', recfile) == 0
  if (projModel .and. .not.readw_or_imod(recfile)) call exitError( &
      'READING MODEL FILE TO REPROJECT')
  !
  ! Get entries for reprojection from rec file
  ierr = PipGetInteger('SIRTIterations', numSIRTiter)
  if (PipGetString('RecFileToReproject', recfile) == 0) then
    if (projModel) call exitError( &
        'YOU CANNOT USE -RecFileToReproject with -ProjectModel')
    projMean = dmeanIn
    call IMOPEN(3, recfile, 'RO')
    CALL IRDHDR(3, NRXYZ, MPXYZ, MODE, dminIn, dmaxIn, dmeanIn)
    if (numSIRTiter <= 0) then
      recReproj = .true.
      minXreproj = 0
      minYreproj = 0
      minZreproj = 0
      maxXreproj = nrxyz(1) - 1
      maxYreproj = nrxyz(2) - 1
      maxZreproj = nrxyz(3) - 1
      ierr = PipGetTwoIntegers('XMinAndMaxReproj', minXreproj, maxXreproj)
      ierr = PipGetTwoIntegers('YMinAndMaxReproj', minYreproj, maxYreproj)
      ierr = PipGetTwoIntegers('ZMinAndMaxReproj', minZreproj, maxZreproj)
      if (minXreproj < 0 .or. minYreproj < 0 .or. maxXreproj >= &
          nrxyz(1) .or. maxYreproj >= nrxyz(2)) call exitError( &
          'Min or Max X, or Y coordinate to project is out of range')
      if (PipGetString('ViewsToReproj', card) == 0) then
        call parselist(card, ivreprj, nViewsReproj)
        if (nViewsReproj > limView) call exitError( &
            'TOO MANY VIEWS IN LIST TO REPROJECT FOR ARRAYS')
      endif
      ierr = PipGetLogical('SIRTSubtraction', projSubtraction)
    endif
    minXreproj = minXreproj + 1
    maxXreproj = maxXreproj + 1
    minYreproj = minYreproj + 1
    maxYreproj = maxYreproj + 1
    minZreproj = minZreproj + 1
    maxZreproj = maxZreproj + 1
    !
    ! If not reading from a rec and doing sirt, then must be doing from 0
  else if (numSIRTiter  > 0) then
    sirtFromZero = .true.
    flatFrac = 1.
  endif

  if (.not. recReproj .and. .not.projModel .and. numSIRTiter <= 0 .and. &
      PipGetString('BaseRecFile', basefile) == 0) then
    readBaseRec = .true.
    if (PipGetString('SubtractFromBase', card) == 0) then
      call parselist(card, iviewSubtract, numViewSubtract)
      if (numViewSubtract > limView) call exitError( &
          'TOO MANY VIEWS IN LIST TO SUBTRACT FOR ARRAYS')
      if (numViewSubtract == 1 .and. iviewSubtract(1) < 0) then
        recSubtraction = .true.
        numViewSubtract = 0
      endif
    endif
    if (.not. recSubtraction .and. &
        PipGetInteger('BaseNumViews', numViewBase) .ne. 0) call exitError( &
        'YOU MUST ENTER -BaseNumViews with -BaseRecFile')
    call imopen(3, basefile, 'RO')
    call irdhdr(3, NRXYZ, MPXYZ, MODE, dmint, dmaxt, dmeant)
  endif
  !
  !-------------------------------------------------------------
  ! Set up defaults:
  !
  !...... Default slice is all rows in a projection plane
  isliceStart = 1
  isliceEnd = nprojXyz(2)
  idelSlice = 1
  !...... and has the same number of columns.
  iwidth = nprojXyz(1)
  !...... Default is no maskEdges and extra pixels to maskEdges is 0
  maskEdges = .FALSE.
  numExtraMaskPix = 0
  !...... Default is no scaling of output map
  outAdd = 0.
  outScale = 1.
  !...... Default is no offset or rotation
  DELANG = 0.
  !...... Default is output mode 2
  newMode = 2
  !...... Start with no list of views to use or exclude
  nvuse = 0
  nvexcl = 0
  !...... Default is no logarithms
  ifLog = 0
  !...... Default radial weighting parameters - no filtering
  irmax = nprojXyz(1) / 2 + 1
  ifall = 0
  !...... Default overall and individual compression of 1; no alpha tilt
  ncompress = 0
  compfac = 1.
  do nv = 1, numViews
    compress(nv) = 1.
    alpha(nv) = 0.
    xzfac(nv) = 0.
    yzfac(nv) = 0.
    exposeWeight(nv) = 1.
  enddo
  ifAlpha = 0
  globalpha = 0.
  ifZfac = 0
  !
  !...... Default weighting by density of adjacent views
  numTiltIncWgt = 2
  do i = 1, numTiltIncWgt
    tiltIncWgts(i) = 1. / (i - 0.5)
  enddo
  numWgtAngles = 0
  !
  xoffset = 0
  yOffset = 0
  axisXoffset = 0.
  nxWarp = 0
  nyWarp = 0
  scalelocal = 0.
  nxfull = 0
  nyfull = 0
  ixsubset = 0
  iysubset = 0
  ithickBP = 10
  imageBinned = 1
  ifThickIn = 0
  ifWidthIn = 0
  ifSliceIn = 0
  ifSubsetIn = 0
  ifExpWeight = 0
  minTotSlice = -1
  maxTotSlice = -1
  !
  !...... Default double - width linear interpolation in cosine stretching
  interpFacStretch = 2
  interpOrdStretch = 1
  interpOrdXtilt = 1
  perpendicular = .true.
  reprojBP = .false.
  numReproj = 0
  adjustorigin = .false.
  numViewSubtract = 0
  iactGpuFailOption = 0
  iactGpuFailEnviron = 0
  ifOutSirtProj = 0
  ifOutSirtRec = 0
  isignConstraint = 0
  vertOutFile = ' '
  vertBoundFile = ' '
  !
  !...... Default title
  CALL b3dDATE(DAT)
  CALL TIME(TIM)
  !
  write(titlech, 49) 'Tomographic reconstruction', dat, tim
  if (recReproj) write(titlech, 49) 'Reprojection from tomogram', dat, tim
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
  !
  !

  if (PipGetString('TITLE', card) == 0) then
    write(titlech, 49) CARD(1:50), DAT, TIM
    read(titlech, '(20a4)') (title(kti), kti = 1, 20)
    WRITE(6, 101) title
  endif
  !
  nfields = 0
  if (PipGetIntegerArray('SLICE', inum, nfields, limnum) == 0) then
    if (NFIELDS / 2 .NE. 1) &
        call exitError('Wrong number of fields on SLICE line')
    isliceStart = INUM(1) + 1
    isliceEnd = INUM(2) + 1
    if (nfields > 2) idelSlice = inum(3)
    if (idelSlice <= 0) call exitError( &
        'Negative slice increments are not allowed')
    ifSliceIn = 1
  endif
  !
  if (PipGetInteger('THICKNESS', ithickBP) == 0) ifThickIn = 1
  !
  if (PipGetInteger('MASK', numExtraMaskPix) == 0) then
    maskEdges = .TRUE.
    numExtraMaskPix = max(-nprojXyz(1) / 50, min(nprojXyz(1) / 50, numExtraMaskPix))
    WRITE(6, 401) numExtraMaskPix
  endif
  !

  if (PipGetTwoFloats('RADIAL', rrmax, rfall) == 0) then
    irmax = rrmax
    ifall = rfall
    if (irmax == 0) irmax = nprojXyz(1) * rrmax
    if (ifall == 0) ifall = nprojXyz(1) * rfall
    WRITE(6, 501) IRMAX, IFALL
  endif
  !
  nfields = 0
  if (PipGetFloatArray('OFFSET', xnum, nfields, limnum) == 0) then
    if (NFIELDS == 0 .OR. NFIELDS >= 3) &
        call exitError('Wrong number of fields on OFFSET line')
    if (NFIELDS == 2) axisXoffset = XNUM(2)
    DELANG = XNUM(1)
  endif
  !
  if (PipGetTwoFloats('SCALE', outAdd, outScale) == 0) &
      WRITE(6, 701) outAdd, outScale
  !
  if (PipGetLogical('PERPENDICULAR', perpendicular) == 0) &
      WRITE(6, 801)
  !
  i = 0
  if (PipGetBoolean('PARALLEL', i) == 0) then
    if (i .ne. 0) then
      perpendicular = .FALSE.
      WRITE(6, 901)
    endif
  endif
  !
  if (PipGetInteger('MODE', newMode) == 0) then
    if (newMode < 0 .or. newMode > 15 .or. (newMode > 2 .and. newMode < 9)) &
        call exitError('Illegal output mode')
    write(6, 1001) newMode
  endif
  !
  ierr = PipNumberOfEntries('INCLUDE', nument)
  do j = 1, nument
    ierr = PipGetString('INCLUDE', card)
    call parselist(card, mapUsedView(nvuse + 1), nexclist)
    if (nvuse + nexclist > limView) call exitError( &
        'TOO MANY INCLUDED VIEWS FOR ARRAYS')
    do i = nvuse + 1, nvuse + nexclist
      if (mapUsedView(i) < 1 .or. mapUsedView(i) > numViews) call exitError( &
          'Illegal view number in INCLUDE list')
    enddo
    nvuse = nvuse + nexclist
  enddo
  !
  ierr = PipNumberOfEntries('EXCLUDELIST2', nument)
  do j = 1, nument
    ierr = PipGetString('EXCLUDELIST2', card)
    call parselist(card, ivexcl(nvexcl + 1), nexclist)
    if (nvexcl + nexclist > limView) call exitError( &
        'TOO MANY EXCLUDED VIEWS FOR ARRAYS')
    do i = nvexcl + 1, nvexcl + nexclist
      if (ivexcl(i) < 1 .or. ivexcl(i) > numViews) call exitError( &
          'Illegal view number in EXCLUDE list')
    enddo
    nvexcl = nvexcl + nexclist
  enddo
  !
  if (nvuse > 0 .and. nvexcl > 0) call exitError( &
      'Illegal to have both INCLUDE and EXCLUDE entries')
  !
  if (PipGetFloat('LOG', baseForLog) == 0) then
    ifLog = 1
    write(6, 1301) baseForLog
  endif
  !
  ! Removed replications
  !
  ierr = PipNumberOfEntries('ANGLES', nument)
  do j = 1, nument
    nfields = 0
    ierr = PipGetFloatArray('ANGLES',  angles(newangles + 1), nfields, &
        limView - newangles)
    newangles = newangles + nfields
  enddo
  !
  ierr = PipNumberOfEntries('COMPRESS', nument)
  do j = 1, nument
    nfields = 0
    ierr = PipGetFloatArray('COMPRESS',  angles(ncompress + 1), nfields, &
        limView - ncompress)
    ncompress = ncompress + nfields
  enddo
  !
  if (PipGetFloat('COMPFRACTION', compfac) == 0) &
      write(6, 1701) compfac
  !
  nfields = 0
  if (PipGetFloatArray('DENSWEIGHT', xnum, nfields, limnum) == 0) then
    numTiltIncWgt = nint(xnum(1))
    if (numTiltIncWgt > 0) then
      do i = 1, numTiltIncWgt
        tiltIncWgts(i) = 1. / (i - 0.5)
      enddo
      if (nfields == numTiltIncWgt + 1) then
        do i = 1, numTiltIncWgt
          tiltIncWgts(i) = xnum(i + 1)
        enddo
      elseif (nfields .ne. 1) then
        call exitError('Wrong number of fields on DENSWEIGHT line')
      endif
      write(6, 1801) numTiltIncWgt, (tiltIncWgts(i), i = 1, numTiltIncWgt)
    else
      write(6, 1802)
    endif
  endif
  !
  if (PipGetString('TILTFILE', card) == 0) then
    call dopen(3, card, 'ro', 'f')
    read(3,*,err = 2411, end = 2411) (angles(i), i = 1, numViews)
    close(3)
    iftiltfile = 1
  endif
  !
  if (PipGetInteger('WIDTH', iwidth) == 0)  ifWidthIn = 1
  !
  nfields = 0
  if (PipGetFloatArray('SHIFT', xnum, nfields, limnum) == 0) then
    if ((nfields + 1) / 2 .ne. 1) &
        call exitError('Wrong number of fields on SHIFT line')
    xoffset = xnum(1)
    if (nfields == 2) yOffset = xnum(2)
  endif
  !
  if (PipGetString('XTILTFILE', card) == 0) then
    call dopen(3, card, 'ro', 'f')
    read(3,*,err = 2412, end = 2412) (alpha(i), i = 1, numViews)
    close(3)
    ifAlpha = 1
    do i = 1, numViews
      if (abs(alpha(i) - alpha(1)) > 1.e-5) ifAlpha = 2
    enddo
    if (ifAlpha == 2) write(6, 2201)
    if (ifAlpha == 1) write(6, 2202) - alpha(1)
  endif
  !
  if (PipGetString('WeightFile', card) == 0) then
    ifExpWeight = 1
    call dopen(3, card, 'ro', 'f')
    read(3,*,err = 2414, end = 2414) (exposeWeight(i), i = 1, numViews)
    close(3)
  endif
  !
  boundFile = ' '
  ierr = PipGetString('BoundaryInfoFile', boundFile)
  !
  if (PipGetFloat('XAXISTILT', globalpha) == 0) then
    write(6, 2301) globalpha
    if (abs(globalpha) > 1.e-5 .and. ifAlpha == 0) ifAlpha = 1
  endif
  !
  ! REPROJECT entry must be read in before local alignments
  ! violates original unless a blank entry is allowed
  ierr = PipNumberOfEntries('REPROJECT', nument)
  do j = 1, nument
    nfields = 0
    ierr = PipGetFloatArray('REPROJECT',  xnum, nfields, limnum)
    if (nfields == 0) then
      nfields = 1
      xnum(1) = 0.
    endif
    if (nfields + numReproj > limReproj) call exitError( &
        'TOO MANY REPROJECTION ANGLES FOR ARRAYS')
    do i = 1, nfields
      numReproj = numReproj + 1
      angReproj(numReproj) = xnum(i)
    enddo
    if (j == 1) WRITE(6, 3101)
    reprojBP = .not.recReproj
  enddo
  !
  if (PipGetString('LOCALFILE', card) == 0) then
    call dopen(3, card, 'ro', 'f')
    read(3, '(a)', err = 2410, end = 2410) titlech
    ! read(3,*) nxWarp, nyWarp, ixStartWarp, iyStartWarp, idelXwarp, idelYwarp
    call frefor(titlech, xnum, ninp)
    ifDelAlpha = 0
    if (ninp > 6) ifDelAlpha = nint(xnum(7))
    pixelLocal = 0.
    if (ninp > 7) pixelLocal = xnum(8)
    localZfacs = 0
    if (ninp > 8) localZfacs = xnum(9)
    nxWarp = nint(xnum(1))
    nyWarp = nint(xnum(2))
    ixStartWarp = nint(xnum(3))
    iyStartWarp = nint(xnum(4))
    idelXwarp = nint(xnum(5))
    idelYwarp = nint(xnum(6))
    numWarpPos = nxWarp * nyWarp
    limWarp = nxWarp * nyWarp * numViews
    ipos = limWarp
    indDelta = numViews
    !
    ! If reprojecting rec, make sure arrays are big enough for all the
    ! reprojections and  allocate extra space at top of some arrays for
    ! temporary use
    if (recReproj) then
      indDelta = max(numViews, numReproj)
      limWarp = nxWarp * nyWarp * indDelta
      ipos = limWarp + indDelta
    endif
    if (nxWarp < 2 .or. nyWarp < 2) call exitError( &
        'THERE MUST BE AT LEAST TWO LOCAL ALIGNMENT AREAS IN X AND IN Y')
    allocate(indWarp(numWarpPos), delAlpha(ipos), cWarpBeta(limWarp), &
        sWarpBeta(limWarp), cWarpAlpha(limWarp), sWarpAlpha(limWarp), fwarp(2, 3, ipos), &
        delBeta(ipos), warpXZfac(ipos), warpYZfac(ipos), stat = ierr)
    if (ierr .ne. 0) call exitError( &
        'ALLOCATING ARRAYS FOR LOCAL ALIGNMENT DATA')
    indbase = 0
    do ipos = 1, nxWarp * nyWarp
      indWarp(ipos) = indbase
      read(3,*,err = 2410, end = 2410) (delBeta(i), i = indbase + 1, indbase + numViews)
      if (ifDelAlpha > 0) then
        read(3,*,err = 2410, end = 2410) (delAlpha(i), i = indbase + 1, indbase + &
            numViews)
      else
        do i = indbase + 1, indbase + numViews
          delAlpha(i) = 0.
        enddo
      endif
      !
      ! Set z factors to zero, read in if supplied, then negate them
      !
      do i = indbase + 1, indbase + numViews
        warpXZfac(i) = 0.
        warpYZfac(i) = 0.
      enddo
      if (localZfacs > 0) read(3,*,err = 2410, end = 2410) (warpXZfac(i), &
          warpYZfac(i), i = indbase + 1, indbase + numViews)
      do i = indbase + 1, indbase + numViews
        warpXZfac(i) = -warpXZfac(i)
        warpYZfac(i) = -warpYZfac(i)
      enddo
      do i = 1, numViews
        call xfread(3, fwarp(1, 1, i + indbase), 2410, 2410)
      enddo
      indbase = indbase + indDelta
    enddo
    close(3)
    write(6, 2401)
  endif
  !
  if (PipGetFloat('LOCALSCALE', scalelocal) == 0) write(6, 2501) scalelocal
  !
  ierr = PipGetTwoIntegers('FULLIMAGE', nxfull, nyfull)
  if (PipGetTwoIntegers('SUBSETSTART', ixsubset, iysubset) == 0) &
      ifSubsetIn = 1
  !
  nfields = 0
  if (PipGetIntegerArray('COSINTERP', inum, nfields, limnum) == 0) then
    interpOrdStretch = inum(1)
    if (nfields > 1) interpFacStretch = inum(2)
    interpOrdStretch = max(0, min(3, interpOrdStretch))
    if (interpOrdStretch == 0) interpFacStretch = 0
    if (interpFacStretch == 0) then
      print *,'Cosine stretching is disabled'
    else
      write(6, 2801) interpOrdStretch, interpFacStretch
    endif
  endif
  !
  if (PipGetInteger('XTILTINTERP', interpOrdXtilt) == 0) then
    interpOrdXtilt = xnum(1)
    if (interpOrdXtilt > 2) interpOrdXtilt = 3
    if (interpOrdXtilt <= 0) then
      print *,'New-style X-tilting with vertical slices is disabled'
    else
      write(6, 3001) interpOrdXtilt
    endif
  endif
  !
  ! Read environment variable first, then override by entry
  if (imodGetEnv('IMOD_USE_GPU', card) == 0) read(card,*) nGPU
  ifGpuByEnviron = PipGetInteger('UseGPU', nGPU)
  useGPU = nGPU >= 0
  ierr = PipGetTwoIntegers('ActionIfGPUFails', iactGpuFailOption, &
      iactGpuFailEnviron)
  !
  if (PipGetString('ZFACTORFILE', card) == 0) then
    call dopen(3, card, 'ro', 'f')
    read(3,*,err = 2413, end = 2413) (xzfac(i), yzfac(i), i = 1, numViews)
    close(3)
    ifZfac = 1
    write(6, 3201)
  endif
  !
  if (PipGetInteger('IMAGEBINNED', imageBinned) == 0) then
    imageBinned = max(1, imageBinned)
    if (imageBinned > 1) write(6, 3301) imageBinned
  endif
  !
  if (PipGetTwoIntegers('TOTALSLICES', inum(1), inum(2)) == 0) then
    minTotSlice = inum(1) + 1
    maxTotSlice = inum(2) + 1
  endif
  !
  ierr = PipGetFloat('FlatFilterFraction', flatFrac)
  flatFrac = max(0.,  flatFrac)
  !
  ierr = PipGetLogical('AdjustOrigin', adjustOrigin)
  !
  if (.not. recReproj) &
      ierr = PipGetThreeFloats('MinMaxMean', dminIn, dmaxIn, dmeanIn)
  !
  if (PipGetString('WeightAngleFile', card) == 0) then
    call dopen(3, card, 'ro', 'f')
313 read(3,*,err = 2415, end = 314) wgtAngles(numWgtAngles + 1)
    numWgtAngles = numWgtAngles + 1
    go to 313
    !
    ! Sort the angles
314 do i = 1, numWgtAngles - 1
      do j = i + 1, numWgtAngles
        if (wgtAngles(i) > wgtAngles(j)) then
          dmint = wgtAngles(i)
          wgtAngles(i) = wgtAngles(j)
          wgtAngles(j) = dmint
        endif
      enddo
    enddo
  endif
  !
  ! SIRT-related options
  ierr = PipGetInteger('ConstrainSign', isignConstraint)
  !
  ierr = PipGetTwoIntegers('InternalSIRTSlices', ifOutSirtProj, &
      ifOutSirtRec)
  if (numSIRTiter > 0 .or. recSubtraction) &
      ierr = PipGetInteger('StartingIteration', iterForReport)
  ierr = PipGetLogical('VertForSIRTInput', vertSirtInput)
  ierr = PipGetString('VertSliceOutputFile', vertOutFile)
  ierr = PipGetString('VertBoundaryFile', vertBoundFile)
  !
  call PipDone()
  !
  ! END OF OPTION READING
  !
  WRITE(6, 48)
  if (ifAlpha .ne. 0 .and. idelSlice .ne. 1) call exitError( &
      'Cannot do X axis tilt with non-consecutive slices')
  if (nxWarp .ne. 0 .and. idelSlice .ne. 1) call exitError( &
      'Cannot do local alignments with non-consecutive slices')
  if (minTotSlice > 0 .and. idelSlice .ne. 1) call exitError( &
      'Cannot do chunk writing with non-consecutive slices')
  if (nxfull == 0 .and. nyfull == 0 .and. &
      (ixsubset .ne. 0 .or. iysubset .ne. 0)) call exitError( &
      'YOU MUST ENTER THE FULL IMAGE SIZE IF YOU HAVE A SUBSET')
  if (.not.perpendicular .and. minTotSlice > 0) call exitError( &
      'Cannot do chunk writing with parallel slices')
  if (numReproj > 0 .and. nViewsReproj > 0) call exitError( &
      'You cannot enter both views and angles to reproject')
  if (projModel .and. (numReproj > 0 .or. &
      nViewsReproj > 0)) call exitError('You cannot do projection '// &
      'from a model with image reprojection')
  if (numSIRTiter > 0 .and. (numReproj > 0 .or. &
      nViewsReproj > 0)) call exitError('You cannot do SIRT '// &
      'with entries for angles/views to reproject')
  !
  ! outScale dimensions down by binning then report them
  !
  if (imageBinned > 1) then
    if (ifSliceIn .ne. 0 .and. (minTotSlice <= 0 .or. isliceStart > 0)) then
      isliceStart = max(1, min(nprojXyz(2), &
          (isliceStart + imageBinned - 1) / imageBinned))
      isliceEnd = max(1, min(nprojXyz(2), &
          (isliceEnd + imageBinned - 1) / imageBinned))
    endif
    if (ifThickIn .ne. 0) ithickBP = ithickBP / imageBinned
    axisXoffset = axisXoffset / imageBinned
    nxfull = (nxfull + imageBinned - 1) / imageBinned
    nyfull = (nyfull + imageBinned - 1) / imageBinned
    ixsubset = ixsubset / imageBinned
    iysubset = iysubset / imageBinned
    xoffset = xoffset / imageBinned
    yOffset = yOffset / imageBinned
    if (ifWidthIn .ne. 0) iwidth = iwidth / imageBinned
    if (minTotSlice >= 0 .and. .not. recReproj) then
      minTotSlice = max(1, min(nprojXyz(2), &
          (minTotSlice + imageBinned - 1) / imageBinned))
      maxTotSlice = max(1, min(nprojXyz(2), &
          (maxTotSlice + imageBinned - 1) / imageBinned))
    endif
  endif
  !
  ! Set up an effective scaling factor for non-log scaling to test output
  ! The equation in ( ) is based on fitting to the amplification of the range
  ! with linear scaling for different input sizes
  effectiveScale = 1.
  if (ifLog == 0) effectiveScale = outScale * (0.011 * nprojXyz(1) + 6) / 2.
  !
  if (recReproj) then
    if (debug) print *,minTotSlice, maxTotSlice, minZreproj, maxZreproj, nrxyz(3)
    if ((minTotSlice <= 0 .and. (minZreproj <= 0 .or. maxZreproj > &
        nrxyz(3))) .or. (minTotSlice >= 0 .and. &
        maxTotSlice > nrxyz(3))) call exitError( &
        'Min or Max Z coordinate to project is out of range')

    if (.not.perpendicular) call exitError( &
        'Cannot reproject from reconstruction output with PARALLEL')
    if (idelSlice .ne. 1) call exitError( &
        'Cannot reproject from reconstruction with a slice increment')
    if (iwidth .ne. nrxyz(1) .or. isliceEnd + 1 - isliceStart .ne. nrxyz(3) .or. &
        ithickBP .ne. nrxyz(2)) call exitError( &
        'Dimensions of rec file do not match expected values')
  else
    !
    ! Check conditions of SIRT (would slice increment work?)
    if (numSIRTiter > 0) then
      if (.not.perpendicular .or. idelSlice .ne. 1 .or. &
          xoffset .ne. 0.) call exitError('Cannot do SIRT with PARALLEL'// &
          ' output, slice increment, or X shifts')
      if (nxWarp .ne. 0 .or. ifzfac .ne. 0 .or. ifAlpha > 1 .or. &
          (ifAlpha == 1 .and. interpOrdXtilt == 0)) &
          call exitError('Cannot do SIRT with  local alignments, Z '// &
          'factors, or variable or old-style X tilt')
      if (iwidth .ne. nxProj .or. (.not. sirtFromZero .and. (iwidth .ne. nrxyz(1) .or. &
          (ithickBP .ne. nrxyz(2) .and. .not.vertSirtInput) .or. nrxyz(3) .ne. nyProj))) &
          call exitError( 'For SIRT, sizes of input projections, rec '// &
          'file, and width/thickness entries must match')
      write(6, 3501) numSIRTiter
      interpFacStretch = 0
    endif
    if (ifSliceIn .ne. 0) WRITE(6, 201) isliceStart, isliceEnd, idelSlice
    if (ifThickIn .ne. 0) WRITE(6, 301) ithickBP
    if (delang .ne. 0. .or. axisXoffset .ne. 0.) WRITE(6, 601) DELANG, axisXoffset
    if (nxfull .ne. 0 .or. nyfull .ne. 0) write(6, 2601) nxfull, nyfull
    if (ifSubsetIn .ne. 0) write(6, 2701) ixsubset, iysubset
    if (ifWidthIn .ne. 0) WRITE(6, 2001) iwidth
    if (xoffset .ne. 0 .or. yOffset .ne. 0) WRITE(6, 2101) yOffset, xoffset
    if (minTotSlice > 0) write(6, 3401) minTotSlice, maxTotSlice
  endif
  !
  ! If NEWANGLES is 0, get angles from file header.  Otherwise check if angles OK
  !
  if (newangles == 0 .and. iftiltfile == 0) then
    !
    ! Tilt information is stored in stack header. Read into angles
    ! array. All sections are assumed to be equally spaced. If not,
    ! you need to set things up differently. In such a case, great
    ! care should be taken, since missing views may have severe
    ! effects on the quality of the reconstruction.
    !
    !
    ! call irtdat(1, idtype, lens, nd1, nd2, vd1, vd2)
    !
    if (idtype .ne. 1) call exitError( ' Not tilt data.')
    !
    if (nd1 .ne. 2) call exitError(' Tilt axis not along Y.')
    !
    dtheta = vd1
    theta = vd2
    !
    DO NV = 1, numViews
      angles(NV) = theta
      theta = theta + dtheta
    enddo
    !
  else
    if (iftiltfile == 1 .and. newangles .ne. 0) then
      call exitError( &
          'Tried to enter angles with both ANGLES and TILTFILE')
    elseif (iftiltfile == 1) then
      write(6,*) ' Tilt angles were entered from a tilt file'
    elseif (newangles == numViews) then
      write(6,*) ' Tilt angles were entered with ANGLES card(s)'
    else
      call exitError('If using ANGLES, a value must be '// &
          'entered for each view')
    endif
  endif
  !
  if (ncompress > 0) then
    if (ncompress == numViews) then
      write(6,*) &
          ' Compression values were entered with COMPRESS card(s)'
    else
      call exitError('If using COMPRESS, a value must be '// &
          'entered for each view')
    endif
    do nv = 1, numViews
      compress(nv) = 1. +(compress(nv) - 1.) / compfac
    enddo
  endif
  !
  if (globalpha .ne. 0.) then
    do iv = 1, numViews
      alpha(iv) = alpha(iv) - globalpha
    enddo
  endif
  !
  if (ifExpWeight .ne. 0) then
    if (ifLog == 0) write(6,*) &
        ' Weighting factors were entered from a file'
    if (ifLog .ne. 0) write(6,*) ' Weighting factors were entered '// &
        'but will be ignored because log is being taken'
  endif
  !
  ! if no INCLUDE cards, set up map to views, excluding any specified by
  ! EXCLUDE cards
  if (nvuse == 0) then
    do i = 1, numViews
      ierr = 0
      do iex = 1, nvexcl
        if (i == ivexcl(iex)) ierr = 1
      enddo
      if (ierr == 0) then
        nvuse = nvuse + 1
        mapUsedView(nvuse) = i
      endif
    enddo
  endif
  !
  ! Replace angles at +/-90 with 89.95 etc
  do i = 1, nvuse
    j = mapUsedView(i)
    if (abs(abs(angles(j)) - 90.) < 0.05) &
        angles(j) = sign(90. - sign(0.05, 90 - abs(angles(j))), angles(j))
  enddo
  !
  ! If reprojecting from rec and no angles entered, copy angles in original
  ! order
  if (recReproj .and. numReproj == 0) then
    if (nViewsReproj == 1 .and. ivreprj(1) == 0) then
      numReproj = numViews
      do i = 1, numViews
        angReproj(i) = angles(i)
      enddo
    else if (nViewsReproj > 0) then
      numReproj = nViewsReproj
      do i = 1, numReproj
        if (ivreprj(i) < 1 .or. ivreprj(i) > numViews) call exitError( &
            'View number to reproject is out of range')
        angReproj(i) = angles(ivreprj(i))
      enddo
    else
      !
      ! For default set of included views, order them by view number by
      ! first ordering the mapUsedView array by view number.
      do i = 1, nvuse-1
        do j = i + 1, nvuse
          if (mapUsedView(i) > mapUsedView(j)) then
            indi = mapUsedView(i)
            mapUsedView(i) = mapUsedView(j)
            mapUsedView(j) = indi
          endif
        enddo
      enddo
      numReproj = nvuse
      do i = 1, nvuse
        angReproj(i) = angles(mapUsedView(i))
      enddo
    endif
  endif
  !
  ! order the MAPUSE array by angle
  do i = 1, nvuse-1
    do j = i + 1, nvuse
      indi = mapUsedView(i)
      if (angles(indi) > angles(mapUsedView(j))) then
        mapUsedView(i) = mapUsedView(j)
        mapUsedView(j) = indi
        indi = mapUsedView(i)
      endif
    enddo
  enddo
  !
  ! For SIRT, now copy the angles in the ordered list; this is the order
  ! in which the reprojections are needed internally
  ! Also adjust the recon mean for a fill value
  if (numSIRTiter > 0) then
    numReproj = nvuse
    do i = 1, nvuse
      angReproj(i) = angles(mapUsedView(i))
    enddo
    outAdd = outAdd / filterScale
    outScale = outScale * filterScale
    dmeanIn = dmeanIn / outScale - outAdd
  endif
  if (numReproj > 0) then
    allocate(cosReproj(numReproj), sinReproj(numReproj), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR REPROJECTION SINES/COSINES')
    do i = 1, numReproj
      cosReproj(i) = cos(dtor * angReproj(i))
      sinReproj(i) = sin(dtor * angReproj(i))
    enddo
  endif

  !
  ! Open output map file
  call irtdel(1, delta)
  call irtorg(1, origx, origy, origz)
  if (.not. recReproj) then
    if ((minTotSlice <= 0 .and. (isliceStart < 1 .or. isliceEnd < 1)) &
        .or. isliceStart > nprojXyz(2) .or. isliceEnd > nprojXyz(2)) call exitError( &
        'SLICE NUMBERS OUT OF RANGE')
    NSLICE = (isliceEnd - isliceStart) / idelSlice + 1
    if (minTotSlice > 0 .and. isliceStart < 1) &
        nslice = maxTotSlice + 1 - minTotSlice
    ! print *,'NSLICE', minTotSlice, maxTotSlice, isliceStart, nslice
    if (nslice <= 0) call exitError( 'SLICE NUMBERS REVERSED')
    if (reprojBP .or. readBaseRec) then
      allocate(projLine(iwidth), stat = ierr)
      call memoryError(ierr, 'ARRAY FOR PROJECTION LINE')
    endif
    !
    ! DNM 7/27/02: transfer pixel sizes depending on orientation of output
    !
    NOXYZ(1) = iwidth
    cell(1) = iwidth * delta(1)
    if (perpendicular) then
      NOXYZ(2) = ithickBP
      NOXYZ(3) = NSLICE
      cell(2) = ithickBP * delta(1)
      cell(3) = nslice * idelSlice * delta(2)
    ELSE
      NOXYZ(2) = NSLICE
      NOXYZ(3) = ithickBP
      cell(3) = ithickBP * delta(1)
      cell(2) = nslice * idelSlice * delta(2)
    END IF
    if (reprojBP) then
      NOXYZ(2) = NSLICE
      NOXYZ(3) = numReproj
      cell(2) = nslice * idelSlice * delta(2)
      cell(3) = delta(1) * numReproj
      j = iwidth * numReproj
      allocate(xRayStart(j), yRayStart(j), numPixInRay(j), maxRayPixels(numReproj),  &
          stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS FOR PROJECTION RAY DATA')
      do i = 1, numReproj
        j = (i - 1) * iwidth + 1
        !
        ! Note that this will set cosReproj to 0 after you carefully kept it from being 0
        call set_projection_rays(sinReproj(i), cosReproj(i), iwidth, ithickBP, &
            iwidth, xRayStart(j), yRayStart(j), numPixInRay(j), maxRayPixels(i))
      enddo
    endif
  else
    !
    ! recReproj stuff
    noxyz(1) = maxXreproj + 1 - minXreproj
    noxyz(2) = maxZreproj + 1 - minZreproj
    ithickReproj = maxYreproj + 1 - minYreproj
    noxyz(3) = numReproj
    if (minTotSlice > 0 .and. minZreproj < 1) &
        noxyz(2) = maxTotSlice + 1 - minTotSlice
    if (noxyz(1) < 1 .or. noxyz(2) < 1 .or. ithickReproj < 1) &
        call exitError('Min and max limits for output are reversed for '// &
        'X, Y, or Z')
    cell(1) = noxyz(1) * delta(1)
    cell(2) = noxyz(2) * delta(1)
    cell(3) = delta(1) * numReproj
    if (projSubtraction .and. (noxyz(1) .ne. nxProj .or. maxZreproj > &
        nyProj .or. noxyz(3) .ne. nprojXyz(3))) call exitError('OUTPUT SIZE'// &
        ' MUST MATCH ORIGINAL PROJECTION FILE SIZE FOR SIRT SUBTRACTION')
  endif
  !
  ! Check compatibility of base rec file
  if ((readBaseRec .and. .not. recReproj) .and. (nrxyz(1) .ne. iwidth .or. &
      nrxyz(2) .ne. ithickBP .or. nrxyz(3) < isliceEnd)) call exitError( &
      'BASE REC FILE IS NOT THE SAME SIZE AS OUTPUT FILE')
  !
  ! open old file if in chunk mode and there is real starting slice
  ! otherwise open new file
  !
  if (.not.projModel) then
    if (minTotSlice > 0 .and. ((.not.recReproj .and. isliceStart > 0) .or. &
        (recReproj .and. minZreproj > 0))) then
      CALL IMOPEN(2, FILOUT, 'OLD')
      CALL IRDHDR(2, NOXYZ, MPXYZ, newMode, dmint, dmaxt, dmeant)
    else
      CALL IMOPEN(2, FILOUT, 'NEW')
      CALL ICRHDR(2, NOXYZ, NOXYZ, newMode, title, 0)
      ! print *,'created', NOXYZ
    endif
    CALL ITRLAB(2, 1)
    call ialcel(2, cell)
  endif
  !
  ! if doing perpendicular slices, set up header info to make coordinates
  ! congruent with those of tilt series
  !
  if (recReproj) then
    call ialorg(2, 0., 0., 0.)
    outilt(1) = 0.
    call ialtlt(2, outilt)
  else if (perpendicular) then
    outilt(1) = 90
    if (adjustOrigin) then
      !
      ! Full adjustment if requested
      origx = origx  - delta(1) * (nprojXyz(1) / 2 - iwidth / 2 - xoffset)
      origz = origy - delta(1) * float(max(0, isliceStart - 1))
      if (minTotSlice > 0 .and. isliceStart <= 0) &
          origz = origy - delta(1) * (minTotSlice-1)
      origy = delta(1) * (ithickBP / 2 + yOffset)
    else
      !
      ! Legacy origin.  All kinds of wrong.
      origx = cell(1) / 2. +axisXoffset
      origy = cell(2) / 2.
      origz = -float(max(0, isliceStart - 1))
    endif

    if (.not.projModel) then
      call ialorg(2, origx, origy, origz)
      call ialtlt(2, outilt)
    endif
  endif
  !
  ! Initialize parallel writing routines if bound file entered
  ierr = parWrtInitialize(boundFile, 7, noxyz(1), noxyz(2), noxyz(3))
  if (ierr .ne. 0) then
    write(*,'(a,i3)') 'ERROR: TILT - INITIALIZING PARALLEL WRITE '// &
        'BOUNDARY FILE, ERROR', ierr
    call exit(1)
  endif
  !
  ! chunk mode starter run: write header and exit
  !
  ifexit = 0
  if (.not.projModel) then
    if (minTotSlice > 0 .and. ((.not.recReproj .and. isliceStart <= 0) .or. &
        (recReproj .and. minZreproj <= 0))) then
      CALL IWRHDR(2, title, 1, dminIn, dmaxIn, dmeanIn)
      CALL IMCLOSE(2)
      ifexit = 1
    elseif (minTotSlice > 0 .and. .not.recReproj) then
      call parWrtPosn(2, isliceStart - minTotSlice, 0)
      if (readBaseRec .and. .not. recReproj) &
          call imposn(3, isliceStart - minTotSlice, 0)
    endif
  endif
  !
  ! If reprojecting, need to look up each angle in full list of angles and
  ! find ones to interpolate from, then pack data into arrays that are
  ! otherwise used for packing these factors down
  if (recReproj) then
    do i = 1, numReproj
      sinBeta(i) = angReproj(i)
      call lookupAngle(angReproj(i), angles, numViews, ind1, ind2, frac)
      cosBeta(i) = (1. - frac) * compress(ind1) + frac * compress(ind2)
      sinAlpha(i) = (1. - frac) * alpha(ind1) + frac * alpha(ind2)
      cosAlpha(i) = (1. - frac) * xzfac(ind1) + frac * xzfac(ind2)
      array(i) = (1. - frac) * yzfac(ind1) + frac * yzfac(ind2)
      array(i + numViews) = (1. - frac) * exposeWeight(ind1) + frac * exposeWeight(ind2)
    enddo
    !
    ! Do the same thing with all the local data: pack it into the spot at
    ! the top of the local data then copy it back into the local area
    if (nxWarp > 0) then
      do i = 1, nxWarp * nyWarp
        do iv = 1, numReproj
          call lookupAngle(angReproj(iv), angles, numViews, ind1, ind2, frac)
          ind1 = ind1 + indWarp(i)
          ind2 = ind2 + indWarp(i)
          delBeta(indbase + iv) = (1. -frac) * delBeta(ind1) + &
              frac * delBeta(ind2)
          delAlpha(indbase + iv) = (1. -frac) * delAlpha(ind1) + &
              frac * delAlpha(ind2)
          warpXZfac(indbase + iv) = (1. -frac) * warpXZfac(ind1) + &
              frac * warpXZfac(ind2)
          warpYZfac(indbase + iv) = (1. -frac) * warpYZfac(ind1) + &
              frac * warpYZfac(ind2)
          do j = 1, 2
            do k = 1, 3
              fwarp(j, k, indbase + iv) = (1. -frac) * fwarp(j, k, ind1) + &
                  frac * fwarp(j, k, ind2)
            enddo
          enddo
        enddo
        do iv = 1, numReproj
          ind1 = indbase + iv
          ind2 = indWarp(i) + iv
          delBeta(ind2) = delBeta(ind1)
          delAlpha(ind2) = delAlpha(ind1)
          warpXZfac(ind2) = warpXZfac(ind1)
          warpYZfac(ind2) = warpYZfac(ind1)
          do j = 1, 2
            do k = 1, 3
              fwarp(j, k, ind2) = fwarp(j, k, ind1)
            enddo
          enddo
        enddo
      enddo
    endif
    !
    ! Replace the mapUsedView array
    nvuse = numReproj
    do i = 1, nvuse
      mapUsedView(i) = i
    enddo
  else
    !
    ! pack angles and other data down as specified by MAPUSE
    ! Negate the z factors since things are upside down here
    ! Note that local data is not packed but always referenced by mapUsedView
    !
    do i = 1, nvuse
      sinBeta(i) = angles(mapUsedView(i))
      cosBeta(i) = compress(mapUsedView(i))
      sinAlpha(i) = alpha(mapUsedView(i))
      cosAlpha(i) = xzfac(mapUsedView(i))
      array(i) = yzfac(mapUsedView(i))
      array(i + numViews) = exposeWeight(mapUsedView(i))
    enddo
  endif
  do i = 1, nvuse
    angles(i) = sinBeta(i)
    compress(i) = cosBeta(i)
    alpha(i) = sinAlpha(i)
    xzfac(i) = -cosAlpha(i)
    yzfac(i) = -array(i)
    exposeWeight(i) = array(i + numViews)
  enddo
  nvorig = numViews
  numViews = nvuse
  !
  WRITE(6, 51) (angles(NV), NV = 1, numViews)
  WRITE(6, 52)
  !
  ! Turn off cosine stretch for high angles
  if (angles(1) < -80. .or. angles(numViews) > 80.) then
    if (interpFacStretch > 0) write(*,662)
662 format(/,'Tilt angles are too high to use cosine stretching')
    interpFacStretch = 0
  endif
  !
  ! Set up trig tables -  Then convert angles to radians
  !
  DO  iV = 1, numViews
    thetanv = angles(IV) + DELANG
    if (thetanv > 180.) thetanv = thetanv - 360.
    if (thetanv <= -180.) thetanv = thetanv + 360.
    cosBeta(iv) = COS(thetanv * DTOR)
    !
    ! Keep cosine from going to zero so it can be divided by
    if (abs(cosBeta(iv)) < 1.e-6) cosBeta(iv) = sign(1.e-6, cosBeta(iv))
    !
    ! Take the negative of the sine of tilt angle to account for the fact
    ! that all equations are written for rotations in the X/Z plane,
    ! viewed from  the negative Y axis
    ! Take the negative of alpha because the entered value is the amount
    ! that the specimen is tilted and we need to rotate by negative of that
    sinBeta(iv) = -SIN(thetanv * DTOR)
    cosAlpha(iv) = cos(alpha(iv) * dtor)
    sinAlpha(iv) = -sin(alpha(iv) * dtor)
    angles(iv) = -dtor * (angles(iv) + delang)
  enddo
  !
  ! If there are weighting angles, convert those the same way, otherwise
  ! copy the main angles to weighting angles
  if (numWgtAngles > 0) then
    do iv = 1, numWgtAngles
      wgtAngles(iv) = -dtor * (wgtAngles(iv) + delang)
    enddo
  else
    numWgtAngles = numViews
    do iv = 1, numViews
      wgtAngles(iv) = angles(iv)
    enddo
  endif
  !
  ! if fixed x axis tilt, set up to try to compute vertical planes
  ! and interpolate output planes: adjust thickness that needs to
  ! be computed, and find number of vertical planes that are needed
  !
  if (ifZfac > 0 .and. ifAlpha == 0) ifAlpha = 1
  ithickOut = ithickBP
  ycenModProj = ithickBP / 2 + 0.5 + yOffset
  if (ifAlpha == 1 .and. nxWarp == 0 .and. interpOrdXtilt > 0 .and. &
      ifZfac == 0 .and. .not.recReproj) then
    ifAlpha = -1
    ithickOut = ithickBP
    ithickBP = ithickBP / cosAlpha(1) + 4.5
    numVertNeeded = ithickOut * abs(sinAlpha(1)) + 5.
    numReadNeed = 0
    if (numSIRTiter > 0) then
      saveVertSlices = vertOutFile .ne. ' '
      if (.not.sirtFromZero .and. .not.vertSirtInput) &
          numReadNeed = ithickBP * abs(sinAlpha(1)) + 4.
      if (.not.sirtFromZero .and. vertSirtInput .and. ithickBP .ne. nrxyz(2)) &
          call exitError( &
          'THICKNESS OF VERTICAL SLICE INPUT FILE DOES NOT MATCH NEEDED THICKNESS')
    endif
  endif
  if (ifAlpha .ne. -1 .and. numSIRTiter > 0 .and. (vertSirtInput .or. vertOutFile &
      .ne. ' ')) call exitError('VertForSIRTInput OR VertSliceOutputFile '// &
      'CANNOT BE ENTERED UNLESS VERTICAL SLICES ARE BEING COMPUTED')
  !
  ! Now that we know vertical slices, open or set up output file for them under SIRT
  if (numSIRTiter > 0 .and. saveVertSlices) then
    if (minTotSlice > 0 .and. isliceStart > 0) then
      CALL IMOPEN(6, vertOutFile, 'OLD')
      CALL IRDHDR(6, nsxyz, MPXYZ, newMode, dmint, dmaxt, dmeant)
    else
      nsxyz = noxyz
      nsxyz(2) = ithickBP
      CALL IMOPEN(6, vertOutFile, 'NEW')
      CALL ICRHDR(6, nsxyz, nsxyz, 2, title, 0)
    endif
    !
    ! Initialize parallel writing if vertical bound file
    ierr = parWrtInitialize(vertBoundFile, 8, nsxyz(1), nsxyz(2), nsxyz(3))
    if (ierr .ne. 0) then
      write(*,'(a,i3)') 'ERROR: TILT - INITIALIZING PARALLEL WRITE '// &
          'BOUNDARY FILE FOR VERTICAL SLICES, ERROR', ierr
      call exit(1)
    endif
    !
    ! chunk mode: either write header, or set up to write correct location
    if (ifexit .ne. 0) then
      CALL IWRHDR(6, title, 0, dminIn, dmaxIn, dmeanIn)
      CALL IMCLOSE(6)
    elseif (minTotSlice > 0) then
      call parWrtPosn(6, isliceStart - minTotSlice, 0)
    endif
    ierr = parWrtSetCurrent(1)
  endif


  if (ifexit .ne. 0) then
    print *,'Exiting after setting up output file for chunk writing'
    call exit(0)
  endif
  !
  ! Set center of output plane and center of input for transformations
  ! Allow the full size to be less than the aligned stack, with a negative
  ! subset start
  !
  if (nxfull == 0) nxfull = nprojXyz(1)
  if (nyfull == 0) nyfull = nprojXyz(2)
  xcenIn = nxfull / 2. +0.5 - ixsubset
  centerSlice = nyfull / 2. +0.5 - iysubset
  xoffAdj = xoffset - (nprojXyz(1) / 2 + ixsubset - nxfull / 2)
  xcenOut = iwidth / 2 + 0.5 + axisXoffset + xoffAdj
  ycenOut = ithickBP / 2 + 0.5 + yOffset
  if (numSIRTiter > 0 .and. abs(xoffAdj + axisXoffset) > 0.1) call exitError( &
      'CANNOT DO SIRT WITH A TILT AXIS OFFSET FROM CENTER OF INPUT IMAGES')
  !
  ! if doing warping, convert the angles to radians and set sign
  ! Also cancel the z factors if global entry was not made
  !
  if (nxWarp > 0) then
    do i = 1, nvorig * nxWarp * nyWarp
      delBeta(i) = -dtor * delBeta(i)
    enddo
    do iv = 1, numViews
      do i = 1, nxWarp * nyWarp
        ind = indWarp(i) + mapUsedView(iv)
        cWarpBeta(ind) = cos(angles(iv) + delBeta(ind))
        sWarpBeta(ind) = sin(angles(iv) + delBeta(ind))
        cWarpAlpha(ind) = cos(dtor * (alpha(iv) + delAlpha(ind)))
        sWarpAlpha(ind) = -sin(dtor * (alpha(iv) + delAlpha(ind)))
        if (ifZfac == 0) then
          warpXZfac(ind) = 0.
          warpYZfac(ind) = 0.
        endif
      enddo
    enddo
    !
    ! See if local outScale was entered; if not see if it can be set from
    ! pixel size and local align pixel size
    if (scalelocal <= 0.) then
      scalelocal = 1.
      if (pixelLocal > 0) then
        scalelocal = pixelLocal / delta(1)
        if (abs(scalelocal - 1.) > 0.001) write(6, 53) scaleLocal
      endif
    endif
    !
    ! outScale the x and y dimensions and shifts if aligned data were
    ! shrunk relative to the local alignment solution
    ! 10/16/04: fixed to use mapUsedView to outScale used views properly
    !
    if (scalelocal .ne. 1.) then
      ixStartWarp = nint(ixStartWarp * scalelocal)
      iyStartWarp = nint(iyStartWarp * scalelocal)
      idelXwarp = nint(idelXwarp * scalelocal)
      idelYwarp = nint(idelYwarp * scalelocal)
      do iv = 1, numViews
        do i = 1, nxWarp * nyWarp
          ind = indWarp(i) + mapUsedView(iv)
          fwarp(1, 3, ind) = fwarp(1, 3, ind) * scalelocal
          fwarp(2, 3, ind) = fwarp(2, 3, ind) * scalelocal
        enddo
      enddo
    endif
    !
    ! if the input data is a subset in X or Y, subtract starting
    ! coordinates from ixStartWarp and iyStartWarp
    !
    ixStartWarp = ixStartWarp - ixsubset
    iyStartWarp = iyStartWarp - iysubset
  endif
  !
  ! Done with array in its small form and with angReproj
  deallocate(array, angReproj, ivexcl, ivreprj, stat = ierr)
  !
  ! Here is the place to project model points and exit
  if (projModel) call projectModel(filout, delta, nvorig)
  !
  ! If reprojecting, set the pointers and return
  if (recReproj) then
    indOutSlice = 1
    minXload = minXreproj
    maxXload = maxXreproj
    if (nxWarp .ne. 0) then
      minXload = max(1, minXload - 100)
      maxXload = min(nrxyz(1), maxXload + 100)
    endif
    iwideReproj = maxXload + 1 - minXload
    isliceSizeBP = iwideReproj * ithickReproj
    inPlaneSize = isliceSizeBP
    if (nxWarp .ne. 0) then
      dxWarpDelz = idelXwarp / 2.
      numWarpDelz = max(2., (iwideReproj - 1) / dxWarpDelz) + 1
      dxWarpDelz = (iwideReproj - 1.) / (numWarpDelz - 1.)
    endif
    !
    ! Get projection offsets for getting from coordinate in reprojection
    ! to coordinate in original projections.  The X coordinate must account
    ! for the original offset in building the reconstruction plus any
    ! additional offset.  But the line number here is the line # in the
    ! reconstruction so we only need to adjust Y by the original starting
    ! line.  Also replace the slice limits.
    xprojOffset = minXreproj - 1 + nprojXyz(1) / 2 - iwidth / 2 - xoffset
    yprojOffset = isliceStart - 1
    isliceStart = minZreproj
    isliceEnd = maxZreproj
    iwidth = noxyz(1)
    nyProj = nrxyz(3)
    dmeanIn = (dmeanIn / outScale - outAdd) / filterScale
    indLoadBase = 1
    ipExtraSize = 0
    numPad = 0
    if (debug) print *,'scale: ', outScale, outAdd

    numNeedEval = min(numNeedEval, isliceEnd + 1 - isliceStart)
    call setNeededSlices(maxNeeds, numNeedEval)
    if (allocateArray(maxNeeds, numNeedEval, 1, minMemory) == 0) &
        call exitError('THE MAIN ARRAY CANNOT BE ALLOCATED LARGE ENOUGH'// &
        ' TO REPROJECT A SINGLE Y VALUE')
    allocate(reprojLines(iwidth * numPlanes), stat = ierr)
    if (ierr .ne. 0) &
        call exitError('FAILED TO ALLOCATE ARRAY FOR REPROJECTED LINES')
    !
    if (projSubtraction) then
      allocate(origLines(iwidth * numPlanes), stat = ierr)
      if (ierr .ne. 0) &
          call exitError('FAILED TO ALLOCATE ARRAY FOR ORIGINAL LINES')
    endif
    if (useGPU) then
      ind = 0
      if (debug) ind = 1
      if (useGPU) useGPU = gpuAvailable(nGPU, gpuMemory, ind) .ne. 0
      ind = maxNeeds(1) * inPlaneSize + iwidth * numPlanes
      iex = iwidth * numPlanes
      kti = 0
      if (useGPU .and. nxWarp > 0) then
        ind = ind + maxNeeds(1) * (8 * iwidth + numWarpDelz) + &
            12 * numWarpPos * numViews
        iex = iex + 12 * numWarpPos * numViews
        kti = numWarpDelz
        call packLocalData()
      endif
      if (useGPU) then
        useGPU = 4 * ind <= gpuMemoryFrac * gpuMemory
        if (.not. useGPU) print *, 'GPU is available but it has insufficient memory'
      endif
      if (useGPU)  call allocateGpuPlanes(iex, nxWarp * nyWarp, kti, 0, &
          numPlanes, iwideReproj, ithickReproj)
      if (useGPU .and. nxWarp .ne. 0) &
          useGPU = gpuLoadLocals(packLocal, nxWarp * nyWarp) == 0
      if (useGPU) then
        print *,'Using the GPU for reprojection'
      else
        print *,'The GPU cannot be used, using the CPU for reprojection'
      endif
      if (allocated(packLocal)) deallocate(packLocal)
      call warnOrExitIfNoGPU()
    endif
    !
    ! Finally allocate the warpDelz now that number of lines is known,
    ! and projecton factors now that number of planes is known
    if (nxWarp .ne. 0) then
      kti = iwideReproj * numPlanes
      allocate(warpDelz(numWarpDelz * max(1, numGpuPlanes)), xprojfs(kti), &
          xprojzs(kti), yprojfs(kti), yprojzs(kti), stat = ierr)
      if (ierr .ne. 0) call exitError( &
          'ALLOCATING LOCAL PROJECTION FACTOR OR warpDelz ARRAYS')
    endif
    return
  endif
  !
  ! BACKPROJECTION ONLY.  First allocate the projection factor array
  if (nxWarp .ne. 0) then
    allocate(xprojfs(iwidth), xprojzs(iwidth), yprojfs(iwidth), &
        yprojzs(iwidth), stat = ierr)
    if (ierr .ne. 0) call exitError( &
        'ALLOCATING ARRAYS FOR LOCAL PROJECTION FACTORS')
  endif
  !
  ! If reading base, figure out total views being added and adjust scales
  if (readBaseRec .and. .not. recSubtraction) then
    iv = numViews
    do j = 1, numViews
      k = 0
      do i = 1, numViewSubtract
        if (iviewSubtract(i) == 0 .or. mapUsedView(j) == iviewSubtract(i)) k = 1
      enddo
      iv = iv - 2 * k
    enddo
    baseOutScale = outScale / numViewBase
    baseOutAdd = outAdd * numViewBase
    outScale = outScale / (iv + numViewBase)
    outAdd = outAdd * (iv + numViewBase)
    if (debug)  print *,'base: ', baseOutScale, baseOutAdd
  else if (numSIRTiter <= 0) then
    outScale = outScale / numViews
    outAdd = outAdd * numViews
  endif
  if (debug) print *,'scale: ', outScale, outAdd
  numNeedEval = min(numNeedEval, isliceEnd + 1 - isliceStart)
  call setNeededSlices(maxNeeds, numNeedEval)
  if (debug) print *,(maxNeeds(i), i = 1, numNeedEval)
  if (iterForReport > 0) then
    allocate(reportVals(3, max(1, numSIRTiter)), stat = ierr)
    if (ierr .ne. 0) call exitError('ALLOCATING REPORT VALUE ARRAY')
    reportVals(1:3, 1:max(1, numSIRTiter)) = 0.
  endif
  !
  ! 12/13/09: removed fast backprojection code
  !
  ! Set up padding: 10% of X size or minimum of 16, max of 50
  npadtmp = min(50, 2 * max(8, nprojXyz(1) / 20))
  ! npadtmp = 2 * nprojXyz(1)
  nprpad = niceframe(2 * ((nprojXyz(1) + npadtmp) / 2), 2, 19)
  numPad = nprpad - nprojXyz(1)
  !
  ! Set up defaults for plane size and start of planes of input data
  isliceSizeBP = iwidth * ithickBP
  ipExtraSize = 0
  NXPRJ2 = nxProj + 2 + numPad
  inPlaneSize = NXPRJ2 * numViews
  indOutSlice = inPlaneSize + 1
  indLoadBase = indOutSlice + isliceSizeBP
  if (numSIRTiter > 0) then
    if (sirtFromZero) indOutSlice = 2 * inPlaneSize + 1
    indLoadBase = indOutSlice + isliceSizeBP
    ireadBase = indLoadBase
    indWorkPlane = indLoadBase + isliceSizeBP
    indLoadBase = indWorkPlane + inPlaneSize
    nsxyz(1) = iwidth
    nsxyz(2) = isliceEnd + 1 - isliceStart
    nsxyz(3) = numViews
    if (ifOutSirtProj > 0) then
      call imopen(4, 'sirttst.prj', 'NEW')
      call icrhdr(4, nsxyz, nsxyz, 2, title, 0)
      CALL IWRHDR(4, title, 0, -1.e6, 1.e6, 0.)
    endif
    nsxyz(2) = ithickBP
    nsxyz(3) = isliceEnd + 1 - isliceStart
    if (ifOutSirtRec > 0) then
      call imopen(5, 'sirttst.drec', 'NEW')
      call icrhdr(5, nsxyz, nsxyz, 2, title, 0)
      CALL IWRHDR(5, title, 0, -1.e6, 1.e6, 0.)
    endif
  endif
  maxStack = 0
  !
  ! Determine if GPU can be used, but don't try to allocate yet
  if (useGPU) then
    ind = 0
    if (debug) ind = 1
    wallstart = wallTime()
    useGPU = gpuAvailable(nGPU, gpuMemory, ind) .ne. 0
    if (debug) write(*,'(a,f8.4)') 'Time to test if GPU available: ', &
        wallTime() - wallstart
    if (useGPU) then
      !
      ! Basic need is input planes for reconstructing one slice plus 2
      ! slices for radial filter and planes being filtered, plus output
      ! slice. Local alignment adds 4 arrays for local proj factors
      iv = (maxNeeds(1) + 2) * inPlaneSize + isliceSizeBP
      if (nxWarp .ne. 0) &
          iv = iv + 4 * (iwidth * numViews + 12 * numWarpPos * numViews)
      if (numSIRTiter > 0) iv = iv + isliceSizeBP + inPlaneSize
      if (sirtFromZero) iv = iv + inPlaneSize
      useGPU = 4 * iv <= gpuMemoryFrac * gpuMemory
      if (useGPU) then
        interpFacStretch = 0
      else
        print *,'GPU is available but it has insufficient memory'
      endif
    endif
    call warnOrExitIfNoGPU()
  endif
  !
  ! next evaluate cosine stretch and new-style tilt for memory
  !
  if (ifAlpha < 0) then
    !
    ! new-style X-axis tilt
    !
    indLoadBase = indOutSlice + isliceSizeBP * (numVertNeeded + 1)
    if (numSIRTiter > 0) then
      ireadBase = indLoadBase
      indWorkPlane = indLoadBase + numReadNeed * ithickOut * iwidth
      indLoadBase = indWorkPlane + inPlaneSize
    endif
    !
    ! find out what cosine stretch adds if called for
    !
    if (interpFacStretch > 0) then
      call set_cos_stretch()
      inPlaneSize = max(inPlaneSize, indStretchLine(numViews + 1))
      ipExtraSize = inPlaneSize
    endif
    !
    ! Does everything fit?  If not, drop back to old style tilting
    !
    if (allocateArray(maxNeeds, numNeedEval, 4, minMemory) == 0) then
      if (numSIRTiter > 0) call exitError( &
          'ALLOCATING ARRAYS NEEDED FOR IN-MEMORY SIRT ITERATIONS')
      ifAlpha = 1
      ithickBP = ithickOut
      isliceSizeBP = iwidth * ithickBP
      ycenOut = ithickBP / 2 + 0.5 + yOffset
      indLoadBase = indOutSlice + isliceSizeBP
      ipExtraSize = 0
      inPlaneSize = nxprj2 * numViews
      write(*,63)
63    format(/,'Failed to allocate an array big enough ', &
          'to use new-style X-axis tilting')
      call setNeededSlices(maxNeeds, numNeedEval)
    endif
  endif
  !
  ! If not allocated yet and not warping, try cosine stretch here
  !
  if (maxStack == 0 .and. nxWarp == 0 .and. interpFacStretch > 0) then
    call set_cos_stretch()
    !
    ! set size of plane as max of loading size and stretched size
    ! also set that an extra plane is needed
    ! if there is not enough space for the planes needed, then
    ! disable stretching and drop back to regular code
    !
    inPlaneSize = max(inPlaneSize, indStretchLine(numViews + 1))
    ipExtraSize = inPlaneSize
    if (allocateArray(maxNeeds, numNeedEval, 1, minMemory) == 0) then
      ipExtraSize = 0
      inPlaneSize = nxprj2 * numViews
      interpFacStretch = 0
      write(*,62)
62    format(/,'Failed to allocate an array big enough ', &
          'to use cosine stretching')
    endif
  endif
  !
  ! If array still not allocated (failure, or local alignments), do it now
  if (maxStack == 0) then
    if (allocateArray(maxNeeds, numNeedEval, 1, minMemory) == 0) &
        call exitError('COULD NOT ALLOCATE MAIN ARRAY LARGE ENOUGH TO '// &
        'RECONSTRUCT A SINGLE SLICE')
  endif
  !
  ! Set up radial weighting
  if (numSIRTiter > 0 .and. .not.sirtFromZero) flatFrac = 2.
  CALL RADWT(IRMAX, IFALL, 1)
  if (sirtFromZero) then
    frac = zeroWeight
    flatFrac = 2.
    CALL RADWT(IRMAX, IFALL, 2)
    zeroWeight = frac
  endif
  !
  ! If Using GPU, make sure memory is OK now and allocate and load things
  ind = isliceSizeBP + 2 * inPlaneSize
  if (useGPU .and. nxWarp .ne. 0) then
    ind = ind + 4 * numViews * iwidth + 12 * numWarpPos * numViews
    call packLocalData()
  endif
  wallstart = wallTime()
  if (useGPU) then
    if (ifAlpha <= 0 .and. nxWarp == 0) then
      iv = 0
      j = 1
      if (numSIRTiter > 0) iv = numViews
      if (sirtFromZero) j = 2
      useGPU = gpuAllocArrays(iwidth, ithickBP, nxprj2, numViews, 1, numViews, 0, &
          0, j, iv, 1, 1) == 0
    else
      call allocateGpuPlanes(ind, nxWarp * nyWarp, 0, 1, ithickBP, nxprj2, numViews)
    endif
    if (debug .and. useGPU) write(*,'(a,f8.4)') 'Time to allocate on GPU: ', &
        wallTime() - wallstart

    ! print *,useGPU
    wallstart = wallTime()
    if (useGPU) useGPU = gpuLoadFilter(array) == 0
    ! print *,useGPU
    if (useGPU .and. nxWarp .ne. 0) &
        useGPU = gpuLoadLocals(packLocal, nxWarp * nyWarp) == 0
    ! print *,useGPU
    if (debug .and. useGPU) write(*,'(a,f8.4)') 'Time to load filter/locals on GPU: ', &
        wallTime() - wallstart
    if (useGPU) then
      print *,'Using GPU for backprojection'
    else
      print *,'Failed to allocate or load arrays on the GPU - using CPU '// &
          'for backprojection'
    endif
    if (allocated(packLocal)) deallocate(packLocal)
    call warnOrExitIfNoGPU()
  endif

  ! print *,interpFacStretch, ipExtraSize, ifAlpha, numVertNeeded
  !
  RETURN
  !
2410 call exitError('READING LOCAL TILT ALIGNMENT DATA FROM FILE')
2411 call exitError('READING TILT ANGLES FROM FILE')
2412 call exitError('READING X-AXIS TILT ANGLES FROM FILE')
2413 call exitError('READING Z FACTORS FROM FILE')
2414 call exitError('READING WEIGHTING FACTORS FROM FILE')
2415 call exitError('READING ANGLES FOR WEIGHTING FROM FILE')
  !
  !
48 FORMAT(//,1X,78('-'))
49 FORMAT('TILT: ',a,t57,A9,2X,A8)
51 FORMAT(/' Projection angles:'//(8F9.2))
52 FORMAT(//,1X,78('-'))
53 format(/,'Scaling of local alignments by ',f8.3, &
      ' determined from pixel sizes')
101 FORMAT(/' Title:    ',20A4)
201 FORMAT(/' Rows',I5,' to',I5,', (at intervals of',i4,') of the' &
      ,' projection planes will be reconstructed.')
301 FORMAT(/' Thickness of reconstructed slice is',I5, &
      ' pixels.')
401 FORMAT(/' Mask applied to edges of output slices with',i5, &
      ' extra pixels masked')
501 FORMAT(/' Radial weighting function parameters IRMAX =',I5, &
      '  IWIDE =',I5)
601 FORMAT(/' Output map rotated by',F6.1,' degrees about tilt axis', &
      ' with respect to tilt origin' / &
      ' Tilt axis displaced by',F9.2,' pixels from centre' &
      ,' of projection')
701 FORMAT(/' Output map densities incremented by',F8.2, &
      ' and then multiplied by',F8.2)
801 FORMAT(/' Output map is sectioned perpendicular to the ' &
      ,'tilt axis')
901 FORMAT(/' Output map is sectioned parallel to the' &
      ,' zero tilt projection')
1001 format(/' Data mode of output file is',i3)
1301 format(/' Taking logarithm of input data plus',f10.3)
1701 format(/' Compression was confined to',f6.3, &
      ' of the distance over which it was measured')
1801 format(/' Weighting by tilt density computed to distance of' &
      ,i3,' views',/,'  weighting factors:',(10f6.3))
1802 format(/' No weighting by tilt density')
2001 format(/,' Width of reconstruction is',i6,' pixels')
2101 format(/,' Output slice shifted up',f7.1,' and to right',f7.1, &
      ' pixels')
2201 format(/,' Alpha tilting to be applied with angles from file')
2202 format(/,' Constant alpha tilt of',f6.1,' to be applied based on ', &
      'angles from file')
2301 format(/,' Global alpha tilt of',f6.1,' will be applied')
2401 format(/,' Local tilt alignment information read from file')
2501 format(/,' Local alignment positions and shifts reduced by',f7.4)
2601 format(/,' Full aligned stack will be assumed to be',i6,' by', &
      i6,' pixels')
2701 format(/,' Aligned stack will be assumed to be a subset ', &
      'starting at',2i6)
2801 format(/,' Cosine stretching, if any, will have interpolation', &
      ' order', i2,', sampling factor',i2)
3001 format(/,' X-tilting with vertical slices, if any, will have ', &
      'interpolation order', i2)
3101 format(/,' Output will be one or more reprojections')
3201 format(/,' Z-dependent shifts to be applied with factors from file' &
      )
3301 format(/,' Dimensions and coordinates will be scaled down by a ', &
      'factor of ',i2)
3401 format(/,' Computed slices are part of a total volume from slice', &
      i6,' to',i6)
3501 format(/,i4,' iterations of SIRT algorithm will be done')

CONTAINS

  !
  ! Allocate as many planes as possible on the GPU up to the number
  ! allowed in ARRAY; nonPlane gives the number of floats needed for
  ! other data and numWarps is number of local positions, numDelz is
  ! size of warpDelz array, numfilts is number of filter
  ! filter arrays needed, nygout is size of output in y, nxgplane and
  ! nxyplane are size of input data
  !
  subroutine allocateGpuPlanes(nonPlane, numWarps, numDelz, numFilts, &
      nygout, nxgplane, nygplane)
    integer*4 nonPlane, numWarps, maxGpuPlane, nygout, nxgplane, nygplane
    integer*4 numFilts, numDelz, iperPlane
    !
    ! Start with as many planes as possible but no more than in array
    ! and no more that 65535 lines for array on GPU (the limit is 32767 before Fermi)
    iperPlane = inPlaneSize
    if (numDelz > 0) iperPlane = inPlaneSize + 8 * iwidth + numWarpDelz
    maxGpuPlane = (gpuMemoryFrac * gpuMemory / 4. - nonPlane) / iperPlane
    maxGpuPlane = min(maxGpuPlane, numPlanes, 65535 / nygplane)
    ! FOR TESTING MEMORY SHIFTING ETC
    ! ind = max(maxNeeds(1), min(ind, numPlanes / 3))
    numGpuPlanes = 0
    ! print *, ind, numPlanes, maxNeeds(1)
    do i = maxGpuPlane, maxNeeds(1), -1
      if (gpuAllocArrays(iwidth, nygout, nxgplane, nygplane, i, numViews, &
          numWarps,  numDelz, numFilts, 0, maxGpuPlane, maxNeeds(1)) == 0) &
          then
        numGpuPlanes = i
        loadGpuStart = 0
        loadGpuEnd = 0
        exit
      endif
    enddo
    useGPU = numGpuPlanes > 0
    return
  end subroutine allocateGpuPlanes

  ! Issue desired warning or exit on error if GPU not available
  subroutine warnOrExitIfNoGPU()
    if (ifGpuByEnviron .ne. 0) then
      if (useGPU .or. iactGpuFailEnviron == 0) return
      if (iactGpuFailEnviron == 2) call exitError('The environment '// &
          'variable IMOD_USE_GPU was set but a GPU cannot be used')
      write(*,'(a)') 'MESSAGE: The environment variable IMOD_USE_GPU was set ' &
          //'but a GPU will not be used'
    else
      if (useGPU .or. iactGpuFailOption == 0) return
      if (iactGpuFailOption == 2) call exitError('Use of the GPU was '// &
          'requested with the entry UseGPU but a GPU cannot be used')
      write(*,'(a)') 'MESSAGE: Use of the GPU was requested with the entry '// &
          'UseGPU but a GPU will not be used'
    endif
    call flush(6)
    return
  end subroutine warnOrExitIfNoGPU


  subroutine packLocalData()
    allocate(packLocal(numWarpPos * numViews, 12), stat = ierr)
    if (ierr .ne. 0) then
      useGPU = .false.
      write(*,'(/,a)') 'Failed to allocate array needed for '// &
          'using GPU with local alignments'
      call warnOrExitIfNoGPU()
    else
      !
      ! Pack data into one array
      do ipos = 1, nxWarp * nyWarp
        do iv = 1, numViews
          i = indWarp(ipos) + mapUsedView(iv)
          j = (ipos - 1) * numViews + iv
          packLocal(j, 1) = fwarp(1, 1, i)
          packLocal(j, 2) = fwarp(2, 1, i)
          packLocal(j, 3) = fwarp(1, 2, i)
          packLocal(j, 4) = fwarp(2, 2, i)
          packLocal(j, 5) = fwarp(1, 3, i)
          packLocal(j, 6) = fwarp(2, 3, i)
          packLocal(j, 7) = cWarpAlpha(i)
          packLocal(j, 8) = sWarpAlpha(i)
          packLocal(j, 9) = cWarpBeta(i)
          packLocal(j, 10) = sWarpBeta(i)
          packLocal(j, 11) = warpXZfac(i)
          packLocal(j, 12) = warpYZfac(i)
        enddo
      enddo
    endif
    return
  end subroutine packLocalData

  ! END OF INPUT ROUTINE
END subroutine input


! Finds two nearest angles to PROJ and returns their indices IND1 and
! IND2 and an interpolation fraction FRAC
!
subroutine lookupAngle(proj, angles, numViews, ind1, ind2, frac)
  implicit none
  real*4 proj, angles(*), frac
  integer*4 numViews, ind1, ind2, i
  ind1 = 0
  ind2 = 0
  do i = 1, numViews
    if (proj >= angles(i)) then
      if (ind1 == 0) ind1 = i
      if (angles(i) > angles(ind1)) ind1 = i
    else
      if (ind2 == 0) ind2 = i
      if (angles(i) < angles(ind2)) ind2 = i
    endif
  enddo
  frac = 0.
  if (ind1 == 0) then
    ind1 = ind2
  elseif (ind2 == 0) then
    ind2 = ind1
  else
    frac = (proj - angles(ind1)) / (angles(ind2) - angles(ind1))
  endif
  return
end subroutine lookupAngle


! Compute mean and SD of interior of a slice for SIRT report
!
subroutine sampleForReport(slice, lslice, kthick, iteration, sscale, &
    sflevl)
  use tiltvars
  implicit none
  real*4 slice(*), sscale, sflevl, avg, sd
  integer*4 lslice, kthick, iteration, iskip, ixlo, iylo, ierr
  integer*4 sampleMeanSD
  !
  iskip = (isliceEnd - isliceStart) / 10
  if (lslice < isliceStart + iskip .or. lslice > isliceEnd - iskip) return
  ixlo = iwidth / 10
  iylo = kthick / 4
  ierr = sampleMeanSD(slice, iwidth, kthick, 0.02, ixlo, iylo, iwidth - 2 * &
      ixlo, kthick - 2 * iylo, avg, sd)
  if (ierr .ne. 0) return
  reportVals(1, iteration) = reportVals(1, iteration) + &
      (avg + sflevl) * sscale
  reportVals(2, iteration) = reportVals(2, iteration) + sd * sscale
  reportVals(3, iteration) = reportVals(3, iteration) + 1
  return
end subroutine sampleForReport


! return indices to four local areas, and fractions to apply for each
! at location ix, iy in view iv, where ix and iy are indexes in
! the reconstruction adjusted to match coordinates of projections
!
subroutine local_factors(ix, iy, iv, ind1, ind2, ind3, ind4, f1, f2, f3, f4)
  !
  use tiltvars
  implicit none
  integer*4 ix, iy, iv, ind1, ind2, ind3, ind4, ixt, ixpos, iyt, iypos
  real*4 f1, f2, f3, f4, fx, fy
  !
  ixt = min(max(ix - ixStartWarp, 0), (nxWarp - 1) * idelXwarp)
  ixpos = min(ixt / idelXwarp + 1, nxWarp - 1)
  fx = float(ixt - (ixpos - 1) * idelXwarp) / idelXwarp
  iyt = min(max(iy - iyStartWarp, 0), (nyWarp - 1) * idelYwarp)
  iypos = min(iyt / idelYwarp + 1, nyWarp - 1)
  fy = float(iyt - (iypos - 1) * idelYwarp) / idelYwarp

  ind1 = indWarp(nxWarp * (iypos - 1) + ixpos) + iv
  ind2 = indWarp(nxWarp * (iypos - 1) + ixpos + 1) + iv
  ind3 = indWarp(nxWarp * iypos + ixpos) + iv
  ind4 = indWarp(nxWarp * iypos + ixpos + 1) + iv
  f1 = (1. -fy) * (1. -fx)
  f2 = (1. -fy) * fx
  f3 = fy * (1. -fx)
  f4 = fy * fx
  return
end subroutine local_factors


! Compute local projection factors at a position in a column for view iv:
! j is X index in the reconstruction, lslice is slice # in aligned stack
!
subroutine localProjFactors(j, lslice, iv, xprojf, xprojz, yprojf, &
    yprojz)
  use tiltvars
  implicit none
  integer*4 j, lslice, iv
  real*4 xprojf, xprojz, yprojf, yprojz
  integer*4 ind1, ind2, ind3, ind4, ixc
  real*4 f1, f2, f3, f4, xx, yy
  real*4 calf, salf, a11, a12, a21, a22, xadd, yadd, xalladd, yalladd
  real*4 calf2, salf2, a112, a122, a212, a222, xadd2, yadd2
  real*4 calf3, salf3, a113, a123, a213, a223, xadd3, yadd3
  real*4 calf4, salf4, a114, a124, a214, a224, xadd4, yadd4
  real*4 f1x, f2x, f3x, f4x, f1xy, f2xy, f3xy, f4xy
  real*4 f1y, f2y, f3y, f4y, f1yy, f2yy, f3yy, f4yy
  real*4 xp1f, xp1z, yp1f, xp2f, xp2z, yp2f, xp3f, xp3z, yp3f, xp4f, xp4z, yp4f
  real*4 cbeta, sbeta, cbeta2, sbeta2, cbeta3, sbeta3, cbeta4, sbeta4
  !
  ! get transform and angle adjustment
  !
  ixc = nint(j - xcenOut + xcenIn + axisXoffset)
  call local_factors(ixc, lslice, mapUsedView(iv), ind1, ind2, ind3, ind4, f1, f2, f3, f4)
  !
  ! get all the factors needed to compute a projection position
  ! from the four local transforms
  !
  cbeta = cWarpBeta(ind1)
  sbeta = sWarpBeta(ind1)
  calf = cWarpAlpha(ind1)
  salf = sWarpAlpha(ind1)
  a11 = fwarp(1, 1, ind1)
  a12 = fwarp(1, 2, ind1)
  a21 = fwarp(2, 1, ind1)
  a22 = fwarp(2, 2, ind1)
  xadd = fwarp(1, 3, ind1) + xcenIn - xcenIn * a11 - centerSlice * a12
  yadd = fwarp(2, 3, ind1) + centerSlice - xcenIn * a21 - centerSlice * a22
  !
  cbeta2 = cWarpBeta(ind2)
  sbeta2 = sWarpBeta(ind2)
  calf2 = cWarpAlpha(ind2)
  salf2 = sWarpAlpha(ind2)
  a112 = fwarp(1, 1, ind2)
  a122 = fwarp(1, 2, ind2)
  a212 = fwarp(2, 1, ind2)
  a222 = fwarp(2, 2, ind2)
  xadd2 = fwarp(1, 3, ind2) + xcenIn - xcenIn * a112 - centerSlice * a122
  yadd2 = fwarp(2, 3, ind2) + centerSlice - xcenIn * a212 - centerSlice * a222
  !
  cbeta3 = cWarpBeta(ind3)
  sbeta3 = sWarpBeta(ind3)
  calf3 = cWarpAlpha(ind3)
  salf3 = sWarpAlpha(ind3)
  a113 = fwarp(1, 1, ind3)
  a123 = fwarp(1, 2, ind3)
  a213 = fwarp(2, 1, ind3)
  a223 = fwarp(2, 2, ind3)
  xadd3 = fwarp(1, 3, ind3) + xcenIn - xcenIn * a113 - centerSlice * a123
  yadd3 = fwarp(2, 3, ind3) + centerSlice - xcenIn * a213 - centerSlice * a223
  !
  cbeta4 = cWarpBeta(ind4)
  sbeta4 = sWarpBeta(ind4)
  calf4 = cWarpAlpha(ind4)
  salf4 = sWarpAlpha(ind4)
  a114 = fwarp(1, 1, ind4)
  a124 = fwarp(1, 2, ind4)
  a214 = fwarp(2, 1, ind4)
  a224 = fwarp(2, 2, ind4)
  xadd4 = fwarp(1, 3, ind4) + xcenIn - xcenIn * a114 - centerSlice * a124
  yadd4 = fwarp(2, 3, ind4) + centerSlice - xcenIn * a214 - centerSlice * a224
  !
  f1x = f1 * a11
  f2x = f2 * a112
  f3x = f3 * a113
  f4x = f4 * a114
  f1xy = f1 * a12
  f2xy = f2 * a122
  f3xy = f3 * a123
  f4xy = f4 * a124
  ! fxfromy=f1*a12+f2*a122+f3*a123+f4*a124
  f1y = f1 * a21
  f2y = f2 * a212
  f3y = f3 * a213
  f4y = f4 * a214
  f1yy = f1 * a22
  f2yy = f2 * a222
  f3yy = f3 * a223
  f4yy = f4 * a224
  ! fyfromy=f1*a22+f2*a222+f3*a223+f4*a224
  xalladd = f1 * xadd + f2 * xadd2 + f3 * xadd3 + f4 * xadd4
  yalladd = f1 * yadd + f2 * yadd2 + f3 * yadd3 + f4 * yadd4
  !
  ! Each projection position is a sum of a fixed factor ("..f")
  ! and a factor that multiplies z ("..z")
  !
  xx = j - xcenOut
  yy = lslice - centerSlice
  xp1f = xx * cbeta + yy * salf * sbeta + xcenIn + axisXoffset
  xp1z = calf * sbeta + warpXZfac(ind1)
  xp2f = xx * cbeta2 + yy * salf2 * sbeta2 + xcenIn + axisXoffset
  xp2z = calf2 * sbeta2 + warpXZfac(ind2)
  xp3f = xx * cbeta3 + yy * salf3 * sbeta3 + xcenIn + axisXoffset
  xp3z = calf3 * sbeta3 + warpXZfac(ind3)
  xp4f = xx * cbeta4 + yy * salf4 * sbeta4 + xcenIn + axisXoffset
  xp4z = calf4 * sbeta4 + warpXZfac(ind4)

  yp1f = yy * calf + centerSlice
  yp2f = yy * calf2 + centerSlice
  yp3f = yy * calf3 + centerSlice
  yp4f = yy * calf4 + centerSlice
  !
  ! store the fixed and z-dependent component of the
  ! projection coordinates
  !
  xprojf = f1x * xp1f + f2x * xp2f + f3x * xp3f + f4x * xp4f + &
      f1xy * yp1f + f2xy * yp2f + f3xy * yp3f + f4xy * yp4f + xalladd
  xprojz = f1x * xp1z + f2x * xp2z + f3x * xp3z + f4x * xp4z - &
      (f1xy * (salf - warpYZfac(ind1)) + f2xy * (salf2 - warpYZfac(ind2)) + &
      f3xy * (salf3 - warpYZfac(ind3)) + f4xy * (salf4 - warpYZfac(ind4)))
  yprojf = f1y * xp1f + f2y * xp2f + f3y * xp3f + f4y * xp4f + &
      f1yy * yp1f + f2yy * yp2f + f3yy * yp3f + f4yy * yp4f + yalladd
  yprojz = f1y * xp1z + f2y * xp2z + f3y * xp3z + f4y * xp4z - &
      (f1yy * (salf - warpYZfac(ind1)) + f2yy * (salf2 - warpYZfac(ind2)) + &
      f3yy * (salf3 - warpYZfac(ind3)) + f4yy * (salf4 - warpYZfac(ind4)))
  return
end subroutine localProjFactors


! This is the former code for assessing backprojection positions,
! new method using localProjFactors verified to give the same result
! But this shows how the BP position can be computed directly
!!$  call local_factors (ixsam, itry, mapUsedView(iv), ind1, ind2, &
!!$      ind3, ind4, f1, f2, f3, f4)
!!$c
!!$c   for each position, find back - projection location
!!$c   transform if necessary, and use to get min and
!!$c   max slices needed to get this position
!!$c
!!$  xx = ixsam - xcenOut
!!$  yy = itry - centerSlice
!!$  zz = iy - ycenOut
!!$c   Global bp position
!!$  xp = xx * cosBeta(iv) + yy * sinAlpha(iv) * sinBeta(iv) + zz * (cosAlpha(iv) * sinBeta(iv) + xzfac(iv)) + &
!!$      xcenIn + axisXoffset
!!$  yp = yy * cosAlpha(iv) - zz * (sinAlpha(iv) - yzfac(iv)) + centerSlice
!!$c   Local position:
!!$  xp = xx * cWarpBeta(ind1) + yy * sWarpAlpha(ind1) * sWarpBeta(ind1) + &
!!$      zz * (cWarpAlpha(ind1) * sWarpBeta(ind1) + warpXZfac(ind1)) + xcenIn + axisXoffset
!!$  yp = yy * cWarpAlpha(ind1) - zz * (sWarpAlpha(ind1) - warpYZfac(ind1)) + centerSlice
!!$  call xfapply(fwarp(1, 1, ind1), xcenIn, centerSlice, xp, yp, xp, yp)
!!$  xp2 = xx * cWarpBeta(ind2) + yy * sWarpAlpha(ind2) * sWarpBeta(ind2) + &
!!$      zz * (cWarpAlpha(ind2) * sWarpBeta(ind2) + warpXZfac(ind2)) + xcenIn + axisXoffset
!!$  yp2 = yy * cWarpAlpha(ind2) - zz * (sWarpAlpha(ind2) - warpYZfac(ind2)) + centerSlice
!!$  call xfapply(fwarp(1, 1, ind2), xcenIn, centerSlice, xp2, yp2, xp2, yp2)
!!$  xp3 = xx * cWarpBeta(ind3) + yy * sWarpAlpha(ind3) * sWarpBeta(ind3) + &
!!$      zz * (cWarpAlpha(ind3) * sWarpBeta(ind3) + warpXZfac(ind3)) + xcenIn + axisXoffset
!!$  yp3 = yy * cWarpAlpha(ind3) - zz * (sWarpAlpha(ind3) - warpYZfac(ind3)) + centerSlice
!!$  call xfapply(fwarp(1, 1, ind3), xcenIn, centerSlice, xp3, yp3, xp3, yp3)
!!$  xp4 = xx * cWarpBeta(ind4) + yy * sWarpAlpha(ind4) * sWarpBeta(ind4) + &
!!$      zz * (cWarpAlpha(ind4) * sWarpBeta(ind4) + warpXZfac(ind4)) + xcenIn + axisXoffset
!!$  yp4 = yy * cWarpAlpha(ind4) - zz * (sWarpAlpha(ind4) - warpYZfac(ind4)) + centerSlice
!!$  call xfapply(fwarp(1, 1, ind4), xcenIn, centerSlice, xp4, yp4, xp4, yp4)
!!$  xp = f1 * xp + f2 * xp2 + f3 * xp3 + f4 * xp4
!!$  yp = f1 * yp + f2 * yp2 + f3 * yp3 + f4 * yp4


! Finds the point at centered Z coordinate zz projecting to
! xproj, yproj in view iv of original projections.  xx is X index in
! reconstruction, yy is slice number in original projections
!
subroutine findProjectingPoint(xproj, yproj, zz, iv, xx, yy)
  use tiltvars
  implicit none
  real*4 xproj, yproj, zz, xx, yy
  integer*4 iv, iter, ifdone, ixassay, iyassay
  real*4 xprojf11, xprojz11, yprojf11, yprojz11, xprojf21, xprojz21, &
      yprojf21, yprojz21, xprojf12, xprojz12, yprojf12, yprojz12
  real*4 xp11, yp11, xp12, yp12, xp21, yp21, xerr, yerr, dxpx, dxpy, dypx
  real*4 dypy, fx, fy, den
  !
  iter = 1
  ifdone = 0
  do while (ifdone == 0 .and. iter <= 5)
    ixassay = floor(xx)
    iyassay = floor(yy)
    call localProjFactors(ixassay, iyassay, iv, xprojf11, xprojz11, &
        yprojf11, yprojz11)
    call localProjFactors(ixassay + 1, iyassay, iv, xprojf21, xprojz21, &
        yprojf21, yprojz21)
    call localProjFactors(ixassay, iyassay + 1, iv, xprojf12, xprojz12, &
        yprojf12, yprojz12)
    xp11 = xprojf11 + xprojz11 * zz
    yp11 = yprojf11 + yprojz11 * zz
    xp21 = xprojf21 + xprojz21 * zz
    yp21 = yprojf21 + yprojz21 * zz
    xp12 = xprojf12 + xprojz12 * zz
    yp12 = yprojf12 + yprojz12 * zz
    xerr = xproj - xp11
    yerr = yproj - yp11
    dxpx = xp21 - xp11
    dxpy = xp12 - xp11
    dypx = yp21 - yp11
    dypy = yp12 - yp11
    den = dxpx * dypy - dxpy * dypx
    fx = (xerr * dypy - yerr * dxpy) / den
    fy = (dxpx * yerr - dypx * xerr) / den
    xx = ixassay + fx
    yy = iyassay + fy
    if (fx > -0.1 .and. fx < 1.1 .and. fy > -0.1 .and. fy < 1.1) &
        ifdone = 1
    iter = iter + 1
  enddo
  return
end subroutine findProjectingPoint

!
! Compute space needed for cosine stretched data
!
subroutine set_cos_stretch()
  use tiltvars
  implicit none
  integer*4 lsmin, lsmax, iv, ix, iy, lslice
  real*4 tanal, xpmax, xpmin, zz, zpart, yy, xproj
  ! make the indexes be bases, numbered from 0
  !
  indStretchLine(1) = 0
  lsmin = min(isliceEnd, isliceStart)
  lsmax = max(isliceEnd, isliceStart)
  if (ifAlpha < 0) then
    !
    ! New-style X tilting: SET MINIMUM NUMBER OF INPUT SLICES HERE
    !
    lsmin = centerSlice + (lsmin - centerSlice) * cosAlpha(1) + yOffset * sinAlpha(1) - &
        0.5 * ithickOut * abs(sinAlpha(1)) - 1.
    lsmax = centerSlice + (lsmax - centerSlice) * cosAlpha(1) + yOffset * sinAlpha(1) + &
        0.5 * ithickOut * abs(sinAlpha(1)) + 2.
    tanal = sinAlpha(1) / cosAlpha(1)
    lsmin = max(1, lsmin)
    lsmax = min(lsmax, nyProj)
  endif
  do iv = 1, numViews
    xpmax = 1
    xpmin = nxProj
    !
    ! find min and max position of 8 corners of reconstruction
    !
    do ix = 1, iwidth, iwidth - 1
      do iy = 1, ithickBP, ithickBP - 1
        do lslice = lsmin, lsmax, max(1, lsmax - lsmin)
          ZZ = (IY - ycenOut) * compress(iv)
          if (ifAlpha < 0) zz = compress(iv) * &
              (iy - (ycenOut - nint(tanal * (lslice - centerSlice))))
          if (ifAlpha <= 0) then
            zPART = zz * sinBeta(iv) + xcenIn + axisXoffset
          else
            yy = lslice - centerSlice
            zpart = yy * sinAlpha(iv) * sinBeta(iv) + zz * (cosAlpha(iv) * sinBeta(iv) + &
                xzfac(iv)) + xcenIn + axisXoffset
          endif
          xproj = zpart + (ix - xcenOut) * cosBeta(iv)
          xpmin = max(1., min(xpmin, xproj))
          xpmax = min(float(nxProj), max(xpmax, xproj))
        enddo
      enddo
    enddo
    ! print *,iv, xpmin, xpmax
    !
    ! set up extent and offset of stretches
    !
    stretchOffset(iv) = xpmin / cosBeta(iv) - 1. / interpFacStretch
    nxStretched(iv) = interpFacStretch * (xpmax - xpmin) / cosBeta(iv) + 2.
    indStretchLine(iv + 1) = indStretchLine(iv) + nxStretched(iv)
    ! print *,iv, xpmin, xpmax, stretchOffset(iv), nxStretched(iv), indStretchLine(iv)
  enddo
  return
end subroutine set_cos_stretch


! Determine starting and ending input slice needed to reconstruct
! each output slice, as well as the maximum needed over all slices
! for a series of numbers of output slices up to numEval
!
subroutine setNeededSlices(maxNeeds, numEval)
  use tiltvars
  implicit none
  integer*4 numEval, maxNeeds(*)
  integer*4 lsmin, lsmax, ierr, itry, nxassay, minslice, ixassay
  integer*4 maxslice, iassay, ixsam, iv, iy, iyp
  real*4 dxassay, dxtmp, xx, yy, zz, xp, yp
  real*4 xprojf, xprojz, yprojf, yprojz, xproj, yproj
  lsmin = isliceStart
  lsmax = isliceEnd
  if (ifAlpha < 0) then
    lsmin = centerSlice + (isliceStart - centerSlice) * cosAlpha(1) +  &
        yOffset * sinAlpha(1) - 0.5 * ithickOut * abs(sinAlpha(1)) - 1.
    lsmax = centerSlice + (isliceEnd - centerSlice) * cosAlpha(1) +  &
        yOffset * sinAlpha(1) + 0.5 * ithickOut * abs(sinAlpha(1)) + 2.
    lsmin = max(1, lsmin)
    lsmax = min(lsmax, nyProj)
  endif
  indNeededBase = lsmin - 1
  numNeedSE = lsmax - indNeededBase
  allocate(neededStarts(numNeedSE), neededEnds(numNeedSE), stat = ierr)
  if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS needStarts/needEnds')

  do itry = lsmin, lsmax

    if (ifAlpha <= 0 .and. nxWarp == 0) then
      !
      ! regular case is simple: just need the current slice
      !
      neededStarts(itry - indNeededBase) = itry
      neededEnds(itry - indNeededBase) = itry
    else
      !
      ! for old-style X-tilt or local alignment, determine what
      ! slices are needed by sampling
      ! set up sample points: left and right if no warp,
      ! or half the warp spacing
      !
      if (nxWarp == 0) then
        nxassay = 2
        dxassay = iwidth - 1
      else
        dxtmp = idelXwarp / 2
        nxassay = max(2., iwidth / dxtmp + 1.)
        dxassay = (iwidth - 1.) / (nxassay - 1.)
      endif
      !
      ! sample top and bottom at each position
      !
      minslice = nyProj + 1
      maxslice = 0
      do iassay = 1, nxassay
        ixassay = nint(1 + (iassay - 1) * dxassay)
        do iv = 1, numViews
          if (.not. recReproj) then
            ixsam = nint(ixassay - xcenOut + xcenIn + axisXoffset)
            if (nxWarp .ne. 0) then
              call localProjFactors(ixassay, itry, iv, xprojf, &
                  xprojz, yprojf, yprojz)
            endif
            do iy = 1, ithickBP, ithickBP - 1
              !
              ! for each position, find back-projection location
              ! transform if necessary, and use to get min and
              ! max slices needed to get this position
              !
              xx = ixsam - xcenOut
              yy = itry - centerSlice
              zz = iy - ycenOut
              xp = xx * cosBeta(iv) + yy * sinAlpha(iv) * sinBeta(iv) + &
                  zz * (cosAlpha(iv) * sinBeta(iv) + xzfac(iv)) + xcenIn + axisXoffset
              yp = yy * cosAlpha(iv) - zz * (sinAlpha(iv) - yzfac(iv)) + centerSlice
              if (nxWarp .ne. 0) then
                xp = xprojf + xprojz * zz
                yp = yprojf + yprojz * zz
              endif
              iyp = max(1., yp)
              minslice = min(minslice, iyp)
              maxslice = max(maxslice, min(nyProj, iyp + 1))
              ! if (debug) print *,xx, yy, zz, iyp, minslice, maxslice
            enddo
          else
            !
            ! Projections: get Y coordinate in original projection
            ! if local, get the X coordinate in reconstruction too
            ! then get the refinement
            xproj = ixassay + xprojOffset
            yproj = itry + yprojOffset
            do iy = 1, ithickReproj, ithickReproj - 1
              zz = iy + minYreproj - 1 - ycenOut
              yy = (yproj + zz * (sinAlpha(iv) - yzfac(iv)) - centerSlice) /  &
                  cosAlpha(iv) + centerSlice
              if (nxWarp .ne. 0) then
                xx = (xproj - yy * sinAlpha(iv) * sinBeta(iv) - zz * (cosAlpha(iv) * &
                    sinBeta(iv) + xzfac(iv)) - xcenIn - axisXoffset) / cosBeta(iv) + &
                    xcenOut
                call findProjectingPoint(xproj, yproj, zz, iv, xx, yy)
              endif
              iyp = max(1., yy - yprojOffset)
              minslice = min(minslice, iyp)
              maxslice = max(maxslice, min(nyProj, iyp + 1))
            enddo
          endif
        enddo
      enddo
      !
      ! set up starts and ends
      !
      neededStarts(itry - indNeededBase) = max(1, minslice)
      neededEnds(itry - indNeededBase) = min(nyProj, maxslice)
    endif
  enddo
  !
  ! Count maximum # of slices needed for number of slices to be computed
  do iv = 1, numEval
    maxNeeds(iv) = 0
    do iy = lsmin, lsmax + 1 - iv
      maxslice = neededEnds(iy + iv - 1 - indNeededBase) + 1 -  &
          neededStarts(iy - indNeededBase)
      maxNeeds(iv) = max(maxNeeds(iv), maxslice)
    enddo
  enddo
  return
end subroutine setNeededSlices


! Allocate array, trying to get enough to do numEval slices without
! reloading any data, based on the numbers in maxNeeds, and trying fewer
! slices down to minLoad if that fails.  MinMemory is the minimum amount
! it will allocate.
!
integer*4 function allocateArray(maxNeeds, numEval, minLoad, minMemory)
  use tiltvars
  implicit none
  integer*4 maxNeeds(*), numEval, minLoad, minMemory, ierr, i
  integer(kind = 8) memNeed, minNeed
  minNeed = inPlaneSize
  minNeed = indLoadBase + ipExtraSize + minNeed * &
      (neededEnds(numNeedSE) + 1 - neededStarts(1)) + 32
  if (minNeed > minMemory) minNeed = minMemory
  do i = numEval, minLoad, -1
    memNeed = inPlaneSize
    memNeed = indLoadBase + ipExtraSize + memNeed * maxNeeds(i) + 32
    memNeed = max(memNeed, minNeed)
    if (memNeed < 2147000000) then
      allocate(array(memNeed), stat = ierr)
      if (ierr == 0) then
        maxStack = memNeed
        numPlanes = (maxStack - indLoadBase - ipExtraSize + 1) / inPlaneSize
        allocateArray = i
        write(*,'(/,a,i5,a)') 'Allocated', nint(maxStack / (1024 * 256.)), &
            ' MB for stack array'
        return
      endif
    endif
  enddo
  allocateArray = 0
  return
end function allocateArray


subroutine reproject(array, nxs, nys, nxout, sinang, cosang, xRayStart, &
    yRayStart, numPixInRay, maxRayPixels, fill, projLine, linear, noScale)
  implicit none
  integer*4 nxs, nys, nxout, numPixInRay(*), maxRayPixels, linear, noScale
  real*4 array(nxs, nys), xRayStart(*), yRayStart(*), fill, projLine(*)
  integer*4 ixout, iray, ixr, iyr, nraypts, idir
  real*4 sinang, cosang, rayfac, rayadd, xray, yray, pixtmp, fullfill
  real * 4 dx, dy, v2, v4, v6, v8, v5, a, b, c, d
  !
  rayfac = 1. / maxRayPixels
  fullfill = fill
  if (noScale .ne. 0) then
    rayfac = 1.
    fullfill = fill * maxRayPixels
  endif
  do ixout = 1, nxout
    projLine(ixout) = fullfill
    nraypts = numPixInRay(ixout)
    if (nraypts > 0) then
      pixtmp = 0.
      if (sinang .ne. 0.) then
        if (linear == 0) then
          do iray = 0, nraypts - 1
            xray = xRayStart(ixout) + iray * sinang
            yray = yRayStart(ixout) + iray * cosang
            ixr = nint(xray)
            iyr = nint(yray)
            dx = xray - ixr
            dy = yray - iyr
            v2 = array(ixr, iyr - 1)
            v4 = array(ixr - 1, iyr)
            v5 = array(ixr, iyr)
            v6 = array(ixr + 1, iyr)
            v8 = array(ixr, iyr + 1)
            !
            A = (V6 + V4) * .5 - V5
            B = (V8 + V2) * .5 - V5
            C = (V6 - V4) * .5
            D = (V8 - V2) * .5
            pixtmp = pixtmp + A * DX * DX + B * DY * DY + C * DX + D * DY + V5
          enddo
        else
          do iray = 0, nraypts - 1
            xray = xRayStart(ixout) + iray * sinang
            yray = yRayStart(ixout) + iray * cosang
            ixr = xray
            iyr = yray
            dx = xray - ixr
            dy = yray - iyr
            pixtmp = pixtmp + (1 - dy) * &
                ((1. -dx) * array(ixr, iyr) + dx * array(ixr + 1, iyr)) + dy * &
                ((1. -dx) * array(ixr, iyr + 1) + dx * array(ixr + 1, iyr + 1))
          enddo
        endif
      else
        !
        ! vertical projection
        !
        ixr = nint(xRayStart(ixout))
        iyr = nint(yRayStart(ixout))
        idir = sign(1., cosang)
        do iray = 0, nraypts - 1
          pixtmp = pixtmp + array(ixr, iyr + idir * iray)
        enddo
      endif

      rayadd = rayfac * (maxRayPixels - nraypts) * fill
      projLine(ixout) = rayfac * pixtmp + rayadd
    endif
  enddo
  return
end subroutine reproject

! Reprojects slices from lsStart to lsEnd and writes the reprojections
! Fewer slices may be done if on the GPU, and the ending slice done is
! returned in lsEnd.  INLOADSTR and INLOADEND are slice numbers of
! started and ending slices loaded; min/max/sum densities are maintained
! in DMIN, DMAX, and DTOT8
!
subroutine reprojectRec(lsStart, lsEnd, inloadstr, inloadend, DMIN, DMAX, &
    DTOT8)
  use tiltvars
  implicit none
  integer*4 lsStart, lsEnd, inloadstr, inloadend
  real*4 dmin, dmax
  integer*4 iv, ix, iy, iz, ixp, line, i, iys, ind
  integer*4 ind1, ind2, ind3, ind4, load
  real*4 calf, salf, cbeta, sbeta, delz, delx, fz, omfz, zz, xx, fx
  real*4 omfx, yy, fy, omfy, xproj, yproj, d11, d12, d21, d22
  real*4 f1, f2, f3, f4, xxgood, yygood, zzgood
  real*4 ytol, xprojMin, xprojMax, xjump, zjump, dely, diffxmax, diffymax
  integer*4 indbase, nxload, ixc, lastZdone
  integer*4 ijump, njump, lgpuEnd, lineBase
  real*4 ycenAdj
  real*8 sum, dtot8, walltime, tstart, tcumul
  logical*4 tryjump
  real*4 reprojDelz
  integer*4 gpuReproject, gpuReprojLocal

  ytol = 3.05
  xjump = 5.0
  nxload = maxXload + 1 - minXload
  tstart = walltime()
  tcumul = 0.
  ycenAdj = ycenOut - (minYreproj - 1)
  !
  if (useGPU .and. loadGpuStart > 0) then
    ijump = 1
    !
    ! GPU REPROJECTION: Find last slice that can be done
    do lgpuEnd = lsEnd, lsStart, -1
      if (neededEnds(lgpuEnd) <= loadGpuEnd) then
        ijump = 0
        exit
      endif
    enddo
    if (ijump == 0) then
      !
      ! Loop on views; do non-local case first
      do iv = 1, numViews
        if (nxWarp == 0) then
          delz = reprojDelz(sinBeta(iv), cosBeta(iv), sinAlpha(iv), cosAlpha(iv), &
              xzfac(iv), yzfac(iv))
          ijump = gpuReproject(reprojLines, sinBeta(iv), cosBeta(iv), sinAlpha(iv), &
              cosAlpha(iv), xzfac(iv), yzfac(iv), delz, lsStart, lgpuEnd, &
              ithickReproj, xcenOut, xcenIn + axisXoffset, minXreproj, xprojOffset, &
              ycenOut, minYreproj, yprojOffset, centerSlice, ifAlpha, dmeanIn)
        else
          !
          ! GPU with local alignments: fill warpDelz array for all lines
          do line = lsStart, lgpuEnd
            call fillWarpDelz(warpDelz(1 + (line - lsStart) * numWarpDelz))
          enddo
          !
          ! Get the xprojmin and max adjusted by 5
          xprojMin = 10000000.
          xprojMax = 0.
          do load = inloadstr, inloadend
            iys = nint(load + yprojOffset)
            do ix = 1, nxload, nxload - 1
              call localProjFactors(ix + minXload - 1, iys, iv, sbeta, &
                  cbeta, salf, calf)
              salf = sbeta + (1 - ycenAdj) * cbeta
              calf = sbeta + (ithickReproj - ycenAdj) * cbeta
              xprojMin = min(xprojMin, salf - 5., calf - 5.)
              xprojMax = max(xprojMax, salf + 5., calf + 5.)
            enddo
          enddo
          ! print *,'xprojmin, max', xprojMin, xprojMax
          !
          ! Do it
          ijump = gpuReprojLocal(reprojLines, sinBeta(iv), cosBeta(iv), sinAlpha(iv), &
              cosAlpha(iv), xzfac(iv), yzfac(iv), nxWarp, nyWarp, ixStartWarp, &
              iyStartWarp, idelXwarp, idelYwarp, warpDelz, numWarpDelz, &
              dxWarpDelz, xprojMin, xprojMax, lsStart, lgpuEnd, &
              ithickReproj, iv, xcenOut, xcenIn, axisXoffset, minXload, xprojOffset, &
              ycenAdj, yprojOffset, centerSlice, dmeanIn)
        endif
        if (ijump .ne. 0) exit
        tcumul = tcumul + walltime() - tstart
        call writeReprojLines(iv, lsStart, lgpuEnd, DMIN, DMAX, DTOT8)
        tstart = walltime()
      enddo
      if (ijump == 0) then
        if (debug) write(*, '(a,f8.4)') 'GPU reprojection time', tcumul
        lsEnd = lgpuEnd
        return
      endif
    endif
  endif
  !
  ! CPU REPROJECTION: loop on views; first handle non-local alignments
  do iv = 1, numViews
    if (nxWarp == 0) then
      !
      ! Get the delta z for this view
      calf = cosAlpha(iv)
      salf = sinAlpha(iv)
      cbeta = cosBeta(iv)
      sbeta = sinBeta(iv)
      delz = reprojDelz(sbeta, cbeta, salf, calf, xzfac(iv), yzfac(iv))
      ! print *,sbeta, cbeta, salf, calf, xzfac(iv), yzfac(iv)
      ! print *,delx, delz
      !
      ! Loop on the output lines to be done
      do line = lsStart, lsEnd
        lineBase = (line - lsStart) * iwidth + 1
        call reprojOneAngle(array(indLoadBase), reprojLines(lineBase), &
            inLoadStr, inLoadEnd, line, cbeta, sbeta, calf, salf, &
            delz, iwidth, ithickReproj, inPlaneSize, nxload, minXreproj, &
            minYreproj, xprojOffset, yprojOffset, xcenOut, ycenOut, &
            xcenIn + axisXoffset, centerSlice, ifAlpha, xzfac(iv), yzfac(iv), dmeanIn)
      enddo
    else
      !
      ! LOCAL ALIGNMENTS
      !
      ! first step: precompute all the x/yprojf/z  for all slices
      ! general BUG ycenAdj replaces ycenOut - minYreproj (off by 1)
      xprojMin = 10000000.
      xprojMax = 0.
      do load = inloadstr, inloadend
        indbase = nxload * (load - inloadstr)
        iys = nint(load + yprojOffset)
        do ix = 1, nxload
          ind = indbase + ix
          call localProjFactors(ix + minXload - 1, iys, iv, xprojfs(ind), &
              xprojzs(ind), yprojfs(ind), yprojzs(ind))
          if (ix == 1) xprojMin = min(xprojMin, &
              xprojfs(ind) + (1 - ycenAdj) * xprojzs(ind),  xprojfs(ind) &
              + (ithickReproj - ycenAdj) * xprojzs(ind))
          if (ix == nxload) xprojMax = max(xprojMax, &
              xprojfs(ind) + (1 - ycenAdj) * xprojzs(ind),  xprojfs(ind) &
              + (ithickReproj - ycenAdj) * xprojzs(ind))
        enddo
      enddo
      ! print *,'xprojmin, max', xprojMin, xprojMax
      !
      ! loop on lines to be done
      do line = lsStart, lsEnd
        lineBase = (line - lsStart) * iwidth
        call fillWarpDelz(warpDelz)
        ! print *,iv, line, inloadstr, inloadend
        !
        ! loop on pixels across line
        yproj = line + yprojOffset
        do ixp = 1, iwidth
          !
          ! Get x projection coord, starting centered Z coordinate, and
          ! approximate x and y coordinates
          ! xproj, yproj are coordinates in original projections
          ! Equations relate them to coordinates in reconstruction
          ! and then X coordinate is adjusted to be a loaded X index
          ! and Y coordinate is adjusted to be a slice of reconstruction
          xproj = ixp + xprojOffset
          zz = 1. -ycenAdj
          sum = 0.
          ! print *,ixp, xproj, yproj, xx, yy
          ! BUG these lines needed to be swapped and yprojOffset deferred
          yy = (yproj + zz * (sinAlpha(iv) - yzfac(iv)) - centerSlice) / cosAlpha(iv) + &
              centerSlice
          xx = (xproj - yy * sinAlpha(iv) * sinBeta(iv) -  &
              zz * (cosAlpha(iv) * sinBeta(iv) + &
              xzfac(iv)) - xcenIn - axisXoffset) / cosBeta(iv) + xcenOut - (minXload - 1)
          yy = yy - yprojOffset
          !
          ! Move on ray up in Z
          lastZdone = 0
          tryjump = .true.
          diffxmax = 0
          diffymax = 0
          ! BUG ?? Surely this should be abs(sinBeta(iv))
          zjump = xjump * cosBeta(iv) / max(0.2, abs(sinBeta(iv)))
          do while (zz < ithickReproj + 1 - ycenAdj .and. &
              lastZdone == 0)
            if (xproj < xprojMin - 5. .or. xproj > xprojMax + 5.) then
              sum = sum + dmeanIn
            else
              call loadedProjectingPoint(xproj, yproj, zz, nxload, &
                  inloadstr, inloadend, xx, yy)
              !
              ! If X or Y is out of bounds, fill with mean
              if (yy < inloadstr - ytol .or. yy > inloadend + ytol &
                  .or. xx < 1. .or. xx >= nxload) then
                sum = sum + dmeanIn
              else
                !
                ! otherwise, get x, y, z indexes, clamp y to limits, allow
                ! a fractional Z pixel at top of volume
                ix = xx
                fx = xx - ix
                omfx = 1. - fx
                yy = max(float(inloadstr), min(inloadend - 0.01, yy))
                iy = yy
                fy = yy - iy
                omfy = 1. - fy
                ! BUG ????  Shouldn't this be + ycenOut - minYreproj?
                iz = max(1., zz + ycenAdj)
                fz = zz + ycenAdj - iz
                omfz = 1. - fz
                if (iz == ithickReproj) then
                  iz = iz - 1
                  fz = omfz
                  omfz = 0.
                  lastZdone = 1
                endif
                !
                ! Do the interpolation
                d11 = omfx * omfy
                d12 = omfx * fy
                d21 = fx * omfy
                d22 = fx * fy
                ind = indLoadBase + inPlaneSize * (iy - inloadstr) + (iz - 1) * nxload &
                    + ix - 1
                sum = sum + omfz * (d11 * array(ind) &
                    + d12 * array(ind + inPlaneSize) + d21 * array(ind + 1) &
                    + d22 * array(ind + inPlaneSize + 1)) &
                    + fz * (d11 * array(ind + nxload) &
                    + d12 * array(ind + inPlaneSize + nxload) &
                    + d21 * array(ind + 1 + nxload) &
                    + d22 * array(ind + inPlaneSize + 1 + nxload))
                !
                do while(tryjump)
                  !
                  ! If jumping is OK, save the current position and compute
                  ! how many steps can be jumped, stopping below the top
                  xxgood = xx
                  yygood = yy
                  zzgood = zz
                  ind = max(1., min(float(numWarpDelz), xx / dxWarpDelz))
                  delz = warpDelz(ind)
                  njump = zjump / delz
                  if (zz + zjump > ithickReproj - ycenAdj - 1) then
                    njump = (ithickReproj - ycenAdj - 1 - zz) / delz
                    tryjump = .false.
                  endif
                  if (njump > 0) then
                    !
                    ! Make the jump, find the projecting point;
                    ! if it's out of bounds restore last point
                    zz = zz + njump * delz
                    xx = xx + njump * sinBeta(iv)
                    call loadedProjectingPoint(xproj, yproj, zz, &
                        nxload, inloadstr, inloadend, xx, yy)
                    if (yy < inloadstr .or. yy > inloadend .or. &
                        xx < 1. .or. xx >= nxload) then
                      njump = 0
                      xx = xxgood
                      yy = yygood
                      zz = zzgood
                      tryjump = .false.
                    else
                      delx = (xx - xxgood) / njump
                      dely = (yy - yygood) / njump
                    endif
                  endif
                  !
                  ! Loop on points from last one to final one
                  do ijump = 1, njump
                    xx = xxgood + ijump * delx
                    yy = yygood + ijump * dely
                    zz = zzgood + ijump * delz
                    ix = xx
                    fx = xx - ix
                    omfx = 1. - fx
                    iy = min(int(yy), inloadend - 1)
                    fy = yy - iy
                    omfy = 1. - fy
                    ! BUG again, need ycenAdj
                    iz = zz + ycenAdj
                    fz = zz + ycenAdj - iz
                    omfz = 1. - fz
                    d11 = omfx * omfy
                    d12 = omfx * fy
                    d21 = fx * omfy
                    d22 = fx * fy
                    ind = indLoadBase + inPlaneSize * (iy - inloadstr) + (iz - 1) * &
                        nxload + ix - 1
                    sum = sum + omfz * (d11 * array(ind) &
                        + d12 * array(ind + inPlaneSize) + d21 * array(ind + 1) &
                        + d22 * array(ind + inPlaneSize + 1)) &
                        + fz * (d11 * array(ind + nxload) &
                        + d12 * array(ind + inPlaneSize + nxload) &
                        + d21 * array(ind + 1 + nxload) &
                        + d22 * array(ind + inPlaneSize + 1 + nxload))
                    ! fx = xx
                    ! fy = yy
                    ! call loadedProjectingPoint(xproj, yproj, zz, &
                    ! nxload, inloadstr, inloadend, fx, fy)
                    ! diffxmax = max(diffxmax , abs(fx - xx))
                    ! diffymax = max(diffymax , abs(fy - yy))
                  enddo
                enddo
              endif
            endif
            !
            ! Adjust Z by local factor, move X approximately for next pixel
            ind = max(1., min(float(numWarpDelz), xx / dxWarpDelz))
            zz = zz + warpDelz(ind)
            xx = xx + sinBeta(iv)
          enddo
          reprojLines(lineBase + ixp) = sum
          ! write (*,'(i5,2f10.4)') ixp, diffxmax, diffymax
        enddo
      enddo
    endif
    tcumul = tcumul + walltime() - tstart
    call writeReprojLines(iv, lsStart, lsEnd, DMIN, DMAX, DTOT8)
    tstart = walltime()
  enddo
  if (debug) write(*, '(a,f8.4)') 'CPU reprojection time', tcumul

CONTAINS
  !
  ! compute delta z as function of X across the loaded slice
  ! which is not ideal since the data will not be coming from slice
  subroutine fillWarpDelz(wrpdlz)
    real*4 wrpdlz(*)
    iys = nint(line + yprojOffset)
    do i = 1, numWarpDelz
      xx = 1 + dxWarpDelz * (i - 1)
      ixc = nint(xx + minXload - 1 - xcenOut + xcenIn + axisXoffset)
      call local_factors(ixc, iys, iv, ind1, ind2, ind3, ind4, f1, f2, &
          f3, f4)
      wrpdlz(i) = f1 * reprojDelz(sWarpBeta(ind1), cWarpBeta(ind1), &
          sWarpAlpha(ind1), cWarpAlpha(ind1), warpXZfac(ind1), warpYZfac(ind1)) &
          + f2 * reprojDelz(sWarpBeta(ind2), cWarpBeta(ind2), &
          sWarpAlpha(ind2), cWarpAlpha(ind2), warpXZfac(ind2), warpYZfac(ind2)) &
          + f3 * reprojDelz(sWarpBeta(ind3), cWarpBeta(ind3), &
          sWarpAlpha(ind3), cWarpAlpha(ind3), warpXZfac(ind3), warpYZfac(ind3)) &
          + f4 * reprojDelz(sWarpBeta(ind4), cWarpBeta(ind4), &
          sWarpAlpha(ind4), cWarpAlpha(ind4), warpXZfac(ind4), warpYZfac(ind4))
    enddo
    ! print *,'got delz eg:', wrpdlz(1), wrpdlz(numWarpDelz/2), &
    ! wrpdlz(numWarpDelz)
  end subroutine fillWarpDelz

end subroutine reprojectRec

! Finds loaded point that projects to xproj, yproj at centered Z value
! zz, using stored values for [xy]zfac[fv].
! Takes starting value in xx, yy and returns found value.
! X coordinate needs to be a loaded X index
! Y coordinate yy is in slices of reconstruction, yproj in original proj
!
subroutine loadedProjectingPoint(xproj, yproj, zz, nxload, &
    inloadstr, inloadend, xx, yy)
  use tiltvars
  implicit none
  real*4 xproj, yproj, zz, xx, yy
  integer*4 nxload, inloadstr, inloadend
  integer*4 iter, ifdone, ind, ix, iy, ifout, i
  real*4 xp11, yp11, xp12, yp12, xp21, yp21, xerr, yerr, dypx, dxpy, dxpx
  real*4 dypy, den, fx, fy
  ! logical*4 dbout
  ! dbout = abs(xproj - 700.) < 3 .and. abs(zz) < 3
  ! dbout = .false.
  ! print *,'Finding proj pt to', xproj, yproj, zz
  iter = 0
  ifdone = 0
  do while (ifdone == 0 .and. iter < 5)
    ix = floor(xx)
    iy = floor(yy)
    ifout = 0
    if (ix < 1 .or. ix >= nxload .or. iy < inloadstr .or. &
        iy >= inloadend) then
      ifout = 1
      ix = min(nxload - 1, max(1, ix))
      iy = min(inloadend - 1, max(inloadstr, iy))
    endif
    ind = nxload * (iy - inloadstr) + ix
    xp11 = xprojfs(ind) + xprojzs(ind) * zz
    yp11 = yprojfs(ind) + yprojzs(ind) * zz
    xp21 = xprojfs(ind + 1) + xprojzs(ind + 1) * zz
    yp21 = yprojfs(ind + 1) + yprojzs(ind + 1) * zz
    xp12 = xprojfs(ind + nxload) + xprojzs(ind + nxload) * zz
    yp12 = yprojfs(ind + nxload) + yprojzs(ind + nxload) * zz
    ! write(*,101) 'facs', (xprojfs(i), xprojzs(i), yprojfs(i), &
    ! yprojzs(i), i=ind, ind+1)
    ! write(*,101) 'xps', xx, yy, zz, xp11, yp11, xp21, yp21, xp12, yp12
    !101     format(a,9f8.2)
    xerr = xproj - xp11
    yerr = yproj - yp11
    dxpx = xp21 - xp11
    dxpy = xp12 - xp11
    dypx = yp21 - yp11
    dypy = yp12 - yp11
    den = dxpx * dypy - dxpy * dypx
    fx = (xerr * dypy - yerr * dxpy) / den
    fy = (dxpx * yerr - dypx * xerr) / den
    ! write(*,101) 'dx,err,f', dxpx, dxpy, dypx, dypy, den, xerr, yerr, fx, fy
    xx = ix + fx
    yy = iy + fy
    if (fx > -0.1 .and. fx < 1.1 .and. fy > -0.1 .and. &
        fy < 1.1) ifdone = 1
    if (ifout .ne. 0 .and. (iter > 0 .or.  xx < 0. .or. &
        xx > nxload + 1 .or. yy < inloadstr - 1. .or. &
        yy > inloadend + 1.)) ifdone = 1
    iter = iter + 1
  enddo
  return
end subroutine loadedProjectingPoint

! reprojOneAngle reprojects a line at one angle from projection data
! in ARRAY into reprojLines.  LINE is the Y value in projections, Z value
! in reconstructed slices.  ARRAY is loaded with slices from inLoadStr to
! inLoadEnd, with a slice size of IPLANE and NXLOAD values on each line
! in X.  CBETA, SBETA, CALF, SALF are cosines and sines of tilt angle
! and alpha tilt.  IFALPHA is non-zero for tilt araound the X axis.
! IWIDE is the width and ithickReproj is the thickness to reprojection;
! the coordinates of the region in the slice to reproject start at
! minXreproj, minYreproj and are offset from center by xprojOffset,
! yprojOffset.  DELZ is the spacing in Y at which to sample points along
! a projection ray. XCEN, YCEN are center coordinates of output;
! xcenPdelxx is xcenIn + axisXoffset; SLICEN is the middle coordinate in Z
! XZFACV, YZFACV are Z factors for this view, and PMEAN is the mean of
! the slices for filling.
! Many of these names are the same as in the rest of the program and
! have the same meaning, but they are passed in, not taken from the
! tiltvars module, so that this can be called in cases other than the
! reprojectRec where they are defined.
!
subroutine reprojOneAngle(array, reprojLines, inLoadStr, inLoadEnd, line, &
    cbeta, sbeta, calf, salf, delzIn, iwidth, ithickReproj, inPlaneSize, &
    nxload, minXreproj, minYreproj, xprojOffset, yprojOffset, xcenOut, ycenOut, &
    xcenPdelxx, centerSlice, ifAlpha, xzfacv, yzfacv, dmeanIn)
  implicit none
  real*4 array(*), reprojLines(*), cbeta, sbeta, calf, salf, delx, delzIn
  integer*4 inLoadStr, inLoadEnd, line, iwidth, ithickReproj, inPlaneSize, nxload
  integer*4 minXreproj, minYreproj, ifAlpha
  real*4 xprojOffset, yprojOffset, centerSlice, xzfacv, yzfacv, dmeanIn
  real*4 xcenOut, ycenOut, xcenPdelxx
  !
  integer*4 ix, iz, i, numz, kz, iys, ixnd, ixst, ind, indbase
  real*4 znum, fz, omfz, zz, xx, fx, ytol, pfill, salfsbetdcal, xcenAdj, ysl, dely
  real*4 omfx, yy, fy, omfy, xproj, yproj, yslice, d11, d12, d21, d22
  real*8 xx8, yy8, zz8
  integer*4 numx, kx, ixyOKst, ixyOKnd, iyfix, ifixst, ifixnd
  real*4 delz, xnum, eps

  ytol = 3.05
  eps = 0.01
  reprojLines(1:iwidth) = 0.
  if (abs(sbeta * ithickReproj) <= abs(cbeta * iwidth)) then
    !
    delz = delzIn
    delx = 1. / cbeta
    znum = 1. + (ithickReproj - 1) / delz
    numz = znum
    if (znum - numz >= 0.1) numz = numz + 1
    !
    ! Loop up in Z through slices, adding in lines of data to the
    ! output line
    do kz = 1, numz
      zz = 1 + (kz - 1) * delz
      iz = zz
      fz = zz - iz
      omfz = 1. - fz
      pfill = dmeanIn
      !
      ! If Z is past the top, drop back one line and set up fractions
      ! to take just a fraction of the top line
      if (zz >= ithickReproj) then
        zz = ithickReproj
        iz = ithickReproj - 1
        fz = omfz
        omfz = 0.
        pfill = dmeanIn * fz
      endif
      zz = zz + minYreproj - 1 - ycenOut
      !
      ! Get y slice for this z value
      yproj = line + yprojOffset
      yy = (yproj + zz * (salf - yzfacv) - centerSlice) / calf
      yslice = yy + centerSlice - yprojOffset
      if (ifAlpha == 0) yslice = line
      ! if (line==591) print *,kz, zz, iz, fz, omfz, yproj, yy, yslice
      if (yslice < inloadstr - ytol .or. &
          yslice > inloadend + ytol) then
        !
        ! Really out of bounds, do fill
        ! if (line==591) print *,'Out of bounds, view, line, zz', line, zz
        reprojLines(1:iwidth) = reprojLines(1:iwidth) + pfill
      else
        !
        ! otherwise set up iy and interpolation factors
        iys = floor(yslice)
        if (ifAlpha .ne. 0) then
          if (iys < inloadstr) then
            iys = inloadstr
            fy = 0.
          else if (iys >= inloadend) then
            iys = inloadend - 1
            fy = 1.
          else
            fy = yslice - iys
          endif
          omfy = 1. - fy
        endif
        !
        ! Now get starting X coordinate, fill to left
        xproj = 1 + xprojOffset
        xx = (xproj - (yy * salf * sbeta + zz * (calf * sbeta + &
            xzfacv) + xcenPdelxx)) / cbeta + xcenOut - (minXreproj - 1)
        ixst = 1
        if (xx < 1) then
          ixst = ceiling((1. - xx) / delx + 1.)
        elseif (xx >= iwidth) then
          ixst = ceiling((iwidth - xx) / delx + 1.)
        endif
        xx = xx + (ixst - 1) * delx
        if (xx < 1 .or. xx >= iwidth) then
          ixst = ixst + 1
          xx = xx + delx
        endif
        if (ixst > 1) reprojLines(1:ixst - 1) = reprojLines(1:ixst - 1) + pfill
        !
        ! get ending X coordinate, fill to right
        ixnd = iwidth
        if (xx + (ixnd - ixst) * delx >= iwidth - eps) then
          ixnd = (iwidth - xx) / delx + ixst
          if (xx + (ixnd - ixst) * delx >= iwidth - eps) ixnd = ixnd - 1
        elseif (xx + (ixnd - ixst) * delx < 1 + eps) then
          ixnd = (1. - xx) / delx + ixst
          if (xx + (ixnd - ixst) * delx < 1 + eps) ixnd = ixnd - 1
        endif
        if (ixnd < iwidth) &
            reprojLines(ixnd + 1:iwidth) = reprojLines(ixnd + 1:iwidth) + pfill

        ! if (line == lsStart) write(*,'(3i6,3f11.3)') iv, ixst, ixnd, xx, &
        ! (ixst+xprojOffset - &
        ! (yy * salf * sbeta + zz * (calf * sbeta + &
        ! xzfacv) + xcenPdelxx)) / cbeta + xcenOut - (minXreproj-1) &
        ! , (ixnd+xprojOffset - &
        ! (yy * salf * sbeta + zz * (calf * sbeta + &
        ! xzfacv) + xcenPdelxx)) / cbeta + xcenOut - (minXreproj-1)
        !
        ! Add the line in: do simple 2x2 interpolation if no alpha
        indbase = 1 + inPlaneSize * (iys - inloadstr) + (iz - 1) * nxload
        ! if (line==591) print *,ixst, ixnd
        xx8 = xx
        if (ifAlpha == 0) then
          do i = ixst, ixnd
            ix = xx8
            fx = xx8 - ix
            omfx = 1. - fx
            ind = indbase + ix - 1
            reprojLines(i) = reprojLines(i) + &
                omfz * omfx * array(ind) + &
                omfz * fx * array(ind + 1) + &
                fz * omfx * array(ind + nxload) + &
                fz * fx * array(ind + nxload + 1)
            ! if (line==591.and.i==164) print *,reprojLines(i), array(ind), &
            ! array(ind + 1), array(ind + nxload), array(ind + nxload + 1)
            xx8 = xx8 + delx
          enddo
        else
          !
          ! Or do the full 3D interpolation if any variation in Y
          do i = ixst, ixnd
            ix = xx8
            fx = xx8 - ix
            omfx = 1. - fx
            d11 = omfx * omfy
            d12 = omfx * fy
            d21 = fx * omfy
            d22 = fx * fy
            ind = indbase + ix - 1
            reprojLines(i) = reprojLines(i) + &
                omfz * (d11 * array(ind) &
                + d12 * array(ind + inPlaneSize) + d21 * array(ind + 1) &
                + d22 * array(ind + inPlaneSize + 1)) &
                + fz * (d11 * array(ind + nxload) &
                + d12 * array(ind + inPlaneSize + nxload) &
                + d21 * array(ind + 1 + nxload) &
                + d22 * array(ind + inPlaneSize + 1 + nxload))
            xx8 = xx8 + delx
          enddo
        endif
      endif
    enddo

  else
    !
    ! angles higher than the corner angle need to be done in vertical lines,
    ! outer loop on X instead of z
    ! Spacing between vertical lines is now sine beta
    ! The step between pixels along a line is 1/sin beta with no alpha tilt,
    ! The alpha tilt compresses it by the delta Z factor divided by cosine
    ! beta, the amount that delta Z factor is compressed from cosine beta.
    delx = abs(sbeta)
    xnum = 1. + (iwidth - 1) / delx
    numx = xnum
    if (xnum - numx >= 0.1) numx = numx + 1
    delz = delzIn / (sbeta * abs(cbeta))
    dely = delz * (salf - yzfacv) / calf
    ! print *,'delx, dely, delz', delx, dely, delz
    !
    ! Loop in X across slices, adding in vertical lines of data to the output line
    do kx = 1, numx
      xx = 1 + (kx - 1) * delx
      ix = xx
      fx = xx - ix
      omfx = 1. - fx
      pfill = dmeanIn
      !
      ! If X is past the end, drop back one line and set up fractions
      ! to take just a fraction of the right column
      if (xx >= iwidth) then
        xx = iwidth
        ix = iwidth - 1
        fx = omfx
        omfx = 0.
        pfill = dmeanIn * fx
      endif

      ! get starting Z coordinate
      salfsbetdcal = salf * sbeta / calf
      xcenAdj = xcenOut - (minXreproj - 1)
      xproj = 1 + xprojOffset
      yproj = line + yprojOffset

      zz = (xproj - (yproj - centerSlice) * salfsbetdcal - xcenPdelxx - (xx - xcenAdj) * &
          cbeta) / ((salf - yzfacv) * salfsbetdcal + calf * sbeta + xzfacv)
      !
      ! Get y slice for this z value, then convert Z to be index coordinates in slice
      yy = (yproj + zz * (salf - yzfacv) - centerSlice) / calf
      yslice = yy + centerSlice - yprojOffset
      if (ifAlpha == 0) yslice = line
      zz = zz - (minYreproj - 1 - ycenOut)
      !
      ! Get starting X proj limit based on Z
      ixst = 1
      if (zz < 1.) then
        ixst = ceiling((1. - zz) / delz + 1.)
      elseif (zz >= ithickReproj) then
        ixst = ceiling((ithickReproj - zz) / delz + 1.)
      endif
      !
      ! Revise starting limit for Y
      if (ifAlpha .ne. 0) then
        ysl = yslice + (ixst - 1.) * dely
        if (ysl < inloadstr - ytol) then
          ixst = ceiling((inloadstr - ytol - yslice) / dely + 1.)
        elseif (ysl > inloadend + ytol) then
          ixst = ceiling((inloadend + ytol - yslice) / dely + 1.)
        endif
      endif
      !
      ! Adjust Z start for final start and make sure it works, adjust Y also
      zz = zz + (ixst - 1.) * delz
      if (zz < 1. .or. zz >= ithickReproj) then
        zz = zz + delz
        ixst = ixst + 1
      endif
      yslice = yslice + (ixst - 1.) * dely
      if (ifAlpha == 0) yslice = line
      !
      ! get ending coordinate based on limits in Z and Y
      ixnd = iwidth
      if (zz + (ixnd - ixst) * delz >= ithickReproj - eps) then
        ixnd = (ithickReproj - zz) / delz + ixst
        if (zz + (ixnd - ixst) * delz >= ithickReproj - eps) ixnd = ixnd - 1
      elseif ( zz + (ixnd - ixst) * delz < 1. + eps) then
        ixnd = (1. - zz) / delz + ixst
        if (zz + (ixnd - ixst) * delz < 1. + eps) ixnd = ixnd - 1
      endif
      if (ifAlpha .ne. 0) then
        ysl = yslice + (ixnd - ixst) * dely
        if (ysl < inloadstr - ytol) then
          ixnd = (inloadstr - ytol - yslice) / dely + ixst
        elseif (ysl > inloadend + ytol) then
          ixnd = (inloadend + ytol - yslice) / dely + ixst
        endif
      endif
      !
      ! Now get X indexes within which Y can safely be varied
      ixyOKst = ixst
      ixyOKnd = ixnd
      if (ifAlpha .ne. 0) then
        if (yslice < inloadstr) then
          ixyOKst = ceiling((inloadstr - yslice) / dely + ixst)
          if (yslice + (ixyOKst - ixst) * dely < inloadstr + eps) &
              ixyOKst = ixyOKst + 1
        elseif (yslice >= inloadend) then
          ixyOKst = ceiling((inloadend - yslice) / dely + ixst)
          if (yslice + (ixyOKst - ixst) * dely >= inloadend - eps) &
              ixyOKst = ixyOKst + 1
        endif
        yslice = yslice + (ixyOKst - ixst) * dely
        !
        ysl = yslice + (ixnd - ixyOKst) * dely
        if (ysl < inloadstr) then
          ixyOKnd = (inloadstr - yslice) / dely + ixyOKst
          if (yslice + (ixyOKnd - ixyOKst) * dely < inloadstr + eps) &
              ixyOKnd = ixyOKnd-1
        elseif (ysl >= inloadend) then
          ixyOKnd = (inloadend - yslice) / dely + ixyOKst
          if (yslice + (ixyOKnd - ixyOKst) * dely >= inloadend - eps) &
              ixyOKnd = ixyOKnd-1
        endif
      endif
      ! write( *,'(i5,f7.1,4i5,2f7.1)') kx, xx, ixst, ixyOKst, ixyOKnd, ixnd, zz, &
      ! zz+(ixnd-ixst)*delz
      !
      ! Do the fills
      if (ixst > 1) reprojLines(1:ixst - 1) = reprojLines(1:ixst - 1) + pfill
      if (ixnd < iwidth) &
          reprojLines(ixnd + 1:iwidth) = reprojLines(ixnd + 1:iwidth) + pfill
      !
      ! Add the line in: do simple 2x2 interpolation if no alpha
      ! if (line==591) print *,ixst, ixnd
      if (ifAlpha == 0) then
        zz8 = zz
        indbase = 1 + inPlaneSize * (line - inloadstr) + ix - 1
        do i = ixst, ixnd
          iz = zz8
          fz = zz8 - iz
          omfz = 1. - fz
          ind = indbase + (iz - 1) * nxload
          reprojLines(i) = reprojLines(i) + &
              omfz * omfx * array(ind) + &
              omfz * fx * array(ind + 1) + &
              fz * omfx * array(ind + nxload) + &
              fz * fx * array(ind + nxload + 1)
          ! if (i==70) print *,reprojLines(i)
          ! if (line==591.and.i==164) print *,reprojLines(i), array(ind), &
          ! array(ind + 1), array(ind + nxload), array(ind + nxload + 1)
          zz8 = zz8 + delz
        enddo
      else
        !
        ! Or do the full 3D interpolation if any variation in Y, starting with
        ! the loop where Y varies
        yy8 = yslice
        zz8 = zz + (ixyOKst - ixst) * delz
        indbase = 1 - inPlaneSize * inloadstr + ix - 1
        do i = ixyOKst, ixyOKnd
          iz = zz8
          fz = zz8 - iz
          omfz = 1. - fz
          iys = yy8
          fy = yy8 - iys
          omfy = 1. - fy
          d11 = omfx * omfy
          d12 = omfx * fy
          d21 = fx * omfy
          d22 = fx * fy
          ind = indbase + inPlaneSize * iys + (iz - 1) * nxload
          reprojLines(i) = reprojLines(i) + &
              omfz * (d11 * array(ind) &
              + d12 * array(ind + inPlaneSize) + d21 * array(ind + 1) &
              + d22 * array(ind + inPlaneSize + 1)) &
              + fz * (d11 * array(ind + nxload) &
              + d12 * array(ind + inPlaneSize + nxload) &
              + d21 * array(ind + 1 + nxload) &
              + d22 * array(ind + inPlaneSize + 1 + nxload))
          zz8 = zz8 + delz
          yy8 = yy8 + dely
        enddo
        !
        ! Now do special loops with Y fixed - do the one at the end first
        ! since Y and Z are all set for that
        ifixst = ixyOKnd + 1
        ifixnd = ixnd
        do iyfix = 1, 2
          do i = ifixst, ifixnd
            iz = zz8
            fz = zz8 - iz
            omfz = 1. - fz
            d11 = omfx * omfy
            d12 = omfx * fy
            d21 = fx * omfy
            d22 = fx * fy
            ind = indbase + inPlaneSize * iys + (iz - 1) * nxload
            reprojLines(i) = reprojLines(i) + &
                omfz * (d11 * array(ind) &
                + d12 * array(ind + inPlaneSize) + d21 * array(ind + 1) &
                + d22 * array(ind + inPlaneSize + 1)) &
                + fz * (d11 * array(ind + nxload) &
                + d12 * array(ind + inPlaneSize + nxload) &
                + d21 * array(ind + 1 + nxload) &
                + d22 * array(ind + inPlaneSize + 1 + nxload))
            zz8 = zz8 + delz
          enddo
          !
          ! Set up for loop with Y fixed at start, reset y and z
          yy8 = yslice
          zz8 = zz
          iys = yy8
          fy = yy8 - iys
          omfy = 1. - fy
          ifixst = ixst
          ifixnd = ixyOKst - 1
        enddo
      endif
    enddo
  endif
  return
end subroutine reprojOneAngle

! Computes the change in Z that moves by 1 pixel along a projection
! ray given the sines and cosines of alpha and beta and the z factors
!
real*4 function reprojDelz(sbeta, cbeta, salf, calf, xzfac, yzfac)
  implicit none
  real*4 sbeta, cbeta, salf, calf, xzfac, yzfac,  dyfac
  dyfac = (salf - yzfac) / calf
  reprojDelz = 1. / sqrt(1. + dyfac**2 + &
      ((dyfac * salf * sbeta + calf * sbeta + xzfac) / cbeta)**2)
  return
end function reprojDelz


! Writes line LINE for view IV of a reprojection
!
subroutine writeReprojLines(iv, lineStart, lineEnd, DMIN, DMAX, DTOT8)
  use tiltvars
  implicit none
  integer*4 line, i, iyout, iv, lineStart, lineEnd, numVals
  real*4 dmin, dmax, val
  real*8 dtot8
  !
  ! Write the line after scaling.  outScale log data to give approximately
  ! constant mean levels.  Descale non-log data by exposure weights
  numVals = iwidth * (lineEnd + 1 - lineStart)
  if (ifLog .ne. 0) then
    ! Hopefully this works for local as well
    if (abs(sinBeta(iv) * ithickReproj) <= abs(cosBeta(iv) * iwidth)) then
      val = alog10(projMean + baseForLog) - ithickReproj * dmeanIn / abs(cosBeta(iv))
    else
      val = alog10(projMean + baseForLog) - iwidth * dmeanIn / abs(sinBeta(iv))
    endif
    if (debug) print *,iv, lineStart, lineEnd, val
    do i = 1, numVals
      reprojLines(i) = 10**(reprojLines(i) + val) - baseForLog
    enddo
  else
    do i = 1, numVals
      reprojLines(i) = reprojLines(i) / exposeWeight(iv)
    enddo
  endif
  if (projSubtraction) then
    call imposn(1, iv - 1, lineStart - 1)
    call irdsecl(1, origLines, lineEnd + 1 - lineStart, *99)
    reprojLines(1:numVals) = reprojLines(1:numVals) - origLines(1:numVals)
  endif
  do i = 1, numVals
    val = reprojLines(i)
    ! if (debug .and. val < dmin) print *,'min:', i, val
    ! if (debug .and. val > dmax) print *,'max:', i, val
    dmin = min(dmin, val)
    dmax = max(dmax, val)
    dtot8 = dtot8 + val
  enddo
  do line = lineStart, lineEnd
    iyout = line - isliceStart
    if (minTotSlice > 0) iyout = line - minTotSlice
    call parWrtPosn(2, iv - 1, iyout)
    call parWrtLin(2, reprojLines(1 + (line - lineStart) * iwidth))
  enddo
  return
99 call exitError('READING FROM ORIGINAL PROJECTION FILE')
end subroutine writeReprojLines


! Projects model points onto the included views
!
subroutine projectModel(filout, delta, nvorig)
  use tiltvars
  implicit none
  include 'model.inc90'
  character*(*) filout
  real*4 delta(3), orig(3)
  integer*4 nvorig, ibase, numPt, iobj, ipt, ip1, iv, nv
  real*4 value, rj, ri, rlslice, zz, yy, zpart, xproj, yproj
  integer*4 j, lslice, imodobj, imodcont, ierr, size
  real*4 fj, fls, f11, f12, f21, f22, xf11, xz11, yf11, yz11
  real*4 xf21, xz21, yf21, yz21, xf12, xz12, yf12, yz12, xf22, xz22, yf22
  real*4 yz22, xprojf, xprojz, yprojf, yprojz
  real*4, allocatable :: values(:), coords(:,:)
  integer*4, allocatable :: mapnv(:)
  integer*4 getContValue, putImageRef, putContValue, putImodFlag
  integer*4 getScatSize, putScatSize, putImodMaxes
  !
  call irtorg(1, orig(1), orig(2), orig(3))
  call scale_model(0)
  if (getScatSize(1, size) .ne. 0) size = 5
  allocate(values(n_point), coords(3, n_point), mapnv(limView), stat = j)
  if (j .ne. 0) call exitError('ALLOCATING ARRAYS FOR REPROJECTING MODEL')
  !
  ! get each point and its contour value into the arrays
  numPt = 0
  do iobj = 1, max_mod_obj
    call objtocont(iobj, obj_color, imodobj, imodcont)
    if (getContValue(imodobj, imodcont, value) .ne. 0) value = -1.
    ibase = ibase_obj(iobj)
    do ipt = 1, npt_in_obj(iobj)
      numPt = numPt + 1
      values(numPt) = value
      ip1 = abs(object(ipt + ibase))
      coords(1, numPt) = p_coord(1, ip1)
      coords(2, numPt) = p_coord(2, ip1)
      coords(3, numPt) = p_coord(3, ip1)
    enddo
  enddo
  !
  ! Start a new model
  call newimod()
  n_point = 0
  iobj = 0
  if (putImageRef(delta, orig) .ne. 0 .or. putImodMaxes(nprojXyz(1), nprojXyz(2),  &
      nprojXyz(3)) .ne. 0) call exitError( &
      'Putting image reference or maximum size information in output model')
  !
  ! Build a map from views in file to ordered views in program
  do nv = 1, nvorig
    mapnv(nv) = 0
  enddo
  do nv = 1, numViews
    mapnv(mapUsedView(nv)) = nv
  enddo
  !
  ! Loop on the points, start new contour for each
  do ipt = 1, numPt
    iobj = iobj + 1
    obj_color(1, iobj) = 1
    obj_color(2, iobj) = 255
    ierr = putContValue(1, iobj, values(ipt))
    ibase_obj(iobj) = n_point
    npt_in_obj(iobj) = 0
    !
    ! Get real pixel coordinates in tomogram file
    rj = coords(1, ipt) + 0.5
    ri = coords(2, ipt) + 0.5
    rlslice = coords(3, ipt) + 0.5
    !
    ! This may never be tesed but seems simple enough
    if (.not.perpendicular) then
      ri = coords(3, ipt) + 0.5
      rlslice = coords(2, ipt) + 0.5
    endif
    !
    ! Loop on the views in the file
    do nv = 1, nvorig
      iv = mapnv(nv)
      if (iv > 0) then
        zz = (ri - ycenModProj) * compress(iv)
        yy = rlslice - centerSlice
        if (nxWarp == 0) then
          zpart = yy * sinAlpha(iv) * sinBeta(iv) + zz * (cosAlpha(iv) * sinBeta(iv) +  &
              xzfac(iv)) + xcenIn + axisXoffset
          yproj = yy * cosAlpha(iv) - zz * (sinAlpha(iv) - yzfac(iv)) + centerSlice
          xproj = zpart + (rj - xcenOut) * cosBeta(iv)
        else
          !
          ! local alignments
          j = rj
          fj = rj - j
          lslice = rlslice
          fls = rlslice - lslice
          f11 = (1. -fj) * (1. -fls)
          f12 = (1. -fj) * fls
          f21 = fj * (1. -fls)
          f22 = fj * fls
          call localProjFactors(j, lslice, iv, xf11, xz11, yf11, yz11)
          call localProjFactors(j + 1, lslice, iv, xf21, xz21, yf21, yz21)
          call localProjFactors(j, lslice + 1, iv, xf12, xz12, yf12, yz12)
          call localProjFactors(j + 1, lslice + 1, iv, xf22, xz22, yf22, yz22)
          xprojf = f11 * xf11 + f12 * xf12 + f21 * xf21 + f22 * xf22
          xprojz = f11 * xz11 + f12 * xz12 + f21 * xz21 + f22 * xz22
          yprojf = f11 * yf11 + f12 * yf12 + f21 * yf21 + f22 * yf22
          yprojz = f11 * yz11 + f12 * yz12 + f21 * yz21 + f22 * yz22
          xproj = xprojf + zz * xprojz
          yproj = yprojf + zz * yprojz
        endif
        !
        ! Store model coordinates
        n_point = n_point + 1
        if (n_point > max_pt) call exitError( &
            'Too many projection points for small model arrays')
        npt_in_obj(iobj) = npt_in_obj(iobj) + 1
        object(n_point) = n_point
        p_coord(1, n_point) = xproj - 0.5
        p_coord(2, n_point) = yproj - 0.5
        p_coord(3, n_point) = nv - 1.
      endif
    enddo
  enddo
  !
  ! Save model
  max_mod_obj = iobj
  !
  ! Set to open contour, show values etc., and show sphere on section only
  ierr = putImodFlag(1, 1)
  ierr = putImodFlag(1, 7)
  ierr = putImodFlag(1, 9)
  ierr = putScatSize(1, size)
  call scale_model(1)
  call write_wmod(filout)
  print *,n_point, ' points written to output model'
  call exit(0)
end subroutine projectModel

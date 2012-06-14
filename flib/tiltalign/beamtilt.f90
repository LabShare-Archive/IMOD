! BEAMTILT.F   -  has searchBeamTilt and runMetro routines
!
! $Id$
!
! searchBeamTilt will perform a one - dimensional search for the beam tilt
! that minimizes the alignment error.
! beamTilt starts with an initial value and is returned with the final
! value.  binStepIni and binStepFinal are initial and final step sizes
! in the binary search.  scanStep is the step size in the full scan
! through the last interval of the binary search.  Other parameters are
! as passed to runMetro
!
subroutine searchBeamTilt(beamTilt, binStepIni, binStepFinal, scanStep, numVarSearch, &
    var, varerr, grad, h, ifLocal, facm, ncycle, rmsScale, fFinal, kountInit, metroError)
  implicit none
  integer LIMSCAN
  parameter (LIMSCAN = 200)
  real*4 beamTilt, binStepIni, binStepFinal, scanStep, rmsScale
  real*4 var(*), varerr(*), grad(*), h(*), facm, fFinal
  integer*4 numVarSearch, ncycle, metroError, ifLocal, kountInit
  real*4 btOrig, btMax, fScan(LIMSCAN), xx(LIMSCAN/2), xxsq(LIMSCAN/2)
  real*4 btMin, aa, bb, cc, xmin, scanInt
  integer*4 idir, iter, numScan, nfit, i, iMin, istr, iend, j, kount
  real*4 fnew, fmin, fAbove, btAbove, fBelow, btBelow, binStep
  real*4 dtor/0.0174532/
  logical*4 scanning


  btMax = 5.
  btOrig = beamTilt
  idir = 1
  call runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, ncycle, 1, fmin, &
      kount, metroError)
  print *
  write(*,101) beamTilt / dtor, kount, sqrt(fmin * rmsScale)
101 format(' For beam tilt =',f6.2, ', ',i4,' cycles,',T48,'final   F : ', &
        T61,F14.6)
  kountInit = kount
  !
  ! Restart every run at the output of this one
  do i = 1, numVarSearch
    varerr(i) = var(i)
  enddo
  btMin = beamTilt
  scanning = .true.
  iter = 1
  do while (scanning)
    !
    ! Take a step from current minimum
    beamTilt = btMin + idir * binStepIni * dtor

    if (abs(beamTilt - btOrig) > btMax) then
      write(*,102) btMax / dtor, btOrig / dtor
102   format(/,'WARNING: NO MINIMUM ERROR FOUND FOR BEAM TILT CHANGE UP TO' &
          ,f5.1, /, 'WARNING: RETURNING TO ORIGINAL BEAM TILT =',f6.2, /)
      beamTilt = btOrig

      do i = 1, numVarSearch
        var(i) = varerr(i)
      enddo
      call runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, -ncycle, 1, &
          rmsScale, fFinal, kount, metroError)
      return
    endif

    do i = 1, numVarSearch
      var(i) = varerr(i)
    enddo
    call runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, -ncycle, 1,  &
        rmsScale,  fnew, kount, metroError)
    write(*,101) beamTilt / dtor, kount, sqrt(fnew * rmsScale)
    iter = iter + 1
    if (fnew > fmin) then
      !
      ! If higher than min, then record above or below point depending
      ! on direction
      if (idir > 0) then
        btAbove = beamTilt
        fAbove = fnew
      else
        btBelow = beamTilt
        fBelow = fnew
      endif
      !
      ! If this is first iteration, reverse direction; otherwise terminate
      if (iter == 2) then
        idir = -1
      else
        scanning = .false.
      endif
    else
      !
      ! If lower than min, record a new min and make old min be new above
      ! or below point
      if (idir < 0) then
        btAbove = btMin
        fAbove = fmin
      else
        btBelow = btMin
        fBelow = fmin
      endif
      btMin = beamTilt
      fmin = fnew
    endif
  enddo
  !
  ! Now search until the interval becomes small enough.  Start in most
  ! promising direction
  idir = 1
  if (fBelow < fAbove) idir = -1
  do while (btAbove - btBelow > 2.05 * binStepFinal * dtor)
    !
    ! Go in the given direction halfway between min and endpoint
    if (idir > 0) then
      beamTilt = (btMin + btAbove) / 2.
    else
      beamTilt = (btMin + btBelow) / 2.
    endif
    binStep = binStep / 2.
    !
    do i = 1, numVarSearch
      var(i) = varerr(i)
    enddo
    call runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, -ncycle, 1, &
        rmsScale, fnew, kount, metroError)
    write(*,101) beamTilt / dtor, kount, sqrt(fnew * rmsScale)
    if (fnew < fmin) then
      !
      ! If new minimum, replace the old, go in most promising direction
      ! again
      if (idir < 0) then
        btAbove = btMin
        fAbove = fmin
      else
        btBelow = btMin
        fBelow = fmin
      endif
      btMin = beamTilt
      fmin = fnew
      idir = 1
      if (fBelow < fAbove) idir = -1
    else
      !
      ! Or replace the endpoint, go in opposite direction
      if (idir > 0) then
        btAbove = beamTilt
        fAbove = fnew
      else
        btBelow = beamTilt
        fBelow = fnew
      endif
      idir = -idir
    endif
  enddo
  !
  ! Now scan from below to above, find a minimum excluding the endpoints
  numScan = nint((btAbove - btBelow) / (scanStep * dtor) + 1.)
  numScan = max(3, min(LIMSCAN, numScan))
  scanInt = (btAbove - btBelow) / (numScan - 1)
  fScan(1) = fBelow
  fScan(numScan) = fAbove
  do j = 2, numScan - 1
    beamTilt = btBelow + (j - 1) * scanInt
    do i = 1, numVarSearch
      var(i) = varerr(i)
    enddo
    call runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, -ncycle, 1, &
        rmsScale, fScan(j), kount, metroError)
    write(*,101) beamTilt / dtor, kount, sqrt(fScan(j) * rmsScale)
    if (fScan(j) < fmin .or. j == 2) then
      fmin = fScan(j)
      btMin = beamTilt
      iMin = j
    endif
  enddo
  !
  ! See if the change is monotonic from the minimum; if so fit to 3 points,
  ! or 5 if range is not very big, if not fit to up to half the scan
  do i = 1, numScan
    fScan(i) = fScan(i) - fmin
  enddo
  nfit = 3
  if (max(fBelow, fAbove) / fmin < 1.05) nfit = 5
  do i = iMin + 1, numScan
    if (fScan(i) < fScan(i - 1)) nfit = numScan / 2
  enddo
  do i = 1, iMin - 1
    if (fScan(i) < fScan(i + 1)) nfit = numScan / 2
  enddo
  nfit = max(3, min(LIMSCAN / 2, nfit))
  istr = max(1, iMin - nfit / 2)
  iend = min(numScan, iMin + nfit / 2)
  do i = istr, iend
    xx(i + 1 - istr) = i - iMin
    xxsq(i + 1 - istr) = (i - iMin)**2
  enddo
  !
  ! Do fit and compute minimum from derivative, then return to actual
  ! minimum if the parabola is upside down or min is outside fit
  call lsfit2(xx, xxsq, fScan(istr), iend + 1 - istr, aa, bb, cc)
  xmin = iMin - 0.5 * aa / bb
  ! print *,'imin is', iMin, ' fitting ', istr, ' to', iend
  ! print *,'coeffs: ', aa, bb, cc, '  xmin', xmin
  if (bb < 0 .or. xmin < istr .or. xmin > iend) then
    beamTilt = btMin
    if (fBelow < fmin) then
      beamTilt = btBelow
      fmin = fBelow
    endif
    if (fAbove < fmin) beamTilt = btAbove
    write(*,103)
103 format(/,'WARNING: FIT TO BEAM TILT SCAN FAILED TO GIVE MINIMUM;', &
        ' USING SCAN MINIMUM',/)
  else
    beamTilt = btBelow + (xmin - 1) * scanInt
  endif
  !
  ! Run finally at the solved beam tilt
  do i = 1, numVarSearch
    var(i) = varerr(i)
  enddo
  call runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, -ncycle, 1, rmsScale, &
      fFinal, kount, metroError)
  print *
  write(*,104) beamTilt / dtor, kount, sqrt(fFinal * rmsScale)
104 format(' Solved beam tilt =',f6.2, ', ',i4,' cycles,',T48,'Final   F : ', &
      T61,F14.6)
  return
end subroutine searchBeamTilt

! runMetro runs the metro routine, varying the step size as needed and
! reporting errors as appropriate.
! numVarSearch is the number of variables
! VAR is the variable vector
! VARERR is used for temporary storage of the VAR array between trials
! GRAD is an array for gradients
! H is the array for the Hessian matrix and a few vectors
! ifLocal is nonzero if doing local alignments
! FACM is the metro factor or initial step size
! NCYCLE is the limit on the number of cycles, or the negative of the
! limit to suppress some output
! ifHush not equal to zero suppresses the Final F output
! rmsScale is used to scale the sum squared error before taken sqrt
! fFinal is the final error measure
! KOUNT is the cycle count
! metroError is maintained with a count of total errors
!
subroutine runMetro(numVarSearch, var, varerr, grad, h, ifLocal, facm, ncycle, &
    ifHush, rmsScale, fFinal, kount, metroError)
  use alivar
  implicit none
  integer maxMetroTrials
  parameter (maxMetroTrials = 5)
  real*4 var(*), varerr(*), grad(*), h(*), facm, fFinal, rmsScale
  real*4 trialScale(maxMetroTrials) /1.0, 0.9, 1.1, 0.75, 0.5/
  integer*4 numVarSearch, ncycle, metroError, ifLocal
  integer*4 i, ier, metroLoop, kount, ifHush
  real*4 fInit, f, eps
  external funct
  !
  ! save the variable list for multiple trials
  !
  do i = 1, numVarSearch
    varerr(i) = var(i)
  enddo
  metroLoop = 1
  ier = 1
  eps = 0.00001
  ! if (ncycle < 0) eps = eps / 5.
  do while (metroLoop <= maxMetroTrials .and. ier .ne. 0 .and. ier .ne. 3)
    firstFunct = .true.
    call funct(numVarSearch, var, fInit, grad)
    if (metroLoop == 1 .and. ncycle > 0) WRITE(6, 70) sqrt(fInit * rmsScale)
70  FORMAT(/,' Variable Metric minimization',T48, 'Initial F:',T61,F14.6)
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ! Call variable metric minimizer
    !
    CALL metro (numVarSearch, var, funct, F, Grad, facm * trialScale(metroLoop), &
        eps, NCYCLE, IER, H, KOUNT, rmsScale)
    metroLoop = metroLoop + 1

    !
    ! For errors except limit reached, give warning message and restart
    !
    if (ier .ne. 0 .and. ier .ne. 3) then
      print *
      if (ier == 1) print *,'Minimization error #1 - DG > 0'
      if (ier == 2) print *,'Minimization error #2 - Linear search lost'
      if (ier == 4) print *,'Minimization error #4 - ', &
          'Matrix non-positive definite'

      if (metroLoop <= maxMetroTrials) then
        print *,'Restarting with metro step factor of ', &
            facm * trialScale(metroLoop)
        do i = 1, numVarSearch
          var(i) = varerr(i)
        enddo
      endif
    endif
  enddo
  if (ier == 0 .and. metroLoop > 2) &
      print *,'Search succeeded with this step factor'

  ! Final call to FUNCT
  CALL FUNCT(numVarSearch, var, fFinal, Grad)
  if (ifHush == 0) WRITE(6, 98) sqrt(fFinal * rmsScale), KOUNT
98 FORMAT(/,T48,'Final   F : ',T61,f14.6,/,' Number of cycles : ',I5)
  call flush(6)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ! Error returns:
  if (IER .NE. 0) then
    if (ier .ne. 3) then
      call errorExit('Search failed even after varying step factor', &
          ifLocal)
    else
      call errorExit('Minimization error #3 - Iteration limit exceeded', &
          1)
    endif
    metroError = metroError + 1
  END IF
  return
end subroutine runMetro

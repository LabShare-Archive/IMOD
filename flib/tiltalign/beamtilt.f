c       BEAMTILT.F   -  has searchBeamTilt and runMetro routines
c
c       $Id$
c       
c       $Log$
c       Revision 3.1  2007/02/19 21:06:00  mast
c       Added to program
c
c       
c       searchBeamTilt will perform a one-dimensional search for the beam tilt
c       that minimizes the alignment error.
c       beamTilt starts with an initial value and is returned with the final
c       value.  binStepIni and binStepFinal are initial and final step sizes
c       in the binary search.  scanStep is the step size in the full scan
c       through the last interval of the binary search.  Other parameters are
c       as passed to runMetro
c
      subroutine searchBeamTilt(beamTilt, binStepIni, binStepFinal, scanStep, 
     &    nvarsrch,var,varerr,grad,h,ifLocal,facm,ncycle, fFinal, metroError)
      implicit none
      integer limscan
      parameter (limscan=200)
      real*4 beamTilt, binStepIni, binStepFinal, scanStep
      real*4 var(*), varerr(*), grad(*), h(*), facm, fFinal
      integer*4 nvarsrch, ncycle,metroError, ifLocal
      real*4 btOrig,btMax, fScan(limscan), xx(limscan/2), xxsq(limscan/2)
      real*4 btMin,aa,bb,cc,xmin,scanInt
      integer*4 idir, iter, numScan, nfit, i, iMin, istr, iend, j, kount
      real*4 fnew, fmin, fAbove, btAbove, fBelow, btBelow, binStep
      real*4 dtor/0.0174532/
      logical*4 scanning


      btMax = 5.
      btOrig = beamTilt
      idir = 1
      call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,ncycle, 1, fmin,
     &    kount, metroError)
      print *
      write(*,101)beamTilt/dtor, kount, fmin
101   format(' For beam tilt =',f6.2, ', ',i4,' cycles,',T48,'final   F : ',
     &    T65,E14.7)
c       
c       Restart every run at the output of this one
      do i = 1, nvarsrch
        varerr(i) = var(i)
      enddo
      btMin = beamTilt
      scanning = .true.
      iter = 1
      do while (scanning)
c         
c         Take a step from current minimum
        beamTilt = btMin + idir * binStepIni * dtor

        if (abs(beamTilt - btOrig) .gt. btMax) then
          write(*,102)btMax/dtor,btOrig/dtor
102       format(/,'WARNING: NO MINIMUM ERROR FOUND FOR BEAM TILT CHANGE UP TO'
     &        ,f5.1, /, 'WARNING: RETURNING TO ORIGINAL BEAM TILT =',f6.2)
          beamTilt = btOrig

          do i = 1, nvarsrch
            var(i) = varerr(i)
          enddo
          call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,-ncycle, 1, 
     &        fFinal, kount, metroError)
          return
        endif

        do i = 1, nvarsrch
          var(i) = varerr(i)
        enddo
        call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,-ncycle, 1, fnew,
     &      kount, metroError)
        write(*,101)beamTilt/dtor, kount, fnew
        iter = iter + 1
        if (fnew .gt. fmin) then
c           
c           If higher than min, then record above or below point depending
c           on direction
          if (idir .gt. 0) then
            btAbove = beamTilt
            fAbove = fnew
          else
            btBelow = beamTilt
            fBelow = fnew
          endif
c
c           If this is first iteration, reverse direction; otherwise terminate
          if (iter .eq. 2) then
            idir = -1
          else
            scanning = .false.
          endif
        else
c           
c           If lower than min, record a new min and make old min be new above
c           or below point
          if (idir .lt. 0) then
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
c             
c       Now search until the interval becomes small enough.  Start in most
c       promising direction
      idir = 1
      if (fBelow .lt. fAbove) idir = -1
      do while (btAbove - btBelow .gt. 2.05 * binStepFinal * dtor)
c           
c         Go in the given direction halfway between min and endpoint
        if (idir .gt. 0) then
          beamTilt = (btMin + btAbove) / 2.
        else
          beamTilt = (btMin + btBelow) / 2.
        endif
        binStep = binStep / 2.
c
        do i = 1, nvarsrch
          var(i) = varerr(i)
        enddo
        call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,-ncycle, 1, fnew,
     &      kount, metroError)
        write(*,101)beamTilt/dtor, kount, fnew
        if (fnew .lt. fmin) then
c             
c           If new minimum, replace the old, go in most promising direction
c           again
          if (idir .lt. 0) then
            btAbove = btMin
            fAbove = fmin
          else
            btBelow = btMin
            fBelow = fmin
          endif
          btMin = beamTilt
          fmin = fnew
          idir = 1
          if (fBelow .lt. fAbove) idir = -1
        else
c
c           Or replace the endpoint, go in opposite direction
          if (idir .gt. 0) then
            btAbove = beamTilt
            fAbove = fnew
          else
            btBelow = beamTilt
            fBelow = fnew
          endif
          idir = -idir
        endif
      enddo
c       
c       Now scan from below to above, find a minimum excluding the endpoints
      numScan = nint((btAbove - btBelow) / (scanStep * dtor) + 1.)
      numScan = max(3,min(limscan, numScan))
      scanInt = (btAbove - btBelow) / (numScan - 1) 
      fScan(1) = fBelow
      fScan(numScan) = fAbove
      do j = 2, numScan - 1
        beamTilt = btBelow + (j - 1) * scanInt
        do i = 1, nvarsrch
          var(i) = varerr(i)
        enddo
        call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,-ncycle, 1, 
     &      fScan(j), kount, metroError)
        write(*,101)beamTilt/dtor, kount, fScan(j)
        if (fScan(j) .lt. fmin .or. j .eq. 2) then
          fmin = fScan(j)
          btMin = beamTilt
          iMin = j
        endif
      enddo
c       
c       See if the change is monotonic from the minimum; if so fit to 3 points,
c       or 5 if range is not very big, if not fit to up to half the scan
      do i = 1, numScan
        fScan(i) = fScan(i) - fmin
      enddo
      nfit = 3
      if (max(fBelow, fAbove) / fmin .lt. 1.05) nfit = 5
      do i = iMin + 1, numScan
        if (fScan(i) .lt. fScan(i - 1)) nfit = numScan / 2
      enddo
      do i = 1, iMin - 1
        if (fScan(i) .lt. fScan(i + 1)) nfit = numScan / 2
      enddo
      nfit = max(3,min(limscan/2, nfit))
      istr = max(1, iMin - nfit / 2)
      iend = min(numScan, iMin + nfit / 2)
      do i = istr, iend
        xx(i + 1 - istr) = i - iMin
        xxsq(i + 1 - istr) = (i - iMin)**2
      enddo
c
c       Do fit and compute minimum from derivative, then return to actual
c       minimum if the parabola is upside down or min is outside fit
      call lsfit2(xx, xxsq, fScan(istr), iend + 1 - istr, aa, bb, cc)
      xmin = iMin - 0.5 * aa / bb
c      print *,'imin is',imin,' fitting ',istr,' to', iend
c      print *,'coeffs: ', aa, bb, cc,'  xmin', xmin
      if (bb .lt. 0 .or. xmin .lt. istr .or. xmin .gt. iend) then
        beamTilt = btMin
        if (fBelow .lt. fmin) then
          beamTilt = btBelow
          fmin = fBelow
        endif
        if (fAbove .lt. fmin) beamTilt = btAbove
        write(*,103)
103     format(/,'WARNING: FIT TO BEAM TILT SCAN FAILED TO GIVE MINIMUM;',
     &      ' USING SCAN MINIMUM')
      else
        beamTilt = btBelow + (xmin - 1) * scanInt
      endif
c       
c       Run finally at the solved beam tilt
      do i = 1, nvarsrch
        var(i) = varerr(i)
      enddo
      call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,-ncycle, 1,
     &    fFinal, kount, metroError)
      print *
      write(*,104)beamTilt/dtor, kount, fFinal
104   format(' Solved beam tilt =',f6.2, ', ',i4,' cycles,',T48,'Final   F : ',
     &    T65,E14.7)
      return
      end

c       runMetro runs the metro routine, varying the step size as needed and
c       reporting errors as appropriate.
c       NVARSRCH is the number of variables
c       VAR is the variable vector
c       VARERR is used for temporary storage of the VAR array between trials
c       GRAD is an array for gradients
c       H is the array for the Hessian matrix and a few vectors
c       ifLocal is nonzero if doing local alignments
c       FACM is the metro factor or initial step size
c       NCYCLE is the limit on the number of cycles, or the negative of the
c       limit to suppress some output
c       ifHush not equal to zero suppresses the Final F output
c       fFinal is the final error measure
c       KOUNT is the cycle count
c       metroError is maintained with a count of total errors
c
      subroutine runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,ncycle,
     &    ifHush, fFinal, kount, metroError)
      implicit none
      integer maxMetroTrials
      parameter (maxMetroTrials = 5)
      real*4 var(*), varerr(*), grad(*), h(*), facm, fFinal
      real*4 trialScale(maxMetroTrials) /1.0, 0.9, 1.1, 0.75, 0.5/
      integer*4 nvarsrch, ncycle,metroError, ifLocal
      integer*4 i, ier, metroLoop, kount, ifHush
      real*4 fInit, f
      logical firsttime
      common /functfirst/ firsttime
      external funct
c       
c       save the variable list for multiple trials
c       
      do i = 1, nvarsrch
        varerr(i) = var(i)
      enddo
      metroLoop = 1
      ier = 1
      do while (metroLoop.le.maxMetroTrials .and. ier.ne.0 .and. ier.ne.3)
        firsttime=.true.
        call funct(nvarsrch,var,finit,grad)
        if (metroLoop .eq. 1 .and. ncycle .gt. 0) WRITE(6,70)FINIT
70      FORMAT(/,' Variable Metric minimization',T48, 'Initial F:',T65,E14.7)
C         
C         -----------------------------------------------------
C         Call variable metric minimizer
C         
        CALL METRO (nvarsrch,var,funct,F,Grad,facm * trialScale(metroLoop),
     &      .00001,NCYCLE,IER, H,KOUNT)
        metroLoop = metroLoop +1

c         
c         For errors except limit reached, give warning message and restart
c         
        if (ier .ne. 0 .and. ier .ne. 3) then
          print *
          if(ier.eq.1)print *,'Minimization error #1 - DG > 0'
          if(ier.eq.2)print *,'Minimization error #2 - Linear search lost'
          if(ier.eq.4)print *,'Minimization error #4 - ',
     &        'Matrix non-positive definite'

          if (metroLoop .le. maxMetroTrials) then
            print *,'Restarting with metro step factor of ',
     &          facm * trialScale(metroLoop)
            do i = 1, nvarsrch
              var(i) = varerr(i)
            enddo
          endif
        endif
      enddo
      if (ier.eq.0 .and. metroLoop .gt. 2)
     &    print *,'Search succeeded with this step factor'

C       Final call to FUNCT
      CALL FUNCT(nvarsrch,var,FFINAL,Grad)
      if (ifHush .eq. 0) WRITE(6,98)FFINAL,KOUNT
98    FORMAT(/,T48,'Final   F : ',T65,E14.7,/,' Number of cycles : ',I5)
      call flush(6)
C-----------------------------------------------------------------------
C       Error returns:
      IF(IER.NE.0)THEN
        if(ier.ne.3)then
          call errorexit('Search failed even after varying step factor',
     &        iflocal)
        else
          call errorexit('Minimization error #3 - Iteration limit exceeded',
     &        1)
        endif
        metroerror=metroerror+1
      END IF
      return
      end


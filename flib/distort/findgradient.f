c       
c       FINDGRADIENT finds parameters related to magnification gradients that
c       minimize the displacement errors when Blendmont shifts pieces into
c       registration.   The two gradient parameters are the percent change in
c       magnification and the degrees of rotation per micron of Z height
c       change.  The other parameter that affects the Blendmont error is the
c       effective tilt angle for the corrections, which can differ from the
c       nominal tilt angle by several degrees.  Either the gradient
c       parameters, or the angle offset, or all three can be searched.
c       
c       See man page for more details.
c       
c       David Mastronarde, February 2006
c       
c       $Id$
c       
      implicit none
c       
c       Stuff for amoeba: ftol2 and ptol2 are used the FIRST time
c       
      integer maxvar
      parameter (maxvar = 3)
      real*4 pp(maxvar+1,maxvar+1),yy(maxvar+1),ptmp(maxvar),ptol(maxvar)
      real*4 ptol1, ftol1,ptol2,ftol2,delfac,var(maxvar), da(maxvar)
      data da/0.5,0.2,2.0/
      integer*4 jmin, iter, i, j
      external func
c       
c       Common for func
c       
      include 'findgradient.inc'
c       
      integer*4 ierr,iz,nzdo,nAngles,izdo(limsec),nzlist,nSearch, nMagGrad
      real*4 angles(limsec), dmagPerUm(limsec), rotPerUm(limsec)
      real*4 stepFac, relax, dmagMin, drotMin, dmagInc, drotInc, errNew,errMin2
      integer*4 numSince
      logical separate
      character*40 numString
      character*1 starout
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetLogical,PipGetFloat,PipNumberOfEntries
      integer*4 PipGetString,PipGetTwoFloats,PipGetThreeFloats
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  findgradient
c       
      integer numOptions
      parameter (numOptions = 18)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'imin:ImageInputFile:FN:@plin:PieceListInput:FN:@'//
     &    'rootname:RootNameForFiles:CH:@'//
     &    'distort:DistortionField:FN:@'//
     &    'imagebinned:ImagesAreBinned:I:@'//
     &    'gradient:GradientFile:FN:@adjusted:AdjustedFocus:B:@'//
     &    'addgrad:AddToGradient:FP:@geometry:TiltGeometry:FT:@'//
     &    'tiltfile:TiltFile:FN:@offset:OffsetTilts:F:@'//
     &    'sections:SectionsToDo:LI:@separate:RunSeparately:B:@'//
     &    'search:SearchType:I:@trace:TraceOutput:I:@'//
     &    'blend:BlendOption:CHM:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'
c       
      distName = ' '
      gradName = ' '
      sectionList = ' '
      angleFile = ' '
      focusAdjusted = .false.
      inputBinning = 0
      pixelMagGrad = 0.
      dmagStart = 0.
      drotStart = 0.
      tiltOffStart = 0.
      nSearch = 2
      ifTrace = 0
      ptol1 = 1.e-5
      ftol1 = 1.e-5
      ptol2 = 1.e-3
      ftol2 = 1.e-3
      delfac = 2.
      separate = .false.
      nzdo = 1
      nzlist = 0
      nAngles = 0
      nMagGrad = 0
c       
c       Pip startup: set error, parse options, do help output
c       
      call PipReadOrParseOptions(options, numOptions, 'findgradient',
     &    'ERROR: FINDGRADIENT - ', .false., 0, 0, 0, numOptArg,
     &    numNonOptArg)

      if (PipGetString('RootNameForFiles', rootname) .ne. 0) call
     &    exitError('NO ROOT NAME FOR TEMP FILES SPECIFIED')
      if (PipGetString('ImageInputFile', imageName) .ne. 0) call
     &    exitError('NO IMAGE FILE SPECIFIED')
      if (PipGetString('PieceListInput', plName) .ne. 0) call
     &    exitError('NO PIECE LIST FILE SPECIFIED')
      if (PipGetString('SectionsToDo', sectionList) .eq. 0)
     &    call parselist(sectionList, izdo, nzlist)
      ierr = PipGetInteger('ImagesAreBinned', inputBinning)
      ierr = PipGetString('DistortionField', distName)
c       
c       Read gradient file in if specified to get tilt angles, otherwise check
c       for tilt file and read angles from that
c       
      if (PipGetString('GradientFile', gradName) .eq. 0) then
        call readMagGradients(gradName, limsec, pixelMagGrad, axisRot,
     &      angles, dmagPerUm, rotPerUm, nMagGrad)
        nAngles = nMagGrad
      elseif (PipGetString('TiltFile', angleFile) .eq. 0) then
        call read_tilt_file(nAngles, 4, angleFile, angles, limsec)
      endif
c       
      ierr = PipGetThreeFloats('TiltGeometry', pixelMagGrad, axisRot,
     &    tiltAngle)
      ierr = PipGetTwoFloats('AddToGradient', dmagStart, drotStart)
      if (pixelMagGrad .eq. 0. .and. gradName .eq. ' ') call exitError(
     &    'EITHER GradientFile OR TiltGeometry MUST BE ENTERED')
      ierr = PipGetLogical('AdjustedFocus', focusAdjusted)
      ierr = PipGetInteger('TraceOutput', ifTrace)
      ierr = PipGetLogical('RunSeparately', separate)
      if (separate) then
        if (sectionList .eq. ' ') call exitError(
     &      'SECTIONS MUST BE ENTERED WITH -section TO RUN SEPARATELY')
        nzdo = nzlist
      endif
      ierr = PipGetFloat('OffsetTilts', tiltOffStart)
      ierr = PipGetInteger('SearchType', nSearch)
      ierr = PipNumberOfEntries('BlendOption', numBlendOpt)
      if (numBlendOpt .gt. limopt) call exitError (
     &    'TOO MANY BLEND OPTIONS FOR ARRAYS')
      do iz = 1, numBlendOpt
        ierr = PipGetString('BlendOption', blendOption(iz))
      enddo
c       
      call PipDone()
      if (nSearch .eq. 0 .or. nSearch .gt. 3 .or. nSearch .lt. -2)
     &    call exitError('SEARCH TYPE OUT OF ALLOWED RANGE (-1, -2 and 1-3)')
      if (nSearch .eq. 3 .and. (nAngles .eq. 0 .or. nzlist .lt. 2 .or.
     &    separate)) call exitError(
     &    'SEARCH TYPE 3 NEEDS TILT ANGLES AND MULTIPLE SECTIONS')

      nvar = abs(nSearch)
      if (nvar .eq. 1) da(1) = da(3)
c       
c       Set up root name for temp files with filename and sections
c       
      tmpRoot = trim(rootname)//'-fg'
      if (nzlist .gt. 0) then
        write(numString, '(i6)')-izdo(1)
        tmpRoot = trim(tmpRoot)//trim(adjustl(numString))
        if (nzlist .gt. 1) then
          write(numString, '(i6)')-izdo(nzlist)
          tmpRoot = trim(tmpRoot)//trim(adjustl(numString))
        endif
      endif

      do iz = 1, nzdo
c         
c         If doing separate sections, write the section number and 
c         set the tilt angle if those were read in
c         
        if (separate) then
          write(sectionList, '(i5)')izdo(iz)
          if (nAngles .gt. 0) tiltAngle = angles(min(nAngles, izdo(iz) + 1))
        endif
        
        if (nSearch .eq. -2) then
c           
c           Directed search
c           
          errMin = 1.e30
          errMin2 = 1.e30
          nTrial = 0
          var(1) = dmagStart
          var(2) = drotStart
          relax = sqrt(0.5)
          numSince = 0
c           
c           loop until can't improve error by moving in direction of implied
c           gradient
c           
          do while (numSince .lt. 8 .and. nTrial .lt. 200)
            call func(var, errNew)
            dmagInc = 0.
            drotInc = 0.
            do i = 1, nError
              dmagInc = dmagInc + sectDmag(i) / nError
              drotInc = drotInc + sectDrot(i) / nError
            enddo
c             
c             If get a new minimum, save it and reset step factor
c             Need to check against the error stored by func because of 
c             numerical errors when comparing the func return with the stored
c             value here.
c             
            if (errMin2 .gt. errMin) then
              errMin2 = errMin
              stepFac = 1.
              numSince = 0
              dmagMin = var(1)
              drotMin = var(2)
            else
c               
c               Otherwise relax the step factor
c               
              numSince = numSince + 1
              stepFac = stepFac * relax
            endif
c             write(*,'(21x,3f9.4)')dmagInc, drotInc, stepFac
            var(1) = dmagMin + stepFac * dmagInc
            var(2) = drotMin + stepFac * drotInc
          enddo
          var(1) = dmagMin
          var(2) = drotMin
        elseif (nSearch .eq. -1) then
          errMin = 1.e30
          nTrial = 0
          var(1) = tiltOffStart
          call minimize1D(var(1), func, 4., 0.01, 4.1, 20)
        else
c           
c           amoeba search: set up for minimization
c           
          errMin = 1.e30
          nTrial = 0
          var(1) = dmagStart
          var(2) = drotStart
          var(3) = tiltOffStart
          if (nvar .eq. 1) var(1) = var(3)
          call amoebaInit(pp, yy, maxvar + 1, nvar, delfac, ptol2, var, da, 
     &        func,ptol)
          call amoeba(pp,yy,maxvar+1,nvar,ftol2,func,iter,ptol,jmin)
c           
c           per Press et al. recommendation, just restart at current location
c           
          do i=1,nvar
            var(i)=pp(jmin,i)
          enddo
          call amoebaInit(pp, yy, maxvar + 1, nvar, delfac / 4., ptol1, var,
     &        da, func,ptol)
          call amoeba(pp,yy,maxvar+1,nvar,ftol1,func,iter,ptol,jmin)
c           
c           recover result
c           
          do i = 1, nvar
            var(i) = pp(jmin, i)
          enddo
        endif
        call func(var, errMin2)
c         
        write(*,'(/,a,/,a)')'Final results:','Section - mean error'
        write(*, '(i5,f12.3)')(izsect(i), sectErrs(i), i = 1, nError)
        if (nvar .eq. 2) then
          write(*,73)nTrial,errMin2,var(1),var(2)
73        format(i5,' trials, error= ',f10.4,', gradient=',f8.3,' %/um,',
     &        f8.3, ' deg/um')
        else if (nvar .eq. 3) then
          write(*,74)nTrial,errMin2,var(1),var(2),var(3)
74        format(i5,', err=',f9.4,', grad=',f8.3,' %/um,',
     &        f8.3, ' deg/um, offset=',f6.2,' deg')
        else
          write(*,75)nTrial,errMin2,var(1)
75        format(i5,' trials, error =',f10.4,', offset =',f6.2,' deg')
        endif

        call flush(6)
      enddo
      call exit(0)
      end



c       The function called by AMOEBA with the current values
c       
      subroutine func(p, funcErr)
      implicit none
      integer limnum
      parameter (limnum=20)
      real*4 p(*), funcErr
      include 'findgradient.inc'
c       
      character*160 paramName,outName
      character*240 comstring
      real*4 error, errSum,xnum(limnum)
      character*1 starout
      integer*4 ierr, i, imodBackupFile,numeric(limnum),nfield
c       
      nTrial = nTrial + 1
      paramName = trim(tmpRoot)//'.param'
      ierr = imodBackupFile(paramName)
      open(3,file=paramName,form='formatted',status='new', err=96)

      write(3,101)'ImageInputFile',trim(imageName)
101   format(a,5x,a)
      write(3,101)'PieceListInput',trim(plName)
      comString = trim(tmpRoot)//'.imtmp'
      write(3,101)'ImageOutputFile',trim(comString)
      write(3,101)'RootNameForEdges',trim(tmpRoot)
      if (distName .ne. ' ') write(3,101)'DistortionField',
     &    trim(distName)
      if (inputBinning .gt. 0) write(3,'(a,i6)')'ImagesAreBinned',
     &    inputBinning
      if (gradName .ne. ' ') write(3,101)'GradientFile',
     &    trim(gradName)
      if (angleFile .ne. ' ') write(3,101)'TiltFile',
     &    trim(angleFile)
      if (pixelMagGrad .gt. 0.) write(3,'(a,f12.5,2f8.2)')'TiltGeometry',
     &    pixelMagGrad, axisRot, tiltAngle
      
      if (sectionList .ne. ' ')write(3,101)'SectionsToDo',
     &    trim(sectionList)
      if (nvar .ge. 2) then
        write(3,'(a,2f15.8)')'AddToGradient', p(1), p(2)
      else
        write(3,'(a,2f15.8)')'AddToGradient', dmagStart, drotStart
      endif
      if (nvar .eq. 2) then
        write(3,'(a,f15.8)')'OffsetTilts', tiltOffStart
      else
        write(3,'(a,f15.8)')'OffsetTilts', p(nvar)
      endif

      write(3,'(a,/,a/,a)')'TestMode','SloppyMontage','ShiftPieces'
      do i = 1, numBlendOpt
        write(3,'(a)')blendOption(i)
      enddo
      outName   = trim(tmpRoot)//'.out'
      close(3)
      comstring = 'blendmont -param '//trim(paramName)//' > '//trim(outName)

      ierr = imodBackupFile(outName)
      call system(comString)

      open(3,file=outName,form='formatted',status='old', err=97)

      nError = 0
      errSum = 0.
      do while (.true.)
        read(3,'(a)',end=20,err=98)comString
        if (comString(1:6) .eq. 'ERROR:') then
          print *
          print *,'ERROR: findgradient - Blendmont had the following error:'
          print *,comString
          do while (.true.)
            read(3,'(a)',end=10,err=98)comString
          enddo
10        print *
          print *,'For full log, see ',trim(outName)
          call exit(1)
        endif
c         
        if (comString(16:24) .eq. 'gradient:' .or. comString(17:25) .eq.
     &      'gradient:') then
          call frefor2(comString, xnum, numeric, nfield, limnum)
          if (nfield .lt. 9 .or. numeric(9) .eq. 0 .or. numeric(2) .eq. 0)
     &        call exitError('READING GRADIENT OUTPUT FROM BLENDMONT')
          error = xnum(9)
          errSum = errSum + error
          nError = nError + 1
          sectErrs(nError) = error
          sectDmag(nError) = 0.
          sectDrot(nError) = 0.
          izsect(nError) = nint(xnum(2))
        elseif ((comString(1:7) .eq. 'Implied' .or. comString(2:8) .eq.
     &        'Implied') .and. nError .gt. 0) then
          call frefor2(comString, xnum, numeric, nfield, limnum)
          if (nfield .lt. 5 .or. numeric(4) .eq. 0 .or. numeric(5) .eq. 0)
     &        call exitError('READING IMPLIED GRADIENT OUTPUT FROM BLENDMONT')
          sectDmag(nError) = xnum(4)
          sectDrot(nError) = xnum(5)
        endif
      enddo
20    if (nError .eq. 0) call exitError(
     &    'NO EDGE ERRORS FOUND IN BLENDMONT OUTPUT')
      funcErr = errSum / nError
      starout=' '
      if(funcErr.lt.errMin)then
        starout='*'
        errMin=funcErr
      endif
      if(iftrace.gt.1.or.(iftrace.gt.0.and.starout.eq.'*')) then
        write(*,72)starout,nTrial,funcErr, (p(i), i = 1,nvar)
72      format(1x,a1,i4,f15.5, 3f9.4)
        call flush(6)
      endif
c       
c       Amoeba flails on hard zeros so add something (no longer 6/17/06)
c       
      close(3)
      return
96    call exitError('OPENING BLENDMONT PARAMETER FILE')
97    call exitError('OPENING BLENDMONT OUTPUT FILE')
98    call exitError('READING BLENDMONT OUTPUT')
      end


c       Minimizes a 1 dimensional function funk that is assumed to be monotonic
c       around the minimum.  pmin is a starting value and is returned with the
c       minimum, pstep is the initial stpe size for scanning from pstart,
c       and the search terminates when the interval in which the minimum lies
c       is less than ptol.  If points equal to the minimum are found, it scans
c       the whole interval at a finer grain to find a smaller interval
c       reliably.
c       
      subroutine minimize1D(pmin, funk, pstep, ptol, scanInt, numScan)
      implicit none
      real*4 pmin, pstep, ptol, scanInt
      external funk
      integer*4 idir, iter, maxiter, numScan
      real*4 fnew, pnew, fmin, fAbove, pAbove, fBelow, pBelow, equalCrit
      logical*4 scanning, bigScanned

      maxiter = 500
      idir = 1
      call funk(pmin, fmin)
      equalCrit = 1.e-6 * abs(fmin)
      scanning = .true.
      bigScanned = .false.
      iter = 1
      do while (scanning .and. iter .lt. maxiter)
c         
c         Take a step from current minimum
        pnew = pmin + idir * pstep
        call funk(pnew, fnew)
        iter = iter + 1
        if (fnew .gt. fmin) then
c           
c           If higher than min, then record above or below point depending
c           on direction
          if (idir .gt. 0) then
            pAbove = pnew
            fAbove = fnew
          else
            pBelow = pnew
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
            pAbove = pmin
            fAbove = fmin
          else
            pBelow = pmin
            fBelow = fmin
          endif
          pmin = pnew
          fmin = fnew
        endif
      enddo
c       
c       Now search until the interval becomes small enough.  Start in most
c       promising direction
      idir = 1
      if (fBelow .lt. fAbove) idir = -1
      do while (pAbove - pBelow .gt. ptol .and. iter .lt. maxiter)
c         
c         Is it time for a big special scan?
c         
        if (.not. bigScanned .and. pAbove - pBelow .lt. scanInt) then
          call specialScan(funk, pBelow, fBelow, pAbove, fAbove, pmin, fmin,
     &        equalCrit, numScan, iter)
          bigScanned = .true.
        else
c           
c           Go in the given direction halfway between min and endpoint
          if (idir .gt. 0) then
            pnew = (pmin + pAbove) / 2.
          else
            pnew = (pmin + pBelow) / 2.
          endif
          call funk(pnew, fnew)
          iter = iter + 1
          if (abs(fnew - fmin) .lt. equalCrit) then
c             
c             If equal to minimum, do a special scan
            call specialScan(funk, pBelow, fBelow, pAbove, fAbove, pmin, fmin,
     &          equalCrit, 10, iter)
c             
          elseif (fnew .lt. fmin) then
c             
c             If new minimum, replace the old, go in most promising direction
c             again
            if (idir .lt. 0) then
              pAbove = pmin
              fAbove = fmin
            else
              pBelow = pmin
              fBelow = fmin
            endif
            pmin = pnew
            fmin = fnew
            idir = 1
            if (fBelow .lt. fAbove) idir = -1
          else
c             
c             Or replace the endpoint, go in opposite direction
            if (idir .gt. 0) then
              pAbove = pnew
              fAbove = fnew
            else
              pBelow = pnew
              fBelow = fnew
            endif
            idir = -idir
          endif
        endif
      enddo
      return
      end

      subroutine specialScan(funk, pBelow, fBelow, pAbove, fAbove, pmin, fmin,
     &    equalCrit, numScan, iter)
      implicit none
      real*4 pBelow, fBelow, pAbove, fAbove, pmin, fmin, equalCrit
      integer*4 numScan, iter, iScan, iAbove, iBelow
      external funk
      real*4 fScan(51), fnew, pnew, delScan
      fScan(1) = fBelow
      fScan(numScan + 1) = fAbove
      fmin = fAbove
      delScan =  (pAbove - pBelow) / numScan
      do iScan = 2, numScan
        pnew = pBelow + (iScan - 1) * delScan
        call funk(pnew, fnew)
        fScan(iScan) = fnew
        if (fnew .lt. fmin) fmin = fnew
      enddo
c       
c       Find last one before minimum and first after
c       
      iAbove = numScan + 1
      iBelow = 1
      do iScan = 2, numScan
        if (abs(fScan(iScan) - fmin) .gt. equalCrit .and.
     &      iBelow + 1 .eq. iScan) iBelow = iScan
      enddo
      do iScan = numScan, 2, -1
        if (abs(fScan(iScan) - fmin) .gt. equalCrit .and.
     &      iAbove .eq. iScan + 1) iAbove = iScan
      enddo
c       
c       If interval gets no smaller, give up
c       
      if (iAbove - iBelow .ge. numScan) then
        pmin = 0.5 * (fAbove + fBelow)
        fAbove = fBelow
      else
c         
c         otherwise proceed with new interval and min
c         
        iScan = (iBelow + iAbove) / 2
        pmin = pBelow + (iScan - 1) * delScan
        pAbove = pBelow + (iAbove - 1) * delScan
        pBelow = pBelow + (iBelow - 1) * delScan
        fBelow = fScan(iBelow)
        fAbove = fScan(iAbove)
      endif
      return
      end

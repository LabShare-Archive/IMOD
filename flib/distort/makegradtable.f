c
c       MAKEGRADTABLE will take the output from aligning image pairs with
c       stage movement or analyzing montages at high tilt and produce a mag
c       gradient table, with smoothing and extrapolation.
c       
c       See man page for more details.
c       
c       David Mastronarde 2/6/06
c
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c
c       $Log$
c
      implicit none
      integer idim, limnum
      parameter (idim = 1000, limnum = 20)
      character*120 xffile, findfile, outfile, intfile, rawfile, line
      logical*4 smoothMag, smoothRot
      integer*4 nFitSmoothMag, nFitSmoothRot, nFitExtrapMag, nFitExtrapRot
      integer*4 numC2all, numXF, numeric(limnum)
      real*4 f(2,3,idim), c2raw(idim), c2lin(idim), dmagRaw(idim), c2all(idim)
      real*4 drotRaw(idim), dmagSmth(idim), drotSmth(idim), xnum(limnum)
      real*4 extremeLo, extremeHi, fromCross, umMoved, theta, smag, str, phi
      integer*4 ierr, i, j, iunOut, numGrad, numSect, nField, isect
      integer*4 indBelow, indStr, indEnd
      real*4 c2sum, temp, extrapLo, extrapHi, dmag, drot, crossover, extrapLin
      logical b3dxor
      real*4 fittedValue

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoIntegers,PipGetLogical,PipGetFloat
      integer*4 PipGetString,PipGetTwoFloats, PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  makegradtable
c       
      integer numOptions
      parameter (numOptions = 15)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'findgrad:FindgradFile:FN:@xffile:TransformFile:FN:@'//
     &    'intensity:IntensityFile:FN:@output:OutputTable:FN:@'//
     &    'raw:RawOutput:FN:@crossover:CrossoverIntensity:F:@'//
     &    'microns:MicronsMoved:F:@msmooth:SmoothMags:B:@'//
     &    'rsmooth:SmoothRotations:B:@'//
     &    'extremes:ExtremesForExtrapolation:FP:@'//
     &    'sfit:MaxInFitForSmoothing:IP:@'//
     &    'efit:MaxInFitForExtrapolation:IP:@'//
     &    'distance:DistanceFromCrossover:F:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c
      xffile = ' '
      findfile = ' '
      rawfile = ' '
      smoothRot = .false.
      smoothMag = .false.
      extremeLo = 0.2
      extremeHi = 0.8
      nFitSmoothMag = 5
      nFitSmoothRot = 0
      nFitExtrapMag = 0
      nFitExtrapRot = 0
      fromCross = 0.5
      iunOut = 6
      umMoved = 10.

      call PipReadOrParseOptions(options, numOptions, 'makegradtable',
     &    'ERROR: makegradtable - ', .false., 4, 0, 1, numOptArg,
     &    numNonOptArg)
c
      if (PipGetInOutFile('OutputTable', 1, ' ', outfile)
     &    .ne. 0) call errorexit('NO OUTPUT FILE FOR TABLE SPECIFIED')
      ierr = PipGetString('TransformFile', xffile)
      ierr = PipGetString('FindgradFile', findfile)
      if (PipGetString('IntensityFile', intfile) .ne. 0) call errorexit(
     &    'NO INPUT FILE WITH INTENSITIES SPECIFIED')
      ierr = PipGetString('RawOutput', rawfile)

      if (.not.b3dxor(xffile .ne. ' ', findfile .ne. ' ')) call errorexit(
     &    'FindgradFile OR TransformFile MUST BE ENTERED, BUT NOT BOTH')
      if (PipGetFloat('CrossoverIntensity', crossover) .ne. 0) call errorexit(
     &    'CROSSOVER INTENSITY MUST BE ENTERED')

      ierr = PipGetLogical('SmoothMags', smoothMag)
      ierr = PipGetLogical('SmoothRotations', smoothRot)
      ierr = PipGetTwoFloats('ExtremesForExtrapolation', extremeLo, extremeHi)
      ierr = PipGetTwoIntegers('MaxInFitForSmoothing', nFitSmoothMag,
     &    nFitSmoothRot)
      ierr = PipGetTwoIntegers('MaxInFitForExtrapolation', nFitExtrapMag,
     &    nFitExtrapRot)
      ierr = PipGetFloat('DistanceFromCrossover', fromCross)
      ierr = PipGetFloat('MicronsMoved', umMoved)
      
      if (nFitSmoothMag .lt. 0 .or. nFitSmoothMag .eq. 1 .or. nFitSmoothRot
     &    .lt. 0 .or. nFitSmoothRot .eq. 1) call errorexit(
     &    'MAXIMUM IN FIT FOR SMOOTHING MUST BE 0 OR > 1')
      if (nFitExtrapMag .lt. 0 .or. nFitExtrapMag .eq. 1 .or. nFitExtrapRot
     &    .lt. 0 .or. nFitExtrapRot .eq. 1) call errorexit(
     &    'MAXIMUM IN FIT FOR EXTRAPOLATING MUST BE 0 OR > 1')
      call PipDone()
c
      if (rawfile .ne. ' ') then
        call dopen(7, rawfile, 'new', 'f')
        iunOut = 7
      endif
c       
      call dopen(8, outfile, 'new', 'f')
      write(8,'(i3,/,f10.5)')2, crossover
c       
c       get intensities
c
      call dopen(1, intfile, 'ro', 'f')
      numC2all = 0
10    read(1, *, end=12, err=95)c2all(numC2all + 1)
      numC2all = numC2all + 1
      if (numC2all .ge. idim) call errorexit('TOO MANY INTENSITIES FOR ARRAYS')
      go to 10
12    close(1)
c       
c       Read transforms, take every other one
c
      if (xffile .ne. ' ') then
        call dopen(1, xffile, 'ro', 'f')
        call xfrdall(1, f, numXF, *96)
        close(1)
        if (numXF .gt. idim) call errorexit('TOO MANY TRANSFORMS FOR ARRAYS')
        if (numXF .gt. numC2all) call errorexit(
     &      'NOT ENOUGH INTENSITIES FOR THE TRANSFORMS')
        numGrad = numXF / 2
        do i = 1, numGrad
          c2raw(i) = c2all(2 * i)
          call amat_to_rotmagstr(f(1,1,2 * i), theta, smag, str, phi)
          dmagRaw(i) = 100. * (smag * sqrt(str) - 1.) / umMoved
          drotRaw(i) = theta / umMoved
        enddo
      else
c         
c         Or read a findgradient log file
c         
        call dopen(1, findfile, 'ro', 'f')
        numGrad = 0
        do while (numGrad .lt. idim)
          read(1,'(a)', end=20)line
          if (line(1:7) .eq. 'Section' .or. line(2:8) .eq. 'Section') then
            numSect = 0
            c2sum = 0.
            nField = 2
            do while (nField .eq. 2)
              read(1,'(a)', end=20)line
              call frefor2(line, xnum, numeric, nField, limnum)
              if (nField .eq. 2) then
                numSect = numSect + 1
                if (numeric(1) .eq. 0) call errorexit(
     &              'READING SECTION NUMBER FROM GRADIENT FILE')
                isect = nint(xnum(1)) + 1
                if (isect .lt. 1 .or. isect .gt. numC2all) call errorexit(
     &              'SECTION NUMBER NEGATIVE OR BIGGER THAN NUMBER '//
     &              'OF INTENSITIES')
                c2sum = c2sum + c2all(isect)
              endif
            enddo
            if (nField .lt. 8 .or. numeric(6) .eq. 0 .or. numeric(8) .eq. 0)
     &          call errorexit('GRADIENT NOT READABLE IN FIELDS 6 AND 8')
            if (numSect .eq. 0) call errorexit(
     &          'NO SECTION NUMBER FOUND BEFORE GRADIENT')
            numGrad = numGrad + 1
            c2raw(numGrad) = c2sum / numSect
            dmagRaw(numGrad) = xnum(6)
            drotRaw(numGrad) = xnum(8)
          endif
        enddo
20      if (numGrad .eq. idim) call errorexit('TOO MANY GRADIENTS FOR ARRAYS')
      endif
c       
c       sort the data
c       
      do i = 1, numGrad - 1
        do j = i + 1, numGrad
          if (c2raw(i) .gt. c2raw(j)) then
            temp = c2raw(i)
            c2raw(i) = c2raw(j)
            c2raw(j) = temp
            temp = dmagRaw(i)
            dmagRaw(i) = dmagRaw(j)
            dmagRaw(j) = temp
            temp = drotRaw(i)
            drotRaw(i) = drotRaw(j)
            drotRaw(j) = temp
          endif
        enddo
      enddo
c       
c       get linearized values, find crossover
c       
      indBelow = 0
      do i = 1, numGrad
        c2lin(i) = 1. / (c2raw(i) - crossover)
        if (c2raw(i) .lt. crossover) indBelow = i
      enddo
c
c       Set up for below crossover, loop on bleow and above, do a side of
c       crossover if there is at least one point
c
      indStr = 1
      indEnd = indBelow
      extrapLo = extremeLo
      if (indEnd .gt. 0) extrapHi = crossover + fromCross / c2lin(indEnd)
      do j = 1, 2
        if (indStr .le. indEnd) then
c           
c           Extrapolate to low point if there is more than one point to fit,
c           extrap is below range, and it is not crossover
c
          if (indStr .lt. indEnd .and. extrapLo .ne. crossover .and.
     &        extrapLo .lt. c2raw(indStr)) then
            extrapLin = 1./(extrapLo - crossover)
            dmag = fittedValue(c2lin, dmagRaw, indStr, indEnd, indStr,
     &          nFitExtrapMag, extrapLin)
            drot = fittedValue(c2lin, drotRaw, indStr, indEnd, indStr,
     &          nFitExtrapRot, extrapLin)
            write(iunout,101)2,extrapLo,0.01*extrapLin,dmag,dmag, drot,drot
            write(8, 102) extrapLo, dmag, drot
101         format(i1,2f10.5,4f9.3)
102         format(f10.5,2f9.3)
          endif
c           
c           Smooth at each point if there is more than one point
c
          do i = indStr, indEnd
            dmag = dmagRaw(i)
            drot = drotRaw(i)
            if (indEnd .gt. indStr) then
              if (smoothMag) dmag = fittedValue(c2lin,
     &            dmagRaw, indStr, indEnd, i, nFitSmoothMag, c2lin(i))
              if (smoothRot) drot = fittedValue(c2lin,
     &            drotRaw, indStr, indEnd, i, nFitSmoothRot, c2lin(i))
            endif
            write(iunout,101)1,c2raw(i),0.01*c2lin(i),dmagRaw(i),dmag,
     &          drotRaw(i),drot
            write(8, 102) c2raw(i), dmag, drot
          enddo
c           
c           Extrapolate to high point if there is more than one point to fit,
c           extrap is above range, and it is not crossover
c
          if (indStr .lt. indEnd .and. extrapHi .ne. crossover .and.
     &        extrapHi .gt. c2raw(indEnd)) then
            extrapLin = 1./(extrapHi - crossover)
            dmag = fittedValue(c2lin, dmagRaw, indStr, indEnd, indEnd,
     &          nFitExtrapMag, extrapLin)
            drot = fittedValue(c2lin, drotRaw, indStr, indEnd, indEnd,
     &          nFitExtrapRot, extrapLin)
            write(iunout,101)2,extrapHi,0.01*extrapLin,dmag,dmag,drot,drot
            write(8, 102) extrapHi, dmag, drot
          endif
        endif
c         
c         Set up for above crossover
c
        indStr = indBelow + 1
        indEnd = numGrad
        if (indStr .le. numGrad) extrapLo = crossover + fromCross /
     &      c2lin(indStr)
        extrapHi = extremeHi
      enddo
      close(8)
      if (iunOut .eq. 7) close(7)
      call exit(0)
95    call errorexit('READING INTENSITIES')
96    call errorexit('READING TRANSFORMS')
      end

      real*4 function fittedValue(x, y, indStr, indEnd, indCen, maxFit, xeval)
      implicit none
      real*4 x(*), y(*), xeval
      integer*4 indStr, indEnd, indCen, maxFit, istr, iend, nfit
      real*4 slope, bint, ro, se, sb,sa,yeval, prederr
      if (maxFit .eq. 0) then
        istr = indStr
        iend = indEnd
      else
        istr = max(indStr, indCen - maxFit / 2)
        iend = min(indEnd, istr + maxFit - 1)
        istr = max(indStr, iend + 1 - maxFit)
      endif
      nfit = iend + 1 - istr
      call lsfitpred(x(istr), y(istr), nfit, slope, bint, ro, se, sb,
     &    sa, xeval, yeval, prederr)
      fittedValue = yeval
      return
      end

        
      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: MAKEGRADTABLE - ',message
      call exit(1)
      end

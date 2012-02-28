*****   FINDBEADS3D    *********************************************
*       
*       Will find gold beads in a tomogram, given the size of the beads
c       
c       For details, see man page
*       
c       $Id$
c       
      implicit none
      integer maxPiece, maxArray, limHisto
      parameter (maxPiece= 400, maxArray = 200000000)
      parameter (limHisto = 10000)
      integer*4 lenPiece(maxPiece,3), ind0(maxPiece,3), ind1(maxPiece,3)
      integer*4 nx,ny,nz,nxyz(3),nxCorr, nyCorr, nzCorr, limPeak
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      integer*4, allocatable :: indPeak(:), indCorr(:)
      real*4, allocatable :: peakVal(:), peakPos(:,:), corrVal(:),corrPos(:, :)
      real*4 origin(3), delta(3), histo(limHisto)
      integer*4 MXYZ(3), mode, maxShift, i, index, loopCorr, numLook
      integer*4 nsum, nxOverlap, nyOverlap, nzOverlap, ierr, maxVol
      integer*4 minYsize, minZsize, maxZ, maxYZ, numXpieces, numYpieces
      integer*4 numZpieces, numPeaks, maxPeaks, numCorrs, indPlanes
      integer*4 ixStart, ixEnd, iyStart, iyEnd, izStart, izEnd
      real*4 elongation, radius, distMin, xpeak, ypeak, zpeak
      real*4 dmin,dmax,dmean,polarity, peakCorr, fracAvg
c      integer*4 iy, iz
      integer*4 ix, ixPiece, iyPiece, izPiece, minInside, maxXsize
      integer*4 ixpeak, iypeak, izpeak, numSave, ix0,ix1,iy0,iy1,iz0,iz1
      integer*4 numPass, minGuess, iVerbose, ibinning
      real*4 sumXoffset, sumYoffset, sumZoffset, storeThresh,beadSize,dtor
      real*4 histDip, peakBelow, peakAbove, dxadj, dyadj, dzadj,avgThresh
      real*4 peakRelMin, sepMin, blackThresh, findCorr, aboveAvg, aboveSD
      
      logical loaded,yElongated,lightBeads, found, cleanBoth
      character*320 imageFile, modelFile, firstFile, tiltFile
      integer*4 findHistogramDip

      real*4 array(maxArray)
      common /bigarr/ array
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetLogical
      integer*4 PipGetString,PipGetFloat,PipGetTwoFloats
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  findbeads3d
c       
      integer numOptions
      parameter (numOptions = 19)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'candidate:CandidateModel:FN:@size:BeadSize:F:@'//
     &    'binning:BinningOfVolume:I:@light:LightBeads:B:@'//
     &    'angle:AngleRange:FP:@tilt:TiltFile:FN:@ylong:YAxisElongated:B:@'//
     &    'peakmin:MinRelativeStrength:F:@'//
     &    'threshold:ThresholdForAveraging:F:@store:StorageThreshold:F:@'//
     &    'spacing:MinSpacing:F:@both:EliminateBoth:B:@'//
     &    'guess:GuessNumBeads:I:@max:MaxNumBeads:I:@'//
     &    'verbose:VerboseOutput:I:@param:ParameterFile:PF:@help:usage:B:'
c       
      dtor = 0.0174532
      modelFile = ' '
      firstFile = ' '
      elongation = 1.
      yElongated = .false.
      polarity = -1.
      lightBeads = .false.
      cleanBoth = .false.
      minInside = 64
      fracAvg = 0.5
      peakRelMin = 0.05
      numPass = 1
      maxPeaks = 50000
      sepMin = 0.9
      avgThresh = -2.
      storeThresh = 0.
      iVerbose = 0
      minGuess = 0
      ibinning = 1
c       
c       Pip startup: set error, parse options, do help output
c       
      call PipReadOrParseOptions(options, numOptions, 'findbeads3d',
     &    'ERROR: FINDBEADS3D - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
C       
C       Open image file
C       
      if (PipGetInOutFile('InputFile', 1, ' ', imageFile)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      
      ierr = PipGetInOutFile('OutputFile', 2, ' ', modelFile)
      CALL IMOPEN(1,imageFile,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      call irtdel(1, delta)
      call irtorg(1, origin(1), origin(2), origin(3))

      if (PipGetFloat('BeadSize', beadSize) .ne. 0) call exitError(
     &    'BEAD DIAMETER MUST BE ENTERED')
      ierr = PipGetInteger('BinningOfVolume', ibinning)
      if (ibinning .lt. 1) call exitError('BINNING MUST BE POSITIVE')
      beadSize = beadSize / ibinning
      radius = beadSize / 2.

      ierr = PipGetLogical('YAxisElongated', yElongated)
c       
      ierr = PipGetFloat('MinRelativeStrength', peakRelMin)
      ierr = PipGetFloat('StorageThreshold', storeThresh)
      ierr = PipGetFloat('ThresholdForAveraging', avgThresh)
c       
      ierr = PipGetLogical('LightBeads', lightBeads)
      if (lightBeads) polarity = 1.
      ierr = PipGetString('CandidateModel', firstFile)
c
      ierr = PipGetFloat('MinSpacing', sepMin)
      distMin = sepMin * beadSize
      ierr = PipGetLogical('EliminateBoth', cleanBoth)
      ierr = PipGetInteger('GuessNumBeads', minGuess)
      ierr = PipGetInteger('MaxNumBeads', maxPeaks)
      ierr = PipGetInteger('VerboseOutput', iVerbose)
      ierr = PipGetTwoFloats('AngleRange', dxadj, dyadj)
      ix = PipGetString('TiltFile', tiltFile)
      if (ierr .eq. 0 .or. ix .eq. 0) then
        if (ierr + ix .eq. 0) call exitError(
     &      'YOU CANNOT ENTER BOTH AN ANGLE RANGE AND A TILT FILE')
        if (ix .eq. 0) then
          call dopen(1, tiltFile, 'ro', 'f')
          dxadj = 2000.
          dyadj = -2000.
10        read(1,*,err=97,end=20)dzadj
          dxadj = min(dzadj, dxadj)
          dyadj = max(dzadj, dyadj)
          go to 10
20        continue
        endif
c         
c         Elongation factor from Radermacher 1988 paper
        dzadj = 0.5 * (abs(dxadj) + abs(dyadj)) * dtor
        elongation = sqrt((dzadj + cos(dzadj) * sin(dzadj)) /
     &      (dzadj - cos(dzadj) * sin(dzadj)))
        print *,'Elongation factor is', elongation
      endif
      if (maxPeaks .lt. 3) call exitError('-maxPeaks entry is negative or too small')
      limPeak = maxPeaks + 10
      allocate(indPeak(limPeak), indCorr(limPeak), peakVal(limPeak), peakPos(3, limPeak), 
     &    corrVal(limPeak),corrPos(3, limPeak), stat=ierr)
      call memoryError(ierr, 'ALLOCATING ARRAYS FOR PEAKS')
c       
      call PipDone()
c       
c       Determine the correlation box size 
c       
      nxCorr = 2 * nint(1.5 * radius + 0.5)
      nyCorr = nxCorr
      nzCorr = 2 * nint((0.5 + elongation) * radius + 0.5)
      if (yElongated) then
        nyCorr = nzCorr
        nzCorr = nxCorr
      endif
      maxVol = maxArray - nxCorr * nyCorr * nzCorr
c       
c       Given correlation dimensions, set up the overlaps and minimum sizes
c       
      nxOverlap = 2 * ((nxCorr + 1) / 2 + 1)
      nyOverlap = 2 * ((nyCorr + 1) / 2 + 1)
      nzOverlap = 2 * ((nzCorr + 1) / 2 + 1)
      minYsize = minInside + nyOverlap
      minZsize = minInside + nzOverlap
      maxZ = maxVol / (nx * ny) - 3
      maxYZ = (sqrt(9. + 4. * maxVol / nx) - 3.) / 2.
      if (iVerbose .gt. 0)print *,maxVol,nxOverlap,minYsize,minZsize,maxZ,maxYZ
c
      if (maxZ .ge. nz) then
c         
c         the entire load will fit at once, set min's to ny and nz
c         
        minZsize = nz
        minYsize = ny
      else if (maxZ / 2 .ge. minZsize) then
c         
c         X/Y planes will fit; set miny to ny and increase minZsize
c         
        minYsize = ny
        minZsize = maxZ / 2
      else if (maxYZ / 2 .ge. minZsize .and. maxYZ / 2 .ge. minYsize) then
c         
c         X rows will fit in their entirety; increase the min sizes
c         
        minYsize = maxYZ / 2
        minZsize = maxYZ / 2
      endif
c       
c       Get the definition of the pieces
c       
      call definePieces(nz, nzOverlap, minZsize, 0, maxPiece, numZpieces,
     &    lenPiece(1,3), ind0(1,3), ind1(1,3), ierr)
      if (ierr. ne. 0) call exitError(
     &    'TOO MANY PIECES FOR ARRAY IN Z DIMENSION')
      call definePieces(ny, nyOverlap, minYsize, 0, maxPiece, numYpieces,
     &    lenPiece(1,2), ind0(1,2), ind1(1,2), ierr)
      if (ierr. ne. 0) call exitError(
     &    'TOO MANY PIECES FOR ARRAY IN Y DIMENSION')
      maxXsize = maxVol / (lenPiece(1,2) * (lenPiece(1,3) + 3))
      call definePieces(nx, nxOverlap, 0, maxXsize, maxPiece, numXpieces,
     &    lenPiece(1,1), ind0(1,1), ind1(1,1), ierr)
      if (ierr. ne. 0) call exitError(
     &    'TOO MANY PIECES FOR ARRAY IN X DIMENSION')
c
c       Redefine the longest dimension of Y or Z
      if (ny .gt. nz) then
        maxXsize = maxVol / (lenPiece(1,1) * (lenPiece(1,3) + 3))
        if (iVerbose .gt. 0)print *,'New max for y', maxXsize
        call definePieces(ny, nyOverlap, 0, maxXsize, maxPiece,
     &      numYpieces, lenPiece(1,2), ind0(1,2), ind1(1,2), ierr)
      else
        maxXsize = maxVol / (lenPiece(1,1) * (lenPiece(1,2) + 3))
        if (iVerbose .gt. 0)print *,'New max for z', maxXsize
        call definePieces(nz, nzOverlap, 0, maxXsize, maxPiece,
     &      numZpieces, lenPiece(1,3), ind0(1,3), ind1(1,3), ierr)
      endif

      if (ierr. ne. 0 .or. lenPiece(1,1) * lenPiece(1,2) * (lenPiece(1,3) + 3)
     &    .gt. maxVol) call exitError('BUG IN DIVIDING VOLUME INTO PIECES')

      nsum = max(1, nint(0.75 * radius))
      if (iVerbose .gt. 0) then
        print *,numXpieces, numYpieces,numZpieces,nxOverlap,nyOverlap,nzOverlap
        print *,(ind0(i,1),ind1(i,1),lenPiece(i,1),i = 1,numXpieces)
        print *,(ind0(i,2),ind1(i,2),lenPiece(i,2),i = 1,numYpieces)
        print *,(ind0(i,3),ind1(i,3),lenPiece(i,3),i = 1,numZpieces)
        print *,'nsum = ', nsum
      endif
      numPeaks = 0
      indPlanes = lenPiece(1,1) * lenPiece(1,2) * lenPiece(1,3) + 1
c       
c       Scan for peaks in simple pixel sums
c       
      do izPiece = 1, numZpieces
        do iyPiece = 1, numYpieces
          do ixPiece = 1, numXpieces
            call loadvol(1, array, lenPiece(ixPiece,1), lenPiece(iyPiece,2),
     &          ind0(ixPiece,1), ind1(ixPiece,1), ind0(iyPiece,2),
     &          ind1(iyPiece,2), ind0(izPiece,3), ind1(izPiece,3))
            call getAnalysisLimits(ind0(ixPiece,1), ind1(ixPiece,1), nx,
     &          nxOverlap, nsum, ixStart, ixEnd)
            call getAnalysisLimits(ind0(iyPiece,2), ind1(iyPiece,2), ny,
     &          nyOverlap, nsum, iyStart, iyEnd)
            call getAnalysisLimits(ind0(izPiece,3), ind1(izPiece,3), nz,
     &          nzOverlap, nsum, izStart, izEnd)
            call findPixelSumPeaks(array, array(indPlanes),
     &          lenPiece(ixPiece,1), lenPiece(iyPiece,2), lenPiece(izPiece,3),
     &          ixStart, ixEnd, iyStart, iyEnd, izStart, izEnd,
     &          ind0(ixPiece,1), ind0(iyPiece,2), ind0(izPiece,3), nsum,
     &          polarity, nxCorr, nyCorr, nzCorr, indPeak, peakVal, peakPos,
     &          maxPeaks, numPeaks, peakRelMin)
          enddo
        enddo
      enddo
c       
c       remove points that are too close to each other
c       
      print *,numPeaks,' candidate peaks found'
      peakCorr = peakVal(indPeak(1))
      do i = 1, numPeaks
        peakVal(indPeak(i)) = peakVal(indPeak(i)) / peakCorr
        corrVal(i) = peakVal(indPeak(i))
        if (iVerbose .gt. 1) write(*,'(f8.4,3f8.1)')peakVal(indPeak(i)),
     &      (peakPos(ix,indPeak(i)), ix = 1, 3)
      enddo

      call cleanSortedList(indPeak, peakVal, peakPos, numPeaks, distMin,
     &    cleanBoth, peakRelMin)
      print *,numPeaks,' candidate peaks left after eliminating close points'
c       
      call flush(6)
      ierr = findHistogramDip(corrVal, numPeaks, minGuess, histo, limHisto,
     &    0., 1., histDip, peakBelow, peakAbove, max(0, iVerbose - 2))
      blackThresh = histDip
c
      if (avgThresh .lt. 0) then
        if (ierr .ne. 0) call exitError('NO HISTOGRAM DIP FOUND FOR INITIAL'//
     &      ' PEAKS; ENTER POSITIVE -thresh TO PROCEED')
        findCorr = histDip
        if (avgThresh .lt. -1) then
          call findValueInList(corrVal, numPeaks,
     &        histDip + (peakAbove - histDip) / 4., numLook)
        else
          call findValueInList(corrVal, numPeaks, histDip, numLook)
          numLook = max(1, nint(-avgThresh * numLook))
        endif
      else 
c         
c         Find number to average for number between 0 and 1
        if (avgThresh .le. 1.) then
          call findValueInList(corrVal, numPeaks, avgThresh, numLook)
        else
          numLook = nint(avgThresh)
        endif
        if (ierr.ne. 0) blackThresh = 0
      endif
      call writePeakModel(firstFile, indPeak, peakVal, peakPos,
     &    numPeaks, blackThresh, nxyz, delta, origin, radius)
c       
c       Now loop through the pieces again looking for points and getting
c       correlation positions
c       
c       
      maxShift = max(8, nint(radius))
      numCorrs = 0
      numSave = numPeaks
      print *,numLook,' peaks being averaged to make reference for correlation'
      do loopCorr = 1, 2 * numPass
c         
c         Zero the array on odd loops
        if (mod(loopCorr, 2) .eq. 1) then
          do i = 1, nxCorr * nyCorr * nzCorr
            array(maxVol + i) = 0.
          enddo
        endif
c         
c         Copy correlation positions to peak positions on loop 3
        if (loopCorr .eq. 3) then
          do i = 1, numCorrs
            ix  = indCorr(i)
            indPeak(i) = ix
            peakVal(ix) = corrVal(ix)
            peakPos(1,ix) = corrPos(1,ix)
            peakPos(2,ix) = corrPos(2,ix)
            peakPos(3,ix) = corrPos(3,ix)
          enddo
          numPeaks = numCorrs
          numCorrs = 0
        endif
        if (loopCorr .eq. 2 * numPass) numLook = numSave
        if (iVerbose .gt. 0) print *,loopCorr, numLook
        do izPiece = 1, numZpieces
          do iyPiece = 1, numYpieces
            do ixPiece = 1, numXpieces
              loaded = numXpieces * numYpieces * numZpieces .eq. 1
              call getAnalysisLimits(ind0(ixPiece,1), ind1(ixPiece,1), nx,
     &            nxOverlap, nxCorr, ixStart, ixEnd)
              call getAnalysisLimits(ind0(iyPiece,2), ind1(iyPiece,2), ny,
     &            nyOverlap, nyCorr, iyStart, iyEnd)
              call getAnalysisLimits(ind0(izPiece,3), ind1(izPiece,3), nz,
     &            nzOverlap, nzCorr, izStart, izEnd)
c               
c               Loop on points, for ones inside the box, get correlation
c               
              if (iVerbose .gt. 0) print *,ixStart,ixEnd,iyStart,iyEnd,
     &            izStart,izEnd
              do i = 1, numLook
                ix = indPeak(i)
                xpeak = peakPos(1,ix) - ind0(ixPiece,1)
                ypeak = peakPos(2,ix) - ind0(iyPiece,2)
                zpeak = peakPos(3,ix) - ind0(izPiece,3)
                ixpeak = nint(xpeak)
                iypeak = nint(ypeak)
                izpeak = nint(zpeak)
                if (ixpeak .ge. ixStart .and. ixpeak .le. ixend .and.
     &              iypeak .ge. iyStart .and. iypeak .le. iyend .and.
     &              izpeak .ge. izStart .and. izpeak .le. izend) then
                  
c                   print *,i,xpeak,ypeak,zpeak
                  if (.not.loaded) then
                    call loadvol(1, array, lenPiece(ixPiece,1),
     &                  lenPiece(iyPiece,2),
     &                  ind0(ixPiece,1), ind1(ixPiece,1), ind0(iyPiece,2),
     &                  ind1(iyPiece,2), ind0(izPiece,3), ind1(izPiece,3))
                    loaded = .true.
                  endif

                  if (mod(loopCorr, 2) .eq. 1) then
c                     
c                     On the first round from pixel sums, get centroid before
c                     adding peak in
                    if (loopCorr .eq. 1) then
                      ix0 = max(1, ixpeak - nxCorr / 2)
                      ix1 = min(lenPiece(ixPiece,1), ix0 + nxCorr - 1)
                      iy0 = max(1, iypeak - nyCorr / 2)
                      iy1 = min(lenPiece(iyPiece,2), iy0 + nyCorr - 1)
                      iz0 = max(1, izpeak - nzCorr / 2)
                      iz1 = min(lenPiece(izPiece,3), iz0 + nzCorr - 1)
                      call findPeakCenter(array, lenPiece(ixPiece,1),
     &                  lenPiece(iyPiece,2), lenPiece(izPiece,3), ix0,ix1,
     &                    iy0,iy1,iz0,iz1, polarity, dxadj, dyadj, dzadj)
                      xpeak = (ix1 + ix0 - 1) / 2. + dxadj
                      ypeak = (iy1 + iy0 - 1) / 2. + dyadj
                      zpeak = (iz1 + iz0 - 1) / 2. + dzadj
                    endif
                    call addPeakToSum(array(maxVol+1), nxCorr,
     &                  nyCorr, nzCorr, array, lenPiece(ixPiece,1),
     &                  lenPiece(iyPiece,2),lenPiece(izPiece,3),xpeak,
     &                  ypeak, zpeak)
                  else
                    call find_best_corr(array(maxVol+1), nxCorr,
     &                  nyCorr, nzCorr, array, lenPiece(ixPiece,1),
     &                  lenPiece(iyPiece,2), lenPiece(izPiece,3), ixStart,
     &                  ixEnd,iyStart,iyEnd, izStart,izEnd, xpeak,
     &                  ypeak, zpeak, maxShift, found, peakCorr)
                    if (found) then
                      call addToSortedList(indCorr, corrVal, numCorrs,
     &                    maxPeaks, peakRelMin, peakCorr, index)
                      if (index .gt. 0) then
                        corrPos(1, index) = xpeak + ind0(ixPiece,1) +sumXoffset
                        corrPos(2, index) = ypeak + ind0(iyPiece,2) +sumYoffset
                        corrPos(3, index) = zpeak + ind0(izPiece,3) +sumZoffset
c                        write(*,'(i6,f8.4,i3,f10.5,6f7.1)') i,peakVal(ix),
c     &                      peakCorr*1.e-9,(peakPos(ix1,ix),ix1=1,3),
c     &                      (corrPos(ix1,index),ix1=1,3)
                      endif
                    endif
                  endif
                endif
              enddo
            enddo      
          enddo      
        enddo      
        if (mod(loopCorr, 2) .eq. 1) then
          call findPeakCenter(array(maxVol+1), nxCorr, nyCorr, nzCorr,
     &        1,nxCorr,1,nyCorr,1,nzCorr,
     &        polarity, sumXoffset, sumYoffset, sumZoffset)
          if (iVerbose .gt. 0) print *,'Offsets:',sumXoffset, sumYoffset,
     &        sumZoffset
c           do iz = 1,nzCorr
c           do iy = 1,nyCorr
c           write(*,'(2i3,(9i8))')iy,iz,(nint(array(maxVol + ix + (iy - 1) *
c     &           nxCorr + (iz - 1) * nxCorr * nyCorr)), ix =1, nxCorr)
c           enddo
c           enddo
          call meanzero(array(maxVol+1),nxCorr,nxCorr, nyCorr * nzCorr)
          if (iVerbose .gt. 0) call volwrite(17, 'volsum.st', array(maxVol+1),
     &        nxCorr, nyCorr, nzCorr)
        endif
      enddo
c       
      print *,numCorrs,' peaks found by correlation'
      call cleanSortedList(indCorr, corrVal, corrPos, numCorrs, distMin,
     &    cleanBoth, peakRelMin)
      print *,numCorrs,' peaks left after eliminating close points'
      peakCorr = corrVal(indCorr(1))
      do i = 1, numCorrs
        corrVal(indCorr(i)) = corrVal(indCorr(i)) / peakCorr
        peakVal(i) = corrVal(indCorr(i))
        if (iVerbose .gt. 1) write(*,'(f8.4,3f8.1)')corrVal(indCorr(i)),
     &      (corrPos(ix,indCorr(i)), ix = 1, 3)
      enddo
c
      call flush(6)
      ierr = findHistogramDip(peakVal, numCorrs, minGuess, histo, limHisto,
     &    0., 1., histDip, peakBelow, peakAbove, 0)
      blackThresh = histDip
      if (storeThresh .le. 0.) then
        if (ierr .ne. 0) call exitError('NO DIP FOUND IN HISTOGRAM OF '//
     &      'CORRELATION PEAKS; ENTER -store WITH POSITIVE VALUE TO PROCEED')
        call findValueInList(peakVal, numCorrs, histDip, numLook)
        print *,numLook,' peaks are above the histogram dip'
        if (storeThresh .lt. 0) then
          numPeaks = max(1, min(numCorrs, nint(-storeThresh * numLook)))
          print *,'Storing',numPeaks,' peaks in model'
        else
          call avgsd(peakVal, numLook, aboveAvg, aboveSD, peakBelow)
          peakBelow = aboveAvg - 5. * aboveSD
          call findValueInList(peakVal, numCorrs, peakBelow, numPeaks)
          numPeaks = max(numLook, min(2 * numLook, numPeaks))
          write(*,'(a,i7,a,f7.4)')'Storing an additional',numPeaks - numLook,
     &        ' peaks in model down to a value of', peakBelow
        endif
        
      else
        call findValueInList(peakVal, numCorrs, min(1., storeThresh), numPeaks)
        if (ierr.ne. 0) blackThresh = 0
        write(*,'(a,i7,a,f7.4)')'Storing',numPeaks,
     &      ' peaks in model above threshold of',storeThresh
      endif

      call writePeakModel(modelFile, indCorr, corrVal, corrPos,
     &    numPeaks, blackThresh, nxyz, delta, origin, radius)
      call exit(0)
97    call exitError('READING TILT FILE')
      end



c       DEFINEPIECES will determine how to divide an extent into overlapping
c       pieces.  Inputs:
c       nTotal = total extent
c       nOverlap = overlap between pieces
c       minSize = minimum size of pieces, or 0 to apply maximum size instead
c       maxSize = maximum size of pieces ( if minSize is 0)
c       maxPieces = maximum number of pieces allowed
c       Outputs:
c       nPieces = number of pieces
c       lenPiece = array with lengths of pieces
c       ind0, ind1 = arrays of starting, ending indices numbered from zero
c       ierr = 1 if too many pieces for arrays
c       
      subroutine definePieces(nTotal, nOverlap, minSize, maxSize, maxPieces,
     &    nPieces, lenPiece, ind0, ind1, ierr)
      implicit none
      integer*4 nTotal, nOverlap, minSize, maxSize, maxPieces, nPieces
      integer*4 lenPiece(*), ind0(*), ind1(*), isize, i, ierr, irem
c       
c       If a minimum size is defined, find the biggest number of pieces that
c       divide into sizes bigger than the minimum
c       
      ierr = 1
      if (minSize .gt. 0) then
        isize = minSize + 1
        nPieces = 0
        do while (nPieces .lt. maxPieces .and. isize .ge. minSize)
          nPieces = nPieces + 1
          isize = (nTotal + (nPieces - 1) * nOverlap) / Npieces
        enddo
        if (isize .ge. minSize) return
        if (nPieces .gt. 1) nPieces = nPieces - 1
      else
c         
c         Otherwise just compute the number from the maximum size
c         
        nPieces = (nTotal - maxSize) / (maxSize - nOverlap) + 1
        if (nPieces .le. 0) nPieces = 1
        isize = (nTotal + (nPieces - 1) * nOverlap + nPieces - 1) / Npieces
        if (isize .gt. maxSize) nPieces = nPieces + 1
        if (nPieces .gt. maxPieces) return
      endif
c       
c       get basic size and remainder to distribute, loop on pieces
c       
      isize = (nTotal + (nPieces - 1) * nOverlap) / Npieces
      irem = mod(nTotal + (nPieces - 1) * nOverlap, Npieces)
      ind0(1) = 0
      do i = 1, nPieces
        lenPiece(i) = isize
        if (irem .ge. i) lenPiece(i) = lenPiece(i) + 1
        if (i .gt. 1) ind0(i) = ind0(i - 1) + lenPiece(i - 1) - nOverlap
        ind1(i) = ind0(i) + lenPiece(i) - 1
      enddo
      ierr = 0
      return
      end

c       LOADVOL loads a subset of the volume from unit IUNIT, into ARRAY
c       assuming dimensions of NXDIM by NYDIM, from index coordinates
c       IX0, IX1, IY0, IY1, IZ0, IZ1.
c       
      subroutine loadvol(iunit,array,nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1)
      implicit none
      integer*4 nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1,iunit,indz,iz
      real*4 array(nxdim,nydim,*)
c       
c       print *,iunit,nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1
      indz=0
      do iz=iz0,iz1
        indz=indz+1
        call imposn(iunit,iz,0)
        call irdpas(iunit,array(1,1,indz),nxdim,nydim,ix0,ix1,iy0,iy1,*99)
      enddo
      return
99    call exitError('READING IMAGE FILE')
      end


c       AddToSortedList maintains a list of VALUES with an ordering in INDEX
c       numVals is the number currently in the list
c       maxVals is the maximum number allowed
c       peakRelMin is a threshold for adding a value relative to current 
c       maximum
c       valNew is the new value to add
c       newIndex is returned with the index in VALUES at which it was placed,
c       or 0 if the value is too small to fit on the list
c       
      subroutine addToSortedList(index, values,numVals, maxVals, peakRelMin,
     &    valNew, newIndex)
      implicit none
      integer*4 index(*), numVals, maxVals, newIndex, i
      real*4 values(*), valNew, peakRelMin
      integer*4 less, more, itest, newOrder
c       
c       quick test for full list and less than the last item on it
c       And for value less than threshold for storing
      newIndex = 0
      if (numVals .eq. maxVals) then
        if (values(index(maxVals)) .ge. valNew) return
      endif
      if (numVals .gt. 0) then
        if (valNew .lt. peakRelMin * values(index(1))) return
      endif
c       
c       Handle simple cases of inserting at the front or end of the list
c       
      if (numVals .eq. 0) then
        newOrder = 1
      else if (valNew .ge. values(index(1))) then
        newOrder = 1
      else if (values(index(numVals)) .ge. valNew) then
        newOrder = numVals + 1
      else
c         
c         Otherwise search for position.  Set up index of one more and one
c         less than the value.  Divide the interval in two and revise less
c         or more until there is no longer an interval between them
c         
        more = 1
        less = numVals
        do while (less - more .gt. 1)
          itest = (less + more) / 2
          if (values(index(itest)) .eq. valNew) then
            more = itest
            less = itest
          else if (values(index(itest)) .lt. valNew) then
            less = itest
          else
            more = itest
          endif
        enddo
        newOrder = less
      endif
c       
c       If space exists, position is at end of list; otherwise it is the
c       position occupied by the one being bumped off
c       
      if (numVals .lt. maxVals) then
        newIndex = numVals + 1
        numVals = numVals + 1
      else
        newIndex = index(maxVals)
      endif
      values(newIndex) = valNew
c       
c       shift index array up if necessary
c       
      do i = numVals, newOrder + 1, -1
        index(i) = index(i - 1)
      enddo
      index(newOrder) = newIndex
c       print *,'Inserted ',valNew,' at',newOrder,', index',newIndex
c       write(*,'(4(2i5,f9.0))')(i, index(i), values(index(i)), i =1, numVals)
      return
      end

c       Cleans the sorted list by eliminating ones that are too close
c
      subroutine cleanSortedList(index, peakVal, peakPos, numVals, distMin,
     &    cleanBoth, peakRelMin)
      implicit none
      integer*4 index(*), peakVal(*), numVals
      real*4 peakPos(3,*), distMin, peakRelMin, thresh
      integer*4 i,j,indi,indj
      real*4 xx,yy,zz,dx,dy,dz,sqrMin
      logical cleanBoth, knockout
c       
      sqrMin = distMin**2
      thresh = peakRelMin * peakVal(index(1))
c       
c       Scan from strongest down, knocking out peaks if point is too close
c       
      do j = 1, numVals - 1
        indj = index(j)
        knockout = .false.
        if (indj .gt. 0) then
          if (peakVal(indj) .lt. thresh) then
            indj = 0
            knockout = .true.
          endif
        endif
        if (indj .gt. 0) then
          xx = peakPos(1,indj)
          yy = peakPos(2,indj)
          zz = peakPos(3,indj)
          do i = j + 1, numVals
            indi = index(i)
            if (indi .gt. 0) then
              dx = xx - peakPos(1,indi)
              if (abs(dx) .lt. distmin) then
                dy = yy - peakPos(2,indi)
                if (abs(dy) .lt. distMin) then
                  dz = zz - peakPos(3,indi)
                  if (abs(dz) .lt. distMin .and.
     &                dx**2 + dy**2 + dz**2 .lt. sqrMin) then
                    index(i) = 0
                    knockout = .true.
                  endif
                endif
              endif
            endif
          enddo
        endif
        if (knockout .and. cleanBoth) index(j) = 0
      enddo
c       
c       repack index
c       
      j = 0
      do i = 1, numVals
        if (index(i) .gt. 0) then
          j = j + 1
          index(j) = index(i)
        endif
      enddo
      numVals = j
      return
      end


c       Finds the limits for analyzing one chunk of the tomogram
c
      subroutine getAnalysisLimits(ind0, ind1, nTotal, nOverlap, ncorr,
     &    iStart, iEnd)
      implicit none
      integer*4 ind0, ind1, nTotal, nOverlap, ncorr, iStart, iEnd
      if (ind0 .gt. 0) then
        iStart = nOverlap / 2
      else
        iStart = 2 + ncorr / 2
      endif
      if (ind1 .lt. nTotal - 1) then
        iEnd = ind1 + 1 - ind0 - nOverlap / 2
      else
        iEnd = ind1 - ind0 - ncorr / 2
      endif
      return
      end


c       findPixelSumPeaks will find peaks within a subvolume consisting of
c       rolling sums over cubes NSUM on a side
c       ARRAY contains the volume, dimensioned NX x nY x NZ
c       PLANES is an array NX x NY x 3 for storing sums of pixels
c       ixStart, ixEnd are the starting indixes in X at which to look for
c       peaks, similarly for iyStart, iyEnd and izStart, izEnd.
c       ixOffset, iyOffset, izOffset are the offsets of the subvolume in the
c       whole volume
c       POLARITY is -1 to look for dark peaks
c       indPeak in an array with indexes to sorted peak values in peakVal
c       peakPos will receive the X, y, Z coordinates of the peaks
c       maxPeaks is the maximum number of peaks to store
c       numPeaks is the number currently stored
c       peakRelMin is a threshold for storing peaks relative to current max
c       
      subroutine findPixelSumPeaks(array, planes, nx, ny, nz, ixStart, ixEnd,
     &    iyStart, iyEnd, izStart, izEnd, ixOffset, iyOffset, izOffset,
     &    nsum, polarity, nxCorr, nyCorr, nzCorr, indPeak, peakVal, peakPos,
     &    maxPeaks, numPeaks, peakRelMin)
      implicit none
      integer*4 nx, ny, nz, ixStart, ixEnd, iyStart, iyEnd, izStart, izEnd
      real*4 array(nx, ny, nz), planes(nx, ny, 3), peakVal(*), peakPos(3, *)
      integer*4 indPeak(*), nxCorr, nyCorr, nzCorr
      integer*4 ixOffset, iyOffset, izOffset, nsum, maxPeaks, numPeaks
      real*4 polarity, cen, posShift, sum, sumScale, peakRelMin
      integer*4 ix, iy, iz, jx, jy, jz, nextPlane, nBefore, nAfter, ip1, ip2
      integer*4 ip3, index, ix0, ix1, iy0, iy1, iz0, iz1
      
c       
      nextPlane = 1
      nBefore = (nsum - 1) / 2
      nAfter = nsum / 2
      posShift = 0.5 * (nAfter - nBefore - 1)
      sumScale = polarity / nsum**3
c       
c       Loop on the Z planes with extended limits
c       
      do iz = izStart - 1, izEnd + 1
c         
c         first compute the sums on the next plane
c         
        do iy = iyStart - 1, iyEnd + 1
          do ix = ixStart - 1, ixEnd + 1
            sum = 0.
            do jz = iz - nBefore, iz + nAfter
              do jy = iy - nBefore, iy + nAfter
                do jx = ix - nBefore, ix + nAfter
                  sum = sum + array(jx, jy, jz)
                enddo
              enddo
            enddo
            planes(ix, iy, nextPlane) = sum * sumScale
          enddo
        enddo
c         
c         If at least 3 planes have been computed, now search for peaks
c         
        if (iz .gt. izStart) then
          ip1 = mod(nextPlane, 3) + 1
          ip2 = mod(nextPlane + 1, 3) + 1
          ip3 = nextPlane
          do iy = iyStart, iyEnd
            do ix = ixStart, ixEnd
              cen = planes(ix, iy, ip2)
c               
c               Test for whether greater than all diagonal and ones ahead
c               in X, Y, or Z, and >= ones behind in X, Y, or Z
c               
              if (cen.ge.planes(ix-1,iy,ip2) .and. cen.gt.planes(ix+1,iy,ip2)
     &            .and. cen.ge.planes(ix,iy-1,ip2) .and.
     &            cen.gt.planes(ix,iy+1,ip2) .and. cen.ge.planes(ix,iy,ip1)
     &            .and. cen.gt.planes(ix,iy,ip3) .and.
     &            cen.gt.planes(ix,iy+1,ip1) .and. cen.ge.planes(ix,iy-1,ip1)
     &            .and. cen.ge.planes(ix-1,iy,ip1) .and.
     &            cen.ge.planes(ix+1,iy,ip1) .and. cen.ge.planes(ix+1,iy,ip3)
     &            .and. cen.ge.planes(ix-1,iy,ip3) .and.
     &            cen.gt.planes(ix,iy+1,ip3) .and. cen.ge.planes(ix,iy-1,ip3)
     &            .and.cen.ge.planes(ix-1,iy-1,ip2) .and.
     &            cen.ge.planes(ix+1,iy-1,ip2) .and.
     &            cen.ge.planes(ix+1,iy+1,ip2) .and.
     &            cen.ge.planes(ix-1,iy+1,ip2) .and.
     &            cen.ge.planes(ix-1,iy-1,ip1) .and.
     &            cen.ge.planes(ix+1,iy-1,ip1) .and.
     &            cen.ge.planes(ix+1,iy+1,ip1) .and.
     &            cen.ge.planes(ix-1,iy+1,ip1) .and.
     &            cen.ge.planes(ix-1,iy-1,ip3) .and.
     &            cen.ge.planes(ix+1,iy-1,ip3) .and.
     &            cen.ge.planes(ix+1,iy+1,ip3) .and.
     &            cen.ge.planes(ix-1,iy+1,ip3)) then
c                 
c                 Got a peak; try to add it to the list
c                 
                ix0 = ix - nxCorr / 2
                ix1 = ix0 + nxCorr - 1
                iy0 = iy - nyCorr / 2
                iy1 = iy0 + nyCorr - 1
                iz0 = iz - nzCorr / 2 - 1
                iz1 = iz0 + nzCorr - 1
c                print *,cen,ix0,ix1,iy0,iy1,iz0,iz1,nx,ny,nz
                if (ix0 .ge. 1 .and. ix1 .le. nx .and. iy0 .ge. 1 .and.
     &              iy1 .le. ny .and. iz0 .ge. 1 .and. iz1 .le. nz) then
                  call integratePeak(array, nx, ny, nz, ix0, ix1, iy0, iy1,
     &                iz0, iz1, polarity, cen)
c                  print *,cen
                  if (cen .gt. 0) then
                    call addToSortedList(indPeak, peakVal, numPeaks, maxPeaks,
     &                  peakRelMin, cen, index)
                    if (index .gt. 0) then
                      peakPos(1, index) = ix + ixOffset + posShift
                      peakPos(2, index) = iy + iyOffset + posShift
                      peakPos(3, index) = iz + izOffset + posShift - 1
                    endif
                  endif
                endif
              endif
            enddo
          enddo
          
        endif
        nextPlane = mod(nextPlane, 3) + 1
      enddo
      return
      end

c       addPeakToSum adds one peak located at dxadj, dyadj, dzadj in the
c       volume brray (size nxb, nyb, nzb) to the sum in array (size nxa, nya,
c       nza)
      subroutine addPeakToSum(array,nxa,nya,nza, brray,nxb,nyb,nzb,
     &    dxadj, dyadj, dzadj)
      implicit none
      integer*4 nxa,nxb,nya,nyb,nza,nzb
      real*4 array(nxa,nya,nza),brray(nxb,nyb,nzb)
      real*4 dxadj,dyadj,dzadj,dx,dy,dz,d11,d12,d21,d22
      integer*4 ix, iy, iz, idx, idy, idz,ixp,iyp,izp,ixpp1,iypp1,izpp1
      idx = int(dxadj) - nxa / 2
      dx = dxadj - int(dxadj)
      idy = int(dyadj) - nya / 2
      dy = dyadj - int(dyadj)
      idz = int(dzadj) - nza / 2
      dz = dzadj - int(dzadj)
      d11 = (1. - dx) * (1. - dy)
      d12 = (1. - dx) * dy
      d21 = dx * (1. - dy)
      d22 = dx * dy
      do iz = 1, nza
        izp = max(1, iz + idz)
        izpp1 = min(izp + 1, nzb)
        do iy = 1, nya
          iyp = max(1, iy + idy)
          iypp1 = min(iyp + 1, nyb)
          do ix = 1, nxa
            ixp = max(1, ix + idx)
            ixpp1 = min(ixp + 1, nxb)
            array(ix, iy, iz) = array(ix, iy, iz) +
     &          (1. - dz) * (d11 * brray(ixp, iyp, izp)
     &          + d12 * brray(ixp, iypp1, izp)
     &          + d21 * brray(ixpp1, iyp, izp)
     &          + d22 * brray(ixpp1, iypp1, izp)) +
     &          dz * (d11 * brray(ixp, iyp, izpp1)
     &          + d12 * brray(ixp, iypp1, izpp1)
     &          + d21 * brray(ixpp1, iyp, izpp1)
     &          + d22 * brray(ixpp1, iypp1, izpp1))
          enddo
        enddo
      enddo
      return 
      end


c       Finds the centroid of the pixels in box above the background at the
c       edges of the box
c       
      subroutine findPeakCenter(array,nxa,nya,nza, ix0,ix1,iy0,iy1,iz0,iz1,
     &    polarity, dxadj, dyadj, dzadj)
      implicit none
      integer*4 nxa,nya,nza,ix0,ix1,iy0,iy1,iz0,iz1
      real*4 array(nxa,nya,nza)
      real*4 polarity,dxadj,dyadj,dzadj,edge,xsum,ysum,zsum,wsum,diff
      integer*4 ix, iy, iz
c       
      call peakEdgeMean(array,nxa,nya,nza, ix0,ix1,iy0,iy1,iz0,iz1,edge)
c       
c       Get weighted sum of pixel indexes
      xsum = 0.
      ysum = 0.
      zsum = 0.
      wsum = 0.
      do iz = iz0+1, iz1-1
        do iy = iy0+1, iy1-1
          do ix = ix0+1, ix1-1
            diff = polarity * (array(ix,iy,iz) - edge)
            if (diff .gt. 0.) then
              wsum = wsum + diff
              xsum = xsum + diff * (ix - ix0)
              ysum = ysum + diff * (iy - iy0)
              zsum = zsum + diff * (iz - iz0)
            endif
          enddo
        enddo
      enddo
c       
c       Get offset from center
      dxadj = xsum / wsum - (ix1 - ix0) / 2.
      dyadj = ysum / wsum - (iy1 - iy0) / 2.
      dzadj = zsum / wsum - (iz1 - iz0) / 2.
      return
      end

c       Finds the integral of a peak relative to background at the edges
c       
      subroutine integratePeak(array, nx, ny, nz, ix0, ix1, iy0, iy1, iz0, iz1,
     &    polarity, peak)
      implicit none
      integer*4 nx, ny, nz, ix0, ix1, iy0, iy1, iz0, iz1, ix, iy, iz
      real*4 array(nx,ny,nz), polarity, peak, edge
c       
      call peakEdgeMean(array, nx, ny, nz, ix0, ix1, iy0, iy1, iz0, iz1, edge)
      peak = 0.
      do iz = iz0+1, iz1-1
        do iy = iy0+1,iy1-1
          do iX = ix0+1, ix1-1
            peak = peak + array(ix,iy,iz) - edge
          enddo
        enddo
      enddo
      peak = peak * polarity
      return
      end


c       Finds the mean along the walls of a box
c
      subroutine peakEdgeMean(array, nx, ny, nz, ix0, ix1, iy0, iy1, iz0, iz1,
     &    edge)
      implicit none
      integer*4 nx, ny, nz, ix0, ix1, iy0, iy1, iz0, iz1, ix, iy, iz
      real*4 array(nx,ny,nz), edgesum, edge
      edgesum = 0
      do iy = iy0, iy1
        do ix = ix0, ix1
          edgesum = edgesum + array(ix,iy,iz0) + array(ix,iy,iz1)
        enddo
      enddo
      do iz = iz0+1, iz1-1
        do ix = ix0, ix1
          edgesum = edgesum + array(ix,iy0,iz) + array(ix,iy1,iz)
        enddo
      enddo
      do iz = iz0+1, iz1-1
        do iy = iy0+1,iy1-1
          edgesum = edgesum + array(ix0,iy,iz) + array(ix1,iy,iz)
        enddo
      enddo
      edge = edgesum / (2 * ((ix1+1-ix0) * (iy1+1-iy0) +
     &    (ix1+1-ix0) * (iz1-1-iz0) + (iy1-1-iy0) * (iz1-1-iz0)))
      return
      end

c       FIND_BEST_CORR will search for the location in the volume BRRAY
c       with the highest correlation to the 
c       volume in ARRAY.  ARRAY is dimensioned to NXA by NYA
c       by NZA; BRRAY is dimensioned to NXB by NYB by NZB.
c       IXSTART, IXEND, IYSTART, IYEND, IZSTART, IZEND are the limits for
c       valid center locations (array coordinates) in BRRAY.
c       DXADJ, DYADJ, DZADJ
c       should contain starting location upon entry, and will return the
c       location with the best correlation.  MAXSHIFT specifies the maximum
c       shift that is allowed.  FOUND is returned as TRUE if a correlation
c       peak is found within the maximum shift.  The maximum cross-product is
c       returned in PEAKCORR
c       
      subroutine find_best_corr(array,nxa,nya,nza, brray,nxb,nyb,nzb,
     &    ixStart,ixEnd,iyStart,iyEnd, izStart,izEnd,dxadj, dyadj,
     &    dzadj,maxshift, found, peakCorr)
      implicit none

      integer*4 nxa,nxb,nya,nyb,nza,nzb
      real*4 array(nxa,nya,nza),brray(nxb,nyb,nzb), peakCorr
      real*8 corrs(-1:1,-1:1,-1:1),corrtmp(-2:2,-2:2,-2:2)
      real*8 corrmax
      logical done(-1:1,-1:1,-1:1),donetmp(-2:2,-2:2,-2:2)
      integer*4 idyseq(9)/0,-1,1,0,0,-1,1,-1,1/
      integer*4 idzseq(9)/0,0,0,1,-1,-1,-1,1,1/
      logical found
      integer*4 maxshift,ixStart,ixEnd,iyStart,iyEnd, izStart,izEnd

      real*4 dxadj,dyadj,dzadj
c       
      integer*4 idxglb,idyglb,idzglb,ix,iy,iz,iseq,idy,idz
      integer*4 idycor,idzcor,idxcor,minseq
      integer*4 indmax,idxmax,idymax,idzmax
      real*4 cx,y1,y2,y3,cy,cz
      real*8 parabolicFitPosition
c       
c       Minimum # of rows to do in sequence before shifting center
      minseq = 5
c       
c       get global displacement of b
c       
c       print *,'findbest',nxa,nya,nza,
c     &    nxb,nyb,nzb,ixStart,ixEnd,iyStart,iyEnd, izStart,izEnd,
c     &    dxadj, dyadj,dzadj,maxshift
      idxglb=nint(dxadj)
      idyglb=nint(dyadj)
      idzglb=nint(dzadj)
c      print *,'starting',idxglb,idyglb,idzglb
c       
c       clear flags for existence of corr
c       
      do iz=-1,1
        do iy=-1,1
          do ix=-1,1
            done(ix,iy,iz)=.false.
          enddo
        enddo
      enddo

      corrmax=-1.e30
      iseq=1
      do while(iseq.le.9)
        idy=idyseq(iseq)
        idz=idzseq(iseq)
c         print *,iseq,idy,idz
        if(.not.(done(-1,idy,idz).and.done(0,idy,idz).and.
     &      done(1,idy,idz)))then
c           
c           if the whole row does not exist, do the correlations
c           limit the extent if b is displaced and near an edge
c           
          idxcor = idxglb - nxa / 2
          idycor=idyglb+idy - nya / 2
          idzcor=idzglb+idz - nza / 2
          call threecorrs(array,nxa,nya,brray,nxb,nyb,0,nxa-1,
     &        0,nya-1,0,nza-1,idxcor,idycor,idzcor,
     &        corrs(-1,idy,idz),corrs(0,idy,idz),corrs(1,idy,idz))
          done(-1,idy,idz)=.true.
          done(0,idy,idz)=.true.
          done(1,idy,idz)=.true.
        endif
        if(corrs(0,idy,idz).gt.corrs(-1,idy,idz).and.
     &      corrs(0,idy,idz).gt.corrs(1,idy,idz))then
          indmax=0
        elseif(corrs(-1,idy,idz).gt.corrs(0,idy,idz).and.
     &        corrs(-1,idy,idz).gt.corrs(1,idy,idz))then
          indmax=-1
        else
          indmax=1
        endif
        if(corrs(indmax,idy,idz).gt.corrmax)then
c           print *,'New max',corrmax
          corrmax=corrs(indmax,idy,idz)
          idxmax = indmax
          idymax = idy
          idzmax = idz
        endif

        if (iseq .ge. minseq .and.
     &      (idxmax .ne. 0 .or. idymax .ne. 0 .or. idzmax .ne. 0)) then
c           
c           if there is a new maximum, after a minimum number of rows has
c           been done, shift the done flags and the existing
c           correlations, and reset the sequence
c           
          idxglb=idxglb+idxmax
          idyglb=idyglb+idymax
          idzglb=idzglb+idzmax
c           print *,'moving by',idxmax,idymax,idzmax
c           
c           but if beyond the limit, return failure
c           
          if (max(abs(idxglb-dxadj),abs(idyglb-dyadj),abs(idzglb-dzadj)).gt.
     &        maxshift .or. idxglb .lt. ixStart .or. idxglb .gt. ixEnd .or.
     &        idyglb .lt. iyStart .or. idyglb .gt. iyEnd .or.
     &        idzglb .lt. izStart .or. idzglb .gt. izEnd)then
            found=.false.
c             print *,'outside'
            return
          endif
          do iz=-1,1
            do iy=-1,1
              do ix=-1,1
                donetmp(ix,iy,iz)=.false.
              enddo
            enddo
          enddo
          do iz=-1,1
            do iy=-1,1
              do ix=-1,1
                donetmp(ix-indmax,iy-idy,iz-idz)=done(ix,iy,iz)
                corrtmp(ix-indmax,iy-idy,iz-idz)=corrs(ix,iy,iz)
              enddo
            enddo
          enddo
          do iz=-1,1
            do iy=-1,1
              do ix=-1,1
                done(ix,iy,iz)=donetmp(ix,iy,iz)
                corrs(ix,iy,iz)=corrtmp(ix,iy,iz)
              enddo
            enddo
          enddo
          iseq=0
          idxmax = 0
          idymax = 0
          idzmax = 0
        endif
        iseq=iseq+1
      enddo
c       
c       do independent parabolic fits in 3 dimensions
c       
      y1=corrs(-1,0,0)
      y2=corrs(0,0,0)
      y3=corrs(1,0,0)
      cx = parabolicFitPosition(y1, y2, y3)
c      print *,'X:',y1, y2, y3, cx
      y1=corrs(0,-1,0)
      y3=corrs(0,1,0)
      cy = parabolicFitPosition(y1, y2, y3)
c      print *,'Y:',y1, y2, y3, cy
      y1=corrs(0,0,-1)
      y3=corrs(0,0,1)
      cz = parabolicFitPosition(y1, y2, y3)
c      print *,'Z:',y1, y2, y3, cz
c       
      dxadj=idxglb+cx
      dyadj=idyglb+cy
      dzadj=idzglb+cz
      peakCorr = y2
      found=.true.
c       print *,'returning a peak', peakCorr, dxadj, dyadj, dzadj
      return
      end


c       THREECORRS computes three correlations between volumes in ARRAY
c       and BRRAY.  The volume in ARRAY is dimensioned to NXA by NYA, that
c       in BRRAY is dimensioned to NXB by NYB.  The starting and ending
c       index coordinates (numbered from 0) in ARRAY over which the 
c       correlations are to be computed are IX0, IX1, IY0, IY1, IZ0, IZ1.
c       The shift between coordinates in ARRAY and coordinates in B is given
c       by IDX, IDY, IDZ.  The three correlations are returned in CORR1 (for
c       IDX-1), CORR2 (for IDX), and CORR3 (for IDX+1).
c       
      subroutine threecorrs(array,nxa,nya,brray,nxb,nyb,ix0,ix1,
     &    iy0,iy1,iz0,iz1,idx,idy,idz,corr1,corr2,corr3)
      implicit none
      real*4 array(*),brray(*)
c       real*4 first, prev
      real*8 sum1,sum2,sum3,corr1,corr2,corr3,bsumt,bsumsqt
      real*8 asum, bsum1, bsum2, bsum3, asumsq, bsumsq1, bsumsq2, bsumsq3
      integer*4 ix0,ix1,iy0,iy1, iz0,iz1,idx,idy,idz,nxa,nxb,nya,nyb
      integer*4 iz,izb,iy,iyb,indbasea,inddelb,ix,ixb,nsum
      sum1=0.
      sum2=0.
      sum3=0.
      asum = 0.
      bsum1 = 0.
      bsum2 = 0.
      bsum3 = 0.
      asumsq = 0.
      bsumsq1 = 0.
      bsumsq2 = 0.
      bsumsq3 = 0.
      do iz=iz0,iz1
        izb=iz+idz
        do iy=iy0,iy1
          iyb=iy+idy
          indbasea=1+iy*nxa+iz*nxa*nya
          inddelb=1+iyb*nxb+izb*nxb*nyb+idx-indbasea
          bsumt = 0.
          bsumsqt = 0.

          do ix=indbasea+ix0,indbasea+ix1
            ixb=ix+inddelb
            sum1=sum1+array(ix)*brray(ixb-1)
            sum2=sum2+array(ix)*brray(ixb)
            sum3=sum3+array(ix)*brray(ixb+1)
c             
c             All this computes correlation coefficients and is too precious
c             to delete, but not right for featureless particles
c             In fact this routine is the same as in corrsearch3d without this
c             asum = asum + array(ix)
c             asumsq = asumsq + array(ix)**2
c             bsumt = bsumt + brray(ixb)
c             bsumsqt = bsumsqt + brray(ixb)**2
          enddo
c           bsum2 = bsum2 + bsumt
c           bsumsq2 = bsumsq2 + bsumsqt
c           first = brray(indbasea+ix0+inddelb)
c           prev = brray(indbasea+ix0+inddelb - 1)
c           bsum1 = bsum1 + bsumt + prev - brray(ixb)
c           bsum3 = bsum3 + bsumt - first + brray(ixb+1)
c           bsumsq1 = bsumsq1 + bsumsqt + prev**2 - brray(ixb)**2
c           bsumsq3 = bsumsq3 + bsumsqt - first**2 + brray(ixb+1)**2
        enddo
      enddo     
      nsum=(iz1+1-iz0)*(iy1+1-iy0)*(ix1+1-ix0)
c       corr1 = (nsum * sum1 - asum * bsum1) /
c       &           sqrt((nsum * asumsq - asum**2) * (nsum * bsumsq1 - bsum1**2))
c       corr2 = (nsum * sum2 - asum * bsum2) /
c       &           sqrt((nsum * asumsq - asum**2) * (nsum * bsumsq2 - bsum2**2))
c       corr3 = (nsum * sum3 - asum * bsum3) /
c       &           sqrt((nsum * asumsq - asum**2) * (nsum * bsumsq3 - bsum3**2))
      corr1=sum1/nsum
      corr2=sum2/nsum
      corr3=sum3/nsum
c       print *,idx,idy,idz,corr1,corr2,corr3
      return
      end

      subroutine findValueInList(peakVal, numPeaks, findVal, index)
      implicit none
      real*4 peakVal(*), findVal
      integer*4 numPeaks, index, i
      i = 1
      do while (i .le. numPeaks .and. peakVal(i) .ge. findVal)
        i = i + 1
      enddo
      index = i - 1
      return
      end

c       Write a models with the given number of peaks to the file and set the
c       threshold if any
c
      subroutine writePeakModel(firstFile, indPeak, peakVal, peakPos,
     &    numPeaks, blackThresh, nxyz, delta, origin, radius)
      implicit none
      include 'model.inc'
      character*(*) firstFile
      integer*4 indPeak(*), numPeaks, iobj, i, j, ierr, nxyz(3)
      real*4 peakVal(*), peakPos(3,*), delta(*), origin(*), radius, blackThresh
      real*4  peak, peakmin, peakmax
      integer*4 ind(3)/1,2,3/
      integer*4 putContValue, putImodFlag, putValBlackWhite, putImageRef
      integer*4 putImodMaxes

      if (firstFile .eq. ' ') return

      n_point = numPeaks
      iobj = 0
      max_mod_obj = numPeaks
      call newimod()
      peakmin = peakVal(indPeak(numPeaks))
      peakmax = peakVal(indPeak(1))
      ierr = putimodflag(1, 2)
      ierr = putImodFlag(1, 7)
      call putscatsize(1, max(1, 1 + ceiling(radius)))
      ierr = putImodMaxes(nxyz(1), nxyz(2), nxyz(3))
      do i = 1, n_point
        peak = peakVal(indPeak(i))
c         
c         set up object if it is time for next one
c         
        iobj = i
        obj_color(2, iobj) = 255
        npt_in_obj(iobj) = 1
        ibase_obj(iobj) = i - 1

        do j = 1, 3
          p_coord(j, i) = peakPos(ind(j), indPeak(i))
        enddo
        ierr = putContValue(1, iobj, peak)
        object(i) = i
      enddo
      iobj = 0
      if (blackThresh .gt. 0) iobj = max(1, min(255,
     &    nint(255. * (blackThresh - peakmin) / peakmax - peakmin)))
      ierr = putValBlackWhite(1, iobj, 255)
      ierr = putImageRef(delta, origin)
      call scale_model(1)
      call write_wmod(firstFile)
      return
      end


c       Routine to write a volume in a contiguous array
c
      subroutine volwrite(iunit, filout,array,nx, ny, nz)
      implicit none
      character*(*) filout
      integer*4 iunit, nx, ny, nz, iz
      real*4 array(nx,ny,nz),tmin,tmax,tmean,dmin,dmax,dmean,title(20)
      integer*4 nxyz(3)
      real*4 cell(6)/0.,0.,1.,90.,90.,90./
      character*80 titlech
c       
      call imopen(iunit,filout,'NEW')
      write(titlech,3000)
3000  FORMAT('DEBUG VOLUME')
      nxyz(1)=nx
      nxyz(2)=ny
      nxyz(3)=nz
      cell(1)=nx
      cell(2)=ny
      cell(3)=nz
      call icrhdr(iunit,nxyz,nxyz,2,title,0)
      call ialcel(iunit,cell)
      dmean = 0.
      tmin = 1.e30
      tmax = -1.e30
      do iz = 1, nz
        call iclden(array(1,1,iz),nx,ny,1,nx,1,ny,tmin,tmax,tmean)
        call iwrsec(iunit,array(1,1,iz))
        dmin=min(tmin,dmin)
        dmax=max(tmax,dmax)
        dmean = dmean + tmean
      enddo
      call iwrhdrc(iunit,titlech,1,dmin,dmax,dmean / nz)
      call imclose(iunit)
      return
      end

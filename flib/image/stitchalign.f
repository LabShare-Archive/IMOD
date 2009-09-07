c       STITCHALIGN
c       
c       Aligns adjacent volumes for stitching together using patch vectors in 
c       the areas where they overlap.  It finds the best rotation for bringing
c       them onto a regular rectangular grid, then finds the best
c       transformation for aligning them.  The transformation can include
c       rotations around the three axes and magnification, in-plane stretch,
c       and thinning.  Then it finds a warping vector field for each volume to
c       resolve remaining disparities in their overlap zones.
c
c       $Id$
c       Log at end of file
c
      use stitchvars
      implicit none
      integer maxvar, maxsec, limoutgrid
      parameter (maxvar = 10*maxvols, maxsec = 1000, limoutgrid=500000)
      real*4 var(maxvar), grad(maxvar), hess(maxvar*(maxvar+3))
      real*4 rmat(3,3)
      integer*4 listAllZ(maxsec), listZdo(maxsec), ixPiece(maxvols)
      integer*4 iyPiece(maxvols), indSection(maxvols)
      integer*4 indXorY(2*maxvols)
      real*4 fitdxyz(3,maxvols), fullMat(3,3,maxvols)
      real*4 edgeShift(3, 2*maxvols), volShift(3, maxvols), fulldxyz(3,maxvols)
      integer*4 iPairLower(2*maxvols), iPairUpper(2*maxvols)
      integer*4 iPairEdge(2*maxvols), ioutGridStart(3,maxvols)
      integer*4 ioutGridEnd(3,maxvols), iposOut(3,limoutgrid)
      integer*4 numOutGrid(3,maxvols), ioutGridDelta(3,maxvols)
      real*4 edgeGeomVars(7,2*maxvols), vecOut(4,limoutgrid)

      real*4 facMetro, eps, xyRotation, angsum, xsum, ysum, angle, dx, dy
      integer*4 maxCycle, ifAlpha, ifBeta, ifGamma, nCycle, ierr, ifMaxSize
      integer*4 ifFindRot, numAllVols, numAllEdges, numSections, numAllZ,numZdo
      integer*4 i, j, iv, jv, ivar, indz, ix, iy, iz, izval, indVec, indlo, m
      integer*4 indhi, ixhi, iyhi, nsum, nxsum, nysum, nxmax, nymax, iexy
      integer*4 ixpstr, ixpend, iypstr, iypend, ivf, jvf, numPairs, iPair
      logical*4 findMag, findStretch, findThin, pairwise, allTogether
      real*4 cosrot, sinrot, dist, finalErr, gridPad, sampleFactor
      real*4 residCrit, outlierCrit
      real*8 dsum, dsumsq
      integer*4 kstr, kend, k, nvarsrch, mapAngles, numEdge, numVecOut, ndim
      integer*4 ixpos, iypos, maxNumVecZ
      character*160 infofile
      character*1024 listString,filename
      character*4 xoryStr
      real*4 atan2d, cosd, sind
      integer*4 lnblnk
      external stitchfunc

      integer*4 AdocRead, AdocGetNumberOfSections, AdocGetThreeIntegers
      integer*4 AdocGetSectionName, AdocGetThreeFloats, AdocGetString
      integer*4 AdocGetInteger, AdocSetKeyValue, AdocWrite, AdocAddSection
      integer*4 AdocSetThreeIntegers, AdocSetTwoIntegers, AdocSetFloat
      integer*4 AdocSetInteger
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoIntegers,PipGetThreeIntegers
      integer*4 PipGetString,PipGetFloat,PipGetFloatArray,PipgetTwoFloats
      integer*4 PipGetInOutFile,PipGetLogical
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  stitchalign
c
      integer numOptions
      parameter (numOptions = 17)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'info:InfoFile:FN:@zvalues:ZvaluesToDo:LI:@'//
     &    'rotation:RotationInXYplane:F:@size:SizeOfOutputFramesXandY:IP:@'//
     &    'mag:FindMagnification:B:@stretch:FindStretch:B:@'//
     &    'thinning:FindThinning:B:@angles:FindAngles:IT:@'//
     &    'metro:MetroFactor:F:@xrun:XRunStartEnd:IP:@yrun:YRunStartEnd:IP:@'//
     &    'residual:WarpFitResidualCriterion:F:@'//
     &    'outlier:OutlierFractionCriterion:F:@'//
     &    'spacing:VectorSpacingFactor:F:@all:AllTogether:B:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'

      ifAlpha = 1
      ifBeta = 1
      ifGamma = 1
      findMag = .false.
      findThin = .false.
      findStretch = .false.
      allTogether = .false.
      maxCycle = 1000
      eps = 0.000001
      facMetro = 0.24
      ixpstr = -100000
      ixpend = 100000
      iypstr = ixpstr
      iypend = ixpend
      dtor = 0.0174532
      sampleFactor = 0.7
      outlierCrit = 0.33
      residCrit = 100.
c	  
c	  Pip startup: set error, parse options, do help output
c
      call PipReadOrParseOptions(options, numOptions, 'stitchalign',
     &    'ERROR: STITCHALIGN - ', .false., 1, 1, 0, numOptArg,
     &    numNonOptArg)
c       
c       Get various options
      if (PipGetInOutFile('InfoFile', 1, ' ', infofile)
     &    .ne. 0) call exitError('NO SUPERMONTAGE INFO FILE SPECIFIED')
      ifFindRot = PipGetFloat('RotationInXYplane', xyRotation)
      ierr = PipGetLogical('FindMagnification', findMag)
      ierr = PipGetLogical('FindThinning', findThin)
      ierr = PipGetLogical('FindStretch', findStretch)
      ierr = PipGetThreeIntegers('FindAngles', ifAlpha, ifBeta, ifGamma)
      ierr = PipGetFloat('MetroFactor', facMetro)
      ierr = PipGetTwoIntegers('XRunStartEnd', ixpstr, ixpend)
      ierr = PipGetTwoIntegers('YRunStartEnd', iypstr, iypend)
      ierr = PipGetLogical('AllTogether', allTogether)
      ierr = PipGetFloat('VectorSpacingFactor', sampleFactor)
      ierr = PipGetFloat('WarpFitResidualCriterion', residCrit)
      ierr = PipGetFloat('OutlierFractionCriterion', outlierCrit)
      ifMaxSize = PipGetTwoIntegers('SizeOfOutputFramesXandY', nxout, nyout)
c       
c       Open the smi file and make a list of Z values there
      if (AdocRead(infofile) .lt. 0) call exitError
     &    ('READING THE SUPERMONTAGE INFO FILE')
      numAllVols = AdocGetNumberOfSections('Piece')
      if (numAllVols .lt. 2) call exitError(
     &    'THERE IS ONLY ONE VOLUME LISTED IN THE SMI FILE')
      numAllEdges = AdocGetNumberOfSections('Edge')
      numAllZ = 0
      do i = 1, numAllVols
        if (AdocGetThreeIntegers('Piece', i, 'Frame', ix, iy, iz).ne. 0)
     &      call exitError('GETTING FRAME NUMBER FOR A PIECE')
        indz = 0
        do j = 1, numAllZ
          if (iz .eq. listAllZ(j)) indz = j
        enddo
        if (indz .eq. 0) then
          numAllZ = numAllZ + 1
          listAllZ(numAllZ) = iz
          listZdo(numAllZ) = iz
        endif
      enddo
      numZdo = numAllZ
c       
c       Get optional list of Z's to do and check against all Z list
      if (PipGetString('ZvaluesToDo', listString) .eq. 0)
     &    call parselist(listString, listZdo, numZdo)
      do i = 1, numZdo
        indz = 0
        do j = 1, numAllZ
          if (listZdo(i) .eq. listAllZ(j)) indz = j
        enddo
        if (indz .eq. 0) call exitError(
     &      'THE LIST OF Z VALUES TO DO CONTAINS A NONEXISTENT VALUE')
      enddo
      call PipDone()
      call dopenHush(.true.)
c       
c       Loop on Z values to do
      do indz = 1, numZdo
        izval = listZdo(indz)
        if (numZdo .gt. 1) print *,'Doing section at Z =',izval
        do i = 1,2
          do j = 1,3
            intervals(i,j) = 0
            spacings(i,j) = 0.
          enddo
        enddo
c       
c         Load data about the volumes at this Z value
        numVols = 0
        do i = 1, numAllVols
          ierr = AdocGetThreeIntegers('Piece', i, 'Frame', ix, iy, iz)
          if (iz .eq. izval .and. ix .ge. ixpstr .and. ix .le. ixpend .and.
     &        iy .ge. iypstr .and. iy .le.iypend) then
            numVols = numVols + 1
            if (numVols .gt. maxvols) call exitError(
     &          'TOO MANY VOLUMES AT ONE Z VALUE FOR ARRAYS')
            ixPiece(numVols) = ix
            iyPiece(numVols) = iy
            indSection(numVols) = i
            if (AdocGetThreeIntegers('Piece', i, 'size', nxyzin(1, numVols),
     &          nxyzin(2, numVols), nxyzin(3, numVols)) .ne. 0) call exitError
     &          ('GETTING VOLUME SIZE FOR A PIECE')
            do j = 1, 2
              iVolUpper(j,numVols) = 0
              iVolLower(j,numVols) = 0
              indVector(j,numVols) = 0
            enddo
          endif
        enddo
c         
c         Now load the edges at this value and link them back to volumes
        numEdge = 0
        indVec = 1
        indG2Vbase = 1
        maxNumVecZ = 0
        do i = 1, numAllEdges
          if (AdocGetThreeIntegers('Edge', i, 'lower', ix, iy, iz).ne. 0)
     &        call exitError('GETTING LOWER FRAME NUMBER FOR AN EDGE')
          if (AdocGetString('Edge', i, 'XorY', xoryStr) .ne. 0)
     &        call exitError('GETTING XorY FOR AN EDGE')
          j = AdocGetString('Edge', i, 'patches', listString)
          if (j .lt. 0) call exitError('GETTING PATCH FILE NAME FOR AN EDGE')
          if (xoryStr .eq. 'X') then
            ixhi = ix + 1
            iyhi = iy
          else
            ixhi = ix
            iyhi = iy + 1
          endif
          if (j .eq. 0 .and. iz .eq. izval .and. ix .ge. ixpstr .and.
     &        ix .le. ixpend .and. iy .ge. iypstr .and. iy .le.iypend .and.
     &        ixhi .le. ixpend .and. iyhi .le.iypend) then
c             
c             A valid edge has a patch file - get shifts and x/y index
            numEdge = numEdge + 1
            if (numEdge .gt. 2 * maxvols) call exitError
     &          ('TOO MANY EDGES AT ONE Z VALUE FOR ARRAYS')
            if (AdocGetThreeFloats('Edge', i, 'shift', edgeShift(1, numEdge),
     &          edgeShift(2, numEdge), edgeShift(3, numEdge)) .ne. 0)
     &          call exitError('GETTING SHIFTS FOR AN EDGE')
            indXorY(numEdge) = 1
            if (xoryStr .eq. 'Y') indXorY(numEdge) = 2
c             
c             read the patches in
            istrVector(numEdge) = indVec
            call readConsensusVectors(i, listString, indVec, residCrit,
     &          outlierCrit)
            numVectors(numEdge) = indVec - istrVector(numEdge)
            call setVectorGrid(numEdge)
            maxNumVecZ = max(maxNumVecZ, numVecGrid(3,numEdge))
c             
c             Look up lower and upper piece in volume list
            indlo = 0
            indhi = 0
            ixhi = ix
            iyhi = iy
            if (indXorY(numEdge) .eq. 1) ixhi = ix + 1
            if (indXorY(numEdge) .eq. 2) iyhi = iy + 1
            do j = 1, numVols
              if (ixPiece(j) .eq. ix .and. iyPiece(j) .eq. iy) indlo = j
              if (ixPiece(j) .eq. ixhi .and. iyPiece(j) .eq. iyhi) indhi = j
            enddo
            if (indlo * indhi .ne. 0) then
              iVolUpper(indXorY(numEdge), indlo) = indhi
              indVector(indXorY(numEdge), indlo) = numEdge
              iVolLower(indXorY(numEdge), indhi) = indlo
            endif
          endif
        enddo
c
c         Check that each volume is connected - just quit if not
        do i = 1, numVols
          if (iVolUpper(1,i) + iVolUpper(2,i) + iVolLower(1,i) +
     &        iVolLower(2,i) .eq. 0) then
            write(listString,'(a,3i4,a)')'THE VOLUME AT',ixPiece(i), iyPiece(
     &          i), izval ,' HAS NO EDGES WITH OTHER VOLUMES'
            call exitError(listString(1:lnblnk(listString)))
          endif
        enddo
        print *,numVols,' piece volumes and', numEdge, ' edges found'
        pairwise = .not.allTogether .and. numVols .gt. 2
c
c         find the rotation angle unless it was set
        if (ifFindRot .ne. 0) then
          angsum = 0.
          nsum = 0
          do iv = 1, numVols
            do iexy = 1,2
              jv = iVolUpper(iexy,iv)
              if (jv .gt. 0) then
                j = indVector(iexy,iv)
                dx = nxyzin(1,jv) / 2. - nxyzin(1,iv) / 2. - edgeShift(1,j)
                dy = nxyzin(2,jv) / 2. - nxyzin(2,iv) / 2. - edgeShift(2,j)
                angle = atan2d(dy, dx)
                angsum = angsum + angle
                if (iexy .eq. 2) angsum = angsum - 90.
                nsum = nsum + 1
              endif
            enddo
          enddo
          xyRotation = -angsum / nsum
          write(*,'(a,f6.1,a)')'Rotating all pieces by',xyRotation,
     &        ' in X/Y plane'
        endif
c
c         Rotate the shifts if rotation is non-zero (don't need to keep old?)
c         In any case, convert to shifts adjusted to equal sized volumes
        nxsum = 0
        nysum = 0
        xsum = 0.
        ysum = 0.
        nxmax = 0
        nymax = 0
        nzout = 0
        call fillZmatrix(rmat, xyRotation * DTOR, cosrot, sinrot)
        do iv = 1, numVols
          do i = 1, 3
            do j = 1, 3
              fullMat(i,j,iv) = rmat(i,j)
            enddo
          enddo
          do iexy = 1,2
            jv = iVolUpper(iexy,iv)
            if (jv .gt. 0) then
              j = indVector(iexy,iv)
              dx = edgeShift(1,j) + nxyzin(1,iv) / 2. - nxyzin(1,jv) / 2.
              dy = edgeShift(2,j) + nxyzin(2,iv) / 2. - nxyzin(2,jv) / 2.
c               print *,iv,iexy,dx,dy,(edgeShift(i,j),i=1,3)
              if (xyRotation .ne. 0.) then
                scalexyz = cosrot * dx - sinrot * dy
                dy = sinrot * dx + cosrot * dy
                dx = scalexyz
              endif
              edgeShift(1,j) = dx
              edgeShift(2,j) = dy
              edgeShift(3,j) = edgeShift(3,j) + nxyzin(3,iv) / 2. - 
     &            nxyzin(3,jv) / 2.
c               print *,edgeShift(1,j),edgeShift(2,j),edgeShift(3,j)
              if (iexy .eq. 1) then
                xsum = xsum - edgeShift(1,j)
                nxsum = nxsum + 1
              else
                ysum = ysum - edgeShift(2,j)
                nysum = nysum + 1
              endif
            endif
          enddo
          nxmax = max(nxmax, nxyzin(1,iv))
          nymax = max(nymax, nxyzin(2,iv))
          nzout = max(nzout, nxyzin(3,iv))
        enddo
        intervalX = nint(xsum / max(1,nxsum))
        intervalY = nint(ysum / max(1,nysum))
        print *,'Spacing between aligned pieces in X and Y:', intervalX,
     &      intervalY
c         
c         If no output size entered, use the max size
        if (ifMaxSize .ne. 0) then
          nxout = nxmax
          nyout = nymax
        endif
        scalexyz = 1. / (max(nxmax, nymax))
c        scalexyz = 1.
        spacings(1,1) = scalexyz * intervals(1,1)
        spacings(2,2) = scalexyz * intervals(2,2)
c         
c         Copy the edge displacements into an appropriate array and resolve
c         them to volume shifts
        do j = 1, numEdge
          do i = 1, 3
            edgeGeomVars(i, j) = edgeShift(i,j) + intervals(indXorY(j),i)
          enddo
        enddo
        call resolveEdgesToVolumes(edgeGeomVars, 7, volShift, 3, 1, 3)
c         
c         Get an error measure
        xsum = 0.
        ysum = 0.
        nsum = 0
        do iv = 1, numVols
          do iexy = 1,2
            jv = iVolUpper(iexy,iv)
            if (jv .gt. 0) then
              j = indVector(iexy,iv)
              dist = 0
              do i = 1, 3
                dist = dist + (edgeShift(i,j) + intervals(iexy,i) +
     &              volShift(i,jv) - volShift(i,iv))**2
              enddo
              dist = sqrt(dist)
              xsum = xsum + dist
              ysum = max(ysum, dist)
              nsum = nsum + 1
            endif
          enddo
        enddo
        write(*,105) xsum / nsum, ysum
105     format('The error after shifting pieces into register has mean',f7.2,
     &      ', maximum', f7.2)
c         
c         transform the vectors to fit the output volumes as now defined, and
c         compute the centered positions in upper and lower piece
        call transformVectors(fullMat, volShift, edgeShift)
c         
c         Set up pairs or set up to run whole set at once
        numPairs = 0
        if (pairwise) then
          numSolve = 2
          do iv = 1, numVols
            do iexy = 1,2
              jv = iVolUpper(iexy,iv)
              if (jv .gt. 0) then
                numPairs = numPairs + 1
                iPairLower(numPairs) = iv
                iPairUpper(numPairs) = jv
                iPairEdge(numPairs) = indVector(iexy, iv)
              endif
            enddo
          enddo
        else
          numSolve = numVols
          numPairs = 1
c           
c           Initialize ivolSolve(0) to 0 so that tests below can test on
c           ivolSolve alone and not on the index too.
          do i = 0, numVols
            iVolSolve(i) = i
            iVolFull(i) = i
          enddo
        endif
c         
c         Loop on the pairs
        do iPair = 1, numPairs
          if (pairwise) then
            do i = 0, numVols
              iVolSolve(i) = 0
            enddo
            iVolFull(1) = iPairLower(iPair)
            iVolFull(2) = iPairUpper(iPair)
            iVolSolve(iPairLower(iPair)) = 1
            iVolSolve(iPairUpper(iPair)) = 2
            write(*,108)ixPiece(iVolFull(1)), iyPiece(iVolFull(1)),
     &          ixPiece(iVolFull(2)), iyPiece(iVolFull(2))
108         format(/,'Analyzing overlap between piece',2i3,'  and piece',2i3)
          endif
c         
c           Set up variables for minimization
          nvarsrch = 0
          do i = 1, 7
            mapArray(i) = 0
          enddo
          if (findMag) then
            mapGmag = nvarsrch + 1
            nvarsrch = nvarsrch + numSolve - 1
          endif
          if (findThin) then
            mapComp = nvarsrch + 1
            nvarsrch = nvarsrch + numSolve - 1
          endif
          if (findStretch) then
            mapDmag = nvarsrch + 1
            mapSkew = mapDmag + numSolve - 1
            nvarsrch = nvarsrch + 2 * (numSolve - 1)
            mapAngles = mapSkew
          else
            mapAngles = nvarsrch + 1
          endif
          if (ifAlpha .ne. 0) then
            mapAlpha = nvarsrch + 1
            nvarsrch = nvarsrch + numSolve - 1
          endif
          if (ifBeta .ne. 0) then
            mapBeta = nvarsrch + 1
            nvarsrch = nvarsrch + numSolve - 1
          endif
          if (ifGamma .ne. 0) then
            mapGamma = nvarsrch + 1
            nvarsrch = nvarsrch + numSolve - 1
          endif
          call initializeVar(var, gmag, mapGmag, numSolve, 1.)
          call initializeVar(var, comp, mapComp, numSolve, 1.)
          call initializeVar(var, dmag, mapDmag, numSolve, 0.)
          call initializeVar(var, skew, mapSkew, numSolve, 0.)
          call initializeVar(var, alpha, mapAlpha, numSolve, 0.)
          call initializeVar(var, beta, mapBeta, numSolve, 0.)
          call initializeVar(var, gamma, mapGamma, numSolve, 0.)
c           
          ifUnload = 1
          call stitchfunc(nvarsrch,var,finalErr,Grad)
          write(6,106)nvarsrch,finalErr
106       format(i5,' variables in search',/,T48,'Initial F : ',T65,E14.7)
          call metro(nvarsrch, var, stitchfunc, finalErr, grad, facMetro, eps,
     &        maxCycle, ierr, hess, ncycle)
          if (ierr.eq.1) call exitError('Minimization error #1 - DG > 0')
          if (ierr.eq.2) call exitError(
     &        'Minimization error #2 - Linear search lost')
          if (ierr.eq.4) call exitError(
     &        'Minimization error #4 - Matrix non-positive definite')
          if (ierr.eq.3) call exitError(
     &        'Minimization error #3 - Iteration limit exceeded')

          call stitchfunc(nvarsrch,var,finalErr,Grad)
          write(6,101)finalErr,ncycle
101       format(T48,'Final   F : ',T65,E14.7,/,' Number of cycles : ',I5)
          call flush(6)
          if (pairwise) then
            j = iPairEdge(iPair)
c             
c             Store values that represent how much the upper piece is displaced
c             from alignment, the negative of how much needs to be applied to 
c             it - these will work the same as shift displacements
            edgeGeomVars(1,j) = log(gmag(1)) - log(gmag(2))
            edgeGeomVars(2,j) = log(comp(1)) - log(comp(2))
            edgeGeomVars(3,j) = dmag(1) - dmag(2)
            edgeGeomVars(4,j) = skew(1) - skew(2)
            edgeGeomVars(5,j) = alpha(1) - alpha(2)
            edgeGeomVars(6,j) = beta(1) - beta(2)
            edgeGeomVars(7,j) = gamma(1) - gamma(2)
          endif
          call scaleAndReport(ixPiece, iyPiece)
        enddo
        if (pairwise) then
c           
          write(*,'(/,a)')
     &        'Resolving pairwise solution to solution for all volumes:'
          call resolveEdgesToVolumes(edgeGeomVars, 7, volGeomVars, 7, maxvols,
     &        1)
c           
c           Get mag and comp back, then set up so stitchfunc can compute
c           dxyz from the existing variable values
          do i = 1, numVols
            gmag(i) = exp(gmag(i))
            comp(i) = exp(comp(i))
          enddo
          numSolve = numVols
          do i = 0, numVols
            iVolSolve(i) = i
            iVolFull(i) = i
          enddo
          ifUnload = 0
          call stitchfunc(nvarsrch,var,finalErr,Grad)
          call scaleAndReport(ixPiece, iyPiece)
        endif
c         
c         compute full transform and output to file
        do iv = 1, numVols
          do i = 1,3
            fitdxyz(i,iv) = dxyz(iv,i)
          enddo
          call xfmult3d(rmat, volShift(1,iv), fitMat(1,1,iv), fitdxyz(1,iv),
     &        fullMat(1,1,iv), fullDxyz(1,iv))
          call xfInv3d(fullMat(1,1,iv), fullDxyz(1,iv), finvMat(1,1,iv),
     &        finvDxyz(1,iv))
          if (AdocGetSectionName('Piece', indSection(iv), listString) .lt. 0)
     &        call exitError('GETTING ADOC SECTION NAME FOR A PIECE')
          call rootAndExtension(listString, 'matxf', filename)
          call dopen(1, filename, 'new', 'f')
          write(1,104)((fullMat(i,j,iv),j=1,3), fullDxyz(i,iv),i=1,3)
104       format(3f10.6,f10.3)
          close(1)
          if (AdocSetKeyValue('Piece', indSection(iv), 'matxf', filename)
     &        .ne. 0) call exitError('SETTING NEW KEY-VALUE PAIR')
        enddo
c         
c         Transform the vectors to the new alignments then get some summary
c         features of the edges
c        call transformVectors(fullMat, fullDxyz, edgeShift)
        do i = 1,3
          delMean(i) = 0.
        enddo
        nsum = 0
        edgeWidthMean = 0.
        do iv = 1, numVols
          do iexy = 1,2
            jv = iVolUpper(iexy,iv)
            if (jv .gt. 0) then
              j = indVector(iexy,iv)
              kstr = istrVector(j)
              kend = kstr + numVectors(j) - 1
              nsum = nsum + 1
c               
c               Transform each position in lower and upper volume with added
c               transform, and get vector from difference.  This matches the
c               scaled residuals
c              print *,'Volume, upper vol, xy, edge #:',iv,jv,iexy,j
c             print *,numVectors(j),' positions'
              do k = kstr, kend
                call transformPos(cenRot(1,k), fitMat(1,1,iv), fitdxyz(1,iv), 
     &              nxyzout, nxyzout, poslo(1,k))
                do i = 1, 3
                  var(i) = cenRot(i,k) + vecRot(i,k) - intervals(iexy,i)
                enddo
                call transformPos(var, fitMat(1,1,jv), fitdxyz(1,jv), 
     &              nxyzout, nxyzout, posup(1,k))
                do i = 1, 3
                  cenRot(i,k) = poslo(i,k)
                  vecRot(i,k) = posup(i,k) + intervals(iexy,i) - poslo(i,k)
                enddo
c               write(*,'(3i6,4f9.2)')(nint(cenRot(i,k)),i=1,3),(vecRot(i,k),i=1,3)
              enddo
c              
              do i = 1, 3
                edgeMin(i,j) = 1.e10
                edgeMax(i,j) = -1.e10
                gridPad = 0.
                if (i .lt. 3) gridPad = 0.5 * vecGridDelta(i,j)
c                 
c                 Get the mean grid spacing, but pad the edge by half of its
c                 own grid spacing for X and Y, not for Z
                delMean(i) = delMean(i) + vecGridDelta(i,j)
                do k = kstr, kend
                  edgeMin(i,j) = min(edgeMin(i,j), cenRot(i,k) - gridPad)
                  edgeMax(i,j) = max(edgeMax(i,j), cenRot(i,k) + gridPad)
                enddo
              enddo
              edgeWidthMean = edgeWidthMean + edgeMax(iexy,j) - edgeMin(iexy,j)
c               
c               Determine min and max in bands across the edge for better
c               estimation of edge fractions
              numBands(j) = max(1,min(maxBand, numVecGrid(3-iexy,j) / 2))
              do i = 1, numBands(j)
                bandMin(i,j) = edgeMax(iexy,j)
                bandMax(i,j) = edgeMin(iexy,j)
              enddo
              gridPad = 0.5 * vecGridDelta(iexy,j)
              bandDel(j) = (edgeMax(3-iexy,j) - edgeMin(3-iexy,j)) /numBands(j)
              do k = kstr, kend
                i = (cenRot(3-iexy,k) - edgeMin(3-iexy,j)) / bandDel(j) + 1.
                i = max(1, min(numBands(j), i))
                bandMin(i,j) = min(bandMin(i,j), cenRot(iexy,k) - gridPad)
                bandMax(i,j) = max(bandMax(i,j), cenRot(iexy,k) + gridPad)
c                if (k.lt.kstr + 100) write(*,'(3f8.1,9f6.1)')
c     &              (cenRot(i,k),i=1,3),
c     &              (vector(i,k),i=1,3),(vecRot(i,k),i=1,3),
c     &              (resid(i,k)/scalexyz,i=1,3)
              enddo
c              print *,iv,iexy,(edgeMin(i,j), edgeMax(i,j), i=1,3)
c              print *,numBands(j), ' bands'
c              print *,(bandMin(i,j), bandMax(i,j), i=1,numBands(j))
            endif
          enddo
        enddo
        do i = 1,3
          delMean(i) = delMean(i) / nsum
        enddo
        edgeWidthMean = edgeWidthMean / nsum
c         
c         Get limits of corners of volumes and limits for output grids
        do iv = 1, numVols
          volLimTR(1,iv) = nxout
          volLimTR(2,iv) = nyout
          volLimBR(1,iv) = nxout
          volLimBR(2,iv) = 0.
          volLimTL(1,iv) = 0.
          volLimTL(2,iv) = nyout
          volLimBL(1,iv) = 0.
          volLimBR(2,iv) = 0.
          ioutGridStart(1,iv) = nxout / 2
          ioutGridStart(2,iv) = nyout / 2
          ioutGridStart(3,iv) = nzout / 2
          ioutGridEnd(1,iv) = nxout / 2
          ioutGridEnd(2,iv) = nyout / 2
          ioutGridEnd(3,iv) = nzout / 2
          do iexy = 1,2
c             
c             First do upper edge
            if (iVolUpper(iexy,iv) .gt. 0) then
              j = indVector(iexy,iv)
c               
c               Output grid goes out as far as any edges go in each direction
              do i = 1, 3
                ioutGridStart(i,iv) = min(ioutGridStart(i,iv), 
     &              nint(edgeMin(i,j)))
                ioutGridEnd(i,iv) = max(ioutGridEnd(i,iv), 
     &              nint(edgeMax(i,j)))
              enddo
c               
c               volume limits for end fractions are the minimum of all limits
              volLimTR(1,iv) = min(volLimTR(1,iv), edgeMax(1,j))
              volLimTR(2,iv) = min(volLimTR(2,iv), edgeMax(2,j))
              if (iexy .eq. 1) then
                volLimBR(1,iv) = min(volLimBR(1,iv), edgeMax(1,j))
                volLimBR(2,iv) = max(volLimBR(2,iv), edgeMin(2,j))
              else
                volLimTL(1,iv) = max(volLimTL(1,iv), edgeMin(1,j))
                volLimTL(2,iv) = min(volLimTL(2,iv), edgeMax(2,j))
              endif
            endif
c
c             Now do lower edge
            jv = iVolLower(iexy,iv)
            if (jv .gt. 0) then
              j = indVector(iexy,jv)
              do i = 1, 3
                ioutGridStart(i,iv) = min(ioutGridStart(i,iv), 
     &              nint(edgeMin(i,j)) - intervals(iexy,i))
                ioutGridEnd(i,iv) = max(ioutGridEnd(i,iv), 
     &              nint(edgeMax(i,j)) - intervals(iexy,i))
              enddo
              volLimBL(1,iv) = max(volLimBL(1,iv), edgeMin(1,j) -
     &            intervals(iexy,1))
              volLimBL(2,iv) = max(volLimBL(2,iv), edgeMin(2,j) -
     &            intervals(iexy,2))
              if (iexy .eq. 1) then
                volLimTL(1,iv) = max(volLimTL(1,iv), edgeMin(1,j) - intervalX)
                volLimTL(2,iv) = min(volLimTL(2,iv), edgeMax(2,j))
              else
                volLimBR(1,iv) = min(volLimBR(1,iv), edgeMax(1,j))
                volLimBR(2,iv) = max(volLimBR(2,iv), edgeMin(2,j) - intervalY)
              endif
            endif
          enddo
c          print *,iv,volLimTL(1,iv),volLimTL(2,iv),volLimTR(1,iv),volLimTR(2,iv)
c          print *,volLimBL(1,iv),volLimBL(2,iv),volLimBR(1,iv),volLimBR(2,iv)
c          print *,(ioutGridStart(i,iv),ioutGridEnd(i,iv),i=1,3)
c
c         Massage the starting and ending output grid values to give evenly
c         spaced data
          do i = 1, 3
            ioutGridStart(i,iv) = max(ioutGridStart(i,iv), 1)
            ioutGridEnd(i,iv) = min(ioutGridEnd(i,iv), nxyzout(i) - 2)
            ix = ioutGridEnd(i,iv) + 1 - ioutGridStart(i,iv)
            numOutGrid(i,iv) = min(ix, nint(ix / (delMean(i) * sampleFactor))
     &          + 1)
            if (i .eq. 3. .and. maxNumVecZ .lt. 2) then
              numOutGrid(i,iv) = 1
              ioutGridStart(i,iv) = (ioutGridStart(i,iv) + ioutGridEnd(i,iv))/2
            endif
            ioutGridDelta(i,iv) = ix / max(1, numOutGrid(i,iv) - 1)
            iy = mod(iz, max(1, numOutGrid(i,iv) - 1))
            ioutGridStart(i,iv) = ioutGridStart(i,iv) + iy / 2
            ioutGridEnd(i,iv) = ioutGridStart(i,iv) + (numOutGrid(i,iv) - 1) *
     &          ioutGridDelta(i,iv)
          enddo 
        enddo
c         
c         Loop on the creating the output grids
        do iv = 1, numVols
          numVecOut = 0
          do ix = 1, numOutGrid(1,iv)
            ixpos = ioutGridStart(1,iv) + (ix-1)*ioutGridDelta(1,iv)
            do iy = 1, numOutGrid(2,iv)
              iypos = ioutGridStart(2,iv) + (iy-1)*ioutGridDelta(2,iv)
c               
c               Prescan all the positions in a column for each edge and enforce
c               consistency in whether an edge is kept active in the initial
c               test: if any points in column give a vector and not all points
c               do so, then set up to extend to nearest vector
              call countedges(float(ixpos), float(iypos), iv)
              do iexy=1,2
                do i=1,numedges(iexy)
                  j=inedge(i,iexy)
                  k = inedlower(i,iexy)
                  nsum = 0
                  do iz = 1, numOutGrid(3,iv)
                    xsum = ioutGridStart(3,iv) + (iz-1)*ioutGridDelta(3,iv)
                    call interpolateVector(xInPiece(k), yInPiece(k), xsum,
     &                  inPiece(k), j, 0.5, 0, fitdxyz, ndim)
                    if (ndim .gt. 0) nsum = nsum + 1
                  enddo
                  nIniExtend(i,iexy) = 0
c                  if (nsum .gt. 0 .and. nsum .lt. numOutGrid(3,iv))
                  if (nsum .eq. numOutGrid(3,iv))
     &                nIniExtend(i,iexy) = 5
                enddo
              enddo

              do iz = 1, numOutGrid(3,iv)
                i = numVecOut + 1
                if (i .gt. limoutgrid) call exitError(
     &              'TOO MANY OUTPUT VECTORS FOR ARRAYS')
                iposOut(1,i) = ixpos
                iposOut(2,i) = iypos
                iposOut(3,i) = ioutGridStart(3,iv) + (iz-1)*ioutGridDelta(3,iv)
                call findWarpVector(iposOut(1,i), iv, vecOut(1,i), ndim)
                if (ndim .gt. 0) numVecOut = i
              enddo
            enddo
          enddo
c
          if (AdocGetSectionName('Piece', indSection(iv), listString) .lt. 0)
     &        call exitError('GETTING ADOC SECTION NAME FOR A PIECE')
          call rootAndExtension(listString, 'patch', filename)
          call dopen(1, filename, 'new', 'f')
          write(1,'(i8,a)')numVecOut,' positions'
          write(1,'(3i6,4f9.2)')((iposOut(i,j),i=1,3),(vecOut(i,j),i=1,4),
     &        j=1,numVecOut)
          close(1)
          if (AdocSetKeyValue('Piece', indSection(iv), 'vectors', filename)
     &        .ne. 0) call exitError('SETTING NEW KEY-VALUE PAIR')
        enddo
        
c
c         Add the output size and spacing for the section
        numSections = AdocGetNumberOfSections('Section')
        ix = 0
        do i = 1, numSections
          if (AdocGetInteger('Section', i, 'zvalue', iz) .ne. 0)
     &        call exitError('GETTING Z VALUE FOR A SECTION')
          if (iz .eq. izval) ix = i
        enddo
        if (ix .eq. 0) then
          call int_iwrite(listString, izval, iy)
          filename = 'Section'//listString(1:iy)
          ix = AdocAddSection('Section', filename)
          if (ix .lt. 0) call exitError('ADDING A SECTION TO INFO FILE') 
          if (AdocSetInteger('Section', ix, 'zvalue', izval) .ne. 0)
     &      call exitError('SETTING NEW KEY-VALUE PAIR')
        endif

        if (AdocSetThreeIntegers('Section', ix, 'outsize', nxout, nyout, nzout)
     &      .ne. 0 .or.
     &      AdocSetTwoIntegers('Section', ix, 'spacing', intervalX, intervalY)
     &      .ne. 0 .or.
     &      AdocSetFloat('Section', ix, 'intervalFactor', sampleFactor) .ne. 0)
     &      call exitError('SETTING NEW KEY-VALUE PAIR')
      enddo
c
      ierr = AdocWrite(infofile)
      if (ierr .ne. 0) print *,'Error',ierr,' writing to info file'
      call exit(0)
      end

c       Reads the main patch vector file, and reads reduced vector file from
c       Refinematch and residual vector file from Findwarp if they exist.
c       Stores a vector in the arrays if it is still in all files and if
c       its residual and outlier fraction are less than the criteria
c
      subroutine readConsensusVectors(iedge, patchfile, indVec, residCrit,
     &    outlierCrit)
      use stitchvars
      implicit none
      integer*4 iedge, indVec
      character*(*) patchfile
      real*4 residCrit, outlierCrit
      logical keep
      integer*4 numVecs, numReduce, numResid, i,j,k,numResLeft,ierr
      real*4 residual,outFrac,dum1, dum2, dum3
      character*320 filename, errString
      real*4, allocatable :: reduceCen(:,:), residCen(:,:)
      integer*4 AdocGetString
      
      numReduce = 0
      numResid = 0
      numResLeft = 0
      filename = patchfile
      call dopen(1, patchfile, 'ro', 'f')
      read(1, *, end=98,err=98) numVecs
c       
      if (AdocGetString('Edge', iedge, 'reducePatch', filename) .eq. 0) then
        call dopen(2, filename, 'ro', 'f')
        read(2, *, end=98,err=98) numReduce
        if (numReduce .gt. 0) then
          allocate(reduceCen(3,numReduce), stat = ierr)
          if (ierr .ne. 0) call exitError(
     &        'ALLOCATING MEMORY FOR REDUCED PATCHES')
          do j = 1, numReduce
            read(2, *, end=98,err=98)(reduceCen(k, j), k = 1,3)
          enddo
        endif
        close(2)
      endif
c      
      if (AdocGetString('Edge', iedge, 'residPatch', filename) .eq. 0) then
        call dopen(2, filename, 'ro', 'f')
        read(2, *, end=98,err=98) numResid
        if (numResid .gt. 0) then
          allocate(residCen(3,numResid), stat = ierr)
          if (ierr .ne. 0) call exitError(
     &        'ALLOCATING MEMORY FOR RESIDUAL PATCH DATA')
c           
c           Read the residual vectors and eliminate outliers right here
          numResLeft = 0
          do j = 1, numResid
            read(2, *, end=98,err=98)(residCen(k, numResLeft + 1), k = 1,3),
     &          dum1, dum2, dum3, residual, outFrac
            if (residual .lt. residCrit .and. outFrac .lt. outlierCrit)
     &          numResLeft = numResLeft + 1
          enddo
         write(*,'(i5,a,/,5x,a)') numResid - numResleft,' patches eliminated'//
     &        ' by high residual or outlier fraction from', trim(patchfile)
        endif
        close(2)
      else
        write(*,'(/,a,a)')'WARNING: NO RESIDUAL OR OUTLIER INFORMATION '//
     &      'AVAILABLE FOR PATCHES IN ',trim(patchfile)
      endif
c       
c       Read in the vectors
      filename = patchfile
      do j = 1, numVecs
        if (indVec .ge. maxvecs) call exitError(
     &    'TOO MANY PATCH VECTORS IN EDGES FOR ARRAYS')
        read(1, *, end=98,err=98)(center(k,indVec),k=1,3),
     &      (vector(k,indVec), k=1,3)
c         
c         Look the vector up in the reduced vectors
        keep = .true.
        if (numReduce .gt. 0) then
          keep = .false.
          do i = 1, numReduce
            if (abs(center(1,indVec) - reduceCen(1,i)) .le. 0.1 .and.
     &          abs(center(2,indVec) - reduceCen(2,i)) .le. 0.1 .and.
     &          abs(center(3,indVec) - reduceCen(3,i)) .le. 0.1) then
              keep = .true.
              exit
            endif
          enddo
c          if (.not.keep) print *,(nint(center(i,indVec)),i=1,3),
c     &        ' not found in reduced patches'
        endif
c         
c         If still in, look it up in the residual vectors
        if (keep .and. numResLeft .gt. 0) then
          keep = .false.
          do i = 1, numresLeft
            if (abs(center(1,indVec) - residCen(1,i)) .le. 0.1 .and.
     &          abs(center(2,indVec) - residCen(2,i)) .le. 0.1 .and.
     &          abs(center(3,indVec) - residCen(3,i)) .le. 0.1) then
              keep = .true.
              exit
            endif
          enddo
c          if (.not.keep) print *,(nint(center(i,indVec)),i=1,3),
c     &        ' not found in resid patches'
        endif
        if (keep) indVec = indVec + 1
      enddo
      close(1)
      if (numReduce .gt. 0) deallocate(reduceCen, stat = ierr)
      if (numResid .gt. 0) deallocate(residCen, stat = ierr)
      return
98    write(errString, '(a,a)')'READING PATCH VECTOR FILE ',filename
      call exitError(errString)
      end


c       STITCHFUNC for evaluating matrices, error and gradient
c
      subroutine stitchfunc(nvarsrch,var,ferror,grad)
c       
      use stitchvars
      implicit none
      integer mv
      parameter (mv = maxvols)
      integer*4 nvarsrch
      real*4 var(*),ferror,grad(*)
      real*4 xmat(9,mv), ymat(9,mv), zmat(9,mv), dmat(9,mv)
      real*4 dermat(3,3), da(9), dan(9)
      real*4 calf(mv), salf(mv), cbet(mv), sbet(mv), cgam(mv), sgam(mv)
      real*4 cdel(mv), sdel(mv), fac
      real*8 derror, dsum
      integer*4 ivar, igrad, i, k, iexy, kstr, kend, ixyz, itype, j
      integer*4 numVec, ivs, jvs, ivf, jvf
      real*8 gradientSum
c      equivalence (aa, fitMat)
c       
c       ivs, jvs are used to access arrays indexed on volumes beings solved,
c       ivf, jvf are used to access the full list of loaded volumes
c       
      if (ifUnload .ne. 0) call unloadVars(var)
c       
c       Build the matrices for each volume
      do i = 1, numSolve
        call fillDistMatrix(dmat(1,i), gmag(i), comp(i), dmag(i), skew(i),
     &      cdel(i), sdel(i))
        call fillXmatrix(xmat(1,i), alpha(i), calf(i), salf(i))
        call fillYmatrix(ymat(1,i), beta(i), cbet(i), sbet(i))
        call fillZmatrix(zmat(1,i), gamma(i), cgam(i), sgam(i))
        call fullMatProduct(dmat(1,i), zmat(1,i), ymat(1,i), xmat(1,i),
     &      aa(1,1,i))
      enddo
c      print *,(dmat(i,1),i=1,9)
c      print *,(xmat(i,1),i=1,9)
c      print *,(ymat(i,1),i=1,9)
c      print *,(zmat(i,1),i=1,9)
c      print *,((aa(i,k,1),i=1,3),k=1,3)
c       
c       Compute the residual error components needed to solve for dxyz
      do ivs = 1, numSolve
        ivf = iVolFull(ivs)
        do iexy = 1, 2
          jvf = iVolUpper(iexy,ivf)
          if (iVolSolve(jvf) .gt. 0) then
            jvs = iVolSolve(jvf)
            kstr = istrVector(indVector(iexy,ivf))
            kend = kstr + numVectors(indVector(iexy,ivf)) - 1
            do ixyz = 1, 3
              do k = kstr, kend
                resid(ixyz,k) = spacings(iexy,ixyz)
                do i = 1, 3
                  resid(ixyz,k) = resid(ixyz,k) + aa(ixyz,i,jvs) * posup(i,k) -
     &                aa(ixyz,i,ivs) * poslo(i,k)
                enddo
              enddo
            enddo
c            if (ivs .eq. 1) write(*,'(9f8.1)')((resid(ixyz, k)
c     &          /scalexyz,ixyz=1,3),k=kstr,kend)
          endif
        enddo
      enddo
c       
c       Solve for dxyz
      do ivar = 1, numSolve - 1
        do j = 1, numSolve - 1
          daa(ivar,j) = 0.
        enddo
        do j = 1, 3
          dbb(ivar,j) = 0
        enddo
        
        do ivs = 1, numSolve
          ivf = iVolFull(ivs)
          do iexy = 1, 2
            jvf = iVolUpper(iexy,ivf)
            if (iVolSolve(jvf) .gt. 0) then
              jvs = iVolSolve(jvf)
              numVec = numVectors(indVector(iexy,ivf))
              fac = 0.
c
c               Cover the cases in which this edge counts, and of what to do
c               with the upper piece variables: either add to all variables if
c               it is N, subtract from upper variable if it this is lower, or
c               add to upper var if it is upper
              if (jvs .eq. numSolve) then
                do j = 1, numSolve - 1
                  daa(ivar,j) = daa(ivar,j) + numVec
                enddo
                fac = 1.
              else if (ivs .eq. ivar) then
                daa(ivar,jvs) = daa(ivar,jvs) - numVec
                fac = 1.
              else if (jvs .eq. ivar) then
                daa(ivar,jvs) = daa(ivar,jvs) + numVec
                fac = -1.
              endif
              if (fac .ne. 0.) then
c                 
c                 In any case, add to this variable depending on fac, and 
c                 add up the resid components for the constant terms
                daa(ivar,ivs) = daa(ivar,ivs) + fac * numVec
                kstr = istrVector(indVector(iexy,ivf))
                kend = kstr + numVec - 1
                do j = 1, 3
                  dsum = 0.
                  do k = kstr, kend
                    dsum = dsum + resid(j,k)
                  enddo
                  dbb(ivar,j) = dbb(ivar,j) + dsum * fac
                enddo
              endif
            endif
          enddo
        enddo
      enddo
c       
c       Get solution and copy the dxyz out, compose the N one
c        write(*,'(8f6.0,3f10.2)')((daa(ivar,i),i=1,numSolve-1),(dbb(ivar,i),i=1,3)
c     &      ,ivar=1,numSolve-1)
      call gaussjd(daa, numSolve - 1, maxvols, dbb, 3, maxvols, 7)
c        print *,'Solution:'
c        write(*,'(8f6.3,3f10.2)')((daa(ivar,i),i=1,numSolve-1),(dbb(ivar,i),i=1,3)
c     &      ,ivar=1,numSolve-1)
      do j = 1, 3
        dxyz(numSolve,j) = 0
        do i = 1, numSolve - 1
          dxyz(i,j) = dbb(i,j)
          dxyz(numSolve,j) = dxyz(numSolve,j) - dbb(i,j)
        enddo
      enddo
c      write(*,'(9f8.1)')((dxyz(i,j)/scalexyz,i=1,3),j = 1, numSolve)
c       
c       Now correct the residuals and get the error
      derror = 0.
      do ivs = 1, numSolve
        ivf = iVolFull(ivs)
        do iexy = 1, 2
          jvf = iVolUpper(iexy,ivf)
          if (iVolSolve(jvf) .gt. 0) then
            jvs = iVolSolve(jvf)
            kstr = istrVector(indVector(iexy,ivf))
            kend = kstr + numVectors(indVector(iexy,ivf)) - 1
            do ixyz = 1, 3
              do k = kstr, kend
                resid(ixyz,k) = resid(ixyz,k) + dxyz(jvs,ixyz) - dxyz(ivs,ixyz)
                derror = derror + resid(ixyz,k)**2
              enddo
            enddo
c            if (ivs .eq. 1 .and. iexy .eq. 1) write(*,'(9f8.1)')((resid(ixyz, k)
c     &          /scalexyz,ixyz=1,3),k=kstr,kend)
c            if (ivs .eq. 1 .and. iexy .eq. 2) write(*,'(6f8.1)')((poslo(ixyz, k)
c     &          /scalexyz,ixyz=1,3),(posup(ixyz, k)/scalexyz ,ixyz=1,3),k=kstr,kend)
          endif
        enddo
      enddo
      ferror = derror
c       
c       Loop on the variables, compute gradients for ones that are variable
c       
      do itype = 1, 7
        if (mapArray(itype) .gt. 0) then
          do ivar = numSolve, 1, -1
            dsum = 0.
c             
c             compute derivative of matrix w/r to this variable
            call zeroMatrix(dermat)
            if (itype .eq. 1) then
c               
c               gmag variable
              dermat(1,1) = cdel(ivar)
              dermat(2,1) = -sdel(ivar)
              dermat(1,2) = -sdel(ivar)
              dermat(2,2) = cdel(ivar)
              dermat(3,3) = comp(ivar)
              call fullMatProduct(dermat, zmat(1,ivar), ymat(1,ivar),
     &            xmat(1,ivar), da)
            else if (itype .eq. 2) then
c               
c               comp variable
              dermat(3,3) = gmag(ivar)
              call fullMatProduct(dermat, zmat(1,ivar), ymat(1,ivar),
     &            xmat(1,ivar), da)
            else if (itype .eq. 3) then
c               
c               dmag variable
              dermat(1,1) = cdel(ivar)
              dermat(2,1) = -sdel(ivar)
              dermat(1,2) = sdel(ivar)
              dermat(2,2) = -cdel(ivar)
              call fullMatProduct(dermat, zmat(1,ivar), ymat(1,ivar),
     &            xmat(1,ivar), da)
            else if (itype .eq. 4) then
c               
c               skew variable
              dermat(1,1) = -(gmag(ivar) + dmag(ivar)) * sdel(ivar)
              dermat(2,1) = -(gmag(ivar) + dmag(ivar)) * cdel(ivar)
              dermat(1,2) = -(gmag(ivar) - dmag(ivar)) * cdel(ivar)
              dermat(2,2) = -(gmag(ivar) - dmag(ivar)) * sdel(ivar)
              call fullMatProduct(dermat, zmat(1,ivar), ymat(1,ivar),
     &            xmat(1,ivar), da)
            else if (itype .eq. 5) then
c              
c               X rotation
              dermat(2,2) = -salf(ivar)
              dermat(3,2) = calf(ivar)
              dermat(3,3) = -salf(ivar)
              dermat(2,3) = -calf(ivar)
              call fullMatProduct(dmat(1,ivar), zmat(1,ivar), ymat(1,ivar),
     &            dermat, da)
            else if (itype .eq. 6) then
c              
c               Y rotation
              dermat(1,1) = -sbet(ivar)
              dermat(1,3) = cbet(ivar)
              dermat(3,3) = -sbet(ivar)
              dermat(3,1) = -cbet(ivar)
              call fullMatProduct(dmat(1,ivar), zmat(1,ivar), dermat,
     &            xmat(1,ivar), da)
            else 
c              
c               Z rotation
              dermat(1,1) = -sgam(ivar)
              dermat(2,1) = cgam(ivar)
              dermat(2,2) = -sgam(ivar)
              dermat(1,2) = -cgam(ivar)
              call fullMatProduct(dmat(1,ivar), dermat, ymat(1,ivar),
     &            xmat(1,ivar), da)
            endif
            if (ivar .eq. numSolve) then
c               
c               The last volume is not a variable, but we need the derivative
c               evaluated at that point, so save it
              do k = 1, 9
                dan(k) = da(k)
              enddo
            else
c
c               for real variables, get the appropriate gradient sums
              do ivs = 1, numSolve
                ivf = iVolFull(ivs)
                do iexy = 1, 2
                  jvf = iVolUpper(iexy,ivf)
                  if (iVolSolve(jvf) .gt. 0) then
                    jvs = iVolSolve(jvf)
                    kstr = istrVector(indVector(iexy,ivf))
                    kend = kstr + numVectors(indVector(iexy,ivf)) - 1
                    if (ivs .eq. ivar) then
                      dsum = dsum + gradientSum(da, resid, poslo, kstr, kend,
     &                    -2.)
                    else if (jvs .eq. ivar) then
                      dsum = dsum + gradientSum(da, resid, posup, kstr, kend,
     &                    2.)
                    endif
                    if (jvs .eq. numSolve) then
                     dsum = dsum + gradientSum(dan, resid, posup, kstr, kend,
     &                    -2.)
                    endif
                  endif
                enddo
              enddo
              grad(ivar + mapArray(itype) - 1) = dsum
            endif
          enddo
        endif
      enddo
c      write(*,'(2f12.6,3x,2f12.6,3x,2f12.6)')(var(i),grad(i),i=1,nvarsrch)
      return
      end

c       Forms a gradient sum of the product of position times the derivative
c       matrix times the residual
c
      real*8 function gradientSum(da, resid, pos, kstr, kend, fac)
      implicit none
      real*4 da(3,3), resid(3,*), pos(3,*), fac
      real*8 dsum
      integer*4 kstr, kend, k, ixyz, i
      gradientSum = 0.
      do ixyz = 1, 3
        do k = kstr, kend
          dsum = 0.
          do i = 1,3
            dsum = dsum + da(ixyz,i) * pos(i,k)
          enddo
          gradientSum = gradientSum + fac * dsum * resid(ixyz, k)
        enddo
      enddo
      return
      end
      

c       Initializes one variable to the given value, and if mapVar is > 0, it
c       also initializes n-1 variables at that position in varlist
c
      subroutine InitializeVar(varlist, var, mapVar, numVol, value)
      implicit none
      real*4 varlist(*), var(*), value
      integer*4 mapVar, numVol, i
      do i = 1, numVol
        var(i) = value
      enddo
      if (mapVar .le. 0) return
      do i= 1, numVol - 1
        varlist(i + mapVar - 1) = value
      enddo
      return
      end

c       Unloads all variables from the VAR array
c
      subroutine unloadVars(var)
      use stitchvars
      implicit none
      real*4 var(*)
      call unloadOneVar(var, gmag, mapGmag, numSolve, 1.)
      call unloadOneVar(var, comp, mapComp, numSolve, 1.)
      call unloadOneVar(var, dmag, mapDmag, numSolve, 0.)
      call unloadOneVar(var, skew, mapSkew, numSolve, 0.)
      call unloadOneVar(var, alpha, mapAlpha, numSolve, 0.)
      call unloadOneVar(var, beta, mapBeta, numSolve, 0.)
      call unloadOneVar(var, gamma, mapGamma, numSolve, 0.)
      return 
      end

c       Unloads one variable from VARLIST into VAR if the mapping index
c       mapVar is > 0.  N-1 variables are copied, and the Nth value is set
c       so that the mean of all is valMean.
c
      subroutine unloadOneVar(varlist, var, mapVar, numVol, valMean)
      implicit none
      real*4 varlist(*), var(*), valMean, varSum
      integer*4 mapVar, numVol, i
      if (mapVar .le. 0) return
      varSum = 0.
      do i= 1, numVol - 1
        var(i) = varlist(i + mapVar - 1)
        varSum = varSum + var(i)
      enddo
      var(numVol) = valMean * numVol - varSum
      return
      end

c       Multiply the 4 component matrices (one of which may be a derivative)
c       to form the full product matrix
c
      subroutine fullMatProduct(dmat, zmat, ymat, xmat, prod)
      implicit none
      real*4 dmat(3,3), xmat(3,3), ymat(3,3), zmat(3,3), prod(3,3)
      call mat3dmul(dmat, zmat, prod)
      call mat3dmul(prod, ymat, prod)
      call mat3dmul(prod, xmat, prod)
      return
      end

c       Fill the 3x3 matrices for rotation about X, Y, or Z axis or for the 
c       distortion terms.  Note that unlike tiltalign funct, all matrices are
c       3x3 and they are indexed as row,column not as 1-d across the rows
c
      subroutine fillXmatrix(xmat, angle, calf, salf)
      implicit none
      real*4 xmat(3,3), angle, calf, salf
      call zeroMatrix(xmat)
      calf = cos(angle)
      salf = sin(angle)
      xmat(1,1) = 1.
      xmat(2,2) = calf
      xmat(3,2) = salf
      xmat(3,3) = calf
      xmat(2,3) = -salf
      return
      end

      subroutine fillYmatrix(ymat, angle, cbet, sbet)
      implicit none
      real*4 ymat(3,3), angle, cbet, sbet
      call zeroMatrix(ymat)
      cbet = cos(angle)
      sbet = sin(angle)
      ymat(2,2) = 1.
      ymat(1,1) = cbet
      ymat(1,3) = sbet
      ymat(3,3) = cbet
      ymat(3,1) = -sbet
      return
      end

      subroutine fillZmatrix(zmat, angle, cgam, sgam)
      implicit none
      real*4 zmat(3,3), angle, cgam, sgam
      call zeroMatrix(zmat)
      cgam = cos(angle)
      sgam = sin(angle)
      zmat(3,3) = 1.
      zmat(1,1) = cgam
      zmat(2,1) = sgam
      zmat(2,2) = cgam
      zmat(1,2) = -sgam
      return
      end

c       This a model of semi-natural params, divided by 2
c
      subroutine fillDistMatrix(dmat, gmag, comp, dmag, skew, cdel, sdel)
      implicit none
      real*4 dmat(3,3), gmag, comp, dmag, skew, cdel, sdel
      call zeroMatrix(dmat)
      cdel = cos(skew)
      sdel = sin(skew)
      dmat(1,1)=(gmag+dmag)*cdel
      dmat(2,1)=-(gmag+dmag)*sdel
      dmat(1,2)=-(gmag-dmag)*sdel
      dmat(2,2)=(gmag-dmag)*cdel
      dmat(3,3)=gmag*comp
      return
      end

c       Zero out a matrix
c
      subroutine zeroMatrix(amat)
      implicit none
      real*4 amat(9)
      integer*4 i
      do i = 1, 9
        amat(i) = 0.
      enddo
      return
      end

c       Multiples 3x3 matrix a1 times a2 ( a1 applied first) and places result
c       in a3, which can be the same as a1
c
      subroutine mat3dmul(a1,a2,a3)
      real*4 a1(3,3),a2(3,3),a3(3,3),at(3,3)
c	write(*,'(a,9f8.4)')'a1:',((a1(i,j),j=1,3),i=1,3)
c	write(*,'(a,9f8.4)')'a2:',((a2(i,j),j=1,3),i=1,3)
      do i=1,3
        do j=1,3
          at(i,j)=0.
          do k=1,3
            at(i,j)=at(i,j)+a2(i,k)*a1(k,j)
          enddo
        enddo
      enddo
      do i=1,3
        do j=1,3
          a3(i,j)=at(i,j)
        enddo
      enddo
c	write(*,'(a,9f8.4)')'a3:',((a3(i,j),j=1,3),i=1,3)
      return
      end


c       This routine takes values representing the amount that an upper piece
c       is displaced from alignment across an edge, and resolves it to an 
c       amount to be applied to each volume (shift, rotation, etc).
c
      subroutine resolveEdgesToVolumes(edgeArray, maxcols, volArray, numCols,
     &    icolStride, ivolStride)
      use stitchvars
      implicit none
      integer*4 maxcols, numCols, icolStride, ivolStride
      real*4 edgeArray(maxcols,*),volArray(*), sum
      integer*4 ivar, iexy, j, m, iv, jv

c       The shifts in coordinates correspond to amount upper is displaced
c       from alignment, as opposed to amount it needs to be shifted to be in
c       alignment
      do ivar = 1, numVols - 1
        do m = 1, numVols - 1
          daa(ivar,m) = 0.
        enddo
        do j = 1, numCols
          dbb(ivar,j) = 0
        enddo

        do iexy = 1, 2
c           
c           If this is an upper piece, find lower piece and edge, add 1 to
c           its term and subtract 1 from lower piece's term.  Subtract 
c           displacements from the constant term
          iv = iVolLower(iexy,ivar)
          if (iv .gt. 0) then
            daa(ivar,ivar) = daa(ivar,ivar) + 1
            if (iv .ne. numVols) then
              daa(ivar,iv) = daa(ivar,iv) - 1
            else
              do m = 1, numVols - 1
                daa(ivar,m) = daa(ivar,m) + 1
              enddo
            endif
            j = indVector(iexy,iv)
            do m = 1, numCols
              dbb(ivar,m) = dbb(ivar,m) - edgeArray(m,j)
            enddo
          endif
c           
c           For a lower piece, set up coefficients the same but add the
c           displacement.
          jv = iVolUpper(iexy,ivar)
          if (jv .gt. 0) then
            daa(ivar,ivar) = daa(ivar,ivar) + 1
            if (jv .ne. numVols) then
              daa(ivar,jv) = daa(ivar,jv) - 1
            else
              do m = 1, numVols - 1
                daa(ivar,m) = daa(ivar,m) + 1
              enddo
            endif
            j = indVector(iexy,ivar)
            do m = 1, numCols
              dbb(ivar,m) = dbb(ivar,m) + edgeArray(m,j)
            enddo
          endif
        enddo
c         write(*,'(8f6.0,3f10.2)')(daa(ivar,j),j=1,numVols-1),(dbb(ivar,j),j=1,3)
      enddo
c       
c       Get the solution and store the shifts
      call gaussjd(daa, numVols - 1, maxvols, dbb, numCols, maxvols, 7)
c       print *,'Solution:'
c       write(*,'(8f6.3,3f10.2)')((daa(ivar,j),j=1,numVols-1),(dbb(ivar,j),j=1,3)
c     &     ,ivar=1,numVols-1)
      do j = 1, numCols
        sum = 0
        do iv = 1, numVols - 1
          volArray(1 + (j - 1) * icolStride + (iv-1) * ivolStride) = dbb(iv,j)
          sum = sum - dbb(iv,j)
        enddo
        volArray(1 + (j - 1) * icolStride + (numVols - 1) * ivolStride) = sum
      enddo
      return
      end


c       Transforms vector positions and lengths for the initial rotation and
c       shift of the volumes, and computes corresponding positions in the two
c       volumes to be used in the search.
c
      subroutine transformVectors(rmat, volShift, edgeShift)
      use stitchvars
      implicit none
      real*4 rmat(3,3,*), volShift(3,*), edgeShift(3,*), ysum
      integer*4 iv, iexy, jv, j, k, kstr, kend, i, m
      do iv = 1, numVols
        do iexy = 1,2
          jv = iVolUpper(iexy,iv)
          if (jv .gt. 0) then
            j = indVector(iexy,iv)
            kstr = istrVector(j)
            kend = kstr + numVectors(j) - 1
            do k = kstr, kend
                call transformPos(center(1,k), rmat(1,1,iv), volShift(1,iv),
     &              nxyzin(1,iv), nxyzout, cenRot(1,k))
              do i = 1, 3
                ysum = 0.
                do m = 1,3
                  ysum = ysum + rmat(i,m,iv) * vector(m,k)
                enddo
                vecRot(i,k) = ysum + edgeShift(i,j) + volShift(i,jv) -
     &              volShift(i,iv) + intervals(iexy,i)
                poslo(i,k) = scalexyz * (cenRot(i,k) - nxyzout(i) / 2.)
                posup(i,k) = poslo(i,k) + scalexyz *
     &              (vecRot(i,k) - intervals(iexy,i))
c                 if (iv.eq.1.and.iexy.eq.2. .and. k .lt. kstr+5) print *,i,
c     &                center(i,k),vector(i,k),cenrot(i,k),vecrot(i,k),
c     &                poslo(i,k)/scalexyz,posup(i,k)/scalexyz, intervals(iexy,i),
c     &                ysum,edgeShift(i,j),volShift(i,iv),volShift(i,jv),
c     &              (rmat(i,m,iv),m=1,3)
              enddo
            enddo
          endif
        enddo
      enddo
      return
      end


c       Scales the results from a minimization and reports the results
c
      subroutine scaleAndReport(ixPiece, iyPiece)
      use stitchvars
      implicit none
      integer*4 ixPiece(*), iyPiece(*)
      integer*4 iexy, j, m, iv, jv, ivf, jvf, nsum,kstr, kend, k, i
      real*8 dsum, dsumsq
      real*4 distMax,avg,sd,dist
c         
c       Scale radians to degrees and unscale the dxyz
      do iv = 1, numSolve
        do j = 4, 7
          volGeomVars(iv,j) = volGeomVars(iv,j) / dtor
        enddo
      enddo
      do iv = 1, numSolve
        do j = 1, 3
          dxyz(iv,j) = dxyz(iv,j) / scalexyz
        enddo
      enddo
      write(6,102)
102   format(/,' ix iy Alpha   Beta  Gamma    Mag    Comp    Dmag     Skew',
     &    '    Dx     Dy     Dz')
      write(6,103)(ixPiece(iVolFull(iv)), iyPiece(iVolFull(iv)),
     &    alpha(iv), beta(iv), gamma(iv), gmag(iv), comp(iv),
     &    dmag(iv), skew(iv), (dxyz(iv,i), i=1,3),iv = 1, numSolve)
103   format(2i3,f6.2,2f7.2,3f8.4,f8.2,3f7.1)
c       
c       Get mean residuals
      dsum = 0.
      dsumsq = 0.
      nsum = 0.
      distMax = 0.
      do iv = 1, numSolve
        ivf = iVolFull(iv)
        do iexy = 1,2
          jvf = iVolUpper(iexy,ivf)
          if (jvf .gt. 0 .and. iVolSolve(jvf) .gt. 0) then
            j = indVector(iexy,ivf)
            kstr = istrVector(j)
            kend = kstr + numVectors(j) - 1
            do k = kstr, kend
              dist = sqrt(resid(1,k)**2 + resid(2,k)**2 + resid(3,k)**2) /
     &            scalexyz
              dsum = dsum + dist
              dsumsq = dsumsq + dist**2
              nsum = nsum + 1
              distMax = max(distMax, dist)
            enddo
          endif
        enddo
      enddo
      call sums_to_avgsd8(dsum, dsumsq, nsum, 1, avg,sd)
      write(*,107)avg,sd,distMax
107   format(/,'Residual error mean',f8.2,', SD:',f8.2,', max:',2f8.1)
      return
      end


c       Determines properties of the grid of vectors in the original volume 
c       for edge IEDGE.
c
      subroutine setVectorGrid(iedge)
      use stitchvars
      implicit none
      integer maxpos, iedge, lstr, lend, l, j, i, k, ipos, inlist, itmp
      integer*4 intmin, numPos, ix, iy, iz
      real*4 span, delmax, delta
      parameter (maxpos = 1000)
      integer*4 npatxyz(3), listpos(maxpos,3)

c       make lists of X, Y, Z values
      lstr = istrVector(iedge)
      lend = lstr + numVectors(iedge) - 1
      npatxyz(1) = 0
      npatxyz(2) = 0
      npatxyz(3) = 0
      do l = lstr, lend
        do j = 1, 3
          ipos = nint(center(j,l))
          inlist=0
          k=1
          do while(inlist.eq.0.and.k.le.npatxyz(j))
            if(ipos.eq.listpos(k,j))inlist=k
            k=k+1
          enddo
          if(inlist.eq.0)then
            if(npatxyz(j) .gt. maxpos)call exitError(
     &          'TOO MANY PATCH POSITIONS ALONG ONE AXIS FOR ARRAYS')
            npatxyz(j)=npatxyz(j)+1
            listpos(npatxyz(j),j)=ipos
          endif
        enddo
      enddo
c
      do i=1,3
c       
c       sort the position list
c       
        do j=1,npatxyz(i)-1
          do k=j,npatxyz(i)
            if(listpos(j,i).gt.listpos(k,i))then
              itmp=listpos(j,i)
              listpos(j,i)=listpos(k,i)
              listpos(k,i)=itmp
            endif
          enddo
        enddo
c         
c         Find the minimum interval, the span, and deduce number and interval
        span = listpos(npatxyz(i),i) - listpos(1,i)
        intmin = span
        do j=1,npatxyz(i)-1
          intmin = min(intmin, listpos(j+1,i) - listpos(j,i))
        enddo
        numPos = span / max(1,intMin) + 1
        delta = span / (max(numPos - 1, 1))
c         
c         Maybe somehow the number of positions is too small by 1: so check
c         that all positions are close to the defined spots
        delmax = 0.
        do j=2,npatxyz(i)
          itmp = listpos(j,i) - listpos(1,i)
          ipos = nint(itmp / delta)
          delmax = max(delmax, abs(delta * ipos - itmp))
        enddo
        if (delmax .gt. 0.25 * delta) numPos = numPos + 1
c         
c         Set the parameters for this edge
        numVecGrid(i,iedge) = numPos
        vecGridStart(i,iedge) = listpos(1,i)
        vecGridDelta(i,iedge) = max(1., span / (max(numPos - 1, 1)))
c        print *,'edge',iedge,' axis',i,' start delta num:',vecGridStart(i,
c     &      iedge) ,vecGridDelta(i,iedge), numVecGrid(i,iedge)
      enddo
      numPos = numVecGrid(1,iedge) * numVecGrid(2,iedge) * numVecGrid(3,iedge)
      if (numPos + indG2Vbase .gt. maxvecs) call exitError(
     &    'TOO MANY VECTOR GRID POSITIONS FOR ARRAYS')
      istrVecGrid(iedge) = indG2Vbase
c       
c       Clear out the index
      do i = 1, numPos
        indGridToVec(i + indG2Vbase - 1) = 0
      enddo
c       
c       Go through vectors filling in indices to them
      do l = lstr, lend
        ix = nint((center(1,l) - vecGridStart(1,iedge)) /vecGridDelta(1,iedge))
        iy = nint((center(2,l) - vecGridStart(2,iedge)) /vecGridDelta(2,iedge))
        iz = nint((center(3,l) - vecGridStart(3,iedge)) /vecGridDelta(3,iedge))
        itmp = indG2Vbase + ix + (iy + iz * numVecGrid(2,iedge)) *
     &      numVecGrid(1,iedge)
        indGridToVec(itmp) = l
      enddo
      indG2Vbase = indG2Vbase + numPos
      return
      end


c       Determines the volumes and edges that position XX, YY in volume IVOL
c       falls into.  The number of volumes is placed in numPieces, the number
c       of edges in X and Y in numEdges.  The actual volume numbers are placed
c       in inPiece, the actual edge numbers in inEdge.  The index of the pieces
c       below and above an edge (indexes in the inPiece list, not the actual
c       volume numbers) are stored in inEdLower and inEdUpper, and an edge
c       fraction is returned in edgeFrac4.  Positions within each piece are
c       returned in xInPiece, yInPiece.
c       The position is assumed to lie in the given volume, which will be first
c       on the list.  Another volume is added to the list only if the position
c       falls within the limits of the edge between that volume and an existing
c       volume on the list.  However, if both volumes on the two sides of an
c       edge are on the list alredy, the edge is added regardless.
c
      subroutine countedges(xx, yy, ivol)
      use stitchvars
      implicit none
      real*4 xx,yy,xinp,yinp,xyinp(2)
      integer*4 ivol, iexy, i, j, jv, ifup, iv, indpiece, indinp, indedge
      equivalence (xinp,xyinp(1)), (yinp,xyinp(2))
c      
      numEdges(1) = 0
      numEdges(2) = 0
      numPieces = 1
      xInPiece(1) = xx
      yInPiece(1) = yy
      indinp = 1
      inPiece(1) = ivol
      do while (indinp .le. numPieces)
c         
c         Come into loop looking at next piece on list.
c         Need to check each edge to see if point is in it, then add adjacent
c         piece
        iv = inPiece(indinp)
        do iexy = 1, 2
c           
c           Set values for this being a lower piece and edge is to upper
          ifup = 0
          xinp = xInPiece(indinp)
          yinp = yInPiece(indinp)
          jv = iVolUpper(iexy,iv)
          if (jv .ne. 0) j = indVector(iexy,iv)
          if (xyinp(iexy) .lt. nxyzout(iexy) / 2) then
c             
c             But if coordinate is low, check for lower piece instead
            ifup = 1
            jv = iVolLower(iexy,iv)
            if (jv .ne. 0) j = indVector(iexy,jv)
            xinp = xinp + intervals(iexy,1)
            yinp = yinp + intervals(iexy,2)
          endif
          if (jv .ne. 0) then
c
c           See if piece is on list already
            indpiece = 0
            do i = 1, numPieces
              if (jv .eq. inPiece(i)) indpiece = i
            enddo
c
c           Accept edge if in the edge or the other piece is on list already
            if (indpiece .gt. 0 .or. (xinp .ge. edgeMin(1,j)
     &        .and. xinp .le. edgeMax(1,j) .and. yinp .ge. edgeMin(2,j) .and.
     &        yinp .le. edgeMax(2,j))) then
c               
c               In an edge.  
c               Add piece if not already on list, and position in piece
              if (indpiece .eq. 0) then
                numPieces = numPieces + 1
                indpiece = numPieces
                inPiece(indpiece) = jv
                if (ifup .eq. 0) then
                  xInPiece(indpiece) = xinp - intervals(iexy,1)
                  yInPiece(indpiece) = yinp - intervals(iexy,2)
                else
                  xInPiece(indpiece) = xinp
                  yInPiece(indpiece) = yinp
                endif
              endif
c             
c               Add edge if not already on list
              indedge = 0
              do i = 1, numEdges(iexy)
                if (inedge(i,iexy) .eq. j) indedge = i
              enddo
              if (indedge .eq. 0) then
                numEdges(iexy) = numEdges(iexy) + 1
                indedge = numEdges(iexy)
                inEdge(indedge,iexy) = j
                if (ifup .eq. 0) then
                  inEdLower(indedge,iexy) = indinp
                  inEdUpper(indedge,iexy) = indpiece
                else
                  inEdLower(indedge,iexy) = indpiece
                  inEdUpper(indedge,iexy) = indinp
                endif
c               
c                 Get the edge fraction based on position in band that this
c                 point is in along the edge
                i = (xyinp(3-iexy) - edgeMin(3-iexy,j)) / bandDel(j) + 1.
                i = max(1, min(numBands(j), i))
                if (bandMax(i,j) .gt. bandMin(i,j)) then
                  edgefrac4(indedge,iexy) = (xyinp(iexy) - bandMin(i,j)) /
     &                (bandMax(i,j) - bandMin(i,j))
                else
                  edgefrac4(indedge,iexy) = (xyinp(iexy) - edgeMin(iexy,j)) /
     &                (edgeMax(iexy,j) - edgeMin(iexy,j))
                endif
              endif
            endif
          endif
        enddo
        indinp = indinp + 1
      enddo
      if (numPieces .eq. 4 .and. numEdges(1) + numEdges(2) .ne. 4) print *,
     &    'WARNING: Incomplete edge list:',xx,yy,numEdges(1),numEdges(2)
c      if (numPieces .gt. 1) print *,xx,yy,numPieces,numEdges(2),(inPiece(i),
c     &    xinpiece(i),yinpiece(i),i=1,2),edgefrac4(1,2)
      return
      end


c       Finds a warping vector at the position in the output volume given in
c       the 3-element array IPOS, for volume IVOL.  The vector is returned in 
c       OUTVEC and NDIM is returned with the number of dimension used for
c       interpolating the vector, or 0 if there is no vector available.
c       The routine assumes that COUNTEDGES has already been run on this 
c       position in X and Y.  This routine is based as far as possible on
c       the section of Blendmont that resolves edge functions for getting a
c       pixel.
c
      subroutine findWarpVector(ipos, ivol, outvec, ndim)
      use stitchvars
      implicit none
      integer*4 ipos(*), ivol, ndim
      real*4 outvec(*)
      real*4 pos(3), xpos, ypos, zpos, vec(3)
      equivalence (pos(1),xpos), (pos(2),ypos), (pos(3),zpos)
      integer*4 indp1,indp2,indp3,indp4,inde12,inde13,inde34,inde24,nactivep
      real*4 er3,eb3,el4,eb4,fx,fy,dr1,dt1,dl2,dt2,dr3,db3,dl4,db4,dla,dra
      real*4 dba,dta,ax,ay,ex,ey,wll,wlr,wul,wur,wsum,x1,y1,w1,w2,c11,c12,c21
      real*4 xgconst,ygconst,x1last,y1last,z1last
      integer*4 indpidef,indpl,indpu,ipiece1,ipiece2,iter,jndp1,jndp2,jndp3,ipc
      real*4 dx2,dy2,x2,y2,w3
      integer*4 ind12edg,ind13edg,icortyp,ind23edg,ind14edg,ind43edg,ixy,ied
      real*4 x3,y3,x3t,y3t,z3t
      real*4 dy3,dx3,bx,by,emin,w4
      integer*4 ipiece3,ipiece4,nxgr,nygr,indbray1,indbray2,jndp4,ndim2,ndim3
      integer*4 nedgesum,iedge,indlower,interval12x, interval12y,interval13x
      integer*4 interval13y, interval14x, interval14y, interval23x, interval23y
      integer*4 niter, indedg, i, interval43x, interval43y,ixdbg,iydbg
      real*4 x4,y4,dx4,dy4,f34,f12,f13,f24, er1,et1,el2,et2
      real*4 x1234(4), y1234(4), z1234(4), z1, z2, z3, z4, dz2, dz3, dz4
      equivalence (x1234(1),x1), (x1234(2),x2), (x1234(3),x3), (x1234(4),x4)
      equivalence (y1234(1),y1), (y1234(2),y2), (y1234(3),y3), (y1234(4),y4)
      equivalence (z1234(1),z1), (z1234(2),z2), (z1234(3),z3), (z1234(4),z4)
      logical*4 active4(2,2),debug

      ixdbg = -1
      iydbg=-1
      debug = ipos(1) .eq. ixdbg .and. ipos(2) .eq. iydbg
      do i = 1, 3
        pos(i) = ipos(i)
        outvec(i) = 0.
      enddo
      niter = 10
c       
c       look at each edge, make sure there is a non-extended vector available
c       and if not, set edge fraction to limit and make the edge inactive
      nedgesum=0
      do ixy=1,2
        do ied=1,numedges(ixy)
          iedge=inedge(ied,ixy)
          indlower=inedlower(ied,ixy)
          call interpolateVector(xInPiece(indlower), yInPiece(indlower), zpos,
     &        inPiece(indlower), iedge, 0.7, 0, vec, ndim)
c     &        inPiece(indlower), iedge, 0.5, nIniExtend(ied,ixy), vec, ndim)
c          print *,xpos,ypos,zpos,inPiece(indlower), iedge,(vec(i),i=1,3), ndim
          if (ndim .eq. 0 .or. nIniExtend(ied,ixy) .eq. 0) then
c          if (ndim .eq. 0) then
            if (edgefrac4(ied,ixy) .lt. 0.5) then
              edgefrac4(ied,ixy) = 0.
            else
              edgefrac4(ied,ixy) = 1.
            endif
          endif
          
          active4(ied,ixy)=edgefrac4(ied,ixy).lt..999
     &        .and.edgefrac4(ied,ixy).gt..001
          if(active4(ied,ixy)) nedgesum=nedgesum+1
          if(edgefrac4(ied,ixy).lt.0.)edgefrac4(ied,ixy)=0.
          if(edgefrac4(ied,ixy).gt.1.)edgefrac4(ied,ixy)=1.
        enddo
      enddo
      if (debug) print *,'fractions',numpieces,nedgesum,((edgefrac4(ied,ixy),
     &    ied=1,2), ixy=1,2)
c       
c       get indices of pieces and edges: for now, numbers
c       1 to 4 represent lower left, lower right, upper left,
c       and upper right
c       
      if(numpieces.gt.1)then
        indp1=0                                 !index of piece 1, 2, 3, or 4
        indp2=0                                 !if the point is in them
        indp3=0
        indp4=0
        inde12=0                                !index of edges between 1 and 2
        inde13=0                                !1 and 3, etc
        inde34=0
        inde24=0
        f12=0.                                  !edge fractions between 1 and 2
        f13=0.                                  !1 and 3, etc
        f34=0.
        f24=0.
        er1=0.                                  !end fractions to right and top
        et1=0.                                  !of piece 1, left and bottom of
        el2=0.                                  !piece 3, etc
        et2=0.
        er3=0.
        eb3=0.
        el4=0.
        eb4=0.
        fx=0.                                   !the composite edge fractions
        fy=0.                                   !in x and y directions
        if(numpieces.gt.2)then
          do ipc=1,numpieces
            if(xinpiece(ipc).gt.nxout/2.and.
     &          yinpiece(ipc).gt.nyout/2)indp1=ipc
            if(xinpiece(ipc).gt.nxout/2.and.
     &          yinpiece(ipc).lt.nyout/2)indp3=ipc
            if(xinpiece(ipc).lt.nxout/2.and.
     &          yinpiece(ipc).gt.nyout/2)indp2=ipc
            if(xinpiece(ipc).lt.nxout/2.and.
     &          yinpiece(ipc).lt.nyout/2)indp4=ipc
          enddo
          do ied=1,numedges(1)
            if(inedlower(ied,1).eq.indp1)inde12=ied
            if(inedlower(ied,1).eq.indp3)inde34=ied
          enddo
          do ied=1,numedges(2)
            if(inedlower(ied,2).eq.indp1)inde13=ied
            if(inedlower(ied,2).eq.indp2)inde24=ied
          enddo
          if(inde12.ne.0)f12=edgefrac4(inde12,1)
          if(inde34.ne.0)f34=edgefrac4(inde34,1)
          if(inde13.ne.0)f13=edgefrac4(inde13,2)
          if(inde24.ne.0)f24=edgefrac4(inde24,2)
        else
c           
c           two piece case - identify as upper or lower to
c           simplify computation of end fractions
c           
          if(numedges(1).gt.0)then
            fx=edgefrac4(1,1)
            outvec(4) = 1
            if(0.5*(yinpiece(1)+yinpiece(2)).gt.nyout/2)then
              inde12=1
              if(xinpiece(1).gt.xinpiece(2))then
                indp1=1
                indp2=2
              else
                indp1=2
                indp2=1
              endif
            else
              inde34=1
              fy=1.
              if(xinpiece(1).gt.xinpiece(2))then
                indp3=1
                indp4=2
              else
                indp3=2
                indp4=1
              endif
            endif
          else
            fy=edgefrac4(1,2)
            outvec(4) = 2
            if(0.5*(xinpiece(1)+xinpiece(2)).gt.nxout/2)then
              inde13=1
              if(yinpiece(1).gt.yinpiece(2))then
                indp1=1
                indp3=2
              else
                indp1=2
                indp3=1
              endif
            else
              inde24=1
              fx=1.
              if(yinpiece(1).gt.yinpiece(2))then
                indp2=1
                indp4=2
              else
                indp2=2
                indp4=1
              endif
            endif
          endif
        endif
c         
c         get distance to top, right, bottom, or left edges
c         as needed for each piece, and compute end fractions
c         
        if(indp1.gt.0)then
          dr1 = max(0., volLimTR(1, inPiece(indp1)) - xinpiece(indp1))
          dt1 = max(0., volLimTR(2, inPiece(indp1)) - yinpiece(indp1))
          er1=(dr1+1.)/edgeWidthMean
          et1=dt1/edgeWidthMean
        endif			
        if(indp2.gt.0)then
          dl2=max(0., xinpiece(indp2) - volLimTL(1, inPiece(indp2)))
          dt2=max(0., volLimTL(2, inPiece(indp2)) - yinpiece(indp2))
          el2=(dl2+1.)/edgeWidthMean
          et2=(dt2+1.)/edgeWidthMean
        endif			
        if(indp3.gt.0)then
          dr3 = max(0., volLimBR(1, inPiece(indp3)) - xinpiece(indp3))
          db3 = max(0., yinpiece(indp3) - volLimBR(2, inPiece(indp3)))
          er3=(dr3+1.)/edgeWidthMean
          eb3=(db3+1.)/edgeWidthMean
        endif			
        if(indp4.gt.0)then
          dl4=max(0., xinpiece(indp4) - volLimBL(1, inPiece(indp4)))
          db4=max(0., yinpiece(indp4) - volLimBL(2, inPiece(indp4)))
          el4=(dl4+1.)/edgeWidthMean
          eb4=(db4+1.)/edgeWidthMean
        endif
c         
c         If there are 4 pieces, fx and fy are weighted sum
c         of the f's in the two overlapping edges.  The
c         weights ex and ey are modified fractional distances
c         across the overlap zone.  First get the distances
c         to the borders of the overlap zone (dla, etc) and
c         use them to get absolute fractional distances
c         across zone, ax in the y direction and ay in the
c         x direction.  They are used to make ex slide from
c         being a distance across the lower overlap to being
c         a distance across the upper overlap.  This gives
c         continuity with the edge in 2 or 3-piece cases
c         
        if(numpieces.eq.4) then
          dla=min(dl2,dl4)
          dra=min(dr1,dr3)
          dba=min(db3,db4)
          dta=min(dt1,dt2)
          ax=dba/(dta+dba)
          ay=dla/(dra+dla)
          ex=((1-ay)*db3+ay*db4)/
     &        ((1-ay)*(dt1+db3)+ay*(dt2+db4))
          ey=((1-ax)*dl2+ax*dl4)/
     &        ((1-ax)*(dr1+dl2)+ax*(dr3+dl4))
          fx=(1-ex)*f12+ex*f34
          fy=(1-ey)*f13+ey*f24
        elseif(numpieces.eq.3)then
c           
c           Three-piece case is simple, only two edges
c           
          fx=edgefrac4(1,1)
          fy=edgefrac4(1,2)
        endif
c         
c         weighting factors are a product of the two f's,
c         attenuated if necessary by fractional distance to
c         end of piece, then normalized to sum to 1.
c         
        wll=min(1-fx,et1)*min(1-fy,er1)
        wlr=min(fx,et2)*min(1-fy,el2)
        wul=min(1-fx,eb3)*min(fy,er3)
        wur=min(fx,eb4)*min(fy,el4)
        wsum=wll+wlr+wul+wur
        if(wsum.gt.0.)then
          wll=wll/wsum
          wlr=wlr/wsum
          wul=wul/wsum
          wur=wur/wsum
        endif
c         
c         count up active pieces implied by the w's
c         
        nactivep=0
        if(wll.gt.0.)nactivep=nactivep+1
        if(wlr.gt.0.)nactivep=nactivep+1
        if(wul.gt.0.)nactivep=nactivep+1
        if(wur.gt.0.)nactivep=nactivep+1
        if (debug) print *,'weights',nactivep, wll,wlr,wul,wur
c         
c         filter out cross-corner 2-piece cases, pick the
c         piece where the point is most interior
c         
        if(nactivep.eq.2.and.wll*wur.gt.0.)then
          if(min(dt1,dr1).lt.min(db4,dl4))then
            wll=0.
          else
            wur=0.
          endif
          nactivep=1
        elseif(nactivep.eq.2.and.wul*wlr.gt.0.)then
          if(min(dt2,dl2).lt.min(db3,dr3))then
            wlr=0.
          else
            wul=0.
          endif
          nactivep=1
        endif			    
      else
c         
c         the one-piece case, avoid all that computation
c         
        nactivep=1
        indp1=1
        wll=1.
      endif
c       
c       NOW SORT OUT THE CASES OF 1, 2, 3 or 4 PIECES
c       
      if(nactivep.le.1)then                     !ONE PIECE
        if(wll.gt.0.)then
          indpidef=indp1
        elseif(wlr.gt.0.)then
          indpidef=indp2
        elseif(wul.gt.0.)then
          indpidef=indp3
        else
          indpidef=indp4
        endif
c        if(nactivep.eq.0)indpidef=1
c         
c         Not sure how there can be no active pieces, but it seems safer to
c         return no vector than a zero vector there
        ndim = 3
        outvec(4) = 0
        if (indpidef .ne. 1 .or. nactivep .eq. 0) ndim = 0
        return
c         
c         ONE EDGE, TWO PIECES
c         
      elseif(nactivep.eq.2)then
c         
c         find the pieces around the edge, set edge index
c         
        interval12x = 0
        interval12y = 0
        if(wll.gt.0..and.wlr.gt.0.)then
          indpl=indp1
          indpu=indp2
          w1=wll
          indedg=inEdge(inde12,1)
        elseif(wul.gt.0..and.wur.gt.0.)then
          indpl=indp3
          indpu=indp4
          w1=wul
          indedg=inEdge(inde34,1)
        elseif(wll.gt.0..and.wul.gt.0.)then
          indpl=indp1
          indpu=indp3
          w1=wll
          indedg=inEdge(inde13,2)
        else
          indpl=indp2
          indpu=indp4
          w1=wlr
          indedg=inEdge(inde24,2)
        endif
c         
c         set up pieces #'s, and starting coords
c         
        ipiece1=inpiece(indpl)
        ipiece2=inpiece(indpu)
        x1=xinpiece(indpl)
        y1=yinpiece(indpl)
        z1=zpos
        w2=1.-w1
c         
c         in this loop, we only need the position in lower piece and the
c         vector, iterate until weighted mean of position in lower and
c         upper piece stabilizes
c         
        x1last=-100.
        y1last=-100.
        z1last=-100.
        iter=1
        do while(iter.le.niter.and.(abs(x1-x1last) .gt.0.01 .or.
     &      abs(y1-y1last).gt.0.01 .or. abs(z1-z1last).gt.0.01))
          call interpolateVector(x1, y1, z1, ipiece1, indedg, 0.7, 0, vec, 
     &        ndim)
          if (ndim .eq. 0) return
          x1last=x1
          y1last=y1
          z1last=z1
          x1=xinpiece(indpl) - w2*vec(1)
          y1=yinpiece(indpl) - w2*vec(2)
          z1=zpos - w2*vec(3)
          iter=iter+1
        enddo
c        
c         
        if (indpl .eq. 1) then
          w3 = -w2
        else if (indpu .eq. 1) then
          w3 = w1
        else
          ndim = 0
          return
        endif
        do i = 1, 3
          outvec(i) = w3 * vec(i)
        enddo
c        write(*,'(i3,9f8.1)')2,xinpiece(1),yinpiece(1),zpos,
c     &      w1,w2,w3,(outvec(i),i=1,3)
c         
c         THREE PIECES AND TWO EDGES
c         
      elseif(nactivep.eq.3)then
c         
c         now decide between the three cases of divergent,
c         serial, or convergent functions, and assign pieces
c         and weights accordingly
c         
c         DIVERGENT: if lower piece is same for x and y edge
c         
        if(wur.le.0.)then
          w1=wll
          w2=wlr
          w3=wul
          jndp1=indp1
          jndp2=indp2
          jndp3=indp3
          ind12edg=inedge(inde12,1)
          ind13edg=inedge(inde13,2)
          icortyp=1
          interval12x = intervalX
          interval12y = 0
          interval13x = 0
          interval13y = intervalY
          outvec(4) = 3
c           
c           CONVERGENT: if upper piece same for x and y edge
c           
        elseif(wll.le.0.)then
          w3=wur
          w1=wul
          w2=wlr
          jndp1=indp3
          jndp2=indp2
          jndp3=indp4
          ind13edg=inedge(inde34,1)
          ind23edg=inedge(inde24,2)
          icortyp=2
          interval23x = 0
          interval23y = intervalY
          interval13x = intervalX
          interval13y = 0
          interval12x = intervalX
          interval12y = -intervalY
          outvec(4) = 4
c           
c           SERIAL: lower of one is the upper of the other
c           
        else
          w1=wll
          w3=wur
          jndp1=indp1
          jndp3=indp4
          if(wlr.le.0.)then
            w2=wul
            jndp2=indp3
            ind12edg=inEdge(inde13,2)
            ind23edg=inEdge(inde34,1)
            interval12x = 0
            interval12y = intervalY
            interval23x = intervalX
            interval23y = 0
            interval13x = intervalX
            interval13y = intervalY
            outvec(4) = 5
          else
            w2=wlr
            jndp2=indp2
            ind12edg=inEdge(inde12,1)
            ind23edg=inEdge(inde24,2)
            interval12x = intervalX
            interval12y = 0
            interval23x = 0
            interval23y = intervalY
            interval13x = intervalX
            interval13y = intervalY
            outvec(4) = 6
          endif
          icortyp=3
        endif
        
        ipiece1=inpiece(jndp1)
        ipiece2=inpiece(jndp2)
        ipiece3=inpiece(jndp3)
        x1=xinpiece(jndp1)
        y1=yinpiece(jndp1)
        z1 = zpos
        x2=xinpiece(jndp2)
        y2=yinpiece(jndp2)
        z2 = zpos
c         
c         set up to solve equations for new (x1,y1), (x2,y2)
c         and (x3,y3) given the differences between them and
c         the desired weighted coordinate
c         
        xgconst = x1 - w2 * interval12x - w3 * interval13x
        ygconst = y1 - w2 * interval12y - w3 * interval13y
c         
c         do iteration, starting with coordinates and solving
c         for new coordinates until convergence
c         
        x1last=-100.
        y1last=-100.
        z1last=-100.
        iter=1
        do while(iter.le.niter.and.(abs(x1-x1last) .gt.0.01 .or.
     &      abs(y1-y1last).gt.0.01 .or. abs(z1-z1last).gt.0.01))
          if(icortyp.eq.1)then
c             
c             divergent case
c             
            call interpolateVector(x1, y1, z1, ipiece1, ind12edg, 0.7, 0, vec, 
     &          ndim)
            x2 = x1 + vec(1) - interval12x
            y2 = y1 + vec(2) - interval12y
            z2 = z1 + vec(3)
            call interpolateVector(x1, y1, z1, ipiece1, ind13edg, 0.7, 0, vec,
     &          ndim2)
            x3 = x1 + vec(1) - interval13x
            y3 = y1 + vec(2) - interval13y
            z3 = z1 + vec(3)

          elseif(icortyp.eq.2)then
c             
c             convergent case
c             
            call interpolateVector(x1, y1, z1, ipiece1, ind13edg, 0.7, 0, vec, 
     &          ndim)
            x3 = x1 + vec(1) - interval13x
            y3 = y1 + vec(2) - interval13y
            z3 = z1 + vec(3)
            call interpolateVector(x2, y2, z2, ipiece2, ind23edg, 0.7, 0, vec, 
     &          ndim2)
            x3t = x2 + vec(1) - interval23x
            y3t = y2 + vec(2) - interval23y
            z3t = z2 + vec(3)
            x2=x2+x3-x3t
            y2=y2+y3-y3t
            z2=z2+z3-z3t
          else
c             
c             serial case
c             
            call interpolateVector(x1, y1, z1, ipiece1, ind12edg, 0.7, 0, vec, 
     &          ndim)
            x2 = x1 + vec(1) - interval12x
            y2 = y1 + vec(2) - interval12y
            z2 = z1 + vec(3)
c            write(*,'(9f8.1)')x1, y1, z1,(vec(i),i=1,3),x2, y2, z2
            call interpolateVector(x2, y2, z2, ipiece2, ind23edg, 0.7, 0, vec, 
     &          ndim2)
            x3 = x2 + vec(1) - interval23x
            y3 = y2 + vec(2) - interval23y
            z3 = z2 + vec(3)
c            write(*,'(9f8.1)')(vec(i),i=1,3),x3, y3, z3
          endif
          if (ndim .eq. 0 .or. ndim2 .eq. 0) then
            ndim = 0
            return
          endif
c           
c           solve equations for new coordinates
c           
          x1last=x1
          y1last=y1
          z1last=z1
          dx2=x2-x1
          dy2=y2-y1
          dz2=z2-z1
          dx3=x3-x1
          dy3=y3-y1
          dz3=z3-z1
          x1=xgconst-w2*dx2-w3*dx3
          y1=ygconst-w2*dy2-w3*dy3
          z1=zpos-w2*dz2-w3*dz3
          x2=x1+dx2
          y2=y1+dy2
          z2=z1+dz2
          x3=x1+dx3
          y3=y1+dy3
          z3=z1+dz3
          iter=iter+1
        enddo
        if (jndp1 .eq. 1) then
          indp1 = 1
        else if (jndp2 .eq. 1) then
          indp1 = 2
        else if (jndp3 .eq. 1) then
          indp1 = 3
        else
          ndim = 0
          return
        endif
c         
c         Hope the sign is right here
        outvec(1) = x1234(indp1) - xInPiece(1)
        outvec(2) = y1234(indp1) - yInPiece(1)
        outvec(3) = z1234(indp1) - zpos
c        write(*,'(2i3,9f8.1)')3,icortyp,xinpiece(1),yinpiece(1),zpos,
c     &      x1234(indp1),y1234(indp1),z1234(indp1),(outvec(i),i=1,3)
      else
c         
c         FOUR PIECES, THREE EDGES USED FOR SOLUTION
c         
c         First, need to have only 3 active edges, so if
c         there are four, knock one out based on ex and ey
c         
        if(nedgesum.eq.4)then
          emin=min(ex,1.-ex,ey,1.-ey)
          if(ex.eq.emin)then
            active4(inde34,1)=.false.
          elseif(1.-ex.eq.emin)then
            active4(inde12,1)=.false.
          elseif(ey.eq.emin)then
            active4(inde24,2)=.false.
          else
            active4(inde13,2)=.false.
          endif
        endif
c         
c         here there is always a serial chain from ll to ur,
c         through either ul or lr, and in each case the
c         fourth piece is either divergent from the first or
c         converging on the fourth
c         
        w1=wll
        w3=wur
        interval13x = intervalX
        interval13y = intervalY
        if(.not.active4(inde12,1).or.
     &      .not.active4(inde24,2))then
          w2=wul
          w4=wlr
          jndp2=indp3
          jndp4=indp2
          ind12edg=inEdge(inde13,2)
          ind23edg=inEdge(inde34,1)
          if(.not.active4(inde24,2))then
            ind14edg=inEdge(inde12,1)
            icortyp=1                           !divergent
            outvec(4) = 7
          else
            ind43edg=inEdge(inde24,2)
            icortyp=2                           !convergent
            outvec(4) = 8
          endif
          interval12x = 0
          interval12y = intervalY
          interval23x = intervalX
          interval23y = 0
          interval14x = intervalX
          interval14y = 0
          interval43x = 0
          interval43y = intervalY
        else
          w2=wlr
          w4=wul
          jndp2=indp2
          jndp4=indp3
          ind12edg=inEdge(inde12,1)
          ind23edg=inEdge(inde24,2)
          if(.not.active4(inde34,1))then
            ind14edg=inEdge(inde13,2)
            icortyp=1
            outvec(4) =9
          else
            ind43edg=inEdge(inde34,1)
            icortyp=2
            outvec(4) = 10
          endif
          interval12x = intervalX
          interval12y = 0
          interval23x = 0
          interval23y = intervalY
          interval14x = 0
          interval14y = intervalY
          interval43x = intervalX
          interval43y = 0
        endif
c         
        ipiece1=inpiece(indp1)
        ipiece2=inpiece(jndp2)
        ipiece3=inpiece(indp4)
        ipiece4=inpiece(jndp4)
        x1=xinpiece(indp1)
        y1=yinpiece(indp1)
        z1 = zpos
        x4=xinpiece(jndp4)
        y4=yinpiece(jndp4)
        z4 = zpos
c         
c         set up to solve equations for new (x1,y1), (x2,y2)
c         (x3,y3), and (x4,y4) given the differences between
c         them and the desired weighted coordinate
c         
        xgconst = x1 - w2 * interval12x - w3 * interval13x - w4 * interval14x
        ygconst = y1 - w2 * interval12y - w3 * interval13y - w4 * interval14y
c         
c         do iteration, starting with coordinates and solving
c         for new coordinates until convergence
c         
        x1last=-100.
        y1last=-100.
        z1last=-100.
        iter=1
        do while(iter.le.niter.and.(abs(x1-x1last) .gt.0.01 .or.
     &      abs(y1-y1last).gt.0.01 .or. abs(z1-z1last).gt.0.01))
          call interpolateVector(x1, y1, z1, ipiece1, ind12edg, 0.7, 0, vec, 
     &        ndim)
          x2 = x1 + vec(1) - interval12x
          y2 = y1 + vec(2) - interval12y
          z2 = z1 + vec(3)
          if (debug) write(*,'(2i3,9f8.1)')1,2,x1, y1, z1,(vec(i),i=1,3),x2,
     &        y2, z2
          call interpolateVector(x2, y2, z2, ipiece2, ind23edg, 0.7, 0, vec, 
     &        ndim2)
          x3 = x2 + vec(1) - interval23x
          y3 = y2 + vec(2) - interval23y
          z3 = z2 + vec(3)
          if (debug) write(*,'(2i3,9f8.1)')2,3,(vec(i),i=1,3),x3, y3, z3
          if(icortyp.eq.1)then
c             
c             divergent case
c             
            call interpolateVector(x1, y1, z1, ipiece1, ind14edg, 0.7, 0, vec, 
     &          ndim3)
            x4 = x1 + vec(1) - interval14x
            y4 = y1 + vec(2) - interval14y
            z4 = z1 + vec(3)
            if (debug) write(*,'(2i3,9f8.1)')1,4,(vec(i),i=1,3),x4, y4, z4
          else
c             
c             convergent case
c             
            call interpolateVector(x4, y4, z4, ipiece4, ind43edg, 0.7, 0, vec, 
     &          ndim3)
            x3t = x4 + vec(1) - interval43x
            y3t = y4 + vec(2) - interval43y
            z3t = z4 + vec(3)
            if (debug) write(*,'(2i3,9f8.1)')4,3,x4, y4, z4,(vec(i),i=1,3),
     &          x3t, y3t, z3t
            x4=x4+x3-x3t
            y4=y4+y3-y3t
            z4=z4+z3-z3t
          endif
c           
c           solve equations for new coordinates
c           
          if (ndim * ndim2 * ndim3 .eq. 0) then
            ndim = 0
            return
          endif
          x1last=x1
          y1last=y1
          z1last=z1
          dx2=x2-x1
          dy2=y2-y1
          dz2=z2-z1
          dx3=x3-x1
          dy3=y3-y1
          dz3=z3-z1
          dx4=x4-x1
          dy4=y4-y1
          dz4=z4-z1
          x1=xgconst-w2*dx2-w3*dx3-w4*dx4
          y1=ygconst-w2*dy2-w3*dy3-w4*dy4
          z1=zpos-w2*dz2-w3*dz3-w4*dz4
          x2=x1+dx2
          y2=y1+dy2
          z2=z1+dz2
          x3=x1+dx3
          y3=y1+dy3
          z3=z1+dz3
          x4=x1+dx4
          y4=y1+dy4
          z4=z1+dz4
          iter=iter+1
        enddo
        if (jndp2 .eq. 1) then
          indp1 = 2
        else if (indp4 .eq. 1) then
          indp1 = 3
        else if (jndp4 .eq. 1) then
          indp1 = 4
        endif
c         
c         Hope the sign is right here
        outvec(1) = x1234(indp1) - xInPiece(1)
        outvec(2) = y1234(indp1) - yInPiece(1)
        outvec(3) = z1234(indp1) - zpos
c        write(*,'(2i3,9f8.1)')4,icortyp,xinpiece(1),yinpiece(1),zpos,
c     &      x1234(indp1),y1234(indp1),z1234(indp1),(outvec(i),i=1,3)
      endif
      return
      end


c       Finds a vector at position XPOS, YPOS, ZPOS in volume IVOL and edge
c       IEDGE by interpolation from the transformed edge vectors.  EXTRAPLIM
c       sets the limit for extrapolation; namely the sum of extrapolation
c       fractions in the 3 dimensions must be less than this amount.  Beyond
c       the extrapolation limit, a value is extended in the Z direction by
c       taking the value at the extrapolation limit.  If nExtendXY is greater
c       than 0 it allows extension by up this number of grid positions in X
c       and Y, where the value is taken from the nearest grid position.  This
c       had no merit.  The vector is returned in VEC and NDIM is returned with
c       3 if the vector are determined from a trilinear interpolation, 2 if
c       it is determined yb bilinear interpolation from one layer of vectors
c       in Z, 1 if is is determined by extension, or 0 if no vector is
c       returned.
c
      subroutine interpolateVector(xpos, ypos, zpos, ivol, iedge, extraplim,
     &    nExtendXY, vec, ndim)
      use stitchvars
      implicit none
      integer*4 ivol, iedge, ndim, nExtendXY
      real*4 xpos, ypos, zpos, pos(3), vec(3), extraplim, back(3)
      logical*4 completeCube, completeSquare
      integer*4 ixg,iyg,izg,igrd(3), nxg, nyg, nzg, istr, ix, iy, iz, idir
      equivalence (ixg,igrd(1)), (iyg,igrd(2)), (izg,igrd(3))
      real*4 vgStartX, vgStartY, vgStartZ, vgDeltaX, vgDeltaY, vgDeltaZ
      real*4 fx, fy, fz, fxout, fyout, fzout, fxmax, fxoutmin, dz, dz2
      integer*4 mindir, ind2, i, ind, minind
      real*4 f11, f12, f21, f22, dist, distmin
      integer*4 idx(6) /-1,1,0,0,0,0/, idy(6) /0,0,-1,1,0,0/
      integer*4 idz(6) /0,0,0,0,-1,1/
      logical*4 debug
c       
c       backtransform position to the original volume for looking up in grid
      pos(1) = xpos
      pos(2) = ypos
      pos(3) = zpos
c       debug = xpos .eq. 3213. .and. ypos .eq. 2916.
      debug = .false.
      call transformPos(pos, finvMat(1,1,ivol), finvDxyz(1,ivol), nxyzout,
     &    nxyzin(1,ivol), back)
      nxg = numVecGrid(1,iedge)
      nyg = numVecGrid(2,iedge)
      nzg = numVecGrid(3,iedge)
      istr = istrVecGrid(iedge)
      vgStartX = vecGridStart(1,iedge)
      vgStartY = vecGridStart(2,iedge)
      vgStartZ = vecGridStart(3,iedge)
      vgDeltaX = vecGridDelta(1,iedge)
      vgDeltaY = vecGridDelta(2,iedge)
      vgDeltaZ = vecGridDelta(3,iedge)
      vec(1) = 0.
      vec(2) = 0.
      vec(3) = 0.
c       
c       Get truncated constrained position, which could be lower left corner of
c       cube that point is in
      do i = 1,3
        igrd(i) = min(numVecGrid(i,iedge) - 1, max(1, 1 +
     &      int((back(i) - vecGridStart(i,iedge)) / vecGridDelta(i,iedge))))
      enddo
      if (debug) write(*,'(2i4,6f9.2,3i3)')ivol,iedge,xpos,ypos,zpos,
     &    (back(i),i=1,3),(igrd(i),i=1,3)
      ndim =0
c         
c       Get the interpolation factors and the fractions that the point is
c       outside in each direction
      fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
      fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
      fxout = abs(fx - 0.5) - 0.5
      fyout = abs(fy - 0.5) - 0.5
c       
c       Handle one-layer case
      if (nzg .eq. 1) then
c         
c         Reset iz to 1, set flag, find out if in a complete square
        izg = 1
        if (completeSquare(ixg, iyg, izg, indGridToVec(istr), nxg, nyg)) then
          ndim = 1
          if (fxout .le. 0. .and. fyout .le. 0.) ndim = 2
        endif
      endif
c       
c       Next see if closest cube is complete
      if (ndim .eq. 0) then
        if (completeCube(ixg, iyg, izg, indGridToVec(istr), nxg, nyg)) then
          ndim = 1
          fz = (back(3) - (vgStartZ + (izg - 1) * vgDeltaZ)) / vgDeltaZ
          fzout = abs(fz - 0.5) - 0.5
          if (debug) write(*,'(6f9.4)')fx,fy,fz,fxout,fyout,fzout
          if ((fxout .le. 0. .and. fyout .le. 0.) .or. (max(0., fxout) +
     &        max(0., fyout) + max(0., fzout) .le. extraplim)) ndim = 3
        endif
      endif

      if (ndim .eq. 0) then
        mindir = 0
c         
c         Loop over 6 directions from central cube
        do idir = 1, 6
          ix = ixg + idx(idir)
          iy = iyg + idy(idir)
          iz = izg + idz(idir)
          if (ix .gt. 0 .and. ix .lt. nxg .and. iy .gt. 0 .and. iy .lt. nyg
     &        .and. iz .gt. 0 .and. iz .lt. nzg) then
            if (completeCube(ix, iy, iz, indGridToVec(istr), nxg, nyg)) then
c             
c               If cube is legal and complete, get the interpolation fractions
              fx = (back(1) - (vgStartX + (ix - 1) * vgDeltaX)) / vgDeltaX
              fy = (back(2) - (vgStartY + (iy - 1) * vgDeltaY)) / vgDeltaY
              fz = (back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
c             
c               Get the fractions that they are outside in each direction
              fxout = max(0., abs(fx - 0.5) - 0.5)
              fyout = max(0., abs(fy - 0.5) - 0.5)
              fzout = max(0., abs(fz - 0.5) - 0.5)
              fxmax = fxout + fyout + fzout
              if ((fxout .le. 0. .and. fyout .le. 0.) .or.
     &            fxmax .le. extraplim) then
c                 
c                 If it is within bounds in X/Y, or within bounds in Z and X or
c                 Y and within the extrapolation fraction in Y or X, then it is
c                 an acceptable one, so keep track of one with the maximum
c                 fraction out of bounds
                if (mindir .eq. 0 .or. fxmax .lt. fxoutmin) then
                  fxoutmin = fxmax
                  mindir = idir
                endif
              endif
            endif
          endif
        enddo
c         
c         If one was found, set cube there
        if (mindir .gt. 0) then
          ndim = 3
          ixg = ixg + idx(mindir)
          iyg = iyg + idy(mindir)
          izg = izg + idz(mindir)
        endif
      endif
c       
c       Now if it is still not there, look up and down in Z progressively
c       farther away, but require being in X/Y range
      if (ndim .eq. 0) then 
        fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
        fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
        if (abs(fx - 0.5) .gt. 0.5 .or. abs(fy - 0.5) .gt. 0.5) ndim = 1
        idir = 0
        do while (idir .lt. nzg - 1 .and. ndim .eq. 0)
c           
c           First test squares below and above, pick the closest one if one
c           exists
          iz = izg - idir
          dz = -1.
          if (iz .gt. 0) then
            if (completeSquare(ixg, iyg, iz, indGridToVec(istr), nxg, nyg))
     &          dz = abs(back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
          endif
          iz = izg + 1 + idir
          if (iz .le. nzg) then
            if (completeSquare(ixg, iyg, iz, indGridToVec(istr), nxg, nyg))
     &          then
              dz2 = abs(back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
              if (dz .lt. 0. .or. dz2 .lt. dz) then
                izg = iz
                ndim = 2
              endif
            endif
          endif
          if (ndim .eq. 0 .and. dz .ge. 0.) then
            izg = izg - idir
            ndim = 2
          endif
c           
c           Next test cubes farther away
          if (ndim .eq. 0) then
            dz = -1.
            iz = izg - 2 - idir
            if (iz .gt. 0) then
              if (completeCube(ixg, iyg, iz, indGridToVec(istr), nxg, nyg))
     &            dz = abs(back(3) - (vgStartZ + iz * vgDeltaZ)) / vgDeltaZ
            endif
            iz = izg + 2 + idir
            if (iz .lt. nzg) then
              if (completeCube(ixg, iyg, iz, indGridToVec(istr), nxg, nyg))then
                dz2 = abs(back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) /vgDeltaZ
                if (dz .lt. 0. .or. dz2 .lt. dz) then
                  izg = iz
                  ndim = 2
                endif
              endif
            endif
            if (ndim .eq. 0 .and. dz .ge. 0.) then
              izg = izg - 2 - idir
              ndim = 2
            endif
          endif
          idir = idir + 1
        enddo
      endif
c       
c       Here we are at last with something theoretically usable
      if (ndim .eq. 3) then
        fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
        fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
        fz = (back(3) - (vgStartZ + (izg - 1) * vgDeltaZ)) / vgDeltaZ
c         
c         Extrapolate by at most the given fraction
        fx = max(-extraplim, min(1 + extraplim, fx))
        fy = max(-extraplim, min(1 + extraplim, fy))
        fz = max(-extraplim, min(1 + extraplim, fz))
        ind = istr + ixg - 1 + (iyg - 1 + (izg - 1) * nyg) * nxg
        ind2 = ind + nxg * nyg
        f11 = (1. - fx) * (1. - fy)
        f12 = (1. - fx) * fy
        f21 = fx * (1. - fy)
        f22 = fx * fy
        do i = 1, 3
          vec(i) =  (1. - fz) * (f11 * vecRot(i,indGridToVec(ind))
     &        + f12 * vecRot(i, indGridToVec(ind + nxg))
     &        + f21 * vecRot(i, indGridToVec(ind + 1))
     &        + f22 * vecRot(i, indGridToVec(ind + 1 + nxg)))
     &        + fz * (f11 * vecRot(i, indGridToVec(ind2))
     &        + f12 * vecRot(i, indGridToVec(ind2 + nxg))
     &        + f21 * vecRot(i, indGridToVec(ind2 + 1))
     &        + f22 * vecRot(i, indGridToVec(ind2 + 1 + nxg)))
        enddo
      else if (ndim .eq. 2) then
        fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
        fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
        ind = istr + ixg - 1 + (iyg - 1 + (izg - 1) * nyg) * nxg
        do i = 1, 3
          vec(i) =  (1. - fx) * (1. - fy) * vecRot(i,indGridToVec(ind))
     &        + (1. - fx) * fy * vecRot(i, indGridToVec(ind + nxg))
     &        + fx * (1. - fy) * vecRot(i, indGridToVec(ind + 1))
     &        + fx * fy * vecRot(i, indGridToVec(ind + 1 + nxg))
        enddo        
      else if (nExtendXY .gt. 0) then
c         
c         Extend if allowed: just find nearest vector
        do i = 1,3
          igrd(i) = min(numVecGrid(i,iedge), max(1, 1 +
     &        nint((back(i) - vecGridStart(i,iedge)) / vecGridDelta(i,iedge))))
        enddo
        minind = istr + ixg - 1 + (iyg - 1 + (izg - 1) * nyg) * nxg
c         
c         If the nominally closest one does not exist, do real search
        if (indGridToVec(minind) .eq. 0) then
          minind = 0
          distmin = 1.e20
          do ix = ixg - nExtendXY, ixg + nExtendXY
            do iy = iyg - nExtendXY, iyg + nExtendXY
              do iz = izg - nExtendXY, izg + nExtendXY
                if (ix .gt. 1 .and. ix .le. numVecGrid(1,iedge) .and.
     &              iy .gt. 1 .and. iy .le. numVecGrid(2,iedge) .and.
     &              iz .gt. 1 .and. iz .le. numVecGrid(3,iedge)) then
                  ind = istr + ix - 1 + (iy - 1 + (iz - 1) * nyg) * nxg
                  i = indGridToVec(ind)
                  if (i .gt. 0) then
                    dist = (pos(1) - cenRot(1,i))**2 +
     &                  (pos(2) - cenRot(2,i))**2 + (pos(3) - cenRot(3,i))**2
                    if (dist .lt. distmin) then
                      distmin = dist
                      minind = ind
                    endif
                  endif
                endif
              enddo
            enddo
          enddo
          if (debug)print *,'search',minind,sqrt(distmin)
        endif
        if (minind .gt. 0) then
          do i = 1, 3
            vec(i) = vecRot(i,indGridToVec(minind))
          enddo
          ndim = 1
          i = indGridToVec(minind)
          if (debug) write(*,'(2i6,3f8.1)')minind,i,(cenRot(ix,i),ix=1,3)
        else
          ndim = 0
        endif
c
      else
        ndim = 0
      endif
      if (debug) write(*,'(i2,3f8.2)')ndim,(vec(i),i=1,3)
      return
      end


c       Tests for whether position with indexes IX, IY, IZ is the lower left
c       corner of a complete cube of data, where indG2V is the index from grid
c       position to actual vectors, and NXG and NYG are the dimensions of that
c       piece of the index array.
c
      logical*4 function completeCube(ix, iy, iz, indG2V, nxg, nyg)
      implicit none
      integer*4 ix, iy, iz, nxg, nyg, indG2V(nxg,nyg,*)
      completeCube = indG2V(ix,iy,iz) .gt. 0 .and. indG2V(ix+1,iy,iz) .gt. 0
     &    .and. indG2V(ix,iy+1,iz) .gt. 0 .and. indG2V(ix,iy,iz+1) .gt. 0
     &    .and. indG2V(ix+1,iy+1,iz) .gt. 0 .and. indG2V(ix+1,iy,iz+1) .gt. 0
     &    .and. indG2V(ix,iy+1,iz+1) .gt. 0 .and. indG2V(ix+1,iy+1,iz+1) .gt. 0
      return
      end


c       Tests for whether position with indexes IX, IY, IZ is the lower left
c       corner of a complete square of data in the IZ plane.
c
      logical*4 function completeSquare(ix, iy, iz, indG2V, nxg, nyg)
      implicit none
      integer*4 ix, iy, iz, nxg, nyg, indG2V(nxg,nyg,*)
      completeSquare = indG2V(ix,iy,iz) .gt. 0 .and. indG2V(ix+1,iy,iz) .gt. 0
     &    .and. indG2V(ix,iy+1,iz) .gt. 0 .and. indG2V(ix+1,iy+1,iz) .gt. 0
      return
      end


c       Transforms the position POS with the 3x3 matrix RMAT and the
c       displacements in DXYZ, where NXYZIN and NXYZOUT arethe sizes of the
c       source and destination volumes.  The transformed vector is in OUT.
c
      subroutine transformPos(pos, rmat, dxyz, nxyzin, nxyzout, out)
      implicit none
      real*4 pos(3), rmat(3,3), dxyz(3), out(3), dx, dy, dz
      integer*4 nxyzin(3), nxyzout(3), i
      dx = pos(1) - nxyzin(1) / 2.
      dy = pos(2) - nxyzin(2) / 2.
      dz = pos(3) - nxyzin(3) / 2.
      do i = 1,3
        out(i) = rmat(i,1) * dx + rmat(i,2) * dy + rmat(i,3) * dz + dxyz(i) +
     &      nxyzout(i) / 2.
      enddo
      return 
      end


c       Composes a filename from the root of NAMEIN and the extension EXT
c
      subroutine rootAndExtension(namein, ext, nameout)
      implicit none
      character*(*) namein, ext, nameout
      integer*4 lnblnk,i,lenroot
      lenRoot = lnblnk(namein)
      i = lenRoot
      do while (i .gt. 2 .and. lenRoot .eq. lnblnk(namein))
        if (namein(i:i) .eq. '.') lenRoot = i - 1
        i = i - 1
      enddo
      nameout = namein(1:lenroot)//'.'//ext(1:lnblnk(ext))
      return 
      end


c       $Log$
c       Revision 3.6  2008/12/29 20:43:18  mast
c       Avoid multiple vector outputs at same Z level and enforce only one
c       level of output if input has only one level
c
c       Revision 3.5  2008/10/16 23:15:36  mast
c       Added piece number to lines with overall transformation output
c
c       Revision 3.4  2007/06/17 04:56:55  mast
c       Dimensioned outvec in findWarpVector to * for old Imtel compiler
c
c       Revision 3.3  2007/04/10 19:45:03  mast
c       Made it do pairwise only if more than 2 volumes
c
c       Revision 3.2  2007/04/08 23:11:23  mast
c       Declared lnblnk
c
c       Revision 3.1  2007/04/08 16:13:55  mast
c       Added to package
c
c

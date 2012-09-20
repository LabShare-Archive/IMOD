! STITCHALIGN
!
! Aligns adjacent volumes for stitching together using patch vectors in
! the areas where they overlap.  It finds the best rotation for bringing
! them onto a regular rectangular grid, then finds the best
! transformation for aligning them.  the transformation can include
! rotations around the three axes and magnification, in-plane stretch,
! and thinning.  then it finds a warping vector field for each volume to
! resolve remaining disparities in their overlap zones.
!
! $Id$
!
program stitchalign
  use stitchvars
  implicit none
  integer MAXVAR, MAXSEC, LIMOUTGRID
  parameter (MAXVAR = 10 * MAXVOLS, MAXSEC = 1000, LIMOUTGRID = 500000)
  real*4 var(MAXVAR), grad(MAXVAR), hess(MAXVAR*(MAXVAR+3))
  real*4 rmat(3,3)
  integer*4 listAllZ(MAXSEC), listZdo(MAXSEC), indSection(MAXVOLS)
  integer*4 indXorY(2*MAXVOLS)
  real*4 fitDxyz(3,MAXVOLS), fullMat(3,3,MAXVOLS), edgeDxy(2, 2*MAXVOLS)
  real*4 edgeShift(3, 2*MAXVOLS), volShift(3, MAXVOLS)
  integer*4 iPairLower(2*MAXVOLS), iPairUpper(2*MAXVOLS)
  integer*4 iPairEdge(2*MAXVOLS), ioutGridStart(3,MAXVOLS)
  integer*4 ioutGridEnd(3,MAXVOLS), iposOut(3,LIMOUTGRID), indEdgeInAdoc(2*MAXVOLS)
  integer*4 numOutGrid(3,MAXVOLS), ioutGridDelta(3,MAXVOLS)
  real*4 edgeGeomVars(7,2*MAXVOLS), vecOut(4,LIMOUTGRID)

  real*4 facMetro, eps, xyRotation, angSum, xsum, ysum, angle, dx, dy
  integer*4 maxCycle, ifAlpha, ifBeta, ifGamma, nCycle, ierr, ifMaxSize
  integer*4 ifFindRot, numAllVols, numAllEdges, numSections, numAllZ, numZdo
  integer*4 i, j, iv, jv, ivar, indz, ix, iy, iz, izval, indVec, indLow, m
  integer*4 indHigh, ixhi, iyhi, nsum, nxSum, nySum, nxMax, nyMax, iexy
  integer*4 ixPcStart, ixPcEnd, iyPcStart, iyPcEnd, ivf, jvf, numPairs, iPair
  logical*4 findMag, findStretch, findThin, pairwise, allTogether
  real*4 cosRot, sinRot, dist, finalErr, gridPad, sampleFactor
  real*4 residCrit, outlierCrit
  real*8 dsum, dsumSq
  integer*4 kstr, kend, k, numVarSearch, mapAngles, numEdge, numVecOut, ndim
  integer*4 ixPos, iyPos, maxNumVecZ, metroLoop
  integer*4 minXpiece, minYpiece, maxXpiece, maxYpiece, numXedge, isectIndex
  character*320 infoFile
  character*1024 listString, filename
  character*4 xoryStr
  integer maxMetroTrials
  parameter (maxMetroTrials = 5)
  real*4 trialScale(maxMetroTrials) /1.0, 0.9, 1.1, 0.75, 0.5/
  real*4 atan2d, cosd, sind
  logical pieceExists
  external stitchFunc

  integer*4 AdocRead, AdocGetNumberOfSections, AdocGetThreeIntegers
  integer*4 AdocGetSectionName, AdocGetThreeFloats, AdocGetString
  integer*4 AdocGetInteger, AdocSetKeyValue, AdocWrite, AdocAddSection
  integer*4 AdocSetThreeIntegers, AdocSetTwoIntegers, AdocSetFloat
  integer*4 AdocSetInteger
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetTwoIntegers, PipGetThreeIntegers
  integer*4 PipGetString, PipGetFloat, PipGetFloatArray, PipgetTwoFloats
  integer*4 PipGetInOutFile, PipGetLogical
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  stitchalign
  !
  integer numOptions
  parameter (numOptions = 17)
  character*(40 * numOptions) options(1)
  options(1) = &
      'info:InfoFile:FN:@zvalues:ZvaluesToDo:LI:@'// &
      'rotation:RotationInXYplane:F:@size:SizeOfOutputFramesXandY:IP:@'// &
      'mag:FindMagnification:B:@stretch:FindStretch:B:@'// &
      'thinning:FindThinning:B:@angles:FindAngles:IT:@'// &
      'metro:MetroFactor:F:@xrun:XRunStartEnd:IP:@yrun:YRunStartEnd:IP:@'// &
      'residual:WarpFitResidualCriterion:F:@'// &
      'outlier:OutlierFractionCriterion:F:@'// &
      'spacing:VectorSpacingFactor:F:@all:AllTogether:B:@'// &
      'param:ParameterFile:PF:@help:usage:B:'

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
  ixPcStart = -100000
  ixPcEnd = 100000
  iyPcStart = ixPcStart
  iyPcEnd = ixPcEnd
  dtor = 0.0174532
  sampleFactor = 0.7
  outlierCrit = 0.33
  residCrit = 100.
  !
  ! Pip startup: set error, parse options, do help output
  !
  call PipReadOrParseOptions(options, numOptions, 'stitchalign', &
      'ERROR: STITCHALIGN - ', .false., 1, 1, 0, numOptArg, &
      numNonOptArg)
  !
  ! Get various options
  if (PipGetInOutFile('InfoFile', 1, ' ', infoFile) &
      .ne. 0) call exitError('NO SUPERMONTAGE INFO FILE SPECIFIED')
  ifFindRot = PipGetFloat('RotationInXYplane', xyRotation)
  ierr = PipGetLogical('FindMagnification', findMag)
  ierr = PipGetLogical('FindThinning', findThin)
  ierr = PipGetLogical('FindStretch', findStretch)
  ierr = PipGetThreeIntegers('FindAngles', ifAlpha, ifBeta, ifGamma)
  ierr = PipGetFloat('MetroFactor', facMetro)
  ierr = PipGetTwoIntegers('XRunStartEnd', ixPcStart, ixPcEnd)
  ierr = PipGetTwoIntegers('YRunStartEnd', iyPcStart, iyPcEnd)
  ierr = PipGetLogical('AllTogether', allTogether)
  ierr = PipGetFloat('VectorSpacingFactor', sampleFactor)
  ierr = PipGetFloat('WarpFitResidualCriterion', residCrit)
  ierr = PipGetFloat('OutlierFractionCriterion', outlierCrit)
  ifMaxSize = PipGetTwoIntegers('SizeOfOutputFramesXandY', nxOut, nyOut)
  !
  ! Open the smi file and make a list of Z values there
  if (AdocRead(infoFile) < 0) call exitError('READING THE SUPERMONTAGE INFO FILE')
  numAllVols = AdocGetNumberOfSections('Piece')
  if (numAllVols < 2) call exitError( &
      'THERE IS ONLY ONE VOLUME LISTED IN THE SMI FILE')
  numAllEdges = AdocGetNumberOfSections('Edge')
  numAllZ = 0
  do i = 1, numAllVols
    if (AdocGetThreeIntegers('Piece', i, 'Frame', ix, iy, iz) .ne. 0) &
        call exitError('GETTING FRAME NUMBER FOR A PIECE')
    indz = 0
    do j = 1, numAllZ
      if (iz == listAllZ(j)) indz = j
    enddo
    if (indz == 0) then
      numAllZ = numAllZ + 1
      listAllZ(numAllZ) = iz
      listZdo(numAllZ) = iz
    endif
  enddo
  numZdo = numAllZ
  !
  ! Get optional list of Z's to do and check against all Z list
  if (PipGetString('ZvaluesToDo', listString) == 0) &
      call parselist(listString, listZdo, numZdo)
  do i = 1, numZdo
    indz = 0
    do j = 1, numAllZ
      if (listZdo(i) == listAllZ(j)) indz = j
    enddo
    if (indz == 0) call exitError( &
        'THE LIST OF Z VALUES TO DO CONTAINS A NONEXISTENT VALUE')
  enddo
  call PipDone()
  call dopenHush(.true.)
  !
  ! Loop on Z values to do
  do indz = 1, numZdo
    izval = listZdo(indz)
    if (numZdo > 1) print *,'Doing section at Z =', izval
    do i = 1, 2
      do j = 1, 3
        intervals(i, j) = 0
        spacings(i, j) = 0.
      enddo
    enddo
    !
    ! Load data about the volumes at this Z value
    numVols = 0
    minXpiece = 10000; minYpiece = 10000 ; maxXpiece = -10000; maxYpiece = -10000
    do i = 1, numAllVols
      ierr = AdocGetThreeIntegers('Piece', i, 'Frame', ix, iy, iz)
      if (iz == izval .and. ix >= ixPcStart .and. ix <= ixPcEnd .and. &
          iy >= iyPcStart .and. iy <= iyPcEnd) then
        numVols = numVols + 1
        if (numVols > MAXVOLS) call exitError( &
            'TOO MANY VOLUMES AT ONE Z VALUE FOR ARRAYS')
        ixPiece(numVols) = ix
        iyPiece(numVols) = iy
        minXpiece = min(minXpiece, ix)
        minYpiece = min(minYpiece, iy)
        maxXpiece = max(maxXpiece, ix)
        maxYpiece = max(maxYpiece, iy)
        indSection(numVols) = i
        if (AdocGetThreeIntegers('Piece', i, 'size', nxyzIn(1, numVols), &
            nxyzIn(2, numVols), nxyzIn(3, numVols)) .ne. 0) call exitError &
            ('GETTING VOLUME SIZE FOR A PIECE')
        do j = 1, 2
          iVolUpper(j, numVols) = 0
          iVolLower(j, numVols) = 0
          indVector(j, numVols) = 0
        enddo
      endif
    enddo
    !
    ! Now load the edges at this value and link them back to volumes
    numEdge = 0
    indVec = 1
    indG2Vbase = 1
    maxNumVecZ = 0
    do i = 1, numAllEdges
      if (AdocGetThreeIntegers('Edge', i, 'lower', ix, iy, iz) .ne. 0) &
          call exitError('GETTING LOWER FRAME NUMBER FOR AN EDGE')
      if (AdocGetString('Edge', i, 'XorY', xoryStr) .ne. 0) &
          call exitError('GETTING XorY FOR AN EDGE')
      j = AdocGetString('Edge', i, 'patches', listString)
      if (j < 0) call exitError('GETTING PATCH FILE NAME FOR AN EDGE')
      if (xoryStr == 'X') then
        ixhi = ix + 1
        iyhi = iy
      else
        ixhi = ix
        iyhi = iy + 1
      endif
      if (j == 0 .and. iz == izval .and. ix >= ixPcStart .and. &
          ix <= ixPcEnd .and. iy >= iyPcStart .and. iy <= iyPcEnd .and. &
          ixhi <= ixPcEnd .and. iyhi <= iyPcEnd) then
        !
        ! A valid edge has a patch file - get shifts and x/y index
        numEdge = numEdge + 1
        if (numEdge > 2 * MAXVOLS) call exitError &
            ('TOO MANY EDGES AT ONE Z VALUE FOR ARRAYS')
        if (AdocGetThreeFloats('Edge', i, 'shift', edgeShift(1, numEdge), &
            edgeShift(2, numEdge), edgeShift(3, numEdge)) .ne. 0) &
            call exitError('GETTING SHIFTS FOR AN EDGE')
        indXorY(numEdge) = 1
        if (xoryStr == 'Y') indXorY(numEdge) = 2
        !
        ! read the patches in
        istrVector(numEdge) = indVec
        call readConsensusVectors(i, listString, indVec, residCrit, outlierCrit)
        numVectors(numEdge) = indVec - istrVector(numEdge)
        call setVectorGrid(numEdge)
        maxNumVecZ = max(maxNumVecZ, numVecGrid(3, numEdge))
        !
        ! Look up lower and upper piece in volume list
        indLow = 0
        indHigh = 0
        ixhi = ix
        iyhi = iy
        if (indXorY(numEdge) == 1) ixhi = ix + 1
        if (indXorY(numEdge) == 2) iyhi = iy + 1
        do j = 1, numVols
          if (ixPiece(j) == ix .and. iyPiece(j) == iy) indLow = j
          if (ixPiece(j) == ixhi .and. iyPiece(j) == iyhi) indHigh = j
        enddo
        if (indLow * indHigh .ne. 0) then
          iVolUpper(indXorY(numEdge), indLow) = indHigh
          indVector(indXorY(numEdge), indLow) = numEdge
          iVolLower(indXorY(numEdge), indHigh) = indLow
        endif
        indEdgeInAdoc(numEdge) = i
      endif
    enddo
    !
    ! Check that each volume is connected - just quit if not
    do i = 1, numVols
      if (iVolUpper(1, i) + iVolUpper(2, i) + iVolLower(1, i) + iVolLower(2, i) == 0) &
          then
        write(listString, '(a,3i4,a)') 'THE VOLUME AT', ixPiece(i), iyPiece( i), izval, &
            ' HAS NO EDGES WITH OTHER VOLUMES'
        call exitError(trim(listString))
      endif
    enddo
    print *,numVols, ' piece volumes and', numEdge, ' edges found'
    pairwise = .not.allTogether .and. numVols > 2
    !
    ! find the rotation angle unless it was set
    if (ifFindRot .ne. 0) then
      angSum = 0.
      nsum = 0
      do iv = 1, numVols
        do iexy = 1, 2
          jv = iVolUpper(iexy, iv)
          if (jv > 0) then
            j = indVector(iexy, iv)
            dx = nxyzIn(1, jv) / 2. - nxyzIn(1, iv) / 2. - edgeShift(1, j)
            dy = nxyzIn(2, jv) / 2. - nxyzIn(2, iv) / 2. - edgeShift(2, j)
            angle = atan2d(dy, dx)
            angSum = angSum + angle
            if (iexy == 2) angSum = angSum - 90.
            nsum = nsum + 1
          endif
        enddo
      enddo
      xyRotation = -angSum / nsum
      write(*,'(a,f6.1,a)') 'Rotating all pieces by', xyRotation, ' in X/Y plane'
    endif
    !
    ! Rotate the shifts if rotation is non-zero (don't need to keep old?)
    ! in any case, convert to shifts adjusted to equal sized volumes
    nxSum = 0
    nySum = 0
    xsum = 0.
    ysum = 0.
    nxMax = 0
    nyMax = 0
    nzOut = 0
    call fillZmatrix(rmat, xyRotation * dtor, cosRot, sinRot)
    do iv = 1, numVols
      fullMat(1:3, 1:3, iv) = rmat(1:3, 1:3)
      do iexy = 1, 2
        jv = iVolUpper(iexy, iv)
        if (jv > 0) then
          j = indVector(iexy, iv)
          dx = edgeShift(1, j) + nxyzIn(1, iv) / 2. - nxyzIn(1, jv) / 2.
          dy = edgeShift(2, j) + nxyzIn(2, iv) / 2. - nxyzIn(2, jv) / 2.
          ! print *,iv, iexy, dx, dy, (edgeShift(i, j), i=1, 3)
          if (xyRotation .ne. 0.) then
            scaleXyz = cosRot * dx - sinRot * dy
            dy = sinRot * dx + cosRot * dy
            dx = scaleXyz
          endif
          edgeShift(1, j) = dx
          edgeShift(2, j) = dy
          edgeShift(3, j) = edgeShift(3, j) + nxyzIn(3, iv) / 2. -  nxyzIn(3, jv) / 2.
          ! print *,edgeShift(1, j), edgeShift(2, j), edgeShift(3, j)
          if (iexy == 1) then
            xsum = xsum - edgeShift(1, j)
            nxSum = nxSum + 1
          else
            ysum = ysum - edgeShift(2, j)
            nySum = nySum + 1
          endif
        endif
      enddo
      nxMax = max(nxMax, nxyzIn(1, iv))
      nyMax = max(nyMax, nxyzIn(2, iv))
      nzOut = max(nzOut, nxyzIn(3, iv))
    enddo
    intervalX = nint(xsum / max(1, nxSum))
    intervalY = nint(ysum / max(1, nySum))
    print *,'Spacing between aligned pieces in X and Y:', intervalX, intervalY
    !
    ! if no output size entered, use the max size
    if (ifMaxSize .ne. 0) then
      nxOut = nxMax
      nyOut = nyMax
    endif
    scaleXyz = 1. / (max(nxMax, nyMax))
    ! scaleXyz = 1.
    spacings(1, 1) = scaleXyz * intervals(1, 1)
    spacings(2, 2) = scaleXyz * intervals(2, 2)
    !
    ! Copy the edge displacements into an appropriate array and resolve
    ! them to volume shifts
    do j = 1, numEdge
      do i = 1, 3
        edgeGeomVars(i, j) = edgeShift(i, j) + intervals(indXorY(j), i)
      enddo
    enddo
    call resolveEdgesToVolumes(edgeGeomVars, 7, volShift, 3, 1, 3)
    !
    ! Get an error measure
    xsum = 0.
    ysum = 0.
    nsum = 0
    do iv = 1, numVols
      do iexy = 1, 2
        jv = iVolUpper(iexy, iv)
        if (jv > 0) then
          j = indVector(iexy, iv)
          dist = 0
          do i = 1, 3
            dist = dist + (edgeShift(i, j) + intervals(iexy, i) + &
                volShift(i, jv) - volShift(i, iv))**2
          enddo
          dist = sqrt(dist)
          xsum = xsum + dist
          ysum = max(ysum, dist)
          nsum = nsum + 1
        endif
      enddo
    enddo
    write(*,105) xsum / nsum, ysum
105 format('The error after shifting pieces into register has mean',f7.2, &
        ', maximum', f7.2)
    !
    ! transform the vectors to fit the output volumes as now defined, and
    ! compute the centered positions in upper and lower piece
    call transformVectors(fullMat, volShift, edgeShift)
    !
    ! Set up pairs or set up to run whole set at once
    numPairs = 0
    if (pairwise) then
      numSolve = 2
      do iv = 1, numVols
        do iexy = 1, 2
          jv = iVolUpper(iexy, iv)
          if (jv > 0) then
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
      !
      ! Initialize iVolSolve(0) to 0 so that tests below can test on
      ! iVolSolve alone and not on the index too.
      do i = 0, numVols
        iVolSolve(i) = i
        iVolFull(i) = i
      enddo
    endif
    !
    ! Loop on the pairs
    do iPair = 1, numPairs
      if (pairwise) then
        do i = 0, numVols
          iVolSolve(i) = 0
        enddo
        iVolFull(1) = iPairLower(iPair)
        iVolFull(2) = iPairUpper(iPair)
        iVolSolve(iPairLower(iPair)) = 1
        iVolSolve(iPairUpper(iPair)) = 2
        write(*,108) ixPiece(iVolFull(1)), iyPiece(iVolFull(1)), &
            ixPiece(iVolFull(2)), iyPiece(iVolFull(2))
108     format(/,'Analyzing overlap between piece',2i3,'  and piece',2i3)
      endif
      !
      ! Loop on the metro factors
      metroLoop = 1
      ierr = 1
      do while (metroLoop <= maxMetroTrials .and. ierr .ne. 0 .and. ierr .ne. 3)
        !
        ! Set up variables for minimization
        numVarSearch = 0
        do i = 1, 7
          mapArray(i) = 0
        enddo
        if (findMag) then
          mapGmag = numVarSearch + 1
          numVarSearch = numVarSearch + numSolve - 1
        endif
        if (findThin) then
          mapComp = numVarSearch + 1
          numVarSearch = numVarSearch + numSolve - 1
        endif
        if (findStretch) then
          mapDmag = numVarSearch + 1
          mapSkew = mapDmag + numSolve - 1
          numVarSearch = numVarSearch + 2 * (numSolve - 1)
          mapAngles = mapSkew
        else
          mapAngles = numVarSearch + 1
        endif
        if (ifAlpha .ne. 0) then
          mapAlpha = numVarSearch + 1
          numVarSearch = numVarSearch + numSolve - 1
        endif
        if (ifBeta .ne. 0) then
          mapBeta = numVarSearch + 1
          numVarSearch = numVarSearch + numSolve - 1
        endif
        if (ifGamma .ne. 0) then
          mapGamma = numVarSearch + 1
          numVarSearch = numVarSearch + numSolve - 1
        endif
        call initializeVar(var, gmag, mapGmag, numSolve, 1.)
        call initializeVar(var, comp, mapComp, numSolve, 1.)
        call initializeVar(var, dmag, mapDmag, numSolve, 0.)
        call initializeVar(var, skew, mapSkew, numSolve, 0.)
        call initializeVar(var, alpha, mapAlpha, numSolve, 0.)
        call initializeVar(var, beta, mapBeta, numSolve, 0.)
        call initializeVar(var, gamma, mapGamma, numSolve, 0.)
        !
        ifUnload = 1
        call stitchFunc(numVarSearch, var, finalErr, grad)
        if (metroLoop == 1) write(6, 106) numVarSearch, finalErr
106     format(i5,' variables in search',/,T48,'Initial F : ',T65,E14.7)
        call metro(numVarSearch, var, stitchFunc, finalErr, grad, facMetro * &
            trialScale(metroLoop), eps, &
            maxCycle, ierr, hess, nCycle, 0.)
        metroLoop = metroLoop + 1
        !
        ! For errors except limit reached, give warning message and restart
        if (ierr .ne. 0 .and. ierr .ne. 3) then
          print *
          if (ierr == 1) print *,'Minimization error #1 - DG > 0'
          if (ierr == 2) print *, 'Minimization error #2 - Linear search lost'
          if (ierr == 4) print *, &
              'Minimization error #4 - Matrix non-positive definite'
          if (ierr == 3) call exitError( &
              'Minimization error #3 - Iteration limit exceeded')
          if (metroLoop <= maxMetroTrials) print *, 'Restarting with ', &
              'metro step factor of ', facMetro * trialScale(metroLoop)
        endif
      enddo
      if (ierr .ne. 0) call exitError('Search failed even after varying step factor')
      !
      call stitchFunc(numVarSearch, var, finalErr, grad)
      write(6, 101) finalErr, nCycle
101   format(T48,'Final   F : ',T65,E14.7,/,' Number of cycles : ',i5)
      if (ierr == 0 .and. metroLoop > 2) &
          print *,'Search succeeded with this step factor'
      call flush(6)
      if (pairwise) then
        j = iPairEdge(iPair)
        !
        ! Store values that represent how much the upper piece is displaced
        ! from alignment, the negative of how much needs to be applied to
        ! it - these will work the same as shift displacements
        edgeGeomVars(1, j) = log(gmag(1)) - log(gmag(2))
        edgeGeomVars(2, j) = log(comp(1)) - log(comp(2))
        edgeGeomVars(3, j) = dmag(1) - dmag(2)
        edgeGeomVars(4, j) = skew(1) - skew(2)
        edgeGeomVars(5, j) = alpha(1) - alpha(2)
        edgeGeomVars(6, j) = beta(1) - beta(2)
        edgeGeomVars(7, j) = gamma(1) - gamma(2)
      endif
      call scaleAndReport()
    enddo
    if (pairwise) then
      !
      write(*,'(/,a)') 'Resolving pairwise solution to solution for all volumes:'
      call resolveEdgesToVolumes(edgeGeomVars, 7, volGeomVars, 7, MAXVOLS, 1)
      !
      ! Get mag and comp back, then set up so stitchFunc can compute
      ! dxyz from the existing variable values
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
      call stitchFunc(numVarSearch, var, finalErr, grad)
      call scaleAndReport()
    endif
    !
    ! compute full transform and output to file
    do iv = 1, numVols
      do i = 1, 3
        fitDxyz(i, iv) = dxyz(iv, i)
      enddo
      call xfmult3d(rmat, volShift(1, iv), fitMat(1, 1, iv), fitDxyz(1, iv), &
          fullMat(1, 1, iv), fullDxyz(1, iv))

      ! To work in unshifted volumes that get shifted by blendmont, remove the shift
      ! before taking the inverse, and output matxf's with zero X and Y shifts
      var(1:2) = 0.
      var(3) = fullDxyz(3, iv)
      call xfInv3d(fullMat(1, 1, iv), var, fInvMat(1, 1, iv), fInvDxyz(1, iv))
      if (AdocGetSectionName('Piece', indSection(iv), listString) < 0) &
          call exitError('GETTING ADOC SECTION NAME FOR A PIECE')
      call rootAndExtension(listString, 'matxf', filename)
      call dopen(1, filename, 'new', 'f')
      write(1, 104) ((fullMat(i, j, iv), j = 1, 3), 0., i = 1, 2)
      write(1, 104) (fullMat(3, j, iv), j = 1, 3), fullDxyz(3, iv)
104   format(3f10.6,f10.3)
      close(1)
      if (AdocSetKeyValue('Piece', indSection(iv), 'matxf', filename) .ne. 0) &
          call exitError('SETTING NEW KEY-VALUE PAIR')
    enddo
    !
    ! Transform the vectors to the new alignments then get some summary
    ! features of the edges
    ! call transformVectors(fullMat, fullDxyz, edgeShift)
    do i = 1, 3
      delMean(i) = 0.
    enddo
    nsum = 0
    edgeWidthMean = 0.
    do iv = 1, numVols
      do iexy = 1, 2
        jv = iVolUpper(iexy, iv)
        if (jv > 0) then
          j = indVector(iexy, iv)
          kstr = istrVector(j)
          kend = kstr + numVectors(j) - 1
          nsum = nsum + 1
          
          !
          ! Transform each position in lower and upper volume with added transform, and
          ! get vector from difference.  This matches the scaled residuals.
          ! The computation includes the full volume shifts for getting vectors
          ! print *,'Volume, upper vol, xy, edge #:', iv, jv, iexy, j
          ! print *,numVectors(j), ' positions', kstr, kend
          do k = kstr, kend
            call transformPos(cenRot(1, k), fitMat(1, 1, iv), fitDxyz(1, iv), nxyzOut, &
                nxyzOut, posLower(1, k))

            do i = 1, 3
              var(i) = cenRot(i, k) + vecRot(i, k) - intervals(iexy, i)
            enddo
            call transformPos(var, fitMat(1, 1, jv), fitDxyz(1, jv), nxyzOut, nxyzOut, &
                posUpper(1, k))
            vecRot(1:3, k) = posUpper(1:3, k) + intervals(iexy, 1:3) - posLower(1:3, k)
            
            ! shift vector position into unshifted volume coordinates
            cenRot(1:2, k) = posLower(1:2, k) - fullDxyz(1:2, iv)
            cenRot(3, k) = posLower(3, k)
          enddo
          !
          do i = 1, 3
            edgeMin(i, j) = 1.e10
            edgeMax(i, j) = -1.e10
            gridPad = 0.
            if (i < 3) gridPad = 0.5 * vecGridDelta(i, j)
            !
            ! Get the mean grid spacing, but pad the edge by half of its
            ! own grid spacing for X and Y, not for Z
            delMean(i) = delMean(i) + vecGridDelta(i, j)
            do k = kstr, kend
              edgeMin(i, j) = min(edgeMin(i, j), cenRot(i, k) - gridPad)
              edgeMax(i, j) = max(edgeMax(i, j), cenRot(i, k) + gridPad)
            enddo
            extendedEdMin(i, j) = edgeMin(i, j)
            extendedEdMax(i, j) = edgeMax(i, j)
          enddo
          edgeWidthMean = edgeWidthMean + edgeMax(iexy, j) - edgeMin(iexy, j)
          !
          ! Set extended min/max to edge of volume if there is no edge
          ! butting up in that direction
          if (iexy == 1) then
            if (.not.(pieceExists(ixPiece(iv), iyPiece(iv) - 1) .and. &
                pieceExists(ixPiece(jv), iyPiece(jv) - 1))) &
                extendedEdMin(2, j) = min(extendedEdMin(2, j), 0.)
            if (.not.(pieceExists(ixPiece(iv), iyPiece(iv) + 1) .and. &
                pieceExists(ixPiece(jv), iyPiece(jv) + 1))) &
                extendedEdMax(2, j) = max(extendedEdMax(2, j), nyOut - 0.1)
          else
            if (.not.(pieceExists(ixPiece(iv) - 1, iyPiece(iv)) .and. &
                pieceExists(ixPiece(jv) - 1, iyPiece(jv)))) &
                extendedEdMin(1, j) = min(extendedEdMin(1, j), 0.)
            if (.not.(pieceExists(ixPiece(iv) + 1, iyPiece(iv)) .and. &
                pieceExists(ixPiece(jv) + 1, iyPiece(jv)))) &
                extendedEdMax(1, j) = max(extendedEdMax(1, j), nxOut - 0.1)
          endif
          !
          ! Determine min and max in bands across the edge for better
          ! estimation of edge fractions
          numBands(j) = max(1, min(MAXBAND, numVecGrid(3 - iexy, j) / 2))
          do i = 1, numBands(j)
            bandMin(i, j) = edgeMax(iexy, j)
            bandMax(i, j) = edgeMin(iexy, j)
          enddo
          gridPad = 0.5 * vecGridDelta(iexy, j)
          bandDel(j) = (edgeMax(3 - iexy, j) - edgeMin(3 - iexy, j)) / numBands(j)
          do k = kstr, kend
            i = (cenRot(3 - iexy, k) - edgeMin(3 - iexy, j)) / bandDel(j) + 1.
            i = max(1, min(numBands(j), i))
            bandMin(i, j) = min(bandMin(i, j), cenRot(iexy, k) - gridPad)
            bandMax(i, j) = max(bandMax(i, j), cenRot(iexy, k) + gridPad)
            ! if (k<kstr + 100) write(*,'(3f8.1,9f6.1)') &
            ! (cenRot(i, k), i=1, 3), &
            ! (vector(i, k), i=1, 3), (vecRot(i, k), i=1, 3), &
            ! (resid(i, k) /scaleXyz, i=1, 3)
          enddo
          ! print *,iv, iexy, (edgeMin(i, j), edgeMax(i, j), i=1, 3), &
          ! (extendedEdMin(i, j), extendedEdMax(i, j), i=1, 2)
          ! print *,numBands(j), ' bands'
          ! print *,(bandMin(i, j), bandMax(i, j), i=1, numBands(j))
        endif
      enddo
    enddo
    do i = 1, 3
      delMean(i) = delMean(i) / nsum
    enddo
    edgeWidthMean = edgeWidthMean / nsum
    !
    ! Get limits of corners of volumes and limits for output grids
    do iv = 1, numVols
      volLimTR(1, iv) = nxOut
      volLimTR(2, iv) = nyOut
      volLimBR(1, iv) = nxOut
      volLimBR(2, iv) = 0.
      volLimTL(1, iv) = 0.
      volLimTL(2, iv) = nyOut
      volLimBL(1, iv) = 0.
      volLimBL(2, iv) = 0.
      ioutGridStart(1, iv) = nxOut / 2
      ioutGridStart(2, iv) = nyOut / 2
      ioutGridStart(3, iv) = nzOut / 2
      ioutGridEnd(1, iv) = nxOut / 2
      ioutGridEnd(2, iv) = nyOut / 2
      ioutGridEnd(3, iv) = nzOut / 2
      do iexy = 1, 2
        !
        ! First do upper edge
        if (iVolUpper(iexy, iv) > 0) then
          j = indVector(iexy, iv)
          !
          ! Output grid goes out as far as any edges go in each direction
          do i = 1, 3
            ioutGridStart(i, iv) = min(ioutGridStart(i, iv), nint(edgeMin(i, j)))
            ioutGridEnd(i, iv) = max(ioutGridEnd(i, iv), nint(edgeMax(i, j)))
          enddo
          !
          ! volume limits for end fractions are the minimum of all limits
          volLimTR(1, iv) = min(volLimTR(1, iv), edgeMax(1, j))
          volLimTR(2, iv) = min(volLimTR(2, iv), edgeMax(2, j))
          if (iexy == 1) then
            volLimBR(1, iv) = min(volLimBR(1, iv), edgeMax(1, j))
            volLimBR(2, iv) = max(volLimBR(2, iv), edgeMin(2, j))
          else
            volLimTL(1, iv) = max(volLimTL(1, iv), edgeMin(1, j))
            volLimTL(2, iv) = min(volLimTL(2, iv), edgeMax(2, j))
          endif
        endif
        !
        ! Now do lower edge
        jv = iVolLower(iexy, iv)
        if (jv > 0) then
          j = indVector(iexy, jv)

          ! var is used to get edge coordinates from their unshifted positions in lower
          ! volume to shifted position, then from shifted position in upper volume to
          ! position in unshifted volume
          var(1:2) = fullDxyz(1:2, jv) - fullDxyz(1:2, iv)
          var(3) = 0.
          do i = 1, 3
            ioutGridStart(i, iv) = min(ioutGridStart(i, iv),  &
                nint(edgeMin(i, j) + var(i)) - intervals(iexy, i))
            ioutGridEnd(i, iv) = max(ioutGridEnd(i, iv),  &
                nint(edgeMax(i, j) + var(i)) - intervals(iexy, i))
          enddo
          volLimBL(1, iv) = max(volLimBL(1, iv),  &
              edgeMin(1, j) + var(1) - intervals(iexy, 1))
          volLimBL(2, iv) = max(volLimBL(2, iv),  &
              edgeMin(2, j) + var(2) - intervals(iexy, 2))
          if (iexy == 1) then
            volLimTL(1, iv) = max(volLimTL(1, iv), edgeMin(1, j) + var(1) - intervalX)
            volLimTL(2, iv) = min(volLimTL(2, iv), edgeMax(2, j))
          else
            volLimBR(1, iv) = min(volLimBR(1, iv), edgeMax(1, j))
            volLimBR(2, iv) = max(volLimBR(2, iv), edgeMin(2, j) + var(2) - intervalY)
          endif
        endif
      enddo
      ! print *,iv, volLimTL(1, iv), volLimTL(2, iv), volLimTR(1, iv), volLimTR(2, iv)
      ! print *,volLimBL(1, iv), volLimBL(2, iv), volLimBR(1, iv), volLimBR(2, iv)
      ! print *,(ioutGridStart(i, iv), ioutGridEnd(i, iv), i=1, 3)
      !
      ! Massage the starting and ending output grid values to give evenly
      ! spaced data
      do i = 1, 3
        ioutGridStart(i, iv) = max(ioutGridStart(i, iv), 1)
        ioutGridEnd(i, iv) = min(ioutGridEnd(i, iv), nxyzOut(i) - 2)
        ix = ioutGridEnd(i, iv) + 1 - ioutGridStart(i, iv)
        numOutGrid(i, iv) = min(ix, nint(ix / (delMean(i) * sampleFactor)) &
            + 1)
        if (i == 3. .and. maxNumVecZ < 2) then
          numOutGrid(i, iv) = 1
          ioutGridStart(i, iv) = (ioutGridStart(i, iv) + ioutGridEnd(i, iv)) / 2
        endif
        ioutGridDelta(i, iv) = ix / max(1, numOutGrid(i, iv) - 1)
        iy = mod(iz, max(1, numOutGrid(i, iv) - 1))
        ioutGridStart(i, iv) = ioutGridStart(i, iv) + iy / 2
        ioutGridEnd(i, iv) = ioutGridStart(i, iv) + (numOutGrid(i, iv) - 1) * &
            ioutGridDelta(i, iv)
      enddo
    enddo
    !
    ! Loop on creating the output grids
    do iv = 1, numVols
      numVecOut = 0
      do ix = 1, numOutGrid(1, iv)
        ixPos = ioutGridStart(1, iv) + (ix - 1) * ioutGridDelta(1, iv)
        do iy = 1, numOutGrid(2, iv)
          iyPos = ioutGridStart(2, iv) + (iy - 1) * ioutGridDelta(2, iv)
          !
          ! Prescan all the positions in a column for each edge and enforce
          ! consistency in whether an edge is kept active in the initial
          ! test: if any points in column give a vector and not all points
          ! do so, then set up to extend to nearest vector  HUH? STILL?
          call countEdges(float(ixPos), float(iyPos), iv)
          !
          ! With the current behavior in findWarpVector, this whole loop
          ! has no function
!!$              do iexy=1, 2
!!$                do i=1, numEdges(iexy)
!!$                  j=inEdge(i, iexy)
!!$                  k = inEdLower(i, iexy)
!!$                  nsum = 0
!!$                  if (ixPos == -1 .and. iyPos == -1) &
!!$                      print *,xInPiece(k), yInPiece(k)
!!$                  do iz = 1, numOutGrid(3, iv)
!!$                    xsum = ioutGridStart(3, iv) + (iz-1)*ioutGridDelta(3, iv)
!!$                    call interpolateVector(xInPiece(k), yInPiece(k), xsum, &
!!$                        inPiece(k), j, 0.5, 0, fitDxyz, ndim)
!!$                    if (ndim > 0) nsum = nsum + 1
!!$                  enddo
!!$                  nIniExtend(i, iexy) = 0
!!$c                  if (nsum > 0 .and. nsum < numOutGrid(3, iv))
!!$                  if (nsum == numOutGrid(3, iv)) &
!!$                      nIniExtend(i, iexy) = 5
!!$                enddo
!!$              enddo

          do iz = 1, numOutGrid(3, iv)
            i = numVecOut + 1
            if (i > LIMOUTGRID) call exitError('TOO MANY OUTPUT VECTORS FOR ARRAYS')
            iposOut(1, i) = ixPos
            iposOut(2, i) = iyPos
            iposOut(3, i) = ioutGridStart(3, iv) + (iz - 1) * ioutGridDelta(3, iv)
            call findWarpVector(iposOut(1, i), iv, vecOut(1, i), ndim)
            if (ixPos == -1 .and. iyPos == -1) &
                print *,iposOut(3, i), ndim, (vecOut(j, i), j = 1, 3)
            if (ndim > 0) numVecOut = i
          enddo
        enddo
      enddo
      !
      if (AdocGetSectionName('Piece', indSection(iv), listString) < 0) &
          call exitError('GETTING ADOC SECTION NAME FOR A PIECE')
      call rootAndExtension(listString, 'patch', filename)
      call dopen(1, filename, 'new', 'f')
      write(1, '(i8,a)') numVecOut, ' positions'
      write(1, '(3i6,4f9.2)') ((iposOut(i, j), i = 1, 3), (vecOut(i, j), i = 1, 4), &
          j = 1, numVecOut)
      close(1)
      if (AdocSetKeyValue('Piece', indSection(iv), 'vectors', filename) &
          .ne. 0) call exitError('SETTING NEW KEY-VALUE PAIR')
    enddo

    !
    ! Add the output size and spacing for the section
    numSections = AdocGetNumberOfSections('Section')
    isectIndex = 0
    do i = 1, numSections
      if (AdocGetInteger('Section', i, 'zvalue', iz) .ne. 0) &
          call exitError('GETTING Z VALUE FOR A SECTION')
      if (iz == izval) isectIndex = i
    enddo
    if (isectIndex == 0) then
      call int_iwrite(listString, izval, iy)
      filename = 'Section'//listString(1:iy)
      isectIndex = AdocAddSection('Section', filename)
      if (isectIndex < 0) call exitError('ADDING A SECTION TO INFO FILE')
      if (AdocSetInteger('Section', isectIndex, 'zvalue', izval) .ne. 0) &
          call exitError('SETTING NEW KEY-VALUE PAIR')
    endif
    !
    ! Compute edge displacements from the difference between shifts of a pair of pieces
    nsum = 0
    do iy = minYpiece, maxYpiece
      do ix = minXpiece, maxXpiece - 1
        call getEdgeDisplacement(ix, iy, 1)
      enddo
    enddo
    numXedge = nsum
    do ix = minXpiece, maxXpiece
      do iy = minYpiece, maxYpiece - 1
        call getEdgeDisplacement(ix, iy, 2)
      enddo
    enddo
    !
    ! put edge disp out in a stub file
    if (AdocGetSectionName('Section', isectIndex, listString) < 0) &
        call exitError('GETTING ADOC SECTION NAME FOR A SECTION')
    call rootAndExtension(listString, 'ecdstub', filename)
    call dopen(1, filename, 'new', 'f')
    write(1,'(2i8)') numXedge, nsum - numXedge
    write(1,'(2f12.3)')((edgeDxy(j,i),j=1,2),i=1,nsum)
    close(1)

    if (AdocSetThreeIntegers('Section', isectIndex, 'outsize', nxOut, nyOut, nzOut) .ne. &
        0 &
        .or. AdocSetTwoIntegers('Section', isectIndex, 'spacing', intervalX, intervalY) &
        .ne. 0 .or. AdocSetFloat('Section', isectIndex, 'intervalFactor', sampleFactor) &
        .ne. 0 .or. AdocSetKeyValue('Section', isectIndex, 'ecdStub', filename) .ne. 0) &
        call exitError('SETTING NEW KEY-VALUE PAIR')
  enddo
  !
  ierr = AdocWrite(infoFile)
  if (ierr .ne. 0) print *,'Error', ierr, ' writing to info file'
  call exit(0)

CONTAINS

  subroutine getEdgeDisplacement(ix, iy, iexy)
    integer*4 ix, iy, iexy, iex, iey, iez
    integer*4 AdocSetTwoFloats
    do iv = 1, numVols
      jv = ivolUpper(iexy, iv)
      if (ixPiece(iv) == ix .and. iyPiece(iv) == iy .and. jv > 0) then
        nsum = nsum + 1
        edgeDxy(1:2, nsum) = fullDxyz(1:2, jv) - fullDxyz(1:2, iv)
        if (AdocSetTwoFloats('Edge', indEdgeInAdoc(indVector(iexy, iv)), 'blendShift', &
            edgeDxy(1, nsum), edgeDxy(2, nsum)) .ne. 0) &
            call exitError('SETTING NEW KEY-VALUE PAIR')
      endif
    enddo
    return
  end subroutine getEdgeDisplacement

end program stitchalign

! Reads the main patch vector file, and reads reduced vector file from
! Refinematch and residual vector file from Findwarp if they exist.
! Stores a vector in the arrays if it is still in all files and if
! its residual and outlier fraction are less than the criteria
!
subroutine readConsensusVectors(iedge, patchFile, indVec, residCrit, outlierCrit)
  use stitchvars
  implicit none
  integer*4 iedge, indVec
  character*(*) patchFile
  real*4 residCrit, outlierCrit
  logical keep
  integer*4 numVecs, numReduce, numResid, i, j, k, numResLeft, ierr
  real*4 residual, outFrac, dum1, dum2, dum3
  character*320 filename, errString
  real*4, allocatable :: reduceCen(:,:), residCen(:,:)
  integer*4 AdocGetString

  numReduce = 0
  numResid = 0
  numResLeft = 0
  filename = patchFile
  call dopen(1, patchFile, 'ro', 'f')
  read(1, *, end = 98, err = 98) numVecs
  !
  if (AdocGetString('Edge', iedge, 'reducePatch', filename) == 0) then
    call dopen(2, filename, 'ro', 'f')
    read(2, *, end = 98, err = 98) numReduce
    if (numReduce > 0) then
      allocate(reduceCen(3, numReduce), stat = ierr)
      call memoryError(ierr, 'ALLOCATING MEMORY FOR REDUCED PATCHES')
      do j = 1, numReduce
        read(2, *, end = 98, err = 98) (reduceCen(k, j), k = 1, 3)
      enddo
    endif
    close(2)
  endif
  !
  if (AdocGetString('Edge', iedge, 'residPatch', filename) == 0) then
    call dopen(2, filename, 'ro', 'f')
    read(2, *, end = 98, err = 98) numResid
    if (numResid > 0) then
      allocate(residCen(3, numResid), stat = ierr)
      call memoryError(ierr, 'ALLOCATING MEMORY FOR RESIDUAL PATCH DATA')
      !
      ! read the residual vectors and eliminate outliers right here
      numResLeft = 0
      do j = 1, numResid
        read(2, *, end = 98, err = 98) (residCen(k, numResLeft + 1), k = 1, 3), &
            dum1, dum2, dum3, residual, outFrac
        if (residual < residCrit .and. outFrac < outlierCrit) &
            numResLeft = numResLeft + 1
      enddo
      write(*,'(i5,a,/,5x,a)') numResid - numResLeft, ' patches eliminated'// &
          ' by high residual or outlier fraction from', trim(patchFile)
    endif
    close(2)
  else
    write(*,'(/,a,a)') 'WARNING: NO RESIDUAL OR OUTLIER INFORMATION '// &
        'AVAILABLE FOR PATCHES IN ', trim(patchFile)
  endif
  !
  ! read in the vectors
  filename = patchFile
  do j = 1, numVecs
    if (indVec >= MAXVECS) call exitError( &
        'TOO MANY PATCH VECTORS IN EDGES FOR ARRAYS')
    read(1, *, end = 98, err = 98) (center(k, indVec), k = 1, 3), (vector(k, indVec),  &
        k = 1, 3)
    !
    ! Look the vector up in the reduced vectors
    keep = .true.
    if (numReduce > 0) then
      keep = .false.
      do i = 1, numReduce
        if (abs(center(1, indVec) - reduceCen(1, i)) <= 0.1 .and. &
            abs(center(2, indVec) - reduceCen(2, i)) <= 0.1 .and. &
            abs(center(3, indVec) - reduceCen(3, i)) <= 0.1) then
          keep = .true.
          exit
        endif
      enddo
      ! if (.not.keep) print *,(nint(center(i, indVec)), i=1, 3), &
      ! ' not found in reduced patches'
    endif
    !
    ! if still in, look it up in the residual vectors
    if (keep .and. numResLeft > 0) then
      keep = .false.
      do i = 1, numResLeft
        if (abs(center(1, indVec) - residCen(1, i)) <= 0.1 .and. &
            abs(center(2, indVec) - residCen(2, i)) <= 0.1 .and. &
            abs(center(3, indVec) - residCen(3, i)) <= 0.1) then
          keep = .true.
          exit
        endif
      enddo
      ! if (.not.keep) print *,(nint(center(i, indVec)), i=1, 3), &
      ! ' not found in resid patches'
    endif
    if (keep) indVec = indVec + 1
  enddo
  close(1)
  if (numReduce > 0) deallocate(reduceCen, stat = ierr)
  if (numResid > 0) deallocate(residCen, stat = ierr)
  return
98 write(errString, '(a,a)') 'READING PATCH VECTOR FILE ', filename
  call exitError(errString)
end subroutine readConsensusVectors


! STITCHFUNC for evaluating matrices, error and gradient
!
subroutine stitchFunc(numVarSearch, var, ferror, grad)
  !
  use stitchvars
  implicit none
  integer mv
  parameter (mv = MAXVOLS)
  integer*4 numVarSearch
  real*4 var(*), ferror, grad(*)
  real*4 xmat(9,mv), ymat(9,mv), zmat(9,mv), dmat(9,mv)
  real*4 derMat(3,3), da(9), dan(9)
  real*4 cosAlpha(mv), sinAlpha(mv), cosBeta(mv), sinBeta(mv), cosGamma(mv), sinGamma(mv)
  real*4 cosDelta(mv), sinDelta(mv), fac
  real*8 derror, dsum
  integer*4 ivar, igrad, i, k, iexy, kstr, kend, ixyz, itype, j
  integer*4 numVec, ivs, jvs, ivf, jvf
  real*8 gradientSum
  ! equivalence (aa, fitMat)
  !
  ! ivs, jvs are used to access arrays indexed on volumes beings solved,
  ! ivf, jvf are used to access the full list of loaded volumes
  !
  if (ifUnload .ne. 0) call unloadVars(var)
  !
  ! Build the matrices for each volume
  do i = 1, numSolve
    call fillDistMatrix(dmat(1, i), gmag(i), comp(i), dmag(i), skew(i), &
        cosDelta(i), sinDelta(i))
    call fillXmatrix(xmat(1, i), alpha(i), cosAlpha(i), sinAlpha(i))
    call fillYmatrix(ymat(1, i), beta(i), cosBeta(i), sinBeta(i))
    call fillZmatrix(zmat(1, i), gamma(i), cosGamma(i), sinGamma(i))
    call fullMatProduct(dmat(1, i), zmat(1, i), ymat(1, i), xmat(1, i), &
        aa(1, 1, i))
  enddo
  ! print *,(dmat(i, 1), i=1, 9)
  ! print *,(xmat(i, 1), i=1, 9)
  ! print *,(ymat(i, 1), i=1, 9)
  ! print *,(zmat(i, 1), i=1, 9)
  ! print *,((aa(i, k, 1), i=1, 3), k=1, 3)
  !
  ! Compute the residual error components needed to solve for dxyz
  do ivs = 1, numSolve
    ivf = iVolFull(ivs)
    do iexy = 1, 2
      jvf = iVolUpper(iexy, ivf)
      if (iVolSolve(jvf) > 0) then
        jvs = iVolSolve(jvf)
        kstr = istrVector(indVector(iexy, ivf))
        kend = kstr + numVectors(indVector(iexy, ivf)) - 1
        do ixyz = 1, 3
          do k = kstr, kend
            resid(ixyz, k) = spacings(iexy, ixyz)
            do i = 1, 3
              resid(ixyz, k) = resid(ixyz, k) + aa(ixyz, i, jvs) * posUpper(i, k) - &
                  aa(ixyz, i, ivs) * posLower(i, k)
            enddo
          enddo
        enddo
        ! if (ivs == 1) write(*,'(9f8.1)') ((resid(ixyz, k) &
        ! /scaleXyz, ixyz=1, 3), k=kstr, kend)
      endif
    enddo
  enddo
  !
  ! Solve for dxyz
  do ivar = 1, numSolve - 1
    do j = 1, numSolve - 1
      daa(j, ivar) = 0.
    enddo
    do j = 1, 3
      dbb(j, ivar) = 0
    enddo

    do ivs = 1, numSolve
      ivf = iVolFull(ivs)
      do iexy = 1, 2
        jvf = iVolUpper(iexy, ivf)
        if (iVolSolve(jvf) > 0) then
          jvs = iVolSolve(jvf)
          numVec = numVectors(indVector(iexy, ivf))
          fac = 0.
          !
          ! Cover the cases in which this edge counts, and of what to do
          ! with the upper piece variables: either add to all variables if
          ! it is N, subtract from upper variable if it this is lower, or
          ! add to upper var if it is upper
          if (jvs == numSolve) then
            do j = 1, numSolve - 1
              daa(j, ivar) = daa(j, ivar) + numVec
            enddo
            fac = 1.
          else if (ivs == ivar) then
            daa(jvs, ivar) = daa(jvs, ivar) - numVec
            fac = 1.
          else if (jvs == ivar) then
            daa(jvs, ivar) = daa(jvs, ivar) + numVec
            fac = -1.
          endif
          if (fac .ne. 0.) then
            !
            ! in any case, add to this variable depending on fac, and
            ! add up the resid components for the constant terms
            daa(ivs, ivar) = daa(ivs, ivar) + fac * numVec
            kstr = istrVector(indVector(iexy, ivf))
            kend = kstr + numVec - 1
            do j = 1, 3
              dsum = 0.
              do k = kstr, kend
                dsum = dsum + resid(j, k)
              enddo
              dbb(j, ivar) = dbb(j, ivar) + dsum * fac
            enddo
          endif
        endif
      enddo
    enddo
  enddo
  !
  ! Get solution and copy the dxyz out, compose the N one
  ! write(*,'(8f6.0,3f10.2)') ((daa(i, ivar), i=1, numSolve-1), (dbb(i, ivar), i=1, 3) &
  ! , ivar=1, numSolve-1)
  call gaussj(daa, numSolve - 1, MAXVOLS, dbb, 3, 7)
  ! print *,'Solution:'
  ! write(*,'(8f6.3,3f10.2)') ((daa(i, ivar), i=1, numSolve-1), (dbb(i, ivar), i=1, 3) &
  ! , ivar=1, numSolve-1)
  do j = 1, 3
    dxyz(numSolve, j) = 0
    do i = 1, numSolve - 1
      dxyz(i, j) = dbb(j, i)
      dxyz(numSolve, j) = dxyz(numSolve, j) - dbb(j, i)
    enddo
  enddo
  ! write(*,'(9f8.1)') ((dxyz(i, j) /scaleXyz, i=1, 3), j = 1, numSolve)
  !
  ! Now correct the residuals and get the error
  derror = 0.
  do ivs = 1, numSolve
    ivf = iVolFull(ivs)
    do iexy = 1, 2
      jvf = iVolUpper(iexy, ivf)
      if (iVolSolve(jvf) > 0) then
        jvs = iVolSolve(jvf)
        kstr = istrVector(indVector(iexy, ivf))
        kend = kstr + numVectors(indVector(iexy, ivf)) - 1
        do ixyz = 1, 3
          do k = kstr, kend
            resid(ixyz, k) = resid(ixyz, k) + dxyz(jvs, ixyz) - dxyz(ivs, ixyz)
            derror = derror + resid(ixyz, k)**2
          enddo
        enddo
        ! if (ivs == 1 .and. iexy == 1) write(*,'(9f8.1)') ((resid(ixyz, k) &
        ! /scaleXyz, ixyz=1, 3), k=kstr, kend)
        ! if (ivs == 1 .and. iexy == 2) write(*,'(6f8.1)') ((posLower(ixyz, k) &
        ! /scaleXyz, ixyz=1, 3), (posUpper(ixyz, k) /scaleXyz , ixyz=1, 3), k=kstr, kend)
      endif
    enddo
  enddo
  ferror = derror
  !
  ! Loop on the variables, compute gradients for ones that are variable
  !
  do itype = 1, 7
    if (mapArray(itype) > 0) then
      do ivar = numSolve, 1, -1
        dsum = 0.
        !
        ! compute derivative of matrix w/r to this variable
        call zeroMatrix(derMat)
        if (itype == 1) then
          !
          ! gmag variable
          derMat(1, 1) = cosDelta(ivar)
          derMat(2, 1) = -sinDelta(ivar)
          derMat(1, 2) = -sinDelta(ivar)
          derMat(2, 2) = cosDelta(ivar)
          derMat(3, 3) = comp(ivar)
          call fullMatProduct(derMat, zmat(1, ivar), ymat(1, ivar), &
              xmat(1, ivar), da)
        else if (itype == 2) then
          !
          ! comp variable
          derMat(3, 3) = gmag(ivar)
          call fullMatProduct(derMat, zmat(1, ivar), ymat(1, ivar), &
              xmat(1, ivar), da)
        else if (itype == 3) then
          !
          ! dmag variable
          derMat(1, 1) = cosDelta(ivar)
          derMat(2, 1) = -sinDelta(ivar)
          derMat(1, 2) = sinDelta(ivar)
          derMat(2, 2) = -cosDelta(ivar)
          call fullMatProduct(derMat, zmat(1, ivar), ymat(1, ivar), &
              xmat(1, ivar), da)
        else if (itype == 4) then
          !
          ! skew variable
          derMat(1, 1) = -(gmag(ivar) + dmag(ivar)) * sinDelta(ivar)
          derMat(2, 1) = -(gmag(ivar) + dmag(ivar)) * cosDelta(ivar)
          derMat(1, 2) = -(gmag(ivar) - dmag(ivar)) * cosDelta(ivar)
          derMat(2, 2) = -(gmag(ivar) - dmag(ivar)) * sinDelta(ivar)
          call fullMatProduct(derMat, zmat(1, ivar), ymat(1, ivar), &
              xmat(1, ivar), da)
        else if (itype == 5) then
          !
          ! X rotation
          derMat(2, 2) = -sinAlpha(ivar)
          derMat(3, 2) = cosAlpha(ivar)
          derMat(3, 3) = -sinAlpha(ivar)
          derMat(2, 3) = -cosAlpha(ivar)
          call fullMatProduct(dmat(1, ivar), zmat(1, ivar), ymat(1, ivar), &
              derMat, da)
        else if (itype == 6) then
          !
          ! Y rotation
          derMat(1, 1) = -sinBeta(ivar)
          derMat(1, 3) = cosBeta(ivar)
          derMat(3, 3) = -sinBeta(ivar)
          derMat(3, 1) = -cosBeta(ivar)
          call fullMatProduct(dmat(1, ivar), zmat(1, ivar), derMat, &
              xmat(1, ivar), da)
        else
          !
          ! Z rotation
          derMat(1, 1) = -sinGamma(ivar)
          derMat(2, 1) = cosGamma(ivar)
          derMat(2, 2) = -sinGamma(ivar)
          derMat(1, 2) = -cosGamma(ivar)
          call fullMatProduct(dmat(1, ivar), derMat, ymat(1, ivar), &
              xmat(1, ivar), da)
        endif
        if (ivar == numSolve) then
          !
          ! the last volume is not a variable, but we need the derivative
          ! evaluated at that point, so save it
          do k = 1, 9
            dan(k) = da(k)
          enddo
        else
          !
          ! for real variables, get the appropriate gradient sums
          do ivs = 1, numSolve
            ivf = iVolFull(ivs)
            do iexy = 1, 2
              jvf = iVolUpper(iexy, ivf)
              if (iVolSolve(jvf) > 0) then
                jvs = iVolSolve(jvf)
                kstr = istrVector(indVector(iexy, ivf))
                kend = kstr + numVectors(indVector(iexy, ivf)) - 1
                if (ivs == ivar) then
                  dsum = dsum + gradientSum(da, resid, posLower, kstr, kend, -2.)
                else if (jvs == ivar) then
                  dsum = dsum + gradientSum(da, resid, posUpper, kstr, kend, 2.)
                endif
                if (jvs == numSolve) then
                  dsum = dsum + gradientSum(dan, resid, posUpper, kstr, kend, -2.)
                endif
              endif
            enddo
          enddo
          grad(ivar + mapArray(itype) - 1) = dsum
        endif
      enddo
    endif
  enddo
  ! write(*,'(2f12.6,3x,2f12.6,3x,2f12.6)') (var(i), grad(i), i=1, numVarSearch)
  return
end subroutine stitchFunc

! Forms a gradient sum of the product of position times the derivative
! matrix times the residual
!
real*8 function gradientSum(da, resid, pos, kstr, kend, fac)
  implicit none
  real*4 da(3,3), resid(3,*), pos(3,*), fac
  real*8 dsum
  integer*4 kstr, kend, k, ixyz, i
  gradientSum = 0.
  do ixyz = 1, 3
    do k = kstr, kend
      dsum = 0.
      do i = 1, 3
        dsum = dsum + da(ixyz, i) * pos(i, k)
      enddo
      gradientSum = gradientSum + fac * dsum * resid(ixyz, k)
    enddo
  enddo
  return
end function gradientSum


! Initializes one variable to the given value, and if mapVar is > 0, it
! also initializes n-1 variables at that position in varList
!
subroutine initializeVar(varList, var, mapVar, numVol, value)
  implicit none
  real*4 varList(*), var(*), value
  integer*4 mapVar, numVol, i
  do i = 1, numVol
    var(i) = value
  enddo
  if (mapVar <= 0) return
  do i = 1, numVol - 1
    varList(i + mapVar - 1) = value
  enddo
  return
end subroutine initializeVar

! Unloads all variables from the VAR array
!
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
end subroutine unloadVars

! Unloads one variable from VARLIST into VAR if the mapping index
! mapVar is > 0.  N-1 variables are copied, and the Nth value is set
! so that the mean of all is valMean.
!
subroutine unloadOneVar(varList, var, mapVar, numVol, valMean)
  implicit none
  real*4 varList(*), var(*), valMean, varSum
  integer*4 mapVar, numVol, i
  if (mapVar <= 0) return
  varSum = 0.
  do i = 1, numVol - 1
    var(i) = varList(i + mapVar - 1)
    varSum = varSum + var(i)
  enddo
  var(numVol) = valMean * numVol - varSum
  return
end subroutine unloadOneVar

! Multiply the 4 component matrices (one of which may be a derivative)
! to form the full product matrix
!
subroutine fullMatProduct(dmat, zmat, ymat, xmat, prod)
  implicit none
  real*4 dmat(3,3), xmat(3,3), ymat(3,3), zmat(3,3), prod(3,3)
  call mat3dmul(dmat, zmat, prod)
  call mat3dmul(prod, ymat, prod)
  call mat3dmul(prod, xmat, prod)
  return
end subroutine fullMatProduct

! Fill the 3x3 matrices for rotation about X, Y, or Z axis or for the
! distortion terms.  Note that unlike tiltalign funct, all matrices are
! 3x3 and they are indexed as row, column not as 1-d across the rows
!
subroutine fillXmatrix(xmat, angle, cosAlpha, sinAlpha)
  implicit none
  real*4 xmat(3,3), angle, cosAlpha, sinAlpha
  call zeroMatrix(xmat)
  cosAlpha = cos(angle)
  sinAlpha = sin(angle)
  xmat(1, 1) = 1.
  xmat(2, 2) = cosAlpha
  xmat(3, 2) = sinAlpha
  xmat(3, 3) = cosAlpha
  xmat(2, 3) = -sinAlpha
  return
end subroutine fillXmatrix

subroutine fillYmatrix(ymat, angle, cosBeta, sinBeta)
  implicit none
  real*4 ymat(3,3), angle, cosBeta, sinBeta
  call zeroMatrix(ymat)
  cosBeta = cos(angle)
  sinBeta = sin(angle)
  ymat(2, 2) = 1.
  ymat(1, 1) = cosBeta
  ymat(1, 3) = sinBeta
  ymat(3, 3) = cosBeta
  ymat(3, 1) = -sinBeta
  return
end subroutine fillYmatrix

subroutine fillZmatrix(zmat, angle, cosGamma, sinGamma)
  implicit none
  real*4 zmat(3,3), angle, cosGamma, sinGamma
  call zeroMatrix(zmat)
  cosGamma = cos(angle)
  sinGamma = sin(angle)
  zmat(3, 3) = 1.
  zmat(1, 1) = cosGamma
  zmat(2, 1) = sinGamma
  zmat(2, 2) = cosGamma
  zmat(1, 2) = -sinGamma
  return
end subroutine fillZmatrix

! This a model of semi-natural params, divided by 2
!
subroutine fillDistMatrix(dmat, gmag, comp, dmag, skew, cosDelta, sinDelta)
  implicit none
  real*4 dmat(3,3), gmag, comp, dmag, skew, cosDelta, sinDelta
  call zeroMatrix(dmat)
  cosDelta = cos(skew)
  sinDelta = sin(skew)
  dmat(1, 1) = (gmag + dmag) * cosDelta
  dmat(2, 1) = -(gmag + dmag) * sinDelta
  dmat(1, 2) = -(gmag - dmag) * sinDelta
  dmat(2, 2) = (gmag - dmag) * cosDelta
  dmat(3, 3) = gmag * comp
  return
end subroutine fillDistMatrix

! Zero out a matrix
!
subroutine zeroMatrix(amat)
  implicit none
  real*4 amat(9)
  integer*4 i
  do i = 1, 9
    amat(i) = 0.
  enddo
  return
end subroutine zeroMatrix

! Multiples 3x3 matrix a1 times a2 ( a1 applied first) and places result
! in a3, which can be the same as a1
!
subroutine mat3dmul(a1, a2, a3)
  real*4 a1(3,3), a2(3,3), a3(3,3), at(3,3)
  ! write(*,'(a,9f8.4)') 'a1:', ((a1(i, j), j=1, 3), i=1, 3)
  ! write(*,'(a,9f8.4)') 'a2:', ((a2(i, j), j=1, 3), i=1, 3)
  do i = 1, 3
    do j = 1, 3
      at(i, j) = 0.
      do k = 1, 3
        at(i, j) = at(i, j) + a2(i, k) * a1(k, j)
      enddo
    enddo
  enddo
  do i = 1, 3
    do j = 1, 3
      a3(i, j) = at(i, j)
    enddo
  enddo
  ! write(*,'(a,9f8.4)') 'a3:', ((a3(i, j), j=1, 3), i=1, 3)
  return
end subroutine mat3dmul


! This routine takes values representing the amount that an upper piece
! is displaced from alignment across an edge, and resolves it to an
! amount to be applied to each volume (shift, rotation, etc) .
!
subroutine resolveEdgesToVolumes(edgeArray, maxCols, volArray, numCols, &
    icolStride, ivolStride)
  use stitchvars
  implicit none
  integer*4 maxCols, numCols, icolStride, ivolStride
  real*4 edgeArray(maxCols,*), volArray(*), sum
  integer*4 ivar, iexy, j, m, iv, jv

  ! the shifts in coordinates correspond to amount upper is displaced
  ! from alignment, as opposed to amount it needs to be shifted to be in
  ! alignment
  do ivar = 1, numVols - 1
    do m = 1, numVols - 1
      daa(m, ivar) = 0.
    enddo
    do j = 1, numCols
      dbb(j, ivar) = 0
    enddo

    do iexy = 1, 2
      !
      ! if this is an upper piece, find lower piece and edge, add 1 to
      ! its term and subtract 1 from lower piece's term.  Subtract
      ! displacements from the constant term
      iv = iVolLower(iexy, ivar)
      if (iv > 0) then
        daa(ivar, ivar) = daa(ivar, ivar) + 1
        if (iv .ne. numVols) then
          daa(iv, ivar) = daa(iv, ivar) - 1
        else
          do m = 1, numVols - 1
            daa(m, ivar) = daa(m, ivar) + 1
          enddo
        endif
        j = indVector(iexy, iv)
        do m = 1, numCols
          dbb(m, ivar) = dbb(m, ivar) - edgeArray(m, j)
        enddo
      endif
      !
      ! For a lower piece, set up coefficients the same but add the
      ! displacement.
      jv = iVolUpper(iexy, ivar)
      if (jv > 0) then
        daa(ivar, ivar) = daa(ivar, ivar) + 1
        if (jv .ne. numVols) then
          daa(jv, ivar) = daa(jv, ivar) - 1
        else
          do m = 1, numVols - 1
            daa(m, ivar) = daa(m, ivar) + 1
          enddo
        endif
        j = indVector(iexy, ivar)
        do m = 1, numCols
          dbb(m, ivar) = dbb(m, ivar) + edgeArray(m, j)
        enddo
      endif
    enddo
    ! write(*,'(8f6.0,3f10.2)') (daa(j, ivar), j=1, numVols-1), (dbb(j, ivar), j=1, 3)
  enddo
  !
  ! Get the solution and store the shifts
  call gaussj(daa, numVols - 1, MAXVOLS, dbb, numCols, 7)
  ! print *,'Solution:'
  ! write(*,'(8f6.3,3f10.2)') ((daa(j, ivar), j=1, numVols-1), (dbb(j, ivar), j=1, 3) &
  ! , ivar=1, numVols-1)
  do j = 1, numCols
    sum = 0
    do iv = 1, numVols - 1
      volArray(1 + (j - 1) * icolStride + (iv - 1) * ivolStride) = dbb(j, iv)
      sum = sum - dbb(j, iv)
    enddo
    volArray(1 + (j - 1) * icolStride + (numVols - 1) * ivolStride) = sum
  enddo
  return
end subroutine resolveEdgesToVolumes


! Transforms vector positions and lengths for the initial rotation and
! shift of the volumes, and computes corresponding positions in the two
! volumes to be used in the search.
!
subroutine transformVectors(rmat, volShift, edgeShift)
  use stitchvars
  implicit none
  real*4 rmat(3,3,*), volShift(3,*), edgeShift(3,*), ysum
  integer*4 iv, iexy, jv, j, k, kstr, kend, i, m
  do iv = 1, numVols
    do iexy = 1, 2
      jv = iVolUpper(iexy, iv)
      if (jv > 0) then
        j = indVector(iexy, iv)
        kstr = istrVector(j)
        kend = kstr + numVectors(j) - 1
        do k = kstr, kend
          call transformPos(center(1, k), rmat(1, 1, iv), volShift(1, iv), &
              nxyzIn(1, iv), nxyzOut, cenRot(1, k))
          do i = 1, 3
            ysum = 0.
            do m = 1, 3
              ysum = ysum + rmat(i, m, iv) * vector(m, k)
            enddo
            vecRot(i, k) = ysum + edgeShift(i, j) + volShift(i, jv) - &
                volShift(i, iv) + intervals(iexy, i)
            posLower(i, k) = scaleXyz * (cenRot(i, k) - nxyzOut(i) / 2.)
            posUpper(i, k) = posLower(i, k) +  &
                scaleXyz * (vecRot(i, k) - intervals(iexy, i))
            ! if (iv==1.and.iexy==2. .and. k < kstr+1) print *,i, k &
            ! center(i, k), vector(i, k), cenRot(i, k), vecRot(i, k), &
            ! posLower(i, k) /scaleXyz, posUpper(i, k) /scaleXyz, intervals(iexy, i), &
            ! ysum, edgeShift(i, j), volShift(i, iv), volShift(i, jv), &
            ! (rmat(i, m, iv), m=1, 3)
          enddo
        enddo
      endif
    enddo
  enddo
  return
end subroutine transformVectors


! Scales the results from a minimization and reports the results
!
subroutine scaleAndReport()
  use stitchvars
  implicit none
  integer*4 iexy, j, m, iv, jv, ivf, jvf, nsum, kstr, kend, k, i
  real*8 dsum, dsumSq
  real*4 distMax, avg, sd, dist
  !
  ! Scale radians to degrees and unscale the dxyz
  do iv = 1, numSolve
    do j = 4, 7
      volGeomVars(iv, j) = volGeomVars(iv, j) / dtor
    enddo
  enddo
  do iv = 1, numSolve
    do j = 1, 3
      dxyz(iv, j) = dxyz(iv, j) / scaleXyz
    enddo
  enddo
  write(6, 102)
102 format(/,' ix iy Alpha   Beta  Gamma    Mag    Comp    Dmag     Skew', &
      '    Dx     Dy     Dz')
  write(6, 103) (ixPiece(iVolFull(iv)), iyPiece(iVolFull(iv)), &
      alpha(iv), beta(iv), gamma(iv), gmag(iv), comp(iv), &
      dmag(iv), skew(iv), (dxyz(iv, i), i = 1, 3), iv = 1, numSolve)
103 format(2i3,f6.2,2f7.2,3f8.4,f8.2,3f7.1)
  !
  ! Get mean residuals
  dsum = 0.
  dsumSq = 0.
  nsum = 0.
  distMax = 0.
  do iv = 1, numSolve
    ivf = iVolFull(iv)
    do iexy = 1, 2
      jvf = iVolUpper(iexy, ivf)
      if (jvf > 0 .and. iVolSolve(jvf) > 0) then
        j = indVector(iexy, ivf)
        kstr = istrVector(j)
        kend = kstr + numVectors(j) - 1
        do k = kstr, kend
          dist = sqrt(resid(1, k)**2 + resid(2, k)**2 + resid(3, k)**2) / scaleXyz
          dsum = dsum + dist
          dsumSq = dsumSq + dist**2
          nsum = nsum + 1
          distMax = max(distMax, dist)
        enddo
      endif
    enddo
  enddo
  call sums_to_avgsd8(dsum, dsumSq, nsum, 1, avg, sd)
  write(*,107) avg, sd, distMax
107 format(/,'Residual error mean',f8.2,', SD:',f8.2,', max:',2f8.1)
  return
end subroutine scaleAndReport


! Determines properties of the grid of vectors in the original volume
! for edge IEDGE.
!
subroutine setVectorGrid(iedge)
  use stitchvars
  implicit none
  integer maxPos, iedge, lstr, lend, l, j, i, k, ipos, inlist, itmp
  integer*4 intMin, numPos, ix, iy, iz
  real*4 span, delMax, delta
  parameter (maxPos = 1000)
  integer*4 numPatchXyz(3), listpos(maxPos,3)

  ! make lists of X, Y, Z values
  lstr = istrVector(iedge)
  lend = lstr + numVectors(iedge) - 1
  numPatchXyz(1) = 0
  numPatchXyz(2) = 0
  numPatchXyz(3) = 0
  do l = lstr, lend
    do j = 1, 3
      ipos = nint(center(j, l))
      inlist = 0
      k = 1
      do while(inlist == 0 .and. k <= numPatchXyz(j))
        if (ipos == listpos(k, j)) inlist = k
        k = k + 1
      enddo
      if (inlist == 0) then
        if (numPatchXyz(j) > maxPos) call exitError( &
            'TOO MANY PATCH POSITIONS ALONG ONE AXIS FOR ARRAYS')
        numPatchXyz(j) = numPatchXyz(j) + 1
        listpos(numPatchXyz(j), j) = ipos
      endif
    enddo
  enddo
  !
  do i = 1, 3
    !
    ! sort the position list
    !
    do j = 1, numPatchXyz(i) - 1
      do k = j, numPatchXyz(i)
        if (listpos(j, i) > listpos(k, i)) then
          itmp = listpos(j, i)
          listpos(j, i) = listpos(k, i)
          listpos(k, i) = itmp
        endif
      enddo
    enddo
    !
    ! Find the minimum interval, the span, and deduce number and interval
    span = listpos(numPatchXyz(i), i) - listpos(1, i)
    intMin = span
    do j = 1, numPatchXyz(i) - 1
      intMin = min(intMin, listpos(j + 1, i) - listpos(j, i))
    enddo
    numPos = span / max(1, intMin) + 1
    delta = span / (max(numPos - 1, 1))
    !
    ! Maybe somehow the number of positions is too small by 1: so check
    ! that all positions are close to the defined spots
    delMax = 0.
    do j = 2, numPatchXyz(i)
      itmp = listpos(j, i) - listpos(1, i)
      ipos = nint(itmp / delta)
      delMax = max(delMax, abs(delta * ipos - itmp))
    enddo
    if (delMax > 0.25 * delta) numPos = numPos + 1
    !
    ! Set the parameters for this edge
    numVecGrid(i, iedge) = numPos
    vecGridStart(i, iedge) = listpos(1, i)
    vecGridDelta(i, iedge) = max(1., span / (max(numPos - 1, 1)))
    ! print *,'edge', iedge, ' axis', i, ' start delta num:', vecGridStart(i, &
    ! iedge) , vecGridDelta(i, iedge), numVecGrid(i, iedge)
  enddo
  numPos = numVecGrid(1, iedge) * numVecGrid(2, iedge) * numVecGrid(3, iedge)
  if (numPos + indG2Vbase > MAXVECS) call exitError( &
      'TOO MANY VECTOR GRID POSITIONS FOR ARRAYS')
  istrVecGrid(iedge) = indG2Vbase
  !
  ! Clear out the index
  do i = 1, numPos
    indGridToVec(i + indG2Vbase - 1) = 0
  enddo
  !
  ! Go through vectors filling in indices to them
  do l = lstr, lend
    ix = nint((center(1, l) - vecGridStart(1, iedge)) / vecGridDelta(1, iedge))
    iy = nint((center(2, l) - vecGridStart(2, iedge)) / vecGridDelta(2, iedge))
    iz = nint((center(3, l) - vecGridStart(3, iedge)) / vecGridDelta(3, iedge))
    itmp = indG2Vbase + ix + (iy + iz * numVecGrid(2, iedge)) * &
        numVecGrid(1, iedge)
    indGridToVec(itmp) = l
  enddo
  indG2Vbase = indG2Vbase + numPos
  return
end subroutine setVectorGrid


! Determines the volumes and edges that position XX, YY in volume IVOL
! falls into.  the number of volumes is placed in numPieces, the number
! of edges in X and Y in numEdges.  the actual volume numbers are placed
! in inPiece, the actual edge numbers in inEdge.  the index of the pieces
! below and above an edge (indexes in the inPiece list, not the actual
! volume numbers) are stored in inEdLower and inEdUpper, and an edge
! fraction is returned in edgefrac4.  Positions within each piece are
! returned in xInPiece, yInPiece.
! the position is assumed to lie in the given volume, which will be first
! on the list.  Another volume is added to the list only if the position
! falls within the limits of the edge between that volume and an existing
! volume on the list.  However, if both volumes on the two sides of an
! edge are on the list already, the edge is added regardless.
!
subroutine countEdges(xx, yy, ivol)
  use stitchvars
  implicit none
  real*4 xx, yy, xinp, yinp, xyinp(2)
  integer*4 ivol, iexy, i, j, jv, ifUp, iv, indPiece, indinp, indEdge
  equivalence (xinp, xyinp(1)), (yinp, xyinp(2))
  !
  numEdges(1) = 0
  numEdges(2) = 0
  numPieces = 1
  xInPiece(1) = xx
  yInPiece(1) = yy
  indinp = 1
  inPiece(1) = ivol
  do while (indinp <= numPieces)
    !
    ! Come into loop looking at next piece on list.
    ! Need to check each edge to see if point is in it, then add adjacent
    ! piece
    iv = inPiece(indinp)
    do iexy = 1, 2
      !
      ! Set values for this being a lower piece and edge is to upper
      ifUp = 0
      xinp = xInPiece(indinp)
      yinp = yInPiece(indinp)
      jv = iVolUpper(iexy, iv)
      if (jv .ne. 0) j = indVector(iexy, iv)
      if (xyinp(iexy) < nxyzOut(iexy) / 2) then
        !
        ! But if coordinate is low, check for lower piece instead
        ifUp = 1
        jv = iVolLower(iexy, iv)
        if (jv .ne. 0) j = indVector(iexy, jv)
        xinp = xinp + intervals(iexy, 1) + fullDxyz(1, iv) - fullDxyz(1, jv)
        yinp = yinp + intervals(iexy, 2) + fullDxyz(2, iv) - fullDxyz(2, jv)
      endif
      if (jv .ne. 0) then
        !
        ! See if piece is on list already
        indPiece = 0
        do i = 1, numPieces
          if (jv == inPiece(i)) indPiece = i
        enddo
        !
        ! Accept edge if in the edge or the other piece is on list already
        if (indPiece > 0 .or. (xinp >= extendedEdMin(1, j) &
            .and. xinp <= extendedEdMax(1, j) .and. yinp >= &
            extendedEdMin(2, j) .and. yinp <= extendedEdMax(2, j))) then
          !
          ! in an edge.
          ! Add piece if not already on list, and position in piece
          if (indPiece == 0) then
            numPieces = numPieces + 1
            indPiece = numPieces
            inPiece(indPiece) = jv
            if (ifUp == 0) then
              xInPiece(indPiece) = xinp - intervals(iexy, 1) + fullDxyz(1, iv) -  &
                  fullDxyz(1, jv)
              yInPiece(indPiece) = yinp - intervals(iexy, 2) + fullDxyz(2, iv) - &
                  fullDxyz(2, jv)
            else
              xInPiece(indPiece) = xinp
              yInPiece(indPiece) = yinp
            endif
          endif
          !
          ! Add edge if not already on list
          indEdge = 0
          do i = 1, numEdges(iexy)
            if (inEdge(i, iexy) == j) indEdge = i
          enddo
          if (indEdge == 0) then
            numEdges(iexy) = numEdges(iexy) + 1
            indEdge = numEdges(iexy)
            inEdge(indEdge, iexy) = j
            if (ifUp == 0) then
              inEdLower(indEdge, iexy) = indinp
              inEdUpper(indEdge, iexy) = indPiece
            else
              inEdLower(indEdge, iexy) = indPiece
              inEdUpper(indEdge, iexy) = indinp
            endif
            !
            ! Get the edge fraction based on position in band that this
            ! point is in along the edge
            i = (xyinp(3 - iexy) - edgeMin(3 - iexy, j)) / bandDel(j) + 1.
            i = max(1, min(numBands(j), i))
            if (bandMax(i, j) > bandMin(i, j)) then
              edgefrac4(indEdge, iexy) = (xyinp(iexy) - bandMin(i, j)) / &
                  (bandMax(i, j) - bandMin(i, j))
            else
              edgefrac4(indEdge, iexy) = (xyinp(iexy) - edgeMin(iexy, j)) / &
                  (edgeMax(iexy, j) - edgeMin(iexy, j))
            endif
          endif
        endif
      endif
    enddo
    indinp = indinp + 1
  enddo
  if (numPieces == 4 .and. numEdges(1) + numEdges(2) .ne. 4) print *, &
      'WARNING: Incomplete edge list:', xx, yy, numEdges(1), numEdges(2)
  ! if (numPieces > 1) print *,xx, yy, numPieces, numEdges(2), (inPiece(i), &
  ! xInPiece(i), yInPiece(i), i=1, 2), edgefrac4(1, 2)
  return
end subroutine countEdges


! Finds a warping vector at the position in the output volume given in
! the 3-element array IPOS, for volume IVOL.  the vector is returned in
! OUTVEC and NDIM is returned with the number of dimension used for
! interpolating the vector, or 0 if there is no vector available.
! the routine assumes that COUNTEDGES has already been run on this
! position in X and Y.  This routine is based as far as possible on
! the section of Blendmont that resolves edge functions for getting a
! pixel.
!
subroutine findWarpVector(ipos, ivol, outVec, ndim)
  use stitchvars
  implicit none
  integer*4 ipos(*), ivol, ndim
  real*4 outVec(*)
  real*4 pos(3), xpos, ypos, zpos, vec(3)
  equivalence (pos(1), xpos), (pos(2), ypos), (pos(3), zpos)
  integer*4 indp1, indp2, indp3, indp4, inde12, inde13, inde34, inde24, nactivep
  real*4 er3, eb3, el4, eb4, fx, fy, dr1, dt1, dl2, dt2, dr3, db3, dl4, db4, dla, dra
  real*4 dba, dta, ax, ay, ex, ey, wll, wlr, wul, wur, wsum, x1, y1, w1, w2, c11, c12, c21
  real*4 xgConst, ygConst, x1last, y1Last, z1Last
  integer*4 indpidef, indpl, indpu, ipiece1, ipiece2, iter, jndp1, jndp2, jndp3, ipc
  real*4 dx2, dy2, x2, y2, w3
  integer*4 ind12edg, ind13edg, icorType, ind23edg, ind14edg, ind43edg, ixy, ied
  real*4 x3, y3, x3t, y3t, z3t
  real*4 dy3, dx3, bx, by, emin, w4, pieceDxyz(2,4)
  integer*4 ipiece3, ipiece4, nxgr, nygr, indBray1, indBray2, jndp4, ndim2, ndim3
  integer*4 nEdgeSum, iedge, indLower, interval12x, interval12y, interval13x
  integer*4 interval13y, interval14x, interval14y, interval23x, interval23y
  integer*4 niter, indEdg, i, interval43x, interval43y, ixdbg, iydbg
  real*4 x4, y4, dx4, dy4, f34, f12, f13, f24, er1, et1, el2, et2
  real*4 x1234(4), y1234(4), z1234(4), z1, z2, z3, z4, dz2, dz3, dz4
  equivalence (x1234(1), x1), (x1234(2), x2), (x1234(3), x3), (x1234(4), x4)
  equivalence (y1234(1), y1), (y1234(2), y2), (y1234(3), y3), (y1234(4), y4)
  equivalence (z1234(1), z1), (z1234(2), z2), (z1234(3), z3), (z1234(4), z4)
  logical*4 active4(2,2), debug

  ixdbg = -1
  iydbg = -1
  debug = ipos(1) == ixdbg .and. ipos(2) == iydbg
  do i = 1, 3
    pos(i) = ipos(i)
    outVec(i) = 0.
  enddo
  niter = 10
  !
  ! look at each edge, make sure there is a non-extended vector available
  ! and if not, set edge fraction to limit and make the edge inactive
  nEdgeSum = 0
  do ixy = 1, 2
    do ied = 1, numEdges(ixy)
      iedge = inEdge(ied, ixy)
      indLower = inEdLower(ied, ixy)
      ! call interpolateVector(xInPiece(indLower), yInPiece(indLower), zpos, &
      ! inPiece(indLower), iedge, 0.7, 0, vec, ndim) &
      ! inPiece(indLower), iedge, 0.5, nIniExtend(ied, ixy), vec, ndim)
      ! if (debug) print *,xpos, ypos, zpos, inPiece(indLower), iedge, (vec(i), i=1, 3), ndim
      ! if (ndim == 0 .or. nIniExtend(ied, ixy) == 0) then
      ! if (ndim == 0) then
      ! if (edgefrac4(ied, ixy) < 0.5) then
      ! edgefrac4(ied, ixy) = 0.
      ! else
      ! edgefrac4(ied, ixy) = 1.
      ! endif
      ! endif

      active4(ied, ixy) = edgefrac4(ied, ixy) < .999 .and. edgefrac4(ied, ixy) > .001
      if (active4(ied, ixy)) nEdgeSum = nEdgeSum + 1
      if (edgefrac4(ied, ixy) < 0.) edgefrac4(ied, ixy) = 0.
      if (edgefrac4(ied, ixy) > 1.) edgefrac4(ied, ixy) = 1.
    enddo
  enddo
  if (debug) print *,'fractions', numPieces, nEdgeSum, ((edgefrac4(ied, ixy), &
      ied = 1, 2), ixy = 1, 2)

  ! Copy the Dxyz so they can be indexed by piece index below
  do i = 1, numPieces
    pieceDxyz(1:2, i) = fullDxyz(1:2, inPiece(i))
  enddo

  !
  ! get indices of pieces and edges: for now, numbers 1 to 4 represent lower left, lower
  ! right, upper left, and upper right
  !
  if (numPieces > 1) then
    indp1 = 0                                 !index of piece 1, 2, 3, or 4
    indp2 = 0                                 !if the point is in them
    indp3 = 0
    indp4 = 0
    inde12 = 0                                !index of edges between 1 and 2
    inde13 = 0                                !1 and 3, etc
    inde34 = 0
    inde24 = 0
    f12 = 0.                                  !edge fractions between 1 and 2
    f13 = 0.                                  !1 and 3, etc
    f34 = 0.
    f24 = 0.
    er1 = 0.                                  !end fractions to right and top
    et1 = 0.                                  !of piece 1, left and bottom of
    el2 = 0.                                  !piece 3, etc
    et2 = 0.
    er3 = 0.
    eb3 = 0.
    el4 = 0.
    eb4 = 0.
    fx = 0.                                   !the composite edge fractions
    fy = 0.                                   !in x and y directions
    if (numPieces > 2) then
      do ipc = 1, numPieces
        if (xInPiece(ipc) > nxOut / 2 .and. &
            yInPiece(ipc) > nyOut / 2) indp1 = ipc
        if (xInPiece(ipc) > nxOut / 2 .and. &
            yInPiece(ipc) < nyOut / 2) indp3 = ipc
        if (xInPiece(ipc) < nxOut / 2 .and. &
            yInPiece(ipc) > nyOut / 2) indp2 = ipc
        if (xInPiece(ipc) < nxOut / 2 .and. &
            yInPiece(ipc) < nyOut / 2) indp4 = ipc
      enddo
      do ied = 1, numEdges(1)
        if (inEdLower(ied, 1) == indp1) inde12 = ied
        if (inEdLower(ied, 1) == indp3) inde34 = ied
      enddo
      do ied = 1, numEdges(2)
        if (inEdLower(ied, 2) == indp1) inde13 = ied
        if (inEdLower(ied, 2) == indp2) inde24 = ied
      enddo
      if (inde12 .ne. 0) f12 = edgefrac4(inde12, 1)
      if (inde34 .ne. 0) f34 = edgefrac4(inde34, 1)
      if (inde13 .ne. 0) f13 = edgefrac4(inde13, 2)
      if (inde24 .ne. 0) f24 = edgefrac4(inde24, 2)
    else
      !
      ! two piece case - identify as upper or lower to simplify computation of end
      ! fractions
      !
      if (numEdges(1) > 0) then
        fx = edgefrac4(1, 1)
        outVec(4) = 1
        if (0.5 * (yInPiece(1) + yInPiece(2)) > nyOut / 2) then
          inde12 = 1
          if (xInPiece(1) > xInPiece(2)) then
            indp1 = 1
            indp2 = 2
          else
            indp1 = 2
            indp2 = 1
          endif
        else
          inde34 = 1
          fy = 1.
          if (xInPiece(1) > xInPiece(2)) then
            indp3 = 1
            indp4 = 2
          else
            indp3 = 2
            indp4 = 1
          endif
        endif
      else
        fy = edgefrac4(1, 2)
        outVec(4) = 2
        if (0.5 * (xInPiece(1) + xInPiece(2)) > nxOut / 2) then
          inde13 = 1
          if (yInPiece(1) > yInPiece(2)) then
            indp1 = 1
            indp3 = 2
          else
            indp1 = 2
            indp3 = 1
          endif
        else
          inde24 = 1
          fx = 1.
          if (yInPiece(1) > yInPiece(2)) then
            indp2 = 1
            indp4 = 2
          else
            indp2 = 2
            indp4 = 1
          endif
        endif
      endif
    endif
    !
    ! get distance to top, right, bottom, or left edges as needed for each piece, and
    ! compute end fractions
    !
    if (indp1 > 0) then
      dr1 = max(0., volLimTR(1, inPiece(indp1)) - xInPiece(indp1))
      dt1 = max(0., volLimTR(2, inPiece(indp1)) - yInPiece(indp1))
      er1 = (dr1 + 1.) / edgeWidthMean
      et1 = dt1 / edgeWidthMean
    endif
    if (indp2 > 0) then
      dl2 = max(0., xInPiece(indp2) - volLimTL(1, inPiece(indp2)))
      dt2 = max(0., volLimTL(2, inPiece(indp2)) - yInPiece(indp2))
      el2 = (dl2 + 1.) / edgeWidthMean
      et2 = (dt2 + 1.) / edgeWidthMean
    endif
    if (indp3 > 0) then
      dr3 = max(0., volLimBR(1, inPiece(indp3)) - xInPiece(indp3))
      db3 = max(0., yInPiece(indp3) - volLimBR(2, inPiece(indp3)))
      er3 = (dr3 + 1.) / edgeWidthMean
      eb3 = (db3 + 1.) / edgeWidthMean
    endif
    if (indp4 > 0) then
      dl4 = max(0., xInPiece(indp4) - volLimBL(1, inPiece(indp4)))
      db4 = max(0., yInPiece(indp4) - volLimBL(2, inPiece(indp4)))
      el4 = (dl4 + 1.) / edgeWidthMean
      eb4 = (db4 + 1.) / edgeWidthMean
    endif
    !
    ! if there are 4 pieces, fx and fy are weighted sum of the f's in the two
    ! overlapping edges.  the weights ex and ey are modified fractional distances
    ! across the overlap zone.  First get the distances to the borders of the overlap
    ! zone (dla, etc) and use them to get absolute fractional distances
    ! across zone, ax in the y direction and ay in the x direction.  They are used to
    ! make ex slide from being a distance across the lower overlap to being
    ! a distance across the upper overlap.  This gives continuity with the edge in 2 or
    ! 3-piece cases
    !
    if (numPieces == 4) then
      dla = min(dl2, dl4)
      dra = min(dr1, dr3)
      dba = min(db3, db4)
      dta = min(dt1, dt2)
      ax = dba / (dta + dba)
      ay = dla / (dra + dla)
      ex = ((1 - ay) * db3 + ay * db4) / &
          ((1 - ay) * (dt1 + db3) + ay * (dt2 + db4))
      ey = ((1 - ax) * dl2 + ax * dl4) / &
          ((1 - ax) * (dr1 + dl2) + ax * (dr3 + dl4))
      fx = (1 - ex) * f12 + ex * f34
      fy = (1 - ey) * f13 + ey * f24
    elseif (numPieces == 3) then
      !
      ! Three-piece case is simple, only two edges
      !
      fx = edgefrac4(1, 1)
      fy = edgefrac4(1, 2)
    endif
    !
    ! weighting factors are a product of the two f's, attenuated if necessary by
    ! fractional distance to end of piece, then normalized to sum to 1.
    !
    wll = min(1 - fx, et1) * min(1 - fy, er1)
    wlr = min(fx, et2) * min(1 - fy, el2)
    wul = min(1 - fx, eb3) * min(fy, er3)
    wur = min(fx, eb4) * min(fy, el4)
    wsum = wll + wlr + wul + wur
    if (wsum > 0.) then
      wll = wll / wsum
      wlr = wlr / wsum
      wul = wul / wsum
      wur = wur / wsum
    endif
    !
    ! count up active pieces implied by the w's
    !
    nactivep = 0
    if (wll > 0.) nactivep = nactivep + 1
    if (wlr > 0.) nactivep = nactivep + 1
    if (wul > 0.) nactivep = nactivep + 1
    if (wur > 0.) nactivep = nactivep + 1
    if (debug) print *,'weights', nactivep, wll, wlr, wul, wur
    !
    ! filter out cross-corner 2-piece cases, pick the piece where the point is most
    ! interior
    !
    if (nactivep == 2 .and. wll * wur > 0.) then
      if (min(dt1, dr1) < min(db4, dl4)) then
        wll = 0.
      else
        wur = 0.
      endif
      nactivep = 1
    elseif (nactivep == 2 .and. wul * wlr > 0.) then
      if (min(dt2, dl2) < min(db3, dr3)) then
        wlr = 0.
      else
        wul = 0.
      endif
      nactivep = 1
    endif
  else
    !
    ! the one-piece case, avoid all that computation
    !
    nactivep = 1
    indp1 = 1
    wll = 1.
  endif
  !
  ! NOW SORT OUT THE CASES OF 1, 2, 3 or 4 PIECES
  !
  if (nactivep <= 1) then                     !ONE piece
    if (wll > 0.) then
      indpidef = indp1
    elseif (wlr > 0.) then
      indpidef = indp2
    elseif (wul > 0.) then
      indpidef = indp3
    else
      indpidef = indp4
    endif
    ! if (nactivep==0) indpidef=1
    !
    ! not sure how there can be no active pieces, but it seems safer to return no vector
    ! than a zero vector there
    ndim = 3
    outVec(4) = 0
    if (indpidef .ne. 1 .or. nactivep == 0) ndim = 0
    return
    !
    ! ONE EDGE, TWO PIECES
    !
  elseif (nactivep == 2) then
    !
    ! find the pieces around the edge, set edge index
    ! If these intervals were used, one would need to be set in each case below, but they
    ! are not used.
    interval12x = 0
    interval12y = 0
    if (wll > 0. .and. wlr > 0.) then
      indpl = indp1
      indpu = indp2
      w1 = wll
      indEdg = inEdge(inde12, 1)
    elseif (wul > 0. .and. wur > 0.) then
      indpl = indp3
      indpu = indp4
      w1 = wul
      indEdg = inEdge(inde34, 1)
    elseif (wll > 0. .and. wul > 0.) then
      indpl = indp1
      indpu = indp3
      w1 = wll
      indEdg = inEdge(inde13, 2)
    else
      indpl = indp2
      indpu = indp4
      w1 = wlr
      indEdg = inEdge(inde24, 2)
    endif
    !
    ! set up pieces #'s, and starting coords
    !
    ipiece1 = inPiece(indpl)
    ipiece2 = inPiece(indpu)
    x1 = xInPiece(indpl)
    y1 = yInPiece(indpl)
    z1 = zpos
    w2 = 1. -w1
    !
    ! in this loop, we only need the position in lower piece and the vector, iterate
    ! until weighted mean of position in lower and upper piece stabilizes
    !
    x1last = -100.
    y1Last = -100.
    z1Last = -100.
    iter = 1
    do while(iter <= niter .and. (abs(x1 - x1last) > 0.01 .or. &
        abs(y1 - y1Last) > 0.01 .or. abs(z1 - z1Last) > 0.01))
      call interpolateVector(x1, y1, z1, ipiece1, indEdg, 0.7, 0, vec, ndim)
      if (ndim == 0) return
      x1last = x1
      y1Last = y1
      z1Last = z1
      x1 = xInPiece(indpl) - w2 * vec(1)
      y1 = yInPiece(indpl) - w2 * vec(2)
      z1 = zpos - w2 * vec(3)
      iter = iter + 1
    enddo
    !
    !
    if (indpl == 1) then
      w3 = -w2
    else if (indpu == 1) then
      w3 = w1
    else
      ndim = 0
      return
    endif
    do i = 1, 3
      outVec(i) = w3 * vec(i)
    enddo
    ! write(*,'(i3,9f8.1)') 2, xInPiece(1), yInPiece(1), zpos, &
    ! w1, w2, w3, (outVec(i), i=1, 3)
    !
    ! THREE PIECES AND TWO EDGES
    !
  elseif (nactivep == 3) then
    !
    ! now decide between the three cases of divergent, serial, or convergent functions,
    ! and assign pieces and weights accordingly
    !
    ! DIVERGENT: if lower piece is same for x and y edge
    !
    if (wur <= 0.) then
      w1 = wll
      w2 = wlr
      w3 = wul
      jndp1 = indp1
      jndp2 = indp2
      jndp3 = indp3
      ind12edg = inEdge(inde12, 1)
      ind13edg = inEdge(inde13, 2)
      icorType = 1
      interval12x = intervalX + pieceDxyz(1, jndp2) - pieceDxyz(1, jndp1)
      interval12y = pieceDxyz(2, jndp2) - pieceDxyz(2, jndp1)
      interval13x = pieceDxyz(1, jndp3) - pieceDxyz(1, jndp1)
      interval13y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2, jndp1)
      outVec(4) = 3
      !
      ! CONVERGENT: if upper piece same for x and y edge
      !
    elseif (wll <= 0.) then
      w3 = wur
      w1 = wul
      w2 = wlr
      jndp1 = indp3
      jndp2 = indp2
      jndp3 = indp4
      ind13edg = inEdge(inde34, 1)
      ind23edg = inEdge(inde24, 2)
      icorType = 2
      interval23x = pieceDxyz(1, jndp3) - pieceDxyz(1, jndp2)
      interval23y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2, jndp2)
      interval13x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1, jndp1)
      interval13y = pieceDxyz(2, jndp3) - pieceDxyz(2, jndp1)
      interval12x = intervalX + pieceDxyz(1, jndp2) - pieceDxyz(1, jndp1)
      interval12y = -intervalY + pieceDxyz(2, jndp2) - pieceDxyz(2, jndp1)
      outVec(4) = 4
      !
      ! SERIAL: lower of one is the upper of the other
      !
    else
      w1 = wll
      w3 = wur
      jndp1 = indp1
      jndp3 = indp4
      if (wlr <= 0.) then
        w2 = wul
        jndp2 = indp3
        ind12edg = inEdge(inde13, 2)
        ind23edg = inEdge(inde34, 1)
        interval12x = pieceDxyz(1, jndp2) - pieceDxyz(1, jndp1)
        interval12y = intervalY + pieceDxyz(2, jndp2) - pieceDxyz(2,jndp1)
        interval23x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1,jndp2)
        interval23y = pieceDxyz(2, jndp3) - pieceDxyz(2, jndp2)
        interval13x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1,jndp1)
        interval13y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2, jndp1)
        outVec(4) = 5
      else
        w2 = wlr
        jndp2 = indp2
        ind12edg = inEdge(inde12, 1)
        ind23edg = inEdge(inde24, 2)
        interval12x = intervalX + fullDxyz(1, jndp2) - pieceDxyz(1,jndp1)
        interval12y = pieceDxyz(2, jndp2) - pieceDxyz(2, jndp1)
        interval23x = pieceDxyz(1, jndp3) - pieceDxyz(1, jndp2)
        interval23y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2,jndp2)
        interval13x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1,jndp1)
        interval13y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2,jndp1)
        outVec(4) = 6
      endif
      icorType = 3
    endif

    ipiece1 = inPiece(jndp1)
    ipiece2 = inPiece(jndp2)
    ipiece3 = inPiece(jndp3)
    x1 = xInPiece(jndp1)
    y1 = yInPiece(jndp1)
    z1 = zpos
    x2 = xInPiece(jndp2)
    y2 = yInPiece(jndp2)
    z2 = zpos
    !
    ! set up to solve equations for new (x1, y1), (x2, y2)
    ! and (x3, y3) given the differences between them and
    ! the desired weighted coordinate
    !
    xgConst = x1 - w2 * interval12x - w3 * interval13x
    ygConst = y1 - w2 * interval12y - w3 * interval13y
    !
    ! do iteration, starting with coordinates and solving
    ! for new coordinates until convergence
    !
    x1last = -100.
    y1Last = -100.
    z1Last = -100.
    iter = 1
    do while(iter <= niter .and. (abs(x1 - x1last) > 0.01 .or. &
        abs(y1 - y1Last) > 0.01 .or. abs(z1 - z1Last) > 0.01))
      if (icorType == 1) then
        !
        ! divergent case
        !
        call interpolateVector(x1, y1, z1, ipiece1, ind12edg, 0.7, 0, vec, &
            ndim)
        x2 = x1 + vec(1) - interval12x
        y2 = y1 + vec(2) - interval12y
        z2 = z1 + vec(3)
        call interpolateVector(x1, y1, z1, ipiece1, ind13edg, 0.7, 0, vec, &
            ndim2)
        x3 = x1 + vec(1) - interval13x
        y3 = y1 + vec(2) - interval13y
        z3 = z1 + vec(3)

      elseif (icorType == 2) then
        !
        ! convergent case
        !
        call interpolateVector(x1, y1, z1, ipiece1, ind13edg, 0.7, 0, vec, &
            ndim)
        x3 = x1 + vec(1) - interval13x
        y3 = y1 + vec(2) - interval13y
        z3 = z1 + vec(3)
        call interpolateVector(x2, y2, z2, ipiece2, ind23edg, 0.7, 0, vec, &
            ndim2)
        x3t = x2 + vec(1) - interval23x
        y3t = y2 + vec(2) - interval23y
        z3t = z2 + vec(3)
        x2 = x2 + x3 - x3t
        y2 = y2 + y3 - y3t
        z2 = z2 + z3 - z3t
      else
        !
        ! serial case
        !
        call interpolateVector(x1, y1, z1, ipiece1, ind12edg, 0.7, 0, vec, &
            ndim)
        x2 = x1 + vec(1) - interval12x
        y2 = y1 + vec(2) - interval12y
        z2 = z1 + vec(3)
        ! write(*,'(9f8.1)') x1, y1, z1, (vec(i), i=1, 3), x2, y2, z2
        call interpolateVector(x2, y2, z2, ipiece2, ind23edg, 0.7, 0, vec, &
            ndim2)
        x3 = x2 + vec(1) - interval23x
        y3 = y2 + vec(2) - interval23y
        z3 = z2 + vec(3)
        ! write(*,'(9f8.1)') (vec(i), i=1, 3), x3, y3, z3
      endif
      if (ndim == 0 .or. ndim2 == 0) then
        ndim = 0
        return
      endif
      !
      ! solve equations for new coordinates
      !
      x1last = x1
      y1Last = y1
      z1Last = z1
      dx2 = x2 - x1
      dy2 = y2 - y1
      dz2 = z2 - z1
      dx3 = x3 - x1
      dy3 = y3 - y1
      dz3 = z3 - z1
      x1 = xgConst - w2 * dx2 - w3 * dx3
      y1 = ygConst - w2 * dy2 - w3 * dy3
      z1 = zpos - w2 * dz2 - w3 * dz3
      x2 = x1 + dx2
      y2 = y1 + dy2
      z2 = z1 + dz2
      x3 = x1 + dx3
      y3 = y1 + dy3
      z3 = z1 + dz3
      iter = iter + 1
    enddo
    if (jndp1 == 1) then
      indp1 = 1
    else if (jndp2 == 1) then
      indp1 = 2
    else if (jndp3 == 1) then
      indp1 = 3
    else
      ndim = 0
      return
    endif
    !
    ! Hope the sign is right here
    outVec(1) = x1234(indp1) - xInPiece(1)
    outVec(2) = y1234(indp1) - yInPiece(1)
    outVec(3) = z1234(indp1) - zpos
    ! write(*,'(2i3,9f8.1)') 3, icorType, xInPiece(1), yInPiece(1), zpos, &
    ! x1234(indp1), y1234(indp1), z1234(indp1), (outVec(i), i=1, 3)
  else
    !
    ! FOUR PIECES, THREE EDGES USED FOR SOLUTION
    !
    ! First, need to have only 3 active edges, so if there are four, knock one out based
    ! on ex and ey
    !
    if (nEdgeSum == 4) then
      emin = min(ex, 1. -ex, ey, 1. -ey)
      if (ex == emin) then
        active4(inde34, 1) = .false.
      elseif (1. -ex == emin) then
        active4(inde12, 1) = .false.
      elseif (ey == emin) then
        active4(inde24, 2) = .false.
      else
        active4(inde13, 2) = .false.
      endif
    endif
    !
    ! here there is always a serial chain from ll to ur, through either ul or lr, and in
    ! each case the fourth piece is either divergent from the first or converging on the
    ! fourth.  Assign the jndp1 and 3 for clarity
    !
    w1 = wll
    w3 = wur
    jndp1 = indp1
    jndp3 = indp4
    interval13x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1, jndp1)
    interval13y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2, jndp1)
    if (.not.active4(inde12, 1) .or. &
        .not.active4(inde24, 2)) then
      w2 = wul
      w4 = wlr
      jndp2 = indp3
      jndp4 = indp2
      ind12edg = inEdge(inde13, 2)
      ind23edg = inEdge(inde34, 1)
      if (.not.active4(inde24, 2)) then
        ind14edg = inEdge(inde12, 1)
        icorType = 1                           !divergent
        outVec(4) = 7
      else
        ind43edg = inEdge(inde24, 2)
        icorType = 2                           !convergent
        outVec(4) = 8
      endif
      interval12x = pieceDxyz(1, jndp2) - pieceDxyz(1, jndp1)
      interval12y = intervalY + pieceDxyz(2, jndp2) - pieceDxyz(2, jndp1)
      interval23x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1, jndp2)
      interval23y = pieceDxyz(2, jndp3) - pieceDxyz(2, jndp2)
      interval14x = intervalX + pieceDxyz(1, jndp4) - pieceDxyz(1, jndp1)
      interval14y = pieceDxyz(2, jndp4) - pieceDxyz(2, jndp1)
      interval43x = pieceDxyz(1, jndp3) - pieceDxyz(1, jndp4)
      interval43y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2, jndp4)
    else
      w2 = wlr
      w4 = wul
      jndp2 = indp2
      jndp4 = indp3
      ind12edg = inEdge(inde12, 1)
      ind23edg = inEdge(inde24, 2)
      if (.not.active4(inde34, 1)) then
        ind14edg = inEdge(inde13, 2)
        icorType = 1
        outVec(4) = 9
      else
        ind43edg = inEdge(inde34, 1)
        icorType = 2
        outVec(4) = 10
      endif
      interval12x = intervalX + pieceDxyz(1, jndp2) - pieceDxyz(1, jndp1)
      interval12y = pieceDxyz(2, jndp2) - pieceDxyz(2, jndp1)
      interval23x = pieceDxyz(1, jndp3) - pieceDxyz(1, jndp2)
      interval23y = intervalY + pieceDxyz(2, jndp3) - pieceDxyz(2, jndp2)
      interval14x = pieceDxyz(1, jndp4) - pieceDxyz(1, jndp1)
      interval14y = intervalY + pieceDxyz(2, jndp4) - pieceDxyz(2, jndp1)
      interval43x = intervalX + pieceDxyz(1, jndp3) - pieceDxyz(1, jndp4)
      interval43y = pieceDxyz(2, jndp3) - pieceDxyz(2, jndp4)
    endif
    !
    ipiece1 = inPiece(jndp1)
    ipiece2 = inPiece(jndp2)
    ipiece3 = inPiece(jndp3)
    ipiece4 = inPiece(jndp4)
    x1 = xInPiece(indp1)
    y1 = yInPiece(indp1)
    z1 = zpos
    x4 = xInPiece(jndp4)
    y4 = yInPiece(jndp4)
    z4 = zpos
    !
    ! set up to solve equations for new (x1, y1), (x2, y2)
    ! (x3, y3), and (x4, y4) given the differences between
    ! them and the desired weighted coordinate
    !
    xgConst = x1 - w2 * interval12x - w3 * interval13x - w4 * interval14x
    ygConst = y1 - w2 * interval12y - w3 * interval13y - w4 * interval14y
    !
    ! do iteration, starting with coordinates and solving
    ! for new coordinates until convergence
    !
    x1last = -100.
    y1Last = -100.
    z1Last = -100.
    iter = 1
    do while(iter <= niter .and. (abs(x1 - x1last) > 0.01 .or. &
        abs(y1 - y1Last) > 0.01 .or. abs(z1 - z1Last) > 0.01))
      call interpolateVector(x1, y1, z1, ipiece1, ind12edg, 0.7, 0, vec, &
          ndim)
      x2 = x1 + vec(1) - interval12x
      y2 = y1 + vec(2) - interval12y
      z2 = z1 + vec(3)
      if (debug) write(*,'(2i3,9f8.1)') 1, 2, x1, y1, z1, (vec(i), i = 1, 3), x2, &
          y2, z2
      call interpolateVector(x2, y2, z2, ipiece2, ind23edg, 0.7, 0, vec, &
          ndim2)
      x3 = x2 + vec(1) - interval23x
      y3 = y2 + vec(2) - interval23y
      z3 = z2 + vec(3)
      if (debug) write(*,'(2i3,9f8.1)') 2, 3, (vec(i), i = 1, 3), x3, y3, z3
      if (icorType == 1) then
        !
        ! divergent case
        !
        call interpolateVector(x1, y1, z1, ipiece1, ind14edg, 0.7, 0, vec, &
            ndim3)
        x4 = x1 + vec(1) - interval14x
        y4 = y1 + vec(2) - interval14y
        z4 = z1 + vec(3)
        if (debug) write(*,'(2i3,9f8.1)') 1, 4, (vec(i), i = 1, 3), x4, y4, z4
      else
        !
        ! convergent case
        !
        call interpolateVector(x4, y4, z4, ipiece4, ind43edg, 0.7, 0, vec, &
            ndim3)
        x3t = x4 + vec(1) - interval43x
        y3t = y4 + vec(2) - interval43y
        z3t = z4 + vec(3)
        if (debug) write(*,'(2i3,9f8.1)') 4, 3, x4, y4, z4, (vec(i), i = 1, 3), &
            x3t, y3t, z3t
        x4 = x4 + x3 - x3t
        y4 = y4 + y3 - y3t
        z4 = z4 + z3 - z3t
      endif
      !
      ! solve equations for new coordinates
      !
      if (ndim * ndim2 * ndim3 == 0) then
        ndim = 0
        return
      endif
      x1last = x1
      y1Last = y1
      z1Last = z1
      dx2 = x2 - x1
      dy2 = y2 - y1
      dz2 = z2 - z1
      dx3 = x3 - x1
      dy3 = y3 - y1
      dz3 = z3 - z1
      dx4 = x4 - x1
      dy4 = y4 - y1
      dz4 = z4 - z1
      x1 = xgConst - w2 * dx2 - w3 * dx3 - w4 * dx4
      y1 = ygConst - w2 * dy2 - w3 * dy3 - w4 * dy4
      z1 = zpos - w2 * dz2 - w3 * dz3 - w4 * dz4
      x2 = x1 + dx2
      y2 = y1 + dy2
      z2 = z1 + dz2
      x3 = x1 + dx3
      y3 = y1 + dy3
      z3 = z1 + dz3
      x4 = x1 + dx4
      y4 = y1 + dy4
      z4 = z1 + dz4
      iter = iter + 1
    enddo
    if (jndp2 == 1) then
      indp1 = 2
    else if (indp4 == 1) then
      indp1 = 3
    else if (jndp4 == 1) then
      indp1 = 4
    endif
    !
    ! Hope the sign is right here
    outVec(1) = x1234(indp1) - xInPiece(1)
    outVec(2) = y1234(indp1) - yInPiece(1)
    outVec(3) = z1234(indp1) - zpos
    ! write(*,'(2i3,9f8.1)') 4, icorType, xInPiece(1), yInPiece(1), zpos, &
    ! x1234(indp1), y1234(indp1), z1234(indp1), (outVec(i), i=1, 3)
  endif
  return
end subroutine findWarpVector


! Finds a vector at position XPOS, YPOS, ZPOS in volume IVOL and edge
! IEDGE by interpolation from the transformed edge vectors.  EXTRAPLIM
! sets the limit for extrapolation; namely the sum of extrapolation
! fractions in the 3 dimensions must be less than this amount.  Beyond
! the extrapolation limit, a value is extended in the Z direction by
! taking the value at the extrapolation limit.  if nExtendXY is greater
! than 0 it allows extension by up this number of grid positions in X
! and Y, where the value is taken from the nearest grid position.  This
! had no merit.  the vector is returned in VEC and NDIM is returned with
! 3 if the vector are determined from a trilinear interpolation, 2 if
! it is determined by bilinear interpolation from one layer of vectors
! in Z, 1 if is is determined by extension, or 0 if no vector is
! returned.
!
subroutine interpolateVector(xpos, ypos, zpos, ivol, iedge, extrapLim, &
    nExtendXY, vec, ndim)
  use stitchvars
  implicit none
  integer*4 ivol, iedge, ndim, nExtendXY
  real*4 xpos, ypos, zpos, pos(3), vec(3), extrapLim, back(3)
  logical*4 completeCube, completeSquare
  integer*4 ixg, iyg, izg, igrd(3), nxg, nyg, nzg, istr, ix, iy, iz, idir
  equivalence (ixg, igrd(1)), (iyg, igrd(2)), (izg, igrd(3))
  real*4 vgStartX, vgStartY, vgStartZ, vgDeltaX, vgDeltaY, vgDeltaZ
  real*4 fx, fy, fz, fxOut, fyOut, fzOut, fxMax, fxOutMin, dz, dz2
  integer*4 minDir, ind2, i, ind, minInd
  real*4 f11, f12, f21, f22, dist, distMin
  integer*4 idx(6) /-1, 1, 0, 0, 0, 0/, idy(6) /0, 0, -1, 1, 0, 0/
  integer*4 idz(6) /0, 0, 0, 0, -1, 1/
  logical*4 debug
  !
  ! backtransform position to the original volume for looking up in grid
  pos(1) = xpos
  pos(2) = ypos
  pos(3) = zpos
  debug = xpos == -10000. .and. ypos == -10000.
  call transformPos(pos, fInvMat(1, 1, ivol), fInvDxyz(1, ivol), nxyzOut, &
      nxyzIn(1, ivol), back)
  nxg = numVecGrid(1, iedge)
  nyg = numVecGrid(2, iedge)
  nzg = numVecGrid(3, iedge)
  istr = istrVecGrid(iedge)
  vgStartX = vecGridStart(1, iedge)
  vgStartY = vecGridStart(2, iedge)
  vgStartZ = vecGridStart(3, iedge)
  vgDeltaX = vecGridDelta(1, iedge)
  vgDeltaY = vecGridDelta(2, iedge)
  vgDeltaZ = vecGridDelta(3, iedge)
  vec(1) = 0.
  vec(2) = 0.
  vec(3) = 0.
  !
  ! Get truncated constrained position, which could be lower left corner of
  ! cube that point is in
  do i = 1, 3
    igrd(i) = min(numVecGrid(i, iedge) - 1, max(1, 1 + &
        int((back(i) - vecGridStart(i, iedge)) / vecGridDelta(i, iedge))))
  enddo
  if (debug) write(*,'(2i4,6f9.2,3i3)') ivol, iedge, xpos, ypos, zpos, &
      (back(i), i = 1, 3), (igrd(i), i = 1, 3)
  ndim = 0
  !
  ! Get the interpolation factors and the fractions that the point is
  ! outside in each direction
  fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
  fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
  fxOut = abs(fx - 0.5) - 0.5
  fyOut = abs(fy - 0.5) - 0.5
  !
  ! Handle one-layer case
  if (nzg == 1) then
    !
    ! Reset iz to 1, set flag, find out if in a complete square
    izg = 1
    if (completeSquare(ixg, iyg, izg, indGridToVec(istr), nxg, nyg)) then
      ndim = 1
      if (fxOut <= 0. .and. fyOut <= 0.) ndim = 2
    endif
  endif
  !
  ! Next see if closest cube is complete
  if (ndim == 0) then
    if (completeCube(ixg, iyg, izg, indGridToVec(istr), nxg, nyg)) then
      ndim = 1
      fz = (back(3) - (vgStartZ + (izg - 1) * vgDeltaZ)) / vgDeltaZ
      fzOut = abs(fz - 0.5) - 0.5
      if (debug) write(*,'(a,6f9.4)') 'complete', fx, fy, fz, fxOut, fyOut, fzOut
      if ((fxOut <= 0. .and. fyOut <= 0.) .or. (max(0., fxOut) + &
          max(0., fyOut) + max(0., fzOut) <= extrapLim)) ndim = 3
    endif
  endif

  if (ndim == 0) then
    minDir = 0
    !
    ! Loop over 6 directions from central cube
    do idir = 1, 6
      ix = ixg + idx(idir)
      iy = iyg + idy(idir)
      iz = izg + idz(idir)
      if (ix > 0 .and. ix < nxg .and. iy > 0 .and. iy < nyg &
          .and. iz > 0 .and. iz < nzg) then
        if (completeCube(ix, iy, iz, indGridToVec(istr), nxg, nyg)) then
          !
          ! if cube is legal and complete, get the interpolation fractions
          fx = (back(1) - (vgStartX + (ix - 1) * vgDeltaX)) / vgDeltaX
          fy = (back(2) - (vgStartY + (iy - 1) * vgDeltaY)) / vgDeltaY
          fz = (back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
          !
          ! Get the fractions that they are outside in each direction
          fxOut = max(0., abs(fx - 0.5) - 0.5)
          fyOut = max(0., abs(fy - 0.5) - 0.5)
          fzOut = max(0., abs(fz - 0.5) - 0.5)
          fxMax = fxOut + fyOut + fzOut
          if ((fxOut <= 0. .and. fyOut <= 0.) .or. &
              fxMax <= extrapLim) then
            !
            ! if it is within bounds in X/Y, or within bounds in Z and X or
            ! Y and within the extrapolation fraction in Y or X, then it is
            ! an acceptable one, so keep track of one with the maximum
            ! fraction out of bounds
            if (minDir == 0 .or. fxMax < fxOutMin) then
              fxOutMin = fxMax
              minDir = idir
            endif
          endif
        endif
      endif
    enddo
    !
    ! if one was found, set cube there
    if (minDir > 0) then
      ndim = 3
      ixg = ixg + idx(minDir)
      iyg = iyg + idy(minDir)
      izg = izg + idz(minDir)
      if (debug) write(*,'(a,6f9.4)') 'nearest', fx, fy, fz, fxOut, fyOut, fzOut
    endif
  endif
  !
  ! Now if it is still not there, look up and down in Z progressively
  ! farther away, but require being in X/Y range
  if (ndim == 0) then
    fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
    fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
    if (abs(fx - 0.5) > 0.5 .or. abs(fy - 0.5) > 0.5) ndim = 1
    if (debug) print *,'ndim', ndim, fx, fy
    idir = 0
    do while (idir < nzg - 1 .and. ndim == 0)
      !
      ! First test squares below and above, pick the closest one if one
      ! exists
      iz = izg - idir
      dz = -1.
      if (iz > 0) then
        if (completeSquare(ixg, iyg, iz, indGridToVec(istr), nxg, nyg)) &
            dz = abs(back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
      endif
      iz = izg + 1 + idir
      if (iz <= nzg) then
        if (completeSquare(ixg, iyg, iz, indGridToVec(istr), nxg, nyg)) &
            then
          dz2 = abs(back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
          if (dz < 0. .or. dz2 < dz) then
            izg = iz
            ndim = 2
          endif
        endif
      endif
      if (ndim == 0 .and. dz >= 0.) then
        izg = izg - idir
        ndim = 2
      endif
      !
      ! Next test cubes farther away
      if (ndim == 0) then
        dz = -1.
        iz = izg - 2 - idir
        if (iz > 0) then
          if (completeCube(ixg, iyg, iz, indGridToVec(istr), nxg, nyg)) &
              dz = abs(back(3) - (vgStartZ + iz * vgDeltaZ)) / vgDeltaZ
        endif
        iz = izg + 2 + idir
        if (iz < nzg) then
          if (completeCube(ixg, iyg, iz, indGridToVec(istr), nxg, nyg)) then
            dz2 = abs(back(3) - (vgStartZ + (iz - 1) * vgDeltaZ)) / vgDeltaZ
            if (dz < 0. .or. dz2 < dz) then
              izg = iz
              ndim = 2
            endif
          endif
        endif
        if (ndim == 0 .and. dz >= 0.) then
          izg = izg - 2 - idir
          ndim = 2
        endif
      endif
      idir = idir + 1
    enddo
  endif
  if (debug) print *,'ndim', ndim
  !
  ! Here we are at last with something theoretically usable
  if (ndim == 3) then
    fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
    fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
    fz = (back(3) - (vgStartZ + (izg - 1) * vgDeltaZ)) / vgDeltaZ
    !
    ! Extrapolate by at most the given fraction
    fx = max(-extrapLim, min(1 + extrapLim, fx))
    fy = max(-extrapLim, min(1 + extrapLim, fy))
    fz = max(-extrapLim, min(1 + extrapLim, fz))
    ind = istr + ixg - 1 + (iyg - 1 + (izg - 1) * nyg) * nxg
    ind2 = ind + nxg * nyg
    f11 = (1. - fx) * (1. - fy)
    f12 = (1. - fx) * fy
    f21 = fx * (1. - fy)
    f22 = fx * fy
    do i = 1, 3
      vec(i) =  (1. - fz) * (f11 * vecRot(i, indGridToVec(ind)) &
          + f12 * vecRot(i, indGridToVec(ind + nxg)) &
          + f21 * vecRot(i, indGridToVec(ind + 1)) &
          + f22 * vecRot(i, indGridToVec(ind + 1 + nxg))) &
          + fz * (f11 * vecRot(i, indGridToVec(ind2)) &
          + f12 * vecRot(i, indGridToVec(ind2 + nxg)) &
          + f21 * vecRot(i, indGridToVec(ind2 + 1)) &
          + f22 * vecRot(i, indGridToVec(ind2 + 1 + nxg)))
    enddo
  else if (ndim == 2) then
    fx = (back(1) - (vgStartX + (ixg - 1) * vgDeltaX)) / vgDeltaX
    fy = (back(2) - (vgStartY + (iyg - 1) * vgDeltaY)) / vgDeltaY
    ind = istr + ixg - 1 + (iyg - 1 + (izg - 1) * nyg) * nxg
    do i = 1, 3
      vec(i) =  (1. - fx) * (1. - fy) * vecRot(i, indGridToVec(ind)) &
          + (1. - fx) * fy * vecRot(i, indGridToVec(ind + nxg)) &
          + fx * (1. - fy) * vecRot(i, indGridToVec(ind + 1)) &
          + fx * fy * vecRot(i, indGridToVec(ind + 1 + nxg))
    enddo
  else if (nExtendXY > 0) then
    !
    ! Extend if allowed: just find nearest vector
    do i = 1, 3
      igrd(i) = min(numVecGrid(i, iedge), max(1, 1 + &
          nint((back(i) - vecGridStart(i, iedge)) / vecGridDelta(i, iedge))))
    enddo
    minInd = istr + ixg - 1 + (iyg - 1 + (izg - 1) * nyg) * nxg
    !
    ! if the nominally closest one does not exist, do real search
    if (indGridToVec(minInd) == 0) then
      minInd = 0
      distMin = 1.e20
      do ix = ixg - nExtendXY, ixg + nExtendXY
        do iy = iyg - nExtendXY, iyg + nExtendXY
          do iz = izg - nExtendXY, izg + nExtendXY
            if (ix > 1 .and. ix <= numVecGrid(1, iedge) .and. &
                iy > 1 .and. iy <= numVecGrid(2, iedge) .and. &
                iz > 1 .and. iz <= numVecGrid(3, iedge)) then
              ind = istr + ix - 1 + (iy - 1 + (iz - 1) * nyg) * nxg
              i = indGridToVec(ind)
              if (i > 0) then
                dist = (pos(1) - cenRot(1, i))**2 + &
                    (pos(2) - cenRot(2, i))**2 + (pos(3) - cenRot(3, i))**2
                if (dist < distMin) then
                  distMin = dist
                  minInd = ind
                endif
              endif
            endif
          enddo
        enddo
      enddo
      if (debug) print *,'search', minInd, sqrt(distMin)
    endif
    if (minInd > 0) then
      do i = 1, 3
        vec(i) = vecRot(i, indGridToVec(minInd))
      enddo
      ndim = 1
      i = indGridToVec(minInd)
      if (debug) write(*,'(2i6,3f8.1)') minInd, i, (cenRot(ix, i), ix = 1, 3)
    else
      ndim = 0
    endif
    !
  else
    ndim = 0
  endif
  if (debug) write(*,'(i2,3f8.2)') ndim, (vec(i), i = 1, 3)
  return
end subroutine interpolateVector


! Tests for whether position with indexes IX, IY, IZ is the lower left
! corner of a complete cube of data, where indG2V is the index from grid
! position to actual vectors, and NXG and NYG are the dimensions of that
! piece of the index array.
!
logical*4 function completeCube(ix, iy, iz, indG2V, nxg, nyg)
  implicit none
  integer*4 ix, iy, iz, nxg, nyg, indG2V(nxg,nyg,*)
  completeCube = indG2V(ix, iy, iz) > 0 .and. indG2V(ix + 1, iy, iz) > 0 &
      .and. indG2V(ix, iy + 1, iz) > 0 .and. indG2V(ix, iy, iz + 1) > 0 &
      .and. indG2V(ix + 1, iy + 1, iz) > 0 .and. indG2V(ix + 1, iy, iz + 1) > 0 &
      .and. indG2V(ix, iy + 1, iz + 1) > 0 .and. indG2V(ix + 1, iy + 1, iz + 1) > 0
  return
end function completeCube


! Tests for whether position with indexes IX, IY, IZ is the lower left
! corner of a complete square of data in the IZ plane.
!
logical*4 function completeSquare(ix, iy, iz, indG2V, nxg, nyg)
  implicit none
  integer*4 ix, iy, iz, nxg, nyg, indG2V(nxg,nyg,*)
  completeSquare = indG2V(ix, iy, iz) > 0 .and. indG2V(ix + 1, iy, iz) > 0 &
      .and. indG2V(ix, iy + 1, iz) > 0 .and. indG2V(ix + 1, iy + 1, iz) > 0
  return
end function completeSquare


! Transforms the position POS with the 3x3 matrix RMAT and the
! displacements in DXYZ, where NXYZIN and NXYZOUT arethe sizes of the
! source and destination volumes.  the transformed vector is in OUT.
!
subroutine transformPos(pos, rmat, dxyz, nxyzIn, nxyzOut, out)
  implicit none
  real*4 pos(3), rmat(3,3), dxyz(3), out(3), dx, dy, dz
  integer*4 nxyzIn(3), nxyzOut(3), i
  dx = pos(1) - nxyzIn(1) / 2.
  dy = pos(2) - nxyzIn(2) / 2.
  dz = pos(3) - nxyzIn(3) / 2.
  do i = 1, 3
    out(i) = rmat(i, 1) * dx + rmat(i, 2) * dy + rmat(i, 3) * dz + dxyz(i) + &
        nxyzOut(i) / 2.
  enddo
  return
end subroutine transformPos


! Composes a filename from the root of NAMEIN and the extension EXT
!
subroutine rootAndExtension(nameIn, ext, nameOut)
  implicit none
  character*(*) nameIn, ext, nameOut
  integer*4 i, lenroot
  lenroot = len_trim(nameIn)
  i = lenroot
  do while (i > 2 .and. lenroot == len_trim(nameIn))
    if (nameIn(i:i) == '.') lenroot = i - 1
    i = i - 1
  enddo
  nameOut = nameIn(1:lenroot) //'.'//trim(ext)
  return
end subroutine rootAndExtension


! Looks up whether a piece exists
!
logical function pieceExists(ix, iy)
  use stitchvars
  implicit none
  integer*4 ix, iy, iv
  pieceExists = .true.
  do iv = 1, numVols
    if (ixPiece(iv) == ix .and. iyPiece(iv) == iy) return
  enddo
  pieceExists = .false.
  return
end function pieceExists


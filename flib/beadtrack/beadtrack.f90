! * * * * * BEADTRACK * * * * * *
!
! BEADTRACK will track selected fiducial gold beads through a series of
! tilted views.  It takes a "seed" model, where each bead of choice
! is marked with at least a single point on a view near zero tilt.  It
! tracks each bead as far as possible and puts out a new model.
!
! David Mastronarde, 1995
! added tilt alignment, 10 / 6 / 97
!
! $Id$
!
program beadtrack
  use tltcntrl
  use mapSep
  use cgPixels
  implicit none
  include 'smallmodel.inc90'
  !
  integer npad, maxarr, limPcList, maxArea, limGaps, maxPeaks, msizeXR
  integer limInside, limEdge, maxOlist, limResid, maxAllReal
  character*80 titlech

  integer*4 nx, ny, nz, nxyz(3), mxyz(3)
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  !
  real*4 title(20), matKernel(49)
  real*4, allocatable :: boxes(:,:,:), curSum(:), boxTmp(:), corrSum(:,:), sobelSum(:,:)
  real*4, allocatable :: array(:), BRRAY(:), sarray(:), sbrray(:), sobelEdgeSD(:)
  real*4, allocatable :: refSobel(:), tmpSobel(:), boxSobel(:), sobelWsums(:)
  real*4, allocatable :: sobelXpeaks(:), sobelYpeaks(:), sobelPeaks(:), prexf(:,:,:)
  ! maxAllReal
  real*4, allocatable :: seqDist(:), xxtmp(:), yytmp(:), xvtmp(:), yvtmp(:)
  integer*4, allocatable :: indgap(:), numResSaved(:)
  logical*1, allocatable :: inAnArea(:)
  !
  integer*4, allocatable :: ixPclist(:), iyPclist(:), izPclist(:), listz(:), inCore(:,:)
  CHARACTER*320 filin, outFile, pieceFile, modelFile, elongFile, XYZfile, prexfFile
  ! maxView
  integer*4, allocatable :: ipClose(:), izClose(:)
  integer*4, allocatable :: ivList(:), ivSnapList(:)
  logical, allocatable :: missing(:)
  real*4, allocatable :: wsumSave(:,:), prevRes(:), edgeSDsave(:,:), elongSave(:,:)
  integer*1, allocatable :: isIsolated(:,:)
  character*6 areaObjStr
  character*9 date
  character*8 tim
  logical exist, readSmallMod
  !
  ! maxreal
  real*4, allocatable :: xseek(:), yseek(:), wsumCrit(:), xr(:,:)
  integer*4, allocatable :: ifFound(:), ipNearest(:), ipNearSave(:)
  integer*4, allocatable :: iobjDel(:), idrop(:), numInSobelSum(:)
  logical, allocatable :: inCorrSum(:,:), inSobelSum(:,:)

  real*4, allocatable :: resMean(:,:)
  ! maxArea
  integer*4, allocatable :: iareaSeq(:), ninObjList(:), indObjList(:)
  real*4, allocatable :: areaDist(:)
  integer*4, allocatable :: ivSeqStr(:), ivSeqEnd(:), listSeq(:)
  integer*4, allocatable :: numWneighbors(:), neighborsForWfits(:,:)
  !
  integer*2, allocatable :: ivGap(:)
  ! maxOlist
  real*4, allocatable :: residLists(:), xyzAllArea(:,:)
  integer*4, allocatable :: iobjLists(:), iobjLisTmp(:), iobjMap(:)
  real*4 ctf(8193)
  character*1024 listString
  character*60 addfmt, delfmt1, delfmt2
  character*14 objfmt
  character*20 objNames(5) /'Model projection', 'Fitted point', &
      'Correlation peak' , 'Centroid', 'Saved point'/
  !
  real*4 xf(2,3)
  real*4 colors(3,10) /0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, &
      1.0, 0.0, 0.0, 0.0, 1.0, 0.5, 0.2, 0.2, 0.8, 0.8, 0.2, 0.2, 0.9, 0.6, 0.4, &
      0.6, 0.4, 0.9/
  !
  integer*4 modeBox, maxWavg, limPtsStretch, limPtsMag, limPtsRot, limPtsShift
  real*4 rotStart, fracCrit, dmin2, dmax2, dmean2
  integer*4 ifWhite, ifFillIn, maxgap, mode, k, kti, lastSeq, iobjDo, ip, igap
  integer*4 numRounds, maxSum, numFit, minFit, maxResid, minResid, ierr, npclist, i, j
  real*4 sdCrit, distCrit, relaxInt, relaxDist, fitDistCrit, relaxFit, radMaxFit
  real*4 resDiffMin, resDiffCrit, tiltFitMin, cgRadius, tiltMin, xst, xnd, yst, ynd
  integer*4 minXpiece, nxPieces, nxOverlap, minYpiece, nyPieces, nyOverlap
  integer*4 nxTotPix, nyTotPix, ig, nxLocal, nyLocal
  integer*4 nxBox, nyBox, nxpad, nypad, npixBox, nxpDim, ifTrace, iv
  integer*4 minEndZ, indFree, iobj, ibase, numInObj, ipt, iz, jz, jpt, itmp, iznext
  integer*4 maxnpt, numAreaX, numAreaY, ix, iy, nobjLists
  integer*4 indStart, numObjTot, numSeqs, ipass, limcg, nzout, nxTaper, nyTaper
  real*4 radPix, boxSum, boxMin, boxMax, refSum, refMin, refMax, corroSum, corrMin
  real*4 corrMax, xbox, ybox, xt, yt, xtmp, ytmp, wsum, tiltCur, dxCur, taperFrac
  integer*4 lastList, iseq, numAdded, ivSeq, idir, nvList, iview, ivuse, ib
  integer*4 ifExclude, iexcl, ivl, numToDo, nClose, nearDiff, izv, ipcz
  integer*4 ix0, ix1, iy0, iy1, ic, jc, maxBoxUse, ibox, izBox, idebugObj
  integer*4 ifDidAlign, ivDel, indr, numData, numAvg, idif, idirw, itry
  real*4 rotCur, dyCur, cosr, sinr, cost, sint, a, b, c, d, e, f, wsumsq, wstmp
  real*4 xnext, ynext, relax, radMax, xpeak, ypeak, xpos, ypos, devAvg, devSD
  real*4 devMax, errMax, curDiff, resAvg, resSD, resSEM, gmagCur, wsumAvg, wsumSD
  integer*4 numPioneer, ifTrans, ifRoTrans, ipntMaxDev, numAddTmp, ivLook
  integer*4 ifMeanBad, isCur, nprev, ndel, missTot, numListZ, imodObj, imodCont
  real*4 peak, dist, tmin, tmax, tmean, cvbxcen, cvbycen, area, density, elongSigma
  integer*4 numVert, minInArea, minBeadOverlap, ifLocalArea, localTarget
  integer*4 nvLocalIn, localViewPass, nSnapList, iobjSave
  logical*4 keepGoing, saveAllPoints, ignoreObjs, done, needTaper, splitFirstRound
  logical*4 inSplitRound
  integer*4 numNew, nOverlap, iseqPass, ipassSave, iAreaSave, maxObjOrig
  real*4 outlieElimMin, outlieCrit, outlieCritAbs, curResMin, tiltMax, minPeakRatio
  real*4 sigma1, sigma2, radius2, radius1, deltaCtf, ranFrac, diameter, sobelSigma
  integer*4 maxDrop, numDrop, ndatFit, numAreaTot, maxInArea, limInArea, interpType
  integer*4 minViewDo, maxViewDo, numViewDo, numBound, iseed, limcxBound, nFarther
  integer*4 ivsOnAlign, ifAlignDone, imageBinned, maxSobelSum, maxAnySum, nFillTaper
  integer*4 nxSobel, nySobel, nxsPad, nysPad, ninSobel, kernelDim, ifReadXfs
  real*4 xOffSobel, yOffSobel, scaleFacSobel, scaleByInterp, targetSobel, edgeSD
  real*4 sdsum, sdsumsq, sdmin, sdmax, sdavg, sdmed, sdmean, edgeSDsd, elongMean, elongMed
  real*4 percentileCritFrac, dblNormMinCrit, wcritToLocalMinRatio, aloneMinAngleDiff
  real*4 aloneDistCrit, aloneWsumCrit, aloneRelaxCrit, aloneMaxAngleDiff, alonePeakCrit
  integer*4 maxWneigh, numWneighWant, minFilled, maxFilled, maxInitCorrPeaks
  character*320 concat
  integer*4 getImodObjsize, niceFrame, surfaceSort, scaledSobel
  logical itemOnList
  real*4 ran
  !
  logical pipinput
  integer*4 numOptArg, numNonOptArg, PipGetLogical
  integer*4 PipGetInteger, PipGetBoolean
  integer*4 PipGetString, PipGetFloat, PipGetTwoIntegers, PipGetTwoFloats
  integer*4 PipGetInOutFile, PipNumberOfEntries, ifpip
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  beadtrack
  !
  integer numOptions
  parameter (numOptions = 57)
  character*(40 * numOptions) options(1)
  options(1) = &
      ':InputSeedModel:FN:@:OutputModel:FN:@:ImageFile:FN:@:PieceListFile:FN:@'// &
      ':ImagesAreBinned:I:@:SurfaceOutputFile:FN:@:SkipViews:LI:@:RotationAngle:F:@'// &
      ':SeparateGroup:LIM:@first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'// &
      'tiltfile:TiltFile:FN:@angles:TiltAngles:FAM:@:TiltDefaultGrouping:I:@'// &
      ':TiltNondefaultGroup:ITM:@:MagDefaultGrouping:I:@:MagNondefaultGroup:ITM:@'// &
      ':RotDefaultGrouping:I:@:RotNondefaultGroup:ITM:@:MinViewsForTiltalign:I:@'// &
      ':CentroidRadius:F:@:BeadDiameter:F:@:MedianForCentroid:B:@:LightBeads:B:@'// &
      ':FillGaps:B:@:MaxGapSize:I:@:MinTiltRangeToFindAxis:F:@'// &
      ':MinTiltRangeToFindAngles:F:@:BoxSizeXandY:IP:@:RoundsOfTracking:I:@'// &
      ':MaxViewsInAlign:I:@:RestrictViewsOnRound:I:@:LocalAreaTracking:B:@'// &
      ':LocalAreaTargetSize:I:@:MinBeadsInArea:I:@:MaxBeadsInArea:I:@'// &
      ':MinOverlapBeads:I:@:TrackObjectsTogether:B:@:MaxBeadsToAverage:I:@'// &
      ':SobelFilterCentering:B:@:KernelSigmaForSobel:F:@:AverageBeadsForSobel:I:@'// &
      ':InterpolationType:I:@:PointsToFitMaxAndMin:IP:@'// &
      ':DensityRescueFractionAndSD:FP:@:DistanceRescueCriterion:F:@'// &
      ':RescueRelaxationDensityAndDistance:FP:@:PostFitRescueResidual:F:@'// &
      ':DensityRelaxationPostFit:F:@:MaxRescueDistance:F:@'// &
      ':ResidualsToAnalyzeMaxAndMin:IP:@:DeletionCriterionMinAndSD:FP:@'// &
      'param:ParameterFile:PF:@help:usage:B:@:BoxOutputFile:FN:@:SnapshotViews:LI:@'// &
      ':SaveAllPointsAreaRound:IP:'
  !
  idebugObj = 49
  maxWavg = 15
  limPtsStretch = 16
  limPtsMag = 6
  limPtsRot = 4
  limPtsShift = 3
  minInView = 4
  maxh = 0
  facMetro = .25
  nCycle = 1000
  eps = 0.00002                               !was .00001 then .00002
  ifpip = 0
  pieceFile = ' '
  rotStart = 0.
  minViewsTiltAli = 4
  ifWhite = 0
  ifFillIn = 0
  maxgap = 5
  rangeDoAxis = 10
  rangeDoTilt = 20
  ifLocalArea = 0
  minInArea = 8
  minBeadOverlap = 3
  localTarget = 1000
  numRounds = 1
  maxSum = 4
  numFit = 7
  minFit = 3
  fracCrit = 0.6
  sdCrit = 1.
  distCrit = 10.
  relaxInt = 0.7
  relaxDist = 0.9
  fitDistCrit = 2.5
  relaxFit = 0.9
  radMaxFit = 2.5
  maxResid = 9
  minResid = 5
  resDiffMin = 0.04
  resDiffCrit = 2
  modeBox = 2
  ipassSave = 0
  iAreaSave = 0
  outlieCrit = 0.01
  outlieCritAbs = 0.002
  outlieElimMin = 2.
  curResMin = 0.5
  deltaCtf = 0.
  sigma1 = 0.00
  sigma2 = 0.05
  radius2 = 0.
  radius1 = 0.
  areaObjStr = 'object'
  limInArea = 1000
  limcxBound = 2500
  imageBinned = 1
  npad = 8
  maxSobelSum = 0
  scaleByInterp = 1.2
  targetSobel = 8
  sobelSigma = 0.5
  elongSigma = 0.85
  interpType = 0
  maxPeaks = 20
  minPeakRatio = 0.2
  msizeXR = 19
  taperFrac = 0.2
  edgeMedian = .false.
  getEdgeSD = .false.
  numWneighWant = 30
  percentileCritFrac = 0.8
  dblNormMinCrit = 0.2
  wcritToLocalMinRatio = 0.075
  splitFirstRound = .true.
  maxInitCorrPeaks = 5             ! # of correlation peaks in initial real-space corr
  aloneWsumCrit = 0.25             ! wsum must be > this frac of wsum for non-isolated
  alonePeakCrit = 0.2              ! Peak must be > this fraction
  aloneDistCrit = 1.8              ! distance must be < this * bead size for non-isolated
  aloneRelaxCrit = 1.0             ! Amount to relax criteria for isolated bead
  aloneMinAngleDiff = 1.4          ! Minimum angle for assessing if bead is alone
  aloneMaxAngleDiff = 5.1          ! Maximum angle for assessing if bead is alone
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'beadtrack', &
      'ERROR: BEADTRACK - ', .true., 3, 1, 1, numOptArg, &
      numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0

  if (pipinput) then
    ifpip = 1
    if (PipGetString('ImageFile', filin) .ne. 0) call errorExit &
        ('NO IMAGE INPUT FILE SPECIFIED', 0)
    ierr = PipGetString('PieceListFile', pieceFile)
  else

    write(*,'(1x,a,$)') 'Image input file: '
    read(5, 101) filin
101 format(a)
    write(*,'(1x,a,$)') 'Piece list file if image is montage,'// &
        ' otherwise Return: '
    read(*,101) pieceFile
  endif
  !
  CALL imopen(1, filin, 'RO')
  CALL irdhdr(1, nxyz, mxyz, MODE, DMIN2, dmax2, DMEAN2)
  !
  limPcList = nz + 10
  allocate(ixPclist(limPcList), iyPclist(limPcList), izPclist(limPcList),  &
      listz(limPcList), prexf(2,3,limPclist), stat = ierr)
  call memoryError(ierr, 'PIECE LIST ARRAYS')

  call read_piece_list2(pieceFile, ixPclist, iyPclist, izPclist, npclist, limPcList)
  !
  ! if no pieces, set up mocklist
  !
  if (npclist == 0) then
    do i = 1, nz
      ixPclist(i) = 0
      iyPclist(i) = 0
      izPclist(i) = i - 1
    enddo
    npclist = nz
  endif
  call fill_listz(izPclist, npclist, listz, nviewAll)
  !
  ! assign maxView the same as in alivar based on actual maximum views and allocate
  maxView = nviewAll + 4
  allocate(izExclude(maxView), ipClose(maxView), izClose(maxView), ivList(maxView), &
      ivSnapList(maxView), missing(0:maxView), prevRes(maxView),  tiltOrig(maxView), &
      gmagOrig(maxView), rotOrig(maxView), dxySave(2, maxView), tiltAll(maxView), &
      ivsepIn(maxView, MAXGRP), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR VIEWS')
  call allocateMapsep(ierr)
  call memoryError(ierr, 'ARRAYS IN MAPSEP')

  call checkList(ixPclist, npclist, 1, nx, minXpiece , nxPieces, nxOverlap)
  nxTotPix = nx + (nxPieces - 1) * (nx - nxOverlap)
  call checkList(iyPclist, npclist, 1, ny, minYpiece , nyPieces, nyOverlap)
  nyTotPix = ny + (nyPieces - 1) * (ny - nyOverlap)
  xcen = nxTotPix / 2.
  ycen = nyTotPix / 2.
  scaleXY = sqrt(xcen**2 + ycen**2)
  !
  if (PipGetInOutFile('InputSeedModel', 1, 'Input seed model file', modelFile) .ne. 0) &
      call errorExit ('NO INPUT SEED MODEL FILE SPECIFIED', 0)
  !
  exist = readSmallMod(modelFile)
  if (.not.exist) call errorExit('READING SEED MODEL FILE', 0)
  if (n_point == 0 .or. max_mod_obj == 0) &
      call errorExit('INPUT SEED MODEL IS EMPTY', 0)
  !
  ! Initial allocations based on model size, and way oversized temp array
  ! for object lists, which is also needed for gaps
  maxAllReal = max_mod_obj + 10
  maxOlist = max(40 * maxAllReal, 100000)
  allocate(seqDist(maxAllReal), xxtmp(maxAllReal), yytmp(maxAllReal), &
      xvtmp(maxAllReal), yvtmp(maxAllReal), &
      iobjSeq(maxAllReal), xyzSave(3, maxAllReal), &
      iobjLisTmp(maxOlist), indgap(maxAllReal), inAnArea(maxAllReal), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR ALL CONTOURS IN MODEL')
  !
  ! convert to image index coordinates and change the origin and delta
  ! for X / Y to reflect this
  !
  call scaleModelToImage(1, 0)
  xorig = 0.
  yorig = 0.
  xdelt = 1.
  ydelt = 1.
  !
  ! Check for multiple points on a view
  do iobj = 1, max_mod_obj
    numInObj = npt_in_obj(iobj)
    if (numInObj > 0) then
      ibase = ibase_obj(iobj)
      ivList = 0
      do ipt = 1, numInObj
        iz = nint(p_coord(3, object(ipt + ibase)) + 1.)
        if (iz > 0 .and. iz <= nviewAll) then
          if (ivList(iz) > 0) then
            call objToCont(iobj, obj_color, ibase, numInObj)
            write(*,'(/,a,i5,a,i5,a,i5,a,i5,a,i4)') &
                'ERROR: BEADTRACK - TWO POINTS (#', ivList(iz), ' AND', ipt, &
                ') ON VIEW', iz, ' IN CONTOUR', numInObj, ' OF OBJECT', ibase
            call exit(1)
          else
            ivList(iz) = ipt
          endif
        endif
      enddo
    endif
  enddo
  !
  if (PipGetInOutFile('OutputModel', 2, 'Output model file', modelFile) &
      .ne. 0) call errorExit('NO OUTPUT MODEL FILE SPECIFIED', 0)
  !
  outFile = ' '
  elongFile = ' '
  xyzFile = ''
  numExclude = 0
  if (pipinput) then
    ierr = PipGetString('BoxOutputFile', outFile)
    ierr = PipGetString('ElongationOutputFile', elongFile)
    ierr = PipGetString('XYZOutputFile', xyzFile)
    getEdgeSD = elongFile .ne. ' '
    if (PipGetString('SkipViews', listString) == 0) call parseList2 &
        (listString, izExclude, numExclude, maxView)
    ierr = PipGetFloat('RotationAngle', rotStart)
    ierr = PipNumberOfEntries('SeparateGroup', ngsep)
    if (ngsep > MAXGRP) call errorExit &
        ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
    do ig = 1, ngsep
      ierr = PipGetString('SeparateGroup', listString)
      call parseList2(listString, ivsepIn(1, ig), nsepInGrpIn(ig), maxView)
    enddo
  else
    !
    write(*,'(1x,a,$)') 'List of views to skip over: '
    call rdlist(5, izExclude, numExclude)
    !
    ! new parameters for tilt alignment
    !
    write(*,'(1x,a,$)') &
        'Initial angle of rotation in projection plane: '
    read(5,*) rotStart
    !
    ! Get list of views to treat separately in automapping
    !
    write(*,'(1x,a,/,a,$)') 'For automapping tilt and mag,' &
        //' enter the number of sets of views to treat separately' &
        //' from the main set of views (otherwise enter 0): '
    read(5,*) ngsep
    if (ngsep > MAXGRP) call errorExit &
        ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
    do ig = 1, ngsep
      write(*,'(1x,a,i3,a,$)') 'List of views in set', ig, &
          '(ranges OK): '
      call rdlist(5, ivsepIn(1, ig), nsepInGrpIn(ig))
    enddo
  endif
  !
  call get_tilt_angles(nviewAll, 3, tiltAll, maxView, ifpip)
  !
  ! DNM 5 / 3 / 02: accommodate changes to tiltalign by setting up the
  ! mapping arrays, adding to automap call
  ! DNM 3 / 25 / 05: mapping arrays are going to be used for real
  ! 4 / 11 / 05: default is to ignore objects for transferfid situations
  !
  nFileViews = nviewAll
  ignoreObjs = nviewAll <= 2
  !
  if (.not.pipinput) print *, &
      'Specify grouping for mapping of tilt variables'
  call inputGroupings(nviewAll, ifpip, 1,  'TiltDefaultGrouping', &
      'TiltNondefaultGroup', nmapTilt, ivSpecStrTilt, &
      ivSpecEndTilt, nmapSpecTilt, nRanSpecTilt, MAXGRP)
  !
  if (.not.pipinput) print *, &
      'Specify grouping for mapping of magnification variables'
  call inputGroupings(nviewAll, ifpip, 1,  'MagDefaultGrouping', &
      'MagNondefaultGroup', nmapMag, ivSpecStrMag, &
      ivSpecEndMag, nmapSpecMag, nRanSpecMag, MAXGRP)

  nmapRot = 1
  nRanSpecRot = 0
  if (pipinput) call inputGroupings(nviewAll, ifpip, 0, &
      'RotDefaultGrouping', 'RotNondefaultGroup', nmapRot, &
      ivSpecStrRot, ivSpecEndRot, nmapSpecRot, nRanSpecRot, MAXGRP)
  !
  nvLocalIn = 0
  localViewPass = 0
  tiltFitMin = 8.   ! Was 15 until cos/sin fitting improved 7/25/12
  nSnapList = 0
  !
  if (pipinput) then
    ierr = PipGetInteger('ImagesAreBinned', imageBinned)
    ierr = PipGetFloat('CentroidRadius', cgRadius)
    j = PipGetFloat('BeadDiameter', diameter)
    if (ierr + j .ne. 1) call errorExit( &
        'YOU MUST ENTER EITHER BeadDiameter OR CentroidRadius '// &
        'BUT NOT BOTH', 0)
    !
    ! compute diameter from cgRadius by the formula used to get cgRadius in
    ! copytomocoms, then divide by binning and get cgRadius back
    if (ierr == 0) diameter = max(2., 2. * cgRadius - 3.)
    diameter = diameter / imageBinned
    cgRadius = 0.5 * (diameter + 3)
    if (PipGetTwoIntegers('BoxSizeXandY', nxBox, nyBox) .ne. 0) &
        call errorExit('YOU MUST ENTER A BOX SIZE', 0)

    if (imageBinned > 1) then
      !
      ! Adjust box size if binned.  The formula here is
      ! size = 2 * max(0, diameter - 3) + 32
      ierr = max(0, int(diameter) - 3) + 16
      nxBox = 2 * max(nxBox / (imageBinned * 2), ierr)
      nyBox = 2 * max(nyBox / (imageBinned * 2), ierr)
      print *,'Box size in X and Y adjusted for binning to:', nxBox, nyBox
    endif
    ierr = PipGetInteger('MinViewsForTiltalign', minViewsTiltAli)
    ierr = PipGetBoolean('LightBeads', ifWhite)
    ierr = PipGetBoolean('FillGaps', ifFillIn)
    ierr = PipGetInteger('MaxGapSize', maxgap)
    ierr = PipGetFloat('MinTiltRangeToFindAxis', rangeDoAxis)
    ierr = PipGetFloat('MinTiltRangeToFindAngles', rangeDoTilt)
    ierr = PipGetInteger('MaxViewsInAlign', nvLocalIn)
    ierr = PipGetInteger('RestrictViewsOnRound', localViewPass)
    ierr = PipGetInteger('LocalAreaTargetSize', localTarget)
    ierr = PipGetInteger('LocalAreaTracking', ifLocalArea)
    ierr = PipGetInteger('MinBeadsInArea', minInArea)
    if (ifLocalArea .ne. 0) &
        ierr = PipGetInteger('MaxBeadsInArea', limInArea)
    ! limInArea = min(limInArea, maxreal)
    ierr = PipGetInteger('MinOverlapBeads', minBeadOverlap)
    ierr = PipGetInteger('RoundsOfTracking', numRounds)
    ierr = PipGetInteger('MaxBeadsToAverage', maxSum)
    ierr = PipGetTwoIntegers('PointsToFitMaxAndMin', numFit, minFit)
    ierr = PipGetTwoFloats('DensityRescueFractionAndSD', fracCrit, sdCrit)
    ierr = PipGetFloat('DistanceRescueCriterion', distCrit)
    ierr = PipGetTwoFloats('RescueRelaxationDensityAndDistance', relaxInt, relaxDist)
    ierr = PipGetFloat('PostFitRescueResidual', fitDistCrit )
    ierr = PipGetFloat('DensityRelaxationPostFit', relaxFit)
    ierr = PipGetFloat('MaxRescueDistance', radMaxFit)
    ierr = PipGetTwoIntegers('ResidualsToAnalyzeMaxAndMin', maxResid, minResid)
    ierr = PipGetTwoFloats('DeletionCriterionMinAndSD', resDiffMin, resDiffCrit)
    ierr = PipGetFloat('IsolatedBeadCriterionFactor', aloneRelaxCrit)
    ierr = PipGetLogical('TrackObjectsTogether', ignoreObjs)
    maxSobelSum = 50
    ierr = PipGetInteger('AverageBeadsForSobel', maxSobelSum)
    i = 0
    ierr = PipGetBoolean('SobelFilterCentering', i)
    if (i == 0) maxSobelSum = 0
    ierr = PipGetFloat('KernelSigma', sobelSigma)
    ierr = PipGetLogical('MedianForCentroid', edgeMedian)
    i = 0
    ierr = PipGetBoolean('UnsplitFirstRound', i)
    splitFirstRound = i == 0
    if (sobelSigma > 1.49) interpType = 1
    ierr = PipGetInteger('InterpolationType', interpType)

    ifReadXfs = 1 - PipGetString('PrealignTransformFile', prexfFile)
    if (ifReadXfs .ne. 0) then
      call dopen(3, prexfFile, 'ro', 'f')
      call xfrdall2(3, prexf, iv, limPcList, ierr)
      if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
      if (iv .lt. nz) call exitError('NOT ENOUGH TRANSFORMS IN PREALIGN TRANSFORM FILE')
      prexf(1:2,3,1:nz) = prexf(1:2,3,1:nz) / imageBinned
      close(3)
      nFillTaper = max(4, nint(0.1 * (nxBox * nyBox) / 2.))
    endif

    if (PipGetString('SnapshotViews', listString) == 0) call parseList2 &
        (listString, ivSnapList, nSnapList, maxView)
    ierr = PipGetTwoIntegers('SaveAllPointsAreaRound', iAreaSave, ipassSave)
  else
    write(*,'(1x,a,$)') 'Minimum # of views required for tilt'// &
        ' aligning: '
    read(5,*) minViewsTiltAli
    !
    write(*,'(1x,a,$)') 'Radius for centroid calculation, 0 for '// &
        'dark or 1 for light beads: '
    read(5,*) cgRadius, ifWhite
    diameter = max(2., 2. * cgRadius - 3.)
    !
    write(*,'(1x,a,$)') '1 to fill in gaps, 0 not to: '
    read(5,*) ifFillIn
    !
    write(*,'(1x,a,$)') 'Maximum gap to create: '
    read(5,*) maxgap
    !
    write(*,'(1x,a,/,a,$)') 'Minimum ranges of tilt angles required' &
        //' for finding tilt axis', ' and for finding tilt angles: '
    read(5,*) rangeDoAxis, rangeDoTilt
    !
    write(*,'(1x,a,i4,a,$)') 'X and Y dimensions of box: '
    read(5,*) nxBox, nyBox
    !
    write(*,'(1x,a,$)') &
        'Maximum number of beads to average for correlation: '
    read(5,*) maxSum
    !
    write(*,'(1x,a,$)') '# of points to fit for extrapolation' &
        //', minimum # required: '
    read(5,*) numFit, minFit
    !
    write(*,'(1x,a,$)') 'Fraction of mean and # of SDs for'// &
        ' density criteron for rescue: '
    read(5,*) fracCrit, sdCrit
    write(*,'(1x,a,$)') &
        'Distance criterion for rescue (- for full report): '
    read(5,*) distCrit
    !
    write(*,'(1x,a,$)') 'Fractions to relax criterion during '// &
        'density and distance rescue: '
    read(5,*) relaxInt, relaxDist
    !
    write(*,'(1x,a,$)') &
        'Criterion distance for rescue after fitting: '
    read(5,*) fitDistCrit
    !
    write(*,'(1x,a,$)') 'Relaxation of density criterion, maximum '// &
        'rescue distance: '
    read(5,*) relaxFit, radMaxFit
    write(*,'(1x,a,$)') 'Max and min # of residual changes to use '// &
        'to get mean and SD: '
    read(5,*) maxResid, minResid
    write(*,'(1x,a,$)') 'Minimum residual change & criterion # of'// &
        ' SD from mean for deletion of pt: '
    read(5,*) resDiffMin, resDiffCrit
    !
  endif
  call PipDone()
  !
  iPolarity = -1
  if (ifWhite .ne. 0) iPolarity = 1
  !
  ! Set up tapering.  It is inside, so make box a little bigger but not much
  ! Some tracking deterioriated with it larger.
  nxTaper = max(8, nint(taperFrac * nxBox))
  nyTaper = max(8, nint(taperFrac * nyBox))
  nxBox = nxBox + nxTaper / 2
  nyBox = nyBox + nyTaper / 2
  nxpad = niceFrame(nxBox + 2 * npad, 2, 19)
  nypad = niceFrame(nyBox + 2 * npad, 2, 19)
  npixBox = nxBox * nyBox
  nxpDim = nxpad + 2
  maxarr = nypad * nxpDim
  if (sigma1 .ne. 0 .or. radius2 .ne. 0) call setCtfwsr &
      (sigma1, sigma2, radius1, radius2, ctf, nxpad, nypad, deltaCtf)
  !
  ifTrace = 0
  if (distCrit < 0.) then
    ifTrace = 1
    distCrit = -distCrit
  endif
  !
  tiltMax = 0.
  do iv = 1, nviewAll
    rotOrig(iv) = rotStart
    tiltOrig(iv) = tiltAll(iv)
    gmagOrig(iv) = 1.
    dxySave(1, iv) = 0.
    dxySave(2, iv) = 0.
    tiltMax = max(tiltMax, abs(tiltAll(iv)))
  enddo
  mapDumDmag = 0
  mapDmagStart = 1
  !
  ! figure out where to start: section with most points at minimum tilt
  ! ivList is the number of points present on each view
  ! ivGap has a list of views with gaps for each object (if not filling in)
  ! indgap is index to location in ivGap for each object
  ! minEndZ is the minimum ending Z of all the objects
  ! use iobjLisTmp for ivGap
  !
  minEndZ = maxView
  indFree = 1
  do i = 1, maxView
    ivList(i) = 0
  enddo
  do iobj = 1, max_mod_obj
    numInObj = npt_in_obj(iobj)
    indgap(iobj) = indFree
    if (numInObj > 0) then
      ibase = ibase_obj(iobj)
      !
      ! first order them by z
      !
      do ipt = 1, numInObj - 1
        do jpt = ipt + 1, numInObj
          iz = nint(p_coord(3, object(ipt + ibase))) + 1
          jz = nint(p_coord(3, object(jpt + ibase))) + 1
          if (iz > jz) then
            itmp = object(ipt + ibase)
            object(ipt + ibase) = object(jpt + ibase)
            object(jpt + ibase) = itmp
          endif
        enddo
      enddo
      do ipt = 1, numInObj
        iz = nint(p_coord(3, object(ipt + ibase))) + 1
        ivList(iz) = ivList(iz) + 1
      enddo
      minEndZ = min(minEndZ, iz)
      if (ifFillIn == 0) then
        !
        ! find gaps and add to list
        !
        do ipt = 1, numInObj - 1
          iz = nint(p_coord(3, object(ipt + ibase))) + 1
          iznext = nint(p_coord(3, object(ipt + 1 + ibase))) + 1
          if (iznext > iz + 1) then
            do iv = iz + 1, iznext - 1
              iobjLisTmp(indFree) = iv
              indFree = indFree + 1
              if (indFree > maxOlist) call errorExit( &
                  'TOO MANY GAPS IN EXISTING MODEL FOR ARRAYS', 0)
            enddo
          endif
        enddo
      endif
    endif
  enddo
  indgap(max_mod_obj + 1) = indFree
  !
  ! allocate ivGap and copy to it
  limGaps = indFree
  allocate(ivGap(limGaps), stat = ierr)
  call memoryError(ierr, 'ARRAY FOR GAPS')
  if (indFree > 1) ivGap(1:indFree-1) = iobjLisTmp(indFree-1)
  !
  ! now find section with most points
  !
  maxnpt = 0
  tiltMin = 200.
  do i = 1, nviewAll
    if (ivList(i) > maxnpt .or. &
        (ivList(i) == maxnpt .and. abs(tiltAll(i)) < tiltMin)) then
      maxnpt = ivList(i)
      tiltMin = abs(tiltAll(i))
      minTiltInd = i
    endif
  enddo

  ! Get range of sections that have more than half of maximum
  minFilled = minTiltInd
  maxFilled = minTiltInd
  do i = 1, nviewAll
    if (ivList(i) >= maxnpt / 2) then
      minFilled = min(minFilled, i)
      maxFilled = max(maxFilled, i)
    endif
  enddo
  !
  ! Get minimim and maximum views to do: including minTiltInd even if
  ! it is excluded
  minViewDo = 0
  iv = 1
  do while(iv <= nviewAll .and. minViewDo == 0)
    minViewDo = iv
    if (iv .ne. minTiltInd .and. itemOnList(iv, izExclude, numExclude)) &
        minViewDo = 0
    iv = iv + 1
  enddo
  maxViewDo = 0
  iv = nviewAll
  do while(iv >= 1 .and. maxViewDo == 0)
    maxViewDo = iv
    if (iv .ne. minTiltInd .and. itemOnList(iv, izExclude, numExclude)) &
        maxViewDo = 0
    iv = iv - 1
  enddo
  numViewDo = maxViewDo + 1 - minViewDo
  ! print *,minTiltInd, minViewDo, maxViewDo

  ! Determine if first round will be split
  if (splitFirstRound) then
    splitFirstRound = maxViewDo - minTiltInd > 5 .and. minTiltInd - minViewDo > 5 .and. &
        abs(tiltAll(minViewDo)) > 30. .and. abs(tiltAll(maxViewDo)) > 30. .and.  &
        maxFilled - minTiltInd < (maxViewDo - minTiltInd) / 3 .and. &
        minTiltInd - minFilled < (minTiltInd - minViewDo) / 3
    if (splitFirstRound) then
      print *,'First round of tracking will be split in two'
    else
      print *,'First round of tracking will NOT be split in two; it is not sensible'
    endif
  endif
  !
  ! figure out an order for the points: from the center outwards
  ! i.e., first find position from center at minimum tilt, save that in
  ! xyzSave, and store the square of distance in seqDist
  ! iobjSeq is just a list of objects to do in original order
  !
  numObjDo = 0
  do iobj = 1, max_mod_obj
    numInObj = npt_in_obj(iobj)
    if (numInObj > 0) then
      numObjDo = numObjDo + 1
      iobjSeq(numObjDo) = iobj
      tiltMin = 200.
      ibase = ibase_obj(iobj)
      do ipt = 1, numInObj
        iz = nint(p_coord(3, object(ipt + ibase))) + 1
        if (abs(tiltAll(iz)) < tiltMin) then
          tiltMin = abs(tiltAll(iz))
          xyzSave(1, iobj) = p_coord(1, object(ipt + ibase)) - xcen
          xyzSave(2, iobj) = p_coord(2, object(ipt + ibase)) - ycen
          seqDist(numObjDo) = xyzSave(1, iobj)**2 + xyzSave(2, iobj)**2
          xxtmp(numObjDo) = xyzSave(1, iobj)
          yytmp(numObjDo) = xyzSave(2, iobj)
        endif
      enddo
      xyzSave(3, iobj) = 0.
    endif
  enddo
  if (ifLocalArea .ne. 0 .and. minBeadOverlap > 0) then
    !
    ! determine an average density from the area of the convex bound
    ! First get a random subset not to exceed the limiting number
    numBound = numObjDo
    if (numObjDo > limcxBound) then
      numBound = 0
      iseed = 1234567
      ranFrac = float(limcxBound) / numObjDo
      do i = 1, numObjDo
        if (ran(iseed) < ranFrac) then
          numBound = numBound + 1
          xxtmp(numBound) = xxtmp(i)
          yytmp(numBound) = yytmp(i)
        endif
      enddo
    endif
    !
    call convexBound(xxtmp, yytmp, numBound, 0., 2. * cgRadius, xvtmp, yvtmp, &
        numVert, cvbxcen, cvbycen, maxAllReal)
    area = 0.
    do i = 1, numVert
      j = mod(i, numVert) + 1
      area = area + 0.5 * (yvtmp(j) + yvtmp(i)) * (xvtmp(j) - xvtmp(i))
    enddo
    density = numObjDo / abs(area)
    ! print *,area, density, numObjDo
  endif
  !
  ! set up for one area in X and Y, then compute number of areas and
  ! overlaps if locals
  !
  numAreaX = 1
  numAreaY = 1
  nxOverlap = 0
  nyOverlap = 0
  done = .false.
  do while (.not.done)
    done = .true.
    if (ifLocalArea .ne. 0) then
      areaObjStr = 'area'
      nOverlap = 0
      !
      ! set target overlap so that 1.5 overlap areas at this density
      ! will give minimum number of overlap beads
      ! 6 / 21 / 08: but constrain it to be smaller than the target itself
      if (minBeadOverlap > 0) then
        nOverlap = min(minBeadOverlap / (density * localTarget), &
            0.8 * localTarget)
        ! print *,area, density, nOverlap
      endif
      !
      ! get number of areas, round up so areas will start below target
      ! Then compute or set overlaps so local sizes can be computed
      !
      numAreaX = (nxTotPix + localTarget - 2 * nOverlap - 2) / &
          (localTarget - nOverlap)
      numAreaY = (nyTotPix + localTarget - 2 * nOverlap - 2) / &
          (localTarget - nOverlap)
      if (numAreaX == 1) then
        if (numAreaY > 1) nyOverlap = nOverlap
      else if (numAreaY == 1) then
        nxOverlap = nOverlap
      else

        nxLocal = max(nxTotPix / numAreaX, nyTotPix / numAreaY) + 1
        do while ((nxLocal * numAreaX - nxTotPix) / (numAreaX - 1) + &
            (nxLocal * numAreaY - nyTotPix) / (numAreaY - 1) < &
            2 * nOverlap)
          nxLocal = nxLocal + 1
        enddo
        nyLocal = nxLocal
        nxOverlap = (nxLocal * numAreaX - nxTotPix) / (numAreaX - 1)
        nyOverlap = (nyLocal * numAreaY - nyTotPix) / (numAreaY - 1)
      endif
    endif
    !
    ! Get area sizes regardless; compute area distances from center
    !
    nxLocal = (nxTotPix + nxOverlap * (numAreaX - 1)) / numAreaX + 1
    nyLocal = (nyTotPix + nyOverlap * (numAreaY - 1)) / numAreaY + 1
    numAreaTot = numAreaX * numAreaY
    if (ifLocalArea == 0 .and. getImodObjsize() > 0 .and. .not. ignoreObjs) &
        numAreaTot = getImodObjsize()
    maxArea = numAreaTot + 10;
    !
    ! Get arrays for areas
    ix = 2 * numRounds * maxArea
    if (splitFirstRound) ix = ix + 2 * maxArea
    deallocate(xxtmp, yytmp)
    allocate(iareaSeq(maxArea), ninObjList(maxArea), indObjList(maxArea), &
        areaDist(maxArea), ivSeqStr(ix), ivSeqEnd(ix), listSeq(ix), xxtmp(2 * maxArea), &
        yytmp(3 * maxArea), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR AREA DATA')

    do i = 1, numAreaTot
      if (ifLocalArea == 0 .and. getImodObjsize() > 0 .and. .not. ignoreObjs) then
        !
        ! If there are no local areas but there are multiple objects, set up
        ! one area per object.  No point getting distance, they are independent
        iareaSeq(i) = i
        areaDist(i) = 0.
      else
        ix = mod(i - 1, numAreaX)
        iy = (i - 1) / numAreaX
        iareaSeq(i) = i
        areaDist(i) = (ix * (nxLocal - nxOverlap) + nxLocal / 2 - xcen)**2 + &
            (iy * (nyLocal - nyOverlap) + nyLocal / 2 - ycen)**2
      endif
    enddo
    !
    ! order areas by distance: iareaSeq has sequence of area numbers
    do i = 1, numAreaTot - 1
      do j = i + 1, numAreaTot
        if (areaDist(iareaSeq(i)) > areaDist(iareaSeq(j))) then
          itmp = iareaSeq(i)
          iareaSeq(i) = iareaSeq(j)
          iareaSeq(j) = itmp
        endif
      enddo
    enddo
    !
    ! go through each area finding points within it
    ! iobjLists is the list of objects to do for each area
    ! indObjList is the starting index in that list for each area
    ! ninObjList is the number of objects in the list for each area
    !
    nobjLists = 0
    indFree = 1
    maxInArea = 0
    do j = 1, max_mod_obj
      inAnArea(j) = .false.
    enddo
    do k = 1, numAreaTot
      if (ifLocalArea .ne. 0 .or. numAreaTot == 1) then
        !
        ! get starting coordinates and set up to loop until conditions met
        !
        ix = mod(iareaSeq(k) - 1, numAreaX)
        iy = (iareaSeq(k) - 1) / numAreaX
        xst = ix * (nxLocal - nxOverlap) - xcen
        xnd = xst + nxLocal
        yst = iy * (nyLocal - nyOverlap) - ycen
        ynd = yst + nyLocal
        indStart = indFree
        keepGoing = .true.
        do while(keepGoing)
          indFree = indStart
          numNew = 0
          !
          ! Look for objects in the area, count up the new ones
          !
          do j = 1, numObjDo
            iobj = iobjSeq(j)
            if (xyzSave(1, iobj) >= xst .and. xyzSave(1, iobj) <= xnd .and. &
                xyzSave(2, iobj) >= yst .and. xyzSave(2, iobj) <= ynd) then
              iobjLisTmp(indFree) = iobj
              indFree = indFree + 1
              if (indFree > maxOlist) call errorExit( &
                  'WAY TOO MANY LOCAL AREAS; EACH POINT IS IN MANY AREAS', 0)
              if (.not. inAnArea(iobj)) numNew = numNew + 1
            endif
          enddo
          !
          ! make area bigger if there are any new points at all in it and
          ! it does not already have all the points, and
          ! either the total in it is too low or this is an area after the
          ! first and the old ones are too low for overlap
          !
          keepGoing = numNew > 0 .and. indFree - indStart < numObjDo &
              .and. (indFree - indStart < minInArea .or. (k > 1 .and. &
              indFree - indStart - numNew < minBeadOverlap))
          if (keepGoing) then
            j = max(1, localTarget / 200)
            xst = xst - j
            xnd = xnd + j
            yst = yst - j
            ynd = ynd + j
          endif
        enddo
      else
        !
        ! areas from objects: make list of objects in it
        !
        numNew = 0
        indStart = indFree
        do j = 1, numObjDo
          iobj = iobjSeq(j)
          call objToCont(iobj, obj_color, ix, iy)
          if (ix == k) then
            numNew = numNew + 1
            iobjLisTmp(indFree) = iobj
            indFree = indFree + 1
          endif
        enddo
      endif
      !
      if (numNew > 0) then
        !
        ! if the area has new points, order the list by distance from
        ! center
        ! (Should that be center of area?  It is center of whole field)
        !
        nobjLists = nobjLists + 1
        indObjList(nobjLists) = indStart
        ninObjList(nobjLists) = indFree - indStart
        do i = indStart, indFree-1
          inAnArea(iobjLisTmp(i)) = .true.
        enddo
        maxInArea = max(maxInArea, ninObjList(nobjLists))
        do i = indStart, indFree-2
          do j = i + 1, indFree-1
            if (seqDist(iobjLisTmp(i)) > seqDist(iobjLisTmp(j))) then
              itmp = iobjLisTmp(i)
              iobjLisTmp(i) = iobjLisTmp(j)
              iobjLisTmp(j) = itmp
            endif
          enddo
        enddo
        xxtmp(2 * nobjLists - 1) = xst + xcen
        xxtmp(2 * nobjLists) = xnd + xcen
        yytmp(3 * nobjLists - 1) = yst + ycen
        yytmp(3 * nobjLists) = ynd + ycen
        yytmp(3 * nobjLists - 2) = numNew
      else
        indFree = indStart
      endif
    enddo
    if (ifLocalArea .ne. 0 .and. maxInArea > limInArea .and. localTarget > 100) then
      localTarget = 0.98 * localTarget
      done = .false.
    endif
  enddo

  if (ifLocalArea .ne. 0) then
    if (maxInArea > limInArea) call errorExit( &
        'THE NUMBER OF POINTS IN SOME LOCAL AREAS IS ABOVE THE LIMIT', 0)
    print *, 'Local area number, size, overlap - X:', numAreaX, nxLocal, &
        nxOverlap, ',  Y:', numAreaY, nyLocal, nyOverlap
    do i = 1, nobjLists
      write(*,119) i, nint(xxtmp(2 * i - 1)), nint(xxtmp(2 * i)), &
          nint(yytmp(3 * i - 1)), &
          nint(yytmp(3 * i)), ninObjList(i), nint(yytmp(3 * i - 2))
119   format('Area',i4,', X:',i6,' to',i6,', Y:',i6,' to',i6,',', &
          i5,' points,',i5,' new')
    enddo
    ! elseif (maxInArea > maxreal) then
    ! call errorExit( 'TOO MANY POINTS FOR ARRAYS - TRY LOCAL TRACKING', 0)
  endif
  !
  ! Allocate resMean array based on maximum object # and maximum view sequence #
  ! which is # of views for multiple areas, but times # of rounds if only one area
  limResid = numViewDo
  if (nobjLists == 1) limResid = limResid * numRounds
  allocate(resMean(maxAllReal, limResid), stat = ierr)
  call memoryError(ierr, 'ARRAY FOR MEAN RESIDUALS')
  !
  ! Maximum Number of points finally known for tiltalign solutions: allocate
  call allocateAlivar(maxInArea * nviewAll, nviewAll, maxInArea, ierr)
  call memoryError(ierr, 'ARRAYS IN ALIVAR')
  call allocateFunctVars(ierr)
  call memoryError(ierr, 'ARRAYS FOR FUNCT')
  comp = 1.
  mapComp = 0
  skew = 0.
  mapSkew = 0
  dmag = 0.
  mapDmag = 0
  alf = 0.
  mapAlf = 0
  !
  maxAnySum = max(maxSobelSum, maxSum)

  ! Get final arrays for the object lists and free temporary stuff
  maxOlist = indFree
  allocate(iobjLists(maxOlist), inCore(maxAnySum, maxInArea), numWneighbors(maxArea),  &
      neighborsForWfits(max(numWneighWant, maxInArea), maxArea), iobjMap(max_mod_obj), &
      stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR OBJECT LISTS')
  if (elongFile .ne. ' ' .or. xyzFile .ne. ' ') then
    allocate(residLists(maxOlist), xyzAllArea(3, maxOlist), numResSaved(maxAllReal),  &
        stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR RESIDUALS/XYZ DATA')
    residLists = -1.
    xyzAllArea = 0.
    numResSaved = 0
  endif
  iobjLists(1:indFree-1) = iobjLisTmp(1:indFree-1)

  ! Make additional lists of neighbors for Wsum fitting and analysis
  maxWneigh = 0
  do iseq = 1, nobjLists
    numObjDo = ninObjList(iseq)
    numWneighbors(iseq) = numObjDo

    ! Copy over this area's objects and get their centroid
    xpos = 0.
    ypos = 0.
    do i = 1, numObjDo
      iobj = iobjLists(indObjList(iseq) + i - 1)
      xpos = xpos + xyzSave(1, iobj) / numObjDo
      ypos = ypos + xyzSave(2, iobj) / numObjDo
      neighborsForWfits(i, iseq) = iobj
    enddo
    maxWneigh = max(maxWneigh, numObjDo)
    if (numObjDo >= numWneighWant) &
        cycle

    ! Make a list of ones outside this area and their distance to the centroid
    numObjDo = 0
    do iobj = 1, max_mod_obj
      if (npt_in_obj(iobj) > 0 .and.  &
          .not. itemOnList(iobj, neighborsForWfits(1, iseq), ninObjList(iseq))) then
        numObjDo = numObjDo + 1
        iobjLisTmp(numObjDo) = numObjDo
        iobjMap(numObjDo) = iobj
        seqDist(numObjDo) = (xyzSave(1, iobj) - xpos)**2 + (xyzSave(2, iobj) - ypos)**2
      endif
    enddo
    if (numObjDo == 0) &
        cycle

    ! sort and add to neightbor list
    call rsSortIndexedFloats(seqDist, iobjLisTmp, numObjDo)
    do i = 1, min(numWneighWant - numWneighbors(iseq), numObjDo)
      numWneighbors(iseq) = numWneighbors(iseq) + 1
      neighborsForWfits(numWneighbors(iseq), iseq) = iobjMap(iobjLisTmp(i))
    enddo
    maxWneigh = max(maxWneigh, numWneighbors(iseq))
  enddo

  ! Finally done with these temp arrays
  deallocate(iobjLisTmp, xxtmp, yytmp, xvtmp, yvtmp, seqDist, iobjMap)

  !
  ! Allocate boxes and other image arrays
  allocate(boxes(npixBox, maxAnySum, maxInArea), corrSum(npixBox, maxInArea), &
      curSum(npixBox), boxTmp(maxarr), &
      array(maxarr), BRRAY(maxarr), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR BOXES OF IMAGE DATA')
  !
  ! Allocate arrays for all real points in current solution
  i = maxInArea + 10
  if (getEdgeSD) then
    allocate(edgeSDsave(minViewDo:maxViewDo, maxAllReal),  &
        elongSave(minViewDo:maxViewDo, maxAllReal), elongSmooth(nxBox, nyBox), &
        elongMask(nxBox, nyBox), ixElong(nxBox * nyBox), iyElong(nxBox * nyBox),  &
        stat=ierr)
    call memoryError(ierr, 'ARRAY FOR EDGE SDS')
    edgeSDsave = -1.
    call scaledGaussianKernel(elongKernel, kernDimElong, 7, max(elongSigma, sobelSigma))
  endif
  !
  allocate(xseek(i), yseek(i), wsumCrit(i), xr(msizeXR, i), ifFound(i), &
      ipNearest(i), ipNearSave(i), iobjDel(i), idrop(i), iobjAli(i), numInSobelSum(i), &
      wsumSave(maxView, maxAllReal), isIsolated(maxView, maxAllReal),  &
      inCorrSum(maxAnySum, i), inSobelSum(maxAnySum, i), sobelXpeaks(maxPeaks), &
      sobelYpeaks(maxPeaks), sobelPeaks(maxPeaks), sobelWsums(maxPeaks), &
      sobelEdgeSD(maxPeaks), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR POINTS IN AREA')
  xr(3, 1:i) = 0.
  sobelXpeaks(:) = 0.
  sobelYpeaks(:) = 0.
  wsumSave(:,:) = -1.
  isIsolated(:,:) = -1
  !
  ! Get size and offset of sobel filtered
  if (maxSobelSum > 0) then
    scaleFacSobel = diameter / targetSobel
    ! print *,scaleFacSobel, diameter, targetSobel, scaleByInterp
    ierr = scaledSobel(boxes, nxBox, nyBox, scaleFacSobel, scaleByInterp, &
        interpType, -1., boxes, nxSobel, nySobel, xOffSobel, yOffSobel)
    nxsPad = niceFrame(nxSobel + 2 * npad, 2, 19)
    nysPad = niceFrame(nySobel + 2 * npad, 2, 19)
    maxarr = (nxsPad + 2) * nysPad
    i = nxSobel * nySobel
    ix = 0
    allocate(sarray(maxarr), sbrray(maxarr), refSobel(i),  boxSobel(i), &
        sobelSum(npixBox, maxInArea),  stat = ierr)
    if (sobelSigma > 0) then
      allocate(tmpSobel(npixBox), stat = ix)
      call scaledGaussianKernel(matKernel, kernelDim, 7, sobelSigma)
    endif
    call memoryError(ierr + ix, 'ARRAYS FOR SOBEL FILTERING')

  endif

  !
  ! set up sequencing for object lists and views - odd passes from
  ! middle outward, even passes from ends inward
  !
  numObjTot = numObjDo
  numSeqs = 0
  lastSeq = 0
  do ipass = 1, numRounds
    if (mod(ipass, 2) == 1) then
      if (ipass == 1 .and. splitFirstRound) then
        ix = (minViewDo + minTiltInd) / 2
        iy = (maxViewDo + minTiltInd) / 2
        do i = 1, nobjLists
          call addSequence(i, minTiltInd - 1, ix)
          call addSequence(i, minTiltInd, iy)
        enddo
        do i = 1, nobjLists
          call addSequence(i, ix - 1, minViewDo)
          call addSequence(i, iy + 1, maxViewDo)
        enddo
      else
        do i = 1, nobjLists
          call addSequence(i, minTiltInd - 1, minViewDo)
          call addSequence(i, minTiltInd, maxViewDo)
        enddo
      endif
    else
      do i = 1, nobjLists
        call addSequence(i, minViewDo, minTiltInd - 1)
        call addSequence(i, maxViewDo, minTiltInd)
      enddo
    endif
  enddo
  !
  ! get list of inside and edge pixels for centroid
  !
  limInside = 3.5 * cgRadius**2 + 20.
  limEdge = 3.5 * ((cgRadius + 1.5)**2 - cgRadius**2) + 20.
  allocate(idxin(limInside), idyin(limInside), idxEdge(limEdge), idyEdge(limEdge), &
      edgePixels(limEdge), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR COMPUTING CENTROID')

  numInside = 0
  numEdge = 0
  limcg = cgRadius + 4
  do iy = -limcg, limcg
    do ix = -limcg, limcg
      radPix = sqrt((ix - 0.5)**2 + (iy - 0.5)**2)
      if (radPix <= cgRadius) then
        numInside = numInside + 1
        idxin(numInside) = ix
        idyin(numInside) = iy
      elseif (radPix <= cgRadius + 1.5) then
        numEdge = numEdge + 1
        idxEdge(numEdge) = ix
        idyEdge(numEdge) = iy
      endif
      if (numInside > limInside .or. numEdge > limEdge) call errorExit( &
          'PROGRAMMER ERROR COMPUTING SIZE OF CENTROID ARRAYS', 0)
    enddo
  enddo
  !
  nzout = 0
  if (outFile .ne. ' ') then
    CALL imopen(2, concat(outFile, '.box'), 'NEW')
    CALL imopen(3, concat(outFile, '.ref'), 'NEW')
    CALL imopen(4, concat(outFile, '.cor'), 'NEW')
    do i = 2, 4
      call itrhdr(i, 1)
      call ialmod(i, modeBox)
      call setsiz_sam_cel(i, nxBox, nyBox, 1)
    enddo
    call setsiz_sam_cel(4, nxpad, nypad, 1)
    !
    call time(tim)
    call b3dDate(date)
    write(titlech, 301) date, tim
301 FORMAT('Boxes',32x,a9,2x,a8)
    read(titlech, '(20a4)') (title(kti), kti = 1, 20)
    boxSum = 0.
    boxMin = 1.e20
    boxMax = -1.e20
    refSum = 0.
    refMin = 1.e20
    refMax = -1.e20
    corroSum = 0.
    corrMin = 1.e20
    corrMax = -1.e20
    CALL dopen(2, concat(outFile, '.brpl'), 'NEW', 'F')
    CALL dopen(3, concat(outFile, '.cpl'), 'NEW', 'F')
  endif
  !
  ! Set up formats
  if (numObjTot < 1000) then
    addfmt = '(i4,'' pts added on pass 2, conts:'',(11i4))'
    delfmt1 = '(i4,'' pts deleted for pass 2, conts:'' ,(11i4))'
    delfmt2 = '(i4,'' pts deleted, big mean residual, conts:'',(9i4))'
    objfmt = '(i3,19i4)'
  else
    addfmt = '(i4,'' pts added on pass 2, conts:'',(9i5))'
    delfmt1 = '(i4,'' pts deleted for pass 2, conts:'' ,(8i5))'
    delfmt2 = '(i4,'' pts deleted, big mean residual, conts:'',(7i5))'
    objfmt = '(i4,15i5)'
  endif
  !
  ! Start looping on the sequences of views
  !
  lastList = -1
  maxObjOrig = max_mod_obj
  do iseq = 1, numSeqs
    numAdded = 1
    nviewLocal = 0
    ifAlignDone = 0
    iseqPass = ((iseq + 1) / 2 - 1) / nobjLists + 1
    inSplitRound = splitFirstRound .and. iseqPass == 1
    if (splitFirstRound) iseqPass = max(1, iseqPass - 1)
    if (iseqPass >= localViewPass) nviewLocal = nvLocalIn
    if (saveAllPoints .and. iAreaSave < 0 .and. listSeq(iseq) .ne. lastSeq)  &
        exit
    saveAllPoints = abs(iAreaSave) == listSeq(iseq) .and. ipassSave == iseqPass
    
    ! Initial mean residuals for new set of points or beginning of a full round when
    ! there is just one set; doing more than 2 rounds will then work the same as 
    ! restarting from fid as seed
    if (listSeq(iseq) .ne. lastSeq .or. (nobjLists == 1 .and. iseqPass > 1 .and.  &
        mod(iseqPass, 2) == 1) .and. mod(iseq, 2) == 1) then
      ivSeq = 1
      resMean(:,:) = -1.
    endif

    if (listSeq(iseq) .ne. lastSeq) then
      !
      ! initialize if doing a new set of points
      !
      initXyzDone = 0
      lastSeq = listSeq(iseq)
      numObjDo = ninObjList(lastSeq)
      do i = 1, numObjDo
        iobjSeq(i) = iobjLists(i + indObjList(lastSeq) - 1)
      enddo
      !
      ! Initialize arrays for boxes and residuals
      inCore(1:maxAnySum, 1:numObjDo) = -1
      numInSobelSum(1:numObjDo) = 0
      inCorrSum(1:maxAnySum, 1:numObjDo) = .false.
      inSobelSum(1:maxAnySum, 1:numObjDo) = .false.
      corrSum(1:npixBox, 1:numObjDo) = 0.
      if (maxSobelSum > 0) sobelSum(1:npixBox, 1:numObjDo) = 0.
      write(*,123) areaObjStr, listSeq(iseq), iseqPass, numObjDo
123   format('Starting ',a,i4,', round',i3,',',i4,' contours')
      if (nobjLists > 1) write(*,objfmt) (iobjSeq(i), i = 1, numObjDo)
    endif
    if (saveAllPoints .and. mod(iseq, 2) == 1) then
      do j = 1, 10
        iobj = getImodObjsize() + j
        call putImodFlag(iobj, 1)
        call putSymType(iobj, 0)
        call putSymSize(iobj, 5)
        call putImodObjName(iobj, objNames(mod(j - 1, 5) + 1))
        call putObjColor(iobj, int(255. * colors(1, j)), &
            int(255. * colors(2, j)), int(255. * colors(3, j)))
        do i = 1, numObjDo
          iobj = iobjSeq(i) + j * maxObjOrig
          ibase_obj(iobj) = ibase_free
          npt_in_obj(iobj) = 0
          obj_color(1, iobj) = 1
          obj_color(2, iobj) = 256 - getImodObjsize() - j
          max_mod_obj = max(max_mod_obj, iobj)
          ndx_order(iobj) = iobj
          obj_order(iobj) = iobj
        enddo
      enddo
    endif
    !
    ! Set direction and make list of views to do
    !
    idir = sign(1, ivSeqEnd(iseq) - ivSeqStr(iseq))
    nvList = 0
    do iview = ivSeqStr(iseq), ivSeqEnd(iseq), idir
      ifExclude = 0
      do iexcl = 1, numExclude
        if (iview == izExclude(iexcl)) ifExclude = 1
      enddo
      if (ifExclude == 0) then
        nvList = nvList + 1
        ivList(nvList) = iview
      endif
    enddo
    !
    ! loop on the views
    !
    do ivl = 1, nvList
      iview = ivList(ivl)
      iznext = iview - 1
      call countAndPreparePointsToDo()
      ipass = 1
      do while(ipass <= 2 .and. numToDo .ne. 0)
        !
        ! now try to do tiltalign if possible
        if (numAdded .ne. 0) then
          call tiltAli(ifDidAlign, ifAlignDone, resMean(1, ivSeq), iview)
          if (ifDidAlign .ne. 0) ivsOnAlign = ivSeq
        endif
        call getProjectedPositionsSetupFits()
        !
        ! Loop through points, refining projections before search
        if (ipass == 1) numPioneer = numData
        numAdded = 0
        call getWsumCriteria()
        call findAllBeadsOnView()
        !
        ! get a final fit and a new tiltalign, then find a maximum
        ! error for each pt
        if (ipass == 2) write(*,addfmt) numAdded, (iobjDel(i), i = 1, numAdded)
        call redoFitsEvaluateResiduals()
        ipass = ipass + 1
        call flush(6)
      enddo
      ivSeq = ivSeq + 1
      call flush(6)
    enddo
    !
    ! Report total missing at end of pass
    !
    if (mod(iseq, 2) == 0 .and. .not. inSplitRound) then
      missTot = 0
      do i = 1, numObjDo
        call countMissing(iobjSeq(i), nviewAll, izExclude, &
            numExclude, missing, listz, numListZ)
        missTot = missTot + numListZ
      enddo
      write(*,121) areaObjStr, listSeq(iseq), iseqPass, numObjDo, missTot
121   format('For ',a,i3,', round',i3,':',i3, ' contours, points missing =',i5)
    endif
    !
    ! Save residuals and XYZ values on every round if tiltalign run
    if ((elongFile .ne. ' ' .or. xyzFile .ne. ' ') .and. ifAlignDone .ne. 0) then
      !
      ! Find each object in the real object list and move residual/XYZ data into list
      do i = 1, numObjDo
        do j = 1, nrealpt
          if (iobjAli(j) == iobjSeq(i)) then
            residLists(i + indObjList(lastSeq) - 1) = resMean(iobjAli(j), ivsOnAlign)
            xyzAllArea(1:3, i + indObjList(lastSeq) - 1) = xyz(1:3, j)
            exit
          endif
        enddo
      enddo
    endif
  enddo
  !
  ! output lists of missing points, except for excluded sections
  !
  write(*,118)
118 format(/,'Obj cont:  Views on which points are missing')
  missTot = 0
  do iobj = 1, maxObjOrig

    call countMissing(iobj, nviewAll, izExclude, numExclude, missing, listz, numListZ)
    if (numListZ .ne. 0) then
      call objToCont(iobj, obj_color, imodObj, imodCont)
      write(*,117) imodObj, imodCont
117   format(i2,i4,': ',$)
      call wrlist(listz, numListZ)
    endif
    missTot = missTot + numListZ
  enddo
  print *,'Total points missing =', missTot
  !
  ! Write out residual file and XYZ file
  if (elongFile .ne. ' ' .or. xyzFile .ne. ' ') then
    if (xyzFile .ne. ' ') then
      deallocate(boxes, corrSum, stat=ierr)
      call adjustXYZinAreas(iobjLists, maxOlist, indObjList, ninObjList, xyzAllArea, &
          nobjLists, xyzSave, maxObjOrig)
      call dopen(7, xyzFile, 'new', 'f')
    endif

    if (elongFile .ne. ' ') call dopen(4, elongFile, 'new', 'f')

    ! Get average of the edge SD values
    sdsum = 0.
    iy = 0
    do iobj = 1, maxObjOrig
      resMean(iobj, 1) = 0.
      do iview = minViewDo, maxViewDo
        if (edgeSDsave(iview, iobj) > 0) then
          sdsum = sdsum + edgeSDsave(iview, iobj)
          iy = iy + 1
        endif
      enddo
    enddo
    if (iy > 0) sdavg = sdsum / iy
    !
    ! Add up mean residuals for all points that have any
    do i = 1, indFree
      if (residLists(i) >= 0.) then
        iobj = iobjLists(i)
        numResSaved(iobj) = numResSaved(iobj) + 1
        resMean(iobj, 1) = resMean(iobj, 1) + residLists(i)
      endif
    enddo
    !
    ! For each real point, get the top/bottom number, mean residual, and mean, sd, min
    ! and max of the edge sd
    do iobj = 1, maxObjOrig
      if (npt_in_obj(iobj) == 0) cycle
      call objToCont(iobj, obj_color, imodObj, imodCont)
      xpos = -1.
      if (numResSaved(iobj) > 0) xpos = resMean(iobj, 1) / numResSaved(iobj)
      sdsum = 0. ; sdsumsq = 0.; sdmin = 1.e20; sdmax = -1.; iy = 0 ; ix = 0
      do iview = minViewDo, maxViewDo
        xtmp = edgeSDsave(iview, iobj) / sdavg
        if (xtmp > 0) then
          sdsum = sdsum + xtmp
          sdsumsq = sdsumsq + xtmp**2
          sdmin = min(sdmin, xtmp)
          sdmax = max(sdmax, xtmp)
          iy = iy + 1
          prevRes(iy) = xtmp
        endif
        if (elongSave(iview, iobj) > 0) then
          ix = ix + 1
          tiltAll(ix) = elongSave(iview, iobj)
        endif
      enddo
      sdmean = -1. ; edgeSDsd = 0. ; sdmed = -1.
      if (iy < 1) then
        sdmin = -1.
      else if (iy > 1) then
        call sums_to_avgsd(sdsum, sdsumsq, iy, sdmean, edgeSDsd, xtmp)
        call rsSortFloats(prevRes, iy)
        call rsMedianOfSorted(prevRes, iy, sdmed)
      endif
      elongMean = 0.; elongMed = 0.
      if (ix > 1) then
        call avgsd(tiltAll, ix, elongMean, elongMed, xtmp)
        call rsSortFloats(tiltAll, ix)
        call rsMedianOfSorted(tiltAll, ix, elongMed)
      endif
      if (elongFile .ne. ' ') write(4, '(i3,i7,f12.4,5f11.4)')imodObj, imodCont, xpos, &
          sdmean, sdmed, edgeSDsd, elongMean, elongMed
           
      if (xyzFile .ne. ' ' .and. xpos >= 0 .and. (xyzSave(1,iobj) .ne. 0. .or.  &
          xyzSave(2,iobj) .ne. 0. .or. xyzSave(3,iobj) .ne. 0.)) &
          write(7,'(i6,4f12.3)') iobj, (xyzSave(i,iobj),i=1,3), xpos
    enddo
    if (elongFile .ne. ' ') close(4)
    if (xyzFile .ne. ' ') close(7)
  endif

  !
  ! convert index coordinates back to model coordinates
  !
  call putImodZscale(min(50., 0.75 * max(nx, ny) / nz))
  call scaleModelToImage(1, 1)
  call write_wmod(modelFile)
  if (outFile .ne. ' ') then
    call setsiz_sam_cel(2, nxBox, nyBox, nzout)
    call setsiz_sam_cel(3, nxBox, nyBox, nzout)
    call setsiz_sam_cel(4, nxpad, nypad, nzout)
    CALL iwrhdr(2, title, 1, boxMin, boxMax, boxSum / nzout)
    CALL iwrhdr(3, title, 1, refMin, refMax, refSum / nzout)
    CALL iwrhdr(4, title, 1, corrMin, corrMax, corroSum / nzout)
    CALL imclose(2)
    CALL imclose(3)
    CALL imclose(4)
    close(2)
    close(3)
  endif
  call exit(0)


CONTAINS

  ! countAndPreparePointsToDo evaluates each potential point on a new view to see
  ! if it already done, a gap to be skipped, or has no near enough neighbors
  ! Then it loads necessary boxes, forms averages and evaluates wsum if needed
  !
  subroutine countAndPreparePointsToDo()
    integer*4 numberInList, numZeroW, numWtot
    logical*4 getEdgeSDsave
    real*4 snrSum, snrSumb, bestSum
    !
    ! first see how many are done on this view
    !
    numToDo = 0
    numWtot = 0
    numZeroW = 0
    snrSum = 0.
    snrSumb = 0.
    getEdgeSDsave = getEdgeSD
    if (iseq == 1 .and. ivl == 1) getEdgeSD = .true.
    do iobjDo = 1, numObjDo
      iobj = iobjSeq(iobjDo)
      numInObj = npt_in_obj(iobj)
      nClose = 0
      nFarther = 0
      ifFound(iobjDo) = 0
      ibase = ibase_obj(iobj)
      !
      ! find closest ones within gap distance, find out if already
      ! done, mark as a 2 to protect them
      ! ipNearest holds the point number of the nearest for each object
      ! ipClose is the list of points within gap distance for this object
      !
      nearDiff = 10000
      do ip = 1, numInObj
        ipt = object(ibase + ip)
        izv = nint(p_coord(3, ipt)) + 1
        if (abs(izv - iview) < nearDiff) then
          nearDiff = abs(izv - iview)
          ipNearest(iobjDo) = ip
        endif
        if (izv == iview) then
          ifFound(iobjDo) = 2
        elseif (abs(izv - iview) <= max(maxgap, 2 * maxSobelSum)) then
          xbox = p_coord(1, ipt)
          ybox = p_coord(2, ipt)
          izBox = nint(p_coord(3, ipt))
          call findPiece(ixPclist, iyPclist, izPclist, npclist, nx, ny, nxBox, nyBox, &
              xbox, ybox, izBox, ix0, ix1, iy0, iy1, ipcz, ifReadXfs, prexf, needTaper)
          if (ipcz >= 0) then
            if (abs(izv - iview) <= maxgap) nClose = nClose + 1
            nFarther = nFarther + 1
            ipClose(nFarther) = ip
            izClose(nFarther) = izv
          endif
        endif
      enddo
      !
      ! if not found and none close, mark as -1 not to do
      ! If there are close ones, make sure this is not a gap to preserve
      !
      if (nClose == 0 .and. ifFound(iobjDo) == 0) ifFound(iobjDo) = -1
      if (nClose > 0 .and. ifFound(iobjDo) == 0) then
        if (ifFillIn == 0 .and. indgap(iobj + 1) > indgap(iobj)) then
          !
          ! if not filling in, look for gap on list
          !
          do igap = indgap(iobj), indgap(iobj + 1) - 1
            if (ivGap(igap) == iview) ifFound(iobjDo) = -1
          enddo
        endif
        if (ifFound(iobjDo) == 0) then
          !
          ! order feasible points by distance
          !
          numToDo = numToDo + 1
          do ic = 1, nFarther - 1
            do jc = ic + 1, nFarther
              if (abs(izClose(jc) - iview) < abs(izClose(ic) - iview)) then
                itmp = izClose(ic)
                izClose(ic) = izClose(jc)
                izClose(jc) = itmp
                itmp = ipClose(ic)
                ipClose(ic) = ipClose(jc)
                ipClose(jc) = itmp
              endif
            enddo
          enddo
          maxBoxUse = min(nFarther, maxAnySum)
          !
          ! see if boxes in memory match, free them if not
          !
          do ibox = 1, maxAnySum
            if (inCore(ibox, iobjDo) >= 0) then
              !
              ! First subtract from correlation sum
              if (inCorrSum(ibox, iobjDo)) then
                if (numberInList(inCore(ibox, iobjDo), izClose, maxSum, 0) == 0) then
                  corrSum(1:npixBox, iobjDo) = corrSum(1:npixBox, iobjDo) - &
                      boxes(1:npixBox, ibox, iobjDo)
                  inCorrSum(ibox, iobjDo) = .false.
                endif
              endif
              !
              ! Then subtract from sobel sum
              if (inSobelSum(ibox, iobjDo)) then
                if (numberInList(inCore(ibox, iobjDo), izClose, maxSobelSum, 0) == 0) then
                  sobelSum(1:npixBox, iobjDo) = sobelSum(1:npixBox, iobjDo) - &
                      boxes(1:npixBox, ibox, iobjDo)
                  inSobelSum(ibox, iobjDo) = .false.
                  numInSobelSum(iobjDo) = numInSobelSum(iobjDo) - 1
                endif
              endif
              if (numberInList(inCore(ibox, iobjDo), izClose, maxBoxUse, 0) == 0) &
                  inCore(ibox, iobjDo) = -1
            endif
          enddo
          !
          ! now load ones that are needed into last free box and form sums
          !
          do ic = 1, maxBoxUse
            if (numberInList(izClose(ic), inCore(1, iobjDo), maxAnySum, 0) == 0) then
              !
              ! If not loaded, look up last free box
              do ibox = 1, maxAnySum
                if (inCore(ibox, iobjDo) < 0) ib = ibox
              enddo
              inCore(ib, iobjDo) = izClose(ic)
              ipt = abs(object(ibase + ipClose(ic)))
              xbox = p_coord(1, ipt)
              ybox = p_coord(2, ipt)
              izBox = nint(p_coord(3, ipt))
              call findPiece(ixPclist, iyPclist, izPclist, npclist, nx, ny, nxBox, &
                   nyBox, xbox, ybox, izBox, ix0, ix1, iy0, iy1, ipcz, ifReadXfs, prexf, &
                   needTaper)
              call loadBoxAndTaper()
              !
              ! do subpixel shift and calculate CG and wsum value
              ! if it is not already saved
              !
              xt = nint(xbox) - xbox
              yt = nint(ybox) - ybox
              call qdshift(boxTmp, boxes(1, ib, iobjDo) , nxBox, nyBox, xt, yt)
              if (wsumSave(izBox + 1, iobjSeq(iobjDo)) < 0.) then
                xtmp = 0.
                ytmp = 0.
                call calcCG(boxes(1, ib, iobjDo), nxBox, nyBox, xtmp, ytmp,  wsum, edgeSD)
                wsumSave(izBox + 1, iobjSeq(iobjDo)) = wsum
                if (getEdgeSDsave .and. izBox + 1 >= minViewDo .and. &
                    izBox + 1 <= maxViewDo) then
                  edgeSDsave(izBox + 1, iobjSeq(iobjDo)) = edgeSD
                  call calcElongation(boxes(1, ib, iobjDo), nxBox, nyBox, 0., 0.,  &
                      elongSave(izBox + 1, iobjSeq(iobjDo)))
                endif

                ! Check the integrals of the seed for light/dark beads setting
                if (iseq == 1 .and. ivl == 1) then
                  numWtot = numWtot + 1
                  if (wsum <= 0.) numZeroW = numZeroW + 1
                  xtmp = 0.
                  ytmp = 0.
                  call bestCenterForCG(boxTmp, nxBox, nyBox, xtmp, ytmp, ix, iy, bestSum)
                  print *,wsum,edgeSD
                  snrSum = snrSum + (wsum * 4. / (3.14159 * diameter**2)) /  &
                      max(abs(1.e-10 * wsum), edgeSD)
                  snrSumb = snrSumb + bestSum / max(abs(1.e-10 * wsum), edgeSD)
                endif
              endif
            else
              !
              ! Or if it is loaded, look it up
              do ibox = 1, maxAnySum
                if (inCore(ibox, iobjDo) == izClose(ic)) ib = ibox
              enddo
            endif
            !
            ! Add to correlation sum if not in there
            if (ic <= maxSum .and. .not. inCorrSum(ib, iobjDo)) then
              inCorrSum(ib, iobjDo) = .true.
              corrSum(1:npixBox, iobjDo) = corrSum(1:npixBox, iobjDo) + &
                  boxes(1:npixBox, ib, iobjDo)
            endif
            !
            ! Add to sobel sum if not there
            if (ic <= maxSobelSum .and. .not. inSobelSum(ib, iobjDo)) then
              inSobelSum(ib, iobjDo) = .true.
              numInSobelSum(iobjDo) = numInSobelSum(iobjDo) + 1
              sobelSum(1:npixBox, iobjDo) = sobelSum(1:npixBox, iobjDo) + &
                  boxes(1:npixBox, ib, iobjDo)
            endif
          enddo
        endif
      endif
      ipNearSave(iobjDo) = ipNearest(iobjDo)
    enddo
    if (numWtot > 0 .and. numZeroW >= numWtot / 2) then
      if (ifWhite .ne. 0) call exitError( &
          'MOST BEADS ARE DARKER THAN BACKGROUND; DO NOT USE THE LIGHT BEADS OPTION')
      if (ifWhite .ne. 0) call exitError('MOST BEADS ARE LIGHTER THAN BACKGROUND;'// &
          ' YOU NEED TO USE THE LIGHT BEADS OPTION')
    endif
    getEdgeSD = getEdgeSDsave
    !if (numWtot > 0) then
    !  print *, 'SNR is ', snrSum / numWtot, snrSumb / numWtot
    !endif
    return
  end subroutine countAndPreparePointsToDo


  ! getProjectedPositionsSetupFits
  !
  subroutine getProjectedPositionsSetupFits()
    integer*4 indRealInAli(numObjDo), justAverage
    real*4 cosd, sind

    ! Get the index of points in tiltalign and determine if all points to be done were
    ! not in align AND consist of only 1 or 2 points on previous 2 views
    justAverage = 1
    do iobjDo = 1, numObjDo
      iobj = iobjSeq(iobjDo)
      indRealInAli(iobjDo) = 0
      if (ifDidAlign == 1) then
        do ipt = 1, nrealpt
          if (iobjSeq(iobjDo) == iobjAli(ipt)) indRealInAli(iobjDo) = iobjSeq(iobjDo)
        enddo
      endif
      if (ifFound(iobjDo) == 0) then
        if (indRealInAli(iobjDo) .ne. 0 .or. npt_in_obj(iobj) > 2) justAverage = 0
        if (justAverage > 0) then
          justAverage = max(justAverage,  npt_in_obj(iobj))
          xtmp = nint(p_coord(3, object(ibase_obj(iobj) + 1)) + 1.)
          ytmp = nint(p_coord(3, object(ibase_obj(iobj) + npt_in_obj(iobj))) + 1.)
          if ((xtmp .ne. iview - idir .and. xtmp .ne. iview - 2 * idir) .or. &
              (ytmp .ne. iview - idir .and. ytmp .ne. iview - 2 * idir)) justAverage = 0
        endif
      endif
    enddo

    ! If just averaging seems to be in order, now try to do it for all points, but
    ! bail out if a previously done point does not have at least one in the range
    if (justAverage > 0) then
      do iobjDo = 1, numObjDo
        iobj = iobjSeq(iobjDo)
        ibase = ibase_obj(iobj)
        xpos = 0.
        ypos = 0.
        ix = 0
        do ipt = 1, npt_in_obj(iobj)
          ip = object(ibase + ipt)
          xtmp = nint(p_coord(3, ip) + 1.)
          if (xtmp == iview - idir .or. xtmp == iview - justAverage * idir) then
            xpos = xpos + p_coord(1, ip)
            ypos = ypos + p_coord(2, ip)
            ix = ix + 1
          endif
        enddo
        if (ix == 0) then
          justAverage = 0
          exit
        endif
        xseek(iobjDo) = xpos / ix
        yseek(iobjDo) = ypos / ix
        !if (saveAllPoints) print *,'justAvg',iobj,xseek(iobjDo), yseek(iobjDo)
      enddo
    endif
    !
    ! If not just averaging, get tentative tilt, rotation, mag for current view
    !
    if (ifDidAlign > 0 .and. justAverage == 0) then
      ivuse = iview
      if (mapFileToView(iview) == 0) then
        ivDel = 200
        do iv = 1, nview
          if (abs(mapViewToFile(iv) - iview) < ivDel) then
            ivDel = abs(mapViewToFile(iv) - iview)
            ivuse = mapViewToFile(iv)
          endif
        enddo
      endif
      if (saveAllPoints) print *,'ivuse, iview', ivuse, iview
      tiltCur = tiltOrig(ivuse) + tiltAll(iview) - tiltAll(ivuse)
      gmagCur = gmagOrig(ivuse)
      rotCur = rotOrig(ivuse)
      cosr = cosd(rotCur)
      sinr = sind(rotCur)
      cost = cosd(tiltCur)
      sint = sind(tiltCur)
      dxCur = dxySave(1, ivuse)
      dyCur = dxySave(2, ivuse)
      if (ivuse .ne. iview .and. tiltMax <= 80.) then
        !
        ! if going from another view and cosine stretch is appropriate,
        ! back - rotate the dxy so tilt axis vertical, adjust the
        ! dx by the difference in tilt angle, and rotate back
        a = cosr * dxCur + sinr * dyCur
        b = -sinr * dxCur + cosr * dyCur
        a = a * cost / cosd(tiltOrig(ivuse))
        dxCur = cosr * a - sinr * b
        dyCur = sinr * a + cosr * b
      endif
      a = gmagCur * cost * cosr
      b = - gmagCur * sinr
      c = gmagCur * sint * cosr
      d = gmagCur * cost * sinr
      e = gmagCur * cosr
      f = gmagCur * sint * sinr
    endif
    !
    ! now get projected positions otherwise, and save the points
    !
    do iobjDo = 1, numObjDo
      indr = indRealInAli(iobjDo)
      iobj = iobjSeq(iobjDo)
      ibase = ibase_obj(iobj)
      if (indr .ne. 0 .and. justAverage == 0) then
        !
        ! there is a 3D point for it: so project it
        !
        xseek(iobjDo) = a * xyzSave(1, indr) + b * xyzSave(2, indr) +  &
            c * xyzSave(3, indr) + dxCur + xcen
        yseek(iobjDo) = d * xyzSave(1, indr) + e * xyzSave(2, indr) +  &
            f * xyzSave(3, indr) + dyCur + ycen
        !if (saveAllPoints) print *,'3d proj',iobj,xseek(iobjDo), yseek(iobjDo)
      else if (justAverage == 0) then
        call nextPos(iobj, ipNearest(iobjDo), idir, iznext, tiltAll, numFit, &
            minFit, rotStart, tiltFitMin, izExclude, numExclude, xseek(iobjDo),  &
            yseek(iobjDo))
        !if (saveAllPoints) print *,'nextPos',iobj,p_coord(1, object(ibase +  &
            !ipNearest(iobjDo))),p_coord(2, object(ibase + ipNearest(iobjDo))), &
            !xseek(iobjDo), yseek(iobjDo)
      endif
      if (saveAllPoints) then
        iobjSave = iobj + (5 * ipass - 4) * maxObjOrig
        call add_point(iobjSave, 0, xseek(iobjDo), yseek(iobjDo), iznext)
      endif
    enddo
    !
    ! build list of points already found for fitting; put
    ! actual positions in 4 and 5 for compatibiity with xfmodel
    !
    numData = 0
    do iobjDo = 1, numObjDo
      if (ifFound(iobjDo) > 0) then
        numData = numData + 1
        xr(1, numData) = xseek(iobjDo) - xcen
        xr(2, numData) = yseek(iobjDo) - ycen
        ipt = object(ibase_obj(iobjSeq(iobjDo)) + ipNearest(iobjDo))
        xr(4, numData) = p_coord(1, ipt) - xcen
        xr(5, numData) = p_coord(2, ipt) - ycen
        xr(6, numData) = iobjDo
      endif
    enddo
    return
  end subroutine getProjectedPositionsSetupFits

  ! Analyze wsums in the vicinity and make up a criterion for each bead
  !
  subroutine getWsumCriteria()
    real*4 ivFit(maxWavg + 2), wsFit(maxWavg + 2), wLocalMeans(-maxWavg:maxWavg)
    real*4 prederr, prSlope, prIntcp, prro, prsa, prsb, prse, wpred, wpredLocal
    real*4 wsPctl(maxWavg + 2)
    real*4 wsumsLocal(maxWneigh * (maxWavg + 1)), wlocalSum, dblNormCrit
    integer*4 minWsumForPred, numInWlocalMean(-maxWavg:maxWavg), maxLocalForPred
    integer*4 minUsableNum, maxNumInMean, numLocals, numNeighInLocal, minDif, numPctl
    real*8 percentileFloat
    minWsumForPred = 4
    maxLocalForPred = 7

    ! Go through the collection of neighbors for wsum and get the mean on views in
    ! a large range
    numLocals = 0
    numInWlocalMean(:) = 0
    wLocalMeans(:) = 0
    do iobjDo = 1, numWneighbors(lastSeq)
      iobj = neighborsForWfits(iobjDo, lastSeq)
      do itry = max(minViewDo, iview - maxWavg), min(maxViewDo, iview + maxWavg)
        idif = itry - iview
        if (wsumSave(itry, iobj) >= 0.) then
          numInWlocalMean(idif) = numInWlocalMean(idif) + 1
          wLocalMeans(idif) = wLocalMeans(idif) + wsumSave(itry, iobj)
        endif
      enddo
    enddo

    ! Get the mean, and max number in any group, and limit the usable usable ones to
    ! ones with at least 3 and at least 1/5 of the maximum up to 9.
    maxNumInMean = 0
    do i = -maxWavg, maxWavg
      maxNumInMean = max(maxNumInMean, numInWlocalMean(i))
      if (numInWlocalMean(i) > 0) wLocalMeans(i) = wLocalMeans(i) / numInWlocalMean(i)
    enddo
    minUsableNum = min(9, max(3, maxNumInMean / 5))

    ! Do a fit over usable means
    numAvg = 0
    minDif = 100
    LOCAL_FIT_LOOP: do ix = 0, maxWavg
      do idirw = -1, 1, 2
        idif = ix * idirw
        if (numInWlocalMean(idif) >= minUsableNum) then
          minDif = min(minDif, abs(idif))
          numAvg = numAvg + 1
          ivFit(numAvg) = idif
          wsFit(numavg) = wLocalMeans(idif)
          if (numAvg >= maxLocalForPred) then
            exit LOCAL_FIT_LOOP
          endif
        endif
      enddo
    enddo LOCAL_FIT_LOOP

    wpredLocal = 0.

    ! Get the average and do a predictive line fit if there are enough
    if (numAvg > 0) then
      call avgsd(wsFit, numAvg, wsumAvg, wsumSD, xtmp)
      if (numAvg > minWsumForPred .and. minDif < 3) then
        call lsFitPred(ivFit, wsFit, numAvg, prSlope, prIntcp, prro, prsa, prsb, &
            prse, 0., wpredLocal, prederr) 
        if (saveAllPoints) write(*,'(a,i4,6f10.2)')'pred',numAvg,wsumAvg,wsumSD, &
            wpredLocal, prederr,prSlope, prsb
      else
        wpredLocal = wsumAvg
      endif
    endif

    ! Look at the neighbors and get the mean of values normalized by view mean
    ! and collect.  This is done the same way as individuals below, go up to maxWavg
    ! views away and get up to maxWavg + 1 closest nviews
    numNeighInLocal = 0
    do iobjDo = 1, numWneighbors(lastSeq)
      iobj = neighborsForWfits(iobjDo, lastSeq)
      wsum = 0.
      numAvg = 0
      do ix = 0, maxWavg
        do idirw = -1, 1, 2
          idif = ix * idirw
          if (numInWlocalMean(idif) >= minUsableNum) then
            itry = iview + idif
            if (wsumSave(itry, iobj) >= 0.) then
              numAvg = numAvg + 1
              wstmp = wsumSave(itry, iobj) / wLocalMeans(idif)
              wsum = wsum + wstmp
              wsumsLocal(numLocals + numAvg) = wstmp
            endif
          endif
        enddo
        if (numAvg >= maxWavg) then
          exit
        endif
      enddo

      ! Divide the ones just added to the list by the bead mean so the list now has
      ! double-normalized values
      if (numAvg > 0 .and. wsum > 0.) then
        numNeighInLocal = numNeighInLocal + 1
        wsumsLocal(numLocals + 1 : numLocals + numAvg) =  &
            wsumsLocal(numLocals + 1 : numLocals + numAvg) / (wsum / numAvg)
        numLocals = numLocals + numAvg
      endif
    enddo

    ! Set the double-normalized criterion by taking what can be considered a 20th
    ! percentile point of the minima from all the neighbors, and down-rating that,
    ! but don't let it get too low
    dblNormCrit = 0.
    if (numLocals >= 5) then
      idif = 1 + numNeighInLocal / 5
      dblNormCrit = percentileCritFrac * percentileFloat(idif, wsumsLocal, numLocals)
      if (saveAllPoints) print *,numLocals,' locals, crit:',idif,dblNormCrit
      dblNormCrit = max(dblNormCrit, dblNormMinCrit)
    endif

    do iobjDo = 1, numObjDo
      if (ifFound(iobjDo) == 0 .or. ifFound(iobjDo) == 1) then
        !
        ! For each one to be done, accumulate sums for all the views in range of both
        ! the bead itself and of the local means on the same views.  Store the first
        ! maxWavg + 1 of those for personalized fitting if needed, and store a normalized
        ! value for views with usable local means for percentile finding
        !
        numAvg = 0
        wlocalSum = 0.
        numPctl = 0
        wsum = 0.
        wsumsq = 0.
        do ix = 0, maxWavg
          do idirw = -1, 1, 2
            idif = ix * idirw
            if (numInWlocalMean(idif) > 0) then
              itry = iview + idif
              wstmp = wsumSave(itry, iobjSeq(iobjDo))
              if (wstmp >= 0) then
                wsum = wsum + wstmp
                wsumsq = wsumsq + wstmp**2
                wlocalSum = wlocalSum + wLocalMeans(idif)
                numAvg  = numAvg + 1
                if (numAvg < maxWavg + 2) then
                  wsFit(numAvg) = wstmp
                  ivFit(numAvg) = idif
                endif
                if (numInWlocalMean(idif) >= minUsableNum) then
                  numPctl = numPctl + 1
                  wsPctl(numPctl) = wstmp / wLocalMeans(idif)
                endif
              endif
            endif
          enddo
          if (numPctl >= maxWavg) then
            exit
          endif
        enddo

        call sums_to_avgsd(wsum, wsumsq, numAvg, wsumAvg, wsumSD)
        wlocalSum = wlocalSum / numAvg
        if (wpredLocal > 0. .and. numPctl > 0) then

          ! If there is a good local prediction, then use  a down-rated 20th percentile
          ! value if there is enough data in the distruibution, otherwise use the
          ! established criterion.  Do not let the normalized criterion get too low
          xtmp = dblNormCrit
          if (numPctl >= 5 .or. dblNormCrit == 0) then
            idif = 1 + numPctl / 5
            xtmp = max(percentileCritFrac * percentileFloat(idif, wsPctl, numPctl), &
                dblNormMinCrit)
          endif

          ! Scale the criterion by the predicted value and the ratio of this bead's mean
          ! to the corresponding local mean.  Limit to a small fraction of the local mean
          ! 
          wsumCrit(iobjDo) = max(xtmp * wpredLocal * wsumAvg / wlocalSum, &
              wcritToLocalMinRatio * wlocalSum)
          wsumCrit(iobjDo) = min(wsumCrit(iobjDo), percentileCritFrac * wsumAvg)
          if (saveAllPoints) write(*,'(a,3i4,2f8.0,f8.4,f8.0)')'crit',iobjSeq(iobjDo), &
              numPctl, numAvg, wsumAvg, wlocalSum, xtmp, wsumCrit(iobjDo)
        else

          ! If no good local prediction, fall back to the old mean/SD based criterion but
          ! Do not allow it to get too low.  Then, if there is enough data for a line fit,
          ! get a predicted value and use it if the error is low enough and the slope is
          ! significant.  Relax the predicted value by less than the mean would be
          wsumCrit(iobjDo) = max(min(fracCrit * wsumAvg, wsumAvg - sdCrit * wsumSD), &
              wsumAvg * dblNormMinCrit)
          if (numAvg >= nint(1.5 * minWsumForPred)) then
            call lsfitPred(ivFit, wsFit, min(numAvg, maxWavg + 1), prSlope, prIntcp, &
                prro, prsa, prsb, prse, 0., wpred, prederr)
            !write(*,'(a,5f8.0)')'bead fit',wsumAvg, wsumSD, wpred, prSlope, prsb
            if (prederr < wsumSD .and. abs(prSlope) > 2.5 * prsb)  then
                wsumCrit(iobjDo) = max((1. - 0.6 * (1. - fracCrit)) * wpred, &
                    wsumAvg * dblNormMinCrit)
              !write(*,'(a,3f8.0)')'replacement',wsumCrit(iobjDo)
            endif
          endif

        endif
      endif
    enddo
    return
  end subroutine getWsumCriteria


  ! findAllBeadsOnView
  !
  subroutine findAllBeadsOnView()
    do iobjDo = 1, numObjDo
      if (ifFound(iobjDo) == 0) then
        iobj = iobjSeq(iobjDo)
        xnext = xseek(iobjDo)
        ynext = yseek(iobjDo)
        !
        ! if there are existing points, find right kind of transform
        ! and modify position
        !
        if (numPioneer > 0 .or. numData >= limPtsShift) then
          ifRoTrans = 0
          ifTrans = 1
          ndatFit = numData
          if (numData < numPioneer + limPtsShift) ndatFit = numPioneer
          if (ndatFit >= limPtsStretch) then
            ifTrans = 0
          elseif (ndatFit >= limPtsMag) then
            ifRoTrans = 2
          elseif (ndatFit >= limPtsRot) then
            ifRoTrans = 1
          endif
          maxDrop = nint(0.26 * ndatFit)
          if (ndatFit < 4 .or. ifDidAlign == 0) maxDrop = 0
          call findxf_wo_outliers(xr, msizeXR, ndatFit, xcen, ycen, ifTrans, ifRoTrans, &
              maxDrop, outlieCrit, outlieCritAbs, outlieElimMin, idrop, numDrop, xf, &
              devAvg, devSD, devMax, ipntMaxDev)
          call xfApply(xf, xcen, ycen, xseek(iobjDo), yseek(iobjDo), xnext, ynext)
          if (saveAllPoints) then
            iobjSave = iobj + (5 * ipass - 3) * maxObjOrig
            call add_point(iobjSave, 0, xnext, ynext, iznext)
          endif
        endif
        !
        call findPiece(ixPclist, iyPclist, izPclist, npclist, nx, ny, nxBox, nyBox, &
            xnext, ynext, iznext, ix0, ix1, iy0, iy1, ipcz, ifReadXfs, prexf, needTaper)
        if (ipcz >= 0) then
          call lookForOneBead()
        endif
      endif
    enddo
    return
  end subroutine findAllBeadsOnView

  ! lookForOneBead
  !
  subroutine lookForOneBead()
    integer*4 scaledSobel
    real*4 xcXpeaks(maxInitCorrPeaks), xcYpeaks(maxInitCorrPeaks)
    real*4 xcPeaks(maxInitCorrPeaks), wsumTmp, edgeSDtmp
    !
    ! get image area
    !
    call loadBoxAndTaper()

    ! Do cross-correlation on first pass
    if (ipass == 1) then
      !
      ! pad image into array on first pass, pad correlation sum into brray
      call taperInPad(boxTmp, nxBox, nyBox, array, nxpDim, nxpad, nypad, nxTaper, nyTaper)
      call meanZero(array, nxpDim, nxpad, nypad)
      call taperInPad(corrSum(1, iobjDo), nxBox, nyBox, brray, nxpDim, nxpad, nypad, &
          nxTaper, nyTaper)
      call meanZero(brray, nxpDim, nxpad, nypad)
      !
      ! correlate the two
      !
      call todfft(array, nxpad, nypad, 0)
      call todfft(brray, nxpad, nypad, 0)
      call conjugateProduct(array, brray, nxpad, nypad)
      if (deltaCtf .ne. 0) call filterPart(array, array, nxpad, nypad, ctf, deltaCtf)
      call todfft(array, nxpad, nypad, 1)
      !
      ! find peak of correlation
      !
      call xcorrPeakFind(array, nxpDim, nypad, xcXpeaks, xcYpeaks, xcPeaks,  &
          maxInitCorrPeaks)
      xpeak = xcXpeaks(1)
      ypeak = xcYpeaks(1)
      if (saveAllPoints) then
        iobjSave = iobj + (5 * ipass - 2) * maxObjOrig
        call add_point(iobjSave, 0, nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
      endif
      call calcCG(boxTmp, nxBox, nyBox, xpeak, ypeak, wsum, edgeSD)
      !
      ! Look at other peaks within range, if any, and mark point as isolated if there
      ! are none strong enough
      if (aloneRelaxCrit > 1.) then
        isIsolated(iview, iobjSeq(iobjDo)) = 1
        do i = 2, maxInitCorrPeaks
          if (xcPeaks(i) > -1.e29) then
            if (sqrt(xcXpeaks(i)**2 + xcYpeaks(i)**2) < aloneDistCrit * diameter) then
              xtmp = xcXpeaks(i)
              ytmp = xcYpeaks(i)
              call calcCG(boxTmp, nxBox, nyBox, xtmp, ytmp, wsumTmp, edgeSDtmp)
              if ((wsumTmp > aloneWsumCrit * wsum .and.  &
                  xcPeaks(i) > alonePeakCrit * xcPeaks(1)) .or.  &
                  (wsumTmp > 0.75 * aloneWsumCrit * wsum .and.  &
                  xcPeaks(i) > 0.75 * alonePeakCrit * xcPeaks(1) .and. &
                  wsumTmp * xcPeaks(i) > aloneWsumCrit * wsum * alonePeakCrit *  &
                  xcPeaks(1))) then
                ! print *,xcXpeaks(i), xcYpeaks(i),xtmp,ytmp,wsumTmp,wsum,xcPeaks(i),xcPeaks(1)
                isIsolated(iview, iobjSeq(iobjDo)) = 0
                exit
              endif
            endif
          endif
        enddo
        ! print *,iobjSeq(iobjDo), isIsolated(iview, iobjSeq(iobjDo))
      endif
    endif
    !
    ! Need sobel filter peak positions on both passes
    ! First get sobel sum with enough beads in it
    if (maxSobelSum > 0) then
      curSum(1:npixBox) = sobelSum(1:npixBox, iobjDo)
      ninSobel = numInSobelSum(iobjDo)
      do i = 1, numObjDo
        if (ninSobel >= maxSobelSum) then
          exit
        endif
        if (i .ne. iobjDo) then
          curSum(1:npixBox) = curSum(1:npixBox) + sobelSum(1:npixBox, i)
          ninSobel = ninSobel + numInSobelSum(i)
        endif
      enddo
      !
      ! It seems like a model bead is better for low numbers in the average
      if (ninSobel < maxSobelSum / 2 .and. nxbox == nybox) &
          call makeModelBead(nxbox, 1.05 * diameter, curSum)
      !
      ! sobel filter the sum
      ierr = scaledSobel(curSum, nxBox, nyBox, scaleFacSobel, scaleByInterp, &
          interpType, 2., refSobel, nxSobel, nySobel, xOffSobel, yOffSobel)
      if (ierr .ne. 0) call errorExit('DOING SOBEL FILTER ON SUMMED BEADS', 0)
      ! write(*,'(i4,13i5)') (nint(refSobel(i)), i = 1, 14 * 14)
      !
      ! Gaussian filter the box and sobel filter it
      if (sobelSigma > 0) then
        call applyKernelFilter(boxTmp, tmpSobel, nxBox, nxBox, nyBox, matKernel, &
            kernelDim)
        ierr = scaledSobel(tmpSobel, nxBox, nyBox, scaleFacSobel, scaleByInterp, &
            interpType, 2., boxSobel, nxSobel, nySobel, xOffSobel, yOffSobel)
      else
        ierr = scaledSobel(boxTmp, nxBox, nyBox, scaleFacSobel, scaleByInterp, &
            interpType, 2., boxSobel, nxSobel, nySobel, xOffSobel, yOffSobel)
      endif
      if (ierr .ne. 0) call errorExit('DOING SOBEL FILTER ON SEARCH BOX', 0)
      !
      ! Taper / pad etc
      call taperInPad(boxSobel, nxSobel, nySobel, sarray, nxsPad + 2, nxsPad, &
          nysPad, nxTaper, nyTaper)
      call meanZero(sarray, nxsPad + 2, nxsPad, nysPad)
      call taperInPad(refSobel, nxSobel, nySobel, sbrray, nxsPad + 2, nxsPad, nysPad, &
          nxTaper, nyTaper)
      call meanZero(sbrray, nxsPad + 2, nxsPad, nysPad)
      !
      ! correlate the two
      call todfft(sarray, nxsPad, nysPad, 0)
      call todfft(sbrray, nxsPad, nysPad, 0)
      call conjugateProduct(sarray, sbrray, nxsPad, nysPad)
      call todfft(sarray, nxsPad, nysPad, 1)
      !
      ! get the peaks and adjust them for the sobel scaling
      ! get the centroid offset of the average and use it to adjust the positions too
      ! the original offsets from scaledSobel are irrelevant to correlation
      call xcorrPeakFind(sarray, nxsPad + 2, nysPad, sobelXpeaks, &
          sobelYpeaks, sobelPeaks, maxPeaks)
      call calcCG(curSum, nxBox, nyBox, xOffSobel, yOffSobel, xtmp, ytmp)
      ! write(*,'(2i5,a,2f7.2)') iobjDo, ninSobel, '  Sobel average offset', xOffSobel, yOffSobel
      sobelWsums(1:maxPeaks) = -1.
      sobelXpeaks(1:maxPeaks) = sobelXpeaks(1:maxPeaks) * &
          scaleFacSobel + xOffSobel
      sobelYpeaks(1:maxPeaks) = sobelYpeaks(1:maxPeaks) * &
          scaleFacSobel + yOffSobel
      !do i = 1, maxPeaks
      !   if (sobelPeaks(i) > - 1.e29) &
      !     write(*,'(2f7.2,f13.2)') sobelXpeaks(i), sobelYpeaks(i), &
      !        sobelPeaks(i)
      !  enddo

      ! Revise position of current peak on first pass
      if (ipass == 1) &
        call findNearestSobelPeak()
    endif

    if (ipass == 1) then
      dist = sqrt(xpeak**2 + ypeak**2)
      if (saveAllPoints) then
        iobjSave = iobj + (5 * ipass - 1) * maxObjOrig
        call add_point(iobjSave, 0, nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
      endif
    endif
    if (wsum < wsumCrit(iobjDo) .or. dist > distCrit .or. ipass == 2) then
      !
      ! rescue attempt; search concentric rings from the
      ! center of box and take the first point that goes
      ! above the relaxed wsum criterion
      !
      if (ipass == 1) then
        if (dist > distCrit) then
          relax = relaxDist
          if (ifTrace .ne. 0) &
              write(*,102) 'distance', iznext, dist, wsum, wsumCrit(iobjDo)
102       format(' Rescue-',a,', sec',i4,', dist=',f5.1, &
              ', dens=',f9.0,', crit=',f9.0)
        else
          relax = relaxInt
          if (ifTrace .ne. 0) &
              write(*,102) 'density ', iznext, dist, wsum, wsumCrit(iobjDo)
        endif
        radMax = max(nxBox, nyBox)
      else
        relax = relaxFit
        radMax = radMaxFit
      endif
      ! if (maxSobelSum > 0) then
      ! call rescueFromSobel(boxTmp, nxBox, nyBox, xpeak, ypeak, sobelXpeaks, &
      ! sobelYpeaks, sobelPeaks, sobelWsums, &
      ! maxPeaks, radMax, relax * wsumCrit(iobjDo), wsum, edgeSD)
      ! else
      call rescue(boxTmp, nxBox, nyBox, xpeak, ypeak, radMax, relax * wsumCrit(iobjDo), &
          wsum, edgeSD)
      if (maxSobelSum > 0) call findNearestSobelPeak()
      ! endif
    elseif (ipass == 2) then
      wsum = 0.
    endif
    !
    ! Add point if it passes wsum criterion
    !
    if (wsum > 0.) then
      wsumSave(iview, iobjSeq(iobjDo)) = wsum
      if (getEdgeSD) then 
        edgeSDsave(iview, iobjSeq(iobjDo)) = edgeSD
        call calcElongation(boxTmp, nxBox, nyBox, xpeak, ypeak,  &
            elongSave(iview, iobjSeq(iobjDo)))
      endif
      xpos = nint(xnext) + xpeak
      ypos = nint(ynext) + ypeak
      if (ifTrace .ne. 0) &
          write(*,'(a,3i5,4f7.1,2f12.0,f12.4)') 'add',nzout, iznext, iobj, &
          xnext, ynext, xpos, ypos, peak, wsum, edgeSD
      !
      ! add point to model
      !
      call add_point(iobj, ipNearest(iobjDo), xpos, ypos, iznext)
      if (saveAllPoints) then
        iobjSave = iobj + (5 * ipass) * maxObjOrig
        call add_point(iobjSave, 0, xpos, ypos, iznext)
      endif
      numAdded = numAdded + 1
      iobjDel(numAdded) = iobj
      !
      ! mark as found, add to data matrix for alignment
      !
      ifFound(iobjDo) = 1
      numData = numData + 1
      xr(1, numData) = xseek(iobjDo) - xcen
      xr(2, numData) = yseek(iobjDo) - ycen
      xr(4, numData) = xpos - xcen
      xr(5, numData) = ypos - ycen
      xr(6, numData) = iobjDo
      !
      if (outFile .ne. ' ') then
        if (modeBox == 2) then
          call iclden(boxTmp, nxBox, nyBox, 1, nxBox, 1, nyBox, tmin, tmax, tmean)
        else
          call isetdn(boxTmp, nxBox, nyBox, modeBox, 1, nxBox, 1, nyBox, tmin, tmax,tmean)
        endif
        call iwrsec(2, boxTmp)
        boxMin = min(boxMin, tmin)
        boxMax = max(boxMax, tmax)
        boxSum = boxSum + tmean
        nzout = nzout + 1
        boxTmp(1:npixBox) = corrSum(1:npixBox, iobjDo)
        do iy = 1, nyBox
          do ix = 1, nxBox
            xtmp = ix - nxBox / 2.
            ytmp = iy - nyBox / 2.
            call calcCG(boxTmp, nxBox, nyBox, xtmp, ytmp, brray(ix + (iy - 1) * nxBox),  &
                edgeSDtmp)
          enddo
        enddo
        boxTmp(1:npixBox) = brray(1:npixBox)
        if (modeBox == 2) then
          call iclden(boxTmp, nxBox, nyBox, 1, nxBox, 1, nyBox, tmin, tmax, tmean)
        else
          call isetdn(boxTmp, nxBox, nyBox, modeBox, 1, nxBox, 1, nyBox, tmin, tmax,tmean)
        endif
        call iwrsec(3, boxTmp)
        refMin = min(refMin, tmin)
        refMax = max(refMax, tmax)
        refSum = refSum + tmean
        call splitPack(array, nxpDim, nxpad, nypad, boxTmp)
        if (modeBox == 2) then
          call iclden(boxTmp, nxpad, nypad, 1, nxpad, 1, nypad, tmin, tmax, tmean)
        else
          call isetdn(boxTmp, nxpad, nypad, modeBox, 1, nxpad, 1, nypad, tmin, tmax,tmean)
        endif
        call iwrsec(4, boxTmp)
        corrMin = min(corrMin, tmin)
        corrMax = max(corrMax, tmax)
        corroSum = corroSum + tmean
        write(2, '(3i6)') nint(xnext) - nxBox / 2, nint(ynext) - nyBox / 2, iznext
        write(3, '(3i6)') nint(xnext) - nxpad / 2, nint(ynext) - nypad / 2, iznext
      endif
    endif
    return
  end subroutine lookForOneBead

  
  subroutine loadBoxAndTaper()
    integer*4 taperAtFill
    call imposn(1, ipcz, 0)
    call irdpas(1, boxTmp, nxBox, nyBox, ix0, ix1, iy0, iy1,*299)
    if (needTaper) then
      if (taperAtFill(boxTmp, nxBox, nyBox, nFillTaper, 1) .ne. 0) &
          call exitError('GETTING MEMORY FOR TAPERING FROM FILL IN BOX)')
    endif
    return
299 call errorExit('READING IMAGE FILE', 0)
  end subroutine loadBoxAndTaper


  ! Find the sobel peak nearest to the current position (xpeak, ypeak) that is
  ! valid and still reasonably strong, and substitute for xpeak, ypeak
  subroutine findNearestSobelPeak()
    integer*4 ibest
    real*4 distMin, peakMax, xpcen, ypcen, reduceFac
    !
    ! Tried to moved reference position toward center of expected position (0,0) by up to
    ! half a bead diameter - it was sometimes signinficantly worse
    xpcen = xpeak
    ypcen = ypeak
    ibest = 0
    peakMax = -1.e30
    distMin = 1.e20
    do i = 1, maxPeaks
      call checkSobelPeak(i, boxTmp, nxBox, nyBox, sobelXpeaks, sobelYpeaks, sobelPeaks, &
          sobelWsums, sobelEdgeSD, maxPeaks)
       !if (sobelPeaks(i) > - 1.e29) write(*,'(2f7.2,2f12.2)')  &
       !    sobelXpeaks(i), sobelYpeaks(i), sobelPeaks(i), &
       !    sobelWsums(i)

      if (sobelWsums(i) > 0) then
        if (peakMax < - 1.e29) peakMax = sobelPeaks(i)
        if (sobelPeaks(i) < minPeakRatio * peakMax) then
          exit
        endif
        dist = (sobelXpeaks(i) - xpcen)**2 + (sobelYpeaks(i) - ypcen)**2

        ! Do not allow it more than a diameter away or a lot weaker than the criterion
        if (dist < distMin .and. dist < diameter**2 .and.  &
            sobelWsums(i) > 0.5 * wsumCrit(iobjDo)) then
          distMin = dist
          ibest = i
        endif
      endif
    enddo
    if (ibest == 0) return

    ! On second pass, only accept a position if it is closer to the expected position (0)
    if (ipass == 2 .and. sobelXpeaks(ibest)**2 + sobelYpeaks(ibest)**2 >  &
        xpeak**2 + ypeak**2) return
    xpeak = sobelXpeaks(ibest)
    ypeak = sobelYpeaks(ibest)
    ! wsum = sobelWsums(ibest)
    ! print *,ibest, xpeak, ypeak, wsum
    return
  end subroutine findNearestSobelPeak


  ! redoFitsEvaluateResiduals
  !
  subroutine redoFitsEvaluateResiduals()
    real*4 relaxAllCrit
    if (numData >= limPtsShift) then
      ifRoTrans = 0
      ifTrans = 1
      if (numData >= limPtsStretch) then
        ifTrans = 0
      elseif (numData >= limPtsMag) then
        ifRoTrans = 2
      elseif (numData >= limPtsRot) then
        ifRoTrans = 1
      endif
      maxDrop = nint(0.26 * numData)
      if (numData < 4 .or. ifDidAlign == 0) maxDrop = 0
      call findxf_wo_outliers(xr, msizeXR, numData, xcen, ycen, ifTrans, ifRoTrans, &
          maxDrop, outlieCrit, outlieCritAbs, outlieElimMin, idrop, numDrop, xf, devAvg, &
          devSD, devMax, ipntMaxDev)
      write(*,113) iview, ipass, numData, numDrop, devAvg, devSD, devMax
113   format('view',i4,', pass',i2,',',i4, &
          ' points (-', i3,'), mean, sd, max dev:' ,3f8.2)
      if (ifDidAlign .ne. 0) then
        dxCur = dxCur + xf(1, 3)
        dyCur = dyCur + xf(2, 3)
      endif
    endif
    !
    ! do tiltAli if anything changed, so can get current residuals
    !
    if (numAdded > 0) then
      if (ifDidAlign .ne. 0 .and. ivuse .ne. iview) then
        dxySave(1, iview) = dxCur
        dxySave(2, iview) = dyCur
      endif
      call tiltAli(ifDidAlign, ifAlignDone, resMean(1, ivSeq),  iview)
      if (ifDidAlign .ne. 0) ivsOnAlign = ivSeq
    endif
    if (itemOnList(iview, ivSnapList, nSnapList)) then
      call int_iwrite(listString, iview, ndel)
      write(pieceFile, '(a,a,a,i1)') '.', listString(1:ndel), '.', ipass
      call scale_model(1)
      call write_wmod(concat(modelFile, pieceFile))
      call scale_model(0)
    endif
    numAddTmp = numAdded
    numAdded = 0
    numToDo = 0
    ndel = 0
    if (numAddTmp > 0) then
      numToDo = 0
      do iobjDo = 1, numObjDo
        if (ifFound(iobjDo) == 0) numToDo = numToDo + 1
        if (ifFound(iobjDo) == 1) then
          iobj = iobjSeq(iobjDo)
          errMax = 0.
          ! if (ipass == 1) then
          indr = 0
          !
          ! find point in the tiltalign, get its residual
          !
          if (ifDidAlign == 1) then
            do ipt = 1, nrealpt
              if (iobj == iobjAli(ipt)) indr = ipt
            enddo
          endif
          if (indr .ne. 0) then
            ivLook = mapFileToView(iview)
            ipt = irealStr(indr)
            do while(ipt < irealStr(indr + 1) .and. errMax == 0.)
              if (isecView(ipt) == ivLook) errMax = scaleXY * &
                  sqrt(xresid(ipt)**2 + yresid(ipt)**2)
              ipt = ipt + 1
            enddo
          endif
          !
          ! When no tiltalign available, just redo the point with
          ! highest deviation?  No, projections could be lousy.
          !
          ! if (ifDidAlign == 0 .and. numData >= limPtsShift) then
          ! do i = 1, numData
          ! if (nint(xr(6, i)) == iobjDo .and. xr(13, i) > &
          ! 0.99 * devMax) errMax = xr(13, i)
          ! enddo
          ! endif
          !
          ! endif

          ! Look at nearby views within a range of angle differences 
          relaxAllCrit = 1.
          isCur = 0
          if (wsumSave(iview, iobj) >= wsumCrit(iobjDo) .and. aloneRelaxCrit > 1 &
              .and. ipass > 1) then
            ALONE_DIR_LOOP: do idirw = -1, 1, 2
              ALONE_DIFF_LOOP: do ix = 1, nviewAll
                idif = ix * idirw
                if (iview + idif > 0 .and. iview + idif <= nviewAll) then
                  curDiff = abs(tiltAll(iview) - tiltAll(iview + idif))
                  if (curDiff > aloneMaxAngleDiff) then
                    exit ALONE_DIFF_LOOP
                  endif
                  if (curDiff >= aloneMinAngleDiff) then
                    if (isIsolated(iview + idif, iobj) > 0) then
                      relaxAllCrit = aloneRelaxCrit
                      isCur = isCur + 1
                    endif
                    if (isIsolated(iview + idif, iobj) == 0) then
                      relaxAllCrit = 1.001
                      exit ALONE_DIR_LOOP
                    endif
                  endif
                endif
              enddo ALONE_DIFF_LOOP
            enddo ALONE_DIR_LOOP
          endif
          ! if (ipass == 1)print *,iobj,wsumSave(iview, iobj) >= wsumCrit(iobjDo),isCur,relaxAllCrit
          ifMeanBad = 0
          !
          ! see if mean residual has gotten a lot bigger
          !
          if (ifDidAlign == 1) then
            isCur = ivSeq - 1
            nprev = 0
            do while(nprev <= maxResid .and. isCur > 0)
              if (resMean(iobj, isCur) > 0.) then
                nprev = nprev + 1
                prevRes(nprev) = resMean(iobj, isCur )
              endif
              isCur = isCur - 1
            enddo
            if (nprev > minResid) then
              curDiff = resMean(iobj, ivSeq) - prevRes(1)
              do i = 1, nprev - 1
                prevRes(i) = prevRes(i) - prevRes(i + 1)
              enddo
              call avgsd(prevRes, nprev - 1, resAvg, resSD, resSEM)
              if (curDiff > relaxAllCrit * resDiffMin .and. &
                  (curDiff - resAvg) / resSD > relaxAllCrit * resDiffCrit .and.  &
                  errMax > relaxAllCrit * curResMin) &
                  ifMeanBad = 1
              if (ifMeanBad == 0 .and. curDiff > resDiffMin .and. &
                  (curDiff - resAvg) / resSD > resDiffCrit .and.  &
                  errMax > curResMin) print *,'NOT MEAN BAD FOR ALONE',iobj
              if (ifMeanBad) print *,'mean bad',iobj,curDiff,(curDiff - resAvg) / resSD, &
                  errMax, curResMin
            endif
          endif
          !
          ! if error greater than criterion after pass 1, or mean
          ! residual has zoomed on either pass, delete point for
          ! next round
          !
          if (ifMeanBad == 0 .and. ipass == 1 .and. errMax > fitDistCrit .and. &
              errMax < relaxAllCrit * fitDistCrit)  &
              print *,'RELAX FOR ALONE', iobj,errMax,fitDistCrit
          if ((ipass == 1 .and. errMax > fitDistCrit) .or. &
              ifMeanBad == 1) then
            if (ipass == 1 .and. ifMeanBad == 0) print *,'big res', errMax
            if (saveAllPoints)write(*,'(i3,f7.2,i2,4f10.5)') iobj, errMax, ifMeanBad, &
                resMean(iobj, ivSeq), curDiff, resAvg, resSD
            wsumSave(iview, iobj) = -1.
            if (getEdgeSD) edgeSdsave(iview, iobj) = -1.
            resMean(iobj, ivSeq) = -1.
            ibase = ibase_obj(iobj)
            do ip = ibase + ipNearest(iobjDo) + 1, &
                ibase + npt_in_obj(iobj)
              object(ip - 1) = object(ip)
            enddo
            npt_in_obj(iobj) = npt_in_obj(iobj) - 1
            ifFound(iobjDo) = 0
            numToDo = numToDo + 1
            ipNearest(iobjDo) = ipNearSave(iobjDo)
            ndel = ndel + 1
            iobjDel(ndel) = iobj
          endif
        endif
      enddo
      numAdded = ndel
      if (ndel .ne. 0) then
        if (ipass == 1) then
          write(*,delfmt1) ndel, (iobjDel(i), i = 1, ndel)
        else
          write(*,delfmt2) ndel, (iobjDel(i), i = 1, ndel)
          numAdded = 0
          call tiltAli(ifDidAlign, ifAlignDone, resMean(1, ivSeq),  iview)
          if (ifDidAlign .ne. 0) ivsOnAlign = ivSeq
        endif
      endif
    endif
    return
  end subroutine redoFitsEvaluateResiduals


  ! Add a sequence from ivStart to ivEnd to the sequence list
  !
  subroutine addSequence(ind, ivStart, ivEnd)
    integer*4 ind, ivStart, ivEnd
    numSeqs = numSeqs + 1
    ivSeqStr(numSeqs) = ivStart
    ivSeqEnd(numSeqs) = ivEnd
    listSeq(numSeqs) = ind
    return
  end subroutine addSequence

end program beadtrack

subroutine errorExit(message, ifLocal)
  implicit none
  integer*4 ifLocal
  character*(*) message
  write(*,'(/,a,a)') 'ERROR: BEADTRACK - ', message
  call exit(1)
end subroutine errorExit

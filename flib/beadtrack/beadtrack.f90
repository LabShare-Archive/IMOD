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
  real*4, allocatable :: array(:), BRRAY(:), sarray(:), sbrray(:)
  real*4, allocatable :: refSobel(:), tmpSobel(:), boxSobel(:), sobelWsums(:,:)
  real*4, allocatable :: sobelXpeaks(:,:), sobelYpeaks(:,:), sobelPeaks(:,:)
  ! maxAllReal
  real*4, allocatable :: seqDist(:), xxtmp(:), yytmp(:), xvtmp(:), yvtmp(:)
  integer*4, allocatable :: indgap(:), numBotTop(:,:)
  logical*1, allocatable :: inAnArea(:)
  !
  integer*4, allocatable :: ixPclist(:), iyPclist(:), izPclist(:), listz(:), inCore(:,:)
  CHARACTER*320 filin, outFile, pieceFile, modelFile, surfaceFile
  ! maxView except wsumSave
  integer*4, allocatable :: izExclude(:), ipClose(:), izClose(:)
  integer*4, allocatable :: ivList(:), ivSnapList(:)
  logical, allocatable :: missing(:)
  real*4, allocatable :: wsumSave(:,:), prevRes(:)
  character*6 areaObjStr
  character*9 date
  character*8 tim
  logical exist, readSmallMod
  !
  ! maxreal
  real*4, allocatable :: xseek(:), yseek(:), wsumCrit(:), xr(:,:)
  integer*4, allocatable :: numWsums(:), ifFound(:), ipNearest(:), ipNearSave(:)
  integer*4, allocatable :: iobjDel(:), idrop(:), igrpBotTop(:), numInSobelSum(:)
  logical, allocatable :: inCorrSum(:,:), inSobelSum(:,:)

  real*4, allocatable :: resMean(:,:)
  ! maxArea
  integer*4, allocatable :: iareaSeq(:), ninObjList(:), indObjList(:)
  real*4, allocatable :: areaDist(:)
  integer*4, allocatable :: ivSeqStr(:), ivSeqEnd(:), listSeq(:)
  !
  integer*2, allocatable :: ivGap(:)
  ! maxOlist
  real*4, allocatable :: residLists(:)
  integer*4, allocatable :: iobjLists(:), iobjLisTmp(:)
  integer*1, allocatable :: listsBotTop(:)
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
  integer*4 nxTotPix, nyTotPix, numExclude, ig, iaxTilt, nxLocal, nyLocal
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
  real*4 peak, dist, tmin, tmax, tmean, cvbxcen, cvbycen, area, density
  integer*4 numVert, minInArea, minBeadOverlap, ifLocalArea, localTarget
  integer*4 nvLocalIn, localViewPass, nSnapList, iobjSave
  logical keepGoing, saveAllPoints, ignoreObjs, done
  integer*4 numNew, nOverlap, iseqPass, ipassSave, iAreaSave, maxObjOrig
  real*4 outlieElimMin, outlieCrit, outlieCritAbs, curResMin, tiltMax, minPeakRatio
  real*4 sigma1, sigma2, radius2, radius1, deltaCtf, ranFrac, diameter, sobelSigma
  integer*4 maxDrop, numDrop, ndatFit, numAreaTot, maxInArea, limInArea, interpType
  integer*4 minViewDo, maxViewDo, numViewDo, numBound, iseed, limcxBound, nFarther
  integer*4 ivsOnAlign, ifAlignDone, imageBinned, maxSobelSum, maxAnySum
  integer*4 nxSobel, nySobel, nxsPad, nysPad, ninSobel, kernelDim
  real*4 xOffSobel, yOffSobel, scaleFacSobel, scaleByInterp, targetSobel
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
  modeBox = 0
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
  maxSobelSum = 50
  scaleByInterp = 1.2
  targetSobel = 8
  sobelSigma = 0.5
  interpType = 0
  maxPeaks = 20
  minPeakRatio = 0.2
  msizeXR = 19
  taperFrac = 0.2
  edgeMedian = .false.
  !
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
      listz(limPcList), stat = ierr)
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
      numBotTop(2, maxAllReal), iobjSeq(maxAllReal), xyzSave(3, maxAllReal), &
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
  if (PipGetInOutFile('OutputModel', 2, 'Output model file', modelFile) &
      .ne. 0) call errorExit('NO OUTPUT MODEL FILE SPECIFIED', 0)
  !
  outFile = ' '
  surfaceFile = ' '
  numExclude = 0
  if (pipinput) then
    ierr = PipGetString('BoxOutputFile', outFile)
    ierr = PipGetString('SurfaceOutputFile', surfaceFile)
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
  iaxTilt = 2
  if (abs(rotStart) >= 45.) iaxTilt = 1

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
  tiltFitMin = 15.
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
    ierr = PipGetTwoFloats('RescueRelaxationDensityAndDistance', &
        relaxInt, relaxDist)
    ierr = PipGetFloat('PostFitRescueResidual', fitDistCrit )
    ierr = PipGetFloat('DensityRelaxationPostFit', relaxFit)
    ierr = PipGetFloat('MaxRescueDistance', radMaxFit)
    ierr = PipGetTwoIntegers('ResidualsToAnalyzeMaxAndMin', &
        maxResid, minResid)
    ierr = PipGetTwoFloats('DeletionCriterionMinAndSD', resDiffMin, &
        resDiffCrit)
    ierr = PipGetLogical('TrackObjectsTogether', ignoreObjs)
    ierr = PipGetInteger('AverageBeadsForSobel', maxSobelSum)
    i = 0
    ierr = PipGetBoolean('SobelFilterCentering', i)
    if (i == 0) maxSobelSum = 0
    ierr = PipGetFloat('KernelSigma', sobelSigma)
    ierr = PipGetLogical('MedianForCentroid', edgeMedian)
    if (sobelSigma > 1.49) interpType = 1
    ierr = PipGetInteger('InterpolationType', interpType)
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
  nxTaper = max(8, nint(taperFrac * nxBox))
  nyTaper = max(8, nint(taperFrac * nyBox))
  nxBox = nxBox + nxTaper / 2
  nyBox = nyBox + nyTaper / 2
  nxpad = niceFrame(nxBox + 2 * npad, 2, 19)
  nypad = niceFrame(nyBox + 2 * npad, 2, 19)
  npixBox = nxBox * nyBox
  nxpDim = nxpad + 2
  maxarr = nypad * nxpDim
  ! if (npixBox > maxbox**2 .or. nypad * nxpDim > maxarr) call &
  ! errorExit('BOX SIZE TOO LARGE FOR ARRAYS - TRY BINNING INPUT DATA', 0)
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
    deallocate(xxtmp, yytmp)
    allocate(iareaSeq(maxArea), ninObjList(maxArea), indObjList(maxArea), &
        areaDist(maxArea), ivSeqStr(ix), ivSeqEnd(ix), listSeq(ix), xxtmp(2 * maxArea), &
        yytmp(3 * maxarea), stat = ierr)
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
    if (ifLocalArea .ne. 0 .and. maxInArea > limInArea .and. &
        localTarget > 100) then
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
119   format('Area',i3,', X:',i6,' to',i6,', Y:',i6,' to',i6,',', &
          i4,' points,',i4,' new')
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
  allocate(listsBotTop(maxOlist), residLists(maxOlist), iobjLists(maxOlist), &
      inCore(maxAnySum, maxInArea), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR OBJECT LISTS')
  iobjLists(1:indFree-1) = iobjLisTmp(1:indFree-1)
  deallocate(iobjLisTmp, xxtmp, yytmp, xvtmp, yvtmp, seqDist)
  !
  ! Allocate boxes and other image arrays
  allocate(boxes(npixBox, maxAnySum, maxInArea), corrSum(npixBox, maxInArea), &
      curSum(npixBox), boxTmp(maxarr), &
      array(maxarr), BRRAY(maxarr), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR BOXES OF IMAGE DATA')
  !
  ! Allocate arrays for all real points in current solution
  i = maxInArea + 10
  allocate(xseek(i), yseek(i), wsumCrit(i), xr(msizeXR, i), numWsums(i), ifFound(i), &
      ipNearest(i), ipNearSave(i), iobjDel(i), idrop(i), igrpBotTop(i), &
      wsumSave(maxView, i), iobjAli(i), numInSobelSum(i), inCorrSum(maxAnySum, i), &
      inSobelSum(maxAnySum, i), sobelXpeaks(maxPeaks, i), sobelYpeaks(maxPeaks, i), &
      sobelPeaks(maxPeaks, i), sobelWsums(maxPeaks, i), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR POINTS IN AREA')
  xr(3, 1:i) = 0.
  sobelXpeaks = 0.
  sobelYpeaks = 0.
  !
  ! Get size and offset of sobel filtered
  if (maxSobelSum > 0) then
    scaleFacSobel = diameter / targetSobel
    print *,scaleFacSobel, diameter, targetSobel, scaleByInterp
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
      do i = 1, nobjLists
        numSeqs = numSeqs + 1
        ivSeqStr(numSeqs) = minTiltInd - 1
        ivSeqEnd(numSeqs) = minViewDo
        listSeq(numSeqs) = i
        numSeqs = numSeqs + 1
        ivSeqStr(numSeqs) = minTiltInd
        ivSeqEnd(numSeqs) = maxViewDo
        listSeq(numSeqs) = i
      enddo
    else
      do i = 1, nobjLists
        numSeqs = numSeqs + 1
        ivSeqEnd(numSeqs) = minTiltInd - 1
        ivSeqStr(numSeqs) = minViewDo
        listSeq(numSeqs) = i
        numSeqs = numSeqs + 1
        ivSeqEnd(numSeqs) = minTiltInd
        ivSeqStr(numSeqs) = maxViewDo
        listSeq(numSeqs) = i
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
  do i = 1, indFree
    listsBotTop(i) = 0
  enddo
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
    if (iseqPass >= localViewPass) nviewLocal = nvLocalIn
    saveAllPoints = iAreaSave == listSeq(iseq) .and. ipassSave == iseqPass
    if (listSeq(iseq) .ne. lastSeq) then
      !
      ! initialize if doing a new set of points
      !
      initXyzDone = 0
      ivSeq = 1
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
      wsumSave(1:nviewAll, 1:numObjDo) = -1.
      resMean = -1.
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
    if (mod(iseq, 2) == 0) then
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
    ! Do surface fitting on every round if tiltalign run
    if (surfaceFile .ne. ' ' .and. ifAlignDone .ne. 0) then
      if (surfaceSort(xyz, nrealpt, igrpBotTop) .ne. 0) call errorExit( &
          'ALLOCATING MEMORY in surfaceSort', 0)
      !
      ! Find each object in the real object list and move group number
      ! into list
      do i = 1, numObjDo
        j = 1
        ix = 0
        xpos = 0.
        do while (j <= nrealpt .and. ix == 0)
          if (iobjAli(j) == iobjSeq(i)) then
            ix = igrpBotTop(j)
            xpos = resMean(iobjAli(j), ivsOnAlign)
          endif
          j = j + 1
        enddo
        listsBotTop(i + indObjList(lastSeq) - 1) = ix
        residLists(i + indObjList(lastSeq) - 1) = xpos
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

    call countMissing(iobj, nviewAll, izExclude, numExclude, missing, &
        listz, numListZ)
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
  ! Write out surface file
  if (surfaceFile .ne. ' ') then
    call dopen(4, surfaceFile, 'new', 'f')
    do iobj = 1, maxObjOrig
      numBotTop(1, iobj) = 0
      numBotTop(2, iobj) = 0
      resMean(iobj, 1) = 0.
    enddo
    do i = 1, indFree
      iobj = iobjLists(i)
      j = listsBotTop(i)
      if (j > 0) then
        numBotTop(j, iobj) = numBotTop(j, iobj) + 1
        resMean(iobj, 1) = resMean(iobj, 1) + residLists(i)
      endif
    enddo
    do iobj = 1, maxObjOrig
      call objToCont(iobj, obj_color, imodObj, imodCont)
      xpos = 0.
      ix = numBotTop(1, iobj) + numBotTop(2, iobj)
      if (ix > 0) xpos = resMean(iobj, 1) / ix
      write(4, '(i3,3i6,f8.3)') imodObj, imodCont, (numBotTop(j, iobj), j = 1, 2), &
          xpos
    enddo
  endif
  !
  ! convert index coordinates back to model coordinates
  !
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
    integer*4 numberInList
    !
    ! first see how many are done on this view
    !
    numToDo = 0
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
          call find_piece(ixPclist, iyPclist, izPclist, npclist, nx, ny, nxBox, nyBox, &
              xbox, ybox, izBox, ix0, ix1, iy0, iy1, ipcz)
          if (ipcz >= 0) then
            if (abs(izv - iview) <= maxgap) nClose = nClose + 1
            nFarther = nFarther + 1
            ipClose(nFarther) = ip
            izClose(nFarther) = izv
          endif
        endif
      enddo
      !
      ! if not found and none close, mark as - 1 not to do
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
          ! now load ones that are needed into last free box
          !
          do ic = 1, maxBoxUse
            if (numberInList(izClose(ic), inCore(1, iobjDo), maxAnySum, 0) == 0) then
              do ibox = 1, maxAnySum
                if (inCore(ibox, iobjDo) < 0) ib = ibox
              enddo
              inCore(ib, iobjDo) = izClose(ic)
              ipt = abs(object(ibase + ipClose(ic)))
              xbox = p_coord(1, ipt)
              ybox = p_coord(2, ipt)
              izBox = nint(p_coord(3, ipt))
              call find_piece(ixPclist, iyPclist, izPclist, npclist, nx, ny, nxBox, &
                  nyBox, xbox, ybox, izBox, ix0, ix1, iy0, iy1, ipcz)
              call imposn(1, ipcz, 0)
              call irdpas(1, boxTmp, nxBox, nyBox, ix0, ix1, iy0, iy1,*299)
              !
              ! do subpixel shift and calculate CG and wsum value
              ! if it is not already saved
              !
              xt = nint(xbox) - xbox
              yt = nint(ybox) - ybox
              call qdshift(boxTmp, boxes(1, ib, iobjDo) , nxBox, nyBox, xt, yt)
              if (wsumSave(izBox + 1, iobjDo) < 0.) then
                xtmp = 0.
                ytmp = 0.
                call calcCG(boxes(1, ib, iobjDo), nxBox, nyBox, xtmp, ytmp,  wsum)
                wsumSave(izBox + 1, iobjDo) = wsum
              endif
              !
              ! Add to correlation sum
              if (ic <= maxSum) then
                inCorrSum(ib, iobjDo) = .true.
                corrSum(1:npixBox, iobjDo) = corrSum(1:npixBox, iobjDo) + &
                    boxes(1:npixBox, ib, iobjDo)
              endif
              !
              ! Add to sobel sum
              if (ic <= maxSobelSum) then
                inSobelSum(ib, iobjDo) = .true.
                numInSobelSum(iobjDo) = numInSobelSum(iobjDo) + 1
                sobelSum(1:npixBox, iobjDo) = sobelSum(1:npixBox, iobjDo) + &
                    boxes(1:npixBox, ib, iobjDo)
              endif
            endif
          enddo
        endif
      endif
      ipNearSave(iobjDo) = ipNearest(iobjDo)
    enddo
    return
299 call errorExit('ERROR READING IMAGE FILE', 0)
  end subroutine countAndPreparePointsToDo


  ! getProjectedPositionsSetupFits
  !
  subroutine getProjectedPositionsSetupFits()
    real*4 cosd, sind
    !
    ! get tentative tilt, rotation, mag for current view
    !
    if (ifDidAlign > 0) then
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
      ! print *,'ivuse, iview', ivuse, iview
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
    ! now get projected positions
    !
    do iobjDo = 1, numObjDo
      indr = 0
      if (ifDidAlign == 1) then
        do ipt = 1, nrealpt
          if (iobjSeq(iobjDo) == iobjAli(ipt)) indr = iobjSeq(iobjDo)
        enddo
      endif
      if (indr .ne. 0) then
        !
        ! there is a 3D point for it: so project it
        !
        xseek(iobjDo) = a * xyzSave(1, indr) + b * xyzSave(2, indr) +  &
            c * xyzSave(3, indr) + dxCur + xcen
        yseek(iobjDo) = d * xyzSave(1, indr) + e * xyzSave(2, indr) +  &
            f * xyzSave(3, indr) + dyCur + ycen
      else
        call nextPos(iobjSeq(iobjDo), ipNearest(iobjDo), idir, iznext, tiltAll, numFit, &
            minFit, iaxTilt, tiltFitMin, xseek(iobjDo), yseek(iobjDo))
      endif
      if (saveAllPoints) then
        iobjSave = iobjSeq(iobjDo) + (5 * ipass - 4) * maxObjOrig
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


  ! findAllBeadsOnView
  !
  subroutine findAllBeadsOnView()
    do iobjDo = 1, numObjDo
      if (ifFound(iobjDo) == 0 .or. ifFound(iobjDo) == 1) then
        !
        ! get w criterion: average it over as many near views as
        ! possible.  maxWavg specifies both the maximum distance
        ! away in Z and the maximum number to average
        !
        numAvg = 0
        wsum = 0.
        wsumsq = 0.
        idif = 1
        do while(idif <= maxWavg .and. numAvg < maxWavg)
          do idirw = -1, 1, 2
            itry = iview + idirw * idif
            if (itry > 0 .and. itry <= nviewAll) &
                then
              wstmp = wsumSave(itry, iobjDo)
              if (wstmp >= 0) then
                wsum = wsum + wstmp
                wsumsq = wsumsq + wstmp**2
                numAvg = numAvg + 1
              endif
            endif
          enddo
          idif = idif + 1
        enddo
        call sums_to_avgsd(wsum, wsumsq, numAvg, wsumAvg, wsumSD)
        wsumCrit(iobjDo) = min(fracCrit * wsumAvg, wsumAvg - sdCrit * wsumSD)
        numWsums(iobjDo) = numAvg
      endif
      !
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
        call find_piece(ixPclist, iyPclist, izPclist, npclist, nx, ny, nxBox, nyBox, &
            xnext, ynext, iznext, ix0, ix1, iy0, iy1, ipcz)
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
    !
    ! get image area
    !
    call imposn(1, ipcz, 0)
    call irdpas(1, boxTmp, nxBox, nyBox, ix0, ix1, iy0, iy1,*199)
    if (ipass == 1) then
      !
      ! pad image into array on first pass, padd correlation sum into brray
      call taperInPad(boxTmp, nxBox, nyBox, array, nxpDim, nxpad, nypad, nxTaper, nyTaper)
      call setMeanZero(array, nxpDim, nypad, nxpad, nypad)
      call taperInPad(corrSum(1, iobjDo), nxBox, nyBox, brray, nxpDim, nxpad, nypad, &
          nxTaper, nyTaper)
      call setMeanZero(brray, nxpDim, nypad, nxpad, nypad)
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
      call peakFind(array, nxpDim, nypad, xpeak, ypeak, peak)
      if (saveAllPoints) then
        iobjSave = iobj + (5 * ipass - 2) * maxObjOrig
        call add_point(iobjSave, 0, nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
      endif
      call calcCG(boxTmp, nxBox, nyBox, xpeak, ypeak, wsum)
      !
      ! Get sobel sum with enough beads in it
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
        call setMeanZero(sarray, nxsPad + 2, nysPad, nxsPad, nysPad)
        call taperInPad(refSobel, nxSobel, nySobel, sbrray, nxsPad + 2, nxsPad, nysPad, &
            nxTaper, nyTaper)
        call setMeanZero(sbrray, nxsPad + 2, nysPad, nxsPad, nysPad)
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
        call xcorrPeakFind(sarray, nxsPad + 2, nysPad, sobelXpeaks(1, iobjDo), &
            sobelYpeaks(1, iobjDo), sobelPeaks(1, iobjDo), maxPeaks)
        call calcCG(curSum, nxBox, nyBox, xOffSobel, yOffSobel, xtmp)
        ! write(*,'(2i5,a,2f7.2)') iobjDo, ninSobel, '  Sobel average offset', xOffSobel, yOffSobel
        sobelWsums(1:maxPeaks, iobjDo) = -1.
        sobelXpeaks(1:maxPeaks, iobjDo) = sobelXpeaks(1:maxPeaks, iobjDo) * &
            scaleFacSobel + xOffSobel
        sobelYpeaks(1:maxPeaks, iobjDo) = sobelYpeaks(1:maxPeaks, iobjDo) * &
            scaleFacSobel + yOffSobel
        ! do i = 1, maxPeaks
        ! if (sobelPeaks(i, iobjDo) > - 1.e29) &
        ! write(*,'(2f7.2,f13.2)') sobelXpeaks(i, iobjDo), sobelYpeaks(i, iobjDo), sobelPeaks(i, iobjDo)
        ! enddo

        ! Revise position of current peak
        call findNearestSobelPeak()
      endif

      dist = sqrt(xpeak**2 + ypeak**2)
      if (saveAllPoints) then
        iobjSave = iobj + (5 * ipass - 1) * maxObjOrig
        call add_point(iobjSave, 0, nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
      endif
    endif
    if (numWsums(iobjDo) > 2 .and. (wsum < wsumCrit(iobjDo) &
        .or. dist > distCrit .or. ipass == 2)) then
      !
      ! rescue attempt; search concentric rings from the
      ! center of box and take the first point that goes
      ! above the relaxed wsum criterion
      !
      if (ipass == 1) then
        if (dist > distCrit) then
          relax = relaxDist
          if (ifTrace .ne. 0) &
              write(*,102) 'distance', iznext, dist, wsum, wsumAvg, wsumSD
102       format(' Rescue-',a,', sec',i4,', dist=',f5.1, &
              ', dens=',f9.0,', mean,sd=',f9.0,f7.0)
        else
          relax = relaxInt
          if (ifTrace .ne. 0) &
              write(*,102) 'density ', iznext, dist, wsum, wsumAvg, wsumSD
        endif
        radMax = max(nxBox, nyBox)
      else
        relax = relaxFit
        radMax = radMaxFit
      endif
      ! if (maxSobelSum > 0) then
      ! call rescueFromSobel(boxTmp, nxBox, nyBox, xpeak, ypeak, sobelXpeaks(1, iobjDo), &
      ! sobelYpeaks(1, iobjDo), sobelPeaks(1, iobjDo), sobelWsums(1, iobjDo), &
      ! maxPeaks, radMax, relax * wsumCrit(iobjDo), wsum)
      ! else
      call rescue(boxTmp, nxBox, nyBox, xpeak, ypeak, radMax, relax * wsumCrit(iobjDo), &
          wsum)
      if (maxSobelSum > 0) call findNearestSobelPeak()
      ! endif
    elseif (ipass == 2) then
      wsum = 0.
    endif
    !
    !
    !
    if (wsum .ne. 0.) then
      wsumSave(iview, iobjDo) = wsum
      xpos = nint(xnext) + xpeak
      ypos = nint(ynext) + ypeak
      if (ifTrace .ne. 0) &
          write(*,'(3i5,4f7.1,2f12.0)') nzout, iznext, iobj, &
          xnext, ynext, xpos, ypos, peak, wsum
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
199 call errorExit('ERROR READING IMAGE FILE', 0)
  end subroutine lookForOneBead


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
      call checkSobelPeak(i, boxTmp, nxBox, nyBox, sobelXpeaks(1, iobjDo), &
          sobelYpeaks(1, iobjDo), sobelPeaks(1, iobjDo), sobelWsums(1, iobjDo), &
          maxPeaks)
      ! if (sobelPeaks(i, iobjDo) > - 1.e29) &
      ! write(*,'(2f7.2,2f13.2)') sobelXpeaks(i, iobjDo), sobelYpeaks(i, iobjDo), sobelPeaks(i, iobjDo), sobelWsums(1, iobjDo)

      if (sobelWsums(i, iobjDo) > 0) then
        if (peakMax < - 1.e29) peakMax = sobelPeaks(i, iobjDo)
        if (sobelPeaks(i, iobjDo) < minPeakRatio * peakMax) then
          exit
        endif
        dist = (sobelXpeaks(i, iobjDo) - xpcen)**2 + (sobelYpeaks(i, iobjDo) - ypcen)**2
        if (dist < distMin) then
          distMin = dist
          ibest = i
        endif
      endif
    enddo
    if (ibest == 0) return
    xpeak = sobelXpeaks(ibest, iobjDo)
    ypeak = sobelYpeaks(ibest, iobjDo)
    ! wsum = sobelWsums(ibest, iobjDo)
    ! print *,ibest, xpeak, ypeak, wsum
    return
  end subroutine findNearestSobelPeak


  ! redoFitsEvaluateResiduals
  !
  subroutine redoFitsEvaluateResiduals()
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
              if (curDiff > resDiffMin .and. (curDiff - resAvg) / &
                  resSD > resDiffCrit .and. errMax > curResMin) &
                  ifMeanBad = 1
            endif
          endif
          !
          ! if error greater than criterion after pass 1, or mean
          ! residual has zoomed on either pass, delete point for
          ! next round
          !
          if ((ipass == 1 .and. errMax > fitDistCrit) &
              .or. ifMeanBad == 1) then
            ! write(*,'(i3,f7.2,4f10.5)') iobj, errMax, &
            ! resMean(iobj, ivSeq), curDiff, resAvg, resSD
            wsumSave(iview, iobjDo) = -1.
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

end program beadtrack

subroutine errorExit(message, ifLocal)
  implicit none
  integer*4 ifLocal
  character*(*) message
  write(*,'(/,a,a)') 'ERROR: BEADTRACK - ', message
  call exit(1)
end subroutine errorExit

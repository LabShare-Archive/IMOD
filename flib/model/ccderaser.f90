!***    CCDERASER.for
!
! This program replaces deviant pixels with interpolated values from
! surrounding pixels.  It is designed to correct defects in electron
! microscope images from CCD cameras.  It can use two algorithms to
! automatically remove peaks in intensity caused by X-rays.  It can
! also take an IMOD model file with specifications of regions to be
! replaced.  With a model, the program can replace a group of adjacent
! pixels with interpolated values, or all of the pixels along a line.
! It can do this on only a specific image, or on all of the sections
! in the file.  The program can operate in trial mode, without making
! an output file, and it can output a model file with points at the
! pixels to be replaced.
!
! See the man page for a description of the operation and inputs.
!
! David Mastronarde 11/10/98
!
! $Id$
!
! A module for array limits, entered parameters, some 1-D arrays, and a few other items
! to reduce the number of arguments in the big function calls
!
module ccdvars
  implicit none
  integer MAXDEV, LIMPATCH, LIMPTOUT, LIMDIFF, LIMPATCHOUT
  parameter (LIMDIFF = 512, LIMPATCHOUT = 40000)
  parameter (MAXDEV = 300, LIMPATCH = 10000, LIMPTOUT = 25 * LIMPATCHOUT)
  integer*4 numPixBorder, iorder, ifIncludeAdj, iScanSize, numEdgePixels, ifVerbose
  integer*4 matSize, maxInDiffPatch
  integer*4 nxyz(3), nx, ny, nz
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  real *4 scanOverlap, critScan, critGrow, critBigDiff, fracBigDiff
  real*4 critMain, critDiff, radiusMax, outerRadius, critGiant, giantRadius
  integer*4 ixFix(LIMPATCH), iyFix(LIMPATCH)
  integer*4, allocatable :: indPatch(:), ixOut(:), iyOut(:), izOut(:)
  real*4, allocatable :: exceedCrit(:)
end module ccdvars

program ccderaser
  use ccdvars
  implicit none
  include 'model.inc90'
  real*4 title(20), delta(3), origin(3)
  integer*4 mxyz(3)
  character*320 inFile, outFile, pointFile, modelOut
  integer*4, allocatable :: iobjline(:), iobjDoAll(:), iobjBound(:), iobjCircle(:)
  integer*4, allocatable :: indSize(:), ixPcList(:), iyPcList(:), izPcList(:)
  real*4, allocatable :: xbound(:), ybound(:), sizes(:), array(:), diffArr(:,:)
  real*4, allocatable :: betterRadius(:), betterIn(:)
  logical*4, allocatable :: objTaper(:)
  !
  character*80 titlech
  character dat * 9, tim * 8
  character*1024 line
  !
  integer*4 mode, imFileOut, i, j, numObjDoAll, numObjLine
  integer*4 imSize, limObj, limPcList
  integer*4 iobj, ibase, itype, imodObj, imodCont, ip, ipt, jtype, jobj
  real*4 dmin, dmax, dmean, tmin, tmax, tsum, dminTmp, dmaxTmp, dmeant, tmean
  real*4 zmin, zmax, xmin, xmax, yMin, ymax, diamMerge
  integer*4 izSect, numFix, lineFix, ip1, ip2, ix1, ix2, iy1, iy2, kti, numInObj
  integer*4 ierr, ierr2, numPtOut, numPatchOut, numObjOrig, ifMerge
  real*4 annulusWidth, radSq, sizeMax, size, xcen, ycen, dist
  integer*4 ifPeakSearch, numPatch, numPixels, ifTouch
  integer*4 ifTrialMode, maxObjectsOut, ifGrew
  integer*4 jp1, jp2, jx1, jx2, jy1, jy2, iborder, jBordLow, jBordHigh, idir, jbase
  integer*4 numObjBound, numObjCircle, numBetterIn, numCircleObj, numExpandIter
  integer*4 indFree, indCont, numSizes, loopStart, loopEnd, loop, numConSize
  integer*4 ixFixMin, ixFixMax, iyFixMin, iyFixMax, numGrowIter, taperedPatchCrit
  integer*4 maxSizes, limSizes, maxBound, numTapering, numPcList, minXpiece, numXpieces
  integer*4 nxOverlap, minYpiece, numYpieces, nyOverlap, newXoverlap, newYoverlap
  integer*4 indx, indy, indz, ipcx, ipcy, ipcz
  logical*4 circleCont, allSecCont, contOnSecOrAllSec
  logical readw_or_imod, typeOnList, nearby
  integer*4 getImodObjSize, getImodSizes
  integer*4 getContPointSizes, imodGetPID
  real*4 contourArea
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean
  integer*4 PipGetString, PipGetFloat, PipGetFloatArray, PipGetTwoIntegers
  integer*4 PipGetNonOptionArg, PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  ccderaser
  !
  integer numOptions
  parameter (numOptions = 35)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@piece:PieceListFile:FN:@'// &
      'overlaps:OverlapsForModel:IP:@find:FindPeaks:B:@peak:PeakCriterion:F:@'// &
      'diff:DiffCriterion:F:@grow:GrowCriterion:F:@scan:ScanCriterion:F:@'// &
      'radius:MaximumRadius:F:@giant:GiantCriterion:F:@large:ExtraLargeRadius:F:@'// &
      'big:BigDiffCriterion:F:@maxdiff:MaxPixelsInDiffPatch:I:@outer:OuterRadius:F:@'// &
      'width:AnnulusWidth:F:@xyscan:XYScanSize:I:@edge:EdgeExclusionWidth:I:@'// &
      'points:PointModel:FN:@model:ModelFile:FN:@lines:LineObjects:LI:@'// &
      'boundary:BoundaryObjects:LI:@allsec:AllSectionObjects:LI:@'// &
      'circle:CircleObjects:LI:@better:BetterRadius:FA:@'// &
      'expand:ExpandCircleIterations:I:@merge:MergePatches:B:@border:BorderSize:I:@'// &
      'order:PolynomialOrder:I:@exclude:ExcludeAdjacent:B:@trial:TrialMode:B:@'// &
      'verbose:verbose:B:@PID:ProcessID:B:@param:ParameterFile:PF:@help:usage:B:'
  !
  ! Set all defaults here
  !
  numObjDoAll = 1
  numObjLine = 1
  numObjBound = 1
  numObjCircle = 1
  numPixBorder = 2
  iorder = 2
  ifIncludeAdj = 1
  critMain = 10.
  critDiff = 10.
  critGrow = 4.0
  critScan = 3.0
  radiusMax = 2.1
  outerRadius = 4.1
  critGiant = 12.
  giantRadius = 8.
  critBigDiff = 19.
  fracBigDiff = 0.25
  scanOverlap = 0.1
  ifPeakSearch = 0
  iScanSize = 100
  ifVerbose = 0
  ifTrialMode = 0
  numEdgePixels = 0
  maxInDiffPatch = 2
  ifMerge = 0
  maxObjectsOut = 4
  numExpandIter = 0
  taperedPatchCrit = 1000
  limPcList = 10000
  newXoverlap = 0
  newYoverlap = 0
  call b3dDate(dat)
  call time(tim)
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'ccderaser', &
      'ERROR: CCDERASER - ', .true., 2, 1, 1, numOptArg, &
      numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0
  !
  if (PipGetInOutFile('InputFile', 1, 'Name of input file', inFile) &
      .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
  !
  outFile = ' '
  if (pipInput) then
    !
    ! Get an output file string; or if none, look on command line
    !
    ierr = PipGetBoolean('TrialMode', ifTrialMode)
    ierr = PipGetString('OutputFile', outFile)
    if (ierr > 0 .and. numNonOptArg > 1) &
        ierr = PipGetNonOptionArg(2, outFile)
  else
    print *,'Enter output file name (Return to put modified'// &
        ' sections back in input file)'
    read(*,'(a)') outFile
  endif
  !
  if (ifTrialMode .ne. 0 .or. outFile .ne. ' ') then
    call imopen(1, inFile, 'ro')
  else
    call imopen(1, inFile, 'old')
  endif

  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)

  imFileOut = 1
  if (ifTrialMode == 0 .and. outFile .ne. ' ') then
    imFileOut = 2
    call imopen(2, outFile, 'new')
    call itrhdr(2, 1)
  endif
  !
  max_mod_obj = 0
  n_point = 0
  ibase_free = 0
  numPcList = 0
  if (pipInput) then
    pointFile = ' '
    ierr = PipGetString('PieceListFile', pointFile)
    ierr = PipGetTwoIntegers('OverlapsForModel', newXoverlap, newYoverlap)
    limPcList = max(limPcList, nz + 10)
    allocate(ixPcList(limPcList), iyPcList(limPcList), izPcList(limPcList), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR PIECE COORDINATES')
    call read_piece_list2(pointFile, ixPcList, iyPcList, izPcList, numPcList, limPcList)
    if (numPcList > 0) then
      if (numPcList < nz) call exitError('NOT ENOUGH PIECE COORDINATES IN FILE')
      call checkList(ixPcList, numPcList, 1, nx, minXpiece, numXpieces, nxOverlap)
      call checkList(iyPcList, numPcList, 1, ny, minYpiece, numYpieces, nyOverlap)
      if (numXpieces < 1 .or. numYpieces < 1) call exitError( &
          'PIECE COORDINATES ARE NOT REGULARLY SPACED APART')
      call adjustPieceOverlap(ixPcList, numPcList, nx, minXpiece, nxOverlap, &
          newXoverlap)
      call adjustPieceOverlap(iyPcList, numPcList, ny, minYpiece, nyOverlap, &
          newYoverlap)
    endif

    ierr = PipGetString('ModelFile', pointFile)
  else
    write(*,'(1x,a,$)') 'Model file: '
    read(*,'(a)') pointFile
  endif
  limObj = 10
  if (.not. pipInput .or. ierr == 0) then
    if (.not.readw_or_imod(pointFile)) call exitError('READING MODEL FILE')
    call scaleModelToImage(1, 0)
    !
    ! Convert model coordinates to coordinates within each piece
    if (numPcList > 0) then
      do ipt = 1, n_point
        indx = p_coord(1, ipt)
        indy = p_coord(2, ipt)
        indz = nint(p_coord(3, ipt))
        call lookup_piece(ixPcList, iyPcList, izPcList, numPcList, nx, ny, indx, indy, &
            indz, ipcx, ipcy, ipcz)
        if (ipcx < 0) then
          write(pointFile, '(a, 3i6, a)') 'THE MODEL POINT AT', indx + 1, indy + 1,  &
              indz + 1, ' IS NOT LOCATED WITHIN A PIECE'
          call exitError(pointFile)
        endif
        p_coord(1, ipt) = ipcx + p_coord(1, ipt) - indx
        p_coord(2, ipt) = ipcy + p_coord(2, ipt) - indy
        p_coord(3, ipt) = ipcz
      enddo
    endif
    limObj = getImodObjSize() + 10
  endif
  numObjOrig = max_mod_obj

  allocate(diffArr(LIMDIFF, LIMDIFF), exceedCrit(LIMPATCHOUT), ixOut(LIMPTOUT), &
      iyOut(LIMPTOUT), izOut(LIMPTOUT), indPatch(LIMPATCHOUT), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR PATCHES')
  allocate(iobjCircle(limObj), iobjline(limObj), iobjDoAll(limObj), iobjBound(limObj), &
      betterRadius(limObj), betterIn(limObj), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR MODEL OBJECT DATA')
  do i = 1, limObj
    betterRadius(i) = -1.
  enddo
  iobjDoAll(1) = -999
  iobjline(1) = -999
  iobjBound(1) = -999
  iobjCircle(1) = -999
  !
  if (pipInput) then
    !
    ! get old parameters for model based erasing
    !
    if (PipGetString('AllSectionObjects', line) == 0) then
      call parseList(line, iobjDoAll, numObjDoAll)
    else
      numObjDoAll = 0
    endif

    if (PipGetString('LineObjects', line) == 0) then
      call parseList(line, iobjline, numObjLine)
    else
      numObjLine = 0
    endif

    if (PipGetString('CircleObjects', line) == 0) then
      call parseList(line, iobjCircle, numObjCircle)
    else
      numObjCircle = 0
    endif
    if (PipGetString('BoundaryObjects', line) == 0) then
      call parseList(line, iobjBound, numObjBound)
    else
      numObjBound = 0
    endif
    ierr2 = 0
    ierr = PipGetBoolean('ProcessID', ierr2)
    if (ierr2 .ne. 0) call pidToStderr()

    ierr = PipGetInteger('ExpandCircleIterations', numExpandIter)
    ierr = PipGetInteger('BorderSize', numPixBorder)
    ierr = PipGetInteger('PolynomialOrder', iorder)
    if (PipGetBoolean('ExcludeAdjacent', i) == 0) ifIncludeAdj = 0
    !
    ! get the new parameters for auto peak search
    !
    ierr = PipGetBoolean('FindPeaks', ifPeakSearch)
    ierr = PipGetInteger('XYScanSize', iScanSize)
    ierr = PipGetInteger('EdgeExclusionWidth', numEdgePixels)
    ierr = PipGetFloat('MaximumRadius', radiusMax)
    ierr = PipGetFloat('PeakCriterion', critMain)
    ierr = PipGetFloat('GrowCriterion', critGrow)
    ierr = PipGetFloat('ScanCriterion', critScan)
    ierr = PipGetFloat('DiffCriterion', critDiff)
    ierr = PipGetInteger('verbose', ifVerbose)
    ierr = PipGetInteger('MergePatches', ifMerge)
    ierr = PipGetInteger('MaxPixelsInDiffPatch', maxInDiffPatch)
    ierr = PipGetFloat('OuterRadius', outerRadius)
    ierr2 = PipGetFloat('AnnulusWidth', annulusWidth)
    if (ierr == 0 .and. ierr2 == 0) call exitError( &
        'YOU CANNOT ENTER BOTH -outer AND -width')
    if (ierr2 == 0) outerRadius = radiusMax + annulusWidth
    ierr = PipGetFloat('ExtraLargeRadius', giantRadius)
    ierr = PipGetFloat('GiantCriterion', critGiant)
    ierr = PipGetFloat('BigDiffCriterion', critBigDiff)

    modelOut = ' '
    ierr = PipGetString('PointModel', modelOut)
    numBetterIn = 0
    ierr = PipGetFloatArray('BetterRadius', betterIn, numBetterIn, limObj)
    if (numObjCircle > 0 .and. iobjCircle(1) .ne. -999) then
      if (numBetterIn > 1 .and. numBetterIn .ne. numObjCircle) call &
          exitError('THE NUMBER OF BETTER RADIUS VALUES MUST BE EITHER '// &
          '1 OR THE SAME AS THE NUMBER OF CIRCLE OBJECTS')
      do i = 1, numObjCircle
        if (iobjCircle(i) > 0 .and. iobjCircle(i) <= limObj) &
            betterRadius(iobjCircle(i)) = betterIn(min(i, numBetterIn))
      enddo
    endif
  else
    !
    ! interactive input for old parameters only
    !
    print *,'Enter list of objects specifying replacement on ALL'// &
        ' sections (/ for all, Return for none)'
    !
    call rdlist(5, iobjDoAll, numObjDoAll)
    !
    print *,'Enter list of objects specifying horizontal or vertical'// &
        ' line replacements', ' (/ for all, Return for none)'
    !
    call rdlist(5, iobjline, numObjLine)
    !
    write(*,'(1x,a,i2,a,$)') 'border size [/ for', numPixBorder, ']: '
    read(*,*) numPixBorder
    !
    write(*,'(1x,a,i2,a,$)') 'order of 2-D polynomial fit [/ for' &
        , iorder, ']: '
    read(*,*) iorder
    !
    write(*,'(1x,a,$)') &
        '0 to exclude or 1 to include adjacent points in fit: '
    read(5,*) ifIncludeAdj
  endif
  call PipDone()

  if (max_mod_obj == 0 .and. ifPeakSearch == 0) call exitError &
      ('NO MODEL POINTS AND NO PEAK SEARCH SPECIFIED')
  !
  ! check input values, set reasonable limits
  !
  numPixBorder = min(5, max(1, numPixBorder))
  iorder = min(3, max(0, iorder))
  matSize = 2 + (iorder * (iorder + 3)) / 2
  iScanSize = min(LIMDIFF - 10, max(20, iScanSize))
  radiusMax = min(10., max(0.5, radiusMax))
  outerRadius = min(radiusMax + 10., max(radiusMax + 0.75, outerRadius))
  if (critMain == 0) critMain = 1000.
  critMain = max(2., critMain)
  if (critDiff == 0) critDiff = 1000.
  critDiff = max(2., critDiff)
  critScan = max(2., critScan)
  critGrow = max(2., critGrow)
  numEdgePixels = max(0, min(nx / 3, numEdgePixels))

  if (ifPeakSearch > 0) indPatch(1) = 1

  print *
  tmin = 1.e10
  tmax = -1.e10
  tsum = 0.
  !
  ! Check that lists are mutually exclusive
  numCircleObj = 0
  do itype = 1, getImodObjSize()
    ibase = 0
    if (typeOnList(itype, iobjCircle, numObjCircle)) then
      ibase = 1
      if (typeOnList(itype, iobjDoAll, numObjDoAll)) then
        write(*,106) itype, 'is on both the circle and the all-section list'
106     format('ERROR: CCDERASER - Object',i4,1x,a)
        call exit(1)
      endif
      numCircleObj = numCircleObj + 1
      if (numObjCircle == 1 .and. iobjCircle(1) == -999) &
          betterRadius(itype) = betterIn(min(numCircleObj, numBetterIn))
    endif
    if (typeOnList(itype, iobjline, numObjLine)) ibase = ibase + 1
    if (typeOnList(itype, iobjBound, numObjBound)) ibase = ibase + 1
    if (ibase > 1) then
      write(*,106) itype, &
          'is included in more than one list (circle, line, boundary)'
      call exit(1)
    endif
  enddo
  if (numCircleObj > 0 .and. numBetterIn > 1 .and. numCircleObj .ne. &
      numBetterIn) call exitError('THE NUMBER OF BETTER RADIUS VALUES '// &
      'MUST BE EITHER 1 OR THE SAME AS THE NUMBER OF CIRCLE OBJECTS')
  !
  ! Determine maximum size needed for boundary array
  maxBound = 0
  maxSizes = 0
  limSizes = 10
  do iobj = 1, max_mod_obj
    numInObj = npt_in_obj(iobj)
    itype = 256 - obj_color(2, iobj)
    if (typeOnList(itype, iobjBound, numObjBound)) maxBound = max(maxBound, numInObj)
    if (typeOnList(itype, iobjCircle, numObjCircle)) then
      maxSizes = max(maxSizes, numInObj)
      limSizes = limSizes + numInObj
    endif
  enddo
  imSize = max(nx * ny, maxSizes) + 10
  allocate(xbound(maxBound + 10), ybound(maxBound + 10), objTaper(max_mod_obj + 10), &
      sizes(limSizes), indSize(max_mod_obj + 10), array(imSize), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR BOUNDARY CONTOURS, SIZES, AND IMAGE')

  !
  ! Convert boundary objects to interior point collections
  do iobj = 1, max_mod_obj
    itype = 256 - obj_color(2, iobj)
    objTaper(iobj) = .false.
    if (npt_in_obj(iobj) > 2 .and. typeOnList(itype, iobjBound, numObjBound)) then
      if (contourArea(iobj) < taperedPatchCrit) then
        call fillBoundaryArrays(iobj, xbound, ybound, xmin, xmax, yMin, ymax)
        call convertBoundary(iobj, nx, ny, xbound, ybound, xmin, xmax, yMin, ymax)
      else
        objTaper(iobj) = .true.
      endif
    endif
  enddo
  !
  ! Check all but circle objects and tapered patch opjects
  do iobj = 1, max_mod_obj
    if (npt_in_obj(iobj) > 0) then
      ibase = ibase_obj(iobj)
      itype = 256 - obj_color(2, iobj)
      call objToCont(iobj, obj_color, imodObj, imodCont)

      if (typeOnList(itype, iobjline, numObjLine)) then
        if (npt_in_obj(iobj) .ne. 2) then
          write(*,107) imodObj, imodCont, 'does not have exactly two points'
107       format('ERROR: CCDERASER - object',i4,', contour',i6,1x,a)
          call exit(1)
        endif
        ip1 = object(ibase + 1)
        ip2 = object(ibase + 2)
        if (nint(p_coord(3, ip1)) .ne. nint(p_coord(3, ip2))) then
          write(*,107) imodObj, imodCont, &
              'is supposed to be a line and is not in one Z-plane'
          call exit(1)
        elseif (nint(p_coord(1, ip1) + 0.5) .ne. nint(p_coord(1, ip2) + 0.5) .and. &
            nint(p_coord(2, ip1) + 0.5) .ne. nint(p_coord(2, ip2) + 0.5)) then
          write(*,107) imodObj, imodCont, 'is supposed to be a line and is'// &
              ' not horizontal or vertical'
          call exit(1)
        endif
      else if (.not.typeOnList(itype, iobjCircle, numObjCircle) .and. &
          .not.objTaper(iobj)) then
        zmin = 1.e10
        zmax = -1.e10
        xmin = zmin
        xmax = zmax
        yMin = zmin
        ymax = zmax
        numInObj = npt_in_obj(iobj)
        if (numInObj > LIMPATCH) then
          write(*,107) imodObj, imodCont, 'has too many points for arrays'
          call exit(1)
        endif
        do ip = 1, numInObj
          ipt = object(ibase + ip)
          xmin = min(xmin, p_coord(1, ipt))
          xmax = max(xmax, p_coord(1, ipt))
          yMin = min(yMin, p_coord(2, ipt))
          ymax = max(ymax, p_coord(2, ipt))
          zmin = min(zmin, p_coord(3, ipt))
          zmax = max(zmax, p_coord(3, ipt))
        enddo
        if (xmax - xmin >= MAXDEV / 2 .or. ymax - yMin >= MAXDEV / 2) then
          write(*,107) imodObj, imodCont, &
              'has points too far apart, so patch is too large'
          call exit(1)
        endif
        if (.not.typeOnList(itype, iobjDoAll, numObjDoAll) .and. &
            zmax .ne. zmin) then
          write(*,107) 'is not in one Z-plane'
          call exit(1)
        endif
      endif
    endif
  enddo
  !
  ! Load point sizes
  indFree = 1
  indCont = 1
  sizeMax = -1.
  do itype = 1, getImodObjSize()
    if (typeOnList(itype, iobjCircle, numObjCircle)) then
      ierr = getImodSizes(itype, sizes(indFree), limSizes + 1 - indFree, numSizes)
      if (ierr .ne. 0) call exitError('LOADING POINT SIZES FROM MODEL')
      !
      ! Make indexes to all contours in this object
      do iobj = 1, max_mod_obj
        call objToCont(iobj, obj_color, imodObj, imodCont)
        if (npt_in_obj(iobj) > 0 .and. itype == imodObj) then
          indSize(iobj) = indCont
          indCont = indCont + npt_in_obj(iobj)
          !
          ! If there is a better radius for this object, get the sizes
          ! for this contour and replacethe defaults
          if (betterRadius(itype) > 0) then
            ierr = getContPointSizes(imodObj, imodCont, array, imSize, numConSize)
            if (numConSize > 0 .and. numConSize .ne. npt_in_obj(iobj)) &
                call exitError('MISMATCH BETWEEN CONTOUR AND POINT ARRAY SIZE')
            do i = 1, npt_in_obj(iobj)
              if (numConSize == 0 .or. array(i) < 0.) &
                  sizes(indSize(iobj) + i - 1) = betterRadius(itype)
            enddo
          endif
        endif
      enddo
      do i = indFree, indFree + numSizes - 1
        sizeMax = max(sizeMax, sizes(i))
      enddo
      indFree = indFree + numSizes
      if (indCont > indFree) call exitError( &
          'MISMATCH BETWEEN LOADED POINT SIZES AND POINTS IN CONTOURS')
    endif
  enddo
  if (ifVerbose .ne. 0 .and. sizeMax > 0) &
      print *,'Maximum point size', sizeMax
  if (3.1416 * sizeMax**2 + 5 > LIMPATCH) call exitError( &
      'THE LARGEST CIRCLE RADIUS IS TOO BIG FOR THE ARRAYS')
  !
  ! start looping on sections; need to read regardless
  !
  do izSect = 0, nz - 1
    write(*,101) izSect
101 format('Section',i5,' -',$)
    call imposn(1, izSect, 0)
    call irdsec(1, array,*99)
    numFix = 0
    lineFix = 0
    numTapering = 0
    !
    ! scan through points to see if any need fixing
    ! Go backwards in case this model is a peak model
    !
    do iobj = numObjOrig, 1, -1
      if (npt_in_obj(iobj) > 0) then
        ibase = ibase_obj(iobj)
        itype = 256 - obj_color(2, iobj)
        circleCont = typeOnList(itype, iobjCircle, numObjCircle)
        allSecCont = typeOnList(itype, iobjDoAll, numObjDoAll)
        contOnSecOrAllSec = nint(p_coord(3, object(ibase + 1))) == izSect .or. allSecCont
        !
        ! first see if this has a line to do
        !
        if (typeOnList(itype, iobjline, numObjLine)) then
          if (contOnSecOrAllSec) then
            if (lineFix == 0) write(*,'(a,$)') ' fixing lines -'
            lineFix = lineFix + 1
            ip1 = object(ibase + 1)
            ip2 = object(ibase + 2)
            ix1 = nint(p_coord(1, ip1) + 0.5)
            ix2 = nint(p_coord(1, ip2) + 0.5)
            iy1 = nint(p_coord(2, ip1) + 0.5)
            iy2 = nint(p_coord(2, ip2) + 0.5)
            !
            ! Check against other lines to determine borders.  First look
            ! for lines below, then for lines above
            !
            do idir = -1, 1, 2
              jBordLow = iborder
              iborder = 0
              nearby = .true.
              !
              ! Look for adjacent lines then ones next farther out, until
              ! find no lines with overlap in the other coordinate
              !
              do while (iborder <= 5 .and. nearby)
                nearby = .false.
                iborder = iborder + 1
                do jobj = numObjOrig, 1, -1
                  if (npt_in_obj(jobj) > 0) then
                    jbase = ibase_obj(jobj)
                    jtype = 256 - obj_color(2, jobj)
                    if (typeOnList(jtype, iobjline, numObjLine)) then
                      if (nint(p_coord(3, object(jbase + 1))) == izSect .or. &
                          typeOnList(jtype, iobjDoAll, numObjDoAll)) then
                        jp1 = object(jbase + 1)
                        jp2 = object(jbase + 2)
                        jx1 = nint(p_coord(1, jp1) + 0.5)
                        jx2 = nint(p_coord(1, jp2) + 0.5)
                        jy1 = nint(p_coord(2, jp1) + 0.5)
                        jy2 = nint(p_coord(2, jp2) + 0.5)
                        !
                        ! Check if line is on the line in question and
                        ! check endpoints for overlap
                        !

                        if ((iy1 == iy2 .and. jy1 == jy2 .and. &
                            jy1 == iy1 + idir * iborder .and. &
                            .not. (max(jx1, jx2) < min(ix1, ix2) .or. &
                            min(jx1, jx2) > max(ix1, ix2))) .or. &
                            (ix1 == ix2 .and. jx1 == jx2 .and. &
                            jx1 == ix1 + idir * iborder .and. &
                            .not.(max(jy1, jy2) < min(iy1, iy2) .or. &
                            min(jy1, jy2) > max(iy1, iy2)))) &
                            nearby = .true.
                      endif
                    endif
                  endif
                enddo
              enddo
            enddo
            jBordHigh = iborder
            ! print *,ix1, iy1, ix2, iy2, iBordLo, iBordHi
            call cleanLine(array, nx, ny, ix1, iy1, ix2, iy2, jBordLow, jBordHigh)
          endif
          !
          ! Next taper-inside contour
        elseif (contOnSecOrAllSec .and. objTaper(iobj)) then
          if (numTapering == 0) write(*,'(a,$)') ' tapering large patches -'
          numTapering = numTapering + 1
          call fillBoundaryArrays(iobj, xbound, ybound, xmin, xmax, yMin, ymax)
          call taperInsideCont(array, nx, ny, xbound, ybound, npt_in_obj(iobj), &
              xmin, xmax, yMin, ymax, ierr)
          if (ierr .ne. 0) then
            call objToCont(iobj, obj_color, imodObj, imodCont)
            write(*,107) imodObj, imodCont, 'does not have enough adjacent points'
            call exit(1)
          endif
          !
          ! Then check circle or other contour at Z
        elseif (contOnSecOrAllSec .or. circleCont) then
          !
          ! Set up to loop once or on circle points
          loopStart = 1
          loopEnd = 1
          numGrowIter = 0
          if (circleCont) loopEnd  = npt_in_obj(iobj)
          do loop = loopStart, loopEnd
            ixFixMin = 100000000
            ixFixMax = -100000000
            iyFixMin = 100000000
            iyFixMax = -100000000
            if (circleCont) then
              numGrowIter = numExpandIter
              !
              ! add circle point to patch if on this Z
              ipt = object(ibase + loop)
              numInObj = 0
              size = sizes(indSize(iobj) + loop - 1)
              if (nint(p_coord(3, ipt)) == izSect .and. size > 0) &
                  call addCircleToPatch(iobj, loop, size, numInObj, ixFixMin, ixFixMax, &
                  iyFixMin, iyFixMax)
              diamMerge = MAXDEV
            else
              !
              ! Or add contour points to patch
              numInObj = min(npt_in_obj(iobj), LIMPATCH)
              do ip = 1, numInObj
                ipt = object(ibase + ip)
                ixFix(ip) = p_coord(1, ipt) + 1.01
                iyFix(ip) = p_coord(2, ipt) + 1.01
                ixFixMin = min(ixFixMin, ixFix(ip))
                ixFixMax = max(ixFixMax, ixFix(ip))
                iyFixMin = min(iyFixMin, iyFix(ip))
                iyFixMax = max(iyFixMax, iyFix(ip))
              enddo
              diamMerge = 2 * radiusMax
            endif
            !
            if (numFix == 0) write(*,'(a,$)') ' fixing points -'
            numFix = numFix + 1
            !
            ! see if there are patches to merge - loop on objects until
            ! patch stops growing
            !
            if (.not. allSecCont .and. numInObj > 0 .and.  ifMerge .ne. 0) &
                then
              !
              ! The radius criterion for being too large is based on the
              ! maximum radius entry unless there is a circle in the merge,
              ! then it is based on the MAXDEV parameter
              radSq = diamMerge**2
              ifGrew = 1
              do while (ifGrew > 0)
                ifGrew = 0
                do i = 1, numObjOrig
                  jbase = ibase_obj(i)
                  jtype = 256 - obj_color(2, i)
                  if (typeOnList(jtype, iobjCircle, numObjCircle)) then
                    !
                    ! For a circle contour, loop on points, first check Z,
                    ! space in patch, and against the min/max of patch
                    do ip1 = 1, npt_in_obj(i)
                      ipt = object(jbase + ip1)
                      size = sizes(indSize(i) + ip1 - 1)
                      if (nint(p_coord(3, ipt)) == izSect .and. &
                          size > 0 .and. &
                          (i .ne. iobj .or. loop .ne. ip1) .and. &
                          numInObj + 3.1416 * size**2 + 5 <= LIMPATCH) then
                        xcen = p_coord(1, ipt)
                        ycen = p_coord(2, ipt)
                        if (xcen + size + 1 >= ixFixMin .and. &
                            xcen - size - 1 <= ixFixMax .and. &
                            ycen + size + 1 >= iyFixMin .and. &
                            ycen - size - 1 <= iyFixMax) then
                          !
                          ! Then check each pixel for ones close enough to
                          ! touch and ones far enough to make patch too big
                          ifTouch = 0
                          ip2 = 1
                          do while (ip2 <= numInObj .and. ifTouch >= 0)
                            dist = sqrt((xcen - ixFix(ip2))**2 + &
                                (ycen - iyFix(ip2))**2)
                            if (dist - size < 1.5) ifTouch = 1
                            if (dist + size > MAXDEV) ifTouch = -1
                            ip2 = ip2 + 1
                          enddo
                          !
                          ! Merge if touch and size not too big
                          if (ifTouch > 0) then
                            if (ifVerbose .ne. 0) then
                              call objToCont(iobj, obj_color, ix1, iy1)
                              call objToCont(i, obj_color, ix2, iy2)
                              write (*,'(a,3i5,a,3i5)') 'Merging', ix2, iy2, &
                                  ip1, ' to', ix1, iy1, loop
                            endif
                            diamMerge = MAXDEV
                            call addCircleToPatch(i, ip1, size, numInObj, ixFixMin, &
                                ixFixMax, iyFixMin, iyFixMax)
                            sizes(indSize(i) + ip1 - 1) = 0.
                            ifGrew = 1
                          endif
                        endif
                      endif
                    enddo
                    !
                    ! Check regular contour
                  else if (i .ne. iobj .and. npt_in_obj(i) > 0 .and. &
                      nint(p_coord(3, object(jbase + 1))) == izSect .and. &
                      .not. typeOnList(jtype, iobjline, numObjLine) .and. &
                      .not. typeOnList(jtype, iobjDoAll, numObjDoAll) .and. &
                      numInObj + npt_in_obj(i) <= LIMPATCH) then
                    !
                    ! Loop on all pairs of points and make sure none are
                    ! too far and see if one touches
                    !
                    ifTouch = 0
                    ip1 = 1
                    do while (ip1 <= npt_in_obj(i) .and. ifTouch >= 0)
                      ipt = object(jbase + ip1)
                      ix1 = p_coord(1, ipt) + 1.01
                      iy1 = p_coord(2, ipt) + 1.01
                      ip2 = 1
                      do while (ip2 <= numInObj .and. ifTouch >= 0)
                        ix2 = (ix1 - ixFix(ip2))**2 + (iy1 - iyFix(ip2))**2
                        if (ix2 <= 2) ifTouch = 1
                        if (ix2 > radSq) ifTouch = -1
                        ip2 = ip2 + 1
                      enddo
                      ip1 = ip1 + 1
                    enddo
                    !
                    ! Merge the patch, set # of points to 0, and set # of
                    ! points 0 for starting patch to avoid duplicate hits
                    !
                    if (ifTouch > 0) then
                      if (ifVerbose .ne. 0) then
                        call objToCont(iobj, obj_color, ix1, iy1)
                        call objToCont(i, obj_color, ix2, iy2)
                        write (*,'(a,2i5,a,3i5)') 'Merging', ix2, iy2, ' to', &
                            ix1, iy1, loop
                      endif
                      do ip = 1, npt_in_obj(i)
                        ipt = object(jbase + ip)
                        numInObj = numInObj + 1
                        ixFix(numInObj) = p_coord(1, ipt) + 1.01
                        iyFix(numInObj) = p_coord(2, ipt) + 1.01
                        ixFixMin = min(ixFixMin, ixFix(ip))
                        ixFixMax = max(ixFixMax, ixFix(ip))
                        iyFixMin = min(iyFixMin, iyFix(ip))
                        iyFixMax = max(iyFixMax, iyFix(ip))
                      enddo
                      npt_in_obj(i) = 0
                      ifGrew = 1
                    endif
                  endif
                enddo
              enddo
              !
              ! Zero out this contour or point
              if (circleCont) then
                sizes(indSize(iobj) + loop - 1) = 0.
              else
                npt_in_obj(iobj) = 0
              endif
            endif
            !
            if (numInObj > 0) call cleanArea(array, numInObj, numGrowIter)
          enddo
        endif
      endif
    enddo
    !
    ! do peak search for automatic removal
    !
    numPatch = 0
    numPatchOut = 0
    numPtOut = 0
    if (ifPeakSearch > 0) call searchPeaks(array, diffArr, izSect, numPatch, numPixels, &
        numPtOut, numPatchOut)
    if (numPatch > 0) write(*,102) numPixels, numPatch
102 format(i7,' pixels replaced in',i6,' peaks -',$)
    !
    ! Save points from this section in model if requested
    !
    if (modelOut .ne. ' ') then
      do iobj = 1, numPatchOut
        numPixels = indPatch(iobj + 1) - indPatch(iobj)
        if (n_point + numPixels <= max_pt .and. &
            max_mod_obj < max_obj_num) then
          !
          ! If there is room for both another object and all the points,
          ! then set up the object and copy the points
          !
          max_mod_obj = max_mod_obj + 1
          obj_color(2, max_mod_obj) = 256 - &
              max(1, min(maxObjectsOut, int(1. + exceedCrit(iobj))))
          obj_color(1, max_mod_obj) = 1
          npt_in_obj(max_mod_obj) = numPixels
          ibase_obj(max_mod_obj) = ibase_free
          ibase_free = ibase_free + numPixels
          do i = indPatch(iobj), indPatch(iobj + 1) - 1
            n_point = n_point + 1
            p_coord(1, n_point) = ixOut(i) - 0.5
            p_coord(2, n_point) = iyOut(i) - 0.5
            p_coord(3, n_point) = izOut(i)
            object(n_point) = n_point
          enddo
        endif
      enddo
    endif
    !
    call iclden(array, nx, ny, 1, nx, 1, ny, dminTmp, dmaxTmp, dmeant)
    tmin = min(tmin, dminTmp)
    tmax = max(tmax, dmaxTmp)
    tsum = tsum + dmeant
    !
    ! write out if any changes or if new output file
    !
    if ((numFix > 0 .or. lineFix > 0 .or. numPatch > 0 .or. imFileOut == 2) &
        .and. ifTrialMode == 0) then
      call imposn(imFileOut, izSect, 0)
      call iwrsec(imFileOut, array)
    endif
    write(*,'(a)') ' Done'
    call flush(6)
  enddo
  !
  tmean = tsum / nz
  write(titlech, 109) dat, tim
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
109 format('CCDERASER: Bad points replaced with interpolated values' &
      , t57, a9, 2x, a8 )
  if (ifTrialMode == 0) then
    call iwrhdr(imFileOut, title, 1, tmin, tmax, tmean)
    call imclose(imFileOut)
  else
    print *,'New minimum and maximum density would be:', tmin, tmax
  endif
  call irtorg(1, origin(1), origin(2), origin(3))
  call irtdel(1, delta)

  !
  ! put out a model, even if there are no points.  Slide objects down
  ! over original input model if any
  !
  if (modelOut .ne. ' ') then
    if (numObjOrig > 0) then
      do iobj = numObjOrig + 1, max_mod_obj
        i = iobj - numObjOrig
        obj_color(1, i) = obj_color(1, iobj)
        obj_color(2, i) = obj_color(2, iobj)
        npt_in_obj(i) = npt_in_obj(iobj)
        ibase_obj(i) = ibase_obj(iobj)
      enddo
      max_mod_obj = max_mod_obj - numObjOrig
    endif
    !
    ! Convert to montage coordinates
    ! Have to shift montage coordinates to 0 because it is a new model
    if (numPcList > 0) then
      do iobj = 1, max_mod_obj
        ibase = ibase_obj(iobj)
        do ip = 1, npt_in_obj(iobj)
          ipt = object(ibase + ip)
          i = nint(p_coord(3, ipt)) + 1
          p_coord(1, ipt) = p_coord(1, ipt) + ixPcList(i) - minXpiece
          p_coord(2, ipt) = p_coord(2, ipt) + iyPcList(i) - minYpiece
          p_coord(3, ipt) = izPcList(i)
        enddo
      enddo
    endif
    !
    call newImod()
    do i = 1, maxObjectsOut
      call putImodFlag(i, 2)
      call putSymType(i, 0)
      call putSymSize(i, 5)
    enddo
    call putImageRef(delta, origin)
    call putImodMaxes(nx, ny, nz)
    call scaleModelToImage(1, 1)
    call write_wmod(modelOut)
    write(*,105) maxObjectsOut, maxObjectsOut, maxObjectsOut - 1
105 format('In the output model, contours have been sorted into',i3, &
        ' objects based on how',/, &
        ' much a peak exceeds the criterion.',/, &
        ' Object 1 has peaks that exceed the criterion by < 1 SD,' &
        ,/, ' object 2 has peaks that exceed the criterion by ', &
        '1 - 2 SDs,',/, ' object ',i2, &
        ' has peaks that exceed the criterion by >',i2, ' SDs')
  endif

  call exit(0)
99 call exitError('READING FILE')
end program ccderaser


! SEARCHPEAKS finds X rays given all the parameters being passed in and set in module
!
subroutine searchPeaks(array, diffArr, izSect, numPatch, numPixels, numPtOut, &
    numPatchOut)
  use ccdvars
  implicit none
  integer LIMLIST
  parameter (LIMLIST = 400)
  integer*4 izSect
  real*4 array(nx,ny), diffArr(LIMDIFF,LIMDIFF)
  real*4 bigDiffs(LIMPATCH)
  integer*4 numPtOut, numPatchOut
  integer*4 numPtSave, numPatchSave
  integer*4 numScanX, numScanY, nxScan, nyScan, iScanY, iScanX, iyStart
  integer*4 iyEnd, ixStart, ixEnd, jx, jxs, jxn, jy, jys, jyn, ix, iy
  real*4 dmin, dmax, psum, sumsq, scanAvg, scanSd, polarity, scanCrit
  integer*4 ixPeak, iyPeak, numSum, numInPatch, numPatch, numPixels
  real*4 radMaxSq, growCrit, sdDiff, absDiffAvg, absDiffSd
  real*4 pixDiff, diffAvg, diffSd, diffCrit
  logical movedPeak, storePatch, onList, aboveCrit
  integer*4 numInList, ixList(LIMLIST), iyList(LIMLIST), lookingAt, i, j, ip
  integer*4 minDistSq, maxDistSq, iDistSq, ixOffset, iyOffset
  integer*4 ixMin, ixMax, iyMin, iyMax, nxUse, nyUse, numAtPeak
  real*8 sum8, sumsq8

  numPatch = 0
  numPixels = 0
  nxUse = nx - 2 * numEdgePixels
  nyUse = ny - 2 * numEdgePixels
  !
  ! set up extent of scan regions
  !
  numScanX = max(1., (nxUse - scanOverlap * iScanSize) / &
      (iScanSize * (1. - scanOverlap)))
  nxScan = nxUse / (numScanX - (numScanX - 1) * scanOverlap)
  do while (nxScan > LIMDIFF - numScanX)
    numScanX = numScanX + 1
    nxScan = nxUse / (numScanX - (numScanX - 1) * scanOverlap)
  enddo

  numScanY = max(1., (nyUse - scanOverlap * iScanSize) / &
      (iScanSize * (1. - scanOverlap)))
  nyScan = nyUse / (numScanY - (numScanY - 1) * scanOverlap)
  do while (nyScan > LIMDIFF - numScanY)
    numScanY = numScanY + 1
    nyScan = nyUse / (numScanY - (numScanY - 1) * scanOverlap)
  enddo
  !
  ! loop on scan regions, getting start and end in each dimension
  !
  do iScanY = 1, numScanY
    iyStart = 1 + numEdgePixels + (nyUse - nyScan) * (iScanY - 1) / &
        max(1, numScanY - 1)
    iyEnd = iyStart + nyScan - 1
    if (iScanY == numScanY) iyEnd = ny - numEdgePixels
    do iScanX = 1, numScanX
      ixStart = 1 + numEdgePixels + (nxUse - nxScan) * (iScanX - 1) / &
          max(1, numScanX - 1)
      ixEnd = ixStart + nxScan - 1
      if (iScanX == numScanX) ixEnd = nx - numEdgePixels
      !
      ! get statistics and scan for points outside the reduced criterion
      !
      call iclavgsd(array, nx, ny, ixStart, ixEnd, iyStart, iyEnd, &
          dmin, dmax, sum8, sumsq8, scanAvg, scanSd)
      scanCrit = critScan * scanSd
      !
      ! get mean of difference from neighbors now, for looking at big differences
      !
      ixOffset = 1 - ixStart
      iyOffset = 1 - iyStart
      call computeDiffs(array, nx, ny, diffArr, LIMDIFF, LIMDIFF, ixStart + 1,  &
          ixEnd - 1, iyStart + 1, iyEnd - 1, ixOffset, iyOffset, diffAvg, diffSd,  &
          absDiffAvg, absDiffSd, 0)
      !
      ! Now loop on points in patch looking for peaks above criterion
      do iy = iyStart, iyEnd
        do ix = ixStart, ixEnd
          if (abs(array(ix, iy) - scanAvg) > scanCrit) then
            !
            ! found a point above criterion, need to walk to peak
            ! Start a list of peak points to check
            polarity = sign(1., array(ix, iy) - scanAvg)
            ixPeak = ix
            iyPeak = iy
            numAtPeak = 1
            ixList(1) = ix
            iyList(1) = iy
            lookingAt = 1
            !
            ! Check points on list; loop until all checked
            do while (lookingAt <= numAtPeak)
              jxs = max(1, ixList(lookingAt) - 1)
              jxn = min(nx, ixList(lookingAt) + 1)
              jys = max(1, iyList(lookingAt) - 1)
              jyn = min(ny, iyList(lookingAt) + 1)
              do jy = jys, jyn
                do jx = jxs, jxn
                  if (polarity * (array(jx, jy) - array(ixPeak, iyPeak)) >= 0.) then
                    !
                    ! If the point is equal, add it to the list
                    if (array(jx, jy) == array(ixPeak, iyPeak)) then
                      onList = .false.
                      do ip = 1, numAtPeak
                        if (jx == ixList(ip) .and. jy == iyList(ip)) onList = .true.
                      enddo
                      if (.not. onList .and. numAtPeak < LIMLIST) then
                        numAtPeak = numAtPeak + 1
                        ixList(numAtPeak) = jx
                        iyList(numAtPeak) = jy
                      endif
                    else
                      !
                      ! If the point is higher, move to it, reset the list, keep looking
                      ! around current center in case an even higher one occurs
                      ixPeak = jx
                      iyPeak = jy
                      numAtPeak = 1
                      ixList(1) = jx
                      iyList(1) = jy
                      lookingAt = 0
                    endif
                  endif
                enddo
              enddo
              lookingAt = lookingAt + 1
            enddo
            if (numAtPeak > 1) then
              ixPeak = nint(sum(ixList(1:numAtPeak)) / float(numAtPeak))
              iyPeak = nint(sum(iyList(1:numAtPeak)) / float(numAtPeak))
            endif
            call checkAndErasePeak(radiusMax, outerRadius, critMain, .false., aboveCrit, &
                'P')
            if (critGiant > 0. .and. .not. aboveCrit .and.  &
                abs(array(ix, iy) - scanAvg) > (critScan + 1.) * scanSd) then
              call checkAndErasePeak(giantRadius, giantRadius + outerRadius - radiusMax, &
                  critGiant, .true., aboveCrit, 'Giant p')
            endif
          endif
        enddo
      enddo
      !
      ! Next search for single-pixel difference anomalies
      diffCrit = critDiff * diffSd
      growCrit = critGrow * diffSd
      radMaxSq = radiusMax**2
      do iy = iyStart + 1, iyEnd - 1
        do ix = ixStart + 1, ixEnd - 1
          pixDiff = diffArr(ix + ixOffset, iy + iyOffset)
          if (abs(pixDiff - diffAvg) > diffCrit) then
            !
            ! Make a list of adjacent points that exceed criterion in
            ! either direction.  Accumulate sum of points on list
            ! and neighboring  points not on list
            !
            numInList = 1
            ixList(1) = ix
            iyList(1) = iy
            lookingAt = 1
            ixMin = ix
            ixMax = ixMin
            iyMin = iy
            iyMax = iyMin
            psum = array(ix, iy)
            do while (lookingAt <= numInList)
              do jx = ixList(lookingAt) - 1, ixList(lookingAt) + 1
                do jy = iyList(lookingAt) - 1, iyList(lookingAt) + 1
                  !
                  ! compute min and max distance from existing points
                  !
                  minDistSq = (jx - ixList(1))**2 + (jy - iyList(1))**2
                  maxDistSq = minDistSq
                  do i = 1, numInList
                    iDistSq = (jx - ixList(i))**2 + (jy - iyList(i))**2
                    minDistSq = min(minDistSq, iDistSq)
                    maxDistSq = max(maxDistSq, iDistSq)
                  enddo
                  !
                  ! if point not on list and is within diameter and is
                  ! interior and exceeds grow criterion, add to list
                  !
                  if (minDistSq > 0 .and. maxDistSq <= 4. * radMaxSq &
                      .and. jx > ixStart .and. jx < ixEnd .and. &
                      jy > iyStart .and. jy < iyEnd .and. &
                      abs(diffArr(jx + ixOffset, jy + iyOffset) - diffAvg) > growCrit &
                      .and. numInList < LIMLIST) then
                    numInList = numInList + 1
                    ixList(numInList) = jx
                    iyList(numInList) = jy
                    ixMin = min(ixMin, jx)
                    ixMax = max(ixMax, jx)
                    iyMin = min(iyMin, jy)
                    iyMax = max(iyMax, jy)
                    psum = psum + array(jx, jy)
                  endif
                enddo
              enddo
              lookingAt = lookingAt + 1
            enddo
            !
            ! get polarity from mean of points on list versus mean
            ! of neighboring points
            !
            sumsq = 0.
            do jx = ixMin - 1, ixMax + 1
              do jy = iyMin - 1, iyMax + 1
                sumsq = sumsq + array(jx, jy)
              enddo
            enddo
            sumsq = (sumsq - psum) / &
                ((ixMax + 3 - ixMin) * (iyMax + 3 - iyMin) - numInList)
            polarity = sign(1., psum / numInList - sumsq)
            !
            ! and order the list by differences
            !

            do i = 1, numInList - 1
              do j = i + 1, numInList
                if ((diffArr(ixList(i) + ixOffset, iyList(i) + iyOffset) - &
                    diffArr(ixList(j) + ixOffset, iyList(j) + iyOffset)) * &
                    polarity < 0.) then
                  jx = ixList(i)
                  ixList(i) = ixList(j)
                  ixList(j) = jx
                  jy = iyList(i)
                  iyList(i) = iyList(j)
                  iyList(j) = jy
                endif
              enddo
            enddo
            !
            ! Move points to fix list, starting with strongest, and
            ! for each other point, recompute difference measure
            ! excluding the ones already on fix list
            !
            numInPatch = 0
            storePatch = (numPatchOut < LIMPATCHOUT - 1 .and. &
                numPtOut < LIMPTOUT)
            numPtSave = numPtOut
            numPatchSave = numPatchOut
            call addPointToPatch(ixList(1), iyList(1))
            do i = 2, numInList
              numSum = 0
              psum = 0.
              do jx = ixList(i) - 1, ixList(i) + 1
                do jy = iyList(i) - 1, iyList(i) + 1
                  onList = jx == ixList(i) .and. jy == iyList(i)
                  do j = 1, numInPatch
                    if (jx == ixFix(j) .and. jy == iyFix(j)) &
                        onList = .true.
                  enddo
                  if (.not.onList) then
                    psum = psum + array(jx, jy)
                    numSum = numSum + 1
                  endif
                enddo
              enddo
              !
              ! add point to list if it passes the grow criterion
              !
              if (numSum > 0) then
                pixDiff = array(ixList(i), iyList(i)) - psum / numSum
                if (polarity * (pixDiff - diffAvg) > growCrit) then
                  call addPointToPatch(ixList(i), iyList(i))
                endif
              endif
            enddo

            if (numInPatch <= maxInDiffPatch) then
              pixDiff = diffArr(ixFix(1) + ixOffset, iyFix(1) + iyOffset)
              sdDiff = abs(pixDiff - diffAvg) / diffSd
              if (storePatch) then
                exceedCrit(numPatchOut + 1) = sdDiff - critDiff
                numPatchOut = numPatchOut + 1
                indPatch(numPatchOut + 1) = numPtOut + 1
              endif
              !
              ! report if verbose, fix the patch
              !
              if (ifVerbose > 0) write (*,104) ixFix(1), iyFix(1), &
                  array(ix, iy), pixDiff, sdDiff
104           format(/,'Diff peak at',2i6,' = ',f8.0,', diff =',f8.0, &
                  ', ',f7.2, ' SDs above mean',$)
              call cleanAreaRecomputeDiffs()
            else
              numPtOut = numPtSave
              numPatchOut = numPatchSave
            endif
          endif
        enddo
      enddo

    enddo
  enddo
  return

CONTAINS

  ! checkAndErasePeak finds the mean and SD in the annulus defined by its arguments
  ! and tests the currently found peak with the given criterion.  It sets aboveCrit
  ! true if it erases the peak
  !
  subroutine checkAndErasePeak(radMax, outerRad, critPeak, doingBig, aboveCrit, bigText)
    real*4 radMax, outerRad, critPeak
    real*4 outerSq, radSq, ringAvg, ringSd, radInnerSq
    real*4 peakXcen, peakYcen
    integer*4 iouter, numPlast
    integer*4 ixpCen, iypCen, numGrowLoop, loopGrow, ixpkSum, iypkSum, ixpLast, iypLast
    logical aboveCrit, doingBig
    character*(*) bigText
    iouter = nint(outerRad)
    radInnerSq = radMax**2
    outerSq = outerRad**2
    numPtSave = numPtOut
    !
    ! find mean and SD in the annulus
    !
    jxs = max(1, ixPeak - iouter)
    jxn = min(nx, ixPeak + iouter)
    jys = max(1, iyPeak - iouter)
    jyn = min(ny, iyPeak + iouter)
    numSum = 0
    sum8 = 0.
    sumsq8 = 0.
    do jy = jys, jyn
      do jx = jxs, jxn
        radSq = (jx - ixPeak)**2 + (jy - iyPeak)**2
        if (radSq > radInnerSq .and. radSq <= outerSq) then
          numSum = numSum + 1
          sum8 = sum8 + array(jx, jy)
          sumsq8 = sumsq8 + array(jx, jy)**2
        endif
      enddo
    enddo
    call sums_to_avgsd8(sum8, sumsq8, numSum, 1, ringAvg, ringSd)
    !
    ! Make sure the peak passes the main criterion
    !
    sdDiff = abs(array(ixPeak, iyPeak) - ringAvg) / ringSd
    aboveCrit = sdDiff > critPeak
    if (aboveCrit) then
      !
      ! look inside radius and make list of points above the grow criterion 
      ! Repeat the loop for a big patch or one that would qualify for that, so that
      ! the center can be moved
      growCrit = ringSd * critGrow
      iouter = nint(radMax)
      peakXcen = ixPeak
      peakYcen = iyPeak
      numGrowLoop = 1
      if (doingBig .or. (critGiant > 0. .and. sdDiff > critGiant)) numGrowLoop = 3
      do loopGrow = 1, numGrowLoop
        !
        ! initialize for this time through the loop, get limits to look at
        numInPatch = 0
        ixpkSum = 0.
        iypkSum = 0.
        numPtOut = numPtSave
        jxs = max(1, nint(peakXcen) - iouter)
        jxn = min(nx, nint(peakXcen) + iouter)
        jys = max(1, nint(peakYcen) - iouter)
        jyn = min(ny, nint(peakYcen) + iouter)
        storePatch = (numPatchOut < LIMPATCHOUT - 1 .and. numPtOut < LIMPTOUT)
        do jy = jys, jyn
          do jx = jxs, jxn
            !
            ! Find points within radius limit and above the grow criterion
            radSq = (jx - peakXcen)**2 + (jy - peakYcen)**2
            if (radSq < radInnerSq .and. &
                polarity * (array(jx, jy) - ringAvg) > growCrit &
                .and. numInPatch < limPatch) then
              call addPointToPatch(jx, jy)
              ixpkSum = ixpkSum + jx
              iypkSum = iypkSum + jy
              !
              ! If doing a big patch, store the point's absolute difference
              if (doingBig) then
                bigDiffs(numInPatch) = abs(max( &
                    abs(array(jx, jy) - array(max(1, jx - 1), jy)),  &
                    abs(array(jx, jy) - array(min(nx, jx + 1), jy)), &
                    abs(array(jx, jy) - array(jx, max(1, jy - 1))), &
                    abs(array(jx, jy) - array(jx, min(ny, jy + 1)))) - absDiffAvg) /  &
                    absDiffSd
              endif
            endif
          enddo
        enddo
        !
        ! If looping more than once, check if it has stabilized and revise the center
        if (numGrowLoop > 1) then
          if (loopGrow > 1 .and. numPlast == numInPatch .and. ixpLast == ixpkSum .and. &
              iypLast == iypkSum) then
            exit
          endif
          peakXcen = float(ixpkSum) / numInPatch
          peakYcen = float(iypkSum) / numInPatch
          numPlast = numInPatch
          ixpLast = ixpkSum
          iypLast = iypkSum
        endif
      enddo
      !
      ! If doing big peak, make sure there were enough big differences and abort if not
      if (doingBig) then
        call rsSortFloats(bigDiffs, numInPatch)
        jx = min(numInPatch, max(3, nint(fracBigDiff * numInPatch)))
        if (sum(bigDiffs(numInPatch + 1 - jx : numInPatch)) / jx < critBigDiff) then
          numPtOut = numPtSave
          aboveCrit = .false.
          return
        endif
      endif
      if (storePatch) then
        exceedCrit(numPatchOut + 1) = sdDiff - critMain
        numPatchOut = numPatchOut + 1
        indPatch(numPatchOut + 1) = numPtOut + 1
      endif
      !
      ! fix the patch!
      !
      if (ifVerbose > 0) write (*,103) bigText, ixPeak, iyPeak, &
          array(ixPeak, iyPeak), sdDiff, ringAvg
103   format(/,a,'eak at',2i6,' = ',f8.0,',',f7.2, ' SDs above mean',f8.0,$)
      call cleanAreaRecomputeDiffs()

    endif
    return
  end subroutine checkAndErasePeak


  ! addPointToPatch adds one point to the patch, maintain min and max of patch, and save
  ! in output array if there is space
  !
  subroutine addPointToPatch(ixAdd, iyAdd)
    integer*4 ixAdd, iyAdd, numOut
    numInPatch = numInPatch + 1
    ixFix(numInPatch) = ixAdd
    iyFix(numInPatch) = iyAdd
    if (numInPatch == 1) then
      ixMin = ixAdd
      ixMax = ixAdd
      iyMin = iyAdd
      iyMax = iyAdd
    else
      ixMin = min(ixMin, ixAdd)
      ixMax = max(ixMax, ixAdd)
      iyMin = min(iyMin, iyAdd)
      iyMax = max(iyMax, iyAdd)
    endif
    if (storePatch .and. numPtOut < LIMPTOUT) then
      numPtOut = numPtOut + 1
      ixOut(numPtOut) = ixAdd
      iyOut(numPtOut) = iyAdd
      izOut(numPtOut) = izSect
    endif
    return
  end subroutine addPointToPatch


  ! cleanAreaRecomputeDiffs calls cleanArea then adjusts the limits of patch and
  ! recomputes the differences in this area in a way that maintains the means/sds
  !
  subroutine cleanAreaRecomputeDiffs()
    !
    ! fix the difference array 1 pixel beyonds limits of patch by first calling
    ! to subtract this area, then calling after the replacement to add it back
    ixMin = max(ixMin - 1, ixStart + 1)
    ixMax = min(ixMax + 1, ixEnd - 1)
    iyMin = max(iyMin - 1, iyStart + 1)
    iyMax = min(iyMax + 1, iyEnd - 1)
    call computeDiffs(array, nx, ny, diffArr, LIMDIFF, LIMDIFF, ixMin, ixMax, &
        iyMin, iyMax, ixOffset, iyOffset, diffAvg, diffSd, absDiffAvg, absDiffSd, -1)
    call cleanArea(array, numInPatch, 0)
    call computeDiffs(array, nx, ny, diffArr, LIMDIFF, LIMDIFF, ixMin, ixMax, &
        iyMin, iyMax, ixOffset, iyOffset, diffAvg, diffSd, absDiffAvg, absDiffSd, 1)
    numPatch = numPatch + 1
    numPixels = numPixels + numInPatch
    return
  end subroutine cleanAreaRecomputeDiffs

end subroutine searchPeaks


! COMPUTEDIFFS finds a mean difference between each pixel and its 8 neighbors over the
! range ixStart-ixEnd, iyStart-iyEnd in array, places the result in diffArr using the
! offsets in ixofs, iyofs and returns the mean and standard deviation of these differences
! in diffAvg and diffSd.  It also computes the absolute value of differences between all
! vertically and horizontally adjacent pairs of pixels and returns the mean and SD of that
! difference in absDiffAvg and absDiffSd.
!
subroutine computeDiffs(array, nx, ny, diffArr, ixdim, iyDim, ixStart, ixEnd, iyStart, &
    iyEnd, ixOffset, iyOffset, diffAvg, diffSd, absDiffAvg, absDiffSd, incremental)
  implicit none
  integer*4 nx, ny, ixdim, iyDim, ixStart, ixEnd, iyStart, iyEnd, incremental
  real*4 array(nx,ny), diffArr(ixdim,iyDim), diffAvg, diffSd, absDiffAvg, absDiffSd
  real*4 pixDiff, addFac
  integer*4 ix, iy, numSum, ixOffset, iyOffset
  real*8 sum8, sumsq8, abssum8, abssq8
  save numSum, sum8, sumsq8, abssum8, abssq8
  if (incremental == 0) then
    sum8 = 0.
    sumsq8 = 0.
    abssum8 = 0.
    abssq8 = 0.
    numSum = 0
    addFac = 1.
  else
    addFac = sign(1, incremental)
  endif
  numSum = numSum + sign(1, incremental) * (iyEnd + 1 - iyStart) * (ixEnd + 1 - ixStart)
  do iy = iyStart, iyEnd
    do ix = ixStart, ixEnd
      pixDiff = array(ix, iy) - (array(ix - 1, iy) + array(ix + 1, iy) + &
          array(ix, iy + 1) + array(ix, iy - 1) + array(ix - 1, iy - 1) + &
          array(ix - 1, iy + 1) + array(ix + 1, iy - 1) + array(ix + 1, iy + 1)) / 8.
      sum8 = sum8 + addFac * pixDiff
      sumsq8 = sumsq8 + addFac * pixDiff**2
      diffArr(ix + ixOffset, iy + iyOffset) = pixDiff
      pixDiff = abs(array(ix, iy) - array(ix - 1, iy))
      abssum8 = abssum8 + addFac * pixDiff
      abssq8 = abssq8 + addFac * pixDiff**2
      pixDiff = abs(array(ix, iy) - array(ix, iy - 1))
      abssum8 = abssum8 + addFac * pixDiff
      abssq8 = abssq8 + addFac * pixDiff**2
    enddo
  enddo
  call sums_to_avgsd8(sum8, sumsq8, numSum, 1, diffAvg, diffSd)
  call sums_to_avgsd8(abssum8, abssq8, numSum * 2, 1, absDiffAvg, absDiffSd)
  return
end subroutine computeDiffs


! CLEANLINE replaces points along a line with points from adjacent
! lines
!
subroutine cleanLine(array, ixdim, iyDim, ix1, iy1, ix2, iy2, jBordLow, jBordHigh)
  implicit none
  integer*4 ixdim, iyDim, ix1, ix2, iy1, iy2, jBordLow, jBordHigh
  real*4 array(ixdim,iyDim)
  integer*4 iy, ix
  if (ix1 == ix2) then
    do iy = min(iy1, iy2), max(iy1, iy2)
      array(ix1, iy) = (array(ix1 - jBordLow, iy) + array(ix1 + jBordHigh, iy)) / 2.
    enddo
  else
    do ix = min(ix1, ix2), max(ix1, ix2)
      array(ix, iy1) = (array(ix, iy1 - jBordLow) + array(ix, iy1 + jBordHigh)) / 2.
    enddo
  endif
  return
end subroutine cleanLine


! CLEANAREA replaces a patch given the list of points in ixfix, iyfix, by finding
! surrounding points, fitting a polynomial to them, and replacing points in the
! patch with fitted values.  Can grow the patch if numGrowIter is > 0
!
subroutine cleanArea(array, numInObj, numGrowIter)
  !
  use ccdvars
  implicit none
  !
  real*4 array(nx,ny)
  integer*4 numInObj, numGrowIter
  logical*1 inList(-MAXDEV:MAXDEV,-MAXDEV:MAXDEV)
  integer*2 adjacent(-MAXDEV:MAXDEV,-MAXDEV:MAXDEV)
  real*4 adjValue(LIMPATCH)
  logical nearEdge
  real*4 rmat(matSize,LIMPATCH), xm(matSize), sd(matSize) , ssd(matSize,matSize)
  real*4 b1(matSize), vector(matSize)
  !
  integer*4 ixCen, iyCen, i, j, minXlist, minYlist, maxXlist, maxYlist, igrow, numVals
  integer*4 numPatPix, numAdded
  integer*4 k, ixl, iyl, numBordM1, ixBordLow, ixBordHigh, iyBordLow, iyBordHigh, icut
  integer*4 numPix, numPoints, nindep, ix, iy, ixOffset, iyOffset, minAdjacent
  real*4 c1, rsq, fra, xsum, patchSum, polarity, cutoff, percentile, adjMedian, pctile
  logical warned /.false./
  save warned

  percentile = 0.05
  do igrow = 0, numGrowIter
    !
    ! get range of patch and initialize initialize inlist and adjacent arrays
    !
    minXlist = ixFix(1)
    minYlist = iyFix(1)
    maxXlist = ixFix(1)
    maxYlist = iyFix(1)
    do k = 2, numInObj
      minXlist = min(minXlist, ixFix(k))
      minYlist = min(minYlist, iyFix(k))
      maxXlist = max(maxXlist, ixFix(k))
      maxYlist = max(maxYlist, iyFix(k))
    enddo
    ixCen = (maxXlist + minXlist) / 2
    iyCen = (maxYlist + minYlist) / 2
    !
    do j = -MAXDEV, MAXDEV
      do i = -MAXDEV, MAXDEV
        inList(i, j) = .false.
        adjacent(i, j) = 0
      enddo
    enddo
    !
    ! Mark all points as inlist and all adjacent points as 1's
    do k = 1, numInObj
      ixl = ixFix(k) - ixCen
      iyl = iyFix(k) - iyCen
      inList(ixl, iyl) = .true.
      do i = -1, 1
        do j = -1, 1
          adjacent(ixl + i, iyl + j) = 1
        enddo
      enddo
    enddo
    !
    ! get limits of region including border
    numBordM1 = numPixBorder - 1
    ixBordLow = max(1, minXlist - numPixBorder)
    ixBordHigh = min(nx, maxXlist + numPixBorder)
    iyBordLow = max(1, minYlist - numPixBorder)
    iyBordHigh = min(ny, maxYlist + numPixBorder)
    !
    ! if growing, get the sum of points in patch and an array of adjacent points
    if (igrow == numGrowIter) exit
    numVals = 0
    numPatPix = 0
    patchSum = 0.
    do iy = iyBordLow, iyBordHigh
      do ix = ixBordLow, ixBordHigh
        ixOffset = ix - ixCen
        iyOffset = iy - iyCen
        if (inList(ixOffset, iyOffset)) then
          numPatPix = numPatPix + 1
          patchSum = patchSum + array(ix, iy)
        elseif (adjacent(ixOffset, iyOffset) > 0 .and. numVals < LIMPATCH) then
          numVals = numVals + 1
          adjValue(numVals) = array(ix, iy)
        endif
      enddo
    enddo
    !
    ! Get the median to determine polarity and the percentile cutoff
    call rsSortFloats(adjValue, numVals)
    call rsMedianOfSorted(adjValue, numVals, adjMedian)
    polarity = sign(1., patchSum / max(1, numPatPix) - adjMedian)
    pctile = percentile
    if (polarity < 0) pctile = 1. - percentile
    icut = min(numVals, max(1, int(pctile * numVals - 0.5) + 1))
    cutoff = 2. * adjMedian - adjValue(icut)
    ! print *,adjMedian, polarity, icut, adjValue(icut), cutoff
    numAdded = 0
    !
    ! Added adjacent points above the cutoff to the patch
    do iy = iyBordLow, iyBordHigh
      do ix = ixBordLow, ixBordHigh
        ixOffset = ix - ixCen
        iyOffset = iy - iyCen
        if (adjacent(ixOffset, iyOffset) > 0 .and. .not.inList(ixOffset, iyOffset) .and. &
            polarity * (array(ix, iy) - cutoff) > 0. .and. numInObj < LIMPATCH) then
          inList(ixOffset, iyOffset) = .true.
          numInObj = numInObj + 1
          ixFix(numInObj) = ix
          iyFix(numInObj) = iy
          numAdded = numAdded + 1
        endif
      enddo
    enddo
    if (ifVerbose > 0) write(*,'(a,i2,i6,a)') 'Iteration', igrow, numAdded, &
        ' adjacent points added to patch'
    if (numAdded == 0) exit
  enddo
  !
  ! Mark higher level adjacent points to get a region of width "nborder"
  do igrow = 1, numPixBorder - ifIncludeAdj
    do iy = iyBordLow, iyBordHigh
      do ix = ixBordLow, ixBordHigh
        ixOffset = ix - ixCen
        iyOffset = iy - iyCen
        if (adjacent(ixOffset, iyOffset) == igrow .and. .not.inList(ixOffset, iyOffset)) &
            then
          do i = -1, 1
            do j = -1, 1
              if (adjacent(ixl + i, iyl + j) == 0) adjacent(ixl + i, iyl + j) = igrow + 1
            enddo
          enddo
        endif
      enddo
    enddo
  enddo

  ! total pixels in current area
  numPix = (iyBordHigh + 1 - iyBordLow) * (ixBordHigh + 1 - ixBordLow)
  !
  ! list of points is complete: fill regression matrix with data for all
  ! points outside the list.  the ixbordlo etc. should be correct
  !
  numPoints = 0
  nindep = iorder * (iorder + 3) / 2
  xsum = 0.
  minAdjacent = 2 - ifIncludeAdj
  do iy = iyBordLow, iyBordHigh
    do ix = ixBordLow, ixBordHigh
      ixOffset = ix - ixCen
      iyOffset = iy - iyCen
      nearEdge = ix - ixBordLow < numBordM1 .or. &
          ixBordHigh - ix < numBordM1 .or. &
          iy - iyBordLow < numBordM1 .or. &
          iyBordHigh - iy < numBordM1
      if (.not.inList(ixOffset, iyOffset) .and. &
          (nearEdge .or. adjacent(ixOffset, iyOffset) >= minAdjacent)) then
        numPoints = numPoints + 1
        if (nindep > 0) then
          if (numPoints > LIMPATCH) then
            if (.not. warned) write(*,'(/,a)') 'WARNING: CCDERASER - SOME ' &
                //'PATCHES ARE TOO LARGE FOR POLYNOMIAL FITS, USING MEAN ' &
                //'OF SURROUNDING PIXELS'
            warned = .true.
          else
            call polyTerm(ixOffset, iyOffset, iorder, rmat(1, numPoints))
            rmat(nindep + 1, numPoints) = array(ix, iy)
          endif
        endif
        xsum = xsum + array(ix, iy)
      endif
    enddo
  enddo
  !
  ! do regression or just get mean for order 0
  !
  if (ifVerbose > 0) write (*,104) numInObj, ixCen, iyCen, numPoints
104 format(/,i5,' points to fix at',2i6,',',i4,' points being fit')
  if (nindep > 0 .and. numPoints <= LIMPATCH) then
    ! call multr(xr, nindep+1, npnts, sx, ss, ssd, d, r, xm, sd, b, b1, c1, rsq , fra)
    call multRegress(rmat, matSize, 1, nindep, numPoints, 1, 0, b1, matSize, c1, xm, sd, &
        ssd)
  endif
  xsum = xsum / numPoints
  !
  ! replace points on list with values calculated from fit
  ! cannot truncate range by nbordm1 because could be on edge of image
  !
  do iy = iyBordHigh, iyBordLow, -1
    do ix = ixBordLow, ixBordHigh
      ixOffset = ix - ixCen
      iyOffset = iy - iyCen
      if (inList(ixOffset, iyOffset)) then
        if (nindep > 0 .and. numPoints <= LIMPATCH) then
          call polyTerm(ixOffset, iyOffset, iorder, vector)
          xsum = c1
          do i = 1, nindep
            xsum = xsum + b1(i) * vector(i)
          enddo
        endif
        array(ix, iy) = xsum
      endif
    enddo
  enddo
  !
  return
end subroutine cleanArea


! FILLBOUNDARYARRAYS puts a contour into X and Y boundary arrays and returns the
! min and max values.
!
subroutine fillBoundaryArrays(iobj, xbound, ybound, xmin, xmax, yMin, ymax)
  implicit none
  include 'model.inc90'
  real*4 xmin, xmax, yMin, ymax, xbound(*), ybound(*)
  integer*4 iobj, ip, ipt, ibase
  ibase = ibase_obj(iobj)
  xmin = 1.e20
  xmax = -1.e20
  yMin = xmin
  ymax = xmax
  !
  ! Put points in boundary array and get min/max
  do ip = 1, npt_in_obj(iobj)
    ipt = object(ibase + ip)
    xbound(ip) = p_coord(1, ipt)
    ybound(ip) = p_coord(2, ipt)
    xmin = min(xmin, xbound(ip))
    xmax = max(xmax, xbound(ip))
    yMin = min(yMin, ybound(ip))
    ymax = max(ymax, ybound(ip))
  enddo
  return
end subroutine fillBoundaryArrays


! CONVERTBOUNDARY Converts a boundary contour to a list of interior
! points, in the same contour
!
subroutine convertBoundary(iobj, nx, ny, xbound, ybound, xmin, xmax, yMin, ymax)
  implicit none
  include 'model.inc90'
  real*4 xmin, xmax, yMin, ymax, xbound(*), ybound(*), xx, yy, zz
  integer*4 iobj, ip, ipt, ixStart, iyStart, ixEnd, iyEnd, ibase, numInObj
  integer*4 nx, ny, iy, ix
  logical inside
  ibase = ibase_obj(iobj)
  zz = p_coord(3, object(ibase + 1))
  ibase_obj(iobj) = ibase_free
  numInObj = 0
  ixStart = max(1, nint(xmin - 2.))
  ixEnd = min(nx, nint(xmax + 2.))
  iyStart = max(1, nint(yMin - 2.))
  iyEnd = min(ny, nint(ymax + 2.))
  !
  ! Look at all pixels in range, add to object at new base
  do iy = iyStart, iyEnd
    do ix = ixStart, ixEnd
      xx = ix - 0.5
      yy = iy - 0.5
      if (inside(xbound, ybound, npt_in_obj(iobj), xx, yy)) then
        numInObj = numInObj + 1
        n_point = n_point + 1
        if (n_point > max_pt) call exitError( &
            'NOT ENOUGH MODEL ARRAY SPACE TO CONVERT BOUNDARY CONTOURS')
        ibase_free = ibase_free + 1
        object(ibase_free) = n_point
        p_coord(1, n_point) = xx
        p_coord(2, n_point) = yy
        p_coord(3, n_point) = zz
      endif
    enddo
  enddo
  npt_in_obj(iobj) = numInObj
  return
end subroutine convertBoundary


! ADDCIRCLETOPATCH adds a circle to the current patch, keeping track of
! the overall mins and maxes
!
subroutine addCircleToPatch(iobj, ipt, size, numInObj, ixFixMin, ixFixMax, iyFixMin, &
    iyFixMax)
  use ccdvars
  implicit none
  include 'model.inc90'
  integer*4 ixFixMin, ixFixMax, iyFixMin, iyFixMax
  integer*4 iobj, ipt, numInObj
  real*4 size, xcen, ycen, xx, yy
  integer*4 ip, ixStart, ixEnd, iyStart, iyEnd, ix, iy, ifInPatch, numInStart
  ip = object(ibase_obj(iobj) + ipt)
  xcen = p_coord(1, ip)
  ycen = p_coord(2, ip)
  ixStart = max(1, nint(xcen - size - 2.))
  ixEnd = min(nx, nint(xcen + size + 2.))
  iyStart = max(1, nint(ycen - size - 2.))
  iyEnd = min(ny, nint(ycen + size + 2.))
  numInStart = numInObj
  do iy = iyStart, iyEnd
    do ix = ixStart, ixEnd
      xx = ix - 0.5
      yy = iy - 0.5
      if ((xx - xcen)**2 + (yy - ycen)**2 <= size**2) then
        ifInPatch = 0
        if (ix >= ixFixMin .and. ix <= ixFixMax .and. iy >= iyFixMin .and. &
            iy <= iyFixMax) then
          do ip = 1, numInStart
            if (ix == ixFix(ip) .and. iy == iyFix(ip)) then
              ifInPatch = 1
              exit
            endif
          enddo
        endif
        if (ifInPatch == 0) then
          numInObj = numInObj + 1
          !
          ! This is not supposed to happen, but better to check...
          if (numInObj > LIMPATCH) call exitError( &
              'TOO MANY POINTS IN PATCH TO MERGE ANOTHER CIRCLE IN')
          ixFix(numInObj) = ix
          iyFix(numInObj) = iy
          ixFixMin = min(ixFixMin, ix)
          ixFixMax = max(ixFixMax, ix)
          iyFixMin = min(iyFixMin, iy)
          iyFixMax = max(iyFixMax, iy)
        endif
      endif
    enddo
  enddo
  return
end subroutine addCircleToPatch


! CONTOURAREA Measures the area of a contour
!
real*4 function contourArea(iobj)
  implicit none
  include 'model.inc90'
  integer*4 ipt, nextPt, ip1, ip2, numInObj, ibase, iobj
  contourArea = 0.
  numInObj = npt_in_obj(iobj)
  if (numInObj < 3) return
  ibase = ibase_obj(iobj)
  do ipt = 1, numInObj
    nextPt = mod(ipt, numInObj) + 1
    ip1 = object(ibase + ipt)
    ip2 = object(ibase + nextPt)
    contourArea = contourArea + (p_coord(2, ip2) + p_coord(2, ip1)) * &
        (p_coord(1, ip2) - p_coord(1, ip1))
  enddo
  contourArea = abs(contourArea * 0.5)
  return
end function contourArea


! TAPERINSIDECONT erases points inside of a large contour and tapers down on the
! inside from a border value to the mean value.
!
subroutine taperInsideCont(array, nx, ny, xbound, ybound, numInObj, xmin, xmax, &
    yMin, ymax, iferr)
  implicit none
  integer*4 nx, ny, numInObj, iferr
  real*4 array(nx, ny), xbound(*), ybound(*), xmin, xmax, yMin, ymax
  real*4 sum, segmentX, segmentY, vectorX, vectorY, xLine, yline, taper, dist, distMin
  real*4 t, tmin, dx, dy, fill, xx, yy, frac, taperSq, vecLen
  integer*4 numSum, ip, ipNext, ix, iy, ixf, iyf, numVecPts, ixStart, ixEnd
  integer*4 iyStart, iyEnd, ipMin, i
  logical inside
  taper = 8.
  taperSq = taper**2
  iferr = 1
  !
  ! First we need the mean outside the periphery
  sum = 0.
  numSum = 0
  do ip = 1, numInObj
    ipNext = mod(ip, numInObj) + 1
    segmentX = xbound(ipNext) - xbound(ip)
    segmentY = ybound(ipNext) - ybound(ip)
    vecLen = sqrt(segmentX**2 + segmentY**2)
    if (vecLen > 0.) then
      vectorX = 1.5 * segmentX / vecLen
      vectorY = 1.5 * segmentY / vecLen
      numVecPts = vecLen
      !
      ! Go to middle of line and test a point to one side
      xLine = xbound(ip) + segmentX * 0.5
      yline = ybound(ip) + segmentY * 0.5
      ix = nint(xLine + vectorY + 0.5)
      iy = nint(yline - vectorX + 0.5)
      if (.not.inside(xbound, ybound, numInObj, ix - 0.5, iy - 0.5)) then
        !
        ! If that wasn't inside, reverse direction and test that point and give up
        ! on segment if neither one is inside
        vectorX = -vectorX
        vectorY = -vectorY
        ix = nint(xLine + vectorY + 0.5)
        iy = nint(yline - vectorX + 0.5)
        if (.not.inside(xbound, ybound, numInObj, ix - 0.5, iy - 0.5)) cycle
      endif
      !
      ! Then loop along line and collect some points
      do i = 1, numVecPts
        xLine = xbound(ip) + (segmentX * i) / numVecPts
        yline = ybound(ip) + (segmentY * i) / numVecPts
        ix = nint(xLine + vectorY + 0.5)
        iy = nint(yline - vectorX + 0.5)
        if (ix > 0 .and. ix <= nx .and. iy > 0 .and. iy <= ny) then
          sum = sum + array(ix, iy)
          numSum = numSum + 1
        endif
      enddo
    endif
  enddo

  if (numSum < 3) return
  iferr = 0
  fill = sum / numSum
  ixStart = max(1, min(nx, floor(xmin - 0.5)))
  iyStart = max(1, min(ny, floor(yMin - 0.5)))
  ixEnd = max(1, min(nx, ceiling(xmax - 0.5)))
  iyEnd = max(1, min(ny, ceiling(ymax - 0.5)))
  do iy = iyStart, iyEnd
    do ix = ixStart, ixEnd
      xx = ix - 0.5
      yy = iy - 0.5
      if (inside(xbound, ybound, numInObj, xx, yy)) then
        !
        ! Find nearest distance to contour
        distMin = 1.e30
        t = 0.
        do ip = 1, numInObj
          ipNext = mod(ip, numInObj) + 1
          dx = xbound(ipNext) - xbound(ip)
          dy = ybound(ipNext) - ybound(ip)
          if (dx .ne. 0. .or. dy .ne. 0.) &
              t = ((xx - xbound(ip)) * dx + (yy - ybound(ip)) * dy) / (dx**2 + dy**2)
          t = min(1., max(0., t))
          dist = (xx - (xbound(ip) + t * dx))**2 + (yy - (ybound(ip) + t * dy))**2
          if (dist < distMin) then
            distMin = dist
            ipMin = ip
            tmin = t
          endif
        enddo
        !
        ! If it is close to contour, get pixel on other side
        array(ix, iy) = fill
        if (distMin < taperSq) then
          ipNext = mod(ipMin, numInObj) + 1
          dx = xbound(ipNext) - xbound(ipMin)
          dy = ybound(ipNext) - ybound(ipMin)
          xLine = xbound(ipMin) + dx * tmin
          yline = ybound(ipMin) + dy * tmin
          segmentX = xLine - xx
          segmentY = yline - yy
          vecLen = sqrt(segmentX**2 + segmentY**2)
          if (vecLen > 1.e-6) then
            xx = xLine + 1.2 * segmentX / vecLen
            yy = yline + 1.2 * segmentY / vecLen
            ixf = nint(xx + 0.5)
            iyf = nint(yy + 0.5)
            if (ixf > 0 .and. ixf <= nx .and. iyf > 0 .and. iyf <= ny) then
              frac = sqrt(distMin / taperSq)
              array(ix, iy) = frac * fill + (1. - frac) * array(ixf, iyf)
            endif
          endif
        endif
      endif
    enddo
  enddo
  return
end subroutine taperInsideCont



logical function typeOnList(itype, iTypeList, numTypeList)
  implicit none
  integer*4 iTypeList(*), itype, numTypeList, i
  typeOnList = .true.
  if (numTypeList == 1 .and. iTypeList(1) == -999) return
  do i = 1, numTypeList
    if (itype == iTypeList(i)) return
  enddo
  typeOnList = .false.
  return
end function typeOnList

! * * * * * *  TILTALIGN  * * * * * * *
!
! This program will solve for the displacements, rotations, tilts, and
! magnification differences relating a set of tilted views of an
! object.  It uses a set of fiducial points that have been identified
! in a series of views.  These input data are read from a model in
! which each fiducial point is a separate object or contour.
!
! See Man page for all details.
!
! David Mastronarde  March 1989
!
! $Id$
! Really old history at end of file
!
program tiltalign
  use alivar
  implicit none
  integer maxvar, maxh, maxtemp
  !
  integer*4, allocatable :: ninReal(:), igroup(:)
  integer*4, allocatable :: imodObj(:), imodCont(:)
  real*4, allocatable :: var(:), varErr(:), grad(:), h(:)
  real*8, allocatable :: sprod(:)
  character*8, allocatable :: varName(:)
  double precision error, errorScan(-20:20)
  real*4, allocatable :: tiltOrig(:), viewRes(:), xyzerr(:,:)
  integer*4, allocatable :: ninView(:), indSave(:), jptSave(:)
  real*4, allocatable :: errSave(:)
  real*4, allocatable :: viewErrsum(:), viewErrsq(:)
  real*4, allocatable :: viewMeanRes(:), viewSdRes(:)

  logical orderErr, nearbyErr
  character*320 modelFile, residualFile, pointFile, unadjTiltFile
  !
  real*4, allocatable :: fl(:,:,:)
  real*4 fa(2,3), fb(2,3), fc(2,3), fpstr(2,3)
  !
  real*4, allocatable :: xzfac(:), yzfac(:)
  real*4, allocatable :: allxyz(:,:)
  real*4, allocatable :: allxx(:), allyy(:)
  real*4, allocatable :: glbfl(:,:,:), glbxzfac(:), glbyzfac(:)
  integer*4, allocatable :: iallSecv(:), iallRealStr(:), listReal(:)
  integer*4, allocatable :: indAllReal(:), mapAllToLocal(:)
  integer*4, allocatable :: mapLocalToAll(:)
  integer*4, allocatable :: mallFileToView(:), mallViewToFile(:)
  !
  logical tooFewFid, useTarget, dirDone(-1:1)
  integer*4 ncycle/500/
  real*4 DTOR/0.0174532/
  !
  integer*4 numLocalRes, numSurface, iwhichOut, metroError, i, itry
  integer*4 inputAlf, mapAlfEnd, ifVarOut, ifResOut, ifXyzOut, ifLocal
  integer*4 iv, nvarSearch, nvarGeometric, index, nvarAngle, nvarScaled
  real*4 errCrit, facm, znew, xtiltNew, scaleXY, errSum, znewInput
  real*4 errSqsm, residErr, vwErrSum, vwErrSq, sxoz, szox, sxox, szoz
  real*4 xo, zo, xShift, zShift, rollPoints, cosTheta, sinTheta, xtmp, compInc, compAbs
  integer*4 nviewAdd, ninViewSum, ivst, ivnd, iunit2, numUnknownTot, iunit
  real*4 unknownRatio, tiltOut, zmin, zmax, zmiddle, dysum
  real*4 dyavg, offmin, dxmid, offsum, dxtry, xtFac, xtConst, off, yShift
  integer*4 j, iuAngle, iuXtilt, ndxtry, iunLocal, nAllRealPt, localv, ll
  integer*4 mapAlfStart, nord, jpt, numPatchX, numPatchY, ivt, ipt, nAllView
  integer*4 nxpMin, nypMin, minfidtot, minfidsurf, ifxyzfix, nAllProjPt
  real*4 errMean, errSd, errnosd, tiltMax, fixedMax, xsum, ysum, zsum
  integer*4 idxPatch, idyPatch, ipatchX, ipatchy, ixsPatch, iysPatch
  integer*4 nxp, nyp, minSurf, nbot, ntop, ixmin, ixmax, iymin, iymax, kk
  integer*4 nprojpt, minTiltView, ncompSearch, mapTiltStart, ifBTSearch, ifOriginalZ
  real*4 xcen, ycen, ffinal, dxmin, tmp, tiltNew, fixedDum, tiltAdd, allBorder
  integer*4 ixtry, itmp, iord, ixPatch, iyPatch, ierr, ifZfac, ifDoRobust
  integer*4 nxTarget, nyTarget, minInitError, minResRobust, minLocalResRobust
  real*4 xpmin, ypmin, xdelt, projStrFactor, projStrAxis
  real*4 dmat(9), xtmat(9), ytmat(9), prmat(4), rmat(9), cosTmp, sinTmp
  real*4 afac, bfac, cfac, dfac, efac, ffac, cosAlf, sinAlf, cosBet, rotScanErrCrit
  real*4 sinBet, cosDel, sinDel, denom, unknownRatio2, cos2rot, sin2rot
  real*4 a11, a12, a21, a22, xzOther, yzOther, errsumLocal, errLocalMin
  real*4 errLocalMax, beamInv(9), beamMat(9), cosBeam, sinBeam, allYmax, rotEntered
  real*4 binStepIni, binStepFinal, scanStep, allXmin, allXmax, allYmin, rotIncForInit
  real*8 pmat(9), wgtErrSum, wallTime, wallStart, wgtSum
  integer*4 imageBinned, numUnknownTot2, ifDoLocal, ninThresh, numInitSteps
  integer*4 numWgtTotal, numWgtZero, numWgt1, numWgt2, numWgt5, maxDelWgtBelowCrit
  integer*4 maxRobustOneCycle, numOneCycle, numTotCycles, maxTotCycles, numBelowCrit
  real*4 robustTotCycleFac, delWgtMeanCrit, delWgtMaxCrit, rmsScale
  real*4 wgtErrMean, wgtErrSumLocal, wgtErrLocalMin, wgtErrLocalMax
  character*20 message
  character*320 concat
  !
  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean
  integer*4 PipGetString, PipGetFloat, PipGetTwoIntegers
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  tiltalign
  !
  integer numOptions
  parameter (numOptions = 108)
  character*(40 * numOptions) options(1)
  options(1) = &
      ':ModelFile:FN:@:ImageFile:FN:@:ImageSizeXandY:IP:@:ImageOriginXandY:FP:@'// &
      ':ImagePixelSizeXandY:FP:@:ImagesAreBinned:I:@:OutputModelFile:FN:@'// &
      ':OutputResidualFile:FN:@:OutputModelAndResidual:FN:@'// &
      ':OutputTopBotResiduals:FN:@:OutputFidXYZFile:FN:@:OutputTiltFile:FN:@'// &
      ':OutputUnadjustedTiltFile:FN:@:OutputXAxisTiltFile:FN:@'// &
      ':OutputTransformFile:FN:@:OutputZFactorFile:FN:@:IncludeStartEndInc:IT:@'// &
      ':IncludeList:LI:@:ExcludeList:LI:@:RotationAngle:F:@:SeparateGroup:LIM:@'// &
      'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@tiltfile:TiltFile:FN:@'// &
      'angles:TiltAngles:FAM:@:AngleOffset:F:@:ProjectionStretch:B:@'// &
      ':BeamTiltOption:I:@:FixedOrInitialBeamTilt:F:@:RotOption:I:@'// &
      ':RotDefaultGrouping:I:@:RotNondefaultGroup:ITM:@:RotationFixedView:I:@'// &
      ':LocalRotOption:I:@:LocalRotDefaultGrouping:I:@:LocalRotNondefaultGroup:ITM:@'// &
      ':TiltOption:I:@:TiltFixedView:I:@:TiltSecondFixedView:I:@'// &
      ':TiltDefaultGrouping:I:@:TiltNondefaultGroup:ITM:@:LocalTiltOption:I:@'// &
      ':LocalTiltFixedView:I:@:LocalTiltSecondFixedView:I:@'// &
      ':LocalTiltDefaultGrouping:I:@:LocalTiltNondefaultGroup:ITM:@'// &
      ':MagReferenceView:I:@:MagOption:I:@:MagDefaultGrouping:I:@'// &
      ':MagNondefaultGroup:ITM:@:LocalMagReferenceView:I:@:LocalMagOption:I:@'// &
      ':LocalMagDefaultGrouping:I:@:LocalMagNondefaultGroup:ITM:@'// &
      ':CompReferenceView:I:@:CompOption:I:@:CompDefaultGrouping:I:@'// &
      ':CompNondefaultGroup:ITM:@:XStretchOption:I:@:XStretchDefaultGrouping:I:@'// &
      ':XStretchNondefaultGroup:ITM:@:LocalXStretchOption:I:@'// &
      ':LocalXStretchDefaultGrouping:I:@:LocalXStretchNondefaultGroup:ITM:@'// &
      ':SkewOption:I:@:SkewDefaultGrouping:I:@:SkewNondefaultGroup:ITM:@'// &
      ':LocalSkewOption:I:@:LocalSkewDefaultGrouping:I:@'// &
      ':LocalSkewNondefaultGroup:ITM:@:XTiltOption:I:@:XTiltDefaultGrouping:I:@'// &
      ':XTiltNondefaultGroup:ITM:@:LocalXTiltOption:I:@:LocalXTiltDefaultGrouping:I:@'// &
      ':LocalXTiltNondefaultGroup:ITM:@:ResidualReportCriterion:F:@'// &
      ':SurfacesToAnalyze:I:@:MetroFactor:F:@:MaximumCycles:I:@:RobustFitting:B:@'// &
      ':KFactorScaling:I:@:MinWeightGroupSizes:IP:@:AxisZShift:F:@:AxisXShift:F:@'// &
      ':LocalAlignments:B:@:OutputLocalFile:FN:@:NumberOfLocalPatchesXandY:IP:@'// &
      ':TargetPatchSizeXandY:IP:@:MinSizeOrOverlapXandY:FP:@'// &
      ':MinFidsTotalAndEachSurface:IP:@:FixXYZCoordinates:B:@:LocalOutputOptions:IT:@'// &
      ':RotMapping:IAM:@:LocalRotMapping:IAM:@:TiltMapping:IAM:@'// &
      ':LocalTiltMapping:IAM:@:MagMapping:IAM:@:LocalMagMapping:IAM:@'// &
      ':CompMapping:IAM:@:XStretchMapping:IAM:@:LocalXStretchMapping:IAM:@'// &
      ':SkewMapping:IAM:@:LocalSkewMapping:IAM:@:XTiltMapping:IAM:@'// &
      ':LocalXTiltMapping:IAM:@param:ParameterFile:PF:@help:usage:B:'
  !
  maxtemp = 10000
  numLocalRes = 50
  xyzFixed = .false.
  tooFewFid = .false.
  kfacRobust = 4.685
  minResRobust = 100
  minLocalResRobust = 65
  smallWgtThreshold = 0.5
  delWgtMeanCrit = 0.001
  delWgtMaxCrit = 0.01
  maxDelWgtBelowCrit = 4
  maxRobustOneCycle = 10
  robustTotCycleFac = 3.
  ifDoRobust = 0
  incrGmag = 0
  incrDmag = 0
  incrSkew = 0
  incrRot = 0
  incrTilt = 0
  incrAlf = 0
  dxmin = 0.
  dyavg = 0.
  ifZfac = 0
  ifOriginalZ = 0
  imageBinned = 1
  ifDoLocal = 0
  binStepIni = 1.
  binStepFinal = 0.25
  scanStep = 0.02
  ninThresh = 3
  rotIncForInit = 10.
  rotScanErrCrit = 5.
  smallWgtMaxFrac = 0.25
  !
  ! set this to 1 to get inputs for X - axis tilting from sequential input
  !
  inputAlf = 0
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'tiltalign', &
      'ERROR: TILTALIGN - ', .true., 3, 1, 1, numOptArg, &
      numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0
  !
  ! Do temporary allocations for imodObj, imodCont, listz needed here
  allocate(iallRealStr(maxtemp), indAllReal(maxtemp), listReal(maxtemp), stat = ierr)
  call memoryError(ierr, 'TEMPORARY OVERSIZED ARRAYS')
  !
  iuXtilt = inputAlf
  call input_model(iallRealStr, indAllReal, nprojpt, iwhichOut, xcen, ycen, xdelt, &
      listReal, maxtemp, modelFile, residualFile, pointFile, iuAngle, iuXtilt, pipinput)
  !
  ! Get things into the actual arrays now
  allocate(imodObj(maxReal), imodCont(maxReal), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR OBJECT/CONTOUR #s')
  imodObj(1:nrealPt) = iallRealStr(1:nrealPt)
  imodCont(1:nrealPt) = indAllReal(1:nrealPt)
  mapViewToFile(1:nview) = listReal(1:nview)
  deallocate(iallRealStr, indAllReal, listReal)
  !
  ! Do big allocations
  ! print *,maxReal, maxView, maxProjPt
  allocate(ninReal(maxReal), igroup(maxReal), &
      tiltOrig(nfileViews), viewRes(maxView), xyzerr(3, maxReal), &
      ninView(maxView), indSave(maxProjPt), jptSave(maxProjPt), &
      errSave(maxProjPt), viewErrsum(maxView), viewErrsq(maxView), &
      viewMeanRes(maxView), viewSdRes(maxView), &
      fl(2, 3, maxView), xzfac(maxView), yzfac(maxView), &
      allxyz(3, maxReal), allxx(maxProjPt), allyy(maxProjPt), &
      glbfl(2, 3, maxView), glbxzfac(nfileViews), glbyzfac(nfileViews), &
      iallSecv(maxProjPt), iallRealStr(maxReal), listReal(maxReal), &
      indAllReal(maxReal), mapAllToLocal(maxView), mapLocalToAll(maxView), &
      mallFileToView(nfileViews), mallViewToFile(maxView), stat = ierr)
  call memoryError(ierr, 'MAIN PROGRAM ARRAYS')
  !
  ! Allocate the variable arrays to maximum plausible size
  maxvar = 7 * maxView + 3 * maxReal
  allocate(var(maxvar), varErr(maxvar), grad(maxvar), varName(7 * maxView), stat = ierr)
  call memoryError(ierr, 'VARIABLE ARRAYS')

  do i = 1, nrealPt
    listReal(i) = i
    ninReal(i) = irealStr(i + 1) - irealStr(i)
  enddo
  call countNumInView(listReal, nrealPt, irealStr, isecView, nview, ninView)
  call input_vars(var, varName, inputAlf, nvarSearch, nvarAngle, nvarScaled, &
      minTiltView, ncompSearch, 0, mapTiltStart, mapAlfStart, mapAlfEnd, ifBTSearch, &
      tiltOrig, tiltAdd, pipinput, ninView, ninThresh, rotEntered)
  !
  facm = 0.5
  if (pipinput) then
    errCrit = 3.0
    numSurface = 0
    ncycle = 1000
    znewInput = 0.
    xtiltNew = 0.
    ierr = PipGetFloat('ResidualReportCriterion', errCrit)
    ierr = PipGetInteger('SurfacesToAnalyze', numSurface)
    ierr = PipGetFloat('MetroFactor', facm)
    ierr = PipGetInteger('MaximumCycles', ncycle)
    ierr = PipGetBoolean('RobustFitting', ifDoRobust)
    ierr = PipGetTwoIntegers('MinWeightGroupSizes', minResRobust, minLocalResRobust)
    if (PipGetFloat('KFactorScaling', cosTmp) .eq. 0)  &
        kfacRobust = kfacRobust * cosTmp
    ierr = PipGetFloat('AxisZShift', znewInput)
    ierr = PipGetBoolean('ShiftZFromOriginal', ifOriginalZ)
    ierr = PipGetFloat('AxisXShift', xtiltNew)
    ierr = PipGetInteger('ImagesAreBinned', imageBinned)
    imageBinned = max(1, imageBinned)
    ierr = PipGetBoolean('LocalAlignments', ifDoLocal)

  else
    write(*,'(1x,a,/,a,$)') 'Criterion # of sd above mean residual' &
        //' error to report (+ for ', &
        'relative to absolute mean,  - for relative to mean '// &
        'of nearby views): '
    read(5,*) errCrit
    !
    write(*,'(1x,a,$)') '1 or 2 to derive a tilt angle assuming'// &
        ' points are on 1 or 2 surfaces: '
    read(5,*) numSurface
    !
    write(*,'(1x,a,f5.2,i5,a,$)') 'Factor for METRO, limit on # '// &
        'of cycles [', facm, ncycle, ']: '
    read(5,*) facm, ncycle
    !
    if (iwhichOut >= 0) then
      !
      ! find out what to do with z value of axis
      !
      print *,'Z shift in tilt axis relative to centroid,'
      write(*,'(1x,a,$)') &
          '         or 1000 to shift to middle of z range: '
      read(5,*) znewInput
      !
      ! get new position of tilt axis in x
      !
      write(*,'(1x,a,$)') &
          'New X position of tilt axis relative to center: '
      read(5,*) xtiltNew
    endif
  endif
  !
  if (nint(znewInput) .ne. 1000) znewInput = znewInput / imageBinned
  xtiltNew = xtiltNew / imageBinned
  orderErr = .true.
  nearbyErr = errCrit < 0.
  errCrit = abs(errCrit)
  !
  ! scale the points down to range of 1.0: helps convergence
  !
  ifVarOut = 1
  ifResOut = 1
  ifXyzOut = 1
  ifLocal = 0
  metroError = 0
  do i = 1, nrealPt
    indAllReal(i) = i
  enddo
  !
  scaleXY = 0.
  do i = 1, nprojpt
    scaleXY = max(scaleXY, abs(xx(i)), abs(yy(i)))
  enddo
  do i = 1, nprojpt
    xx(i) = xx(i) / scaleXY
    yy(i) = yy(i) / scaleXY
  enddo
  !
  ! call new solveXyzd to get initial values of x, y, z
  ! Allocate a double array because of potential alignment issues using real array as
  ! double in a subroutine, deallocate when done
  allocate(sprod((3 * min(maxReal, maxRealForDirectInit))**2 / 2), stat=ierr)
  call memoryError(ierr, 'ARRAY FOR SOLVEXYZD')

  call remap_params(var)

  wallStart = wallTime()
  if (nrealPt > 1) then
    numInitSteps = ceiling(90. / rotIncForInit)
    dirDone = .false.
    !
    ! Scan around the given initial rotation, going in each direction until an
    ! error is reached that is a criterion ratio bigger than the current minimum
    do itry = 0, numInitSteps
      do iv = -1, 1, 2
        index = iv * itry
        if (itry > 0 .and. .not. dirDone(iv)) then
          if (errorScan(iv * (itry - 1)) > &
              rotScanErrCrit * errorScan(minInitError)) dirDone(iv) = .true.
        endif
        if (dirDone(iv) .or. index == numInitSteps) cycle
        call solveXyzd(xx, yy, isecView, irealStr, nview, nrealPt, tilt, rot, &
            gmag, comp, xyz, dxy, dtor * rotIncForInit * index, sprod, error, ierr)
        if (itry == 0) then
          if (ierr .ne. 0) &
              call errorExit('SOLVING FOR INITIAL VALUES OF X/Y/Z COORDINATES', 0)
          minInitError = 0
          errorScan(0) = error
          exit
        endif
        if (ierr .ne. 0) then
          errorScan(index) = 1.1 * rotScanErrCrit * errorScan(minInitError)
        else
          errorScan(index) = error
        endif
        if (errorScan(index) < errorScan(minInitError)) minInitError = index
      enddo
    enddo
    !
    ! Give warning if minimum is far from expected
    if (abs(minInitError * rotIncForInit) > 15.) &
        write(*,137) rotEntered + minInitError * rotIncForInit, rotEntered
137 format(/, 'WARNING: BASED ON INITIAL FITTING ERRORS, THE ROTATION ANGLE SEEMS', &
        ' TO BE CLOSER TO',f7.1,' THAN TO THE SPECIFIED ANGLE,',f7.1,/, &
        'WARNING: AN INCORRECT ROTATION ANGLE WILL THROW OFF PREALIGNMENT,', &
        ' BEADTRACKING, AND THIS ALIGNMENT', /)
  endif
  !
  ! Redo initialization at 0 increment
  call solveXyzd(xx, yy, isecView, irealStr, nview, nrealPt, tilt, rot, &
      gmag, comp, xyz, dxy, 0., sprod, error, ierr)
  !write(*,'(a,f8.2)') 'Initialization time', 1000 * (wallTime() - wallStart)
  !call flush(6)
  ! write(*, '(3(2f9.4,f8.4))') ((xyz(i, iv), i = 1, 3), iv = 1, nrealPt)
  deallocate(sprod, stat=ierr)
  !
  ! Get the h array big enough for the solution
  maxvar = nvarSearch + 3 * nrealPt
  maxh = (maxvar + 3) * maxvar
  allocate(h(maxh), stat = ierr)
  call memoryError(ierr, 'ARRAY FOR H MATRIX')
  !
  call alignAndOutputResults()
  !
  ! shift the fiducials to real positions in X and Y for xyz output file
  ! and for possible use with local alignments
  ! Continue to use zero - centroid xyz for find_surfaces but output
  ! a 3D model with real positions also
  !
  nAllRealPt = nrealPt
  do i = 1, nrealPt
    iallRealStr(i) = irealStr(i)
    allxyz(1, i) = xyz(1, i) - dxmin + xcen
    allxyz(2, i) = xyz(2, i) - dyavg + ycen
    allxyz(3, i) = xyz(3, i) - znew
  enddo
  iallRealStr(nrealPt + 1) = irealStr(nrealPt + 1)

  !
  if (pointfile .ne. ' ') then
    call dopen(13, pointfile, 'new', 'f')
    write(13, '(i4,3f10.2,i7,i5,a,f12.5,a,2i6)') 1, (allxyz(i, 1), i = 1, 3), &
        imodObj(1), imodCont(1), ' Pix:', xdelt, ' Dim:', nint(2. * xcen), nint(2. * ycen)
    write(13, '(i4,3f10.2,i7,i5)') (j, (allxyz(i, j), i = 1, 3), &
        imodObj(j), imodCont(j), j = 2, nrealPt)
    close(13)
  endif
  !
  ! analyze for surfaces if desired.  Find the biggest tilt and the
  ! biggest fixed tilt, get recommended new value for the biggest fixed
  ! tilt if it is not too small
  !
  tiltMax = 0.
  fixedMax = 0.
  do iv = 1, nview
    if (abs(tilt(iv)) > abs(tiltMax)) tiltMax = tilt(iv)
    if (mapTilt(iv) == 0 .and. abs(tilt(iv)) > abs(fixedMax)) &
        fixedMax = tilt(iv)
  enddo
  if (fixedMax >= 5.) tiltMax = fixedMax
  if (numSurface > 0) call find_surfaces(xyz, nrealPt, numSurface, tiltMax, iunit2, &
      tiltNew, igroup, ncompSearch, tiltAdd, znew, znewInput, imageBinned)
  call write_xyz_model(modelFile, allxyz, igroup, nrealPt)
  !
  ! Write separate residual outputs now that surfaces are known
  !
  if (pipinput .and. numSurface > 1) then
    if (PipGetString('OutputTopBotResiduals', modelFile) == 0) then
      residualFile = concat(modelFile, '.botres')
      do j = 1, 2
        nbot = 0
        do jpt = 1, nrealPt
          if (igroup(jpt) == j) then
            nbot = nbot + irealStr(jpt + 1) - irealStr(jpt)
          endif
        enddo
        if (nbot > 0) then
          call dopen(13, residualFile, 'new', 'f')
          write(13, '(i6,a)') nbot, ' residuals'
          do jpt = 1, nrealPt
            if (igroup(jpt) == j) then
              do i = irealStr(jpt), irealStr(jpt + 1) - 1
                write(13, '(2f10.2,i5,3f8.2)') xx(i) + xcen, yy(i) + ycen, &
                    mapViewToFile(isecView(i)) - 1, xresid(i), yresid(i)
              enddo
            endif
          enddo
          close(13)

        endif
        residualFile = concat(modelFile, '.topres')
      enddo
    endif
  endif
  !
  ! Ask about local alignments
  !
  if (pipinput) then
    ifLocal = ifDoLocal
  else
    write(*,'(1x,a,$)') '1 to do series of local alignments, 0 to exit: '
    read(5,*,iostat = ierr) ifLocal
    if (ierr .ne. 0) ifLocal = 0
  endif
  if (ifLocal .ne. 0) then
    call setupAndDoLocalAlignments()
    close(iunLocal)
    if (.not. tooFewFid) then
      if (ifResOut > 0) print *
      if (ifDoRobust > 0) then
        write(*,'(/,a)')'Summary of robust weighting in local areas:'
        write(*,321)numWgtTotal, numWgtZero, numWgt1, numWgt2, numWgt5,  &
            (100. * numWgt5) / numWgtTotal
321     format(i6,' weights:',i5,' are 0,',i5,' are < .1,',i5,' are < .2,',i6, &
            ' (',f4.1,'%) are < .5')
      endif
      write(*,119) errsumLocal / (numPatchX * numPatchY), errLocalMin, errLocalMax
119   format(/,' Residual error local mean:  ',f9.3,'    range', f8.3, ' to',f8.3)
      if (ifDoRobust > 0 )write(*,1191) wgtErrsumLocal / (numPatchX * numPatchY), wgtErrLocalMin, wgtErrLocalMax
1191   format(/,' Weighted error local mean:  ',f9.3,'    range', f8.3, ' to',f8.3)
    endif
  endif
  close(7)
  if (metroError .ne. 0) print *,'WARNING:', metroError, ' MINIMIZATION ERRORS OCCURRED'
  if (tooFewFid) call errorexit( &
      'Minimum numbers of fiducials are too high - check if '// &
      'there are enough fiducials on the minority surface', 0)

  call exit(0)

CONTAINS

  ! Run the alignment for global or local area, compute transforms and other
  ! output, and output results

  subroutine alignAndOutputResults()
    real*4 atand, sind, cosd
    integer*4 nearest_view
    !
    ! pack the xyz into the var list
    nvarGeometric = nvarSearch
    do jpt = 1, nrealPt - 1
      do i = 1, 3
        nvarSearch = nvarSearch + 1
        var(nvarSearch) = xyz(i, jpt)
      enddo
    enddo
    !
    ! Make sure the h array is big enough
    maxvar = nvarGeometric + 3 * nrealPt
    if ((maxvar + 3) * maxvar > maxh) then
      deallocate(h)
      maxh = (maxvar + 3) * maxvar
      allocate(h(maxh), stat = ierr)
      call memoryError(ierr, 'ARRAY FOR H MATRIX')
    endif
    !
    ! Do beam tilt search only for global alignment
    robustWeights = .false.
    rmsScale = scaleXY**2 / nprojpt
    if (ifBTSearch == 0 .or. ifLocal > 0) then
      call runMetro(nvarSearch, var, varErr, grad, h, ifLocal, facm, ncycle, 0, &
          rmsScale, fFinal, i, metroError)
    else
      call searchBeamTilt(beamTilt, binStepIni, binStepFinal, scanStep, &
          nvarSearch, var, varErr, grad, h, ifLocal, facm, ncycle, &
          rmsScale, fFinal, i, metroError)
    endif
    !
    ! If doing robust fitting, just restart the start with all current values
    if (ifDoRobust .ne. 0) then
      maxTotCycles = abs(ncycle) * robustTotCycleFac
      numTotCycles = 0
      numOneCycle = 0
      numBelowCrit = 0
      jpt = minResRobust
      index = maxWgtRings
      if (ifLocal .ne. 0) then
        jpt = minLocalResRobust
        index = 1
      else
        numWgtTotal = 0
        numWgtZero = 0
        numWgt1 = 0
        numWgt2 = 0
        numWgt5 = 0
      endif
      call findMedianResidual()
      call setupWeightGroups(index, jpt, minTiltView, ierr)
      if (ierr .ne. 0) call exitError('TOO FEW DATA POINTS TO DO ROBUST FITTING')
      robustWeights = .true.
      do while (numTotCycles .lt. maxTotCycles)
        errSave(1:nprojpt) = weight(1:nprojpt)
        call computeWeights(indSave, jptSave, xyzerr)
        call runMetro(nvarSearch, var, varErr, grad, h, ifLocal, facm, -abs(ncycle), 1, &
            rmsScale, fFinal, i, metroError)
        numTotCycles = numTotCycles + i
        if (i <= 1) then
          numOneCycle = numOneCycle + 1
        else
          numOneCycle = 0
        endif
        jpt = 0
        index = 0
        ipt = 0
        iv = 0
        errMean = 0.
        errSd = 0.
        do i = 1, nprojpt
          if (weight(i) < 0.5) then
            jpt = jpt + 1
            errMean = errMean + abs(weight(i) - errSave(i))
          endif
          if (weight(i) == 0.) index = index + 1
          if (weight(i) < 0.1) ipt = ipt + 1
          if (weight(i) < 0.2) iv = iv + 1
          errSd = max(errSd, abs(weight(i) - errSave(i)))
        enddo
        if (jpt > 0) errMean = errMean / jpt
        if (errSd < delWgtMaxCrit .or. errMean < delWgtMeanCrit) then
          numBelowCrit = numBelowCrit + 1
        else
          numBelowCrit = 0
        endif
        if (numBelowCrit >= maxDelWgtBelowCrit .or. numOneCycle >= maxRobustOneCycle) then
          exit
        endif
      enddo
      write(*,'(a,i5,t48,a,t61,f14.6)')' Total cycles for robust fitting:', &
          numTotCycles, 'Final   F : ', sqrt(fFinal * rmsScale)
      if (numTotCycles > maxTotCycles)  &
          write(*,'(/,a,i5,a,/)') 'WARNING: Robust fitting ended after', &
          numTotCycles,' cycles without meeting convergence criteria'
      write(*,'(a,2f8.4)')' Final mean and max weight change',errMean, errSd
      write(*,321) nprojpt, index, ipt, iv, jpt, (100. * jpt) / nprojpt
321     format(i6,' weights:',i5,' are 0,',i5,' are < .1,',i5,' are < .2,',i6, &
            ' (',f4.1,'%) are < .5',/)
      if (ifLocal .ne. 0) then
        numWgtTotal = numWgtTotal + nprojpt
        numWgtZero = numWgtZero + index
        numWgt1 = numWgt1 + ipt
        numWgt2 = numWgt2 + iv
        numWgt5 = numWgt5 + jpt
      endif
    endif

    !
    ! unscale all the points, dx, dy, and restore angles to degrees
    !
    index = 0
    do i = 1, nvarAngle
      var(i) = var(i) / dtor
      index = index + 1
      varErr(i) = (sqrt(h(index * nvarSearch - nvarSearch + index)) / nvarSearch) / dtor
    enddo
    !
    do i = nvarAngle + 1, nvarScaled
      index = index + 1
      varErr(i) = sqrt(h(index * nvarSearch - nvarSearch + index)) / nvarSearch
    enddo
    !
    do i = mapAlfStart, mapAlfEnd
      var(i) = var(i) / dtor
      index = index + 1
      varErr(i) = (sqrt(h(index * nvarSearch - nvarSearch + index)) / nvarSearch) / dtor
    enddo
    ! leave projection skew and beam tilt as radians
    do i = mapAlfEnd + 1, nvarGeometric
      index = index + 1
      varErr(i) = sqrt(h(index * nvarSearch - nvarSearch + index)) / nvarSearch
    enddo
    !
    do i = 1, nrealPt
      do j = 1, 3
        xyz(j, i) = xyz(j, i) * scaleXY
        index = index + 1
        if (i < nrealPt) xyzerr(j, i) = &
            scaleXY * sqrt(h(index * nvarSearch - nvarSearch + index)) / nvarSearch
      enddo
    enddo
    !
    errSum = 0.
    errSqsm = 0.
    wgtErrSum = 0.
    wgtSum = 0.
    do i = 1, nview
      viewRes(i) = 0.
      ninView(i) = 0
      viewErrsum(i) = 0.
      viewErrsq(i) = 0.
    enddo
    do i = 1, nprojpt
      xx(i) = xx(i) * scaleXY
      yy(i) = yy(i) * scaleXY
      xresid(i) = xresid(i) * scaleXY
      yresid(i) = yresid(i) * scaleXY
      residErr = sqrt(xresid(i)**2 + yresid(i)**2)
      wgtErrSum = wgtErrSum + residErr * sqrt(weight(i))
      wgtSum = wgtSum + sqrt(weight(i))
      iv = isecView(i)
      ninView(iv) = ninView(iv) + 1
      viewErrsum(iv) = viewErrsum(iv) + residErr
      viewErrsq(iv) = viewErrsq(iv) + residErr**2
    enddo
    !
    do iv = 1, nview
      dxy(1, iv) = dxy(1, iv) * scaleXY
      dxy(2, iv) = dxy(2, iv) * scaleXY
      !
      ! save global solution now
      !
      if (ifLocal == 0) then
        glbAlf(iv) = alf(iv)
        glbTilt(iv) = tilt(iv)
        glbRot(iv) = rot(iv)
        glbSkew(iv) = skew(iv)
        glbGmag(iv) = gmag(iv)
        glbDmag(iv) = dmag(iv)
      endif
      rot(iv) = rot(iv) / dtor
      tilt(iv) = tilt(iv) / dtor
      skew(iv) = skew(iv) / dtor
      alf(iv) = alf(iv) / dtor
      viewRes(iv) = viewErrsum(iv) / ninView(iv)
      errSum = errSum + viewErrsum(iv)
      errSqsm = errSqsm + viewErrsq(iv)
      !
      ! find mean and sd residual of minimum number of points in a local
      ! group of views
      !
      nviewAdd = 1
      ninViewSum = 0
      do while(ninViewSum < numLocalRes .and. nviewAdd < nview)
        ivst = max(1, iv - nviewAdd / 2)
        ivnd = min(nview, ivst + nviewAdd - 1)
        ninViewSum = 0
        do ivt = ivst, ivnd
          ninViewSum = ninViewSum + ninView(ivt)
        enddo
        nviewAdd = nviewAdd + 1
      enddo
      vwErrSum = 0.
      vwErrSq = 0.
      do ivt = ivst, ivnd
        vwErrSum = vwErrSum + viewErrsum(ivt)
        vwErrSq = vwErrSq + viewErrsq(ivt)
      enddo
      viewMeanRes(iv) = vwErrSum / ninViewSum
      viewSdRes(iv) = sqrt((vwErrSq - vwErrSum**2 / ninViewSum) / (ninViewSum - 1))
    enddo
    !
    ! convert the projection stretch to a matrix
    ! (This only works directly into fpstr if it is symmetric)
    !
    call fill_proj_matrix(projStrRot, projSkew, fpstr, cosTmp, sinTmp, &
        cos2rot, sin2rot)
    call amat_to_rotmagstr(fpstr, xo, zo, projStrFactor, projStrAxis)
    !
    ! if doing local solution, need to find rotation to match
    ! the original set of points
    !
    if (ifLocal .ne. 0) then
      sxoz = 0.
      szox = 0.
      sxox = 0.
      szoz = 0.
      do i = 1, nrealPt
        xo = allxyz(1, indAllReal(i)) - xcen - xShift
        zo = allxyz(3, indAllReal(i)) - zShift
        sxox = sxox + xo * xyz(1, i)
        sxoz = sxoz + xo * xyz(3, i)
        szox = szox + zo * xyz(1, i)
        szoz = szoz + zo * xyz(3, i)
      enddo
      rollPoints = 0.
      if ((sxox + szoz) > 1.e-5 * abs(sxoz - szox)) &
          rollPoints = atand((sxoz - szox) / (sxox + szoz))
      !
      ! rolls the points, reduce this amount from the tilts
      !
      cosTheta = cosd(rollPoints)
      sinTheta = sind(rollPoints)
      do i = 1, nrealPt
        xtmp = xyz(1, i) * cosTheta + xyz(3, i) * sinTheta
        xyz(3, i) = -xyz(1, i) * sinTheta + xyz(3, i) * cosTheta
        xyz(1, i) = xtmp
      enddo
      do i = 1, nview
        tilt(i) = tilt(i) - rollPoints
      enddo
    endif
    !
    iunit2 = 7
    if (iwhichOut > 0) iunit2 = 6
    compInc = 1.
    compAbs = 1.
    numUnknownTot = nvarGeometric + 3 * (nrealPt - 1)
    if (xyzFixed) numUnknownTot = nvarGeometric
    numUnknownTot2 = numUnknownTot + 2 * (nview - 1)
    unknownRatio = (2. * nprojpt) / max(numUnknownTot, 1)
    unknownRatio2 = (2. * nprojpt) / max(numUnknownTot2, 1)
    do iunit = 6, iunit2
      write (iunit, 113) nview, nvarGeometric, nrealPt, nprojpt, &
          2 * nprojpt, numUnknownTot2, unknownRatio2
113   format(i4,' views,',i5,' geometric variables,',i5, &
          ' 3-D points,',i6,' projection points',/, &
          '  Ratio of total measured values to all unknowns =', &
          i6,'/',i4,' =',f7.2)
      if (ifVarOut .ne. 0) then
        if (iunit .ne. 6) write(iunit, '(/,21x,a)') &
            'Geometric variable values and errors'
        if (iunit .ne. 6) write(iunit, '(3(f10.4,f7.4,a9,1x))', err = 85) &
            (var(i), varErr(i), varName(i), i = 1, nvarGeometric)
85      if (ncompSearch == 0) then
          if (mapProjStretch > 0) write(iunit, '(/,a,f8.2,a)') &
              'Projection skew is', projSkew / dtor, ' degrees'
          if (mapBeamTilt > 0 .or. ifBTSearch .ne. 0) &
              write(iunit, '(/,a,f8.2,a)') 'Beam tilt angle is', &
              beamTilt / dtor, ' degrees'
          if (mapAlfStart > mapAlfEnd) then
            if (ifLocal == 0) write(iunit, '(/,a,f7.2)') &
                ' At minimum tilt, rotation angle is', rot(minTiltView)
            write(iunit, '(/,a)') ' view   rotation    tilt    '// &
                'deltilt     mag      dmag      skew    mean resid'
            do i = 1, nview
              j = mapViewToFile(i)
              write(iunit, '(i4,2f10.1,f10.2,2f10.4,2f10.2)') &
                  j, rot(i), tilt(i), tilt(i) - tiltOrig(j), &
                  gmag(i), dmag(i), skew(i), viewRes(i)
            enddo
          elseif (ifRotFix == - 1 .or. ifRotFix == - 2) then
            if (ifRotFix == - 1) write(iunit, '(/,a,f7.2)') &
                ' Fixed rotation angle is', rot(1)
            if (ifRotFix == - 2) write(iunit, '(/,a,f7.2)') &
                ' Overall rotation angle is', rot(1)
            write(iunit, '(/,a)') ' view     tilt    deltilt   '// &
                '  mag      dmag      skew     X-tilt   mean resid'
            do i = 1, nview
              j = mapViewToFile(i)
              write(iunit, '(i4,f10.1,f10.2,2f10.4,3f10.2)') &
                  j, tilt(i), tilt(i) - tiltOrig(j), gmag(i), dmag(i), &
                  skew(i), alf(i), viewRes(i)
            enddo
          else
            if (mapAlfEnd > mapAlfStart) &
                write(iunit, '(/,a)') 'WARNING: SOLUTIONS FOR BOTH '// &
                'ROTATION AND X-AXIS TILT ARE VERY UNRELIABLE'
            write(iunit, '(/,a)') ' view rotation  tilt  deltilt' &
                //'    mag     dmag    skew   X-tilt  mean resid'
            do i = 1, nview
              j = mapViewToFile(i)
              write(iunit, '(i4,2f8.1,f8.2,2f9.4,3f8.2)') j, rot(i), &
                  tilt(i), tilt(i) - tiltOrig(j), gmag(i), dmag(i), skew(i), &
                  alf(i), viewRes(i)
            enddo
          endif
        else
          write(iunit, '(/,a)') ' view   rotation    tilt      mag' &
              //'    comp-inc  comp-abs    dmag      skew'
          do i = 1, nview
            !
            ! for 0 tilts, output same compression values as last view
            !
            if (tilt(i) .ne. 0.) then
              compInc = comp(i)
              compAbs = compInc * gmag(i)
            endif
            write(iunit, '(i4,2f10.1,4f10.4,f10.2)') mapViewToFile(i) &
                , rot(i), tilt(i), &
                gmag(i), compInc, compAbs, dmag(i), skew(i)
          enddo
        endif
        write(iunit,*)
        if ((iuAngle == 0 .or. iunit .ne. 6) .and. ifLocal == 0) &
            write(iunit, 116) (tilt(i), i = 1, nview)
116     format(' ANGLES',10f7.2)
        if (ncompSearch > 0) write(iunit, 117) (comp(i), i = 1, nview)
117     format(' COMPRESS',10f7.4)
      endif
    enddo
    if (ifXyzOut .ne. 0) then
      write(iunit2, 111)
111   format(/,21x,'3-D point coordinates (with centroid zero)' &
          ,/,'   #',7x,'X',9x,'Y',9x,'Z',6x,'obj  cont   mean resid')
      do j = 1, nrealPt
        vwErrSum = 0.
        do i = irealStr(j), irealStr(j + 1) - 1
          vwErrSum = vwErrSum + sqrt((xresid(i)**2 + yresid(i)**2))
        enddo
        write(iunit2, '(i4,3f10.2,i7,i5,f12.2)', err = 86) &
            indAllReal(j), (xyz(i, j), i = 1, 3), imodObj(indAllReal(j)), &
            imodCont(indAllReal(j)), vwErrSum / ninReal(j)
      enddo
    endif
    !
    ! get min, max and midpoint of z values
    !
86  zmin = 1.e10
    zmax = -1.e10
    do ipt = 1, nrealPt
      zmin = min(zmin, xyz(3, ipt))
      zmax = max(zmax, xyz(3, ipt))
    enddo
    zmiddle = (zmax + zmin) / 2.
    if (ifLocal == 0) write(*,'(/,a,f8.2)') &
        ' Midpoint of Z range relative to centroid in Z:', zmiddle

    ifAnyAlf = 0
    unadjTiltFile = ' '
    if (mapAlfEnd > mapAlfStart) ifAnyAlf = 1
    !
    ! Output unmodified tilt angles
    if (pipinput .and. ifLocal == 0) then
      if (PipGetString('OutputUnadjustedTiltFile', unadjTiltFile) == 0) then
        call dopen(13, unadjTiltFile, 'new', 'f')
        do i = 1, nfileViews
          tiltOut = tiltOrig(i)
          if (mapFileToView(i) .ne. 0) tiltOut = tilt(mapFileToView(i))
          write(13, '(f7.2)') tiltOut
        enddo
        close(13)
      endif
    endif
    !
    ! Modify angles to account for beam tilt
    !
    if (beamTilt .ne. 0.) then
      do iv = 1, nview
        call convert_for_beamtilt(alf(iv), tilt(iv), rot(iv), beamTilt, &
            ifAnyAlf)
      enddo
    endif
    !
    ! output lists of angles that are complete for all file views
    !
    if (ifLocal == 0 .and. iuAngle .ne. 0) then
      do i = 1, nfileViews
        tiltOut = tiltOrig(i)
        if (mapFileToView(i) .ne. 0) tiltOut = tilt(mapFileToView(i))
        write(iuAngle, '(f7.2)') tiltOut
      enddo
      close(iuAngle)
      if (iuXtilt == 0 .and. (ifAnyAlf .ne. 0 .or. beamTilt .ne. 0.) .and. &
          unadjTiltFile == ' ') write(*,122)
122   format(/,'WARNING: THE SOLUTION INCLUDES X-AXIS TILTS THAT CHANGE', &
          ' THROUGH THE SERIES',/,'WARNING: X-AXIS TILTS SHOULD BE OUTPUT', &
          ' TO A FILE AND FED TO THE TILT PROGRAM')
    endif
    !
    if (ifLocal == 0 .and. iuXtilt .ne. 0) then
      do i = 1, nfileViews
        tiltOut = 0.
        if (mapFileToView(i) .ne. 0) tiltOut = alf(mapFileToView(i))
        write(iuXtilt, '(f7.2)') tiltOut
      enddo
      close(iuXtilt)
    endif
    !
    ! compute xforms, shift the dy's to minimize total shift, allow
    ! user to shift dx's (and tilt axis) similarly or specify new location
    ! of tilt axis
    ! shift axis in z by making proper shifts in x
    !
    if (iwhichOut >= 0) then
      do iv = 1, nview
        !
        ! To compute transform, first get the coefficients of the full
        ! projection.  Assume 0 beam tilt, it is already corrected
        !
        call fill_dist_matrix(gmag(iv), dmag(iv), skew(iv) * dtor, comp(iv), &
            1, dmat, cosDel, sinDel)
        call fill_beam_matrices(0., beamInv, beamMat, cosBeam, &
            sinBeam)
        call fill_xtilt_matrix(alf(iv) * dtor, ifAnyAlf, xtmat, cosAlf, &
            sinAlf)
        call fill_ytilt_matrix(tilt(iv) * dtor, ytmat, cosBet, sinBet)
        call fill_proj_matrix(projStrRot, projSkew, prmat, cosTmp, sinTmp, &
            cos2rot, sin2rot)
        call fill_rot_matrix(rot(iv) * dtor, rmat, cosTmp, sinTmp)
        call matrix_to_coef(dmat, xtmat, beamInv, ytmat, beamMat, prmat, &
            rmat, afac, bfac, cfac, dfac, efac, ffac)
        !
        ! Solve for transformation that maps 1, 0, 0 to cos beta, 0
        ! and 0, 1, 0 to sin alf * sin beta, cos alf
        !
        denom = bfac * dfac - afac * efac
        fl(1, 1, iv) = (dfac * sinAlf * sinBet - efac * cosBet) / denom
        fl(1, 2, iv) = (bfac * cosBet - afac * sinAlf * sinBet) / denom
        fl(2, 1, iv) = dfac * cosAlf / denom
        fl(2, 2, iv) = -afac * cosAlf / denom
        fl(1, 3, iv) = -(fl(1, 1, iv) * dxy(1, iv) + fl(1, 2, iv) * dxy(2, iv))
        fl(2, 3, iv) = -(fl(2, 1, iv) * dxy(1, iv) + fl(2, 2, iv) * dxy(2, iv))
        !
        ! Compute Z - dependent factors to add to X and Y in backprojection
        ! This method does not depend on distortion model:
        ! Compute coefficients of distortion plus tilts, solve for
        ! transformation that aligns images to that, determine Z component
        ! of projection equation to aligned images, and subtract component
        ! expected to be applied in backprojection
        !
        do i = 1, 9
          pmat(i) = dmat(i)
        enddo
        call mat_product(pmat, 3, 3, xtmat, 3, 3)
        call mat_product(pmat, 3, 3, ytmat, 2, 3)
        denom = pmat(2) * pmat(4) - pmat(1) * pmat(5)
        a11 = (pmat(4) * sinAlf * sinBet - pmat(5) * cosBet) / denom
        a12 = (pmat(2) * cosBet - pmat(1) * sinAlf * sinBet) / denom
        a21 = pmat(4) * cosAlf / denom
        a22 = -pmat(1) * cosAlf / denom
        xzfac(iv) = (a11 * pmat(3) + a12 * pmat(6)) / comp(iv) - &
            cosAlf * sinBet
        yzfac(iv) = (a21 * pmat(3) + a22 * pmat(6)) / comp(iv) + sinAlf
        !
        ! Alternate based on solving equations from type 1 distortion model
        !
        yzOther = -sinDel * sinBet / (cosDel * cosBet)
        xzOther = (gmag(iv) / ((gmag(iv) + dmag(iv)) * cosDel) - 1. + &
            sinAlf * yzOther) * sinBet / cosAlf
        ! write(*,'(6f11.6)') xzfac(iv), xzOther, xzfac(iv) - xzOther, &
        ! yzfac(iv), yzOther, yzfac(iv) - yzOther
        !
        ! The old way, and validation by inverse multiplication
!!$        c
!!$        c         set the distortion matrix into fa and the rotation matrix into fb
!!$        c
!!$        fa(1, 1) = (gmag(iv) + dmag(iv)) * cosd(skew(iv))
!!$        fa(2, 1) = (gmag(iv) + dmag(iv)) * sind(skew(iv)) / cosd(tilt(iv))
!!$        fa(2, 2) = gmag(iv)
!!$        fa(1, 2) = 0.
!!$        fa(1, 3) = 0.
!!$        fa(2, 3) = 0.
!!$        cosphi = cosd(rot(iv))
!!$        sinphi = sind(rot(iv))
!!$        fb(1, 1) = cosphi
!!$        fb(1, 2) = -sinphi
!!$        fb(2, 1) = sinphi
!!$        fb(2, 2) = cosphi
!!$        fb(1, 3) = 0.
!!$        fb(2, 3) = 0.
!!$        fpstr(1, 3) = 0.
!!$        fpstr(2, 3) = 0.
!!$        c
!!$        c         get product, then add the dx's and dy's, then invert
!!$        c
!!$        call xfmult(fa, fpstr, fc)
!!$        call xfcopy(fc, fa)
!!$        call xfmult(fa, fb, fc)
!!$        fc(1, 3) = dxy(1, iv)
!!$        fc(2, 3) = dxy(2, iv)
!!$        call xfinvert(fc, fb)
!!$
!!$        call xfmult(fc, fl(1, 1, iv), fa)
!!$        c       call xfinvert(fc, fl(1, 1, iv))
!!$        call xfwrite(6, fl(1, 1, iv), *299)
!!$        call xfwrite(6, fb, *299)
!!$        call xfwrite(6, fa, *299)
!!$299     continue
        ! Set up to fit shifts against sines and determine z shift from original
        h(iv) = fl(1, 3, iv)
        h(iv + nview) = sind(tilt(iv))
      enddo

      ! If Z change is relative to original position, determine component needed to
      ! get back to original
      znew = znewInput
      if (ifLocal == 0 .and. ifOriginalZ .ne. 0 .and. nint(znew) .ne. 1000) then
        call lsfit(h(1 + nview), h, nview, zShift, dxmid, offmin)
        znew = znew + zShift
      endif
      if (nint(znew) == 1000) znew = zmiddle
      if (ifLocal .ne. 0) znew = -zShift

      ! adjust dx by the factor needed to shift axis in Z and set up for working on Y
      dysum = 0.
      do iv = 1, nview
        fl(1, 3, iv) = fl(1, 3, iv) - znew * sind(tilt(iv))
        h(iv) = 1. - cosd(tilt(iv))
        dysum = dysum + fl(2, 3, iv)
      enddo
      dyavg = dysum / nview
      if (ifLocal == 0) then
        !
        ! find value of X shift that minimizes overall loss of image - do
        ! exhaustive scan centered on dx of the minimum tilt image
        !
        offmin = 1.e10
        dxmid = fl(1, 3, minTiltView)
        !
        ! DNM 11 / 10 / 01: eliminate real variable do loop in deference to f95
        ! do dxtry = dxmid - 0.1 * xcen, dxmid + 0.1 * xcen, 0.1
        !
        ndxtry = 2. * xcen
        do ixtry = 0, ndxtry
          dxtry = dxmid + 0.1 * (ixtry - xcen)
          offsum = 0.
          xtFac = xtiltNew + dxtry
          xtConst = xtiltNew - xtFac
          do iv = 1, nview
            off = abs(fl(1, 3, iv) + xtConst + xtFac * h(iv)) - xcen * h(iv)
            if (off > 0.) offsum = offsum + off
          enddo
          if (offsum < offmin) then
            offmin = offsum
            dxmin = dxtry
          endif
        enddo
        xtFac = xtiltNew + dxmin
        xtConst = xtiltNew - xtFac
        !
        ! Put tilt axis at the new position, and get the final dy to
        ! add up to 0.
        !
        do iv = 1, nview
          fl(2, 3, iv) = fl(2, 3, iv) - dyavg
          fl(1, 3, iv) = fl(1, 3, iv) + xtConst + xtFac * h(iv)
        enddo
        !
        ! output a transform for each file view, find the nearest one
        ! for non - included view
        !
        do iv = 1, nfileViews
          i = nearest_view(iv)
          call xfwrite(7, fl(1, 1, i),*99)
99      enddo
        if (residualFile .ne. ' ') then
          call dopen(13, residualFile, 'new', 'f')
          write(13, '(i6,a)') nprojpt, ' residuals'
          do i = 1, nprojpt
            write(13, '(2f10.2,i5,3f8.2)') xx(i) + xcen, yy(i) + ycen, &
                mapViewToFile(isecView(i)) - 1, &
                xresid(i), yresid(i)
          enddo
          close(13)
        endif
        !
        ! output the z factors if option requested
        !
        if (pipinput) then
          if (PipGetString('OutputZFactorFile', residualFile) == 0) then
            ifZfac = 1
            call dopen(13, residualFile, 'new', 'f')
            do iv = 1, nfileViews
              i = nearest_view(iv)
              glbxzfac(iv) = xzfac(i)
              glbyzfac(iv) = yzfac(i)
              write(13, '(2f12.6)') xzfac(i), yzfac(i)
            enddo
            close(13)
          endif
        endif
      else
        !
        ! If local, the procedure now is first to process each relevant item
        ! for the local views, in place or into a different array (grad)
        ! This is mixing local views and global values, so mapping from local
        ! to global view number is needed to access the global values
        ! Then expand this array to the global views.
        ! Then create output for all file views, taking the global or null
        ! value as appropriate for an excluded view
        !
        ! Do this for tilt angles, alpha if they exist, z factors, and xforms
        !
        do iv = 1, nview
          grad(iv) = tilt(iv) - glbTilt(mapLocalToAll(iv)) / dtor
        enddo
        call expandLocalToAll(grad, 1, 1, nview, nAllView, mapAllToLocal)
        do i = 1, nfileViews
          iv = mallFileToView(i)
          h(i) = 0.
          if (iv > 0) h(i) = grad(i)
        enddo
        write(iunLocal, '(10f7.2)') (h(i), i = 1, nfileViews)
        ! write(6, '(f8.2)') (tilt(iv), iv = 1, nview)
        if (mapAlfStart <= mapAlfEnd .or. beamTilt .ne. 0) then
          do iv = 1, nview
            grad(iv) = alf(iv) - glbAlf(mapLocalToAll(iv)) / dtor
          enddo
          call expandLocalToAll(grad, 1, 1, nview, nAllView, mapAllToLocal)
          do i = 1, nfileViews
            iv = mallFileToView(i)
            h(i) = 0.
            if (iv > 0) h(i) = grad(iv)
          enddo
          write(iunLocal, '(10f7.2)') (h(i), i = 1, nfileViews)
        endif
        !
        ! Z factors if they were output globally
        !
        if (ifZfac .ne. 0) then
          call expandLocalToAll(xzfac, 1, 1, nview, nAllView, mapAllToLocal)
          call expandLocalToAll(yzfac, 1, 1, nview, nAllView, mapAllToLocal)
          do i = 1, nfileViews
            iv = mallFileToView(i)
            h(i) = glbxzfac(i)
            grad(i) = glbyzfac(i)
            if (iv > 0) then
              h(i) = xzfac(iv)
              grad(i) = yzfac(iv)
            endif
          enddo
          write(iunLocal, '(6f12.6)') (h(i), grad(i), i = 1, nfileViews)
        endif
        !
        ! add the shifts to the dx and dy to get transforms that
        ! work to get back to the original point positions.
        ! Compose the inverse of an adjusting transform
        !
        do iv = 1, nview
          fl(1, 3, iv) = fl(1, 3, iv) + xShift * cosd(tilt(iv))
          fl(2, 3, iv) = fl(2, 3, iv) + yShift
          ! call xfwrite(6, fl(1, 1, iv),*199)
          call xfinvert(glbfl(1, 1, mapLocalToAll(iv)), fa)
          call xfmult(fa, fl(1, 1, iv), fb)
          call xfinvert(fb, fl(1, 1, iv))
        enddo
        do i = 1, 6
          call expandLocalToAll(fl, 6, i, nview, nAllView, mapAllToLocal)
        enddo
        do i = 1, nfileViews
          iv = mallFileToView(i)
          if (iv > 0) then
            call xfcopy(fl(1, 1, iv), fc)
          else
            call xfunit(fc, 1.)
          endif
          call xfwrite(iunLocal, fc,*199)
199     enddo
        ! If we need to restore the maps and isecView
!!$          if (nview < nAllView) then
!!$c
!!$c             Restore all the maps now too, and correct isecView
!!$            do jpt = 1, nrealPt
!!$              do i = irealStr(jpt), irealStr(jpt + 1) - 1
!!$                isecView(i) = mapLocalToAll(isecView(i))
!!$              enddo
!!$            enddo
!!$            do iv = 1, nfileViews
!!$              mapFileToView(iv) = mallFileToView(iv)
!!$            enddo
!!$            print *,'nview, nallview', nview, nAllView
!!$            nview = nAllView
!!$            do iv = 1, nview
!!$              mapViewToFile(iv) = mallViewToFile(iv)
!!$            enddo
!!$          endif
      endif
    endif
    !
    ! print out points with high residuals
    !
    errMean = errSum / nprojpt
    errSd = sqrt((errSqsm - errSum**2 / nprojpt) / (nprojpt - 1))
    if (ifDoLocal == 0) then
      write(*,118) errMean, errSd
118   format(/,' Residual error mean and sd:  ',2f8.3,4x,a,2i4,a)
      if (ifDoRobust .ne. 0) write(*,1181) wgtErrSum / wgtSum
1181  format(' Residual error weighted mean:',f8.3,12x,a,2i4,a)
    else if (ifLocal == 0) then
      write(*,118) errMean, errSd, '(Global)'
      if (ifDoRobust .ne. 0) write(*,1181) wgtErrSum / wgtSum, '(Global)'
    else
      write(*,118) errMean, errSd, '(Local area', ipatchX, ipatchy, ')'
      errsumLocal = errsumLocal + errMean
      errLocalMin = min(errLocalMin, errMean)
      errLocalMax = max(errLocalMax, errMean)
      if (ifDoRobust .ne. 0) then
        wgtErrMean = wgtErrSum / wgtSum
        write(*,1181) wgtErrMean, '(Local area', ipatchX, ipatchy, ')'
        wgtErrsumLocal = wgtErrsumLocal + wgtErrMean
        wgtErrLocalMin = min(wgtErrLocalMin, wgtErrMean)
        wgtErrLocalMax = max(wgtErrLocalMax, wgtErrMean)
      endif
    endif
    if (ifResOut > 0) then
      message = ' '
      if (ifDoRobust .ne. 0) message = '   Weights'
      write(*,112) trim(message)
      !
      ! DEPENDENCY WARNING: Beadfixer relies on the # # ... line up to the
      ! second X
      !
112   format(/,9x,'Projection points with large residuals',/, &
          ' obj  cont  view   index coordinates      residuals', &
          '        # of',/, &
          '   #     #     #      X         Y        X        Y', &
          '        S.D.',a)
      nord = 0
      do jpt = 1, nrealPt
        do i = irealStr(jpt), irealStr(jpt + 1) - 1
          if (nearbyErr) then
            iv = isecView(i)
            errnosd = (sqrt(xresid(i)**2 + yresid(i)**2) - &
                viewMeanRes(iv)) / viewSdRes(iv)
          else
            errnosd = (sqrt(xresid(i)**2 + yresid(i)**2) - errMean) / errSd
          endif
          if (errnosd > errCrit) then
            if (orderErr) then
              nord = nord + 1
              errSave(nord) = errnosd
              indSave(nord) = i
              jptSave(nord) = jpt
            else
              if (ifDoRobust == 0) then
                write(*,114) imodObj(indAllReal(jpt)), &
                    imodCont(indAllReal(jpt)), mapViewToFile(isecView(i)) &
                    , xx(i) + xcen , yy(i) + ycen, xresid(i), yresid(i), errnosd
              else
                write(*,114) imodObj(indAllReal(jpt)), imodCont(indAllReal(jpt)),  &
                    mapViewToFile(isecView(i)) , xx(i) + xcen , yy(i) + ycen, xresid(i), &
                    yresid(i), errnosd, weight(i)
              endif
114           format(i4,2i6,2f10.2,3f9.2,f9.4)
            endif
          endif
        enddo
      enddo
      if (orderErr) then
        do i = 1, nord - 1
          do j = i + 1, nord
            if (errSave(i) < errSave(j)) then
              tmp = errSave(i)
              errSave(i) = errSave(j)
              errSave(j) = tmp
              itmp = indSave(i)
              indSave(i) = indSave(j)
              indSave(j) = itmp
              itmp = jptSave(i)
              jptSave(i) = jptSave(j)
              jptSave(j) = itmp
            endif
          enddo
        enddo
        do iord = 1, nord
          i = indSave(iord)
          if (ifDoRobust == 0) then
            write(*,114) imodObj(indAllReal(jptSave(iord))), &
                imodCont(indAllReal(jptSave(iord))), mapViewToFile(isecView(i)), &
                xx(i) + xcen, yy(i) + ycen, xresid(i), yresid(i), errSave(iord)
          else
            write(*,114) imodObj(indAllReal(jptSave(iord))), &
                imodCont(indAllReal(jptSave(iord))), mapViewToFile(isecView(i)), &
                xx(i) + xcen, yy(i) + ycen, xresid(i), yresid(i), errSave(iord), weight(i)
          endif
        enddo
      endif
    endif
  end subroutine alignAndOutputResults


  ! Get parameters for doing local alignments, set them up, and loop on the local
  ! areas

  subroutine setupAndDoLocalAlignments()
    integer*4 PipGetTwoIntegers, PipGetTwoFloats, PipGetThreeIntegers
    ifDoLocal = ifLocal
    if (iwhichOut < 0) call errorexit( &
        'SOLUTION TRANSFORMS MUST BE OUTPUT TO DO LOCAL ALIGNMENTS', 0)
    errsumLocal = 0.
    errLocalMin = 1.e10
    errLocalMax = -10.
    wgtErrSumLocal = 0.
    wgtErrLocalMin = 1.e10
    wgtErrLocalMax = -10.
    ifVarOut = 0
    ifResOut = 0
    ifXyzOut = 0
    numPatchX = 5
    numPatchY = 5
    xpmin = 0.5
    ypmin = 0.5
    minfidtot = 8
    minfidsurf = 3
    ifxyzfix = 0
    iunLocal = 9
    nxTarget = 700
    nyTarget = 700
    if (pipinput) then
      if (PipGetString('OutputLocalFile', modelFile) .ne. 0) call errorexit &
          ('NO OUTPUT FILE FOR LOCAL TRANSFORMS SPECIFIED', 0)
      itmp = PipGetTwoIntegers('NumberOfLocalPatchesXandY', numPatchX, numPatchY)
      kk = PipGetTwoIntegers('TargetPatchSizeXandY', nxTarget, nyTarget)
      ierr = PipGetTwoFloats('MinSizeOrOverlapXandY', xpmin, ypmin)
      ierr = PipGetTwoIntegers('MinFidsTotalAndEachSurface', &
          minfidtot, minfidsurf)
      ierr = PipGetBoolean('FixXYZCoordinates', ifxyzfix)
      ierr = PipGetThreeIntegers('LocalOutputOptions', ifVarOut, &
          ifXyzOut, ifResOut)
      if (itmp == 0 .and. kk == 0) call errorexit('YOU CANNOT ENTER '// &
          'BOTH A NUMBER OF LOCAL PATCHES AND A TARGET SIZE', 0)
      useTarget = itmp > 0
      if (useTarget .and. (xpmin > 1. .or. ypmin > 1)) call errorexit &
          ('YOU CANNOT ENTER A MINIMUM PATCH SIZE WITH A TARGET SIZE', 0)
    else
      !
      useTarget = .false.
      write(*,'(1x,a,$)') &
          'Name of output file for local transformations: '
      read(5, '(a)') modelFile
      !
      write(*,'(1x,a,$)') 'Number of patches in X and Y: '
      read(5,*) numPatchX, numPatchY
      write(*,'(1x,a,/,a,$)') 'Enter either the minimum size of '// &
          'patches in X and Y (values > 1) or the', &
          'minimum fractional overlap between patches in'// &
          ' X and Y (values < 1): '
      read(5,*) xpmin, ypmin
      write(*,'(1x,a,$)') 'Minimum total # of fiducials, minimum '// &
          'on one surface if two surfaces: '
      read(5,*) minfidtot, minfidsurf
      write(*,'(1x,a,$)') '1 to fix XYZ coordinates to global '// &
          'solution, 0 to solve for them also: '
      read(5,*) ifxyzfix
      write(*,'(1x,a,/,a,$)') 'Enter 1 for full output of variables,' &
          //' 1 for output of XYZ coordinates,', ' and 1 for output' &
          //' of points with high residuals (0 for no output): '
      read(5,*) ifVarOut, ifXyzOut, ifResOut
    endif
    !
    call dopen(iunLocal, modelFile, 'new', 'f')
    ifLocal = 1
    xyzFixed = ifxyzfix .ne. 0
    !
    ! set for incremental solution - could be input as option at this point
    !
    incrDmag = 1
    incrGmag = 1
    incrSkew = 1
    incrTilt = 1
    incrRot = 1
    incrAlf = 1
    !
    ! save transforms, scale angles back to radians (global solution
    ! already saved)
    !
    do iv = 1, nview
      call xfcopy(fl(1, 1, iv), glbfl(1, 1, iv))
      tilt(iv) = glbTilt(iv)
      mallViewToFile(iv) = mapViewToFile(iv)
    enddo
    do i = 1, nfileViews
      mallFileToView(i) = mapFileToView(i)
    enddo
    nAllView = nview
    !
    nAllProjPt = nprojpt
    do i = 1, nprojpt
      allxx(i) = xx(i)
      allyy(i) = yy(i)
      iallSecv(i) = isecView(i)
    enddo
    ! write(*,121)
    ! 121    format(/,11x,'Absolute 3-D point coordinates' &
    ! ,/,'   #',7x,'X',9x,'Y',9x,'Z')
    ! write(*,'(i4,3f10.2)', err = 86) &
    ! (j, (allxyz(i, j), i = 1, 3), j = 1, nrealPt)

    if (useTarget) then
      !
      ! If using a target, use the real extent of the data, get the number
      ! of patches that fill the extent at the target size, then get the
      ! real size with the defined overlap
      allXmin = 1.e10
      allXmax = -1.e10
      allYmin = 1.e10
      allYmax = -1.e10
      allBorder = 5.
      do i = 1, nAllRealPt
        allXmin = min(allXmin, allxyz(1, i) - allBorder)
        allXmax = max(allXmax, allxyz(1, i) + allBorder)
        allYmin = min(allYmin, allxyz(2, i) - allBorder)
        allYmax = max(allYmax, allxyz(2, i) + allBorder)
      enddo
      ! print *, allXmin, allXmax , allYmin, allYmax
      !
      numPatchX = (allXmax - allXmin - nxTarget) / (nxTarget * (1. - xpmin)) + 1
      numPatchX = max(2, numPatchX)
      nxpMin = (allXmax - allXmin) / (numPatchX - xpmin * (numPatchX - 1))
      if (nxpMin > 1.05 * nxTarget) then
        numPatchX = numPatchX + 1
        nxpMin = (allXmax - allXmin) / (numPatchX - xpmin * (numPatchX - 1))
      endif
      idxPatch = (allXmax - allXmin - nxpMin) / max(1, numPatchX - 1) + 1
      ixsPatch = allXmin + nxpMin / 2
      !
      numPatchY = (allYmax - allYmin - nyTarget) / (nyTarget * (1. - ypmin)) + 1
      numPatchY = max(2, numPatchY)
      nypMin = (allYmax - allYmin) / (numPatchY - ypmin * (numPatchY - 1))
      if (nypMin > 1.05 * nyTarget) then
        numPatchY = numPatchY + 1
        nypMin = (allYmax - allYmin) / (numPatchY - ypmin * (numPatchY - 1))
      endif
      idyPatch = (allYmax - allYmin - nypMin) / max(1, numPatchY - 1) + 1
      iysPatch = allYmin + nypMin / 2
      write(*,120) nint(allXmax - allXmin - 2. * allBorder), nint(allYmax - &
          allYmin - 2. * allBorder), numPatchX, numPatchY, nxpMin, nypMin
120   format(/,'Extent of fiducials is',i6,' and',i6,' pixels in X and Y', &
          /,'Doing',i3,' by',i3,' local areas, minimum size',i5,' x',i5)
    else
      !
      ! legacy behavior with # of patches entered: get the minimum patch
      ! size from full size of image area
      !
      numPatchX = max(2, numPatchX)
      numPatchY = max(2, numPatchY)
      if (xpmin > 1.) then
        nxpMin = xpmin
      else
        nxpMin = 2 * xcen / (numPatchX - xpmin * (numPatchX - 1))
      endif
      if (ypmin > 1.) then
        nypMin = ypmin
      else
        nypMin = 2 * ycen / (numPatchY - ypmin * (numPatchY - 1))
      endif
      !
      ! set up starting patch locations and intervals
      !
      idxPatch = (nint(2 * xcen) - nxpMin) / max(1, numPatchX - 1)
      idyPatch = (nint(2 * ycen) - nypMin) / max(1, numPatchY - 1)
      ixsPatch = nxpMin / 2
      iysPatch = nypMin / 2
    endif
    !
    ! LOOP ON LOCAL REGIONS
    !
    do ipatchy = 1, numPatchY
      do ipatchX = 1, numPatchX
        ixPatch = ixsPatch + (ipatchX - 1) * idxPatch
        iyPatch = iysPatch + (ipatchy - 1) * idyPatch
        !
        ! find the points whose real X and Y coordinates are within the bounds
        ! of the patch; expand the patch if necessary to achieve the minimum
        ! number of fiducials.  Load points from the "all" arrays into the
        ! current arrays
        !
        nxp = nxpMin - 40
        nyp = nypMin - 40
        nrealPt = 0
        minSurf = 0
        do while (nxp < 4 * xcen .and. nyp < 4 * ycen .and. &
            (nrealPt < minfidtot .or. &
            (numSurface >= 2 .and. minSurf < minfidsurf)))
          nxp = nxp + 40
          nyp = nyp + 40
          nrealPt = 0
          nbot = 0
          ntop = 0
          ixmin = ixPatch - nxp / 2
          ixmax = ixmin + nxp
          iymin = iyPatch - nyp / 2
          iymax = iymin + nyp
          do i = 1, nAllRealPt
            if (allxyz(1, i) >= ixmin .and. allxyz(1, i) <= ixmax .and. &
                allxyz(2, i) >= iymin .and. allxyz(2, i) <= iymax) then
              nrealPt = nrealPt + 1
              indAllReal(nrealPt) = i
              if (numSurface >= 2) then
                if (igroup(i) == 1) nbot = nbot + 1
                if (igroup(i) == 2) ntop = ntop + 1
              endif
            endif
          enddo
          minSurf = min(nbot, ntop)
        enddo
        if (nxp >= 4 * xcen .and. nyp >= 4 * ycen) then
          tooFewFid = .true.
          return
        endif
        !
        ! Get count of points in each view so empty views can be eliminated, then
        ! get mapping from all views to remaining views in this local area
        call countNumInView(indAllReal, nrealPt, iallRealStr, iallSecv, nAllView, &
            ninView)
        localv = 0
        do iv = 1, nfileViews
          mapFileToView(iv) = 0
        enddo
        do iv = 1, nAllView
          if (ninView(iv) > 0) then
            localv = localv + 1
            mapAllToLocal(iv) = localv
            mapLocalToAll(localv) = iv
            mapViewToFile(localv) = mallViewToFile(iv)
            mapFileToView(mallViewToFile(iv)) = localv
            tilt(localv) = glbTilt(iv)
          else
            mapAllToLocal(iv) = 0
          endif
        enddo
        nview = localv
        !
        ! Now load the coordinate data with these local view numbers
        nprojpt = 0
        do ll = 1, nrealPt
          listReal(ll) = ll
          i = indAllReal(ll)
          irealStr(ll) = nprojpt + 1
          do j = 1, ninReal(i)
            nprojpt = nprojpt + 1
            kk = J + iallRealStr(i) - 1
            xx(nprojpt) = allxx(kk)
            yy(nprojpt) = allyy(kk)
            isecView(nprojpt) = mapAllToLocal(iallSecv(kk))
          enddo
        enddo
        irealStr(nrealPt + 1) = nprojpt + 1
        call countNumInView(listReal, nrealPt, irealStr, isecView, nview, &
            ninView)

        call input_vars(var, varName, inputAlf, nvarSearch, nvarAngle, nvarScaled, &
            minTiltView, ncompSearch, ifLocal, mapTiltStart, mapAlfStart, mapAlfEnd, &
            ifBTSearch, tiltOrig, tiltAdd, pipinput, ninView, ninThresh, rotEntered)
        !
        ! DNM 7 / 16 / 04: Add pixel size to local file
        ! 2 / 15 / 07: Output after first read of variables
        ifAnyAlf = 0
        if (mapAlfEnd > mapAlfStart .or. beamTilt .ne. 0) ifAnyAlf = 1
        if (ifLocal == 1) write(iunLocal, '(7i6,f12.5,i4)') numPatchX, numPatchY, &
            ixsPatch, iysPatch, idxPatch, idyPatch, ifAnyAlf, xdelt, ifZfac
        ifLocal = 2
        !
        ! take care of initializing the mapped variables properly
        !
!!$        if (ifRotFix == 0) then
!!$        globrot = glbRot(1)
!!$        var(1) = globrot
!!$        mapTiltStart = nview + 1
!!$        else
!!$        globrot = glbRot(ifRotFix)
!!$        rot(ifRotFix) = globrot
!!$        mapTiltStart = nview
!!$        endif
!!$        do i = 1, nview
!!$        if ((ifRotFix == 0 .and. i > 1) .or. i < ifRotFix) then
!!$        var(i) = glbRot(i) - globrot
!!$        elseif (ifRotFix > 0 .and. i > ifRotFix) then
!!$        var(i - 1) = glbRot(i) - globrot
!!$        endif
!!$        enddo
        !
        ! reload the geometric variables
        !
        call reload_vars(glbRot, rot, mapRot, frcRot, nview, &
            1, mapTiltStart - 1, var, fixedDum, 1, mapLocalToAll)
        call reload_vars(glbTilt, tilt, mapTilt, frcTilt, nview, &
            mapTiltStart, nvarAngle, var, fixedDum, incrTilt, mapLocalToAll)
        !
        ! if doing tilt incremental, just set tiltInc to the global tilt and
        ! all the equations work in map_vars
        !
        if (incrTilt .ne. 0) then
          fixedTilt2 = 0.
          fixedTilt = 0.
          do i = 1, nview
            tiltInc(i) = glbTilt(mapLocalToAll(i))
          enddo
        endif
        call reload_vars(glbGmag, gmag, mapGmag, frcGmag, nview, nvarAngle + 1, &
            mapDmagStart - ncompSearch - 1, var, fixedGmag, incrGmag, mapLocalToAll)
        call reload_vars(glbDmag, dmag, mapDmag, frcDmag, nview, mapDmagStart, &
            nvarScaled, var, fixedDmag, incrDmag, mapLocalToAll)
        call reload_vars(glbSkew, skew, mapSkew, frcSkew, nview, nvarScaled + 1, &
            mapAlfStart - 1, var, fixedSkew, incrSkew, mapLocalToAll)
        call reload_vars(glbAlf, alf, mapAlf, frcAlf, nview, mapAlfStart, nvarSearch, &
            var, fixedAlf, incrAlf, mapLocalToAll)
        !
        ! get new scaling and scale projection points
        !
        scaleXY = 0.
        do i = 1, nprojpt
          scaleXY = max(scaleXY, abs(xx(i)), abs(yy(i)))
        enddo
        xx(1:nprojpt) = xx(1:nprojpt) / scaleXY
        yy(1:nprojpt) = yy(1:nprojpt) / scaleXY
        !
        ! load the xyz's and shift them to zero mean and scale them down
        !
        xsum = 0.
        ysum = 0.
        zsum = 0.
        do i = 1, nrealPt
          j = indAllReal(i)
          xyz(1, i) = allxyz(1, j) - xcen
          xsum = xsum + xyz(1, i)
          xyz(2, i) = allxyz(2, j) - ycen
          ysum = ysum + xyz(2, i)
          xyz(3, i) = allxyz(3, j)
          zsum = zsum + xyz(3, i)
        enddo
        xShift = xsum / nrealPt
        yShift = ysum / nrealPt
        zShift = zsum / nrealPt
        xyz(1, 1:nrealPt) = (xyz(1, 1:nrealPt) - xShift) / scaleXY
        xyz(2, 1:nrealPt) = (xyz(2, 1:nrealPt) - yShift) / scaleXY
        xyz(3, 1:nrealPt) = (xyz(3, 1:nrealPt) - zShift) / scaleXY
        write(*,'(/,a,2i3,a,2i5,a,2i5,a,i3,a)') ' Doing local area', &
            ipatchX, ipatchy, ', centered on', ixPatch, iyPatch, ', size', &
            nxp, nyp, ',  ', nrealPt, ' fiducials'
        if (minSurf > 0) write(*,'(a,i3,a,i3,a)') '    (', nbot, &
            ' on bottom and', ntop, ' on top)'
        ncycle = -abs(ncycle)
        call alignAndOutputResults()
      enddo
    enddo
    !
  end subroutine setupAndDoLocalAlignments


  subroutine findMedianResidual()
    real*4 tmpRes(maxReal), tmpMedian(maxView), xvfit(25), slope(5)
    integer*4 iorder/2/, nFullFit/15/, numIter/3/, iter, nfit
    logical allZero/.true./
    integer*4 iterCount/0/
    save iterCount
    
    do iv = 1, nview
      ninViewSum = 0
      do i = 1, nprojpt
        if (isecView(i) == iv) then
          ninViewSum = ninViewSum + 1
          tmpRes(ninViewSum) = sqrt(xresid(i)**2 + yresid(i)**2)
        endif
      enddo
      call rsSortFloats(tmpRes, ninViewSum)
      call rsMedianOfSorted(tmpRes, ninViewSum, viewMedianRes(iv))
      ! write(*,'(2i4,f9.3,a)') iterCount,iv, viewMedianRes(iv) * scaleXY,'  FMR'
      if (viewMedianRes(iv) .ne. 0) allZero = .false.
    enddo
    iterCount = iterCount + 1
    if (allZero) then
      viewMedianRes(1:nview) = 1.
      return
    endif
    !
    ! Make sure there are no zeros, just copy nearest value
    do iv = 1, nview
      if (viewMedianRes(iv) .eq. 0.) then
        NEAR_VIEW_LOOP:  do i = 1, nview - 1
          do iter = -1, 1, 2
            if (viewMedianRes(iv + iter * i) .ne. 0.) then
              viewMedianRes(iv) = viewMedianRes(iv + iter * i)
              exit NEAR_VIEW_LOOP
            endif
          enddo
        enddo NEAR_VIEW_LOOP
      endif
    enddo
    !
    ! smooth with iterations
    do iter = 1, numIter
      tmpMedian(1:nview) = viewMedianRes(1:nview)
      do iv = 1, nview
        ivst = max(1, iv - nFullFit / 2)
        ivnd = min(nview, iv + nFullFit / 2)
        nfit = ivnd + 1 - ivst
        if (nfit .lt. 5) iorder = 1
        do i = ivst, ivnd
          xvfit(i + 1 - ivst) = i - iv
        enddo
        call polyfit(xvfit, tmpMedian(ivst), nfit, iorder, slope, viewMedianRes(iv))
        ! write(*,'(2i4,f9.3,a)') iterCount, iv,viewMedianRes(iv) * scaleXY,'  FMR'
      enddo
      iterCount = iterCount + 1
    enddo

    return
  end subroutine findMedianResidual

end program tiltalign


subroutine errorexit(message, ifLocal)
  implicit none
  integer*4 ifLocal
  character*(*) message
  if (ifLocal .ne. 0) then
    write(*,'(/,a,a)') 'WARNING: ', message
    return
  endif
  write(*,'(/,a,a)') 'ERROR: TILTALIGN - ', message
  call exit(1)
end subroutine errorexit

! 5 / 19 / 89 added model output, changed format of output table
! 6 / 21 / 89 added mean residual output to find_surfaces, changed to
! get recommendation on maximum FIXED tilt angle
! 4 / 9 / 93 allow full mapping of compression variables
! 10 / 30 / 95 added distortion, automapping, point & angle output.
! 10 / 17 / 98 added linear combinations to automapping
! 2 / 12 / 98 added local alignments; changed find_surfaces to find and
! recommend an X - axis tilt.

!******************xfsimplex*******************************************
!
! This program searches for the best general linear transform between
! a pair of images by varying either the six formal parameters of the
! transform, the six "semi-natural" parameters underlying such a
! transform, or restricted subsets of those semi-natural parameters.
!
! See man page for details
!
! David Mastronarde 4/5/91 (adapted from XFSEARCH)
!
!************************************************************************
!
! $Id$
!
module simplexvars
  implicit none
  integer IDIMB, ISUB, LIMSPIR, ISUBP1, ISUBT2, ISUBT25
  parameter (IDIMB = 4100 * 4100)
  parameter (ISUB = IDIMB / 3, LIMSPIR = 1000)
  parameter (ISUBP1 = ISUB + 1, ISUBT2 = 2 * ISUB + 1, ISUBT25 = 2.5 * ISUB + 2)
  integer*4 nx, ny, nz, nxyz(3)
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  !
  ! array for second image if doing diffs, using same storage as
  ! all the arrays for doing distances
  real*4, allocatable :: array(:)
  real*4 brray(IDIMB)
  integer*2 ixComp(ISUB), iyComp(ISUB)
  real*4 denLow(ISUB), denHigh(ISUB)
  equivalence (brray(1), denLow(1)), (brray(ISUBP1), denHigh(1)), &
      (brray(ISUBT2), ixComp(1)), (brray(ISUBT25), iyComp(1))
  integer*4 nx1, nx2, ny1, ny2, ifInterp, natural, numCompare, numSpiral, ifDist, ifTrace
  real*4 reduction, deltaMin, sd1, distSpiral(LIMSPIR), acall(6), aLimits(2,6)
  integer*4 idima, numTrials, ivEnd, ifCCC, idxSpiral(LIMSPIR), idySpiral(LIMSPIR)
  real*8, allocatable :: sxa(:,:), sya(:,:), sxSqa(:,:), sySqa(:,:), sxya(:,:)
  integer*4, allocatable :: numPixA(:,:)
  integer*4 numXpatch, numYpatch, nxyPatch
end module simplexvars

! MAIN PROGRAM
!
Program xfsimplex
  use simplexvars
  implicit none
  integer*4 mxyz(3), nxyzst(3), nxyz2(3)
  !
  character*320 inputFile1, inputFile2, outFile, xfInFile
  !
  real*4, allocatable :: temp(:)
  data nxyzst/0, 0, 0/
  real*4 a(6), da(6), amat(2,2), anat(6), danat(6)
  real*4 pp(7,7), yy(7), ptol(6), ctf(8193)
  ! if doing formal params, the a(i) are DX, DY, a11, a12, a21, and a22
  ! for natural paramas, the a(i) are DX and DY, Global rotation, global
  ! stretch, difference between Y&X-axis stretch, and difference
  ! between Y and X axis rotation.
  ! the da are the step sizes for the respective a's
  logical trace
  ! starting values of a, and da, for formal search
  data a/0., 0., 1., 0., 0., 1./
  data da/1., 1., .025, .025, .025, .025/
  ! starting values of a and da for natural search
  data anat/0., 0., 0., 1., 0., 0./
  data danat/1., 1., 2., .02, .02, 2./
  real*8 tsum, tsumSq
  !
  integer*4 ihist(0:1001)
  real*4 range(10,2), paramLim(6), ranLow(10), rangeHigh(10), percentile(10,2), &
      pctLow(10), pctHigh(10)
  equivalence (ranLow(1), range(1, 1)), (rangeHigh(1), range(1, 2))
  equivalence (pctLow(1), percentile(1, 1)), (pctHigh(1), percentile(1, 2))
  external func
  !
  ! default values for potentially input parameters
  real*4 ftol1/5.e-4/, ptol1/.02/, ftol2/5.e-3/, ptol2/.2/
  real*4 deltaFac/2/
  integer*4 ifFloatMean/1/, ibinning/2/, numRanges/2/, idistRedund/0/
  real*4 radius/4/, difflim/0.05/
  !
  integer*4 mode, ierr, ivarStart, ii, jj, i, numLimit, lenTemp
  integer*4 numPixels, ibase, mattX, mattY, ixy, ind, iran, lowHigh
  real*4 fracMatt, dmin, dmax, dmean, pctRange, dmin2, dmax2, dmean2, sd2
  integer*4 ix, iy, ixm, iym, ixp, ic, ixcm, iycm
  real*4 histScale, val, dmin1, dmax1, dmean1
  integer*4 ncrit, limDxy, idx, idy, j, itmp, iter, jmin, izRef, izAli, npad, nxPad
  integer*4 nyPad, nxOrig, nyOrig, ifSobel, ifFiltAfter, lineUse
  integer*4 ifXminMax, ifYminMax, iAntiFiltType
  real*4 scale, dAdd, window, valScale, distance, delmin, ptolFac, tmp
  real*4 sigma1, sigma2, radius1, radius2, deltac
  integer*4 niceframe, niceFFTlimit
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetString, PipGetInteger, PipGetFloat, PipGetTwoFloats
  integer*4 PipGetInOutFile, PipGetBoolean, PipGetFloatArray
  integer*4 PipGetTwoIntegers
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  xfsimplex
  !
  integer numOptions
  parameter (numOptions = 34)
  character*(40 * numOptions) options(1)
  options(1) = &
      'aimage:AImageFile:FN:@bimage:BImageFile:FN:@output:OutputFile:FN:@'// &
      'initial:InitialTransformFile:FN:@useline:UseTransformLine:I:@'// &
      'sections:SectionsToUse:IP:@variables:VariablesToSearch:I:@'// &
      'limits:LimitsOnSearch:FA:@edge:EdgeToIgnore:F:@xminmax:XMinAndMax:IP:@'// &
      'yminmax:YMinAndMax:IP:@binning:BinningToApply:I:@antialias:AntialiasFilter:I:@'// &
      'sig1:FilterSigma1:F:@rad1:FilterRadius1:F:@rad2:FilterRadius2:F:@'// &
      'sig2:FilterSigma2:F:@after:FilterAfterBinning:B:@sobel:SobelFilter:B:@'// &
      'float:FloatOption:I:@ccc:CorrelationCoefficient:B:@local:LocalPatchSize:I:@'// &
      'linear:LinearInterpolation:B:@distance:DistanceMeasure:B:@'// &
      'near:NearestDistance:I:@radius:RadiusToSearch:F:@density:DensityDifference:F:@'// &
      'percent:PercentileRanges:FA:@coarse:CoarseTolerances:FP:@'// &
      'final:FinalTolerances:FP:@step:StepSizeFactor:F:@trace:TraceOutput:I:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  ! default values for parameters in module and equivalenced arrays
  !
  pctLow(1) = 0.
  pctLow(2) = 92.
  pctHigh(1) = 8.
  pctHigh(2) = 100.
  ifTrace = 0
  fracMatt = 0.05
  ifDist = 0
  natural = 0
  ifInterp = 0
  nxyPatch = 0
  izRef = 0
  izAli = 0
  do i = 1, 6
    aLimits(1, i) = 0
    aLimits(2, i) = 0
  enddo
  ifFiltAfter = 0
  sigma1 = 0.
  sigma2 = 0.
  radius1 = 0.
  radius2 = 0.
  npad = 16
  ifSobel = 0
  ifCCC = 0
  lineUse = 0
  ifXminMax = 0
  ifYminMax = 0
  iAntiFiltType = 0
  lenTemp = 32000 * 64
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'xfsimplex', &
      'ERROR: XFSIMPLEX - ', .true., 3, 2, 1, numOptArg, &
      numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0
  !
  if (PipGetInOutFile('AImageFile', 1, 'Enter first image file name', &
      inputFile1) .ne. 0) call exitError('NO FIRST IMAGE FILE SPECIFIED')
  if (PipGetInOutFile('BImageFile', 2, 'Enter second image file name', &
      inputFile2) .ne. 0) call exitError('NO SECOND IMAGE FILE SPECIFIED')
  if (PipGetInOutFile('OutputFile', 3, 'Transform output file name', &
      outFile) .ne. 0) call exitError('NO TRANSFORM OUTPUT FILE SPECIFIED')
  if (pipInput) then
    xfInFile = ' '
    ierr = PipGetString('InitialTransformFile', xfInFile)
  else
    write(*,'(1x,a,$)') &
        'File with starting transform, or Return if none: '
    read (5, 40) xfInFile
40  format (a)
  endif
  !
  ! open file now to get sizes and try to adjust defaults
  !
  call ialprt(.false.)
  call imopen(1, inputFile1, 'RO')
  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
  !
  if (nx <= 128 .and. ny <= 128) then
    ftol1 = 2. *ftol1
    ptol1 = 2. *ptol1
  endif
  !
  if (pipInput) then
    ierr = PipGetTwoFloats('CoarseTolerances', ftol2, ptol2)
    ierr = PipGetTwoFloats('FinalTolerances', ftol1, ptol1)
    ierr = PipGetFloat('StepSizeFactor', deltaFac)
    ierr = PipGetInteger('TraceOutput', ifTrace)
    ierr = PipGetInteger('UseTransformLine', lineUse)
    if (lineUse < 0) goto 95
  else
    write(*,'(1x,a,/,a,/,a,/,a,f6.4,f5.2,f7.4,f5.2,f4.1,i2,a,$)') &
        'Enter fractional tolerances in difference measure and'// &
        ' in parameter values', '   for terminating final search,'// &
        ' corresponding tolerances for initial search', &
        '   (or 0,0 for only one search), factor for initial'// &
        ' step size, and 1 or 2 for', '    trace output [' &
        , ftol1, ptol1, ftol2, ptol2, deltaFac, ifTrace, ']: '
    read(5,*) ftol1, ptol1, ftol2, ptol2, deltaFac, ifTrace
  endif
  trace = ifTrace > 0
  !
  if (pipInput) then
    ierr = PipGetInteger('VariablesToSearch', natural)
    if (natural < 0 .or. natural > 6) call exitError( &
        'NUMBER OF VARIABLES TO SEARCH IS OUT OF RANGE')
  else
13  write(*,'(1x,a,i1,a,$)') '0 to search 6 formal variables,'// &
        ' or # of natural variables to search [', natural, ']: '
    read(*,*) natural
    if (natural < 0 .or. natural > 6) go to 13
  endif
  ivarStart = 1
  if (natural == 0) then                      !all six formal params
    ivEnd = 6
  else
    ivEnd = natural                           !or selected # of natural
    do i = 1, 6
      a(i) = anat(i)
      da(i) = danat(i)
    enddo
  endif
  !
  ! if reading in a transform, get it and convert to natural if necessary
  !
  if (xfInFile .ne. ' ') then
    call checkForWarpFile(xfInFile)
    call dopen(1, xfInFile, 'old', 'f')
    do i = 0, lineUse
      read(1,*,err = 94, end = 95) ((amat(ii, jj), jj = 1, 2), ii = 1, 2), a(1), a(2)
    enddo
    if (natural == 0) then
      a(3) = amat(1, 1)
      a(4) = amat(1, 2)
      a(5) = amat(2, 1)
      a(6) = amat(2, 2)
    else
      call amat_to_rotmag(amat, a(3), a(6), a(4), a(5))
    endif
  endif
  !
  do i = 1, 6
    acall(i) = a(i)
  enddo
  !
  numLimit = 0
  if (pipInput) then
    ierr = PipGetFloat('EdgeToIgnore', fracMatt)
    ierr = PipGetInteger('FloatOption', ifFloatMean)
    ierr = PipGetInteger('BinningToApply', ibinning)
    ierr = PipGetBoolean('DistanceMeasure', ifDist)
    ierr = PipGetTwoIntegers('SectionsToUse', izRef, izAli)
    ierr = PipGetFloat('FilterSigma1', sigma1)
    ierr = PipGetFloat('FilterSigma2', sigma2)
    ierr = PipGetFloat('FilterRadius1', radius1)
    ierr = PipGetFloat('FilterRadius2', radius2)
    ierr = PipGetBoolean('FilterAfterBinning', ifFiltAfter)
    ierr = PipGetBoolean('SobelFilter', ifSobel)
    ierr = PipGetBoolean('CorrelationCoefficient', ifCCC)
    ibinning = max(1, ibinning)
    ifXminMax = 1 - PipGetTwoIntegers('XMinAndMax', nx1, nx2)
    ifYminMax = 1 - PipGetTwoIntegers('YMinAndMax', ny1, ny2)
    ierr = PipGetInteger('LocalPatchSize', nxyPatch)
    ierr = PipGetInteger('AntialiasFilter', iAntiFiltType)
    if (iAntiFiltType < 0) iAntiFiltType = 5
    if (iAntiFiltType < 2 .or. ibinning == 1) iAntiFiltType = 0
    if (iAntiFiltType > 0) then 
      lenTemp = 10000000
      ifFiltAfter = 1
    endif
    
    !
    ! Get search limits
    if (PipGetFloatArray('LimitsOnSearch', paramLim, numLimit, 6) == 0) then
      if (natural == 0 .and. numLimit > 2) call exitError( &
          'YOU CAN LIMIT ONLY X AND Y SHIFTS WHEN SEARCHING FOR FORMAL '// &
          'PARAMETERS; TRY -variables 6')
      if (natural > 0) numLimit = min(numLimit, natural)
      paramLim(1) = paramLim(1) / ibinning
      paramLim(2) = paramLim(2) / ibinning
    endif
  else
    write(*,'(1x,a,f4.2,a,$)') 'edge fraction to ignore [' , fracMatt, ']: '
    read(*,*) fracMatt
    !
    write(*,'(1x,a,i1,a,$)') '0 to float to range,'// &
        ' 1 to float to mean&sd, -1 no float [', ifFloatMean, ']: '
    read(*,*) ifFloatMean
    !
    write(*,'(1x,a,i1,a,$)') 'Binning to apply to image [' , ibinning, ']: '
    read(*,*) ibinning
    ibinning = max(1, ibinning)
    !
    write(*,'(1x,a,i1,a,$)') '0 for difference,'// &
        ' 1 for distance measure [', ifDist, ']: '
    read(*,*) ifDist
  endif
  if (ifDist == 0) then
    if (pipInput) then
      ierr = PipGetBoolean('LinearInterpolation', ifInterp)
    else
      write(*,'(1x,a,i1,a,$)') '1 to use interpolation [' , ifInterp, ']: '
      read(*,*) ifInterp
    endif
  else
    !
    ! change defaults based on image size and reduction by 2
    !
    numPixels = nx * ny
    if (ibinning > 1) then
      numPixels = numPixels / (ibinning**2)
      radius = 4.
    else
      radius = 5.
    endif
    !
    if (numPixels > 480 * 360) then
      idistRedund = 2
    elseif (numPixels > 240 * 180) then
      idistRedund = 1
    else
      idistRedund = 0
    endif
    !
    ! run percentile range from 8 down to 5 as go from 320x240 to 640x480
    !
    pctRange = min(8., max(5., 8. -3. *(numPixels - 320 * 240) / &
        (640 * 480 - 320 * 240)))
    pctHigh(1) = pctRange
    pctLow(2) = 100. -pctRange
    !
    if (pipInput) then
      ierr = PipGetInteger('NearestDistance', idistRedund)
      ierr = PipGetFloat('RadiusToSearch', radius)
      ierr = PipGetFloat('DensityDifference', difflim)
      ibase = 0
      if (PipGetFloatArray('PercentileRanges', brray, ibase, 20) == 0) &
          then
        if (mod(ibase, 2) .ne. 0) call exitError( &
            'YOU MUST ENTER AN EVEN NUMBER OF VALUES FOR PercentileRanges')
        numRanges = ibase / 2
        do i = 1, numRanges
          pctLow(i) = brray(2 * i - 1)
          pctHigh(i) = brray(2 * i)
        enddo
      endif
    else
      write(*,'(1x,a,i1,a,$)') 'distance to search for and'// &
          ' eliminate redundancy, 0 not to [', idistRedund, ']: '
      read(*,*) idistRedund
      !
      ! get density window and search radius
      write(*,'(1x,a,f3.1,a,$)') 'radius to search for match [', radius, ']: '
      read(*,*) radius
      !
      write(*,'(1x,a,f4.2,a,$)') 'max density difference for match' &
          //' as fraction of range [', difflim, ']: '
      read(*,*) difflim
      !
      write(*,'(1x,a,i1,a,$)') 'number of percentile ranges [' , numRanges, ']: '
      read(*,*) numRanges
      !
      ! get percentile ranges
      write(*,'(1x,a,6f6.1)') 'lower and upper percentiles'// &
          ' in ranges - defaults:', (pctLow(i), pctHigh(i), i = 1, numRanges)
      read(*,*) (pctLow(i), pctHigh(i), i = 1, numRanges)
    endif
  endif
  call PipDone()
  !
  call ialprt(.true.)
  call imclose(1)
  call imopen(1, inputFile1, 'RO')
  ! NOTE: ABSOLUTELY NEED TO READ HEADER AGAIN
  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
  !
  call imopen(2, inputFile2, 'RO')
  call irdhdr(2, nxyz2, mxyz, mode, dmin, dmax, dmean)
  if (nxyz2(1) .ne. nx .or. nxyz(2) .ne. ny) call exitError( &
      'THE TWO IMAGES MUST BE THE SAME SIZE IN X AND Y')
  if (izRef < 0 .or. izRef >= nz .or. izAli < 0 .or. &
      izAli >= nxyz2(3)) call exitError( &
      'ONE OF THE SECTION NUMBERS IS OUT OF RANGE')
  !
  ! Open data file for transform
  !
  call dopen(4, outFile, 'NEW', 'F')
  !
  ! Get possibly filtered sizes, and check dimensions
  nxOrig = nx
  nyOrig = ny
  nx = nx / ibinning
  ny = ny / ibinning
  if (ifFiltAfter == 0) then
    nxPad = niceframe(nxOrig + npad, 2, niceFFTlimit())
    nyPad = niceframe(nyOrig + npad, 2, niceFFTlimit())
  else
    nxPad = niceframe(nx + npad, 2, niceFFTlimit())
    nyPad = niceframe(ny + npad, 2, niceFFTlimit())
  endif
  call setctfwsr(sigma1, sigma2, radius1, radius2, ctf, nxPad, nyPad, deltac)
  !
  ! Allocate big array and temp array
  lenTemp = min(lenTemp, nxOrig * nyOrig)
  idima = (nxPad + 2) * nyPad + 10
  allocate(array(idima), temp(lenTemp), stat = ierr)
  call memoryError(ierr, 'LARGE IMAGE ARRAYS')

  if (nx * ny > IDIMB) call exitError('IMAGE TOO BIG FOR ARRAYS - USE HIGHER BINNING')
  !
  ! Reduce the shift parameters, and set the limits now that shift is set
  reduction = ibinning
  da(1) = da(1) / reduction
  da(2) = da(2) / reduction
  a(1) = a(1) / reduction
  a(2) = a(2) / reduction
  do i = 1, numLimit
    if (paramLim(i) >= 0.) then
      aLimits(1, i) = a(i) - paramLim(i)
      aLimits(2, i) = a(i) + paramLim(i)
    endif
  enddo
  !
  ! just get second section to work with
  call readFilterSection(2, array, izAli, nxOrig, nyOrig, nx, ny, nxPad, nyPad, &
      ibinning, ctf, deltac, ifFiltAfter, iAntiFiltType, ifSobel, temp, lenTemp)
  !
  ! Just deal with points in central portion
  if (fracMatt >= 1.) then
    mattX = nint(fracMatt / reduction)
    mattY = mattX
  else
    mattX = max(0, nint(float(nx) * fracMatt))
    mattY = max(0, nint(float(ny) * fracMatt))
  endif
  if (mattX >= 0.49 * nx .or. mattY >= 0.49 * ny) call exitError( &
      'FRACTION OR NUMBER OF PIXELS TO IGNORE IS TOO LARGE')
  if (ifXminMax .ne. 0) then
    nx1 = (nx1 + ibinning - 1) / ibinning
    nx2 = nx2 / ibinning
    if (nx1 <= 0 .or. nx2 > nx .or. nx1 >= nx2) call exitError &
        ('STARTING AND ENDING X VALUES OUT OF RANGE OR REVERSED')
  else
    nx1 = 1 + mattX
    nx2 = nx - mattX
  endif
  if (ifYminMax .ne. 0) then
    ny1 = (ny1 + ibinning - 1) / ibinning
    ny2 = ny2 / ibinning
    if (ny1 <= 0 .or. ny2 > ny .or. ny1 >= ny2) call exitError &
        ('STARTING AND ENDING Y VALUES OUT OF RANGE OR REVERSED')
  else
    ny1 = 1 + mattY
    ny2 = ny - mattY
  endif
  !
  ! Adjust patch size for binning, set to one patch if none, and allocate
  nxyPatch = nxyPatch / ibinning
  if (nxyPatch .ne. 0 .and. nxyPatch < 10) call exitError( &
      'LOCAL PATCH SIZE MUST BE AT LEAST 10 BINNED PIXELS')
  if (nxyPatch == 0) nxyPatch = max(nx2 + 1 - nx1, ny2 + 1 - ny1)
  numXpatch = (nx2 + 1 - nx1 + nxyPatch - 1) / nxyPatch
  numYpatch = (ny2 + 1 - ny1 + nxyPatch - 1) / nxyPatch
  ! print *,nxyPatch, numXpatch, numYpatch
  allocate(sxa(numXpatch, numYpatch), sya(numXpatch, numYpatch), &
      sxSqa(numXpatch, numYpatch), sySqa(numXpatch, numYpatch), &
      sxya(numXpatch, numYpatch), numPixA(numXpatch, numYpatch), stat = ierr)
  if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS FOR PATCHES')
  !
  call iclavgsd(array, nx, ny, nx1, nx2, ny1, ny2 , dmin2, dmax2, tsum, tsumSq, &
      dmean2, sd2)
  !
  if (ifDist == 0) then
    ! if doing simple difference measure, move array into brray
    do ixy = 1, ny * nx
      brray(ixy) = array(ixy)
    enddo
  else
    ! if doing distance measure, make histogram of intensities
    do i = 0, 1000
      ihist(i) = 0
    enddo
    histScale = 1000. / (dmax2 - dmin2)
    do j = ny1, ny2
      ibase = nx * (j - 1)
      do i = nx1, nx2
        !
        ind = (array(i + ibase) - dmin2) * histScale
        ihist(ind) = ihist(ind) + 1
        !
      enddo
    enddo
    ! convert to cumulative histogram
    do i = 1, 1000
      ihist(i) = ihist(i) + ihist(i - 1)
    enddo
    ! find density corresponding to each percentile limit
    do iran = 1, numRanges
      do lowHigh = 1, 2
        ncrit = (nx2 + 1 - nx1) * (ny2 + 1 - ny1) * percentile(iran, lowHigh) / 100.
        i = 0
        do while(i <= 1000 .and. ncrit > ihist(i))
          i = i + 1
        enddo
        range(iran, lowHigh) = (i / histScale) + dmin2
      enddo
    enddo
    ! find all points in central part of array within those density
    ! limits
    numCompare = 0
    do iy = ny1, ny2
      ibase = nx * (iy - 1)
      do ix = nx1, nx2
        val = array(ix + ibase)
        do iran = 1, numRanges
          if (val >= ranLow(iran) .and. val <= rangeHigh(iran)) then
            ! redundancy reduction: look for previous nearby points in
            ! list
            if (idistRedund > 0) then
              ixm = ix - idistRedund
              iym = iy - idistRedund
              ixp = ix + idistRedund
              do ic = numCompare, 1, -1
                ixcm = ixComp(ic)
                iycm = iyComp(ic)
                ! if point in list is nearby and within same density
                ! range, skip this one
                if (ixcm >= ixm .and. ixcm <= ixp .and. iycm >= iym &
                    .and. denLow(ic) >= ranLow(iran) .and. denLow(ic) &
                    <= rangeHigh(iran)) go to 80
                ! if gotten back far enough on list, take this point
                if (iycm < iym .or. (iycm == iym .and. ixcm < ixm)) &
                    go to 78
              enddo
            endif
78          numCompare = numCompare + 1
            ixComp(numCompare) = ix
            iyComp(numCompare) = iy
            ! just store density temporarily here
            denLow(numCompare) = val
            go to 80
          endif
        enddo
80    enddo
    enddo
    print *,numCompare, ' points for comparison'
  endif
  !
  ! Now get first section
  call readFilterSection(1, array, izRef, nxOrig, nyOrig, nx, ny, nxPad, nyPad, &
      ibinning, ctf, deltac, ifFiltAfter, iAntiFiltType, ifSobel, temp, lenTemp)
  !
  call iclavgsd(array, nx, ny, nx1, nx2, ny1, ny2 , dmin1, dmax1, tsum, tsumSq, &
      dmean1, sd1)
  !
  ! get scale factor for floating second array densities to match that
  ! of first
  scale = 1.
  dAdd = 0.
  if (ifFloatMean == 0) then
    scale = (dmax1 - dmin1) / (dmax2 - dmin2)
    dAdd = dmin1 - scale * dmin2
  elseif (ifFloatMean > 0) then
    scale = sd1 / sd2
    dAdd = dmean1 - scale * dmean2
  endif
  if (ifDist == 0) then
    ! for simple difference, rescale whole array
    do ixy = 1, nx * ny
      brray(ixy) = scale * brray(ixy) + dAdd
    enddo
    call diff(delmin, array, brray, a, nx, ny)
  else
    ! otherwise, for distance,
    ! rescale list of densities and add lower and upper window
    window = scale * 0.5 * difflim * (dmax2 - dmin2)
    do i = 1, numCompare
      valScale = scale * denLow(i) + dAdd
      denLow(i) = valScale - window
      denHigh(i) = valScale + window
    enddo
    ! find points within search radius
    limDxy = radius + 1
    numSpiral = 0
    do idx = -limDxy, limDxy
      do idy = -limDxy, limDxy
        distance = sqrt(float(idx**2 + idy**2))
        if (distance <= radius) then
          numSpiral = numSpiral + 1
          distSpiral(numSpiral) = distance
          idxSpiral(numSpiral) = idx
          idySpiral(numSpiral) = idy
        endif
      enddo
    enddo
    ! order them by distance
    do i = 1, numSpiral
      do j = i + 1, numSpiral
        if (distSpiral(i) > distSpiral(j)) then
          tmp = distSpiral(i)
          distSpiral(i) = distSpiral(j)
          distSpiral(j) = tmp
          itmp = idxSpiral(i)
          idxSpiral(i) = idxSpiral(j)
          idxSpiral(j) = itmp
          itmp = idySpiral(i)
          idySpiral(i) = idySpiral(j)
          idySpiral(j) = itmp
        endif
      enddo
    enddo
    !
    call dist(delmin, array, a, aLimits, nx, ny, ixComp, iyComp, denLow, denHigh, &
        numCompare, idxSpiral, idySpiral, distSpiral, numSpiral, natural)
    !
  endif
  numTrials = 0
  deltaMin = 1.e30
  !
  ! DNM 4/29/02: search fails if images match perfectly, so skip if so
  !
  if (delmin > 0.) then
    ptolFac = ptol1
    if (ftol2 > 0 .or. ptol2 > 0) ptolFac = ptol2
    call amoebaInit(pp, yy, 7, ivEnd, deltaFac, ptolFac, a, da, func, ptol)
    if (ftol2 > 0 .or. ptol2 > 0) then
      call amoeba(pp, yy, 7, ivEnd, ftol2, func, iter, ptol, jmin)
      if (trace) print *,'restarting'
      deltaMin = 1.e30
      do i = 1, ivEnd
        a(i) = pp(jmin, i)
      enddo
      call amoebaInit(pp, yy, 7, ivEnd, deltaFac, ptol1, a, da, func, ptol)
    endif
    call amoeba(pp, yy, 7, ivEnd, ftol1, func, iter, ptol, jmin)
    !
    do i = 1, ivEnd
      a(i) = pp(jmin, i)
    enddo
    deltaMin = yy(jmin)
  else
    deltaMin = 0.
  endif
  !
  ! DEPENDENCY: transferfid expects two lines with natural params
  print *,' FINAL VALUES'
  if (ifCCC .ne. 0) deltaMin = 1 - deltaMin
  write(*,72) numTrials, deltaMin, (a(ii), ii = 3, 6), reduction * a(1), reduction * a(2)
72 format(i4,f14.7,1x,4f10.5,2f10.3)
  !
  if (natural .ne. 0) then
    call rotmag_to_amat(a(3), a(6), a(4), a(5), amat)
    write(*,70) ((amat(ii, jj), jj = 1, 2), ii = 1, 2), reduction * a(1), reduction * a(2)
    write(4,70) ((amat(ii, jj), jj = 1, 2), ii = 1, 2), reduction * a(1), reduction * a(2)
  else
    write(4,70) (a(ii), ii = 3, 6), reduction * a(1), reduction * a(2)
  endif
70 format(4f12.7,2f12.3)
  !
  call imclose(1)
  call imclose(2)
  !
  call exit(0)
  !
94 call exitError('READING INITIAL TRANSFORM FILE')
95 call exitError('INITIAL TRANSFORM NUMBER OUT OF RANGE')
end program
!
!*********************************************************************
!
! FIND MISFIT OF IMAGES
!
!*******************************************************************
!
! Taking the difference or correlation between images
!
subroutine diff(delta, crray, drray, a, nxIn, nyIn)
  !
  use simplexvars
  implicit none
  real*4 crray(nxIn,nyIn), drray(nxIn,nyIn), a(6), amat(2,2)
  real*4 delta
  integer*4 nxIn, nyIn
  !
  real*4 deltaLast/0./
  save deltaLast
  real*4 xcen, ycen, xAdd, yAdd, fj, fi, x, y, fx, fx1, fy, fy1, den, deltaFac, ccc
  real*4 daltaSum, dend
  integer*4 numPix, j, i, ix, iy, ix1, iy1, ixp, iyp, idirX, idirY, idx, idy
  integer*4 numCrit, minDist, ixMin, iyMin
  logical*4 oldDiff
  real*8 sx
  real*4 outsideMultiplier

  delta = 0.

  sx = 0.
  numPix = 0
  do iyp = 1, numYpatch
    do ixp = 1, numXpatch
      sxa(ixp, iyp) = 0.
      sya(ixp, iyp) = 0.
      sxya(ixp, iyp) = 0.
      sxSqa(ixp, iyp) = 0.
      sySqa(ixp, iyp) = 0.
      numPixA(ixp, iyp) = 0
    enddo
  enddo
  oldDiff = numXpatch * numYpatch == 1 .and. ifCCC == 0
  xcen = float(nx) * 0.5 + 0.5                    !use + 0.5 to be consistent
  ycen = float(ny) * 0.5 + 0.5                    !with new cubinterp usage
  !
  if (natural == 0) then
    amat(1, 1) = a(3)
    amat(1, 2) = a(4)
    amat(2, 1) = a(5)
    amat(2, 2) = a(6)
  else
    call rotmag_to_amat(a(3), a(6), a(4), a(5), amat)
  endif
  ! print *,((amat(i, j), j=1, 2), i=1, 2)
  xAdd = xcen + a(1)
  yAdd = ycen + a(2)
  if (ifInterp == 0) then
    xAdd = xcen + a(1) + 0.5                      !the 0.5 here gets nearest int
    yAdd = ycen + a(2) + 0.5
  endif
  do j = ny1, ny2
    fj = float(j) - ycen
    iyp = (j - ny1) / nxyPatch + 1
    if (ifInterp == 0) then
      !
      do i = nx1, nx2
        fi = float(i) - xcen
        !
        ix = amat(1, 1) * fi + amat(1, 2) * fj + xAdd
        iy = amat(2, 1) * fi + amat(2, 2) * fj + yAdd
        !
        if (ix >= 1 .and. ix <= nx .and. iy >= 1 .and. iy <= ny) then
          den = crray(ix, iy)
          !
          ! DUPLICATION FROM HERE (sorry, it saves 5% over an internal sub)
          dend = drray(i, j)
          if (oldDiff) then
            sx = sx + abs(den - dend)
            numPix = numPix + 1
          else if (ifCCC == 0) then
            ixp = (i - nx1) / nxyPatch + 1
            sxa(ixp, iyp) = sxa(ixp, iyp) + den - dend
            sxSqa(ixp, iyp) = sxSqa(ixp, iyp) + (den - dend)**2
            numPixA(ixp, iyp) = numPixA(ixp, iyp) + 1
          else
            ixp = (i - nx1) / nxyPatch + 1
            sxa(ixp, iyp) = sxa(ixp, iyp) + den
            sya(ixp, iyp) = sya(ixp, iyp) + dend
            sxya(ixp, iyp) = sxya(ixp, iyp) + den * dend
            sxSqa(ixp, iyp) = sxSqa(ixp, iyp) + den**2
            sySqa(ixp, iyp) = sySqa(ixp, iyp) + dend**2
            numPixA(ixp, iyp) = numPixA(ixp, iyp) + 1
          endif
          ! DUPLICATION TO HERE
        endif
        !
      enddo
      !
    else
      do i = nx1, nx2
        fi = float(i) - xcen
        !
        x = amat(1, 1) * fi + amat(1, 2) * fj + xAdd
        y = amat(2, 1) * fi + amat(2, 2) * fj + yAdd
        !
        ix = x
        iy = y
        if (ix >= 1 .and. ix < nx .and. iy >= 1 .and. iy < ny) then
          ! IX = MIN(NX-1, MAX(IX, 1))
          ! IY = MIN(NY-1, MAX(IY, 1))
          ix1 = ix + 1
          iy1 = iy + 1
          fx = 1 + ix - x
          fx1 = 1. -fx
          fy = 1 + iy - y
          fy1 = 1. -fy
          den = crray(ix, iy) * fx * fy + crray(ix1, iy) * fx1 * fy + &
              crray(ix, iy1) * fx * fy1 + crray(ix1, iy1) * fx1 * fy1
          !
          ! DUPLICATED SECTION
          dend = drray(i, j)
          if (oldDiff) then
            sx = sx + abs(den - dend)
            numPix = numPix + 1
          else if (ifCCC == 0) then
            ixp = (i - nx1) / nxyPatch + 1
            sxa(ixp, iyp) = sxa(ixp, iyp) + den - dend
            sxSqa(ixp, iyp) = sxSqa(ixp, iyp) + (den - dend)**2
            numPixA(ixp, iyp) = numPixA(ixp, iyp) + 1
          else
            ixp = (i - nx1) / nxyPatch + 1
            sxa(ixp, iyp) = sxa(ixp, iyp) + den
            sya(ixp, iyp) = sya(ixp, iyp) + dend
            sxya(ixp, iyp) = sxya(ixp, iyp) + den * dend
            sxSqa(ixp, iyp) = sxSqa(ixp, iyp) + den**2
            sySqa(ixp, iyp) = sySqa(ixp, iyp) + dend**2
            numPixA(ixp, iyp) = numPixA(ixp, iyp) + 1
          endif
        endif
        !
      enddo
      !
    endif
  enddo
  if (oldDiff) then
    if (numPix > 0) delta = sx / (numPix * sd1)
  else
    !
    ! Combine patches with less than half the pixels with nearest patch
    ! with more than half
    numCrit = nxyPatch**2 / 2
    do iyp = 1, numYpatch
      do ixp = 1, numXpatch
        if (numPixA(ixp, iyp) > 0 .and. numPixA(ixp, iyp) < numCrit) then
          minDist = 1000000000
          !
          ! Find nearest qualifying patch
          do idy = 0, max(iyp, numYpatch - iyp)
            if (idy**2 <= minDist) then
              do idirY = -1, 1, 2
                iy = iyp + idirY * idy
                if (iy >= 1 .and. iy <= numYpatch) then
                  do idx = 0, max(ixp, numXpatch - ixp)
                    if (idx**2 <= minDist) then
                      do idirX = -1, 1, 2
                        ix = ixp + idirX * idx
                        if (ix >= 1 .and. ix <= numXpatch) then
                          if (numPixA(ix, iy) >= numCrit .and. &
                              idx**2 + idy**2 < minDist) then
                            minDist = idx**2 + idy**2
                            ixMin = ix
                            iyMin = iy
                          endif
                        endif
                      enddo
                    endif
                  enddo
                endif
              enddo
            endif
          enddo
          !
          ! If found a qualifying closest patch, add the data into it
          if (minDist < 100000000) then
            ! write(*,'(a,6i6)') 'Pooling', ixp, iyp, npixa(ixp, iyp), ixmin, &
            ! iymin, npixa(ixmin, iymin)
            numPixA(ixMin, iyMin) = numPixA(ixMin, iyMin) + numPixA(ixp, iyp)
            sxa(ixMin, iyMin) = sxa(ixMin, iyMin) + sxa(ixp, iyp)
            sxSqa(ixMin, iyMin) = sxSqa(ixMin, iyMin) + sxSqa(ixp, iyp)
            sxya(ixMin, iyMin) = sxya(ixMin, iyMin) + sxya(ixp, iyp)
            sya(ixMin, iyMin) = sya(ixMin, iyMin) + sya(ixp, iyp)
            sySqa(ixMin, iyMin) = sySqa(ixMin, iyMin) + sySqa(ixp, iyp)
            numPixA(ixp, iyp) = 0
          endif
        endif
      enddo
    enddo
    !
    ! Now add up the delta values from the patches, weighted by number of
    ! pixels
    daltaSum = 0.
    numPix = 0
    do iyp = 1, numYpatch
      do ixp = 1, numXpatch
        if (numPixA(ixp, iyp) > 1) then
          if (ifCCC == 0) then
            !
            ! Get the SD of the diff and scale it by the sd of the data
            den = (sxSqa(ixp, iyp) - sxa(ixp, iyp)**2 / numPixA(ixp, iyp)) / &
                (numPixA(ixp, iyp) - 1.)
            if (den > 0) then
              daltaSum = daltaSum + numPixA(ixp, iyp) * sqrt(den) / sd1
              numPix = numPix + numPixA(ixp, iyp)
            endif
          else
            !
            ! Or get the ccc and use 1 - ccc
            den = (numPixA(ixp, iyp) * sxSqa(ixp, iyp) - sxa(ixp, iyp)**2) * &
                (numPixA(ixp, iyp) * sySqa(ixp, iyp) - sya(ixp, iyp)**2)
            if (den > 0) then
              ccc = (numPixA(ixp, iyp) * sxya(ixp, iyp) - &
                  sxa(ixp, iyp) * sya(ixp, iyp)) / sqrt(den)
              daltaSum = daltaSum + numPixA(ixp, iyp) * (1. - ccc)
              numPix = numPix + numPixA(ixp, iyp)
            endif
          endif
        endif
      enddo
    enddo
    !
    ! Take the weighted average
    if (numPix > 0) delta = daltaSum / numPix
  endif
  !
  deltaFac = outsideMultiplier(a, aLimits)
  delta = delta * deltaFac
  !
  ! DNM 5/17/02: if the number of pixels falls below a small threshold
  ! return a big delta; otherwise adjust for # of pixels and save value
  ! 5/13/06: Normalize to # of Sds difference per pixel
  !
  if (deltaFac > 1. .or. numPix > 0.02 * (nx2 + 1 - nx1) * (ny2 + 1 - ny1)) then
    deltaLast = delta
  else
    delta = 10. *deltaLast
  endif
  return
  !
end subroutine diff


! Or measure distance between features
!
subroutine dist(delta, array, a, aLimits, nx, ny, ixComp, iyComp, denLow, denHigh, &
    numCompare, idxSpiral, idySpiral, distSpiral, numSpiral, natural)
  implicit none
  !
  integer*4 nx, ny, numCompare, numSpiral, natural
  real*4 delta
  real*4 array(nx,ny), a(6), amat(2,2), aLimits(2,6)
  integer*2 ixComp(*), iyComp(*)
  real*4 denLow(*), denHigh(*), distSpiral(*)
  integer*4 idxSpiral(*), idySpiral(*)
  real*4 xcen, ycen, xAdd, yAdd, fj, fi, distance, critLow, critHigh, den1, distMax
  integer*4 ispir, ixa, iya, icomp, ix, iy
  real*4 deltaFac, outsideMultiplier
  !
  xcen = float(nx) * 0.5 + 0.5
  ycen = float(ny) * 0.5 + 0.5
  !
  delta = 0.
  !
  xAdd = xcen + a(1) + 0.5
  yAdd = ycen + a(2) + 0.5
  if (natural == 0) then
    amat(1, 1) = a(3)
    amat(1, 2) = a(4)
    amat(2, 1) = a(5)
    amat(2, 2) = a(6)
  else
    call rotmag_to_amat(a(3), a(6), a(4), a(5), amat)
  endif
  ! print *,((amat(i, j), j=1, 2), i=1, 2)
  distMax = distSpiral(numSpiral) + 1.
  do icomp = 1, numCompare
    fj = iyComp(icomp) - ycen
    fi = ixComp(icomp) - xcen
    !
    ix = amat(1, 1) * fi + amat(1, 2) * fj + xAdd
    iy = amat(2, 1) * fi + amat(2, 2) * fj + yAdd
    !
    distance = distMax
    critLow = denLow(icomp)
    critHigh = denHigh(icomp)
    do ispir = 1, numSpiral
      ixa = min(nx, max(1, ix + idxSpiral(ispir)))
      iya = min(ny, max(1, iy + idySpiral(ispir)))
      den1 = array(ixa, iya)
      if (den1 >= critLow .and. den1 <= critHigh) then
        distance = distSpiral(ispir)
        go to 40
      endif
    enddo
40  delta = delta + distance
    !
  enddo
  !
  ! 5/13/06: Normalize to per comparison point to match normalization
  ! of difference measure
  !
  deltaFac = outsideMultiplier(a, aLimits)
  delta = deltaFac * delta / numCompare
  return
  !
end subroutine dist
!
!
! find a multiplier for the delta factor if outside limits
!
real*4 function outsideMultiplier(a, aLimits)
  implicit none
  real*4 a(6), aLimits(2,6), deltaFac, outside
  integer*4 i
  deltaFac = 1.
  do i = 1, 6
    if (aLimits(1, i) < aLimits(2, i)) then
      outside = max(a(i) - aLimits(2, i), aLimits(1, i) - a(i)) / &
          (aLimits(2, i) - aLimits(1, i))
      if (outside > 0.) deltaFac = max(deltaFac, 100.**min(5., outside))
    endif
  enddo
  outsideMultiplier = deltaFac
  return
end function outsideMultiplier


! Function to be called by minimization routine
!
subroutine func(x, error)
  use simplexvars
  implicit none
  real*4 x(*), a(6), error
  real*4 delta, deltaOut
  character*1 starOut
  integer*4 i, ii
  !
  do i = 1, ivEnd
    a(i) = x(i)
  enddo
  do i = ivEnd + 1, 6
    a(i) = acall(i)
  enddo
  !
  if (ifDist == 0) then
    call diff(delta, array, brray, a, nx, ny)
  else
    call dist(delta, array, a, aLimits, nx, ny, ixComp, iyComp, denLow, denHigh, &
        numCompare, idxSpiral, idySpiral, distSpiral, numSpiral, natural)
  endif
  error = delta
  numTrials = numTrials + 1
  if (ifTrace .ne. 0) then
    starOut = ' '
    if (delta < deltaMin) then
      starOut = '*'
      deltaMin = delta
    endif
    deltaOut = delta
    if (ifCCC .ne. 0) deltaOut = 1. - delta
    if (ifTrace == 1 .or. starOut == '*') write(*,72) starOut, numTrials, deltaOut, &
        (a(ii), ii = 3, 6), reduction * a(1), reduction * a(2)
72  format(1x,a1,i3,f14.7,4f10.5,2f10.3)
  endif
  return
end subroutine func

!
! Read a section with optional fourier filtering before or after
! binning, and with optional sobel filtering
!
subroutine readFilterSection(iunit, array, iz, nxOrig, nyOrig, nx, ny, nxPad, nyPad, &
    ibinning, ctf, deltac, ifFiltAfter, iAntiFiltType, ifSobel, temp, lenTemp)
  implicit none
  real*4 array(*), ctf(*), deltac, temp(*)
  integer*4 iunit, iz, nxOrig, nyOrig, nx, ny, nxPad, nyPad
  integer*4 ibinning, ifFiltAfter, ifSobel, lenTemp, iAntiFiltType
  integer*4 ierr, nxFilt, nyFilt, ixLow, iyLow
  real*4 xOffset, yOffset
  integer*4 scaledSobel
  !
  ! Read in the section without or with binning, set size being filtered
  if (deltac .ne. 0 .and. ifFiltAfter == 0) then
    call imposn(iunit, iz, 0)
    call irdsec(iunit, array, 99)
    nxFilt = nxOrig
    nyFilt = nyOrig
  else
    if (iAntiFiltType > 0) then
      call irdreduced(iunit, iz, array, nx, 0., 0., float(ibinning), nx, ny,  &
          iAntiFiltType - 1, temp, lenTemp, ierr)
    else
      call irdbinned(iunit, iz, array, nx, ny, 0, 0, ibinning, nx, ny, temp, lenTemp, &
          ierr)
    endif
    if (ierr .ne. 0) goto 99
    nxFilt = nx
    nyFilt = ny
  endif
  !
  ! Apply fourier filter
  if (deltac .ne. 0.) then
    call taperOutPad(array, nxFilt, nyFilt, array, nxPad + 2, nxPad, nyPad, 0, 0)
    call todfft(array, nxPad, nyPad, 0)
    call filterPart(array, array, nxPad, nyPad, ctf, deltac)
    call todfft(array, nxPad, nyPad, 1)
    ixLow = (nxPad - nxFilt) / 2
    iyLow = (nyPad - nyFilt) / 2
    call irepak(array, array, nxPad + 2, nyPad, ixLow, ixLow + nxFilt - 1, iyLow, &
        iyLow + nyFilt - 1)
    !
    ! Now bin if necessary
    if (ifFiltAfter == 0 .and. ibinning > 1) call reduce_by_binning &
        (array, nxOrig, nyOrig, ibinning, array, nxFilt, nyFilt)
  endif
  !
  ! Do sobel if requested: NOTE THAT IT CAN BE DONE IN PLACE AS LONG AS
  ! SCALING = 1 BUT THIS SHOULD BE DOCUMENTED
  if (ifSobel .ne. 0) then
    if (scaledSobel(array, nx, ny, 1., 1., 1, 2, array, nxFilt, nyFilt, &
        xOffset, yOffset) .ne. 0) &
        call exitError('GETTING MEMORY FOR SOBEL FILTERING')
  endif
  return
99 call exitError('READING FILE')
end subroutine readFilterSection


! Routine to check for a warp file and exit with error if so
!
subroutine checkForWarpFile(xfInFile)
  character*(*) xfInFile
  character*240 strnTmp
  integer*4 ierr, idx, idy, itmp, i, jj
  real*4 deltac
  integer*4 readCheckWarpFile
  ierr = readCheckWarpFile(xfInFile, 0, 0, idx, idy, itmp, i, deltac, jj, strnTmp)
  if (ierr >= 0) call exitError( &
      'THE INITIAL TRANSFORM FILE CONTAINS WARPING TRANSFORMS')
  if (ierr .ne. -1) then
    write(*, '(/,a)') 'ERROR: A PROBLEM OCCURRED TESTING WHETHER THE INITIAL'// &
        ' TRANSFORM FILE HAD WARPINGS'
    call exitError(strnTmp)
  endif
  return
end subroutine checkForWarpFile

! ************* NEWSTACK **********************************************
!
! NEWSTACK is a general stack editor to move images into, out of, or
! between stacks.  It can float the images to a common range or mean of
! density. It can apply a general linear transform specified as a line
! in a file. It can put the output into a smaller or larger array and
! independently recenter each image separately from the transform.
! Images can be taken from multiple input files and placed into multiple
! output files.
!
! for all details see the man page.
!
! $Id$
!
program newstack
  implicit none
  integer MAXTEMP, LIMSEC, maxChunks, LIMGRADSEC
  parameter (LIMSEC = 1000000, maxChunks = 250, LIMGRADSEC = 10000)
  parameter (MAXTEMP = 5000000)
  integer*4 nx, ny, nz
  real*4, allocatable :: array(:)
  !
  integer*4 nxyz(3), mxyz(3), nxyzst(3), nxyz2(3), mxyz2(3), maxExtraIn, maxExtraOut
  real*4 cell2(6), cell(6), title(20), delta(3), xOrigin, yOrigin, zOrigin, deltafirst(3)
  !
  character*320 xfFile, inFileList, outFileList
  character*320, allocatable :: inFile(:), outFile(:)
  character*320 idfFile, magGradFile
  character*320 tempName, temp_filename, seriesExt
  character*100000 listString
  character*6 convFormat
  character*10 convNum
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  !
  data nxyzst/0, 0, 0/
  character*20 floatText/' '/, xfText/' '/, trunctText/' '/
  real*4 frot(2,3), fexp(2,3), fprod(2,3)
  integer*4 inList(LIMSEC)
  integer*4, allocatable :: nlist(:), listInd(:), numSecOut(:), lineTmp(:), isecExclude(:)
  real*4 optimalMax(16)
  integer*4 lineOutSt(maxChunks+1), numLinesOut(maxChunks)
  integer*4 lineInSt(maxChunks+1), numLinesIn(maxChunks)
  real*4, allocatable :: scaleFacs(:), scaleConsts(:), secZmins(:), secZmaxes(:), ztemp(:)
  integer*1, allocatable :: extraIn(:), extraOut(:)
  data optimalMax/255., 32767., 255., 32767., 255., 255., 65535., 255., 255., &
      511., 1023., 2047., 4095., 8191., 16383., 32767./
  !
  integer(kind = 8) idimInOut, limToAlloc, i8, numPix, iChunkBase, iBufOutBase, istart
  integer(kind = 8) numMove, moveOffset, limIfFail
  integer*4 ifDistort, idfBinning, iBinning, idfNx, idfNy, iWarpFlags
  integer*4 nxGrid, nyGrid, numFields, numIdfUse
  real*4 xGridIntrv, yGridIntrv, pixelSize, xGridStart, yGridStart, warpScale
  real*4 xnBig, ynbig
  !
  integer*4 ifMagGrad, numMagGrad, magUse
  real*4 pixelMagGrad, axisRot
  integer*4, allocatable :: lineUse(:), listReplace(:), idfUse(:), nControl(:)
  real*4, allocatable :: xcen(:), ycen(:), secMean(:), f(:,:,:)
  real*4, allocatable :: tmpDx(:,:), tmpDy(:,:), fieldDx(:,:), fieldDy(:,:)

  real*4 tiltAngles(LIMGRADSEC), dmagPerMicron(LIMGRADSEC), rotPerMicron(LIMGRADSEC)
  !
  logical rescale, blankOutput, adjustOrigin, hasWarp, fillTmp, fillNeeded, stripExtra
  logical readShrunk, numberedFromOne, twoDirections
  character dat * 9, timeStr * 8, tempExt * 9
  logical nbytes_and_flags
  character*80 titlech
  integer*4 inUnit, numInFiles, listTotal, numOutTot, numOutFiles, nxOut, nyOut, lmGrid
  integer*4 newMode, ifOffset, ifXform, nXforms, nLineUse, ifMean, ifFloat, ifWarping
  integer*4 nsum, ilist, iFile, iSecRead, loadYstart, loadYend, isec, isecOut
  real*4 xOffsAll, yOffsAll, fracZero, dminSpecified, dmaxSpecified, contrastLo
  real*4 zmin, zmax, diffMinMean, diffMaxMean, grandSum, sdSec, contrastHi
  real*4 grandMean, shiftMin, shiftMax, shiftMean, dminIn, dmaxIn, dmeanIn
  integer*4 iOutFile, numTruncLow, numTruncHigh, ifHeaderOut, ifTempOpen, nByteSymIn
  integer*4 nByteExtraIn, iFlagExtraIn, mode, nByteExtraOut, nByteSymOut, indExtraOut
  real*4 dmin, dmax, dmean, dmin2, dmax2, dmean2, optimalIn, optimalOut, bottomIn
  real*4 bottomOut, xcenIn, ycenIn, dx, dy, fieldMaxX, ystart, readReduction
  integer*4 linesLeft, numChunks, nextLine, iChunk, ifOutChunk, iscan, iyTest, iVerbose
  integer*4 iyBase, iy1, iy2, lnu, maxin, numScaleFacs, maxFieldX, needYfirst, needYlast
  real*4 dmeanSec, tmpMin, tmpMax, val, tsum2, scaleFactor
  integer*4 needYstart, needYend, numLinesLoad, numYload, numYchunk, iseriesBase, nyNeeded
  integer*4 ix1, ix2, nByteCopy, nByteClear, ifLinear, limEntered, insideTaper, indFile
  real*4 constAdd, densOutMin, dens, tmin2, tmax2, tmean2, avgSec, enteredSD, enteredMean
  integer*4 numInputFiles, numSecLists, numOutputFiles, numToGet, maxNxGrid, maxNyGrid
  integer*4 numOutValues, numOutEntries, ierr, ierr2, i, kti, iy, ind
  integer*4 maxFieldY, inputBinning, nxFirst, nyFirst, nxBin, nyBin
  integer*4 lenTemp, ierr3, applyFirst, numTaper, numberOffset, numExclude
  integer*4 ifOnePerFile, ifUseFill, listIncrement, indOut, ifMeanSdEntered
  integer*4 numReplace, isecReplace, modeOld, loadYoffset, loadBaseInd, listAlloc
  integer*4 indFilter, linesShrink, numAllSec, maxNumXF, nxMax, nyMax, ifControl
  integer*4 indFiltTemp, ifFiltSet, ifShrink
  real*4 rxOffset, ryOffset
  real*4 fieldMaxY, rotateAngle, expandFactor, fillVal, shrinkFactor
  real*8 dsum, dsumSq, tsum, tsumSq, wallStart, wallTime, loadTime, saveTime
  real*8 rotTime
  real*4 cosd, sind
  integer*4 taperAtFill, selectZoomFilter, zoomFiltInterp, numberInList
  integer*4 readCheckWarpFile
  integer*4 getLinearTransform, findMaxGridSize, getSizeAdjustedGrid
  character*320 concat
  !
  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean, PipGetLogical
  integer*4 PipGetString, PipGetTwoIntegers, PipGetFloatArray, PipGetFloat
  integer*4 PipGetIntegerArray, PipGetNonOptionArg, PipGetTwoFloats
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  newstack
  !
  integer numOptions
  parameter (numOptions = 47)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FNM:@output:OutputFile:FNM:@fileinlist:FileOfInputs:FN:@'// &
      'fileoutlist:FileOfOutputs:FN:@split:SplitStartingNumber:I:@'// &
      'append:AppendExtension:CH:@secs:SectionsToRead:LIM:@'// &
      'fromone:NumberedFromOne:B:@exclude:ExcludeSections:LI:@'// &
      'twodir:TwoDirectionTiltSeries:B:@skip:SkipSectionIncrement:I:@'// &
      'numout:NumberToOutput:IAM:@replace:ReplaceSections:LI:@blank:BlankOutput:B:@'// &
      'offset:OffsetsInXandY:FAM:@applyfirst:ApplyOffsetsFirst:B:@'// &
      'xform:TransformFile:FN:@uselines:UseTransformLines:LIM:@'// &
      'onexform:OneTransformPerFile:B:@rotate:RotateByAngle:F:@'// &
      'expand:ExpandByFactor:F:@shrink:ShrinkByFactor:F:@'// &
      'antialias:AntialiasFilter:I:@bin:BinByFactor:I:@distort:DistortionField:FN:@'// &
      'imagebinned:ImagesAreBinned:I:@fields:UseFields:LIM:@'// &
      'gradient:GradientFile:FN:@origin:AdjustOrigin:B:@'// &
      'linear:LinearInterpolation:B:@nearest:NearestNeighbor:B:@'// &
      'size:SizeToOutputInXandY:IP:@mode:ModeToOutput:I:@'// &
      'bytes:BytesSignedInOutput:I:@strip:StripExtraHeader:B:@'// &
      'float:FloatDensities:I:@meansd:MeanAndStandardDeviation:FP:@'// &
      'contrast:ContrastBlackWhite:IP:@scale:ScaleMinAndMax:FP:@'// &
      'multadd:MultiplyAndAdd:FPM:@fill:FillValue:F:@taper:TaperAtFill:IP:@'// &
      'memory:MemoryLimit:I:@test:TestLimits:IP:@verbose:VerboseOutput:I:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'newstack', &
      'ERROR: NEWSTACK - ', .true., 2, 2, 1, numOptArg, &
      numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0
  !
  ! defaults
  !
  inFileList = ' '
  outFileList = ' '
  numInputFiles = 0
  numOutputFiles = 0
  numSecLists = 0
  ifOnePerFile = 0
  ifDistort = 0
  ifMagGrad = 0
  ifMeanSdEntered = 0
  maxFieldY = 0
  idfFile = ' '
  magGradFile = ' '
  rotateAngle = 0.
  expandFactor = 0.
  iBinning = 1
  inputBinning = 1
  limToAlloc = 4 * LIMSEC
  lenTemp = MAXTEMP
  idimInOut = limToAlloc - 1
  limEntered = 0
  applyFirst = 0
  ifLinear = 0
  numScaleFacs = 0
  ifUseFill = 0
  blankOutput = .false.
  stripExtra = .false.
  adjustOrigin = .false.
  listIncrement = 1
  numReplace = 0
  iVerbose = 0
  limIfFail = 1950000000 / 4
  numTaper = 0
  insideTaper = 0
  loadTime = 0.
  saveTime = 0.
  rotTime = 0.
  maxExtraIn = 0
  maxExtraOut = 0
  indFilter = 5
  shrinkFactor = 1.
  linesShrink = 0
  iseriesBase = -1
  seriesExt = ' '
  maxNumXF = 40000
  ifWarping = 0
  ifControl = 0
  lmGrid = 200
  readReduction = 1.
  readShrunk = .false.
  numberedFromOne = .false.
  twoDirections = .false.
  numberOffset = 0
  numExclude = 0
  !
  ! Preliminary allocation of array
  allocate(array(limToAlloc), stat = ierr)
  call memoryError(ierr, 'ALLOCATING MAIN ARRAY')
  !
  ! read in list of input files
  !
  call ialprt(.false.)
  inUnit = 5
  !
  ! get number of input files and other preliminary items
  !
  if (pipinput) then
    ierr = PipGetInteger('VerboseOutput', iVerbose)
    ierr = PipGetString('FileOfInputs', inFileList)
    call PipNumberOfEntries('InputFile', numInputFiles)
    numInFiles = numInputFiles + max(0, numNonOptArg - 1)
    if (numInFiles > 0 .and. inFileList .ne. ' ') call exitError( &
        'YOU CANNOT ENTER BOTH INPUT FILES AND AN INPUT LIST FILE')
    if (inFileList .ne. ' ') numInFiles = -1
    call PipNumberOfEntries('SectionsToRead', numSecLists)
    ierr = PipGetInteger('SkipSectionIncrement', listIncrement)
    ierr = PipGetLogical('BlankOutput', blankOutput)
    ierr = PipGetLogical('StripExtraHeader', stripExtra)
    ierr = PipGetLogical('NumberedFromOne', numberedFromOne)
    ierr = PipGetLogical('TwoDirectionTiltSeries', twoDirections)
    if (numberedFromOne) numberOffset = 1
    if (PipGetInteger('BytesSignedInOutput', i) == 0) call overrideWriteBytes(i)
    i = 1
    if (PipGetString('ExcludeSections', listString) .eq. 0) then
      call parseList2(listString, inList, numExclude, LIMSEC)
      i = numExclude
    endif
    allocate(isecExclude(i), stat=ierr)
    call memoryError(ierr, 'ARRAY FOR EXCLUDED SECTIONS')
    if (numExclude > 0) then
      isecExclude(1:numExclude) = inList(1:numExclude) - numberOffset
    endif
  else
    write(*,'(1x,a,$)') '# of input files (or -1 to read list'// &
        ' of input files from file): '
    read(5,*) numInFiles
  endif
  !
  ! if it is negative, open a list file, set up input from 7
  !
  if (numInFiles == 0) call exitError('NO INPUT FILE SPECIFIED')
  if (numInFiles < 0) then
    inUnit = 7
    if (.not.pipinput) then
      write(*,'(1x,a,$)') 'Name of input list file: '
      read(5, 101) inFileList
    endif
    call dopen(7, inFileList, 'ro', 'f')
    read(inUnit,*) numInFiles
  endif
  listTotal = 0
  numAllSec = 0
  allocate(inFile(numInFiles), nlist(numInFiles), listInd(numInFiles),   &
      lineTmp(numInFiles), scaleFacs(numInFiles), scaleConsts(numInFiles),  stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR INPUT FILES')
  !
  nxMax = 0
  nyMax = 0
  if (twoDirections .and. numInFiles .ne. 2)  &
      call exitError('THERE MUST BE EXACTLY TWO INPUT FILES TO USE -twodir')
  do indFile = 1, numInFiles
    !
    ! get the next filename
    !
    if (pipinput .and. inUnit .ne. 7) then
      if (indFile <= numInputFiles) then
        ierr = PipGetString('InputFile', inFile(indFile))
      else
        ierr = PipGetNonOptionArg(indFile - numInputFiles, inFile(indFile))
      endif
    else
      if (inUnit .ne. 7) then
        if (numInFiles == 1) then
          write(*,'(1x,a,$)') 'Name of input file: '
        else
          write(*,'(1x,a,i3,a,$)') 'Name of input file #', indFile, ': '
        endif
      endif
      read(inUnit, 101) inFile(indFile)
    endif
    !
    ! open file to make sure it exists and get default section list
    !
    call imopen(1, inFile(indFile), 'RO')
    call irdhdr(1, nxyz, mxyz, mode, dmin2, dmax2, dmean2)
    if (mode == 16) call exitError('CANNOT WORK DIRECTLY WITH COLOR DATA'// &
        ' (MODE 16); USE COLORNEWST INSTEAD')
    if (indFile == 1) then
      nxFirst = nx
      nyFirst = ny
      call irtdel(1, deltafirst)
    endif
    call imclose(1)
    nxMax = max(nx, nxMax)
    nyMax = max(ny, nyMax)
    nlist(indFile) = nz
    do isec = 1, nz
      iy = isec - 1
      if (twoDirections .and. indFile == 1) iy = nz - isec
      inList(min(listTotal + isec, LIMSEC)) = iy
    enddo
    numAllSec = numAllSec + nz
    !
    ! get section list
    !
    if (.not.pipinput .or. inUnit == 7) then
      if (inUnit .ne. 7) print *,'Enter list of sections to read from' &
          //' file (/ for all, 1st sec is 0; ranges OK)'
      call rdlist2(inUnit, inList(listTotal + 1), nlist(indFile), LIMSEC - listTotal)
    elseif (indFile <= numSecLists) then
      ierr = PipGetString('SectionsToRead', listString)
      if (ierr == 0 .and. twoDirections)  &
          call exitError('YOU CANNOT ENTER SECTION LISTS WITH -twodir')
      call parseList2(listString, inList(listTotal + 1), nlist(indFile),  &
          LIMSEC - listTotal)
    endif
    !
    ! check list legality and whether excluded; copy over if not excluded
    !
    listInd(indFile) = listTotal + 1
    indOut = listInd(indFile)
    do isec = listTotal + 1, listTotal + nlist(indFile), max(1, listIncrement)
      inList(isec) = inList(isec) - numberOffset
      if (.not.blankOutput .and. &
          (inList(isec) < 0 .or. inList(isec) >= nz)) then
        write(*,'(/,a,i7,a,a)') 'ERROR: NEWSTACK -', inList(isec) + numberOffset, &
            ' IS AN ILLEGAL SECTION NUMBER FOR ', trim(inFile(indFile))
        call exit(1)
      endif
      if (numberInList(inList(isec), isecExclude, numExclude, 0) .eq. 0) then
        inList(indOut) = inList(isec)
        indOut = indOut + 1
      endif
    enddo
    nlist(indFile) = indOut - listInd(indFile)
    listTotal = listTotal + nlist(indFile)
  enddo
  close(7)
101 format(a)
  !
  maxNumXF = max(maxNumXF, numAllSec)
  listAlloc = listTotal + 10
  allocate(lineUse(listAlloc), listReplace(listAlloc), idfUse(listAlloc),  &
      xcen(listAlloc), ycen(listAlloc), secMean(listAlloc), f(2, 3, maxNumXF), &
      stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR INPUT FILES')
  !
  ! read in list of output files
  !
  inUnit = 5
  numOutTot = 0
  !
  ! get number of output files
  !
  if (pipinput) then
    ierr = PipGetString('FileOfOutputs', outFileList)
    call PipNumberOfEntries('OutputFile', numOutputFiles)
    numOutFiles = numOutputFiles + min(1, numNonOptArg)
    if (numOutFiles > 0 .and. outFileList .ne. ' ') call exitError( &
        'YOU CANNOT ENTER BOTH OUTPUT FILES AND AN OUTPUT'// &
        ' LIST FILE')
    if (outFileList .ne. ' ') numOutFiles = -1
    ierr = PipGetInteger('SplitStartingNumber', iseriesBase)
    if (iseriesBase >= 0 .and. numOutFiles .ne. 1) call exitError('THERE'// &
        ' MUST BE ONLY ONE OUTPUT FILE NAME FOR SERIES OF NUMBERED FILES')
    if (iseriesBase >= 0) numOutFiles = listTotal
    ierr = PipGetString('AppendExtension', seriesExt)
  else
    write(*,'(1x,a,$)') '# of output files (or -1 to read list'// &
        ' of output files from file): '
    read(5,*) numOutFiles
  endif
  if (numOutFiles == 0) call exitError('NO OUTPUT FILE SPECIFIED')
  !
  if (numOutFiles > 0) then
    allocate(outFile(numOutFiles), numSecOut(numOutFiles), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR OUTPUT FILES')
  endif
  !
  ! get list input
  !
  if (numOutFiles < 0) then
    inUnit = 7
    if (.not.pipinput) then
      write(*,'(1x,a,$)') 'Name of output list file: '
      read(5, 101) outFileList
    endif
    call dopen(7, outFileList, 'ro', 'f')
    read(inUnit,*) numOutFiles
    if (numOutFiles <= 0) call exitError('THE OUTPUT LIST FILE MUST START'// &
        ' WITH A POSITIVE NUMBER OF FILES TO OUTPUT')
    allocate(outFile(numOutFiles), numSecOut(numOutFiles), stat = ierr)
    call memoryError(ierr, 'ARRAYS FOR OUTPUT FILES')
  elseif (numOutFiles == 1 .and. .not.pipinput) then
    !
    ! get single output file
    !
    write(*,'(1x,a,$)') 'Name of output file: '
    read(5, 101) outFile(1)
    numSecOut(1) = listTotal
    numOutTot = listTotal
  endif
  !
  ! or get all the output files and the number of sections
  !
  if (numOutTot == 0) then
    if (pipinput .and. inUnit .ne. 7) then
      if (numOutFiles == 1) then
        numSecOut(1) = listTotal
      elseif (numOutFiles == listTotal) then
        do i = 1, numOutFiles
          numSecOut(i) = 1
        enddo
      else
        call PipNumberOfEntries('NumberToOutput', numOutEntries)
        if (numOutEntries == 0) &
            call exitError('YOU MUST SPECIFY NUMBER OF SECTIONS '// &
            'TO WRITE TO EACH OUTPUT FILE')

        numOutValues = 0
        do i = 1, numOutEntries
          numToGet = 0
          ierr = PipGetIntegerArray('NumberToOutput', &
              numSecOut(numOutValues + 1), numToGet, numOutFiles - numOutValues)
          numOutValues = numOutValues + numToGet
        enddo
        if (numOutValues .ne. numOutFiles) call exitError( &
            'THE NUMBER OF VALUES FOR SECTIONS TO OUTPUT DOES' &
            //' NOT EQUAL THE NUMBER OF OUTPUT FILES')
      endif
      do i = 1, numOutFiles
        if (i <= numOutputFiles) then
          ierr = PipGetString('OutputFile', outFile(i))
        else
          ierr = PipGetNonOptionArg(numNonOptArg, outFile(i))
        endif
        numOutTot = numOutTot + numSecOut(i)
      enddo
    else
      do i = 1, numOutFiles
        if (inUnit .ne. 7) write(*,'(1x,a,i3,a,$)') &
            'Name of output file #', i, ': '
        read(inUnit, 101) outFile(i)
        if (inUnit .ne. 7) write(*,'(1x,a,$)') &
            'Number of sections to store in that file: '
        read(inUnit,*) numSecOut(i)
        numOutTot = numOutTot + numSecOut(i)
      enddo
    endif
  endif
  !
  ! if series, now take the one filename as root name and make filenames
  if (iseriesBase >= 0) then
    ierr = alog10(10. * (iseriesBase + listTotal - 1))
    tempName = outFile(1)
    write(convFormat, 132) ierr, ierr
132 format('(i',i1,'.',i1,')')
    do i = 1, listTotal
      numSecOut(i) = 1
      write(convNum, convFormat) i + iseriesBase - 1
      if (seriesExt == ' ') then
        outFile(i) = trim(tempName)//'.'//trim(adjustl(convNum))
      else
        outFile(i) = trim(tempName)//trim(adjustl(convNum))//'.'//trim(seriesExt)
      endif
    enddo
    numOutTot = listTotal
  endif

  if (numOutTot .ne. listTotal) call exitError( &
      'Number of input and output sections does not match')
  !
  ! get new size and mode and offsets
  !
  nxOut = -1
  nyOut = -1
  newMode = -1
  xOffsAll = 0.
  yOffsAll = 0.
  ifOffset = 0
  if (pipinput) then
    ierr = PipGetTwoIntegers('SizeToOutputInXandY', nxOut, nyOut)
    ierr = PipGetInteger('ModeToOutput', newMode)
    call PipNumberOfEntries('OffsetsInXandY', numOutEntries)
    if (numOutEntries > 0) then
      ifOffset = 1
      numOutValues = 0
      do i = 1, numOutEntries
        numToGet = 0
        ierr = PipGetFloatArray('OffsetsInXandY', &
            array(numOutValues + 1), numToGet, LIMSEC * 2 - numOutValues)
        numOutValues = numOutValues + numToGet
      enddo
      if (numOutValues .ne. 2 .and. numOutValues .ne. 2 * listTotal) &
          call exitError('THERE MUST BE EITHER ONE OFFSET OR AN' &
          //' OFFSET FOR EACH SECTION')
      do i = 1, numOutValues / 2
        xcen(i) = array(2 * i - 1)
        ycen(i) = array(2 * i)
      enddo
      if (numOutValues == 2) ifOffset = -1
      xOffsAll = xcen(1)
      yOffsAll = ycen(1)
    endif
  else
    write(*,'(1x,a,$)') 'Output file X and Y dimensions'// &
        ' (/ for same as first input file): '
    read(5,*) nxOut, nyOut
    write(*,'(1x,a,$)') 'Output file data mode (/ for same as first input file): '
    read(5,*) newMode
    !
    ! get list of x, y coordinate offsets
    !
    write(*,'(1x,a,/,a,$)') '1 to offset centers of individual '// &
        'images,', '  -1 to apply same offset to all sections,'// &
        ' or 0 for no offsets: '
    read(5,*) ifOffset
    if (ifOffset > 0) then
      print *,'Enter X and Y center offsets for each section'
      read(5,*) (xcen(i), ycen(i), i = 1, listTotal)
    elseif (ifOffset < 0) then
      write(*,'(1x,a,$)') 'X and Y center offsets for all sections: '
      read(5,*) xOffsAll, yOffsAll
    endif
  endif
  !
  ! fill offset list if only one or none
  !
  if (ifOffset <= 0) then
    do i = 1, listTotal
      xcen(i) = xOffsAll
      ycen(i) = yOffsAll
    enddo
  endif
  !
  ! Get list of transforms
  !
  ifXform = 0
  if (pipinput) then
    ierr = PipGetBoolean('LinearInterpolation', ifLinear)
    if (PipGetString('TransformFile', xfFile) == 0) ifXform = 1
    ierr = PipGetBoolean('OneTransformPerFile', ifOnePerFile)
    ix1 = 0
    ierr = PipGetBoolean('NearestNeighbor', ix1)
    if (ix1 .ne. 0 .and. ifLinear .ne. 0) call exitError( &
        'YOU CANNOT ENTER BOTH -linear AND -nearest')
    if (ix1 .ne. 0) ifLinear = -1
  else
    write(*,'(1x,a,$)') '1 or 2 to transform images with cubic or' &
        //' linear interpolation, 0 not to: '
    read(5,*) ifXform
    if (ifXform .ne. 0) then
      write(*,'(1x,a,$)') 'Name of transform file: '
      read(5, 101) xfFile
    endif
    if (ifXform > 1) ifLinear = 1
  endif
  if (ifXform .ne. 0) then
    !
    ! read transforms, set default to section list unless there is just
    ! one line in file
    !
    ierr = readCheckWarpFile(xfFile, 0, 1, idfNx, idfNy, nXforms, &
        idfBinning, pixelSize,  iWarpFlags, listString)
    if (ierr < -1) call exitError(listString)
    if (ierr >= 0) then
      write(*,'(a,a)') 'Warping file opened: ', trim(xfFile)
      ifWarping = 1
      if (mod(iWarpFlags / 2, 2) .ne. 0) ifControl = 1
      if (nXforms > maxNumXF) call exitError( &
          'TOO MANY SECTIONS IN WARPING FILE FOR TRANSFORM ARRAY')
      warpScale = pixelSize / deltafirst(1)
      do i = 1, nXforms
        if (getLinearTransform(i, f(1, 1, i)) .ne. 0) &
            call exitError('GETTING LINEAR TRANSFORM FROM WARP FILE')
        f(1, 3, i) = f(1, 3, i) * warpScale
        f(2, 3, i) = f(2, 3, i) * warpScale
      enddo
      numFields = nXforms
    else
      call dopen(3, xfFile, 'ro', 'f')
      call xfrdall2(3, f, nXforms, maxNumXF, ierr)
      if (ierr == 2) call exitError('READING TRANSFORM FILE')
      if (ierr == 1) call exitError( &
          'TOO MANY TRANSFORMS IN FILE FOR TRANSFORM ARRAY')
      close(3)
    endif
    if (nXforms == 0) call exitError('THE TRANSFORM FILE CONTAINS NO TRANSFORMS')

    call getItemsToUse(nXforms, listTotal, inList, 'UseTransformLines', &
        listString, pipinput, 'TRANSFORM LINE', ifOnePerFile, numInFiles, &
        lineUse, nLineUse, numberOffset, listAlloc)

    if (ifOnePerFile > 0) then
      if (nLineUse < numInFiles) call exitError( &
          'NOT ENOUGH TRANSFORMS SPECIFIED FOR THE INPUT FILES')
      !
      ! Copy list to temp array and build list with line for each sec
      !
      do iy = 1, numInFiles
        lineTmp(iy) = lineUse(iy)
      enddo
      nLineUse = 0
      do iy = 1, numInFiles
        do i = 1, nlist(iy)
          nLineUse = nLineUse + 1
          lineUse(nLineUse) = lineTmp(iy)
        enddo
      enddo
    endif
    !
    ! use single number for all sections
    !
    xfText = ', transformed'
    if (ifWarping .ne. 0) xfText = ', warped'
    if (nLineUse == 1) then
      do i = 2, listTotal
        lineUse(i) = lineUse(1)
      enddo
      nLineUse = listTotal
    endif
    if (nLineUse .ne. listTotal) call exitError( &
        'Specified # of transform lines does not match # of sections')
  endif
  !
  ! find out if root beer float or what
  !
  fracZero = 0.
  ifMean = 0
  ifFloat = 0
  dminSpecified = 0
  dmaxSpecified = 0
  contrastLo = 0
  contrastHi = 255
  if (pipinput) then
    ierr = 1 - PipGetTwoFloats('ContrastBlackWhite', contrastLo, contrastHi)
    ierr2 = 1 - PipGetTwoFloats('ScaleMinAndMax', dminSpecified, dmaxSpecified)
    ierr3 = 1 - PipGetInteger('FloatDensities', ifFloat)
    ifMeanSdEntered = 1 - PipGetTwoFloats('MeanAndStandardDeviation', enteredMean, &
        enteredSD)
    call PipNumberOfEntries('MultiplyAndAdd', numScaleFacs)
    if (ifMeanSdEntered .ne. 0) then
      if ((ierr3 .ne. 0 .and. ifFloat .ne. 2) .or. ierr + ierr2 + numScaleFacs > 0)  &
          call exitError('You cannot use -meansd with any scaling option except -float 2')
      ierr3 = 1
      ifFloat = 2
    endif
    ifUseFill = 1 - PipGetFloat('FillValue', fillVal)
    if (ifFloat >= 4) then
      if (ierr2 == 0) call exitError &
          ('You must enter -scale with -float 4')
    else
      if (ierr + ierr2 + ierr3 + min(numScaleFacs, 1) > 1) &
          call exitError('The -scale, -contrast, -multadd, and -float '// &
          'options are mutually exclusive except with -float 4')
      if (ifFloat < 0) call exitError('You must use -contrast or ' &
          //'-scale instead of a negative -float entry')
      if (ierr .ne. 0) ifFloat = -2
      if (ierr2 .ne. 0 .or. numScaleFacs .ne. 0) ifFloat = -1
      !
      ! get scale factors, make sure there are right number
      !
      if (numScaleFacs > 0) then
        if (numScaleFacs .ne. 1 .and. numScaleFacs .ne. numInFiles) &
            call exitError('You must enter -multadd either once '// &
            'or once per input file')
        do i = 1, numScaleFacs
          ierr = PipGetTwoFloats('MultiplyAndAdd', scaleFacs(i), &
              scaleConsts(i))
        enddo
      endif
    endif
  else
    write(*,102)
102 format(' Enter 0 for no floating',/,8x, &
        '-2 to scale to bytes based on black and white contrast ', &
        'levels',/,8x, &
        '-1 to specify a single rescaling of all sections' &
        ,/,8x,' 1 to float all sections to same range',/,8x,' ', &
        '2 to float all sections to same mean & standard deviation' &
        ,/,8x,' 3 to shift sections to same mean without scaling',/ &
        ,6x,'or 4 to shift to same mean and specify a single', &
        ' rescaling: ',$)
    read(5,*) ifFloat
  endif
  if (ifFloat > 1) ifMean = 1
  if (ifFloat < 0) then
    floatText = ', densities scaled'
    if (ifFloat == -1 .and. .not.pipinput) then
      write(*,'(1x,a,/,a,$)') 'Values to scale input file''s'// &
          ' min and max to,', '   or / to scale to maximum range,' &
          //' or 1,1 to override mode scaling: '
      read(5,*) dminSpecified, dmaxSpecified
    elseif (ifFloat < -1) then
      if (.not.pipinput) then
        write(*,'(1x,a,$)') 'Contrast ramp black and white settings ' &
            //'(values between 0 and 255): '
        read(5,*) contrastLo, contrastHi
      endif
      contrastHi = max(contrastHi, contrastLo + 1.)
      dminSpecified = -contrastLo * 255 / (contrastHi - contrastLo)
      dmaxSpecified = dminSpecified + 65025 / (contrastHi - contrastLo)
      ! print *,contrastLo, contrastHi, dminSpecified, dmaxSpecified
    endif
  endif
  !
  ! get new options
  !
  if (pipinput) then
    ierr = PipGetBoolean('ApplyOffsetsFirst', applyFirst)
    ierr = PipGetFloat('RotateByAngle', rotateAngle)
    ierr = PipGetFloat('ExpandByFactor', expandFactor)
    ierr = PipGetInteger('BinByFactor', iBinning)
    ierr = PipGetString('DistortionField', idfFile)
    ierr = PipGetString('GradientFile', magGradFile)
    ierr = PipGetLogical('AdjustOrigin', adjustOrigin)
    ierr = PipGetTwoIntegers('TaperAtFill', numTaper, insideTaper)
    !
    ! Memory limits
    limEntered = 1 - PipGetTwoIntegers('TestLimits', ierr, lenTemp)
    if (limEntered > 0) limToAlloc = ierr
    if (PipGetInteger('MemoryLimit', ierr) == 0) then
      limEntered = 1
      limToAlloc = int(ierr, kind = 8) * 1024 * 256
    endif
    if (limEntered > 0) then
      if (limToAlloc < 1000 .or. lenTemp < 1 .or. lenTemp > &
          limToAlloc / 2) call exitError('INAPPROPRIATE MEMORY LIMITS ENTERED')
      idimInOut = limToAlloc - lenTemp
      call reallocateArray()
    endif
    !
    if (ifWarping .ne. 0 .and. (idfFile .ne. ' ' .or. magGradFile .ne. ' ')) call &
        exitError('YOU CANNOT USE DISTORTION CORRECTIONS WITH WARPING TRANSFORMS')
    !
    if (ifWarping .ne. 0 .and. (rotateAngle .ne. 0 .or. expandFactor .ne. 0.)) call &
        exitError('YOU CANNOT USE -expand or -rotate WITH WARPING TRANSFORMS')
    if (iBinning <= 0) call exitError('BINNING FACTOR MUST BE A POSITIVE NUMBER')
    readReduction = iBinning
    !
    ! Get filter entry and if there is binning and no separate shrink entry, convert
    ! the binning to a shrinkage.  Also allow a negative filter entry to set the default
    indFiltTemp = indFilter
    ifFiltSet = 1 - PipGetInteger('AntialiasFilter', indFiltTemp)
    if (indFiltTemp < 0) indFiltTemp = indFilter
    ifShrink = 1 - PipGetFloat('ShrinkByFactor', shrinkFactor)
    if (ifFiltSet > 0 .and. ifShrink == 0 .and. iBinning > 1 .and. indFiltTemp > 0) then
      shrinkFactor = iBinning
      iBinning = 1
      print *,'Doing antialias-filtered image reduction instead of ordinary binning'
    endif
    indFilter = max(0, indFiltTemp - 1)
    !
    ! Handle shrinkage
    if (ifShrink > 0 .or. shrinkFactor > 1.) then
      !
      ! Do shrinkage on input unless there is binning specified, since this will be
      ! more memory-efficient by default and it will produce a correct origin by default
      ! with no size change
      readShrunk = iBinning == 1
      if (iBinning > 1 .and. (ifXform .ne. 0 .or. rotateAngle .ne. 0. .or. &
          expandFactor .ne. 0. .or.  idfFile .ne. ' ' .or. magGradFile .ne. ' ' .or. &
          ifWarping .ne. 0)) call exitError('YOU CANNOT USE BOTH -shrink '// &
          'AND -bin WITH -xform, -rotate, -expand, -distort, or -gradient')
      if (shrinkFactor <= 1.) call exitError('FACTOR FOR -shrink MUST BE GREATER THAN 1')
      if (ifWarping .ne. 0 .and. abs(nint(shrinkFactor) - shrinkFactor) > 1.e-4) call  &
          exitError('YOU CANNOT USE -shrink WITH WARPING UNLESS THE FACTOR IS AN INTEGER')
      ierr = 1
      i = indFilter
      do while (ierr == 1)
        ierr = selectZoomFilter(indFilter, 1. / shrinkFactor, linesShrink)
        if (ierr == 1) indFilter = indFilter - 1
        if (ierr > 1) call exitError( 'SELECTING ANTIALIASING FILTER')
      enddo
      if (indFilter < i) print *,'Using the last antialiasing filter, #', indFilter + 1
      if (readShrunk) then
        readReduction = shrinkFactor
        linesShrink = 0
      else
        !
        ! Post-read shrinkage: provide extra buffer of what needs reading in for a chunk
        ! and set an expansion factor
        linesShrink = linesShrink / 2 + 2
        expandFactor = 1. / shrinkFactor
      endif
      if (iVerbose > 0) print *,'Shrinking; readShrunk ', readShrunk
    endif
    !
    ! Section replacement
    if (PipGetString('ReplaceSections', listString) == 0) then
      call parseList2(listString, listReplace, numReplace, listTotal)
      if (numReplace > 0) then
        ! print *,'replacing', (listReplace(i), i = 1, numReplace)
        if (numOutFiles > 1) call exitError( &
            'THERE MUST BE ONLY ONE OUTPUT FILE TO USE -replace')
        call ialprt(.true.)
        call imopen(2, outFile(1), 'OLD')
        call irdhdr(2, nxyz2, mxyz2, modeOld, dmin, dmax, dmean)
        call ialprt(.false.)
        do i = 1, numReplace
          listReplace(i) = listReplace(i) - numberOffset
          if (listReplace(i) < 0 .or. listReplace(i) >= nxyz2(3)) &
              call exitError('REPLACEMENT SECTION NUMBER OUT OF RANGE')
        enddo
      endif
    endif
    !
    ! Distortion field
    if (idfFile .ne. ' ') then
      ifDistort = 1
      xfText = ', undistorted'

      ierr = readCheckWarpFile(idfFile, 1, 1, idfNx, idfNy, numFields, &
          idfBinning, pixelSize, iWarpFlags, listString)
      if (ierr < 0) call exitError(listString)
      !
      if (PipGetInteger('ImagesAreBinned', inputBinning) .ne. 0) then
        if (nxFirst <= idfNx * idfBinning / 2 .and. &
            nyFirst <= idfNy * idfBinning / 2) call exitError &
            ('YOU MUST SPECIFY BINNING OF IMAGES BECAUSE THEY '// &
            'ARE NOT LARGER THAN HALF THE CAMERA SIZE')
      endif
      if (inputBinning <= 0) call exitError &
          ('IMAGE BINNING MUST BE A POSITIVE NUMBER')
      warpScale = float(idfBinning) / inputBinning
      !
      ! Set up default field numbers to use then process use list if any
      !
      call getItemsToUse(numFields, listTotal, inList, 'UseFields', listString, &
          pipinput, 'FIELD', 0, 0, idfUse, numIdfUse, numberOffset, listAlloc)
      if (numIdfUse == 1) then
        do i = 2, listTotal
          idfUse(i) = idfUse(1)
        enddo
        numIdfUse = listTotal
      endif
      if (numIdfUse .ne. listTotal) call exitError( &
          'Specified # of fields does not match # of sections')

    endif
    !
    ! get mag gradient information; multiply pixel size by binning
    !
    if (magGradFile .ne. ' ') then
      ifMagGrad = 1
      xfText = ', undistorted'
      call readMagGradients(magGradFile, LIMGRADSEC, pixelMagGrad, axisRot, &
          tiltAngles, dmagPerMicron, rotPerMicron, numMagGrad)
      pixelMagGrad = pixelMagGrad * readReduction
    endif
  endif
  call PipDone()
  !
  ! if not transforming and distorting, rotating, or expanding, set up
  ! a unit transform
  !
  if (ifXform == 0 .and. (ifDistort .ne. 0 .or. ifMagGrad .ne. 0 .or. &
      rotateAngle .ne. 0. .or. expandFactor .ne. 0.)) then
    ifXform = 1
    call xfunit(f(1, 1, 1), 1.)
    do i = 1, listTotal
      lineUse(i) = 0
    enddo
    nLineUse = listTotal
    nXforms = 1
    do while (rotateAngle > 180.01 .or. rotateAngle < -180.01)
      rotateAngle = rotateAngle - sign(360., rotateAngle)
    enddo
  endif
  !
  ! set up rotation and expansion transforms and multiply by transforms
  !
  if (rotateAngle .ne. 0. .or. expandFactor .ne. 0.) then
    call xfunit(frot, 1.)
    if (rotateAngle .ne. 0.) then
      frot(1, 1) = cosd(rotateAngle)
      frot(1, 2) = -sind(rotateAngle)
      frot(2, 2) = frot(1, 1)
      frot(2, 1) = -frot(1, 2)
      ! This was needed to correct for cubInterp rotating off center,
      ! changed 10/12/07
      ! frot(1, 3) = 0.5 * (frot(1, 1) + frot(1, 2)) - 0.5
      ! frot(2, 3) = 0.5 * (frot(2, 1) + frot(2, 2)) - 0.5
    endif
    if (expandFactor == 0.) expandFactor = 1.
    call xfunit(fexp, expandFactor)
    call xfmult(frot, fexp, fprod)
    ! print *,'transform', ((fprod(i, iy), i=1, 2), iy=1, 3)
    do i = 1, nXforms
      call xfmult(f(1, 1, i), fprod, frot)
      call xfcopy(frot, f(1, 1, i))
    enddo
  endif
  if (expandFactor == 0.) expandFactor = 1.
  !
  ! adjust xcen, ycen and transforms if binning and allocate temp space
  !
  if (readReduction > 1.) then
    do i = 1, listTotal
      xcen(i) = xcen(i) / readReduction
      ycen(i) = ycen(i) / readReduction
    enddo
    if (ifXform .ne. 0) then
      do i = 1, nXforms
        f(1, 3, i) = f(1, 3, i) / readReduction
        f(2, 3, i) = f(2, 3, i) / readReduction
      enddo
    endif
    idimInOut = limToAlloc - lenTemp
  else
    idimInOut = limToAlloc - 1
  endif
  !
  if (ifFloat > 0) then
    floatText = ', floated to range'
    if (fracZero .ne. 0.) &
        write(trunctText, '(a,f6.3)') ', truncated by', fracZero
    if (ifMean .ne. 0) then
      if (ifFloat == 2) then
        floatText = ', floated to means'
        zmin = 1.e10
        zmax = -1.e10
        allocate(secZmins(numOutTot), secZmaxes(numOutTot), ztemp(numOutTot), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR Z MIN/MAX')
      else
        diffMinMean = 0.
        diffMaxMean = 0.
        grandSum = 0.
        nsum = 0.
        if (ifFloat == 3) then
          floatText = ',  shifted to means'
        else
          floatText = ', mean shift&scaled'
          if (.not.pipinput) then
            write(*,'(1x,a,/,a,$)') 'Values to scale the shifted'// &
                ' min and max to,', '   or / to scale to maximum'// &
                ' range: '
            read(5,*) dminSpecified, dmaxSpecified
          endif
        endif
      endif
    endif
      !
      ! if means, need to read all sections to get means
      !
    if (ifMean .ne. 0 .and. ifMeanSdEntered == 0) then
      do iFile = 1, numInFiles
        call imopen(1, inFile(iFile), 'RO')
        call irdhdr(1, nxyz, mxyz, mode, dmin2, dmax2, dmean2)
        !
        ! get the binned size to read
        !
        call getReducedSize(nx, readReduction, readShrunk, nxBin, rxOffset)
        call getReducedSize(ny, readReduction, readShrunk, nyBin, ryOffset)
        !
        nyNeeded = nyBin
        call reallocateIfNeeded()
        !
        do ilist = 1, nlist(iFile)
          ind = ilist + listInd(iFile) - 1
          iSecRead = inList(ind)
          if (iSecRead >= 0 .and. iSecRead < nz) then
            !
            if (iVerbose > 0) print *,'scanning for mean/sd', iSecRead
            call scanSection(array, idimInOut, nxBin, nyBin, 0, readReduction, rxOffset, &
                ryOffset, iSecRead, ifFloat, dmin2, dmax2, dmean2, sdSec, loadYstart, &
                loadYend, indFilter, readShrunk, array(idimInOut + 1), lenTemp)
            secMean(ind) = dmean2
            !
            if (ifFloat == 2) then
              !
              ! find the min and max Z values ((density-mean) /sd)
              !
              secZmins(ind) = 0.
              secZmaxes(ind) = 0.
              if (dmax2 > dmin2 .and. sdSec > 0.) then
                secZmins(ind) = (dmin2 - dmean2) / sdSec
                secZmaxes(ind) = (dmax2 - dmean2) / sdSec
              endif
            else
              !
              ! or, if shifting, get maximum range from mean
              !
              diffMinMean = min(diffMinMean, dmin2 - dmean2)
              diffMaxMean = max(diffMaxMean, dmax2 - dmean2)
              grandSum = grandSum + dmean2
              nsum = nsum + 1
            endif
          endif
        enddo
        call imclose(1)
      enddo
      !
      ! for shift to mean, figure out new mean, min and max and whether
      ! scaling will be needed to fit range
      !
      if (ifFloat > 2) then
        grandMean = grandSum / nsum
        shiftMin = max(0., grandMean + diffMinMean)
        shiftMean = shiftMin - diffMinMean
        shiftMax = grandMean + diffMaxMean
        if (ifFloat == 3 .and. mode .ne. 2 .and. newMode .ne. 2 .and. &
            optimalMax(mode + 1) < shiftMax) then
          print *,'Densities will be compressed by', &
              optimalMax(mode + 1) / shiftMax, ' to fit in range'
          floatText = ', mean shift&scaled'
        endif
      endif
      !
      ! For float to mean, find outliers and get zmin and zmax without them
      if (ifFloat == 2) then
        !call rsMedian(secZmaxes, numOutTot, ztemp, zmaxMed)
        !call rsMADN(secZmaxes, numOutTot, zmaxMed, ztemp, zmaxMADN)
        !print *,'median', zmaxMed, '   MADN', zmaxMADN
        !write(*,'(8f9.2)')(ztemp(i) / zmaxMADN, i = 1, numOutTot)
        isecOut =   0
        call rsMadMedianOutliers(secZmins, numOutTot, 8., ztemp)
        do i = 1, numOutTot
          if (ztemp(i) >= 0.) then
            zmin = min(zmin, secZmins(i))
          else
            isecOut = isecOut + 1
          endif
        enddo
        call rsMadMedianOutliers(secZmaxes, numOutTot, 8., ztemp)
        do i = 1, numOutTot
          if (ztemp(i) <= 0.) then
            zmax = max(zmax, secZmaxes(i))
          else
            isecOut = isecOut + 1
          endif
        enddo
        deallocate(secZmins, secZmaxes, ztemp)
        if (isecOut > 0) write(*,'(/,a,i3,a)')'WARNING: NEWSTACK - ',isecOut, &
            ' sections have extreme ranges and will be truncated to preserve dynamic'// &
            ' range'
      endif
    endif
  endif
  !
  ! start looping over input images
  !
  isec = 1
  isecOut = 1
  isecReplace = 1
  iOutFile = 1
  call ialprt(.true.)
  call time(timeStr)
  call b3dDate(dat)
  numTruncLow = 0
  numTruncHigh = 0
  ifHeaderOut = 0
  ifTempOpen = 0
  do iFile = 1, numInFiles
    call imopen(1, inFile(iFile), 'RO')
    call irdhdr(1, nxyz, mxyz, mode, dminIn, dmaxIn, dmeanIn)
    call irtsiz(1, nxyz, mxyz, nxyzst)
    call irtcel(1, cell)
    !
    ! get the binned size to read
    !
    call getReducedSize(nx, readReduction, readShrunk, nxBin, rxOffset)
    call getReducedSize(ny, readReduction, readShrunk, nyBin, ryOffset)
    if (iVerbose > 0) &
        print *,'Size and offsets X:', nxBin, rxOffset, ', Y:', nyBin, ryOffset
    !
    ! get extra header information if any
    !
    call irtnbsym(1, nByteSymIn)
    if (nByteSymIn > 0) then
      !
      ! Deallocate array if it was allocated an is not big enough
      if (maxExtraIn > 0 .and. nByteSymIn > maxExtraIn) then
        deallocate(extraIn, stat = ierr)
        maxExtraIn = 0
      endif
      !
      ! Allocate array if needed
      if (maxExtraIn == 0) then
        maxExtraIn = nByteSymIn + 1024
        allocate(extraIn(maxExtraIn), stat = ierr)
        if (ierr .ne. 0) call exitError('ALLOCATING MEMORY FOR EXTRA HEADER ARRAYS')
      endif
      call irtsym(1, nByteSymIn, extraIn)
      call irtsymtyp(1, nByteExtraIn, iFlagExtraIn)
      !
      ! DNM 4/18/02: if these numbers do not represent bytes and
      ! flags, then number of bytes is 4 times nint + nreal
      !
      if (.not.nbytes_and_flags(nByteExtraIn, iFlagExtraIn)) &
          nByteExtraIn = 4 * (nByteExtraIn + iFlagExtraIn)
    endif
    !
    ! get each section in input file
    !
    do ilist = 1, nlist(iFile)
      iSecRead = inList(ilist + listInd(iFile) - 1)
      !
      ! set output characteristics from first section, transposing size
      ! for 90 degree rotation and resizing as necessary
      !
      if (isec == 1) then
        if (abs(abs(rotateAngle) - 90.) < 0.1 .and. nxOut <= 0 .and. &
            nyOut <= 0) then
          nxOut = nint(nyBin * expandFactor)
          nyOut = nint(nxBin * expandFactor)
        endif
        if (nxOut <= 0) nxOut = nint(nxBin * expandFactor)
        if (nyOut <= 0) nyOut = nint(nyBin * expandFactor)
        if (newMode < 0) newMode = mode
        !
        ! if warping or distortions, figure out how big to allocate the arrays
        if (ifDistort + ifWarping .ne. 0) then
          allocate(nControl(numFields), stat = ierr)
          call memoryError(ierr, 'ARRAY FOR NUMBER OF CONTROL POINTS')
          dx = 0.
          dy = 0.
          !
          ! Expanded grid size is based on the input size for distortion and the
          ! output size for warping
          if (ifDistort .ne. 0) then
            xnBig = nxMax / warpScale
            ynbig = nyMax / warpScale
          else
            xnBig = readReduction * nxOut / warpScale
            ynbig = readReduction * nyOut / warpScale
            if (applyFirst == 0) then
              do i = 1, listTotal
                dx = 1.e20
                dy = 1.e20
                xnBig = -dx
                xnBig = -dx
                xnBig = max(xnBig, (nxOut + xcen(i)) * readReduction / warpScale)
                dx = min(dx, xcen(i) * readReduction / warpScale)
                ynbig = max(ynbig, (nyOut + ycen(i)) * readReduction / warpScale)
                dy = min(dy, ycen(i) * readReduction / warpScale)
              enddo
            endif
          endif
          if (findMaxGridSize(dx, xnBig, dy, ynbig, nControl, maxNxGrid, maxNyGrid, &
              listString) .ne. 0) call exitError(listString)
          ! print *,maxNxGrid, maxNyGrid
          lmGrid = max(lmGrid, maxNxGrid, maxNyGrid)
        endif
        if (ifDistort + ifMagGrad + ifWarping .ne. 0) then
          allocate(fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid), &
              tmpDx(lmGrid, lmGrid), tmpDy(lmGrid, lmGrid), stat = ierr)
          call memoryError(ierr, 'ARRAYS FOR WARPING FIELDS')
        endif
      endif
      !
      if (numTaper == 1) then
        numTaper = min(127, max(16, nint((nxOut + nyOut) / 200.)))
        write(*,'(/,a,i4,a)') 'Tapering will be done over', numTaper, ' pixels'
      endif
      !
      ! First see if this is the first section to replace
      if (numReplace > 0 .and. isecReplace == 1) then
        if (nxOut .ne. nxyz2(1) .or. nyOut .ne. nxyz2(2)) call exitError( &
            'EXISTING OUTPUT FILE DOES NOT HAVE RIGHT SIZE IN X OR Y')
        if (newMode .ne. modeOld) call exitError( &
            'OUTPUT MODE DOES NOT MATCH EXISTING OUTPUT FILE')
        isecOut = listReplace(isecReplace) + 1
      endif
      !
      ! then see if need to open an output file
      if (numReplace == 0 .and. isecOut == 1) then
        !
        ! Create output file, transfer header from currently open file,
        ! fix it
        !
        call imopen(2, outFile(iOutFile), 'NEW')
        call itrhdr(2, 1)
        call ialmod(2, newMode)
        !
        ! set new size, keep old nxyzst
        !
        nxyz2(1) = nxOut
        nxyz2(2) = nyOut
        nxyz2(3) = numSecOut(iOutFile)
        call ialsiz(2, nxyz2, nxyzst)
        !
        ! if mxyz=nxyz, keep this relationship
        !
        if (mxyz(1) == nx .and. mxyz(2) == ny .and. mxyz(3) == nz) then
          mxyz2(1) = nxOut
          mxyz2(2) = nyOut
          mxyz2(3) = numSecOut(iOutFile)
          call ialsam(2, mxyz2)
        else
          mxyz2(1) = mxyz(1)
          mxyz2(2) = mxyz(2)
          mxyz2(3) = mxyz(3)
        endif
        !
        ! keep delta the same by scaling cell size from change in mxyz
        !
        do i = 1, 3
          cell2(i) = mxyz2(i) * (cell(i) / mxyz(i)) * readReduction / expandFactor
          cell2(i + 3) = 90.
        enddo
        cell2(3) = mxyz2(3) * (cell(3) / mxyz(3))
        call ialcel(2, cell2)
        !
        ! shift origin by the fraction pixel offset when binning or reducing with read
        ! a positive change is needed to indicate origin inside image
        ! When reduction was added, removed a convoluted subpixel adjustment to this
        ! that was wrong as basis for adjusting origin for size changes
        call irtdel(1, delta)
        call irtorg(1, xOrigin, yOrigin, zOrigin)
        if (readReduction > 1) then
          xOrigin = xOrigin - delta(1) * rxOffset
          yOrigin = yOrigin - delta(2) * ryOffset
        endif
        !
        ! Adjust origin if requested: it is different depending on whether
        ! there are transforms and whether offset was applied before or
        ! after.  delta can be modified, it will be reread
        if (adjustOrigin) then
          zOrigin = zOrigin - iSecRead * delta(3)
          delta(1) = delta(1) * readReduction / expandFactor
          delta(2) = delta(2) * readReduction / expandFactor
          if (ifXform == 0) then
            xOrigin = xOrigin - (nxBin / 2 + nint(xcen(isec)) - nxOut / 2) * delta(1)
            yOrigin = yOrigin - (nyBin / 2 + nint(ycen(isec)) - nyOut / 2) * delta(2)
          elseif (applyFirst .ne. 0) then
            xOrigin = xOrigin - (expandFactor * (nxBin / 2. + xcen(isec))  - nxOut / 2.) &
                * delta(1)
            yOrigin = yOrigin - (expandFactor * (nyBin / 2. + ycen(isec)) - nyOut / 2.)  &
                * delta(2)
          else
            xOrigin = xOrigin - (expandFactor * nxBin / 2. + xcen(isec) - nxOut / 2.) *  &
                delta(1)
            yOrigin = yOrigin - (expandFactor * nyBin / 2. + ycen(isec) - nyOut / 2.) *  &
                delta(2)
          endif
        endif
        if (adjustOrigin .or. readReduction > 1) &
            call ialorg(2, xOrigin, yOrigin, zOrigin)
        !
        if (trunctText == ' ') then
          write(titlech, 302) xfText, floatText, dat, timeStr
        else
          write(titlech, 301) xfText, floatText, trunctText
        endif
        read(titlech, '(20a4)') (title(kti), kti = 1, 20)
301     format('NEWSTACK: Images copied',a13,a18,a20)
302     format('NEWSTACK: Images copied',a13,a18,t57,a9,2x,a8)
        dmax = -1.e30
        dmin = 1.e30
        dmean = 0.
        !
        ! adjust extra header information if currently open file has it
        !
        nByteSymOut = 0
        if (nByteSymIn > 0 .and. .not.stripExtra) then
          nByteExtraOut = nByteExtraIn
          nByteSymOut = numSecOut(iOutFile) * nByteExtraOut
          if (maxExtraOut > 0 .and. nByteSymOut > maxExtraOut) then
            deallocate(extraOut, stat = ierr)
            maxExtraOut = 0
          endif
          !
          ! Allocate array if needed
          if (maxExtraOut == 0) then
            maxExtraOut = nByteSymOut + 1024
            allocate(extraOut(maxExtraOut), stat = ierr)
            call memoryError(ierr, 'ARRAYS FOR EXTRA HEADER DATA')
          endif
          call ialnbsym(2, nByteSymOut)
          call imposn(2, 0, 0)
          indExtraOut = 0
        else
          call ialnbsym(2, 0)
        endif
      endif
      !
      ! handle complex images here and skip out
      !
      if ((newMode + 1) / 2 == 2 .or. (mode + 1) / 2 == 2) then
        if ((mode + 1) / 2 .ne. 2 .or. (newMode + 1) / 2 .ne. 2) call exitError( &
            'ALL INPUT FILES MUST BE COMPLEX IF ANY ARE')

        if (limEntered == 0 .and. nx * ny * 2 > idimInOut) then
          idimInOut = nx * ny * 2
          lenTemp = 1
          limToAlloc = idimInOut + 1
          call reallocateArray()
        endif
        if (nx * ny * 2 > idimInOut) call exitError('INPUT IMAGE TOO LARGE FOR ARRAY.')

        call imposn(1, iSecRead, 0)
        call irdsec(1, array,*99)
        call iclcdn(array, nx, ny, 1, nx, 1, ny, dmin2, dmax2, dmean2)
        call imposn(2, isecOut - 1, 0)
        call iwrsec(2, array)
        go to 80
      endif
      !
      ! determine whether rescaling will be needed
      !
      optimalIn = optimalMax(mode + 1)
      optimalOut = optimalMax(newMode + 1)
      !
      ! set bottom of input range to 0 unless mode 1 or 2; set bottom
      ! of output range to 0 unless not changing modes
      !
      bottomIn = 0.
      if (dminIn < 0. .and. (mode == 1 .or. mode == 2)) bottomIn = -optimalIn
      bottomOut = 0.
      if (mode == newMode) bottomOut = bottomIn
      rescale = .false.
      !
      ! for no float: if either mode = 2, no rescale;
      ! otherwise rescale from input range to output range only if
      ! mode is changing
      !
      if (ifFloat == 0 .and. newMode .ne. 2 .and. mode .ne. 2) then
        rescale = mode .ne. newMode
      elseif (ifFloat .ne. 0) then
        rescale = .true.
      endif
      !
      ! Handle blank images here and skip out
      !
      if (iSecRead < 0 .or. iSecRead >= nz) then
        tmpMin = dmeanIn
        if (ifUseFill .ne. 0) tmpMin = fillVal
        tmpMax = tmpMin
        dsumSq = 0.
        dsum = tmpMin * (float(nxOut) * nyOut)
        nyNeeded = 1
        call reallocateIfNeeded()
        call findScaleFactors(tmpMin, tmpMax)
        do i = 1, nxOut
          array(i) = tmpMin * scaleFactor + constAdd
        enddo
        call imposn(2, isecOut - 1, 0)
        do i = 1, nyOut
          call iwrlin(2, array)
        enddo
        dmin2 = array(1)
        dmax2 = dmin2
        dmean2 = dmin2
        go to 80
      endif
      if (iVerbose > 0) print *,'rescale', rescale
      !
      ! Get the index of the transform
      if (ifXform .ne. 0) then
        lnu = lineUse(isec) + 1
        call xfcopy(f(1, 1, lnu), fprod)
      endif
      !
      ! if doing distortions or warping, get the grid
      !
      hasWarp = .false.
      dx = 0.
      dy = 0.
      if (ifDistort > 0) then
        iy = idfUse(isec) + 1
        hasWarp = .true.
        xnBig = nx / warpScale
        ynbig = ny / warpScale
      else if (ifWarping .ne. 0) then
        iy = lnu
        hasWarp = nControl(iy) > 2
        xnBig = readReduction * nxOut / warpScale
        ynbig = readReduction * nyOut / warpScale
        !
        ! for warping with center offset applied after, it will subtract the
        ! offset from the grid start and add it to the grid displacements
        if (applyFirst == 0) then
          dx = readReduction * xcen(isec) / warpScale
          dy = readReduction * ycen(isec) / warpScale
        endif
      endif
      if (hasWarp) then
        if (getSizeAdjustedGrid(iy, xnBig, ynbig, dx, dy, 1, warpScale,  &
            nint(readReduction), nxGrid, nyGrid, xGridStart, yGridStart, xGridIntrv, &
            yGridIntrv, fieldDx, fieldDy, lmGrid, lmGrid, listString) .ne. 0) &
            call exitError(listString)
        !
        !print *,nxGrid, nyGrid, xGridIntrv, yGridIntrv, xGridStart, xGridStart +(nxGrid &
        ! - 1) * xGridIntrv, yGridStart, yGridStart + (nyGrid - 1)*yGridIntrv
        ! do ierr = 1, nyGrid
        ! write(*,'(f7.2,9f8.2)') (fieldDx(i, ierr), fieldDy(i, ierr), i=1, nxGrid)
        ! enddo
        !
        ! copy field to tmpDx, y in case there are mag grads
        tmpDx(1:nxGrid, 1:nyGrid) = fieldDx(1:nxGrid, 1:nyGrid)
        tmpDy(1:nxGrid, 1:nyGrid) = fieldDy(1:nxGrid, 1:nyGrid)
      endif
      !
      ! if doing mag gradients, set up or add to distortion field
      !
      if (ifMagGrad .ne. 0) then
        magUse = min(iSecRead + 1, numMagGrad)
        if (ifDistort .ne. 0) then
          call addMagGradField(tmpDx, tmpDy, fieldDx, fieldDy, lmGrid, nxBin, &
              nyBin, nxGrid, nyGrid, xGridStart, yGridStart, xGridIntrv, &
              yGridIntrv, nxBin / 2., nyBin / 2., pixelMagGrad, axisRot, &
              tiltAngles(magUse), dmagPerMicron(magUse), rotPerMicron(magUse))
        else
          call makeMagGradField(tmpDx, tmpDy, fieldDx, fieldDy, lmGrid, &
              nxBin, nyBin, nxGrid, nyGrid, xGridStart, yGridStart, xGridIntrv, &
              yGridIntrv, nxBin / 2., nyBin / 2., &
              pixelMagGrad, axisRot, tiltAngles(magUse), &
              dmagPerMicron(magUse), rotPerMicron(magUse))
        endif
      endif
      !
      ! if transforming, and apply first is selected, get the shifts by
      ! applying the offset first then multiplying that by the transform
      ! Otherwise do it the old way, subtract offset from final xform, but only
      ! if not warping (now that warping is known for this section)
      if (ifXform .ne. 0) then
        if (applyFirst .ne. 0) then
          call xfunit(frot, 1.)
          frot(1, 3) = -xcen(isec)
          frot(2, 3) = -ycen(isec)
          call xfmult(frot, f(1, 1, lnu), fprod)
        elseif (.not. (ifWarping .ne. 0 .and. hasWarp)) then
          fprod(1, 3) = fprod(1, 3) - xcen(isec)
          fprod(2, 3) = fprod(2, 3) - ycen(isec)
        endif
      endif
      !
      ! get maximum Y deviation with current field to adjust chunk
      ! limits with
      !
      if (ifMagGrad .ne. 0 .or. hasWarp) then
        fieldMaxX = 0.
        fieldMaxY = 0.
        do iy = 1, nyGrid
          do i = 1, nxGrid
            fieldMaxX = max(fieldMaxX, abs(fieldDx(i, iy)))
            fieldMaxY = max(fieldMaxY, abs(fieldDy(i, iy)))
          enddo
        enddo
        maxFieldX = int(fieldMaxX + 1.5)
        maxFieldY = int(fieldMaxY + 1.5)
      endif
      !
      ! Determine starting and ending lines needed from the input, and whether any
      ! fill is needed
      call linesNeededForOutput(0, nyOut - 1, needYfirst, needYlast, fillNeeded)
      nyNeeded = needYlast + 1 - needYfirst
      !
      ! Now that needed input and output size is finally known, make sure memory is
      ! enough
      call reallocateIfNeeded()
      !
      ! figure out how the data will be loaded and saved; first see if
      ! both input and output images will fit in one chunk, or if
      ! entire input image will fit
      !
      numChunks = 0
      if (idimInOut / nxBin > nyNeeded) then
        linesLeft = (idimInOut - nxBin * nyNeeded) / nxOut
        numChunks = (nyOut + linesLeft - 1) / linesLeft
        if (iVerbose > 0) print *,'linesleft', linesLeft, '  nchunk', numChunks
      endif
      if (numChunks == 1 .or. (numChunks > 0 .and. numChunks <= maxChunks .and. &
          .not.rescale)) then
        !
        ! Use entire input and multi-chunk output only if not rescaling
        ! set up chunks for output, and set up that
        ! whole image is needed for every output chunk.
        ! Note that lines are numbered from 0 here
        !
        lineOutSt(1) = 0
        do iChunk = 1, numChunks
          nextLine = (nyOut / numChunks) * iChunk + min(iChunk, mod(nyOut, numChunks))
          numLinesOut(iChunk) = nextLine - lineOutSt(iChunk)
          lineOutSt(iChunk + 1) = nextLine
          lineInSt(iChunk) = needYfirst
          numLinesIn(iChunk) = nyNeeded
        enddo
        maxin = nyNeeded
        ifOutChunk = 1
      else
        !
        ! otherwise, break output into successively more chunks, and
        ! find out how many lines are needed of input for each chunk,
        ! Scan twice: first trying to fit all the output to avoid a
        ! read-write to temp file; then breaking equally
        !
        ifOutChunk = -1
        iscan = 1
        iyTest = nyOut
        do while(iscan <= 2 .and. ifOutChunk < 0)
          numChunks = 1
          do while(numChunks <= maxChunks .and. ifOutChunk < 0)
            lineOutSt(1) = 0
            maxin = 0
            do iChunk = 1, numChunks
              nextLine = (nyOut / numChunks) * iChunk + min(iChunk, mod(nyOut, numChunks))
              numLinesOut(iChunk) = nextLine - lineOutSt(iChunk)
              lineOutSt(iChunk + 1) = nextLine
              !
              call linesNeededForOutput(lineOutSt(iChunk), nextLine - 1, iy1, iy2, &
                  fillTmp)
              lineInSt(iChunk) = iy1
              numLinesIn(iChunk) = iy2 + 1 - iy1
              maxin = max(maxin, numLinesIn(iChunk))
            enddo
            !
            ! Will the input and output data now fit?  then terminate.
            !
            if (iscan == 2) iyTest = numLinesOut(1)
            if (idimInOut / maxin > nxBin .and. idimInOut / iyTest > nxOut .and. &
                 maxin * int(nxBin, kind = 8) + iyTest * int(nxOut, kind = 8) <= &
                 idimInOut) then
              ifOutChunk = iscan - 1
            else
              numChunks = numChunks + 1
            endif
          enddo
          iscan = iscan + 1
        enddo
        if (ifOutChunk < 0) call exitError( &
            ' INPUT IMAGE TOO LARGE FOR ARRAY.')
      endif
      if (iVerbose > 0) then
        print *,'number of chunks:', numChunks
        do i = 1, numChunks
          print *,i, lineInSt(i), numLinesIn(i), lineOutSt(i), numLinesOut(i)
        enddo
      endif
      if (numChunks > 1 .and. numTaper > 0) &
          call exitError('CANNOT TAPER OUTPUT IMAGE - IT DOES NOT '// &
          'FIT COMPLETELY IN MEMORY')
      !
      ! open temp file if one is needed
      !
      if (rescale .and. ifOutChunk > 0 .and. numChunks > 1 .and. &
          ifTempOpen == 0) then
        tempExt = 'nws      '
        tempExt(4:5) = timeStr(1:2)
        tempExt(6:7) = timeStr(4:5)
        tempExt(8:9) = timeStr(7:8)
        tempName = temp_filename(outFile(iOutFile), ' ', tempExt)
        !
        call imopen(3, tempName, 'scratch')
        call icrhdr(3, nxyz2, nxyz2, 2, title, 0)
        ifTempOpen = 1
      endif
      !
      iBufOutBase = int(maxin, kind = 8) * nxBin + 1
      !
      ! get the mean of section from previous scan, or a new scan
      !
      loadYstart = -1
      loadYend = -1
      if (ifUseFill .ne. 0) then
        dmeanSec = fillVal
      elseif (ifMean .ne. 0 .and. ifMeanSdEntered == 0) then
        dmeanSec = secMean(ilist + listInd(iFile) - 1)
      elseif (.not. fillNeeded) then
        dmeanSec = dmeanIn
      else
        if (iVerbose > 0) print *,'scanning for mean for fill', iSecRead, nyNeeded, &
            needYfirst
        wallStart = wallTime()
        call scanSection(array, idimInOut, nxBin, nyNeeded, needYfirst, readReduction, &
            rxOffset, ryOffset, iSecRead, 0, dmin2, dmax2, dmeanSec, sdSec, loadYstart, &
            loadYend, indFilter, readShrunk, array(idimInOut + 1), lenTemp)
        loadYend = min(loadYend, loadYstart + maxin - 1)
        loadTime = loadTime + wallTime() - wallStart
      endif
      !
      ! loop on chunks
      !
      dsum = 0.
      dsumSq = 0.
      tmpMin = 1.e30
      tmpMax = -1.e30
      do iChunk = 1, numChunks
        needYstart = lineInSt(iChunk)
        needYend = needYstart + numLinesIn(iChunk) - 1
        !
        ! first load data that is needed if not already loaded
        !
        wallStart = wallTime()
        if (needYstart < loadYstart .or. needYend > loadYend) then
          loadYoffset = needYstart
          loadBaseInd = 0
          if (loadYstart <= needYstart .and. loadYend >= needYstart) then
            !
            ! move data down if it will fill a bottom region
            !
            numMove = (loadYend + 1 - needYstart) * int(nxBin, kind = 8)
            moveOffset = (needYstart - loadYstart) * int(nxBin, kind = 8)
            if (iVerbose > 0) print *,'moving data down', numMove, moveOffset
            do i8 = 1, numMove
              array(i8) = array(i8 + moveOffset)
            enddo
            numLinesLoad = needYend - loadYend
            loadYoffset = loadYend + 1
            loadBaseInd = numMove
          elseif (needYstart <= loadYstart .and. needYend >= loadYstart) then
            !
            ! move data up if it will fill top
            !
            numMove = (needYend + 1 - loadYstart) * int(nxBin, kind = 8)
            moveOffset = (loadYstart - needYstart) * int(nxBin, kind = 8)
            if (iVerbose > 0) print *,'moving data up', numMove, moveOffset
            do i8 = numMove, 1, -1
              array(i8 + moveOffset) = array(i8)
            enddo
            numLinesLoad = loadYstart - needYstart
          else
            !
            ! otherwise just get whole needed region
            !
            numLinesLoad = needYend + 1 - needYstart
            if (iVerbose > 0) print *,'loading whole region', needYstart, needYend, &
                numLinesLoad
          endif
          call readBinnedOrReduced(1, iSecRead, array(1 + loadBaseInd), nxBin, &
              numLinesLoad, rxOffset, ryOffset + readReduction * loadYoffset, &
              readReduction, nxBin, numLinesLoad, indFilter, readShrunk, &
              array(idimInOut + 1), lenTemp)
          loadYstart = needYstart
          loadYend = needYend
        endif
        numYload = loadYend + 1 - loadYstart
        numYchunk = numLinesOut(iChunk)
        numPix = int(nxOut, kind = 8) * numYchunk
        iChunkBase = iBufOutBase
        if (ifOutChunk == 0)  &
            iChunkBase = iBufOutBase + lineOutSt(iChunk) * int(nxOut, kind = 8)
        loadTime = loadTime + wallTime() - wallStart

        if (ifXform .ne. 0) then
          !
          ! do transform if called for
          !
          wallStart = wallTime()
          xcenIn = nxBin / 2.
          ycenIn = nyBin / 2. -loadYstart
          dx = fprod(1, 3)
          dy = (nyOut - numYchunk) / 2. +fprod(2, 3) - lineOutSt(iChunk)
          ! dx=f(1, 3, lnu) -xcen(isec)
          ! dy=(nyOut-numYchunk) /2.+f(2, 3, lnu) - ycen(isec) - lineOutSt(iChunk)
          if (linesShrink > 0) then
            ierr = zoomFiltInterp(array, array(iChunkBase), nxBin, numYload, nxOut, &
                numYchunk, xcenIn , ycenIn, dx, dy, dmeanSec)
            if (ierr .ne. 0) then
              write(listString, '(a,i3)') &
                  'CALLING zoomFiltInterp FOR IMAGE REDUCTION, ERROR', ierr
              call exitError(listString)
            endif
          elseif (.not. hasWarp .and. ifMagGrad == 0) then
            call cubInterp(array, array(iChunkBase), nxBin, numYload, nxOut, numYchunk, &
                fprod, xcenIn , ycenIn, dx, dy, 1., dmeanSec, ifLinear)
          else
            !
            ! if undistorting, adjust the grid start down by first loaded input line
            ! if warping, adjust it down by first output line
            ystart = yGridStart - loadYstart
            if (ifWarping .ne. 0) ystart = yGridStart - lineOutSt(iChunk)
            call warpInterp(array, array(iChunkBase), nxBin, numYload, nxOut, numYchunk, &
                fprod, xcenIn , ycenIn, dx, dy, 1., dmeanSec, ifLinear, ifWarping, &
                fieldDx, fieldDy, lmGrid, nxGrid, nyGrid, xGridStart, ystart, &
                xGridIntrv, yGridIntrv)
          endif
          rotTime = rotTime + wallTime() - wallStart
        else
          !
          ! otherwise repack array into output space nxOut by nyOut, with
          ! offset as specified, using the special repack routine
          !
          ix1 = nxBin / 2 - nxOut / 2 + nint(xcen(isec))
          ix2 = ix1 + nxOut - 1
          !
          iyBase = nyBin / 2 - nyOut / 2 + nint(ycen(isec))
          iy1 = iyBase + lineOutSt(iChunk) - loadYstart
          iy2 = iy1 + numYchunk - 1
          !
          call irepak2(array(iChunkBase), array, nxBin, numYload, ix1, ix2, iy1, &
              iy2, dmeanSec)
          if (iVerbose > 0) print *,'did repack'
        endif
        if (numTaper > 0) then
          if (taperAtFill(array(iChunkBase), nxOut, nyOut, numTaper, insideTaper) .ne. &
              0) call exitError('MEMORY ALLOCATION ERROR TAPERING IMAGE')
        endif
        !
        ! if no rescaling, or if mean is needed now, accumulate sums for
        ! mean
        !
        if (.not.rescale .or. ifMean .ne. 0) then
          if (ifFloat == 2) then
            call iclAvgSd(array(iChunkBase), nxOut, numYchunk, 1, nxOut, 1, numYchunk, &
                tmin2, tmax2, tsum, tsumSq, avgSec, sdSec)
            if (iVerbose > 0) print *,'chunk mean&sd', iChunk, avgSec, sdSec
            dsumSq = dsumSq + tsumSq
          else
            call iclden(array(iChunkBase), nxOut, numYchunk, 1, nxOut, 1, numYchunk, &
                tmin2, tmax2, tmean2)
            tsum = tmean2 * numPix
          endif
          tmpMin = min(tmpMin, tmin2)
          tmpMax = max(tmpMax, tmax2)
          dsum = dsum + tsum
          if (iVerbose > 0) print *,'did iclden ', tmin2, tmax2, tmpMin, tmpMax
        else
          !
          ! otherwise get new min and max quickly
          !
          do i8 = 1, numPix
            val = array(i8 + iChunkBase - 1)
            if (val < tmpMin) tmpMin = val
            if (val > tmpMax) tmpMax = val
          enddo
        endif
        if (ifFloat == 0 .and. newMode .ne. 2 .and. mode .ne. 2) then
          !
          ! 6/27/01: really want to truncate rather than rescale; so if
          ! the min or max is now out of range for the input mode,
          ! truncate the data and adjust the min and max
          !
          if (tmpMin < bottomIn .or. tmpMax > optimalIn) then
            tsum2 = 0.
            do i8 = 1, numPix
              val = max(bottomIn, min(optimalIn, array(i8 + iChunkBase - 1)))
              tsum2 = tsum2 + val
              array(i8 + iChunkBase - 1) = val
            enddo
            tmpMin = max(tmpMin, bottomIn)
            tmpMax = min(tmpMax, optimalIn)
            dsum = dsum + tsum2 - tsum
          endif
        endif
        !
        ! write all but last chunk
        !
        wallStart = wallTime()
        if (.not.rescale) then
          if (iVerbose > 0) print *,'writing to real file', iChunk
          call imposn(2, isecOut - 1, lineOutSt(iChunk))
          call iwrsecl(2, array(iChunkBase), numLinesOut(iChunk))
        elseif (iChunk .ne. numChunks .and. ifOutChunk > 0) then
          if (iVerbose > 0) print *,'writing to temp file', iChunk
          call imposn(3, 0, lineOutSt(iChunk))
          call iwrsecl(3, array(iBufOutBase), numLinesOut(iChunk))
        endif
        saveTime = saveTime + wallTime() - wallStart
      enddo

      call findScaleFactors(tmpMin, tmpMax)

      if (rescale) then
        dmin2 = 1.e20
        dmax2 = -1.e20
        dmean2 = 0.
        ! set up minimum value to output based on mode
        if (newMode == 1) then
          densOutMin = -32767
        elseif (newMode == 2) then
          densOutMin = -1.e30
          optimalOut = 1.e30
        else
          densOutMin = 0.
        endif
        !
        ! loop backwards on chunks, reloading all but last, scaling
        ! and rewriting
        !
        do iChunk = numChunks, 1, -1
          iChunkBase = iBufOutBase
          if (ifOutChunk == 0) iChunkBase = iBufOutBase + lineOutSt(iChunk) * nxOut
          if (iChunk .ne. numChunks .and. ifOutChunk > 0) then
            if (iVerbose > 0) print *,'reading', iChunk
            call imposn(3, 0, lineOutSt(iChunk))
            call irdsecl(3, array(iBufOutBase), numLinesOut(iChunk),*99)
          endif
          do iy = 1, numLinesOut(iChunk)
            istart = iChunkBase + (iy - 1) * int(nxOut, kind = 8)
            tsum = 0.
            do i8 = istart, istart + nxOut - 1
              dens = scaleFactor * array(i8) + constAdd
              if (dens < densOutMin) then
                numTruncLow = numTruncLow + 1
                dens = densOutMin
              elseif (dens > optimalOut) then
                numTruncHigh = numTruncHigh + 1
                dens = optimalOut
              endif
              array(i8) = dens
              tsum = tsum + dens
              dmin2 = min(dmin2, dens)
              dmax2 = max(dmax2, dens)
            enddo
            dmean2 = dmean2 + tsum
          enddo
          wallStart = wallTime()
          if (iVerbose > 0) print *,'writing', iChunk
          call imposn(2, isecOut - 1, lineOutSt(iChunk))
          call iwrsecl(2, array(iChunkBase), numLinesOut(iChunk))
          saveTime = saveTime + wallTime() - wallStart
        enddo
      else
        !
        ! if not scaling
        !
        dmin2 = tmpMin
        dmax2 = tmpMax
        dmean2 = dsum
      endif
      !
      dmean2 = dmean2 / (float(nxOut) * nyOut)
      if (ifHeaderOut == 0) print *, &
          'section   input min&max       output min&max  &  mean'
      ifHeaderOut = 1
      write(*,'(i6,5f10.2)') isec - 1, tmpMin, tmpMax, dmin2, dmax2, &
          dmean2

      !
80    isecOut = isecOut + 1
      dmin = min(dmin, dmin2)
      dmax = MAX(dmax, dmax2)
      if (numReplace == 0) then
        dmean = dmean + dmean2
        !
        ! transfer extra header bytes if present
        !
        if (nByteSymOut .ne. 0 .and. indExtraOut < nByteSymOut) then
          nByteCopy = min(nByteExtraOut, nByteExtraIn, nByteSymIn)
          nByteClear = nByteExtraOut - nByteCopy
          do i = 1, nByteCopy
            indExtraOut = indExtraOut + 1
            extraOut(indExtraOut) = extraIn(iSecRead * nByteExtraIn + i)
          enddo
          do i = 1, nByteClear
            indExtraOut = indExtraOut + 1
            extraOut(indExtraOut) = 0
          enddo
        endif
      else if (isecReplace < listTotal) then
        isecReplace = isecReplace + 1
        isecOut = listReplace(isecReplace) + 1
      endif
      !
      ! see if need to close stack file
      !
      if (numReplace == 0 .and. isecOut > numSecOut(iOutFile)) then
        if (nByteSymOut > 0) call ialsym(2, nByteSymOut, extraOut)
        dmean = dmean / numSecOut(iOutFile)
        call iwrhdr(2, title, 1, dmin, dmax, dmean)
        call imclose(2)
        isecOut = 1
        iOutFile = iOutFile + 1
      endif
      isec = isec + 1
    enddo
    call imclose(1)
  enddo
  if (numReplace > 0) then
    call iwrhdr(2, title, -1, dmin, dmax, dmean)
    call imclose(2)
  endif
  !
  if (ifTempOpen .ne. 0) call imclose(3)
  if (numTruncLow + numTruncHigh > 0) write(*,103) numTruncLow, numTruncHigh
103 format(' TRUNCATIONS OCCURRED:',i11,' at low end,',i11, &
      ' at high end of range')
  if (iVerbose > 0) write(*,'(a,f8.4,a,f8.4,a,f8.4,a,f8.4)') 'loadtime', &
      loadTime, '  savetime', saveTime, '  sum', loadTime + saveTime, &
      '  rottime', rotTime
  call exit(0)
99 call exitError(' END OF IMAGE WHILE READING')

CONTAINS

  subroutine reallocateIfNeeded()
    integer(kind = 8) needDim
    integer*4 needTemp
    real*4 defLimit/3.75e9/
    if (limEntered == 0) then
      needTemp = 1
      if (readShrunk) needTemp = max(nx * (nint(readReduction) + 20),  &
          int(min(int(MAXTEMP, kind=8), int(nx, kind=8) * ny)))
      if (iBinning > 1) needTemp = nx * iBinning
      needDim = int(nxBin, kind = 8) * nyNeeded
      if (nxOut > 0 .and. nyOut > 0) &
          needDim = needDim + int(nxOut, kind = 8) * nyOut
      if (needDim > defLimit) needDim = defLimit
      if (iVerbose > 0) print *,'reallocate sizes:', nxOut, nyOut, nxBin, nyBin, needDim
      if (needDim + needTemp > limToAlloc) then
        limToAlloc = needDim + needTemp
        call reallocateArray()
      endif
      idimInOut = needDim
      lenTemp = needTemp
    endif
  end subroutine reallocateIfNeeded

  subroutine reallocateArray()
    if (iVerbose > 0) print *, 'reallocating array to', limToAlloc / (1024 * 256.), &
        ' MB'
    deallocate(array)
    allocate(array(limToAlloc), stat = ierr)
    if (ierr .ne. 0 .and. limToAlloc > limIfFail) then
      limToAlloc = limIfFail
      idimInOut = limToAlloc - lenTemp
      if (iVerbose > 0) print *, 'failed, dropping reallocation to', &
          limToAlloc / (1024 * 256), ' MB'
      allocate(array(limToAlloc), stat = ierr)
    endif
    if (ierr .ne. 0) call exitError('REALLOCATING MEMORY FOR MAIN ARRAY')
    return
  end subroutine reallocateArray


  ! Finds what lines of input are needed to produce lines from LINEOUTFIRST through
  ! LINEOUTLAST of output (numbered from 0), and also determines if x or Y input goes
  ! out of range so that fill is needed
  !
  subroutine linesNeededForOutput(lineOutFirst, lineOutLast, iyIn1, iyIn2, needFill)
    integer*4 lineOutFirst, lineOutLast, iyIn1, iyIn2, ixIn1, ixIn2, ixBase
    logical needFill
    real*4 xp1, yp1, xp2, yp2, xp3, yp3, xp4, yp4
    if (ifXform == 0) then
      !
      ! simple case of no transform
      !
      iyBase = nyBin / 2 + ycen(isec) - (nyOut / 2)
      ixBase = nxBin / 2 + xcen(isec) - (nxOut / 2)
      iyIn1 = max(0, iyBase + lineOutFirst)
      iyIn2 = min(nyBin - 1, iyBase + lineOutLast)
      needFill = iyBase + lineOutFirst < 0 .or. iyBase + lineOutLast >= nyBin .or. &
          ixBase < 0 .or. ixBase + nxOut > nxBin
    else
      !
      ! transform: get input needs of 4 corners
      ! pass and get back coordinates numbered from 1, subtract
      ! an extra 1 to get to lines numbered from 0
      ! Allow extra for distortion field Y component
      !
      xcenIn = nxBin / 2.
      ycenIn = nyBin / 2.
      dx = fprod(1, 3)
      dy = fprod(2, 3)
      ! dx=f(1, 3, lnu) -xcen(isec)
      ! dy=f(2, 3, lnu) -ycen(isec)
      call backXform(nxOut, nyOut, fprod, xcenIn , ycenIn, dx, dy, 1, lineOutFirst + 1, &
          xp1, yp1)
      call backXform(nxOut, nyOut, fprod, xcenIn , ycenIn, dx, dy, nxOut,  &
          lineOutFirst + 1, xp2, yp2)
      call backXform(nxOut, nyOut, fprod, xcenIn , ycenIn, dx, dy, 1, lineOutLast + 1, &
          xp3, yp3)
      call backXform(nxOut, nyOut, fprod, xcenIn , ycenIn, dx, dy, nxOut,  &
          lineOutLast + 1, xp4, yp4)

      iyIn1 = int(min(yp1, yp2, yp3, yp4)) - 2 - maxFieldY - linesShrink
      iyIn2 = int(max(yp1, yp2, yp3, yp4)) + 1 + maxFieldY + linesShrink
      ixIn1 = int(min(xp1, xp2, xp3, xp4)) - 2 - maxFieldX - linesShrink
      ixIn2 = int(max(xp1, xp2, xp3, xp4)) + 1 + maxFieldX + linesShrink
      needFill = ixIn1 < 0 .or. ixIn1 >= nxBin .or. ixIn2 < 0 .or. ixIn2 >= nxBin .or. &
          iyIn1 < 0 .or. iyIn1 >= nyBin .or. iyIn2 < 0 .or. iyIn2 >= nyBin
      iyIn1 = min(nyBin - 1, max(0, iyIn1))
      iyIn2 = min(nyBin - 1, max(0, iyIn2))
    endif

  end subroutine linesNeededForOutput


  ! Determine the scale factors scaleFactor and constAdd from a host of option
  ! settings, controlling values, and values determined for the particular
  ! section.  This code is really dreadful since first it calculates new
  ! min and max and uses that to determine scaling.
  !
  subroutine findScaleFactors(tmpMinIn, tmpMaxIn)
    !
    real*4 avgSec, sdSec, dminNew, dmaxNew, dmin2, dmax2, zminSec, zmaxSec
    real*4 tmpMean, tmpMinShift, tmpMaxShift, tmpMinIn, tmpMaxIn
    !
    ! calculate new min and max after rescaling under various possibilities
    !
    scaleFactor = 1.
    constAdd = 0.
    tmpMin = tmpMinIn
    tmpMax = tmpMaxIn
    dmin2 = tmpMin
    dmax2 = tmpMax
    !
    if (ifFloat == 0 .and. rescale) then
      !
      ! no float but mode change (not to mode 2) :
      ! rescale from input range to output range
      !
      dmin2 = (tmpMin - bottomIn) * (optimalOut - bottomOut) / &
          (optimalIn - bottomIn) + bottomOut
      dmax2 = (tmpMax - bottomIn) * (optimalOut - bottomOut) / &
          (optimalIn - bottomIn) + bottomOut
    elseif (ifFloat < 0 .and. numScaleFacs == 0) then
      !
      ! if specified global rescale, set values that dminIn and dmaxIn
      ! map to, either the maximum range or the values specified
      !
      if (dminSpecified == 0 .and. dmaxSpecified == 0) then
        dminNew = 0.
        dmaxNew = optimalOut
      else if (dminSpecified == dmaxSpecified) then
        dminNew = dminIn
        dmaxNew = dmaxIn
      else
        dminNew = dminSpecified
        dmaxNew = dmaxSpecified
      endif
      !
      ! then compute what this section's tmpMin and tmpMax map to
      !
      dmin2 = (tmpMin - dminIn) * (dmaxNew - dminNew) / (dmaxIn - dminIn) + dminNew
      dmax2 = (tmpMax - dminIn) * (dmaxNew - dminNew) / (dmaxIn - dminIn) + dminNew
    elseif (ifFloat > 0 .and. ifMeanSdEntered == 0) then
      !
      ! if floating: scale to a dmin2 that will knock out fracZero of
      ! the range after truncation to zero
      !
      dmin2 = -optimalOut * fracZero / (1. -fracZero)
      if (ifMean == 0) then
        !
        ! float to range, new dmax2 is the max of the range
        !
        dmax2 = optimalOut
      elseif (ifFloat == 2) then
        ! :float to mean, it's very hairy
        call sums_to_avgsd8(dsum, dsumSq, nxOut, nyOut, avgSec, sdSec)
        ! print *,'overall mean & sd', avgSec, sdSec
        if (tmpMin == tmpMax .or. sdSec == 0.) sdSec = 1.
        
        ! Truncate the min and max to what will fit in the common range determined after
        ! outlier elimination, then compute the min and max that those map to.
        tmpMin = max(tmpMin, zmin * sdsec + avgSec)
        tmpMax = min(tmpMax, zmax * sdsec + avgSec)
        zminSec = (tmpMin - avgSec) / sdSec
        zmaxSec = (tmpMax - avgSec) / sdSec
        dmin2 = (zminSec - zmin) * optimalOut / (zmax - zmin)
        dmax2 = (zmaxSec - zmin) * optimalOut / (zmax - zmin)
        dmin2 = max(0., dmin2)
        dmax2 = min(dmax2, optimalOut)
      else
        !
        ! shift to mean
        !
        tmpMean = dsum / (float(nxOut) * nyOut)
        !
        ! values that min and max shift to
        !
        tmpMinShift = tmpMin + shiftMean - tmpMean
        tmpMaxShift = tmpMax + shiftMean - tmpMean
        !
        if (ifFloat == 3) then
          !
          ! for no specified scaling, set new min and max to
          ! shifted values
          !
          dmin2 = tmpMinShift
          dmax2 = tmpMaxShift
          if (newMode .ne. 2) then
            !
            ! then, if mode is not 2, set up for scaling if range is
            ! too large and/or if there is a modal shift
            !
            optimalIn = max(optimalIn, shiftMax)
            dmin2 = tmpMinShift * optimalOut / optimalIn
            dmax2 = tmpMaxShift * optimalOut / optimalIn
          endif
        else
          !
          ! for specified scaling of shifted means
          !
          if (dminSpecified == dmaxSpecified) then
            dminNew = 0.5
            dmaxNew = optimalOut - 0.5
          else
            dminNew = dminSpecified
            dmaxNew = dmaxSpecified
          endif
          !
          ! for specified scaling, compute what this section's tmpMin
          ! and tmpMax map to
          !
          dmin2 = (tmpMinShift - shiftMin) * (dmaxNew - dminNew) / &
              (shiftMax - shiftMin) + dminNew
          dmax2 = (tmpMaxShift - shiftMin) * (dmaxNew - dminNew) / &
              (shiftMax - shiftMin) + dminNew
        endif
      endif
    endif
    !
    if (rescale) then
      !
      ! if scaling, set up equation, scale and compute new mean
      ! or use scaling factors directly
      !
      if (numScaleFacs > 0) then
        scaleFactor = scaleFacs(min(numScaleFacs, iFile))
        constAdd = scaleConsts(min(numScaleFacs, iFile))
      else if (ifMeanSdEntered .ne. 0) then
        call sums_to_avgsd8(dsum, dsumSq, nxOut, nyOut, avgSec, sdSec)
        if (sdSec == 0) sdSec = 1.
        scaleFactor = enteredSD / sdSec
        constAdd = enteredMean - scaleFactor * avgSec
      else
        !
        ! 2/9/05: keep scale factor 1 if image has no range
        !
        scaleFactor = 1.
        if (dmax2 .ne. dmin2 .and. tmpMax .ne. tmpMin) &
            scaleFactor = (dmax2 - dmin2) / (tmpMax - tmpMin)
        constAdd = dmin2 - scaleFactor * tmpMin
      endif
    endif
    return
  end subroutine findScaleFactors


end program newstack


! getItemsToUse gets the list of transform line numbers or distortion fields to apply,
! given the number available in NXFORMS, the list of LISTTOTAL section numbers in
! INLIST, the PIP option in OPTION, the PIPINPUT flag if doing pip input, and a scratch
! string in LISTSTRING. ERROR should have a base error string, IFONEPERFILE > 0 to do
! one transform per file, and NUMINFILES should have number of files. NUMBEROFFSET
! should be 0 or 1 depending on whether items are nunbered from 1.  The list
! of items is returned in LINEUSE, and the number in the list in NLINEUSE
!
subroutine getItemsToUse(nXforms, listTotal, inList, option, listString, pipinput, &
    error, ifOnePerFile, numInFiles, lineUse, nLineUse, numberOffset, LIMSEC)
  implicit none
  integer*4 nXforms, listTotal, inList(*), numXfLines, lineUse(*), nLineUse, numberOffset
  integer*4 ifOnePerFile, numInFiles
  integer*4 LIMSEC, numLinesTemp, ierr, i, PipGetString
  character*(*) error, option, listString
  character*80 errString
  logical*4 pipinput
  !
  ! Set up default list, add one back if numbered from one
  write(errString, '(a,a,a)') 'TOO MANY ', error, ' NUMBERS FOR ARRAYS'
  if (nXforms == 1) then
    !
    ! for one transform, set up single line for now
    !
    nLineUse = 1
    lineUse(1) = numberOffset
  elseif (ifOnePerFile > 0) then
    !
    ! for one transform per file, default is 0 to nfile - 1
    !
    nLineUse = numInFiles
    do i = 1, numInFiles
      lineUse(i) = i + numberOffset - 1
    enddo
  else
    !
    ! Otherwise default comes from section list
    !
    nLineUse = listTotal
    do i = 1, listTotal
      lineUse(i) = inList(i) + numberOffset
    enddo
  endif
  !
  numXfLines = 0
  if (pipinput) then
    call PipNumberOfEntries(option, numXfLines)
    if (numXfLines > 0) then
      numLinesTemp = nLineUse
      nLineUse = 0
      do i = 1, numXfLines
        ierr = PipGetString(option, listString)
        call parseList2(listString, lineUse(nLineUse + 1), numLinesTemp, &
            LIMSEC - nLineUse)
        nLineUse = nLineUse + numLinesTemp
      enddo
    endif
  else
    !
    print *,'Enter list of lines to use in file, or a single line number to apply that'
    print *,' transform to all sections (1st line is 0; ranges OK; / for section list)'
    call rdlist2(5, lineUse, nLineUse, LIMSEC)
  endif

  do i = 1, nLineUse
    lineUse(i) = lineUse(i) - numberOffset
    if (lineUse(i) < 0 .or. lineUse(i) >= nXforms) then
      write(errString, '(a, a,i5)') error, ' NUMBER OUT OF BOUNDS:', &
          lineUse(i) + numberOffset
      call exitError(trim(errString))
    endif
  enddo
  return
end subroutine getItemsToUse


! irepak2 repacks an image from a portion of a 2-d array sequentially
! into a 1-d array (which should not be the same array) .  Pixels
! outside the range of the original array will be filled with the
! supplied value of dmean.  brray is the repacked array,
! everything else follows definition of iwrpas; i.e. array is
! dimensioned mx by my, and the starting and ending index coordinates
! (numbered from 0) are given by nx1, nx2, ny1, ny2
!
subroutine irepak2(brray, array, mx, my, nx1, nx2, ny1, ny2, dmean)
  implicit none
  integer*4 mx, my, nx1, nx2, ny1, ny2
  real*4 brray(*), array(mx,my), dmean
  integer*4 ind, iy, ix
  ind = 1
  do iy = ny1 + 1, ny2 + 1
    if (iy >= 1 .and. iy <= my) then
      do ix = nx1 + 1, nx2 + 1
        if (ix >= 1 .and. ix <= mx) then
          brray(ind) = array(ix, iy)
        else
          brray(ind) = dmean
        endif
        ind = ind + 1
      enddo
    else
      do ix = nx1 + 1, nx2 + 1
        brray(ind) = dmean
        ind = ind + 1
      enddo
    endif
  enddo
  return
end subroutine irepak2


! scanSection will determine the min DMIN2, max DMAX2, and mean DMEAN2 of section
! ISECREAD.  It will also determine the standard deviation SDSEC if IFFLOAT = 2.
! It uses ARRAY for storage.  IDIMINOUT specifies the size of ARRAY, while NX is the
! binned image size in x, NYNEEDED is the number of lines to scan, and NEEDYFIRST
! is the first line.  The image will be loaded in chunks if necessary.  LOADYSTART
! and LOADYEND are the starting and ending lines (numbered from 0) that are left
! in ARRAY.
!
subroutine scanSection(array, idimInOut, nx, nyNeeded, needYfirst, reduction, rxOffset, &
    ryOffset, iSecRead, ifFloat, dmin2, dmax2, dmean2, sdSec, loadYstart, loadYend, &
    indFilter, readShrunk, temp, lenTemp)
  implicit none
  integer(kind = 8) idimInOut
  integer*4 nx, iSecRead, ifFloat, loadYstart, loadYend, lenTemp, needYfirst
  integer*4 indFilter
  real*4 array(idimInOut), temp(lenTemp), dmin2, dmax2, dmean2, sdSec
  real*4 reduction, rxOffset, ryOffset
  logical readShrunk
  integer*4 maxLines, numLoads, iline, iload, numLines, ierr, nyNeeded
  real*4 tmin2, tmax2, tmean2, avgSec
  real*8 dsum, dsumSq, tsum, tsumSq
  !
  ! load in chunks if necessary, based on the maximum number
  ! of lines that will fit in the array
  !
  maxLines = idimInOut / nx
  numLoads = (nyNeeded + maxLines - 1) / maxLines
  iline = needYfirst
  dmin2 = 1.e30
  dmax2 = -dmin2
  dsum = 0.
  dsumSq = 0.
  do iload = 1, numLoads
    numLines = nyNeeded / numLoads
    if (iload <= mod(nyNeeded, numLoads)) numLines = numLines + 1
    call readBinnedOrReduced(1, iSecRead, array, nx, numLines, rxOffset, ryOffset + &
        reduction * iline, reduction, nx, numLines, indFilter, readShrunk, temp, lenTemp)
    !
    ! accumulate sums for mean and sd if float 2, otherwise
    ! just the mean
    !
    if (ifFloat == 2) then
      call iclAvgSd(array, nx, numLines, 1, nx, 1, numLines, tmin2, tmax2, &
          tsum, tsumSq, avgSec, sdSec)
      dsumSq = dsumSq + tsumSq
    else
      call iclden(array, nx, numLines, 1, nx, 1, numLines, tmin2, &
          tmax2, tmean2)
      tsum = tmean2 * nx * numLines
    endif
    dmin2 = min(dmin2, tmin2)
    dmax2 = max(dmax2, tmax2)
    dsum = dsum + tsum
    iline = iline + numLines
  enddo
  !
  if (ifFloat == 2) then
    call sums_to_avgsd8(dsum, dsumSq, nx, nyNeeded, dmean2, sdSec)
  else
    dmean2 = dsum / (float(nx) * nyNeeded)
  endif
  loadYend = iline-1
  loadYstart = iline - numLines
  return
end subroutine scanSection


! backXform will determine the array coordinates XP and YP in an input
! image that transform to the given array indexes IX, IY in an output
! image.  Other arguments match those of cubInterp: the input image is
! NXA by NYA; the output image is NXB by NYB; XFCEN, YFCEN is the center
! coordinate of the input image; AMAT is the 2x2 transformation matrix
! and XTRANS, YTRANS are the translations.
!
subroutine backXform(nxb, nyb, amat, xfcen, yfcen, xtrans, ytrans, ix, iy, xp, yp)
  implicit none
  integer*4 nxb, nyb, ix, iy
  real*4 amat(2,2), xfcen, yfcen, xtrans, ytrans, xp, yp
  real*4 xcen, ycen, xcenOut, ycenOut, denom, a11, a12, a21, a22, dyo, dxo
  !
  ! Calc inverse transformation
  !
  xcen = nxb / 2. + xtrans + 0.5
  ycen = nyb / 2. + ytrans + 0.5
  xcenOut = xfcen + 0.5
  ycenOut = yfcen + 0.5
  denom = amat(1, 1) * amat(2, 2) - amat(1, 2) * amat(2, 1)
  a11 =  amat(2, 2) / denom
  a12 = -amat(1, 2) / denom
  a21 = -amat(2, 1) / denom
  a22 =  amat(1, 1) / denom
  !
  ! get coordinate transforming to ix, iy
  !
  dyo = iy - ycen
  dxo = ix - xcen
  xp = a11 * dxo + a12 * dyo + xcenOut
  yp = a21 * dxo + a22 * dyo + ycenOut
  return
end subroutine backXform


! getReducedSize returns the input size and offset in one dimension when there is binning
! or shrinkage reduction
!
subroutine getReducedSize(nx, reduction, doShrink, nxBin, xOffset)
  implicit none
  integer*4 nx, nxBin, ixOffset
  real*4 reduction, xOffset
  logical doShrink
  if (doShrink) then
    nxBin = nx / reduction
    xOffset = (nx - nxBin * reduction) / 2.
  else
    call getBinnedSize(nx, nint(reduction), nxBin, ixOffset)
    xOffset = ixOffset
  endif
  return
end subroutine getReducedSize


! readBinnedOrReduced will read an area from the file with either binning or reduction
!
subroutine readBinnedOrReduced(imUnit, iz, array, nxDim, nyDim, xUBstart, yUBstart,  &
    redFac, nxRed, nyRed, ifiltType, doShrink, temp, lenTemp)
  implicit none 
  integer*4 imUnit, iz, nxDim, nyDim, nxRed, nyRed, ifiltType, lenTemp, ierr
  real*4 array(*), xUBstart, yUBstart, temp(*), redFac
  logical doShrink
  character*100 listString

  if (doShrink) then
    call irdReduced(imUnit, iz, array, nxDim, xUBstart, yUBstart, redFac, nxRed, &
        nyRed, ifiltType, temp, lenTemp, ierr)
    if (ierr > 0) then
      write(listString, '(a,i2,a)') 'CALLING irdReduced TO READ IMAGE (ERROR CODE', &
          ierr, ')'
      call exitError(listString)
    endif
  else
    call irdBinned(imUnit, iz, array, nxDim, nyDim, nint(xUBstart), nint(yUBstart), &
        nint(redFac), nxRed, nyRed, temp, lenTemp, ierr)
  endif
  if (ierr .ne. 0) call exitError('READING IMAGE FILE')
end subroutine readBinnedOrReduced

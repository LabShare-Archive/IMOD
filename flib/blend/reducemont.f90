! * * * * * * * * REDUCEMONT.FOR * * * * * * *
!
! REDUCEMONT will reduce images in size by an integral factor (1, 2, 3,
! etc.) using simple averaging of blocks of pixels.  It will also
! "recut" an image into different sized pieces.  Specifically, it can
! take either a whole or a montaged image as input and produce a whole
! or a montaged image as output, with the montage pieces of a desired
! size, and with a different total size if desired.  Header origin
! and scaling information are generated so that the new images will
! have the same coordinate system as the original images.  Thus,
! models will display properly on reduced image stacks.
!
! See man page for details
!
! $Id$
!
program reducemont
  use blendvars
  implicit none
  !
  character*320 filename
  integer*4 mxyzin(3), nxyzst(3) /0, 0, 0/
  real*4 cell(6) /1., 1., 1., 0., 0., 0./
  character*80 titlech
  !
  integer*4, allocatable :: listz(:), izWant(:)
  integer*4, allocatable :: ixPcTmp(:), iyPcTmp(:), izPcTmp(:)
  character dat * 9, tim * 8
  real*4 title(20), delta(3)
  logical anyPixels, doFast, anyLinesOut, xinLong, loadLines
  integer*4 modePow(0:15) /8, 15, 8, 0, 0, 0, 16, 0, 0, 9, 10, 11, 12, 13, 14, 15/
  !
  integer*4 modeIn, numListZ, minZpiece, maxZpiece, numSect, nxTotPix, nyTotPix
  integer*4 maxXpiece, maxYpiece, modeOut, ifFloat, i, minXoverlap
  integer*4 minYoverlap, nreduce, minXwant, minYwant, maxXwant
  integer*4 maxYwant, nxTotWant, nyTotWant, newXpieces, newYpieces
  integer*4 newXtotPix, newYtotPix, newXframe, newYframe, newMinXpiece
  integer*4 newMinYpiece, ifWant, ierr
  real*4 xOrigin, yOrigin, zOrigin, dmin, dmax, outMin, outMax, dfltInMin
  real*4 dfltInMax, reduce, curInMin, curInMax, pixScale, pixAdd, tsum
  integer*4 ixFrame, iyFrame, ipc, nshort, nlong, ishort, ilong, indBray
  integer*4 newUse, newPieceXll, newPieceYll, nxFast, nyFast, iyFast, ixFast
  integer*4 indYlow, indYhigh, numLinesOut, indXlow, indXhigh, inOnePiece
  integer*4 ixInll, iyInll, inPieceNum, indx, indy, iyStart, iyEnd, ioutBase
  integer*4 ixStart, ixEnd, indBase, ix, iy, nsum, ilineOut, ifill, loadYlow, loadYhigh
  real*4 dminOut, dmaxOut, grandSum, sum, val, tmpsum, tmean
  integer*4 nzWant, newXoverlap, newYoverlap, kti, izSect, iwant, lineLoadMBlimit
  integer*4 ixOut, iyout, ixInPiece, iyInPiece, ifRevise, mostNeeded, nyUse
  character*100000 listString
  logical noFFTsizes
  !
  logical pipinput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetTwoIntegers, PipGetBoolean, PipGetString
  integer*4 PipGetInOutFile, PipGetLogical
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  reducemont
  integer numOptions
  parameter (numOptions = 16)
  character*(40 * numOptions) options(1)
  options(1) = &
      'imin:ImageInputFile:FN:@plin:PieceListInput:FN:@imout:ImageOutputFile:FN:@'// &
      'plout:PieceListOutput:FNM:@sections:SectionsToDo:LIM:@mode:ModeToOutput:I:@'// &
      'float:FloatToRange:B:@bin:BinByFactor:I:@xminmax:StartingAndEndingX:IP:@'// &
      'yminmax:StartingAndEndingY:IP:@frame:MaximumFrameSizeXandY:IP:@'// &
      'overlap:MinimumOverlapXandY:IP:@nofft:NoResizeForFFT:B:@line:LineLoadLimit:I:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  
  nreduce = 1
  ifFloat = 0
  minXoverlap = 0
  minYoverlap = 0
  lineLoadMBlimit = 1000
  loadLines = .false.
  noFFTsizes = .false.
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'reducemont', &
      'ERROR: REDUCEMONT - ', .false., 3, 1, 1, numOptArg, numNonOptArg)
  pipinput = numOptArg + numNonOptArg > 0

  ! Allocate temp arrays
  allocate(ixPcTmp(limInit), iyPcTmp(limInit), izPcTmp(limInit), stat = ierr)
  call memoryError(ierr, 'INITIAL ARRAYS')
  if (PipGetInOutFile('ImageInputFile', 1, ' ', filename) .ne. 0) &
      call exitError('NO INPUT IMAGE FILE SPECIFIED')
!
  call imopen(1, filename, 'ro')
  call irdhdr(1, nxyzIn, mxyzin, modeIn, dmin, dmax, dmean)
  dfill = dmean
  !
  ierr = PipGetBoolean('FloatToRange', ifFloat)
  ierr = PipGetInteger('LineLoadLimit', lineLoadMBlimit)
  ierr = PipGetLogical('NoResizeForFFT', noFFTsizes)
  !
  call irtdel(1, delta)
  call irtorg(1, xOrigin, yOrigin, zOrigin)
  !
  filename = ' '
  ierr = PipGetString('PieceListInput', filename)
  call read_piece_list(filename, ixPcTmp, iyPcTmp, izPcTmp, npcList)
  !
  ! Load image in strips and treat each line as a piece if all one image and large enough
  nyUse = nyin
  loadLines = npcList == 0 .and. ifFloat == 0 .and.  &
      4. * nxin * nyin / 1024.**2 > lineLoadMBlimit
  ! print *,loadLines, npcList, 4. * nxin * nyin / 1024.**2, lineLoadMBlimit
  if (loadLines) npcList = nzin * nyin
  !
  limNpc = npcList + 10
  if (npcList == 0) limNpc = nzin + 10
  allocate(ixPcList(limNpc), iyPcList(limNpc), izPcList(limNpc), &
      memIndex(limNpc), stat = ierr)
  call memoryError(ierr, 'ARRAYS PER PIECE')
  !
  ! if no pieces, set up mocklist
  if (npcList == 0) then
    do i = 1, nzin
      ixPcList(i) = 0
      iyPcList(i) = 0
      izPcList(i) = i - 1
    enddo
    npcList = nzin
  else if (loadLines) then
    !
    ! or if loading strips, set up the piece list for lines
    do izSect = 0, nzin - 1
      do iy = 0, nyin - 1
        i = nyin * izSect + iy + 1
        ixPcList(i) = 0
        iyPcList(i) = iy
        izPcList(i) = izSect
      enddo
    enddo
    nyUse = 1
    minYpiece = 0
    nyPieces = nyin
    nyOverlap = 0
    nxPieces = 1
    minXpiece = 0
    nXoverlap = 0
  else
    !
    ! or just copy the list
    ixPcList(1:npcList) = ixPcTmp(1:npcList)
    iyPcList(1:npcList) = iyPcTmp(1:npcList)
    izPcList(1:npcList) = izPcTmp(1:npcList)
  endif
  !
  call fill_listz(izPcList, npcList, izPcTmp, numListZ)
  minZpiece = izPcTmp(1)
  maxZpiece = izPcTmp(numListZ)
  numSect = maxZpiece + 1 - minZpiece
  limSect = numSect + 10
  allocate(listz(limSect), stat = ierr)
  call memoryError(ierr, 'ARRAYS PER SECTION')
  listz(1:numListZ) = izPcTmp(1:numListZ)
  !
  ! now check lists and get basic properties of overlap etc
  !
  if (.not. loadLines) then
    call checkList(ixPcList, npcList, 1, nxin, minXpiece, nxPieces, nXoverlap)
    call checkList(iyPcList, npcList, 1, nyUse, minYpiece, nyPieces, nyOverlap)
  endif
  if (nxPieces <= 0 .or. nyPieces <= 0) call exitError('PIECE LIST NOT GOOD')
  !
  nxTotPix = nxPieces * (nxin - nXoverlap) + nXoverlap
  nyTotPix = nyPieces * (nyUse - nyOverlap) + nyOverlap
  maxXpiece = minXpiece + nxTotPix - 1
  maxYpiece = minYpiece + nyTotPix - 1
  write(*,115) nxTotPix, 'X', nxPieces, nxin, nXoverlap
  if (loadLines) then
    write(*,115) nyTotPix, 'Y', 1, nyin, nyOverlap
  else
    write(*,115) nyTotPix, 'Y', nyPieces, nyin, nyOverlap
  endif
115 format(i7,' total ',a1,' pixels in',i4,' pieces of',i6,' pixels, with overlap of',i5)
  !
  !
  newXframe = 10 * nxTotPix
  newYframe = 10 * nyTotPix
  if (PipGetInOutFile('ImageOutputFile', 2, ' ', filename) .ne. 0) &
      call exitError('NO OUTPUT IMAGE FILE SPECIFIED')
  call imopen(2, filename, 'new')
  call itrhdr(2, 1)
  call ialnbsym(2, 0)
  !
  if (PipGetString('PieceListOutput', filename) .ne. 0) &
      call exitError('NO OUTPUT PIECE LIST FILE SPECIFIED')
  call dopen(3, filename, 'new', 'f')
  !
18 modeOut = modeIn
  ierr = PipGetInteger('ModeToOutput', modeOut)
  if (modeOut < 0 .or. modeOut > 15 .or. (modeOut >= 3 .and. modeOut .ne. 6))  &
      call exitError('BAD VALUE FOR OUTPUT MODE')
  call ialmod(2, modeOut)
  !
  ! set up output range, and default input range and minimum.  If real
  ! input, use actual input range and min; otherwise use theoretical
  ! range for the input mode.  This will preserve data values if no float
  !
  outMin = 0.
  outMax = 2**modePow(modeOut) - 1.
  if (modeIn == 2) then
    dfltInMax = dmax
    dfltInMin = dmin
    if (modeOut == 2) then
      outMin = dmin
      outMax = dmax
    endif
  else
    dfltInMax = 2**modePow(modeIn) - 1.
    dfltInMin = 0.
  endif
  !
  ! get list of sections desired, set up default as all sections
  do i = 1, numListZ
    izPcTmp(i) = listz(i)
  enddo
  nzWant = numListZ
  if (PipGetString('SectionsToDo', listString) .eq. 0) then
    call parseList2(listString, izPcTmp, nzWant, limInit)
  endif

  allocate(izWant(nzWant + 10), stat = ierr)
  call memoryError(ierr, 'ARRAYS PER SECTION')
  izWant(1:nzWant) = izPcTmp(1:nzWant)
  deallocate(ixPcTmp, iyPcTmp, izPcTmp, stat = i)
  !
  ierr = PipGetInteger('BinByFactor', nreduce)
  reduce = nreduce
  if (nreduce < 1 .or. nreduce > 64) call exitError('Binning must be between 1 and 64')
  minXwant = minXpiece
  minYwant = minYpiece
  maxXwant = minXwant + nxTotPix - 1
  maxYwant = minYwant + nyTotPix - 1
  ierr = PipGetTwoIntegers('StartingAndEndingX', minXwant, maxXwant)
  ierr = PipGetTwoIntegers('StartingAndEndingY', minYwant, maxYwant)
  if (minXwant > maxXwant .or. minYwant > maxYwant .or. maxXwant < 0 .or. &
      minXwant > nxTotPix .or. maxYwant < 0 .or. minYwant > nyTotPix)  &
      call exitError('X OR Y LIMITS TO OUTPUT ARE OUT OF RANGE')
  minXwant = int(minXwant / reduce)
  minYwant = int(minYwant / reduce)
  maxXwant = int(maxXwant / reduce)
  maxYwant = int(maxYwant / reduce)
  nxTotWant = 2 * ((maxXwant + 2 - minXwant) / 2)
  nyTotWant = 2 * ((maxYwant + 2 - minYwant) / 2)
  ierr = PipGetTwoIntegers('MaximumFrameSizeXandY', newXframe, newYframe)
  if (newXframe <= 0) newXframe = nxTotWant + 100
  if (newYframe <= 0) newYframe = nyTotWant + 100
  ierr = PipGetTwoIntegers('MinimumOverlapXandY', minXoverlap, minYoverlap)
  if (minXoverlap < 0 .or. minYoverlap < 0) &
      call exitError('NEGATIVE OVERLAP IS NOT ALLOWED')
  !
  call setOverlap(nxTotWant, minXoverlap, noFFTsizes, newXframe, 2, newXpieces, &
      newXoverlap, newXtotPix)
  call setOverlap(nyTotWant, minYoverlap, noFFTsizes, newYframe, 2, newYpieces, &
      newYoverlap, newYtotPix)
  !
  write(*,115) newXtotPix, 'X', newXpieces, newXframe, newXoverlap
  write(*,115) newYtotPix, 'Y', newYpieces, newYframe, newYoverlap
  !
  maxLineLength = newXframe + 32
  maxBsiz = (ifastSiz + maxBin) * maxLineLength
  allocate(brray(maxBsiz), mapPiece(nxPieces, nyPieces), stat = ierr)
  call memoryError(ierr, 'OUTPUT LINE ARRAY')
  !
  newMinXpiece = minXwant - (newXtotPix - nxTotWant) / 2
  newMinYpiece = minYwant - (newYtotPix - nyTotWant) / 2
  nxOut = newXframe
  nyOut = newYframe
  nzOut = 1
  dminOut = 1.e10
  dmaxOut = -1.e10
  grandSum = 0.
  call ialsiz(2, nxyzOut, nxyzst)

  nzOut = 0
  !
  ! initialize memory allocator
  !
  memLim = 256
  if (loadLines) memLim = max(memLim, nreduce * (newYframe + 1))
  allocate(izMemList(memLim), lastUsed(memLim), stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR PIECE BUFFER')

  call clearShuffle()

  ! If loading lines, need all the lines for a row of frames
  if (loadLines) then
    npixIn = nxin
    maxSiz = nxin * int((newYframe + 1) * nreduce, kind = 8)
    maxLoad = newYframe
    if (npixIn > memMaximum)  call exitError( &
        'OUTPUT FRAMES TOO LARGE IN Y FOR 32-BIT ARRAY INDEXES; TRY SMALLER IN Y')
    allocate(array(maxSiz), stat = ierr)
    call memoryError(ierr, 'MAIN IMAGE ARRAY; TRY SMALLER OUTPUT FRAME SIZE IN Y')
  else
    !
    ! Otherwise allocate memory as in blendmont - start with minimum size, then
    ! go to preferred memory if that is needed to get 4 pieces, then drop
    ! back to minimum pieces needed
    npixIn = nxin * int(nyin, kind = 8)
    mostNeeded = nxPieces * nyPieces
    maxLoad = min(memMinimum / npixIn, memLim, mostNeeded)
    maxLoad = max(maxLoad, min(4, mostNeeded))
    if (maxLoad * npixIn > memPreferred) maxLoad = min(2, mostNeeded)
    if (maxLoad * npixIn > memMaximum) call exitError( &
        'IMAGES TOO LARGE FOR 32-BIT ARRAY INDEXES')
    maxSiz = maxLoad * npixIn
    allocate(array(maxSiz), stat = ierr)
    if (ierr .ne. 0 .and. maxLoad > min(2, mostNeeded)) then
      maxLoad = min(2, mostNeeded)
      maxSiz = maxLoad * npixIn
      allocate(array(maxSiz), stat = ierr)
    endif
    call memoryError(ierr, 'MAIN IMAGE ARRAY')
  endif

  !
  ! loop on z: do everything within each section for maximum efficiency
  !
  do ilistz = 1, numListZ
    izSect = listz(ilistz)
    xinLong = nxPieces > nyPieces .and. .not. loadLines
    !
    ! test if this section is wanted in output: if not, skip out
    !
    ifWant = 0
    do iwant = 1, nzWant
      if (izWant(iwant) == izSect) ifWant = 1
    enddo
    if (ifWant == 0) cycle
    write(*,'(a,i4)') ' working on section #', izSect
    !
    ! make a map of pieces in this sections
    !
    do iyFrame = 1, nyPieces
      do ixFrame = 1, nxPieces
        mapPiece(ixFrame, iyFrame) = 0
      enddo
    enddo
    do ipc = 1, npcList
      if (izPcList(ipc) == izSect) then
        ixFrame = 1 + (ixPcList(ipc) - minXpiece) / (nxin - nXoverlap)
        iyFrame = 1 + (iyPcList(ipc) - minYpiece) / (nyUse - nyOverlap)
        mapPiece(ixFrame, iyFrame) = ipc
      endif
    enddo
    !
    call  crossValue(xinLong, nxPieces, nyPieces, nshort, nlong)
    if (ifFloat == 0) then
      curInMin = dfltInMin
      curInMax = dfltInMax
    else
      curInMax = -1.e10
      curInMin = 1.e10
      do ilong = nlong, 1, -1
        do ishort = nshort, 1, -1
          call crossValue(xinLong, ishort, ilong, ixFrame, iyFrame)
          if (mapPiece(ixFrame, iyFrame) > 0) then
            call shuffler(mapPiece(ixFrame, iyFrame), indBray)
            do i = indBray, indBray + nxin * nyin - 1
              curInMin = min(curInMin, array(i))
              curInMax = max(curInMax, array(i))
            enddo
          endif
        enddo
      enddo
    endif
    !
    ! now get output scaling factor and additive factor
    !
    pixScale = (outMax - outMin) / max(1., curInMax - curInMin)
    pixAdd = outMin - pixScale * curInMin


    if (.not. loadLines) then
      !
      ! look through memory list and renumber them with priorities
      ! backwards from the first needed piece
      newUse = juseCount - 1
      do ilong = 1, nlong
        do ishort = 1, nshort
          call crossValue(xinLong, ishort, ilong, ixFrame, iyFrame)
          if (mapPiece(ixFrame, iyFrame) > 0) then
            do i = 1, maxLoad
              if (izMemList(i) == mapPiece(ixFrame, iyFrame)) then
                lastUsed(i) = newUse
                newUse = newUse-1
              endif
            enddo
          endif
        enddo
      enddo
    endif
    !
    ! GET THE PIXEL OUT
    ! -  loop on output frames; within each frame loop on little boxes
    !
    call  crossValue(xinLong, newXpieces, newYpieces, nshort, nlong)
    !
    do ilong = 1, nlong
      if (loadLines) then
        !
        ! If loading lines, figure out starting and ending lines for this row of frames
        ! and load them into array
        newPieceYll = newMinYpiece + (ilong - 1) * (newYframe - newYoverlap)
        loadYlow = max(0, newPieceYll * nReduce)
        loadYhigh = min(nyin - 1, (newPieceYll + newYframe) * nReduce - 1)
        call imposn(1, izSect, loadYlow)
        call irdsecl(1, array, loadYhigh + 1 - loadYlow, *99)
        !
        ! Fill the right part of the memIndex array with the line numbers
        do i = loadYlow, loadYhigh
          memIndex(izSect * nyin + i + 1) = i + 1 - loadYlow
        enddo
      endif

      do ishort = 1, nshort
        call crossValue(xinLong, ishort, ilong, ixOut, iyout)
        newPieceYll = newMinYpiece + (iyout - 1) * (newYframe - newYoverlap)
        newPieceXll = newMinXpiece + (ixOut - 1) * (newXframe - newXoverlap)
        anyPixels = .false.
        anyLinesOut = .false.
        tsum = 0.
        !
        ! do fast little boxes
        !
        nxFast = (newXframe + (ifastSiz - 1)) / ifastSiz
        nyFast = (newYframe + (ifastSiz - 1)) / ifastSiz
        !
        ! loop on boxes, get lower & upper limits in each box
        !
        do iyFast = 1, nyFast
          indYlow = newPieceYll + (iyFast - 1) * ifastSiz
          indYhigh = min(indYlow + ifastSiz, newPieceYll + newYframe) - 1
          numLinesOut = indYhigh + 1 - indYlow
          !
          ! fill array with dmean
          !
          do i = 1, nxOut * numLinesOut
            brray(i) = dmean
          enddo
          !
          ! If loading lines without binning, then just fill the array directly
          if (loadLines .and. nReduce == 1) then
            indXlow = max(0, newPieceXll)
            indXhigh = min(newPieceXll + nxOut, nxin) - 1
            do indy = indYlow, indYhigh
              if (indy >= loadYlow .and. indy <= loadYhigh) then
                do indx = indXlow, indXhigh
                  brray(1 + indx - newPieceXll + nxOut * (indy - indYlow)) =  &
                      array(1 + indx + nxin * (indy - loadYlow))
                enddo
                anyPixels = .true.
              endif
            enddo
          else
          !
            do ixFast = 1, nxFast
              indXlow = newPieceXll + (ixFast - 1) * ifastSiz
              indXhigh = min(indXlow + ifastSiz, newPieceXll + newXframe) - 1
              !
              ! check # of edges, and prime piece number, for each corner
              !
              doFast = .true.
              do indy = indYlow, indYhigh, indYhigh - indYlow
                do indx = indXlow, indXhigh, indXhigh - indXlow
                  call checkBox(indx, indy, nreduce, nyUse, inPieceNum, ixInll, iyInll)
                  if (indx == indXlow .and. indy == indYlow) then
                    inOnePiece = inPieceNum
                    ixInPiece = ixInll
                    iyInPiece = iyInll
                  endif
                  doFast = doFast .and. inPieceNum == inOnePiece
                enddo
              enddo
              !
              ! ALL ON ONE PIECE:
              !
              if (doFast .and. inPieceNum > 0) then
                call shuffler(inPieceNum, indBray)
                do indy = indYlow, indYhigh
                  iyStart = iyInPiece + (indy - indYlow) * nreduce
                  iyEnd = iyStart + nreduce-1
                  ioutBase = nxOut * (indy - indYlow) + 1 - newPieceXll
                  do indx = indXlow, indXhigh
                    if (nreduce == 1) then
                      brray(ioutBase + indx) = &
                          array(ixInPiece + indx - indXlow + nxin * &
                          (iyInPiece + indy - indYlow) + indBray)
                    else
                      ixStart = ixInPiece + (indx - indXlow) * nreduce
                      ixEnd = ixStart + nreduce-1
                      sum = 0.
                      do iy = iyStart, iyEnd
                        indBase = nxin * iy + indBray
                        do ix = ixStart, ixEnd
                          sum = sum + array(indBase + ix)
                        enddo
                      enddo
                      brray(ioutBase + indx) = sum / nreduce**2
                    endif
                  enddo
                enddo
                anyPixels = .true.
              elseif (.not.doFast .or. nyUse < 2 * nReduce) then
                !
                ! in or near an edge: loop on each pixel
                !
                do indy = indYlow, indYhigh
                  ioutBase = nxOut * (indy - indYlow) + 1 - newPieceXll
                  do indx = indXlow, indXhigh
                    call checkBox(indx, indy, nreduce, nyUse, inPieceNum, ixInPiece, &
                        iyInPiece)
                    if (inPieceNum > 0) then
                      !
                      ! if this output pixel comes all from one input piece
                      !
                      call shuffler(inPieceNum, indBray)
                      if (nreduce == 1) then
                        brray(ioutBase + indx) = array(ixInPiece + nxin * &
                            iyInPiece + indBray)
                      else
                        iyEnd = iyInPiece + nreduce-1
                        ixEnd = ixInPiece + nreduce-1
                        sum = 0.
                        do iy = iyInPiece, iyEnd
                          indBase = nxin * iy + indBray
                          do ix = ixInPiece, ixEnd
                            sum = sum + array(indBase + ix)
                          enddo
                        enddo
                        brray(ioutBase + indx) = sum / nreduce**2
                      endif
                      anyPixels = .true.
                    elseif (inPieceNum == 0) then
                      !
                      ! but if it comes from more than one piece, need to
                      ! get each pixel independently
                      !
                      ixStart = indx * nreduce
                      ixEnd = ixStart + nreduce-1
                      iyStart = indy * nreduce
                      iyEnd = iyStart + nreduce-1
                      nsum = 0
                      sum = 0.
                      do iy = iyStart, iyEnd
                        do ix = ixStart, ixEnd
                          call findPixel(ix, iy, nyUse, inPieceNum, ixInPiece, iyInPiece)
                          if (inPieceNum > 0) then
                            call shuffler(inPieceNum, indBray)
                            sum = sum + array(ixInPiece + nxin * (iyInPiece) + indBray)
                            nsum = nsum + 1
                          endif
                        enddo
                      enddo
                      if (nsum > 0) then
                        brray(ioutBase + indx) = sum / nsum
                        anyPixels = .true.
                      endif
                    endif
                  enddo
                enddo
              endif
            enddo
          endif
          !
          ! if any pixels have been present in this frame, write lines out
          !
          if (anyPixels) then
            tmpsum = 0.
            do i = 1, nxOut * numLinesOut
              val = pixScale * brray(i) + pixAdd
              brray(i) = val
              dminOut = min(dminOut, val)
              dmaxOut = max(dmaxOut, val)
              tmpsum = tmpsum + val
            enddo
            tsum = tsum + tmpsum
            ilineOut = (iyFast - 1) * ifastSiz
            call imposn(2, nzOut, ilineOut)
            call iwrsecl(2, brray, numLinesOut)
            !
            ! if this is the first time anything is written, and it
            ! wasn't the first set of lines, then need to go back and
            ! fill the lower part of frame with mean values
            !
            if (.not.anyLinesOut .and. iyFast > 1) then
              val = dmean * pixScale + pixAdd
              do i = 1, nxOut * ifastSiz
                brray(i) = val
              enddo
              call imposn(2, nzOut, 0)
              do ifill = 1, iyFast - 1
                call iwrsecl(2, brray, ifastSiz)
              enddo
              tsum = tsum + val * nxOut * (iyFast - 1) * ifastSiz
            endif
            anyLinesOut = .true.
          endif
        enddo
        !
        ! if any pixels present, write piece coordinates
        !
        if (anyPixels) then
          grandSum = grandSum + tsum
          !write(*,'(a,i5)') ' wrote new frame #', nzOut
          nzOut = nzOut + 1
          !
          ! 12/12/03: no longer keep header up to date in case of crashes
          !
          write(3, '(2i6,i4)') newPieceXll, newPieceYll, izSect
        endif
        !
      enddo
    enddo
  enddo
  !
  tmean = grandSum / (float(nzOut * nxOut) * nyOut)
  call ialsiz(2, nxyzOut, nxyzst)
  call ialsam(2, nxyzOut)
  cell(1) = nxOut * nreduce * delta(1)
  cell(2) = nyOut * nreduce * delta(2)
  cell(3) = nzOut * delta(3)
  call ialcel(2, cell)
  call b3ddate(dat)
  call time(tim)
  !
  write(titlech, 90) nreduce, dat, tim
90 format( 'REDUCEMONT: recut and reduced by factor of',i3, t57, a9, 2x, a8 )
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
  call iwrhdr(2, title, 1, dminOut, dmaxOut, tmean)
  close(3)
  call imclose(2)
  call exit(0)
99 call exitError('READING IMAGE')
end program reducemont



subroutine checkbox(indx, indy, nreduce, nyUse, inpieceno, ixinll, iyinll)
  implicit none
  integer*4 indx, indy, nreduce, nyUse, inpieceno, ixinll, iyinll
  integer*4 ix, iy, inpt, ixinpc, iyinpc
  !
  ix = indx * nreduce
  iy = indy * nreduce
  call findpixel(ix, iy, nyUse, inpieceno, ixinll, iyinll)
  if (nreduce == 1) return
  !
  call findpixel(ix + nreduce-1, iy, nyUse, inpt, ixinpc, iyinpc)
  if (inpt .ne. inpieceno) go to 10
  !
  call findpixel(ix + nreduce-1, iy + nreduce-1, nyUse, inpt, ixinpc, iyinpc)
  if (inpt .ne. inpieceno) go to 10
  !
  call findpixel(ix, iy + nreduce-1, nyUse, inpt, ixinpc, iyinpc)
  if (inpt == inpieceno) return
  !
10 inpieceno = 0
  return
end subroutine checkbox



subroutine findpixel(ix, iy, nyUse, inpieceno, ixInPiece, iyInPiece)
  use blendvars
  implicit none
  integer*4 ix, iy, nyUse, inPieceNo, ixInPiece, iyInPiece
  !
  integer*4 nxDel, nyDel, ipieceX, ipieceY, idx, idy, newx, newPcX
  integer*4 newy, newPcY
  integer*4 nxTotPix, nyTotPix, maxXpiece, maxYpiece
  !
  ! DNM 8/18/02: need to either recompute these or equivalence them
  ! them to something in common
  !
  nxTotPix = nxPieces * (nxin - nXoverlap) + nXoverlap
  nyTotPix = nyPieces * (nyUse - nyOverlap) + nyOverlap
  maxXpiece = minXpiece + nxTotPix - 1
  maxYpiece = minYpiece + nyTotPix - 1
 ! print *,ix, iy, nyUse
  !
  inPieceNo = -1
  if (ix < minXpiece .or. iy < minYpiece .or. &
      ix > maxXpiece .or. iy > maxYpiece) return
  nxDel = nxin - nXoverlap
  nyDel = nyUse - nyOverlap
  !
  ! get piece # and coordinate within piece
  !
  ipieceX = 1 + (ix - minXpiece) / nxDel
  ixInPiece = mod(ix - minXpiece, nxDel)
  ipieceY = 1 + (iy - minYpiece) / nyDel
  iyInPiece = mod(iy - minYpiece, nyDel)
  !print *,ipieceX, ixInPiece, ipieceY, iyInPiece
  !
  ! shift into the lower piece if below the middle of overlap zone
  ! (except for 1st piece) or if past the last piece.
  !
  if ((ixInPiece < nXoverlap / 2 .and. ipieceX > 1) .or. ipieceX > nxPieces) then
    ipieceX = ipieceX - 1
    ixInPiece = ixInPiece + nxDel
  endif
  if ((iyInPiece < nyOverlap / 2 .and. ipieceY > 1) .or. ipieceY > nyPieces) then
    ipieceY = ipieceY - 1
    iyInPiece = iyInPiece + nyDel
  endif
  !
  ! if outside a piece in either coordinate (negative overlap) then
  ! return on blank location
  !
  if (ixInPiece >= nxin .or. iyInPiece >= nyUse) return
  !
  ! if the piece exist, done
  !
  if (mapPiece(ipieceX, ipieceY) > 0) go to 10
!  print *,ipieceX, ipieceY,'not mapped'
  idx = -1
  idy = -1
  !
  ! set up for potential changes in X or Y
  !
  if (ixInPiece < nXoverlap .and. ipieceX > 1) then
    idx = ixInPiece - nXoverlap / 2
    newx = ixInPiece + nxDel
    newPcX = ipieceX - 1
  elseif (ixInPiece >= nxDel .and. ipieceX < nxPieces) then
    idx = nxin - 1 - nXoverlap / 2 - ixInPiece
    newx = ixInPiece - nxDel
    newPcX = ipieceX + 1
  endif
  if (iyInPiece < nyOverlap .and. ipieceY > 1) then
    idy = iyInPiece - nyOverlap / 2
    newy = iyInPiece + nyDel
    newPcY = ipieceY - 1
  elseif (iyInPiece >= nyDel .and. ipieceY < nyPieces) then
    idy = nyUse - 1 - nyOverlap / 2 - iyInPiece
    newy = iyInPiece - nyDel
    newPcY = ipieceY + 1
  endif
  !
  ! take care of the various cases
  !
  if (idx < 0 .and. idy < 0) return
  if (idx >= 0 .and. idy < 0) then
    if (mapPiece(newPcX, ipieceY) <= 0) return
    go to 8
  elseif (idx < 0 .and. idy >= 0) then
    if (mapPiece(ipieceX, newPcY) <= 0) return
    go to 9
  else
    if (idx <= idy) then
      if (mapPiece(newPcX, ipieceY) > 0) go to 8
      if (mapPiece(ipieceX, newPcY) > 0) go to 9
    else
      if (mapPiece(ipieceX, newPcY) > 0) go to 9
      if (mapPiece(newPcX, ipieceY) > 0) go to 8
    endif
    if (mapPiece(newPcX, newPcY) <= 0) return
    ipieceX = newPcX
    ixInPiece = newx
    go to 9
  endif
  return
  !
8 ipieceX = newPcX
  ixInPiece = newx
  go to 10
9 ipieceY = newPcY
  iyInPiece = newy
10 inPieceNo = mapPiece(ipieceX, ipieceY)
!  print *,'in piece', inPieceNo
  return
end subroutine findpixel


subroutine crossvalue(xinlong, nxpieces, nypieces, nshort, nlong)
  implicit none
  logical xinlong
  integer*4 nxpieces, nypieces, nshort, nlong
  if (xinlong) then
    nshort = nypieces
    nlong = nxpieces
  else
    nshort = nxpieces
    nlong = nypieces
  endif
  return
end subroutine crossvalue

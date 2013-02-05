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
  character*80 filnam
  integer*4 mxyzin(3), nxyzst(3) /0, 0, 0/
  real*4 cell(6) /1., 1., 1., 0., 0., 0./
  !
  !
  ! 7/7/00 CER: remove the encode's; titlech is the temp space
  !
  character*80 titlech
  !
  integer*4, allocatable :: listz(:), izWant(:)
  integer*4, allocatable :: ixPcTmp(:), iyPcTmp(:), izPcTmp(:)
  character dat * 9, tim * 8
  real*4 title(20), delta(3)
  logical anyPixels, doFast, anyLinesOut, xinLong
  integer*4 modePow(0:15) /8, 15, 8, 0, 0, 0, 0, 0, 0, 9, 10, 11, 12, 13, 14, 15/
  !
  integer*4 modeIn, numListZ, minZpiece, maxZpiece, numSect, nxTotPix, nyTotPix
  integer*4 maxXpiece, maxYpiece, modeOut, ifFloat, i, minXoverlap
  integer*4 minYoverlap, numTrials, nreduce, minXwant, minYwant, maxXwant
  integer*4 maxYwant, nxTotWant, nyTotWant, newXpieces, newYpieces
  integer*4 newXtotPix, newYtotPix, newXframe, newYframe, newMinXpiece
  integer*4 newMinYpiece, ifWant
  real*4 xOrigin, yOrigin, zOrig, dmin, dmax, outMin, outMax, dfltInMin
  real*4 dfltInMax, reduce, curInMin, curInMax, pixScale, pixAdd, tsum
  integer*4 ixFrame, iyFrame, ipc, nshort, nlong, ishort, ilong, indBray
  integer*4 newUse, newPieceXll, newPieceYll, nxFast, nyFast, iyFast, ixFast
  integer*4 indYlow, indYhigh, numLinesOut, indXlow, indXhigh, inOnePiece
  integer*4 ixInll, iyInll, inPieceNum, indx, indy, iyStart, iyEnd, ioutBase
  integer*4 ixStart, ixEnd, indBase, ix, iy, nsum, ilineOut, ifill
  real*4 dminOut, dmaxOut, grandSum, sum, val, tmpsum, tmean
  integer*4 nzWant, newXoverlap, newYoverlap, kti, izSect, iwant
  integer*4 ixOut, iyout, ixInPiece, iyInPiece, ifRevise, mostNeeded
  !
  call setExitPrefix('ERROR: REDUCEMONT -')
  nreduce = 1
  !
  ! Allocate temp arrays
  allocate(ixPcTmp(limInit), iyPcTmp(limInit), izPcTmp(limInit), stat = i)
  if (i .ne. 0) call exitError('ALLOCATING INITIAL ARRAYS')
  !
  write(*,'(1x,a,$)') 'Input image file: '
  read(5, '(a)') filnam
  call imopen(1, filnam, 'ro')
  call irdhdr(1, nxyzIn, mxyzin, modeIn, dmin, dmax, dmean)
  dfill = dmean
  !
  call irtdel(1, delta)
  call irtorg(1, xOrigin, yOrigin, zOrig)
  !
  write(*,'(1x,a,$)') 'Piece list file if image is a'// &
      ' montage, otherwise Return: '
  read(*,'(a)') filnam
  call read_piece_list(filnam, ixPcTmp, iyPcTmp, izPcTmp, npcList)
  limNpc = npcList + 10
  if (npcList == 0) limNpc = nzin + 10
  allocate(ixPcList(limNpc), iyPcList(limNpc), izPcList(limNpc), &
      memIndex(limNpc), stat = i)
  if (i .ne. 0) call exitError('ALLOCATING ARRAYS PER PIECE')
  !
  ! if no pieces, set up mocklist
  if (npcList == 0) then
    do i = 1, nzin
      ixPcTmp(i) = 0
      iyPcTmp(i) = 0
      izPcTmp(i) = i - 1
    enddo
    npcList = nzin
  endif
  ixPcList(1:npcList) = ixPcTmp(1:npcList)
  iyPcList(1:npcList) = iyPcTmp(1:npcList)
  izPcList(1:npcList) = izPcTmp(1:npcList)
  !
  call fill_listz(izPcList, npcList, izPcTmp, numListZ)
  minZpiece = izPcTmp(1)
  maxZpiece = izPcTmp(numListZ)
  numSect = maxZpiece + 1 - minZpiece
  limSect = numSect + 10
  allocate(listz(limSect), stat = i)
  if (i .ne. 0) call exitError('ALLOCATING ARRAYS PER SECTION')
  listz(1:numListZ) = izPcTmp(1:numListZ)
  !
  ! now check lists and get basic properties of overlap etc
  !
  call checkList(ixPcList, npcList, 1, nxin, minXpiece, nxPieces, &
      nXoverlap)
  call checkList(iyPcList, npcList, 1, nyin, minYpiece, nyPieces, &
      nyOverlap)
  if (nxPieces <= 0 .or. nyPieces <= 0) call exitError('PIECE LIST NOT GOOD')
  !
  nxTotPix = nxPieces * (nxin - nXoverlap) + nXoverlap
  nyTotPix = nyPieces * (nyin - nyOverlap) + nyOverlap
  maxXpiece = minXpiece + nxTotPix - 1
  maxYpiece = minYpiece + nyTotPix - 1
  write(*,115) nxTotPix, 'X', nxPieces, nxin, nXoverlap
  write(*,115) nyTotPix, 'Y', nyPieces, nyin, nyOverlap
115 format(i7,' total ',a1,' pixels in',i4,' pieces of', &
      i6, ' pixels, with overlap of',i5)
  !
  !
  write(*,'(1x,a,$)') 'Output image file: '
  read(5, '(a)') filnam
  call imopen(2, filnam, 'new')
  call itrhdr(2, 1)
  call ialnbsym(2, 0)
  !
  write(*,'(1x,a,$)') 'Name of output piece list file: '
  read(5, '(a)') filnam
  call dopen(3, filnam, 'new', 'f')
  ! open(3, file=filnam, form='formatted', status='new' &
  ! , carriagecontrol='list', err=30)
  !
18 modeOut = modeIn
  write(*,'(1x,a,i2,a,$)') 'Mode for output file [', modeOut, ']: '
  read(5,*) modeOut
  if (modeOut < 0 .or. modeOut > 15 .or. &
      (modeOut >= 3 .and. modeOut <= 8)) then
    print *,'bad mode value'
    go to 18
  endif
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
  write(*,'(1x,a,$)') &
      '1 to float each section to maximum range, 0 not to: '
  read(5,*) ifFloat
  !
  !
  ! get list of sections desired, set up default as all sections
  !
  print *,'Enter list of sections to be included in output '// &
      'file (ranges ok)', '   or / to include all sections'
  do i = 1, numListZ
    izPcTmp(i) = listz(i)
  enddo
  nzWant = numListZ
  call rdlist(5, izPcTmp, nzWant)
  allocate(izWant(nzWant + 10), stat = i)
  if (i .ne. 0) call exitError('ALLOCATING ARRAYS PER SECTION')
  izWant(1:nzWant) = izPcTmp(1:nzWant)
  deallocate(ixPcTmp, iyPcTmp, izPcTmp, stat = i)
  !
  minXoverlap = 2
  minYoverlap = 2
  numTrials = 0
  reduce = nreduce
32 write(*,'(1x,a,$)') 'Reduction factor: '
  read(5,*) nreduce
  reduce = nreduce
  minXwant = minXpiece
  minYwant = minYpiece
  maxXwant = minXwant + nxTotPix - 1
  maxYwant = minYwant + nyTotPix - 1
  write(*,'(1x,a,/,a,4i6,a,$)') 'Enter Min X, Max X, Min Y, and'// &
      ' Max Y coordinates to process into output section,' &
      , '    or / for whole input section [=' &
      , minXwant, maxXwant, minYwant, maxYwant, ']: '
  read(5,*) minXwant, maxXwant, minYwant, maxYwant
  minXwant = int(minXwant / reduce)
  minYwant = int(minYwant / reduce)
  maxXwant = int(maxXwant / reduce)
  maxYwant = int(maxYwant / reduce)
  nxTotWant = 2 * ((maxXwant + 2 - minXwant) / 2)
  nyTotWant = 2 * ((maxYwant + 2 - minYwant) / 2)
  write(*,'(1x,a,$)') &
      'Maximum new X and Y frame size, minimum overlap: '
  read(5,*) newXframe, newYframe, minXoverlap, minYoverlap
  if (numTrials <= 1) then                       !on first 2 trials, enforce min
    minXoverlap = max(2, minXoverlap)          !overlap of 2 so things look
    minYoverlap = max(2, minYoverlap)          !nice in wimp.  After that, let
  endif                                     !the user have it.
  numTrials = numTrials + 1
  !
  call setOverlap(nxTotWant, minXoverlap, newXframe, 2, newXpieces, &
      newXoverlap, newXtotPix)
  call setOverlap(nyTotWant, minYoverlap, newYframe, 2, newYpieces, &
      newYoverlap, newYtotPix)
  !
  write(*,115) newXtotPix, 'X', newXpieces, newXframe, newXoverlap
  write(*,115) newYtotPix, 'Y', newYpieces, newYframe, newYoverlap
  !
  write(*,'(1x,a,$)') &
      '1 to revise reduction, frame size, or overlap: '
  read(5,*) ifRevise
  if (ifRevise .ne. 0) go to 32
  !
  maxLineLength = newXframe + 32
  maxBsiz = (ifastSiz + maxBin) * maxLineLength
  allocate(brray(maxBsiz), mapPiece(nxPieces, nyPieces), stat = i)
  if (i .ne. 0) call exitError('ALLOCATING OUTPUT LINE ARRAY')
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
  call clearShuffle()
  !
  ! Allocate memory as in blendmont - start with minimum size, then
  ! go to preferred memory of that is needed to get 4 pieces, then drop
  ! back to minimum pieces needed
  npixIn = nxin * int(nyin, kind = 8)
  mostNeeded = nxPieces * nyPieces
  maxLoad = min(memMinimum / npixIn, memLim, mostNeeded)
  maxLoad = max(maxLoad, min(4, mostNeeded))
  if (maxLoad * npixIn > memPreferred) maxLoad = min(2, mostNeeded)
  if (maxLoad * npixIn > memMaximum) call exitError( &
      'IMAGES TOO LARGE FOR 32-BIT ARRAY INDEXES')
  maxSiz = maxLoad * npixIn
  allocate(array(maxSiz), stat = i)
  if (i .ne. 0 .and. maxLoad > min(2, mostNeeded)) then
    maxLoad = min(2, mostNeeded)
    maxSiz = maxLoad * npixIn
    allocate(array(maxSiz), stat = i)
  endif
  if (i .ne. 0) call exitError('ALLOCATING MAIN IMAGE ARRAY')

  !
  ! loop on z: do everything within each section for maximum efficiency
  !
  do ilistz = 1, numListZ
    izSect = listz(ilistz)
    write(*,'(a,i4)') ' working on section #', izSect
    xinLong = nxPieces > nyPieces
    !
    ! test if this section is wanted in output: if not, skip out
    !
    ifWant = 0
    do iwant = 1, nzWant
      if (izWant(iwant) == izSect) ifWant = 1
    enddo
    if (ifWant == 0) go to 92
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
        iyFrame = 1 + (iyPcList(ipc) - minYpiece) / (nyin - nyOverlap)
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
    !
    ! look through memory list and renumber them with priorities
    ! backwards from the first needed piece
    !
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
    !
    ! GET THE PIXEL OUT
    ! -  loop on output frames; within each frame loop on little boxes
    !
    call  crossValue(xinLong, newXpieces, newYpieces, nshort, nlong)
    !
    do ilong = 1, nlong
      do ishort = 1, nshort
        call crossValue(xinLong, ishort, ilong, ixOut, iyout)
        write(*,'(a,2i4)') ' composing frame at', ixOut, iyout
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
          do ixFast = 1, nxFast
            indXlow = newPieceXll + (ixFast - 1) * ifastSiz
            indXhigh = min(indXlow + ifastSiz, newPieceXll + newXframe) - 1
            !
            ! check # of edges, and prime piece number, for each corner
            !
            doFast = .true.
            do indy = indYlow, indYhigh, indYhigh - indYlow
              do indx = indXlow, indXhigh, indXhigh - indXlow
                call checkBox(indx, indy, nreduce, inPieceNum, ixInll, &
                    iyInll)
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
            elseif (.not.doFast) then
              !
              ! in or near an edge: loop on each pixel
              !
              do indy = indYlow, indYhigh
                ioutBase = nxOut * (indy - indYlow) + 1 - newPieceXll
                do indx = indXlow, indXhigh
                  call checkBox(indx, indy, nreduce, inPieceNum, ixInPiece, &
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
                        call findPixel(ix, iy, inPieceNum, ixInPiece, &
                            iyInPiece)
                        if (inPieceNum > 0) then
                          call shuffler(inPieceNum, indBray)
                          sum = sum + array(ixInPiece + nxin * (iyInPiece) + &
                              indBray)
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
          !
          ! if any pixels have been present in this frame, write line out
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
          write(*,'(a,i5)') ' wrote new frame #', nzOut
          nzOut = nzOut + 1
          !
          ! 12/12/03: no longer keep header up to date in case of crashes
          !
          write(3, '(2i6,i4)') newPieceXll, newPieceYll, izSect
        endif
        !
      enddo
    enddo
92 enddo
  !
  tmean = grandSum / (nzOut * float(nxOut * nyOut))
  call ialsiz(2, nxyzOut, nxyzst)
  call ialsam(2, nxyzOut)
  cell(1) = nxOut * nreduce * delta(1)
  cell(2) = nyOut * nreduce * delta(2)
  cell(3) = nzOut * delta(3)
  call ialcel(2, cell)
  call b3ddate(dat)
  call time(tim)
  !
  ! 7/7/00 CER: remove the encodes
  !
  ! encode(80, 90, title) nreduce, dat, tim
  write(titlech, 90) nreduce, dat, tim
90 format( 'REDUCEMONT: recut and reduced by factor of',i3, &
      t57, a9, 2x, a8 )
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
  call iwrhdr(2, title, 1, dmin, dmax, dmean)
  close(3)
  call imclose(2)
  call exit(0)
  call exitError('reading transforms')
end program reducemont



subroutine checkbox(indx, indy, nreduce, inpieceno, ixinll, iyinll)
  implicit none
  integer*4 indx, indy, nreduce, inpieceno, ixinll, iyinll
  integer*4 ix, iy, inpt, ixinpc, iyinpc
  !
  ix = indx * nreduce
  iy = indy * nreduce
  call findpixel(ix, iy, inpieceno, ixinll, iyinll)
  if (nreduce == 1) return
  !
  call findpixel(ix + nreduce-1, iy, inpt, ixinpc, iyinpc)
  if (inpt .ne. inpieceno) go to 10
  !
  call findpixel(ix + nreduce-1, iy + nreduce-1, inpt, ixinpc, iyinpc)
  if (inpt .ne. inpieceno) go to 10
  !
  call findpixel(ix, iy + nreduce-1, inpt, ixinpc, iyinpc)
  if (inpt == inpieceno) return
  !
10 inpieceno = 0
  return
end subroutine checkbox



subroutine findpixel(ix, iy, inpieceno, ixInPiece, iyInPiece)
  use blendvars
  implicit none
  integer*4 ix, iy, inPieceNo, ixInPiece, iyInPiece
  !
  integer*4 nxDel, nyDel, ipieceX, ipieceY, idx, idy, newx, newPcX
  integer*4 newy, newPcY
  integer*4 nxTotPix, nyTotPix, maxXpiece, maxYpiece
  !
  ! DNM 8/18/02: need to either recompute these or equivalence them
  ! them to something in common
  !
  nxTotPix = nxPieces * (nxin - nXoverlap) + nXoverlap
  nyTotPix = nyPieces * (nyin - nyOverlap) + nyOverlap
  maxXpiece = minXpiece + nxTotPix - 1
  maxYpiece = minYpiece + nyTotPix - 1
  !
  inPieceNo = -1
  if (ix < minXpiece .or. iy < minYpiece .or. &
      ix > maxXpiece .or. iy > maxYpiece) return
  nxDel = nxin - nXoverlap
  nyDel = nyin - nyOverlap
  !
  ! get piece # and coordinate within piece
  !
  ipieceX = 1 + (ix - minXpiece) / nxDel
  ixInPiece = mod(ix - minXpiece, nxDel)
  ipieceY = 1 + (iy - minYpiece) / nyDel
  iyInPiece = mod(iy - minYpiece, nyDel)
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
  if (ixInPiece >= nxin .or. iyInPiece >= nyin) return
  !
  ! if the piece exist, done
  !
  if (mapPiece(ipieceX, ipieceY) > 0) go to 10
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
    idy = nyin - 1 - nyOverlap / 2 - iyInPiece
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

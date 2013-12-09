! * * * * * EDPIECEPOINT * * * * *
!
! This program allows one to "edit" a list of piece coordinates or make up a new piece
! list for single or multiple frames.
!
! See man page for details
!
! David Mastronarde  5/8/89
!
! $Id$
!
program edpiecepoint
  implicit none
  integer LIMPIECE, LIMSEC
  parameter (LIMPIECE = 10000000, LIMSEC = 10000000)
  character*320 pieceFile
  integer*4 ixPcList(LIMPIECE), iyPcList(LIMPIECE), izPcList(LIMPIECE)
  integer*4 listZ(LIMSEC), newlist(LIMSEC), indexZlist(LIMSEC)
  integer*4 numPcList, nxFrame, nyFrame, nxDelta, nyDelta, nzDummy, ix, iy, iz
  integer*4 i, numListZ, numNewZ, ixAdd, iyAdd, izAdd, ipc, newZval, indMove, ifInColumns
  integer*4 ixBase, iyBase, inner, iouter, nInner, nOuter, nyOverlap
  integer*4  nx, minXpiece, nxPieces, nxOverlap, ny, minYpiece, nyPieces
  integer*4 iOutArg, ierr, newXoverlap, newYoverlap, ifDivByBin, ifAdjust, newNx, newNy
  integer*4 newMinX, newMinY, ibinning, inStacks, nzOuter, izInnerStart, izInnerEnd
  integer*4 izInnerInc, izOuter, izInner
  character*10240 listString
  !
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetInteger, PipGetBoolean, PipGetString
  integer*4 PipGetTwoIntegers, PipGetThreeIntegers, PipGetInOutFile
  logical*4 pipInput
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2 edpiecepoint
  !
  integer numOptions
  parameter (numOptions = 15)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@create:CreateForSections:I:@'// &
      'pieces:PiecesInXandY:IP:@spacing:SpacingInXandY:IP:@'// &
      'columns:ColumnsOfPieces:B:@stacks:StacksOfPieces:I:@divide:DivideByBinning:I:@'// &
      'overlap:NewOverlapInXandY:IP:@size:SizeInXandY:IP:@'// &
      'binned:BinnedSizeInXandY:IP:@new:NewZList:LI:@add:AddToAllCoordinates:FT:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  nxFrame = 1
  nyFrame = 1
  nxDelta = 0
  nyDelta = 0
  ixAdd = 0
  iyAdd = 0
  izAdd = 0
  ifInColumns = 0
  inStacks = 0
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'edpiecepoint', &
      'ERROR: EDPIECEPOINT - ', .true., 1, 2, 1, numOptArg, numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0
  !
  if (pipInput) then
    iOutArg = 2
    if (PipGetInteger('CreateForSections', nzDummy) == 0) then
      pieceFile = ' '
      iOutArg = 1
    else
      if (PipGetInOutFile('InputFile', 1, ' ', pieceFile) .ne. 0) &
          call exitError('NO INPUT FILE SPECIFIED')
    endif
  else
    write(*,'(1x,a)') 'Enter name of input piece or point list file (Return if none)'
    read(*,'(a)') pieceFile
  endif
  call read_piece_list(pieceFile, ixPcList, iyPcList, izPcList, numPcList)
  !
  if (numPcList == 0) then
    if (pipInput) then
      ierr = PipGetTwoIntegers('PiecesInXandY', nxFrame, nyFrame)
      ierr = PipGetTwoIntegers('SpacingInXandY', nxDelta, nyDelta)
      ierr = PipGetBoolean('ColumnsOfPieces', ifInColumns)
      ierr = PipGetInteger('StacksOfPieces', inStacks)
    else
      write(*,'(1x,a,$)') '# of sections to make new piece list for: '
      read(*,*) nzDummy
      write(*,'(1x,a,$)') '# of montage pieces in X and in Y (/ for 1,1): '
      read(5,*) nxFrame, nyFrame
      write(*,'(1x,a,$)') 'Spacing between pieces in X and Y '// &
          '(- for pieces in inverse order) : '
      read(5,*) nxDelta, nyDelta
      write(*,'(1x,a,$)') '0 for pieces in rows, 1 for pieces in columns: '
      read(5,*) ifInColumns
    endif
    !
    ! Set up loop limits for X and Y loops
    nInner = nxFrame
    nOuter = nyFrame
    if (ifInColumns .ne. 0) then
      nInner = nyFrame
      nOuter = nxFrame
    endif
    i = 0
    !
    ! Set the starting X and Y
    ixBase = 0
    iyBase = 0
    if (nxDelta < 0) ixBase = -(nxFrame - 1) * nxDelta
    if (nyDelta < 0) iyBase = -(nyFrame - 1) * nyDelta
    !
    ! Set up two loops on Z, inside and outside, and run one or the other through range
    nzOuter = nzDummy
    izInnerStart = 1
    izInnerEnd = 1
    izInnerInc = 1
    if (inStacks > 0) then
      nzOuter = 1
      izInnerEnd = nzDummy
    else if (inStacks < 0) then
      nzOuter = 1
      izInnerStart = nzDummy
      izInnerInc = -1
    endif
    !
    ! Loop to create list
    do izOuter = 1, nzOuter
      do iouter = 1, nOuter
        do inner = 1, nInner
          do izInner = izInnerStart, izInnerEnd, izInnerInc
            iz = izOuter * izInner
            ix = inner
            iy = iouter
            if (ifInColumns .ne. 0) then
              ix = iouter
              iy = inner
            endif
            i = i + 1
            ixPcList(i) = ixBase + (ix - 1) * nxDelta
            iyPcList(i) = iyBase + (iy - 1) * nyDelta
            izPcList(i) = iz - 1
          enddo
        enddo
      enddo
    enddo
    numPcList = i
  endif
  !
  if (PipGetInOutFile('OutputFile', iOutArg, 'Name of output piece or '// &
      'point list file', pieceFile) .ne. 0) &
      call exitError('NO OUTPUT FILE SPECIFIED')
  call fill_listz(izPcList, numPcList, listZ, numListZ)
  if (pipInput) then
    ibinning = 1
    ifDivByBin = 1 - PipGetInteger('DivideByBinning', ibinning)
    ifAdjust = 1 - PipGetTwoIntegers('NewOverlapInXandY', newXoverlap, newYoverlap)
    if (ifDivByBin > 0 .and. ifAdjust > 0) call exitError( &
        'YOU CANNOT ENTER BOTH -divide AND -overlap')
    if (ifDivByBin > 0 .or. ifAdjust > 0) then
      if (PipGetTwoIntegers('SizeInXandY', nx, ny) .ne. 0) call exitError('YOU MUST'// &
          ' ENTER IMAGE SIZE IN X AND Y TO ADJUST OVERLAPS OR DIVIDE BY BINNING')
      call checkList(ixPcList, numPcList, 1, nx, minXpiece , nxPieces, nxOverlap)
      call checkList(iyPcList, numPcList, 1, ny, minYpiece , nyPieces, nyOverlap)
      if (nxPieces < 1 .or. nyPieces < 1) call exitError( &
          'PIECE COORDINATES ARE NOT REGULARLY SPACED APART')
      newNx = nx
      newNy = ny
      if (ifDivByBin > 0) then
        if (ibinning < 2) call exitError('BINNING FACTOR MUST BE 2 OR HIGHER')
        if (PipGetTwoIntegers('BinnedSizeInXandY', newNx, newNy) .ne. 0) &
            call exitError('YOU MUST ENTER THE BINNED IMAGE SIZE TOO')
        !
        ! pick overlap that matches the original extent binned
        newXoverlap = nxOverlap / ibinning
        ipc = ((nxPieces - 1) * (nx - nxOverlap)) / ibinning
        if (abs((nxPieces - 1) * (newNx - newXoverlap) - ipc) > &
            abs((nxPieces - 1) * (newNx - newXoverlap - 1) - ipc)) &
            newXoverlap = newXoverlap + 1
        newYoverlap = nyOverlap / ibinning
        ipc = ((nyPieces - 1) * (ny - nyOverlap)) / ibinning
        if (abs((nyPieces - 1) * (newNy - newYoverlap) - ipc) > &
            abs((nyPieces - 1) * (newNy - newYoverlap - 1) - ipc)) &
            newYoverlap = newYoverlap + 1
      endif
      !
      ! Adjust the piece coordinates
      newMinX = minXpiece / ibinning
      newMinY = minYpiece / ibinning
      do i = 1, numPcList
        ipc = (ixPcList(i) - minXpiece) / (nx - nxOverlap)
        ixPcList(i) = newMinX + ipc * (newNx - newXoverlap)
        ipc = (iyPcList(i) - minYpiece) / (ny - nyOverlap)
        iyPcList(i) = newMinY + ipc * (newNy - newYoverlap)
      enddo
    endif
  endif
  !
  ! get list of ranges
  !
  print *,'The current list of Z values in input list is:'
  call wrlist(listZ, numListZ)
  !
  ! get new list of z values for remapping
  !
20 do i = 1, numListZ
    newlist(i) = listZ(i)
  enddo
  numNewZ = numListZ
  if (pipInput) then
    if (PipGetString('NewZList', listString) == 0) &
        call parselist2(listString, newlist, numNewZ, LIMSEC)
  else
    write(*,'(/,a,i4,a,/,a,/,a,/,a)') ' Enter new list of', numListZ, &
        ' Z values to remap these values to.', &
        ' Enter ranges just as above, or / to leave list alone,', &
        '   or -1 to replace each Z value with its negative.', &
        ' Use numbers from -999 to -990 to remove points with a'// &
        ' particular Z value.'
    call rdlist(5, newlist, numNewZ)
  endif
  !
  if (numNewZ == 1 .and. newlist(1) == -1) then
    do i = 1, numListZ
      newlist(i) = -listZ(i)
    enddo
  elseif (numNewZ .ne. numListZ) then
    if (pipInput) call exitError('NUMBER OF Z VALUES DOES NOT CORRESPOND')
    print *,'Number of Z values does not correspond'
    go to 20
  endif
  do i = 1, numListZ
    indexZlist(listZ(i) + 1 - listZ(1)) = i
  enddo
  !
  if (pipInput) then
    ierr = PipGetThreeIntegers('AddToAllCoordinates', ixAdd, iyAdd, izAdd)
  else
    write(*,'(1x,a,$)') &
        'Amounts to ADD to ALL X, Y and Z coordinates (/ for 0,0,0): '
    read(*,*) ixAdd, iyAdd, izAdd
  endif
  !
  ipc = 1
  do while(ipc <= numPcList)
    newZval = newlist(indexZlist(izPcList(ipc) + 1 - listZ(1)))
    if (newZval < -999 .or. newZval > -990) then
      izPcList(ipc) = newZval + izAdd
      ixPcList(ipc) = ixPcList(ipc) + ixAdd
      iyPcList(ipc) = iyPcList(ipc) + iyAdd
      ipc = ipc + 1
    else
      numPcList = numPcList - 1
      do indMove = ipc, numPcList
        ixPcList(indMove) = ixPcList(indMove + 1)
        iyPcList(indMove) = iyPcList(indMove + 1)
        izPcList(indMove) = izPcList(indMove + 1)
      enddo
    endif
  enddo
  call dopen(3, pieceFile, 'new', 'f')
  write(3, '(3i8)') (ixPcList(i), iyPcList(i), izPcList(i) , i = 1, numPcList)
  close(3)
  call exit(0)
end program edpiecepoint

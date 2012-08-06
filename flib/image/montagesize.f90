! * * * * * * MONTAGESIZE * * * * * *
!
! MONTAGESIZE will determine the X, Y, and Z dimensions of a montaged
! image file, from piece coordinates that are contained either in the
! the file header or in a separate piece list file.
!
! The file names are specified exclusively as command line arguments:
! first the image file name, then the piece list file name, if any.
! If there is one argument, the program attempts to read the
! coordinates from the image file header.
!
! David Mastronarde, 1/2/00
!
! $Id$
!
program montagesize
  implicit none
  real*4, allocatable :: array(:)
  integer*4 nxyz(3), mxyz(3)
  integer*4, allocatable :: ixPcList(:), iyPcList(:), izPcList(:)
  character*320 imageFile, pieceFile
  integer*4 modeIn, numExtraBytes, numSecBytes, iflags, numPcList, minZpc, maxZpc, i
  integer*4 nxTotPix, minXpiece, nxPieces, nxOverlap, minYpiece, nyPieces, numSections
  integer*4 nyOverlap, nyTotPix, maxExtra, maxPiece, ierr
  real*4 dmin, dmax, dmean
  !
  maxPiece = 1000000
  if (iargc() < 1 .or. iargc() > 2) then
    print *,'Usage: montagesize image_file piece_list_file'
    print *,'   (piece_list_file is optional if image_file'// &
        ' contains piece coordinates)'
    call exit(0)
  endif
  call setExitPrefix('ERROR: MONTAGESIZE - ')
  call getarg(1, imageFile)
  call ialprt(.false.)
  call imopen(1, imageFile, 'ro')
  call irdhdr(1, nxyz, mxyz, modeIn, dmin, dmax, dmean)
  maxPiece = max(maxPiece, 2 * nxyz(3))
  allocate(ixPcList(maxPiece), iyPcList(maxPiece), izPcList(maxPiece), &
      stat = ierr)
  call memoryError(ierr, 'ARRAYS FOR PIECE COORDINATES')
  if (iargc() == 1) then
    call irtnbsym(1, numExtraBytes)
    maxExtra = numExtraBytes + 1000
    allocate(array(maxExtra / 4), stat = ierr)
    call memoryError(ierr, 'ARRAY FOR EXTRA HEADER DATA')
    call irtsym(1, numExtraBytes, array)
    call irtsymtyp(1, numSecBytes, iflags)
    call get_extra_header_pieces (array, numExtraBytes, numSecBytes, iflags, nxyz(3) &
        , ixPcList, iyPcList, izPcList, numPcList, maxPiece)
    if (numPcList == 0) call exitError( &
        'No piece list information in this image file')
  else
    call getarg(2, pieceFile)
    call read_piece_list(pieceFile, ixPcList, iyPcList, izPcList, &
        numPcList)
    if (numPcList == 0) call exitError( &
        'No piece list information in the piece list file')
  endif
  !
  ! find min and max Z
  !
  minZpc = 1000000
  maxZpc = -minZpc
  do i = 1, min(nxyz(3), numPcList)
    minZpc = min(minZpc, izPcList(i))
    maxZpc = max(maxZpc, izPcList(i))
  enddo
  numSections = maxZpc + 1 - minZpc
  !
  ! now check lists and get basic properties of overlap etc
  !
  call checkList(ixPcList, numPcList, 1, nxyz(1), minXpiece, nxPieces, &
      nxOverlap)
  call checkList(iyPcList, numPcList, 1, nxyz(2), minYpiece, nyPieces, &
      nyOverlap)
  if (nxPieces <= 0 .or. nyPieces <= 0) call exitError('Piece list information not good')
  !
  nxTotPix = nxPieces * (nxyz(1) - nxOverlap) + nxOverlap
  nyTotPix = nyPieces * (nxyz(2) - nyOverlap) + nyOverlap
  write(*,'(a,3i10)') ' Total NX, NY, NZ:', nxTotPix, nyTotPix, numSections
  if (iargc() > 1) then
    if (nxyz(3) < numPcList) then
      write(*, '(/,a)')'ERROR: MONTAGESIZE - The Z size of the image file is smaller '// &
          'than the size of the piece list'
      call exit(2)
    endif
    if (nxyz(3) > numPcList) then
      write(*, '(/,a)')'ERROR: MONTAGESIZE - The Z size of the image file is larger '// &
          'than the size of the piece list'
      call exit(3)
    endif
  endif
  call exit(0)
end program montagesize


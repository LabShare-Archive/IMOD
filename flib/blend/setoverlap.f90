!
! $Id$
!
! SETOVERLAP, given the minimum total # of pixels MINTOTPIX and the
! minimum overlap MINOVERLAP, and the frame dimension NFRAME, and the
! amount by which frame size can be reduced, IDELFRAME, finds
! the minimum # of pieces needed, NUMPIECES, the overlap NOVERLAP that
! minimizes the number of extra pixels, and total # of pixels NUMTOTPIX
! Note that if it can be done with a single piece it will return
! NOVERLAP=MINOVERLAP.
!
! If noFFTsizes is false it will ensure no prime factor greater than 19 so that
! filtering and FFT's can be done, regardless of whether FFTW is being used, because
! goodframe assumes 19 as well.  If it is true, frame sizes will just be even.
!
subroutine setOverlap(minTotPix, minOverlap, noFFTsizes, nFrame, idelFrame, numPieces, &
    nOverlap, numTotPix)
  implicit none 
  integer*4 minTotPix, minOverlap, nFrame, idelFrame, numPieces, nOverlap, numTotPix
  logical noFFTsizes
  integer*4 minOver, nonOverlap, newFrame, lapOver, ntotTmp
  integer*4 niceFrame
  !
  ! make sure overlap is even
  minOver = 2 * ((minOverlap + 1) / 2)
  ! round frame size down if necessary to have no larger prime factors
  if (noFFTsizes) then
    nFrame = nFrame - mod(nFrame, 2)
  else
    nFrame = niceFrame(nFrame, -idelFrame, 19)
  endif
  nonOverlap = nFrame - minOver
  !
  ! here is the minimum number of pieces needed
  numPieces = ((minTotPix - minOver) + (nonOverlap - 1)) / nonOverlap
  !
  ! if only one piece, round up frame size to fit # of pixels
  !
  if (numPieces == 1) then
    nFrame = idelFrame * ((minTotPix + idelFrame - 1) / idelFrame)
    if (noFFTsizes) then
      nFrame = nFrame + mod(nFrame, 2)
    else
      nFrame = niceFrame(nFrame, idelFrame, 19)     !round UP if necessary
    endif
    numTotPix = nFrame
    nOverlap = minOver
    return
  endif
  !
  ! otherwise find smallest frame size that keeps this number of pieces
  ! and has no big prime factors
  !
  do while (.true.)
    if (noFFTsizes) then
      newFrame = nFrame - idelFrame
      newFrame = newFrame - mod(newFrame, 2)
    else
      newFrame = niceFrame(nFrame - idelFrame, -idelFrame, 19)
    endif
    nonOverlap = newFrame - minOver
    if (((minTotPix - minOver) + (nonOverlap - 1)) / nonOverlap == numPieces) then
      nFrame = newFrame
    else
      exit
    endif
  enddo
  !
  ! find overlap that gives this number of pieces and requires fewest
  ! extra pixels
  !
  numTotPix = 2 * minTotPix
  lapOver = minOver
  do while(((minTotPix - lapOver) + (nFrame - lapOver - 1)) / &
      (nFrame - lapOver) == numPieces)
    !
    ! total # of pixels required with this overlap
    ntotTmp = numPieces * (nFrame - lapOver) + lapOver
    if (ntotTmp < numTotPix) then
      numTotPix = ntotTmp
      nOverlap = lapOver
    endif
    lapOver = lapOver + 2                       !keep overlap even
  enddo
  return
end subroutine setOverlap

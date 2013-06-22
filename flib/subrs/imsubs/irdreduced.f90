! IRDREDUCED: read in image reduced in size with anitalias filter
!
! $Id$
!
!!
! Reads a specified portion of a section while shrinking the input by an arbitrary
! amount with antialias reduction.  Arguments are:  ^
! [imUnit] - image file unit number  ^
! [iz]     - section #, numbered from 0  ^
! [array]  - array in which to return image  ^
! [nxDim]  - X dimension of array (set to [nxRed] for a packed, one-D array)  ^
! [xUBstart] - starting unbinned X coordinate, 0 is at left edge of first pixel  ^
! [yUBstart] - starting unbinned Y coordinate, 0 is at bottom edge of first pixel  ^
! [redFac]   - factor to reduce by, must by > 1  ^
! [nxRed]    - reduced size of image in X  ^
! [nyRin]    - reduced size of image in Y  ^
! [ifiltType] - Type of filter (0-5), as defined by selectZoomFilter  ^
! [temp]      - temporary real array for reading unbinned data into  ^
! [lenTemp]   - Size of [temp] array  ^
! [ierr]      - return value, 0 if succeeded, -1 on read error, 1 if filter type is out
! of range, 2 for zoom out of range, 3 for insufficient temporary array space, 4 for
! starting or ending unbinned coordinates out of range, or 5 for memory allocation error ^
!
! The temporary array needs to be big enough to fit enough input to compose 10 lines of
! output, but data copies will be reduced by larger arrays.
!!
subroutine irdReduced(imUnit, iz, array, nxDim, xUBstart, yUBstart, redFac, nxRed, &
    nyRed, ifiltType, temp, lenTemp, ierr)
  use imsubs
  implicit none
  integer*4 imUnit, nxDim, ix0, ix1, iy0, iy1, nx, ny, ifiltType
  integer*4 lenTemp, nxRed, nyRed, ierr, iz
  real*4 xUBstart, yUBstart, redFac, chunkYstart, zoomFac
  real*4 array(nxDim, *), temp(lenTemp)
  integer*4 nxLoad, nyLoad, maxLineLoad, loadYstart, ifiltWidth, ihalfWidth, ix
  integer*4 iyStart, maxChunkLines, lastY1, lastY0, indStart, numCopy, iyEnd
  integer*4 selectZoomFilter, zoomWithFilter
  !
  ix = lstream(imUnit)
  nx = ncrs(1, ix)
  ny = ncrs(2, ix)
  !
  ! Check the validity of the limits here
  zoomFac = 1. / redFac
  ierr = 4
  if (xUBstart < 0 .or. int(xUBstart + redFac * nxRed) > nx .or. &
      yUBstart < 0 .or. int(yUBstart + redFac * nyRed) > ny) return
  !
  ! Set up the filter and get its width
  ierr = selectZoomFilter(ifiltType, zoomFac, ifiltWidth)
  if (ierr .ne. 0) return

  ! get the limits of the load in X and the maximum # of lines that can be done in one
  ! chunk; insist on being able to do 10 lines per load
  ihalfWidth = (ifiltWidth + 3) / 2
  ix0 = max(0, floor(xUBstart - ihalfWidth))
  ix1 = min(nx - 1, ceiling(xUBstart + redFac * nxRed + ihalfWidth))
  nxLoad = ix1 + 1 - ix0
  maxLineLoad = lenTemp / nxLoad
  maxChunkLines = zoomFac * (maxLineLoad - 2 * ihalfWidth)
  ierr = 3
  if (maxChunkLines < 10) return
  ! print *,lenTemp, maxLineLoad, maxChunkLines, nyRed/maxChunkLines
  !
  ! Loop on chunks
  iyStart = 0
  lastY1 = -1
  do while (iyStart < nyRed)
    
    ! get limits of loaded data needed for next chunk, if the arithmetic is a bit off,
    ! reduce the end of the chunk to get within limits
    iyEnd = min(nyRed, iyStart + maxChunkLines)
    iy0 = max(0, floor(yUBstart + redFac * iyStart - ihalfWidth))
    iy1 = min(ny - 1, ceiling(yUBstart + redFac * iyEnd + ihalfWidth))
    do while (iy1 >= iy0 + maxLineLoad)
      iyEnd = iyEnd - 1
      iy1 = min(ny - 1, ceiling(yUBstart + redFac * iyEnd + ihalfWidth))
    enddo
    indStart = 1
    loadYstart = iy0

    ! if there is overlap, shift the data down and adjust the starting line and index
    if (iy0 <= lastY1) then
      indStart = (iy0 - lastY0) * nxLoad
      numCopy = (lastY1 + 1 - iy0) * nxLoad
      do ix = 1, numCopy
        temp(ix) = temp(ix + indStart)
      enddo
      loadYstart = lastY1 + 1
      indStart = numCopy + 1
    endif
    lastY0 = iy0
    lastY1 = iy1
    call imposn(imUnit, iz, 0)
    ierr = -1
    call irdpas(imUnit, temp(indStart), nxLoad, iy1 + 1 - loadYstart, ix0, ix1, &
        loadYstart, iy1, *99)

    chunkYstart = yUBstart + iyStart * redFac - iy0
    ierr = zoomWithFilter(temp, nxLoad, iy1 + 1 - iy0, xUBstart - ix0, chunkYstart,  &
        nxRed, iyEnd - iyStart, nxDim, 0, array(1, iyStart + 1))
    if (ierr .ne. 0) return
    iyStart = iyEnd
  enddo
  ierr = 0
99 return
end subroutine irdReduced

! IRDBINNED reads a specified portion of a section and can bin the
! image by an arbitrary amount.
!
! $Id$
!
!!
! Reads a specified portion of a section and can bin the image by an arbitrary amount.
! Arguments are:  ^
! [imUnit]    - image file unit number  ^
! [iz]        - section #, numbered from 0  ^
! [array]     - array in which to return image  ^
! [ixDim]     - X dimension of array (set to nxBin for packed, one-D array)  ^
! [iyDim]     - Y dimension of array  ^
! [ixUBstart] - starting unbinned X coordinate, numbered from 0.  ^
! [iyUBstart] - starting unbinned Y coordinate, numbered from 0.  ^
! [nbin]      - factor to bin by, or 1 for no binning  ^
! [nxBin]     - binned size of image in X  ^
! [nyBin]     - binned size of image in Y  ^
! [temp]      - temporary real array for reading unbinned data into  ^
! [lenTemp]   - size of TEMP array  ^
! [ierr]      - return value, 0 if succeeded or 1 if failed  ^
!
! Both [ixUBstart] and [iyUBstart] can be negative as long as they are bigger than
! -[nbin].  This allows one to offset the binned data so as to maintain
! centering.  With an offset, the first binned row or column is based
! on fewer than [nbin] rows or columns of unbinned data.  In addition,
! the last row or column can also be based on fewer than [nbin] rows or
! columns of unbinned data, but the calling routine is responsible
! for not setting [nxBin] or [nyBin] too high and requesting a completely
! empty binned column.  The temporary array may be as small as needed
! to fit one binned pixel, but access will be more efficient if it is
! at least as big as the binning times the unbinned X dimension.
!!
subroutine irdBinned(imUnit, iz, array, ixDim, iyDim, &
    ixUBstart, iyUBstart, nbin, nxBin, nyBin, temp, lenTemp, ierr)
  implicit none
  integer*4 imUnit, ixDim, iyDim, ix0, ix1, iy0, iy1, nbin, nx, ny
  integer*4 lenTemp, nxBin, ixoffset, nyBin, iyoffset, ierr, iz
  integer*4 ixUBstart, iyUBstart, nxyz(3), mxyz(3), nxyzst(3)
  real*4 array(ixDim, iyDim), temp(lenTemp)
  integer*4 ixb, nxLoad, nyLoad, maxLineLoad, maxColLoad, loadYoffset
  integer*4 iyStart, iyDone, nBinLines, loadXoffset, ixStart, ixDone
  integer*4 nBinCols, iyb, iFastStrt, iFastEnd, ixEnd, iyEnd, ix, iy
  integer*4 iCheckStrt, iCheckEnd, iCheck, nsum
  real*4 sum, binsq
  integer*4 iiuReadSection, iiuReadSecPart
  !
  call iiuRetSize(imUnit, nxyz, mxyz, nxyzst)
  nx = nxyz(1)
  ny = nxyz(2)
  ierr = 1
  !
  ! convert input starting coordinate to actual start and offset
  ! a negative start becomes an offset, and start then becomes 0
  !
  ix1 = min(nx, ixUBstart + nxBin * nbin) - 1
  ixOffset = min(0, ixUBstart)
  ix0 = max(0, ixUBstart)
  !
  iy1 = min(ny, iyUBstart + nyBin * nbin) - 1
  iyOffset = min(0, iyUBstart)
  iy0 = max(0, iyUBstart)
  !
  ! if binning 1, use regular routines
  !
  if (nbin == 1) then
    call iiuSetPosition(imUnit, iz, 0)
    if (ixDim == nx .and. nxBin == nx .and. nyBin == ny) then
      ierr = iiuReadSection(imUnit, array)
    else
      ierr = iiuReadSecPart(imUnit, array, ixDim, ix0, ix1, iy0, iy1)
    endif
    return
  endif
  !
  ! figure out how many columns and lines can be done at once in temp
  !
  if (lenTemp < nbin**2) then
    print *
    print *,'ERROR: IRDBINNED - BINNING TOO LARGE FOR TEMPORARY ARRAY'
    return
  endif
  binsq = nbin**2
  nxLoad = ix1 + 1 - ix0
  maxLineLoad = lenTemp / nxLoad
  maxColLoad = nxLoad
  if (maxLineLoad < nbin) then
    maxLineLoad = nbin
    maxColLoad = lenTemp / nbin
  endif

  loadYoffset = iyoffset
  iyStart = iy0
  iyDone = 0
  do while (iyStart <= iy1)
    !
    ! if loading is limited by max lines, then load to a bin boundary
    ! otherwise load the rest of the lines
    !
    if (maxLineLoad < iy1 + 1 - iyStart) then
      nyLoad = nbin * ((maxLineLoad - loadYoffset) / nbin) + loadYoffset
    else
      nyLoad = iy1 + 1 - iyStart
    endif
    !
    ! this will do a partially empty row at end if there is one
    !
    nBinLines = (nyLoad - loadYoffset + nbin - 1) / nbin
    loadXoffset = ixoffset
    ixStart = ix0
    ixDone = 0
    do while (ixStart <= ix1)
      !
      ! similarly, load X to a bin boundary or to the end of the line
      !
      if (maxColLoad < ix1 + 1 - ixStart) then
        nxLoad = nbin * ((maxColLoad - loadXoffset) / nbin) + loadXoffset
      else
        nxLoad = ix1 + 1 - ixStart
      endif
      nBinCols = (nxLoad - loadXoffset + nbin - 1) / nbin
      !
      ! read in data
      !
      ! print *,'loading', nxLoad, nyLoad, ixStart, &
      ! ixStart + nxLoad - 1, iyStart, iyStart + nyLoad - 1
      call iiuSetPosition(imUnit, iz, 0)
      ierr = iiuReadSecPart(imUnit, temp, nxLoad, ixStart, ixStart + nxLoad - 1, &
          iyStart, iyStart + nyLoad - 1)
      if (ierr .ne. 0) return
      do iyb = 1, nBinLines
        !
        ! find extent in X that can be done without checks
        !
        if ((iyb - 1) * nbin + loadYoffset < 0 .or. &
            iyb * nbin + loadYoffset > nyLoad) then
          iFastStrt = nBinCols + 1
          iFastEnd = nBinCols
        else
          iFastStrt = 1
          iFastEnd = nBinCols
          if (loadXoffset .ne. 0) iFastStrt = 2
          if (nBinCols * nbin + loadXoffset > nxLoad) &
              iFastEnd = nBinCols - 1
        endif
        !
        ! quick sums without checks
        !
        do ixb = iFastStrt, iFastEnd
          sum = 0.
          iyEnd = iyb * nbin + loadYoffset
          ixEnd = ixb * nbin + loadXoffset
          do iy = iyEnd + 1 - nbin, iyEnd
            do ix = ixEnd + 1 - nbin, ixEnd
              sum = sum + temp(ix + (iy - 1) * nxLoad)
            enddo
          enddo
          array(ixDone + ixb, iyDone + iyb) = sum / binsq
        enddo

        iCheckStrt = 1
        iCheckEnd = iFastStrt - 1
        do iCheck = 1, 2
          do ixb = iCheckStrt, iCheckEnd
            !
            ! sums with checks
            !
            sum = 0.
            nsum = 0
            iyEnd = iyb * nbin + loadYoffset
            ixEnd = ixb * nbin + loadXoffset
            do iy = iyEnd + 1 - nbin, iyEnd
              do ix = ixEnd + 1 - nbin, ixEnd
                if (ix >= 1 .and. ix <= nxLoad .and. &
                    iy >= 1 .and. iy <= nyLoad) then
                  sum = sum + temp(ix + (iy - 1) * nxLoad)
                  nsum = nsum + 1
                endif
              enddo
            enddo
            array(ixDone + ixb, iyDone + iyb) = sum / nsum
          enddo
          iCheckStrt = iFastEnd + 1
          iCheckEnd = nBinCols
        enddo
      enddo

      ixDone = ixDone + nBinCols
      ixStart = ixStart + nxLoad
      loadXoffset = 0
    enddo
    iyDone  = iyDone + nBinLines
    iyStart = iyStart + nyLoad
    loadYoffset = 0
  enddo
  return
end subroutine irdBinned

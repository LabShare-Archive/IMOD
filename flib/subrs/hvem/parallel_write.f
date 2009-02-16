c       parallel_write.f - routines for managing boundaries when writing
c       in parallel to a file.
c       
c       $Id$
c       $Log$
c       
c
c       !
c       Initializes the parallel writing routines for use from Fortran.  The
c       name of the boundary info file should be in [filename], which can be
c       empty.  An available unit for opening image files should be in
c       [iunit], and the dimensions of the image file in [nxin], [nyin], 
c       [nzin]. Returns the return value from the initialization routine
c       parWrtInitialize
c
      integer*4 function parWrtInitialize(filename, iunit, nxin, nyin, nzin)
      implicit none
      character*(*) filename
      integer*4 iunit, nxin, nyin, nzin, ierr, nfiles
      include 'parallel_write.inc'
      integer*4 parWrtInitializeFW, parWrtProperties

      iunBound = iunit
      nxpw = nxin
      nypw = nyin
      nzpw = nzin
      izcur = 0
      iycur = 0
      linesBound = 0
      ifopen = 0
      parWrtInitialize = 0
      if (filename .ne. ' ') then
        parWrtInitialize = parWrtInitializeFW(filename, nxpw, nypw)
        ierr = parWrtProperties(ifAllSec, linesBound, nfiles)
      endif
      return
      end

c       !
c       Positions unit [iunit] for writing at section [iz], line[iy], numbered
c       from 0.
c
      subroutine parWrtPosn(iunit, iz, iy)
      implicit none
      integer*4 iunit, iz, iy
      include 'parallel_write.inc'
c
      call imposn(iunit, iz, iy)
      izcur = iz
      iycur = iy
      return
      end

c       !
c       Writes an entire section from [array] into unit [iunit] at the current
c       position and writes boundary lines if appropriate
c
      subroutine parWrtSec(iunit, array)
      implicit none
      integer*4 iunit, ierr
      real*4 array(*)
      include 'parallel_write.inc'
c
      call iwrsec(iunit, array)
      if (linesBound .eq. 0) return
      call pwOpenIfNeeded(izcur, 0, nypw, ierr)
      if (ierr .ne. 0) then
        write(*,'(/,a,i5,a,i2)')'ERROR: parWrtSec - Finding parallel write'//
     &      ' boundary region sec',izcur,' err',ierr
        call exit(1)
      endif
      if (izcur .eq. izBound(1)) then
        call imposn(iunBound, 0, 0)
        call iwrsec(iunBound, array)
      endif
      if (izcur .eq. izBound(2)) then
        call imposn(iunBound, 1, 0)
        call iwrsec(iunBound, array(nxpw * (nypw - linesBound) + 1))
      endif
      izcur = izcur + 1
      iycur = 0
      return
      end

c       !
c       Writes one line from [array] into unit [iunit] at the current location
c       and writes it to a boundary file if appropriate
c
      subroutine parWrtLin(iunit, array)
      implicit none
      integer*4 iunit, ierr
      real*4 array(*)
      include 'parallel_write.inc'
c
      call iwrlin(iunit, array)
      if (linesBound .eq. 0) return
      if (ifopen .eq. 0) call pwOpenIfNeeded(izcur, iycur, 1, ierr)
      if (ierr .ne. 0) then
        write(*,'(/,a,i5,a,i5,a,i2)')'ERROR: parWrtLin - Finding parallel'//
     &      ' write boundary region at',izcur,',',iycur,' err',ierr
        call exit(1)
      endif
c     
c      
      if (ifAllSec .ne. 0) then
        if (iycur .lt. iyBound(1) + linesBound) then
          call imposn(iunBound, 2 * izcur, iycur - iyBound(1))
          call iwrlin(iunBound, array)
        endif
        if (iycur .ge. iyBound(2))then
          call imposn(iunBound, 2 * izcur + 1, iycur - iyBound(2))
          call iwrlin(iunBound, array)
        endif
      else
        if (izcur .eq. izBound(1) .and. iycur .lt. iyBound(1) + linesBound)then
          call imposn(iunBound, 0, iycur - iyBound(1))
          call iwrlin(iunBound, array)
        endif
        if (izcur .eq. izBound(2) .and. iycur .ge. iyBound(2))then
          call imposn(iunBound, 1, iycur - iyBound(2))
          call iwrlin(iunBound, array)
        endif
      endif
c
      iycur = iycur + 1
      if (iycur .ge. nypw) then
        iycur = 0
        izcur = izcur + 1
      endif
      return
      end


c       Internal routine to determine region and open file if needed, when
c       writing at section izsec, nlwrite lines starting at iyline
c
      subroutine pwOpenIfNeeded(izsec, iyline, nlwrite, ierr)
      implicit none
      integer*4 izsec, iyline, nlwrite, ierr, nxyz(3)
      character*320 filename
      include 'parallel_write.inc'
      real*4 title(20)
      integer*4 parWrtFindRegion

      if (ifopen .ne. 0) return
      ierr = parWrtFindRegion(izsec, iyline, nlwrite, filename, izBound,
     &    iyBound)
      if (ierr .ne. 0) return
      nxyz(1) = nxpw
      nxyz(2) = linesBound
      nxyz(3) = 2
      if (ifAllSec .ne. 0) nxyz(3) = 2 * nzpw
      call imopen(iunBound, filename, 'new')
      call icrhdr(iunBound, nxyz, nxyz, 2, title, 0)
      call ialsiz_sam_cel(iunBound, nxpw, linesBound, nxyz(3))
      call iwrhdr(iunBound, title, 0, -32000., 32000., 0.)
      ifopen = 1
      return
      end

c       parallel_write.f - routines for managing boundaries when writing
c       in parallel to a file.
c       
c       $Id$
c       $Log$
c       Revision 1.4  2010/06/23 15:23:48  mast
c       switched to module for parallel write vars
c
c       Revision 1.3  2009/08/04 22:41:55  mast
c       Fixed another problem with uninitialized ierr for line writing
c
c       Revision 1.2  2009/02/25 00:25:03  mast
c       Fixed initialization of ierr and title
c
c       Revision 1.1  2009/02/16 06:23:33  mast
c       Added to package
c       
c       Module for common variables
      module parWrtVars
      integer maxInfos
      parameter (maxInfos = 5)
      integer*4 izcur(maxInfos), iycur(maxInfos), nxpw(maxInfos), nypw(maxInfos)
      integer*4 nzpw(maxInfos), linesBound(maxInfos), iunBound(maxInfos), ifopen(maxInfos)
      integer*4 izBound(2,maxInfos), iyBound(2,maxInfos), ifAllSec(maxInfos)
      integer*4 infoCur /0/, numInfos /0/
      end module parWrtVars
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
      use parWrtVars
      implicit none
      character*(*) filename
      integer*4 iunit, nxin, nyin, nzin, ierr, nfiles
      integer*4 parWrtInitializeFW, parWrtProperties

      parWrtInitialize = 5
      if (numInfos .ge. maxInfos) return
      iunBound(numInfos+1) = iunit
      nxpw(numInfos+1) = nxin
      nypw(numInfos+1) = nyin
      nzpw(numInfos+1) = nzin
      izcur(numInfos+1) = 0
      iycur(numInfos+1) = 0
      linesBound(numInfos+1) = 0
      ifopen(numInfos+1) = 0
      parWrtInitialize = parWrtInitializeFW(filename, nxpw(numInfos+1),nypw(numInfos+1))
      if (parWrtInitialize .eq. 0) then
        numInfos = numInfos + 1
        infoCur = numInfos
        if (filename .ne. ' ') ierr = parWrtProperties(ifAllSec(infoCur),
     &      linesBound(infoCur), nfiles)
      endif
      return
      end

C       !
c       Sets the index of the current info file, numbered from 1
c       
      integer*4 function parWrtSetCurrent(index)
      use parWrtVars
      implicit none
      integer*4 index, parWrtSetCurrentFW
      parWrtSetCurrent = parWrtSetCurrentFW(index)
      if (parWrtSetCurrent .eq. 0) infoCur = index
      return
      end

c       !
c       Positions unit [iunit] for writing at section [iz], line[iy], numbered
c       from 0.
c
      subroutine parWrtPosn(iunit, iz, iy)
      use parWrtVars
      implicit none
      integer*4 iunit, iz, iy
c       
      call imposn(iunit, iz, iy)
      if (numInfos .le. 0 .or. infoCur .le. 0) return
      izcur(infoCur) = iz
      iycur(infoCur) = iy
      return
      end

c       !
c       Writes an entire section from [array] into unit [iunit] at the current
c       position and writes boundary lines if appropriate
c
      subroutine parWrtSec(iunit, array)
      use parWrtVars
      implicit none
      integer*4 iunit, ierr
      real*4 array(*)
c
      call iwrsec(iunit, array)
      if (numInfos .le. 0 .or. infoCur .le. 0) return
      if (linesBound(infoCur) .eq. 0) return
      call pwOpenIfNeeded(izcur(infoCur), 0, nypw(infoCur), ierr)
      if (ierr .ne. 0) then
        write(*,'(/,a,i5,a,i2)')'ERROR: parWrtSec - Finding parallel write'//
     &      ' boundary region sec',izcur(infoCur),' err',ierr
        call exit(1)
      endif
      if (izcur(infoCur) .eq. izBound(1, infoCur)) then
        call imposn(iunBound(infoCur), 0, 0)
        call iwrsec(iunBound(infoCur), array)
      endif
      if (izcur(infoCur) .eq. izBound(2, infoCur)) then
        call imposn(iunBound(infoCur), 1, 0)
        call iwrsec(iunBound(infoCur), array(nxpw(infoCur) * (nypw(infoCur) -
     &      linesBound(infoCur)) + 1))
      endif
      izcur(infoCur) = izcur(infoCur) + 1
      iycur(infoCur) = 0
      return
      end

c       !
c       Writes one line from [array] into unit [iunit] at the current location
c       and writes it to a boundary file if appropriate
c
      subroutine parWrtLin(iunit, array)
      use parWrtVars
      implicit none
      integer*4 iunit, ierr;
      real*4 array(*)
c
      call iwrlin(iunit, array)
      if (numInfos .le. 0 .or. infoCur .le. 0) return
      if (linesBound(infoCur) .eq. 0) return
      if (ifopen(infoCur) .eq. 0) then
        call pwOpenIfNeeded(izcur(infoCur), iycur(infoCur), 1, ierr)
        if (ierr .ne. 0) then
          write(*,'(/,a,i5,a,i5,a,i2)')'ERROR: parWrtLin - Finding parallel'//
     &        ' write boundary region at',izcur(infoCur),',',iycur(infoCur),' err',ierr
          call exit(1)
        endif
      endif
c     
c       The find region return modified non-boundary numbers for Y chunks
c       so that these simple tests work with them too.
      if (ifAllSec(infoCur) .ne. 0) then
        if (iycur(infoCur) .lt. iyBound(infoCur, 1) + linesBound(infoCur)) then
          call imposn(iunBound(infoCur), 2 * izcur(infoCur),
     &        iycur(infoCur) - iyBound(infoCur, 1))
          call iwrlin(iunBound(infoCur), array)
        endif
        if (iycur(infoCur) .ge. iyBound(2, infoCur))then
          call imposn(iunBound(infoCur), 2 * izcur(infoCur) + 1,
     &        iycur(infoCur) - iyBound(2, infoCur))
          call iwrlin(iunBound(infoCur), array)
        endif
      else
        if (izcur(infoCur) .eq. izBound(1, infoCur) .and.
     &      iycur(infoCur) .lt. iyBound(1, infoCur) + linesBound(infoCur)) then
          call imposn(iunBound(infoCur), 0, iycur(infoCur) - iyBound(1, infoCur))
          call iwrlin(iunBound(infoCur), array)
        endif
        if (izcur(infoCur) .eq. izBound(2, infoCur) .and.
     &      iycur(infoCur) .ge. iyBound(2, infoCur))then
          call imposn(iunBound(infoCur), 1, iycur(infoCur) - iyBound(2, infoCur))
          call iwrlin(iunBound(infoCur), array)
        endif
      endif
c
      iycur(infoCur) = iycur(infoCur) + 1
      if (iycur(infoCur) .ge. nypw(infoCur)) then
        iycur(infoCur) = 0
        izcur(infoCur) = izcur(infoCur) + 1
      endif
      return
      end


c       Internal routine to determine region and open file if needed, when
c       writing at section izsec, nlwrite lines starting at iyline
c
      subroutine pwOpenIfNeeded(izsec, iyline, nlwrite, ierr)
      use parWrtVars
      implicit none
      integer*4 izsec, iyline, nlwrite, ierr, kti, nxyz(3)
      character*320 filename
      real*4 title(20)
      character*80 titlech /'parallel_write: boundary lines'/
      integer*4 parWrtFindRegion

      ierr = 0
      if (numInfos .le. 0 .or. infoCur .le. 0) return
      if (ifopen(infoCur) .ne. 0 .or. linesBound(infoCur) .eq. 0) return
      ierr = parWrtFindRegion(izsec, iyline, nlwrite, filename, izBound(1, infoCur),
     &    iyBound(1, infoCur))
      if (ierr .ne. 0) return
      nxyz(1) = nxpw(infoCur)
      nxyz(2) = linesBound(infoCur)
      nxyz(3) = 2
      if (ifAllSec(infoCur) .ne. 0) nxyz(3) = 2 * nzpw(infoCur)
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      call imopen(iunBound(infoCur), filename, 'new')
      call icrhdr(iunBound(infoCur), nxyz, nxyz, 2, title, 0)
      call ialsiz_sam_cel(iunBound(infoCur), nxpw(infoCur), linesBound(infoCur), nxyz(3))
      call iwrhdr(iunBound(infoCur), title, 0, -32000., 32000., 0.)
      ifopen(infoCur) = 1
      return
      end

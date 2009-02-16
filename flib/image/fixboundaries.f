c       FIXBOUNDARIES
c       
c       Rewrites data near chunk boundaries after direct writing in parallel
c       to an output file. Takes two arguments, the name of the main image
c       file and the name of the boundary info file.  Uses the entries in the
c       info file to rewrite all of the data in the boundary files into the
c       main file.
c
c       $Id$
c       
c       $Log$
c
      implicit none
      integer idim
      parameter (idim = 1000000)
      real*4 array(idim)
      character*320 mainfile, infofile
      integer*4 nxyz(3), mxyz(3), nxyz2(3), mode, i, j, ifile, iz, ierr
      integer*4 izSecs(2), lineStart(2), nfiles, ifAllSec, linesGuard
      real*4 dmin, dmax, dmean
      integer*4 parWrtInitialize,parWrtGetRegion,parWrtProperties
c
      call getinout(2, mainfile, infofile)
      call setExitPrefix('ERROR: fixboundaries - ')
      call imopen(1, mainfile, 'old')
      call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
      ierr = parWrtInitialize(infofile, 2, nxyz(1), nxyz(2), nxyz(3))
      if (ierr .ne. 0) call exitError(
     &    'Initializing from the parallel write information file')
      ierr = parWrtProperties(ifAllSec, linesGuard, nfiles)
      if (ierr .ne. 0) call exitError(
     &    'Getting properties from the parallel write information file')
c       
c       Loop on all the files
      do ifile = 1, nfiles
        if (parWrtGetRegion(ifile, infofile, izSecs, lineStart) .ne. 0)
     &      call exitError('Getting information for one region')
        call imopen(2, infofile, 'ro')
        call irdhdr(2, nxyz2, mxyz, mode, dmin, dmax, dmean)
        if (ifAllSec .ne. 0) then
c           
c           If writing all sections, loop on all sections
          do iz = 1, nxyz(3)
            do j = 1, 2
              if (lineStart(j) .ge. 0) then
                call imposn(2, iz * 2 + j - 3, 0)
                call imposn(1, iz - 1, lineStart(j))
                do i = 1, linesGuard
                  call irdlin(2, array, *99)
                  call iwrlin(1, array)
                enddo
              endif
            enddo
          enddo
        else
c           
c           Otherwise just write the two sections
          do j = 1, 2
            if (izSecs(j) .ge. 0) then
              call imposn(2, j-1, 0)
              call imposn(1, izSecs(j), lineStart(j))
              do i = 1, linesGuard
                call irdlin(2, array, *99)
                call iwrlin(1, array)
              enddo
            endif
          enddo
        endif
        call imclose(2)
      enddo
      call imclose(1)
      call exit(0)
99    call exitError('Reading from guard file')
      end



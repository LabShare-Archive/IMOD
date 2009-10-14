*************EXTRACTPIECES.FOR**********************************************
c       
c       EXTRACTPIECES will extract piece coordinates from the header of
c       an image file, if they are present, and produce a file with those
c       coordinates (a piece list file).
c       
c       The Unix version will take the file names either on the command line
c       or as entries to the program.  If there are no file names on the
c       command line, the program asks for both the input and output file
c       names.  If there is one file name on the command line, the program
c       takes that as the input file and asks for the output file name.
c       
c       Entries to program:
c       
c       Image file
c       Output file for piece coordinates
c       
c       David Mastronarde, 1/2/00
c       
      implicit none
      integer*4 NXYZ(3),MXYZ(3), nz, maxpiece, maxextra, nbsym,nbyte,iflags
      real*4, allocatable :: array(:)
      integer*4, allocatable :: ixpiece(:),iypiece(:),izpiece(:)
      integer*4 MODE,npiece,i, ierr
      real*4 DMIN,DMAX,DMEAN
C       
      CHARACTER*320 FILIN,filout
C       
      EQUIVALENCE (Nz,NXYZ(3))
C       
      call getinout(2,filin,filout)
c       
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      call irtnbsym(1,nbsym)
      maxextra = nbsym + 1024
      maxpiece = nz + 1024
      allocate(array(maxextra/4), ixpiece(maxpiece), iypiece(maxpiece),
     &    izpiece(maxpiece), stat = ierr)
      if(ierr .ne. 0) then
        print *
        print *,'ERROR: EXTRACTPIECES - ALLOCATING ARRAYS FOR '//
     &      'EXTRA HEADER DATA'
        call exit(1)
      endif
      call irtsym(1,nbsym,array)
      call irtsymtyp(1,nbyte,iflags)
      call get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &    ixpiece,iypiece,izpiece,npiece,maxpiece)
      if(npiece.eq.0) then
        print *,'No piece list information in this image file'
      else
        call dopen(1,filout,'new','f')
        write(1,'(2i7,i5)')(ixpiece(i),iypiece(i),izpiece(i),i=1,npiece)
        close(1)
        print *,npiece,' piece coordinates output to file'
      endif

      CALL IMCLOSE(1)
c       
      call exit(0)
      END


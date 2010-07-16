*       * * * * * * MONTAGESIZE * * * * * *
c       
c       MONTAGESIZE will determine the X, Y, and Z dimensions of a montaged
c       image file, from piece coordinates that are contained either in the
c       the file header or in a separate piece list file.
c       
c       The file names are specified exclusively as command line arguments:
c       first the image file name, then the piece list file name, if any.
c       If there is one argument, the program attempts to read the
c       coordinates from the image file header.
c       
c       David Mastronarde, 1/2/00
c
c       $Id$
c       
c       $Log$
c       Revision 3.2  2008/11/15 01:21:35  mast
c       Standardized error messages and increased array sizes
c
c       
      implicit none
      real*4, allocatable :: array(:)
      integer*4 nxyz(3),mxyz(3)
      integer*4, allocatable :: ixpclist(:), iypclist(:), izpclist(:)
      character*320 imfile,plfile
      integer*4 modein, nbsym, nbyte,iflags,npclist,minzpc,maxzpc,i,nsect
      integer*4 nxtotpix, minxpiece,nxpieces,nxoverlap,minypiece,nypieces
      integer*4 nyoverlap,nytotpix,maxextra,maxpiece,ierr
      real*4 dmin,dmax,dmean
c
      maxpiece=1000000
      if(iargc().lt.1.or.iargc().gt.2)then
        print *,'Usage: montagesize image_file piece_list_file'
        print *,'   (piece_list_file is optional if image_file'//
     &      ' contains piece coordinates)'
        call exit(0)
      endif
      call setExitPrefix('ERROR: MONTAGESIZE - ')
      call getarg(1,imfile)
      call ialprt(.false.)
      call imopen(1,imfile,'ro')
      call irdhdr(1,nxyz,mxyz,modein,dmin,dmax,dmean)
      maxpiece = max(maxpiece, 2 * nxyz(3))
      allocate(ixpclist(maxpiece),iypclist(maxpiece), izpclist(maxpiece),
     &    stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR PIECE COORDINATES')
      if (iargc().eq.1) then
        call irtnbsym(1,nbsym)
        maxextra = nbsym + 1000
        allocate(array(maxextra/4), stat = ierr)
        call memoryError(ierr, 'ARRAY FOR EXTRA HEADER DATA')
        call irtsym(1,nbsym,array)
        call irtsymtyp(1,nbyte,iflags)
        call get_extra_header_pieces (array,nbsym,nbyte,iflags,nxyz(3)
     &      ,ixpclist,iypclist,izpclist,npclist,maxpiece)
        if(npclist.eq.0) call exitError(
     &      'No piece list information in this image file')
      else
        call getarg(2,plfile)
        call read_piece_list(plfile,ixpclist,iypclist,izpclist,
     &	    npclist)
        if (npclist.eq.0) call exitError(
     &      'No piece list information in the piece list file')
      endif
c	
c	find min and max Z
c       
      minzpc = 1000000
      maxzpc=-minzpc
      do i=1,npclist
        minzpc=min(minzpc,izpclist(i))
        maxzpc=max(maxzpc,izpclist(i))
      enddo
      nsect=maxzpc+1-minzpc
c       
c       now check lists and get basic properties of overlap etc
c       
      call checklist(ixpclist,npclist,1,nxyz(1),minxpiece,nxpieces,
     &    nxoverlap)
      call checklist(iypclist,npclist,1,nxyz(2),minypiece,nypieces,
     &    nyoverlap)
      if(nxpieces.le.0. or. nypieces.le.0) call exitError(
     &    'Piece list information not good')
c       
      nxtotpix=nxpieces*(nxyz(1)-nxoverlap)+nxoverlap
      nytotpix=nypieces*(nxyz(2)-nyoverlap)+nyoverlap
      write(*,'(a,3i10)')' Total NX, NY, NZ:',nxtotpix,nytotpix,nsect
      call exit(0)
      end


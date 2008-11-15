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
c       
      parameter (maxextra = 10000000, maxpiece=500000)
      real*4 array(maxextra/4)
      integer*4 nxyz(3),mxyz(3)
      integer*4 ixpclist(maxpiece),iypclist(maxpiece)
      integer*4 izpclist(maxpiece)
      character*320 imfile,plfile
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
      if (iargc().eq.1) then
        call irtnbsym(1,nbsym)
        if(nbsym.gt.maxextra)
     &      call exitError('ARRAYS NOT LARGE ENOUGH FOR EXTRA HEADER DATA')
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
      write(*,'(a,3i6)')' Total NX, NY, NZ:',nxtotpix,nytotpix,nsect
      call exit(0)
      end


*************EXTRACTTILTS.FOR**********************************************
c
c	  EXTRACTTILTS will extract tilt angles from the header of an image
c	  file, if they are present, and produce a file with a list of the
c	  angles.
c
c	  The Unix version will take the file names either on the command line
c	  or as entries to the program.  If there are no file names on the
c	  command line, the program asks for both the input and output file
c	  names.  If there is one file name on the command line, the program
c	  takes that as the input file and asks for the output file name.
c	  
c	  Entries to program:
c	  
c	  Image file with tilt information
c	  Output file for tilt angles
c	  
c	  David Mastronarde, 1/2/00
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	parameter (maxextra = 1000000, maxtilts = 1000, maxpiece=50000)
	DIMENSION NXYZ(3),MXYZ(3)
	real*4 tilt(maxtilts)
	real*4 array(maxextra/4)
	integer*4 ixpiece(maxpiece),iypiece(maxpiece),izpiece(maxpiece)
C
	CHARACTER*120 FILIN,filout
C	  
	EQUIVALENCE (Nz,NXYZ(3))
C
	call getinout(2,filin,filout)
c
	CALL IMOPEN(1,FILIN,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	call irtnbsym(1,nbsym)
	if(nbsym.gt.maxextra) then
	  print *
	  print *,'ERROR: EXTRACTTILTS - ARRAYS NOT LARGE ENOUGH ',
     &	    'FOR EXTRA HEADER DATA'
	  call exit(1)
	endif
	call irtsym(1,nbsym,array)
	call irtsymtyp(1,nbyte,iflags)
	call get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &	    ixpiece,iypiece,izpiece,npiece,maxpiece)
	if(npiece.eq.0)then
	  do i=1,nz
	    izpiece(i)=i-1
	  enddo
	  maxz=nz
	else
	  maxz=0
	  do i=1,npiece
	    maxz=max(maxz,izpiece(i))
	  enddo
	endif
c	  
c	  set up a marker value for empty slots
c
	do i=1,maxz
	  tilt(i)=-999.
	enddo
c
	call get_extra_header_tilts
     &	    (array,nbsym,nbyte,iflags,nz,tilt,ntilt,maxtilts,izpiece)
	if(ntilt.eq.0) then
	  print *,'No tilt information in this image file'
	else
c	    
c	    pack the tilts down
c
	  ntiltout=0
	  do i=1,ntilt
	    if(tilt(i).ne.-999.)then
	      ntiltout=ntiltout+1
	      tilt(ntiltout)=tilt(i)
	    endif
	  enddo
c
	  call dopen(1,filout,'new','f')
	  write(1,'(f7.2)')(tilt(i),i=1,ntiltout)
	  close(1)
	  print *,ntiltout,' tilt angles output to file'
	endif

	CALL IMCLOSE(1)
c
	call exit(0)
	END


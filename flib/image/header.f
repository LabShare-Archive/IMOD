*************HEADER.FOR**********************************************
*
*	A JIFFY TO READ THE HEADER ON AN IMAGE FILE
*
************************************************************************
c       Version for unix can take the file name either on the command line
c       or as an entry to the program.  If here is no file name on the command
c       line, the program asks for the file name.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
*
	
	parameter (maxextra = 1000000)
	real*4 array(maxextra/4)
	DIMENSION NXYZ(3),MXYZ(3)
	logical nbytes_and_flags
C
	CHARACTER*120 FILIN
	INTEGER*2 EXTRA(32)
C
	EQUIVALENCE (NX,NXYZ)
C
	call getinout(1,filin,filin)
c
	CALL IMOPEN(1,FILIN,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C	  
	call irtnbsym(1,nbsym)
	if(nbsym.gt.0.and.nbsym.le.maxextra) then
	  call irtsym(1,nbsym,array)
	  call irtsymtyp(1,nint,nreal)
	  if (.not. nbytes_and_flags(nint, nreal) .and. nreal .ge. 12) then
	    tiltaxis = array(nint + 11)
	    if (tiltaxis .ge. -360. .and. tiltaxis .le. 360.)
     &		write(*,101)tiltaxis
101	    format(10x,'Tilt axis rotation angle =', f6.1)
	    pixel = array(nint + 12) * 1.e9
	    if (pixel .gt. 0.01 .and. pixel .lt. 10000.) write(*,102)pixel
102	    format(10x,'Pixel size in nanometers =', g11.4) 
	  endif
	endif

	CALL IMCLOSE(1)
c
	call exit(0)
	END



*	  * * * * * * FINDCONTRAST * * * * * *
c	  
c	  FINDCONTRAST finds the black and white contrast settings that are
c	  used when converting an image file to bytes with newstack or mrcbyte.
c	  It computes a histogram of pixel values within a selected volume,
c	  which it uses to determine the contrast settings that would truncate
c	  the values of a specified, small number of pixels in the volume.
c	  
c	  The program is designed to be used with newly-created tomograms,
c	  so coordinates are entered as they would be observed in a flipped
c	  volume, with Y and Z transposed.
c	  
c	  The program reports the absolute minimum and maximum pixel values
c	  within the selected volume, the minimum and maximum values with
c	  truncation of the specified number of pixels, and the black and white
c	  contrast settings that would give this amount of truncation.
c
c	  
c	  Entries to the program:
c	  
c	  Name of image file
c	  
c	  First and last slice to include in the volume to be analyzed.  The
c	  first slice is numbered 1, so the numbers entered can be the 
c	  Imod section #'s in a flipped volume.
c	  
c	  Lower and upper X coordinates, and lower and upper Y coordinates,
c	  of the volume to analyze, as viewed in a flipped volume.  Enter /
c	  for the default, which omits a 10% border on each side.
c	  
c	  Maximum number of pixels to truncate (saturate) at black and at
c	  white within the analyzed volume.  Enter / for the default, which
c	  is one pixel per slice or one pixel per million, whichever is 
c	  greater.
c	  
c	  David Mastronarde, 1/1/00
c
	parameter (idim=10000*500)
	parameter (limden=100000)
	COMMON //NX,NY,NZ
C
	DIMENSION NXYZ(3),MXYZ(3)
	real array(idim)
	integer*4 ihist(-limden:limden)
	EQUIVALENCE (NX,NXYZ)
	character*120 filbig
C
C	Open image files.
C
	write(*,'(1x,a,$)')'Name of image file: '
	read(5,50)filbig
50	format(A)
c
	CALL IMOPEN(1,filbig,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	IF ((NX*NY.GT.idim)) GOTO 94
C
	if (mode.ne.1.and.(dmin.lt.-limden.or.dmax.gt.limden))
     &	    stop '- DATA VALUES HAVE TOO LARGE A RANGE FOR ARRAYS'
c
10	write(*,'(1x,a,/,a,$)')'First and last slice (Imod section #'//
     &	    ' in flipped volume)','  to include in analysis: '
	read(5,*)iylo,iyhi
	if (iylo.le.0.or.iyhi.gt.ny.or.iylo.gt.iyhi)then
	  print *,'Illegal values, try again'
	  go to 10
	endif
	iylo=iylo-1
	iyhi=iyhi-1
c	  
20	ixlo=nx/10
	ixhi=nx-1-ixlo
	izlo=nz/10
	izhi=nz-1-izlo
	write(*,'(1x,a,/,a,4i5,a,$)')'Lower & upper X, lower & upper'
     &	    //' Y (in flipped volume) to include in analysis',
     &	    '  (/ for ',ixlo,ixhi,izlo,izhi,'): '
	read(5,*)ixlo,ixhi,izlo,izhi
	if(ixlo.lt.0.or.ixhi.ge.nx.or.ixlo.ge.ixhi.or.
     &	    izlo.lt.0.or.izhi.ge.nz.or.izlo.ge.izhi)then
	  print *,'Illegal values, try again'
	  go to 20
	endif
c	  
	areafac=max(1.,nx*ny*1.e-6)
	ntrunclo=areafac*(iyhi+1-iylo)
	ntrunchi=ntrunclo
	write(*,'(1x,a,/,a,2i5,a,$)')'Maximum numbers of pixels to '//
     &	    'truncate at black and white in analyzed volume',
     &	    '  (/ for ',ntrunclo,ntrunchi,'): '
	read(5,*)ntrunclo,ntrunchi
c	  
	do i=-limden,limden
	  ihist(i)=0
	enddo
	nxt=ixhi+1-ixlo
	nyt=iyhi+1-iylo

	ivmin=limden
	ivmax=-limden
	do iz=izlo,izhi
	  call imposn(1,iz,0)
	  call irdpas(1,array,nxt,nyt,ixlo,ixhi,iylo,iyhi)
	  do i=1,nxt*nyt
	    ival=nint(array(i))
	    ihist(ival)=ihist(ival)+1
	    ivmin=min(ivmin,ival)
	    ivmax=max(ivmax,ival)
	  enddo
	enddo
c	  
	ntrunc=0
	ilo=ivmin
	do while(ntrunc.le.ntrunclo.and.ilo.lt.ivmax)
	  ntrunc=ntrunc+ihist(ilo)
	  ilo=ilo+1
	enddo
c
	ntrunc=0
	ihi=ivmax
	do while(ntrunc.le.ntrunchi.and.ihi.gt.ivmin)
	  ntrunc=ntrunc+ihist(ihi)
	  ihi=ihi-1
	enddo
c	write(*,'(i7,9i8)')(ihist(i),i=ivmin,ivmax)
	iclo=255*(ilo-dmin)/(dmax-dmin)
	ichi=255*(ihi-dmin)/(dmax-dmin)+0.99
	write(*,101)ivmin,ivmax,ilo,ihi,iclo,ichi
101	format('Min and max density levels in the analyzed volume are',
     &	    i7,' and',i7,/,
     &	    'Min and max density levels with truncation are',
     &	    i7,' and',i7,/,'Implied black and white contrast levels are'
     &	    ,i4,' and',i4)
	call exit(0)
	
94	WRITE (6,660)
660	FORMAT(' Input image too big .')
	STOP
	END

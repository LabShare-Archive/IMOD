C*FFTRANS.FOR************************************************************
C									*
C	  This program will do 2-dimensional FFT's in either		*
C	direction. The real-space origin is at (1,1) and		*
C	the origin of reciprocal space is at (1,NY/2+1).		*
C	The FT of an image NX,NY is NX/2+1,NY complex value.		*
C									*
C	  All transforms are done using Lynn ten Eyck's subroutines.	*
C	These allow arbitrary-sized images having a LARGEST PRIME	*
C	factor of 19!!.							*
C									*
C	Very large images (upto 8192 x 8192) are transformed using	*
C	the disk-based routine BIGFFT. The switch point is now 		*
C	determined by the available working set size (small images	*
C	are done in core)						*
C									*
C	NOTE:  3-D images are treated as image stacks.			*
C	  Each section is transformed independently!!			*
C	  HOWEVER, only single sections of LARGE images may be used!	*
C									*
C       The program will accept file names either from the command line *
C       or as entries to the program after it is started.  If there are *
C       two names on the command line, they will be taken as the input  *
C       and output file names; if there is one name, it will be taken   *
C       as the input file name and the program will ask for the output  *
C       file name; if there are no command line arguments, the program  *
C       will ask for both input and output file names.                  *
C									*
C	No input parametrs are required.				*
C									*
C	Should work with any input data mode but backward FFT will	*
C	be in real*4 mode (2) -DNM					*
c	  
c	DNM modified to do in "memory" rather than use giantfft as long as
c	the image will actually fit into the dimensioned array.  Also, changed
c	to properly package arrays in memory and use iwrsec to write sections;
c	this is much faster and saves lots of disk access.
c
C									*
C	Version  1.00	18.10.81	DAA		FOR VAX		*
C	Version  1.01	27.MAY.82	DAA		FOR VAX		*
C	Version  1.02	10.JUNE.82	DAA		FOR VAX		*
C	Version  1.03	23.JULY.82	DAA		FOR VAX		*
C	Version  1.04	01.OCTOBER.82	DAA		FOR VAX		*
C	Version  1.05	08.November.82	DAA		FOR VAX		*
C	Update   1.05	18.November.82	DAA		FOR VAX		*
C	Revision 1.06	27.November.84	RH		FOR VAX		*
C	Bitmodes 1.07	09.August.88	DNM		FOR uVAX	*
C	Bug fix  1.07	31.December.88	DNM		FOR uVAX	*
C       Ported to unix	07.December.94  DNM		FOR SGI		*
C									*
C************************************************************************
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2002/07/30 17:44:30  mast
c	  Fixed titles, increased buffer size, standardized error exit
c	
C
c	  DNM: set this parameter for true buffer size
	parameter (ibufreal=3100*3100)
	COMMON//NX,NY,NZ
	COMMON/FTBUF/MAXSIZ,ARRAY(ibufreal)
	DIMENSION TITLE(20),NXYZR(3),MXYZR(3),NXYZF(3),NXYZST(3)
	CHARACTER DAT*9,TIM*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech

	LOGICAL LARGE
	EQUIVALENCE (NX,NXYZR)
	DATA IFOR/0/,IBAK/1/,ZERO/0.0/, LARGE/.FALSE./, NXYZST/3*0/
	character*80 filin,filout
C
	WRITE(6,1000)
1000	FORMAT(//' FFTRANS: Fourier Transform Program  V1.07',//)
C
	call getinout(2,filin,filout)
	CALL IMOPEN(1,filin,'RO')
	CALL IMOPEN(2,filout,'NEW')
	CALL DATE(DAT)
	CALL TIME(TIM)
C
C   Read input header & decide which direction to go
C
	CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
	CALL ITRHDR(2,1)
c	CALL RETWKSIZE(ISIZE,IQUOTA,IEXTENT)
c         here is the value of quota reported by HVEM VAX running from
c         slow batch queue
c	iquota = 3524
c      		READ (TIM,20) IHR,IMIN,ISEC
c20		FORMAT (I2,1X,I2,1X,I2)
c      		WRITE (6,21) IHR,IMIN,ISEC
c21		FORMAT(' TIME',5X,I2,' HR ',I2,' MIN ',I2,' SEC')
c      		IF (IHR.GT.19.OR.IHR.LT.4) THEN
c      		     IQUOTA = IQUOTA + .5*(IEXTENT-IQUOTA)
c      		ENDIF
c	JQUOTA = MAX(IQUOTA,242+(NX+2)/16,242+NY/16,321)
c	MAXSIZ = (JQUOTA - 240)*128
c	IF (JQUOTA .GT. IQUOTA) 
c     .	PRINT *,' Quota too small, must use virtual memory!!'
c	PRINT *,' Quota= ',IQUOTA,'  Buffersize= ',MAXSIZ
c	  DNM 12/19/98: just set this to the true buffer size
	maxsiz=ibufreal
	NY2 = NY/2
	NY21 = NY2 + 1
	NZM1 = NZ - 1
	TMIN =  1.E10
	TMAX = -1.E10
	TMEAN = 0.0
	IF (NZ .GT. 1) WRITE(6,1200) NZ
1200	FORMAT(//' Each of the ',I4,' sections are SEPARATELY ',
     .	'transformed!!!!'//)
	IF (MODE .EQ. 3 .OR. MODE .EQ. 4) GOTO 50
C
C   Here for foward transform
C
	NXM1 = NX - 1
	NYM1 = NY -1
	NX2 = NX/2
	NX21 = NX2 + 1
	NXP2 = NX + 2
	NXYZF(1) = NX21
	NXYZF(2) = NY
	NXYZF(3) = NZ
c	  DNM: use the true buffer size here, it's much faster to do regular
	IF (NXP2*NY .GT. ibufreal) LARGE = .TRUE.
	IF (LARGE .AND. NZ .GT. 1) then
	  write(6,1202)
1202	  format(/,' ERROR: FFTRANS - ONLY SINGLE SECTIONS CAN BE DONE',
     &	      'FOR LARGE IMAGES')
	endif
	CALL IALMOD(2,4)
	CALL IALSIZ(2,NXYZF,NXYZST)
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,1500,TITLE) DAT,TIM
        write(titlech,1500) DAT,TIM
1500    FORMAT('FFTRANS: Forward Fourier Transform Calculated',12X,A9,
     .  2X,A8)
	read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
	CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C  Loop over all sections & write out with shifted origin
C
	DO 100 ISEC = 0,NZM1
	  IF (LARGE) THEN
	    CALL GIANTFFT(1,2,NX,NY,DMIN,DMAX,DMEAN,IFOR)
	  ELSE
	    CALL IRDPAS(1,ARRAY,NXP2,NY,0,NXM1,0,NYM1,*99)
	    CALL TODFFT(ARRAY,NX,NY,IFOR)
c	      DNM: a speed trick: swap the lines in memory and use iwrsec
	    call swapden(array,nx21,ny,dmin,dmean,dmax)
	    call iwrsec(2,array)
c	      DNM: add this IMPOSN call to make it work on stacks
c            CALL IMPOSN(2,ISEC,0)
c	    CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,NY2,NYM1)
c            CALL IMPOSN(2,ISEC,NY2)
c	    CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,0,NY2-1)
	    CALL ICLCDN(ARRAY,NX21,NY,1,NX21,1,NY,DMIN,DMAX,DMEAN)
	  END IF
	  IF (DMIN .LT. TMIN) TMIN = DMIN
	  IF (DMAX .GT. TMAX) TMAX = DMAX
	  TMEAN = TMEAN + DMEAN
	  IF (NZ .GT. 1) WRITE(6,1600) ISEC,DMIN,DMAX,DMEAN
100	CONTINUE
1600	FORMAT(' Min,Max,Mean for section ',I4,' are: ',3G13.5)
	GOTO 90
C
C   Here for inverse transform
C
50	NXR = (NX - 1)*2
	NXP2 = NXR + 2
	NXYZF(1) = NXR
	NXYZF(2) = NY
	NXYZF(3) = NZ
c	  DNM: use the true buffer size here, it's much faster to do regular
	IF (NXP2*NY .GT. ibufreal) LARGE = .TRUE.
	IF (LARGE .AND. NZ .GT. 1) then
	  write(6,1202)
	  call exit(1)
	endif
	INDEX = NXP2*NY2 + 1
	CALL IALMOD(2,2)
	CALL IALSIZ(2,NXYZF,NXYZST)
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,1700,TITLE) DAT,TIM
        write(titlech,1700) DAT,TIM
1700    FORMAT('FFTRANS: Inverse Fourier Transform Calculated',12X,A9,
     .  2X,A8)
	read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
	CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C  Loop over all sections , shift origin on reading in
C
	DO 300 ISEC = 0,NZM1
	  IF (LARGE) THEN
	    CALL GIANTFFT(1,2,NXR,NY,DMIN,DMAX,DMEAN,IBAK)
	  ELSE
	    DO 200 IY = 1,NY
	      IF (IY .EQ. NY21) INDEX = 1
	      CALL IRDLIN(1,ARRAY(INDEX),*99)
	      INDEX = INDEX + NXP2
200	    CONTINUE
	    CALL TODFFT(ARRAY,NXR,NY,IBAK)
	    CALL ICLDEN(ARRAY,NXP2,NY,1,NXR,1,NY,DMIN,DMAX,DMEAN)
c	      DNM: moved the ICLDEN up, switched to a repack and iwrsec
	    CALL irepak(ARRAY,ARRAY,NXP2,NY,0,NXR-1,0,NY-1)
	    call iwrsec(2,array)
c	    CALL IWRPAS(2,ARRAY,NXP2,NY,0,NXR-1,0,NY-1)
	  END IF
	  IF (DMIN .LT. TMIN) TMIN = DMIN
	  IF (DMAX .GT. TMAX) TMAX = DMAX
	  TMEAN = TMEAN + DMEAN
	  IF (NZ .GT. 1) WRITE(6,1600) ISEC,DMIN,DMAX,DMEAN
300	CONTINUE
C
90	TMEAN = TMEAN/NZ
	WRITE(6,1800) TMIN,TMAX,TMEAN
1800	FORMAT(/,' Overall Min,Max,Mean values are: ',3G13.5)
	CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
	CALL IMCLOSE(1)
	CALL IMCLOSE(2)
	CALL EXIT(0)
C
99	print *
	print *,'ERROR: FFTRANS - END-OF-FILE ERROR ON READ'
	call exit(1)
	END


c	  DNM: a subroutine to swap the lines of the array and to calculate
c	  min, max and mean densities
	subroutine swapden(array,nx,ny,dmin,dmax,dmean)
	complex*8 array(nx,ny),ctmp,ctmp2
c	  swap lines
	dmin=1.e20
	dmax=-dmin
	dsum=0.
	do iy=1,ny/2
	  iyhi=iy+ny/2
	  do ix=1,nx
	    ctmp=array(ix,iy)
	    dabs=cabs(ctmp)
	    if(dabs.gt.dmax)dmax=dabs
	    if(dabs.lt.dmin)dmin=dabs
	    dsum=dsum+dabs
	    ctmp2=array(ix,iyhi)
	    dabs=cabs(ctmp2)
	    if(dabs.gt.dmax)dmax=dabs
	    if(dabs.lt.dmin)dmin=dabs
	    dsum=dsum+dabs
	    array(ix,iy)=ctmp2
	    array(ix,iyhi)=ctmp
	  enddo
	enddo
	dmean=dsum/(nx*ny)
	return
	end


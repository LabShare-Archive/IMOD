C*ENHANCE.FOR************************************************************
C									*
C	  This is a two-dimensional gaussian bandpass filter		*
C	program for image enhancement purposes.				*
C	  For three-dimensional images, each image in the stack		*
C	is separately bandpass filtered.				*
C									*
C	  The filter is in the form of a gaussian highpass filter	*
C	given by:  (if Sigma1 is positive)				*
C	   (1. - exp(-r**2/(2*Sigma1**2)				*
C	multiplied by a gaussian-edged band-pass filter. This		*
C	filter is flat between Radius1 --> Radius2 and decays		*
C	symmetrically as a gaussian:					*
C	   exp(-(r-Radius)**2)/(2.*sigma2**2)				*
C									*
C	The units are in fractional reciprocal lattice units,		*
C	that is r goes from 0-->sqrt(2)   (0-->.5 on each axis)		*
C									*
C	If either Sigma = 0, then that part of the filter is removed!	*
C									*
C	If Sigma1 is negative, the first filter is second derivative	*
C	of a gaussian, Del-squared G, with formula			*
C	   r**2*exp(-r**2/(2.*Sigma1**2))				*
C	This filter alone is bandpass with a peak at 1.414*|Sigma1|, so	*
C	Sigma2 and the Radii can be set to zero				*
C									*
C	If Sigma2 is negative, the second filter is inverted (1 minus	*
C	the Gaussian band-pass filter).  This filter is then multiplied	*
C	by the filter specified by sigma1 (if any).			*
C									*
C	Several modes of operation are possible:			*
C									*
C	Gaussian low-pass filter (temperature factor)			*
C		:  Sigma1 & Radii = 0, use Sigma2			*
C									*
C	Gaussian bandpass centered at Radius				*
C		:  Sigma1=0, 		use Radius1=Radius2 & Sigma2	*
C									*
C	Gaussian-edged badpass between Radius1 & Radius2		*
C		:  Sigma1=0, 		use Radius1,Radius2 & Sigma2	*
C									*
C	Gaussian bandpass (low-pass + high-pass)			*
C		: Radii = 0,		use Sigma1 & Sigma2		*
C									*
C									*
C	Can now handle quite large images (8192 x 8192) using		*
C	the disk-based BIGFILT routine. The switch point is determined	*
C	by the current working set size.				*
C									*
C       The program will accept file names either from the command line *
C       or as entries to the program after it is started.  If there are *
C       two names on the command line, they will be taken as the input  *
C       and output file names; if there is one name, it will be taken   *
C       as the input file name and the program will ask for the output  *
C       file name; if there are no command line arguments, the program  *
C       will ask for both input and output file names.                  *
C									*
C	Input parameters are:						*
C									*
C	SIGMA1,SIGMA2,RADIUS1,RADIUS2					*
C				as described above			*
C									*
C	IORIG 			if IORIG = 1 then the F(0,0) is		*
C				unchanged by filter operation!!		*
C									*
C									*
C									*
C	Version 1.10	27.MAY.82	DAA		FOR VAX		*
C	Version 1.11	02.JUNE.82	DAA		FOR VAX		*
C	Version 1.12	10.JUNE.82	DAA		FOR VAX		*
C	Version 1.13	23.JULY.82	DAA		FOR VAX		*
C	Update  1.13	18.November.82	DAA		FOR VAX		*
C       Bug fix         14.July.88	DNM		FOR uVAX	*
C	Bit mode	09.August.88	DNM		FOR uVAX	*
C	Del-squared G	01.September.88	DNM		FOR uVAX	*
C	Inverted filter	26.April.89	DNM		FOR uVAX	*
C       Ported to unix	07.December.94  DNM		FOR SGI		*
C									*
C************************************************************************
C
	COMMON//NX,NY,NZ
	include 'ftbuf.inc'
	DIMENSION CTF(8193),NXYZ(3),MXYZ(3),TITLE(20)
	LOGICAL LARGE
	EQUIVALENCE (NX,NXYZ)
	DATA LARGE/.FALSE./
	character*80 filin,filout
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech

C
	WRITE(6,1000)
1000	FORMAT(' ENHANCE: GAUSSIAN BANDPASS FILTER PROGRAM  V1.13'//)
C
	call getinout(2,filin,filout)
	CALL IMOPEN(1,filin,'RO')
	CALL IMOPEN(2,filout,'NEW')
C
C READ IN CONTROL INFO
C
c	CALL RETWSIZE(ISIZE,IQUOTA)
c         here is the value of quota reported by HVEM VAX running from command
c         line or from slow batch queue
c	iquota = 2048
c	JQUOTA = MAX(IQUOTA,242+(NX+2)/16,242+NY/16,321)
c	NBUFSIZ = (JQUOTA - 240)*128
c	IF (JQUOTA .GT. IQUOTA) 
c     .	PRINT *,' Quota too small, must use virtual memory!!'
c	PRINT *,' Quota= ',IQUOTA,'  Buffersize= ',NBUFSIZ
c	  DNM 12/19/98: just set this to the true buffer size
	nbufsiz=ibufreal
	NY2 = NY/2
	SIGMA1 = 0.0
	SIGMA2 = 0.0
	RADIUS = 0.0
	IORIG = 0.0
	WRITE(6,1100)
1100	FORMAT(/,' Enter Sigma1,2, Radius1,2: ',$)
	READ(5,*) SIGMA1,SIGMA2,RADIUS1,RADIUS2
	WRITE(6,1200)
1200	FORMAT(' Reset origin to original value (0-no, 1-yes): ',$)
	READ(5,*,END=2) IORIG
2	WRITE(6,1500) SIGMA1,SIGMA2,RADIUS1,RADIUS2
1500	FORMAT(//,' SIGMAS & RADII for bandpass filter = ',4F10.4)
	IF (IORIG .EQ. 1) WRITE(6,1600)
1600	FORMAT(//,' ****    Origin is reset to original value ***'//)
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,2000,TITLE) SIGMA1,SIGMA2,RADIUS1,RADIUS2,IORIG
        write(titlech,2000) SIGMA1,SIGMA2,RADIUS1,RADIUS2,IORIG
2000    FORMAT('ENHANCE: Bandpass Sigmas,Radii,Iorig= ',4F9.4,2X,I1)
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
C
C   Take care of headers
C
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
	CALL ITRHDR(2,1)
	CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)

	call setctfwsr(sigma1,sigma2,radius1,radius2,ctf,nx,ny,delta)
	IF (IORIG .EQ. 1) CTF(1) = 1.0
C
C
C Loop over all sections is stack
C
	NX1 = NX - 1
	NY1 = NY - 1
	NX2 = NX + 2
	TMIN =  1.E10
	TMAX = -1.E10
	TMEAN = 0.0
c	  DNM: use the true buffer size here, it's much faster to do regular
	IF (NX2*NY .GT. ibufreal) LARGE = .TRUE.
	DO 300 IZ = 1,NZ
C
	  IF (LARGE) THEN
c   DNM added: bigfilt must use output mode 2 because it can't scale
            if(mode.ne.2)call ialmod(2,2)
            print *,'doing bigfilt'
c   DNM added new parameters to fit revised format of BIGFILT
	    CALL BIGFILT(1,2,NX,NY,CTF,DELTA,1.,0.,
     &          -1.e10,1.e10,DMIN,DMAX,DMEAN)
	  ELSE
C
c            print *,'reading section'
	    CALL IRDPAS(1,ARRAY,NX2,NY,0,NX1,0,NY1,*99)
c            print *,'doing filter'
	    CALL FILTER(ARRAY,NX,NY,CTF,DELTA)
c   dnm: every mode but 2 needs rescaling
            if(mode.ne.2)then
              CALL ISETDN(ARRAY,NX2,NY,mode,1,NX,1,NY,DMIN,DMAX,DMEAN)
            else
              CALL ICLDEN(ARRAY,NX2,NY,1,NX,1,NY,DMIN,DMAX,DMEAN)
            endif
c            print *,'writing section'
c   dnm replaced with call to repack then write whole section
	    CALL IREPAK(array,ARRAY,NX2,NY,0,NX1,0,NY1)
            call iwrsec(2,array)
	  END IF
C
	  IF (DMIN .LT. TMIN) TMIN = DMIN
	  IF (DMAX .GT. TMAX) TMAX = DMAX
	  TMEAN = TMEAN + DMEAN
	  JZ = IZ - 1
	  IF (NZ .GT. 1) WRITE(6,2500) JZ,DMIN,DMAX,DMEAN
300	CONTINUE
2500	FORMAT(' Section # ',I4,' Min,Max,Mean density = ',3F12.4)
2700	FORMAT(//' Overall Min,Max,Mean density = ',3F12.4)
C
	TMEAN = TMEAN/NZ
	WRITE(6,2700) TMIN,TMAX,TMEAN
	CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
	CALL IMCLOSE(1)
	CALL IMCLOSE(2)
C
	CALL EXIT(0)
99	STOP 'ENHANCE: END-OF-FILE ERROR ON READ'
	END

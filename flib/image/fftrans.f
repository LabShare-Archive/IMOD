C*FFTRANS.F**************************************************************
C									*
C	  This program will do 2- or 3-dimensional FFT's in either	*
C	  direction. The real-space origin is at (1,1) and		*
C	  the origin of reciprocal space is at (1,NY/2+1).		*
C	  The FT of an image NX,NY is NX/2+1,NY complex value.		*
C	  All transforms are done using Lynn ten Eyck's subroutines.	*
C	  These allow arbitrary-sized images having a LARGEST PRIME	*
C	  factor of 19.					        	*
c	  
c	  See man page for more details
C									*
c	DNM modified to do in "memory" rather than use giantfft as long as
c	the image will actually fit into the dimensioned array.  Also, changed
c	to properly package arrays in memory and use iwrsec to write sections;
c	this is much faster and saves lots of disk access.
c
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
c	  Revision 3.2  2002/07/30 17:57:28  mast
c	  Forget increasing buffer size for now
c	
c	  Revision 3.1  2002/07/30 17:44:30  mast
c	  Fixed titles, increased buffer size, standardized error exit
c	
C
	implicit none
	include 'ftbuf.inc'
	integer*4 NXYZR(3),MXYZR(3),NXYZF(3),NXYZST(3)
	CHARACTER DAT*9,TIM*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech

	LOGICAL LARGE, do3d, quiet, verbose
	integer*4 NX,NY,NZ,ifor,ibak,mode,ny2,ny21,nzm1,nxm1,nym1,nx2,nx21
	integer*4 nxp2,isec,index,nxr,iy,indBase,ierr,modeOut
	EQUIVALENCE (NX,NXYZR(1)),(NY,NXYZR(2)),(NZ,NXYZR(3))
	real*4 zero,DMIN,DMAX,DMEAN,TMIN,TMAX,TMEAN
	character*120 filin,filout
	DATA IFOR/0/,IBAK/1/,ZERO/0.0/, LARGE/.FALSE./, NXYZST/3*0/
C
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetLogical, PipGetInteger
	integer*4 PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  fftrans
c
	integer numOptions
	parameter (numOptions = 6)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@:3dfft:B:@'//
     &      'mode:Mode:I:@quiet:Quiet:B:@help:usage:B:'
c
	do3d = .false.
	modeOut = 2
	quiet = .false.
c
	call PipReadOrParseOptions(options, numOptions, 'fftrans',
     &	    'ERROR: FFTRANS - ', .true., 2, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0
	if (pipinput) then
	  ierr = PipGetLogical('Quiet', quiet)
	  ierr = PipGetInteger('Mode', modeOut)
	  if (quiet) call ialprt(.false.)
	endif
	verbose = .not.quiet
c
	if (verbose) WRITE(6,1000)
1000	FORMAT(//' FFTRANS: Fourier Transform Program  V1.10',//)
C
	if (PipGetInOutFile('InputFile', 1, 'Name of input file', filin) .ne.
     &	    0) call errorexit('NO INPUT FILE SPECIFIED')
c
	if (PipGetInOutFile('OutputFile', 2, 'Name of output file', filout)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
	CALL IMOPEN(1,filin,'RO')
	CALL IMOPEN(2,filout,'NEW')
	CALL DATE(DAT)
	CALL TIME(TIM)
C
C   Read input header & decide which direction to go
C
	CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
	CALL ITRHDR(2,1)
c
c	  DNM 12/19/98: just set this to the true buffer size
	nbufsiz=ibufreal
	NY2 = NY/2
	NY21 = NY2 + 1
	NZM1 = NZ - 1
	TMIN =  1.E10
	TMAX = -1.E10
	TMEAN = 0.0
	IF (NZ .GT. 1) then
	  if (pipinput) ierr = PipGetLogical('3dfft', do3d)
	  if (.not.do3d .and. verbose) WRITE(6,1200) NZ
1200	  FORMAT(//' Each of the ',I4,' sections are SEPARATELY ',
     &	      'transformed!!!!'//)
	endif
	IF (.not.(MODE .EQ. 3 .OR. MODE .EQ. 4)) then
C	    
C	    Here for foward transform
C	    
	  NXM1 = NX - 1
	  NYM1 = NY -1
	  NX2 = NX/2
	  NX21 = NX2 + 1
	  NXP2 = NX + 2
	  NXYZF(1) = NX21
	  NXYZF(2) = NY
	  NXYZF(3) = NZ
	  if (do3d .and. nxp2 * (ny + 1) * nz .gt. ibufreal) call errorexit(
     &	      'IMAGE TOO LARGE TO DO 3D FFT IN ARRAY; USE clip fft -3d')
c	    DNM: use the true buffer size here, it's much faster to do regular
	  IF (NXP2*NY .GT. ibufreal) LARGE = .TRUE.
	  IF (LARGE .AND. NZ .GT. 1) call errorexit(
     &	      'ONLY SINGLE SECTIONS CAN BE DONE FOR LARGE IMAGES')
	  CALL IALMOD(2,4)
	  CALL IALSIZ(2,NXYZF,NXYZST)
	  if (quiet) print *,'FFTRANS: Computing forward Fourier transform'
c	    
c	    7/7/00 CER: remove the encodes
c	    
c	    ENCODE(80,1500,TITLE) DAT,TIM
	  write(titlech,1500) DAT,TIM
1500	  FORMAT('FFTRANS: Forward Fourier Transform Calculated',12X,A9,
     .	      2X,A8)
	  CALL IWRHDRc(2,TITLEch,1,ZERO,ZERO,ZERO)
	  if (.not.do3d) then
C	      
C	      Loop over all sections & write out with shifted origin
C	      
	    DO ISEC = 0,NZM1
	      IF (LARGE) THEN
		CALL GIANTFFT(1,2,NX,NY,DMIN,DMAX,DMEAN,IFOR)
	      ELSE
		CALL IRDPAS(1,ARRAY,NXP2,NY,0,NXM1,0,NYM1,*99)
		CALL TODFFT(ARRAY,NX,NY,IFOR)
c		  DNM: a speed trick: swap the lines in memory and use iwrsec
		call swapden(array,nx21,ny,dmin,dmean,dmax)
		call iwrsec(2,array)
c		  DNM: add this IMPOSN call to make it work on stacks
c		  CALL IMPOSN(2,ISEC,0)
c		  CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,NY2,NYM1)
c		  CALL IMPOSN(2,ISEC,NY2)
c		  CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,0,NY2-1)
		CALL ICLCDN(ARRAY,NX21,NY,1,NX21,1,NY,DMIN,DMAX,DMEAN)
	      END IF
	      IF (DMIN .LT. TMIN) TMIN = DMIN
	      IF (DMAX .GT. TMAX) TMAX = DMAX
	      TMEAN = TMEAN + DMEAN
	      IF (NZ .GT. 1 .and. verbose) WRITE(6,1600) ISEC,DMIN,DMAX,DMEAN
	    enddo
1600	    FORMAT(' Min,Max,Mean for section ',I4,' are: ',3G13.5)
	  else
c	      
c	      3D forward FFT
c	      
	    DO ISEC = 0,NZM1
	      CALL IRDPAS(1,ARRAY(isec*nxp2*ny + 1),NXP2,NY,0,NXM1,0,NYM1,*99)
	    enddo
	    call thrdfft(array, array(nxp2 * ny * nz + 1), nx, ny, nz, ifor)
	    index = nxp2 * ny * nz / 2 + 1
	    DO ISEC = 0,NZM1
	      if (isec .eq. nz / 2) index = 1
	      call swapden(array(index),nx21,ny,dmin,dmean,dmax)
	      call iwrsec(2,array(index))
	      CALL ICLCDN(ARRAY(index),NX21,NY,1,NX21,1,NY,DMIN,DMAX,DMEAN)
	      IF (DMIN .LT. TMIN) TMIN = DMIN
	      IF (DMAX .GT. TMAX) TMAX = DMAX
	      TMEAN = TMEAN + DMEAN
	      index = index + nxp2 * ny
	    enddo
	  endif
	else
C	    
C	    Here for inverse transform
C	    
	  NXR = (NX - 1)*2
	  NXP2 = NXR + 2
	  NXYZF(1) = NXR
	  NXYZF(2) = NY
	  NXYZF(3) = NZ
	  if (do3d .and. nxp2 * (ny + 1) * nz .gt. ibufreal) call errorexit(
     &	      'IMAGE TOO LARGE TO DO 3D FFT IN ARRAY; USE clip fft -3d')
c	    DNM: use the true buffer size here, it's much faster to do regular
	  IF (NXP2*NY .GT. ibufreal) LARGE = .TRUE.
	  IF (LARGE .AND. NZ .GT. 1) call errorexit(
     &	      'ONLY SINGLE SECTIONS CAN BE DONE FOR LARGE IMAGES')
	  INDEX = NXP2*NY2 + 1
	  CALL IALMOD(2, modeOut)
	  CALL IALSIZ(2,NXYZF,NXYZST)
	  if (quiet) print *,'FFTRANS: Computing inverse Fourier transform'
c	    
c	    7/7/00 CER: remove the encodes
c	    
c	    ENCODE(80,1700,TITLE) DAT,TIM
	  write(titlech,1700) DAT,TIM
1700	  FORMAT('FFTRANS: Inverse Fourier Transform Calculated',12X,A9,
     .	      2X,A8)
	  CALL IWRHDRc(2,TITLEch,1,ZERO,ZERO,ZERO)
	  if (.not.do3d) then
C	      
C	      Loop over all sections , shift origin on reading in
C	      
	    DO ISEC = 0,NZM1
	      IF (LARGE) THEN
		CALL GIANTFFT(1,2,NXR,NY,DMIN,DMAX,DMEAN,IBAK)
	      ELSE
		DO IY = 1,NY
		  IF (IY .EQ. NY21) INDEX = 1
		  CALL IRDLIN(1,ARRAY(INDEX),*99)
		  INDEX = INDEX + NXP2
		enddo
		CALL TODFFT(ARRAY,NXR,NY,IBAK)
		CALL ICLDEN(ARRAY,NXP2,NY,1,NXR,1,NY,DMIN,DMAX,DMEAN)
c		  DNM: moved the ICLDEN up, switched to a repack and iwrsec
		CALL irepak(ARRAY,ARRAY,NXP2,NY,0,NXR-1,0,NY-1)
		call iwrsec(2,array)
c		  CALL IWRPAS(2,ARRAY,NXP2,NY,0,NXR-1,0,NY-1)
	      END IF
	      IF (DMIN .LT. TMIN) TMIN = DMIN
	      IF (DMAX .GT. TMAX) TMAX = DMAX
	      TMEAN = TMEAN + DMEAN
	      IF (NZ .GT. 1 .and. verbose) WRITE(6,1600) ISEC,DMIN,DMAX,DMEAN
	    enddo
	  else
c	      
c	      backward 3D transform: reorder sections as well as Y on read-in
c
	    indBase = nxp2 * ny * nz / 2
	    DO ISEC = 0,NZM1
	      if (isec .eq. nz / 2) indBase = 0
	      DO IY = 1,NY
		IF (IY .EQ. NY21) INDEX = 1
		CALL IRDLIN(1,ARRAY(INDEX + indBase),*99)
		INDEX = INDEX + NXP2
	      enddo
	      indBase = indBase + nxp2 * ny
	    enddo
c	      
c	      Call 3d routine with -1, as in clip
c
	    call thrdfft(array, array(nxp2 * ny * nz + 1), nxr, ny, nz, -1)
	    DO ISEC = 0,NZM1
	      index = isec*nxp2*ny + 1
	      CALL ICLDEN(ARRAY(index),NXP2,NY,1,NXR,1,NY,DMIN,DMAX,DMEAN)
	      CALL irepak(ARRAY(index),ARRAY(index),NXP2,NY,0,NXR-1,0,NY-1)
	      call iwrsec(2,array(index))
	      IF (DMIN .LT. TMIN) TMIN = DMIN
	      IF (DMAX .GT. TMAX) TMAX = DMAX
	      TMEAN = TMEAN + DMEAN
	    enddo
	  endif
	endif
C
	TMEAN = TMEAN/NZ
	if (verbose) WRITE(6,1800) TMIN,TMAX,TMEAN
1800	FORMAT(/,' Overall Min,Max,Mean values are: ',3G13.5)
	CALL IWRHDRc(2,TITLEch,-1,TMIN,TMAX,TMEAN)
	CALL IMCLOSE(1)
	CALL IMCLOSE(2)
	CALL EXIT(0)
C
99	call errorexit('READING FILE')
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

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: FFTRANS - ',message
	call exit(1)
	end

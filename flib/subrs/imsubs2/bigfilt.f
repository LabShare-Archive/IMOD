C*BIGFILT.FOR************************************************************
C									*
C	  This subroutine does 2-d convolution filtering in fourier 	*
C	space, on very LARGE images. The Input and Output images are	*
C	 stored on disk.						*
C	  The input images is fourier transformed. A radially-		*
C	symmetric contrast-transfer function is applied in fourier 	*
C	space and an inverse transform performed.			*
C	  This subroutine does the foward and inverse 2-D FFT's using	*
C	the Lynn Ten Eyck FFT subroutines - on very large images.	*
C	Arbitray sized transforms having prime factors not larger	*
C	than 19 can be accomidated.					*
C	  								*
C	  All necessary file set up for In/Out units must be		*
C	done by the calling program!!!					*
C	  								*
C	A scratch file is created to handle the required 		*
C	transpositions.							*
C									*
C									*
C	CALL BIGFILT(INUNIT,IOUTUNIT,NX,NY,CTF,DELTA,			*
C		     SCL,OFF,OMIN,OMAX,DMIN,DMAX,DMEAN)			*
C									*
C	NX is fast-dimension						*
C	CTF is the filter function					*
C	DELTA is the reciprocal-space interval for elements of the	*
C	      contrast-transfer function (ctf). The  first element	*
C	      is at s=0							*
C	SCL,OFF  are the scale and offsets for output densities		*
C	OMIN,OMAX are the limits for output densities after scaling	*
C									*
C	  The buffer space is available thru the common block /FTBUF/	*
C									*
C	Last Update:	23.July.1982		DAA      for VAX	*
C	Last Update:	18.Jan.1984		DAA      for VAX	*
C									*
C									*
C************************************************************************
C
	SUBROUTINE BIGFILT(INUNIT,IOUTUNIT,NX,NY,CTF,DELTA,
     .	scl,off,omin,omax,dmin,DMAX,DMEAN)
	include 'ftbuf.inc'
	COMPLEX CRAY(ibufreal/2),CLINE(10240)
	DIMENSION CTF(*)
	EQUIVALENCE (ARRAY,CRAY)
C
	CALL QOPEN(ISCR,'SCRATCH','SCRATCH')
	NXP2 = NX + 2
	NXP24 = NXP2*4
	NX21 = NXP2/2
	NXM1 = NX - 1
	NY2 = NY*2
	NY21 = NY/2 + 1
	DELX = 1.0/NX
	DELY = 1.0/NY
	DMIN =  1.E20
	DMAX = -1.E20
	DMEAN = 0.0
C
C  Calculate buffer loads
C
	NLINES = MIN(NBUFSIZ/NXP2,NY)
	JLINES = NLINES
	NSEC = NLINES*NXP24
	NLOADS = NY/NLINES
	NLAST = NY - NLINES*NLOADS
	IF (NLAST .EQ. 0) THEN
	  NLAST = NLINES
	ELSE
	  NLOADS = NLOADS + 1
	END IF
C
	NUSE = MIN(NBUFSIZ/NY2,NX21)
	NUSE8 = NUSE*8
	MLOADS = NX21/NUSE
	MLAST = NX21 - NUSE*MLOADS
	IF (MLAST .EQ. 0) THEN
	  MLAST = NUSE
	ELSE
	  MLOADS = MLOADS + 1
	END IF
C
C  Do first part of foward transform - writeout with no transposition
C
	WRITE(6,1000) NLINES,NLOADS,NUSE,MLOADS
1000	FORMAT(/,' BIGFILT: # Lines, Loads for passes 1,2: ',4I8,/)
	DO 150 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) THEN
	    NLINES = NLAST
	    NSEC = NLINES*NXP24
	  END IF
	  INDEX = 1
	  DO 100 LINES = 1,NLINES
	    CALL IRDLIN(INUNIT,ARRAY(INDEX),*90)
	    INDEX = INDEX + NXP2
100	  CONTINUE
C
	  CALL ODFFT(ARRAY,NX,NLINES,-3)
	  CALL QWRITE(ISCR,ARRAY,NSEC)
150	CONTINUE
C
C  Do second half of foward transform, transpose on reading in 
C
	AX = 0.0
	INDEX = 1
	DO 500 LOADS = 1,MLOADS
	  IF (LOADS .EQ. MLOADS) THEN
	    NUSE = MLAST
	    NUSE8 = NUSE*8
	  END IF
	  DO 250 IY = 1,NY
	    CALL QSEEK(ISCR,IY,INDEX,NXP24)
	    CALL QREAD(ISCR,CLINE,NUSE8,IER)
	    IF (IER .NE. 0) GOTO 90
	    IND = IY
	    DO 200 J = 1,NUSE
	      CRAY(IND) = CLINE(J)
	      IND = IND + NY
200	    CONTINUE
250	  CONTINUE
C
	  CALL ODFFT(CRAY,NY,NUSE,-1)
C
C  Apply filter function
C
	  IND = 0
	  DO 350 J = 1,NUSE
	    AX2 = AX*AX
	    AY = 0.0
	    DO 300 IY = 1,NY
	      IND = IND + 1
	      IF (IY .LE. NY21) THEN
		S = SQRT(AX2 + AY*AY)
	      ELSE
		S = SQRT(AX2 + (1.0 - AY)**2)
	      END IF
	      INDF = S/DELTA + 1.5
	      CRAY(IND) = CRAY(IND)*CTF(INDF)
	      AY = AY + DELY
300	    CONTINUE
	    AX = AX + DELX
350	  CONTINUE
C
C  Do first part of inverse transform, transpose on writing out
C
	  CALL ODFFT(CRAY,NY,NUSE,-2)
C
	  DO 450 IY = 1,NY
	    IND = IY
	    DO 400 J = 1,NUSE
	      CLINE(J) = CRAY(IND)
	      IND = IND + NY
400	    CONTINUE
	    CALL QSEEK(ISCR,IY,INDEX,NXP24)
	    CALL QWRITE(ISCR,CLINE,NUSE8)
450	  CONTINUE
	  INDEX = INDEX + NUSE8
500	CONTINUE
C
C   Do second half of inverse transform, no transposition required
C
	NLINES = JLINES
	CALL QSEEK(ISCR,1,1,1)
	DO 700 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) NLINES = NLAST
	  NSEC = NLINES*NXP24
	  CALL QREAD(ISCR,CRAY,NSEC,IER)
	  IF (IER .NE. 0) GOTO 90
C
	  CALL ODFFT(CRAY,NX,NLINES,1)
C
	  IND = 1
	  DO 650 LINES=1,NLINES
	    DO 600 J = 0,NXM1
	      indj = ind + j
	      VAL = ARRAY(INDJ)*scl + off
	      IF (VAL .LT. OMIN) VAL = OMIN
	      IF (VAL .GT. OMAX) VAL = OMAX
	      IF (VAL .LT. DMIN) DMIN = VAL
	      IF (VAL .GT. DMAX) DMAX = VAL
	      DMEAN = DMEAN + VAL
	     array(indj) = val
600	    CONTINUE
	    CALL IWRLIN(IOUTUNIT,ARRAY(IND))
	    IND = IND + NXP2
650	  CONTINUE
	  INDEX = INDEX + NLINES
700	CONTINUE
	DMEAN = DMEAN/(NX*NY)
C
	GOTO 99
C
C  Finish up
C
90	WRITE(6,9000)
9000	FORMAT(//,' ******* BIGFILT:   ERROR ON READ  ********',//)
	CALL IMCLOSE(INUNIT)
	CALL IMCLOSE(IOUTUNIT)
	CALL QCLOSE(ISCR)
	STOP '*** ERROR ****'
99	CALL QCLOSE(ISCR)
	RETURN
	END	

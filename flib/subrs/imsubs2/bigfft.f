C*BIGFFT.FOR*************************************************************
C									*
C	  This subroutine will do foward and inverse 2-D FFT's using	*
C	the Lynn Ten Eyck FFT subroutines - on very large images.	*
C	Arbitray sized transforms having prime factors not larger	*
C	than 19 can be accomidated.					*
C	  								*
C	  All necessary file set uo for In/Out units must be		*
C	done by the calling program!!!					*
C	  								*
C	A scratch file is created to handle the required 		*
C	transpositions. Two transpositions are done so that the final	*
C	transform is directly compatible with the in-core routines.	*
C									*
C									*
C	  The buffer space is available thru the common block /FTBUF/	*
C									*
C	Fourier Transform MUST ALWAYS be complex reals!!		*
C									*
C									*
C	Last Update:	23.July.1982		DAA      for VAX	*
C	Last Update:	18.Jan.1984		DAA      for VAX	*
C									*
C									*
C************************************************************************
C
	SUBROUTINE BIGFFT(INUNIT,IOUTUNIT,NX,NY,DMIN,DMAX,DMEAN,IDIR)
	include 'ftbuf.inc'
	COMPLEX CRAY(ibufreal/2),CLINE(10240)
	EQUIVALENCE (ARRAY,CRAY)
C
	IN = IMUNIT(INUNIT)
	IOUT = IMUNIT(IOUTUNIT)
	CALL QOPEN(ISCR,'SCRATCH','SCRATCH')
	NXP2 = NX + 2
	NXP24 = NXP2*4
	NX21 = NXP2/2
	NXM1 = NX - 1
	NY2 = NY*2
	NYO2 = NY/2
	NY8 = NY*8
	DMIN =  1.E20
	DMAX = -1.E20
	DMEAN = 0.0
C
C  Calculate buffer loads
C
	NLINES = MIN(NBUFSIZ/NXP2,NY)
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
C===============================================================
C
C   HERE FOR FOWARD TRANSFORM
C
	IF (IDIR .NE. 0) GOTO 50
C
C  Do first part of transform - writeout with no transposition
C
	WRITE(6,1000) NLINES,NLOADS,NUSE,MLOADS
1000	FORMAT(/,' BIGFFT: # Lines, Loads for passes 1,2: ',4I8,/)
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
C  Do second half of transform, traspose on reading in and writing out
C
	INDEX = 1
	DO 400 LOADS = 1,MLOADS
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
	  INDEX2 = INDEX + 1024
	  DO 350 IY = 1,NY
	    IND = IY
	    DO 300 J = 1,NUSE
	      CLINE(J) = CRAY(IND)
	      IND = IND + NY
	      VAL = CABS(CRAY(J))
	      IF (VAL .LT. DMIN) DMIN = VAL
	      IF (VAL .GT. DMAX) DMAX = VAL
	      DMEAN = DMEAN + VAL
300	    CONTINUE
	    IF (IY .LE. NYO2) THEN
	      JY = IY + NYO2
	    ELSE
	      JY = IY - NYO2
	    END IF
	    CALL QSEEK(IOUT,JY,INDEX2,NXP24)
	    CALL QWRITE(IOUT,CLINE,NUSE8)
350	  CONTINUE
	  INDEX = INDEX + NUSE8
400	CONTINUE
	DMEAN = DMEAN/(NX21*NY)
	GOTO 99
C
C==================================================================
C
C   HERE FOR INVERSE TRANSFORM
C
C
C  Do first half of transform, transpose on reading in
C
50	WRITE(6,1000) NUSE,MLOADS,NLINES,NLOADS
	INDEX = 1025
	DO 600 LOADS = 1,MLOADS
	  IF (LOADS .EQ. MLOADS) NUSE = MLAST
	  NUSE8 = NUSE*8
	  DO 550 IY = 1,NY
	    IF (IY .LE. NYO2) THEN
	      JY = IY + NYO2
	    ELSE
	      JY = IY - NYO2
	    END IF
	    CALL QSEEK(IN,JY,INDEX,NXP24)
	    CALL QREAD(IN,CLINE,NUSE8,IER)
	    IF (IER .NE. 0) GOTO 90
	    IND = IY
	    DO 500 J = 1,NUSE
	      CRAY(IND) = CLINE(J)
	      IND = IND + NY
500	    CONTINUE
550	  CONTINUE
C
	  CALL ODFFT(CRAY,NY,NUSE,-2)
	  CALL QWRITE(ISCR,CRAY,NUSE*NY8)
C
	  INDEX = INDEX + NUSE8
600	CONTINUE
C
C   Do second half of transform, transpose on reading in
C
	INDEX = 1
	DO 850 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) NLINES = NLAST
	  NLINE8 = NLINES*8
	  DO 700 IX = 1,NX21
	    CALL QSEEK(ISCR,IX,INDEX,NY8)
	    CALL QREAD(ISCR,CLINE,NLINE8,IER)
	    IF (IER .NE. 0) GOTO 90
	    IND = IX
	    DO 650 J = 1,NLINES
	      CRAY(IND) = CLINE(J)
	      IND = IND + NX21
650	    CONTINUE
700	  CONTINUE
C
	  CALL ODFFT(CRAY,NX,NLINES,1)
C
	  IND = 1
	  DO 800 LINES=1,NLINES
	    CALL IWRLIN(IOUTUNIT,ARRAY(IND))
	    DO 750 J = 0,NXM1
	      VAL = ARRAY(IND + J)
	      IF (VAL .LT. DMIN) DMIN = VAL
	      IF (VAL .GT. DMAX) DMAX = VAL
	      DMEAN = DMEAN + VAL
750	    CONTINUE
	    IND = IND + NXP2
800	  CONTINUE
	  INDEX = INDEX + NLINE8
850	CONTINUE
	DMEAN = DMEAN/(NX*NY)
C
	GOTO 99
C
C  Finish up
C
90	WRITE(6,9000)
9000	FORMAT(//,' ******* BIGFFT:   ERROR ON READ  ********',//)
	CALL IMCLOSE(INUNIT)
	CALL IMCLOSE(IOUTUNIT)
	CALL QCLOSE(ISCR)
	STOP '*** ERROR ****'
99	CALL QCLOSE(ISCR)
	RETURN
	END	

C*GIANTFFT.FOR***********************************************************
C									*
C	  This subroutine will do forward and inverse 2-D FFT's using	*
C	the Lynn Ten Eyck FFT subroutines - on very large images.	*
C	Arbitrary sized transforms having prime factors not larger	*
C	than 19 can be accommodated.					*
C	  								*
C	All necessary file set up for In/Out units must be		*
C	done by the calling program!!!					*
C	  								*
C	A scratch file is created to handle the required 		*
C	transpositions. Two transpositions are done so that the final	*
C	transform is directly compatible with the in-core routines.	*
C									*
C									*
C	The buffer size and space is in the common block /FTBUF/	*
C									*
C	Fourier Transform MUST ALWAYS be complex reals!!		*
C									*
C									*
C	Last Update:	23.July.1982		DAA      for VAX	*
C                       15.December.1984        RH       for VAX        *
c	Inversion fix   31.December.1988        DNM	 for uVAX
C									*
C************************************************************************
C
	SUBROUTINE GIANTFFT(INUNIT,IOUTUNIT,NX,NY,DMIN,DMAX,DMEAN,IDIR)
	include 'ftbuf.inc'
      	PARAMETER (INOUTMAX=10240)
      	DIMENSION BRRAY(2*INOUTMAX)
	COMPLEX CRAY(ibufreal/2),CLINE(INOUTMAX),CRRAY(INOUTMAX)
	EQUIVALENCE (ARRAY,CRAY),(CLINE,CRRAY,BRRAY)
C
	IN = IMUNIT(INUNIT)
	IOUT = IMUNIT(IOUTUNIT)
	NXP2 = NX + 2
	NXP24 = NXP2*4
	NX21 = NXP2/2
	NXM1 = NX - 1
	NY2 = NY*2
	NYO2 = NY/2
      	NSCRSIZE = (NXP2*NY*4 + 1024)/512 + 1
      	  WRITE(6,10) NSCRSIZE
10	  FORMAT('  Scratch file requires filespace of',I7,'  pages')
	CALL QOPEN(ISCR,'SCRATCH','SCRATCH')
	DMIN =  1.E20
	DMAX = -1.E20
	DMEAN = 0.0
      	INOUTSQRT=SQRT(2.0*INOUTMAX)
C
C  Calculate buffer loads
C
	NLINES = MIN(NBUFSIZ/NXP2,NY,INOUTSQRT)
	NLOADS = NY/NLINES
	NLAST = NY - NLINES*NLOADS
	IF (NLAST .EQ. 0) THEN
	  NLAST = NLINES
	ELSE
	  NLOADS = NLOADS + 1
	END IF
C
	NUSE = MIN(NBUFSIZ/NY2,NX21,INOUTSQRT/2)
	MLOADS = NX21/NUSE
	MLAST = NX21 - NUSE*MLOADS
	IF (MLAST .EQ. 0) THEN
	  MLAST = NUSE
	ELSE
	  MLOADS = MLOADS + 1
	END IF
C==============================================================================
C
C   HERE FOR FORWARD TRANSFORM
C
	IF (IDIR .NE. 0) GOTO 50
C
C  Do first part of transform - writeout with partial transposition
C  Scratch file storage has local y-coord fastest in strips parallel to x,
C   then x-coord, then slowest variable is the y-index of the strip.
C
	WRITE(6,1000) NLINES,NLOADS,NUSE,MLOADS
1000	FORMAT(/,' GIANTFFT: # Lines, Loads for passes 1,2: ',4I8,/)
	DO 150 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) THEN
	    NL = NLAST
      	  ELSE
      	    NL=NLINES
	  END IF
	  INDEX = 1
	  DO 100 LINES = 1,NL
	    CALL IRDLIN(INUNIT,ARRAY(INDEX),*90)
	    INDEX = INDEX + NXP2
100	  CONTINUE
C
	  CALL ODFFT(ARRAY,NX,NL,0)
C
C    This section partially transposes the array before output in strips.
      	  NTURN= MIN(INOUTMAX/NL,NX21)
      	  NOUT=NTURN*NL*8
      	  NTIMES=NX21/NTURN
      	  NTURNLAST=NX21-NTIMES*NTURN
      	  IF(NTURNLAST.EQ.0) THEN
      		NTURNLAST=NTURN
      	  ELSE
      		NTIMES=NTIMES+1
      	  ENDIF
      	  INDEX=0
      	  DO 120 IDO=1,NTIMES
      		INDEXB=1
      		IF(IDO.EQ.NTIMES) THEN
      		  NTURN=NTURNLAST
      		  NOUT=NTURN*NL*8
      		ENDIF
      	        DO 110 ITURN=1,NTURN
      		  INDEX=INDEX+1
      		  INDEXA=INDEX*2-1
      	        DO 110 LINES=1,NL
      		  BRRAY(INDEXB)=ARRAY(INDEXA)
      		  BRRAY(INDEXB+1)=ARRAY(INDEXA+1)
      		  INDEXB=INDEXB+2
      		  INDEXA=INDEXA+NXP2
110	        CONTINUE
      		CALL QWRITE(ISCR,BRRAY,NOUT)
120	   CONTINUE
150	CONTINUE
C
C Do second half of transform, build up strip parallel to y by picking up
C  bands on reading in. Write out in same way - initially to same scratch area.
C
        NUSES=NUSE
	DO 400 LOADS = 1,MLOADS
	  IF (LOADS .EQ. MLOADS) THEN
	    NUSE = MLAST
	  END IF
	  DO 250 IY = 1,NLOADS
      		IF(IY.EQ.NLOADS) THEN
      		  NL=NLAST
      		ELSE
      		  NL=NLINES
      	    	ENDIF
      	    	NBLOCK=NUSE*NL
      	    	NBLOCK8=NBLOCK*8
      	    	INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
	    	CALL QSEEK(ISCR,INDEX,1,8)
	    	CALL QREAD(ISCR,CRRAY,NBLOCK8,IER)
	    	IF (IER .NE. 0) GOTO 90
      		IND=1+(IY-1)*NLINES-NY
      		INDB=1
      		DO 200 J=1,NUSE
      		  IND=IND+NY
      		  INDA=IND
      		DO 200 I=1,NL
      		  CRAY(INDA)=CRRAY(INDB)
      		  INDA=INDA+1
      		  INDB=INDB+1
200		CONTINUE
250	  CONTINUE
C
	  CALL ODFFT(CRAY,NY,NUSE,-1)
C
C  Write back to same area of scratch file now.
	  DO 350 IY = 1,NLOADS
      		IF(IY.EQ.NLOADS) THEN
      		  NL=NLAST
      		ELSE
      		  NL=NLINES
      		ENDIF
      		NBLOCK=NUSE*NL
      		NBLOCK8=NBLOCK*8
      		IND=1+(IY-1)*NLINES-NY
      		INDB=1
      		DO 300 J=1,NUSE
      		  IND=IND+NY
      		  INDA=IND
      		DO 300 I=1,NL
      		  CRRAY(INDB)=CRAY(INDA)
      		  INDA=INDA+1
      		  INDB=INDB+1
300		CONTINUE
      		INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
		CALL QSEEK(ISCR,INDEX,1,8)
		CALL QWRITE(ISCR,CRRAY,NBLOCK8)
350	  CONTINUE
400	CONTINUE
C
C  Read from scratch area, transpose and output fully transformed file.
      	CALL QSEEK(ISCR,1,1,8)
	DO 450 LOADS = 1,NLOADS
		IF (LOADS .EQ. NLOADS) THEN
		  NL = NLAST
      		ELSE
      		  NL=NLINES
		END IF
      		NSEC=NL*NXP24
      		CALL QREAD(ISCR,CRAY,NSEC,IER)
      		IF(IER.NE.0) GOTO 90
      		DO 420	LINES=1,NL
      			INDEXB=LINES
      			INDEXA=1
      			DO 410 IX=1,NX21
c			    DNM: seems to need complex conjugate to avoid
c			    inversion about origin after introducing the
c			    transposition below.
      			   CLINE(INDEXA)=conjg(CRAY(INDEXB))
	        	   VAL = CABS(CRAY(INDEXB))
	        	   IF (VAL .LT. DMIN) DMIN = VAL
	        	   IF (VAL .GT. DMAX) DMAX = VAL
	        	   DMEAN = DMEAN + VAL
      			   INDEXA=INDEXA+1
      			   INDEXB=INDEXB+NL
410			CONTINUE
      			IY=LINES + (LOADS-1)*NLINES
			IF (IY .LE. NYO2) THEN
			  JY = IY + NYO2
			ELSE
			  JY = IY - NYO2
			END IF
c			  DNM: with deep apologies, this transposition of all
c			  but the first line is needed to make the result be
c			  both correct and the same as that put out by TODFFT
			if (jy .gt. 1) jy = ny + 2 - jy
      			CALL QSEEK(IOUT,JY,1025,NXP24)
      			CALL QWRITE(IOUT,CLINE,NXP24)
420		CONTINUE
450	CONTINUE
	DMEAN = DMEAN/(NX21*NY)
	GOTO 99
C
C==============================================================================
C
C   HERE FOR INVERSE TRANSFORM
C
C
C  Read in, transpose into strips, and write out to scratch area.
C
50	WRITE(6,1000) NUSE,MLOADS,NLINES,NLOADS
	DO 550 LOADS = 1,NLOADS
		IF (LOADS .EQ. NLOADS) THEN
		  NL = NLAST
      		ELSE
      		  NL=NLINES
		END IF
      		DO 520	LINES=1,NL
      			INDEXB=LINES
      			INDEXA=1
      			IY=LINES + (LOADS-1)*NLINES
			IF (IY .LE. NYO2) THEN
			  JY = IY + NYO2
			ELSE
			  JY = IY - NYO2
			END IF
      			CALL QSEEK(IN,JY,1025,NXP24)
      			CALL QREAD(IN,CLINE,NXP24,IER)
      			IF(IER.NE.0) GOTO 90
      			DO 510 IX=1,NX21
      			   CRAY(INDEXB)=CLINE(INDEXA)
      			   INDEXA=INDEXA+1
      			   INDEXB=INDEXB+NL
510			CONTINUE
520		CONTINUE
      		NSEC=NL*NXP24
      		CALL QWRITE(ISCR,CRAY,NSEC)
550	CONTINUE
C
C   Do first part of transform : build up strip parallel to y by picking up
C   bands on reading in.   Write out in same way to same scratch area.
C
        NUSES=NUSE
	DO 800 LOADS = 1,MLOADS
	  IF (LOADS .EQ. MLOADS) THEN
	    NUSE = MLAST
	  END IF
	  DO 650 IY = 1,NLOADS
      		IF(IY.EQ.NLOADS) THEN
      		  NL=NLAST
      		ELSE
      		  NL=NLINES
      	    	ENDIF
      	    	NBLOCK=NUSE*NL
      	    	NBLOCK8=NBLOCK*8
      	    	INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
	    	CALL QSEEK(ISCR,INDEX,1,8)
	    	CALL QREAD(ISCR,CRRAY,NBLOCK8,IER)
	    	IF (IER .NE. 0) GOTO 90
      		IND=1+(IY-1)*NLINES-NY
      		INDB=1
      		DO 600 J=1,NUSE
      		  IND=IND+NY
      		  INDA=IND
      		DO 600 I=1,NL
      		  CRAY(INDA)=CRRAY(INDB)
      		  INDA=INDA+1
      		  INDB=INDB+1
600		CONTINUE
650	  CONTINUE
C
	  CALL ODFFT(CRAY,NY,NUSE,-2)
C
C  Write back to same area of scratch file now.
	  DO 750 IY = 1,NLOADS
      		IF(IY.EQ.NLOADS) THEN
      		  NL=NLAST
      		ELSE
      		  NL=NLINES
      		ENDIF
      		NBLOCK=NUSE*NL
      		NBLOCK8=NBLOCK*8
      		IND=1+(IY-1)*NLINES-NY
      		INDB=1
      		DO 700 J=1,NUSE
      		  IND=IND+NY
      		  INDA=IND
      		DO 700 I=1,NL
      		  CRRAY(INDB)=CRAY(INDA)
      		  INDA=INDA+1
      		  INDB=INDB+1
700		CONTINUE
      		INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
		CALL QSEEK(ISCR,INDEX,1,8)
		CALL QWRITE(ISCR,CRRAY,NBLOCK8)
750	  CONTINUE
800	CONTINUE
C
C  Read from scratch area, transpose, do second part of transform and
C				        output fully transformed file.
      	CALL QSEEK(ISCR,1,1,8)
	DO 950 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) THEN
	    NL = NLAST
      	  ELSE
      	    NL=NLINES
	  END IF
C    This section does partial transpose of the array on input.
      	  NTURN= MIN(INOUTMAX/NL,NX21)
      	  NOUT=NTURN*NL*8
      	  NTIMES=NX21/NTURN
      	  NTURNLAST=NX21-NTIMES*NTURN
      	  IF(NTURNLAST.EQ.0) THEN
      		NTURNLAST=NTURN
      	  ELSE
      		NTIMES=NTIMES+1
      	  ENDIF
      	  INDEX=0
      	  DO 920 IDO=1,NTIMES
      		INDEXB=1
      		IF(IDO.EQ.NTIMES) THEN
      		  NTURN=NTURNLAST
      		  NOUT=NTURN*NL*8
      		ENDIF
	  	CALL QREAD(ISCR,BRRAY,NOUT,IER)
      		IF(IER.NE.0) GOTO 90
      	        DO 910 ITURN=1,NTURN
      		  INDEX=INDEX+1
      		  INDEXA=INDEX*2-1
      	        DO 910 LINES=1,NL
      		  ARRAY(INDEXA)=BRRAY(INDEXB)
      		  ARRAY(INDEXA+1)=BRRAY(INDEXB+1)
      		  INDEXB=INDEXB+2
      		  INDEXA=INDEXA+NXP2
910	        CONTINUE
920	  CONTINUE
C
	  CALL ODFFT(ARRAY,NX,NL,1)
C
	  INDEX = 1
	  DO 900 LINES = 1,NL
	    CALL IWRLIN(IOUTUNIT,ARRAY(INDEX))
	    DO 940 J = 0,NXM1
	      VAL = ARRAY(INDEX + J)
	      IF (VAL .LT. DMIN) DMIN = VAL
	      IF (VAL .GT. DMAX) DMAX = VAL
	      DMEAN = DMEAN + VAL
940	    CONTINUE
	    INDEX = INDEX + NXP2
900	  CONTINUE
950	CONTINUE
	DMEAN = DMEAN/(NX*NY)
C
C  Finish up
C
99	CALL QCLOSE(ISCR)
	RETURN
C
90	WRITE(6,9000)
9000	FORMAT(//,' ****** GIANTFFT:   ERROR ON DISCREAD  *******',//)
	CALL QCLOSE(ISCR)
	CALL IMCLOSE(INUNIT)
	CALL IMCLOSE(IOUTUNIT)
	STOP '*** ERROR ****'
	END

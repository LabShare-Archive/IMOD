C*ODFFT.FOR*******************************************************
C
C	Performs NY 1-D FFT'S in place. Uses Lynn Ten Eyck's FFT routines.
C	Thus only restrictions are NX MUST be EVEN & max prime factor =19.
C
C	Origin is at first point in each strip
C	A Normalization factor is applied during ALL transformations
C
C	For Real/complex or Complex/real:
C		Only the unique portion of the Transform is written
C		DIMENSION ARRAY(NX+2,NY)
C
C	For Complex/complex
C		The entire Transform is written
C		COMPLEX ARRAY(NX,NY)
C
C
C	IDIR =  0 Foward   (Real --> Complex)     exp(+2PIirs)
C	IDIR =  1 Reverse  (Complex --> Real)	  exp(-2PIirs)
C	IDIR = -1 Foward   (Complex --> Complex)  exp(+2PIirs)
C	IDIR = -2 Reverse  (Complex --> Complex)  exp(-2PIirs)
C	IDIR = -3 Reverse  (Real --> Complex)     exp(-2PIirs)
C
C
C	Version 1.00	Apr 19 1982		DAA
C	Version 1.01	Nov 23 1982		DAA
C	Version 1.02	Jul 28 1983		DAA
C	Version 1.03	Jan 18 1984		DAA
C
C
        SUBROUTINE ODFFT(ARRAY,NX,NY,IDIR)
        DIMENSION ARRAY(*),IDIM(5)
C
        NXO2 = NX/2
        IF (2*NXO2 .EQ. NX .OR. IDIR .LT. 0) GOTO 1
        WRITE(6,1000) NX
1000    FORMAT(' ODFFT: NX= ',I7,' MUST BE EVEN!!!')
        STOP
1       ONEVOL = SQRT(1.0/NX)
        GOTO (30,10,10,30,40) IDIR + 4
C
C********      COMPLEX TRANSFORMS COME HERE       ******************
C
C
C
C  SET UP FOR FIRST DIMENSION OF TRANSFORM
C
10      NX2 = NX*2
        NXT = NX2*NY
        NXT1 = NXT - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NX2
C
        CALL CMPLFT(ARRAY(1),ARRAY(2),NX,IDIM)
C
        IF (IDIR .EQ. -1) GOTO 15
C
C   SCALE BY 1/VOLUME
C
        DO 100 J = 1,NXT
          ARRAY(J) = ARRAY(J)*ONEVOL
100     CONTINUE
        RETURN
C
C   TAKE COMPLEX CONJUGATE TO DO FOWARD & SCALE BY 1/VOLUME
C
15      DO 150 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = -ARRAY(J+1)*ONEVOL
150     CONTINUE
        RETURN
C
C
C********      FOWARD REAL TRANSFORM COMES HERE       ******************
C
C
C  SET UP FOR TRANSFORM
C
30      NXP2 = NX + 2
        NXT = NXP2*NY
        NXT1 = NXT - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
        CALL REALFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
        if (idir .eq. -3) goto 35
C
C    NORMALIZE DATA AND TAKE COMPLEX CONJUGATE FOR TRUE FOWARD TRANSFORM
C
        DO 200 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J + 1) = -ARRAY(J + 1)*ONEVOL
200     CONTINUE
C
        RETURN
C
C    NORMALIZE DATA
C
35      DO 250 J = 1,NXT
          ARRAY(J) = ARRAY(J)*ONEVOL
250     CONTINUE
C
        RETURN

C
C********      INVERSE HERMITE TRANSFORM COMES HERE       ******************
C
C
C  SET UP FOR TRANSFORM
C
40      NXP2 = NX + 2
        NXT = NXP2*NY
        NXM1 = NX - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
C   NORMALIZE DATA
C
        DO 300 J = 1,NXT
          ARRAY(J) = ONEVOL*ARRAY(J)
300     CONTINUE
C
C  CHANGE DATA STORAGE MODE
C
        INDEX = 2
        DO 350 IY = 1,NY
          ARRAY(INDEX) = ARRAY(NXM1 + INDEX)
          INDEX = INDEX + NXP2
350     CONTINUE
C
        CALL HERMFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
        RETURN
        END

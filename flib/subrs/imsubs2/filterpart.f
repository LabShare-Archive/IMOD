c	  FILTERPART applies the filter in CTF to the array in FFT, puts result
c	  into ARRAY.  NX and NY are real image dimensions, DELTA is the
c	  interval in reciprocal space for the CTF.  See filter.f
c
	SUBROUTINE FILTERPART(FFT,ARRAY,NX,NY,CTF,DELTA)
	DIMENSION ARRAY(*),CTF(*),FFT(*)
	COMPLEX ARRAY,FFT
C
	NXO2 = NX/2
	NX21 = NXO2 + 1
	NYM1 = NY - 1
	DELX = 1.0/NX
	DELY = 1.0/NY
C
C   APPLY FILTER FUNCTION on FFT, put result in ARRAY
C
c        print *,'filtering ...'
	INDEX = 1
	DO 200 IY = 0,NYM1
	  Y = IY*DELY
	  IF (Y .GT. 0.5) Y = 1.0 - Y
	  Y2 = Y*Y
	  X = 0.0
	  DO 100 IX = 0,NXO2
	    IND = INDEX + IX
	    S = SQRT(X*X + Y2)
	    INDF = S/DELTA + 1.5
	    ARRAY(IND) = FFT(IND)*CTF(INDF)
	    X = X + DELX
100	  CONTINUE
	  INDEX = INDEX + NX21
200	CONTINUE
C
	RETURN
	END

C*FFT1D.FR***********************************************
C
C     SIMPLE (NON-OPTIMIZED) SUBROUTINE FOR DOING
C     1-DIMENSIONAL FFT'S
C     IDIR = 0   FOWARD  TRANSFORM
C     IDIR = 1   INVERSE TRANSFORM
C
      SUBROUTINE FFT1D(A,N,IDIR)
      COMPLEX A(1),U,W,T
      DATA ALOG2/0.69314718/,PI/3.141592653/
C
      NV2 = N/2
      NM1 = N - 1
      J = 1
      LOG2N = ALOG(FLOAT(N))/ALOG2 + .1
      DO 100 I = 1,NM1
        IF (I .GE. J) GOTO 5
        T = A(J)
        A(J) = A(I)
        A(I) = T
5       K = NV2
6       IF (K .GE. J) GOTO 7
        J = J - K
        K = K/2
        GOTO 6
7       J = J + K
100   CONTINUE
C
      API = PI
      IF (IDIR .EQ. 1) API = -PI
      LE = 1
      DO 400 L = 1,LOG2N
        LE1 = LE
        LE = LE*2
        U = (1.0,0.)
        PL1 = API/LE1
        W = CMPLX(COS(PL1),SIN(PL1))
        DO 300 J = 1,LE1
          DO 200 I = J,N,LE
            IP = I + LE1
            T = A(IP)*U
            A(IP) = A(I) - T
            A(I) = A(I) + T
200       CONTINUE
          U = U*W
300     CONTINUE
400   CONTINUE
C
      RETURN
      END

      SUBROUTINE R SYM FT (X, N, DIM)
      INTEGER N
      INTEGER DIM(5)
      REAL X(1)
C
C     REAL SYMMETRIC MULTIDIMENSIONAL FOURIER TRANSFORM
C     N MUST BE A MULTIPLE OF 4.  THE TWO UNIQUE ELEMENTS ARE STORED AT
C     X(1) AND X(N+1).
C
C     DECIMATION IN FREQUENCY APPLIED TO A REAL SYMMETRIC SEQUENCE OF
C     LENGTH 2N GIVES A REAL SYMMETRIC SEQUENCE OF LENGTH N, THE
C     TRANSFORM OF WHICH GIVES THE EVEN NUMBERED FOURIER COEFFICIENTS,
C     AND A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH N, THE TRANSFORM OF
C     WHICH GIVES THE ODD NUMBERED FOURIER COEFFICIENTS.  THE SUM OF
C     THE TWO SEQUENCES IS A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH N,
C     WHICH MAY BE STORED IN N/2 COMPLEX LOCATIONS.  THE TRANSFORM OF
C     THIS SEQUENCE IS N REAL NUMBERS REPRESENTING THE TERM BY TERM SUM
C     OF THE EVEN AND ODD NUMBERED FOURIER COEFFICIENTS.  THIS SYMMETRIC
C     SEQUENCE MAY BE SOLVED IF ANY OF THE FOURIER COEFFICIENTS ARE
C     KNOWN.  FOR THIS PURPOSE X0, WHICH IS SIMPLY THE SUM OF THE
C     ORIGINAL SEQUENCE, IS COMPUTED AND SAVED IN X(N+1).
C
      REAL A, B, ANGLE, CO, SI, TWO N, TWO PI
      INTEGER I, J0, J1, J2, K, K0, K1, K2, L, MOD, NN, N2, TEST
      INTEGER N OVER 2, N OVER 4
      INTEGER D1, D2, D3, D4, D5
      INTEGER II, I0, I1, I2, J, M, MJ, MK, ML, MM, TWOD2
C
      IF (N .EQ. 1) GO TO 1300
      N OVER 2 = N/2
      N OVER 4 = N/4
      IF (4*N OVER 4 .NE. N) GO TO 1400
      D1 = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
      TWO PI = 6.2831853
      TWO N = FLOAT(2*N)
      TWOD2 = 2*D2
C
      K0 = N*D2 + 1
      DO 100 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 100 K = K1, K2, D5
      X(K) = X(K)/2.0
  100 CONTINUE
C
      DO 300 I = 2, N OVER 2
      ANGLE = TWO PI*FLOAT(I-1)/TWO N
      CO = COS(ANGLE)
      SI = SIN(ANGLE)
      K0 = (I - 1)*D2 + 1
      J0 = (N + 2 - 2*I)*D2
      J1 = (N + 1 - I)*D2
      DO 200 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 200 K = K1, K2, D5
      L = K + J0
      NN = K + J1
      A = X(L) + X(K)
      B = X(L) - X(K)
      X(K) = A - B*CO
      X(L) = B*SI
      X(NN) = X(NN) + A
  200 CONTINUE
  300 CONTINUE
C
      IF (N OVER 4 .EQ. 1) GO TO 600
      J0 = N OVER 4 - 1
      DO 500 I = 1, J0
      K0 = (N OVER 2 + I)*D2 + 1
      J1 = (N OVER 2 - 2*I)*D2
      DO 400 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 400 K = K1, K2, D5
      L = K + J1
      A = X(K)
      X(K) = X(L)
      X(L) = A
  400 CONTINUE
  500 CONTINUE
C
  600 CONTINUE
      J0 = N OVER 2*D2
      J1 = N*D2
      DO 700 K1 = 1, D1, D3
      K2 = K1 + D4
      DO 700 K = K1, K2, D5
      I = K + J0
      L = K + J1
      X(I) = 2.0*X(I)
      X(L) = X(K) + X(I) + 2.0*X(L)
      X(K) = 2.0*X(K)
  700 CONTINUE
C
      K = N OVER 2*D2 + 1
      CALL HERM FT (X(1), X(K), N OVER 2, DIM)
C
C     SOLVE THE EQUATIONS FOR ALL OF THE SEQUENCES
C
      I0 = 1 - D2
      MK = N OVER 2*D2
      MJ = MK + D2
      ML = N*D2 + D2
      MM = ML
      DO 800 II = 1, N OVER 4
      I0 = I0 + D2
      MJ = MJ - TWOD2
      ML = ML - TWOD2
      MM = MM - D2
      DO 800 I1 = I0, D1, D3
      I2 = I1 + D4
      DO 800 I = I1, I2, D5
      J = I + MJ
      K = I + MK
      L = I + ML
      M = I + MM
      A = X(I) - X(M)
      B = X(L) - A
      C = X(K) - B
      D = X(J) - C
      X(I) = X(M)
      X(J) = A
      X(K) = B
      X(L) = C
      X(M) = D
  800 CONTINUE
C
C     THE RESULTS ARE NOW IN A SCRAMBLED DIGIT REVERSED ORDER, I.E.
C     X(1), X(5), X(9), ..., X(10), X(6), X(2), ..., X(3), X(7), X(11),
C     ..., X(12), X(8), X(4).  THE FOLLOWING SECTION OF PROGRAM FOLLOWS
C     THE PERMUTATION CYCLES AND DOES THE NECESSARY INTERCHANGES.
C
      IF (N OVER 4 .EQ. 1) GO TO 1300
      NN = N - 2
      DO 1200 I = 1, NN
      K = I
C
 1000 CONTINUE
      K0 = K/4
      L = K - K0*4
      IF (L .NE. (L/2)*2) K0 = N OVER 4 - 1 - K0
      K = K0 + L*N OVER 4
      IF (K .LT. I) GO TO 1000
      IF (K .EQ. I) GO TO 1200
C
      K0 = I*D2 + 1
      J0 = (K - I)*D2
      DO 1100 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 1100 K = K1, K2, D5
      L = K + J0
      A = X(K)
      X(K) = X(L)
      X(L) = A
 1100 CONTINUE
 1200 CONTINUE
C
 1300 CONTINUE
      RETURN
C
 1400 CONTINUE
      WRITE (6, 1500) N
      STOP
C
 1500 FORMAT (/,' N NOT A MULTIPLE OF 4 IN R SYM FT.  N =',I10//)
C
      END

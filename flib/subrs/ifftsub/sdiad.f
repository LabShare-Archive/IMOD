      SUBROUTINE SDIAD (X, Y, N, DIM)
      INTEGER N
      INTEGER DIM(5)
      REAL X(1), Y(1)
C
C     THIS SUBROUTINE COMPUTES HALF THE FOURIER SYNTHESIS ALONG A SCREW
C     DIAD LYING ALONG A CRYSTALLOGRAPHIC AXIS GIVEN HALF THE FOURIER
C     COEFFICIENTS.  THAT IS, IT ASSUMES THAT F(T) = CONJG(F(-T)) FOR T
C     EVEN AND F(T) = -CONJG(F(-T)) FOR T ODD.  N IS THE LENGTH OF THE
C     DESIRED HALF OF THE TRANSFORM.  THE LOCATION X(N+1) IS REQUIRED AS
C     A SCRATCH LOCATION AND THEREFORE A VALUE IS ALSO RETURNED IN
C     X(N+1) AND Y(N+1).  THE VALUE OF THE SECOND HALF OF THE TRANSFORM
C     MAY BE GENERATED FROM THE FIRST HALF BY THE FORMULA X(N+T) = X(T),
C     Y(N+T) = -Y(T).  IN OTHER WORDS, THE LAST HALF OF THE TRANSFORM IS
C     THE COMPLEX CONJUGATE OF THE FIRST HALF.
C
C     THE TRANSFORM IS CALCULATED BY FORMING THE SUM OF THE EVEN TERMS
C     AND THE ODD TERMS IN PLACE, USING THE SYMMETRY RELATIONS TO
C     OBTAIN THE VALUES FOR NEGATIVE SUBSCRIPTS.  THE TRANSFORM OF THE
C     RESULTING SEQUENCE MAY BE SEPARATED BY USING THE FACT THAT THE
C     TRANSFORM OF THE EVEN TERMS IS REAL, WHILE THE PRODCT OF THE
C     TRANSFORM OF THE ODD TERMS AND (COS(PI*T/N) - I*SIN(PI*T/N)) IS
C     IMAGINARY.  THE SCRATCH LOCATION IS REQUIRED BECAUSE THE FORMULA
C     FOR SEPARATING THE TWO TRANSFORMS BREAKS DOWN WHEN T = N/2.
C
      LOGICAL FOLD
      REAL A, ANGLE, C, S, TWO N, TWO PI
      INTEGER D1, D2, D3, D4, D5
      INTEGER I, J, K, K0, K1, K2, L, M, MM, NN, N OVER 2
C
      N OVER 2 = N/2
      IF (2*N OVER 2 .NE. N) GO TO 700
      TWO N = FLOAT(2*N)
      TWO PI = 6.2831853
      D1 = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
C
      K0 = (N - 1)*D2 + 1
      DO 100 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 100 K = K1, K2, D5
      L = K + D2
      X(L) = X(K)
  100 CONTINUE
      S = 1.0
      NN = N - 2
      DO 200 I = 1, NN, 2
      S = -S
      MN = (N + 1 - I)*D2
      K0 = (I - 1)*D2 + 1
      DO 150 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 150 K = K1, K2, D5
      J = K + D2
      L = K + 2*D2
      M = K + MN
      X(M) = X(M) + S*X(J)
      X(K) = X(K) + X(J)
      X(J) = X(L) - X(J)
      Y(K) = Y(K) + Y(J)
      Y(J) = Y(J) - Y(L)
  150 CONTINUE
  200 CONTINUE
      K0 = (N - 2)*D2 + 1
      DO 250 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 250 K = K1, K2, D5
      L = K + D2
      X(K) = X(K) + X(L)
      Y(K) = Y(K) + Y(L)
      X(L) = -X(L)
  250 CONTINUE
C
C     REORDER SCRAMBLED FOURIER COEFFICIENTS
C
      DO 400 I = 1, NN
      K = I
  300 CONTINUE
      K = 2*K
      IF (K .GT. N - 1) K = 2*N - 1 - K
      IF (K .LT. I) GO TO 300
      IF (K .EQ. I) GO TO 400
      J = (K - I)*D2
      K0 = I*D2 + 1
      DO 350 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 350 K = K1, K2, D5
      L = K + J
      A = X(K)
      X(K) = X(L)
      X(L) = A
      A = Y(K)
      Y(K) = Y(L)
      Y(L) = A
  350 CONTINUE
  400 CONTINUE
C
      CALL CMPL FT (X, Y, N, DIM)
C
      M = N OVER 2 - 1
      DO 600 I = 1, M
      ANGLE = TWO PI*FLOAT(I)/TWO N
      C = COS(ANGLE)
      S = SIN(ANGLE)
      K0 = I*D2 + 1
      FOLD = .TRUE.
      GO TO 500
C
  450 CONTINUE
      C = -C
      K0 = (N - I)*D2 + 1
      FOLD = .FALSE.
C
  500 CONTINUE
      DO 550 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 550 K = K1, K2, D5
      A = Y(K)/C
      X(K) = X(K) + S*A
      Y(K) = A
  550 CONTINUE
      IF (FOLD) GO TO 450
  600 CONTINUE
C
      M = N OVER 2*D2
      K0 = M + 1
      DO 650 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 650 K = K1, K2, D5
      J = K - M
      L = K + M
      A = 2.0*X(L)
      X(K) = X(K) + A
      Y(K) = A
      X(L) = X(J)
      Y(L) = -Y(J)
  650 CONTINUE
C
      RETURN
C
  700 CONTINUE
      WRITE (6, 1000) N
      STOP
C
 1000 FORMAT (/,'SDIAD N ODD.  N =', I10)
C
      END

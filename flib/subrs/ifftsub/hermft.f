      SUBROUTINE HERM FT(X, Y, N, DIM)
      INTEGER N
      INTEGER DIM(5)
      REAL X(1), Y(1)
C
C     HERMITIAN SYMMETRIC FOURIER TRANSFORM
C
C     GIVEN THE UNIQUE TERMS OF A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH
C     2N THIS SUBROUTINE CALCULATES THE 2N REAL NUMBERS WHICH ARE ITS
C     FOURIER TRANSFORM.  THE EVEN NUMBERED ELEMENTS OF THE TRANSFORM
C     (0, 2, 4, . . ., 2N-2) ARE RETURNED IN X AND THE ODD NUMBERED
C     ELEMENTS (1, 3, 5, . . ., 2N-1) IN Y.
C
C     A FINITE HERMITIAN SEQUENCE OF LENGTH 2N CONTAINS N + 1 UNIQUE
C     REAL NUMBERS AND N - 1 UNIQUE IMAGINARY NUMBERS.  FOR CONVENIENCE
C     THE REAL VALUE FOR X(N) IS STORED AT Y(0).
C
      REAL A, B, C, D, E, F, ANGLE, CO, SI, TWO N, TWO PI
      INTEGER NT, D2, D3, D4, D5
      INTEGER I, J, K, N OVER 2, I0, I1, I2
      INTEGER K1
C
      TWO PI = 6.2831853
      TWO N = FLOAT(2*N)
C
      NT = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
C
      DO 100 I0 = 1, NT, D3
      I1 = I0 + D4
      DO 100 I = I0, I1, D5
      A = X(I)
      B = Y(I)
      X(I) = A + B
      Y(I) = A - B
  100 CONTINUE
C
      N OVER 2 = N/2 + 1
      IF (N OVER 2 .LT. 2) GO TO 500
      DO 400 I0 = 2, N OVER 2
      ANGLE = TWO PI*FLOAT(I0-1)/TWO N
      CO = COS(ANGLE)
      SI = SIN(ANGLE)
      K = (N + 2 - 2*I0)*D2
      K1 = (I0 - 1)*D2 + 1
      DO 300 I1 = K1, NT, D3
      I2 = I1 + D4
      DO 200 I = I1, I2, D5
      J = I + K
      A = X(I) + X(J)
      B = X(I) - X(J)
      C = Y(I) + Y(J)
      D = Y(I) - Y(J)
      E = B*CO + C*SI
      F = B*SI - C*CO
      X(I) = A + F
      X(J) = A - F
      Y(I) = E + D
      Y(J) = E - D
  200 CONTINUE
  300 CONTINUE
  400 CONTINUE
C
      CALL CMPL FT (X, Y, N, DIM)
C
  500 CONTINUE
      RETURN
C
      END

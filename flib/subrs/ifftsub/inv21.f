      SUBROUTINE INV21(X,Y,N,D)
      INTEGER N,D(5)
      REAL X(1),Y(1)
C
C     INVERTS FOURIER TRANSFORM ALONG A SCREW
C     DIAD. THE RESULT IS SCALED BY N.
C
      INTEGER D1,D2,D3,D4,D5
      REAL A,B,C,C1,R,S,S1,PI
C
      PI = 3.1415927
C
      D1 = D(1)
      D2 = D(2)
      D3 = D(3)
      D4 = D(4)-1
      D5 = D(5)
C
      NOVER2 = N/2
      LL = N*D2
      KK = NOVER2*D2
      DO 100 J1 = 1,D1,D3
      J2 = J1+D4
      DO 100 J = J1,J2,D5
      L = LL+J
      K = KK+J
      X(L) = X(J)+X(K)
      X(K) = X(K)+Y(K)
      Y(L) = 0.0
100   Y(K) = 0.0
C
      C1 = COS(PI/FLOAT(N))
      S1 = SIN(PI/FLOAT(N))
      C = 1.0
      S = 0.0
      DO 200 I = 2, NOVER2
      KK = (N+2-2*I)*D2
      LL = (N+1-I)*D2
      R = C*C1-S*S1
      S = C*S1+S*C1
      C = R
      J1 = (I-1)*D2+1
      DO 150 J2 = J1,D1,D3
      J3 = J2+D4
      DO 150 J = J2,J3,D5
      L = J+LL
      K = J+KK
      X(L) = X(L)+X(J)+X(K)
      X(J) = X(J)+S*Y(J)
      X(K) = X(K)+S*Y(K)
      Y(J) = C*Y(J)
150   Y(K) = -C*Y(K)
200   CONTINUE
C
      CALL CMPL FT(X,Y,N,D)
C
      DO 300 I = 1,NOVER2
      KK = (N+1-2*I)*D2
      LL = KK+I*D2
      J1 = (I-1)*D2+1
      DO 250 J2 = J1,D1,D3
      J3 = J2+D4
      DO 250 J = J2,J3,D5
      K = J+KK
      L = J+LL
      A = X(J)-X(L)
      B = Y(J)+Y(L)
      X(J) = X(L)
      Y(J) = -Y(L)
      X(L) = X(K)+A
      Y(L) = Y(K)-B
      X(K) = A
250   Y(K) = B
300   CONTINUE
C
      M = N-2
      DO 400 I = 1,M
      K = I
320   J = K
      K = J/2
      IF(2*K.NE.J) K = N-1-K
      IF(K-I) 320,400,340
340   KK = (K-I)*D2
      J1 = I*D2+1
      DO 360 J2 = J1,D1,D3
      J3 = J2+D4
      DO 360 J = J2,J3,D5
      K = J+KK
      A = X(K)
      B = Y(K)
      X(K) =X(J)
      Y(K) = Y(J)
      X(J) = A
360   Y(J) = B
400   CONTINUE
C
      RETURN
C
      END

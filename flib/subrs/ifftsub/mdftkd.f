      SUBROUTINE MD FT KD (N, FACTOR, DIM, X, Y)
C     MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL DRIVER
C
      INTEGER N
      INTEGER FACTOR (10), DIM(5)
      REAL X(10), Y(10)
C
      INTEGER F, M, P, R, S
C
      S = DIM(2)
      F = 0
      M = N
  100 CONTINUE
      F = F + 1
      P = FACTOR(F)
      IF (P.EQ.0) RETURN
      M = M/P
      R = M*S
      IF (P.GT.8) GO TO 700
      GO TO (100, 200, 300, 400, 500, 800, 700, 600), P
      GO TO 800
C
  200 CONTINUE
      CALL R2 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), DIM)
      GO TO 100
C
  300 CONTINUE
      CALL R3 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., DIM)
      GO TO 100
C
  400 CONTINUE
      CALL R4 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., X(3*R+1), Y(3*R+1), DIM)
      GO TO 100
C
  500 CONTINUE
      CALL R5 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., X(3*R+1), Y(3*R+1), X(4*R+1), Y(4*R+1), DIM)
      GO TO 100
C
  600 CONTINUE
      CALL R8 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., X(3*R+1), Y(3*R+1), X(4*R+1), Y(4*R+1), X(5*R+1), Y(5*R+1),
     .X(6*R+1), Y(6*R+1), X(7*R+1), Y(7*R+1), DIM)
      GO TO 100
C
  700 CONTINUE
      CALL RP CFTK (N, M, P, R, X, Y, DIM)
      GO TO 100
C
  800 CONTINUE
      WRITE (6, 900)
      RETURN
  900 FORMAT (/,' TRANSFER ERROR DETECTED IN MDFTKD',//)
      END
      SUBROUTINE R2 CFTK (N, M, X0, Y0, X1, Y1, DIM)
C     RADIX 2 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(5)
      REAL X0 (10), Y0 (10), X1 (10), Y1 (10)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M2,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM2, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,C,IS,IU,RS,RU,S,TWOPI
      REAL FJM1, FM2
      DATA TWO PI/6.2831853/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M2=M*2
      FM2 = FLOAT(M2)
      M OVER 2=M/2+1
      MM2 = SEP*M2
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM2
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C=COS(ANGLE)
      S=SIN(ANGLE)
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      C=-C
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM2
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      RS=X0(K)+X1(K)
      IS=Y0(K)+Y1(K)
      RU=X0(K)-X1(K)
      IU=Y0(K)-Y1(K)
      X0(K)=RS
      Y0(K)=IS
      IF (ZERO) GO TO 300
      X1(K)=RU*C+IU*S
      Y1(K)=IU*C-RU*S
      GO TO 400
  300 CONTINUE
      X1(K)=RU
      Y1(K)=IU
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R3 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, DIM)
C     RADIX 3 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(5)
      REAL X0 (10), Y0 (10), X1 (10), Y1 (10), X2 (10), Y2 (10)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M3,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM3, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,A,B,C1,C2,S1,S2,T,TWOPI
      REAL I0,I1,I2,IA,IB,IS,R0,R1,R2,RA,RB,RS
      REAL FJM1, FM3
      DATA TWO PI/6.2831853/, A/-0.5/, B/0.86602540/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M3=M*3
      FM3 = FLOAT(M3)
      MM3 = SEP*M3
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM3
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=C1*A+S1*B
      S1=C1*B-S1*A
      C1=T
      T=C2*A-S2*B
      S2=-C2*B-S2*A
      C2=T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM3
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      R0=X0(K)
      I0=Y0(K)
      RS=X1(K)+X2(K)
      IS=Y1(K)+Y2(K)
      X0(K)=R0+RS
      Y0(K)=I0+IS
      RA=R0+RS*A
      IA=I0+IS*A
      RB=(X1(K)-X2(K))*B
      IB=(Y1(K)-Y2(K))*B
      IF (ZERO) GO TO 300
      R1=RA+IB
      I1=IA-RB
      R2=RA-IB
      I2=IA+RB
      X1(K)=R1*C1+I1*S1
      Y1(K)=I1*C1-R1*S1
      X2(K)=R2*C2+I2*S2
      Y2(K)=I2*C2-R2*S2
      GO TO 400
  300 CONTINUE
      X1(K)=RA+IB
      Y1(K)=IA-RB
      X2(K)=RA-IB
      Y2(K)=IA+RB
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R4 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, X3, Y3, DIM)
C     RADIX 4 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(5)
      REAL X0 (10), Y0 (10), X1 (10), Y1 (10)
      REAL X2 (10), Y2 (10), X3 (10), Y3 (10)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M4,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM4, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,C1,C2,C3,S1,S2,S3,T,TWOPI
      REAL I1,I2,I3,IS0,IS1,IU0,IU1,R1,R2,R3,RS0,RS1,RU0,RU1
      REAL FJM1, FM4
      DATA TWO PI/6.2831853/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M4=M*4
      FM4 = FLOAT(M4)
      MM4 = SEP*M4
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM4
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      C3=C2*C1-S2*S1
      S3=S2*C1+C2*S1
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=C1
      C1=S1
      S1=T
      C2=-C2
      T=C3
      C3=-S3
      S3=-T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM4
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      RS0=X0(K)+X2(K)
      IS0=Y0(K)+Y2(K)
      RU0=X0(K)-X2(K)
      IU0=Y0(K)-Y2(K)
      RS1=X1(K)+X3(K)
      IS1=Y1(K)+Y3(K)
      RU1=X1(K)-X3(K)
      IU1=Y1(K)-Y3(K)
      X0(K)=RS0+RS1
      Y0(K)=IS0+IS1
      IF (ZERO) GO TO 300
      R1=RU0+IU1
      I1=IU0-RU1
      R2=RS0-RS1
      I2=IS0-IS1
      R3=RU0-IU1
      I3=IU0+RU1
      X2(K)=R1*C1+I1*S1
      Y2(K)=I1*C1-R1*S1
      X1(K)=R2*C2+I2*S2
      Y1(K)=I2*C2-R2*S2
      X3(K)=R3*C3+I3*S3
      Y3(K)=I3*C3-R3*S3
      GO TO 400
  300 CONTINUE
      X2(K)=RU0+IU1
      Y2(K)=IU0-RU1
      X1(K)=RS0-RS1
      Y1(K)=IS0-IS1
      X3(K)=RU0-IU1
      Y3(K)=IU0+RU1
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R5 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, X3, Y3, X4, Y4,
     . DIM)
C     RADIX 5 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(5)
      REAL X0 (10), Y0 (10), X1 (10), Y1 (10), X2 (10), Y2 (10)
      REAL X3 (10), Y3 (10), X4 (10), Y4 (10)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M5,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM5, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,A1,A2,B1,B2,C1,C2,C3,C4,S1,S2,S3,S4,T,TWOPI
      REAL R0,R1,R2,R3,R4,RA1,RA2,RB1,RB2,RS1,RS2,RU1,RU2
      REAL I0,I1,I2,I3,I4,IA1,IA2,IB1,IB2,IS1,IS2,IU1,IU2
      REAL FJM1, FM5
      DATA TWO PI/6.2831853/, A1/0.30901699/, B1/0.95105652/,
     .      A2/-0.80901699/, B2/0.58778525/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M5=M*5
      FM5 = FLOAT(M5)
      MM5 = SEP*M5
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM5
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      C3=C2*C1-S2*S1
      S3=S2*C1+C2*S1
      C4=C2*C2-S2*S2
      S4=S2*C2+C2*S2
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=C1*A1+S1*B1
      S1=C1*B1-S1*A1
      C1=T
      T=C2*A2+S2*B2
      S2=C2*B2-S2*A2
      C2=T
      T=C3*A2-S3*B2
      S3=-C3*B2-S3*A2
      C3=T
      T=C4*A1-S4*B1
      S4=-C4*B1-S4*A1
      C4=T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM5
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      R0=X0(K)
      I0=Y0(K)
      RS1=X1(K)+X4(K)
      IS1=Y1(K)+Y4(K)
      RU1=X1(K)-X4(K)
      IU1=Y1(K)-Y4(K)
      RS2=X2(K)+X3(K)
      IS2=Y2(K)+Y3(K)
      RU2=X2(K)-X3(K)
      IU2=Y2(K)-Y3(K)
      X0(K)=R0+RS1+RS2
      Y0(K)=I0+IS1+IS2
      RA1=R0+RS1*A1+RS2*A2
      IA1=I0+IS1*A1+IS2*A2
      RA2=R0+RS1*A2+RS2*A1
      IA2=I0+IS1*A2+IS2*A1
      RB1=RU1*B1+RU2*B2
      IB1=IU1*B1+IU2*B2
      RB2=RU1*B2-RU2*B1
      IB2=IU1*B2-IU2*B1
      IF (ZERO) GO TO 300
      R1=RA1+IB1
      I1=IA1-RB1
      R2=RA2+IB2
      I2=IA2-RB2
      R3=RA2-IB2
      I3=IA2+RB2
      R4=RA1-IB1
      I4=IA1+RB1
      X1(K)=R1*C1+I1*S1
      Y1(K)=I1*C1-R1*S1
      X2(K)=R2*C2+I2*S2
      Y2(K)=I2*C2-R2*S2
      X3(K)=R3*C3+I3*S3
      Y3(K)=I3*C3-R3*S3
      X4(K)=R4*C4+I4*S4
      Y4(K)=I4*C4-R4*S4
      GO TO 400
  300 CONTINUE
      X1(K)=RA1+IB1
      Y1(K)=IA1-RB1
      X2(K)=RA2+IB2
      Y2(K)=IA2-RB2
      X3(K)=RA2-IB2
      Y3(K)=IA2+RB2
      X4(K)=RA1-IB1
      Y4(K)=IA1+RB1
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R8 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, X3, Y3, X4, Y4,
     . X5, Y5, X6, Y6, X7, Y7, DIM)
C     RADIX 8 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(5)
      REAL X0 (10), Y0 (10), X1 (10), Y1 (10), X2 (10), Y2 (10)
      REAL X3 (10), Y3 (10), X4 (10), Y4 (10), X5 (10), Y5 (10)
      REAL X6 (10), Y6 (10), X7 (10), Y7 (10)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M8,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM8, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,C1,C2,C3,C4,C5,C6,C7,E,S1,S2,S3,S4,S5,S6,S7,T,TWOPI
      REAL R1,R2,R3,R4,R5,R6,R7,RS0,RS1,RS2,RS3,RU0,RU1,RU2,RU3
      REAL I1,I2,I3,I4,I5,I6,I7,IS0,IS1,IS2,IS3,IU0,IU1,IU2,IU3
      REAL RSS0,RSS1,RSU0,RSU1,RUS0,RUS1,RUU0,RUU1
      REAL ISS0,ISS1,ISU0,ISU1,IUS0,IUS1,IUU0,IUU1
      REAL FJM1, FM8
      DATA TWO PI/6.2831853/, E/0.70710678/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M8=M*8
      FM8 = FLOAT(M8)
      MM8 = SEP*M8
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM8
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      C3=C2*C1-S2*S1
      S3=S2*C1+C2*S1
      C4=C2*C2-S2*S2
      S4=S2*C2+C2*S2
      C5=C4*C1-S4*S1
      S5=S4*C1+C4*S1
      C6=C4*C2-S4*S2
      S6=S4*C2+C4*S2
      C7=C4*C3-S4*S3
      S7=S4*C3+C4*S3
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=(C1+S1)*E
      S1=(C1-S1)*E
      C1=T
      T=S2
      S2=C2
      C2=T
      T=(-C3+S3)*E
      S3=(C3+S3)*E
      C3=T
      C4=-C4
      T=-(C5+S5)*E
      S5=(-C5+S5)*E
      C5=T
      T=-S6
      S6=-C6
      C6=T
      T=(C7-S7)*E
      S7=-(C7+S7)*E
      C7=T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM8
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      RS0=X0(K)+X4(K)
      IS0=Y0(K)+Y4(K)
      RU0=X0(K)-X4(K)
      IU0=Y0(K)-Y4(K)
      RS1=X1(K)+X5(K)
      IS1=Y1(K)+Y5(K)
      RU1=X1(K)-X5(K)
      IU1=Y1(K)-Y5(K)
      RS2=X2(K)+X6(K)
      IS2=Y2(K)+Y6(K)
      RU2=X2(K)-X6(K)
      IU2=Y2(K)-Y6(K)
      RS3=X3(K)+X7(K)
      IS3=Y3(K)+Y7(K)
      RU3=X3(K)-X7(K)
      IU3=Y3(K)-Y7(K)
      RSS0=RS0+RS2
      ISS0=IS0+IS2
      RSU0=RS0-RS2
      ISU0=IS0-IS2
      RSS1=RS1+RS3
      ISS1=IS1+IS3
      RSU1=RS1-RS3
      ISU1=IS1-IS3
      RUS0=RU0-IU2
      IUS0=IU0+RU2
      RUU0=RU0+IU2
      IUU0=IU0-RU2
      RUS1=RU1-IU3
      IUS1=IU1+RU3
      RUU1=RU1+IU3
      IUU1=IU1-RU3
      T=(RUS1+IUS1)*E
      IUS1=(IUS1-RUS1)*E
      RUS1=T
      T=(RUU1+IUU1)*E
      IUU1=(IUU1-RUU1)*E
      RUU1=T
      X0(K)=RSS0+RSS1
      Y0(K)=ISS0+ISS1
      IF (ZERO) GO TO 300
      R1=RUU0+RUU1
      I1=IUU0+IUU1
      R2=RSU0+ISU1
      I2=ISU0-RSU1
      R3=RUS0+IUS1
      I3=IUS0-RUS1
      R4=RSS0-RSS1
      I4=ISS0-ISS1
      R5=RUU0-RUU1
      I5=IUU0-IUU1
      R6=RSU0-ISU1
      I6=ISU0+RSU1
      R7=RUS0-IUS1
      I7=IUS0+RUS1
      X4(K)=R1*C1+I1*S1
      Y4(K)=I1*C1-R1*S1
      X2(K)=R2*C2+I2*S2
      Y2(K)=I2*C2-R2*S2
      X6(K)=R3*C3+I3*S3
      Y6(K)=I3*C3-R3*S3
      X1(K)=R4*C4+I4*S4
      Y1(K)=I4*C4-R4*S4
      X5(K)=R5*C5+I5*S5
      Y5(K)=I5*C5-R5*S5
      X3(K)=R6*C6+I6*S6
      Y3(K)=I6*C6-R6*S6
      X7(K)=R7*C7+I7*S7
      Y7(K)=I7*C7-R7*S7
      GO TO 400
  300 CONTINUE
      X4(K)=RUU0+RUU1
      Y4(K)=IUU0+IUU1
      X2(K)=RSU0+ISU1
      Y2(K)=ISU0-RSU1
      X6(K)=RUS0+IUS1
      Y6(K)=IUS0-RUS1
      X1(K)=RSS0-RSS1
      Y1(K)=ISS0-ISS1
      X5(K)=RUU0-RUU1
      Y5(K)=IUU0-IUU1
      X3(K)=RSU0-ISU1
      Y3(K)=ISU0+RSU1
      X7(K)=RUS0-IUS1
      Y7(K)=IUS0+RUS1
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE RP CFTK (N, M, P, R, X, Y, DIM)
C     RADIX PRIME MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, P, R, DIM(5)
      REAL X(R,P), Y(R,P)
C
      LOGICAL FOLD,ZERO
      REAL ANGLE,IS,IU,RS,RU,T,TWOPI,XT,YT
      REAL FU, FP, FJM1, FMP
      INTEGER J,JJ,K0,K,M OVER 2,MP,PM,PP,U,V
      INTEGER K1, K2, KK, L, L1, MMP, NT, SIZE, SEP
      INTEGER NS
C
      REAL AA (9,9), BB (9,9)
      REAL A (18), B (18), C (18), S (18)
      REAL IA (9), IB (9), RA (9), RB (9)
      DATA TWO PI/6.2831853/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M OVER 2=M/2+1
      MP=M*P
      FMP = FLOAT(MP)
      MMP = SEP*MP
      PP=P/2
      PM=P-1
      FP = FLOAT(P)
      FU = 0.0
      DO 100 U=1,PP
      FU = FU + 1.0
      ANGLE = TWO PI*FU/FP
      JJ=P-U
      A(U)=COS(ANGLE)
      B(U)=SIN(ANGLE)
      A(JJ)=A(U)
      B(JJ)=-B(U)
  100 CONTINUE
      DO 300 U=1,PP
      DO 200 V=1,PP
      JJ=U*V-U*V/P*P
      AA(V,U)=A(JJ)
      BB(V,U)=B(JJ)
  200 CONTINUE
  300 CONTINUE
C
      FJM1 = -1.0
      DO 1500 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FMP
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 700
      C(1)=COS(ANGLE)
      S(1)=SIN(ANGLE)
      DO 400 U=2,PM
      C(U)=C(U-1)*C(1)-S(U-1)*S(1)
      S(U)=S(U-1)*C(1)+C(U-1)*S(1)
  400 CONTINUE
      GO TO 700
  500 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      DO 600 U=1,PM
      T=C(U)*A(U)+S(U)*B(U)
      S(U)=-S(U)*A(U)+C(U)*B(U)
      C(U)=T
  600 CONTINUE
  700 CONTINUE
C
      DO 1400 KK = K0, NS, MMP
      DO 1340 L = KK, NT, L1
      K1 = L + SIZE
      DO 1320 K = L, K1, K2
      XT=X(K,1)
      YT=Y(K,1)
      RS=X(K,2)+X(K,P)
      IS=Y(K,2)+Y(K,P)
      RU=X(K,2)-X(K,P)
      IU=Y(K,2)-Y(K,P)
      DO 800 U=1,PP
      RA(U)=XT+RS*AA(U,1)
      IA(U)=YT+IS*AA(U,1)
      RB(U)=RU*BB(U,1)
      IB(U)=IU*BB(U,1)
  800 CONTINUE
      XT=XT+RS
      YT=YT+IS
      DO 1000 U=2,PP
      JJ=P-U
      RS=X(K,U+1)+X(K,JJ+1)
      IS=Y(K,U+1)+Y(K,JJ+1)
      RU=X(K,U+1)-X(K,JJ+1)
      IU=Y(K,U+1)-Y(K,JJ+1)
      XT=XT+RS
      YT=YT+IS
      DO 900 V=1,PP
      RA(V)=RA(V)+RS*AA(V,U)
      IA(V)=IA(V)+IS*AA(V,U)
      RB(V)=RB(V)+RU*BB(V,U)
      IB(V)=IB(V)+IU*BB(V,U)
  900 CONTINUE
 1000 CONTINUE
      X(K,1)=XT
      Y(K,1)=YT
      DO 1300 U=1,PP
      JJ=P-U
      IF (ZERO) GO TO 1100
      XT=RA(U)+IB(U)
      YT=IA(U)-RB(U)
      X(K,U+1)=XT*C(U)+YT*S(U)
      Y(K,U+1)=YT*C(U)-XT*S(U)
      XT=RA(U)-IB(U)
      YT=IA(U)+RB(U)
      X(K,JJ+1)=XT*C(JJ)+YT*S(JJ)
      Y(K,JJ+1)=YT*C(JJ)-XT*S(JJ)
      GO TO 1200
 1100 CONTINUE
      X(K,U+1)=RA(U)+IB(U)
      Y(K,U+1)=IA(U)-RB(U)
      X(K,JJ+1)=RA(U)-IB(U)
      Y(K,JJ+1)=IA(U)+RB(U)
 1200 CONTINUE
 1300 CONTINUE
 1320 CONTINUE
 1340 CONTINUE
 1400 CONTINUE
      IF (FOLD) GO TO 500
 1500 CONTINUE
C
      RETURN
      END

      SUBROUTINE D IP RP (PTS,SYM,P SYM,UN SYM,DIM,X,Y)
C     DOUBLE IN PLACE REORDERING PROGRAMME
C
      INTEGER PTS,P SYM
      REAL X (10), Y (10)
      INTEGER SYM(10), UN SYM(10), DIM(5)
C
      REAL T
      LOGICAL ONE MOD
      INTEGER MODULO (14)
      INTEGER DK,JJ,KK,LK,MODS,MULT,NEST,P UN SYM,TEST
      INTEGER NT, SEP, DELTA, P, P0, P1, P2, P3, P4, P5, SIZE
C
      INTEGER S (14), U (14)
      INTEGER A,B,C,D,E,F,G,H,I,J,K,L,M,N
      INTEGER BS,CS,DS,ES,FS,GS,HS,IS,JS,KS,LS,MS,NS
      INTEGER AL,BL,CL,DL,EL,FL,GL,HL,IL,JL,KL,LL,ML,NL
      EQUIVALENCE           (AL,U(1)),(BS,S(2)),(BL,U(2))
      EQUIVALENCE (CS,S(3)),(CL,U(3)),(DS,S(4)),(DL,U(4))
      EQUIVALENCE (ES,S(5)),(EL,U(5)),(FS,S(6)),(FL,U(6))
      EQUIVALENCE (GS,S(7)),(GL,U(7)),(HS,S(8)),(HL,U(8))
      EQUIVALENCE (IS,S(9)),(IL,U(9)),(JS,S(10)),(JL,U(10))
      EQUIVALENCE (KS,S(11)),(KL,U(11)),(LS,S(12)),(LL,U(12))
      EQUIVALENCE (MS,S(13)),(ML,U(13)),(NS,S(14)),(NL,U(14))
C
      NEST=14
C
      NT = DIM(1)
      SEP = DIM(2)
      P2 = DIM(3)
      SIZE = DIM(4) - 1
      P4 = DIM(5)
      IF (SYM(1).EQ.0) GO TO 500
      DO 100 J=1,NEST
      U(J)=1
      S(J)=1
  100 CONTINUE
      N=PTS
      DO 200 J=1,NEST
      IF (SYM(J).EQ.0) GO TO 300
      JJ=NEST+1-J
      U(JJ)=N
      S(JJ)=N/SYM(J)
      N=N/SYM(J)
  200 CONTINUE
  300 CONTINUE
C
      JJ=0
      DO 400 A=1,AL
      DO 400 B=A,BL,BS
      DO 400 C=B,CL,CS
      DO 400 D=C,DL,DS
      DO 400 E=D,EL,ES
      DO 400 F=E,FL,FS
      DO 400 G=F,GL,GS
      DO 400 H=G,HL,HS
      DO 400 I=H,IL,IS
      DO 400 J=I,JL,JS
      DO 400 K=J,KL,KS
      DO 400 L=K,LL,LS
      DO 400 M=L,ML,MS
      DO 400 N=M,NL,NS
      JJ=JJ+1
      IF (JJ.GE.N) GO TO 400
      DELTA = (N-JJ)*SEP
      P1 = (JJ-1)*SEP + 1
      DO 350 P0 = P1, NT, P2
      P3 = P0 + SIZE
      DO 350 P = P0, P3, P4
      P5 = P + DELTA
      T = X(P)
      X(P) = X(P5)
      X(P5) = T
      T = Y(P)
      Y(P) = Y(P5)
      Y(P5) = T
  350 CONTINUE
  400 CONTINUE
C
  500 CONTINUE
      IF (UN SYM(1).EQ.0) GO TO 1900
      P UN SYM=PTS/P SYM**2
      MULT=P UN SYM/UN SYM(1)
      TEST=(UN SYM(1)*UN SYM(2)-1)*MULT*P SYM
      LK=MULT
      DK=MULT
      DO 600 K=2,NEST
      IF (UN SYM(K).EQ.0) GO TO 700
      LK=LK*UN SYM(K-1)
      DK=DK/UN SYM(K)
      U(K)=(LK-DK)*P SYM
      MODS=K
  600 CONTINUE
  700 CONTINUE
      ONE MOD=MODS.LT.3
      IF (ONE MOD) GO TO 900
      DO 800 J=3,MODS
      JJ=MODS+3-J
      MODULO(JJ)=U(J)
  800 CONTINUE
  900 CONTINUE
      MODULO(2)=U(2)
      JL=(P UN SYM-3)*P SYM
      MS=P UN SYM*P SYM
C
      DO 1800 J=P SYM,JL,P SYM
      K=J
C
 1000 CONTINUE
      K=K*MULT
      IF (ONE MOD) GO TO 1200
      DO 1100 I=3,MODS
      K=K-(K/MODULO(I))*MODULO(I)
 1100 CONTINUE
 1200 CONTINUE
      IF (K.GE.TEST) GO TO 1300
      K=K-(K/MODULO(2))*MODULO(2)
      GO TO 1400
 1300 CONTINUE
      K=K-(K/MODULO(2))*MODULO(2)+MODULO(2)
 1400 CONTINUE
      IF (K.LT.J) GO TO 1000
C
      IF (K.EQ.J) GO TO 1700
      DELTA = (K-J)*SEP
      DO 1600 L=1,P SYM
      DO 1500 M=L,PTS,MS
      P1 = (M+J-1)*SEP + 1
      DO 1500 P0 = P1, NT, P2
      P3 = P0 + SIZE
      DO 1500 JJ = P0, P3, P4
      KK = JJ + DELTA
      T=X(JJ)
      X(JJ)=X(KK)
      X(KK)=T
      T=Y(JJ)
      Y(JJ)=Y(KK)
      Y(KK)=T
 1500 CONTINUE
 1600 CONTINUE
 1700 CONTINUE
 1800 CONTINUE
C
 1900 CONTINUE
      RETURN
      END

c       $Id$
c       
c       $Log$
c       
C       *CINTERP
C       
C       As above but for COMPLEX data
C       
C       
      SUBROUTINE CINTERP(ARRAY,BRAY,NXA,NYA,NXB,NYB,AMAT,
     &    XC,YC,XT,YT,SCALE)
      COMPLEX ARRAY(NXA,NYA),BRAY(NXB,NYB),A,B,C,D,V2,V4,V5,V6,V8
      DIMENSION AMAT(2,2)
C       
C       Calc inverse transformation
C       
      XCEN = NXB/2. + XT + 0.5
      YCEN = NYB/2. + YT + 0.5
      XCO = XC + 0.5
      YCO = YC + 0.5
      DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
      A11 =  AMAT(2,2)/DENOM
      A12 = -AMAT(1,2)/DENOM
      A21 = -AMAT(2,1)/DENOM
      A22 =  AMAT(1,1)/DENOM
C       
C       Loop over output image
C       
      DO 200 IY = 1,NYB
        DYO = IY - YCEN
        DO 100 IX = 1,NXB
          DXO = IX - XCEN
          XP = A11*DXO + A12*DYO + XCO
          YP = A21*DXO + A22*DYO + YCO
          IXP = NINT(XP)
          IYP = NINT(YP)
          BRAY(IX,IY) = 0.0
          IF (IXP .LT. 1 .OR. IXP .GT. NXA) GOTO 100
          IF (IYP .LT. 1 .OR. IYP .GT. NYA) GOTO 100
C           
C           Do quadratic interpolation
C           
          DX = XP - IXP
          DY = YP - IYP
          IXPP1 = IXP + 1
          IXPM1 = IXP - 1
          IYPP1 = IYP + 1
          IYPM1 = IYP - 1
          IF (IXPM1 .LT. 1) IXPM1 = 1
          IF (IYPM1 .LT. 1) IYPM1 = 1
          IF (IXPP1 .GT. NXA) IXPP1 = NXA
          IF (IYPP1 .GT. NYA) IYPP1 = NYA
C           
C           Set up terms for quadratic interpolation
C           
          V2 = ARRAY(IXP, IYPM1)
          V4 = ARRAY(IXPM1, IYP)
          V5 = ARRAY(IXP, IYP)
          V6 = ARRAY(IXPP1, IYP)
          V8 = ARRAY(IXP, IYPP1)
C           
          A = (V6 + V4)*.5 - V5
          B = (V8 + V2)*.5 - V5
          C = (V6 - V4)*.5
          D = (V8 - V2)*.5
C           
          BRAY(IX,IY) = SCALE*(A*DX*DX + B*DY*DY + C*DX + D*DY + V5)
C           
100     CONTINUE
200   CONTINUE
C       
      RETURN
      END

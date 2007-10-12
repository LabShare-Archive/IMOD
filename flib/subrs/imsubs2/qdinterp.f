c       $Id$
c       
c       $Log$
c
c       new version of interp sets non-existent cells to mean intensity DMEAN
c       also prevents quadratic interpolation from generating densities outside
c       the original range.  Note that if the results from this quadratic
c       interpolation, and from a simpler bilinear interpolation, are compared
c       with the original image, the quadratic appears to give a more faithful
c       reproduction of the original.
c       modified by mast (DNM) 8/88
C       *QDINTERP.FOR***************************************************
C       
C       This subroutine will perform coordinate transformations
C       (rotations,translations, etc.) by bilinear interpolation.
C       
C       BRAY = T[ ARRAY ]*scale
C       
C       ARRAY   - The input image array
C       BRAY    - The output image array
C       NXA,NYA - The dimensions of ARRAY
C       NXB,NYB - The dimensions of BRAY
C       AMAT    - A 2x2 matrix to specify rotation,scaling,skewing
C       XC,YC   - The cooridinates of the Center of ARRAY
C       XT,YT   - The translation to add to the final image. The
C       center of the output array is normally taken as
C       NXB/2, NYB/2
C       SCALE   - A multiplicative scale actor for the intensities
C       DMEAN   - Mean intensity of image- dnm added to use this instead of 0
C       
C       Xo = a11(Xi - Xc) + a12(Yi - Yc) + NXB/2. + XT
C       Yo = a21(Xi - Xc) + a22(Yi - Yc) + NYB/2. + YT
C       
      SUBROUTINE QDINTERP(ARRAY,BRAY,NXA,NYA,NXB,NYB,AMAT,
     &    XC,YC,XT,YT,SCALE,DMEAN)
      DIMENSION ARRAY(NXA,NYA),BRAY(NXB,NYB),AMAT(2,2)
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
c           DNM replaced the default value of 0 with DMEAN, mean intensity
          BRAY(IX,IY) = DMEAN
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
c           DNM: find min and max of all 5 points
          vmax=max(v2,v4,v5,v6,v8)
          vmin=min(v2,v4,v5,v6,v8)
C           
          A = (V6 + V4)*.5 - V5
          B = (V8 + V2)*.5 - V5
          C = (V6 - V4)*.5
          D = (V8 - V2)*.5
C           
          dennew = SCALE*(A*DX*DX + B*DY*DY + C*DX + D*DY + V5)
c           DNM: limit the new density to between the min and max of original points
          if(dennew.gt.vmax)dennew=vmax
          if(dennew.lt.vmin)dennew=vmin
          bray(ix,iy)=dennew
C           
100     CONTINUE
200   CONTINUE
C       
      RETURN
      END

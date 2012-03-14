c       SCALE takes the minimum and maximum data values and finds "nice"
c       axis limits that contain them, as specified by the lower limit XLO
c       and an increment DX, 1/10 of the axis length.
c       
      SUBROUTINE SCALE(XMIN,XMAX,DX,XLO)
      DIMENSION RANGE(10)
      DATA RANGE/1.,1.5,2.,2.5,3.,4.,5.,6.,8.,10./
      XLO=XMIN
      XLAS=1.E30
      DXLAS=1.E30
15    IF(XMAX.LE.XLO)XMAX=XLO+1.
      DXLOG=ALOG10(XMAX-XLO)
      LOGEXP=DXLOG
      IF(DXLOG.LT.0.)LOGEXP=LOGEXP-1
      FRACT=(XMAX-XLO)/10.**(LOGEXP)
      DO 20 K=1,10
        IF(FRACT.LE.RANGE(K))GO TO 25
20    CONTINUE
25    DX=RANGE(K)*10.**(LOGEXP-1)
      XLO=DX*IFIX(XMIN/DX)
      IF(XLO.GT.XMIN)XLO=XLO-DX
      IF(XLO.EQ.XLAS.AND.DX.EQ.DXLAS)RETURN
      XLAS=XLO
      DXLAS=DX
      IF(XLO+10.*DX.LT.XMAX)GO TO 15
      RETURN
      END

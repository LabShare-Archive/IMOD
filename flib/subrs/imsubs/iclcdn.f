C       *ICLCDN
C       
C       Subroutine to calculate the min/max/ & mean MODULI
C       for a portiion of an array. Array is assumed to be
C       a COMPLEX image array.
C       
C       MX,MY           : Dimensions of COMPLEX ARRAY
C       NX1,NX2         : Start and stop Column numbers (in COMPLEX if 3,4)
C       NY1,NY2         : Start and stop Line numbers
C       DMIN,DMAX,DMEAN : Min, Max, and Mean density values for the selected
C       : area of ARRAY
C       
C       
      SUBROUTINE ICLCDN(ARRAY,MX,MY,NX1,NX2,NY1,NY2,DMIN,DMAX,DMEAN)
      COMPLEX ARRAY(MX,MY)
C       
      DMIN = 1.E10
      DMAX = -1.E10
      DMEAN = 0.0
      DO 200 IY = NY1,NY2
        DO 100 IX = NX1,NX2
          VAL = CABS(ARRAY(IX,IY))
          DMEAN = DMEAN + VAL
          IF (VAL .LT. DMIN) DMIN = VAL
          IF (VAL .GT. DMAX) DMAX = VAL
100     CONTINUE
200   CONTINUE
C       
      DMEAN = DMEAN/((NX2 - NX1 + 1)*(NY2 - NY1 + 1))
C       
      RETURN
      END

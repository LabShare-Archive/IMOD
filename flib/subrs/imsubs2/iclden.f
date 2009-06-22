C*ICLDEN
C
C	Subroutine to calculate the min/max/ & mean densities
C	for a portion of an array. Array is assumed to be
C	a real image array.
C
C	MX,MY		: Dimesnions of ARRAY
C			: for complex numbers (MODES 3 & 4)
C			: MX MUST be multiplied by 2 (ie # of REALS)
C	NX1,NX2		: Start and stop Column numbers (in COMPLEX if 3,4)
C	NY1,NY2		: Start and stop Line numbers
C	DMIN,DMAX,DMEAN : Min, Max, and Mean density values for the selected
C			: area of ARRAY
C	  DNM 9/23/01: compute sub-sums to retain accuracy with big images
C
	SUBROUTINE ICLDEN(ARRAY,MX,MY,NX1,NX2,NY1,NY2,DMIN,DMAX,DMEAN)
        implicit none
        integer*4 MX,MY,NX1,NX2,NY1,NY2
	real*4 ARRAY(MX,MY),DMIN,DMAX,DMEAN
        integer*4 ix, iy
        real*4 val, tsum
C
	DMIN = 1.E30
	DMAX = -1.E30
	DMEAN = 0.0
	DO IY = NY1,NY2
	  tsum = 0.
	  DO IX = NX1,NX2
	    VAL = ARRAY(IX,IY)
	    tsum = tsum + VAL
	    IF (VAL .LT. DMIN) DMIN = VAL
	    IF (VAL .GT. DMAX) DMAX = VAL
          enddo
	  dmean = dmean + tsum
        enddo
C
	DMEAN = DMEAN/(int(NX2 - NX1 + 1, kind=8)*(NY2 - NY1 + 1))
C
	RETURN
	END

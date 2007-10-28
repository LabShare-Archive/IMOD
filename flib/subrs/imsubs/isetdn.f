C**********************************************************************
C       
C       *ISETDN
c       $Id$
c       $Log$
c
C       
C       Scales densities appropriately for the disk storage
c       mode, then calculate and return min/max/ & mean densities,
C       for a portion of an array.  For compatibility with ICLDEN and
c       nothing else, column and line numbers start at 1, not 0 !!!
C       
C       MX,MY           : Dimensions of ARRAY
c       MODE            : Storage mode of file: 0-2 or 9-15 only
C       NX1,NX2         : Start and stop Column numbers
C       NY1,NY2         : Start and stop Line numbers
C       DMIN,DMAX,DMEAN : Min, Max, and Mean density values for the selected
C       : area of ARRAY
C       
C       
      SUBROUTINE ISETDN(ARRAY,MX,MY,MODE,NX1,NX2,NY1,NY2,DMIN,DMAX,DMEAN)
      implicit none
      integer*4 MX,MY,MODE,NX1,NX2,NY1,NY2,ix,iy
      real*4 ARRAY(MX,MY), DMIN,DMAX,DMEAN,DMINin,DMAXin,val,tsum,sclfac
C       
      if(mode.eq.6)then
        dmax=65530.                             !unisgned integer*2 mode
      elseif(mode.eq.1)then
        dmax=32767.                             !integer*2 mode
      elseif(mode.ge.9.and.mode.le.15)then
        dmax=2**mode-1                          !bit modes
      else
        dmax=255.                               !covers integer*1 mode and real mode(s)
      endif
      DMINin = 1.E30
      DMAXin = -1.E30
      DMEAN = 0.0
      DO IY = NY1,NY2
        DO IX = NX1,NX2
          VAL = ARRAY(IX,IY)
          IF (VAL .LT. DMINin) DMINin = VAL
          IF (VAL .GT. DMAXin) DMAXin = VAL
        enddo
      enddo
C       
c       the 0.99999 is to prevent occasional overflow on scaling
      sclfac=0.99999*dmax/(dmaxin-dminin)
      do iy=ny1,ny2
        tsum = 0.
        do ix=nx1,nx2
          val = sclfac*(array(ix,iy)-dminin)
          tsum=tsum+val
          array(ix,iy)=val
        enddo
          dmean=dmean+tsum
      enddo
      DMEAN = DMEAN/((NX2 - NX1 + 1.)*(NY2 - NY1 + 1.))
C       
      dmin=0.
      RETURN
      END

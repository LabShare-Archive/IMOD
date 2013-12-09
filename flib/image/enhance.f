C       *ENHANCE.FOR************************************************************
C       *
C       This is a two-dimensional gaussian bandpass filter              *
C       program for image enhancement purposes.                         *
C       For three-dimensional images, each image in the stack           *
C       is separately bandpass filtered.                                *
C       *
C       The filter is in the form of a gaussian highpass filter *
C       given by:  (if Sigma1 is positive)                              *
C       (1. - exp(-r**2/(2*Sigma1**2)))                         *
C       multiplied by a gaussian-edged band-pass filter. This           *
C       filter is flat between Radius1 --> Radius2 and decays           *
C       symmetrically as a gaussian:                                    *
C       exp(-(r-Radius)**2/(2.*sigma2**2))                              *
C       *
C       The units are in fractional reciprocal lattice units,           *
C       that is r goes from 0-->sqrt(2)   (0-->.5 on each axis)         *
C       *
C       If either Sigma = 0, then that part of the filter is removed!   *
C       *
C       If Sigma1 is negative, the first filter is second derivative    *
C       of a gaussian, Del-squared G, with formula                      *
C       r**2*exp(-r**2/(2.*Sigma1**2))                          *
C       This filter alone is bandpass with a peak at 1.414*|Sigma1|, so *
C       Sigma2 and the Radii can be set to zero                         *
C       *
C       If Sigma2 is negative, the second filter is inverted (1 minus   *
C       the Gaussian band-pass filter).  This filter is then multiplied *
C       by the filter specified by sigma1 (if any).                     *
C       *
c       If Radius1 is negative, then the first filter is 0 out to 
c       |Radius1| and rises as an inverted gaussian from that point:
c       (1. - exp(-(r-|Radius1|)**2/(2.*Sigma1**2)))
c       The effective Radius1 for the second filter is then 0.          *
C       *
C       Several modes of operation are possible:                        *
C       *
C       Gaussian low-pass filter (temperature factor)                   *
C       :  Sigma1 & Radii = 0, use Sigma2                       *
C       *
C       Gaussian bandpass centered at Radius                            *
C       :  Sigma1=0,            use Radius1=Radius2 & Sigma2    *
C       *
C       Gaussian-edged bandpass between Radius1 & Radius2               *
C       :  Sigma1=0,            use Radius1,Radius2 & Sigma2    *
C       *
C       Gaussian bandpass (low-pass + high-pass)                        *
C       : Radii = 0,            use Sigma1 & Sigma2             *
C       *
c       See man page for other details                                *
C       *
C       *
C       Version 1.10    27.MAY.82       DAA             FOR VAX         *
C       Version 1.11    02.JUNE.82      DAA             FOR VAX         *
C       Version 1.12    10.JUNE.82      DAA             FOR VAX         *
C       Version 1.13    23.JULY.82      DAA             FOR VAX         *
C       Update  1.13    18.November.82  DAA             FOR VAX         *
C       Bug fix         14.July.88      DNM             FOR uVAX        *
C       Bit mode        09.August.88    DNM             FOR uVAX        *
C       Del-squared G   01.September.88 DNM             FOR uVAX        *
C       Inverted filter 26.April.89     DNM             FOR uVAX        *
C       Ported to unix  07.December.94  DNM             FOR SGI         *
C       *
C************************************************************************
c       $Id$
c       
      COMMON//NX,NY,NZ
      DIMENSION CTF(8193),NXYZ(3),MXYZ(3),TITLE(20)
      EQUIVALENCE (NX,NXYZ)
      character*320 filin,filout
      real*4, allocatable :: array(:)
      character*80 titlech
C       
      WRITE(6,1000)
1000  FORMAT(' ENHANCE: GAUSSIAN BANDPASS FILTER PROGRAM  V1.13',/)
C       
      call getinout(2,filin,filout)
      CALL IMOPEN(1,filin,'RO')
      CALL IMOPEN(2,filout,'NEW')
      NY2 = NY/2
      SIGMA1 = 0.0
      SIGMA2 = 0.0
      RADIUS = 0.0
      IORIG = 0.0
      WRITE(6,1100)
1100  FORMAT(/,' Enter Sigma1,2, Radius1,2: ',$)
      READ(5,*) SIGMA1,SIGMA2,RADIUS1,RADIUS2
      WRITE(6,1200)
1200  FORMAT(' Reset origin to original value (0-no, 1-yes): ',$)
      READ(5,*,END=2) IORIG
2     WRITE(6,1500) SIGMA1,SIGMA2,RADIUS1,RADIUS2
1500  FORMAT(//,' SIGMAS & RADII for bandpass filter = ',4F10.4)
      IF (IORIG .EQ. 1) WRITE(6,1600)
1600  FORMAT(//,' ****    Origin is reset to original value ***'//)
      write(titlech,2000) SIGMA1,SIGMA2,RADIUS1,RADIUS2,IORIG
2000  FORMAT('ENHANCE: Bandpass Sigmas,Radii,Iorig= ',4F9.4,2X,I1)
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
C       
C       Take care of headers
C       
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL ITRHDR(2,1)
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)

      call setctfwsr(sigma1,sigma2,radius1,radius2,ctf,nx,ny,delta)
      IF (IORIG .EQ. 1) CTF(1) = 1.0
C       
C       
C       Loop over all sections is stack
C       
      NX1 = NX - 1
      NY1 = NY - 1
      NX2 = NX + 2
      TMIN =  1.E10
      TMAX = -1.E10
      TMEAN = 0.0
c       DNM: switch to array allocation
      allocate(array(nx2*ny), stat = iz)
      IF (iz .ne. 0) then
        write(*, '(/,a)')'ERROR: enhance - failed to allocate large array for image'
        call exit(1)
      endif
      DO 300 IZ = 1,NZ
C         
c         print *,'reading section'
        CALL IRDPAS(1,ARRAY,NX2,NY,0,NX1,0,NY1,*99)
c         print *,'doing filter'
        CALL TODFFT(ARRAY,NX,NY,0)
C         
C         APPLY FILTER FUNCTION
C         
        call filterpart(array,array,nx,ny,ctf,delta)
C         
        CALL TODFFT(ARRAY,NX,NY,1)
c         dnm: every mode but 2 needs rescaling
        if(mode.ne.2)then
          CALL ISETDN(ARRAY,NX2,NY,mode,1,NX,1,NY,DMIN,DMAX,DMEAN)
        else
          CALL ICLDEN(ARRAY,NX2,NY,1,NX,1,NY,DMIN,DMAX,DMEAN)
        endif
c         print *,'writing section'
c         dnm replaced with call to repack then write whole section
        CALL IREPAK(array,ARRAY,NX2,NY,0,NX1,0,NY1)
        call iwrsec(2,array)
C         
        IF (DMIN .LT. TMIN) TMIN = DMIN
        IF (DMAX .GT. TMAX) TMAX = DMAX
        TMEAN = TMEAN + DMEAN
        JZ = IZ - 1
        IF (NZ .GT. 1) WRITE(6,2500) JZ,DMIN,DMAX,DMEAN
300   CONTINUE
2500  FORMAT(' Section # ',I4,' Min,Max,Mean density = ',3F12.4)
2700  FORMAT(//' Overall Min,Max,Mean density = ',3F12.4)
C       
      TMEAN = TMEAN/NZ
      WRITE(6,2700) TMIN,TMAX,TMEAN
      CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
C       
      CALL EXIT(0)
99    print *,'ERROR: ENHANCE - END-OF-FILE ERROR ON READ'
      call exit(1)
      END

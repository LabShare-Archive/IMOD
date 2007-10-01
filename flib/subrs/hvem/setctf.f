c       SETCTF gets filter parameters from the user and sets up the contrast
c       transfer function in the array CTF, which should be dimensioned
c       at least 8193.  The real image size is NX by NY, and the step
c       size between CTF values is returned in DELTA, or 0 if no filtering
c       is selected.

      subroutine setctf(ctf,nx,ny,delta)
      dimension ctf(*)
      data sigma1,sigma2,radius1,radius2/0.,0.,0.,0./
15    WRITE(6,1100)
1100  FORMAT(' Sigma1, Sigma2, Radius1, Radius2: ',$)
      READ(5,*) SIGMA1,SIGMA2,RADIUS1,RADIUS2
      call setctfwsr(SIGMA1,SIGMA2,RADIUS1,RADIUS2,ctf,nx,ny,delta)
      return
      end


c       SETCTFWSR takes the filter parameters SIGMA1,2 and RADIUS1,2 and sets
c       up the CTF, with scaling so that mean of all but the zero element is 1
c       
      subroutine setctfwsr(SIGMA1,SIGMA2,RADIUS1,RADIUS2,ctf,nx,ny,
     &    delta)
      implicit none
      real*4 ctf(*),SIGMA1,SIGMA2,RADIUS1,RADIUS2,delta,sum,scl
      integer*4 nx,ny,nsize,j

      call setctfnoscl(SIGMA1,SIGMA2,RADIUS1,RADIUS2,ctf,nx,ny,delta,nsize)
      if (delta .eq. 0.) return
      SUM = 0.0
      do j=2,nsize
        SUM = SUM + CTF(J)
      enddo
C       
      SCL = (nsize-1)/SUM
      DO J = 2,NSIZE
        CTF(J) = CTF(J)*SCL
      enddo
      return
      end

c       SETCTFWSRNOSCL takes the filter parameters SIGMA1,2 and RADIUS1,2 and
c       sets up the CTF, with no scaling 
c       
      subroutine setctfnoscl(SIGMA1,SIGMA2,RADIUS1,RADIUS2,ctf,nx,ny,
     &    delta,nsize)
      implicit none
      real*4 ctf(*),SIGMA1,SIGMA2,RADIUS1,RADIUS2,delta
      integer*4 nx,ny,nsize,j
      real*4 alpha,beta1,beta2,asize,s,scl,sum,radius1p, radius1n, ssqrd

      delta=0.
      if(sigma1.eq.0..and.sigma2.eq.0.)return
C       
C       SET UP CONTRAST TRANSFER FUNCTION
C       
      ALPHA = 0.0
      BETA1 = 0.0
      BETA2 = 0.0
      NSIZE = MIN(8192,MAX(1024,2*MAX(NX,NY)))
      ASIZE = NSIZE
      NSIZE = NSIZE + 1
      IF (abs(SIGMA1) .GT. 1.E-6) ALPHA = -0.5/SIGMA1**2
      IF (abs(SIGMA2) .GT. 1.E-6) BETA1  = -0.5/SIGMA2**2
      beta2=beta1
c       
c       Yes, delta is twice as big as it should be and values are generated
c       out to 1.41.  It should really be 0.71/asize.  What to do?
c       
      DELTA = 1.0/(.71*ASIZE)
      if (radius1 .ge. 0.) then
        radius1p = radius1
        radius1n = 0.
      else
        radius1n = -radius1
        radius1p = 0.
      endif
      S = 0.0
      DO J = 1,NSIZE
        IF (S .LT. RADIUS1P) THEN
          CTF(J) = EXP(BETA1*(S - RADIUS1P)**2)
        ELSE IF (S .GT. RADIUS2) THEN
          CTF(J) = EXP(BETA2*(S - RADIUS2)**2)
        ELSE
          CTF(J) = 1.0
        END IF
        S = S + DELTA
        if(sigma2.lt.-1.e-6) ctf(j)=1. - ctf(j)
      enddo
      IF (SIGMA1 .GT. 1.E-6)then
        s=0.
        do j=1,nsize
          if (s .lt. radius1n) then
            ctf(j) = 0.
          else
            CTF(J) = CTF(J)*(1.0 - EXP(ALPHA*(S - radius1n)**2))
          endif
          S = S + DELTA
        enddo
      else if (sigma1 .lt. -1.e-6)then
        s=0.
        do j=1,nsize
          ssqrd=s*s
          ctf(j)=ctf(j)*ssqrd*exp(alpha*ssqrd)
          S = S + DELTA
        enddo
      endif
      return
      end

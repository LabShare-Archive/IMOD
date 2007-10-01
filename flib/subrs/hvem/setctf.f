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

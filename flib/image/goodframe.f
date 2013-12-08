c       GOODFRAME reads a number and prints out the next highest valid
c       frame size for an FFT.  It requires no prime factor greater than 19
c       regardless of whether FFTW is being used, because blendmont does the
c       same
c       
c       $Id$
c       
      implicit none
      integer*4 nices(10)
      character*120 arg
      integer*4 nx,nxp,i,ndo
      integer*4 iargc, niceframe
      if (iargc() .eq. 0) then
        read(5,*)nx
        nx=nx+mod(nx,2)
        nxp=niceframe(nx,2,19)
        print *,nxp
        call exit(0)
      endif
      ndo = min(iargc(), 10)
      do i = 1, ndo
        call getarg(i, arg)
        read(arg, *)nx
        nices(i) = niceframe(nx,2,19)
      enddo
      print *,(nices(i),i=1,ndo)
      call exit(0)
      end

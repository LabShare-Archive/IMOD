c	  GOODFRAME reads a number and prints out the next highest valid
c	  frame size for an FFT
c
      read(5,*)nx
      nxp=niceframe(nx,2,19)
      print *,nxp
      call exit(0)
      end

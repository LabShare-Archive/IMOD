c	  GOODFRAME reads a number and prints out the next highest valid
c	  frame size for an FFT
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
      read(5,*)nx
      nx=nx+mod(nx,2)
      nxp=niceframe(nx,2,19)
      print *,nxp
      call exit(0)
      end

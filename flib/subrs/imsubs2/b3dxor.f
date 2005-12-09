c	  B3DXOR returns the exclusive or of its two arguments
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 1.1  2005/12/08 15:56:46  mast
c	  Addition for use of gfortran
c	
c
	logical function b3dxor(a, b)
	logical a, b
	b3dxor = (a .and. .not.b) .or. (b .and. .not.a)
	return
	end

c	  XFCOPY copies one transform to another; replaces structure assignment
	subroutine xfcopy(f,g)
	real*4 f(*),g(*)
	do i=1,6
	  g(i)=f(i)
	enddo
	end

c	  returns the complementary error function erfc(x) with fractional
c	  error everywhere less than 1.2e-7
c
	real*4 function erfcc(x)
	implicit none
	real*4 x,z,t
c
	z=abs(x)
	t=1./(1.+0.5*Z)
	erfcc=t*exp(-z*z-1.26551223+T*(1.00002368+T*(.37409196+
     &	    t*(.09678418+t*(-.18628806+t*(.27886807+t*(-1.13520398+
     &	    t*(1.48851587+t*(-.82215223+t*.17087277)))))))))
	if(x.lt.0) erfcc=2.-erfcc
	return
	end


	function gammln(xx)
c	  returns the value ln[gamma(XX)] for XX > 0.  Full accuracy is
c	  obtained for XX >1.  For 0 < XX < 1, the reflection formula (6.1.4)
c	  can be used first
	real*8 cof(6),stp,half,one,fpf,x,tmp,ser
c	  internal arithmetic will be done in double precision, a nicety that
c	  you can omit if 5-figure accuracy is good enough
	data cof,stp/76.18009173D0,-86.50532033d0,24.01409822d0,
     &	    -1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
	data half,one,fpf/0.5d0,1.0d0,5.5d0/
	x=xx-one
	tmp=x+fpf
	tmp=(x+half)*log(tmp)-tmp
	ser=one
	do 11 j=1,6
	  x=x+one
	  ser=ser+cof(j)/x
11	continue
	gammln=tmp+log(stp*ser)
	return
	end

	function trnc(xx)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	save /imparm/
	real*4 safe2(3)/.35,1.2,2.65/
	idot=nint(xx*upi)
	trnc=(3*(idot/3) + safe2(mod(idot,3)+1))/upi
c	trnc=(ifix((xx+0.0001)*upi))/upi+safe
	return
	end


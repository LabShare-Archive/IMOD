	function betai(a,b,x)
c	  returns the incomplete beta function I (sub X) (A,B)
c	  DNM 11/10/01: changed from pause statement in deference to f95
	if(x.lt.0..or.x.gt.1.)print *,'bad argument X in BETAI'
	if(x.eq.0..or.x.eq.1.)then
	  bt=0.
	else					!factors in front of continued
	  bt=exp(gammln(a+b)-gammln(a)-gammln(b) !fraction
     &	      +a*alog(x)+b*alog(1.-x))
	endif
	if(x.lt.(a+1.)/(a+b+2.))then		!use continued fraction direct
	  betai=bt*betacf(a,b,x)/a
	  return
	else
	  betai=1.-bt*betacf(b,a,1.-x)/b	!use continued fraction after
	  return				!making symmetry transformation
	endif
	end

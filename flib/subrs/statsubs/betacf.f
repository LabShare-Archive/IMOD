	function betacf(a,b,x)
c	  continued fraction for incomplete beta function, used by BETAI
	parameter (itmax=100,eps=3.e-7)
	am=1.
	bm=1.
	az=1.
	qab=a+b					!these Q's will be used in 
	qap=a+1.				!factors which occur in the
	qam=a-1.				!coefficients (6.3.6)
	bz=1.-qab*x/qap
	do 11 m=1,itmax				!continued fraction evaluation
	  em=m					!by the recurrence method
	  tem=em+em				!(5.2.5)
	  d=em*(b-m)*x/((qam+tem)*(a+tem))
	  ap=az+d*am				!even step of recurrence
	  bp=bz+d*bm
	  d=-(a+em)*(qab+em)*x/((a+tem)*(qap+tem))
	  app=ap+d*az				!odd step of the recurrence
	  bpp=bp+d*bz
	  aold=az				!save the old answer
	  am=ap/bpp				!renormalize to prevent 
	  bm=bp/bpp				!overflows
	  az=app/bpp
	  bz=1.
	  if(abs(az-aold).lt.eps*abs(az))go to 1 !are we done?
11	continue
c	  DNM 11/10/01: changed from a pause statement in deference to f95
	print *,'betacf: A or B too big, or ITMAX too small'
1	betacf=az
	return
	end

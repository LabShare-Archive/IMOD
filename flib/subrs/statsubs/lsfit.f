c	  LSFIT fits a straight line to the N points in arrays X and Y by the
c	  method of least squares, returning SLOPE, intercept BINT, and
c	  correlation coeficient RO
c
	subroutine lsfit(x,y,n,slope,bint,ro)
	dimension x(*),y(*)
	call lsfits(x,y,n,slope,bint,ro,se,sb,sa)
	return
	end

	subroutine lsfits(x,y,n,slope,bint,ro,se,sb,sa)
	dimension x(*),y(*)
	sx=0.
	sy=0.
	do 10 i=1,n
	  sx=sx+x(i)
	  sy=sy+y(i)
10	continue
	xbar=sx/n
	ybar=sy/n
	sxpsq=0.
	sxyp=0.
	sypsq=0.
	do 12 i=1,n
	  xp=x(i)-xbar
	  yp=y(i)-ybar
	  sxpsq=sxpsq+xp**2
	  sypsq=sypsq+yp**2
	  sxyp=sxyp+xp*yp
12	continue
	d=n*sxpsq
	slope=sxyp/sxpsq
	bint=(ybar*sxpsq-xbar*sxyp)/sxpsq
	roden=sqrt(sxpsq*sypsq)
	ro=1.
	if(roden.ne.0..and.abs(sxyp).le.abs(roden))ro=sxyp/roden
	se=0.
	sxy=sxyp+n*xbar*ybar
	sysq=sypsq+n*ybar**2
	if(n.gt.2)se=sqrt(max(0.,(sysq-bint*sy-slope*sxy)/(n-2)))
	sa=se*sqrt(1./n+(sx**2/n)/d)
	sb=se/sqrt(d/n)
	return
	end

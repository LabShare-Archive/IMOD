c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
c	  !Fits a straight line to the [N] points in arrays [X] and [Y] by the
c	  method of least squares, returning [SLOPE], intercept [BINT], and
c	  correlation coeficient [RO]
c
	subroutine lsfit(x,y,n,slope,bint,ro)
	implicit none
	real*4 x(*),y(*)
	real*4 slope,bint,ro,se,sb,sa,xpred,ypred,prederr
	integer*4 n
	xpred = x(1)
	call lsfitpred(x,y,n,slope,bint,ro,se,sb,sa,xpred,ypred,prederr)
	return
	end

c	  !Fits a straight line to the [N] points in arrays [X] and [Y] by the
c	  method of least squares, returning [SLOPE], intercept [BINT], 
c	  correlation coeficient [RO], and standard errors of the estimate 
c	  [SE], slope [SB], and intercept [SA]
c
	subroutine lsfits(x,y,n,slope,bint,ro,se,sb,sa)
	implicit none
	real*4 x(*),y(*)
	real*4 slope,bint,ro,se,sb,sa,xpred,ypred,prederr
	integer*4 n
	xpred = x(1)
	call lsfitpred(x,y,n,slope,bint,ro,se,sb,sa,xpred,ypred,prederr)
	return
	end

c	  !Fits a straight line to the [N] points in arrays [X] and [Y] by the
c	  method of least squares, returning [SLOPE], intercept [BINT], 
c	  correlation coeficient [RO], standard errors of the estimate 
c	  [SE], the slope [SB], and the intercept [SA], and for one X value
c	  [XPRED], it returns the predicted value [YPRED] and the standard
c	  error of the prediction [PREDERR]
c
	subroutine lsfitpred(x,y,n,slope,bint,ro,se,sb,sa,xpred,ypred,prederr)
	implicit none
	real*4 x(*),y(*)
	real*4 slope,bint,ro,se,sb,sa,xpred,ypred,prederr
	integer*4 n,i
	real*4 sx,sy,xbar,ybar,sxpsq,sxyp,sypsq,d,roden,sxy,sysq,xp,yp
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
	ypred=slope*xpred+bint;
	prederr=(se*sqrt((1.+1./n+ n*(xpred-xbar)**2/d)));
	return
	end

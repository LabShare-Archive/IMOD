c	  WEIGHTPOLY uses weighted multiple linear regression to fit a
c	  polynomial of order IORDER to NFIT points whose (x,y) coordinates
c	  are in the arrays X and Y, and weighting valued are in W.   It
c	  returns the coefficient of x**i in the array SLOP and a constant
c	  term in BINT.  Y = BINT + sum ( SLOP(i) * X**i )

	subroutine weightpoly(x,y,w,nfit,iorder,slop,bint)
	real*4 x(*),y(*),w(*),slop(*)
	parameter (idim=1000)
	include 'statsize.inc'
	real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     1	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     2	    , b(msiz), b1(msiz)
	do i=1,nfit
	  do j=1,iorder
	    xr(j,i)=x(i)**j
	  enddo
	  xr(iorder+1,i)=y(i)
	  xr(iorder+2,i)=w(i)
	enddo
	call multr(xr,-(iorder+1),nfit,sx,ss,ssd,d,r,xm,sd,b,b1,bint,rsq
     &	    ,fra)
	do j=1,iorder
	  slop(j)=b1(j)
	enddo
	return
	end

c	  LOCALPOLYFIT uses multiple linear regression to fit a polynomial of
c	  order IORDER to NFIT points whose (x,y) coordinates are in the
c	  arrays X and Y It returns the coefficient of x**i in the array SLOP
c	  and a constant term in BINT.  Y = BINT + sum ( SLOP(i) * X**i )
c	  NFIT is limited to 50 to save memory in this version.
c
	subroutine localpolyfit(x,y,infit,iorder,slop,bint)
	real*4 x(*),y(*),slop(*)
	parameter (idim=50)
	include 'statsize.inc'
	real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     1	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     2	    , b(msiz), b1(msiz)
	nfit=min(idim,infit)
	do i=1,nfit
	  do j=1,iorder
	    xr(j,i)=x(i)**j
	  enddo
	  xr(iorder+1,i)=y(i)
	enddo
	call multr(xr,iorder+1,nfit,sx,ss,ssd,d,r,xm,sd,b,b1,bint,rsq
     &	    ,fra)
	do j=1,iorder
	  slop(j)=b1(j)
	enddo
	return
	end

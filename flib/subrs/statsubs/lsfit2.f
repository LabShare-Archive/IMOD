c	  LSFIT2 does a linear regression fit of the N values in the array Y to
c	  the values in the arrays X1 and X2, namely to the equation
c	  Y = A*X1 + B*X2 + C
c	  It returns the coefficients A and B, and the intercept C

	subroutine lsfit2(x1,x2,y,n,a,b,c)
	real*4 x1(*),x2(*),y(*)
	x1s=0.
	x2s=0.
	ys=0.
	do i=1,n
	  x1s=x1s+x1(i)
	  x2s=x2s+x2(i)
	  ys=ys+y(i)
	enddo
c	  
	x1m=x1s/n
	x2m=x2s/n
	ym=ys/n
	x1sqs=0.
	x2sqs=0.
	x1x2s=0.
	x1ys=0.
	x2ys=0.
	do i=1,n
	  x1p=x1(i)-x1m
	  x2p=x2(i)-x2m
	  yp=y(i)-ym
	  x1sqs=x1sqs+x1p**2
	  x2sqs=x2sqs+x2p**2
	  x1ys=x1ys+x1p*yp
	  x2ys=x2ys+x2p*yp
	  x1x2s=x1x2s+x1p*x2p
	enddo
	denom=x1sqs*x2sqs-x1x2s**2
	anum=x1ys*x2sqs-x1x2s*x2ys
	bnum=x1sqs*x2ys-x1ys*x1x2s
	a=0.
	b=0.
	c=0.
	if(abs(denom).lt.1.e-20*max(abs(anum),abs(bnum)))return
	a=anum/denom
	b=bnum/denom
	c=ym-a*x1m-b*x2m
	return
	end

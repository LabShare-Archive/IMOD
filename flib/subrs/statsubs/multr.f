c	subroutine multr
c	from Cooley and Lohnes - Multivariate Procedures for the
c	Behavioral Sciences, Wiley, 1962 (!).
c	computes multiple regression coefficients for an input data matrix
c	with the dependent variable in the last column
	subroutine multr(x,mpin,ng,sx,ss,ssd,d,r,xm,sd,b,b1,c,rsq,f)
        include 'statsize.inc'
c	x = data matrix, msiz columns by any # of rows
c	mp = # of columns (parameters),  ng = # of rows (subjects)
c	sx, xm, sd = sum, mean and sd of each column
c	ss, ssd = raw and deviation sums of squares and cross products
c	d, r = dispersion and correlation matrices
c	b, b1 = beta and b weights,  c = constant term
c	rsq is r squared, f is anova f with mp-1 and ng-mp degrees of freedom
c	pass mpin as negative if there are weights in column mp+1
	dimension x(msiz,*), sx(msiz), xm(msiz), sd(msiz)
     1, ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     2, r11(msiz,msiz), r12(msiz), b(msiz), b1(msiz)
	mp=abs(mpin)
	call correl(x,mp,ng,sx,ss,ssd,d,r,xm,sd,mpin)
	m=mp-1
	do 1 i=1,m
	r12(i)=r(i,mp)
	b(i)=r12(i)
	do 1 j=1,m
1	r11(i,j)=r(i,j)
	call matinv(r11, m, b, 1, determ)
	rsq=0.
	do 4 i=1,m
4	rsq=rsq+b(i)*r12(i)
	if(rsq.lt.1.)then
	  f=rsq*(ng-mp)/(m*(1.-rsq))
	else
c	  write(*,*) 'rsquared is 1.0'
	endif
	c=xm(mp)
	do 15 i=1,m
	if(sd(i).lt.1.e-30) write(*,*) 'division by sd',sd(i)
	b1(i)=b(i)*sd(mp)/sd(i)
15	c=c-b1(i)*xm(i)
	return
	end

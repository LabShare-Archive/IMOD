c	subroutine correl
c	from Cooley and Lohnes - Multivariate Procedures for the
c	Behavioral Sciences, Wiley, 1962 (!).
c	computes basic matrices - means, sd's, variances, covariances,
c	correlations - for an input data matrix
      subroutine correl(x,m,ng,sx,ss,ssd,d,r,xm,sd,ifdisp)
      include 'statsize.inc'
c	x = data matrix, msiz columns by any # of rows
c       m = # of columns (parameters),  ng = # of rows (subjects)
cXXX    or ng = 10000*(starting row #) + (ending row #)  ELIMINATED
c	sx, xm, sd = sum, mean and sd of each column
c	ss, ssd = raw and deviation sums of squares and cross products
c	d, r = dispersion and correlation matrices, compute if ifdisp>0
c	IF ifdisp <0, then compute ssd with weights in column m+1
c       
c       4/30/92: rewrote to calculate means first then form SSD from the
c       deviations from means; this is slower, but the old formula was
c       too inaccurate when fitting high-order polynomials.
c       Also indented loops and consolidated some loops
c       
      dimension x(msiz,*), sx(msiz), xm(msiz), sd(msiz)
     1    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
      ns=1
c	if(ng.le.10000)go to 220
c	ns=ng/10000
c	ng=ng-10000*ns
220   eng=ng+1-ns
c	write(*,*) 'doing correl on cells',ns,' to',ng
      do 232 i=1,m
        sx(i)=0.
        do 230 j=1,m
          ssd(i,j)=0.
          r(i,j)=0.
230     continue
232   continue
      if(ifdisp.ge.0)then
        do 242 i=1,m
          do 240 k=ns,ng
            sx(i)=sx(i)+x(i,k)
240       continue
          xm(i)=sx(i)/eng
242     continue
      else
        wsum=0.
        do 328 k=ns,ng
          wsum=wsum+x(m+1,k)
328     continue
        do 342 i=1,m
          do 340 k=ns,ng
            sx(i)=sx(i)+x(i,k)*x(m+1,k)
340       continue
          xm(i)=sx(i)/wsum
342     continue
      endif
      do 264 k=ns,ng
c         write(*,'(8f9.4)')(x(i,k),i=1,m)
        weight=1.
        if(ifdisp.lt.0)weight=x(m+1,k)
        do 262 i=1,m
          do 260 j=i,m
            ssd(i,j)=ssd(i,j)+(x(i,k)-xm(i))*(x(j,k)-xm(j))*weight
260       continue
262     continue
264   continue
      do 288 i=1,m
        sd(i)=sqrt(ssd(i,i)/(eng-1.))
        do 286 j=i,m
          ss(i,j)=ssd(i,j)+sx(i)*sx(j)/eng
          ss(j,i)=ss(i,j)
          ssd(j,i)=ssd(i,j)
286     continue
288   continue
c	call mprint(ss,m,'ss ')
c	call mprint(ssd,m,'ssd')
c	write(*,*)'xm '
c	write(*,105)(xm(i),i=1,m)
c       105	format(5f15.7)
c	write(*,*)'sd '
c	write(*,105)(sd(i),i=1,m)
      if(ifdisp.eq.0)go to 700
      do 442 i=1,m
c	  if(sd(i).lt.1.e-30)write(*,*) 'small sd',sd(i)
        do 441 j=i,m
          d(i,j)=ssd(i,j)/(eng-1.)
          d(j,i)=d(i,j)
          den=sd(i)*sd(j)
          if(den.gt.1.e-30)r(i,j)=d(i,j)/(sd(i)*sd(j))
          r(j,i)=r(i,j)
441     continue
442   continue
c	call mprint(d,m,'d  ')
c	call mprint(r,m,'r  ')
700   return
      end

c	subroutine mprint(x,m,string)
c       include 'statsize.inc'
c	dimension x(msiz,msiz)
c	character*3 string
c	write(*,*)string
c	do 10 i=1,m
c       10	write(*,20)(x(i,j),j=1,m)
c       20	format(5g15.7)
c	return
c	end

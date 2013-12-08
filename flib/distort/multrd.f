c       subroutine multr
c       from Cooley and Lohnes - Multivariate Procedures for the
c       Behavioral Sciences, Wiley, 1962 (!).
c       computes multiple regression coefficients for an input data matrix
c       with the dependent variable in the last column
      subroutine multrd(x,msiz,mp,ng,sx,ss,xm,sd,b,b1,rsq,f)
c       x = data matrix, msiz columns by any # of rows
c       mp = # of columns (parameters),  ng = # of rows (subjects)
c       sx, xm, sd = sum, mean and sd of each column
c       ss, ssd = raw and deviation sums of squares and cross products
c       d, r = dispersion and correlation matrices
c       b, b1 = beta and b weights,  c = constant term
c       rsq is r squared, f is anova f with mp-1 and ng-mp degrees of freedom
c       pass mpin as negative if there are weights in column mp+1
      parameter (linsiz = 10000)
      real*4 x(msiz,*)
      real*8 sx(msiz), xm(msiz), sd(msiz), ss(msiz,msiz),
     &    r12(linsiz), b(msiz), b1(msiz), determ
      call correld(x,msiz,mp,ng,sx,ss,r12,xm,sd)
      m=mp-1
      do i=1,m
        b(i)=ss(i,mp) / (sd(i) * sd(mp))
        do j=1,m
          ss(i,j)=ss(i,j) / (sd(i) * sd(j))
        enddo
      enddo
      call matinvd(ss, msiz, m, b, 1, determ)
      rsq=0.
      do i=1,m
        rsq=rsq+b(i)*r12(i)
        b1(i)=b(i)*sd(mp)/sd(i)
      enddo
      if(rsq.lt.1.)then
        f=rsq*(ng-mp)/(m*(1.-rsq))
      else
c         write(*,*) 'rsquared is 1.0'
      endif
      return
      end


c       subroutine correl
c       from Cooley and Lohnes - Multivariate Procedures for the
c       Behavioral Sciences, Wiley, 1962 (!).
c       computes basic matrices - means, sd's, variances, covariances,
c       correlations - for an input data matrix
      subroutine correld(x,msiz,m,ng,sx,ss,r12,xm,sd)
c       x = data matrix, msiz columns by any # of rows
c       m = # of columns (parameters),  ng = # of rows (subjects)
c       or ng = 10000*(starting row #) + (ending row #)
c       sx, xm, sd = sum, mean and sd of each column
c       ss, ssd = raw and deviation sums of squares and cross products
c       d, r = dispersion and correlation matrices, compute if ifdisp>0
c       IF ifdisp <0, then compute ssd with weights in column m+1
c       
c       4/30/92: rewrote to calculate means first then form SSD from the
c       deviations from means; this is slower, but the old formula was
c       too inaccurate when fitting high-order polynomials.
c       Also indented loops and consolidated some loops
c       
      real*4 x(msiz,*)
      real*8 sx(msiz), xm(msiz), sd(msiz), ss(msiz,msiz), r12(*)
      ns=1
      eng=ng+1-ns
c       write(*,*) 'doing correl on cells',ns,' to',ng
      do 232 i=1,m
        sx(i)=0.
        do 230 j=1,m
          ss(i,j)=0.
230     continue
232   continue
      do 242 i=1,m
        do 240 k=ns,ng
          sx(i)=sx(i)+x(i,k)
240     continue
        xm(i)=sx(i)/eng
242   continue
      do 264 k=ns,ng
c         write(*,'(8f9.4)')(x(i,k),i=1,m)
        do 262 i=1,m
          do 260 j=1,m
            ss(i,j)=ss(i,j)+(x(i,k)-xm(i))*(x(j,k)-xm(j))
260       continue
262     continue
264   continue
      do i = 1,m
        sd(i)=sqrt(ss(i,i)/(eng-1.))
      enddo
      do i = 1,m
        r12(i) = 0.
        den=sd(i)*sd(m)
        if(den.gt.1.e-30)r12(i)=(ss(i,m)/(eng-1.))/(sd(i)*sd(m))
      enddo
      do 288 i=1,m
        do 286 j=1,m
          ss(i,j)=ss(i,j)+sx(i)*sx(j)/eng
286     continue
288   continue
c       call mprint(ss,m,'ss ')
c       call mprint(ssd,m,'ssd')
c       write(*,*)'xm '
c       write(*,105)(xm(i),i=1,m)
c       105     format(5f15.7)
c       write(*,*)'sd '
c       write(*,105)(sd(i),i=1,m)
c       call mprint(d,m,'d  ')
c       call mprint(r,m,'r  ')
700   return
      end

c       subroutine mprint(x,m,string)
c       include 'statsize.inc'
c       dimension x(msiz,msiz)
c       character*3 string
c       write(*,*)string
c       do 10 i=1,m
c       10      write(*,20)(x(i,j),j=1,m)
c       20      format(5g15.7)
c       return
c       end

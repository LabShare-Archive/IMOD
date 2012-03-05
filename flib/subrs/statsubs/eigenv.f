c       subroutine eigenv
c       adapted from discrim program of Cooley and Lohnes
c   9/88 modified for VAX, leave out tests for missing data, drop argument
c   ircol, fix dialog, put in "quiet" option
c   INVERTED SUBSCRIPT ORDER OF DMAT ARRAY
        subroutine eigenv(dmat,m,ndisc,ning,nstr,nend,ssd,ifquiet)
        include 'statsize.inc'
        dimension sx(msiz), xm(msiz), sd(msiz),icltmp(msiz)
     1, ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz),sclvec(msiz)
     3, dmat(msiz,*),sumt(msiz),ts(msiz,msiz),w(msiz,msiz)
     4, ning(*),nstr(*),nend(*),grand(msiz),ltot(msiz)
        k=iabs(ndisc)
        do 6 i=1,m
        do 6 j=1,m
        w(i,j)=0.
6       d(i,j)=0.
        call dmean(dmat,m,1,nend(k),grand)
        do 20 ii=1,k
        call dmean(dmat,m,nstr(ii),nend(ii),xm)
        do 20 i=1,m
        do 20 j=i,m
        d(i,j)=d(i,j)+ning(ii)*(xm(i)-grand(i))*(xm(j)-grand(j))
        nm=0
        ssdm=0.
        do 10 icl=nstr(ii),nend(ii)
c   commented out test for missing data
c       if(dmat(i,icl).le.-1000..or.dmat(j,icl).le.-1000.)go to 10
        nm=nm+1
        ssdm=ssdm+(dmat(i,icl)-xm(i))*(dmat(j,icl)-xm(j))
10      continue
        if(nm-1)12,14,16
12      write(*,*) 'no covariant data for group',ii,', columns',i,j
        go to 18
14      write(*,*) 'only one covariant datum for group',ii,', columns'
     1,i,j
        nm=2
16      w(i,j)=w(i,j)+ssdm*(ning(ii)-1.)/(nm-1.)
18      ts(i,j)=w(i,j)+d(i,j)
        r(i,j)=w(i,j)   
        ts(j,i)=ts(i,j)
        r(j,i)=r(i,j)
20      continue
        do 22 i=1,m
22      sx(i)=sqrt(w(i,i))
c ** negative # of groups indicates just do non-stepwise on all variables
        if(ndisc.lt.0)go to 23
        write(*,'(1x,a,$)')
     &      'lower & upper # of variables for stepwise (0,0 none): '
        read(*,*)mlo,mhi
        if(mlo.gt.0.and.mlo.le.mhi.and.mhi.le.m)go to 24
23      mlo=m
        mhi=m
24      do 60 ms=mlo,mhi
        call stepwz(ms,m,ltot,sx,w,d,r,ts,ssd)
c   alternative when columns had numbers
c       do 26 i=1,ms
c26     icltmp(i)=ircol(ltot(i))
c       write(*,201)ms,(icltmp(i),i=1,ms)
        if(ndisc.gt.0) write(*,201)ms,(ltot(i),i=1,ms)
201     format('best',i3,' variables:',20i3)
c       call prmat(ts,m,'full total matrix t')
c       call prmat(r,m,'full within matrix')
c       call prmat(w,ms,'pooled within matrix')
c       call prmat(d,ms,'among matrix')
        call dirnm(d, ms, w, ssd, sd)
c       do 630 k=1,m
c       asum=0.
c       wsum=0.
c       do 620 i=1,m
c       do 620 j=1,m
c       asum=asum+d(i,j)*ssd(i,k)*ssd(j,k)
c620    wsum=wsum+w(i,j)*ssd(i,k)*ssd(j,k)
c       frat=asum/wsum
c630    write(*,'(1x,a,i3,f12.6)')'f ratio for vector',k,frat
        if(ifquiet.eq.0)then
          write(*,*) 'roots of W-inverse*A'
          write(*,'(5f15.4)')(sd(i),i=1,ms)
          trace=0.
          do 35 i=1,ms
35        trace = trace +sd(i)
          write(*,'(1x,a,f15.4)')'trace of W-1*A =',trace
          do 36 i=1,ms
36        xm(i)=100.*sd(i)/trace
          write(*,*) 'percentage which each root is of trace'
          write(*,'(f7.2,9f8.2)')(xm(i),i=1,ms)
        endif
        md=min0(ms,k-1)
        write(*,*) 'vectors of W-1*A'
        do 130 j=1,md
130     write(*,131)(ssd(i,j),i=1,ms)
131     format(f7.4,9f8.4)
        if(ifquiet.eq.0)then
          write(*,*) 'scaled vectors'
          do 134 j=1,md
            do 133 i=1,ms
133         sclvec(i)=sx(i)*ssd(i,j)
134       write(*,131)(sclvec(i),i=1,ms)
          xlamb=1.
          do 51 i=1,md
51        xlamb=xlamb*(1./(1.+sd(i)))
          write(*,'(1x,a,f14.7)')'lambda',xlamb
        endif
        if(ms.ne.mhi)call mypause(' ')
60      continue
        if(mhi.eq.m)return
        do 70 i=m,1,-1
        do 66 ii=1,mhi
        if(ltot(ii).ne.i)go to 66
        do 64 j=1,md
64      ssd(i,j)=ssd(ii,j)
        go to 70
66      continue
        do 68 j=1,md
68      ssd(i,j)=0.
70      continue
        return
        end

        subroutine prmat(x,n,heading)
        include 'statsize.inc'
        dimension x(msiz,msiz)
        character*(*) heading
        write(*,*) heading
        do 20 i=1,n
20      write(*,'(f9.4,7f10.4)')(x(i,j),j=1,n)
        return
        end

        subroutine dmean(dmat,ncol,nstr,nend,avgs)
        include 'statsize.inc'
        dimension dmat(msiz,*),avgs(*)
        do 30 j=1,ncol
        n=0
        s=0.
        do 20 i=nstr,nend
c   commented out test for missing data
c       if(dmat(j,i).le.-1000.)go to 20
        n=n+1
        s=s+dmat(j,i)
20      continue
30      avgs(j)=s/n
        return
        end

c       subroutine stepwz
c       A meager adapted portion of stepwz routine from Eisenbeis and Avery
c       Does complete stepwise variable selection at a particular level
c       i.e. it picks the best m out of mfull variables for discriminant
c       give it full total SSCP matrix in ts, full within-group in r, it
c       will return best m variables in ltot, and for those variables,
c       total SSCP in ssd, within in w, between in d, sqrt(w(i,i)) in sx
        subroutine stepwz(m,mfull,ltot,sx,w,d,r,ts,ssd)
        include 'statsize.inc'
        dimension ltot(msiz),w(msiz,msiz),d(msiz,msiz),r(msiz,msiz)
     1,ts(msiz,msiz),ssd(msiz,msiz),sx(msiz)
        common /stpcom/ mdiff,k12,ns(msiz)
        det=1.e38
        call stpini(m,mfull,ltot)
        if(mdiff.le.0)go to 105
77      do 80 i=1,m
        ki=ns(i)
        do 80 j=1,m
        kj=ns(j)
        d(i,j)=ts(ki,kj)
80      w(i,j)=r(ki,kj)
        call matinv(d,m,sx,0,dt)
        call matinv(w,m,sx,0,dw)
        dt=dw/dt
        if(dt.gt.det)go to 70
        det=dt
        do 85 i=1,m
85      ltot(i)=ns(i)
C
C 7/20/00 CER remove ltot from call
C
70      call stpmov(m,mfull)
        if(k12.gt.0)go to 77
105     do 113 i=1,m
        ki=ltot(i)
        do 110 j=1,m
        kj=ltot(j)
        d(i,j)=ts(ki,kj)-r(ki,kj)
        ssd(i,j)=ts(ki,kj)
110     w(i,j)=r(ki,kj)
        sx(i)=sqrt(w(i,i))
113     continue
        return
        end

        subroutine stpini(m,mfull,ltot)
        include 'statsize.inc'
        dimension ltot(msiz)
        common /stpcom/ mdiff,k12,ns(msiz)
        mdiff=mfull-m
        do 65 i=1,m
        ns(i)=i
65      ltot(i)=i
        return
        end

        subroutine stpmov(m,mfull)
        include 'statsize.inc'
        common /stpcom/ mdiff,k12,ns(msiz)
        ns(m)=ns(m)+1
75      k12=m
        if(ns(m).le.mfull)return
90      k12=k12-1
        if(k12.eq.0) return
        ns(k12)=ns(k12)+1
        if(ns(k12)-k12.gt.mdiff)go to 90
        do 100 i=k12+1,m
100     ns(i)=ns(k12)+i-k12
        go to 75
        end

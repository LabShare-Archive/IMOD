c       subroutine hdiag
c       from Cooley and Lohnes - Multivariate Procedures for the
c       Behavioral Sciences, Wiley, 1962 (!).
c       computes eigenvalues andeigenvectors of a real symmetric
c       matrix h, of order n.  It places eigenvalues in the diagonal elements
c       of h, eigenvectors in the columns of matrix u if iegen =0 not 1.
c       nr contains # of rotations done
        subroutine hdiag(h,n,iegen,u,nr)
        include 'statsize.inc'
        dimension h(msiz,msiz), u(msiz,msiz), x(msiz), iq(msiz)
c       call prmat(h,n,'matrix entering hdiag')
        if(iegen.ne.0)go to 15
        do 14 i=1,n
        do 14 j=1,n
        u(i,j)=0.
        if(i.eq.j)u(i,j)=1.0
14      continue
15      nr=0
        if(n.eq.1)go to 1000
c       scan for largest off-diagonal element in each row
c       x(i) contains largest element in i-th row
c       iq(i) holds second subscript defining position of element
        nmi1=n-1
        do 30 i=1,nmi1
        x(i)=0.
        do 30 j=i+1,n
        if(x(i).gt.abs(h(i,j)))go to 30
        x(i)=abs(h(i,j))
        iq(i)=j
30      continue
c       set indicator for shut-off, rap=2**-27, nr= no. of rotations
        rap=7.450580596e-9
        hdtest=1.0e38
c       find maximum of h(i)'s for pivot element and test for end of problem
40      do 70 i=1,nmi1
        if(i.eq.1)go to 60
        if(xmax.ge.x(i))go to 70
60      xmax=x(i)
        ipiv=i
        jpiv=iq(i)
70      continue
c       is max. x(i) is equal ro zero, if less than hdtest, revise hdtest
        if(xmax.le.0)go to 1000
        if(hdtest.le.0)go to 90
        if(xmax.gt.hdtest)go to 148
90      hdimin=abs(h(1,1))
        do 110 i=2,n
        if(hdimin.gt.abs(h(i,i)))hdimin=h(i,i)
110     continue
        hdtest=hdimin*rap
c       return if max. h(i,j) less than (2**-27)abs(h(k,k)-min)
        if(hdtest.ge.xmax)go to 1000
148     nr=nr+1
c       compute tangent, sine cosine h(i,i) H(j,j)
150     tang=sign(2.0,(h(ipiv,ipiv)-h(jpiv,jpiv)))*h(ipiv,jpiv)/(abs(
     1 h(ipiv,ipiv)-h(jpiv,jpiv))+sqrt((h(ipiv,ipiv)-h(jpiv,jpiv))**2
     2 +4.0*h(ipiv,jpiv)**2))
        cosine=1.0/sqrt(1+tang**2)
        sine=tang*cosine
        hii=h(ipiv,ipiv)
        h(ipiv,ipiv)=(cosine**2)*(hii+tang*(2.*h(ipiv,jpiv)+tang*
     1 h(jpiv,jpiv)))
        h(jpiv,jpiv)=(cosine**2)*(h(jpiv,jpiv)-tang*(2.*h(ipiv,jpiv)-
     2 tang*hii))
        h(ipiv,jpiv)=0.
c       pseudo rank the eigenvalues
c       adjust sine and cosine for computation of h(ik) and u(ik)
        if(h(ipiv,ipiv).ge.h(jpiv,jpiv))go to 153
        htemp=h(ipiv,ipiv)
        h(ipiv,ipiv)=h(jpiv,jpiv)
        h(jpiv,jpiv)=htemp
c       recompute sine and cosine
        htemp=sign(1.0,-sine)*cosine
        cosine=abs(sine)
        sine=htemp
153     continue
c       inspect the iq's between i+1 and n-1 to determine whether a new maximum
c       value should be computed since the present value is in the i or j row
        do 350 i=1,nmi1
        if(i-ipiv)210,350,200
200     if(i.eq.jpiv)go to 350
210     if(iq(i).eq.ipiv)go to 240
        if(iq(i).ne.jpiv)go to 350
240     k=iq(i)
        htemp=h(i,k)
        h(i,k)=0.
        x(i)=0.
c       search in depleted row for new maximum
        do 320 j=i+1,n
        if(x(i).gt.abs(h(i,j)))go to 320
        x(i)=abs(h(i,j))
        iq(i)=j
320     continue
        h(i,k)=htemp
350     continue
        x(ipiv)=0.
        x(jpiv)=0.
c       change the other elements of h
        do 530 i=1,n
        if(i-ipiv)370,530,420
370     htemp=h(i,ipiv)
        h(i,ipiv)=cosine*htemp+sine*h(i,jpiv)
        if(x(i).ge.abs(h(i,ipiv)))go to 390
        x(i)=abs(h(i,ipiv))
        iq(i)=ipiv
390     h(i,jpiv)=-sine*htemp+cosine*h(i,jpiv)
        if(x(i).ge.abs(h(i,jpiv)))go to 530
        x(i)=abs(h(i,jpiv))
        iq(i)=jpiv
        go to 530
420     if(i-jpiv)430,530,480
430     htemp=h(ipiv,i)
        h(ipiv,i)=cosine*htemp+sine*h(i,jpiv)
        if(x(ipiv).ge.abs(h(ipiv,i)))go to 390
        x(ipiv)=abs(h(ipiv,i))
        iq(ipiv)=i
        go to 390
480     htemp=h(ipiv,i)
        h(ipiv,i)=cosine*htemp+sine*h(jpiv,i)
        if(x(ipiv).ge.abs(h(ipiv,i)))go to 500
        x(ipiv)=abs(h(ipiv,i))
        iq(ipiv)=i
500     h(jpiv,i)=-sine*htemp+cosine*h(jpiv,i)
        if(x(jpiv).ge.abs(h(jpiv,i)))go to 530
        x(jpiv)=abs(h(jpiv,i))
        iq(jpiv)=i
530     continue
c       test for computation of eigenvectors
        if(iegen.ne.0)go to 40
        do 550 i=1,n
        htemp=u(i,ipiv)
        u(i,ipiv)=cosine*htemp+sine*u(i,jpiv)
550     u(i,jpiv)=-sine*htemp+cosine*u(i,jpiv)
        go to 40
c1000   call prmat(h,n,'matrix leaving hdiag')
c       call prmat(u,n,'matrix of eigenvectors')
1000    return
        end

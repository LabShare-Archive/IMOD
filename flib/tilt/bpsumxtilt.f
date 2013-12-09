      subroutine bpsumxtilt(array,ind1,ipbase,ipdel,n,
     &    xproj1,cbeta,yfrac,omyfrac)
      real*4 array(*)
      real*8 xproj8,xproj1,xproj2,xproj3,xproj4,xproj5,xproj6,xproj7

      n8 = n / 8
      nleft=n - 8*n8

      DO J=1,n8
        xproj2=xproj1 + cbeta
        xproj3=xproj2 + cbeta
        xproj4=xproj3 + cbeta
        xproj5=xproj4 + cbeta
        xproj6=xproj5 + cbeta
        xproj7=xproj6 + cbeta
        xproj8=xproj7 + cbeta
        iproj1=xproj1
        iproj2=xproj2
        iproj3=xproj3
        iproj4=xproj4
        iproj5=xproj5
        iproj6=xproj6
        iproj7=xproj7
        iproj8=xproj8
        xfrac1=xproj1-iproj1
        xfrac2=xproj2-iproj2
        xfrac3=xproj3-iproj3
        xfrac4=xproj4-iproj4
        xfrac5=xproj5-iproj5
        xfrac6=xproj6-iproj6
        xfrac7=xproj7-iproj7
        xfrac8=xproj8-iproj8
        ip1=ipbase+iproj1
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac1)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac1*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj2
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac2)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac2*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj3
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac3)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac3*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj4
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac4)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac4*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj5
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac5)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac5*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj6
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac6)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac6*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj7
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac7)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac7*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        ip1=ipbase+iproj8
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac8)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac8*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        xproj1=xproj8+cbeta
      enddo

      do j=1,nleft
        IPROJ1=XPROJ1
        XFRAC1=XPROJ1-IPROJ1
        ip1=ipbase+iproj1
        ip2=ip1+ipdel
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-xfrac1)*(omyfrac*ARRAY(IP1) +yFRAC*ARRAY(IP2)) +
     &      xfrac1*(omyfrac*ARRAY(IP1+1) +yFRAC*ARRAY(IP2+1))
        IND1=IND1+1
        xproj1=xproj1+cbeta
      enddo
      return
      end

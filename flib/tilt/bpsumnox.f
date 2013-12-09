      subroutine bpsumnox(array,ind1,ipoint,n,xproj1,cbeta)
      real*4 array(*)
      real*8 xproj8,xproj1,xproj2,xproj3,xproj4,xproj5,xproj6,xproj7

      n8 = n / 8
      nleft=n - 8*n8
      ipp1=ipoint+1
      
      do j=1,n8
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
        ind2=ind1+1
        ind3=ind2+1
        ind4=ind3+1
        ind5=ind4+1
        ind6=ind5+1
        ind7=ind6+1
        ind8=ind7+1
        ARRAY(IND1)=ARRAY(IND1)+(1.-XFRAC1)*ARRAY(IPOINT+IPROJ1)
     &      +XFRAC1*ARRAY(IPp1+IPROJ1) 
        ARRAY(IND2)=ARRAY(IND2)+(1.-XFRAC2)*ARRAY(IPOINT+IPROJ2)
     &      +XFRAC2*ARRAY(IPp1+IPROJ2) 
        ARRAY(IND3)=ARRAY(IND3)+(1.-XFRAC3)*ARRAY(IPOINT+IPROJ3)
     &      +XFRAC3*ARRAY(IPp1+IPROJ3) 
        ARRAY(IND4)=ARRAY(IND4)+(1.-XFRAC4)*ARRAY(IPOINT+IPROJ4)
     &      +XFRAC4*ARRAY(IPp1+IPROJ4) 
        ARRAY(IND5)=ARRAY(IND5)+(1.-XFRAC5)*ARRAY(IPOINT+IPROJ5)
     &      +XFRAC5*ARRAY(IPp1+IPROJ5) 
        ARRAY(IND6)=ARRAY(IND6)+(1.-XFRAC6)*ARRAY(IPOINT+IPROJ6)
     &      +XFRAC6*ARRAY(IPp1+IPROJ6) 
        ARRAY(IND7)=ARRAY(IND7)+(1.-XFRAC7)*ARRAY(IPOINT+IPROJ7)
     &      +XFRAC7*ARRAY(IPp1+IPROJ7) 
        ARRAY(IND8)=ARRAY(IND8)+(1.-XFRAC8)*ARRAY(IPOINT+IPROJ8)
     &      +XFRAC8*ARRAY(IPp1+IPROJ8) 
        ind1=ind8+1
        xproj1=xproj8+cbeta
      enddo

      DO J=1,nleft
        IPROJ1=XPROJ1
        XFRAC1=XPROJ1-IPROJ1
        ARRAY(IND1)=ARRAY(IND1)+
     &      (1.-XFRAC1)*ARRAY(IPOINT+IPROJ1)
     &      +XFRAC1*ARRAY(IPp1+IPROJ1) 
        IND1=IND1+1
        xproj1=xproj1+cbeta
      enddo
      return
      end

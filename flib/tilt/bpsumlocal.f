      subroutine bpsumlocal(array,index,zz,xprojf,xprojz,yprojf,
     &    yprojz,ipoint,ipdel,lslice,jstrt, jend)
      real*4 array(*),xprojf(*),xprojz(*),yprojf(*),yprojz(*)

      ndiv=5
      nfast=(jend+1-jstrt)/ndiv
      jendfast=jstrt+(nfast-1)*ndiv
      jstrtslow=jstrt+nfast*ndiv
      
      do j=jstrt,jendfast,ndiv
        k=j
        xproj1=xprojf(k)+zz*xprojz(k)
        yproj1=yprojf(k)+zz*yprojz(k)
        k=k+1
        xproj2=xprojf(k)+zz*xprojz(k)
        yproj2=yprojf(k)+zz*yprojz(k)
        k=k+1
        xproj3=xprojf(k)+zz*xprojz(k)
        yproj3=yprojf(k)+zz*yprojz(k)
        k=k+1
        xproj4=xprojf(k)+zz*xprojz(k)
        yproj4=yprojf(k)+zz*yprojz(k)
        k=k+1
        xproj5=xprojf(k)+zz*xprojz(k)
        yproj5=yprojf(k)+zz*yprojz(k)
        IPROJ1=XPROJ1
        jPROJ1=YPROJ1
        IPROJ2=XPROJ2
        jPROJ2=YPROJ2
        IPROJ3=XPROJ3
        jPROJ3=YPROJ3
        IPROJ4=XPROJ4
        jPROJ4=YPROJ4
        IPROJ5=XPROJ5
        jPROJ5=YPROJ5
c         
        XFRAC=XPROJ1-IPROJ1
        YFRAC=YPROJ1-JPROJ1
        ip1=ipoint+(jproj1-lslice)*ipdel+iproj1
        ip2=ip1+ipdel
        ARRAY(INDEX)=ARRAY(INDEX)+
     &      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
c         INDEX=INDEX+1
c         
        XFRAC=XPROJ2-IPROJ2
        YFRAC=YPROJ2-JPROJ2
        ip1=ipoint+(jproj2-lslice)*ipdel+iproj2
        ip2=ip1+ipdel
        ARRAY(INDEX+1)=ARRAY(INDEX+1)+
     &      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
        INDEX=INDEX+2
c         
        XFRAC=XPROJ3-IPROJ3
        YFRAC=YPROJ3-JPROJ3
        ip1=ipoint+(jproj3-lslice)*ipdel+iproj3
        ip2=ip1+ipdel
        ARRAY(INDEX)=ARRAY(INDEX)+
     &      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
        INDEX=INDEX+1
c         
        XFRAC=XPROJ4-IPROJ4
        YFRAC=YPROJ4-JPROJ4
        ip1=ipoint+(jproj4-lslice)*ipdel+iproj4
        ip2=ip1+ipdel
        ARRAY(INDEX)=ARRAY(INDEX)+
     &      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
        INDEX=INDEX+1
c         
        XFRAC=XPROJ5-IPROJ5
        YFRAC=YPROJ5-JPROJ5
        ip1=ipoint+(jproj5-lslice)*ipdel+iproj5
        ip2=ip1+ipdel
        ARRAY(INDEX)=ARRAY(INDEX)+
     &      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
        INDEX=INDEX+1
      enddo

      do j=jstrtslow,jend
        xproj=xprojf(j)+zz*xprojz(j)
        yproj=yprojf(j)+zz*yprojz(j)
        IPROJ=XPROJ
        XFRAC=XPROJ-IPROJ
        jPROJ=YPROJ
        YFRAC=YPROJ-JPROJ
c         
        ip1=ipoint+(jproj-lslice)*ipdel+iproj
        ip2=ip1+ipdel
        ARRAY(INDEX)=ARRAY(INDEX)+
     &      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
        INDEX=INDEX+1
      enddo
      return
      end

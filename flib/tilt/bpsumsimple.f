	subroutine bpsumnox(array,index,ipoint,n,xproj8,cbeta)
	real*4 array(*)
	real*8 xproj8
	DO J=1,n
	  IPROJ=XPROJ8
	  XFRAC=XPROJ8-IPROJ
	  ARRAY(INDEX)=ARRAY(INDEX)+
     &	      (1.-XFRAC)*ARRAY(IPOINT+IPROJ)
     &	      +XFRAC*ARRAY(IPOINT+IPROJ+1) 
	  INDEX=INDEX+1
	  xproj8=xproj8+cbeta
	enddo
	return
	end

	subroutine bpsumxtilt(array,index,ipbase,ipdel,n,
     &		      xproj8,cbeta,yfrac,omyfrac)
	real*4 array(*)
	real*8 xproj8
	DO J=1,n
	  IPROJ=XPROJ8
	  XFRAC=XPROJ8-IPROJ
	  ip1=ipbase+iproj
	  ip2=ip1+ipdel
	  ARRAY(INDEX)=ARRAY(INDEX)+
     &	      (1.-xfrac)*(omyfrac*ARRAY(IP1)
     &	      +yFRAC*ARRAY(IP2)) +
     &	      xfrac*(omyfrac*ARRAY(IP1+1)
     &	      +yFRAC*ARRAY(IP2+1))
	  INDEX=INDEX+1
	  xproj8=xproj8+cbeta
	enddo
	return
	end

	subroutine bpsumlocal(array,index,zz,xprojf,xprojz,yprojf,
     &	    yprojz,ipoint,ipdel,lslice,jstrt, jend)
	real*4 array(*),xprojf(*),xprojz(*),yprojf(*),yprojz(*)
	do j=jstrt,jend
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
     &	      (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1) +XFRAC*ARRAY(IP1+1)) +
     &	      yfrac*((1.-XFRAC)*ARRAY(IP2) +XFRAC*ARRAY(IP2+1))
	  INDEX=INDEX+1
	enddo
	return
	end

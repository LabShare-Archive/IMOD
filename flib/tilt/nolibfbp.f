c	  NOLIBFBP
c	  
c	  This file contains dummy subroutine entries that can be 
c	  called if the USFFT libraries are not available
c	  
c	  fbpneed returns nprjp = 0 to signal that these dummy libraries
c	  have been reached
c
	subroutine fbpini(angles,Nprj,Nviews,Nwide,Nthick,
     &	    rmax,sdg,interpol,inplace,
     &	    iw,w,zw,wrk)
	implicit real *4 (a-h,o-z)
	integer *4 iw(*)
	real *4 angles(*),w(*),wrk(*)
	complex *8 zw(*)
	return
	end

	subroutine fbpneed(angles,Nprj,Nviews,Nwide,Nthick,
     &	    inplace,
     &	    iw,wrk,nprjp,nwidep,needwrk,needzwrk,neediw,needrw,needzw,
     &	    needout,minsup,maxsup)
	implicit real *4 (a-h,o-z)
	integer *4 iw(*)
	real *4 angles(*),wrk(*)

	nprjp=0
	minsup=1
	maxsup=nprj

	return
	end


        subroutine fbp(u,NprjDim,Nprj,Nviews,Nwide,Nthick,
     &	    uout,iw,rw,zw,wrk,zwrk)
        implicit real *4 (a-h,o-z)
        integer *4 iw(*)
        real *4 u(*),uout(*),rw(*),wrk(*)
        complex *8 zw(*),zwrk(*)
	return
	end

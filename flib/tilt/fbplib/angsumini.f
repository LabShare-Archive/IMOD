c	$Log$
c	Revision 1.1.1.1  2001/11/21 23:12:01  rickg
c	Import into repository
c	
c 
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
c                                                                           c
c        This file is a part of                                             c
c        Fast Transform Library                                             c
c        Contains proprietary information supplied by GB Consulting.        c
c        Copyright (C), 1994-99 GB Consulting. All rights reserved          c
c                                                                           c
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
c
c	Input:        
c
c     freq      : A specific frequency within the range of seismic frequencies
c                 (real *4)
c     nviews    : The number of slopes (angles)
c     nthick    : Number of elements in the thick direction
c     theta1,  
c     theta2    : Array of curve function values (real *4) 
c                 Can be any explicit function.
c     xcen      : Center parameter for the projcetion data.
c
c	Output:        
c
c     x         : Function theta values mod 2 Pi as appropriate
c                 to use in the exponent; may not be ordered;
c                 size nviews (real *4)
c
c     diag      : Complex factors of size nviews (complex *8)
c
c     work      : Tables of coefficients of size 
c                 13*nthick+13*nviews+16 (real *4)
c
c  
c________________________________________________________

   
c        subroutine angsumini(freq,nthick,theta1,theta2,
c     &                      xcen,nviews,x,diag,work)

        subroutine angsumini(freq,nthick,theta1,theta2,
     &                      xcen,nviews,x,diag,wsave,wrk)

   

        implicit real *4 (a-h,o-z)

        real *4 twopi,freq,freq2,fact,argm,freqxcen
c        real *4 theta1(*),theta2(*),x(*),work(*)
        real *4 theta1(*),theta2(*),x(*),wrk(*)

        complex *8 diag(*),wsave(*)

        data twopi/6.2831853071795865/

c
c ============================================================
c


c ----------------- computation of x ----------------------------------
c

        fact   = freq/twopi

        do l=1,nviews
           x(l)=theta2(l)*fact
           x(l)=x(l)-nint(x(l))
        enddo

c
c -------------------------------------------------------------------


c ----------------- computation of diagonal factor ---------------
c
    
        freq2=freq*0.5
        freqxcen=freq*xcen

        do l=1,nviews
           argm=-(freq2*theta2(l)+freqxcen*(theta1(l)+1)+freq)
           diag(l)=cmplx(cos(argm),sin(argm))
        enddo

c
c -------------------------------------------------------------------


c --------------------- initialization ---------------
c


        call ffti(nthick,wsave) 
        call ufs1i(nthick,nviews,x,wrk)

c
c -------------------------------------------------------------


c
c ============================================================
c

      return
      end



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
c     flow      : Lowest frequency
c                 (real *4)
c     nviews    : the number of slopes (angles)
c     nwide     : Maximum number of frequencies to be computed
c     thetal    : Curve function value (real *4) 
c     df        : Frequency step (real *4)
c     cutoffl   : Number of freq. needed  (cutoffl <= nwide)
c     ifh1      : ifh+1 where ifh is the highest freq. (integer)
c
c     dt        : "delta t" step size in t (real *4)
c     tmiddle   : Middle of the "t" range    
c
c	Output:        
c
c     x         : function theta values mod 2 Pi as appropriate
c                 to use in the exponent; may not be ordered;
c                 size ifh1 (real *4)
c
c     diag      : complex factors of size ifh1 (complex *8)
c
c     work      : tables of coefficients of size 
c                 13*nwide+13*ifh1+16 (real *4)
c
c __________________________________________________________________

     
c        subroutine ust2fini(flow,df,cutoffl,ifh1,nwide,thetal,nviews,
c     &                     tmiddle,x,diag,work)

        subroutine fbpt2fini(flow,df,cutoffl,ifh1,nwide,thetal,nviews,
     &                     tmiddle,x,diag,wsave,wrk)



        implicit real *4 (a-h,o-z)

        integer *4 cutoffl


        real *4 twopi,tmiddle,fact,argm,flowa,dfa
       
c         real *4 x(*),work(*)
        real *4 x(*),wrk(*)


        complex *8 diag(*),wsave(*)


        data twopi/6.2831853071795865/

c
c =======================================================================
c


c ----------------- computation of x ----------------------------------
c 

        flowl=thetal*flow/twopi
        dfl=thetal*df/twopi

        do ifr=1,cutoffl
           x(ifr)=flowl+(ifr-1)*dfl
           x(ifr)=x(ifr)-nint(x(ifr))
        enddo

c
c -------------------------------------------------------------------


c ----------------- computation of diagonal factor ---------------
c
        
        argm   = tmiddle*thetal
        flowa=flow*argm
        dfa=df*argm

        do ifr=1,cutoffl
           fact=flowa+(ifr-1)*dfa 
           diag(ifr)= cmplx(cos(fact),sin(fact))
        enddo

c
c -------------------------------------------------------------------


c ----------------- initialization of work vector ---------------
c

c         call fftini(2*nwide,1,work) 
        call ffti(nwide,wsave)
   
c        call ufs1i(nwide,cutoffl,x,work)
        call ufs1i(nwide,cutoffl,x,wrk)

c
c --------------------------------------------------------------


c
c ==============================================================
c

      return
      end



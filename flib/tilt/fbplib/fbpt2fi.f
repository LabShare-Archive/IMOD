c	$Log$
c	Revision 1.1.1.1  2001/11/21 23:12:01  rickg
c	Import into repository
c	
c 
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
c                                                                           c
c        This file is a part of                                             c
c        Unequally Spaced Fast Fourier Transform Library                    c
c        Contains proprietary information supplied by GB Consulting.        c
c        Copyright (C), 1998    GB Consulting. All rights reserved          c
c                                                                           c
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
c
c     It is an "envelope" routine calling ust2ini for each frequency
c     within the range of interest
c
c
c	Input:        
c
c     flow      : Lowest frequency (real *4)
c     cutoff    : Vector of size nviews with the number of 
c                 freq. needed for each angle  (integer *4)
c     df        : "delta" f (frequency step size)
c     ifh       : Highest frequence (integer)
c    
c     nviews    : The number of slopes (angles) 
c     theta     : Curve function values. Any explicit function.             
c
c     nwide     : The number of projections
c     nwidepad  : Size of oversampled data set
c
c	Output:        
c
c     xbuf      : TwoD table of function theta values mod 2 Pi of size 
c                 xbuf(ifh+1,nviews) (real *4)
c
c     diagbuf   : TwoD table of complex factors for each frequency of size 
c                 diagbuf(ifh+1,nviews) (complex *8)
c                 these are used to post-multiply the data
c
c     workbuf   : TwoD table of coefficients for all frequencies of size 
c                 workbuf(13*nwide+13*(ifh+1)+16,nviews) (real *4)
c                 (NOTE: frequency is a "slow" index)
c
c _____________________________________________________________



        subroutine fbpt2fi(df,flow,cutoff,ifh,nwidepad,nwide,nviews,
     &                    theta,xbuf,diagbuf,wsave,wrk)


        implicit real *4 (a-h,o-z)


        integer *4 cutoffl,nwidepad
        integer *4 cutoff(*)


        real *4 tmiddle,thetal
        real *4 theta(*)


        real *4 wrk((13*(ifh+1)+nwide),nviews)
  
        real *4 xbuf(ifh+1,nviews)
       

        complex *8 diagbuf(ifh+1,nviews)
        complex *8 wsave((4*nwide+8),nviews)


c
c =============================================================
c

        ifh1=ifh+1
        tmiddle = nwidepad/2 
 

        do l=1,nviews
           thetal=theta(l)
           cutoffl=cutoff(l)

 
           call fbpt2fini(flow,df,cutoffl,ifh1,nwide,thetal,nviews,
     &                   tmiddle,xbuf(1,l),diagbuf(1,l),
     &                   wsave(1,l),wrk(1,l))

       enddo


c
c =============================================================
c

        return
        end



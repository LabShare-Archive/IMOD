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
c     This is an "envelope" routine calling ansumini for each frequency
c     within the range of interest
c
c
c	Input:        
c
c     flow      : Lowest frequency (real *4)
c     df        : "delta" f (frequency step size)
c     ifl       : Low  end of the frequency range (integer)
c     ifh       : High end of the frequency range (integer)
c     nviews    : The number of slopes (angles)
c     theta1,
c     theta2    : Curve function values. Any explicit function.
c     xcen      : Center parameter for the projcetion data.
c     nthick    : The number of elements in the thick direction 
c
c
c	Output:        
c
c     xbuf      : TwoD table of function theta values mod 2 Pi of size 
c                 xbuf(nviews,ifh-ifl+1) (real *4)
c
c     diagbuf   : TwoD table of complex factors for each frequency of size 
c                 diagbuf(nviews,ifh-ifl+1) (complex *8)
c                 these are used to pre-multiply the data
c
c     workbuf   : TwoD table of coefficients for all frequencies of size 
c                 workbuf(13*nthick+13*nviews+16,ifh-ifl+1) (real *4)
c                 (NOTE: frequency is a "slow" index)
c
c_______________________________________________________________




        subroutine angsumi(ifl,ifh,df,flow,nthick,nviews,theta1,
     &                     theta2,xcen,xbuf,diagbuf,wsave,wrk)



        implicit real *4 (a-h,o-z)


        real *4 freq,xcen
        real *4 theta1(*),theta2(*)

        real *4 wrk(13*nviews+nthick,ifh-ifl+1)
        real *4 xbuf(nviews,ifh-ifl+1)


        complex *8 diagbuf(nviews,ifh-ifl+1),
     &             wsave(4*nthick+8,ifh-ifl+1)

c
c ================================================================
c

        do ifr=ifl,ifh
           freq    = flow+(ifr-ifl)*df
           ifrindex=ifr-ifl+1



           call angsumini(freq,nthick,theta1,theta2,xcen,
     &                    nviews,xbuf(1,ifrindex),diagbuf(1,ifrindex),     
     &                    wsave(1,ifrindex),wrk(1,ifrindex)) 

                    
         

        enddo

c
c ==============================================================
c

        return
        end


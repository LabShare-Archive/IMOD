c SUBROUTINE: 
c
c PURPOSE:     
c
c INPUT:      
c
c OUTPUT:    
c
c
c NOTE: 
c
c <010607 Kristian Sandberg
c
c 010607 Made ipol input and introduced ipow as a power for the 
c        sinc-interpolation filter (KS). 
c	  DNM change irmax to rmax, eliminate breakfreq
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       subroutine makefilter(filter,theta,cutoff,nviews,flow,df,ifh,
     &                       rmax,sdg,angles,ipol)       


       implicit real *4 (a-h,o-z)


       integer *4 cutoff(nviews)

       real *4 twopi,flow2,flowtp,df2,dftp,freq,bfreql,
     &         pifreq,pifreq2,avgint,atten,wsum,sumint

       real *4 theta(nviews),wincr(20),angles(nviews)    
  
       real *4 filter(ifh+1,nviews)  


       data pi/3.1415926535897932385/

c
c ============================================================
c


       twopi=2*pi
       flowtp=flow/twopi
       dftp=df/twopi

       flow2=flow/2
       df2=df/2
       

c_____________________ |w|-filter __________________________
c
    
c ----------- Set parameters for angle-dep. filter -----------
c

        wincr(1)=2.
        wincr(2)=2./3.

        avgint=(angles(nviews)-angles(1))/(nviews-1)

c
c -------------------------------------------


c	  DNM: changed to do everything in one loop on views

c ------------- Linear part ------------------------
c

       do l=1,nviews
          filter(1,l)=0
          atten=1.
          sumint=0.
          wsum=0.   
              
          do lw=1,2
             if (l-lw .gt. 0) then
                wsum=wsum+wincr(lw)
                sumint=sumint+wincr(lw)*
     &                 (angles(l+1-lw)-angles(l-lw))
             endif
             if (l+lw .le. nviews) then
                 wsum=wsum+wincr(lw)
                 sumint=sumint+wincr(lw)*
     &                  (angles(l+lw)-angles(l+lw-1))
             endif
           enddo
           atten=atten*(sumint/wsum)/avgint 

c            DNM: compute irmax for each view separately, add factor of 2 to
c	     make rmax be a frequency not a fraction of range
c
	   irmax=2.*rmax*cutoff(l)
           do ifr=2,min(irmax,cutoff(l))
              freq=flowtp+(ifr-1)*dftp    
	      filter(ifr,l)=atten*freq*theta(l)
	   enddo    

c	     DNM save the last frequency value as basis for cutoff instead
c	     of using fixed breakfreq 
c

	   bfreql=freq*theta(l)
c
c -----------------------------------------------


c ----------- Gaussian cutoff ---------------------
c

          do ifr=irmax+1,min(ifh+1,cutoff(l))
             freq=flowtp+(ifr-1)*dftp
c             bfreql=breakfreq*theta(l)
             filter(ifr,l)=filter(irmax,l)*
     &                     exp(-((freq*theta(l)-bfreql)/sdg)**2)
          enddo
       enddo

c
c ----------------------------------------------------


c
c____________________________________________________
  



c______________ interpolation ________________________________
c
c ipow introduced for higher order interpolation. This one is used
c to adjust the power of the sinc-filter which is the Fourier transform
c of the B-spline of corresponding order.	
c
 
        ipow=ipol+1
 
        do l=1,nviews
           filter(1,l)=theta(l)*filter(1,l)
        enddo

        do l=1,nviews
           do ifr=2,min(ifh+1,cutoff(l))
               pifreq=flow2+(ifr-1)*df2
               pifreq2=pifreq*theta(l)
               fact = theta(l)*(sin(pifreq2)/pifreq2)**ipow
               filter(ifr,l)=fact*filter(ifr,l)
            enddo
        enddo

c
c ______________________________________________________________


c
c ===============================================================
c

       return
       end

       

      

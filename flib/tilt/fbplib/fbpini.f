c SUBROUTINE: fbpini
c
c PURPOSE:    Intialize the fbp routine used for fast back projection. 
c
c INPUT:      angles        real*4(array)        Tilt angles.
c             Nprj          integer*4            Number of pixels perp. to
c                                                tilt axis of input image.
c             Nviews        integer*4            Number of tilt angles.
c             Nwide         integer*4            Size of output image in
c                                                wide direction.
c             Nthick        integer*4            Size of output image in
c                                                thick direction.
c             rmax          real*4               Maximum frequency
c                                                where the |omega|-filter
c                                                is used (0 <= rmax <= 0.5).
c             sdg           real*4               Standard deviation of 
c                                                Gaussian cutoff to follow
c                                                |omega|-filter if rmax<0.5.
c             interpol      integer*4            Order of interpolation.
c             inplace       integer*4            1 to pad input data in place
c             wrk           real*4(array)        Working array to be used
c                                                locally by this routine.
c                                                Can be used for other purposes
c                                                outside this routine.
c                                                Memory allocation: less than
c                                                needwrk returned by fbpneed
c            
c
c OUTPUT:     iw            integer*4(array)     Array with parameters to
c                                                be passed onto fbp-routine.
c                                                Memory allocation:
c                                                neediw returned by fbpneed
c             w             real*4(array)        Array with parameters to
c                                                be passed onto fbp-routine.
c                                                Memory allocation: 
c                                                needrw returned by fbpneed
c             zw            complex*8(array)     Array with parameters to
c                                                be passed onto fbp-routine.
c                                                Memory allocation: 
c                                                needzw returned by fbpneed
c
c
c NOTE: Only odd order interpolation is supported at the moment.  
c
c       Future version should store the filter in zw to include
c       some of the diagonal factors and higher order interpolation
c       which may give a complex filter.
c
c       This routine will print some numbers. These numbers are only
c       for performance measure and debugging. The two oversampling factors 
c       that are written gives the oversampling needed for Nprj and Nwide
c       respectively. The lower these factors are, the better. The first
c       factor should be between 1 and 3 and the second one between 1 and 2.
c       Higher oversampling factors than 3 and 2 respectively may give 
c       segmentation faults or defected output images. (The code can be
c       adjusted for this on the expense of more memory requirements.)
c
c	  
c Created: April 2, 2001 (Kristian Sandberg)
c
c Updates: July 18, 2001 Made interpol input to makefilter.f. (KS)
c	   October 28, 2001 The tan of the angles are now copied into w (the
c		            array that is passed to  fbp.f). This is necessary
c			    for handling vertical shifts.
c          November 7, 2001  DNM fixed expression for memory needs for zw
c          August 1, 2003   DNM added paddedin thickness
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine fbpini(angles,Nprj,Nviews,Nwide,Nthick,
     &                   rmax,sdg,interpol,inplace,
     &                   iw,w,zw,wrk)


           

       implicit real *4 (a-h,o-z)

       
       integer *4 iw(*)

       real *4 angles(*),w(*),wrk(*)

       complex *8 zw(*)

      
      

c
c ==============================================================
c






c ================ DISTRIBUTE WORKING ARRAYs ===============
c


c _________________ Real working array _____________________
c
c
c wrk array: 
c
c     c:      Nviews           ( Cos(angles) )
c     s:      Nviews           ( Sin(angles) )
c     t:      Nviews           ( tan(angles) )
c     ci:     Nviews           ( 1/cos(angles) )
c            
c 
c Total memory: 4*Nviews
c

c --------------- Trigonometric functions ------------
c

       iptc=1
       ipts=iptc+Nviews
       iptt=ipts+Nviews
       iptci=iptt+Nviews

c
c ---------------------------------------------------

c
c ___________________________________________________________






c _ Integer working area to be passed outside this routine _
c
c
c iw array:
c     co:      Nviews        (Cutoff frequencies)
c     smin:    Nviews        (Smallest projection to be used for each angle.)
c     smax:    Nviews        (Largest projection to be used for each angle.)
c     ip:      8             (Number of parameters packed, see end of code.)
c
c
c Total memory:   3*Nviews+8
c          

       iptco=1
       iptsmin=iptco+Nviews
       iptsmax=iptsmin+Nviews
       iptip=iptsmax+Nviews

c
c __________________________________________________________

c
c ===========================================================








c ========== COMPUTE TRIGONOMETRIC FUNCTIONS OF ANGLES ======
c
c c:  cos
c s:  sin
c t:  tan
c ci: 1/cos
c
      
       call trig_ang(angles,Nviews,wrk(iptc),
     &               wrk(ipts),wrk(iptt),wrk(iptci))

c
c ===========================================================

    





c ================ DERIVE PARAMETERS ========================
c
c Derivation of a number of parameters needed later on. For details,
c see header to der_par.
c

       call der_par(angles,wrk(iptc),wrk(iptt),Nprj,Nviews,
     &              Nwide,Nthick,
     &              Nwidep,NprjP,lenpad,sdgpad,
     &              nf,df,flow,ifl,ifh,
     &              iw(iptco),iw(iptsmin),iw(iptsmax),NthickP,
     &              widecen,thickcen)

c
c ===========================================================

       isize=Nviews*(Nwidep/2+1)
    




c === DISTRIBUTE  ARRAYS TO BE PASSED OUTSIDE THIS ROUTINE ==
c

c ___ Real working area to be passed outside this routine __
c
c
c w array:       
c
c     x1:      isize           (Phase parameters for  call to fbpt2f.)   
c     ini1:    (13*(NwideP/2+1)+NprjP)*Nviews   
c                              (Stuff needed by fbpt2f.)
c     x2:      isize           (Phase parameters for  call to angsum).
c     ini2:    (13*Nviews+NthickP)*(NwideP/2+1)
c                              (Stuff needed by angsum.) 
c     filt:    isize           (Filter, |omega| & interpolation filter.)
c     upad:    NprjP*Nviews    (Working area for fbp that has to be clean
c                               upon entry to fbp.)
c     rp:      6+Nviews        (Number of parameters packed, see end of code). 
c
c     
c Total memory: 3*Nviews*(NwideP/2+1)+Nviews*(13*(NwideP/2+1)+2*NprjP*Nviews+
c               +Nviews*(13*NwideP/2+13)+NthickP*(NwideP/2+1)+6+Nviews=
c               
c
c        ->     (30*Nviews+NthickP)*(Nwidep/2+1)+2*NprjP*Nviews+6
c
c	 or, only one NprojP*Nviews if padding in place
c	 (Do not rearrange the division by 2!)
c

       iptx1=1
       iptini1=iptx1+isize

       iptx2=iptini1+(13*(ifh+1)+NprjP)*Nviews
       iptini2=iptx2+isize
       
       iptfilt=iptini2+(13*Nviews+NthickP)*(ifh+1)

       iptupad=iptfilt+isize

c	 DNM: allocate space for padded data if not padding in place
       if(inplace.eq.0)then
	 iptrp=iptupad+NprjP*Nviews
       else
	 iptrp=iptupad
       endif
c
c __________________________________________________________






c _ Complex working area to be passed outside this routine _
c
c
c zw array:      
c
c     diag1:       isize               (Diagonal factors for fbpt2f.)   
c     fft1:        (4*NprjP+8)*Nviews  (Stuff to be used for FFT for fbpt2f.)
c     diag2:       isize               (Diagonal factors for angsum.) 
c     fft2:        (4*NthickP+8)*(NwideP/2+1)
c                                      (Stuff to be used for FFT for angsum.)
c     zu:          isize               (Working area for fbp that has to
c                                       to be clean upon entry to fbp.)
c
c
c Total memory:  3*(NwideP/2+1)*Nviews+(4*NprjP+8)*Nviews
c                +(4*NthickP+8)*(NwideP/2+1)=
c
c	->       (3*Nviews+4*NthickP+8)*(NwideP/2+1)+(4*NprjP+8)*Nviews
c	 DNM: Do not rearrange to divide Nviews by 2!
c

       iptdiag1=1
       iptfft1=1+isize

       iptdiag2=iptfft1+(4*NprjP+8)*Nviews
       iptfft2=iptdiag2+isize

       iptzu=iptfft2+(4*NthickP+8)*(NwideP/2+1)
      
c
c __________________________________________________________

c
c ===========================================================





  

c =================== INITIALIZE USFFT-ROUTINES =============
c

c _________________ fbpt2fi _________________________________
c
c Intialization of fbpt2f ("Fast Back Projection Time To Fourier") 
c routine (called from fbp).
c

       call fbpt2fi(df,flow,iw(iptco),ifh,Nwidep,NprjP,Nviews,
     &              wrk(iptci),w(iptx1),
     &              zw(iptdiag1),zw(iptfft1),w(iptini1))

c
c ___________________________________________________________


          
 

c ________________ angsumi __________________________________
c
c Intialization of angsum ("Angular Summation") routine called from fbp.
c

       call angsumi(ifl,ifh,df,flow,NthickP,Nviews,wrk(iptci),
     &              wrk(iptt),widecen,w(iptx2),zw(iptdiag2),
     &              zw(iptfft2),w(iptini2))

c
c ___________________________________________________________
 
c
c ===========================================================


     




c ============== MAKE FILTER ================================
c
c Construct filter. ( |omega|*interpolation -filter.)
c
c	 DNM change irmax to rmax, sdgpad to sdg, eliminate breakfreq, add
c	 interpol
c
       call makefilter(w(iptfilt),wrk(iptci),iw(iptco),Nviews,
     &                 flow,df,ifh,rmax,sdg,angles,interpol) 

c
c ===========================================================





      
c =========== PACK PARAMETERS INTO iw AND w ARRAYS ==========
c

c	 DNM: eliminate irmax and breakfreq
c	 replace irmax with NthickP
       
c_______________ Integer parameters _________________________
c

       iw(iptip)=NwideP
       iw(iptip+1)=NprjP
       iw(iptip+2)=lenpad
       iw(iptip+3)=nf
       iw(iptip+4)=ifh
       iw(iptip+5)=NthickP
       iw(iptip+6)=iptrp

c	 DNM: pass the inplace flag to fbp also
c
       iw(iptip+7)=inplace

c
c ___________________________________________________________




c ______________ Real parameters ____________________________ 
c

       w(iptrp)=sdgpad
       w(iptrp+1)=df
       w(iptrp+2)=flow
c       w(iptrp+3)=breakfreq
       w(iptrp+4)=widecen
       w(iptrp+5)=thickcen

       do l=0,Nviews-1
          w(iptrp+6+l)=wrk(iptt+l)
       enddo	

c
c ___________________________________________________________

c
c ===========================================================


     


c =============== CLEAN WORKING AREA ========================
c
 
c _____________ Real ________________________________________
c
c	 DNM: clear only if not doing pad in place
       if(inplace.eq.0)then
	 do i=0,NprjP*Nviews-1
	   w(iptupad+i)=0.0
	 enddo
       endif
c
c ___________________________________________________________



c __________ Complex ________________________________________
c

       do i=0,isize-1
          zw(iptzu+i)=cmplx(0.0,0.0)
       enddo

c
c ___________________________________________________________


c
c ===========================================================       


     

c
c ==============================================================
c

       return
       end

c SUBROUTINE:  fbp
c
c PURPOSE: Reconstruct one slice perpendicular to tilt axis.     
c
c INPUT:  u        real*4(array)          Input slice. One slice (Nprj*Nviews)
c                			  perpendicular to tilt axis. 
c	  NprjDim  integer*4              X dimension of input slice - 
c                                         spacing between lines of Nprj points
c         Nprj     integer*4              Number of pixels in input image perp.
c                			  to tilt axis.
c         Nviews   integer*4              Number of tilt angles.
c         Nwide    integer*4              Size of output image in wide 
c                			  direction.
c         Nthick   integer*4              Size of output image in thick 
c                			  direction.
c	  ishift   integer*4		  Flag whether to shift vertically.
c	  shift    real*4                 Vertical shift (in pixels).
c         iw       integer*4(array)       Initialized data from fbpini routine.
c         rw       real*4(array)          Initialized data from fbpini routine.
c         zw       complex*8(array)       Initialized data from fbpini routine.
c         wrk      real*4(array)          Working area. Memory usage:
c                                         needwrk returned by fbpneed
c         zwrk     complex*8(array)       Working area. Memory usage:
c                                         needzwrk returned by fbpneed
c
c
c OUTPUT: uout     real*4                 Output image (Nwide*Nthick).   
c
c
c NOTE: 
c
c
c Created: April 2, 2001 (Kristian Sandberg)
c 010504  DNM added the NprjDim argument to allow a subset of the width to
c	  be reconstructed efficiently.  Changed to trim output image in place
c	  (always) and to pad the input image in place if specified in the
c	  call to fbpini
c Last modified: October 29, 2001 (KS)
c 		 Two new input parameters (ishift and shift). Call to
c		 angsum changed (two new input parameters). New local
c		 parameter dfs introduced.
c 011107  DNM changed to do angular summation into the output array then
c         do the final transform in place in that array
c 030801  DNM changed to use padded thickness
c 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        subroutine fbp(u,NprjDim,Nprj,Nviews,Nwide,Nthick,
     &	               ishift,shift,
     &                 uout,iw,rw,zw,wrk,zwrk)



        implicit real *4 (a-h,o-z)


        integer *4 iw(*)

        real *4 u(*),uout(*),rw(*),wrk(*)

        complex *8 zw(*),zwrk(*)


c
c ==============================================================
c



c ============= EXTRACT PARAMETERS FROM iw AND rw ==========
c

c _______________ Integers _________________________________
c

        itemp1=3*Nviews

        NwideP=iw(itemp1+1)
        NprjP=iw(itemp1+2)
        lenpad=iw(itemp1+3)
c        nf=iw(itemp1+4)
        ifh=iw(itemp1+5)
        NthickP=iw(itemp1+6)
        iptrp=iw(itemp1+7)
	inplace=iw(itemp1+8)
        ifl=0


c
c __________________________________________________________




c ____________ Reals _______________________________________
c
c sdgpad: Some kind of standard deviation for padding.
c dfs:    Slice dependent constant needed for vertical shifts.
c	  

       sdgpad=rw(iptrp)
       dfs=-shift*rw(iptrp+1)

c
c __________________________________________________________


c
c ==========================================================

       



c ================ SET MEMORY POINTERS =====================
c

       ifhp1=ifh+1
       isize=Nviews*(Nwidep/2+1)


c ________________ Real working array ______________________
c
c wrk:
c
c     rwrk1:      4*NwideP+15       (Working area 1 for f2t.)
c     rwrk2:      2*NwideP          (Working area 2 for f2t.)  
c
c	 DNM: not needed, output area now supplied by caller as uout
ccc     uout:       NwideP*NthickP     (Padded output image.)
c
c
c Total memory: 6*NwideP+15
c

       iptrwrk1=1
       iptrwrk2=iptrwrk1+4*NwideP+15
c       iptuout=iptrwrk2+2*NwideP

c
c _________________________________________________________




c ________________ Complex working array __________________
c
c
c zwrk:
c
c     wrk1:         3*NprjP+NwideP/2+1  
c                                  (Working area for fbpt2f.)
c
c	 DNM: eliminate, do final transform in place in uout
ccc     zuout:        (NwideP/2+1)*NthickP
c                                  (Fourier representation after
c                                   summation.)
c     wrk2:         3*NthickP+Nviews       
c                                  (Working area for angsum.)
c
c
ccc Total memory:  NwideP*NthickP/2+3*NprjP+NwideP/2+4*NthickP+Nviews+1
c Total memory:  3*NprjP+NwideP/2+3*NthickP+Nviews+1
c

       iptwrk1=1       
       iptzuout=iptwrk1+3*NprjP+ifhp1
c       iptwrk2=iptzuout+NthickP*ifhp1
       iptwrk2=iptzuout

c
c _________________________________________________________


     


c ________________ iw array _______________________________
c

       iptco=1
       iptsmin=iptco+Nviews
       iptsmax=iptsmin+Nviews

c
c _________________________________________________________





c ________________ rw array _______________________________
c

       iptx1=1
       iptini1=iptx1+isize

       iptx2=iptini1+(13*(ifhp1)+NprjP)*Nviews
       iptini2=iptx2+isize
       
       iptfilt=iptini2+(13*Nviews+NthickP)*ifhp1

       iptupad=iptfilt+isize

c
c _________________________________________________________



c ________________ zw array _______________________________
c

       iptdiag1=1
       iptfft1=1+isize

       iptdiag2=iptfft1+(4*NprjP+8)*Nviews
       iptfft2=iptdiag2+isize

       iptzu=iptfft2+(4*NthickP+8)*(NwideP/2+1)

c
c _________________________________________________________


c
c ==========================================================


      



c ================== PAD DATA ==============================
c	 
c	 DNM: pad into working array or in place
c
       if(inplace.eq.0)then
	 call pad(u,NprjDim,Nprj,Nviews,NprjP,lenpad,sdgpad,
     &	     iw(iptsmin),iw(iptsmax),rw(iptupad)) 
       else
	 call pad(u,NprjDim,Nprj,Nviews,NprjP,lenpad,sdgpad,
     &	     iw(iptsmin),iw(iptsmax),u)
       endif
    
c
c ==========================================================
     

      


       
c ====================== BACK PROJECT  ======================
c


c ______________________ USFFT (of proj. data) ______________
c

c	 
c	 DNM: use the working array or the caller's data array
c
	if(inplace.eq.0)then
	  call fbpt2f(rw(iptupad),NprjP,NprjP,Nviews,
     &	      zw(iptzu),ifhp1,iw(iptco),ifh,
     &	      rw(iptx1),zw(iptdiag1),rw(iptini1),
     &	      zw(iptfft1),zwrk(iptwrk1))
	else
	  call fbpt2f(u,NprjP,NprjP,Nviews,
     &	      zw(iptzu),ifhp1,iw(iptco),ifh,
     &	      rw(iptx1),zw(iptdiag1),rw(iptini1),
     &	      zw(iptfft1),zwrk(iptwrk1))
	endif

c
c ___________________________________________________________

    
      
            


c __ Premultiplication (incl. filtering and interpolation) __
c

        call diagmult(zw(iptzu),ifhp1,Nviews,
     &                rw(iptfilt),iw(iptco),ifh)  
     
c
c ___________________________________________________________     


   
    


c __________________ USFFT (summation) ______________________
c
c	  DNM: change zwrk(iptzuout) to uout

	  call angsum(zw(iptzu),ifhp1,uout,ifhp1,
     &	      ifl,ifh,Nviews,NthickP,
     &	      ishift,dfs,rw(iptrp+6),
     &	      rw(iptx2),zw(iptdiag2),rw(iptini2),
     &	      zw(iptfft2),zwrk(iptwrk2))
      
c
c____________________________________________________________

      
  

    

c ________ TRANSFORM INTO SPACE DOMAIN (eq. spaced FFT) _____
c
 
         isign = -1

c	   DNM: change wrk(iptuout) to uout
c	   DNM: change zwrk(iptzuout) to uout and add 2 to dimension

         call f2t(uout,Nwidep+2,Nwidep,NthickP,ifhp1,
     &            uout,wrk(iptrwrk1),wrk(iptrwrk2),isign)

c
c ___________________________________________________________




c ______________ Crop image (get rid of padding) ____________
c
c	   DNM: change wrk(iptuout) to uout to crop in place, add dimension
c
         call crop(uout,uout,Nwidep+2,Nwidep,Nwide,NthickP,Nthick)

c
c ___________________________________________________________



c
c ===========================================================




c
c ==============================================================
c




         return
         end

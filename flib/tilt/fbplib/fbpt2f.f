c SUBROUTINE: fbpt2f
c
c PURPOSE:    Go from space domain to Fourier domain. 
c
c INPUT:      uin        real*4(array)        Input data (padded).
c             lduin      integer*4            Leading dimension of uin
c                                             (=NprjP in present code.)
c             NprjP      integer*4            Size of padded array.
c             Nviews     integer*4            Number of tilt angles.
c             ldzuout    integer*4            Leading dimension of output
c                                             array (ifh+1 in present code.)
c             cutoff     integer*4(array)     Index of highest freqeuncy 
c                                             needed for each angle.
c             ifh        integer*4            Highest frequency (=max(cutoff)).
c             x          real*4(array)        Nodes between (-0.5,0.5).
c             diag       complex*8(array)     Diagonal factors.
c             wini       complex*8(array)     Initialization data for USFFT.
c             wsave      complex*8(array)     Initialization data for fft.
c             zwrk       complex*8(array)     Working area (used locally for
c                                             this routine). 
c                                             Memory: 4*NprjP
c             
c
c OUTPUT:     zuout      complex*8(array)     Array with Fourier coeff.
c                                             for (non-negative) freq.    
c
c
c NOTE:       The arrays x, diag, wini and wsave are used by the USFFT
c             and are pre-computed by the routine fbpt2fi called from 
c             fbpini.
c
c Created: April 2, 2001 (Kristian Sandberg)
c Last updated: October 27, 2001 (KS) Introduced the local constants
c		iptinp1 and iptoutm1.
c		
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        subroutine fbpt2f(uin,lduin,NprjP,Nviews,
     &                    zuout,ldzuout,cutoff,ifh,
     &                    x,diag,wini,wsave,zwrk)



        implicit real *4 (a-h,o-z)

        
        integer *4 cutoffl,cutoff(*)

        real *4 uin(lduin,Nviews), x(ifh+1,Nviews),
     &          wini(13*(ifh+1)+NprjP,Nviews)
             
        complex *8  zuout(ldzuout,Nviews),diag(ifh+1,Nviews),
     &              wsave(4*NprjP+8,Nviews),zwrk(*)
        

c
c ==============================================================
c

      
      


c ================= SET CONSTANTS AND POINTERS ==============
c
c in:     NprjP         (Copy of one row of complex data to send
c                        in to fbpt2f0.)
c out:    ifhp1         (Copy (half a row +1) of complex data from
c                        fbpt2f0. This piece of row corresponds toc
c                        positive wavenumbers. )
c wrk:    2*NprjP       (Working array for fbpt2f0.) 
c
c
c Total memory: 3*NprjP+ifhp1 = (ifhp1=NwideP/2+1) =
c     ->        3*NprjP+NwideP/2+1 
c

        ifh1=ifh+1
       
        iptin=0
        iptout=iptin+NprjP+1
        iptwrk=iptout+ifh1
	
	iptinp1=iptin+1
	iptoutm1=iptout-1
   
c
c ===========================================================    


 


c ================= USFFT ===================================
c

        do 1000 l=1,Nviews
   
           cutoffl=cutoff(l)

           do 2000 i=1,NprjP
              zwrk(iptin+i)=cmplx(uin(i,l),0)
 2000      continue


        call fbpt2f0(zwrk(iptinp1),zwrk(iptout),NprjP,cutoffl,
     &               x(1,l),diag(1,l),wini(1,l),wsave(1,l),zwrk(iptwrk))

    
        do 3000 ifr=1,cutoffl          
           zuout(ifr,l)=zwrk(iptoutm1+ifr)
 3000   continue  
 

 1000   continue

c
c ===========================================================  


c
c ==============================================================
c

      return
      end


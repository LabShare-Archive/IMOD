c SUBROUTINE: angsum
c
c PURPOSE:    Perform angular summation in Fourier space.
c
c INPUT:      zuin      complex*8(array)   Input data (=output from fbpt2f).
c             ldzuin    integer*4          Leading dimension of zuin 
c                                          (=ifh+1)
c             ldzuout   integer*4          Leading dimension of zuout 
c                                          (=ifh+1)
c             ifl       integer*4          Index of lowest frequency.
c             ifh       integer*4          Index of highest frequency.
c             Nviews    integer*4          Number of tilt angles.
c             Nthick    integer*4          Size of final image in thick
c                                          direction.
c	      ishift    integer*4          Flag whether to apply vertical shift
c					   or not.
c	      dfs	real*4		   Shift constant (slice dependent, see
c					   fpb.f)
c	      tanang	real*4(array)	   tan(angles) Precomputed in fbpini.f	
c             x         real*4(array)      Nodes between (-0.5,0.5)
c             diag      complex*8(array)   Diagonal factors.
c             wini      real*4(array)      Initialization stuff for angsum.
c             wsave     complex*8(array)   Initialization stuff for fft to
c                                          be used by angsum.
c             zwrk      complex*8(array)   Working area (memory: see code)
c 
c
c
c OUTPUT:    zuout      complex*8(array)   Output array corresponding to
c                                          output image in Fourier space
c                                          with respect to Nwide direction.
c
c
c NOTE: The arrays x,diag,wini and wsave are pre-computed in angsumi inside
c       fbpini.
c
c Created: April 2, 2001 (Kristian Sandberg)
c Last updated: October 27, 2001 (KS) Introduced the local constants
c		iflm1, iptuinm1 and iptoutm1. New input parameters.
c		Vertical shifts implemented.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


     
       subroutine angsum(zuin,ldzuin,zuout,ldzuout,
     &                    ifl,ifh,Nviews,Nthick,
     &                    ishift,dfs,tanang,
     &                    x,diag,wini,wsave,zwrk)


        implicit real *4 (a-h,o-z)


	 real *4 x(nviews,ifh-ifl+1),tanang(*),	 
     &           wini(13*nviews+nthick,ifh-ifl+1)

        complex *8 zuin(ldzuin,nviews),zuout(ldzuout,nthick),    
     &             diag(nviews,ifh-ifl+1),
     &             wsave(4*nthick+8,ifh-ifl+1),zwrk(*)

 

c
c ============================================================
c



c ============== SET POINTERS ============================
c
c Nviews:   uin  (Input to angsum0)
c Nthick:   uout (Output from angsum0)
c 2*Nthick: wrk  (Working array for angsum0)
c
c Total memory: 3*Nthick+Nviews
c
   
      iptuin=1
      iptuout=iptuin+Nviews
      iptwrk=iptuout+Nthick
      
      iflm1=ifl-1
      iptuinm1=iptuin-1
      iptoutm1=iptuout-1

c
c ========================================================





c ================= SUM ==================================
c

        do 1000 ifr=ifl,ifh
          
           ifrindex=ifr-iflm1 
	   argm=ifr*dfs 
           
           do 2000 l=1,nviews
              zwrk(iptuinm1+l)=zuin(ifrindex,l)
 2000      continue
   	   
           call angsum0(zwrk(iptuin),nthick,zwrk(iptuout),
     &                  nviews,x(1,ifrindex), 
     &                  ishift,tanang,argm,
     &                  diag(1,ifrindex),wini(1,ifrindex),
     &                  wsave(1,ifrindex),zwrk(iptwrk))                


           do 3000 ith=1,nthick
              zuout(ifrindex,ith) = zwrk(iptoutm1+ith)
 3000      continue


 1000   continue

c
c ========================================================



c
c ============================================================
c 

      return
      end



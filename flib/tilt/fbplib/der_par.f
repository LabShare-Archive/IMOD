c SUBROUTINE: der_par.f
c
c PURPOSE:    To derive a number of parameters needed throughout
c             the back projection. 
c
c INPUT:      angles      real*4(array)      Array with tilt angles.
c             c           real*4(array)      Array with cos(angles).
c             t           real*4(array)      Array with tan(angles).
c             Nprj        integer*4          Number of projections
c                                            perp to tilt axis.
c             Nviews      integer*4          Number of angles.
c             Nwide       integer*4          Size in wide diretion. 
c             Nthick      integer*4          Size in thick direction.
c                                            of output image.
c
c OUTPUT:     Nwidep      integer*4          Size of extended (periodized)
c                                            output image.
c             NprjP       integer*4          Size of padded input data.
c             lenpad      integer*4          Length of padding region on each
c                                            side.
c             sdgpad      real*4             Standard deviation of Gaussian
c                                            padding of input data.
c             nf          integer*4          Number of frequencies in wide
c                                            direction (unless de-noising,
c                                            nf=Nwidep).
c             df          real*4             "delta phase" (=2pi/nf).
c             flow        real*4             Lowest frequency to use. 
c                                            Zero by default.
c             ifl         real*4             Index of lowest frequency to use. 
c                                            Zero by default.
c             ifh         integer*4          Highest frequency to use.
c                                            (=nf/2 by default.)
c             cutoff      integer*4(array)   Array where each index correspond
c                                            to an angle that keeps track of
c                                            the last frequency needed. (This
c                                            comes from "stretching" the data
c                                            in Fourier space and the effect
c                                            of this stretching on the 
c                                            |omega|-filter. 
c             supmin      integer*4(array)   Smallest projection index
c                                            contributing to output image
c                                            for each angle.
c             supmax      integer*4(array)   Largest projection index
c                                            contributing to output image
c                                            for each angle.
c	      NthickP     integer*4          Padded thickness of output image
c             widecen     real*4             Center of coord-system in wide
c                                            direction.
c             thickcen    real*4             Center of coord-system in thick
c                                            direction.
c                                                           
c
c
c NOTE:   Parameters that can be varied are lenpad and decay that adjust
c         the Gaussian padding of the input data.
c
c         SETTING FOR NwideP and sdgpad TEMPORARILY CHANGED FOR DEBUGGING!!!
c	  
c	  DNM: eliminated rmax and sdg as input parameters, and irmax and
c	  breakfreq as output parameters; the latter are no longer used
c	  Added thickness padding parameter
c
c 010506
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine der_par(angles,c,t,Nprj,Nviews,
     &                    Nwide,Nthick,
     &                    Nwidep,NprjP,lenpad,sdgpad,
     &                    nf,df,flow,ifl,ifh,
     &                    cutoff,supmin,supmax,NthickP,
     &                    widecen,thickcen)


       implicit real *4 (a-h,o-z)

       integer *4 cutoff(*),supmin(*),supmax(*)

       real *4 angles(*),c(*),t(*)
 
       data pi/3.1415926535897932385/

c
c ==============================================================
c




c ============= SET CONSTANTS ===============================
c
c lenpad: The minimum number of pixels of the padding region.
c decay:  The last element in the padding region will be multiplied
c         by this number.
c

       pi2=pi*2.0
       lenpad=40
       decay=0.5E-02

c
c ===========================================================







c ============ DERIVE PARAMETERS ============================
c
      
c __________________ Derive padding parameters ______________
c


c ----------------- For output image ------------------------
c

       if (abs(angles(1)) .gt. abs(angles(Nviews))) then
          Ntemp=Nthick*abs(t(1))+Nprj+1
       else
          Ntemp=Nthick*abs(t(Nviews))+Nprj+1
       endif

       call nice_pad(Ntemp,Nwidep)

cccc COMMENT OUT LATER!!!
c
c       Nwidep=300
c
ccccccccccccc

       call prinf('  NwideP:*',NwideP,1)
       temp=real(NwideP)/Nwide
       call prin('  Oversampling factor:*',temp,1)

c
c ------------------ Pad the thickness too -----------------
c
	call nice_pad(Nthick, NthickP)

c -------------- For input data -----------------------------
c

       call nice_pad(Nprj+lenpad,NprjP)
       lenpad=(NprjP-Nprj)/2
       sdgpad=lenpad**2/log(1.0/decay)

cccc COMMENT OUT LATER!!!
c
c       sdgpad=120
c
ccccccccccccc


       call prinf('  NprjP:*',NprjP,1)
       temp=real(NprjP)/Nprj
       call prin('  Oversampling factor:*',temp,1)


c
c -----------------------------------------------------------


c
c ___________________________________________________________





c __________________ Derive frequency parameters ____________
c

       nf=nwidep
       df=pi2/nf
       
       flow=0
       ifl=flow
       ifh=nf/2

c	 DNM: eliminate these, not needed
c       irmax=rmax*ifh
c       breakfreq=(flow+(irmax-1)*df)/pi2

       ifhp1=ifh+1
       do l=1,Nviews
          cutoff(l)=ifhp1*c(l)
       enddo

c
c ___________________________________________________________




c ________________ Derive center parameters _________________
c

       widecen=Nwidep/2-0.5
       thickcen=NthickP/2+0.5

c
c ___________________________________________________________



c ________________ Compute support of used data _____________
c

       M2=Nwide/2
       N2=NthickP/2
c	 
c	 DNM: changed to make the supports centered on nrpj/2 not nwide/2,
c	 and limited by nprj not nwide
c
       np2=Nprj/2
       
       do l=1,Nviews
          if (angles(l) .lt. 0) then
             supmin(l)=max(1,
     &                 int(np2-M2*cos(angles(l))+N2*sin(angles(l))-1))
             supmax(l)=min(nprj,
     &                 int(np2+M2*cos(angles(l))
     &                          -N2*sin(angles(l))+1)+1)    
          else
             supmin(l)=max(1,
     &                 int(np2-M2*cos(angles(l))-N2*sin(angles(l))-1))
             supmax(l)=min(nprj,
     &                 int(np2+M2*cos(angles(l))
     &                          +N2*sin(angles(l))+1)+1)
          endif
	enddo  

c
c __________________________________________________________


c
c ===========================================================



c
c ==============================================================
c

       return
       end

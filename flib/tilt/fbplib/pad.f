c SUBROUTINE: pad
c
c PURPOSE:    Pad input data (extend data in Nprj-direction with a 
c             smooth window). 
c
c INPUT:      ucp           real*4(array)      Input slice.
c	      NPrjDim       integer*4          X-dimension of ucp array
c             Nprj          integer*4          Number of projections
c                                              perp. to tilt axis.
c             Nviews        integer*4          Number of tilt angles.
c             NprjP         integer*4          Length of padded data
c                                              (in Nprj direction).
c             lenpad        integer*4          (NprjP-Nprj)/2 (length of
c                                              padding window on each side
c                                              of input data).
c             sdg           real*4             Standard deviation of Gaussian
c                                              window.
c             supmin        integer*4(array)   First projection needed (for 
c                                              each angle).
c             supmax        integer*4(array)   Lasst projection needed (for 
c                                              each angle).
c             
c         
c
c OUTPUT:    upad           real*4(array)      Padded input data.
c
c
c NOTE: Future version may use a better window than the Gaussian.
c
c 010314
c 010504 DNM: Changed to allow data to be padded in place, by adding argument
c	  for X dimension of the incoming data, doing the loops backwards when
c	  necessary, zeroing the area outside the pad, and consolidating all
c	  operations into one loop on views
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    
       subroutine pad(ucp,NprjDim,Nprj,Nviews,NprjP,lenpad,sdg,
     &                supmin,supmax,upad) 

 

       implicit real*4 (a-h,o-z)

       
       integer *4 supmin(*),supmax(*)

       real *4 ucp(NprjDim,Nviews),upad(NprjP,Nviews)

      
c
c =============================================================
c

     

       NprjP2=(NprjP-Nprj)/2
c	 
c	 invert the order of lines if the destination lines are longer
c
	if(NprjDim.ge.NprjP)then
	  lstart=1
	  lend=nviews
	  linc=1
	else
	  lstart=nviews
	  lend=1
	  linc=-1
	endif
c
       do l=lstart,lend,linc
     
c ============ SUBTRACT OFF THE AVERAGE OF THE ENDPOINTS AND =======
c =============== COPY DATA INTO MIDDLE OF PADDED ARRAY =======
c
          average=(ucp(supmin(l),l)+ucp(supmax(l),l))/2.

c	    
c	    for this line, invert the order if absolute location in array
c	    is higher in destination
c	    
	  if((l-1)*NprjDim .ge. (l-1)*NprjP+NprjP2)then
	    istart=supmin(l)
	    iend=supmax(l)
	    iinc=1
	  else
	    istart=supmax(l)
	    iend=supmin(l)
	    iinc=-1
	  endif	    
          do i=istart,iend,iinc
             upad(i+NprjP2,l)=ucp(i,l)-average
          enddo

	  minsuppad=supmin(l)+NprjP2
	  maxsuppad=supmax(l)+NprjP2
	  ucpmin=upad(minsuppad,l)
	  ucpmax=upad(maxsuppad,l)
      
c =================  SMOOTH PADDING ===========================
c =================  ZERO ARRAY OUTSIDE THE PAD ===============
c
     
	  do i=1,minsuppad-lenpad-1
	    upad(i,l)=0.
	  enddo

          do i=minsuppad-lenpad,minsuppad-1
             upad(i,l)=ucpmin*exp(-(i-minsuppad)**2/sdg)
          enddo

          do i=maxsuppad+1,maxsuppad+lenpad
             upad(i,l)=ucpmax*exp(-(i-maxsuppad)**2/sdg)
          enddo

	  do i=maxsuppad+lenpad+1,NprjP
	    upad(i,l)=0.	
	  enddo

       enddo

c
c =================================================================
c

       return
       end

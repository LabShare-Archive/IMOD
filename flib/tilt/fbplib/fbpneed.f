c SUBROUTINE: fbpneed
c	  
c PURPOSE: preinitialize to determine working storage needs
c
c INPUT:  angles        real*4(array)        Tilt angles.
c	  Nprj          integer*4            Number of pixels perp. to
c                                            tilt axis of input image.
c	  Nviews        integer*4            Number of tilt angles.
c	  Nwide         integer*4            Size of output image in
c                                            wide direction.
c	  Nthick        integer*4            Size of output image in
c                                            thick direction.
c	  inplace       integer*4            1 to pad input data in place
c	  iw            integer*4 (array)    working space, Nviews*3 needed
c	  rw            real*4 (array)       working space, Nviews*4 needed
c	  
c OUTPUT: nprjp         integer*4            Padded size of input image
c	  nwidep   	integer*4            Size of extended output image
c	  needwrk  	integer*4            Space needed for real working
c	                                     arrays
c	  needzwrk 	integer*4            Space needed for complex working 
c	                                     arrays
c	  neediw   	integer*4            Space needed for the iw array
c	  needrw   	integer*4            Space needed for the rw array
c	  needzw   	integer*4            Space needed for the zw array
c	  needout  	integer*4            Space needed for extended output
c                                            image
c	  minsup   	integer*4            Minimum index of data needed
c	  maxsup   	integer*4            Maximum index of data needed
c 
c	DNM: This subroutine has been added to provide the calling
c	program with definitive information about storage requirements.
c	It calls trig_ang and der_par just as fbpini does, and it uses the
c	memory allocations described in comments in fbpini.f and in fbp.f to
c	provide the necessary information.
c	  
c 010504
c
       subroutine fbpneed(angles,Nprj,Nviews,Nwide,Nthick,inplace,
     &	    iw,wrk,nprjp,nwidep,needwrk,needzwrk,neediw,needrw,needzw,
     &	    needout,minsup,maxsup)
           

       implicit real *4 (a-h,o-z)

       
       integer *4 iw(*)

       real *4 angles(*),wrk(*)
c	 
c	 call trig_ang to get cosines and tangents
c
       iptc=1
       ipts=iptc+Nviews
       iptt=ipts+Nviews
       iptci=iptt+Nviews
       call trig_ang(angles,Nviews,wrk(iptc),
     &               wrk(ipts),wrk(iptt),wrk(iptci))

c	 
c	 call der_par to get all other parameters
c
       iptco=1
       iptsmin=iptco+Nviews
       iptsmax=iptsmin+Nviews
       call der_par(angles,wrk(iptc),wrk(iptt),Nprj,Nviews,
     &              Nwide,Nthick,
     &              Nwidep,NprjP,lenpad,sdgpad,
     &              nf,df,flow,ifl,ifh,
     &              iw(iptco),iw(iptsmin),iw(iptsmax),NthickP,
     &              widecen,thickcen)

c	 
c	 compute the needed space based on all the requirements listed above
c	 and in fbp.f
c
	needwrk=max((4*Nviews), (6*NwideP+15))
	needzwrk=(3*NprjP+NwideP/2+3*NthickP+Nviews+1)
	neediw=(3*Nviews+8)
	needrw=((30*Nviews+NthickP)*(Nwidep/2+1)+2*NprjP*Nviews+6)
	if(inplace.ne.0)needrw=needrw-NprjP*Nviews
	needzw=((3*Nviews+4*NthickP+8)*(NwideP/2+1)+(4*NprjP+8)*Nviews)
	needout=(NwideP+2)*NthickP

c	  
c	  get the min and max support indexes
c
	minsup=nprj
	maxsup=1
	do i=0,nviews-1
	  minsup=min(minsup,iw(iptsmin+i))
	  maxsup=max(maxsup,iw(iptsmax+i))
	enddo

	return
	end

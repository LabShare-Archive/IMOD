c SUBROUTINE: 	angsum0.f
c
c PURPOSE:      Sum over angles in Fourier space
c
c INPUT:      	up	complex*8(array)	Input data
c		Nthick	integer*4		Thickness.
c		Nviews  integer*4		Number of angles.
c		x	real*4(array)		Phases (pre-computed in
c						angsumi.f.
c		ishift	integer*4		Flag whether to apply vertical
c						shift or not.
c		tanang	real*4(array)		tan(angles)
c		argm	real*4			Vertical shift parameter.
c		diag	complex*8		Diagonal factors (pre-computed
c						in angsumi.f)
c		wrk,wsave
c			complex*8(array)	Arrays with pre-computed info
c						(see angsumi.f)
c		work	complex*8(array)	Working area.		
c
c OUTPUT:    	utr	complex*8(array)	Output data
c
c
c NOTE: 
c
c Created: December 12, 2001 (Kristian Sandberg)
c Updates: Octobber 28, 2001 (KS) Can now handle vertical shifts.
c				  nc re-named to Nthick.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


	subroutine angsum0(up,Nthick,utr,nviews,x,
     &                     ishift,tanang,argm,
     &                     diag,wrk,wsave,work)


         implicit real *4 (a-h,o-z)


	 real *4 x(*),tanang(*),wrk(*)

  	 complex *8  up(*),utr(*),diag(*),wsave(*),work(*),
     &               ysfact
       
c
c ===============================================================
c

   
	if (ishift .eq. 1) then
  
           do l=1,Nviews
	
 	      temp=argm*tanang(l)
	      ysfact=cmplx(cos(temp),sin(temp))
	
              up(l)=up(l)*diag(l)*ysfact
           enddo
	
	else
	
	    do l=1,Nviews
               up(l)=up(l)*diag(l)
	    enddo
	    
	endif

        isign = -1
     

        call ufs1to(Nthick,utr,nviews,x,up,isign,wrk,wsave,work)

c
c ===============================================================
c

      return
      end



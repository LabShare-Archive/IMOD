c SUBROUTINE: crop
c
c PURPOSE: Crop a image in the wide direction.    
c
c INPUT:   upad        real*4(array)    Padded image.   
c	   Nwidim      integer*4        Dimension of upad in wide direction
c          NwideP      integer*4        Padded size in wide direction.
c          Nwide       integer*4        Final size in wide direction.
c          NthickP     integer*4        Padded size in thick direction.
c          Nthick      integer*4        Final size in thick direction.
c           
c
c OUTPUT:  u           real*4(array)    Output image (Nwide*Nthick)  
c
c
c NOTE: 
c
c 010326
c 011107   DNM added dimension argument to deal with image transformed in place
c 030802   DNM added padded thickness argument to crop out padded thickness
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine crop(upad,u,Nwidim,Nwidep,Nwide,NthickP,Nthick)
      
      implicit real*4(a-h,o-z)

      real *4 upad(*),u(*)
 

c
c =================================================
c
c	apparently for odd sizes there is more padding on the lower side in
c	both dimensions, so add 1 to size difference to get padding amount
c
       npad=(NwideP+1-nwide)/2  
       
       do j=0,nthick-1

          jshift=j*Nwide
          jshiftp=npad+(j+ (NthickP + 1 - Nthick)/2)*Nwidim

          do i=1,Nwide
             u(jshift+i)=upad(jshiftp+i)
          enddo         
       enddo

c
c =================================================
c

       return
       end

c SUBROUTINE: crop
c
c PURPOSE: Crop a image in the wide direction.    
c
c INPUT:   upad        real*4(array)    Padded image.   
c	   Nwidim      integer*4        Dimension of upad in wide direction
c          NwideP      integer*4        Padded size in wide direction.
c          Nwide       integer*4        Final size in wide direction.
c          Nthick      integer*4        Size in thick direction.
c           
c
c OUTPUT:  u           real*4(array)    Output image (Nwide*Nthick)  
c
c
c NOTE: 
c
c 010326
c 011107   DNM added dimension argument to deal with image transformed in place
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine crop(upad,u,Nwidim,Nwidep,Nwide,Nthick)
      
      implicit real*4(a-h,o-z)

      real *4 upad(*),u(*)
 

c
c =================================================
c

       npad=(nwidep-nwide)/2  
       
       do j=0,nthick-1

          jshift=j*Nwide
          jshiftp=npad+j*nwidim

          do i=1,Nwide
             u(jshift+i)=upad(jshiftp+i)
          enddo         
       enddo

c
c =================================================
c

       return
       end

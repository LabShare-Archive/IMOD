c SUBROUTINE: writeslice
c
c PURPOSE: Write an output slice to a file that has already been opened.    
c
c INPUT:  u          real*4(array)      Input slice to be written.
c         Nwide      integer*4          Size in wide direction.
c         Nthick     integer*4          Size in thick direction.
c      
c OUTPUT:    
c
c
c NOTE:  This way of writing the data is probably not very neat nor efficient.
c        It will produce some annoying gaps between consecutive slices.
c
c 010326
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine writeslice(u,Nwide,Nthick)
      
      implicit real*4(a-h,o-z)

      real *4 u(*)
 

c
c =================================================
c
       
       do j=0,Nthick-1
          jshift=j*Nwide
          write (3) (u(jshift+i),i=1,Nwide)
       enddo

c
c =================================================
c

       return
       end

c SUBROUTINE: freadbin
c
c PURPOSE:    Read binary data (without any spaces) 
c
c INPUT:      fname      character*12          Filename
c             N          integer*4            Number of elements in file. 
c
c OUTPUT:     u          real*4(array)        Array where the read array
c                                             is stored.
c
c
c NOTE: For some reason, this routine does not seem to read binary
c       data coming from Matlab. To get data from Matlab, save as ascii in
c       Matlab, then use form2bin.f to convert the ascii file to a binary
c       format that this routine can read.
c
c 010220
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine freadbin(fname,u,N)

       implicit real *4 (a-h,o-z)

       real *4 u(N)
      
       character *12 fname
   

c
c =====================================================
c     

       open (unit=15,file=fname, status='old',
     &       form='unformatted',iostat=ios)


       if (ios .ne. 0) then
          call prina('Did not open*',fname,8)
       else
          call prina('Opening*',fname,8)
          read(15) u
       endif
 
       close(15)

     
c
c =====================================================
c
  
      
       return
       end

c SUBROUTINE: angread
c
c PURPOSE:    Read angles from a formatted file. After reading them, 
c             the routine converts them into radians. 
c
c INPUT:      fname      character*12      Name of file to read from.
c             N          integer*4         Number of angles to read.
c             iform      integer*4         Format to use for the angles.
c                                          For available formats, see code for
c                                          sub routine.
c                                          
c OUTPUT:     angles     real*4            Array with the angles in radians.
c
c
c NOTE: The angles are assumed to be ordered in increasing order.
c       Example: -70,-69,...,-1,0,1,...,69,70.
c
c 010307
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       subroutine angread(fname,angles,N,iform)

       implicit real *4 (a-h,o-z)

       
       real *4 angles(N)
      
       character *12 fname
   
       data pi/3.1415926535897932385/
 
c
c ==============================================================
c


      




c ================ READ DATA =================================
c

       open (unit=3,file=fname,status='old',
     &       form='formatted',iostat=ios)

       if (ios .ne. 0) then
          call prina('  Did NOT open*',fname,8)
       else

          call prina('  Opened*',fname,8)
          do l=1,N

             if (iform .eq. 1000) then
                 read(3,1000) angles(l)
             endif

             if (iform .eq. 2000) then
                 read(3,2000) angles(l)
             endif

          enddo   


c _______________ Set format ____________________________
c

 1000     format(1x,f6.2)
 2000     format(f5.1)
    
c
c ________________________________________________________


       endif
       
       close(3)
    
c
c ============================================================





c ===================== CONVERT INTO RADIANS =================
c

       deg2rad=pi/180.0
       do l=1,N
          angles(l)=deg2rad*angles(l)
       enddo   

c
c ============================================================





c
c ================================================================
c



       return
       end

       
       

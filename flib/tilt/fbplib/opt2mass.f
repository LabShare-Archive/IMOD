c SUBROUTINE: opt2mass.f
c
c PURPOSE:    Convert optical density to mass density. 
c
c INPUT:      u        real*4(array)       Dataset to be converted.
c             N        integer*4           Size of dataset.
c             const    real*4              Constant (fog level?) to sub-
c                                          tract before taking the log.
c
c OUTPUT:     u        real*4(array)       Converted data set. 
c
c
c NOTE: 
c
c 010223
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       subroutine opt2mass(u,N,const)


       implicit real*4 (a-h,o-z)

      
       real *4 u(*)

c
c =============================================================
c

       do i=1,N
          u(i)=alog10(u(i)-const) 
       enddo
    
c
c =============================================================
c

       return
       end

c SUBROUTINE: nice_pad.f
c
c PURPOSE:    Finds the largest "nice" prime number factorization above a user
c             specified integer n. The code finds the smallest number  
c             n_nice >= n such that n_nice has a prime factorization of 2,3 
c             and 5. 
c
c INPUT:      n             integer*4       The input integer.
c
c OUTPUT:     n_nice        integer*4       The output integer.
c
c
c NOTE:       nmax in the code gives the maximum integer the routine handles.
c             If you need it to handle larger numers, just change the
c             setting for nmax below.
c
c             There have been no attempts to making this routine particularly
c             "neat", so the speed performance may not be optimal. However,
c             it should work fine for moderate numbers ~100-1000 if this one
c             is used in an initialization routine so that it is only used
c             in a precomputational stage.
c
c             The routine has been tested for some random integers, and seems
c             to work, but I don't guarantee it's without bugs...
c
c 010307
c	  $Author$
c
c	  $Date$

c	  $Revision$
c
c	  $Log$
c	  Revision 1.2  2001/11/28 18:27:14  mast
c	  After the fbp code was set to do the angular summation in place,
c	  problems occurred with odd values of nwidep.  Changed this to
c	  make the nice number be even by rounding n up to an even
c	  number and incrementing by 2 between trial values.
c	
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine nice_pad(n,n_nice)


       implicit real *8 (a-h,o-z)


 
      

c
c ==============================================================
c

       nmax=10000

       n_nice=n + mod(n,2)

 100   if (n_nice .le. nmax) then

          n_temp1=n_nice
 200      if (mod(n_temp1,2) .eq. 0.0) then
             n_temp2=n_temp1/2
             if (n_temp2 .gt. 2) then
                n_temp1=n_temp2
                goto 200
             else
                goto 600
             endif
          endif
 
 300      if (mod(n_temp1,3) .eq. 0.0) then
             n_temp2=n_temp1/3
             if (n_temp2 .gt. 3) then
                n_temp1=n_temp2
                goto 200
             else
                goto 600
             endif
          endif

 500      if (mod(n_temp1,5) .eq. 0.0) then
             n_temp2=n_temp1/5
             if (n_temp2 .gt. 5) then
                n_temp1=n_temp2
                goto 200
             else
                goto 600
             endif
          else          
             n_nice=n_nice+2
             goto 100
          endif

       else

          call prinf('Did not find a nice number below*',nmax,1)
          n_nice=n_nice-1
          return

       endif


c
c ==============================================================
c

 600   return
       end

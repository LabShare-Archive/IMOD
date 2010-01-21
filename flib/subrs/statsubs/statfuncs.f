c       Simple floating point wrappers for statistical functions in 
c       libcfshr, to replace old routines based on Press et al.

      real*4 function betai(a, b, x)
      implicit none
      real*4 a, b, x
      real*8 da, db, dx, incompBeta
      da = a
      db = b
      dx = x
      betai = incompBeta(da, db, dx)
      return
      end

      real*4 function tvalue(signif, ndf)
      implicit none
      real*4 signif
      integer*4 ndf
      real*8 dsig, dtvalue
      dsig = signif
      tvalue = dtvalue(dsig, ndf)
      return
      end

      real*4 function fvalue(signif, ndf1, ndf2)
      implicit none
      real*4 signif
      integer*4 ndf1, ndf2
      real*8 dsig, dfvalue
      dsig = signif
      fvalue = dfvalue(dsig, ndf1, ndf2)
      return
      end

      real*4 function erfcc(x)
      implicit none
      real*4 x
      real*8 dx, errFunc
      dx = x
      erfcc = 1. - errFunc(dx)
      return
      end

      
      

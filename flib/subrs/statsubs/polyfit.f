c       POLYFIT uses multiple linear regression to fit a polynomial of order
c       IORDER to NFIT points whose (x,y) coordinates are in the arrays X
c       and Y It returns the coefficient of x**i in the array SLOP and a
c       constant term in BINT.  Y = BINTCP + sum ( SLOPES(i) * X**i )

      integer*4 function  polyfit(x,y,nfit,iorder,slopes,bintcp)
      implicit none
      real*4 x(*),y(*),slopes(*),bintcp
      integer*4 nfit, iorder, polynomialFit
      real*4 work(2*(iorder + 1) * (iorder + 3 + nfit))
      polyfit = polynomialFit(x, y, nfit, iorder, slopes, bintcp, work)
      return
      end

      integer*4 function  localpolyfit(x,y,nfit,iorder,slopes,bintcp)
      implicit none
      real*4 x(*),y(*),slopes(*),bintcp
      integer*4 nfit, iorder, polyfit
      localpolyfit = polyfit(x,y,nfit,iorder,slopes,bintcp)
      return
      end


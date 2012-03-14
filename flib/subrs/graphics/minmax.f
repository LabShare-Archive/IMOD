c       This routine computes the min and max of a real array x of n elements
      subroutine minmax(x,n,xmin,xmax)
      dimension x(*)
      xmin=x(1)
      xmax=xmin
      do 10 i=2,n
        xx=x(i)
        if(xx.gt.xmax)xmax=xx
        if(xx.lt.xmin)xmin=xx
10    continue
      return
      end

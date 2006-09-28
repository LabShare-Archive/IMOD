c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
c       !
c       Calculates the mean [avg], standard deviation [sd], and standard
c       error of mean [sem], from the [n] values in array [x].
c       
      subroutine avgsd(x,n,avg,sd,sem)
      dimension x(*)
      sx=0.
      sxsq=0.
      do i=1,n
        sx=sx+x(i)
      enddo
      avg=sx/n
      sx=0.
      do i=1,n
        d=x(i)-avg
        sx=sx+d
        sxsq=sxsq+d**2
      enddo
      call sums_to_avgsd(sx,sxsq,n,avnew,sd)
      avg=avg+avnew
      sem=0.
      if(n.gt.0)sem=sd/sqrt(float(n))
      return
      end


c       !
c       Computes a mean [avg] and standard deviation [sd] from the
c       sum of values [sx], sum of squares [sxsq], and number of values [n].
c       It will not generate any division by 0 errors.
c       
      subroutine sums_to_avgsd(sx,sxsq,n,avg,sd)
      avg=0.
      sd=0.
      if(n.le.0)return
      avg=sx/n
      if(n.gt.1)sd=sqrt(max(0.,(sxsq-n*avg**2)/(n-1.)))
      return
      end

c       !
c       Computes a mean [avg] and standard deviation [sd] from the sum of
c       values [sx8], sum of squares [sxsq8], and number of values [n1] * [n2],
c       where [sx8] and [sxsq8] are real*8 and the number of values can be
c       greater than 2**31.
c       It will not generate any division by 0 errors.
c       
      subroutine sums_to_avgsd8(sx8,sxsq8,n1, n2,avg,sd)
      implicit none
      real*4 avg,sd
      integer*4 n1,n2
      real*8 sx8,sxsq8,avg8,dn
      avg=0.
      sd=0.
      dn = n1
      dn = dn * n2
      if(dn.le.0)return
      avg8=sx8/dn
      avg = avg8
      if(dn.gt.1.)sd=sqrt(max(0.,(sxsq8-dn*avg8**2)/(dn-1.)))
      return
      end

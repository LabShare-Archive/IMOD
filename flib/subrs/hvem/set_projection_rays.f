c       $Id$
c       
c       $Log$
c       Revision 3.2  2006/06/20 04:48:18  mast
c       Changed origin definition to make projections match input data to Tilt
c       Changed arguments so sinang and cosang could be modified
c       
c       Revision 3.1  2006/05/17 00:05:18  mast
c       Added to library after extracting from xyzproj
c       
c       !
c       Sets up information about projection rays at an angle whose sine and
c       cosine are [sinang] and [cosang], through
c       an image slice whose size is [nxslice] by [nyslice].  The number of 
c       rays, i.e. the size of centered output in X, is given by [nxout].
c       The arrays [xraystr] and [yraystr] are returned with the coordinates
c       at which to start each ray, the array [nrayinc] is returned with the
c       number of points in each ray, and [nraymax] is returned with the 
c       maximum number of points.  [sinang] and [cosang] may be modified for
c       projections very near vertical or horizontal.
c       !
      subroutine set_projection_rays(sinang, cosang, nxslice, nyslice,
     &    nxout, xraystr, yraystr, nrayinc, nraymax)
      implicit none
      real*4 xraystr(*), yraystr(*)
      integer*4 nxslice, nyslice, nxout, nrayinc(*), nraymax
      real*4 sinang, cosang, xraytmp, yraytmp,xgood(4),ygood(4)
      integer*4 nraytmp,ngoodinter,indgood,ixout
      real*4 tanang,rayintcp,xleft,xright,ybot,ytop,yleft,yright,xtop,xbot
      logical b3dxor
c       
      nraymax = 0
      do ixout=1,nxout
        nraytmp=0
c         
c         if a near-vertical projection, set up to be exactly vertical
c         (limit was 0.01, try it at 0.001)
        if(abs(sinang).lt.0.001)then
          sinang=0.
          cosang=sign(1.,cosang)
          if(cosang.gt.0.)then
            yraytmp=2.
            xraytmp=nxslice/2 + ixout - nxout/2
          else
            yraytmp=nyslice-1.
            xraytmp=nxslice/2 + 1 + nxout/2 - ixout 
          endif         
          if(xraytmp.ge.1 .and. xraytmp.le.nxslice)
     &        nraytmp=nyslice-2
c           
c           if a near-horizontal projection, set up to be exactly horizontal
c           
        elseif(abs(cosang).lt.0.001)then
          sinang=sign(1.,sinang)
          cosang=0.
          if(sinang.gt.0.)then
            xraytmp=2.
            yraytmp=nyslice/2 + 1 + nxout/2 - ixout
          else
            xraytmp=nxslice-1.
            yraytmp=nyslice/2 + ixout - nxout/2
          endif
          if(yraytmp.ge.1 .and. yraytmp.le.nyslice)
     &        nraytmp=nxslice-2
c           
c           otherwise need to look at intersections with slice box
c           
        else
          ngoodinter=0
          tanang=sinang/cosang
          rayintcp=-(ixout-0.5-nxout/2)/sinang
c           
c           coordinates of edges of slice box
c           
          xleft=1.01-(nxslice/2)
          xright=nxslice - 1.01 - (nxslice/2)
          ybot=1.01-(nyslice/2)
          ytop=nyslice - 1.01 - (nyslice/2)
c           
c           corresponding intersections of the ray with extended edges
c           
          yleft=xleft/tanang + rayintcp
          yright=xright/tanang + rayintcp
          xbot=(ybot-rayintcp)*tanang
          xtop=(ytop-rayintcp)*tanang
c           
c           make list of intersections that are actually within slice box
c           
          if(yleft.ge.ybot.and.yleft.le.ytop)then
            ngoodinter=ngoodinter+1
            xgood(ngoodinter)=xleft
            ygood(ngoodinter)=yleft
          endif
          if(yright.ge.ybot.and.yright.le.ytop)then
            ngoodinter=ngoodinter+1
            xgood(ngoodinter)=xright
            ygood(ngoodinter)=yright
          endif
          if(xbot.ge.xleft.and.xbot.le.xright)then
            ngoodinter=ngoodinter+1
            xgood(ngoodinter)=xbot
            ygood(ngoodinter)=ybot
          endif
          if(xtop.ge.xleft.and.xtop.le.xright)then
            ngoodinter=ngoodinter+1
            xgood(ngoodinter)=xtop
            ygood(ngoodinter)=ytop
          endif
c           
c           if there are real intersections, use them to set up ray start
c           
          if(ngoodinter.gt.0)then
            indgood=1
            if(b3dxor(ygood(2).lt.ygood(1), cosang.lt.0))indgood=2
            xraytmp=xgood(indgood)+nxslice/2+0.5
            yraytmp=ygood(indgood)+nyslice/2+0.5
            nraytmp=1+
     &          sqrt((xgood(1)-xgood(2))**2+(ygood(1)-ygood(2))**2)
            if(nraytmp.lt.3)nraytmp=0
          endif
        endif
c         
c         store final ray information
c         
        xraystr(ixout)=xraytmp
        yraystr(ixout)=yraytmp
        nrayinc(ixout)=nraytmp
        nraymax = max(nraymax, nraytmp)
      enddo
      return
      end

c       $Id$
c
c       $Log$
c       Revision 1.3  2005/12/09 04:39:44  mast
c       gfortran: .xor., continuation, byte, or open fixes
c	
c       Revision 1.2  2004/06/04 16:55:44  mast
c       Eliminated degree-type trig functions
c
c	
c       LINETRACK tracks a line between two selected points of a contour or
c       between the last point and the first point.
c       ARRAY is the image data, in bytes, size NX by NY
c       P_COORD(3,*) has X, Y, Z coordinates of each point in contour
c       NINOBJ is the number of points in the contour
c       IPTCUR is the index of the first point of the pair (numbered from 1)
c       MAXLEN is the maximum size the contour can become
c       inKSIZE is the size of the kernels
c       iNKERN is the number of kernels
c       SIGMA is the sigma value for the cross-dimension of the kernels
c       H is the H value for the parabolic long dimension
c       IFDARK is nonzero for tracking dark lines
c       STEP is the size of the stpe to make along the line
c       REDTOL is the tolerance for point reduction
c       IFREPLACE nonzero allows replacement of the endpoints
c       OFFSET is an amount to offset the points from center of line
c       IFCLOSE is nonzero for tracking from last to first point
c       IFFAIL is returned with nonzero for failure
c
      subroutine linetrack(array,nx,ny,p_coord,ninobj,iptcur,maxlen,
     &    inksize,inkern,sigma,h,ifdark,step,redtol,ifreplace,offset,
     &    ifclose,iffail)
      parameter (limdx=6,limkern=40,limksize=21,limpath=1000)
      parameter (maxtry=2)
      integer*1 array(nx,ny)
      real*4 p_coord(3,maxlen)
c       
      real*4 orig(3),bkern(limkern*limksize**2)
      real*4 xpath(limpath,2),ypath(limpath,2)
      real*4 xdum(limpath),ydum(limpath)
      integer*4 idum(limpath),jdum(limpath)
      data orig/0.,0.,0./
C       
      logical needprod(-limdx:limdx,limkern)
      real*4 aprod(-limdx:limdx,limkern),theta(limkern)
      real*4 xstnew(2),ystnew(2),dely(2),tbestnew(2)
      integer*4 ixofs(limkern),iyofs(limkern),kbase(limkern)
      integer*4 ninpath(2)
      real*4 tolinitscantry(maxtry)/55.,20./
      real*4 endtoltry(maxtry)/1.,1.5/
      real*4 tolpathangtry(maxtry)/15.,10./
      integer*4 ndxlimtry(maxtry)/2.,1./
      real*4 dtor/0.01745329252/
      data ksizsav/0/
      save bkern
      save ksizsav,sigsav,hsav,nksav,ifdsav,kbase,theta,ixofs,iyofs
      common /linmen/izmean,dmean
      data izmean/-10000/
C       
      nkern=min(inkern,limkern)
      ksize=min(2*inksize+1,limksize)
C       
      tolmeet=3.*step
      tolangoff=75.
      tolcross=2.*step
      thetinc=180./nkern
c       
c       write(*,'(3f10.3)')((p_coord(i,j),i=1,3),j=1,ninobj)
c       
c       First time, or if kernel parameters have changed, make kernels at
c       the range of orientations
      if(ksizsav.ne.ksize.or.sigsav.ne.sigma.or.hsav.ne.h.or.
     &    nksav.ne.nkern.or.ifdsav.ne.ifdark)then
        call makekern(bkern,ksize,ksize,sigma,h,nkern,ifdark,theta,
     &      ixofs,iyofs)
        do k=1,nkern
          kbase(k)=1+ksize**2*(k-1)
        enddo
        ksizsav=ksize
        sigsav=sigma
        hsav=h
        nksav=nkern
        ifdsav=ifdark
c         print *,'made kernels'
      endif
c       print *,nx,ny,ninobj,iptcur,maxlen,ksize,nkern,sigma,h,ifdark,step
c       
c       
c       if close function selected, replicate first point as last point
c       
      if(ifclose.ne.0)then
        distclose=sqrt((p_coord(1,1)-p_coord(1,ninobj))**2+
     &      (p_coord(1,1)-p_coord(1,ninobj))**2)
        iffail=0
        if(distclose.lt.1.)return
        f=max(0.5,(distclose-1.)/distclose)
        iptcur=ninobj
        ninobj=ninobj+1
        p_coord(1,ninobj)=f*p_coord(1,1)+(1.-f)*p_coord(1,iptcur)
        p_coord(2,ninobj)=f*p_coord(2,1)+(1.-f)*p_coord(2,iptcur)
        p_coord(3,ninobj)=p_coord(3,iptcur)
      endif
c       
c       Set up starting and ending points
      ipst=iptcur
      ipnd=iptcur+1
      if(ipst.le.0.or.ipnd.gt.ninobj)return
c       
c       Get the Z value from contour, and new mean of edge if needed
      zmodel=p_coord(3,ipnd)
      izmodel=nint(zmodel)
      if(izmean.ne.izmodel)then
        call edgemean(array,nx,ny,dmean)
        izmean=izmodel
c         print *,'Mean =',dmean,izmean,izmodel
      endif
c       
c       Start loop to try multiple times (twice) with different parameters
      ifmeet=0
      itry=1
      do while(ifmeet.eq.0.and.itry.le.maxtry)
        endtol=endtoltry(itry)
        tolinitscan=tolinitscantry(itry)
        tolpathang=tolpathangtry(itry)
c         
c         Get start and end points, modify for offset to get to center of line
        xst=p_coord(1,ipst)+orig(1)+0.5
        yst=p_coord(2,ipst)+orig(2)+0.5
        xnd=p_coord(1,ipnd)+orig(1)+0.5
        ynd=p_coord(2,ipnd)+orig(2)+0.5
        if(offset.ne.0.)then
          tstnd=atan2(ynd-yst,xnd-xst) / dtor
          sints=sin(tstnd * dtor)
          costs=cos(tstnd * dtor)
          xst=xst+sints*offset
          yst=yst-costs*offset
          xnd=xnd+sints*offset
          ynd=ynd-costs*offset
        endif
c         call typearray(array,nx,ny,nint(xst),nint(yst))
c         print *,'segment',xst,yst,xnd,ynd
        iffail=0
        if(sqrt((xst-xnd)**2+(yst-ynd)**2).le.step)return
        iffail=1
c         
c         Loop on looking at starting point and ending point
        do ifback=1,2
          ixc=nint(xst)
          iyc=nint(yst)
          tstnd=atan2(ynd-yst,xnd-xst) / dtor
c           
c           scan for best overall direction from this point, limit range of
c           search from direction between start and end point by tolinitscan
c           
          ndxlim=1
          if(ifreplace.lt.0)ndxlim=3
          best=-1.e10
          do k=1,nkern
            diffang=min(abs(goodangle(theta(k)-tstnd)),
     &          abs(goodangle(theta(k)-180.-tstnd)))
            if(diffang.lt.tolinitscan)then
              call kernprod(array,nx,ny,ixc,iyc,bkern(kbase(k)),
     &            ksize, ksize,ixofs(k), iyofs(k),dmean,prod)
              if(prod.gt.best)then
                best=prod
                tbest=theta(k)
              endif
            endif
          enddo
          tdiff=abs(goodangle(tstnd-tbest))
c           
c           if the difference in angle is greater than 90, the
c           direction needs to be flipped
          if(tdiff.gt.90.)tbest=tbest-180.
c           
c           Given this direction, refine the position and direction
          call findangpos(array,nx,ny,bkern,kbase,ksize,nkern,ixofs,
     &        iyofs,dmean,thetinc,tolpathang,ndxlim,xnd,ynd,ixc,iyc,
     &        tbest,cxnew,cynew)
c           
c           Save best starting point and angle for this direction and get the
c           amount that it slid sideways
          xstnew(ifback)=cxnew
          ystnew(ifback)=cynew
          tbestnew(ifback)=tbest
          dely(ifback)=-sin(tbest * dtor)*(xst-cxnew)+
     &        cos(tbest * dtor)*(yst-cynew)
c           
c           Swap start and end for next time through loop
          xtmp=xst
          xst=xnd
          xnd=xtmp
          ytmp=yst
          yst=ynd
          ynd=ytmp
        enddo
        delyavg=0.5*(dely(1)-dely(2))
c         print *,delyavg,dely(1),dely(2)
        dely(1)=delyavg
        dely(2)=-delyavg
c         print *,delyavg,dely(1),dely(2)
c         
c         Start loop to try each direction (start->end, end->start)
        ifback=1
        ifterm=0
        ndxlim=ndxlimtry(itry)
        endtolsq=endtol**2
        do while (ifback.le.2.and.ifterm.le.0)
c
c           Start loop to step from one position to the next
          ninpath(ifback)=0
          ifterm=0
          iffirst=1
          do while(ifterm.eq.0)
            if(iffirst.eq.0)then
c
c               come into here with tbest, ixc,iyc from last step and do
c               search along perpendicular and at neighboring angles
              call findangpos(array,nx,ny,bkern,kbase,ksize,nkern,ixofs,
     &            iyofs,dmean,thetinc,tolpathang,ndxlim,xnd,ynd,ixc,iyc,
     &            tbest,cxnew,cynew)
            else
c               
c               Or, first time, initialize with the saved starting positions
c               after such a search
              cxnew=xstnew(ifback)
              cynew=ystnew(ifback)
              tbest=tbestnew(ifback)
              xnd=xstnew(3-ifback)
              ynd=ystnew(3-ifback)
            endif
c             
c             Compute model point from index position, adjust if replace <0 ?
c             and add the offset back
            xmodel=cxnew -orig(1)-0.5
            ymodel=cynew -orig(2)-0.5
            if(ifreplace.lt.0)then
              xmodel=xmodel-sin(tbest * dtor)*dely(ifback)
              ymodel=ymodel+cos(tbest * dtor)*dely(ifback)
            endif
            if(offset.ne.0)then
              xmodel=xmodel-sin(tbest * dtor)*offset*(3-2*ifback)
              ymodel=ymodel+cos(tbest * dtor)*offset*(3-2*ifback)
            endif
c             
c             After first step, test if it is at end.  If so decide whether to 
c             save or not depending on distance from previous point 
            ifsave=1
            if(iffirst.eq.0)then
              call point_to_line(xnd,ynd,cxlast,cylast,cxnew,
     &            cynew, tmin,distsq)
              if(distsq.lt.endtolsq)then
c                 ifterm=1 is unconditional.
c                 This commented stuff is to try making it go past
c                 before terminating, then replacing endpoint with
c                 closest point on the line
c                 if(tmin.lt.1.)then
                ifterm=1
c                 xstnew(3-ifback)=cxlast+tmin*(cxnew-cxlast)
c                 ystnew(3-ifback)=cylast+tmin*(cynew-cylast)
c                 endif
                ifsave=0
                if(tmin.eq.1.)then
                  distlast=sqrt((xnd-cxlast)**2+(ynd-cylast)**2)
                  if(distlast.gt.1.33*step)ifsave=1
                endif
              endif
            endif
c             
c             set new last position and save model point into path
            iffirst=0
            cxlast=cxnew
            cylast=cynew
            if(ifsave.ne.0)then
              ninpath(ifback)=ninpath(ifback)+1
              xpath(ninpath(ifback),ifback)=xmodel
              ypath(ninpath(ifback),ifback)=ymodel
            endif
            if(ifterm.eq.0)then
c               
c               Stop if the angle has gotten too far off
              tcurnd=atan2(ynd-cynew,xnd-cxnew) / dtor
              if(abs(goodangle(tcurnd-tbest)).gt.tolangoff)then
                ifterm=-1
              else
c                 
c                 or set up new position
c                 stop and try the other direction if too close to edge
                ixc=nint(cxnew+step*cos(tbest * dtor))
                iyc=nint(cynew+step*sin(tbest * dtor))
                if(ixc.lt.ksize/2.or.ixc.gt.nx-ksize/2.or.
     &              iyc.lt.ksize/2.or.iyc.gt.ny-ksize/2.or.
     &              ninpath(ifback).ge.limpath-1) ifterm=-1
              endif
              if(ifterm.eq.-1) ifback=ifback+1
            endif
          enddo
        enddo
c         
c         if went through both paths without success, need to
c         try for a composite path
c         
c         print *,'end of paths',ifback,ninpath(1),ifterm
        if(ifback.eq.3)then
          ipseg=1
          ifmeet=0
c           
c           Loop on segments in path 1
          do while (ipseg.lt.ninpath(1).and.ifmeet.eq.0)
            x2=xpath(ipseg,1)
            y2=ypath(ipseg,1)
            x1=xpath(ipseg+1,1)
            y1=ypath(ipseg+1,1)
            dxseg=x2-x1
            dyseg=x2-x1
            segsq=(x2-x1)**2+(y2-y1)**2
            ip2=0
c             
c             Loop on points in path 2, testing distance of each to the segment
c             in path 1 - if it is close enough then they have met
            do while(ip2.lt.ninpath(2).and.ifmeet.eq.0)
              ip2=ip2+1
              x0=xpath(ip2,2)
              y0=ypath(ip2,2)
              dxp=x1-x0
              dyp=y1-y0
              if(abs(dxp).lt.tolmeet.and.abs(dyp).lt.tolmeet)
     &            then
                tmin=-(dxp*dxseg+dyp*dyseg)/segsq
                tmin=max(0.,min(1.,tmin))
                distsq=(dxp+tmin*dxseg)**2+(dyp+tmin*dyseg)**2
                if(distsq.lt.endtolsq)then
                  ifmeet=1
                  ip1=ipseg
                  if(tmin.eq.0.)then
                    distlast=sqrt((x2-x0)**2+(y2-y0)**2)
                    if(distlast.gt.1.33*step)ip1=ipseg+1
                  endif
                endif
              endif
            enddo
            ipseg=ipseg+1
          enddo
c           
        elseif(ifback.eq.1)then
c           
c           Otherwise if one of the paths worked, set ip1 and ip2 to match
c           their meaning if two paths met; adjust starting point (?)
          ip1=ninpath(1)+1
          xpath(ip1,1)=xstnew(2)+sin(tbestnew(2) * dtor)*offset
          ypath(ip1,1)=ystnew(2)-cos(tbestnew(2) * dtor)*offset
          ip2=0
        else
          ip1=1
          xpath(1,1)=xstnew(1)-sin(tbestnew(1) * dtor)*offset
          ypath(1,1)=ystnew(1)+cos(tbestnew(1) * dtor)*offset
          ip2=ninpath(2)
        endif
c         
        if(ifback.lt.3.or.ifmeet.eq.1)then
          ifmeet=1
          if(ip2+ip1.gt.limpath)return
c           
c           Add the two paths together from the meeting point
          do jp=ip2,1,-1
            ip1=ip1+1
            xpath(ip1,1)=xpath(jp,2)
            ypath(ip1,1)=ypath(jp,2)
          enddo
c           
c           Retain existing coordinates at end if not replacing
          if(ifreplace.le.0)then
            xpath(ip1,1)=p_coord(1,ipnd)
            ypath(ip1,1)=p_coord(2,ipnd)
            xpath(1,1)=p_coord(1,ipst)
            ypath(1,1)=p_coord(2,ipst)
          endif
c           
c           now could reduce the path array
c           
          call reducepts(xpath,ypath,ip1,2,5,redtol,xpath(1,2),
     &        ypath(1,2),xdum,ydum,idum,jdum)
          if(maxlen.lt.ninobj+ip1-2)return
c           
c           shift points in contour from current one to end to allow insertion
c           
          ishft=ip1-2
c           print * ,ishft,' points being added'
          do ip=ninobj,ipnd,-1
            ipnew=ip+ishft
            p_coord(1,ipnew)=p_coord(1,ip)
            p_coord(2,ipnew)=p_coord(2,ip)
            p_coord(3,ipnew)=p_coord(3,ip)
          enddo
c           
c           save points in open spot in contour, replacing ends potentially
c           
          do ip=1,ip1
            ipnew=ip+ipst-1
            p_coord(1,ipnew)=xpath(ip,1)
            p_coord(2,ipnew)=ypath(ip,1)
            p_coord(3,ipnew)=zmodel
          enddo
          ninobj=ninobj+ishft
          iptcur=iptcur+ishft
        endif
        itry=itry+1
      enddo
      iffail=1-ifmeet
c       
c       If this closed the contour, do the uncross operation to separate 
c       endpoints
      if(iffail.eq.0.and.ifclose.ne.0)
     &    call uncrosscont(p_coord,ninobj,ninobj,tolcross)
      return
      end

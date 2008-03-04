
c       CONVEXBOUND finds the smallest convex boundary around an arbitrary
c       set of points SX, SY.  NPNTS is the total number of points in SX, SY.
c       If FRACOMIT is nonzero, it omits that fraction  of points from
c       consideration completely, choosing the points that are farthest from
c       the centroid of all the points.  (If FRACOMIT is negative, the Y
c       coordinates of the points will be temporarily scaled to have the 
c       same standard deviation as the X range in order to determine the
c       farthest points; it then eliminates -FRACOMIT points.) It returns
c       the centroid of the points under consideration as XCEN, YCEN.  It
c       returns the set of NVERT boundary points in BX, BY.  If PAD is
c       nonzero, it makes the boundary points be that distance away from the
c       outermost SX, SY points.  It uses Graham's scan algorithm.
c       
c       $Id$
c       
c       $Log$
c       Revision 1.1  2005/03/22 23:15:10  mast
c       Moved to libhvem
c       
c       Revision 3.1  2002/06/05 21:11:47  mast
c       Added check for overrunning internal arrays and output arrays, and
c       increased size of internal arrays
c       

      subroutine convexbound(sx,syin,npnts,fracomit,pad,bx,by,nvert,
     &    xcen,ycen,maxverts)
      real*4 sx(*),syin(*),bx(*),by(*)
      parameter (limpts=5000)
      real*4 cendist(limpts),sy(limpts)
      integer*2 inddist(limpts),iangtopt(limpts)
      logical*1 vertex(limpts)
      integer*4 ipttoang(limpts)
      nextpt(ind)=iangtopt(mod(ipttoang(ind),nptuse)+1)
c       
c       scale y to have same sd as x if fracomit is negative
c       
      sdx=1.
      sdy=1.
      if(fracomit.lt.0.)then
        call avgsd(sx,npnts,avgx,sdx,semx)
        call avgsd(syin,npnts,avgy,sdy,semy)
      endif
      if (npnts.gt.limpts) then
        print *,
     &      'There are too many points for the arrays in CONVEXBOUND'
        return
      endif
c       
c       start with pointers in numerical order
c       
      do i=1,npnts
        inddist(i)=i
        sy(i)=sdx*syin(i)/sdy
      enddo
c       
c       find distances from center and order by distance
c       
      call cenorder(sx,sy,npnts,inddist,cendist,xcen,ycen)
      nptuse=npnts
c       
c       if omitting farthest points, find distances again
c       
      if(fracomit.ne.0)then
        nptuse=max(npnts-nint(abs(fracomit)*npnts),min(3,npnts))
        if(fracomit.lt.0)then
          do i=1,npnts
            sy(i)=syin(i)
          enddo
        endif
        call cenorder(sx,sy,nptuse,inddist,cendist,xcen,ycen)
      endif
c       
c       get pointers to points in order by angle and inverse pointers
c       
      call angorder(sx,sy,nptuse,inddist,xcen,ycen,iangtopt,ipttoang)
c       
c       find the rightmost lowest point; it must be a vertex
c       
      ymin=1.e30
      do i=1,nptuse
        ind=inddist(i)
        if(sy(ind).lt.ymin.or.(sy(ind).eq.ymin.and.sx(ind).gt.xmax))
     &      then
          indstart=ind
          ymin=sy(ind)
          xmax=sx(ind)
        endif
        vertex(ind)=.true.
      enddo
c       
c       start the scan at INDSTART and next 2 points
c       
      ind1=indstart
      ind2=nextpt(ind1)
      ind3=nextpt(ind2)
      nvert=nptuse
      do while (ind2.ne.indstart.and.nvert.gt.2)
c         
c         test for left or right turn
c         
        if(sx(ind1)*(sy(ind2)-sy(ind3))-sx(ind2)*(sy(ind1)-sy(ind3))
     &      +sx(ind3)*(sy(ind1)-sy(ind2)).gt.0.)then
c           
c           left turn; advance the scan
c           
          ind1=ind2
          ind2=ind3
          ind3=nextpt(ind3)
        else
c           
c           right turn; mark point 2 as non-vertex
c           
          vertex(ind2)=.false.
          nvert=nvert-1
          if(ind1.eq.indstart)then
c             
c             if still at start, advance points 2 and 3
c             
            ind2=ind3
            ind3=nextpt(ind3)
          else
c             
c             otherwise drop points 1 and 2 back; amke point 1 be the last
c             point that is still eligible as a vertex
c             
            ind2=ind1
20          itry=ipttoang(ind1)-1
            if(itry.lt.1)itry=nptuse
            ind1=iangtopt(itry)
            if(.not.vertex(ind1))go to 20
          endif
        endif
      enddo
c       
c       put vertices into bx,by in order by angle relative to
c       centroid, with pad if called for
c       
      nvert=0
      do i=1,nptuse
        ind=iangtopt(i)
        if(vertex(ind))then
          if (nvert.ge.maxverts) then
            print *,
     &          'The contour has too many vertices for the arrays'
            return
          endif
          nvert=nvert+1
          padfrac=0.
          if(pad.gt.0..and.cendist(ind).gt.1.e-10)
     &        padfrac=pad/sqrt(cendist(ind))
          bx(nvert)=sx(ind)+padfrac*(sx(ind)-xcen)
          by(nvert)=sy(ind)+padfrac*(sy(ind)-ycen)
        endif
      enddo
      return
      end


c       
c       CENORDER finds the centroid XCEN, YCEN of the points in the SX,XY
c       array pointed to by the first NPNTS entries in the pointer INDDIST
c       It also returns distances from the centroid in CENDIST and orders
c       the array INDDIST to point to the points in order by increasing
c       CENDIST

      subroutine cenorder(sx,sy,npnts,inddist,cendist,xcen,ycen)
      real*4 sx(*),sy(*),cendist(*)
      integer*2 inddist(*)
c       
c       find centroid of the points
c       
      xsum=0.
      ysum=0.
      do i=1,npnts
        xsum=xsum+sx(inddist(i))
        ysum=ysum+sy(inddist(i))
      enddo
      xcen=xsum/npnts
      ycen=ysum/npnts
c       
c       get distances from center
c       
      do i=1,npnts
        ind=inddist(i)
        cendist(ind)=(sx(ind)-xcen)**2+(sy(ind)-ycen)**2
      enddo
c       
c       order pointers by distance
c       
      do i=1,npnts-1
        do j=i+1,npnts
          if(cendist(inddist(i)).gt.cendist(inddist(j)))then
            indtmp=inddist(i)
            inddist(i)=inddist(j)
            inddist(j)=indtmp
          endif
        enddo
      enddo
      return
      end


c       
c       ANGORDER computes the angles of NPTUSE points in the arrays SX, SY
c       (pointed to by INDDIST), relative to a center point XCEN, YCEN,
c       and then orders the points by increasing angle.  IANGTOPT points to
c       the points in order by increasing angle, while IPTTOANG is the
c       inverse pointer, used to get back from point number to angle number.
c       
      subroutine angorder(sx,sy,nptuse,inddist,xcen,ycen,iangtopt,
     &    ipttoang)
      parameter (limpts=5000)
      real*4 sx(*),sy(*),angle(limpts)
      integer*2 inddist(*),iangtopt(*)
      integer*4 ipttoang(*)
      do i=1,nptuse
        ind=inddist(i)
        angle(ind)=atan2(sy(ind)-ycen,sx(ind)-xcen)
        iangtopt(i)=ind
      enddo
      do i=1,nptuse-1
        ind=iangtopt(i)
        do j=i+1,nptuse
          jnd=iangtopt(j)
          if(angle(ind).gt.angle(jnd))then
            iangtopt(j)=ind
            iangtopt(i)=jnd
            ind=jnd
          endif
        enddo
      enddo
      do i=1,nptuse
        ipttoang(iangtopt(i))=i
      enddo
      return
      end

c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 1.3  2005/12/09 04:39:30  mast
c       Changed r**i to repeated multiplication for gfortran
c       
c       Revision 1.2  2004/06/04 16:55:57  mast
c       Eliminated degree-type trig functions
c       
c
c       FINDANGPOS searches from a given starting position and angle for the
c       position and angle that give the strongest kernel product
c       
c       ARRAY is the image, in bytes, dimensioned NX by NY
c       BKERN is an array with all NKERN kernels
c       KBASE is an array of starting indexes of the kernels in BKERN
c       KSIZE is the size in X and Y of the individual kernels
c       IXOFS, IYOFS are arrays with offsets from the array start to the start
c       of the particular kernel in X and Y
c       DMEAN is the edge mean of the image, used for fill when computing
c       products at edges of the array
c       THETAINC is the angular increment between kernels
c       TOLPATHANG is the maximum change in angle to search
c       NDXLIM is the maximum distance (in integer steps) to shift along
c       normal to the current direction
c       XND, YND are possibly target endpoints in the direction of movement
c       IXC, IYC is the center position to start the search at
c       TBEST is the starting best angular direction, returned with the new
c       best direction
c       CXNEW, CYNEW are returned with the new best center position
c
      subroutine findangpos(array,nx,ny,bkern,kbase,ksize,nkern,ixofs,
     &    iyofs,dmean,thetinc,tolpathang,ndxlim,xnd,ynd,ixc,iyc,tbest,
     &    cxnew,cynew)
      integer*1 array(nx,ny)
      real*4 bkern(*)
      integer*4 kbase(*),ixofs(*),iyofs(*)
      parameter (limdx=10,limkern=40)
      logical needprod(-limdx:limdx,limkern)
      real*4 aprod(-limdx:limdx,limkern)
      real*4 dtor/0.01745329252/
c       
c       get estimate of current angle from starting best angle or direction
c       from ixc,iyc to xnd,ynd; find center angle and angular range
      tcurnd=tbest
      if(ynd.ne.iyc.or.xnd.ne.ixc)tcurnd=atan2(ynd-iyc,xnd-ixc) / dtor
      halfdiff=0.5*goodangle(tbest-tcurnd)
      tcen=goodangle(tcurnd+halfdiff)
      wideangle=abs(halfdiff)+tolpathang
c       
c       Get integer dx and dy along normal to direction
      normdx=nint(cos((tbest+90.) * dtor))
      normdy=nint(sin((tbest+90.) * dtor))
c       
c       Initialize DX, angle, and kernel index at max, and evaluate product
      ndxmax=0
      ndx=0
      ktmp=nint(tbest/thetinc)
      tmax=goodangle(ktmp*thetinc)
      kmax=indmap(ktmp+1,nkern)
      call kernprod(array,nx,ny,ixc,iyc,bkern(kbase(kmax))
     &    ,ksize,ksize,ixofs(kmax),iyofs(kmax),dmean,pmax)
c       
c       Clear array to keep track of evaluated positions/angles
      do i=-ndxlim,ndxlim
        do k=1,nkern
          needprod(i,k)=.true.
        enddo
      enddo
      aprod(0,kmax)=pmax
      needprod(0,kmax)=.false.
      moved =1
c       
c       Loop until no more movements occur
      do while(moved.eq.1)
        ndxcur=ndxmax
        tcur=tmax
        kcur=kmax
        moved=0
c         
c         From the current position, check one angle on either side and one
c         position on either side along the integer-normal
        do kinc=-1,1
          th=goodangle(tcur+kinc*thetinc)
          if(abs(goodangle(th-tcen)).lt.wideangle)then
            k=indmap(kcur+kinc,nkern)
            do norminc=-1,1
              ndx=ndxcur+norminc
              if(abs(ndx).le.ndxlim)then
                ixtry=ixc+ndx*normdx
                iytry=iyc+ndx*normdy
c                 
c                 A valid position/angle: evaluate product if needed
                if(needprod(ndx,k))then
                  call kernprod(array,nx,ny,ixtry,iytry,
     &                bkern(kbase(k)),ksize, ksize,
     &                ixofs(k),iyofs(k),dmean,
     &                aprod(ndx,k))
                  needprod(ndx,k)=.false.
                endif
c                 
c                 If product better, save parameters of the new max and move
                if(aprod(ndx,k).gt.pmax)then
                  moved=1
                  tmax=th
                  kmax=k
                  pmax=aprod(ndx,k)
                  ndxmax=ndx
                endif
              endif
            enddo
          endif
        enddo
      enddo
c       
c       come out with a best integral position and angle 
c       interpolate angle if both sides exist
      cy=0.
      y2=pmax
      if(.not.(needprod(ndxmax,indmap(kmax-1,nkern)).or.
     &    needprod(ndxmax,indmap(kmax-1,nkern))))then
        y1=aprod(ndxmax,indmap(kmax-1,nkern))
        y3=aprod(ndxmax,indmap(kmax+1,nkern))
        denom=2.*(y1+y3-2.*y2)
        if(abs(denom).gt.-1.e6)cy=(y1-y3)/denom
        if(abs(cy).gt.0.5)cy=sign(0.5,cy)
      endif
      tbest=tmax+cy*thetinc
c       
c       interpolate offset if not at edge of allowable shifts
c       
      cx=0.
      if(abs(ndxmax).lt.ndxlim)then
        y1=aprod(ndxmax-1,kmax)
        y3=aprod(ndxmax+1,kmax)
        denom=2.*(y1+y3-2.*y2)
        if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
        if(abs(cx).gt.0.5)cx=sign(0.5,cx)
      endif
      cxnew=ixc+(ndxmax+cx)*normdx
      cynew=iyc+(ndxmax+cx)*normdy
      return
      end


c       INDMAP maps an index IX to a value between 1 and NX by adding or
c       subtracting NX
      function indmap(ix,nx)
      indmap=ix
      if(ix.lt.1)indmap=ix+nx
      if(ix.gt.nx)indmap=ix-nx
      return
      end


c       GOODANGLE converts THIN to an angle between -180 and 180.
      function goodangle(thin)
      theta=thin
      if(theta.lt.-180)theta=theta+360.
      if(theta.ge.180.)theta=theta-360.
      goodangle=theta
      return
      end


c       MAKEKERN makes a set of NKERN kernels in ARRAY, dimensioned NX by NY.
c       H is the half-length of the parabolic component of the kernel in the
c       long dimension; SIGMA is the sigma of the component of the kernel in
c       the short dimension, which is the product of a Gaussian and an
c       inverted parabola that passes through zero at sigma.  The kernels are
c       at evenly orientations between 0 and 180 degrees; THETA is returned
c       with the angle.  If IFDARK is 1 then the kernel is inverted.  IXOFS
c       and IYOFS are returned with the offsets to the start of the kernel
c       within ARRAY.
c
      subroutine makekern(array,nx,ny,sigma,h,nkern,ifdark,theta,
     &    ixofs,iyofs)
      real*4 array(nx,ny,*),theta(*)
      integer*4 ixofs(*),iyofs(*)
      real*4 dtor/0.01745329252/
c       
      thetinc=180./nkern
      xcen=(nx+1)/2.
      ycen=(ny+1)/2.
      const=(0.75/h)
c       const=(0.75/h)/(sqrt(2*3.14159)*sigma**3)
      if(ifdark.eq.0)const=-const
      do k=1,nkern
        theta(k)=(k-1)*thetinc
        costh=cos(-theta(k) * dtor)
        sinth=sin(-theta(k) * dtor)
        minx=nx
        miny=ny
        maxx=0
        maxy=0
        do iy=1,ny
          do ix=1,nx
            xx=ix-xcen
            yy=iy-ycen
c             
c             Rotate the pixel position and compute the kernel value
            xr=xx*costh-yy*sinth
            yr=xx*sinth+yy*costh
            if(abs(xr).lt.h)then
              yrat=yr**2/sigma**2
              array(ix,iy,k)=
     &            const*(1.-xr**2/h**2)*(yrat-1.)*exp(-yrat/2.)
              minx=min(minx,ix)
              maxx=max(maxx,ix)
              miny=min(miny,iy)
              maxy=max(maxy,iy)
            else
              array(ix,iy,k)=0.
            endif
          enddo
        enddo
c         
c         get offsets from min and max nonzero pixels
c         
        ixofs(k)=min(minx-1,nx-maxx)
        iyofs(k)=min(miny-1,ny-maxy)
c         
c         adjust to zero mean
c         
        sum=0.
        npix=0
        do iy=1+iyofs(k),ny-iyofs(k)
          do ix=1+ixofs(k),nx-ixofs(k)
            sum=sum+array(ix,iy,k)
            npix=npix+1
          enddo
        enddo
        dofs=sum/npix
        do iy=1+iyofs(k),ny-iyofs(k)
          do ix=1+ixofs(k),nx-ixofs(k)
            array(ix,iy,k)=array(ix,iy,k)-dofs
          enddo
        enddo
      enddo
      return
      END


c       KERNPROD computes the product of the kernel in BKERN, which is in
c       dimensioned NXK by NYK and starts at offset IXOFS, IYOFS within the
c       array, with a position IXC, IYC in ARRAY, dimensioned to NX by NY and
c       containing bytes.  DMEAN is a mean value used for positions outside the
c       the array.  The product is returned in PROD.
c
      subroutine kernprod(array,nx,ny,ixc,iyc,bkern,nxk,nyk,ixofs,
     &    iyofs,dmean,prod)
      real*4 bkern(nxk,nyk)
      integer*1 array(nx,ny)
      include 'endian.inc'
      integer*1 btmp(2)
      integer*2 i2val
      equivalence (btmp,i2val)
c       
      btmp(3-lowbyte)=0
      prod=0.
      ixbas=ixc-(nxk/2+1)
      iybas=iyc-(nyk/2+1)
      if(ixbas+ixofs.gt.0.and.ixbas+nxk-ixofs.le.nx .and.
     &    iybas+iyofs.gt.0.and.iybas+nyk-iyofs.le.ny)then
        do iyk=1+iyofs,nyk-iyofs
          iyim=iyk+iybas
          do ixk=1+ixofs,nxk-ixofs
            btmp(lowbyte)=array(ixk+ixbas,iyim)
            prod=prod+i2val*bkern(ixk,iyk)
          enddo
        enddo
      else
        do iyk=1+iyofs,nyk-iyofs
          iyim=iyk+iybas
          if(iyim.lt.1.or.iyim.gt.ny)then
            do ixk=1+ixofs,nxk-ixofs
              prod=prod+dmean*bkern(ixk,iyk)
            enddo
          else
            do ixk=1+ixofs,nxk-ixofs
              ixim=ixk+ixbas
              if(ixim.lt.1.or.ixim.gt.nx)then
                prod=prod+dmean*bkern(ixk,iyk)
              else
                btmp(lowbyte)=array(ixim,iyim)
                prod=prod+i2val*bkern(ixk,iyk)
              endif
            enddo
          endif
        enddo
      endif
      return
      end


c       Computes the minimum distance between X0,Y0 and the line segment from
c       X1,Y1 to X2,Y2.  Returns TMIN with the parameter t of the closest
c       point in the segment (i.e., x = x1 + t * (x2 - x1) ) and DISTSQ with
c       the square of the distance.
c
      subroutine point_to_line(x0,y0,x1,y1,x2,y2,tmin,distsq)
      tmin=((x0-x1)*(x2-x1)+(y0-y1)*(y2-y1))/((x2-x1)**2+(y2-y1)**2)
      tmin=max(0.,min(1.,tmin))
      distsq=(x1+tmin*(x2-x1)-x0)**2+(y1+tmin*(y2-y1)-y0)**2
      return
      end


c       EDGEMEAN computes the mean near the edge of ARRAY, dimensioned to NX by
c       NY and containing bytes.  The mean is computed in strips 5 pixels wide
c       and indented 5 pixels in from the edge and is returned in DMEAN.
c
      subroutine edgemean(array,nx,ny,dmean)
      integer*1 array(nx,ny)
      include 'endian.inc'
      integer*1 btmp(2)
      integer*2 i2val
      equivalence (btmp,i2val)
      ixh1=min(10,nx)
      ixl1=max(1,ixh1-5)
      ixl2=nx+1-ixh1
      ixh2=nx+1-ixl1
      iyh1=min(10,ny)
      iyl1=max(1,iyh1-5)
      iyl2=ny+1-iyh1
      iyh2=ny+1-iyl1
      nsum=0
      dsum=0.
      btmp(3-lowbyte)=0
      do iy=iyl1,iyh1
        do ix=ixl1,ixh2
          btmp(lowbyte)=array(ix,iy)
          dsum=dsum+i2val
        enddo
      enddo
      nsum=nsum+(iyh1+1-iyl1)*(ixh2+1-ixl1)
      do iy=iyl2,iyh2
        do ix=ixl1,ixh2
          btmp(lowbyte)=array(ix,iy)
          dsum=dsum+i2val
        enddo
      enddo
      nsum=nsum+(iyh2+1-iyl2)*(ixh2+1-ixl1)
      do iy=iyl1,iyh2
        do ix=ixl1,ixh1
          btmp(lowbyte)=array(ix,iy)
          dsum=dsum+i2val
        enddo
      enddo
      nsum=nsum+(iyh2+1-iyl1)*(ixh1+1-ixl1)
      do iy=iyl1,iyh2
        do ix=ixl2,ixh2
          btmp(lowbyte)=array(ix,iy)
          dsum=dsum+i2val
        enddo
      enddo
      nsum=nsum+(iyh2+1-iyl1)*(ixh2+1-ixl2)
      dmean=dsum/nsum
      return
      end

c$$$    subroutine typearray(array,nx,ny,ixc,iyc)
c$$$    integer*1 array(nx,ny)
c$$$    integer*4 itmp(10)
c$$$    include 'endian.inc'
c$$$    integer*1 btmp(2)
c$$$    integer*2 i2val
c$$$    equivalence (btmp,i2val)
c$$$    ixst=max(1,ixc-4)
c$$$    ixnd=min(nx,ixst+9)
c$$$    ixst=max(1,ixnd-9)
c$$$    iyst=max(1,iyc-4)
c$$$    iynd=min(ny,iyst+9)
c$$$    iyst=max(1,iynd-9)
c$$$    nty=ixnd+1-ixst
c$$$    btmp(3-lowbyte)=0
c$$$    do iy=iyst,iynd
c$$$    do ix=ixst,ixnd
c$$$    btmp(lowbyte)=array(ix,iy)
c$$$    itmp(ix+1-ixst)=i2val
c$$$    enddo
c$$$    write(*,'(10i5)')(itmp(i),i=1,nty)
c$$$    enddo
c$$$    return
c$$$    end

c       Smooths out crossings and crinkles in contour at one point
c       P_COORD has the points, NINOBJ is the number of points in the contour
c       IPNT4 is the point number for the second of 4 points numbered as
c       3 - 4 - 1 - 2
c       If the distance from 4 to segment 1-2 or the distance of 1 from segment
c       3-4 is less than tol, it slides 4 toward 3 and 1 toward 2 if necessary
c       until the angle from 4 and 1 is within 40 degrees of the mean of the
c       angles from 1 to 2 and from 3 to 4.
c
      subroutine uncrosscont(p_coord,ninobj,ipnt4,tol)
      real*4 p_coord(3,*)
      real*4 dtor/0.01745329252/
      if(ninobj.le.3)return
      ipnt3=indmap(ipnt4-1,ninobj)
      ipnt1=indmap(ipnt4+1,ninobj)
      ipnt2=indmap(ipnt4+2,ninobj)
      x1=p_coord(1,ipnt1)
      y1=p_coord(2,ipnt1)
      x2=p_coord(1,ipnt2)
      y2=p_coord(2,ipnt2)
      x3=p_coord(1,ipnt3)
      y3=p_coord(2,ipnt3)
      x4=p_coord(1,ipnt4)
      y4=p_coord(2,ipnt4)
      call point_to_line(x4,y4,x1,y1,x2,y2,tmin1,distsq1)
      call point_to_line(x1,y1,x3,y3,x4,y4,tmin2,distsq2)
      if(min(distsq1,distsq2).le.tol**2)then
        tstrt=atan2(y2-y1,x2-x1) / dtor
        tend=atan2(y4-y3,x4-x3) / dtor
        tcon=atan2(y1-y4,x1-x4) / dtor
        halfdiff=0.5*goodangle(tstrt-tend)
        tmid=goodangle(tend+halfdiff)
        halfcrit=abs(halfdiff)+40.
        if(abs(goodangle(tcon-tmid)).gt.halfcrit)then
          fraclimst=1.-1./sqrt((x2-x1)**2+(y2-y1)**2)
          fraclimnd=1.-1./sqrt((x4-x3)**2+(y4-y3)**2)
          itry=1
c           print *,'shifting point',ipnt4,' of',ninobj
          do while(itry.le.10)
            fracst=max(0.,itry*fraclimst/10.)
            fracnd=max(0.,itry*fraclimnd/10.)
            x1t=x1+fracst*(x2-x1)
            x4t=x4+fracnd*(x3-x4)
            y1t=y1+fracst*(y2-y1)
            y4t=y4+fracnd*(y3-y4)
            tcon=atan2(y1t-y4t,x1t-x4t) / dtor
            if(abs(goodangle(tcon-tmid)).le.halfcrit)itry=10
            itry=itry+1
          enddo
          p_coord(1,ipnt1)=x1t
          p_coord(2,ipnt1)=y1t
          p_coord(1,ipnt4)=x4t
          p_coord(2,ipnt4)=y4t
        endif
      endif
      return
      end

c       $Id$
c       
c       
c       CONTTRACK adjusts the positions of points in a contour perpendicular
c       to the path of the contour to fit the image data.  It does this by
c       making kernels shaped like the contour and finding the location that
c       maximizes the product of these kernels with the image data
c       ARRAY has the image data, bytes, size NX by NY
c       P_COORD(3,*) has the points of an existing contour and is returned with
c       the adjusted positions.
c       NINOBJ is the number of points in the contour
c       IPTCUR is a current point number, which is modified if NINOBJ changes
c       P_COPY(3,*) is an array for temporary storage of the contour
c       MAXLEN is the maximum number of points in the contour arrays
c       inKSIZE is the kernel size
c       inKERN is the number of kernels to make if line tracking to fill in
c       wiaps between points
c       SIGMA is the sigma for the gaussian perpendicular to the contour
c       H is the h for the parabola along the contour length
c       IFDARK is non zero for dark centers
c       STEP is a step size for line tracking
c       REDTOL is the tolerance for reduction if line tracking
c       OFFSET is an offset from the center of the line
c       LIMSHIFT is a limit on how far to shift laterally
c       inPOOL is the number of contour points to pool the kernel product over
c       inFIT is the number of points to fit over for smoothing the contour
c       
      subroutine conttrack(array,nx,ny,p_coord,ninobj,iptcur,p_copy,
     &    maxlen, inksize,inkern,sigma,h,ifdark,step,redtol,offset,
     &    limshift ,inpool,infit)
      parameter (limkern=5,limksize=21,ndlim=20,limfit=9)
      integer*1 array(nx,ny)
      real*4 p_coord(3,maxlen),p_copy(2,maxlen),xt(limfit),yt(limfit)
c       
      real*4 orig(3),bkern(limkern*limksize**2)
      data orig/0.,0.,0./
C       
      real*4 theta(limkern),costh(limkern),sinth(limkern)
      real*4 xcen(limkern),ycen(limkern)
      real*8 psum(-ndlim:ndlim),pmax
      integer*4 ixcn(limkern),iycn(limkern)
      integer*4 ixofs(limkern),iyofs(limkern),kbase(limkern)
      real*4 aprod(-ndlim:ndlim,-ndlim:ndlim,limkern)
      logical needprod(-ndlim:ndlim,-ndlim:ndlim,limkern)
      common /linmen/izmean,dmean
      real*4 dtor/0.01745329252/
C       
      trackthresh=3.
      closethresh=0.25
      tolcross=1.5*limshift
c       
      npool=max(1,min(abs(inpool),limkern))
      nfit=min(infit,limfit)
      iorder=min(2,nfit-2)
      ksize=min(2*inksize+1,limksize)
C       
      zmodel=p_coord(3,1)
      izmodel=nint(zmodel)
      if(izmean.ne.izmodel)then
        call edgemean(array,nx,ny,dmean)
        izmean=izmodel
      endif
      do k=1,limkern
        kbase(k)=1+ksize**2*(k-1)
      enddo
c       
c       Loop over all points in contour
c       
      indkcen=npool/2+1
      wdenom=indkcen
      do iptcen=1,ninobj
c         call b3dputs('top of loop')
c         
c         Make a set of contour kernels at a number of points equal to the
c         pooling size - after the first point, just need to make one more
c         kernel at new point of set and advance kernel center index indkcen
        ipdst=-npool/2
        ipdnd=npool+ipdst-1
        if(iptcen.ne.1)ipdst=ipdnd
        do ipdel=ipdst,ipdnd
          ipt=indmap(iptcen+ipdel,ninobj)
          indk=indmap(indkcen+ipdel,npool)
c           
c           Store kernel, its angles and offsets, and center position
          call makecontkern(bkern(kbase(indk)),ksize,ksize,sigma,h,
     &        ifdark,p_coord,ipt,ninobj,theta(indk),sinth(indk),
     &        costh(indk), ixofs(indk),iyofs(indk))
          xcen(indk)=p_coord(1,ipt)+orig(1)+0.5
          ycen(indk)=p_coord(2,ipt)+orig(2)+0.5
c           print *,'kernel',ipt,theta(indk),sinth(indk),costh(indk)
c           print *,'origin',xcen(indk),ycen(indk)
c           
c           If offsetting from center of line, adjust center positions
          if(offset.ne.0)then
            xcen(indk)=xcen(indk)+sinth(indk)*offset
            ycen(indk)=ycen(indk)-costh(indk)*offset
c             print *,'offset',xcen(indk),ycen(indk)
          endif
          ixcn(indk)=nint(xcen(indk))
          iycn(indk)=nint(ycen(indk))
c           
c           Initialize that we don't have products for any positions of new
c           kernel
          do i=-ndlim,ndlim
            do j=-ndlim,ndlim
              needprod(i,j,indk)=.true.
            enddo
          enddo
        enddo
c         
c         Get angle perpendicular to center kernel and dx,dy for search on that
c         perpendicular axis
        thetasrch=goodangle(theta(indkcen)+90.)
        dxsrch=cos(thetasrch * dtor)
        dysrch=sin(thetasrch * dtor)
        ndmax=0
        pmax=-1.e10
c         
c         Loop until an evaluation stops moving the center point
        moved=1
        do while(moved.eq.1)
c           call b3dputs('move loop')
          ndcur=ndmax
          moved=0
c           
c           Evaluate a summed product moving to left or right along normal
          do norminc=-1,1
            nd=ndcur+norminc
            if(abs(nd).le.limshift)then
              ipdst=-npool/2
              psum(nd)=0.
c               
c               Evaluate kernel product it each of the point positions in the
c               pool
              do ipdel=ipdst,ipdnd
                indk=indmap(indkcen+ipdel,npool)
                xtry=xcen(indk)+nd*dxsrch
                ytry=ycen(indk)+nd*dysrch
                ixtry=xtry
                iytry=ytry
                do ixc=ixtry,ixtry+1
                  do iyc=iytry,iytry+1
                    ii=ixc-ixcn(indk)
                    jj=iyc-iycn(indk)
                    if(needprod(ii,jj,indk))then
                      call kernprod(array,nx,ny,ixc,iyc,
     &                    bkern(kbase(indk)),ksize,ksize,ixofs(indk),
     &                    iyofs(indk),dmean,aprod(ii,jj,indk))
                      needprod(ii,jj,indk)=.false.
                    endif
                  enddo
                enddo
c                 
c                 Interpolate the product bilinearly
                fx=xtry-ixtry
                fy=ytry-iytry
                idx=ixtry-ixcn(indk)
                idy=iytry-iycn(indk)
                prod=((1.-fx)*aprod(idx,idy,indk)+
     &              fx*aprod(idx+1,idy,indk))*(1.-fy)+
     &              ((1.-fx)*aprod(idx,idy+1,indk)+
     &              fx*aprod(idx+1,idy+1,indk))*fy
c                 
c                 Add product into sum with weight if inpool > 0, or equally
c                 if inpool < 0
                if(inpool.gt.0)then
                  psum(nd)=psum(nd)+(1.-abs(ipdel)/wdenom)*prod
                else
                  psum(nd)=psum(nd)+prod
                endif
              enddo
c               
c               If product is bigget than max, move along normal
c               Fancy comparison needed because psum(nd) is not committed
c               as a float and looks bigger than pmax on Win-Intel 8+ 
c               Double fix to do this and make it a real*8
              if(psum(nd)-pmax .gt. 1.e-6 * abs(max(psum(nd),pmax)))then
                moved=1
                pmax=psum(nd)
                ndmax=nd
              endif
            endif
          enddo
        enddo
c         
c         interpolate offset along normal if not at edge of allowable shifts
c         
        cx=0.
        if(abs(ndmax).lt.limshift)then
          y1=psum(ndmax-1)
          y3=psum(ndmax+1)
          denom=2.*(y1+y3-2.*pmax)
          if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
          if(abs(cx).gt.0.5)cx=sign(0.5,cx)
        endif
        cxnew=xcen(indkcen)+(ndmax+cx)*dxsrch
        cynew=ycen(indkcen)+(ndmax+cx)*dysrch
c         print *,'origin',cxnew,cynew
c         
c         Reapply offset from line center if any and save contour point
        if(offset.ne.0)then
          cxnew=cxnew-sinth(indkcen)*offset
          cynew=cynew+costh(indkcen)*offset
c           print *,'offset',cxnew,cynew
        endif
        p_copy(1,iptcen)=cxnew-orig(1)-0.5
        p_copy(2,iptcen)=cynew-orig(2)-0.5
c         
        indkcen=indmap(indkcen+1,npool)
      enddo
c       call b3dputs('got through main loop')
c       
c       eliminate close points
c       
      closesq=(step*closethresh)**2
      iseg=1
      idel=0
      do while(iseg.le.ninobj)
        ip3=indmap(iseg+1,ninobj)
        if((p_copy(1,ip3)-p_copy(1,iseg))**2+
     &      (p_copy(2,ip3)-p_copy(2,iseg))**2 .lt. closesq)then
c           print *,'eliminating segment',iseg,' of',ninobj
          ip1=indmap(iseg-1,ninobj)
          ip4=indmap(iseg+2,ninobj)
          idel=iseg
          if((p_copy(1,ip1)-p_copy(1,iseg))**2+
     &        (p_copy(2,ip1)-p_copy(2,iseg))**2 .gt.
     &        (p_copy(1,ip3)-p_copy(1,ip4))**2+
     &        (p_copy(2,ip3)-p_copy(2,ip4))**2)idel=ip3
          do i=idel+1,ninobj
            p_copy(1,i-1)=p_copy(1,i)
            p_copy(2,i-1)=p_copy(2,i)
          enddo
          ninobj=ninobj-1
        else
          iseg=iseg+1
        endif
      enddo
c       
c       Replace original contour
      do i=1,ninobj
        p_coord(1,i)=p_copy(1,i)
        p_coord(2,i)=p_copy(2,i)
      enddo
c       call b3dputs('got through elimination')
c       
c       track between points that are far apart
c       
      threshsq=(step*trackthresh)**2
      iptcen=1
      ifadd=0
      do while (iptcen.lt.ninobj)
        if((p_coord(1,iptcen)-p_coord(1,iptcen+1))**2+
     &      (p_coord(2,iptcen)-p_coord(2,iptcen+1))**2 .ge.threshsq)
     &      then
          ninold=ninobj
          call linetrack(array,nx,ny,p_coord,ninobj,iptcen,maxlen,
     &        inksize,inkern,sigma,h,ifdark,step,redtol,0,offset,
     &        0,iffail)
          if(ninobj.gt.ninold)ifadd=1
        endif
        iptcen=iptcen+1
      enddo
c       call b3dputs('got through tracking')
c       
c       smoothing next
c       
      if (iorder .gt. 0) then
        do i=1,ninobj
          p_copy(1,i)=p_coord(1,i)
          p_copy(2,i)=p_coord(2,i)
        enddo
        do iptcen=1,ninobj
          xmid=p_copy(1,iptcen)
          ymid=p_copy(2,iptcen)
          if(iorder.gt.0.and.nfit.gt.iorder.and.nfit.le.ninobj)then
c             
c             Copy points into fitting array, then rotate so X is along
c             line from first to last point being fit
            do i=1,nfit
              ipt=indmap(iptcen+i-1-nfit/2,ninobj)
              xt(i)=p_copy(1,ipt)
              yt(i)=p_copy(2,ipt)
            enddo
            dx=xt(nfit)-xt(1)
            dy=yt(nfit)-yt(1)
            dlen=sqrt(dx**2+dy**2)
            costs=dx/dlen
            sints=-dy/dlen
            do i=1,nfit
              xtmp=xt(i)-xmid
              xt(i)=costs*xtmp-sints*(yt(i)-ymid)
              yt(i)=sints*xtmp+costs*(yt(i)-ymid)
            enddo
c             
c             Fit line or parabola to the Y versus X
            if(iorder.eq.1)then
              call lsfit(xt,yt,nfit,slop1,bint,ro)
            else
              call quadfit(xt,yt,nfit,slop2,slop1,bint)
            endif
            xmid=sints*bint+xmid
            ymid=costs*bint+ymid
c             print *,nfit,iptcen,p_copy(1,iptcen),p_copy(2,iptcen),bint,xmid,ymid
          endif
c           
c           Replace with midpoint of fit
          p_coord(1,iptcen)=xmid
          p_coord(2,iptcen)=ymid
        enddo
      endif
c       call b3dputs('got through smoothing')
c       
c       fix up points that are crossed
c       
      do ipt=1,ninobj 
        call uncrosscont(p_coord,ninobj,ipt,tolcross)
      enddo
      if(ifadd.ne.0.or.idel.ne.0)iptcur=ninobj-1
      return
      end


c       MAKECONTKERN makes a kernel shaped like the contour around one point
c       ARRAY receives the kernel
c       NX, NY are the sizes of the kernel in X and Y
c       SIGMA is the sigma of the gaussian in direction across the contour
c       H is the H of the parabola along the contour
c       IFDARK nonzero for dark lines
c       P_COPY(3,*) is the contour array
c       IPT is the index of the center point
c       NINOBJ is the number of points in contour
c       THETA is returned with the angle of a line connecting the points before
c       and after the center point
c       SINTH and COSTH are returned with the angle's sine and cosine
c       IXOFS, IYOFS are offsets to start of kernel in array
c       
      subroutine makecontkern(array,nx,ny,sigma,h, ifdark,p_copy,ipt,
     &    ninobj,theta, sinth,costh,ixofs,iyofs)
      real*4 array(nx,ny),p_copy(3,*)
      parameter (limrot=25)
      real*4 xt(-limrot:limrot),yt(-limrot:limrot)
      real*4 patht(-limrot:limrot)
      integer*4 irlim(-1:1)
      real*4 dtor/0.01745329252/
c       
      xcen=(nx+1)/2.
      ycen=(ny+1)/2.
      const=(0.75/h)
      if(ifdark.eq.0)const=-const
      ipnex=indmap(ipt+1,ninobj)
      iplas=indmap(ipt-1,ninobj)
c       
c       Get angle of line through previous and next points to the center point
      dx=p_copy(1,ipnex)-p_copy(1,iplas)
      dy=p_copy(2,ipnex)-p_copy(2,iplas)
      dlen=sqrt(dx**2+dy**2)
      sinth=dy/dlen
      costh=dx/dlen
      theta=atan2(dy,dx) / dtor
c       
c       Set up to go each direction from the center point
      xmid=p_copy(1,ipt)
      ymid=p_copy(2,ipt)
      xt(0)=0.
      yt(0)=0.
      patht(0)=0.
      do idir=-1,1,2
        irot=0
        ifterm=0
        iptr=ipt
        pathlen=0.
c         
c         Rotate points so the previous-next line is the X axis
c         Build list of rotated points and path length at each point
        do while (abs(irot).lt.limrot.and.ifterm.eq.0)
          iptr=indmap(iptr+idir,ninobj)
          xr=costh*(p_copy(1,iptr)-xmid)+sinth*(p_copy(2,iptr)-ymid)
          yr=-sinth*(p_copy(1,iptr)-xmid)+costh*(p_copy(2,iptr)-ymid)
          if(idir*xr.le.idir*xt(irot))then
            ifterm=1
          else
            pathlen=pathlen+sqrt((xr-xt(irot))**2+(yr-yt(irot))**2)
            irot=irot+idir
            xt(irot)=xr
            yt(irot)=yr
            patht(irot)=pathlen
            if(abs(xr).gt.xcen)ifterm=1
          endif
        enddo
        irlim(idir)=irot
      enddo
c       
      minx=nx
      miny=ny
      maxx=0
      maxy=0
c       
c       Loop over the points in the kernel
      do iy=1,ny
        do ix=1,nx
c           
c           Rotate each point around kernel center as contour points were above
          xx=ix-xcen
          yy=iy-ycen
          xr=xx*costh+yy*sinth
          yr=-xx*sinth+yy*costh
          idir=nint(sign(1.,xr))
          irot=0
          ifgot=0
c           
c           Find point along rotated contour that is at same X
          do while(irot.ne.irlim(idir).and.ifgot.eq.0)
            if(idir*xt(irot+idir).gt.idir*xr)then
              ifgot=1
c               
c               Get Y deviation of the point from the contour and the path
c               length along contour from center
              yint=yt(irot)+(xr-xt(irot))*(yt(irot+idir)-yt(irot))/
     &            (xt(irot+idir)-xt(irot))
              ydiff=yr-yint
              pathlen=patht(irot)+
     &            sqrt((xr-xt(irot))**2+(yr-yt(irot))**2)
            endif
            irot=irot+idir
          enddo
          if(abs(pathlen).lt.h.and.ifgot.eq.1)then
c             
c             If path length within range, compute kernel value
            yrat=ydiff**2/sigma**2
            array(ix,iy)=
     &          const*(1.-pathlen**2/h**2)*(yrat-1.)*exp(-yrat/2.)
            minx=min(minx,ix)
            maxx=max(maxx,ix)
            miny=min(miny,iy)
            maxy=max(maxy,iy)
          else
            array(ix,iy)=0.
          endif
        enddo
      enddo
c       
c       get offsets from min and max nonzero pixels
c       
      ixofs=min(minx-1,nx-maxx)
      iyofs=min(miny-1,ny-maxy)
c       
c       adjust to zero mean
c       
      sum=0.
      npix=0
      do iy=1+iyofs,ny-iyofs
        do ix=1+ixofs,nx-ixofs
          sum=sum+array(ix,iy)
          npix=npix+1
        enddo
      enddo
      dofs=sum/npix
      do iy=1+iyofs,ny-iyofs
        do ix=1+ixofs,nx-ixofs
          array(ix,iy)=array(ix,iy)-dofs
        enddo
      enddo
      return
      END

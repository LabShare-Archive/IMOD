c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.3  2005/04/09 14:53:55  mast
c       Fixed a line length problem
c       
c       Revision 3.2  2005/04/07 03:56:31  mast
c       New version with local tracking, new mapping, outliers, etc.
c       
c       Revision 3.1  2002/07/28 22:56:31  mast
c       Standardize error output
c       
c       
c       NEXTPOS computes the projected position of a point from positions on
c       nearby views in the absence of a tilt alignment
c       IOBJ is the object #
c       IPNEAR is the point number of the nearest point in Z
c       IDIR is the direction (+1/-1)
c       IZNEXT is the Z value of the view to project to
c       TILT is array of tilt angles
c       NFIT and MINFIT are maximum and minimum number of points to fit
c       IAXTILT is 1 if tilt axis is near 0 degrees
c       TILTMIN is minimum tilt angle for fitting to sin/cosine
c       XNEXT, YNEXT is the projected position
c       
      subroutine nextpos(iobj,ipnear,idir,iznext,tilt,nfit,minfit,
     &    iaxtilt, tiltmin, xnext, ynext)
      implicit none
      include 'smallmodel.inc'
      integer idim
      parameter (idim=50)
      real*4 tilt(*)
      real*4 xx(idim),yy(idim),zz(idim)
      include 'statsize.inc'
      real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     &    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &    , b(msiz), b1(msiz)
      integer*4 iobj,ipnear,idir,iznext,nfit,minfit,iaxtilt
      real*4 tiltmin,xnext,ynext
      integer*4 ibase,ninobj,mfit,ipend,indx,indy,iptnear,ip,ipt,ipb,ipp
      integer*4 ipast,ibefo,i
      real*4 xsum,ysum,slope,bint,ro,const,rsq,fra,thetnext,xtmp,theta
      real*4 cosd,sind

      ibase=ibase_obj(iobj)
      ninobj=npt_in_obj(iobj)
      mfit=0
      ipend=1
      if(idir.eq.-1)ipend=ninobj
      if(iaxtilt.le.1)then
        indx=1
        indy=2
      else
        indx=2
        indy=1
      endif
c       
c       set pointer past the view if possible
c       
      iptnear=object(ibase+ipnear)
      ip=ipnear
      do while(idir*(nint(p_coord(3,object(ibase+ip)))-iznext).ge.0
     &    .and.idir*(ip-ipend).gt.0)
        ip=ip-idir
      enddo
      if(idir*(nint(p_coord(3,object(ibase+ip)))-iznext).eq.-1)then
c         
c         if point is adjacent in that direction, then starting from there,
c         load nfit points
c         
        do while(idir*(ip-ipend).ge.0.and.mfit.lt.nfit)
          ipt=object(ibase+ip)
          mfit=mfit+1
          if (mfit .gt. idim) call errorexit(
     &        'TOO MANY POINTS IN NON-TILT ALIGNMENT FITS FOR ARRAYS', 0)
          xx(mfit)=p_coord(indx,ipt)
          yy(mfit)=p_coord(indy,ipt)
          zz(mfit)=p_coord(3,ipt)
          ip=ip-idir
        enddo
      elseif(nint(p_coord(3,iptnear)).eq.iznext)then
c         
c         otherwise, if there is already a point on the section, just take it
c         
        mfit=1
        xx(mfit)=p_coord(indx,iptnear)
        yy(mfit)=p_coord(indy,iptnear)
        zz(mfit)=iznext
      else
c         
c         otherwise, set pointers to points past and before view and get
c         points from both directions
c         
        ipast=ipnear
        if(p_coord(3,object(ibase+ipnear)).lt.iznext)ipast=ipnear+1
        ibefo=ipast-1
c         
c         starting from there, load nfit nearest points
c         
        do while((ibefo.gt.0.or.ipast.le.ninobj).and.mfit.lt.nfit)
          ipb=object(ibase+ibefo)
          ipp=object(ibase+ipast)
c           
c           take the only one that's legal
c           
          if(ibefo.le.0)then
            ipt=ipp
            ipast=ipast+1
          elseif(ipast.gt.ninobj)then
            ipt=ipb
            ibefo=ibefo-1
          else
c             
c             or if both are legal, take the one closest to target view
c             
            if(abs(p_coord(3,ipb)-iznext).lt.
     &          abs(p_coord(3,ipp)-iznext)) then
              ipt=ipb
              ibefo=ibefo-1
            else
              ipt=ipp
              ipast=ipast+1
            endif
          endif
          mfit=mfit+1
          if (mfit .gt. idim) call errorexit(
     &        'TOO MANY POINTS IN NON-TILT ALIGNMENT FITS FOR ARRAYS', 0)
          xx(mfit)=p_coord(indx,ipt)
          yy(mfit)=p_coord(indy,ipt)
          zz(mfit)=p_coord(3,ipt)
        enddo
      endif
c       
      if(mfit.lt.minfit)then
        xsum=0.
        ysum=0.
        do i=1,mfit
          xsum=xsum+xx(i)
          ysum=ysum+yy(i)
        enddo
        xnext=xsum/mfit
        ynext=ysum/mfit
c         print *,'average:',iobj,mfit,xnext,ynext
      else
        call lsfit(zz,xx,mfit,slope,bint,ro)
        xnext=iznext*slope+bint
        if(iaxtilt.eq.0.or.abs(tilt(iznext+1)).lt.tiltmin)then 
          call lsfit(zz,yy,mfit,slope,bint,ro)
          ynext=iznext*slope+bint
c           print *,'linear fit:',iobj,mfit,xnext,ynext
        else
          do i=1,mfit
            theta=tilt(nint(zz(i))+1)
            xr(1,i)=cosd(theta)
            xr(2,i)=sind(theta)
            xr(3,i)=yy(i)
          enddo
          call multr(xr,3,mfit,sx,ss,ssd,d,r,xm,sd,b,b1,
     &        const, rsq ,fra)
          thetnext=tilt(iznext+1)
          ynext=b1(1)*cosd(thetnext)+b1(2)*sind(thetnext)+const
c           print *,'sin/cos fit:',iobj,mfit,xnext,ynext
        endif
      endif
      if(iaxtilt.gt.1)then
        xtmp=xnext
        xnext=ynext
        ynext=xtmp
      endif
      return
      end


      


c       find_PIECE takes a list of NPCLIST piece coordinates in
c       I[XYZ]PCLIST, the piece dimensions NX and NY, and index coordinates
c       in the montaged image IND[XYZ], and finds the piece that those
c       coordinates are in (IPCZ) and the coordinates IPCX, IPCY of the point
c       in that piece
c       
      subroutine find_piece(ixpclist,iypclist,izpclist, npclist,nx,
     &    ny,nxbox,nybox,xnext,ynext,iznext,ix0,ix1,iy0,iy1,ipcz)
c       
      integer*4 ixpclist(*),iypclist(*),izpclist(*)
c       
      indx0=nint(xnext)-nxbox/2
      indx1=indx0+nxbox-1
      indy0=nint(ynext)-nybox/2
      indy1=indy0+nybox-1
      ipcz=-1
      do ipc=1,npclist
        if(iznext.eq.izpclist(ipc).and.
     &      indx0.ge.ixpclist(ipc).and.indx1.lt.ixpclist(ipc)+nx .and.
     &      indy0.ge.iypclist(ipc).and.indy1.lt.iypclist(ipc)+ny)then
          ipcz=ipc-1
          ix0=indx0-ixpclist(ipc)
          ix1=indx1-ixpclist(ipc)
          iy0=indy0-iypclist(ipc)
          iy1=indy1-iypclist(ipc)
          return
        endif
      enddo
      return
      end




      SUBROUTINE QDshift(ARRAY,BRAY,NX,NY,xt,yt)
      DIMENSION ARRAY(nx,ny),BRAY(nx,ny)
C       
C       Loop over output image
C       
      dx=-xt
      dy=-yt
      dxsq=dx**2
      dysq=dy**2
      DO IYP = 1,NY
        DO IXP = 1,NX
C           
C           Do quadratic interpolation
C           
          IXPP1 = IXP + 1
          IXPM1 = IXP - 1
          IYPP1 = IYP + 1
          IYPM1 = IYP - 1
          IF (IXPM1 .LT. 1) IXPM1 = 1
          IF (IYPM1 .LT. 1) IYPM1 = 1
          IF (IXPP1 .GT. NX) IXPP1 = NX
          IF (IYPP1 .GT. NY) IYPP1 = NY
C           
C           Set up terms for quadratic interpolation
C           
          V2 = ARRAY(IXP, IYPM1)
          V4 = ARRAY(IXPM1, IYP)
          V5 = ARRAY(IXP, IYP)
          V6 = ARRAY(IXPP1, IYP)
          V8 = ARRAY(IXP, IYPP1)
C           
          A = (V6 + V4)*.5 - V5
          B = (V8 + V2)*.5 - V5
          C = (V6 - V4)*.5
          D = (V8 - V2)*.5
C           
          
          bray(ixp,iyp)=(A*DXsq + B*DYsq + C*DX + D*DY + V5)
C           
        enddo
      enddo
C       
      RETURN
      END



      subroutine setmeanzero(array,nxdim,nydim,nx,ny)
      real*4 array(nxdim,nydim)
      sum=0.
      do iy=1,ny
        sumt=0.
        do ix=1,nx
          sumt=sumt+array(ix,iy)
        enddo
        sum=sum+sumt
      enddo
      avg=sum/(nx*ny)
      do iy=1,ny
        do ix=1,nx
          array(ix,iy)=array(ix,iy)-avg
        enddo
      enddo
      return
      end


c       PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c       in the array ARRAY, which is dimensioned to nx+2 by ny.
c       
      subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,peak)
      real*4 array(nxplus,nyrot)
      nxrot=nxplus-2
c       
c       find peak
c       
      peak=-1.e30
      do iy=1,nyrot
        do ix=1,nxrot
          if(array(ix,iy).gt.peak)then
            peak=array(ix,iy)
            ixpeak=ix
            iypeak=iy
          endif
        enddo
      enddo
c       print *,ixpeak,iypeak
c       
c       return adjusted pixel coordinate minus 1
c       
      xpeak=ixpeak-1.
      ypeak=iypeak-1.
      if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
      if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
c       print *,xpeak,ypeak
      return
      end



      subroutine calcg(boxtmp,nxbox,nybox,xpeak,ypeak,idxin,
     &    idyin,ninside,idxedge,idyedge,nedge,ipolar,wsum)
      real*4 boxtmp(nxbox,nybox)
      integer*4 idxin(*),idyin(*),idxedge(*),idyedge(*)
c       
      ixcen=nxbox/2+nint(xpeak)
      iycen=nybox/2+nint(ypeak)
c       
c       look around, find most extreme 4 points as center
c       
      if(ixcen.ge.2.and.ixcen.le.nxbox-2.and.
     &    iycen.ge.2.and.iycen.le.nybox-2)then
        best=0.
        do iy=iycen-1,iycen+1
          do ix=ixcen-1,ixcen+1
            sum4=(boxtmp(ix,iy)+boxtmp(ix+1,iy)+
     &          boxtmp(ix,iy+1)+ boxtmp(ix+1,iy+1))
            if(ipolar*sum4.gt.ipolar*best.or.best.eq.0.)then
              ixbest=ix
              iybest=iy
              best=sum4
            endif
          enddo
        enddo
        ixcen=ixbest
        iycen=iybest
      endif
      sum=0.
      nsum=0
      xsum=0.
      ysum=0.
      wsum=0.
c       
c       find edge mean - require half the points to be present
c       
      do i=1,nedge
        ix=ixcen+idxedge(i)
        iy=iycen+idyedge(i)
        if(ix.ge.1.and.ix.le.nxbox.and.iy.ge.1.and.iy.le.nybox)then
          sum=sum+boxtmp(ix,iycen+idyedge(i))
          nsum=nsum+1
        endif
      enddo
      if(nsum.lt.nedge/2)return
      edge=sum/nsum
c       
c       subtract edge and get weighted sum of pixel coordinates for POSITIVE
c       pixels
c       
      do i=1,ninside
        ix=ixcen+idxin(i)
        iy=iycen+idyin(i)
        if(ix.ge.1.and.ix.le.nxbox.and.iy.ge.1.and.iy.le.nybox)then
          weight=boxtmp(ix,iy)-edge
          if(ipolar*weight.gt.0.)then
            xsum=xsum+ix*weight
            ysum=ysum+iy*weight
            wsum=wsum+weight
          endif
        endif
      enddo
      if(wsum.eq.0.)return
      xpeak=xsum/wsum-0.5-nxbox/2
      ypeak=ysum/wsum-0.5-nybox/2
      wsum=wsum*ipolar
      return
      end


      subroutine rescue(boxtmp,nxbox,nybox,xpeak,ypeak, idxin, idyin,
     &    ninside,idxedge,idyedge, nedge,ipolar,radmax,relaxcrit,wsum)
      real*4 boxtmp(*)
      integer*4 idxin(*),idyin(*),idxedge(*),idyedge(*)
      wsum=0.
      rad=0.
      radinc=1.5
      do while(rad.le.radmax .and.wsum.eq.0.)
        radsq=rad**2
        radosq=min(rad+radinc,radmax)**2
        wbest=0.
        do idy=-nybox/2,nybox/2
          do idx=-nxbox/2,nxbox/2
            distsq=idx**2+idy**2
            if(distsq.gt.radsq.and. distsq.le.radosq)then
              xtmp=idx
              ytmp=idy
              call calcg(boxtmp,nxbox,nybox,xtmp, ytmp, idxin, idyin,
     &            ninside, idxedge,idyedge, nedge,ipolar, wtmp)
              if(wtmp.gt.wbest)then
                wbest=wtmp
                xpeak=xtmp
                ypeak=ytmp
              endif
            endif
          enddo
        enddo
        rad=rad+radinc
        if(wbest.ge. relaxcrit)wsum=wbest
      enddo
      return
      end



      subroutine add_point(iobj,ipnear, xpos,ypos,iznext)
      include 'smallmodel.inc'
      logical failed
c       
      call object_mover(iobj,failed)
      if(failed)call errorexit('insufficient object space',0)
      ibase=ibase_obj(iobj)
      n_point=n_point+1
      p_coord(1,n_point)=xpos
      p_coord(2,n_point)=ypos
      p_coord(3,n_point)=iznext
      pt_label(n_point)=0
      npt_in_obj(iobj)=npt_in_obj(iobj)+1
      ipadd=ipnear
      if (ipadd .eq. 0) then
        ipadd = npt_in_obj(iobj)
        if (ipadd .eq. 1) then
          ibase_obj(iobj) = ibase_free
          ibase = ibase_free
        endif
      elseif (nint(p_coord(3,object(ibase+ipadd))).lt.iznext) then
        ipadd=ipadd+1
      endif
      do j=ibase+npt_in_obj(iobj),ibase+ipadd+1,-1
        object(j)=object(j-1)
      enddo
      object(ibase+ipadd)=n_point
      ibase_free=ibase_free+1
      if (ipnear.ne. 0) ipnear=ipadd
      return
      end


c       READRAWSCALE takes information about the desired image box ix0,ix1,
c       iy0,iy1 from image file of dimensions nx,ny, and a size nxraw,nyraw
c       of a larger box to be used for local scaling, and the target mean
c       and sd in targavg and targsd.  It reads data into the array rawtmp,
c       scales them, and places the desired box of data into boxtmp

      subroutine readrawscale(rawtmp,boxtmp,nx,ny,nxraw,nyraw,
     &    nxbox,nybox,ix0,ix1,iy0,iy1,targavg,targsd)
      real*4 rawtmp(*),boxtmp(*)
c       
      ixr0=max(0,ix0-(nxraw-nxbox)/2)
      ixr1=min(nx-1,ixr0+nxraw-1)
      ixr0=max(0,ixr1+1-nxraw)
      nxread=ixr1+1-ixr0
      iyr0=max(0,iy0-(nyraw-nybox)/2)
      iyr1=min(ny-1,iyr0+nyraw-1)
      iyr0=max(0,iyr1+1-nyraw)
      nyread=iyr1+1-iyr0
      call irdpas(1,rawtmp,nxread,nyread,ixr0,ixr1,iyr0,iyr1,*99)
      call avgsd(rawtmp,nxread*nyread,avg,sd,sem)
      call irepak(boxtmp,rawtmp,nxread,nyread,ix0-ixr0,ix1-ixr0,
     &    iy0-iyr0,iy1-iyr0)
c       
c       Ad hoc protection against zero, should be improved
c       
      if(sd.lt.0.0001)sd = 0.0001
      dscal=targsd/sd
      dadd=targavg-avg*dscal
      do i=1,nxbox*nybox
        boxtmp(i)=boxtmp(i)*dscal+dadd
      enddo
      return
99    print *,'ERROR READING IMAGE FROM FILE'
      call exit(1)
      end

c       SETSIZ_SAM_CEL modifies the header size, sampling and cell size
c       for the MRC file open on unit IUNIT, given dimensions NX, NY, NZ.
c       It preserves the pixel size in Unit 1
c       
      subroutine setsiz_sam_cel(iunit,nx,ny,nz)
      integer*4 nxyz(3),nxyzst(3)
      real*4 cell(6),delta(3)
      data cell/0.,0.,0.,90.,90.,90./
      data nxyzst/0,0,0/
c       
      call irtdel(1,delta)
      nxyz(1)=nx
      nxyz(2)=ny
      nxyz(3)=nz
      cell(1)=nx*delta(1)
      cell(2)=ny*delta(2)
      cell(3)=nz*delta(3)
      call ialsiz(iunit,nxyz,nxyzst)
      call ialsam(iunit,nxyz)
      call ialcel(iunit,cell)
      return
      end

c       SPLITPACK splits ARRAY into the 4 corners of BRRAY
c       ARRAY is dimensioned NXDIM by NY, data are NX by NY and pack into BRRAY
c       
      subroutine splitpack(array, nxdim, nx, ny, brray)
      implicit none
      integer*4 nx, nxdim, ny, ixnew, iynew, ix, iy
      real*4 array(nxdim,ny),brray(nx,ny)
      do iy=1,ny
        do ix=1,nx
          ixnew=mod(ix+nx/2-1,nx)+1
          iynew=mod(iy+ny/2-1,ny)+1
          brray(ixnew,iynew)=array(ix,iy)
        enddo
      enddo
      return
      end


c       countMissing counts the number of points missing from object IOBJ
c       NVUALL is the total number of views, NEXCLUDE excluded views are
c       in IZEXCLUDE, MISSING is a logical array to use, and the number
c       or missing views and the list of views are returned in NLISTZ and
c       LISTZ
c       
      subroutine countMissing(iobj, nvuall, izexclude, nexclude, missing,
     &    listz, nlistz)
      implicit none
      include 'smallmodel.inc'
      logical missing(0:*)
      integer*4 izexclude(*), listz(*), iobj, nvuall, nexclude, nlistz
      integer*4 ninobj,i,ibase,ip,iz
c       
      ninobj=npt_in_obj(iobj)
      nlistz = 0
      if(ninobj.eq.0)return
      ibase=ibase_obj(iobj)
      do i=1,nvuall
        missing(i)=.true.
      enddo
      do i=1,nexclude
        missing(izexclude(i))=.false.
      enddo
      do ip=1,ninobj
        iz=nint(p_coord(3,object(ibase+ip)))+1
        missing(iz)=.false.
      enddo
      nlistz=0
      do i=1,nvuall
        if(missing(i))then
          nlistz=nlistz+1
          listz(nlistz)=i
        endif
      enddo
      return
      end

      logical function itemOnList(item, list, nlist)
      implicit none
      integer*4 list(*), item, nlist, i
      itemOnList = .true.
      do i = 1, nlist
        if (item .eq. list(i)) return
      enddo
      itemOnList = .false.
      return
      end

c       FINDXF_WO_OUTLIERS calls FINDXF to get a 2D transformation,
c       and eliminates outlying position-pairs from the solution
c       
c       XR is the data matrix, NDAT is the full amount of data
c       XCEN, YCEN should contain the center coordinates that have been
c       subtracted from the X and Y point data.
c       IFTRANS and IFROTRANS should be 0 to get a general linear transform
c       IFTRANS should be 1 to solve for translation only
c       IFROTRANS should be 1 to solve for rotations and translations, or
c       2 to solve for magnification also
c       MAXDROP is the maximum number of points to drop as outliers
c       CRITPROB and CRITABS are the criterion probabilities for considering
c       a point an outlier
c       If the maximum residual is below ELIMMIN nothing will be eliminated
c       NDROP is number of points dropped, point numbers returned in IDROP
c       F is the 2 by 3 matrix transformation computed
c       DEVAVG, DEVSD, DEVMAX are mean, SD, and maximum deviations
c       IPNTMAX give the point number at which the maximum occurred
c       
      subroutine findxf_wo_outliers(xr,ndat,xcen,ycen,iftrans,
     &    ifrotrans,maxdrop, critprob, critabs, elimmin, idrop,ndrop,
     &    f, devavg,devsd, devmax, ipntmax)
      implicit none
      include 'statsize.inc'
      integer*4 idrop(*)
      real*4 xr(msiz,*)
      integer*4 iftrans,ifrotrans
      real*4 f(2,3),xcen,ycen
      integer*4 ndat,maxdrop,ndrop,ipntmax
      real*4 critprob,critabs,elimmin,devavg,devsd,devmax
      integer*4 i,j,lastdrop,itmp,nkeep,jdrop,isaveBase,icolDev,indexCol
      real*4 probperpt,absperpt,sigfromavg,sigfromsd,sigma,z,prob,xx,yy
      real*4 erfcc,gprob
      gprob(z)=1.-0.5*erfcc(z/1.414214)
      isaveBase = 13
      icolDev = 13
      indexCol = 19
c       
c       get probability per single point from the overall criterion prob
c       
      probperpt=(1.-critprob)**(1./ndat)
      absperpt=(1.-critabs)**(1./ndat)
c       
c       copy the data into far columns 
c       
      do i=1,ndat
        do j=1,5
          xr(j+isaveBase,i)=xr(j,i)
        enddo
      enddo

      call findxf(xr,ndat,xcen,ycen,iftrans,ifrotrans,1,f,devavg,
     &    devsd,devmax,ipntmax)
      ndrop = 0
      if(maxdrop.eq.0.or.devmax.lt.elimmin) return
c       
c       Sort the residuals and keep index back to initial values
c       
      lastdrop=0
      do i=1,ndat
        idrop(i)=i
      enddo
      do i=1,ndat-1
        do j=i+1,ndat
          if(xr(icolDev,idrop(i)).gt.xr(icolDev,idrop(j)))then
            itmp=idrop(i)
            idrop(i)=idrop(j)
            idrop(j)=itmp
          endif
        enddo
      enddo
c       
c       load the data in this order, save index in xr
c       
      do i=1,ndat
        do j=1,5
          xr(j,i)=xr(j+isaveBase,idrop(i))
        enddo
        xr(indexCol, i) = idrop(i)
      enddo
c       
c       Drop successively more points: get mean and S.D. of the remaining
c       points and check how many of the points pass the criterion 
c       for outliers.  
c       
      do jdrop = 1, maxdrop+1
        call findxf(xr,ndat-jdrop,xcen,ycen,iftrans,ifrotrans,1,f,devavg,
     &      devsd,devmax,ipntmax)
c         
c         get deviations for points out of fit
c         
        do i = ndat + 1 - jdrop, ndat
          call xfapply(f,0.,0.,xr(1,i),xr(2,i),xx, yy)
          xr(icolDev, i) = sqrt((xx - xr(1,i))**2 + (yy - xr(2,i))**2)
        enddo
c         
c         estimate the sigma for the error distribution as the maximum of
c         the values implied by the mean and the SD of the deviations
c         
        sigfromavg=devavg/sqrt(8./3.14159)
        sigfromsd=devsd/sqrt(3.-8./3.14159)
        sigma=max(sigfromavg,sigfromsd)

        nkeep=0
        do j=ndat-jdrop+1,ndat
          z=xr(icolDev,j)/sigma
          prob=2*(gprob(z)-0.5)-sqrt(2./3.14159)*z*exp(-z**2/2.)
          if(prob.lt.probperpt)nkeep=nkeep+1
          if(prob.ge.absperpt) ndrop=min(maxdrop,max(ndrop,ndat+1-j))
        enddo
c         
c         If all points are outliers, this is a candidate for a set to drop
c         When only the first point is kept, and all the rest of the points
c         were outliers on the previous round, then this is a safe place to
c         draw the line between good data and outliers.  In this case, set
c         ndrop; and at end take the biggest ndrop that fits these criteria
c         
        if(nkeep.eq.0)lastdrop=jdrop
        if(nkeep.eq.1.and.lastdrop.eq.jdrop-1.and.lastdrop.gt.0)
     &      ndrop=lastdrop
c         print *,'drop',jdrop,', keep',nkeep,', lastdrop',lastdrop,
c         &           ',  ndrop =',ndrop
      enddo
c       
c       when finish loop, need to redo with right amount of data and restore
c       data
c       
      do i=1,ndrop
        idrop(i)=nint(xr(indexCol, ndat+i-ndrop))
      enddo
      call findxf(xr,ndat-ndrop,xcen,ycen,iftrans,ifrotrans,1,f,devavg,
     &    devsd,devmax,ipntmax)
      ipntmax=nint(xr(indexCol,ipntmax))
      do i=1,ndat
        do j=1,5
          xr(j,i)=xr(j+isaveBase,i)
        enddo
      enddo
      return
      end

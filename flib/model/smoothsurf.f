*       * * * * * * SMOOTHSURF * * * * * * *
c       
c       This program will smooth a surface defined by model contours,
c       adjusting the positions of points in each contour as needed.  At each
c       point on the surface, it fits a 3-D polynomial to all points within
c       a defined range of Z-levels and within a specified distance of the
c       central point.  That point's position is then replaced by the fitted
c       position from the polynomial.  After this surface smoothing
c       operation, each contour is independently smoothed by local fitting to
c       2-D (ordinary) polynomials.
c       
c       See man page for more details
c       
c       David Mastronarde, 9/9/97
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.6  2006/03/02 00:25:25  mast
c       Move polyterm to library
c
c       Revision 3.5  2005/12/09 04:43:27  mast
c       gfortran: .xor., continuation, format tab continuation or byte fixes
c
c       Revision 3.4  2005/05/31 00:56:23  mast
c       Switched to loading one object at a time
c       
c       Revision 3.3  2004/06/18 03:10:36  mast
c       Called PipDone and added a reminder to remesh
c       
c       Revision 3.2  2004/06/17 16:38:03  mast
c       Fixed to work with index coordinates instead of pixel-size dependent
c       model coordinates, fixed bug in contour smoothing that might have
c       kept it from happened, converted to PIP and provided defaults
c       
c       Revision 3.1  2003/09/22 19:29:08  mast
c       removed mysterious common /dumcom/dummy(1500000) - failed on Cygwin
c       
c       
      implicit none
      integer idim, limpt, idzlim, limflags
      include 'model.inc'
      parameter (idim=20000,limpt=100000,idzlim=5000, limflags=20000)
C       
      REAL*4 xt(limpt),yt(limpt),zt(limpt),pt(limpt,3),p_new(2,max_pt)
      equivalence (xt,pt),(yt,pt(1,2)),(zt,pt(1,3))
C       
      CHARACTER*320 FILin,filout
c       
      logical readw_or_imod,failed,getModelObjectRange
      include 'statsize.inc'
      real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     &    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &    , b(msiz), b1(msiz)
      real*4 vect(msiz),slop(msiz)
      integer*4 izobj(max_obj_num)
      integer*4 iobjbest(-idzlim:idzlim),iptbest(-idzlim:idzlim)
      integer*4 iobjdo(limflags),iflags(limflags)
      logical*1 objectOK(max_obj_num)

      integer*4 minpts, nobjdo, iorder, nzfit, iorder2, ierr, ifflip
      real*4 tolcross, closethresh, ximscale,yimscale,zimscale,xofs,yofs,zofs
      real*4 sepmin, distlim, xyscal, zscale, dzsq, distmin, dx, dy, distsq
      integer*4 iobj, ninobj, ibase, ipnt1, i, ifonlist, imodobj, icheck, ipt
      integer*4 ifspan, idzlo, idzhi, idzmin, idzmax, loop, iz, jobj, jbase
      integer*4 jpnt, jpt, iplas, ipnex, mzfit, nfit, iftoofar, ifsave, j
      real*4 xx, yy, dlen, sinth, costh, xlas, ylas, xrot, yrot, dist, frac
      real*4 xcen, ycen, sep, distlas, distcen, ynew, c1, rsq, fra, xmid
      real*4 ymid, tmp, bint
      integer*4 ninsert, norder, nindep, ichek, ipnt, idz, ninj, idir, iptcen
      integer*4 ipc, iflag, nobjSmooth, nTotPoints, nobjTot
      integer*4 getimodhead, indmap, getimodflags,getImodObjSize
      common /bigarr/p_new
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString,PipGetFloat
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  smoothsurf
c       
      integer numOptions
      parameter (numOptions = 8)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'objects:ObjectsToSmooth:LI:@nz:NumberOfSections:I:@'//
     &    'distance:MaximumDistance:F:@contorder:ContourOrder:I:@'//
     &    'surforder:SurfaceOrder:I:@help:usage:B:'
c       
c       defaults
c       
      minpts=4
      tolcross=4.
      closethresh=1.
      nobjdo=0
      iorder = 3
      iorder2 = 2
      nzfit = 7
      distlim = 15.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'smoothsurf',
     &    'ERROR: SMOOTHSURF - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
c       
      if (PipGetInOutFile('InputFile', 1, ' ', filin) .ne. 0)
     &    call errorexit('NO INPUT FILE SPECIFIED')
c       
      if (PipGetInOutFile('OutputFile', 2, ' ', filout) .ne. 0)
     &    call errorexit('NO OUTPUT FILE SPECIFIED')
c       
      call imodPartialMode(1)
      if(.not.readw_or_imod(filin))call errorexit('READING MODEL')
c       

      if (PipGetString('ObjectsToSmooth', filin) .eq. 0)
     &    call parselist(filin, iobjdo, nobjdo)
      if (nobjdo .gt. limflags) call errorexit(
     &    'TOO MANY OBJECTS IN LIST FOR ARRAYS')

      ierr = PipGetInteger('NumberOfSections', nzfit)
      ierr = PipGetInteger('ContourOrder', iorder2)
      ierr = PipGetInteger('SurfaceOrder', iorder)
      ierr = PipGetFloat('MaximumDistance', distlim)
      call PipDone()

      nobjTot = getImodObjSize()
      if (nzfit .lt. 1) call errorexit('NUMBER OF SECTIONS IS TOO SMALL')
      if (nzfit .gt. idzlim) call exitError(
     &    'NUMBER OF SECTIONS IS TOO LARGE FOR ARRAYS')
      if (iorder2 .lt. 0 .or. iorder2 .gt. 4) call errorexit(
     &    'CONTOUR SMOOTHING ORDER IS OUTSIDE OF ALLOWED RANGE')
      if (iorder .lt. 1 .or. iorder .gt. 5) call errorexit(
     &    'SURFACE SMOOTHING ORDER IS OUTSIDE OF ALLOWED RANGE')
      if (distlim .le. 1.) call errorexit('MAXIMUM DISTANCE IS TOO SMALL')
c       
c       unflip data if it was flipped
c       
      if (getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip) .ne. 0) call
     &    errorexit('READING MODEL HEADER INFORMATION FOR SCALING')
c       
c       set minimum separation between points to get at least minpts
c       each side of center
c       
      sepmin=distlim/minpts
c       
c       Get object flags
c       
      do i=1,limflags
        iflags(i)=0
      enddo
      ierr=getimodflags(iflags,limflags)
      if(ierr.ne.0)print *,'Error getting object types, assuming',
     &    ' all are closed contours'
c       
c       loop on model objects and decide whether to load
c       
      do imodObj = 1, nobjTot
c         
c         is IMOD object on list and object not scattered
c         
        ifonlist=0
        if(nobjdo.eq.0)then
          ifonlist=1
        else
          do ichek=1,nobjdo
            if(imodobj.eq.iobjdo(ichek))ifonlist=1
          enddo
        endif
        iflag = 0
        if (imodobj .le. limflags) iflag = mod(iflags(imodobj), 4)
        if (ifonlist .eq. 1 .and. iflag .lt. 2) then
c           
c           Mark all objects to include by requiring at leat 2 points and
c           closed contour or open coplanar contour
c           
          if (.not.getModelObjectRange(imodObj, imodObj)) then
            print *
            print *, 'ERROR: SMOOTHSURF - LOADING DATA FOR OBJECT #',imodobj
            call exit(1)
          endif
          call scale_model(0)
          if(ifflip.ne.0)then
            do i=1,n_point
              tmp=p_coord(2,i)
              p_coord(2,i)=p_coord(3,i)
              p_coord(3,i)=tmp
            enddo
          endif
          
          nobjSmooth = 0
          nTotPoints = 0
          do iobj=1,max_mod_obj
            ninobj=npt_in_obj(iobj)
            ibase=ibase_obj(iobj)
            ipnt1=object(ibase+1)
            izobj(iobj)=nint(p_coord(3,ipnt1))
c             
c             require at least 2 points
c             
            objectOK(iobj) = ninobj .gt. 2
c             
c             for open objects, make sure contour is coplanar
c             
            if (objectOK(iobj) .and. iflag .eq. 1) then
              do ipt=2,ninobj
                ipnt=object(ipt+ibase)
                if (nint(p_coord(3,ipnt)) .ne. izobj(iobj))
     &              objectOK(iobj) = .false.
              enddo
            endif
c             
c             uncross/straighten out start and end points, set Z of object
c             negative if not an OK one
c             
            if (objectOK(iobj)) then
              call uncrosscont(p_coord,3,object,ibase,ninobj,ninobj,20.,
     &            tolcross*2.)
              nobjSmooth = nobjSmooth +1
              nTotPoints = nTotPoints + ninobj
            else
              izobj(iobj)=-100000.
            endif
          enddo
          write(*,102)imodobj,nTotPoints,nobjSmooth
102       format('Doing object',i5,', ',i8,' points in',i6,
     &        ' contours being smoothed')
c           
c           make copy into new coordinates
c           
          do i=1,n_point
            p_new(1,i)=p_coord(1,i)
            p_new(2,i)=p_coord(2,i)
          enddo
c           
          do iobj=1,max_mod_obj
            ninobj=npt_in_obj(iobj)
            ibase=ibase_obj(iobj)
            if(objectOK(iobj))then
              do ipt=1,ninobj
                ipnt=object(ipt+ibase)
c                 
c                 consider this point in this object; look for the closest
c                 point in objects of the same color (i.e. contours of the
c                 same object) within the z range and the distance limit
c                 
                xx=p_coord(1,ipnt)
                yy=p_coord(2,ipnt)
                do i=-nzfit,nzfit
                  iobjbest(i)=0
                enddo
                iobjbest(0)=iobj
                iptbest(0)=ipt
                ifspan=0
                idzlo=-nzfit/2
                idzhi=idzlo+nzfit-1
                idzmin=0
                idzmax=0
                loop=1
                do while(ifspan.eq.0.and.loop.le.2)
                  do idz=idzlo,idzhi
                    if(iobjbest(idz).eq.0)then
                      dzsq=idz**2
                      iz=izobj(iobj)+idz
                      distmin=distlim**2
                      do jobj=1,max_mod_obj
                        if(objectOK(jobj) .and. izobj(jobj).eq.iz.and.
     &                      obj_color(2,jobj).eq.obj_color(2,iobj))then
                          jbase=ibase_obj(jobj)
                          do jpt=1,npt_in_obj(jobj)
                            jpnt=object(jpt+jbase)
                            dx=xx-p_coord(1,jpnt)
                            if(abs(dx).le.distlim)then
                              dy=yy-p_coord(2,jpnt)
                              if(abs(dy).le.distlim)then
                                distsq=dx**2+dy**2+dzsq
                                if(distsq.lt.distmin)then
                                  distmin = distsq
                                  iobjbest(idz) = jobj
                                  iptbest(idz) = jpt
                                endif
                              endif
                            endif
                          enddo
                        endif
                      enddo
                    endif
c                     
                    if(iobjbest(idz).ne.0)then
                      idzmin=min(idzmin,idz)
                      idzmax=max(idzmax,idz)
                    endif
                  enddo
c                   
c                   if didn't get the full range in both directions in Z,
c                   redo the search at farther Z values to find a full span
c                   of Z values that includes the point in question near its
c                   middle
c                   
                  if(loop.eq.1)then
                    if(idzmin.eq.idzlo.and.idzmax.eq.idzhi)then
                      ifspan=1
                    else
                      if(idzmax.lt.idzhi)idzlo=idzmax-(nzfit-1)
                      if(idzmin.gt.-nzfit/2)idzhi=idzmin+nzfit-1
                    endif
                  endif
                  loop=loop+1
                enddo
c                 
c                 use next and previous point to define an angle to rotate to
c                 the horizontal
c                 
                iplas=object(ibase+indmap(ipt-1,ninobj))
                ipnex=object(ibase+indmap(ipt+1,ninobj))
                dx=p_coord(1,ipnex)-p_coord(1,iplas)
                dy=p_coord(2,ipnex)-p_coord(2,iplas)
                dlen=sqrt(dx**2+dy**2)
                if(dlen.gt.1..and.idzmin.lt.0.and.idzmax.gt.0)then
                  sinth=-dy/dlen
                  costh=dx/dlen
                  mzfit=0
                  nfit=0
c                   
c                   for each Z level, start at the closest point and add
c                   points within the distance limit or until X starts to
c                   fold back toward the central point
c                   
                  do idz=idzmin,idzmax
                    if(iobjbest(idz).ne.0)then
                      mzfit=mzfit+1
                      jobj=iobjbest(idz)
                      jbase=ibase_obj(jobj)
                      xlas=-1.e10
                      jpt=iptbest(idz)
                      ninj=npt_in_obj(jobj)
                      do idir=1,-1,-2
                        iftoofar=0
                        do while(iftoofar.eq.0.and.nfit.lt.idim)
                          jpnt=object(jbase+jpt)
                          dx=p_coord(1,jpnt)-xx
                          dy=p_coord(2,jpnt)-yy
                          xrot=costh*dx-sinth*dy
                          yrot=sinth*dx+costh*dy
                          if(idir*(xrot-xlas).lt.0.)then
                            iftoofar=1
                          else
                            dist=sqrt(dx**2+dy**2+idz**2)
                            ifsave=1
                            if(dist.gt.distlim)then
c                               
c                               if go past the distance limit, add a point
c                               within the limit if is far from the last
c                               point
c                               
                              if(dist-distlas.gt.0.1*distlim)then
                                frac=(distlim-distlas)/(dist-distlas)
                                xrot=xlas+frac*(xrot-xlas)
                                yrot=ylas+frac*(yrot-ylas)
                              else
                                ifsave=0
                              endif
                              iftoofar=1
                            endif
                            if(ifsave.eq.1)then
                              nfit=nfit+1
                              xt(nfit)=xrot
                              yt(nfit)=yrot
                              zt(nfit)=idz
                              if(jpt.eq.iptbest(idz))then
                                xcen=xrot
                                ycen=yrot
                                distcen=dist
                              else
c                                 
c                                 also add points to maintain a maximum
c                                 separation between point = sepmin
c                                 
                                sep=sqrt((xrot-xlas)**2+(yrot-ylas)**2)
                                ninsert=sep/sepmin
                                do j=1,ninsert
                                  frac=j/(ninsert+1.)
                                  if(nfit.lt.idim)then
                                    nfit=nfit+1
                                    xt(nfit)=xlas+frac*(xrot-xlas)
                                    yt(nfit)=ylas+frac*(yrot-ylas)
                                    zt(nfit)=idz
                                  endif
                                enddo
                              endif
                            endif
                            xlas=xrot
                            ylas=yrot
                            distlas=dist
                            jpt=indmap(jpt+idir,ninj)
                          endif
                        enddo
                        jpt=indmap(iptbest(idz)-1,ninj)
                        xlas=xcen
                        ylas=ycen
                        distlas=distcen
                      enddo
                    endif
                  enddo
c                   
c                   figure out a valid order for the fit and do the fit
c                   
                  norder=0
                  do while(norder.lt.iorder.and.norder.lt.mzfit-1.and.
     &                (norder+1)*(norder+4).le.nfit)
                    norder=norder+1
                  enddo
                  if(norder.ge.0)then
                    nindep=norder*(norder+3)/2  !# of independent variables
                    do i=1,nfit
                      call polytermReal(xt(i),zt(i),norder,xr(1,i))
                      xr(nindep+1,i)=yt(i)
                    enddo
                    call multr(xr,nindep+1,nfit,sx,ss,ssd,d,r,xm,sd,b,b1,
     &                  c1, rsq ,fra)
                    ynew=c1
c                     
c                     back rotate the fitted point to get the new value
c                     
                    p_new(1,ipnt)=sinth*ynew+xx
                    p_new(2,ipnt)=costh*ynew+yy
                  endif
                endif
              enddo
            endif
          enddo
c           
c           now treat each individual contour
c           
          do iobj=1,max_mod_obj
            ninobj=npt_in_obj(iobj)
            if(ninobj.gt.4 .and. objectOK(iobj))then
              ibase=ibase_obj(iobj)
c               
c               smoothing the same way as above: take each point as a center
c               
              do iptcen=1,ninobj
                ipc=object(ibase+iptcen)
                xmid=p_new(1,ipc)
                ymid=p_new(2,ipc)
                if(iorder2.gt.0)then
                  iplas=object(ibase+indmap(iptcen-1,ninobj))
                  ipnex=object(ibase+indmap(iptcen+1,ninobj))
                  dx=p_new(1,ipnex)-p_new(1,iplas)
                  dy=p_new(2,ipnex)-p_new(2,iplas)
                  dlen=sqrt(dx**2+dy**2)
                  if(dlen.gt.1.)then
                    sinth=-dy/dlen
                    costh=dx/dlen
                    nfit=0
                    xlas=-1.e10
                    jpt=iptcen
c                     
c                     work from the center outward until get past limit or X
c                     folds back
c                     
                    do idir=1,-1,-2
                      iftoofar=0
                      do while(iftoofar.eq.0.and.nfit.lt.idim)
c                         
c                         rotate so tangent is horizontal
c                         
                        jpnt=object(ibase+jpt)
                        dx=p_new(1,jpnt)-xmid
                        dy=p_new(2,jpnt)-ymid
                        xrot=costh*dx-sinth*dy
                        yrot=sinth*dx+costh*dy
                        if(idir*(xrot-xlas).lt.0.)then
                          iftoofar=1
                        else
                          dist=sqrt(dx**2+dy**2)
                          ifsave=1
                          if(dist.gt.distlim)then
                            if(dist-distlas.gt.0.1*distlim)then
                              frac=(distlim-distlas)/(dist-distlas)
                              xrot=xlas+frac*(xrot-xlas)
                              yrot=ylas+frac*(yrot-ylas)
                            else
                              ifsave=0
                            endif
                            iftoofar=1
                          endif
                          if(ifsave.eq.1)then
                            nfit=nfit+1
                            xt(nfit)=xrot
                            yt(nfit)=yrot
                            if(jpt.eq.iptcen)then
                              xcen=xrot
                              ycen=yrot
                              distcen=dist
                            else
c                               
c                               add points to maintain maximum separation:
c                               this is superior to a fit to a fixed number
c                               of points 
                              sep=sqrt((xrot-xlas)**2+(yrot-ylas)**2)
                              ninsert=sep/sepmin
                              do j=1,ninsert
                                frac=j/(ninsert+1.)
                                if(nfit.lt.idim)then
                                  nfit=nfit+1
                                  xt(nfit)=xlas+frac*(xrot-xlas)
                                  yt(nfit)=ylas+frac*(yrot-ylas)
                                endif
                              enddo
                            endif
                          endif
                          xlas=xrot
                          ylas=yrot
                          distlas=dist
                          jpt=indmap(jpt+idir,ninobj)
                        endif
                      enddo
                      jpt=indmap(iptcen-1,ninobj)
                      xlas=xcen
                      ylas=ycen
                      distlas=distcen
                    enddo
c                     
c                     get order, set up and do fit, substitute fitted point
c                     
                    norder=min(iorder2,nfit-2)
                    if(norder.gt.0)then
                      do i=1,nfit
                        do j=1,norder
                          xr(j,i)=xt(i)**j
                        enddo
                        xr(norder+1,i)=yt(i)
                      enddo
                      call multr(xr,norder+1,nfit,sx,ss,ssd,d,r,xm,sd,b,b1,
     &                    bint, rsq ,fra)
c                       call polyfit(xt,yt,nfit,norder,slop,bint)
                      xmid=sinth*bint+xmid
                      ymid=costh*bint+ymid
                    endif
                  endif
                endif
                p_coord(1,ipc)=xmid
                p_coord(2,ipc)=ymid
              enddo
c               
c               eliminate close points
c               
              call elimclose(p_coord,3,object,ibase,ninobj,closethresh,3)
              
              npt_in_obj(iobj)=ninobj
c               
c               fix up points that are crossed
c               
              do ipt=1,ninobj 
                call uncrosscont(p_coord,3,object,ibase,ninobj,ipt,30.,
     &              tolcross)
              enddo
            endif
          enddo
c           
c           reflip data if it was unflipped
c           
          if(ifflip.ne.0)then
            do i=1,n_point
              tmp=p_coord(2,i)
              p_coord(2,i)=p_coord(3,i)
              p_coord(3,i)=tmp
            enddo
          endif
          call scale_model(1)
          call putModelObjects()
        endif
      enddo
      n_point = -1
      call write_wmod(filout)
      print *,'DONE - Be sure to remesh the smoothed objects with imodmesh'
      call exit(0)
      end

c       
      real*4 function goodangle(thin)
      implicit none
      real*4 thin, theta 
      theta=thin
      if(theta.lt.-180)theta=theta+360.
      if(theta.ge.180.)theta=theta-360.
      goodangle=theta
      return
      end



      subroutine uncrosscont(p_coord,nxyz,object,ibase,ninobj,ip4,ang,
     &    tol)
      implicit none
      integer*4 object(*), nxyz, ibase, ninobj, ip4
      real*4 p_coord(nxyz,*), ang, tol
      integer*4 ip3, ip1, ip2, ipnt1, ipnt2, ipnt3, ipnt4, itry
      real*4 x1, y1, x2, y2, x3, y3, x4, y4, tmin1, tmin2, distsq1, distsq2
      real*4 tstrt, tend, tcon, halfdiff, tmid, halfcrit, fraclimst
      real*4 fraclimnd, fracst, fracnd, x1t, x4t, y1t, y4t
      integer*4 indmap
      real*4 atan2d, goodangle
c       
      if(ninobj.le.3)return
      ip3=indmap(ip4-1,ninobj)
      ip1=indmap(ip4+1,ninobj)
      ip2=indmap(ip4+2,ninobj)
      ipnt1=object(ibase+ip1)
      ipnt2=object(ibase+ip2)
      ipnt3=object(ibase+ip3)
      ipnt4=object(ibase+ip4)
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
        tstrt=atan2d(y2-y1,x2-x1)
        tend=atan2d(y4-y3,x4-x3)
        tcon=atan2d(y1-y4,x1-x4)
        halfdiff=0.5*goodangle(tstrt-tend)
        tmid=goodangle(tend+halfdiff)
        halfcrit=abs(halfdiff)+ang
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
            tcon=atan2d(y1t-y4t,x1t-x4t)
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




      subroutine point_to_line(x0,y0,x1,y1,x2,y2,tmin,distsq)
      implicit none
      real*4 x0,y0,x1,y1,x2,y2,tmin,distsq
      tmin=((x0-x1)*(x2-x1)+(y0-y1)*(y2-y1))/((x2-x1)**2+(y2-y1)**2)
      tmin=max(0.,min(1.,tmin))
      distsq=(x1+tmin*(x2-x1)-x0)**2+(y1+tmin*(y2-y1)-y0)**2
      return
      end



      subroutine elimclose(p_new,nxyz,object,ibase,ninobj,closethresh,
     &    minpts)
      implicit none
      integer*4 object(*), nxyz, ibase, ninobj, minpts
      real*4 p_new(nxyz,*), closethresh
      real*4 closesq
      integer*4 iseg, idel, ip3, ipt3, ipseg, ip1, ip4, ipt1, ipt4, i
      integer*4 indmap
c       
      closesq=closethresh**2
      iseg=1
      idel=0
      do while(iseg.le.ninobj.and.ninobj.gt.minpts)
        ip3=indmap(iseg+1,ninobj)
        ipt3=object(ibase+ip3)
        ipseg=object(ibase+iseg)
        if((p_new(1,ipt3)-p_new(1,ipseg))**2+
     &      (p_new(2,ipt3)-p_new(2,ipseg))**2 .lt. closesq)then
c           print *,'eliminating segment',iseg,' of',ninobj
          ip1=indmap(iseg-1,ninobj)
          ip4=indmap(iseg+2,ninobj)
          ipt1=object(ibase+ip1)
          ipt4=object(ibase+ip4)
          idel=iseg
          if((p_new(1,ipt1)-p_new(1,ipseg))**2+
     &        (p_new(2,ipt1)-p_new(2,ipseg))**2 .gt.
     &        (p_new(1,ipt3)-p_new(1,ipt4))**2+
     &        (p_new(2,ipt3)-p_new(2,ipt4))**2)idel=ip3
          do i=ibase+idel+1,ibase+ninobj
            object(i-1)=object(i)
          enddo
          ninobj=ninobj-1
        else
          iseg=iseg+1
        endif
      enddo
      return
      end

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: SMOOTHSURF - ',message
      call exit(1)
      end

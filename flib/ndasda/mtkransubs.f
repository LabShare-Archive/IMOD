      subroutine shiftsurface(isurf,shift,idir,vshifted,ifzerod,zscal)
      real*4 shift(3)
      logical*1 vshifted(*)
      include 'mtk.inc'
      include 'model.inc'
      xshft=idir*shift(1)
      yshft=idir*shift(2)
      zshft=idir*shift(3)
c       
c       loop through contours, adjusting min/max's and all points
c       
      do icont=istrcont(isurf),istrcont(isurf)+ncontinsurf(isurf)-1
        iobj=listcont(icont)
        ibase=ibase_obj(iobj)
        contxmin(icont)=contxmin(icont)+xshft
        contxmax(icont)=contxmax(icont)+xshft
        contymin(icont)=contymin(icont)+yshft
        contymax(icont)=contymax(icont)+yshft
        contzval(icont)=contzval(icont)+zshft
        do ipt=1,npt_in_obj(iobj)
          ipnt=object(ibase+ipt)
          p_coord(1,ipnt)=p_coord(1,ipnt)+xshft
          p_coord(2,ipnt)=p_coord(2,ipnt)+yshft
          p_coord(3,ipnt)=zscal*nint((p_coord(3,ipnt)+zshft)/zscal)
        enddo
      enddo
      call shiftsurfmesh(isurf,shift,idir,vshifted,ifzerod,zscal)
      return
      end


      subroutine shiftsurfmesh(isurf,shift,idir,vshifted,ifzerod,zscal)
      real*4 shift(3)
      logical*1 vshifted(*)
      include 'mtk.inc'
c       
c       set up array to keep track of shifted vertices
c       
      if(ifzerod.eq.0)then
        do i=1,nverts
          vshifted(i)=.false.
        enddo
      endif
      ifzerod=1
      xshft=idir*shift(1)
      yshft=idir*shift(2)
      zshft=idir*shift(3)
      surfxmin(isurf)=surfxmin(isurf)+xshft
      surfxmax(isurf)=surfxmax(isurf)+xshft
      surfymin(isurf)=surfymin(isurf)+yshft
      surfymax(isurf)=surfymax(isurf)+yshft
      surfzmin(isurf)=surfzmin(isurf)+zshft
      surfzmax(isurf)=surfzmax(isurf)+zshft
c       
c       loop on polygons in surface
c       
      do list=istrsurf(isurf),
     &    istrsurf(isurf)+ninsurf(isurf)-1
        ipoly=listsurf(list)
        polyxmin(ipoly)=polyxmin(ipoly)+xshft
        polyxmax(ipoly)=polyxmax(ipoly)+xshft
        polyymin(ipoly)=polyymin(ipoly)+yshft
        polyymax(ipoly)=polyymax(ipoly)+yshft
        polyzmin(ipoly)=polyzmin(ipoly)+zshft
        polyzmax(ipoly)=polyzmax(ipoly)+zshft
c         
c         loop on triangles in polygon
c         
        do itri=istrpoly(ipoly),
     &      istrpoly(ipoly)+ninpoly(ipoly)-1
          trixmin(itri)=trixmin(itri)+xshft
          trixmax(itri)=trixmax(itri)+xshft
          triymin(itri)=triymin(itri)+yshft
          triymax(itri)=triymax(itri)+yshft
          trizmin(itri)=trizmin(itri)+zshft
          trizmax(itri)=trizmax(itri)+zshft
c           
c           get rotated shift to add to rotated coordinates
c           
          tmp=xshft*cgam(itri)-yshft*sgam(itri)
          xrshft=tmp*cbet(itri)-zshft*sbet(itri)
          yrshft=xshft*sgam(itri)+yshft*cgam(itri)
          zrshft=tmp*sbet(itri)+zshft*cbet(itri)
          zrot(itri)=zrot(itri)+zrshft
          do i=1,3
            xyrot(i,1,itri)=xyrot(i,1,itri)+xrshft
            xyrot(i,2,itri)=xyrot(i,2,itri)+yrshft
c             
c             shift vertices if not already done
c             
            ind=indvert(i,itri)
            if(.not.vshifted(ind))then
              verts(1,ind)=verts(1,ind)+xshft
              verts(2,ind)=verts(2,ind)+yshft
              verts(3,ind)=zscal*nint((verts(3,ind)+zshft)/zscal)
              vshifted(ind)=.true.
            endif
          enddo
        enddo
      enddo
      return
      end

      subroutine shiftline(iobjref,indref,limref,delx,dely,delz,
     &    xmt,ymt,zmt,xmin,xmax,ymin,ymax,zmin,zmax,
     &    glbxmin,glbxmax,glbymin,glbymax,glbzmin,glbzmax,zscal)
      real*4 xmt(*),ymt(*),zmt(*), xmin(*),xmax(*),ymin(*),ymax(*)
      real*4 zmin(*),zmax(*), glbxmin(*),glbxmax(*),glbymin(*)
      real*4  glbymax(*),glbzmin(*),glbzmax(*)
      glbxmin(iobjref)=glbxmin(iobjref)+delx
      glbymin(iobjref)=glbymin(iobjref)+dely
      glbzmin(iobjref)=glbzmin(iobjref)+delz
      glbxmax(iobjref)=glbxmax(iobjref)+delx
      glbymax(iobjref)=glbymax(iobjref)+dely
      glbzmax(iobjref)=glbzmax(iobjref)+delz
      do iref=indref,limref
        xmin(iref)=xmin(iref)+delx
        ymin(iref)=ymin(iref)+dely
        zmin(iref)=zmin(iref)+delz
        xmax(iref)=xmax(iref)+delx
        ymax(iref)=ymax(iref)+dely
        zmax(iref)=zmax(iref)+delz
      enddo
      do iref=indref,limref+1
        xmt(iref)=xmt(iref)+delx
        ymt(iref)=ymt(iref)+dely
        zmt(iref)=zscal*nint((zmt(iref)+delz)/zscal)
      enddo
      return
      end

      subroutine shiftpoint(iobjref,iref,delx,dely,delz,
     &    xmt,ymt,zmt,xmin,xmax,ymin,ymax,zmin,zmax,
     &    glbxmin,glbxmax,glbymin,glbymax,glbzmin,glbzmax,zscal)
      real*4 xmt(*),ymt(*),zmt(*), xmin(*),xmax(*),ymin(*),ymax(*)
      real*4 zmin(*),zmax(*), glbxmin(*),glbxmax(*),glbymin(*)
      real*4  glbymax(*),glbzmin(*),glbzmax(*)
      xmin(iref)=xmin(iref)+delx
      ymin(iref)=ymin(iref)+dely
      zmin(iref)=zmin(iref)+delz
      xmax(iref)=xmax(iref)+delx
      ymax(iref)=ymax(iref)+dely
      zmax(iref)=zmax(iref)+delz
      xmt(iref)=xmt(iref)+delx
      ymt(iref)=ymt(iref)+dely
      zmt(iref)=zscal*nint((zmt(iref)+delz)/zscal)
      glbxmin(iobjref)=min(glbxmin(iobjref),xmin(iref))
      glbymin(iobjref)=min(glbymin(iobjref),ymin(iref))
      glbzmin(iobjref)=min(glbzmin(iobjref),zmin(iref))
      glbxmax(iobjref)=max(glbxmax(iobjref),xmax(iref))
      glbymax(iobjref)=max(glbymax(iobjref),ymax(iref))
      glbzmax(iobjref)=max(glbzmax(iobjref),zmax(iref))
      return
      end

      subroutine unshift_object(itypshift,iobjflag,xmt,ymt,
     &    zmt,icolor, nmt,indstrt,npntobj,xyscal,zscal,
     &    zgapst,zgapnd,ngaps)
      real*4 xmt(*),ymt(*),zmt(*),zgapst,zgapnd
      integer*4 icolor(*),indstrt(*),npntobj(*)
      include 'model.inc'
      include 'mtk.inc'
      parameter (limind=limverts*6)
      integer*4 modind(limind)
c       
      ifon=0
      do imo=1,nobjshifted
        if(iobjshift(imo).eq.itypshift)ifon=imo
      enddo
      if(ifon.eq.0)then
        print *,'This object is not currently shifted'
        return
      endif
      ishift=istrshift(ifon)
      if(iobjflag.eq.1)then
c         
c         unshift lines
c         
        do iobj=1,nmt
          if(icolor(iobj).eq.itypshift)then
            if(shifted(ishift))then
              delx=-shifts(1,ishift)
              dely=-shifts(2,ishift)
              delz=-shifts(3,ishift)
              do ind=indstrt(iobj),indstrt(iobj)+npntobj(iobj)-1
                xmt(ind)=xmt(ind)+delx
                ymt(ind)=ymt(ind)+dely
                zmt(ind)=zscal*nint((zmt(ind)+delz)/zscal)
              enddo
              shifts(1,ishift)=0.
              shifts(2,ishift)=0.
              shifts(3,ishift)=0.
            endif
            ishift=ishift+1
          endif
        enddo
      elseif(iobjflag.eq.2)then
c         
c         unshift points
c         
        do iobj=1,nmt
          if(icolor(iobj).eq.itypshift)then
            do ind=indstrt(iobj),indstrt(iobj)+npntobj(iobj)-1
              if(shifted(ishift))then
                delx=-shifts(1,ishift)
                dely=-shifts(2,ishift)
                delz=-shifts(3,ishift)
                xmt(ind)=xmt(ind)+delx
                ymt(ind)=ymt(ind)+dely
                zmt(ind)=zscal*nint((zmt(ind)+delz)/zscal)
                shifts(1,ishift)=0.
                shifts(2,ishift)=0.
                shifts(3,ishift)=0.
              endif
              ishift=ishift+1
            enddo
          endif
        enddo
      else
c         
c         unshift meshes
c         
        imesh=0
        do i=1,nmeshloaded
          if(iobjmesh(i).eq.itypshift)imesh=i
        enddo
        if(imesh.eq.0)then
          nmeshloaded=0
          nverts=0
          ntriang=0
          npoly=0
          nsurf=0
          ncont=0
          call process_mesh(itypshift,xyscal,zscal,modind,
     &        zgapst,zgapnd,ngaps,iferr,0)
          if(iferr.ne.0)return
          imesh=1
        endif
        ifzerod=0
        do isurf=iobjsurf(imesh),iobjsurf(imesh)+nsurfobj(imesh)-1
          if(shifted(ishift))then
            call shiftsurface(isurf,shifts(1,ishift),-1,modind,
     &          ifzerod,zscal)
            shifts(1,ishift)=0.
            shifts(2,ishift)=0.
            shifts(3,ishift)=0.
          endif
          ishift=ishift+1
        enddo

      endif
      return
      end

      subroutine check_line_surface(isurf,indref,limref, xmin,xmax,
     &    ymin,ymax,zmin,zmax,xmt,ymt,zmt, distmin,distabs,zscal)
      real*4 xmt(*),ymt(*),zmt(*), xmin(*),xmax(*),ymin(*),ymax(*)
      real*4 zmin(*),zmax(*)
      include 'mtk.inc'
      logical inside_wimpobj,crosses_wimpobj
c       
c       loop on line segments, check against bounding box of surface
c       
      zlim=.01*zscal
      do iref=indref,limref
        refxmin=xmin(iref)
        refymin=ymin(iref)
        refzmin=zmin(iref)
        refxmax=xmax(iref)
        refymax=ymax(iref)
        refzmax=zmax(iref)
        if(refxmin-surfxmax(isurf).lt.distmin.and.
     &      surfxmin(isurf)-refxmax.lt.distmin.and.
     &      refymin-surfymax(isurf).lt.distmin.and.
     &      surfymin(isurf)-refymax.lt.distmin.and.
     &      refzmin-surfzmax(isurf).lt.distmin.and.
     &      surfzmin(isurf)-refzmax.lt.distmin)then
c           
c           better search now for contours that overlap
c           
          do icont=istrcont(isurf),istrcont(isurf)+ncontinsurf(isurf)-1
            if(refxmin.le.contxmax(icont).and.
     &          contxmin(icont).le.refxmax.and.
     &          refymin.le.contymax(icont).and.
     &          contymin(icont).le.refymax.and.
     &          refzmin.le.contzval(icont)+zlim.and.
     &          contzval(icont)-zlim.le.refzmax)then
              iobj=listcont(icont)
              if(refzmax-refzmin.gt.zlim)then
c                 
c                 if line passes through the zplane, get point on the
c                 contour's plane and check if inside
c                 
                frac=(contzval(icont)-zmt(iref))/(zmt(iref+1)-zmt(iref))
                xintrp=xmt(iref)+frac*(xmt(iref+1)-xmt(iref))
                yintrp=ymt(iref)+frac*(ymt(iref+1)-ymt(iref))
                if(inside_wimpobj(iobj,xintrp,yintrp))then
                  distmin=-1.
                  return
                endif
              else
c                 
c                 otherwise, need to check for 2-d intersection between the
c                 line and each of the segments of the contour
c                 
                if(crosses_wimpobj(iobj,xmt(iref),ymt(iref),
     &              xmt(iref+1),ymt(iref+1)))then
                  distmin=-1.
                  return
                endif
              endif
            endif
          enddo

          do list=istrsurf(isurf),
     &        istrsurf(isurf)+ninsurf(isurf)-1
            ipoly=listsurf(list)
c             
c             check against polygon global limits
c             
            if(refxmin-polyxmax(ipoly).lt.distmin.and.
     &          polyxmin(ipoly)-refxmax.lt.distmin.and.
     &          refymin-polyymax(ipoly).lt.distmin.and.
     &          polyymin(ipoly)-refymax.lt.distmin.and.
     &          refzmin-polyzmax(ipoly).lt.distmin.and.
     &          polyzmin(ipoly)-refzmax.lt.distmin)then
c               
c               scan through triangles
c               
              do itri=istrpoly(ipoly),istrpoly(ipoly)+
     &            ninpoly(ipoly)-1
                if(refxmin-trixmax(itri).lt.distmin.and.
     &              trixmin(itri)-refxmax.lt.distmin.and.
     &              refymin-triymax(itri).lt.distmin.and.
     &              triymin(itri)-refymax.lt.distmin.and.
     &              refzmin-trizmax(itri).lt.distmin.and.
     &              trizmin(itri)-refzmax.lt.distmin)then
c                   
                  call segment_to_triangle(xmt(iref),
     &                ymt(iref),zmt(iref),xmt(iref+1),
     &                ymt(iref+1),zmt(iref+1),
     &                itri,tref,xrot,yrot,dist)
                  if(dist.lt.distmin)then
                    distmin=dist
                    if(dist.lt.distabs)return
                  endif
                endif
              enddo
            endif
          enddo
        endif
      enddo
      return
      end

c       
      subroutine check_point_surface(isurf,xmt, ymt,zmt, xmin,xmax,
     &    ymin, ymax,zmin,zmax, sizer,distmin,distabs,zscal)
      include 'mtk.inc'
      logical inside_wimpobj
c       
c       loop on contours, see if point is inside
c       
      zlim=.01*zscal
      do icont=istrcont(isurf),istrcont(isurf)+ncontinsurf(isurf)-1
        if(contxmin(icont).le.xmt.and.
     &      contxmax(icont).ge.xmt.and.
     &      contymin(icont).le.ymt.and.
     &      contymax(icont).ge.ymt.and.
     &      abs(contzval(icont)-zmt).le.zlim)then
          iobj=listcont(icont)
          if(inside_wimpobj(iobj,xmt,ymt))then
            distmin=-1.
            return
          endif
        endif
      enddo
c       
c       find minimum distance of point from surface now
c       
      do list=istrsurf(isurf),
     &    istrsurf(isurf)+ninsurf(isurf)-1
        ipoly=listsurf(list)
c         
c         check against polygon global limits
c         
        if(xmin-polyxmax(ipoly).lt.distmin.and.
     &      polyxmin(ipoly)-xmax.lt.distmin.and.
     &      ymin-polyymax(ipoly).lt.distmin.and.
     &      polyymin(ipoly)-ymax.lt.distmin.and.
     &      zmin-polyzmax(ipoly).lt.distmin.and.
     &      polyzmin(ipoly)-zmax.lt.distmin)then
c           
c           scan through triangles
c           
          do itri=istrpoly(ipoly),istrpoly(ipoly)+
     &        ninpoly(ipoly)-1
            if(xmin-trixmax(itri).lt.distmin.and.
     &          trixmin(itri)-xmax.lt.distmin.and.
     &          ymin-triymax(itri).lt.distmin.and.
     &          triymin(itri)-ymax.lt.distmin.and.
     &          zmin-trizmax(itri).lt.distmin.and.
     &          trizmin(itri)-zmax.lt.distmin)then
c               
c               
              call point_to_triangle(xmt,ymt,zmt,itri,xrot,yrot,dist)
              dist=dist-sizer
              if(dist.lt.distmin)then
                distmin=dist
                if(dist.lt.distabs)return
              endif
            endif
          enddo
        endif
      enddo
      return
      end


      subroutine check_two_meshes(isurf,jsurf,distmin,distabs,zscal)
      include 'mtk.inc'
      include 'model.inc'
      logical crosses_wimpobj,inside_wimpobj
      zlim=.1*zscal
c       
c       first check contours against each other to find overlap
c       
      refxmin=surfxmin(jsurf)
      refymin=surfymin(jsurf)
      refzmin=surfzmin(jsurf)
      refxmax=surfxmax(jsurf)
      refymax=surfymax(jsurf)
      refzmax=surfzmax(jsurf)
      do icont=istrcont(isurf),istrcont(isurf)+ncontinsurf(isurf)-1
        if(refxmin.le.contxmax(icont).and.
     &      contxmin(icont).le.refxmax.and.
     &      refymin.le.contymax(icont).and.
     &      contymin(icont).le.refymax.and.
     &      refzmin.le.contzval(icont)+zlim.and.
     &      contzval(icont)-zlim.le.refzmax)then
          cixmin=contxmin(icont)
          ciymin=contymin(icont)
          cixmax=contxmax(icont)
          ciymax=contymax(icont)
          do jcont=istrcont(jsurf),istrcont(jsurf)+ncontinsurf(jsurf)-1
            if(abs(contzval(icont)-contzval(jcont)).lt.zlim.and.
     &          contxmin(jcont).le.cixmax.and.
     &          cixmin.le.contxmax(jcont).and.
     &          contymin(jcont).le.ciymax.and.
     &          ciymin.le.contymax(jcont))then
              iobj=listcont(icont)
              jobj=listcont(jcont)
c               
c               check each segment of j contour to see if crosses i
c               
              jbase=ibase_obj(jobj)
              jnpt=npt_in_obj(jobj)
              ipt=object(jbase+jnpt)
              x1=p_coord(1,ipt)
              y1=p_coord(2,ipt)
              jpt=object(ibase_obj(iobj)+1)
              x2=p_coord(1,jpt)
              y2=p_coord(2,jpt)
c               
c               but first check if one point of either is inside the other
c               this handles case of completely enclosed contours
c               
              if(inside_wimpobj(iobj,x1,y1).or.
     &            inside_wimpobj(jobj,x2,y2))then
                distmin=-1.
                return
              endif
c               
              do j=1,jnpt
                jpt=object(jbase+j)
                x2=p_coord(1,jpt)
                y2=p_coord(2,jpt)
                if(min(x1,x2).le.cixmax.and.cixmin.le.max(x1,x2).and.
     &              min(y1,y2).le.ciymax.and.ciymin.le.max(y1,y2))then
                  if(crosses_wimpobj(iobj,x1,y1,x2,y2))then
                    distmin=-1.
                    return
                  endif
                endif
                x1=x2
                y1=y2
              enddo
            endif
          enddo
        endif
      enddo
c       
c       now find actual distance between surfaces
c       
c       
c       loop on polygons of neighbor surface
c       
      refxmin=surfxmin(isurf)
      refymin=surfymin(isurf)
      refzmin=surfzmin(isurf)
      refxmax=surfxmax(isurf)
      refymax=surfymax(isurf)
      refzmax=surfzmax(isurf)
      do mist=istrsurf(jsurf),
     &    istrsurf(jsurf)+ninsurf(jsurf)-1
        jpoly=listsurf(mist)
        if(refxmin-polyxmax(jpoly).lt.distmin.and.
     &      polyxmin(jpoly)-refxmax.lt.distmin.and.
     &      refymin-polyymax(jpoly).lt.distmin.and.
     &      polyymin(jpoly)-refymax.lt.distmin.and.
     &      refzmin-polyzmax(jpoly).lt.distmin.and.
     &      polyzmin(jpoly)-refzmax.lt.distmin)then
c           
c           loop on triangles of neighbor
c           
          do jtri=istrpoly(jpoly),
     &        istrpoly(jpoly)+ninpoly(jpoly)-1
            xminnay=trixmin(jtri)
            yminnay=triymin(jtri)
            zminnay=trizmin(jtri)
            xmaxnay=trixmax(jtri)
            ymaxnay=triymax(jtri)
            zmaxnay=trizmax(jtri)
            if(refxmin-xmaxnay.lt.distmin.and.
     &          xminnay-refxmax.lt.distmin.and.
     &          refymin-ymaxnay.lt.distmin.and.
     &          yminnay-refymax.lt.distmin.and.
     &          refzmin-zmaxnay.lt.distmin.and.
     &          zminnay-refzmax.lt.distmin)then
c               
c               now loop on polygons of reference surface
c               
              do list=istrsurf(isurf),
     &            istrsurf(isurf)+ninsurf(isurf)-1
                ipoly=listsurf(list)
                if(xminnay-polyxmax(ipoly).lt.distmin.and.
     &              polyxmin(ipoly)-xmaxnay.lt.distmin.and.
     &              yminnay-polyymax(ipoly).lt.distmin.and.
     &              polyymin(ipoly)-ymaxnay.lt.distmin.and.
     &              zminnay-polyzmax(ipoly).lt.distmin.and.
     &              polyzmin(ipoly)-zmaxnay.lt.distmin)then
c                   
c                   loop on triangles in reference polygon
c                   
                  do itri=istrpoly(ipoly),
     &                istrpoly(ipoly)+ninpoly(ipoly)-1
                    if(xminnay-trixmax(itri).lt.distmin.and.
     &                  trixmin(itri)-xmaxnay.lt.distmin.and.
     &                  yminnay-triymax(itri).lt.distmin.and.
     &                  triymin(itri)-ymaxnay.lt.distmin.and.
     &                  zminnay-trizmax(itri).lt.distmin.and.
     &                  trizmin(itri)-zmaxnay.lt.distmin)then
                      call triangle_to_triangle(itri,jtri,
     &                    xr1,yr1,zr1,xr2,yr2, itrir, dist)
                      if(dist.lt.distmin)then
                        distmin=dist
                        if(dist.lt.distabs)return
                      endif
                    endif
                  enddo
                endif
              enddo
            endif
          enddo
        endif
      enddo
      return
      end



      LOGICAL FUNCTION INSIDE_wimpobj (iobj,X,Y)
      include 'model.inc'
C       
      INSIDE_wimpobj=.FALSE.
      np=npt_in_obj(iobj)
      IF (NP.LE.2) RETURN
      ibase=ibase_obj(iobj)
      ipt=object(ibase+np)
      X0=X-p_coord(1,ipt)
      Y0=Y-p_coord(2,ipt)
      IF (X0.EQ.0..AND.Y0.EQ.0.) then
        INSIDE_wimpobj=.TRUE.
        RETURN
      endif
      SUMA=0.
      DO J=1,NP
        ipt=object(ibase+j)
        XA=X-p_coord(1,ipt)
        YA=Y-p_coord(2,ipt)
        IF (XA.EQ.0..AND.YA.EQ.0.) then
          INSIDE_wimpobj=.TRUE.
          RETURN
        endif
        SINA=(YA*X0-XA*Y0)
        COSA=(XA*X0+YA*Y0)
        ANGLE=ATAN2(SINA,COSA)
        SUMA=SUMA+ANGLE
        X0=XA
        Y0=YA
      enddo
      IF (ABS(SUMA).GE.4.0) INSIDE_wimpobj=.TRUE.
      RETURN
      END


      LOGICAL FUNCTION crosses_wimpobj (iobj,X1s,Y1s,x1e,y1e)
      include 'model.inc'
C       
      crosses_wimpobj=.FALSE.
      np=npt_in_obj(iobj)
      IF (NP.LE.1) RETURN
      ibase=ibase_obj(iobj)
      ipt=object(ibase+np)
      X2s=p_coord(1,ipt)
      Y2s=p_coord(2,ipt)
      dx1=x1e-x1s
      dy1=y1e-y1s
      crosses_wimpobj=.true.
      DO J=1,NP
        ipt=object(ibase+j)
        X2e=p_coord(1,ipt)
        Y2e=p_coord(2,ipt)
        dx2=x2s-x2e
        dy2=y2s-y2e
        dxs=x2s-x1s
        dys=y2s-y1s
        den=dx1*dy2-dy1*dx2
        tnum=dxs*dy2-dys*dx2
        unum=dx1*dys-dy1*dxs
        if(den.lt.0)then
          if(tnum.le.0..and.unum.le.0..and.tnum.ge.den.and.unum.ge.den)
     &        return
        else
          if(tnum.ge.0..and.unum.ge.0..and.tnum.le.den.and.unum.le.den)
     &        return
        endif
        X2s=X2e
        Y2s=Y2e
      enddo
      crosses_wimpobj=.false.
      RETURN
      END


      subroutine getranshift(ntrials,ntrialcycle,cyclefac, ranmin,
     &    ranmax,ranzrel,curshft,delxyz, tstxmin,tstxmax,tstymin,
     &    tstymax,tstzmin,tstzmax, shiftxmin,shiftxmax,shiftymin,
     &    shiftymax,shiftzmin, shiftzmax,zscal,iseed,nbound,listbound,
     &    zbound, xmt,ymt, zmt,indstr,indend)
      real*4 curshft(3),delxyz(3),xmt(*),ymt(*),zmt(*),zbound(*)
      integer*4 listbound(*)
      integer*2 ntrials
      include 'model.inc'
      include 'mtk.inc'
      logical badshift,outside_boundary
c       
      nattempt=0
      nbcheck=0
      ntrials=ntrials+1
      nchange=ntrials/ntrialcycle
      changefac=cyclefac**nchange
      ranmaxtmp=ranmax*changefac
      ranmintmp=ranmin*changefac
      ranztmp=ranmaxtmp*ranzrel
      ranzmin=ranmintmp*ranzrel
      badshift=.true.
      do while(badshift)
        x0=ran(iseed)+ran(iseed)+ran(iseed)+ran(iseed)
        shiftx=ranmaxtmp*(2.*ran(iseed)-1.)
        x0=ran(iseed)+ran(iseed)+ran(iseed)+ran(iseed)
        shifty=ranmaxtmp*(2.*ran(iseed)-1.)
        x0=ran(iseed)+ran(iseed)+ran(iseed)+ran(iseed)
        shiftz=zscal*nint(ranztmp*(2.*ran(iseed)-1.)/zscal)
        distshft=shiftx**2+shifty**2
        delx=shiftx-curshft(1)
        dely=shifty-curshft(2)
        delz=shiftz-curshft(3)
c         
c         if shift is out of range or moves the item outside the
c         bounding box, repeat without counting a trial
c         
        badshift = distshft.lt.ranmintmp**2.or.
     &      distshft.gt.ranmaxtmp**2.or.
     &      abs(shiftz).lt.ranzmin.or.abs(shiftz).gt.ranztmp.or.
     &      tstxmin+delx.lt.shiftxmin.or.
     &      tstxmax+delx.gt.shiftxmax.or.
     &      tstymin+dely.lt.shiftymin.or.
     &      tstymax+dely.gt.shiftymax.or.
     &      tstzmin+delz.lt.shiftzmin.or.
     &      tstzmax+delz.gt.shiftzmax
        nattempt=nattempt+1
        if(nattempt.gt.10000)then
          print *,'too many trials with bad shift'
          delxyz(1)=0.
          delxyz(2)=0.
          delxyz(3)=0.
          return
        endif
        if(.not.badshift.and.nbound.gt.0)then
          nbcheck=nbcheck+1
          badshift=outside_boundary(nbound,listbound,
     &        zbound, xmt,ymt, zmt,indstr,indend,delx,dely,delz)

          if(badshift.and.nbcheck.gt.500)then
            print *,'too many shifts outside boundaries'
            delxyz(1)=0.
            delxyz(2)=0.
            delxyz(3)=0.
            return
          endif
        endif
      enddo
c       
      curshft(1)=shiftx
      curshft(2)=shifty
      curshft(3)=shiftz
      delxyz(1)=delx
      delxyz(2)=dely
      delxyz(3)=delz
      return
      end


c       OUTSIDE_BOUNDARY tests whether a point, line, or surface, shifted
c       by delx,dely,delz, crosses or is outside the boundary contours

      logical function outside_boundary(nbound,listbound,
     &    zbound, xmt,ymt, zmt,indstr,indend,delx,dely,delz)
      real*4 xmt(*),ymt(*),zmt(*),zbound(*)
      integer*4 listbound(*)
      include 'model.inc'
      include 'mtk.inc'
      logical badshift,crosses_wimpobj,inside_wimpobj
      badshift=.false.

      if(indend.gt.0)then
c         
c         check single point or line of points for whether they are
c         all inside the nearest boundary in z
c         
        ind=indstr
        do while(.not.badshift.and.ind.le.indend)
          xpt=xmt(ind)+delx
          ypt=ymt(ind)+dely
          zpt=zmt(ind)+delz
          icont=nearest_boundary(zpt,zbound,nbound)
          badshift=.not.inside_wimpobj(listbound(icont),xpt,ypt)
          ind=ind+1
        enddo
      else
c         
c         for mesh, loop on contours of surface, checking against
c         nearest boundary contour in z
c         
        ind=istrcont(indstr)
        do while(.not.badshift.and.ind.lt.
     &      istrcont(indstr)+ncontinsurf(indstr))
          jobj=listcont(ind)
          zpt=contzval(ind)+delz
          icont=nearest_boundary(zpt,zbound,nbound)
          iobj=listbound(icont)
          jbase=ibase_obj(jobj)
          jnpt=npt_in_obj(jobj)
          ipt=object(jbase+jnpt)
          x1=p_coord(1,ipt)+delx
          y1=p_coord(2,ipt)+dely
c           
c           first check that at least one point is inside the boundary
c           then check each segment for crossing the boundary
c           
          badshift=.not.inside_wimpobj(iobj,x1,y1)
c           
          j=1
          do while(.not.badshift.and.j.le.jnpt)
            jpt=object(jbase+j)
            x2=p_coord(1,jpt)+delx
            y2=p_coord(2,jpt)+dely
            badshift=crosses_wimpobj(iobj,x1,y1,x2,y2)
            x1=x2
            y1=y2
            j=j+1
          enddo
c           
          ind=ind+1
        enddo
      endif
      outside_boundary=badshift
      return
      end


      subroutine get_random_boundary(iobjbound,listbound,zbound,
     &    nbound,limbound)
      real*4 zbound(*)
      integer*4 listbound(*)
      include 'model.inc'
      nbound=0
      if(iobjbound.le.0)return
      do iobj=1,max_mod_obj
        if(256-obj_color(2,iobj).eq.iobjbound.and.npt_in_obj(iobj).gt.2)then
          if(nbound.eq.limbound)then
            print *,'WARNING: NOT ALL BOUNDARY CONTOURS WERE ',
     &          'LOADED - TOO MANY FOR ARRAYS'
            return
          endif
          nbound=nbound+1
          listbound(nbound)=iobj
          zbound(nbound)=p_coord(3,object(ibase_obj(iobj)+1))
        endif
      enddo
      return
      end

      function nearest_boundary(z,zbound,nbound)
      real*4 zbound(*)
      delzmin=1.e20
      do i=1,nbound
        if(abs(z-zbound(i)).lt.delzmin)then
          delzmin=abs(z-zbound(i))
          nearest_boundary=i
        endif
      enddo
      return
      end

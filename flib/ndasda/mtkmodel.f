c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.8  2007/01/06 23:56:32  mast
c       Added ability to preserve and transfer point sizes when writing model
c       
c       Revision 3.7  2006/10/30 17:45:14  mast
c       Fixed moving contours to new object when only one meshed object and
c       fixed flag setting to get closed contour objects
c       
c       Revision 3.6  2006/05/12 14:37:32  mast
c       Added moving contours in surfaces within window to new objects
c       
c       Revision 3.5  2004/10/08 03:27:08  mast
c       Added scaling to undo image file pixel size
c       
c       Revision 3.4  2003/10/26 05:33:27  mast
c       change command files to use unit 4 instead reopening 5
c       
c       Revision 3.3  2003/08/08 23:55:16  mast
c       *** empty log message ***
c       
c       Revision 3.2  2003/08/08 17:48:13  mast
c       Eliminated STOP statements, provided for model file to be skipped,
c       fixed Z limiting code to not assume contours in order by Z.  Fixed
c       object-saving and changing logic so that user no longer specifies
c       objects and it knows what objects the data are being saved in and
c       sets properties correctly.
c       
c       
c       READ_MODEL reads a WIMP model.
c       if MODELFILE is blank, it requests a model file name; otherwise it
c       attempts to open the file specified by that name.
c       If there is an error, it loops back on the request for a file name.
c       If a new file name is needed, it then asks for a file of tilt info,
c       then requests scaling parameters
c       for this file, and returns them in XYSCAL and ZSCAL.
c       The file names are returned in MODELFILE and TILTFILE.
c       
c       
      subroutine read_model(modelfile,tiltfile,xyscal,zscal,xofs,yofs,
     &    zofs,ifflip, iobjflag, limflag,zgapst,zgapnd,ngaps)
      include 'model.inc'
      include 'mtk.inc'
      character*(*) modelfile,tiltfile
      integer*4 iobjflag(*)
      logical exist,readw_or_imod,newfile
      real*4 tiltzstrt(1000),tilt(1000),costilt(1000),remapz(1000)
      integer*4 listgap(1000)
      real*4 zgapst(*),zgapnd(*)
      integer*4 getimodhead,getimodflags,getimodscales
      integer*4 in5
      common /nmsinput/ in5
c       
      newfile=.false.
      if(modelfile.ne.' ')go to 92
91    print *,'Enter name of input model file, or Return to skip',
     &    ' to entering options'
      read(in5,'(a)')modelfile
      if (modelfile .eq.' ') return
      newfile=.true.
c       
92    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 91
c       
      if(newfile)then
        write(*,'(1x,a,$)')
     &      'Name of file of tilt info (Return if none): '
        read(in5,'(a)')tiltfile
      endif
c       
      if(tiltfile.ne.' ')then
        call dopen(3,tiltfile,'ro','f')
        ntilts=0
3       i=ntilts+1
        read(3,*,end=5)tiltzstrt(i),tilt(i)
        ntilts=i
        costilt(ntilts)=cosd(tilt(ntilts))
        go to 3
5       remapz(1)=tiltzstrt(1)
        do i=1,ntilts-1
          remapz(i+1)=remapz(i)+(tiltzstrt(i+1)-tiltzstrt(i))
     &        /costilt(i)
        enddo
        zmax=-1.e10
        do i=1,n_point
          zz=p_coord(3,i)
          if(zz.ge.tiltzstrt(1))then
            itilt=ntilts
            do while(zz.lt.tiltzstrt(itilt))
              itilt=itilt-1
            enddo
            znew=remapz(itilt)+(zz-tiltzstrt(itilt))/
     &          costilt(itilt)
            p_coord(3,i)=znew
            zmax=max(zmax,znew)
          endif
        enddo
        print *,'new maximum z is',zmax
      endif
c       
c       DNM 10/7/04: Better get scale and offset regardless of new or not
c       
      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      ierr2 = getimodscales(ximscale, yimscale, zimscale)
      if(newfile)then
        defscal=1.e6
        if(ierr.eq.0.and.abs(xyscal-defscal)/defscal.gt.1.e-5)then
          write(*,'(a,f10.6,a)')' Scale set from model header at',
     &        xyscal,' microns/pixel'
          zscal=xyscal*zscale
        else
          ifflip=0
          write(*,'(1x,a,$)')'Magnification of negatives: '
          read(in5,*)xmag
          write(*,'(1x,a,$)')'Scale at which negatives were digitized'
     &        //' (microns per pixel): '
          read(in5,*)umperpix
          write(*,'(1x,a,$)')'Nominal section thickness (nm): '
          read(in5,*)secthick
c           
          xyscal=umperpix/xmag
          zscal=secthick/1000.
        endif
      endif
c       
c       flip, shift back to imod coordinates, and scale points
c       
      do i=1,n_point
        xt=(p_coord(1,i)-xofs) / ximscale
        yt=(p_coord(2,i)-yofs) / yimscale
        zt=(p_coord(3,i)-zofs) / zimscale
        if(ifflip.ne.0)then
          ftmp=yt
          yt=zt
          zt=ftmp
        endif
        p_coord(1,i)=xt*xyscal
        p_coord(2,i)=yt*xyscal
        p_coord(3,i)=zt*zscal
      enddo
c       
c       get the object types
c       
      do i=1,limflag
        iobjflag(i)=1
      enddo
      ierr=getimodflags(iobjflag,limflag)
      if(ierr.ne.0)then
        print *,'Error getting object types; assuming all objects '//
     &      'are open contours'
      else
c         
c         strip mesh flag off of 1's and 2's
c         
        do i=1,limflag
          if(iobjflag(i).gt.4)iobjflag(i)=iobjflag(i)-4
        enddo
      endif       
c       
c       get list of z gaps to connect surfaces across
c       
      if(newfile)then
        print *,'Enter list of Z values (numbered from 0) across ',
     &      'which surfaces should be connected (Return for none)'
        call rdlist(in5,listgap,nlistgap)
        ngaps=0
        if(nlistgap.gt.0)then
c           
c           really dumb, but convert the list back to start and end values
c           
          icur=1
          ngaps=1
          zgapst(1)=listgap(1)*zscal
          do while(icur.le.nlistgap)
            if(icur.eq.nlistgap.or.listgap(icur)+1.ne.listgap(icur+1))
     &          then
              zgapnd(ngaps)=listgap(icur)*zscal
              icur=icur+1
              if(icur.le.nlistgap)then
                ngaps=ngaps+1
                zgapst(ngaps)=listgap(icur)*zscal
              endif
            else
              icur=icur+1
            endif
          enddo
        endif
      endif
c       
c       initialize # of loaded meshes and number of shifted objects
c       
      nmeshloaded=0
      nobjshifted=0
      return
      end

c       
c       return open contours and scattered points in the "mt" arrays
c       
      subroutine get_objects(zstart,zend,
     &    xmt,ymt, zmt, indstrt,npntobj, icolor,nmt,iobjmod,
     &    iobjflag,limxyz,limwobj)
      include 'model.inc'
      real*4 xmt(*),ymt(*),zmt(*)
      integer*4 indstrt(*),npntobj(*),icolor(*),iobjmod(*),iobjflag(*)
c       
      indfree=1
      nmt=0
c       
      do iobj=1,max_mod_obj
        imodobj=256-obj_color(2,iobj)
        if (imodobj.le.0)call errorexit(
     &      'TOO MANY MODEL OBJECTS FOR ARRAYS')
        iflag=iobjflag(imodobj)
        if(npt_in_obj(iobj).gt.0)then
          ibase=ibase_obj(iobj)
c           
c           keep "coplanar" contours now (huh?); if z limits are set, trim
c           open contours until start and end within limits
c           
          ipstrt=1
          ipend=npt_in_obj(iobj)
          if(iflag.eq.1.and.(zstart.ne.0..or.zend.ne.0.))then
            do while (((zend.ne.0.and.
     &          p_coord(3,abs(object(ipend+ibase))).gt.zend)
     &          .or. (zstart.ne.0.and.
     &          p_coord(3,abs(object(ipend+ibase))).lt.zstart)))
              ipend=ipend-1
              if (ipend .le. 0) exit
            enddo
            do while (ipstrt.lt.ipend.and.
     &          ((zend.ne.0.and.p_coord(3,abs(object(ipstrt+ibase))).gt.zend)
     &          .or. (zstart.ne.0.and.
     &          p_coord(3,abs(object(ipstrt+ibase))).lt.zstart)))
              ipstrt=ipstrt+1
            enddo
          endif
          if(iflag.eq.2.or.(iflag.eq.1.and.ipend+1-ipstrt.ge.2))then
            nmt=nmt+1
            if(nmt.gt.limwobj)call errorexit(
     &          'TOO MANY LINE & SCATTERED POINT CONTOURS FOR ARRAYS')
            indstrt(nmt)=indfree
            npntobj(nmt)=ipend+1-ipstrt
            iobjmod(nmt)=iobj
            icolor(nmt)=imodobj
            if(obj_color(1,iobj).eq.0)icolor(nmt)=-icolor(nmt)
            if(indfree+ipend-ipstrt.ge.limxyz)call errorexit(
     &          'TOO MANY POINTS FOR LINE/SCATTERED POINT ARRAYS')
            do ipt=ipstrt,ipend
              ip=abs(object(ipt+ibase))
              xmt(indfree)=p_coord(1,ip)
              ymt(indfree)=p_coord(2,ip)
              zmt(indfree)=p_coord(3,ip)
              indfree=indfree+1
            enddo
          endif
        endif
      enddo
      return
      end

c       
c       
c       SAVE_MODEL saves the current model after unscaling it.  If NINWIN
c       is nonzero, it can also save new objects marking the points of
c       closest approach
c       
      subroutine save_model(xyscal,zscal,xofs,yofs,zofs,ifflip,
     &    iobjflag,xmt, ymt, zmt,indstrt,
     &    npntobj, icolor,nmt,ninwin,iobjwin,nobjwin,iobjmod,endsep)
      implicit none
      include 'model.inc'
      include 'mtk.inc'
      integer limchg,limsizes
      parameter (limchg=2000, limsizes = 10000)
      real*4 xmt(*),ymt(*),zmt(*),endsep(*),chnglo(limchg),chnghi(limchg)
      integer*4 indstrt(*),npntobj(*),icolor(*),iobjwin(*),iobjmod(*)
      real*4 xyscal,zscal,xofs,yofs,zofs,ftmp,xt,yt,zt
      real*4 ximscale, yimscale, zimscale, sizes(limsizes)
      integer*4 ifflip,nmt,ninwin,nobjwin,ibasescat,locobj,icont,isurf,imesh
      integer*4 icolold(limchg),icolnew(limchg),iobjflag(*),icolused(limchg)
      integer*4 i,lastobject,ncolchg,icolcon,icolscat,ierr2,iflag,iobj,iorig
      integer*4 icoltmp,maybenew,icol,icolind,iow,ibase,ipt,ii,jj,ibasecon
      integer*4 inew, numSizes, imodobj, imodcont
      character*120 lastmodel
      integer*4 getimodobjsize,getimodscales
      integer*4 getContPointSizes,putContPointSizes
      integer*4 in5
      common /nmsinput/ in5
c       
95    write(*,'(1x,a,$)')'Name of output model file: '
      read(in5,'(a)')lastmodel
c       
      lastobject = getimodobjsize()
      ncolchg=0
      if(nobjwin.ne.0)then
        print *,'Enter list of objects for which to put',
     &      'contours in window into new objects (Return for none)'
        call rdlist(in5,icolold,ncolchg)
        do i = 1, ncolchg
          icolnew(i) = lastobject + i
          icolused(i) = 0
        enddo
        if(ncolchg.ne.0 .and. nobjwin.lt.0)then
          print *,'Enter lower and upper limits of end separation',
     &        ' for each object change'
          read(in5,*)(chnglo(i),chnghi(i),i=1,ncolchg)
        endif
      endif
      icolcon = lastobject + ncolchg + 1
      icolscat = lastobject + ncolchg + 2
c       
c       zero out the model first; all non-mesh objects
c       
      ierr2 = getimodscales(ximscale, yimscale, zimscale)
      do i=1,max_mod_obj
        if(npt_in_obj(i).gt.0)then
          iflag=iobjflag(256-obj_color(2,i))
          if(iflag.eq.1.or.iflag.eq.2) npt_in_obj(i)=0
        endif
      enddo
c       
c       save each separate object.  Keep track of which new objects are
c       being accessed
c       
      do iobj=1,nmt
        iorig=iobjmod(iobj)
c         ibase_obj(iobj)=indstrt(iobj)-1
        icoltmp=icolor(iobj)
        if(ncolchg.ne.0)then
          maybenew=-999
          if(nobjwin.ge.0)then
c             
c             First see if this contour is in an object being changed
c             
            do icol=1,ncolchg
              if(icoltmp.eq.icolold(icol))then
                maybenew=icolnew(icol)
                icolind = icol
              endif
            enddo
c             
c             Then see if this contour is in the window (positive iobjwin)
c             
            if(maybenew.ne.-999)then
              do iow=1,nobjwin
                if(iobj.eq.iobjwin(iow))then
                  icoltmp=maybenew
                  icolused(icolind) = 1
                endif
              enddo
            endif
          else
c             
c             For dealing with end separations
c             
            do iow=1,-nobjwin
              if(iobjmod(iobj).eq.iobjwin(iow))then
                do icol=1,ncolchg
                  if(icoltmp.eq.icolold(icol).and. endsep(iow).ge.
     &                chnglo(icol).and. endsep(iow).le.chnghi(icol)) then
                    maybenew=icolnew(icol)
                    icolused(icol) = 1
                  endif
                enddo
              endif
            enddo
            if(maybenew.ne.-999)icoltmp=maybenew
          endif
        endif
c         
c         reassign contour to new object by making a new contour with the
c         new color value, leaving old one empty, and copy the
c         contour's points in regardless of whether it is new or old
c         Also transfer point sizes to a new contour
c         
        inew = iorig
        if (icoltmp.ne.icolor(iobj)) then
          max_mod_obj = max_mod_obj + 1
          inew = max_mod_obj
          obj_color(2,inew)=256-abs(icoltmp)
          call objtocont(iorig, obj_color, imodobj, imodcont)
          ierr2 = getContPointSizes(imodobj, imodcont, sizes, limsizes,
     &        numSizes)
          if (ierr2 .eq. 0 .and. numSizes .gt. 0) then
            call objtocont(inew, obj_color, imodobj, imodcont)
            ierr2 = putContPointSizes(imodobj, imodcont, sizes, numSizes);
          endif
        endif
        obj_color(2,inew)=256-abs(icoltmp)
        npt_in_obj(inew)=npntobj(iobj)
        if(icoltmp.ge.0)then
          obj_color(1,inew)=1
        else
          obj_color(1,inew)=0
        endif
        ibase=ibase_obj(iorig)
        ibase_obj(inew) = ibase

        do ipt=1,npntobj(iobj)
          ii=indstrt(iobj)+ipt-1
          jj=object(ibase+ipt)
          p_coord(1,jj)=xmt(ii)
          p_coord(2,jj)=ymt(ii)
          p_coord(3,jj)=zmt(ii)
        enddo
      enddo
c       
c       Next look at surfaces in window (negative iobjwin) and find the objects
c       they originate from
c       
      if (ncolchg .gt. 0) then
        do iow=1,nobjwin
          isurf = -iobjwin(iow)
          if (isurf .gt. 0) then
            icoltmp = 0
            do imesh = 1, nmeshloaded
              if (isurf .ge. iobjsurf(imesh) .and. isurf .lt.
     &            iobjsurf(imesh) + nsurfobj(imesh)) then
c                 
c                 See if this object is on change list
c                 
                icolind = -1
                do icol=1,ncolchg
                  if(iobjmesh(imesh).eq.icolold(icol)) icolind = icol
                enddo
                if (icolind .gt. 0) then
c                   
c                   change all the contours in the surface: again, make a new
c                   contour, assign points to it and zero out old one, and
c                   transfer point sizes
c                   
                  icolused(icolind) = 1
                  do icont=istrcont(isurf),istrcont(isurf)+ncontinsurf(isurf)-1
                    iobj=listcont(icont)
                    max_mod_obj = max_mod_obj + 1
                    inew = max_mod_obj
                    npt_in_obj(inew) = npt_in_obj(iobj)
                    npt_in_obj(iobj) = 0
                    ibase_obj(inew) = ibase_obj(iobj)
                    obj_color(2,inew) = 256 - icolnew(icolind)
                    obj_color(1,inew) = 1
                    call objtocont(iobj, obj_color, imodobj, imodcont)
                    ierr2 = getContPointSizes(imodobj, imodcont, sizes,
     &                  limsizes, numSizes)
                    if (ierr2 .eq. 0 .and. numSizes .gt. 0) then
                      call objtocont(inew, obj_color, imodobj, imodcont)
                      ierr2 = putContPointSizes(imodobj, imodcont, sizes,
     &                    numSizes);
                    endif
                  enddo
                endif
              endif
            enddo
          endif
        enddo
      endif
c       
c       create connectors
c       
      n_point=ibase_free+3*ninwin
      do i=1,3*ninwin
        jj=ibase_free+i
        ii=indstrt(nmt)+npntobj(nmt)+i-1
        object(jj)=jj
        p_coord(1,jj)=xmt(ii)
        p_coord(2,jj)=ymt(ii)
        p_coord(3,jj)=zmt(ii)
      enddo
c       
c       transfer object flag to new objects if any
c       
      do icol = 1, ncolchg
        if (icolused(icol) .ne. 0) then
          lastobject = lastobject + 1
          iflag = iobjflag(icolold(icol))
          if (iflag .eq. 4) iflag = 0
          call putimodflag(lastobject, iflag)
        endif
      enddo
c       
c       save connecting lines if any
c       
      ibasecon=ibase_free
      ibasescat=ibase_free+2*ninwin
      do i=1,ninwin
        iobj=max_mod_obj+i
        npt_in_obj(iobj)=2
        obj_color(1,iobj)=1
        obj_color(2,iobj)=256-icolcon
        locobj=ibase_free+1+3*(i-1)
        ibase_obj(iobj)=ibasecon+2*(i-1)
        object(ibase_obj(iobj)+1)=locobj
        object(ibase_obj(iobj)+2)=locobj+2
        object(ibasescat+i)=locobj+1
      enddo
      if(ninwin.gt.0)then
        n_object=n_object+ninwin+1
        max_mod_obj=max_mod_obj+ninwin+1
        npt_in_obj(max_mod_obj)=ninwin
        ibase_obj(max_mod_obj)=ibasescat
        obj_color(1,max_mod_obj)=1
        obj_color(2,max_mod_obj)=256-icolscat
        
        call putimodflag(lastobject + 1, 1)
        lastobject = lastobject + 2
        call putimodflag(lastobject, 2)
        call putscatsize(lastobject, 5)
      endif
      print *,'There are',lastobject,' objects in the output model'
      print *,'Connecting lines and points are in objects', lastobject-1,
     &    ' and',lastobject
c       
c       unscale, unflip
c       
      do i=1,n_point
        xt=p_coord(1,i)/xyscal
        yt=p_coord(2,i)/xyscal
        zt=p_coord(3,i)/zscal
        if(ifflip.ne.0)then
          ftmp=yt
          yt=zt
          zt=ftmp
        endif
        p_coord(1,i)=ximscale*xt+xofs
        p_coord(2,i)=yimscale*yt+yofs
        p_coord(3,i)=zimscale*zt+zofs
      enddo
c       
      call write_wmod(lastmodel)
      close(20)
      return
      end


c       PROCESS_MESH reads in the mesh for object IMODOBJ, scales coordinates
c       by XYSCAL in X/Y and ZSCAL in Z, and fills arrays with indices to
c       triangles and polygons.  It determines the sines and cosines needed
c       to rotate each triangle to a plane, and stores coordinates for
c       rotated triangles.  It then combines adjacent, overlapping polygons
c       into surfaces, continuing a surface across gaps if any are specified.
c       MODIND is for temporary storage of indices
c       ZGAPST and ZGAPND have starting and ending Z values of NGAP gaps.
c       IFERR returns 1 if there is an error
c       Diagnostic reports and error messages are suppressed if IFHUSH = 1

      subroutine process_mesh(imodobj,xyscal,zscal,modind,zgapst,
     &    zgapnd,ngaps,iferr,ifhush)
      include 'mtk.inc'
      include 'model.inc'
      parameter (limind=limverts*6,limcxy=2000,limipmin=100)
      integer*4 modind(limind),ipmin(limipmin)
      real*4 avec(3),bvec(3),cvec(3),zgapst(*),zgapnd(*)
      real*4 cx1(limcxy),cx2(limcxy),cy1(limcxy),cy2(limcxy)
      real*4 tmpxmin(limcont),tmpxmax(limcont)
      real*4 tmpymin(limcont),tmpymax(limcont)
      real*4 tmpzval(limcont)
      logical anyinside,inside
      integer*4 getimodverts
      istartcode=-23
      iendcode=-22
      idonecode=-1
      iferr=1
      nduptri=0
      nnontri=0
      ierr=getimodverts(imodobj,verts(1,nverts+1),
     &    modind, limverts-nverts,limind,newverts,newind)
      if (ierr.ne.0)then
        if(ifhush.eq.0)print *,'Error or insufficient space '//
     &      'loading mesh for object',imodobj
        return
      endif
c       
c       scale the vertices
c       
      do iv=nverts+1,nverts+newverts
        verts(1,iv)=verts(1,iv)*xyscal
        verts(2,iv)=verts(2,iv)*xyscal
        verts(3,iv)=verts(3,iv)*zscal
      enddo
c       
c       process polygons, get indices to triangles
c       
      nmeshloaded=nmeshloaded+1
      iobjmesh(nmeshloaded)=imodobj
      iobjpoly(nmeshloaded)=npoly+1
      ilist=1
      do while(modind(ilist).ne.idonecode)
c         
c         advance to start of a polygon
c         
        do while(modind(ilist).ne.istartcode.and.
     &      modind(ilist).ne.idonecode)
          ilist=ilist+1
        enddo
        if(modind(ilist).eq.istartcode)then
          ilist=ilist+1
          if(npoly.eq.limpoly)then
            if(ifhush.eq.0)print *,'Too many polygons for arrays'
            return
          endif
          npoly=npoly+1
          ninpoly(npoly)=0
          polyxmin(npoly)=1.e10
          polyymin(npoly)=1.e10
          polyzmin(npoly)=1.e10
          polyxmax(npoly)=-1.e10
          polyymax(npoly)=-1.e10
          polyzmax(npoly)=-1.e10
          polyarea(npoly)=0.
          do while (modind(ilist).ne.iendcode)
c             
c             start a new triangle: first check that its a triangle
c             shift the indices up by amount already in verts array
c             plus one because they are C indices
c             
            i1=modind(ilist)+nverts+1
            i2=modind(ilist+1)+nverts+1
            i3=modind(ilist+2)+nverts+1
            ifok=1
            if(i1.eq.i2.or.i2.eq.i3.or.i1.eq.i3)then
c               print *,'NON-TRIANGLE ignored',npoly,ntriang
              nnontri=nnontri+1
              ifok=0
            endif
c             
c             check for duplicate triangles in this polygon
c             
            if(ifok.eq.1.and.ninpoly(npoly).gt.0)then
              isum=i1+i2+i3
              do itri=istrpoly(npoly),ntriang
                j1=indvert(1,itri)
                j2=indvert(2,itri)
                j3=indvert(3,itri)
                if(j1+j2+j3.eq.isum)then
                  if((i1.eq.j1.and.(i2.eq.j2.or.i2.eq.j3)).or.
     &                (i1.eq.j2.and.(i2.eq.j1.or.i2.eq.j3)).or.
     &                (i1.eq.j3.and.(i2.eq.j1.or.i2.eq.j2)))
     &                then
                    ifok=0
                    nduptri=nduptri+1
c                     print *,'DUPLICATE TRIANGLE ignored',
c                     &    npoly,ntriang,itri
                  endif
                endif
              enddo
            endif
c             
            if(ifok.ne.0)then
c               
c               save the triangle indices
c               
              if(ntriang.eq.limtri)then
                if(ifhush.eq.0)print *,'Too many triangles in mesh'
     &              //' to fit in arrays'
                return
              endif
              ntriang=ntriang+1
              if(ninpoly(npoly).eq.0)istrpoly(npoly)=ntriang
              ninpoly(npoly)=ninpoly(npoly)+1
              indvert(1,ntriang)=i1
              indvert(2,ntriang)=i2
              indvert(3,ntriang)=i3
c               
c               get the bounding boxes
c               
              trixmin(ntriang)=min(verts(1,i1),verts(1,i2),
     &            verts(1,i3))
              triymin(ntriang)=min(verts(2,i1),verts(2,i2),
     &            verts(2,i3))
              trizmin(ntriang)=min(verts(3,i1),verts(3,i2),
     &            verts(3,i3))
              trixmax(ntriang)=max(verts(1,i1),verts(1,i2),
     &            verts(1,i3))
              triymax(ntriang)=max(verts(2,i1),verts(2,i2),
     &            verts(2,i3))
              trizmax(ntriang)=max(verts(3,i1),verts(3,i2),
     &            verts(3,i3))
              polyxmin(npoly)=min(polyxmin(npoly),trixmin(ntriang))
              polyymin(npoly)=min(polyymin(npoly),triymin(ntriang))
              polyzmin(npoly)=min(polyzmin(npoly),trizmin(ntriang))
              polyxmax(npoly)=max(polyxmax(npoly),trixmax(ntriang))
              polyymax(npoly)=max(polyymax(npoly),triymax(ntriang))
              polyzmax(npoly)=max(polyzmax(npoly),trizmax(ntriang))
c               
c               compute rotation to a plane and rotated coordinates
c               
              do i=1,3
                avec(i)=verts(i,i2)-verts(i,i1)
                bvec(i)=verts(i,i3)-verts(i,i2)
              enddo
              call crossproduct(avec,bvec,cvec)
              cxylen=sqrt(cvec(1)**2+cvec(2)**2)
              clen=sqrt(cvec(1)**2+cvec(2)**2+cvec(3)**2)
              polyarea(npoly)=polyarea(npoly)+clen/2
c               
c               get rotation angles, save sines and cosines
c               
              if(cxylen.lt.1.e-10)then
                singam=0.
                cosgam=1.
              else
                singam=-cvec(2)/cxylen
                cosgam=cvec(1)/cxylen
              endif
              sinbet=cxylen/clen
              cosbet=cvec(3)/clen
              cgam(ntriang)=cosgam
              sgam(ntriang)=singam
              cbet(ntriang)=cosbet
              sbet(ntriang)=sinbet
c               
c               calculate and save rotated coordinates
c               
              zrsum=0.
              do i=1,3
                iv=indvert(i,ntriang)
                xyrot(i,1,ntriang)=(verts(1,iv)*cosgam-
     &              verts(2,iv)*singam)*cosbet-verts(3,iv)*sinbet
                xyrot(i,2,ntriang)=verts(1,iv)*singam+
     &              verts(2,iv)*cosgam
                zrsum=zrsum+(verts(1,iv)*cosgam-
     &              verts(2,iv)*singam)*sinbet+verts(3,iv)*cosbet
              enddo
              zrot(ntriang)=zrsum/3.
            endif
            ilist=ilist+3
          enddo
        endif
      enddo
      nverts=nverts+newverts
      npolyobj(nmeshloaded)=npoly+1-iobjpoly(nmeshloaded)
c       
c       sort the polygons into surfaces: use modind as temporary surface #
c       
c       print *,'Sorting surfaces'
      listend=iobjpoly(nmeshloaded)-1
      do i=listend+1,npoly
        modind(i)=0
      enddo
      iobjsurf(nmeshloaded)=nsurf+1
c       
c       do until all polygons are on the list
c       
      do while(listend.lt.npoly)
c         
c         find next unassigned polygon, assign it next surface #
c         
        iffound=0
        ip=iobjpoly(nmeshloaded)
        do while(ip.le.npoly.and.iffound.eq.0)
          if(modind(ip).eq.0)then
            iffound=1
          endif
          ip=ip+1
        enddo
c         
c         start a new surface, set current point to the new one on list
c         
        ip=ip-1
        if(nsurf.ge.limsurf)then
          if(ifhush.eq.0)print *,'Too many surfaces for arrays'
          return
        endif
        nsurf=nsurf+1
        modind(ip)=nsurf
        listend=listend+1
        listsurf(listend)=ip
        istrsurf(nsurf)=listend
        ncontinsurf(nsurf)=0
        surfxmin(nsurf)=polyxmin(ip)
        surfymin(nsurf)=polyymin(ip)
        surfzmin(nsurf)=polyzmin(ip)
        surfxmax(nsurf)=polyxmax(ip)
        surfymax(nsurf)=polyymax(ip)
        surfzmax(nsurf)=polyzmax(ip)
        surfarea(nsurf)=polyarea(ip)
c         print *,'starting surface ',nsurf,' with polygon',ip
        lcur=listend
c         
c         search for more to add to list until current is caught up to end
c         
        do while(lcur.le.listend)
          ipcur=listsurf(lcur)
          do ip=iobjpoly(nmeshloaded),npoly
            if(modind(ip).eq.0)then
c               
c               check for bounding box overlap, don't worry about whether
c               on same plane in Z.  THIS WILL FAIL FOR MESHES WITH FORCED
c               CONNECTIONS BETWEEN CONTOURS THAT DON'T OVERLAP
c               
              if(polyxmin(ip).le.polyxmax(ipcur).and.
     &            polyxmin(ipcur).le.polyxmax(ip).and.
     &            polyymin(ip).le.polyymax(ipcur).and.
     &            polyymin(ipcur).le.polyymax(ip).and.
     &            polyzmin(ip).le.polyzmax(ipcur).and.
     &            polyzmin(ipcur).le.polyzmax(ip))then
c                 
c                 confirm connection by searching for common vertex
c                 
                iffound=0
                itr=istrpoly(ipcur)
                do while(itr.lt.istrpoly(ipcur)+ninpoly(ipcur).and.
     &              iffound.eq.0)
                  i1=indvert(1,itr)
                  i2=indvert(2,itr)
                  i3=indvert(3,itr)
                  jtr=istrpoly(ip)
                  do while(jtr.lt.istrpoly(ip)+ninpoly(ip).and.iffound.eq.0)
                    do iv=1,3
                      itst=indvert(iv,jtr)
                      if(i1.eq.itst.or.i2.eq.itst.or.i3.eq.itst)iffound=1
                    enddo
                    jtr=jtr+1
                  enddo
                  itr=itr+1
                enddo
c                 
c                 if there is a common vertex, then add the polygon being
c                 tested to the surface and the list.
c                 THIS COULD BE STRENGTHENED BY REQUIRING MULTIPLE COMMON
c                 POINTS, BUT THAT MIGHT NOT ALWAYS WORK EITHER
c                 
                if(iffound.gt.0)then
                  modind(ip)=nsurf
                  listend=listend+1
                  listsurf(listend)=ip
                  surfxmin(nsurf)=min(surfxmin(nsurf),polyxmin(ip))
                  surfymin(nsurf)=min(surfymin(nsurf),polyymin(ip))
                  surfzmin(nsurf)=min(surfzmin(nsurf),polyzmin(ip))
                  surfxmax(nsurf)=max(surfxmax(nsurf),polyxmax(ip))
                  surfymax(nsurf)=max(surfymax(nsurf),polyymax(ip))
                  surfzmax(nsurf)=max(surfzmax(nsurf),polyzmax(ip))
                  surfarea(nsurf)=surfarea(nsurf)+polyarea(ip)
                endif
              endif
            endif
          enddo
c           
c           end of scan through polygons; advance the current list position
c           
          lcur=lcur+1
c           
c           if there are gaps to connect across and the surface appears done
c           then see if this surface has an edge in the region.  
c           
          if(ngaps.gt.0.and.lcur.gt.listend)then
            zinc=1.01*zscal
            do igap=1,ngaps
              if(surfzmax(nsurf)+zinc.ge.zgapst(igap).and.
     &            surfzmin(nsurf)-zinc.le.zgapnd(igap))then
                do ilist=istrsurf(nsurf),listend
                  ipoly=listsurf(ilist)
                  ifmost=0
                  if(polyzmax(ipoly)+zinc.ge.zgapst(igap).and.
     &                polyzmin(ipoly)-zinc.le.zgapnd(igap))then
c                     
c                     for a polygon on the gap, first search for the closest
c                     polygon in free list whose bounding box overlaps and
c                     that is in gap
c                     
                    dzmin=1.e10
                    npmin=0
                    do ip=iobjpoly(nmeshloaded),npoly
                      if(modind(ip).eq.0)then
                        if(polyxmin(ip).le.polyxmax(ipoly).and.
     &                      polyxmin(ipoly).le.polyxmax(ip).and.
     &                      polyymin(ip).le.polyymax(ipoly).and.
     &                      polyymin(ipoly).le.polyymax(ip).and.
     &                      polyzmin(ip)-zinc.le.zgapnd(igap).and.
     &                      polyzmax(ip)+zinc.ge.zgapst(igap)) then
                          dz=abs(polyzmin(ip)-polyzmin(ipoly))
                          if(dz.lt.dzmin)then
                            dzmin=dz
                            npmin=1
                            ipmin(1)=ip
                          elseif(dz.eq.dzmin.and.npmin.lt.limipmin)then
                            npmin=npmin+1
                            ipmin(npmin)=ip
                          endif
                        endif
                      endif
                    enddo
c                     
c                     now that direction is known, make sure that the
c                     polygon in the surface is actually the farthest one in
c                     
                    if(npmin.gt.0)then
                      ifmost=1
                      do jlist=istrsurf(nsurf),listend
                        jpoly=listsurf(jlist)
                        if(jpoly.ne.ipoly.and.
     &                      ((polyzmax(ipmin(1)).gt.polyzmax(ipoly).and.
     &                      polyzmax(jpoly).gt.polyzmax(ipoly).and.
     &                      polyzmax(jpoly)+zinc.le.zgapnd(igap)).or.
     &                      (polyzmax(ipmin(1)).lt.polyzmax(ipoly).and.
     &                      polyzmin(jpoly).lt.polyzmin(ipoly).and.
     &                      polyzmin(jpoly)-zinc.ge.zgapst(igap))).and.
     &                      polyxmin(ipoly).le.polyxmax(jpoly).and.
     &                      polyxmin(jpoly).le.polyxmax(ipoly).and.
     &                      polyymin(ipoly).le.polyymax(jpoly).and.
     &                      polyymin(jpoly).le.polyymax(ipoly))ifmost=0
                      enddo
c                       
c                       if this is the farther one into the gap, extract the
c                       contours
c                       
                      if(ifmost.ne.0)then
                        do ipt=1,npmin
                          ip=ipmin(ipt)
                          zextr1=polyzmax(ipoly)
                          zextr2=polyzmin(ip)
                          if(polyzmax(ip).lt.polyzmax(ipoly))then
                            zextr1=polyzmin(ipoly)
                            zextr2=polyzmax(ip)
                          endif
                          call contour_from_poly(ipoly,zextr1,cx1, cy1,
     &                        nc1, limcxy)
                          call contour_from_poly(ip,zextr2,cx2,cy2,
     &                        nc2,limcxy)
c                           
c                           check each contour for points inside the other
c                           
                          anyinside=.false.
                          if(nc2.gt.2.and.nc1.gt.2)then
                            ic1=1
                            do while(ic1.le.nc1.and. .not.anyinside)
                              anyinside=inside(cx2,cy2,nc2,cx1(ic1),
     &                            cy1(ic1))
                              ic1=ic1+1
                            enddo
                            ic2=1
                            do while(ic2.le.nc2.and. .not.anyinside)
                              anyinside=inside(cx1,cy1,nc1,cx2(ic2),
     &                            cy2(ic2))
                              ic2=ic2+1
                            enddo
                          endif
c                           
c                           add to surface
c                           
                          if(anyinside)then
                            modind(ip)=nsurf
                            listend=listend+1
                            listsurf(listend)=ip
                            surfxmin(nsurf)=min(surfxmin(nsurf),
     &                          polyxmin(ip))
                            surfymin(nsurf)=min(surfymin(nsurf),
     &                          polyymin(ip))
                            surfzmin(nsurf)=min(surfzmin(nsurf),
     &                          polyzmin(ip))
                            surfxmax(nsurf)=max(surfxmax(nsurf),
     &                          polyxmax(ip))
                            surfymax(nsurf)=max(surfymax(nsurf),
     &                          polyymax(ip))
                            surfzmax(nsurf)=max(surfzmax(nsurf),
     &                          polyzmax(ip))
                            surfarea(nsurf)=surfarea(nsurf)+
     &                          polyarea(ip)
                          endif
                        enddo
                      endif
                    endif
                  endif
                enddo
              endif
            enddo
          endif
        enddo
c         
c         surface all found, finish assignments for it
c         
        ninsurf(nsurf)=listend+1-istrsurf(nsurf)
      enddo
      nsurfobj(nmeshloaded)=nsurf+1-iobjsurf(nmeshloaded)
c       
c       GIVEN SURFACES, NEED TO APPLY SHIFTS NOW IF ANY EXIST
c       
      ifzerod=0
      do iobj=1,nobjshifted
        if(iobjshift(iobj).eq.imodobj)then
          if(nitemshift(iobj).eq.nsurfobj(nmeshloaded))then
            do ii=1,nitemshift(iobj)
              ishift=ii+istrshift(iobj)-1
              isurf=iobjsurf(nmeshloaded)+ii-1
              call shiftsurfmesh(isurf,shifts(1,ishift),1,modind,
     &            ifzerod,zscal)
            enddo
          else
            if(ifhush.eq.0)print *,
     &          'Number of shifts and surfaces do not match'
          endif
        endif
      enddo
c       
c       NOW CROSS-REFERENCE CONTOURS TO SURFACES
c       
c       print *,'Cross-referencing contours'
      ntmp=0
      norphan=0
      tol=0.1*xyscal
      do iobj=1,max_mod_obj
        if(obj_color(2,iobj).eq.256-imodobj.and.npt_in_obj(iobj).gt.2)
     &      then
c           
c           look for contours in object with at least 3 points
c           
          ninobj=npt_in_obj(iobj)
          ibase=ibase_obj(iobj)
          txmin=1.e10
          txmax=-txmin
          tymin=txmin
          tymax=txmax
c           
c           get min and max, copy contour
c           
          do ipt=1,min(ninobj,limcxy)
            ipnt=abs(object(ibase+ipt))
            cx1(ipt)=p_coord(1,ipnt)
            cy1(ipt)=p_coord(2,ipnt)
            txmin=min(txmin,cx1(ipt))
            tymin=min(tymin,cy1(ipt))
            txmax=max(txmax,cx1(ipt))
            tymax=max(tymax,cy1(ipt))
          enddo
          tzval=p_coord(3,ipnt)
          tzmax=tzval+tol
          tzmin=tzval-tol
c           
c           scan surfaces of this object
c           
          isurf=iobjsurf(nmeshloaded)
          icsurf=0
          do while(isurf.le.iobjsurf(nmeshloaded)+nsurfobj(nmeshloaded)-1
     &        .and.icsurf.eq.0)
            if(txmin.le.surfxmax(isurf).and.surfxmin(isurf).le.txmax.and.
     &          tymin.le.surfymax(isurf).and.surfymin(isurf).le.tymax
     &          .and.tzmax.ge.surfzmin(isurf).and.tzmin.le.surfzmax(isurf))
     &          then
c               
c               if intersect, check through polygons
c               
              list=istrsurf(isurf)
              do while(list.le.istrsurf(isurf)+ninsurf(isurf)-1
     &            .and.icsurf.eq.0)
                ipoly=listsurf(list)
                if(txmin.le.polyxmax(ipoly).and.polyxmin(ipoly).le.txmax.and.
     &              tymin.le.polyymax(ipoly).and.polyymin(ipoly).le.tymax
     &              .and.tzmax.ge.polyzmin(ipoly).and.
     &              tzmin.le.polyzmax(ipoly)) then
c                   
c                   then check triangles
c                   
                  itri=istrpoly(ipoly)
                  do while(itri.le.istrpoly(ipoly)+ninpoly(ipoly)-1
     &                .and.icsurf.eq.0)
c                     
c                     check each vertex of triangle against each of contour
c                     
                    do ivert=1,3
                      iv=indvert(ivert,itri)
                      do ipt=1,min(ninobj,limcxy)
                        if(abs(verts(1,iv)-cx1(ipt)).lt.tol.and.
     &                      abs(verts(2,iv)-cy1(ipt)).lt.tol.and.
     &                      abs(verts(3,iv)-tzval).lt.tol)icsurf=isurf
                      enddo
                    enddo
                    itri=itri+1
                  enddo
                endif
                list=list+1
              enddo
            endif
            isurf=isurf+1
          enddo
c           
c           got out, found or not
c           
          if(icsurf.ne.0)then
            if(ncont+ntmp.ge.limcont)then
              if(ifhush.eq.0)print *,'Too many contours for arrays'
              return
            endif
            ntmp=ntmp+1
            modind(ntmp)=iobj
            modind(ntmp+max_mod_obj)=icsurf
            ncontinsurf(icsurf)=ncontinsurf(icsurf)+1
            tmpxmin(ntmp)=txmin
            tmpymin(ntmp)=tymin
            tmpxmax(ntmp)=txmax
            tmpymax(ntmp)=tymax
            tmpzval(ntmp)=tzval
          else
            norphan=norphan+1
c             print *,iobj,' is orphan'
          endif
        endif
      enddo
c       
c       put the contours in lists, and everything else
c       
      istrcont(iobjsurf(nmeshloaded))=ncont+1
      do isurf=iobjsurf(nmeshloaded),
     &    iobjsurf(nmeshloaded)+nsurfobj(nmeshloaded)-1
        istrcont(isurf+1)=istrcont(isurf)+ncontinsurf(isurf)
        ncontinsurf(isurf)=0
        ncont=istrcont(isurf+1)-1
      enddo
      do i=1,ntmp
        isurf=modind(i+max_mod_obj)
        ind=istrcont(isurf)+ncontinsurf(isurf)
        ncontinsurf(isurf)=ncontinsurf(isurf)+1
        listcont(ind)=modind(i)
        contxmin(ind)=tmpxmin(i)
        contymin(ind)=tmpymin(i)
        contxmax(ind)=tmpxmax(i)
        contymax(ind)=tmpymax(i)
        contzval(ind)=tmpzval(i)
      enddo
      naddcont=ncont+1-istrcont(iobjsurf(nmeshloaded))

c       check integrity of all loaded mesh and contour data
c       
      do imesh=1,nmeshloaded
        imodobj=iobjmesh(imesh)
        icolobj=256-imodobj
        do isurf=iobjsurf(imesh),iobjsurf(imesh)+nsurfobj(imesh)-1
          do icont=istrcont(isurf),istrcont(isurf)+ncontinsurf(isurf)-1
            iobj=listcont(icont)
            if(icolobj.ne.obj_color(2,iobj))then
              print *,'object mismatch',imesh,imodobj,isurf,icont,
     &            iobj,icolobj,obj_color(2,iobj)
            endif
          enddo
        enddo
      enddo
      
      iferr=0
      if(ifhush.ne.0)return
      write(*,109)imodobj,newverts,newind,
     &    ntriang+1-istrpoly(iobjpoly(nmeshloaded)),
     &    npolyobj(nmeshloaded),nsurfobj(nmeshloaded),naddcont
109   format('Object',i4,':',i9,' vertices,',i9,' indices,'
     &    ,i9,' triangles,',/,i7,' polygons,',i7,' surfaces,',i7,
     &    ' contours')
      if(nduptri.gt.0)print *,nduptri,
     &    ' duplicate triangles ignored'
      if(nnontri.gt.0)print *,nnontri,
     &    ' non-triangles ignored'
      if(norphan.ne.0)print *,norphan,
     &    ' orphan contours not referenced to mesh'
      return
      end



c       CONTOUR_FROM_POLY extracts a contour at the z level given by ZEXTR
c       from polygon IP; NC points are placed in CX, CY; LIMC specifies the
c       limiting size of the CX and CY arrays

      subroutine contour_from_poly(ip,zextr,cx,cy,nc,limc)
      real*4 cx(*),cy(*)
      include 'mtk.inc'
      last=-1
      nc=0
      do itr=istrpoly(ip),istrpoly(ip)+ninpoly(ip)-1
        do iv=1,3
          ind=indvert(iv,itr)
          if(ind.ne.last.and.verts(3,ind).eq.zextr.and.nc.lt.limc)then
            nc=nc+1
            cx(nc)=verts(1,ind)
            cy(nc)=verts(2,ind)
            last=ind
          endif
        enddo
      enddo
      return
      end

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: MTK - ',message
      call exit(1)
      end


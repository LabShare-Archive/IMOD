      subroutine read_model(modelfile,iobjsurf,xyscal,zscal,
     &    clipplanes,nmodclip)
      character*(*) modelfile
      integer*4 openimoddata
      include 'sda.inc'
      include 'model.inc'
      integer*4 getimodmesh,getimodscat,getimodclip,getimodhead
      logical*1 neednorm(maxverts)
      integer*4 modind(maxverts*6)
      real*4 clipplanes(4,*)
      logical newfile,inplane,inside,firsttime
      real*4 avec(3),bvec(3),cvec(3),xm(3,4),ci(3),xminv(3,3),portm(3)
      real*4 distmin(3),cross(3,4),ptmin(3),ptclose(3),prtxyz(3)
      integer*4 indsub(3,200),ipoladj(10)
      real*4 subvert(3,200),tmpnorm(3),xyzmin(3),xyzmax(3),bx(3),by(3)
      logical cappoly(maxpoly)
      real*4 xyrot(3,2,maxtri),zrot(maxtri),cbet(maxtri),sbet(maxtri)
      real*4 cgam(maxtri),sgam(maxtri)
c       equivalence (modind,xyrot)
      equivalence (prtx,prtxyz(1)),(prty,prtxyz(2)),(prtz,prtxyz(3))
      integer*4 in5
      common /nmsinput/ in5
c       
      newfile=.false.
      firsttime=modelfile.eq.'SDAFIRSTTIME'
      if(firsttime)modelfile=' '
      if(modelfile.ne.' ')go to 92
91    write(*,'(1x,a,$)')'Name of input model file: '
      read(in5,'(a)')modelfile
      if(firsttime.and.modelfile.eq.' ')return
      newfile=.true.
c       
92    ierr=openimoddata(modelfile)
      if(ierr.ne.0)then
        print *,'error opening model, try again'
        go to 91
      endif
      ierr = getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
c       print *,xyscal,zscal
c       
c       get all the clipping planes in the model and report on them
c       
      iobj=1
      nmodclip=0
      numclip=0
      do while(numclip.ge.0)
        numclip=getimodclip(iobj,clipplanes(1,nmodclip+1))
        if(numclip.gt.0)then
          if(newfile)then
            if(nmodclip.eq.0)write(*,117)
117         format(28X,'Clipping planes in model:',/,
     &          'Plane #  Object #      A',10x,'B',10x,'C',10x,'D')
            write(*,118)(j,iobj,(clipplanes(i,j),i=1,4),j=nmodclip+1
     &          ,nmodclip+numclip)
118         format(i4,i10,3x,4f11.6)
          endif
          nmodclip=nmodclip+numclip
        endif
        iobj=iobj+1
      enddo
c       
      if(newfile)then
        write(*,'(1x,a,$)')'Object # of surface to be analyzed: '
        read(in5,*)iobjsurf
      endif
c       
      ierr=getimodmesh(iobjsurf,verts,modind,maxverts,maxverts*6)
      if(ierr.ne.0)then
        print *,'error reading surface data, try again'
        go to 91
      endif
c       
      ntriang=0
      npoly=0
      nvert=0
      istartcode=-23
      istartcode2=-25
      iendcode=-22
      idonecode=-1
      areasum=0.
c       volsumx=0.
c       volsumy=0.
      volsumz=0.
      do i=1,maxverts
        neednorm(i)=.true.
      enddo
      do i=1,3
        xyzmin(i)=1.e10
        xyzmax(i)=-1.e10
      enddo
c       
c       process list of indexes
c       
      if(newfile)print *,'Analyzing surface triangles . . .'
      ilist=1
      do while(modind(ilist).ne.idonecode)
c         
c         advance to start of a polygon
c         
        do while(modind(ilist).ne.istartcode.and.modind(ilist).ne.
     &      istartcode2.and. modind(ilist).ne.idonecode)
          ilist=ilist+1
        enddo
        if(modind(ilist).eq.istartcode.or.modind(ilist).eq.istartcode2)then
          if (modind(ilist).eq.istartcode) then
            ivbase = 1
            listinc = 2
            normadd = 0
          else
            ivbase = 0
            listinc = 1
            normadd = 1
          endif
          ilist=ilist+1
          npoly=npoly+1
          ninpoly(npoly)=0
          do while (modind(ilist).ne.iendcode)
c             
c             start a new triangle: first check that its a triangle
c             
            i1=modind(ilist+ivbase)+1
            i2=modind(ilist+ivbase + listinc)+1
            i3=modind(ilist+ivbase + 2 * listinc)+1
            ifok=1
            if(i1.eq.i2.or.i2.eq.i3.or.i1.eq.i3)then
              print *,'NON-TRIANGLE ignored',npoly,ntriang
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
     &                (i1.eq.j3.and.(i2.eq.j1.or.i2.eq.j2)))then
                    ifok=0
                    print *,'DUPLICATE TRIANGLE ignored',npoly,ntriang,itri
                  endif
                endif
              enddo
            endif
c             
            if(ifok.eq.0)then
              ilist=ilist+3*listinc
            else
              ntriang=ntriang+1
              if(ninpoly(npoly).eq.0)istrpoly(npoly)=ntriang
              ninpoly(npoly)=ninpoly(npoly)+1
c               
c               get indexes to vectors and normals
c               
              do i=1,3
                inorm=modind(ilist)+normadd+1   !IT NEEDS +1 because it's C
                indnorm(i,ntriang)=inorm
                ivert=modind(ilist+ivbase)+1
                indvert(i,ntriang)=ivert
                indedge(i,ntriang)=-1
                ilist=ilist+listinc
c                 
c                 normalize normal, scale vertex
c                 
                if(neednorm(inorm))then
                  call normalize(verts(1,inorm))
                  neednorm(inorm)=.false.
                endif
                if(neednorm(ivert))then
                  nvert=nvert+1
                  listvert(nvert)=ivert
                  do j=1,3
                    vertout(j,ivert)=verts(j,ivert)
                    verts(j,ivert)=xyscal*verts(j,ivert)
                  enddo
                  verts(3,ivert)=zscal*verts(3,ivert)
                  do j=1,3
                    xyzmin(j)=min(xyzmin(j),verts(j,ivert))
                    xyzmax(j)=max(xyzmax(j),verts(j,ivert))
                  enddo
                  neednorm(ivert)=.false.
                endif
              enddo
c               
c               on first triangle of polygon, see if it's an end cap and keep
c               track if it is
c               
              if(ninpoly(npoly).eq.1)then
                delz=(max(verts(3,i1),verts(3,i2),verts(3,i3))-
     &              min(verts(3,i1),verts(3,i2),verts(3,i3)))/
     &              (xyscal*zscal)
                cappoly(npoly)=delz.lt.0.9
              endif
c               
c               compute rotation to a plane and coefficients for normal
c               interpolation
c               
c               get equation of plane from cross-product of sides 1 and 2
c               
              do i=1,3
                avec(i)=verts(i,i2)-verts(i,i1)
                bvec(i)=verts(i,i3)-verts(i,i2)
              enddo
              call crossproduct(avec,bvec,cvec)
              cxylen=sqrt(cvec(1)**2+cvec(2)**2)
              clen=sqrt(cvec(1)**2+cvec(2)**2+cvec(3)**2)
              triarea(ntriang)=clen/2
              areasum=areasum+triarea(ntriang)
c               volsumx=volsumx+cvec(1)*
c               &                   (verts(1,i1)+verts(1,i2)+verts(1,i3))/6.
c               volsumy=volsumy+cvec(2)*
c               &                   (verts(2,i1)+verts(2,i2)+verts(2,i3))/6.
              volsumz=volsumz+cvec(3)*
     &            (verts(3,i1)+verts(3,i2)+verts(3,i3))/6.
c               
c               get rotation angles
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
c               fill matrix for solving for interpolation by rotating
c               vertices, then invert matrix
c               save the cosines, sines, and planar coordinates of triangle
c               for dropping pores onto surface later
c               
              zrsum=0.
              do i=1,3
                iv=indvert(i,ntriang)
                xm(i,1)=1.
                xm(i,2)=(verts(1,iv)*cosgam-verts(2,iv)*singam)*cosbet
     &              -verts(3,iv)*sinbet
                xm(i,3)=verts(1,iv)*singam+verts(2,iv)*cosgam
                xyrot(i,1,ntriang)=xm(i,2)
                xyrot(i,2,ntriang)=xm(i,3)
                zrsum=zrsum+(verts(1,iv)*cosgam-
     &              verts(2,iv)*singam)*sinbet+verts(3,iv)*cosbet
              enddo
              zrot(ntriang)=zrsum/3.
c               if(abs(xm(1,4)-xm(2,4))+abs(xm(2,4)-xm(3,4)).gt.1.e-5)then
c               print *,ntriang,beta,gamma,cvec,(xm(i,4),i=1,3)
c               endif
              call inv_matrix(xm,xminv)
c               
c               for each component of normals, solve for interpolating
c               coefficients and convert them to values to apply to x,y,x
c               
              do ixyz=1,3
                do j=1,3
                  ci(j)=0.
                enddo
                do i=1,3
                  xyznorm=verts(ixyz,indnorm(i,ntriang))
                  do j=1,3
                    ci(j)=ci(j)+xminv(j,i)*xyznorm
                  enddo
                enddo
                coeff(1,ixyz,ntriang)=ci(1)
                coeff(2,ixyz,ntriang)=ci(2)*cosgam*cosbet+ci(3)*singam
                coeff(3,ixyz,ntriang)=-ci(2)*singam*cosbet+ci(3)*cosgam
                coeff(4,ixyz,ntriang)=-ci(2)*sinbet
              enddo
c               do j=1,3
c               print *,ntriang,j,(verts(i,indnorm(j,ntriang)),i=1,3)
c               enddo
            endif
          enddo
        endif
      enddo
      triszsq=2*areasum/ntriang
      trisize=sqrt(triszsq)
      volsize=max(xyzmax(1)-xyzmin(1),xyzmax(2)-xyzmin(2),
     &    xyzmax(3)-xyzmin(3))
      if(newfile)print *,'Maximum extent of volume is',volsize
      if(newfile)print *,'Volume =',volsumz
      secthick=zscal*xyscal
c       
c       8/10/11: computers are much faster these days, make the criteria big
      dropcrit=30.*secthick
      bigcrit=dropcrit
c       print *,'sec,big,drop',secthick,bigcrit,dropcrit
c       dropcrit=3.*trisize
c       bigcrit=3.*dropcrit
c       print *,'dropcrit',dropcrit
c       print *,nvert,npoly
c       
c       build edge index
c       
      if(newfile)print *,'Building edge index . . .'

      do ipoly=1,npoly
        irefst=istrpoly(ipoly)
        irefnd=irefst+ninpoly(ipoly)-1
c         
c         find other adjacent polygons based on vertices of first triangle
c         
        nadjpoly=1
        ipoladj(1)=ipoly
        do ipadj=1,npoly
          if(ipadj.ne.ipoly)then
            iscnst=istrpoly(ipadj)
            iscn=iscnst
            ifhit=0
            do while(iscn.lt.iscnst+ninpoly(ipadj).and.ifhit.eq.0)
              do i=1,3
                do j=1,3
                  if(indvert(i,irefst).eq.indvert(j,iscn))ifhit=1
                enddo
              enddo
              iscn=iscn+1
            enddo
            if(ifhit.eq.1)then
              nadjpoly=nadjpoly+1
              ipoladj(nadjpoly)=ipadj
            endif
          endif
        enddo
c         
c         look for triangles with matching edges
c         
        do iref=irefst,irefnd
          do iedge=1,3
            if(indedge(iedge,iref).le.0)then
              ind1=indvert(iedge,iref)
              ind2=indvert(mod(iedge,3)+1,iref)
              ind3=indvert(mod(iedge+1,3)+1,iref)
              do iadj=1,nadjpoly
                ipadj=ipoladj(iadj)
                iscnst=istrpoly(ipadj)
                iscnnd=istrpoly(ipadj)+ninpoly(ipadj)-1
                do iscn=iscnst,iscnnd
                  if(iscn.ne.iref.and.indedge(iedge,iref).le.0)then
                    do jedge=1,3
                      if(indedge(jedge,iscn).le.0)then
                        jnd1=indvert(jedge,iscn)
                        jnd2=indvert(mod(jedge,3)+1,iscn)
                        if((jnd1.eq.ind1.and.jnd2.eq.ind2).or.
     &                      (jnd1.eq.ind2.and.jnd2.eq.ind1))then
                          indedge(iedge,iref)=iscn
                          numedge(iedge,iref)=jedge
                          indedge(jedge,iscn)=iref
                          numedge(jedge,iscn)=iedge
c                           
c                           check for duplicates (left-over code)
c                           
                          if(ind3.eq.indvert(mod(jedge+1,3)+1,iscn))
     &                        print *,'DUPLICATE TRIANGLES:',iref,iscn
                          
                        endif
                      endif
                    enddo
                  endif
                enddo
              enddo
            endif
          enddo
        enddo
      enddo
      do i=1,ntriang
        do j=1,3
          if(indedge(j,i).eq.0)print *,'no adjacent',i,j
        enddo
      enddo
c       print *,'done with edge index'
c       
c       READ IN PORES HERE
c       
      ierr=getimodscat(ibase_obj,npt_in_obj,p_coord,obj_color,n_point,
     &    n_object)
      call scale_model(0)
      if(ierr.ne.0)then
        print *,'error',ierr,' reading pore positions, try again'
        go to 91
      endif
      do i=1,n_point
        object(i)=i
      enddo
      max_mod_obj=n_object
      npore=0
      if(newfile)print *,'Placing pores on surface . . .'
      do iobj=1,max_mod_obj
        ibase=ibase_obj(iobj)
        do ipt=1,npt_in_obj(iobj)
c           
c           get pore and scale it
c           
          ipnt=abs(object(ibase+ipt))
          do i=1,3
            portm(i)=p_coord(i,ipnt)*xyscal
          enddo
          zpore=zscal*portm(3)
          portm(3)=zpore
          itrmin=0
          dropmin=1.e10
          tolinside=0.001
c           
c           go through each polygon, looking in the ones close to
c           the z-plane of the pore
c           
          do ipoly=1,npoly
            inplane=.true.
            itri=istrpoly(ipoly)
            do while(inplane.and.dropmin.gt.0..and.
     &          itri.lt.istrpoly(ipoly)+ninpoly(ipoly))
              zdistmin=1.e10
              do j=1,3
                zdistmin=min(zdistmin,
     &              abs(zpore-verts(3,indvert(j,itri))))
              enddo
              inplane=zdistmin.le.bigcrit
              if(inplane)then
c                 
c                 rotate pore to coordinate system in which triangle is
c                 flat: do Z first and see if this is any improvement
c                 
                xprime=portm(1)*cgam(itri)-portm(2)*sgam(itri)
                prtz=xprime*sbet(itri) + zpore*cbet(itri)
                pdelz=abs(prtz-zrot(itri))
                if(pdelz.le.bigcrit.and.pdelz.le.dropmin)then
c                   
c                   get X and Y coordinates and see if inside the stored
c                   triangle coordinates
c                   
                  prtx=xprime*cbet(itri) - zpore*sbet(itri)
                  prty=portm(1)*sgam(itri)+portm(2)*cgam(itri)
                  if(inside(xyrot(1,1,itri),xyrot(1,2,itri),3,
     &                prtx,prty))then
c                     
c                     if inside, then distance is the Z distance.  If this
c                     really is a new minimum, back-rotate the position
c                     at the Z-level of the triangle to get pore position
c                     
                    if(pdelz.le.dropcrit.and.pdelz.lt.dropmin)then
                      xprime=prtx*cbet(itri)+zrot(itri)*sbet(itri)
                      ptmin(1)=xprime*cgam(itri)+prty*sgam(itri)
                      ptmin(2)=-xprime*sgam(itri)+prty*cgam(itri)
                      ptmin(3)=-prtx*sbet(itri)+zrot(itri)*cbet(itri)
                      itrmin=itri
                      dropmin=pdelz
                      itydrop=0
c                       distri=dist3d(portm,ptmin)
c                       print *,distri,pdelz,distri-pdelz,rot2diff
c                       i1=indvert(1,itri)
c                       i2=indvert(2,itri)
c                       i3=indvert(3,itri)
c                       do i=1,3
c                       vmax=max(verts(i,i1),verts(i,i2),verts(i,i3))
c                       vmin=min(verts(i,i1),verts(i,i2),verts(i,i3))
c                       if(vmin-ptmin(i).gt.1e-6.or.ptmin(i)-vmax.gt.1e-6)
c                       &                             print *,'outside',npore+1,i,ptmin(i),
c                       &                             vmin,vmax
c                       enddo
                    endif
                  endif
c                   
c                   now check distance from pore to each line segment
c                   
                  do iedge=1,3
                    ind1=indvert(iedge,itri)
                    ind2=indvert(mod(iedge,3)+1,itri)
                    sumnum=0.
                    sumden=0.
                    do i=1,3
                      delc=verts(i,ind2)-verts(i,ind1)
                      sumnum=sumnum+delc*(portm(i)-verts(i,ind1))
                      sumden=sumden+delc**2
                    enddo
                    t=max(0.,min(1.,sumnum/sumden))
                    do i=1,3
                      ptclose(i)=(1.-t)*verts(i,ind1)+t*verts(i,ind2)
                    enddo
c                     
c                     get distance, see if it passes criterion and is new
c                     minimum.  But if point was ostensibly inside this
c                     triangle and and close to an edge, snap it to edge
c                     to prevent points a little outside the triangle
c                     
                    distri=dist3d(portm,ptclose)
                    lockon=0
                    if(itrmin.eq.itri.and.itydrop.eq.0)then
                      if(dist3d(ptclose,ptmin).lt.0.001*trisize)then
                        lockon=1
c                         print *,'locking to edge',npore+1
                      endif
                    endif
                    if((distri.le.dropcrit.and.distri.lt.dropmin)
     &                  .or.lockon.eq.1)then
                      dropmin=distri
                      itrmin=itri
                      call copyvec(ptclose,ptmin)
                      itydrop=iedge
                    endif
                  enddo
                endif
              endif
              itri=itri+1
            enddo
          enddo
c           
c           done looking at triangles: if close enough to one, take the pore
c           
          if(itrmin.gt.0)then
            npore=npore+1
            call copyvec(ptmin,pore(1,npore))
            itypore(npore)=iobj
            itripore(npore)=itrmin
c             print *,npore,itydrop
          else
            print *,'did not drop',iobj,ipt,portm
          endif
        enddo
      enddo
      return
      end


      subroutine putpores(itypore,pore,porout,npore,xyscal,zscal)
      real*4 pore(3,*),porout(3,*)
      integer*4 itypore(*)
      character*80 modelout
      integer*4 in5
      common /nmsinput/ in5
      integer*4 getimodhead,getimodscales,putimodscat
c       
      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      ierr = getimodscales(ximscale, yimscale, zimscale)
      iobj=-1
      do ipore=1,npore
        if(itypore(ipore).ne.iobj)then
          if(iobj.ge.0)then
            ierr=putimodscat(iobj,porout)
            if(ierr.ne.0)print *,'error',ierr,' putting object', iobj
          endif
          ind=0
          iobj=itypore(ipore)
        endif
        ind=ind+1
c         
c         Scale back to index coords
        do i=1,3
          porout(i,ind)=pore(i,ipore)/xyscal
        enddo
        porout(3,ind)=porout(3,ind)/zscal
c         
c         Undo the effects of scale_model and get back to model coords
        porout(1,ind)=ximscale*porout(1,ind)+xofs
        porout(2,ind)=yimscale*porout(2,ind)+yofs
        porout(3,ind)=zimscale*porout(3,ind)+zofs
      enddo
      ierr=putimodscat(iobj,porout)
      if(ierr.ne.0)print *,'error',ierr,' putting object', iobj
      write(*,'(/,a,$)')
     &    'Filename to save model in now, Return not to: '
      read(in5,'(a)')modelout
      if(modelout.ne.' ')call writeimod(modelout)
      return
      end

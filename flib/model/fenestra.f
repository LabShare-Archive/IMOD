*       * * * * * * FENESTRA * * * * * * *
c       
c       This program will measure the sizes of holes through model objects
c       described by a series of contours, such as fenestrations in Golgi
c       cisternae.  It assumes that the objects have been modeled in Z planes
c       while the holes appear when looking at the X or Y face of the model.
c       For each model object, you must first create a separate object with
c       points specifying the locations of the holes.  In general, each hole
c       should be marked by two points, on the first and last Z planes
c       where the opening occurs.  These points should be placed fairly
c       carefully midway between the two contours and underneath the contour
c       that closes off the hole on the adjacent Z place.
c       
c       You may place the locator points in separate contours or in the same
c       contour, although in the latter case one must be particularly
c       careful to enter a pair of points for each hole.  A hole occurring on
c       only one Z plane may be specified by either one point or two, but if
c       you use one point you MUST place that point in a separate contour.
c       
c       
c       The program works by finding points on either side of the hole that
c       are closest to the line connecting the two locator points.  Given
c       all of these edge points, it then finds the plane that fits these
c       points best.  The normal to this plane is taken as the best
c       direction for viewing the hole.  An initial area is computed from
c       the polygon determined by projecting the points into that plane.
c       Then, the program reexamines the contours at the edge of the hole
c       and finds the points that encroach the most upon the hole, when
c       viewed from the best direction determined from the initial points.  
c       These points are used to compute a "minimized" area.  An equivalent
c       diameter is reported, based on this minimized area.
c       
c       The program can read a series of model files and place the output in
c       a single text file.
c       
c       Entries to the program:
c       
c       Name of output file for areas and diameters
c       
c       0 for short output of minimized area and equivalent diameter, or 1
c       for long output identifying the locator point contour number and
c       location, the projection angles, and tyhe initial area
c       
c       For output of the points from which area was computed, as contours
c       showing the outline of each hole, enter the number of a new object
c       for the initial points, and another number for an object for the
c       refined points.  Enter 0,0 for no output, or 0 and an object # for
c       just the refined points.  Make sure the same object numbers will
c       work for all of the models that will be processed.
c       
c       Model file name, or a blank line if there are no more models to
c       process.
c       
c       A list of the numbers of the objects to be analyzed (Ranges
c       may be used, e.g., 1-3,6,8)
c       
c       A list of the numbers of the corresponding objects with locator
c       points (again, ranges may be used)
c       
c       An identifier or type number for each object being analyzed.
c       
c       A criterion hole diameter for assigning the negative of this type
c       number; data for holes larger than this criterion will be output
c       with the negative of the identifier so that they can be analyzed
c       separately.  Enter zero or a very large number to have data output
c       as a single type.
c       
c       IF you selected output of contours, enter the name of the new model
c       file.
c       
c       The program will now loop back and ask for another input model to
c       analyze.  Enter a blank line when done.
c       
c       David Mastronarde, 9/14/97
c       
      include 'model.inc'
      parameter (limo=1000,limedg=10000,limcnd=1000)
      logical readw_or_imod
      character*80 modelfile,fileout
      character*1 xyz
      integer*4 ident(limo),iobjan(limo),iobjloc(limo)
      real*4 xyzlst(3),xyzlnd(3),tstrot(3),rmat(3,3),angles(3)
      real*4 centroid(3),candoff(limcnd),candist(limcnd)
      equivalence (xyzlst(1),xlst),(xyzlst(2),ylst),(xyzlst(3),zlst)
      equivalence (xyzlnd(1),xlnd),(xyzlnd(2),ylnd),(xyzlnd(3),zlnd)
      real*4 edgpt(3,limedg),candpt(3,limcnd),ptrot(3,limedg)
      real*4 edgdist(limedg),scales(3),angcen(3)
      integer*4 izedge(limedg),lredge(limedg),iptedge(limedg)
      integer*4 iobedge(limedg),iptcand(limcnd),iobcand(limcnd)
      integer*4 indord(limedg),icand(2)
c       
      horcritmin=5.
      offcritmin=5.
      sdhorcrit=1.
      sdoffcrit=1.
c       
      write(*,'(1x,a,$)')'Output file name for areas: '
      read(5,'(a)')fileout
      call dopen(1,fileout,'new','f')
c       
      write(*,'(1x,a,$)')'0 for short output or 1 for long output: '
      read(5,*)longout
      if(longout.eq.0)print *,'Columns will be identifier, minimized',
     &    ' area, and equivalent diameter'
      if(longout.ne.0)print *,'13 columns: identifier, object, contour'
     &    //', point, X, Y, Z, alpha, beta, gamma,',
     &    '  initial area, minimized area, equivalent diameter'
c       
      write(*,'(/,1x,a,/,a,/,a,$)') 'For diagnostic output of outline'
     &    //' contours, enter the # of a new object','   in which'//
     &    ' to place initial contours, and the # of a new object for'
     &    //' minimized',
     &    '   contours, or 0 for either object # for no output: '
      read(5,*)iobjout1,iobjout2
c       
10    write(*,'(1x,a,$)')'Model to read, or Return if done: '
      read(5,'(a)')modelfile
c       
      if(modelfile.eq.' ')then
        close(1)
        call exit(0)
      endif
      if(.not.readw_or_imod(modelfile))then
        print *,'Error reading model, try again'
        go to 10
      endif
      lrsort=1
      indx=1
      indy=3
      indz=2
      call getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
      if(xyscal.eq.1.e6)then
        xyscal=1
        print *,'Dimensions will be in pixels'
      else
        xyscal=1000.*xyscal
        print *,'Dimensions will be in nanometers'
      endif
c       
c       if data were flipped, just flip them back so contours are in Z planes
c       
      call flipyz(p_coord,n_point,ifflip)
      scales(1)=xyscal
      scales(2)=xyscal
      scales(3)=xyscal*zscal
14    print *,'Enter list of objects to analyze (Ranges OK)'
      call rdlist(5,iobjan,nobjan)
      print *,'Enter list of corresponding objects with locators',
     &    ' (Ranges OK)'
      call rdlist(5,iobjloc,nobjloc)
      if(nobjan.ne.nobjloc)then
        print *,'Number of objects must agree in the two lists'
        go to 14
      endif
c       
c       make sure contours have 1 or even # of points
c       
      ifok=1
      do iobjl=1,nobjloc
        icol=256-iobjloc(iobjl)
        do iobj=1,max_mod_obj
          if(obj_color(2,iobj).eq.icol.and.npt_in_obj(iobj).gt.2.and.
     &        mod(npt_in_obj(iobj),2).eq.1)then
            call objtocont(iobj,obj_color,imodobj,imodcont)
            print *,'Odd number points in object',imodobj,', contour',
     &          imodcont
            ifok=0
          endif
        enddo
      enddo
      if(ifok.eq.0)then
        print *,'Cannot proceed with this model'
        go to 10
      endif
c       
      write(*,'(1x,a,$)')'Identifier types for each object: '
      read(5,*)(ident(i),i=1,nobjan)
c       
      write(*,'(1x,a,/,a,$)')'Criterion hole diameter for negating'
     &    //'the type number on output',
     &    ' (or 0 not to split output into two types): '
      read(5,*)critneg
c       
c       
      do iobjl=1,nobjloc
        icoloc=256-iobjloc(iobjl)
        icolan=256-iobjan(iobjl)
        do iobj=1,max_mod_obj
          if(obj_color(2,iobj).eq.icoloc.and.npt_in_obj(iobj).gt.0)then
c             
c             got a contour with points; see how many locators
c             
            ntoan=max(1,npt_in_obj(iobj)/2)
            iptan=1
            do ian=1,ntoan
c               
c               get coordinates of lower and upper points of locator
c               

              ipnt1=object(ibase_obj(iobj)+iptan)
              ipnt2=object(ibase_obj(iobj)+
     &            min(iptan+1,npt_in_obj(iobj)))
              iptan=iptan+2
              if(p_coord(3,ipnt1).gt.p_coord(3,ipnt2))then
                itmp=ipnt1
                ipnt1=ipnt2
                ipnt2=itmp
              endif
              do i=1,3
                xyzlst(i)=p_coord(i,ipnt1)
                xyzlnd(i)=p_coord(i,ipnt2)
              enddo
              izst=nint(zlst)
              iznd=nint(zlnd)
              nzl=iznd-izst+1
c               
c               start edge list with 0.5 above and below locator pair
c               
              zlst=zlst-0.5
              zlnd=zlnd+0.5
              nedgpt=2
              edgpt(1,1)=xyzlst(indx)*scales(indx)
              edgpt(2,1)=xyzlst(indy)*scales(indy)
              edgpt(3,1)=xyzlst(indz)*scales(indz)
              edgpt(1,2)=xyzlnd(indx)*scales(indx)
              edgpt(2,2)=xyzlnd(indy)*scales(indy)
              edgpt(3,2)=xyzlnd(indz)*scales(indz)
              izedge(1)=izst-1
              lredge(1)=1
              izedge(2)=iznd+1
              lredge(2)=1
              edgdist(1)=0.5
              edgdist(2)=0.5
c               
c               work from top and bottom to the middle in Z
c               
              do izl=1,nzl
                ifrac=izl/2
                if(mod(izl,2).eq.0)ifrac=nzl-ifrac
                izcur=izst+ifrac
                frac=ifrac/max(1.,nzl-1.)
                xcur=(1.-frac)*xlst+frac*xlnd
                ycur=(1.-frac)*ylst+frac*ylnd
c                 
c                 make list of contours at this z level and their nearest pt
c                 
                nclist=0
                do jobj=1,max_mod_obj
                  ninobj=npt_in_obj(jobj)
                  if(obj_color(2,jobj).eq.icolan.and.ninobj.gt.0)then
                    ibase=ibase_obj(jobj)
                    izobj=nint(p_coord(3,object(ibase+1)))
                    if(izobj.eq.izcur)then
                      distmin=1.e10
                      do ipt=1,ninobj
                        ipnt=object(ibase+ipt)
                        dx=abs(p_coord(1,ipnt)-xcur)
                        if(dx.lt.distmin)then
                          dy=abs(p_coord(2,ipnt)-ycur)
                          if(dy.lt.distmin)then
                            dist=sqrt(dx**2+dy**2)
                            if(dist.lt.distmin)then
                              distmin=dist
                              iptmin=ipt
                            endif
                          endif
                        endif
                      enddo
c                       
c                       now find closest point on previous or next line
c                       segment, store candidate and distance on list
c                       
                      ipnt=object(ibase+iptmin)
                      iplas=object(ibase+indmap(iptmin-1,ninobj))
                      ipnex=object(ibase+indmap(iptmin+1,ninobj))
                      call point_to_line(xcur,ycur,p_coord(1,ipnt),
     &                    p_coord(2,ipnt),p_coord(1,iplas),
     &                    p_coord(2,iplas),tminlas,distlas)
                      call point_to_line(xcur,ycur,p_coord(1,ipnt),
     &                    p_coord(2,ipnt),p_coord(1,ipnex),
     &                    p_coord(2,ipnex),tminnex,distnex)
                      if(distlas.lt.distnex)then
                        distnex=distlas
                        ipnex=iplas
                        tminnex=tminlas
                      endif
                      nclist=nclist+1
                      candpt(1,nclist)=((1.-tminnex)*p_coord(indx,ipnt)
     &                    +tminnex*p_coord(indx,ipnex))*scales(indx)
                      candpt(2,nclist)=((1.-tminnex)*p_coord(indy,ipnt)
     &                    +tminnex*p_coord(indy,ipnex))*scales(indy)
                      candpt(3,nclist)=((1.-tminnex)*p_coord(indz,ipnt)
     &                    +tminnex*p_coord(indz,ipnex))*scales(indz)
                      candist(nclist)=sqrt(distnex)
                      indord(nclist)=nclist
                      iptcand(nclist)=iptmin
                      iobcand(nclist)=jobj
                    endif
                  endif
                enddo
c                 
c                 if only one point, skip it.   Order by distance.  If have
c                 6 points already, evaluate distance from current plane.
c                 
                if(nclist.ge.2)then
                  do i=1,nclist-1
                    do j=i+1,nclist
                      if(candist(indord(i)).gt.candist(indord(j)))then
                        itmp=indord(i)
                        indord(i)=indord(j)
                        indord(j)=itmp
                      endif
                    enddo
                  enddo
c                   
                  if(nedgpt.ge.6)then
                    call planefit(edgpt,nedgpt,centroid,rmat,ptrot)
                    distsum=0.
                    distsq=0.
                    distmax=0.
                    offsum=0.
                    offmax=0.
                    offsq=0.
                    do ie=1,nedgpt
                      distsum=distsum+edgdist(ie)
                      distsq=distsq+edgdist(ie)**2
                      distmax=max(distmax,edgdist(ie))
                      offsum=offsum+abs(ptrot(3,ie))
                      offsq=offsq+ptrot(3,ie)**2
                      offmax=max(offmax,abs(ptrot(3,ie)))
                    enddo
                    call sums_to_avgsd(distsum,distsq,nedgpt,distav,
     &                  distsd)
                    call sums_to_avgsd(offsum,offsq,nedgpt,offav,
     &                  offsd)
                    distcrit=max(horcritmin,sdhorcrit*distsd+distmax)
                    offcrit=xyscal*
     &                  max(offcritmin,sdoffcrit*offsd+offmax)
c                     
c                     count how many satisfy both criteria
c                     
                    nok=0
                    do ic=1,nclist
                      call rotpnt(candpt(1,ic),centroid,rmat,tstrot)
                      candoff(ic)=abs(tstrot(3))
                      if(candoff(ic).le.offcrit.and.
     &                    candist(ic).le.distcrit)nok=nok+1
                    enddo
c                     
c                     figure out how many to get that are ok and the rest
c                     that aren't, then scan through and pick up the first
c                     ones of each kind to satisfy those counts
c                     
                    ngetok=min(2,nok)
                    ngotok=0
                    ngetng=2-ngetok
                    ngotng=0
                    ngot=0
                    io=1
                    do while(ngotok.lt.ngetok.or.ngotng.lt.ngetng)
                      ic=indord(io)
                      if(candoff(ic).le.offcrit.and.
     &                    candist(ic).le.distcrit)then
                        if(ngotok.lt.ngetok)then
                          ngotok=ngotok+1
                          ngot=ngot+1
                          icand(ngot)=ic
                        endif
                      else
                        if(ngotng.lt.ngetng)then
                          ngotng=ngotng+1
                          ngot=ngot+1
                          icand(ngot)=ic
                        endif
                      endif
                      io=io+1
                    enddo
                  else
                    icand(1)=indord(1)
                    icand(2)=indord(2)
                  endif
c                   
c                   now add points to list, keeping track of iz & left/right
c                   
                  do ic=1,2
                    nedgpt=nedgpt+1
                    do i=1,3
                      edgpt(i,nedgpt)=candpt(i,icand(ic))
                    enddo
                    izedge(nedgpt)=izcur
c                     lredge(nedgpt)=0
c                     if(candpt(lrsort,icand(ic)).gt.
c                     &                   candpt(lrsort,icand(3-ic)))lredge(nedgpt)=1
                    iptedge(nedgpt)=iptcand(icand(ic))
                    iobedge(nedgpt)=iobcand(icand(ic))
                    edgdist(nedgpt)=candist(icand(ic))
                  enddo
                endif
              enddo
c               
c               got all the points at last.  Fit plane to them, then
c               determine left and right edges from rotated points
c               
              call planefit(edgpt,nedgpt,centroid,rmat,ptrot)
              do ie=2,nedgpt/2
                i1=2*ie-1
                i2=i1+1
                if(ptrot(1,i1).lt.ptrot(1,i2))then
                  lredge(i1)=0
                  lredge(i2)=1
                else
                  lredge(i1)=1
                  lredge(i2)=0
                endif
              enddo
c               
c               build index to points going up on right, down on left
c               
              nord=0
              do iz=izst-1,iznd+1
                do ie=1,nedgpt
                  if(izedge(ie).eq.iz.and.lredge(ie).eq.1)then
                    nord=nord+1
                    indord(nord)=ie
                  endif
                enddo
              enddo
              do iz=iznd,izst,-1
                do ie=1,nedgpt
                  if(izedge(ie).eq.iz.and.lredge(ie).eq.0)then
                    nord=nord+1
                    indord(nord)=ie
                  endif
                enddo
              enddo
c               
c               output objects if desired
c               
              call putoutline(edgpt,indord,nedgpt,scales,indx,indy,
     &            indz,iobjout1)
c               
              call calcarea(ptrot,indord,nedgpt,area)
c               
c               now adjust edge points to close in on hole
c               
              do ie=3,nedgpt
                io=ie+1
                if(mod(ie,2).eq.0)io=ie-1
                ninobj=npt_in_obj(iobedge(ie))
                ibase=ibase_obj(iobedge(ie))
                nscan=min(7,ninobj)
                nback=(nscan-1)/2
                nfor=nscan-1-nback
                distmin=1.e10
                do idif=-nback,nfor
                  ipt=indmap(iptedge(ie)+idif,ninobj)
                  ipnt=object(ibase+ipt)
                  candpt(1,1)=p_coord(indx,ipnt)*scales(indx)
                  candpt(2,1)=p_coord(indy,ipnt)*scales(indy)
                  candpt(3,1)=p_coord(indz,ipnt)*scales(indz)
                  call rotpnt(candpt,centroid,rmat,tstrot)
                  dist=(tstrot(1)-ptrot(1,io))**2+
     &                (tstrot(2)-ptrot(2,io))**2
                  if(dist.lt.distmin)then
                    distmin=dist
                    ipmin=ipnt
                  endif
                enddo
                edgpt(1,ie)=p_coord(indx,ipmin)*scales(indx)
                edgpt(2,ie)=p_coord(indy,ipmin)*scales(indy)
                edgpt(3,ie)=p_coord(indz,ipmin)*scales(indz)
                call rotpnt(edgpt(1,ie),centroid,rmat, ptrot(1,ie))
              enddo
c               
c               output objects if desired
c               
              call putoutline(edgpt,indord,nedgpt,scales,indx,indy,
     &            indz,iobjout2)
c               
              call calcarea(ptrot,indord,nedgpt,area2)
c$$$            c                 
c$$$            c                 now refine the angles by searching for maximum area
c$$$            c
c$$$            call icalc_angles(angcen,rmat)
c$$$            areamax=area2
c$$$            alphamax=angcen(1)
c$$$            betamax=angcen(2)
c$$$            angles(3)=angcen(3)
c$$$            delang=5.
c$$$            do isrch=1,2
c$$$            do idela=-5,5
c$$$            do idelb=-5,5
c$$$            angles(1)=angcen(1)+idela*delang
c$$$            angles(2)=angcen(2)+idelb*delang
c$$$            call icalc_matrix(angles,rmat)
c$$$            do ie=1,nedgpt
c$$$            if(ie.gt.2)then
c$$$            io=ie+1
c$$$            if(mod(ie,2).eq.0)io=ie-1
c$$$            ninobj=npt_in_obj(iobedge(ie))
c$$$            ibase=ibase_obj(iobedge(ie))
c$$$            nscan=min(7,ninobj)
c$$$            nback=(nscan-1)/2
c$$$            nfor=nscan-1-nback
c$$$            distmin=1.e10
c$$$            do idif=-nback,nfor
c$$$            ipt=indmap(iptedge(ie)+idif,ninobj)
c$$$            ipnt=object(ibase+ipt)
c$$$            candpt(1,1)=p_coord(indx,ipnt)*scales(indx)
c$$$            candpt(2,1)=p_coord(indy,ipnt)*scales(indy)
c$$$            candpt(3,1)=p_coord(indz,ipnt)*scales(indz)
c$$$            call rotpnt(candpt,centroid,rmat,tstrot)
c$$$            dist=(tstrot(1)-ptrot(1,io))**2+
c$$$            &                               (tstrot(2)-ptrot(2,io))**2
c$$$            if(dist.lt.distmin)then
c$$$            distmin=dist
c$$$            ipmin=ipnt
c$$$            endif
c$$$            enddo
c$$$            edgpt(1,ie)=p_coord(indx,ipmin)*scales(indx)
c$$$            edgpt(2,ie)=p_coord(indy,ipmin)*scales(indy)
c$$$            edgpt(3,ie)=p_coord(indz,ipmin)*scales(indz)
c$$$            else
c$$$            call rotpnt(edgpt(1,ie),centroid,rmat,
c$$$            &                             ptrot(1,ie))
c$$$            endif
c$$$            enddo
c$$$            call calcarea(ptrot,indord,nedgpt,areatst)
c$$$            if(areatst.gt.areamax)then
c$$$            areamax=areatst
c$$$            alphamax=angles(1)
c$$$            betamax=angles(2)
c$$$            endif
c$$$            enddo
c$$$            enddo
c$$$            angcen(1)=alphamax
c$$$            angcen(2)=betamax
c$$$            delang=delang/5.
c$$$            enddo
c               
c               OUTPUT
c               
              diam=sqrt(4*area2/3.14159)
              negtype=1
              if(diam.ge.critneg.and.critneg.gt.0.)negtype=-1
              if(longout.eq.0)then
                write(1,102)negtype*ident(iobjl),area2,diam
102             format(i5,f9.1,f9.2)
              else
                call icalc_angles(angles,rmat)
                call objtocont(iobj,obj_color,imodobj,imodcont)
                write(1,103)negtype*ident(iobjl),imodobj,imodcont,iptan,
     &              nint((xlst+xlnd)/2-xofs),nint((ylst+ylnd)/2-yofs),
     &              nint((zlst+zlnd)/2-zofs),(angles(i),i=1,3),
     &              area,area2,diam
103             format(i5,i3,i4,i4,3i6,3f6.1,2f8.1,f7.2)
              endif
            enddo
          endif
        enddo
      enddo
c       
      if(iobjout1.ne.0.or.iobjout2.ne.0)then
        write(*,'(1x,a,$)')'Filename for output model: '
        read(5,'(a)')fileout
        call flipyz(p_coord,n_point,ifflip)
        call write_wmod(fileout)
      endif
      go to 10
      end


      subroutine point_to_line(x0,y0,x1,y1,x2,y2,tmin,distsq)
      tmin=((x0-x1)*(x2-x1)+(y0-y1)*(y2-y1))/((x2-x1)**2+(y2-y1)**2)
      tmin=max(0.,min(1.,tmin))
      distsq=(x1+tmin*(x2-x1)-x0)**2+(y1+tmin*(y2-y1)-y0)**2
      return
      end

      subroutine calcarea(ptrot,indord,nedgpt,area)
      real*4 ptrot(3,*)
      integer*4 indord(*)
      area=0.
      do iord=1,nedgpt
        ie=indord(iord)
        iep=indord(indmap(iord+1,nedgpt))
        area=area+0.5*(ptrot(2,iep)+ptrot(2,ie))*
     &      (ptrot(1,iep)-ptrot(1,ie))
      enddo
      area=abs(area)
      return
      end

      subroutine putoutline(edgpt,indord,nedgpt,scales,indx,indy,
     &    indz,iobjout1)
      include 'model.inc'
      real*4 edgpt(3,*),scales(3)
      integer*4 indord(*)
      if(iobjout1.gt.0)then
        n_object=n_object+1
        max_mod_obj=max_mod_obj+1
        obj_color(1,n_object)=1
        obj_color(2,n_object)=256-iobjout1
        ibase_obj(n_object)=ibase_free
        npt_in_obj(n_object)=nedgpt
        do iord=1,nedgpt
          ie=indord(iord)
          n_point=n_point+1
          object(ibase_free+iord)=n_point
          p_coord(indx,n_point)=edgpt(1,ie)/scales(indx)
          p_coord(indy,n_point)=edgpt(2,ie)/scales(indy)
          p_coord(indz,n_point)=edgpt(3,ie)/scales(indz)
          pt_label(n_point)=0
        enddo
        ibase_free=ibase_free+nedgpt
      endif
      return
      end

      subroutine flipyz(p_coord,n_point,ifflip)
      real*4 p_coord(3,*)
      if(ifflip.ne.0)then
        do i=1,n_point
          tmp=p_coord(2,i)
          p_coord(2,i)=p_coord(3,i)
          p_coord(3,i)=tmp
        enddo
      endif
      return
      end

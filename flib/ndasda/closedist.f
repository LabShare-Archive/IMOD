c       CLOSEDIST produces a series of "graphs" of spatial density
c       of neighboring items as a function of radial distance
c       from an average reference item.  The reference points and the
c       neighboring points may be of various types or sets of types, as
c       defined separately for each graph.  
c       DELR is the bin width (in distance units) desired for the graphs
c       NBINS is the number of bins for each graph
c       NGRAPH is the number of graphs
c       NREFTYP is the number of types for reference points for each graph
c       NNEIGHTYP is the number of types for neighbor points for each graph
c       ITYPREF(I,J) is the Ith reference type for the Jth graph
c       ITYPNEIGH(I,J) is the Ith neighbor type for the Jth graph
c       GRAPHS(I,J) is returned with the density at the Ith bin of Jth graph
c       FRACSUM(I,J) is returned with total area contributing to that bin
c       XMT, YMT, ZMT are arrays with point or line coordinates
c       INDSTRT is the starting index for each object (contour)
c       NPNTOBJ is the number of point in each object
c       NMT is the number of those objects
c       ICOLOR has the type for eavh object
c       POWER is the exponent for distance in the volume component
c       POWERGRF is the array of power values for each graph
c       LIMFIT is number of points to fit to lines over (Z based lines)
c       WINMIN, WINMAX are window limits for saving connectors
c       NINWIN  returned with # of distances in window saved in arrays
c       IOBJWIN has the object or negative surface? number of items in window
c       NOBJWIN is number of objects in that list (pass in - to find end sep)
c       IREFFLAG and NEIGHFLAG have values for the object types (closed, etc)
c       XYZEND  has coordinates of endpoints
c       ENDSEP is returned with end separation
c       SAMPLEN sampling length for lines
c       IFCLOSEG to measure to closest point on segment
c       IFSCATSURF  flag to measure scattered points from surface
c       ZGAPST, ZAPGND are starting and ending Z of gaps
c       NGAPS is the number of gaps
c       XYSCAL, ZSCAL are the pixel size and Z scaling applied to the model
c       MANYRANDOM is a flag to suppress output if many random sets being done
c       ONLYSHIFTED is logical to compute only agains sucessfully shifted items
c       NEARESTONLY is a flag to count nearest neighbors only
c       
c       $Id$

      subroutine closedist(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &    delr,nbins, ngraph,nreftyp,nneightyp,itypref, itypneigh,
     &    power,limfit,winmin,winmax,ninwin,graphs,fracsum, iobjwin,
     &    nobjwin,iobjmod,xyzend,endsep,samplen, ifcloseg,
     &    ifscatsurf,irefflag,neighflag,xyscal,zscal,powergrf,zgapst,
     &    zgapnd,ngaps,manyrandom,onlyshifted, nearestOnly)
      include 'mtk.inc'
      parameter (limgraphs=50,limbins=1001,limwobj=30000,
     &    limtyp=50,itypall=999,limxyz=50000)
      parameter (limind=limverts*6,limbinsave=limind*2,limtmp=1000)
      parameter (limsizes=limxyz)
      real*4 xmt(*),ymt(*),zmt(*)
      integer*4 indstrt(*),npntobj(*),iobjmod(*)
      real*4 graphs(limbins,*),endsep(*),xyzend(3,*)
      integer*4 icolor(*)                       !types of sample points
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*),iobjwin(*)
      logical onlyshifted, nearestOnly
      integer*4 igraphref(limgraphs),nrefobj(limgraphs),nclose(2,2), minBin(limgraphs)
      real*4 aa(limxyz),bb(limxyz),cc(limxyz),dd(limxyz)
      real*4 xmin(limxyz),xmax(limxyz),ymin(limxyz),ymax(limxyz)
      real*4 zmin(limxyz),zmax(limxyz)
      real*4 glbxmin(limwobj),glbymin(limwobj),glbzmin(limwobj)
      real*4 glbxmax(limwobj),glbymax(limwobj),glbzmax(limwobj)
      real*4 xtmp(limtmp),ytmp(limtmp),ztmp(limtmp)
      real*4 xtmpmin(limtmp),ytmpmin(limtmp),ztmpmin(limtmp)
      real*4 xtmpmax(limtmp),ytmpmax(limtmp),ztmpmax(limtmp)
c       
      integer*4 modind(limind)
      real*4 fracsum(limbins,*),powergrf(*)
      real*4 zgapst(*),zgapnd(*)
      logical*1 neighpt(limgraphs,limwobj)
      logical farup,fardown,findsep,zbasedlines,usebinsave,refdone
      integer*2 ibinsave(limbinsave)
      equivalence(ibinsave,modind)
      integer*4 getimodverts,getimodsizes
      integer*4 nsizeloaded/0/
      integer*4 indsize(limwobj),nextsize(limeshobj)
      integer*4 icolorsize(limeshobj)
      real*4 sizes(limsizes)
      save nsizeloaded,indsize,nextsize,icolorsize,sizes
c       
      findsep=winmin.lt.winmax.and.nobjwin.lt.0
      if(findsep)then
        do ii=1,-nobjwin
          endsep(ii)=-1.
        enddo
      endif
      nsizeloaded=0
      ifreesize=1
      zbasedlines=irefflag.eq.1.and.neighflag.eq.1.and.
     &    samplen.le.0..and.limfit.ge.2
      nbinsave=nmt+(nmt-1)*(nmt-2)/2
      usebinsave=irefflag.eq.1.and.neighflag.eq.1.and.samplen.le.0.
     &    .and.nbinsave.lt.limbinsave
c       
c       Analyze which points are valid neighbor points for each graph
c       
      do ii=1,nmt
        needed=0
        indref=indstrt(ii)
        glbxmin(ii)=1.e10
        glbxmax(ii)=-1.e10
        glbymin(ii)=1.e10
        glbymax(ii)=-1.e10
        glbzmin(ii)=1.e10
        glbzmax(ii)=-1.e10
        do jj=1,ngraph
          neighpt(jj,ii)=.false.
          do kk=1,nneightyp(jj)
            if(itypneigh(kk,jj).eq.itypall .or.
     &          itypneigh(kk,jj).eq.icolor(ii)) neighpt(jj,ii)=.true.
          enddo
          if(neighpt(jj,ii))then
            needed=1
            needflag=neighflag
          endif
          do kk=1,nreftyp(jj)
            if(itypref(kk,jj).eq.itypall .or.
     &          itypref(kk,jj).eq.icolor(ii)) then
              needed=1
              needflag=irefflag
            endif
          enddo
        enddo
        if(needed.ne.0)then
          if(needflag.eq.1)then
c             
c             the old code for line to line fits, assuming progression in Z
c             precompute all the needed line fits for this MT
c             
            nfitref=2
            if(zbasedlines)nfitref=min(limfit,npntobj(ii))
            limref=indref+npntobj(ii)-nfitref
            do iref=indref,limref
              if(zbasedlines)then
                zs1=zmt(iref)
                zn1=zmt(iref+nfitref-1)
                call linfit(zmt(iref),xmt(iref),nfitref,aa(iref),bb(iref))
                call linfit(zmt(iref),ymt(iref),nfitref,cc(iref),dd(iref))
                xs1=aa(iref)*zs1+bb(iref)
                ys1=cc(iref)*zs1+dd(iref)
                xn1=aa(iref)*zn1+bb(iref)
                yn1=cc(iref)*zn1+dd(iref)
              else
                xs1=xmt(iref)
                xn1=xmt(iref+1)
                ys1=ymt(iref)
                yn1=ymt(iref+1)
                zs1=zmt(iref)
                zn1=zmt(iref+1)
              endif
              xmax(iref)=max(xs1,xn1)
              ymax(iref)=max(ys1,yn1)
              zmax(iref)=max(zs1,zn1)
              xmin(iref)=min(xs1,xn1)
              ymin(iref)=min(ys1,yn1)
              zmin(iref)=min(zs1,zn1)
              glbxmin(ii)=min(glbxmin(ii),xmin(iref))
              glbxmax(ii)=max(glbxmax(ii),xmax(iref))
              glbymin(ii)=min(glbymin(ii),ymin(iref))
              glbymax(ii)=max(glbymax(ii),ymax(iref))
              glbzmin(ii)=min(glbzmin(ii),zmin(iref))
              glbzmax(ii)=max(glbzmax(ii),zmax(iref))
            enddo
          elseif(needflag.eq.2)then
c             
c             scattered points; first load sizes if needed
c             
            limref=indref+npntobj(ii)-1
            if(ifscatsurf.ne.0)then
c               
c               see if sizes are loaded yet for this obj; if not load them
c               
              ifloaded=0
              i=1
              do while(i.le.nsizeloaded.and.ifloaded.eq.0)
                if(abs(icolor(ii)).eq.icolorsize(i))then
                  ifloaded=1
                  indsize(ii)=nextsize(i)
                  nextsize(i)=nextsize(i)+npntobj(ii)
                endif
                i=i+1
              enddo
              if(ifloaded.eq.0)then
                nsizeloaded=nsizeloaded+1
                icolorsize(nsizeloaded)=abs(icolor(ii))
                ierr = getimodsizes(abs(icolor(ii)), sizes(ifreesize),
     &              limsizes+1-ifreesize,nloaded)
                if(ierr.ne.0)then
                  print *,'Too many point sizes for arrays'
                  return
                endif
                indsize(ii)=ifreesize
                nextsize(nsizeloaded)=ifreesize+npntobj(ii)
                ifreesize=ifreesize+nloaded
              endif
            endif
c             
c             scale the sizes, set bounding boxes
c             
            do iref=indref,limref
              sizetmp=0.
              if(ifscatsurf.ne.0)then
                is=indsize(ii)+iref-indref
                sizes(is)=sizes(is)*xyscal
                sizetmp=sizes(is)
              endif
              xmin(iref)=xmt(iref)-sizetmp
              ymin(iref)=ymt(iref)-sizetmp
              zmin(iref)=zmt(iref)-sizetmp
              xmax(iref)=xmt(iref)+sizetmp
              ymax(iref)=ymt(iref)+sizetmp
              zmax(iref)=zmt(iref)+sizetmp
              glbxmin(ii)=min(glbxmin(ii),xmin(iref))
              glbxmax(ii)=max(glbxmax(ii),xmax(iref))
              glbymin(ii)=min(glbymin(ii),ymin(iref))
              glbymax(ii)=max(glbymax(ii),ymax(iref))
              glbzmin(ii)=min(glbzmin(ii),zmin(iref))
              glbzmax(ii)=max(glbzmax(ii),zmax(iref))
            enddo
c             
          else
            print *,'Improper object type; cannot proceed'
            return
          endif
          if(findsep)then
            do jj=1,-nobjwin
              if(iobjwin(jj).eq.iobjmod(ii))endsep(jj)=1.e10
            enddo
          endif
        endif
      enddo
c       
c       Now see if all objects already have meshes loaded
c       
      if(irefflag.eq.4.or.neighflag.eq.4)then
        ifany=0
        if(irefflag.eq.4)then
          do jj=1,ngraph
            do kk=1,nreftyp(jj)
              ifloaded=0
              do i=1,nmeshloaded
                if(iobjmesh(i).eq.abs(itypref(kk,jj)))ifloaded=1
              enddo
              if(ifloaded.eq.0)ifany=1
            enddo
          enddo
        endif
        if(neighflag.eq.4)then
          do jj=1,ngraph
            do kk=1,nneightyp(jj)
              ifloaded=0
              do i=1,nmeshloaded
                if(iobjmesh(i).eq.abs(itypneigh(kk,jj)))ifloaded=1
              enddo
              if(ifloaded.eq.0)ifany=1
            enddo
          enddo
        endif
c         
c         if not, need to load them all over again
c         
        if(ifany.ne.0)then
          if(manyrandom.eq.0)print *,'Processing meshes...'
          nmeshloaded=0
          nverts=0
          ntriang=0
          npoly=0
          nsurf=0
          ncont=0
          do jj=1,ngraph
            if(irefflag.eq.4)then
              do kk=1,nreftyp(jj)
                ifloaded=0
                imodobj=abs(itypref(kk,jj))
                do i=1,nmeshloaded
                  if(iobjmesh(i).eq.imodobj)ifloaded=1
                enddo
                if(ifloaded.eq.0)then
                  call process_mesh(imodobj,xyscal,zscal,modind,
     &                zgapst,zgapnd,ngaps,iferr,manyrandom)
                  if(iferr.ne.0)go to 99
                endif
              enddo
            endif
            if(neighflag.eq.4)then
              do kk=1,nneightyp(jj)
                ifloaded=0
                imodobj=abs(itypneigh(kk,jj))
                do i=1,nmeshloaded
                  if(iobjmesh(i).eq.imodobj)ifloaded=1
                enddo
                if(ifloaded.eq.0)then
                  call process_mesh(imodobj,xyscal,zscal,modind,
     &                zgapst,zgapnd,ngaps,iferr,manyrandom)
                  if(iferr.ne.0)go to 99
                endif
              enddo
            endif
          enddo
        endif
c         
c         set neighpt for graphs a mesh is needed in
c         
        if(neighflag.eq.4)then
          do ii=1,nmeshloaded
            do jj=1,ngraph
              neighpt(jj,ii)=.false.
              do kk=1,nneightyp(jj)
                if(abs(itypneigh(kk,jj)).eq.iobjmesh(ii))
     &              neighpt(jj,ii)=.true.
              enddo
            enddo
          enddo
        endif
        nbinsave=nsurf+(nsurf-1)*(nsurf-2)/2
        usebinsave=irefflag.eq.4.and.neighflag.eq.4
     &      .and.nbinsave.le.limbinsave
      endif
      if(manyrandom.eq.0)print *,'Analyzing closest distances...'
c       
c       call setfrac(fracsum(1,1),power,delr,nbins)
c       
c       zero out the graphs 
c       
      do ii=1,ngraph
        do jj=1,nbins
          graphs(jj,ii)=0.
          fracsum(jj,ii)=0.
        enddo
        nrefobj(ii)=0
        minBin(ii) = 0
      enddo
      if(usebinsave)then
        do i=1,nbinsave
          ibinsave(i)=-1
        enddo
      endif
      distlim=delr*nbins
      ninwin=0
      ninwtot=0
      if(.not.findsep)nobjwin=0
      indfree=indstrt(nmt)+npntobj(nmt)
      angsum=0.
      angsmsq=0.
      nrefwin=0
      nneighwin=0
      noendwin=1-nobjwin
      ndoing=0
      nclose(1,1)=0
      nclose(1,2)=0
      nclose(2,1)=0
      nclose(2,2)=0
      iorefend=nmt
      if(irefflag.eq.4)iorefend=nmeshloaded
c       
c       loop through each sample point considered as reference
c       
      do iobjref=1,iorefend
c         
c         first make list of graphs the reference point is needed in
c         
        needref=0
        do jj=1,ngraph
          needed=0
          if(irefflag.lt.4)then
            do kk=1,nreftyp(jj)
              if(itypref(kk,jj).eq.itypall .or.
     &            itypref(kk,jj).eq.icolor(iobjref)) needed=1
            enddo
          else
            do kk=1,nreftyp(jj)
              if(abs(itypref(kk,jj)).eq.iobjmesh(iobjref)) needed=1
            enddo
          endif
          if(needed.gt.0)then
            needref=needref+1
            igraphref(needref)=jj
            if (.not. nearestOnly) nrefobj(jj)=nrefobj(jj)+1
          endif
        enddo
c         
c         get shift index if relevant, cancel needref for failed lines
c         
        if(onlyshifted)then
          ishift=indexshift(iobjref,irefflag,icolor,npntobj,nmt)
          if(irefflag.eq.1.and.ishift.gt.0)then
            if(.not.shifted(ishift))needref=0
          endif
        endif
c         
        if(needref.gt.0.and.irefflag.lt.4)then
c           
          refdone=.false.
          indref=indstrt(iobjref)
          limref=indref+npntobj(iobjref)-1
          if(zbasedlines)then
            nfitref=min(limfit,npntobj(iobjref))
            limref=indref+npntobj(iobjref)-nfitref
          endif
c           
c           set up if doing samples along the line
c           
          iref=indref
          sampfrac=0.
          if(irefflag.eq.1.and.samplen.gt.0.)call get_next_sample
     &        (xmt,ymt,zmt,iref,sampfrac,limref,samplen,ifcloseg,
     &        xtmp,ytmp, ztmp,ntmp,irefend,fracend,seglen,
     &        xtmpmin,xtmpmax,ytmpmin,ytmpmax,ztmpmin, ztmpmax,
     &        refxmin,refxmax,refymin,refymax,refzmin,refzmax)

          if(irefflag.eq.1.and.manyrandom.eq.0)then
            ndoing=ndoing+1
            write(*,'(a,i5,$)')char(13)//'Doing line #',ndoing
            call flush(6)
          endif

          do while(.not.refdone)
c             
c             if doing lines
c             
            ifskipfail=0
            if(irefflag.eq.1)then
c               
c               if doing whole lines: use global min/maxes
c               
              if(samplen.le.0.)then
                refxmin=glbxmin(iobjref)
                refymin=glbymin(iobjref)
                refzmin=glbzmin(iobjref)
                refxmax=glbxmax(iobjref)
                refymax=glbymax(iobjref)
                refzmax=glbzmax(iobjref)
c                 
c                 if not z based, copy line and xmin, etc, to tmp
c                 
                if(.not.zbasedlines)then
                  do iref=indref,limref
                    iout=iref+1-indref
                    xtmp(iout)=xmt(iref)
                    ytmp(iout)=ymt(iref)
                    ztmp(iout)=zmt(iref)
                    xtmpmin(iout)=xmin(iref)
                    ytmpmin(iout)=ymin(iref)
                    ztmpmin(iout)=zmin(iref)
                    xtmpmax(iout)=xmax(iref)
                    ytmpmax(iout)=ymax(iref)
                    ztmpmax(iout)=zmax(iref)
                  enddo     
                  ntmp=iout
                endif
              else
c                 
c                 doing sample segments: should be all set up 
c                 
              endif
              sizeref=0.
            else
c               
c               doing points, set min's and max's
c               
              refxmin=xmin(iref)
              refymin=ymin(iref)
              refzmin=zmin(iref)
              refxmax=xmax(iref)
              refymax=ymax(iref)
              refzmax=zmax(iref)
              sizeref=0.
              xtmp(1)=xmt(iref)
              ytmp(1)=ymt(iref)
              ztmp(1)=zmt(iref)
              ntmp=1
              if(ifscatsurf.ne.0)then
                is=indsize(iobjref)+iref-indref
                sizeref=sizes(is)
              endif
              if(neighflag.ne.2.and.manyrandom.eq.0)then
                ndoing=ndoing+1
                write(*,'(a,i7,$)')char(13)//'Doing point #',ndoing
                call flush(6)
              endif
              if(onlyshifted.and.ishift.gt.0)then
                if(.not.shifted(ishift))ifskipfail=1
              endif
            endif
c             
            if(neighflag.lt.4.and.ifskipfail.eq.0)then

              do iobjneigh=1,nmt
                isneigh=0
                do ineed=1,needref
                  jj=igraphref(ineed)
                  if(neighpt(jj,iobjneigh))isneigh=1
                enddo
c                 
c                 cancel failed line neighbors if appropriate
c                 
                if(onlyshifted)then
                  jshift=indexshift(iobjneigh,neighflag,icolor,npntobj,nmt)
                  if(neighflag.eq.1.and.jshift.gt.0)then
                    if(.not.shifted(jshift))isneigh=0
                  endif
                endif
                
                if((iobjneigh.ne.iobjref.or.irefflag.eq.2)
     &              .and.isneigh.ne.0)then
                  if(usebinsave)then
                    iobjmax=max(iobjref,iobjneigh)
                    itriang=(iobjmax-1)*(iobjmax-2)/2+min(iobjref,iobjneigh)
                    ibin=ibinsave(itriang)
                  endif

                  if(iobjref.lt.iobjneigh.or.ibin.eq.-1.or.
     &                .not.usebinsave)then
                    ibin=0
                    distmin=1.01*distlim
                    dminsq=distmin**2
                    indneigh=indstrt(iobjneigh)
                    limneigh=indneigh+npntobj(iobjneigh)-1
                    if(neighflag.eq.1)limneigh=limneigh-1
c                     
c                     check against global limits
c                     
                    if(refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &                  glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &                  refymin-glbymax(iobjneigh).lt.distmin.and.
     &                  glbymin(iobjneigh)-refymax.lt.distmin.and.
     &                  refzmin-glbzmax(iobjneigh).lt.distmin.and.
     &                  glbzmin(iobjneigh)-refzmax.lt.distmin)then
                      if(zbasedlines)then
c                         
c                         OLD CODE FOR Z-BASED LINES
c                         
                        nfitneigh=min(limfit,npntobj(iobjneigh))
                        limneigh=indneigh+npntobj(iobjneigh)-nfitneigh
                        minneigh=indneigh
                        do iref=indref,limref
                          zs1=zmt(iref)
                          zn1=zmt(iref+nfitref-1)
                          minstrt=minneigh
                          ineigh=minstrt
                          idir=-1
                          do while(ineigh.le.limneigh)
                            zs2=zmt(ineigh)
                            zn2=zmt(ineigh+nfitneigh-1)
                            fardown=zs1-zn2.gt.distmin
                            farup=zs2-zn1.gt.distmin
                            if(.not.(fardown.or.farup))then
                              if(xmin(ineigh)-xmax(iref).lt.distmin.and.
     &                            xmin(iref)-xmax(ineigh).lt.distmin)then
                                if(ymin(ineigh)-ymax(iref).lt.distmin.and.
     &                              ymin(iref)-ymax(ineigh).lt.distmin)then
                                  call segment_dist_z(aa(iref),bb(iref),
     &                                cc(iref),dd(iref),aa(ineigh),bb(ineigh),
     &                                cc(ineigh),dd(ineigh),zs1,zn1,zs2,zn2,
     &                                z1,z2,dsqr)
c                                   print *,dsqr
                                  if(dsqr.lt.dminsq)then
                                    dminsq=dsqr
                                    distmin=sqrt(dsqr)
                                    minneigh=ineigh
                                    if(distmin.lt.winmax.and.
     &                                  distmin.ge.winmin)then
                                      z1min=z1
                                      z2min=z2
                                      x1min=z1*aa(iref)+bb(iref)
                                      y1min=z1*cc(iref)+dd(iref)
                                      x2min=z2*aa(ineigh)+bb(ineigh)
                                      y2min=z2*cc(ineigh)+dd(ineigh)
                                      cosang=(1.+aa(iref)*aa(ineigh)+
     &                                    cc(iref)*cc(ineigh))/sqrt(
     &                                    (1.+aa(iref)**2+cc(iref)**2)*
     &                                    (1.+aa(ineigh)**2+cc(ineigh)**2))
                                    endif
                                  endif
                                endif
                              endif
                              ineigh=ineigh+idir
                            else
                              if(idir.eq.-1.and.fardown)then
                                ineigh=indneigh-1
                              elseif(farup)then
                                ineigh=limneigh+1
                              else
                                ineigh=ineigh+idir
                              endif
                              if(fardown)minneigh=max(minneigh,ineigh+1)
                            endif
                            if(ineigh.lt.indneigh)then
                              idir=1
                              ineigh=minstrt+1
                            endif
c                             write(*,'(5f10.5)')zs1,zn1,zs2,zn2,distmin
                          enddo
                        enddo
c                         
c                         NEW LINES TO LINES: seek global minimum
c                         
                      elseif(neighflag.eq.1.and.ntmp.gt.1)then
                        do itmp=1,ntmp-1
                          do ineigh=indneigh,limneigh
                            if(xmin(ineigh)-xtmpmax(itmp).lt.distmin.and.
     &                          xtmpmin(itmp)-xmax(ineigh).lt.distmin.and.
     &                          ymin(ineigh)-ytmpmax(itmp).lt.distmin.and.
     &                          ytmpmin(itmp)-ymax(ineigh).lt.distmin.and.
     &                          zmin(ineigh)-ztmpmax(itmp).lt.distmin.and.
     &                          ztmpmin(itmp)-zmax(ineigh).lt.distmin)then
                              call segment_dist(xtmp(itmp),ytmp(itmp),
     &                            ztmp(itmp),xtmp(itmp+1),ytmp(itmp+1),
     &                            ztmp(itmp+1), xmt(ineigh),ymt(ineigh),
     &                            zmt(ineigh), xmt(ineigh+1),
     &                            ymt(ineigh+1),zmt(ineigh+1),tref,
     &                            tneigh,dsqr)
                              if(dsqr.lt.dminsq)then
                                dminsq=dsqr
                                distmin=sqrt(dsqr)
                                if(distmin.lt.winmax.and.
     &                              distmin.ge.winmin)then
                                  dx1=xtmp(itmp+1)-xtmp(itmp)
                                  dy1=ytmp(itmp+1)-ytmp(itmp)
                                  dz1=ztmp(itmp+1)-ztmp(itmp)
                                  dx2=xmt(ineigh+1)-xmt(ineigh)
                                  dy2=ymt(ineigh+1)-ymt(ineigh)
                                  dz2=zmt(ineigh+1)-zmt(ineigh)
                                  cosang=(dx1*dx2+dy1*dy2+dz1*dz2)/
     &                                sqrt((dx1**2+dy1**2+dz1**2)*
     &                                (dx2**2+dy2**2+dz2**2))
                                  x1min=xtmp(itmp)+tref*dx1
                                  y1min=ytmp(itmp)+tref*dy1
                                  z1min=ztmp(itmp)+tref*dz1
                                  x2min=xmt(ineigh)+tneigh*dx2
                                  y2min=ymt(ineigh)+tneigh*dy2
                                  z2min=zmt(ineigh)+tneigh*dz2
                                endif
                              endif
                            endif
                          enddo
                        enddo
c                         
c                         SINGLE POINTS TO LINES: again, global minimum
c                         
                      elseif(neighflag.eq.1)then
                        do ineigh=indneigh,limneigh
                          if(xmin(ineigh)-refxmax.lt.distmin.and.
     &                        refxmin-xmax(ineigh).lt.distmin.and.
     &                        ymin(ineigh)-refymax.lt.distmin.and.
     &                        refymin-ymax(ineigh).lt.distmin.and.
     &                        zmin(ineigh)-refzmax.lt.distmin.and.
     &                        refzmin-zmax(ineigh).lt.distmin)then
                            in=ineigh
                            ip=in+1
                            call point_line_dist(xmt(in), ymt(in),
     &                          zmt(in),xmt(ip), ymt(ip),zmt(ip),
     &                          xtmp(1),ytmp(1),ztmp(1),tneigh,dsqr)
                            dist =sqrt(dsqr)-sizeref
                            if(dist.lt.distmin)then
                              distmin=dist
                              if(distmin.lt.winmax.and.
     &                            distmin.ge.winmin)then
                                x1min=xtmp(1)
                                y1min=ytmp(1)
                                z1min=ztmp(1)
                                omt=1.-tneigh
                                x2min=xmt(in)*omt+ xmt(ip)*tneigh
                                y2min=ymt(in)*omt+ ymt(ip)*tneigh
                                z2min=zmt(in)*omt+ zmt(ip)*tneigh
                                call trim_win_seg(x1min,y1min,z1min,
     &                              x2min,y2min,z2min,sizeref,0.)
                              endif
                            endif
                          endif
                        enddo
c                         
c                         LINE SEGMENTS TO POINTS: consider each neighbor
c                         
                      elseif(neighflag.eq.2.and.ntmp.gt.1)then
                        do ineigh=indneigh,limneigh
                          neighskip=0
                          if(onlyshifted.and.jshift.gt.0)then
                            if(.not.shifted(jshift))neighskip=1
                            jshift=jshift+1
                          endif
                          if(neighskip.eq.0)then
                            in=ineigh
                            sizen=0.
                            if(ifscatsurf.ne.0)then
                              is=indsize(iobjneigh)+ineigh-indneigh
                              sizen=sizes(is)
                            endif
                            distmin=1.01*distlim
                            do itmp=1,ntmp-1
                              if(xmin(ineigh)-xtmpmax(itmp).lt.distmin.and.
     &                            xtmpmin(itmp)-xmax(ineigh).lt.distmin.and.
     &                            ymin(ineigh)-ytmpmax(itmp).lt.distmin.and.
     &                            ytmpmin(itmp)-ymax(ineigh).lt.distmin.and.
     &                            zmin(ineigh)-ztmpmax(itmp).lt.distmin.and.
     &                            ztmpmin(itmp)-zmax(ineigh).lt.distmin)then
                                call point_line_dist(xtmp(itmp),ytmp(itmp),
     &                              ztmp(itmp),xtmp(itmp+1),ytmp(itmp+1),
     &                              ztmp(itmp+1),xmt(in), ymt(in),
     &                              zmt(in), tref,dsqr)
                                dist=sqrt(dsqr)-sizen
                                if(dist.lt.distmin)then
                                  distmin=dist
                                  omt=1.-tref
                                  x1min=xtmp(itmp)*omt+xtmp(itmp+1)*tref
                                  y1min=ytmp(itmp)*omt+ytmp(itmp+1)*tref
                                  z1min=ztmp(itmp)*omt+ztmp(itmp+1)*tref
                                endif
                              endif
                            enddo
c                             
c                             bin the distance to this point
c                             
                            if(distmin.lt.distlim)then
                              ibin=max(1.,distmin/delr+1.)
                              call addDistanceToGraphs(iobjNeigh)
                            endif
c                             
c                             add to list if in window
c                             
                            if(distmin.lt.winmax.and.distmin.ge.winmin)then
                              x2=xmt(in)
                              y2=ymt(in)
                              z2=zmt(in)
                              call trim_win_seg(x1min,y1min,z1min, x2,
     &                            y2,z2, 0.,sizen)
                              call save_connector(x1min,y1min,z1min,x2,
     &                            y2,z2,xmt,ymt,zmt,limxyz,indfree,
     &                            iobjref, iobjneigh,iobjwin, nobjwin,
     &                            ninwin,ninwtot, nrefwin, nneighwin)
                            endif
                          endif
                        enddo
c                         set bin back to zero to avoid getting counted
c                         below
                        ibin=0
                        distmin=1.01*distlim
                      else
c                         
c                         POINTS TO POINTS: consider each neighbor
c                         
                        do ineigh=indneigh,limneigh
                          neighskip=0
                          if(onlyshifted.and.jshift.gt.0)then
                            if(.not.shifted(jshift))neighskip=1
                            jshift=jshift+1
                          endif
                          sizen=0.
                          if(ifscatsurf.ne.0)then
                            is=indsize(iobjneigh)+ineigh-indneigh
                            sizen=sizes(is)
                          endif
                          if(iref.ne.ineigh.and.neighskip.eq.0.and.
     &                        xmin(ineigh)-refxmax.lt.distlim.and.
     &                        refxmin-xmax(ineigh).lt.distlim.and.
     &                        ymin(ineigh)-refymax.lt.distlim.and.
     &                        refymin-ymax(ineigh).lt.distlim.and.
     &                        zmin(ineigh)-refzmax.lt.distlim.and.
     &                        refzmin-zmax(ineigh).lt.distlim)then
                            in=ineigh
                            dist =sqrt((xtmp(1)-xmt(in))**2+
     &                          (ytmp(1)-ymt(in))**2+
     &                          (ztmp(1)-zmt(in))**2)-sizeref-sizen
c                             
c                             bin the distance to this point
c                             
                            if(dist.lt.distlim)then
                              ibin=max(1.,dist/delr+1.)
                              call addDistanceToGraphs(iobjNeigh)
                            endif
c                             
c                             add to list if in window
c                             
                            if(dist.lt.winmax.and.dist.ge.winmin)then
                              x1=xtmp(1)
                              y1=ytmp(1)
                              z1=ztmp(1)
                              x2=xmt(in)
                              y2=ymt(in)
                              z2=zmt(in)
                              call trim_win_seg(x1,y1,z1, x2,y2,z2,
     &                            sizeref,sizen)
                              ifsave=1
                              if(irefflag.eq.2)then
                                i=1
                                do while(ifsave.eq.1.and.i.le.ninwin)
                                  j1=indstrt(nmt)+npntobj(nmt)+(i-1)*3
                                  j3=j1+2
                                  if(x1.eq.xmt(j3).and.x2.eq.xmt(j1).and.
     &                                y1.eq.ymt(j3).and.y2.eq.ymt(j1).and.
     &                                z1.eq.zmt(j3).and.z2.eq.zmt(j1))
     &                                ifsave=0
                                  i=i+1
                                enddo
                              endif
                              if(ifsave.ne.0)call save_connector(x1,y1,z1,x2,
     &                            y2,z2,xmt,ymt,zmt,limxyz, indfree, iobjref,
     &                            iobjneigh,iobjwin, nobjwin, ninwin,ninwtot,
     &                            nrefwin, nneighwin)
                            endif
                          endif
                        enddo
                        ibin=0
                        distmin=1.01*distlim
                      endif
c                       

                      if(distmin.lt.distlim)ibin=max(1.,distmin/delr+1.)
                      if(distmin.lt.winmax.and.distmin.ge.winmin)then
                        ang=acosd(cosang)
                        angsum=angsum+ang
                        angsmsq=angsmsq+ang**2
                        if(findsep)then
                          ifrefon=0
                          ifneighon=0
                          ninwin=ninwin+1
                          do iow=1,-nobjwin
                            if(iobjmod(iobjref).eq.iobjwin(iow))then
                              ifrefon=1
                              sepref=sqrt((x1min-xyzend(1,iow))**2+
     &                            (y1min-xyzend(2,iow))**2+
     &                            (z1min-xyzend(3,iow))**2)
                              endsep(iow)=min(endsep(iow),sepref)
                              xmt(indfree)=sepref
                              indfree=indfree+1
                            endif
                            if(iobjmod(iobjneigh).eq.iobjwin(iow))then
                              ifneighon=1
                              sepneigh=sqrt((x2min-xyzend(1,iow))**2+
     &                            (y2min-xyzend(2,iow))**2+
     &                            (z2min-xyzend(3,iow))**2)
                              endsep(iow)=min(endsep(iow),sepneigh)
                              xmt(indfree)=sepneigh
                              indfree=indfree+1
                            endif
                          enddo
                          nclose(ifrefon+1,ifneighon+1)=
     &                        nclose(ifrefon+1,ifneighon+1)+1
                          if(ifrefon.eq.0)then
                            do iow=1-nobjwin,noendwin
                              if(iobjref.eq.iobjwin(iow))ifrefon=1
                            enddo
                            if(ifrefon.eq.0)then
                              noendwin=noendwin+1
                              iobjwin(noendwin)=iobjref
                              nrefwin=nrefwin+1
                            endif
                          endif
                          if(ifneighon.eq.0)then
                            do iow=1-nobjwin,noendwin
                              if(iobjneigh.eq.iobjwin(iow))ifneighon=1
                            enddo
                            if(ifneighon.eq.0)then
                              noendwin=noendwin+1
                              iobjwin(noendwin)=iobjneigh
                              nneighwin=nneighwin+1
                            endif
                          endif
                        else
                          call save_connector(x1min,y1min,z1min,x2min,
     &                        y2min,z2min,xmt,ymt,zmt,limxyz,indfree,
     &                        iobjref, iobjneigh,iobjwin,nobjwin,ninwin,
     &                        ninwtot,nrefwin, nneighwin)
                        endif
                      endif
                    else
c                       print *,'Eliminated based on global min/max'
                    endif
                  endif
                  if(ibin.gt.0)then
                    call addDistanceToGraphs(iobjNeigh)
                  endif
                  if(usebinsave)ibinsave(itriang)=ibin
                endif
              enddo
            elseif(ifskipfail.eq.0)then
c               
c               NEIGHBOR IS A MESH
c               
              do imesh=1,nmeshloaded
                isneigh=0
                do jj = 1, needref
                  if(neighpt(igraphref(jj),imesh))isneigh=1
                enddo
                if(isneigh.eq.1)then
c                   
c                   find a separate distance for each surface
c                   
                  if(onlyshifted)jshift=indexshift(imesh,
     &                4,icolor,npntobj,nmt)
                  do isurf=iobjsurf(imesh),iobjsurf(imesh)+nsurfobj(imesh)-1
c                     
c                     check against surface global limits, eliminate failed
c                     
                    distmin=1.01*distlim
                    dminsq=distmin**2
                    neighskip=0
                    if(onlyshifted.and.jshift.gt.0)then
                      if(.not.shifted(jshift))neighskip=1
                      jshift=jshift+1
                    endif
                    if(neighskip.eq.0.and.
     &                  refxmin-surfxmax(isurf).lt.distmin.and.
     &                  surfxmin(isurf)-refxmax.lt.distmin.and.
     &                  refymin-surfymax(isurf).lt.distmin.and.
     &                  surfymin(isurf)-refymax.lt.distmin.and.
     &                  refzmin-surfzmax(isurf).lt.distmin.and.
     &                  surfzmin(isurf)-refzmax.lt.distmin)then
                      do list=istrsurf(isurf),
     &                    istrsurf(isurf)+ninsurf(isurf)-1
                        ipoly=listsurf(list)
c                         
c                         check against polygon global limits
c                         
                        if(refxmin-polyxmax(ipoly).lt.distmin.and.
     &                      polyxmin(ipoly)-refxmax.lt.distmin.and.
     &                      refymin-polyymax(ipoly).lt.distmin.and.
     &                      polyymin(ipoly)-refymax.lt.distmin.and.
     &                      refzmin-polyzmax(ipoly).lt.distmin.and.
     &                      polyzmin(ipoly)-refzmax.lt.distmin)then
c                           
c                           scan through triangles
c                           
                          do itri=istrpoly(ipoly),istrpoly(ipoly)+
     &                        ninpoly(ipoly)-1
                            if(refxmin-trixmax(itri).lt.distmin.and.
     &                          trixmin(itri)-refxmax.lt.distmin.and.
     &                          refymin-triymax(itri).lt.distmin.and.
     &                          triymin(itri)-refymax.lt.distmin.and.
     &                          refzmin-trizmax(itri).lt.distmin.and.
     &                          trizmin(itri)-refzmax.lt.distmin)then
c                               
c                               SINGLE POINT TO TRIANGLE
c                               
                              if(ntmp.eq.1)then
                                call point_to_triangle(xtmp(1),
     &                              ytmp(1),ztmp(1), itri,xrot,yrot,dist)
                                dist=dist-sizeref
                                if(dist.lt.distmin)then
                                  distmin=dist
                                  if(distmin.lt.winmax.and.
     &                                distmin.ge.winmin)then
                                    x1min=xtmp(1)
                                    y1min=ytmp(1)
                                    z1min=ztmp(1)
                                    xprime=xrot*cbet(itri)+zrot(itri)*
     &                                  sbet(itri)
                                    x2min=xprime*cgam(itri)+yrot*sgam(itri)
                                    y2min=-xprime*sgam(itri)+yrot*cgam(itri)
                                    z2min=-xrot*sbet(itri)+zrot(itri)*
     &                                  cbet(itri)
                                    call trim_win_seg(x1min,y1min,z1min,
     &                                  x2min,y2min,z2min,sizeref,0.)
                                  endif

                                endif
                              else
c                                 
c                                 SERIES OF LINE SEGMENTS TO TRIANGLE
c                                 
                                do itmp=1,ntmp-1
                                  if(xtmpmin(itmp)-trixmax(itri).lt.
     &                                distmin.and.
     &                                trixmin(itri)-xtmpmax(itmp).lt.
     &                                distmin.and.
     &                                ytmpmin(itmp)-triymax(itri).lt.
     &                                distmin.and.
     &                                triymin(itri)-ytmpmax(itmp).lt.
     &                                distmin.and.
     &                                ztmpmin(itmp)-trizmax(itri).lt.
     &                                distmin.and.
     &                                trizmin(itri)-ztmpmax(itmp).lt.
     &                                distmin)then
                                    call segment_to_triangle(xtmp(itmp),
     &                                  ytmp(itmp),ztmp(itmp),xtmp(itmp+1),
     &                                  ytmp(itmp+1),ztmp(itmp+1),
     &                                  itri,tref,xrot,yrot,dist)
                                    if(dist.lt.distmin)then
                                      distmin=dist
                                      if(distmin.lt.winmax.and.
     &                                    distmin.ge.winmin)then
                                        omt=1.-tref
                                        x1min=xtmp(itmp)*omt+xtmp(itmp+1)*tref
                                        y1min=ytmp(itmp)*omt+ytmp(itmp+1)*tref
                                        z1min=ztmp(itmp)*omt+ztmp(itmp+1)*tref
                                        xprime=xrot*cbet(itri)+
     &                                      zrot(itri)*sbet(itri)
                                        x2min=xprime*cgam(itri)+yrot*sgam(itri)
                                        y2min=-xprime*sgam(itri)+
     &                                      yrot*cgam(itri)
                                        z2min=-xrot*sbet(itri)+
     &                                      zrot(itri)*cbet(itri)
                                      endif
                                    endif
                                  endif
                                enddo
                              endif
c                               
                            endif
                          enddo

                        endif
                      enddo
                    endif
c                     
c                     add to bins
c                     
                    if(distmin.lt.distlim)then
                      ibin=max(1.,distmin/delr+1.)
                      call addDistanceToGraphs(imesh)
c$$$                    if(distmin.lt.0.005)then
c$$$                    distmin2=0.03
c$$$                    distabs=0.015
c$$$                    call check_line_surface(isurf,indref,limref,
c$$$                    &                             xmin,xmax,ymin,ymax,zmin,zmax,xmt,ymt,zmt,
c$$$                    &                             distmin2,distabs,zscal)
c$$$                    print *,glbxmin(iobjref),glbxmax(iobjref),
c$$$                    &                             glbymin(iobjref),glbymax(iobjref),
c$$$                    &                             glbzmin(iobjref),glbzmax(iobjref)
c$$$                    print *,surfxmin(isurf),surfxmax(isurf),
c$$$                    &                             surfymin(isurf),surfymax(isurf),
c$$$                    &                             surfzmin(isurf),surfzmax(isurf)
c$$$                    print *,isurf,iobjref,iref,distmin,distmin2
c$$$                    endif

c                       
                    endif
c                     
c                     take care of window stuff here
c                     
                    if(distmin.lt.winmax.and.distmin.ge.winmin)
     &                  call save_connector(x1min,y1min,z1min,x2min, y2min,
     &                  z2min,xmt,ymt,zmt,limxyz,indfree,iobjref, -isurf,
     &                  iobjwin,nobjwin,ninwin,ninwtot, nrefwin, nneighwin)
                  enddo
                endif
              enddo
            endif
c             
c             advance at end of loop and test for termination
c             
            if(irefflag.eq.1)then
c               
c               whole line is done
c               
              refdone=samplen.le.0.
              if(.not.refdone)then
c                 
c                 segmented line: add cylindrical/spherical shells to
c                 fracsum volume; advance to next sample, done if less than
c                 half of sample length traversed
c                 
                do ib=1,nbins
                  shellvol=3.14159*delr**2*(4.*delr*(ib**2-ib+0.333333)+
     &                2*(ib-0.5))
                  do ineed=1,needref
                    jj=igraphref(ineed)
                    fracsum(ib,jj)=fracsum(ib,jj)+shellvol
                  enddo
                enddo
                iref=irefend
                sampfrac=fracend
                call get_next_sample
     &              (xmt,ymt,zmt,iref,sampfrac,limref,samplen,ifcloseg,
     &              xtmp,ytmp, ztmp,ntmp,irefend,fracend,seglen,
     &              xtmpmin,xtmpmax,ytmpmin,ytmpmax,ztmpmin, ztmpmax,
     &              refxmin,refxmax,refymin,refymax,refzmin,refzmax)
                refdone=seglen.lt.0.5*samplen
              endif
            else
c               
c               points: advance iref, add to fracsum
c               
              iref=iref+1
              refdone=iref.gt.limref
              if(onlyshifted.and.ishift.gt.0)ishift=ishift+1
              do ib=1,nbins
                shellvol=1.33333*3.14159*
     &              ((sizeref+ib*delr)**3-(sizeref+(ib-1)*delr)**3)
                do ineed=1,needref
                  jj=igraphref(ineed)
                  fracsum(ib,jj)=fracsum(ib,jj)+shellvol
                enddo
              enddo
            endif
            call addNearestToGraphs()
          enddo
        elseif(needref.gt.0)then
c           
c           REFERENCE IS A MESH
c           loop on surfaces
c           
          do isurf=iobjsurf(iobjref),iobjsurf(iobjref)+nsurfobj(iobjref)-1
c             
c             if failed shift, set loop limit to 0
c             
            ifskipfail=0
            if(onlyshifted.and.ishift.gt.0)then
              if(.not.shifted(ishift))ifskipfail=1
              ishift=ishift+1
            endif
            ionend=0
c             
            if(ifskipfail.eq.0)then
              if(manyrandom.eq.0)then
                write(*,'(a,i5,$)')char(13)//'Doing surface #',isurf
                call flush(6)
              endif
              refxmin=surfxmin(isurf)
              refymin=surfymin(isurf)
              refzmin=surfzmin(isurf)
              refxmax=surfxmax(isurf)
              refymax=surfymax(isurf)
              refzmax=surfzmax(isurf)
              ionend=nmt
c               
c               add spherical shells to fracsum, with the equivalent radius
c               taken from the area of the surface
c               
              sizeref=sqrt(0.25*surfarea(isurf)/3.14159)
              do ib=1,nbins
                shellvol=1.33333*3.14159*
     &              ((sizeref+ib*delr)**3-(sizeref+(ib-1)*delr)**3)
                do ineed=1,needref
                  jj=igraphref(ineed)
                  fracsum(ib,jj)=fracsum(ib,jj)+shellvol
                enddo
              enddo
c               
              if(neighflag.eq.4)ionend=nmeshloaded
            endif
c             
c             scan through objects for neighbors, unless failed shift
c             
            do iobjneigh=1,ionend
              isneigh=0
              do ineed=1,needref
                jj=igraphref(ineed)
                if(neighpt(jj,iobjneigh))isneigh=1
              enddo
              distmin=1.01*distlim
              if(neighflag.lt.4.and.isneigh.ne.0)then
c                 
c                 POINTS OR LINES
c                 
                indneigh=indstrt(iobjneigh)
                limneigh=indneigh+npntobj(iobjneigh)-1
                if(neighflag.eq.1)limneigh=limneigh-1
                neighskip=0
                if(onlyshifted)then
                  jshift=indexshift(iobjneigh, neighflag,icolor,
     &                npntobj,nmt)
                  if(neighflag.eq.1.and.jshift.gt.0)then
                    if(.not.shifted(jshift))neighskip=1
                  endif
                endif
c                 
c                 check against global limits of neighbor
c                 
                if(neighskip.eq.0.and.
     &              refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &              glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &              refymin-glbymax(iobjneigh).lt.distmin.and.
     &              glbymin(iobjneigh)-refymax.lt.distmin.and.
     &              refzmin-glbzmax(iobjneigh).lt.distmin.and.
     &              glbzmin(iobjneigh)-refzmax.lt.distmin)then
                  do ineigh=indneigh,limneigh
                    if(neighflag.eq.2)then
                      distmin=1.01*distlim
                      sizen=0.
                      if(ifscatsurf.ne.0)then
                        is=indsize(iobjneigh)+ineigh-indneigh
                        sizen=sizes(is)
                      endif
                      neighskip=0
                      if(onlyshifted.and.jshift.gt.0)then
                        if(.not.shifted(jshift))neighskip=1
                        jshift=jshift+1
                      endif
                    endif
c                     
c                     loop on points/segments, check against surface limits
c                     
                    xminnay=xmin(ineigh)
                    yminnay=ymin(ineigh)
                    zminnay=zmin(ineigh)
                    xmaxnay=xmax(ineigh)
                    ymaxnay=ymax(ineigh)
                    zmaxnay=zmax(ineigh)
                    if(neighskip.eq.0.and.
     &                  xminnay-refxmax.lt.distmin.and.
     &                  refxmin-xmaxnay.lt.distmin.and.
     &                  yminnay-refymax.lt.distmin.and.
     &                  refymin-ymaxnay.lt.distmin.and.
     &                  zminnay-refzmax.lt.distmin.and.
     &                  refzmin-zmaxnay.lt.distmin)then
c                       
c                       loop on polygons in surface
c                       
                      do list=istrsurf(isurf),
     &                    istrsurf(isurf)+ninsurf(isurf)-1
                        ipoly=listsurf(list)
                        if(xminnay-polyxmax(ipoly).lt.distmin.and.
     &                      polyxmin(ipoly)-xmaxnay.lt.distmin.and.
     &                      yminnay-polyymax(ipoly).lt.distmin.and.
     &                      polyymin(ipoly)-ymaxnay.lt.distmin.and.
     &                      zminnay-polyzmax(ipoly).lt.distmin.and.
     &                      polyzmin(ipoly)-zmaxnay.lt.distmin)then
c                           
c                           loop on triangles in polygon
c                           
                          do itri=istrpoly(ipoly),
     &                        istrpoly(ipoly)+ninpoly(ipoly)-1
                            if(xminnay-trixmax(itri).lt.distmin.and.
     &                          trixmin(itri)-xmaxnay.lt.distmin.and.
     &                          yminnay-triymax(itri).lt.distmin.and.
     &                          triymin(itri)-ymaxnay.lt.distmin.and.
     &                          zminnay-trizmax(itri).lt.distmin.and.
     &                          trizmin(itri)-zmaxnay.lt.distmin)then
                              if(neighflag.eq.1)then
c                                 
c                                 MESH TO LINES
c                                 
                                call segment_to_triangle(xmt(ineigh),
     &                              ymt(ineigh),zmt(ineigh),xmt(ineigh+1),
     &                              ymt(ineigh+1),zmt(ineigh+1),
     &                              itri,tn,xrot,yrot,dist)
                                if(dist.lt.distmin)then
                                  distmin=dist
                                  if(distmin.lt.winmax.and.
     &                                distmin.ge.winmin)then
                                    omt=1.-tn
                                    x1min=xmt(ineigh)*omt+xmt(ineigh+1)*tn
                                    y1min=ymt(ineigh)*omt+ymt(ineigh+1)*tn
                                    z1min=zmt(ineigh)*omt+zmt(ineigh+1)*tn
                                    xprime=xrot*cbet(itri)+
     &                                  zrot(itri)*sbet(itri)
                                    x2min=xprime*cgam(itri)+
     &                                  yrot*sgam(itri)
                                    y2min=-xprime*sgam(itri)+
     &                                  yrot*cgam(itri)
                                    z2min=-xrot*sbet(itri)+
     &                                  zrot(itri)*cbet(itri)
                                  endif
                                endif
                              else
c                                 
c                                 MESH TO POINTS
c                                 
                                call point_to_triangle(xmt(ineigh),
     &                              ymt(ineigh),zmt(ineigh), itri,
     &                              xrot,yrot,dist)
                                dist=dist-sizen
                                if(dist.lt.distmin)then
                                  distmin=dist
                                  if(distmin.lt.winmax.and.
     &                                distmin.ge.winmin)then
                                    x1min=xmt(ineigh)
                                    y1min=ymt(ineigh)
                                    z1min=zmt(ineigh)
                                    xprime=xrot*cbet(itri)+zrot(itri)*
     &                                  sbet(itri)
                                    x2min=xprime*cgam(itri)+yrot*sgam(itri)
                                    y2min=-xprime*sgam(itri)+yrot*
     &                                  cgam(itri)
                                    z2min=-xrot*sbet(itri)+zrot(itri)*
     &                                  cbet(itri)
                                    call trim_win_seg(x1min,y1min,z1min,
     &                                  x2min,y2min,z2min,sizen,0.)
                                  endif
                                endif
                              endif
                            endif
                          enddo
                        endif
                      enddo
                    endif
c                     
c                     doing points: bin distance for each point
c                     
                    if(neighflag.eq.2.and.distmin.lt.distlim)then
                      ibin=max(1.,distmin/delr+1.)
                      call addDistanceToGraphs(iobjNeigh)
c                       
                      if(distmin.lt.winmax.and.distmin.ge.winmin)
     &                    call save_connector(x1min,y1min,z1min,x2min,
     &                    y2min,z2min,xmt,ymt,zmt,limxyz,indfree,-isurf,
     &                    iobjneigh,iobjwin,nobjwin,ninwin,ninwtot,
     &                    nrefwin, nneighwin)
                    endif

                  enddo
c                   
c                   doing lines: bin distance after getting global minimum
c                   
                  if(neighflag.eq.1.and.distmin.lt.distlim)then
                    ibin=max(1.,distmin/delr+1.)
                    call addDistanceToGraphs(iobjNeigh)
c                     
                    if(distmin.lt.winmax.and.distmin.ge.winmin)
     &                  call save_connector(x1min,y1min,z1min,x2min,
     &                  y2min,z2min,xmt,ymt,zmt,limxyz,indfree,-isurf,
     &                  iobjneigh,iobjwin,nobjwin,ninwin,ninwtot,
     &                  nrefwin, nneighwin)
                  endif
                endif
              elseif(isneigh.ne.0)then
c                 
c                 MESH TO MESH
c                 loop through surfaces of neighbor, skipping failed shifts
c                 
                if(onlyshifted)jshift=indexshift(iobjneigh,
     &              4,icolor,npntobj,nmt)
                
                do jsurf=iobjsurf(iobjneigh),iobjsurf(iobjneigh)+
     &              nsurfobj(iobjneigh)-1
                  neighskip=0
                  if(onlyshifted.and.jshift.gt.0)then
                    if(.not.shifted(jshift))neighskip=1
                    jshift=jshift+1
                  endif
                  if(isurf.ne.jsurf.and.neighskip.eq.0)then
                    if(usebinsave)then
                      isurfmax=max(isurf,jsurf)
                      itriang=(isurfmax-1)*(isurfmax-2)/2+min(isurf,jsurf)
                      ibin=ibinsave(itriang)
                    endif

                    if(isurf.lt.jsurf.or.ibin.eq.-1.or.
     &                  .not.usebinsave)then
                      
                      distmin=1.01*distlim
                      ibin=0
                      if(refxmin-surfxmax(jsurf).lt.distmin.and.
     &                    surfxmin(jsurf)-refxmax.lt.distmin.and.
     &                    refymin-surfymax(jsurf).lt.distmin.and.
     &                    surfymin(jsurf)-refymax.lt.distmin.and.
     &                    refzmin-surfzmax(jsurf).lt.distmin.and.
     &                    surfzmin(jsurf)-refzmax.lt.distmin)then
c                         
c                         loop on polygons of neighbor surface
c                         
                        do mist=istrsurf(jsurf),
     &                      istrsurf(jsurf)+ninsurf(jsurf)-1
                          jpoly=listsurf(mist)
                          if(refxmin-polyxmax(jpoly).lt.distmin.and.
     &                        polyxmin(jpoly)-refxmax.lt.distmin.and.
     &                        refymin-polyymax(jpoly).lt.distmin.and.
     &                        polyymin(jpoly)-refymax.lt.distmin.and.
     &                        refzmin-polyzmax(jpoly).lt.distmin.and.
     &                        polyzmin(jpoly)-refzmax.lt.distmin)then
c                             
c                             loop on triangles of neighbor
c                             
                            do jtri=istrpoly(jpoly),
     &                          istrpoly(jpoly)+ninpoly(jpoly)-1
                              xminnay=trixmin(jtri)
                              yminnay=triymin(jtri)
                              zminnay=trizmin(jtri)
                              xmaxnay=trixmax(jtri)
                              ymaxnay=triymax(jtri)
                              zmaxnay=trizmax(jtri)
                              if(refxmin-xmaxnay.lt.distmin.and.
     &                            xminnay-refxmax.lt.distmin.and.
     &                            refymin-ymaxnay.lt.distmin.and.
     &                            yminnay-refymax.lt.distmin.and.
     &                            refzmin-zmaxnay.lt.distmin.and.
     &                            zminnay-refzmax.lt.distmin)then
c                                 
c                                 now loop on polygons of reference surface
c                                 
                                do list=istrsurf(isurf),
     &                              istrsurf(isurf)+ninsurf(isurf)-1
                                  ipoly=listsurf(list)
                                  if(xminnay-polyxmax(ipoly).lt.distmin.and.
     &                                polyxmin(ipoly)-xmaxnay.lt.distmin.and.
     &                                yminnay-polyymax(ipoly).lt.distmin.and.
     &                                polyymin(ipoly)-ymaxnay.lt.distmin.and.
     &                                zminnay-polyzmax(ipoly).lt.distmin.and.
     &                                polyzmin(ipoly)-zmaxnay.lt.distmin)then
c                                     
c                                     loop on triangles in reference polygon
c                                     
                                    do itri=istrpoly(ipoly),
     &                                  istrpoly(ipoly)+ninpoly(ipoly)-1
                                      if(xminnay-trixmax(itri).lt.distmin.and.
     &                                    trixmin(itri)-xmaxnay.lt.distmin.and.
     &                                    yminnay-triymax(itri).lt.distmin.and.
     &                                    triymin(itri)-ymaxnay.lt.distmin.and.
     &                                    zminnay-trizmax(itri).lt.distmin.and.
     &                                    trizmin(itri)-zmaxnay.lt.distmin)then
                                        call triangle_to_triangle(itri,jtri,
     &                                      xr1,yr1,zr1,xr2,yr2, itrir, dist)
                                        if(dist.lt.distmin)then
                                          distmin=dist
                                          if(distmin.lt.winmax.and.
     &                                        distmin.ge.winmin)then
                                            xprime=xr1*cbet(itrir)+
     &                                          zr1*sbet(itrir)
                                            x1min=xprime*cgam(itrir)+
     &                                          yr1*sgam(itrir)
                                            y1min=-xprime*sgam(itrir)+yr1*
     &                                          cgam(itrir)
                                            z1min=-xr1*sbet(itrir)+zr1*
     &                                          cbet(itrir)
                                            xprime=xr2*cbet(itrir)+
     &                                          zrot(itrir)* sbet(itrir)
                                            x2min=xprime*cgam(itrir)+
     &                                          yr2*sgam(itrir)
                                            y2min=-xprime*sgam(itrir)+yr2*
     &                                          cgam(itrir)
                                            z2min=-xr2*sbet(itrir)+
     &                                          zrot(itrir)* cbet(itrir)
                                          endif
                                        endif
                                      endif
                                    enddo
                                  endif
                                enddo
                              endif
                            enddo
                          endif
                        enddo
c                         
c                         doing meshes: bin distance after getting global
c                         minimum
c                         
                        if(distmin.lt.distlim)then
                          ibin=max(1.,distmin/delr+1.)
c                           
                          if(distmin.lt.winmax.and.distmin.ge.winmin)
     &                        call save_connector(x1min,y1min,z1min,x2min,
     &                        y2min,z2min,xmt,ymt,zmt,limxyz,indfree,-isurf,
     &                        -jsurf,iobjwin,nobjwin,ninwin,ninwtot,
     &                        nrefwin, nneighwin)
                        endif
                        
                      endif                 
c$$$                    if(distmin.lt.0.005)then
c$$$                    distmin2=0.03
c$$$                    distabs=0.015
c$$$                    call check_two_meshes(isurf,jsurf,distmin2,
c$$$                    &                             distabs,zscal)
c$$$                    print *,'close', isurf,jsurf,distmin,distmin2
c$$$                    endif

                    endif                   
                    if(ibin.gt.0)then
                      call addDistanceToGraphs(iobjNeigh)
                    endif           
                    if(usebinsave)ibinsave(itriang)=ibin
                  endif
                enddo
              endif
            enddo
            call addNearestToGraphs()
          enddo
        endif
      enddo
c       
c       scale counts: use power parameter for whole lines to compute fracsum,
c       otherwise set power to 2 (fracsum should be complete)
c       
      if((irefflag.eq.1.and.samplen.le.0.) .or. nearestOnly)then
        do ibin=1,nbins
          if(power.eq.0.)then
            radpow=1.
          else
            radpow=(2.*(ibin-0.5)*delr)**power
          endif
          frac=radpow*delr*3.14159
          if (nearestOnly) frac = 1.
          do jj=1,ngraph
            fracsum(ibin,jj)=frac*nrefobj(jj)
            graphs(ibin,jj)=graphs(ibin,jj)/fracsum(ibin,jj)
          enddo
        enddo
        poweruse=power
        if (nearestOnly) poweruse = 0
      else
        do ibin=1,nbins
          do jj=1,ngraph
            graphs(ibin,jj)=graphs(ibin,jj)/fracsum(ibin,jj)
          enddo
        enddo
        poweruse=2
      endif
      do jj=1,ngraph
        powergrf(jj)=poweruse
      enddo
c       
      if(ninwtot.gt.0.)then
        call sums_to_avgsd(angsum,angsmsq,ninwtot,angav,angsd)
        write(*,106)ninwtot,angav,angsd,nrefwin,nneighwin
106     format(i4,' distances in window; angle mean=',f6.2,',  SD=',
     &      f6.2,/,i6,' reference and',i5,' neighbor objects')
        if (ninwin.lt.ninwtot)print *,ninwtot-ninwin,
     &      ' connectors could not be saved - arrays full'
      endif
      if(findsep)then
        write(*,108)((nclose(i,j),i=1,2),j=1,2)
108     format(i5,' &',i4,' distances in window for neighbor OFF end',
     &      ' list, reference OFF & ON',/,i5,' &',i4,
     &      ' distances in window for neighbor ON end',
     &      ' list, reference OFF & ON')
        ninwin=indfree-(indstrt(nmt)+npntobj(nmt))
      endif
      return
99    print *,'Data not loaded; try fewer objects'
      nmeshloaded=0
      return

      CONTAINS

c       Add the bin to each of the graphs that includes it, unless doing nearest neighbor
c       then just maintain the minimum for the graph
      subroutine addDistanceToGraphs(neighbor)
      integer*4 neighbor
      do ineed=1,needref
        jj=igraphref(ineed)
        if (neighpt(jj,neighbor)) then
          if (nearestOnly) then
            if (minBin(jj) .eq. 0 .or. ibin .lt. minBin(jj)) minBin(jj) = ibin
          else
            graphs(ibin,jj)=graphs(ibin,jj)+1.
          endif
        endif
      enddo
      return
      end subroutine addDistanceToGraphs

c       After getting a minimu distance to a reference, add it to the graphs
      subroutine addNearestToGraphs()
      if (.not. nearestOnly) return
      do ineed=1,needref
        jj=igraphref(ineed)
        if (minBin(jj) .gt. 0) graphs(minBin(jj),jj) = graphs(minBin(jj),jj) + 1.
        nrefObj(jj) = nrefObj(jj) + 1
        minBin(jj) = 0
      enddo
      return
      end subroutine addNearestToGraphs

      end subroutine closedist

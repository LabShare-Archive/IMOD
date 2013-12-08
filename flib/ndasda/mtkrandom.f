c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2003/08/08 17:52:26  mast
c       Removed ctrl-M from ends of lines
c       
c       Revision 3.1  2002/08/03 22:52:05  mast
c       Break up an if statement to keep outside_boundary from being called
c       with no boundaries.
c       
c       
      subroutine random_shifts(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &    iobjflag,ranmin,ranmax,probnear,limprobs,delnear,nrestrict,
     &    nshiftyp, itypshift, ishiftflag, nchcktyp,
     &    itypchck,iuseprob, zgapst, zgapnd,
     &    ngaps,boxtol,iobjbound,ifcheckunshifted, ranzrel,maxtrials,
     &    ntrialcycle,cyclefac,ifscatsurf,xyscal,zscal,manyrandom,
     &    ifexcludeout)
      include 'mtk.inc'
      parameter (limwobj=30000,
     &    limtyp=50,itypall=999,limxyz=50000,limbound=1000)
      parameter (limind=limverts*6)
      parameter (limsizes=limxyz)
      real*4 xmt(*),ymt(*),zmt(*),probnear(limprobs,*),delnear(*)
      integer*4 indstrt(*),npntobj(*),iobjflag(*)
      integer*4 icolor(*)                       !types of sample points
      integer*4 itypshift(*),itypchck(*),nrestrict(*),iuseprob(*)
      real*4 zgapst(*),zgapnd(*)
      real*4 xmin(limxyz),xmax(limxyz),ymin(limxyz),ymax(limxyz)
      real*4 zmin(limxyz),zmax(limxyz)
      real*4 glbxmin(limwobj),glbymin(limwobj),glbzmin(limwobj)
      real*4 glbxmax(limwobj),glbymax(limwobj),glbzmax(limwobj)
      logical*1 needshift(limxyz),needcheck(limxyz)
      integer*2 ntrials(limxyz)
      integer*4 modind(limind)
      real *4 delxyz(3)
      equivalence (delx,delxyz(1)),(dely,delxyz(2)),(delz,delxyz(3))
      real*4 probuse(250)
      integer*4 getimodsizes
      integer*4 listbound(limbound)
      real*4 zbound(limbound)
      integer*4 nsizeloaded/0/
      integer*4 indsize(limwobj),nextsize(limeshobj)
      integer*4 icolorsize(limeshobj)
      real*4 sizes(limsizes)
      save nsizeloaded,indsize,nextsize,icolorsize,sizes
      logical docheck,badshift,passing,outside_boundary
      data iffirst/1/
      save iffirst,iseed
      data icall/0/
      save icall
c       
      character*8 jtime
c       
      nsizeloaded=0
      ifreesize=1
      if (iffirst.ne.0)then
        call time(jtime)
        iseed=2*((ichar(jtime(8:8))+128*(ichar(jtime(7:7))+
     &      128*(ichar(jtime(5:5))+128*ichar(jtime(4:4)))))/2)+1
        iffirst=0
      endif
c       print *,' '
c       print *,'seed =',iseed
c       iseed = 112007479
c       if(icall.eq.1)iseed=112023601
c       icall=icall+1
      shiftxmin=1.e10
      shiftxmax=-1.e10
      shiftymin=1.e10
      shiftymax=-1.e10
      shiftzmin=1.e10
      shiftzmax=-1.e10
      call get_random_boundary(iobjbound,listbound,zbound,nbound,limbound)
c       
c       scan through the objects to be shifted, setting up the table of
c       shifts and undoing shifts that already exist, for lines and points
c       
      ibaseshift=1
      nitemstot=0
      if(nobjshifted.gt.0)ibaseshift=istrshift(nobjshifted)+
     &    nitemshift(nobjshifted)
      do kk=1,nshiftyp
        if(iobjflag(abs(itypshift(kk))).eq.1)then
          ifon=0
          do imo=1,nobjshifted
            if(iobjshift(imo).eq.itypshift(kk))ifon=imo
          enddo
          if(ifon.ne.0)then
c             
c             if this line object is already on the list, shift it back and
c             zero shifts
c             
            ishift=istrshift(ifon)
            do ii=1,nmt
              if(itypshift(kk).eq.icolor(ii)) then
                do ip=indstrt(ii),indstrt(ii)+npntobj(ii)-1
                  xmt(ip)=xmt(ip)-shifts(1,ishift)
                  ymt(ip)=ymt(ip)-shifts(2,ishift)
                  zmt(ip)=zmt(ip)-shifts(3,ishift)
                enddo             
                shifts(1,ishift)=0.
                shifts(2,ishift)=0.
                shifts(3,ishift)=0.
                shifted(ishift)=.true.
                ishift=ishift+1
              endif
            enddo
          else
c             
c             otherwise, set up the right number of shifts for the lines
c             
            nobjshifted=nobjshifted+1
            istrshift(nobjshifted)=ibaseshift
            iobjshift(nobjshifted)=itypshift(kk)
            nitemp=0
            do ii=1,nmt
              if(itypshift(kk).eq.icolor(ii)) then
                nitemp=nitemp+1
                shifts(1,ibaseshift)=0.
                shifts(2,ibaseshift)=0.
                shifts(3,ibaseshift)=0.
                shifted(ibaseshift)=.true.
                ibaseshift=ibaseshift+1
                if(ibaseshift.gt.limshift)then
                  print *,'Too many shifts for arrays'
                  return
                endif
              endif
            enddo
            nitemshift(nobjshifted)=nitemp
            ifon=nobjshifted
          endif
          nitemstot=nitemstot+nitemshift(ifon)
        elseif(iobjflag(abs(itypshift(kk))).eq.2)then
          ifon=0
          do imo=1,nobjshifted
            if(iobjshift(imo).eq.itypshift(kk))ifon=imo
          enddo
          if(ifon.gt.0)then
c             
c             if points are already on the list, shift them back and zero
c             shifts
c             
            ishift=istrshift(ifon)
            do ii=1,nmt
              if(itypshift(kk).eq.icolor(ii)) then
                do ip=indstrt(ii),indstrt(ii)+npntobj(ii)-1
                  xmt(ip)=xmt(ip)-shifts(1,ishift)
                  ymt(ip)=ymt(ip)-shifts(2,ishift)
                  zmt(ip)=zmt(ip)-shifts(3,ishift)
                  shifts(1,ishift)=0.
                  shifts(2,ishift)=0.
                  shifts(3,ishift)=0.
                  shifted(ishift)=.true.
                  ishift=ishift+1
                enddo  
              endif
            enddo
          else
c             
c             otherwise, set up the right number of shifts for the points
c             
            nobjshifted=nobjshifted+1
            istrshift(nobjshifted)=ibaseshift
            iobjshift(nobjshifted)=itypshift(kk)
            nitemp=0
            do ii=1,nmt
              if(itypshift(kk).eq.icolor(ii)) then
                do ip=1,npntobj(ii)
                  nitemp=nitemp+1
                  shifts(1,ibaseshift)=0.
                  shifts(2,ibaseshift)=0.
                  shifts(3,ibaseshift)=0.
                  shifted(ibaseshift)=.true.
                  ibaseshift=ibaseshift+1
                  if(ibaseshift.gt.limshift)then
                    print *,'Too many shifts for arrays'
                    return
                  endif
                enddo
              endif
            enddo
            nitemshift(nobjshifted)=nitemp
            ifon=nobjshifted
          endif
          nitemstot=nitemstot+nitemshift(ifon)
        endif
      enddo
c       
c       scan through loaded "mt" objects, load all sizes if needed
c       
      do ii=1,nmt
        needed=0
        indref=indstrt(ii)
        limref=indref+npntobj(ii)-1
        glbxmin(ii)=1.e10
        glbxmax(ii)=-1.e10
        glbymin(ii)=1.e10
        glbymax(ii)=-1.e10
        glbzmin(ii)=1.e10
        glbzmax(ii)=-1.e10
        do kk=1,nshiftyp
          if(itypshift(kk).eq.icolor(ii)) needed=1
        enddo
        inshifted=needed
        do kk=1,nchcktyp
          if(itypchck(kk).eq.icolor(ii)) needed=1
        enddo
        if(needed.ne.0)then
          needflag=iobjflag(abs(icolor(ii)))
          if(needflag.eq.1)then
            do iref=indref,limref-1
              xs1=xmt(iref)
              xn1=xmt(iref+1)
              ys1=ymt(iref)
              yn1=ymt(iref+1)
              zs1=zmt(iref)
              zn1=zmt(iref+1)
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
            print *,'Inappropriate object type in MT lists'
            return
          endif
          if(inshifted.eq.1)then
            shiftxmin=min(shiftxmin,glbxmin(ii))
            shiftymin=min(shiftymin,glbymin(ii))
            shiftzmin=min(shiftzmin,glbzmin(ii))
            shiftxmax=max(shiftxmax,glbxmax(ii))
            shiftymax=max(shiftymax,glbymax(ii))
            shiftzmax=max(shiftzmax,glbzmax(ii))
          endif
        endif
      enddo
c       
c       now initialize mesh loading if any meshes are used, then load 
c       shifted ones, if any, and set up to trim back to that
c       
      meshcheck=0
      do kk=1,nchcktyp
        if(iobjflag(abs(itypchck(kk))).eq.4)meshcheck=1
      enddo
      if(meshcheck.eq.1.or.ishiftflag.eq.4)then
        nmeshloaded=0
        nverts=0
        ntriang=0
        npoly=0
        nsurf=0
        ncont=0
      endif
      if(ishiftflag.eq.4)then
        ifzerod=0
        do kk=1,nshiftyp
          imodobj=abs(itypshift(kk))
          call process_mesh(imodobj,xyscal,zscal,modind,
     &        zgapst,zgapnd,ngaps,iferr,1)
          if(iferr.ne.0)go to 99
          ifon=0
          do imo=1,nobjshifted
            if(iobjshift(imo).eq.itypshift(kk))ifon=imo
          enddo
          if(ifon.gt.0)then
c             
c             if mesh is already on the list, shift surfaces back to zero
c             shift and zero the shifts
c             
            ishift=istrshift(ifon)
            do isurf=iobjsurf(nmeshloaded),
     &          iobjsurf(nmeshloaded)+nsurfobj(nmeshloaded)-1
              call shiftsurface(isurf,shifts(1,ishift),-1,shifted,
     &            ifzerod,zscal)
              shifts(1,ishift)=0.
              shifts(2,ishift)=0.
              shifts(3,ishift)=0.
              shifted(ishift)=.true.
              ishift=ishift+1
            enddo  
          else
c             
c             otherwise, set up the right number of shifts for the points
c             
            nobjshifted=nobjshifted+1
            istrshift(nobjshifted)=ibaseshift
            iobjshift(nobjshifted)=itypshift(kk)
            nitemp=0
            do isurf=iobjsurf(nmeshloaded),
     &          iobjsurf(nmeshloaded)+nsurfobj(nmeshloaded)-1
              nitemp=nitemp+1
              shifts(1,ibaseshift)=0.
              shifts(2,ibaseshift)=0.
              shifts(3,ibaseshift)=0.
              shifted(ibaseshift)=.true.
              ibaseshift=ibaseshift+1
              if(ibaseshift.gt.limshift)then
                print *,'Too many shifts for arrays'
                return
              endif
            enddo
            nitemshift(nobjshifted)=nitemp
            ifon=nobjshifted
          endif
          nitemstot=nitemstot+nitemshift(ifon)
c           
c           get mins/maxes
c           
          do isurf=iobjsurf(nmeshloaded),
     &        iobjsurf(nmeshloaded)+nsurfobj(nmeshloaded)-1
            shiftxmin=min(shiftxmin,surfxmin(isurf))
            shiftymin=min(shiftymin,surfymin(isurf))
            shiftzmin=min(shiftzmin,surfzmin(isurf))
            shiftxmax=max(shiftxmax,surfxmax(isurf))
            shiftymax=max(shiftymax,surfymax(isurf))
            shiftzmax=max(shiftzmax,surfzmax(isurf))
          enddo
        enddo
      endif
      shiftxmin=shiftxmin-boxtol
      shiftxmax=shiftxmax+boxtol
      shiftymin=shiftymin-boxtol
      shiftymax=shiftymax+boxtol
      shiftzmin=shiftzmin-boxtol
      shiftzmax=shiftzmax+boxtol
      nmeshtrim=nmeshloaded
      nvertstrim=nverts
      ntriangtrim=ntriang
      npolytrim=npoly
      nsurftrim=nsurf
      nconttrim=ncont
c       
c       initialize the shift flag and count arrays
c       
      do i=1,nitemstot
        needshift(i)=.true.
        ntrials(i)=0
      enddo
      nfinal=0
c       
      nfail=0
      nfinal=0
      nround=0
      nexclude=0

      do while(nfinal+nfail+nexclude.lt.nitemstot)
        if (nround.gt.0.and.manyrandom.eq.0)then
          write(*,'(a,i4,a,i5,a,i5,a,i5,a,$)')char(13)//'Round',nround,
     &        ':',nfinal,' moved,',nfail,' failed,',nexclude,
     &        ' excluded'
          call flush(6)
        endif
        nround=nround+1 
        locshft=1
        do kk=1,nshiftyp
          ifon=0
          do imo=1,nobjshifted
            if(iobjshift(imo).eq.itypshift(kk))ifon=imo
          enddo
          ishift=istrshift(ifon)
          if(ishiftflag.eq.1)then
c             
c             loop through the objects looking for ones to shift
c             
            do iobjref=1,nmt
c               
              if(icolor(iobjref).eq.itypshift(kk))then
                indref=indstrt(iobjref)
                limref=indref+npntobj(iobjref)-2
c                 
c                 on first trial, check if outside boundaries and fail
c                 DNM 7/3/02: this was a single if test, but SGI was calling
c                 outside_boudary even when nbound was zero
c                 
                if(nround.eq.1.and.ifexcludeout.ne.0.and.
     &              nbound.gt.0)then
                  if (outside_boundary(nbound,listbound,zbound,xmt,ymt,
     &                zmt,indref,limref+1,0.,0.,0.))then
                    shifted(ishift)=.false.
                    needshift(locshft)=.false.
                    needcheck(locshft)=.false.
                    nexclude=nexclude+1
                  endif
                endif
c                 
                do while(needshift(locshft))
c                   
                  call getranshift(ntrials(locshft),ntrialcycle,cyclefac,
     &                ranmin,ranmax,ranzrel,shifts(1,ishift),delxyz,
     &                glbxmin(iobjref),glbxmax(iobjref),glbymin(iobjref),
     &                glbymax(iobjref),glbzmin(iobjref),glbzmax(iobjref),
     &                shiftxmin,shiftxmax,shiftymin,shiftymax,shiftzmin,
     &                shiftzmax,zscal,iseed,nbound,listbound,zbound,xmt,ymt,
     &                zmt,indref,limref+1)
c                   
c                   now shift the line and all its components
c                   
                  if(delx.ne.0..or.dely.ne.0..or.delz.ne.0.)then
                    call shiftline(iobjref,indref,limref,delx,dely,delz,
     &                  xmt,ymt,zmt, xmin,xmax,ymin,ymax,zmin,zmax,
     &                  glbxmin,glbxmax,glbymin,glbymax,glbzmin,glbzmax,
     &                  zscal)
                    refxmin=glbxmin(iobjref)
                    refymin=glbymin(iobjref)
                    refzmin=glbzmin(iobjref)
                    refxmax=glbxmax(iobjref)
                    refymax=glbymax(iobjref)
                    refzmax=glbzmax(iobjref)
c                     
c                     set up for loops through shifted and fixed objects
c                     skip through all and restore if at end of trials
c                     
                    passing=ntrials(locshft).le.maxtrials
                  else
                    passing=.false.
                  endif
c                   
                  ifcheckfixed=0
                  ncheck=nshiftyp
                  jlocshft=1
                  do while(passing.and.ifcheckfixed.le.1)
                    icheck=1
                    do while(passing.and.icheck.le.ncheck)
                      if(ifcheckfixed.eq.0)then
                        itycheck=itypshift(icheck)
                        jprob=1
                      else
                        itycheck=itypchck(icheck)
                        jprob=iuseprob(icheck)
                      endif
                      call get_distlims(nrestrict,delnear,probnear,
     &                    limprobs,jprob,distlim,distabs,probuse,deluse)
                      if(iobjflag(itycheck).eq.1)then
c                         
c                         THE LINE AGAINST LINES
c                         
                        iobjneigh=1
                        do while(passing.and.iobjneigh.le.nmt)
                          if(icolor(iobjneigh).eq.itycheck)then
                            distmin=1.01*distlim
                            docheck=
     &                          glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &                          refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &                          glbymin(iobjneigh)-refymax.lt.distmin.and.
     &                          refymin-glbymax(iobjneigh).lt.distmin.and.
     &                          glbzmin(iobjneigh)-refzmax.lt.distmin.and.
     &                          refzmin-glbzmax(iobjneigh).lt.distmin
                            if(ifcheckfixed.eq.0)then
                              docheck=docheck.and.iobjneigh.ne.iobjref
                              if(ifcheckunshifted.eq.0)docheck=docheck.and.
     &                            .not.needshift(jlocshft)
                              jlocshft=jlocshft+1
                            endif
                            if(docheck)then
                              dminsq=distmin**2
                              
                              indneigh=indstrt(iobjneigh)
                              limneigh=indneigh+npntobj(iobjneigh)-2
                              ineigh=indneigh
                              do while(passing.and.ineigh.le.limneigh)
                                if(xmin(ineigh)-refxmax.lt.distmin.and.
     &                              refxmin-xmax(ineigh).lt.distmin.and.
     &                              ymin(ineigh)-refymax.lt.distmin.and.
     &                              refymin-ymax(ineigh).lt.distmin.and.
     &                              zmin(ineigh)-refzmax.lt.distmin.and.
     &                              refzmin-zmax(ineigh).lt.distmin)then
                                  iref=indref
                                  in=ineigh
                                  do while(passing.and.iref.le.limref)
                                    if(xmin(in)-xmax(iref).lt.distmin.and.
     &                                  xmin(iref)-xmax(in).lt.distmin.and.
     &                                  ymin(in)-ymax(iref).lt.distmin.and.
     &                                  ymin(iref)-ymax(in).lt.distmin.and.
     &                                  zmin(in)-zmax(iref).lt.distmin.and.
     &                                  zmin(iref)-zmax(in).lt.distmin)then
                                      call segment_dist(xmt(iref),ymt(iref),
     &                                    zmt(iref),xmt(iref+1),ymt(iref+1),
     &                                    zmt(iref+1), xmt(in),ymt(in),
     &                                    zmt(in), xmt(in+1),
     &                                    ymt(in+1),zmt(in+1),tref,
     &                                    tneigh,dsqr)
                                      if(dsqr.lt.dminsq)then
                                        dminsq=dsqr
                                        distmin=sqrt(dsqr)
                                        passing=distmin.ge.distabs
                                      endif
                                    endif
                                    iref=iref+1
                                  enddo
                                endif
                                ineigh=ineigh+1
                              enddo
c                               
c                               got a distance now - see if need to reject
c                               
                              if(passing.and.distmin.lt.distlim)
     &                            passing=ran(iseed).lt.
     &                            probuse(int(distmin/deluse+1.))
                            endif
                          endif
                          iobjneigh=iobjneigh+1
                        enddo
                      elseif(iobjflag(itycheck).eq.2)then
c                         
c                         THE LINE AGAINST POINTS
c                         
                        iobjneigh=1
                        do while(passing.and.iobjneigh.le.nmt)
                          if(icolor(iobjneigh).eq.itycheck)then
                            distmin=1.01*distlim
                            docheck=
     &                          glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &                          refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &                          glbymin(iobjneigh)-refymax.lt.distmin.and.
     &                          refymin-glbymax(iobjneigh).lt.distmin.and.
     &                          glbzmin(iobjneigh)-refzmax.lt.distmin.and.
     &                          refzmin-glbzmax(iobjneigh).lt.distmin
                            if(docheck)then

                              indneigh=indstrt(iobjneigh)
                              limneigh=indneigh+npntobj(iobjneigh)-2
                              in=indneigh
                              do while(passing.and.in.le.limneigh)
                                distmin=1.01*distlim
                                if(xmin(in)-refxmax.lt.distmin.and.
     &                              refxmin-xmax(in).lt.distmin.and.
     &                              ymin(in)-refymax.lt.distmin.and.
     &                              refymin-ymax(in).lt.distmin.and.
     &                              zmin(in)-refzmax.lt.distmin.and.
     &                              refzmin-zmax(in).lt.distmin)then
                                  
                                  sizen=0.
                                  if(ifscatsurf.ne.0)then
                                    is=indsize(iobjneigh)+in-indneigh
                                    sizen=sizes(is)
                                  endif

                                  iref=indref
                                  do while(passing.and.iref.le.limref)
                                    if(xmin(in)-xmax(iref).lt.distmin.and.
     &                                  xmin(iref)-xmax(in).lt.distmin.and.
     &                                  ymin(in)-ymax(iref).lt.distmin.and.
     &                                  ymin(iref)-ymax(in).lt.distmin.and.
     &                                  zmin(in)-zmax(iref).lt.distmin.and.
     &                                  zmin(iref)-zmax(in).lt.distmin)then
                                      call point_line_dist(xmt(iref),
     &                                    ymt(iref), zmt(iref),
     &                                    xmt(iref+1),ymt(iref+1), 
     &                                    zmt(iref+1),xmt(in), ymt(in),
     &                                    zmt(in), tref,dsqr)
                                      dist=sqrt(dsqr)-sizen
                                      if(dist.lt.distmin)then
                                        distmin=dist
                                        passing=distmin.ge.distabs
                                      endif
                                    endif
                                    iref=iref+1
                                  enddo
c                                   
c                                   got a distance now - see if reject
c                                   
                                  if(passing.and.distmin.lt.distlim)
     &                                passing=ran(iseed).lt.
     &                                probuse(int(distmin/deluse+1.))
                                endif
                                in=in+1
                              enddo
                            endif
                          endif
                          iobjneigh=iobjneigh+1
                        enddo
                      endif
                      icheck=icheck+1
                    enddo
                    ifcheckfixed=ifcheckfixed+1
                    ncheck=nchcktyp
                  enddo
c                   
c                   got through all stage 1 tests: if passed, set flag; if
c                   not, test for trial limit and restore to zero shift
c                   
                  if(passing)then
                    needshift(locshft)=.false.
                    needcheck(locshft)=.true.
                    needtot=needtot+1
                  elseif(ntrials(locshft).ge.maxtrials)then
                    call shiftline(iobjref,indref,limref, -shifts(1,ishift),
     &                  -shifts(2,ishift),-shifts(3,ishift),
     &                  xmt,ymt,zmt, xmin,xmax,ymin,ymax,zmin,zmax,
     &                  glbxmin,glbxmax,glbymin,glbymax,glbzmin,glbzmax,
     &                  zscal)
                    shifts(1,ishift)=0.
                    shifts(2,ishift)=0.
                    shifts(3,ishift)=0.
                    shifted(ishift)=.false.
                    needshift(locshft)=.false.
                    needcheck(locshft)=.false.
                    nfail=nfail+1
                  endif
                enddo
c                 
c                 advance to next item to shift
c                 
                ishift=ishift+1
                locshft=locshft+1
              endif
            enddo
          elseif(ishiftflag.eq.2)then
c             
c             POINTS TO SHIFT: LOOP THROUGH OBJECTS
c             
            do iobjref=1,nmt
              if(icolor(iobjref).eq.itypshift(kk))then
c                 
                indref=indstrt(iobjref)
                limref=indref+npntobj(iobjref)-1
                do iref=indref,limref
c                   
c                   on first trial, check if outside boundaries and fail
c                   
                  if(nround.eq.1.and.ifexcludeout.ne.0.and. nbound.gt.0) then
                    if (outside_boundary(nbound,listbound,zbound,xmt,ymt,
     &                  zmt,iref,iref,0.,0.,0.))then
                      shifted(ishift)=.false.
                      needshift(locshft)=.false.
                      needcheck(locshft)=.false.
                      nexclude=nexclude+1
                    endif
                  endif
c                   
                  do while(needshift(locshft))
c                     
                    call getranshift(ntrials(locshft),ntrialcycle,cyclefac,
     &                  ranmin,ranmax,ranzrel,shifts(1,ishift),delxyz,
     &                  xmin(iref),xmax(iref),ymin(iref),
     &                  ymax(iref),zmin(iref),zmax(iref),
     &                  shiftxmin,shiftxmax,shiftymin,shiftymax,shiftzmin,
     &                  shiftzmax,zscal,iseed,nbound,listbound,zbound,
     &                  xmt,ymt, zmt,iref,iref)
c                     
c                     now shift the point and all its components
c                     
                    if(delx.ne.0..or.dely.ne.0..or.delz.ne.0.)then
                      call shiftpoint(iobjref,iref,delx,dely,delz,
     &                    xmt,ymt,zmt, xmin,xmax,ymin,ymax,zmin,zmax,
     &                    glbxmin,glbxmax,glbymin,glbymax,glbzmin,glbzmax,
     &                    zscal)
                      refxmin=xmin(iref)
                      refymin=ymin(iref)
                      refzmin=zmin(iref)
                      refxmax=xmax(iref)
                      refymax=ymax(iref)
                      refzmax=zmax(iref)
                      sizer=0.
                      if(ifscatsurf.ne.0)then
                        is=indsize(iobjref)+iref-indref
                        sizer=sizes(is)
                      endif
c                       
c                       set up for loops through shifted and fixed objects
c                       skip through all and restore if at end of trials
c                       
                      passing=ntrials(locshft).le.maxtrials
                    else
                      passing=.false.
                    endif
c                     
                    ifcheckfixed=0
                    ncheck=nshiftyp
                    jlocshft=1
                    do while(passing.and.ifcheckfixed.le.1)
                      icheck=1
                      do while(passing.and.icheck.le.ncheck)
                        if(ifcheckfixed.eq.0)then
                          itycheck=itypshift(icheck)
                          jprob=1
                        else
                          itycheck=itypchck(icheck)
                          jprob=iuseprob(icheck)
                        endif
                        call get_distlims(nrestrict,delnear,probnear,
     &                      limprobs,jprob,distlim,distabs,probuse,deluse)
                        if(iobjflag(itycheck).eq.1)then
c                           
c                           THE POINT AGAINST LINES
c                           
                          iobjneigh=1
                          do while(passing.and.iobjneigh.le.nmt)
                            if(icolor(iobjneigh).eq.itycheck)then
                              distmin=1.01*distlim
                              docheck=
     &                            glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &                            refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &                            glbymin(iobjneigh)-refymax.lt.distmin.and.
     &                            refymin-glbymax(iobjneigh).lt.distmin.and.
     &                            glbzmin(iobjneigh)-refzmax.lt.distmin.and.
     &                            refzmin-glbzmax(iobjneigh).lt.distmin
                              if(docheck)then
                                dminsq=distmin**2
                                
                                indneigh=indstrt(iobjneigh)
                                limneigh=indneigh+npntobj(iobjneigh)-2
                                in=indneigh
                                do while(passing.and.in.le.limneigh)
                                  if(xmin(in)-refxmax.lt.distmin.and.
     &                                refxmin-xmax(in).lt.distmin.and.
     &                                ymin(in)-refymax.lt.distmin.and.
     &                                refymin-ymax(in).lt.distmin.and.
     &                                zmin(in)-refzmax.lt.distmin.and.
     &                                refzmin-zmax(in).lt.distmin)then
                                    call point_line_dist(xmt(in),ymt(in),
     &                                  zmt(in),xmt(in+1),ymt(in+1),
     &                                  zmt(in+1),xmt(iref), ymt(iref),
     &                                  zmt(iref), tref,dsqr)
                                    dist=sqrt(dsqr)-sizer
                                    if(dist.lt.distmin)then
                                      distmin=dist
                                      passing=distmin.ge.distabs
                                    endif
                                  endif
                                  in=in+1
                                enddo
c                                 
c                                 got a distance now - see if need to reject
c                                 
                                if(passing.and.distmin.lt.distlim)
     &                              passing=ran(iseed).lt.
     &                              probuse(int(distmin/deluse+1.))
                              endif
                            endif
                            iobjneigh=iobjneigh+1
                          enddo

                        elseif(iobjflag(itycheck).eq.2)then
c                           
c                           THE POINT AGAINST POINTS
c                           
                          iobjneigh=1
                          do while(passing.and.iobjneigh.le.nmt)
                            distmin=1.01*distlim
                            if(icolor(iobjneigh).eq.itycheck.and.
     &                          glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &                          refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &                          glbymin(iobjneigh)-refymax.lt.distmin.and.
     &                          refymin-glbymax(iobjneigh).lt.distmin.and.
     &                          glbzmin(iobjneigh)-refzmax.lt.distmin.and.
     &                          refzmin-glbzmax(iobjneigh).lt.distmin)then
                              indneigh=indstrt(iobjneigh)
                              limneigh=indneigh+npntobj(iobjneigh)-1
                              in=indneigh
                              if(ifscatsurf.ne.0)
     &                            isbase=indsize(iobjneigh)-indneigh
                              do while(passing.and.in.le.limneigh)
                                docheck=xmin(in)-refxmax.lt.distmin.and.
     &                              refxmin-xmax(in).lt.distmin.and.
     &                              ymin(in)-refymax.lt.distmin.and.
     &                              refymin-ymax(in).lt.distmin.and.
     &                              zmin(in)-refzmax.lt.distmin.and.
     &                              refzmin-zmax(in).lt.distmin
                                if(ifcheckfixed.eq.0)then
                                  docheck=docheck.and.in.ne.iref
                                  if(ifcheckunshifted.eq.0)docheck=
     &                                docheck.and..not.needshift(jlocshft)
                                  jlocshft=jlocshft+1
                                endif
                                if(docheck)then
                                  sizen=0.
                                  if(ifscatsurf.ne.0)sizen=sizes(in+isbase)
                                  dist=sqrt((xmt(in)-xmt(iref))**2+
     &                                (ymt(in)-ymt(iref))**2+
     &                                (zmt(in)-zmt(iref))**2)-sizen-sizer
c                                   
c                                   got a distance now - see if reject
c                                   
                                  passing=dist.ge.distabs
                                  if(passing.and.dist.lt.distlim)
     &                                passing=ran(iseed).lt.
     &                                probuse(int(dist/deluse+1.))
                                endif
                                in=in+1
                              enddo
                            endif
                            iobjneigh=iobjneigh+1
                          enddo
                        endif
                        icheck=icheck+1
                      enddo
                      ifcheckfixed=ifcheckfixed+1
                      ncheck=nchcktyp
                    enddo
c                     
c                     got through all stage 1 tests: if passed, set flag; if
c                     not, test for trial limit and restore to zero shift
c                     
                    if(passing)then
                      needshift(locshft)=.false.
                      needcheck(locshft)=.true.
                      needtot=needtot+1
                    elseif(ntrials(locshft).ge.maxtrials)then
                      call shiftpoint(iobjref,iref, -shifts(1,ishift),
     &                    -shifts(2,ishift),-shifts(3,ishift),
     &                    xmt,ymt,zmt, xmin,xmax,ymin,ymax,zmin,zmax,
     &                    glbxmin,glbxmax,glbymin,glbymax,glbzmin,glbzmax,
     &                    zscal)
                      shifts(1,ishift)=0.
                      shifts(2,ishift)=0.
                      shifts(3,ishift)=0.
                      shifted(ishift)=.false.
                      needshift(locshft)=.false.
                      needcheck(locshft)=.false.
                      nfail=nfail+1
                    endif
                  enddo
c                   
c                   advance to next item to shift
c                   
                  ishift=ishift+1
                  locshft=locshft+1
                enddo
              endif
            enddo               
          else
c             
c             MESHES TO SHIFT: find the mesh in the loaded ones
c             
            do i=1,nmeshloaded
              if(iobjmesh(i).eq.abs(itypshift(kk)))imesh=i
            enddo
c             
c             loop through surfaces
c             
            do isurf=iobjsurf(imesh),iobjsurf(imesh)+nsurfobj(imesh)-1
c               
c               on first trial, check if outside boundaries and fail
c               
              if(nround.eq.1.and.ifexcludeout.ne.0.and. nbound.gt.0) then
                if (outside_boundary(nbound,listbound,zbound,xmt,ymt,
     &              zmt,isurf,0,0.,0.,0.))then
                  shifted(ishift)=.false.
                  needshift(locshft)=.false.
                  needcheck(locshft)=.false.
                  nexclude=nexclude+1
                endif
              endif
c               
              do while(needshift(locshft))
c                 
                call getranshift(ntrials(locshft),ntrialcycle,cyclefac,
     &              ranmin,ranmax,ranzrel,shifts(1,ishift),delxyz,
     &              surfxmin(isurf),surfxmax(isurf),surfymin(isurf),
     &              surfymax(isurf),surfzmin(isurf),surfzmax(isurf),
     &              shiftxmin,shiftxmax,shiftymin,shiftymax,shiftzmin,
     &              shiftzmax,zscal,iseed,nbound,listbound,zbound,
     &              xmt,ymt, zmt,isurf,0)
c                 
c                 now shift the surface and all its components
c                 
                if(delx.ne.0..or.dely.ne.0..or.delz.ne.0.)then
                  ifzerod=0
                  call shiftsurface(isurf,delxyz,1,modind,ifzerod,zscal)
                  refxmin=surfxmin(isurf)
                  refymin=surfymin(isurf)
                  refzmin=surfzmin(isurf)
                  refxmax=surfxmax(isurf)
                  refymax=surfymax(isurf)
                  refzmax=surfzmax(isurf)
c                   
c                   set up for loops through shifted and fixed objects
c                   skip through all and restore if at end of trials
c                   
                  passing=ntrials(locshft).le.maxtrials
                else
                  passing=.false.
                endif
c                 
                icheck=1
                do while(passing.and.icheck.le.nchcktyp)
                  jprob=iuseprob(icheck)
                  call get_distlims(nrestrict,delnear,probnear,
     &                limprobs,jprob,distlim,distabs,probuse,deluse)
                  itycheck=itypchck(icheck)
                  iobjneigh=1
                  if(iobjflag(itycheck).eq.4)iobjneigh=nmt+1
                  do while(passing.and.iobjneigh.le.nmt)
                    distmin=1.01*distlim
                    indneigh=indstrt(iobjneigh)
                    if(icolor(iobjneigh).eq.itycheck.and.
     &                  glbxmin(iobjneigh)-refxmax.lt.distmin.and.
     &                  refxmin-glbxmax(iobjneigh).lt.distmin.and.
     &                  glbymin(iobjneigh)-refymax.lt.distmin.and.
     &                  refymin-glbymax(iobjneigh).lt.distmin.and.
     &                  glbzmin(iobjneigh)-refzmax.lt.distmin.and.
     &                  refzmin-glbzmax(iobjneigh).lt.distmin)then
                      if(iobjflag(itycheck).eq.1)then
c                         
c                         THE MESH AGAINST A LINE
c                         
                        limneigh=indneigh+npntobj(iobjneigh)-2
                        call check_line_surface(isurf,indneigh,limneigh,
     &                      xmin,xmax,ymin,ymax,zmin,zmax,xmt,ymt,zmt,
     &                      distmin,distabs,zscal)
                        passing=distmin.ge.distabs
                        if(passing.and.distmin.lt.distlim)
     &                      passing=ran(iseed).lt.
     &                      probuse(int(distmin/deluse+1.))
                      else
c                         
c                         THE MESH AGAINST A SET OF POINTS
c                         
                        limneigh=indneigh+npntobj(iobjneigh)-1
                        in=indneigh
                        if(ifscatsurf.ne.0)isbase=indsize(iobjneigh)-indneigh
                        do while(passing.and.in.le.limneigh)
                          distmin=1.01*distlim
                          if(xmin(in)-refxmax.lt.distmin.and.
     &                        refxmin-xmax(in).lt.distmin.and.
     &                        ymin(in)-refymax.lt.distmin.and.
     &                        refymin-ymax(in).lt.distmin.and.
     &                        zmin(in)-refzmax.lt.distmin.and.
     &                        refzmin-zmax(in).lt.distmin)then
                            sizen=0.
                            if(ifscatsurf.ne.0)sizen=sizes(in+isbase)
                            call check_point_surface(isurf,xmt(in),ymt(in),
     &                          zmt(in), xmin(in),xmax(in),ymin(in),ymax(in),
     &                          zmin(in),zmax(in),sizen,distmin,
     &                          distabs,zscal)
                            passing=distmin.ge.distabs
                            if(passing.and.distmin.lt.distlim)
     &                          passing=ran(iseed).lt.
     &                          probuse(int(distmin/deluse+1.))
                          endif
                          in=in+1
                        enddo
                      endif
                    endif
                    iobjneigh=iobjneigh+1
                  enddo
                  icheck=icheck+1
                enddo
c                 
c                 NOW CHECK THIS MESH AGAINST OTHER SHIFTED MESHES
c                 
                icheck=1
                jlocshft=1
                jprob=1
                call get_distlims(nrestrict,delnear,probnear,
     &              limprobs,jprob,distlim,distabs,probuse,deluse)
                do while(passing.and.icheck.le.nshiftyp)
                  do i=1,nmeshloaded
                    if(iobjmesh(i).eq.abs(itypshift(icheck)))jmesh=i
                  enddo
                  jsurf=iobjsurf(jmesh)
                  do while(passing.and.
     &                jsurf.le.iobjsurf(jmesh)+nsurfobj(jmesh)-1)
                    distmin=1.01*distlim
                    docheck=surfxmin(jsurf)-refxmax.lt.distmin.and.
     &                  refxmin-surfxmax(jsurf).lt.distmin.and.
     &                  surfymin(jsurf)-refymax.lt.distmin.and.
     &                  refymin-surfymax(jsurf).lt.distmin.and.
     &                  surfzmin(jsurf)-refzmax.lt.distmin.and.
     &                  refzmin-surfzmax(jsurf).lt.distmin
                    docheck=docheck.and.jsurf.ne.isurf
                    if(ifcheckunshifted.eq.0)docheck=docheck.and.
     &                  .not.needshift(jlocshft)
                    jlocshft=jlocshft+1

                    if(docheck)then
                      call check_two_meshes(isurf,jsurf,distmin,
     &                    distabs,zscal)
                      passing=distmin.ge.distabs
                      if(passing.and.distmin.lt.distlim)
     &                    passing=ran(iseed).lt.
     &                    probuse(int(distmin/deluse+1.))
                    endif
                    jsurf=jsurf+1
                  enddo

                  icheck=icheck+1
                enddo
c                 got through all stage 1 tests: if passed, set flag; if
c                 not, test for trial limit and restore to zero shift
c                 
                if(passing)then
                  needshift(locshft)=.false.
                  needcheck(locshft)=.true.
                  needtot=needtot+1
                elseif(ntrials(locshft).ge.maxtrials)then
                  ifzerod=0
                  call shiftsurface(isurf,shifts(1,ishift),-1,modind,
     &                ifzerod,zscal)
                  shifts(1,ishift)=0.
                  shifts(2,ishift)=0.
                  shifts(3,ishift)=0.
                  shifted(ishift)=.false.
                  needshift(locshft)=.false.
                  needcheck(locshft)=.false.
                  nfail=nfail+1
                endif
              enddo
c               
c               advance to next item to shift
c               
              ishift=ishift+1
              locshft=locshft+1
            enddo
          endif
        enddo
c         
c         FINISHED STAGE 1, NOW LOOP ON THE MESHES TO BE CHECKED
c         
        do icheck=1,nchcktyp
          jprob=iuseprob(icheck)
          call get_distlims(nrestrict,delnear,probnear,
     &        limprobs,jprob,distlim,distabs,probuse,deluse)
          itycheck=itypchck(icheck)
          if(iobjflag(itycheck).eq.4.and.needtot.gt.0)then
            imesh=0
            imodobj=abs(itycheck)
            do i=1,nmeshloaded
              if(iobjmesh(i).eq.imodobj)imesh=i
            enddo
            if(imesh.eq.0)then
              call process_mesh(imodobj,xyscal,zscal,modind,
     &            zgapst,zgapnd,ngaps,iferr,1)
              if(iferr.ne.0)then
c                 
c                 if not enough space, trim back, try again
c                 
                nmeshloaded=nmeshtrim
                nverts=nvertstrim
                ntriang=ntriangtrim
                npoly=npolytrim
                nsurf=nsurftrim
                ncont=nconttrim
                call process_mesh(imodobj,xyscal,zscal,modind,
     &              zgapst,zgapnd,ngaps,iferr,1)
                if(iferr.ne.0)go to 99
              endif
              imesh=nmeshloaded
            endif
c             
c             go through surfaces checking for distances
c             
            do isurf=iobjsurf(imesh),iobjsurf(imesh)+nsurfobj(imesh)-1
              refxmin=surfxmin(isurf)
              refymin=surfymin(isurf)
              refzmin=surfzmin(isurf)
              refxmax=surfxmax(isurf)
              refymax=surfymax(isurf)
              refzmax=surfzmax(isurf)
              locshft=1
              do kk=1,nshiftyp
                ifon=0
                do imo=1,nobjshifted
                  if(iobjshift(imo).eq.itypshift(kk))ifon=imo
                enddo
                ishift=istrshift(ifon)
                if(ishiftflag.eq.1)then
c                   
c                   CHECK LINES AGAINST THE SURFACE
c                   
                  do iobjref=1,nmt
                    if(icolor(iobjref).eq.itypshift(kk))then
                      if(needcheck(locshft))then
                        indref=indstrt(iobjref)
                        limref=indref+npntobj(iobjref)-2
                        distmin=1.01*distlim
                        if(glbxmin(iobjref)-refxmax.lt.distmin.and.
     &                      refxmin-glbxmax(iobjref).lt.distmin.and.
     &                      glbymin(iobjref)-refymax.lt.distmin.and.
     &                      refymin-glbymax(iobjref).lt.distmin.and.
     &                      glbzmin(iobjref)-refzmax.lt.distmin.and.
     &                      refzmin-glbzmax(iobjref).lt.distmin)then
                          call check_line_surface(isurf,indref,limref,
     &                        xmin,xmax,ymin,ymax,zmin,zmax,xmt,ymt,zmt,
     &                        distmin,distabs,zscal)
                          passing=distmin.ge.distabs
                          if(passing.and.distmin.lt.distlim)
     &                        passing=ran(iseed).lt.
     &                        probuse(int(distmin/deluse+1.))
                          if(.not.passing)then
c                             
c                             If it doesn't pass, toggle the two flags and
c                             shift the line back to zero shift
c                             
                            needcheck(locshft)=.false.
                            needshift(locshft)=.true.
                            needtot=needtot-1
                            call shiftline(iobjref,indref,limref,
     &                          -shifts(1,ishift),-shifts(2,ishift),
     &                          -shifts(3,ishift), xmt,ymt,zmt,
     &                          xmin,xmax,ymin,ymax,zmin,zmax, glbxmin,
     &                          glbxmax,glbymin,glbymax,glbzmin,
     &                          glbzmax,zscal)
                            shifts(1,ishift)=0.
                            shifts(2,ishift)=0.
                            shifts(3,ishift)=0.
                          endif
                        endif
                      endif
                      ishift=ishift+1
                      locshft=locshft+1
                    endif
                  enddo
                elseif(ishiftflag.eq.2)then
c                   
c                   CHECK POINTS AGAINST THE SURFACE
c                   
                  do iobjref=1,nmt
                    if(icolor(iobjref).eq.itypshift(kk))then
                      indref=indstrt(iobjref)
                      limref=indref+npntobj(iobjref)-1
                      distmin=1.01*distlim
                      if(glbxmin(iobjref)-refxmax.lt.distmin.and.
     &                    refxmin-glbxmax(iobjref).lt.distmin.and.
     &                    glbymin(iobjref)-refymax.lt.distmin.and.
     &                    refymin-glbymax(iobjref).lt.distmin.and.
     &                    glbzmin(iobjref)-refzmax.lt.distmin.and.
     &                    refzmin-glbzmax(iobjref).lt.distmin)then
                        if(ifscatsurf.ne.0)isbase=indsize(iobjref)-indref

                        do iref=indref,limref
                          if(needcheck(locshft))then
                            distmin=1.01*distlim
                            if(xmin(iref)-refxmax.lt.distmin.and.
     &                          refxmin-xmax(iref).lt.distmin.and.
     &                          ymin(iref)-refymax.lt.distmin.and.
     &                          refymin-ymax(iref).lt.distmin.and.
     &                          zmin(iref)-refzmax.lt.distmin.and.
     &                          refzmin-zmax(iref).lt.distmin)then
                              sizer=0.
                              if(ifscatsurf.ne.0)sizer=sizes(iref+isbase)

                              call check_point_surface(isurf,xmt(iref),
     &                            ymt(iref),zmt(iref),
     &                            xmin(iref),xmax(iref),ymin(iref),
     &                            ymax(iref),zmin(iref),zmax(iref),
     &                            sizer,distmin,distabs,zscal)
                              passing=distmin.ge.distabs
                              if(passing.and.distmin.lt.distlim)
     &                            passing=ran(iseed).lt.
     &                            probuse(int(distmin/deluse+1.))
                              if(.not.passing)then
c                                 
c                                 if it doesn't pass, shift point back to 0
c                                 
                                needcheck(locshft)=.false.
                                needshift(locshft)=.true.
                                needtot=needtot-1
                                call shiftpoint(iobjref,iref,
     &                              -shifts(1,ishift),-shifts(2,ishift),
     &                              -shifts(3,ishift), xmt,ymt,zmt,
     &                              xmin,xmax,ymin,ymax,zmin,zmax,glbxmin,
     &                              glbxmax,glbymin,glbymax,glbzmin,glbzmax,
     &                              zscal)
                                shifts(1,ishift)=0.
                                shifts(2,ishift)=0.
                                shifts(3,ishift)=0.
                              endif
                            endif
                          endif
                          locshft=locshft+1
                          ishift=ishift+1
                        enddo
                      else
c                         
c                         If skip over a whole contour, advance both indexes
c                         to account for all the points skipped
c                         
                        locshft=locshft+npntobj(iobjref)
                        ishift=ishift+npntobj(iobjref)
                      endif
                    endif
                  enddo
                else
c                   
c                   CHECK OTHER SURFACES AGAINST THIS SURFACE
c                   
                  ifzerod=0
                  do i=1,nmeshloaded
                    if(iobjmesh(i).eq.itypshift(kk))jmesh=i
                  enddo
                  do jsurf=iobjsurf(jmesh),iobjsurf(jmesh)+nsurfobj(jmesh)-1
                    distmin=1.01*distlim
                    docheck=needcheck(locshft).and.
     &                  surfxmin(jsurf)-refxmax.lt.distmin.and.
     &                  refxmin-surfxmax(jsurf).lt.distmin.and.
     &                  surfymin(jsurf)-refymax.lt.distmin.and.
     &                  refymin-surfymax(jsurf).lt.distmin.and.
     &                  surfzmin(jsurf)-refzmax.lt.distmin.and.
     &                  refzmin-surfzmax(jsurf).lt.distmin

                    if(docheck)then
                      call check_two_meshes(isurf,jsurf,distmin,
     &                    distabs,zscal)
                      passing=distmin.ge.distabs
                      if(passing.and.distmin.lt.distlim)
     &                    passing=ran(iseed).lt.
     &                    probuse(int(distmin/deluse+1.))
                      if(.not.passing)then
                        needcheck(locshft)=.false.
                        needshift(locshft)=.true.
                        needtot=needtot-1
                        call shiftsurface(jsurf,shifts(1,ishift),-1,modind,
     &                      ifzerod,zscal)
                        shifts(1,ishift)=0.
                        shifts(2,ishift)=0.
                        shifts(3,ishift)=0.
                      endif
                    endif
                    locshft=locshft+1
                    ishift=ishift+1
                  enddo
                endif
              enddo
            enddo
          endif
        enddo
c         
c         finalize anyone still listed as "needcheck"
c         
        do i=1,nitemstot
          if(needcheck(i))then
            needcheck(i)=.false.
            nfinal=nfinal+1
c             write(*,'(2i5,3f8.4)')i,ntrials(i),(shifts(j,i),j=1,3)
          endif
        enddo
      enddo
c       
      ntrialtot=0
      nitemsum=0
      do i=1,nitemstot
        if (ntrials(i).gt.0)then
          nitemsum=nitemsum+1
          ntrialtot=ntrialtot+ntrials(i)
        endif
      enddo
      avgtrial=float(ntrialtot)/nitemsum
c       
      write(*,104)nfinal,nfail,nexclude,avgtrial
104   format(i5,' items shifted;',i5,' could not be;',i5,
     &    ' excluded;',f7.1, ' trials/item')
      return
99    print *,'Unable to load meshes'
      return
      end

      subroutine get_distlims(nrestrict,delnear,probnear,
     &    limprobs,jprob,distlim,distabs,probuse,deluse)
      real*4 probnear(limprobs,*),probuse(*),delnear(*)
      integer*4 nrestrict(*)
      distlim=delnear(jprob)*nrestrict(jprob)
      deluse=delnear(jprob)
      distabs=0.
      i=1
      do while (i.le.nrestrict(jprob).and.probnear(i,jprob).eq.0)
        distabs=i*deluse
        i=i+1
      enddo
      do i=1,nrestrict(jprob)
        probuse(i)=probnear(i,jprob)
      enddo
      return
      end

c       $Id$
c       Log at end
c       
      subroutine tiltali(ifdidalign,ifAlignDone,resmean,ibaseRes,ibaseOnAlign,
     &    iview)
      implicit none
      include 'alivar.inc'
      include 'tltcntrl.inc'
      include 'smallmodel.inc'
c       
c       IF MAXVAR IS NOT BIGGER THAN MAXMETRO, NEED TO DIMENSION
c       var to maxmetro
c       
      integer maxvar,maxmetro,maxMetroTrials
      parameter (maxvar=5*maxview,maxmetro=3600,maxMetroTrials=5)

      integer*4 ifdidalign, iview,ifAlignDone,ibaseRes,ibaseOnAlign
      real*4 resmean(*)
c       
      real*4 var(maxvar),grad(maxmetro),h(maxmetro*(maxmetro+3))
      real*4 erlist(100),varsave(maxvar)
      external funct
      logical firsttime
      double precision error
      common /functfirst/ firsttime
      common /bigharr/ h
      real*4 dtor
      data dtor/0.0174532/
      integer*4 imintiltsolv,itry,isolmininit,i,isolmin,nprojpt,iv
      real*4 tltslvmin,tltslvmax,ang,tltran,ermin,ermininit,finit
      integer*4 nvarsrch,ifmaptilt,isolve,jpt,kpt,nvargeom,ier,kount,nsum
      real*4 f,ffinal,rsum
      integer*4 ior,ipt,ivor,j,metroLoop
      real*4 trialScale(maxMetroTrials) /1.0, 0.9, 1.1, 0.75, 0.5/
      real*4 restmp(25000)
      logical itemOnList

      ifanyalf=0
      call proc_model(xcen,ycen,xdelt,ydelt,xorig,yorig, scalexy,
     &    nvuall,mininview,iview,nvlocal,iobjseq,nobjdo,mapFileToView,
     &    mapViewToFile, xx,yy,isecview,maxprojpt,maxreal,irealstr,
     &    iobjali, nview, nprojpt,nrealpt)
      ifdidalign=0
      if(nview.ge.minvtiltali)then
c         
c         if enough views, set up for solving tilt axis and tilt
c         angles depending on range of tilt angles
c         reload tilt from nominal angles to get the increments right
c         
        tltslvmin=1.e10
        tltslvmax=-1.e10
        do iv=1,nview
          ang=tltall(mapViewToFile(iv))
          tltslvmin=min(tltslvmin,ang)
          tltslvmax=max(tltslvmax,ang)
          tilt(iv) = dtor * ang
        enddo
        tltran=tltslvmax-tltslvmin
        imintiltsolv=mapFileToView(imintilt)
        ifrotfix=0
        if(tltran.lt.randoaxis)ifrotfix=imintiltsolv
        ifmaptilt=1
        if(tltran.lt.randotilt)ifmaptilt=0
c	  print *,'ifrotfix',ifrotfix,'  ifmaptilt',ifmaptilt,
c         &	      '  imintilt&solv',imintilt,imintiltsolv
        call proc_vars(ifmaptilt,imintiltsolv, var, nvarsrch)
c         
c         find out how many points have not been done yet and decide
c         whether to reinitialize dxy and xyz
c         
        nsum = 0
        do jpt=1,nrealpt
          do i=1,3
            if (xyzsav(i,iobjali(jpt)) .eq. 0.) nsum = nsum + 1
          enddo
        enddo
        if (nsum .gt. 0.2 * nrealpt) initxyzdone = 0
c         
        if(initxyzdone.eq.0)then
c           
c           first time, initialize xyz and dxy
c           
c           try either with initial dxy solved to equalize centroids
c           section-to- section, or with dxy 0.  Find which way gives
c           lowest error somewhere along the line, and redo it that
c           way to do just the best number of iterations
c           
          call remap_params(var)
c           
c           initial trial with call to INIT_DXY
c           
c	    dxy(1,imintiltsolv)=cgx/scalexy
c	    dxy(1,imintiltsolv)=cgy/scalexy
          call init_dxy(xx,yy,isecview,irealstr,
     &        nview,nrealpt,imintiltsolv,dxy)
c           
          do itry=1,2
c             
c             second time through, save minimum error and
c             iteration # from first trial that used call to
c             init_dxy
c             
            isolmininit=isolmin
            ermininit=ermin
c             
            call solve_xyzd(xx,yy,isecview,irealstr,
     &          nview, nrealpt,tilt,rot, gmag,comp,xyz,
     &          dxy,nsolve,error,erlist,isolve)
c             
c             find iteration with minimum error
c             
            ermin=1.e30
            do i=1,isolve-1
              if(erlist(i).lt.ermin)then
                isolmin=i
                ermin=erlist(i)
              endif
            enddo
c             print *,itry,isolve,ermin,isolmin
c             
c             set dxy to 0 for second try, or leave at zero for
c             final setup
c             
            do iv=1,nview
              dxy(1,iv)=cgx/scalexy
              dxy(2,iv)=cgy/scalexy
            enddo
          enddo
c           
          if(ermininit.lt.ermin)then
            isolmin=isolmininit
            call init_dxy(xx,yy,isecview,irealstr,
     &          nview,nrealpt,imintiltsolv,dxy)
          endif
c           
          call solve_xyzd(xx,yy,isecview,irealstr,
     &        nview, nrealpt,tilt,rot, gmag,comp,xyz,
     &        dxy,isolmin,error,erlist,isolve)
          initxyzdone=1
        else
          do jpt=1,nrealpt
            kpt=iobjali(jpt)
            do i=1,3
              xyz(i,jpt)=xyzsav(i,kpt)/scalexy
            enddo
          enddo
          do iv=1,nview
            dxy(1,iv)=dxysav(1,mapViewToFile(iv))/scalexy
            dxy(2,iv)=dxysav(2,mapViewToFile(iv))/scalexy
          enddo
        endif
c         
c         pack the xyz into the var list
c         
        if(nvarsrch+3*nrealpt.gt.min(maxmetro,maxvar))call errorexit(
     &      'TOO MANY VARIABLES FOR H ARRAY IN METRO',0)
        
        nvargeom=nvarsrch
        do jpt=1,nrealpt-1
          do i=1,3
            nvarsrch=nvarsrch+1
            var(nvarsrch)=xyz(i,jpt)
          enddo
        enddo
c	  
c         save the variable list for multiple trials and 
c         ability to restart on errors.
c         1/25/06: changed to restart on all errros, including too many cycles,
c         since varying metro factor  works for this too
c         
        do i = 1, nvarsrch
          varsave(i) = var(i)
        enddo
c         
        metroLoop = 1
        ier = 1
        do while (metroLoop.le.maxMetroTrials .and. ier.ne.0)
          firsttime=.true.
          call funct(nvarsrch,var,finit,grad)
c           WRITE(6,70)FINIT
c           70	    FORMAT(/' Variable Metric minimization',T50,
c	    &	    'Initial F:',T67,E14.7)
C	    
          CALL METRO(nvarsrch,var,funct,F,Grad,facm * trialScale(metroLoop),
     &        eps,-NCYCLE,IER, H,KOUNT)
          metroLoop = metroLoop +1
          if (ier .ne. 0) then
            if (metroLoop .le. maxMetroTrials) then
              print *,'Metro error #',ier,', Restarting with step ',
     &            'factor of ', facm * trialScale(metroLoop)
              do i = 1, nvarsrch
                var(i) = varsave(i)
              enddo
            endif
          endif
        enddo
c         
C         Final call to FUNCT
        CALL FUNCT(nvarsrch,var,FFINAL,Grad)
c         
c         unscale all the points, dx, dy, and restore angles to
c         degrees
c         
        tltran = 0
        ifmaptilt = 0
        do i=1,nrealpt
          ior=iobjali(i)
          do j=1,3
            xyz(j,i)=xyz(j,i)*scalexy
            xyzsav(j,ior)=xyz(j,i)
          enddo
          rsum=0.
          do ipt=irealstr(i),irealstr(i+1)-1
            rsum=rsum+sqrt(xresid(ipt)**2+yresid(ipt)**2)
            restmp(ipt+1-irealstr(i)) = scalexy*
     &          sqrt(xresid(ipt)**2+yresid(ipt)**2)
          enddo
          resmean(ior+ibaseRes)=rsum*scalexy/
     &        (irealstr(i+1)-irealstr(i))
c	    write(*,'(i4,(10f7.3))')ior,resmean(ior+ibaseRes),(restmp(ipt),
c     &        ipt = 1, min(9,irealstr(i+1)-irealstr(i)))
          tltran = tltran + rsum * scalexy
          ifmaptilt = ifmaptilt + irealstr(i+1)-irealstr(i)
        enddo
c         
        WRITE(6,98)nview,KOUNT,ffinal,tltran/ifmaptilt
98      format(i4,' views,',i5,' cycles, F =',e14.7,', mean residual =',
     &      f8.2)
c         
C         Error reports:
        IF(IER.NE.0)THEN
          print *,'tiltalign error going to view',iview,
     &        ' even after varying step factor'
        END IF
c         
        do iv=1,nview
          ivor=mapViewToFile(iv)
          dxysav(1,ivor)=dxy(1,iv)*scalexy
          dxysav(2,ivor)=dxy(2,iv)*scalexy
          rotorig(ivor)=rot(iv)/dtor
          tiltorig(ivor)=tilt(iv)/dtor
          gmagorig(ivor)=gmag(iv)

c$$$	    ifmaptilt=0
c$$$	    rsum=0
c$$$	    do i=1,nprojpt
c$$$        if (iv.eq.isecview(i)) then
c$$$        rsum=rsum+scalexy*sqrt(xresid(i)**2 + yresid(i)**2)
c$$$        ifmaptilt = ifmaptilt + 1
c$$$        endif
c$$$	    enddo
c$$$	    write(*,'(i4,3f8.2,f8.4,2f10.2,f8.2)')ivor,rotorig(ivor),
c$$$        &		tiltorig(ivor), tiltorig(ivor) - tltall(ivor), gmag(iv),
c$$$        &		dxysav(1,ivor),dxysav(2,ivor), rsum/ifmaptilt
        enddo
c$$$	  do ior = 1, max_mod_obj
c$$$      if (itemOnList(ior, iobjali, nrealpt))
c$$$      &		write(*,'(i4,3f10.2)')ior,(xyzsav(j,ior),j=1,3)
c$$$	  enddo

        ifdidalign=1
        ifAlignDone = 1
        ibaseOnAlign = ibaseRes
      endif
      return
      end

c       
c       $Log$
c       Revision 3.10  2008/06/21 19:26:04  mast
c       Unscaled xyz so it can be used for find_surfaces without worry
c
c       Revision 3.9  2008/03/05 00:32:18  mast
c       Increased maxmetro and put h in common
c
c       Revision 3.8  2007/02/19 20:50:23  mast
c       Changes for beam tilt and grouping improvements in tiltalign
c
c       Revision 3.7  2006/06/29 04:53:31  mast
c       Set up to use small model
c
c       Revision 3.6  2006/01/26 05:50:04  mast
c       Made it restart on too many cycle error also
c
c       Revision 3.5  2005/04/10 18:06:21  mast
c       Actually changed metro factor on repeats
c	
c       Revision 3.4  2005/04/07 03:56:31  mast
c       New version with local tracking, new mapping, outliers, etc.
c	
c       Revision 3.2  2003/04/11 17:29:33  mast
c       Added declarations for implicit none, added cgx, cgy to tltcntrl
c	
c       Revision 3.1  2002/07/28 22:56:54  mast
c       Stadardize error output
c	

c       $Id$
c       
c       Routine to run the tiltalign operation
c
      subroutine tiltali(ifdidalign,ifAlignDone,resmean,iview)
      use tltcntrl
      implicit none
      include 'smallmodel.inc'
      integer maxMetroTrials
      parameter (maxMetroTrials=5)

      integer*4 ifdidalign, iview,ifAlignDone
      real*4 resmean(*)
c       
      real*4 var(5*size(tilt)),grad(5*size(tilt))
      real*4 varsave(5*size(tilt))
      external funct
      double precision error
      real*4 dtor
      data dtor/0.0174532/
      integer*4 imintiltsolv,i,nprojpt,iv,maxvar
      real*4 tltslvmin,tltslvmax,ang,tltran,finit
      integer*4 nvarsrch,ifmaptilt,jpt,kpt,nvargeom,ier,kount,nsum
      real*4 f,ffinal,rsum
      integer*4 ior,ipt,ivor,j,metroLoop
      real*4 trialScale(maxMetroTrials) /1.0, 0.9, 1.1, 0.75, 0.5/
      real*4 restmp(25000)
      logical itemOnList

      xyzfixed = .false.
      robustWeights = .false.
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
c         Find minimum tilt for this angle range
c         
        tltslvmin=1.e10
        tltslvmax=-1.e10
        rsum = 1.e10
        do iv=1,nview
          ang=tltall(mapViewToFile(iv))
          tltslvmin=min(tltslvmin,ang)
          tltslvmax=max(tltslvmax,ang)
          tilt(iv) = dtor * ang
          if (abs(ang) .lt. rsum) then
            rsum = abs(ang)
            imintiltsolv = iv
          endif
        enddo
        tltran=tltslvmax-tltslvmin
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
c         check h allocation; if it is not enough, try  to make it enough for the
c         full set of views
        maxvar = nvarsrch + 3 * nrealpt
        iv = max((maxvar + 3) * maxvar, 9 * nrealpt**2 + 36 * nrealpt)
        if (iv .gt. maxh) then
          maxvar = (nvuall + nview - 1) * nvarsrch / nview
          iv = max((maxvar + 3) * maxvar, iv)
          if (maxh .gt. 0) deallocate(h)
          maxh = iv
c          print *,'Allocated h to', maxh,' based on',maxvar, maxvar + 3
          allocate(h(maxh), stat=iv)
          call memoryError(iv, 'ARRAY FOR H MATRIX')
        endif
c
        if(initxyzdone.eq.0)then
c           
c           first time, initialize xyz and dxy 
c           
          call remap_params(var)
c           
          call solveXyzd(xx,yy,isecview,irealstr, nview, nrealpt,tilt,rot, gmag,comp,
     &        xyz, dxy, 0., h, maxh, error,ier)
          if (ier .ne. 0) print *,'WARNING: failed to initialize X/Y/Z coordinates '//
     &        'for tiltalign solution'
          initxyzdone=1
        else
          do jpt=1,nrealpt
            kpt=iobjali(jpt)
              xyz(1:3,jpt)=xyzsav(1:3,kpt)/scalexy
          enddo
          do iv=1,nview
            dxy(1:2,iv)=dxysav(1:2,mapViewToFile(iv))/scalexy
          enddo
        endif
c         
c         pack the xyz into the var list
c         
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
        varsave(1:nvarsrch) = var(1:nvarsrch)
c         
        metroLoop = 1
        ier = 1
        do while (metroLoop.le.maxMetroTrials .and. ier.ne.0)
          firstFunct=.true.
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
              var(1:nvarsrch) = varsave(1:nvarsrch)
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
          xyz(1:3,i)=xyz(1:3,i)*scalexy
          xyzsav(1:3,ior)=xyz(1:3,i)
          rsum=0.
          do ipt=irealstr(i),irealstr(i+1)-1
            rsum=rsum+sqrt(xresid(ipt)**2+yresid(ipt)**2)
            restmp(ipt+1-irealstr(i)) = scalexy*
     &          sqrt(xresid(ipt)**2+yresid(ipt)**2)
          enddo
          resmean(ior)=rsum*scalexy/ (irealstr(i+1)-irealstr(i))
c	    write(*,'(i4,(10f7.3))')ior,resmean(ior),(restmp(ipt),
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
      endif
      return
      end

*       * * * * * *  TILTALIGN  * * * * * * *
c       
c       This program will solve for the displacements, rotations, tilts, and
c       magnification differences relating a set of tilted views of an
c       object.  It uses a set of fiducial points that have been identified
c       in a series of views.  These input data are read from a model in
c       which each fiducial point is a separate object or contour.
c       
c       See Man page for all details.
c       
c       David Mastronarde  March 1989
c       
c       $Id$
c       Log and history at end of file
c
c       MEMORY ANALYSIS:
c       
c       Variables in alivar.inc:
c       5 * 4 * maxprojpt
c       33 * 4 * maxview
c       4 * 4 * maxreal
c       
c       Variables here:
c       6 * 4 * maxprojpt
c       3 * 4 * maxvar = 21 * 4 * maxview
c       22 * 4 * maxview
c       13 * 4 * maxreal
c       4 * maxmetro*(maxmetro+4)
c       
c       44 * maxprojpt  = 6.2 M
c       304 * maxview   = 0.2 M
c       68 * maxreal    = 0.07 M
c       55 M for maxmetro
c       
c       Note: IF MAXVAR IS NOT BIGGER THAN MAXMETRO, NEED TO DIMENSION
c       var to maxmetro
c       
      implicit none
      include 'alivar.inc'
      integer maxvar,maxmetro
      parameter (maxvar=7*maxview)
      parameter (maxmetro=3700)
c       
      integer*4 ninreal(maxreal),igroup(maxreal)
      integer*4 imodobj(maxreal),imodcont(maxreal)
      real*4 var(maxvar),varerr(maxvar),xyzerr(3,maxreal)
      real*4 grad(maxmetro),h(maxmetro*(maxmetro+3))
      character*8 varname(maxvar)
      double precision error
      real*4 erlist(100),tiltorig(maxview),viewres(maxview)
      integer*4 ninview(maxview),indsave(maxprojpt),jptsave(maxprojpt)
      real*4 errsave(maxprojpt) 
      real*4 viewerrsum(maxview),viewerrsq(maxview)
      real*4 viewmeanres(maxview),viewsdres(maxview)
      
      logical ordererr,nearbyerr
      character*320 modelfile,residualfile,pointFile,unadjTiltFile
c       
      real*4 fl(2,3,maxview),fa(2,3),fb(2,3),fc(2,3),fpstr(2,3)
c       
      real*4 xzfac(maxview),yzfac(maxview)
      real*4 allxyz(3,maxreal)
      real*4 allxx(maxprojpt),allyy(maxprojpt)
      real*4 glbfl(2,3,maxview),glbxzfac(maxview),glbyzfac(maxview)
      integer*4 iallsecv(maxprojpt),iallrealstr(maxreal),listreal(maxreal)
      integer*4 indallreal(maxreal),mapAllToLocal(maxview)
      integer*4 mapLocalToAll(maxview)
      integer*4 mallFileToView(maxview), mallViewToFile(maxview)
c       
      logical firsttime,xyzfixed,toofewfid,useTarget
      common /functfirst/ firsttime,xyzfixed
      integer*4 ncycle/500/,nsolve/95/
      real*4 DTOR/0.0174532/
c       
      integer*4 nlocalres,nsurface,iwhichout,metroerror,isolmin,i,itry
      integer*4 inputalf,mapalfend,ifvarout,ifresout,ifxyzout,iflocal
      integer*4 isolmininit,iv,nvarsrch,nvargeom,index,nvarang,nvarscl
      real*4 errcrit,facm,znew,xtiltnew,scalexy,ermin,ermininit,errsum
      real*4 errsqsm,residerr,vwerrsum,vwerrsq,sxoz,szox,sxox,szoz
      real*4 xo,zo,xshft,zshft,rollpts,costh,sinth,xtmp,compinc,compabs
      integer*4 nvadd,ninvsum,ivst,ivnd,iunit2,nunknowtot,iunit
      real*4 unkrat,tiltout,zmin,zmax,zmiddle,dysum,cosphi,sinphi
      real*4 dyavg,offmin,dxmid,offsum,dxtry,xtfac,xtconst,off,yshft
      integer*4 j,iuangle,iuxtilt,ndxtry,iunlocal,nallrealpt,localv,ll
      integer*4 mapalfstart,nord,jpt,npatchx,npatchy,ivt,ipt,nAllView
      integer*4 nxpmin,nypmin,minfidtot,minfidsurf,ifxyzfix,nallprojpt
      real*4 errmean,errsd,errnosd,tiltmax,fixedmax,xsum,ysum,zsum
      integer*4 idxpatch,idypatch,ipatchx,ipatchy,ixspatch,iyspatch
      integer*4 nxp,nyp,minsurf,nbot,ntop,ixmin,ixmax,iymin,iymax,kk
      integer*4 nprojpt,imintilt,ncompsrch,maptiltstart,isolve,ifBTSearch
      real*4 xcen,ycen,ffinal,dxmin,tmp,tiltnew,fixeddum,tiltadd,allBorder
      integer*4 ixtry,itmp,iord,ixpatch,iypatch,ivdel,ierr,ifZfac
      integer*4 nxTarget, nyTarget
      real*4 xpmin,ypmin,xdelt,projStrFactor, projStrAxis
      real*4 dmat(9),xtmat(9),ytmat(9),prmat(4),rmat(9),costmp,sintmp
      real*4 afac, bfac, cfac, dfac, efac, ffac, cosalf, sinalf, cosbet
      real*4 sinbet, cosdel, sindel,denom, unkrat2, angles(3), cos2rot,sin2rot
      real*4 a11, a12, a21, a22, xzOther, yzOther, errsumLocal, errLocalMin
      real*4 errLocalMax,beamInv(9), beamMat(9), cosBeam, sinBeam, allYmax
      real*4 binStepIni, binStepFinal, scanStep, allXmin, allXmax, allYmin
      real*8 pmat(9)
      integer*4 imageBinned, nunknowtot2, ifDoLocal, ninThresh
      real*4 atand,sind,cosd
      integer*4 nearest_view,lnblnk
      character*320 concat
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean, PipGetThreeIntegers
      integer*4 PipGetString,PipGetFloat, PipGetTwoIntegers, PipGetTwoFloats
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  tiltalign
c       
      integer numOptions
      parameter (numOptions = 104)
      character*(40 * numOptions) options(1)
      options(1) =
     &    ':ModelFile:FN:@:ImageFile:FN:@:ImageSizeXandY:IP:@'//
     &    ':ImageOriginXandY:FP:@:ImagePixelSizeXandY:FP:@'//
     &    ':ImagesAreBinned:I:@:OutputModelFile:FN:@:OutputResidualFile:FN:@'//
     &    ':OutputModelAndResidual:FN:@:OutputTopBotResiduals:FN:@'//
     &    ':OutputFidXYZFile:FN:@:OutputTiltFile:FN:@'//
     &    ':OutputXAxisTiltFile:FN:@:OutputTransformFile:FN:@'//
     &    ':OutputZFactorFile:FN:@:IncludeStartEndInc:IT:@:IncludeList:LI:@'//
     &    ':ExcludeList:LI:@:RotationAngle:F:@:SeparateGroup:LIM:@'//
     &    'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'//
     &    'tiltfile:TiltFile:FN:@angles:TiltAngles:FAM:@:AngleOffset:F:@'//
     &    ':ProjectionStretch:B:@:BeamTiltOption:I:@'//
     &    ':FixedOrInitialBeamTilt:F:@'//
     &    ':RotOption:I:@:RotDefaultGrouping:I:@:RotNondefaultGroup:ITM:@'//
     &    ':RotationFixedView:I:@:LocalRotOption:I:@'//
     &    ':LocalRotDefaultGrouping:I:@:LocalRotNondefaultGroup:ITM:@'//
     &    ':TiltOption:I:@:TiltFixedView:I:@:TiltSecondFixedView:I:@'//
     &    ':TiltDefaultGrouping:I:@:TiltNondefaultGroup:ITM:@'//
     &    ':LocalTiltOption:I:@:LocalTiltFixedView:I:@'//
     &    ':LocalTiltSecondFixedView:I:@:LocalTiltDefaultGrouping:I:@'//
     &    ':LocalTiltNondefaultGroup:ITM:@:MagReferenceView:I:@'//
     &    ':MagOption:I:@:MagDefaultGrouping:I:@:MagNondefaultGroup:ITM:@'//
     &    ':LocalMagReferenceView:I:@:LocalMagOption:I:@'//
     &    ':LocalMagDefaultGrouping:I:@:LocalMagNondefaultGroup:ITM:@'//
     &    ':CompReferenceView:I:@:CompOption:I:@:CompDefaultGrouping:I:@'//
     &    ':CompNondefaultGroup:ITM:@:XStretchOption:I:@'//
     &    ':XStretchDefaultGrouping:I:@:XStretchNondefaultGroup:ITM:@'//
     &    ':LocalXStretchOption:I:@:LocalXStretchDefaultGrouping:I:@'//
     &    ':LocalXStretchNondefaultGroup:ITM:@:SkewOption:I:@'//
     &    ':SkewDefaultGrouping:I:@:SkewNondefaultGroup:ITM:@'//
     &    ':LocalSkewOption:I:@:LocalSkewDefaultGrouping:I:@'//
     &    ':LocalSkewNondefaultGroup:ITM:@:XTiltOption:I:@'//
     &    ':XTiltDefaultGrouping:I:@:XTiltNondefaultGroup:ITM:@'//
     &    ':LocalXTiltOption:I:@:LocalXTiltDefaultGrouping:I:@'//
     &    ':LocalXTiltNondefaultGroup:ITM:@:ResidualReportCriterion:F:@'//
     &    ':SurfacesToAnalyze:I:@:MetroFactor:F:@:MaximumCycles:I:@'//
     &    ':AxisZShift:F:@:AxisXShift:F:@:LocalAlignments:B:@'//
     &    ':OutputLocalFile:FN:@:NumberOfLocalPatchesXandY:IP:@'//
     &    ':TargetPatchSizeXandY:IP:@:MinSizeOrOverlapXandY:FP:@'//
     &    ':MinFidsTotalAndEachSurface:IP:@:FixXYZCoordinates:B:@'//
     &    ':LocalOutputOptions:IT:@:RotMapping:IAM:@:LocalRotMapping:IAM:@'//
     &    ':TiltMapping:IAM:@:LocalTiltMapping:IAM:@:MagMapping:IAM:@'//
     &    ':LocalMagMapping:IAM:@:CompMapping:IAM:@:XStretchMapping:IAM:@'//
     &    ':LocalXStretchMapping:IAM:@:SkewMapping:IAM:@'//
     &    ':LocalSkewMapping:IAM:@:XTiltMapping:IAM:@'//
     &    ':LocalXTiltMapping:IAM:@param:ParameterFile:PF:@help:usage:B:'
c       
      nlocalres=50
      firsttime=.true.
      xyzfixed=.false.
      toofewfid=.false.
      incrgmag=0
      incrdmag=0
      incrskew=0
      incrrot=0
      incrtilt=0
      incralf=0
      dxmin = 0.
      dyavg = 0.
      ifZfac = 0
      imageBinned = 1
      ifDoLocal = 0
      binStepIni = 1.
      binStepFinal = 0.25
      scanStep = 0.02
      ninThresh = 3
c       
c       set this to 1 to get inputs for X-axis tilting from sequential input
c       
      inputalf=0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'tiltalign',
     &    'ERROR: TILTALIGN - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
      iuxtilt=inputalf
      call input_model(xx,yy,isecview,maxprojpt,maxreal,irealstr,
     &    ninreal,imodobj,imodcont,nview,nprojpt, nrealpt,iwhichout,
     &    xcen,ycen, xdelt, mapviewtofile,mapfiletoview,nfileviews,
     &    modelfile, residualFile, pointFile, iuangle,iuxtilt,pipinput)
c       
      if(nview.gt.maxview)call errorexit('TOO MANY VIEWS FOR ARRAYS',
     &    0)

      do i = 1, nrealpt
        listreal(i) = i
      enddo
      call countNumInView(listreal, nrealpt, irealstr, isecview, nview,
     &    ninview)
      call input_vars(var,varname,inputalf,nvarsrch,nvarang,nvarscl,
     &    imintilt, ncompsrch,0,maptiltstart,mapalfstart,mapalfend,ifBTSearch,
     &    tiltorig, tiltadd,pipinput,.false.,ninview,ninThresh)
c       
c       TODO: is this OK to delete?
c      do i=1,nview
c        viewres(i)=0.
c        ninview(i)=0
c      enddo
c       
      facm=0.5
      if (pipinput) then
        errcrit = 3.0
        nsurface = 0
        ncycle = 1000
        znew = 0.
        xtiltnew = 0.
        ierr = PipGetFloat('ResidualReportCriterion', errcrit)
        ierr = PipGetInteger('SurfacesToAnalyze', nsurface)
        ierr = PipGetFloat('MetroFactor', facm)
        ierr = PipGetInteger('MaximumCycles', ncycle)
        ierr = PipGetFloat('AxisZShift', znew)
        ierr = PipGetFloat('AxisXShift', xtiltnew)
        ierr = PipGetInteger('ImagesAreBinned', imageBinned)
        imageBinned = max(1, imageBinned)
        ierr = PipGetBoolean('LocalAlignments', ifDoLocal)

      else
        write(*,'(1x,a,/,a,$)') 'Criterion # of sd above mean residual'
     &      //' error to report (+ for ',
     &      'relative to absolute mean,  - for relative to mean '//
     &      'of nearby views): '
        read(5,*)errcrit
c         
        write(*,'(1x,a,$)')'1 or 2 to derive a tilt angle assuming'//
     &      ' points are on 1 or 2 surfaces: '
        read(5,*)nsurface
c         
        write(*,'(1x,a,f5.2,i5,a,$)')'Factor for METRO, limit on # '//
     &      'of cycles [',facm,ncycle,']: '
        read(5,*)facm,ncycle
c         
        if(iwhichout.ge.0)then
c           
c           find out what to do with z value of axis
c           
          print *,'Z shift in tilt axis relative to centroid,'
          write(*,'(1x,a,$)')
     &        '         or 1000 to shift to middle of z range: '
          read(5,*)znew
c           
c           get new position of tilt axis in x
c           
          write(*,'(1x,a,$)')
     &        'New X position of tilt axis relative to center: '
          read(5,*)xtiltnew
        endif
      endif
c       
      if (nint(znew) .ne. 1000) znew = znew / imageBinned
      xtiltnew = xtiltnew / imageBinned
      ordererr=.true.
      nearbyerr=errcrit.lt.0.
      errcrit=abs(errcrit)
C       
c       scale the points down to range of 1.0: helps convergence
c       
      ifvarout=1
      ifresout=1
      ifxyzout=1
      iflocal=0
      metroerror=0
      do i=1,nrealpt
        indallreal(i)=i
      enddo
c       
      scalexy=0.
      do i=1,nprojpt
        scalexy=max(scalexy,abs(xx(i)),abs(yy(i)))
      enddo
      do i=1,nprojpt
        xx(i)=xx(i)/scalexy
        yy(i)=yy(i)/scalexy
      enddo
c       
c       call ye olde init_dxy to get initial dx and dy, and solve_xyzd to
c       get initial values of x,y,z
c       NOTE that these routines are probably more complicated than necessary
c       to get the minimization going; they were written for an earlier
c       attempt to solve for alignment variables and were simply adopted here
c       without assessing their necessity.  They do start the process out
c       with x,y,z values that are nearly correct for the initial angles.
c       
c       try either with initial dxy solved to equalize centroids section-to-
c       section, or with dxy 0.  Find which way gives lowest error somewhere
C       along the line, and redo it that way to do just the best number of
c       iterations
c       
      call remap_params(var)
c       
c       initial trial with call to INIT_DXY
c       
      call init_dxy(xx,yy,isecview,irealstr,
     &    nview,nrealpt,imintilt,dxy)
c       
      do itry=1,2
c         
c         second time through, save minimum error and iteration # from
c         first trial that used call to init_dxy
c         
        isolmininit=isolmin
        ermininit=ermin
c         
        call solve_xyzd(xx,yy,isecview,irealstr,nview, nrealpt,tilt,rot,
     &      gmag,comp,xyz,dxy,nsolve,error,erlist,isolve)
c         
c         find iteration with minimum error
c         
        ermin=1.e30
        do i=1,isolve-1
          if(erlist(i).lt.ermin)then
            isolmin=i
            ermin=erlist(i)
          endif
        enddo
c         print *,itry,isolve,ermin,isolmin
c         
c         set dxy to 0 for second try, or leave at zero for final setup
c         
        do iv=1,nview
          dxy(1,iv)=0.
          dxy(2,iv)=0.
        enddo
      enddo
c       
      if(ermininit.lt.ermin)then
        isolmin=isolmininit
        call init_dxy(xx,yy,isecview,irealstr,
     &      nview,nrealpt,imintilt,dxy)
        print *,
     &      'DXY set to equalize centroids gave best initialization'
      else
        print *, 'DXY set to zero gave best initialization'
      endif
c       
      call solve_xyzd(xx,yy,isecview,irealstr,nview, nrealpt,tilt,rot,
     &    gmag,comp,xyz,dxy,isolmin,error,erlist,isolve)
c       
c       
c       pack the xyz into the var list
c       
180   nvargeom=nvarsrch
      if(nvargeom+3*nrealpt.gt.min(maxvar,maxmetro))call errorexit(
     &    'TOO MANY VARIABLES FOR VAR, GRAD, AND METRO H ARRAYS', 0)
      do jpt=1,nrealpt-1
        do i=1,3
          nvarsrch=nvarsrch+1
          var(nvarsrch)=xyz(i,jpt)
        enddo
      enddo
c
      if (ifBTSearch .eq. 0) then
        call runMetro(nvarsrch,var,varerr,grad,h,ifLocal,facm,ncycle, 0,
     &      fFinal, i, metroError)
      else
        call searchBeamTilt(beamTilt, binStepIni, binStepFinal, scanStep, 
     &      nvarsrch,var,varerr,grad,h,ifLocal,facm,ncycle,
     &      fFinal, metroError)
      endif
c       
c       unscale all the points, dx, dy, and restore angles to degrees
c       
      index=0
      do i=1,nvarang
        var(i)=var(i)/dtor
        index=index+1
        varerr(i)=(sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch) /dtor
      enddo
c       
      do i=nvarang+1,nvarscl
        index=index+1
        varerr(i)=sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch
      enddo
c       
      do i=mapalfstart, mapalfend
        var(i)=var(i)/dtor
        index=index+1
        varerr(i)=(sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch) /dtor
      enddo
c       leave projection skew and beam tilt as radians
      do i=mapalfend+1,nvargeom
        index=index+1
        varerr(i)=sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch
      enddo
c       
      do i=1,nrealpt
        do j=1,3
          xyz(j,i)=xyz(j,i)*scalexy
          index=index+1
          if(i.lt.nrealpt)xyzerr(j,i)=
     &        scalexy*sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch
        enddo
      enddo
c       
      errsum=0.
      errsqsm=0.
      do i=1,nview
        viewres(i)=0.
        ninview(i)=0
        viewerrsum(i)=0.
        viewerrsq(i)=0.
      enddo
      do i=1,nprojpt
        xx(i)=xx(i)*scalexy
        yy(i)=yy(i)*scalexy
        xresid(i)=xresid(i)*scalexy
        yresid(i)=yresid(i)*scalexy
        residerr=sqrt(xresid(i)**2 + yresid(i)**2)
        iv=isecview(i)
        ninview(iv)=ninview(iv)+1
        viewerrsum(iv)=viewerrsum(iv)+residerr
        viewerrsq(iv)=viewerrsq(iv)+residerr**2
      enddo
c       
      do iv=1,nview
        dxy(1,iv)=dxy(1,iv)*scalexy
        dxy(2,iv)=dxy(2,iv)*scalexy
c         
c         save global solution now
c
        if (iflocal .eq. 0) then
          glbalf(iv) = alf(iv)
          glbtilt(iv) = tilt(iv)
          glbrot(iv) = rot(iv)
          glbskew(iv)=skew(iv)
          glbgmag(iv)=gmag(iv)
          glbdmag(iv)=dmag(iv)
        endif
        rot(iv)=rot(iv)/dtor
        tilt(iv)=tilt(iv)/dtor
        skew(iv)=skew(iv)/dtor
        alf(iv)=alf(iv)/dtor
        viewres(iv)=viewerrsum(iv)/ninview(iv)
        errsum=errsum+viewerrsum(iv)
        errsqsm=errsqsm+viewerrsq(iv)
c         
c         find mean and sd residual of minimum number of points in a local
c         group of views
c         
        nvadd=1
        ninvsum=0
        do while(ninvsum.lt.nlocalres.and.nvadd.lt.nview)
          ivst=max(1, iv-nvadd/2)
          ivnd=min(nview, ivst+nvadd-1)
          ninvsum=0
          do ivt=ivst,ivnd
            ninvsum=ninvsum+ninview(ivt)
          enddo
          nvadd=nvadd+1
        enddo
        vwerrsum=0.
        vwerrsq=0.
        do ivt=ivst,ivnd
          vwerrsum=vwerrsum+viewerrsum(ivt)
          vwerrsq=vwerrsq+viewerrsq(ivt)
        enddo
        viewmeanres(iv)=vwerrsum/ninvsum
        viewsdres(iv)=sqrt((vwerrsq-vwerrsum**2/ninvsum)/(ninvsum-1))
      enddo
c       
c       convert the projection stretch to a matrix
c       (This only works directly into fpstr if it is symmetric)
c       
      call fill_proj_matrix(projStrRot, projSkew, fpstr, costmp, sintmp,
     &    cos2rot, sin2rot)
      call amat_to_rotmagstr(fpstr, xo, zo, projStrFactor, projStrAxis)
c       
c       if doing local solution, need to find rotation to match
c       the original set of points
c       
      if(iflocal.ne.0)then
        sxoz=0.
        szox=0.
        sxox=0.
        szoz=0.
        do i=1,nrealpt
          xo=allxyz(1,indallreal(i))-xcen-xshft
          zo=allxyz(3,indallreal(i))-zshft
          sxox=sxox+xo*xyz(1,i)
          sxoz=sxoz+xo*xyz(3,i)
          szox=szox+zo*xyz(1,i)
          szoz=szoz+zo*xyz(3,i)
        enddo
        rollpts=0.
        if((sxox+szoz).gt.1.e-5*abs(sxoz-szox))
     &      rollpts=atand((sxoz-szox)/(sxox+szoz))
c         
c         rolls the points, reduce this amount from the tilts
c         
        costh=cosd(rollpts)
        sinth=sind(rollpts)
        do i=1,nrealpt
          xtmp=xyz(1,i)*costh+xyz(3,i)*sinth
          xyz(3,i)=-xyz(1,i)*sinth+xyz(3,i)*costh
          xyz(1,i)=xtmp
        enddo
        do i=1,nview
          tilt(i)=tilt(i)-rollpts
        enddo
      endif
c       
      iunit2=7
      if(iwhichout.gt.0)iunit2=6
      compinc=1.
      compabs=1.
      nunknowtot=nvargeom+3*(nrealpt-1)
      if(xyzfixed)nunknowtot=nvargeom
      nunknowtot2 = nunknowtot + 2 * (nview - 1)
      unkrat=(2.*nprojpt)/max(nunknowtot, 1)
      unkrat2=(2.*nprojpt)/max(nunknowtot2, 1)
      do iunit=6,iunit2
        write (iunit,113)nview,nvargeom,nrealpt,nprojpt,
     &      2*nprojpt,nunknowtot2,unkrat2
113     format(i4,' views,',i5,' geometric variables,',i5,
     &      ' 3-D points,',i6,' projection points',/,
     &      '  Ratio of total measured values to all unknowns =',
     &      i6,'/',i4,' =',f7.2)
        if(ifvarout.ne.0)then
          if(iunit.ne.6)write(iunit,'(/,21x,a)')
     &        'Geometric variable values and errors'
          if(iunit.ne.6)write(iunit,'(3(f10.4,f7.4,a9,1x))',err=85)
     &        (var(i),varerr(i),varname(i),i=1,nvargeom)
85        if(ncompsrch.eq.0)then
            if (mapProjStretch .gt. 0) write(iunit,'(/,a,f8.2,a)')
     &          'Projection skew is',projSkew / dtor,' degrees'
            if (mapBeamTilt .gt. 0 .or. ifBTSearch .ne. 0)
     &          write(iunit,'(/,a,f8.2,a)') 'Beam tilt angle is',
     &          beamTilt / dtor, ' degrees'
            if(mapalfstart.gt.mapalfend)then
              write(iunit,'(/,a,f7.2)')
     &            ' At minimum tilt, rotation angle is',rot(imintilt)
              write(iunit,'(/,a)') ' view   rotation    tilt    '//
     &            'deltilt     mag      dmag      skew    mean resid'
              do i=1,nview
                j=mapviewtofile(i)
                write(iunit,'(i4,2f10.1,f10.2,2f10.4,2f10.2)')
     &              j,rot(i), tilt(i), tilt(i)-tiltorig(j),
     &              gmag(i),dmag(i), skew(i), viewres(i)
              enddo
            elseif(ifrotfix.eq.-1.or.ifrotfix.eq.-2)then
              if(ifrotfix.eq.-1)write(iunit,'(/,a,f7.2)')
     &            ' Fixed rotation angle is',rot(1)
              if(ifrotfix.eq.-2)write(iunit,'(/,a,f7.2)')
     &            ' Overall rotation angle is',rot(1)
              write(iunit,'(/,a)') ' view     tilt    deltilt   '//
     &            '  mag      dmag      skew     X-tilt   mean resid'
              do i=1,nview
                j=mapviewtofile(i)
                write(iunit,'(i4,f10.1,f10.2,2f10.4,3f10.2)')
     &              j, tilt(i), tilt(i)-tiltorig(j),gmag(i),dmag(i),
     &              skew(i), alf(i),viewres(i)
              enddo           
            else
              if (mapalfend .gt. mapalfstart)
     &            write(iunit,'(/,a)')'WARNING: SOLUTIONS FOR BOTH '//
     &            'ROTATION AND X-AXIS TILT ARE VERY UNRELIABLE'
              write(iunit,'(/,a)') ' view rotation  tilt  deltilt'
     &            //'    mag     dmag    skew   X-tilt  mean resid'
              do i=1,nview
                j=mapviewtofile(i)
                write(iunit,'(i4,2f8.1,f8.2,2f9.4,3f8.2)') j,rot(i),
     &              tilt(i), tilt(i)-tiltorig(j),gmag(i),dmag(i),skew(i),
     &              alf(i),viewres(i)
              enddo
            endif
          else
            write(iunit,'(/,a)') ' view   rotation    tilt      mag'
     &          //'    comp-inc  comp-abs    dmag      skew'
            do i=1,nview
c               
c               for 0 tilts, output same compression values as last view
c               
              if(tilt(i).ne.0.)then
                compinc=comp(i)       
                compabs=compinc*gmag(i)
              endif
              write(iunit,'(i4,2f10.1,4f10.4,f10.2)')mapviewtofile(i)
     &            ,rot(i),tilt(i),
     &            gmag(i),compinc,compabs,dmag(i),skew(i)
            enddo
          endif
          write(iunit,*)
          if((iuangle.eq.0.or.iunit.ne.6).and.iflocal.eq.0)
     &        write(iunit,116)(tilt(i),i=1,nview)
116       format(' ANGLES',10f7.2)
          if(ncompsrch.gt.0)write(iunit,117)(comp(i),i=1,nview)
117       format(' COMPRESS',10f7.4)
        endif
      enddo
      if(ifxyzout.ne.0)then
        write(iunit2,111)
111     format(/,21x,'3-D point coordinates (with centroid zero)'
     &      ,/,'   #',7x,'X',9x,'Y',9x,'Z',6x,'obj  cont')
        write(iunit2,'(i4,3f10.2,i7,i5)',err=86)
     &      (indallreal(j),(xyz(i,j),i=1,3),imodobj(indallreal(j)),
     &      imodcont(indallreal(j)),j=1,nrealpt)
      endif
c       
c       get min, max and midpoint of z values
c       
86    zmin=1.e10
      zmax=-1.e10
      do ipt=1,nrealpt
        zmin=min(zmin,xyz(3,ipt))
        zmax=max(zmax,xyz(3,ipt))
      enddo
      zmiddle=(zmax+zmin)/2.
      if(iflocal.eq.0)write(*,'(/,a,f8.2)')
     &    ' Midpoint of Z range relative to centroid in Z:',zmiddle

      ifanyalf = 0
      unadjTiltFile = ' '
      if (mapalfend .gt. mapalfstart)ifanyalf = 1
c       
c       Output unmodified tilt angles
      if (pipinput .and. iflocal.eq.0) then
        if (PipGetString('OutputUnadjustedTiltFile', unadjTiltFile) .eq. 0) then
          call dopen(13, unadjTiltFile, 'new', 'f')
          do i=1,nfileviews
            tiltout=tiltorig(i)
            if(mapfiletoview(i).ne.0)tiltout=tilt(mapfiletoview(i))
            write(13,'(f7.2)')tiltout
          enddo
          close(13)
        endif
      endif
c       
c       Modify angles to account for beam tilt
c
      if (beamTilt .ne. 0.) then
        do iv=1,nview
          call convert_for_beamtilt(alf(iv), tilt(iv), rot(iv), beamTilt,
     &        ifanyalf)
        enddo
      endif            
c       
c       output lists of angles that are complete for all file views
c       
      if (iflocal.eq.0.and.iuangle.ne.0) then
        do i=1,nfileviews
          tiltout=tiltorig(i)
          if(mapfiletoview(i).ne.0)tiltout=tilt(mapfiletoview(i))
          write(iuangle,'(f7.2)')tiltout
        enddo
        close(iuangle)
        if (iuxtilt .eq. 0 .and. (ifanyalf .ne. 0 .or. beamTilt .ne. 0.) .and.
     &      unadjTiltFile .eq. ' ') write(*,122)
122     format(/,'WARNING: THE SOLUTION INCLUDES X-AXIS TILTS THAT CHANGE',
     &      ' THROUGH THE SERIES',/,'WARNING: X-AXIS TILTS SHOULD BE OUTPUT',
     &      ' TO A FILE AND FED TO THE TILT PROGRAM')
      endif
c       
      if (iflocal.eq.0.and.iuxtilt.ne.0) then
        do i=1,nfileviews
          tiltout=0.
          if(mapfiletoview(i).ne.0)tiltout=alf(mapfiletoview(i))
          write(iuxtilt,'(f7.2)')tiltout
        enddo
        close(iuxtilt)
      endif
c       
c       compute xforms, shift the dy's to minimize total shift, allow
c       user to shift dx's (and tilt axis) similarly or specify new location
c       of tilt axis
c       shift axis in z by making proper shifts in x
c       
      if(iwhichout.ge.0)then
        if(znew.eq.1000.)znew=zmiddle
        if(iflocal.ne.0)znew=-zshft
        dysum=0.
 
        do iv=1,nview
c           
c           To compute transform, first get the coefficients of the full
c           projection.  Assume 0 beam tilt, it is already corrected
c           
          call fill_dist_matrix(gmag(iv), dmag(iv), skew(iv)*dtor, comp(iv),
     &        1, dmat, cosdel, sindel)
          call fill_beam_matrices(0., beamInv, beamMat, cosBeam,
     &        sinBeam)
          call fill_xtilt_matrix(alf(iv)*dtor, ifanyalf, xtmat, cosalf,
     &        sinalf)
          call fill_ytilt_matrix(tilt(iv)*dtor, ytmat, cosbet, sinbet)
          call fill_proj_matrix(projStrRot, projSkew, prmat, costmp, sintmp,
     &        cos2rot, sin2rot)
          call fill_rot_matrix(rot(iv)*dtor, rmat, costmp, sintmp)
          call matrix_to_coef(dmat, xtmat, beamInv, ytmat, beamMat, prmat,
     &        rmat, afac, bfac, cfac, dfac, efac, ffac)
c           
c           Solve for transformation that maps 1,0,0 to cos beta, 0
c           and 0,1,0 to sin alf * sin beta, cos alf
c           
          denom = bfac * dfac - afac * efac
          fl(1,1,iv) = (dfac * sinalf * sinbet - efac * cosbet) / denom
          fl(1,2,iv) = (bfac * cosbet - afac * sinalf * sinbet) / denom
          fl(2,1,iv) = dfac * cosalf / denom
          fl(2,2,iv) = -afac * cosalf / denom
          fl(1,3,iv) = -(fl(1,1,iv) * dxy(1,iv) + fl(1,2,iv) * dxy(2,iv))
          fl(2,3,iv) = -(fl(2,1,iv) * dxy(1,iv) + fl(2,2,iv) * dxy(2,iv))
c           
c           Compute Z-dependent factors to add to X and Y in backprojection
c           This method does not depend on distortion model:
c           Compute coefficients of distortion plus tilts, solve for
c           transformation that aligns images to that, determine Z component
c           of projection equation to aligned images, and subtract component
c           expected to be applied in backprojection
c           
          do i = 1, 9
            pmat(i) = dmat(i)
          enddo
          call mat_product(pmat, 3, 3, xtmat, 3, 3)
          call mat_product(pmat, 3, 3, ytmat, 2, 3)
          denom = pmat(2) * pmat(4) - pmat(1) * pmat(5)
          a11 = (pmat(4) * sinalf * sinbet - pmat(5) * cosbet) / denom
          a12 = (pmat(2) * cosbet - pmat(1) * sinalf * sinbet) / denom
          a21 = pmat(4) * cosalf / denom
          a22 = -pmat(1) * cosalf / denom
          xzfac(iv) = (a11 * pmat(3) + a12 * pmat(6)) / comp(iv) -
     &        cosalf * sinbet
          yzfac(iv) = (a21 * pmat(3) + a22 * pmat(6)) / comp(iv) + sinalf
c           
c           Alternate based on solving equations from type 1 distortion model
c           
          yzOther = -sindel * sinbet / (cosdel * cosbet)
          xzOther = (gmag(iv) / ((gmag(iv) + dmag(iv)) * cosdel) - 1. +
     &        sinalf * yzOther) * sinbet / cosalf
c           write(*,'(6f11.6)')xzfac(iv), xzOther, xzfac(iv) - xzOther,
c           &         yzfac(iv), yzOther, yzfac(iv) - yzOther
c           
c           The old way, and validation by inverse multiplication
c$$$        c         
c$$$        c         set the distortion matrix into fa and the rotation matrix into fb
c$$$        c
c$$$        fa(1,1)=(gmag(iv)+dmag(iv))*cosd(skew(iv))
c$$$        fa(2,1)=(gmag(iv)+dmag(iv))*sind(skew(iv))/cosd(tilt(iv))
c$$$        fa(2,2)=gmag(iv)
c$$$        fa(1,2)=0.
c$$$        fa(1,3)=0.
c$$$        fa(2,3)=0.
c$$$        cosphi=cosd(rot(iv))
c$$$        sinphi=sind(rot(iv))
c$$$        fb(1,1)=cosphi
c$$$        fb(1,2)=-sinphi
c$$$        fb(2,1)=sinphi
c$$$        fb(2,2)=cosphi
c$$$        fb(1,3)=0.
c$$$        fb(2,3)=0.
c$$$        fpstr(1,3)=0.
c$$$        fpstr(2,3)=0.
c$$$        c         
c$$$        c         get product, then add the dx's and dy's, then invert
c$$$        c         
c$$$        call xfmult(fa,fpstr,fc)
c$$$        call xfcopy(fc,fa)
c$$$        call xfmult(fa,fb,fc)
c$$$        fc(1,3)=dxy(1,iv)
c$$$        fc(2,3)=dxy(2,iv)
c$$$        call xfinvert(fc,fb)
c$$$        
c$$$        call xfmult(fc,fl(1,1,iv), fa)
c$$$        c       call xfinvert(fc,fl(1,1,iv))
c$$$        call xfwrite(6, fl(1,1,iv), *299)
c$$$        call xfwrite(6, fb, *299)
c$$$        call xfwrite(6, fa, *299)
c           
c           adjust dx by the factor needed to shift axis in Z
c           
299       fl(1,3,iv)=fl(1,3,iv) -znew*sind(tilt(iv))
          h(iv)=1.-cosd(tilt(iv))
          dysum=dysum+fl(2,3,iv)
        enddo
        dyavg=dysum/nview
        if(iflocal.eq.0)then
c           
c           find value of X shift that minimizes overall loss of image - do
c           exhaustive scan centered on dx of the minimum tilt image
c           
          offmin=1.e10
          dxmid=fl(1,3,imintilt)
c           
c           DNM 11/10/01: eliminate real variable do loop in deference to f95
c           do dxtry=dxmid-0.1*xcen,dxmid+0.1*xcen,0.1
c           
          ndxtry=2.*xcen
          do ixtry=0,ndxtry
            dxtry=dxmid+0.1*(ixtry-xcen)
            offsum=0.
            xtfac=xtiltnew+dxtry
            xtconst=xtiltnew-xtfac
            do iv=1,nview
              off=abs(fl(1,3,iv)+xtconst+xtfac*h(iv))-xcen*h(iv)
              if(off.gt.0.)offsum=offsum+off
            enddo
            if(offsum.lt.offmin)then
              offmin=offsum
              dxmin=dxtry
            endif
          enddo
          xtfac=xtiltnew+dxmin
          xtconst=xtiltnew-xtfac
c           
c           Put tilt axis at the new position, and get the final dy to
c           add up to 0.
c           
          do iv=1,nview
            fl(2,3,iv)=fl(2,3,iv)-dyavg
            fl(1,3,iv)=fl(1,3,iv)+xtconst+xtfac*h(iv)
          enddo
c           
c           output a transform for each file view, find the nearest one
c           for non-included view
c           
          do iv=1,nfileviews
            i=nearest_view(iv)
            call xfwrite(7,fl(1,1,i),*99)
99        enddo
          if (residualfile .ne. ' ') then
            call dopen(13,residualfile, 'new', 'f')
            write(13,'(i6,a)')nprojpt,' residuals'
            do i=1,nprojpt
              write(13, '(2f10.2,i5,3f8.2)')xx(i)+xcen,yy(i)+ycen,
     &            mapviewtofile(isecview(i))-1,
     &            xresid(i),yresid(i)
            enddo
            close(13)
          endif
c           
c           output the z factors if option requested
c           
          if (pipinput) then
            if (PipGetString('OutputZFactorFile', residualFile) .eq. 0) then
              ifZfac = 1
              call dopen(13,residualfile, 'new', 'f')
              do iv=1,nfileviews
                i=nearest_view(iv)
                glbxzfac(iv) = xzfac(i)
                glbyzfac(iv) = yzfac(i)
                write(13, '(2f12.6)')xzfac(i),yzfac(i)
              enddo
              close(13)
            endif
          endif
        else
c           
c           If local, the procedure now is first to process each relevant item
c           for the local views, in place or into a different array (grad)
c           This is mixing local views and global values, so mapping from local
c           to global view number is needed to access the global values
c           Then expand this array to the global views.
c           Then create output for all file views, taking the global or null
c           value as appropriate for an excluded view
c
c           Do this for tilt angles, alpha if they exist, z factors, and xforms
c           
          do iv = 1, nview
            grad(iv) = tilt(iv)-glbtilt(mapLocalToAll(iv))/dtor
          enddo
          call expandLocalToAll(grad, 1, 1, nview, nAllView, mapAllToLocal)
          do i=1,nfileviews
            iv=mallFileToView(i)
            h(i)=0.
            if(iv.gt.0)h(i)=grad(i)
          enddo
          write(iunlocal,'(10f7.2)')(h(i),i=1,nfileviews)
c           write(6,'(f8.2)')(tilt(iv),iv=1,nview)
          if(mapalfstart.le.mapalfend .or. beamTilt .ne. 0)then
            do iv = 1, nview
              grad(iv) = alf(iv)-glbalf(mapLocalToAll(iv))/dtor
            enddo
            call expandLocalToAll(grad, 1, 1, nview, nAllView, mapAllToLocal)
            do i=1,nfileviews
              iv=mallFileToView(i)
              h(i)=0.
              if(iv.gt.0)h(i)=grad(iv)
            enddo
            write(iunlocal,'(10f7.2)')(h(i),i=1,nfileviews)
          endif
c           
c           Z factors if they were output globally
c           
          if (ifZfac .ne. 0) then
            call expandLocalToAll(xzfac, 1, 1, nview, nAllView, mapAllToLocal)
            call expandLocalToAll(yzfac, 1, 1, nview, nAllView, mapAllToLocal)
            do i=1,nfileviews
              iv=mallFileToView(i)
              h(i)=glbxzfac(i)
              grad(i) = glbyzfac(i)
              if(iv.gt.0)then
                h(i) = xzfac(iv)
                grad(i) = yzfac(iv)
              endif
            enddo
            write(iunlocal, '(6f12.6)')(h(i), grad(i), i=1,nfileviews)
          endif
c           
c           add the shifts to the dx and dy to get transforms that
c           work to get back to the original point positions.
c           Compose the inverse of an adjusting transform
c           
          do iv = 1, nview
            fl(1,3,iv)=fl(1,3,iv)+xshft*cosd(tilt(iv))
            fl(2,3,iv)=fl(2,3,iv)+yshft
c             call xfwrite(6,fl(1,1,iv),*199)
            call xfinvert(glbfl(1,1,mapLocalToAll(iv)),fa)
            call xfmult(fa,fl(1,1,iv),fb)
            call xfinvert(fb,fl(1,1,iv))
          enddo
          do i = 1, 6
            call expandLocalToAll(fl, 6, i, nview, nAllView, mapAllToLocal)
          enddo
          do i=1,nfileviews
            iv=mallFileToView(i)
            if(iv.gt.0)then
              call xfcopy(fl(1,1,iv),fc)
            else
              call xfunit(fc,1.)
            endif
            call xfwrite(iunlocal,fc,*199)
199       enddo
c           If we need to restore the maps and isecview
c$$$          if (nview .lt. nAllView) then
c$$$c         
c$$$c             Restore all the maps now too, and correct isecview
c$$$            do jpt=1,nrealpt
c$$$              do i=irealstr(jpt),irealstr(jpt+1)-1
c$$$                isecview(i) = mapLocalToAll(isecview(i))
c$$$              enddo
c$$$            enddo
c$$$            do iv = 1, nfileviews
c$$$              mapFileToView(iv) = mallFileToView(iv)
c$$$            enddo
c$$$            print *,'nview, nallview',nview, nallview
c$$$            nview = nAllView
c$$$            do iv = 1, nview
c$$$              mapViewToFile(iv) = mallViewToFile(iv)
c$$$            enddo
c$$$          endif
        endif
      endif
c       
c       print out points with high residuals
c       
      errmean=errsum/nprojpt
      errsd=sqrt((errsqsm-errsum**2/nprojpt)/(nprojpt-1))
      if (ifDoLocal .eq. 0) then
        write(*,118)errmean,errsd
118     format(/,' Residual error mean and sd:',2f8.3,4x,a,2i4,a)
      else if (iflocal .eq. 0) then
        write(*,118)errmean,errsd,'(Global)'
      else
        write(*,118)errmean,errsd, '(Local area', ipatchx, ipatchy, ')'
        errsumLocal = errsumLocal + errmean
        errLocalMin = min(errLocalMin, errmean)
        errLocalMax = max(errLocalMax, errmean)
      endif
      if(ifresout.gt.0)then
        write(*,112)
c         
c         DEPENDENCY WARNING: Beadfixer relies on the # # ... line up to the
c         second X
c         
112     format(/,9x,'Projection points with large residuals',/,
     &      ' obj  cont  view   index coordinates      residuals',
     &      '        # of',/,
     &      '   #     #     #      X         Y        X        Y',
     &      '        S.D.')
        nord=0
        do jpt=1,nrealpt
          do i=irealstr(jpt),irealstr(jpt+1)-1
            if(nearbyerr)then
              iv=isecview(i)
              errnosd=(sqrt(xresid(i)**2+yresid(i)**2)-
     &            viewmeanres(iv))/viewsdres(iv)
            else
              errnosd=(sqrt(xresid(i)**2+yresid(i)**2)-errmean)/errsd
            endif
            if(errnosd.gt.errcrit)then
              if(ordererr)then
                nord=nord+1
                errsave(nord)=errnosd
                indsave(nord)=i
                jptsave(nord)=jpt
              else
                write(*,114) imodobj(indallreal(jpt)),
     &              imodcont(indallreal(jpt)), mapviewtofile(isecview(i))
     &              ,xx(i)+xcen
     &              ,yy(i)+ycen, xresid(i), yresid(i),errnosd
114             format(i4,2i6,2f10.2,3f9.2)
              endif
            endif
          enddo
        enddo
        if(ordererr)then
          do i=1,nord-1
            do j=i+1,nord
              if(errsave(i).lt.errsave(j))then
                tmp=errsave(i)
                errsave(i)=errsave(j)
                errsave(j)=tmp
                itmp=indsave(i)
                indsave(i)=indsave(j)
                indsave(j)=itmp
                itmp=jptsave(i)
                jptsave(i)=jptsave(j)
                jptsave(j)=itmp
              endif
            enddo
          enddo
          do iord=1,nord
            i=indsave(iord)
            write(*,114) imodobj(indallreal(jptsave(iord))),
     &          imodcont(indallreal(jptsave(iord))),
     &          mapviewtofile(isecview(i)),
     &          xx(i)+xcen, yy(i)+ycen,xresid(i), yresid(i),
     &          errsave(iord)
          enddo
        endif
      endif
      if(iflocal.ne.0)go to 200
c       
c       shift the fiducials to real positions in X and Y for xyz output file
c       and for possible use with local alignments
c       Continue to use zero-centroid xyz for find_surfaces but output 
c       a 3D model with real positions also
c       
      nallrealpt=nrealpt
      do i=1,nrealpt
        iallrealstr(i)=irealstr(i)
        allxyz(1,i)=xyz(1,i)-dxmin+xcen
        allxyz(2,i)=xyz(2,i)-dyavg+ycen
        allxyz(3,i)=xyz(3,i)-znew
      enddo
c       
      if(pointfile.ne.' ')then
        call dopen(13,pointfile,'new','f')
        write(13,'(i4,3f10.2,i7,i5,a,f12.5,a,2i6)')1,(allxyz(i,1),i=1,3),
     &      imodobj(1),imodcont(1),' Pix:',xdelt,' Dim:',nint(2.*xcen),
     &      nint(2.*ycen)
        write(13,'(i4,3f10.2,i7,i5)')(j,(allxyz(i,j),i=1,3),
     &      imodobj(j),imodcont(j),j=2,nrealpt)
        close(13)
      endif
c       
c       analyze for surfaces if desired.  Find the biggest tilt and the
c       biggest fixed tilt, get recommended new value for the biggest fixed 
c       tilt if it is not too small
c       
      tiltmax=0.
      fixedmax=0.
      do iv=1,nview
        if(abs(tilt(iv)).gt.abs(tiltmax))tiltmax=tilt(iv)
        if(maptilt(iv).eq.0.and.abs(tilt(iv)).gt.abs(fixedmax))
     &      fixedmax=tilt(iv)
      enddo
      if(fixedmax.ge.5.)tiltmax=fixedmax
      if(nsurface.gt.0)call find_surfaces(xyz,nrealpt,nsurface,
     &    tiltmax,iunit2,tiltnew,igroup,ncompsrch,tiltadd, znew, imageBinned)
      call write_xyz_model(modelfile,allxyz,igroup,nrealpt)
c       
c       Write separate residual outputs now that surfaces are known
c       
      if (pipinput .and. nsurface .gt. 1) then
        if (PipGetString('OutputTopBotResiduals', modelfile) .eq. 0) then
          residualfile = concat(modelfile,'.botres')
          do j = 1, 2
            nbot = 0
            do jpt=1,nrealpt
              if (igroup(jpt) .eq. j) then
                nbot = nbot + irealstr(jpt+1) - irealstr(jpt)
              endif
            enddo
            if (nbot .gt. 0) then
              call dopen(13,residualfile, 'new', 'f')
              write(13,'(i6,a)')nbot,' residuals'
              do jpt=1,nrealpt
                if (igroup(jpt) .eq. j) then
                  do i=irealstr(jpt),irealstr(jpt+1)-1
                    write(13, '(2f10.2,i5,3f8.2)')xx(i)+xcen,yy(i)+ycen,
     &                  mapviewtofile(isecview(i))-1,
     &                  xresid(i),yresid(i)
                  enddo
                endif
              enddo
              close(13)
              
            endif
            residualfile = concat(modelfile,'.topres')
          enddo
        endif
      endif
c       
c       Ask about local alignments
c       
      if (pipinput) then
        iflocal = ifDoLocal
      else
        write(*,'(1x,a,$)') '1 to do series of local alignments, 0 to exit: '
        read(5,*,err=209,end=209)iflocal
      endif
      if(iflocal.eq.0)go to 209
c       
      ifDoLocal = iflocal
      if(iwhichout.lt.0)call errorexit(
     &    'SOLUTION TRANSFORMS MUST BE OUTPUT TO DO LOCAL ALIGNMENTS', 0)
      errsumLocal = 0.
      errLocalMin = 1.e10
      errLocalMax = -10.
      ifvarout=0
      ifresout=0
      ifxyzout=0
      npatchx = 5
      npatchy = 5
      xpmin = 0.5
      ypmin = 0.5
      minfidtot = 8
      minfidsurf = 3
      ifxyzfix = 0
      iunlocal=9
      nxTarget = 700
      nyTarget = 700
      if (pipinput) then
        if (PipGetString('OutputLocalFile', modelfile) .ne. 0) call errorexit
     &      ('NO OUTPUT FILE FOR LOCAL TRANSFORMS SPECIFIED',0)
        itmp = PipGetTwoIntegers('NumberOfLocalPatchesXandY', npatchx, npatchy)
        kk = PipGetTwoIntegers('TargetPatchSizeXandY', nxTarget, nyTarget)
        ierr = PipGetTwoFloats('MinSizeOrOverlapXandY', xpmin,ypmin)
        ierr = PipGetTwoIntegers('MinFidsTotalAndEachSurface',
     &      minfidtot,minfidsurf)
        ierr = PipGetBoolean('FixXYZCoordinates', ifxyzfix)
        ierr = PipGetThreeIntegers('LocalOutputOptions', ifvarout,
     &      ifxyzout,ifresout)
        if (itmp .eq. 0 .and. kk .eq. 0) call errorexit('YOU CANNOT ENTER '//
     &      'BOTH A NUMBER OF LOCAL PATCHES AND A TARGET SIZE', 0)
        useTarget = itmp .gt. 0
        if (useTarget .and. (xpmin .gt. 1. .or. ypmin .gt. 1)) call errorexit
     &      ('YOU CANNOT ENTER A MINIMUM PATCH SIZE WITH A TARGET SIZE', 0)
      else
c         
        useTarget = .false.
        write(*,'(1x,a,$)')
     &      'Name of output file for local transformations: '
        read(5,'(a)')modelfile
c         
        write(*,'(1x,a,$)')'Number of patches in X and Y: '
        read(5,*)npatchx,npatchy
        write(*,'(1x,a,/,a,$)')'Enter either the minimum size of '//
     &      'patches in X and Y (values > 1) or the',
     &      'minimum fractional overlap between patches in'//
     &      ' X and Y (values < 1): '
        read(5,*)xpmin,ypmin
        write(*,'(1x,a,$)')'Minimum total # of fiducials, minimum '//
     &      'on one surface if two surfaces: '
        read(5,*)minfidtot,minfidsurf
        write(*,'(1x,a,$)')'1 to fix XYZ coordinates to global '//
     &      'solution, 0 to solve for them also: '
        read(5,*)ifxyzfix
        write(*,'(1x,a,/,a,$)')'Enter 1 for full output of variables,'
     &      //' 1 for output of XYZ coordinates,', ' and 1 for output'
     &      //' of points with high residuals (0 for no output): '
        read(5,*)ifvarout,ifxyzout,ifresout
      endif
c       
      call dopen(iunlocal,modelfile,'new','f')
      iflocal=1
      xyzfixed=ifxyzfix.ne.0
c       
c       set for incremental solution - could be input as option at this point
c       
      incrdmag=1
      incrgmag=1
      incrskew=1
      incrtilt=1
      incrrot=1
      incralf=1
c       
c       save transforms, scale angles back to radians (global solution
c       already saved)
c
      do iv=1,nview
        call xfcopy(fl(1,1,iv),glbfl(1,1,iv))
        tilt(iv)=glbtilt(iv)
        mallViewToFile(iv) = mapViewToFile(iv)
      enddo
      do i=1,nfileviews
        mallFileToView(i)=mapfiletoview(i)
      enddo
      nAllView = nview
c       
      nallprojpt=nprojpt
      do i=1,nprojpt
        allxx(i)=xx(i)
        allyy(i)=yy(i)
        iallsecv(i)=isecview(i)
      enddo
c       write(*,121)
c       121    format(/,11x,'Absolute 3-D point coordinates'
c       &     ,/,'   #',7x,'X',9x,'Y',9x,'Z')
c       write(*,'(i4,3f10.2)',err=86)
c       &     (j,(allxyz(i,j),i=1,3),j=1,nrealpt)

      if (useTarget) then
c         
c         If using a target, use the real extent of the data, get the number
c         of patches that fill the extent at the target size, then get the 
c         real size with the defined overlap
        allXmin = 1.e10
        allXmax = -1.e10
        allYmin = 1.e10
        allYmax = -1.e10
        allBorder = 5.
        do i=1,nallrealpt
          allXmin = min(allXmin, allxyz(1,i) - allBorder)
          allXmax = max(allXmax, allxyz(1,i) + allBorder)
          allYmin = min(allYmin, allxyz(2,i) - allBorder)
          allYmax = max(allYmax, allxyz(2,i) + allBorder)
        enddo
c        print *, allXmin, allXmax ,allYmin,allYmax
c
        npatchx = (allXmax - allXmin - nxTarget) / (nxTarget * (1. - xpmin)) +1
        npatchx = max(2,npatchx)
        nxpmin = (allXmax - allXmin) / (npatchx - xpmin * (npatchx - 1))
        if (nxpmin .gt. 1.05 * nxTarget) then
          npatchx = npatchx + 1
          nxpmin = (allXmax - allXmin) / (npatchx - xpmin * (npatchx - 1))
        endif
        idxpatch = (allXmax - allXmin - nxpmin) / max(1,npatchx-1) + 1
        ixspatch = allXmin + nxpmin / 2
c
        npatchy = (allYmax - allYmin - nyTarget) / (nyTarget * (1. - ypmin)) +1
        npatchy = max(2,npatchy)
        nypmin = (allYmax - allYmin) / (npatchy - ypmin * (npatchy - 1))
        if (nypmin .gt. 1.05 * nyTarget) then
          npatchy = npatchy + 1
          nypmin = (allYmax - allYmin) / (npatchy - ypmin * (npatchy - 1))
        endif
        idypatch = (allYmax - allYmin - nypmin) / max(1,npatchy-1) + 1
        iyspatch = allYmin + nypmin / 2
        write(*,120)nint(allXmax - allXmin - 2. * allBorder), nint(allYmax -
     &      allYmin - 2. * allBorder),npatchx,npatchy,nxpmin,nypmin
120     format(/,'Extent of fiducials is',i6,' and',i6,' pixels in X and Y',
     &      /,'Doing',i3,' by',i3,' local areas, minimum size',i5,' x',i5)
      else
c         
c         legacy behavior with # of patches entered: get the minimum patch 
c         size from full size of image area
c         
        npatchx = max(2,npatchx)
        npatchy = max(2,npatchy)
        if (xpmin.gt.1.) then
          nxpmin = xpmin
        else
          nxpmin = 2*xcen/(npatchx - xpmin * (npatchx - 1))
        endif
        if (ypmin.gt.1.) then
          nypmin = ypmin
        else
          nypmin = 2*ycen/(npatchy - ypmin * (npatchy - 1))
        endif
c         
c         set up starting patch locations and intervals
c         
        idxpatch=(nint(2*xcen)-nxpmin)/max(1,npatchx-1)
        idypatch=(nint(2*ycen)-nypmin)/max(1,npatchy-1)
        ixspatch=nxpmin/2
        iyspatch=nypmin/2
      endif
      ipatchx=0
      ipatchy=1
c       
c       START OR CONTINUE LOOPING ON LOCAL REGIONS
c       
200   ipatchx=ipatchx+1
      if(ipatchx.gt.npatchx)then
        ipatchx=1
        ipatchy=ipatchy+1
        if(ipatchy.gt.npatchy)then
          close(iunlocal)
          if (ifresout .gt. 0) print *
          write(*,119)errsumLocal / (npatchx * npatchy),
     &        errLocalMin,errLocalMax
119       format(/,' Residual error local mean:',f9.3,'    range', f8.3,
     &        ' to',f8.3)
          go to 209
        endif
      endif
      ixpatch=ixspatch+(ipatchx-1)*idxpatch
      iypatch=iyspatch+(ipatchy-1)*idypatch
c       
c       find the points whose real X and Y coordinates are within the bounds
c       of the patch; expand the patch if necessary to achieve the minimum
c       number of fiducials.  Load points from the "all" arrays into the
c       current arrays
c       
      nxp=nxpmin-40
      nyp=nypmin-40
      nrealpt=0
      minsurf=0
      do while (nxp.lt.4*xcen.and.nyp.lt.4*ycen.and.
     &    (nrealpt.lt.minfidtot.or.
     &    (nsurface.ge.2.and.minsurf.lt.minfidsurf)))
        nxp=nxp+40
        nyp=nyp+40
        nrealpt=0
        nbot=0
        ntop=0
        ixmin=ixpatch-nxp/2
        ixmax=ixmin+nxp
        iymin=iypatch-nyp/2
        iymax=iymin+nyp
        do i=1,nallrealpt
          if(allxyz(1,i).ge.ixmin.and.allxyz(1,i).le.ixmax.and.
     &        allxyz(2,i).ge.iymin.and.allxyz(2,i).le.iymax)then
            nrealpt=nrealpt+1
            indallreal(nrealpt)=i
            if(nsurface.ge.2)then
              if(igroup(i).eq.1)nbot=nbot+1
              if(igroup(i).eq.2)ntop=ntop+1
            endif
          endif
        enddo
        minsurf=min(nbot,ntop)
      enddo
      if(nxp.ge.4*xcen.and.nyp.ge.4*ycen)then
        toofewfid=.true.
        go to 209
      endif
c       
c       Get count of points in each view so empty views can be eliminated, then
c       get mapping from all views to remaining views in this local area
      call countNumInView(indallreal, nrealpt, iallrealstr, iallsecv, nAllView,
     &    ninview)
      localv = 0
      do iv = 1, nfileviews
        mapFileToView(iv) = 0
      enddo
      do iv = 1, nAllView
        if (ninview(iv) .gt. 0) then
          localv = localv + 1
          mapAllToLocal(iv) = localv
          mapLocalToAll(localv) = iv
          mapViewToFile(localv) = mallViewToFile(iv)
          mapFileToView(mallViewToFile(iv)) = localv
          tilt(localv) = glbtilt(iv)
        else
          mapAllToLocal(iv) = 0
        endif
      enddo
      nview = localv
c       
c       Now load the coordinate data with these local view numbers
      nprojpt=0
      do ll = 1, nrealpt
        listreal(ll) = ll
        i = indallreal(ll)
        irealstr(ll)=nprojpt+1
        do j=1,ninreal(i)
          nprojpt=nprojpt+1
          kk=J+iallrealstr(i)-1
          xx(nprojpt)=allxx(kk)
          yy(nprojpt)=allyy(kk)
          isecview(nprojpt)=mapAllToLocal(iallsecv(kk))
        enddo
      enddo
      irealstr(nrealpt+1)=nprojpt+1
      call countNumInView(listreal, nrealpt, irealstr, isecview, nview,
     &    ninview)

      call input_vars(var,varname,inputalf,nvarsrch,nvarang,nvarscl,
     &    imintilt, ncompsrch,iflocal,maptiltstart,mapalfstart,mapalfend,
     &    ifBTSearch, tiltorig,tiltadd,pipinput,xyzfixed,ninview,ninThresh)
c       
c       DNM 7/16/04: Add pixel size to local file
c       2/15/07: Output after first read of variables
      ifanyalf = 0
      if (mapalfend .gt. mapalfstart .or. beamTilt .ne. 0)ifanyalf = 1
      if (iflocal .eq. 1) write(iunlocal,'(7i6,f12.5,i4)')npatchx,npatchy,
     &    ixspatch,iyspatch, idxpatch,idypatch,ifanyalf,xdelt,ifZfac
      iflocal = 2
c       
c       take care of initializing the mapped variables properly
c       
c$$$    if(ifrotfix.eq.0)then
c$$$    globrot=glbrot(1)
c$$$    var(1)=globrot
c$$$    maptiltstart=nview+1
c$$$    else
c$$$    globrot=glbrot(ifrotfix)
c$$$    rot(ifrotfix)=globrot
c$$$    maptiltstart=nview
c$$$    endif
c$$$    do i=1,nview
c$$$    if((ifrotfix.eq.0.and.i.gt.1) .or. i.lt.ifrotfix)then
c$$$    var(i)=glbrot(i)-globrot
c$$$    elseif(ifrotfix.gt.0 .and. i.gt.ifrotfix)then
c$$$    var(i-1)=glbrot(i)-globrot
c$$$    endif
c$$$    enddo
c       
c       reload the geometric variables
c       
      call reload_vars(glbrot,rot,maprot,frcrot,nview,
     &    1,maptiltstart-1,var,fixeddum,1, mapLocalToAll)
      call reload_vars(glbtilt,tilt,maptilt,frctilt,nview,
     &    maptiltstart, nvarang,var,fixeddum,incrtilt,mapLocalToAll)
c       
c       if doing tilt incremental, just set tiltinc to the global tilt and
c       all the equations work in map_vars
c       
      if(incrtilt.ne.0)then
        fixedtilt2=0.
        fixedtilt=0.
        do i=1,nview
          tiltinc(i)=glbtilt(mapLocalToAll(i))
        enddo
      endif
      call reload_vars(glbgmag,gmag,mapgmag,frcgmag,nview, nvarang+1,
     &    mapdmagstart-ncompsrch-1,var,fixedgmag,incrgmag,mapLocalToAll)
      call reload_vars(glbdmag,dmag,mapdmag,frcdmag,nview, mapdmagstart,
     &    nvarscl,var,fixeddmag,incrdmag,mapLocalToAll)
      call reload_vars(glbskew,skew,mapskew,frcskew,nview, nvarscl+1,
     &    mapalfstart-1,var,fixedskew,incrskew,mapLocalToAll)
      call reload_vars(glbalf,alf,mapalf,frcalf,nview, mapalfstart,nvarsrch,
     &    var,fixedalf,incralf,mapLocalToAll)
c       
c       get new scaling and scale projection points
c       
      scalexy=0.
      do i=1,nprojpt
        scalexy=max(scalexy,abs(xx(i)),abs(yy(i)))
      enddo
      do i=1,nprojpt
        xx(i)=xx(i)/scalexy
        yy(i)=yy(i)/scalexy
      enddo
c       
c       load the xyz's and shift them to zero mean and scale them down
c       
      xsum=0.
      ysum=0.
      zsum=0.
      do i=1,nrealpt
        j=indallreal(i)
        xyz(1,i)=allxyz(1,j)-xcen
        xsum=xsum+xyz(1,i)
        xyz(2,i)=allxyz(2,j)-ycen
        ysum=ysum+xyz(2,i)
        xyz(3,i)=allxyz(3,j)
        zsum=zsum+xyz(3,i)
      enddo
      xshft=xsum/nrealpt
      yshft=ysum/nrealpt
      zshft=zsum/nrealpt
      do i=1,nrealpt
        xyz(1,i)=(xyz(1,i)-xshft)/scalexy
        xyz(2,i)=(xyz(2,i)-yshft)/scalexy
        xyz(3,i)=(xyz(3,i)-zshft)/scalexy
      enddo
      write(*,'(/,a,2i3,a,2i5,a,2i5,a,i3,a)')' Doing local area',
     &    ipatchx,ipatchy, ', centered on',ixpatch,iypatch,', size',
     &    nxp,nyp,',  ',nrealpt,' fiducials'
      if(minsurf.gt.0)write(*,'(a,i3,a,i3,a)')'    (',nbot,
     &    ' on bottom and',ntop,' on top)'
      ncycle=-abs(ncycle)
      go to 180
209   close(7)
      if(metroerror.ne.0)print *,'WARNING:',metroerror,
     &    ' MINIMIZATION ERRORS OCCURRED'
      if (toofewfid) call errorexit(
     &    'Minimum numbers of fiducials are too high - check if '//
     &    'there are enough fiducials on the minority surface', 0)

      call exit(0)
      end


      subroutine errorexit(message, iflocal)
      implicit none
      integer*4 iflocal
      character*(*) message
      if (iflocal.ne.0) then
        write(*,'(/,a,a)')'WARNING: ', message
        return
      endif
      write(*,'(/,a,a)')'ERROR: TILTALIGN - ', message
      call exit(1)
      end

c       
c       $Log$
c       Revision 3.42  2009/10/06 02:30:38  mast
c       Output rotation angle at minimum tilt
c
c       Revision 3.41  2008/12/12 00:47:36  mast
c       Call find_surfaces with binnig and z shift
c
c       Revision 3.40  2008/11/02 13:51:25  mast
c       Call new function for converting angles for beam tilt
c
c       Revision 3.39  2008/07/06 23:39:03  mast
c       Eliminated former total unknown ratio
c
c       Revision 3.38  2008/03/05 00:34:05  mast
c       Increased maxmetro to go along with increase in maxreal
c
c       Revision 3.37  2007/12/12 17:20:08  mast
c       Had to add mapalfend to two calls to input_vars
c
c       Revision 3.36  2007/12/11 22:23:17  mast
c       Removed X tilt/rotation warning if solving for only one X-tilt variable
c
c       Revision 3.35  2007/11/18 04:57:10  mast
c       Redeclared concat at 320
c
c       Revision 3.34  2007/05/03 23:58:50  mast
c       Switched from projection stretch to projection skew
c
c       Revision 3.33  2007/03/08 23:48:57  mast
c       Enforced minimum of 2 x 2 local areas, adjusted arguments to runMetro
c
c       Revision 3.32  2007/03/05 22:30:43  mast
c       Changed initial beam tilt option
c
c       Revision 3.31  2007/02/19 21:13:25  mast
c       Changes for beam tilt solving by variable fitting and by one-dimensional
c       search; added option of specifying target size of local areas; made
c       local fits drop views with no points; made number of views in groups
c       depend on the number of points in views
c
c       Revision 3.30  2006/07/18 00:10:20  mast
c       Fixed formatting in errorexit and output global and local area # and
c       local area mean and range for mean residuals
c       
c       Revision 3.29  2005/10/11 21:38:47  mast
c       Updated PIP fallback options
c       
c       Revision 3.28  2005/07/01 19:45:27  mast
c       Chnaged formats to allow bigger variable/measurement totals
c       
c       Revision 3.27  2005/07/01 19:34:35  mast
c       Added correct ratio of measurements to unknowns
c       
c       Revision 3.26  2005/06/26 19:51:54  mast
c       Added a blank line after residual output before exiting (?)
c       
c       Revision 3.25  2005/06/09 19:20:18  mast
c       Added image binned option so that Z shift can be entered unbinned
c       
c       Revision 3.24  2005/04/20 16:26:51  mast
c       Added a success message after the restart messages
c       
c       Revision 3.23  2005/04/20 04:46:05  mast
c       Converted WARNINGS to messages for metro errors until trials all fail
c       
c       Revision 3.22  2005/04/15 22:39:44  mast
c       Fixed sign in computation of xzOther
c       
c       Revision 3.21  2005/03/14 06:05:54  mast
c       Increased maxmetro limit
c       
c       Revision 3.20  2005/02/16 06:43:10  mast
c       Added image size to fid.xyz output file for solvematch to use
c       
c       Revision 3.19  2004/10/24 22:38:13  mast
c       Fixed a line length - forgot to say changes to compute the image
c       transformations more robustly and to put out Z factors
c       
c       Revision 3.18  2004/10/24 22:30:27  mast
c       Converted to PIP input
c       
c       Revision 3.17  2004/10/08 17:29:57  mast
c       Eliminated manual info
c       
c       Revision 3.16  2004/10/08 17:27:14  mast
c       Fixed failure to get both model and residual output with a filename
c       containing a period.
c       
c       Revision 3.15  2004/09/16 16:12:30  mast
c       Made it try new metro factors upon error; switched to opening
c       fid.xyz only when ready to write it.
c       
c       Revision 3.14  2004/07/16 23:24:21  mast
c       Added pixel size to local alignment file
c       
c       Revision 3.13  2004/06/10 05:39:18  mast
c       Output pixel size in fiducial file
c       
c       Revision 3.12  2004/05/21 20:06:34  mast
c       Put out iteration limit error as a formal WARNING
c       
c       Revision 3.11  2004/05/07 23:41:21  mast
c       Fixed problem with Z shift being setto zero
c       
c       Revision 3.10  2004/05/05 05:50:26  mast
c       Output real 3D coordinates, fix bug in getting local residuals,
c       and finally added +/-10% to the messages about metro factor
c       
c       Revision 3.9  2003/10/24 03:31:54  mast
c       remove tab from label scanned by alignlog
c       
c       Revision 3.8  2003/10/03 00:59:07  mast
c       Changed terminology to refered to tilt angle offset
c       
c       Revision 3.7  2003/01/30 20:54:51  mast
c       Made fields for residuals bigger, amplified IER error messages
c       
c       Revision 3.6  2002/12/21 00:00:33  mast
c       Add ability to get both residual output and 3D model
c       
c       Revision 3.5  2002/10/17 23:18:31  mast
c       Added proper error message and exit for minimum number of beads too
c       high in local alignments
c       
c       Revision 3.4  2002/07/28 23:02:54  mast
c       Needed to declare lnblnk for SGI
c       
c       Revision 3.3  2002/07/28 22:42:35  mast
c       Changes to output a residual listing file and to standardize error
c       exits and output
c       
c       Revision 3.2  2002/05/09 03:48:38  mast
c       Fixed a line length that did not compile on SGI
c       
c       Revision 3.1  2002/05/07 02:05:19  mast
c       Changes to handle subset of views better: output of transforms and
c       tilt angles for all views in file, and interpretation of user input
c       and all output in terms of view numbers in file rather than in
c       program.  Also changed the surface analysis output to make it more
c       understandable and machine readable.
c       
c       5/19/89 added model output, changed format of output table
c       6/21/89 added mean residual output to find_surfaces, changed to
c       get recommendation on maximum FIXED tilt angle
c       4/9/93 allow full mapping of compression variables
c       10/30/95 added distortion, automapping, point & angle output.
c       10/17/98 added linear combinations to automapping
c       2/12/98 added local alignments; changed find_surfaces to find and
c       recommend an X-axis tilt.

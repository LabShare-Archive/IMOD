*       * * * * * BEADTRACK * * * * * *
c       
c       BEADTRACK will track selected fiducial gold beads through a series of
c       tilted views.  It takes a "seed" model, where each bead of choice
c       is marked with at least a single point on a view near zero tilt.  It
c       tracks each bead as far as possible and puts out a new model.
c       
c       David Mastronarde, 1995
c       added tilt alignment, 10/6/97
c       
c       $Id$
c
      use tltcntrl
      use mapsep
      use cgpixels
      implicit none
      include 'smallmodel.inc'
      include 'statsize.inc'
c       
      integer npad,maxarr,limpcl,maxarea,limgaps,maxpeaks,msizexr
      integer liminside,limedge,maxolist,limresid, maxAllReal
      character*80 titlech

      integer*4 nx,ny,nz,NXYZ(3),MXYZ(3)
      equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
C       
      real*4 TITLE(20), matKernel(49)
      real*4, allocatable :: boxes(:,:,:), cursum(:), boxtmp(:),corrSum(:,:),sobelSum(:,:)
      real*4, allocatable :: ARRAY(:),BRRAY(:),sarray(:),sbrray(:)
      real*4, allocatable :: refSobel(:), tmpSobel(:), boxSobel(:), sobelWsums(:,:)
      real*4, allocatable :: sobelXpeaks(:,:), sobelYpeaks(:,:), sobelPeaks(:,:)
c       maxAllReal
      real*4, allocatable :: seqdist(:),xxtmp(:),yytmp(:), xvtmp(:), yvtmp(:)
      integer*4, allocatable :: indgap(:), numBotTop(:,:)
      logical*1, allocatable :: inAnArea(:)
C       
      integer*4, allocatable :: ixpclist(:),iypclist(:),izpclist(:),listz(:), incore(:,:)
      CHARACTER*320 FILIN,FILOUT,plfile,modelfile,surfaceFile
c       maxview except wsave
      integer*4, allocatable :: izexclude(:),ipclose(:),izclose(:)
      integer*4, allocatable :: ivlist(:),ivSnapList(:)
      logical, allocatable :: missing(:)
      real*4, allocatable :: wsave(:,:),prevres(:)
      character*6 areaObjStr
      character*9 dat
      character*8 tim
      logical exist,readSmallMod
c       
c       maxreal
      real*4, allocatable :: xseek(:),yseek(:),wcrit(:),xr(:,:)
      integer*4, allocatable :: nws(:),iffound(:), ipnearest(:), ipnearsav(:)
      integer*4, allocatable :: iobjdel(:),idrop(:),igrpBotTop(:), numInSobelSum(:)
      logical, allocatable :: inCorrSum(:,:), inSobelSum(:,:)

      real*4, allocatable :: resmean(:)
c       maxarea
      integer*4, allocatable :: iareaseq(:),ninobjlist(:),indobjlist(:)
      real*4, allocatable :: areadist(:)
      integer*4, allocatable :: ivseqst(:),ivseqnd(:),listseq(:)
c
      integer*2, allocatable :: ivgap(:)
c       maxolist
      real*4, allocatable :: residLists(:)
      integer*4, allocatable :: iobjlists(:), iobjlistmp(:)
      integer*1, allocatable :: listsBotTop(:)
      real*4 ctf(8193)
      character*1024 listString
      character*60 addfmt,delfmt1,delfmt2
      character*14 objfmt
      character*20 objnames(5)/'Model projection', 'Fitted point',
     &    'Correlation peak' , 'Centroid', 'Saved point'/
C       
      real*4 xf(2,3)
c       
      integer*4 modebox,maxwavg,limpstr,limpmag,limprot,limpshft
      real*4 rotstart,fraccrit,dmin2,dmax2,dmean2
      integer*4 ifwhite,iffillin,maxgap,mode,k,kti,lastseq,iobjdo,ip,igap
      integer*4 nround,maxsum,nfit,minfit,maxresid,minresid,ierr,npclist,i,j
      real*4 sdcrit,distcrit,relaxint,relaxdis,fitdistcrit,relaxfit,radmaxfit
      real*4 resdifmin,resdifcrit, tiltfitmin,cgrad,tiltmin,xst,xnd,yst,ynd
      integer*4 minxpiece,nxpieces,nxoverlap,minypiece,nypieces,nyoverlap
      integer*4 nxtotpix,nytotpix,nexclude,ig,iaxtilt,nxlocal,nylocal
      integer*4 nxbox,nybox,nxpad,nypad,npixbox,nxpdim,iftrace,iv
      integer*4 minendz,indfree,iobj,ibase,ninobj,ipt,iz,jz,jpt,itmp,iznext
      integer*4 maxnpt,nareax,nareay,ix,iy,nobjlists
      integer*4 indstart,nobjtot,nseqs,ipass,limcg,nzout
      real*4 radpix,boxsum,boxmin,boxmax,refsum,refmin,refmax,corsum,cormin
      real*4 cormax,xbox,ybox,xt,yt,xtmp,ytmp,wsum,tiltcur,dxcur
      integer*4 lastlist,iseq,nadded,ivseq,idir,nvlist,iview,ivuse,ib
      integer*4 ifexclude,iexcl,ivl,ntodo,nclose,neardiff,izv,ipcz
      integer*4 ix0,ix1,iy0,iy1,ic,jc,maxuse,ibox,izbox
      integer*4 ifdidalign,ivdel,indr,ndat,navg,idif,idirw,itry
      real*4 rotcur,dycur,cosr,sinr,cost,sint,a,b,c,d,e,f,wsumsq,wstmp
      real*4 xnext,ynext,relax,radmax,xpeak,ypeak,xpos,ypos,devavg,devsd
      real*4 devmax,errmax,curdif,resavg,ressd,ressem,gmagcur,wavg,wsd
      integer*4 npioneer,iftrans,ifrotrans,ipntmax,nadtmp,ivlook
      integer*4 ifmeanbad,iscur,nprev,ndel,misstot,nlistz,imodobj,imodcont
      real*4 peak,dist,tmin,tmax,tmean,cvbxcen, cvbycen,area,density
      integer*4 nvert,minInArea,minBeadOverlap,ifLocalArea, localTarget
      integer*4 nvLocalIn, localViewPass, ibaseRes,nSnapList,iobjSave
      logical keepGoing,saveAllPoints,ignoreObjs, done, highestSobel
      integer*4 numNew,nOverlap,iseqPass,ipassSave,iAreaSave,maxObjOrig
      real*4 outlieElimMin, outlieCrit, outlieCritAbs, curResMin, tiltmax, minPeakRatio
      real*4 sigma1,sigma2,radius2,radius1,deltactf, ranFrac, diameter, sobelSigma
      integer*4 maxDrop, nDrop, ndatFit, nareaTot, maxInArea, limInArea, interpType
      integer*4 minViewDo, maxViewDo, numViewDo, numBound, iseed, limcxBound,nfarther
      integer*4 ibaseOnAlign, ifAlignDone, imageBinned, maxSobelSum, maxAnySum
      integer*4 nxSobel, nySobel, nxsPad, nysPad, ninSobel, kernelDim
      real*4 xOffSobel, yOffSobel, scaleFacSobel, scaleByInterp, targetSobel
      character*320 concat
      integer*4 getImodObjsize, niceframe, surfaceSort, scaledSobel
      logical itemOnList
      real*4 ran
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg,PipGetLogical
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString,PipGetFloat, PipGetTwoIntegers, PipGetTwoFloats
      integer*4 PipGetInOutFile, PipNumberOfEntries, ifpip
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  beadtrack
c       
      integer numOptions
      parameter (numOptions = 52)
      character*(40 * numOptions) options(1)
      options(1) =
     &    ':InputSeedModel:FN:@:OutputModel:FN:@:ImageFile:FN:@'//
     &    ':PieceListFile:FN:@:ImagesAreBinned:I:@:SurfaceOutputFile:FN:@'//
     &    ':SkipViews:LI:@:RotationAngle:F:@:SeparateGroup:LIM:@'//
     &    'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'//
     &    'tiltfile:TiltFile:FN:@angles:TiltAngles:FAM:@'//
     &    ':TiltDefaultGrouping:I:@:TiltNondefaultGroup:ITM:@'//
     &    ':MagDefaultGrouping:I:@:MagNondefaultGroup:ITM:@'//
     &    ':RotDefaultGrouping:I:@:RotNondefaultGroup:ITM:@'//
     &    ':MinViewsForTiltalign:I:@:CentroidRadius:F:@:BeadDiameter:F:@'//
     &    ':LightBeads:B:@:FillGaps:B:@:MaxGapSize:I:@'//
     &    ':MinTiltRangeToFindAxis:F:@:MinTiltRangeToFindAngles:F:@'//
     &    ':BoxSizeXandY:IP:@:RoundsOfTracking:I:@:MaxViewsInAlign:I:@'//
     &    ':RestrictViewsOnRound:I:@:LocalAreaTracking:B:@'//
     &    ':LocalAreaTargetSize:I:@:MinBeadsInArea:I:@:MaxBeadsInArea:I:@'//
     &    ':MinOverlapBeads:I:@:TrackObjectsTogether:B:@'//
     &    ':MaxBeadsToAverage:I:@:PointsToFitMaxAndMin:IP:@'//
     &    ':DensityRescueFractionAndSD:FP:@:DistanceRescueCriterion:F:@'//
     &    ':RescueRelaxationDensityAndDistance:FP:@'//
     &    ':PostFitRescueResidual:F:@:DensityRelaxationPostFit:F:@'//
     &    ':MaxRescueDistance:F:@:ResidualsToAnalyzeMaxAndMin:IP:@'//
     &    ':DeletionCriterionMinAndSD:FP:@param:ParameterFile:PF:@'//
     &    'help:usage:B:@:BoxOutputFile:FN:@:SnapshotViews:LI:@'//
     &    ':SaveAllPointsAreaRound:IP:'
c
      maxwavg=15
      limpstr=16
      limpmag=6
      limprot=4
      limpshft=3
      mininview=4
      maxh = 0
      facm=.25
      ncycle=1000
      eps=0.00002                               !was .00001 then .00002
      ifpip = 0 
      plfile = ' '
      rotstart = 0.
      minvtiltali = 4
      ifwhite = 0
      iffillin = 0
      maxgap = 5
      randoaxis = 10
      randotilt = 20
      ifLocalArea = 0
      minInArea = 8
      minBeadOverlap = 3
      localTarget = 1000
      nround = 1
      maxsum = 4
      nfit = 7
      minfit = 3
      fraccrit = 0.6
      sdcrit = 1.
      distcrit = 10.
      relaxint = 0.7
      relaxdis = 0.9
      fitdistcrit = 2.5
      relaxfit = 0.9
      radmaxfit = 2.5
      maxresid = 9
      minresid = 5
      resdifmin = 0.04
      resdifcrit = 2
      modebox = 0
      ipassSave = 0
      iAreaSave = 0
      outlieCrit = 0.01
      outlieCritAbs = 0.002
      outlieElimMin = 2.
      curResMin = 0.5
      deltactf = 0.
      sigma1 = 0.00
      sigma2 = 0.05
      radius2 = 0.
      radius1 = 0.
      areaObjStr = 'object'
      limInArea = 1000
      limcxBound = 2500
      imageBinned = 1
      npad = 8
      maxSobelSum = 0
      scaleByInterp = 1.2
      targetSobel = 8.
      sobelSigma = 0.85
      interpType = 0
      maxpeaks = 20
      highestSobel = .false.
      minPeakRatio = 0.2
      msizexr = 19
c       
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'beadtrack',
     &    'ERROR: BEADTRACK - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (pipinput) then
        ifpip = 1
        if (PipGetString('ImageFile', filin) .ne. 0) call errorexit
     &      ('NO IMAGE INPUT FILE SPECIFIED',0)
        ierr = PipGetString('PieceListFile', plfile)
      else

        write(*,'(1x,a,$)')'Image input file: '
        READ(5,101)FILIN
101     format(a)
        write(*,'(1x,a,$)')'Piece list file if image is montage,'//
     &      ' otherwise Return: '
        read(*,101)plfile
      endif
c       
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C       
      limpcl = nz + 10
      allocate(ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl),listz(limpcl),stat=ierr)
      call memoryError(ierr, 'PIECE LIST ARRAYS')

      call read_piece_list2(plfile,ixpclist,iypclist,izpclist,npclist, limpcl)
c       
c       if no pieces, set up mocklist
c       
      if(npclist.eq.0)then
        do i=1,nz
          ixpclist(i)=0
          iypclist(i)=0
          izpclist(i)=i-1
        enddo
        npclist=nz
      endif
      call fill_listz(izpclist,npclist,listz,nvuall)
c       
c       assign maxview the same as in alivar based on actual maximum views and allocate
      maxview = nvuall + 4
      allocate(izexclude(maxview),ipclose(maxview),izclose(maxview), ivlist(maxview),
     &    ivSnapList(maxview), missing(0:maxview), prevres(maxview),  tiltorig(maxview),
     &    gmagorig(maxview),rotorig(maxview),dxysav(2,maxview),tltall(maxview),
     &    ivsepIn(maxview,maxgrp), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR VIEWS')
      call allocateMapsep(ierr)
      call memoryError(ierr, 'ARRAYS IN MAPSEP')

      call checklist(ixpclist,npclist,1,nx,minxpiece ,nxpieces,nxoverlap)
      nxtotpix=nx+(nxpieces-1)*(nx-nxoverlap)
      call checklist(iypclist,npclist,1,ny,minypiece ,nypieces,nyoverlap)
      nytotpix=ny+(nypieces-1)*(ny-nyoverlap)
      xcen=nxtotpix/2.
      ycen=nytotpix/2.
      scalexy=sqrt(xcen**2+ycen**2)
c       
      if (PipGetInOutFile('InputSeedModel', 1, 'Input seed model file', modelfile) .ne. 0)
     &    call errorexit ('NO INPUT SEED MODEL FILE SPECIFIED', 0)
c       
      exist=readSmallMod(modelfile)
      if(.not.exist)call errorexit('READING SEED MODEL FILE', 0)
      if (n_point .eq. 0 .or. max_mod_obj .eq. 0)
     &    call errorexit('INPUT SEED MODEL IS EMPTY', 0)
c       
c       Initial allocations based on model size, and way oversized temp array
c       for object lists, which is also needed for gaps
      maxAllReal = max_mod_obj + 10
      maxolist = max(40*maxAllReal, 100000)
      allocate(seqdist(maxAllReal), xxtmp(maxAllReal), yytmp(maxAllReal),
     &    xvtmp(maxAllReal), yvtmp(maxAllReal), 
     &    numBotTop(2,maxAllReal), iobjseq(maxAllReal), xyzsav(3,maxAllReal),
     &    iobjlistmp(maxolist), indgap(maxAllReal), inAnArea(maxAllReal), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR ALL CONTOURS IN MODEL')
c
c       convert to image index coordinates and change the origin and delta
c       for X/Y to reflect this
c       
      call scaleModelToImage(1, 0)
      xorig=0.
      yorig=0.
      xdelt=1.
      ydelt=1.
c       
      if (PipGetInOutFile('OutputModel', 2, 'Output model file', modelfile)
     &    .ne. 0) call errorexit('NO OUTPUT MODEL FILE SPECIFIED', 0)
c       
      filout=' '
      surfaceFile = ' '
      nexclude = 0
      if (pipinput) then
        ierr = PipGetString('BoxOutputFile', filout)
        ierr = PipGetString('SurfaceOutputFile', surfaceFile)
        if (PipGetString('SkipViews', listString) .eq. 0) call parselist2
     &      (listString, izexclude, nexclude, maxview)
        ierr = PipGetFloat('RotationAngle', rotstart)
        ierr = PipNumberOfEntries('SeparateGroup', ngsep)
        if (ngsep .gt. maxgrp) call errorexit
     &      ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
        do ig =1, ngsep
          ierr = PipGetString('SeparateGroup', listString)
          call parselist2(listString, ivsepIn(1,ig),nsepingrpIn(ig), maxview)
        enddo
      else
c         
        write(*,'(1x,a,$)')'List of views to skip over: '
        call rdlist(5,izexclude,nexclude)
c         
c         new parameters for tilt alignment
c         
        write(*,'(1x,a,$)')
     &      'Initial angle of rotation in projection plane: '
        read(5,*)rotstart
c         
c         Get list of views to treat separately in automapping
c         
        write(*,'(1x,a,/,a,$)') 'For automapping tilt and mag,'
     &      //' enter the number of sets of views to treat separately'
     &      //' from the main set of views (otherwise enter 0): '
        read(5,*)ngsep
        if (ngsep .gt. maxgrp) call errorexit
     &      ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
        do ig=1,ngsep
          write(*,'(1x,a,i3,a,$)')'List of views in set',ig,
     &        '(ranges OK): '
          call rdlist(5,ivsepIn(1,ig),nsepingrpIn(ig))
        enddo
      endif
c       
      iaxtilt=2
      if(abs(rotstart).ge.45.)iaxtilt=1

      call get_tilt_angles(nvuall,3,tltall, maxview, ifpip)
c       
c       DNM 5/3/02: accommodate changes to tiltalign by setting up the
c       mapping arrays, adding to automap call
c       DNM 3/25/05: mapping arrays are going to be used for real
c       4/11/05: default is to ignore objects for transferfid situations
c       
      nfileviews=nvuall
      ignoreObjs = nvuall.le.2
c       
      if (.not.pipinput)print *,
     &    'Specify grouping for mapping of tilt variables'
      call inputGroupings(nvuall, ifpip, 1,  'TiltDefaultGrouping',
     &    'TiltNondefaultGroup', nmapTilt, ivSpecStrTilt,
     &    ivSpecEndTilt, nmapSpecTilt, nRanSpecTilt, maxgrp)
c       
      if (.not.pipinput)print *,
     &    'Specify grouping for mapping of magnification variables'
      call inputGroupings(nvuall, ifpip, 1,  'MagDefaultGrouping',
     &    'MagNondefaultGroup', nmapMag, ivSpecStrMag,
     &    ivSpecEndMag, nmapSpecMag, nRanSpecMag, maxgrp)

      nmapRot = 1
      nRanSpecRot = 0
      if (pipinput) call inputGroupings(nvuall, ifpip, 0,
     &    'RotDefaultGrouping', 'RotNondefaultGroup', nmapRot,
     &    ivSpecStrRot, ivSpecEndRot, nmapSpecRot, nRanSpecRot, maxgrp)
c       
      nvLocalIn = 0
      localViewPass = 0
      tiltfitmin=15.
      nSnapList = 0
c       
      if (pipinput) then
        ierr = PipGetInteger('ImagesAreBinned', imageBinned)
        ierr = PipGetFloat('CentroidRadius', cgrad)
        j = PipGetFloat('BeadDiameter', diameter)
        if (ierr + j .ne. 1) call errorexit(
     &      'YOU MUST ENTER EITHER BeadDiameter OR CentroidRadius '//
     &      'BUT NOT BOTH', 0)
c         
c         compute diameter from cgrad by the formula used to get cgrad in
c         copytomocoms, then divide by binning and get cgrad back
        if (ierr .eq. 0) diameter = max(2., 2. * cgrad - 3.)
        diameter = diameter / imageBinned
        cgrad = 0.5 * (diameter + 3)
        if (PipGetTwoIntegers('BoxSizeXandY', nxbox, nybox) .ne. 0)
     &      call errorexit('YOU MUST ENTER A BOX SIZE', 0)

        if (imageBinned .gt. 1) then
c           
c           Adjust box size if binned.  The formula here is
c           size = 2 * max(0, diameter - 3) + 32
          ierr = max(0, int(diameter) - 3) + 16
          nxbox = 2 * max(nxbox / (imageBinned * 2), ierr)
          nybox = 2 * max(nybox / (imageBinned * 2), ierr)
          print *,'Box size in X and Y adjusted for binning to:',nxbox, nybox
        endif
        ierr = PipGetInteger('MinViewsForTiltalign', minvtiltali)
        ierr = PipGetBoolean('LightBeads', ifwhite)
        ierr = PipGetBoolean('FillGaps', iffillin)
        ierr = PipGetInteger('MaxGapSize', maxgap)
        ierr = PipGetFloat('MinTiltRangeToFindAxis', randoaxis)
        ierr = PipGetFloat('MinTiltRangeToFindAngles', randotilt)
        ierr = PipGetInteger('MaxViewsInAlign', nvLocalIn)
        ierr = PipGetInteger('RestrictViewsOnRound', localViewPass)
        ierr = PipGetInteger('LocalAreaTargetSize', localTarget)
        ierr = PipGetInteger('LocalAreaTracking', ifLocalArea)
        ierr = PipGetInteger('MinBeadsInArea', minInArea)
        if (ifLocalArea .ne. 0)
     &      ierr = PipGetInteger('MaxBeadsInArea', limInArea)
c        limInArea = min(limInArea, maxreal)
        ierr = PipGetInteger('MinOverlapBeads', minBeadOverlap)
        ierr = PipGetInteger('RoundsOfTracking', nround)
        ierr = PipGetInteger('MaxBeadsToAverage', maxsum)
        ierr = PipGetTwoIntegers('PointsToFitMaxAndMin', nfit, minfit)
        ierr = PipGetTwoFloats('DensityRescueFractionAndSD', fraccrit,sdcrit)
        ierr = PipGetFloat('DistanceRescueCriterion', distcrit)
        ierr = PipGetTwoFloats('RescueRelaxationDensityAndDistance',
     &      relaxint,relaxdis)
        ierr = PipGetFloat('PostFitRescueResidual',fitdistcrit )
        ierr = PipGetFloat('DensityRelaxationPostFit', relaxfit)
        ierr = PipGetFloat('MaxRescueDistance', radmaxfit)
        ierr = PipGetTwoIntegers('ResidualsToAnalyzeMaxAndMin',
     &      maxresid,minresid)
        ierr = PipGetTwoFloats('DeletionCriterionMinAndSD', resdifmin,
     &      resdifcrit)
        ierr = PipGetLogical('TrackObjectsTogether', ignoreObjs)
        ierr = PipGetInteger('SobelBeadsToAverage', maxSobelSum)
        ierr = PipGetFloat('KernelSigma', sobelSigma)
        if (maxSobelSum .gt. 0) ierr = PipGetLogical('HighestSobel', highestSobel)
        ierr = PipGetInteger('InterpolationType', interpType)
        if (PipGetString('SnapshotViews', listString) .eq. 0) call parselist2
     &      (listString, ivSnapList, nSnapList, maxview)
        ierr = PipGetTwoIntegers('SaveAllPointsAreaRound', iAreaSave,
     &      iPassSave)
      else
        write(*,'(1x,a,$)')'Minimum # of views required for tilt'//
     &      ' aligning: '
        read(5,*)minvtiltali
c         
        write(*,'(1x,a,$)')'Radius for centroid calculation, 0 for '//
     &      'dark or 1 for light beads: '
        read(5,*)cgrad,ifwhite
c         
        write(*,'(1x,a,$)')'1 to fill in gaps, 0 not to: '
        read(5,*)iffillin
c         
        write(*,'(1x,a,$)')'Maximum gap to create: '
        read(5,*)maxgap
c         
        write(*,'(1x,a,/,a,$)')'Minimum ranges of tilt angles required'
     &      //' for finding tilt axis',' and for finding tilt angles: '
        read(5,*)randoaxis,randotilt
c         
        write(*,'(1x,a,i4,a,$)')'X and Y dimensions of box: '
        read(5,*)nxbox,nybox
c         
        write(*,'(1x,a,$)')
     &      'Maximum number of beads to average for correlation: '
        read(5,*)maxsum
c         
        write(*,'(1x,a,$)')'# of points to fit for extrapolation'
     &      //', minimum # required: '
        read(5,*)nfit,minfit
c         
        write(*,'(1x,a,$)')'Fraction of mean and # of SDs for'//
     &      ' density criteron for rescue: '
        read(5,*)fraccrit,sdcrit
        write(*,'(1x,a,$)')
     &      'Distance criterion for rescue (- for full report): '
        read(5,*)distcrit
c         
        write(*,'(1x,a,$)')'Fractions to relax criterion during '//
     &      'density and distance rescue: '
        read(5,*)relaxint,relaxdis
c         
        write(*,'(1x,a,$)')
     &      'Criterion distance for rescue after fitting: '
        read(5,*)fitdistcrit
c         
        write(*,'(1x,a,$)')'Relaxation of density criterion, maximum '//
     &      'rescue distance: '
        read(5,*)relaxfit,radmaxfit
        write(*,'(1x,a,$)')'Max and min # of residual changes to use '//
     &      'to get mean and SD: '
        read(5,*)maxresid,minresid
        write(*,'(1x,a,$)')'Minimum residual change & criterion # of'//
     &      ' SD from mean for deletion of pt: '
        read(5,*)resdifmin,resdifcrit
c         
      endif
      call PipDone()
c
      ipolar=-1
      if(ifwhite.ne.0)ipolar=1
c       
      nxpad=niceframe(nxbox+2*npad, 2, 19)
      nypad=niceframe(nybox+2*npad, 2, 19)
      npixbox=nxbox*nybox
      nxpdim=nxpad+2
      maxarr = nypad * nxpdim
c      if (npixbox .gt. maxbox**2 .or. nypad * nxpdim .gt. maxarr) call
c    &    errorexit('BOX SIZE TOO LARGE FOR ARRAYS - TRY BINNING INPUT DATA',0)
      if (sigma1 .ne. 0 .or. radius2 .ne. 0) call setctfwsr
     &    (sigma1,sigma2,radius1,radius2,ctf,nxpad,nypad,deltactf)
c
      iftrace=0
      if(distcrit.lt.0.)then
        iftrace=1
        distcrit=-distcrit
      endif
c       
      tiltmax = 0.
      do iv=1,nvuall
        rotorig(iv)=rotstart
        tiltorig(iv)=tltall(iv)
        gmagorig(iv)=1.
        dxysav(1,iv)=0.
        dxysav(2,iv)=0.
        tiltmax = max(tiltmax, abs(tltall(iv)))
      enddo
      mapdumdmag=0
      mapdmagstart=1
c       
c       figure out where to start: section with most points at minimum tilt
c       ivlist is the number of points present on each view
c       ivgap has a list of views with gaps for each object (if not filling in)
c       indgap is index to location in ivgap for each object
c       minendz is the minimum ending Z of all the objects
c       use iobjlistmp for ivgap
c       
      minendz=maxview
      indfree=1
      do i=1,maxview
        ivlist(i)=0
      enddo
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        indgap(iobj)=indfree
        if(ninobj.gt.0)then
          ibase=ibase_obj(iobj)
c           
c           first order them by z
c           
          do ipt=1,ninobj-1
            do jpt=ipt+1,ninobj
              iz=nint(p_coord(3,object(ipt+ibase)))+1
              jz=nint(p_coord(3,object(jpt+ibase)))+1
              if(iz.gt.jz)then
                itmp=object(ipt+ibase)
                object(ipt+ibase)=object(jpt+ibase)
                object(jpt+ibase)=itmp
              endif
            enddo
          enddo
          do ipt=1,ninobj
            iz=nint(p_coord(3,object(ipt+ibase)))+1
            ivlist(iz)=ivlist(iz)+1
          enddo
          minendz=min(minendz,iz)
          if(iffillin.eq.0)then
c             
c             find gaps and add to list
c             
            do ipt=1,ninobj-1
              iz=nint(p_coord(3,object(ipt+ibase)))+1
              iznext=nint(p_coord(3,object(ipt+1+ibase)))+1
              if(iznext.gt.iz+1)then
                do iv=iz+1,iznext-1
                  iobjlistmp(indfree)=iv
                  indfree=indfree+1
                  if(indfree.gt. maxolist)call errorexit(
     &                'TOO MANY GAPS IN EXISTING MODEL FOR ARRAYS', 0)
                enddo
              endif
            enddo
          endif
        endif
      enddo
      indgap(max_mod_obj+1)=indfree
c       
c       allocate ivgap and copy to it
      limgaps = indfree
      allocate(ivgap(limgaps), stat=ierr)
      call memoryError(ierr, 'ARRAY FOR GAPS')
      if (indfree .gt. 1) ivgap(1:indfree-1) = iobjlistmp(indfree-1)
c       
c       now find section with most points
c       
      maxnpt=0
      tiltmin=200.
      do i=1,nvuall
        if (ivlist(i).gt.maxnpt .or.
     &      (ivlist(i).eq.maxnpt .and. abs(tltall(i)).lt.tiltmin)) then
          maxnpt=ivlist(i)
          tiltmin=abs(tltall(i))
          imintilt=i
        endif
      enddo
c       
c       Get minimim and maximum views to do: including imintilt even if
c       it is excluded
      minViewDo = 0
      iv = 1
      do while(iv .le. nvuall .and. minViewDo .eq. 0)
        minViewDo = iv
        if (iv .ne. imintilt .and. itemOnList(iv, izexclude, nexclude))
     &      minViewDo = 0
        iv = iv + 1
      enddo
      maxViewDo = 0
      iv = nvuall
      do while(iv .ge. 1 .and. maxViewDo .eq. 0)
        maxViewDo = iv
        if (iv .ne. imintilt .and. itemOnList(iv, izexclude, nexclude))
     &      maxViewDo = 0
        iv = iv - 1
      enddo
      numViewDo = maxViewDo + 1 - minViewDo
c      print *,imintilt,minViewDo,maxViewDo
      limresid = numViewDo * maxAllReal
      allocate(resmean(limresid), stat=ierr)
      call memoryError(ierr, 'ARRAY FOR MEAN RESIDUALS')
c
c       
c       figure out an order for the points: from the center outwards
c       i.e., first find position from center at minimum tilt, save that in
c       xyzsav, and store the square of distance in seqdist
c       iobjseq is just a list of objects to do in original order
c       
      nobjdo=0
      if (max_mod_obj .gt. maxAllReal) call errorexit(
     &    'TOO MANY CONTOURS IN MODEL FOR ARRAYS', 0)
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.0)then
          nobjdo=nobjdo+1
          iobjseq(nobjdo)=iobj
          tiltmin=200.
          ibase=ibase_obj(iobj)
          do ipt=1,ninobj
            iz=nint(p_coord(3,object(ipt+ibase)))+1
            if(abs(tltall(iz)).lt.tiltmin)then
              tiltmin=abs(tltall(iz))
              xyzsav(1,iobj)=p_coord(1,object(ipt+ibase))-xcen
              xyzsav(2,iobj)=p_coord(2,object(ipt+ibase))-ycen
              seqdist(nobjdo)=xyzsav(1,iobj)**2+xyzsav(2,iobj)**2
              xxtmp(nobjdo) = xyzsav(1,iobj)
              yytmp(nobjdo) = xyzsav(2,iobj)
            endif
          enddo
          xyzsav(3,iobj)=0.
        endif
      enddo
      if (ifLocalArea .ne. 0 .and. minBeadOverlap .gt. 0) then
c           
c         determine an average density from the area of the convex bound
c         First get a random subset not to exceed the limiting number
        numBound = nobjdo
        if (nobjdo .gt. limcxBound) then
          numBound = 0
          iseed = 1234567
          ranFrac = float(limcxBound) / nobjdo
          do i = 1, nobjdo
            if (ran(iseed) .lt. ranFrac) then
              numBound = numBound + 1
              xxtmp(numBound) = xxtmp(i)
              yytmp(numBound) = yytmp(i)
           endif
          enddo
        endif
c          
        call convexbound(xxtmp,yytmp,numBound,0., 2. * cgrad, xvtmp,yvtmp,
     &      nvert, cvbxcen, cvbycen, maxAllReal)
        area = 0.
        do i = 1, nvert
          j = mod(i, nvert) + 1
          area = area + 0.5 * (yvtmp(j)+yvtmp(i)) * (xvtmp(j)-xvtmp(i))
        enddo
        density = nobjdo / abs(area)
c        print *,area,density,nobjdo
      endif        
c       
c       set up for one area in X and Y, then compute number of areas and
c       overlaps if locals
c       
      nAreaX = 1
      nAreaY = 1
      nxOverlap = 0
      nyOverlap = 0
      done = .false.
      do while (.not.done)
        done = .true.
        if (ifLocalArea .ne. 0) then
          areaObjStr = 'area'
          noverlap = 0
c           
c           set target overlap so that 1.5 overlap areas at this density
c           will give minimum number of overlap beads
c           6/21/08: but constrain it to be smaller than the target itself
          if (minBeadOverlap .gt. 0) then
            noverlap = min(minBeadOverlap / (density * localTarget),
     &          0.8 * localTarget)
c             print *,area,density,noverlap
          endif
c           
c           get number of areas, round up so areas will start below target
c           Then compute or set overlaps so local sizes can be computed
c           
          nareax = (nxtotpix + localTarget - 2 * noverlap - 2) /
     &        (localTarget - noverlap)
          nareay = (nytotpix + localTarget - 2 * noverlap - 2) /
     &        (localTarget - noverlap)
          if (nareax .eq. 1) then
            if (nAreaY .gt. 1) nyoverlap = noverlap
          else if (nAreaY .eq. 1) then
            nxoverlap = noverlap
          else
            
            nxLocal = max(nxtotpix / nAreaX, nytotpix / nAreaY) + 1
            do while ((nxLocal * nAreaX - nxtotpix) / (nAreaX - 1) +
     &          (nxLocal * nAreaY - nytotpix) / (nAreaY - 1) .lt.
     &          2 * noverlap)
              nxLocal = nxLocal + 1
            enddo
            nyLocal = nxLocal
            nxOverlap = (nxLocal * nAreaX - nxtotpix) / (nAreaX - 1)
            nyOverlap = (nyLocal * nAreaY - nytotpix) / (nAreaY - 1)
          endif
        endif
c         
c         Get area sizes regardless; compute area distances from center
c         
        nxLocal = (nxtotpix + nxoverlap * (nareax - 1)) / nAreaX + 1
        nyLocal = (nytotpix + nyoverlap * (nareay - 1)) / nAreaY + 1
        nareaTot = nareax*nareay
        if (ifLocalArea .eq. 0 .and. getImodObjsize() .gt. 0 .and. .not. ignoreObjs) 
     &      nareaTot = getImodObjsize()
        maxarea = nareaTot + 10;
c         
c         Get arrays for areas
        allocate(iareaseq(maxarea),ninobjlist(maxarea),indobjlist(maxarea),
     &      areadist(maxarea), ivseqst(4*maxarea),ivseqnd(4*maxarea),listseq(4*maxarea),
     &      stat=ierr)
        call memoryError(ierr, 'ARRAYS FOR AREA DATA')

        do i=1,nareaTot
          if (ifLocalArea .eq. 0 .and. getImodObjsize() .gt. 0 .and. .not. ignoreObjs)then
c         
c             If there are no local areas but there are multiple objects, set up
c             one area per object.  No point getting distance, they are independent
            iareaseq(k) = k
            areadist(k) = 0.
          else
            ix=mod(i-1,nareax)
            iy=(i-1)/nareax
            iareaseq(i)=i
            areadist(i)=(ix*(nxlocal-nxoverlap)+nxlocal/2-xcen)**2+
     &          (iy*(nylocal-nyoverlap)+nylocal/2-ycen)**2
          endif
        enddo
c         
c         order areas by distance: iareaseq has sequence of area numbers
        do i=1,nareaTot-1
          do j=i+1,nareaTot
            if(areadist(iareaseq(i)).gt.areadist(iareaseq(j)))then
              itmp=iareaseq(i)
              iareaseq(i)=iareaseq(j)
              iareaseq(j)=itmp
            endif
          enddo
        enddo
c         
c         go through each area finding points within it
c         iobjlists is the list of objects to do for each area
c         indobjlist is the starting index in that list for each area
c         ninobjlist is the number of objects in the list for each area
c         
        nobjlists=0
        indfree=1
        maxInArea = 0
        do j = 1, max_mod_obj
          inAnArea(j) = .false.
        enddo
        do k=1,nareaTot
          if (ifLocalArea .ne. 0 .or. nareaTot .eq. 1) then
c             
c             get starting coordinates and set up to loop until conditions met
c             
            ix=mod(iareaseq(k)-1,nareax)
            iy=(iareaseq(k)-1)/nareax
            xst=ix*(nxlocal-nxoverlap)-xcen
            xnd=xst+nxlocal
            yst=iy*(nylocal-nyoverlap)-ycen
            ynd=yst+nylocal
            indstart=indfree
            keepGoing = .true.
            do while(keepGoing)
              indfree = indstart
              numNew = 0
c               
c               Look for objects in the area, count up the new ones
c               
              do j=1,nobjdo
                iobj=iobjseq(j)
                if(xyzsav(1,iobj).ge.xst.and.xyzsav(1,iobj).le.xnd.and.
     &              xyzsav(2,iobj).ge.yst.and.xyzsav(2,iobj).le.ynd)then
                  iobjlistmp(indfree)=iobj
                  indfree=indfree+1
                  if(indfree.gt.maxolist)call errorexit(
     &                'WAY TOO MANY LOCAL AREAS; EACH POINT IS IN MANY AREAS', 0)
                  if (.not. inAnArea(iobj)) numNew = numNew + 1
                endif
              enddo
c               
c               make area bigger if there are any new points at all in it and
c               it does not already have all the points, and
c               either the total in it is too low or this is an area after the
c               first and the old ones are too low for overlap
c               
              keepGoing = numNew .gt. 0 .and. indfree - indstart .lt. nobjdo
     &            .and. (indfree - indstart .lt. minInArea .or. (k .gt. 1 .and.
     &            indfree - indstart - numNew .lt. minBeadOverlap))
              if (keepGoing) then
                j = max(1, localTarget / 200)
                xst = xst - j
                xnd = xnd + j
                yst = yst -j
                ynd = ynd + j
              endif
            enddo
          else
c             
c             areas from objects: make list of objects in it
c             
            numNew = 0
            indstart = indfree
            do j=1,nobjdo
              iobj=iobjseq(j)
              call objtocont(iobj, obj_color, ix, iy)
              if (ix .eq. k) then
                numNew = numNew + 1
                iobjlistmp(indfree)=iobj
                indfree=indfree+1
              endif
            enddo
          endif
c           
          if (numNew .gt. 0) then
c             
c             if the area has new points, order the list by distance from
c             center
c             (Should that be center of area?  It is center of whole field)
c             
            nobjlists=nobjlists+1
            indobjlist(nobjlists)=indstart
            ninobjlist(nobjlists)=indfree-indstart
            do i = indstart,indfree-1
              inAnArea(iobjlistmp(i)) = .true.
            enddo
            maxInArea = max(maxInArea, ninobjlist(nobjlists))
            do i=indstart,indfree-2
              do j=i+1,indfree-1
                if(seqdist(iobjlistmp(i)).gt.seqdist(iobjlistmp(j)))then
                  itmp=iobjlistmp(i)
                  iobjlistmp(i)=iobjlistmp(j)
                  iobjlistmp(j)=itmp
                endif
              enddo
            enddo
            xxtmp(2*nobjlists-1) = xst+xcen
            xxtmp(2*nobjlists) = xnd+xcen
            yytmp(3*nobjlists-1) = yst+ycen
            yytmp(3*nobjlists) = ynd+ycen
            yytmp(3*nobjlists-2) = numNew
          else
            indfree = indstart
          endif
        enddo
        if (ifLocalArea .ne. 0 .and. maxInArea .gt. limInArea .and.
     &      localTarget .gt. 100) then
          localTarget = 0.98 * localTarget
          done = .false.
        endif
      enddo

      if (ifLocalArea .ne. 0) then
        if (maxInArea .gt. limInArea) call errorexit(
     &      'THE NUMBER OF POINTS IN SOME LOCAL AREAS IS ABOVE THE LIMIT', 0)
        print *, 'Local area number, size, overlap - X:', nAreaX, nxLocal,
     &      nxOverlap,',  Y:', nAreaY, nyLocal, nyOverlap
        do i = 1, nobjlists
          write(*,119)i, nint(xxtmp(2*i-1)), nint(xxtmp(2*i)),
     &        nint(yytmp(3*i-1)),
     &        nint(yytmp(3*i)), ninobjlist(i), nint(yytmp(3*i-2))
119       format('Area',i3,', X:',i6,' to',i6,', Y:',i6,' to',i6,',',
     &        i4,' points,',i4,' new')
        enddo
c      elseif (maxInArea.gt.maxreal) then
c        call errorexit( 'TOO MANY POINTS FOR ARRAYS - TRY LOCAL TRACKING',0)
      endif
c       
c       Maximum Number of points finally known for tiltalign solutions: allocate
      call allocateAlivar(maxInArea * nvuall, nvuall, maxInArea, ierr)
      call memoryError(ierr, 'ARRAYS IN ALIVAR')
      call allocateFunctVars(ierr)
      call memoryError(ierr, 'ARRAYS FOR FUNCT')
      comp=1.
      mapcomp=0
      skew=0.
      mapskew=0
      dmag=0.
      mapdmag=0
      alf=0.
      mapalf=0
c       
      maxAnySum = max(maxSobelSum, maxsum)

c       Get final arrays for the object lists and free temporary stuff
      maxolist = indfree
      allocate(listsBotTop(maxolist), residLists(maxolist), iobjlists(maxolist),
     &   incore(maxAnySum,maxInArea), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR OBJECT LISTS')
      iobjlists(1:indfree-1) = iobjlistmp(1:indfree-1)
      deallocate(iobjlistmp, xxtmp, yytmp, xvtmp, yvtmp, seqdist)
c
c       Allocate boxes and other image arrays
      allocate(boxes(npixbox, maxAnySum, maxInArea), corrSum(npixbox, maxInArea),
     &    cursum(npixbox), boxtmp(maxarr),
     &    ARRAY(maxarr), BRRAY(maxarr), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR BOXES OF IMAGE DATA')
c       
c       Allocate arrays for all real points in current solution
      i = maxInArea + 10
      allocate(xseek(i),yseek(i),wcrit(i),xr(msizexr,i), nws(i),iffound(i), ipnearest(i),
     &    ipnearsav(i), iobjdel(i),idrop(i),igrpBotTop(i), wsave(maxview, i),
     &    iobjali(i), numInSobelSum(i), inCorrSum(maxAnySum,i), inSobelSum(maxAnySum,i),
     &    sobelXpeaks(maxpeaks,i), sobelYpeaks(maxpeaks,i), sobelPeaks(maxpeaks,i), 
     &    sobelWsums(maxpeaks,i), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR POINTS IN AREA')
      xr(3,1:i) = 0.
      sobelXpeaks = 0.
      sobelYpeaks = 0.
c       
c       Get size and offset of sobel filtered 
      if (maxSobelSum .gt. 0) then
        scaleFacSobel = diameter / targetSobel
        print *,scaleFacSobel, diameter, targetSobel, scaleByInterp
        ierr = scaledSobel(boxes, nxbox, nybox, scaleFacSobel, scaleByInterp,
     &      interpType, -1., boxes, nxSobel, nySobel, xOffSobel, yOffSobel)
        nxspad=niceframe(nxSobel+2*npad, 2, 19)
        nyspad=niceframe(nySobel+2*npad, 2, 19)
        maxarr = (nxspad + 2) * nyspad
        i = nxSobel * nySobel
        ix = 0
        allocate(sarray(maxarr),sbrray(maxarr), refSobel(i),  boxSobel(i),
     &    sobelSum(npixbox, maxInArea),  stat=ierr)
        if (sobelSigma .gt. 0) then
          allocate(tmpSobel(npixbox), stat=ix)
          call scaledGaussianKernel(matKernel, kernelDim, 7, sobelSigma)
        endif
        call memoryError(ierr+ix, 'ARRAYS FOR SOBEL FILTERING')
        
      endif
        
c       
c       set up sequencing for object lists and views - odd passes from
c       middle outward, even passes from ends inward
c       
      nobjtot=nobjdo
      nseqs=0
      lastseq=0
      do ipass=1,nround
        if(mod(ipass,2).eq.1)then
          do i=1,nobjlists
            nseqs=nseqs+1
            ivseqst(nseqs)=imintilt-1
            ivseqnd(nseqs)=minViewDo
            listseq(nseqs)=i
            nseqs=nseqs+1
            ivseqst(nseqs)=imintilt
            ivseqnd(nseqs)=maxViewDo
            listseq(nseqs)=i
          enddo
        else
          do i=1,nobjlists
            nseqs=nseqs+1
            ivseqnd(nseqs)=imintilt-1
            ivseqst(nseqs)=minViewDo
            listseq(nseqs)=i
            nseqs=nseqs+1
            ivseqnd(nseqs)=imintilt
            ivseqst(nseqs)=maxViewDo
            listseq(nseqs)=i
          enddo
        endif
      enddo
c       
c       get list of inside and edge pixels for centroid
c       
      liminside = 3.5 * cgrad**2 + 20.
      limedge = 3.5 * ((cgrad + 1.5)**2 - cgrad**2) + 20.
      allocate(idxin(liminside),idyin(liminside),idxedge(limedge),idyedge(limedge),
     &    stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR COMPUTING CENTROID')

      ninside=0
      nedge=0
      limcg=cgrad+4
      do iy=-limcg,limcg
        do ix=-limcg,limcg
          radpix=sqrt((ix-0.5)**2+(iy-0.5)**2)
          if(radpix.le.cgrad)then
            ninside=ninside+1
            idxin(ninside)=ix
            idyin(ninside)=iy
          elseif(radpix.le.cgrad+1.5)then
            nedge=nedge+1
            idxedge(nedge)=ix
            idyedge(nedge)=iy
          endif
          if (ninside.gt.liminside.or.nedge.gt.limedge) call errorexit(
     &        'PROGRAMMER ERROR COMPUTING SIZE OF CENTROID ARRAYS',0)
        enddo
      enddo
c       
      nzout=0
      if(filout.ne.' ')then
        CALL IMOPEN(2,concat(filout,'.box'),'NEW')
        CALL IMOPEN(3,concat(filout,'.ref'),'NEW')
        CALL IMOPEN(4,concat(filout,'.cor'),'NEW')
        do i= 2,4
          call itrhdr(i, 1)
          call ialmod(i, modebox)
          call setsiz_sam_cel(i, nxbox, nybox, 1)
        enddo
        call setsiz_sam_cel(4, nxpad, nypad, 1)
C         
        call time(tim)
        call b3ddate(dat)
        write(titlech,301) dat,tim
301     FORMAT('Boxes',32x,a9,2x,a8)
        read(titlech,'(20a4)')(title(kti),kti=1,20)
        boxsum=0.
        boxmin = 1.e20
        boxmax = -1.e20
        refsum=0.
        refmin = 1.e20
        refmax = -1.e20
        corsum=0.
        cormin = 1.e20
        cormax = -1.e20
        CALL DOPEN(2,concat(filout,'.brpl'),'NEW','F')
        CALL DOPEN(3,concat(filout,'.cpl'),'NEW','F')
      endif
c       
c       Set up formats
      if (nobjtot .lt. 1000) then
        addfmt = '(i4,'' pts added on pass 2, conts:'',(11i4))'
        delfmt1 = '(i4,'' pts deleted for pass 2, conts:'' ,(11i4))'
        delfmt2 = '(i4,'' pts deleted, big mean residual, conts:'',(9i4))'
        objfmt = '(i3,19i4)'
      else
        addfmt = '(i4,'' pts added on pass 2, conts:'',(9i5))'
        delfmt1 = '(i4,'' pts deleted for pass 2, conts:'' ,(8i5))'
        delfmt2 = '(i4,'' pts deleted, big mean residual, conts:'',(7i5))'
        objfmt = '(i4,15i5)'
      endif
c       
      do i = 1, indfree
        listsBotTop(i) = 0
      enddo
c       
c       Start looping on the sequences of views
c       
      lastlist=-1
      maxObjOrig = max_mod_obj
      do iseq=1,nseqs
        nadded=1
        nvLocal = 0
        ifAlignDone = 0
        iseqPass = ((iseq + 1) / 2 - 1) / nobjlists + 1
        if (iseqPass .ge. localViewPass)
     &      nvLocal = nvLocalIn
        saveAllPoints = iareaSave .eq. listseq(iseq) .and.
     &      ipassSave .eq. iseqPass
        if(listseq(iseq).ne.lastseq)then
c           
c           initialize if doing a new set of points
c           
          initxyzdone=0
          ivseq=1
          ibaseRes = 0
          lastseq=listseq(iseq)
          nobjdo=ninobjlist(lastseq)
          do i=1,nobjdo
            iobjseq(i)=iobjlists(i+indobjlist(lastseq)-1)
          enddo
c           
c           Initialize arrays for boxes and residuals
          incore(1:maxAnySum, 1:nobjdo) = -1
          numInSobelSum(1:nobjdo) = 0
          inCorrSum(1:maxAnySum, 1:nobjdo) = .false.
          inSobelSum(1:maxAnySum, 1:nobjdo) = .false.
          corrSum(1:npixbox, 1:nobjdo) = 0.
          if (maxSobelSum .gt. 0) sobelSum(1:npixbox, 1:nobjdo) = 0.
          wsave(1:nvuall, 1:nobjdo) = -1.
          resmean(1:maxObjOrig*numViewDo)=-1.
          write(*,123)areaObjStr,listseq(iseq),iseqPass,nobjdo
123       format('Starting ',a,i4,', round',i3,',',i4,' contours')
          if (nobjlists .gt. 1) write(*,objfmt) (iobjseq(i),i=1,nobjdo)
        endif
        if (saveAllPoints .and. mod(iseq,2) .eq. 1) then
          do j = 1, 10
            iobj = getImodObjsize() + j
            call putimodflag(iobj, 1)
            call putsymtype(iobj, 0)
            call putsymsize(iobj, 5)
            call putimodobjname(iobj, objnames(mod(j-1, 5) + 1))
            do i=1,nobjdo
              iobj = iobjseq(i) + j * maxObjOrig
              ibase_obj(iobj) = ibase_free
              npt_in_obj(iobj) = 0
              obj_color(1,iobj) = 1
              obj_color(2,iobj) = 256 - getImodObjsize() - j
              max_mod_obj = max(max_mod_obj, iobj)
            enddo
          enddo
        endif
c         
c         Set direction and make list of views to do
c         
        idir=sign(1,ivseqnd(iseq)-ivseqst(iseq))
        nvlist=0
        do iview=ivseqst(iseq),ivseqnd(iseq),idir
          ifexclude=0
          do iexcl=1,nexclude
            if(iview.eq.izexclude(iexcl))ifexclude=1
          enddo
          if(ifexclude.eq.0)then
            nvlist=nvlist+1
            ivlist(nvlist)=iview
          endif
        enddo
c         
c         loop on the views
c         
        do ivl=1,nvlist
          iview=ivlist(ivl)
          iznext=iview-1
          call countAndPreparePointsToDo()
          ipass=1
          do while(ipass.le.2.and.ntodo.ne.0)
c             
c             now try to do tiltalign if possible
            if(nadded.ne.0) call tiltali(ifdidalign, ifAlignDone, resmean,
     &          ibaseRes, ibaseOnAlign, iview)
            call getProjectedPositionsSetupFits()
c             
c             Loop through points, refining projections before search
            if(ipass.eq.1)npioneer=ndat
            nadded=0
            call findAllBeadsOnView()
c             
c             get a final fit and a new tiltalign, then find a maximum
c             error for each pt
            if(ipass.eq.2)write(*,addfmt)nadded,(iobjdel(i),i=1,nadded)
            call redoFitsEvaluateResiduals()
            ipass=ipass+1
            call flush(6)
          enddo
          ivseq=ivseq+1
          ibaseRes = ibaseRes + maxObjOrig
          call flush(6)
        enddo
c         
c         Report total missing at end of pass
c         
        if (mod(iseq, 2) .eq. 0) then
          misstot = 0
          do i = 1, nobjdo
            call countMissing(iobjseq(i), nvuall, izexclude,
     &          nexclude, missing, listz, nlistz)
            misstot=misstot+nlistz
          enddo
          write(*,121)areaObjStr,listseq(iseq), iseqPass, nobjdo,misstot
121       format('For ',a,i3,', round',i3,':',i3,
     &        ' contours, points missing =',i5)
        endif
c         
c         Do surface fitting on every round if tiltalign run
        if (surfaceFile .ne. ' ' .and. ifAlignDone .ne. 0) then
          if (surfaceSort(xyz,nrealpt,igrpBotTop) .ne. 0) call errorExit(
     &        'ALLOCATING MEMORY in surfaceSort', 0)
c           
c           Find each object in the real object list and move group number
c           into list
          do i = 1, nobjdo
            j = 1
            ix = 0
            xpos = 0.
            do while (j .le. nrealpt .and. ix .eq. 0)
              if (iobjali(j) .eq. iobjseq(i)) then
                ix = igrpBotTop(j)
                xpos = resmean(iobjali(j) + ibaseOnAlign)
              endif
              j = j + 1
            enddo
            listsBotTop(i+indobjlist(lastseq)-1) = ix
            residLists(i+indobjlist(lastseq)-1) = xpos
          enddo
        endif
      enddo
c       
c       output lists of missing points, except for excluded sections
c       
      write(*,118)
118   format(/,'Obj cont:  Views on which points are missing')
      misstot=0
      do iobj=1,maxObjOrig

        call countMissing(iobj, nvuall, izexclude, nexclude, missing,
     &      listz, nlistz)
        if(nlistz.ne.0)then
          call objtocont(iobj,obj_color,imodobj,imodcont)
          write(*,117)imodobj,imodcont
117       format(i2,i4,': ',$)
          call wrlist(listz,nlistz)
        endif
        misstot=misstot+nlistz
      enddo
      print *,'Total points missing =',misstot
c       
c       Write out surface file
      if (surfaceFile .ne. ' ') then
        call dopen(4, surfaceFile, 'new', 'f')
        do iobj=1,maxObjOrig
          numBotTop(1,iobj) = 0
          numBotTop(2,iobj) = 0
          resmean(iobj) = 0.
        enddo
        do i = 1, indfree
          iobj = iobjLists(i)
          j = listsBotTop(i)
          if (j .gt. 0) then
            numBotTop(j,iobj) = numBotTop(j,iobj) + 1
            resmean(iobj) = resmean(iobj) + residLists(i)
          endif
        enddo
        do iobj=1,maxObjOrig
          call objtocont(iobj,obj_color,imodobj,imodcont)
          xpos = 0.
          ix = numBotTop(1,iobj) + numBotTop(2,iobj)
          if (ix .gt. 0) xpos = resmean(iobj) / ix
          write(4, '(i3,3i6,f8.3)')imodobj,imodcont,(numBotTop(j,iobj),j=1,2),
     &        xpos
        enddo
      endif
c       
c       convert index coordinates back to model coordinates
c       
      call scaleModelToImage(1, 1)
      call write_wmod(modelfile)
      if(filout.ne.' ')then
        call setsiz_sam_cel(2,nxbox,nybox,nzout)
        call setsiz_sam_cel(3,nxbox,nybox,nzout)
        call setsiz_sam_cel(4,nxpad,nypad,nzout)
        CALL IWRHDR(2,TITLE,1,BOXMIN,BOXMAX,boxsum/nzout)
        CALL IWRHDR(3,TITLE,1,REFMIN,REFMAX,refsum/nzout)
        CALL IWRHDR(4,TITLE,1,CORMIN,CORMAX,corsum/nzout)
        CALL IMCLOSE(2)
        CALL IMCLOSE(3)
        CALL IMCLOSE(4)
        close(2)
        close(3)
      endif
      call exit(0)


      CONTAINS

c       countAndPreparePointsToDo evaluates each potential point on a new view to see
c       if it already done, a gap to be skipped, or has no near enough neighbors
c       Then it loads necessary boxes, forms averages and evaluates wsum if needed
c
      subroutine countAndPreparePointsToDo()
      integer*4 numberInList
c       
c       first see how many are done on this view
c       
      ntodo=0
      do iobjdo=1,nobjdo
        iobj=iobjseq(iobjdo)
        ninobj=npt_in_obj(iobj)
        nclose=0
        nfarther=0
        iffound(iobjdo)=0
        ibase=ibase_obj(iobj)
c         
c         find closest ones within gap distance, find out if already
c         done, mark as a 2 to protect them
c         ipnearest holds the point number of the nearest for each object
c         ipclose is the list of points within gap distance for this object
c         
        neardiff=10000
        do ip=1,ninobj
          ipt=object(ibase+ip)
          izv=nint(p_coord(3,ipt))+1
          if(abs(izv-iview).lt.neardiff)then
            neardiff=abs(izv-iview)
            ipnearest(iobjdo)=ip
          endif
          if(izv.eq.iview)then
            iffound(iobjdo)=2
          elseif(abs(izv-iview).le. max(maxgap, 2 * maxSobelSum))then
            xbox=p_coord(1,ipt)
            ybox=p_coord(2,ipt)
            izbox=nint(p_coord(3,ipt))
            call find_piece(ixpclist,iypclist,izpclist,npclist, nx, ny, nxbox,nybox,
     &          xbox,ybox,izbox,ix0,ix1, iy0,iy1,ipcz)
            if(ipcz.ge.0)then
              if (abs(izv-iview).le. maxgap) nclose=nclose+1
              nfarther=nfarther+1
              ipclose(nfarther)=ip
              izclose(nfarther)=izv
            endif
          endif
        enddo
c         
c         if not found and none close, mark as -1 not to do
c         If there are close ones, make sure this is not a gap to preserve
c         
        if(nclose.eq.0.and.iffound(Iobjdo).eq.0) iffound(iobjdo)=-1
        if(nclose.gt.0.and.iffound(iobjdo).eq.0)then
          if(iffillin.eq.0.and.indgap(iobj+1).gt.indgap(iobj))then
c             
c             if not filling in, look for gap on list
c             
            do igap=indgap(iobj),indgap(iobj+1)-1
              if(ivgap(igap).eq.iview)iffound(iobjdo)=-1
            enddo
          endif
          if(iffound(iobjdo).eq.0)then
c             
c             order feasible points by distance
c             
            ntodo=ntodo+1
            do ic=1,nfarther-1
              do jc=ic+1,nfarther
                if(abs(izclose(jc)-iview).lt. abs(izclose(ic)-iview)) then
                  itmp=izclose(ic)
                  izclose(ic)=izclose(jc)
                  izclose(jc)=itmp
                  itmp=ipclose(ic)
                  ipclose(ic)=ipclose(jc)
                  ipclose(jc)=itmp
                endif
              enddo
            enddo
            maxuse=min(nfarther,maxAnySum)
c             
c             see if boxes in memory match, free them if not
c             
            do ibox=1,maxAnySum
              if (incore(ibox,iobjdo).ge.0) then
c                 
c                 First subtract from correlation sum
                if (inCorrSum(ibox,iobjdo)) then
                  if (numberInList(incore(ibox,iobjdo), izclose, maxsum, 0) .eq. 0) then
                    corrSum(1:npixbox, iobjdo) = corrSum(1:npixbox, iobjdo) -
     &                  boxes(1:npixbox, ibox, iobjdo)
                    inCorrSum(ibox,iobjdo) = .false.
                  endif
                endif
c                 
c                 Then subtract from sobel sum
                if (inSobelSum(ibox,iobjdo)) then
                  if (numberInList(incore(ibox,iobjdo),izclose,maxSobelSum, 0) .eq. 0)then
                    sobelSum(1:npixbox, iobjdo) = sobelSum(1:npixbox, iobjdo) -
     &                  boxes(1:npixbox, ibox, iobjdo)
                    inSobelSum(ibox,iobjdo) = .false.
                    numInSobelSum(iobjdo) = numInSobelSum(iobjdo) - 1
                  endif
                endif
                if (numberInList(incore(ibox,iobjdo), izclose, maxuse, 0) .eq. 0)
     &              incore(ibox,iobjdo)=-1
              endif
            enddo
c             
c             now load ones that are needed into last free box
c             
            do ic=1,maxuse
              if (numberInList(izclose(ic), incore(1,iobjdo), maxAnySum, 0) .eq. 0) then
                do ibox=1,maxAnySum
                  if(incore(ibox,iobjdo).lt.0)ib=ibox
                enddo
                incore(ib,iobjdo)=izclose(ic)
                ipt=abs(object(ibase+ipclose(ic)))
                xbox=p_coord(1,ipt)
                ybox=p_coord(2,ipt)
                izbox=nint(p_coord(3,ipt))
                call find_piece(ixpclist,iypclist,izpclist, npclist, nx, ny, nxbox,
     &              nybox,xbox,ybox, izbox,ix0,ix1, iy0,iy1,ipcz)
                call imposn(1,ipcz,0)
                call irdpas(1,boxtmp,nxbox,nybox, ix0,ix1,iy0, iy1,*299)
c                 
c                 do subpixel shift and calculate CG and wsum value
c                 if it is not already saved
c                 
                xt=nint(xbox)-xbox
                yt=nint(ybox)-ybox
                call qdshift(boxtmp,boxes(1, ib, iobjdo) ,nxbox, nybox,xt,yt)
                if(wsave(izbox+1, iobjdo).lt.0.)then
                  xtmp=0.
                  ytmp=0.
                  call calccg(boxes(1, ib, iobjdo),nxbox, nybox,xtmp, ytmp,  wsum)
                  wsave(izbox+1, iobjdo) = wsum
                endif
c                 
c                 Add to correlation sum
                if (ic .le. maxsum) then
                  inCorrSum(ib, iobjdo) = .true.
                  corrSum(1:npixbox, iobjdo) = corrSum(1:npixbox, iobjdo) +
     &                boxes(1:npixbox, ib, iobjdo)
                endif
c                 
c                 Add to sobel sum
                if (ic .le. maxSobelSum) then
                  inSobelSum(ib, iobjdo) = .true.
                  numInSobelSum(iobjdo) = numInSobelSum(iobjdo) + 1
                  sobelSum(1:npixbox, iobjdo) = sobelSum(1:npixbox, iobjdo) +
     &                boxes(1:npixbox, ib, iobjdo)
                endif
              endif
            enddo
          endif
        endif
        ipnearsav(iobjdo)=ipnearest(iobjdo)
      enddo
      return
299   call errorexit('ERROR READING IMAGE FILE',0)
      end subroutine countAndPreparePointsToDo


c       getProjectedPositionsSetupFits
c
      subroutine getProjectedPositionsSetupFits()
      real*4 cosd,sind
c       
c       get tentative tilt, rotation, mag for current view
c       
      if(ifdidalign.gt.0)then
        ivuse=iview
        if(mapFileToView(iview).eq.0) then
          ivdel=200
          do iv=1,nview
            if(abs(mapViewToFile(iv)-iview).lt.ivdel)then
              ivdel=abs(mapViewToFile(iv)-iview)
              ivuse=mapViewToFile(iv)
            endif
          enddo
        endif
c         print *,'ivuse, iview', ivuse, iview
        tiltcur=tiltorig(ivuse)+tltall(iview)-tltall(ivuse)
        gmagcur=gmagorig(ivuse)
        rotcur=rotorig(ivuse)
        cosr=cosd(rotcur)
        sinr=sind(rotcur)
        cost=cosd(tiltcur)
        sint=sind(tiltcur)
        dxcur = dxysav(1,ivuse)
        dycur = dxysav(2,ivuse)
        if (ivuse .ne. iview .and. tiltmax .le. 80.) then
c           
c           if going from another view and cosine stretch is appropriate,
c           back-rotate the dxy so tilt axis vertical, adjust the 
c           dx by the difference in tilt angle, and rotate back
          a = cosr * dxcur + sinr * dycur
          b = -sinr * dxcur + cosr * dycur
          a = a * cost / cosd(tiltorig(ivuse))
          dxcur = cosr * a - sinr * b
          dycur = sinr * a + cosr * b
        endif
        a = gmagcur * cost * cosr
        b = - gmagcur * sinr
        c = gmagcur * sint * cosr
        d = gmagcur * cost * sinr
        e = gmagcur * cosr
        f = gmagcur * sint * sinr
      endif
c       
c       now get projected positions
c       
      do iobjdo=1,nobjdo
        indr=0
        if(ifdidalign.eq.1)then
          do ipt=1,nrealpt
            if(iobjseq(iobjdo).eq.iobjali(ipt)) indr=iobjseq(iobjdo)
          enddo
        endif
        if(indr.ne.0)then
c           
c           there is a 3D point for it: so project it
c           
          xseek(iobjdo)=a*xyzsav(1,indr)+b*xyzsav(2,indr)+ c*xyzsav(3,indr)+dxcur+xcen
          yseek(iobjdo)=d*xyzsav(1,indr)+e*xyzsav(2,indr)+ f*xyzsav(3,indr)+dycur+ycen
        else
          call nextpos(iobjseq(iobjdo),ipnearest(iobjdo),idir, iznext,tltall,nfit,
     &        minfit,iaxtilt,tiltfitmin, xseek(iobjdo),yseek(iobjdo))
        endif
        if (saveAllPoints) then
          iobjSave = iobjseq(iobjdo) + (5 * ipass - 4) * maxObjOrig
          call add_point(iobjSave,0, xseek(iobjdo), yseek(iobjdo), iznext)
        endif
      enddo
c       
c       build list of points already found for fitting; put
c       actual positions in 4 and 5 for compatibiity with xfmodel
c       
      ndat=0
      do iobjdo=1,nobjdo
        if(iffound(iobjdo).gt.0)then
          ndat=ndat+1
          xr(1,ndat)=xseek(iobjdo)-xcen
          xr(2,ndat)=yseek(iobjdo)-ycen
          ipt=object(ibase_obj(iobjseq(iobjdo))+ ipnearest(iobjdo))
          xr(4,ndat)=p_coord(1,ipt)-xcen
          xr(5,ndat)=p_coord(2,ipt)-ycen
          xr(6,ndat)=iobjdo
        endif
      enddo
      return
      end subroutine getProjectedPositionsSetupFits
      

c       findAllBeadsOnView
c
      subroutine findAllBeadsOnView()
      do iobjdo=1,nobjdo
        if(iffound(iobjdo).eq.0.or.iffound(iobjdo).eq.1)then
c           
c           get w criterion: average it over as many near views as
c           possible.  maxwavg specifies both the maximum distance
c           away in Z and the maximum number to average
c           
          navg=0
          wsum=0.
          wsumsq=0.
          idif=1
          do while(idif.le.maxwavg.and.navg.lt.maxwavg)
            do idirw=-1,1,2
              itry=iview+idirw*idif
              if(itry.gt.0.and.itry.le.nvuall)
     &            then
                wstmp=wsave(itry, iobjdo)
                if(wstmp.ge.0)then
                  wsum=wsum+wstmp
                  wsumsq=wsumsq+wstmp**2
                  navg=navg+1
                endif
              endif
            enddo
            idif=idif+1
          enddo
          call sums_to_avgsd(wsum,wsumsq,navg,wavg,wsd)
          wcrit(iobjdo)=min(fraccrit*wavg,wavg-sdcrit*wsd)
          nws(iobjdo)=navg
        endif
c         
        if(iffound(iobjdo).eq.0)then
          iobj=iobjseq(iobjdo)
          xnext=xseek(iobjdo)
          ynext=yseek(iobjdo)
c           
c           if there are existing points, find right kind of transform
c           and modify position
c           
          if(npioneer.gt.0.or.ndat.ge.limpshft)then
            ifrotrans=0
            iftrans=1
            ndatFit = ndat
            if (ndat .lt. npioneer + limpshft) ndatFit = npioneer
            if(ndatFit.ge.limpstr)then
              iftrans=0
            elseif(ndatFit.ge.limpmag)then
              ifrotrans=2
            elseif(ndatFit.ge.limprot)then
              ifrotrans=1
            endif
            maxDrop = nint(0.26 * ndatFit)
            if (ndatFit .lt. 4 .or. ifdidalign.eq.0) maxDrop = 0
            call findxf_wo_outliers(xr,msizexr, ndatFit,xcen,ycen,iftrans, ifrotrans,
     &          maxDrop, outlieCrit,outlieCritAbs, outlieElimMin,idrop,nDrop,xf, devavg,
     &          devsd, devmax,ipntmax)
            call xfapply(xf,xcen,ycen,xseek(iobjdo), yseek(iobjdo), xnext,ynext)
            if (saveAllPoints) then
              iobjSave = iobj + (5 * ipass - 3) * maxObjOrig
              call add_point(iobjSave, 0, xnext, ynext, iznext)
            endif
          endif
c           
          call find_piece(ixpclist,iypclist,izpclist, npclist, nx, ny, nxbox,nybox,
     &        xnext,ynext, iznext,ix0,ix1, iy0,iy1,ipcz)
          if(ipcz.ge.0)then
            call lookForOneBead()
          endif
        endif
      enddo
      return
      end subroutine findAllBeadsOnView

c       lookForOneBead
c
      subroutine lookForOneBead()
      integer*4 scaledSobel
      integer*4 ibest
      real*4 distmin, peakmax
c       
c       get image area
c       
      call imposn(1,ipcz,0)
      call irdpas(1,boxtmp,nxbox,nybox,ix0,ix1,iy0,iy1,*199)
      if(ipass.eq.1)then

        if (.not. highestSobel) then
c         
c           pad image into array on first pass, padd correlation sum into brray
          call taperoutpad(boxtmp,nxbox,nybox,array,nxpdim, nxpad, nypad, 0, 0.)
          call setmeanzero(array,nxpdim,nypad,nxpad, nypad)
          call taperoutpad(corrSum(1,iobjdo),nxbox,nybox,brray,nxpdim, nxpad, nypad, 0,0.)
          call setmeanzero(brray,nxpdim,nypad,nxpad, nypad)
c         
c           correlate the two
c           
          call todfft(array,nxpad,nypad,0)
          call todfft(brray,nxpad,nypad,0)
          call conjugateProduct(array, brray, nxpad, nypad)
          if (deltactf .ne. 0) call filterpart(array,array,nxpad, nypad,ctf, deltactf)
          call todfft(array,nxpad,nypad,1)
c           
c           find peak of correlation
c           
          call peakfind(array,nxpdim,nypad,xpeak,ypeak, peak)
          if (saveAllPoints) then
            iobjSave = iobj + (5 * ipass - 2) * maxObjOrig
            call add_point(iobjSave, 0, nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
          endif
        endif
c           
c         Get sobel sum with enough beads in it
        if (maxSobelSum .gt. 0) then
          cursum(1:npixbox) = sobelSum(1:npixbox, iobjdo)
          ninSobel = numInSobelSum(iobjdo)
          do i = 1, nobjdo
            if (ninSobel .ge. maxSobelSum) exit
            if (i .ne. iobjdo) then
              cursum(1:npixbox) = cursum(1:npixbox) + sobelSum(1:npixbox, i)
              ninSobel = ninSobel + numInSobelSum(i)
            endif
          enddo
c           
c           sobel filter the sum
          ierr = scaledSobel(cursum, nxbox, nybox, scaleFacSobel, scaleByInterp,
     &        interpType, 2., refSobel, nxSobel, nySobel, xOffSobel, yOffSobel)
          if (ierr .ne. 0) call errorExit('DOING SOBEL FILTER ON SUMMED BEADS', 0)
c          write(*,'(i4,13i5)')(nint(refSobel(i)),i=1,14*14)
c           
c           Gaussian filter the box and sobel filter it
          if (sobelSigma .gt. 0) then
            call applyKernelFilter(boxtmp, tmpSobel, nxbox, nxbox, nybox, matKernel,
     &          kernelDim)
            ierr = scaledSobel(tmpSobel, nxbox, nybox, scaleFacSobel, scaleByInterp,
     &          interpType, 2., boxSobel, nxSobel, nySobel, xOffSobel, yOffSobel)
          else
            ierr = scaledSobel(boxtmp, nxbox, nybox, scaleFacSobel, scaleByInterp,
     &          interpType, 2., boxSobel, nxSobel, nySobel, xOffSobel, yOffSobel)
          endif
          if (ierr .ne. 0) call errorExit('DOING SOBEL FILTER ON SEARCH BOX', 0)
c           
c           Taper/pad etc
          call taperoutpad(boxSobel,nxSobel,nySobel,sarray,nxspad+2, nxspad,
     &        nyspad, 0, 0.)
          call setmeanzero(sarray,nxspad+2,nyspad,nxspad, nyspad)
          call taperoutpad(refSobel,nxSobel,nySobel,sbrray,nxspad+2, nxspad, nyspad, 0,0.)
          call setmeanzero(sbrray,nxspad+2,nyspad,nxspad, nyspad)
c           
c           correlate the two
          call todfft(sarray,nxspad,nyspad,0)
          call todfft(sbrray,nxspad,nyspad,0)
          call conjugateProduct(sarray, sbrray, nxspad, nyspad)
          call todfft(sarray,nxspad,nyspad,1)
c
c           get the peaks and adjust them for the sobel scaling
c           get the centroid offset of the average and use it to adjust the positions too
c           the original offsets from scaledsobel are irrelevant to correlation
          call xcorrPeakFind(sarray, nxspad+2, nyspad, sobelXpeaks(1, iobjdo),
     &        sobelYpeaks(1, iobjdo), sobelPeaks(1, iobjdo), maxPeaks)
          call calccg(cursum, nxbox, nybox, xOffSobel, yOffSobel, wsum)
c          write(*,'(2i5,a,2f7.2)')iobjdo,ninSobel,'  Sobel average offset',xOffSobel, yOffSobel
          sobelWsums(1:maxPeaks, iobjdo) = -1.
          sobelXpeaks(1:maxPeaks, iobjdo) = sobelXpeaks(1:maxPeaks, iobjdo) *
     &        scaleFacSobel + xOffSobel
          sobelYpeaks(1:maxPeaks, iobjdo) = sobelYpeaks(1:maxPeaks, iobjdo) *
     &        scaleFacSobel + yOffSobel
c          do i = 1, maxpeaks
c            if (sobelPeaks(i, iobjdo) .gt. -1.e29)
c     &          write(*,'(2f7.2,f13.2)')sobelXpeaks(i, iobjdo),sobelYpeaks(i, iobjdo),sobelPeaks(i, iobjdo)
c          enddo
c           
c           Find either the highest valid peak or the one closest to the regular xcorr one
c           that is still relatively strong
          ibest = 0
          peakmax = -1.e30
          distmin = 1.e20
          do i = 1, maxpeaks
            call checkSobelPeak(i, boxtmp, nxbox, nybox, sobelXpeaks(1, iobjdo),
     &          sobelYpeaks(1, iobjdo), sobelPeaks(1, iobjdo), sobelWsums(1, iobjdo), 
     &          maxpeaks)
c            if (sobelPeaks(i, iobjdo) .gt. -1.e29)
c     &          write(*,'(2f7.2,2f13.2)')sobelXpeaks(i, iobjdo),sobelYpeaks(i, iobjdo),sobelPeaks(i, iobjdo), sobelWsums(1, iobjdo)
            
            if (sobelWsums(i, iobjdo) .gt. 0) then
              if (highestSobel) then
                ibest = i
                exit
              endif
              if (peakmax .lt. -1.e29) peakmax = sobelPeaks(i, iobjdo)
              if (sobelPeaks(i, iobjdo) .lt. minPeakRatio * peakmax) exit
              dist = (sobelXpeaks(i, iobjdo) - xpeak)**2 +
     &            (sobelYpeaks(i, iobjdo) - ypeak)**2
              if (dist .lt. distmin) then
                distmin = dist
                ibest = i
              endif
            endif
          enddo
          if (ibest .gt. 0) then
            xpeak = sobelXpeaks(ibest, iobjdo)
            ypeak = sobelYpeaks(ibest, iobjdo)
            wsum = sobelWsums(ibest, iobjdo)
c            print *,ibest,xpeak,ypeak,wsum
          elseif (highestSobel) then
            wsum = 0.
            xpeak = 0.
            ypeak = 0.
          endif

        else
c       
c           If no sobel at all 
          call calccg(boxtmp,nxbox,nybox,xpeak,ypeak, wsum)
        endif
        dist=sqrt(xpeak**2+ypeak**2)
        if (saveAllPoints) then
          iobjSave = iobj + (5 * ipass - 1) * maxObjOrig
          call add_point(iobjSave, 0, nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
        endif
      endif
      if(nws(iobjdo).gt.2.and.(wsum.lt.wcrit(iobjdo)
     &    .or. dist.gt.distcrit .or. ipass.eq.2))then
c         
c         rescue attempt; search concentric rings from the
c         center of box and take the first point that goes
c         above the relaxed wsum criterion
c         
        if(ipass.eq.1)then
          if(dist.gt.distcrit)then
            relax=relaxdis
            if(iftrace.ne.0)
     &          write(*,102)'distance',iznext,dist,wsum,wavg,wsd
102         format(' Rescue-',a,', sec',i4,', dist=',f5.1,
     &          ', dens=',f9.0,', mean,sd=',f9.0,f7.0)
          else
            relax=relaxint
            if(iftrace.ne.0)
     &          write(*,102)'density ',iznext,dist,wsum,wavg,wsd
          endif
          radmax=max(nxbox,nybox)
        else
          relax=relaxfit
          radmax=radmaxfit
        endif
        if (maxSobelSum .gt. 0) then
          call rescueFromSobel(boxtmp,nxbox,nybox, xpeak, ypeak, sobelXpeaks(1, iobjdo),
     &        sobelYpeaks(1, iobjdo), sobelPeaks(1, iobjdo), sobelWsums(1, iobjdo),
     &        maxpeaks, radmax,relax*wcrit(iobjdo), wsum)
        else
          call rescue(boxtmp,nxbox,nybox,xpeak,ypeak, radmax,relax*wcrit(iobjdo), wsum)
        endif
      elseif(ipass.eq.2)then
        wsum=0.
      endif
c       
c       
c       
      if(wsum.ne.0.)then
        wsave(iview, iobjdo) = wsum
        xpos=nint(xnext)+xpeak
        ypos=nint(ynext)+ypeak
        if(iftrace.ne.0)
     &      write(*,'(3i5,4f7.1,2f12.0)')nzout,iznext,iobj,
     &      xnext, ynext, xpos,ypos,peak,wsum
c         
c         add point to model
c         
        call add_point(iobj,ipnearest(iobjdo), xpos,ypos,iznext)
        if (saveAllPoints) then
          iobjSave = iobj + (5 * ipass) * maxObjOrig
          call add_point(iobjSave, 0, xpos, ypos, iznext)
        endif
        nadded=nadded+1
        iobjdel(nadded)=iobj
c         
c         mark as found, add to data matrix for alignment
c         
        iffound(iobjdo)=1
        ndat=ndat+1
        xr(1,ndat)=xseek(iobjdo)-xcen
        xr(2,ndat)=yseek(iobjdo)-ycen
        xr(4,ndat)=xpos-xcen
        xr(5,ndat)=ypos-ycen
        xr(6,ndat)=iobjdo
c         
        if(filout.ne.' ')then
          if (modebox .eq. 2) then
            call iclden(boxtmp,nxbox,nybox,1, nxbox, 1, nybox,tmin,tmax,tmean)
          else
            call isetdn(boxtmp,nxbox,nybox,modebox,1, nxbox, 1, nybox,tmin,tmax,tmean)
          endif
          call iwrsec(2,boxtmp)
          boxmin=min(boxmin,tmin)
          boxmax=max(boxmax,tmax)
          boxsum=boxsum+tmean
          nzout=nzout+1
          boxtmp(1:npixbox) = corrSum(1:npixbox,iobjdo)
          if (modebox .eq. 2) then
            call iclden(boxtmp,nxbox,nybox,1, nxbox, 1, nybox,tmin,tmax,tmean)
          else
            call isetdn(boxtmp,nxbox,nybox,modebox,1, nxbox, 1, nybox,tmin,tmax,tmean)
          endif
          call iwrsec(3,boxtmp)
          refmin=min(refmin,tmin)
          refmax=max(refmax,tmax)
          refsum=refsum+tmean
          call splitpack(array,nxpdim,nxpad,nypad,boxtmp)
          if (modebox .eq. 2) then
            call iclden(boxtmp,nxpad,nypad,1, nxpad, 1, nypad,tmin,tmax,tmean)
          else
            call isetdn(boxtmp,nxpad,nypad,modebox,1, nxpad, 1, nypad,tmin,tmax,tmean)
          endif
          call iwrsec(4,boxtmp)
          cormin=min(cormin,tmin)
          cormax=max(cormax,tmax)
          corsum=corsum+tmean
          write(2,'(3i6)')nint(xnext)-nxbox/2, nint(ynext)-nybox/2,iznext
          write(3,'(3i6)')nint(xnext)-nxpad/2, nint(ynext)-nypad/2,iznext
        endif
      endif
      return
199   call errorexit('ERROR READING IMAGE FILE',0)
      end subroutine lookForOneBead

       
c       redoFitsEvaluateResiduals
c       
      subroutine redoFitsEvaluateResiduals()
      if(ndat.ge.limpshft)then
        ifrotrans=0
        iftrans=1
        if(ndat.ge.limpstr)then
          iftrans=0
        elseif(ndat.ge.limpmag)then
          ifrotrans=2
        elseif(ndat.ge.limprot)then
          ifrotrans=1
        endif
        maxDrop = nint(0.26 * ndat)
        if (ndat .lt. 4 .or. ifdidalign.eq.0) maxDrop = 0
        call findxf_wo_outliers(xr,msizexr,ndat,xcen,ycen,iftrans, ifrotrans,maxDrop,
     &      outlieCrit,outlieCritAbs, outlieElimMin,idrop,nDrop,xf, devavg,devsd,
     &      devmax,ipntmax)
        write(*,113)iview,ipass,ndat,ndrop,devavg,devsd,devmax
113     format('view',i4,', pass',i2,',',i4,
     &      ' points (-', i3,'), mean, sd, max dev:' ,3f8.2)
        if (ifdidalign .ne. 0) then
          dxcur = dxcur + xf(1,3)
          dycur = dycur + xf(2,3)
        endif
      endif
c       
c       do tiltali if anything changed, so can get current residuals
c       
      if(nadded.gt.0)then
        if (ifdidalign .ne. 0 .and. ivuse .ne. iview) then
          dxysav(1, iview) = dxcur
          dxysav(2, iview) = dycur
        endif
        call tiltali(ifdidalign, ifAlignDone, resmean, ibaseRes,
     &      ibaseOnAlign, iview)
      endif
      if (itemOnList(iview, ivSnapList, nSnapList)) then
        call int_iwrite(listString, iview, ndel)
        write(plfile, '(a,a,a,i1)')'.',listString(1:ndel),'.',ipass
        call scale_model(1)
        call write_wmod(concat(modelfile, plfile))
        call scale_model(0)
      endif
      nadtmp=nadded
      nadded=0
      ntodo=0
      ndel=0
      if(nadtmp.gt.0)then
        ntodo=0
        do iobjdo=1,nobjdo
          if(iffound(iobjdo).eq.0)ntodo=ntodo+1
          if(iffound(iobjdo).eq.1)then
            iobj=iobjseq(iobjdo)
            errmax=0.
c             if(ipass.eq.1)then
            indr=0
c             
c             find point in the tiltalign, get its residual
c             
            if(ifdidalign.eq.1)then
              do ipt=1,nrealpt
                if(iobj.eq.iobjali(ipt))indr=ipt
              enddo
            endif
            if(indr.ne.0)then
              ivlook=mapFileToView(iview)
              ipt=irealstr(indr)
              do while(ipt.lt.irealstr(indr+1).and. errmax.eq.0.)
                if(isecview(ipt).eq.ivlook)errmax=scalexy*
     &              sqrt(xresid(ipt)**2+yresid(ipt)**2)
                ipt=ipt+1
              enddo
            endif
c             
c             When no tiltalign available, just redo the point with
c             highest deviation?  No, projections could be lousy.
c             
c             if(ifdidalign.eq.0.and.ndat.ge.limpshft)then
c             do i=1,ndat
c             if(nint(xr(6,i)).eq.iobjdo.and.xr(13,i).gt.
c             &                           0.99*devmax)errmax=xr(13,i)
c             enddo
c             endif
c             
c             endif
            ifmeanbad=0
c             
c             see if mean residual has gotten a lot bigger
c             
            if(ifdidalign.eq.1)then
              iscur=ivseq-1
              nprev=0
              do while(nprev.le.maxresid.and.iscur.gt.0)
                if(resmean(iobj+(iscur-1)*maxObjOrig).gt.0.)then
                  nprev=nprev+1
                  prevres(nprev)=resmean(iobj+(iscur-1)*maxObjOrig)
                endif
                iscur=iscur-1
              enddo
              if(nprev.gt.minresid)then
                curdif=resmean(iobj+ibaseRes)-prevres(1)
                do i=1,nprev-1
                  prevres(i)=prevres(i)-prevres(i+1)
                enddo
                call avgsd(prevres,nprev-1,resavg,ressd, ressem)
                if(curdif.gt.resdifmin.and.(curdif-resavg)/
     &              ressd.gt.resdifcrit .and. errmax .gt. curResMin)
     &              ifmeanbad=1
              endif
            endif
c             
c             if error greater than criterion after pass 1, or mean
c             residual has zoomed on either pass, delete point for
c             next round
c             
            if ((ipass .eq. 1 .and. errmax.gt.fitdistcrit)
     &          .or.ifmeanbad.eq.1)then
c               write(*,'(i3,f7.2,4f10.5)')iobj,errmax,
c               &                  resmean(iobj+ibaseRes), curdif,resavg,ressd
              wsave(iview, iobjdo) = -1.
              resmean(iobj+ibaseRes) = -1.
              ibase=ibase_obj(iobj)
              do ip=ibase+ipnearest(iobjdo)+1,
     &            ibase+npt_in_obj(iobj)
                object(ip-1)=object(ip)
              enddo
              npt_in_obj(iobj)=npt_in_obj(iobj)-1
              iffound(iobjdo)=0
              ntodo=ntodo+1
              ipnearest(iobjdo)=ipnearsav(iobjdo)
              ndel=ndel+1
              iobjdel(ndel)=iobj
            endif
          endif
        enddo
        nadded=ndel
        if(ndel.ne.0)then
          if(ipass.eq.1)then
            write(*,delfmt1)ndel,(iobjdel(i),i=1,ndel)
          else
            write(*,delfmt2)ndel,(iobjdel(i),i=1,ndel)
            nadded=0
            call tiltali(ifdidalign, ifAlignDone, resmean, ibaseRes,
     &          ibaseOnAlign, iview)
          endif
        endif
      endif
      return
      end subroutine redoFitsEvaluateResiduals

      end

      subroutine errorexit(message, iflocal)
      implicit none
      integer*4 iflocal
      character*(*) message
      write(*,'(/,a,a)')'ERROR: BEADTRACK - ', message
      call exit(1)
      end

* * * * * * BEADTRACK * * * * * *
c	  
c	  BEADTRACK will track selected fiducial gold beads through a series of
c	  tilted views.  It takes a "seed" model, where each bead of choice
c	  is marked with at least a single point on a view near zero tilt.  It
c	  tracks each bead as far as possible and puts out a new model.
c	  
c	  David Mastronarde, 1995
c	  added tilt alignment, 10/6/97
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c	  Log at end of file
	implicit none
	include 'model.inc'
	include 'statsize.inc'
	include 'alivar.inc'
	include 'tltcntrl.inc'
c
	integer maxbox,maxstor,npad,maxarr,limpcl,maxnbox,maxarea,limgaps
	integer liminside,limedge
	parameter (maxbox=64,maxstor=10,npad=8)
	parameter (maxarr=(maxbox+2*npad)*(maxbox+2*npad+2))
	parameter (limpcl=50000,maxnbox=500,maxarea=400,limgaps=20000)
	parameter (liminside=10000,limedge=3000)
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
	character*80 titlech

	integer*4 nx,ny,nz,NXYZ(3),MXYZ(3)
	COMMON //NX,NY,NZ
C   
	real*4 TITLE(20)
	real*4 cursum(maxbox*maxbox),boxes(maxbox*maxbox*maxnbox)
	real*4 boxtmp(maxarr)
	complex ARRAY(maxarr/2),BRRAY(maxarr/2)
C	  
	integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
	CHARACTER*80 FILIN,FILOUT,plfile,modelfile
	integer*4 idxin(liminside),idyin(liminside)
	integer*4 idxedge(limedge),idyedge(limedge)
	integer*4 listz(maxview),izexclude(maxview)
	character*6 areaObjStr
	character*9 dat
	character*8 tim
	logical exist,readw_or_imod
C   
	EQUIVALENCE (NX,NXYZ)
c
	integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),ngsep
	common /mapsep/ ivsep,nsepingrp,ngsep
c	  
	real*4 xseek(maxreal),yseek(maxreal),wcrit(maxreal)
	integer*4 nws(maxreal),iffound(maxreal)
	integer*4 ipnearest(maxreal),ipclose(maxview),izclose(maxview)
	integer*4 incore(maxstor,maxreal),ipnearsav(maxreal)
	integer*4 iobjdel(maxreal),idrop(maxreal)
	real*4 wsave(maxprojpt),xr(msiz,maxreal),seqdist(maxreal)
	real*4 resmean(maxprojpt),prevres(maxview)
	logical missing(0:maxview)
	integer*4 iareaseq(maxarea),ninobjlist(maxarea),indobjlist(maxarea)
	real*4 areadist(maxarea)
	integer*4 iobjlists(3*maxreal),ivlist(maxview),ivSnapList(maxview)
	integer*4 ivseqst(4*maxarea),ivseqnd(4*maxarea),listseq(4*maxarea)
	integer*2 indgap(max_obj_num+1),ivgap(limgaps)
	logical*1 inAnArea(max_obj_num)
	real*4 delta(3), ctf(8193)
	character*1024 listString
C   
	real*4 xf(2,3)
c
	integer*4 modebox,maxwavg,limpstr,limpmag,limprot,limpshft
	real*4 rotstart,fxlocal,fylocal,fraccrit,zorig,dmin2,dmax2,dmean2
	integer*4 ifwhite,iffillin,maxgap,mode,k,kti,lastseq,iobjdo,ip,igap
	integer*4 nround,maxsum,nfit,minfit,maxresid,minresid,ierr,npclist,i,j
	real*4 sdcrit,distcrit,relaxint,relaxdis,fitdistcrit,relaxfit,radmaxfit
	real*4 resdifmin,resdifcrit, tiltfitmin,cgrad,tiltmin,xst,xnd,yst,ynd
	integer*4 minxpiece,nxpieces,nxoverlap,minypiece,nypieces,nyoverlap
	integer*4 nxtotpix,nytotpix,nexclude,ig,iaxtilt,nxlocal,nylocal
	integer*4 nxbox,nybox,nxpad,nypad,ipolar,npixbox,nxpdim,iftrace,iv
	integer*4 minendz,indfree,iobj,ibase,ninobj,ipt,iz,jz,jpt,itmp,iznext
	integer*4 maxnpt,nxnonov,nareax,nynonov,nareay,ix,iy,nobjlists
	integer*4 indstart,nobjtot,nseqs,ipass,nedge,ninside,limcg,nzout
	real*4 radpix,boxsum,boxmin,boxmax,refsum,refmin,refmax,corsum,cormin
	real*4 cormax,xbox,ybox,xt,yt,xtmp,ytmp,wsum,tiltcur,gmamgcur,dxcur
	integer*4 lastlist,iseq,nadded,ivseq,idir,nvlist,iview,ivuse,ib,jx
	integer*4 ifexclude,iexcl,ivl,ntodo,nclose,neardiff,izv,nfound,ipcz
	integer*4 ix0,ix1,iy0,iy1,ic,jc,maxuse,ibox,needed,nextbox,izbox
	integer*4 ifdidalign,ivdel,ivnear,indr,ndat,navg,idif,idirw,itry
	real*4 rotcur,dycur,cosr,sinr,cost,sint,a,b,c,d,e,f,wsumsq,wstmp
	real*4 xnext,ynext,relax,radmax,xpeak,ypeak,xpos,ypos,devavg,devsd
	real*4 devmax,errmax,curdif,resavg,ressd,ressem,gmagcur,wavg,wsd
	integer*4 npioneer,iftrans,ifrotrans,iboxbase,ipntmax,nadtmp,ivlook
	integer*4 ifmeanbad,iscur,nprev,ndel,misstot,nlistz,imodobj,imodcont
	real*4 peak,dist,tmin,tmax,tmean,cvbxcen, cvbycen,area,density
	integer*4 nvert,minInArea,minBeadOverlap,ifLocalArea, localTarget
	integer*4 nvLocalIn, localViewPass, ibaseRes,nSnapList,iobjSave
	logical keepGoing,saveAllPoints,ignoreObjs
	integer*4 numNew,nOverlap,iseqPass,ipassSave,iAreaSave,maxObjOrig
	real*4 outlieElimMin, outlieCrit, outlieCritAbs, curResMin
	real*4 sigma1,sigma2,radius2,radius1,deltactf
	integer*4 maxDrop, nDrop, ndatFit, nareaTot
	real*4 cosd,sind
	character*80 concat
	integer*4 getImodObjsize, niceframe
	logical itemOnList
c	  
	logical pipinput
	integer*4 numOptArg, numNonOptArg,PipGetLogical
	integer*4 PipGetInteger,PipGetBoolean, PipGetThreeIntegers
	integer*4 PipGetString,PipGetFloat, PipGetTwoIntegers, PipGetTwoFloats
	integer*4 PipGetInOutFile, PipNumberOfEntries, ifpip
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  beadtrack
c
	integer numOptions
	parameter (numOptions = 48)
	character*(40 * numOptions) options(1)
	options(1) =
     &      ':InputSeedModel:FN:@:OutputModel:FN:@:ImageFile:FN:@'//
     &      ':PieceListFile:FN:@:SkipViews:LI:@:RotationAngle:F:@'//
     &      ':SeparateGroup:LI:@first:FirstTiltAngle:F:@'//
     &      'increment:TiltIncrement:F:@tiltfile:TiltFile:FN:@'//
     &      'angles:TiltAngles:FA:@:TiltDefaultGrouping:I:@'//
     &      ':TiltNondefaultGroup:IT:@:MagDefaultGrouping:I:@'//
     &      ':MagNondefaultGroup:IT:@:RotDefaultGrouping:I:@'//
     &      ':RotNondefaultGroup:IT:@:MinViewsForTiltalign:I:@'//
     &      ':CentroidRadius:F:@:LightBeads:B:@:FillGaps:B:@'//
     &      ':MaxGapSize:I:@:MinTiltRangeToFindAxis:F:@'//
     &      ':MinTiltRangeToFindAngles:F:@:BoxSizeXandY:IP:@'//
     &      ':RoundsOfTracking:I:@:MaxViewsInAlign:I:@'//
     &      ':RestrictViewsOnRound:I:@:LocalAreaTracking:B:@'//
     &      ':LocalAreaTargetSize:I:@:MinBeadsInArea:I:@'//
     &      ':MinOverlapBeads:I:@:TrackObjectsTogether:B:@'//
     &      ':MaxBeadsToAverage:I:@:PointsToFitMaxAndMin:IP:@'//
     &      ':DensityRescueFractionAndSD:FP:@'//
     &      ':DistanceRescueCriterion:F:@'//
     &      ':RescueRelaxationDensityAndDistance:FP:@'//
     &      ':PostFitRescueResidual:F:@:DensityRelaxationPostFit:F:@'//
     &      ':MaxRescueDistance:F:@:ResidualsToAnalyzeMaxAndMin:IP:@'//
     &      ':DeletionCriterionMinAndSD:FP:@:BoxOutputFile:FN:@'//
     &      ':SnapshotViews:LI:@:SaveAllPointsAreaRound:IP:@'//
     &      'param:ParameterFile:PF:@help:usage:B:'
c
	maxwavg=15
	limpstr=16
	limpmag=6
	limprot=4
	limpshft=3
	mininview=4
	facm=.25
	ncycle=1000
	nsolve=95
	eps=0.00002				!was .00001 then .00002
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
c
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'beadtrack',
     &	    'ERROR: BEADTRACK - ', .true., 3, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

	if (pipinput) then
	  ifpip = 1
	  if (PipGetString('ImageFile', filin) .ne. 0) call errorexit
     &	      ('NO IMAGE INPUT FILE SPECIFIED',0)
	  ierr = PipGetString('PieceListFile', plfile)
	else

	  write(*,'(1x,a,$)')'Image input file: '
	  READ(5,101)FILIN
101	  format(a)
	  write(*,'(1x,a,$)')'Piece list file if image is montage,'//
     &	      ' otherwise Return: '
	  read(*,101)plfile
	endif
c
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C   
	call read_piece_list(plfile,ixpclist,iypclist,izpclist,npclist)
c	  
c	    if no pieces, set up mocklist
c
	if(npclist.gt.limpcl)call errorexit(
     &	    'TOO MANY PIECE COORDINATES FOR ARRAYS',0)
	if(npclist.eq.0)then
	  do i=1,nz
	    ixpclist(i)=0
	    iypclist(i)=0
	    izpclist(i)=i-1
	  enddo
	  npclist=nz
	endif
	call fill_listz(izpclist,npclist,listz,nvuall)
c	print *,nvuall,maxview
	if(nvuall.gt.maxview)call errorexit(
     &	    'TOO MANY VIEWS FOR ARRAYS',0)
	call checklist(ixpclist,npclist,1,nx,minxpiece
     &	    ,nxpieces,nxoverlap)
	nxtotpix=nx+(nxpieces-1)*(nx-nxoverlap)
	call checklist(iypclist,npclist,1,ny,minypiece
     &	    ,nypieces,nyoverlap)
	nytotpix=ny+(nypieces-1)*(ny-nyoverlap)
	xcen=nxtotpix/2.
	ycen=nytotpix/2.
	scalexy=sqrt(xcen**2+ycen**2)
c	    
	if (PipGetInOutFile('InputSeedModel', 1, 'Input seed model file',
     &	    modelfile) .ne. 0) call errorexit
     &	    ('NO INPUT SEED MODEL FILE SPECIFIED', 0)
c
	exist=readw_or_imod(modelfile)
	if(.not.exist)call errorexit('READING SEED MODEL FILE', 0)
c	    
c	  repack this model before working with it
c	    
	call repack_mod
c       
c       convert to image index coordinates and change the origin and delta
c       for X to reflect this
c       
	call irtorg(1,xorig,yorig,zorig)
	call irtdel(1,delta)
	do i=1,n_point
	  p_coord(1,i)=(p_coord(1,i)+xorig)/delta(1)
	  p_coord(2,i)=(p_coord(2,i)+yorig)/delta(2)
	  p_coord(3,i)=(p_coord(3,i)+zorig)/delta(3)
	enddo
	xorig=0.
	yorig=0.
	delta(1)=1.
	delta(2)=1.
c
	xdelt=delta(1)
	ydelt=delta(2)
c
	if (PipGetInOutFile('OutputModel', 2, 'Output model file', modelfile)
     &	    .ne. 0) call errorexit('NO OUTPUT MODEL FILE SPECIFIED', 0)
c
	filout=' '
	nexclude = 0
	if (pipinput) then
	  ierr = PipGetString('BoxOutputFile', filout)
	  if (PipGetString('SkipViews', listString) .eq. 0) call parselist
     &	      (listString, izexclude, nexclude)
	  ierr = PipGetFloat('RotationAngle', rotstart)
	  ierr = PipNumberOfEntries('SeparateGroup', ngsep)
	  if (ngsep .gt. maxgrp) call errorexit
     &	      ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
	  do ig =1, ngsep
	    ierr = PipGetString('SeparateGroup', listString)
	    call parselist(listString, ivsepIn(1,ig),nsepingrpIn(ig))
	  enddo
	else
c
	  write(*,'(1x,a,$)')'List of views to skip over: '
	  call rdlist(5,izexclude,nexclude)
c
c	  new parameters for tilt alignment
c
	  write(*,'(1x,a,$)')
     &	      'Initial angle of rotation in projection plane: '
	  read(5,*)rotstart
c	  
c	    Get list of views to treat separately in automapping
c	    
	  write(*,'(1x,a,/,a,$)') 'For automapping tilt and mag,'
     &	      //' enter the number of sets of views to treat separately'
     &	      //' from the main set of views (otherwise enter 0): '
	  read(5,*)ngsep
	  if (ngsep .gt. maxgrp) call errorexit
     &	      ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
	  do ig=1,ngsep
	    write(*,'(1x,a,i3,a,$)')'List of views in set',ig,
     &		'(ranges OK): '
	    call rdlist(5,ivsepIn(1,ig),nsepingrpIn(ig))
	  enddo
	endif
c	  
	iaxtilt=2
	if(abs(rotstart).ge.45.)iaxtilt=1

	call get_tilt_angles(nvuall,3,tltall, maxview, ifpip)
c	  
c	  DNM 5/3/02: accommodate changes to tiltalign by setting up the
c	  mapping arrays, adding to automap call
c	  DNM 3/25/05: mapping arrays are going to be used for real
c	  4/11/05: default is to ignore objects for transferfid situations
c	  
	nfileviews=nvuall
	ignoreObjs = nvuall.le.2
c
	if (.not.pipinput)print *,
     &	    'Specify grouping for mapping of tilt variables'
	call inputGroupings(nvuall, ifpip, 1,  'TiltDefaultGrouping',
     &	    'TiltNondefaultGroup', nmapTilt, ivSpecStrTilt,
     &	    ivSpecEndTilt, nmapSpecTilt, nRanSpecTilt, maxgrp)
c
	if (.not.pipinput)print *,
     &	    'Specify grouping for mapping of magnification variables'
	call inputGroupings(nvuall, ifpip, 1,  'MagDefaultGrouping',
     &	    'MagNondefaultGroup', nmapMag, ivSpecStrMag,
     &	    ivSpecEndMag, nmapSpecMag, nRanSpecMag, maxgrp)

	nmapRot = 1
	nRanSpecRot = 0
	if (pipinput) call inputGroupings(nvuall, ifpip, 0,
     &	    'RotDefaultGrouping', 'RotNondefaultGroup', nmapRot,
     &	    ivSpecStrRot, ivSpecEndRot, nmapSpecRot, nRanSpecRot, maxgrp)
c	  
	nvLocalIn = 0
	localViewPass = 0
	tiltfitmin=15.
	nSnapList = 0
c	  
	if (pipinput) then
	  if (PipGetFloat('CentroidRadius', cgrad) .ne. 0) call errorexit
     &	      ('YOU MUST ENTER A RADIUS FOR CENTROID CALCULATION', 0)
	  if (PipGetTwoIntegers('BoxSizeXandY', nxbox, nybox) .ne. 0)
     &	      call errorexit('YOU MUST ENTER A BOX SIZE', 0)
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
	  ierr = PipGetInteger('MinOverlapBeads', minBeadOverlap)
	  ierr = PipGetInteger('RoundsOfTracking', nround)
	  ierr = PipGetInteger('MaxBeadsToAverage', maxsum)
	  ierr = PipGetTwoIntegers('PointsToFitMaxAndMin', nfit, minfit)
	  ierr = PipGetTwoFloats('DensityRescueFractionAndSD', fraccrit,sdcrit)
	  ierr = PipGetFloat('DistanceRescueCriterion', distcrit)
	  ierr = PipGetTwoFloats('RescueRelaxationDensityAndDistance',
     &	      relaxint,relaxdis)
	  ierr = PipGetFloat('PostFitRescueResidual',fitdistcrit )
	  ierr = PipGetFloat('DensityRelaxationPostFit', relaxfit)
	  ierr = PipGetFloat('MaxRescueDistance', radmaxfit)
	  ierr = PipGetTwoIntegers('ResidualsToAnalyzeMaxAndMin',
     &	      maxresid,minresid)
	  ierr = PipGetTwoFloats('DeletionCriterionMinAndSD', resdifmin,
     &	      resdifcrit)
	  ierr = PipGetLogical('TrackObjectsTogether', ignoreObjs)
	  if (PipGetString('SnapshotViews', listString) .eq. 0) call parselist
     &	      (listString, ivSnapList, nSnapList)
	  ierr = PipGetTwoIntegers('SaveAllPointsAreaRound', iAreaSave,
     &	      iPassSave)
	else
	  write(*,'(1x,a,$)')'Minimum # of views required for tilt'//
     &	      ' aligning: '
	  read(5,*)minvtiltali
c	    
	  write(*,'(1x,a,$)')'Radius for centroid calculation, 0 for '//
     &	      'dark or 1 for light beads: '
	  read(5,*)cgrad,ifwhite
c	    
	  write(*,'(1x,a,$)')'1 to fill in gaps, 0 not to: '
	  read(5,*)iffillin
c	    
	  write(*,'(1x,a,$)')'Maximum gap to create: '
	  read(5,*)maxgap
c	    
	  write(*,'(1x,a,/,a,$)')'Minimum ranges of tilt angles required'
     &	      //' for finding tilt axis',' and for finding tilt angles: '
	  read(5,*)randoaxis,randotilt
c	    
	  write(*,'(1x,a,i4,a,$)')'X and Y dimensions of box (max'
     &	      ,maxbox,'): '
	  read(5,*)nxbox,nybox
c	    
	  write(*,'(1x,a,$)')
     &	      'Maximum number of beads to average for correlation: '
	  read(5,*)maxsum
c	  
	  write(*,'(1x,a,$)')'# of points to fit for extrapolation'
     &	      //', minimum # required: '
	  read(5,*)nfit,minfit
c
	  write(*,'(1x,a,$)')'Fraction of mean and # of SDs for'//
     &	      ' density criteron for rescue: '
	  read(5,*)fraccrit,sdcrit
	  write(*,'(1x,a,$)')
     &	      'Distance criterion for rescue (- for full report): '
	  read(5,*)distcrit
c	    
	  write(*,'(1x,a,$)')'Fractions to relax criterion during '//
     &	      'density and distance rescue: '
	  read(5,*)relaxint,relaxdis
c	    
	  write(*,'(1x,a,$)')
     &	      'Criterion distance for rescue after fitting: '
	  read(5,*)fitdistcrit
c	    
	  write(*,'(1x,a,$)')'Relaxation of density criterion, maximum '//
     &	      'rescue distance: '
	  read(5,*)relaxfit,radmaxfit
	  write(*,'(1x,a,$)')'Max and min # of residual changes to use '//
     &	      'to get mean and SD: '
	  read(5,*)maxresid,minresid
	  write(*,'(1x,a,$)')'Minimum residual change & criterion # of'//
     &	      ' SD from mean for deletion of pt: '
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
	if (sigma1 .ne. 0 .or. radius2 .ne. 0) call setctfwsr
     &	    (sigma1,sigma2,radius1,radius2,ctf,nxpad,nypad,deltactf)
c
	iftrace=0
	if(distcrit.lt.0.)then
	  iftrace=1
	  distcrit=-distcrit
	endif
c
	do iv=1,nvuall
	  rotorig(iv)=rotstart
	  tiltorig(iv)=tltall(iv)
	  gmagorig(iv)=1.
	  comp(iv)=1.
	  mapcomp(iv)=0
	  skew(iv)=0.
	  mapskew(iv)=0
	  dmag(iv)=0.
	  mapdmag(iv)=0
	  alf(iv)=0.
	  mapalf(iv)=0
	  dxysav(1,iv)=0.
	  dxysav(2,iv)=0.
	enddo
	mapdumdmag=0
	mapdmagstart=1
c	  
c	  figure out where to start: section with most points at minimum tilt
c	  ivlist is the number of points present on each view
c	  ivgap has a list of views with gaps for each object (if not filling in)
c	  indgap is index to location in ivgap for each object
c	  minendz is the minimum ending Z of all the objects
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
c	      first order them by z
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
c	      find gaps and add to list
c	      
	      do ipt=1,ninobj-1
		iz=nint(p_coord(3,object(ipt+ibase)))+1
		iznext=nint(p_coord(3,object(ipt+1+ibase)))+1
		if(iznext.gt.iz+1)then
		  do iv=iz+1,iznext-1
		    ivgap(indfree)=iv
		    indfree=indfree+1
		    if(indfree.gt.limgaps)call errorexit(
     &			'TOO MANY GAPS IN EXISTING MODEL FOR ARRAYS', 0)
		  enddo
		endif
	      enddo
	    endif
	  endif
	enddo
	indgap(max_mod_obj+1)=indfree
c	  
c	  now find section with most points
c
	maxnpt=0
	tiltmin=200.
	do i=1,nvuall
	  if (ivlist(i).gt.maxnpt .or.
     &	      (ivlist(i).eq.maxnpt .and. abs(tltall(i)).lt.tiltmin)) then
	    maxnpt=ivlist(i)
	    tiltmin=abs(tltall(i))
	    imintilt=i
	  endif
	enddo
c
c	  figure out an order for the points: from the center outwards
c	  i.e., first find position from center at minimum tilt, save that in
c	  xyzsav, and store the square of distance in seqdist
c	  iobjseq is just a list of objects to do in original order
c	  
	nobjdo=0
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
		xx(nobjdo) = xyzsav(1,iobj)
		yy(nobjdo) = xyzsav(2,iobj)
	      endif
	    enddo
	    xyzsav(3,iobj)=0.
	  endif
	enddo
c	  
c	  set up for one area in X and Y, then compute number of areas and
c	  overlaps if locals
c
	nAreaX = 1
	nAreaY = 1
	nxOverlap = 0
	nyOverlap = 0
	if (ifLocalArea .ne. 0) then
	  areaObjStr = 'area'
	  noverlap = 0
	  if (minBeadOverlap .gt. 0) then
c	    
c	      determine an average density from the area of the convex bound
c	      set target overlap so that 1.5 overlap areas at this density
c	      will give minimum number of overlap beads
c	      
	    call convexbound(xx,yy,nobjdo,0., 2. * cgrad, xresid,yresid, nvert,
     &		cvbxcen, cvbycen, maxprojpt)
	    area = 0.
	    do i = 1, nvert
	      j = mod(i, nvert) + 1
	      area = area + 0.5 * (yresid(j)+yresid(i)) * (xresid(j)-xresid(i))
	    enddo
	    density = nobjdo / abs(area)
	    noverlap = minBeadOverlap / (density * localTarget)
c	    print *,area,density,noverlap
	  endif
c	    
c	    get number of areas, round up so areas will start below target
c	    Then compute or set overlaps so local sizes can be computed
c
	  nareax = (nxtotpix + localTarget - 2 * noverlap - 2) /
     &	      (localTarget - noverlap)
	  nareay = (nytotpix + localTarget - 2 * noverlap - 2) /
     &	      (localTarget - noverlap)
	  if (nareax .eq. 1) then
	    if (nAreaY .gt. 1) nyoverlap = noverlap
	  else if (nAreaY .eq. 1) then
	    nxoverlap = noverlap
	  else
	    
	    nxLocal = max(nxtotpix / nAreaX, nytotpix / nAreaY) + 1
	    do while ((nxLocal * nAreaX - nxtotpix) / (nAreaX - 1) +
     &		(nxLocal * nAreaY - nytotpix) / (nAreaY - 1) .lt.
     &		2 * noverlap)
	      nxLocal = nxLocal + 1
	    enddo
	    nyLocal = nxLocal
	    nxOverlap = (nxLocal * nAreaX - nxtotpix) / (nAreaX - 1)
	    nyOverlap = (nyLocal * nAreaY - nytotpix) / (nAreaY - 1)
	  endif
	endif
c	  
c	  Get area sizes regardless; compute area distances from center
c
	nxLocal = (nxtotpix + nxoverlap * (nareax - 1)) / nAreaX + 1
	nyLocal = (nytotpix + nyoverlap * (nareay - 1)) / nAreaY + 1
	nareaTot = nareax*nareay
	if (ifLocalArea .ne. 0) print *,
     &	    'Local area number, size, overlap - X:', nAreaX, nxLocal,
     &	    nxOverlap,',  Y:', nAreaY, nyLocal, nyOverlap
	if(nareaTot.gt.maxarea) call errorexit(
     &	    'TOO MANY LOCAL AREAS FOR ARRAYS', 0)
	do i=1,nareaTot
	  ix=mod(i-1,nareax)
	  iy=(i-1)/nareax
	  iareaseq(i)=i
	  areadist(i)=(ix*(nxlocal-nxoverlap)+nxlocal/2-xcen)**2+
     &	      (iy*(nylocal-nyoverlap)+nylocal/2-ycen)**2
	enddo
c	  
c	  If there are no local areas but there are multiple objects, set up
c	  one area per object.  No point getting distance, they are independent
c	  
	if (ifLocalArea .eq. 0 .and. getImodObjsize() .gt. 0 .and.
     &	    .not. ignoreObjs) then
	  nareaTot = getImodObjsize()
	  do k = 1, nareaTot
	    iareaseq(k) = k
	    areadist(k) = 0.
	  enddo
	endif
c	  
c	  order areas by distance: iareaseq has sequence of area numbers
c
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
c	  go through each area finding points within it
c	  iobjlists is the list of objects to do for each area
c	  indobjlist is the starting index in that list for each area
c	  ninobjlist is the number of objects in the list for each area
c	  
	nobjlists=0
	indfree=1
	do j = 1, max_mod_obj
	  inAnArea(j) = .false.
	enddo
	do k=1,nareaTot
	  if (ifLocalArea .ne. 0 .or. nareaTot .eq. 1) then
c	    
c	      get starting coordinates and set up to loop until conditions met
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
c		Look for objects in the area, count up the new ones
c		
	      do j=1,nobjdo
		iobj=iobjseq(j)
		if(xyzsav(1,iobj).ge.xst.and.xyzsav(1,iobj).le.xnd.and.
     &		    xyzsav(2,iobj).ge.yst.and.xyzsav(2,iobj).le.ynd)then
		  iobjlists(indfree)=iobj
		  indfree=indfree+1
		  if(indfree.gt.maxreal*3)call errorexit(
     &		      'TOO MANY AREAS FOR OBJECT LISTS', 0)
		  if (.not. inAnArea(iobj)) numNew = numNew + 1
		endif
	      enddo
c		
c		make area bigger if there are any new points at all in it and
c		it does not already have all the points, and
c		either the total in it is too low or this is an area after the
c		first and the old ones are too low for overlap
c
	      keepGoing = numNew .gt. 0 .and. indfree - indstart .lt. nobjdo
     &		  .and. (indfree - indstart .lt. minInArea .or. (k .gt. 1 .and.
     &		  indfree - indstart - numNew .lt. minBeadOverlap))
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
c	      areas from objects: make list of objects in it
c	      
	    numNew = 0
	    indstart = indfree
	    do j=1,nobjdo
	      iobj=iobjseq(j)
	      call objtocont(iobj, obj_color, ix, iy)
	      if (ix .eq. k) then
		numNew = numNew + 1
		iobjlists(indfree)=iobj
		indfree=indfree+1
	      endif
	    enddo
	  endif
c
	  if (numNew .gt. 0) then
c	  
c	      if the area has new points, order the list by distance from
c	      center
c	      (Should that be center of area?  It is center of whole field)
c
	    nobjlists=nobjlists+1
	    indobjlist(nobjlists)=indstart
	    ninobjlist(nobjlists)=indfree-indstart
	    do i = indstart,indfree-1
	      inAnArea(iobjlists(i)) = .true.
	    enddo
	    do i=indstart,indfree-2
	      do j=i+1,indfree-1
		if(seqdist(iobjlists(i)).gt.seqdist(iobjlists(j)))then
		  itmp=iobjlists(i)
		  iobjlists(i)=iobjlists(j)
		  iobjlists(j)=itmp
		endif
	      enddo
	    enddo
	    if (ifLocalArea .ne. 0) write(*,119)nobjlists, nint(xst+xcen),
     &		nint(xnd+xcen),
     &		nint(yst+ycen), nint(ynd+ycen), ninobjlist(nobjlists), numNew
119	    format('Area',i3,', X:',i6,' to',i6,', Y:',i6,' to',i6,',',
     &		i4,' points,',i4,' new')
	  endif
	enddo
c
	if(nobjdo.gt.maxreal.or.nvuall*max_mod_obj.gt.maxprojpt)
     &	    call errorexit(
     &	    'TOO MANY OBJECTS OR VIEWS AND OBJECTS FOR ARRAYS',0)
c	  
	if(nobjdo*maxsum*npixbox.gt.maxnbox*maxbox**2) call errorexit(
     &	    'TOO MUCH SUMMING IN BOXES TOO LARGE FOR TOO MANY POINTS',0)
c	  
c	  set up sequencing for object lists and views - odd passes from
c	  middle outward, even passes from ends inward
c	  
	nobjtot=nobjdo
	nseqs=0
	lastseq=0
	do ipass=1,nround
	  if(mod(ipass,2).eq.1)then
	    do i=1,nobjlists
	      nseqs=nseqs+1
	      ivseqst(nseqs)=imintilt-1
	      ivseqnd(nseqs)=1
	      listseq(nseqs)=i
	      nseqs=nseqs+1
	      ivseqst(nseqs)=imintilt
	      ivseqnd(nseqs)=nvuall
	      listseq(nseqs)=i
	    enddo
	  else
	    do i=1,nobjlists
	      nseqs=nseqs+1
	      ivseqnd(nseqs)=imintilt-1
	      ivseqst(nseqs)=1
	      listseq(nseqs)=i
	      nseqs=nseqs+1
	      ivseqnd(nseqs)=imintilt
	      ivseqst(nseqs)=nvuall
	      listseq(nseqs)=i
	    enddo
	  endif
	enddo
c
	do i=1,nobjdo
	  do j=1,maxsum
	    incore(j,i)=-1
	  enddo
	enddo
	do i=1,max_mod_obj*nvuall
	    resmean(i)=-1.
	    wsave(i)=-1.
	enddo
c	  
c	  get list of inside and edge pixels for centroid
c	  
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
     &		'BEAD RADIUS FOR CENTROIDS TOO LARGE FOR ARRAYS',0)
	  enddo
	enddo
c	  
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
	  call date(dat)
	  write(titlech,301) dat,tim
301	  FORMAT('Boxes',32x,a9,2x,a8)
	  read(titlech,'(20a4)')(title(kti),kti=1,20)
	  nzout=0
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
c	  Start looping on the sequences of views
c	  
	lastlist=-1
	maxObjOrig = max_mod_obj
	do iseq=1,nseqs
	  nadded=1
	  nvLocal = 0
	  iseqPass = ((iseq + 1) / 2 - 1) / nobjlists + 1
	  if (iseqPass .ge. localViewPass)
     &	      nvLocal = nvLocalIn
	  saveAllPoints = iareaSave .eq. listseq(iseq) .and.
     &	      ipassSave .eq. iseqPass
	  if(listseq(iseq).ne.lastseq)then
c	      
c	      initialize if doing a new set of points
c
	    initxyzdone=0
	    ivseq=1
	    ibaseRes = 0
	    lastseq=listseq(iseq)
	    nobjdo=ninobjlist(lastseq)
	    do i=1,nobjdo
	      iobjseq(i)=iobjlists(i+indobjlist(lastseq)-1)
	    enddo
	    do i=1,maxObjOrig*nvuall
	      resmean(i)=-1.
	    enddo
	    write(*,123)areaObjStr,listseq(iseq),iseqPass,nobjdo
123	    format('Starting ',a,i4,', round',i3,',',i4,' contours');
	    if (nobjlists .gt. 1) write(*,'(19i4)') (iobjseq(i),i=1,nobjdo)
	  endif
	  if (saveAllPoints .and. mod(iseq,2) .eq. 1) then
	    do j = 1, 10
	      iobj = getImodObjsize() + j
	      call putimodflag(iobj, 1)
	      call putsymtype(iobj, 0)
	      call putsymsize(iobj, 5)
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
c	    Set direction and make list of views to do
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
c	    loop on the views
c
	  do ivl=1,nvlist
	    iview=ivlist(ivl)
	    iznext=iview-1
c	      
c	      first see how many are done on this view
c	      
	    ntodo=0
	    do iobjdo=1,nobjdo
	      iobj=iobjseq(iobjdo)
	      ninobj=npt_in_obj(iobj)
	      nclose=0
	      iffound(iobjdo)=0
	      ibase=ibase_obj(iobj)
c		
c		find closest ones within gap distance, find out if already
c		done, mark as a 2 to protect them
c		ipnearest holds the point number of the nearest for each object
c		ipclose is the list of points within gap distance for this object
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
		  nfound=nfound+1
		elseif(abs(izv-iview).le.maxgap)then
		  xbox=p_coord(1,ipt)
		  ybox=p_coord(2,ipt)
		  izbox=p_coord(3,ipt)
		  call find_piece(ixpclist,iypclist,izpclist,npclist,
     &		      nx, ny, nxbox,nybox,xbox,ybox,izbox,ix0,ix1,
     &		      iy0,iy1,ipcz)
		  if(ipcz.ge.0)then
		    nclose=nclose+1
		    ipclose(nclose)=ip
		    izclose(nclose)=izv
		  endif
		endif
	      enddo
c		
c		if not found and none close, mark as -1 not to do
c		If there are close ones, make sure this is not a gap to preserve
c
	      if(nclose.eq.0.and.iffound(Iobjdo).eq.0)
     &		  iffound(iobjdo)=-1
	      if(nclose.gt.0.and.iffound(iobjdo).eq.0)then
		if(iffillin.eq.0.and.indgap(iobj+1).gt.indgap(iobj))then
c		    
c		    if not filling in, look for gap on list
c		    
		  do igap=indgap(iobj),indgap(iobj+1)-1
		    if(ivgap(igap).eq.iview)iffound(iobjdo)=-1
		  enddo
		endif
		if(iffound(iobjdo).eq.0)then
c		    
c		    order feasible points by distance
c		    
		  ntodo=ntodo+1
		  do ic=1,nclose-1
		    do jc=ic+1,nclose
		      if(abs(izclose(jc)-iview).lt.
     &			  abs(izclose(ic)-iview)) then
			itmp=izclose(ic)
			izclose(ic)=izclose(jc)
			izclose(jc)=itmp
			itmp=ipclose(ic)
			ipclose(ic)=ipclose(jc)
			ipclose(jc)=itmp
		      endif
		    enddo
		  enddo
		  maxuse=min(nclose,maxsum)
c		    
c		    see if boxes in memory match, free them if not
c		    
		  do ibox=1,maxsum
		    if(incore(ibox,iobjdo).ge.0)then
		      needed=0
		      do ic=1,maxuse
			if(incore(ibox,iobjdo).eq.izclose(ic))needed=1
		      enddo
		      if(needed.eq.0)incore(ibox,iobjdo)=-1
		    endif
		  enddo
c		    
c		    now load ones that are needed into last free box
c		    
		  do ic=1,maxuse
		    needed=1
		    do ibox=1,maxsum
		      if(incore(ibox,iobjdo).eq.izclose(ic))needed=0
		    enddo
		    if(needed.eq.1)then
		      do ibox=1,maxsum
			if(incore(ibox,iobjdo).lt.0)ib=ibox
		      enddo
		      incore(ib,iobjdo)=izclose(ic)
		      nextbox=ib+(iobjdo-1)*maxsum
		      ipt=abs(object(ibase+ipclose(ic)))
		      xbox=p_coord(1,ipt)
		      ybox=p_coord(2,ipt)
		      izbox=nint(p_coord(3,ipt))
		      call find_piece(ixpclist,iypclist,izpclist,
     &			  npclist, nx, ny, nxbox,nybox,xbox,ybox,
     &			  izbox,ix0,ix1, iy0,iy1,ipcz)
		      call imposn(1,ipcz,0)
		      call irdpas(1,boxtmp,nxbox,nybox, ix0,ix1,iy0,
     &			  iy1,*99)
c			
c			do subpixel shift and calculate CG and wsum value
c			if it is not already saved
c
		      xt=nint(xbox)-xbox
		      yt=nint(ybox)-ybox
		      call qdshift(boxtmp,boxes(1+(nextbox-1)*npixbox)
     &			  ,nxbox, nybox,xt,yt)
		      if(wsave(izbox+1+(iobjdo-1)*nvuall).lt.0.)then
			xtmp=0.
			ytmp=0.
			call calcg(boxes(1+(nextbox-1)*npixbox),nxbox,
     &			    nybox,xtmp, ytmp, idxin, idyin,ninside,
     &			    idxedge, idyedge, nedge, ipolar,wsum)
			wsave(izbox+1+(iobjdo-1)*nvuall)=wsum
		      endif
		    endif
		  enddo
		endif
	      endif
	      ipnearsav(iobjdo)=ipnearest(iobjdo)
	    enddo
	    ipass=1
	    do while(ipass.le.2.and.ntodo.ne.0)
c		
c		now try to do tiltalign if possible
c		
	      if(nadded.ne.0)call tiltali(ifdidalign,
     &		  resmean(1+ibaseRes),iview)

c		
c		get tentative tilt, rotation, mag for current view
c		
	      if(ifdidalign.gt.0)then
		ivdel=200
		do iv=1,nview
		  if(abs(mapViewToFile(iv)-iview).lt.ivdel)then
		    ivdel=abs(mapViewToFile(iv)-iview)
		    ivnear=mapViewToFile(iv)
		  endif
		enddo
		ivuse=ivnear
		if(mapFileToView(iview).gt.0)ivuse=iview
		tiltcur=tiltorig(ivuse)+tltall(iview)-tltall(ivuse)
		gmagcur=gmagorig(ivuse)
		rotcur=rotorig(ivuse)
		cosr=cosd(rotcur)
		sinr=sind(rotcur)
		cost=cosd(tiltcur)
		sint=sind(tiltcur)
c		  
c		  back-rotate the dxy so tilt axis vertical, adjust the 
c		  dx by the difference in tilt angle, and rotate back
c
		a = cosr * dxysav(1,ivuse) + sinr * dxysav(2,ivuse)
		b = -sinr * dxysav(1,ivuse) + cosr * dxysav(2,ivuse)
		a = a * cost / cosd(tiltorig(ivuse))
		dxcur = cosr * a - sinr * b
		dycur = sinr * a + cosr * b
		a = gmagcur * cost * cosr
		b = - gmagcur * sinr
		c = gmagcur * sint * cosr
		d = gmagcur * cost * sinr
		e = gmagcur * cosr
		f = gmagcur * sint * sinr
	      endif
c		
c		now get projected positions
c		
	      do iobjdo=1,nobjdo
		indr=0
		if(ifdidalign.eq.1)then
		  do ipt=1,nrealpt
		    if(iobjseq(iobjdo).eq.iobjali(ipt))
     &			indr=iobjseq(iobjdo)
		  enddo
		endif
		if(indr.ne.0)then
c		    
c		    there is a 3D point for it: so project it
c		    
		  xseek(iobjdo)=a*xyzsav(1,indr)+b*xyzsav(2,indr)+
     &		      c*xyzsav(3,indr)+dxcur+xcen
		  yseek(iobjdo)=d*xyzsav(1,indr)+e*xyzsav(2,indr)+
     &		      f*xyzsav(3,indr)+dycur+ycen
		else
		  call nextpos(iobjseq(iobjdo),ipnearest(iobjdo),idir,
     &		      iznext,tltall,nfit, minfit,iaxtilt,tiltfitmin,
     &		      xseek(iobjdo),yseek(iobjdo))
		endif
		if (saveAllPoints) then
		  iobjSave = iobjseq(iobjdo) + (5 * ipass - 4) * maxObjOrig
		  call add_point(iobjSave,0 ,
     &		      xseek(iobjdo), yseek(iobjdo), iznext)
		endif
	      enddo
c		
c		build list of points already found for fitting; put
c		actual positions in 4 and 5 for compatibiity with xfmodel
c		
	      ndat=0
	      do iobjdo=1,nobjdo
		if(iffound(iobjdo).gt.0)then
		  ndat=ndat+1
		  xr(1,ndat)=xseek(iobjdo)-xcen
		  xr(2,ndat)=yseek(iobjdo)-ycen
		  ipt=object(ibase_obj(iobjseq(iobjdo))+
     &		      ipnearest(iobjdo))
		  xr(4,ndat)=p_coord(1,ipt)-xcen
		  xr(5,ndat)=p_coord(2,ipt)-ycen
		  xr(6,ndat)=iobjdo
		endif
	      enddo
c		
c		LOOP THROUGH POINTS, REFINING PROJECTIONS BEFORE SEARCH
c		
	      if(ipass.eq.1)npioneer=ndat
	      nadded=0
	      do iobjdo=1,nobjdo
		if(iffound(iobjdo).eq.0.or.iffound(iobjdo).eq.1)then
c		    
c		    get w criterion: average it over as many near views as
c		    possible.  maxwavg specifies both the maximum distance
c		    away in Z and the maximum number to average
c		    
		  navg=0
		  wsum=0.
		  wsumsq=0.
		  idif=1
		  do while(idif.le.maxwavg.and.navg.lt.maxwavg)
		    do idirw=-1,1,2
		      itry=iview+idirw*idif
		      if(itry.gt.0.and.itry.le.nvuall)
     &			  then
			wstmp=wsave(itry+(iobjdo-1)*nvuall)
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
c		    if there are existing points, find right kind of transform
c		    and modify position
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
		    if (ndatFit .lt. 3 .or. ifdidalign.eq.0) maxDrop = 0
		    call findxf_wo_outliers(xr,ndatFit,xcen,ycen,iftrans,
     &			ifrotrans,maxDrop, outlieCrit,outlieCritAbs,
     &			outlieElimMin,idrop,nDrop,xf, devavg,devsd,
     &			devmax,ipntmax)
		    call xfapply(xf,xcen,ycen,xseek(iobjdo),
     &			yseek(iobjdo), xnext,ynext)
		    if (saveAllPoints) then
		      iobjSave = iobj + (5 * ipass - 3) * maxObjOrig
		      call add_point(iobjSave, 0, xnext, ynext, iznext)
		    endif
		  endif
c		    
		  call find_piece(ixpclist,iypclist,izpclist,
     &		      npclist, nx, ny, nxbox,nybox,xnext,ynext,
     &		      iznext,ix0,ix1, iy0,iy1,ipcz)
		  if(ipcz.ge.0)then
c		      
c		      get image area and pad into array on first pass
c		      
		    call imposn(1,ipcz,0)
		    call irdpas(1,boxtmp,nxbox,nybox,ix0,ix1,iy0,iy1,*99)
		    if(ipass.eq.1)then
		      call taperoutpad(boxtmp,nxbox,nybox,array,nxpdim,
     &			  nxpad, nypad, 0, 0.)
		      call setmeanzero(array,nxpdim,nypad,nxpad, nypad)
c			
c			Add all nearby boxes in memory into cursum
c
		      do i=1,npixbox
			cursum(i)=0.
		      enddo
		      do ibox=1,maxsum
			if(incore(ibox,iobjdo).ge.0)then
			  nextbox=ibox+(iobjdo-1)*maxsum
			  iboxbase=(nextbox-1)*npixbox
			  do i=1,npixbox
			    cursum(i)=cursum(i)+boxes(i+iboxbase)
			  enddo
			endif
		      enddo
		      call taperoutpad(cursum,nxbox,nybox,brray,nxpdim,
     &			  nxpad, nypad, 0, 0.)
		      call setmeanzero(brray,nxpdim,nypad,nxpad, nypad)
c			
c			correlate the two
c			
		      call todfft(array,nxpad,nypad,0)
		      call todfft(brray,nxpad,nypad,0)
		      do jx=1,nypad*(nxpad+2)/2
			array(jx)=array(jx)*conjg(brray(jx))
		      enddo
		      if (deltactf .ne. 0) call filterpart(array,array,nxpad,
     &			  nypad,ctf, deltactf)
		      call todfft(array,nxpad,nypad,1)
c			
c			find peak of correlation, then centroid
c			
		      call peakfind(array,nxpdim,nypad,xpeak,ypeak, peak)
		      if (saveAllPoints) then
			iobjSave = iobj + (5 * ipass - 2) * maxObjOrig
			call add_point(iobjSave, 0,
     &			    nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
		      endif

		      call calcg(boxtmp,nxbox,nybox,xpeak,ypeak, idxin,
     &			  idyin,ninside,idxedge,idyedge, nedge,ipolar,wsum)
		      dist=sqrt(xpeak**2+ypeak**2)
		      if (saveAllPoints) then
			iobjSave = iobj + (5 * ipass - 1) * maxObjOrig
			call add_point(iobjSave, 0,
     &			    nint(xnext) + xpeak, nint(ynext) + ypeak, iznext)
		      endif
		    endif
		    if(nws(iobjdo).gt.2.and.(wsum.lt.wcrit(iobjdo)
     &			.or. dist.gt.distcrit .or. ipass.eq.2))then
c			
c			rescue attempt; search concentric rings from the
c			center of box and take the first point that goes
c			above the relaxed wsum criterion
c			
		      if(ipass.eq.1)then
			if(dist.gt.distcrit)then
			  relax=relaxdis
c			  write(*,102)'distance',iznext,dist,wsum,wavg,wsd
c102			  format(' Rescue-',a,', sec',i4,', dist=',f5.1,
c     &			      ', dens=',f9.0,', mean,sd=',f9.0,f7.0)
			else
			  relax=relaxint
c			    write(*,102)'density ',iznext,dist,wsum,wavg,wsd
			endif
			radmax=max(nxbox,nybox)
		      else
			relax=relaxfit
			radmax=radmaxfit
		      endif
		      call rescue(boxtmp,nxbox,nybox,xpeak,ypeak,
     &			  idxin, idyin,ninside,idxedge,idyedge,
     &			  nedge,ipolar,radmax,relax*wcrit(iobjdo),
     &			  wsum)

		    elseif(ipass.eq.2)then
		      wsum=0.
		    endif
c		      
c		      
c		      
		    if(wsum.ne.0.)then
		      wsave(iview+(iobjdo-1)*nvuall)=wsum
		      xpos=nint(xnext)+xpeak
		      ypos=nint(ynext)+ypeak
		      if(iftrace.ne.0)
     &			  write(*,'(3i5,4f7.1,2f12.0)')nzout,iznext,iobj,
     &			  xnext, ynext, xpos,ypos,peak,wsum
c			
c			add point to model
c			
		      call add_point(iobj,ipnearest(iobjdo),
     &			  xpos,ypos,iznext)
		      if (saveAllPoints) then
			iobjSave = iobj + (5 * ipass) * maxObjOrig
			call add_point(iobjSave, 0, xpos, ypos, iznext)
		      endif
		      nadded=nadded+1
		      iobjdel(nadded)=iobj
c			
c			mark as found, add to data matrix for alignment
c			
		      iffound(iobjdo)=1
		      nfound=nfound+1
		      ndat=ndat+1
		      xr(1,ndat)=xseek(iobjdo)-xcen
		      xr(2,ndat)=yseek(iobjdo)-ycen
		      xr(4,ndat)=xpos-xcen
		      xr(5,ndat)=ypos-ycen
		      xr(6,ndat)=iobjdo
c			
		      if(filout.ne.' ')then
			if (modebox .eq. 2) then
			  call iclden(boxtmp,nxbox,nybox,1, nxbox, 1,
     &			      nybox,tmin,tmax,tmean)
			else
			  call isetdn(boxtmp,nxbox,nybox,modebox,1,
     &			      nxbox, 1, nybox,tmin,tmax,tmean)
			endif
			call iwrsec(2,boxtmp)
			boxmin=min(boxmin,tmin)
			boxmax=max(boxmax,tmax)
			boxsum=boxsum+tmean
			nzout=nzout+1
			do i=1,npixbox
			  boxtmp(i)=cursum(i)
			enddo
			if (modebox .eq. 2) then
			  call iclden(boxtmp,nxbox,nybox,1, nxbox, 1,
     &			      nybox,tmin,tmax,tmean)
			else
			  call isetdn(boxtmp,nxbox,nybox,modebox,1,
     &			      nxbox, 1, nybox,tmin,tmax,tmean)
			endif
			call iwrsec(3,boxtmp)
			refmin=min(refmin,tmin)
			refmax=max(refmax,tmax)
			refsum=refsum+tmean
			call splitpack(array,nxpdim,nxpad,nypad,boxtmp)
			if (modebox .eq. 2) then
			  call iclden(boxtmp,nxpad,nypad,1, nxpad, 1,
     &			      nypad,tmin,tmax,tmean)
			else
			  call isetdn(boxtmp,nxpad,nypad,modebox,1,
     &			      nxpad, 1, nypad,tmin,tmax,tmean)
			endif
			call iwrsec(4,boxtmp)
			cormin=min(cormin,tmin)
			cormax=max(cormax,tmax)
			corsum=corsum+tmean
			write(2,'(3i6)')nint(xnext)-nxbox/2,
     &			    nint(ynext)-nybox/2,iznext
			write(3,'(3i6)')nint(xnext)-nxpad/2,
     &			    nint(ynext)-nypad/2,iznext
		      endif
		    endif
		  endif
		endif
	      enddo
c		
c		get a final fit and a new tiltalign, then find a maximum
c		error for each pt
c		
	      if(ipass.eq.2)write(*,114)nadded,(iobjdel(i),i=1,nadded)
114	      format(i4,' pts added on pass 2, conts:',(11i4))
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
		if (ndat .lt. 3 .or. ifdidalign.eq.0) maxDrop = 0
		call findxf_wo_outliers(xr,ndat,xcen,ycen,iftrans,
     &		    ifrotrans,maxDrop, outlieCrit,outlieCritAbs,
     &		    outlieElimMin,idrop,nDrop,xf, devavg,devsd,
     &		    devmax,ipntmax)
		write(*,113)iview,ipass,ndat,ndrop,devavg,devsd,devmax
113		format('view',i4,', pass',i2,',',i4,
     &		    ' points (-', i3,'), mean, sd, max dev:' ,3f8.2)
	      endif
c		
c		do tiltali if anything changed, so can get current residuals
c		
	      if(nadded.gt.0)call tiltali(ifdidalign,
     &		  resmean(1+ibaseRes),iview)

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
c		    if(ipass.eq.1)then
		      indr=0
c			
c			find point in the tiltalign, get its residual
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
     &			      sqrt(xresid(ipt)**2+yresid(ipt)**2)
			  ipt=ipt+1
			enddo
		      endif
c			
c			When no tiltalign available, just redo the point with
c			highest deviation?  No, projections could be lousy.
c			
c			if(ifdidalign.eq.0.and.ndat.ge.limpshft)then
c			do i=1,ndat
c			if(nint(xr(6,i)).eq.iobjdo.and.xr(13,i).gt.
c			&				0.99*devmax)errmax=xr(13,i)
c			enddo
c			endif
c			
c		    endif
		    ifmeanbad=0
c		      
c		      see if mean residual has gotten a lot bigger
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
     &			    ressd.gt.resdifcrit .and. errmax .gt. curResMin)
     &			    ifmeanbad=1
		      endif
		    endif
c		      
c		      if error greater than criterion after pass 1, or mean
c		      residual has zoomed on either pass, delete point for
c		      next round
c		      
		    if ((ipass .eq. 1 .and. errmax.gt.fitdistcrit)
     &			.or.ifmeanbad.eq.1)then
c		      write(*,'(i3,f7.2,4f10.5)')iobj,errmax,
c     &			  resmean(iobj+ibaseRes), curdif,resavg,ressd
		      wsave(iview+(iobjdo-1)*nvuall)=-1.
		      resmean(iobj+ibaseRes) = -1.
		      ibase=ibase_obj(iobj)
		      do ip=ibase+ipnearest(iobjdo)+1,
     &			  ibase+npt_in_obj(iobj)
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
		    write(*,115)ndel,(iobjdel(i),i=1,ndel)
115		    format(i4,' pts deleted for pass 2, conts:' ,(11i4))
		  else
		    write(*,116)ndel,(iobjdel(i),i=1,ndel)
116		    format(i4,' pts deleted, big mean residual, conts:',(9i4))
		    nadded=0
		    call tiltali(ifdidalign, resmean(1+ibaseRes),iview)
		  endif
		endif
	      endif
	      ipass=ipass+1
	      call flush(6)
	    enddo
	    ivseq=ivseq+1
	    ibaseRes = ibaseRes + maxObjOrig
	    call flush(6)
	  enddo
c	    
c	    Report total missing at end of pass
c	    
	  if (mod(iseq, 2) .eq. 0) then
	    misstot = 0
	    do i = 1, nobjdo
	      call countMissing(iobjseq(i), nvuall, izexclude,
     &		  nexclude, missing, listz, nlistz)
	      misstot=misstot+nlistz
	    enddo
	    write(*,121)areaObjStr,listseq(iseq), iseqPass, nobjdo,misstot
121	    format('For ',a,i3,', round',i3,':',i3,
     &		' contours, points missing =',i5)
	  endif
	enddo
c	  
c	  output lists of missing points, except for excluded sections
c	  
	write(*,118)
118	format(/,'Obj cont:  Views on which points are missing')
	misstot=0
	do iobj=1,maxObjOrig

	  call countMissing(iobj, nvuall, izexclude, nexclude, missing,
     &	      listz, nlistz)
	  if(nlistz.ne.0)then
	    call objtocont(iobj,obj_color,imodobj,imodcont)
	    write(*,117)imodobj,imodcont
117	    format(i2,i4,': ',$)
	    call wrlist(listz,nlistz)
	  endif
	  misstot=misstot+nlistz
	enddo
	print *,'Total points missing =',misstot
c
	call repack_mod
c       
c       convert index coordinates back to model coordinates
c       
	call irtorg(1,xorig,yorig,zorig)
	call irtdel(1,delta)
	do i=1,n_point
	  p_coord(1,i)=delta(1)*p_coord(1,i)-xorig
	  p_coord(2,i)=delta(2)*p_coord(2,i)-yorig
	  p_coord(3,i)=delta(3)*p_coord(3,i)-zorig
	enddo
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
99	call errorexit('ERROR READING IMAGE FILE',0)
	end

	subroutine errorexit(message, iflocal)
	implicit none
	integer*4 iflocal
	character*(*) message
	print *
	print *,'ERROR: BEADTRACK - ', message
	call exit(1)
	end

c	
c
c	  $Log$
c	  Revision 3.15  2005/04/26 18:45:08  mast
c	  Fixed to work with fewer than 8 points with default params
c	
c	  Revision 3.14  2005/04/11 19:11:28  mast
c	  Make it ignore multiple objects when there are only two views
c	
c	  Revision 3.13  2005/04/09 04:28:34  mast
c	  Changed default rotation mapping to 1 for full compatibility
c	
c	  Revision 3.12  2005/04/08 15:26:06  mast
c	  Fixed a line toolong
c	
c	  Revision 3.11  2005/04/08 04:21:59  mast
c	  Only drop outliers if fit is based on tiltalign positions
c	
c	  Revision 3.10  2005/04/07 04:15:41  mast
c	  Fixing some output
c	
c	  Revision 3.9  2005/04/07 03:56:31  mast
c	  New version with local tracking, new mapping, outliers, etc.
c	
c	  Revision 3.6  2003/06/21 00:39:26  mast
c	  Changed to use new version of get_tilt_angles
c	
c	  Revision 3.5  2003/05/20 23:42:09  mast
c	  Add space before wrlist output
c	
c	  Revision 3.4  2003/04/11 17:28:42  mast
c	  added cgx, cgy to tltcntrl common to make them available to tiltali
c	
c	  Revision 3.3  2002/07/28 22:55:53  mast
c	  Scale model coordinates correctly in Z; standardize error output
c	
c	  Revision 3.2  2002/05/07 02:01:33  mast
c	  Changes to accommodate distinction in tiltalign between views in
c	  solution and views in file
c	
c	  Revision 3.1  2002/01/07 22:35:15  mast
c	  Increased dimension for centroid calculation pixel lists, and added
c	  checks to catch errors in future
c

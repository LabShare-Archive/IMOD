* * * * * * BEADTRACK * * * * * *
c	  
c	  BEADTRACK will track selected fiducial gold beads through a series of
c	  tilted views.  It takes a "seed" model, where each bead of choice
c	  is marked with at least a single point on a view near zero tilt.  It
c	  tracks each bead as far as possible and puts out a new model.
c	  
c	  The program works from one view to the next, using the existing data
c	  about bead positions to deduce a projected position for each bead on
c	  the next view to be processed.  After bead positions are available
c	  on enough views, the program will use tilt alignment to estimate the
c	  3D position of each bead, and use this position to project a position
c	  on the next view.  The program searches for beads from the center
c	  outward.  Before looking for a particular bead, it uses the positions
c	  of beads already found on that view to refine the projected position.
c	  It will adjust by a shift if there are 3 points (or at least one
c	  point from the seed model), by shift and rotation if there are 4 or 5
c	  points, or by shift, rotation, and a size change if there are more
c	  than 5 points.
c	  
c	  The program will do one or two passes for each view.  On the first
c	  pass, it uses an average of the current bead over the nearest views
c	  for which positions have already been identified.  It cross-
c	  correlates that average with a box at the expected position of the
c	  bead and derives a bead position from the peak of the correlation.
c	  It then calculates the centroid and the integral of the density at
c	  that position.  To do so, it takes the average of pixels more than
c	  a certain radius away, subtracts this background value from the
c	  pixels within that radius, and uses pixels above the background to
c	  calculate a centroid and an integral.  If the integral is too low, or
c	  if the putative position is too far from the expected position, the
c	  program then attempts a "rescue".  The criterion for a rescue based
c	  on distance is set by the user.  The criterion for a rescue based on
c	  density is set by the mean value of the bead integral on previous
c	  views.  Specifically, the criterion is the minimum of a certain
c	  fraction of the mean and a certain number of standard deviations
c	  below the mean.  The latter parameters are also set by the user.
c	  To rescue, the program starts at the expected bead position and
c	  examines every position in a series of progressively wider circles
c	  around that point.  It calculates an integral for each position, and
c	  when it first finds an integral above a relaxed criterion value, it
c	  takes that position as the bead position.  The criterion can be
c	  relaxed by different factors for "density" and "distance" rescues.
c	  
c	  After the first pass, the program does a tilt alignment and uses two
c	  different tests for whether to  eliminate a point on the current
c	  view.  One test is whether the residual for the point is greater
c	  than a user-specifed limit ("criterion for rescue after fitting").
c	  The other test is whether the mean residual for that bead, averaged
c	  over all currently available views, has just increased by an unusual
c	  amount.  It finds the mean and SD of previous increases in this mean
c	  residual, and tests whether the latest increase exceeds the mean by
c	  a user-specified criterion number of SD's.
c	  
c	  If any points were eliminated by these tests, or if any points failed
c	  to be found, the program does a second pass.  On this pass there is
c	  no correlation, just an attempted rescue at the expected position
c	  (possibly refined because of the additional information about other
c	  beads on this pass).  The maximum distance for this rescue is set by
c	  the user.  After the second pass, the program does another tilt
c	  alignment and tests only the behavior of the increase in mean
c	  residual for each point.  If that increase is too great, a point is
c	  eliminated for good.
c	  
c	  The program will leave gaps in the model rather than insert bad
c	  points.  It will try to resume tracking after a gap, but only if the
c	  gap is not larger than a user-defined limit.
c	  
c	  The user can choose whether or not the program will fill in gaps in
c	  the incoming seed model.  If the image stack has sizable shifts that
c	  could prevent accurate tracking, then one should use one of the beads
c	  as a "pioneer".  It is not necessary to model that bead on every
c	  view, just on the views before and after a large shift.  However, one
c	  should be sure to have the program fill in gaps in this case.  Also,
c	  one can place one point for a bead on the view near zero tilt, then
c	  place a few points on distant views where one can anticipate that the
c	  program will have trouble tracking through (e.g., where another bead
c	  crosses, or where the bead goes too close to the edge).
c	  
c	  For each view and pass, the program outputs the results from the
c	  linear fit between actual and projected positions, the tilt
c	  alignment total error, and which beads have been deleted or added
c	  back. After doing all views, the program outputs a summary of which
c	  views are missing for each bead.
c	  
c	  INPUTS TO THE PROGRAM:
c	  
c	  Image file name
c
c	  Piece list file name if the file is montaged (or if it is digitized 
c	  .  out of order), otherwise enter a blank line
c	  
c	  Name of model file with starting bead coordinates
c	  
c	  Name of output model file
c	  
c	  List of views to skip over.  Model objects will pass through 
c	  .  these views; points will need to be added by hand afterwards. 
c	  .  Ranges may be entered, e.g. 1,4-6.  Views are numbered from 1.
c	  
c	  Angle of rotation of the tilt axis in the images; specifically, the
c	  .  angle from the vertical to the tilt axis (counterclockwise
c	  .  positive).
c
c	  Number of sets of views to treat separately from the main set of
c	  .  views when automapping the tilt angle and magnification
c	  .  variables.  Enter 0 if all views can be treated together when
c	  .  automapping.  These sets would typically be lists of views
c	  .  that were reshot.
c	  
c	  IF a number other than 0 was entered, next enter one line for each
c	  .   separate set, giving a list of the views included in that set.
c	  .   Ranges are allowed here.
c
c	  -1 to enter individual tilt angle for each view, 1 to specify a
c	  .  starting and increment tilt, or 0 to read tilt angles from a file
c	  
c	  IF you entered 1, next enter the starting and incremental tilt angles
c	  IF you entered -1, enter the tilt angle of each view.
c	  IF you entered 0, enter name of file with tilt angles
c	  
c	  The default number of views to group together in solving for tilt
c	  .  angles, and the number of ranges of views that should have some
c	  .  grouping other than the default.  If a negative number of views
c	  .  is entered, then reshoot sets will NOT be segregated from the
c	  .  rest of the views in this default mapping.
c	  
c	  .  IF you entered a non-zero number of ranges to be treated
c	  .     separately, then for each such range, enter the starting and
c	  .     ending view number and the number of views that should be
c	  .     grouped in that range.  If a negative number of views is
c	  .     entered, then reshoot sets will NOT be segregated from the
c	  .     rest of the views in this range.
c	  
c	  The default number of views to group together in solving for
c	  .  magnifications, and the number of ranges of views to group in
c	  .  some other way.  If you enter a non-zero number of ranges, then
c	  .  for each one, enter starting and emding view numbers and group
c	  .  size.  Note that extensive grouping of tilt angle and
c	  .  magnification variables is desirable, but the grouping should be
c	  .  adjusted if there are known places where magnification or the
c	  .  deviation from the ideal tilt angle changes abruptly.
c	  
c	  Minimum number of views with bead positions available before trying
c	  .  to do a tilt alignment.  To skip the tilt alignment computations,
c	  .  set this to a number higher than the number of views.
c
c	  Radius for centroid calculation, and 0 if beads are darker or 1 if
c	  .  they are lighter than background.  The radius need not be a whole
c	  .  number; e.g., 4.5 is acceptable.
c	  
c	  1 to fill in gaps in the seed model, or 0 not to fill in gaps
c	  
c	  Maximum size of gap to create in the model.  If a bead cannot be 
c	  .  tracked through some views, the tracking may be resumed as long as
c	  .  the gap thus created is no larger than this amount.
c	  
c	  Minimum range of tilt angles for which data must be available before
c	  .  trying to find the angle of the tilt axis, and minimum range of
c	  .  angles required before trying to solve for tilt angles.  Suggested
c	  .  values are 10 and 20.
c	  
c	  X and Y dimensions of the box used to search for a bead 
c	  .  (32,32 suggested)
c	  
c	  Maximum number of views over which to average a bead (4 suggested)
c	  .  A running average is kept of the appearance of the bead over 
c	  .  the most recent views examined; this parameter specifies the 
c	  .  maximum number of views averaged.
c
c	  Number of positions to use for extrapolating the bead position to
c	  .  the next view, and minimum required to do extrapolation rather
c	  .  than simply taking the mean of positions on the last few views.
c	  .  Suggested values 7 and 5.
c	  
c	  Fraction of mean bead integral, and number of standard deviations
c	  .  below mean, to use as the criterion for when to attempt a rescue
c	  .  based on bead density.
c	  
c	  Distance in pixels away from expected position at which to attempt
c	  .  a rescue based on excessive distance
c	  
c	  Factors by which to adjust (relax) the density criterion when
c	  .  trying to rescue.  Enter one factor for density rescue and one for
c	  .  distance rescue.  A value of 1 does not relax the criterion.
c
c	  Criterion distance for deletion of a point after tilt alignment.
c	  .  Points with residuals greater than this amount will be deleted on
c	  .  the first pass, and a rescue search performed on the second pass.
c	  
c	  Factor by which to relax the density criterion on the second pass, 
c	  .  and maximum distance to search from the expected position on this
c	  .  pass.
c	  
c	  Maximum and minimum number of changes in mean residual to use in 
c	  .  finding the mean and SD of changes in the mean residual for a
c	  .  bead as more points have been added.  Suggested values 9 and 5.
c	  
c	  Minimum change in residual, and criterion number of SD's from the
c	  .  mean residual change, to require for deletion of apoint on pass 1
c	  .  or 2.
c
c	  David Mastronarde, 1995
c	  added tilt alignment, 10/6/97
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.7  2004/05/07 23:41:49  mast
c	  Fixed two uses of uninitialized variables
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
c
	include 'model.inc'
	include 'statsize.inc'
	include 'alivar.inc'
	parameter (maxbox=64,maxstor=10,npad=8)
	parameter (maxarr=(maxbox+2*npad)*(maxbox+2*npad+2))
	parameter (limpcl=50000,maxnbox=500,liminside=3000,limedge=1000)
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
	character*80 titlech

	COMMON //NX,NY,NZ
C   
	DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),TITLE(20),
     &      NXYZ2(3),MXYZ2(3),CELL2(6)
	real*4 cursum(maxbox*maxbox),boxes(maxbox*maxbox*maxnbox)
	real*4 boxtmp(maxbox*maxbox),tltall(limpcl)
	complex ARRAY(maxarr/2),BRRAY(maxarr/2)
C	  
	integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
	CHARACTER*80 FILIN,FILOUT,plfile,modelfile
	integer*4 idxin(liminside),idyin(liminside)
	integer*4 idxedge(limedge),idyedge(limedge)
	integer*4 listz(maxview),izexclude(maxview)
	character*9 dat
	character*8 tim
	logical exist,readw_or_imod
	data nxyzst/0,0,0/
C   
	EQUIVALENCE (NX,NXYZ)
c
	parameter (maxgrp=20)
	integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),ngsep
	common /mapsep/ ivsep,nsepingrp,ngsep
c
	real*4 tiltorig(maxview),gmagorig(maxview),rotorig(maxview)
	integer*4 mapalltilt(maxview),mapallmag(maxview),ivorig(maxview)
	integer*4 ivsolv(maxview),iobjali(maxreal)
	real*4 dxysav(2,maxview),xyzsav(3,maxreal)
	common /tltcntrl/nvuall,imintilt,mininview,minvtiltali,
     &	    randoaxis,randotilt, scalexy,xcen,ycen,cgx,cgy,xorig,
     &	    yorig,xdelt,ydelt, initxyzdone,nsolve,facm, ncycle,eps,
     &	    tiltorig,gmagorig,rotorig,mapalltilt,mapallmag,ivorig,
     &	    ivsolv,iobjali,dxysav,xyzsav
c	  
	real*4 xseek(maxreal),yseek(maxreal),wcrit(maxreal)
	integer*4 nws(maxreal),iffound(maxreal),iobjseq(maxreal)
	integer*4 ipnearest(maxreal),ipclose(maxview),izclose(maxview)
	integer*4 incore(maxstor,maxreal),ipnearsav(maxreal)
	integer*4 iobjdel(maxreal)
	real*4 wsave(maxprojpt),xr(msiz,maxreal),seqdist(maxreal)
	real*4 resmean(maxprojpt),prevres(maxview)
	logical missing(0:maxview)

	real*4 delta(3)
C   
	real*4 xf(2,3)
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
	eps=0.00002				!was .00001
c
        write(*,'(1x,a,$)')'Image input file: '
	READ(5,101)FILIN
101     format(a)
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C   
	write(*,'(1x,a,$)')'Piece list file if image is montage,'//
     &	    ' otherwise Return: '
	read(*,101)plfile
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
70	write(*,'(1x,a,$)')'Input model file: '
	read(*,'(a)')modelfile
c
	exist=readw_or_imod(modelfile)
	if(.not.exist)go to 70
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
	write(*,'(1x,a,$)')'Output model file: '
	read(*,'(a)')modelfile
c
	filout=' '
c	  write(*,'(1x,a,$)')'Image output file for boxes (Return none): '
c	  READ(5,101)FILout
c
	write(*,'(1x,a,$)')'List of views to skip over: '
	call rdlist(5,izexclude,nexclude)
c
c	  new parameters for tilt alignment
c
	write(*,'(1x,a,$)')
     &	    'Initial angle of rotation in projection plane: '
	read(5,*)rotstart
	iaxtilt=2
	if(abs(rotstart).ge.45.)iaxtilt=1
c	  
c	  Get list of views to treat separately in automapping
c
	write(*,'(1x,a,/,a,$)') 'For automapping tilt and mag,'
     &	    //' enter the number of sets of views to treat separately'
     &	    //' from the main set of views (otherwise enter 0): '
	read(5,*)ngsep
	do ig=1,ngsep
	  write(*,'(1x,a,i3,a,$)')'List of views in set',ig,
     &	      '(ranges OK): '
	  call rdlist(5,ivsep(1,ig),nsepingrp(ig))
	enddo
c	  
	call get_tilt_angles(nvuall,3,tltall, limpcl, 0)
c	  
c	  DNM 5/3/02: accommodate changes to tiltalign by setting up the
c	  mapping arrays, adding to automap call
c	  
	nfileviews=nvuall
	do i=1,nvuall
	  mapfiletoview(i)=i
	  mapviewtofile(i)=i
	enddo
c
	print *,'Specify grouping for mapping of tilt variables'
	call setgrpsize(tltall,nvuall,0.,boxes)
	call automap(nvuall,mapalltilt,boxes,mapfiletoview,nfileviews,
     &	    0,0,' ',' ')
c
	print *,'Specify grouping for mapping of magnification variables'
	call automap(nvuall,mapallmag,boxes,mapfiletoview,nfileviews,
     &	    0,0,' ',' ')
c
	write(*,'(1x,a,$)')'Minimum # of views required for tilt'//
     &	    ' aligning: '
	read(5,*)minvtiltali
c	  
	write(*,'(1x,a,$)')'Radius for centroid calculation, 0 for '//
     &	    'dark or 1 for light beads: '
	read(5,*)cgrad,ifwhite
	ipolar=-1
	if(ifwhite.ne.0)ipolar=1
c	  
	write(*,'(1x,a,$)')'1 to fill in gaps, 0 not to: '
	read(5,*)iffillin
c	  
	write(*,'(1x,a,$)')'Maximum gap to create: '
	read(5,*)maxgap
c
	write(*,'(1x,a,/,a,$)')'Minimum ranges of tilt angles required'
     &	    //' for finding tilt axis',' and for finding tilt angles: '
	read(5,*)randoaxis,randotilt
c
	write(*,'(1x,a,i4,a,$)')'X and Y dimensions of box (max'
     &	    ,maxbox,'): '
	read(5,*)nxbox,nybox
	nxpad=nxbox+2*npad
	nypad=nybox+2*npad
	npixbox=nxbox*nybox
	nxpdim=nxpad+2
c
	write(*,'(1x,a,$)')
     &	    'Maximum number of beads to average for correlation: '
	read(5,*)maxsum
c	  
c	write(*,'(1x,a,$)')'1 or 2 for tilt around X or Y axis, or '//
c     &	    '0 not to assume tilts: '
c	read(5,*)iaxtilt
c
	write(*,'(1x,a,$)')'# of points to fit for extrapolation'
     &	    //', minimum # required: '
	read(5,*)nfit,minfit
c
	tiltfitmin=15.
c	write(*,'(1x,a,$)')'Minimum tilt angle required for '//
c     &	    'extrapolation fitting to cosine/sine: '
c	read(5,*)tiltfitmin
c	  
	write(*,'(1x,a,$)')'Fraction of mean and # of SDs for'//
     &	    ' density criteron for rescue: '
	read(5,*)fraccrit,sdcrit
	write(*,'(1x,a,$)')
     &	    'Distance criterion for rescue (- for full report): '
	read(5,*)distcrit
	iftrace=0
	if(distcrit.lt.0.)then
	  iftrace=1
	  distcrit=-distcrit
	endif
c
	write(*,'(1x,a,$)')'Fractions to relax criterion during '//
     &	    'density and distance rescue: '
	read(5,*)relaxint,relaxdis
c	  
	write(*,'(1x,a,$)')
     &	    'Criterion distance for rescue after fitting: '
	read(5,*)fitdistcrit
c	  
	write(*,'(1x,a,$)')
     &	    'Relaxation of density criterion, maximum rescue distance: '
	read(5,*)relaxfit,radmaxfit
	write(*,'(1x,a,$)')'Max and min # of residual changes to use '//
     &	    'to get mean and SD: '
	read(5,*)maxresid,minresid
	write(*,'(1x,a,$)')'Minimum residual change & criterion # of'//
     &	    ' SD from mean for deletion of pt: '
	read(5,*)resdifmin,resdifcrit
c
	do iv=1,nvuall
	  rotorig(iv)=rotstart
	  tiltorig(iv)=tltall(iv)
	  gmagorig(iv)=1.
	  lintilt(iv)=0
	  frctilt(iv)=1.
	  lingmag(iv)=0
	  frcgmag(iv)=1.
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
c
	minendz=200
	do i=1,maxview
	  ivsolv(i)=0
	enddo
	do iobj=1,max_mod_obj
	  ninobj=npt_in_obj(iobj)
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
	      ivsolv(iz)=ivsolv(iz)+1
	    enddo
	    minendz=min(minendz,iz)
	  endif
	enddo
	maxnpt=0
	tiltmin=200.
	do i=1,nvuall
	  if(ivsolv(i).gt.maxnpt)then
	    maxnpt=ivsolv(i)
	    tiltmin=abs(tltall(i))
	    imintilt=i
	  elseif(ivsolv(i).eq.maxnpt.and.abs(tltall(i)).lt.tiltmin)then
	    tiltmin=abs(tltall(i))
	    imintilt=i
	  endif
	enddo
c
c	  figure out an order for the points: from the center outwards
c	  
	nobjdo=0
	xsum=0.
	ysum=0.
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
	      endif
	    enddo
	    xsum=xsum+xyzsav(1,iobj)
	    ysum=ysum+xyzsav(2,iobj)
	    xyzsav(3,iobj)=0.
	  endif
	enddo
	cgx=xsum/nobjdo
	cgy=ysum/nobjdo
	do i=1,nobjdo-1
	  do j=i+1,nobjdo
	    if(seqdist(i).gt.seqdist(j))then
	      tmp=seqdist(i)
	      seqdist(i)=seqdist(j)
	      seqdist(j)=tmp
	      itmp=iobjseq(i)
	      iobjseq(i)=iobjseq(j)
	      iobjseq(j)=itmp
	    endif
	  enddo
	enddo
c
	if(nobjdo.gt.maxreal.or.nvuall*max_mod_obj.gt.maxprojpt)
     &	    call errorexit(
     &	    'TOO MANY OBJECTS OR VIEWS AND OBJECTS FOR ARRAYS',0)
c	  
	if(nobjdo*maxsum*npixbox.gt.maxnbox*maxbox**2) call errorexit(
     &	    'TOO MUCH SUMMING IN BOXES TOO LARGE FOR TOO MANY POINTS',0)
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
	  CALL IMOPEN(2,FILOUT,'NEW')
	  NXYZ2(1)=Nxbox
	  NXYZ2(2)=NYbox
	  NXYZ2(3)=1
	  MXYZ2(1)=NXbox
	  MXYZ2(2)=NYbox
	  MXYZ2(3)=1
	  CELL2(1)=NXbox
	  CELL2(2)=NYbox
	  CELL2(3)=1
	  CELL2(4)=90.
	  CELL2(5)=90.
	  CELL2(6)=90.
C   
	  call time(tim)
	  call date(dat)
c
c 7/7/00 CER: remove the encodes
c
c	  ENCODE(80,301,TITLE)dat,tim
	  write(titlech,301) dat,tim
301	  FORMAT('Boxes',32x,a9,2x,a8)
	  read(titlech,'(20a4)')(title(kti),kti=1,20)
	  CALL ICRHDR(2,NXYZ2,MXYZ2,0,TITLE,0)
	  call itrlab(2,1)
	  CALL IALCEL(2,CELL2)
	  nzout=0
	endif
c	  
c	  
c	  
	nadded=1
	initxyzdone=0
	ivstr=imintilt-1
	ivend=1
	ivseq=1
	do idir=-1,1,2
	  do iview=ivstr,ivend,idir
	    iznext=iview-1
	    ifexclude=0
	    do iexcl=1,nexclude
	      if(iview.eq.izexclude(iexcl))ifexclude=1
	    enddo
	    if(ifexclude.eq.0)then
c	    
c		first see how many are done on this view
c
	      ntodo=0
	      do iobjdo=1,nobjdo
		iobj=iobjseq(iobjdo)
		ninobj=npt_in_obj(iobj)
		nclose=0
		iffound(iobjdo)=0
		ibase=ibase_obj(iobj)
c		  
c		  find closest ones within gap distance, find out if already
c		  done, mark as a 2 to protect them
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
     &			nx, ny, nxbox,nybox,xbox,ybox,izbox,ix0,ix1,
     &			iy0,iy1,ipcz)
		    if(ipcz.ge.0)then
		      nclose=nclose+1
		      ipclose(nclose)=ip
		      izclose(nclose)=izv
		    endif
		  endif
		enddo
		if(nclose.eq.0.and.iffound(Iobjdo).eq.0)
     &		    iffound(iobjdo)=-1
		if(nclose.gt.0.and.iffound(iobjdo).eq.0)then
		  if(iffillin.eq.0)then
c
c		      if not filling in, just make sure this section is beyond
c		      last point
c
		    ipchk=1
		    if(idir.eq.1)ipchk=ninobj
		    izchk=nint(p_coord(3,object(ibase+ipchk)))+1
		    if(idir*(izchk-iview).gt.0)iffound(iobjdo)=-1
		  endif
		  if(iffound(iobjdo).eq.0)then
c		      
c		      order feasible points by distance
c
		    ntodo=ntodo+1
		    do ic=1,nclose-1
		      do jc=ic+1,nclose
			if(abs(izclose(jc)-iview).lt.
     &			    abs(izclose(ic)-iview)) then
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
c		      see if boxes in memory match, free them if not
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
c		      now load ones that are needed
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
			izbox=p_coord(3,ipt)
			call find_piece(ixpclist,iypclist,izpclist,
     &			    npclist, nx, ny, nxbox,nybox,xbox,ybox,
     &			    izbox,ix0,ix1, iy0,iy1,ipcz)
			call imposn(1,ipcz,0)
			call irdpas(1,boxtmp,nxbox,nybox, ix0,ix1,iy0,
     &			    iy1,*99)
			xt=nint(xbox)-xbox
			yt=nint(ybox)-ybox
			call qdshift(boxtmp,boxes(1+(nextbox-1)*npixbox)
     &			    ,nxbox, nybox,xt,yt)
			if(wsave(izbox+1+(iobjdo-1)*nvuall).lt.0.)then
			  xtmp=0.
			  ytmp=0.
			  call calcg(boxes(1+(nextbox-1)*npixbox),nxbox,
     &			      nybox,xtmp, ytmp, idxin, idyin,ninside,
     &			      idxedge, idyedge, nedge, ipolar,wsum)
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
c		      now try to do tiltalign if possible
c
		if(nadded.ne.0)call tiltali(ifdidalign,
     &		    resmean(1+(ivseq-1)*max_mod_obj),iview)

c		    
c		    get tentative tilt, rotation, mag for current view
c		    
		if(ifdidalign.gt.0)then
		  ivdel=200
		  do iv=1,nview
		    if(abs(ivorig(iv)-iview).lt.ivdel)then
		      ivdel=abs(ivorig(iv)-iview)
		      ivnear=ivorig(iv)
		    endif
		  enddo
		  ivuse=ivnear
		  if(ivsolv(iview).gt.0)ivuse=iview
		  tiltcur=tiltorig(ivuse)+tltall(iview)-tltall(ivuse)
		  gmagcur=gmagorig(ivuse)
		  rotcur=rotorig(ivuse)
		  dxcur=dxysav(1,ivuse)
		  dycur=dxysav(2,ivuse)
		  cosr=cosd(rotcur)
		  sinr=sind(rotcur)
		  cost=cosd(tiltcur)
		  sint=sind(tiltcur)
		  a = gmagcur * cost * cosr
		  b = - gmagcur * sinr
		  c = gmagcur * sint * cosr
		  d = gmagcur * cost * sinr
		  e = gmagcur * cosr
		  f = gmagcur * sint * sinr
		endif
c
c		  now get projected positions
c		  
		do iobjdo=1,nobjdo
		  indr=0
		  if(ifdidalign.eq.1)then
		    do ipt=1,nrealpt
		      if(iobjseq(iobjdo).eq.iobjali(ipt))
     &			  indr=iobjseq(iobjdo)
		    enddo
		  endif
		  if(indr.ne.0)then
c		      
c		      there is a 3D point for it: so project it
c		      
		    xseek(iobjdo)=a*xyzsav(1,indr)+b*xyzsav(2,indr)+
     &			c*xyzsav(3,indr)+dxcur+xcen
		    yseek(iobjdo)=d*xyzsav(1,indr)+e*xyzsav(2,indr)+
     &			f*xyzsav(3,indr)+dycur+ycen
		  else
		    call nextpos(iobjseq(iobjdo),ipnearest(iobjdo),idir,
     &			iznext,tltall,nfit, minfit,iaxtilt,tiltfitmin,
     &			xseek(iobjdo),yseek(iobjdo))
		  endif
		enddo
c		  
c		  build list of points already found for fitting; put
c		  actual positions in 4 and 5 for compatibiity with xfmodel
c		  
		ndat=0
		do iobjdo=1,nobjdo
		  if(iffound(iobjdo).gt.0)then
		    ndat=ndat+1
		    xr(1,ndat)=xseek(iobjdo)-xcen
		    xr(2,ndat)=yseek(iobjdo)-ycen
		    ipt=object(ibase_obj(iobjseq(iobjdo))+
     &			ipnearest(iobjdo))
		    xr(4,ndat)=p_coord(1,ipt)-xcen
		    xr(5,ndat)=p_coord(2,ipt)-ycen
		    xr(6,ndat)=iobjdo
		  endif
		enddo
c		  
c		  loop through points, refining projections before search
c		  
		if(ipass.eq.1)npioneer=ndat
		nadded=0
		do iobjdo=1,nobjdo
		  if(iffound(iobjdo).eq.0.or.iffound(iobjdo).eq.1)then
c		      
c		      get w criterion
c		      
		    navg=0
		    wsum=0.
		    wsumsq=0.
		    idif=1
		    do while(idif.le.maxwavg.and.navg.lt.maxwavg)
		      do idirw=-1,1,2
			itry=iview+idirw*idif
			if(itry.gt.0.and.itry.le.nvuall)
     &			    then
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
		    if(npioneer.gt.0.or.ndat.ge.limpshft)then
		      ifrotrans=0
		      iftrans=1
		      if(ndat.ge.limpstr)then
			iftrans=0
		      elseif(ndat.ge.limpmag)then
			ifrotrans=2
		      elseif(ndat.ge.limprot)then
			ifrotrans=1
		      endif
		      call findxf(xr,ndat,iftrans,ifrotrans,0,xf,
     &			  devavg,devsd,devmax,ipntmax)
		      call xfapply(xf,xcen,ycen,xseek(iobjdo),
     &			  yseek(iobjdo), xnext,ynext)
		    endif
c		      
c		      
c		      
		    call find_piece(ixpclist,iypclist,izpclist,
     &			npclist, nx, ny, nxbox,nybox,xnext,ynext,
     &			iznext,ix0,ix1, iy0,iy1,ipcz)
		    if(ipcz.ge.0)then
c			
c			get image area and pad into array
c			
		      call imposn(1,ipcz,0)
		      call irdpas(1,boxtmp,nxbox,nybox,ix0,ix1,iy0,
     &			  iy1 ,*99)
		      if(ipass.eq.1)then
			call padfill(boxtmp,nxbox,nybox,array,nxpdim,
     &			    nxpad, nypad)
			call setmeanzero(array,nxpdim,nypad,nxpad,
     &			    nypad)
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
			call padfill(cursum,nxbox,nybox,brray,nxpdim,
     &			    nxpad, nypad)
			call setmeanzero(brray,nxpdim,nypad,nxpad,
     &			    nypad)
c			
c			  correlate the two
c			
			call todfft(array,nxpad,nypad,0)
			call todfft(brray,nxpad,nypad,0)
			do jx=1,nypad*(nxpad+2)/2
			  array(jx)=array(jx)*conjg(brray(jx))
			enddo
			call todfft(array,nxpad,nypad,1)
c			  
c			  find peak
c			
			call peakfind(array,nxpdim,nypad,xpeak,ypeak,
     &			    peak)
			call calcg(boxtmp,nxbox,nybox,xpeak,ypeak,
     &			    idxin, idyin,ninside,idxedge,idyedge,
     &			    nedge,ipolar,wsum)
			dist=sqrt(xpeak**2+ypeak**2)
		      endif
		      if(nws(iobjdo).gt.2.and.(wsum.lt.wcrit(iobjdo)
     &			  .or. dist.gt.distcrit .or. ipass.eq.2))then
c			  
c			  rescue attempt; search concentric rings from the
c			  center of box and take the first point that goes
c			  above the relaxed wsum criterion
c			  
			if(ipass.eq.1)then
			  if(dist.gt.distcrit)then
			    relax=relaxdis
c			    write(*,102)'distance',iznext,dist,wsum,wavg,wsd
c102			    format(' Rescue-',a,', sec',i4,', dist=',f5.1,
c	   &			    ', dens=',f9.0,', mean,sd=',f9.0,f7.0)
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
     &			    idxin, idyin,ninside,idxedge,idyedge,
     &			    nedge,ipolar,radmax,relax*wcrit(iobjdo),
     &			    wsum)
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
     &			    write(*,'(2i5,4f7.1,2f12.0)')nzout,iznext,
     &			    xnext, ynext, xpos,ypos,peak/ninsum,wsum
c			  
c			  add point to model
c			  
			call add_point(iobj,ipnearest(iobjdo),
     &			    xpos,ypos,iznext)
			nadded=nadded+1
			iobjdel(nadded)=iobj
c			  
c			  mark as found, add to data matrix for alignment
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
			  do iboxo=1,2
			    call isetdn(boxtmp,nxbox,nybox,modebox,1,
     &				nxbox, 1, nybox,tmin,tmax,tmean)
			    call iwrsec(2,boxtmp)
			    boxmin=min(boxmin,tmin)
			    boxmax=max(boxmax,tmax)
			    boxsum=boxsum+tmean
			    nzout=nzout+1
			    if (iboxo.eq.1)then
			      do i=1,npixbox
				boxtmp(i)=cursum(i)
			      enddo
			    endif
			  enddo
			endif
		      endif
		    endif
		  endif
		enddo
c		  
c		    get a final fit and a new tiltalign, then find a maximum
c		    error for each pt
c		  
		if(ipass.eq.2)write(*,114)nadded,(iobjdel(i),i=1,nadded)
114		format(i4,' points added on pass 2, objects:',(12i3))
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
		  call findxf(xr,ndat,iftrans,ifrotrans,1,xf,
     &		      devavg,devsd,devmax,ipntmax)
		  write(*,113)iview,ipass,ndat,devavg,devsd,devmax
113		  format('view',i4,', pass',i2,',',i4,
     &		      ' points, mean, sd, max dev:' ,3f8.2)
		endif
c		  
c		  do tiltali if anything changed, so can get current residuals
c
		if(nadded.gt.0)call tiltali(ifdidalign,
     &		    resmean(1+(ivseq-1)*max_mod_obj),iview)
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
		      if(ipass.eq.1)then
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
			  ivlook=ivsolv(iview)
			  ipt=irealstr(indr)
			  do while(ipt.lt.irealstr(indr+1).and.
     &			      errmax.eq.0.)
			    if(isecview(ipt).eq.ivlook)errmax=scalexy*
     &				sqrt(xresid(ipt)**2+yresid(ipt)**2)
			    ipt=ipt+1
			  enddo
			endif
c			
c			  When no tiltalign available, just redo the point with
c			  highest deviation?  No, projections could be lousy.
c
c			if(ifdidalign.eq.0.and.ndat.ge.limpshft)then
c			  do i=1,ndat
c			    if(nint(xr(6,i)).eq.iobjdo.and.xr(13,i).gt.
c     &				0.99*devmax)errmax=xr(13,i)
c			  enddo
c			endif
c			
		      endif
		      ifmeanbad=0
c			
c			see if mean residual has gotten a lot bigger
c
		      if(ifdidalign.eq.1)then
			iscur=ivseq-1
			nprev=0
			do while(nprev.le.maxresid.and.iscur.gt.0)
			  if(resmean(iobj+(iscur-1)*max_mod_obj).gt.0.)then
			    nprev=nprev+1
			    prevres(nprev)=resmean(iobj+(iscur-1)*max_mod_obj)
			  endif
			  iscur=iscur-1
			enddo
			if(nprev.gt.minresid)then
			  curdif=resmean(iobj+(ivseq-1)*max_mod_obj)-prevres(1)
			  do i=1,nprev-1
			    prevres(i)=prevres(i)-prevres(i+1)
			  enddo
			  call avgsd(prevres,nprev-1,resavg,ressd,
     &			      ressem)
c			  write(*,'(i3,4f10.5)')iobj,resmean(iobj+
c     &			      (ivseq-1)*max_mod_obj), curdif,resavg,ressd
			  if(curdif.gt.resdifmin.and.(curdif-resavg)/
     &			      ressd.gt.resdifcrit)ifmeanbad=1
			endif
		      endif
c			
c			  if error greater than criterion after pass 1, or mean
c			  residual has zoomed on either pass, delete point for
c			  next round
c
		      if(errmax.gt.fitdistcrit.or.ifmeanbad.eq.1)then
			wsave(iview+(iobjdo-1)*nvuall)=-1.
			ibase=ibase_obj(iobj)
			do ip=ibase+ipnearest(iobjdo)+1,
     &			    ibase+npt_in_obj(iobj)
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
115		      format(i4,' points deleted for pass 2, objects:'
     &			  ,(12i3))
		    else
		      write(*,116)ndel,(iobjdel(i),i=1,ndel)
116		      format(i4,' points deleted, big mean residual, ',
     &			  'objects:',(10i3))
		      nadded=0
		      call tiltali(ifdidalign,
     &			  resmean(1+(ivseq-1)*max_mod_obj),iview)
		    endif
		  endif
		endif
		ipass=ipass+1
	      enddo
	    endif
	    ivseq=ivseq+1
	    call flush(6)
	  enddo
	  ivstr=minendz+1
	  ivend=nvuall
	enddo
c	  
c	  output lists of missing points, except for excluded sections
c	  
	write(*,118)
118	format(/,'Obj cont:  Views on which points are missing')
	misstot=0
	do iobj=1,max_mod_obj
	  ninobj=npt_in_obj(iobj)
	  if(ninobj.gt.0)then
	    ibase=ibase_obj(iobj)
	    do i=1,nvuall
	      missing(i)=.true.
	    enddo
	    do i=1,nexclude
	      missing(izexclude(i))=.false.
	    enddo
	    do ip=1,ninobj
	      iz=nint(p_coord(3,object(ibase+ip)))+1
	      missing(iz)=.false.
	    enddo
	    nlistz=0
	    do i=1,nvuall
	      if(missing(i))then
		nlistz=nlistz+1
		listz(nlistz)=i
	      endif
	    enddo
	    if(nlistz.ne.0)then
	      call objtocont(iobj,obj_color,imodobj,imodcont)
	      write(*,117)imodobj,imodcont
117	      format(i2,i4,': ',$)
	      call wrlist(listz,nlistz)
	    endif
	    misstot=misstot+nlistz
	  endif
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
	  boxmean=boxsum/nzout
	  cell2(3)=nzout
	  mxyz2(3)=nzout
	  nxyz2(3)=nzout
	  CALL IALCEL(2,CELL2)
	  call ialsam(2,nxyz2)
	  call ialsiz(2,nxyz2,nxyzst)
	  CALL IWRHDR(2,TITLE,1,BOXMIN,BOXMAX,BOXMEAN)
	  CALL IMCLOSE(2)
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
	

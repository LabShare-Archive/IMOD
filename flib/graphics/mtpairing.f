* * * * * MTPAIRING * * * * *
c	  
c	  MTPAIRING calculates the length over which MTs are paired with each
c	  other in 3-D, and allows one to assign MTs to new objects ("recolor"
c	  them) based on these pairing lengths.  It can also assign polarities
c	  to MTs based on the positions of their endpoints in Z, and allows
c	  one to recolor MTs based on these polarities.  The program combines
c	  features of MTOVERLAP and GENHSTPLT.  It also has several
c	  exploratory features that are not documented because they were not
c	  particularly useful.
c	  
c	  MTs are considered "paired" when they are within a certain distance
c	  of each other in the X/Y plane.  Typically, you would set this
c	  distance to be the upper limit of the peak from a neighbor density
c	  analysis.  The absolute pairing length for a pair of MTs is the
c	  total length in Z over which they are within that distance of each
c	  other.  A fractional pairing length is also computed; this is the
c	  pairing length divided by the length over which the two MTs appear
c	  in the same sections.
c
c	  Before running the program, you must figure out how to specify which
c	  MT's are in a bundle.  If all of the MT's in a model belong to
c	  one bundle, then this task is easy.  If you have several bundles in
c	  one model, then you have several alternatives.  One is to determine
c	  the lower and upper X, Y and Z coordinates of a box, such that the
c	  bundle consists of all MT's that contain at least one point within
c	  the box.  Another way is to make a model contour within the plane
c	  of one section to serve as a boundary contour.  This contour,
c	  together with a lower and upper Z coordinate, specifies a "cylinder",
c	  and this program will include in the bundle any MT with at least one
c	  point inside this cylinder.  The most elaborate way is to make a
c	  series of model contours for boundary contours in different sections.
c	  The program will then include in the bundle any MT that is included
c	  within any one of the contours.
c	  
c	  When you enter X, Y or Z coordinates for this purpose, they must be
c	  index coordinates of the image file.  That is, X and Y values must
c	  be in terms of pixel coordinates, and Z values must be in units of
c	  the original section numbers, before adjustment for tilt or scaling
c	  by section thickness.
c
c	  If the sections were significantly tilted during microscopy, the
c	  program can adjust for these tilts given the proper information.
c	  Prepare a file in which the first line shows the Z value and the
c	  tilt of the first tilted section (or of the first section, if that
c	  one was tilted), and each successive line shows the Z value and tilt
c	  for each section on which tilt was changed.  Z values should occur in
c	  ascending order.
c
c	  When you start the program, you will have to make a standard series
c	  of entries until you get the first display.  From there, you can
c	  select a number of options to loop back and change those entries.
c	  Initial entries in order are:
c	  
c	  0 for plots in a Plax window or 1 for plots only on the terminal.
c	  Note that if you choose to use terminal plots, you will need to
c	  specify that option each time that you do a plot.
c	  
c	  Name of command file to take entries from, or Return to continue
c	  making entries from the keyboard. The program can read entries from
c	  a file instead of from the keyboard, then switch back to keyboard
c	  input if the file ends with the appropriate entry. 
c	  
c	  Number of bundles to read from model files, or 0 if the entries
c	  specifying all of the bundles are in yet another file.
c
c	  IF you enter a positive number, then enter for each bundle:
c	  
c	  .  Name of model file with bundle in it, or Return to use same file
c	  .  as previous bundle
c	  
c	  .  IF you enter the name of file, make the following 1-3 entries:
c
c	  .     Name of file with information on tilt angles, or Return if
c	  .     there is no such file (pictures taken at 0 tilt)
c	  
c	  .     IF the model header has no scaling information, make the next
c	  .     two entries as well to specify scaling:
c
c	  .       Section thickness in nm, to scale Z coordinates to microns;
c	  .       or / to leave Z values unscaled
c	  
c	  .       Magnification of negatives, and scale of digitization (the
c	  .       value of microns/pixel from VIDS), to scale the X/Y
c	  .       coordinates correctly; or / to leave X/Y coordinates
c	  .       unscaled.
c	  
c	  .  Number of limiting regions (boundary contours or rectangles
c	  .  defined by X/Y coordinates) needed to specify the bundle, or
c	  .  0 to take all of the objects in the model.
c	  .
c	  .  For each limiting region, then enter:
c
c	  .     Either IMOD object number and contour number of the boundary
c	  .     contour, or a WIMP object number and 0 for data taken from a
c	  .     WIMP model file, or 0,0 to enter limiting X and Y coordinates
c	  .     of a box.
c	  
c	  .        IF you entered 0,0 next enter the lower and upper X index
c	  .        coordinates and the lower and upper Y coordinates of the
c	  .        box, or enter / to have no limit on the X and Y coordinates
c	  .        THEN enter the lower and upper Z coordinates of the box (in
c	  .        units of sections), or / to have no limits on Z coordinates
c	  
c	  .        IF you entered numbers for a boundary contour, next enter
c	  .        lower and upper Z coordinates of the "cylinder", or /
c	  .        to set those limiting coordinates to the Z coordinate of the
c	  .        boundary contour.  The latter is typical if one uses several
c	  .        contours in different sections to specify the bundle.
c	  
c	  IF you entered 0 for the number of bundles, next enter instead the 
c	  name of a file.  The first line of this file should have the number
c	  of bundles specified there.  The rest of the file should be all of
c	  the entries just described for each bundle.
c	  
c	  Enter a list of numbers of the bundles to work with.  Ranges may be
c	  entered, e.g. 1-3,7-9.
c
c	  The lower and upper limits of Z within which to compute pairing.
c	  
c	  A minimum number of sections to assume as shared sections when the
c	  fractional pairing is computed.  This entry was intended to avoid
c	  unreasonably large fractional pairing lengths when two MTs only
c	  appear together in a few sections.  A value of 4 may be useful.
c	  
c	  Enter a list of the types of MTs for which to compute pairing.
c	  These will be the "reference MTs" in the pairing calculations.  Type
c	  Return to include all MTs.  The type is the IMOD object number for an
c	  IMOD model, or 256 minus the color, for data from a WIMP model file.
c	  
c	  Enter a list of the types (object #s) of MTs to consider as neighbors
c	  to those reference MTs.  Type Return to include all MTs.
c	  
c	  Enter 1 for a simple pairing factor which is 1 for MTs within a
c	  certain distance of each other and 0 beyond, or 2 or 3 for a pairing
c	  factor that decays with distance in the X/Y plane, either as an
c	  inverse power or exponentially.
c	  
c	  Enter the distance in the X/Y plane at and below which overlap will
c	  equal 1.  The distance should be in microns if you have scaled X/Y
c	  values, or in pixels if you have not.
c	  
c	  .  IF you entered 2, next enter the power for the decay (e.g., with
c	  .     a power of 2, overlap will decay as the inverse square of
c	  .     distance)
c	  
c	  .  IF you entered 3, enter instead the space constant for exponential
c	  .  decay.  Overlap will be 1/e less for MT's separated by 2 space
c	  .  constants than for MT's separated by 1 space constant.  Distance
c	  .  should be in microns if you have scaled X/Y values, or in pixels
c	  .  if you have not.
c	  
c	  Minimum pairing length that a pair of MTs should have before its
c	  data will be stored for examination.  If there are not hundreds of
c	  MTs, a minimum of 0 will retain all data about MTs with any pairing.
c	  
c	  At this point, the program computes the pairings and gives
c	  information about the lengths of the many pairs of MTs with no
c	  pairing.  You are then at the option point.  Options are:
c	  
c	  1: to plot the pairing data about each MT.  Columns available are:
c	  .  1 = MT length
c	  .  2 = absolute pairing length summed over all neighbors to the MT
c	  .  3 = maximum pairing length achieved with one other MT
c	  .  4 = fractional pairing length summed over all neighbors to the MT
c	  .  5 = maximum fractional pairing length achieved with one other MT
c	  .  6 = Z value of midpoint of MT.
c	  
c	  2: to plot the data about paired MTs.  Columns available are:
c	  .  1 = arithmetic mean of the lengths of the two MTs
c	  .  2 = geometric mean of the lengths of the two MTs
c	  .  3 = absolute pairing length of that pair
c	  .  4 = fractional pairing length
c	  .  9 = mean separation between MTs while they were paired
c	  . 10 = SD of separation
c	  . 11 = coefficient of variation = SD/mean of separation
c
c	  With either option 1 or 2, you must enter the numbers of the columns
c	  to be plotted on the X or Y axes.  Next, enter a number for the
c	  symbol type as commonly referred to in GENHSTPLT, BSPLT, etc.
c	  After this, you will enter the BSPLT subroutine, whose entries are
c	  described therein.
c	  
c	  3: to loop back to the point where you specify which bundles to work
c	  with and then enter other parameters of the pairing calculations.
c	  
c	  4: to loop back and read in new bundles, replacing existing ones.
c
c	  5: to loop back and read in new bundles, retaining existing ones.
c	  
c	  6/7: to plot the current metacode file on the screen/printer
c	  
c	  8: to exit the program
c	  
c	  9: to recolor the model.  After selecting this option, you make an
c	  indefinite series of entries of the following form.  In one line,
c	  you enter the following information to select a set of MTs:
c	  
c	  .  New color of MTs.  Enter -1 here to terminate the series of
c	  .  recolorings.
c	  
c	  .  Column to use to select MTs.  The data about each MT are referred
c	  .  to by positive column numbers (1 to 6 as described above); the
c	  .  data about pairs are referred to by negative column numbers (-1
c	  .  to -4 as described above).  An entry of 0 will use the "polarity"
c	  .  values determined after using options 11 and 12.
c	  
c	  .  The lower and upper criterion limits to apply to values in that
c	  .  column.
c	  
c	  .  0 to select MTs that are within the limits, or 1 to select ones
c	  .  outside the limits.
c	  
c	  .  On the next line, enter a list of the types (original IMOD object
c	  .  numbers) that MTs should have in order for them to be recolored
c	  .  according to these criteria, or Return to apply the criteria to
c	  .  MTs of all types.
c	  
c	  In this way, you can assign each type of MT that meets a particular
c	  criterion to a particular new object.
c	  
c	  After you have entered all of the selections, enter the name of the
c	  output file in which to place the recolored model.
c	  
c	  10: to take commands from a file (next enter filename, or Return to
c	  .  take input from the keyboard)
c	  
c	  11 will find clusters of mutually paired MTs (which can be all of
c	  the MTs in a bundle), 12 will find polarities based on positions of
c	  the MTs in the bundle or cluster, and 13 will graph the clusters.
c	  These features are not documented here - consult command files for
c	  examples (these files may call this program "COMSYMP").
c	  
c	  David Mastronarde 1993
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2005/01/07 15:57:41  mast
c	  Fixed to undo effects of pixel size in image file header
c	
c	  Revision 3.2  2003/08/29 17:33:56  mast
c	  Change to use new multithreaded Plax graphics
c	
c
	call plax_initialize('mtpairing')
	call exit(0)
	end

	subroutine realgraphicsmain()
	parameter (limbun=200,limmt=10000,limdsp=2000,limtyp=60)
	parameter (limpt=1000000,limpair=20000,limclust=600,limgrf=90)
	real*4 zstrt(limmt),zend(limmt),datdisp(3,limdsp)
	integer*4 itype(limmt),indbundle(limbun),ninbundle(limbun)
	integer*4 listbund(limbun),itypcena(limtyp),itypcenb(limtyp)
	integer*4 itypovra(limtyp),itypovrb(limtyp),itypdisp(limtyp)
	integer*4 icoldisp(limtyp),iorder(limtyp),ipos(limtyp)
	integer*4 indtyp(limtyp,limgrf),nintyp(limtyp,limgrf)
	integer*4 itypset(limtyp,limbun),igrfbund(limbun),lstmp(limbun)
	integer*4 ntypset(limtyp),numov(4,limbun),listcalc(limbun)
	real*4 ovsum(4,limbun),ovsumsq(4,limbun),xmt(limpt),ymt(limpt)
	real*4 zmt(limpt),tiltzstrt(2000),remapz(2000),costilt(2000)
	integer*4 indxymt(limmt),nfitslp(3),iobjnum(limmt),indat(limmt)
	integer*4 icontnum(limmt)
	real*4 slopinv(3),fitpast(3),objlen(limmt)
	real*4 zptscal(limbun),xyscal(limbun)
	integer*4 ifile(limbun),ntilts(limbun),indtlt(limbun)
	real*4 datpair(limpair,11),datmt(limmt,7)
	include 'model.inc'
	integer*4 ngx(limpair),nsymb(1),icolsave(2,max_obj_num)
	logical typeonlist,outopen,aonlist,bonlist
	character*80 modelfile,comfile,modelout,outfile
	character*4 dummy4,objtext
	integer*4 indpair(limpair),mtclust(limpair*2),icluster(limpair)
	integer*4 indclust(limclust),ninclust(limclust),mtout(limmt)
	integer*4 iobjprn(limclust),markstrt(limtyp),markend(limtyp)
	real*4 overmat(limclust,limclust),zmid(limpair*2)
	real*4 polarity(limpair*2),polnew(limpair*2),zlink(limclust)
	real*4 gapstrt(limpair),gapend(limpair),zshrlnk(limclust)
	integer*4 imtlnk(limclust),jmtlnk(limclust),midlist(limtyp)
	integer*4 itypcheck(limtyp),listclust(limclust),iord(limpair)
	integer*4 liststrtuse(limtyp),listenduse(limtyp),itythrs(limtyp)
	integer*4 itycntrd(limtyp)
	logical b3dxor
c
	ifchange=1
	iffil=-1
	continue
c	  HVEM version only
	write(*,'(1x,a,$)')
     &	    '0 for plots in Plax window, 1 for terminal only: '
	read(*,*)iffil
c
	call grfopn(iffil)
c
	call opencomfile
c
c	  initialize list of bundles
c	  
10	nbundles=0
	indfree=0
	indxyfree=0
	modelfile=' '
	nfile=0
	indtlt(1)=1
c	  
c	  get list of bundles
c	  
15	write(*,'(1x,a,$)')'# of bundles to specify (or 0 to take'//
     &	    ' specifications from a file): '
	read(5,*)nbunspec
16	inunit=5
	if(nbunspec.le.0)then
17	  write(*,*)'Enter name of file with specifications, or'//
     &	      ' Return to take input from keyboard'
	  read(5,'(a)')comfile
	  if(comfile.eq.' ')go to 15
	  close(4)
c
c 7/20/00 CER remove shared, readonly for gnu
c
	  open(4,file=comfile,status='old',err=17)
	  inunit=4
	  read(4,*)nbunspec
	endif
c	  
	do ibun=1,nbunspec
	  write(*,'(a,i4)')' Enter specifications for bundle #',
     &	      nbundles+1
	  nfp=nfile+1
	  intl=indtlt(nfp)
	  call read_model(modelfile,inunit,xyscal(nfp),zptscal(nfp),
     &	      tiltzstrt(intl),remapz(intl),costilt(intl),
     &	      ntilts(nfp),nfile)
	  if(nfile.eq.nfp)indtlt(nfile+1)=indtlt(nfile)+ntilts(nfile)
	  intl=indtlt(nfile)
	  call get_bundle(zstrt,zend,itype,indbundle,ninbundle,
     &	      nbundles,indfree,xmt,ymt,zmt,indxymt,indxyfree,inunit,
     &	      xyscal(nfile),zptscal(nfile),tiltzstrt(intl),
     &	      remapz(intl),costilt(intl),ntilts(nfile),iobjnum,
     &	      icontnum,objlen)
	  ifile(nbundles)=nfile
	enddo
c	  
20	print *,'Enter list of bundles to calculate pairing'
     &	    ,' for (ranges OK)'
	call rdlist(5,listbund,nlistbund)
	ifi=ifile(listbund(1))
	intl=indtlt(ifi)
	do ibun=1,nlistbund
	  ifi2=ifile(listbund(ibun))
	  if(ifi.ne.ifi2.and.(ntilts(ifi2).ne.0.or.ntilts(ifi).ne.0
     &	      .or.zptscal(ifi2).ne.zptscal(ifi)))then
	    print *,'Cannot deal with bundles with different z scaling'
	    go to 20
	  endif
	enddo
c
	
	write(*,'(1x,a,$)')'Lower and upper section numbers to'//
     &	    ' calculate pairing between: '
	read(5,*)izlow,izhigh
	zlow=scalez(float(izlow),zptscal(ifi),tiltzstrt(intl),
     &	    remapz(intl),costilt(intl),ntilts(ifi))
	zhigh=scalez(float(izhigh),zptscal(ifi),tiltzstrt(intl),
     &	    remapz(intl),costilt(intl),ntilts(ifi))
	write(*,'(1x,a,/,a,$)')'Minimum number of sections to assume'//
     &	    ' as shared sections when calculating',
     &	    '   fraction of shared sections with pairing: '
	read(5,*)minshare
	zmidlo=(izlow+izhigh)/2.-0.5*minshare
	zminshare=scalez(zmidlo+minshare,zptscal(ifi),tiltzstrt(intl),
     &	    remapz(intl),costilt(intl),ntilts(ifi)) -
     &	    scalez(zmidlo,zptscal(ifi),tiltzstrt(intl),
     &	    remapz(intl),costilt(intl),ntilts(ifi))
c	  
c	  specify types to compute pairing from
c
30	write(*,'(1x,a,/,a,/,a,$)')'Enter the # of kinds of pairs '//
     &	    'to compute pairing for',' (enter minus the # to compute '//
     &	    'pairing per MT only for the first type',' of each pair'//
     &	    ' and treat 2nd type strictly as neighbors): '
	read(5,*)npairdo
	print *,'Enter the two types for each pair in turn; enter a 0 ',
     &	    'to refer to all types'
	ntypovra=abs(npairdo)
	read(5,*)(itypovra(i),itypovrb(i),i=1,ntypovra)
c	  
c	  make pairs symmetric if positive # entered
c
	do i=1,npairdo
	  if(itypovra(i).ne.itypovrb(i))then
	    ntypovra=ntypovra+1
	    itypovra(ntypovra)=itypovrb(i)
	    itypovrb(ntypovra)=itypovra(i)
	  endif
	enddo
	ntypovrb=ntypovra

	write(*,'(1x,a,/,a,$)')'Enter 1 for simple pairing'//
     &	    ' calculation, 2 for inverse power','     decay'//
     &	    ' with distance, or 3 for exponential decay: '
	read(5,*)iovertype
	write(*,'(1x,a,/,a,$)')'Enter distance in X/Y plane below'//
     &	    ' which pairing quantity will equal 1', '  (typically'//
     &	    ' this should be the maximum preferred MT spacing): '
	read(5,*)rzero
	rnull=rzero
	if(iovertype.ge.2)then
	  write(*,'(1x,a,$)')'Enter distance at which pairing '//
     &	      'is to be counted as zero: '
	  read(5,*)rnull
	  if(iovertype.eq.2)then
	    write(*,'(1x,a,$)')'Power to apply (a positive integer): '
	    read(5,*)ipower
	  elseif(iovertype.gt.2)then
	    write(*,'(1x,a,$)')'Space constant for exponential decay: '
	    read(5,*)xlambda
	  endif
	endif
	write(*,'(1x,a,$)')'Minimum pairing length to store pair: '
	read(5,*)pairmin
c	    
c	  calculate pairing factors
c
	npair=0
	nmt=0
	zerarith=0.
	zergeom=0.
	zerarithsq=0
	zergeomsq=0
	nzer=0
	do ibun=1,nlistbund
	  indstrt=indbundle(listbund(ibun))
	  indend=indstrt+ninbundle(listbund(ibun))-1
	  do ind=indstrt,indend
	    if(typeonlist(itype(ind),itypovra,ntypovra))then
	      nmt=nmt+1
	      indat(ind)=nmt
	      datmt(nmt,1)=objlen(ind)
	      do i=2,5
		datmt(nmt,i)=0.
	      enddo
	      datmt(nmt,6)=0.5*(zstrt(ind)+zend(ind))
	      datmt(nmt,7)=ind
	    endif
	  enddo
	  do inda=indstrt,indend-1
	    do indb=inda+1,indend
	      aonlist=.false.
	      bonlist=.false.
	      do iov=1,ntypovra
		if((itype(inda).eq.itypovra(iov).or.itypovra(iov).eq.0)
     &		    .and.(itype(indb).eq.itypovrb(iov).or.itypovrb(iov).eq.0))
     &		    aonlist=.true.
		if((itype(indb).eq.itypovra(iov).or.itypovra(iov).eq.0)
     &		    .and.(itype(inda).eq.itypovrb(iov).or.itypovrb(iov).eq.0))
     &		    bonlist=.true.
	      enddo
	      if(aonlist.or.bonlist)then
		call calcoverlap(zstrt,zend,xmt,ymt,zmt,indxymt,
     &		    iovertype,rnull,rzero,ipower,xlambda,inda,indb,
     &		    zlow,zhigh,zshared, overlap,sepavg,sepsd)
		if(zshared.gt.0.)then
		  arith=0.5*(objlen(inda)+objlen(indb))
		  geom=sqrt(objlen(inda)*objlen(indb))
		  if(overlap.gt.0.)then
		    fracover=overlap/max(zshared,zminshare)
		    if(overlap.ge.pairmin)then
		      npair=npair+1
		      datpair(npair,1)=arith
		      datpair(npair,2)=geom
		      datpair(npair,3)=overlap
		      datpair(npair,4)=fracover
		      datpair(npair,5)=icontnum(inda)
		      datpair(npair,6)=icontnum(indb)
		      datpair(npair,7)=inda
		      datpair(npair,8)=indb
		      datpair(npair,9)=1000.*sepavg
		      datpair(npair,10)=1000.*sepsd
		      datpair(npair,11)=sepsd/sepavg
		    endif
		    if(aonlist)then
		      kk=indat(inda)
		      datmt(kk,2)=datmt(kk,2)+overlap
		      datmt(kk,3)=max(datmt(kk,3),overlap)
		      datmt(kk,4)=datmt(kk,4)+fracover
		      datmt(kk,5)=max(datmt(kk,5),fracover)
		    endif
		    if(bonlist)then
		      kk=indat(indb)
		      datmt(kk,2)=datmt(kk,2)+overlap
		      datmt(kk,3)=max(datmt(kk,3),overlap)
		      datmt(kk,4)=datmt(kk,4)+fracover
		      datmt(kk,5)=max(datmt(kk,5),fracover)
		    endif
		  else
		    nzer=nzer+1
		    zerarith=zerarith+arith
		    zerarithsq=zerarithsq+arith**2
		    zergeom=zergeom+geom
		    zergeomsq=zergeomsq+geom**2
		  endif
		endif
	      endif
	    enddo
	  enddo
	enddo
	call sums_to_avgsd(zerarith,zerarithsq,nzer,avgarith,sdarith)
	call sums_to_avgsd(zergeom,zergeomsq,nzer,avggeom,sdgeom)
	write(*,107)nzer,avgarith,sdarith,avggeom,sdgeom
107	format(i6,' pairs with no pairing; mean & sd of lengths are:',/
     &	    ,2f7.2,' (arithmetic means), ',2f6.2,' (geometric means)')
c	  
50	write(*,104)
104	format(' Enter 1/16 for plot/print of per MT data,',
     &	    '   2/14 for plot/print of pair data,',/,
     &	    '       3 for new parameters ',
     &	    '   4 for new bundles (5 to add to old),',/,
     &	    '       6 or 7 to plot metacode file on screen or printer,',
     &	    '   8 to exit program',/,'       9 to output recolored model'
     &	    ,'    10 to open com file',/,'       11 to find clusters'
     &	    ,'    12 to find polarities     13 to graph cluster',/
     &	    ,'    15 for lengths and positions of MTs present',
     &	    ' throughout Z range',/
     &	    ,'    17 to average per MT data for MTs meeting criteria')
	read(5,*)iopt
	if(iopt.eq.-123)go to 99
	if(iopt.eq.209)iopt=7
	if(iopt.le.0.or.iopt.gt.17)go to 50
	go to(41,42,20,10,15,90,90,99,80,91,60,70,75,45,85,47,180)iopt
c	  
41	print *,'Columns for per MT data are: 1 = MT length;',
     &	    ' 2 = summed absolute pairing length',
     &	    '3 = maximum pairing length with one other MT',
     &	    '4 = summed fractional pairing length; 5 = maximum',
     &	    ' fractional pairing length'
	go to 40
42	print *,'Columns for pairs are: 1/2 = arithmetic/geometric',
     &	    ' mean length of the two MTs','3 = absolute pairing length;',
     &	    '  4 = fractional pairing length', '9/10/11 = average'//
     &	    '/SD/coeff. of variation of separation while paired'
40	write(*,'(1x,a,$)')'Columns for X and Y axes: '
	read(5,*)icolx,icoly
	nsymb(1)=9
	write(*,'(1x,a,$)')'Symbol type: '
	read(5,*)nsymb(1)
	do i=1,max(nmt,npair)
	  ngx(i)=1
	enddo
	if(iopt.eq.1)then
	  call gnplt(datmt(1,icolx),datmt(1,icoly),ngx,nmt,nsymb,1,0,0)
	else
	  call gnplt(datpair(1,icolx),datpair(1,icoly),ngx,npair,nsymb,
     &	      1,0,0)
	endif
	go to 50
c	  
c	  OUTPUT SORTED PAIRING DATA
c
45	write(*,'(1x,a,$)')'Pairing data column to order by: '
	read(5,*)iocol
	write(*,'(1x,a,$)')'Set identifier (0 for none): '
	read(5,*)idset
	do i=1,npair
	  iord(i)=i
	enddo
	do i=1,npair-1
	  do j=i+1,npair
	    if(datpair(iord(i),iocol).lt.datpair(iord(j),iocol))then
	      itmp=iord(i)
	      iord(i)=iord(j)
	      iord(j)=itmp
	    endif
	  enddo
	enddo
	if(idset.eq.0)then
	  write(*,120)
120	  format(43x,'Separation (nm)',/,'    Types   ',
     &	      'contour #s   lengths   pairing  mean    sd  coef-var')
	else
	  write(*,1201)
1201	  format(48x,'Separation (nm)',/,' Set     Types   ',
     &	      'contour #s   lengths   pairing  mean    sd  coef-var')
	endif
	do j=1,npair
	  i=iord(j)
	  inda=datpair(i,7)
	  indb=datpair(i,8)
	  if (idset.eq.0)then
	    write(*,119)itype(inda),itype(indb),icontnum(inda),
     &		icontnum(indb),objlen(inda),objlen(indb),datpair(i,3),
     &		datpair(i,9),datpair(i,10),datpair(i,11)
119	    format(4i5,3f7.3,2f7.1,f7.3)
	  else
	    write(*,1191)idset,itype(inda),itype(indb),icontnum(inda),
     &		icontnum(indb),objlen(inda),objlen(indb),datpair(i,3),
     &		datpair(i,9),datpair(i,10),datpair(i,11)
1191	    format(5i5,3f7.3,2f7.1,f7.3)
	  endif
	enddo
	nmtout=0
c	  
c	  BUG...  The following outputs max pairing for the object only if the
c	  pairs were sorted on max pairing in the preceding output.
c	  SUPERCEDED BY BELOW
c	  
	if(iocol.ne.3)go to 50
	if(idset.eq.0)then
	  write(*,121)
121	  format(/,' Type  contour # length  pairing')
	else
	  write(*,1211)
1211	  format(/,' Set  Type  contour # length  pairing')
	endif
	do j=1,npair
	  i=iord(j)
	  do indcol=7,8
	    inda=datpair(i,indcol)
	    if(nmtout.eq.0.or..not.typeonlist(inda,mtout,nmtout))then
	      nmtout=nmtout+1
	      mtout(nmtout)=inda
	      if(idset.eq.0)then
		write(*,122)itype(inda),icontnum(inda),objlen(inda),
     &		    datpair(i,3)
122		format(i5,i7,2f9.3)
	      else
		write(*,1221)idset,itype(inda),icontnum(inda),objlen(inda),
     &		    datpair(i,3)
1221		format(2i5,i7,2f9.3)
	      endif
	    endif
	  enddo
	enddo
	go to 50
c	  
c	  OUTPUT SUMMED AND MAX PAIRING DATA FOR EACH MT
c
47	write(*,'(1x,a,$)')'Per MT data column to order by: '
	read(5,*)iocol
	write(*,'(1x,a,$)')'Set identifier (0 for none): '
	read(5,*)idset
	write(*,'(1x,a,$)')'1 to output MTs with 0 pairing, 0 not to: '
	read(5,*)ifout0
	do i=1,nmt
	  iord(i)=i
	enddo
	do i=1,nmt-1
	  do j=i+1,nmt
	    if(datmt(iord(i),iocol).lt.datmt(iord(j),iocol))then
	      itmp=iord(i)
	      iord(i)=iord(j)
	      iord(j)=itmp
	    endif
	  enddo
	enddo
	if(idset.eq.0)then
	  write(*,131)
131	  format(/,' Type cont   length   summed & max pairing')
	else
	  write(*,1311)
1311	  format(/,' Set  Type cont   length   summed & max pairing')
	endif
	do j=1,nmt
	  i=iord(j)
	  ind=datmt(i,7)
	  if (idset.eq.0.and.(ifout0.ne.0.or.datmt(i,2).gt.0.))then
	    write(*,319)itype(ind),icontnum(ind),objlen(ind),
     &		datmt(i,2),datmt(i,3)
319	    format(2i5,3f9.3)
	  elseif(ifout0.ne.0.or.datmt(i,2).gt.0.)then
	    write(*,3191)idset,itype(ind),icontnum(ind),objlen(ind),
     &		datmt(i,2),datmt(i,3)
3191	    format(3i5,3f9.3)
	  endif
	enddo
	go to 50
c
c
60	write(*,'(1x,a,$)')'Criterion pairing length for clustering; '//
     &	    '1 to use fractional pairing: '
	read(5,*)bundcrit,ifusefrac
	icol=3
	if(ifusefrac.gt.0)icol=4
	if(bundcrit.le.0.)then
c	    
c	    if criterion is 0, just copy all mts in each bundle into cluster
c
	  indfree=0
	  ncluster=nlistbund
	  do ibun=1,nlistbund
	    indclust(ibun)=indfree+1
	    ninclust(ibun)=ninbundle(listbund(ibun))
	    indstrt=indbundle(listbund(ibun))
	    indend=indstrt+ninbundle(listbund(ibun))-1
	    do ind=indstrt,indend
	      indfree=indfree+1
	      mtclust(indfree)=ind
	    enddo
	  enddo
	else
c	  
c	    make list of pairs above the criterion
c	  
	  nlistpair=0
	  do i=1,npair
	    if(datpair(i,icol).ge.bundcrit)then
	      nlistpair=nlistpair+1
	      indpair(nlistpair)=i
	      icluster(nlistpair)=0
	    endif
	  enddo
	  ncluster=0
	  iclusfree=1
	  iclusend=0
	  print *,nlistpair,' pairs above criterion'
	  do while (iclusfree.le.nlistpair)
	    if(icluster(iclusfree).eq.0)then
c		
c		start new cluster with first free pair
c		
	      ncluster=ncluster+1
	      iclustrt=iclusend+1
	      indclust(ncluster)=iclustrt
	      iclusend=iclustrt+1
	      mtclust(iclustrt)=nint(datpair(indpair(iclusfree),7))
	      mtclust(iclusend)=nint(datpair(indpair(iclusfree),8))
	      icluster(iclusfree)=ncluster
	      mtlook=iclustrt
	      do while (mtlook.le.iclusend)
c		  
c		  for each MT already in cluster, look for pairs that include
c		  that MT and add the other member of sucvh pairs to cluster
c		  
		indlook=iclusfree+1
		do while (indlook.le.nlistpair)
		  if(icluster(indlook).eq.0)then
c		      
c		      if not in any cluster yet, see if either one of pair
c		      matches the current one on the MT list in this cluster
c		      
		    do indcol=7,8
		      if(nint(datpair(indpair(indlook),indcol)).eq.
     &			  mtclust(mtlook))then
			icluster(indlook)=ncluster
c			  
c			  got a match: see if mate is on list already
c			  
			ifonlist=0
			mttoadd=nint(datpair(indpair(indlook),15-indcol))
			do imt=iclustrt,iclusend
			  if(mtclust(imt).eq.mttoadd)ifonlist=1
			enddo
c			  
c			  if mate is not already on list, add it
			
			if(ifonlist.eq.0)then
			  iclusend=iclusend+1
			  mtclust(iclusend)=mttoadd
			endif
		      endif
		    enddo
		  endif
		  indlook=indlook+1
		enddo
		mtlook=mtlook+1
	      enddo
	      ninclust(ncluster)=iclusend+1-indclust(ncluster)
	    else
	      iclusfree=iclusfree+1
	    endif
	  enddo
	endif
	write(*,105)ncluster,(ninclust(i),i=1,ncluster)
105	format(i5,' clusters with these numbers of MTs:',/,(15i5))
	go to 50
c
70	write(*,'(1x,a,$)')'Output file, or Return for screen: '
	read(5,'(a)')outfile
	if(outfile.eq.' ')then
	  iout=6
	else
	  iout=9
	  call dopen(9,outfile,'new','f')
	endif
	write(*,'(1x,a,/,a,$)')'Limit on # of times to loop (0 for'//
     &	    ' polarities based solely on midZ,',
     &	    '   - limit to use just ends to compute center): '
	read(5,*)looplim
	write(*,'(1x,a,$)')
     &	    'Starting midZ value, or 0 to use a calculated mean midZ: '
	read(5,*)zmidin
	if(zmidin.eq.0)then
	  print *,'Enter list of types to include in calculating mean',
     &	      ' midZ (Return for all)'
	  call rdlist(5,midlist,nmidlist)
	endif
	ifusends=0
	if(looplim.lt.0)then
	  ifusends=1
	  looplim=-looplim
	  write(*,'(1x,a,$)')
     &	      '-1 to use - ends, 0 to use midZ, +1 to use + ends: '
	  read(5,*)iwhich
	  print *,'Enter list of types to use',
     &	      ' if midZ < center (Return for all)'
	  call rdlist(5,listenduse,nlistenduse)
	  print *,'Enter list of types to use',
     &	      ' if midZ > center (Return for all)'
	  call rdlist(5,liststrtuse,nliststrtuse)
	endif
	do iclust=1,ncluster
	  iclustrt=indclust(iclust)
	  iclusend=iclustrt+ninclust(iclust)-1
	  zmidavg=zmidin
	  if(ifusends.eq.0)then
c	  
c	      Make matrix of pairings
c	  
	    do imt=1,ninclust(iclust)
	      do jmt=imt+1,ninclust(iclust)
		call calcoverlap(zstrt,zend,xmt,ymt,zmt,indxymt,
     &		    iovertype,rnull,rzero,ipower,xlambda,
     &		    mtclust(imt+iclustrt-1), mtclust(jmt+iclustrt-1),
     &		    zlow,zhigh,zshared, overlap,sepavg,sepsd)
		if(zshared.gt.0)then
		  overmat(imt,jmt)=overlap
		else
		  overmat(imt,jmt)=-1.
		endif
		overmat(jmt,imt)=overmat(imt,jmt)
	      enddo
	      overmat(imt,imt)=0.
	    enddo
	  endif
c	    
c	    Analyze S and N polarities: determine average mid Z value
c	      
	  zsum=0.
	  ninsum=0
	  do imt=iclustrt,iclusend
	    ind=mtclust(imt)
	    zmid(imt)=(zstrt(ind)+zend(ind))/2.
	    if(typeonlist(itype(ind),midlist,nmidlist))then
	      zsum=zsum+zmid(imt)
	      ninsum=ninsum+1
	    endif
	  enddo
	  if(zmidavg.eq.0.)zmidavg=zsum/max(1,ninsum)
c	    
c	    start with polarities based solely on mid Z relative to average
c	    
	  do imt=iclustrt,iclusend
	    polarity(imt)=zmid(imt)-zmidavg
	  enddo
	  npolchg=1
	  nloop=0
	  center=zmidavg
	  if(ifusends.eq.0)then
c	      
c	      compute new polarity from sign of old polarity and pairing with
c	      each other MT in cluster - until it stabilizes
c	    
	    do while (npolchg.ne.0.and.nloop.lt.looplim)
	      npolchg=0
	      do imt=iclustrt,iclusend
		polsum=0.
		do jmt=iclustrt,iclusend
		  overlap=overmat(imt+1-iclustrt,jmt+1-iclustrt)
		  if(overlap.gt.0.)polsum=
     &		      polsum-overlap*sign(1.,polarity(jmt))
		enddo
		polnew(imt)=polsum
		if(polsum.ne.polarity(imt))npolchg=npolchg+1
	      enddo
	      do imt=iclustrt,iclusend
		polarity(imt)=polnew(imt)
	      enddo
	      nloop=nloop+1
	    enddo
	  else
c	      
c	      or, compute center based on putative plus ends z values,
c	      then revise polarities 
c	      
	    write(*,*)
	    do while (npolchg.ne.0.and.nloop.lt.looplim)
	      npolchg=0
	      strtsum=0
	      nstrt=0
	      endsum=0
	      nend=0
	      do imt=iclustrt,iclusend
		ind=mtclust(imt)
		if(typeonlist(itype(ind),listenduse,nlistenduse).and.
     &		    polarity(imt).lt.0)then
		  nend=nend+1
		  if(iwhich.gt.0)then
		    endsum=endsum+zend(ind)
		  elseif(iwhich.lt.0)then
		    endsum=endsum+zstrt(ind)
		  else
		    endsum=endsum+zmid(imt)
		  endif
		endif
		if(typeonlist(itype(ind),liststrtuse,nliststrtuse).and.
     &		    polarity(imt).ge.0)then
		  nstrt=nstrt+1
		  if(iwhich.gt.0)then
		    strtsum=strtsum+zstrt(ind)
		  elseif(iwhich.lt.0)then
		    strtsum=strtsum+zend(ind)
		  else
		    strtsum=strtsum+zmid(imt)
		  endif
		endif
	      enddo
	      center=0.5*(strtsum/max(1,nstrt)+endsum/max(1,nend))
	      do imt=iclustrt,iclusend
		polsum=zmid(imt)-center
		if(polsum.ne.polarity(imt))npolchg=npolchg+1
		polarity(imt)=polsum
	      enddo
	      nloop=nloop+1
	      write(iout,117)iclust,nloop,nend,nstrt, ninclust(iclust)-
     &		  nend-nstrt,endsum/max(1,nend),center,strtsum/max(1,nstrt)
117	      format(5i5,3f7.2)
	    enddo
	  endif
	  if(nloop.eq.looplim)write(iout,*)
     &	      ' WARNING: polarities did not stabilize for this cluster'
	  write(iout,111)nloop,ninclust(iclust),iclust,center
111	  format(/,i5,' iterations:'
     &	      ,i5,' MTs in Cluster #',i4,',  mean Z =',f7.2,/,
     &	      ' Object  type  start   middle  end Z  Polarity')
	  do imt=iclustrt,iclusend
	    inda=mtclust(imt)
	    iobjprn(imt+1-iclustrt)=icontnum(inda)
	    write(iout,112)icontnum(inda),itype(inda),zstrt(inda),
     &		zmid(imt),zend(inda),polarity(imt)
112	    format(2i6,4f8.2)
	  enddo
	  if(ifusends.eq.0.and.looplim.gt.0)then
	    nprnt=(ninclust(iclust)+9)/10
	    do iprn=1,nprnt
	      icolst=(iprn-1)*10+1
	      icolnd=min(icolst+9,ninclust(iclust))
	      write(iout,108)(iobjprn(i),i=icolst,icolnd)
108	      format(/,4x,10i7)
	      do irow=1,ninclust(iclust)
		write(iout,109)iobjprn(irow),
     &		    (overmat(irow,icol),icol=icolst,icolnd)
109		format(i6,10f7.2)
	      enddo
	    enddo
	  endif
	enddo
	if(iout.eq.9)close(9)
	go to 50
c
75	write(*,'(1x,a,$)')'Cluster #: '
	read(5,*)iclust
	iclustrt=indclust(iclust)
	iclusend=iclustrt+ninclust(iclust)-1
	oversum=0.
	nlinks=0
	zmin=1.e10
	zmax=-zmin
	do imt=1,ninclust(iclust)
	  mtnum=mtclust(imt+iclustrt-1)
	  zmin=min(zmin,zstrt(mtnum))
	  zmax=max(zmax,zend(mtnum))
	  do jmt=imt+1,ninclust(iclust)
	    call calcoverlap(zstrt,zend,xmt,ymt,zmt,indxymt, iovertype,
     &		rnull,rzero,ipower,xlambda,mtnum,mtclust(jmt+iclustrt-1)
     &		,zlow,zhigh, zshared, overlap,sepavg,sepsd)
	    if(zshared.gt.0.and.overlap.gt.0.)then
	      nlinks=nlinks+1
	      oversum=oversum+overlap
	      imtlnk(nlinks)=imt
	      jmtlnk(nlinks)=jmt
	      zshrlnk(nlinks)=zshared
	      overmat(imt,jmt)=overlap
	    endif
	  enddo
	enddo
	write(*,118)ninclust(iclust),zmin,zmax
118	format(i5,' MTs in cluster.  Z ranges from',f8.2,' to',f8.2)
c	  
	do ilink=1,nlinks-1
	  do jlink=ilink+1,nlinks
	    if(zshrlnk(ilink).gt.zshrlnk(jlink))then
	      ztmp=zshrlnk(ilink)
	      zshrlnk(ilink)=zshrlnk(jlink)
	      zshrlnk(jlink)=ztmp
	      itmp=imtlnk(ilink)
	      imtlnk(ilink)=imtlnk(jlink)
	      imtlnk(jlink)=itmp
	      itmp=jmtlnk(ilink)
	      jmtlnk(ilink)=jmtlnk(jlink)
	      jmtlnk(jlink)=itmp
	    endif
	  enddo
	enddo
c
77	write(*,'(1x,a,$)')'Lower left corner X, Y, 1 for new page: '
	read(5,*)xll,yll,ifpage
	if(ifpage.ne.0)call frame
	ifpage=0
	write(*,'(1x,a,f8.2,a,$)')
     &	    'Minimum Z value, or / for current minimum [',zmin,']: '
	read(5,*)zmin
	if(ifchange.eq.0)then
	  write(*,'(1x,a,$)')'1 to change any other parameters: '
	  read(5,*)ifchange
	endif
	if(ifchange.ne.0)then
	  ifchange=0
	  write(*,'(1x,a,$)')
     &	      'Inches per um horizontal, per MT vertical: '
	  read(5,*)xscale,yscale
	  write(*,'(1x,a,$)')'Character size (0 for no object #),'
     &	      //' spacing from left end: '
	  read(5,*)charsize,charspac
	  vertofs=0.6*charsize
	  horizofs=4*0.825*charsize+charspac
	  write(*,'(1x,a,$)')'MT line thickness, link width '//
     &	      'per um pairing, interruption size: '
	  read(5,*)mtthick,widlink,gapsize
	  rightmarg=0.
	  write(*,'(1x,a,$)')'Symbols at starts and ends of MTs: '
	  read(5,*)isymstrt,isymend
	  if(isymstrt.ne.0)then
	    print *,'Enter list of types for which to put symbols',
     &		' at starts'
	    call rdlist(5,markstrt,nmarkstrt)
	  endif
	  if(isymstrt.ne.0)then
	    print *,'Enter list of types for which to put symbols',
     &		' at end'
	    call rdlist(5,markend,nmarkend)
	  endif
	  if(isymstrt.ne.0.or.isymend.ne.0)then
	    write(*,'(1x,a,$)')'symbol size, thickness: '
	    read(5,*)symwid,isymthk
	    call symsiz(symwid)
	    rightmarg=0.6*symwid
	    horizofs=horizofs+rightmarg
	    vertofs=max(vertofs,rightmarg)
	  endif
	endif
	ytop=yll+(ninclust(iclust)-1)*yscale+2*vertofs
	xright=xll+horizofs+xscale*(zmax-zmin)+rightmarg
	write(*,114)xright,ytop
114	format(' Graph will go to',f7.2,' on the right and',f7.2,
     &	    ' on the top')
	if(xright.gt.7.5.or.ytop.gt.7.5.or.xll.lt.0.or.yll.lt.0.)then
	  print *,'graph will not fit on page - change position or scale'
	  go to 77
	endif
c
	buflink=(xscale*(zmax-zmin)-oversum*widlink)/nlinks
79	if(buflink.lt..02)then
	  print *,'Links will not fit - increase X scale or decrease',
     &	      ' link width'
	  ifchange=1
	  go to 77
	endif
	ilink=0
	ngaps=1
	gapstrt(1)=0.
	gapend(1)=xscale*(zmax-zmin)
	do ilink=1,nlinks
	  imt=imtlnk(ilink)
	  mti=mtclust(imt+iclustrt-1)
	  jmt=jmtlnk(ilink)
	  mtj=mtclust(jmt+iclustrt-1)
	  overlap=overmat(imt,jmt)
	  zintstr=xscale*(max(zstrt(mti),zstrt(mtj))-zmin)
	  zintend=xscale*(min(zend(mti),zend(mtj))-zmin)
	  totwid=overlap*widlink+buflink
	  zneed=max(0.,totwid-(zintend-zintstr))
	  zintstr=zintstr-0.5*zneed
	  zintend=zintend+0.5*zneed
	  igap=1
	  ifound=0
	  do while(igap.le.ngaps.and.ifound.eq.0)
	    gapintstr=max(gapstrt(igap),zintstr)
	    gapintend=min(gapend(igap),zintend)
	    if(gapintend-gapintstr.ge.totwid)then
	      ifound=1
	      zlink(ilink)=gapintstr+0.5*buflink
	      if(gapintstr.eq.gapstrt(igap))then
		gapstrt(igap)=gapstrt(igap)+totwid
	      else
		do iup=ngaps,igap,-1
		  gapstrt(iup+1)=gapstrt(iup)
		  gapend(iup+1)=gapend(iup)
		enddo
		gapend(igap)=gapintstr
		gapstrt(igap+1)=gapintstr+totwid
		ngaps=ngaps+1
	      endif
	    endif
	    igap=igap+1
	  enddo
	  if(ifound.eq.0)then
	    buflink=0.9*buflink
	    go to 79
	  endif
	enddo
c	  
	call imset(mtthick,c1,upi,c3,0)
	thkadj=0.5*(mtthick-1)/upi
	itxtsiz=3175.*charsize
	do imt=iclustrt,iclusend
	  call imset(mtthick,c1,upi,c3,0)
	  mtnum=mtclust(imt)
	  yline=yll+vertofs+(imt-iclustrt)*yscale
	  xlinstr=xll+horizofs+(zstrt(mtnum)-zmin)*xscale
	  xlinend=xll+horizofs+(zend(mtnum)-zmin)*xscale
	  call imma(xlinstr,yline)
	  call imva(xlinend,yline)
	  if(isymstrt.ne.0.and.
     &	      typeonlist(itype(mtnum),markstrt,nmarkstrt))then
	    call imset(isymthk,c1,c2,c3,0)
	    call imsymb(xlinstr,yline+thkadj,isymstrt)
	  endif
	  if(isymend.ne.0.and.
     &	      typeonlist(itype(mtnum),markend,nmarkend))then
	    call imset(isymthk,c1,c2,c3,0)
	    call imsymb(xlinend+2.*thkadj,yline+thkadj,isymend)
	  endif
	  if(itxtsiz.ne.0.)then
	    write(objtext,'(i4)')icontnum(mtnum)
	    call pwrit(xll,yline,objtext,4,itxtsiz,0,-1)
	  endif
	enddo
c	  
	call imset(1,c1,c2,c3,0)
	do ilink=1,nlinks
	  imt=imtlnk(ilink)
	  jmt=jmtlnk(ilink)
	  overlap=overmat(imt,jmt)
	  xlnkstr=xll+horizofs+zlink(ilink)
	  xlnkend=xlnkstr+overlap*widlink
	  nline=(xlnkend-xlnkstr)*upi+1
	  do mtline=imt,jmt-1
	    ybot=yll+vertofs+(mtline-1)*yscale
	    ytop=ybot+yscale
	    if(mtline.gt.imt)ybot=ybot+gapsize+(mtthick-1)/upi
	    if(mtline.lt.jmt-1)ytop=ytop-gapsize
	    do iline=1,nline
	      xline=min(xlnkstr+(iline-1)/upi,xlnkend)
	      call imma(xline,ytop)
	      call imva(xline,ybot)
	    enddo
	  enddo
	enddo
	go to 50
c
c	  "RECOLOR" THE MODEL BASED ON CRITERIA
c
80	do iobj=1,max_mod_obj
	  icolsave(1,iobj)=obj_color(1,iobj)
	  icolsave(2,iobj)=obj_color(2,iobj)
	enddo
	newobject=1
	do while (newobject.ge.0)
	  write(*,'(1x,a,/,a,/,a,$)')'New object (-1 if done), column'//
     &	      ' of per MT data or - column of',
     &	      ' pair data, lower and upper limit',' of interval, 0 or'//
     &	      ' 1 to select MTs inside or outside interval: '
	  read(5,*)newobject,icolm,critlo,crithi,ifoutside
	  if(newobject.ge.0)then
	    newcolor=256-newobject
	    call putimodflag(newobject,1)
	    print *,'List of original types to apply this to, or Return'
     &		,' for all'
	    call rdlist(5,itypcheck,ntypcheck)
	    indchg=2
c	    if(newcolor.lt.5)indchg=1
	    if(icolm.gt.0)then
	      do ibun=1,nlistbund
		indstrt=indbundle(listbund(ibun))
		indend=indstrt+ninbundle(listbund(ibun))-1
		do ind=indstrt,indend
		  if(typeonlist(itype(ind),itypcheck,ntypcheck).and.
     &		      typeonlist(itype(ind),itypovra,ntypovra))then
		    critval=datmt(indat(ind),icolm)
		    if(b3dxor(critval.ge.critlo.and.critval.le.crithi,
     &			ifoutside.ne.0))
     &			obj_color(indchg,iobjnum(ind))=newcolor
		  endif
		enddo
	      enddo
	    elseif(icolm.lt.0)then
	      do ipair=1,npair
		critval=datpair(ipair,-icolm)
		if(b3dxor(critval.ge.critlo.and.critval.le.crithi,
     &		    ifoutside.ne.0))then
		  ind=nint(datpair(ipair,7))
		  if(typeonlist(itype(ind), itypcheck,ntypcheck))
     &		      obj_color(indchg,iobjnum(ind))=newcolor
		  ind=nint(datpair(ipair,8))
		  if(typeonlist(itype(ind), itypcheck,ntypcheck))
     &		      obj_color(indchg,iobjnum(ind))=newcolor
		endif
	      enddo
	    else
	      print *,'Enter list of clusters to apply this to, or ',
     &		  'Return for all'
	      call rdlist(5,listclust,nlistclust)
	      if(nlistclust.eq.0)then
		do i=1,ncluster
		  listclust(i)=i
		enddo
	      endif
	      do ilist=1,nlistclust
		iclust=listclust(ilist)
		do imt=indclust(iclust),indclust(iclust)+
     &		    ninclust(iclust)-1
		  critval=polarity(imt)
		  if(typeonlist(itype(mtclust(imt)),itypcheck,ntypcheck)
     &		      .and.b3dxor(critval.ge.critlo.and.critval.le.crithi,
     &		      ifoutside.ne.0))
     &		      obj_color(indchg,iobjnum(mtclust(imt)))=newcolor
		enddo
	      enddo
	    endif
	  endif
	enddo
82	write(*,'(1x,a,$)')'Name of output model file: '
	read(5,'(a)')modelout
	call scale_model(1)
	call write_wmod(modelout)
	call scale_model(0)
	do iobj=1,max_mod_obj
	  obj_color(1,iobj)=icolsave(1,iobj)
	  obj_color(2,iobj)=icolsave(2,iobj)
	enddo
	go to 50
c	  
c	  COMPUTE MEAN OF A COLUMN FOR MTS MEETING & NOT MEETING CRITERIA
c
180	write(*,'(1x,a,$)')
     &	    'Column to sum, # of criterion sets to apply: '
	read(5,*)icolsum,ncrit
	do i=1,nmt
	  mtout(i)=0
	enddo
	do icrit=1,ncrit
	  write(*,'(1x,a,/,a,$)')'Column of per MT data, '//
     &	      'lower and upper limit of interval,',
     &	      ' 0 or 1 to select MTs inside or outside interval: '
	  read(5,*)icolm,critlo,crithi,ifoutside
	  print *,'List of types to apply this to, or Return for all'
	  call rdlist(5,itypcheck,ntypcheck)
	  do i=1,nmt
	    ind=nint(datmt(i,7))
	    if(typeonlist(itype(ind),itypcheck,ntypcheck))then
	      critval=datmt(i,icolm)
	      if(b3dxor(critval.ge.critlo.and.critval.le.crithi,
     &		  ifoutside.ne.0))mtout(i)=1
	    endif
	  enddo	    
	enddo
	selsum=0.
	unsum=0.
	selsq=0.
	unsq=0.
	nsel=0
	nun=0
	do i=1,nmt
	  if(mtout(i).eq.0)then
	    unsum=unsum+datmt(i,icolsum)
	    unsq=unsq+datmt(i,icolsum)**2
	    nun=nun+1
	  else
	    selsum=selsum+datmt(i,icolsum)
	    selsq=selsq+datmt(i,icolsum)**2
	    nsel=nsel+1
	  endif
	enddo
	call sums_to_avgsd(selsum,selsq,nsel,selavg,selsd)
	call sums_to_avgsd(unsum,unsq,nun,unavg,unsd)
	write(*,1180)' Selected  :',nsel,selavg,selsd
	write(*,1180)' Unselected:',nun,unavg,unsd
1180	format(a,' n =',i4,', mean =',f7.3,', sd =',f7.3)
	go to 50
c
85	write(*,'(1x,a,$)')'Threshold maximum pairing length: '
	read(5,*)threshlen
	print *,'Enter list of types to consider'
	call rdlist(5,itythrs,ntythrs)
	write(*,'(1x,a,$)')'0 to clear accumulated results or 1 to'//
     &	    ' accumulate with previous results: '
	read(5,*)ifaccum
	write(*,'(1x,a,$)')' 1 to type out MTs: '
	read(5,*)iftyp
	if(iftyp.ne.0)then
	  print *,'Enter list of types to use in computing',
     &	      ' centroid of bundle (Return for all)'
	  call rdlist(5,itycntrd,ntycntrd)
	endif
	nprd=0
	prdsum=0.
	prdsq=0.
	nunp=0
	unpsum=0.
	unpsq=0.
	do ibun=1,nlistbund
	  indstrt=indbundle(listbund(ibun))
	  indend=indstrt+ninbundle(listbund(ibun))-1
	  if(iftyp.ne.0)then
	    maxcnt=0
	    do izcnt=izlow,izhigh
	      zcnt=scalez(float(izcnt),zptscal(ifi),tiltzstrt(intl),
     &		  remapz(intl),costilt(intl),ntilts(ifi))
	      ncnt=0
	      do ind=indstrt,indend
		if(zstrt(ind).le.zcnt.and.zend(ind).ge.zcnt)ncnt=ncnt+1
	      enddo
	      if(ncnt.gt.maxcnt)then
		maxcnt=ncnt
		zmaxcnt=zcnt
	      endif
	    enddo
	    xsum=0.
	    ysum=0.
	    nxysum=0
	    do ind=indstrt,indend
	      if(typeonlist(itype(ind),itycntrd,ntycntrd).and.
     &		  zstrt(ind).le.zmaxcnt.and.zend(ind).ge.zmaxcnt)then
		ipta=indxymt(ind)
		do while (zmt(ipta).lt.zmaxcnt)
		  ipta=ipta+1
		enddo
		xsum=xsum+xmt(ipta)
		ysum=ysum+ymt(ipta)
		nxysum=nxysum+1
	      endif
	    enddo
	    xcntrd=xsum/max(1,nxysum)
	    ycntrd=ysum/max(1,nxysum)
	    print *,xcntrd,ycntrd,nxysum,zmaxcnt
	  endif
	  do ind=indstrt,indend
	    if(typeonlist(itype(ind),itypovra,ntypovra).and.
     &		typeonlist(itype(ind),itythrs,ntythrs).and.
     &		zstrt(ind).le.zlow.and.zend(ind).ge.zhigh)then
	      imt=indat(ind)
	      if(datmt(imt,3).lt.threshlen)then
		ioutyp=1
		nunp=nunp+1
		unpsum=unpsum+objlen(ind)
		unpsq=unpsq+objlen(ind)**2
	      else
		ioutyp=2
		nprd=nprd+1
		prdsum=prdsum+objlen(ind)
		prdsq=prdsq+objlen(ind)**2
	      endif
	      if(iftyp.ne.0)then
		ipta=indxymt(ind)
		do while (zmt(ipta).lt.zmaxcnt)
		  ipta=ipta+1
		enddo
		cendist=sqrt((xmt(ipta)-xcntrd)**2+
     &		    (ymt(ipta)-ycntrd)**2)
		write(*,285)ioutyp,itype(ind),icontnum(ind),
     &		    objlen(ind),datmt(imt,3),cendist
285		format(i2,2i5,3f8.3)
	      endif
	    endif
	  enddo
	enddo
	call sums_to_avgsd(prdsum,prdsq,nprd,prdavg,prdsd)
	call sums_to_avgsd(unpsum,unpsq,nunp,unpavg,unpsd)
	write(*,286)' This set:  ',nprd,prdavg,prdsd,nunp,unpavg,unpsd
286	format(a,' paired n=',i3,', avg =',f7.3,', sd=',f7.3,/,
     &	    10x,' unpaired n=',i3,', avg =',f7.3,', sd=',f7.3)
	if(ifaccum.eq.0)then
	  nprdcum=nprd
	  prdsumcum=prdsum
	  prdsqcum=prdsq
	  nunpcum=nunp
	  unpsumcum=unpsum
	  unpsqcum=unpsq
	else
	  nunpcum=nunpcum+nunp
	  unpsumcum=unpsumcum+unpsum
	  unpsqcum=unpsqcum+unpsq
	  nprdcum=nprdcum+nprd
	  prdsumcum=prdsumcum+prdsum
	  prdsqcum=prdsqcum+prdsq
	  call sums_to_avgsd(prdsumcum,prdsqcum,nprdcum,prdavg,prdsd)
	  call sums_to_avgsd(unpsumcum,unpsqcum,nunpcum,unpavg,unpsd)
	  write(*,286)' Cumulative:',nprdcum,prdavg,prdsd,nunpcum,
     &	      unpavg,unpsd
	endif
	go to 50
c	  
90	call pltout(7-iopt)
	go to 50
c	  
91	call opencomfile
	go to 50
c
99	call plxoff
	call imexit
	end





	subroutine read_model(modelfile,inunit,xyscal,zscal,tiltzstrt,
     &	    remapz,costilt,ntilts,nfile)
	include 'model.inc'
	character*(*) modelfile
	real*4 tiltzstrt(*),remapz(*),costilt(*)
	character*50 newfile,tiltfile
	logical exist,readw_or_imod
	integer getimodhead
c	  
91	if(inunit.eq.5.and.modelfile.ne.' ')then
	  write(*,'(1x,a,$)')
     &	      'Name of model file (or Return for same as last): '
	elseif(inunit.eq.5)then
	  write(*,'(1x,a,$)')'Name of model file: '
	endif
	read(inunit,'(a)')newfile
	if(modelfile.ne.' '.and.newfile.eq.' ')return
c
75	exist=readw_or_imod(newfile)
	if(.not.exist)then
	  modelfile=' '
	  go to 91
	else
	  nfile=nfile+1
	  modelfile=newfile
	  write(*,'(1x,a,$)')
     &	      'Name of file of tilt info (Return if none): '
	  read(inunit,'(a)')tiltfile
	  if(tiltfile.ne.' ')then
	    call dopen(3,tiltfile,'ro','f')
	    ntilts=0
3	    i=ntilts+1
	    read(3,*,end=5)tiltzstrt(i),costilt(i)
	    ntilts=i
	    costilt(ntilts)=cosd(costilt(ntilts))
	    go to 3
5	    remapz(1)=tiltzstrt(1)
	    close(3)
	    do i=1,ntilts-1
	      remapz(i+1)=remapz(i)+(tiltzstrt(i+1)-tiltzstrt(i))
     &		/costilt(i)
	    enddo
	  endif
c	    
	  defscal=1.e6
	  call scale_model(0)
	  ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	  if(ierr.eq.0.and.abs(xyscal-defscal)/defscal.gt.1.e-5)then
	    write(*,'(a,f10.6,a)')' Scale set from model header at',
     &		xyscal,' microns/pixel'
	    zscal=xyscal*zscale
	    return
	  endif
	  secthick=1000.
	  write(*,'(1x,a,$)')'Nominal section thickness in nm, or / for'
     &	      //' no scaling of Z values: '
	  read(inunit,*)secthick
	  zscal=secthick/1000.
c
	  xmag=1.
	  umperpix=1.
	  write(*,'(1x,a,/,a,$)')'Enter magnification of negatives, '//
     &	      'and scale at which negatives were digitized',
     &	      '   (microns/pixel from VIDS), or / for no '//
     &	      'scaling of X/Y values: '
	  read(inunit,*)xmag,umperpix
c
	  xyscal=umperpix/xmag
	  return
	endif
	end



	subroutine get_bundle(zstrt,zend,itype,indbundle,ninbundle,
     &	    nbundles,indfree,xmt,ymt,zmt,indxymt,indxyfree,inunit,
     &	    xyscal, zscal,tiltzstrt, remapz,costilt,ntilts,iobjnum,
     &	    icontnum,objlen)
c	  
	real*4 zstrt(*),zend(*),xmt(*),ymt(*),zmt(*),objlen(*)
	integer*4 itype(*),indbundle(*),ninbundle(*),indxymt(*)
	integer*4 iobjnum(*),icontnum(*)
	real*4 tiltzstrt(*),remapz(*),costilt(*)
	include 'model.inc'
	real*4 bx(500),by(500)
	logical*1 notgot(max_obj_num)
	logical looking,inside
c
10	if(inunit.eq.5)write(*,'(1x,a,/,a,$)')'Enter 0 to take all'//
     &	    ' objects in model, or enter the number of limiting regions'
     &	    ,'   (rectangular areas or boundary contours) to specify: '
	read(inunit,*)ncoords
	do i=1,max_mod_obj
	  notgot(i)=.true.
	enddo
	indbase=indfree
	do icoord=1,max(1,ncoords)
	  xlo=-1.e10
	  xhi=1.e10
	  ylo=xlo
	  yhi=xhi
	  zlo=-1.e10
	  zhi=1.e10
	  nvert=0
	  if(ncoords.gt.0)then
	    if(inunit.eq.5)write(*,'(1x,a,/,a,$)')'Enter IMOD Object'//
     &		' # and contour # of boundary contour,',
     &		'  or WIMP object # AND 0, or 0,0 to enter X/Y'//
     &		' coordinate limits: '
	    read(inunit,*)iobjboundin,icontbound
	    nvert=0
	    if(iobjboundin.le.0)then
	      if(inunit.eq.5)write(*,'(1x,a,$)')'Lower & upper X, lower &'
     &		  //' upper Y limits, or / for no limits: '
	      read(inunit,*)xlo,xhi,ylo,yhi
	      if(inunit.eq.5)write(*,'(1x,a,$)')'Lower and upper Z '//
     &		  'limits of region, or / for no limits: '
	      read(inunit,*)zlo,zhi
	    else
	      iobjbound=iobjfromcont(iobjboundin,icontbound)
	      if(iobjbound.eq.0)then
		print *,'Non-existent object'
		go to 10
	      endif
	      if(npt_in_obj(iobjbound).lt.3)then
		print *,'Not enough points in that object'
		go to 10
	      endif
c		
c		extract object
c		
	      zz=p_coord(3,abs(object(ibase_obj(iobjbound)+1)))
	      do i=1,npt_in_obj(iobjbound)
		ipnt=abs(object(ibase_obj(iobjbound)+i))
		if(p_coord(3,ipnt).ne.zz)then
		  print *,'Object not all in one Z plane'
		  go to 10
		endif
		if(nvert.eq.0 .or. p_coord(1,ipnt).ne.bx(max(1,nvert))
     &		    .or. p_coord(2,ipnt).ne.by(max(1,nvert)))then
		  nvert=nvert+1
		  bx(nvert)=p_coord(1,ipnt)
		  by(nvert)=p_coord(2,ipnt)
		endif
	      enddo
	      bx(nvert+1)=bx(1)
	      by(nvert+1)=by(1)
c		
	      zlo=zz-0.01
	      zhi=zz+0.01
	      if(inunit.eq.5)write(*,'(1x,a,$)')'Lower and upper Z '//
     &		  'limits of region, or / for Z value of contour only: '
	      read(inunit,*)zlo,zhi
	    endif
	  endif
c	  
	  do iobj=1,max_mod_obj
	    looking=notgot(iobj)
	    if(npt_in_obj(iobj).gt.1)then
	      if(abs(p_coord(3,abs(object(ibase_obj(iobj)+1)))-
     &		  p_coord(3,abs(object(ibase_obj(iobj)+2)))).lt.0.01)
     &		  looking=.false.
	    endif
	    i=1
	    do while(looking.and.i.le.npt_in_obj(iobj))
	      ipnt=abs(object(ibase_obj(iobj)+i))
	      if(p_coord(3,ipnt).ge.zlo.and.p_coord(3,ipnt).le.zhi)then
		if(nvert.eq.0)then
		  looking=p_coord(1,ipnt).lt.xlo.or.p_coord(1,ipnt).gt.xhi
     &		      .or.p_coord(2,ipnt).lt.ylo.or.p_coord(2,ipnt).gt.yhi
		else
		  looking=.not.
     &		      inside(bx,by,nvert,p_coord(1,ipnt),p_coord(2,ipnt))
		endif
		if(.not.looking)then
		  indfree=indfree+1
		  zstrtmp=p_coord(3,abs(object(ibase_obj(iobj)+1)))
		  zendtmp=p_coord(3,abs(object(ibase_obj(iobj)+
     &		      npt_in_obj(iobj))))
		  call objtocont(iobj,obj_color,itype(indfree),
     &		      icontnum(indfree))
c		  itype(indfree)=256-obj_color(2,iobj)
		  if(obj_color(1,iobj).eq.0)itype(indfree)=-itype(indfree)
		  iobjnum(indfree)=iobj
c		    
c		    save object x/y coordinates
c		    
		  indxymt(indfree)=indxyfree+1
		  izst=nint(zstrtmp)
		  iznd=nint(zendtmp)
		  zstrt(indfree)=scalez(zstrtmp-0.5,zscal,tiltzstrt,
     &		      remapz,costilt,ntilts)
		  zend(indfree)=scalez(zendtmp+0.5,zscal,tiltzstrt,
     &		      remapz,costilt,ntilts)
		  ii=1
		  sumlen=0.
		  do iz=izst,iznd
		    do while(nint(p_coord(3,abs(object(ibase_obj(iobj)
     &			+ii)))).lt.iz.and.ii.lt.npt_in_obj(iobj))
		      ii=ii+1
		    enddo
		    ipnt=abs(object(ibase_obj(iobj)+ii))
		    indxyfree=indxyfree+1
		    if(nint(p_coord(3,ipnt)).eq.iz)then
		      xmt(indxyfree)=xyscal*p_coord(1,ipnt)
		      ymt(indxyfree)=xyscal*p_coord(2,ipnt)
		      ii=min(ii+1,npt_in_obj(iobj))
		    else
		      iplast=abs(object(ibase_obj(iobj)+ii-1))
		      zfac=float(iz-nint(p_coord(3,iplast)))/
     &			  (nint(p_coord(3,ipnt))-nint(p_coord(3,iplast)))
		      xmt(indxyfree)=xyscal*(zfac*p_coord(1,ipnt)+
     &			  (1.-zfac)*p_coord(1,iplast))
		      ymt(indxyfree)=xyscal*(zfac*p_coord(2,ipnt)+
     &			  (1.-zfac)*p_coord(2,iplast))
		    endif
		    zmt(indxyfree)=scalez(float(iz),zscal,tiltzstrt,
     &			remapz,costilt,ntilts)
		    if(iz.eq.izst)sumlen=zmt(indxyfree)-zstrt(indfree)
		    if(iz.gt.izst)sumlen=sumlen+sqrt(
     &			(xmt(indxyfree)-xmt(indxyfree-1))**2+
     &			(ymt(indxyfree)-ymt(indxyfree-1))**2+
     &			(zmt(indxyfree)-zmt(indxyfree-1))**2)
		  enddo
		  objlen(indfree)=sumlen+zend(indfree)-zmt(indxyfree)
		endif
	      endif
	      i=i+1
	    enddo
	    notgot(iobj)=looking
	  enddo
	enddo
c	  
	nbundles=nbundles+1
	indbundle(nbundles)=indbase+1
	ninbundle(nbundles)=indfree-indbase
	print *,ninbundle(nbundles),' objects in bundle'
	indxymt(indfree+1)=indxyfree+1
	return
	end


c	  SCALEZ will scale the Z index coordinate ZZ by first remapping
c	  the Z values via the tilt remappings, then by multiplying by ZSCAL
c
	function scalez(zz,zscal,tiltzstrt,remapz,costilt,ntilts)
	real*4 tiltzstrt(*),remapz(*),costilt(*)
	scalez=zz
	if(ntilts.gt.0)then
	  if(zz.ge.tiltzstrt(1))then
	    itilt=ntilts
	    do while(zz.lt.tiltzstrt(itilt))
	      itilt=itilt-1
	    enddo
	    scalez=remapz(itilt)+(zz-tiltzstrt(itilt))/costilt(itilt)
	  endif
	endif
	scalez=scalez*zscal
	return
	end



c	  OPENCOMFILE requests a command file name and either opens that file
c	  for input instead of keyboard input, or restores keyboard input
c	  if no file name is entered, or if end of file or error occurs.
c
	subroutine opencomfile
	character*50 comfile
	logical istty/.true./
	save istty
	write(*,*) 'Enter name of file with commands,',
     &	    ' or Return for input from keyboard'
	read(5,'(a)',err=10,end=10)comfile
	if(comfile.ne.' ')go to 20
10	if(istty)return
	comfile='/dev/tty'
20	close(5)
c
c 7/20/00 remove shared, readonly for gnu
c
	open(5,file=comfile,status='old',err=10)
	istty=comfile.eq.'/dev/tty'
	return
	end


	logical function typeonlist(itype,ityplist,ntyplist)
	integer*4 ityplist(*)
	typeonlist=.true.
	if(ntyplist.eq.0)return
	do i=1,ntyplist
	  if(itype.eq.ityplist(i).or.ityplist(i).eq.0)return
	enddo
	typeonlist=.false.
	return
	end
	
	
	subroutine calcoverlap(zstrt,zend,xmt,ymt,zmt,indxymt,
     &	    iovertype,rnull,rzero,ipower,xlambda,inda,indb,
     &	    zlow,zhigh,zshared, overlap,sepavg,sepsd)
	integer*4 indxymt(*)
	real*4 zstrt(*),zend(*),xmt(*),ymt(*),zmt(*)
	zovstrt=max(zstrt(inda),zstrt(indb),zlow)
	zovend=min(zend(inda),zend(indb),zhigh)
	zshared=zovend-zovstrt
	if(zshared.gt.0.)then
	  ipta=indxymt(inda)
	  iptb=indxymt(indb)
	  do while (zmt(ipta).lt.zovstrt)
	    ipta=ipta+1
	  enddo
	  do while (zmt(iptb).lt.zovstrt)
	    iptb=iptb+1
	  enddo
	  delz=zshared
	  overlap=0.
	  rsum=0.
	  rsqsum=0.
	  nsum=0
	  rnullsq=rnull**2
	  do while (zmt(ipta).le.zovend .and.
     &	      ipta.lt.indxymt(inda+1))
	    delx=xmt(ipta)-xmt(iptb)
	    if(delx.le.rnull)then
	      dely=ymt(ipta)-ymt(iptb)
	      if(dely.le.rnull)then
		rsq=delx**2+dely**2
		if(rsq.le.rnullsq)then
		  rr=sqrt(rsq)
		  if(ipta+1.lt.indxymt(inda+1))
     &		      delz=zmt(ipta+1)-zmt(ipta)
		  if(iptb+1.lt.indxymt(indb+1))
     &		      delz=zmt(iptb+1)-zmt(iptb)
		  if(iovertype.lt.2)then
		    overlap=overlap+delz
		  else
		    relr=max(1.,rr/rzero)
		    if(iovertype.eq.2)then
		      overlap=overlap+delz*(1./relr)**ipower
		    else
		      overlap=overlap+delz*exp((1.-relr)/xlambda)
		    endif
		  endif
		  rsum=rsum+rr
		  rsqsum=rsqsum+rsq
		  nsum=nsum+1
		endif
	      endif
	    endif
	    ipta=ipta+1
	    iptb=iptb+1
	  enddo
	  call sums_to_avgsd(rsum,rsqsum,nsum,sepavg,sepsd)
	endif
	return
	end

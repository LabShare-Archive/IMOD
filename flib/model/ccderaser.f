****	  CCDERASER.FOR
c	  
c	  This program replaces deviant pixels with interpolated values from
c	  surrounding pixels.  It is designed to correct defects in electron
c	  microscope images from CCD cameras.  It can use two algorithms to
c	  automatically remove peaks in intensity caused by X-rays.  It can
c	  also take an IMOD model file with specifications of regions to be
c	  replaced.  With a model, the program can replace a group of adjacent
c	  pixels with interpolated values, or all of the pixels along a line.
c	  It can do this on only a specific image, or on all of the sections
c	  in the file.  The program can operate in trial mode, without making
c	  an output file, and it can output a model file with points at the
c	  pixels to be replaced.
c	  
c	  See the man page for a description of the operation and inputs.
c       
c	  David Mastronarde 11/10/98
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.13  2003/11/03 23:37:53  mast
c	  Made it put out an empty model if there are no points to replace
c	
c	  Revision 3.12  2003/10/30 06:32:24  mast
c	  Limited number of pixels in difference patches to avoid erasing gold
c	  particles in binned-down images.
c	
c	  Revision 3.11  2003/10/10 20:42:23  mast
c	  Used new subroutine for getting input/output files
c	
c	  Revision 3.10  2003/10/09 02:33:42  mast
c	  converted to use autodoc
c	
c	  Revision 3.9  2003/06/20 23:55:37  mast
c	  Converted to using one lone string for options
c	
c	  Revision 3.8  2003/06/20 22:08:46  mast
c	  Replaced option initialization with assignments to make SGI happy
c	
c	  Revision 3.7  2003/06/20 20:20:29  mast
c	  Renamed two options
c	
c	  Revision 3.6  2003/06/11 20:36:57  mast
c	  Reorganize and rename a few options
c	
c	  Revision 3.5  2003/06/10 20:42:07  mast
c	  New version with automatic peak detection and PIP input
c	
c	  Revision 3.4  2002/09/09 21:36:00  mast
c	  Eliminate stat_source: and nimp_source: from all includes
c	
c	  Revision 3.3  2002/07/21 19:43:39  mast
c	  Moved big array into a common block to avoid stack size limit on SGI
c	
c	  Revision 3.2  2002/07/21 19:31:59  mast
c	  Standardized error outputs
c	
c	  Revision 3.1  2002/07/07 20:14:16  mast
c	  Modified to use scale factors from model file instead of from image
c	  file to get back to index coordinates.  Made it exit with error
c	  status on all errors, made it insert a proper title, and implemented
c	  implicit none.
c	
c	  
	implicit none
	integer imsiz,mxd,limpatch,limobj,limptout,limdiff,limpatchout
	parameter (imsiz=4100, limdiff = 512, limpatchout=50000)
	parameter (mxd=50,limpatch=5000,limobj=1000,limptout=100000)
	real*4 array(imsiz*imsiz),title(20),orig(3),delt(3)
	real*4 diffArr(limdiff, limdiff), exceedCrit(limpatchout)
	integer*4 nxyz(3),mxyz(3),nx,ny,nz
	equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
	character*120 infile,outfile,ptfile,modelout
	integer*4 ixfix(limpatch),iyfix(limpatch)
	integer*4 ixout(limptout),iyout(limptout),izout(limptout)
	integer*4 indPatch(limpatchout)
	integer*4 iobjline(limobj),iobjdoall(limobj)
	include 'model.inc'
	common /bigcom/array,diffArr
	logical readw_or_imod, typeonlist
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech, line
	character dat*9, tim*8
c	  
	integer*4 mode,imfilout,i,j,nobjdoall,nobjline,nborder,iorder
	integer*4 ifincadj,iobj,ibase,itype,imodobj,imodcont,ip,ipt
	real*4 dmin,dmax,dmean,tmin,tmax,tsum,dmint,dmaxt,dmeant,tmean
	real*4 zmin,zmax,xmin,xmax,ymin,ymax
	integer*4 izsect,nfix,linefix,ip1,ip2,ix1,ix2,iy1,iy2,kti,ninobj
	integer*4 ierr,ierr2,ifflip,numPtOut, numPatchOut
	integer*4 getimodhead,getimodscales
	real*4 xyscal,zscale,xofs,yofs,zofs,ximscale, yimscale, zimscale
	real*4 critMain, critGrow, critScan, critDiff, radiusMax, outerRadius
	real*4 scanOverlap, annulusWidth
	integer*4 ifPeakSearch, iScanSize,ifVerbose,numPatch,numPixels
	integer*4 ifTrialMode,nEdgePixels, maxObjectsOut, maxInDiffPatch
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger,PipGetBoolean
	integer*4 PipGetString,PipGetFloat
	integer*4 PipGetNonOptionArg, PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  ccderaser
c
	integer numOptions
	parameter (numOptions = 24)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'find:FindPeaks:B:@peak:PeakCriterion:F:@'//
     &      'diff:DiffCriterion:F:@grow:GrowCriterion:F:@'//
     &      'scan:ScanCriterion:F:@radius:MaximumRadius:F:@'//
     &      'maxdiff:MaxPixelsInDiffPatch:I:@outer:OuterRadius:F:@'//
     &      'width:AnnulusWidth:F:@xyscan:XYScanSize:I:@'//
     &      'edge:EdgeExclusionWidth:I:@points:PointModel:FN:@'//
     &      'model:ModelFile:FN:@lines:LineObjects:LI:@'//
     &      'allsec:AllSectionObjects:LI:@border:BorderSize:I:@'//
     &      'order:PolynomialOrder:I:@exclude:ExcludeAdjacent:B:@'//
     &      'trial:TrialMode:B:@verbose::B:@param:ParameterFile:PF:@'//
     &      'help:usage:B:'
c
c	  Set all defaults here
c
	nobjdoall=1
	iobjdoall(1)=-999
	nobjline=1
	iobjline(1)=-999
	nborder=2
	iorder=2
	ifincadj=1
	critMain = 10.
	critDiff = 10.
	critGrow = 4.0
	critScan = 3.0
	radiusMax = 2.1
	outerRadius = 4.1
	scanOverlap = 0.1
	ifPeakSearch = 0
	iScanSize = 100
	ifVerbose = 0
	ifTrialMode = 0
	nEdgePixels = 0
	maxInDiffPatch = 2

	call date(dat)
	call time(tim)
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'ccderaser',
     &	    'ERROR: CCDERASER - ', .true., 2, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0
c
	if (PipGetInOutFile('InputFile', 1, 'Name of input file', infile)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
c	call irtdel(1,delt)
c	call irtorg(1,orig(1),orig(2),orig(3))
c	  
	outfile = ' '
	if (pipinput) then
c	    
c	    Get an output file string; or if none, look on command line
c
	  ierr = PipGetBoolean('TrialMode', ifTrialMode)
	  ierr = PipGetString('OutputFile', outfile)
	  if (ierr .gt. 0 .and. numNonOptArg .gt. 1)
     &	      ierr = PipGetNonOptionArg(2, outfile)
	else
	  print *,'Enter output file name (Return to put modified'//
     &	      ' sections back in input file)'
	  read(*,'(a)')outfile
	endif
c	  
	if(ifTrialMode .ne. 0 .or. outfile.ne.' ')then
	  call imopen(1,infile,'ro')
	else
	  call imopen(1,infile,'old')
	endif

	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
	if(nx*ny.gt.imsiz**2) call errorexit
     &	    ('IMAGE TOO LARGE FOR ARRAYS')

	imfilout=1
	if(ifTrialMode .eq. 0 .and. outfile.ne.' ')then
	  imfilout=2
	  call imopen(2,outfile,'new')
	  call itrhdr(2,1)
	endif
c	  
	max_mod_obj = 0
	if (pipinput) then
	  ierr = PipGetString('ModelFile', ptfile)
	else
	  write(*,'(1x,a,$)')'Model file: '
	  read(*,'(a)')ptfile
	endif
	if (.not. pipinput .or. ierr .eq. 0) then
	  if(.not.readw_or_imod(ptfile))call errorexit
     &	      ('READING MODEL FILE')

	  ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	  ierr2 = getimodscales(ximscale, yimscale, zimscale)
	  if (ierr .ne. 0 .or. ierr2 .ne. 0)
     &	      call errorexit('getting model header')

	  do i=1,n_point
	    p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
	    p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
	    p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
c	  do j=1,3
c	    p_coord(j,i)=(p_coord(j,i)+orig(j))/delt(j)
c	  enddo
	  enddo
	endif
c	  
	if (pipinput) then
c	    
c	    get old parameters for model based erasing
c
	  if (PipGetString('AllSectionObjects', line) .eq. 0)then
	    call parselist(line, iobjdoall,nobjdoall)
	  else
	    nobjdoall = 0
	  endif

	  if (PipGetString('LineObjects', line) .eq. 0)then
	    call parselist(line, iobjline,nobjline)
	  else
	    nobjline = 0
	  endif

	  ierr = PipGetInteger('BorderSize', nborder)
	  ierr = PipGetInteger('PolynomialOrder', iorder)
	  if (PipGetBoolean('ExcludeAdjacent', i) .eq. 0) ifincadj = 0
c	    
c	    get the new parameters for auto peak search
c	    
	  ierr = PipGetBoolean('FindPeaks', ifPeakSearch)
	  ierr = PipGetInteger('XYScanSize', iScanSize)
	  ierr = PipGetInteger('EdgeExclusionWidth', nEdgePixels)
	  ierr = PipGetFloat('MaximumRadius', radiusMax)
	  ierr = PipGetFloat('PeakCriterion', critMain)
	  ierr = PipGetFloat('GrowCriterion', critGrow)
	  ierr = PipGetFloat('ScanCriterion', critScan)
	  ierr = PipGetFloat('DiffCriterion', critDiff)
	  ierr = PipGetInteger('verbose', ifVerbose)
	  ierr = PipGetInteger('MaxPixelsInDiffPatch', maxInDiffPatch)
	  ierr = PipGetFloat('OuterRadius', outerRadius)
	  ierr2 = PipGetFloat('AnnulusWidth', annulusWidth)
	  if (ierr .eq. 0 .and. ierr2 .eq. 0) call errorexit(
     &	      'YOU CANNOT ENTER BOTH -outer AND -width')
	  if (ierr2 .eq. 0) outerRadius = radiusMax + annulusWidth

	  modelout = ' '
	  ierr = PipGetString('PointModel', modelout)
	else
c	    
c	    interactive input for old parameters only
c
	  print *,'Enter list of objects specifying replacement on ALL'//
     &	      ' sections (/ for all, Return for none)'
c	  
	  call rdlist(5,iobjdoall,nobjdoall)
c	    
	  print *,'Enter list of objects specifying horizontal or vertical'//
     &	      ' line replacements',' (/ for all, Return for none)'
c	    
	  call rdlist(5,iobjline,nobjline)
c	    
	  write(*,'(1x,a,i2,a,$)')'border size [/ for',nborder,']: '
	  read(*,*)nborder
c	    
	  write(*,'(1x,a,i2,a,$)')'order of 2-D polynomial fit [/ for'
     &	      ,iorder,']: '
	  read(*,*)iorder
c	  
	  write(*,'(1x,a,$)')
     &	      '0 to exclude or 1 to include adjacent points in fit: '
	  read(5,*)ifincadj
	endif
	call PipDone()

	if (max_mod_obj .eq. 0 .and. ifPeakSearch .eq. 0) call errorexit
     &	    ('NO MODEL POINTS AND NO PEAK SEARCH SPECIFIED')
c	  
c	  check input values, set reasonable limits
c	  
	nborder = min(5, max(1, nborder))
	iorder = min(3, max(1, iorder))
	iScanSize = min(limdiff - 10, max(20, iScanSize))
	radiusMax = min(10., max(0.5, radiusMax))
	outerRadius = min(radiusMax + 10., max(radiusMax + 0.75, outerRadius))
	if (critMain .eq. 0) critMain = 1000.
	critMain = max(2., critMain)
	if (critDiff .eq. 0) critDiff = 1000.
	critDiff = max(2., critDiff)
	critScan = max(2., critScan)
	critGrow = max(2., critGrow)
	nEdgePixels = max(0, min(nx / 3, nEdgePixels))

	if (ifPeakSearch .gt. 0) then
	  numPatchOut = 0
	  indPatch(1) = 1
	  numPtOut = 0
	endif

	print *
	tmin=1.e10
	tmax=-1.e10
	tsum=0.
c       
	do iobj=1,max_mod_obj
	  if(npt_in_obj(iobj).gt.0)then
	    ibase=ibase_obj(iobj)
	    itype = 256-obj_color(2,iobj)
	    call objtocont(iobj,obj_color,imodobj,imodcont)
	    if(typeonlist(itype,iobjline,nobjline)) then
	      if(npt_in_obj(iobj).ne.2) then
		print *,'ERROR: CCDERASER - object',imodobj,
     &		    ', contour',imodcont,
     &              ' does not have only two points'
		call exit(1)
	      endif
	      ip1 = object(ibase+1)
	      ip2 = object(ibase+2)
	      if (nint(p_coord(3,ip1)).ne.nint(p_coord(3,ip2))) then
		print *,'ERROR: CCDERASER - object',imodobj,', contour',
     &		    imodcont, ' is supposed to be a line and ',
     &              ' is not in one Z-plane'
		call exit(1)
	      elseif(nint(p_coord(1,ip1)).ne.nint(p_coord(1,ip2)).and.
     &		    nint(p_coord(2,ip1)).ne.nint(p_coord(2,ip2))) then
		print *,'ERROR: CCDERASER - object',imodobj,', contour',
     &		    imodcont, ' is supposed to be a line and ',
     &		    'is not horizontal or vertical'
		call exit(1)
	      endif
	    else
	      zmin=1.e10
	      zmax=-1.e10
	      xmin=zmin
	      xmax=zmax
	      ymin=zmin
	      ymax=zmax
	      ninobj=npt_in_obj(iobj)
	      if (ninobj.gt.limpatch)then
		print *,'ERROR: CCDERASER - object',imodobj,', contour',
     &		    imodcont, ' has too many points for arrays'
		call exit(1)
	      endif
	      do ip=1,ninobj
		ipt=object(ibase+ip)
		xmin=min(xmin,p_coord(1,ipt))
		xmax=max(xmax,p_coord(1,ipt))
		ymin=min(ymin,p_coord(2,ipt))
		ymax=max(ymax,p_coord(2,ipt))
		zmin=min(zmin,p_coord(3,ipt))
		zmax=max(zmax,p_coord(3,ipt))
	      enddo
	      if(xmax-xmin.ge.mxd/2 .or.ymax-ymin.ge.mxd/2)then
		print *,'ERROR: CCDERASER - object',imodobj,', contour',
     &		    imodcont, ' has points too far apart, ',
     &		    'so patch is too large'
		call exit(1)
	      endif
	      if(.not.typeonlist(itype,iobjdoall,nobjdoall).and.
     &            zmax.ne.zmin)then
		print *,'ERROR: CCDERASER - object',imodobj,', contour',
     &		    imodcont, ' is not in one Z-plane'
		call exit(1)
	      endif
	    endif
	  endif
	enddo
c	  
c	  start looping on sections; need to read regardless
c	  
	do izsect=0,nz-1
	  write(*,101)izsect
101	  format('Section',i5,' -',$)
	  call irdsec(1,array,*99)
	  nfix=0
	  linefix =0
c	    
c	    scan through points to see if any need fixing
c	    
	  do iobj=1,max_mod_obj
	    if(npt_in_obj(iobj).gt.0)then
	      ibase=ibase_obj(iobj)
	      itype = 256-obj_color(2,iobj)
c             
c             first see if this has a line to do
c
	      if(typeonlist(itype,iobjline,nobjline))then
		if(nint(p_coord(3,object(ibase+1))).eq.izsect .or.
     &              typeonlist(itype,iobjdoall,nobjdoall)) then
		  if(linefix.eq.0)write(*,'(a,$)')' fixing lines -' 
		  linefix=linefix+1
		  ip1=object(ibase+1)
		  ip2=object(ibase+2)
		  ix1=p_coord(1,ip1)+1.01
		  ix2=p_coord(1,ip2)+1.01
		  iy1=p_coord(2,ip1)+1.01
		  iy2=p_coord(2,ip2)+1.01
		  call cleanline(array,nx,ny,ix1,iy1,ix2,iy2)
		endif
	      elseif(nint(p_coord(3,object(ibase+1))).eq.izsect .or.
     &		  typeonlist(itype,iobjdoall,nobjdoall))then
		ninobj=npt_in_obj(iobj)
		do ip=1,ninobj
		  ipt=object(ibase+ip)
		  ixfix(ip)=p_coord(1,ipt)+1.01
		  iyfix(ip)=p_coord(2,ipt)+1.01
		enddo
		if(nfix.eq.0)write(*,'(a,$)')' fixing points -' 
		nfix=nfix+1
		call cleanarea(array,nx,ny,nx,ny,ixfix
     &		  ,iyfix,ninobj,nborder,iorder,ifincadj,ifVerbose)
	      endif
	    endif
	  enddo
c	    
c	    do peak search for automatic removal
c
	  numPatch = 0
	  if (ifPeakSearch .gt. 0) call searchpeaks(array,nx,ny,
     &	      diffArr, limdiff, izsect, iScanSize, scanOverlap,
     &	      nEdgePixels, critMain, critDiff, critGrow, critScan,
     &	      radiusMax, outerRadius, nborder,iorder,ifincadj, ixfix,
     &	      iyfix, limpatch,numPatch,numPixels,ifVerbose,
     &	      numPtOut, numPatchOut, indPatch, exceedCrit,
     &	      maxInDiffPatch, limpatchout,
     &	      ixout, iyout, izout, limptout)
	  if (numPatch .gt. 0)write(*,102)numPixels,numPatch
102	  format(i5,' pixels replaced in',i4,' peaks -',$)

	  call iclden(array,nx,ny,1,nx,1,ny,dmint,dmaxt,dmeant)
	  tmin=min(tmin,dmint)
	  tmax=max(tmax,dmaxt)
	  tsum=tsum+dmeant
c	    
c	    write out if any changes or if new output file
c
	  if((nfix.gt.0.or.linefix.gt.0.or. numPatch.gt.0.or.imfilout.eq.2)
     &	      .and. ifTrialMode .eq. 0)then
	    call imposn(imfilout,izsect,0)
	    call iwrsec(imfilout,array)
	  endif
	  write(*,'(a)')' Done'
	  call flush(6)
	enddo
c
	tmean=tsum/nz

c
c 7/7/00 CER: remove the encodes
c
c       encode(80,109,title)
        write(titlech,109)dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20)
109	format('CCDERASER: Bad points replaced with interpolated values'
     &	    , t57, a9, 2x, a8 )
	if (ifTrialMode .eq. 0) then
	  call iwrhdr(imfilout,title,1,tmin,tmax,tmean)
	else
	  print *,'New minimum and maximum density would be:',tmin, tmax
	endif
	call imclose(imfilout)
	
	maxObjectsOut = 4
c	  
c	  put out a model, even if there are no points
c
	if (modelout .ne. ' ')then
	  max_mod_obj = numPatchOut
	  n_point = numPtOut
	  do i = 1, n_point
	    p_coord(1,i) = ixout(i) - 0.5
	    p_coord(2,i) = iyout(i) - 0.5
	    p_coord(3,i) = izout(i)
	    object(i) = i
	  enddo
	  do iobj = 1, numPatchOut
	    obj_color(2, iobj) = 256 - 
     &		max(1, min(maxObjectsOut, int(1. + exceedCrit(iobj))))
	    obj_color(1, iobj) = 1
	    npt_in_obj(iobj) = indPatch(iobj + 1) - indPatch(iobj)
	    ibase_obj(iobj) = indPatch(iobj) - 1
	  enddo
	  call newimod()
	  do i = 1, maxObjectsOut
	    call putimodflag(i, 2)
	    call putsymtype(i, 0)
	    call putsymsize(i, 5)
	  enddo
	  call write_wmod(modelout)
	  write(*,105)maxObjectsOut,maxObjectsOut,maxObjectsOut
105	  format('In the output model, contours have been sorted into',i3,
     &	      ' objects based on how',/,
     &	      ' much a peak exceeds the criterion.',/,
     &	      ' Object 1 has peaks that exceed the criterion by < 1 SD,'
     &	      ,/, ' object 2 has peaks that exceed the criterion by ',
     &	      '1 - 2 SDs,',/, ' object ',i2,
     &	      ' has peaks that exceed the criterion by >',i2, ' SDs')
	endif

	call exit(0)
99	call errorexit('READING FILE')
	end


	subroutine searchpeaks(array,nx,ny,diffarr,limdiff, izsect,
     &	    iScanSize,scanOverlap, nEdgePixels, critMain, critDiff,
     &	    critGrow, critScan,
     &	    radiusMax, outerRadius, nborder,iorder,ifincadj, ixfix,
     &	    iyfix, limpatch,numPatch, numPixels,ifVerbose, numPtOut,
     &	    numPatchOut, indPatch, exceedCrit, maxInDiffPatch,
     &	    limpatchout, ixout, iyout, izout, limptout)

	implicit none
	integer limlist
	parameter (limlist=400)
	integer*4 iScanSize,nx,ny,nborder,iorder,ifincadj, limpatch, izsect
	integer*4 ixfix(*), iyfix(*),ifVerbose,limdiff,limptout,limpatchout
	real*4 critMain, critGrow, critScan, critDiff, radiusMax, outerRadius
	real*4 scanOverlap, array(nx,ny), diffarr(limdiff,limdiff)
	real*4 exceedCrit(*)
	integer*4 numPtOut, numPatchOut, indPatch(*),ixout(*),iyout(*),izout(*)
	integer*4 nEdgePixels, maxInDiffPatch, numPtSave, numPatchSave
	integer*4 numScanX, numScanY, nxScan, nyScan, iScanY, iScanX, iyStart
	integer*4 iyEnd, ixStart, ixEnd, jx, jxs, jxn, jy, jys, jyn, ix, iy
	real*4 dmin, dmax, sum, sumsq, scanAvg, scanSd, polarity, scanCrit
	integer*4 ixPeak, iyPeak, iouter, nsum, ninPatch, numPatch, numPixels
	real*4 radMaxSq, outerSq, radSq, ringAvg, ringSd, growCrit, sdDiff
	real*4 pixDiff, diffAvg, diffSd, diffCrit
	logical movedPeak, storePatch, onList
	integer*4 ninList, ixlist(limlist), iylist(limlist), lookingAt, i, j
	integer*4 minDistSq, maxDistSq, iDistSq, ixofs, iyofs
	integer*4 ixmin, ixmax, iymin, iymax, nxuse, nyuse


	numPatch = 0
	numPixels = 0
	nxuse = nx - 2 * nEdgePixels
	nyuse = ny - 2 * nEdgePixels
c	  
c	  set up extent of scan regions
c	  
	numScanX = max(1., (nxuse - scanOverlap * iScanSize) /
     &	    (iScanSize * (1. - scanOverlap)))
	nxScan = nxuse / (numScanX - (numScanX - 1) * scanOverlap)
	do while (nxScan .gt. limdiff - numScanX)
	  numScanX = numScanX + 1
	  nxScan = nxuse / (numScanX - (numScanX - 1) * scanOverlap)
	enddo	  

	numScanY = max(1., (nyuse - scanOverlap * iScanSize) /
     &	    (iScanSize * (1. - scanOverlap)))
	nyScan = nyuse / (numScanY - (numScanY - 1) * scanOverlap)
	do while (nyScan .gt. limdiff - numScanY)
	  numScanY = numScanY + 1
	  nyScan = nyuse / (numScanY - (numScanY - 1) * scanOverlap)
	enddo	  
c	  
c	  loop on scan regions, getting start and end in each dimension
c
	do iScanY = 1, numScanY
	  iyStart = 1 + nEdgePixels + (nyuse - nyScan) * (iScanY - 1) /
     &	      max(1, numScanY - 1)
	  iyEnd = iyStart + nyScan - 1
	  if (iScanY .eq. numScanY) iyEnd = ny - nEdgePixels
	  do iScanX = 1, numScanX
	    ixStart = 1 + nEdgePixels + (nxuse - nxScan) * (iScanX - 1) /
     &		max(1, numScanX - 1)
	    ixEnd = ixStart + nxScan - 1
c	    print *,'scanning',ixStart, ixEnd,iyStart,iyEnd
	    if (iScanX .eq. numScanX) ixEnd = nx - nEdgePixels
c	      
c	      get statistics and scan for points outside the reduced criterion
c

	    call iclavgsd(array, nx, ny, ixStart, ixEnd, iyStart, iyEnd,
     &		dmin, dmax, sum, sumsq, scanAvg, scanSd)
	    scanCrit = critScan * scanSd

	    do iy = iyStart, iyEnd
	      do ix = ixStart, ixEnd
		if (abs(array(ix,iy) - scanAvg) .gt. scanCrit) then
c		    
c		    found a point above criterion, need to walk to peak
c		    
		  polarity = sign(1., array(ix,iy) - scanAvg)
		  movedPeak = .true.
		  ixPeak = ix
		  iyPeak = iy
		  do while (movedPeak)
		    movedPeak = .false.
		    jxs = max(1,ixPeak - 1)
		    jxn = min(nx, ixPeak + 1)
		    jys = max(1,iyPeak - 1)
		    jyn = min(ny, iyPeak + 1)
		    do jy = jys,jyn
		      do jx = jxs, jxn
			if (polarity * (array(jx,jy) - array(ixPeak, iyPeak))
     &			    .gt. 0.) then
			  ixPeak = jx
			  iyPeak = jy
			  movedPeak = .true.
			endif
		      enddo
		    enddo
		  enddo
c		    
c		    find mean and SD in the annulus
c
		  radMaxSq = radiusMax**2
		  outerSq = outerRadius**2
		  iouter = nint(outerRadius)
		  jxs = max(1, ixPeak - iouter)
		  jxn = min(nx, ixPeak + iouter)
		  jys = max(1, iyPeak - iouter)
		  jyn = min(ny, iyPeak + iouter)
		  nsum = 0
		  sum = 0.
		  sumsq = 0.
		  do jy = jys,jyn
		    do jx = jxs, jxn
		      radSq = (jx - ixPeak)**2 + (jy - iyPeak)**2
		      if (radSq .gt. radMaxSq .and. radSq .le. outerSq) then
			nsum = nsum +1
			sum = sum + array(jx,jy)
			sumsq = sumsq + array(jx,jy)**2
		      endif
		    enddo
		  enddo
		  call sums_to_avgsd(sum, sumsq, nsum, ringAvg, ringSd)
c		    
c		    Make sure the peak passes the main criterion
c
		  sdDiff = abs(array(ixPeak, iyPeak) - ringAvg) / ringSd
		  if (sdDiff .gt. critMain) then
c		      
c		      look inside radius and make list of points above the 
c		      grow criterion or above the difference criterion
c
		    growCrit = ringSd * critGrow
		    iouter = nint(radiusMax)
		    jxs = max(1, ixPeak - iouter)
		    jxn = min(nx, ixPeak + iouter)
		    jys = max(1, iyPeak - iouter)
		    jyn = min(ny, iyPeak + iouter)
		    ninPatch = 0
		    storePatch = (numPatchOut .lt. limpatchout - 1 .and.
     &			numPtOut .lt. limptout)
		    do jy = jys,jyn
		      do jx = jxs, jxn
			radSq = (jx - ixPeak)**2 + (jy - iyPeak)**2
			if (radSq .lt. radMaxSq .and.
     &			    polarity * (array(jx,jy) - ringAvg) .gt. growCrit
     &			    .and. ninPatch .lt. limpatch) then
			  ninPatch=ninPatch + 1
			  ixfix(ninPatch) = jx
			  iyfix(ninPatch) = jy
			  if (storePatch .and. numPtOut .lt. limPtOut) then
			    numPtOut = numPtOut + 1
			    ixout(numPtOut) = jx
			    iyout(numPtOut) = jy
			    izout(numPtOut) = izsect
			  endif
			endif
		      enddo
		    enddo
		    if (storePatch) then
		      exceedCrit(numPatchOut + 1) = sdDiff - critMain
		      numPatchOut = numPatchOut + 1
		      indPatch(numPatchOut + 1) = numPtOut + 1
		    endif
c		      
c		      fix the patch!
c		      
		    if (ifVerbose.gt.0)write (*,103)ixPeak,iyPeak,
     &			array(ixPeak,iyPeak),sdDiff, ringAvg
103		    format(/,'Peak at',2i6,' = ',f8.0,',',f7.2,
     &			' SDs above mean',f8.0,$)
		    call cleanarea(array,nx,ny,nx,ny,ixfix,iyfix,
     &			ninPatch,nborder,iorder,ifincadj,ifVerbose)
		    numPatch=numPatch + 1
		    numPixels=numPixels + ninPatch
		  endif
		endif
	      enddo
	    enddo

c	      
c	      to search for single-pixel difference anomalies
c	      get mean of difference from neighbors
c	      
	    ixofs = 1 - ixStart
	    iyofs = 1 - iyStart
	    call compute_diffs(array, nx, ny, diffArr, limdiff, limdiff,
     &		ixStart + 1, ixEnd - 1, iyStart + 1, iyEnd - 1,
     &		ixofs, iyofs, diffAvg, diffSd)
	    diffCrit = critDiff * diffSd
	    growCrit = critGrow * diffSd

	    do iy = iyStart + 1, iyEnd - 1
	      do ix = ixStart + 1, ixEnd - 1
		pixDiff = diffArr(ix + ixofs, iy + iyofs)
		if (abs(pixDiff - diffAvg) .gt. diffCrit) then
c
c		    Make a list of adjacent points that exceed criterion in
c		    either direction.  Accumulate sum of points on list
c		    and neighboring  points not on list
c
		  ninList = 1
		  ixlist(1) = ix
		  iylist(1) = iy
		  lookingAt = 1
		  ixmin = ix
		  ixmax = ixmin
		  iymin = iy
		  iymax = iymin
		  sum = array(ix, iy)
		  do while (lookingAt .le. ninList)
		    do jx = ixlist(lookingAt) - 1, ixlist(lookingAt) + 1
		      do jy = iylist(lookingAt) - 1, iylist(lookingAt) + 1
c			  
c			  compute min and max distance from existing points
c
			minDistSq = (jx - ixlist(1))**2 + (jy - iylist(1))**2
			maxDistSq = minDistSq
			do i = 1, ninList
			  iDistSq = (jx - ixlist(i))**2 + (jy - iylist(i))**2
			  minDistSq = min(minDistSq, iDistSq)
			  maxDistSq = max(maxDistSq, iDistSq)
			enddo
c			  
c			  if point not on list and is within diameter and is
c			  interior and exceeds grow criterion, add to list
c
			if (minDistSq .gt. 0 .and. maxDistSq .le. 4. * radMaxSq
     &			    .and. jx .gt. ixStart .and. jx .lt. ixEnd .and.
     &			    jy .gt. iyStart .and. jy .lt. iyEnd .and.
     &			    abs(diffArr(jx + ixofs, jy + iyofs) - diffAvg)
     &			    .gt. growCrit .and. ninList .le. limlist) then
			  ninList = ninList + 1
			  ixlist(ninList) = jx
			  iylist(ninList) = jy
			  ixmin = min(ixmin, jx)
			  ixmax = max(ixmax, jx)
			  iymin = min(iymin, jy)
			  iymax = max(iymax, jy)
			  sum = sum + array(jx, jy)
			endif
		      enddo
		    enddo
		    lookingAt=lookingAt + 1
		  enddo
c		    
c		    get polarity from mean of points on list versus mean
c		    of neighboring points
c		    
		  sumsq = 0.
		  do jx = ixmin - 1, ixmax + 1
		    do jy = iymin - 1, iymax + 1
		      sumsq = sumsq + array(jx, jy)
		    enddo
		  enddo
		  sumsq = (sumsq - sum) /
     &		      ((ixmax + 3 - ixmin) * (iymax + 3 - iymin) - ninList)
		  polarity = sign(1., sum / ninList - sumsq)
c
c		    and order the list by differences
c		    

		  do i = 1, ninList - 1
		    do j = i + 1, ninList
		      if ((diffArr(ixlist(i) + ixofs, iylist(i) + iyofs) -
     &			  diffArr(ixlist(j) + ixofs, iylist(j) + iyofs)) *
     &			  polarity .lt. 0.) then
			jx = ixlist(i)
			ixlist(i) = ixlist(j)
			ixlist(j) = jx
			jy = iylist(i)
			iylist(i) = iylist(j)
			iylist(j) = jy
		      endif
		    enddo
		  enddo
c		    
c		    Move points to fix list, starting with strongest, and
c		    for each other point, recompute difference measure
c		    excluding the ones already on fix list
c
		  ninPatch = 1
		  ixfix(1) = ixlist(1)
		  iyfix(1) = iylist(1)
		  ixmin = ixfix(1)
		  ixmax = ixmin
		  iymin = iyfix(1)
		  iymax = iymin
		  storePatch = (numPatchOut .lt. limpatchout - 1 .and.
     &		      numPtOut .lt. limptout)
		  numPtSave = numPtOut
		  numPatchSave = numPatchOut
		  if (storePatch) then
		    numPtOut = numPtOut + 1
		    ixout(numPtOut) = ixlist(1)
		    iyout(numPtOut) = iylist(1)
		    izout(numPtOut) = izsect
		  endif
		  do i = 2, ninList
		    nsum = 0
		    sum = 0.
		    do jx = ixlist(i) - 1, ixlist(i) + 1
		      do jy = iylist(i) - 1, iylist(i) + 1
			onList = jx .eq. ixlist(i) .and. jy .eq. iylist(i)
			do j = 1, ninPatch
			  if (jx .eq. ixfix(j) .and. jy .eq. iyfix(j))
     &			      onList = .true.
			enddo
			if (.not.onList) then
			  sum = sum + array(jx,jy)
			  nsum = nsum + 1
			endif
		      enddo
		    enddo
c		      
c		      add point to list if it passes the grow criterion
c
		    if (nsum .gt. 0) then
		      pixDiff = array(ixlist(i), iylist(i)) - sum / nsum
		      if (polarity * (pixDiff - diffAvg) .gt. growCrit) then
			ninPatch=ninPatch + 1
			ixfix(ninPatch) = ixlist(i)
			iyfix(ninPatch) = iylist(i)
			ixmin = min(ixmin, ixlist(i))
			ixmax = max(ixmax, ixlist(i))
			iymin = min(iymin, iylist(i))
			iymax = max(iymax, iylist(i))
			if (storePatch .and. numPtOut .lt. limPtOut) then
			  numPtOut = numPtOut + 1
			  ixout(numPtOut) = ixlist(i)
			  iyout(numPtOut) = iylist(i)
			  izout(numPtOut) = izsect
			endif
		      endif
		    endif
		  enddo
		  
		  if (ninPatch .le. maxInDiffPatch) then
		    pixDiff = diffArr(ixfix(1) + ixofs, iyfix(1) + iyofs)
		    sdDiff = abs(pixDiff - diffAvg) / diffSd
		    if (storePatch) then
		      exceedCrit(numPatchOut + 1) = sdDiff - critDiff
		      numPatchOut = numPatchOut + 1
		      indPatch(numPatchOut + 1) = numPtOut + 1
		    endif
c		      
c		      report if verbose, fix the patch
c		      
		    if (ifVerbose.gt.0)write (*,104)ixfix(1),iyfix(1),
     &			array(ix,iy),pixDiff,sdDiff
104		    format(/,'Diff peak at',2i6,' = ',f8.0,', diff =',f8.0,
     &			', ',f7.2, ' SDs above mean',$)
		    call cleanarea(array,nx,ny,nx,ny,ixfix,iyfix,
     &			ninPatch,nborder,iorder,ifincadj,ifVerbose)
		    numPatch=numPatch + 1
		    numPixels=numPixels + ninPatch
c		      
c		      fix the difference array 1 pixel beyonds limits of patch
c
		    ixmin = max(ixmin - 1, ixStart + 1)
		    ixmax = min(ixmax + 1, ixEnd - 1)
		    iymin = max(iymin - 1, iyStart + 1)
		    iymax = min(iymax + 1, iyEnd - 1)
		    call compute_diffs(array, nx, ny, diffArr, limdiff,
     &			limdiff, ixmin, ixmax, iymin, iymax, ixofs,
     &			iyofs, sum, sumsq)
		  else
		    numPtOut = numPtSave
		    numPatchOut = numPatchSave
		  endif
		endif
	      enddo
	    enddo

	  enddo
	enddo

	return
	end

	  
c	  COMPUTE_DIFFS finds a difference between each pixel and its
c	  neighbors over the range ixStart-ixEnd, iyStart-iyEnd in array,
c	  places the result in diffArr using the offsets in ixofs, iyofs
c	  and returns the mean and standard deviation of the differences
c
	subroutine compute_diffs(array, nx, ny, diffArr, ixdim, iydim,
     &	    ixStart, ixEnd, iyStart, iyEnd, ixofs, iyofs, diffAvg,
     &	    diffSd)
	implicit none
	integer*4 nx, ny,ixdim, iydim,ixStart, ixEnd, iyStart, iyEnd
	real*4 array(nx,ny), diffArr(ixdim,iydim), diffAvg, diffSd
	integer*4 ix, iy, nsum, ixofs, iyofs
	real*4 sum, sumsq, pixDiff
	sum = 0.
	sumsq = 0.
	do iy = iyStart, iyEnd
	  do ix = ixStart, ixEnd
	    pixDiff = array(ix,iy) - (array(ix-1,iy) + array(ix+1,iy) +
     &		array(ix,iy+1) + array(ix,iy-1) + array(ix-1,iy-1) +
     &		array(ix-1,iy+1) + array(ix+1,iy-1) + array(ix+1,iy+1)) /8.
	    sum = sum + pixDiff
	    sumsq = sumsq + pixDiff**2
	    diffArr(ix + ixofs, iy + iyofs) = pixDiff
	  enddo
	enddo
	nsum = (iyEnd + 1 - iyStart) * (ixEnd + 1 - ixStart)
	call sums_to_avgsd(sum, sumsq, nsum, diffAvg, diffSd)
	return
	end	


c	  CLEANLINE replaces points along a line with points from adjacent
c	  lines
c
	subroutine cleanline(array,ixdim,iydim,ix1,iy1,ix2,iy2)
	implicit none
	integer*4 ixdim,iydim,ix1,ix2,iy1,iy2
	real*4 array(ixdim,iydim)
	integer*4 miny,maxy,iy,ix,minx,maxx
	if (ix1.eq.ix2)then
	  miny=iy1
	  maxy=iy2
	  if(maxy.lt.miny)then
	    miny=iy2
	    maxy=iy1
	  endif
	  do iy=miny,maxy
	    array(ix1,iy)=(array(ix1-1,iy)+array(ix1+1,iy))/2.
	  enddo
	else
	  minx=ix1
	  maxx=ix2
	  if(maxx.lt.minx)then
	    minx=ix2
	    maxx=ix1
	  endif
	  do ix=minx,maxx
	    array(ix,iy1)=(array(ix,iy1-1)+array(ix,iy1+1))/2.
	  enddo
	endif
	return
	end




	subroutine cleanarea(array,ixdim,iydim,nx,ny,ixfix,iyfix,
     &	    ninobj,nborder,iorder,ifincadj,ifVerbose)
c	  
	implicit none
	integer mxd,isdim
	parameter (mxd=50)
	integer*4 ixdim,iydim,nx,ny,ifVerbose
	real*4 array(ixdim,iydim)
	integer*4 ixfix(*), iyfix(*)
	integer*4 ninobj,nborder,iorder,ifincadj
	logical*1 inlist(-mxd:mxd,-mxd:mxd),adjacent(-mxd:mxd,-mxd:mxd)
	logical nearedge
	parameter (isdim=1000)
	include 'statsize.inc'
	real*4 xr(msiz,isdim), sx(msiz), xm(msiz), sd(msiz)
     &	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &	    , b(msiz), b1(msiz),vect(msiz)
c	  
	integer*4 ixcen,iycen,i,j,minxlist,minylist,maxxlist,maxylist
	integer*4 k,ixl,iyl,nbordm1,ixbordlo,ixbordhi,iybordlo,iybordhi
	integer*4 npixel,npnts,nindep,ix,iy,ixofs,iyofs
	real*4 c1,rsq,fra,xsum
c	  
c	  initialize list
c	  
	ixcen=ixfix(1)
	iycen=iyfix(1)
	do j=-mxd,mxd
	  do i=-mxd,mxd
	    inlist(i,j)=.false.
	    adjacent(i,j)=.false.
	  enddo
	enddo
	minxlist=ixcen
	minylist=iycen
	maxxlist=ixcen
	maxylist=iycen
	do k=1,ninobj
	  ixl=ixfix(k)-ixcen
	  iyl=iyfix(k)-iycen
	  inlist(ixl,iyl)=.true.
	  do i=-1,1
	    do j=-1,1
	      adjacent(ixl+i,iyl+j)=.true.
	    enddo
	  enddo
	  minxlist=min(minxlist,ixfix(k))
	  minylist=min(minylist,iyfix(k))
	  maxxlist=max(maxxlist,ixfix(k))
	  maxylist=max(maxylist,iyfix(k))
	enddo
	nbordm1=nborder-1
	ixbordlo=max(1,minxlist-nborder)
	ixbordhi=min(nx,maxxlist+nborder)
	iybordlo=max(1,minylist-nborder)
	iybordhi=min(ny,maxylist+nborder)
c	  total pixels in current area
	npixel=(iybordhi+1-iybordlo)*(ixbordhi+1-ixbordlo)
c	  
c	  list of points is complete: fill regression matrix with data for all
c	  points outside the list.  the ixbordlo etc. should be correct
c	  
	npnts=0
	nindep=iorder*(iorder+3)/2
	do iy=iybordlo,iybordhi
	  do ix=ixbordlo,ixbordhi
	    ixofs=ix-ixcen
	    iyofs=iy-iycen
	    nearedge= ix-ixbordlo .lt. nbordm1 .or.
     &		ixbordhi-ix .lt. nbordm1 .or.
     &		iy-iybordlo .lt. nbordm1 .or.
     &		iybordhi-iy .lt. nbordm1
	    if(.not.inlist(ixofs,iyofs) .and. (ifincadj.eq.1 .or.
     &		(nearedge.or. .not.adjacent(ixofs,iyofs))))then
	      npnts=npnts+1
	      call polyterm(ixofs,iyofs,iorder,xr(1,npnts))
	      xr(nindep+1,npnts)=array(ix,iy)
	    endif
	  enddo
	enddo
c	  
c	  do regession
c	  
	if (ifVerbose.gt.0)write (*,104)ninobj,npnts
104	format(/,i4,' points to fix,',i4,' points being fit')
	call multr(xr,nindep+1,npnts,sx,ss,ssd,d,r,xm,sd,b,b1,c1,rsq
     &	    ,fra)
c	  
c	  replace points on list with values calculated from fit
c
	do iy=iybordhi-nbordm1,iybordlo+nbordm1,-1
	  do ix=ixbordlo+nbordm1,ixbordhi-nbordm1
	    ixofs=ix-ixcen
	    iyofs=iy-iycen
	    if(inlist(ixofs,iyofs))then
	      call polyterm(ixofs,iyofs,iorder,vect)
	      xsum=c1
	      do i=1,nindep
		xsum=xsum+b1(i)*vect(i)
	      enddo
	      array(ix,iy)=xsum
	    endif
	  enddo
	enddo
c	  
	return
	end



c	  POLYTERM computes polynomial terms from ix and iy of order norder,
c	  puts in array vect.  The first set of terms is ix and iy.  Each next
c	  set is the previous set multipled by x, plus the last term of the
c	  previous set multiplied by y
c
	subroutine polyterm(ix,iy,norder,vect)
	implicit none
	integer*4 ix,iy,norder
	real*4 vect(*)
	real*4 x,y
	integer*4 istr,iend,iorder,i
	x=ix
	y=iy
	vect(1)=x
	vect(2)=y
	istr=1
	iend=2
	do iorder=2,norder
	  do i=istr,iend
	    vect(i+iorder)=vect(i)*x
	  enddo
	  istr=istr+iorder
	  vect(iend+iorder+1)=vect(iend)*y
	  iend=iend+iorder+1
	enddo
	return
	end

	logical function typeonlist(itype,ityplist,ntyplist)
	implicit none
	integer*4 ityplist(*),itype,ntyplist,i
	typeonlist=.true.
	if(ntyplist.eq.1.and.ityplist(1).eq.-999)return
	do i=1,ntyplist
	  if(itype.eq.ityplist(i))return
	enddo
	typeonlist=.false.
	return
	end
	
	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: CCDERASER - ',message
	call exit(1)
	end


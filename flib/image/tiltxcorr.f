*	  * * * * * * TILTXCORR * * * * * *
*	  
*	  TILTXCORR uses cross-correlation to find an initial translational
c	  alignment between successive images of a tilt series.  For a given
c	  pair of images, it stretches the image with the larger tilt angle
c	  perpendicular to the tilt axis, by an amount equal to the ratio of
c	  the cosines of the two tilt angles (cosine stretch).  The stretched
c	  image is correlated with the other image, and the position of the
c	  peak of the correlation indicates the relative shift between the
c	  images.  There are options to use only a subset of the image, to
c	  pad the image with a border before correlating, and to taper the
c	  image intensities down to the average level over some boundary
c	  region.  The latter feature is particularly important for getting
c	  reliable correlation peaks.  The program also has an option to
c	  correlate each image with the sum of already-aligned images at lower
C	  tilts, a method developed by Christian Renkin. The program will
c	  reduce the size of images larger than 1024 pixels in one dimension
c	  by binning them down, i.e. by averaging the values in square sets of
c	  adjacent pixels (2x3, or 3x3, etc).  Images are binned by the
c	  smallest factor needed to make them 1024 or smaller.
c
c	  For further details, see the man page.
C	  
c	  
c	  David Mastronarde 10/6/98
*
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.17  2003/12/04 16:31:21  mast
c	  Added * for an error return from fwrite
c	
c	  Revision 3.16  2003/11/27 06:03:26  mast
c	  Changed to determine binning after padding to allow large padding
c	
c	  Revision 3.15  2003/10/30 00:48:24  mast
c	  Incorporated cumulative reference method of Renkin, and added ability
c	  to correlate a subset shifted from the center
c	
c	  Revision 3.14  2003/10/24 03:48:52  mast
c	  do not call flush for small files due to Windows problems
c	
c	  Revision 3.13  2003/10/10 20:42:04  mast
c	  Used new subroutine for getting input/output files
c	
c	  Revision 3.12  2003/10/09 02:33:08  mast
c	  Converted to use autodoc
c	
c	  Revision 3.11  2003/06/21 00:48:00  mast
c	  New version that uses PIP input - but documentation not revised yet
c	
c	  Revision 3.10  2003/05/12 18:58:21  mast
c	  Fix problem with padding mean intensity when compiled to get
c	  correlation output file
c	
c	  Revision 3.9  2003/03/14 01:10:06  mast
c	  Add argument to interpolation acll
c	
c	  Revision 3.8  2002/07/27 15:49:27  mast
c	  Standardized error outputs
c	
c	  Revision 3.7  2002/05/21 03:22:39  mast
c	  Moved big array to common to avoid stack size problem on SGI
c	
c	  Revision 3.6  2002/05/21 03:18:27  mast
c	  Equivalenced the array used for test output to part of the first big
c	  array, to reduce stack size for SGI
c	
c	  Revision 3.5  2002/05/20 15:45:16  mast
c	  Increased dimension of input array to handle 4Kx4K image, reduced
c	  dimensions of other arrays to rely on binning down to 1024
c	
c	  Revision 3.4  2002/05/02 22:41:51  mast
c	  Fixed bug in which all shifts were being destrected, not just the
c	  ones where the current view was stretched
c	
c	  Revision 3.3  2002/04/29 21:56:54  mast
c	  Added automatic binning of images down to 1024 pixels.
c	
c	  Revision 3.2  2002/01/28 16:07:46  mast
c	  Added declarations for implicit none
c	
c	  Revision 3.1  2002/01/10 01:44:58  mast
c	  Increased limview to 720 and added check on number of views
c	
	implicit none
	integer idim,idim2,limview
	parameter (idim=4100*4100,idim2=1200*1200,limview=720)
	integer*4 NX,NY,NZ,nxs,nys,nzs
	COMMON //NX,NY,NZ,nxs,nys,nzs
C
	integer*4 NXYZ(3),MXYZ(3),nxyzs(3),mxyzs(3) ,label(20,20)
	real*4 title(20)
	real*4 ctfa(8193),ctfb(8193),ctfp(8193),sumray(idim2),crray(idim2)
	complex array(idim/2),brray(idim2/2)
C
	EQUIVALENCE (NX,NXYZ),(nxs,nxyzs),(crray(1),array(idim/4))
	common /bigarr/ array,sumray
c
	character*120 filin,plfile,imfilout
        real*4 f(2,3,limview),fs(2,3),fsinv(2,3),funit(2,3)
	character*9 dat
	character*8 tim
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	character*70 titstr
	character*7 fltrdp/' '/

	integer*4 ixpclist(limview),iypclist(limview),izpclist(limview)
	integer*4 listz(limview), ixBoxOffset(limview), iyBoxOffset(limview)
	real*4 tilt(limview), axisOffset(limview)
	real*4 dmin2,dmax2,dmean2,dmean3,rotangle,deltap,radexcl
	integer*4 i,npclist,nview,minxpiece,nxpieces,nxoverlap,minypiece
	integer*4 nypieces,nyoverlap,ifimout,nxpad,nypad,ifexclude,mode
	integer*4 nxtrim,nytrim,nxuse,nyuse,nxbord,nybord,nxtap,nytap
	integer*4 izst,iznd,kk,nout,izlast,izcur,idir,iztmp
	real*4 dmsum,dmax,dmin,stretch,streak,xpeak,ypeak,usdx,usdy
	real*4 dmean, radius1, radius2, sigma1, sigma2, tiltAtMin
	integer*4 jx,iv,iview,kti,isout, ierr, ivStart, ivEnd, loopDir
	integer*4 iloop, nloops, minTilt, ifAbsStretch, ivRef, ifLeaveAxis
	integer*4 nbin,maxbinsize,nxusebin,nyusebin, ifcumulate, ifNoStretch
	integer*4 ixst, ixnd, iyst, iynd, ivCur
	integer*4 ixstCen, ixndCen, iystCen, iyndCen
	real*4 xBoxOfs, yBoxOfs, cosphi, sinphi, x0, y0, xshift, yshift
	real*4 usemin, usemax, usemean, cumXshift, cumYshift, cumXrot, xAdjust
	integer*4 niceframe
	real*4 cosd, sind

	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger,PipGetBoolean
	integer*4 PipGetString,PipGetFloat, PipGetTwoIntegers
	integer*4 PipGetInOutFile, ifpip
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  tiltxcorr
c
	integer numOptions
	parameter (numOptions = 26)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@piece:PieceListFile:FN:@'//
     &      'output:OutputFile:FN:@rotation:RotationAngle:F:@'//
     &      'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'//
     &      'tiltfile:TiltFile:FN:@angles:TiltAngles:FA:@'//
     &      'radius1:FilterRadius1:F:@radius2:FilterRadius2:F:@'//
     &      'sigma1:FilterSigma1:F:@sigma2:FilterSigma2:F:@'//
     &      'exclude:ExcludeCentralPeak:B:@border:BordersInXandY:IP:@'//
     &      'xminmax:XMinAndMax:IP:@yminmax:YMinAndMax:IP:@'//
     &      'leaveaxis:LeaveTiltAxisShifted:B:@pad:PadsInXandY:IP:@'//
     &      'taper:TapersInXandY:IP:@views:StartingEndingViews:IP:@'//
     &      'cumulative:CumulativeCorrelation:B:@'//
     &      'absstretch:AbsoluteCosineStretch:B:@'//
     &      'nostretch:NoCosineStretch:B:@test:TestOutput:FN:@'//
     &      'param:ParameterFile:PF:@help:usage:B:'
c	  
c	  set defaults here where not dependent on image size
c	  
	ifimout=0
	ifexclude=0
	nxtrim=0
	nytrim=0
	sigma1 = 0.
	sigma2 = 0.
	radius1 = 0.
	radius2 = 0.
	rotangle = 0.
	imfilout = ' '
	ifcumulate = 0
	ifNoStretch = 0
	ifAbsStretch = 0
	ifLeaveAxis = 0

	maxbinsize=1180
	ifpip = 0
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'tiltxcorr',
     &	    'ERROR: TILTXCORR - ', .true., 3, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

	if (PipGetInOutFile('InputFile', 1, 'Image input file', filin)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
	IF (((NX+2)*NY.GT.idim)) call errorexit(
     &	    'IMAGE TOO LARGE FOR ARRAYS')

	if (nz.gt.limview) call errorexit('TOO MANY VIEWS FOR ARRAYS')
C   
	if (pipinput) then
	  ifpip = 1
	  plfile = ' '
	  ierr = PipGetString('PieceListFile', plfile)
	else
	  write(*,'(1x,a,$)')'Piece list file if there is one,'//
     &	      ' otherwise Return: '
	  read(*,101)plfile
101	  format(a)
	endif
	call read_piece_list(plfile,ixpclist,iypclist,izpclist,npclist)
c	  
c	    if no pieces, set up mocklist
c
	if(npclist.eq.0)then
	  do i=1,nz
	    ixpclist(i)=0
	    iypclist(i)=0
	    izpclist(i)=i-1
	  enddo
	  npclist=nz
	endif
	if(npclist.ne.nz)then
	  print *
	  print *,'ERROR: TILTXCORR - Piece list should have an '//
     &	      'entry for each image; nz =',
     &	      nz,', # in piece list =',npclist
	  call exit(1)
	endif
	call fill_listz(izpclist,npclist,listz,nview)
	call checklist(ixpclist,npclist,1,nx,minxpiece
     &	    ,nxpieces,nxoverlap)
	call checklist(iypclist,npclist,1,ny,minypiece
     &	    ,nypieces,nyoverlap)
	if(nxpieces*nypieces.gt.1)call errorexit(
     &	    'Program will not work with montages; '//
     &	      'blend images into single frames first')

	if(nview.ne.nz.or.listz(1).ne.0.or.listz(nview).ne.nz-1)then
	  print *
	  print *,'ERROR: TILTXCORR - The piece list should specify',
     &	      ' all Z values from 0 to' ,nz-1
	  call exit(1)
	endif
c
	if (PipGetInOutFile('OutputFile', 2,
     &	    'Output file for transforms', filin) .ne. 0) call errorexit(
     &	    'NO OUTPUT FILE SPECIFIED FOR TRANSFORMS')
	call dopen(1,filin,'new','f')
c	    
	call get_tilt_angles(nview,3,tilt, limview, ifpip)
	if(nview.ne.nz)then
	  print *
	  print *,'ERROR: TILTXCORR - There must be a tilt angle for'
     &	      //' each image: nz =', nz,', but there are',nview,
     &	      ' tilt angles'
	  call exit(1)
	endif
c
c
c	  DNM 4/28/02: figure out binning now, and fix this to send setctf
c	  approximately correct nx and ny instead of nxpad and nypad which
c	  don't yet exist
c	  DNM 7/11/03: better fix is to wait to get ctf until pad size is known
c	  DNM 11/1/5/03: and wait to get binning so big padding can be used
c
	if (pipinput) then
	  if (PipGetString('TestOutput', imfilout) .eq. 0) then
	    CALL IMOPEN(3,imfilout,'NEW')
	    CALL ITRHDR(3,1)
	    ifimout=1
	  endif
	  ierr = PipGetFloat('RotationAngle', rotangle)
	  ierr = PipGetFloat('FilterRadius1', radius1)
	  ierr = PipGetFloat('FilterRadius2', radius2)
	  ierr = PipGetFloat('FilterSigma1', sigma1)
	  ierr = PipGetFloat('FilterSigma2', sigma2)
	  ierr = PipGetBoolean('ExcludeCentralPeak', ifexclude)
	  ierr = PipGetTwoIntegers('BordersInXandY', nxtrim, nytrim)
	  ixst = nxtrim
	  ixnd = nx - 1 - nxtrim
	  iyst = nytrim
	  iynd = ny - 1 - nytrim
	  ierr = PipGetTwoIntegers('XMinAndMax', ixst, ixnd)
	  ierr = PipGetTwoIntegers('YMinAndMax', iyst, iynd)

	  ierr = PipGetBoolean('CumulativeCorrelation', ifcumulate)
	  ierr = PipGetBoolean('NoCosineStretch', ifNoStretch)
	  ierr = PipGetBoolean('AbsoluteCosineStretch', ifAbsStretch)
	  ierr = PipGetBoolean('LeaveTiltAxisShifted', ifLeaveAxis)
	else
	  write(*,'(1x,a,$)')
     &	      'Rotation angle FROM vertical TO the tilt axis: '
	  read(5,*)rotangle

	  print *,'Enter filter parameters to filter the correlation,'
     &	      //' or / for no filter'
	  WRITE(6,1100)
1100	  FORMAT(' Sigma1, Sigma2, Radius1, Radius2: ',$)
	  READ(5,*) SIGMA1,SIGMA2,RADIUS1,RADIUS2
c
	  write(*,'(1x,a,$)')'1 to exclude central correlation peak due'
     &	      //' to fixed pattern noise, 0 not to: '
	  read(5,*)ifexclude
c
	  write(*,'(1x,a,$)')
     &	      'Amounts to trim off each side in X and Y (/ for 0,0):'
	  read(5,*)nxtrim,nytrim
	  ixst = nxtrim
	  ixnd = nx - 1 - nxtrim
	  iyst = nytrim
	  iynd = ny - 1 - nytrim
	endif

	radexcl=0.
	if(ifexclude.eq.1) radexcl=1.1

	if(ixst.lt.0.or.iyst.lt.0.or.ixnd.ge.nx.or.iynd.ge.ny.or.
     &	    ixnd - ixst .lt. 24 .or. iynd - iyst .lt. 24)
     &	    call errorexit(
     &	    'Impossible amount to trim by or incorrect coordinates')

	nxuse = ixnd + 1 - ixst
	nyuse = iynd + 1 - iyst
c	  
c	  determine padding
c	  
	nxbord = max(5,min(20,nint(0.05*nxuse)))
	nybord = max(5,min(20,nint(0.05*nyuse)))
	if (pipinput) then
	  ierr = PipGetTwoIntegers('PadsInXandY', nxbord, nybord)
	else
	  write(*,'(1x,a,2i4,a,$)') 'Amounts to pad images on each side '
     &	      //'in X and Y (/ for',nxbord,nybord,'): '
	  read(*,*)nxbord,nybord
	endif
c
c	  get a binning based on the padded size so that large padding is
c	  possible
c
	nbin=(max(nxuse+2*nxbord,nyuse+2*nybord)+maxbinsize-1)/maxbinsize
	nxusebin=nxuse/nbin
	nyusebin=nyuse/nbin

	nxpad=niceframe((nxuse+2*nxbord)/nbin,2,19)
	nypad=niceframe((nyuse+2*nybord)/nbin,2,19)
	if((nxpad+2)*nypad.gt.idim2) call errorexit(
     &	    'Padded image too big, try less padding')

	write(*,'(/,a,i5,a,i5)')' Padded, binned size is',nxpad,' by',
     &	    nypad
c
c	  Now that padded size exists, get the filter ctf
c
	call setctfwsr(sigma1,sigma2,radius1,radius2,ctfp,nxpad,nypad,deltap)
c	  
c	  Set up tapering
c
	nxtap = max(5,min(100,nint(0.1*nxuse)))
	nytap = max(5,min(100,nint(0.1*nyuse)))
	if (pipinput) then
	  ierr = PipGetTwoIntegers('TapersInXandY', nxtap, nytap)
	else
	  write(*,'(1x,a,2i4,a,$)') 'Widths over which to taper images'
     &	      //' in X and Y (/ for',nxtap,nytap,'): '
	  read(*,*)nxtap,nytap
	endif
	nxtap=nxtap/nbin
	nytap=nytap/nbin
c	  
c	  Get view range
c
	izst=1
	iznd=nz
	if (pipinput) then
	  ierr = PipGetTwoIntegers('StartingEndingViews', izst, iznd)
	else
	  write(*,'(1x,a,$)') 'Starting and ending views'
     &	      //' to do (first is 1), or / for all: '
	  read(*,*)izst,iznd
	endif
	izst = max(1,min(nz,izst))
	iznd = max(izst,min(nz,iznd))
c	  
	do kk=1,nz
	  call xfunit(f(1,1,kk),1.0)
	enddo
c
	if(imfilout.ne.' ')then
	  nout=iznd-izst
	  if(ifimout.ne.0)nout=nout*3
	  call ialsiz_sam_cel(3,nxpad,nypad,nout)
	  dmsum=0.
	  dmax=-1.e10
	  dmin=1.e10
	endif
c	  
c	  get centered starting and ending coordinates to which box offsets
c	  will be added for loading
c
	ixstCen = (nx - nxuse) / 2
	ixndCen = ixstCen + nxuse - 1
	iystCen = (ny - nyuse) / 2
	iyndCen = iystCen + nyuse - 1
c	  
c	  precompute the box offsets
c	  start with the box offset at zero tilt
c	  rotate it so that the tilt axis is vertical
c	  compress along the X axis by cosine of the tilt
c	  rotate back to position in images and round to integers
c	  adjust each offset to stay in the field
c	  store the offset across axis also
c	  
	xBoxOfs = (ixnd + 1 + ixst - nx) / 2.
	yBoxOfs = (iynd + 1 + iyst - ny) / 2.
	cosphi = cosd(rotangle)
	sinphi = sind(rotangle)
	do i = izst, iznd
	  x0 = cosd(tilt(i)) * (xBoxOfs * cosphi + yBoxOfs * sinphi)
	  y0 = -xBoxOfs * sinphi + yBoxOfs * cosphi
	  ixBoxOffset(i) = nint(x0 * cosphi - y0 * sinphi)
	  iyBoxOffset(i) = nint(x0 * sinphi + y0 * cosphi)
	  ixBoxOffset(i) = max(-ixstCen, min(nx - 1 - ixndCen, ixBoxOffset(i)))
	  iyBoxOffset(i) = max(-iystCen, min(ny - 1 - iyndCen, iyBoxOffset(i)))
	  axisOffset(i) = ixBoxOffset(i) * cosphi + iyBoxOffset(i) * sinphi
c	  print *,i, tilt(i), ixBoxOffset(i), iyBoxOffset(i), axisOffset(i)
	enddo

	if (ifLeaveAxis .ne. 0) write(*,'(/,a,f8.1,a)')
     &	    ' The tilt axis is being left at a shift of',
     &	    xBoxOfs * cosphi + yboxOfs * sinphi,' pixels from center'

c	print *,xBoxOfs,yBoxOfs,ixstCen,ixndCen,iystCen,iyndCen
c	  
c	  set up for one forward loop through data - modified by case below
c
	nloops = 1
	loopDir = 1
c	    
c	  find minimum tilt view
c	  
	tiltAtMin = 10000.
	do i = 1, nview
	  if (abs(tilt(i)) .lt. abs(tiltAtMin)) then
	    minTilt = i
	    tiltAtMin = tilt(i)
	  endif
	enddo
c	  
c	  set up for first or only loop
c	  
	if (minTilt .ge. iznd) then
	  ivStart = iznd - 1
	  ivEnd = izst
	  loopDir = -1
	else if (minTilt .lt. iznd .and. minTilt .gt. izst) then
	  ivStart = minTilt + 1
	  ivEnd = iznd
	  nloops = 2
	else
	  ivStart = izst + 1
	  ivEnd = iznd
	endif

	do iloop = 1, nloops
	  do i = 1, nxusebin * nyusebin
	    sumray(i) = 0.
	  enddo
	  usdx = 0.
	  usdy = 0.
	  xpeak = 0.
	  ypeak = 0.
	  cumXshift = 0.
	  cumYshift = 0.
	  call xfunit(funit,1.0)

	  DO iview=ivStart, ivEnd, loopDir

	    ivCur = iview
	    ivRef = iview - loopDir
	    do i=1,nz
	      if(izpclist(i)+1.eq.iview-loopDir)izlast=i-1
	      if(izpclist(i)+1.eq.iview)izcur=i-1
	    enddo
c	      
c	      get the stretch - if its less than 1., invert everything
c	      
	    idir=1
	    stretch=cosd(tilt(iview-loopDir))/cosd(tilt(iview))
	    if (ifcumulate .ne. 0 .and. ifAbsStretch .ne. 0)
     &		stretch=cosd(tilt(minTilt))/cosd(tilt(iview))
	    if (ifNoStretch .ne. 0) stretch = 1.
	    if(stretch.lt.1.)then
	      idir=-1
	      iztmp=izcur
	      izcur=izlast
	      izlast=iztmp
	      stretch=1./stretch
	      iztmp = ivCur
	      ivCur = ivRef
	      ivRef = iztmp
	    endif
	    call rotmagstr_to_amat(0.,1.,stretch,rotangle,fs)
	    fs(1,3)=0.
	    fs(2,3)=0.
	    call xfinvert(fs,fsinv)
c	      print *,izlast,izcur,stretch
c	      
c	      get "current" into array, stretch into brray, pad it
C	      
	    call imposn(1,izcur,0)
	    if(nxuse.eq.nx.and.nyuse.eq.ny)then
	      call irdsec(1,array,*99)
	    else
	      call irdpas(1, array, nxuse, nyuse, ixstCen+ixBoxOffset(ivCur),
     &		  ixndCen+ixBoxOffset(ivCur), iystCen+iyBoxOffset(ivCur),
     &		  iyndCen+iyBoxOffset(ivCur),*99)
	    endif
c	      
c	      bin in place
c	      
	    if(nbin.gt.1) call reduce_by_binning(array,nxuse,nyuse,nbin,
     &		array,nxusebin, nyusebin)
c	      
c	      7/11/03: We have to feed the interpolation the right mean or
c	      it will create a bad edge mean for padding
c
	    call iclden(array,nxusebin,nyusebin,1,nxusebin,1,nyusebin,
     &		usemin, usemax, usemean)
	    call cubinterp(array,brray,nxusebin,nyusebin,nxusebin,nyusebin,fs,
     &		nxusebin/2., nyusebin/2.,0.,0. ,1.,usemean, 0)
	    call taperinpad(brray,nxusebin,nyusebin,brray,nxpad+2,nxpad,nypad,
     &		nxtap, nytap)
c	      
	    call meanzero(brray,nxpad+2,nxpad,nypad)
c	      
c	      get "last" into array, just bin and pad it there
c	      
	    call imposn(1,izlast,0)
	    if(nxuse.eq.nx.and.nyuse.eq.ny)then
	      call irdsec(1,array,*99)
	    else
	      call irdpas(1, array, nxuse, nyuse, ixstCen+ixBoxOffset(ivRef),
     &		  ixndCen+ixBoxOffset(ivRef), iystCen+iyBoxOffset(ivRef),
     &		  iyndCen+iyBoxOffset(ivRef),*99)
	    endif
	    if(nbin.gt.1) call reduce_by_binning(array,nxuse,nyuse,nbin,
     &		array,nxusebin, nyusebin)
	    if (ifcumulate .ne. 0) then
c		
c		if accumulating, transform image by last shift, add it to
c		sum array, then taper and pad into array
c
	      if (ifAbsStretch .eq. 0) then 
		call xfunit(funit,1.0)
	      else
		usdx = xpeak
		usdy = ypeak
	      endif
	      call iclden(array,nxusebin,nyusebin,1,nxusebin,1,nyusebin,
     &		  usemin, usemax, usemean)
	      call cubinterp(array,crray,nxusebin,nyusebin,nxusebin,nyusebin,
     &		  funit, nxusebin/2., nyusebin/2.,usdx,usdy ,1.,usemean, 0)
	      do i = 1, nxusebin * nyusebin
		sumray(i) = sumray(i) + crray(i)
	      enddo	      
	      call taperinpad(sumray,nxusebin,nyusebin,array,nxpad+2,
     &		  nxpad,nypad, nxtap, nytap)
	    else
	      call taperinpad(array,nxusebin,nyusebin,array,nxpad+2,
     &		  nxpad,nypad, nxtap, nytap)
	    endif
	    if(ifimout.ne.0)then
	      do isout=1,2
		if(isout.eq.1)then
		  call irepak(crray,array,nxpad+2,nypad,
     &		      0,nxpad-1,0,nypad-1)
		else
		  call irepak(crray,brray,nxpad+2,nypad,
     &		      0,nxpad-1,0,nypad-1)
		endif
		if(mode.ne.2)then
		  CALL IsetDN(crray,NXpad,NYpad,MODE,1,NXpad,1,NYpad,DMIN2,
     &		      DMAX2,DMEAN3)
		else
		  CALL IclDeN(crray,NXpad,NYpad,1,NXpad,1,NYpad,DMIN2,DMAX2,
     &		      DMEAN3)
		endif
		CALL IWRSEC(3,crray)
C		  
		DMAX = max(dmax,dmax2)
		DMIN = min(dmin,dmin2)
		DMsum = dmsum + dmean3
	      enddo
	    endif
	    call meanzero(array,nxpad+2,nxpad,nypad)

C	      
c	      print *,'taking fft'
	    call todfft(array,nxpad,nypad,0)
	    call todfft(brray,nxpad,nypad,0)
c	      
c	      multiply array by complex conjugate of brray, put back in array
c	      
c	      print *,'multiplying arrays'
	    do jx=1,nypad*(nxpad+2)/2
	      array(jx)=array(jx)*conjg(brray(jx))
	    enddo
c	      
	    if(deltap.ne.0.)call filterpart(array,array,nxpad,nypad,ctfp,
     &		deltap)
c	      print *,'taking back fft'
	    call todfft(array,nxpad,nypad,1)
c	      
c	      the factor here determines the balance between accepting a spurious
c	      peak and rejecting a real peak because it falls in the streak.
c	      The spurious peak could be as far as 0.5 out but is much more
c	      likely to be within 0.25 of the maximum displacement
c	      
	    streak=0.25*(stretch-1.0)*nxuse
	    call peakfind(array,nxpad+2,nypad,xpeak,ypeak,radexcl,
     &		rotangle, streak)
c	      
c	      DNM 5/2/02: only destretch the shift if the current view was
c	      stretched.  Also, put the right sign out in the standard output
c	      
	    if (idir.gt.0)then
	      call xfapply(fsinv,0.,0.,xpeak,ypeak,usdx,usdy)
	    else
	      usdx=xpeak
	      usdy=ypeak
	    endif
c	      
c	      compensate for the box offsets of reference and current view,
c	      where the reference is the starting view if accumulating
c
	    ivRef = iview - loopDir
	    if (ifcumulate .ne. 0) ivRef = ivStart - loopDir
	    xshift = idir*nbin*usdx + ixBoxOffset(ivRef) - ixBoxOffset(iview)
	    yshift = idir*nbin*usdy + iyBoxOffset(ivRef) - iyBoxOffset(iview)
c	      
c	      if not leaving axis at center of box, compute amount that
c	      current view must be shifted across tilt axis to be lined up at
c	      center, then rotate that to get shifts in X and Y
c
	    if (ifLeaveAxis .eq. 0) then
	      xBoxOfs = axisOffset(ivRef) *
     &		  (cosd(tilt(iview)) / cosd(tilt(ivRef)) - 1.)
	      xshift = xshift + xBoxOfs * cosphi
	      yshift = yshift + xBoxOfs * sinphi
	    endif
c	      
c	      If not cumulative, adjust the shift to bring the tilt axis
c	      to the true center from the center of last view
c	      If last view was shifted to right to line up with center,
c	      then the true center is to the left of the center of that view
c	      and the tilt axis (and this view) must be shifted to the left
c
	    if (ifcumulate .eq. 0) then
	      cumXrot = cumXshift * cosphi + cumYshift * sinphi
	      xAdjust = cumXrot *  (cosd(tilt(iview)) / cosd(tilt(ivRef)) - 1.)
	      xshift = xshift + xAdjust * cosphi
	      yshift = yshift + xAdjust * sinphi
c		
c		Add to cumulative shift and report and save the absolute shift
c
	      cumXshift = cumXshift + xshift
	      cumYshift = cumYshift + yshift
	      xshift = cumXshift
	      yshift = cumYshift
	    endif

	    write(*,'(a,i4,a,2f8.2)')'View',iview,', shifts', xshift, yshift
c
c	      DNM 10/22/03: Only do flush for large stacks because of problem
c	      inside shell scripts in Windows/Intel
c
	    if (iznd - izst .gt. 2) call flush(6)
	    f(1,3,iview)=xshift
	    f(2,3,iview)=yshift
	    call xfcopy(fs, funit)
	    if(imfilout.ne.' ')then
	      call packcorr(crray,array,nxpad+2,nypad)
	      if(mode.ne.2)then
		CALL IsetDN(crray,NXpad,NYpad,MODE,1,NXpad,1,NYpad,DMIN2,
     &		    DMAX2,DMEAN3)
	      else
		CALL IclDeN(crray,NXpad,NYpad,1,NXpad,1,NYpad,DMIN2,DMAX2,
     &		    DMEAN3)
	      endif
	      CALL IWRSEC(3,crray)
C		
	      DMAX = max(dmax,dmax2)
	      DMIN = min(dmin,dmin2)
	      DMsum = dmsum + dmean3
	    endif
	  enddo
c	    
c	    set up for second loop
c
	  ivStart = minTilt - 1
	  ivEnd = izst
	  loopDir = -1
	enddo
c
	if(imfilout.ne.' ')then
	  dmean=dmsum/nout
	  CALL DATE(DAT)
	  CALL TIME(TIM)
c	
	  if(deltap.ne.0.)fltrdp=', filtered'
	  titstr='TILTXCORR: stack cosine stretch/correlated '//
     &	      fltrdp
c	   
c 7/7/00 CER: remove the encodes
c
C         ENCODE(80,1500,TITLE) titstr,DAT,TIM
          write(titlech,1500) titstr,DAT,TIM
          read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
1500	  FORMAT(A54,2x,A9,2X,A8)
	  
	  CALL IWRHDR(3,TITLE,1,DMIN,DMAX,DMEAN)
	  call imclose(3)
	endif
c	  
c	  Anticipate what xftoxg will do, namely get to transforms with zero
c	  mean, and shift tilt axis to be at middle in that case
c	  
	iloop = 1
	cumXshift = 10.
	do while (iloop.le.10 .and. (cumXshift.gt.0.1 .or. cumYshift.gt.0.1))
	  iloop = iloop + 1
	  cumXshift = 0.
	  cumYshift = 0.
	  do iv = 1, nz
	    cumXshift = cumXshift - f(1,3,iv) / nz
	    cumYshift = cumYshift - f(2,3,iv) / nz
	  enddo
c	  print *,iloop,', mean shift',cumXshift, cumYshift
c	    
c	    rotate the average shift to tilt axis vertical, adjust the X shift
c	    to keep tilt axis in center and apply shift for all views
c
	  cumXrot = cumXshift * cosphi + cumYshift * sinphi
	  yshift = -cumXshift * sinphi + cumYshift * cosphi
	  do iv = 1,nz
	    xAdjust = cumXrot * cosd(tilt(iv))
	    f(1,3,iv) = f(1,3,iv) + xAdjust * cosphi - yshift * sinphi
	    f(2,3,iv) = f(2,3,iv) + xAdjust * sinphi + yshift * cosphi
	  enddo
	enddo
c
c	  convert from g to f transforms by taking differences
c	  
	do iv = nz, 2, -1
	  f(1,3,iv) = f(1,3,iv) - f(1,3,iv - 1)
	  f(2,3,iv) = f(2,3,iv) - f(2,3,iv - 1)
	enddo
	call xfunit(f(1,1,1), 1.)
	do iv=1,nz
	  call xfwrite(1,f(1,1,iv),*96)
	enddo
	close(1)
	call imclose(1)
C
	WRITE(6,500)
500	FORMAT(' PROGRAM EXECUTED TO END.')
	call exit(0)
99	call errorexit('END OF IMAGE WHILE READING')
96	call errorexit('ERROR WRITING TRANSFORMS TO FILE')
	END


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: TILTXCORR - ',message
	call exit(1)
	end


c	  PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c	  in the array ARRAY, which is dimensioned to nx+2 by ny.  It fits
c	  a parabola in to the three points around the peak in X or Y and gets
c	  a much better estimate of peak location.
c
	subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,radexcl,
     &	    rotangle,streak)
	implicit none
	integer*4 nxplus,nyrot
	real*4 xpeak,ypeak,radexcl,rotangle,streak
	real*4 array(nxplus,nyrot)
	integer*4 nxrot,ix,iy,idx,idy,lower,ixpeak,iypeak
	real*4 peak,xrot,yrot,cx,y1,y2,y3,denom,cy,costh,sinth
	real*4 cosd,sind
	integer*4 indmap
c
	nxrot=nxplus-2
c	  
c	  find peak
c	  
	costh=cosd(-rotangle)
	sinth=sind(-rotangle)
	peak=-1.e30
	do iy=1,nyrot
	  do ix=1,nxrot
	    if(array(ix,iy).gt.peak)then
c		
c		first check if outside exclusion streak
c
	      idx=ix-1
	      idy=iy-1
	      if(idx.gt.nxrot/2)idx=idx-nxrot
	      if(idy.gt.nyrot/2)idy=idy-nyrot
	      xrot=idx*costh-idy*sinth
	      yrot=idx*sinth+idy*costh
	      if(abs(yrot).ge.radexcl.or.abs(xrot).ge.streak+radexcl)then
c		  
c		  next check that point is actually a local peak
c		  
		lower=0
		do idx =-1,1
		  do idy=-1,1
		    if(array(ix,iy).lt.array(indmap(ix+idx,nxrot),
     &			indmap(iy+idy,nyrot)))lower=1
		  enddo
		enddo
		if(lower.eq.0)then
		  peak=array(ix,iy)
		  ixpeak=ix
		  iypeak=iy
		endif
	      endif
	    endif
	  enddo
	enddo
c	print *,ixpeak,iypeak
c	  
c	  simply fit a parabola to the two adjacent points in X or Y
c
	cx=0.
	y1=array(indmap(ixpeak-1,nxrot),iypeak)
	y2=peak
	y3=array(indmap(ixpeak+1,nxrot),iypeak)
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
	if(abs(cx).gt.0.5)cx=sign(0.5,cx)
c	print *,'X',y1,y2,y3,cx
	cy=0.
	y1=array(ixpeak,indmap(iypeak-1,nyrot))
	y3=array(ixpeak,indmap(iypeak+1,nyrot))
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.-1.e6)cy=(y1-y3)/denom
	if(abs(cy).gt.0.5)cy=sign(0.5,cy)
c	print *,'Y',y1,y2,y3,cy
c	  
c	  return adjusted pixel coordinate minus 1
c
	xpeak=ixpeak+cx-1.
	ypeak=iypeak+cy-1.
c	print *,xpeak,ypeak
	if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
	if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
	return
	end



	subroutine packcorr(crray,array,nxpadpl,nypad)
	implicit none
	integer*4 nxpadpl,nypad
	real*4 array(nxpadpl,nypad),crray(*)
	integer*4 nxpad,iout,ixin,iyin,ix,iy

	nxpad=nxpadpl-2
	iout=1
	ixin=nxpad/2+1
	iyin=nypad/2+1
	do iy=1,nypad
	  do ix=1,nxpad
	    crray(iout)=array(ixin,iyin)
	    iout=iout+1
	    ixin=ixin+1
	    if(ixin.gt.nxpad)ixin=1
	  enddo
	  iyin=iyin+1
	  if(iyin.gt.nypad)iyin=1
	enddo
	return
	end


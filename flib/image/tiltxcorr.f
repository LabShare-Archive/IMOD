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
c	  reliable correlation peaks.  The program also has on option to
c	  ignore a central peak caused by fixed pattern noise in the images,
c	  which can be a serious problem unless digital camera images are well
c	  gain-normalized.  The program will reduce the size of images larger
c	  than 1024 pixels in one dimension by binning them down, i.e. by
c	  averaging the values in square sets of adjacent pixels (2x3, or 3x3,
c	  etc).  Images are binned by the smallest factor needed to make them
c	  1024 or smaller.
c
c	  Some notes about some of the options:
c	  
c	  Filtering: Some high pass filtering, using a small value of Sigma1
c	  such as 0.03, may be helpful to keep the program from being misled
c	  by very large scale features in the images.  If the images are
c	  noisy, some low pass filtering with Sigma2 and Radius2 is
c	  appropriate (e.g. 0.05 for Sigma2, 0.25 for Radius2).  If the images
c	  are binned, these values specify frequencies in the binned image, so
c	  a higher cutoff (less filtering) might be appropriate.
c	  
c	  The exclusion of a central peak can be essential when there is
c	  fixed noise in the images.  Because one image is stretched, this
c	  spurious peak can actually occur anywhere in an elongated region
c	  perpendicular to the tilt axis.  If the real alignment happens to
c	  fall in that streak, then it will be ignored as well, and an
c	  incorrect alignment will be found.  For this reason, this option 
c	  should be used only when necessary.
c	  
c	  Trimming some area off the edges of the images may be helpful if
c	  those areas are particularly out of focus or contain material with
c	  no useful features in it.
c	  
c	  Padding is customarily done to reduce the contribution to the
c	  correlation from wrapped around features, which occurs when
c	  correlation is done with Fourier transforms.  Extensive padding does
c	  not help with typical biological specimens but may be needed for
c	  specimens with periodic structures, in which case one should pad each
c	  edge by half the image size.
c	  
c	  In contrast, tapering the images down to a mean intensity at their
c	  edges is very important.  Tapering over as few as 20 pixels may be
c	  adequate, but fewer artifacts will appear in the correlation with
c	  longer tapers (say, 50 to 100 pixels).
c	  
c	  Entries to the program in order are:
c	  
C	  Image input file
c
C	  Piece list file for reordering the Z values in the stack, or Return
c	  if none
c
C	  Output file for F transforms
C	  
c	  -1 to enter individual tilt angle for each view, 1 to specify a
c	  starting and increment tilt, or 0 to read tilt angles from a file
c	  
c	  IF you entered 1, next enter the starting and incremental tilt angles
c	  IF you entered -1, enter the tilt angle of each view.
c	  IF you entered 0, enter name of file with tilt angles
c	  
c	  Angle of rotation of the tilt axis in the images; specifically, the
c	  angle from the vertical to the tilt axis (counterclockwise
c	  positive).
c
C	  Filter parameters to filter the correlation, or / for no filter
C	  (Enter values of Sigma1, Sigma2, Radius1, Radius2 just as for
c	  ENHANCE.)
c
c	  1 to exclude a central correlation peak due to fixed pattern noise
c	  in the images, or 0 not to
c	  
c	  Amounts to trim off each side in the X and Y dimensions, or / to use
c	  the whole image area
c	  
C	  Borders with which to pad images in the X and Y dimensions, or / for
c	  the default, which is 5% of the image dimensions up to 20 pixels
c	  
c	  Distances over which to taper image intensities down to the mean at
c	  the edges, in the X and Y dimensions.  Enter / for the default, which
c	  is 10% of the image dimensions up to 100 pixels.
c
C	  Starting and ending view #'s (first is 1), or / for all views
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
	real*4 ctfa(8193),ctfb(8193),ctfp(8193)
	complex array(idim/2),brray(idim2/2),crray(idim2/2)
C
	EQUIVALENCE (NX,NXYZ),(nxs,nxyzs),(crray(1),array(idim/4))
	common /bigarr/ array
c
	character*80 filin,plfile,imfilout
        real*4 f(2,3,limview),fs(2,3),fsinv(2,3)
	character*9 dat
	character*8 tim
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	character*70 titstr
	character*7 fltrda/' '/,fltrdb/' '/,fltrdp/' '/

	integer*4 ixpclist(limview),iypclist(limview),izpclist(limview)
	integer*4 listz(limview)
	real*4 tilt(limview)
	real*4 dmin2,dmax2,dmean2,dmean3,rotangle,deltap,radexcl
	integer*4 i,npclist,nview,minxpiece,nxpieces,nxoverlap,minypiece
	integer*4 nypieces,nyoverlap,ifimout,nxpad,nypad,ifexclude,mode
	integer*4 nxtrim,nytrim,nxuse,nyuse,nxbord,nybord,nxtap,nytap
	integer*4 izst,iznd,kk,nout,izlast,izcur,idir,iztmp
	real*4 dmsum,dmax,dmin,stretch,streak,xpeak,ypeak,usdx,usdy
	real*4 dmean, radius1, radius2, sigma1, sigma2
	integer*4 jx,iv,iview,kti,isout, ierr
	integer*4 nbin,maxbinsize,nxusebin,nyusebin
	integer*4 niceframe
	real*4 cosd

	integer numOptions
	parameter (numOptions = 20)
	character*(80 * numOptions) options(1)
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipParseInput, PipGetInteger,PipGetBoolean
	integer*4 PipGetString,PipGetFloat, PipGetIntegerArray
	integer*4 PipGetNonOptionArg, PipPrintHelp, ifpip
	integer*4 nxytrim(2), nxypad(2), nxytap(2), izstnd(2)
	equivalence (nxytrim(1), nxtrim), (nxytrim(2), nytrim)
	equivalence (nxypad(1), nxpad), (nxypad(2), nypad)
	equivalence (nxytap(1), nxtap), (nxytap(2), nytap)
	equivalence (izstnd(1), izst), (izstnd(2), iznd)
	options(1) =
     &	    'input:InputFile:FN:Input image file@'//
     &	    'piece:PieceListFile:FN:Piece file with rearranged Z'//
     &	    ' values@'//
     &	    'output:OutputFile:FN:Output file for transforms@'//
     &	    'rotation:RotationAngle:F:Rotation angle from the Y'//
     &	    ' axis to the tilt axis@'//
     &	    'first:FirstTiltAngle:F:Tilt angle of first view in'//
     &	    ' degrees@'//
     &	    'increment:TiltIncrement:F:Increment between tilt angles@'//
     &	    'tiltfile:TiltFile:FN:File with tilt angles@'//
     &	    'angles:TiltAngles:FAM:Individual tilt angles for each'//
     &	    ' view@'//
     &	    'radius1:FilterRadius1:F:Left cutoff radius for filter@'//
     &	    'radius2:FilterRadius2:F:Right cutoff radius for filter@'//
     &	    'sigma1:FilterSigma1:F:Sigma for low-frequency'//
     &	    ' (1 - gaussian) filter@'//
     &	    'sigma2:FilterSigma2:F:Sigma for gaussian rolloff below'//
     &	    ' radius1 & above radius2@'//
     &	    'exclude:ExcludeCentralPeak:B:Exclude central peak due to'//
     &	    ' fixed pattern noise@'//
     &	    'border:BordersInXandY:IA:Number of pixels to trim off in'//
     &	    ' X and in Y@'//
     &	    'pad:PadsInXandY:IA:Number of pixels to pad images in'//
     &	    ' X and in Y@'//
     &	    'taper:TapersInXandY:IA:Number of pixels to taper images'//
     &	    ' in X and in Y@'//
     &	    'views:StartingEndingViews:IA:Starting and ending view'//
     &	    ' number for doing subset@'//
     &	    'test:TestOutput:FN:File to save processed images and'//
     &	    ' correlations in@'//
     &	    'param:ParameterFile:PF:Read parameter entries from file@'//
     &	    'help:usage:B:Print help output'

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

	maxbinsize=1024
	ifpip = 0
c	  
c	  Pip startup: set error, parse options, set flag if used
c
	call PipExitOnError(0, "ERROR: TILTXCORR - ")
	call PipAllowCommaDefaults(1)
	ierr = PipParseInput(options, numOptions, '@', numOptArg, numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0
c
	if (pipinput) then
c	    
c	    First action if pip used: check for help
c
	  if (PipGetBoolean('usage', ierr) .eq. 0) then
	    ierr = PipPrintHelp('tiltxcorr', 0, 1, 1)
	    call exit(0)
	  endif
	  ifpip = 1
c	    
c	    Get an input file string; or if none, look on command line
c
	  ierr = PipGetString('InputFile', filin)
	  if (ierr .gt. 0) then
	    if (numNonOptArg .eq. 0) call errorexit
     &		('NO INPUT FILE SPECIFIED')
	    ierr = PipGetNonOptionArg(1, filin)
	  endif
	else

	  write(*,'(1x,a,$)')'Image input file: '
	  READ(5,101)FILIN
101	  format(a)
	endif
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
	IF (((NX+2)*NY.GT.idim)) call errorexit(
     &	    'IMAGE TOO LARGE FOR ARRAYS')

	if (nz.gt.limview) call errorexit('TOO MANY VIEWS FOR ARRAYS')
C   
	if (pipinput) then
	  plfile = ' '
	  ierr = PipGetString('PieceListFile', plfile)
	else
	  write(*,'(1x,a,$)')'Piece list file if there is one,'//
     &	      ' otherwise Return: '
	  read(*,101)plfile
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
	if (pipinput) then
	  ierr = PipGetString('OutputFile', filin)
	  if (ierr .gt. 0) then
	    if (numNonOptArg .lt. 2) call errorexit(
     &		'NO OUTPUT FILE SPECIFIED FOR TRANSFORMS')
	    ierr = PipGetNonOptionArg(2, filin)
	  endif
	else
	  write(*,'(1x,a,$)')'Output file for transforms: '
	  READ(5,101)FILIN
	endif
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
c	  
	nbin=(max(nx,ny)+maxbinsize-1)/maxbinsize

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
	  call setctfwsr(sigma1,sigma2,radius1,radius2,ctfp,nx/nbin,
     &	      ny/nbin,deltap)
	  ierr = PipGetBoolean('ExcludeCentralPeak', ifexclude)
	  ierr = PipGetIntegerArray('BordersInXandY', nxytrim, 2, 2)
	else
	  write(*,'(1x,a,$)')
     &	      'Rotation angle FROM vertical TO the tilt axis: '
	  read(5,*)rotangle

	  print *,'Enter filter parameters to filter the correlation,'
     &	      //' or / for no filter'
	  call setctf(ctfp,nx/nbin,ny/nbin,deltap)
c
	  write(*,'(1x,a,$)')'1 to exclude central correlation peak due'
     &	      //' to fixed pattern noise, 0 not to: '
	  read(5,*)ifexclude
c
	  write(*,'(1x,a,$)')
     &	      'Amounts to trim off each side in X and Y (/ for 0,0):'
	  read(5,*)nxtrim,nytrim
	endif

	radexcl=0.
	if(ifexclude.eq.1) radexcl=1.1

	if(nxtrim.lt.0.or.nytrim.lt.0.or.nxtrim.gt.nx/2-16.or.
     &	    nytrim.gt.ny/2-16)call errorexit(
     &	    'Impossible amount to trim by')

	nxuse=nx-2*nxtrim
	nyuse=ny-2*nytrim
	nxusebin=nxuse/nbin
	nyusebin=nyuse/nbin
c
	nxbord = max(5,min(20,nint(0.05*nxuse)))
	nybord = max(5,min(20,nint(0.05*nyuse)))
	if (pipinput) then
	  ierr = PipGetIntegerArray('PadsInXandY', nxypad, 2, 2)
	else
	  write(*,'(1x,a,2i4,a,$)') 'Amounts to pad images on each side '
     &	      //'in X and Y (/ for',nxbord,nybord,'): '
	  read(*,*)nxbord,nybord
	endif
	nxpad=niceframe((nxuse+2*nxbord)/nbin,2,19)
	nypad=niceframe((nyuse+2*nybord)/nbin,2,19)
	if((nxpad+2)*nypad.gt.idim2) call errorexit(
     &	    'Padded image too big, try less padding')

	write(*,'(a,i5,a,i5)')' Padded, binned size is',nxpad,' by',nypad
c
	nxtap = max(5,min(100,nint(0.1*nxuse)))
	nytap = max(5,min(100,nint(0.1*nyuse)))
	if (pipinput) then
	  ierr = PipGetIntegerArray('TapersInXandY', nxytap, 2, 2)
	else
	  write(*,'(1x,a,2i4,a,$)') 'Widths over which to taper images'
     &	      //' in X and Y (/ for',nxtap,nytap,'): '
	  read(*,*)nxtap,nytap
	endif
	nxtap=nxtap/nbin
	nytap=nytap/nbin

	izst=1
	iznd=nz
	if (pipinput) then
	  ierr = PipGetIntegerArray('StartingEndingViews', izstnd, 2, 2)
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
        DO iview=izst+1,iznd

	  do i=1,nz
	    if(izpclist(i)+1.eq.iview-1)izlast=i-1
	    if(izpclist(i)+1.eq.iview)izcur=i-1
	  enddo
c	    
c	    get the stretch - if its less than 1., invert everything
c
	  idir=1
	  stretch=cosd(tilt(iview-1))/cosd(tilt(iview))
	  if(stretch.lt.1.)then
	    idir=-1
	    iztmp=izcur
	    izcur=izlast
	    izlast=iztmp
	    stretch=1./stretch
	  endif
	  call rotmagstr_to_amat(0.,1.,stretch,rotangle,fs)
	  fs(1,3)=0.
	  fs(2,3)=0.
	  call xfinvert(fs,fsinv)
c	  print *,izlast,izcur,stretch
c	 
c	    get "current" into array, stretch into brray, pad it
C
	  call imposn(1,izcur,0)
	  if(nxtrim.eq.0.and.nytrim.eq.0)then
	    call irdsec(1,array,*99)
	  else
	    call irdpas(1,array,nxuse,nyuse,nxtrim,nx-nxtrim-1,nytrim,
     &		ny-nytrim-1,*99)
	  endif
c	    
c	    bin in place
c	    
	  if(nbin.gt.1) call reduce_by_binning(array,nxuse,nyuse,nbin,
     &	      array,nxusebin, nyusebin)
c
	  call cubinterp(array,brray,nxusebin,nyusebin,nxusebin,nyusebin,fs,
     &	      nxusebin/2., nyusebin/2.,0.,0. ,1.,dmean2, 0)
	  call taperinpad(brray,nxusebin,nyusebin,brray,nxpad+2,nxpad,nypad,
     &	      nxtap, nytap)
c
	  call meanzero(brray,nxpad+2,nxpad,nypad)
c	    
c	    get "last" into array, just bin and pad it there
c
	  call imposn(1,izlast,0)
	  if(nxtrim.eq.0.and.nytrim.eq.0)then
	    call irdsec(1,array,*99)
	  else
	    call irdpas(1,array,nxuse,nyuse,nxtrim,nx-nxtrim-1,nytrim,
     &		ny-nytrim-1,*99)
	  endif
	  if(nbin.gt.1) call reduce_by_binning(array,nxuse,nyuse,nbin,
     &	      array,nxusebin, nyusebin)
	  call taperinpad(array,nxusebin,nyusebin,array,nxpad+2,nxpad,nypad,
     &	      nxtap, nytap)
	  if(ifimout.ne.0)then
	    do isout=1,2
	      if(isout.eq.1)then
		call irepak(crray,array,nxpad+2,nypad,
     &		    0,nxpad-1,0,nypad-1)
	      else
		call irepak(crray,brray,nxpad+2,nypad,
     &		    0,nxpad-1,0,nypad-1)
	      endif
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
	    enddo
	  endif
	  call meanzero(array,nxpad+2,nxpad,nypad)

C	    
c	  print *,'taking fft'
	  call todfft(array,nxpad,nypad,0)
	  call todfft(brray,nxpad,nypad,0)
c	    
c	    multiply array by complex conjugate of brray, put back in array
c	    
c	  print *,'multiplying arrays'
	  do jx=1,nypad*(nxpad+2)/2
	    array(jx)=array(jx)*conjg(brray(jx))
	  enddo
c
	  if(deltap.ne.0.)call filterpart(array,array,nxpad,nypad,ctfp,
     &	      deltap)
c	  print *,'taking back fft'
	  call todfft(array,nxpad,nypad,1)
c
c	    the factor here determines the balance between accepting a spurious
c	    peak and rejecting a real peak because it falls in the streak.
c	    The spurious peak could be as far as 0.5 out but is much more
c	    likely to be within 0.25 of the maximum displacement
c	    
	  streak=0.25*(stretch-1.0)*nxuse
	  call peakfind(array,nxpad+2,nypad,xpeak,ypeak,radexcl,
     &	      rotangle, streak)
c
c	    DNM 5/2/02: only destretch the shift if the current view was
c	    stretched.  Also, put the right sign out in the standard output
c
	  if (idir.gt.0)then
	    call xfapply(fsinv,0.,0.,xpeak,ypeak,usdx,usdy)
	  else
	    usdx=xpeak
	    usdy=ypeak
	  endif
	  write(*,'(a,i4,a,2f8.2)')'View',iview,', shifts',
     &	      idir*nbin*usdx,idir*nbin*usdy
	  call flush(6)
	  f(1,3,iview)=idir*nbin*usdx
	  f(2,3,iview)=idir*nbin*usdy
	  if(imfilout.ne.' ')then
	    call packcorr(crray,array,nxpad+2,nypad)
	    if(mode.ne.2)then
	      CALL IsetDN(crray,NXpad,NYpad,MODE,1,NXpad,1,NYpad,DMIN2,
     &		  DMAX2,DMEAN3)
	    else
	      CALL IclDeN(crray,NXpad,NYpad,1,NXpad,1,NYpad,DMIN2,DMAX2,
     &		  DMEAN3)
	    endif
	    CALL IWRSEC(3,crray)
C	      
	    DMAX = max(dmax,dmax2)
	    DMIN = min(dmin,dmin2)
	    DMsum = dmsum + dmean3
	  endif
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
	do iv=1,nz
	  call xfwrite(1,f(1,1,iv),96)
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


*****XFMODEL.FOR*********************************************
*   Will take a model from wimp or imod, and either
*   a) use corresponding points in two sections to obtain a transformation
*      between the sections, or
*   b) transform the points in the model to match a new alignment of images
*
*	  To solve for transforms, the model objects should consist of
*   corresponding points in two or more successive sections.  The program
*   considers each pair of successive sections independently.  If an object
*   contains two points in the same section, the program will take the point
*   whose Z value is closer to that of the other section in the pair.
*
*	The program can "edit" an existing list of f transforms (transforms
*   that relate each section to the previous one).  That is, the model may
*   have points from only a few sections, or one may specify which sections to
*   find transforms for, and the program will output a list containing new
*   transforms for those sections and transforms from the existing list for
*   the rest.
*
*	In solving for transforms, the model can be built on unaligned images
*   or on images that have been aligned with a previously existing set of
*   transforms.  In the latter case, the program will request the name of the
*   appropriate file of g transforms used to align the images.
*
*	A model that was built on unaligned images can be transformed to match
*   with aligned images.  A model built on aligned images can be
*   back-transformed to match the raw, unaligned images, or it can be
*   transformed to match a new alignment of the images.  In all of these
*   cases, the program will ask for the files with the appropriate sets of g
*   transforms. 
*	  
*       When the program solves for the transformation between a pair of
*   sections, it applies the transformation to the points on the second
*   section of the pair, and computes the displacement, or deviation, between
*   each point and the corresponding point on the first section of the pair.
*   It then reports the mean deviation for all of the points, the maximum
*   deviation, and the object number of the point with maximum deviation.
*   In addition, you may elect to have a complete report of the deviations of
*   all points for particularly bad sections.  If you choose this option, you
*   control which sections are reported by specifying criterion values for the
*   mean and maximum deviations; the full report will be made for any sections
*   with mean or maximum deviations greater than the respective criteria.
*
*	If the images are montaged, this is specified by entering the
*   the name of the file of piece coordinates.  The Z values in this list of
*   pieces are used to establish the correspondence between Z values in the
*   model and transform number in the list of transforms.  If the image is
*   missing some sections, the program will detect this fact and ask whether
*   the transform lists contain a transform only for each existing section or
*   a transform for each section number, including the missing sections.  The
*   choice here will be applied both to lists of existing transforms that are
*   read in and to the list that is computed by the program, if any.  In either
*   case, if there are model objects that bridge a gap over missing
*   sections, the program can compute a transform between the sections on
*   either side of the gap.
*	  
*	  Instead of solving for transforms between pairs of adjacent sections,
*  the typical mode of operation, the program can solve for transforms between
*  a single specified section and each other section.  To use this feature,
*  enter "-999" when the program requests a list of sections to find transforms
*  for.  Next enter the number of the single section; a transform will be found
*  relating each other section to that single section.  Then enter the true
*  list of sections to find transforms for.
*
*	  It is possible to find the X/Y translation alone that best aligns
*  the set of points on a section to those on a previous section.  The
*  resulting transformations (which involve no rotations or size changes)
*  can be used in a second stage of model alignment to remove progressive
*  shifts in position while retaining trends in size and rotation.  To obtain
*  translations only, enter 2 rather than 0 when specifying that you want
*  to find transformation.
*	  
*	  It is also possible to find the translation and rotation alone that
*  best aligns two sections.  The resulting transformations (which involve no
*  size changes) can be used in a second stage of model alignment to remove
*  progressive shifts in position and rotations while retaining trends in size.
*  To obtain translations and rotations only, enter 3 rather than 0 when
*  specifying that you want to find transformation.
c
c	  Just to round out the options, you can also enter 4 to obtain
c  transformations that include translation, rotation, and magnification change
c  but no stretch.
*	  
c	  ENTRIES TO THE PROGRAM
c	  
c	  Name of image file that model was built on, or blank line to enter
c	  center coordinates instead
c	  
c	  IF you entered an image file name, next enter the name of a piece
c	  list file if the image is a montage, otherwise enter a blank line
c	  
c	  OR if you did not enter an image file, next enter the X and Y index
c	  coordinates of the center of the image (NX/2, NY/2)
c	  
c	  IF there are gaps in the Z values described by the piece list, next
c	  enter 0 if transform files have transforms only for the Z values
c	  that exist, or 1 if they have transforms for all Z values
c	  
c	  Name of model file
c	  
c	  Enter one of:
c	  -1 to back-transform the model to fit a raw image stack
c	  0 to find linear transformations
c	  1 to transform the model with a set of transformations
c	  2 to find X/Y translations only
c	  3 to find translations and rotations
c	  4 to find translation, ratation, and mag change
c	  
c	  IF you entered any option other than -1 to back-transform, next enter
c	  0 if the model was built on raw sections, or the sections that you
c	  want to further transform; or 1 if the model was built on pre-aligned
c	  sections and you want to reference transforms to the raw sections
c	  
c	  IF you are back-transforming OR if you entered 1 to the last query,
c	  next enter the file name of the g transforms used to pre-align the 
c	  sections
c	  
c	  IF you are transforming or back-transforming the model, next enter
c	  the name of the output model file.  This is the final entry for
c	  option -1
c
c	  IF you are transforming, enter the name of the file with transforms
c	  to apply; this is the final entry for option 1
c	  
c	  IF you finding transforms instead, continue with the following
c	  entries:
c	  
c	  Name of file with existing f transforms to be replaced by any
c	  transforms that are solved for
c	  
c	  Name of output file for new f transforms
c	  
c	  A list of section numbers to find transforms for (the second section
c	  of each pair, sections numbered from zero), or / to find transforms
c	  for all sections with data in the model, or -999 to find transforms
c	  of all sections relative to a single section
c	  
c	  IF you entered -999, next enter:
c	  .  The number of the single section
c	  .  The real list of transforms to find sections for, or / for all
c	  
c	  1 for complete reports of the deviations for each point on sections
c	  with bad fits, or 0 for no detailed reports
c	  
c	  IF you entered 1, then enter a criterion for the mean deviation and
c	  a criterion for the maximum deviation; a complete report will be 
c	  given for any section that exceeds either criterion.
c
c
*	  David Mastronarde 1988
c	  DNM 7/20/89  changes for new model format
c	  DNM 1/10/90  have it transform only existing model points, not all
c	  points in p_coord array, to avoid bad Z values
c	  DNM 5/28/90  fix bug in rounding 0.5 values, implement ability to
c	  transform relative to a single section.
c	  DNM 3/31/92  Implement translation only finding
c	  DNM 5/1/92   Implement translation and rotation only finding
c	  DNM 4/24/95 changed model reading/writing to be portable
c	  DNM 9/23/97  Add translation, rotation, mag only finding
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2003/10/26 15:31:48  mast
c	  switch from long prints to formatted writes for Intel compiler
c	
c	  Revision 3.1  2002/09/05 05:36:45  mast
c	  Changes to take scaling information from model header and to scale
c	  coordinates properly to index coordinates.  Also put in error
c	  checks, standardize error outputs and made declarations for implicit
c	  none
c	
c
	implicit none
	include 'model.inc'
	integer nflimit,limpcl,idim,lmGrid
	parameter (nflimit=1001,limpcl=100000)
	real*4 f(2,3,nflimit),g(2,3,nflimit),gtmp(2,3)
	integer*4 nxyz(3),mxyz(3),nx,ny,nz,mode
	equivalence (nxyz(1),nx),(nxyz(2),ny),(nxyz(3),nz)
c	real*4 delt(3)
	integer*4 nsec(nflimit),listz(nflimit),indfl(nflimit)
	integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
	parameter (idim=400)
	parameter (lmGrid = 200)
	include 'statsize.inc'
	real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     &	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &	    , b(msiz), b1(msiz)
	character*120 modelfile,newmodel,oldxfgfile,oldxffile,newxffile,idfFile
	logical gotthis,gotlast,exist,readw_or_imod
	integer*4 getimodhead,getimodscales,getimodmaxes
	integer*4 limpnts/4/			!min # of points for regression
c	  
	integer*4 i,nlistz,nfout,npclist,izrange,iffillgap,indf,indval
	integer*4 minxpiece,nxpieces,nxoverlap,minypiece,nypieces,nyoverlap
	real*4 xhaf,yhaf,xcen,ycen,critmean,critmax,dmin,dmax,dmean
	integer*4 ifxfmod,iftrans,ifrotrans,ifprealign,ntofind, ifmagrot
	integer*4 ifsingle,izsingle,iffullrpt,ierr,ierr2,ifflip,indg
	real*4 xyscal,zscale,xofs,yofs,zofs
	real*4 ximscale, yimscale, zimscale,zz,zdex,zthis,zlast
	integer*4 nundefine,iobj,ipt,iz,nfgin,indind,izmin,izmax,ibase
	integer*4 lastsec,npnts,iobject,ninobj,ipnt,isol,ipntmax,j
	real*4 const,rsq,fra,theta,sinth,costh,gmag,devsum,devmax
	real*4 xdev,ydev,devpnt,devavg,xx,yy
	integer*4 loop,noldg,izsec,il,indobj,maxx,maxy,maxz
	real*4 atan2d
c
	integer*4 ifDistort, idfBinning, iBinning, idfNx, idfNy
	integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
	real*4 xGridIntrv, yGridIntrv, pixelIdf, binRatio, dx, dy
	real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
	character*10240 stringList
c	  
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger,PipGetBoolean, PipNumberOfEntries
	integer*4 PipGetString,PipGetTwoIntegers, PipGetFloatArray, PipGetFloat
	integer*4 PipGetIntegerArray, PipGetNonOptionArg, PipGetTwoFloats
	integer*4 PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  xfmodel
c
	integer numOptions
	parameter (numOptions = 20)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'image:ImageFile:FN:@piece:PieceListFile:FN:@'//
     &      'center:CenterInXandY:FP:@transonly:TranslationOnly:B:@'//
     &      'rottrans:RotationTranslation:B:@magrot:MagRotTrans:B:@'//
     &      'back:BackTransform:B:@edit:EditTransforms:FN:@'//
     &      'sections:SectionsToAnalyze:LI:@single:SingleSection:I:@'//
     &      'full:FullReportMeanAndMax:FP:@'//
     &      'prealign:PrealignTransforms:FN:@'//
     &      'xforms:XformsToApply:FN:@allz:AllZhaveTransforms:B:@'//
     &      'distort:DistortionField:FN:@binning:BinningOfImages:I:@'//
     &      'param:ParameterFile:PF:@help:usage:B:'
c	  
c	  set defaults
c	  
	modelfile = ' '
	xcen = 0.
	ycen = 0.
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'xfmodel',
     &	    'ERROR: XFMODEL - ', .true., 3, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

c
c   get parameters
c
	if (pipinput) then
	  ierr = PipGetString('ImageFile', modelfile)
	else
	  write(*,'(1x,a,$)')
     &	      'Image file (or Return to enter Xcen, Ycen directly): '
	  read(*,'(a)')modelfile
	endif
c
c	  if no image file, get crucial info directly
c	  DNM 8/28/02: just get xcen, ycen since origin and delta aren't needed
c
	if(modelfile.eq.' ')then
	  if (pipinput) then
	    ierr = PipGetTwoFloats('CenterInXandY', xcen, ycen)
	  else
	    print *,char(7),
     &		'Be SURE to enter CENTER coordinates, NOT NX and NY'
	    write(*,'(1x,a,$)')'Xcen, Ycen: '
	    read(*,*)xcen,ycen
	  endif
	  do i=1,nflimit
	    listz(i)=i-1
	  enddo
	  nlistz=nflimit
	  nfout=0
	else
c
c	    otherwise get header info from image file
	  call imopen(1,modelfile,'ro')
c
c	    get header info for proper coordinate usage
	  call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
c	    call irtdel(1,delt)
c	  call irtorg(1,xorig,yorig,zorig)
c	  write(*,'(/,a,a,/)')' This header info MUST be the'
c     &	      ,' same as when model was built'
c
	  modelfile = ' '
	  if (pipinput) then
	    ierr = PipGetString('PieceListFile', modelfile)
	  else
	    write(*,'(1x,a,$)') 'Piece list file if image is a'//
     &		' montage, otherwise Return: '
	    read(*,'(a)')modelfile
	  endif
	  call read_piece_list(modelfile,ixpclist,iypclist,izpclist,
     &	      npclist)
	  if (npclist.gt.limpcl)call errorexit(
     &	      'too many piece coordinates for arrays')
c	  
c	    if no pieces, set up mocklist
	  if(npclist.eq.0)then
	    do i=1,nz
	      ixpclist(i)=0
	      iypclist(i)=0
	      izpclist(i)=i-1
	    enddo
	    npclist=nz
	  endif
c	    get ordered list of z values 
	  call fill_listz(izpclist,npclist,listz,nlistz)
	  if (nlistz.gt.nflimit)call errorexit(
     &	      'too many Z values for arrays')
	  
	  call checklist(ixpclist,npclist,1,nx,minxpiece
     &	      ,nxpieces,nxoverlap)
	  call checklist(iypclist,npclist,1,ny,minypiece
     &	      ,nypieces,nyoverlap)
	  xhaf=(nx+(nxpieces-1)*(nx-nxoverlap))/2.
	  yhaf=(ny+(nypieces-1)*(ny-nyoverlap))/2.

c	    DNM 8/28/02: don't scale any more
c	    but still add minxpiece - the index coordinates are montage
c	    coordinates starting at minxpiece.
c
c	    [xy]cen is the amount to SUBTRACT from real model coordinates to
c	    get coordinates centered on the center of the image.  Adding [xy]
c	    orig shifts coordinates to the lower left corner of image.
c	    Subtracting 0.5*nx*delt(1) (half of the image width in real
c	    coordinates) then shifts coords to center of image.  Hence this:
c
c	  xcen=(minxpiece+xhaf)*delt(1)-xorig
c	  ycen=(minypiece+yhaf)*delt(2)-yorig
	  xcen=(minxpiece+xhaf)
	  ycen=(minypiece+yhaf)
	  nfout=nlistz

	endif
c	  
c	  find out if gaps in z and ask how to store f's if there are gaps
c	  
	izrange=listz(nlistz)+1-listz(1)
	iffillgap=0
	if(izrange.gt.nlistz)then
	  write(*,'(1x,a,i4,a)')'There are',izrange-nlistz,
     &	      ' gaps in section Z values'
	  if (pipinput) then
	    ierr = PipGetBoolean('AllZhaveTransforms', iffillgap)
	    if (iffillgap .eq. 0) then
	      print *,'Xform lists will be assumed to have xforms ',
     &		  'only for existing sections'
	    else
	      print *,'Xform lists will be assumed to have xforms ',
     &		  'for all sections'
	    endif
	  else
	    write(*,'(1x,a,/,a$)')'Enter 0 for xform lists'//
     &		' that have xforms only for existing sections,',
     &		'    or 1 for xform lists that have xforms for'//
     &		' all Z values in range: '
	    read(*,*)iffillgap
	  endif
	endif
c	  
c	  make index from z values to transform list: index=0 for non-existent
c
	do i=1,nflimit
	  indfl(i)=0
	enddo
	do i=1,nlistz
	  indf=listz(i)+1-listz(1)
	  indval=i
	  if(iffillgap.ne.0)indval=indf
	  indfl(indf)=indval
	enddo
	if(nfout.gt.0)nfout=indfl(listz(nlistz)+1-listz(1))

c
	ifxfmod = 0
	if (PipGetInOutFile('InputFile', 1, 'Input model file', modelfile)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
	oldxfgfile = ' '
	oldxffile = ' '
	idfFile = ' '
	iftrans=0
	ifrotrans=0
	ifmagrot = 0
	if (pipinput) then
	  ierr = PipGetBoolean('TranslationOnly', iftrans)
	  ierr = PipGetBoolean('RotationTranslation', ifrotrans)
	  ierr = PipGetBoolean('MagRotTrans', ifmagrot)

	  if (PipGetString('XformsToApply', oldxffile) .eq. 0) ifxfmod = 1
	  if (PipGetString('DistortionField', idfFile) .eq. 0) ifxfmod = 1
	  if (PipGetString('BackTransform', oldxfgfile) .eq. 0) then
	    if (ifxfmod .ne. 0) call errorexit('You cannot enter both'
     &		//' -back and -xform or -distort')
	    ifxfmod = -1
	    ifprealign = 1
	  endif
	  if (iftrans + ifrotrans + ifmagrot .gt. 1 .or. (iftrans +
     &	      ifrotrans + ifmagrot .gt. 0 .and. ifxfmod .ne. 0))
     &	      call errorexit('Only one of -trans, -rottrans, -magrot,'//
     &	      '-back or -distort, and -xform may be entered')
	  if (iftrans .ne. 0) ifxfmod = 2
	  if (ifrotrans .ne. 0) ifxfmod = 3
	  if (ifmagrot .ne. 0) ifxfmod = 4

	else
c
	  write(*,'(1x,a,/,a,/,a/,a,$)') 'Enter 0 to find '//
     &	      'transformations, 2 to find X/Y translations only,',
     &	      '    3 to find translations and rotations only,',
     &	      '    4 to find translation, rotation and mag change only,',
     &	      '    1 to transform model, or -1 to back-transform model: '
	  read(*,*)ifxfmod
	endif
c	  
c	  set variables for restricted fits
c
	if(ifxfmod.eq.2)then
	  ifxfmod=0
	  iftrans=1
	  limpnts=1
	elseif(ifxfmod.eq.3)then
	  ifrotrans=1
	  ifxfmod=0
	  limpnts=2
	elseif(ifxfmod.eq.4)then
	  ifrotrans=2
	  ifxfmod=0
	  limpnts=3
	endif
c	  
c
	ifsingle=0
	iffullrpt = 0
	ntofind=0
	if (pipinput) then
	  if (PipGetString('PrealignTransforms', oldxfgfile) .eq. 0)then
	    if (ifxfmod .lt. 0) call errorexit('You cannot specify '//
     &		'both -prealign and -back')
	    ifprealign = 1
	  endif
	  if (PipGetInOutFile('OutputFile', 2, ' ', newxffile) .ne. 0)
     &	      call errorexit('NO OUTPUT FILE SPECIFIED')
	  if(ifxfmod.eq.0)then
	    oldxffile = ' '
	    ierr = PipGetString('EditTransforms', oldxffile)

	    if (PipGetTwoFloats('FullReportMeanAndMax', critmean, critmax)
     &		.eq. 0) iffullrpt = 1
	    if (PipGetInteger('SingleSection', izsingle) .eq. 0) ifsingle = 1
	    if (PipGetString('SectionsToAnalyze', stringList) .eq. 0)
     &		call parselist(stringList, nsec, ntofind)
	  else
	    newmodel = newxffile
	    if (idfFile .ne. ' ') then
	      ifDistort = 1
	      call readDistortions(idfFile, fieldDx, fieldDy, lmGrid, idfNx,
     &		  idfNy, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &		  iyGridStrt, yGridIntrv, nyGrid)
c
	      if (PipGetInteger('BinningOfImages', iBinning) .ne. 0)
     &		  call errorexit
     &		  ('FOR NOW, YOU MUST SPECIFY BINNING OF IMAGES')
	      if (iBinning .ne. idfBinning) then
		binRatio = idfBinning / float(iBinning)
		ixGridStrt = nint(ixGridStrt * binRatio)
		iyGridStrt = nint(iyGridStrt * binRatio)
		xGridIntrv = xGridIntrv * binRatio
		yGridIntrv = yGridIntrv * binRatio
		do j = 1, nyGrid
		  do i = 1, nxGrid
		    fieldDx(i, j) = fieldDx(i, j) * binRatio
		    fieldDy(i, j) = fieldDy(i, j) * binRatio
		  enddo
		enddo
	      endif
	      
	    endif
	  endif
	else
c	    
c	    old input: get prealignment or back transforms
c
	  if(ifxfmod.lt.0)then
	    ifprealign=1
	  else
	    write(*,'(1x,a,$)')
     &		'Model built on raw sections (0) or prealigned ones(1): '
	    read(*,*)ifprealign
	  endif
c
	  if(ifprealign.ne.0)then
	    write(*,'(1x,a,$)')
     &		'File of old g transforms used in prealignment: '
	    read(*,'(a)')oldxfgfile
	  endif
c
	  if(ifxfmod.eq.0)then
	    write(*,'(1x,a,$)') 'Name of old file of f transforms'//
     &		' to edit (Return if none): '
	    read(*,'(a)')oldxffile
c	      
	    write(*,'(1x,a,$)')'Name of new file of f transforms: '
	    read(*,'(a)')newxffile
c
	    write(*,118)
118	    format(' Enter / to find transforms for all section pairs,',/,
     &		'      or -999 to find transforms relative to a single',
     &		' section,',/,
     &		'      or a list of the section numbers to find',
     &		' transforms for',/,'         (enter number of second',
     &		' section of each pair, ranges are ok)')
	    call rdlist(5,nsec,ntofind)
	    if(ntofind.eq.1.and.nsec(1).eq.-999)then
	      write(*,'(1x,a,$)')'Number of single section to find'//
     &		  ' transforms relative to: '
	      read(*,*)izsingle
	      ifsingle=1
	      print *,'Now enter list of sections to find transforms',
     &		  ' for (/ for all)'
	      ntofind=0
	      call rdlist(5,nsec,ntofind)
	    endif
c	    
	    write(*,'(1x,a,$)')'1 for full reports of deviations for'//
     &		' sections with bad fits, 0 for none: '
	    read(*,*)iffullrpt
c
	    if(iffullrpt.ne.0)then
	      write(*,'(1x,a,$)')'Enter criterion mean deviation and'//
     &		  ' max deviation; full reports will be given',
     &		  '     for sections with mean OR max greater than'//
     &		  ' these criteria: '
	      read(*,*)critmean,critmax
	    endif
c
	  else
	    write(*,'(1x,a,$)')'New model file name: '
	    read(*,'(a)')newmodel
c	      
	    if(ifxfmod.gt.0)then
	      write(*,'(1x,a,$)')
     &		  'File for list of g transforms to apply: '
	      read(*,'(a)')oldxffile
	    endif
c
	  endif
	endif
	call PipDone()
c
c	  read in the model
c
75	exist=readw_or_imod(modelfile)
	if(.not.exist)go to 91
c
	ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	ierr2 = getimodscales(ximscale, yimscale, zimscale)
	if (ierr .ne. 0 .or. ierr2 .ne. 0) call errorexit(
     &	    'getting model header')
c	  
c	  if the center is not yet defined, use the model header sizes
c
	if (xcen .eq. 0. .and. ycen .eq. 0.) then
	  ierr = getimodmaxes(maxx, maxy, maxz)
	  xcen = maxx / 2.
	  ycen = maxy / 2.
	  write(*,'(a,2f8.1)')'Using model header to determine '//
     &	      'center coordinates:', xcen, ycen
	endif
c	  
c	  shift the data to display coordinates before using
c
	do i=1,n_point
	  p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
	  p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
	  p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
	enddo

c
c	  first fill array with unit transforms in case things get weird
c
	do i=1,nflimit
	  call xfunit(f(1,1,i),1.)
	enddo
c
c	  back-transform if necessary
	if(ifprealign.ne.0)then
	  call dopen(3,oldxfgfile,'ro','f')
c
c	    get g transforms into g list
	  call xfrdall(3,g,noldg,*92)
	  if (noldg.gt.nflimit)call errorexit(
     &	      'too many transforms for arrays')	
	  close(3)
c	    
c	    invert the g's into the g list
	  do indg=1,noldg
	    call xfinvert(g(1,1,indg),gtmp)
	    call xfcopy(gtmp,g(1,1,indg))
	  enddo
c	    
c	    apply inverse g's to all points in model
	  call transformModel(g, noldg, nflimit, xcen, ycen, indfl,
     &		listz, nundefine)
	    
	  if(ifxfmod.lt.0)then
c
c	      write out back-transformed model
	    call rescaleWriteModel(newmodel, nundefine)
	    call exit(0)
	  endif
	endif
c
c	  read in the old f's or g's for whatever purpose 
c	  
	nfgin=0
	if(oldxffile.ne.' ')then
	  call dopen(1,oldxffile,'ro','f')
	  call xfrdall(1,f,nfgin,*92)
	  if (nfgin.gt.nflimit)call errorexit(
     &	      'too many transforms for arrays')	
	  close(1)
	endif
	if(ifxfmod.ne.0)then
	  if (ifDistort .ne. 0) then
c	    
c	    undistort the model
c	      
	    do i=1,n_point
	      call interpolateGrid(p_coord(1,i), p_coord(2,i), fieldDx,
     &		  fieldDy, lmGrid, idfNx, idfNy, ixGridstrt, xGridIntrv,
     &		  iyGridStrt, yGridIntrv, dx, dy)
	      p_coord(1,i) = p_coord(1,i) - dx
	      p_coord(2,i) = p_coord(2,i) - dy
	    enddo
c	      
c	      if there was prealignment and no new transforms, get the 
c	      prealignment transforms back by inversion and set up to use them
c	      
	    if (nfgin .eq. 0 .and. ifprealign .ne. 0) then
	      do indg=1,noldg
		call xfinvert(g(1,1,indg), f(1,1,indg))
	      enddo
	      nfgin = noldg
	    endif
	  endif
c	    
	  nundefine = 0
c
c	      transform the model
c
	  if (nfgin .ne. 0) call transformModel(f, nfgin, nflimit,
     &	      xcen, ycen, indfl, listz, nundefine)
	    
	  call rescaleWriteModel(newmodel, nundefine)
	else
c
c	    SEARCH FOR POINTS TO DERIVE XFORMS FROM
c
c	    first find min and max z in model
	  izmin=100000
	  izmax=-izmin
	  do iobj=1,max_mod_obj
	    ibase=ibase_obj(iobj)
	    do ipt=1,npt_in_obj(iobj)
	      i=abs(object(ipt+ibase))
	      zz=p_coord(3,i)
c	      zdex=(zz+zorig)/delt(3)
	      zdex=zz
	      iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
c	      if(iz.lt.listz(1).or.iz.gt.listz(nlistz))go to 93
	      izmin=min0(izmin,iz)
	      izmax=max0(izmax,iz)
	    enddo		  
	  enddo
c
c	    if didn't specify list of section #'s, make such a list from range
c
	  if(ntofind.le.0)then
	    ntofind=ifsingle+izmax-izmin
	    do i=1,ntofind
	      nsec(i)=izmin+i-ifsingle
	    enddo
	  endif
	  nfout=max(nfout,nfgin)
	  write(*,122)
122	  format(40x,'Deviations between transformed points on',/,
     &	      41x,'section and points on previous section',/,
     &	      32x,'Mean     Max  @object & point #   X-Y Position')
	  do loop=1,ntofind
	    izsec=nsec(loop)
c	      
c	      make sure this is a section in list and find previous z
c	      
	    lastsec=-100000
	    do il=2,nlistz
	      if(izsec.eq.listz(il))lastsec=listz(il-1)
	    enddo
c	      
c	      but if doing to single section, allow section to be first in list
c	      as well and set lastsec to z of single section
c
	    if(ifsingle.ne.0 .and. (lastsec.ne.-100000 .or.
     &		izsec.eq.listz(1)))lastsec=izsingle
	    if(lastsec.ne.-100000)then
c
c		get points out of objects with points in both this and previous
c		section
c
	      npnts=0
	      do iobject=1,max_mod_obj
		gotlast=.false.
		gotthis=.false.
		ninobj=npt_in_obj(iobject)
		do indobj=1,ninobj
		  ipnt=object(indobj+ibase_obj(iobject))
		  if(ipnt.gt.0.and.ipnt.le.n_point)then
c		    zdex=(p_coord(3,ipnt)+zorig)/delt(3)
		    zdex=p_coord(3,ipnt)
		    iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
		    if((iz.eq.izsec).and.(.not.gotthis.or.
     &			(p_coord(3,ipnt).lt.zthis)))then
c
c			if in second section, and either haven't gotten a point
c			there before, or this point has a lower z than the
c			previous point, put x and y into 1st and 2nd
c			column: independent vars
c
		      xr(1,npnts+1)=p_coord(1,ipnt)-xcen
		      xr(2,npnts+1)=p_coord(2,ipnt)-ycen
		      gotthis=.true.
		      zthis=p_coord(3,ipnt)
		      xr(6,npnts+1)=iobject
		      xr(7,npnts+1)=indobj
		    elseif((iz.eq.lastsec).and.(.not.gotlast.or.
     &			  (p_coord(3,ipnt).gt.zlast)))then
c
c			if in first section, and either haven't gotten a point
c			there before, or this point is higher in z than the
c			previous point, put x and y into 4th and 5th
c			column: dependent vars
c
		      xr(4,npnts+1)=p_coord(1,ipnt)-xcen
		      xr(5,npnts+1)=p_coord(2,ipnt)-ycen
		      gotlast=.true.
		    endif
		  endif
		enddo
		if(gotthis.and.gotlast)npnts=npnts+1
		if (npnts.ge.idim)then
		  print *
		  print *,'ERROR: XFMODEL - ',
     &		      'too many points for arrays on section', iz
		  call exit(1)
		endif
	      enddo				!done with looking at objects
	      if(npnts.ge.limpnts)then
c
c		  now if there are at least limpnts points, do regressions:
c		  first last section x then l.s. y as function of this section
c		  x and y
c		  save results in xform for izsec
c
		indf=indfl(izsec+1-listz(1))
		if(indf.le.0)go to 93
		if(ifrotrans.eq.0)then
		  do isol=1,2
		    if(iftrans.eq.0)then
c
c			move 4th or 5th column into 3rd for regression
c		      
		      do i=1,npnts
			xr(3,i)=xr(3+isol,i)
		      enddo
		      call multr(xr,3,npnts,sx,ss,ssd,d,r,xm,sd,b,b1,
     &			  const, rsq ,fra)
		      f(isol,1,indf)=b1(1)
		      f(isol,2,indf)=b1(2)
		    else
c		      
c			or, if getting translations only, find difference in
c			means for X or Y
c
		      const=0.
		      do i=1,npnts
			const=const+xr(3+isol,i)-xr(isol,i)
		      enddo
		      const=const/npnts
		    endif
		    f(isol,3,indf)=const
		  enddo
		else
c
c		    or find rotations and translations only by getting means
c		    and sums of squares and cross-products of deviations
c
		  call correl(xr,5,npnts,sx,ss,ssd,d,r,xm,sd,1)
		  theta=atan(-(ssd(2,4)-ssd(1,5))/(ssd(1,4)+ssd(2,5)))
		  sinth=sin(theta)
		  costh=cos(theta)
		  if(ifrotrans.eq.2)then
		    gmag=((ssd(1,4)+ssd(2,5))*costh
     &			-(ssd(2,4)-ssd(1,5))*sinth)/(ssd(1,1)+ssd(2,2))
		    sinth=sinth*gmag
		    costh=costh*gmag
		  endif
		  f(1,1,indf)=costh
		  f(1,2,indf)=-sinth
		  f(2,1,indf)=sinth
		  f(2,2,indf)=costh
		  f(1,3,indf)=xm(4)-xm(1)*costh+xm(2)*sinth
		  f(2,3,indf)=xm(5)-xm(1)*sinth-xm(2)*costh
		endif
c		  
c		  compute mean and max deviation between points in adjacent
c		  sections after the transformation is applied
c		  
		devsum=0.
		devmax=-1.
		do ipnt=1,npnts
		  call xfapply(f(1,1,indf),0.,0.,xr(1,ipnt),xr(2,ipnt),xx,
     &		      yy)
		  xdev=xr(4,ipnt)-xx
		  ydev=xr(5,ipnt)-yy
		  devpnt=sqrt(xdev**2+ydev**2)
		  xr(8,ipnt)=xr(4,ipnt)+xcen
		  xr(9,ipnt)=xr(5,ipnt)+ycen
		  xr(10,ipnt)=xdev
		  xr(11,ipnt)=ydev
		  if(abs(xdev).gt.1.e-6.and.abs(ydev).gt.1.e-6)then
		    xr(12,ipnt)=atan2d(ydev,xdev)
		  else
		    xr(12,ipnt)=0.
		  endif
		  xr(13,ipnt)=devpnt
		  devsum=devsum+devpnt
		  if(devpnt.gt.devmax)then
		    devmax=devpnt
		    ipntmax=ipnt
		  endif
		enddo
		devavg=devsum/npnts
C
c		  keep track of the highest & lowest transforms obtained
c
		nfout=max0(nfout,indf)
		write(*,121)npnts,izsec,devavg,devmax,
     &		    nint(xr(6,ipntmax)),nint(xr(7,ipntmax)),
     &		    xr(8,ipntmax),xr(9,ipntmax)
121		format(i4,
     &		    ' points, section #',i4,2x,2f9.2,2i6,4x,2f9.2)
		if(iffullrpt.ne.0.and.
     &		    (devmax.ge.critmax.or.devavg.gt.critmean))
     &		    write(*,124)((xr(i,j),i=6,13),j=1,npnts)
124		format('    Object  Point       position        ',
     &		    'deviation vector   angle   magnitude',/,
     &		    (f10.0,f6.0,4f10.2,f9.0,f10.2))
	      else
		print *,'less than',limpnts,' points for section # ',
     &		    izsec
	      endif
	    endif
	  enddo					!end of loop the loop
c
c	    write out enough for whole file, and at least as many as were in
c	    an input file if any
c
	  call dopen(2,newxffile,'new','f')
	  do i=1,nfout
	    call xfwrite(2,f(1,1,i),*94)
	  enddo
	  close(2)
	endif
	call exit(0)
91	call errorexit('reading model file')
92	call errorexit('reading old f/g file')
93	print *
	print *,'ERROR: XFMODEL - z value out of range for ',
     &	    'transforms: ',zz
	call exit(1)
94	call errorexit('writing out f file')
	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: XFMODEL - ',message
	call exit(1)
	end


	subroutine transformModel(f, nfgin, nflimit, xcen, ycen,
     &	    indfl, listz, nundefine)
	implicit none
	include 'model.inc'
	real*4 f(2,3,*), xcen, ycen
	integer*4 nfgin, nflimit, nundefine, indfl(*), listz(*)
	integer*4 iobj, ipt, i, iz, indind, indg
	real*4 zdex,zz
c
	nundefine=0
	do iobj=1,max_mod_obj
	  do ipt=1,npt_in_obj(iobj)
	    i=abs(object(ibase_obj(iobj)+ipt))
	    zz=p_coord(3,i)
c	      zdex=(zz+zorig)/delt(3)
	    zdex=zz
	    iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
	    indind=iz+1 -listz(1)
	    if(indind.lt.1.or.indind.gt.nflimit)then
	      nundefine=nundefine+1
	    else
	      indg=indfl(indind)
	      if(indg.lt.1.or.indg.gt.nfgin)then
		nundefine=nundefine+1
	      else
		call xfapply(f(1,1,indg),xcen,ycen,p_coord(1,i),
     &		    p_coord(2,i),p_coord(1,i),p_coord(2,i))
	      endif
	    endif
	  enddo
	enddo
	return
	end

	subroutine rescaleWriteModel(newmodel, nundefine)
	implicit none
	include 'model.inc'
	integer*4 nundefine
	character*(*) newmodel
	integer*4 getimodhead,getimodscales,ifflip,i,ierr
	real*4 xyscal,zscale,xofs,yofs,zofs
	real*4 ximscale, yimscale, zimscale
	
	ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	ierr = getimodscales(ximscale, yimscale, zimscale)
c
	if(nundefine.gt.0)print *,nundefine,
     &		' points with Z values out of range of transforms'
c	    
c	  write model out
c	  shift the data back for saving
c	  
	do i=1,n_point
	  p_coord(1,i)=ximscale*p_coord(1,i)+xofs
	  p_coord(2,i)=yimscale*p_coord(2,i)+yofs
	  p_coord(3,i)=zimscale*p_coord(3,i)+zofs
	enddo
	call write_wmod(newmodel)
	return
	end



	subroutine readDistortions(idfFile, fieldDx, fieldDy, lmGrid, idfNx,
     &	    idfNy, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &	    iyGridStrt, yGridIntrv, nyGrid)
	character*(*) idfFile
	integer*4  idfBinning, idfNx, idfNy, lmGrid
	integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
	real*4 xGridIntrv, yGridIntrv, pixelIdf
	real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
c	  
	integer*4 idfVersion, i, j
c	  
	call dopen(14, idfFile, 'ro', 'f')
	read(14, *) idfVersion
	if (idfVersion .eq. 1) then
	  read(14, *)idfNx, idfNy, idfBinning, pixelIdf
	  read(14, *)ixGridStrt, xGridIntrv, nxGrid, iyGridStrt,
     &	      yGridIntrv, nyGrid
	  if (nxGrid .gt. lmGrid .or. nyGrid .gt. lmGrid) then
	    print *
	    print *,'ERROR: readDistortions - too many grid points for arrays'
	    call exit(1)
	  endif
	  do j = 1, nyGrid
	    read(14, *)(fieldDx(i, j), fieldDy(i,j), i = 1, nxGrid)
	  enddo
	else
	  print *
	  print *,'ERROR: readDistortions - version',idfVersion,
     &	      ' of idf file not recognized'
	  call exit(1)
	endif
	close(14)
	return
	end

	subroutine interpolateGrid(x, y, dxGrid, dyGrid, ixgDim,
     &	    nxGrid, nyGrid, ixGridStrt, xGridIntrv, iyGridStrt,
     &	    yGridIntrv, dx, dy)
	implicit none
	integer*4 ixgDim, nxGrid, nyGrid, ixGridStrt, iyGridStrt
	real*4 dxGrid(ixgDim, *), dyGrid(ixgDim, *)
	real*4 xGridIntrv, yGridIntrv, x, y, dx, dy
	real*4 xgrid,ygrid,fx1,fx,fy1,fy,c00,c10,c01,c11
	integer*4 ixg,iyg,ixg1,iyg1

	xgrid = 1. + (x - ixGridStrt) / xGridIntrv
	ixg=xgrid
	ixg=max(1,min(nxGrid-1,ixg))
	fx1=max(0.,min(1.,xgrid-ixg))		!NO EXTRAPOLATIONS ALLOWED
	fx=1.-fx1
	ixg1=ixg+1
	ygrid = 1. + (y - iyGridStrt) / yGridIntrv
	iyg=ygrid
	iyg=max(1,min(nyGrid-1,iyg))
	fy1=max(0.,min(1.,ygrid-iyg))
	fy=1.-fy1
	iyg1=iyg+1
	c00=fx*fy
	c10=fx1*fy
	c01=fx*fy1
	c11=fx1*fy1
c	  
c	  interpolate
c
	dx = c00*dxGrid(ixg,iyg) + c10*dxGrid(ixg1,iyg)
     &	    + c01*dxGrid(ixg,iyg1) + c11*dxGrid(ixg1,iyg1)
	dy = c00*dyGrid(ixg,iyg) + c10*dyGrid(ixg1,iyg)
     &	    + c01*dyGrid(ixg,iyg1) + c11*dyGrid(ixg1,iyg1)
	return
	end


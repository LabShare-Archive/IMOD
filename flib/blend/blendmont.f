* * * * * * * * * BLENDMONT.FOR * * * * * * *
c
c	  BLENDMONT will take montaged images, blend their overlapping edges
c	  together, and output the blended images with essentially no overlap.
c	  Transformations can be applied to align serial sections, and
c	  densities can be floated over whole sections so that density
c	  occupies the maximum range for each section.  The program can also
c	  correct for minor or substantial displacements between pieces, using
c	  cross-correlation to find substantial shifts.  Multinegative montages
c	  can also be handled, with each negative transformed so as to produce
c	  the best fit between the negatives.
c
c	  See the man page for further details.
c
c	  David Mastronarde, February 1989
c	  12/21/98 added capability to do initial cross-correlation of overlaps
c	  and correct for sloppy montages
c	  12/99: revamped the treatment of overlap zones to avoid artifacts in
c	  sloppy montages
c	  8/22/00: made it rename existing edge function files if 'new' ones
c	  are requested and they already exist
c	  12/11/00: implemented byte-swapping when reading edge function files
c	  built on other machines
c	  6/1/01: implemented ability to write and read in edge correlation
c	  displacements; added a search step to improve on cross-correlations  
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2003/06/20 20:18:13  mast
c	  Standardized error reporting
c	
c	  Revision 3.1  2002/08/19 03:19:14  mast
c	  Implemented include file for commons.  Put large arrays in common
c	  to prevent stack problems.  Passed array to find_best_shifts to
c	  reduce memory needs there.
c	
c
	include 'recl_bytes.inc'
	include 'blend.inc'
	character*80 filnam,edgenam
	character*80 concat
	character*4 edgeext(2)/'.xef','.yef'/
	character*4 xcorrext/'.ecd'/
	integer*4 mxyzin(3),nxyzst(3)/0,0,0/
	real*4 cell(6)/1.,1.,1.,0.,0.,0./
c
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech

c
c
c	structure /rotrans/
c	  real*4 theta,dx,dy,xcen,ycen
c	end structure
c
c
	integer*4 nskip(2)/1,2/			!regression position skip s&l
	integer*4 nfit(2)/5,7/			!# to include in regression s&l
	integer*4 nxgrid(2),nygrid(2)		!# of grid points for x&y edges
	integer*4 igridstr(2),iofset(2)		!just temp usage here
c
	parameter (limneg=30)
	integer*4 minnegx(limneg),maxnegx(limneg) !min and max coords of
	integer*4 minnegy(limneg),maxnegy(limneg) !each neg
	integer*4 jointlower(limneg,2),jointupper(limneg,2) !joints around neg
	integer*4 neglower(limneg,2),negupper(limneg,2)	!negs around joint
	integer*4 negind(limneg)		!index to arbitrary neg #'s
	integer*4 nhadjsum(limneg),njoint(2)
	integer*4 nedonjoint(15),listonjoint(15,limneg)	!#, list edges on joint
	integer*4 ixpclo(15),ixpcup(15),iypclo(15),iypcup(15)
c
	integer*4 iblend(2)			!blending width in x and y
	integer*4 indoneedge(2),indedge4(3,2),nedgetmp(2)
	integer*4 listz(limsect),izwant(limsect)
	logical skipxforms
	character dat*9, tim*8
	real*4 edgefrac(2),title(20),distout(2),edgefrac4(3,2)
	equivalence (edgefrac(1),edgefracx),(edgefrac(2),edgefracy)
	integer*2 irray(limxypc,limxypc,limsect)
	equivalence (array,irray)
	logical edgedone(limedge,2),multineg(limsect),active4(3,2)
	logical anypixels,inframe,dofast,anyneg,anylinesout,xinlong
	logical shifteach,docross,fromedge,exist,xcreadin,xclegacy,outputpl
	real*4 dxgridmean(limedge,2),dygridmean(limedge,2)
	real*4 edgedispx(limedge,2),edgedispy(limedge,2)
	real*4 aftermean(2),aftermax(2)
	character*6 edgexcorrtext(2)/'xcorr:','edges:'/
	real*4 gl(2,3,limsect),h(2,3,limnpc),fastf(2,3),hit(2,3),fstmp(2,3)
c	  the former rotrans structures
	real*4 hcum(6,limneg),hadj(6,limneg),r(6,limneg,2),hfram(6),
     &	    rnet(6)
	integer*4 modepow(0:15)/8,15,8,0,0,0,0,0,0,9,10,11,12,13,14,15/
	integer*4 rename
	integer*4  minxwant, maxxwant
	integer*4  minywant, maxywant
c	  
	integer numOptions
	parameter (numOptions = 25)
	character*(80 * numOptions) options(1)
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipParseInput, PipGetInteger,PipGetBoolean
	integer*4 PipGetString,PipGetFloat, PipGetIntegerArray
	integer*4 PipGetTwoIntegers, PipGetTwoFloats,PipGetLogical
	integer*4 PipGetNonOptionArg, PipPrintHelp
	options(1) =
     &	    'imin:ImageInputFile:FN:Montaged image input file to be'//
     &	    ' blended@'//
     &	    'plin:PieceListInput:FN:File with list of piece'//
     &	    ' coordinates for image input file@'//
     &	    'imout:ImageOutputFile:FN:Output file for blended images@'//
     &	    'plout:PieceListOutput:FN:File for list of coordinates of'//
     &	    ' pieces in output image file@'//
     &	    'rootname:RootNameForEdges:CH:Root name for edge function'//
     &	    ' and .ecd files@'//
     &	    'mode:ModeToOutput:I:Mode for output file - 0 for bytes,'//
     &	    ' 1 for integers, 2 for reals@'//
     &	    'float:FloatToRange:B:Stretch intensities of each output'//
     &	    ' section to fill range of data mode@'//
     &	    'xform:TransformFile:FN:File with transformations to'//
     &	    ' apply@'//
     &	    'center:TransformCenterXandY:TF:X and Y coordinates of'//
     &	    ' center of transformations@'//
     &	    'sloppy:SloppyMontage:B:Do initial cross-correlations for'//
     &	    ' finding edge functions@'//
     &	    'shift:ShiftPieces:B:Shift pieces to minimize'//
     &	    ' displacements in the overlap zones@'//
     &	    'edge:ShiftFromEdges:B:Use only edge functions for'//
     &	    ' shifting pieces@'//
     &	    'xcorr:ShiftFromXcorrs:B:Use only cross-correlations of'//
     &	    ' overlap zones for shifting pieces (legacy behavior)@'//
     &	    'readxcorr:ReadInXcorrs:B:Read displacements from .ecd'//
     &	    ' file instead of computing correlations@'//
     &	    'sections:SectionsToDo:LI:Sections to blend into output'//
     &	    ' file@'//
     &	    'xminmax:StartingAndEndingX:TI:Minimum and maximum X index'//
     &	    ' coordinates to output@'//
     &	    'yminmax:StartingAndEndingY:TI:Minimum and maximum Y index'//
     &	    ' coordinates to output@'//
     &	    'maxsize:MaximumNewSizeXandY:TI:Maximum size in X and Y of'//
     &	    ' pieces in output file@'//
     &	    'minoverlap:MinimumOverlapXandY:TI:Minimum overlap between'//
     &	    ' pieces in X and Y in output file@'//
     &	    'oldedge:OldEdgeFunctions:B:Use existing edge functions@'//
     &	    'perneg:FramesPerNegativeXandY:TI:Number of frames per'//
     &	    ' negative for multi-negative montage@'//
     &	    'missing:MissingFromFirstNegativeXandY:TI:Number of pieces'//
     &	    ' missing from first negative in X and Y@'//
     &	    'width:BlendingWidthXandY:TI:Width in X and Y across which'//
     &	    ' to blend overlaps@'//
     &	    'param:ParameterFile:PF:Read parameter entries from file@'//
     &	    'help:usage:B:Print help output'
c
c	  initialization of elements in common; this is done with statements
c	  to avoid huge executables on SGI
c
	iunedge(1)=7
	iunedge(2)=8
	indent(1)=5				!minimum indent short & long
	indent(2)=5
	intgrid(1)=10				!grid interval short & long
	intgrid(2)=10
	iboxsiz(1)=10				!box size short & long
	iboxsiz(2)=15
	iffloat = 0
	ifsloppy = 0
	ioptabs = 0
	nxmissing = 0
	nymissing = 0
	ifoldedge = 0
	shifteach = .false.
	xclegacy = .false.
	fromedge = .false.
	xcreadin = .false.
c
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
	    ierr = PipPrintHelp('blendmont', 0, 0, 0)
	    call exit(0)
	  endif

	  if (PipGetString('ImageInputFile', filnam) .ne. 0) call
     &	      errorexit('NO INPUT IMAGE FILE SPECIFIED')
	else

	  write(*,'(1x,a,$)')'Input image file: '
	  read(5,'(a)')filnam
	endif
	call imopen(1,filnam,'ro')
	call irdhdr(1,nxyzin,mxyzin,modein,dmin,dmax,dmean)
c	  
	if (pipinput) then
	  if (PipGetString('ImageOutputFile', filnam) .ne. 0) call
     &	      errorexit('NO OUTPUT IMAGE FILE SPECIFIED')

	else
	  write(*,'(1x,a,$)')'Output image file: '
	  read(5,'(a)')filnam
	endif
	call imopen(2,filnam,'new')
	call itrhdr(2,1)
	call ialnbsym(2,0)
	call ialsymtyp(2,0,0)
c	call ialcon(2,.false.)
c	  
	modeout=modein
	if (pipinput) then
	  ierr = PipGetInteger('ModeToOutput', modeout)
	else
	  write(*,'(1x,a,i2,a,$)')'Mode for output file [/ for',modeout,
     &	      ']: '
	  read(5,*)modeout
	endif
	if(modeout.lt.0.or.modeout.gt.15.or.
     &	    (modeout.ge.3.and.modeout.le.8))call errorexit(
     &	    'bad mode value')
	call ialmod(2,modeout)
c	  
c	  set up output range, and default input range and minimum.  If real
c	  input, use actual input range and min; otherwise use theoretical
c	  range for the input mode.  This will preserve data values if no float
c
	outmin=0.
	outmax=2**modepow(modeout)-1.
	if(modein.eq.2)then
	  definmax=dmax
	  definmin=dmin
	  if(modeout.eq.2)then
	    outmin=dmin
	    outmax=dmax
	  endif
	else
	  definmax=2**modepow(modein)-1.
	  definmin=0.
	endif
c	  
	filnam = ' '
	if (pipinput) then
	  ierr = PipGetBoolean('FloatToRange', iffloat)
	  ierr = PipGetString('TransformFile', filnam)
	else
	  write(*,'(1x,a,$)')
     &	      '1 to float each section to maximum range, 0 not to: '
	  read(5,*)iffloat
c
	  write(*,'(1x,a,$)')
     &	      'File of g transforms to apply (Return if none): '
	  read(5,'(a)')filnam
	endif
	dogxforms=.false.
	if(filnam.ne.' ')then
	  call dopen(3,filnam,'ro','f')
	  call xfrdall(3,gl,nglist,*98)
	  close(3)
	  dogxforms=.true.
	endif
c	
	call read_list(ixpclist,iypclist,izpclist,neglist,
     &	    multineg,npclist,minzpc,maxzpc,anyneg,pipinput)
	nsect=maxzpc+1-minzpc
	call fill_listz(izpclist,npclist,listz,nlistz)
	if(dogxforms)then
	  if (nlistz.gt.nglist) call errorexit(
     &	      'More sections than g transforms')

	  skipxforms=.false.
	  if(nlistz.lt.nsect)then
	    if(nglist.eq.nlistz)then
	      print *,'Looks like there are transforms only for '//
     &		  'the sections that exist in file'
	    elseif(nglist.eq.nsect)then
	      print *,'There seem to be transforms for each z value,'
     &		  //' including ones missing from file'
	      skipxforms=.true.
	    else
	      call errorexit('Cannot tell how transforms match up to '//
     &		  'sections, because of missing sections')
	    endif
	  endif
	endif
c	  
c	  now check lists and get basic properties of overlap etc
c
	call checklist(ixpclist,npclist,1,nxin,minxpiece,nxpieces,
     &	    nxoverlap)
	call checklist(iypclist,npclist,1,nyin,minypiece,nypieces,
     &	    nyoverlap)
	if(nxpieces.le.0. or. nypieces.le.0)call errorexit
     &	    ('CHECKLIST reported 0 pieces in one direction')
	if(nxin.ne.nxin .or. nyin.ne.nyin) call errorexit
     &	    ('frame size and image file dimension mismatch')

c
	nxtotpix=nxpieces*(nxin-nxoverlap)+nxoverlap
	nytotpix=nypieces*(nyin-nyoverlap)+nyoverlap
	if (pipinput) print *,'Input file:'
	write(*,115)nxtotpix,'X',nxpieces,nxin,nxoverlap
	write(*,115)nytotpix,'Y',nypieces,nyin,nyoverlap
115	format(i6,' total ',a1,' pixels in',i3,' pieces of',
     &	    i5, ' pixels, with overlap of',i4)
c	  
c	  find out if global multi-neg specifications are needed or desired
c	  
	if (pipinput) then
	  ierr = PipGetBoolean('SloppyMontage', ifsloppy)
	  shifteach = ifsloppy.ne.0
	  if (PipGetTwoIntegers('FramesPerNegativeXandY', nxfrmpneg,
     &		nyfrmpneg) .eq. 0) ioptabs = 1
	  ierr = PipGetLogical('ShiftPieces', shifteach)
	  ierr = PipGetLogical('ShiftFromEdges', fromedge)
	  ierr = PipGetLogical('ShiftFromXcorrs', xclegacy)
	  ierr = PipGetLogical('ReadInXcorrs', xcreadin)
	  if ((anyneg .or. ioptabs .ne. 0) .and. (shifteach .or. xcreadin))
     &	      call errorexit('you cannot use ShiftPieces or '//
     &	      'ReadInXcorrs with multiple negatives')
	  if (fromedge .and. xclegacy) call errorexit(
     &	      'you cannot use both ShiftFromEdges and ShiftFromXcorrs')
	else
	  if(anyneg)then
	    print *,'There are multi-negative specifications in list file'
	    write(*,'(1x,a,$)')'1 to do initial cross-correlations '//
     &		'in overlap zones, 0 not to: '
	    read(5,*)ifsloppy
	  else
	    print *,'Enter the negative of one of the following '//
     &		'options to do initial','cross-correlations in overlap'//
     &		' zones in combination with the particular option'
	    write(*,'(1x,a,/,a,/,a,/,a,/,a,/,a,$)')
     &		'Enter 1 to specify division'//
     &		' into negatives to apply to all sections,',
     &		'      2 to use edge functions to find a shift for each '
     &		//'frame to align frames',
     &		'      3 to use cross-correlation only to find a shift '//
     &		'for each frame',
     &		'      4 to use only cross-correlation displacements '//
     &		'read from a file',
     &		'      5 to use best shifts from edge functions and '//
     &		'correlations',
     &		'      6 to use best shifts from edge functions and '//
     &		'displacements read from a file: '
	    read(5,*)ioptneg
	    ifsloppy=0
	    if(ioptneg.lt.0)ifsloppy=1
	    ioptabs=abs(ioptneg)
	    shifteach=ioptabs.ge.2
	    fromedge=ioptabs.eq.2
	    xclegacy=ioptabs.eq.3.or.ioptabs.eq.4
	    xcreadin=ioptabs.eq.4.or.ioptabs.eq.6
	  endif
	endif

	if(ioptabs.eq.1)then
	  if (pipinput) then
	    ierr = PipGetTwoIntegers('FramesPerNegativeXandY',
     &		nxmissing, nymissing)
	  else
	    write(*,'(1x,a,$)')'# of frames per negative in X;'//
     &		' # missing from left-most negative: '
	    read(5,*)nxfrmpneg,nxmissing
	    write(*,'(1x,a,$)')'# of frames per negative in Y;'//
     &		' # missing from bottom-most negative: '
	    read(5,*)nyfrmpneg,nymissing
	  endif
	  nxneg=(nxpieces+(nxfrmpneg-1) + nxmissing)/nxfrmpneg
c	    
c	      derive frame number of each piece and assign negative #
c	    
	  do ipc=1,npclist
	    ixfrm=(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap)
	    iyfrm=(iypclist(ipc)-minypiece)/(nyin-nyoverlap)
	    ixneg=(ixfrm+nxmissing)/nxfrmpneg
	    iyneg=(iyfrm+nymissing)/nyfrmpneg
	    neglist(ipc)=1+ixneg+iyneg*nxneg
	  enddo
c	    
c	    now deduce true multi-neg character of each section
c	    
	  do iz=minzpc,maxzpc
	    ineg=iz+1-minzpc
	    multineg(ineg)=.false.
	    listfirst=-100000
	    do ipc=1,npclist
	      if(izpclist(ipc).eq.iz)then
		if(listfirst.eq.-100000)listfirst=neglist(ipc)
		multineg(ineg)=
     &		    multineg(ineg).or.(neglist(ipc).ne.listfirst)
	      endif
	    enddo
	    anyneg=anyneg.or.multineg(ineg)
	  enddo
c
	endif
c	  
	filnam = ' '
	if (pipinput) then
	  ierr = PipGetString('PieceListOutput', filnam)
	else
	  write(*,'(1x,a,$)')
     &	      'Name of new piece list file (Return for none): '
	  read(5,'(a)')filnam
	endif
	outputpl = filnam .ne. ' '
	if (outputpl) call dopen(3,filnam,'new','f')
c	  
c	  find out center of transforms
c	  
	gxcen=minxpiece+nxtotpix/2
	gycen=minypiece+nytotpix/2
	if (pipinput) then
	  ierr = PipGetTwoFloats('TransformCenterXandY', gxcen, gycen)
	else
	  if(dogxforms)then
	    write(*,'(1x,a,/,a,f6.1,a,f6.1,a,$)')
     &		'Enter true center coordinates of the transforms,',
     &		'    or / for the default',gxcen,',',gycen,
     &		', the center of the image area: '
	    read(5,*)gxcen,gycen
	  endif
	endif
c	  
c	  get list of sections desired, set up default as all sections
c	  
	do i=1,nlistz
	  izwant(i)=listz(i)
	enddo
	nzwant=nlistz
	if (pipinput) then
	  if (PipGetString('SectionsToDo', filnam) .eq. 0)
     &	      call parselist(filnam, izwant, nzwant)
	else
	  print *,'Enter list of sections to be included in output '//
     &	      'file (ranges ok)','   or / to include all sections'
	  call rdlist(5,izwant,nzwant)
	endif
c	  
c	  output size limited by line length in X
c	  
	minxoverlap=2
	minyoverlap=2
	newxframe=maxlinelength
	newyframe=65535
	ntrial=0
32	minxwant=minxpiece
	minywant=minypiece
	maxxwant=minxwant+nxtotpix-1
	maxywant=minywant+nytotpix-1
	if (pipinput) then
	  ierr = PipGetTwoIntegers('StartingAndEndingX', minxwant, maxxwant)
	  ierr = PipGetTwoIntegers('StartingAndEndingY', minywant, maxywant)
	  ierr = PipGetTwoIntegers('MaximumNewSizeXandY', newxframe, newyframe)
	  ierr = PipGetTwoIntegers('MinimumOverlapXandY', minxoverlap,
     &	      minyoverlap)
	else
	  write(*,'(1x,a,/,a,4i6,a,$)')'Enter Min X, Max X, Min Y, and'//
     &	      ' Max Y coordinates of desired output section,'
     &	      ,'    or / for whole input section [='
     &	      ,minxwant,maxxwant,minywant,maxywant,']: '
	  read(5,*)minxwant,maxxwant,minywant,maxywant
	  write(*,'(1x,a,$)')
     &	      'Maximum new X and Y frame size, minimum overlap: '
	  read(5,*)newxframe,newyframe,minxoverlap,minyoverlap
	endif
	nxtotwant=2*((maxxwant+2-minxwant)/2)
	nytotwant=2*((maxywant+2-minywant)/2)
	if(ntrial.le.1)then			!on first 2 trials, enforce min
	  minxoverlap=max(2,minxoverlap)	!overlap of 2 so things look
	  minyoverlap=max(2,minyoverlap)	!nice in wimp.  After that, let
	endif					!the user have it.
	ntrial=ntrial+1
	if(newxframe.gt. maxlinelength) call errorexit
     &	    ('output size is too large in X for arrays')
c
	call setoverlap(nxtotwant,minxoverlap,newxframe,2,newxpieces,
     &	    newxoverlap,newxtotpix)
	call setoverlap(nytotwant,minyoverlap,newyframe,2,newypieces,
     &	    newyoverlap,newytotpix)
c
	if (.not.outputpl .and. (newxpieces.gt.1 .or. newypieces.gt.1))
     &	    call errorexit('you must specify an output piece list file'//
     &	    ' to have more than one output frame')
	if (pipinput) print *,'Output file:'
	write(*,115)newxtotpix,'X',newxpieces,newxframe,newxoverlap
	write(*,115)newytotpix,'Y',newypieces,newyframe,newyoverlap
c	  
	if (.not.pipinput)then
	  write(*,'(1x,a,$)')'1 to revise frame size/overlap: '
	  read(5,*)ifrevise
	  if(ifrevise.ne.0)go to 32	  
	endif
c	  
	newminxpiece=minxwant-(newxtotpix-nxtotwant)/2
	newminypiece=minywant-(newytotpix-nytotwant)/2
	nxout=newxframe
	nyout=newyframe
	nzout=1
	dminout=1.e10
	dmaxout=-1.e10
	grandsum=0.
	call ialsiz(2,nxyzout,nxyzst)
	call ialsam(2,nxyzout)
	cell(1)=nxout
	cell(2)=nyout
	cell(3)=nzout
	call ialcel(2,cell)
	call date(dat)
	call time(tim)
c
c 7/7/00 CER: remove the encodes
c
c       encode(80,90,title) dat,tim
        write(titlech,90) dat,tim
90	format( 'BLENDMONT: Montage pieces blended and recut',
     &	    13x, a9, 2x, a8 )
	read(titlech,'(20a4)')(title(kti),kti=1,20)
	call iwrhdr(2,title,1,dmin,dmax,dmean)
	nzout=0
	hxcen=nxin/2.
	hycen=nyin/2.
c
c	  get edge indexes for pieces and piece indexes for edges
c	  first build a map in the array of all pieces present
c	  
	do iz=1,nsect
	  do iy=1,nypieces
	    do ix=1,nxpieces
	      irray(ix,iy,iz)=0
	    enddo
	  enddo
	enddo
c
	do ipc=1,npclist
	  irray(1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap),
     &	      1+(iypclist(ipc)-minypiece)/(nyin-nyoverlap),
     &	      izpclist(ipc)+1-minzpc)=ipc
	  iedgelower(ipc,1)=0
	  iedgeupper(ipc,1)=0
	  iedgelower(ipc,2)=0
	  iedgeupper(ipc,2)=0
	enddo
c	  
c	  look at all the edges in turn, add to list if pieces on both sides
c	  
	nalong=nypieces
	ncross=nxpieces
	do ixy=1,2
	  ned=0
	  do iz=1,nsect
	    do iy=1,nalong
	      do ix=2,ncross
		if(ixy.eq.1)then
		  ipclo=irray(ix-1,iy,iz)
		  ipchi=irray(ix,iy,iz)
		else
		  ipclo=irray(iy,ix-1,iz)
		  ipchi=irray(iy,ix,iz)
		endif
		if(ipclo.ne.0 .and. ipchi.ne.0)then 
		  ned=ned+1
		  ipiecelower(ned,ixy)=ipclo
		  ipieceupper(ned,ixy)=ipchi
		  iedgelower(ipchi,ixy)=ned
		  iedgeupper(ipclo,ixy)=ned
		  edgedone(ned,ixy)=.false.
		endif
	      enddo
	    enddo
	  enddo
	  nedge(ixy)=ned
	  nalong=nxpieces
	  ncross=nypieces
	enddo
c	  
c	  get edge file name root, parameters if doing a new one
c
	if (pipinput) then
	  ierr = PipGetBoolean('OldEdgeFunctions', ifoldedge)
	  if (PipGetString('RootNameForEdges', filnam) .ne. 0) call
     &	      errorexit('NO ROOT NAME FOR EDGE FUNCTIONS SPECIFIED')
	else
	  write(*,'(1x,a,$)')'0 for new edge files, 1 to read old ones: '
	  read(5,*)ifoldedge
	  write(*,'(1x,a,$)')'Root file name for edge function files: '
	  read(5,'(a)')filnam
	endif
c
	if(ifoldedge.ne.0)then
c	    
c	    for old files, open, get edge count and make sure it matches
c
	  do ixy=1,2
	    call setgridchars(nxyzin,noverlap,iboxsiz,indent,intgrid,
     &	        ixy,0,0,nxgrid(ixy),nygrid(ixy),igridstr,iofset)
	    lenrec=4*max(6,3*(nxgrid(ixy)*nygrid(ixy)+2))/nbytes_recl_item
	    edgenam=concat(filnam,edgeext(ixy))
c
c 7/20/00 CER remove readonly for gnu
c
	    open(iunedge(ixy),file=edgenam,status='old',
     &		form='unformatted',access='direct', recl=lenrec, err=53)
	    read(iunedge(ixy),rec=1)nedgetmp(ixy)
	  enddo
	  if(nedgetmp(1).ne.nedge(1).or.nedgetmp(2).ne.nedge(2))then
	    call convert_longs(nedgetmp,2)
	    if(nedgetmp(1).ne.nedge(1).or.nedgetmp(2).ne.nedge(2))
     &		call errorexit('wrong # of edges in edge function file')
	    needbyteswap=1
	  endif
	  go to 54
c	    
c	    8/8/03: if there is an error opening old files, just build new ones
c	    This is to allow command files to say use old functions even if
c	    a previous command file might not get run
c
53	  ifoldedge = 0
	  if (ixy.eq.2) close(iunedge(1))
	  print *
	  print *,'WARNING: BLENDMONT - ERROR OPENING OLD EDGE ',
     &	      'FUNCTION FILE','NEW EDGE FUNCTIONS WILL BE COMPUTED'
	endif

54	if(ifoldedge.eq.0)then
	  ifdiddle=0
c	  write(*,'(1x,a,$)')
c     &	      '1 to diddle with edge function parameters: '
c	  read(5,*)ifdiddle
	  sdcrit=2.
	  devcrit=2.
	  norder=2
	  if(ifdiddle.ne.0)then
	    write(*,'(1x,a,$)')
     &		'grid intervals in short and long directions: '
	    read(5,*)(intgrid(i),i=1,2)
	    write(*,'(1x,a,$)')'box size in short and long directions: '
	    read(5,*)(iboxsiz(i),i=1,2)
	    write(*,'(1x,a,$)')
     &		'minimum indentation in short and long directions: '
	    read(5,*)(indent(i),i=1,2)
	    write(*,'(1x,a,$)')'criterion # of sds away from mean'//
     &		' of sd and deviation: '
	    read(5,*)sdcrit,devcrit
	    write(*,'(1x,a,$)') '# grid positions in short and long'//
     &		' directions to include in regression: '
	    read(5,*)(nfit(i),i=1,2)
	    write(*,'(1x,a,$)')'order of polynomial: '
	    read(5,*)norder
	    write(*,'(1x,a,$)') 'intervals at which to do regression'
     &		//' in short and long directions: '
	    read(5,*)(nskip(i),i=1,2)
	  endif
c	    
c	    get edge function characteristics for x and y edges
c
	  needbyteswap = 0
	  do ixy=1,2
	    call setgridchars(nxyzin,noverlap,iboxsiz,indent,intgrid,
     &		ixy,0,0,nxgrid(ixy),nygrid(ixy),igridstr,iofset)
c	      open file, write header record
c	      set record length to total bytes divided by system-dependent
c	      number of bytes per item
c
	    lenrec=4*max(6,3*(nxgrid(ixy)*nygrid(ixy)+2))/nbytes_recl_item
	    edgenam=concat(filnam,edgeext(ixy))
	    inquire(file=edgenam,exist=exist)
	    if(exist)then
	      ierr=rename(edgenam,concat(edgenam,'~'))
	      if(ierr.ne.0)write(6,*)' Error attempting to rename',
     &		  ' existing edge function file'
	    endif
	    open(iunedge(ixy),file=edgenam,status='new'
     &		,form='unformatted',access='direct',recl=lenrec)
c	      write header record
	    write(iunedge(ixy),rec=1)nedge(ixy),nxgrid(ixy),nygrid(ixy)
     &		,intgrid(ixy),intgrid(3-ixy)
	  enddo
	endif
c	  
	if(xcreadin)then
	  edgenam=concat(filnam,xcorrext)
	  inquire(file=edgenam,exist=exist)
	  if(.not.exist)then
	    print *
	    print *,'ERROR: BLENDMONT - Edge correlation file does ',
     &		'not exist: ',edgenam
	    call exit(1)
	  endif
	  call dopen(4,edgenam,'ro','f')
	  read(4,*)nedgetmp(1),nedgetmp(2)
	  if(nedgetmp(1).ne.nedge(1).or.nedgetmp(2).ne.nedge(2))then
	    print *,'wrong # of edges in edge correlation file'
	    call exit(1)
	  endif
	  read(4,*)((edgedispx(i,ixy),edgedispy(i,ixy),i=1,nedge(ixy))
     &	      ,ixy=1,2)
	  close(4)
	endif
c	  
c	  make default blending width be 80% of overlap up to 50, then
c	  half of overlap above 50
	iblend(1) = max(max(nxoverlap / 2, 50), min(4 * nxoverlap / 5, 50))
	iblend(2) = max(max(nyoverlap / 2, 50), min(4 * nyoverlap / 5, 50))
	if (pipinput) then
	  ierr = PipGetIntegerArray('BlendingWidthXandY', iblend, 2, 2)
	else
	  write(*,'(1x,a,2i5,a,$)')'Blending width in X & Y (/ for'
     &	      ,iblend(1), iblend(2),'): '
	  read(5,*)(iblend(i),i=1,2)
	endif
c	  
c	  initialize memory allocator
c	  
	jusecount=0
	do i=1,memlim
	  izmemlist(i)=-1
	  lastused(i)=0
	enddo
	npixin=nxin*nyin
	limsec=min(maxsiz/npixin,memlim)
	intgrcopy(1)=intgrid(1)
	intgrcopy(2)=intgrid(2)
c	    
c	    loop on z: do everything within each section for maximum efficiency
c	  
	do ilistz=1,nlistz
	  izsect=listz(ilistz)
c	do izsect=minzpc,maxzpc
	  write(*,'(a,i4)')' working on section #',izsect
	  call flush(6)
	  multng=multineg(izsect+1-minzpc)
	  xinlong=nxpieces .gt. nypieces
c
	  if(ifoldedge.eq.0)then 
c	      
c	      first get edge functions if haven't already:
c	      loop on short then long direction
c	  
	    do iedgedir=1,2
	      call  crossvalue(xinlong,iedgedir,3-iedgedir,ixy,iyx)
c		
c		loop on all edges of that type with pieces in section that are
c		not done yet
c
	      do iedge=1,nedge(ixy)
		if(izsect.eq. izpclist(ipiecelower(iedge,ixy)) .and.
     &		    .not.edgedone(iedge,ixy))then
c		    
c		    do cross-correlation if the sloppy flag is set and either
c		    we are shifting each piece or the pieces are't on same neg
c
		  docross=ifsloppy.ne.0.and.(shifteach.or.(
     &		      anyneg.and.neglist(ipiecelower(iedge,ixy))
     &		      .ne.neglist(ipieceupper(iedge,ixy))))
		  call doedge(iedge,ixy,edgedone,sdcrit,devcrit,
     &		      nfit, norder, nskip, docross, xcreadin, xclegacy,
     &		      edgedispx, edgedispy)
c		    
c		    after each one, check memory list to see if there's any
c		    pieces with undone lower edge in orthogonal direction
c
		  do imem=1,limsec
		    ipc=izmemlist(imem)
		    if(ipc.gt.0)then
		      jedge=iedgelower(ipc,iyx)
		      if(jedge.gt.0)then
			if(.not.edgedone(jedge,iyx))then
			  docross=ifsloppy.ne.0.and.(shifteach.or.(
     &			      anyneg.and.neglist(ipiecelower(jedge,iyx))
     &			      .ne.neglist(ipieceupper(jedge,iyx))))
			  call doedge(jedge,iyx,edgedone,sdcrit,
     &			      devcrit,nfit, norder, nskip, docross,
     &			      xcreadin, xclegacy, edgedispx, edgedispy)
			endif
		      endif
		    endif
		  enddo
		endif
	      enddo
	    enddo
	  endif
c	    
c	    make a map of pieces in this sections
c
	  do iyfrm=1,nypieces
	    do ixfrm=1,nxpieces
	      mappiece(ixfrm,iyfrm)=0
	    enddo
	  enddo
	  do ipc=1,npclist
	    if(izpclist(ipc).eq.izsect)then
	      ixfrm=1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap)
	      iyfrm=1+(iypclist(ipc)-minypiece)/(nyin-nyoverlap)
	      mappiece(ixfrm,iyfrm)=ipc
	    endif
	  enddo
c		
c		now if doing multinegatives, need to solve for h transforms
c		
	  if(multng)then
c	      
c	      first make index of negatives and find coordinates of each
c	      
	    numneg=0
	    do i=1,limneg
	      maxnegx(i)=-1000000
	      minnegx(i)=1000000
	      maxnegy(i)=-1000000
	      minnegy(i)=1000000
	    enddo
	    do ipc=1,npclist
	      if(izpclist(ipc).eq.izsect)then
		iwhich=0
		do i=1,numneg
		  if(neglist(ipc).eq.negind(i))iwhich=i
		enddo
		if(iwhich.eq.0)then
		  numneg=numneg+1
		  negind(numneg)=neglist(ipc)
		  iwhich=numneg
		endif
c		  point max coordinate 1 past end: it'll be easier
		minnegx(iwhich)=min(minnegx(iwhich),ixpclist(ipc))
		maxnegx(iwhich)=max(maxnegx(iwhich),ixpclist(ipc)+nxin)
		minnegy(iwhich)=min(minnegy(iwhich),iypclist(ipc))
		maxnegy(iwhich)=max(maxnegy(iwhich),iypclist(ipc)+nyin)
	      endif
	    enddo
c	      
c	      look at all edges, make tables of joints between negs
c	      
	    do ixy=1,2
	      njoint(ixy)=0
	      do i=1,numneg
		jointupper(i,ixy)=0
		jointlower(i,ixy)=0
	      enddo
c		
	      do iedge=1,nedge(ixy)
		neglo=neglist(ipiecelower(iedge,ixy))
		negup=neglist(ipieceupper(iedge,ixy))
		if(izpclist(ipiecelower(iedge,ixy)).eq.izsect.and.
     &		    neglo.ne.negup)then
c		    convert neg #'s to neg indexes
		  do i=1,numneg
		    if(negind(i).eq.neglo)indlo=i
		    if(negind(i).eq.negup)indup=i
		  enddo
c		    see if joint already on list
		  joint=0
		  do j=1,njoint(ixy)
		    if(neglower(j,ixy).eq.indlo.and.
     &			negupper(j,ixy).eq.indup) joint=j
		  enddo
c		    if not, add to list
		  if(joint.eq.0)then
		    njoint(ixy)=njoint(ixy)+1
		    joint=njoint(ixy)
		    neglower(joint,ixy)=indlo
		    negupper(joint,ixy)=indup
c		      point the negatives to the joint
		    jointlower(indlo,ixy)=joint
		    jointupper(indup,ixy)=joint
		    nedonjoint(joint)=0
		  endif
c		    add edge to list of ones on that joint
		  nedonjoint(joint)=nedonjoint(joint)+1
		  listonjoint(nedonjoint(joint),joint)=iedge
		endif
	      enddo
c		
c		now process all edges on each joint to get a rotrans
c		
	      do joint=1,njoint(ixy)
		do ied=1,nedonjoint(joint)
		  iedge=listonjoint(ied,joint)
		  if (needbyteswap.eq.0)then
		    read(iunedge(ixy),rec=1+iedge)
     &			nxgr,nygr,ixgrdstbf(ied),ixofsbf(ied)
     &			,iygrdstbf(ied),iyofsbf(ied)
     &			,((dxgrbf(ix,iy,ied),dygrbf(ix,iy,ied)
     &			,ddengrbf(ix,iy,ied),ix=1,nxgr),iy=1,nygr)
		  else
		    read(iunedge(ixy),rec=1+iedge)nxgr,nygr
		    call convert_longs(nxgr,1)
		    call convert_longs(nygr,1)
		    read(iunedge(ixy),rec=1+iedge)
     &			idum1,idum2,ixgrdstbf(ied),ixofsbf(ied)
     &			,iygrdstbf(ied),iyofsbf(ied)
     &			,((dxgrbf(ix,iy,ied),dygrbf(ix,iy,ied)
     &			,ddengrbf(ix,iy,ied),ix=1,nxgr),iy=1,nygr)
		    call convert_longs(ixgrdstbf(ied),1)
		    call convert_longs(ixofsbf(ied),1)
		    call convert_longs(iygrdstbf(ied),1)
		    call convert_longs(iyofsbf(ied),1)
		    do iy=1,nygr
		      call convert_floats(dxgrbf(1,iy,ied),nxgr)
		      call convert_floats(dygrbf(1,iy,ied),nxgr)
		      call convert_floats(ddengrbf(1,iy,ied),nxgr)
		    enddo
		  endif
		  nxgrbf(ied)=nxgr
		  nygrbf(ied)=nygr
		  ipclo=ipiecelower(iedge,ixy)
		  ixpclo(ied)=ixgrdstbf(ied) +ixpclist(ipclo)
		  iypclo(ied)=iygrdstbf(ied) +iypclist(ipclo)
		  ipcup=ipieceupper(iedge,ixy)
		  ixpcup(ied)=ixofsbf(ied) +ixpclist(ipcup)
		  iypcup(ied)=iyofsbf(ied) +iypclist(ipcup)
		enddo
		call joint_to_rotrans(dxgrbf,dygrbf,ixgdim,iygdim,
     &		    nxgrbf,nygrbf,intgrid(ixy),intgrid(3-ixy)
     &		    ,ixpclo,iypclo,ixpcup,iypcup,nedonjoint(joint)
     &		    ,r(1,joint,ixy))
		write(*,'(1x,a,2i4,a,f6.2,a,2f6.1)')
     &		    char(ixy+ichar('W'))//' joint, negatives'
     &		    ,neglist(ipclo),neglist(ipcup),'   theta ='
     &		    ,r(1,joint,ixy),'   dx, dy ='
     &		    ,r(2,joint,ixy),r(3,joint,ixy)
	      enddo
	    enddo
c	      
c	      Resolve the edges into rotrans centered on each negative
c	      Initially set the rotrans for each negative to null
c	      
	    maxswing=0
	    do i=1,numneg
	      hcum(1,i)=0.
	      hcum(2,i)=0.
	      hcum(3,i)=0.
	      hcum(4,i)=0.5*(maxnegx(i)+minnegx(i))
	      hcum(5,i)=0.5*(maxnegy(i)+minnegy(i))
	      maxswing=max(maxswing,(maxnegx(i)-minnegx(i))/2,
     &		  (maxnegy(i)-minnegy(i))/2)
	    enddo
c	      
c	      start loop - who knows how long this will take
c	      
	    ishift=1
	    erradj=1.e10
	    nshiftneg=100
	    errlim=0.1*numneg
	    do while(ishift.le.nshiftneg.and.erradj.gt.errlim)
c		
c		set sum of adjustment h's to null
c
	      erradj=0.
	      do i=1,numneg
		call lincom_rotrans(hcum(1,i),0.,hcum(1,i),0.,hadj(1,i))
		nhadjsum(i)=0
	      enddo
c		
c		loop on joints, adding up net adjustments still needed
c
	      do ixy=1,2
		do joint=1,njoint(ixy)
c
c		    get net rotrans still needed at this joint: subtract upper
c		    h and add lower h to joint rotrans
c
		  negup=negupper(joint,ixy)
		  neglo=neglower(joint,ixy)
		  call lincom_rotrans(hcum(1,negup),-1.,r(1,joint,ixy),-1.,
     &		      rnet)
		  call lincom_rotrans(hcum(1,neglo),1.,rnet,1.,rnet)
c
c		    now add half of the net needed to the upper adjustment
c		    subtract half from the lower adjustment
c
		  call lincom_rotrans(rnet,0.5,hadj(1,negup),1.
     &		      ,hadj(1,negup))
		  nhadjsum(negup)=nhadjsum(negup)+1
		  call lincom_rotrans(rnet,-0.5,hadj(1,neglo),1.
     &		      ,hadj(1,neglo))
		  nhadjsum(neglo)=nhadjsum(neglo)+1
		enddo
	      enddo
c		
c		get the average adjustment, adjust hcum with it
c
	      dxsum=0.
	      dysum=0.
	      do i=1,numneg
		erradj=erradj+(abs(hadj(2,i))+abs(hadj(3,i))+
     &		    abs(hadj(1,i))*maxswing)/nhadjsum(i)
		call lincom_rotrans(hadj(1,i),1./nhadjsum(i),hcum(1,i),1.
     &		    ,hcum(1,i))
		dxsum=dxsum+hcum(2,i)
		dysum=dysum+hcum(3,i)
	      enddo
c		
	    enddo				!end of cycle
c	      
c	      shift all dx and dy to have a mean of zero
c	      
	    do i=1,numneg
	      hcum(1,i)=hcum(1,i)-dxsum/numneg
	      hcum(2,i)=hcum(2,i)-dysum/numneg
	    enddo
c
c	      compute the h function (and hinv) centered on corner of frame
c
	    do ipc=1,npclist
	      if(izpclist(ipc).eq.izsect)then
		do i=1,numneg
		  if(negind(i).eq. neglist(ipc))then
		    call recen_rotrans(hcum(1,i),float(ixpclist(ipc))
     &			,float(iypclist(ipc)),hfram)
		    h(1,1,ipc)=cosd(hfram(1))
		    h(2,1,ipc)=sind(hfram(1))
		    h(1,2,ipc)=-h(2,1,ipc)
		    h(2,2,ipc)=h(1,1,ipc)
		    h(1,3,ipc)=hfram(2)
		    h(2,3,ipc)=hfram(3)
		    call xfinvert(h(1,1,ipc),hinv(1,1,ipc))
		  endif
		enddo
	      endif
	    enddo
c
	  endif					!end of multineg stuff
c	    
c	    initialize edge buffer allocation
c
	  jusedgct=0
	  do ixy=1,2
	    do i=1,nedge(ixy)
	      ibufedge(i,ixy)=0
	    enddo
	  enddo
	  do i=1,limedgbf
	    iedgbflist(i)=-1
	    lasedguse(i)=0
	  enddo 
c	    
c	    test if this section is wanted in output: if not, skip out
c	    
	  ifwant=0
	  do iwant=1,nzwant
	    if(izwant(iwant).eq.izsect)ifwant=1
	  enddo
	  if(ifwant.eq.0)go to 92
c	    
c	    scan through all edges in this section to determine limits for when
c	    a point is near an edge
c	    
	  do ixy=1,2
	    edgelonear(ixy)=0.
	    edgehinear(ixy)=nxyzin(ixy)-1.
	    do iedge=1,nedge(ixy)
	      if(izpclist(ipiecelower(iedge,ixy)).eq.izsect)then
		if (needbyteswap.eq.0)then
		  read(iunedge(ixy),rec=1+iedge)nxgr,nygr,(igridstr(i),
     &		      iofset(i),i=1,2)
		else
		  read(iunedge(ixy),rec=1+iedge)nxgr,nygr
		  call convert_longs(nxgr,1)
		  call convert_longs(nygr,1)
		  read(iunedge(ixy),rec=1+iedge)idum1,idum2,
     &		      (igridstr(i), iofset(i),i=1,2)
		  call convert_longs(igridstr,2)
		  call convert_longs(iofset,2)
		endif
		edgehinear(ixy)=min(edgehinear(ixy),
     &		    float(igridstr(ixy)))
		edgelonear(ixy)=max(edgelonear(ixy),float
     &		    ((min(nxgr,nygr)-1)*intgrid(1)+iofset(ixy)+10))
	      endif
	    enddo
	  enddo
c	    
c	    Now analyze for h transforms if need to shift each piece - set
c	    multng because it is a flag that hinv exists and needs to be used
c	    
	  if(shifteach)then
	    multng=.true.
	    do ixy=1,2
	      do iedge=1,nedge(ixy)
		if(izpclist(ipiecelower(iedge,ixy)).eq.izsect)then
c		    
c		    need displacements implied by edges, unless this is to be
c		    done by old cross-correlation only
c
		  if(.not.xclegacy)then
		    call edgeswap(iedge,ixy,inde)
c
c		      compute the mean current displacement of upper relative
c		      to lower implied by the d[xy]mean
c
		    sumx=0.
		    sumy=0.
		    do ix=1,nxgrbf(inde)
		      do iy=1,nygrbf(inde)
			sumx=sumx+dxgrbf(ix,iy,inde)
			sumy=sumy+dygrbf(ix,iy,inde)
		      enddo
		    enddo  
		    dxgridmean(iedge,ixy)=sumx/
     &			(nxgrbf(inde)*nygrbf(inde))
		    dygridmean(iedge,ixy)=sumy/
     &			(nxgrbf(inde)*nygrbf(inde))
c
c		      adjust these by the current displacements implied by
c		      starting and offset coordinates of the edge areas
c
		    if(ixy.eq.1)then
		      dxgridmean(iedge,ixy)=dxgridmean(iedge,ixy)+
     &			  ixofsbf(inde)-ixgrdstbf(inde)+nxin-nxoverlap
		      dygridmean(iedge,ixy)=dygridmean(iedge,ixy)+
     &			  iyofsbf(inde)-iygrdstbf(inde)
		    else
		      dxgridmean(iedge,ixy)=dxgridmean(iedge,ixy)+
     &			  ixofsbf(inde)-ixgrdstbf(inde)
		      dygridmean(iedge,ixy)=dygridmean(iedge,ixy)+
     &			  iyofsbf(inde)-iygrdstbf(inde)+nyin-nyoverlap
		    endif
		  endif
c
		  if(.not.fromedge.and..not.xcreadin.and.
     &		      .not.(ifsloppy.eq.1.and.ifoldedge.eq.0))then
c		      
c		      If the edges of the image are lousy, it's better to use
c		      correlation, so here is this option.  Compute the
c		      correlations unless doing this by edges only,
c		      if they aren't already available
c		      
		    call shuffler(ipiecelower(iedge,ixy),indlow)
		    call shuffler(ipieceupper(iedge,ixy),indup)
		    call xcorredge(array(indlow),array(indup),
     &			nxyzin, noverlap,ixy,xdisp,ydisp,xclegacy)
		    edgedispx(iedge,ixy)=xdisp
		    edgedispy(iedge,ixy)=ydisp
		  endif
c		  write(*,'(1x,a,2i4,a,2f8.2,a,2f8.2)')
c     &		      char(ixy+ichar('W'))//' edge, pieces'
c     &		      ,ipiecelower(iedge,ixy),ipieceupper(iedge,ixy),
c     &		      '  dxygridmean:',dxgridmean(iedge,ixy),
c     &		      dygridmean(iedge,ixy),'  xcorr:',-xdisp,-ydisp
c		    dxgridmean(iedge,ixy)=-xdisp
c		    dygridmean(iedge,ixy)=-ydisp
c		  endif
		endif
	      enddo
	    enddo
c	      
c	      DNM 8/18/02: pass array to find_best_shifts, but do not pass
c	      hinv since it is in common
c
	    if(.not.fromedge)then
	      call find_best_shifts(array,edgedispx,edgedispy,-1,izsect,h,
     &		  nbestedge,beforemean,beforemax,aftermean(1),aftermax(1))
	      indbest=1
	    endif
	    if(.not.xclegacy)then
	      call find_best_shifts(array,dxgridmean,dygridmean,1,izsect,h,
     &		  nbestedge,beforemean,beforemax,aftermean(2),aftermax(2))
	      indbest=2
	    endif
c	      
c	      if first one was better based upon mean, redo it and reset the
c	      index to 1
c
	    if(.not.(xclegacy.or.fromedge).and.(aftermean(1).lt.aftermean(2)))
     &		then
	      call find_best_shifts(array,edgedispx,edgedispy,-1,izsect,h,
     &		  nbestedge,beforemean,beforemax,aftermean(1),aftermax(1))
	      indbest=1
	    endif
	    write(*,'(i4,a,2f6.2,a,a,2f6.2)')nbestedge,
     &		' edges, mean, max error before:', beforemean,beforemax,
     &		', after from ', edgexcorrtext(indbest),
     &		aftermean(indbest),aftermax(indbest)
		
	  endif
c		
c	    if doing g transforms, get inverse and recenter it from center
c	    of output image to corner of image
c
	  if(dogxforms)then
	    if(skipxforms)then
	      indgl=izsect+1-minzpc
	    else
	      do ilis=1,nlistz
		if(listz(ilis).eq.izsect)indgl=ilis
	      enddo
	    endif
	    call xfcopy(gl(1,1,indgl),hit)
	    hit(1,3)=hit(1,3)+(1.-hit(1,1))*gxcen-hit(1,2)*gycen
	    hit(2,3)=hit(2,3)+(1.-hit(2,2))*gycen-hit(2,1)*gxcen
	    call xfinvert(hit,ginv)
	  endif
c	    
c	    if floating, need to get current input min and max
c	    To pad edges properly, need current mean: put it into dmean
c
	  call  crossvalue(xinlong,nxpieces,nypieces,nshort,nlong)
	  curinmax=-1.e10
	  curinmin=1.e10
	  cursum=0.
	  nsum=0.
	  do ilong=nlong,1,-1
	    do ishort=nshort,1,-1
	      call crossvalue(xinlong,ishort,ilong,ixfrm,iyfrm)
	      if(mappiece(ixfrm,iyfrm).gt.0)then
		call shuffler(mappiece(ixfrm,iyfrm),indbray)
		do iy=1,nyin
		  tsum=0.
		  do i=indbray+(iy-1)*nxin,indbray+iy*nxin-1
		    curinmin=min(curinmin,array(i))
		    curinmax=max(curinmax,array(i))
		    cursum=cursum+array(i)
		  enddo
		  cursum=cursum+tsum
		  nsum=nsum+nxin
		enddo
	      endif
	    enddo
	  enddo
	  dmean=cursum/nsum
	  if(iffloat.eq.0)then
	    curinmin=definmin
	    curinmax=definmax
	  endif
c	    
c	    now get output scaling factor and additive factor
c
	  pixscale=(outmax-outmin)/max(1.,curinmax-curinmin)
	  pixadd=outmin-pixscale*curinmin
c
c	    look through memory list and renumber them with priorities
c	    backwards from the first needed piece
c	    
	  newuse=jusecount-1
	  do ilong=1,nlong
	    do ishort=1,nshort
	      call crossvalue(xinlong,ishort,ilong,ixfrm,iyfrm)
	      if(mappiece(ixfrm,iyfrm).gt.0)then
		do i=1,limsec
		  if(izmemlist(i).eq.mappiece(ixfrm,iyfrm))then
		    lastused(i)=newuse
		    newuse=newuse-1
		  endif
		enddo
	      endif
	    enddo
	  enddo
c	    
c	    GET THE PIXEL OUT
c	    -  loop on output frames; within each frame loop on little boxes
c	    
	  call  crossvalue(xinlong,newxpieces,newypieces,nshort,nlong)
c	  
	  do ilong=1,nlong
	    do ishort=1,nshort
	      call crossvalue(xinlong,ishort,ilong,ixout,iyout)
c		write(*,'(a,2i4)')' composing frame at',ixout,iyout
	      newpcyll=newminypiece+(iyout-1)*(newyframe-newyoverlap)
	      newpcxll=newminxpiece+(ixout-1)*(newxframe-newxoverlap)
	      anypixels=.false.
	      anylinesout=.false.
	      tsum=0.
c		
c		do fast little boxes
c		
	      nxfast=(newxframe+(ifastsiz-1))/ifastsiz
	      nyfast=(newyframe+(ifastsiz-1))/ifastsiz
c		
c		loop on boxes, get lower & upper limits in each box
c		
	      do iyfast=1,nyfast
		indylo=newpcyll+(iyfast-1)*ifastsiz
		indyhi=min(indylo+ifastsiz,newpcyll+newyframe)-1
		nlinesout=indyhi+1-indylo
c		  
c		  fill array with dmean
c		  
		do i=1,nxout*nlinesout
		  brray(i)=dmean
		enddo
c		  
		do ixfast=1,nxfast
		  indxlo=newpcxll+(ixfast-1)*ifastsiz
		  indxhi=min(indxlo+ifastsiz,newpcxll+newxframe)-1
c		    
c		    check # of edges, and prime piece number, for each corner
c		    
		  dofast=.true.
		  inframe=.false.
		  inonepiece=0
		  do indy=indylo,indyhi,indyhi-indylo
		    do indx=indxlo,indxhi,indxhi-indxlo
		      call countedges(indx,indy,xg,yg)
		      if(numpieces.gt.0)then
			if(inonepiece.eq.0)inonepiece=inpiece(1)
			dofast=dofast.and. numpieces.eq.1 .and.
     &			    inpiece(1).eq.inonepiece
			do i=1,numpieces
			  inframe=inframe .or. (xinpiece(i).ge.0. .and.
     &			      xinpiece(i).le.nxin-1. .and. yinpiece(i)
     &			      .ge.0. .and. yinpiece(i).le.nyin-1.)
			enddo
		      endif
		    enddo
		  enddo
c		    
c		    ALL ON ONE PIECE: do a fast transform of whole box
c		    
		  if(dofast.and.inframe)then
		    call xfunit(fastf,1.)	!start with unit xform
c		      
c		      if doing g xforms, put operations into xform that will
c		      perform inverse of xform
c		      
		    if(dogxforms)then
		      call xfcopy(ginv,fastf)
		    endif
c		      
c		      shift coordinates down to be within piece
c		      
		    fastf(1,3)=fastf(1,3)-ixpclist(inonepiece)
		    fastf(2,3)=fastf(2,3)-iypclist(inonepiece)
c		      
c		      if doing h's, implement the h inverse
c		      
		    if(multng)then
		      call xfmult(fastf,hinv(1,1,inonepiece),fstmp)
		      call xfcopy(fstmp,fastf)
		    endif
c		      
c		      now add 1 to get array index
c		      
		    fastf(1,3)=fastf(1,3)+1.
		    fastf(2,3)=fastf(2,3)+1.
c		      
		    call shuffler(inonepiece,indbray)
		    call fastinterp(brray,nxout,nlinesout,
     &			array(indbray),nxin,nyin,indxlo,indxhi,indylo,
     &			indyhi,newpcxll,fastf,fastf(1,3),
     &			fastf(2,3),dmean)
		    anypixels=.true.
c		      
		  elseif(inframe)then
c		      
c		      in or near an edge: loop on each pixel
c		      
		    niter=10
		    linebase=1-newpcxll
		    do indy=indylo,indyhi
		      do indx=indxlo,indxhi
			call countedges(indx,indy,xg,yg)
c			  
c			  load the edges and compute edge fractions
c			  
			nedgesum=0
			do ixy=1,2
			  do ied=1,numedges(ixy)
			    iedge=inedge(ied,ixy)
			    call edgeswap(iedge,ixy,indedg)
			    indlower=inedlower(ied,ixy)
			    indedge4(ied,ixy)=indedg
			    if(ixy.eq.1) then
			      edgefrac4(ied,ixy)=0.5+(xinpiece(indlower)
     &				  -(ixgrdstbf(indedg)+ (nxgrbf(indedg)-1)
     &				  *intgrid(1)/2.))/
     &				  min(nxgrbf(indedg)*intgrid(1),iblend(1))
			    else
			      edgefrac4(ied,ixy)=0.5+(yinpiece(indlower)
     &				  -(iygrdstbf(indedg)+(nygrbf(indedg)-1)
     &				  *intgrid(1)/2.))/
     &				  min(nygrbf(indedg)*intgrid(1),iblend(2))
			    endif
			    active4(ied,ixy)=edgefrac4(ied,ixy).lt..999
     &				.and.edgefrac4(ied,ixy).gt..001
			    if(active4(ied,ixy)) nedgesum=nedgesum+1
			    if(edgefrac4(ied,ixy).lt.0.)edgefrac4(ied,ixy)=0.
			    if(edgefrac4(ied,ixy).gt.1.)edgefrac4(ied,ixy)=1.
			  enddo
			enddo
c			  
c			  get indices of pieces and edges: for now, numbers
c			  1 to 4 represent lower left, lower right, upper left,
c			  and upper right
c			  
			if(numpieces.gt.1)then
			  indp1=0		!index of piece 1, 2, 3, or 4
			  indp2=0		!if the point is in them
			  indp3=0
			  indp4=0
			  inde12=0		!index of edges between 1 and 2
			  inde13=0		!1 and 3, etc
			  inde34=0
			  inde24=0
			  f12=0.		!edge fractions betweem 1 and 2
			  f13=0.		!1 and 3, etc
			  f23=0.
			  f24=0.
			  er1=0.		!end fractions to right and top
			  et1=0.		!of piece 1, left and bottom of
			  el2=0.		!piece 3, etc
			  et2=0.
			  er3=0.
			  eb3=0.
			  el4=0.
			  eb4=0.
			  fx=0.			!the composite edge fractions
			  fy=0.			!in x and y directions
			  if(numpieces.gt.2)then
			    do ipc=1,numpieces
			      if(xinpiece(ipc).gt.nxin/2.and.
     &				  yinpiece(ipc).gt.nyin/2)indp1=ipc
			      if(xinpiece(ipc).gt.nxin/2.and.
     &				  yinpiece(ipc).lt.nyin/2)indp3=ipc
			      if(xinpiece(ipc).lt.nxin/2.and.
     &				  yinpiece(ipc).gt.nyin/2)indp2=ipc
			      if(xinpiece(ipc).lt.nxin/2.and.
     &				  yinpiece(ipc).lt.nyin/2)indp4=ipc
			    enddo
			    do ied=1,numedges(1)
			      if(inedlower(ied,1).eq.indp1)inde12=ied
			      if(inedlower(ied,1).eq.indp3)inde34=ied
			    enddo
			    do ied=1,numedges(2)
			      if(inedlower(ied,2).eq.indp1)inde13=ied
			      if(inedlower(ied,2).eq.indp2)inde24=ied
			    enddo
			    if(inde12.ne.0)f12=edgefrac4(inde12,1)
			    if(inde34.ne.0)f34=edgefrac4(inde34,1)
			    if(inde13.ne.0)f13=edgefrac4(inde13,2)
			    if(inde24.ne.0)f24=edgefrac4(inde24,2)

			  else
c			      
c			      two piece case - identify as upper or lower to
c			      simplify computation of end fractions
c			      
			    if(numedges(1).gt.0)then
			      fx=edgefrac4(1,1)
			      if(0.5*(yinpiece(1)+yinpiece(2)).gt.nyin/2)then
				inde12=1
				if(xinpiece(1).gt.xinpiece(2))then
				  indp1=1
				  indp2=2
				else
				  indp1=2
				  indp2=1
				endif
			      else
				inde34=1
				fy=1.
				if(xinpiece(1).gt.xinpiece(2))then
				  indp3=1
				  indp4=2
				else
				  indp3=2
				  indp4=1
				endif
			      endif
			    else
			      fy=edgefrac4(1,2)
			      if(0.5*(xinpiece(1)+xinpiece(2)).gt.nxin/2)then
				inde13=1
				if(yinpiece(1).gt.yinpiece(2))then
				  indp1=1
				  indp3=2
				else
				  indp1=2
				  indp3=1
				endif
			      else
				inde24=1
				fx=1.
				if(yinpiece(1).gt.yinpiece(2))then
				  indp2=1
				  indp4=2
				else
				  indp2=2
				  indp4=1
				endif
			      endif
			    endif
			  endif
c			    
c			    get distance to top, right, bottom, or left edges
c			    as needed for each piece, and compute end fractions
c			    
			  if(indp1.gt.0)then
			    dr1=nxin-1.-xinpiece(indp1)
			    dt1=nyin-1.-yinpiece(indp1)
			    er1=(dr1+1.)/iblend(1)
			    et1=dt1/iblend(2)
			  endif			
			  if(indp2.gt.0)then
			    dl2=xinpiece(indp2)
			    dt2=nyin-1.-yinpiece(indp2)
			    el2=(dl2+1.)/iblend(1)
			    et2=(dt2+1.)/iblend(2)
			  endif			
			  if(indp3.gt.0)then
			    dr3=nxin-1.-xinpiece(indp3)
			    db3=yinpiece(indp3)
			    er3=(dr3+1.)/iblend(1)
			    eb3=(db3+1.)/iblend(2)
			  endif			
			  if(indp4.gt.0)then
			    dl4=xinpiece(indp4)
			    db4=yinpiece(indp4)
			    el4=(dl4+1.)/iblend(1)
			    eb4=(db4+1.)/iblend(2)
			  endif
c			    
c			    If there are 4 pieces, fx and fy are weighted sum
c			    of the f's in the two overlapping edges.  The
c			    weights ex and ey are modified fractional distances
c			    across the overlap zone.  First get the distances
c			    to the borders of the overlap zone (dla, etc) and
c			    use them to get absolute fractional distances
c			    across zone, ax in the y direction and ay in the
c			    x direction.  They are used to make ex slide from
c			    being a distance across the lower overlap to being
c			    a distance across the upper overlap.  This gives
c			    continuity with the edge in 2 or 3-piece cases
c
			  if(numpieces.eq.4) then
			    dla=min(dl2,dl4)
			    dra=min(dr1,dr3)
			    dba=min(db3,db4)
			    dta=min(dt1,dt2)
			    ax=dba/(dta+dba)
			    ay=dla/(dra+dla)
			    ex=((1-ay)*db3+ay*db4)/
     &				((1-ay)*(dt1+db3)+ay*(dt2+db4))
			    ey=((1-ax)*dl2+ax*dl4)/
     &				((1-ax)*(dr1+dl2)+ax*(dr3+dl4))
			    fx=(1-ex)*f12+ex*f34
			    fy=(1-ey)*f13+ey*f24
			  elseif(numpieces.eq.3)then
c			      
c			      Three-piece case is simple, only two edges
c			      
			    fx=edgefrac4(1,1)
			    fy=edgefrac4(1,2)
			  endif
c			    
c			    weighting factors are a product of the two f's,
c			    attenuated if necessary by fractional distance to
c			    end of piece, then normalized to sum to 1.
c
			  wll=min(1-fx,et1)*min(1-fy,er1)
			  wlr=min(fx,et2)*min(1-fy,el2)
			  wul=min(1-fx,eb3)*min(fy,er3)
			  wur=min(fx,eb4)*min(fy,el4)
			  wsum=wll+wlr+wul+wur
			  if(wsum.gt.0.)then
			    wll=wll/wsum
			    wlr=wlr/wsum
			    wul=wul/wsum
			    wur=wur/wsum
			  endif
c			    
c			    count up active pieces implied by the w's
c
			  nactivep=0
			  if(wll.gt.0.)nactivep=nactivep+1
			  if(wlr.gt.0.)nactivep=nactivep+1
			  if(wul.gt.0.)nactivep=nactivep+1
			  if(wur.gt.0.)nactivep=nactivep+1
c			    
c			    filter out cross-corner 2-piece cases, pick the
c			    piece where the point is most interior
c			    
			  if(nactivep.eq.2.and.wll*wur.gt.0.)then
			    if(min(dt1,dr1).lt.min(db4,dl4))then
			      wll=0.
			    else
			      wur=0.
			    endif
			    nactivep=1
			  elseif(nactivep.eq.2.and.wul*wlr.gt.0.)then
			    if(min(dt2,dl2).lt.min(db3,dr3))then
			      wlr=0.
			    else
			      wul=0.
			    endif
			    nactivep=1
			  endif			    
			else
c			    
c			    the one-piece case, avoid all that computation
c			    
			  nactivep=1
			  indp1=1
			  wll=1.
			endif
c			  
c			  NOW SORT OUT THE CASES OF 1, 2, 3 or 4 PIECES
c			  
			if(nactivep.le.1)then	!ONE PIECE
			  if(wll.gt.0.)then
			    indpidef=indp1
			  elseif(wlr.gt.0.)then
			    indpidef=indp2
			  elseif(wul.gt.0.)then
			    indpidef=indp3
			  else
			    indpidef=indp4
			  endif
			  if(nactivep.eq.0)indpidef=1
c			    
			  call shuffler(inpiece(indpidef),indbray)
			  pixval=oneintrp(array(indbray),nxin,nyin,
     &			      xinpiece(indpidef),yinpiece(indpidef),
     &			      dmean)
c			    
c			    ONE EDGE, TWO PIECES
c			    
			elseif(nactivep.eq.2)then
c			    
c			    find the pieces around the edge, set edge index
c			    
			  if(wll.gt.0..and.wlr.gt.0.)then
			    indpl=indp1
			    indpu=indp2
			    w1=wll
			    indedg=indedge4(inde12,1)
			  elseif(wul.gt.0..and.wur.gt.0.)then
			    indpl=indp3
			    indpu=indp4
			    w1=wul
			    indedg=indedge4(inde34,1)
			  elseif(wll.gt.0..and.wul.gt.0.)then
			    indpl=indp1
			    indpu=indp3
			    w1=wll
			    indedg=indedge4(inde13,2)
			  else
			    indpl=indp2
			    indpu=indp4
			    w1=wlr
			    indedg=indedge4(inde24,2)
			  endif
c			    
c			    set up pieces #'s, and starting coords
c			    
			  ipiece1=inpiece(indpl)
			  ipiece2=inpiece(indpu)
			  x1=xinpiece(indpl)
			  y1=yinpiece(indpl)
			  w2=1.-w1
c			    
c			    set up to solve equation for (x1,y1) and (x2,y2)
c			    given their difference and the desired xg,yg
c			    
			  xgconst=xg-w1*ixpclist(ipiece1)
     &			      -w2*ixpclist(ipiece2)
			  ygconst=yg-w1*iypclist(ipiece1)
     &			      -w2*iypclist(ipiece2)
			  if(multng)then
			    xgconst=xgconst-w1*h(1,3,ipiece1)
     &				-w2*h(1,3,ipiece2)
			    ygconst=ygconst-w1*h(2,3,ipiece1)
     &				-w2*h(2,3,ipiece2)
			    c11=w1*h(1,1,ipiece1)
     &				+w2*h(1,1,ipiece2)
			    c12=w1*h(1,2,ipiece1)
     &				+w2*h(1,2,ipiece2)
			    c21=w1*h(2,1,ipiece1)
     &				+w2*h(2,1,ipiece2)
			    c22=w1*h(2,2,ipiece1)
     &				+w2*h(2,2,ipiece2)
			    denom=c11*c22-c12*c21
			    fb11=w2*h(1,1,ipiece2)
			    fb12=w2*h(1,2,ipiece2)
			    fb21=w2*h(2,1,ipiece2)
			    fb22=w2*h(2,2,ipiece2)
			  endif
c			    
c			    in this loop, use the difference between (x1,y1)
c			    and (x2,y2) at the one place to solve for values of
c			    those coords; iterate until stabilize
c			    
			  x1last=-100.
			  y1last=-100.
			  iter=1
			  do while(iter.le.niter.and.(abs(x1-x1last)
     &			      .gt.0.01 .or.abs(y1-y1last).gt.0.01))
			    call dxydgrinterp(x1,y1,indedg,x2,y2,dden)
			    x1last=x1
			    y1last=y1
			    dx2=x2-x1
			    dy2=y2-y1
			    if(multng)then
			      bx=xgconst-fb11*dx2-fb12*dy2
			      by=ygconst-fb21*dx2-fb22*dy2
			      x1=(bx*c22-by*c12)/denom
			      y1=(by*c11-bx*c21)/denom
			    else
			      x1=xgconst-w2*dx2
			      y1=ygconst-w2*dy2
			    endif
			    x2=x1+dx2
			    y2=y1+dy2
			    iter=iter+1
			  enddo
c			    
c			    get the pixel from the piece with the bigger weight
c			    and adjust density by the difference across edge
c			    Except average them within 4 pixels of center!
c			    
			  if(abs(w1-w2)*max(iblend(1),iblend(2)).lt.2.5)then
			    call shuffler(ipiece1,indbray1)
			    call shuffler(ipiece2,indbray2)
			    pixval=w1*oneintrp(array(indbray1),nxin,
     &				nyin, x1,y1,dmean) +
     &				w2*oneintrp(array(indbray2),nxin,
     &				nyin, x2,y2,dmean)
			  elseif(w1.gt.w2)then
			    call shuffler(ipiece1,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x1,y1,dmean) + w2*dden
			  else
			    call shuffler(ipiece2,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x2,y2,dmean) - w1*dden
			  endif
c			    
c			    THREE PIECES AND TWO EDGES
c			    
			elseif(nactivep.eq.3)then
c			    
c			    now decide between the three cases of divergent,
c			    serial, or convergent functions, and assign pieces
c			    and weights accordingly
c			    
c			    DIVERGENT: if lower piece is same for x and y edge
c			    
			  if(wur.le.0.)then
			    w1=wll
			    w2=wlr
			    w3=wul
			    jndp1=indp1
			    jndp2=indp2
			    jndp3=indp3
			    ind12edg=indedge4(inde12,1)
			    ind13edg=indedge4(inde13,2)
			    icortyp=1
c			      
c			      CONVERGENT: if upper piece same for x and y edge
c			      
			  elseif(wll.le.0.)then
			    w3=wur
			    w1=wul
			    w2=wlr
			    jndp1=indp3
			    jndp2=indp2
			    jndp3=indp4
			    ind13edg=indedge4(inde34,1)
			    ind23edg=indedge4(inde24,2)
			    icortyp=2
c			      
c			      SERIAL: lower of one is the upper of the other
c			      
			  else
			    w1=wll
			    w3=wur
			    jndp1=indp1
			    jndp3=indp4
			    if(wlr.le.0.)then
			      w2=wul
			      jndp2=indp3
			      ind12edg=indedge4(inde13,2)
			      ind23edg=indedge4(inde34,1)
			    else
			      w2=wlr
			      jndp2=indp2
			      ind12edg=indedge4(inde12,1)
			      ind23edg=indedge4(inde24,2)
			    endif
			    icortyp=3
			  endif
			  
			  ipiece1=inpiece(jndp1)
			  ipiece2=inpiece(jndp2)
			  ipiece3=inpiece(jndp3)
			  x1=xinpiece(jndp1)
			  y1=yinpiece(jndp1)
			  wmax=max(w1,w2,w3)
c			    
c			    set up to solve equations for new (x1,y1), (x2,y2)
c			    and (x3,y3) given the differences between them and
c			    the desired weighted coordinate (xg,yg)
c			    
			  xgconst=xg-w1*ixpclist(ipiece1)-
     &			      w2*ixpclist(ipiece2)-w3*ixpclist(ipiece3)
			  ygconst=yg-w1*iypclist(ipiece1)-
     &			      w2*iypclist(ipiece2)-w3*iypclist(ipiece3)
			  if(multng)then
			    xgconst=xgconst-w1*h(1,3,ipiece1)
     &				-w2*h(1,3,ipiece2)-w3*h(1,3,ipiece3)
			    ygconst=ygconst-w1*h(2,3,ipiece1)
     &				-w2*h(2,3,ipiece2)-w3*h(2,3,ipiece3)
			    c11=w1*h(1,1,ipiece1)+w2*
     &				h(1,1,ipiece2)+w3*h(1,1,ipiece3)
			    c12=w1*h(1,2,ipiece1)+w2*
     &				h(1,2,ipiece2)+w3*h(1,2,ipiece3)
			    c21=w1*h(2,1,ipiece1)+w2*
     &				h(2,1,ipiece2)+w3*h(2,1,ipiece3)
			    c22=w1*h(2,2,ipiece1)+w2*
     &				h(2,2,ipiece2)+w3*h(2,2,ipiece3)
			    denom=c11*c22-c12*c21
			    f2b11=w2*h(1,1,ipiece2)
			    f2b12=w2*h(1,2,ipiece2)
			    f2b21=w2*h(2,1,ipiece2)
			    f2b22=w2*h(2,2,ipiece2)
			    f3b11=w3*h(1,1,ipiece3)
			    f3b12=w3*h(1,2,ipiece3)
			    f3b21=w3*h(2,1,ipiece3)
			    f3b22=w3*h(2,2,ipiece3)
			  endif
c			    
c			    do iteration, starting with coordinates and solving
c			    for new coordinates until convergence
c			    
			  x1last=-100.
			  y1last=-100.
			  iter=1
			  do while(iter.le.niter.and.(abs(x1-x1last)
     &			      .gt.0.01 .or.abs(y1-y1last).gt.0.01))
			    if(icortyp.eq.1)then
c				
c				divergent case
c				
			      call dxydgrinterp(x1,y1,ind12edg,x2,y2
     &				  ,dden12)
			      call dxydgrinterp(x1,y1,ind13edg,x3,y3
     &				  ,dden13)
			    elseif(icortyp.eq.2)then
c				
c				convergent case
c				
			      call dxydgrinterp(x1,y1,ind13edg,x3,y3
     &				  ,dden13)
c				
			      if(iter.eq.1)then
				x2=x3+ixgrdstbf(ind23edg)
     &				    -ixofsbf(ind23edg)
				y2=y3+iygrdstbf(ind23edg)
     &				    -iyofsbf(ind23edg)
			      endif
			      call dxydgrinterp(x2,y2,ind23edg,x3t,y3t
     &				  ,dden23)
			      x2=x2+x3-x3t
			      y2=y2+y3-y3t
			    else
c				
c				serial case
c				
			      call dxydgrinterp(x1,y1,ind12edg,x2,y2
     &				  ,dden12)
			      call dxydgrinterp(x2,y2,ind23edg,x3,y3
     &				  ,dden23)
			    endif
c			      
c			      solve equations for new coordinates
c			      
			    x1last=x1
			    y1last=y1
			    dx2=x2-x1
			    dy2=y2-y1
			    dx3=x3-x1
			    dy3=y3-y1
			    if(multng)then
			      bx=xgconst-f2b11*dx2-f2b12*dy2
     &				  -f3b11*dx3-f3b12*dy3
			      by=ygconst-f2b21*dx2-f2b22*dy2
     &				  -f3b21*dx3-f3b22*dy3
			      x1=(bx*c22-by*c12)/denom
			      y1=(by*c11-bx*c21)/denom
			    else
			      x1=xgconst-w2*dx2-w3*dx3
			      y1=ygconst-w2*dy2-w3*dy3
			    endif
			    x2=x1+dx2
			    y2=y1+dy2
			    x3=x1+dx3
			    y3=y1+dy3
			    iter=iter+1
			  enddo
c			    
c			    take pixel from the piece with the highest weight
c			    
			  if(w1.eq.wmax)then
			    call shuffler(ipiece1,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x1,y1,dmean)
			  elseif(w2.eq.wmax)then
			    call shuffler(ipiece2,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x2,y2,dmean)
			  else
			    call shuffler(ipiece3,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x3,y3,dmean)
			  endif
c			    
c			    Adjust for differences in mean density: divergent
c			    
			  if(icortyp.eq.1)then
			    if(w1.eq.wmax)then
			      pixval=pixval + w2*dden12 + w3*dden13
			    elseif(w2.eq.wmax)then
			      pixval=pixval+(w2-1.)*dden12+w3*dden13
			    else
			      pixval=pixval+w2*dden12+(w3-1.)*dden13
			    endif
c			      
c			      convergent
c			      
			  elseif(icortyp.eq.2)then
			    if(w1.eq.wmax)then
			      pixval=pixval-(w1-1.)*dden13-w2*dden23
			    elseif(w2.eq.wmax)then
			      pixval=pixval-w1*dden13-(w2-1.)*dden23
			    else
			      pixval=pixval-w1*dden13 -w2*dden23
			    endif
c			      
c			      serial
c			      
			  else
c			      
			    if(w1.eq.wmax)then
			      pixval=pixval-(w1-1.)*dden12+w3*dden23
			    elseif(w2.eq.wmax)then
			      pixval=pixval-w1*dden12+w3*dden23
			    else
			      pixval=pixval-w1*dden12+(w3-1.)*dden23
			    endif
			  endif
			else
c			    
c			    FOUR PIECES, THREE EDGES USED FOR SOLUTION
c			    
c			    First, need to have only 3 active edges, so if
c			    there are four, knock one out based on ex and ey
c			    
			  if(nedgesum.eq.4)then
			    emin=min(ex,1.-ex,ey,1.-ey)
			    if(ex.eq.emin)then
			      active4(inde34,1)=.false.
			    elseif(1.-ex.eq.emin)then
			      active4(inde12,1)=.false.
			    elseif(ey.eq.emin)then
			      active4(inde24,2)=.false.
			    else
			      active4(inde13,2)=.false.
			    endif
			  endif
c			    
c			    here there is always a serial chain from ll to ur,
c			    through either ul or lr, and in each case the
c			    fourth piece is either divergent from the first or
c			    converging on the fourth
c			    
			  w1=wll
			  w3=wur
			  if(.not.active4(inde12,1).or.
     &			      .not.active4(inde24,2))then
			    w2=wul
			    w4=wlr
			    jndp2=indp3
			    jndp4=indp2
			    ind12edg=indedge4(inde13,2)
			    ind23edg=indedge4(inde34,1)
			    if(.not.active4(inde24,2))then
			      ind14edg=indedge4(inde12,1)
			      icortyp=1		!divergent
			    else
			      ind43edg=indedge4(inde24,2)
			      icortyp=2		!convergent
			    endif
			  else
			    w2=wlr
			    w4=wul
			    jndp2=indp2
			    jndp4=indp3
			    ind12edg=indedge4(inde12,1)
			    ind23edg=indedge4(inde24,2)
			    if(.not.active4(inde34,1))then
			      ind14edg=indedge4(inde13,2)
			      icortyp=1
			    else
			      ind43edg=indedge4(inde34,1)
			      icortyp=2
			    endif
			  endif
c			    
			  ipiece1=inpiece(indp1)
			  ipiece2=inpiece(jndp2)
			  ipiece3=inpiece(indp4)
			  ipiece4=inpiece(jndp4)
			  x1=xinpiece(indp1)
			  y1=yinpiece(indp1)
			  wmax=max(w1,w2,w3,w4)
c			    
c			    set up to solve equations for new (x1,y1), (x2,y2)
c			    (x3,y3), and (x4,y4) given the differences between
c			    them and the desired weighted coordinate (xg,yg)
c			    
			  xgconst=xg-w1*ixpclist(ipiece1)-w2*ixpclist(ipiece2)
     &			      -w3*ixpclist(ipiece3)-w4*ixpclist(ipiece4)
			  ygconst=yg-w1*iypclist(ipiece1)-w2*iypclist(ipiece2)
     &			      -w3*iypclist(ipiece3)-w4*iypclist(ipiece4)
			  if(multng)then
			    xgconst=xgconst-w1*h(1,3,ipiece1)-w2*h(1,3,ipiece2)
     &				-w3*h(1,3,ipiece3)-w4*h(1,3,ipiece4)
			    ygconst=ygconst-w1*h(2,3,ipiece1)-w2*h(2,3,ipiece2)
     &				-w3*h(2,3,ipiece3)-w4*h(2,3,ipiece4)
			    c11=w1*h(1,1,ipiece1)+w2*h(1,1,ipiece2)
     &				+w3*h(1,1,ipiece3)+w4*h(1,1,ipiece4)
			    c12=w1*h(1,2,ipiece1)+w2*h(1,2,ipiece2)
     &				+w3*h(1,2,ipiece3)+w4*h(1,2,ipiece4)
			    c21=w1*h(2,1,ipiece1)+w2*h(2,1,ipiece2)
     &				+w3*h(2,1,ipiece3)+w4*h(2,1,ipiece4)
			    c22=w1*h(2,2,ipiece1)+w2*h(2,2,ipiece2)
     &				+w3*h(2,2,ipiece3)+w4*h(2,2,ipiece4)
			    denom=c11*c22-c12*c21
			    f2b11=w2*h(1,1,ipiece2)
			    f2b12=w2*h(1,2,ipiece2)
			    f2b21=w2*h(2,1,ipiece2)
			    f2b22=w2*h(2,2,ipiece2)
			    f3b11=w3*h(1,1,ipiece3)
			    f3b12=w3*h(1,2,ipiece3)
			    f3b21=w3*h(2,1,ipiece3)
			    f3b22=w3*h(2,2,ipiece3)
			    f4b11=w4*h(1,1,ipiece4)
			    f4b12=w4*h(1,2,ipiece4)
			    f4b21=w4*h(2,1,ipiece4)
			    f4b22=w4*h(2,2,ipiece4)
			  endif
c			    
c			    do iteration, starting with coordinates and solving
c			    for new coordinates until convergence
c			    
			  x1last=-100.
			  y1last=-100.
			  iter=1
			  do while(iter.le.niter.and.(abs(x1-x1last)
     &			      .gt.0.01 .or.abs(y1-y1last).gt.0.01))
			    call dxydgrinterp(x1,y1,ind12edg,x2,y2
     &				,dden12)
			    call dxydgrinterp(x2,y2,ind23edg,x3,y3
     &				,dden23)
			    if(icortyp.eq.1)then
c				
c				divergent case
c				
			      call dxydgrinterp(x1,y1,ind14edg,x4,y4
     &				  ,dden14)
			    else
c				
c				convergent case
c				
			      if(iter.eq.1)then
				x4=x3+ixgrdstbf(ind43edg)
     &				    -ixofsbf(ind43edg)
				y4=y3+iygrdstbf(ind43edg)
     &				    -iyofsbf(ind43edg)
			      endif
			      call dxydgrinterp(x4,y4,ind43edg,x3t,y3t
     &				  ,dden43)
			      x4=x4+x3-x3t
			      y4=y4+y3-y3t
			    endif
c			      
c			      solve equations for new coordinates
c			      
			    x1last=x1
			    y1last=y1
			    dx2=x2-x1
			    dy2=y2-y1
			    dx3=x3-x1
			    dy3=y3-y1
			    dx4=x4-x1
			    dy4=y4-y1
			    if(multng)then
			      bx=xgconst-f2b11*dx2-f2b12*dy2
     &				  -f3b11*dx3-f3b12*dy3-f4b11*dx4-f4b12*dy4
			      by=ygconst-f2b21*dx2-f2b22*dy2
     &				  -f3b21*dx3-f3b22*dy3-f4b21*dx4-f4b22*dy4
			      x1=(bx*c22-by*c12)/denom
			      y1=(by*c11-bx*c21)/denom
			    else
			      x1=xgconst-w2*dx2-w3*dx3-w4*dx4
			      y1=ygconst-w2*dy2-w3*dy3-w4*dy4
			    endif
			    x2=x1+dx2
			    y2=y1+dy2
			    x3=x1+dx3
			    y3=y1+dy3
			    x4=x1+dx4
			    y4=y1+dy4
			    iter=iter+1
			  enddo
c			    
c			    take pixel from the piece with the highest weight
c			    and adjust density appropriately for case
c			    
			  if(w1.eq.wmax)then
			    call shuffler(ipiece1,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x1,y1,dmean)
			    if(icortyp.eq.1)then
			      pixval=pixval+(w2+w3)*dden12+w3*dden23+w4*dden14
			    else
			      pixval=pixval+(1.-w1)*dden12+(w3+w4)*dden23
     &				  -w4*dden43
			    endif
			  elseif(w2.eq.wmax)then
			    call shuffler(ipiece2,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x2,y2,dmean)
			    if(icortyp.eq.1)then
			      pixval=pixval+(w2+w3-1.)*dden12+w3*dden23+
     &				  w4*dden14
			    else
			      pixval=pixval-w1*dden12+(w3+w4)*dden23
     &				  -w4*dden43
			    endif
			  elseif(w3.eq.wmax)then
			    call shuffler(ipiece3,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x3,y3,dmean)
			    if(icortyp.eq.1)then
			      pixval=pixval+(w2+w3-1.)*dden12+(w3-1.)*dden23+
     &				  w4*dden14
			    else
			      pixval=pixval-w1*dden12+(w3+w4-1.)*dden23
     &				  -w4*dden43
			    endif
			  else
			    call shuffler(ipiece4,indbray)
			    pixval=oneintrp(array(indbray),nxin,nyin,
     &				x4,y4,dmean)
			    if(icortyp.eq.1)then
			      pixval=pixval+(w2+w3)*dden12+w3*dden23
     &				  +(w4-1.)*dden14
			    else
			      pixval=pixval-w1*dden12+(w3+w4-1.)*dden23
     &				  -(w4-1.)*dden43
			    endif
			  endif
			endif
c			  
c			  stick limited pixval into array and mark as output
c			  
			brray(linebase+indx)=
     &			    max(curinmin,min(curinmax,pixval))
			anypixels=.true.
		      enddo
		      linebase=linebase+nxout
		    enddo
		  endif
		enddo
c		  
c		  if any pixels have been present in this frame, write line out
c		  
		if(anypixels)then
		  tmpsum=0.
		  do i=1,nxout*nlinesout
		    val=pixscale*brray(i)+pixadd
		    brray(i)=val
		    dminout=min(dminout,val)
		    dmaxout=max(dmaxout,val)
		    tmpsum=tmpsum+val
		  enddo
		  tsum=tsum+tmpsum
		  ilineout=(iyfast-1)*ifastsiz
		  call imposn(2,nzout,ilineout)
		  call iwrsecl(2,brray,nlinesout)
c		    
c		    if this is the first time anything is written, and it
c		    wasn't the first set of lines, then need to go back and
c		    fill the lower part of frame with mean values
c		    
		  if(.not.anylinesout.and.iyfast.gt.1)then
		    val=dmean*pixscale+pixadd
		    do i=1,nxout*ifastsiz
		      brray(i)=val
		    enddo
		    call imposn(2,nzout,0)
		    do ifill=1,iyfast-1
		      call iwrsecl(2,brray,ifastsiz)
		    enddo
		    tsum=tsum+val*nxout*(iyfast-1)*ifastsiz
		  endif
		  anylinesout=.true.
		endif
	      enddo
c		
c		if any pixels present, write piece coordinates
c		
	      if(anypixels)then
		grandsum=grandsum+tsum
c		  write(*,'(a,i5)')' wrote new frame #',nzout
		nzout=nzout+1
c		  
c		  keep header up to date in case of crashes
c		  
		call ialsiz(2,nxyzout,nxyzst)
		call ialsam(2,nxyzout)
		cell(3)=nzout
		call ialcel(2,cell)
		tmean=grandsum/(nzout*nxout*nyout)
		call iwrhdr(2,title,-1,dminout,dmaxout, tmean)
		if (outputpl) write(3,'(2i6,i4)')newpcxll,newpcyll,izsect
	      endif
c		
	    enddo
	  enddo
92	enddo
c
	close(3)
	call imclose(2)
c	  
c	  write edge correlations if they were not read in and if they were
c	  computed in their entirety
c
	if(.not.xcreadin.and.((ifsloppy.eq.1.and.shifteach).or.
     &	    (ifsloppy.eq.0.and.shifteach.and..not.fromedge)))then
	  edgenam=concat(filnam,xcorrext)
	  call dopen(4,edgenam,'new','f')
	  write(4,'(2i7)')nedge(1),nedge(2)
	  write(4,'(f8.3,f9.3)')((edgedispx(i,ixy),edgedispy(i,ixy),
     &	      i=1,nedge(ixy)),ixy=1,2)
	  close(4)
	endif
	call exit(0)
98	call errorexit ('reading transforms')
	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: BLENDMONT - ',message
	call exit(1)
	end

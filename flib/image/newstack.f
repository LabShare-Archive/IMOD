*************NEWSTACK.FOR**********************************************
*   
*   Newstack is a general stack editor to move images into, out of, or between
*   stacks.  It can float the images to a common range or mean of density,
*   and truncate a selected fraction of the density range to zero.
*   It can apply a general linear transform specified as a line in a file.
*   It can put the output into a smaller or larger array and independently
*   recenter each image separately from the transform.  Images can be taken
*   from multiple input files and placed into multiple output files.
*   
*   All inputs are prompted interactively.  The input is as follows; lines
*   starting with IF are entered only if a particular option is selected.
*
*   Number of input files, or -1 to enter list of files from a file
*
*   IF you enter a number >0, then for each input file, enter two lines:
*   .	File name
*   .	list of section numbers to take from that file.  The first section is
*   .		numbered 0 and ranges may be entered (e.g. 0-4,5,7-9)
*
*   OTHERWISE, IF you enter -1, then enter the name of the file with list of
*   .		files and sections.  The format of this file should be
*      Number of input files
*      File name
*      list of section numbers (first is 0!!!), ranges allowed (e.g. 0-3,5,7-9)
*      next file and list of sections, etc.
*
*   Number of output files, or -1 to enter list of files from a file
*
*   IF you enter 1, next enter the name of the single output file.
*
*   OR IF you enter a number >1, then for each output file, enter two lines:
*   .	File name
*   .	Number of sections to put in that file
*
*   OR IF you enter -1, then enter the name of the file with the list of files
*   .	and numbers of of sections.  The format of this file is:
*      Number of output files
*      File name
*      number of sections to place in that file
*      next file and number of sections, etc.
*
*   The X and Y dimensions of the output file, or / to take the same
*   .	dimensions as the first input file
*
*   The data mode of the output file, or / to take the same mode as the first
*   .	input file.  Mode can be 0 for bytes, 1 for 16-bit integers, 2 for 
*   .   32-bit real numbers, or 9 to 15 for 9 to 15 bit values.
*
*   0 for no offsets, or 1 to apply X and Y offsets separately to each section,
*   .   or -1 to apply the same X and Y offset to all sections
*
*   IF offsets are selected, next specify the X and Y center offsets of
*      each of the sections (they need not be on separate lines), or the single
*      offset to apply to all sections.
*      Hint: offsets of 50,50 will take the center of the output image
*      from the upper right quadrant of the input image.
*
*   0 for no transforms, or 1 to transform images using cubic interpolation, or
*      2 to transform using bilinear interpolation
*
*   IF transforming is selected, next specify the file for transforms, and
*      on a separate line, a list of lines to use in the file (first is 0!!!).
*      Ranges are allowed here, but all must be on one line.  To have a single
*      transform applied to all of the sections, just enter a single number.
*
*   0 for no floating of densities, 1 to float densities so that each section
*	occupies the same RANGE of densities (the optimum range for the
* 	particular data mode), 2 to float so that all sections have the same
*	MEAN and STANDARD DEVIATION of density (which requires the input
*	sections to be read twice), -1 to specify a single scaling that will
c	be applied equally to all sections, -2 to scale all sections by the
c	same amount, determined by black and white contrast settings, in order
c	to convert to bytes with stretched contrast, 3 to SHIFT sections to
c	the same mean value without rescaling, or 4 to SHIFT sections to the 
c	same mean then rescale all sections by the same specified scaling.
*	  
*   IF single rescaling is selected with "-1", next specify the density values
*	to which the minimum and maximum in the data file should be mapped, or
*	just enter "/" to have them mapped to the full-scale range for the
*	data mode, or enter "1,1" to override any scaling of data that would
*	otherwise occur because of a change of mode.  This scaling method can
*	be used to invert densities by entering, e.g., 255,0.
*
c   IF scaling by black and white contrast settings is selected with -2, next
c	enter the black and white settings (see mrcbyte(1)).
c
*   OR, IF shifting and rescaling was selected with "4", next specify the
*	density values to which the new, shifted minimum and maximum values
*	should be mapped, or just enter "/" to have them mapped to the full-
*	scale range for the data mode.
*
*   When there is a change of modes, data are rescaled optimally in most cases.
*	If floating is not selected, the scaling is by an amount that would
*	map the range for the input data mode into the range for the output
*	data mode (e.g., going from mode 1 to mode 2, it would scale data up
*       by a factor of 128).  Generally, all sections will be rescaled by
*	the same amount, thus preserving relative differences between sections.
*	However, if the output mode is 2 (reals), this
*	kind of rescaling will not occur.  In general, if your input mode is
*	real and you are outputing to bytes or integers, you need to specify
*	some kind of floating, shifting, or scaling in order to get usable
*	values.  These same kinds of automatic scalings upon change of mode
*	will occur with shifting to a common mean (option 3), but here the
*	program is more careful to apply the same scaling to all sections.
*	To disable the scaling upon change of mode, select the "-1" floating
*	option and enter 1,1.
*	  
*   At the end, the program will report how many pixels have been truncated
*	at the low or high limits of the output range.  If this happens after
*	shifting with a specified output range, try again with a slightly
*	smaller range.
*
*   Complex data (fourier transforms) can be stacked but not otherwise  
*   manipulated.  Don't try to change the size or center of the fft's.
*	  
*  The program operates on very large images by reading, operating on, and
*	  writing strips of the image if necessary.  Images can be larger than
*	  10K by 10K if they are not being rotated much, but when images are 
*	  rotated by large angles (30-90 degrees), input image size may be 
*	  limited to 4K by 4K.
*   
*   DNM (mast) for VAX	8/88
*	  5/4/89 changed to float to same mean and sd rather than mean and min.
*	  12/19/89 added full control of density scaling; fixed bug in
*	  interpolation that was generating half-pixel error half the time.
*	  4/19/90 added line-by-line prompts for file and section entry and
*	  added time/date stamp
*	  1/23/91: fixed so that it can correctly pad images and extract
*	  subsets of images with either center-shifting or transforms
*	  6/11/91: fixed bug if trying to output scaled real values les than 0
*	  11/16/94: added shifting to mean, rationalized scaling somewhat
*	  9/24/01: changed to read and write in chunks if images are big
*   
************************************************************************
*	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.9  2003/12/12 20:25:57  mast
c	  Initial checkin with PIP conversion and distortion correction
c	
c	  Revision 3.8  2003/03/14 01:57:05  mast
c	  Added a linear interpolation option
c	
c	  Revision 3.7  2002/08/18 23:12:47  mast
c	  Changed to call iclavgsd in library
c	
c	  Revision 3.6  2002/08/17 05:38:04  mast
c	  Standardized error outputs
c	
c	  Revision 3.5  2002/05/20 15:59:38  mast
c	  Changed all STOP statements to print the message then call exit(1)
c	
c	  Revision 3.4  2002/05/07 02:00:29  mast
c	  Eliminate output of mean and SD from a test print statement
c	
c	  Revision 3.3  2002/04/18 20:02:55  mast
c	  Made it transfer extra header data correctly if it consists of
c	  integers and reals
c	
c	  Revision 3.2  2002/04/18 20:00:34  mast
c	  wrong comment was here
c	
c	  Revision 3.1  2001/12/29 00:57:49  mast
c	  Added an entry to -1 floating option to disable all rescaling
c	
	implicit none
	integer idim,lmfil,lmsec,maxchunks,maxextra,lmGrid
	parameter (idim=19000000,lmfil=1000,lmsec=50000,maxchunks=20)
	parameter (maxextra=1000000)
	parameter (lmGrid = 200)
	integer*4 nx,ny,nz
	COMMON //NX,NY,NZ
c
c	  9/25/01: make the buffer be in common to avoid stack limit size on
c	  the SGI
	real*4 array(idim)
	common /bigarr/ARRAY
C   
	integer*4 NXYZ(3),MXYZ(3),NXYZST(3), NXYZ2(3),MXYZ2(3)
	real*4 CELL2(6),cell(6), TITLE(20)
C   
	CHARACTER*120 FILIN(lmfil),FILOUT(lmfil),xffil,filistin,filistout
	character*120 idfFile
	character*100000 listString
	equivalence (listString, array)
	EQUIVALENCE (NX,NXYZ)
C   
	DATA NXYZST/0,0,0/
	character*20 floatxt/' '/,xftext/' '/,trunctxt/' '/
	real*4 f(2,3,lmsec), frot(2,3), fexp(2,3), fprod(2,3)
	integer*4 inlist(lmsec),nlist(lmfil),listind(lmfil)
     &      ,nsecout(lmfil),lineuse(lmsec)
	real*4 xcen(lmsec),ycen(lmsec),optmax(16),secmean(lmsec)
	integer*4 lineOutSt(maxchunks+1),nLinesOut(maxchunks)
	integer*4 lineInSt(maxchunks+1),nLinesIn(maxchunks)
	byte extrain(maxextra),extraout(maxextra)
	data optmax/255.,32767.,7*255.,511.,1023.,2047.,4095.,8191.,
     &      16383.,32767./
c	  
	integer*4 ifDistort, idfBinning, iBinning, idfNx, idfNy
	integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
	real*4 xGridIntrv, yGridIntrv, pixelIdf
	real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
c
	logical rescale
	character dat*9,tim*8,tempext*9
	character*120 tempname,temp_filename
	logical nbytes_and_flags
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
	character*80 titlech
	integer*4 inunit,nfilein,listot,noutot,nfileout,nx3,ny3
	integer*4 newmode,ifoffset,ifxform,nxforms,nlineuse,ifmean,iffloat
	integer*4 nsum,ilis,ifil,nsecred,loadyst,loadynd,isec,isecout
	real*4 xofsall,yofsall,fraczero,dminspec,dmaxspec,conlo,conhi
	real*4 zmin,zmax,diffmin,diffmax,grandsum,sdsec
	real*4 grandmean,shiftmin,shiftmax,shiftmean,dminin,dmaxin,dmeanin
	integer*4 ifileout,ntrunclo,ntrunchi,ifheaderout,iftempopen,nbsymin
	integer*4 nbytexin,iflagxin,mode,nbytexout,nbsymout,indxout
	real*4 dmin,dmax,dmean,dmin2,dmax2,dmean2,optin,optout,bottomin
	real*4 bottomout,xci,yci,dx,dy,xp1,yp1,xp2,yp2,xp3,yp3,xp4,yp4
	integer*4 linesleft,nchunk,nextline,ichunk,ifOutChunk,iscan,iytest
	integer*4 iybase,iy1,iy2,lnu,maxin,ibbase
	real*4 dmeansec,dsum,dsumsq,tmpmin,tmpmax,tsum,val,tsum2,dminnew
	real*4 dmaxnew,zminsec,zmaxsec,tmpmean,tmpminshf,tmpmaxshf,sclfac
	integer*4 needyst,needynd,nmove,noff,nload,nyload,nych,npix,ibchunk
	integer*4 ixcen,iycen,ix1,ix2,istart,nbcopy,nbclear
	real*4 const,denoutmin,den, tmin2,tmax2,tmean2,tsumsq,avgsec
	integer*4 numInputFiles, numSecLists, numOutputFiles, numToGet
	integer*4 numOutValues, numOutEntries, ierr, ierr2, i, kti, iy
	integer*4 maxFieldY
	real*4 fieldMaxY, binRatio, rotateAngle, expandFactor
	real*4 cosd, sind
	integer*4 lnblnk
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger,PipGetBoolean, PipNumberOfEntries
	integer*4 PipGetString,PipGetTwoIntegers, PipGetFloatArray, PipGetFloat
	integer*4 PipGetIntegerArray, PipGetNonOptionArg, PipGetTwoFloats
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  newstack
c
	integer numOptions
	parameter (numOptions = 21)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'listin:ListOfInputs:FN:@listout:ListOfOutputs:FN:@'//
     &      'sections:SectionsToRead:LI:@numout:NumberToOutput:IA:@'//
     &      'size:SizeToOutputInXandY:IP:@mode:ModeToOutput:I:@'//
     &      'offset:OffsetsInXandY:FA:@xffile:TransformFile:FN:@'//
     &      'uselines:UseTransformLines:LI:@rotate:RotateByAngle:F:@'//
     &      'expand:ExpandByFactor:F:@linear:LinearInterpolation:B:@'//
     &      'float:FloatDensities:I:@contrast:ContrastBlackWhite:IP:@'//
     &      'scale:ScaleMinAndMax:FP:@distort:DistortionField:FN:@'//
     &      'binning:BinningOfImages:I:@param:ParameterFile:PF:@'//
     &      'help:usage:B:'
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'newstack',
     &	    'ERROR: NEWSTACK - ', .true., 2, 2, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0
c	  
c	  defaults
c	  
	filistin = ' '
	filistout = ' '
	numInputFiles = 0
	numOutputFiles = 0
	numSecLists = 0
	ifDistort = 0
	maxFieldY = 0
	idfFile = ' '
	rotateAngle = 0.
	expandFactor = 0.
C   
C	  Read in list of input files
C   
	call ialprt(.false.)
	inunit=5
c	  
c	  get number of input files
c
	if (pipinput) then
	  ierr = PipGetString('ListOfInputs', filistin)
	  ierr = PipNumberOfEntries('InputFile', numInputFiles)
	  nfilein = numInputFiles + max(0, numNonOptArg - 1)
	  if (nfilein .gt. 0 .and. filistin .ne. ' ') call errorexit(
     &	      'YOU CANNOT ENTER BOTH INPUT FILES AND AN INPUT LIST FILE')
	  if (filistin .ne. ' ')nfilein = -1
	  ierr = PipNumberOfEntries('SectionsToRead', numSecLists)
	else
	  write(*,'(1x,a,$)')'# of input files (or -1 to read list'//
     &	      ' of input files from file): '
	  read(5,*)nfilein
	endif
c	  
c	  if it is negative, open a list file, set up input from 7
c
	if (nfilein .eq. 0) call errorexit('NO INPUT FILE SPECIFIED')
	if(nfilein.lt.0)then
	  inunit=7
	  if (.not.pipinput) then
	    write(*,'(1x,a,$)')'Name of input list file: '
	    read(5,101)filistin
	  endif
	  call dopen(7,filistin,'ro','f')
	  read(inunit,*)nfilein
	endif
	listot=0
	if(nfilein.gt.lmfil)call errorexit(
     &	    'TOO MANY FILES FOR ARRAYS')
c	  
	do i=1,nfilein
c
c	    get the next filename
c
	  if (pipinput .and. inunit.ne.7) then
	    if (i .le. numInputFiles) then
	      ierr = PipGetString('InputFile', filin(i))
	    else
	      ierr = PipGetNonOptionArg(i - numInputFiles, filin(i))
	    endif
	  else
	    if(inunit.ne.7)then
	      if(nfilein.eq.1)then
		write(*,'(1x,a,$)')'Name of input file: '
	      else
		write(*,'(1x,a,i3,a,$)')'Name of input file #',i,': '
	      endif
	    endif
	    READ(inunit,101)FILIN(i)
	  endif
c	    
c	    open file to make sure it exists and get default section list
c
	  CALL IMOPEN(1,FILIN(i),'RO')
	  CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
	  call imclose(1)
	  nlist(i) = nz
	  do isec = 1, nz
	    inlist(min(listot + isec, lmsec)) = isec - 1
	  enddo
c	    
c	    get section list
c
	  if (.not.pipinput) then
	    if(inunit.ne.7)print *,'Enter list of sections to read from'
     &		//' file (/ for all, 1st sec is 0; ranges OK)'
	    call rdlist(inunit,inlist(listot+1),nlist(i))
	  elseif (i .le. numSecLists) then
	    ierr = PipGetString('SectionsToRead', listString)
	    call parselist(listString, inlist(listot+1),nlist(i))
	  endif
c	    
c	    check list legality
c
	  listind(i)=listot+1
	  do isec = listot + 1, listot + nlist(i)
	    if (inlist(isec) .lt. 0 .or. inlist(isec) .ge. nz) then
	      print *
	      print *,'ERROR: NEWSTACK -',inlist(isec),
     &		  ' IS AN ILLEGAL SECTION NUMBER FOR ',
     &		  filin(i)(1:lnblnk(filin(i)))
	      call exit(1)
	    endif
	  enddo
	  listot=listot+nlist(i)
	  if(listot.gt.lmsec)call errorexit(
     &	      'TOO MANY SECTIONS FOR ARRAYS')
	enddo
	close(7)
101	FORMAT(A)
C   
C	  Read in list of output files
C   
	inunit=5
	noutot=0
c	  
c	  get number of output files
c
	if (pipinput) then
	  ierr = PipGetString('ListOfOutputs', filistout)
	  ierr = PipNumberOfEntries('OutputFile', numOutputFiles)
	  nfileout = numOutputFiles + min(1, numNonOptArg)
	  if (nfileout .gt. 0 .and. filistout .ne. ' ') call errorexit(
     &	      'YOU CANNOT ENTER BOTH OUTPUT FILES AND AN OUTPUT'//
     &	      ' LIST FILE')
	else
	  write(*,'(1x,a,$)')'# of output files (or -1 to read list'//
     &	      ' of output files from file): '
	  read(5,*)nfileout
	endif
	if (nfileout .eq. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
	if(nfileout.gt.lmfil)call errorexit(
     &	    'TOO MANY OUTPUT FILES FOR ARRAYS')
c	  
c	  get list input
c
	if(nfileout.lt.0)then
	  inunit=7
	  if (.not.pipinput) then
	    write(*,'(1x,a,$)')'Name of output list file: '
	    read(5,101)filistout
	  endif
	  call dopen(7,filistout,'ro','f')
	  read(inunit,*)nfileout
	elseif(nfileout.eq.1 .and. .not.pipinput)then
c	    
c	    get single output file
c
	  write(*,'(1x,a,$)')'Name of output file: '
	  read(5,101)filout(1)
	  nsecout(1)=listot
	  noutot=listot
	endif
c	  
c	  or get all the output files and the number of sections
c
	if(noutot.eq.0)then
	  if (pipinput) then
	    if (nfileout .eq. 1) then
	      nsecout(1)=listot
	    elseif (nfileout .eq. listot) then
	      do i = 1, nfileout
		nsecout(i) = 1
	      enddo
	    else
	      ierr = PipNumberOfEntries('NumberToOutput', numOutEntries)
     		  if (numOutEntries .eq. 0)
     &		  call errorexit('YOU MUST SPECIFY NUMBER OF SECTIONS '//
     &		  'TO WRITE TO EACH OUTPUT FILE')
	      
	      numOutValues = 0
	      do i = 1, numOutEntries
		numToGet = 0
		ierr = PipGetIntegerArray('NumberToOutput',
     &		    nsecout(numOutValues + 1), numToGet, lmfil)
		numOutValues = numOutValues + numToGet
	      enddo
	      if (numOutValues .ne. nfileout) call errorexit(
     &		  'THE NUMBER OF VALUES FOR SECTIONS TO OUTPUT DOES'
     &		  //' NOT EQUAL THE NUMBER OF OUTPUT FILES')
	    endif
	    do i=1,nfileout
	      if (i .le. numOutputFiles) then
		ierr = PipGetString('OutputFile', filout(i))
	      else
		ierr = PipGetNonOptionArg(numNonOptArg, filout(i))
	      endif
	      noutot=noutot+nsecout(i)
	    enddo
	  else
	    do i=1,nfileout
	      if(inunit.ne.7)write(*,'(1x,a,i3,a,$)')
     &		  'Name of output file #',i,': '
	      READ(inunit,101)FILOUT(i)
	      if(inunit.ne.7)write(*,'(1x,a,$)')
     &		  'Number of sections to store in that file: '
	      read(inunit,*)nsecout(i)
	      noutot=noutot+nsecout(i)
	    enddo
	  endif
	endif
	if(noutot.ne.listot)call errorexit(
     &	      'Number of input and output sections does not match')
c	  
c	  get new size and mode and offsets
c	  
	nx3=-1
	ny3=-1
	newmode=-1
	xofsall=0.
	yofsall=0.
	ifoffset = 0
	if (pipinput) then
	  ierr = PipGetTwoIntegers('SizeToOutputInXandY', nx3, ny3)
	  ierr = PipGetInteger('ModeToOutput', newmode)
	  ierr = PipNumberOfEntries('OffsetsInXandY', numOutEntries)
	  if (numOutEntries .gt. 0) then
	    ifoffset = 1
	    numOutValues = 0
	    do i = 1, numOutEntries
	      numToGet = 0
	      ierr = PipGetFloatArray('OffsetsInXandY',
     &		  array(numOutValues + 1), numToGet, lmsec * 2)
	      numOutValues = numOutValues + numToGet
	    enddo
	    if (numOutValues .ne. 2 .and. numOutValues .ne. 2 * listot)
     &		call errorexit('THERE MUST BE EITHER ONE OFFSET OR AN'
     &		//' OFFSET FOR EACH SECTION')
	    do i = 1, numOutValues / 2
	      xcen(i) = array(2 * i - 1)
	      ycen(i) = array(2 * i)
	    enddo
	    if (numOutValues .eq. 2) ifoffset = -1
	    xofsall = xcen(1)
	    yofsall = ycen(1)
	  endif
	else
	  write(*,'(1x,a,$)')'Output file X and Y dimensions'//
     &	      ' (/ for same as first input file): '
	  read(5,*)nx3,ny3
	  write(*,'(1x,a,$)')'Output file data mode (/ for same'//
     &	      ' as first input file): '
	  read(5,*)newmode
c
c	    get list of x,y coordinate offsets
c	    
	  write(*,'(1x,a,/,a,$)')'1 to offset centers of individual '//
     &	      'images,','  -1 to apply same offset to all sections,'//
     &	      ' or 0 for no offsets: '
	  read(5,*)ifoffset
	  if(ifoffset.gt.0)then
	    print *,'Enter X and Y center offsets for each section'
	    read(5,*)(xcen(i),ycen(i),i=1,listot)
	  elseif(ifoffset.lt.0)then
	    write(*,'(1x,a,$)')
     &		'X and Y center offsets for all sections: '
	    read(5,*)xofsall,yofsall
	  endif
	ENDIF
c	  
c	  fill offset list if only one or none
c
	if (ifoffset .le. 0) then
	  do i=1,listot
	    xcen(i)=xofsall
	    ycen(i)=yofsall
	  enddo
	endif
C   
C	  Get list of transforms
C	  
	ifxform = 0
	if (pipinput) then
	  if (PipGetString('TransformFile', xffil) .eq. 0) then
	    ierr = PipGetBoolean('LinearInterpolation', ifxform)
	    ifxform = ifxform + 1
	  endif
	else
	  write(*,'(1x,a,$)')'1 or 2 to transform images with cubic or'
     &	      //' linear interpolation, 0 not to: '
	  read(5,*)ifxform
	  if(ifxform.ne.0)then
	    write(*,'(1x,a,$)')'Name of transform file: '
	    read(5,101)xffil
	  endif
	endif
	if(ifxform.ne.0)then
c	    
c	    read transforms, set default to section list
c
	  call dopen(3,xffil,'ro','f')
	  call xfrdall(3,f,nxforms,*96)
	  close(3)
	  nlineuse = listot
	  do i = 1, listot
	    lineuse(i) = inlist(i)
	  enddo
	  if (pipinput) then
	    if (PipGetString('UseTransformLines', listString) .eq. 0)
     &		call parselist(listString, lineuse, nlineuse)
	  else
	    print *,'Enter list of lines to use in file, or a single ',
     &		'line number to apply that'
	    print *,' transform to all sections',
     &		' (1st line is 0; ranges OK; / for section list)'
	    call rdlist(5,lineuse,nlineuse)
	  endif
C	    
C	    use single number for all sections
C
	  xftext=', transformed'
	  if(nlineuse.eq.1)then
	    do i=2,listot
	      lineuse(i)=lineuse(1)
	    enddo
	    nlineuse=listot
	  endif
	  if(nlineuse.ne.listot)call errorexit(
     &		'Number of lines does not match number of sections')
	endif
c
c	  find out if root beer float or what
c
	fraczero=0.
	ifmean=0
	iffloat = 0
	dminspec=0
	dmaxspec=0
	conlo=0
	conhi=255
	if (pipinput) then
	  ierr = 1 - PipGetTwoFloats('ContrastBlackWhite', conlo, conhi)
	  ierr2 = 1 - PipGetTwoFloats('ScaleMinAndMax', dminspec, dmaxspec)
	  if (ierr + ierr2 + 1 - PipGetInteger('FloatDensities', iffloat)
     &	       .gt. 1) call errorexit('The -scale, -contrast,'
     &	      //' and -float options are mutually exclusive')
	  if (iffloat .lt. 0)call errorexit('You must use -contrast or '
     &	      //'-scale instead of a negative -float entry')
	  if (ierr .ne. 0) iffloat = -2
	  if (ierr2 .ne. 0) iffloat = -1
	  
	else
	  write(*,102)
102	  format(' Enter 0 for no floating',/,8x,
     &	      '-2 to scale to bytes based on black and white contrast ',
     &	      'levels',/,8x,
     &	      '-1 to specify a single rescaling of all sections'
     &	      ,/,8x,' 1 to float all sections to same range',/,8x,' ',
     &	      '2 to float all sections to same mean & standard deviation'
     &	      ,/,8x,' 3 to shift sections to same mean without scaling',/
     &	      ,6x,'or 4 to shift to same mean and specify a single',
     &	      ' rescaling: ',$)
	  read(5,*)iffloat
	endif
	if(iffloat.gt.1)ifmean=1
	if(iffloat.lt.0)then
	  floatxt=', densities scaled'
	  if(iffloat.eq.-1 .and. .not.pipinput)then
	    write(*,'(1x,a,/,a,$)')'Values to scale input file''s'//
     &		' min and max to,','   or / to scale to maximum range,'
     &		//' or 1,1 to override mode scaling: '
	    read(5,*)dminspec,dmaxspec
	  elseif (iffloat .lt. -1) then
	    if (.not.pipinput) then
	      write(*,'(1x,a,$)')'Contrast ramp black and white settings '
     &		  //'(values between 0 and 255): '
	      read(5,*)conlo,conhi
	    endif
	    conhi=max(conhi,conlo+1.)
	    dminspec= -conlo*255/(conhi-conlo)
	    dmaxspec=dminspec+65025/(conhi-conlo)
	    print *,conlo, conhi,dminspec,dmaxspec
	  endif
	endif
c	  
c	  get new options
c
	if (pipinput) then
	  ierr = PipGetFloat('RotateByAngle', rotateAngle)
	  ierr = PipGetFloat('ExpandByFactor', expandFactor)

	  ierr = PipGetString('DistortionField', idfFile)
	  if (idfFile .ne. ' ') then
	    ifDistort = 1
	    xftext=', undistorted'
	    call readDistortions(idfFile, fieldDx, fieldDy, lmGrid, idfNx,
     &	    idfNy, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &	    iyGridStrt, yGridIntrv, nyGrid)
c
	    if (PipGetInteger('BinningOfImages', iBinning) .ne. 0)
     &		call errorexit
     &		('FOR NOW, YOU MUST SPECIFY BINNING OF IMAGES')
	    if (iBinning .ne. idfBinning) then
	      binRatio = idfBinning / float(iBinning)
	      ixGridStrt = nint(ixGridStrt * binRatio)
	      iyGridStrt = nint(iyGridStrt * binRatio)
	      xGridIntrv = xGridIntrv * binRatio
	      yGridIntrv = yGridIntrv * binRatio
	      do iy = 1, nyGrid
		do i = 1, nxGrid
		  fieldDx(i, iy) = fieldDx(i, iy) * binRatio
		  fieldDy(i, iy) = fieldDy(i, iy) * binRatio
		enddo
	      enddo
	    endif
c
c	      get maximum Y deviation to adjust chunk limits with
c
	    fieldMaxY = 0.
	    do iy = 1, nyGrid
	      do i = 1, nxGrid
		fieldMaxY = max(fieldMaxY, abs(fieldDy(i, iy)))
	      enddo
	    enddo
	    maxFieldY = int(fieldMaxY + 1.)
	  endif
	endif
	call PipDone()
c	    
c	  if not transforming and distorting, rotating, or expanding, set up
c	  a unit transform
c	  
	if (ifxform .eq. 0 .and. (ifDistort .ne. 0 .or.
     &	    rotateAngle .ne. 0. .or. expandFactor .ne. 0.)) then
	  ifxform = 1
	  call xfunit(f(1,1,1), 1.)
	  do i = 1, listot
	    lineuse(i) = 0
	  enddo
	  nlineuse = listot
	  nxforms = 1
	endif
c	  
c	  set up rotation and expansion transforms and multiply by transforms
c	  
	if (rotateAngle .ne. 0. .or. expandFactor .ne. 0.) then
	  call xfunit(frot, 1.)
	  if (rotateAngle .ne. 0.) then
	    frot(1,1) = cosd(rotateAngle)
	    frot(1,2) = -sind(rotateAngle)
	    frot(2,2) = frot(1,1)
	    frot(2,1) = -frot(1,2)
	    frot(1,3) = 0.5 * (frot(1,1) + frot(1,2)) - 0.5
	    frot(2,3) = 0.5 * (frot(2,1) + frot(2,2)) - 0.5
	  endif
	  if (expandFactor .eq. 0.) expandFactor = 1.
	  call xfunit(fexp, expandFactor)
	  call xfmult(frot, fexp, fprod)
	  print *,'transform',((fprod(i, iy),i=1,2),iy=1,3)
	  do i = 1, nxforms
	    call xfmult(f(1,1,i), fprod, frot)
	    call xfcopy(frot, f(1,1,i))
	  enddo
	endif
	if (expandFactor .eq. 0.) expandFactor = 1.
c
	if(iffloat.gt.0)then
	  floatxt=', floated to range'
	  if(fraczero.ne.0.)
     &        write(trunctxt,'(a,f6.3)')', truncated by',fraczero
	  if(ifmean.ne.0)then
c
c	      if means, need to read all sections to get means
c
	    if(iffloat.eq.2)then
	      floatxt=', floated to means'
	      zmin=1.e10
	      zmax=-1.e10
	    else
	      diffmin=0.
	      diffmax=0.
	      grandsum=0.
	      nsum=0.
	      if(iffloat.eq.3)then
		floatxt=',  shifted to means'
	      else
		floatxt=', mean shift&scaled'
		if (.not.pipinput) then
		  write(*,'(1x,a,/,a,$)')'Values to scale the shifted'//
     &		      ' min and max to,','   or / to scale to maximum'//
     &		      ' range: '
		  read(5,*)dminspec,dmaxspec
		endif
	      endif
	    endif
	    do ifil=1,nfilein
	      CALL IMOPEN(1,FILIN(ifil),'RO')
	      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C   
	      do ilis=1,nlist(ifil)
		nsecred=inlist(ilis+listind(ifil)-1)
		if(nsecred.lt.0.or.nsecred.ge.nz) then
		  print *
		  print *,'ERROR: NEWSTACK - SECTION',nsecred,
     &		      ' DOES NOT EXIST IN FILE'
		  call exit(1)
		endif
c		
		call scansection(array,idim,nx,ny,nsecred,iffloat,dmin2,
     &		    dmax2,dmean2,sdsec,loadyst,loadynd)
		secmean(ilis+listind(ifil)-1)=dmean2
c
		if(iffloat.eq.2)then
c
c		    find the min and max Z values ((density-mean)/sd) 
c
		  zmin=min(zmin,(dmin2-dmean2)/sdsec)
		  zmax=max(zmax,(dmax2-dmean2)/sdsec)
		else
c		    
c		    or, if shifting, get maximum range from mean
c
		  diffmin=min(diffmin,dmin2-dmean2)
		  diffmax=max(diffmax,dmax2-dmean2)
		  grandsum=grandsum+dmean2
		  nsum=nsum+1
		endif
	      enddo
	      call imclose(1)
	    enddo
c	      
c	      for shift to mean, figure out new mean, min and max and whether
c	      scaling will be needed to fit range
c
	    if(iffloat.gt.2)then
	      grandmean=grandsum/nsum
	      shiftmin=max(0.,grandmean+diffmin)
	      shiftmean=shiftmin-diffmin
	      shiftmax=grandmean+diffmax
	      if(iffloat.eq.3.and.mode.ne.2.and.newmode.ne.2.and.
     &		  optmax(mode+1).lt.shiftmax)then
		print *,'Densities will be compressed by',
     &		    optmax(mode+1)/shiftmax,' to fit in range'
		floatxt=', mean shift&scaled'
	      endif
	    endif
	  endif
	endif
c
c	  start looping over input images
c
	isec=1
	isecout=1
	ifileout=1
	call ialprt(.true.)
	call time(tim)
	call date(dat)
	ntrunclo=0
	ntrunchi=0
	ifheaderout=0
	iftempopen=0
	do ifil=1,nfilein
	  CALL IMOPEN(1,FILIN(ifil),'RO')
	  CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMININ,DMAXIN,DMEANIN)
          call irtsiz(1,nxyz,mxyz,nxyzst)
          call irtcel(1,cell)
c	    
c	    get extra header information if any
c
	  call irtnbsym(1,nbsymin)
	  if(nbsymin.gt.0)then
	    if(nbsymin.gt.maxextra)then
	      print *,'Extra header data too large for array and not',
     &		  ' accessed for input file ',filin(ifil)
	      nbsymin=0
	    else
	      call irtsym(1,nbsymin,extrain)
	      call irtsymtyp(1,nbytexin,iflagxin)
c		
c		DNM 4/18/02: if these numbers do not represent bytes and
c		flags, then number of bytes is 4 times nint + nreal
c
	      if(.not.nbytes_and_flags(nbytexin,iflagxin))
     &		  nbytexin=4*(nbytexin+iflagxin)
	    endif
	  endif
C   
c	    get each section in input file
c
	  do ilis=1,nlist(ifil)
	    nsecred=inlist(ilis+listind(ifil)-1)
	    if(nsecred.lt.0.or.nsecred.ge.nz) then
	      print *
	      print *,'ERROR: NEWSTACK - SECTION',nsecred,
     &		  ' DOES NOT EXIST IN FILE'
	      call exit(1)
	    endif
c
c	      set output characteristics from first section
c
	    if(isec.eq.1)then
	      if(nx3.le.0)nx3=nint(nx * expandFactor)
	      if(ny3.le.0)ny3=nint(ny * expandFactor)
	      if(newmode.lt.0)newmode=mode
	    endif
c
c	      see if need to open an output file

	    if(isecout.eq.1)then
C   
C		Create output file, transfer header from currently open file,
c		fix it
C   
	      CALL IMOPEN(2,FILOUT(ifileout),'NEW')
              call itrhdr(2,1)
	      call ialmod(2,newmode)
c
c		set new size, keep old nxyzst
c
	      NXYZ2(1)=NX3
	      NXYZ2(2)=NY3
	      NXYZ2(3)=NSECOUT(IFILEOUT)
              call ialsiz(2,nxyz2,nxyzst)
c
c		if mxyz=nxyz, keep this relationship
c
              if(mxyz(1).eq.nx.and.mxyz(2).eq.ny.and.mxyz(3).eq.nz)then
                MXYZ2(1)=NX3
                MXYZ2(2)=NY3
                MXYZ2(3)=NSECOUT(IFILEOUT)
                call ialsam(2,mxyz2)
	      else
		mxyz2(1)=mxyz(1)
		mxyz2(2)=mxyz(2)
		mxyz2(3)=mxyz(3)
              endif
c
c		keep delta the same by scaling cell size from change in mxyz
c
              do i=1,3
                CELL2(i)=mxyz2(i)*(cell(i)/mxyz(i)) / expandFactor
                CELL2(i+3)=90.
              enddo
	      CELL2(3)=mxyz2(3)*(cell(3)/mxyz(3))
	      CALL IALCEL(2,CELL2)
c
c 7/7/00 CER: remove the encodes
c
	      if(trunctxt.eq.' ')then
	        write(titlech,302) xftext,floatxt,dat,tim
                read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
C		ENCODE(80,302,TITLE)xftext,floatxt,dat,tim
	      else
                write(titlech,301) xftext,floatxt,trunctxt
                read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
C               ENCODE(80,301,TITLE)xftext,floatxt,trunctxt
	      endif
301	      FORMAT('NEWSTACK: Images copied',a13,a18,a20)
302	      FORMAT('NEWSTACK: Images copied',a13,a18,t57,a9,2x,a8)
	      DMAX=-1.e30
	      DMIN=1.e30
	      DMEAN=0.
c		
c		adjust extra header information if currently open file has it
c		
	      nbsymout=0
	      if(nbsymin.gt.0)then
		nbytexout=nbytexin
		nbsymout=nsecout(ifileout)*nbytexout
		nbsymout=min(maxextra,nbsymout)
		call ialnbsym(2,nbsymout)
		call imposn(2,0,0)
		indxout=0
	      endif
	    endif
C   
c	      handle complex images here and skip out
c
	    if((newmode+1)/2.eq.2.or.(mode+1)/2.eq.2)then
	      if((mode+1)/2.ne.2.or.(newmode+1)/2.ne.2)call errorexit(
     &		  'ALL INPUT FILES MUST BE COMPLEX IF ANY ARE')
	      if(nx*ny*2.gt.idim)call errorexit(
     &		  'INPUT IMAGE TOO LARGE FOR ARRAY.')
	      call imposn(1,nsecred,0)
	      call irdsec(1,array,*99)
	      call iclcdn(array,nx,ny,1,nx,1,ny,dmin2,dmax2,dmean2)
	      call imposn(2,isecout-1,0)
	      call iwrsec(2,array)
	      go to 80
	    endif
c   
c	      determine whether rescaling will be needed
c
	    optin=optmax(mode+1)
	    optout=optmax(newmode+1)
c	      
c	      set bottom of input range to 0 unless mode 1 or 2; set bottom
c	      of output range to 0 unless not changing modes
c
	    bottomin=0.
	    if(dminin.lt.0..and.(mode.eq.1.or.mode.eq.2))bottomin=-optin
	    bottomout=0.
	    if(mode.eq.newmode)bottomout=bottomin
	    rescale=.false.
c
c	      for no float: if mode = 2, no rescale;
c		otherwise rescale from input range to output range only if
c		mode is changing
c
	    if(iffloat.eq.0.and.newmode.ne.2)then
	      rescale=mode.ne.newmode
	    elseif(iffloat.ne.0)then
	      rescale=.true.
	    endif
c	      
c	      if transforming, get the shifts by applying the offset first
c	      then multiplying that by the transform
c
	    if (ifxform .ne. 0) then
	      lnu=lineuse(isec)+1
	      if(lnu.le.0.or.lnu.gt.nxforms)call errorexit(
     &		  'LINE NUMBER FOR TRANSFORM OUT OF BOUNDS')
	      call xfunit(frot, 1.)
	      frot(1,3) = -xcen(isec)
	      frot(2,3) = -ycen(isec)
	      call xfmult(frot, f(1,1,lnu), fprod)
	    endif
c	      
c	      figure out how the data will be loaded and saved; first see if
c	      both input and output images will fit in one chunk, or if
c	      entire input image will fit
c
	    linesleft=(idim-nx*ny)/nx3
	    nchunk=(ny3+linesleft-1)/linesleft
	    if(nchunk.eq.1.or.(nchunk.gt.0.and.nchunk.le.maxchunks.and.
     &		.not.rescale))then
c		
c		Use entire input and multi-chucnk output only if not rescaling
c		set up chunks for output, and set up that
c		whole image is needed for every output chunk.
c		Note that lines are numbered from 0 here
c
	      lineOutSt(1)=0
	      do ichunk=1,nchunk
		nextline=(ny3/nchunk)*ichunk+min(ichunk,mod(ny3,nchunk))
		nLinesOut(ichunk)=nextline-lineOutSt(ichunk)
		lineOutSt(ichunk+1)=nextline
		lineInSt(ichunk)=0
		nLinesIn(ichunk)=ny
	      enddo	      
	      maxin=ny
	      ifOutChunk=1
	    else
c		
c		otherwise, break output into successively more chunks, and
c		find out how many lines are needed of input for each chunk,
c		Scan twice: first trying to fit all the output to avoid a
c		read-write to temp file; then breaking equally
c		
	      ifOutChunk=-1
	      iscan=1
	      iytest=ny3
	      do while(iscan.le.2.and.ifOutChunk.lt.0)
		nchunk=1
		do while(nchunk.le.maxchunks.and.ifOutChunk.lt.0)
		  lineOutSt(1)=0
		  maxin=0
		  do ichunk=1,nchunk
		    nextline=(ny3/nchunk)*ichunk+min(ichunk,mod(ny3,nchunk))
		    nLinesOut(ichunk)=nextline-lineOutSt(ichunk)
		    lineOutSt(ichunk+1)=nextline
c
		    if(ifxform.eq.0)then
c			
c			simple case of no transform
c			
		      IYbase = NY/2 + YCEN(isec) - (NY3/2)
		      IY1 = max(0,IYbase+lineOutSt(ichunk))
		      iy2 = min(ny-1, iybase+nextline-1)
		    else
c			
c			transform: get input needs of 4 corners
c			pass and get back coordinates numbered from 1, subtract
c			an extra 1 to get to lines numbered from 0
c			Allow extra for distortion field Y component
c			
		      xci=nx/2.
		      yci=ny/2.
		      dx = fprod(1,3)
		      dy = fprod(2,3)
c		      dx=f(1,3,lnu)-xcen(isec)
c		      dy=f(2,3,lnu)-ycen(isec)
		      call backxform(nx,ny,nx3,ny3,fprod,xci ,yci,dx,dy,
     &			  1,lineOutSt(ichunk)+1,xp1,yp1)
		      call backxform(nx,ny,nx3,ny3,fprod,xci ,yci,dx,dy,
     &			  nx3,lineOutSt(ichunk)+1,xp2,yp2)
		      call backxform(nx,ny,nx3,ny3,fprod,xci ,yci,dx,dy,
     &			  1,nextline,xp3,yp3)
		      call backxform(nx,ny,nx3,ny3,fprod,xci ,yci,dx,dy,
     &			  nx3,nextline,xp4,yp4)
		      iy1=min(ny-1,max(0,int(min(yp1,yp2,yp3,yp4))-2 -
     &			  maxFieldY))
		      iy2=min(ny-1,max(0,int(max(yp1,yp2,yp3,yp4))+1 +
     &			  maxFieldY))
		    endif
		    lineInSt(ichunk)=iy1
		    nLinesIn(ichunk)=iy2+1-iy1
		    maxin=max(maxin,nLinesIn(ichunk))
		  enddo
c		    
c		    Will the input and output data now fit?  Then terminate.
c		    
		  if(iscan.eq.2)iytest=nLinesOut(1)
		  if(maxin*nx+iytest*nx3.le.idim)then
		    ifOutChunk=iscan-1
		  else
		    nchunk=nchunk+1
		  endif
		enddo
		iscan=iscan+1
	      enddo
	      if(ifOutChunk.lt.0)call errorexit(
     &		  ' INPUT IMAGE TOO LARGE FOR ARRAY.')
	    endif
c	    print *,'number of chunks:',nchunk
c	    do i=1,nchunk
c	      print *,i,lineInSt(i),nLinesIn(i),lineOutSt(i),nLinesOut(i)
c	    enddo
c	      
c	      open temp file if one is needed
c
	    if(rescale.and.ifOutChunk.gt.0.and.nchunk.gt.1.and.
     &		iftempopen.eq.0)then
	      tempext='nws      '
	      tempext(4:5)=tim(1:2)
	      tempext(6:7)=tim(4:5)
	      tempext(8:9)=tim(7:8)
	      tempname=temp_filename(FILOUT(ifileout),' ',tempext)
c
	      call imopen(3,tempname,'scratch')
	      call icrhdr(3,nxyz2,nxyz2,2,title,0)
	      iftempopen=1
	    endif
c
	    ibbase=maxin*nx+1
c	      
c	      get the mean of section from previous scan, or a new scan
c
	    if(ifmean.ne.0)then
	      dmeansec=secmean(ilis+listind(ifil)-1)
	      loadyst=-1
	      loadynd=-1
	    else
	      call scansection(array,idim,nx,ny,nsecred,0,dmin2,
     &		  dmax2,dmeansec,sdsec,loadyst,loadynd)
	      loadynd=min(loadynd,loadyst+maxin-1)
	    endif
c	      
c	      loop on chunks
c
	    dsum=0.
	    dsumsq=0.
	    tmpmin=1.e30
	    tmpmax=-1.e30
	    do ichunk=1,nchunk
	      needyst=lineInSt(ichunk)
	      needynd=needyst+nLinesIn(ichunk)-1
c		
c		first load data that is needed if not already loaded
c
	      if(needyst.lt.loadyst.or.needynd.gt.loadynd)then
		if(loadyst.le.needyst.and.loadynd.ge.needyst)then
c		    
c		    move data down if it will fill a bottom region
c		    
		  nmove=(loadynd+1-needyst)*nx
		  noff=(needyst-loadyst)*nx
		  do i=1,nmove
		    array(i)=array(i+noff)
		  enddo
		  nload=needynd-loadynd
		  call imposn(1,nsecred,loadynd+1)
		  call irdsecl(1,array(nmove+1),nload,*99)
		elseif(needyst.le.loadyst.and.needynd.ge.loadyst)then
c		    
c		    move data up if it will fill top
c		    
		  nmove=(needynd+1-loadyst)*nx
		  noff=(loadyst-needyst)*nx
		  do i=nmove,1,-1
		    array(i+noff)=array(i)
		  enddo
		  nload=loadyst-needyst
		  call imposn(1,nsecred,needyst)
		  call irdsecl(1,array,nload,*99)
		else
c		    
c		    otherwise just get whole needed region
c
		  nload=needynd+1-needyst
		  call imposn(1,nsecred,needyst)
		  call irdsecl(1,array,nload,*99)
		endif
		loadyst=needyst
		loadynd=needynd
	      endif
	      nyload=loadynd+1-loadyst
	      nych=nLinesOut(ichunk)
	      npix=nx3*nych
	      ibchunk=ibbase
	      if(ifOutChunk.eq.0)ibchunk=ibbase+lineOutSt(ichunk)*nx3

	      if(ifxform.ne.0)then
c		  
c		  do transform if called for
c		  
		xci=nx/2.
		yci=ny/2.-loadyst
		dx=fprod(1,3)
		dy=(ny3-nych)/2.+fprod(2,3) - lineOutSt(ichunk)
c		dx=f(1,3,lnu)-xcen(isec)
c		dy=(ny3-nych)/2.+f(2,3,lnu) - ycen(isec) - lineOutSt(ichunk)
		if (ifDistort .eq. 0) then
		  call cubinterp(array,array(ibchunk),nx,nyload, nx3, nych,
     &		      fprod,xci ,yci, dx,dy,1.,dmeansec, ifxform - 1)
		else
		  call undistort(array,array(ibchunk),nx,nyload, nx3, nych,
     &		      fprod,xci ,yci, dx,dy,1.,dmeansec, ifxform - 1,
     &		      fieldDx, fieldDy, lmGrid, nxGrid, ixGridStrt, xGridIntrv,
     &		      nyGrid, iyGridStrt, yGridIntrv)
		endif
	      else
c		  
c		  otherwise repack array into output space nx3 by ny3, with
c		  offset as specified, using the special repack routine
c		  
		IXCEN = NX/2 + XCEN(isec)
		IX1 = IXCEN - (NX3/2)
		IX2 = IX1 + NX3 - 1
C		  
		IYbase = NY/2 + YCEN(isec) - (NY3/2)
		IY1 = IYbase+lineOutSt(ichunk)-loadyst
		iy2 = iy1+nych-1
c		  
		CALL IREPAK2(array(ibchunk),ARRAY,nx,nyload,IX1,IX2,IY1,
     &		    IY2,dmeansec)
	      endif
c	      
c		if no rescaling, or if mean is needed now, accumulate sums for
c		mean
c		
	      if(.not.rescale.or.ifmean.ne.0)then
		if(iffloat.eq.2)then
		  call iclavgsd(array(ibchunk),nx3,nych,1,nx3,1,nych,
     &		      tmin2,tmax2,tsum, tsumsq, avgsec,sdsec)
c		  print *,'chunk mean&sd',ichunk,avgsec,sdsec
		  dsumsq=dsumsq+tsumsq
		else
		  call iclden(array(ibchunk),nx3,nych,1,nx3,1,nych,tmin2,
     &		      tmax2,tmean2)
		  tsum=tmean2*npix
		endif
		tmpmin=min(tmpmin,tmin2)
		tmpmax=max(tmpmax,tmax2)
		dsum=dsum+tsum
	      else
c		  
c		  otherwise get new min and max quickly
c		  
		do i=1,npix
		  val=array(i+ibchunk-1)
		  if(val.lt.tmpmin)tmpmin=val
		  if(val.gt.tmpmax)tmpmax=val
		enddo
	      endif
	      if(iffloat.eq.0.and.newmode.ne.2)then
c		  
c		  6/27/01: really want to truncate rather than rescale; so if
c		  the min or max is now out of range for the input mode,
c		  truncate the data and adjust the min and max
c
		if(tmpmin.lt.bottomin.or.tmpmax.gt.optin)then
		  tsum2=0.
		  do i=1,npix
		    val=max(bottomin,min(optin,array(i+ibchunk-1)))
		    tsum2=tsum2+val
		    array(i+ibchunk-1)=val
		  enddo
		  tmpmin=max(tmpmin,bottomin)
		  tmpmax=min(tmpmax,optin)
		  dsum=dsum+tsum2-tsum
		endif
	      endif
c		
c		write all but last chunk
c		
	      if(.not.rescale)then
c		print *,'writing to real file',ichunk
		call imposn(2,isecout-1,lineOutSt(ichunk))
		call iwrsecl(2,array(ibchunk),nLinesOut(ichunk))
	      elseif(ichunk.ne.nchunk.and.ifOutChunk.gt.0)then
c		print *,'writing to temp file',ichunk
		call imposn(3,0,lineOutSt(ichunk))
		call iwrsecl(3,array(ibbase),nLinesOut(ichunk))
	      endif
	    enddo
c   
c   calculate new min and max after rescaling under various possibilities
c
	    dmin2=tmpmin
	    dmax2=tmpmax
c
	    if(iffloat.eq.0.and.rescale)then
c		
c		no float but mode change (not to mode 2):
c		rescale from input range to output range
c		  
	      dmin2=(tmpmin-bottomin)*(optout-bottomout)/
     &		  (optin-bottomin)+bottomout
	      dmax2=(tmpmax-bottomin)*(optout-bottomout)/
     &		  (optin-bottomin)+bottomout
	    elseif(iffloat.lt.0)then
c		
c		if specified global rescale, set values that dminin and dmaxin
c		map to, either the maximum range or the values specified
c
	      if(dminspec.eq.0.and.dmaxspec.eq.0)then
		dminnew=0.
		dmaxnew=optout
	      else if(dminspec.eq.dmaxspec)then
		dminnew=dminin
		dmaxnew=dmaxin
	      else
		dminnew=dminspec
		dmaxnew=dmaxspec
	      endif
c		
c		then compute what this section's tmpmin and tmpmax map to
c		
	      dmin2=(tmpmin-dminin)*(dmaxnew-dminnew)/(dmaxin-dminin)
     &		  +dminnew
	      dmax2=(tmpmax-dminin)*(dmaxnew-dminnew)/(dmaxin-dminin)
     &		  +dminnew
	    elseif(iffloat.gt.0)then
c
c		If floating: scale to a dmin2 that will knock out fraczero of
c		the range after truncation to zero
c
	      dmin2=-optout*fraczero/(1.-fraczero)
	      if(ifmean.eq.0)then
c
c		  float to range, new dmax2 is the max of the range
c
		dmax2=optout
	      elseif(iffloat.eq.2)then
c		  :float to mean, it's very hairy
		call sums_to_avgsd(dsum,dsumsq,nx3*ny3,avgsec,sdsec)
c		print *,'overall mean & sd',avgsec,sdsec
		zminsec=(tmpmin-avgsec)/sdsec
		zmaxsec=(tmpmax-avgsec)/sdsec
		dmin2=(zminsec-zmin)*optout/(zmax-zmin)
		dmax2=(zmaxsec-zmin)*optout/(zmax-zmin)
		dmin2=max(0.,dmin2)
		dmax2=min(dmax2,optout)
	      else
c		  
c		  shift to mean
c
		tmpmean=dsum/(nx3*ny3)
c		  
c		  values that min and max shift to
c
		tmpminshf=tmpmin+shiftmean-tmpmean
		tmpmaxshf=tmpmax+shiftmean-tmpmean
c
		if(iffloat.eq.3)then
c
c		    for no specified scaling, set new min and max to
c		    shifted values
c
		  dmin2=tmpminshf
		  dmax2=tmpmaxshf
		  if(newmode.ne.2)then
c
c		      then, if mode is not 2, set up for scaling if range is
c		      too large and/or if there is a modal shift
c
		    optin=max(optin,shiftmax)
		    dmin2=tmpminshf*optout/optin
		    dmax2=tmpmaxshf*optout/optin
		  endif
		else
c		    
c		    for specified scaling of shifted means
c
		  if(dminspec.eq.dmaxspec)then
		    dminnew=0.5
		    dmaxnew=optout-0.5
		  else
		    dminnew=dminspec
		    dmaxnew=dmaxspec
		  endif
c		
c		    for specified scaling, compute what this section's tmpmin
c		    and tmpmax map to
c
		  dmin2=(tmpminshf-shiftmin)*(dmaxnew-dminnew)/
     &		      (shiftmax-shiftmin)+dminnew
		  dmax2=(tmpmaxshf-shiftmin)*(dmaxnew-dminnew)/
     &		      (shiftmax-shiftmin)+dminnew
		endif
	      endif
	    endif
c
	    if(rescale)then
c
c		if scaling, set up equation, scale and compute new mean
c
	      sclfac=(dmax2-dmin2)/(tmpmax-tmpmin)
	      const=dmin2-sclfac*tmpmin
	      dmin2=1.e20
	      dmax2=-1.e20
	      dmean2=0.
c		set up minimum value to output based on mode
	      if(newmode.eq.1)then
		denoutmin=-32767
	      elseif(newmode.eq.2)then
		denoutmin=-1.e30
		optout=1.e30
	      else
		denoutmin=0.
	      endif
c		
c		loop backwards on chunks, reloading all but last, scaling
c		and rewriting
c		
	      do ichunk=nchunk,1,-1
		ibchunk=ibbase
		if(ifOutChunk.eq.0)ibchunk=ibbase+lineOutSt(ichunk)*nx3
		if(ichunk.ne.nchunk.and.ifOutChunk.gt.0)then
c		  print *,'reading',ichunk
		  call imposn(3,0,lineOutSt(ichunk))
		  call irdsecl(3,array(ibbase),nLinesOut(ichunk),*99)
		endif
		do iy=1,nLinesOut(ichunk)
		  istart=ibchunk+(iy-1)*nx3
		  tsum=0.
		  do i=istart,istart+nx3-1
		    den=sclfac*array(i)+const
		    if(den.lt.denoutmin)then
		      ntrunclo=ntrunclo+1
		      den=denoutmin
		    elseif(den.gt.optout)then
		      ntrunchi=ntrunchi+1
		      den=optout
		    endif
		    array(i)=den
		    tsum=tsum+den
		    dmin2=min(dmin2,den)
		    dmax2=max(dmax2,den)
		  enddo
		  dmean2=dmean2+tsum
		enddo
c		print *,'writing',ichunk
		call imposn(2,isecout-1,lineOutSt(ichunk))
		call iwrsecl(2,array(ibchunk),nLinesOut(ichunk))
	      enddo
	    else
c
c		if not scaling
c
	      dmean2=dsum
	    endif
c
	    dmean2=dmean2/(nx3*ny3)
	    if(ifheaderout.eq.0) print *,
     &		'section   input min&max       output min&max  &  mean'
	    ifheaderout=1
	    write(*,'(i6,5f10.2)')isec-1,tmpmin,tmpmax,dmin2,dmax2,
     &		dmean2

C   
80	    isecout=isecout+1
	    DMIN = MIN(DMIN,DMIN2)
	    DMAX = MAX(DMAX,DMAX2)
	    DMEAN = DMEAN + DMEAN2
c	      
c	      transfer extra header bytes if present
c
	    if(nbsymout.ne.0.and.indxout.lt.nbsymout)then
	      nbcopy=min(nbytexout,nbytexin,nbsymin)
	      nbclear=nbytexout-nbcopy
	      do i=1,nbcopy
		indxout=indxout+1
		extraout(indxout)=extrain(nsecred*nbytexin+i)
	      enddo
	      do i=1,nbclear
		indxout=indxout+1
		extraout(indxout)=0
	      enddo
	    endif
c
c	      see if need to close stack file
c
	    if(isecout.gt.nsecout(ifileout))then
	      if(nbsymout.gt.0)call ialsym(2,nbsymout,extraout)
	      DMEAN=DMEAN/nsecout(ifileout)
C   
	      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
	      CALL IMCLOSE(2)
	      isecout=1
	      ifileout=ifileout+1
	    endif
	    isec=isec+1
	  enddo
	  call imclose(1)
	enddo
C   
	if(iftempopen.ne.0)call imclose(3)
	if(ntrunclo+ntrunchi.gt.0)write(*,103)ntrunclo,ntrunchi
103	format(' TRUNCATIONS OCCURRED:',i7,' at low end and',i7,
     &	    ' at high end of range')
	call exit(0)
99	call errorexit(' END OF IMAGE WHILE READING')
96	call errorexit('ERROR READING TRANSFORM FILE')
	END


c	  IREPAK2 repacks an image from a portion of a 2-d array sequentially
c	  into a 1-d array (which should not be the same array).  Pixels
c	  outside the range of the original array will be filled with the
c	  supplied value of DMEAN.  BRRAY is the repacked array,
c	  everything else follows definition of iwrpas; i.e. ARRAY is
c	  dimensioned MX by MY, and the starting and ending index coordinates
c	  (numbered from 0) are given by NX1, NX2, NY1, NY2
c
        subroutine irepak2(brray,array,mx,my,nx1,nx2,ny1,ny2,dmean)
	implicit none
	integer*4 mx,my,nx1,nx2,ny1,ny2
        real*4 brray(*),array(mx,my),dmean
	integer*4 ind, iy,ix
        ind=1
        do iy=ny1+1,ny2+1
	  if(iy.ge.1.and.iy.le.my)then
	    do ix=nx1+1,nx2+1
	      if(ix.ge.1.and.ix.le.mx)then
		brray(ind)=array(ix,iy)
	      else
		brray(ind)=dmean
	      endif
	      ind=ind+1
	    enddo
	  else
	    do ix=nx1+1,nx2+1
	      brray(ind)=dmean
	      ind=ind+1
	    enddo
	  endif
        enddo
        return
        end


c	  SCANSECTION will determine the min DMIN2, max DMAX2, and mean DMEAN2
c	  of section NSECRED.  It will also determine the standard deviation
c	  SDSEC if IFFLOAT = 2.  It uses ARRAY for storage.  IDIM specifies
c	  the size of ARRAY, while NX and NY are the image size.  The image
c	  will be loaded in chunks if necessary.  LOADYST and LOADYND are the
c	  starting and ending lines (numbered from 0) that are left in ARRAY.
c
	subroutine scansection(array,idim,nx,ny,nsecred,iffloat,dmin2,
     &	    dmax2,dmean2,sdsec,loadyst,loadynd)
	implicit none
	integer*4 idim,nx,ny,nsecred,iffloat,loadyst,loadynd
	real*4 array(idim),dmin2,dmax2,dmean2,sdsec
	integer*4 maxlines,nloads,iline,iload,nlines
	real*4 dsum,dsumsq,tsum,tmin2,tmax2,tmean2,tsumsq,avgsec
c	  
c	  load in chunks if necessary, based on the maximum number
c	  of lines that will fit in the array
c	  
	maxlines=idim/nx
	nloads=(ny+maxlines-1)/maxlines
	iline=0
	dmin2=1.e30
	dmax2=-dmin2
	dsum=0.
	dsumsq=0.
	do iload=1,nloads
	  nlines=ny/nloads
	  if(iload.le.mod(ny,nloads))nlines=nlines+1
	  call imposn(1,nsecred,iline)
	  call irdsecl(1,array,nlines,*99)
c	    
c	    accumulate sums for mean and sd if float 2, otherwise
c	    just the mean
c	    
	  if(iffloat.eq.2)then
	    call iclavgsd(array,nx,nlines,1,nx,1,nlines,tmin2,tmax2,
     &		tsum,tsumsq, avgsec,sdsec)
	    dsumsq=dsumsq+tsumsq
	  else
	    call iclden(array,nx,nlines,1,nx,1,nlines,tmin2,
     &		tmax2,tmean2)
	    tsum=tmean2*nx*nlines
	  endif
	  dmin2=min(dmin2,tmin2)
	  dmax2=max(dmax2,tmax2)
	  dsum=dsum+tsum
	  iline=iline+nlines
	enddo
c
	if(iffloat.eq.2)then
	  call sums_to_avgsd(dsum,dsumsq,nx*ny,dmean2,sdsec)
	else
	  dmean2=dsum/(nx*ny)
	endif
	loadynd=iline-1
	loadyst=iline-nlines
	return
99	call errorexit( 'READ ERROR')
	end


c	  BACKXFORM will determine the array coordinates XP and YP in an input
c	  image that transform to the given array indexes IX, IY in an output
c	  image.  Other arguments match those of CUBINTERP: the input image is
c	  NXA by NYA; the output image is NXB by NYB; XC, YC is the center
c	  coordinate of the input image; AMAT is the 2x2 transformation matrix
c	  and XT, YT are the translations.
c
	SUBROUTINE backxform(NXA,NYA,NXB,NYB,AMAT,XC,YC,XT,YT,ix,iy,xp,yp)
	implicit none
	integer*4 NXA,NYA,NXB,NYB,ix,iy
	real*4 AMAT(2,2),XC,YC,XT,YT,xp,yp
	real*4 xcen,ycen,xco,yco,denom,a11,a12,a21,a22,dyo,dxo
C
C   Calc inverse transformation
C
	XCEN = NXB/2. + XT + 1
	YCEN = NYB/2. + YT + 1
	XCO = XC + 1
	YCO = YC + 1
	DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
	A11 =  AMAT(2,2)/DENOM
	A12 = -AMAT(1,2)/DENOM
	A21 = -AMAT(2,1)/DENOM
	A22 =  AMAT(1,1)/DENOM
c
c	  get coordinate transforming to ix,iy
c
	DYO = IY - YCEN
	DXO = IX - XCEN
	XP = A11*DXO + A12*DYO + XCO
	YP = A21*DXO + A22*DYO + YCO
	return
	end


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: NEWSTACK - ',message
	call exit(1)
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


	SUBROUTINE undistort(ARRAY,BRAY,NXA,NYA,NXB,NYB,AMAT,
     &	    XC,YC,XT,YT,SCALE,DMEAN, linear, dxGrid, dyGrid, ixgDim,
     &	    nxGrid, ixGridStrt, xGridIntrv, nyGrid, iyGridStrt,
     &	    yGridIntrv)
	implicit none
	integer*4 NXA,NYA,NXB,NYB,linear
	real*4 ARRAY(NXA,NYA),BRAY(NXB,NYB),AMAT(2,2)
	real*4 XC,YC,XT,YT,SCALE,dmean
	real*4 xcen,ycen,xco,yco,denom,a11,a12,a22,a21,dyo,xbase,ybase
	real*4 xst,xnd,xlft,xrt,xp,yp,dennew,dx,dy,v2,v4,v6,v8,v5,a,b,c,d
	real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3,vmin,vmax
	integer*4 iy,ix,ixp,ixpp1,iyp,iypp1,iypp2,ixpp2,ixpm1,iypm1,linefb
	integer*4 ixnd,ixst,ixfbst,ixfbnd,iqst,iqnd,ifall
	integer*4 ixgDim, nxGrid, nyGrid, ixGridStrt, iyGridStrt
	real*4 dxGrid(ixgDim, *), dyGrid(ixgDim, *)
	real*4 xGridIntrv, yGridIntrv
C
C   Calc inverse transformation
C
	XCEN = NXB/2. + XT + 1
	YCEN = NYB/2. + YT + 1
	XCO = XC + 1
	YCO = YC + 1
	DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
	A11 =  AMAT(2,2)/DENOM
	A12 = -AMAT(1,2)/DENOM
	A21 = -AMAT(2,1)/DENOM
	A22 =  AMAT(1,1)/DENOM
C
C Loop over output image
C
	DO IY = 1,NYB
	  DYO = IY - YCEN
	  xbase = a12*dyo +xco - a11*xcen
	  ybase = a22*dyo + yco - a21*xcen

	  if (linear .ne. 0) then
	    do ix=1,nxb
	      xp = a11*ix + xbase
	      yp = a21*ix + ybase
	      call interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid,
     &		  nyGrid, ixGridstrt, xGridIntrv, iyGridStrt,
     &		  yGridIntrv, dx, dy)
	      xp = xp + dx
	      yp = yp + dy
	      IXP = XP
	      IYP = YP
	      bray(ix,iy)=dmean
	      IF (IXP .ge. 1 .and. IXP .lt. NXA .and. IYP .ge. 1 .and.
     &		  IYP .lt. NYA) then
		DX = XP - IXP
		DY = YP - IYP
		IXPP1 = IXP + 1
		IYPP1 = IYP + 1
		bray(ix, iy) = (1. - dy) * ((1. - dx) * array(ixp, iyp) +
     &		    dx * array(ixpp1, iyp)) +
     &		    dy * ((1. - dx) * array(ixp, iypp1) +
     &		    dx * array(ixpp1, iypp1))
	      endif
	    enddo
	  else
	    do ix=1,nxb
	      xp = a11*ix + xbase
	      yp = a21*ix + ybase
	      call interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid,
     &		  nyGrid, ixGridstrt, xGridIntrv, iyGridStrt,
     &		  yGridIntrv, dx, dy)
	      xp = xp + dx
	      yp = yp + dy
	      IXP = XP
	      IYP = YP
	      bray(ix,iy)=dmean
	      IF (IXP .ge. 1 .and. IXP .lt. NXA - 1 .and. IYP .ge. 1 .and.
     &		  IYP .lt. NYA - 1) then

		DX = XP - IXP
		DY = YP - IYP
		IXPP1 = IXP + 1
		IXPM1 = IXP - 1
		IYPP1 = IYP + 1
		IYPM1 = IYP - 1
		ixpp2 = ixp + 2
		iypp2 = iyp + 2
		
		dxm1 = dx-1.
		dxdxm1=dx*dxm1
		fx1=-dxm1*dxdxm1
		fx4=dx*dxdxm1
		fx2=1+dx**2*(dx-2.)
		fx3=dx*(1.-dxdxm1)
		
		dym1 = dy-1.
		dydym1=dy*dym1
		
		v1=fx1*array(ixpm1,iypm1)+fx2*array(ixp,iypm1)+
     &		    fx3*array(ixpp1,iypm1)+fx4*array(ixpp2,iypm1)
		v2=fx1*array(ixpm1,iyp)+fx2*array(ixp,iyp)+
     &		    fx3*array(ixpp1,iyp)+fx4*array(ixpp2,iyp)
		v3=fx1*array(ixpm1,iypp1)+fx2*array(ixp,iypp1)+
     &		    fx3*array(ixpp1,iypp1)+fx4*array(ixpp2,iypp1)
		v4=fx1*array(ixpm1,iypp2)+fx2*array(ixp,iypp2)+
     &		    fx3*array(ixpp1,iypp2)+fx4*array(ixpp2,iypp2)
		bray(ix,iy)=-dym1*dydym1*v1+(1.+dy**2*(dy-2.))*v2+
     &		    dy*(1.-dydym1)*v3 +dy*dydym1*v4
c		
	      endif
	    enddo
	  endif
	  
	enddo
	RETURN
	END


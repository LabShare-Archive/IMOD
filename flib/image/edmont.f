*************EDMONT.FOR**********************************************
*   
*   A GENERAL MONTAGE EDITOR TO MOVE IMAGES INTO, OUT OF, OR BETWEEN MONTAGES
*   It can float the images to a common range or mean of density, scaling all
*   of the pieces in a section by the same amount.
*   It can output only the pieces that intersect a defined subset of the image
*   area. 
*	  
*   If there are piece coordinates in the extended header of the image file,
*   the program will transfer those coordinates to the output file.  These
*   coordinates can be used instead of ones from a piece list file.  If one
*   wishes to have sections numbered sequentially from zero, the Z coordinates
*   in the header of the output file will be modified appropriately.  
*   
*   All inputs are prompted interactively.  The input is as follows; lines
*   starting with IF are entered only if a particular option is selected.
*
*   Number of input files, or -1 to enter list of files from a file
*
*   IF you enter a number >0, then for each input file, enter two lines:
*   .	Image file name
*   .   Piece list file name. or Return to use coordinates from image header
*   .	list of section numbers to take from that file.  The sections numbers
*   .		correspond to the Z values in the list of piece coordinates.
*   .		Ranges may be entered (e.g. 0-4,5,7-9)
*
*   OTHERWISE, IF you enter -1, then enter the name of the file with list of
*   .		files and sections.  The format of this file should be
*      Number of input files
*      Image file name
*      Piece list file name, or blank line to use coordinates from image header
*      list of section numbers, ranges allowed (e.g. 0-3,5,7-9)
*      next file and list of sections, etc.
*
*   Number of output files, or -1 to enter list of files from a file
*
*   IF you enter 1, next enter:
*	the name of the single output image file
*	the name of the output piece list file, or Return for none (but only
*   .      if the input file header has piece coordinates)
*
*   OR IF you enter a number >1, then for each output file, enter three lines:
*   .	Image file name
*	Piece list file name, or Return for none
*   .	Number of sections to put in that file
*
*   OR IF you enter -1, then enter the name of the file with the list of files
*   .	and numbers of of sections.  The format of this file is:
*      Number of output files
*      Image file name
*      Piece list file name, or blank line for none
*      number of sections to place in that file
*      next file and number of sections, etc.
*
*   The minimum and maximum X, and minimum and maximum Y coordinates of the
*   	area that you want to include in the output file.  All pieces that
*   	intersect this area will be included.  Enter / to include all pieces
*	regardless of X-Y coordinates.
*
*   1 to enter a list of points marking individual pieces that should be
*	excluded, or 0 to skip this option.
*	  
*   IF you enter 1, next enter the name of a text file with a list of X, Y,
*       and Z coordinates, one set of X,Y,Z per line.  Each point should be
*       located within a piece to be excluded.
*
*   The data mode of the output file, or / to take the same mode as the first
*   .	input file.  Mode can be 0 or 1 for integer*1 or 2, 2 for real*4, or 9
*   .	to 15 for 9 to 15 bit values.
*
*   0 for no floating of densities, 1 to float densities so that each section
*	occupies the same RANGE of densities (the optimum range for the
* 	particular data mode), or 2 to float so that all scetions have the same
*	MEAN and STANDARD DEVIATION of density.  Either kind of floating
*	requires the input sections to be read twice.   
*
*   1 to renumber output sections sequentially from zero in each output file,
*	or 0 to leave all Z values the same as in the input files.  This option
*	is the most convenient way to close up gaps in an image file if some
*	sections are being deleted.
*
*   IF you specify only 1 input and 1 output file and they have the same name,
*	  and you select all sections in the input stack, with all pieces
*	  included and no change of data mode, then you have the option of
*	  rewriting pieces to the input data stack.
*	  Enter 1 to rewrite the data to input stack, or 0 to write the data to
*	  a new file.
*	  If you do rewrite the data, then no new file of piece coordinates is
*	  produced.  However, coordinates in the extra header may be modified
*	  and rewritten.
*
*   If floating is not selected, data is rescaled uniformly, if necessary, to
*   fit a new output data mode.  If the input file has real data (mode 2) that
*   is not confined to the range 0-255, and you are converting to another
*   mode, then you must specify one of the two kinds of floating, otherwise
*   the program will fail.
*
*   David Mastronarde for VAX	5/9/89
*	  4/19/90 Added line-by-line prompts for files and section entry, added
*	  time/date stamp, fixed bugs on rescaling in place and missing secs
*         1999: added ability to knock out pieces.
*	  1/3/00: made it handle extra header data, made scaling logic more
*	  like NEWSTACK and made sure it could handle negative integers. 
*	  10/24/00: made it actually use coordinates in header and renumber
*	  sections sequentially.
*   
************************************************************************
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2006/07/08 13:47:48  mast
c	  Raised maxextra again and some other limits, put big array in common
c	
c	  Revision 3.2  2005/12/09 04:45:32  mast
c	  gfortran: .xor., continuation, or byte fixes
c	
c	  Revision 3.1  2005/07/27 03:31:00  mast
c	  Redimensioned maxextra to handle mistakenly large extra header
c	
*   
	parameter (idim=4200,lmfil=1000,lmsec=2048,limpcl=100000)
	parameter (maxextra=4000000)
	COMMON //NX,NY,NZ
C   
	integer*4 NXYZ(3),MXYZ(3),NXYZST(3),NXYZ2(3),MXYZ2(3)
        real*4 ARRAY(idim*idim),TITLE(20), CELL2(6),cell(6)
C   
	CHARACTER*120 FILIN(lmfil),FILOUT(lmfil),filistin,filistout,
     &	    pifilin(lmfil),pifilout(lmfil)
C   
	EQUIVALENCE (NX,NXYZ)
C   
	DATA NXYZST/0,0,0/
	character*20 floatxt/' '/,trunctxt/' '/
	integer*4 inlist(lmsec),nlist(lmfil),listind(lmfil)
     &      ,nsecout(lmfil),listz(lmsec)
     &	    ,ixpclist(limpcl),iypclist(limpcl) ,izpclist(limpcl)
     &	    ,ixpcout(limpcl),iypcout(limpcl),izpcout(limpcl)
	real*4 optmax(16)
     &	    ,dminsec(lmsec),dmaxsec(lmsec),avgsec(lmsec),sdsec(lmsec)
	integer*1 extrain(maxextra),extraout(maxextra)
	data optmax/255.,32767.,7*255.,511.,1023.,2047.,4095.,8191.,
     &      16383.,32767./
	logical rescale,notknock
	integer*4 ixko(limpcl),iyko(limpcl),izko(limpcl)
	character*3 instat
	character dat*9,tim*8
	integer*2 temp
        common /bigarr/ array
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
C   
C   Read in list of input files
C         
        call setExitPrefix('ERROR: EDMONT - ')
	inunit=5
	write(*,'(1x,a,$)')'# of input files (or -1 to read list'//
     &	    ' of input files from file): '
	read(*,*)nfilein
	if(nfilein.lt.0)then
	  inunit=7
	  write(*,'(1x,a,$)')'Name of input list file: '
	  read(*,101)filistin
	  call dopen(7,filistin,'ro','f')
	  read(inunit,*)nfilein
	endif
	listot=0
	do i=1,nfilein
	  if(inunit.ne.7)then
	    if(nfilein.eq.1)then
	      write(*,'(1x,a,$)')'Name of input image file: '
	    else
	      write(*,'(1x,a,i3,a,$)')
     &		  'Name of input image file #',i,': '
	    endif
	  endif
	  READ(inunit,101)FILIN(i)
	  if(inunit.ne.7)write(*,'(1x,a,/,a,$)')
     &	      'Name of file with list of piece coordinates (or Return'
     &	      //' to use piece',' coordinates in image header): '
	  READ(inunit,101)pifilIN(i)
	  if(inunit.ne.7)print *,'Enter list of sections to read from'//
     &	      ' file (1st sec is 0; ranges may be entered)'
	  call rdlist(inunit,inlist(listot+1),nlist(i))
	  listind(i)=listot+1
	  listot=listot+nlist(i)
	enddo
	close(7)
101	FORMAT(A)
C   
C   Read in list of output files
C   
	inunit=5
	noutot=0
	write(*,'(1x,a,$)')'# of output files (or -1 to read list'//
     &	    ' of output files from file): '
	read(*,*)nfileout
	if(nfileout.lt.0)then
	  inunit=7
	  write(*,'(1x,a,$)')'Name of output list file: '
	  read(*,101)filistout
	  call dopen(7,filistout,'ro','f')
	  read(inunit,*)nfileout
	elseif(nfileout.eq.1)then
	  write(*,'(1x,a,$)')'Name of output image file: '
	  read(*,101)filout(1)
	  write(*,'(1x,a,/,a,$)')'Name of output piece list file '//
     &	      '(or Return for none, if image headers',
     &	      ' have piece coordinates): '
	  read(*,101)pifilout(1)
	  nsecout(1)=listot
	  noutot=listot
	endif
	if(noutot.eq.0)then
	  do i=1,nfileout
	    if(inunit.ne.7)write(*,'(1x,a,i3,a,$)')
     &		'Name of output image file #',i,': '
	    READ(inunit,101)FILOUT(i)
	    if(inunit.ne.7)write(*,'(1x,a,i3,a,$)')
     &		'Name of output piece list file #',i,
     &		' (Return for none): '
	    READ(inunit,101)PIFILOUT(i)
	    if(inunit.ne.7)write(*,'(1x,a,$)')
     &		'Number of sections to store in that file: '
	    read(inunit,*)nsecout(i)
	    noutot=noutot+nsecout(i)
	  enddo
	endif
	if(noutot.ne.listot)print *,'WARNING: specified numbers of'//
     &	    ' input and output sections do not match'
c	  
c	  get new size and mode
c	  
	minxinc=0
	maxxinc=0
	minyinc=0
	maxyinc=0
	write(*,'(1x,a,/,a,$)')'Enter Min X, Max X, Min Y, & Max Y;'//
     &	    ' pieces outside this range will be excluded',
     &	    '   from output file (enter / to include all pieces): '
	read(*,*)minxinc,maxxinc,minyinc,maxyinc
c	  
	write(*,'(1x,a,$)')'1 to enter a list of points specifying '//
     &	    'pieces to exclude, 0 not to: '
	read(5,*)ifknock
	knocks=0
	if(ifknock.ne.0)then
	  write(*,'(1x,a,$)')'Name of file with list of points: '
	  read(5,101)filistin
	  call dopen(7,filistin,'ro','f')
	  knocks=0
310	  read(7,*,end=320)ixko(knocks+1),iyko(knocks+1),izko(knocks+1)
	  knocks=knocks+1
	  go to 310
320	endif
c
	newmode=-1
	write(*,'(1x,a,$)')'Output file data mode (/ for same'//
     &	    ' as first input file): '
	read(*,*)newmode
c
c   find out if root beer float or what
c
	fraczero=0.
	ifmean=0
	write(*,'(1x,a,$)')'0 for no floating, or 1 or 2 to float'//
     &	    ' sections to common range or mean: '
	read(*,*)iffloat
	if(iffloat.gt.1)ifmean=1
c	  
c	  renumber sections starting at zero
c	  
	write(*,'(1x,a,$)')'1 to renumber sections in output file(s)'
     &	    //' sequentially from zero, 0 not to: '
	read(5,*)ifrenumb
c	  
c	  see if replacing data into input stack is an option
c	  
	ifrewrite=0
	iunitout=2
	instat='RO'
	if(minxinc.eq.0.and.maxxinc.eq.0.and.minyinc.eq.0.and.
     &	    maxyinc.eq.0.and.nfilein.eq.1.and.nfileout.eq.1.and.
     &	    newmode.eq.-1.and.filin(1).eq.filout(1).and.ifknock.eq.0)
     &	    then
	  call read_pl_or_header(pifilin(1),filin(1),extrain,maxextra,
     &	      ixpclist,iypclist,izpclist,npclist,limpcl)
	  call fill_listz(izpclist,npclist,listz,nlistz)
c	    
c	    make sure each section in piece list is included in the input list
c
	  matchz=0
	  if(nlistz.le.nlist(1))then
	    matchz=1
	    i=1
	    do while (i.le.nlistz.and.matchz.eq.1)
	      isinlist=0
	      j=1
	      do while (j.le.nlist(1).and.isinlist.eq.0)
		if(inlist(j).eq.listz(i))isinlist=1
		j=j+1
	      enddo
	      matchz=isinlist
	      i=i+1
	    enddo
	  endif
c
	  if(matchz.eq.1)then
	    write(*,'(1x,a,/,a,$)')
     &		'It looks like you are just scaling the file.',
     &		' Enter 1 to replace input file with scaled data,'//
     &		' 0 not to (BE CAREFUL): '
	    read(*,*)ifrewrite
	    if(ifrewrite.ne.0)then
	      iunitout=1
	      instat='OLD'
	    endif
	  endif
	endif
c
	if(iffloat.ne.0)then
c   if means, need to read all sections to get means!
c   find the minimum of the ratio (dmean-dmin)/(dmax-dmin)
	  floatxt=', floated to range'
	  if(fraczero.ne.0.)
     &        write(trunctxt,'(a,f6.3)')', truncated by',fraczero
	  if(ifmean.ne.0)floatxt=', floated to means'
	  zmin=1.e10
	  zmax=-1.e10
	  do ifil=1,nfilein
	    CALL IMOPEN(1,FILIN(ifil),'RO')
	    CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C	      
	    IF ((NX*NY.GT.idim*idim)) call exitError(
     &          'INPUT IMAGE TOO LARGE FOR ARRAY')
	    call read_pl_or_header(pifilin(ifil),filin(ifil),extrain,
     &		maxextra, ixpclist,iypclist,izpclist,npclist,limpcl)
	    do ilis=1,nlist(ifil)
	      indsec=ilis+listind(ifil)-1
	      nsecred=inlist(indsec)
	      dmin2=1.e10
	      dmax2=-1.e10
	      sum=0.
	      sumsq=0.
	      nsum=0
	      do ipc=1,npclist
		if(izpclist(ipc).eq.nsecred .and.
     &		    ((minxinc.eq.0.and.maxxinc.eq.0) .or.
     &		    (max(ixpclist(ipc),minxinc).le.
     &		    min(ixpclist(ipc)+nx-1,maxxinc))) .and.
     &		    ((minyinc.eq.0.and.maxyinc.eq.0) .or.
     &		    (max(iypclist(ipc),minyinc).le.
     &		    min(iypclist(ipc)+ny-1,maxyinc))) .and.
     &		    notknock(ixpclist(ipc),iypclist(ipc),nsecred,nx,ny,
     &		    ixko,iyko,izko,knocks)) then
		  call imposn(1,ipc-1,0)
		  call irdsec(1,array,*99)
		  if(ifmean.eq.0)then
		    do i=1,nx*ny
		      den=array(i)
		      dmin2=min(dmin2,den)
		      dmax2=max(dmax2,den)
		    enddo
		  else
		    do iy=0,ny-1
		      smtm=0.
		      smtmsq=0.
		      ibas=iy*nx
		      do i=1+ibas,nx+ibas
			den=array(i)
			smtm=smtm+den
			smtmsq=smtmsq+den**2
			dmin2=min(dmin2,den)
			dmax2=max(dmax2,den)
		      enddo
		      sum=sum+smtm
		      sumsq=sumsq+smtmsq
		    enddo
		    nsum=nsum+nx*ny
		  endif
		endif
	      enddo
	      dminsec(indsec)=dmin2
	      dmaxsec(indsec)=dmax2
	      if(ifmean.ne.0.and.nsum.gt.0)then
		avg=sum/nsum
		sd=sqrt((sumsq-sum**2/nsum)/(nsum-1))
		avgsec(indsec)=avg
		sdsec(indsec)=sd
		zmin=min(zmin,(dmin2-avg)/sd)
		zmax=max(zmax,(dmax2-avg)/sd)
	      endif
	    enddo
	    call imclose(1)
	  enddo
	endif
c
c   start looping over input images
c
	call time(tim)
	call date(dat)
	isec=1
	isecout=1
	ifileout=1
	ipcout=0
	do ifil=1,nfilein
	  CALL IMOPEN(1,FILIN(ifil),instat)
	  CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMININ,DMAXIN,DMEANIN)
          call irtsiz(1,nxyz,mxyz,nxyzst)
          call irtcel(1,cell)
	  call read_pl_or_header(pifilin(ifil),filin(ifil),extrain,
     &	      maxextra, ixpclist,iypclist,izpclist,npclist,limpcl)
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
	    endif
	  endif
C   
          IF ((NX*NY.GT.idim*idim)) call exitError(
     &        'INPUT IMAGE TOO LARGE FOR ARRAY')
c	    get each section in input file
	  nx3=nx
	  ny3=ny
	  do ilis=1,nlist(ifil)
	    indsec=ilis+listind(ifil)-1
	    nsecred=inlist(indsec)
	    ifanyout=0
	    do ipc=1,npclist
	      if(izpclist(ipc).eq.nsecred .and.
     &		  ((minxinc.eq.0.and.maxxinc.eq.0) .or.
     &		  (max(ixpclist(ipc),minxinc).le.
     &		  min(ixpclist(ipc)+nx-1,maxxinc))) .and.
     &		  ((minyinc.eq.0.and.maxyinc.eq.0) .or.
     &		  (max(iypclist(ipc),minyinc).le.
     &		  min(iypclist(ipc)+ny-1,maxyinc))).and.
     &		  notknock(ixpclist(ipc),iypclist(ipc),nsecred,nx,ny,
     &		  ixko,iyko,izko,knocks)) then
		call imposn(1,ipc-1,0)
		call irdsec(1,array,*99)
c		  set output characteristics from first section
		if(isec.eq.1.and.newmode.lt.0)newmode=mode
		npix=nx3*ny3
c   
c		  calculate new min and max after rescaling under various
c		  possibilities
c
		optin=optmax(mode+1)
		optout=optmax(newmode+1)
c	      
c		  set bottom of input range to 0 unless mode 1 or 2; set bottom
c		  of output range to 0 unless not changing modes
c
		bottomin=0.
		if(dminin.lt.0..and.(mode.eq.1.or.mode.eq.2))
     &		    bottomin=-optin
		bottomout=0.
		if(mode.eq.newmode)bottomout=bottomin
		rescale=.false.
		if(iffloat.eq.0)then
c
c		    get new min and max quickly
c		    
		  tmpmin=1.e10
		  tmpmax=-1.e10
		  do i=1,npix
		    val=array(i)
		    if(val.lt.tmpmin)tmpmin=val
		    if(val.gt.tmpmax)tmpmax=val
		  enddo
		  dmin2=tmpmin
		  dmax2=tmpmax
		endif
c		    
c		  for no float: if mode = 2, no rescale
c		  
		if(iffloat.eq.0.and.newmode.ne.2)then
c
c		      if within proper input range, rescale from input range to
c		      output range only if mode is changing
c		  
		  rescale=mode.ne.newmode
		  if(tmpmin.ge.bottomin.and.tmpmax.le.optin)then
		    dmin2=(tmpmin-bottomin)*(optout-bottomout)/
     &			(optin-bottomin)+bottomout
		    dmax2=(tmpmax-bottomin)*(optout-bottomout)/
     &			(optin-bottomin)+bottomout
		  elseif(rescale)then
c		      :if outside proper range, tell user to start over
                    call exitError('Input data outside expected range. '//
     &			'Start over, specifying float to range')
		  endif
		elseif(iffloat.ne.0)then
c		    If floating: scale to a dmin2 that will knock out fraczero
c		    of the range after truncation to zero
		  dmin2=-optout*fraczero/(1.-fraczero)
		  rescale=.true.
		  tmpmin=dminsec(indsec)
		  tmpmax=dmaxsec(indsec)
		  if(ifmean.eq.0)then
c		      :float to range, new dmax2 is the max of the range
		    dmax2=optout
		  else
c		      :float to mean, it's very hairy, first need mean again
		    zminsec=(tmpmin-avgsec(indsec))/sdsec(indsec)
		    zmaxsec=(tmpmax-avgsec(indsec))/sdsec(indsec)
		    dmin2=(zminsec-zmin)*optout/(zmax-zmin)
		    dmax2=(zmaxsec-zmin)*optout/(zmax-zmin)
		    dmin2=max(0.,dmin2)
c		      but in case of problems, just limit dmax2 to the range
		    dmax2=min(dmax2,optout)
		  endif
		endif
		dmean2=0.
c		  set up minimum value to output based on mode
		if(newmode.eq.1)then
		  denoutmin=-32767
		elseif(newmode.eq.2)then
		  denoutmin=-1.e30
		  optout=1.e30
		else
		  denoutmin=0.
		endif
c
		if(rescale)then
c		    if scaling, set up equation, scale and compute new mean
		  sclfac=(dmax2-dmin2)/(tmpmax-tmpmin)
		  const=dmin2-sclfac*tmpmin
		  dmin2=1.e20
		  dmax2=-1.e20
		  do i=1,npix
		    den=sclfac*array(i)+const
		    if(den.lt.denoutmin)then
c		      ntrunclo=ntrunclo+1
		      den=denoutmin
		    elseif(den.gt.optout)then
c		      ntrunchi=ntrunchi+1
		      den=optout
		    endif
		    array(i)=den
		    dmean2=dmean2+den
		    dmin2=min(dmin2,den)
		    dmax2=max(dmax2,den)
		  enddo
		else
c		    if not scaling, just need new mean
		  do i=1,npix
		    dmean2=dmean2+array(i)
		  enddo
		endif
		dmean2=dmean2/npix
		print *,'frame',isec-1,': min&max before and after, mean:'
		write(*,'(5f10.2)')tmpmin,tmpmax,dmin2,dmax2,dmean2
c		  see if need to open an output file
80		if(ipcout.eq.0)then
C   
C		    Create output file, transfer header from currently open
c		    file, fix it enough to get going
		  if(ifrewrite.eq.0)then
		    CALL IMOPEN(2,FILOUT(ifileout),'NEW')
		    call itrhdr(2,1)
		    call ialmod(2,newmode)
c		
c		      adjust extra header information if current file has it
c		
		    nbsymout=0
		    if(nbsymin.gt.0)then
		      call checklist(ixpclist,npclist,1,nx,minxpiece,
     &			  nxpieces, nxoverlap)
		      call checklist(iypclist,npclist,1,ny,minypiece,
     &			  nypieces, nyoverlap)
		      nbytexout=nbytexin
		      nbsymout=nsecout(ifileout)*nbytexout*nxpieces
     &			  *nypieces
		      nbsymout=min(maxextra,nbsymout)
		      if(nbsymout.gt.0)then
			call ialnbsym(2,nbsymout)
			call imposn(2,0,0)
		      endif
		      indxout=0
		    endif
		  endif
c
c 7/7/00 CER: remove the encodes
c
C                 ENCODE(80,301,TITLE)floatxt,dat,tim  
                  write(titlech,301) floatxt,dat,tim
301		  FORMAT('EDMONT: Images transferred',a18,t57,a9,2x,a8)
                  read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
		  DMAX=-100000.
		  DMIN=100000.
		  DMEAN=0.
		endif
C   
		DMIN = MIN(DMIN,DMIN2)
		DMAX = MAX(DMAX,DMAX2)
		DMEAN = DMEAN + DMEAN2
C   
		if(ifrewrite.ne.0)call imposn(1,ipc-1,0)
		call iwrsec(iunitout,array)
		isec=isec+1
		ipcout=ipcout+1
		ixpcout(ipcout)=ixpclist(ipc)
		iypcout(ipcout)=iypclist(ipc)
		izpcout(ipcout)=izpclist(ipc)
		if(ifrenumb.ne.0) izpcout(ipcout) = isecout-1
		ifanyout=ifanyout+1
c	      
c		  transfer extra header bytes if present
c
		if(nbsymout.ne.0.and.indxout.lt.nbsymout)then
		  nbcopy=min(nbytexout,nbytexin,nbsymin)
		  nbclear=nbytexout-nbcopy
		  do i=1,nbcopy
		    indxout=indxout+1
		    extraout(indxout)=extrain((ipc-1)*nbytexin+i)
		  enddo
		  do i=1,nbclear
		    indxout=indxout+1
		    extraout(indxout)=0
		  enddo
		  if(ifrenumb.ne.0.and.nbcopy.ge.6)then
		    temp=isecout-1
		    ind=indxout+5-nbytexout
		    if(mod(iflagxin,2).ne.0)ind=ind+2
		    call move(extraout(ind),temp,2)
		  endif
		endif
	      endif
	    enddo
	    if(ifanyout.gt.0)isecout=isecout+1
c   see if need to close stack file
	    if(isecout.gt.nsecout(ifileout) .or.
     &		(ifil.eq.nfilein.and.ilis.eq.nlist(ifil)))then
c		set new size, keep old nxyzst
	      if(ifrewrite.eq.0)then
		if(pifilout(ifileout).ne.' ')then
		  call dopen(3,pifilout(ifileout),'new','f')
		  write(3,'(3i6)')(ixpcout(i),iypcout(i),izpcout(i),i=1,
     &		      ipcout)
		  close(3)
		endif
		NXYZ2(1)=NX3
		NXYZ2(2)=NY3
		NXYZ2(3)=ipcout
		call ialsiz(2,nxyz2,nxyzst)
c		  if mxyz=nxyz, keep this relationship
		if(mxyz(1).eq.nx.and.mxyz(2).eq.ny.and.mxyz(3).eq.nz)
     &		    then
		  MXYZ2(1)=NX3
		  MXYZ2(2)=NY3
		  MXYZ2(3)=ipcout
		  call ialsam(2,mxyz2)
		endif
c		  keep delta the same by scaling cell size from change in mxyz
		do i=1,3
		  CELL2(i)=mxyz2(i)*(cell(i)/mxyz(i))
		  CELL2(i+3)=90.
		enddo
		CALL IALCEL(2,CELL2)
	      endif
	      if(nbsymout.gt.0)call ialsym(2,nbsymout,extraout)
	      DMEAN=DMEAN/ipcout
C   
	      CALL IWRHDR(iunitout,TITLE,1,DMIN,DMAX,DMEAN)
	      CALL IMCLOSE(iunitout)
	      ipcout=0
	      isecout=1
	      ifileout=ifileout+1
	    endif
	  enddo
	  if(ifrewrite.eq.0)call imclose(1)
	enddo
C   
        call exit(0)
99	call exitError('END OF IMAGE WHILE READING')
	END


	logical function notknock(ixpc,iypc,izpc,nx,ny, ixko,iyko,izko,
     &	    knocks)
	integer*4 ixko(*),iyko(*),izko(*)
	notknock=.false.
	do i=1,knocks
	  if(izko(i).eq.izpc.and.ixpc.le.ixko(i).and.ixpc+nx.gt.ixko(i)
     &	      .and.iypc.le.iyko(i).and.iypc+ny.gt.iyko(i))return
	enddo
	notknock=.true.
	return
	end

	subroutine read_pl_or_header(pifilin,filin,extrain,maxextra,
     &	      ixpclist,iypclist,izpclist,npclist,limpcl)
	character*(*) pifilin,filin
	integer*1 extrain(*)
	integer*4 ixpclist(*),iypclist(*),izpclist(*)
	integer*4 nxyz(3),mxyz(3)
c	  
	if(pifilin.ne.' ')then
	  call read_piece_list(pifilin, ixpclist,iypclist,izpclist,
     &	      npclist)
	else
	  call ialprt(.false.)
	  call imopen(4,filin,'RO')
	  CALL IRDHDR(4,NXYZ,MXYZ,MODE,DMININ,DMAXIN,DMEANIN)
c	    
c	    get extra header information if any
c
	  call irtnbsym(4,nbsymin)
	  if(nbsymin.gt.0)then
	    if(nbsymin.gt.maxextra)then
	      print *,'Cannot proceed because no piece list file was',
     &		  ' given for input file ',filin,
     &		  ' and the extra header data are too large for the array'
	      call exit(1)
	    else
	      call irtsym(1,nbsymin,extrain)
	      call irtsymtyp(1,nbytexin,iflagxin)
	      call get_extra_header_pieces(extrain,nbsymin,nbytexin,
     &		  iflagxin,nxyz(3),ixpclist,iypclist,izpclist,npclist,
     &		  limpcl)
	    endif
	  endif
	  if (nbsymin.eq.0.or.npclist.eq.0)then
	    print *,'Cannot proceed because no piece list file was',
     &		' given for input file ',filin,
     &		' and the header does not contain piece coordinates'
	    call exit(1)
	  endif
	endif
	return
	end


******************XFSIMPLEX*******************************************
*   
*	  This program searches for the best general linear transform between
*	  a pair of images by varying either the six formal parameters of the
*	  transform, the six "semi-natural" parameters underlying such a
c	  transform, or restricted subsets of those semi-natural parameters.
*
*	  These semi-natural parameters are, in the order in which the program
c	  will consider them:
*	  .  Delta X
*	  .  Delta Y
*	  .  Global rotation (average rotation of X & Y axes)
*	  .  Global magnification (average stretch of X & Y axes)
*	  .  Difference between stretch along Y- & X-axis
*	  .  Difference between rotation of Y- & X-axis
*	  In the fifth line of input, one enters either zero to search for
c	  formal parameters, or a number specifying how many of the natural
c	  parameters are to be varied.  If one selects 2, only Delta X and
c	  Delta Y will be varied; if one selects 4, global rotation and
c	  magnification will be varied also. At the end, the program outputs a
c	  six-parameter transformation (the 2x2 A matrix and DX and DY) in the
c	  standard format.
c	  
c	  Because the search method used by this program works iteratively from
c	  a given starting point, it is unlikely to find the proper alignment
c	  if it requires a large displacement.	To overcome this problem,
c	  the program can be given an initial transformation to work from.
c	  This allows a large displacement to be found by cross-correlation and
c	  passed to this program.
c
*	  To find the best fit between images, the search can optimize either
c	  a simple point-by-point difference between the images, or a measure
c	  of the distance between points of similar intensities in the
c	  images.  The resulting transformation is applied to the second image
c	  to align it to the first.
c
c	  The search uses a so-called simplex minimization routine which starts
c	  searching with an initial step size and refines the step size near a
c	  minimum.  It terminates the minimization when either 1) the most
c	  recent points under consideration gave difference measures all
c	  within a certain fractional tolerance of the point with the minimum
c	  measure; or 2) the most recent points had transformation parameters
c	  all within a certain tolerance of the point with the minimum measure.
c	  The latter tolerances are expressed as fractions of the following
c	  basic step sizes: 1 for delta X and Y; 0.025 for the 4 parameters of
c	  the transformation matrix, if using formal parameters; or 2 degrees
c	  for global rotation and differences between X and Y rotations, and
c	  0.025 for global magnification and difference between X and Y
c	  magnifications, if using semi-natural parameters.
c	  
c	  By default, the program will perform an initial minimization with
c	  a coarse tolerance for termination, then it will restart the
c	  minimization at the best point, and terminate with a finer tolerance.
c	  If the overall alignment method involves two stages, coarse and
c	  fine, then you should omit the initial minimization by specifying
c	  tolerances of zero for it.
*   
c	  The program prompts for all inputs, but all of the parameters have
c	  defaults which may be selected with , or / (the values in [] below
c	  and contained in [] in the prompts)
c
c	  ***lines 1-4
c	  first image file
c	  second image file
c	  data file into which to place the best fitting transformation
c	  name of file file with starting transformation, or Return if none
c
c	  *** line 5 (6 values):
c	  Fractional tolerances in the difference/distance measure and in the
c	  .  transformation parameter values, to allow termination of final or
c	  .  only minimization [.0005 and 0.02, or .001 and 0.04 for
c	  .  images no bigger than 128 by 128]
c	  Fractional tolerances in the difference/distance measure and in the
c	  .  transformation parameter values, to allow termination of initial
c	  .  minimization [.005 and 0.2].  Enter 0,0 to skip initial search.
c	  Factor to apply to basic step sizes to get initial step sizes [2]
c	  1 for trial-by-trial output, 2 for output of trials that yield new
c	  .  minima only.
c
c	  ***line 6
c	  0 for search on formal parameters, or # of natural parameters to
c	  .  vary [0]
c
c	  ***lines 7-10
c	  Fraction of images to ignore at edges [0.05]; or number of pixels
c	  .  if the number entered is 1 or greater
c	  float images to have same range (0) or same mean and S.D. (1), or
c	  .  do not float images (-1) [1]
c	  # of times to reduce images by factor of 2 in x and y [1]
c	  use difference (0) or distance (1) measure [0]
c
c	  If difference measure is chosen, one more line of input:
c
c	  ***line 11
c	  1 to use bilinear interpolation during the search [0]
c
c	  If distance measure is chosen, 5 more lines of input:
c
c	  *** lines 11-14
c	  distance to search to eliminate redundant points with similar
c	  .  densities from comparison [default depends on image size after
c	  .  reduction, if any: 0 for # of pixels < 240*180, 1 if # of pixels
c	  .  between 240*180 and 480*360, 2 if # of pixels > 480*360
c	  distance to search for matching densities [4 if reduce by 2, 5 if
c	  .  not]
c	  maximum density difference constituting a match [0.05]
c	  Number of ranges of densities to make comparisons with [2]
c
c	  *** line 15
c	  lower and upper PERCENTILES for these ranges.  The default is 0,8,92,
c	  . 100 for small images.  This means that the darkest 8% and brightest
c	  .  8% of pixels will be used for comparison (minus ones eliminated
c	  .  because of redundancy).  The default depends on image size after
c	  .  reduction, if any; the range is scaled from 8% down to 5% as image
c	  .  size increases from 320*240 to 640*480
c
c	  These defaults are based on limited experimentation.  When the
c	  distance measure is used, the defaults are set in an attempt to
c	  limit the number of "points for comparison" to several thousand.  If
c	  there are more than about "5000 points for comparison", you should
c	  depart from the defaults in order to reduce this number.
c
*	  David Mastronarde 4/5/91 (adapted from XFSEARCH)
*   
************************************************************************
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.5  2003/12/24 19:07:27  mast
c	  Moved amoeba subroutine to a library
c	
c	  Revision 3.4  2002/08/20 19:23:48  mast
c	  Didn't change both calls to iclavgsd
c	
c	  Revision 3.3  2002/08/18 23:13:05  mast
c	  Changed to call iclavgsd in library
c	
c	  Revision 3.2  2002/05/20 15:47:33  mast
c	  Made the DIFF function put out a very high value when the number of
c	  pixels evaluated falls below 1% of total pixels, to keep it from
c	  running away into impossible shifts.  Also increased dimensions of
c	  input array to allow 4Kx4K images.
c	
c	  Revision 3.1  2002/04/29 16:18:37  mast
c	  Added test to keep it from failing when images match perfectly
c	
*   
	parameter (ixlim=2100,iylim=2100,idima=4100*4100)
	parameter (isub=ixlim*iylim/3,limspir=1000)
	parameter (isubp1=isub+1, isubt2=2*isub+1, isubt25=2.5*isub+2)
	COMMON //NX,NY,NZ
	DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),ARRAY(idima)
	EQUIVALENCE (NX,NXYZ)
C	  
	CHARACTER*80 FILIN1,FILIN2,DATOUT,xfinfile
C	  
	DATA NXYZST/0,0,0/
	real*4 a(6),da(6),amat(2,2),anat(6),danat(6),acall(6)
	real*4 pp(7,7),yy(7),ptmp(6),ptol(6)
c	  if doing formal params, the a(i) are DX, DY, a11,a12,a21, and a22
c	  for natural paramas, the a(i) are DX and DY, Global rotation, global
c	  stretch, difference between Y&X-axis stretch, and difference
c	  between Y and X axis rotation.
c	  the da are the step sizes for the respective a's
	logical trace
c	  starting values of a, and da, for formal search
	data a/0.,0.,1.,0.,0.,1./
	data da/1.,1.,.025,.025,.025,.025/
c	  starting values of a and da for natural search
	data anat/0.,0.,0.,1.,0.,0./
	data danat/1.,1.,2.,.02,.02,2./
	real*8 tsum, tsumsq
c
	integer*4 ihist(0:1000),idxspir(limspir),idyspir(limspir)
	integer*2 ixcomp(isub),iycomp(isub)
	real*4 distspir(limspir),denlo(isub),denhi(isub),range(10,2)
     &	    ,ranlo(10),ranhi(10),percen(10,2),pclo(10),pchi(10)
	equivalence (ranlo(1),range(1,1)),(ranhi(1),range(1,2))
	equivalence (pclo(1),percen(1,1)),(pchi(1),percen(1,2))
c	  array for second image if doing diffs, using same storage as
c	  all the arrays for doing distances
	real*4 brray(ixlim*iylim)
	external func
	equivalence (brray(1),denlo(1)),(brray(isubp1),denhi(1))
     &	    ,(brray(isubt2),ixcomp(1)),(brray(isubt25),iycomp(1))
	common /funcom/ array,brray,idxspir,idyspir,distspir,mattx,
     &	    matty, interp,natural,ncompare,nspiral,ifdist,iftrace,rds,
     &	    ntrial, deltmin,ivend,acall
C	  
c	  default values for potentially input parameters
	data ftol1/5.e-4/,ptol1/.02/,ftol2/5.e-3/,ptol2/.2/
	data delfac/2/
	data iflmean/1/,ifreduce/1/
	data idredun/0/,radius/4/,difflim/0.05/,nrange/2/
	data pclo/0.,92.,8*0./,pchi/8.,100.,8*0./
c	  
c	  default values for parameters in common
c	  
	iftrace=0
	fracmatt=0.05
	ifdist=0
	natural=0
	interp=0
c	  
	write(*,'(1x,a,$)')'First image file: '
	READ (5,40)FILIN1
	write(*,'(1x,a,$)')'Second image file: '
	READ (5,40)FILIN2
	write(*,'(1x,a,$)')'Transform output file: '
	READ (5,40)DATOUT
	write(*,'(1x,a,$)')
     &	    'File with starting transform, or Return if none: '
	READ (5,40)xfinfile
40	FORMAT (A)
c
c	  open file now to get sizes and try to adjust defaults
c
	call ialprt(.false.)
	CALL IMOPEN(1,FILIN1,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
c
	if(nx.le.128.and.ny.le.128)then
	  ftol1=2.*ftol1
	  ptol1=2.*ptol1
	endif
c
	write(*,'(1x,a,/,a,/,a,/,a,f6.4,f5.2,f7.4,f5.2,f4.1,i2,a,$)')
     &	    'Enter fractional tolerances in difference measure and'//
     &	    ' in parameter values','   for terminating final search,'//
     &	    ' corresponding tolerances for initial search',
     &	    '   (or 0,0 for only one search), factor for initial'//
     &	    ' step size, and 1 or 2 for','    trace output ['
     &	    ,ftol1,ptol1,ftol2,ptol2,delfac,iftrace,']: '
	read(5,*)ftol1,ptol1,ftol2,ptol2,delfac,iftrace
	trace=iftrace.gt.0
C	  
13	write(*,'(1x,a,i1,a,$)')'0 to search 6 formal variables,'//
     &	    ' or # of natural variables to search [',natural,']: '
	read(*,*)natural
	if(natural.lt.0.or.natural.gt.6)go to 13
	ivst=1
	if(natural.eq.0)then			!all six formal params
	  ivend=6
	else
	  ivend=natural				!or selected # of natural
	  do i=1,6
	    a(i)=anat(i)
	    da(i)=danat(i)
	  enddo
	endif
c	  
c	  if reading in a transform, get it and convert to natural if necessary
c
	if(xfinfile.ne.' ')then
	  call dopen(1,xfinfile,'old','f')
	  read(1,*)((amat(ii,jj),jj=1,2),ii=1,2),a(1),a(2)
	  if(natural.eq.0)then
	    a(3)=amat(1,1)
	    a(4)=amat(1,2)
	    a(5)=amat(2,1)
	    a(6)=amat(2,2)
	  else
	    call amat_to_rotmag(amat,a(3),a(6),a(4),a(5))
	  endif
	endif
c
	do i=1,6
	  acall(i)=a(i)
	enddo
c	  
	write(*,'(1x,a,f4.2,a,$)')'edge fraction to ignore ['
     &	    ,fracmatt,']: '
	read(*,*)fracmatt
c
	write(*,'(1x,a,i1,a,$)')'0 to float to range,'//
     &	    ' 1 to float to mean&sd, -1 no float [',iflmean,']: '
	read(*,*)iflmean
c
	write(*,'(1x,a,i1,a,$)')'# of times to reduce image by 2 ['
     &	    ,ifreduce,']: '
	read(*,*)ifreduce
c
	write(*,'(1x,a,i1,a,$)')'0 for difference,'//
     &	    ' 1 for distance measure [',ifdist,']: '
	read(*,*)ifdist
c
	if(ifdist.eq.0)then
	  write(*,'(1x,a,i1,a,$)')'1 to use interpolation ['
     &	      ,interp,']: '
	  read(*,*)interp
	else
c	    
c	    change defaults based on image size and reduction by 2
c
	  npixel=nx*ny
	  if(ifreduce.gt.0)then
	    npixel=npixel/(4**ifreduce)
	    radius=4.
	  else
	    radius=5.
	  endif
c
	  if(npixel.gt.480*360)then
	    idredun=2
	  elseif(npixel.gt.240*180)then
	    idredun=1
	  else
	    idredun=0
	  endif
c	    
c	    run percentile range from 8 down to 5 as go from 320x240 to 640x480
c	    
	  pcrange=min(8.,max(5.,8.-3.*(npixel-320*240)/
     &	      (640*480-320*240)))
	  pchi(1)=pcrange
	  pclo(2)=100.-pcrange
c	    
	  write(*,'(1x,a,i1,a,$)')'distance to search for and'//
     &	      ' eliminate redundancy, 0 not to [',idredun,']: '
	  read(*,*)idredun
c	    
c	    get density window and search radius
	  write(*,'(1x,a,f3.1,a,$)')'radius to search for match ['
     &	      ,radius,']: '
	  read(*,*)radius
c	    
	  write(*,'(1x,a,f4.2,a,$)')'max density difference for match'
     &	      //' as fraction of range [',difflim,']: '
	  read(*,*)difflim
c	    
	  write(*,'(1x,a,i1,a,$)')'number of percentile ranges ['
     &	      ,nrange,']: '
	  read(*,*)nrange
c	    
c	    get percentile ranges
	  write(*,'(1x,a,6f6.1)')'lower and upper percentiles'//
     &	      ' in ranges - defaults:',(pclo(i),pchi(i),i=1,nrange)
	  read(*,*)(pclo(i),pchi(i),i=1,nrange)
	endif
C	  
	call ialprt(.true.)
	call imclose(1)
	CALL IMOPEN(1,FILIN1,'RO')
C	  NOTE: ABSOLUTELY NEED TO READ HEADER AGAIN
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C	  
	CALL IMOPEN(2,FILIN2,'RO')
	CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
	nxorig=nx
	nyorig=ny
C	  
C	  Open data file for transform
C	  
	CALL DOPEN(4,DATOUT,'NEW','F')
C	  
	IF (NX*NY.GT.idima) GOTO 94
C	  just get second section to work with 
	call irdsec(2,array,*99)
c	  
	rds=1.
c	  
c	  Reduce array sizes by requested amount before doing searches.
c	  Notice assumption of no trip trough loop if ifreduce = 0
	do nreduce=1,ifreduce
	  CALL REDUCE2D(ARRAY,NX,NY)
C	    
	  rds=2.*rds
	  da(1)=da(1)/2.
	  da(2)=da(2)/2.
	  a(1)=a(1)/2.
	  a(2)=a(2)/2.
	enddo
c	  check reduced size against size of B
	if (nx*ny.gt.ixlim*iylim) goto 94

c	  Just deal with points in central portion
	if(fracmatt.ge.1.)then
	  mattx=nint(fracmatt/rds)
	  matty=mattx
	else
	  mattx=max(0,NINT(FLOAT(NX)*fracmatt))
	  matty=max(0,NINT(FLOAT(NY)*fracmatt))
	endif	  
	NX1 = 1+mattx 
	NX2 = NX-mattx
	NY1 = 1+matty 
	NY2 = NY-matty
C	  
	CALL ICLavgsd(ARRAY,nx,ny,NX1,NX2,NY1,NY2
     &	    ,DMIN2,DMAX2,tsum,tsumsq,DMEAN2,sd2)
C	  
	if(ifdist.eq.0)then
c	    if doing simple difference measure, move array into brray
	  do ixy=1,ny*nx
	    brray(ixy)=array(ixy)
	  enddo
	else
c	    if doing distance measure, make histogram of intensities
	  do i=0,1000
	    ihist(i)=0
	  enddo
	  histscal=1000./(dmax2-dmin2)
	  DO J=NY1,NY2
	    ibase=nx*(j-1)
	    DO I=NX1,NX2
C		
	      ind=(ARRAY(I+ibase)-dmin2)*histscal
	      ihist(ind)=ihist(ind)+1
C		
	    ENDDO
	  ENDDO
c	    convert to cumulative histogram
	  DO i=1,1000
	    ihist(i)=ihist(i)+ihist(i-1)
	  enddo
c	    find density corresponding to each percentile limit
	  do iran=1,nrange
	    do lohi=1,2
	      ncrit=(nx2+1-nx1)*(ny2+1-ny1)*percen(iran,lohi)/100.
	      i=0
	      do while(i.le.1000.and.ncrit.gt.ihist(i))
		i=i+1
	      enddo
	      range(iran,lohi)=(i/histscal)+dmin2
	    enddo
	  enddo
c	    find all points in central part of array within those density
c	    limits
	  ncompare=0
	  do iy=ny1,ny2
	    ibase=nx*(iy-1)
	    do ix=nx1,nx2
	      val=array(ix+ibase)
	      do iran=1,nrange
		if(val.ge.ranlo(iran).and.val.le.ranhi(iran))then
c		    redundancy reduction: look for previous nearby points in
c		    list
		  if(idredun.gt.0)then
		    ixm=ix-idredun
		    iym=iy-idredun
		    ixp=ix+idredun
		    do ic=ncompare,1,-1
		      ixcm=ixcomp(ic)
		      iycm=iycomp(ic)
c			if point in list is nearby and within same density
c			range, skip this one
		      if(ixcm.ge.ixm.and.ixcm.le.ixp.and.iycm.ge.iym
     &			  .and.denlo(ic).ge.ranlo(iran).and.denlo(ic)
     &			  .le.ranhi(iran))go to 80
c			if gotten back far enough on list, take this point
		      if(iycm.lt.iym.or.(iycm.eq.iym.and.ixcm.lt.ixm))
     &			  go to 78
		    enddo
		  endif
78		  ncompare=ncompare+1
		  ixcomp(ncompare)=ix
		  iycomp(ncompare)=iy
c		    just store density temporarily here
		  denlo(ncompare)=val
		  go to 80
		endif
	      enddo
80	    enddo
	  enddo
	  print *,ncompare,' points for comparison'
	endif
C	  Now get first section
	nx=nxorig
	ny=nyorig
	call irdsec(1,array,*99)
c	  
c	  Reduce array sizes by requested amount before doing searches
c	  
	do nreduce=1,ifreduce
	  CALL REDUCE2D(ARRAY,NX,NY)
	enddo
C	  
	CALL ICLavgsd(ARRAY,nx,ny,NX1,NX2,NY1,NY2
     &	    ,DMIN1,DMAX1,tsum,tsumsq,DMEAN1,sd1)
C	  
c	  get scale factor for floating second array densities to match that
c	  of first
	scale=1.
	dadd=0.
	if(iflmean.eq.0)then
	  SCALE=(DMAX1-dmin1)/(DMAX2 - dmin2)
	  dadd=dmin1-scale*dmin2
	elseif(iflmean.gt.0)then
	  scale=sd1/sd2
	  dadd=dmean1-scale*dmean2
	endif
	if(ifdist.eq.0)then
c	    for simple difference, rescale whole array
	  do ixy=1,nx*ny
	    brray(ixy)=SCALE*brray(ixy)+dadd
	  enddo
	  CALL DIFF(DELMIN,ARRAY,BRRAY,A,NX,NY,MATTX,MATTY
     &	      ,interp,natural)
	else
c	    otherwise, for distance,
c	    rescale list of densities and add lower and upper window
	  window=SCALE*0.5*difflim*(dmax2-dmin2)
	  do i=1,ncompare
	    valscl=scale*denlo(i)+dadd
	    denlo(i)=valscl-window
	    denhi(i)=valscl+window
	  enddo
c	    find points within search radius
	  limdxy=radius+1
	  nspiral=0
	  do idx=-limdxy,limdxy
	    do idy=-limdxy,limdxy
	      dstnc=sqrt(float(idx**2+idy**2))
	      if(dstnc.le.radius)then
		nspiral=nspiral+1
		distspir(nspiral)=dstnc
		idxspir(nspiral)=idx
		idyspir(nspiral)=idy
	      endif
	    enddo
	  enddo
c	    order them by distance
	  do i=1,nspiral
	    do j=i+1,nspiral
	      if(distspir(i).gt.distspir(j))then
		tmp=distspir(i)
		distspir(i)=distspir(j)
		distspir(j)=tmp
		itmp=idxspir(i)
		idxspir(i)=idxspir(j)
		idxspir(j)=itmp
		itmp=idyspir(i)
		idyspir(i)=idyspir(j)
		idyspir(j)=itmp
	      endif
	    enddo
	  enddo
C	    
	  CALL DIST(DELMIN,ARRAY,A,NX,NY,ixcomp,iycomp,
     &	      denlo, denhi,ncompare,idxspir,idyspir,distspir,nspiral
     &	      ,natural)
c	    
	endif
	ntrial=0
	deltmin=1.e30
c	  
c	  DNM 4/29/02: search fails if images match perfectly, so skip if so
c
	if (delmin.gt.0.) then
	  ptfac=ptol1
	  if(ftol2.gt.0.or.ptol2.gt.0)ptfac=ptol2
	  do j=1,ivend+1
	    do i=1,ivend
	      pp(j,i)=a(i)
	      if(j.gt.1.and.i.eq.j-1)pp(j,i)=a(i)+delfac*da(i)
	      ptmp(i)=pp(j,i)
	      ptol(i)=da(i)*ptfac
	    enddo
	    yy(j)=func(ptmp)
	  enddo
	  if(ftol2.gt.0.or.ptol2.gt.0)then
	    call amoeba(pp,yy,7,7,ivend,ftol2,func,iter,ptol,jmin)
	    if(trace)print *,'restarting'
	    deltmin=1.e30
	    do i=1,ivend
	      pp(1,i)=pp(jmin,i)
	      ptol(i)=da(i)*ptol1
	    enddo
	    do j=1,ivend+1
	      do i=1,6
		pp(j,i)=pp(1,i)
		if(j.gt.1.and.i.eq.j-1)pp(j,i)=pp(1,i)+delfac*da(i)
		ptmp(i)=pp(j,i)
	      enddo
	      yy(j)=func(ptmp)
	    enddo
	  endif
	  call amoeba(pp,yy,7,7,ivend,ftol1,func,iter,ptol,jmin)
C	    
	  do i=1,ivend
	    a(i)=pp(jmin,i)
	  enddo
	endif
c
	PRINT *,' FINAL VALUES'
	write(*,72)ntrial,yy(jmin),(a(ii),ii=3,6),rds*a(1),rds*a(2)
72	format(i5,f14.0,4f10.5,2f10.3)
C	  
	if(natural.ne.0)then 
	  call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
	  WRITE(*,70)((amat(ii,jj),jj=1,2),ii=1,2),rds*a(1),rds*a(2)
	  WRITE(4,70)((amat(ii,jj),jj=1,2),ii=1,2),rds*a(1),rds*a(2)
	else
	  write(4,70)(a(ii),ii=3,6),rds*a(1),rds*a(2)
	endif
70	FORMAT(4f12.7,2f12.3)
C	  
	CALL IMCLOSE(1)
	CALL IMCLOSE(2)
C	  
	call exit(0)
C	  
94	PRINT *,'IMAGE TOO BIG.'
	call exit(1)
99	WRITE(6,400)
400	FORMAT(' END OF IMAGE WHILE READING')
	call exit(1)
	END
C	  
*********************************************************************
*	  
*	  FIND MISFIT OF IMAGES
*	  
*******************************************************************
C	  
	subroutine DIST(DELTA,ARRAY,A,NX,NY,
     &	    ixcomp,iycomp,denlo,denhi,ncompare,
     &	    idxspir,idyspir,distspir,nspiral,natural)
C	  
	DIMENSION ARRAY(nx,ny),A(6),amat(2,2)
	integer*2 ixcomp(*),iycomp(*)
	dimension denlo(*),denhi(*),idxspir(*),idyspir(*),distspir(*)
C	  
	XCEN=FLOAT(NX)*0.5+1
	YCEN=FLOAT(NY)*0.5+1
C	  
	DELTA = 0.
C	  
	xadd=xcen+a(1)+0.5
	yadd=ycen+a(2)+0.5
	if(natural.eq.0)then
	  amat(1,1)=a(3)
	  amat(1,2)=a(4)
	  amat(2,1)=a(5)
	  amat(2,2)=a(6)
	else
	  call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
	endif
c	print *,((amat(i,j),j=1,2),i=1,2)
	distmax=distspir(nspiral)+1.
	do icomp=1,ncompare
	  FJ=iycomp(icomp) - YCEN
	  FI=ixcomp(icomp) - XCEN
C	    
	  IX=amat(1,1)*FI + amat(1,2)*FJ + xadd
	  IY= amat(2,1)*FI + amat(2,2)*FJ + yadd
C	    
	  dstnc=distmax
	  critlo=denlo(icomp)
	  crithi=denhi(icomp)
	  do ispir=1,nspiral
	    ixa=min(nx,max(1,ix+idxspir(ispir)))
	    iya=min(ny,max(1,iy+idyspir(ispir)))
	    den1=array(ixa,iya)
	    if(den1.ge.critlo.and.den1.le.crithi)then
	      dstnc=distspir(ispir)
	      go to 40
	    endif
	  enddo
40	  DELTA=DELTA + dstnc
C	    
	ENDDO
C	  
c	delta=delta/ncompare
	RETURN
C	  
	END
C	  
	SUBROUTINE DIFF(DELTA,ARRAY,BRRAY,A,NX,NY,MATTX,MATTY
     &	    ,interp,natural)
C	  
	DIMENSION ARRAY(nx,ny),BRRAY(nx,ny),A(6),amat(2,2)
C	  
	real*4 deltalast/0./
	save deltalast

	DELTA=0.
	XCEN=FLOAT(NX)*0.5+1			!use +1 to be consistent
	YCEN=FLOAT(NY)*0.5+1			!with qdinterp usage
C	  
C	  
	NX1 = 1+mattx 
	NX2 = NX-mattx
	NY1 = 1+matty 
	NY2 = NY-matty
C	  
	if(natural.eq.0)then
	  amat(1,1)=a(3)
	  amat(1,2)=a(4)
	  amat(2,1)=a(5)
	  amat(2,2)=a(6)
	else
	  call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
	endif
c	print *,((amat(i,j),j=1,2),i=1,2)
	npix=0
	if(interp.eq.0)then
	  xadd=xcen+a(1)+0.5			!the 0.5 here gets nearest int
	  yadd=ycen+a(2)+0.5
	  DO J=NY1,NY2
	    FJ=FLOAT(J) - YCEN
C	      
	    DO I=NX1,NX2
	      FI=FLOAT(I) - XCEN
C	      
	      IX=amat(1,1)*FI + amat(1,2)*FJ + xadd
	      IY= amat(2,1)*FI + amat(2,2)*FJ + yadd
C		
	      if(ix.ge.1.and.ix.le.nx.and.iy.ge.1.and.iy.le.ny)then
C	      
		delta = delta + abs(array(ix,iy)- brray(I,J))
		npix=npix+1
	      endif
C		
	    ENDDO
C	    
	  ENDDO

	else
	  xadd=xcen+a(1)
	  yadd=ycen+a(2)
	  DO J=NY1,NY2
	    FJ=FLOAT(J) - YCEN
C	      
	    DO I=NX1,NX2
	      FI=FLOAT(I) - XCEN
C	      
	      X= amat(1,1)*FI + amat(1,2)*FJ + xadd
	      Y= amat(2,1)*FI + amat(2,2)*FJ + yadd
C		
	      ix=x
	      iy=y
	      if(ix.ge.1.and.ix.lt.nx.and.iy.ge.1.and.iy.lt.ny)then
c	      IX = MIN(NX-1,MAX(IX,1))
c	      IY = MIN(NY-1,MAX(IY,1))
		ix1=ix+1
		iy1=iy+1
		fx=1+ix-x
		fx1=1.-fx
		fy=1+iy-y
		fy1=1.-fy
		den=array(ix,iy)*fx*fy+array(ix1,iy)*fx1*fy+
     &		    array(ix,iy1)*fx*fy1+array(ix1,iy1)*fx1*fy1
C	      
		delta = delta + abs(den- brray(I,J))
		npix=npix+1
	      endif
c	      
C		
	    ENDDO
C	    
	  ENDDO
	endif
C	  
c	  DNM 5/17/02: if the number of pixels falls below a small threshold
c	  return a big delta; otherwise adjust for # of pixels and save value
c
	if (npix.gt.0.01*nx*ny)then
	  delta=(delta*nx*ny)/npix
	  deltalast=delta
	else
	  delta = 10.*deltalast
	endif
	RETURN
C	  
	END
	
C*********************************************************************
C	  
C	  Halves the dimensions of the image to speed up search.
C	  
C*******************************************************************
C	  
	SUBROUTINE REDUCE2D(ARRAY,NX,NY)
C	  
	DIMENSION ARRAY(NX,NY)
C	  
C	  AVERAGE pixels
C	  
	DO  JJ=1,NY/2
	  J=(JJ*2)-1
C	    
	  DO II=1,NX/2
C	      
	    I=(II*2)-1
C	      
	    ARRAY(II,JJ)=(ARRAY(I,J)+ARRAY(I+1,J)+ARRAY(I,J+1)
     &		+ARRAY(I+1,J+1))/4.
C	      
	  enddo
	enddo
	call irepak(array,array,nx,ny,0,nx/2-1,0,ny/2-1)
c	
	NX=NX/2
	NY=NY/2
C	  
	RETURN
C	  
	END



c	  Function to be called by minimization routine
c
	function func(x)
	real*4 x(*),a(6)
	parameter (ixlim=2100,iylim=2100,limspir=1000,idima=4100*4100)
	parameter (isub=ixlim*iylim/3)
	parameter (isubp1=isub+1, isubt2=2*isub+1, isubt25=2.5*isub+2)
	COMMON //NX,NY,NZ
	real*4 array(idima),brray(ixlim*iylim)
	integer*4 idxspir(limspir),idyspir(limspir)
	integer*2 ixcomp(isub),iycomp(isub)
	real*4 distspir(limspir),denlo(isub),denhi(isub)
	equivalence (brray(1),denlo(1)),(brray(isubp1),denhi(1))
     &	    ,(brray(isubt2),ixcomp(1)),(brray(isubt25),iycomp(1))
	common /funcom/ array,brray,idxspir,idyspir,distspir,mattx,matty
     &	    ,interp,natural,ncompare,nspiral,ifdist,iftrace,rds,ntrial,
     &	    deltmin,ivend,a
	character*1 starout
c	  
	do i=1,ivend
	  a(i)=x(i)
	enddo
c
	if(ifdist.eq.0)then
	  CALL DIFF(DELTA,ARRAY,BRRAY,A,NX,NY,MATTX,MATTY,interp,
     &	      natural)
	else
	  CALL DIST(DELTA,ARRAY,A,NX,NY,ixcomp,iycomp,denlo,denhi,
     &	      ncompare,idxspir,idyspir,distspir,nspiral,natural)
	endif
	func=delta
	ntrial=ntrial+1
	if(iftrace.ne.0)then
	  starout=' '
	  if(delta.lt.deltmin)then
	    starout='*'
	    deltmin=delta
	  endif
	  if(iftrace.eq.1.or.delta.lt.deltmin) write(*,72)
     &	      starout,ntrial,delta,(a(ii),ii=3,6),rds*a(1),rds*a(2)
72	  format(1x,a1,i3,f14.0,4f10.5,2f10.3)
	endif
	return
	end

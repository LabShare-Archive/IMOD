C -----------------------------------------------------------------
C
C TILT......A program for reconstructing a three-dimensional object
C           from a series of two-dimensional projections.
C 	    The projections are assumed to arise from rotation about
C           a fixed tilt axis.
C
C	  The program will accept file names either from the command line or as
C	  entries to the program after it is started.  If there are two names
C	  on the command line, they will be taken as the input and output file
C	  names; if there is one name, it will be taken as the input file name
C	  and the program will ask for the output file name; if there are no
C	  command line arguments, the program will ask for both input and
c	  output file names.
C
c	  The program uses a number of different numerical strategies
c	  depending on whether the reconstructed volume is rotated around the
c	  X axis and depending on the amount of memory available.  If there is
c	  no X-axis tilt being imposed, then each output plane is derived from
c	  one line of the input data.  The program tries first to use the fast
c	  backprojection algorithm developed by Sandberg and Beylkin.  If there
c	  is insufficient memory for this, or if other preconditions are not
c	  satisfied, it then tries to use preliminary stretching of each input
c	  line by the cosine of the tilt angle, which speeds up the direct
c	  backprojection.  If there is not enough memory for this method
c	  either, the program falls back to direct backprojection from the
c	  unstretched input lines. 
c
c	  When a tilt around the X axis is specified, then each output slice
c	  derives from several lines of input data.  The program tries to do
c	  this first by computing untilted slices, one per input line, and
c	  interpolating an output slice from the relevant untilted slices.
c	  It uses fast backprojection if possible; if not, it tries to use
c	  cosine stretching if possible, or falls back to direct computation.
c	  If there is insufficient memory for this approach, the program
c	  reverts to "old-style" X-axis tilting, in which the output slice is
c	  computed directly from the various input slices.  In this case,it
c	  also tries to use cosine stretching if memory is available.
c
c	  The user can disable various strategies by specifying an
c	  interpolation order of 0: cosine stretching by entering
c	  "COSINTERP 0", fast backprojection by entering "FBPINTERP 0", or the
c	  new style of X-axis tilting by entering "XTILTINTERP 0".  There are
c	  some cases where the fast backprojection is slower than the
c	  conventional approach (e.g., few input views).  The program attempts
c	  to detect these cases and use conventional summation instead.  If
c	  you use "FBPINTERP 1", you can bypass this checking and force the 
c	  program to use fast backprojection as long as other conditions are
c	  met.
c
C -----------------------------------------------------------------
C
C	  INPUTS TO THE PROGRAM
C
C	  Each line of input to the program consists of a keyword, usually
c	  followed by one or more values.  The following are all possible
c	  input lines.  Many of these are optional, and they can be input in
c	  any order.  The keyword can be un upper or lower case.
C
C	  ANGLES  TILT_ANGLES
C	  ------
C	  Use this line to specify the tilt angles of the views.  Put the tilt
c	  angles after ANGLES, separated by spaces.  You must enter one tilt
c	  angle for each view.  Use more than one ANGLES line if necessary.
c	  This information will override tilt angles specified in the file
c	  header.  If you enter angles in this way, the file header need not
c	  contain tilt information.
C	  
C	  COMPFRACTION   FRACTION
C	  ------
C	  Use this line if the compression measured by TILTALIGN occurred over
c	  only a FRACTION of the distance between the fiducials.
C
C	  COMPRESS   COMPRESSIONS
C	  ------
C	  With this line, the program will assume that the section has
c	  compressed in Z by the amount given by the COMPRESSION for each
c	  view.  The compressions would be taken directly from the  output of
c	  TILTALIGN for incremental compression.  A value must be entered for
c	  each view.
C
C	  COSINTERP  ORDER  FACTOR
C	  ------
C	  Use this line to control the interpolation ORDER and sampling FACTOR
C	  for cosine stretching of the input data.  ORDER can be 1 for linear,
c	  2 for quadratic, 3 for cubic, or 0 to disable cosine stretching.
c	  The default is linear to provide some smoothing of the data; higher
c	  orders are appropriate if data are relatively noise-free.  FACTOR is
c	  optional; the default is 2, which prevents further smoothing when
c	  the stretched data are linearly interpolated during backprojection.  
C
C	  DENSWEIGHT   NADJACENT   WEIGHTS
C	  ------
C	  Use this line to weight each view proportionally to the local
c	  average tilt increment between views.  NADJACENT specifies the
c	  number of intervals on EACH side of a view to consider; the default
c	  is 2.  Optionally, one may also enter that number of WEIGHTS to be
c	  applied in averaging the adjacent increments.
C
C	  DONE
C	  ------
C	  Put this line at the end to terminate data entry if you wish to put
c	  commands to run another program in the same command file.
C
C	  EXCLUDE  VIEWS	  
C	  ------
C	  Use this line to specify a subset of views to be excluded from the
c	  reconstruction.  Put the view numbers after the EXCLUDE statement,
c	  separated by spaces.  Use more than one EXCLUDE line if the numbers
c	  do not all fit on one line.  You may not have both an INCLUDE and an
c	  EXCLUDE line.
c
C	  EXCLUDELIST  VIEWLIST
C	  EXCLUDELIST2  VIEWLIST
C	  ------
C	  This line is an alternative way to specify views to be excluded.  The
c	  VIEWLIST should be a list of ranges, separated by commas, and with no
c	  embedded spaces; e.g., 1-3,54,67,85-88.  You may have any number of
c	  EXCLUDE, EXCLUDELIST, and EXLCUDELIST2 lines, but they cannot be 
c	  combined with INCLUDE lines.
C
C	  FBPINTERP  ORDER
C	  ------
C	  Use this line to specify the interpolation ORDER for the fast
c	  backprojection algorithm: 1 for linear interpolation (the default),
c	  3 for cubic spline interpolation, which will low-pass filter the
c	  input data more, or 0 to disable fast backprojection.  Entering a 
c	  value with this option will also force the program to use fast
c	  backprojection even if the width, thickness, and number of views
c	  are small enough so that fast backprojection will run slower than
c	  direct summation.
C
C	  FULLIMAGE XSIZE YSIZE
C	  ------
C	  Use this line to specify the full size XSIZE by YSIZE of the
c	  original stack of tilted views, so that a subset of the aligned
c	  stack can be handled properly when using a global X-axis tilt or
c	  local alignments.
C
C	  INCLUDE  VIEWS	  
C	  ------
C	  Use this line to specify a subset of views to be used for the
c	  reconstruction.  Put the view numbers after the INCLUDE statement,
c	  separated by spaces.  Use more than one INCLUDE line if the numbers
c	  do not all fit on one line.
C
C	  LOCALFILE   Filename
C	  ------
C	  Use this line to specify a file containing local tilt alignment
c	  information.
C
C	  LOCALSCALE  FACTOR   
C	  ------
C	  If local tilt alignments were obtained from unreduced data, but the
c	  aligned stack was reduced by binning or transforming, use this line
c	  to specify the FACTOR by which the data were scaled, so that the
c	  local alignment information can be adjusted.
C
C	  LOG BASE
C	  ------
C	  This line allows one to generate a reconstruction using the
c	  logarithm of the densities in the input file, with the value BASE 
c	  added before taking the logarithm.
C
C	  MASK   RMASK
C	  ------
C	  This line allows a mask to be applied so as to exclude from the
c	  reconstructed volume those parts which  lie outside the volume for
c	  which reconstruction is actually possible.  The volume for which
c	  reconstruction is possible is a cylinder whose axis lies along the
c	  tilt axis. Grid points outside this volume will be set to value
c	  RMASK.  THIS OPTION CAN BE IGNORED FOR THIN SECTIONS, where the
c	  thickness is considerably smaller than the maximum radius about the
c	  tilt axis on the zero angle projection.  For 'thick' sections, this
c	  option saves some execution time.
C
C	  MODE    NEWMODE
C	  ------
C	  This line allows one to specify the data mode of the output file,
c	  which is 2 by default.  Be sure to use an appropriate SCALE line so
c	  that data will be scaled to the proper range.
C
C	  OFFSET   DELANG DELXX
C	  ------
C	  This line allows an offset of DELANG degrees to be applied to all
c	  tilt angles and indicates that the tilt axis is offset in the
c	  projection images, cutting the X-axis at  NX/2. + DELXX instead of
c	  NX/2.  DELANG positive rotates reconstructed sections anticlockwise.
c	  The DELXX entry is optional and defaults to 0 when omitted.
C
C	  PERPENDICULAR or  PARALLEL
C	  ------
C	  This line controls which axis order is used for the final
c	  reconstruction.  PARALLEL mode produces sections parallel to the
c	  plane of the zero tilt projection, PERPENDICULAR mode produces
c	  sections normal to the tilt axis.
C
C	  RADIAL   IRMAX IFALL  or RMAX RFALL
C	  ------
C	  This line controls the radial weighting function.  The radial
c	  weighting function is linear away from the origin for a distance of
c	  IRMAX in reciprocal space followed by a gaussian fall-off of s.d.
c	  IFALL.  The distances may be specified either as pixels (values
c	  greater than 1) or as frequencies (cycles/pixel) in Fourier space
c	  (values < 1).
C
C	  REPLICATE  NREPLIC REPINC
C	  ------
C	  With this line, the input views will be replicated NREPLIC times,
c	  with an increment of REPIN degrees added to the original angles for
c	  each replication.  For example, if you can assume 9-fold symmetry,
c	  enter REPLICATE 8 40.  More than one REPLICATE line can be entered.
C	  
C	  SCALE   FLEVL SCALE
C	  ------
C	  This line allows a linear change of density in the  reconstructed
c	  image according to the formula
C	  ARRAY(I)=(ARRAY(I)+FLEVL)*SCALE
C	  After the reconstruction is complete, the program will output the
c	  scale values that would make the data range from 10 to 245.
C
C	  SHIFT   XOFFSET  ZOFFSET
C	  ------
C	  This line allows one to shift the reconstructed slice in X or Z
c	  before it is output.  If XOFFSET is positive, the slice will be
c	  shifted to the right, and the output will contain the left part of
c	  the whole potentially reconstructable area.  If ZOFFSET is positive,
c	  the slice is shifted upward.  The ZOFFSET entry is optional and
c	  defaults to 0 when omitted.
C	  
C	  SLICE   ISLICE JSLICE IDELSLICE
C	  ------
C	  This line allows a limited part of the map to be reconstructed and
c	  is useful for test purposes. A slab from column ISLICE to column
c	  JSLICE (that is, along the medium axis, perpendicular to the tilt
c	  axis) of the volume is reconstructed, at intervals of IDELSLICE.
c	  Slices are numbered from 0.  The IDELSLICE entry is optional and
c	  defaults to 1 when omitted.
C
C	  SUBSETSTART IX IY
C	  ------
C	  If the aligned stack contains a subset of the area in the original
c	  images, and a global X-axis tilt or local alignments are being used,
c	  use this line to enter the index coordinates IX, IY of the lower
c	  left corner of the subset within the original images.  A FULLIMAGE 
c	  line must also be entered.
C
C	  THICKNESS   ITHICK
C	  ------
C	  This line controls the thickness (along the z-axis) of the
c	  reconstructed volume, the thickness being ITHICK pixels.
C	
C	  TILTFILE   Filename
C	  ------
C	  Use this line to specify a file containing a list of all tilt
c	  angles.  The angles may be one per line or many per line.
C
C	  TITLE   string
C	  ----- 
C	  An alphanumeric string giving the title for the job, which will be
c	  added to the output map.  Limit 50 characters.  This entry is
c	  optional; the default is "Tomographic reconstruction".
C
C	  WIDTH   IWIDE
C	  ------
C	  This line specifies the width of the output image; the default is
c	  the width of the input image.
C
C	  XAXISTILT   ANGLE
C	  ------
C	  This line allows one to rotate the reconstruction around the X axis,
c	  so that a section that appears to be tilted around the X axis can be
c	  made flat to fit into a smaller volume.  The ANGLE should be the
c	  tilt of the section relative to the X-Y plane in an unrotated
c	  reconstruction.  For example, if the reconstruction extends 500
c	  slices, and the section is 5 pixels below the middle in the first
c	  slice and 5 pixels above the middle in the last slice, ANGLE should
c	  be 1.1 (the arc sine of 10/500).
C	  
C	  XTILTFILE   Filename
C	  ------
C	  Use this line to specify a file containing a list of tilts around
c	  the X axis for the individual views.  A global tilt specified by the
c	  XAXISTILT line, if any, will be added to these tilts.
c
c	  XTILTINTERP  ORDER
c	  ------
c	  This line controls the order for interpolating an output slice
c	  tilted around the X axis from vertical, untilted slices each computed
c	  from a single line of input data.  Set ORDER to 1 for linear, 2 for
c	  quadratic, 3 for cubic, or 0 to disable this method of X-axis
c	  tilting and revert to computing the output slice directly from input
c	  data.  The default is 1; higher orders are appropriate if data are
c	  particularly noise-free.
C
C
C------------------------------------------------------------------------------
C
C	  Some notes:
C
C	  There are NVIEWS projections taken at tilt angles ANGLES(NV).
C
C	  The projections have size NPRJ x MPRJ where NPRJ is taken 
C	  perpendicular to the tilt axis.
C
C	  The reconstructed image has size 
c	  IWIDE x ITHICK x 1+(JSLICE-ISLICE)/IDELSLICE.
C	  1+(JSLICE-ISLICE)/IDELSLICE planes are reconstructed, of size 
c	  IWIDE x ITHICK  lying perpendicular to the tilt axis.
C 
C	  With perpendicular slices, the header of the output file will set up
c	  a coordinate system congruent with that of the original views.  If
c	  slices are output in order of increasing slice number, this
c	  represents a 90 degree rotation about X, so the slices are inverted
c	  about their new Z axis (by using the negative of the tilt angles).
c	  If slices are output in inverted order (IDELSLICE < 0), this
c	  represents a -90 degree rotation about X; slices are not inverted in
c	  this case. 
c
C	  Originally written by Mike Lawrence.  Storage of tilt angle data 
c	  altered by Guy Vigers; angles can be stored in image stack header by
c	  the program ALTERHEADER.
C	  David Mastronarde added ability to output slices at selected
c	  intervals and in inverse direction, and setting of header
c	  information so that coordinates correspond between output file and
c	  tilt data set.  Also added ability to specify a subset of views to
c	  use or exclude, control of output mode, abilities to take log,
c	  replicate views, enter specific tilt angles or read in angles from a
c	  file, adjust for compression, set width of output, and shift the
c	  output.  Implemented weighting by local tilt increment and padding
c	  before filtering.  Added ability to adjust for tilt around the X axis
c	  and to use local alignments.  Implemented subroutine calls for the
c	  inner loops so that assembly code could be adjusted to minimize very
c	  slow steps on the PC.  Implemented cosine stretching of the input
c	  data as an alternative solution to this problem that is faster on
c	  all machines.  Incorporated the fast backprojection code developed by
c	  Kristian Sandberg and Gregory Beylkin.  Implemented X-axis tilting
c	  by interpolation from untilted slices, so that the fast
c	  backprojection could be used with X-axis tilting.
c
c	  $Author$
c	  
c	  $Date$
c	  
c	  $Revision$
c
c	  $Log$
c	  Revision 3.15  2004/04/01 01:44:23  mast
c	  Used input file range to avoid taking logs of very small numbers
c	
c	  Revision 3.14  2003/12/09 00:11:49  mast
c	  Have card reader accept blank lines in case sed in new sample.com
c	  creates one
c	
c	  Revision 3.13  2003/10/24 03:44:56  mast
c	  took out flush call for Windows/Intel
c	
c	  Revision 3.12  2003/10/16 20:38:32  mast
c	  Adding to option documentation
c	
c	  Revision 3.11  2003/08/02 22:36:49  mast
c	  Revert from the version that padded thickness for x-axis tilting now
c	  that fbp takes care of this.
c	  Limit stack usage so that when loaded data is bigger than a certain
c	  size, only enough is loaded to reconstruct 10 output slices.
c	
c	  Revision 3.8  2003/04/29 23:33:54  mast
c	  Set default for radial filter and increase thickness limit
c	
c	  Revision 3.7  2002/07/28 00:03:40  mast
c	  Made it preserve pixel spacings in output file
c	
c	  Revision 3.6  2002/07/26 19:19:04  mast
c	  Added machine-specific switch-points for not doing fast
c	  backprojection
c	
c	  Revision 3.5  2002/07/21 19:37:25  mast
c	  Replaced STOP with call exit(1) and standardized error outputs
c	
c	  Revision 3.4  2002/05/07 02:02:53  mast
c	  Added EXCLUDELIST option
c	
c	  Revision 3.3  2002/02/01 15:27:31  mast
c	  Made it write extra data periodically with PARALLEL option to 
c	  partially demangle the output file and prevent very slow reading
c	  under Linux.
c	
c	  Revision 1.2  2001/11/22 00:41:57  mast
c	  Fixed computation of mean for files > 2 GPixels
c	
c
	include 'tilt.inc'
	real*4 fp(2,3)
	integer*4 nxyztmp(3),nxyzst(3)
	data nxyzst/0.,0.,0./
	character*20 radtxt1/'Radial weighting'/
	character*18 radtxt2/'   function'/
	TMASK = -1.E+30
	maxSTACK=limstack
	interhsave=20
	nsliceout=0
	memBigCrit = 20000000
	memBigOutLim = 10
	DTOT=0.
	DMIN=1.E30
	DMAX=-1.E30
C
C Open files and read control data
	CALL INPUT
	NSLICE=(JSLICE-ISLICE)/idelslice+1
c	  
c	  recompute items not in common
c
	if(fastbp)then
	  nprj2=nprj+npad
	  radtxt1='Fast back projection'
	  radtxt2='   working storage'
	else
	  NPRJ2=NPRJ+2+npad
	endif
c	    
c	  initialize variables for loaded slices and for ring buffer of
c	  vertical slices
c	  
	inloadstr=0
	inloadend=0
	lastready=0
	lastcalc=0
	idir=sign(1,idelslice)
	nextfreevs=1
	lvsstart=-1
	lvsend=-1
	nvsinring=0
	ycenfix=ycen
	abssal=abs(sal(1))
C	  
C Calculate and report stack loading
c
	NPLANES=(maxSTACK-NBASE-ipextra+1)/IPLANE
	if(ifalpha.eq.0.and.nxwarp.eq.0)then
	  NPLANES=MIN(NPLANES,NSLICE)
	else
	  NPLANES=MIN(NPLANES,MPRJ)
	endif	  
	NI=NPLANES*IPLANE
	WRITE(6,900)maxSTACK,radtxt1,imap-1,radtxt2,ITHWID
	if(ifalpha.lt.0)write(6,902)nvertneed,nvertneed*ithick*iwide
	write(6,903)NPLANES,NI
	if (ipextra.ne.0)write(6,901)ipextra
	call maskprep
c	print *,'slicen',slicen,', imap',imap,', nbase',nbase
	if(ifalpha.ge.0)then
	  loadlimit=jslice
	else
	  if(idir.gt.0)then
	    loadlimit=slicen+(jslice-slicen)*cal(1)+yoffset*sal(1)+
     &		0.5*ithickout*abssal+2.
	  else
	    loadlimit=slicen+(jslice-slicen)*cal(1)+yoffset*sal(1)-
     &		0.5*ithickout*abssal-1.
	  endif
	endif
C
C Main loop over slices perpendicular to tilt axis
C ------------------------------------------------
	DO 100 LSLICEout=ISLICE,JSLICE,idelslice
c	    
c	    get limits for slices that are needed: the slice itself for regular
c	    work, or required vertical slices for new-style X tilting
c	    
c	  print *,'working on',lsliceout
	  if(ifalpha.ge.0)then
	    lsstart=lsliceout
	    lsend=lsliceout
	  else
	    tanalpha=sal(1)/cal(1)
	    lsmin=slicen+(lsliceout-slicen)*cal(1)+yoffset*sal(1)-
     &		0.5*ithickout*abssal-1.
	    lsmax=slicen+(lsliceout-slicen)*cal(1)+yoffset*sal(1)+
     &		0.5*ithickout*abssal+2.
c	    print *,'need slices',lsmin,lsmax
	    lsmin=max(1,lsmin)
	    lsmax=min(mprj,lsmax)
	    if(idir.gt.0)then
	      lsstart=lsmin
	      if(lsmin.ge.lvsstart.and.lsmin.le.lvsend)lsstart=lvsend+1
	      lsend=lsmax
	    else
	      lsstart=lsmax
	      if(lsmax.le.lvsstart.and.lsmax.ge.lvsend)lsstart=lvsend-1
	      lsend=lsmin
	    endif
	  endif
c	    
c	    loop on needed vertical slices
c	    
c	  print *,'looping to get',lsstart,lsend
	  do lslice=lsstart,lsend,idir
C	      
C	      Load stack with as many lines from projections as will
C	      fit into the remaining space. The stack is constructed as
C	      follows: 
C	      (Radial weighting function or FBP working storage)--->
C	      (current output plane) --->
C	      (vertical reconstructed planes for new X-axis tilting) --->
C	      (first slice,first view)....(first slice,last view)--->
C	      ..................................................--->
C	      (last slice,first view )....(last slice,last view)
C	      (Extra space for another set of slices, if cosine stretching)
C	      After each view two additional elements are inserted for
C	      the transform. 
C...............don't load too many slices 
C	      ..not more than NPLANES..not beyond eof.
c	      
c	      Enter loading procedures unless the load is already set for the
c	      current slice
c	      
	    if(inloadstr.eq.0.or.idir*lslice.gt.idir*lastready)then
	      needstart=0
	      needend=0
	      itryend=0
	      lastready=0
	      itry=lslice
	      ifenough=0
c		
c		loop on successive output slices to find what input slices are
c		needed for them; until all slices would be loaded or there
c		would be no more room
c		or until the big memory criterion is exceeded and the limit
c		on number of output slices for that case is reached
	      do while(itry.gt.0.and.itry.le.mprj.and.ifenough.eq.0.and.
     &		  idir*itry.le.idir*loadlimit.and.
     &		  idir*(itryend-needstart)+1.le.nplanes .and.
     &		  (nbase + ipextra + idir*(itryend-needstart) * iplane .le.
     &		  memBigCrit .or. idir *(itry - lslice) .le. memBigOutLim))
		if(ifalpha.le.0.and.nxwarp.eq.0)then
c		    
c		    regular case is simple: just need the current slice
c		    
		  laststart=itry
		  lastend=itry
		elseif(itry.ne.lastcalc)then
c		    
c		    for old-style X-tilt or local alignment, determine what
c		    slices are needed by sampling 
c		    set up sample points: left and right if no warp,
c		    or twice the warp spacing 
c		    
		  ixleft=1-xcen+xcenin+delxx
		  if(nxwarp.eq.0)then
		    nxassay=2
		    dxassay=ixleft+iwide-1
		  else
		    dxtmp=idxwarp/2
		    nxassay=max(2.,iwide/dxtmp+1.)
		    dxassay=(iwide-1.)/(nxassay-1.)
		  endif
c		    
c		    sample top and bottom at each position
c		    
		  minslice=mprj+1
		  maxslice=0
		  do iassay=1,nxassay
		    ixsam=nint(ixleft+(iassay-1)*dxassay)
		    do iv=1,nviews
		      if(nxwarp.ne.0)	then
			call local_factors
     &			    (ixsam,itry,mapuse(iv),ind1,ind2,ind3,ind4,
     &			    f1,f2,f3,f4)
			cosb=cwarpb(ind1)
			sinb=swarpb(ind1)
		      else
			cosb=cbet(iv)
			sinb=sbet(iv)
		      endif
		      do iy=1,ithick,ithick-1
c			  
c			  for each position, find back-projection location
c			  transform if necessary, and use to get min and
c			  max slices needed to get this position
c			  
			xx=ixsam-xcen
			yy=itry-slicen
			zz=iy-ycen
			xp=xx*cosb+yy*sal(iv)*sinb+zz*cal(iv)*sinb+
     &			    xcenin+delxx
			yp=yy*cal(iv)-zz*sal(iv)+slicen
			if(nxwarp.ne.0)then
			  call xfapply(fw(1,1,ind1),xcenin,slicen,xp,yp,
     &			      xp1,yp1)
			  xp2=xx*cwarpb(ind2)+yy*sal(iv)*swarpb(ind2)+
     &			      zz*cal(iv)*swarpb(ind2)+ xcenin+delxx
			  call xfapply(fw(1,1,ind2),xcenin,slicen,xp2,yp,
     &			      xp2,yp2)
			  xp3=xx*cwarpb(ind3)+yy*sal(iv)*swarpb(ind3)+
     &			      zz*cal(iv)*swarpb(ind3)+ xcenin+delxx
			  call xfapply(fw(1,1,ind3),xcenin,slicen,xp3,yp,
     &			      xp3,yp3)
			  xp4=xx*cwarpb(ind4)+yy*sal(iv)*swarpb(ind4)+
     &			      zz*cal(iv)*swarpb(ind4)+ xcenin+delxx
			  call xfapply(fw(1,1,ind4),xcenin,slicen,xp4,yp,
     &			      xp4,yp4)
			  xp=f1*xp1+f2*xp2+f3*xp3+f4*xp4
			  yp=f1*yp1+f2*yp2+f3*yp3+f4*yp4
			endif
			iyp=max(1.,yp)
			minslice=min(minslice,iyp)
			maxslice=max(maxslice,min(mprj,iyp+1))
		      enddo
		    enddo
		  enddo
c		    
c		    set up starts and ends as appropriate for direction
c		    
		  lastcalc=itry
		  if(idir.gt.0)then
		    laststart=minslice
		    lastend=maxslice
		  else
		    laststart=maxslice
		    lastend=minslice
		  endif
		endif
c		  
c		  values here must work in single-slice case too
c		  if this is the first time, set needstart
c		  if this load still fits, set needend and lastready
c		  
		itryend=lastend
		if(needstart.eq.0)needstart=laststart
		if(idir*(itryend-needstart)+1.le.nplanes)then
		  needend=itryend
		  lastready=itry
		else
		  ifenough=1
		endif
		itry=itry+idelslice
	      enddo
c	      print *,'itryend,needstart,needend,lastready',itryend,
c     &		  needstart,needend,lastready
	      if(needend.eq.0) call errorexit
     &		  ('INSUFFICIENT STACK SPACE TO DO A SINGLE SLICE')
c		
c		if some are already loaded, need to shift them down
c		
	      nalready=max(0,idir*(inloadend-needstart)+1)
	      if(inloadend.eq.0)nalready=0
	      ioffset=idir*(needstart-inloadstr)*iplane
	      do i=nbase,nbase+nalready*iplane-1
		array(i)=array(i+ioffset)
	      enddo
c	      if(nalready.ne.0)print *,'shifting',needstart,inloadend
c		
c		load the planes in one plane high if stretching
c		
	      IBASE=NBASE+nalready*iplane+ipextra
	      lstart=needstart+idir*nalready
C		
c	      print *,'loading',lstart,needend
	      DO 200 NV=1,NVIEWS
		ISTART=IBASE
		DO 210 NL=lstart,needend,idelslice
C		    Position to read from projection NV at record NL
		  iyload=max(0,min(mprj-1,nl-1))
		  CALL IMPOSN(1,mapuse(NV)-1,iyload)
		  CALL IRDLIN(1,ARRAY(ISTART),*999)
c		    Take log if requested
c		    3/31/04: limit values to .001 time dynamic range
		  if(iflog.ne.0)then
		    valmin = 1.e-3 * (pmax - pmin)
		    do 220 ix=istart,istart+nprj-1
		      array(ix)=alog10(max(valmin,array(ix)+baselog))
220		    continue
		  endif
c		    
		  if(.not.fastbp)then
c		      
c		      pad with taper between start and end of line
c		      
		    nsum=0
		    xsum=0.
		    do ix=istart,istart+min(2,nprj-1)
		      nsum=nsum+1
		      xsum=xsum+array(ix)
		    enddo
		    stmean=xsum/nsum
		    if(nsum.eq.0)print *,'stmean bogus'
		    nsum=0
		    xsum=0.
		    do ix=istart+max(0,nprj-3),istart+nprj-1
		      nsum=nsum+1
		      xsum=xsum+array(ix)
		    enddo
		    if(nsum.eq.0)print *,'ndmean bogus'
		    endmean=xsum/nsum
		    do ipad=1,npad
		      f=ipad/(npad+1.)
		      array(istart+nprj+ipad-1)=f*stmean+(1.-f)*endmean
		    enddo
		  endif
c		    
		  ISTART=ISTART+IPLANE
210		continue
		IBASE=IBASE+NPRJ2
200	      continue
	      if(.not.fastbp)then
		IBASE=NBASE+nalready*iplane
		DO  NL=lstart,needend,idelslice
		  call transform(ibase)
		  ibase=ibase+iplane
		enddo
	      endif
	      inloadstr=needstart
	      inloadend=needend
	      nstack=nbase+iplane*((inloadend-inloadstr)/idelslice+1)-1
	    END IF
C	      
C	      Stack is full.
c
C Zero array and set background mask if required
	    CALL ZEROM
C Process all views for current slice
	    ISTART=NBASE+IPLANE*(lslice-inloadstr)/idelslice
c	      
c	      If new-style X tilt, set  the Y center based on the slice
c	      number, and adjust the y offset slightly
c
	    if(ifalpha.lt.0)ycen=ycenfix+(cal(1)-1.)*yoffset
     &		-nint(tanalpha*(lslice-slicen))
c
c	    print *,nbase,lslice,inloadstr
c	  print *,'projecting',lslice,' at',istart,', ycen =',ycen
	    CALL PROJECT(ISTART,lslice)
c	      
c	      move vertical slice into ring buffer, adjust ring variables
c
	    if(ifalpha.lt.0)then
c	      print *,'moving slice to ring position',nextfreevs
	      ioffset=ithwid+(nextfreevs-1)*ithick*iwide
	      do i=imap,imap+ithick*iwide-1
		array(i+ioffset)=array(i)
	      enddo
	      if(nvsinring.lt.nvertneed)then
		if(nvsinring.eq.0)lvsstart=lslice
		nvsinring=nvsinring+1
	      else
		lvsstart=lvsstart+idir
	      endif
	      lvsend=lslice
	      nextfreevs=nextfreevs+1
	      if(nextfreevs.gt.nvertneed)nextfreevs=1
	    endif
	  enddo
c
	  if(ifalpha.lt.0)then
c	    
c	    interpolate output slice from vertical slices
c	      
	    iringstart=1
	    if(nvsinring.eq.nvertneed)iringstart=nextfreevs
c	    print *,'composing',lsliceout,' from',lvsstart,lvsend,iringstart
	    call compose(lsliceout,lvsstart,lvsend,idir,iringstart)
	  endif
C
C Write out current slice
	  CALL DUMP(LSLICEout,DMIN,DMAX,DTOT)
c	  DNM 10/22/03:  Can't use flush in Windows/Intel because of sample.com
c	  call flush(6)
c	    
c	    write out header periodically, restore writing position
c
	  if(perp.and.interhsave.gt.0)then
	    nsliceout=nsliceout+1
	    nxyztmp(1)=iwide
	    nxyztmp(2)=ithickout
	    nxyztmp(3)=nsliceout
	    if(mod(nsliceout,interhsave).eq.1)then 
	      call ialsiz(2,nxyztmp,nxyzst)
	      DMEAN=DTOT/(float(NSLICEout)*IWIDE*ITHICK)
	      CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
	      call imposn(2,nsliceout,0)
	    endif
	  endif
C%	WRITE(6,1000)LSLICE-1
C%1000	FORMAT(' Slice',I10,' processed')
100	CONTINUE
C
C End of main loop
C-----------------
C
C Close files
	CALL IMCLOSE(1)
	DMEAN=DTOT/(float(NSLICE)*IWIDE*ITHICK)
	if(perp.and.interhsave.gt.0)then
	  nxyztmp(3)=nslice
	  call ialsiz(2,nxyztmp,nxyzst)
	endif
	CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
	WRITE(6,930)
	CALL IRDHDR(2,nxyztmp,nxyzst,MODE,DMIN,DMAX,DMEAN)
	CALL IMCLOSE(2)
	if(fastbp)then
	  scale=scale/1000.
	  flevl=flevl/1000.
	endif
	unscmin=dmin/scale-flevl
	unscmax=dmax/scale-flevl
	recscale=nviews*nreplic*245./(unscmax-unscmin)
	recflevl=(10.*(unscmax-unscmin)/245.-unscmin)/(nviews*nreplic)
	write(6,905)recflevl,recscale
	WRITE(6,910)NSLICE
	call exit(0)
999	WRITE(6,920)mapuse(NV),L
	call exit(1)
C
C
900	FORMAT(//' STACK LOADING'
	1	/' -------------'
	2      //' Total stack size             ',I8/
	2       /,1x,a20,'         ',I8/,a,//,
	4	 ' Output slice                 ',I8,/)
901	format(' Stretching buffer            ',I8,/)
902	format(1x,i4,  ' Untilted slices         ',I8,/)
903	format(1X,I4,' Transposed projections  ',I8,/)
905	format(//' To scale output to bytes (10-245), use SCALE',f12.3,
     &	    f12.5)
910	FORMAT(//' Reconstruction of',I5,' slices complete.',
	1//,1X,78('-'))
920	FORMAT(//' ERROR: TILT -  reading in view',I3,' for slice'
	1 ,I5,/)
930	FORMAT(//' Header on reconstructed map file'/
	1        ' --------------------------------'//)
	END
C --------------------------------------------------------------------
	SUBROUTINE RADWT(IRMAXin,IFALLin)
C	-----------------------------
C
C Set Radial Transform weighting
C Linear ramp plus Gaussian fall off
	include 'tilt.inc'
	COMMON/DENSWT/nweight,wincr(20)
c
	nprj2=nprj+2+npad
	IEND=NPRJ2/2
	stretch=float(nprj+npad)/nprj
	irmax=nint(irmaxin*stretch)
	ifall=nint(ifallin*stretch)
	if(nweight.gt.0)then
	  avgint=(angles(nviews)-angles(1))/(nviews-1)
c	  write(6,401)
c401	  format(/' View  Angle Weighting')
	endif
C
C Set up linear ramp
	do iv=1,nviews
	  atten=1.
	  if(nweight.gt.0.)then
	    sumint=0
	    wsum=0.
	    do iw=1,nweight
	      if(iv-iw.gt.0)then
		wsum=wsum+wincr(iw)
		sumint=sumint+wincr(iw)*(angles(iv+1-iw)-
     &		    angles(iv-iw))
	      endif
	      if(iv+iw.le.nviews)then
		wsum=wsum+wincr(iw)
		sumint=sumint+wincr(iw)*(angles(iv+iw)-
     &		    angles(iv+iw-1))
	      endif
	    enddo
	    atten=atten*(sumint/wsum)/avgint
c	    write(6,402)iv,angles(iv),atten
c402	    format(i4,f8.2,f10.5)
	  endif
	  ibase=(iv-1)*nprj2
	  DO  I=1,min(IRMAX,iend)
	    ARRAY(ibase+2*I-1)=atten*(I-1)
	    ARRAY(ibase+2*I)=atten*(I-1)
	  enddo
	enddo
C
C Set up Gaussian
	DO I=IRMAX+1,IEND
	  ARG=FLOAT(I-IRMAX)/FLOAT(IFALL)
	  atten=EXP(-ARG*ARG)
	  ibase=0
	  do iv=1,nviews
	    Z=atten*array(ibase+2*irmax)
	    ARRAY(ibase+2*I-1)=z
	    ARRAY(ibase+2*I)=Z
	    ibase=ibase+nprj2
	  enddo
	enddo
	RETURN
	END
C
C
C ---------------------------------------------------------------------
	SUBROUTINE MASKPREP
C	----------------
C
C This subroutine prepares the limits of the slice width to be computed if
c masking is used
	include 'tilt.inc'
C
C Compute left and right edges of unmasked area
	IF(MASK)THEN
	  radlft=(nprj/2+delxx)**2
	  radrt=(nprj/2-delxx)**2
	  DO I=1,ITHICK
	    Y=I-YCEN
	    YY=min(Y*Y,radlft)
	    ixlft=xcen+1.-sqrt(radlft-yy)
	    masklft(i)=max(1,ixlft)
	    YY=min(Y*Y,radrt)
	    ixrt=xcen+sqrt(radrt-yy)
	    maskrt(i)=min(iwide,ixrt)
	  enddo
C-------------------------------------------------
C If no mask
	ELSE
	  DO I=1,ITHICK
	    masklft(i)=1
	    maskrt(i)=iwide
	  enddo
	END IF
	RETURN
	END
C
C
C ---------------------------------------------------------------------
	SUBROUTINE ZEROM
C	----------------
C
C This subroutine zeros the slice and applies the mask if requested
	include 'tilt.inc'
C
C Zero and apply mask
	IF(MASK)THEN
	  INDEX=IMAP
	  DO I=1,ITHICK
	    DO J=1,masklft(i)-1
	      ARRAY(INDEX)=TMASK
	      INDEX=INDEX+1
	    enddo
	    do j=masklft(i),maskrt(i)
	      ARRAY(INDEX)=0.
	      INDEX=INDEX+1
	    enddo
	    DO J=maskrt(i)+1,iwide
	      ARRAY(INDEX)=TMASK
	      INDEX=INDEX+1
	    enddo
	  enddo
C-------------------------------------------------
C Zero only
	ELSE
	  IEND=IMAP+ITHWID-1
	  DO I=IMAP,iend
	    ARRAY(I)=0.	
	  enddo
	END IF
	RETURN
	END
C ---------------------------------------------------------------------
	SUBROUTINE TRANSFORM(ibase)
C       ----------------------------
C
C This subroutine applies a one-dimensional Fourier transform to
C all views corresponding to a given slice, applies the radial 
C weighting function and then applies an inverse Fourier transform.
C
	include 'tilt.inc'
	NPRJ2=NPRJ+2+npad
	istart=ibase+ipextra
C
C Apply forward Fourier transform
	CALL ODFFT(ARRAY(ISTART),NPRJ+npad,NVIEWS,0)	
C
C Apply Radial weighting
	INDEX=ISTART
	indrad=1
	DO 10 NV=1,NVIEWS
	DO 10 I=1,NPRJ2
	ARRAY(INDEX)=ARRAY(INDEX)*ARRAY(indrad)
	indrad=indrad+1
10	INDEX=INDEX+1
C
C Apply inverse transform
	CALL ODFFT(ARRAY(ISTART),NPRJ+npad,NVIEWS,1)
	if(ipextra.eq.0)return
c	  
c	  do cosine stretch and move down one plane
c	  Use cubic interpolation a la cubinterp
c
c	print *,'istart, ibase', istart,ibase
	do nv=1,nviews
	  ibfrom=istart+(nv-1)*nprj2-1
	  ibto=ibase+indstretch(nv)-1
c	  print *,nv,ibfrom,ibto,nstretch(nv)
	  diffmax=0.
	  if(interpord.eq.1)then
c		
c	      linear interpolation
c
	    do i=1,nstretch(nv)
	      x=i/float(interpfac)+ofstretch(nv)
	      xp=min(max(1.,x*cbet(nv)), float(nprj))
	      IXP = XP
	      DX = XP - IXP
	      ixp = ixp + ibfrom
	      IXPP1 = min(IXP + 1, nprj +ibfrom)
	      dxm1 = dx-1.
	      array(ibto+i)=-dxm1*array(ixp)+ dx*array(ixpp1)
	    enddo
	  else if(interpord.eq.2)then
c
c	      quadratic
c	      
	    do i=1,nstretch(nv)
	      x=i/float(interpfac)+ofstretch(nv)
	      xp=min(max(1.,x*cbet(nv)), float(nprj))
	      IXP = nint(XP)
	      DX = XP - IXP
	      ixp = ixp + ibfrom
	      IXPP1 = min(IXP + 1, nprj +ibfrom)
	      IXPM1 = max(IXP - 1, 1 +ibfrom)
	      V4 = ARRAY(IXPM1)
	      V5 = ARRAY(IXP)
	      V6 = ARRAY(IXPP1)
C		
	      A = (V6 + V4)*.5 - V5
	      C = (V6 - V4)*.5
C		
	      dennew = A*DX*DX + C*DX + V5
c	      dennew=min(dennew,max(v4,v5,v6))
c	      dennew=max(dennew,min(v4,v5,v6))
	      array(ibto+i)=dennew
	    enddo
	  else
c		
c	      cubic
c
	    do i=1,nstretch(nv)
	      x=i/float(interpfac)+ofstretch(nv)
	      xp=min(max(1.,x*cbet(nv)), float(nprj))
	      IXP = XP
	      DX = XP - IXP
	      ixp = ixp + ibfrom
	      IXPP1 = min(IXP + 1, nprj +ibfrom)
	      IXPM1 = max(IXP - 1, 1 +ibfrom)
	      ixpp2 = min(ixp + 2, nprj+ibfrom)
	      
	      dxm1 = dx-1.
	      dxdxm1=dx*dxm1
	      fx1=-dxm1*dxdxm1
	      fx4=dx*dxdxm1
	      fx2=1+dx**2*(dx-2.)
	      fx3=dx*(1.-dxdxm1)
	      dennew=fx1*array(ixpm1)+fx2*array(ixp)+
     &		  fx3*array(ixpp1)+fx4*array(ixpp2)
c	    dennew=min(dennew,max(array(ixpm1),array(ixp),array(ixpp1),
c     &		array(ixpp2)))
c	    dennew=max(dennew,min(array(ixpm1),array(ixp),array(ixpp1),
c     &		array(ixpp2)))
	      array(ibto+i)=dennew
	    enddo
	  endif
	enddo

	RETURN
	END
C ---------------------------------------------------------------------
	SUBROUTINE PROJECT(ISTART,lslice)
C	--------------------------
C
C This subroutine assembles one reconstructed slice perpendicular
C to the tilt axis, using a back projection method.
C
	include 'tilt.inc'
	real*4 xprojf(limwidth),xprojz(limwidth)
	real*4 yprojf(limwidth),yprojz(limwidth)
	integer*4 jstrt(3),jend(3)
	real*8 xp,yp,xp2,xp3,xp4
	real*4 fp(2,3)
	real*8 xproj8

	if(fastbp)then
	  nprj2=nprj+npad
	  isshift=istart+(nprj-nprjfbp)/2
	  ifshift=0
	  fbpshift=0.
	  ycenorig=ithick/2+0.5
	  if(ycenorig.ne.ycen)then
	    ifshift=1
	    fbpshift=ycenorig-ycen
	  endif
	  call fbp(array(isshift),nprj2,nprjfbp,nviews,iwide,ithick,
     &	      ifshift,fbpshift,
     &	      array(imap), array(ifbpiw),array(ifbprw),array(ifbpzw),
     &	      array(ifbpwrk), array(ifbpzwrk))
	  return
	endif

	NPRJ2=NPRJ+2+npad
c	slicen=mprj/2.+0.5
c	ipdel=idelslice*nprj2*nviews
	ipdel=idelslice*iplane
c	nbase=imap+ithwid
	
	if(nxwarp.eq.0)then
c	  
c Loop over replications
	  do irep=1,nreplic
C
C Loop over all views
	    IPOINT=ISTART-1
	    DO NV=1,NVIEWS
C
C Set view angle
	      iv=nv+(irep-1)*nviews  
	      CBETA=Cbet(iv)
	      SBETA=Sbet(iv)
C
C Loop over all points in output slice
	      INDEX=IMAP
C
	      DO I=1,ITHICK
		ZZ=(I-YCEN)*compress(nv)
		if(ifalpha.le.0)then
		  zPART=zz*SBETA+XCENin+DELXX
		else
c
c		    If x-axis tilting, find interpolation factor between the
c		    slices
c
		  yy=lslice-slicen
		  zpart= yy*sal(iv)*sbeta + zz*cal(iv)*sbeta +
     &		      xcenin+delxx
		  yproj=yy*cal(iv) - zz*sal(iv) + slicen
		  jPROJ=YPROJ
		  jproj=min(mprj-1,jproj)
		  YFRAC=YPROJ-JPROJ
		  omyfrac=1.-yfrac
		endif
c		  
c		  compute left and right limits that come from legal data
c
		xlft=(1.-zpart)/cbeta + xcen
		jlft=xlft
		if(jlft.lt.xlft)jlft=jlft+1
		jlft=max(jlft,masklft(i))
		xrt=(nprj-zpart)/cbeta + xcen
		jrt=xrt
		if(jrt.eq.xrt)jrt=jrt-1
		jrt=min(jrt,maskrt(i))
c		  
c		  set up starting index and projection position
c
		index=index+(jlft-1)
		x=jlft-xcen
		if(interpfac.ne.0)then
c		    
c		    Computation with prestretched data
c
		  XPROJ8=interpfac*(zPART/CBETA + X - ofstretch(iv))
		  IPROJ=XPROJ8
		  XFRAC=XPROJ8-IPROJ
		  iproj=iproj + ipoint + indstretch(iv)
		  omxfrac=1.-xfrac
		  if(ifalpha.le.0)then
c		      
c		      interpolation in simple case of no x-axis tilt
c
		    DO ind=index,index+jrt-jlft
		      ARRAY(IND)=ARRAY(IND)+
     &			  omxfrac*ARRAY(IPROJ) +XFRAC*ARRAY(IPROJ+1) 
		      iproj=iproj+interpfac
		    enddo
		    index=index+jrt+1-jlft
		  else
c		      
c		      If x-axis tilting, interpolate from two lines
c		      
		    ip1=iproj+(jproj-lslice)*ipdel
		    ip2=ip1+ipdel
		    if(yproj.ge.1..and.yproj.le.mprj.and.
     &			ip1.ge.nbase.and.ip2.ge.nbase.and.ip1.lt.nstack
     &			.and.ip2.lt.nstack)then
		      
		      DO ind=index,index+jrt-jlft
			ARRAY(IND)=ARRAY(IND)+
     &			    omxfrac*(omyfrac*ARRAY(IP1)+yFRAC*ARRAY(IP2)) +
     &			    xfrac*(omyfrac*ARRAY(IP1+1)+yFRAC*ARRAY(IP2+1))
			ip1=ip1+interpfac
			ip2=ip2+interpfac
		      enddo
		    endif
		    index=index+jrt+1-jlft
		  endif
		else
c		    
c		    Computation direct from projection data
c
		  XPROJ8=zPART+X*CBETA
		  if(ifalpha.le.0)then
c		      
c		      interpolation in simple case of no x-axis tilt
c		      
		    call bpsumnox(array,index,ipoint,jrt+1-jlft,
     &			xproj8,cbeta)
		  else
c		      
c		      If x-axis tilting
c		      
		    IPROJ=XPROJ8
		    ipbase=ipoint+(jproj-lslice)*ipdel
		    ip1=ipbase+iproj
		    ip2=ip1+ipdel
		    if(yproj.ge.1..and.yproj.le.mprj.and.
     &			ip1.ge.nbase.and.ip2.ge.nbase.and.ip1.lt.nstack
     &			.and.ip2.lt.nstack)then
		      
		      call bpsumxtilt(array,index,ipbase,ipdel,jrt+1-jlft,
     &			  xproj8,cbeta,yfrac,omyfrac)
		    else
		      index=index+jrt+1-jlft
		    endif
		  endif
		endif
		index=index+iwide-jrt
	      enddo
C
C-------------------------------------------
C
C End of projection loop
	      if(interpfac.eq.0)IPOINT=IPOINT+NPRJ2
	    enddo
C
C-------------------------------------------
C	  
	  enddo
	else
c
c	    LOCAL ALIGNMENTS  
c
c Loop over replications
	  do irep=1,nreplic
C
C Loop over all views
	    IPOINT=ISTART-1
	    DO NV=1,NVIEWS
C
C Set view angle
	      iv=nv+(irep-1)*nviews  
	      CBETA=Cbet(iv)
	      SBETA=Sbet(iv)
C		
c		precompute the factors for getting xproj and yproj all the
c		way across the slice
c		
	      ifytest=0
	      zbot=(1-ycen)*compress(nv)
	      ztop=(ithick-ycen)*compress(nv)
	      DO J=1,IWIDE
c		  
c		  get transform and angle adjustment
c		  
		ixc=nint(j-xcen+xcenin+delxx)
		call local_factors(ixc,lslice,mapuse(nv),
     &		    ind1,ind2,ind3,ind4,f1,f2,f3, f4)
c		  
c		  get all the factors needed to compute a projection position
c		  from the four local transforms
c		  
		cbeta=cwarpb(ind1)
		sbeta=swarpb(ind1)
		calf=cwarpa(ind1)
		salf=swarpa(ind1)
		a11=fw(1,1,ind1)
		a12=fw(1,2,ind1)
		a21=fw(2,1,ind1)
		a22=fw(2,2,ind1)
		xadd=fw(1,3,ind1)+xcenin-xcenin*a11-slicen*a12
		yadd=fw(2,3,ind1)+slicen-xcenin*a21-slicen*a22
c		  
		cbeta2=cwarpb(ind2)
		sbeta2=swarpb(ind2)
		calf2=cwarpa(ind2)
		salf2=swarpa(ind2)
		a112=fw(1,1,ind2)
		a122=fw(1,2,ind2)
		a212=fw(2,1,ind2)
		a222=fw(2,2,ind2)
		xadd2=fw(1,3,ind2)+xcenin-xcenin*a112-slicen*a122
		yadd2=fw(2,3,ind2)+slicen-xcenin*a212-slicen*a222
c		  
		cbeta3=cwarpb(ind3)
		sbeta3=swarpb(ind3)
		calf3=cwarpa(ind3)
		salf3=swarpa(ind3)
		a113=fw(1,1,ind3)
		a123=fw(1,2,ind3)
		a213=fw(2,1,ind3)
		a223=fw(2,2,ind3)
		xadd3=fw(1,3,ind3)+xcenin-xcenin*a113-slicen*a123
		yadd3=fw(2,3,ind3)+slicen-xcenin*a213-slicen*a223
c		  
		cbeta4=cwarpb(ind4)
		sbeta4=swarpb(ind4)
		calf4=cwarpa(ind4)
		salf4=swarpa(ind4)
		a114=fw(1,1,ind4)
		a124=fw(1,2,ind4)
		a214=fw(2,1,ind4)
		a224=fw(2,2,ind4)
		xadd4=fw(1,3,ind4)+xcenin-xcenin*a114-slicen*a124
		yadd4=fw(2,3,ind4)+slicen-xcenin*a214-slicen*a224
c
		f1x=f1*a11
		f2x=f2*a112
		f3x=f3*a113
		f4x=f4*a114
		f1xy=f1*a12
		f2xy=f2*a122
		f3xy=f3*a123
		f4xy=f4*a124
c		fxfromy=f1*a12+f2*a122+f3*a123+f4*a124
		f1y=f1*a21
		f2y=f2*a212
		f3y=f3*a213
		f4y=f4*a214
		f1yy=f1*a22
		f2yy=f2*a222
		f3yy=f3*a223
		f4yy=f4*a224
c		fyfromy=f1*a22+f2*a222+f3*a223+f4*a224
		xalladd=f1*xadd+f2*xadd2+f3*xadd3+f4*xadd4
		yalladd=f1*yadd+f2*yadd2+f3*yadd3+f4*yadd4
c		  
c		  Each projection position is a sum of a fixed factor ("..f")
c		  and a factor that multiplies z ("..z")
c		  
		xx=j-xcen
		yy=lslice-slicen
		xp1f=xx*cbeta + yy*salf*sbeta + xcenin+delxx
		xp1z=calf*sbeta
		xp2f=xx*cbeta2 + yy*salf2*sbeta2 + xcenin+delxx
		xp2z=calf2*sbeta2
		xp3f=xx*cbeta3 + yy*salf3*sbeta3 + xcenin+delxx
		xp3z=calf3*sbeta3
		xp4f=xx*cbeta4 + yy*salf4*sbeta4 + xcenin+delxx
		xp4z=calf4*sbeta4
c		  
c		xp1f=xx*cbeta + yy*sal(iv)*sbeta + xcenin+delxx
c		xp1z=cal(iv)*sbeta
c		xp2f=xx*cbeta2 + yy*sal(iv)*sbeta2 + xcenin+delxx
c		xp2z=cal(iv)*sbeta2
c		xp3f=xx*cbeta3 + yy*sal(iv)*sbeta3 + xcenin+delxx
c		xp3z=cal(iv)*sbeta3
c		xp4f=xx*cbeta4 + yy*sal(iv)*sbeta4 + xcenin+delxx
c		xp4z=cal(iv)*sbeta4

c		  
c		ypf=yy*cal(iv) + slicen
c		ypz=-sal(iv)
		yp1f=yy*calf + slicen
		yp2f=yy*calf2 + slicen
		yp3f=yy*calf3 + slicen
		yp4f=yy*calf4 + slicen
c		  
c		  store the fixed and z-dependent component of the
c		  projection coordinates
c		  
		xprojf(j)=f1x*xp1f+f2x*xp2f+f3x*xp3f+f4x*xp4f+
     &		    f1xy*yp1f+f2xy*yp2f+f3xy*yp3f+f4xy*yp4f+xalladd
c     &		    fxfromy*ypf+xalladd
		xprojz(j)=f1x*xp1z+f2x*xp2z+f3x*xp3z+f4x*xp4z
     &		    -(f1xy*salf+f2xy*salf2+f3xy*salf3+f4xy*salf4)
c     &		    +fxfromy*ypz
		yprojf(j)=f1y*xp1f+f2y*xp2f+f3y*xp3f+f4y*xp4f+
     &		    f1yy*yp1f+f2yy*yp2f+f3yy*yp3f+f4yy*yp4f+yalladd
c     &		    fyfromy*ypf+yalladd
		yprojz(j)=f1y*xp1z+f2y*xp2z+f3y*xp3z+f4y*xp4z
     &		    -(f1yy*salf+f2yy*salf2+f3yy*salf3+f4yy*salf4)
c     &		    +fyfromy*ypz
c		  
c		  see if any y testing is needed in the inner loop by checking
c		  yproj at top and bottom in Z
c		  
		yproj=yprojf(j)+yprojz(j)*zbot
		jPROJ=YPROJ
		ip1=ipoint+(jproj-lslice)*ipdel+1
		ip2=ip1+ipdel
		if(ip1.le.nbase.or.ip2.le.nbase.or.ip1.ge.nstack
     &		    .or.ip2.ge.nstack.or.jproj.lt.1.or.jproj.ge.mprj)
     &		    ifytest=1
		yproj=yprojf(j)+yprojz(j)*ztop
		jPROJ=YPROJ
		ip1=ipoint+(jproj-lslice)*ipdel+1
		ip2=ip1+ipdel
		if(ip1.le.nbase.or.ip2.le.nbase.or.ip1.ge.nstack
     &		    .or.ip2.ge.nstack.or.jproj.lt.1.or.jproj.ge.mprj)
     &		    ifytest=1
	      enddo
c		
c		walk in from each end until xproj is safely within bounds
c		to define region where no x checking is needed
c		
	      jtstlft=0
	      j=1
	      do while(jtstlft.eq.0.and.j.lt.iwide)
		if(min(xprojf(j)+zbot*xprojz(j),
     &		    xprojf(j)+ztop*xprojz(j)).ge.1)jtstlft=j
		j=j+1
	      enddo
	      if(jtstlft.eq.0)jtstlft=iwide
c		
	      jtstrt=0
	      j=iwide
	      do while(jtstrt.eq.0.and.j.gt.1)
		if(max(xprojf(j)+zbot*xprojz(j),
     &		    xprojf(j)+ztop*xprojz(j)).lt.nprj)jtstrt=j
		j=j-1
	      enddo
	      if(jtstrt.eq.0)jtstrt=1
	      if(jtstrt.lt.jtstlft)then
		jtstrt=iwide/2
		jtstlft=jtstrt+1
	      endif
c		
	      INDEX=IMAP
C		
c		loop over the slice, outer loop on z levels
c
	      DO I=1,ITHICK
		ZZ=(I-YCEN)*compress(nv)
		jlft=max(jtstlft,masklft(i))
		jrt=min(jtstrt,maskrt(i))
		index=index+masklft(i)-1
c		  
c		  set up to do inner loop in three regions of X
c		  
		jstrt(1)=masklft(i)
		jend(1)=jlft-1
		jstrt(2)=jlft
		jend(2)=jrt
		jstrt(3)=jrt+1
		jend(3)=maskrt(i)
		do jregion=1,3
		  if(jregion.ne.2.or.ifytest.eq.1)then
c		      
c		      loop involving full testing - either left or right
c		      sides needing x testing, or anywhere if y testing
c		      needed
c		      
		    do j=jstrt(jregion),jend(jregion)
		      xproj=xprojf(j)+zz*xprojz(j)
		      yproj=yprojf(j)+zz*yprojz(j)
		      if(xproj.ge.1.and.xproj.le.nprj.and.
     &			  yproj.ge.1..and.yproj.le.mprj)then
c
			IPROJ=XPROJ
			iproj=min(nprj-1,iproj)
			XFRAC=XPROJ-IPROJ
			jPROJ=YPROJ
			jproj=min(mprj-1,jproj)
			YFRAC=YPROJ-JPROJ
c			
			ip1=ipoint+(jproj-lslice)*ipdel+iproj
			ip2=ip1+ipdel
			if(ip1.ge.nbase.and.ip2.ge.nbase.and.
     &			    ip1.lt.nstack .and.ip2.lt.nstack)
     &			    ARRAY(INDEX)=ARRAY(INDEX)+
     &			    (1.-yfrac)*((1.-XFRAC)*ARRAY(IP1)
     &			    +XFRAC*ARRAY(IP1+1)) +
     &			    yfrac*((1.-XFRAC)*ARRAY(IP2)
     &			    +XFRAC*ARRAY(IP2+1))
		      endif
		      index=index+1
		    enddo   
c		      
c		      loop for no x-testing and no y testing
c		      
		  else
		    call bpsumlocal(array,index,zz,xprojf,xprojz,yprojf,
     &			yprojz,ipoint,ipdel,lslice,jstrt(jregion),
     &			jend(jregion))
		  endif
		enddo		    
		index=index+iwide-maskrt(i)
	      enddo
C-------------------------------------------
C
C End of projection loop
	      IPOINT=IPOINT+NPRJ2
	    enddo
	  ENDDO
	endif
	RETURN
	END
C
C-------------------------------------------------------------------------
c	  
c	  COMPOSE will interpolate the output slice LSLICEOUT from vertical
c	  slices in the ring buffer, where LVSSTART and LVSEND are the starting
c	  and ending slices in the ring buffer, IDIR is the direction of
c	  reconstruction, and IRINGSTART is the position of LVSSTART in the
c	  ring buffer.
c
	subroutine compose(lsliceout,lvsstart,lvsend,idir,iringstart)
	include 'tilt.inc'
	integer*4 ind1(4),ind2(4),ind3(4),ind4(4)
	tanalpha=sal(1)/cal(1)
	vertcen=ithick/2+0.5
c	  
c	  loop on lines of data
c
	do j=1,ithickout
	  cenj=j-(ithickout/2+0.5)
	  cenl=lsliceout-slicen
c	    
c	    calculate slice number and y position in vertical slices
c
	  vsl=cenl*cal(1)-cenj*sal(1)+slicen+yoffset*sal(1)
	  vycen=cenl*sal(1)+cenj*cal(1)
	  ivsl=vsl
	  fx=vsl-ivsl
	  ifmiss=0
c	    
c	    for each of 4 slices needed for cubic interpolation, initialize
c	    data indexes at zero then see if slice exists in ring
c
	  do i=1,4
	    ind1(i)=0
	    ind2(i)=0
	    ind3(i)=0
	    ind4(i)=0
	    lvsl=ivsl+i-2
	    if(idir*(lvsl-lvsstart).ge.0.and.idir*(lvsend-lvsl).ge.0)then
c		
c		if slice exists, get base index for the slice, compute the
c		y index in the slice, then set the 4 data indexes if they
c		are within the slice
c
	      iring=idir*(lvsl-lvsstart)+iringstart
	      if(iring.gt.nvertneed)iring=iring-nvertneed
	      ibase=imap+ithwid+(iring-1)*ithick*iwide
	      vy=vycen+vertcen-nint(tanalpha*(lvsl-slicen))
	      ivy=vy
	      fy=vy-ivy
	      if(ivy-1.ge.1.and.ivy-1.le.ithick)ind1(i)=ibase+iwide*(ivy-2)
	      if(ivy.ge.1.and.ivy.le.ithick)ind2(i)=ibase+iwide*(ivy-1)
	      if(ivy+1.ge.1.and.ivy+1.le.ithick)ind3(i)=ibase+iwide*ivy
	      if(ivy+2.ge.1.and.ivy+2.le.ithick)ind4(i)=ibase+iwide*(ivy+1)
	    endif
	    if(ind1(i).eq.0.or.ind2(i).eq.0.or.ind3(i).eq.0.or.
     &		ind4(i).eq.0)ifmiss=1
	  enddo
	  ibase=imap+(j-1)*iwide-1
	  if(intordxtilt.gt.2.and.ifmiss.eq.0)then
c	      
c	      cubic interpolation if selected, and no data missing
c	      
	    fx1=2.*fx**2-fx**3-fx
	    fx2=fx**3-2.*fx**2+1
	    fx3=fx**2+fx-fx**3
	    fx4=fx**3-fx**2
	    fy1=2.*fy**2-fy**3-fy
	    fy2=fy**3-2.*fy**2+1
	    fy3=fy**2+fy-fy**3
	    fy4=fy**3-fy**2
	    do i=1,iwide
	      v1=fx1*array(ind1(1))+fx2*array(ind1(2))+
     &		  fx3*array(ind1(3))+fx4*array(ind1(4))
	      v2=fx1*array(ind2(1))+fx2*array(ind2(2))+
     &		  fx3*array(ind2(3))+fx4*array(ind2(4))
	      v3=fx1*array(ind3(1))+fx2*array(ind3(2))+
     &		  fx3*array(ind3(3))+fx4*array(ind3(4))
	      v4=fx1*array(ind4(1))+fx2*array(ind4(2))+
     &		  fx3*array(ind4(3))+fx4*array(ind4(4))
	      array(ibase+i)=fy1*v1+fy2*v2+fy3*v3+fy4*v4
	      do k=1,4
		ind1(k)=ind1(k)+1
		ind2(k)=ind2(k)+1
		ind3(k)=ind3(k)+1
		ind4(k)=ind4(k)+1
	      enddo
	    enddo
	  elseif(intordxtilt.eq.2.and.ifmiss.eq.0)then
c	      
c	      quadratic interpolation if selected, and no data missing
c	      shift to next column or row if fractions > 0.5
c	      
	    indcen=2
	    if(fx.gt.0.5)then
	      indcen=3
	      fx=fx-1.
	    endif
	    if(fy.le.0.5)then
	      jnd5=ind2(indcen)
	      jnd2=ind1(indcen)
	      jnd8=ind3(indcen)
	      jnd4=ind2(indcen-1)
	      jnd6=ind2(indcen+1)
	    else
	      fy=fy-1.
	      jnd5=ind3(indcen)
	      jnd2=ind2(indcen)
	      jnd8=ind4(indcen)
	      jnd4=ind3(indcen-1)
	      jnd6=ind3(indcen+1)
	    endif
c	      
c	      get coefficients and do the interpolation
c
	    f5=1.-fx**2-fy**2
	    f2=(fy**2-fy)/2.
	    f8=f2+fy
	    f4=(fx**2-fx)/2.
	    f6=f4+fx
	    do i=1,iwide
	      array(ibase+i)=f5*array(jnd5)+f2*array(jnd2)+
     &		  f4*array(jnd4)+f6*array(jnd6)+f8*array(jnd8)
	      jnd5=jnd5+1
	      jnd2=jnd2+1
	      jnd4=jnd4+1
	      jnd6=jnd6+1
	      jnd8=jnd8+1
	    enddo
	  else
c	      
c	      linear interpolation
c
c	    print *,j,ind2(2),ind2(3),ind3(2),ind3(3)
	    if(ind2(2).eq.0.or.ind2(3).eq.0.or.ind3(2).eq.0.or.
     &		ind3(3).eq.0)then
c		
c		if there is a problem, see if it can be rescued by shifting
c		center back to left or below
c
	      if(fx.lt.0.02.and.ind2(1).ne.0.and.ind3(1).ne.0.and.
     &		  ind2(2).ne.0.and.ind3(2).ne.0)then
		fx=fx+1
		ind2(3)=ind2(2)
		ind2(2)=ind2(1)
		ind3(3)=ind3(2)
		ind3(2)=ind3(1)
	      elseif(fy.lt.0.02.and.ind1(2).ne.0.and.ind1(3).ne.0.and.
     &		  ind2(2).ne.0.and.ind3(2).ne.0)then
		fy=fy+1
		ind3(2)=ind2(2)
		ind2(2)=ind1(2)
		ind3(3)=ind2(3)
		ind2(3)=ind1(3)
	      endif
	    endif
c	      
c	      do linear interpolation if conditions are right, otherwise 0 fill
c
	    if(ind2(2).ne.0.and.ind2(3).ne.0.and.ind3(2).ne.0.and.
     &		ind3(3).ne.0)then
	      f22=(1.-fy)*(1.-fx)
	      f23=(1.-fy)*fx
	      f32=fy*(1.-fx)
	      f33=fy*fx
	      do i=1,iwide
		array(ibase+i)=f22*array(ind2(2))+f23*array(ind2(3))+
     &		    f32*array(ind3(2))+f33*array(ind3(3))
		ind2(2)=ind2(2)+1
		ind2(3)=ind2(3)+1
		ind3(2)=ind3(2)+1
		ind3(3)=ind3(3)+1
	      enddo
	    else
c	      print *,'filling',j
	      do i=1,iwide
		array(i+ibase)=0
	      enddo
	    endif
	  endif
	enddo
	return
	end
C
C-------------------------------------------------------------------------
	SUBROUTINE DUMP(LSLICE,DMIN,DMAX,DTOT)
C	--------------------------------------
C
	include 'tilt.inc'
	nparextra=100
	IEND=IMAP+ITHickout*iwide-1
C
C Scale
c	  DNM simplified and fixed bug in getting min/max/mean
	dtmp=0.
	DO 1 I=IMAP,IEND
	  IF(MASK.and.array(i).eq.tmask)THEN
C--------------mask
	    ARRAY(I)=RMASK
	  ELSE
C--------------Scale
	    ARRAY(I)=(ARRAY(I)+FLEVL)*SCALE
	  END IF
	  DMIN=AMIN1(ARRAY(I),DMIN)
	  DMAX=AMAX1(ARRAY(I),DMAX)
	  DTmp=DTmp+ARRAY(I)
C
1       CONTINUE
	dtot=dtot+dtmp
C
C Dump slice
	IF(PERP)THEN
C ....slices correspond to sections of map
		CALL IWRSEC(2,ARRAY(IMAP))
	ELSE
C ....slices must be properly stored
C	Take each line of array and place it in the correct section
C 	of the map.
	  INDEX=IMAP
	  DO J=1,ITHICKout
	    CALL IMPOSN(2,J-1,(LSLICE-ISLICE)/idelslice)
	    CALL IWRLIN(2,ARRAY(INDEX))
c	      
c	      DNM 2/29/01: partially demangle the parallel output by writing
c	      up to 100 lines at a time in this plane
c	      
	    if(mod((LSLICE-ISLICE)/idelslice,nparextra).eq.0)then
	      do i=1,min(nparextra-1,(jslice-lslice)/idelslice)
		CALL IWRLIN(2,ARRAY(INDEX))
	      enddo
	    endif
	    INDEX=INDEX+IWIDE
	  END DO
	END IF
C
	RETURN
	END
C -----------------------------------------------------------------
	SUBROUTINE INPUT
C	----------------
	
	implicit none
	include 'tilt.inc'
	integer*4 nweight
	real*4 wincr(20)
	COMMON /DENSWT/nweight,wincr
C
	integer*4 MPXYZ(3),MOXYZ(3),NOXYZ(3),NXYZST(3)
	real*4 outilt(3),cell(6),dtor
	data outilt/90.,0.,0./
	data cell/0.,0.,0.,90.,90.,90./
	DATA DTOR/0.0174532/
	CHARACTER DAT*9,TIM*8
	real*4 delta(3)
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
C
	CHARACTER TAGS(33)*20,CARD*80
	CHARACTER*80 FILIN,FILOUT
	DATA TAGS/'*TITLE','SLICE','THICKNESS','MASK','RADIAL',
     &	    'OFFSET','SCALE','PERPENDICULAR','PARALLEL','MODE',
     &	    'INCLUDE','EXCLUDE','*EXCLUDELIST','*EXCLUDELIST2','LOG',
     &	    'REPLICATE', 'ANGLES','COMPRESS',
     &	    'COMPFRACTION','DENSWEIGHT','*TILTFILE','WIDTH','SHIFT',
     &	    '*XTILTFILE','XAXISTILT','*LOCALFILE','LOCALSCALE',
     &	    'FULLIMAGE','SUBSETSTART','COSINTERP','FBPINTERP',
     &	    'XTILTINTERP','DONE'/
	integer*4 ntags, nfields
	real*4 XNUM(50)
	COMMON /CARDS/NTAGS,XNUM,NFIELDS
c	  
	integer*4 ivexcl(limview)
	real*4 repinc(limview)
	include 'fbpswitch.inc'
	integer*4 mode,newangles,iftiltfile,nvuse,nvexcl
	real*4 pmean, delang,compfac,globalpha,xoffset,scalelocal,rrmax,rfall
	integer*4 irmax,ifall,ncompress,nxfull,nyfull,ixsubset,iysubset
	integer*4 interpfbp,kti,indbase,ipos,idtype,lens,label
	integer*4 nd1,nd2,nv,nslice,indi,i,iex,nvorig,irep,iv
	real*4 vd1,vd2,dtheta,theta,thetanv,rmax,sdg,oversamp,scalescl
	integer*4 nprjp,nwidep,needwrk,needzwrk,neediw,needrw,needout,minsup
	integer*4 maxsup,nshift,nprj2,nsneed,ninp,nexclist,j,needzw,ind
	integer*4 npadtmp,nprpad,localPixel
	integer*4 inum,licenseusfft,niceframe
c
	NTAGS = 33
	WRITE(6,50)
	call getinout(2,filin,filout)
C-------------------------------------------------------------
C
C Open input projection file
	CALL IMOPEN(1,FILIN,'RO')
	CALL IRDHDR(1,NPXYZ,MPXYZ,MODE,PMIN,PMAX,PMEAN)
	NVIEWS=NPXYZ(3)
c
	if (nviews.gt.limview) call errorexit
     &	    ('Too many images in tilt series.')
	newangles=0
	iftiltfile=0
c
C-------------------------------------------------------------
C Set up defaults for missing cards:
C
C......Default slice is all rows in a projection plane
	ISLICE=1
	JSLICE=NPXYZ(2)
	idelslice=1
C......and has the same number of columns.
	IWIDE=NPXYZ(1)
C......Default is no mask and default mask is zero
	MASK=.FALSE.
	RMASK=0.
C......Default is no scaling of output map
	FLEVL=0.
	SCALE=1.
C......Default is no offset or rotation
	DELANG=0.
c......Default is output mode 2
	newmode=2
c......Start with no list of views to use or exclude
	nvuse=0
	nvexcl=0
c......Default is no logarithms
	iflog=0
c......Default is no replications
	nreplic=1
	repinc(1)=0.
C......Default radial weighting parameters - no filtering
	irmax = npxyz(1) / 2 + 1
	ifall = 0
c......Default overall and individual compression of 1; no alpha tilt
	ncompress=0
	compfac=1.
	do nv=1,nviews
	  compress(nv)=1.
	  alpha(nv)=0.
	enddo
	ifalpha=0
	globalpha=0.
c	  
c......Default weighting by density of adjacent views
	nweight=2
	do i=1,nweight
	  wincr(i)=1./(i-0.5)
	enddo
c	  
	iwide=0
	xoffset=0
	yoffset=0
	delxx=0.
	nxwarp=0
	nywarp=0
	scalelocal=0.
	nxfull=0
	nyfull=0
	ixsubset=0
	iysubset=0
c	  
c......Default double-width linear interpolation in cosine stretching
	interpfac=2
	interpord=1
	intordxtilt=1
	interpfbp=-1
c
c......Default title
	CALL DATE(DAT)
	CALL TIME(TIM)
c
c 7/7/00 CER: remove the encodes
c
c       encode(80,49,title)'Tomographic reconstruction',dat,tim
        write(titlech,49) 'Tomographic reconstruction',dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20)
C
C
C-------------------------------------------------------------
C Begin reading in cards
C
1	CALL CREAD(CARD,TAGS,LABEL,*999)
	GO TO (100,200,300,400,500,600,700,800,900,1000,1100,1200,1250,
     &	    1250,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,
     &	    2400,2500, 2600,2700,2800,2900,3000,999),LABEL
C
C TITLE card
C100     ENCODE(80,49,TITLE)CARD(1:50),DAT,TIM
100     write(titlech,49) CARD(1:50),DAT,TIM
	read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
	WRITE(6,101)TITLE
	GO TO 1
C
C SLICE card
200	IF(NFIELDS/2.NE.1)GO TO 9999
	ISLICE=INUM(1)+1
	JSLICE=INUM(2)+1
	if(nfields.gt.2)idelslice=inum(3)
	WRITE(6,201)ISLICE,JSLICE,idelslice
	GO TO 1
C
C THICKNESS card
300	IF(NFIELDS.NE.1)GO TO 9999
	ITHICK=INUM(1)
	WRITE(6,301)ITHICK
	if(ithick.gt.limmask)call errorexit(
     &	    'Thickness too high for arrays')
	GO TO 1
C
C MASK card
400	IF(NFIELDS.EQ.1)RMASK=XNUM(1)
	MASK=.TRUE.
	WRITE(6,401)RMASK
	GO TO 1
C
C RADIAL card
500	IF(NFIELDS.EQ.0 .OR. NFIELDS.GE.3)GO TO 9999
	IF(NFIELDS.EQ.2)rFALL=xNUM(2)
	rRMAX=xNUM(1)
	irmax=rrmax
	ifall=rfall
	if(irmax.eq.0)irmax=npxyz(1)*rrmax
	if(ifall.eq.0)ifall=npxyz(1)*rfall
	WRITE(6,501)IRMAX,IFALL
	GO TO 1
C
C OFFSET card
600	IF(NFIELDS.EQ.0 .OR. NFIELDS.GE.3)GO TO 9999
	IF(NFIELDS.EQ.2)DELXX=XNUM(2)
	DELANG=XNUM(1)
	WRITE(6,601)DELANG,DELXX
	GO TO 1
C
C SCALE card
700	IF(NFIELDS.EQ.0 .OR. NFIELDS.GE.3)GO TO 9999
	IF(NFIELDS.EQ.2)SCALE=XNUM(2)
	FLEVL=XNUM(1)
	WRITE(6,701)FLEVL,SCALE
	GO TO 1
C
C PERPENDICULAR card
800	PERP=.TRUE.
	WRITE(6,801)
	GO TO 1
C
C PARALLEL card
900	PERP=.FALSE.
	WRITE(6,901)
	GO TO 1
c	  
c MODE card
1000	newmode=inum(1)
	if(newmode.lt.0.or.newmode.gt.15 .or.
     &	    (newmode.gt.2.and.newmode.lt.9))call errorexit(
     &	    'Illegal output mode')
	write(6,1001)newmode
	go to 1
C
c INCLUDE card
1100	if(nvexcl.gt.0)call errorexit(
     &	    'Illegal to have both INCLUDE and EXCLUDE cards')
	do 1110 i=1,nfields
	  if(inum(i).lt.1.or.inum(i).gt.nviews) call errorexit(
     &	      'Illegal view number in INCLUDE list')
	  nvuse=nvuse+1
	  mapuse(nvuse)=inum(i)
1110	continue 
	go to 1
C
c EXCLUDE card
1200	if(nvuse.gt.0)call errorexit(
     &	    'Illegal to have both INCLUDE and EXCLUDE cards')
	do 1210 i=1,nfields
	  if(inum(i).lt.1.or.inum(i).gt.nviews)call errorexit(
     &	      'Illegal view number in EXCLUDE list')
	  nvexcl=nvexcl+1
	  ivexcl(nvexcl)=inum(i)
1210	continue 
	go to 1
c	  
c EXCLUDELIST card
1250	if(nvuse.gt.0)call errorexit(
     &	    'Illegal to have both INCLUDE and EXCLUDE cards')
	call parselist(card,ivexcl(nvexcl+1),nexclist)
	do i=nvexcl+1,nvexcl+nexclist
	  if(ivexcl(i).lt.1.or.ivexcl(i).gt.nviews)call errorexit(
     &	      'Illegal view number in EXCLUDE list')
	enddo
	nvexcl=nvexcl+nexclist
	go to 1
c	  
c LOG card
1300	iflog=1
	baselog=xnum(1)
	write(6,1301)baselog
	go to 1
c	  
c REPLICATE card
1400	do i=1,inum(1)
	  nreplic=nreplic+1
	  repinc(nreplic)=xnum(2)*i
	enddo
	write(6,1401)inum(1),xnum(2)
	go to 1
c	  
c ANGLES card
1500	do 1510 i=1,nfields
	  newangles=newangles+1
	  angles(newangles)=xnum(i)
1510	continue
	go to 1
c	  
c COMPRESS card
1600	do 1610 i=1,nfields
	  ncompress=ncompress+1
	  compress(ncompress)=xnum(i)
1610	continue
	go to 1
c	  
c COMPFACTOR card
1700	compfac=xnum(1)
	write(6,1701)compfac
	go to 1
c	  
c DENSWEIGHT card
1800	nweight=inum(1)
	if(nweight.gt.0)then
	  do i=1,nweight
	    wincr(i)=1./(i-0.5)
	  enddo
	  if(nfields.eq.nweight+1)then
	    do i=1,nweight
	      wincr(i)=xnum(i+1)
	    enddo
	  elseif(nfields.ne.1)then
	    go to 9999
	  endif
	  write(6,1801)nweight,(wincr(i),i=1,nweight)
	else
	  write(6,1802)
	endif
	go to 1
c	  
c TILTFILE card
1900	call dopen(3,card,'ro','f')
	read(3,*)(angles(i),i=1,nviews)
	close(3)
	iftiltfile=1
	go to 1
c	  
c WIDTH card
2000	IF(NFIELDS.NE.1)GO TO 9999
	IWIDE=INUM(1)
	WRITE(6,2001)IWIDE
	GO TO 1
c	  
c SHIFT card
2100	if((nfields+1)/2.ne.1)go to 9999
	xoffset=xnum(1)
	if(nfields.eq.2)yoffset=xnum(2)
	WRITE(6,2101)xoffset,yoffset
	go to 1
c	  
c XTILTFILE card
2200	call dopen(3,card,'ro','f')
	read(3,*)(alpha(i),i=1,nviews)
	close(3)
	do i=1,nviews
	  if(abs(alpha(i)).gt.1.e-5)ifalpha=2
	enddo
	write(6,2201)
	go to 1
c	  
c XAXISTILT card
2300	globalpha=xnum(1)
	write(6,2301)globalpha
	if(abs(globalpha).gt.1.e-5.and.ifalpha.eq.0)ifalpha=1
	go to 1
c	  
c LOCALFILE card
2400	call dopen(3,card,'ro','f')
	read(3,'(a)')titlech
c	read(3,*)nxwarp,nywarp,ixswarp,iyswarp,idxwarp,idywarp
	call frefor(titlech,delbeta,ninp)
	ifdelalpha=0
	if(ninp.gt.6)ifdelalpha=nint(delbeta(7))
	if (ninp .gt. 7) localPixel = delbeta(8)
	nxwarp=nint(delbeta(1))
	nywarp=nint(delbeta(2))
	ixswarp=nint(delbeta(3))
	iyswarp=nint(delbeta(4))
	idxwarp=nint(delbeta(5))
	idywarp=nint(delbeta(6))
	if(nxwarp*nywarp.gt.limwpos.or.nxwarp*nywarp*nviews.gt.limwarp)
     &	    call errorexit(
     &	    'ARRAY SIZE INSUFFICIENT FOR LOCAL TILT ALIGNMENT DATA')
	indbase=0
	do ipos=1,nxwarp*nywarp
	  indwarp(ipos)=indbase
	  read(3,*)(delbeta(i),i=indbase+1,indbase+nviews)
	  if(ifdelalpha.gt.0)then
     	      read(3,*)(delalpha(i),i=indbase+1,indbase+nviews)
	  else
	    do i=indbase+1,indbase+nviews
	      delalpha(i)=0.
	    enddo
	  endif
	  do i=1,nviews
	    call xfread(3,fw(1,1,i+indbase),2410,2410)
	  enddo
	  indbase=indbase+nviews
	enddo
	close(3)
	write(6,2401)
	go to 1
2410	call errorexit(
     &	    'ERROR READING LOCAL TILT ALIGNMENT DATA FROM FILE')
c	  
c LOCALSCALE card
2500	scalelocal=xnum(1)
	write(6,2501)scalelocal
	go to 1
c	  
c FULLIMAGE card
2600	if(nfields.ne.2) go to 9999
	nxfull=inum(1)
	nyfull=inum(2)
	write(6,2601)nxfull,nyfull
	go to 1
c	  
c SUBSETSTART card
2700	if(nfields.ne.2) go to 9999
	ixsubset=inum(1)
	iysubset=inum(2)
	write(6,2701)ixsubset,iysubset
	go to 1
c	  
c	  COSINTERP card
2800	interpord=xnum(1)
	if(nfields.gt.1)interpfac=xnum(2)
	interpord=max(0,min(3,interpord))
	if(interpord.eq.0)interpfac=0
	if(interpfac.eq.0)then
	  print *,'Cosine stretching is disabled'
	else
	  write(6,2801)interpord,interpfac
	endif
	go to 1
c	  
c	  FBPINTERP card
2900	interpfbp=xnum(1)
	if(interpfbp.gt.1)interpfbp=3
	if(interpfbp.le.0)then
	  print *,'Fast back projection is disabled'
	else
	  write(6,2901)interpfbp
	endif
	go to 1
c	  
c	  XTILTINTERP card
3000	intordxtilt=xnum(1)
	if(intordxtilt.gt.2)intordxtilt=3
	if(intordxtilt.le.0)then
	  print *,'New-style X-tilting with vertical slices is disabled'
	else
	  write(6,3001)intordxtilt
	endif
	go to 1
c
C End of data deck
C-------------------------------------------------------------
C
999	WRITE(6,48)
	if(ifalpha.ne.0.and.abs(idelslice).ne.1)call errorexit(
     &	    'Cannot do X axis tilt with non-consecutive slices')
	if(nxwarp.ne.0.and.abs(idelslice).ne.1)call errorexit(
     &	    'Cannot do local alignments with non-consecutive slices')
	if(nxfull.eq.0.and.nyfull.eq.0.and.
     &	    (ixsubset.ne.0.or.iysubset.ne.0))call errorexit(
     &	    'YOU MUST ENTER THE FULL IMAGE SIZE IF YOU HAVE A SUBSET')
c	  
c If NEWANGLES is 0, get angles from file header.  Otherwise check if angles OK
c
	if(newangles.eq.0.and.iftiltfile.eq.0)then
c
c	Tilt information is stored in stack header. Read into angles
c	array. All sections are assumed to be equally spaced. If not,
c	you need to set things up differently. In such a case, great
c	care should be taken, since missing views may have severe 
c	effects on the quality of the reconstruction.
c
c
	  call irtdat(1,idtype,lens,nd1,nd2,vd1,vd2)
c
	  if (idtype.ne.1) call errorexit( ' Not tilt data.')
c
	  if (nd1.ne.2) call errorexit(' Tilt axis not along Y.')
c
	  dtheta = vd1
	  theta = vd2
c
	  DO 1105 NV=1,NVIEWS
	    ANGLES(NV)=theta
	    theta = theta + dtheta
1105	  continue
c
	else
	  if(iftiltfile.eq.1.and.newangles.ne.0)then
	    call errorexit(
     &		'Tried to enter angles with both ANGLES and TILTFILE')
	  elseif(iftiltfile.eq.1)then
	    write(6,*)' Tilt angles were entered from a tilt file'
	  elseif(newangles.eq.nviews)then
	    write(6,*)' Tilt angles were entered with ANGLES card(s)'
	  else
	    call errorexit('If using ANGLES, a value must be '//
     &		'entered for each view')
	  endif
	endif
c	  
	if(ncompress.gt.0)then
	  if(ncompress.eq.nviews)then
	    write(6,*)
     &		' Compression values were entered with COMPRESS card(s)'
	  else
	    call errorexit('If using COMPRESS, a value must be '//
     &		'entered for each view')
	  endif
	  do nv=1,nviews
	    compress(nv)=1.+(compress(nv)-1.)/compfac
	  enddo
	endif
c	  
	if(globalpha.ne.0.)then
	  do iv=1,nviews
	    alpha(iv)=alpha(iv)-globalpha
	  enddo
	endif
C
C Open output map file
	if(islice.lt.1.or.jslice.lt.1.or.islice.gt.npxyz(2).or.
     &	    jslice.gt.npxyz(2)) call errorexit(
     &	    'SLICE NUMBERS OUT OF RANGE')
	NSLICE=(JSLICE-ISLICE)/idelslice+1
	if(nslice.le.0)call errorexit( 'SLICE NUMBERS REVERSED')
	if(iwide.eq.0)iwide=npxyz(1)
	if(iwide.gt.limwidth.and.nxwarp.ne.0)call errorexit('OUTPUT'//
     &	    ' SLICE TOO WIDE FOR ARRAYS IF DOING LOCAL ALIGNMENTS')
c	  
c	  DNM 7/27/02: transfer pixel sizes depending on orientation of output
c
	call irtdel(1,delta)
	NOXYZ(1)=iwide
	cell(1)=iwide*delta(1)
	IF(PERP)THEN
		NOXYZ(2)=ITHICK
		NOXYZ(3)=NSLICE
		cell(2)=ithick*delta(1)
		cell(3)=abs(nslice*idelslice)*delta(2)
	ELSE
		NOXYZ(2)=NSLICE
		NOXYZ(3)=ITHICK
		cell(3)=ithick*delta(1)
		cell(2)=abs(nslice*idelslice)*delta(2)
	END IF
	CALL IMOPEN(2,FILOUT,'NEW')
	CALL ICRHDR(2,NOXYZ,NOXYZ,newmode,title,0)
	CALL ITRLAB(2,1)
	call ialcel(2,cell)
c	  
c	  if doing perpendicular slices, set up header info to make coordinates
c	  congruent with those of tilt series in simplest case
c
	if(perp)then
	  outilt(1)=sign(90,idelslice)
	  call ialorg(2,cell(1)/2.+delxx,cell(2)/2.,
     &	      float(sign(islice-1,-idelslice)))
	  call ialtlt(2,outilt)
	endif
C
c	  if no INCLUDE cards, set up map to views, excluding any specified by
c	  EXCLUDE cards
c	  
	if(nvuse.eq.0)then
	  do 1120 i=1,nviews
	    do 1118 iex=1,nvexcl
	      if(i.eq.ivexcl(iex))go to 1120
1118	    continue
	    nvuse=nvuse+1
	    mapuse(nvuse)=i
1120	  continue
	endif
c       
c         order the MAPUSE array by angle
c       
	do i=1,nvuse-1
	  do j=i+1,nvuse
	    indi=mapuse(i)
	    if(angles(indi).gt.angles(mapuse(j)))then
	      mapuse(i)=mapuse(j)
	      mapuse(j)=indi
	      indi=mapuse(i)
	    endif
	  enddo
	enddo
c	  
c	  pack angles down as specified by MAPUSE
c
	do 1130 i=1,nvuse
	  sbet(i)=angles(mapuse(i))
	  cbet(i)=compress(mapuse(i))
	  sal(i)=alpha(mapuse(i))
1130	continue
	do 1132 i=1,nvuse
	  angles(i)=sbet(i)
	  compress(i)=cbet(i)
	  alpha(i)=sal(i)
1132	continue
	nvorig=nviews
	nviews=nvuse
	scale=scale/(nviews*nreplic)
	flevl=flevl*nviews*nreplic
c
	WRITE(6,51)(ANGLES(NV),NV=1,NVIEWS)
	WRITE(6,52)
c
C	  Set up trig tables - including all of the replications
c	  Then convert angles to radians
c
	do irep=1,nreplic
	  DO  NV=1,NVIEWS
	    iv=NV+(irep-1)*nviews
	    thetanv=ANGLES(NV)+DELANG+repinc(irep)
	    if(thetanv.gt.180.)thetanv=thetanv-360.
	    if(thetanv.le.-180.)thetanv=thetanv+360.
	    CBET(iv)=COS(thetanv*DTOR)
C	      Take the negative of the sine to invert slices around new X
c	      axis, unless slices are being output in inverse order
	    SBET(iv)=sign(1,-idelslice)*SIN(thetanv*DTOR)
	    cal(iv)=cos(alpha(nv)*dtor)
	    sal(iv)=sign(1,-idelslice)*sin(alpha(nv)*dtor)
	  enddo
	enddo
	do iv=1,nviews
	  angles(iv)=dtor*sign(1,-idelslice)*(angles(iv)+delang)
	enddo
c	  
c	  if fixed x axis tilt, set up to try to compute vertical planes
c	  and interpolate output planes: adjust thickness that needs to 
c	  be computed, and find number of vertical planes that are needed
c	  
	ithickout=ithick
	if(ifalpha.eq.1.and.nxwarp.eq.0.and.intordxtilt.gt.0)then
	  ifalpha=-1
	  ithickout=ithick
	  ithick=ithick/cal(1)+4.5
	  nvertneed=ithickout*abs(sal(1))+5.
	endif
C
C	    Set centre of output plane and center of input for transformations
c
	nxfull=max(nxfull,npxyz(1))
	nyfull=max(nyfull,npxyz(2))
	xcenin=nxfull/2.+0.5-ixsubset
	slicen=nyfull/2.+0.5-iysubset
	xoffset=xoffset-(npxyz(1)/2+ixsubset-nxfull/2)
	XCEN=IWIDE/2+0.5+delxx+xoffset
	YCEN=ITHICK/2+0.5+yoffset
c	  
c	  determine if fast bp can be used
c	  
	fastbp=interpfbp.ne.0
	if(fastbp)then
	  fastbp=licenseusfft().eq.0
	  if(.not.fastbp)then
	    write(*,'(/,a,/,a,/)')' No fast back projection: No '//
     &		'license to use USFFT libraries',
     &		' available or specified'
	  else
	    fastbp=ifalpha.le.0.and.nxwarp.eq.0
	    if(.not.fastbp)then
	      write(*,'(/a/)')' No fast back projection is available'
     &		  //' with old-style X-tilt or local alignments'
	    else
	      fastbp=.not.mask.and.delxx.eq.0..and.nreplic.eq.1.and.
     &		  ncompress.eq.0.and.iwide.le.npxyz(1).and.xoffset.eq.0.
	      if(.not.fastbp)write(*,'(/,a,/,a,/)')' No fast back '//
     &		  'projection is available with shift, offset, mask,'
     &		  //' replication',' or compression options, or if'//
     &		  ' output width > input width'
	    endif
	  endif
	endif
c	  
c	  Next check whether dimensions are below cutoff, unless the user has
c	  entered an fbpinterp line
c
	if(fastbp.and.interpfbp.lt.0)then
	  do i=1,nfbplimits
	    if (iwide.lt.limitfbp(1,i).and.ithick.lt.limitfbp(2,i)
     &		.and.nviews.lt.limitfbp(3,i)) fastbp = .false.
	  enddo
	  if(.not.fastbp)write(*,'(/,a,/,a,/)')' Fast back-projection'
     &	      //' will not be used because it would probably be slower',
     &	      ' for these values of width, thickness, and number'//
     &	      ' of views'
	endif
	interpfbp = abs(interpfbp)
c	  
c	  If qualify for fast bp, set up working arrays and make sure there
c	  is enough space.  Have to compute here items that are ordinarily
c	  computed in main program
c
	if(fastbp)then

	  if(iwide.lt.nprj)then
c	    
c	      if subset in width, find out the min and max coordinates
c	      actually needed and set the effective nprj for fbp from that
c
	    call fbpneed(angles,nprj,nviews,iwide,ithick, 1,
     &		array,array(1+3*nviews),nprjp,nwidep,needwrk,needzwrk,
     &		neediw,needrw,needzw, needout,minsup,maxsup)
	    nshift=(nprj-(maxsup+1-minsup))/2
	    nprjfbp=nprj-2*nshift
	  else
	    nprjfbp=nprj
	  endif

	  call fbpneed(angles,nprjfbp,nviews,iwide,ithick, 1,
     &	      array,array(1+3*nviews),nprjp,nwidep,needwrk,needzwrk,
     &	      neediw,needrw,needzw, needout,minsup,maxsup)

	  fastbp=nprjp.gt.0
	  if(fastbp)then
	    npad=max(0,nprjp-nprj)
	    nprj2=nprj+npad
	    IPLANE=NPRJ2*NVIEWS
c
	    ifbpwrk=1
	    ifbpzwrk=ifbpwrk + needwrk
	    ifbpiw=ifbpzwrk + 2 * needzwrk
	    ifbprw=ifbpiw + neediw
	    ifbpzw=ifbprw + needrw
	    imap=ifbpzw + 2 * needzw
	    if(ifalpha.eq.0)then
	      NBASE=IMAP+needout
	    else
	      nbase=imap+needout+nvertneed*iwide*ithick
	    endif
	    ithwid=needout
	    fastbp=(limstack+1-nbase).ge.iplane
	    if(.not.fastbp)write(*,'(/a/)')' Stack is not large enough'
     &		//' to use fast back projection'
	  else
	    write(*,'(/a/)')' No fast back projection: This program '//
     &		'was not built with USFFT libraries'
	  endif

	endif
c
c	  if doing fast bp, get filter parameters, set sign, and initialize now
c
	if(fastbp)then
	  rmax=irmax/float(nprj)
	  sdg=ifall/float(nprj)

c	    uncomment to get outputs from fbpini
c	  call prini(6,0) 
c	  call getini 
	  oversamp=float(nwidep)/iwide
	  write(*,'(/,a,f5.2,/)')' Using fast back projection with '//
     &	      'oversampling of',oversamp

	  call fbpini(angles,nprjfbp,nviews,iwide,ithick, rmax,sdg,
     &	      interpfbp,1,
     &	      array(ifbpiw),array(ifbprw),array(ifbpzw),array(ifbpwrk))
	  scalescl=1000.
	  flevl=flevl/scalescl
	  scale=scale*scalescl
	else

c	  
c Set up padding: 10% of X size or minimum of 16, max of 50
	  npadtmp=min(50,2*max(8,npxyz(1)/20))
	  nprpad=niceframe(2*((npxyz(1)+npadtmp)/2),2,19)
	  npad=nprpad-npxyz(1)
	  ITHWID=IWIDE*ITHICK
c	    
c	    next evaluate cosine stretch
c
	  ipextra=0
	  NPRJ2=NPRJ+2+npad
	  IPLANE=NPRJ2*NVIEWS
	  IMAP=IPLANE+1
	  if(ifalpha.ge.0)then
C	      
C	      anything but new-style X-axis tilt
C
	    NBASE=IMAP+ITHWID
	    if(nxwarp.eq.0.and.interpfac.gt.0)then
	      call set_cos_stretch(nsneed)
c		
c		set size of plane as max of loading size and stretched size
c		also set that an extra plane is needed
c		if there is not enough space for the planes needed, then
c		disable stretching and drop back to regular code
c
	      iplane=max(iplane,indstretch(nviews+1))
	      ipextra=iplane
	      if((maxSTACK-NBASE-ipextra+1)/IPLANE.lt.nsneed)then
		ipextra=0
		iplane=nprj2*nviews
		interpfac=0
		write(*,62)
62		format(/,'There is insufficient stack space ',
     &		    'to use cosine stretching')
	      endif
	    endif

	  else
c	      
c	      new-style X-axis tilt
c
	    nbase=imap+ithwid*(nvertneed+1)
	    if(interpfac.gt.0)then
	      call set_cos_stretch(nsneed)
c		
	      iplane=max(iplane,indstretch(nviews+1))
	      ipextra=iplane
	      if((maxSTACK-NBASE-ipextra+1)/IPLANE.lt.nsneed)then
c		  
c		  cancel the cosine stretch provisionally if it doesn't fit
c		  
		ipextra=0
		iplane=nprj2*nviews
	      endif
	    endif
	    if(ipextra.eq.0)then
c		
c		if not setup for cosine stretch, first see if new-style even
c		fits in stack space.
c
	      if(maxstack-nbase.gt.iplane)then
		if(interpfac.gt.0)then
		  interpfac=0
		  write(*,62)
		endif
	      else
c
c		  if not, drop back to old style tilting - first try
c		  for cosine stretching again
c		  
		ifalpha=1
		ithick=ithickout
		ithwid=iwide*ithick
		NBASE=IMAP+ITHWID
		write(*,'(/,a)')'Insufficient stack space for '//
     &		    'new-style X-axis tilting'
		if(interpfac.gt.0)then
		  call set_cos_stretch(nsneed)
c		    
		  iplane=max(iplane,indstretch(nviews+1))
		  ipextra=iplane
		  if((maxSTACK-NBASE-ipextra+1)/IPLANE.lt.nsneed)then
c		      
c		      old style tilt, no cosine stretch
c		      
		    ipextra=0
		    iplane=nprj2*nviews
		    interpfac=0
		    write(*,62)
		  endif
		endif
	      endif
	    endif
	  endif
c	  print *,interpfac,ipextra,ifalpha,nvertneed
c
C Set up radial weighting
	  CALL RADWT(IRMAX,IFALL)
	endif
c	
c	if doing warping, convert the angles to radians and set sign
c
	if(nxwarp.gt.0)then
	  do i=1,nvorig*nxwarp*nywarp
	    delbeta(i)=dtor*sign(1,-idelslice)*delbeta(i)
	  enddo
	  do iv=1,nviews
	    do i=1,nxwarp*nywarp
	      ind=indwarp(i)+mapuse(iv)
	      cwarpb(ind)=cos(angles(iv)+delbeta(ind))
	      swarpb(ind)=sin(angles(iv)+delbeta(ind))
	      cwarpa(ind)=cos(dtor*(alpha(iv)+delalpha(ind)))
	      swarpa(ind)=sign(1,-idelslice)*
     &		  sin(dtor*(alpha(iv)+delalpha(ind)))
	    enddo
	  enddo
c	    
c	    See if local scale was entered; if not see if it can be set from
c	    pixel size and local align pixel size
	  if (scalelocal .le. 0.) then
	    scalelocal = 1.
	    if (localPixel .gt. 0) then
	      scalelocal = localPixel / delta(1)
	      if (abs(scalelocal - 1.).gt.0.001) write(6,53)scaleLocal
	    endif
	  endif
c	    
c	    scale the x and y dimensions and shifts if aligned data were
c	    shrunk relative to the local alignment solution
c
	  if(scalelocal.ne.1.)then
	    ixswarp=nint(ixswarp*scalelocal)
	    iyswarp=nint(iyswarp*scalelocal)
	    idxwarp=nint(idxwarp*scalelocal)
	    idywarp=nint(idywarp*scalelocal)
	    do i=1,nxwarp*nywarp*nviews
	      fw(1,3,i)=fw(1,3,i)*scalelocal
	      fw(2,3,i)=fw(2,3,i)*scalelocal
	    enddo
	  endif
c	    
c	    if the input data is a subset in X or Y, subtract starting
c	    coordinates from ixswarp and iyswarp
c	    
	  ixswarp=ixswarp-ixsubset
	  iyswarp=iyswarp-iysubset
	endif
	RETURN
C
9999	call errorexit('Wrong number of fields on card')
C
C
48	FORMAT(//,1X,78('-'))
49	FORMAT('TILT: ',a,t57,A9,2X,A8)
50	FORMAT(
	1//,' THREE-D RECONSTRUCTION FROM SERIES OF PROJECTIONS '
	2,' ABOUT A COMMON TILT AXIS'/,1x,76('-')///)
51	FORMAT(/' Projection angles:'//(8F9.2))
52	FORMAT(//,1X,78('-'))
53	format(/,'Scaling of local alignments by ',f8.3,
     &	    ' determined from pixel sizes')
101	FORMAT(/' Title:    ',20A4)
201	FORMAT(/' Rows',I4,' to',I4,', (at intervals of',i4,') of the'
	1,' projection planes will be reconstructed.')
301	FORMAT(/' Thickness of reconstructed slice is',I4,
	1 ' divisions.')
401     FORMAT(/' Mask applied to output slices:'
	1,' points lying outside the reconstructed domain set to',
	2 F6.0)
501	FORMAT(/' Radial weighting function parameters IRMAX =',I4,
	1	'  IWIDE =',I4)
601	FORMAT(/' Output map rotated by',F6.1,' degrees about tilt axis'
	1' with respect to tilt origin'/
	2 ' Tilt axis displaced by',F6.3,' grid units from centre'
	3 ,' of projection')
701	FORMAT(/' Output map densities incremented by',F8.2,
	1' and then multiplied by',F8.2)
801	FORMAT(/' Output map is sectioned perpendicular to the '
	1 ,'tilt axis')
901	FORMAT(/' Output map is sectioned parallel to the'
	1 ,' zero tilt projection')
1001	format(/' Data mode of output file is',i3)
1301	format(/' Taking logarithm of input data plus',f10.3)
1401	format(/' Replicate projection',i4,' times by',f6.1,' degrees')
1701	format(/' Compression was confined to',f6.3,
     &	    ' of the distance over which it was measured')
1801	format(/' Weighting by tilt density computed to distance of'
     &	    ,i3,' views',/,'  weighting factors:',(10f6.3))
1802	format(/' No weighting by tilt density')
2001	format(/,' Width of reconstruction is',i5,' pixels')
2101	format(/,' Output slice shifted up',f7.1,' and to right',f7.1,
     &	    ' pixels')
2201	format(/,' Alpha tilting to be applied with angles from file')
2301	format(/,' Global alpha tilt of',f6.1,' will be applied')
2401	format(/,' Local tilt alignment information read from file')
2501	format(/,' Local alignment positions and shifts reduced by',f7.4)
2601	format(/,' Full aligned stack will be assumed to be',i6,' by',
     &	    i6,' pixels')
2701	format(/,' Aligned stack will be assumed to be a subset ',
     &	    'starting at',2i6)
2801	format(/,' Cosine stretching, if any, will have interpolation',
     &	    ' order', i2,', sampling factor',i2)
2901	format(/,' Fast back projection, if any, will have ',
     &	    'interpolation order', i2)
3001	format(/,' X-tilting with vertical slices, if any, will have ',
     &	    'interpolation order', i2)

	END
C
C-----------------------------------------------------------------------
	SUBROUTINE CREAD(CARD,TAGS,LABEL,*)
C       -----------------------------------
C
C This subroutine performs the free format read. The calling program must
C contain the common block
	integer*4 ntags,nfields
	real*4 xnum(50)
	COMMON /CARDS/NTAGS,XNUM,NFIELDS
C In the calling program, TAGS is an array of Character*20 variables
C containing the data card flags. To indicate that alphanumeric data
C is expected after a particular flag, an asterisk must occur before
C the flag, e.g. *TITLE, in the appropriate TAG element signifying that
C a data record of the form "TITLE This is the Title" is expected.
C Alphanumeric data is placed in the array CARD and numeric data is
C placed element by element in the real array XNUM.  
C The function INTEG can be used for checking whether a non-integral 
C real number has occured in a field where an integer is expected.
	CHARACTER ASTER*1,BLANK*1,CARD*80,TAGS(NTAGS)*20,upcase*80
	DATA ASTER,BLANK/'*',' '/
c	  DNM: switch from Q format to using LNBLNK
c	  12/8/03: swallow blank lines
5	READ(5,10,END=999)CARD
10	FORMAT(A)
	if (card .eq. ' ') go to 5
	NCHAR = lnblnk(CARD)
	do while (nchar.gt.1.and.card(1:1).eq.blank)
	  nchar=nchar-1
	  card(1:)=card(2:)
	enddo
	WRITE(6,15)CARD
15	FORMAT(/' DATA LINE ------- : ',A80) 
C
C Find card
	call strupcase(upcase,card)
	DO 100 KTAG=1,NTAGS
	IPOINT=INDEX(TAGS(KTAG),BLANK)
	ISTAR=INDEX(TAGS(KTAG),ASTER)+1
	IF(INDEX(upcase,TAGS(KTAG)(ISTAR:IPOINT)).EQ.1)GO TO 200
100	CONTINUE
C
C Unidentified tag
	WRITE(6,20)CARD
20	FORMAT(/' Unidentified line: ',A80)
	call errorexit('Unidentified line')
C
C Tag found
200	LABEL=KTAG
	ISTART=IPOINT+1
	IF(ISTAR.EQ.2)THEN
C Character string
	CARD(1:)=CARD(ISTART-1:)
C Numeric string
	ELSE
c	  DNM: indented, added safety checks for end of line
C Zero numbers
	  DO 300 I=1,20
300	  XNUM(I)=0.
	  NFIELDS=0
C Search for starting point
	  DO WHILE (CARD(ISTART:ISTART).EQ.BLANK)
	    ISTART=ISTART+1
	  END DO
C Decode fields
	  DO WHILE (ISTART.LE.NCHAR)
	    NFIELDS=NFIELDS+1
	    NPOINT=INDEX(CARD(ISTART:),BLANK)-1
	    IEND=ISTART+NPOINT-1
	    if(npoint.lt.0)iend=nchar
c	  
c	  switch to standard f77 here, get rid of decode AND variable format
c	  also needed to be * instead f75.0 on sun
c
	    read(card(istart:iend),*)XNUM(NFIELDS)
c	    DECODE(NPOINT,30,CARD(ISTART:IEND))XNUM(NFIELDS)
c30	    FORMAT(f75.0)
	    ISTART=IEND+2
C Skip over recurring blanks
	    DO WHILE (istart.le.nchar.and.CARD(ISTART:ISTART).EQ.BLANK)
	      ISTART=ISTART+1
	    END DO
C
c	be brave and get rid of this
c	    I=I+1
	  END DO
C All numeric fields decoded
	END IF
	RETURN
C
C End of file
999	RETURN 1	
	END


	FUNCTION INUM(I)
C 	----------------
	COMMON /CARDS/NTAGS,XNUM(30),NFIELDS
	X=XNUM(I)
	INUM=INT(X)
	IF(FLOAT(INUM).EQ.X)RETURN
	WRITE(6,1)X
1	FORMAT(//' ERROR: TILT - in free format read, ',F12.2,
     &	    ' is non-integral')
	call exit(1)
	END



	subroutine local_factors(ix,iy,iv,ind1,ind2,ind3,ind4,f1,f2,f3,
     &	    f4)
c	  
	include 'tilt.inc'
c	
	ixt=min(max(ix-ixswarp,0),(nxwarp-1)*idxwarp)
	ixpos=min(ixt/idxwarp+1,nxwarp-1)
	fx=float(ixt-(ixpos-1)*idxwarp)/idxwarp
	iyt=min(max(iy-iyswarp,0),(nywarp-1)*idywarp)
	iypos=min(iyt/idywarp+1,nywarp-1)
	fy=float(iyt-(iypos-1)*idywarp)/idywarp

	ind1=indwarp(nxwarp*(iypos-1)+ixpos)+iv
	ind2=indwarp(nxwarp*(iypos-1)+ixpos+1)+iv
	ind3=indwarp(nxwarp*iypos+ixpos)+iv
	ind4=indwarp(nxwarp*iypos+ixpos+1)+iv
	f1=(1.-fy)*(1.-fx)
	f2=(1.-fy)*fx
	f3=fy*(1.-fx)
	f4=fy*fx
	return
	end


c	  
c	  Compute space needed for cosine stretched data, returning the
c	  number of needed input slices in NSNEED
c	    
	subroutine set_cos_stretch(nsneed)
	include 'tilt.inc'

c	  make the indexes be bases, numbered from 0
c	  
	indstretch(1)=0
	lsmin=min(jslice,islice)
	lsmax=max(jslice,islice)
	nsneed=1
	if(ifalpha.lt.0)then
c	    
c	    New-style X tilting: SET MINIMUM NUMBER OF INPUT SLICES HERE
c
	  nsneed=4
	  lsmin=slicen+(lsmin-slicen)*cal(1)+yoffset*sal(1)-
     &	      0.5*ithickout*abs(sal(1))-1.
	  lsmax=slicen+(lsmax-slicen)*cal(1)+yoffset*sal(1)+
     &	      0.5*ithickout*abs(sal(1))+2.
	  tanal=sal(1)/cal(1)
	  lsmin=max(1,lsmin)
	  lsmax=min(lsmax,mprj)
	endif
	do iv=1,nviews
	  xpmax=1
	  xpmin=nprj
c		
c	    find min and max position of 8 corners of reconstruction
c		
	  do ix=1,iwide,iwide-1
	    do iy=1,ithick,ithick-1
	      do lslice=lsmin,lsmax,max(1,lsmax-lsmin)
		ZZ=(IY-YCEN)*compress(iv)
		if(ifalpha.lt.0) zz=compress(iv)*
     &		    (iy-(ycen-nint(tanal*(lslice-slicen))))
		if(ifalpha.le.0)then
		  zPART=zz*SBET(iv)+XCENin+DELXX
		else
		  yy=lslice-slicen
		  zpart= yy*sal(iv)*sbet(iv) + zz*cal(iv)*sbet(iv) +
     &		      xcenin+delxx
		endif
		xproj=zpart+(ix-xcen)*cbet(iv)
		xpmin=max(1.,min(xpmin,xproj))
		xpmax=min(float(nprj),max(xpmax,xproj))
	      enddo
	    enddo
	  enddo
c	  print *,iv,xpmin,xpmax
c		
c	    set up extent and offset of stretches
c	    
	  ofstretch(iv)=xpmin/cbet(iv)-1./interpfac
	  nstretch(iv)=interpfac*(xpmax-xpmin)/cbet(iv)+2.
	  indstretch(iv+1)=indstretch(iv)+nstretch(iv)
c	    print *,iv,xpmin,xpmax,ofstretch(iv),nstretch(iv),indstretch(iv)
	  if(ifalpha.gt.0)nsneed=max(nsneed,int(ithick*sal(iv)+2))
	enddo
	return
	end


	subroutine errorexit(message)
	character*(*) message
	print *,' '
	print *,'ERROR: TILT - ',message
	call exit(1)
	end

c	  TOMOPITCH  analyes simple models of the boundaries of the section
c	  in slices from a tomogram and recommends how much to change
c	  tilt angles to make the section flat, how much to shift the tilt
c	  axis in Z to produce centered slices, and how thick to make the
c	  slices.  It can also recommend how much X-axis tilt is needed to
c	  make the section flat in the orthogonal direction as well.
c	  
c	  For each sample of the tomogram, make a model file with two contours
c	  in it.  Each contour should have two points, with the line between
c	  them lying along the top or bottom surface of the section.  The
c	  points do not have to be entered in any particular order, and the
c	  lines do not need to be any particular length (the program will
c	  extend them in X, if necessary, to within 5% of each edge of the
c	  tomogram).
c	  
c	  As an alternative to making a separate model file for each sample,
c	  you can load all of the samples into imod together and make a single
c	  model file, creating a pair of contours at each different time index.
c	  Make this model file be the sole entry to Tomopitch, and it will
c	  analyze each if the time indexes separately, the same as if they were
c	  in separate files.
c
c	  The program analyzes the model file for each tomogram sample
c	  separately.  It  determines what rotation is required to make the
c	  section be flat.  It reports the upward shift needed to center the
c	  section in Y, and the slice thickness needed to contain the section.
c	  These values are derived and reported  before and after the
c	  recommended rotation is applied.  Thickness values include an
c	  additional amount that you specify and are rounded up to integers
c	  suitable for taking 3D FFTs.  After all files are analyzed, the
c	  program makes the same analysis and report based on the data from
c	  all of the files.  It then computes an X-axis tilt and reports
c	  thickness and rotation if that tilt is taken into account as well.
c	  
c	  Entries to the program:
c	  
c	  Additional pixels of thickness to add to both the top and the bottom
c	  of the tomogram, beyond the region described by the model lines.
c
c	  Spacing between tomogram samples (the distance in Y in the tilt 
c	  images.)  If a non-zero number is entered, the program will
c	  compute the tilt around the X-axis that will make the tomogram be
c	  flat in its Z dimension.
c
c	  Number of model files to analyze
c	  
c	  For each file, then enter the name of the file.
c
c	  David Mastronarde, January 2000
c	  5/20/01: Added analysis of single file with multiple time indexes
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.6  2003/10/03 00:59:35  mast
c	  Changed terminology to refered to tilt angle offset
c	
c	  Revision 3.5  2002/07/21 19:31:11  mast
c	  *** empty log message ***
c	
c	  Revision 3.4  2002/07/21 19:30:50  mast
c	  Standardized error output and made model coordinates get scaled
c	  correctly
c	
c	  Revision 3.3  2002/06/25 15:25:18  mast
c	  Adjusted sample coordinates to be centered around zero to correct
c	  problem with computation of shift with X-axis tilt.
c	
c	  Revision 3.2  2002/05/21 03:12:17  mast
c	  Remove ; at end of two lines, declare lnblnk
c	
c	  Revision 3.1  2002/05/20 15:42:47  mast
c	  Added analysis of single file with multiple time indexes
c	
c
	implicit none
	include 'model.inc'
	integer limtot
	parameter (limtot=500)
C   
	CHARACTER*120 FILIN
C	
	logical readw_or_imod,usetimes
	integer getimodhead,getimodmaxes,getimodtimes,getimodscales
	real*4 xcen(limtot),ycen(limtot),thkmid(limtot),ysamp(limtot)
	integer*4 ifuse(limtot),itimes(max_obj_num)
	integer*4 ibottop(2)
	real*4 slope(2),bintcp(2),xleft(2),xright(2)
	character*120 message
c	  
	integer*4 npatch,nfiles,ifile,ipbase,ierr,ifflip,i,line,iobj
	integer*4 maxx,maxy,maxz,mintime,maxtime,numobj,iobj1,iobj2
	real*4 border,deltay,ysample,xyscal,zscal,xofs,yofs,zofs
	integer*4 ip1,ip2,ibt
	real*4 x1,y1,x2,y2,xlo,xhi,yll,ylr,yul,yur,ymiddle
	real*4 ximscale,yimscale,zimscale
	integer*4 lnblnk
c
	npatch=2
c	
	write(*,'(1x,a,$)')'Additional thickness to add outside model'
     &      //' lines: '
	read(5,*)border
c
	write(*,'(1x,a,$)')'For analysis of X-axis tilt, enter '//
     &	    'distance between sample tomograms: '
	read(5,*)deltay
	if (deltay.gt.0.) write(*,'(/,a,/)')' MODEL FILES MUST BE '//
     &	    'ENTERED IN ORDER OF INCREASING SAMPLE COORDINATE'
	if (deltay.lt.0.) write(*,'(/,a,/)')' MODEL FILES MUST BE '//
     &	    'ENTERED IN ORDER OF DECREASING SAMPLE COORDINATE'
c
	write(*,'(1x,a,$)')'Number of model files: '
	read(5,*)nfiles
	ipbase=1
	ysample = -deltay * (nfiles-1.)/2.

	ifile = 1
	usetimes = .false.

	do while(ifile.le.nfiles)
	  if (.not.usetimes)then
	    write(*,'(1x,a,i2,a,$)')'Name of model file #',ifile,': '
	    read(5,'(a)')FILIN
	    if(.not.readw_or_imod(filin))then
	      print *,' '
	      print *, 'ERROR: TOMOPITCH - READING MODEL FILE ',filin
	      call exit(1)
	    endif
	    ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
	    ierr=getimodmaxes(maxx,maxy,maxz)
	    ierr=getimodscales(ximscale,yimscale,zimscale)
	    do i=1,n_point
	      p_coord(1,i)=(p_coord(1,i)-xofs)/ximscale-maxx/2
	      p_coord(2,i)=(p_coord(2,i)-yofs)/ximscale-maxy/2
	    enddo
c	      
c	      If there is one file, see if there are multiple times
c	      
	    if (nfiles.eq.1) then
	      ierr=getimodtimes(itimes)
	      mintime=100000
	      maxtime=0
	      do iobj=1,max_mod_obj
		if (npt_in_obj(iobj).gt.0)then
		  mintime=min(mintime,itimes(iobj))
		  maxtime=max(maxtime,itimes(iobj))
		endif
	      enddo
	      if (maxtime .gt. 0) then
		usetimes = .true.
		nfiles = maxtime
		if (mintime .eq. 0) then
		  print *,' '
		  print *,'ERROR: TOMOPITCH - THE MODEL FILE HAS ',
     &		  'MULTIPLE TIMES BUT HAS SOME CONTOURS',
     &		  ' WITH NO TIME INDEX.  ',
     &		  'THESE CONTOURS CANNOT BE INTERPRETED AND',
     &		  '  SHOULD BE ELIMINATED'
		  call exit(1)
		endif
	      endif
	    endif
	  endif
	  if(nfiles*npatch.gt.limtot)then
	    print *,' '
	    print *,
     &		'ERROR: TOMOPITCH - TOO MANY TOTAL PATCHES FOR ARRAYS'
	    call exit(1)
	  endif
c	      
c	      now get the first 2 contours in model or time
c
	  if (usetimes) then
	    numobj=0
	    do iobj = 1, max_mod_obj
	      if (npt_in_obj(iobj) .gt. 0 .and.
     &		  itimes(iobj) .eq. ifile .and. numobj .le. 2) then
		numobj = numobj + 1
		if (numobj .eq. 1) iobj1 = iobj
		if (numobj .eq. 2) iobj2 = iobj
	      endif
	    enddo
	    write(message,'(a,i3)')'time index',ifile
	  else
	    numobj = min(2, max_mod_obj)
	    iobj1 = 1
	    iobj2 = 2
	    message = filin
	  endif

	  if (numobj.gt.0.or..not.usetimes) then
	    if (numobj.lt.2) then
	      print *,' '
	      print *,'ERROR: TOMOPITCH - MODEL OR TIME',ifile,
     &		  ' DOES NOT HAVE ENOUGH CONTOURS'
	      call exit(1)
	    endif
	    if(npt_in_obj(iobj1).lt.2.or.npt_in_obj(iobj2).lt.2)then
	      print *,' '
	      print *,'ERROR: TOMOPITCH - THERE ARE NOT TWO POINTS ',
     &		  'IN FIRST TWO',
     &		  ' CONTOURS OF MODEL OR TIME',ifile
	      call exit(1)
	    endif

	    ibottop(1)=iobj1
	    ibottop(2)=iobj2
	    if(p_coord(2,object(ibase_obj(iobj1)+1)).gt.
     &		p_coord(2,object(ibase_obj(iobj2)+1)))then
	      ibottop(1)=iobj2
	      ibottop(2)=iobj1
	    endif

	    do line=1,2
	      ibt=ibottop(line)
	      ip1=object(ibase_obj(ibt)+1)
	      ip2=object(ibase_obj(ibt)+npt_in_obj(ibt))
	      x1=p_coord(1,ip1)
	      y1=p_coord(2,ip1)
	      x2=p_coord(1,ip2)
	      y2=p_coord(2,ip2)
	      slope(line)=(y2-y1)/(x2-x1)
	      bintcp(line)=y2-slope(line)*x2
	      xleft(line)=min(x1,x2)
	      xright(line)=max(x1,x2)
	    enddo	  
	  
	    xlo=min(xleft(1),xleft(2),-0.45*maxx)
	    xhi=max(xright(1),xright(2),0.45*maxx)
	    yll=xlo*slope(1)+bintcp(1)
	    ylr=xhi*slope(1)+bintcp(1)
	    yul=xlo*slope(2)+bintcp(2)
	    yur=xhi*slope(2)+bintcp(2)
	    xcen(ipbase)=xlo
	    ycen(ipbase)=(yll+yul)/2.
	    thkmid(ipbase)=2*border+yul-yll
	    ysamp(ipbase)=ysample
	    xcen(ipbase+1)=xhi
	    ycen(ipbase+1)=(ylr+yur)/2.
	    thkmid(ipbase+1)=2*border+yur-ylr
	    ysamp(ipbase+1)=ysample
	    ifuse(ipbase)=1
	    ifuse(ipbase+1)=1
c
	    call analyzespots(message(1:lnblnk(message)),xcen(ipbase),
     &		ycen(ipbase), thkmid(ipbase),ifuse(ipbase),npatch,
     &		ysamp(ipbase),0.)
	    ipbase=ipbase+npatch
	  endif
	  ysample=ysample+deltay
	  ifile=ifile+1
	enddo
c	  
c	  DNM 6/25/02: need to make the ysamp values symmetric around zero
c	  i.e., assume that the samples are symmetric in data set
c
	ymiddle = deltay * (nfiles - 1) / 2
	do i = 1, ipbase - 1
	  ysamp(i) = ysamp(i) - ymiddle
	enddo
c
	message='all files'
	if(usetimes)message='all time indexes'
	if(nfiles.eq.1)deltay=0.
	call analyzespots(message(1:lnblnk(message)),xcen,ycen,
     &	    thkmid,ifuse,ipbase-1, ysamp, deltay)
	call exit(0)
	end


	subroutine analyzespots(fillab,xcen,ycen,
     &	    thkmid,ifuse,nspots,ysamp,doxtilt)
	implicit none
	character*(*) fillab
	real*4 xcen(*),ycen(*),thkmid(*),ysamp(*)
	integer*4 ifuse(*),nspots
	real*4 xx(1000),yy(1000),zz(1000),doxtilt
c	  
	integer*4 nd,i
	real*4 ang,slop,bint,ro,a,b,c,alpha,theta,costh,sinth
	real*4 cosal,sinal,zp
	real*4 atand,cosd,sind
c
	write (6,101)fillab
101	format(//,' Analysis of positions from ',a,':')
	call findshift('unrotated',ycen,thkmid,ifuse,nspots)
	nd=0
	do i=1,nspots
	  if(ifuse(i).ne.0)then
	    nd=nd+1
	    xx(nd)=xcen(i)
	    yy(nd)=ycen(i)
	  endif
	enddo
	call lsfit(xx,yy,nd,slop,bint,ro)
	ang=atand(slop)
	do i=1,nspots
	  yy(i)=xcen(i)*sind(-ang)+ycen(i)*cosd(-ang)
	enddo
	write(6,102)slop,ang
102	format(' slope =',f8.4,': to make level, add',f6.1,
     &	    ' to total angle offset')
	call findshift(' rotated ',yy,thkmid,ifuse,nspots)
	if(doxtilt.eq.0.)return
	nd=0
	do i=1,nspots
	  if(ifuse(i).ne.0)then
	    nd=nd+1
	    xx(nd)=xcen(i)
	    zz(nd)=ycen(i)
	    yy(nd)=ysamp(i)
	  endif
	enddo
	call lsfit2(xx,yy,zz,nd,a,b,c)
	alpha=atand(b)
	slop=a/(cosd(alpha)-b*sind(alpha))
	theta=-atand(slop)
	costh=cosd(theta)
	sinth=sind(theta)
	cosal=cosd(alpha)
	sinal=sind(alpha)
	do i=1,nspots
	  zp=ycen(i)*cosal-ysamp(i)*sinal
	  yy(i)=xcen(i)*sinth+zp*costh
	enddo
	write(6,103)alpha,-theta
103	format(/' The pitch between samples can be corrected with ',
     &	    'an X-axis tilt of',f7.2,/,' In this case, to make level,'
     &	    ' add',f6.1,' to total angle offset')
	call findshift('x-tilted ',yy,thkmid,ifuse,nspots)
	return
	end

	subroutine findshift(rotlab,ycen,thick,ifuse,nspots)
	implicit none
	character*(*) rotlab
	real*4 ycen(*),thick(*)
	integer*4 ifuse(*),nspots
c	  
	real*4 bot,top,realthk,shift
	integer*4 i,ithick
	integer*4 niceframe
c
	bot=1.e10
	top=-1.e10
	do i=1,nspots
	  if(ifuse(i).ne.0)then
	    bot=min(bot,ycen(i)-thick(i)/2)
	    top=max(top,ycen(i)+thick(i)/2)
	  endif
	enddo
	realthk=top-bot
	shift=-0.5*(top+bot)
	ithick=2*(int(realthk/2.+0.99))
	ithick=niceframe(ithick,2,19)
	write(6,101)rotlab,shift,realthk,ithick
101	format(1x,a,' lines imply added Z shift of',f7.1,'; thickness of',
     &	    f6.1,', set to',i5)
	return
	end

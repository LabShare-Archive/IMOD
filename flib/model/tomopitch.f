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
c
	include 'model.inc'
	parameter (limtot=500)
C   
	CHARACTER*120 FILIN
C	
	logical readw_or_imod
	integer getimodhead,getimodmaxes
	real*4 xcen(limtot),ycen(limtot),thkmid(limtot),ysamp(limtot)
	integer*4 ifuse(limtot)
	integer*4 ibottop(2)
	real*4 slope(2),bintcp(2),xleft(2),xright(2)
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
	if(nfiles*npatch.gt.limtot)stop
     &	    'TOO MANY TOTAL PATCHES FOR ARRAYS'
	ipbase=1
	ysample = -deltay * (nfiles-1.)/2.
	do ifile=1,nfiles
	  write(*,'(1x,a,i2,a,$)')'Name of model file #',ifile,': '
	  read(5,'(a)')FILIN
	  if(.not.readw_or_imod(filin))stop '- ERROR READING MODEL FILE'
	  ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
	  ierr=getimodmaxes(maxx,maxy,maxz)
	  do i=1,n_point
	    p_coord(1,i)=p_coord(1,i)-xofs-maxx/2
	    p_coord(2,i)=p_coord(2,i)-yofs-maxy/2
	  enddo
	  if (max_mod_obj.lt.2) stop 
     &         ' - MODEL DOES NOT HAVE ENOUGH CONTOURS'
	  if(npt_in_obj(1).lt.2.or.npt_in_obj(2).lt.2)stop 
     &         '- THERE ARE NOT TWO POINTS IN FIRST TWO CONTOURS'

	  ibottop(1)=1
	  ibottop(2)=2
	  if(p_coord(2,object(ibase_obj(1)+1)).gt.
	1     p_coord(2,object(ibase_obj(2)+1)))then
	    ibottop(1)=2
	    ibottop(2)=1
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
	  call analyzespots(filin(1:lnblnk(filin)),xcen(ipbase),
     &	      ycen(ipbase), thkmid(ipbase),ifuse(ipbase),npatch,
     &	      ysamp(ipbase),0.)
	  ipbase=ipbase+npatch
	  ysample=ysample+deltay
	enddo
	call analyzespots('all files',xcen,ycen,
     &	    thkmid,ifuse,npatch*nfiles, ysamp, deltay)
	call exit(0)
99	stop 'read error'
	end


	subroutine analyzespots(fillab,xcen,ycen,
     &	    thkmid,ifuse,nspots,ysamp,doxtilt)
	character*(*) fillab
	real*4 xcen(*),ycen(*),thkmid(*),ysamp(*)
	integer*4 ifuse(*)
	real *4 xx(1000),yy(1000),zz(1000)
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
	write(6,102)slop,-ang,ang
102	format(' slope =',f8.4,': to rotate by',f6.1,', add',f6.1,
     &	    ' to all angles')
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
	write(6,103)alpha,theta,-theta
103	format(/' The pitch between samples can be corrected with ',
     &	    'an X-axis tilt of',f7.2,/,' In this case, rotate by',f6.1,
     &	    ', i.e., add',f6.1,' to all angles')
	call findshift('x-tilted ',yy,thkmid,ifuse,nspots)
	return
	end

	subroutine findshift(rotlab,ycen,thick,ifuse,nspots)
	character*(*) rotlab
	real*4 ycen(*),thick(*)
	integer*4 ifuse(*)
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
101	format(1x,a,' lines imply shift up',f7.1,'; thickness of',
     &	    f6.1,', set to',i5)
	return
	end

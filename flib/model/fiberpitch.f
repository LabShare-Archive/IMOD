* * * * * * FIBERPITCH * * * * * *
c	  
c	  FIBERPITCH measures the angle of fibers relative to a central axis
c	  and their distance from the axis.  The pitch is resolved into radial
c	  and tangential components.  Several fiber bundles in a model may be
c	  analyzed separately by drawing contours around them.  The central
c	  axis may be specified by a model contour; otherwise it will be
c	  assumed to be located at the centroid of the fibers and oriented
c	  either vertically or along the average trajectory of the fibers.
c	  Measurements can be made from fibers in a subset of the model
c	  in Z.  The program can analyze several models and output the results
c	  to a single file. 
c
c	  To use the program, the fibers should be modeled as open contours in
c	  an Imod model.  These contours can be in more than one object. The
c	  pitch for a fiber will be measured from the vector connecting the
c	  starting and ending points of the contour.  The position of the fiber
c	  is characterized by a single point for the purposes of measuring the
c	  distance from the central axis, resolving the pitch into radial
c	  and tangential components, and determining whether a fiber is a
c	  member of a bundle.  This point can be either the starting point,
c	  ending point, or middle of the contour; the same kind of point is
c	  used for every contour in an object.  If the middle is chosen, the
c	  program finds the point in the contour that is halfway along the
c	  length of the contour between the starting and ending points.  If a
c	  subset of Z is chosen for analysis, then the starting and ending
c	  points for all of these purposes are the first and last points inside
c	  the specified Z range, respectively.
c	  
c	  If separate bundles are to be analyzed, include an object with
c	  separate closed contours that surround the fibers in each bundle.
c	  These contours should be drawn in an X/Y plane and may be drawn at
c	  any desired Z-level.  The X and Y coordinates of the characteristic
c	  point for each contour are used to determine whether it is inside a
c	  given boundary contour.
c	  
c	  To define the central axes of bundles, include another object with
c	  open contours that describe the trajectories of the axes.  Only two
c	  points are needed for each axis.  The first point should be placed
c	  at about the same Z level as the characteristic points for the
c	  fibers.  The direction in Z will affect the angles and should be
c	  chosen appropriately (see below).  If no central axis object is
c	  used, or if there is no central axis contour inside the contour
c	  specifying a particular bundle, then the program will assume that
c	  the axis for that bundle is  located at the centroid of the
c	  characteristic points for all of the fibers in that bundle.  You can
c	  specify that such an axis will follow the mean trajectories of the
c	  fibers; otherwise it will be made perpendicular to the X/Y plane.
c	  
c	  The program includes in its output an identifying number for the 
c	  model (optional), the contour number of the boundary contour
c	  (optional), the object and contour number of the fiber, the X and Y
c	  coordinates of the characteristic point relative to the central axis
c	  (in a coordinate system achieved after rotating the axis to
c	  vertical), the distance from the axis to the characteristic point,
c	  the angle between the fiber and axis, and the radial and tangential
c	  components of the pitch.  The coordinates and distance will be in
c	  nanometers, or in pixels if there is no scale in the model header.
c	  The overall angle varies from 0 to 180  degrees and will depend on
c	  the direction of the central axis as well as that of the fiber.  It
c	  is greater than 90 degrees if the fiber forms an oblique angle with
c	  the axis, with these directions taken into account.  The radial
c	  pitch varies from -180 to 180 degrees; it is positive if the fiber
c	  is heading away from the axis and negative if the fiber is heading
c	  toward it; its magnitude is greater than 90 the the angle to the
c	  axis is oblique.  The tangential pitch varies from -90 to 90 degrees
c	  and is positive or negative depending on whether the fiber follows a
c	  right-handed or lefted-handed screw thread around the axis.  The
c	  program allows options to invert the polarity of all the fibers in
c	  an object, and to invert the polarity of the axis marker for a
c	  particular object, so that these angles can all be made to come out
c	  right even if fibers or the axis marker are not modeled in the right
c	  polarity for a particular object.  When an axis is derived from the
c	  mean trajectory of fibers that have mixed polarities, the polarity
c	  of the axis is determined by which polarity is predominant among the
c	  fibers.
c	  
c	  The first three entries to the program apply to the output for all
c	  of the following models:
c	  
c	  Name of output file, or Return to output to the terminal or log file.
c	  
c	  1 to output an identifying number for each model, or 0 not to.
c	  
c	  1 to output boundary contour numbers, or 0 not 0.
c	  
c	  The following entries are made for each model to be processed:
c	  
c	  Name of model file, or Return to exit the program.
c	  
c	  IF you entered 1 above to output identifying numbers, then enter
c	  the identifying number for this model; otherwise skip this entry.
c	  
c	  Lower and upper Z levels within which to measure pitches, or 0,0
c	  to have no constraints in Z.
c
c	  List of objects with fibers to analyze.  Ranges may be entered, e.g.,
c	  1-3,5,8.
c	  
c	  For each object to be analyzed, enter -1, 0, or 1 to take the
c	  characteristic point for measuring fiber position to be at the start,
c	  middle, or end of the contour, respectively.
c	  
c	  For each object to be analyzed, enter 1 to invert the polarity of
c	  fibers and compute their direction using the vector from the end to
c	  the start, or 0 to use the normal polarity from start to end.
c	  
c	  For each object to be analyzed, enter 1 to invert the polarity of 
c	  the central axis, or 0 not to.
c	  
c	  Object number of object containing boundary contours around bundles
c	  of fibers, or 0 to treat all fibers as belonging to one bundle.
c	  
c	  Object number of object containing contours marking the central axes,
c	  or 0 to use a vertical axis through the centroid of the fiber
c	  positions in X and Y, or -1 to use an axis that follows the mean
c	  trajectory of the fibers.
c	  
c	  David Mastronarde, 5/3/01
c
	include 'model.inc'
	parameter (limcpts=2000,limimobj=255)
	character*80 modelfile,fileout
	logical exist,readw_or_imod
	real*4 rmat(3,3),vecmean(3),vecnorm(3),censtr(3),cenend(3)
	real*4 anrot(3),delrot(3)
	integer*4 iobjuse(limimobj),iwhichend(limimobj)
	integer*4 invertfib(limimobj),invertcen(limimobj)
	real*4 delxyz(3,max_obj_num),xyzan(3,max_obj_num)
	integer getimodhead
	real*4 bx(limcpts),by(limcpts)
	logical failed,inside
	character*4 boundtxt,identxt
c	  
	print *,'Enter name of output file (or Return for',
     &	    ' output to terminal)'
	read(5,'(a)')fileout
	if(fileout.eq.' ')then
	  iunout=6
	else
	  iunout=7
	  call dopen(7,fileout,'new','f')
	endif
c	  
	write(*,'(1x,a,$)')
     &	    '1 to output identifiers for each model, 0 not to: '
	read(5,*)ifident
	write(*,'(1x,a,$)')
     &	    '1 to output boundary contour numbers, 0 not to: '
	read(5,*)ifboundout
c	  
c	  loop on models to get multiple outputs in one file
c
91	write(*,'(1x,a,$)')'Name of model file (Return if done): '
	read(*,'(a)')modelfile
	if(modelfile.eq.' ')then
	  if(iunout.eq.7)close(7)
	  print *,'Columns are:'
	  icol=1
	  if(ifident.ne.0)then
	    write(*,'(i2,a)')1,'      : Model identifier'
	    icol=2
	  endif
	  if(ifboundout.ne.0)then
	    write(*,'(i2,a)')icol,'      : Boundary contour #'
	    icol=icol+1
	  endif
	  write(*,'(2i2,a)')icol,icol+1,
     &	      '    : Fiber object and contour #'
	  write(*,'(3i2,a)')icol+2,icol+3,icol+4, '  : X, Y, and total'
     &	      //' displacement of fiber from center (nm or pixels)'
	  write(*,'(2i2,i3,a)')icol+5,icol+6,icol+7,
     &	      ' : Total, radial, and tangential pitch (degrees)'
	  call exit(0)
	endif
c
c	  read in the model
c
	exist=readw_or_imod(modelfile)
	if(.not.exist)then
	  print *,'Failure to open model, try again'
	  go to 91
	endif
c
	ifflip=0
	ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	if(ierr.ne.0)then
	  write(*,'(1x,a,$)')'Z scale is not available from '//
     &	      'model header; enter it now: '
	  read(5,*)zscale
	endif
c	    
c	  if no scale or scale comes back as 1.e6, set it to 1 to be pixels;
c	  otherwise convert from microns to nanometers
c	    
	if(ierr.ne.0.or.(xyscal.gt.0.999e6.and.xyscal.lt.1.001e6))then
	  xyscal=1
	else
	  xyscal=xyscal*1000.
	endif
	if(ifflip.eq.0)then
	  indy=2
	  indz=3
	  zoffset=zofs
	else
	  indz=2
	  indy=3
	  zoffset=yofs
	endif
c
	if(ifident.ne.0)then 
	  write(*,'(1x,a,$)')
     &	      'Identifying number to output, or 0 for none: '
	  read(5,*)ident
	endif
c
	write(*,'(1x,a,$)')'Lower and upper Z to analyze between,'
     &	    //' or 0,0 for no limits in Z: '
	read(5,*)zlo,zhi
	zlo=zlo*xyscal*zscale
	zhi=zhi*xyscal*zscale
c
	nobjuse=0
	print *,'Enter list of IMOD object #s with fibers to analyze'
	call rdlist(5,iobjuse,nobjuse)
c
	write(*,'(1x,a,/,a,$)')'For each object, enter -1, 0, or 1 '//
     &	    'to measure radial distances ',' from the start, middle,'
     &	    //' or end of the contours, respectively: '
	read(5,*)(iwhichend(i),i=1,nobjuse)
c	  
	write(*,'(1x,a,/,a,$)')'For each object, enter 1 to invert '//
     &	    'fiber polarity and use vectors from end',
     &	    ' to start, or 0 to use normal polarity (start to end): '
	read(5,*)(invertfib(i),i=1,nobjuse)
	write(*,'(1x,a,/,a,$)')'For each object, enter 1 to invert '//
     &	    'polarity of central axis,',' or 0 not to: '
	read(5,*)(invertcen(i),i=1,nobjuse)
c
	write(*,'(1x,a,/,a,$)')'Enter object number with contours '//
     &	    'outlining separate areas to analyze,',' or 0 to analyze'
     &	    //' all fibers relative to a single center: '
	read(5,*)iobjbound
c	  
	write(*,'(1x,a,/,a,/,a,$)')'Enter object number with center '//
     &	    'markers, or 0 to assume a center vertical',
     &	    ' in Z, at the X/Y centroid of the measurement points,',
     &	    ' or -1 to use the average trajectory of the fibers at '//
     &	    'the X/Y centroid: '
	read(5,*)iobjcen
c	  
c	 scale and deflip model, remove offset in z
c
	do i=1,n_point
	  p_coord(1,i)=p_coord(1,i)*xyscal
	  ynew=p_coord(indy,i)*xyscal
	  p_coord(3,i)=(p_coord(indz,i)-zoffset)*xyscal*zscale
	  p_coord(2,i)=ynew
	enddo
c	  
c	  first compute analysis points and trajectories for all contours
c
	do iobj=1,max_mod_obj
	  call objtocont(iobj,obj_color,imodobj,imodcont)
	  iuse=0
	  do i=1,nobjuse
	    if(imodobj.eq.iobjuse(i))iuse=i
	  enddo
	  ninobj=npt_in_obj(iobj)
	  ibase=ibase_obj(iobj)
	  ipst=1
	  ipnd=ninobj
	  delxyz(1,iobj)=1.e10
c
	  if((zlo.ne.0..or.zhi.ne.0.).and.ninobj.gt.1.and.iuse.ne.0)then
c	    
c	      find starting and ending index of segment within z limits
c	      or at least min and max index that are within limits
c	      
	    ipst=1000000
	    ipnd=-1
	    do ipt=1,ninobj
	      zpt=p_coord(3,object(ipt+ibase))
	      if(zpt.ge.zlo.and.zpt.le.zhi)then
		ipst=min(ipst,ipt)
		ipnd=max(ipnd,ipt)
	      endif
	    enddo
	  endif
c
	  if(iuse.ne.0.and.ipnd-ipst.gt.0)then
	    dir=1.
	    if(invertfib(iuse).ne.0)dir=-1.
	    do i=1,3
	      delxyz(i,iobj)=dir * (p_coord(i,object(ipnd+ibase)) -
     &		  p_coord(i,object(ipst+ibase)))
	    enddo
	    if(iwhichend(iuse).lt.0)then
c		
c		take start
c
	      do i=1,3
		xyzan(i,iobj)=p_coord(i,object(ipst+ibase))
	      enddo
	    elseif(iwhichend(iuse).gt.0)then
c		
c		take end
c
	      do i=1,3
		xyzan(i,iobj)=p_coord(i,object(ipnd+ibase))
	      enddo
	    else
c		
c		find points that bracket midpoint of distance, and interpolate
c
	      distsum=0.
	      do ipt=ipst,ipnd-1
		ipc=object(ipt+ibase)
		ipn=object(ipt+ibase+1)
		dist=sqrt((p_coord(1,ipc)-p_coord(1,ipn))**2+
     &		    (p_coord(2,ipc)-p_coord(2,ipn))**2+
     &		    (p_coord(3,ipc)-p_coord(3,ipn))**2)
		distsum=distsum+dist
	      enddo
	      dhalf=distsum/2.
	      distsum=0.
	      do ipt=ipst,ipnd-1
		ipc=object(ipt+ibase)
		ipn=object(ipt+ibase+1)
		dist=sqrt((p_coord(1,ipc)-p_coord(1,ipn))**2+
     &		    (p_coord(2,ipc)-p_coord(2,ipn))**2+
     &		    (p_coord(3,ipc)-p_coord(3,ipn))**2)
		if(distsum.le.dhalf.and.distsum+dist.ge.dhalf)then
		  do i=1,3
		    xyzan(i,iobj)=p_coord(i,ipc)+(dhalf-distsum)*
     &			(p_coord(i,ipn)-p_coord(i,ipc))/dist
		  enddo
		endif
		distsum=distsum+dist
	      enddo
	    endif
	  endif
	enddo
c
	nbound=max_mod_obj
	if(iobjbound.eq.0)nbound=1
	do ibound=1,nbound
	  call objtocont(ibound,obj_color,iboundobj,iboundcont)
	  if(iobjbound.eq.0.or.iobjbound.eq.iboundobj)then
c	      
c	      get boundary contour
c
	    if(iobjbound.eq.0)then
	      iboundcont=0
	      ninbound=0
	    else
	      ninbound=npt_in_obj(ibound)
	      ibase=ibase_obj(ibound)
	      do i=1,ninbound
		bx(i)=p_coord(1,object(ibase+i))
		by(i)=p_coord(2,object(ibase+i))
	      enddo
	      if(ninbound.lt.3)then
		ninbound=0
		print *,'Less than 3 points for boundary contour',
     &		    iboundcont
	      endif
	    endif
c	      
c	      go through contours looking for marker and adding up analysis
c	      points as well
c
	    npts=0
	    xsum=0.
	    ysum=0.
	    zsum=0.
	    ifcen=0
	    posnet=0.
	    do i=1,3
	      vecmean(i)=0.
	    enddo
c
	    do iobj=1,max_mod_obj
	      call objtocont(iobj,obj_color,imodobj,imodcont)
	      ninobj=npt_in_obj(iobj)
	      ibase=ibase_obj(iobj)
	      if(imodobj.eq.iobjcen.and.ninobj.gt.1)then
c		  
c		  If this is a center contour, check if inside boundary
c
		xcen=p_coord(1,object(ibase+1))
		ycen=p_coord(2,object(ibase+1))
		if(ninbound.eq.0.or.inside(bx,by,ninbound,xcen,ycen))then
		  if(ifcen.ne.0)print *,'More than one center marker ',
     &		      'for boundary contour',iboundcont
		  ifcen=1
		  do i=1,3
		    censtr(i)=p_coord(i,object(ibase+1))
		    cenend(i)=p_coord(i,object(ibase+ninobj))
		  enddo
		endif
	      elseif(delxyz(1,iobj).ne.1.e10.and.(ninbound.eq.0.or.
     &		    inside(bx,by,ninbound,xyzan(1,iobj),xyzan(2,iobj))))then
c		  
c		  Otherwise, sum analysis points and vectors inside the
c		  boundary, inverting vectors as needed and summing their
c		  polarities in posnet
c
		npts=npts+1
		xsum=xsum+xyzan(1,iobj)
		ysum=ysum+xyzan(2,iobj)
		zsum=zsum+xyzan(3,iobj)
		dirvec=sign(1.,delxyz(3,iobj))
		posnet=posnet+dirvec
		do i=1,3
		  vecmean(i)=vecmean(i)+dirvec*delxyz(i,iobj)
		enddo
	      endif
	    enddo
	    if(npts.eq.0)then
	      print *,'No fibers inside boundary contour',iboundcont
	    else
c		
c		if no marker, setup mean of points as vertical marker
c
	      if(ifcen.eq.0)then
		censtr(1)=xsum/npts
		censtr(2)=ysum/npts
		censtr(3)=zsum/npts
		if(iobjcen.ge.0)then
		  cenend(1)=censtr(1)
		  cenend(2)=censtr(2)
		  cenend(3)=censtr(3)+1.
		else
c		    
c		    make it follow the average vector
c
		  dirvec=1.
		  if(posnet.lt.0.)dirvec=-1.
		  cenend(1)=censtr(1)+dirvec*vecmean(1)/npts
		  cenend(2)=censtr(2)+dirvec*vecmean(2)/npts
		  cenend(3)=censtr(3)+dirvec*vecmean(3)/npts
		endif
	      endif
c		
c		get vector for center marker
c
	      dir=sign(1.,cenend(3)-censtr(3))
	      do i=1,3
		vecmean(i)=dir*(cenend(i)-censtr(i))
		vecnorm(i)=0.
	      enddo
	      vecnorm(3)=1.
c		
c		find rotation matrix to get from center marker to vertical
c		and get the rotated center x/y coordinates
c
	      call vectors_to_rmat(vecmean,vecnorm,rmat)
	      xcen=censtr(1)*rmat(1,1)+censtr(2)*rmat(1,2)+
     &		  censtr(3)*rmat(1,3)
	      ycen=censtr(1)*rmat(2,1)+censtr(2)*rmat(2,2)+
     &		  censtr(3)*rmat(2,3)
c		
c		find fibers inside boundary
c		
	      do iobj=1,max_mod_obj
		call objtocont(iobj,obj_color,imodobj,imodcont)
		ninobj=npt_in_obj(iobj)
		ibase=ibase_obj(iobj)
		if(delxyz(1,iobj).ne.1.e10.and.(ninbound.eq.0.or.
     &		    inside(bx,by,ninbound,xyzan(1,iobj),xyzan(2,iobj))))then
c		    
c		    get total pitch from acos of dot product, using dirtot
c		    to get the true polarity of the center marker
c
		  iuse=0
		  do i=1,nobjuse
		    if(imodobj.eq.iobjuse(i))iuse=i
		  enddo
		  dirtot=dir
		  if(invertcen(iuse).ne.0)dirtot=-dir
		  total=acosd(dirtot*(vecmean(1)*delxyz(1,iobj)+
     &		      vecmean(2)*delxyz(2,iobj)+vecmean(3)*delxyz(3,iobj))/
     &		      (absval(delxyz(1,iobj))*absval(vecmean)))
c		    
c		    rotate the measurement point and the vector
c
		  do i=1,3
		    anrot(i)=xyzan(1,iobj)*rmat(i,1)+xyzan(2,iobj)*
     &			rmat(i,2)+xyzan(3,iobj)*rmat(i,3)
		    delrot(i)=delxyz(1,iobj)*rmat(i,1)+delxyz(2,iobj)*
     &			rmat(i,2)+delxyz(3,iobj)*rmat(i,3)
		  enddo
c
c		    find the angle for rotating the measurement point to the
c		    X axis, then rotate the vector and measure pitches from
c		    radial (x) and tangential (y) components of rotated vector
c
		  delx=anrot(1)-xcen
		  dely=anrot(2)-ycen
		  gamma=-atan2(dely,delx)
		  xrot=delrot(1)*cos(gamma)-delrot(2)*sin(gamma)
		  yrot=delrot(1)*sin(gamma)+delrot(2)*cos(gamma)
c		    
c		    the tangential pitch is polarity invariant and the
c		    ratio of y to z accomplishes that
c		    the radial pitch can have a full range -180 to 180 and
c		    the polarity of marker as well as fiber has to be taken
c		    into account
c
		  radial=atan2d(xrot,dirtot*delrot(3))
		  tangent=atand(yrot/delrot(3))
		  dist=sqrt(delx**2+dely**2)
		  identxt='    '
		  if(ifident.ne.0)write(identxt,'(i4)')ident
		  boundtxt='    '
		  if(ifboundout.ne.0)write(boundtxt,'(i4)')iboundcont
		  write(iunout,101)identxt,boundtxt,imodobj,imodcont,
     &		      delx,dely,dist,total,radial,tangent
101		  format(1x,2a4,i4,i5,3f9.1,3f7.1)
		endif
	      enddo
	    endif
	  endif
	enddo
	go to 91
	end






	subroutine vectors_to_rmat(avec,bvec,rm)
	real*4 rm(3,3),avec(*),bvec(*),cvec(3)
	equivalence (x,cvec(1)),(y,cvec(2)),(z,cvec(3))
	call crossproduct(avec,bvec,cvec)
	do i=1,3
	  do j=1,3
	    rm(i,j)=0.
	    if(i.eq.j)rm(i,j)=1.
	  enddo
	enddo
	if(absval(cvec).eq.0.)return
	sina=absval(cvec)/(absval(avec)*absval(bvec))
	angle=asind(sina)
	cosa=cosd(angle)
	sina=-sina
	omca=1.0-cosa
	call normalize(cvec)
        rm(1,1) = x * x * omca + cosa
        rm(1,2) = y * x * omca + (sina * z)
        rm(1,3) = z * x * omca - (sina * y)

        rm(2,1) = x * y * omca - (sina * z)
        rm(2,2) = y * y * omca + cosa
        rm(2,3) = z * y * omca + (sina * x)

        rm(3,1) = x * z * omca + (sina * y)
        rm(3,2) = y * z * omca - (sina * x)
        rm(3,3) = z * z * omca + cosa
	return
	end



	function absval(vec)
	real*4 vec(3)
	absval = sqrt(vec(1)**2+vec(2)**2+vec(3)**2)
	return
	end


	subroutine crossproduct(avec,bvec,cvec)
	real*4 avec(*),bvec(*),cvec(*)
	cvec(1)=avec(2)*bvec(3)-avec(3)*bvec(2)
	cvec(2)=avec(3)*bvec(1)-avec(1)*bvec(3)
	cvec(3)=avec(1)*bvec(2)-avec(2)*bvec(1)
	return
	end



	subroutine normalize(vect)
	real*4 vect(3)
	sum=0.
	do i=1,3
	  sum=sum+vect(i)**2
	enddo
	fac=sqrt(sum)
	do i=1,3
	  vect(i)=vect(i)/fac
	enddo
	return
	end


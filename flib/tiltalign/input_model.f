c***	  INPUT_MODEL reads in the model data, reads the header info from an
c	  image file, sorts out the model points that are to be included in
c	  the analysis, and converts the coordinates to "index" coordinates
c	  with the origin at the center of the section
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine input_model(xx,yy,isecview,maxprojpt,maxreal,
     &	    irealstr,ninreal, imodobj, imodcont,nview, nprojpt,nrealpt,
     &	    iwhichout, xcen, ycen,listz,mapfiletoview,nfileviews,
     &	    modelfile,iupoint, iuangle, iuxtilt)
c	  
	implicit none
	real*4 xx(*),yy(*),xcen,ycen
	integer*4 isecview(*),irealstr(*),ninreal(*),imodobj(*),imodcont(*)
	integer*4 mapfiletoview(*),maxprojpt,maxreal,nview,nprojpt,nrealpt
	integer*4 iwhichout, iupoint,iuangle,iuxtilt,nfileviews
	character*(*) modelfile
c
	character*80 solufile,anglefile,pointfile
	logical stereopair,exist,readw_or_imod
c
	include 'model.inc'
c
	integer*4 nxyz(3),mxyz(3),listz(*)
	real*4 delta(3)
c	  
	integer*4 nzlist,iobject,ninobj,ipt,iz,i,j,itmp,minzval,ifspecify
	integer*4 ninoutlist,ivstr,ivend,ivinc,ifonlist,ibase,nprojtmp
	integer*4 inlist,mode
	real*4 xorig,yorig,zorig,xdelt,ydelt,dmin,dmax,dmean
c
c	  read model in
c
10	write(*,'(1x,a,$)')'Model file: '
	read(5,'(a)')modelfile
	exist=readw_or_imod(modelfile)
	if(.not.exist)then
	  print *,'bad model file, try again'
	  go to 10
	endif
c
c	  scan to get list of z values
c
	nzlist=0
	do iobject=1,max_mod_obj
	  ninobj=npt_in_obj(iobject)
	  if(ninobj.gt.1)then
	    do ipt=1,ninobj
	      iz=nint(p_coord(3,object(ipt+ibase_obj(iobject))))
	      do i=1,nzlist
		if(iz.eq.listz(i))go to 20
	      enddo
	      nzlist=nzlist+1
	      listz(nzlist)=iz
20	    enddo
	  endif
	enddo
c
c	  order list of z values
c
	do i=1,nzlist-1
	  do j=i+1,nzlist
	    if(listz(i).gt.listz(j))then
	      itmp=listz(i)
	      listz(i)=listz(j)
	      listz(j)=itmp
	    endif
	  enddo
	enddo
c
c	  set minimum z value if any are negative, and set number of views
c	  in file from range of z values
c
	minzval=min(0,listz(1))
	nfileviews=listz(nzlist)-minzval+1
c
c	  get dimensions of file and header info on origin and delta
c
	print *,'Image file with matching header info'//
     &	    ' (Return to enter nx,ny directly)'
	read(5,'(a)')modelfile
c
	if(modelfile.eq.' ')then
	  delta(1)=1.
	  delta(2)=1.
	  xorig=0.
	  yorig=0.
	  write(*,'(1x,a,$)')
     &	      'nx, ny, x origin, y origin, x delta, y delta: '
	  read(5,*)nxyz(1),nxyz(2),xorig,yorig,delta(1),delta(2)
	else
	  call ialprt(.false.)			!don't want to see header!
	  call imopen(1,modelfile,'ro')
	  call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
	  call irtorg(1,xorig,yorig,zorig)
	  call irtdel(1,delta)
	  call imclose(1)
c
c	    minimum z value is determined by origin, but make it as low as
c	    actual minimum
c	    # of file views should be the size of file, but make it big
c	    enough to hold all z values implied by origin
c
	  minzval=min(listz(1),-nint(zorig))
	  nfileviews=max(listz(nzlist)-minzval+1,nxyz(3))
	endif
	xdelt=delta(1)
	ydelt=delta(2)
	xcen=nxyz(1)/2.
	ycen=nxyz(2)/2.
c	  
c	  get name of file for output model
c	  
	write(*,'(1x,a)')'Enter file name for output model of'//
     &	    ' solved X-Y-Z coordinates (Return for none)'
	read(5,'(a)')modelfile
c	  
c	  get name of files for angle list output and 3-D point output
c	  
	iupoint=0
	write(*,'(1x,a)')'Enter file name for ASCII list of'//
     &	    ' solved X-Y-Z coordinates (Return for none)'
	read(5,'(a)')pointfile
	if(pointfile.ne.' ')then
	  call dopen(8,pointfile,'new','f')
	  iupoint=8
	endif
c
	iuangle=0
	write(*,'(1x,a)')'Enter name of output file for list of'//
     &	    ' solved tilt angles (Return for none)'
	read(5,'(a)')anglefile
	if(anglefile.ne.' ')then
	  call dopen(9,anglefile,'new','f')
	  iuangle=9
	endif
c	  
c	  if iuxtilt comes in non-zero, ask about file for x axis tilts
c
	if(iuxtilt.ne.0)then
	  iuxtilt=0
	  write(*,'(1x,a)')'Enter name of output file for list of'//
     &	      ' X-axis tilt angles (Return for none)'
	  read(5,'(a)')anglefile
	  if(anglefile.ne.' ')then
	    call dopen(10,anglefile,'new','f')
	    iuxtilt=10
	  endif
	endif
c
c	  open output file to put transforms into
c
	write(*,'(1x,a,$)')'Output file for solutions/transforms: '
	read(5,'(a)')solufile
	call dopen(7,solufile,'new','f')
c
	write(*,'(1x,a,$)')'1 to put only xforms in file,'
     &	    //' -1 to put only solutions, 0 both: '
	read(5,*)iwhichout
c
c	  find out which z values to include; end up with a list of z values
c
	write(*,'(1x,a,i3,a,/,a,/,a,/,a,$)')'Enter 0 to include all',nzlist,
     &	    ' views with points',
     &	    '    or 1 to enter start, end, and increment view numbers,',
     &	    '    or 2 to specify a list of views to include',
     &	    '    or 3 to specify a list of views to exclude: '
	read(5,*)ifspecify
c
	if (ifspecify.gt.0)then
	  if(ifspecify.eq.1)then
	    write(*,'(1x,a,$)')
     &		'Start, end, and increment of views to include: '
	    read(5,*)ivstr,ivend,ivinc
	    ninoutlist=1+(ivend-ivstr)/ivinc
	    do i=1,ninoutlist
	      isecview(i)=ivstr+(i-1)*ivinc
	    enddo
	  elseif(ifspecify.eq.2)then
	    write(*,'(1x,a,$)')'Enter views to include (ranges ok): '
	    call rdlist(5,isecview,ninoutlist)
	  else
	    write(*,'(1x,a,$)')'Enter views to exclude (ranges ok): '
	    call rdlist(5,isecview,ninoutlist)
	  endif
c	    
c	    go through list of z values in model and make sure they are on
c	    an include list or not on an exclude list
c	    
	  i=1
	  do while(i.le.nzlist)
	    ifonlist=0
	    do j=1,ninoutlist
	      if(listz(i)+1.eq.isecview(j))ifonlist=1
	    enddo
c	      
c	      remove points on exclude list or not on include list
c
	    if(ifspecify.le.2.xor.ifonlist.eq.1)then
	      nzlist=nzlist-1
	      do j=i,nzlist
		listz(j)=listz(j+1)
	      enddo
	    else
	      i=i+1
	    endif
	  enddo
	endif
c
c	  go through model finding objects with more than one point in the
c	  proper z range, and convert to index coordinates, origin at center
c
	nprojpt=0
	nrealpt=0
	do iobject=1,max_mod_obj		!loop on objects
	  ninobj=npt_in_obj(iobject)
	  ibase=ibase_obj(iobject)
	  if(ninobj.gt.1)then			!non-empty object
	    nprojtmp=nprojpt			!save number at start of object
	    stereopair=.false.
	    do ipt=1,ninobj			!loop on points
c
c		find out if the z coordinate of this point is on the list
c
	      iz=nint(p_coord(3,object(ipt+ibase)))
	      inlist=0
	      do i=1,nzlist
		if(iz.eq.listz(i))inlist=i
	      enddo
	      if(inlist.gt.0)then
c
c		  if so, tentatively add index coordinates to list
c
		nprojtmp=nprojtmp+1
		if(nprojtmp.gt.maxprojpt)stop
     &		  'TOO MANY PROJECTION POINTS FOR ARRAYS'
		xx(nprojtmp)=(p_coord(1,object(ipt+ibase))+xorig)/xdelt
     &		    -xcen
		yy(nprojtmp)=(p_coord(2,object(ipt+ibase))+yorig)/ydelt
     &		    -ycen
		isecview(nprojtmp)=inlist
c
c		  if doing stereo pairs, set flag if it's not set, and if
c		  flag is set, point must belong to second view
c
		if(nzlist.eq.1)then
		  if(stereopair)isecview(nprojtmp)=inlist+1
		  stereopair=.true.
		endif
	      endif
	    enddo
c
c	      if there are at least 2 points, take it as a real point
c
	    if(nprojtmp-nprojpt.ge.2)then
	      nrealpt=nrealpt+1
	      if(nrealpt.gt.maxreal)stop
     &		  'TOO MANY FIDUCIAL POINTS FOR ARRAYS'
	      irealstr(nrealpt)=nprojpt+1
	      ninreal(nrealpt)=nprojtmp-nprojpt
	      call objtocont(iobject,obj_color,imodobj(nrealpt),
     &		  imodcont(nrealpt))
	      nprojpt=nprojtmp
	      npt_in_obj(iobject)=1
	    else
	      npt_in_obj(iobject)=0
	    endif
	  endif
	enddo
	irealstr(nrealpt+1)=nprojpt+1		!for convenient looping
	nview=max(2,nzlist)			!nzlist maybe 1 for stereo pair
c	  
c	  shift listz to be a view list, and make the map of file to view
c
	do i=1,nfileviews
	  mapfiletoview(i)=0
	enddo
	do i=1,nzlist
	  listz(i)=listz(i)-minzval+1
	  mapfiletoview(listz(i))=i
	enddo
	return
	end



c	  WRITE_XYZ_MODEL will write out a model containing only a single point
c	  per object with the solved XYZ coordinates, to file MODELFILE, if
c	  that string is non-blank.  IGROUP is an array with the group number
c	  if the points have been assigned to surfaces.
c
	subroutine write_xyz_model(modelfile,xyz,igroup,nrealpt)
c
	implicit none
	real*4 xyz(3,*)
	integer*4 igroup(*),nrealpt
	character*(*) modelfile
	include 'model.inc'
c	  
	integer*4 ireal,iobject,ipt,i
c
	if(modelfile.eq.' ')return
c	  
c	  loop on model objects that are non-zero, stuff point coords into 
c	  first point of object (now only point), and set color by group
c
	ireal=0
	do iobject=1,max_mod_obj
	  if(npt_in_obj(iobject).gt.0)then
	    ipt=object(ibase_obj(iobject)+1)
	    ireal=ireal+1
	    do i=1,3
	      p_coord(i,ipt)=xyz(i,ireal)
	    enddo
	    obj_color(2,iobject)=obj_color(2,iobject)+igroup(ireal)-1
	  endif
	enddo
c	  
	n_object=nrealpt
	call write_wmod(modelfile)
	close(20)
	return
	end

c	  READ_MODEL reads a WIMP model.
c	  if MODELFILE is blank, it requests a model file name; otherwise it
c	  attempts to open the file specified by that name.
c	  If there is an error, it loops back on the request for a file name.
c	  If a new file name is needed, it then requests scaling parameters
c	  for this file, and returns them in IFSCALE and XYSCAL.
c	  The file name is returned in MODELFILE.
c	  If IFSCALE is non-zero (as passed to routine or entered by user),
c	  then all points are scaled by XYSCAL.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.5  2003/10/26 05:33:27  mast
c	  change command files to use unit 4 instead reopening 5
c	
c	  Revision 3.4  2003/08/12 05:10:53  mast
c	  Return if no model file entered
c	
c	  Revision 3.3  2002/09/09 21:36:00  mast
c	  Eliminate stat_source: and nimp_source: from all includes
c	
c	  Revision 3.2  2002/07/07 04:39:52  mast
c	  Add the argument for maximum number of vertices to the convexbound
c	  call.
c	
c	  Revision 3.1  2002/06/05 21:16:50  mast
c	  Made GET_POINTS reject a contour only if was in a closed object
c	  and was completely planar; made it work with scattered points by
c	  not rejecting multiple points on a Z plane for scattered points.
c	  Also added check on overrunning boundary vertex arrays.
c	
c
	subroutine read_model(modelfile,ifscale,xyscal)
	include 'model.inc'
	character*(*) modelfile
	logical exist,readw_or_imod,newfile
	integer*4 getimodhead
	integer*4 in5
	common /nmsinput/ in5
c	  
	newfile=.false.
	if(modelfile.ne.' ')go to 92
91	print *,'Enter name of input model file, or Return to skip',
     &	    ' to entering options'
	read(in5,'(a)')modelfile
	if (modelfile .eq.' ') return
	newfile=.true.
c
92	exist=readw_or_imod(modelfile)
	if(.not.exist)go to 91
	if(newfile)then
	  ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
	  if(ierr.eq.0.and.xyscal.ne.1.0)then
	    ifscale=1
	    write(*,'(a,f10.6,a)')' Scale set from model header at',
     &		xyscal,' microns/pixel'
	  else
c	  
	    write(*,'(1x,a,$)')'1 to scale to microns, -1 to enter '//
     &		'scaling factor directly, 0 no rescaling: '
	    read(in5,*)ifscale
	    if(ifscale.ne.0)then
	      if(ifscale.gt.0)then 
c
		write(*,'(1x,a,$)')'Magnification of negatives: '
		read(in5,*)xmag
		write(*,'(1x,a,$)')'Scale at which negatives were '//
     &		    'digitized (microns per pixel from VIDS): '
		read(in5,*)umperpix
		xyscal=umperpix/xmag
c	      
	      else
		write(*,'(1x,a,$)')
     &		    'Scaling factor to multiply X and Y coordinates by: '
		read(in5,*)xyscal
	      endif
	    endif
	  endif
	endif
c	  
c	  Unscale points from pixel size in image file
c
	call scale_model(0)
	if(ifscale.ne.0)then
	  do i=1,n_point
	    p_coord(1,i)=p_coord(1,i)*xyscal
	    p_coord(2,i)=p_coord(2,i)*xyscal
	  enddo
	endif
	return
	end


	
c	  GET_POINTS gets the points in the current WIMP model that are
c	  contained within the boundary whose NVERT coordinates are in
c	  BX and BY.
c	  ZZ specifies the Z value of the boundary.
c	  ITYPCROSIND is an index of the types found in the model thus far
c	  which may have some values in it on entry; values are added as
c	  needed here.
c	  NTYPES is the number of types found (incremented here as needed)
c	  NPNTS is returned with the number of points returned
c	  SX, SY and ITYPE are returned with point coordinates and colors
c	  NINCLASS is a list of the number of points of each type
c
	subroutine get_points(bx,by,nvert,zz,itypcrosind,ntypes,sx,sy,
     &	    itype,npnts,ninclass)
	include 'model.inc'
        logical inside
	parameter (limtyp=50,itypall=999)
	real*4 bx(*),by(*)
	integer*4 itypcrosind(-256:256),itype(*),ninclass(*)
	real*4 sx(*),sy(*)
	character*120 modelfile
	integer*4 iobjflag(limtyp)
	integer*4 getimodflags
	integer*4 in5
	common /nmsinput/ in5
c
	do ii=1,limtyp
	  ninclass(ii)=0
	enddo
	ierr = getimodflags(iobjflag,limtyp)
c	  
c	  extract points inside polygon
c	  
	npnts=0
	do iobj=1,max_mod_obj
	  imodobj = 256-obj_color(2,iobj)
	  ifplanar=0
	  notclosed=1
	  if(imodobj.le.limtyp)notclosed=mod(iobjflag(imodobj),4)
	  if(npt_in_obj(iobj).gt.1.and.notclosed.eq.0)then
c	      
c	      for closed contour object only, look through the whole contour
c	      to make sure it is not planar
c
	    ifplanar=1
	    do i=1,npt_in_obj(iobj)-1
	      if(abs(p_coord(3,abs(object(ibase_obj(iobj)+i)))-
     &		p_coord(3,abs(object(ibase_obj(iobj)+i+1)))).gt.0.01)
     &		  ifplanar=0
	    enddo
	  endif
	  if(ifplanar.eq.0)then
	    zdiffmin=10.
	    ifscat=0
	    if(imodobj.le.limtyp)ifscat=mod(iobjflag(imodobj)/2,2)
	    do i=1,npt_in_obj(iobj)
	      ipnt=abs(object(ibase_obj(iobj)+i))
	      zdiff=abs(p_coord(3,ipnt)-zz)
	      if(zdiff.lt.0.5)then
		if(inside(bx,by,nvert,p_coord(1,ipnt),p_coord(2,ipnt)))
     &		    then
		  if(zdiffmin.lt.10..and.zdiff.lt.zdiffmin.and.
     &		      ifscat.eq.0) then
c		      
c		      Except for scattered point objects,
c		      If there is already a point from this contour and
c		      the new point is closer to the Z plane, replace it so
c		      that we end up with only one point
c
		    zdiffmin=zdiff
		    sx(npnts)=p_coord(1,ipnt)
		    sy(npnts)=p_coord(2,ipnt)
		  elseif(zdiffmin.eq.10.)then
c		      
c		      store first point that meets criteria
c
		    if(ifscat.eq.0)zdiffmin=zdiff
		    npnts=npnts+1
		    sx(npnts)=p_coord(1,ipnt)
		    sy(npnts)=p_coord(2,ipnt)
		    itype(npnts)=256-obj_color(2,iobj)
		    if(obj_color(1,iobj).eq.0)itype(npnts)=-itype(npnts)
c		    
c		      look if this type is on list of types; if not, add to
c		      list of types, then increment count of this type
c		      
		    if(itypcrosind(itype(npnts)).eq.0)then
		      ntypes=ntypes+1
		      itypcrosind(itype(npnts))=ntypes
		    endif
		    indlist=itypcrosind(itype(npnts))
		    ninclass(indlist)=ninclass(indlist)+1
		  endif
		endif
	      endif
	    enddo
	  endif
	enddo
	return
	end

c
c	  
c	  GET_BOUNDARY_OBJ gets the coordinates of a boundary object whose
c	  number is specified by IOBJBOUND.  NVERT is returned as 0 if there
c	  is an error.  NVERT vertices are returned in BX, BY; ZZ is the Z
c	  value (which must be the same throughout the object).
c
	subroutine get_boundary_obj(iobjbound,bx,by,nvert,zz,itype,
     &	    ntypbound,padbound,ifconvex,fracomit,sx,sy,maxverts)
	include 'model.inc'
	real*4 bx(*),by(*)
	integer*4 itype(*)
	real*4 sx(*),sy(*)
c
	if(iobjbound.gt.0)then
	  nvert=0
	  if(npt_in_obj(iobjbound).lt.3)then
	    print *,'Not enough points in that contour'
	    return
	  endif
c	    
c	    extract object
c	    
	  zz=p_coord(3,abs(object(ibase_obj(iobjbound)+1)))
	  do i=1,npt_in_obj(iobjbound)
	    ipnt=abs(object(ibase_obj(iobjbound)+i))
	    if(p_coord(3,ipnt).ne.zz)then
	      print *,'Contour not all in one Z plane'
	      return
	    endif
	    if(nvert.eq.0 .or. p_coord(1,ipnt).ne.bx(max(1,nvert))
     &		.or. p_coord(2,ipnt).ne.by(max(1,nvert)))then
	      if (nvert.ge.maxverts) then
		print *,
     &		    'The contour has too many vertices for the arrays'
		return
	      endif
	      nvert=nvert+1
	      bx(nvert)=p_coord(1,ipnt)
	      by(nvert)=p_coord(2,ipnt)
	    endif
	  enddo
	else
	  xmin=1.e10
	  xmax=-xmin
	  ymin=xmin
	  ymax=-ymin
	  nconsid=0
	  do iobj=1,max_mod_obj
	    if(npt_in_obj(iobj).gt.0)then
	      ifconsid=0
	      ityobj=256-obj_color(2,iobj)
	      if(obj_color(1,iobj).eq.0)ityobj=-ityobj
	      do i=1,ntypbound
		if(ityobj.eq.itype(i).or.itype(i).eq.itypall)ifconsid=1
	      enddo
	      if(ifconsid.ne.0)then
		ztmp=p_coord(3,abs(object(ibase_obj(iobj)+1)))
		i=1
		ifinplane=1
		do while(ifinplane.eq.1.and.i.le.npt_in_obj(iobj))
		  ipnt=abs(object(ibase_obj(iobj)+i))
		  if(p_coord(3,ipnt).ne.ztmp)ifinplane=0
		  i=i+1
		enddo
	      endif
	      if(ifconsid.ne.0.and.ifinplane.eq.0)then
		do i=1,npt_in_obj(iobj)
		  ipnt=abs(object(ibase_obj(iobj)+i))
		  if(abs(p_coord(3,ipnt)-zz).lt.0.5)then
		    nconsid=nconsid+1
		    sx(nconsid)=p_coord(1,ipnt)
		    sy(nconsid)=p_coord(2,ipnt)
		    xmin=min(xmin,p_coord(1,ipnt))
		    xmax=max(xmax,p_coord(1,ipnt))
		    ymin=min(ymin,p_coord(2,ipnt))
		    ymax=max(ymax,p_coord(2,ipnt))
		  endif
		enddo
	      endif
	    endif
	  enddo
	  if(nconsid.lt.3.or.ifconvex.eq.0)then
	    if(xmax.lt.xmin)then
	      xmin=0.
	      xmax=1.
	      ymin=0.
	      ymax=1.
	    endif
	    nvert=4
	    if(padbound.eq.0.)then
	      padx=.01*(xmax-xmin)
	      pady=.01*(ymax-ymin)
	    else
	      padx=padbound
	      pady=padbound
	    endif
	    bx(1)=xmin-padx
	    bx(2)=xmax+padx
	    bx(3)=bx(2)
	    bx(4)=bx(1)
	    by(1)=ymin-pady
	    by(2)=by(1)
	    by(3)=ymax+pady
	    by(4)=by(3)
	  else
	    call convexbound(sx,sy,nconsid,fracomit,padbound,bx,by,nvert,
     &		xcen,ycen,maxverts)
	  endif
	endif
	bx(nvert+1)=bx(1)
	by(nvert+1)=by(1)
	return
	end
c	  
c	  
c	  SAVE_MODEL saves the boundary contour specified by the NVERT
c	  coordinates in BX, BY, and saves each of the NPNTS points in SX,SY
c	  as a separate object with color ITYPE.  All points are given the Z
c	  value zz.  If IFSCAL is non-zero, coordinates are all scaled by
c	  1/XYSCAL so that they will correspond to the original model.
c
	subroutine save_model(bx,by,nvert,sx,sy,itype,npnts,zz,ifscal,xyscal)
c
	include 'model.inc'
	integer*4 in5
	common /nmsinput/ in5
	character*120 modelfile
c
91	write(*,'(1x,a,$)')'Name of model file to store points in: '
	read(in5,'(a)')modelfile
C
C 7/20/00 CER remove recordtype='setmented' for gnu
C
	open(20,file=modelfile,status='NEW',form='unformatted'
     &	      ,err=91)
c	  
c	  zero out the model first
c	  
	do i=1,max_obj_num
	  npt_in_obj(i)=0
	enddo
c	  
c	  save each sample point in separate object
c
	sclfac=1.
	if(ifscal.ne.0)sclfac=1./xyscal
	do iobj=1,npnts
	  npt_in_obj(iobj)=1
	  ibase_obj(iobj)=iobj-1
	  object(iobj)=iobj
	  p_coord(3,iobj)=zz
	  p_coord(1,iobj)=sclfac*sx(iobj)
	  p_coord(2,iobj)=sclfac*sy(iobj)
	  obj_color(2,iobj)=256-abs(itype(iobj))
	  if(itype(iobj).ge.0)then
	    obj_color(1,iobj)=1
	  else
	    obj_color(1,iobj)=0
	  endif
	enddo
c	  
c	  save boundary object
c	  
	max_mod_obj = npnts+1
	npt_in_obj(max_mod_obj)=nvert
	ibase_obj(max_mod_obj)=npnts
	obj_color(1,max_mod_obj)=1
	obj_color(2,max_mod_obj)=247		!so can turn off points easily
	do i=1,nvert
	  n_point=npnts+i
	  object(n_point)=n_point
	  p_coord(1,n_point)=sclfac*bx(i)
	  p_coord(2,n_point)=sclfac*by(i)
	  p_coord(3,n_point)=zz
	enddo
c
	call scale_model(1)
	call write_wmod(modelfile)
	close(20)
	return
	end



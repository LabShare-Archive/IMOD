****	  CCDERASER.FOR
c	  
c	  This program replaces deviant pixels with interpolated values from
c         surrounding pixels.  It is designed to correct defects in electron
c         microscope images from CCD cameras.  The program can replace a group
c         of adjacent pixels with interpolated values, or all of the pixels 
c         along a line.  It can do this on only a specific image, or on all of
c	  the sections in the file.
c       
c	  To use the program, first prepare an IMOD model file to specify the
c	  points to be replaced.  There should be up to four separate objects:
c	  one for patches of points to be replaced on each section; one for 
c	  patches to be replaced on only a single section, one for lines to be
c	  replaced on a single section, and one for lines to be replaced on a
c	  single section.
c  
c	  To specify a patch of points, start a new contour and place a point 
c	  inside of EVERY pixel in the patch.  Each separate patch should be 
c	  in a separate contour.  If the correction is to be made on only a
c	  single section, all of the points must lie on that section; but the
c	  points for a patch to be corrected on all sections can be on more 
c	  than one section, because sometimes they are more discernable on
c	  particular sections.
c	  
c	  To specify a line of points, make a contour with two points, at the
c	  start and end of the line to be replaced.  Lines must be horizontal
c	  or vertical.  Each pixel will be replaced by the average of the two
c	  pixels on either side of the line.  Put each line in a different
c	  contour.
c
c         The inputs are:
c
c	  Input image file
c
c	  Output image file, or <Return> to place modified sections back into
c	  the input file.  USE REPLACEMENT OPTION WITH CAUTION
c
c	  Model file
c
c	  A list of objects which specify points or lines to be replaced on
c         all sections, or / if all objects do so, or Return if none do.
c         Ranges may be entered.
c       
c         A list of objects which specify lines to be replaced, or / if
c         all objects do so, or Return if none do.  Ranges may be entered.
c
c         Size of the border around the points in a patch, which contains the
c         points which will be fit to (/ for default of 3 pixels)
c         
c         Order of polynomial (/ for default of 2, which includes terms in
c         x, y, x**2, y**2 and x*y)
c       
c         0 to exclude or 1 to include points adjacent to the specified points
c         in the polynomial fit.
c	  
c	  David Mastronarde 11/10/98
c
	parameter (imsiz=2100)
	parameter (mxd=50)
	real*4 array(imsiz*imsiz),title(20),orig(3),delt(3)
	integer*4 nxyz(3),mxyz(3)
	equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
	character*80 infile,outfile,ptfile
	integer*4 ixfix(5000),iyfix(5000),izfix(5000),iobjdoall(256)
	integer*4 iobjline(256)
	include 'model.inc'
	logical readw_or_imod, typeonlist
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
c
	write(*,'(1x,a,$)')'Input image file: '
	read(*,'(a)')infile
	call imopen(1,infile,'old')
	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
	if(nx*ny.gt.imsiz**2)stop 'IMAGE TOO LARGE'
	call irtdel(1,delt)
	call irtorg(1,orig(1),orig(2),orig(3))
c	  
	print *,'Enter output file name (Return to put modified'//
     &	    ' sections back in input file)'
	read(*,'(a)')outfile
c	  
	imfilout=1
	if(outfile.ne.' ')then
	  imfilout=2
	  call imopen(2,outfile,'new')
	  call itrhdr(2,1)
	endif
c	  
75	write(*,'(1x,a,$)')'Model file: '
	read(*,'(a)')ptfile
	if(.not.readw_or_imod(ptfile))then
	  print *,'Bad model file, try again'
	  go to 75
	endif

	do i=1,n_point
	  do j=1,3
	    p_coord(j,i)=(p_coord(j,i)+orig(j))/delt(j)
	  enddo
	enddo
c	  
	print *,'Enter list of objects specifying replacement on ALL'//
     &	    ' sections (/ for all, Return for none)'
c	  
	nobjdoall=1
	iobjdoall(1)=-999
	call rdlist(5,iobjdoall,nobjdoall)
c
	print *,'Enter list of objects specifying horizontal or vertical'//
     &	    ' line replacements',' (/ for all, Return for none)'
c	  
	nobjline=1
	iobjline(1)=-999
	call rdlist(5,iobjline,nobjline)
c
	nborder=3
	write(*,'(1x,a,i2,a,$)')'border size [/ for',nborder,']: '
	read(*,*)nborder
c
	iorder=2
	write(*,'(1x,a,i2,a,$)')'order of 2-D polynomial fit [/ for'
     &	    ,iorder,']: '
	read(*,*)iorder
c	  
	write(*,'(1x,a,$)')
     &	    '0 to exclude or 1 to include adjacent points in fit: '
	read(5,*)ifincadj
	tmin=1.e10
	tmax=-1.e10
	tsum=0.
c       
	do iobj=1,max_mod_obj
	  if(npt_in_obj(iobj).gt.0)then
	    ibase=ibase_obj(iobj)
	    itype = 256-obj_color(2,iobj)
	    call objtocont(iobj,obj_color,imodobj,imodcont)
	    if(typeonlist(itype,iobjline,nobjline)) then
	      if(npt_in_obj(iobj).ne.2) then
		print *,'object',imodobj,', contour',imodcont,
     &              ' does not have only two points'
		stop 'ERROR: LINE DOES NOT HAVE ONLY TWO POINTS'
	      endif
	      ip1 = object(ibase+1)
	      ip2 = object(ibase+2)
	      if (nint(p_coord(3,ip1)).ne.nint(p_coord(3,ip2))) then
		print *,'object',imodobj,', contour',imodcont,
     &              ' is not in one Z-plane'
		stop 'ERROR: LINE IS NOT IN ONE Z-PLANE'
	      elseif(nint(p_coord(1,ip1)).ne.nint(p_coord(1,ip2)).and.
     &		    nint(p_coord(2,ip1)).ne.nint(p_coord(2,ip2))) then
		print *,'object',imodobj,', contour',imodcont,
     &              ' is not horizontal or vertical'
		stop 'ERROR: LINE IS NOT HORIZONTAL OR VERTICAL'
	      endif
	    else
	      zmin=1.e10
	      zmax=-1.e10
	      xmin=zmin
	      xmax=zmax
	      ymin=zmin
	      ymax=zmax
	      ninobj=npt_in_obj(iobj)
	      do ip=1,ninobj
		ipt=object(ibase+ip)
		xmin=min(xmin,p_coord(1,ipt))
		xmax=max(xmax,p_coord(1,ipt))
		ymin=min(ymin,p_coord(2,ipt))
		ymax=max(ymax,p_coord(2,ipt))
		zmin=min(zmin,p_coord(3,ipt))
		zmax=max(zmax,p_coord(3,ipt))
	      enddo
	      if(xmax-xmin.ge.mxd/2 .or.ymax-ymin.ge.mxd/2)then
		print *,'object',imodobj,', contour',imodcont,
     &              ' has points too far apart'
		stop 'ERROR: PATCH IS TOO LARGE'
	      endif
	      if(.not.typeonlist(itype,iobjdoall,nobjdoall).and.
     &            zmax.ne.zmin)then
		print *,'object',imodobj,', contour',imodcont,
     &              ' is not in one Z-plane'
		stop 'ERROR: PATCH IS NOT IN ONE Z-PLANE'
	      endif
	    endif
	  endif
	enddo
c	  
c	  start looping on sections; need to read regardless
c	  
	do izsect=0,nz-1
	  call irdsec(1,array,*99)
	  nfix=0
	  linefix =0
c	    
c	    scan through points to see if any need fixing
c	    
	  do iobj=1,max_obj_num
	    if(npt_in_obj(iobj).gt.0)then
	      ibase=ibase_obj(iobj)
	      itype = 256-obj_color(2,iobj)
c             
c             first see if this has a line to do
c
	      if(typeonlist(itype,iobjline,nobjline))then
		if(nint(p_coord(3,object(ibase+1))).eq.izsect .or.
     &              typeonlist(itype,iobjdoall,nobjdoall)) then
		  if(linefix.eq.0)print *,'fixing lines on section #',
     &                izsect
		  linefix=linefix+1
		  ip1=object(ibase+1)
		  ip2=object(ibase+2)
		  ix1=p_coord(1,ip1)+1.01
		  ix2=p_coord(1,ip2)+1.01
		  iy1=p_coord(2,ip1)+1.01
		  iy2=p_coord(2,ip2)+1.01
		  call cleanline(array,nx,ny,ix1,iy1,ix2,iy2)
		endif
	      elseif(nint(p_coord(3,object(ibase+1))).eq.izsect .or.
     &		  typeonlist(itype,iobjdoall,nobjdoall))then
		ninobj=npt_in_obj(iobj)
		do ip=1,ninobj
		  ipt=object(ibase+ip)
		  ixfix(ip)=p_coord(1,ipt)+1.01
		  iyfix(ip)=p_coord(2,ipt)+1.01
		enddo
		if(nfix.eq.0)print *,'fixing points on section #',izsect
		nfix=nfix+1
		call cleanarea(array,nx,ny,nx,ny,ixfix
     &		  ,iyfix,ninobj,nborder,iorder,ifincadj)
	      endif
	    endif
	  enddo
c	    
	  call iclden(array,nx,ny,1,nx,1,ny,dmint,dmaxt,dmeant)
	  tmin=min(tmin,dmint)
	  tmax=max(tmax,dmaxt)
	  tsum=tsum+dmeant
c	    
c	    write out if any changes or if new output file
c
	  if(nfix.gt.0.or.linefix.gt.0.or. imfilout.eq.2)then
	    call imposn(imfilout,izsect,0)
	    call iwrsec(imfilout,array)
	  endif
	enddo
c
	tmean=tsum/nz

c
c 7/7/00 CER: remove the encodes
c
c       encode(80,109,title)
        write(titlech,109) 
        read(titlech,'(20a4)')(title(kti),kti=1,20)
109	format('ERASER3: bad points replaced with interpolated values')
	call iwrhdr(imfilout,title,1,tmin,tmax,tmean)
	call imclose(imfilout)
	call exit(0)
99	stop 'error'
	end

	subroutine cleanline(array,ixdim,iydim,ix1,iy1,ix2,iy2)
	real*4 array(ixdim,iydim)
	if (ix1.eq.ix2)then
	  miny=iy1
	  maxy=iy2
	  if(maxy.lt.miny)then
	    miny=iy2
	    maxy=iy1
	  endif
	  do iy=miny,maxy
	    array(ix1,iy)=(array(ix1-1,iy)+array(ix1+1,iy))/2.
	  enddo
	else
	  minx=ix1
	  maxx=ix2
	  if(maxx.lt.minx)then
	    minx=ix2
	    maxx=ix1
	  endif
	  do ix=minx,maxx
	    array(ix,iy1)=(array(ix,iy1-1)+array(ix,iy1+1))/2.
	  enddo
	endif
	return
	end




	subroutine cleanarea(array,ixdim,iydim,nx,ny,ixfix,iyfix,
     &	    ninobj,nborder,iorder,ifincadj)
c	  
	parameter (mxd=50)
	real*4 array(ixdim,iydim)
	integer*4 ixfix(*), iyfix(*)
	logical*1 inlist(-mxd:mxd,-mxd:mxd),adjacent(-mxd:mxd,-mxd:mxd)
	logical nearedge
	parameter (isdim=1000)
	include 'stat_source:statsize.inc'
	real*4 xr(msiz,isdim), sx(msiz), xm(msiz), sd(msiz)
     &	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &	    , b(msiz), b1(msiz),vect(msiz)
c	  
c	  initialize list
c	  
	ixcen=ixfix(1)
	iycen=iyfix(1)
	do j=-mxd,mxd
	  do i=-mxd,mxd
	    inlist(i,j)=.false.
	    adjacent(i,j)=.false.
	  enddo
	enddo
	minxlist=ixcen
	minylist=iycen
	maxxlist=ixcen
	maxylist=iycen
	do k=1,ninobj
	  ixl=ixfix(k)-ixcen
	  iyl=iyfix(k)-iycen
	  inlist(ixl,iyl)=.true.
	  do i=-1,1
	    do j=-1,1
	      adjacent(ixl+i,iyl+j)=.true.
	    enddo
	  enddo
	  minxlist=min(minxlist,ixfix(k))
	  minylist=min(minylist,iyfix(k))
	  maxxlist=max(maxxlist,ixfix(k))
	  maxylist=max(maxylist,iyfix(k))
	enddo
	nbordm1=nborder-1
	ixbordlo=max(1,minxlist-nborder)
	ixbordhi=min(nx,maxxlist+nborder)
	iybordlo=max(1,minylist-nborder)
	iybordhi=min(ny,maxylist+nborder)
c	  total pixels in current area
	npixel=(iybordhi+1-iybordlo)*(ixbordhi+1-ixbordlo)
c	  
c	  list of points is complete: fill regression matrix with data for all
c	  points outside the list.  the ixbordlo etc. should be correct
c	  
	npnts=0
	nindep=iorder*(iorder+3)/2
	do iy=iybordlo,iybordhi
	  do ix=ixbordlo,ixbordhi
	    ixofs=ix-ixcen
	    iyofs=iy-iycen
	    nearedge= ix-ixbordlo .lt. nbordm1 .or.
     &		ixbordhi-ix .lt. nbordm1 .or.
     &		iy-iybordlo .lt. nbordm1 .or.
     &		iybordhi-iy .lt. nbordm1
	    if(.not.inlist(ixofs,iyofs) .and. (ifincadj.eq.1 .or.
     &		(nearedge.or. .not.adjacent(ixofs,iyofs))))then
	      npnts=npnts+1
	      call polyterm(ixofs,iyofs,iorder,xr(1,npnts))
	      xr(nindep+1,npnts)=array(ix,iy)
	    endif
	  enddo
	enddo
c	  
c	  do regession
c	  
	print *,ninobj,' points to fix, ',npnts,' points being fit'
	call multr(xr,nindep+1,npnts,sx,ss,ssd,d,r,xm,sd,b,b1,c1,rsq
     &	    ,fra)
c	  
c	  replace points on list with values calculated from fit
c
	do iy=iybordhi-nbordm1,iybordlo+nbordm1,-1
	  do ix=ixbordlo+nbordm1,ixbordhi-nbordm1
	    ixofs=ix-ixcen
	    iyofs=iy-iycen
	    if(inlist(ixofs,iyofs))then
	      call polyterm(ixofs,iyofs,iorder,vect)
	      xsum=c1
	      do i=1,nindep
		xsum=xsum+b1(i)*vect(i)
	      enddo
	      array(ix,iy)=xsum
	    endif
	  enddo
	enddo
c	  
	return
	end



c	  POLYTERM computes polynomial terms from ix and iy of order norder,
c	  puts in array vect.  The first set of terms is ix and iy.  Each next
c	  set is the previous set multipled by x, plus the last term of the
c	  previous set multiplied by y
c
	subroutine polyterm(ix,iy,norder,vect)
	real*4 vect(*)
	x=ix
	y=iy
	vect(1)=x
	vect(2)=y
	istr=1
	iend=2
	do iorder=2,norder
	  do i=istr,iend
	    vect(i+iorder)=vect(i)*x
	  enddo
	  istr=istr+iorder
	  vect(iend+iorder+1)=vect(iend)*y
	  iend=iend+iorder+1
	enddo
	return
	end

	logical function typeonlist(itype,ityplist,ntyplist)
	integer*4 ityplist(*)
	typeonlist=.true.
	if(ntyplist.eq.1.and.ityplist(1).eq.-999)return
	do i=1,ntyplist
	  if(itype.eq.ityplist(i))return
	enddo
	typeonlist=.false.
	return
	end
	

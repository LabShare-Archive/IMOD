* * * * * WARPVOL * * * * *
c	  
c	  WARPVOL will transform a volume using a series of general linear
c	  transformations. Its main use is to "warp" one tomogram from
c	  a two-axis tilt series so that it matches the other tomogram.
c	  For each position in the volume, it interpolates between adjacent
c	  adjacent transformations to find the transformation appropriate for
c	  that position.  Any initial alignment transformation (from
c	  SOLVEMATCH) must be already contained in the transformations entered
c	  into this program; this combining of transformations is accomplished
c	  by FINDWARP.  It can work with either a 2-D matrix of
c	  transformations (varying in X and Z) or with a general 3-D matrix,
c	  as output by FINDWARP.  The program uses the same algorithm as
c	  ROTATEVOL for rotating large volumes.
c
c	  See man page for more details
c
c	  David Mastronarde, 11/15/96; modified for 3-D matrix of
c	  transformations, 7/23/97
c	  DNM 2/26/01: add temporary directory entry and semi-unique filenames
c	  DNM 11/6/01: fixed problem with output array size not being respected
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.5  2004/07/24 17:35:38  mast
c	  Added progress output
c	
c	  Revision 3.4  2003/10/10 20:37:06  mast
c	  Changed to use subroutines in rotmatwarpsubs.f and include file.
c	  Converted to PIP/autodoc input and added linear interpolation option.
c	
c	  Revision 3.3  2003/10/02 19:59:05  mast
c	  Changed method of writing sections to avoid having to fit output
c	  section into array all at once.
c	  Increased array size and put big array in common
c	
c	  Revision 3.2  2003/03/14 22:54:09  mast
c	  Standardized error outout and implemented implicit none
c	

	implicit none
	include 'rotmatwarp.inc'
	integer maxloc
	parameter (maxloc=10000)
	real*4 cell(6),dxyzin(3)
	integer*4 mxyzin(3)
	real*4 mfor(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
	real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3)
	real*4 atmp1(3,3),atmp2(3,3),dtmp1(3),dtmp2(3),delta(3)
	real*4 aloc(3,3,maxloc),dloc(3,maxloc)
	real*4 ofsin(3,limout),amx(3,3,limout)
	integer*4 izsec(4)
	integer*4 inmin(3),inmax(3)
	real*4 freinp(10)
c
	character*120 filein,fileout,fileinv,tempdir,tempext
c
c	DNM 3/8/01: initialize the time in case time(tim) doesn't work
c
	character dat*9,tim*8/'00:00:00'/
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	integer*4 nlocx,ninp,nlocy,nlocz,nloctot,i,j,kti,ja,ix,iy,ierr
	real*4 xlocst,ylocst,xlocmax,ylocmax,zlocst,zlocmax,cenlocx,cenlocy
	real*4 cenlocz,dxloc,dyloc,dzloc,dmin,dmax,tsum,devmx,xcen,ycen,zcen
	integer*4 idimout,ind,izcube,ixcube,iycube,ifx,ify,ifz,ival,ifempty
	integer*4 iz,ixlim,iylim,izlim,ixp,iyp,ixpp1,ixpm1,iypp1,iypm1,numDone
	real*4 xofsout,xp,yp,zp,bval,dx,dy,dz,v2,v4,v6,v5,v8,vu,vd,vmin,vmax
	real*4 a,b,c,d,e,f,tmin,tmax,tmean,dmean,dminin,dmaxin,d11,d12,d21,d22
	integer*4 iunit,longint,l,izp,izpp1,izpm1,nLinesOut,interpOrder, nExtra
	real*4 baseInt
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger, PipGetFloatArray, PipNumberOfEntries
	integer*4 PipGetString,PipGetThreeIntegers,PipGetThreeFloats
	integer*4 PipGetNonOptionArg, PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  warpvol
c
	integer numOptions
	parameter (numOptions = 7)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'xforms:TransformFile:FN:@tempdir:TemporaryDirectory:CH:@'//
     &      'size:OutputSizeXYZ:IT:@order:InterpolationOrder:I:@'//
     &      'help:usage:B:'
c	  
c	  set defaults here
c	  
	interpOrder = 2
	baseInt = 0.5
	tempdir = ' '
c
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'warpvol',
     &	    'ERROR: WARPVOL - ', .true., 3, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

	if (PipGetInOutFile('InputFile', 1, 'Name of input file', filein)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
	call imopen(5,filein,'RO')
	call irdhdr(5,nxyzin,mxyzin,mode,dminin,dmaxin,dmeanin)
	nxout=nzin
	nyout=nyin
	nzout=nxin
c
	if (PipGetInOutFile('OutputFile', 2, 'Name of output file', fileout)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
c
	call imopen(6,fileout,'NEW')
c
	if (pipinput) then
	  ierr = PipGetString('TemporaryDirectory', tempdir)
	  ierr = PipGetInteger('InterpolationOrder', interpOrder)
	  ierr = PipGetThreeIntegers('OutputSizeXYZ', nxout, nyout, nzout)
	  if (PipGetString('TransformFile', fileinv) .ne. 0) call errorexit(
     &	      'NO FILE WITH INVERSE TRANSFORMS SPECIFIED')
	else
	  write(*,'(1x,a,/,a,$)')'Enter path name of directory for '//
     &	      'temporary files, ',' or Return to use current directory: '
	  read(5,'(a)')tempdir
c	    
	  write(*,'(1x,a,3i5,a,$)')'X, Y, and Z dimensions of the '//
     &	      'output file (/ for',nxout,nyout,nzout,'): '
	  read(5,*)nxout,nyout,nzout
c	  
c	    get matrix for inverse transforms
c	  
	  print *,'Enter name of file with inverse transformations'
	  read(5,'(a)')fileinv
	endif
	call PipDone()
	if (interpOrder .lt. 2) baseInt = 0.
c
	call dopen(1,fileinv,'ro','f')
	read(1,'(a)')fileinv
	call frefor(fileinv,freinp,ninp)
	nlocx=nint(freinp(1))
	if(ninp.eq.2)then
	  nlocy=1
	  nlocz=nint(freinp(2))
	else
	  nlocy=nint(freinp(2))
	  nlocz=nint(freinp(3))
	endif
	xlocst=1.e10
	xlocmax=-1.e10
	ylocst=1.e10
	ylocmax=-1.e10
	zlocst=1.e10
	zlocmax=-1.e10
	nloctot=nlocx*nlocz*nlocy
	if(nloctot.gt.maxloc)call errorexit(
     &	    'TOO MANY TRANSFORMATIONS TO FIT IN ARRAYS')

	do l=1,nloctot
	  read(1,*)cenlocx,cenlocy,cenlocz
	  read(1,*)((aloc(i,j,l),j=1,3),dloc(i,l),i=1,3)
	  xlocst=min(cenlocx,xlocst)
	  ylocst=min(cenlocy,ylocst)
	  zlocst=min(cenlocz,zlocst)
	  xlocmax=max(cenlocx,xlocmax)
	  ylocmax=max(cenlocy,ylocmax)
	  zlocmax=max(cenlocz,zlocmax)
	enddo
	dxloc=(xlocmax-xlocst)/max(1,nlocx-1)
	dyloc=(ylocmax-ylocst)/max(1,nlocy-1)
	dzloc=(zlocmax-zlocst)/max(1,nlocz-1)
	close(1)
c
c	  DNM 7/26/02: transfer pixel spacing to same axes
c
	call irtdel(5,delta)
	do i=1,3
	  cell(i)=nxyzout(i)*delta(i)
	  cell(i+3)=90.
	  cxyzout(i)=nxyzout(i)/2.
	enddo
c	  
	call icrhdr(6,nxyzout,nxyzout,mode,title,0)
	call ialcel(6,cell)
	call itrlab(6,5)
	call time(tim)
	call date(dat)
	tempext='wrp      1'
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,302,TITLE)dat,tim
        write(titlech,302) dat,tim
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
302	FORMAT('WARPVOL: 3-D warping of tomogram:',t57,a9,2x,a8)
	dmin=1.e20
	dmax=-dmin
	tsum=0.
	numDone = 0
	call irttlt(5,tiltold)
	call ialtlt(6,tiltold)
c	  
c	  find maximum extent in input volume occupied by a back-transformed
c	  unit cube in output volume
c	  
	devmx=0.
	do ja=1,nloctot
	  do ix=-1,1,2
	    do iy=-1,1,2
	      do i=1,3
		devmx=max(devmx,abs(aloc(i,1,ja)*ix+aloc(i,2,ja)*iy+
     &		    aloc(i,3,ja)))
	      enddo
	    enddo
	  enddo
	enddo
c	  
c	  Get provisional setup of cubes then find actual limits of input
c	  cubes with this setup
c
	nExtra = 0
	call setup_cubes_scratch(devmx, nExtra, ' ', tempdir, tempext, tim)
c
	do izcube=1,ncubes(3)
	  do ixcube=1,ncubes(1)
	    do iycube=1,ncubes(2)
c		
c		back-transform the corner coordinates of the output cube to
c		find the limiting index coordinates of the input cube
c		
	      do i=1,3
		inmin(i)=100000
		inmax(i)=-100000
	      enddo
	      do ifx=0,16
		do ify=0,16
		  do ifz=0,16
		    xcen=ixyzcube(1,ixcube)+ifx*nxyzcube(1,ixcube)/16.-cxout
		    ycen=ixyzcube(2,iycube)+ify*nxyzcube(2,iycube)/16.-cyout
		    zcen=ixyzcube(3,izcube)+ifz*nxyzcube(3,izcube)/16.-czout
		    call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &			zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &			zcen,minv,cxyzin) 
		    do i=1,3
		      ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &			  minv(i,3)*zcen+cxyzin(i)+nxyzin(i)/2)
		      inmin(i)=max(0,min(inmin(i),ival-2))
		      inmax(i)=min(nxyzin(i)-1,max(inmax(i),ival+2))
c			
c			See if any extra pixels are needed in input
c
		      nExtra = max(nExtra, inmax(i) + 1 - inmin(i) - inpdim)
		    enddo
		  enddo
		enddo
	      enddo
	    enddo
	  enddo
	enddo

	print *,nextra,' extra pixels needed in cubes for second setup'
c	  
c	  Get setup again for real this time
c
	call setup_cubes_scratch(devmx, nExtra, filein, tempdir, tempext, tim)
c	  
c	  loop on layers of cubes in Z, do all I/O to complete layer
c	  
	do izcube=1,ncubes(3)
c	    
c	    initialize files and counters
c	    
	  do i=1,4
	    call imposn(i,0,0)
	    izsec(i)=0
	  enddo
c	    
c	    loop on the cubes in the layer
c
	  do ixcube=1,ncubes(1)
	    do iycube=1,ncubes(2)
c		
c		back-transform the corner coordinates of the output cube to
c		find the limiting index coordinates of the input cube
c		
	      do i=1,3
		inmin(i)=100000
		inmax(i)=-100000
	      enddo
	      do ifx=0,16
		do ify=0,16
		  do ifz=0,16
		    xcen=ixyzcube(1,ixcube)+ifx*nxyzcube(1,ixcube)/16-cxout
		    ycen=ixyzcube(2,iycube)+ify*nxyzcube(2,iycube)/16-cyout
		    zcen=ixyzcube(3,izcube)+ifz*nxyzcube(3,izcube)/16-czout
		    call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &			zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &			zcen,minv,cxyzin) 
		    do i=1,3
		      ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &			  minv(i,3)*zcen+cxyzin(i)+nxyzin(i)/2)
		      inmin(i)=max(0,min(inmin(i),ival-2))
		      inmax(i)=min(nxyzin(i)-1,max(inmax(i),ival+2))
		    enddo
		  enddo
		enddo
	      enddo
	      ifempty=0
	      do i=1,3
		if(inmin(i).gt.inmax(i))ifempty=1
	      enddo
c		
c		load the input cube
c		
	      if(ifempty.eq.0)then
		do iz=inmin(3),inmax(3)
		  call imposn(5,iz,0)
c		    write(*,'(10i4)'),ixcube,iycube,izcube,iz,iz+1-inmin(3)
c		    &                ,inmin(1),
c		    &               inmax(1),inmin(2),inmax(2)
		  call irdpas(5,array(1,1,iz+1-inmin(3)),inpdim,inpdim,
     &		      inmin(1),inmax(1),inmin(2),inmax(2),*99)
		enddo
	      endif
c		
c		prepare offsets and limits
c
	      xofsout=ixyzcube(1,ixcube)-1-cxout
c	      xofsin=cxin+1-inmin(1)
c	      yofsin=cyin+1-inmin(2)
c	      zofsin=czin+1-inmin(3)
	      ixlim=inmax(1)+1-inmin(1)
	      iylim=inmax(2)+1-inmin(2)
	      izlim=inmax(3)+1-inmin(3)
c		
c		loop over the output cube, doing and saving one section at a
c		time
c		
c
	      do iz=1,nxyzcube(3,izcube)
		zcen=ixyzcube(3,izcube)+iz-1-czout
c		  
c		  if only one position in Y, get matrices for each X position
c
		if(nlocy.eq.1)then
		  do ix=1,nxyzcube(1,ixcube)
		    xcen=ix+xofsout
		    call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &			zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &			zcen, amx(1,1,ix),ofsin(1,ix))
		    do i=1,3
		      ofsin(i,ix)=ofsin(i,ix)+1+nxyzin(i)/2-inmin(i)
		    enddo
		  enddo
		endif
c
		do iy=1,nxyzcube(2,iycube)
		  ycen=ixyzcube(2,iycube)+iy-1-cyout
		  if(ifempty.eq.0)then 
		    do ix=1,nxyzcube(1,ixcube)
		      xcen=ix+xofsout
		      if(nlocy.gt.1)then
			call interpinv(aloc,dloc,xlocst,dxloc,ylocst,
     &			    dyloc, zlocst,dzloc, nlocx, nlocy, nlocz,
     &			    xcen,ycen, zcen, amx(1,1,ix),ofsin(1,ix))
			do i=1,3
			  ofsin(i,ix)=ofsin(i,ix)+1+nxyzin(i)/2-inmin(i)
			enddo
		      endif
c			
c			get indices in array of input data
c			
		      xp=amx(1,1,ix)*xcen+amx(1,2,ix)*ycen+
     &			  amx(1,3,ix)*zcen+ ofsin(1,ix)
		      yp=amx(2,1,ix)*xcen+amx(2,2,ix)*ycen+
     &			  amx(2,3,ix)*zcen+ ofsin(2,ix)
		      zp=amx(3,1,ix)*xcen+amx(3,2,ix)*ycen+
     &			  amx(3,3,ix)*zcen+ ofsin(3,ix)
		      bval = DMEANIN
c			
c			do generalized evaluation of whether pixel is doable
c			
		      ixp=int(xp + baseInt)
		      iyp=int(yp + baseInt)
		      izp=int(zp + baseInt)
		      IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &			  IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &			  IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
			dx=xp-ixp
			dy=yp-iyp
			dz=zp-izp
			ixpp1=min(ixlim,ixp+1)
			iypp1=min(iylim,iyp+1)
			izpp1=min(izlim,izp+1)
c
			if (interpOrder .ge. 2) then
			  ixpm1=max(1,ixp-1)
			  iypm1=max(1,iyp-1)
			  izpm1=max(1,izp-1)
C			    
C			    Set up terms for quadratic interpolation with
c			    higher-order terms omitted
C			    
			  V2 = ARRAY(IXP, IYPM1, IZP)
			  V4 = ARRAY(IXPM1, IYP, IZP)
			  V5 = ARRAY(IXP, IYP, IZP)
			  V6 = ARRAY(IXPP1, IYP, IZP)
			  V8 = ARRAY(IXP, IYPP1, IZP)
			  VU = ARRAY(IXP, IYP, IZPP1)
			  VD = ARRAY(IXP, IYP, IZPM1)
			  vmax=max(v2,v4,v5,v6,v8,vu,vd)
			  vmin=min(v2,v4,v5,v6,v8,vu,vd)
C			    
			  C = (V6 - V4)*.5
			  A = C + V4 - V5
			  D = (V8 - V2)*.5
			  B = D + V2 - V5
			  F = (VU - VD)*.5
			  E = F + VD - V5
			  bval = (a*dx+c)*dx + (b*dy+d)*dy
     &			      + (e*dz+f)*dz + v5
			  if(bval.gt.vmax)bval=vmax
			  if(bval.lt.vmin)bval=vmin
			else
C			  
C			    Set up terms for linear interpolation
C			    
			  d11 = (1. - dx) * (1. - dy)
			  d12 = (1. - dx) * dy
			  d21 = dx * (1. - dy)
			  d22 = dx * dy
			  bval = (1. - dz) * (d11 * array(ixp, iyp, izp)
     &			      + d12 * array(ixp, iypp1, izp)
     &			      + d21 * array(ixpp1, iyp, izp)
     &			      + d22 * array(ixpp1, iypp1, izp)) +
     &			      dz * (d11 * array(ixp, iyp, izpp1)
     &			      + d12 * array(ixp, iypp1, izpp1)
     &			      + d21 * array(ixpp1, iyp, izpp1)
     &			      + d22 * array(ixpp1, iypp1, izpp1))
			endif
		      endif
		      brray(ix,iy)=bval
		    enddo
		  else
		    do ix=1,nxyzcube(1,ixcube)
		      brray(ix,iy)=dmeanin
		    enddo
		  endif
		enddo
		iunit=ifile(ixcube,iycube)
		call irepak(brray,brray,limout,limout,
     &		    0,nxyzcube(1,ixcube)-1,0,nxyzcube(2,iycube)-1)
		call iwrsec(iunit,brray)
		izinfile(ixcube,iycube,iz)=izsec(iunit)
		izsec(iunit)=izsec(iunit)+1
	      enddo
c	      print *,ixcube,iycube,izcube
	      numDone = numDone + 1
	      write(*,'(a,i4,a,i4)')'Finished',numDone,' of',
     &		  ncubes(1)*ncubes(2)*ncubes(3)
	      call flush(6)
	    enddo
	  enddo
c	    
c	    whole layer of cubes in z is done.  now reread and compose one
c	    row of the output section at a time in array
c	    
	  call recompose_cubes(izcube, dmin, dmax, tsum)
 	enddo
c
	dmean=tsum/nzout
	call iwrhdr(6,title,1,dmin,dmax,dmean)
	do i=1,6
	  call imclose(i)
	enddo
	call exit(0)
99	call errorexit('READING FILE')
	end


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: WARPVOL - ',message
	call exit(1)
	end


	subroutine interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,zlocst,
     &	    dzloc,nlocx, nlocy, nlocz, xcen,ycen,zcen,minv,dxyz)
	implicit none
	integer*4 nlocx, nlocy, nlocz
	real*4 aloc(3,3,nlocx,nlocy,nlocz),dloc(3,nlocx,nlocy,nlocz)
	real*4 minv(3,3),dxyz(3),xlocst,dxloc,ylocst,dyloc,zlocst,dzloc
	real*4  xcen,ycen,zcen
	real*4 x,fx,fx1,y,fy,fy1,z,fz,fz1
	integer*4 ix,ix1,iy,iy1,iz,iz1,i,j
c	  
	x=1.
	if(nlocx.gt.0)then
	  x=1.+(xcen-xlocst)/dxloc
	  x=min(float(nlocx),max(1.,x))
	endif
	ix=x
	ix1=min(ix+1,nlocx)
	fx1=x-ix
	fx=1.-fx1
c	  
	y=1.
	if(nlocy.gt.0)then
	  y=1.+(ycen-ylocst)/dyloc
	  y=min(float(nlocy),max(1.,y))
	endif
	iy=y
	iy1=min(iy+1,nlocy)
	fy1=y-iy
	fy=1.-fy1
c
	z=1.
	if(nlocz.gt.0)then
	  z=1.+(zcen-zlocst)/dzloc
	  z=min(float(nlocz),max(1.,z))
	endif
	iz=z
	iz1=min(iz+1,nlocz)
	fz1=z-iz
	fz=1.-fz1
c	  
	do i=1,3
	  do j=1,3
	    minv(i,j)=fy*(fz*(fx*aloc(i,j,ix,iy,iz) +
     &		fx1*aloc(i,j,ix1,iy,iz)) +
     &		fz1*(fx*aloc(i,j,ix,iy,iz1) +
     &		fx1*aloc(i,j,ix1,iy,iz1))) +
     &		fy1*(fz*(fx*aloc(i,j,ix,iy1,iz) +
     &		fx1*aloc(i,j,ix1,iy1,iz)) +
     &		fz1*(fx*aloc(i,j,ix,iy1,iz1) +
     &		fx1*aloc(i,j,ix1,iy1,iz1)))
	  enddo
	  dxyz(i)=fy*(fz*(fx*dloc(i,ix,iy,iz) +
     &	      fx1*dloc(i,ix1,iy,iz)) +
     &	      fz1*(fx*dloc(i,ix,iy,iz1) +
     &	      fx1*dloc(i,ix1,iy,iz1))) +
     &	      fy1*(fz*(fx*dloc(i,ix,iy1,iz) +
     &	      fx1*dloc(i,ix1,iy1,iz)) +
     &	      fz1*(fx*dloc(i,ix,iy1,iz1) +
     &	      fx1*dloc(i,ix1,iy1,iz1)))
	enddo
	return
	end

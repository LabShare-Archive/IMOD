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
c	  INPUTS to the program:
c	  
c	  Name of the input file to be transformed
c
c	  Name of the output file for the transformed volume
c
c	  Path name of directory (for example, /usr/tmp) where temporary files
c	  .  can be placed, or Return to have files placed in the current
c	  .  directory
c
c	  X, Y, and Z dimensions of the output file, or / to accept the default
c	  .  values, which are NZ, NY, and NX of the input volume (a 90-degree
c	  .  rotation about the Y axis)
c	  
c	  Name of the file with the matrix of inverse transformations
c	  .  (produced by FINDWARP)
c
c
c	  David Mastronarde, 11/15/96; modified for 3-D matrix of
c	  transformations, 7/23/97
c	  DNM 2/26/01: add temporary directory entry and semi-unique filenames
c	  DNM 11/6/01: fixed problem with output array size not being respected
c
	parameter (inpdim=200,limdim=10000,lmcube=limdim/inpdim,
     &	    maxloc=10000)
	parameter (limout=(inpdim*3)/2)
	real*4 array(inpdim,inpdim,inpdim),brray(limout,limout)
	real*4 cxyzin(3),cxyzout(3),cell(6),title(20),dxyzin(3)
	integer*4 nxyzin(3),mxyzin(3),nxyzout(3),indcen(3)
	common /xyz/nxin,nyin,nzin,nxout,nyout,nzout,cxin,cyin,czin
     &	    ,cxout,cyout,czout
	equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
	equivalence (cxyzin(1),cxin),(cxyzout(1),cxout)
	real*4 mfor(3,3),minv(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
	real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3)
	real*4 atmp1(3,3),atmp2(3,3),dtmp1(3),dtmp2(3),delta(3)
	real*4 aloc(3,3,maxloc),dloc(3,maxloc)
	real*4 ofsin(3,limout),amx(3,3,limout)
	integer*4 ncubes(3),nxyzcubas(3),nxyzscr(3),nbigcube(3)
	integer*4 nxyzcube(3,lmcube),ixyzcube(3,lmcube),izsec(4)
	integer*4 inmin(3),inmax(3)
	integer*2 ifile(lmcube,lmcube),izinfile(lmcube,lmcube,limout)
	real*4 freinp(10)
c
	character*80 filein,fileout,fileinv,tempdir,tempext,tempname
	character*80 temp_filename
c
c	DNM 3/8/01: initialize the time in case time(tim) doesn't work
c
	character dat*9,tim*8/'00:00:00'/
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
c
	write(*,'(1x,a,$)')'Name of input file: '
	read(5,'(a)')filein
	call imopen(5,filein,'RO')
	call irdhdr(5,nxyzin,mxyzin,mode,dminin,dmaxin,dmeanin)
c
	write(*,'(1x,a,$)')'Name of output file: '
	read(5,'(a)')fileout
	call imopen(6,fileout,'NEW')
c
	write(*,'(1x,a,/,a,$)')'Enter path name of directory for '//
     &	    'temporary files, ',' or Return to use current directory: '
	read(5,'(a)')tempdir
c
	nxout=nzin
	nyout=nyin
	nzout=nxin
	write(*,'(1x,a,2i4,i5,a,$)')'X, Y, and Z dimensions of the '//
     &	    'output file (/ for',nxout,nyout,nzout,'): '
	read(5,*)nxout,nyout,nzout
c	  
c	  get matrix for inverse transforms
c	  
	print *,'Enter name of file with inverse transformations'
	read(5,'(a)')fileinv
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
	if(nloctot.gt.maxloc)stop
     &	    'TOO MANY TRANSFORMATIONS TO FIT IN ARRAYS'
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
	idimout=inpdim/devmx-2
	idimout=min(idimout,limout)
c	  
c	  now compute sizes of nearly equal sized near cubes to fill output
c	  volume, store the starting index coordinates
c
	do i=1,3
	  ncubes(i)=(nxyzout(i)-1)/idimout+1
	  if(ncubes(i).gt.lmcube) stop
     &	      'TOO MANY CUBES IN LONGEST DIRECTION TO FIT IN ARRAYS'
	  nxyzcubas(i)=nxyzout(i)/ncubes(i)
	  nbigcube(i)=mod(nxyzout(i),ncubes(i))
	  ind=0
	  do j=1,ncubes(i)
	    ixyzcube(i,j)=ind
	    nxyzcube(i,j)=nxyzcubas(i)
	    if(j.le.nbigcube(i))nxyzcube(i,j)=nxyzcube(i,j)+1
	    ind=ind+nxyzcube(i,j)
	  enddo
	  nxyzscr(i)=nxyzcubas(i)+1
	enddo
	write(*,103)ncubes(3),ncubes(1),ncubes(2)
103	format(' Rotations done in',i3,' layers, with',i3,' by',i3,
     &	    ' cubes in each layer')
c	  
c	  open scratch files with 4 different sizes.
c	  compose temporary filenames from the time
c	  
	tempext='wrp      1'
	tempext(4:5)=tim(1:2)
	tempext(6:7)=tim(4:5)
	tempext(8:9)=tim(7:8)
	tempname=temp_filename(filein,tempdir,tempext)
c
	nxyzscr(3)=nxyzscr(3)*ncubes(1)*ncubes(2)
	call ialprt(.false.)
	call imopen(1,tempname,'scratch')
	call icrhdr(1,nxyzscr,nxyzscr,mode,title,0)
c	  
	tempext(10:10)='2'
	tempname=temp_filename(filein,tempdir,tempext)
	nxyzscr(1)=nxyzscr(1)-1
	call imopen(2,tempname,'scratch')
	call icrhdr(2,nxyzscr,nxyzscr,mode,title,0)
c	  
	tempext(10:10)='4'
	tempname=temp_filename(filein,tempdir,tempext)
	nxyzscr(2)=nxyzscr(2)-1
	call imopen(4,tempname,'scratch')
	call icrhdr(4,nxyzscr,nxyzscr,mode,title,0)
c	  
	tempext(10:10)='3'
	tempname=temp_filename(filein,tempdir,tempext)
	nxyzscr(1)=nxyzscr(1)+1
	call imopen(3,tempname,'scratch')
	call icrhdr(3,nxyzscr,nxyzscr,mode,title,0)
c	  
c	  get an array of file numbers for each cube in X/Y plane
c
	do ix=1,ncubes(1)
	  do iy=1,ncubes(2)
	    ifile(ix,iy)=1
	    if(ix.gt.nbigcube(1))ifile(ix,iy)=ifile(ix,iy)+1
	    if(iy.gt.nbigcube(2))ifile(ix,iy)=ifile(ix,iy)+2
	  enddo
	enddo
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
	      do ifx=0,1
		do ify=0,1
		  do ifz=0,1
		    xcen=ixyzcube(1,ixcube)+ifx*nxyzcube(1,ixcube)-cxout
		    ycen=ixyzcube(2,iycube)+ify*nxyzcube(2,iycube)-cyout
		    zcen=ixyzcube(3,izcube)+ifz*nxyzcube(3,izcube)-czout
		    call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &			zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &			zcen,minv,cxyzin) 
		    do i=1,3
		      ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &			  minv(i,3)*zcen+cxyzin(i)+nxyzin(i)/2)
		      inmin(i)=max(0,min(inmin(i),ival-1))
		      inmax(i)=min(nxyzin(i)-1,max(inmax(i),ival+1))
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
c			do triquadratic interpolation with higher-order terms 
c			omitted
c			
		      ixp=nint(xp)
		      iyp=nint(yp)
		      izp=nint(zp)
		      IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &			  IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &			  IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
			dx=xp-ixp
			dy=yp-iyp
			dz=zp-izp
			ixpp1=min(ixlim,ixp+1)
			iypp1=min(iylim,iyp+1)
			izpp1=min(izlim,izp+1)
			ixpm1=max(1,ixp-1)
			iypm1=max(1,iyp-1)
			izpm1=max(1,izp-1)
C			  
C			  Set up terms for quadratic interpolation
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
     &			    + (e*dz+f)*dz + v5
			if(bval.gt.vmax)bval=vmax
			if(bval.lt.vmin)bval=vmin
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
	    enddo
	  enddo
c	    
c	    whole layer of cubes in z is done.  now reread and compose one
c	    output section at a time in array
c	    
	  do iz=1,nxyzcube(3,izcube)
	    do ixcube=1,ncubes(1)
	      do iycube=1,ncubes(2)
		iunit=ifile(ixcube,iycube)
		longint=izinfile(ixcube,iycube,iz)
		call imposn(iunit,longint,0)
		call irdsec(iunit,brray,*99)
		call pack_piece(array,nxout,nyout,ixyzcube(1,ixcube),
     &		    ixyzcube(2,iycube),brray,nxyzcube(1,ixcube),
     &		    nxyzcube(2,iycube))
	      enddo
	    enddo
	    call iclden(array,nxout,nyout,1,nxout,1,nyout,tmin,tmax,
     &		tmean)
	    dmin=min(dmin,tmin)
	    dmax=max(dmax,tmax)
	    tsum=tsum+tmean
	    call iwrsec(6,array)
	  enddo
 	enddo
c
	dmean=tsum/nzout
	call iwrhdr(6,title,1,dmin,dmax,dmean)
	do i=1,6
	  call imclose(i)
	enddo
	call exit(0)
99	print *, 'read error'
	call exit(1)
	end

	subroutine pack_piece (array,ixdout,iydout,ixofs,iyofs,
     &	    brray,nxin,nyin)
	real*4 array(ixdout,iydout),brray(nxin,nyin)
	do iy=1,nyin
	  do ix=1,nxin
	    array(ix+ixofs,iy+iyofs)=brray(ix,iy)
	  enddo
	enddo
	return
	end


	subroutine interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,zlocst,
     &	    dzloc,nlocx, nlocy, nlocz, xcen,ycen,zcen,minv,dxyz)
	real*4 aloc(3,3,nlocx,nlocy,nlocz),dloc(3,nlocx,nlocy,nlocz)
	real*4 minv(3,3),dxyz(3)
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

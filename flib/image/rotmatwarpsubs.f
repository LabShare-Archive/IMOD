c	  rotmatwarpsubs.f  - contains subroutines for Rotatevol, Matchvol, and
c	  Warpvol
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2004/07/24 17:35:38  mast
c	  Added progress output
c	
c	  Revision 3.2  2004/06/22 19:57:48  mast
c	  Fixed computation of mean
c	
c	  Revision 3.1  2003/10/11 00:20:59  mast
c	  Creation of file
c	
c	  
c
c	  setup_cubes_scratch determines the needed size of the scratch files
c	  opens the scratch files, and sets up tables for which cubes use
c	  which file
c
	subroutine setup_cubes_scratch(devmx, nExtra, filein, tempdir,
     &	    tempext, tim)
	implicit none
	real*4 devmx
	character*(*) filein
	character*(*) tempdir
	character*(*) tempext
	character*(*) tim
	include 'rotmatwarp.inc'
	integer*4 nxyzcubas(3),nxyzscr(3),nbigcube(3),nExtra
	integer*4 idimout, i, ind, j, ix, iy
	character*80 temp_filename
	character*120 tempname
c
	idimout=(inpdim - nExtra) / devmx - 2
	idimout=min(idimout,limout)
c	  
c	  now compute sizes of nearly equal sized near cubes to fill output
c	  volume, store the starting index coordinates
c
	do i=1,3
	  ncubes(i)=(nxyzout(i)-1)/idimout+1
	  if(ncubes(i).gt.lmcube) call errorexit(
     &	      'TOO MANY CUBES IN LONGEST DIRECTION TO FIT IN ARRAYS')

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
	if (nxout * (nxyzcubas(2) + 1) .ge. inpdim**3)
     &	    call errorexit('OUTPUT IMAGE TOO WIDE FOR ARRAYS')
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
	if (filein .eq. ' ') return

	write(*,103)ncubes(3),ncubes(1),ncubes(2)
103	format(' Rotations done in',i3,' layers, with',i3,' by',i3,
     &	    ' cubes in each layer')
c	  
c	  open scratch files with 4 different sizes.
c	  compose temporary filenames from the time
c	  
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
	return
	end


c	  
c	  transform_cubes does the complete work of looping on all of the
c	  cubes, transforming them, writing them to files, and calling the
c	  routine to reassemble the output file.
c	  It is used by Rotatevol and Warpvol
c
	subroutine transform_cubes(interpOrder)
	implicit none
	integer*4 interpOrder
	include 'rotmatwarp.inc'
	integer*4 izsec(4)
	integer*4 inmin(3),inmax(3)
	integer*4 ixcube, iycube, izcube, i, ifx, ify, ifz, ival, ifempty
	real*4 xcen, ycen, zcen, xofsout, xofsin, yofsin, zofsin, xp, yp, zp
	integer*4 ixlim, iylim, izlim, ixp, iyp, izp, ixpp1, ixpm1
	real*4 bval, dx, dy, dz, v2, v4, v5, v6, v8, vu, vd, vmin, vmax
	integer*4 iypp1, iypm1, izpp1, izpm1, iunit, iy, ix, iz, numDone
	real*4 a, b, c, d, e, f, dmin, dmax, tsum, dmean
	real*4 xpofs, ypofs, zpofs, d11, d12, d21, d22
c
	dmin=1.e20
	dmax=-dmin
	tsum=0.
	numDone = 0
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
		    do i=1,3
		      ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &			  minv(i,3)*zcen+cxyzin(i))
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
c		  write(*,'(10i4)'),ixcube,iycube,izcube,iz,iz+1-inmin(3)
c     &		      ,inmin(1),
c     &		      inmax(1),inmin(2),inmax(2)
		  call irdpas(5,array(1,1,iz+1-inmin(3)),inpdim,inpdim,
     &		      inmin(1),inmax(1),inmin(2),inmax(2),*99)
		enddo
	      endif
c		
c		prepare offsets and limits
c
	      xofsout=ixyzcube(1,ixcube)-1-cxout
	      xofsin=cxin+1-inmin(1)
	      yofsin=cyin+1-inmin(2)
	      zofsin=czin+1-inmin(3)
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
		do iy=1,nxyzcube(2,iycube)
		  ycen=ixyzcube(2,iycube)+iy-1-cyout
		  if(ifempty.eq.0)then
		    xpofs = minv(1,2)*ycen+minv(1,3)*zcen+ xofsin
		    ypofs = minv(2,2)*ycen+minv(2,3)*zcen+ yofsin
		    zpofs = minv(3,2)*ycen+minv(3,3)*zcen+ zofsin
		    if (interpOrder .ge. 2) then
		      do ix=1,nxyzcube(1,ixcube)
			xcen=ix+xofsout
c			  
c			  get indices in array of input data
c			  
			xp=minv(1,1)*xcen+xpofs
			yp=minv(2,1)*xcen+ypofs
			zp=minv(3,1)*xcen+zpofs
			bval = DMEANIN
c			  
c			  do triquadratic interpolation with higher-order
c			  terms omitted
c			  
			ixp=nint(xp)
			iyp=nint(yp)
			izp=nint(zp)
			IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &			    IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &			    IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
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
C			    Set up terms for quadratic interpolation
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
			endif
			brray(ix,iy)=bval
		      enddo
		    else
c			
c			linear interpolation
c			
		      do ix=1,nxyzcube(1,ixcube)
			xcen=ix+xofsout
c			  
c			  get indices in array of input data
c			  
			xp=minv(1,1)*xcen+xpofs
			yp=minv(2,1)*xcen+ypofs
			zp=minv(3,1)*xcen+zpofs
			bval = DMEANIN
			ixp=int(xp)
			iyp=int(yp)
			izp=int(zp)
			IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &			    IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &			    IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
			  dx=xp-ixp
			  dy=yp-iyp
			  dz=zp-izp
			  ixpp1=min(ixlim,ixp+1)
			  iypp1=min(iylim,iyp+1)
			  izpp1=min(izlim,izp+1)
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
			brray(ix,iy)=bval
		      enddo
		    endif
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


c	  recompose_cubes takes a layer of cubes in Z out of the scratch files
c	  and writes it to the output file
c
	subroutine recompose_cubes(izcube, dmin, dmax, tsum)
	implicit none
	include 'rotmatwarp.inc'
	real*4 dmin, dmax, tsum,tmin,tmax,tmean,tmpsum
	integer*4 iz, iycube, ixcube, izcube, nLinesOut, longint, iunit
c	    
c	    whole layer of cubes in z is done.  now reread and compose one
c	    row of the output section at a time in array
c	    
	do iz=1,nxyzcube(3,izcube)
	  tmpsum = 0.
	  do iycube=1,ncubes(2)
	    nLinesOut = nxyzcube(2,iycube)
	    do ixcube=1,ncubes(1)
	      iunit=ifile(ixcube,iycube)
	      longint=izinfile(ixcube,iycube,iz)
	      call imposn(iunit,longint,0)
	      call irdsec(iunit,brray,*99)
	      call pack_piece(array,nxout,nyout,ixyzcube(1,ixcube),
     &		  0,brray,nxyzcube(1,ixcube), nLinesOut)
	    enddo
	    call iclden(array,nxout,nLinesOut,1,nxout,1,nLinesOut,tmin,tmax,
     &		tmean)
	    dmin=min(dmin,tmin)
	    dmax=max(dmax,tmax)
	    tmpsum=tmpsum+tmean*nLinesOut
	    call iwrsecl(6,array, nLinesOut)
	  enddo
	  tsum = tsum + tmpsum / nyout
	enddo
	return
99	call errorexit('READING FILE')
	end


	subroutine pack_piece (array,ixdout,iydout,ixofs,iyofs,
     &	    brray,nxin,nyin)
	implicit none
	integer*4 ixdout,iydout,ixofs,iyofs,nxin,nyin
	real*4 array(ixdout,iydout),brray(nxin,nyin)
	integer*4 ix, iy
	do iy=1,nyin
	  do ix=1,nxin
	    array(ix+ixofs,iy+iyofs)=brray(ix,iy)
	  enddo
	enddo
	return
	end


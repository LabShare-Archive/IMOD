* * * * * * TOMOPIECES * * * * * *
c	  
c	  TOMOPIECES figures out how to chop up a tomogram into pieces so that
c	  the Fourier transforms of each piece can be done in memory.  It is
c	  meant to be called from the shell script SETUPCOMBINE.  It makes its
c	  decisions based on the maximum amount of memory that should be used
c	  for doing an FFT.
c	  
c	  Its one required input is the name of the tomogram file.  Optional
c	  inputs can follow this filename on the command line: the maximum
c	  memory to be used for FFTs (in bytes); the border size for padding
c	  (and tapering) for extracted pieces in X and Z; the border size for
c	  padding in Y; and the amount of overlap to generate between the
c	  extracted pieces.
c
c	  It outputs the number of pieces in X and Z, the index coordinates in
c	  X, Y, and Z required for extracting each piece with TAPEROUTVOL,
c	  and the index coordinates required for putting the pieces back
c	  together with ASSEMBLEVOL (first the X coordinates for each position
c	  in X, then the Y coordinates, then the Z coordinates for each
c	  position in Z).
c	  
c	  David Mastronarde; revised for pieces in X 3/1/01
c
	parameter (limpart=1000)
	character*80 filin,filout
	integer*4 nxyz(3),mxyz(3)
	real*4 maxmem/4.e7/			!MAXIMUM MEMORY TO BE USED
	integer*4 izbackst(limpart),izbacknd(limpart),izoutst(limpart),
     &       izoutnd(limpart)
	integer*4 ixbackst(limpart),ixbacknd(limpart),ixoutst(limpart),
     &       ixoutnd(limpart)
	logical toobig
	minoverlap=4				!MINIMUM OVERLAP TO OUTPUT
	npadxz=8				!PADDING/TAPER EXTENT IN X/Z
	npady=4					!PADDING IN Y
	maxpiecex=19				!MAX PIECES IN X
	call getinout(1,filin,filout)
c	  
c	  if additional arguments are present, they are maxmem,
c	  npadxz, npady, and minoverlap
c
	if(iargc().gt.1)then
	  call getarg(2,filout)
	  read(filout,*)maxmem
	endif
	if(iargc().gt.2)then
	  call getarg(3,filout)
	  read(filout,*)npadxz
	endif
	if(iargc().gt.3)then
	  call getarg(4,filout)
	  read(filout,*)npady
	endif
	if(iargc().gt.4)then
	  call getarg(5,filout)
	  read(filout,*)minoverlap
	endif
c
	call ialprt(.false.)
	call imopen(1,filin,'ro')
	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
	nx=nxyz(1)
	ny=nxyz(2)
	nz=nxyz(3)
c	  
c	  loop on the possible pieces in X; for each one, get the padded size
c
	nyextr=ny
	nyout=niceframe(2*((nyextr+1)/2+npady),2,19)
	do nxp=1,maxpiecex
	  nxextr=(nx+(nxp-1)*minoverlap + nxp-1)/nxp
	  nxout=niceframe(2*((nxextr+1)/2+npadxz),2,19)
	  nzp=1
	  toobig=.true.
c	    
c	    then loop on pieces in Z, get padded size and compute padded volume
c	    size - until it is no longer too big for maxmem
c
	  do while(nzp.lt.nz/2.and.toobig)
	    nzextr=(nz+(nzp-1)*minoverlap + nzp-1)/nzp
	    nzout=niceframe(2*((nzextr+1)/2+npadxz),2,19)
	    if(nxout*nyout*(nzout*4.).gt.maxmem)then
	      nzp=nzp+1
	    else
	      toobig=.false.
	    endif
	  enddo
c	    
c	    compute perimeter of pieces and keeo track of minimum
c
	  perim=nx*nzp+nz*nxp
	  if(nxp.eq.1.or.perim.lt.perimin)then
	    perimin=perim
	    nxpmin=nxp
	    nzpmin=nzp
	  endif
	enddo
	nxp=nxpmin
	nzp=nzpmin
c	  
c	  get size to extract, and size of padded output, and offset for
c	  getting back from the padded volume
c
	nxextr=(nx+(nxp-1)*minoverlap + nxp-1)/nxp
	nxout=niceframe(2*((nxextr+1)/2+npadxz),2,19)
	nxbackofs=(nxout-nxextr)/2
c	  
c	  divide the total overlap into nearly equal parts
c
	laptot=nxextr*nxp-nx
	lapbase=laptot/max(1,nxp-1)
	lapextra=mod(laptot,max(1,nxp-1))
	ixoutst(1)=0
	ixbackst(1)=nxbackofs
c	  
c	  get coordinates to extract, and coordinates for reassembly
c
	do ip=1,nxp
	  ixoutnd(ip)=ixoutst(ip)+nxextr-1
	  if(ip.lt.nxp)then
	    lap=lapbase
	    if(ip.le.lapextra)lap=lap+1
	    laptop=lap/2
	    lapbot=lap-laptop
	    ixoutst(ip+1)=ixoutst(ip)+nxextr-lap
	    ixbacknd(ip)=nxbackofs+nxextr-1-laptop
	    ixbackst(ip+1)=nxbackofs+lapbot
	  else
	    ixbacknd(ip)=nxbackofs+nxextr-1
	  endif
	enddo
c	  
c	  do the same in Z now
c	  
	nzextr=(nz+(nzp-1)*minoverlap + nzp-1)/nzp
	nzout=niceframe(2*((nzextr+1)/2+npadxz),2,19)
	nzbackofs=(nzout-nzextr)/2
c	  
c	  divide the total overlap into nearly equal parts
c
	laptot=nzextr*nzp-nz
	lapbase=laptot/max(1,nzp-1)
	lapextra=mod(laptot,max(1,nzp-1))
	izoutst(1)=0
	izbackst(1)=nzbackofs
c	  
c	  get coordinates to extract, and coordinates for reassembly
c
	do ip=1,nzp
	  izoutnd(ip)=izoutst(ip)+nzextr-1
	  if(ip.lt.nzp)then
	    lap=lapbase
	    if(ip.le.lapextra)lap=lap+1
	    laptop=lap/2
	    lapbot=lap-laptop
	    izoutst(ip+1)=izoutst(ip)+nzextr-lap
	    izbacknd(ip)=nzbackofs+nzextr-1-laptop
	    izbackst(ip+1)=nzbackofs+lapbot
	  else
	    izbacknd(ip)=nzbackofs+nzextr-1
	  endif
	enddo
c	  
c	  get limits for Y also
c	  
	iyoutst=0
	iyoutnd=ny-1
	nybackofs=(nyout-ny)/2

	write(*,101)nxp,nzp
101	format(2i4)
	do iz=1,nzp
	  do ix=1,nxp
	    call rangeout(ixoutst(ix),ixoutnd(ix),',',filout,nout)
	    call rangeadd(iyoutst,filout,nout)
	    call rangeadd(iyoutnd,filout,nout)
	    call rangeadd(izoutst(iz),filout,nout)
	    call rangeadd(izoutnd(iz),filout,nout)
	    write(*,102)filout(1:nout)
102	    format(a)
	  enddo
	enddo
	do i=1,nxp
	  call rangeout(ixbackst(i),ixbacknd(i),',',filout,nout)
	  write(*,102)filout(1:nout)
	enddo
	call rangeout(nybackofs,nybackofs+ny-1,',',filout,nout)
	write(*,102)filout(1:nout)
	do i=1,nzp
	  call rangeout(izbackst(i),izbacknd(i),',',filout,nout)
	  write(*,102)filout(1:nout)
	enddo
c	  
	call exit(0)
	end

	subroutine rangeout(izst,iznd,link,buf,nout)
	character*(*) buf
	character*1 link
	call int_iwrite(buf,izst,nout)
	buf(nout+1:nout+1)=link
	call int_iwrite(buf(nout+2:),iznd,nadd)
	nout=nout+nadd+1
	return
	end

	subroutine rangeadd(iznd,buf,nout)
	character*(*) buf
	buf(nout+1:nout+1)=','
	call int_iwrite(buf(nout+2:),iznd,nadd)
	nout=nout+nadd+1
	return
	end

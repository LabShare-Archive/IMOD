C	  BSUBS.FOR has subroutines needed by BLEND only:
c	  
c	  READ_LIST
c	  LOCALMEAN
c	  FASTINTERP
c	  JOINT_TO_ROTRANS
c	  EDGE_TO_ROTRANS
c	  SOLVE_ROTRANS
c	  FINDEDGEFUNC
c	  EDGESWAP
c	  SETGRIDCHARS
c	  DOEDGE
c	  LINCOM_ROTRANS
c	  RECEN_ROTRANS
c	  COUNTEDGES
c	  DXYDGRINTERP
c	  CROSSVALUE
c	  FUNCTION ONEINTRP
c	  FUNCTION STANDEV
c
c	  READ_LIST get the name of a piece list file, reads the file
c	  and returns the # of pieces NPCLIST, the min and max z values MINZPC
c	  and MAXZPC, the piece coordinates I[XYZ]PCLIST, the optional negative
c	  number within each section, NEGLIST, and a logical variable MULTINEG
c	  that is true if the section 
c
	subroutine read_list(ixpclist,iypclist,izpclist,neglist,
     &	    multineg,npclist,minzpc,maxzpc,anyneg)
c
	integer*4 ixpclist(*),iypclist(*),izpclist(*),neglist(*)
	logical multineg(*),anyneg
	logical gotfirst,anyzero
	real*4 freinp(10)
	character*80 filnam,dummy
	character*32 errmess(4)/
     &	    'error opening file',
     &	    'error reading file, line',
     &	    'bad number of values on line',
     &	    'bad multinegative specification'/ 
c	  
c	  get file name and open file
c	  
10	write(*,'(1x,a,$)')'name of input piece list file: '
	read(5,'(a)')filnam
	ierr=1
	npclist=0
	anyneg=.false.
c 7/14/00 CER: remove carriagecontrol for LINUX
	open(3,file=filnam,form='formatted',status='old'
     &	    ,err=20)
	minzpc=100000
	maxzpc=-100000
c	  
c	  read each line in turn, get numbers with free format input
c
12	ierr=2
	read(3,'(a)',err=20,end=14)dummy
	lenact=len(dummy)
	do while(lenact.gt.0.and.(dummy(lenact:lenact).eq.' '.or.
     &	    dummy(lenact:lenact).eq.char(0)))
	  lenact=lenact-1
	enddo
	if(lenact.eq.0)go to 12
	call frefor(dummy,freinp,ninp)
	ierr=3
	if(ninp.lt.3.or.ninp.gt.4)go to 20	!error if fewer than 3 numbers
	npclist=npclist+1
	ixpclist(npclist)=freinp(1)
	iypclist(npclist)=freinp(2)
	izpclist(npclist)=freinp(3)
	neglist(npclist)=0			!if 4th number, its a neg #
	if(ninp.eq.4)neglist(npclist)=freinp(4)
	minzpc=min(minzpc,izpclist(npclist))
	maxzpc=max(maxzpc,izpclist(npclist))
	go to 12
c	  
c	  now check for multinegative montaging: all pieces in a section must
c	  be labeled by negative if any are
c
14	ierr=4
	do iz=minzpc,maxzpc
	  ineg=iz+1-minzpc
	  multineg(ineg)=.false.
	  anyzero=.false.
	  gotfirst=.false.
	  do ipc=1,npclist
	    if(izpclist(ipc).eq.iz)then
	      anyzero=anyzero.or.(neglist(ipc).eq.0)
	      if(gotfirst)then
		multineg(ineg)=
     &		    multineg(ineg).or.(neglist(ipc).ne.listfirst)
	      else
		listfirst=neglist(ipc)
		gotfirst=.true.
	      endif
	    endif
	  enddo
	  if(multineg(ineg).and.anyzero)go to 20
	  anyneg=anyneg.or.multineg(ineg)
	enddo
	close(3)
	return
20	write(*,'(1x,a,i6)')errmess(ierr),npclist+1
	close(3)
	go to 10
	end


c	  LOCALMEAN computes the local mean value of DXGRID and DYGRID around
c	  the location IX,IY from positions that are possible (within 1 to
c	  NXGRID, 1 to NYGRID) and have a non-negative SDGRID.
c
	subroutine localmean(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &	    iygdim,nxgrid,nygrid,ix,iy ,dxmean,dymean,ddenmean,nsum)
	real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
	real*4 sdgrid(ixgdim,iygdim),ddengrid(ixgdim,iygdim)
	nsum=0
	dxsum=0.
	dysum=0.
	densum=0.
	do i=ix-1,ix+1
	  do j=iy-1,iy+1
	    if((i.ne.ix.or.j.ne.iy).and.i.ge.1.and.i.le.nxgrid.and.
     &		j.ge.1.and.j.le.nygrid)then
	      if(sdgrid(i,j).ge.0.)then
		dxsum=dxsum+dxgrid(i,j)
		dysum=dysum+dygrid(i,j)
		densum=densum+ddengrid(i,j)
		nsum=nsum+1
	      endif	    
	    endif	    
	  enddo
	enddo
	if(nsum.eq.0)return
	dxmean=dxsum/nsum
	dymean=dysum/nsum
	ddenmean=densum/nsum
	return
	end



c	  function ONEINTRP used quadratic interpolation in a real array ARRAY
c	  (dimensions NX,NY) to find the pixel value of the point at
c	  coordinates (X,Y).  Coordinates run from 0 to NX-1.  A real value
c	  is returned.  If the pixel is not within the array, DMEAN is returned
c
	function oneintrp(array,nx,ny,x,y,dmean)
c
	real*4 array(nx,ny)
	oneintrp=dmean
	xp=x+1.
	yp=y+1.
	ixp=nint(xp)
	iyp=nint(yp)
	if(ixp.lt.1.or.ixp.gt.nx.or.iyp.lt.1.or.iyp.gt.ny)return
C	  
C	  Do quadratic interpolation
C	  
	DX = XP - IXP
	DY = YP - IYP
c	  but if on an integer boundary already, done
	if(dx.eq.0.and.dy.eq.0.)then
	  oneintrp=array(ixp,iyp)
	  return
	endif
c
	IXPP1 = MIN(NX,IXP + 1)
	IXPM1 = MAX(1,IXP - 1)
	IYPP1 = MIN(NY,IYP + 1)
	IYPM1 = MAX(1,IYP - 1)
C	  
C	  Set up terms for quadratic interpolation
C	  
	V2 = ARRAY(IXP, IYPM1)
	V4 = ARRAY(IXPM1, IYP)
	V5 = ARRAY(IXP, IYP)
	V6 = ARRAY(IXPP1, IYP)
	V8 = ARRAY(IXP, IYPP1)
c	  find min and max of all 5 points
	vmax=max(v2,v4,v5,v6,v8)
	vmin=min(v2,v4,v5,v6,v8)
C	  
	A = (V6 + V4)*.5 - V5
	B = (V8 + V2)*.5 - V5
	C = (V6 - V4)*.5
	D = (V8 - V2)*.5
C	  
c	  limit the new density to between the min and max of original points
	oneintrp = max(vmin,min(vmax
     &	    ,A*DX*DX + B*DY*DY + C*DX + D*DY + V5))
	return
	end



c	  FASTINTERP uses quadratic interpolation to fill a portion of an
c	  output byte array BRRAY (dimensions NXOUT x NLINES) from pixels in
c	  the input real array ARRAY (dimensions NXIN x NYIN).  It will fill an
c	  area within the coordinates from INDXLO to INDXHI and from INDYLO to
c	  INDYHI, where the x coordinate of the lower left corner of the output
c	  array is NEWPCXLL.  The transformation is specified by the
c	  2x2 matrix AMAT and FDX and FDY, where these must be prepared so that
c	  the coordinates in the input array (range 0 to NXIN-1 etc) are
c	  obtained simply by X = A11 * INDX + A12 * INDY + DX et (i.e. no
c	  center offsets are needed).  Pixels outside the input array are
c	  filled with DMEAN
c
	subroutine fastinterp(brray,nxout,nlines, array,nxin,nyin,indxlo,
     &	    indxhi,indylo, indyhi,newpcxll,amat,fdx, fdy,dmean)
c
	real*4 brray(nxout,nlines)
	real*4 array(nxin,nyin)
	real*4 amat(2,2),dx,dy,dmean
c	byte bv6,bv2,bv8,bv4,bv5
c	integer*4 iv6/0/,iv2/0/,iv8/0/,iv4/0/,iv5/0/
c	equivalence (iv6,bv6),(iv2,bv2),(iv8,bv8),(iv4,bv4),(iv5,bv5)
c
	do indy=indylo,indyhi
	  iyout=indy+1-indylo
	  do indx=indxlo,indxhi
	    ixout=indx+1-newpcxll
	    pixval=dmean
	    xp=amat(1,1)*indx+amat(1,2)*indy+fdx
	    yp=amat(2,1)*indx+amat(2,2)*indy+fdy
	    ixp=nint(xp)
	    iyp=nint(yp)
	    if(ixp.lt.1.or.ixp.gt.nxin.or.iyp.lt.1.or.iyp.gt.nyin)
     &		go to 80
C	  
C	      Do quadratic interpolation
C	  
	    DX = XP - IXP
	    DY = YP - IYP
	    v5=array(ixp,iyp)
c
c	      but if on an integer boundary already, done
c
	    if(dx.eq.0.and.dy.eq.0.)then
	      pixval=v5
	      go to 80
	    endif
c
	    IXPP1 = MIN(NXIN,IXP + 1)
	    IXPM1 = MAX(1,IXP - 1)
	    IYPP1 = MIN(NYIN,IYP + 1)
	    IYPM1 = MAX(1,IYP - 1)
C	  
C	      Set up terms for quadratic interpolation
C	      
	    V2 = ARRAY(IXP, IYPM1)
	    V4 = ARRAY(IXPM1, IYP)
	    V6 = ARRAY(IXPP1, IYP)
	    V8 = ARRAY(IXP, IYPP1)
c
c	      find min and max of all 5 points
c
	    vmax=max(v2,v4,v5,v6,v8)
	    vmin=min(v2,v4,v5,v6,v8)
C	  
	    A = (V6 + V4)*.5 - V5
	    B = (V8 + V2)*.5 - V5
	    C = (V6 - V4)*.5
	    D = (V8 - V2)*.5
C	  
c	      limit the new density to between min and max of original points
c
	    pixval = max(vmin,min(vmax
     &		,A*DX*DX + B*DY*DY + C*DX + D*DY + V5))
80	    brray(ixout,iyout)=pixval
	  enddo
	enddo
	return
	end



c	  JOINT_TO_ROTRANS uses the values in the list of NEDGE edge functions 
c	  in DXGRID and DYGRID (dimensions IXDIM x IYDIM, # of values in X and
c	  Y directions NXGRID x NYGRID, pixel interval between values INTXGRID
c	  and INTYGRID in X and Y directions, x and y coordinates of the lower-
c	  left corner of the edge in the lower and upper piece in IXPCLO,
c	  IYPCLO and IXPCHI,IYPCHI) to derive a rotation/translation
c	  around the center of the joint; it returns the angle in THETAMIN and
c	  the translation in DXMIN and DYMIN.
c
	subroutine joint_to_rotrans(dxgrid,dygrid,ixdim,iydim,nxgrid,
     &	    nygrid,intxgrid,intygrid,ixpclo,iypclo,ixpchi,iypchi,nedge
     &	    ,r)
c
	real*4 dxgrid(ixdim,iydim,*),dygrid(ixdim,iydim,*)
	integer*4 nxgrid(*),nygrid(*),ixpclo(*),ixpchi(*),iypclo(*)
     &	    ,iypchi(*)
c
c	structure /rotrans/
c	real*4 theta,dx,dy,xcen,ycen
c	end structure
	real*4 r(6)
c
	parameter (npnts=5000)
	real*4 x1(npnts),y1(npnts),x2(npnts),y2(npnts)
c	  
c	  make set of coordinate pairs in each grid, add up sums
	x1sum=0.
	x2sum=0.
	y1sum=0.
	y2sum=0.
	nn=0
	do ied=1,nedge
	  do ix=1,nxgrid(ied)
	    do iy=1,nygrid(ied)
	      nn=nn+1
	      x1(nn)=(ix-1)*intxgrid+ixpclo(ied)
	      y1(nn)=(iy-1)*intygrid+iypclo(ied)
	      x2(nn)=(ix-1)*intxgrid+ixpchi(ied)+dxgrid(ix,iy,ied)
	      y2(nn)=(iy-1)*intygrid+iypchi(ied)+dygrid(ix,iy,ied)
	      x1sum=x1sum+x1(nn)
	      y1sum=y1sum+y1(nn)
	    enddo
	  enddo
	enddo
c	  
c	  get center in lower image and shift points to center around there
c
	r(4)=x1sum/nn
	r(5)=y1sum/nn
	do i=1,nn
	  x1(i)=x1(i)-r(4)
	  x2(i)=x2(i)-r(4)
	  y1(i)=y1(i)-r(5)
	  y2(i)=y2(i)-r(5)
	enddo
	call solve_rotrans(x1,y1,x2,y2,nn,r(1),r(2),r(3))
	return
	end



c	  EDGE_TO_ROTRANS uses the values in an edge function specified by
c	  DXGRID and DYGRID (dimensions IXDIM x IYDIM, # of values in X and Y
c	  directions NXGRID x NYGRID, pixel interval between values INTXGRID
c	  and INTYGRID in X and Y directions) to derive a rotation/translation
c	  around the center of the edge; it returns the angle in THETAMIN and
c	  the translation in DXMIN and DYMIN.
c
	subroutine edge_to_rotrans(dxgrid,dygrid,ixdim,iydim,nxgrid,
     &	    nygrid,intxgrid,intygrid,thetamin,dxmin,dymin)
c	  
	real*4 dxgrid(ixdim,iydim),dygrid(ixdim,iydim)
	parameter (npnts=1000)
	real*4 x1(npnts),y1(npnts),x2(npnts),y2(npnts)
c	  
c	  make a set of coordinate pairs centered around center of grid
c	  
	xcen=(nxgrid-1)/2. + 1.
	ycen=(nygrid-1)/2. + 1.
	nn=0
	do ix=1,nxgrid
	  do iy=1,nygrid
	    nn=nn+1
	    x1(nn)=(ix-xcen)*intxgrid
	    y1(nn)=(iy-ycen)*intygrid
	    x2(nn)=x1(nn)+dxgrid(ix,iy)
	    y2(nn)=y1(nn)+dygrid(ix,iy)
	  enddo
	enddo
	call solve_rotrans(x1,y1,x2,y2,nn,thetamin,dxmin,dymin)
	return
	end



c	  SOLVE_ROTRANS takes a list of NN coordinate pairs in X1,Y1 and X2,Y2
c	  and finds the rotation THETAMIN about the origin and translation
c	  DXMIN and DYMIN that best superimposes the points
c
	subroutine solve_rotrans(x1,y1,x2,y2,nn,thetamin,dxmin,dymin)
	real*4 x1(*),y1(*),x2(*),y2(*)
c	  
c	  the real way to do it
c
	x1s=0.
	x2s=0.
	y1s=0.
	y2s=0.
	do i=1,nn
	  x1s=x1s+x1(i)
	  x2s=x2s+x2(i)
	  y1s=y1s+y1(i)
	  y2s=y2s+y2(i)
	enddo
	x1s=x1s/nn
	x2s=x2s/nn
	y1s=y1s/nn
	y2s=y2s/nn
	ssd23=0.
	ssd14=0.
	ssd13=0.
	ssd24=0.
	do i=1,nn
	  ssd23=ssd23+(y1(i)-y1s)*(x2(i)-x2s)
	  ssd14=ssd14+(x1(i)-x1s)*(y2(i)-y2s)
	  ssd13=ssd13+(x1(i)-x1s)*(x2(i)-x2s)
	  ssd24=ssd24+(y1(i)-y1s)*(y2(i)-y2s)
	enddo
	thetamin=0.
	if(abs(ssd13+ssd24).gt.1.e-20)
     &	    thetamin=atand(-(ssd23-ssd14)/(ssd13+ssd24))
	costh=cosd(thetamin)
	sinth=sind(thetamin)
	dxmin=x2s-x1s*costh+y1s*sinth
	dymin=y2s-x1s*sinth-y1s*costh

c$$$c	  
c$$$c	  search for angle that minimizes the sd of differences between pairs
c$$$c	  set up for big range of angles
c$$$c	  
c$$$	thetalo=-10.
c$$$	thetahi=10.
c$$$	dtheta=1.
c$$$	cuttheta=10.
c$$$	niter=4
c$$$	sdmin=1.e10
c$$$	do iter=1,niter
c$$$c
c$$$c	    get number of steps between lo and hi
c$$$c
c$$$	  nsteps=(thetahi-thetalo)/dtheta+1
c$$$	  dtheta=(thetahi-thetalo)/(nsteps+1)
c$$$	  do istep=1,nsteps
c$$$	    theta=thetalo+dtheta*(istep-1)
c$$$	    costh=cosd(theta)
c$$$	    sinth=sind(theta)
c$$$	    dxsum=0.
c$$$	    dxsumsq=0.
c$$$	    dysum=0.
c$$$	    dysumsq=0.
c$$$c	      
c$$$c	      for each theta value, back-rotate the points in X2,y2 and compute
c$$$c	      sd of their difference from points in x1,y1
c$$$c
c$$$	    do i=1,nn
c$$$	      dx=x2(i)*costh+y2(i)*sinth - x1(i)
c$$$	      dy=-x2(i)*sinth+y2(i)*costh - y1(i)
c$$$	      dxsum=dxsum+dx
c$$$	      dxsumsq=dxsumsq+dx**2
c$$$	      dysum=dysum+dy
c$$$	      dysumsq=dysumsq+dy**2
c$$$	    enddo
c$$$	    xsd=standev(dxsum,dxsumsq,nn)
c$$$	    ysd=standev(dysum,dysumsq,nn)
c$$$c
c$$$c	      just minimize the sum of the two sd's
c$$$c
c$$$	    if(xsd+ysd.lt.sdmin)then
c$$$	      sdmin=xsd+ysd
c$$$	      dxmin=dxsum/nn
c$$$	      dymin=dysum/nn
c$$$	      thetamin=theta
c$$$	    endif
c$$$	  enddo
c$$$c
c$$$c	    for next round, go between two points around minimum, cut step size
c$$$c
c$$$	  thetalo=thetamin-dtheta
c$$$	  thetahi=thetamin+dtheta
c$$$	  dtheta=dtheta/cuttheta
c$$$	enddo
	return
	end
c
	function standev(sum,sumsq,nsum)
	standev=0
	if(nsum.le.1)return
	diff=sumsq-sum**2/nsum
	if(diff.gt.0.)standev=sqrt(diff/(nsum-1))
	return
	end



c	  FINDEDGEFUNC finds the edge function relating a piece in ARRAY to a
c	  piece in BRRAY (dimensions NX by NY).  I[XY]GRIDSTR is the coordinate
c	  of the lower-left corner of the function grid in the lower piece (in
c	  ARRAY).  I[XY]OFSET is the ofset to the corresponding point in the
c	  upper piece.  NXGRID, NYGRID specify the # of grid points in X and Y;
c	  INTXGRID, INTYGRID are the pixel intervals between grid points;
c	  IXBOXSIZ and IYBOXSIZ are the dimensions of the box for image
c	  matching.  At each grid position, the displacements are returned in
c	  DXGRID, DYGRID, the SD value of the image match in SDGRID, and the
c	  mean density difference in DDENGRID (all dimension IXGDIM x IYGDIM).
c
	subroutine findedgefunc(array,brray,nx,ny,ixgridstr,iygridstr,
     &	    ixofset,iyofset,nxgrid,nygrid,intxgrid,intygrid,ixboxsiz,
     &	    iyboxsiz,intscan,dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &	    iygdim)
c
	real*4 array(*),brray(*)
	real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
	real*4 sdgrid(ixgdim,iygdim),ddengrid(ixgdim,iygdim)
c	  
c	  intercept case of no overlap and make a null grid
c	  
	if(nxgrid.le.0.or.nygrid.eq.0)then
	  nxgrid=max(1,nxgrid)
	  nygrid=max(1,nygrid)
	  do iy=1,nygrid
	    do ix=1,nxgrid
	      dxgrid(ix,iy)=0.
	      dygrid(ix,iy)=0.
	      sdgrid(ix,iy)=-1.
	      ddengrid(ix,iy)=0.
	    enddo
	  enddo
	  return
	endif
c
c	  set up grid arrays
c
	do iy=1,nygrid
	  do ix=1,nxgrid
	    sdgrid(ix,iy)=-1
	  enddo
	enddo
	idxbase=ixofset-ixgridstr
	idybase=iyofset-iygridstr
c
c	  start from the inside out: so need the center location of grid
c
	ixgrdcen=nxgrid/2
	iygrdcen=nygrid/2
c
c	  set up loops from center out
c	  i.e. outer loop for x from center to end then from center to start
c
	ixdir=1
	ixloopst=ixgrdcen+1
	ixloopnd=nxgrid
	do ixloop=1,2
	  do ixgrid=ixloopst,ixloopnd,ixdir
c
c	      then inner loop for y from center to end and x center to end
c
	    iydir=1
	    iyloopst=iygrdcen+1
	    iyloopnd=nygrid
	    do iyloop=1,2
	      do iygrid=iyloopst,iyloopnd,iydir
c
c		  to get starting dx, dy, search for and average from neighbors
c
		ixbox0=ixgridstr+(ixgrid-1)*intxgrid-ixboxsiz/2
		ixbox1=ixbox0+ixboxsiz-1
		iybox0=iygridstr+(iygrid-1)*intygrid-iyboxsiz/2
		iybox1=iybox0+iyboxsiz-1
		call localmean(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &		    iygdim,nxgrid, nygrid, ixgrid,iygrid, dxmean ,
     &		    dymean,ddenmean,nsum)
c
c		  if only one neighbor (or none), do complete scan
c
		if(nsum.le.1)then
		  idx0=-intscan
		  idx1=intscan
		  idy0=-intscan
		  idy1=intscan
		  call sdintscan(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &		      iybox1, idx0+idxbase,idy0+idybase,idx1+idxbase,
     &		      idy1+idybase, sdmin,ddenmin,idxmin,idymin)
		  dxgr=idxmin
		  dygr=idymin
		else
c
c		    otherwise take starting point as average
c
		  nsteps=4
		  dxgr=dxmean+idxbase
		  dygr=dymean+idybase
		endif
c
c		  now do the real search
c
		nsteps=4
		limstep=6
		call bigsearch(array,brray,nx,ny,ixbox0,iybox0,ixbox1
     &		    ,iybox1,dxgr,dygr, sdgrid(ixgrid,iygrid),ddengrid(
     &		    ixgrid,iygrid),nsteps,limstep)
		dxgrid(ixgrid,iygrid)=dxgr-idxbase
		dygrid(ixgrid,iygrid)=dygr-idybase
c		write(*,102)ixbox0,iybox0,dxgrid(ixgrid,iygrid)
c     &		    ,dygrid(ixgrid,iygrid),sdgrid(ixgrid,iygrid)
c    &		    ,'  is the minimum'
c102		format(2i5,2f7.2,f10.4,a)
	      enddo
	      iydir=-1
	      iyloopst=iygrdcen
	      iyloopnd=1
	    enddo
	  enddo
	  ixdir=-1
	  ixloopst=ixgrdcen
	  ixloopnd=1
	enddo
	return
	end



c	  EDGESWAP manages the edge function buffers.  Given the edge # and
c	  type with  IEDGE and IXY, it looks to see if that function is already
c	  in the buffers, reads it in if not (replacing the edge function that
c	  has the longest time since last use), and returns the index of the
c	  edge in the buffers in INDBUF.
c
	subroutine edgeswap(iedge,ixy,indbuf)
c
	parameter (limedge=50000,limedgbf=20,ixgdim=256,iygdim=256)
	integer*4 ibufedge(limedge,2),iedgbflist(limedgbf)
     &	    ,ixybflist(limedgbf),lasedguse(limedgbf),iunedge(2)
	integer*4 nxgrbf(limedgbf),nygrbf(limedgbf)
     &	    ,ixgrdstbf(limedgbf),iygrdstbf(limedgbf)
     &	    ,ixofsbf(limedgbf),iyofsbf(limedgbf)
     &	    ,intxgrbf(limedgbf),intygrbf(limedgbf)
	integer*4 intgrcopy(2)
	real*4 dxgrbf(ixgdim,iygdim,limedgbf)
     &	    ,dygrbf(ixgdim,iygdim,limedgbf)
     &	    ,ddengrbf(ixgdim,iygdim,limedgbf)
	common /edgebf/ iunedge,jusedgct,needbyteswap,ibufedge,
     &	    iedgbflist,ixybflist,
     &	    lasedguse, nxgrbf ,nygrbf,ixgrdstbf,iygrdstbf, ixofsbf,
     &	    iyofsbf,intxgrbf,intygrbf,dxgrbf,dygrbf ,ddengrbf,intgrcopy
c	  
	indbuf=ibufedge(iedge,ixy)
	if(indbuf.eq.0)then
c
c	    find oldest
c
	  minused=jusedgct+1
	  do i=1,limedgbf
	    if(minused.gt.lasedguse(i))then
	      minused=lasedguse(i)
	      ioldest=i
	    endif
	  enddo
c
c	    mark oldest as no longer present, mark this as present
c
	  if(iedgbflist(ioldest).gt.0)
     &	      ibufedge(iedgbflist(ioldest),ixybflist(ioldest))=0
	  iedgbflist(ioldest)=iedge
	  ixybflist(ioldest)=ixy
	  indbuf=ioldest
	  ibufedge(iedge,ixy)=indbuf
c
c	    read edge into  buffer
c
	  if(needbyteswap.eq.0)then
	    read(iunedge(ixy),rec=1+iedge)
     &		nxgr,nygr,ixgrdstbf(ioldest),ixofsbf(ioldest)
     &		,iygrdstbf(ioldest),iyofsbf(ioldest)
     &		,((dxgrbf(ix,iy,ioldest),dygrbf(ix,iy,ioldest)
     &		,ddengrbf(ix,iy,ioldest),ix=1,nxgr),iy=1,nygr)
	  else
	    read(iunedge(ixy),rec=1+iedge)nxgr,nygr
	    call convert_longs(nxgr,1)
	    call convert_longs(nygr,1)
	    read(iunedge(ixy),rec=1+iedge)
     &		idum1,idum2,ixgrdstbf(ioldest),ixofsbf(ioldest)
     &		,iygrdstbf(ioldest),iyofsbf(ioldest)
     &		,((dxgrbf(ix,iy,ioldest),dygrbf(ix,iy,ioldest)
     &		,ddengrbf(ix,iy,ioldest),ix=1,nxgr),iy=1,nygr)
	    call convert_longs(ixgrdstbf(ioldest),1)
	    call convert_longs(ixofsbf(ioldest),1)
	    call convert_longs(iygrdstbf(ioldest),1)
	    call convert_longs(iyofsbf(ioldest),1)
	    do iy=1,nygr
	      call convert_floats(dxgrbf(1,iy,ioldest),nxgr)
	      call convert_floats(dygrbf(1,iy,ioldest),nxgr)
	      call convert_floats(ddengrbf(1,iy,ioldest),nxgr)
	    enddo
	  endif
	  nxgrbf(ioldest)=nxgr
	  nygrbf(ioldest)=nygr
	  intxgrbf(ioldest)=intgrcopy(ixy)
	  intygrbf(ioldest)=intgrcopy(3-ixy)
	endif
c
c	  mark this one as used most recently
c
	jusedgct=jusedgct+1
	lasedguse(indbuf)=jusedgct
	return
	end



c	  SETGRIDCHARS determines the location and size of the grid for an edge
c	  function.  NXY and NOVERLAP has the frame dimensions and overlap
c	  between frames.  IBOXSIZ, INDENT, and INTGRID specify the box size
c	  for matching images, minimum indentation from edge of image, and the
c	  grid interval; element (1) is for the short direction of the edge and
c	  (2) is for the long direction.  IXY is 1 for an X, 2 for Y edge.
c	  IXDISPL, IYDISPL specify the displacement from perfect alignment
c	  already expected at the edge.  It returns the size of the grid in
c	  NXGRID and NYGRID, the pixel coordinate of the lower left corner of
c	  the grid in the lower frame in IGRIDSTR, and the offset to the
c	  corresponding place in the upper frame in IOFSET.
c
	subroutine setgridchars(nxy,noverlap,iboxsiz,indent,intgrid,
     &	    ixy,ixdispl,iydispl,nxgrid,nygrid,igridstr,iofset)
c
	integer*4 nxy(*),noverlap(*),igridstr(*),iofset(*) !indexed by x-y
	integer*4 iboxsiz(*),indent(*),intgrid(*) !indexed by short-long
	integer*4 ngrid(2),idispl(2),len(2),jndent(2)
c	  
c	  12/98 change -i[xy]displ to +i[xy]displ
	if(ixy.eq.1)then
	  idispl(1)=nxy(1)-noverlap(1)+ixdispl
	  idispl(2)=iydispl
	else
	  idispl(1)=ixdispl
	  idispl(2)=nxy(2)-noverlap(2)+iydispl
	endif
	iyx=3-ixy
c
c	  get length of overlap in short direction: limit to standard length
c
	len(ixy)=min(noverlap(ixy),nxy(ixy)-idispl(ixy))
c	  length of overlap in long direction: reduce by displacement
	len(iyx)=nxy(iyx)-abs(idispl(iyx))
c
c	  calculate extent usable within box, # gridpoints, indent to 1st point
c
	isl=ixy					!index to short-long variables
	do i=1,2
	  nextent=len(i)-iboxsiz(isl)-2*indent(isl)
	  ngrid(i)=1+nextent/intgrid(isl)
	  jndent(i)=indent(isl)
     &	      +(iboxsiz(isl)+mod(nextent,intgrid(isl)))/2
	  isl=iyx
	enddo
c
c	  calculate coordinates of grid within each frame
c	  
	do i=1,2
	  lapcen=(nxy(i)-idispl(i))/2		!offset to center of overlap
	  ihafgrid=jndent(i)-len(i)/2		!from center to start of grid
	  iofset(i)=lapcen+ihafgrid		!offset in frame 2
	  igridstr(i)=(nxy(i)-lapcen)+ihafgrid	!offset in frame 1
	enddo
c	  
	nxgrid=ngrid(1)
	nygrid=ngrid(2)
	return
	end



c	  DOEDGE "does" edge # IEDGE, whose direction is given by IXY.  It
c	  looks to see if this edge is on a joint between negatives; if so it
c	  composes the whole list of edges along that joint and arranges to
c	  find the edge functions for all of those edges, from the center
c	  outward.  Edge functions are found, then smoothed using the other
c	  arguments as parameters, then written to the appropriate edge file
c
	subroutine doedge(array,iedge,ixy,edgedone,sdcrit,devcrit,nfit,
     &	    norder,nskip,docross,xcreadin,xclegacy,edgedispx,edgedispy)
c	  
	real*4 array(*)
	integer*4 nfit(2),nskip(2)
	logical docross,xcreadin,xclegacy
c
	parameter (limnpc=50000,limedge=50000)
	integer*4 nxyzin(3),nxyzout(3)
	integer*4 noverlap(2),nedge(2)
	integer*4 ixpclist(limnpc),iypclist(limnpc) !piece coords in x,y
	integer*4 izpclist(limnpc),neglist(limnpc) !section #, negative #
	integer*4 iedgelower(limnpc,2),iedgeupper(limnpc,2)
	integer*4 ipiecelower(limedge,2),ipieceupper(limedge,2)
	integer*4 indent(2)			!minimum indent short & long
	integer*4 intgrid(2)			!grid interval short & long
	integer*4 iboxsiz(2)			!box size short & long
	real*4 edgelonear(2),edgehinear(2)	!limits for values near edges
	equivalence (nxin,nxyzin(1))
	equivalence (nxout,nxyzout(1))
	equivalence (noverlap(1),nxoverlap)
	common /pclist/nxin,nyin,nzin,nxout,nyout,nzout,nxpieces,
     &	    nypieces ,nxoverlap,nyoverlap,npclist,minxpiece,minypiece,
     &	    nedge ,ixpclist,iypclist,izpclist,neglist,iedgelower,
     &	    iedgeupper ,ipiecelower,ipieceupper,indent,intgrid,iboxsiz,
     &	    edgelonear,edgehinear
c	  
	logical edgedone(limedge,2)
	real*4 edgedispx(limedge,2),edgedispy(limedge,2)
	integer*4 iunedge(2)
	common /edgebf/ iunedge
c
	parameter (limpneg=20,ixgdim=256,iygdim=256)
	integer*4 multcoord(limpneg),multedge(limpneg),multmp(limpneg)
     &	    ,mcotmp(limpneg),igrstr(2),igrofs(2)
	real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
	real*4 ddengrid(ixgdim,iygdim),sdgrid(ixgdim,iygdim)
c	  
c	  make list of edges to be done
c
	intxgrid=intgrid(ixy)
	intygrid=intgrid(3-ixy)
	nmult=1
	multedge(1)=iedge
	intscan=6
	ipclo=ipiecelower(iedge,ixy)
	ipcup=ipieceupper(iedge,ixy)
	if(neglist(ipclo).ne.neglist(ipcup))then
c	    
c	    if edge is across a negative boundary, need to look for all such
c	    edges and add them to list
c	    
	  nmult=0
	  intscan=9
	  do i=1,nedge(ixy)
	    ipc=ipiecelower(i,ixy)
	    if(izpclist(ipclo).eq.izpclist(ipc).and.
     &		neglist(ipclo).eq. neglist(ipc).and.
     &		neglist(ipcup).eq.neglist(ipieceupper(i,ixy)))then
	      nmult=nmult+1
	      multedge(nmult)=i
c		get coordinate of edge in ortho direction
	      if(ixy.eq.1)then
		mltco=iypclist(ipc)
	      else
		mltco=ixpclist(ipc)
	      endif
	      multcoord(nmult)=mltco
	    endif
	  enddo
c	      
c	    order list to go out from center of edge.  GROSS.. first order it
c
	  do i=1,nmult-1
	    do j=i,nmult
	      if(multcoord(i).gt.multcoord(j))then
		itmp=multcoord(i)
		multcoord(i)=multcoord(j)
		multcoord(j)=itmp
		itmp=multedge(i)
		multedge(i)=multedge(j)
		multedge(j)=itmp
	      endif
	    enddo
	  enddo
	  midcoord=(multcoord(nmult)+multcoord(1))/2
c	    
c	    find element closest to center
c	    
	  mindiff=100000
	  do i=1,nmult
	    idiff=abs(multcoord(i)-midcoord)
	    if(idiff.lt.mindiff)then
	      mindiff=idiff
	      imid=i
	    endif
	  enddo
c	    
c	    set up order from there to top then back from middle to bottom
c	    
	  imult=0
	  do i=imid,nmult
	    imult=imult+1
	    multmp(imult)=multedge(i)
	    mcotmp(imult)=multcoord(i)
	  enddo
	  middone=imult
	  do i=imid-1,1,-1
	    imult=imult+1
	    multmp(imult)=multedge(i)
	    mcotmp(imult)=multcoord(i)
	  enddo
	  do i=1,nmult
	    multedge(i)=multmp(i)
	    multcoord(i)=mcotmp(i)
	  enddo
	endif
c	    
c	  finally ready to set up to get edge
c
	do imult=1,nmult
	  jedge=multedge(imult)
c	    
	  call shuffler(array,ipiecelower(jedge,ixy),indlow)
	  call shuffler(array,ipieceupper(jedge,ixy),indup)
c	    
	  if(imult.eq.1)then
c	  
c	      for first time, set these parameters to
c	      their default values with 0 offsets
c
	    ixdisp=0
	    iydisp=0
	    if(docross)then
	      if(xcreadin)then
		xdisp=edgedispx(jedge,ixy)
		ydisp=edgedispy(jedge,ixy)
	      else
		call xcorredge(array(indlow),array(indup),
     &		    nxyzin, noverlap,ixy,xdisp,ydisp,xclegacy)
		edgedispx(jedge,ixy)=xdisp
		edgedispy(jedge,ixy)=ydisp
	      endif
	      ixdisp=nint(xdisp)
	      iydisp=nint(ydisp)
c	      write(*,'(1x,a,2i4,a,2i4)')
c     &	      char(ixy+ichar('W'))//' edge, pieces'
c     &	      ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c     &	      '  ixydisp:',ixdisp,iydisp
	    endif
	    ixdispmid=ixdisp
	    iydispmid=iydisp
c
	  else
	    if(imult.eq. middone+1)then
c	    
	      theta=thetamid			!at midway point, restore
	      dxedge=dxmid			!values from first (middle)
	      dyedge=dymid			!edge
	      lastedge=multedge(1)
	      lastxdisp=ixdispmid
	      lastydisp=iydispmid
	    else
	      call edge_to_rotrans(dxgrid,dygrid,ixgdim,iygdim,nxgr,
     &		  nygr,intxgrid,intygrid,theta,dxedge,dyedge)
	      if(imult.eq.2)then
		thetamid=theta			!if that was first edge, save
		dxmid=dxedge			!the value
		dymid=dyedge
	      endif
	      lastedge=multedge(imult-1)
	    endif
c
c	      find displacement of center of next edge relative to center of
c	      last edge.  First get x/y displacements between edges
c
	    xdispl=ixpclist(ipieceupper(jedge,ixy))-
     &		ixpclist(ipieceupper(lastedge,ixy))
	    ydispl=iypclist(ipieceupper(jedge,ixy))-
     &		iypclist(ipieceupper(lastedge,ixy))
	    costh=cosd(theta)
	    sinth=sind(theta)
c	      rotate vector by theta and displace by dx, dy; the movement in
c	      the tip of the displacement vector is the expected relative
c	      displacement between this frame and the last
	    xrel=xdispl*costh - ydispl*sinth + dxedge - xdispl
	    yrel=xdispl*sinth + ydispl*costh + dyedge - ydispl
c	      add pixel displacement of last frame to get total expected pixel
c	      displacement of this frame
	    ixdisp=nint(xrel)+lastxdisp
	    iydisp=nint(yrel)+lastydisp
	  endif
c
	  call setgridchars(nxyzin,noverlap,iboxsiz,indent,intgrid,
     &	      ixy,ixdisp,iydisp,nxgr,nygr,igrstr,igrofs)
	  lastxdisp=ixdisp
	  lastydisp=iydisp
c
c	  write(*,'(1x,a,2i4,a,2i4,a,2i4,a,2i3)')
c     &	      char(ixy+ichar('W'))//' edge, pieces'
c     &	      ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c     &	      '  ngrid:',nxgr,nygr,'  start lower:',igrstr,
c     &	      '  upper:',igrofs
	  call findedgefunc(array(indlow),array(indup),nxin,nyin,
     &	      igrstr(1),igrstr(2),igrofs(1),igrofs(2),nxgr,nygr,
     &	      intxgrid,intygrid,iboxsiz(ixy),iboxsiz(3-ixy),intscan,
     &	      dxgrid, dygrid,sdgrid, ddengrid,ixgdim,iygdim)
c
	  call smoothgrid(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &	      iygdim,nxgr,nygr,sdcrit, devcrit,nfit(ixy),nfit(3-ixy),
     &	      norder, nskip(ixy),nskip(3-ixy))
c	    
	  write(iunedge(ixy),rec=jedge+1)nxgr,nygr,(igrstr(i),igrofs(i)
     &	      ,i=1,2) ,((dxgrid(ix,iy),dygrid(ix,iy),
     &	      ddengrid(ix,iy),ix=1,nxgr),iy=1,nygr)
c	    
	  edgedone(jedge,ixy)=.true.
	enddo
	return
	end



c	  LINCOM_ROTRANS forms a linear combination of two rotation/
c	  translations R1 and R2, with weights W1 and W2, shifts the result to
c	  the center of R2, and puts the result in S
c
	subroutine lincom_rotrans(r1,w1,r2,w2,s)
c
c	structure /rotrans/
c	  real*4 theta,dx,dy,xcen,ycen
c	end structure
	real*4 r1(6),r2(6),rt1(6),rt2(6),s(6)
c
	call recen_rotrans(r1,r2(4),r2(5),rt1)
	call xfcopy(r2,rt2)
	rt2(1)=w1*rt1(1) + w2*r2(1)
	rt2(2)=w1*rt1(2) + w2*r2(2)
	rt2(3)=w1*rt1(3) + w2*r2(3)
	call xfcopy(rt2,s)
	return
	end



C	  RECEN_ROTRANS shift the rotation/translation R to the new center
c	  XCENEW,YCENEW and returns the result in S
c
	subroutine recen_rotrans(r,xcenew,ycenew,s)
c
c	structure /rotrans/
c	  real*4 theta,dx,dy,xcen,ycen
c	end structure
	real*4 r(6),s(6)
c
	sinth=sind(r(1))
	cosm1=cosd(r(1))-1.
	s(1)=r(1)
	s(2)=r(2)+cosm1*(xcenew-r(4))-sinth*(ycenew-r(5))
	s(3)=r(3)+cosm1*(ycenew-r(5))+sinth*(xcenew-r(4))
	s(4)=xcenew
	s(5)=ycenew
	return
	end

c$$$	subroutine recen_xform(f,dxcen,dycen,g)
c$$$	structure /xform/
c$$$	real*4 a(2,2),dx,dy
c$$$	end structure
c$$$	record /xform/ f,g
c$$$	g=f
c$$$	g.dx=f.dx+(f.a(1,1)-1.)*dxcen + f.a(1,2)*dycen
c$$$	g.dy=f.dy+(f.a(2,2)-1.)*dycen + f.a(2,1)*dxcen
c$$$	return
c$$$	end



c	  COUNTEDGES takes coordinate INDX, INDY in the output image, converts
c	  it to XG,YG with the inverse of optional g transform, and analyses
c	  the edges and pieces that the point is in, or at least near
c
	subroutine countedges(indx,indy,xg,yg)
c	  
	parameter (limxypc=50,limnpc=50000,limedge=50000)
	integer*4 inpiece(5)			!piece # of pieces point in
	integer*4 inedge(5,2)			!edge # of edges point is in
	integer*4 numedges(2)			!number of edges in x and y
	real*4 xinpiece(5),yinpiece(5)		!x,y coordinates within piece
	integer*4 inedlower(5,2),inedupper(5,2)	!list index of piece be&ab edge
	integer*4 mappiece(limxypc,limxypc)	!map of pieces in this section
	logical dogxforms,multng		!if doing g's, if negs in sect
	real*4 hxcen,hycen			!coord of center of input frame
	real*4 gxcen,gycen			!coord of output image center
	real*4 ginv(2,3),hinv(2,3,limnpc)	!inverse of g and h xforms
	common /edgcnt/ inpiece,inedge,numedges,xinpiece,yinpiece,
     &	    inedlower, inedupper,mappiece,dogxforms,multng,numpieces,
     &	    hxcen,hycen, gxcen,gycen, ginv,hinv
c	  
	integer*4 nxyzin(3),nxyzout(3)
	integer*4 noverlap(2),nedge(2)
	integer*4 ixpclist(limnpc),iypclist(limnpc) !piece coords in x,y
	integer*4 izpclist(limnpc),neglist(limnpc) !section #, negative #
	integer*4 iedgelower(limnpc,2),iedgeupper(limnpc,2)
	integer*4 ipiecelower(limedge,2),ipieceupper(limedge,2)
	integer*4 indent(2)			!minimum indent short & long
	integer*4 intgrid(2)			!grid interval short & long
	integer*4 iboxsiz(2)			!box size short & long
	real*4 edgelonear(2),edgehinear(2)	!limits for values near edges
	equivalence (nxin,nxyzin(1))
	equivalence (nxout,nxyzout(1))
	equivalence (noverlap(1),nxoverlap)
	common /pclist/nxin,nyin,nzin,nxout,nyout,nzout,nxpieces,
     &	    nypieces ,nxoverlap,nyoverlap,npclist,minxpiece,minypiece,
     &	    nedge ,ixpclist,iypclist,izpclist,neglist,iedgelower,
     &	    iedgeupper ,ipiecelower,ipieceupper,indent,intgrid,iboxsiz,
     &	    edgelonear,edgehinear
c
	logical edgeonlist,needcheck(5,2),ngframe
	real*4 xycur(2)
c
	numpieces=0
	numedges(1)=0
	numedges(2)=0
c	  
c	  get frame # that it is nominally in: actual one or nearby valid frame
c
	xg=indx
	yg=indy
	if(dogxforms)then
	  xtmp=xg
	  xg=ginv(1,1)*xtmp+ginv(1,2)*yg+ginv(1,3)
	  yg=ginv(2,1)*xtmp+ginv(2,2)*yg+ginv(2,3)
	endif
	xframe=(xg-minxpiece-nxoverlap/2)/(nxin-nxoverlap)
	yframe=(yg-minypiece-nyoverlap/2)/(nyin-nyoverlap)
	ixframe=xframe+1.			!truncation gives proper frame
	iyframe=yframe+1.
	ngframe=ixframe.lt.1.or.ixframe.gt.nxpieces.or.
     &	    iyframe.lt.1.or.iyframe.gt.nypieces.or.
     &	    mappiece(max(1,ixframe),max(1,iyframe)).eq.0
	if(.not.ngframe.and.multng)then
c
c	    if there are multineg h's, need to make sure point is actually in
c	    frame
c
	  ipc=mappiece(ixframe,iyframe)
	  xtmp=xg-ixpclist(ipc)
	  ytmp=yg-iypclist(ipc)
	  xbak=hinv(1,1,ipc)*xtmp+hinv(1,2,ipc)*ytmp
     &	      +hinv(1,3,ipc)
	  ybak=hinv(2,1,ipc)*xtmp+hinv(2,2,ipc)*ytmp
     &	      +hinv(2,3,ipc)
	  ngframe=xbak.lt.0.or.xbak.gt.nxin-1.or.
     &	      ybak.lt.0.or.ybak.gt.nyin-1
c	    
c	    but in this case, if in a corner, switch to the 9-piece search
c	    to start with most interior point
c
	  if(.not.ngframe)ngframe=
     &	      (xbak.lt.edgelonear(1).and.ybak.lt.edgelonear(2)).or.
     &	      (xbak.lt.edgelonear(1).and.ybak.gt.edgehinear(2)).or.
     &	      (xbak.gt.edgehinear(1).and.ybak.lt.edgelonear(2)).or.
     &	      (xbak.gt.edgehinear(1).and.ybak.gt.edgehinear(2))
	endif
c
c	    if not a good frame, look in square of nine potential pieces,
c	    switch to the one that the point is a minimum distance from
c	  
	if(ngframe)then
	  distmin=1.e10
c	
c	    continue loop to find most piece where point is most interior,
c	    not just to find the first one.  This should be run rarely so
c	    it is not a big drain
c

	  ixfrm=max(1,min(nxpieces,ixframe-1))
	  do while( ixfrm.le.min(nxpieces,max(1,ixframe+1)))
	    iyfrm=max(1,min(nypieces,iyframe-1))
	    do while( iyfrm.le.min(nypieces,max(1,iyframe+1)))
	      ipc=mappiece(ixfrm,iyfrm)
	      if(ipc.ne.0)then
c
c		  get real coordinate in piece, adjusting for h if present
c
		xtmp=xg-ixpclist(ipc)
		ytmp=yg-iypclist(ipc)
		if(multng)then
		  xttmp=xtmp
		  xtmp=hinv(1,1,ipc)*xttmp+hinv(1,2,ipc)*ytmp
     &		      +hinv(1,3,ipc)
		  ytmp=hinv(2,1,ipc)*xttmp+hinv(2,2,ipc)*ytmp
     &		      +hinv(2,3,ipc)
		endif
c		  
c		  distance is negative for a piece that point is actually in;
c		  it is negative of distance from nearest edge
c
		dist=max(-xtmp,xtmp-(nxin-1),-ytmp,ytmp-(nyin-1))
		if(dist.lt.distmin)then
		  distmin=dist
		  minxframe=ixfrm
		  minyframe=iyfrm
		endif
	      endif
	      iyfrm=iyfrm+1
	    enddo
	    ixfrm=ixfrm+1
	  enddo
	  if(distmin.eq.1.e10)return
	  ixframe=minxframe
	  iyframe=minyframe
	endif
c	  
c	  initialize list of pieces with this piece # on it, then start looping
c	  over the pieces present in the list
c
	numpieces=1
	inpiece(1)=mappiece(ixframe,iyframe)
	indinp=1
	needcheck(1,1)=.true.
	needcheck(1,2)=.true.
	do while (indinp.le.numpieces)
c
c	    come into this loop looking at a piece onlist; need to get true
c	    coordinates in piece and see if point is near/in an edge to another
c	    Start by translating into coordinates in piece
c
	  ipc=inpiece(indinp)
	  xycur(1)=xg-ixpclist(ipc)
	  xycur(2)=yg-iypclist(ipc)
c
c	    if there are multineg h's, need to compute (again!) the
c	    location within the piece
c
	  if(multng)then
	    xtmp=xycur(1)
	    xycur(1)=hinv(1,1,ipc)*xtmp+hinv(1,2,ipc)*xycur(2)
     &		+hinv(1,3,ipc)
	    xycur(2)=hinv(2,1,ipc)*xtmp+hinv(2,2,ipc)*xycur(2)
     &		+hinv(2,3,ipc)
	  endif
	  xinpiece(indinp)=xycur(1)
	  yinpiece(indinp)=xycur(2)
c	    
c	    check the x and y directions to see if point is near an edge
c
	  do ixy=1,2
	    if(needcheck(indinp,ixy))then
	      newedge=0
	      if(xycur(ixy).lt.edgelonear(ixy).and.
     &		  iedgelower(ipc,ixy).gt.0)then
		newedge=iedgelower(ipc,ixy)
		newpiece=ipiecelower(newedge,ixy)
		iflo=1
	      endif
	      if(xycur(ixy).gt.edgehinear(ixy).and.
     &		  iedgeupper(ipc,ixy).gt.0)then
		newedge=iedgeupper(ipc,ixy)
		newpiece=ipieceupper(newedge,ixy)
		iflo=0
	      endif
c
c		if either check picked up a new edge, see if edge is on list
c		already
c
	      if(newedge.ne.0)then
		edgeonlist=.false.
		do i=1,numedges(ixy)
		  edgeonlist=edgeonlist.or.(inedge(i,ixy).eq.newedge)
		enddo
c		  
c		  if not, add it, and the implied piece, to list
c		  
		if(.not.edgeonlist)then
		  listno=0
		  do i=1,numpieces
		    if(newpiece.eq.inpiece(i))listno=i
		  enddo
		  if(listno.eq.0)then
c		      
c		      but if adding a new piece, check for point actually in
c		      piece first
c		      
		    xbak=xg-ixpclist(newpiece)
		    ybak=yg-iypclist(newpiece)
		    if(multng)then
		      xtmp=xbak
		      xbak=hinv(1,1,newpiece)*xtmp+
     &			  hinv(1,2,newpiece)*ybak
     &			  +hinv(1,3,newpiece)
		      ybak=hinv(2,1,newpiece)*xtmp+
     &			  hinv(2,2,newpiece)*ybak
     &			  +hinv(2,3,newpiece)
		    endif
		    if(xbak.ge.0.and.xbak.le.nxin-1.and.
     &			ybak.ge.0.and.ybak.le.nyin-1)then
		      numpieces=numpieces+1
		      inpiece(numpieces)=newpiece
		      needcheck(numpieces,ixy)=.false.
		      needcheck(numpieces,3-ixy)=.true.
		      listno=numpieces
		    endif
		  endif
c		    
c		    add edge to list only if legal piece found
c
		  if(listno.gt.0)then
		    numedges(ixy)=numedges(ixy)+1
		    inedge(numedges(ixy),ixy)=newedge
		    if(iflo.eq.0)then
		      inedupper(numedges(ixy),ixy)=listno
		      inedlower(numedges(ixy),ixy)=indinp
		    else
		      inedupper(numedges(ixy),ixy)=indinp
		      inedlower(numedges(ixy),ixy)=listno
		    endif
		  endif
		endif
	      endif
	    endif
	  enddo
	  indinp=indinp+1
	enddo
	return
	end



c	  DXYDGRINTERP takes a coordinate X1,Y1 in the lower piece of the edge
c	  at index INDEDG in the edge buffer, finds the coordinate within the
c	  edge function grid, uses bilinear interpolation to find the values of
c	  DX, DY and DDEN, and returns the coordinate in the upper piece, X2,Y2
c	  and the average density difference DDEN
c
	subroutine dxydgrinterp(x1,y1,indedg,x2,y2,dden)
c
	parameter (limedge=50000,limedgbf=20,ixgdim=256,iygdim=256)
	integer*4 ibufedge(limedge,2),iedgbflist(limedgbf)
     &	    ,ixybflist(limedgbf),lasedguse(limedgbf),iunedge(2)
	integer*4 nxgrbf(limedgbf),nygrbf(limedgbf)
     &	    ,ixgrdstbf(limedgbf),iygrdstbf(limedgbf)
     &	    ,ixofsbf(limedgbf),iyofsbf(limedgbf)
     &	    ,intxgrbf(limedgbf),intygrbf(limedgbf)
	real*4 dxgrbf(ixgdim,iygdim,limedgbf)
     &	    ,dygrbf(ixgdim,iygdim,limedgbf)
     &	    ,ddengrbf(ixgdim,iygdim,limedgbf)
	integer*4 intgrcopy(2)
	common /edgebf/ iunedge,jusedgct,needbyteswap,ibufedge,
     &	    iedgbflist,ixybflist,
     &	    lasedguse, nxgrbf ,nygrbf,ixgrdstbf,iygrdstbf, ixofsbf,
     &	    iyofsbf,intxgrbf,intygrbf,dxgrbf,dygrbf ,ddengrbf,intgrcopy
c	  
c	  find fractional coordinate within edge grid
c	  
	xingrid=x1-ixgrdstbf(indedg)
	yingrid=y1-iygrdstbf(indedg)
	xgrid=1.+(xingrid)/intxgrbf(indedg)
	ygrid=1.+(yingrid)/intygrbf(indedg)
c	  
c	  find all fractions and indices needed for bilinear interpolation
c
	ixg=xgrid
	ixg=max(1,min(nxgrbf(indedg)-1,ixg))
	iyg=ygrid
	iyg=max(1,min(nygrbf(indedg)-1,iyg))
	fx1=max(0.,min(1.,xgrid-ixg))		!NO EXTRAPOLATIONS ALLOWED
	fx=1.-fx1
	ixg1=ixg+1
	fy1=max(0.,min(1.,ygrid-iyg))
	fy=1.-fy1
	iyg1=iyg+1
	c00=fx*fy
	c10=fx1*fy
	c01=fx*fy1
	c11=fx1*fy1
c	  
c	  interpolate
c
	dxinterp=c00*dxgrbf(ixg,iyg,indedg)+c10*dxgrbf(ixg1,iyg,indedg)
     &	    +c01*dxgrbf(ixg,iyg1,indedg)+c11*dxgrbf(ixg1,iyg1,indedg)
	dyinterp=c00*dygrbf(ixg,iyg,indedg)+c10*dygrbf(ixg1,iyg,indedg)
     &	    +c01*dygrbf(ixg,iyg1,indedg)+c11*dygrbf(ixg1,iyg1,indedg)
	dden=c00*ddengrbf(ixg,iyg,indedg)+c10*ddengrbf(ixg1,iyg,indedg)
     &	    +c01*ddengrbf(ixg,iyg1,indedg)+c11*ddengrbf(ixg1,iyg1,indedg)
c
	x2=xingrid+dxinterp+ixofsbf(indedg)
	y2=yingrid+dyinterp+iyofsbf(indedg)
	return
	end

	subroutine crossvalue(xinlong,nxpieces,nypieces,nshort,nlong)
	logical xinlong
	if(xinlong)then
	  nshort=nypieces
	  nlong=nxpieces
	else
	  nshort=nxpieces
	  nlong=nypieces
	endif
	return
	end


	subroutine xcorredge(crray,drray,nxy,noverlap,ixy,xdisp,ydisp,
     &	    legacy)
	real*4 crray(*),drray(*)
	integer*4 nxy(*),noverlap(*)
	parameter (idimt=200*400, idimc=256*512)
	real*4 trray(idimt)
	complex*8 array(idimc/2), brray(idimc/2)
	integer*4 nxybox(2),ind0(2),ind1(2),idispl(2)
	real*4 ctf(8193),rdispl(2)
	logical legacy

	aspectmax=2.0				!maximum aspect ratio of block
	indent=5				!indent for sdsearch
	overfrac=0.9				!fraction of overlap to use
	niter=4					!iterations for sdsearch
	limstep=10				!limiting distance
c	  
c	  find size and limits of box in overlap zone to cut out
c
	iyx=3-ixy
	nxybox(ixy)=noverlap(ixy)
	nxybox(iyx)=min(nxy(iyx), int(aspectmax*noverlap(ixy)))
c	nxybox(iyx)=nxy(iyx)
	ind0(iyx)=nxy(iyx)/2 - nxybox(iyx)/2 
	ind1(iyx)=nxy(iyx)/2 + nxybox(iyx)/2 - 1
	ind0(ixy)=nxy(ixy)-noverlap(ixy)
	ind1(ixy)=nxy(ixy)-1
c	  
c	  get the padded size and the taper extents
c
	nxbord=max(5,nint(0.45*nxybox(1)))
	nybord=max(5,nint(0.45*nxybox(2)))
	nxpad=niceframe(nxybox(1)+2*nxbord,2,19)
	nypad=niceframe(nxybox(2)+2*nybord,2,19)
	nxtap=max(5,nint(0.05*nxybox(1)))
	nytap=max(5,nint(0.05*nxybox(2)))

	if(nxybox(1)*nxybox(2).gt.idimt.or.nxpad*nypad.gt.idimc)
     &	    stop 'Overlap area too big for cross-correlation array'
c	  
c	  get the first image, lower piece
c
	call irepak(trray, crray,nxy(1),nxy(2),ind0(1),ind1(1),ind0(2),
     &	    ind1(2))
	call taperinpad(trray,nxybox(1),nxybox(2),array,nxpad+2,nxpad,
     &	    nypad,nxtap,nytap)
	call meanzero(array,nxpad+2,nxpad,nypad)
	call todfft(array,nxpad,nypad,0)
c
c	  get the second image, upper piece
c
	ind0(ixy)=0
	ind1(ixy)=noverlap(ixy)-1
c
	call irepak(trray, drray,nxy(1),nxy(2),ind0(1),ind1(1),ind0(2),
     &	    ind1(2))
	call taperinpad(trray,nxybox(1),nxybox(2),brray,nxpad+2,nxpad,
     &	    nypad,nxtap,nytap)
	call meanzero(brray,nxpad+2,nxpad,nypad)
	call todfft(brray,nxpad,nypad,0)
c	    
c	    multiply array by complex conjugate of brray, put back in array
c	    
	do jx=1,nypad*(nxpad+2)/2
	  array(jx)=array(jx)*conjg(brray(jx))
	enddo

	call setctfwsr(0.05,0.,0.,0.,ctf,nxpad,nypad,delta)
c
	if(delta.ne.0.)call filterpart(array,array,nxpad,nypad,ctf,
     &	    delta)
	call todfft(array,nxpad,nypad,1)
	call peakfind(array,nxpad+2,nypad,xpeak,ypeak,peak)
c	  
c	  return the amount to shift upper to align it to lower (verified)
c
	xdisp=xpeak
	ydisp=ypeak
	if(legacy)return
c	  
c	  the following is adopted largely from setgridchars
c
	ixdispl=nint(xdisp)
	iydispl=nint(ydisp)
	if(ixy.eq.1)then
	  idispl(1)=nxy(1)-noverlap(1)+ixdispl
	  idispl(2)=iydispl
	else
	  idispl(1)=ixdispl
	  idispl(2)=nxy(2)-noverlap(2)+iydispl
	endif
	iyx=3-ixy
c
c	  get size of box, limit to size of overlap zone
c	  
	nxybox(ixy)=min(noverlap(ixy),nxy(ixy)-idispl(ixy))
	nxybox(ixy)=min(nxybox(ixy)-indent*2, nint(overfrac*nxybox(ixy)))
	nxybox(iyx)=nxy(iyx)-abs(idispl(iyx))
	nxybox(iyx)=min(nxybox(iyx)-indent*2, nint(aspectmax*nxybox(ixy)))
	do i=1,2
	  ind0(i)=(nxy(i)+idispl(i)-nxybox(i))/2
	  ind1(i)=ind0(i)+nxybox(i)
	  rdispl(i)=-idispl(i)
	enddo
c
c	  integer scan is not needed, but to use it uncomment this 
c
c	intscan=6
c	call sdintscan(crray,drray,nxy(1),nxy(2),ind0(1),ind0(2),ind1(1),
c     &	    ind1(2),-idispl(1)-intscan,-idispl(2)-intscan,
c     &	    -idispl(1)+intscan,-idispl(2)+intscan,sdmin,ddenmin,
c     &	    idxmin,idymin)
c	rdispl(1)=idxmin
c	rdispl(2)=idymin
c
	call bigsearch(crray,drray,nxy(1),nxy(2),ind0(1),ind0(2),ind1(1),
     &	    ind1(2),rdispl(1),rdispl(2),sdmin,ddenmin,niter,limstep)
	
	if(ixy.eq.1)then
	  xdisp=-rdispl(1)-(nxy(1)-noverlap(1))
	  ydisp=-rdispl(2)
	else
	  xdisp=-rdispl(1)
	  ydisp=-rdispl(2)-(nxy(2)-noverlap(2))
	endif
c	write(*,'(2f8.2,2f8.2)')xpeak,ypeak,xdisp,ydisp
	return
	end



c	  PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c	  in the array ARRAY, which is dimensioned to nx+2 by ny.
c
	subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,peak)
	real*4 array(nxplus,nyrot),amat(9)
	nxrot=nxplus-2
c	  
c	  find peak
c
	peak=-1.e30
	do iy=1,nyrot
	  do ix=1,nxrot
	    if(array(ix,iy).gt.peak)then
	      peak=array(ix,iy)
	      ixpeak=ix
	      iypeak=iy
	    endif
	  enddo
	enddo
c	print *,ixpeak,iypeak
c	  
c	  simply fit a parabola to the two adjacent points in X or Y
c
	cx=0.
	y1=array(indmap(ixpeak-1,nxrot),iypeak)
	y2=peak
	y3=array(indmap(ixpeak+1,nxrot),iypeak)
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
	if(abs(cx).gt.0.5)cx=sign(0.5,cx)
c	print *,'X',y1,y2,y3,cx
	cy=0.
	y1=array(ixpeak,indmap(iypeak-1,nyrot))
	y3=array(ixpeak,indmap(iypeak+1,nyrot))
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.-1.e6)cy=(y1-y3)/denom
	if(abs(cy).gt.0.5)cy=sign(0.5,cy)
c	print *,'Y',y1,y2,y3,cy
c	  
c	  return adjusted pixel coordinate minus 1
c
	xpeak=ixpeak+cx-1.
	ypeak=iypeak+cy-1.
	if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
	if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
c	print *,xpeak,ypeak
	return
	end

c$$$
c$$$
c$$$
c$$$c	  PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c$$$c	  in the array ARRAY, which is dimensioned to nx+2 by ny.
c$$$c
c$$$	subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,peak)
c$$$	real*4 array(nxplus,nyrot)
c$$$	nxrot=nxplus-2
c$$$c	  
c$$$c	  find peak
c$$$c
c$$$	peak=-1.e30
c$$$	do iy=1,nyrot
c$$$	  do ix=1,nxrot
c$$$	    if(array(ix,iy).gt.peak)then
c$$$	      peak=array(ix,iy)
c$$$	      ixpeak=ix
c$$$	      iypeak=iy
c$$$	    endif
c$$$	  enddo
c$$$	enddo
c$$$c	print *,ixpeak,iypeak
c$$$c	  
c$$$c	  return adjusted pixel coordinate minus 1
c$$$c
c$$$	xpeak=ixpeak-1.
c$$$	ypeak=iypeak-1.
c$$$	if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
c$$$	if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
c$$$c	print *,xpeak,ypeak
c$$$	return
c$$$	end


	subroutine find_best_shifts(dxgridmean,dygridmean,idir,izsect,h,
     &	    hinv, nsum,bavg,bmax,aavg,amax)

	parameter (limxypc=50,limnpc=50000,limedge=50000,limsect=1000)
	integer*4 nxyzin(3),nxyzout(3),nxin,nyin,nzin,nxout,nyout,nzout
	integer*4 noverlap(2),nedge(2),nxoverlap,nyoverlap
	integer*4 ixpclist(limnpc),iypclist(limnpc) !piece coords in x,y
	integer*4 izpclist(limnpc),neglist(limnpc) !section #, negative #
	integer*4 iedgelower(limnpc,2),iedgeupper(limnpc,2)
	integer*4 ipiecelower(limedge,2),ipieceupper(limedge,2)
	integer*4 indent(2)			!minimum indent short & long
	integer*4 intgrid(2)			!grid interval short & long
	integer*4 iboxsiz(2)			!box size short & long
	real*4 edgelonear(2),edgehinear(2)	!limits for values near edges
	equivalence (nxin,nxyzin(1))
	equivalence (nxout,nxyzout(1))
	equivalence (noverlap(1),nxoverlap)
	common /pclist/nxin,nyin,nzin,nxout,nyout,nzout,nxpieces,
     &	    nypieces ,nxoverlap,nyoverlap,npclist,minxpiece,minypiece,
     &	    nedge ,ixpclist,iypclist,izpclist,neglist,iedgelower,
     &	    iedgeupper ,ipiecelower,ipieceupper,indent,intgrid,iboxsiz,
     &	    edgelonear,edgehinear
c	  
	real*4 h(2,3,*),hinv(2,3,*)
	real*4 dxgridmean(limedge,2),dygridmean(limedge,2)
	integer*4 indvar(limnpc)
c	  
	parameter (limvar=400)
	real*4 a(limvar,limvar),b(limvar,2)
	integer*4 ivarpc(limvar)
c
c	  The data coming in are the displacements of upper piece from
c	  being in alignment with the lower piece if idir = 1, or the
c	  shift needed to align upper piece with lower if idir = -1
c	  
c	  build list of variables
c	  
	nvar=0
	do ipc=1,npclist
	  if(izpclist(ipc).eq.izsect)then
	    call xfunit(h(1,1,ipc),1.)
	    call xfunit(hinv(1,1,ipc),1.)
	    indvar(ipc)=0
	    if(iedgelower(ipc,1).gt.0.or.
     &		iedgelower(ipc,2).gt.0.or.
     &		iedgeupper(ipc,1).gt.0.or.
     &		iedgeupper(ipc,2).gt.0)then
	      nvar=nvar+1
	      if (nvar.gt.limvar)stop
     &		  'TOO MANY PIECES FOR ARRAYS IN FIND_BEST_SHIFTS'
	      ivarpc(nvar)=ipc
	      indvar(ipc)=nvar
	    endif
	  endif
	enddo
	nsum=0
	bsum=0.
	bmax=0.
	asum=0.
	amax=0.
	bavg=0.
	aavg=0.
	if(nvar.eq.1)return
c	  
c	  build matrix of simultaneous equations for minimization solution
c
	do ivar=1,nvar-1
	  ipc=ivarpc(ivar)
	  do m=1,nvar-1
	    a(ivar,m)=0.
	    b(ivar,1)=0.
	    b(ivar,2)=0.
	  enddo
c
	  do ixy=1,2
	    iedge=iedgelower(ipc,ixy)
	    if(iedge.gt.0)then
	      a(ivar,ivar)=a(ivar,ivar)+1
	      neighpc=ipiecelower(iedge,ixy)
	      neighvar=indvar(neighpc)
c		
c		for a regular neighbor, enter a -1 in its term; but for the
c		last variable being eliminated, enter a +1 for ALL other
c		variables instead
c
	      if(neighvar.ne.nvar)then
		a(ivar,neighvar)=a(ivar,neighvar)-1
	      else
		do m=1,nvar-1
		  a(ivar,m)=a(ivar,m)+1
		enddo
	      endif
c
c		when this piece is an upper piece, subtract displacements from
c		constant term
c
	      b(ivar,1)=b(ivar,1)-idir*dxgridmean(iedge,ixy)
	      b(ivar,2)=b(ivar,2)-idir*dygridmean(iedge,ixy)
	    endif
c
	    iedge=iedgeupper(ipc,ixy)
	    if(iedge.gt.0)then
	      a(ivar,ivar)=a(ivar,ivar)+1
	      neighpc=ipieceupper(iedge,ixy)
	      neighvar=indvar(neighpc)
	      if(neighvar.ne.nvar)then
		a(ivar,neighvar)=a(ivar,neighvar)-1
	      else
		do m=1,nvar-1
		  a(ivar,m)=a(ivar,m)+1
		enddo
	      endif
c		
c		when a lower piece, add displacements to constant terms
c
	      b(ivar,1)=b(ivar,1)+idir*dxgridmean(iedge,ixy)
	      b(ivar,2)=b(ivar,2)+idir*dygridmean(iedge,ixy)
	    endif
	  enddo
	enddo
c	  
c	  solve the equations, take the b values as dx and dy; compute the
c	  sum to get the shift for the final piece
c
c	write(*,'(9i5)')(ivarpc(i),i=1,nvar)
c	write(*,'(8f7.1)')((a(i,j),i=1,nvar-1),j=1,nvar-1)
c	write(*,'(8f9.2)')((b(i,j),i=1,nvar-1),j=1,2)
	call gaussj(a,nvar-1,limvar,b,2,2)
c	write(*,'(8f9.2)')((b(i,j),i=1,nvar-1),j=1,2)
	xsum=0.
	ysum=0.
	do i=1,nvar-1
	  h(1,3,ivarpc(i))=b(i,1)
	  h(2,3,ivarpc(i))=b(i,2)
	  xsum=xsum+b(i,1)
	  ysum=ysum+b(i,2)
	enddo
	h(1,3,ivarpc(nvar))=-xsum
	h(2,3,ivarpc(nvar))=-ysum
	do i=1,nvar
	  call xfinvert(h(1,1,ivarpc(i)),hinv(1,1,ivarpc(i)))
	enddo
c	  
c	  compute and return the results 
c
	do ivar=1,nvar
	  ipc=ivarpc(ivar)
	  do ixy=1,2
	    iedge=iedgelower(ipc,ixy)
	    if(iedge.gt.0)then
	      ipclo=ipiecelower(iedge,ixy)
	      bdist=sqrt(dxgridmean(iedge,ixy)**2+
     &		  dygridmean(iedge,ixy)**2)
	      bsum=bsum+bdist
	      adist=sqrt((idir*dxgridmean(iedge,ixy)+h(1,3,ipc)-h(1,3,ipclo))
     &		  **2+(idir*dygridmean(iedge,ixy)+h(2,3,ipc)-h(2,3,ipclo))**2)
	      asum=asum+adist
	      amax=max(amax,adist)
	      bmax=max(bmax,bdist)
	      nsum=nsum+1
	    endif
	  enddo
	enddo
	bavg=bsum/nsum
	aavg=asum/nsum
c	write(*,'(i3,a,2f6.2,a,2f6.2)')nsum, ' edges, mean, max '//
c     &	    'displacement before:', bsum/nsum,bmax,', after:',asum/nsum,
c     &	    amax
	return
	end

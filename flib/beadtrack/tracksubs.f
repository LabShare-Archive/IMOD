c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine nextpos(iobj,ipnear,idir,iznext,tilt,nfit,minfit,
     &	    iaxtilt, tiltmin, xnext, ynext)
	include 'model.inc'
	real*4 tilt(*)
	real*4 xx(99),yy(99),zz(99)
	parameter (idim=30)
	include 'statsize.inc'
	real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     &	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &	    , b(msiz), b1(msiz)
	ibase=ibase_obj(iobj)
	ninobj=npt_in_obj(iobj)
	mfit=0
	ipend=1
	if(idir.eq.-1)ipend=ninobj
	if(iaxtilt.le.1)then
	  indx=1
	  indy=2
	else
	  indx=2
	  indy=1
	endif
c	  
c	  set pointer past the view if possible
c	  
	iptnear=object(ibase+ipnear)
	ip=ipnear
	do while(idir*(nint(p_coord(3,object(ibase+ip)))-iznext).ge.0
     &	    .and.idir*(ip-ipend).gt.0)
	  ip=ip-idir
	enddo
	if(idir*(nint(p_coord(3,object(ibase+ip)))-iznext).eq.-1)then
c	  
c	    if point is adjacent in that direction, then starting from there,
c	    load nfit points
c
	  do while(idir*(ip-ipend).ge.0.and.mfit.lt.nfit)
	    ipt=object(ibase+ip)
	    mfit=mfit+1
	    xx(mfit)=p_coord(indx,ipt)
	    yy(mfit)=p_coord(indy,ipt)
	    zz(mfit)=p_coord(3,ipt)
	    ip=ip-idir
	  enddo
	elseif(nint(p_coord(3,iptnear)).eq.iznext)then
c	    
c	    otherwise, if there is already a point on the section, just take it
c
	  mfit=1
	  xx(mfit)=p_coord(indx,iptnear)
	  yy(mfit)=p_coord(indy,iptnear)
	  zz(mfit)=iznear
	else
c	  
c	    otherwise, set pointers to points past and before view and get
c	    points from both directions
c
	  ipast=ipnear
	  if(p_coord(3,object(ibase+ipnear)).lt.iznext)ipast=ipnear+1
	  ibefo=ipast-1
c	  
c	    starting from there, load nfit nearest points
c
	  do while((ibefo.gt.0.or.ipast.le.ninobj).and.mfit.lt.nfit)
	    ipb=object(ibase+ibefo)
	    ipp=object(ibase+ipast)
c	    
c	      take the only one that's legal
c
	    if(ibefo.le.0)then
	      ipt=ipp
	      ipast=ipast+1
	    elseif(ipast.gt.ninobj)then
	      ipt=ipb
	      ibefo=ibefo-1
	    else
c	      
c		or if both are legal, take the one closest to target view
c
	      if(abs(p_coord(3,ipb)-iznext).lt.
     &		  abs(p_coord(3,ipp)-iznext)) then
		ipt=ipb
		ibefo=ibefo-1
	      else
		ipt=ipp
		ipast=ipast+1
	      endif
	    endif
	    mfit=mfit+1
	    xx(mfit)=p_coord(indx,ipt)
	    yy(mfit)=p_coord(indy,ipt)
	    zz(mfit)=p_coord(3,ipt)
	  enddo
	endif
c
	if(mfit.lt.minfit)then
	  xsum=0.
	  ysum=0.
	  do i=1,mfit
	    xsum=xsum+xx(i)
	    ysum=ysum+yy(i)
	  enddo
	  xnext=xsum/mfit
	  ynext=ysum/mfit
	else
	  call lsfit(zz,xx,mfit,slope,bint,ro)
	  xnext=iznext*slope+bint
	  if(iaxtilt.eq.0.or.abs(tilt(iznext+1)).lt.tiltmin)then 
	    call lsfit(zz,yy,mfit,slope,bint,ro)
	    ynext=iznext*slope+bint
	  else
	    do i=1,mfit
	      theta=tilt(nint(zz(i))+1)
	      xr(1,i)=cosd(theta)
	      xr(2,i)=sind(theta)
	      xr(3,i)=yy(i)
	    enddo
	    call multr(xr,3,mfit,sx,ss,ssd,d,r,xm,sd,b,b1,
     &		const, rsq ,fra)
	    thetnext=tilt(iznext+1)
	    ynext=b1(1)*cosd(thetnext)+b1(2)*sind(thetnext)+const
	  endif
	endif
	if(iaxtilt.gt.1)then
	  xtmp=xnext
	  xnext=ynext
	  ynext=xtmp
	endif
	return
	end


	


c	  find_PIECE takes a list of NPCLIST piece coordinates in
c	  I[XYZ]PCLIST, the piece dimensions NX and NY, and index coordinates
c	  in the montaged image IND[XYZ], and finds the piece that those
c	  coordinates are in (IPCZ) and the coordinates IPCX, IPCY of the point
c	  in that piece
c
	subroutine find_piece(ixpclist,iypclist,izpclist, npclist,nx,
     &	    ny,nxbox,nybox,xnext,ynext,iznext,ix0,ix1,iy0,iy1,ipcz)
c
	integer*4 ixpclist(*),iypclist(*),izpclist(*)
c
	indx0=nint(xnext)-nxbox/2
	indx1=indx0+nxbox-1
	indy0=nint(ynext)-nybox/2
	indy1=indy0+nybox-1
	ipcz=-1
	do ipc=1,npclist
	  if(iznext.eq.izpclist(ipc).and.
     &	      indx0.ge.ixpclist(ipc).and.indx1.lt.ixpclist(ipc)+nx .and.
     &	      indy0.ge.iypclist(ipc).and.indy1.lt.iypclist(ipc)+ny)then
	    ipcz=ipc-1
	    ix0=indx0-ixpclist(ipc)
	    ix1=indx1-ixpclist(ipc)
	    iy0=indy0-iypclist(ipc)
	    iy1=indy1-iypclist(ipc)
	    return
	  endif
	enddo
	return
	end




	SUBROUTINE QDshift(ARRAY,BRAY,NX,NY,xt,yt)
	DIMENSION ARRAY(nx,ny),BRAY(nx,ny)
C
C Loop over output image
C	  
	dx=-xt
	dy=-yt
	dxsq=dx**2
	dysq=dy**2
	DO IYP = 1,NY
	  DO IXP = 1,NX
C
C   Do quadratic interpolation
C
	    IXPP1 = IXP + 1
	    IXPM1 = IXP - 1
	    IYPP1 = IYP + 1
	    IYPM1 = IYP - 1
	    IF (IXPM1 .LT. 1) IXPM1 = 1
	    IF (IYPM1 .LT. 1) IYPM1 = 1
	    IF (IXPP1 .GT. NX) IXPP1 = NX
	    IF (IYPP1 .GT. NY) IYPP1 = NY
C
C	Set up terms for quadratic interpolation
C
	    V2 = ARRAY(IXP, IYPM1)
	    V4 = ARRAY(IXPM1, IYP)
	    V5 = ARRAY(IXP, IYP)
	    V6 = ARRAY(IXPP1, IYP)
	    V8 = ARRAY(IXP, IYPP1)
C
	    A = (V6 + V4)*.5 - V5
	    B = (V8 + V2)*.5 - V5
	    C = (V6 - V4)*.5
	    D = (V8 - V2)*.5
C
	    
            bray(ixp,iyp)=(A*DXsq + B*DYsq + C*DX + D*DY + V5)
C
	  enddo
	enddo
C
	RETURN
	END



c	  PADFILL pads an image, dimensions NXBOX by NYBOX in ARRAY,
c	  into the center of a larger array.  The padded image in BRRAY will
c	  have size NX by NY while the BRRAY will be dimensioned NXDIM by NY
c	  The values of the image at its edge will be tapered out to the mean
c	  value at the edge of the new area.
c
	subroutine padfill(array,nxbox,nybox,brray,nxdim,nx,ny)
	real*4 array(nxbox,nybox),brray(nxdim,ny)
c
	sum=0.
	do ix=1,nxbox
	  sum=sum+array(ix,1)+array(ix,nybox)
	enddo
	do iy=2,nybox-1
	  sum=sum+array(1,iy)+array(nxbox,iy)
	enddo
	dmean=sum/(2*(nxbox+nybox-2))
c
	ixlo=nx/2-nxbox/2
	ixhi=ixlo+nxbox
	iylo=ny/2-nybox/2
	iyhi=iylo+nybox
	do iy=nybox,1,-1
	  do ix=nxbox,1,-1
	    brray(ix+ixlo,iy+iylo)=array(ix,iy)
	  enddo
	enddo
	if(nxbox.ne.nx.or.nybox.ne.ny)then
	  do iy=iylo+1,iyhi
	    edgel=brray(ixlo+1,iy)
	    edger=brray(ixhi,iy)
	    do ix=1,ixlo
	      wedge=(ix-1.)/ixlo
	      wmean=1.-wedge
	      brray(ix,iy)=wmean*dmean+wedge*edgel
	      brray(nx+1-ix,iy)=wmean*dmean+wedge*edger
	    enddo
	  enddo
	  do iy=1,iylo
	    wedge=(iy-1.)/iylo
	    prodmean=(1.-wedge)*dmean
	    iytoplin=ny+1-iy
	    do ix=1,nx
	      brray(ix,iy)=prodmean+wedge*brray(ix,iylo+1)
	      brray(ix,iytoplin)=prodmean+wedge*brray(ix,iyhi)
	    enddo
	  enddo
	endif
	return
	end


	subroutine setmeanzero(array,nxdim,nydim,nx,ny)
	real*4 array(nxdim,nydim)
	sum=0.
	do iy=1,ny
	  sumt=0.
	  do ix=1,nx
	    sumt=sumt+array(ix,iy)
	  enddo
	  sum=sum+sumt
	enddo
	avg=sum/(nx*ny)
	do iy=1,ny
	  do ix=1,nx
	    array(ix,iy)=array(ix,iy)-avg
	  enddo
	enddo
	return
	end


c	  PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c	  in the array ARRAY, which is dimensioned to nx+2 by ny.
c
	subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,peak)
	real*4 array(nxplus,nyrot)
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
c	  return adjusted pixel coordinate minus 1
c
	xpeak=ixpeak-1.
	ypeak=iypeak-1.
	if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
	if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
c	print *,xpeak,ypeak
	return
	end



	subroutine calcg(boxtmp,nxbox,nybox,xpeak,ypeak,idxin,
     &	    idyin,ninside,idxedge,idyedge,nedge,ipolar,wsum)
	real*4 boxtmp(nxbox,nybox)
	integer*4 idxin(*),idyin(*),idxedge(*),idyedge(*)
c
	ixcen=nxbox/2+nint(xpeak)
	iycen=nybox/2+nint(ypeak)
c	  
c	  look around, find most extreme 4 points as center
c	  
	if(ixcen.ge.2.and.ixcen.le.nxbox-2.and.
     &	    iycen.ge.2.and.iycen.le.nybox-2)then
	  best=0.
	  do iy=iycen-1,iycen+1
	    do ix=ixcen-1,ixcen+1
	      sum4=(boxtmp(ix,iy)+boxtmp(ix+1,iy)+
     &		  boxtmp(ix,iy+1)+ boxtmp(ix+1,iy+1))
	      if(ipolar*sum4.gt.ipolar*best.or.best.eq.0.)then
		ixbest=ix
		iybest=iy
		best=sum4
	      endif
	    enddo
	  enddo
	  ixcen=ixbest
	  iycen=iybest
	endif
	sum=0.
	nsum=0
	xsum=0.
	ysum=0.
	wsum=0.
c	  
c	  find edge mean
c
	do i=1,nedge
	  ix=ixcen+idxedge(i)
	  iy=iycen+idyedge(i)
	  if(ix.ge.1.and.ix.le.nxbox.and.iy.ge.1.and.iy.le.nybox)then
	    sum=sum+boxtmp(ix,iycen+idyedge(i))
	    nsum=nsum+1
	  endif
	enddo
	if(nsum.eq.0)return
	edge=sum/nsum
c	  
c	  subtract edge and get weighted sum of pixel coordinates for POSITIVE
c	  pixels
c
	do i=1,ninside
	  ix=ixcen+idxin(i)
	  iy=iycen+idyin(i)
	  if(ix.ge.1.and.ix.le.nxbox.and.iy.ge.1.and.iy.le.nybox)then
	    weight=boxtmp(ix,iy)-edge
	    if(ipolar*weight.gt.0.)then
	      xsum=xsum+ix*weight
	      ysum=ysum+iy*weight
	      wsum=wsum+weight
	    endif
	  endif
	enddo
	if(wsum.eq.0.)return
	xpeak=xsum/wsum-0.5-nxbox/2
	ypeak=ysum/wsum-0.5-nybox/2
	wsum=wsum*ipolar
	return
	end


	subroutine rescue(boxtmp,nxbox,nybox,xpeak,ypeak, idxin, idyin,
     &	    ninside,idxedge,idyedge, nedge,ipolar,radmax,relaxcrit,wsum)
	real*4 boxtmp(*)
	integer*4 idxin(*),idyin(*),idxedge(*),idyedge(*)
	wsum=0.
	rad=0.
	radinc=1.5
	do while(rad.le.radmax .and.wsum.eq.0.)
	  radsq=rad**2
	  radosq=min(rad+radinc,radmax)**2
	  wbest=0.
	  do idy=-nybox/2,nybox/2
	    do idx=-nxbox/2,nxbox/2
	      distsq=idx**2+idy**2
	      if(distsq.gt.radsq.and. distsq.le.radosq)then
		xtmp=idx
		ytmp=idy
		call calcg(boxtmp,nxbox,nybox,xtmp, ytmp, idxin, idyin,
     &		    ninside, idxedge,idyedge, nedge,ipolar, wtmp)
		if(wtmp.gt.wbest)then
		  wbest=wtmp
		  xpeak=xtmp
		  ypeak=ytmp
		endif
	      endif
	    enddo
	  enddo
	  rad=rad+radinc
	  if(wbest.ge. relaxcrit)wsum=wbest
	enddo
	return
	end



	subroutine add_point(iobj,ipnear, xpos,ypos,iznext)
	include 'model.inc'
	logical failed
c
	call object_mover(iobj,failed)
	if(failed)call errorexit('insufficient object space',0)
	ibase=ibase_obj(iobj)
	n_point=n_point+1
	p_coord(1,n_point)=xpos
	p_coord(2,n_point)=ypos
	p_coord(3,n_point)=iznext
	pt_label(n_point)=0
	npt_in_obj(iobj)=npt_in_obj(iobj)+1
	ipadd=ipnear
	if(nint(p_coord(3,object(ibase+ipadd))).lt.iznext)ipadd=ipadd+1
	do j=ibase+npt_in_obj(iobj),ibase+ipadd+1,-1
	  object(j)=object(j-1)
	enddo
	object(ibase+ipadd)=n_point
	ibase_free=ibase_free+1
	ipnear=ipadd
	return
	end


c	  READRAWSCALE takes information about the desired image box ix0,ix1,
c	  iy0,iy1 from image file of dimensions nx,ny, and a size nxraw,nyraw
c	  of a larger box to be used for local scaling, and the target mean
c	  and sd in targavg and targsd.  It reads data into the array rawtmp,
c	  scales them, and places the desired box of data into boxtmp

	subroutine readrawscale(rawtmp,boxtmp,nx,ny,nxraw,nyraw,
     &	    nxbox,nybox,ix0,ix1,iy0,iy1,targavg,targsd)
	real*4 rawtmp(*),boxtmp(*)
c	  
	ixr0=max(0,ix0-(nxraw-nxbox)/2)
	ixr1=min(nx-1,ixr0+nxraw-1)
	ixr0=max(0,ixr1+1-nxraw)
	nxread=ixr1+1-ixr0
	iyr0=max(0,iy0-(nyraw-nybox)/2)
	iyr1=min(ny-1,iyr0+nyraw-1)
	iyr0=max(0,iyr1+1-nyraw)
	nyread=iyr1+1-iyr0
	call irdpas(1,rawtmp,nxread,nyread,ixr0,ixr1,iyr0,iyr1,*99)
	call avgsd(rawtmp,nxread*nyread,avg,sd,sem)
	call irepak(boxtmp,rawtmp,nxread,nyread,ix0-ixr0,ix1-ixr0,
     &	    iy0-iyr0,iy1-iyr0)
c
c	  Ad hoc protection against zero, should be improved
c
	if(sd.lt.0.0001)sd = 0.0001
	dscal=targsd/sd
	dadd=targavg-avg*dscal
	do i=1,nxbox*nybox
	  boxtmp(i)=boxtmp(i)*dscal+dadd
	enddo
	return
99	print *,'ERROR READING IMAGE FROM FILE'
	call exit(1)
	end

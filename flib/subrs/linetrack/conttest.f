	subroutine linetrack(array,nx,ny,p_coord,ninobj,iptcur,maxlen,
     &	    inksize,inkern,sigma,h,ifdark,step,redtol,ifreplace,offset,
     &	    ifclose,ifcopy,iffail)
	parameter (limkern=40,limksize=21,limpath=1000)
	parameter (maxtry=2)
	byte array(nx,ny)
	real*4 p_coord(3,maxlen)
c
	real*4 orig(3),bkern(limkern*limksize**2)
	real*4 xpath(limpath,2),ypath(limpath,2)
	real*4 xdum(limpath),ydum(limpath)
	integer*4 idum(limpath),jdum(limpath)
	data orig/0.,0.,0./
C   
	real*4 theta(limkern)
	real*4 xstnew(2),ystnew(2),dely(2),tbestnew(2)
	integer*4 ixofs(limkern),iyofs(limkern),kbase(limkern)
	integer*4 ninpath(2)
	real*4 tolinitscantry(maxtry)/55.,20./
	real*4 endtoltry(maxtry)/1.,1.5/
	real*4 tolpathangtry(maxtry)/15.,10./
	integer*4 ndxlimtry(maxtry)/2.,1./
	data ksizsav/0/
	data izmean/-10000/
	save bkern,dmean,izmean
	save ksizsav,sigsav,hsav,nksav,ifdsav,kbase,theta,ixofs,iyofs
C   
	nkern=min(inkern,limkern)
	ksize=min(2*inksize+1,limksize)
C	  
	tolmeet=3.*step
	tolangoff=75.
	thetinc=180./nkern
	tolcopyang=max(10.,1.1*thetinc)
c
c	write(*,'(3f10.3)')((p_coord(i,j),i=1,3),j=1,ninobj)
	if(ksizsav.ne.ksize.or.sigsav.ne.sigma.or.hsav.ne.h.or.
     &	    nksav.ne.nkern.or.ifdsav.ne.ifdark)then
	  call makekern(bkern,ksize,ksize,sigma,h,nkern,ifdark,theta,
     &	      ixofs,iyofs)
	  do k=1,nkern
	    kbase(k)=1+ksize**2*(k-1)
	  enddo
	  ksizsav=ksize
	  sigsav=sigma
	  hsav=h
	  nksav=nkern
	  ifdsav=ifdark
c	  type *,'made kernels'
	endif
c	type *,nx,ny,ninobj,iptcur,maxlen,ksize,nkern,sigma,h,ifdark,step
c
c	  
c	  if ifcopy is non-zero, try to adjust points all around contour
c	  
	if(ifcopy.gt.0)then
	  zmodel=p_coord(3,1)
	  izmodel=nint(zmodel)
	  if(izmean.ne.izmodel)then
	    call edgemean(array,nx,ny,dmean)
	    izmean=izmodel
	  endif
	  xlas=p_coord(1,ninobj)
	  ylas=p_coord(2,ninobj)
	  xnex=p_coord(1,1)
	  ynex=p_coord(2,1)
	  do ipt=1,ninobj
	    if(ipt.lt.ninobj)then
	      tang=atan2d(p_coord(2,ipt+1)-ylas,p_coord(1,ipt+1)-xlas)
	    else
	      tang=atan2d(ynex-ylas,xnex-xlas)
	    endif
	    xcen=p_coord(1,ipt)+orig(1)+0.5
	    ycen=p_coord(2,ipt)+orig(2)+0.5
	    if(offset.ne.0)then
	      sints=sind(tang)
	      costs=cosd(tang)
	      xcen=xcen-sints*offset
	      ycen=ycen-costs*offset
	    endif
	    ixc=nint(xcen)
	    iyc=nint(ycen)
	    xnd=ixc
	    ynd=iyc
	    call findangpos(array,nx,ny,bkern,kbase,ksize,nkern,ixofs,
     &		iyofs,dmean,thetinc,tolcopyang,ifcopy,xnd,ynd,ixc,iyc,
     &		tang,cxnew,cynew)
	    if(offset.ne.0)then
	      cxnew=cxnew+sints*offset
	      cynew=cynew+costs*offset
	    endif
	    xlas=p_coord(1,ipt)
	    ylas=p_coord(2,ipt)
	    p_coord(1,ipt)=cxnew-orig(1)-0.5
	    p_coord(2,ipt)=cynew-orig(2)-0.5
	  enddo
	  iffail=0
	  return
	endif
c	  
c	  if close function selected, replicate first point as last point
c	  
	if(ifclose)then
	  distclose=sqrt((p_coord(1,1)-p_coord(1,ninobj))**2+
     &	      (p_coord(1,1)-p_coord(1,ninobj))**2)
	  iffail=0
	  if(distclose.lt.1.)return
	  f=max(0.5,(distclose-1.)/distclose)
	  iptcur=ninobj
	  ninobj=ninobj+1
	  p_coord(1,ninobj)=f*p_coord(1,1)+(1.-f)*p_coord(1,iptcur)
	  p_coord(2,ninobj)=f*p_coord(2,1)+(1.-f)*p_coord(2,iptcur)
	  p_coord(3,ninobj)=p_coord(3,iptcur)
	endif
c
	ipst=iptcur
	ipnd=iptcur+1
	if(ipst.le.0.or.ipnd.gt.ninobj)return
	zmodel=p_coord(3,ipnd)
	izmodel=nint(zmodel)
	if(izmean.ne.izmodel)then
	  call edgemean(array,nx,ny,dmean)
	  izmean=izmodel
c	  type *,'Mean =',dmean,izmean,izmodel
	endif
c
	ifmeet=0
	itry=1
	do while(ifmeet.eq.0.and.itry.le.maxtry)
	  endtol=endtoltry(itry)
	  tolinitscan=tolinitscantry(itry)
	  tolpathang=tolpathangtry(itry)
	  xst=p_coord(1,ipst)+orig(1)+0.5
	  yst=p_coord(2,ipst)+orig(2)+0.5
	  xnd=p_coord(1,ipnd)+orig(1)+0.5
	  ynd=p_coord(2,ipnd)+orig(2)+0.5
	  if(offset.ne.0.)then
	    tstnd=atan2d(ynd-yst,xnd-xst)
	    sints=sind(tstnd)
	    costs=cosd(tstnd)
	    xst=xst-sints*offset
	    yst=yst-costs*offset
	    xnd=xnd-sints*offset
	    ynd=ynd-costs*offset
	  endif
c	call typearray(array,nx,ny,nint(xst),nint(yst))
c	  type *,'segment',xst,yst,xnd,ynd
	  iffail=0
	  if(sqrt((xst-xnd)**2+(yst-ynd)**2).le.step)return
	  iffail=1
c	      
	  do ifback=1,2
	    ixc=nint(xst)
	    iyc=nint(yst)
	    tstnd=atan2d(ynd-yst,xnd-xst)
c	      
c	      scan for best overall direction from this point, limit
c	      range of search by tolinitscan
c	      
	    ndxlim=1
	    if(ifreplace.lt.0)ndxlim=3
	    best=-1.e10
	    do k=1,nkern
	      diffang=min(abs(goodangle(theta(k)-tstnd)),
     &		  abs(goodangle(theta(k)-180.-tstnd)))
	      if(diffang.lt.tolinitscan)then
		call kernprod(array,nx,ny,ixc,iyc,bkern(kbase(k)),
     &		    ksize, ksize,ixofs(k), iyofs(k),dmean,prod)
		if(prod.gt.best)then
		  best=prod
		  tbest=theta(k)
		endif
	      endif
	    enddo
	    tdiff=abs(goodangle(tstnd-tbest))
c	      
c	      if the difference in angle is greater than 90, the
c	      direction needs to be flipped
c	      
	    if(tdiff.gt.90.)tbest=tbest-180.
	    call findangpos(array,nx,ny,bkern,kbase,ksize,nkern,ixofs,
     &		iyofs,dmean,thetinc,tolpathang,ndxlim,xnd,ynd,ixc,iyc,
     &		tbest,cxnew,cynew)
c
	    xstnew(ifback)=cxnew
	    ystnew(ifback)=cynew
	    tbestnew(ifback)=tbest
	    dely(ifback)=-sind(tbest)*(xst-cxnew)+
     &		cosd(tbest)*(yst-cynew)
	    xtmp=xst
	    xst=xnd
	    xnd=xtmp
	    ytmp=yst
	    yst=ynd
	    ynd=ytmp
	  enddo
	  delyavg=0.5*(dely(1)-dely(2))
c	  type *,delyavg,dely(1),dely(2)
	  dely(1)=delyavg
	  dely(2)=-delyavg
c	  type *,delyavg,dely(1),dely(2)
c
	  ifback=1
	  ifterm=0
	  ndxlim=ndxlimtry(itry)
	  endtolsq=endtol**2
	  do while (ifback.le.2.and.ifterm.le.0)
c	      
c	      get array index coord of starting point
c	      
	    ninpath(ifback)=0
	    ifterm=0
c	      
c	      come into here with tbest, ixc,iyc and do search along
c	      perpendicular and at neighboring angles
c	      
	    iffirst=1
	    do while(ifterm.eq.0)
	      if(iffirst.eq.0)then
		call findangpos(array,nx,ny,bkern,kbase,ksize,nkern,ixofs,
     &		    iyofs,dmean,thetinc,tolpathang,ndxlim,xnd,ynd,ixc,iyc,
     &		    tbest,cxnew,cynew)
	      else
		cxnew=xstnew(ifback)
		cynew=ystnew(ifback)
		tbest=tbestnew(ifback)
		xnd=xstnew(3-ifback)
		ynd=ystnew(3-ifback)
	      endif
	      xmodel=cxnew -orig(1)-0.5
	      ymodel=cynew -orig(2)-0.5
	      if(ifreplace.lt.0)then
		xmodel=xmodel+sind(tbest)*dely(ifback)
		ymodel=ymodel+cosd(tbest)*dely(ifback)
	      endif
	      if(offset.ne.0)then
		xmodel=xmodel+sind(tbest)*offset*(3-2*ifback)
		ymodel=ymodel+cosd(tbest)*offset*(3-2*ifback)
	      endif
c		
c		is it at end?  If so decide whether to save or not
c		
	      ifsave=1
	      if(iffirst.eq.0)then
		call point_to_line(xnd,ynd,cxlast,cylast,cxnew,
     &		    cynew, tmin,distsq)
		if(distsq.lt.endtolsq)then
c		    ifterm=1 was unconditional, but try making it go past
c		    before terminating, then replacing endpoint with
c		    closest point on the line
c		  if(tmin.lt.1.)then
		    ifterm=1
c		    xstnew(3-ifback)=cxlast+tmin*(cxnew-cxlast)
c		    ystnew(3-ifback)=cylast+tmin*(cynew-cylast)
c		  endif
		  ifsave=0
		  if(tmin.eq.1.)then
		    distlast=sqrt((xnd-cxlast)**2+(ynd-cylast)**2)
		    if(distlast.gt.1.33*step)ifsave=1
		  endif
		endif
	      endif
	      iffirst=0
	      cxlast=cxnew
	      cylast=cynew
	      if(ifsave.ne.0)then
		ninpath(ifback)=ninpath(ifback)+1
		xpath(ninpath(ifback),ifback)=xmodel
		ypath(ninpath(ifback),ifback)=ymodel
	      endif
	      if(ifterm.eq.0)then
		tcurnd=atan2d(ynd-cynew,xnd-cxnew)
		if(abs(goodangle(tcurnd-tbest)).gt.tolangoff)then
		  ifterm=-1
		else
c		    
c		    set up new position, turn around if too close to edge
c		    
		  ixc=nint(cxnew+step*cosd(tbest))
		  iyc=nint(cynew+step*sind(tbest))
		  if(ixc.lt.ksize/2.or.ixc.gt.nx-ksize/2.or.
     &		      iyc.lt.ksize/2.or.iyc.gt.ny-ksize/2.or.
     &		      ninpath(ifback).ge.limpath-1) ifterm=-1
		endif
		if(ifterm.eq.-1) ifback=ifback+1
	      endif
	    enddo
	  enddo
c	    
c	    if went through both paths without success, need to
c	    try for a composite path
c	    
c	  type *,'end of paths',ifback,ninpath(1),ifterm
	  if(ifback.eq.3)then
	    ipseg=1
	    ifmeet=0
	    do while (ipseg.lt.ninpath(1).and.ifmeet.eq.0)
	      x2=xpath(ipseg,1)
	      y2=ypath(ipseg,1)
	      x1=xpath(ipseg+1,1)
	      y1=ypath(ipseg+1,1)
	      dxseg=x2-x1
	      dyseg=x2-x1
	      segsq=(x2-x1)**2+(y2-y1)**2
	      ip2=0
	      do while(ip2.lt.ninpath(2).and.ifmeet.eq.0)
		ip2=ip2+1
		x0=xpath(ip2,2)
		y0=ypath(ip2,2)
		dxp=x1-x0
		dyp=y1-y0
		if(abs(dxp).lt.tolmeet.and.abs(dyp).lt.tolmeet)
     &		    then
		  tmin=-(dxp*dxseg+dyp*dyseg)/segsq
		  tmin=max(0.,min(1.,tmin))
		  distsq=(dxp+tmin*dxseg)**2+(dyp+tmin*dyseg)**2
		  if(distsq.lt.endtolsq)then
		    ifmeet=1
		    ip1=ipseg
		    if(tmin.eq.0.)then
		      distlast=sqrt((x2-x0)**2+(y-y0)**2)
		      if(distlast.gt.1.33*step)ip1=ipseg+1
		    endif
		  endif
		endif
	      enddo
	      ipseg=ipseg+1
	    enddo
c	      
	  elseif(ifback.eq.1)then
	    ip1=ninpath(1)+1
	    xpath(ip1,1)=xstnew(2)-sind(tbestnew(2))*offset
	    ypath(ip1,1)=ystnew(2)-cosd(tbestnew(2))*offset
	    ip2=0
	  else
	    ip1=1
	    xpath(1,1)=xstnew(1)+sind(tbestnew(1))*offset
	    ypath(1,1)=ystnew(1)+cosd(tbestnew(1))*offset
	    ip2=ninpath(2)
	  endif
c	    
	  if(ifback.lt.3.or.ifmeet.eq.1)then
	    ifmeet=1
	    if(ip2+ip1.gt.limpath)return
	    do jp=ip2,1,-1
	      ip1=ip1+1
	      xpath(ip1,1)=xpath(jp,2)
	      ypath(ip1,1)=ypath(jp,2)
	    enddo
c
	    if(ifreplace.le.0)then
	      xpath(ip1,1)=p_coord(1,ipnd)
	      ypath(ip1,1)=p_coord(2,ipnd)
	      xpath(1,1)=p_coord(1,ipst)
	      ypath(1,1)=p_coord(2,ipst)
	    endif
c	      
c	      now could reduce the path array
c
	    call reducepts(xpath,ypath,ip1,2,5,redtol,xpath(1,2),
     &		ypath(1,2),xdum,ydum,idum,jdum)
	    if(maxlen.lt.ninobj+ip1-2)return
c	      
c	      shift points from current one up
c
	    ishft=ip1-2
c	    type * ,ishft,' points being added'
	    do ip=ninobj,ipnd,-1
	      ipnew=ip+ishft
	      p_coord(1,ipnew)=p_coord(1,ip)
	      p_coord(2,ipnew)=p_coord(2,ip)
	      p_coord(3,ipnew)=p_coord(3,ip)
	    enddo
c	      
c	      save points in open spot, replacing ends potentially
c	      
	    do ip=1,ip1
	      ipnew=ip+ipst-1
	      p_coord(1,ipnew)=xpath(ip,1)
	      p_coord(2,ipnew)=ypath(ip,1)
	      p_coord(3,ipnew)=zmodel
	    enddo
	    ninobj=ninobj+ishft
	    iptcur=iptcur+ishft
	  endif
	  itry=itry+1
	enddo
	iffail=1-ifmeet
	return
	end

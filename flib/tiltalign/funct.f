c****	  FUNCT performs the necessary tasks required by the METRO routine for
c	  conjugate gradient minimization.  For the current values of the
c	  rotation, tilt, mag and compression (which are obtained, whether
c	  they are fixed values or variables, by calling the routine
c	  REMAP_PARAMS), and the current values of the real-space (x,y,z)
c	  coordinates of the points (which are kept in the list VAR, after the
c	  "geometric" variables on that list), it computes the projection
c	  coordinates of each point, the best displacement dx, dy, for each
c	  view; the residuals for each point (computed minus measured
c	  projection coordinate), and the error term, which is the sum of
c	  the squares of the residuals.  It then computes the derivative of
c	  the error with respect to each variable and returns these derivatives
c	  in the array GRAD.  The derivatives with respect to the geometric
c	  variables are obtained from the following relations:
c	  _    xproj = a*x + b*y + c*z + dx(view)
c	  _    xproj = d*x + e*y + f*z + dy(view)
c	  where a = mag * cos (tilt) * cos (rot)
c	  _     b = - mag * sin (rot)
c	  _     c = mag * comp * sin (tilt) * cos (rot)
c	  where d = mag * cos (tilt) * sin (rot)
c	  _     e = mag * cos (rot)
c	  _     f = mag * comp * sin (tilt) * sin (rot)
c	  
c	  The derivatives with respect to the coordinate variables are obtained
c	  from expressions relating the projection coordinates to the real-
c	  space (x,y,z) coordinates of all but the last point.  These
c	  expressions incorporate the constraint that the centroid of the
c	  (x,y,z) points is (0,0,0) and the fact that the total error will be a
c	  minimum when the centroid of the measured projection points in one
c	  view matches the centroid of the projection of the real-space
c	  points.  ANGLES ARE EXPECTED TO BE RADIANS.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine funct(nvarsrch,var,ferror,grad)
c	
	include 'alivar.inc'
	parameter (ms=maxview)
c
	real*4 grad(*),var(*)
c
	double precision error
	logical*1 realinview(maxprojpt)
c
	real*4 xbar(ms),ybar(ms),xproj(maxprojpt),yproj(maxprojpt)
	real*4 xcen(ms),ycen(ms),zcen(ms)
c
c	  a, b, etc are the quantities in the above equations for each view
c	  aon is a over n (# of points in that view)
c	  aprime is derivative of a with respect to tilt angle
c
	real*4 a(ms),b(ms),c(ms),d(ms),e(ms),f(ms)
	real*4 aon(ms),bon(ms),con(ms),don(ms),eon(ms),fon(ms)
	real*4 cthet(ms),sthet(ms),calf(ms),salf(ms)
	real*4 cphi(ms),sphi(ms),cdel(ms),sdel(ms),xmag(ms)
	real*4 aprime(ms),cprime(ms),dprime(ms),fprime(ms)
	real*4 dxddel(ms),dyddel(ms)
c
	integer*2 indvreal(maxprojpt)
	integer*4 nptinview(maxview),indvproj(maxprojpt)
	real*4 coefx(3*maxreal),coefy(3*maxreal)
	save xbar,ybar,nptinview,realinview,indvproj,indvreal
c	  
	logical firsttime,xyzfixed
	common /functfirst/ firsttime,xyzfixed
c	  
c	  first time in, precompute the mean projection coords in each view
c	  and build indexes to the points in each view.
c	  
	nprojpt=irealstr(nrealpt+1)-1
	if(firsttime)then
	  if(nrealpt*nview.gt.maxprojpt)call errorexit(
     &	      'TOO MANY 3-D POINTS AND VIEWS FOR ARRAYS IN FUNCT', 0)
	  do iv=1,nview
	    xbar(iv)=0.
	    ybar(iv)=0.
	    nptinview(iv)=0
	  enddo
c
	  do jpt=1,nrealpt
	    do iv=1,nview
	      realinview(jpt+(iv-1)*nrealpt)=.false.
	    enddo
	    do i=irealstr(jpt),irealstr(jpt+1)-1
	      iv=isecview(i)
	      ivbase=(iv-1)*nrealpt
	      nptinview(iv)=nptinview(iv)+1
	      indvreal(ivbase+nptinview(iv))=jpt
	      indvproj(ivbase+nptinview(iv))=i
	      realinview(jpt+ivbase)=.true.
	      xbar(iv)=xbar(iv)+xx(i)
	      ybar(iv)=ybar(iv)+yy(i)
	    enddo
	  enddo
c
	  do iv=1,nview
	    xbar(iv)=xbar(iv)/nptinview(iv)
	    ybar(iv)=ybar(iv)/nptinview(iv)
	  enddo
	  firsttime=.false.
	endif
c	  
c	  precompute the a-f and items related to them
c
	call remap_params(var)
c	  
	if(ifanyalf.eq.0)then
	  do i=1,nview
	    costhet=cos(tilt(i))
	    sinthet=sin(tilt(i))
	    csinthet=comp(i)*sinthet
	    ccosthet=comp(i)*costhet
	    cosphi=cos(rot(i))
	    sinphi=sin(rot(i))
	    gcosphi=gmag(i)*cosphi
	    gsinphi=gmag(i)*sinphi
	    xmag(i)=gmag(i)+dmag(i)
	    xcosdel=xmag(i)*cos(skew(i))
	    xsindel=xmag(i)*sin(skew(i))
c	      
	    a(i)=xcosdel*costhet*cosphi-xsindel*sinphi
	    b(i)=-gsinphi
	    c(i)=csinthet*gcosphi
	    d(i)=xcosdel*costhet*sinphi+xsindel*cosphi
	    e(i)=gcosphi
	    f(i)=csinthet*gsinphi
c	      
	    aprime(i)=-xcosdel*sinthet*cosphi
	    cprime(i)=ccosthet*gcosphi
	    dprime(i)=-xcosdel*sinthet*sinphi
	    fprime(i)=ccosthet*gsinphi
c	      
	    dxddel(i)=-xsindel*costhet*cosphi-xcosdel*sinphi
	    dyddel(i)=xcosdel*cosphi-xsindel*costhet*sinphi
	  enddo
	else
	  do i=1,nview
	    cthet(i)=cos(tilt(i))
	    sthet(i)=sin(tilt(i))
	    cphi(i)=cos(rot(i))
	    sphi(i)=sin(rot(i))
	    cdel(i)=cos(skew(i))
	    sdel(i)=sin(skew(i))
	    calf(i)=cos(alf(i))
	    salf(i)=sin(alf(i))
	    gcosphi=gmag(i)*cphi(i)
	    gsinphi=gmag(i)*sphi(i)
	    xmag(i)=gmag(i)+dmag(i)
c	      
	    ad1=cdel(i)*cthet(i)+sdel(i)*salf(i)*sthet(i)
	    ad2=sdel(i)*calf(i)
	    a(i)=xmag(i)*(ad1*cphi(i)-ad2*sphi(i))
	    d(i)=xmag(i)*(ad1*sphi(i)+ad2*cphi(i))
	    sast=salf(i)*sthet(i)
	    b(i)=sast*gcosphi-calf(i)*gsinphi
	    e(i)=sast*gsinphi+calf(i)*gcosphi
	    cast=calf(i)*sthet(i)
	    c(i)=comp(i)*(cast*gcosphi+salf(i)*gsinphi)
	    f(i)=comp(i)*(cast*gsinphi-salf(i)*gcosphi)
	  enddo
	endif
c
	do i=1,nview
	  aon(i)=-a(i)/nptinview(i)
	  bon(i)=-b(i)/nptinview(i)
	  con(i)=-c(i)/nptinview(i)
	  don(i)=-d(i)/nptinview(i)
	  eon(i)=-e(i)/nptinview(i)
	  fon(i)=-f(i)/nptinview(i)
	enddo
c	  
c	s0=secnds(0.)
	nvmat=3*(nrealpt-1)			!# of x,y,z variables
	icoordbas=nvarsrch-nvmat		!offset to x,y,z's
	kzlas=icoordbas+nrealpt*3		!indexes of x,y,z of last point
	kylas=kzlas-1
	kxlas=kylas-1
c	  
c	  get xproj and yproj: for now, these will be projected points minus
c	  the dx, dy values
c	  compute the coordinates of the last point: minus the sum of the rest
c
	do iv=1,nview
	  xcen(iv)=0.
	  ycen(iv)=0.
	  zcen(iv)=0.
	enddo
	var(kxlas)=0.
	var(kylas)=0.
	var(kzlas)=0.
c
	do jpt=1,nrealpt
	  kz=icoordbas+jpt*3
	  ky=kz-1
	  kx=ky-1
c
	  if(jpt.lt.nrealpt)then		!accumulate last point coords
	    var(kxlas)=var(kxlas)-var(kx)
	    var(kylas)=var(kylas)-var(ky)
	    var(kzlas)=var(kzlas)-var(kz)
	  endif
c	    
	  xyz(1,jpt)=var(kx)			!unpack the coordinates
	  xyz(2,jpt)=var(ky)
	  xyz(3,jpt)=var(kz)
c
	  do i=irealstr(jpt),irealstr(jpt+1)-1
	    iv=isecview(i)
	    xproj(i)=a(iv)*var(kx)+b(iv)*var(ky)+c(iv)*var(kz)
	    yproj(i)=d(iv)*var(kx)+e(iv)*var(ky)+f(iv)*var(kz)
	    xcen(iv)=xcen(iv)+var(kx)
	    ycen(iv)=ycen(iv)+var(ky)
	    zcen(iv)=zcen(iv)+var(kz)
	  enddo
	enddo
c	  
c	  get xcen, ycen, zcen scaled, and get the dx and dy
c	  
	do iv=1,nview
	  xcen(iv)=xcen(iv)/nptinview(iv)
	  ycen(iv)=ycen(iv)/nptinview(iv)
	  zcen(iv)=zcen(iv)/nptinview(iv)
	  dxy(1,iv)=xbar(iv)
     &	      -a(iv)*xcen(iv)-b(iv)*ycen(iv)-c(iv)*zcen(iv)
	  dxy(2,iv)=ybar(iv)
     &	      -d(iv)*xcen(iv)-e(iv)*ycen(iv)-f(iv)*zcen(iv)
	enddo
c	  
c	  adjust xproj&yproj by dxy, get residuals and errors
c	  
	error=0.
	do i=1,nprojpt
	  iv=isecview(i)
	  xproj(i)=xproj(i)+dxy(1,iv)
	  yproj(i)=yproj(i)+dxy(2,iv)
	  xresid(i)=xproj(i)-xx(i)
	  yresid(i)=yproj(i)-yy(i)
	  error=error + xresid(i)**2 + yresid(i)**2
	enddo
	ferror=error
c	write(*,'(f25.15)')error
c	  
c	  compute derivatives of error w/r to search parameters
c	  first clear out all the gradients
c
	do ivar=1,nvarsrch
	  grad(ivar)=0.
	enddo
c	  
c	  loop on views: consider each of 4 parameters
c	  
	ivar=0
	do iv=1,nview
	  ivbase=(iv-1)*nrealpt
c	    
c	    rotation: gradient for each view's angle adds to gradient for that
c	    variable and to gradient for variable 1, global rotation; unless
c	    rotation for one of the views is fixed (ifrotfix)
c	    
	  gradsum=0.
	  if(ifrotfix.lt.0)then
	    if(maprot(iv).gt.0)then
	      do iptinv=1,nptinview(iv)
		ipt=indvproj(ivbase+iptinv)
		gradsum=gradsum+2.*
     &		    ((ybar(iv)-yproj(ipt))*xresid(ipt)
     &		    +(xproj(ipt)-xbar(iv))*yresid(ipt))
	      enddo
	      grad(maprot(iv))=grad(maprot(iv))+frcrot(iv)*gradsum
	      if(linrot(iv).gt.0) grad(linrot(iv))=grad(linrot(iv))+
     &		  (1.-frcrot(iv))*gradsum
	    endif
	  elseif(iv.ne.ifrotfix)then
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      gradsum=gradsum+2.*
     &		  ((ybar(iv)-yproj(ipt))*xresid(ipt)
     &		  +(xproj(ipt)-xbar(iv))*yresid(ipt))
	    enddo
	    ivar=ivar+1
	    grad(ivar)=gradsum
	    if(iv.gt.1.and.ifrotfix.eq.0)grad(1)=grad(1)+gradsum
	  endif
c	    
c	    tilt: add gradient for this tilt angle to the variable it is mapped
c	    from, if any
c	    
	  if(maptilt(iv).ne.0.and.ifanyalf.eq.0)then
	    gradsum=0.
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*((aprime(iv)*(xyz(1,jpt)-xcen(iv))
     &		  +cprime(iv)*(xyz(3,jpt)-zcen(iv)))*xresid(ipt)
     &		  +(dprime(iv)*(xyz(1,jpt)-xcen(iv))
     &		  +fprime(iv)*(xyz(3,jpt)-zcen(iv)))*yresid(ipt))
	    enddo
	    grad(maptilt(iv))=grad(maptilt(iv))+frctilt(iv)*gradsum
	    if(lintilt(iv).gt.0) grad(lintilt(iv))=grad(lintilt(iv))+
     &		  (1.-frctilt(iv))*gradsum
c
	  elseif(maptilt(iv).ne.0)then
	    gradsum=0.
	    adfac=xmag(iv)*(sdel(iv)*salf(iv)*cthet(iv) -
     &		cdel(iv)*sthet(iv))
	    befac=gmag(iv)*salf(iv)*cthet(iv)
	    cffac=comp(iv)*gmag(iv)*calf(iv)*cthet(iv)
	    afac=adfac*cphi(iv)
	    dfac=adfac*sphi(iv)
	    bfac=befac*cphi(iv)
	    efac=befac*sphi(iv)
	    cfac=cffac*cphi(iv)
	    ffac=cffac*sphi(iv)
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*((afac*(xyz(1,jpt)-xcen(iv))+
     &		  bfac*(xyz(2,jpt)-ycen(iv))+cfac*(xyz(3,jpt)-zcen(iv)))
     &		  *xresid(ipt) +  (dfac*(xyz(1,jpt)-xcen(iv))+
     &		  efac*(xyz(2,jpt)-ycen(iv))+ffac*(xyz(3,jpt)-zcen(iv)))
     &		  *yresid(ipt))
	    enddo
	    grad(maptilt(iv))=grad(maptilt(iv))+frctilt(iv)*gradsum
	    if(lintilt(iv).gt.0) grad(lintilt(iv))=grad(lintilt(iv))+
     &		  (1.-frctilt(iv))*gradsum
	  endif
c	    
c	    mag: add gradient for this view to the variable it is mapped from
c	    
	  if(mapgmag(iv).gt.0)then
	    gradsum=0.
	    afac=a(iv)/xmag(iv)
	    bfac=b(iv)/gmag(iv)
	    cfac=c(iv)/gmag(iv)
	    dfac=d(iv)/xmag(iv)
	    efac=e(iv)/gmag(iv)
	    ffac=f(iv)/gmag(iv)
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*((afac*(xyz(1,jpt)-xcen(iv))+
     &		  bfac*(xyz(2,jpt)-ycen(iv))+cfac*(xyz(3,jpt)-zcen(iv)))
     &		  *xresid(ipt) +  (dfac*(xyz(1,jpt)-xcen(iv))+
     &		  efac*(xyz(2,jpt)-ycen(iv))+ffac*(xyz(3,jpt)-zcen(iv)))
     &		  *yresid(ipt))
	    enddo
	    grad(mapgmag(iv))=grad(mapgmag(iv))+frcgmag(iv)*gradsum
	    if(lingmag(iv).gt.0) grad(lingmag(iv))=grad(lingmag(iv))+
     &		  (1.-frcgmag(iv))*gradsum
	  endif
c	    
c	    comp: add gradient for this view to the variable it is mapped from
c
	  if(mapcomp(iv).gt.0)then
	    gradsum=0.
	    cfac=c(iv)/comp(iv)
	    ffac=f(iv)/comp(iv)
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*
     &		  (cfac*(xyz(3,jpt)-zcen(iv))*xresid(ipt)
     &		  +ffac*(xyz(3,jpt)-zcen(iv))*yresid(ipt))
	    enddo
	    grad(mapcomp(iv))=grad(mapcomp(iv))+frccomp(iv)*gradsum
	    if(lincomp(iv).gt.0) grad(lincomp(iv))=grad(lincomp(iv))+
     &		  (1.-frccomp(iv))*gradsum
	  endif
c	    
c	    dmag: add gradient for this view to the variable it is mapped from
c	    
	  if(mapdmag(iv).gt.0)then
	    gradsum=0.
	    afac=a(iv)/xmag(iv)
	    dfac=d(iv)/xmag(iv)
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*(afac*(xyz(1,jpt)-xcen(iv))
     &		  *xresid(ipt) +  dfac*(xyz(1,jpt)-xcen(iv))
     &		  *yresid(ipt))
	    enddo
c	      
c	      if this parameter maps to the dummy dmag, then need to subtract
c	      the fraction times the gradient sum from gradient of every real
c	      variable
c
	    if(mapdmag(iv).eq.mapdumdmag.or.lindmag(iv).eq.mapdumdmag)
     &		then
	      if(mapdmag(iv).eq.mapdumdmag)then
		valadd=dumdmagfac*frcdmag(iv)*gradsum
		if(lindmag(iv).gt.0) grad(lindmag(iv))=
     &		    grad(lindmag(iv))+(1.-frcdmag(iv))*gradsum
	      else
		valadd=dumdmagfac*(1.-frcdmag(iv))*gradsum
		grad(mapdmag(iv))=grad(mapdmag(iv))+frcdmag(iv)*gradsum
	      endif
	      do jj=mapdmagstart,mapdumdmag-1
		grad(jj)=grad(jj)+valadd
	      enddo
	    else
	      grad(mapdmag(iv))=grad(mapdmag(iv))+frcdmag(iv)*gradsum
	      if(lindmag(iv).gt.0) grad(lindmag(iv))=grad(lindmag(iv))+
     &		  (1.-frcdmag(iv))*gradsum
	    endif
	  endif
c	    
c	    skew: add gradient for this view to the variable it is mapped from
c	    
	  if(mapskew(iv).gt.0.and.ifanyalf.eq.0)then
	    gradsum=0.
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*(dxddel(iv)*(xyz(1,jpt)-xcen(iv))
     &		  *xresid(ipt) +  dyddel(iv)*(xyz(1,jpt)-xcen(iv))
     &		  *yresid(ipt))
	    enddo
	    grad(mapskew(iv))=grad(mapskew(iv))+frcskew(iv)*gradsum
	    if(linskew(iv).gt.0) grad(linskew(iv))=grad(linskew(iv))+
     &		(1.-frcskew(iv))*gradsum
c
	  elseif(mapskew(iv).gt.0)then
	    gradsum=0.
	    fac1=cdel(iv)*salf(iv)*sthet(iv)-sdel(iv)*cthet(iv)
	    fac2=cdel(iv)*calf(iv)
	    afac=xmag(iv)*(fac1*cphi(iv)-fac2*sphi(iv))
	    dfac=xmag(iv)*(fac1*sphi(iv)+fac2*cphi(iv))
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*(afac*(xyz(1,jpt)-xcen(iv))
     &		  *xresid(ipt) +  dfac*(xyz(1,jpt)-xcen(iv))
     &		  *yresid(ipt))
	    enddo
	    grad(mapskew(iv))=grad(mapskew(iv))+frcskew(iv)*gradsum
	    if(linskew(iv).gt.0) grad(linskew(iv))=grad(linskew(iv))+
     &		(1.-frcskew(iv))*gradsum
	  endif
c	    
c	    alpha: add gradient for this view to the variable it is mapped from
c	    
	  if(mapalf(iv).gt.0)then
	    gradsum=0.
	    cast=calf(iv)*sthet(iv)
	    sast=salf(iv)*sthet(iv)
	    abfac=cast*cphi(iv)+salf(iv)*sphi(iv)
	    defac=cast*sphi(iv)-salf(iv)*cphi(iv)
	    afac=xmag(iv)*sdel(iv)*abfac
	    bfac=gmag(iv)*abfac
	    dfac=xmag(iv)*sdel(iv)*defac
	    efac=gmag(iv)*defac
	    cfac=-comp(iv)*gmag(iv)*(sast*cphi(iv)-calf(iv)*sphi(iv))
	    ffac=-comp(iv)*gmag(iv)*(sast*sphi(iv)+calf(iv)*cphi(iv))
	    do iptinv=1,nptinview(iv)
	      ipt=indvproj(ivbase+iptinv)
	      jpt=indvreal(ivbase+iptinv)
	      gradsum=gradsum+2.*((afac*(xyz(1,jpt)-xcen(iv))+
     &		  bfac*(xyz(2,jpt)-ycen(iv))+cfac*(xyz(3,jpt)-zcen(iv)))
     &		  *xresid(ipt) +  (dfac*(xyz(1,jpt)-xcen(iv))+
     &		  efac*(xyz(2,jpt)-ycen(iv))+ffac*(xyz(3,jpt)-zcen(iv)))
     &		  *yresid(ipt))
	    enddo
c	    write(*,'(3i4,f7.4,f16.10)')iv,mapalf(iv),linalf(iv)
c     &		,frcalf(iv),gradsum
	    grad(mapalf(iv))=grad(mapalf(iv))+frcalf(iv)*gradsum
	    if(linalf(iv).gt.0) grad(linalf(iv))=grad(linalf(iv))+
     &		(1.-frcalf(iv))*gradsum
	  endif
	enddo
c	  
c	  loop on points, get derivatives w/r to x, y, or z
c
	if(xyzfixed)return
	do jpt=1,nrealpt
	  jz=3*jpt
	  jy=jz-1
	  jx=jy-1
	  iy=0
c
c	    for each projection of the real point, find how that point
c	    contributes to the derivative w/r to each of the x,y,z
c
	  do i=irealstr(jpt),irealstr(jpt+1)-1
	    iv=isecview(i)
	    ivbase=(iv-1)*nrealpt
	    iy=iy+2
	    ix=iy-1
c	      
c	      the relation between the projection (x,y) and the set of (x,y,z)
c	      contains the term dxy, which is actually a sum of the (x,y,z).
c	      There is a 3 by 3 matrix of possibilities: the first set of 3
c	      possibilities is whether this real point is the last one, or
c	      whether the last point is projected in this view or not.
c
	    if(jpt.eq.nrealpt)then
	      xpxrlas=-a(iv)
	      xpyrlas=-b(iv)
	      xpzrlas=-c(iv)
	      ypxrlas=-d(iv)
	      ypyrlas=-e(iv)
	      ypzrlas=-f(iv)
	    elseif(realinview(nrealpt+ivbase))then
	      xpxrlas=0.
	      xpyrlas=0.
	      xpzrlas=0.
	      ypxrlas=0.
	      ypyrlas=0.
	      ypzrlas=0.
	    else
	      xpxrlas=-aon(iv)
	      xpyrlas=-bon(iv)
	      xpzrlas=-con(iv)
	      ypxrlas=-don(iv)
	      ypyrlas=-eon(iv)
	      ypzrlas=-fon(iv)
	    endif
c
c	      The second set of three possibilities is whether the point whose
c	      coordinate that we are taking the derivative w/r to is the same
c	      as the real point whose projections are being considered
c	      (kpt.eq.jpt), and whether the former point is or is not projected
c	      in the view being considered.
c	      
	    do kpt=1,nrealpt-1
	      kz=3*kpt
	      ky=kz-1
	      kx=ky-1
c
	      if(kpt.eq.jpt)then
		coefx(kx)=a(iv)+xpxrlas
		coefx(ky)=b(iv)+xpyrlas
		coefx(kz)=c(iv)+xpzrlas
		coefy(kx)=d(iv)+ypxrlas
		coefy(ky)=e(iv)+ypyrlas
		coefy(kz)=f(iv)+ypzrlas
	      elseif(realinview(kpt+ivbase))then
		coefx(kx)=xpxrlas
		coefx(ky)=xpyrlas
		coefx(kz)=xpzrlas
		coefy(kx)=ypxrlas
		coefy(ky)=ypyrlas
		coefy(kz)=ypzrlas
	      else
		coefx(kx)=aon(iv)+xpxrlas
		coefx(ky)=bon(iv)+xpyrlas
		coefx(kz)=con(iv)+xpzrlas
		coefy(kx)=don(iv)+ypxrlas
		coefy(ky)=eon(iv)+ypyrlas
		coefy(kz)=fon(iv)+ypzrlas
	      endif
c
	    enddo
c	      
c	      The coefficients directly yield derivatives
c
	    do ivar=1,nvmat
	      grad(ivar+icoordbas)=grad(ivar+icoordbas)
     &		  +2.*xresid(i)*coefx(ivar)+2.*yresid(i)*coefy(ivar)
	    enddo
c
	  enddo
	enddo
c
c	write(*,'(i4,2f16.10)')(i,var(i),grad(i),i=1,nvarsrch)
	return
	end



c***	  REMAP_PARAMS returns the complete set of rotation, tilt, mag and
c	  compression variables based on the current values of the search
c	  parameters.

	subroutine remap_params(varlist)
	include 'alivar.inc'
c
	real*4 varlist(*)
c
	if(ifrotfix.ge.0)then
c	  
c	    if no rotations are fixed, then the first view has the global
c	    rotation angle, which is the first variable on the list
c	    Otherwise, there are nview-1 rotation variables on the list and all
c	    of the variables are relative to the view with fixed rotation angle
c
	  if(ifrotfix.eq.0)then
	    globrot=varlist(1)
	    rot(1)=globrot
	  else
	    globrot=rot(ifrotfix)
	  endif
c
	  do i=1,nview
	    if((ifrotfix.eq.0.and.i.gt.1) .or. i.lt.ifrotfix)then
	      rot(i)=globrot+varlist(i)
	    elseif(ifrotfix.gt.0 .and. i.gt.ifrotfix)then
	      rot(i)=globrot+varlist(i-1)
	    endif
	  enddo
	else
c	    
	  call map_one_var(varlist,rot,maprot,frcrot,linrot,fixedrot,
     &	      nview,glbrot,incrrot)
	endif
c
	do i=1,nview
	  if(maptilt(i).gt.0)then
	    if(lintilt(i).gt.0)then
	      tilt(i)=frctilt(i)*varlist(maptilt(i))+(1.-frctilt(i))*
     &		  varlist(lintilt(i))+tiltinc(i)
	    elseif(lintilt(i).eq.-1)then
	      tilt(i)=frctilt(i)*varlist(maptilt(i))+(1.-frctilt(i))*
     &		  fixedtilt+tiltinc(i)
	    elseif(lintilt(i).eq.-2)then
	      tilt(i)=frctilt(i)*varlist(maptilt(i))+(1.-frctilt(i))*
     &		  fixedtilt2+tiltinc(i)
	    else
	      tilt(i)=varlist(maptilt(i))+tiltinc(i)
	    endif
	  endif
	enddo
c
	call map_one_var(varlist,gmag,mapgmag,frcgmag,lingmag,fixedgmag,
     &	    nview,glbgmag,incrgmag)
c
	call map_one_var(varlist,comp,mapcomp,frccomp,lincomp,fixedcomp,
     &	    nview,glbgmag,0)
c
	call map_one_var(varlist,skew,mapskew,frcskew,linskew,fixedskew,
     &	    nview,glbskew,incrskew)
c	  
	if(mapdumdmag.gt.mapdmagstart) then
c	  
c	    if there are any dmag variables, the dummy variable is some factor
c	    times the sum of the real variables.  Save that position on
c	    varlist, put the value there, and compose all of the view
c	    parameters as usual
c
	  sum=0.
	  do i=mapdmagstart,mapdumdmag-1
	    sum=sum+varlist(i)
	  enddo
	  varsave=varlist(mapdumdmag)
	  varlist(mapdumdmag)=dumdmagfac*sum
	endif
c
	call map_one_var(varlist,dmag,mapdmag,frcdmag,lindmag,fixeddmag,
     &	    nview,glbdmag,incrdmag)
	if(mapdumdmag.gt.mapdmagstart)varlist(mapdumdmag)=varsave
c	    
	if(ifanyalf.ne.0)call map_one_var(varlist,alf,mapalf,frcalf,
     &	    linalf,fixedalf, nview,glbalf,incralf)
c
	return
	end


	
	subroutine map_one_var(varlist,val,map,frc,lin,fixed,nview,glb,
     &	    incr)
	real*4 varlist(*),val(*),frc(*),glb(*)
	integer*4 map(*),lin(*)
	if(incr.eq.0)then
	  do i=1,nview
	    if(map(i).gt.0)then
	      if(lin(i).gt.0)then
		val(i)=frc(i)*varlist(map(i))+(1.-frc(i))*
     &		    varlist(lin(i))
	      elseif(lin(i).lt.0)then
		val(i)=frc(i)*varlist(map(i))+(1.-frc(i))*fixed
	      else
		val(i)=varlist(map(i))
	      endif
	    endif
	  enddo
	else
	  do i=1,nview
	    if(map(i).gt.0)then
	      if(lin(i).gt.0)then
		val(i)=glb(i)+frc(i)*varlist(map(i))+(1.-frc(i))*
     &		    varlist(lin(i))
	      elseif(lin(i).lt.0)then
		val(i)=glb(i)+frc(i)*varlist(map(i))+(1.-frc(i))*fixed
	      else
		val(i)=glb(i)+varlist(map(i))
	      endif
	    endif
	  enddo
	endif
	return
	end

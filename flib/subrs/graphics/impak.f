c	  A package that provides an interface to the NCAR graphics and that
c	  has calls similar to DNM's old graphics package calls.  It provides
c	  for drawing with multiple line thicknesses
c	  This is a version to call Postscript graphics library on SGI
c
	subroutine imset(ithset,widset,upiset,safset,ifset)
	character*80 fname
	data fname /'gmeta.dat'/
	logical exist
	integer rename
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks,cscrit
	data nthick,width,upi,safe,xcur,ycur,symscl,cscrit,ifgks
     &	    /1,7.5,300.,0.00022,0.,0.,0.15,0.,0/
	save /imparm/
	if(ifgks.eq.0)then
	  inquire(file=fname,exist=exist)
	  namlen=lnblnk(fname)
	  if(exist)then
	    ierr=rename(fname,fname(1:namlen)//'~')
	    if(ierr.ne.0)write(6,*)
     &		' Error attempting to rename existing file'
	  endif
	  call psopen(fname(1:namlen),0.5,1.75,upi)
	  ifgks=1
	endif
	nthick=ithset
	if(ifset.eq.0)go to 10
	if(widset.gt.0)width=widset
	if(upiset.gt.0.)upi=upiset
	if(safset.ge.0.)safe=safset
	go to 15
10	widset=width
	upiset=upi
	safset=safe
15	xcur=0.
	ycur=0.
	udlen=1./upi
	exlen=udlen*(nthick-1)
	hafthk=0.5*exlen
c	write(*,*) 'udlen,exlen,hafthk',udlen,exlen,hafthk
	return

	entry thkang(setcrt)
	cscrit=0.
	if(setcrt.gt.0..and.setcrt.lt.45.)cscrit=cosd(setcrt)
	return
	end

	subroutine impa(x,y)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks,cscrit
	save /imparm/
	xcur=x
	ycur=y
	call point(trnc(x),trnc(y))
	return
	end

	subroutine imma(xin,yin)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks,cscrit
	save /imparm/
	xcur=xin
	ycur=yin
	go to 20
	entry immi(dx,dy)
	xcur=xcur+dx
	ycur=ycur+dy
20	if(nthick.eq.1.and.cscrit.eq.0.)call frstpt(trnc(xcur),trnc(ycur))
	return
	
	entry imvi(dx,dy)
	x=xcur+dx
	y=ycur+dy
	go to 22
	entry imva(xin,yin)
	x=xin
	y=yin
22	if(nthick.gt.1.or.cscrit.gt.0.)go to 25
	call vector(trnc(x),trnc(y))
	go to 40
25	ddx=x-xcur
	ddy=y-ycur
c	write(*,*) 'ddx,ddy',ddx,ddy
	if(ddx.eq.0..and.ddy.eq.0.)go to 40
	ddlen=sqrt(ddx**2+ddy**2)
	costh=ddx/ddlen
	sinth=ddy/ddlen
c	write(*,*) 'costh,sinth',costh,sinth
	xofs=hafthk*(1.+sinth-costh)
	yofs=hafthk*(1.-sinth-costh)
c	write(*,*) 'xofs,yofs',xofs,yofs
	xs=trnc(xcur+xofs)
	xe=trnc(x+xofs+exlen*costh)
	ys=trnc(ycur+yofs)
	ye=trnc(y+yofs+exlen*sinth)
c	write(*,*) 'xs,xe,ys,ye',xs,xe,ys,ye
	if(abs(ddx).ge.abs(ddy))then
	  udy=sign(udlen,costh)
	  udx=-udy*ddy/ddx
	else
	  udx=sign(udlen,-sinth)
	  udy=-udx*ddx/ddy
	endif
	csmax=max(abs(costh),abs(sinth))
	ntimes=(nthick-csmax)*csmax+0.5
	if(csmax.le.cscrit)ntimes=ntimes+1
c	write(*,*) 'udx,udy,ntimes',udx,udy,ntimes
	xinclas=0.
	yinclas=0.
	do 35 i=0,ntimes
	xinc=nint(i*udx/udlen)*udlen+sign(0.0005,udx)
	yinc=nint(i*udy/udlen)*udlen+sign(0.0005,udy)
	if(xinc.ne.xinclas.and.yinc.ne.yinclas)then
	  if(udx.lt.udy)then
	    call frstpt(xs+xinclas,ys+yinc)
	    call vector(xe+xinclas,ye+yinc)
	  else
	    call frstpt(xs+xinc,ys+yinclas)
	    call vector(xe+xinc,ye+yinclas)
	  endif
	endif
	xinclas=xinc
	yinclas=yinc
	call frstpt(xs+xinc,ys+yinc)
35	call vector(xe+xinc,ye+yinc)
40	xcur=x
	ycur=y
	return
	end


	subroutine imexit()
	call gksoff
	call exit(0)
	return
	end

	subroutine gksoff
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks,cscrit
	if(ifgks.eq.0)return
	call frame()
	call psclose()
	ifgks=0
	end

c	  
c	  Unix version for SGI
c
	subroutine pltout(metascreen)
	character*80 comstr
	call gksoff
	if(metascreen.eq.0)then
	  comstr='lp gmeta.dat'
	else
	  comstr='xpsview gmeta.dat'
	endif
	call system(comstr)
	if(metascreen.ne.0)write(*,101)
101	format(/,' WARNING: If you start making more plots, a new plot'
     &	    ,' file will be started,',/, '          the ',
     &	    'current file will become a backup (gmeta.dat~),'
     &	    ,/,'          and a previous backup will be deleted.')
	return
	end

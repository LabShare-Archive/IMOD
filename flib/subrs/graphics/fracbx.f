c	  FRACBX uses the IMPAK interface to NCAR graphics to produce little
c	  boxes that are filled up to the fraction "FRAC", at the location
c	  X,Y in inches.
c
	subroutine fracbx(x,y,frac)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks
	common /bxparm/ bxwith,bxhite,bxtick
	data bxwith,bxhite,bxtick/.1,.4,.05/
	save /bxparm/
	xlf=trnc(x-bxwith/2.)
	xrt=trnc(x+bxwith/2.)
	ylo=trnc(y-bxhite/2.)
	yhi=trnc(y+bxhite/2.)
	call imma(xlf,ylo)
	call imva(xrt,ylo)
	call imva(xrt,yhi)
	call imma(xlf,ylo)
	call imva(xlf,yhi)
	call imva(xrt,yhi)
	call imma(xlf,y)
	call imva(xlf-bxtick,y)
	call imma(xrt,y)
	call imva(xrt+bxtick,y)
	nlin=(xrt-xlf)*upi
	ylinlo=ylo+exlen
	ylinhi=ylinlo+frac*((yhi-udlen)-ylinlo)
	ntsave=nthick
	nthick=1
	do 10 i=1,nlin
	xx=xlf+i*udlen
	call imma(xx,ylinlo)
10	call imva(xx,ylinhi)
	nthick=ntsave
	return
	end

	subroutine fbxset(wid,hit,tic)
	common /bxparm/ bxwith,bxhite,bxtick
	if(wid.ne.0.)bxwith=wid
	if(hit.ne.0.)bxhite=hit
	if(tic.ne.0.)bxtick=tic
	return
	end

c	block data
c	common /bxparm/ bxwith,bxhite,bxtick
c	data bxwith,bxhite,bxtick/.1,.4,.05/
c	end

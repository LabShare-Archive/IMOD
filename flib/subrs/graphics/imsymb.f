	subroutine imsymb(x,y,ityp)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks,cscrit
	dimension index(21),vec(3,48),abot(4),bbot(4),atop(4),btop(4)
	data index /1,1,6,2,11,3,15,19,-1,-1,23,4,27,33,-1,-2,-1,-1,45
     &	    ,47,-1/
	data vec /-.5,-.5,-1.,.5,-.5,0.,.5,.5,0.,-.5,.5,0.,-.5,-.5,1.
     1, -.5,0.,-1., 0.,-.625,0., .5,0.,0., 0.,.625,0., -.5,0.,1.
     2, -.5,-.289,-1., .5,-.289,0., 0.,.577,0., -.5,-.289,1.
     3, -.4,-.4,-1., .4,.4,0., -.4,.4,-1., .4,-.4,1.
     4, -.5,0.,-1., .5,0.,0., 0.,-.5,-1., 0.,.5,1.
     5, -.5,.289,-1., .5,.289,0., 0.,-.577,0., -.5,.289,1.
     6, -.38,.5,-1., -.38,-.38,0., -.26,-.5,0., .26,-.5,0., .38,-.38,0.,
     7 .38,.5,1.
     8, .38,.38,-1., .26,.5,0., -.26,.5,0., -.38,.38,0., -.38,.12,0.
     9, -.26,0.,0., .26,0.,0., .38,-.12,0., .38,-.38,0., .26,-.5,0.
     8, -.26,-.5,0., -.38,-.38,1.
     &	    ,-.5,0,-1., .5,0,1.,  0,-.5,-1., 0,.5,1./
	data abot/-.5,-.625,-.289,.289/
	data bbot/0,1.25,0,0/
	data atop/.5,.625,.577,-.577/
	data btop/0,-1.25,-1.732,1.732/
	character*4 dummy,dumm2
	if(ityp.eq.0.or.ityp.gt.21)return
	if(ityp.lt.0)then
	  iscal=nint(2**15*0.87*symscl/width)
	  write(dummy,'(i4)')-ityp
	  nchar=alog10(float(-ityp))+1.0001
	  dumm2=' '
	  dumm2(1:nchar)=dummy(5-nchar:4)
	  call wtstr(x+.016,y-.018,dumm2(1:nchar),iscal,0,0)
	  return
	endif
	ind=index(ityp)
	go to (10,30,10,30,10,30,10,10,20,30,10,30,10,10,16,30,18,14
     1,10,10,165),ityp
10	xx=x+symscl*vec(1,ind)
	yy=y+symscl*vec(2,ind)
	instr=vec(3,ind)
	if(instr.lt.0)call imma(xx,yy)
	if(instr.ge.0)call imva(xx,yy)
	if(instr.gt.0)go to 27
	ind=ind+1
	go to 10
14	call imcirc(x,y,(2.+nthick)/upi)
	go to 27
16	dy=0.49*symscl
	call imma(x,y-dy)
	call imva(x,y+dy)
	go to 20
165	dx=0.49*symscl
	call imma(x-dx,y)
	call imva(x+dx,y)
	go to 20
18	call imcirc(x,y,(2.+nthick)/upi)
20	call imcirc(x,y,symscl)
27	return
30	nthsav=nthick
	rscl=symscl+0.5*nthick/upi
	call imset(1,ic1,ic2,ic3,0)
	if(ind+1)42,32,31
31	alo=abot(ind)
	blo=bbot(ind)
	ahi=atop(ind)
	bhi=btop(ind)
32	dx=1./(rscl*upi)
	xs=0.
	xscl=0.
	xt=trnc(x)
33	if(ind.lt.0)go to 34
	yl=alo+blo*xs
	yh=ahi+bhi*xs
	go to 36
34	yh=sqrt(0.25-xs**2)
	yl=-yh
36	xx=xt+xscl
	yyl=y+rscl*yl
	yyh=y+rscl*yh
	call imma(xx,yyl)
	call imva(xx,yyh)
	if(xs.eq.0)go to 38
	xx=xt-xscl
	call imma(xx,yyl)
	call imva(xx,yyh)
38	xs=xs+dx
	xscl=xscl+1./upi
	if(xs.le.0.5)go to 33
40	call imset(nthsav,c1,c2,c3,0)
	return
42	frac=.2+2.*symscl
	frac=amin1(.48,amax1(frac,.4))
	ncirc=0.75*frac*upi*symscl
	do 45 i=0,ncirc
45	call imcirc(x,y,(1.+frac*(float(i)/float(ncirc)-1.))*symscl)
	go to 40
	end

	subroutine symsiz(size)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks,cscrit
	symscl=size
	return
	end

	subroutine imdot(x,y)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     1,udlen,exlen,hafthk,symscl,ifgks,cscrit
	call imcirc(x,y,(2.+nthick)/upi)
	return
	end

	subroutine imcirc(x,y,symscl)
	n=symscl*180.
	n=max0(n,6)
	thefac=6.28319/n
	radfac=symscl*0.5
	do 25 i=0,n
	theta=i*thefac
	xx=x+radfac*cos(theta)
	yy=y+radfac*sin(theta)
	if(i.eq.0)call imma(xx,yy)
	if(i.gt.0)call imva(xx,yy)
25	continue
	return
	end

c	  A package that used to provide an interface to NCAR graphics and that
c	  has calls similar to DNM's old graphics package calls.  It provides
c	  for drawing with multiple line thicknesses, now using line widths
c	  in Postscript
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2003/10/29 04:31:50  mast
c	  fix problem with making backup file, call imodpsview with full path
c	
c	  Revision 3.2  2003/10/24 03:41:37  mast
c	  switched to calling routine for making backup file
c	
c	  Revision 3.1  2003/08/29 17:31:09  mast
c	  Made screen displayer call imodpsview, changed name of file to 
c	  gmeta.ps
c	
c
	subroutine imset(ithset,widset,upiset,safset,ifset)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	data nthick,width,upi,safe,xcur,ycur,symscl,ifgks
     &	    /1,7.5,300.,0.00022,0.,0.,0.15,0/
	save /imparm/
	character*80 fname
	common /gmetaname/fname
	data fname /'gmeta.ps'/

	if(ifgks.eq.0)then
	  ierr = imodBackupFile(fname)
	  if(ierr.ne.0)write(6,*)
     &	      ' Error attempting to rename existing file ',fname
	  call psopen(fname,0.5,1.75,upi)
	  ifgks=1
	endif
	nthick=ithset
	fthick = nthick - 1.
	if (nthick .eq. 2) fthick = 1.5
	if (nthick .lt. 2) fthick = 1.
	call pslinewidth(fthick)
	if(ifset.ne.0)then
	  if(widset.gt.0)width=widset
	  if(upiset.gt.0.)upi=upiset
	  if(safset.ge.0.)safe=safset
	else
	  widset=width
	  upiset=upi
	  safset=safe
	endif
	xcur=0.
	ycur=0.
	udlen=1./upi
	exlen=max(0.,udlen*(nthick-3))
	hafthk=0.5*udlen*(nthick-1)
c	write(*,*) 'udlen,exlen,hafthk',udlen,exlen,hafthk
	return
	end

	subroutine impa(x,y)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	xcur=x
	ycur=y
	call point(trnc(x),trnc(y))
	return
	end

	subroutine imma(xin,yin)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	xcur=xin
	ycur=yin
	if(nthick.eq.1)call frstpt(trnc(xcur),trnc(ycur))
	return
	end

	subroutine immi(dx,dy)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	call imma(xcur+dx, xcur+dy)
	return
	end
	
	
	subroutine imvi(dx,dy)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	call imva(xcur+dx,ycur+dy)
	return
	end

	subroutine imva(x,y)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	if(nthick.le.1)then
	  call vector(trnc(x),trnc(y))
	else
	  ddx=x-xcur
	  ddy=y-ycur
c	    write(*,*) 'ddx,ddy',ddx,ddy
	  if(ddx.ne.0..or.ddy.ne.0.) then
	    ddlen=sqrt(ddx**2+ddy**2)
	    xinc=0.5*ddx* exlen / ddlen
	    yinc=0.5*ddy* exlen / ddlen
	    xs=trnc(xcur-xinc)
	    xe=trnc(x+xinc)
	    ys=trnc(ycur-yinc)
	    ye=trnc(y+yinc)
	    call frstpt(xs,ys)
	    call vector(xe,ye)
	  endif
	endif
	xcur=x
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
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	if(ifgks.eq.0)return
	call frame()
	call psclose()
	ifgks=0
	end

c	  
c	  Plot out the data to screen or printer view script
c
	subroutine pltout(metascreen)
	character*120 comstr
	character*80 fname,imodpath
	character*20 imodshell,outcom
	common /gmetaname/fname

	call gksoff
c	  
c	  10/28/03: switch to calling imodpsview for printing too and run
c	  tcsh explicitly
c	
	if(metascreen.eq.0)then
	  outcom='imodpsview -p'
	else
	  outcom='imodpsview'
	endif
	if (imodGetenv('IMOD_DIR', imodpath) .ne. 0) then
	  print *, 'impak failed to get IMOD_DIR environment '//
     &	      'variable for running imodpsview'
	  return
	endif
	if (imodGetenv('IMOD_CSHELL', imodshell) .ne. 0)
     &	    imodshell = 'tcsh'
	namelen = lnblnk(fname)
	lencom = lnblnk(outcom)
	write(comstr, '(a,1x,a,1x,a,a,a,1x,a,1x,a)')
     &	    imodshell(1:lnblnk(imodshell)),'-f',
     &	    imodpath(1:lnblnk(imodpath)),'/bin/',
     &	    outcom(1:lencom),fname(1:namelen)
	call system(comstr)
	if(metascreen.ne.0)write(*,101)fname(1:namelen)
101	format(/,' WARNING: If you start making more plots, a new plot'
     &	    ,' file will be started,',/, '          the ',
     &	    'current file will become a backup (',a,'~),'
     &	    ,/,'          and a previous backup will be deleted.')
	return
	end

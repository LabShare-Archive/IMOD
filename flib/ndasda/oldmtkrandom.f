	subroutine random_shifts(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &	    ranmin,ranmax,probnear,delnear,nrestrict,nshiftyp,itypshift,
     &	    nchcktyp,itypchck)
	parameter (limgraphs=50,limbins=301,limpnts=2000,
     &	    limtyp=50,itypall=999,limxyz=50000)
	real*4 xmt(*),ymt(*),zmt(*),probnear(*)
	integer*4 indstrt(*),npntobj(*)
	integer*4 icolor(*)			!types of sample points
	integer*4 itypshift(*),itypchck(*)
	real*4 aa(limxyz),bb(limxyz),cc(limxyz),dd(limxyz)
	real*4 xmin(limxyz),xmax(limxyz),ymin(limxyz),ymax(limxyz)
	real*4 glbxmin(limpnts),glbymin(limpnts)
	real*4 glbxmax(limpnts),glbymax(limpnts)
	logical*1 needshift(limpnts),check(limpnts)
c	  
	logical farup,fardown
	character*8 jtime
c	  
	limfit=2
	ntrialim=300
	call time(jtime)
	iseed=2*((ichar(jtime(8:8))+128*(ichar(jtime(7:7))+
     &	    128*(ichar(jtime(5:5))+128*ichar(jtime(4:4)))))/2)+1
c	  
c	      precompute all the needed line fits for each MT
c	  
	do ii=1,nmt
	  needshift(ii)=.false.
	  do kk=1,nshiftyp
	    if(itypshift(kk).eq.itypall .or. itypshift(kk).eq.
     &		icolor(ii)) needshift(ii)=.true.
	  enddo
	  check(ii)=.false.
	  do kk=1,nchcktyp
	    if(itypchck(kk).eq.itypall .or. itypchck(kk).eq.
     &		icolor(ii)) check(ii)=.true.
	  enddo
	  if(needshift(ii).or.check(ii))then
	    indref=indstrt(ii)
	    nfitref=min(limfit,npntobj(ii))
	    limref=indref+npntobj(ii)-nfitref
	    glbxmin(ii)=1.e10
	    glbxmax(ii)=-1.e10
	    glbymin(ii)=1.e10
	    glbymax(ii)=-1.e10
	    do iref=indref,limref
	      zs1=zmt(iref)
	      zn1=zmt(iref+nfitref-1)
	      call linfit(zmt(iref),xmt(iref),nfitref,aa(iref),bb(iref))
	      call linfit(zmt(iref),ymt(iref),nfitref,cc(iref),dd(iref))
	      xs1=aa(iref)*zs1+bb(iref)
	      ys1=cc(iref)*zs1+dd(iref)
	      xn1=aa(iref)*zn1+bb(iref)
	      yn1=cc(iref)*zn1+dd(iref)
	      xmax(iref)=max(xs1,xn1)
	      ymax(iref)=max(ys1,yn1)
	      xmin(iref)=min(xs1,xn1)
	      ymin(iref)=min(ys1,yn1)
	      glbxmin(ii)=min(glbxmin(ii),xmin(iref))
	      glbxmax(ii)=max(glbxmax(ii),xmax(iref))
	      glbymin(ii)=min(glbymin(ii),ymin(iref))
	      glbymax(ii)=max(glbymax(ii),ymax(iref))
	    enddo
	  endif
	enddo
c	  
	distlim=delnear*nrestrict
	distabs=0.
	nfail=0
	need15=0
	need20=0
	i=1
	do while (i.le.nrestrict.and.probnear(i).eq.0)
	  distabs=i*delnear
	  i=i+1
	enddo
c	  
c	  loop through the objects looking for ones to shift
c	  
	do iobjref=1,nmt
c
	  shiftxlas=0.
	  shiftylas=0.
	  shiftz=0.
	  ntrial=0
	  ranmintmp=ranmin
	  ranmaxtmp=abs(ranmax)
	  indref=indstrt(iobjref)
	  nfitref=min(limfit,npntobj(iobjref))
	  limref=indref+npntobj(iobjref)-nfitref
	  do while(needshift(iobjref))
c	      
c	      impose a new shift on object: shift all the intercepts from fits
c	      and the mins and maxes, nut don't need to shift XMT, YMT yet.
c
	    ntrial=ntrial+1
4	    x0=ran(iseed)+ran(iseed)+ran(iseed)+ran(iseed)
	    shiftx=ranmaxtmp*(2.*ran(iseed)-1.)
	    x0=ran(iseed)+ran(iseed)+ran(iseed)+ran(iseed)
	    shifty=ranmaxtmp*(2.*ran(iseed)-1.)
	    if(ranmax.gt.0.)then
	      distshft=shiftx**2+shifty**2
	    else
	      x0=ran(iseed)+ran(iseed)+ran(iseed)+ran(iseed)
	      shiftz=ranmaxtmp*(2.*ran(iseed)-1.)
	      distshft=shiftx**2+shifty**2+shiftz**2
	    endif
	    if(distshft.lt.ranmintmp**2.or.distshft.gt.ranmaxtmp**2)go to 4
	    glbxmin(iobjref)=glbxmin(iobjref)+shiftx-shiftxlas
	    glbymin(iobjref)=glbymin(iobjref)+shifty-shiftylas
	    glbxmax(iobjref)=glbxmax(iobjref)+shiftx-shiftxlas
	    glbymax(iobjref)=glbymax(iobjref)+shifty-shiftylas
	    do iref=indref,limref
	      bb(iref)=bb(iref)+shiftx-shiftxlas
	      dd(iref)=dd(iref)+shifty-shiftylas
	      xmin(iref)=xmin(iref)+shiftx-shiftxlas
	      ymin(iref)=ymin(iref)+shifty-shiftylas
	      xmax(iref)=xmax(iref)+shiftx-shiftxlas
	      ymax(iref)=ymax(iref)+shifty-shiftylas
	    enddo
	    shiftxlas=shiftx
	    shiftylas=shifty
c	      
c	      now check its distance from all neighbors that don't need shift
c
	    do iobjneigh=1,nmt
	      if(check(iobjneigh).and..not.needshift(iobjneigh)) then
		distmin=1.01*distlim
		dminsq=distmin**2
		indneigh=indstrt(iobjneigh)
		nfitneigh=min(limfit,npntobj(iobjneigh))
		minneigh=indneigh
		limneigh=indneigh+npntobj(iobjneigh)-nfitneigh
		if(glbxmin(iobjref)-glbxmax(iobjneigh).lt.distmin.and.
     &		    glbxmin(iobjneigh)-glbxmax(iobjref).lt.distmin.and.
     &		    glbymin(iobjref)-glbymax(iobjneigh).lt.distmin.and.
     &		    glbymin(iobjneigh)-glbymax(iobjref).lt.distmin.and.
     &		    zmt(indref)+shiftz-zmt(indneigh+npntobj(iobjneigh)-1).lt.
     &		    distmin.and.
     &		    zmt(indneigh)-(zmt(indref+npntobj(iobjref)-1)+shiftz).lt.
     &		    distmin)then
		  do iref=indref,limref
		    zs1=zmt(iref)+shiftz
		    zn1=zmt(iref+nfitref-1)+shiftz
		    minstrt=minneigh
		    ineigh=minstrt
		    idir=-1
		    do while(ineigh.le.limneigh)
		      zs2=zmt(ineigh)
		      zn2=zmt(ineigh+nfitneigh-1)
		      fardown=zs1-zn2.gt.distmin
		      farup=zs2-zn1.gt.distmin
		      if(.not.(fardown.or.farup))then
			if(xmin(ineigh)-xmax(iref).lt.distmin.and.
     &			    xmin(iref)-xmax(ineigh).lt.distmin)then
			  if(ymin(ineigh)-ymax(iref).lt.distmin.and.
     &			      ymin(iref)-ymax(ineigh).lt.distmin)then
			    call segment_dist(aa(iref),bb(iref),
     &				cc(iref),dd(iref),aa(ineigh),bb(ineigh),
     &				cc(ineigh),dd(ineigh),zs1,zn1,zs2,zn2,
     &				z1,z2,dsqr)
c			    print *,dsqr
			    if(dsqr.lt.dminsq)then
			      dminsq=dsqr
			      distmin=sqrt(dsqr)
			      minneigh=ineigh
			      if(distmin.lt.distabs)go to 20
			    endif
			  endif
			endif
			ineigh=ineigh+idir
		      else
			if(idir.eq.-1.and.fardown)then
			  ineigh=indneigh-1
			elseif(farup)then
			  ineigh=limneigh+1
			else
			  ineigh=ineigh+idir
			endif
			if(fardown)minneigh=max(minneigh,ineigh+1)
		      endif
		      if(ineigh.lt.indneigh)then
			idir=1
			ineigh=minstrt+1
		      endif
c		      write(*,'(5f10.5)')zs1,zn1,zs2,zn2,distmin
		    enddo
		  enddo
		  if(distmin.lt.distlim)then
		    ibin=distmin/delnear+1.
		    if(ran(iseed).ge.probnear(ibin))go to 20
		  endif
		endif
	      endif
	    enddo
c	      
c	      passed through all the neighbors; accept and implement shift
c	      
	    do iref=indref,indref+npntobj(iobjref)-1
	      xmt(iref)=xmt(iref)+shiftx
	      ymt(iref)=ymt(iref)+shifty
	      zmt(iref)=zmt(iref)+shiftz
	    enddo
	    needshift(iobjref)=.false.
	    write(*,'(a,i4,i6,a)')'+Object ',iobjref,ntrial,' trials'
20	    continue				!rejection point
	    if(ntrial.gt.ntrialim)then
	      if(ranmaxtmp.eq.abs(ranmax))then
		ranmaxtmp=1.5*abs(ranmax)
		ranmintmp=1.5*ranmin
		ntrial=ntrialim/2
		need15=need15+1
	      elseif(ranmaxtmp.eq.1.5*abs(ranmax))then
		ranmaxtmp=2.*abs(ranmax)
		ranmintmp=2.*ranmin
		ntrial=ntrialim/2
		need20=need20+1
	      else
		needshift(iobjref)=.false.
		write(*,'(a,30x,a,i4)')'+','not shifted:',iobjref
		nfail=nfail+1
	      endif
	    endif
	  enddo
	enddo
	write(*,104)nfail,need15-need20,need20-nfail
104	format(i4,' objects were not shifted;',i3,' &',i3,
     &	    ' needed 1.5 & 2 times range')
	return
	end

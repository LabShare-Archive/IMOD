c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2003/04/11 17:29:33  mast
c	  Added declarations for implicit none, added cgx, cgy to tltcntrl
c	
c	  Revision 3.1  2002/07/28 22:56:54  mast
c	  Stadardize error output
c	
c
	subroutine tiltali(ifdidalign,resmean,iview)
	implicit none
	include 'alivar.inc'
c
c	  IF MAXVAR IS NOT BIGGER THAN MAXMETRO, NEED TO DIMENSION
c	  var to maxmetro
c
	integer maxvar,maxmetro
	parameter (maxvar=5*maxview,maxmetro=2100)

	integer*4 ifdidalign, iview
	real*4 resmean(*)
	real*4 tiltorig(maxview),gmagorig(maxview),rotorig(maxview)
	integer*4 mapalltilt(maxview),mapallmag(maxview),ivorig(maxview)
	integer*4 ivsolv(maxview),iobjali(maxreal)
	real*4 dxysav(2,maxview),xyzsav(3,maxreal)
	integer*4 nvuall,imintilt,mininview,minvtiltali,initxyzdone
	real*4 randoaxis,randotilt, scalexy,xcen,ycen,xorig,yorig
	real*4 xdelt,ydelt,facm,eps,cgx,cgy
	integer*4 nsolve,ncycle
	common /tltcntrl/nvuall,imintilt,mininview,minvtiltali,
     &	    randoaxis,randotilt, scalexy,xcen,ycen,cgx,cgy,xorig,
     &	    yorig,xdelt,ydelt, initxyzdone,nsolve,facm, ncycle,eps,
     &	    tiltorig,gmagorig,rotorig,mapalltilt,mapallmag,ivorig,
     &	    ivsolv,iobjali,dxysav,xyzsav
c	  
	real*4 var(maxvar),grad(maxmetro),h(maxmetro*(maxmetro+3))
	real*4 erlist(100)
	logical firsttime
	double precision error
	common /functfirst/ firsttime
	real*4 dtor
	data dtor/0.0174532/
	integer*4 imintiltsolv,itry,isolmininit,i,isolmin,nprojpt,iv
	real*4 tltslvmin,tltslvmax,ang,tltran,ermin,ermininit,finit
	integer*4 nvarsrch,ifmaptilt,isolve,jpt,kpt,nvargeom,ier,kount
	real*4 f,ffinal,rsum
	integer*4 ior,ipt,ivor,j

	ifanyalf=0
	call proc_model(xcen,ycen,xdelt,ydelt,xorig,yorig,
     &	    scalexy, nvuall,mininview,ivsolv,ivorig,
     &	    xx,yy,isecview,maxprojpt,maxreal,irealstr,iobjali,
     &	    nview, nprojpt,nrealpt)
	ifdidalign=0
	if(nview.ge.minvtiltali)then
c	    
c	    if enough views, set up for solving tilt axis and tilt
c	    angles depending on range of tilt angles
c	    
	  tltslvmin=1.e10
	  tltslvmax=-1.e10
	  do iv=1,nview
	    ang=tiltorig(ivorig(iv))
	    tltslvmin=min(tltslvmin,ang)
	    tltslvmax=max(tltslvmax,ang)
	  enddo
	  tltran=tltslvmax-tltslvmin
	  imintiltsolv=ivsolv(imintilt)
	  ifrotfix=0
	  if(tltran.lt.randoaxis)ifrotfix=imintiltsolv
	  ifmaptilt=1
	  if(tltran.lt.randotilt)ifmaptilt=0
	  call proc_vars(rotorig,tiltorig,gmagorig,ivorig,
     &	      mapalltilt ,mapallmag,ifmaptilt,imintiltsolv,
     &	      var, nvarsrch)
	  if(initxyzdone.eq.0)then
c	      
c	      first time, initialize xyz and dxy
c	      
c	      try either with initial dxy solved to equalize centroids
c	      section-to- section, or with dxy 0.  Find which way gives
c	      lowest error somewhere along the line, and redo it that
c	      way to do just the best number of iterations
c	      
	    call remap_params(var)
c	      
c	      initial trial with call to INIT_DXY
c	      
c	    dxy(1,imintiltsolv)=cgx/scalexy
c	    dxy(1,imintiltsolv)=cgy/scalexy
	    call init_dxy(xx,yy,isecview,irealstr,
     &		nview,nrealpt,imintiltsolv,dxy)
c	      
	    do itry=1,2
c		
c		second time through, save minimum error and
c		iteration # from first trial that used call to
c		init_dxy
c		
	      isolmininit=isolmin
	      ermininit=ermin
c		
	      call solve_xyzd(xx,yy,isecview,irealstr,
     &		  nview, nrealpt,tilt,rot, gmag,comp,xyz,
     &		  dxy,nsolve,error,erlist,isolve)
c		
c		find iteration with minimum error
c		
	      ermin=1.e30
	      do i=1,isolve-1
		if(erlist(i).lt.ermin)then
		  isolmin=i
		  ermin=erlist(i)
		endif
	      enddo
c		print *,itry,isolve,ermin,isolmin
c		
c		set dxy to 0 for second try, or leave at zero for
c		final setup
c		
	      do iv=1,nview
		dxy(1,iv)=cgx/scalexy
		dxy(2,iv)=cgy/scalexy
	      enddo
	    enddo
c	      
	    if(ermininit.lt.ermin)then
	      isolmin=isolmininit
	      call init_dxy(xx,yy,isecview,irealstr,
     &		  nview,nrealpt,imintiltsolv,dxy)
	    endif
c	      
	    call solve_xyzd(xx,yy,isecview,irealstr,
     &		nview, nrealpt,tilt,rot, gmag,comp,xyz,
     &		dxy,isolmin,error,erlist,isolve)
	    initxyzdone=1
	  else
	    do jpt=1,nrealpt
	      kpt=iobjali(jpt)
	      do i=1,3
		xyz(i,jpt)=xyzsav(i,kpt)/scalexy
	      enddo
	    enddo
	    do iv=1,nview
	      dxy(1,iv)=dxysav(1,ivorig(iv))/scalexy
	      dxy(2,iv)=dxysav(2,ivorig(iv))/scalexy
	    enddo
	  endif
c	    
c	    pack the xyz into the var list
c	    
	  if(nvarsrch+3*nrealpt.gt.min(maxmetro,maxvar))call errorexit(
     &	      'TOO MANY VARIABLES FOR H ARRAY IN METRO',0)
	  
	  nvargeom=nvarsrch
	  do jpt=1,nrealpt-1
	    do i=1,3
	      nvarsrch=nvarsrch+1
	      var(nvarsrch)=xyz(i,jpt)
	    enddo
	  enddo
c	    
	  firsttime=.true.
	  call funct(nvarsrch,var,finit,grad)
c	    WRITE(6,70)FINIT
c70	    FORMAT(/' Variable Metric minimization',T50,
c	    &	    'Initial F:',T67,E14.7)
C	    
C	    -----------------------------------------------------
C	    Call variable metric minimizer
C	    CALL METRO(N,X,F,G,FACTOR,EST,EPS,LIMIT,IER,H,KOUNT)
C	    -----------------------------------------------------
C	    
	  CALL METRO (nvarsrch,var,F,Grad,facm,.0000001,
     &	      eps,-NCYCLE,IER, H,KOUNT)
C	    Final call to FUNCT
	  CALL FUNCT(nvarsrch,var,FFINAL,Grad)
	  WRITE(6,98)nview,KOUNT,ffinal
98	  format(i4,' views,',i5,' cycles, F =',e14.7)
c98	    FORMAT(/T50,'Final   F : ',T67,E14.7/
c	    &      /' Number of cycles : ',I5)
C-----------------------------------------------------------------------
C	    Error returns:
	  IF(IER.NE.0)THEN
	    print *,'tiltalign error going to view',iview
	    GO TO (91,92,93,94),IER
91	    WRITE(6,910)
	    go to 95
92	    WRITE(6,920)
	    go to 95
94	    WRITE(6,940)
	    go to 95
93	    WRITE(6,930)
910	    FORMAT(/' ERROR: IER=1  DG > 0')
920	    FORMAT(/' ERROR: IER=2  Linear search lost')
930	    FORMAT(/' IER=3  Iteration limit exceeded....')
940	    FORMAT(/' ERROR: IER=4  Matrix non-positive definite')
	  END IF
c	    
c	    unscale all the points, dx, dy, and restore angles to
c	    degrees
c	    
	  do i=1,nrealpt
	    ior=iobjali(i)
	    do j=1,3
	      xyzsav(j,ior)=xyz(j,i)*scalexy
	    enddo
	    rsum=0.
	    do ipt=irealstr(i),irealstr(i+1)-1
	      rsum=rsum+sqrt(xresid(ipt)**2+yresid(ipt)**2)
	    enddo
	    resmean(ior)=rsum*scalexy/
     &			(irealstr(i+1)-irealstr(i))
	  enddo
c	    
	  do iv=1,nview
	    ivor=ivorig(iv)
	    dxysav(1,ivor)=dxy(1,iv)*scalexy
	    dxysav(2,ivor)=dxy(2,iv)*scalexy
	    rotorig(ivor)=rot(iv)/dtor
	    tiltorig(ivor)=tilt(iv)/dtor
	    gmagorig(ivor)=gmag(iv)
	  enddo
	  ifdidalign=1
	endif
95	return
	end

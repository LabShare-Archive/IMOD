c	  INPUT_VARS gets the specifications for the geometric variables
c	  rotation, tilt, mag and compression.  It fills the VAR array and
c	  the different variable and mapping arrays.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.12  2005/03/28 22:52:13  mast
c	  Called function for remapping separate groups, fixed this not being
c	  done for PIP input
c	
c	  Revision 3.11  2005/03/25 20:10:47  mast
c	  Fixed problems with mapping rotation variables in blocks or linearly
c	  with a fixed angle
c	
c	  Revision 3.10  2004/10/24 22:49:34  mast
c	  fixed line length and lnblnk declaration problems
c	
c	  Revision 3.9  2004/10/24 22:29:44  mast
c	  Chnages for pip input and projection stretch
c	
c	  Revision 3.8  2004/06/24 15:38:51  mast
c	  Changed calls to map_vars to be compatible with PIP-input version
c	
c	  Revision 3.7  2004/05/05 05:45:11  mast
c	  Fixed some uninitialized variables
c	
c	  Revision 3.6  2003/10/03 00:59:21  mast
c	  Changed terminology to refered to tilt angle offset
c	
c	  Revision 3.5  2003/06/21 00:48:41  mast
c	  Switch to new version of get_tilt_angles that can use PIP
c	
c	  Revision 3.4  2002/12/20 23:58:39  mast
c	  Fix variable mapping output when one rotation is fixed, and add
c	  option to fix all rotations
c	
c	  Revision 3.3  2002/07/28 22:38:35  mast
c	  Standardized error exit and output
c	
c	  Revision 3.2  2002/05/09 03:53:18  mast
c	  Fixed some line lengths that would not compile on SGI
c	
c	  Revision 3.1  2002/05/07 02:06:30  mast
c	  Changes to make things work well with a subset of views
c	
c
	subroutine input_vars(var,varname,inputalf,nvarsrch,nvarang,
     &	    nvarscl, imintilt,ncompsrch,iflocal,maptiltstart,
     &	    mapalfstart,tiltorig,tiltadd, pipinput) 
	implicit none
	integer*4 inputalf,nvarsrch,nvarang,nvarscl, imintilt,ncompsrch
	integer*4 iflocal,maptiltstart,mapalfstart
	logical pipinput
	include 'alivar.inc'
	integer maxgrp
	parameter (maxgrp=20)
	character*8 varname(*)
	real*4 var(*),tiltorig(*),tiltadd
	integer*4 maplist(maxview)
	real*4 grpsize(maxview)
c
	integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),ngsep
	common /mapsep/ ivsep,nsepingrp,ngsep
	character*8 dump(8)
	character*10 distmp,disttext(3)
	data disttext/'distortion','X stretch ','axis skew '/
	character*17 lintmp,lintext(2)
	data lintext/'3 block, 4 linear','3 linear, 4 block'/
	character*8 distOptTmp, distOptText(2)
	data distOptText /'XStretch', 'Skew'/
	character*4 rotText
	real*4 dtor/0.0174532/
c	  
	real*4 powertilt,powercomp,powermag,powerskew,powerdmag,power
	real*4 powerrot,poweralf,rotstart,fixdum1,fixdum2,defrot
	real*4 tiltmin,origdev,fixdum
	integer*4 i,iview,ioptrot,iref1,iflin,ioptilt,nviewfix,j,ig
	integer*4 iref2,ioptmag,irefcomp,ioptcomp,iffix,iv,jv,ioptdel
	integer*4 irefdmag,nvartmp,ivdum,idist,ioptdist,ioptalf
	integer*4 ireftilt,ndmagvar,ivl,ivh, lenOpt, ioptdmag, ioptskew
	integer*4 nearest_view,ifpip,mapfix,ierr,lnblnk
	character*1024 listString
c
	integer*4 PipGetInteger,PipNumberOfEntries
	integer*4 PipGetString,PipGetFloat,PipGetBoolean
	character*40 PrependLocal
	character*80 concat
c
	powertilt=1.
	powercomp=1.
	powermag=0.
	powerskew=0.
	powerdmag=1.5
	powerrot=0.
	poweralf=0.
	ioptdist = 0
	iffix = 0
c
	tiltadd=0.
	nvarsrch=0
	ifpip = 0
	if (pipinput) ifpip = 1
	if (pipinput .and. iflocal .eq. 0)then
	  ierr = PipNumberOfEntries('SeparateGroup', ngsep)
	  if (ngsep .gt. maxgrp) call errorexit
     &	      ('TOO MANY SEPARATE GROUPS FOR ARRAYS', 0)
	  do ig =1, ngsep
	    ierr = PipGetString('SeparateGroup', listString)
	    call parselist(listString, ivsep(1,ig),nsepingrp(ig))
	    call mapSeparateGroup(ivsep(1,ig), nsepingrp(ig), mapFileToView,
     &		nFileViews)
	  enddo
	  rotstart = 0.
	  ierr = PipGetFloat('RotationAngle', rotstart)
	endif
c	  
	if (pipinput) then
	  ifrotfix = 0
	  ierr = PipGetInteger('RotationFixedView', ifrotfix)
	  ioptrot = 0
	  ierr = PipGetInteger(PrependLocal('RotOption', iflocal), ioptrot)
	else
	  if(iflocal.eq.0)then
c
c	      set up the first search variable as a global rotation angle
c	      and the rest as delta rotation angles of each view after first
c
	    write(*,'(1x,a,$)')
     &		'Initial angle of rotation in projection plane: '
	    read(5,*)rotstart
c
	    write(*,'(1x,a,/,a,/,a,$)')'Enter 0 to find all rotations,'//
     &		' -1 to find a single global rotation,',
     &		' -2 to fix all rotations at the initial angle',
     &		' or a positive # to fix one view at the initial angle: '
	    
	    read(5,*)ifrotfix
c	      
c	      transpose to new option list
c	      
	    ioptrot = 0
	    if (ifrotfix .ge. 0) ioptrot = 1
	    if (ifrotfix .eq. -1) ioptrot = -1
	  else
c	    
c	    local entry already had the option list
c	    
	    print *,'Enter -1 to solve for a single incremental rotation'
	    print *,'      0 to fix all rotations at global value'
	    print *,'      1 to give each view an independent incremental'
     &		//' rotation'
	    write(*,'(1x,a,/,a,$)')'      2 to specify other mappings of'
     &		//' rotation variables',
     &		'       3 or 4 for automatic mapping of groups of views '
     &		//'(3 linear, 4 block): '
	    read(5,*)ioptrot
	  endif
	endif
c	  
c	  4/10/04: Eliminated global rotation variable, simplified treatment
c	  of rotation
c
	if (iflocal .eq. 0) then
	  rotstart=dtor*rotstart
	else
	  rotstart = 0.
	endif
c
	if(ifrotfix.gt.0)ifrotfix=nearest_view(ifrotfix)
c	  
c	  set up appropriate mapping list or read it in
c	  set the meaning of ifrotfix =  0 for a one global rotation variable
c	  and the others incremental to it, + for one variable fixed,
c	  -1 for all fixed, -2 for single variable, -3 otherwise
c	  
	iref1=ifrotfix
	if(iflocal.gt.1.and.ioptrot.gt.0)iref1=0
	iflin=0
	defrot = rotstart
c	  
c	  All one variable - set default to true angle
c	  
	if(ioptrot.lt.0)then
	  do i=1,nview
	    maplist(i)=1
	  enddo
	  ifrotfix=-2
c	    
c	    All fixed - also set default to angle, set reference to 1
c	    
	elseif(ioptrot.eq.0)then
	  do i=1,nview
	    maplist(i)=0
	  enddo
	  ifrotfix=-1
	  iref1 = 1
c	    
c	    All separate variables
c	    
	elseif(ioptrot.eq.1)then
	  do i=1,nview
	    maplist(i)=i
	  enddo
c	    
c	    Specified mapping
c	    5/2/02: changed irotfix (?) to iref1 and made output conditional
c	    
	elseif(ioptrot.eq.2)then
	  mapfix = 0
	  if (iref1.gt.0) mapfix = mapviewtofile(iref1)
	  call GetMapList('rotation', 'Rot', mapfix, defrot, ifpip, iflocal,
     &	      nview, maplist)
c	    
c	    automap
c	    
	else
	  power=0.
	  if(ioptrot.eq.3)then
	    iflin=1
	    power=powerrot
	  endif
	  call setgrpsize(tilt,nview,power,grpsize)
	  call automap(nview,maplist,grpsize,mapfiletoview,nfileviews
     &	      ,ifpip, 1, PrependLocal('RotDefaultGrouping', iflocal),
     &	      PrependLocal('RotNondefaultGroup', iflocal))
	  if (.not.pipinput) write(6,111)(maplist(i),i=1,nview)
	endif
c	  
c	  Make sure ifrotfix = 0 means what it is supposed to
c	  
	if (iflocal.gt.0 .and. ifrotfix .eq. 0) ifrotfix = -3
	rotText = 'rot '
c	  
c	  analyze map list
c	  
	call analyze_maps(rot,maprot,linrot,frcrot,fixedrot,fixdum,
     &	    iflin, maplist,nview, iref1,0,defrot,rotText,var,varname,
     &	    nvarsrch,mapviewtofile)
c	write(6,111)(maprot(i),i=1,nview)
c
	if (.not. pipinput .and. iflocal .eq. 0) then
c	  
c	    Get list of views to treat separately in automapping
c	    
	  write(*,'(1x,a,/,a,$)') 'If you are going to automap '//
     &	      'variables,'
     &	      //' enter the number of sets of views to treat separately'
     &	      //' from the main set of views (otherwise enter 0): '
	  read(5,*)ngsep
	  do ig=1,ngsep
	    write(*,'(1x,a,i3,a,$)')'List of views in set',ig,
     &		' (ranges OK): '
	    call rdlist(5,ivsep(1,ig),nsepingrp(ig))
c	      
c	      check legality and trim ones not in included views
c	      
	    call mapSeparateGroup(ivsep(1,ig), nsepingrp(ig), mapFileToView,
     &		nFileViews)
c	    print *,(ivsep(i,ig),i=1,nsepingrp(ig))
	  enddo
	endif
	if (iflocal .eq. 0) then
c
c	    get initial tilt angles for all views, convert to radians
c	    save adjusted angles in tiltorig, then map as radians into tilt
c	    
	  call get_tilt_angles(nfileviews,3,tiltorig, maxview, ifpip)
	  if (pipinput) then
	    tiltadd = 0.
	    ierr = PipGetFloat('AngleOffset', tiltadd)
	  else
	    write(*,'(1x,a,$)')
     &		'Angle offset, i.e. amount to add to all angles: '
	    read(5,*)tiltadd
	  endif
	  do i=1,nfileviews
	    tiltorig(i)=tiltorig(i)+tiltadd
	    if(mapfiletoview(i).ne.0)tilt(mapfiletoview(i))=tiltorig(i)*dtor
	  enddo
	endif
c	  
c	  For tilt, set up default on increments and maps
c	  also find section closest to zero: it will have mag set to 1.0
c
	maptiltstart=nvarsrch+1
	tiltmin=100.
	do i=1,nview
	  tiltinc(i)=0.
	  maptilt(i)=0
	  maplist(i)=0
	  origdev=abs(tilt(i)-tiltadd*dtor)
	  if(tiltmin.gt.origdev)then
	    tiltmin=origdev
	    imintilt=i
	  endif
	enddo
c	  
c	  get type of mapping
c	  
	if (pipinput) then
	  ioptilt = 0
	  ierr = PipGetInteger(PrependLocal('TiltOption', iflocal), ioptilt)
	else
	  print *,'Enter 0 to have all tilt angles fixed'
	  print *,'      1 to vary all except one for a specified view'
	  print *,'      2 to vary all except for the view at minimum tilt'
	  print *,'      3 to vary all except for a specified view and the'
     &	      //' one at minimum tilt'
	  write(*,'(1x,a,/,a,$)')'      4 to specify other combinations'//
     &	      ' of variable, mapped and fixed',
     &	      '       5-8 for automatic mapping of groups of views '//
     &	      '(5 & 7 linear with 1 or 2','       fixed tilts, 6 & 8 '//
     &	      'block with 1 or 2 fixed tilts): '
	  read(5,*)ioptilt
	endif
c	  
c	set up maplist appropriately or get whole map  
c
	iflin=0
	nviewfix=0
	if(ioptilt.ge.1.and.ioptilt.le.3)then
	  do i=1,nview
	    maplist(i)=i
	  enddo
	  if(ioptilt.gt.1)maplist(imintilt)=0
	  if(ioptilt.ne.2)then
	    if (pipinput) then
	      if (PipGetInteger(PrependLocal('TiltFixedView', iflocal),
     &		  nviewfix) .ne. 0) call errorexit('YOU MUST ENTER A'//
     &		  ' FIXED VIEW WITH THIS TILT OPTION', 0)
	    else
	      write(*,'(1x,a,$)')'Number of view to fix tilt angle for: '
	      read(5,*)nviewfix
	    endif
	    nviewfix=nearest_view(nviewfix)
	    maplist(nviewfix)=0
	  endif
	elseif(ioptilt.eq.4)then
	  if (pipinput) then
	    call GetMapList('tilt', 'Tilt', 0, 0., ifpip, iflocal, nview,
     &		maplist)
	  else
	    print *,'For each view, enter 0 to fix the tilt angle',
     &		'   or the # of a tilt variable to map its tilt angle to'
	    read(5,*)(maplist(i),i=1,nview)
	  endif
	elseif(ioptilt.ge.5)then
	  if(ioptilt.gt.6)then
	    if (pipinput) then
	      if (PipGetInteger(PrependLocal('TiltSecondFixedView', iflocal),
     &		  nviewfix) .ne. 0) call errorexit('YOU MUST ENTER A'//
     &		  ' SECOND FIXED VIEW WITH THIS TILT OPTION', 0)
	    else
	      write(*,'(1x,a,$)')'Second view to fix tilt angle for: '
	      read(5,*)nviewfix
	    endif
	    nviewfix=nearest_view(nviewfix)
	  endif
c	    
c	    automap: get map, then set variable # 0 for ones mapped to 
c	    minimum tilt
c	    
	  power=0.
	  if(ioptilt.eq.5.or.ioptilt.eq.7)then
	    iflin=1
	    power=powertilt
	  endif
	  call setgrpsize(tilt,nview,power,grpsize)
	  call automap(nview,maplist,grpsize,mapfiletoview,nfileviews
     &		,ifpip, 1, PrependLocal('TiltDefaultGrouping', iflocal),
     &		PrependLocal('TiltNondefaultGroup', iflocal))
	  if (.not.pipinput)write(6,111)(maplist(i),i=1,nview)
111	  format(/,' Variable mapping list:',(1x,25i3))
	endif
c	  
c	  analyze map list
c	  
	iref1=imintilt
	iref2=nviewfix
	if(iflocal.gt.1.and.ioptilt.ne.0)then
	  iref1=0
	  iref2=0
	endif
	call analyze_maps(tilt,maptilt,lintilt,frctilt,fixedtilt,
     &	    fixedtilt2,iflin, maplist,nview, iref1,iref2,-999.,
     &	    'tilt',var,varname,nvarsrch,mapviewtofile)
c	  
c	  set tiltinc so it will give back the right tilt for whatever
c	  mapping or interpolation scheme is used
c
	do iv=1,nview
	  tiltinc(iv)=0.
	  if(maptilt(iv).ne.0)then
	    if(lintilt(iv).gt.0)then
	      tiltinc(iv)=tilt(iv)-(frctilt(iv)*var(maptilt(iv))+
     &		  (1.-frctilt(iv))*var(lintilt(iv)))
	    elseif(lintilt(iv).eq.-1)then
	      tiltinc(iv)=tilt(iv)-(frctilt(iv)*var(maptilt(iv))+
     &		  (1.-frctilt(iv))* fixedtilt)
	    elseif(lintilt(iv).eq.-2)then
	      tiltinc(iv)=tilt(iv)-(frctilt(iv)*var(maptilt(iv))+
     &		  (1.-frctilt(iv))* fixedtilt2)
	    else
	      tiltinc(iv)=tilt(iv)-var(maptilt(iv))
	    endif
	  endif
	enddo
	nvarang=nvarsrch
c	  
c	  get reference view to fix magnification at 1.0
c	  
	ireftilt=mapviewtofile(imintilt)
	if (pipinput) then
	  ierr = PipGetInteger(PrependLocal('MagReferenceView', iflocal),
     &	      ireftilt)
	else
	  print *,
     &	      'Enter # of reference view to fix at magnification of 1.0'
	  write(*,'(1x,a,i3,a,$)')
     &	      '   [default =',ireftilt,', view at minimum tilt]: '
	  read(5,*)ireftilt
	endif
	ireftilt=nearest_view(ireftilt)
c	
c	  get type of mag variable set up
c	
	if (pipinput) then
	  ioptmag = 0
	  ierr = PipGetInteger(PrependLocal('MagOption', iflocal), ioptmag)
	else
	  print *,'Enter 0 to fix all magnifications at 1.0'
	  print *,'      1 to give each view an independent magnification'
	  write(*,'(1x,a,/,a,$)')'      2 to specify other mappings of'//
     &	      ' magnification variables',
     &	      '       3 or 4 for automatic mapping of groups of views '//
     &	      '(3 linear, 4 block): '
	  read(5,*)ioptmag
	endif
c	  
c	  set up appropriate mapping list or read it in
c
	iflin=0
	if(ioptmag.le.0)then
	  do i=1,nview
	    maplist(i)=0
	  enddo
	elseif(ioptmag.eq.1)then
	  do i=1,nview
	    maplist(i)=i
	  enddo
	elseif(ioptmag.eq.2)then
	  call GetMapList('magnification', 'Mag', mapviewtofile(ireftilt),
     &	      1.0, ifpip, iflocal, nview, maplist)
	else
	  power=0.
	  if(ioptmag.eq.3)then
	    iflin=1
	    power=powermag
	  endif
	  call setgrpsize(tilt,nview,power,grpsize)
	  call automap(nview,maplist,grpsize,mapfiletoview,nfileviews
     &		,ifpip, 1, PrependLocal('MagDefaultGrouping', iflocal),
     &		PrependLocal('MagNondefaultGroup', iflocal))
	  if (.not.pipinput) write(6,111)(maplist(i),i=1,nview)
	endif
c	  
c	  analyze map list
c	  
	iref1=ireftilt
	if(iflocal.gt.1.and.ioptmag.ne.0)iref1=0
	call analyze_maps(gmag,mapgmag,lingmag,frcgmag,fixedgmag,fixdum,
     &	    iflin, maplist,nview, iref1,0,1.,'mag ',var,varname,
     &	    nvarsrch,mapviewtofile)
c
c	  set up compression variables if desired: the ones at fixed zero tilt
c	  and at least one other one must have compression of 1.0
c	  get reference view to fix compression at 1.0
c	  
	irefcomp=0
	if(iflocal.eq.0)then
	  if (pipinput) then
	    ioptcomp = 0
	    ierr = PipGetInteger('CompReferenceView', irefcomp)
	    ierr = PipGetInteger('CompOption', ioptcomp)
	    if (ioptcomp .eq. 0) irefcomp = 0
	  else
	    write(*,'(1x,a,/,a,$)')'Enter # of reference view to fix at'//
     &		' compression of 1.0,','   or 0 for no compression: '
	    read(5,*)irefcomp
	  endif
	endif
	iflin=0
	if(irefcomp.le.0)then
	  do i=1,nview
	    maplist(i)=0
	  enddo
	  irefcomp=1
	else
	  irefcomp=nearest_view(irefcomp)
c	
c	    get type of comp variable set up
c	    
	  if (.not.pipinput) then
	    print *,'Enter 1 to give each view an independent',
     &		' compression'
	    write(*,'(1x,a,/,a,$)')'      2 to specify other mappings of'
     &		//' compression variables',
     &		'       3 or 4 for automatic mapping of groups of views '//
     &		'(3 linear, 4 block): '
	    read(5,*)ioptcomp
	  endif
c	  
c	    set up appropriate mapping list or read it in
c
	  if(ioptcomp.eq.1)then
	    do i=1,nview
	      maplist(i)=i
	    enddo
	  elseif(ioptcomp.eq.2)then
	    call GetMapList('compression', 'Comp', mapviewtofile(irefcomp),
     &		1., ifpip, 0, nview, maplist)
	  else
	    power=0.
	    if(ioptcomp.eq.3)then
	      iflin=1
	      power=powercomp
	    endif
	    call setgrpsize(tilt,nview,power,grpsize)
	    call automap(nview,maplist,grpsize,mapfiletoview,nfileviews
     &		,ifpip, 1, PrependLocal('CompDefaultGrouping', iflocal),
     &		PrependLocal('CompNondefaultGroup', iflocal))
	    if (.not.pipinput) write(6,111)(maplist(i),i=1,nview)
	  endif
	endif
c		
c	  if view has tilt fixed at zero, see if mapped to any other
c	  view not fixed at zero tilt; if not, need to fix at 1.0
c	  
	do iv=1,nview
	  if(tilt(iv).eq.0..and.maptilt(iv).eq.0)then
	    iffix=1
	    do jv=1,nview
	      if(maplist(iv).eq.maplist(jv).and.
     &		  (tilt(jv).ne.0..or.maptilt(jv).ne.0))iffix=0
	    enddo
	  endif
	  if(iffix.eq.1)maplist(iv)=maplist(irefcomp)
	enddo
c	  
c	  analyze map list
c
	nvartmp=nvarsrch
	call analyze_maps(comp,mapcomp,lincomp,frccomp,fixedcomp,fixdum,
     &	    iflin, maplist,nview, irefcomp,0,1.,'comp',var,varname,
     &	    nvarsrch,mapviewtofile)
c	  
	ncompsrch=nvarsrch-nvartmp
c	  get type of distortion variable set up
c	
	if (pipinput) then
	  ioptdmag = 0
	  ioptskew = 0
	  ierr = PipGetInteger(PrependLocal('XStretchOption', iflocal),
     &	      ioptdmag)
	  ierr = PipGetInteger(PrependLocal('SkewOption', iflocal), ioptskew)
	  if (ioptskew .gt. 0 .or. ioptdmag .gt. 0)ioptdel = 2
	else
	  print *,'Enter 0 to fix all lateral distortions at 0.0'
	  print *,'      1 to solve for distortion with the same '//
     &	      'mappings for stretch and skew'
	  write(*,'(1x,a,$)')'      2 to find distortion using '//
     &	      'different mappings for stretch and skew: '
	  read(5,*)ioptdel
	endif
c	  
c	  get reference and dummy views for distortion: take reference on the
c	  same side of minimum as the mag reference, but avoid the minimum tilt
c	  
	irefdmag=max(1,nview/4)
	ivdum=max(1,3*nview/4)
	if(irefdmag.eq.imintilt .or.
     &	    (ireftilt.gt.imintilt.and.ivdum.ne.imintilt))then
	  irefdmag=ivdum
	  ivdum=max(1,nview/4)
	endif
c
	power=powerdmag
	do idist =1,2
	  iflin=0
	  if(ioptdel.le.0 .or. (pipinput .and.
     &	      ((idist .eq. 1 .and. ioptdmag .le. 0) .or.
     &	      (idist .eq. 2 .and. ioptskew .le. 0)))) then
	    do i=1,nview
	      maplist(i)=0
	    enddo
	  else
	    distmp=disttext(idist*min(1,ioptdel-1)+1)
	    lintmp=lintext(idist)
	    distOptTmp = distOptText(idist)
	    lenOpt = lnblnk(distOptTmp)
	    if(idist.eq.1.or.ioptdel.gt.1)then
	      if (pipinput) then
		ioptdist = ioptdmag
		if (idist .eq. 2) ioptdist = ioptskew
	      else
		print *,'Enter 1 to give each view an independent ',distmp
		write(*,'(1x,a,/,a,$)')'      2 to specify other mappings'
     &		    //' of '//distmp//' variables',
     &		    '       3 or 4 for automatic mapping of groups of '//
     &		    'views ('//lintmp//'): '
		read(5,*)ioptdist
	      endif
c	  
c		set up appropriate mapping list or read it in
c		
	      if(ioptdist.le.1)then
		do i=1,nview
		  maplist(i)=i
		enddo
	      elseif(ioptdist.eq.2)then
		call GetMapList(distmp, distOptTmp, mapviewtofile(ireftilt),
     &		    0., ifpip, iflocal, nview, maplist)
	      else
		if(idist.eq.1)then
		  if(ioptdist.gt.3) iflin=1
		  if(ioptdist.lt.3) power=1.
		else
		  if(ioptdist.eq.3) then
		    iflin=1
		  else
		    power=1.
		  endif
		endif
		call setgrpsize(tilt,nview,power,grpsize)
		call automap(nview,maplist,grpsize,mapfiletoview,nfileviews,
     &		    ifpip, 1, PrependLocal(concat(distOptTmp(1:lenOpt),
     &		    'DefaultGrouping'), iflocal), PrependLocal(concat(
     &		    distOptTmp(1:lenOpt), 'NondefaultGroup'), iflocal))
		if (.not.pipinput) write(6,111)(maplist(i),i=1,nview)
	      endif
	    endif
	  endif
	  iref1=irefdmag
	  if(iflocal.gt.1.and.ioptdist.ne.0)iref1=0
	  if(idist.eq.1)then
c
c	      analyze map list
c	      
	    mapdmagstart=nvarsrch+1
	    call analyze_maps(dmag,mapdmag,lindmag,frcdmag,fixeddmag,
     &		fixdum,iflin, maplist,nview, iref1,0,0.,'dmag',var,
     &		varname,nvarsrch,mapviewtofile)
	    mapdumdmag=mapdmagstart-2
c	      
c	      provided there are at least two dmag variables, turn 
c	      one into a dummy - try for one 1/4 of the way through the views
c
	    ndmagvar=nvarsrch+1-mapdmagstart
c	    if(ndmagvar.gt.1.and.iflocal.le.1)then
	    if(ndmagvar.gt.1.and.iflocal.le.-1)then
	      ivl=ivdum
	      ivh=ivl+1
	      do while(ivl.gt.0.and.ivh.le.nview.and.
     &		  mapdumdmag.lt.mapdmagstart)
		if(ivl.gt.0)then
		  if(mapdmag(ivl).ne.0)mapdumdmag=mapdmag(ivl)
		  ivl=ivl-1
		endif
		if(ivh.le.nview.and.mapdumdmag.lt.mapdmagstart)then
		  if(mapdmag(ivh).ne.0)mapdumdmag=mapdmag(ivh)
		  ivh=ivh+1
		endif
	      enddo
c		
c		pack down the variable list in case they mean anything
c
	      do i=mapdumdmag,nvarsrch-1
		var(i)=var(i+1)
		varname(i)=varname(i+1)
	      enddo
c		
c		if a variable is mapped to the dummy, change its mapping
c		to the endpoint; if it mapped past there, drop it by one to
c		pack the real variables down
c
	      do i=1,nview
		if(mapdmag(i).eq.mapdumdmag)then
		  mapdmag(i)=nvarsrch
		elseif(mapdmag(i).gt.mapdumdmag)then
		  mapdmag(i)=mapdmag(i)-1
		endif
		if(lindmag(i).eq.mapdumdmag)then
		  lindmag(i)=nvarsrch
		elseif(lindmag(i).gt.mapdumdmag)then
		  lindmag(i)=lindmag(i)-1
		endif
	      enddo
c		
c		now set the dummy variable to the end of list and eliminate
c		that from the list
c
	      mapdumdmag=nvarsrch
c	      dumdmagfac=1./(ndmagvar-1.)
	      dumdmagfac=-1.
	      nvarsrch=nvarsrch-1
	    endif
	    nvarscl=nvarsrch
	  else
	    call analyze_maps(skew,mapskew,linskew,frcskew,fixedskew,
     &		fixdum,iflin, maplist,nview, iref1,0,0.,'skew',var,
     &		varname,nvarsrch,mapviewtofile)
	  endif
	  power=powerskew
	enddo
c	  
c	  get type of alpha variable set up
c	  
	ioptalf=0
	if (pipinput) then
	  ierr = PipGetInteger(PrependLocal('XTiltOption', iflocal), ioptalf)
	else if (inputalf.ne.0) then
	  print *,'Enter 0 to fix all X-axis tilts at 0.0'
	  print *,'      1 to give each view an independent X-axis tilt'
	  write(*,'(1x,a,/,a,$)')'      2 to specify other mappings of'
     &	      //' X-axis tilt variables',
     &	      '       3 or 4 for automatic mapping of groups of views '
     &	      //'(3 linear, 4 block): '
	  read(5,*)ioptalf
	endif
c	  
c	  set up appropriate mapping list or read it in
c
	ifanyalf=ioptalf
	iflin=0
	if(ioptalf.le.0)then
	  do i=1,nview
	    maplist(i)=0
	  enddo
	elseif(ioptalf.eq.1)then
	  do i=1,nview
	    maplist(i)=i
	  enddo
	elseif(ioptalf.eq.2)then
	  call GetMapList('X-axis tilt', 'XTilt', mapviewtofile(ireftilt),
     &	      0., ifpip, iflocal, nview, maplist)
	else
	  power=0.
	  if(ioptalf.eq.3)then
	    iflin=1
	    power=poweralf
	  endif
	  call setgrpsize(tilt,nview,power,grpsize)
	  call automap(nview,maplist,grpsize,mapfiletoview,nfileviews
     &		,ifpip, 1, PrependLocal('XTiltDefaultGrouping', iflocal),
     &		PrependLocal('XTiltNondefaultGroup', iflocal))
	  if (.not.pipinput) write(6,111)(maplist(i),i=1,nview)
	endif
c	  
c	  analyze map list - fix reference at minimum tilt
c	  
	mapalfstart=nvarsrch+1
	call analyze_maps(alf,mapalf,linalf,frcalf,fixedalf,fixdum,iflin,
     &	    maplist,nview, imintilt,0,0.,'Xtlt',var,varname,nvarsrch,
     &	    mapviewtofile)
c	  
c	  Add projection stretch variable if desired.
c	  
	mapProjStretch = 0
	if (iflocal .eq. 0)then
	  projStretch = 0.
	  projSkew = 0.
	  if (pipinput) then
	    ierr = PipGetBoolean('ProjectionStretch', mapProjStretch)
	  endif
	  if (mapProjStretch .gt. 0) then
	    mapProjStretch = nvarsrch + 1
	    nvarsrch = nvarsrch + 2
	    varname(mapProjStretch) = 'projstr '
	    varname(mapProjStretch + 1) = 'projskew'
	  endif
	endif
c
	if(iflocal.lt.2.and.ncompsrch.ne.0.and.
     &	    (ioptilt.eq.1.or.(ioptilt.gt.1.and.nviewfix.eq.0)))
     &	    write(*,109)
109	format(/,' WARNING: ONLY ONE TILT ANGLE IS FIXED',/,
     &	    ' THERE SHOULD BE TWO FIXED TILT ANGLES WHEN DOING ',
     &	    'COMPRESSION')
	if(ioptalf.ne.0.and.ifrotfix.ne.-1.and.ifrotfix.ne.-2)write(*,110)
110	format(/,' WARNING: YOU ARE ATTEMPTING TO SOLVE FOR X-AXIS',
     &	    ' TILTS AND MORE THAN ONE',/,' ROTATION ANGLE; ',
     &	    'RESULTS WILL BE VERY UNRELIABLE')
	if(inputalf.eq.0 .and. ifanyalf.eq.0)then
	  write(*,101)
101	  format(/,24x,'Variable mappings',/,' View  Rotation     Tilt',
     &	      '  (+ incr.)      Mag       Comp      Dmag      Skew')
	else
	  write(*,1101)
1101	  format(/,24x,'Variable mappings',/,' View Rotation    Tilt ',
     &	      ' (+ incr.)   Mag      Comp     Dmag      Skew    X-tilt')
	endif
	do iv=1,nview
	  do i=1,8
	    dump(i)='  fixed '
	  enddo
	  dump(3)='        '
	  call setdumpname(maprot(iv),linrot(iv),-1,varname,dump(1))
	  if(maptilt(iv).gt.0)then
	    call setdumpname(maptilt(iv),lintilt(iv),-1,varname,dump(2))
	    if(abs(tiltinc(iv)).gt.5.e-3.and.
     &		(iflocal.eq.0.or.incrtilt.eq.0))
     &		write(dump(3),'(a2,f6.2)') '+ ',tiltinc(iv)/dtor
	  endif
	  call setdumpname(mapgmag(iv),lingmag(iv),-1,varname,dump(4))
	  call setdumpname(mapcomp(iv),lincomp(iv),-1,varname,dump(5))
	  call setdumpname(mapdmag(iv),lindmag(iv),mapdumdmag,varname,
     &	      dump(6))
	  call setdumpname(mapskew(iv),linskew(iv),-1,varname,dump(7))
	  call setdumpname(mapalf(iv),linalf(iv),-1,varname,dump(8))
	  if(inputalf.eq.0 .and. ifanyalf.eq.0)then
	    write(*,'(i4,3x,a8,3x,a8,1x,a8,3x,a8,3x,a8,3x,a8,3x,a8)')
     &		mapviewtofile(iv),(dump(i),i=1,7)
	  else
	    write(*,'(i4,2x,a8,2x,a8,a7,2x,a8,1x,a8,1x,a8,2x,a8,2x,a8)')
     &		mapviewtofile(iv),(dump(i),i=1,8)
	  endif
	enddo
	write(*,*)
	return
	end

	subroutine setdumpname(map,lin,mapdum,varname,dump)
	implicit none
	character*8 varname(*), dump
	integer*4 map,mapdum,lin
	if(map.eq.0)return
	if(map.eq.mapdum)then
	  if(lin.eq.0)then
	    dump='dummy'
	  elseif(lin.lt.0)then
	    dump='dum+fix'
	  else
	    dump='dum+'//varname(lin)(1:1)//varname(lin)(6:8)
	  endif
	else
	  if(lin.eq.0)then
	    dump=varname(map)
	  elseif(lin.lt.0)then
	    dump=varname(map)(1:1)//varname(map)(6:8)//'+fix'
	  elseif(lin.eq.mapdum)then
	    dump=varname(map)(1:1)//varname(map)(6:8)//'+dum'
	  else
	    dump=varname(map)(1:1)//varname(map)(6:8)//'+'//
     &		varname(lin)(6:8)
	  endif
	endif
	return
	end


	subroutine reload_vars(glb,vlocal,map,frc,nview,
     &	    mapstart,mapend,var,fixed,incr)
	implicit none
	integer*4 map(*),nview,mapstart,mapend,incr
	real*4 glb(*),frc(*),var(*),vlocal(*),fixed
c	  
	integer*4 i,m,nsum
	real*4 sum
c	  
c	  first just reload all of the local parameters from the global
c	  fixed will have the right value for any case where there is ONE
c	  fixed parameter
c
	do i=1,nview
	  vlocal(i)=glb(i)
	  if(map(i).eq.0)fixed=glb(i)
	enddo
c	  
c	  if doing incremental, set all the var's to 0 and return
c	  
	if(incr.ne.0)then
	  do m=mapstart,mapend
	    var(m)=0.
	  enddo
	  fixed=0.
	  return
	endif
c	  
c	  next set the value of each variable as the average value of all
c	  the parameters that map solely to it
c	  
	do m=mapstart,mapend
	  sum=0.
	  nsum=0
	  do i=1,nview
	    if(map(i).eq.m.and.frc(i).eq.1.)then
	      sum=sum+glb(i)
	      nsum=nsum+1
	    endif
	  enddo
	  if(nsum.eq.0)then
	    print *
	    print *,'ERROR: TILTALIGN - Nothing mapped to variable',m
	    call exit(1)
	  endif
	  var(m)=sum/nsum
	enddo
	return
	end


c	  It turned out that entered views always should be converted to
c	  the nearest view rather than generating an error
c$$$	subroutine filetoview(iview)
c$$$	implicit none
c$$$	integer*4 iview
c$$$	include 'alivar.inc'
c$$$	if(iview.gt.nfileviews.or.iview.le.0)then
c$$$	  print *,'View',iview,' is beyond the known range of the file'
c$$$	  call exit(1)
c$$$	endif
c$$$	if(mapfiletoview(iview).eq.0)then
c$$$	  print *,'View',iview,' has not been included in this analysis'
c$$$	  call exit(1)
c$$$	endif
c$$$	iview=mapfiletoview(iview)
c$$$	return
c$$$	end


	integer*4 function nearest_view(iview)
	implicit none
	integer*4 iview,ivdel
	include 'alivar.inc'
	if(iview.gt.nfileviews.or.iview.le.0)then
	  print *,'View',iview,' is beyond the known range of the file'
	  call exit(1)
	endif
	nearest_view=mapfiletoview(iview)
	ivdel=1
	do while(nearest_view.eq.0)
	  if(iview-ivdel.gt.0)then
	    if(mapfiletoview(iview-ivdel).gt.0)
     &		nearest_view=mapfiletoview(iview-ivdel)
	  endif
	  if(iview+ivdel.le.nfileviews)then
	    if(mapfiletoview(iview+ivdel).gt.0)
     &		nearest_view=mapfiletoview(iview+ivdel)
	  endif
	  ivdel=ivdel+1
	enddo
	return
	end

	character*40 function PrependLocal(string, iflocal)
	implicit none
	character*(*) string
	integer*4 iflocal
	character*80 concat
	prependLocal = string
	if (iflocal.ne.0) prependlocal = concat('Local', string)
	return
	end

	subroutine GetMaplist(varname, option, iref, fixval, ifpip, iflocal,
     &	    nview, maplist)
	implicit none
	character*(*) varname,option
	integer*4 maplist(*)
	integer*4 iflocal,ifpip,nview,i, iref, len, numEntry, numTot, numGot
	real*4 fixval
	character*40 mapOption
	character*80 concat
	character*40 PrependLocal
	integer*4 PipNumberOfEntries, lnblnk, PipGetIntegerArray

	if (ifpip .eq. 0) then
	  len = lnblnk(varname)
	  print *,'For each view, enter a ',varname(1:len),' variable #'
	  if (iref .gt. 0) write(*,'(a,a,a,f4.1,a,i4)')'   to fix ',
     &	      varname(1:len),' of a view at'
     &	      ,fixval,' give it the same variable # as view',iref
	  read(5,*)(maplist(i),i=1,nview)
	else
	  mapOption = PrependLocal(concat(option(1:lnblnk(option)),'Mapping'),
     &	      iflocal)
	  numEntry = 0
	  len = PipNumberOfEntries(mapOption, numEntry)
	  numTot = 0
	  do i = 1,numEntry
	    numGot = 0
	    len = PipGetIntegerArray(mapOption, maplist(numTot + 1),
     &		numGot, nview-numTot)
	    numTot = numTot + numGot
	  enddo
	  if (numTot .lt. nview) call errorexit(
     &	      'NOT ENOUGH MAPPING VALUES ENTERED WITH '//mapOption, 0)
	endif
	return
	end
	  

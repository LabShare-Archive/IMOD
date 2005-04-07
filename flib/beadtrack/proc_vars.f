c	  INPUT_VARS gets the specifications for the geometric variables
c	  rotation, tilt, mag, etc.  It fills the VAR array and
c	  the different variable and mapping arrays.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2004/10/24 22:33:59  mast
c	  Chnages for projection stretch variables and new rotation stuff (?)
c	
c
	subroutine proc_vars(ifmaptilt,ireftilt,var, nvarsrch)
c
	implicit none
	include 'alivar.inc'
	include 'tltcntrl.inc'

	real*4 var(*)
	integer*4 ifmaptilt
	integer*4 maplist(maxview),ireftilt,nvarsrch
	real*4 rotstart,defrot,power,fixdum,grpsize(maxview)
	integer*4 i,iview,iv,nvarang,iflin,iref1,maptest
	real*4 dtor/0.0174532/
	character*8 varname(maxview)
c	  
	integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),ngsep
	common /mapsep/ ivsep,nsepingrp,ngsep
c	  
c
c	  Remap the separate group information for the current views
c	  
	do iv = 1, ngsep
	  nsepingrp(iv) = nsepInGrpIn(iv)
	  do i = 1, nsepingrp(iv)
	    ivsep(i,iv) = ivSepIn(i,iv)
	  enddo
	  call mapSeparateGroup(ivsep(1,iv), nsepingrp(iv), mapFileToView,
     &	      nFileViews)
	enddo
c
c	  Use the grouping to get the mapping
c
	iflin = 1
	call setgrpsize(tilt,nview,0.,grpsize)
	call makeMapList(nview,maplist,grpsize,mapfiletoview,nfileviews,
     &	    nmapRot, ivSpecStrRot, ivSpecEndRot,nmapSpecRot,nRanSpecRot)
c
c	  If one variable is fixed, that is the reference variable, but if
c	  not, reserve the first variable and set up first rotation as
c	  reference for the mapping
c
	nvarsrch=0
	iref1 = ifrotfix
	defrot = 0.
	if(ifrotfix.eq.0)then
	  iref1 = 1
	  nvarsrch=1
	endif
c	  
c	  get the variables allocated
c
        call analyze_maps(rot,maprot,linrot,frcrot,fixedrot,fixdum,
     &      iflin, maplist,nview, iref1,0,defrot,'rot ',var,varname,
     &      nvarsrch,mapviewtofile)
c	  
c	  reload the old rotation values
c
	do iv = 1, nview
	  rot(iv) = rotorig(mapViewToFile(iv))*dtor
	enddo
c	  
c	  Fix global variable 1 and anything in its block
c	  
	if (ifrotfix .eq. 0) then
	  maptest = 1
	  rotstart = rot(1)
	  var(1) = rot(1)
	  do i = 1, nview
	    if (maprot(i) .eq. 0) then
              rot(i) = rotstart
              maprot(i) = 1
            endif
          enddo
	else
c	    
c	    Otherwise, fix ifrotfix which moves with linear mapping
c	    
	  maptest = 0
	  do i = 1, nview
            if (maprot(i) .eq. 0) then
              ifrotfix = i
              rotstart = rot(i)
            endif
          enddo
        endif
c	  
c	  Set up var with increments from current values by looking at all
c	  mappings but maptest that are not linear combos
c
	do i = 1, nview
	  if (maprot(i) .ne. maptest .and. maprot(i).gt.0 .and. linrot(i).eq.0)
     &	      var(maprot(i)) = rot(i) - rotstart
	enddo
c
c	  set up for fixed tilt angles
c
	do i=1,nview
	  maplist(i)=0
	enddo
	iflin = 0
	power = 0.
	if (ifmaptilt .ne. 0) then
	  if (nmapTilt .gt. 1) then
	    iflin = 1
	    power = 1.
	  endif
	  call setgrpsize(tilt,nview,power,grpsize)
	  call makeMapList(nview,maplist,grpsize,mapFileToView,nFileViews,
     &	      nmapTilt, ivSpecStrTilt, ivSpecEndTilt,nmapSpecTilt,
     &	      nRanSpecTilt)
	endif
c	  
        call analyze_maps(tilt,maptilt,lintilt,frctilt,fixedtilt,
     &      fixedtilt2,iflin, maplist,nview, ireftilt,0,-999.,
     &      'tilt',var,varname,nvarsrch,mapViewToFile)
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
c	  
c	  Now reload the previous values and put them into vars
c
	do i=1,nview
	  if (maptilt(i).gt.0) tilt(i) = dtor * tiltorig(mapViewToFile(i))
	  if (maptilt(i) .gt. 0 .and. lintilt(i) .eq. 0)
     &	      var(maptilt(i)) = tilt(i)
	enddo
	nvarang=nvarsrch
c	  
c	  Set up the mag maplist
c
	iflin = 1
	call setgrpsize(tilt,nview,0.,grpsize)
	call makeMapList(nview,maplist,grpsize,mapfiletoview,nfileviews,
     &	    nmapMag, ivSpecStrMag, ivSpecEndMag,nmapSpecMag,nRanSpecMag)
c	  
c	  Set up mag variables
c
	call analyze_maps(gmag,mapgmag,lingmag,frcgmag,fixedgmag,fixdum,
     &	    iflin, maplist,nview, ireftilt,0,1.,'mag ',var,varname,
     &	    nvarsrch,mapviewtofile)
c	  
c	  Reload values and stuff them into vars by looking at all mappings
c	  that are not linear combos
c
	do i=1,nview
	  gmag(i)=gmagorig(mapViewToFile(i))
	  if (mapgmag(i) .gt. 0 .and. lingmag(i) .eq. 0)
     &	      var(mapgmag(i)) = gmag(i)
	enddo
c
c	  This should be done once before calling
c
	mapProjStretch = 0
	projStretch = 0.
	projSkew = 0.
	return
	end

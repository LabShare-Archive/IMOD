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
c
	subroutine proc_vars(rotorig,tiltorig,gmagorig,ivorig,
     &	    mapalltilt ,mapallmag,ifmaptilt,imintilt,var,
     &	    nvarsrch)
c
	include 'alivar.inc'

	real*4 var(*),rotorig(*),tiltorig(*),gmagorig(*)
	integer*4 mapalltilt(*),mapallmag(*),ivorig(*)
	integer*4 mapvarno(maxview),maplist(maxview)
c
	data dtor/0.0174532/
c
c	  set up the first search variable as a global rotation angle
c	  and the rest as delta rotation angles of each view after first
c	  or, if one is fixed, set up all others as delta from that one
c	  10/10/04: added appropriate mapping entries to work with new funct
c
	nvarsrch=0
	if(ifrotfix.eq.0)then
	  var(1)=rotorig(ivorig(1))*dtor
	  nvarsrch=1
	  rot(1)=var(1)
	  rotstart=rot(1)
	  maprot(1) = 1
	  linrot(1) = 0
	  frcrot(1) = 1.
	else
	  rotstart=rotorig(ivorig(ifrotfix))*dtor
	  rot(ifrotfix)=rotstart
	  maprot(ifrotfix) = 0
	endif
c
	do i=2,nview
	  iview=i
	  if(i.le.ifrotfix)iview=i-1
	  rot(iview)=rotorig(ivorig(iview))*dtor
	  nvarsrch=nvarsrch+1
	  var(nvarsrch)=rot(iview)-rotstart
	  maprot(iview) = nvarsrch
	  linrot(iview) = 0
	  frcrot(iview) = 1.
	enddo
	fixedrot = 0.
c
c	  set up for fixed tilt angles
c
	do i=1,nview
	  tilt(i)=tiltorig(ivorig(i))*dtor
	  tiltinc(i)=0.
	  maptilt(i)=0
	  maplist(i)=0
	enddo
c	    
c	    automap: get map, then set variable # 0 for ones mapped to 
c	    minimum tilt
c
	if(ifmaptilt.ne.0)then    
	  do i=1,nview
	    maplist(i)=mapalltilt(ivorig(i))
	  enddo
	  ivarfix=maplist(imintilt)
	  do i=1,nview
	    if(maplist(i).eq.ivarfix)maplist(i)=0
	  enddo
	endif
c	  
c	  analyze map list
c
	ntiltsrch=0
	do iv=1,nview
	  if(maplist(iv).ne.0)then
c
c	      if value is 0, already all set for a fixed value
c	      otherwise see if this var # is already encountered
c
	    imap=0
	    do it=1,ntiltsrch
	      if(maplist(iv).eq.mapvarno(it))imap=it
	    enddo
	    if(imap.eq.0)then
c		
c		if not, need to add another tilt variable
c		
	      ntiltsrch=ntiltsrch+1
	      mapvarno(ntiltsrch)=maplist(iv)
	      var(nvarsrch+ntiltsrch)=tilt(iv)
	      imap=ntiltsrch
	    endif
c	      
c	      now incrementally map the tilt angle to the variable
c
	    maptilt(iv)=nvarsrch+imap
	    tiltinc(iv)=tilt(iv)-var(nvarsrch+imap)
	  endif
	enddo
	nvarsrch=nvarsrch+ntiltsrch
	nvarang=nvarsrch
c	  
c	  set up mags by original map list
c
	ireftilt=imintilt
	do i=1,nview
	  gmag(i)=gmagorig(ivorig(i))
	  maplist(i)=mapallmag(ivorig(i))
	enddo
c	  
c	  analyze map list
c
	nmagsrch=0
	do iv=1,nview
	  mapgmag(iv)=0
	  if(maplist(iv).ne.maplist(ireftilt))then
c
c	      if value is same as for minimum tilt, all set for fixed mag
c	      otherwise see if this var # is already encountered
c
	    imap=0
	    do it=1,nmagsrch
	      if(maplist(iv).eq.mapvarno(it))imap=it
	    enddo
	    if(imap.eq.0)then
c		
c		if not, need to add another mag variable
c		
	      nmagsrch=nmagsrch+1
	      mapvarno(nmagsrch)=maplist(iv)
	      var(nvarsrch+nmagsrch)=gmag(iv)
	      imap=nmagsrch
	    endif
c	      
c	      now map the mag to the variable
c
	    mapgmag(iv)=nvarsrch+imap
	  endif
	enddo
	nvarsrch=nvarsrch+nmagsrch
c
c	  This should be done once before calling
c
	ncompsrch=0
	mapProjStretch = 0
	projStretch = 0.
	projSkew = 0.
	return
	end

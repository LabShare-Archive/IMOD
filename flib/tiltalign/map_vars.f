	subroutine analyze_maps(gmag,mapgmag,lingmag,frcgmag,fixedgmag,
     &	    fixedgmag2,iflin,maplist,nview,ireftilt,iref2,
     &	    defval,name,var,varname,nvarsrch)
	integer*4 mapgmag(*),maplist(*),lingmag(*),mapvarno(360)
	real*4 gmag(*),var(*),frcgmag(*)
	character*(*) varname(*),name
c
	nmagsrch=0
	do iv=1,nview
	  if(defval.ne.-999.)gmag(iv)=defval
	enddo
	mapref1=-999
	if(ireftilt.gt.0)mapref1=maplist(ireftilt)
	mapref2=-999
	if(iref2.gt.0)mapref2=maplist(iref2)
	if(iflin.le.0)then
	  do iv=1,nview
	    mapgmag(iv)=0
	    lingmag(iv)=0
	    frcgmag(iv)=1.
	    if(maplist(iv).ne.mapref1.and.maplist(iv).ne.mapref2)then
c		
c		if value is same as for minimum tilt, all set for fixed mag
c		otherwise see if this var # is already encountered
c
	      imap=0
	      do it=1,nmagsrch
		if(maplist(iv).eq.mapvarno(it))imap=it
	      enddo
	      if(imap.eq.0)then
c		  
c		  if not, need to add another mag variable
c		
		nmagsrch=nmagsrch+1
		mapvarno(nmagsrch)=maplist(iv)
		var(nvarsrch+nmagsrch)=gmag(iv)
		write(varname(nvarsrch+nmagsrch),'(a4,i4)')name,iv
		imap=nmagsrch
	      endif
c	      
c		now map the mag to the variable
c		
	      mapgmag(iv)=nvarsrch+imap
	    endif
	  enddo
	else
c
c	    LINEAR MAPPING OPTION
c
	  listmax=0
	  do iv=1,nview
	    listmax=max(listmax,maplist(iv))
	  enddo
c	    
c	    go through the variable groupings
c
	  do imap=1,listmax
	    call groupminmax(maplist,nview,imap,ivmin,ivmax,ningrp)
c	      
c	      if any in this group:
c
	    if(ningrp.gt.0)then
	      call groupminmax(maplist,nview,imap+1,nexvmin,nexvmax,
     &		  ninnex)
c		
c		if any in next group, the min view in that group is the
c		endpoint for interpolation, otherwise the max view in this
c		group is the endpoint
c		
	      nadd=2
	      ivadd=ivmin
	      ivfix=0
	      linval=-1
	      if(ninnex.eq.0)then
		nexvmin=ivmax
c		  
c		  ending and only two in group, add only one
c
		if(ningrp.le.2)nadd=1
c		  
c		  but if this one is fixed group, add end to stack unless
c		  only 2, in which case add nothing
c
		if(imap.eq.mapref1.or.imap.eq.mapref2)then
		  mapgmag(ivmin)=0
		  ivadd=nexvmin
		  ivfix=ivmin
		  ivvar=nexvmin
		  if(ningrp.le.2)nadd=0
		  if(imap.eq.mapref2)linval=-2
		endif
	      else
c		
c		  if continuing and this one is fixed, add next start to list
c		
		if(imap.eq.mapref1.or.imap.eq.mapref2)then
		  mapgmag(ivmin)=0
		  ivadd=nexvmin
c		    
c		    but if next one is fixed also, use this one's end instead
c
		  if(imap+1.eq.mapref1.or.imap+1.eq.mapref2)ivadd=ivmax
		  nadd=1
		  ivfix=ivmin
		  ivvar=ivadd
		  if(imap.eq.mapref2)linval=-2
c		
c		  if continuing and next one is fixed, add this start to list
c		
		elseif(imap+1.eq.mapref1.or.imap+1.eq.mapref2)then
		  nadd=1
		  ivfix=nexvmin
		  ivvar=ivmin
		  if(imap+1.eq.mapref2)linval=-2
		endif
	      endif
c		
c		add the starting and ending views for interpolation as
c		variables, if they are not already on the list
c
	      do iadd=1,nadd
		if(nmagsrch.eq.0.or.lastadded.ne.ivadd)then
		  nmagsrch=nmagsrch+1
		  var(nvarsrch+nmagsrch)=gmag(ivadd)
		  write(varname(nvarsrch+nmagsrch),'(a4,i4)')name,ivadd
		  lastadded=ivadd
		  mapgmag(ivadd)=nvarsrch+nmagsrch
		  lingmag(ivadd)=0
		  frcgmag(ivadd)=1.
		endif
		ivadd=nexvmin
	      enddo
c
c
c		now go through rest of views (excluding endpoints), and for
c		each one in the group, map to next to last variable and set
c		fraction to the fraction of the distance in view number from
c		the starting to the ending point
c		
	      if(ivfix.eq.0.and.(ninnex.gt.0.or.ningrp.gt.2))then
		do iv=ivmin+1,ivmax
		  if(iv.ne.nexvmin.and.maplist(iv).eq.imap)then
		    mapgmag(iv)=mapgmag(ivmin)
		    lingmag(iv)=mapgmag(nexvmin)
		    frcgmag(iv)=float(nexvmin-iv)/float(nexvmin-ivmin)
		  endif
		enddo
	      elseif(ivfix.eq.0)then
c		  
c		  ending, not fixed, and only two in group: map 2nd to first
c		  
		mapgmag(ivmax)=mapgmag(ivmin)
		lingmag(ivmax)=0
		frcgmag(ivmax)=1.
	      elseif(ninnex.eq.0.and.ningrp.le.2)then
c		  
c		  ending in a fixed group of 2
c		  
		mapgmag(ivmax)=0
	      else
c		  
c		  linear ramp between fixed and variable
c
		do iv=ivmin,ivmax
		  if(iv.ne.ivfix.and.iv.ne.ivvar.and.maplist(iv)
     &		      .eq.imap)then
		    mapgmag(iv)=mapgmag(ivvar)
		    lingmag(iv)=linval
		    frcgmag(iv)=float(ivfix-iv)/float(ivfix-ivvar)
		  endif
		enddo
	      endif
c		
c		set the fixed value that is being ramped to
c
	      if(imap.eq.mapref1)fixedgmag=gmag(ivfix)
	      if(imap.eq.mapref2)fixedgmag2=gmag(ivfix)
	    endif
	  enddo
	  

	endif
	nvarsrch=nvarsrch+nmagsrch
	return
	end


	subroutine automap(nview,maplist,grpsize)
	integer*4 maplist(*)
	real*4 grpsize(*)
	parameter (maxview=490,maxgrp=20)
	integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),inran(maxview)
	integer*4 ivspecstr(maxgrp),ivspecend(maxgrp),nmapspec(maxgrp)
	common /mapsep/ ivsep,nsepingrp,ngsep
c	  
	print *,'Enter the negative of a group size to NOT treat ',
     &	    'any views separately here.'
	write(*,'(1x,a,$)')
     &	    'Default group size, # of ranges with special group sizes: '
	read(5,*)nmapdef,nranspec
	do iran=1,nranspec
	  write(*,'(1x,a,i3,a,$)')'Starting and ending views in range',
     &	      iran, ', group size: '
	  read(5,*)ivspecstr(iran),ivspecend(iran),nmapspec(iran)
	enddo
c	  
c	  build list of all uninterrupted ranges
c	  
	nran=nranspec+1
	ivspecstr(nran)=1
	ivspecend(nran)=nview
	nmapspec(nran)=nmapdef
	do iran=1,nranspec
	  ir=nranspec+1
c	    
c	    for each special range, scan rest of ranges and see if they overlap
c
	  do while (ir.le.nran)
	    if(.not.(ivspecstr(iran).gt.ivspecend(ir).or.
     &		ivspecend(iran).lt.ivspecstr(ir)))then
c		
c		overlap: then look at three cases
c
	      if(ivspecstr(iran).le.ivspecstr(ir).and.
     &		  ivspecend(iran).ge.ivspecend(ir))then
c		  
c		  CASE #1: complete overlap: wipe out the range from the list,
c		  move rest of list down
c		  
		nran=nran-1
		do ii=ir,nran
		  ivspecstr(ii)=ivspecstr(ii+1)
		  ivspecend(ii)=ivspecend(ii+1)
		  nmapspec(ii)=nmapspec(ii+1)
		enddo
	      elseif(ivspecstr(ir).lt.ivspecstr(iran).and.
     &		  ivspecend(ir).gt.ivspecend(iran))then
c		  
c		  CASE #2: interior subset; need to split range in 2
c		  
		nran=nran+1
		ivspecend(nran)=ivspecend(ir)
		ivspecstr(nran)=ivspecend(iran)+1
		nmapspec(nran)=nmapspec(ir)
		ivspecend(ir)=ivspecstr(iran)-1
	      else
c		  
c		  CASE #3, subset at one end, need to truncate range
c		  
		if(ivspecstr(iran).gt.ivspecstr(ir))then
		  ivspecend(ir)=ivspecstr(iran)-1
		else
		  ivspecstr(ir)=ivspecend(iran)+1
		endif
	      endif
	    endif
	    ir=ir+1
	  enddo
c	write(*,'(4i4)')(i,ivspecstr(i),ivspecend(i),nmapspec(i),i=1,
c     &	    nran)
	enddo
	ivar=1
c
c	  for each range, make list of views in range; exclude the special
c	  ones unless nmap is negative
c
	do iran=1,nran
	  ninran=0
	  do iv=ivspecstr(iran),ivspecend(iran)
	    ifsep=0
	    if(nmapspec(iran).gt.0)then
	      do ig=1,ngsep
		do jj=1,nsepingrp(ig)
		  if(iv.eq.ivsep(jj,ig))ifsep=1
		enddo
	      enddo
	    endif
	    if(ifsep.eq.0)then
	      ninran=ninran+1
	      inran(ninran)=iv
	    endif
	  enddo
c	    
c	    get the ones in range grouped; then group each special set
c
	  call grouplist(inran,ninran,abs(nmapspec(iran)),grpsize,ivar,
     &	      maplist)
	  if(nmapspec(iran).gt.0)then
	    do ig=1,ngsep
	      ninran=0
	      do jj=1,nsepingrp(ig)
		if(ivsep(jj,ig).ge.ivspecstr(iran).and.
     &		    ivsep(jj,ig).le.ivspecend(iran))then
		  ninran=ninran+1
		  inran(ninran)=ivsep(jj,ig)
		endif
	      enddo
	      call grouplist(inran,ninran,nmapspec(iran),grpsize,ivar,
     &		  maplist)
	    enddo
	  endif
	enddo
c	write(*,'(26i3)')(maplist(i),i=1,nview)
	return
	end



	subroutine grouplist(inran,ninran,nmap,grpsize,ivar,maplist)
	integer*4 inran(*),maplist(*)
	real*4 grpsize(*)
	if(ninran.eq.0)return

	setsum=0.
	do i=inran(1),inran(ninran)
	  setsum=setsum+1./(grpsize(i)*nmap)
	enddo
	nsets=max(1,nint(setsum))

	isetstr=inran(1)
	isetend=isetstr-1
	lastmap=isetstr
	setcum=0.
	do iset=1,nsets
c	    
c	    find the ending point that is at or before the next target sum
c	    for a set
c
	  settarg=(iset*setsum)/nsets
	  do while(isetend.lt.inran(ninran).and.
     &	      setcum+1./(grpsize(isetend+1)*nmap).le.settarg+0.01)
	    isetend=isetend+1
	    setcum=setcum+1./(grpsize(isetend)*nmap)
	  enddo
	  if(isetend.ge.isetstr)then
	    ninvar=0
	    do i=1,ninran
	      if(inran(i).ge.isetstr.and.inran(i).le.isetend)then
c		
c		  if it's a long way since last entry, skip a group number
c		
		if(inran(i)-lastmap.gt.nmap+1)ivar=ivar+1
		ninvar=ninvar+1
		maplist(inran(i))=ivar
		lastmap=inran(i)
	      endif
	    enddo
	    if(ninvar.gt.0)ivar=ivar+1
	    isetstr=isetend+1
	  endif
	enddo
c	  
c	  skip a group number at the end to prevent connections between sets
c
	ivar=ivar+1
	return
	end




	subroutine groupminmax(maplist,nview,imap,ivmin,ivmax,ningrp)
	integer*4 maplist(*)
	ivmin=0
	ivmax=0
	ningrp=0
	do iv=1,nview
	  if(maplist(iv).eq.imap)then
	    if(ivmin.eq.0)ivmin=iv
	    ivmax=iv
	    ningrp=ningrp+1
	  endif
	enddo
	return
	end



	subroutine setgrpsize(tilt,nview,power,grpsize)
	real*4 grpsize(*),tilt(*)
	if(power.eq.0.)then
	  do i=1,nview
	    grpsize(i)=1.
	  enddo
	else
	  sum=0.
	  do i=1,nview
	    grpsize(i)=cos(tilt(i))**power
	    sum=sum+grpsize(i)
	  enddo
	  do i=1,nview
	    grpsize(i)=nview*grpsize(i)/sum
	  enddo
	endif
	return
	end

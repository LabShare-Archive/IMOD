c***	  INPUT_MODEL reads in the model data, reads the header info from an
c	  image file, sorts out the model points that are to be included in
c	  the analysis, and converts the coordinates to "index" coordinates
c	  with the origin at the center of the section
c
	subroutine proc_model(xcen,ycen,xdelt,ydelt,xorig,yorig,scalexy,
     &	    nviewtot,mininview,ninview,ivorig,
     &	    xx,yy,isecview,maxprojpt,maxreal,irealstr,iobjali,nview,
     &	    nprojpt,nrealpt)
c
	real*4 xx(*),yy(*)
	integer*4 isecview(*),irealstr(*)
c
	include 'model.inc'
c
	integer*4 ninview(*),ivorig(*),iobjali(*)
c
c	  scan to get number of points on each view; eliminate consideration
c	  of views with not enough points, and objects with less than 2 points
c	  on the legal views; reiterate until all counted points come up above
c	  the minimum
c
	do i=1,nviewtot
	  ivorig(i)=2*mininview
	enddo
	needdo=1
c
	do while(needdo.eq.1)
	  do i=1,nviewtot
	    ninview(i)=0
	  enddo
	  do iobject=1,max_mod_obj
	    ninobj=npt_in_obj(iobject)
	    if(ninobj.gt.1)then
	      ibase=ibase_obj(iobject)
	      nlegal=0
	      do ipt=1,ninobj
		iz=nint(p_coord(3,object(ipt+ibase)))+1
		if(ivorig(iz).ge.mininview)nlegal=nlegal+1
	      enddo
	      if(nlegal.gt.1)then
		do ipt=1,ninobj
		  iz=nint(p_coord(3,object(ipt+ibase)))+1
		  if(ivorig(iz).ge.mininview)ninview(iz)=ninview(iz)+1
		enddo
	      endif
	    endif
	  enddo
	  needdo=0
	  do i=1,nviewtot
	    if(ninview(i).lt.mininview.and.ninview(i).gt.0)needdo=1
	    ivorig(i)=ninview(i)
	  enddo
	enddo
c	  
c	  find number of views with enough points, build cross-indexes
c	  
	nview=0
c	maxview=0
c	minview=nviewtot
	do i=1,nviewtot
	  if(ninview(i).ne.0)then
	    nview=nview+1
	    ninview(i)=nview
	    ivorig(nview)=i
c	    minview=min(minview,i)
c	    maxview=max(maxview,i)
	  endif
	enddo
c
c	  go through model finding objects with more than one point in the
c	  proper z range, and convert to index coordinates, origin at center
c
	nprojpt=0
	nrealpt=0
	do iobject=1,max_mod_obj		!loop on objects
	  ninobj=npt_in_obj(iobject)
	  ibase=ibase_obj(iobject)
	  if(ninobj.gt.1)then			!non-empty object
	    nlegal=0
	    do ipt=1,ninobj
	      iz=nint(p_coord(3,object(ipt+ibase)))+1
	      if(ninview(iz).gt.0)nlegal=nlegal+1
	    enddo
	    if(nlegal.gt.1)then
	      nrealpt=nrealpt+1
	      if(nrealpt.gt.maxreal)stop
     &		  'TOO MANY PROJECTION POINTS FOR ARRAYS' 
	      irealstr(nrealpt)=nprojpt+1
	      iobjali(nrealpt)=iobject
	      do ipt=1,ninobj			!loop on points
c
c		find out if the z coordinate of this point is on the list
c
		iz=nint(p_coord(3,object(ipt+ibase)))+1
		if(ninview(iz).gt.0)then
c
c		  if so, add index coordinates to list
c
		  nprojpt=nprojpt+1
		  if(nprojpt.gt.maxprojpt)stop
     &		      'TOO MANY PROJECTION POINTS FOR ARRAYS' 
		  xx(nprojpt)=((p_coord(1,object(ipt+ibase))+xorig)/
     &		      xdelt -xcen)/scalexy
		  yy(nprojpt)=((p_coord(2,object(ipt+ibase))+yorig)/
     &		      ydelt -ycen)/scalexy
		  isecview(nprojpt)=ninview(iz)
		endif
	      enddo
	    endif
	  endif
	enddo
	irealstr(nrealpt+1)=nprojpt+1		!for convenient looping
	return
	end

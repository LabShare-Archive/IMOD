C	FUNCTION INSIDE -- CONTAINMENT TEST
C
C		RETURNS .TRUE. IF POINT X,Y IS ON OR INSIDE POLYGON DEFINED 
C		BY VERTICES IN XVERT,YVERT
C		RETURNS .FALSE. IF POINT IS OUTSIDE
C		NP IS # VERTICES IN POLYGON
C
c	  New version by DNM, 8/23/00, replaces winding test with atan's.
c	  Based on algorithm in "Computational Geometry in C", Joseph O'Rourke,
c	  1998, with modifications to speed up search for ray crossings.
c
	logical function inside(xvert,yvert,np,x,y)
	real*4 xvert(*),yvert(*)
	logical rstrad, lstrad
	nrcross=0
	nlcross=0
	yp=yvert(np)
	inside=.true.
	j=1
	do while(j.le.np)
	  if(yp.lt.y)then
c	      
c	      if last point below y, search for first that is not below
c
	    do while(j.le.np.and.yvert(j).lt.y)
	      j=j+1
	    enddo
	  elseif(yp.gt.y)then
c	      
c	      or if last point above y, search for first that is not above
c
	    do while(j.le.np.and.yvert(j).gt.y)
	      j=j+1
	    enddo
	  endif
	  if(j.le.np)then
	    jl=j-1
	    if(jl.eq.0)jl=np
	    xp=xvert(jl)
	    yp=yvert(jl)
	    xc=xvert(j)
	    yc=yvert(j)
c	    
c	      return if point is a vertex
c
	    if((x.eq.xc.and.y.eq.yc))then
c	      print *,'vertex'
	      return
	    endif
c	    
c	      does edge straddle the ray to the right or the left?
c
	    rstrad=(yc.gt.y).neqv.(yp.gt.y)
	    lstrad=(yc.lt.y).neqv.(yp.lt.y)
	    if(lstrad.or.rstrad) then
c	      
c		if so, compute the crossing of the ray, add up crossings
c
	      xcross=xp+(y-yp)*(xc-xp)/(yc-yp)
	      if(rstrad.and.xcross.gt.x)nrcross=nrcross+1
	      if(lstrad.and.xcross.lt.x)nlcross=nlcross+1
c	      print *,lstrad,rstrad,j,nrcross,nlcross,x,xcross
	    endif
	    yp=yc
	  endif
	  j=j+1
	enddo
c	  
c	  if left and right crossings don't match, it's on an edge
c	  otherwise, inside iff crossings are odd
c
c	print *,nrcross,nlcross
	if(mod(nrcross,2).ne.mod(nlcross,2))then
c	  print *,'edge'
	  return
	endif
	inside=mod(nrcross,2).gt.0
	return
	end

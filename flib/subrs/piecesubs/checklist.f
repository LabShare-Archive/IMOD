c	  CHECKLIST examines a list of NPCLIST piece coordinates in IXPCLIST
c	  (x or y), applies a reduction factor IREDFAC, and given the frame
c	  size NXFRAME, it finds the minimum coordinate MINXPIECE, the total
c	  pixels NXTOTPIX, the # of pieces NXPIECES, and the overlap NXOVERLAP
c	  It also checks that ALL coordinates on the list are multiples of
c	  frame size minus overlap
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine checklist(ixpclist,npclist,iredfac,nxframe,minxpiece
     &	    ,nxpieces,nxoverlap)
c
	integer*4 ixpclist(*)
c
	minxdiff=100000
	minxpiece=100000
	maxxpiece=-100000
c
c	    get min and max of piece coordinates
c
	do i=1,npclist
	  ixpc=iredfac*ixpclist(i)
	  minxpiece=min(minxpiece,ixpc)
	  maxxpiece=max(maxxpiece,ixpc)
	enddo
c
c	    get difference from minimum; keep track of minimum distance
c
	do i=1,npclist
	  ixdiff=iredfac*ixpclist(i)-minxpiece
	  if(ixdiff.gt.0)minxdiff=min(minxdiff,ixdiff)
	enddo
c
c	  now check and make sure all differences are multiples of minimum
c	  but if there were no non-zero differences, return 1 piece, 0 overlap
c
	if(minxdiff.eq.100000)then
	  nxpieces=1
	  nxoverlap=0
	else
	  if(minxdiff.gt. 1.1 * nxframe)then
c	      
c	      If difference is much bigger than frame size, find out how many
c	      frame sizes to add back to difference to make it positive, then
c	      divide positive difference by this # of times to get overlap
c	      THIS WILL FAIL UNDER EXTREME CONDITIONS
c
	    naddback=1
	    nxovertot=nxframe-minxdiff
	    do while(nxovertot.le.0)
	      nxovertot=nxovertot+nxframe
	      naddback=naddback+1
	    enddo
	    nxoverlap=nxovertot/naddback
	    minxdiff=nxframe-nxoverlap
	  endif
	  nxpieces=0
	  do i=1,npclist
	    ixdiff=iredfac*ixpclist(i)-minxpiece
	    if(mod(ixdiff,minxdiff).ne.0)then
	      print *,'Piece list corrupted, line',i
	      nxpieces=-1
	      return
	    endif
	    nxpieces=max(nxpieces,ixdiff/minxdiff+1)
	  enddo
	  nxoverlap=nxframe-minxdiff
	endif
	return
	end

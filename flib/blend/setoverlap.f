


c	  SETOVERLAP, given the minimum total # of pixels MINTOTPIX and the
c	  minimum overlap MINOVERLAP, and the frame dimension NFRAME, and the
c	  amount by which frame size can be reduced, IDFRAME, finds
c	  the minimum # of pieces needed, NPIECES, the overlap NOVERLAP that
c	  minimizes the number of extra pixels, and total # of pixels NTOTPIX
c	  Note that if it can be done with a single piece it will return
c	  NOVERLAP=MINOVERLAP.
c	  
c	  NOTE THIS insists on no prime factor
c	  greater than 19 so that filtering and FFT's can be done.
c
	subroutine setoverlap(mintotpix,minoverlap,nframe,idframe,
     &	    npieces, noverlap, ntotpix)
c
c	  make sure overlap is even
	minover=2*((minoverlap+1)/2)
c	  round frame size down if necessary to have no larger prime factors
	nframe=niceframe(nframe,-idframe,19)
	nonoverlap=nframe-minover
c
c	  here is the minimum number of pieces needed
	npieces=((mintotpix-minover)+(nonoverlap-1))
     &	    /nonoverlap
c	  
c	  if only one piece, round up frame size to fit # of pixels
c
	if(npieces.eq.1)then
	  nframe=idframe*((mintotpix+idframe-1)/idframe)
	  nframe=niceframe(nframe,idframe,19)	!round UP if necessary
	  ntotpix=nframe
	  noverlap=minover
	  return
	endif
c	  
c	  otherwise find smallest frame size that keeps this number of pieces
c	  and has no big prime factors
c
10	newframe=niceframe(nframe-idframe,-idframe,19) 
	nonoverlap=newframe-minover
	if(((mintotpix-minover)+(nonoverlap-1))
     &	    /nonoverlap .eq. npieces)then
	  nframe=newframe
	  go to 10
	endif
c	  
c	  find overlap that gives this number of pieces and requires fewest
c	  extra pixels
c
	ntotpix=2*mintotpix
	lapover=minover
	do while(((mintotpix-lapover)+(nframe-lapover-1))/
     &	    (nframe-lapover).eq.npieces)
c
c	    total # of pixels required with this overlap
	  ntottmp=npieces*(nframe-lapover)+lapover
	  if(ntottmp.lt.ntotpix)then
	    ntotpix=ntottmp
	    noverlap=lapover
	  endif
	  lapover=lapover+2			!keep overlap even
	enddo
	return
	end

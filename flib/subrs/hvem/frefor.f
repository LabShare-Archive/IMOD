	subroutine frefor(card,xnum,nfields)
	real*4 xnum(*)
	character card*(*),blank*1/' '/
	if(card(1:1).eq.'/')return
	NFIELDS=0
	istart=1
C Search for starting point
	DO WHILE (CARD(ISTART:ISTART).EQ.BLANK)
	  ISTART=ISTART+1
	END DO
C Decode fields - use lnblnk instead of len as limit
	DO WHILE (ISTART.LE.lnblnk(card))
	  NFIELDS=NFIELDS+1
	  NPOINT1=INDEX(CARD(ISTART:),BLANK)-1
	  NPOINT2=INDEX(CARD(ISTART:),',')-1
	  if(npoint1.le.0)then
	    npoint=npoint2
	  elseif(npoint2.le.0)then
	    npoint=npoint1
	  else
	    npoint=min(npoint1,npoint2)
	  endif
	  IEND=ISTART+NPOINT-1
c	  
c	    switch to standard f77 here, get rid of decode AND variable format
c	  * is needed instead of F format on Sun
c
	  read(card(istart:iend),*,err=99)XNUM(NFIELDS)
c	    DECODE(NPOINT,30,CARD(ISTART:IEND),err=99)XNUM(NFIELDS)
c30	  FORMAT(F75.0)
	  ISTART=IEND+2
C Skip over recurring blanks
	  DO WHILE (CARD(ISTART:ISTART).EQ.BLANK.or.
     &        CARD(ISTART:ISTART).EQ.',')
	    ISTART=ISTART+1
	  END DO
	END DO
	return
99	write(*,31)nfields
31	format('WARNING: Invalid input in field',i3,
     &	    '; input ignored after that')
	end

c       FREFOR and FREFOR2 for reading variable numbers of elements on a line
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$

C       !
C       Reads numeric values from a character string in [card], returns the
c       values in [xnum] and the number of values in [nfields].  Values may be
c       separated by any number of spaces and commas.  Prints a warning
c       message if there is non-numeric data in a field.
c       !
      subroutine frefor(card,xnum,nfields)
      implicit none
      real*4 xnum(*)
      integer*4 nfields,istart,npoint,npoint1,npoint2,iend
      character card*(*),blank*1/' '/
      integer*4 lnblnk
c
      if(card(1:1).eq.'/')return
      NFIELDS=0
      istart=1
C       Search for starting point
      DO WHILE (CARD(ISTART:ISTART).EQ.BLANK)
        ISTART=ISTART+1
      END DO
C       Decode fields - use lnblnk instead of len as limit
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
c         switch to standard f77 here, get rid of decode AND variable format
c         * is needed instead of F format on Sun
c         
        read(card(istart:iend),*,err=99)XNUM(NFIELDS)
c         DECODE(NPOINT,30,CARD(ISTART:IEND),err=99)XNUM(NFIELDS)
c         30      FORMAT(F75.0)
        ISTART=IEND+2
C         Skip over recurring blanks
        DO WHILE (CARD(ISTART:ISTART).EQ.BLANK.or.
     &      CARD(ISTART:ISTART).EQ.',')
          ISTART=ISTART+1
        END DO
      END DO
      return
99    write(*,31)nfields
31    format('WARNING: Invalid input in field',i3,
     &    '; input ignored after that')
      end

C       !
C       Reads numeric values from a character string in [card], returns the
c       values in [xnum] and the number of values in [nfields].  Returns 1
c       in the array [numeric] if there is numeric value there, or 0 if not.
c       Reads up to [limnum] values.
c       !
      subroutine frefor2(card,xnum,numeric,nfields,limnum)
      implicit none
      real*4 xnum(*)
      integer*4 numeric(*),limnum,nfields,istart,npoint,npoint1,npoint2,iend
      character card*(*),blank*1/' '/
      integer*4 lnblnk
c
      if(card(1:1).eq.'/')return
      NFIELDS=0
      istart=1
c
C       Search for starting point
c
      DO WHILE (CARD(ISTART:ISTART).EQ.BLANK)
        ISTART=ISTART+1
      END DO
c
C       Decode fields - use lnblnk instead of len as limit
c
      DO WHILE (ISTART.LE.lnblnk(card) .and. nfields .lt. limnum)
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
        numeric(nfields) = 0
        xnum(nfields) = 0.
        read(card(istart:iend),*,err=99)XNUM(NFIELDS)
        numeric(nfields) = 1
99      ISTART=IEND+2
c
C         Skip over recurring blanks
c
        DO WHILE (CARD(ISTART:ISTART).EQ.BLANK.or.
     &      CARD(ISTART:ISTART).EQ.',')
          ISTART=ISTART+1
        END DO
      END DO
      return
      end

C*IMOPEN
C
C	Open file NAME with qualities ATBUTE ('RO' 'OLD' 'NEW' SCRATCH')
C	and associate with stream ISTREAM 
C	(ISTREAM = # between 1 & 12 ; MAX of  10 files opened at any time!!)
c	  DNM 8/22/00: changed size limit for detecting swapped bytes to 60000
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2002/07/31 17:57:41  mast
c	  Finished standardizing error output, added implicit none, changed
c	  line spacing for output, used lnblnk instead of len to truncate
c	  the filename output
c	
c	  Revision 3.2  2002/07/21 19:17:31  mast
c	  Standardized error output to ERROR: ROUTINE
c	
c	  Revision 3.1  2002/06/26 00:23:44  mast
c	  Changed STOP statements to print and call exit(1)
c	
c
	SUBROUTINE IMOPEN(ISTREAM,NAME,ATBUTE)
	implicit none
	CHARACTER*(*) NAME,ATBUTE
	character*7 at2
	CHARACTER*120 FULLNAME
	integer*4 istream
	include 'imsubs.inc'
	DATA NBHDR/1024/, NBW/4/, NBW3/12/, NB/1,2,4,4,8/, NBL/800/
	DATA FLAG/maxunit*.TRUE./, NOCON/maxunit*.FALSE./, numopen/0/
	data spider/maxunit*.false./, print/.true./
        data ibleft/maxunit*0/
	real*4 buf(3)
	integer*4 intbuf(3)
	equivalence (buf,intbuf)
	logical mrctyp,spityp
	integer*4 j,i,nsam,lenrec,nfilsz,ier,intflip
	integer*4 lnblnk
C
C   Check for valid unit number
C
	IF (ISTREAM .GT. maxstream) THEN
	  WRITE(6,1000)
1000	  FORMAT(//' ERROR: IMOPEN - Invalid STREAM number,',
     &	      ' cannot open file!!!'//)
	  call exit(1)
	END IF
	numopen = numopen + 1
	if (numopen .gt. maxunit) then
	  write(6,1100)maxunit
1100	  format(//,' ERROR: IMOPEN: - No More than',i4,
     &	      ' files can be opened, cannot open file!!',//)
	  call exit(1)
	endif
C
C   Open file
C
	call strupcase(at2,atbute)
	CALL QOPEN(LSTREAM(ISTREAM),NAME,AT2)
	J = LSTREAM(ISTREAM)
	FLAG(J) = .TRUE.
	NOCON(J) = .FALSE.
c	  
c	  DNM 7/30/02: initialize flip here for all open units
c
	mrcflip(j) = .false.
	spider(j)=at2(1:2).eq.'RO'.or.at2(1:3).eq.'OLD'
c	  
c	  check if it's a SPIDER file or a flipped file
c
	if(spider(j))then
	  CALL QSEEK(J,1,1,1)
	  CALL QREAD(J,buf,NBW3,IER)
	  IF (IER .NE. 0) then
	    print *
	    print *, 'ERROR: IMOPEN - ERROR READING FILE'
	    call exit(1)
	  endif
	  mrctyp=.true.
	  spityp = .false.
	  do i=1,3
	    mrctyp=mrctyp.and.(intbuf(i).ge.0.and.intbuf(i).le.60000)
	  enddo
	  if(mrctyp)then
	    mrcflip(j)=.false.
	  else
	    mrctyp=.true.
	    do i=1,3
	      intflip=intbuf(i)
	      call convert_longs(intflip,1)
	      mrctyp=mrctyp.and.(intflip.ge.0.and.intflip.le.60000)
	    enddo
	    mrcflip(j)=mrctyp
	  endif
	  if(.not.mrctyp)
     &	      spityp=abs(buf(1)).gt.0.5.and.abs(buf(1)).lt.60000..and.
     &	      buf(2).gt.0.5.and.buf(2).lt.60000..and.
     &	      buf(3).gt.0.5.and.buf(3).lt.60000.
	  spider(j)=spityp.and..not.mrctyp
	  if(.not.(mrctyp.or.spityp))then
	    print *
	    print *, 'ERROR: IMOPEN - THIS FILE IS NOT ',
     &		'RECOGNIZABLE AS MRC OR SPIDER IMAGE FILE'
	    call exit(1)
	  endif
	endif
c	  
c	  if SPIDER file, open a second time on unit 30+j, for direct access
c
	if(spider(j))then
	  INQUIRE(FILE=NAME,RECL=NSAM)
	  LENREC = NSAM/4
c
c	CER change VAXism TYPE='OLD' to STATUS='OLD' for g77
c       alse delete RECORDTYPE='FIXED',SHARED,READONLY
c
	  OPEN(UNIT=30+j,FILE=NAME,STATUS='OLD',
     &	      ACCESS='DIRECT',FORM='UNFORMATTED',
     &	      RECL=LENREC)
	  ncrs(1,j)=lenrec
	endif
C
C Get and print file name
C  DNM: QINQUIRE returns nothing if J>5, then write crashes, so just skip it
C  DNM: for unix, change len(at2) to len(atbute)
	if(j.le.5)then
	CALL QINQUIRE(J,FULLNAME,NFILSZ)
	IF (AT2 .EQ. 'NEW' .OR. AT2 .EQ. 'SCRATCH') THEN
	  if (print)WRITE(6,2000) AT2(1:len(atbute)),ISTREAM,
     .	  FULLNAME(1:lnblnk(fullname))
	ELSE
	  if (print)WRITE(6,2100) AT2(1:len(atbute)),ISTREAM,
     .	  FULLNAME(1:lnblnk(fullname)),nfilsz
	ENDIF
	endif
2000	FORMAT(/,1x,A,' image file on unit',I4,' : ',A)
2100	FORMAT(/,1x,A,' image file on unit',I4,' : ',A,
     .	'     Size= ',I12)
	if(spider(j).and.print)write(6,2200)
2200	format(/,20x,'This is a SPIDER file.')
	if(mrcflip(j).and.print)write(6,2300)
2300	format(/,20x,'This is a byte-swapped file.')
C	  
	RETURN
C
C
C*IMCLOSE
C
C	Close file on stream ISTREAM. This frees up this channel for
C	further use.
C
	ENTRY IMCLOSE(ISTREAM)
	J=LSTREAM(ISTREAM)
	if(spider(j))close(30+j)
	CALL QCLOSE(LSTREAM(ISTREAM))
	numopen = numopen - 1
	if (numopen .lt. 0) numopen = 0
	RETURN
	END

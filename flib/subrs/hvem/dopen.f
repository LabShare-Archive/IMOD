C*DOPEN*****************************************
C
C	SIMPLE DISK OPEN SUBROUTINE THAT IS NICER TO
C	USE THAN THE STANDARD FORTRAN CALLS
C
C	CARRIAGECONTROL = LIST  EXCEPT FOR FORMAT= 'P'
C	IF 'RO' IS SPECIFIED, FILE IS OPENED 'OLD' & 'READONLY'
C	SHARED IS ALWAYS SET... nope... SHARED is gone as it is
C       not suported by g77  23/6/00 CER
C
C	DOPEN(IUNIT,NAME,'NEW/OLD/SCRATCH/RO','F/U/P')
C       FORMAT 1000 CHANGED A.D.MCL 4/10/84
C  16/1/87 removed 'SHARED' from write opens PRE
C  22/6/00 replace implicit sting join (//) with call to CONCAT  CER
C  23/6/00 remove 'SHARED'... not supported by g77 CER
C  23/6/00 remove 'CARRIAGECONTROL='LIST'... not supported by g77 CER
C  23/6/00 remove READONLY... not supproted by g77 CER
C------------------------------------------------------
	SUBROUTINE DOPEN(IUNIT,FNAME,ITYPE,IFORM)
	CHARACTER*(*) FNAME,ITYPE
	CHARACTER*1 IFORM
	CHARACTER*11 FORMAT
	CHARACTER*50 FULLNAM
c       add definition of concat subroutine from hvem directory
        character*80 concat
c	  
c	  special for unix/SGI: if 'NEW' and file exists, rename to file~
c
	logical exist
	integer rename
	if(itype.eq.'NEW'.or.itype.eq.'new')then
	  inquire(file=fname,exist=exist)
	  if(exist)then
	    namlen=lnblnk(fname)
c	    CER patch up string concatenation issue
c	    ierr=rename(fname,fname(1:namlen)//'~')
	    ierr=rename(fname,concat(fname(1:namlen),'~'))
	    if(ierr.ne.0)write(6,*)
     &		' Error attempting to rename existing file'
	  endif
	endif
c
	FORMAT = 'FORMATTED'
	IF (IFORM .EQ. 'U' .OR. IFORM .EQ. 'u') FORMAT = 'UNFORMATTED'
	IF (IFORM .EQ. 'P' .OR. IFORM .EQ. 'p') GOTO 10
C
C   HERE FOR FORMATTED/ UNFORMATTED
C
	IF (ITYPE .EQ. 'RO' .OR. ITYPE .EQ. 'ro') GOTO 5
C	OPEN(UNIT=IUNIT,NAME=FNAME,CARRIAGECONTROL='LIST',
C    .	STATUS=ITYPE,FORM=FORMAT)
       	OPEN(UNIT=IUNIT,NAME=FNAME,STATUS=ITYPE,FORM=FORMAT)
	GOTO 20
C5	OPEN(UNIT=IUNIT,NAME=FNAME,CARRIAGECONTROL='LIST',
C     .	STATUS='OLD',FORM=FORMAT,SHARED,READONLY)
5	OPEN(UNIT=IUNIT,NAME=FNAME,STATUS='OLD',FORM=FORMAT)
	GOTO 20
C
C  HERE FOR FORMATTED, PRINTING (CARRIAGECONTROL=FORTRAN) ONLY
C
10	IF (ITYPE .EQ. 'RO' .OR. ITYPE .EQ. 'ro') GOTO 15
	OPEN(UNIT=IUNIT,NAME=FNAME,STATUS=ITYPE,FORM=FORMAT)
	GOTO 20
C15	OPEN(UNIT=IUNIT,NAME=FNAME,STATUS='OLD',FORM=FORMAT,SHARED,
C     .	READONLY)
15	OPEN(UNIT=IUNIT,NAME=FNAME,STATUS='OLD',FORM=FORMAT)
C
C  NOW WRITE OUT FILE INFO
C
20	INQUIRE (FILE=FNAME,NAME=FULLNAM)
	WRITE(6,1000) FORMAT,ITYPE,IUNIT,FULLNAM
1000	FORMAT(/,5x,A,3X,A,'  file opened on unit # ',I2,
     .	/'  Name= ',A,/)
	RETURN
	END

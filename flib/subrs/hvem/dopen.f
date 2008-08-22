C       *DOPEN*****************************************
C       
C	SIMPLE DISK OPEN SUBROUTINE THAT IS NICER TO
C	USE THAN THE STANDARD FORTRAN CALLS
c
c       $Id$
c       See log and ancient history at end
C       
c       !
c       Opens a file with name [FNAME] on logical unit [IUNIT].  The type of
c       the file is set by [ITYPE], which can be {NEW}, {OLD}, {SCRATCH} or
c       {RO}, where {RO} is treated like {OLD}.  The format is set with
c       [IFORM], which can be {F} for formatted or {U} for unformatted.  These
c       arguments can be all lower case or all upper case.  For a new file, it
c       renames an existing file to FNAME~.
c       ^  It prints a statement that the file was opened.  This statement
c       can be disabled with {call dopenHush(.true.)}.
c       !
      SUBROUTINE DOPEN(IUNIT,FNAME,ITYPE,IFORM)
      implicit none
      logical exist
      CHARACTER*(*) FNAME,ITYPE
      CHARACTER*1 IFORM
      CHARACTER*11 FORMAT
      CHARACTER*320 FULLNAM
      integer*4 ierr,iunit
      integer*4 imodBackupFile,lnblnk
      logical*4 hush
      common /hushcom/hush
      data hush /.false./
c       
c       if 'NEW' and file exists, rename to file~
c       DNM 10/20/03: changed to call subroutine, and clean up old stuff
c       
      if(itype.eq.'NEW'.or.itype.eq.'new')then
        ierr = imodBackupFile(fname)
        if(ierr.ne.0)write(6,'(/,a,a)')
     &      'WARNING: DOPEN - Error attempting to rename existing file',
     &      fname(1:lnblnk(fname))
      endif
c       
c       check existence of old file to get a nice error message
c       
      if(itype.eq.'OLD'.or.itype.eq.'old'.or.
     &    itype.eq.'RO'.or.itype.eq.'ro')then
        inquire(file=fname,exist=exist)
        if(.not.exist)then
          write(*, '(/,a,a,a)') 'ERROR: DOPEN - FILE ',fname(1:lnblnk(fname)),
     &        ' DOES NOT EXIST'
          call exit(1)
        endif
      endif
c       
c       open old 'P' as FORMATTED
c
      FORMAT = 'FORMATTED'
      IF (IFORM .EQ. 'U' .OR. IFORM .EQ. 'u') FORMAT = 'UNFORMATTED'
C       
C       Treat RO as OLD, leave RO in the message for log file stability
C       
      IF (ITYPE .EQ. 'RO' .OR. ITYPE .EQ. 'ro') then
        OPEN(UNIT=IUNIT,FILE=FNAME,STATUS='OLD',FORM=FORMAT,ERR=30)
      else
        OPEN(UNIT=IUNIT,FILE=FNAME,STATUS=ITYPE,FORM=FORMAT,ERR=30)
      endif
C       
C       NOW WRITE OUT FILE INFO
C       
20    if (hush) return
      INQUIRE (FILE=FNAME,NAME=FULLNAM)
      WRITE(6,1000) FORMAT(1:lnblnk(format)),ITYPE,FULLNAM(1:lnblnk(fullnam))
1000  FORMAT(/,1x,A,2X,A,'  file opened: ',A)
      RETURN
30    write(*, '(/,a,a)')'ERROR: DOPEN - CANNOT OPEN FILE ',
     &    fname(1:lnblnk(fname))
      call exit(1)
      END


      subroutine dopenHush(value)
      implicit none
      logical*4 hush, value
      common /hushcom/hush
      hush = value
      return
      end

c       Obsolete statements for historical purposes:
C	CARRIAGECONTROL = LIST  EXCEPT FOR FORMAT= 'P'
C	IF 'RO' IS SPECIFIED, FILE IS OPENED 'OLD' & 'READONLY'
C	SHARED IS ALWAYS SET... nope... SHARED is gone as it is
C       not suported by g77  23/6/00 CER
C       
C	DOPEN(IUNIT,NAME,'NEW/OLD/SCRATCH/RO','F/U/P')
C       FORMAT 1000 CHANGED A.D.MCL 4/10/84
C       16/1/87 removed 'SHARED' from write opens PRE
C       22/6/00 replace implicit sting join (//) with call to CONCAT  CER
C       23/6/00 remove 'SHARED'... not supported by g77 CER
C       23/6/00 remove 'CARRIAGECONTROL='LIST'... not supported by g77 CER
C       23/6/00 remove READONLY... not supproted by g77 CER
c       
c       
c       $Log$
c       Revision 3.8  2007/03/01 14:51:48  mast
c       Had to declare lnblnk
c
c       Revision 3.7  2007/02/28 23:25:10  mast
c       Fixed documentation on new dopenHush
c
c       Revision 3.6  2007/02/28 23:23:56  mast
c       Cleaned up - got sick of sorting through obsolete options
c
c       Revision 3.5  2006/08/21 16:50:48  mast
c       Abbreviated annoying output to 1 line and 1 blank line
c
c       Revision 3.4  2006/02/08 00:40:47  mast
c       Added filename to rename warning
c
c       Revision 3.3  2005/12/09 04:39:44  mast
c       gfortran: .xor., continuation, byte, or open fixes
c	
c       Revision 3.2  2003/10/24 03:36:05  mast
c       switch to calling routine to make backup file
c	
c       Revision 3.1  2002/10/23 21:27:02  mast
c       Added check for existence of old file, and err= clauses to open
c       statements, to get a good error output and exit with error

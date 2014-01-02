C       
C       *STRUPCASE
C       
C       Subroutine to convert the lower case characters in a string to 
C       uppercase and return the converted string in a new string
C       
C       AT2 - destination of conversion
C       ATBUTE - source string
C       
C       
      SUBROUTINE STRUPCASE(AT2,ATBUTE)
C       
      CHARACTER*(*) AT2
      CHARACTER*(*) ATBUTE
C       
      INTEGER LCV
C       
      at2 = ''
      DO LCV = 1, min(LEN(ATBUTE), len(at2))
        IF (ATBUTE(LCV:LCV) .GE. 'a' .AND.
     &      ATBUTE(LCV:LCV) .LE. 'z') THEN
          AT2(LCV:LCV) = CHAR(ICHAR(ATBUTE(LCV:LCV)) - 32)
        ELSE
          AT2(LCV:LCV) = ATBUTE(LCV:LCV)
        ENDIF
      END DO
C       
      RETURN
      END

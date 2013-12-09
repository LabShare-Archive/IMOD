c       PROGRAM DESKCALC
c       fixed to run on unix by David Mastronarde 11/13/90
c       required "getarc" instead of LIB$GET_FOREIGN, printing out
c       STRNG with A format instead of printing byte variable with
c       <LENGTH>A1 atrocity; also fixed declaration of common EVAL to
c       avoid alignment errors (had to rename it EVAL2)
c       DNM 3/4/01: Changes to make it work with less forgiving compilers
c       (gnu).  Removed decodes, replaced comparisons between bytes and
c       characters with byte-to-byte or character-to-character comparisons,
c       removed equivalencing of two-dimensional byte and character arrays
c       and copied the initialized character arrays into the byte arrays
c       instead, fixed a problem caused by assuming that .OR. had higher
c       precedence than .AND.  Removed a few branches into if blocks, but
c       could not easily remove the branches to 8 and 14, which generate
c       warnings and are very dangerous.  Must be compiled without
c       optimization.
c       
c       $Id$
c       
      CHARACTER*132 STRNG
c       ISTAT=LIB$GET_FOREIGN(STRNG,,LENGTH,IFLAG)      !See if expression
      IF (iargc().gt.0) THEN                    !0 for standalone, 1 for "run"
        CALL FUNINPUT(0,6,RESULT,JERR,MORE)
        IF (JERR.EQ.0) CALL EXIT(0)             !Exit if successful
      ENDIF
1     MORE=0
      CALL FUNINPUT(5,6,RESULT,JERR,MORE)       !Else continue in loop
      DO WHILE (MORE.NE.0)
        CALL EVLPOL(RESULT,JERR,MORE)
C         WRITE(*,*)'RETURN FROM EVLPOL, MORE=',MORE
      ENDDO
      GOTO 1
      END



      SUBROUTINE FUNINPUT(NIN,NOUT,RESULT,JERR,MORE)
C       
C       D. Kopf   Duke Anatomy   February 23, 1985
C       Copyright 1985 by David Kopf
C       
C       Evaluates most any FORTRAN expression.
C       The variable STRING contains the expression terminated by a zero byte.
C       The length of the expression must be L.E. 132.
C       Uses an inelegant but original reverse polish parsing routine.
C       If MORE .GT. 0 on return, means arrays have been used. Call again
C       for succeeding values until MORE .eq. 0.
C       If MORE .LT. 0, an error occurred and RESULT is undefined
C       
      INTEGER*1 STRING(132),STRING2(132),MODES(10,10),DEFSTR(132,10)
      CHARACTER*132 STRNG,strng2
      CHARACTER*10 MODS(10),VARNMS(50)
      DATA MODS/'RADIANS','DEGREES','HELP',7*' '/
      DATA VARNMS/'#','L#','PI','pi','E','e',44*' '/
      DATA NMODES/3/
      DATA STBRK,OPCODE,VARCODE,DUNDEF/-3.9E21,-3.9E22,-3.9E23,-3.9E24/
      COMMON /DESKC/ ICSP,ICSTK(50),LINENO,ILFLG,VL4S(50)
      COMMON /DATA/ NLINES,NVVARS,IVARLST(20),VAL(2000,20)
      COMMON /VARS/ NVARS,VARNAMS,VL1(50),VL2(50),VL3(50),VL4(50),DEGFLG
c       EQUIVALENCE (STRING,STRNG),(MODES,MODS)
      EQUIVALENCE (STRING,STRNG),(string2,strng2)
      LOGICAL*1 INDFLG,DEGFLG,DFLAG,jumpto8
      integer*1 VARNAMS(10,50)
      DATA INDFLG,DFLAG,jumpto8/.FALSE.,.FALSE.,.FALSE./
      logical firsttime/.true./
      save firsttime
      integer*1 byteupcase

      if(firsttime)then
        do j=1,10
          do i=1,10
            modes(j,i)=ichar(mods(i)(j:j))
          enddo
          do i=1,50
            varnams(j,i)=ichar(varnms(i)(j:j))
          enddo
        enddo
        firsttime=.false.
      endif

      JERR=1
      MORE=0
      IFFLG=1
      IF (NIN.EQ.0) GOTO 2
C       1       IF (.NOT.INDFLG.AND.IFFLG.EQ.0) WRITE (NOUT,'(''$Data value:'')')
1     NDONE=0
      IF (NIN.EQ.0) RETURN
c       
c       read from command file
c       
2     IF (INDFLG) then
        READ(94,'(A)',ERR=209,END=210) STRNG
        length = len_trim(strng)
        IF (LENGTH.EQ.0) GOTO 2
        IF (STRNG(1:1).EQ.'#'.OR.STRNG(1:1).EQ.'!') GOTO 2 !Ignore these
        STRING(LENGTH+1)=ichar(',')             !Insert comma at end
        jumpto8 = .true.
        goto 5
209     WRITE(*,*)'Error reading command file'
210     CLOSE (UNIT=94)
        INDFLG=.FALSE.
        NLINES=NLINES+1
        VL1(1)=NLINES-1                         !Save number of lines
        GOTO 1
      endif
c       
c       or use argument or read from input
c       
      IF (NIN.EQ.0) THEN
        call getarg(iargc(),strng)              !last argument:
        length=len_trim(strng)                    ! works if in "run" or not
      else
        WRITE (NOUT,'(1x,''Calc? '',$)')
        READ (NIN,'(A)') STRNG
        length =len_trim(strng)
        IF (LENGTH.EQ.0) CALL EXIT(0)           !FOR DESKCALC
      ENDIF
      IFFLG=0
      IF (LENGTH.EQ.0) GOTO 1
      STRING(LENGTH+1)=0

c       DNM: skip over leading spaces for these tests, skip # comments
c       and ! comments here
      j=1
      do while(j.lt.length.and.(STRNG(:J).EQ.' '.OR.STRING(J).EQ.9))
        j=j+1
      enddo
      istart=j
      if (strng(j:j) .eq. '#' .or. strng(j:j) .eq. '!') goto 1

      DO 150 J=1,NMODES
        II=1
        DO I=istart,LENGTH+1
          IF (MODES(II,J).EQ.ichar(' ')) THEN
            IF (J.EQ.1) THEN
              DEGFLG=.FALSE.
              WRITE(*,*)' OK, We''ll use radians'
            ELSE IF (J.EQ.2) THEN
              DEGFLG=.TRUE.
              WRITE(*,*)' OK, We''ll use degrees'
            ELSE IF (J.EQ.3) THEN
              WRITE (6,148)
148           FORMAT(' Possible examples are',/,'  SAVEIT=LOG(10)*5'
     &            ,/, '  @calcfile' ,/, '  Degrees',/,'  X=2*Sin(45)'
     &            ,/, '  LN(pi*X)'/' Etc.',/,
     &            ' # and ! can be used to start a comment')
            ELSE
              LENGTH=1
              DO WHILE (DEFSTR(LENGTH,J).NE.0)
                STRING(LENGTH)=DEFSTR(LENGTH,J)
                LENGTH=LENGTH+1
              ENDDO
              STRING(LENGTH)=0
              GOTO 4
            ENDIF
            GOTO 1
          ENDIF
          IF(STRNG(I:I).NE.' '.AND.
     &        byteupcase(STRING(I)).NE.MODES(II,J))
     &        GOTO 150
          II=II+1
        ENDDO
150   continue
4     IQ=LENGTH
      IPOS=0
      ICSP=0
      ICSPV=0
      LFIRST=0
      I=1
5     DO 20 J=1,LENGTH
        if (jumpto8) goto 8
        IF (STRNG(J:J).EQ.':') THEN
c           
c           For a :, save string  before : as a MODE, string after as defined
c           string for that mode
c           
          NMODES=NMODES+1
          DO I=1,J-1
            MODES(I,NMODES)=byteupcase(STRING(I))
          ENDDO
          MODES(J,NMODES)=ichar(' ')
          I=J+1
          DO WHILE (STRING(I).NE.0)
            DEFSTR(I-J,NMODES)=STRING(I)
            I=I+1
          ENDDO
          DEFSTR(I-J,NMODES)=0
          GOTO 1
        ENDIF
        IF (STRNG(J:J).EQ.' '.OR.STRING(J).EQ.9) GOTO 20 !Ignore spaces, tabs
        IF (STRNG(J:J).EQ.'!'.or.STRNG(J:J).EQ.'#') GOTO 21 !Ignore comments
        IF (STRNG(J:J).EQ.'@') THEN
          OPEN (UNIT=94,FILE=STRNG(J+1:),STATUS='OLD',ERR=6)
          DFLAG=.FALSE.
          GOTO 7
6         WRITE(*,*)' Invalid indirect command file'
          CLOSE(UNIT=94)
          GOTO 1
7         INDFLG=.TRUE.
          JFLAG=1
          goto 1
        endif
8       if (indflg) then
          jumpto8 = .false.
          IF=1
          IVI=0
          IF (STRNG(1:1).EQ.'$') THEN
            DO I=1,20
              IVARLST(I)=0
            ENDDO
            DFLAG=.TRUE.
            NVVARS=0
            NLINES=0
23          DO I=IF+1,LENGTH+1
              IF (STRNG(I:I).EQ.','.OR.STRNG(I:I).EQ.'!') THEN
                CALL PUSHVAR(STRING2,IVI,IPOS)  !Push variable
                IF (IPOS.NE.0) VL3(IPOS)=DUNDEF !Show type
                NVVARS=NVVARS+1
                IVARLST(NVVARS)=IPOS
                IF=I
                IVI=0
                IF (STRNG(I:I).EQ.'!') GOTO 2
                GOTO 23
              ENDIF
              IVI=IVI+1
              STRING2(IVI)=STRING(I)
            ENDDO
            GOTO 2
          ENDIF
          IF (.NOT.DFLAG) GOTO 4                !If calc file
          NVR=0                                 !Assume its a data line
          NSBS=0
          DO 35 I=IF,LENGTH+1
            IF (STRNG(I:I).EQ.'=') GOTO 4       !No, its an assignment
            IF (STRNG(I:I).EQ.'!') GOTO 36      !Ignore comments
            IF (STRNG(I:I).EQ.' '.AND.NSBS.EQ.0) GOTO 35
            IF (STRNG(I:I).EQ.' '.OR.STRNG(I:I).EQ.','.OR.STRING(I)
     &          .EQ.9) THEN
              IF (NVR.EQ.0.AND.I.EQ.LENGTH+1) GOTO 4 !Calculation
              NVR=NVR+1
              IF (IVARLST(NVR).EQ.0) GOTO 35
              IF (NSBS.NE.0) THEN
c                 DECODE (NSBS,'(F20.0)',STRING2,ERR=9)VAL(NLINES+1,NVR)
                read(strng2(1:nsbs),'(F20.0)',ERR=9)VAL(NLINES+1,NVR)
              ELSE
                VAL(NLINES+1,NVR)=DUNDEF
              ENDIF
              NSBS=0
              GOTO 35
            ENDIF
            NSBS=NSBS+1
            STRING2(NSBS)=STRING(I)
35        CONTINUE
36        IF (NVR.NE.0) NLINES=NLINES+1 
          IF (NLINES.EQ.1) THEN
            DO I=1,NVVARS
              NVR=IVARLST(I)
              IF (NVR.NE.0) VL4(NVR)=VAL(1,I)
            ENDDO
          ENDIF
          GOTO 2
9         WRITE(*,*)'Error in indirect command file'
          WRITE (6,5454) (STRING2(KK),KK=1,NSBS)
5454      FORMAT(' STRING IN ERROR IS',128A1)
          CLOSE (UNIT=94)
          INDFLG=.FALSE.
          NLINES=NLINES+1
          VL1(1)=NLINES-1                       !Save number of lines
          GOTO 1
        ELSE IF (STRNG(J:J).EQ.'=') THEN
          IF (IPOS.NE.0) THEN                   !Too many variable defs
            WRITE(*,*)' Too many equal signs'
            GOTO 1
          ENDIF
          IF (I.EQ.1) GOTO 20                   !He likes = signs
          CALL PUSHVAR(STRING2,I-1,IPOS)        !Push the variable
          IF (IPOS.EQ.0) THEN
            WRITE(*,*)' Sorry, a number cannot be redefined'
            GOTO 1
          ENDIF
          I=1
          LFIRST=J-1
          GOTO 20                               !And evaluate the expression
        ELSE IF (STRNG(J:J).EQ.',') THEN
          IF (IPOS.EQ.0) GOTO 19
c           DNM : change this IF-THEN to a GOTO, put the 19 message below
c           19        WRITE(*,*)' Meaningless garbage ignored'
c           GOTO 1
c           ENDIF
          STRING2(I)=0                          !Range variable def
          CALL EVAL(STRING2,JERR)               !Get the number
          IF (JERR.NE.0) GOTO 1
          CALL EVLPOL(RESULT,JERR,MORE)
          goto 14
        ENDIF
        STRING2(I)=STRING(J)
        I=I+1
20    continue
21    STRING2(I)=0
      IF (J.NE.LENGTH) LENGTH=J-1
      IF (LFIRST.NE.0) LENGTH=LFIRST
C       WRITE (6,455) (STRING2(III),III=1,I-1)
C       455     FORMAT(' CALLING EVAL WITH STRING:',100A1)
      CALL EVAL(STRING2,JERR)
      IF (JERR.NE.0) GOTO 1
      LINO=1
22    CALL EVLPOL(RESULT,JERR,MORE)
C       IF (IPOS.EQ.0) RETURN                   !Not used in deskcalc
C       WRITE(*,*)'RETURNED FROM EVLPOL, RESULT,JERR,MORE=',RESULT,JERR,MORE
      NDONE=1
      JERR=0
      IF (ICSPV.NE.0) GOTO 14                   !Store variable def
      JFLAG=2
      IFLAG=2
      IF (ILFLG.NE.0) THEN
        WRITE (6,'(7H Line #,I5,2H, ,$)') LINO
        LINO=LINENO
      ELSE
        WRITE (6,'($)')
      ENDIF
      IF (RESULT.EQ.DUNDEF) THEN
        WRITE (6,'(1X,A,15H =  (undefined))') STRNG(1:LENGTH)
      ELSEIF(RESULT.NE.0..AND.(ABS(RESULT).LT.1E-4.OR.ABS(RESULT)
     &      .GT.1E6))THEN
        WRITE (6,'(1X,A,2H =,1PE16.8,7X,100A1)') 
     &      STRNG(1:LENGTH),RESULT,(STRING(I),I=J,IQ)
      ELSE IF (INT(RESULT)-RESULT.EQ.0.) THEN
        WRITE (6,'(1X,A,2H =,I6,17X,100A1)')
     &      STRNG(1:LENGTH),INT(RESULT),(STRING(I),I=J,IQ)
      ELSE
        WRITE (6,'(1X,A,2H =,F13.6,10X,100A1)')
     &      STRNG(1:LENGTH),RESULT,(STRING(I),I=J,IQ)
      ENDIF
      IF (IPOS.NE.0) GOTO 14                    !Store constant def
      IF (MORE.NE.0) GOTO 22
      GOTO 1

14    IF (JERR.NE.0.OR.MORE.NE.0) GOTO 19
      IF (ICSPV.EQ.0) THEN
        ICSPV=1
        VL1(IPOS)=RESULT                        !Save first number
        VL3(IPOS)=0.                            !Show constant
        VL4(IPOS)=RESULT
        IF (NDONE.NE.0) GOTO 1
      ELSE IF (ICSPV.EQ.1) THEN
        ICSPV=2
        VL2(IPOS)=RESULT                        !Save second number
        VL3(IPOS)=1.                            !Default range
        IF (VL1(IPOS).GT.VL2(IPOS)) VL3(IPOS)=-1.
        IF (NDONE.NE.0) GOTO 1
      ELSE IF (ICSPV.EQ.2) THEN
        ICSPV=3
        IF (RESULT.EQ.0.OR.RESULT.EQ.DUNDEF) GOTO 16
        IF (((VL2(IPOS)-VL1(IPOS))/RESULT).LE.0.) GOTO 16
        VL3(IPOS)=RESULT                        !Save third number
        GOTO 1
16      WRITE(*,*)'Bad variable range'
        GOTO 1
      ENDIF
19    WRITE(*,*)' Meaningless garbage ignored'
      GOTO 1
      END



      SUBROUTINE EVAL(STRNG,JERR)
C       
C       EVALUATE FORTRAN-LIKE ARITHMETIC EXPRESSION IN STRING
C       
      INTEGER*2 STPOS(132)
      CHARACTER*6 FCNS(50)
      INTEGER*1 FUNC(6,50),TERM(20)
c       EQUIVALENCE (FUNC,FCNS)
      DATA NFUNCS/20/
      DATA FCNS/'TRQS','NL','GOL','PXE','NISA','SOCA','NATA','NIS',
     &    'SOC','NAT','HNIS','HSOC','HNAT','SBA','TNI','TAOLF',
     &    'XAM','NIM','MUS','NGIS',30*'TRQS'/
      REAL*4 STACK(1000)
      DATA STBRK,OPCODE,VARCODE,DUNDEF/-3.9E21,-3.9E22,-3.9E23,-3.9E24/
      DIMENSION CSAV(50)
      COMMON /DESKC/ ICSP,ICSTK(50),LINENO,ILFLG,VL4S(50)
      COMMON /EVAL2/ ISP,IOP,IERR,STPOS,IP,IRP,ILP,STACK,STRING
      INTEGER*1 STRNG(132),STRING(134),BB
      COMMON /VARS/ NVARS,VARNAMS,VL1(50),VL2(50),VL3(50),VL4(50),DEGFLG
      LOGICAL*1 DEGFLG,VARNAMS(10,50)
      integer*1 byteupcase
C       
      do i=1,50
        do j=1,6
          func(j,i)=ichar(fcns(i)(j:j))
          if(func(j,i).eq.0)func(j,i)=32
        enddo
      enddo

      JERR=1
      RESULT=0
      STRING(1)=ichar('(')                      !Enclose expression in parenthesis
      LENGTH=1
      I=1
      DO WHILE (STRNG(I).NE.0)                  !Remove spaces and tabs
        IF (.NOT.(STRNG(I).EQ.ichar(' ').OR.STRNG(I).EQ.9)) THEN
          LENGTH=LENGTH+1
          STRING(LENGTH)=STRNG(I)
          IF (STRING(LENGTH).EQ.ichar('*').AND.
     &        STRING(LENGTH-1).EQ.ichar('*'))THEN
            LENGTH=LENGTH-1                     !Convert ** to ^
            STRING(LENGTH)=ichar('^')
          ENDIF
        ENDIF
        I=I+1
        IF (I.GT.131) THEN
          WRITE(*,*)' Sorry, the expression is too long'
          RETURN
        ENDIF
      ENDDO
      LENGTH=LENGTH+1
      STRING(LENGTH)=ichar(')')
      ISP=1
      IERR=0
C       
5     DO IRP=1,LENGTH                           !Find leftmost parenthesis pair
        IF (STRING(IRP).EQ.ichar('(')) ILP=IRP
        IF (STRING(IRP).EQ.ichar(')')) GOTO 10
      ENDDO
10    IF (ILP.EQ.1.OR.IRP.EQ.LENGTH) THEN
        IF (ILP.NE.1.OR.IRP.NE.LENGTH) THEN
          WRITE(*,*)' Sorry, unbalanced parenthesis'
          RETURN
        ENDIF
      ENDIF
      IOP=0                                     !Clear operand flag
      DO IP=ILP+2,IRP-2                         !Scan for operators by precedence
        IF (STRING(IP).EQ.ichar('<')) CALL PUSH2(7)
        IF (STRING(IP).EQ.ichar('>')) CALL PUSH2(6)
      ENDDO
      DO IP=ILP+2,IRP-2
        IF (STRING(IP).EQ.ichar('^')) CALL PUSH2(5)
      ENDDO
      DO IP=ILP+2,IRP-2
        IF (STRING(IP).EQ.ichar('/')) CALL PUSH2(4)
        IF (STRING(IP).EQ.ichar('*')) CALL PUSH2(3)
      ENDDO
      DO IP=ILP+2,IRP-2
        IF (.NOT.(byteupcase(STRING(IP-1)).EQ.ichar('E').AND.
     &      STRING(IP-2).GT.47 .AND.STRING(IP-2).LT.58)) THEN
          IF (STRING(IP).EQ.ichar('-')) CALL PUSH2(2)
          IF (STRING(IP).EQ.ichar('+')) CALL PUSH2(1)
        ENDIF
      ENDDO
      IF (IERR.NE.0) GOTO 99                    !If errors in push2
      IF (IOP.EQ.0) THEN                        !No operands, must be constant
        IF (STRING(ILP+1).EQ.0) GOTO 58         !He likes parenthesis
        J=0
        DO I=ILP+1,IRP-1
          J=J+1
          TERM(J)=STRING(I)
          STRING(I)=0
          STPOS(I)=ISP
        ENDDO
        CALL PUSH1(J,TERM)
        IF (IERR.NE.0) GOTO 99
        STACK(ISP)=STBRK
c         print *,'eval',isp,stack(isp)
        ISP=ISP+1
      ENDIF
58    STRING(ILP)=0                             !Wipe out the parenthesis
      STRING(IRP)=0
      STPOS(ILP)=STPOS(ILP+1)                   !And record polish index
      STPOS(IRP)=STPOS(IRP-1)
C       
      DO 50 I=1,NFUNCS                          !See if function precedes
        IP=ILP-1
        IFL=1
40      IF (FUNC(IFL,I).EQ.ichar(' ')) THEN
          IF (I.GT.16.AND.I.LT.20) THEN
            N=0
            SMAX=DUNDEF
            SMIN=-DUNDEF
            SUM=0.
            ISP=STPOS(ILP)
            MORE=-ISP                           !Special for max,min,sum fcns
41          CALL EVLPOL(RESULT,JERR,MORE)
            IF (RESULT.EQ.DUNDEF) GOTO 43
            N=N+1
            IF (SMAX.LT.RESULT.AND.I.EQ.17) THEN
              SMAX=RESULT
              DO II=1,ICSP
                CSAV(II)=VL4S(II)
              ENDDO
              CSAV(50)=VL1(2)
            ELSE IF (SMIN.GT.RESULT.AND.I.EQ.18) THEN
              SMIN=RESULT
              DO II=1,ICSP
                CSAV(II)=VL4S(II)
              ENDDO
              CSAV(50)=VL1(2)
            ELSE
              SUM=SUM+RESULT
            ENDIF
43          IF (MORE.NE.0) GOTO 41
            WRITE(*,*)N,' Points evaluated'
            IF (I.EQ.17) THEN
              SMIN=SMAX
              WRITE(*,*)'Maximum is',SMAX
            ELSE IF (I.EQ.18) THEN
              WRITE(*,*)'Minimum is',SMIN
            ELSE
              SMIN=SUM
              WRITE(*,*)'Sum is',SUM
              GOTO 44
            ENDIF
            WRITE (6,1402) ((VARNAMS(K,ICSTK(II)),K=1,10),CSAV(II),II=1,ICSP)
1402        FORMAT(' AT:'/(1X,10A1,'=',E20.5))
            IF (ILFLG.NE.0) WRITE(*,*)' Line #',INT(CSAV(50))
44          STACK(ISP)=SMIN                     !The expression becomes const.
            VL1(1)=N
            GOTO 42
          ENDIF
          STACK(ISP-1)=OPCODE                   !Yes, push on stack
          STACK(ISP)=I+9
c           print *,'eval replace',isp,opcode,stack(isp)
          IF (IP.EQ.0) GOTO 42
          BB = ichar(' ')
          if (ip.gt.1) bb = string(ip-1)
          IF ((STRING(IP).EQ.ichar('+').OR.STRING(IP).EQ.ichar('-')) .AND.
     &        (BB.EQ.ichar('(').OR.BB.EQ.ichar('*').OR.
     &        BB.EQ.ichar('/').OR.BB.EQ.ichar('+').OR.
     &        BB.EQ.ichar('-').OR.BB.EQ.ichar('>').OR.
     &        BB.EQ.ichar('<'))) THEN
            IF (STRING(IP).EQ.ichar('-')) THEN
              STACK(ISP+1)=OPCODE
              STACK(ISP+2)=8
c               print *,'eval minus',isp, opcode, 8
              ISP=ISP+2
            ENDIF
            IP=IP-1                             !Remove unary +,-
          ENDIF
42        STACK(ISP+1)=STBRK
c           print *,'eval',isp,stbrk
          ISP=ISP+2
          DO J=IP+1,ILP-1                       !And remove from line
            STPOS(J)=STPOS(ILP)
            STRING(J)=0
          ENDDO
          ILP=IP                                !Back up L paren pointer
          GOTO 51
        ENDIF
        IF (IP.EQ.0) GOTO 50
        IF (byteupcase(STRING(IP)).EQ.FUNC(IFL,I)) THEN
45        IP=IP-1
          IFL=IFL+1
          GOTO 40
        ENDIF
50    continue
51    IF (ILP.GT.1.OR.IRP.LT.LENGTH) GOTO 5     !Loop until outer parens found
      ISPO=STPOS(1)-1
      DO ISP=1,1000
        STACK(ISP)=STACK(ISP+ISPO)
        IF (STACK(ISP).EQ.STBRK) GOTO 100
      ENDDO
100   CONTINUE
C       WRITE (6,143) ISP,(I,STACK(I),I=1,ISP)
C       143     FORMAT(' STACK LENGTH=',I5,' CONTENTS-'/(I5,E20.5))
99    IF (IERR.EQ.-1) THEN
        WRITE(*,*)' Sorry, there is an error in the calculation syntax'
      ELSE IF (IERR.EQ.2) THEN
        WRITE(*,*)' Sorry, the expression has an undefined variable'
      ENDIF
      JERR=IERR
      RETURN
      END



      SUBROUTINE EVLPOL(RESULT,JERR,MORE)
C       
C       EVALUATE THE REVERSE POLISH CREATED ABOVE
C       
      INTEGER*2 STPOS(132)
      REAL*4 STACK(1000)
      COMMON /EVAL2/ ISP,IOP,IERR,STPOS,IP,IRP,ILP,STACK,STRING
      LOGICAL*1 DEGFLG,VARNAMS(10,50)
      integer*1 STRING(134)
      DATA STBRK,OPCODE,VARCODE,DUNDEF/-3.9E21,-3.9E22,-3.9E23,-3.9E24/
      DIMENSION A(50)
      COMMON /DESKC/ ICSP,ICSTK(50),LINENO,ILFLG,VL4S(50)
      COMMON /DATA/ NLINES,NVVARS,IVARLST(20),VAL(2000,20)
      COMMON /VARS/ NVARS,VARNAMS,VL1(50),VL2(50),VL3(50),VL4(50),DEGFLG
      DATA RTOD/57.29577951/
      IF (MORE.LE.0) THEN
        ISPF=1                                  !First time through
        IPFLG=0
        IF (MORE.LT.0) ISPF=-MORE               !For min, max functions
        IF (MORE.LT.0) IPFLG=1
        LINENO=1
        ILFLG=0                                 !Find what vars we need
        ICSP=0
        ISPL=ISPF
        DO WHILE (STACK(ISPL).NE.STBRK)
          IF (STACK(ISPL).EQ.VARCODE) THEN
            ISPL=ISPL+1
            II=STACK(ISPL)
            TEMP=VL3(II)
            IF (TEMP.EQ.DUNDEF) THEN
              ILFLG=1                           !We need line numbers
            ELSE IF (TEMP.NE.0.) THEN
              DO I=1,ICSP
                IF (ICSTK(I).EQ.II) GOTO 5
              ENDDO
              ICSP=ICSP+1
              ICSTK(ICSP)=II                    !We need variable II
            ENDIF
          ENDIF
5         ISPL=ISPL+1
        ENDDO
      ENDIF
      DO I=1,ICSP                               !Save values for min, max
        VL4S(I)=VL4(ICSTK(I))
      ENDDO
      VL1(2)=LINENO
      VL4(2)=LINENO
      JERR=1
      RESULT=DUNDEF
      L=0
      ISPL=ISPF
      MORE=1
      DO WHILE (STACK(ISPL).NE.STBRK)
        IF (STACK(ISPL).EQ.OPCODE) THEN
          ISPL=ISPL+1
          IOP=STACK(ISPL)
          IF (IOP.EQ.1) THEN
            A(L-1)=A(L)+A(L-1)
            L=L-1
          ELSE IF (IOP.EQ.2) THEN
            A(L-1)=A(L)-A(L-1)
            L=L-1
          ELSE IF (IOP.EQ.3) THEN
            A(L-1)=A(L)*A(L-1)
            L=L-1
          ELSE IF (IOP.EQ.4) THEN
            IF (A(L-1).EQ.0.) THEN
              IF (IPFLG.EQ.0) WRITE(*,*)' Division by zero'
              GOTO 50
            ENDIF
            A(L-1)=A(L)/A(L-1)
            L=L-1
          ELSE IF (IOP.EQ.5) THEN
            IF (A(L).LT.0.) THEN
              IF (A(L-1).LT.1.) THEN
                IF (IPFLG.EQ.0) WRITE(*,*)' Neg. number to Neg. power'
                GOTO 50
              ELSE IF (INT(A(L-1)).NE.A(L-1)) THEN
                IF (IPFLG.EQ.0) WRITE(*,*)' Neg. number to frac. power'
                GOTO 50
              ENDIF
            ENDIF
            IF (INT(A(L-1)).EQ.A(L-1)) THEN
              A(L-1)=A(L)**INT(A(L-1))
            ELSE
              A(L-1)=A(L)**A(L-1)
            ENDIF
            L=L-1
          ELSE IF (IOP.EQ.6) THEN
            IF (A(L).GT.A(L-1)) THEN
              A(L-1)=1.
            ELSE
              A(L-1)=0.
            ENDIF
            L=L-1
          ELSE IF (IOP.EQ.7) THEN
            IF (A(L).LT.A(L-1)) THEN
              A(L-1)=1.
            ELSE
              A(L-1)=0.
            ENDIF
            L=L-1
          ELSE IF (IOP.EQ.8) THEN
            A(L)=-A(L)
          ELSE IF (IOP.EQ.10) THEN
            IF (A(L).LT.0.) THEN
              IF (IPFLG.EQ.0) WRITE(*,*)
     &            ' Square root arg. < 0, absolute value taken'
              A(L)=-A(L)
            ENDIF
            A(L)=SQRT(A(L))
          ELSE IF (IOP.EQ.11) THEN
            IF (A(L).LE.0.) THEN
              IF (IPFLT.EQ.0) WRITE(*,*) ' Log arg. .LE. 0 '
              GOTO 50
            ENDIF
            A(L)=LOG(A(L))
          ELSE IF (IOP.EQ.12) THEN
            IF (A(L).LE.0.) then
              IF (IPFLT.EQ.0) WRITE(*,*) ' Log arg. .LE. 0 '
              GOTO 50
            ENDIF
            A(L)=LOG10(A(L))
          ELSE IF (IOP.EQ.13) THEN
            A(L)=EXP(A(L))
          ELSE IF (IOP.EQ.14) THEN
            IF (ABS(A(L)).GT.1.) THEN
              IF (IPFLG.EQ.0) WRITE(*,*)' Arc sine arg. > 1 '
              GOTO 50
            ENDIF
            A(L)=ASIN(A(L))
            IF (DEGFLG) A(L)=A(L)*RTOD
          ELSE IF (IOP.EQ.15) THEN
            IF (ABS(A(L)).GT.1.) THEN
              IF (IPFLG.EQ.0) WRITE(*,*)' Arc cosine arg. > 1 '
              GOTO 50
            ENDIF
            A(L)=ACOS(A(L))
            IF (DEGFLG) A(L)=A(L)*RTOD
          ELSE IF (IOP.EQ.16) THEN
            A(L)=ATAN(A(L))
            IF (DEGFLG) A(L)=A(L)*RTOD
          ELSE IF (IOP.EQ.17) THEN
            IF (DEGFLG) A(L)=A(L)/RTOD
            A(L)=SIN(A(L))
          ELSE IF (IOP.EQ.18) THEN
            IF (DEGFLG) A(L)=A(L)/RTOD
            A(L)=COS(A(L))
          ELSE IF (IOP.EQ.19) THEN
            IF (DEGFLG) A(L)=A(L)/RTOD
            A(L)=TAN(A(L))
          ELSE IF (IOP.EQ.20) THEN
            A(L)=SINH(A(L))
          ELSE IF (IOP.EQ.21) THEN
            A(L)=COSH(A(L))
          ELSE IF (IOP.EQ.22) THEN
            A(L)=TANH(A(L))
          ELSE IF (IOP.EQ.23) THEN
            A(L)=ABS(A(L))
          ELSE IF (IOP.EQ.24) THEN
            A(L)=INT(A(L))
          ELSE IF (IOP.EQ.28) THEN
            IF (A(L).GE.0) THEN
              A(L)=1.
            ELSE IF (A(L).LT.0) THEN
              A(L)=-1.
            ENDIF
          ENDIF
        ELSE IF (STACK(ISPL).EQ.VARCODE) THEN
          ISPL=ISPL+1
          L=L+1
          A(L)=VL4(int(STACK(ISPL)))
          IF(A(L).EQ.DUNDEF) GOTO 50
        ELSE
          L=L+1
          A(L)=STACK(ISPL)
          IF(A(L).EQ.DUNDEF) GOTO 50
        ENDIF
        ISPL=ISPL+1
      ENDDO
      IF (L.NE.1) WRITE(*,*)'Serious bug in program'
      RESULT=A(L)
      JERR=0
50    DO IC=1,ICSP
        ICC=ICSTK(IC)
        IF (VL3(ICC).NE.0.AND.VL3(ICC).NE.DUNDEF) THEN
          VL4(ICC)=VL4(ICC)+VL3(ICC)            !Increment do loop
          IF (VL3(ICC).GT.0.AND.VL4(ICC).LE.VL2(ICC)) GOTO 51
          IF (VL3(ICC).LT.0.AND.VL4(ICC).GE.VL2(ICC)) GOTO 51
          VL4(ICC)=VL1(ICC)
        ENDIF
      ENDDO             
      IF (ILFLG.NE.0) THEN
        LINENO=LINENO+1
        IF (LINENO.GE.NLINES) THEN
          LINENO=1
          MORE=0
        ENDIF
        DO IC=1,NVVARS
          IF (IVARLST(IC).NE.0) VL4(IVARLST(IC))=VAL(LINENO,IC)
        ENDDO
      ELSE
        MORE=0
      ENDIF
51    RETURN
      END



      SUBROUTINE PUSH2(JOP)
      INTEGER*2 STPOS(132)
      REAL*4 STACK(1000)
      DATA STBRK,OPCODE,VARCODE,DUNDEF/-3.9E21,-3.9E22,-3.9E23,-3.9E24/
      COMMON /EVAL2/ ISP,IOP,IERR,STPOS,IP,IRP,ILP,STACK,STRING
      INTEGER*1 B,BB,STRING(134),TERM(132)
      integer*1 byteupcase
      IOP=1                                     !Show an operand occurred
      IT=0
      ITSP=ISP
      STPOS(IP)=ITSP                            !Record where polish is going
      STRING(IP)=0
      DO I=IP+1,IRP                             !Find the right operand
        B=STRING(I)
        IF (B.EQ.0) THEN                        !It's on the stack
C           IF (IT.NE.0) WRITE(*,*)'IT NE 0, GOING TO 99'
          IF (IT.NE.0) GOTO 99                  !Syntax error if decoding value
          J=I
          K=STPOS(I)
          DO WHILE (STRING(J).EQ.0)
            STPOS(J)=ITSP                       !Record where its going
            J=J+1
          ENDDO
          DO WHILE (STACK(K).NE.STBRK)          !And copy it up
            STACK(ISP)=STACK(K)
c             print *,'push2',isp,stack(isp)
            ISP=ISP+1
            K=K+1
          ENDDO 
          GOTO 10
        ELSE IF (B.EQ.ichar('*').OR.B.EQ.ichar('/').OR.B.EQ.ichar(')')
     &        .OR.B.EQ.ichar('^') .OR.B.EQ.ichar('<')
     &        .OR.B.EQ.ichar('>')) THEN         !Decode the constant
          CALL PUSH1(IT,TERM)
          GOTO 10
        ELSE IF (B.EQ.ichar('-').OR.B.EQ.ichar('+')) THEN
          IF (IT.EQ.0) GOTO 88                  !Check for unary +,-
          IF (IT.GT.2.AND.byteupcase(TERM(IT)).EQ.ichar('E')
     &        .AND.TERM(IT-1)-58.LT.0)GOTO88
          CALL PUSH1(IT,TERM)
          GOTO 10
        ENDIF
c         DNM: since all of the above if clauses end in a goto 10, the next
c         item can be moved out of an else clause and be made legal to go to
88      IT=IT+1
        TERM(IT)=B                              !Copy the operand
        STRING(I)=0                             !Eliminate the character
        STPOS(I)=ITSP                           !And record where term will be
      ENDDO
      STOP 'This error 1 can''t happen'
C       
10    IT=0
      DO II=ILP,IP-1
        I=IP-1-II+ILP   
        B=STRING(I)
        IF (B.EQ.0) THEN                        !It's on the stack
C           WRITE(*,*)'2 IT NOT 0, GOTO 99'
          IF (IT.NE.0) GOTO 99                  !Syntax error if decoding value
          J=I
          K=STPOS(I)
          DO WHILE (STRING(J).EQ.0)
            STPOS(J)=ITSP                       !Record where its going
            J=J-1
          ENDDO
          DO WHILE (STACK(K).NE.STBRK)          !And copy it up
            STACK(ISP)=STACK(K)
c             print *,'push2',isp,stack(isp)
            ISP=ISP+1
            K=K+1
          ENDDO 
          GOTO 20
        ELSE IF (B.EQ.ichar('+').OR.B.EQ.ichar('-').OR.B.EQ.ichar('*')
     &        .OR.B.EQ.ichar('/').OR.B.EQ.ichar('(')
     &        .OR.B.EQ.ichar('<').OR.B.EQ.ichar('>')
     &        .OR.B.EQ.ichar('^')) THEN         !Decode the constant
          CALL PUSH1(IT,TERM(20-IT))
          BB=ichar(' ')
          if (i.gt.1)BB=STRING(I-1)
          IF ((B.EQ.ichar('+').OR.B.EQ.ichar('-')).AND.
     &        (BB.EQ.ichar('*').OR.BB.EQ.ichar('/') .OR.BB.EQ.ichar('(')
     &        .OR.BB.EQ.ichar('<').OR.BB.EQ.ichar('>')
     &        .OR.BB.EQ.ichar('^'))) THEN
            IF (B.EQ.ichar('-')) THEN
              STACK(ISP)=OPCODE
              STACK(ISP+1)=8                    !Special code for unary minus
c               print *,'push2',isp,stack(isp), 8
              ISP=ISP+2
            ENDIF
            STRING(I)=0                         !Ignore unary plus
            STPOS(I)=ITSP
          ENDIF
          GOTO 20
        ELSE 
          IT=IT+1
          TERM(20-IT)=B                         !Copy the operand
        ENDIF
        STRING(I)=0                             !Eliminate the character
        STPOS(I)=ITSP                           !And record where term will be
      ENDDO
      STOP 'This error 2 can''t happen'
C       
20    STACK(ISP)=OPCODE                         !Push the operand
      STACK(ISP+1)=JOP
      STACK(ISP+2)=STBRK                        !Flag end of term
c       print *,'push2',isp,stack(isp),jop,stbrk
      ISP=ISP+3
      RETURN
99    IERR=-1
      RETURN
      END



      SUBROUTINE PUSH1(IT,TERM)
C       
C       PUSH AN OPERAND ON THE STACK
      INTEGER*2 STPOS(132)
      INTEGER*1 STRING(134),VARNAMS(10,50),TERM(20),DEGFLG
      DATA STBRK,OPCODE,VARCODE,DUNDEF/-3.9E21,-3.9E22,-3.9E23,-3.9E24/
      COMMON /EVAL2/ ISP,IOP,IERR,STPOS,IP,IRP,ILP,STACK(1000),STRING
      COMMON /VARS/ NVARS,VARNAMS,VL1(50),VL2(50),VL3(50),VL4(50),DEGFLG
      character*20 temp
      integer*1 byteupcase

      do i=1,it
        temp(i:i)=char(term(i))
      enddo

      IF (IT.EQ.0) THEN
        IERR=-1
        RETURN
      ENDIF
      IF (byteupcase(TERM(1)).EQ.ichar('E')) GOTO 10 !SPECIAL CHECK FOR E
c       DECODE (IT,'(F20.0)',TERM,ERR=10) STACK(ISP)
      read(temp(1:it),*,ERR=10) STACK(ISP)
c       print *,'push1',isp,stack(isp)
      ISP=ISP+1                                 !Was constant if successful
      RETURN
10    JFIRST=1
      IF (TERM(1).EQ.ichar('-').OR.TERM(1).EQ.ichar('+')) JFIRST=2
      DO 50 I=1,NVARS                           !Else search for variable
        DO J=JFIRST,IT
          IF (TERM(J).NE.VARNAMS(J-JFIRST+1,I)) GOTO 50
        ENDDO
        IF (VARNAMS(IT-JFIRST+2,I).NE.ichar(' ')) GOTO 50
        IF (VL3(I).EQ.0.) THEN                  !Simple constant
          STACK(ISP)=VL1(I)
          IF (TERM(1).EQ.ichar('-')) STACK(ISP)=-STACK(ISP)
c           print *,'push1',isp,stack(isp)
          ISP=ISP+1
        ELSE
          STACK(ISP)=VARCODE                    !Push var no.
          STACK(ISP+1)=I
c           print *,'push1',isp,stack(isp),i
          ISP=ISP+2             
C           UNARY MINUS?
        ENDIF
        RETURN
50    continue
      IERR=2                                    !Variable not found
      STACK(ISP)=DUNDEF
      ISP=ISP+1
      RETURN
      END



      SUBROUTINE PUSHVAR(TERM,IT,IPOS)
C       
C       DEFINE A VARIABLE
      INTEGER*2 STPOS(132)
      INTEGER*1 VARNAMS(10,50),TERM(20),string(134)
      DATA STBRK,OPCODE,VARCODE,DUNDEF/-3.9E21,-3.9E22,-3.9E23,-3.9E24/
      logical*1 degflg
      COMMON /EVAL2/ ISP,IOP,IERR,STPOS,IP,IRP,ILP,STACK(1000),STRING
      COMMON /VARS/ NVARS,VARNAMS,VL1(50),VL2(50),VL3(50),VL4(50),DEGFLG
c       CHARACTER*10 VARNMS(50)
c       EQUIVALENCE (VARNMS,VARNAMS)
      DATA NVARS/6/
c       DATA VARNMS/'#','L#','PI','pi','E','e',44*' '/
      DATA VL1/2*0.,2*3.1415926535,2*2.71828182,44*0./
      DATA VL2/50*0./
      DATA VL3/0.,-3.9E24,48*0./
      data degflg/.true./
      character*20 temp
      integer*1 byteupcase

      do i=1,it
        temp(i:i)=char(term(i))
      enddo

      IPOS=0                                    !0 return means error
      NVAR1=NVARS+1
      IF (IT.GT.10) RETURN
      DO I=1,IT                                 !Put name at end of list
        VARNAMS(I,NVAR1)=TERM(I)
      ENDDO
      IF (byteupcase(TERM(1)).EQ.ichar('E')) GOTO 10
c       DECODE (IT,'(F20.0)',TERM,ERR=10) TEST
      read (temp(1:it),*,ERR=10) TEST
      RETURN                                    !Constant on left side
10    DO 11 IPOS=1,NVAR1                        !Now search for the name
        DO J=1,IT
          IF (TERM(J).NE.VARNAMS(J,IPOS)) GOTO 11
        ENDDO
        GOTO 12
11    continue
12    IF (IPOS.EQ.NVAR1) NVARS=NVAR1
      IF (NVARS.GT.50) THEN
        WRITE(*,*)'No more space for variable names'
        IPOS=0
        RETURN
      ENDIF
      VL1(IPOS)=DUNDEF                          !Show undefined
      RETURN
      END

      integer*1 function byteupcase(ch)
      integer*1 ch
      byteupcase = ch
      if (ch.ge.ichar('a').and.ch.le.ichar('z')) byteupcase = ch-32
      return
      end

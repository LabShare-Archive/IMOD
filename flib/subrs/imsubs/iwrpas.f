C*IWRPAS
C
C	Writes out a part of a section, converting to Integer*1 or 2
C	or 9-15 bit mode as required.
C	After writing a partial line, the pointer is always advanced
C	to the start of the next line.
C NOTE:	The start of a line is ALWAYS 0 (ie NX1,NX2, NY1,NY2 are relative)
C
C	MX,MY		: Dimesnions of ARRAY
C			: for complex numbers (MODES 3 & 4)
C			: MX MUST be multiplied by 2 (ie # of REALS)
C	NX1,NX2		: Start and stop Column numbers (in COMPLEX if 3,4)
C	NY1,NY2		: Start and stop Line numbers
C
C	ARRAY DIMENSIONS ARE FOR CORRECT TYPE FOR REALS!!
C	MUST MULTIPLY MX*2 FOR COMPLEX!!!
C	BUT NX1,NX2 REFER TO COMPLEX NUMBERS!!!
C
	SUBROUTINE IWRPAS(ISTREAM,ARRAY,MX,MY,NX1,NX2,NY1,NY2)
	include 'imsubs.inc'
C
	DIMENSION ARRAY(MX,MY)
C
	J = LSTREAM(ISTREAM)
	JMODE = MODE(J)
        jb=1
        if(jmode.le.4) JB = NB(JMODE + 1)
	NCB = NCRS(1,J)*JB
C
	DO 100 JY = NY1+1,NY2+1				!FOR START @ 0
	  CALL IWRPAL(ISTREAM,ARRAY(1,JY),NX1,NX2)
100	CONTINUE
C
C   MAY HAVE TO SKIP TO END OF SECTION
c   mast simplified this
        npbleft=ncb*(ncrs(2,j)+ny1-1-ny2)  !# of bytes/pixels left in section
        if(npbleft.gt.0)call altskip(j,npbleft,*99)
        ibleft(j)=0                    !move to byte boundary at end of section
C
	RETURN
99      print *,'error doing read from bitskip'
        return
	END

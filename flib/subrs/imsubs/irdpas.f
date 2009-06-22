C*IRDPAS
C
C	Reads in a part of a section, converting from Integer*1 or 2 or
C	9-15 bit mode as required. After the read, the pointer is positioned at
C	the start of the next section. Array is first cleared
C	and then the selected portion of the image is loaded in.
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
c
c       $Id$
c       
c       $Log$
C
      SUBROUTINE IRDPAS(ISTREAM,ARRAY,MX,MY,NX1,NX2,NY1,NY2,*)
      use imsubs
      implicit none
C
      integer*4 MX,MY,NX1,NX2,NY1,NY2,ISTREAM
      real*4 ARRAY(MX,MY)
      integer*4 j, jmode, jb, ncb, jy,nlinleft,ndo
      integer(kind=8) nread
C
      J = LSTREAM(ISTREAM)
      JMODE = MODE(J)
      jb=1
      if(jmode.le.6) JB = NB(JMODE + 1)
      NCB = NCRS(1,J)*JB
c       
c       6/20/09: Zero array only if it is under 2 Gpixel limit
      nread = int(MX, kind=8) * MY*NBW
      if (nread .lt. 2147000000) CALL ZERO(ARRAY, int(nread, kind = 4))
c      
      if(spider(j))then
        lrecspi(j)=lrecspi(j)-ny1		!advance properly for SPIDER
      else
        CALL ALTSKIP(J,ncb,ny1,*99)             !NY1 IS RELATIVE OFFSET
      endif
      NDO = NY2 - NY1 + 1
C       
      if(nx1.eq.0.and.nx2.eq.(ncrs(1,j)-1).and.mx.eq.ncrs(1,j))then
        call irdsecl(istream,array,ndo,*99)
      else
        DO 100 JY = 1,NDO
          CALL IRDPAL(ISTREAM,ARRAY(1,JY),NX1,NX2,*99)
100     CONTINUE
      endif
C
C       MAY HAVE TO SKIP TO END OF SECTION
C       mast simplified this then changed to handle SPIDER file
      nlinleft=(ncrs(2,j)-1-ny2)		!# of lines left in section
      if(nlinleft.gt.0)then
        if(spider(j))then
          lrecspi(j)=lrecspi(j)-nlinleft
          if(mod(lrecspi(j),ncrs(2,j)).eq.0)
     &        lrecspi(j)=lrecspi(j)+2*ncrs(2,j)
        else
          call altskip(j,ncb,nlinleft,*99)
        endif
      endif
      ibleft(j)=0                   !move to byte boundary at end of section
C
      RETURN
99    RETURN 1
      END

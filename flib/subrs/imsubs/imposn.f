C*IMPOSN
C
C	Position Read/Write pointer to RELATIVE Section NZ and Line NY
C
c   modified by mast to position correctly in bit mode files
c	  DNM 3/1/01: change to include file, move initialization of 
c	  ibleft to imopen.f
c
	SUBROUTINE IMPOSN(ISTREAM,NZ,NY)
	include 'imsubs.inc'
C
	J = LSTREAM(ISTREAM)
	FLAG(J) = .FALSE.
	NSO = NZ + 1
	IF (NSO .LT. 1) NSO = 1
	NRO = NY
	IF (NRO .LT. 0) NRO = 0
c	  
	if(spider(j))then			!if SPIDER file
	  lrecspi(j)=nso*ncrs(2,j)-nro
	  return
	endif
        if(mode(j).lt.8)then           !if regular modes
          JB = NB(MODE(J) + 1)
          NRC = NRO*NCRS(1,J)*JB + 1
          NSIZE = NCRS(1,J)*NCRS(2,J)*JB
          CALL QSEEK(J, NSO, NRC + NBHDR + nbsym(j), NSIZE)
        else                           !if bit modes, seek to start of section
          nsize = (ncrs(1,j)*ncrs(2,j)*mode(j)+7)/8
          call qseek(j, nso,1+ nbhdr + nbsym(j), nsize)
          npixel=nro*ncrs(1,j)         !# of pixels left to skip
          ibleft(j)=0                  !from an even boundary
          call bitskip(j,npixel,*99)
        endif
C
	RETURN
99      print *,'error doing read from bitskip'
        return
	END

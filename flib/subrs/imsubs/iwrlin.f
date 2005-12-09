C*IWRLIN
C*IWRSEC
C*IWRPAL
c*iwrsecl  writes nlines (added 14.feb.89  dnm to speed up iwrpas)
C
C	Write out a line, a section or part of a line (NX1 - NX2)
C	from the REAL or COMPLEX REAL array ARRAY. If required,
C	data is converted to Integer*1, Integer*2 or 9-15 bit format
c	before writing to disk.  In each case the calling program is
c	responsible for supplying real values between 0 and 2**nbits-1.
C	After writing a partial line, the pointer is always advanced
C	to the start of the next line.
C NOTE:	The start of a line is ALWAYS 0 (ie NX1,NX2 are relative)
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.4  2005/11/15 19:56:00  mast
c	  Fixed unsigned write for big endian
c	
c	  Revision 3.3  2005/11/11 22:36:22  mast
c	  Changes for unsigned mode
c	
c	  Revision 3.2  2002/07/21 19:17:20  mast
c	  Standardized error output to ERROR: ROUTINE
c	
c	  Revision 3.1  2002/06/26 00:27:51  mast
c	  Added ability to write back to byte swapped files, and changed STOPs
c	  to call exit(1)
c	
C
	SUBROUTINE IWRLIN(ISTREAM,ARRAY)
C
	include 'imsubs.inc'
        logical noconj
	include 'endian.inc'
C
	DIMENSION ARRAY(*)
	parameter (linesize = 8192)
	INTEGER*2 LINE(linesize/2),IB,iscratch(7300)
        INTEGER*1 BLINE(linesize),QB(2),bstore(4),bcur
	real*4 fline(linesize/4)
        integer*4 lcompos
	logical bad /.true./
	EQUIVALENCE (LINE,BLINE),(IB,QB),(lcompos,bstore(1)),(fline,line)
C
	ITYPE = 1
	GOTO 1
	ENTRY IWRSEC(ISTREAM,ARRAY)
        nlines = ncrs(2,lstream(istream))
	ITYPE = 2
	GOTO 1
	ENTRY IWRPAL(ISTREAM,ARRAY,NX1,NX2)
	ITYPE = 3
	GOTO 1
	ENTRY IWRSECL(ISTREAM,ARRAY,mlines)
	ITYPE = 2
	nlines = mlines
        GOTO 1
C
1	J = LSTREAM(ISTREAM)
	if(spider(j))then
	  print *
	  print *,'ERROR: IWRLIN - TRYING TO WRITE TO SPIDER FILE'
	  call exit(1)
	endif

	JMODE = MODE(J)
        jb=1
        if(jmode.le.6) JB = NB(JMODE + 1)
	INDEX = 1
C   DNM: eliminate this because qlocate gives suspect results for giant files
C  and it's not needed anymore because irdhdr and iwrhdr take care of it
c        call qlocate(j,location)
c        if(location.ge.0.and.location.le.nbhdr+nbsym(j)+1)ibleft(j)=0
	IF (FLAG(J)) THEN				!MAKE SURE PAST HEADER
	  CALL QSEEK(J,2,1,NBHDR+nbsym(j))
	  FLAG(J) = .FALSE.
          ibleft(j)=0
	END IF
C
	IF (ITYPE .EQ. 1) THEN				!WRITE LINE
	  NWRITE = NCRS(1,J)*JB
	ELSE IF (ITYPE .EQ. 2) THEN			!WRITE SECTION
	  NWRITE = NCRS(1,J)*nlines*JB
	ELSE IF (ITYPE .EQ. 3) THEN			!WRITE PART OF LINE
	  NWRITE = (NX2 - NX1  + 1)*JB
	  INDEX = NX1 + 1				!FOR START @0
	  IF (JMODE .GE. 3.and.jmode.le.8) INDEX = 2*INDEX - 1
	END IF
C
	IF (JMODE .EQ. 2 .OR. JMODE .EQ. 4 .OR.
     &      (NOCON(J) .AND. JMODE .LT. 9)) THEN
c
C	    direct write of floats, or no-conversion writes if not bit modes
c
	  if (mrcflip(j) .and. (jmode .eq. 1 .or. jmode .eq. 3)) then
c
c	    for swapped ints, which are already ints in the "real" array
c	      
	    NWRITE = NWRITE/2
	    do while (NWRITE .GT. 0)
	      N = MIN(linesize/2,NWRITE)
	      call imrltoin(array,line,index,n)
	      index=index+n
	      call convert_shorts(line, n)
	      CALL QWRITE(J,LINE,N*2)
	      NWRITE = NWRITE - n
	    enddo
	    
	  else if (mrcflip(j) .and. (jmode .eq. 2 .or. jmode .eq. 4)) then
c	      
c	      for swapped floats it is easier
c	      
	    NWRITE = NWRITE/4
	    do while (NWRITE .GT. 0)
	      N = MIN(linesize/4,NWRITE)
	      DO K = 1,N
		FLINE(K) = array(index)
		INDEX = INDEX + 1
	      enddo
	      call convert_floats(fline, n)
	      CALL QWRITE(J,FLINE,N*4)
	      NWRITE = NWRITE - n
	    enddo
	    
	  else
c	      everything else just writes out straight
	    CALL QWRITE(J,ARRAY(INDEX),NWRITE)
	  endif

	ELSE IF (JMODE .EQ. 0) THEN			!INTEGER*1
	  do while (NWRITE .GT. 0)
	    N = MIN(linesize,NWRITE)
	    DO K = 1,N
c		DNM: changed this test to possibly speed it up; put value into
c		denval, use a single if statement
	      denval=array(index)
	      if (denval .gt. 255. .or. denval .lt. 0.) then
		denval = min(255.,max(denval,0.))
		if (bad) print *,'** overflow on byte conversion ***'
		bad = .false.
	      endif	      
	      IB = NINT(denval)
	      BLINE(K) = QB(lowbyte)
	      INDEX = INDEX + 1
	    enddo
	    CALL QWRITE(J,BLINE,N)
	    NWRITE = NWRITE - n
	  enddo

	ELSEif(jmode.le.8)then         !ELSE INTEGER*2
	  NWRITE = NWRITE/2
	  do while (NWRITE .GT. 0)
	    N = MIN(linesize/2,NWRITE)
	    if (jmode .ne. 6) then		! SIGNED
	      DO K = 1,N
c		  DNM: similar changes, also make limit 32767 instead of 32700.
		denval=array(index)
		if (denval .gt. 32767. .or. denval .lt. -32767.) then
		  denval = min(32767.,max(denval,-32767.))
		  if (bad) print *,'** overflow on integer truncation ***'
		  bad = .false.
		endif	      
		LINE(K) = NINT(denval)
		INDEX = INDEX + 1
	      enddo

	    else				! UNSIGNED
	      DO K = 1,N
		denval=array(index)
		if (denval .gt. 65535.4 .or. denval .lt. -0.4) then
		  denval = min(65535.,max(denval,0.))
		  if (bad) print *,'** overflow on integer truncation ***'
		  bad = .false.
		endif	      
		lcompos = NINT(denval)
		bline(2 * k + lowbyte - 2) = bstore(longb1)
		bline(2 * k + 1 - lowbyte) = bstore(longb2)
		INDEX = INDEX + 1
	      enddo
	    endif
	    if (mrcflip(j))call convert_shorts(line, n)
	    CALL QWRITE(J,LINE,N*2)
	    NWRITE = NWRITE - n
	  enddo

        else                           !bit mode
	  if (mrcflip(j))then
	    print *
	    print *,'ERROR: IWRLIN - TRYING TO WRITE BIT MODE DATA TO',
     &		' BYTE_SWAPPED FILE'
	    call exit(1)
	  endif
          noconj=nocon(j)
20        maxpix=(8*8190)/jmode        !max pixels in one write
          ngetpix=min(nwrite,maxpix)   !# of pixels to do this time
c   if "no conversion", have to get the integer*2 values out of "real" array
          if(noconj)call imrltoin(array,iscratch,index,ngetpix)
          mask=2**jmode-1              !mask for the n bits
          ibofset=mod(8-ibleft(j),8)   !# of bits left in current byte
          bcur=bytcur(j)               !current partial byte
          if(ibofset.eq.0)bcur=0       !need a zero if nothing there
          indbyt=1                     !pointer to next byte to store
          do ival=1,ngetpix
            lcompos=0
            bstore(longb1)=bcur             !put byte in end of longword
            if(noconj)then             !if no-conversion
              inttmp=iscratch(ival)    !put into integer*4 for iand etc
            else                       !otherwise use real value
              inttmp=nint(array(index))
              index=index+1
              if(inttmp.lt.0.or.inttmp.gt.mask)then
                inttmp=min(mask,max(inttmp,0))
                if(bad)print *,'** overflow packing into bit stream ***'
                bad = .false.
              endif
            endif
            lcompos=ior(lcompos,ishft(iand(mask,inttmp),ibofset))
            bline(indbyt)=bstore(longb1)    !always save the first byte
            if(jmode+ibofset.lt.16)then !if it doesn't fill the 2nd byte
              indbyt=indbyt+1
              bcur=bstore(longb2)           !make 2nd byte be bcur
            else
              bline(indbyt+1)=bstore(longb2) !otherwise save 2nd byte
              indbyt=indbyt+2
              bcur=bstore(longb3)           !and make 3rd byte be bcur
            endif
            ibofset=mod(ibofset+jmode,8) !new offset is old plus nbits, mod 8
          enddo            
          if(ibleft(j).gt.0)call qback(j,1) !back up if partial word at start
          if(ibofset.eq.0)then
            nbytes=indbyt-1            !just write all the complete bytes
          else
            nbytes=indbyt              !otherwise stick partial byte out too
            bline(indbyt)=bcur
          endif
          call qwrite(j,bline,nbytes)
          bytcur(j)=bcur
          ibleft(j)=mod(8-ibofset,8)
          nwrite=nwrite-ngetpix        !# left to write
          if(nwrite.gt.0)go to 20
	END IF
C
C  IF PARTIAL WRITE THEN MAKE CERTAIN POINTER IS CORRECTLY POSITIONED
C
	IF (ITYPE .EQ. 3) THEN
	  NSKIP = (NCRS(1,J) - NX2 + NX1 - 1)*JB
          CALL ALTSKIP(J,NSKIP,*99)
	END IF
        if(itype.eq.2)ibleft(j)=0      !move to byte boundary at end of section
	RETURN
99      print *,'error doing read from bitskip'
        return
	END
c       
c       
C   a kludge to get integer*2 values out of an array that iwrlin thinks
C   is a real array (but which its caller thinks is integer*2!!!)
          subroutine imrltoin(iarray,iscratch,index,ngetpix)
          integer*2 iscratch(*),iarray(*)
          do ival=1,ngetpix
            iscratch(ival)=iarray(index)
            index=index+1
          enddo
          return
          end

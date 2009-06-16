C       *IRDLIN
C       *IRDSEC
C       *IRDPAL
c       *irdsecl  reads nlines (added 25.aug.87  daa to speed up irdpas)
C       
C       Read in a line, a section or part of a line (NX1 - NX2)
C       or set of lines
C       into the REAL or COMPLEX REAL array ARRAY. If required,
C       Integer*1 or Integer*2 data on disk is converted to REALS.
c       9 to 15 bit data modes are also converted to positive reals.
C       After reading a partial line, the pointer is positioned to
C       the START of the next line.
C       Byte data is stored with numbers from 128 to 255 interpreted 
C       as -128 to -1, but returned to the calling program as their
C       original values 128 - 255.  
C       NOTE:   The start of a line is ALWAYS 0 (ie NX1,NX2 are relative)
C       
      SUBROUTINE IRDLIN(ISTREAM,ARRAY,*)
C       
      include 'imsubs.inc'
      logical noconj
      include 'endian.inc'
C       
      DIMENSION ARRAY(*)
      parameter (linesize = 40960)
      INTEGER*2 LINE(linesize/2),IB,iscratch(7300)
      INTEGER*1 BLINE(linesize),QB(2),bstore(4),bcur
      integer*4 lcompos
      real*4 rline(linesize/4)
      EQUIVALENCE (LINE,BLINE),(IB,QB),(lcompos,bstore(1)),
     &    (line,rline)
C       
      nlines=1
      ITYPE = 1
      GOTO 1
      ENTRY IRDSEC(ISTREAM,ARRAY,*)
      nlines = ncrs(2,lstream(istream))
      ITYPE = 2
      GOTO 1
      ENTRY IRDPAL(ISTREAM,ARRAY,NX1,NX2,*)
      ITYPE = 3
      GOTO 1
      ENTRY IRDSECL(ISTREAM,ARRAY,mlines,*)
      ITYPE = 2
      nlines = mlines
      GOTO 1
C       
1     J = LSTREAM(ISTREAM)
c       
c       for SPIDER file, read lines and pack into array in inverse order
c       
      if(spider(j))then
        if(itype.lt.3)then
          indnd=nlines*ncrs(1,j)
          do lrec=lrecspi(j)+1-nlines,lrecspi(j)
            indst=indnd+1-ncrs(1,j)
            read(30+j,rec=lbasspi(j)+lrec,err=99)
     &          (array(i),i=indst,indnd)
            indnd=indst-1
          enddo
          lrecspi(j)=lrecspi(j)-nlines
        else
c           
c           reading part of SPIDER line: if NX1 is not 0, read first part
c           of line into scratch array to throw it away
c           
          nread=nx2+1-nx1
          if(nx1.eq.0)then
            read(30+j,rec=lbasspi(j)+lrecspi(j),err=99)
     &          (array(i),i=1,nread)
          else
            read(30+j,rec=lbasspi(j)+lrecspi(j),err=99)
     &          (rline(i),i=1,nx1),(array(i),i=1,nread)
          endif
          lrecspi(j)=lrecspi(j)-1
        endif
        if(mod(lrecspi(j),ncrs(2,j)).eq.0)
     &      lrecspi(j)=lrecspi(j)+2*ncrs(2,j)
        return
      endif
c       
      JMODE = MODE(J)
c       JB is either the number of bytes per pixel for regular modes, or 1 for bit
c       modes: this makes calculations of NREAD work because the resulting value
c       is variously interpreted as number of bytes OR number of pixels to read
      jb=1
      if(jmode.le.6) JB = NB(JMODE + 1)
      QB(3-lowbyte) = 0
C       DNM: eliminate this because qlocate gives suspect results for giant files
C       and it's not needed anymore because irdhdr and iwrhdr take care of it
C       
c       call qlocate(j,location)
c       if(location.ge.0.and.location.le.nbhdr+nbsym(j)+1)ibleft(j)=0
C       
      IF (ITYPE .EQ. 1) THEN                    !READ LINE
        NREAD = NCRS(1,J)*JB
      ELSE IF (ITYPE .EQ. 2) THEN               !READ SECTION
        NREAD = NCRS(1,J)*nlines*JB
      ELSE IF (ITYPE .EQ. 3) THEN               !READ PART OF LINE
        NREAD = (NX2 - NX1  + 1)*JB
        NSKIP = NX1*JB                          !RELATIVE START!!!!
        CALL ALTSKIP(J,NSKIP,1,*99)
      END IF
C       
      IF (JMODE .EQ. 2 .OR. JMODE .EQ. 4 .OR.
     &    (NOCON(J) .AND. (JMODE. LT. 9))) THEN
C         DNM: straight no-conversion is silly for bit modes, will return integer*2
        CALL QREAD(J,ARRAY,NREAD,IER)
        IF (IER .NE. 0) GOTO 99
        if(mrcflip(j))then
          if(jmode.eq.2 .or. jmode.eq.4)then
            call convert_floats(array,nread/4)
          elseif(jmode.eq.1.or.jmode.eq.3)then
            call convert_shorts(array,nread/2)
          endif
        endif         
      ELSE IF (JMODE .EQ. 0) THEN               !INTEGER*1
        INDEX = 1
10      N = MIN(linesize,NREAD)
        CALL QREAD(J,BLINE,N,IER)
        IF (IER .NE. 0) GOTO 99
        QB(3-lowbyte) = 0
        DO K = 1,N
          QB(lowbyte) = BLINE(K)
          ARRAY(INDEX) = IB
          INDEX = INDEX + 1
        enddo
        NREAD = NREAD - n
        IF (NREAD .GT. 0) GOTO 10
      ELSEIF(jmode.le.8)then                    !ELSE INTEGER*2
        NREAD = NREAD/2
        INDEX = 1
15      N = MIN(linesize/2,NREAD)
        CALL QREAD(J,LINE,N*2,IER)
        IF (IER .NE. 0) GOTO 99
        if(mrcflip(j))call convert_shorts(line,n)
        if (jmode .ne. 6) then                  ! SIGNED
          DO  K = 1,N
            ARRAY(INDEX) = LINE(K)
            INDEX = INDEX + 1
          enddo
        else                                    ! UNSIGNED
          lcompos = 0
          DO K = 1,N
            bstore(longb1) = BLINE(2 * K + lowbyte - 2)
            bstore(longb2) = BLINE(2 * K + 1 - lowbyte)
            ARRAY(INDEX) = lcompos
            INDEX = INDEX + 1
          enddo
        endif
        NREAD = NREAD - n
        IF (NREAD .GT. 0) GOTO 15
      else                                      !bit modes
        index=1
        noconj=nocon(j)
20      maxpix=(8*8191+ibleft(j))/jmode         !max pixels in one read
        ngetpix=min(nread,maxpix)               !# of pixels to get this time
        nbytes=(jmode*ngetpix-ibleft(j)+7)/8    !# of bytes needed to do that
        mask=2**jmode-1                         !mask for the n bits
        call qread(j,bline,nbytes,ier)
        if(ier.ne.0)go to 99
        if(mrcflip(j))call convert_shorts(bline,nbytes/2)
        bcur=bytcur(j)                          !get left-over into bcur if any
        ibofset=8-ibleft(j)                     !specify offset 8-#bits left
        indbyt=1                       !pointer to next byte needed from array
        do ival=1,ngetpix
          lcompos=0                      !start with longword 0
          bstore(longb1)=bcur            !set current word into longword byte 1
          if(jmode+ibofset.lt.16)then    !if any leftover bits in next byte
            bcur=bline(indbyt)           !just move next byte into current
            bstore(longb2)=bcur          !and into longword byte 2
            indbyt=indbyt+1              !and point to next
          else                           !otherwise get next two bytes
            bstore(longb2)=bline(indbyt)
            bcur=bline(indbyt+1)         !this will be new current word
            bstore(longb3)=bcur
            indbyt=indbyt+2              !advance pointer by 2
          endif
                                         !shift right by offset, mask to n bits
          if(noconj)then                 !if no conversion, return integer*2
            iscratch(ival)=iand(mask,ishft(lcompos,-ibofset))
          else
            array(index)=iand(mask,ishft(lcompos,-ibofset))
            index=index+1
          endif              
          ibofset=mod(ibofset+jmode,8)     !new offset is old plus nbits, mod 8
        enddo
c         if "no conversion", have to pack the integer values into real array
        if(noconj)call imintorl(iscratch,array,index,ngetpix)
        bytcur(j)=bcur                          !save bcur
        ibleft(j)=mod(8-ibofset,8)              !and # of bits left in it
        nread=nread-ngetpix                     !# pixels left to get
        if(nread.gt.0)go to 20
      END IF
C       
      IF (ITYPE .EQ. 3) THEN                    !SKIP TO END OF RECORD
        NSKIP = (NCRS(1,J) - NX2 - 1)*JB        !IF PARTIAL READ
        CALL ALTSKIP(J,NSKIP,1,*99)
      END IF
      if(itype.eq.2)ibleft(j)=0        !move to byte boundary at end of section
      RETURN
99    RETURN 1
      END
C       
C       a kludge to return integer*2 values into an array that irdlin thinks
C       is a real array (but which its caller thinks is integer*2!!!)
      subroutine imintorl(iscratch,iarray,index,ngetpix)
      integer*2 iscratch(*),iarray(*)
      do ival=1,ngetpix
        iarray(index)=iscratch(ival)
        index=index+1
      enddo
      return
      end

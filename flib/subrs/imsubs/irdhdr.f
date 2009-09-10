C       *IRDHDR
C       
C       Read the header on unit ISTREAM, Print the contents,
C       and return some parameters to the caller:
C       
C       INXYZ             : size of file Columns, Rows, Sections
C       MXYZ              : # of intervals Columns, Rows, Sections
C       IMODE             : Data storage mode (1-4)
C       0 = Image               Integer*1
C       1 = Image               Integer*2
C       2 = Image               Reals
C       3 = Fourier Transform   Integer*2
C       4 = Fourier Transform   Reals
C       DMIN,DMAX,DMEAN   : Min, Max, & Mean densities
c       
c       stuff(1) = ispg and the real nbsym
c       stuff(2) = next # of extra header bytes (stored here as nbsym)
c       stuff(3) = first int*2 is creator id
c       stuff(11) = bytes per section and flags for type of extended data
c       or number of integers and number of reals per section
c       stuff(19) = idtype data type and lens
c       stuff(20) = data value 1
c       stuff(21) = data value 2
c       stuff(22)-stuff(27) = 2 tilt sets
c       OLD HEADER
c       stuff(28) - stuff(30)  wavelength info #wavele, values - 5 max
c       stuff(31) = zorig
C       
c       DNM 10/26/00: There was confusion about the meaning of nbsym.
c       Officially, ispg and nbsym are supposed to be two bytes, followed
c       by the 4-byte "next".  The code here reads ispg as 2 bytes, skips
c       the next two bytes, then
c       reads the next 4 bytes and calls it nbsym, but it is really next,
c       the number of bytes of extra header.  This is essential to allow
c       big enough extra headers, but it means values in the true locations
c       of ispg and nbsym will not be treated correctly.
c       
c       $Id$
c       Log at end
c
c       
      SUBROUTINE IRDHDR(ISTREAM,INXYZ,MXYZ,IMODE,DMIN,DMAX,DMEAN)
      use imsubs
      implicit none
      integer*4 INXYZ(3),MXYZ(3),LABELS(1),NXYZST(3),MCRS(3)
      real*4 TITLE(1),CELL(6),EXTRA(1),delta(3),delt(3)
      character*1 LXYZ(3)
      DATA LXYZ/'X','Y','Z'/
C       
      integer limtmp
      parameter (limtmp = 4096)
      integer*4 jbsym(*)
      real*4 header(256),tilt(3)
      real*4 amat1(3,3),amat2(3,3),amat3(3,3),spibuf(9)
      integer*4 headtmp(limtmp/4)
      logical cflag,onoff
      integer*2 idat(6)
      character*(*) string
      character*80 string80
      character*4 feichar
      logical nbytes_and_flags
      character*27 modeNames(8), modeLabel
      data modeNames/'(byte)','(16-bit integer)', '(32-bit real)',
     &    '(complex integer)', '(complex)', '(unknown)',
     &    '(unsigned 16-bit integer)','RGB color'/
      integer*4 nameMap(0:16) /1,2,3,4,5,6,7,6,6,0,0,0,0,0,0,0,8/
c       SAVE /IMGCOM/
c       
      integer*4 i,j,k,l,ier,lenrec,nbs,ispg,istream,imode,idtype,lens
      integer*4 nints,nreal,ifshorts,nblockunits,iblocksize,nblocks
      integer*4 nleft,iblock,nbread,indconv,nl,ml,ntflag,jstream
      integer*4 itype,lensnum,n1,n2,istart,nextra,mbsym
      integer*4 itype1,itype2,m,jextra
      real*4 dmin,dmax,dmean,vd1,vd2,v1,v2,xorig,yorig,zorig,rmsval

C       
C       Read header
C       
      J = LSTREAM(ISTREAM)
      FLAG(J) = .FALSE.
      CALL QSEEK(J,1,1,1,1,1)
      if(.not.spider(j))then
        CALL QREAD(J,NCRS(1,J),NBW3,IER)
        IF (IER .NE. 0) GOTO 99
        CALL QREAD(J,MODE(J),NBW,IER)
        CALL QREAD(J,NCRST(1,J),NBW3,IER)
        CALL QREAD(J,NXYZ(1,J),NBW3,IER)
        CALL QREAD(J,CEL(1,J),NBW*6,IER)
        CALL QREAD(J,MAPCRS(1,J),NBW3,IER)
        CALL QREAD(J,DENMMM(1,J),NBW3,IER)
        CALL QREAD(J,STUFF(1,J),NBW*27,IER)
        CALL QREAD(J,ORIGXYZ(1,J),3*NBW,IER)
        call qread(j,cmap(1,j),nbw,ier)
        call qread(j,stamp(1,j),nbw,ier)
        call qread(j,rms(j),nbw,ier)
        CALL QREAD(J,NLAB(J),NBW,IER)
        CALL QREAD(J,LABLS(1,1,J),NBL,IER)
C         
c         first detect old-style header and rearrange into memory properly
c         
        if (cmap(1,j).ne.ichar('M') .or. cmap(2,j).ne.ichar('A') .or.
     &      cmap(3,j).ne.ichar('P'))then
c           
c           obliterate old wavelength info then make up cmap and stamp
c           
          call move(origxyz(3,j),cmap(1,j),nbw)
          call move(origxyz(1,j),stamp(1,j),nbw)
          call move(origxyz(2,j),rms(j),nbw)
          rms(j) = 0.
          if (mrcflip(j)) call convert_floats(rms(j),4)
          call set_cmap_stamp(j)
          if (print .and. ifBrief .le. 0) write(6,1005)
1005      format(/,20x,'This file has an old-style MRC header.')
        endif

        if(mrcflip(j))then
          call swap_mrc_header(j)
        endif
        call move(idat,stuff(1,j),2)
        ispg = idat(1)
        nbs = istuff(2,j)
      else
c         
c         for SPIDER file, read first 9 words, get file dimensions and
c         dmin, dmax and dmean, set everything else to some default values,
c         ignore any label information also, assume reals ((mode 2)
c         
        call qread(j,spibuf,nbw*9,ier)
        IF (IER .NE. 0) GOTO 99
        lenrec=ncrs(1,j)
        lbasspi(j)=1
        if(spibuf(1).lt.0) lbasspi(j)=1+255/lenrec
        lrecspi(j)=spibuf(2)
        ncrs(3,j)=abs(spibuf(1))
        ncrs(2,j)=spibuf(2)
        denmmm(1,j)=spibuf(8)
        denmmm(2,j)=spibuf(7)
        denmmm(3,j)=spibuf(9)
        mode(j)=2
        nbs=0
        do k=1,27
          stuff(k,j)=0.
        enddo
        do k=1,3
          nxyz(k,j)=ncrs(k,j)
          cel(k,j) = ncrs(k,j)
          cel(k+3,j) = 90.0
          ncrst(k,j)=0
          mapcrs(k,j)=k
          origxyz(k,j)=0.
        enddo
        rms(j)=0.
        call move(stuff(19,j),idat,12)
        nlab(j)=0
      endif
c       
      CALL MOVE(INXYZ,NCRS(1,J),NBW3)
      CALL MOVE(MXYZ,NXYZ(1,J),NBW3)
      IMODE = MODE(J)
      DMIN = DENMMM(1,J)
      DMAX = DENMMM(2,J)
      DMEAN = DENMMM(3,J)
      if (ispg .ge. 0 .and. ispg .lt. 220 .and. nbs .gt. 0 ) then
        nbsym(j) = nbs
        istuff(2,j) = nbs
c         
c         New extra header stuff: don't read it in anymore
c         
c         if (nbs .le.1024) then
c         call qread(ibsym(1,j),nbs,ier)  !read in symmetry (or angle) info
c         c          take a guess on their being long integers
c         if(mrcflip(j))call convert_longs(ibsym(1,j),nbs/4)
      else
        ispg = 0
        nbsym(j) = 0
        istuff(2,j) = 0
      endif
      if (nxyz(1,j) .eq. 0 .or. cel(1,j) .lt. 1.e-5) then
        do k = 1,3
          mxyz(k) = 1
          nxyz(k,j) = 1
          cel(k,j) = 1.0
          cel(k+3,j) = 90.0
        enddo
      endif
c       
c       DNM 6/3/03: replace null in titles with space to avoid binary output
c       
      do k = 1,nlab(j)
        call fixtitlenulls(labls(1,k,j))
      enddo
c       
c       DNM 6/10/04: Workaround to FEI goof in which nints was set to 
c       # of bytes, 4 * nreal
c       
      call move(idat,stuff(11,j),4)
      write(feichar, '(a4)')labls(1,1,1)
      if (idat(1) .eq. 128 .and. idat(2) .eq. 32  .and. (feichar .eq. 'Fei '
     &    .or. nbsym(j) .eq. 131072)) then
        idat(1) = 0
        call move(stuff(11,j),idat,4)
        if (print .and. ifBrief .le. 0) write(6,1007)
1007    format(/,'  Assuming that the header entry for 128 ',
     &      'integers per section is an error')
      endif

c       
c       after reading the header, need to set to first section now
c       
      call imposn(istream,0,0)
C       
C       Write out header information
c       DNM 7/30/01: eliminate wavelength
C       
      do k = 1,3
        delt(k) = 1.0
        delt(k) = cel(mapcrs(k,j),j)/nxyz(mapcrs(k,j),j)
      enddo
      call move(idat,stuff(19,j),12)
      idtype = idat(1)
      lens = idat(2)
      if (imode .lt. 0 .or. imode .gt. 16) then
        modeLabel = '(unknown)'
      else if (imode .ge. 9 .and. imode .le. 15) then
        write(modeLabel, '(a,i2,a)')'(',imode,'-bit integer)'
      else
        modeLabel = modeNames(nameMap(imode))
      endif
      if (print .and. ifBrief .le. 0)WRITE(6,1000) INXYZ,IMODE,modeLabel,
     &      (NCRST(K,J), K=1,3), MXYZ, delt,(CEL(K,J),K=4,6),
     &      (LXYZ(MAPCRS(K,J)),K=1,3), (ORIGXYZ(K,J),K=1,3),DMIN,DMAX,DMEAN,
     &      (stuff(k,j),k=22,27),ispg,nbsym(j),
     &      idtype,lens,
     &      nlab(j),((LABLS(I,K,J),I=1,20),K=1,NLAB(J))
C       
C       for unix output without carriagecontrol:
C       DNM changed leading 2X to 1X on each line, changed tilt angle output
C       from 6f6.1 to f5.1,5f6.1, eliminated 1X before titles, eliminated 80th
C       char by changing 20A4 to 19A4,A3
C       
1000  FORMAT(/,
     &    1X,'Number of columns, rows, sections .....',3I8,/,
     &    1X,'Map mode ..............................',I5,3x,a,/,
     &    1X,'Start cols, rows, sects, grid x,y,z ...',I5,2I6,1X,3I7,/,
     &    1X,'Pixel spacing (Angstroms)..............',1x,3G11.4,/,
     &    1X,'Cell angles ...........................',3F9.3,/,
     &    1X,'Fast, medium, slow axes ...............',3(4X,A1),/,
     &    1X,'Origin on x,y,z .......................',1x,3G12.4,/,
     &    1X,'Minimum density .......................',G13.5,/,
     &    1X,'Maximum density .......................',G13.5,/,
     &    1X,'Mean density ..........................',G13.5,/,
     &    1X,'tilt angles (original,current) ........',f5.1,5f6.1,/,
     &    1X,'Space group,# extra bytes,idtype,lens .',4I9,//,
     &    1X,i5,' Titles :'/10(19A4,A3/))
c
      if (print .and. ifBrief .gt. 0) then
        write(6,1008)inxyz,delt,imode,dmin,dmax,dmean,
     &      (LABLS(I,1,J),I=1,20)
        if (nlab(j) .gt. 1) write(6,1009)(LABLS(I,nlab(j),J),I=1,20)
        write(6,*)
      endif
1008  format(' Dimensions:',3I7,'   Pixel size:', 3G11.4,/,' Mode:', i3,
     &    15x,'Min, max, mean:', 3g13.5, /, 19A4,A3)
1009  format(19A4,A3)
c       
c       DNM changed definitions and output:
c       itype = 0       normal mono data
c       itype = 1       tilt set    N1 = axis, v1=delta angle, v2=start angle
c       itype = 2       serial stereo pairs n1=axis  v1,v2= Left, right angles
c       itype = 3       avg mono n1=#secs wide, n2=#secs skip 
c       itype = 4       avg stereo n1=#secs wide, n2=#secs skip v1,v2=L,R angle
      if (print .and. idtype .gt. 0 .and. ifBrief .le. 0) then
        vd1 = .01*idat(5)
        vd2 = .01*idat(6)
        if (idtype .eq. 1) then
          write(6,1001) lxyz(idat(3)),vd1,vd2
        else if (idtype .eq. 2) then
          write(6,1002) lxyz(idat(3)),vd1,vd2
        else if (idtype .eq. 3) then
          write(6,1003) idat(3),idat(4)
        else if (idtype .eq. 4) then
          write(6,1004) idat(3),idat(4),vd1,vd2
        endif
1001    format(5x,' TILT data set, axis= ',a1,
     &      ' delta,start angle= ',2f8.2,/)
1002    format(' SERIAL STEREO data set, axis= ',a1,' left angle= ',
     &      f8.2,' right angle= ',f8.2,/)
1003    format(5x,' AVERAGED data set, Navg,Noffset   =  ',2i6,/)
1004    format(5x,' AVG STEREO data set, Navg,Noffset= ',
     &      2i3,' L,R angles= ',2f8.2,/)
      endif
C       
      RETURN
C       
C       
C       *IWRHDR
C       
C       Write out Header record to OUTPUT file. This is the ONLY
C       routine that writes the header.
C       
C       TITLE is a single 80 character title.
C       NTFLAG controls title writing.
C       -1 = No title is added to set
C       0 = This is only title
C       1 = Add this title at end
C       2 = This is first title, push others down.
C       
      ENTRY IWRHDRC(ISTREAM,string,NTFLAG,DMIN,DMAX,DMEAN)
      j = lstream(istream)
      string80 = string
      goto (20,5,6,7) ntflag+2
5     nlab(j) = 1
      read (string80,'(20a4)') (labls(1,k,j),k=1,20)
      goto 20
6     nlab(j) = min(10,nlab(j)+1)
      read (string80,'(20a4)') (labls(k,nlab(j),j),k=1,20)
      goto 20
7     k = min(9,nlab(j))
      do 100 l = k,1,-1
        call move(labls(1,l+1,j),labls(1,l,j),80)
100   continue
      read (string80,'(20a4)') (labls(1,k,j),k=1,20)
      nlab(j) = k + 1
      goto 20
C       
      ENTRY IWRHDR(ISTREAM,TITLE,NTFLAG,DMIN,DMAX,DMEAN)
c       
      J = LSTREAM(ISTREAM)
      GOTO (20,10,11,12) NTFLAG+2
10    NLAB(J) = 1
      CALL MOVE(LABLS(1,1,J),TITLE,80)
      GOTO 20
11    NLAB(J) = MIN(10,NLAB(J)+1)
      CALL MOVE(LABLS(1,NLAB(J),J),TITLE,80)
      GOTO 20
12    K = MIN(9,NLAB(J))
      DO 150 L = K,1,-1
        CALL MOVE(LABLS(1,L+1,J),LABLS(1,L,J),80)
150   CONTINUE
      CALL MOVE(LABLS(1,1,J),TITLE,80)
      NLAB(J) = K + 1
c       
20    FLAG(J) = .FALSE.
      DENMMM(1,J) = DMIN
      DENMMM(2,J) = DMAX
      DENMMM(3,J) = DMEAN
C       
      if(spider(j))then
        print *
        print *,'ERROR: IWRHDR - TRYING TO WRITE TO SPIDER FILE'
        call exit(1)
      endif
c       
c       DNM 6/26/02: if byte-swapped file, swap header before and after
c       DNM 6/30/02: write in new format only
c       
      if(mrcflip(j))call swap_mrc_header(j)
      CALL QSEEK(J,1,1,1,1,1)
      CALL QWRITE(J,NCRS(1,J),NBW3)
      CALL QWRITE(J,MODE(J),NBW)
      CALL QWRITE(J,NCRST(1,J),NBW3)
      CALL QWRITE(J,NXYZ(1,J),NBW3)
      CALL QWRITE(J,CEL(1,J),NBW*6)
      CALL QWRITE(J,MAPCRS(1,J),NBW3)
      CALL QWRITE(J,DENMMM(1,J),NBW3)
      CALL QWRITE(J,STUFF(1,J),NBW*27)
      CALL QWRITE(J,ORIGXYZ(1,J),NBW*3)
      call qwrite(j,cmap(1,j),nbw)
      call qwrite(j,stamp(1,j),nbw)
      call qwrite(j,rms(j),nbw)
      CALL QWRITE(J,NLAB(J),NBW)
      CALL QWRITE(J,LABLS(1,1,J),NBL)
      if(mrcflip(j))call swap_mrc_header(j)
c       if (nbsym(j) .gt. 0) call qwrite(j,ibsym(1,j),nbsym(j)) !write out sym
c       
c       now have to set to first section, to skip extra header
c       
      call imposn(istream,0,0)
c       
      RETURN
c       
c       *igethdr(istream,header)
c       DNM 7.30.02: modified these two with little point
c       
c       gets header from istream as 256 long word array
c       
      entry igethdr(istream,header)
c       
      j = lstream(istream)
      call move(header(1),ncrs(1,j),nbw3)
      call move(header(4),mode(j),nbw)
      call move(header(5),ncrst(1,j),nbw3)
      call move(header(8),nxyz(1,j),nbw3)
      call move(header(11),cel(1,j),nbw*6)
      call move(header(17),mapcrs(1,j),nbw3)
      call move(header(20),denmmm(1,j),nbw3)
      call move(header(23),stuff(1,j),nbw*27)
      call move(header(50),origxyz(1,j),nbw*3)
      call move(header(53),cmap(1,j),4)
      call move(header(54),stamp(1,j),4)
      call move(header(55),rms(j),nbw)
      call move(header(56),nlab(j),nbw)
      call move(header(57),labls(1,1,j),nbl)
      return
c       
c       *iputhdr(istream,header)
c       
c       loads header onto istream from a 256 long word array
c       
      entry iputhdr(istream,header)
c       
      j = lstream(istream)
      call move(ncrs(1,j),header(1),nbw3)
      call move(mode(j),header(4),nbw)
      call move(ncrst(1,j),header(5),nbw3)
      call move(nxyz(1,j),header(8),nbw3)
      call move(cel(1,j),header(11),nbw*6)
      call move(mapcrs(1,j),header(17),nbw3)
      call move(denmmm(1,j),header(20),nbw3)
      call move(stuff(1,j),header(23),nbw*27)
      call move(origxyz(1,j),header(50),nbw*3)
      call move(rms(j),header(55),nbw)
      call move(nlab(j),header(56),nbw)
      call move(labls(1,1,j),header(57),nbl)
      call set_cmap_stamp(j)
      return
C       
C       
C       *ITRHDR(ISTREAM,JSTREAM)
C       
C       Transfer header information from JSTREAM to ISTREAM
C       Note: Information is internally transfered, NOT actually written
C       to file!!!  (Except for extra bytes, which are written)
C       
      ENTRY ITRHDR(ISTREAM,JSTREAM)
C       
      J = LSTREAM(ISTREAM)
      K = LSTREAM(JSTREAM)
      CALL MOVE(NCRS(1,J),NCRS(1,K),NBW3)     
      MODE(J) = MODE(K)
      CALL MOVE(NCRST(1,J),NCRST(1,K),NBW3)
      CALL MOVE(NXYZ(1,J),NXYZ(1,K),NBW3)
      CALL MOVE(CEL(1,J),CEL(1,K),NBW*6)
      CALL MOVE(MAPCRS(1,J),MAPCRS(1,K),NBW3)
      CALL MOVE(DENMMM(1,J),DENMMM(1,K),NBW3)
      CALL MOVE(STUFF(1,J),STUFF(1,K),NBW*27)
      CALL MOVE(ORIGXYZ(1,J),ORIGXYZ(1,K),NBW*3)
      RMS(J) = RMS(K)
      call set_cmap_stamp(j)
      NLAB(J) = NLAB(K)
      CALL MOVE(LABLS(1,1,J),LABLS(1,1,K),NBL)
c       call move(ibsym(1,j),ibsym(1,k),nbsym(j))
C       
      go to 127
c       
c       
c       *ITREXTRA(ISTREAM,JSTREAM)       
c       
c       Transfer extra bytes from JSTREAM to ISTREAM by reading and writing
c       in 1024-byte chunks
c       
      entry itrextra(istream,jstream)
c       
127   J = LSTREAM(ISTREAM)
      K = LSTREAM(JSTREAM)
      if (nbsym(k).eq.0) return
      if (mrcflip(j)) then
        print *
        print *,'ERROR: ITRHDR/ITREXTRA - Cannot transfer extra ',
     &      'header to a byte-swapped file'
        call exit(1)
      endif
c       
c       DNM 4/18/02
c       figure out a block size for the transfer; if the data need to be
c       swapped, and they are not shorts, then get a block size that
c       contains an integral number of sections worth of data
c       
      iblocksize = limtmp
      if (mrcflip(k)) then
        call move(idat,stuff(11,j),4)
        nints = idat(1)
        nreal = idat(2)
        ifshorts = 1
        if(.not.(nbytes_and_flags(nints,nreal).or.
     &      (nints+nreal.eq.0))) then
          ifshorts=0
          nblockunits=limtmp/(4*(nints+nreal))
          iblocksize = nblockunits * 4 *(nints+nreal)
          if(iblocksize.eq.0) then
            print *
            print *,'ERROR: ITRHDR/ITREXTRA - array size too small ',
     &          'to transfer extra header data'
            call exit(1)
          endif
        endif
      endif
      nblocks = (nbsym(k) + iblocksize - 1)/iblocksize
      nleft = nbsym(k)
      call qseek(j,1+nbhdr,1,1,1,1)
      call qseek(k,1+nbhdr,1,1,1,1)
      do iblock = 1,nblocks
        nbread = min(iblocksize,nleft)
        call qread(k,headtmp,nbread,ier)
        if(mrcflip(k)) then
c           
c           if nints and nreal represent nbytes and flags, swap them all as
c           shorts Otherwise, swap them as nints longs and nreal floats
c           
          if(ifshorts.eq.1)then
            call convert_shorts(headtmp,nbread/2)
          else
            indconv=1
            do  i=1,nblockunits
              if(nints.gt.0)call convert_longs(headtmp(indconv),nints)
              indconv=indconv+nints
              if(nreal.gt.0)call convert_floats(headtmp(indconv),nreal)
              indconv=indconv+nreal
            enddo
          endif
        endif
        call qwrite(j,headtmp,nbread)
        nleft = nleft - iblocksize
      enddo
      nbsym(j) = nbsym(k)
      stuff(2,j) = stuff(2,k)
      stuff(11,j) = stuff(11,k)
c       
c       after these transfers, each pointer should be at first section
c       
      return


C       
C       
C       *ITRLAB(ISTREAM,JSTREAM)
C       
C       Transfer LABELS from JSTREAM to ISTREAM
C       Note: Information is internally transfered, NOT actually written
C       to file!!!
C       
      ENTRY ITRLAB(ISTREAM,JSTREAM)
C       
      J = LSTREAM(ISTREAM)
      K = LSTREAM(JSTREAM)
      NLAB(J) = NLAB(K)
      CALL MOVE(LABLS(1,1,J),LABLS(1,1,K),NBL)
C       
      RETURN
C       
C       
C       *ITRCEL(ISTREAM,JSTREAM)
C       
C       Transfer CELL parameters from JSTREAM to ISTREAM
C       Note: Information is internally transfered, NOT actually written
C       to file!!!
C       
      ENTRY ITRCEL(ISTREAM,JSTREAM)
C       
      J = LSTREAM(ISTREAM)
      K = LSTREAM(JSTREAM)
      CALL MOVE(CEL(1,J),CEL(1,K),6*NBW)
C       
      RETURN
C       
C       
C       *ICRHDR
C       
C       Create new header. All of the standard image defaults are
C       set up given the requested information. Header NOT written!!
C       NOTE: The starting point for Columns,Rows,Sections
C       are set to 0 by default.!!!!!!!!!
C       
C       INXYZ             : size of file Columns, Rows, Sections
C       MXYZ              : # of intervals Columns, Rows, Sections
C       IMODE             : Data storage mode (1-4)
C       0 = Image               Integer*1
C       1 = Image               Integer*2
C       2 = Image               Reals
C       3 = Fourier Transform   Integer*2
C       4 = Fourier Transform   Reals
C       LABELS(20,N)      :N=1,10 Up to 10 80 character labels
C       NL                :Actual # of labels to use (0 is O.K.)
C       
      ENTRY ICRHDR(ISTREAM,INXYZ,MXYZ,IMODE,LABELS,NL)
C       
      J = LSTREAM(ISTREAM)
      MODE(J) = IMODE
      ML = MIN(NL,10)
      ML = MAX(ML,0)
      NLAB(J) = ML
      DO 200 K = 1,3
        ORIGXYZ(K,J) = 0.0
        NCRS(K,J) = INXYZ(K)
        NXYZ(K,J) = MXYZ(K)
        CEL(K,J) = MXYZ(K)
        CEL(K+3,J) = 90.0
        NCRST(K,J) = 0
        MAPCRS(K,J) = K
        DENMMM(K,J) = 0.0
200   CONTINUE
      CALL ZERO(STUFF(1,J),NBW*27)
      rms(j) = 0.
      call set_cmap_stamp(j)
c       call zero(ibsym(1,j),nbw*256)
      nbsym(j) = 0
c       DNM: change fill that doesn't work to a zero
      CALL zero(LABLS(1,1,J),NBL)
      IF (ML .GT. 0) CALL MOVE(LABLS(1,1,J),LABELS,ML*80)
C       
      RETURN
C       
C       
C       
C       *IALCEL
C       
C       Alter CELL information for file. Header NOT actually written!!!
C       
C       CELL(6)           : new unit cell parameters (a,b,c, alpha,beta,gamma)
C       
      ENTRY IALCEL(ISTREAM,CELL)
C       
      J = LSTREAM(ISTREAM)
      CALL MOVE(CEL(1,J),CELL,NBW*6)
C       
      RETURN
C       
C       
C       *IALCON
C       
C       Alter data Conversion mode. USE WITH GREAT CARE!!!!!!!!!!
C       By default, all data is passed to the user or received from
C       the user as REALS or COMPLEX REALS, independent of storage mode
C       on the disk.
C       This routine allows the direct transmission of data to and from
C       the disk WITHOUT ANY FORMAT conversion.
C       
C       CFLAG   =        .TRUE.  for conversion (default value)
C       CFLAG   =        .FALSE. for NO conversion
C       
C       
      ENTRY IALCON(ISTREAM,CFLAG)
C       
      NOCON(LSTREAM(ISTREAM)) = .NOT.CFLAG
C       
      RETURN
C       
C       
c       *ialdat
c       
c       alter data type info
c       itype = 0       normal mono data
c       itype = 1       tilt set  N1 = axis, v1=delang
c       itype = 2       stereo tilt set  N1 = axis, v1=delang v2=stereo ang
c       itype = 3       avg mono N1 = number sects avg, N2 = offset per sect
c       itype = 4       avg stereo N1 = number sects avg, N2 = offset per sect
c       V1 = stereo ang
c       n1,n2 are encoded into 1 32bit word, V1,V2 are multby 100 and in 1 word
      entry ialdat(istream,itype,lensnum,N1,N2,V1,V2)
c       
      j = lstream(istream)
      idat(1) = itype
      idat(2) = lensnum
      idat(3) = n1
      idat(4) = n2
      idat(5) = nint(100.*v1)
      idat(6) = nint(100.*v2)
      call move(stuff(19,j),idat,12)
      return
c       
c       *ialdel
c       
c       Alter Delta for each pixel in nm  (columns,rows,sections)
c       
c       DELTA(3)                  : pixel spacing in nm (columns,rows,sections)
c       
      entry ialdel(istream,delta)
c       
      j = lstream(istream)
      do k = 1,3
        cel(mapcrs(k,j),j) = delta(k)
        nxyz(k,j) = 1.0
      enddo
c       
      return
c       
C       
C       *IALEXT
C       
C       Alter EXTRA information stored in "unused" poritions of
C       file header (16 words max!!). Header NOT actually written!!!
C       
C       EXTRA             : Buffer containing data to be placed in extra slot.
C       ISTART            : Which "extra" element to start at (1-16)
C       NEXTRA            : Number of words to transfer
C       
      ENTRY IALEXT(ISTREAM,EXTRA,ISTART,NEXTRA)
C       
      J = LSTREAM(ISTREAM)
      IF (ISTART .GT. 16 .OR. ISTART .LT. 1)  then
        print *
        print *, 'ERROR: IALEXT - IN START NUMBER'
        call exit(1)
      endif
      JEXTRA = MIN(NEXTRA + ISTART,17) - ISTART
      CALL MOVE(STUFF(ISTART+2,J),EXTRA,NBW*JEXTRA)
C       
      RETURN
C       
C       
C       *IALLAB
C       
C       Alters label information. All spaces beyond NL are set = blanks
C       
C       LABELS(20,10)     : space for labels
C       NL                : actual # of labels being used
C       
      ENTRY IALLAB(ISTREAM,LABELS,NL)
C       
      J = LSTREAM(ISTREAM)
      NLAB(J) = MIN(10,NL)
      CALL MOVE(LABLS(1,1,J),LABELS,NLAB(J)*80)
      IF (NL .LT. 10) THEN
        DO K = NL+1,10
c           DNM: change fill that doesn't work to a zero
          CALL zero(LABLS(1,K,J),80)
        END DO
      END IF
C       
      RETURN
C       
C       
c       
C       *IALMAP
C       
C       Alter MAPCRS information
C       
C       MCRS(3)           : columns,rows,sections mapping info
C       
      ENTRY IALMAP(ISTREAM,MCRS)
C       
      j = lstream(istream)
      call move(mapcrs(1,j),mcrs,12)
C       
      return
C       
C       
C       *IALMOD
C       
C       Alter MODE in hedaer. Header NOT actually written!!!
C       IMODE             : Data storage mode (1-4)
C       0 = Image               Integer*1
C       1 = Image               Integer*2
C       2 = Image               Reals
C       3 = Fourier Transform   Integer*2
C       4 = Fourier Transform   Reals
C       
      ENTRY IALMOD(ISTREAM,IMODE)
      MODE(LSTREAM(ISTREAM)) = IMODE
      RETURN
C       
C       *IALORG
C       
C       Alter ORIGIN information
C       
C       XORIG,YORIG       : X,Y origin information
C       
      ENTRY IALORG(ISTREAM,XORIG,YORIG,ZORIG)
C       
      J = LSTREAM(ISTREAM)
      ORIGXYZ(1,J) = XORIG
      ORIGXYZ(2,J) = YORIG
      ORIGXYZ(3,J) = ZORIG
C       
      RETURN
C       
c       turn printing on/off
c       
      entry ialprt(onoff)
      print = onoff
      return
C       
c       turn brief header on/off
c       
      entry ialbrief(istream)
      ifBrief = istream
      return
C       
C       *IALSIZ
C       
C       Alter SIZE information for file. Header NOT actually written!!!
C       
C       INXYZ             : size of file Columns, Rows, Sections
C       NXYZST            : starting # for Columns, Rows, Sections (usually 1) 
C       
      ENTRY IALSIZ(ISTREAM,INXYZ,NXYZST)
C       
      J = LSTREAM(ISTREAM)
      CALL MOVE(NCRS(1,J),INXYZ,NBW3)
      CALL MOVE(NCRST(1,J),NXYZST,NBW3)
C       
      RETURN
C       
C       *IALSAM
C       
C       Alter Sampling Size information for file. Header NOT actually written!!!
C       
C       MXYZ              : size of file original map size (MXYZ)
C       
      ENTRY IALSAM(ISTREAM,MXYZ)
C       
      J = LSTREAM(ISTREAM)
      CALL MOVE(NXYZ(1,J),MXYZ,NBW3)
C       
      RETURN
C       
C       
c       *ialnbsym
c       
c       alters number of bytes of extra header info
c       mbsym = # bytes 
c       
      entry ialnbsym(istream,mbsym)
      j = lstream(istream)
      nbsym(j) = mbsym
      istuff(2,j) = nbsym(j)
      return
C       
c       
c       *ialsym
c       
c       alters symmetry (extra header) info
c       mbsym = # bytes symmetery  (max=1024 no longer enforced)
c       jbsym = array with sym info
c       
      entry ialsym(istream,mbsym,jbsym)
      j = lstream(istream)
      if (mrcflip(j)) then
        print *
        print *,'ERROR: IALSYM - Cannot alter extra ',
     &      'header in a byte-swapped file'
        call exit(1)
      endif
c       nbsym(j) = min(1024,mbsym)
      nbsym(j) = mbsym
      istuff(2,j) = nbsym(j)
c       call move(ibsym(1,j),jbsym,mbsym)
      call qseek(j,1+nbhdr,1,1,1,1)
      call qwrite(j,jbsym,mbsym)
      return
C       
c       
c       *ialsymtyp
c       
c       alters type information of symmetry (extra header) info
c       itype1 = nint or # of bytes per section
c       itype2 = nreal or flags for what features are stored per section
c       
      entry ialsymtyp(istream,itype1,itype2)
      j = lstream(istream)
      idat(1) = itype1
      idat(2) = itype2
      call move(stuff(11,j),idat,4)
      return
c       
C       
C       *IALTLT
C       
C       Alters current set of tilt angles
C       
C       
      ENTRY IALTLT(ISTREAM,tilt)
C       
      J = LSTREAM(ISTREAM)
      call move(stuff(25,j),tilt,12)
C       
      RETURN
C       
C       
C       
C       *IALTLT_orig
C       
C       Alters original set of tilt angles
C       
C       
      ENTRY IALTLT_orig(ISTREAM,tilt)
C       
      J = LSTREAM(ISTREAM)
      call move(stuff(22,j),tilt,12)
C       
      RETURN
c       
c       *ialtlt_rot
c       
c       Alters current set of tilt angles by applying rotation
c       indicated by supplied angles
c       
c       
      entry ialtlt_rot(istream,tilt)
c       
      j = lstream(istream)
      call icalc_matrix(tilt,amat1)             !convert new angs to matrix
      call icalc_matrix(stuff(25,j),amat2)      !convert old angs to matrix
      do k = 1,3                                !multiply them
        do l = 1,3
          amat3(l,k) = 0.0
          do m = 1,3
            amat3(l,k) = amat3(l,k) + amat1(l,m)*amat2(m,k)
          enddo
        enddo
      enddo
      call icalc_angles(stuff(25,j),amat3)      !convert back to angles
c       
      return
c       
c       *ialwav
c       DNM 7/30/02: eliminated
c       
c       
C       
c       *ialrms
c       
c       Alter RMS value
c       
      entry ialrms(istream, rmsval)
      J = LSTREAM(ISTREAM)
      rms(j) = rmsval
      return
c       
C       *IRTCEL
C       
C       Return CELL information from file. 
C       
C       CELL(6)           : unit cell parameters (a,b,c, alpha,beta,gamma)
C       
      ENTRY IRTCEL(ISTREAM,CELL)
C       
      J = LSTREAM(ISTREAM)
      CALL MOVE(CELL,CEL(1,J),NBW*6)
C       
      RETURN
C       
c       *irtdat
c       
c       return data type info
c       itype = 0       normal mono data
c       itype = 1       tilt set        N1 = # angles stored per section
c       if n1=0 then n2=axis, v1=start ang, v2=delang
c       itype = 2       stereo pairs stored L,R V1= angle V2 = angle R
c       itype = 3       avg mono N1 = number sects avg, N2 = offset per sect
c       itype = 4       avg stereo N1 = number sects avg, N2 = offset per sect
c       V1,V2 = angls L,R
c       n1,n2 are encoded into 1 32bit word, V1,V2 are multby 100 and in 1 word
      entry irtdat(istream,itype,lensnum,N1,N2,V1,V2)
c       
      j = lstream(istream)
      call move(idat,stuff(19,j),12)
      itype = idat(1)
      lensnum = idat(2)
      n1 = idat(3)
      n2 = idat(4)
      v1 = .01*idat(5)
      v2 = .01*idat(6)
      return
c       
c       
c       *irtdel
c       
c       Return Delta for each pixel in nm  (columns,rows,sections)
c       
c       DELTA(3)                  : pixel spacing in nm (columns,rows,sections)
c       
      entry irtdel(istream,delta)
c       
      j = lstream(istream)
      do k = 1,3
        delta(k) = cel(mapcrs(k,j),j)/nxyz(mapcrs(k,j),j)
      enddo
c       
      return
c       
c       
C       *IRTEXT
C       
C       Returns EXTRA information stored in "unused" poritions of
C       file header (16 words max!!).
C       
C       EXTRA             : Buffer containing data to be placed in extra slot.
C       ISTART            : Which "extra" element to start at (1-16)
C       NEXTRA            : Number of words to transfer
C       
      ENTRY IRTEXT(ISTREAM,EXTRA,ISTART,NEXTRA)
C       
      J = LSTREAM(ISTREAM)
      IF (ISTART .GT. 16 .OR. ISTART .LT. 1) then
        print *
        print *, 'ERROR: IRTEXT - IN START NUMBER'
        call exit(1)
      endif
      JEXTRA = MIN(NEXTRA + ISTART,17) - ISTART
      CALL MOVE(EXTRA,STUFF(ISTART+2,J),NBW*JEXTRA)
C       
      RETURN
C       
C       
C       *IRTLAB
C       
C       Return label information
C       
C       LABELS(20,10)     : space for labels
C       NL                : actual # of labels being used
C       
      ENTRY IRTLAB(ISTREAM,LABELS,NL)
C       
      J = LSTREAM(ISTREAM)
      NL = NLAB(J)
      CALL MOVE(LABELS,LABLS(1,1,J),NL*80)
C       
      RETURN
C       
C       *IRTMAP
C       
C       Return MAPCRS information
C       
C       MCRS(3)           : columns,rows,sections mapping info
C       
      ENTRY IRTMAP(ISTREAM,MCRS)
C       
      j = lstream(istream)
      call move(mcrs,mapcrs(1,j),12)
C       
      return
C       
c       *irtmst
c       
c       Return map start information from file. 
c       
c       NXYZST(3)                 : map start info
c       
      entry irtmst(istream,nxyzst)
c       
      j = lstream(istream)
      call move(nxyzst,ncrst(1,j),12)
c       
      return
c       
C       
C       *IRTORG
C       
C       Returns ORIGIN information
C       
C       XORIG,YORIG,ZORIG         : X,Y,Z origin information
C       
      ENTRY IRTORG(ISTREAM,XORIG,YORIG,ZORIG)
C       
      J = LSTREAM(ISTREAM)
      XORIG = ORIGXYZ(1,J)
      YORIG = ORIGXYZ(2,J)
      ZORIG = ORIGXYZ(3,J)
C       
      RETURN
C       
c       
c       *irtsam
c       
c       Return Sampling Size information from file. 
c       
c       MXYZ(3)           : sampling size
c       
      entry irtsam(istream,mxyz)
c       
      j = lstream(istream)
      call move(mxyz,nxyz(1,j),12)
c       
      return
c       
C       
C       *IRTSIZ
C       
C       RETURN SIZE information for file. Header NOT actually written!!!
C       
C       INXYZ             : size of file Columns, Rows, Sections
C       MXYZ              : size of full map Columns, Rows, Sections
C       NXYZST            : starting # for Columns, Rows, Sections (usually 1) 
C       
      ENTRY IRTSIZ(ISTREAM,INXYZ,MXYZ,NXYZST)
C       
      J = LSTREAM(ISTREAM)
      CALL MOVE(INXYZ,NCRS(1,J),NBW3)
      CALL MOVE(MXYZ,NXYZ(1,J),NBW3)
      CALL MOVE(NXYZST,NCRST(1,J),NBW3)
C       
      RETURN
C       
C       
c       *irtnbsym
c       
c       returns number of bytes of extra header info
c       mbsym = # bytes 
c       
      entry irtnbsym(istream,mbsym)
      j = lstream(istream)
      mbsym = nbsym(j)
      return
c       
c       
c       *irtsym
c       
c       returns symmetry (extra header) info
c       mbsym = # bytes symmetery
c       jbsym = array with sym info
c       
      entry irtsym(istream,mbsym,jbsym)
      j = lstream(istream)
      mbsym = nbsym(j)
      if (mbsym.eq.0) return
      call qseek(j,1+nbhdr,1,1,1,1)
      call qread(j,jbsym,mbsym,ier)
      if(mrcflip(j))then
c         
c         DNM 2/3/02: if swapping, need to find out if nints and nreal
c         represent nbytes and flags, in which case swap them all as shorts
c         Otherwise, swap them as nints longs and nreal floats
c         
        call move(idat,stuff(11,j),4)
        nints = idat(1)
        nreal = idat(2)
        if(nbytes_and_flags(nints,nreal).or.(nints+nreal.eq.0))then
          call convert_shorts(jbsym,mbsym/2)
        else
          indconv=1
c           
c           DNM 4/18/02 add factor of 4 to avoid converting too many
c           
          do  i=1,mbsym/(4*(nints+nreal))
            if(nints.gt.0)call convert_longs(jbsym(indconv),nints)
            indconv=indconv+nints
            if(nreal.gt.0)call convert_floats(jbsym(indconv),nreal)
            indconv=indconv+nreal
          enddo
        endif
      endif
c       call move(jbsym,ibsym(1,j),mbsym)
      return
C       
c       
c       *irtsymtyp
c       
c       returns type information of symmetry (extra header) info
c       itype1 = nint or # of bytes per section
c       itype2 = nreal or flags for what features are stored per section
c       
      entry irtsymtyp(istream,itype1,itype2)
      j = lstream(istream)
      call move(idat,stuff(11,j),4)
      itype1 = idat(1)
      itype2 = idat(2)
      return
c       
C       
C       *IRTTLT
C       
C       Returns current set of tilt angles
c       
c       
      entry irttlt(istream,tilt)
c       
      j = lstream(istream)
      call move(tilt,stuff(25,j),12)
c       
      return
C       
C       *IRTTLT_orig
C       
C       Returns original set of tilt angles
c       
c       
      entry irttlt_orig(istream,tilt)
c       
      j = lstream(istream)
      call move(tilt,stuff(22,j),12)
c       
      return
c       
c       
c       *irtwav
c       DNM 7/30/02 eliminated
c       
C       
c       *irtrms
c       
c       Return RMS value
c       
      entry irtrms(istream, rmsval)
      J = LSTREAM(ISTREAM)
      rmsval = rms(j)
      return
c       
c       
99    WRITE(6,2000)
2000  FORMAT(/,' ERROR: IRDHDR - End-of-File !!!')
      call exit(1)
      END

      subroutine swap_mrc_header(j)
      use imsubs
      implicit none
      integer*4 j
      call convert_longs(ncrs(1,j),3)
      call convert_longs(mode(j),1)
      call convert_longs(ncrst(1,j),3)
      call convert_longs(nxyz(1,j),3)
      call convert_floats(cel(1,j),6)
      call convert_longs(mapcrs(1,j),3)
      call convert_floats(denmmm(1,j),3)
      call convert_shorts(stuff(1,j),2)
      call convert_longs(stuff(2,j),1)
      call convert_shorts(stuff(3,j),1)
      call convert_shorts(stuff(11,j),4)
      call convert_floats(stuff(13,j),6)
      call convert_shorts(stuff(19,j),6)
      call convert_floats(stuff(22,j),6)
c       old-style header
c       call convert_shorts(stuff(28,j),6)
c       call convert_floats(stuff(31,j),1)
c       call convert_floats(origxy(1,j),2)
      call convert_floats(origxyz(1,j),3)
      call convert_floats(rms(j),1)
      call convert_longs(nlab(j),1)
      return
      end


c       Set up cmap as 'MAP ' and stamp with first byte as 68 for little-
c       endian or 17 for big-endian file
      subroutine set_cmap_stamp(j)
      use imsubs
      implicit none
      include 'endian.inc'
      integer j
      logical b3dxor
c       
      cmap(1,j) = ichar('M')
      cmap(2,j) = ichar('A')
      cmap(3,j) = ichar('P')
      cmap(4,j) = ichar(' ')
      if (b3dxor(lowbyte.eq.1, mrcflip(j))) then
        stamp(1,j) = 68
      else
        stamp(1,j) = 17
      endif
      stamp(2,j) = 0
      stamp(3,j) = 0
      stamp(4,j) = 0
      return
      end


c       Replace any nulls or newlines in the title with spaces
c       
      subroutine fixtitlenulls(label)
      implicit none
      integer*1 label(80)
      integer*4 i
      do i = 1,80
        if (label(i) .eq. 0 .or. label(i) .eq. 10) label(i) = 32
      enddo
      return
      end

c       $Log$
c       Revision 3.18  2009/06/22 20:19:46  mast
c       Switch to module, add mode label output
c
c       Revision 3.17  2009/03/25 23:54:55  mast
c       Switched to writing integer into character variable for testing Fei, it
c       seems safer and corresponds to what is done when printing the labels
c
c       Revision 3.16  2007/12/25 16:00:09  mast
c       Added extra header size to test of FEI #int bug
c
c       Revision 3.15  2006/10/04 22:57:08  mast
c       Fixed qseek accidentally deleted in itrextra
c
c       Revision 3.14  2006/09/28 21:23:22  mast
c       Changes for brief output and new qseek call
c
c       
c       Revision 3.13  2005/12/09 04:39:44  mast
c       gfortran: .xor., continuation, byte, or open fixes
c       
c       Revision 3.12  2005/11/11 22:36:22  mast
c       Changes for unsigned mode
c       
c       Revision 3.11  2004/06/10 22:48:14  mast
c       Added workaround to FEI bug in nint-nreal definition
c       
c       Revision 3.10  2003/06/05 00:15:00  mast
c       Add (Angstroms) to pixel spacing output
c       
c       Revision 3.9  2002/08/17 05:38:39  mast
c       Added entries to return and alter the rms value
c       
c       Revision 3.8  2002/07/31 17:43:38  mast
c       *** empty log message ***
c       
c       Revision 3.7  2002/07/31 17:41:28  mast
c       Changed internal data definitions to correspond to MRC image2000
c       standard, eliminating wavelength entries and associated routines.
c       Made it able to read old or new headers, and put out new headers.
c       Did implicit none!
c       
c       Revision 3.6  2002/07/21 19:17:43  mast
c       Standardized error output to ERROR: ROUTINE
c       
c       Revision 3.5  2002/06/26 16:54:54  mast
c       Fixed bug in transferring extra header bytes from a swapped file
c       Need implicit none!
c       
c       Revision 3.4  2002/06/26 00:27:01  mast
c       Added ability to write a header back to a byte-swapped file, except
c       for extra header stuff.  Changed STOP's to call exit(1)
c       
c       Revision 3.3  2002/04/18 20:04:21  mast
c       Made itrhdr/itrextra swap bytes correctly for integer/real extra
c       header data; also stopped irtsym from swapping 4 times too much data
c       
c       Revision 3.2  2002/02/26 23:04:01  mast
c       *** empty log message ***
c       
c       Revision 3.1  2002/02/26 23:03:37  mast
c       When supplying extended header data, use the values of nint and nreal
c       to test whether it consists of shorts or ints and reals, and swap
c       appropriately
c       

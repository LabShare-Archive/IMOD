C*IRDHDR
C
C	Read the header on unit ISTREAM, Print the contents,
C	and return some parameters to the caller:
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	MXYZ		  : # of intervals Columns, Rows, Sections
C	IMODE		  : Data storage mode (1-4)
C				0 = Image		Integer*1
C				1 = Image               Integer*2
C				2 = Image               Reals
C				3 = Fourier Transform   Integer*2
C				4 = Fourier Transform   Reals
C	DMIN,DMAX,DMEAN   : Min, Max, & Mean densities
c	  
c	stuff(1) = ispg and the real nbsym
c	stuff(2) = next # of extra header bytes (stored here as nbsym)
c       stuff(3) = first int*2 is creator id
c	stuff(11) = bytes per section and flags for type of extended data
c	stuff(19) = idtype data type and lens
c	stuff(20) = data value 1
c	stuff(21) = data value 2
c	stuff(22)-stuff(27) = 2 tilt sets
c	stuff(28) - stuff(30)  wavelength info #wavele, values - 5 max
c	stuff(31) = zorig
C	  
c	  DNM 10/26/00: There was confusion about the meaning of nbsym.
c	  Officially, ispg and nbsym are supposed to be two bytes, followed
c	  by the 4-byte "next".  The code here reads ispg as 2 bytes, skips
c	  the next two bytes, then
c	  reads the next 4 bytes and calls it nbsym, but it is really next,
c	  the number of bytes of extra header.  This is essential to allow
c	  big enough extra headers, but it means values in the true locations
c	  of ispg and nbsym will not be treated correctly.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	SUBROUTINE IRDHDR(ISTREAM,INXYZ,MXYZ,IMODE,DMIN,DMAX,DMEAN)
	DIMENSION INXYZ(3),MXYZ(3),LXYZ(3),LABELS(1),NXYZST(3)
	DIMENSION TITLE(1),CELL(6),EXTRA(1),MCRS(3),delta(3),delt(3)
	DATA LXYZ/'X','Y','Z'/
C
	include 'imsubs.inc'
	dimension istuff(31,10),header(256),tilt(3),jbsym(*),wavelen(6)
	dimension amat1(3,3),amat2(3,3),amat3(3,3),spibuf(9)
	integer*4 headtmp(256)
	logical cflag,onoff
	integer*2 idat(6),iwavelen(6)
	character*(*) string
	character*80 string80
	character*28 string28
	equivalence (istuff,stuff)
	logical nbytes_and_flags
c	SAVE /IMGCOM/
C
C Read header
C
	J = LSTREAM(ISTREAM)
	FLAG(J) = .FALSE.
	CALL QSEEK(J,1,1,1)
	if(.not.spider(j))then
	  CALL QREAD(J,NCRS(1,J),NBW3,IER)
	  IF (IER .NE. 0) GOTO 99
	  CALL QREAD(J,MODE(J),NBW,IER)
	  CALL QREAD(J,NCRST(1,J),NBW3,IER)
	  CALL QREAD(J,NXYZ(1,J),NBW3,IER)
	  CALL QREAD(J,CEL(1,J),NBW*6,IER)
	  CALL QREAD(J,MAPCRS(1,J),NBW3,IER)
	  CALL QREAD(J,DENMMM(1,J),NBW3,IER)
	  CALL QREAD(J,STUFF(1,J),NBW*31,IER)
	  CALL QREAD(J,ORIGXY(1,J),2*NBW,IER)
	  CALL QREAD(J,NLAB(J),NBW,IER)
	  CALL QREAD(J,LABLS(1,1,J),NBL,IER)
C
	  if(mrcflip(j))then
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
	    call convert_shorts(stuff(28,j),6)
	    call convert_floats(stuff(31,j),1)
	    call convert_floats(origxy(1,j),2)
	    call convert_longs(nlab(j),1)
	  endif
	  call move(idat,stuff(1,j),2)
	  ispg = idat(1)
	  nbs = istuff(2,j)
	else
c	    
c	    for SPIDER file, read first 9 words, get file dimensions and
c	    dmin, dmax and dmean, set everything else to some default values,
c	    ignore any label information also, assume reals ((mode 2)
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
	  do k=1,31
	    stuff(k,j)=0.
	  enddo
	  do k=1,3
	    nxyz(k,j)=ncrs(k,j)
	    cel(k,j) = ncrs(k,j)
	    cel(k+3,j) = 90.0
	    ncrst(k,j)=0
	    mapcrs(k,j)=k
	    if(k.lt.3)origxy(k,j)=0.
	    idat(k)=0
	    idat(k+3)=0
	  enddo
	  call move(stuff(19,j),idat,12)
	  call move(stuff(28,j),idat,12)
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
c	    New extra header stuff: don't read it in anymore
c
c	  if (nbs .le.1024) then
c	    call qread(ibsym(1,j),nbs,ier)  !read in symmetry (or angle) info
cc	    take a guess on their being long integers
c	  if(mrcflip(j))call convert_longs(ibsym(1,j),nbs/4)
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
c	  after reading the header, need to set to first section now
c
	call imposn(istream,0,0)
C
C Write out header information
C
	do k = 1,3
	  delt(k) = 1.0
	  delt(k) = cel(mapcrs(k,j),j)/nxyz(mapcrs(k,j),j)
	enddo
	call move(idat,stuff(19,j),12)
	call move(iwavelen,stuff(28,j),12)
	idtype = idat(1)
	lens = idat(2)
	mwave = min(7,max(1,iwavelen(1)))
	write(string28,'(7i4)')(iwavelen(k+1),k=1,mwave)
	if (print)WRITE(6,1000) INXYZ,IMODE,(NCRST(K,J),K=1,3),MXYZ,
     .	delt,(CEL(K,J),K=4,6),(LXYZ(MAPCRS(K,J)),K=1,3),
     .	(ORIGXY(K,J),K=1,2),stuff(31,j),DMIN,DMAX,DMEAN,
     .	(stuff(k,j),k=22,27),ispg,nbsym(j),
     .	idtype,lens,iwavelen(1),string28,
     .	nlab(j),((LABLS(I,K,J),I=1,20),K=1,NLAB(J))
C       
C         for unix output without carriagecontrol:
C         DNM changed leading 2X to 1X on each line, changed tilt angle output
C         from 6f6.1 to f5.1,5f6.1, eliminated 1X before titles, eliminated 80th
C         char by changing 20A4 to 19A4,A3
C
1000	FORMAT(/
     .  1X,'Number of columns, rows, sections .........',3I6/
     .  1X,'Map mode ..................................',I5/
     .  1X,'Start cols, rows, sects, grid x,y,z.... ...',3I5,2X,3i5/
     .  1X,'Pixel spacing .............................',3G11.4/
     .  1X,'Cell angles ...............................',3F9.3/
     .  1X,'Fast, medium, slow axes ...................',3(4X,A1)/
     .	1X,'Origin on x,y,z ...........................',3G11.4,/
     .  1X,'Minimum density ...........................',G13.5/
     .  1X,'Maximum density ...........................',G13.5/
     .  1X,'Mean density ..............................',G13.5/
     .	1X,'tilt angles (original,current) ............',f5.1,5f6.1,/,
     .	1X,'Space group # bytes symm,idtype,lens ......',4I6/
     .  1X,'# wavelengths, wavelengths (nm)............',i4,a28,//,
     .  1X,i5,' Titles :'/10(19A4,A3/))
c
c	  DNM changed definitions and output:
c	itype = 0	normal mono data
c	itype = 1	tilt set    N1 = axis, v1=delta angle, v2=start angle
c	itype = 2	serial stereo pairs n1=axis  v1,v2= Left, right angles
c	itype = 3	avg mono n1=#secs wide, n2=#secs skip 
c	itype = 4	avg stereo n1=#secs wide, n2=#secs skip v1,v2=L,R angle
	if (print .and. idtype .gt. 0) then
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
1001	  format(5x,' TILT data set, axis= ',a1,
     &	      ' delta,start angle= ',2f8.2,/)
1002	  format(' SERIAL STEREO data set, axis= ',a1,' left angle= ',
     .	  f8.2,' right angle= ',f8.2,/)
1003	  format(5x,' AVERAGED data set, Navg,Noffset   =  ',2i6,/)
1004	  format(5x,' AVG STEREO data set, Navg,Noffset= ',
     .	  2i3,' L,R angles= ',2f8.2,/)
	endif
C
	RETURN
C
C
C*IWRHDR
C
C	Write out Header record to OUTPUT file. This is the ONLY
C	routine that writes the header.
C
C	TITLE is a single 80 character title.
C	NTFLAG controls title writing.
C	-1 = No title is added to set
C	 0 = This is only title
C	 1 = Add this title at end
C	 2 = This is first title, push others down.
C
	ENTRY IWRHDRC(ISTREAM,string,NTFLAG,DMIN,DMAX,DMEAN)
	j = lstream(istream)
	string80 = string
	goto (20,5,6,7) ntflag+2
5	nlab(j) = 1
	read (string80,'(20a4)') (labls(1,k,j),k=1,20)
	goto 20
6	nlab(j) = min(10,nlab(j)+1)
	read (string80,'(20a4)') (labls(k,nlab(j),j),k=1,20)
	goto 20
7	k = min(9,nlab(j))
	do 100 l = k,1,-1
	  call move(labls(1,l+1,j),labls(1,l,j),80)
100	continue
	read (string80,'(20a4)') (labls(1,k,j),k=1,20)
	nlab(j) = k + 1
	goto 20
C
	ENTRY IWRHDR(ISTREAM,TITLE,NTFLAG,DMIN,DMAX,DMEAN)
c
	J = LSTREAM(ISTREAM)
	GOTO (20,10,11,12) NTFLAG+2
10	NLAB(J) = 1
	CALL MOVE(LABLS(1,1,J),TITLE,80)
	GOTO 20
11	NLAB(J) = MIN(10,NLAB(J)+1)
	CALL MOVE(LABLS(1,NLAB(J),J),TITLE,80)
	GOTO 20
12	K = MIN(9,NLAB(J))
	DO 150 L = K,1,-1
	  CALL MOVE(LABLS(1,L+1,J),LABLS(1,L,J),80)
150	CONTINUE
	CALL MOVE(LABLS(1,1,J),TITLE,80)
	NLAB(J) = K + 1
c
20	FLAG(J) = .FALSE.
	DENMMM(1,J) = DMIN
	DENMMM(2,J) = DMAX
	DENMMM(3,J) = DMEAN
C	  
	if(spider(j))STOP 'TRYING TO WRITE TO SPIDER FILE'
	if(mrcflip(j))STOP 'TRYING TO WRITE TO UNCONVERTED FILE'
	CALL QSEEK(J,1,1,1)
	CALL QWRITE(J,NCRS(1,J),NBW3)
	CALL QWRITE(J,MODE(J),NBW)
	CALL QWRITE(J,NCRST(1,J),NBW3)
	CALL QWRITE(J,NXYZ(1,J),NBW3)
	CALL QWRITE(J,CEL(1,J),NBW*6)
	CALL QWRITE(J,MAPCRS(1,J),NBW3)
	CALL QWRITE(J,DENMMM(1,J),NBW3)
	CALL QWRITE(J,STUFF(1,J),NBW*31)
	CALL QWRITE(J,ORIGXY(1,J),NBW*2)
	CALL QWRITE(J,NLAB(J),NBW)
	CALL QWRITE(J,LABLS(1,1,J),NBL)
c	if (nbsym(j) .gt. 0) call qwrite(j,ibsym(1,j),nbsym(j))	!write out sym
c	  
c	  now have to set to first section, to skip extra header
c
	call imposn(istream,0,0)
c
	RETURN
c
c*igethdr(istream,header)
c
c	gets header from istream as 256 long word array
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
	call move(header(23),stuff(1,j),nbw*31)
	call move(header(54),origxy(1,j),nbw*2)
	call move(header(56),nlab(j),nbw)
	call move(header(57),labls(1,1,j),nbl)
	return
c
c*iputhdr(istream,header)
c
c	loads header onto istream from a 256 long word array
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
	call move(stuff(1,j),header(23),nbw*31)
	call move(origxy(1,j),header(54),nbw*2)
	call move(nlab(j),header(56),nbw)
	call move(labls(1,1,j),header(57),nbl)
	return
C
C
C*ITRHDR(ISTREAM,JSTREAM)
C
C	Transfer header information from JSTREAM to ISTREAM
C	Note: Information is internally transfered, NOT actually written
C	to file!!!  (Except for extra bytes, which are written)
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
	CALL MOVE(STUFF(1,J),STUFF(1,K),NBW*31)
	CALL MOVE(ORIGXY(1,J),ORIGXY(1,K),NBW*2)
	NLAB(J) = NLAB(K)
	CALL MOVE(LABLS(1,1,J),LABLS(1,1,K),NBL)
c	call move(ibsym(1,j),ibsym(1,k),nbsym(j))
C	  
	go to 127
c
c	  
c*ITREXTRA(ISTREAM,JSTREAM)	  
c	  
c	  Transfer extra bytes from JSTREAM to ISTREAM by reading and writing
c	  in 1024-byte chunks
c
	entry itrextra(istream,jstream)
c	  
127	J = LSTREAM(ISTREAM)
	K = LSTREAM(JSTREAM)
	if (nbsym(k).eq.0) return
	nblocks = (nbsym(k) + 1023)/1024
	nleft = nbsym(k)
	call qseek(j,1,1+nbhdr,1)
	call qseek(k,1,1+nbhdr,1)
	do 130 iblock = 1,nblocks
	  nbread = min(1024,nleft)
	  call qread(k,headtmp,nbread,ier)
c
c	    assume they are shorts
c
	  if(mrcflip(k)) call convert_shorts(headtmp,nbread/2)
	  call qwrite(j,headtmp,nbread)
	  nleft = nleft - 1024
130	continue
	nbsym(j) = nbsym(k)
	stuff(2,j) = stuff(2,k)
	stuff(11,j) = stuff(11,k)
c	  
c	  after these transfers, each pointer should be at first section
c
	return


C
C
C*ITRLAB(ISTREAM,JSTREAM)
C
C	Transfer LABELS from JSTREAM to ISTREAM
C	Note: Information is internally transfered, NOT actually written
C	to file!!!
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
C*ITRCEL(ISTREAM,JSTREAM)
C
C	Transfer CELL parameters from JSTREAM to ISTREAM
C	Note: Information is internally transfered, NOT actually written
C	to file!!!
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
C*ICRHDR
C
C	Create new header. All of the standard image defaults are
C	set up given the requested information. Header NOT written!!
C NOTE: The starting point for Columns,Rows,Sections
C	are set to 0 by default.!!!!!!!!!
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	MXYZ		  : # of intervals Columns, Rows, Sections
C	IMODE		  : Data storage mode (1-4)
C				0 = Image		Integer*1
C				1 = Image               Integer*2
C				2 = Image               Reals
C				3 = Fourier Transform   Integer*2
C				4 = Fourier Transform   Reals
C	LABELS(20,N)	  :N=1,10 Up to 10 80 character labels
C	NL		  :Actual # of labels to use (0 is O.K.)
C
	ENTRY ICRHDR(ISTREAM,INXYZ,MXYZ,IMODE,LABELS,NL)
C
	J = LSTREAM(ISTREAM)
	MODE(J) = IMODE
	ML = MIN(NL,10)
	ML = MAX(ML,0)
	NLAB(J) = ML
	ORIGXY(1,J) = 0.0
	ORIGXY(2,J) = 0.0
	DO 200 K = 1,3
	  NCRS(K,J) = INXYZ(K)
	  NXYZ(K,J) = MXYZ(K)
	  CEL(K,J) = MXYZ(K)
	  CEL(K+3,J) = 90.0
	  NCRST(K,J) = 0
	  MAPCRS(K,J) = K
	  DENMMM(K,J) = 0.0
200	CONTINUE
	CALL ZERO(STUFF(1,J),NBW*31)
c	call zero(ibsym(1,j),nbw*256)
	nbsym(j) = 0
c	  DNM: change fill that doesn't work to a zero
	CALL zero(LABLS(1,1,J),NBL)
	IF (ML .GT. 0) CALL MOVE(LABLS(1,1,J),LABELS,ML*80)
C
	RETURN
C
C
C
C*IALCEL
C
C	Alter CELL information for file. Header NOT actually written!!!
C
C	CELL(6)		  : new unit cell parameters (a,b,c, alpha,beta,gamma)
C
	ENTRY IALCEL(ISTREAM,CELL)
C
	J = LSTREAM(ISTREAM)
	CALL MOVE(CEL(1,J),CELL,NBW*6)
C
	RETURN
C
C
C*IALCON
C
C	Alter data Conversion mode. USE WITH GREAT CARE!!!!!!!!!!
C	By default, all data is passed to the user or received from
C	the user as REALS or COMPLEX REALS, independent of storage mode
C	on the disk.
C	This routine allows the direct transmission of data to and from
C	the disk WITHOUT ANY FORMAT conversion.
C
C	CFLAG	=	 .TRUE.  for conversion (default value)
C	CFLAG	=	 .FALSE. for NO conversion
C
C
	ENTRY IALCON(ISTREAM,CFLAG)
C
	NOCON(LSTREAM(ISTREAM)) = .NOT.CFLAG
C
	RETURN
C
C
c*ialdat
c
c	alter data type info
c	itype = 0	normal mono data
c	itype = 1	tilt set  N1 = axis, v1=delang
c	itype = 2	stereo tilt set  N1 = axis, v1=delang v2=stereo ang
c	itype = 3	avg mono N1 = number sects avg, N2 = offset per sect
c	itype = 4	avg stereo N1 = number sects avg, N2 = offset per sect
c			V1 = stereo ang
c	n1,n2 are encoded into 1 32bit word, V1,V2 are multby 100 and in 1 word
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
c*ialdel
c
c	Alter Delta for each pixel in nm  (columns,rows,sections)
c
c	DELTA(3)		  : pixel spacing in nm (columns,rows,sections)
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
C*IALEXT
C
C	Alter EXTRA information stored in "unused" poritions of
C	file header (16 words max!!). Header NOT actually written!!!
C
C	EXTRA		  : Buffer containing data to be placed in extra slot.
C	ISTART		  : Which "extra" element to start at (1-16)
C	NEXTRA		  : Number of words to transfer
C
	ENTRY IALEXT(ISTREAM,EXTRA,ISTART,NEXTRA)
C
	J = LSTREAM(ISTREAM)
	IF (ISTART .GT. 16 .OR. ISTART .LT. 1) 
     .	 STOP 'IALEXT: ERROR IN START NUMBER'
	JEXTRA = MIN(NEXTRA + ISTART,17) - ISTART
	CALL MOVE(STUFF(ISTART+2,J),EXTRA,NBW*JEXTRA)
C
	RETURN
C
C
C*IALLAB
C
C	Alters label information. All spaces beyond NL are set = blanks
C
C	LABELS(20,10)	  : space for labels
C	NL		  : actual # of labels being used
C
	ENTRY IALLAB(ISTREAM,LABELS,NL)
C
	J = LSTREAM(ISTREAM)
	NLAB(J) = MIN(10,NL)
	CALL MOVE(LABLS(1,1,J),LABELS,NLAB(J)*80)
	IF (NL .LT. 10) THEN
	  DO K = NL+1,10
c	      DNM: change fill that doesn't work to a zero
	    CALL zero(LABLS(1,K,J),80)
	  END DO
	END IF
C
	RETURN
C
C
c
C*IALMAP
C
C	Alter MAPCRS information
C
C	MCRS(3)		  : columns,rows,sections mapping info
C
	ENTRY IALMAP(ISTREAM,MCRS)
C
	j = lstream(istream)
	call move(mapcrs(1,j),mcrs,12)
C
	return
C
C
C*IALMOD
C
C	Alter MODE in hedaer. Header NOT actually written!!!
C	IMODE		  : Data storage mode (1-4)
C				0 = Image		Integer*1
C				1 = Image               Integer*2
C				2 = Image               Reals
C				3 = Fourier Transform   Integer*2
C				4 = Fourier Transform   Reals
C
	ENTRY IALMOD(ISTREAM,IMODE)
	MODE(LSTREAM(ISTREAM)) = IMODE
	RETURN
C
C*IALORG
C
C	Alter ORIGIN information
C
C	XORIG,YORIG	  : X,Y origin information
C
	ENTRY IALORG(ISTREAM,XORIG,YORIG,ZORIG)
C
	J = LSTREAM(ISTREAM)
	ORIGXY(1,J) = XORIG
	ORIGXY(2,J) = YORIG
	stuff(31,j) = zorig
C
	RETURN
C
c  turn printing on/off
c
	entry ialprt(onoff)
	print = onoff
	return
C
C*IALSIZ
C
C	Alter SIZE information for file. Header NOT actually written!!!
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	NXYZST		  : starting # for Columns, Rows, Sections (usually 1) 
C
	ENTRY IALSIZ(ISTREAM,INXYZ,NXYZST)
C
	J = LSTREAM(ISTREAM)
	CALL MOVE(NCRS(1,J),INXYZ,NBW3)
	CALL MOVE(NCRST(1,J),NXYZST,NBW3)
C
	RETURN
C
C*IALSAM
C
C	Alter Sampling Size information for file. Header NOT actually written!!!
C
C	MXYZ		  : size of file original map size (MXYZ)
C
	ENTRY IALSAM(ISTREAM,MXYZ)
C
	J = LSTREAM(ISTREAM)
	CALL MOVE(NXYZ(1,J),MXYZ,NBW3)
C
	RETURN
C
C
c*ialnbsym
c
c	alters number of bytes of extra header info
c	mbsym = # bytes 
c
	entry ialnbsym(istream,mbsym)
	j = lstream(istream)
	nbsym(j) = mbsym
	istuff(2,j) = nbsym(j)
	return
C
c
c*ialsym
c
c	alters symmetry (extra header) info
c	mbsym = # bytes symmetery  (max=1024 no longer enforced)
c	jbsym = array with sym info
c
	entry ialsym(istream,mbsym,jbsym)
	j = lstream(istream)
c	nbsym(j) = min(1024,mbsym)
	nbsym(j) = mbsym
	istuff(2,j) = nbsym(j)
c	call move(ibsym(1,j),jbsym,mbsym)
	call qseek(j,1,1+nbhdr,1)
	call qwrite(j,jbsym,mbsym)
	return
C	  
c	  
c*ialsymtyp
c	  
c	  alters type information of symmetry (extra header) info
c	  itype1 = nint or # of bytes per section
c	  itype2 = nreal or flags for what features are stored per section
c	  
	entry ialsymtyp(istream,itype1,itype2)
	j = lstream(istream)
	idat(1) = itype1
	idat(2) = itype2
	call move(stuff(11,j),idat,4)
	return
c
C
C*IALTLT
C
C	Alters current set of tilt angles
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
C*IALTLT_orig
C
C	Alters original set of tilt angles
C
C
	ENTRY IALTLT_orig(ISTREAM,tilt)
C
	J = LSTREAM(ISTREAM)
	call move(stuff(22,j),tilt,12)
C
	RETURN
c
c*ialtlt_rot
c
c	Alters current set of tilt angles by applying rotation
c	indicated by supplied angles
c
c
	entry ialtlt_rot(istream,tilt)
c
	j = lstream(istream)
	call icalc_matrix(tilt,amat1)		!convert new angs to matrix
	call icalc_matrix(stuff(25,j),amat2)	!convert old angs to matrix
	do k = 1,3				!multiply them
	  do l = 1,3
	    amat3(l,k) = 0.0
	    do m = 1,3
	      amat3(l,k) = amat3(l,k) + amat1(l,m)*amat2(m,k)
	    enddo
	  enddo
	enddo
	call icalc_angles(stuff(25,j),amat3)	!convert back to angles
c
	return
c
c*ialwav
c
c	return wavelength info
c	nwave = # of wavelengths (max 5)
c	wave = vector of nwave wavelengths (in nm)
	entry ialwav(istream,nwave,wavelen)
c
	j = lstream(istream)
	mwave = min(5,nwave)
	do k =1,mwave
	  iwavelen(k+1) = nint(wavelen(k))
	enddo
	iwavelen(1) = mwave
	call move(stuff(28,j),iwavelen,12)
	return
C
c
C
C*IRTCEL
C
C	Return CELL information from file. 
C
C	CELL(6)		  : unit cell parameters (a,b,c, alpha,beta,gamma)
C
	ENTRY IRTCEL(ISTREAM,CELL)
C
	J = LSTREAM(ISTREAM)
	CALL MOVE(CELL,CEL(1,J),NBW*6)
C
	RETURN
C
c*irtdat
c
c	return data type info
c	itype = 0	normal mono data
c	itype = 1	tilt set	N1 = # angles stored per section
c			if n1=0 then n2=axis, v1=start ang, v2=delang
c	itype = 2	stereo pairs stored L,R V1= angle V2 = angle R
c	itype = 3	avg mono N1 = number sects avg, N2 = offset per sect
c	itype = 4	avg stereo N1 = number sects avg, N2 = offset per sect
c			V1,V2 = angls L,R
c	n1,n2 are encoded into 1 32bit word, V1,V2 are multby 100 and in 1 word
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
c*irtdel
c
c	Return Delta for each pixel in nm  (columns,rows,sections)
c
c	DELTA(3)		  : pixel spacing in nm (columns,rows,sections)
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
C*IRTEXT
C
C	Returns EXTRA information stored in "unused" poritions of
C	file header (16 words max!!).
C
C	EXTRA		  : Buffer containing data to be placed in extra slot.
C	ISTART		  : Which "extra" element to start at (1-16)
C	NEXTRA		  : Number of words to transfer
C
	ENTRY IRTEXT(ISTREAM,EXTRA,ISTART,NEXTRA)
C
	J = LSTREAM(ISTREAM)
	IF (ISTART .GT. 16 .OR. ISTART .LT. 1) 
     .	STOP 'IRTEXT: ERROR IN START NUMBER'
	JEXTRA = MIN(NEXTRA + ISTART,17) - ISTART
	CALL MOVE(EXTRA,STUFF(ISTART+2,J),NBW*JEXTRA)
C
	RETURN
C
C
C*IRTLAB
C
C	Return label information
C
C	LABELS(20,10)	  : space for labels
C	NL		  : actual # of labels being used
C
	ENTRY IRTLAB(ISTREAM,LABELS,NL)
C
	J = LSTREAM(ISTREAM)
	NL = NLAB(J)
	CALL MOVE(LABELS,LABLS(1,1,J),NL*80)
C
	RETURN
C
C*IRTMAP
C
C	Return MAPCRS information
C
C	MCRS(3)		  : columns,rows,sections mapping info
C
	ENTRY IRTMAP(ISTREAM,MCRS)
C
	j = lstream(istream)
	call move(mcrs,mapcrs(1,j),12)
C
	return
C
c*irtmst
c
c	Return map start information from file. 
c
c	NXYZST(3)		  : map start info
c
	entry irtmst(istream,nxyzst)
c
	j = lstream(istream)
	call move(nxyzst,ncrst(1,j),12)
c
	return
c
C
C*IRTORG
C
C	Returns ORIGIN information
C
C	XORIG,YORIG,ZORIG	  : X,Y,Z origin information
C
	ENTRY IRTORG(ISTREAM,XORIG,YORIG,ZORIG)
C
	J = LSTREAM(ISTREAM)
	XORIG = ORIGXY(1,J)
	YORIG = ORIGXY(2,J)
	ZORIG = stuff(31,j)
C
	RETURN
C
c
c*irtsam
c
c	Return Sampling Size information from file. 
c
c	MXYZ(3)		  : sampling size
c
	entry irtsam(istream,mxyz)
c
	j = lstream(istream)
	call move(mxyz,nxyz(1,j),12)
c
	return
c
C
C*IRTSIZ
C
C	RETURN SIZE information for file. Header NOT actually written!!!
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	MXYZ		  : size of full map Columns, Rows, Sections
C	NXYZST		  : starting # for Columns, Rows, Sections (usually 1) 
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
c*irtnbsym
c
c	returns number of bytes of extra header info
c	mbsym = # bytes 
c
	entry irtnbsym(istream,mbsym)
	j = lstream(istream)
	mbsym = nbsym(j)
	return
c	  
c
c*irtsym
c
c	returns symmetry (extra header) info
c	mbsym = # bytes symmetery
c	jbsym = array with sym info
c
	entry irtsym(istream,mbsym,jbsym)
	j = lstream(istream)
	mbsym = nbsym(j)
	if (mbsym.eq.0) return
	call qseek(j,1,1+nbhdr,1)
	call qread(j,jbsym,mbsym,ier)
	if(mrcflip(j))then
c	    
c	    DNM 2/3/02: if swapping, need to find out if nint and nreal
c	    represent nbytes and flags, in which case swap them all as shorts
c	    Otherwise, swap them as nint longs and nreal floats
c	    
	  call move(idat,stuff(11,j),4)
	  nints = idat(1)
	  nreal = idat(2)
	  if(nbytes_and_flags(nints,nreal).or.(nints+nreal.eq.0))then
	    call convert_shorts(jbsym,mbsym/2)
	  else
	    indconv=1
	    do  i=1,mbsym/(nints+nreal)
	      if(nints.gt.0)call convert_longs(jbsym(indconv),nints)
	      indconv=indconv+nints
	      if(nreal.gt.0)call convert_floats(jbsym(indconv),nreal)
	      indconv=indconv+nreal
	    enddo
	  endif
	endif
c	call move(jbsym,ibsym(1,j),mbsym)
	return
C	  
c	  
c*irtsymtyp
c	  
c	  returns type information of symmetry (extra header) info
c	  itype1 = nint or # of bytes per section
c	  itype2 = nreal or flags for what features are stored per section
c	  
	entry irtsymtyp(istream,itype1,itype2)
	j = lstream(istream)
	call move(idat,stuff(11,j),4)
	itype1 = idat(1)
	itype2 = idat(2)
	return
c
C
C*IRTTLT
C
C	Returns current set of tilt angles
c
c
	entry irttlt(istream,tilt)
c
	j = lstream(istream)
	call move(tilt,stuff(25,j),12)
c
	return
C
C*IRTTLT_orig
C
C	Returns original set of tilt angles
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
c*irtwav
c
c	return wavelength info
c	nwave = # of wavelengths (max 5)
c	wave = vector of nwave wavelengths (in nm)
	entry irtwav(istream,nwave,wavelen)
c
	j = lstream(istream)
	call move(iwavelen,stuff(28,j),12)
	nwave = iwavelen(1)
	do k =1,nwave
	  wavelen(k) = iwavelen(k+1)
	enddo
	return
c
c
99	WRITE(6,2000)
2000	FORMAT(' IRDHDR: End-of-File error !!!')
	STOP 'ERROR'
	END

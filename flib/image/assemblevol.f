* * * * * * * * ASSEMBLEVOL * * * * * * * *
c	  
c	  Assemblevol will assemble a single MRC file from separate files
c	  that form an array of subvolumes in X, Y, and Z.  In effect, it can
c	  take montaged images and compose them into single images, just as
c	  Reducemont can, but its advantage is that it can take the images
c	  from multiple files.  Its primary use is for reassembling a tomogram
c	  after it has been chopped into pieces, using the coordinates output
c	  by Tomopieces.
c
c	  Inputs to the program:
c	  
c	  Name of output file for the assembled volume
c	  
c	  Number of input files in the X, Y and Z dimensions
c	  
c	  For each position in X, enter a line with the starting and ending X
c	  index coordinates (numbered from 0) to be extracted from the files
c	  at that position.  There should be no overlap between the pixels
c	  extracted from files at successive positions.
c	  
c	  Similarly, for each position in Y, enter a line with starting and
c	  ending Y index coordinates to be extracted from files at that
c	  position.
c
c	  For each position in Z, enter a line with starting and ending Z
c	  index coordinates to be extracted from files at that position.
c
c	  The input file names, one per line, in order by increasing X,
c	  then increasing Y, then increasing Z.  For example, if there are
c	  files only in X and Z, enter all of the files in the first row in
c	  Z in order by increasing X, then the files in the second row, etc.
c	  
c	  David Mastronarde, 3/1/01
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2004/06/14 19:26:06  mast
c	  Made it able to deal with more than 19 files in X and Y by having
c	  it open and close files for every section in Z.
c	
c	  Revision 3.2  2002/07/31 23:59:04  mast
c	  Have it transfer titles also
c	
c	  Revision 3.1  2002/07/31 20:05:11  mast
c	  Made it preserve pixel size. Also standardized error output and
c	  made declarations for implicit none.
c	
c	  
	implicit none
	integer idimin,idimout,limfiles,limran,limopen
	parameter (idimin=4100*2100, idimout=4100*4100,limfiles=2000)
	parameter (limran=200,limopen=19)
	integer*4 nx,ny,nz
	COMMON //NX,NY,NZ
C   
	integer*4 NXYZ(3),MXYZ(3),NXYZST(3),NXYZ2(3),MXYZ2(3)
	real*4 ARRAY(idimin),TITLE(20), CELL2(6),brray(idimout),delta(3)
	common /bigarr/array,brray
C	  
	integer*4 ixlo(limran),ixhi(limran),iylo(limran),iyhi(limran)
	integer*4 izlo(limran),izhi(limran)
	CHARACTER*80 FILOUT
	character*80 files(limfiles)
	character*9 dat
	character*8 tim
        character*80 titlech
C   
	EQUIVALENCE (NX,NXYZ)
C   
	integer*4 nfx,nfy,nfz,nx3,ny3,nz3,maxx,maxy,i,ifile,ix,iy,iz
	real*4 dmin2,dmax2,dmean2,dmin,dmax,dmean,tmin,tmax,tmean,tmpmn
	integer*4 mode,mode1,kti,izf,iunit,iyofs,ixofs,nybox,nxbox
	integer*4 ixf,iyf,layerFile
	logical openLayer
	DATA NXYZST/0,0,0/
c
        write(*,'(1x,a,$)')'Output file for assembled volume: '
	READ(5,101)FILout
101     format(a)
c	  
	write(*,'(1x,a,$)')'Numbers of input files in X, Y, and Z: '
	read(5,*)nfx,nfy,nfz
	if(nfx*nfy*nfz.gt.limfiles.or.nfx.gt.limran.or.nfy.gt.limran
     &	    .or.nfz.gt.limran) call errorexit(
     &	    'TOO MANY FILES FOR ARRAYS')
	openLayer = nfx * nfy .le. limopen
c	if(nfx*nfy.gt.limopen) call errorexit(
c     &	    'TOO MANY FILES IN X AND Y TO OPEN AT ONCE')
c	  
	print *,'Enter the starting and ending index coordinates '//
     &	    'for the pixels to extract',
     &	    ' from the files at successive positions in each dimension'
	nx3=0
	ny3=0
	nz3=0
	maxx=0
	maxy=0
	do i=1,nfx
	  write(*,'(1x,a,i3,a,$)')'X coordinates for files at position #'
     &	      ,i,' in X: '
	  read(5,*)ixlo(i),ixhi(i)
	  if (ixlo(i).lt.0)call errorexit('ILLEGAL COORDINATE LESS THAN ZERO')
	  nx3=nx3+ixhi(i)+1-ixlo(i)
	  maxx=max(maxx,ixhi(i)+1-ixlo(i))
	enddo
	do i=1,nfy
	  write(*,'(1x,a,i3,a,$)')'Y coordinates for files at position #',
     &	      i,' in Y: '
	  read(5,*)iylo(i),iyhi(i)
	  if (iylo(i).lt.0)call errorexit('ILLEGAL COORDINATE LESS THAN ZERO')
	  ny3=ny3+iyhi(i)+1-iylo(i)
	  maxy=max(maxy,iyhi(i)+1-iylo(i))
	enddo
	do i=1,nfz
	  write(*,'(1x,a,i3,a,$)')'Z coordinates for files at position #',
     &	      i,' in Z: '
	  read(5,*)izlo(i),izhi(i)
	  if (izlo(i).lt.0)call errorexit('ILLEGAL COORDINATE LESS THAN ZERO')
	  nz3=nz3+izhi(i)+1-izlo(i)
	enddo
c	  
	if(nx3*ny3.gt.idimout)call errorexit(
     &	    'OUTPUT IMAGE TOO BIG FOR ARRAY')

	if(maxx*maxy.gt.idimin)call errorexit(
     &	    'INPUT IMAGES TOO BIG FOR ARRAY')
c	  
	print *,'Enter the input file names at successive positions '//
     &	    'in X, then Y, then Z'
	ifile=1
	call ialprt(.false.)
	do iz=1,nfz
	  do iy=1,nfy
	    do ix=1,nfx
	      write(*,'(1x,a,3i4,a,$)')'Name of file at',ix,iy,iz,': '
	      read(5,101)files(ifile)
	      call imopen(2,files(ifile),'ro')
	      CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
	      call irtdel(2,delta)
	      if(ifile.eq.1) mode1=mode
	      if(mode.ne.mode1)call errorexit(
     &		    'MODE MISMATCH FOR THIS FILE')
	      if(ixhi(ix).ge.nx.or.iyhi(iy).ge.ny.or.izhi(iz).ge.nz)
     &		    call errorexit(
     &		    'UPPER COORDINATE TOO HIGH FOR THIS FILE')
	      call imclose(2)
	      ifile=ifile+1
	    enddo
	  enddo
	enddo
C   
	CALL IMOPEN(1,FILOUT,'NEW')
	NXYZ2(1)=NX3
	NXYZ2(2)=NY3
	NXYZ2(3)=nz3
	MXYZ2(1)=NX3
	MXYZ2(2)=NY3
	MXYZ2(3)=nz3
	CELL2(1)=NX3*delta(1)
	CELL2(2)=NY3*delta(2)
	CELL2(3)=nz3*delta(3)
	CELL2(4)=90.
	CELL2(5)=90.
	CELL2(6)=90.
C   
	call time(tim)
	call date(dat)
        write(titlech,301) dat,tim
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301	FORMAT('ASSEMBLEVOL: Reassemble a volume from pieces',t57,a9,2x,a8)
	CALL ICRHDR(1,NXYZ2,MXYZ2,mode,TITLE,0)
	CALL IALCEL(1,CELL2)
	dmin=1.e10
	dmax=-1.e10
	tmin=0.
c	  
	ifile=1
	do izf=1,nfz
c	    
c	    open the files on this layer if possible
c	    
	  if (openLayer) then
	    do i=1,nfy*nfx
	      call imopen(i+1,files(ifile),'ro')
	      CALL IRDHDR(i+1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
c	      
c		DNM 7/31/02: transfer labels from first file
c
	      if (ifile.eq.1)call itrlab(1,i+1)
	      ifile=ifile+1
	    enddo
	  endif
c	    
c	   loop on the sections to be composed
c
	  do iz=izlo(izf),izhi(izf)
	    iunit=2
	    iyofs=0
c	      
c	      loop on the files in X and Y
c
	    layerFile = ifile
	    do iyf=1,nfy
	      ixofs=0
	      nybox=iyhi(iyf)+1-iylo(iyf)
	      do ixf=1,nfx
c		  
c		  open files one at a time if necessary
c
		if (.not.openLayer) then
		  call imopen(2, files(layerFile),'ro')
		  CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
		  if (ixf .eq. 1 .and. iyf .eq. 1 .and. izf .eq. 1 .and.
     &		      iz .eq. izlo(izf)) call itrlab(1, 2)
		  layerFile = layerFile + 1
		endif
c		  
c		  read the section, and insert into big array
c
		call imposn(iunit,iz,0)
		nxbox=ixhi(ixf)+1-ixlo(ixf)
		call irdpas(iunit,array,nxbox,nybox,ixlo(ixf),ixhi(ixf),
     &		    iylo(iyf),iyhi(iyf), *99)
		call insert_array(array,nxbox,nybox,brray,nx3,ny3,ixofs,
     &		    iyofs)
		ixofs=ixofs+nxbox
		if (openLayer) then
		  iunit=iunit+1
		else
		  call imclose(2)
		endif
	      enddo
	      iyofs=iyofs+nybox
	    enddo
c	      
c	      section done, get density and write it
c
	    call iclden(brray,nx3,ny3,1,nx3,1,ny3,tmin,tmax, tmpmn)
	    dmin=min(dmin,tmin)
	    dmax=max(dmax,tmax)
	    tmean=tmean+tmpmn
	    call iwrsec(1,brray)
	  enddo
c	    
c	    close layer files if opened; otherwise set file number for next
c	    layer
c
	  if (openLayer) then
	    do i=1,nfy*nfx
	      call imclose(i+1)
	    enddo
	  else
	    ifile = layerFile
	  endif
	enddo
	dmean=tmean/nz3
	CALL IWRHDR(1,TITLE,1,DMIN,DMAX,DMEAN)
	CALL IMCLOSE(1)
	call exit(0)
99	call errorexit('READING FILE')
	end

	subroutine insert_array(array,nxbox,nybox,brray,nx3,ny3,ixofs,
     &		    iyofs)
	implicit none
	integer*4 nxbox,nybox,nx3,ny3,ixofs,iyofs,ix,iy
	real*4 array(nxbox,nybox),brray(nx3,ny3)
	do iy=1,nybox
	  do ix=1,nxbox
	    brray(ix+ixofs,iy+iyofs)=array(ix,iy)
	  enddo
	enddo
	return
	end

	subroutine errorexit(message)
	implicit none
	character*(*) message
	print *
	print *,'ERROR: ASSEMBLEVOL - ',message
	call exit(1)
	end

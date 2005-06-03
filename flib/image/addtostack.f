*************ADDTOSTACK.FOR**********************************************
*   
*	  ADDTOSTACK will append sections from one or more files to an
*	  existing image file.  The X and Y dimensions of all files must match.
*	  All sections will be appended from the files that are being added.
*	  No image transformation or scaling is performed.  The header of the
*	  resulting image file will be set to give "pixel spacing" of 1 (grid
*	  and cell sizes equal to image size).
*	  
*	  There is one option: whether or not to make a copy of the existing
c	  file before adding sections to it.  If you elect to make a copy,
c	  this will take some extra time and will require the disk space to
c	  hold 2 copies of the file.  You will then be able to examine the new
c	  file to see if it meets your expectations before deleting the old
c	  file with a purge command.  If you do not make a copy, the program
c	  will run quicker and less disk space will be required, but it is
c	  conceivable that the file will become unusable if certain types of
c	  errors occur.  (If you specify a non-existent file or one of the
c	  wrong size, or if there is an error reading one of the files, the
c	  file being appended to will NOT become unusable.)
c	  
c	  Entries to the program:
c	  
c	  Name of file to append to
c	  
c	  0 to append to existing file, or 1 to make new copy and append to it
c	  
c	  Number of files to append to the file
c	  
c	  Names of these files, one per line
c	  
c
c	  David Mastronarde  5/28/90
c
************************************************************************
*   
	parameter (idim=5100,lmfil=1000)
	COMMON //NX,NY,NZ
C   
	DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),ARRAY(idim*idim),TITLE(20),
     &      NXYZ2(3),cell(6),mxyz2(3)
C   
	CHARACTER*80 FILIN(lmfil),FILOUT
	character*80 comline
C   
	EQUIVALENCE (NX,NXYZ)
C   
	character dat*9,tim*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	common /bigarr/array
C
	write(*,'(1x,a,$)')'Name of file to add sections to: '
        read(*,101)filout
101	format(a)
	write(*,102)
102	format(' Enter 1 to make copy of file and append to copy',/
     &	    ,9x,'(which needs more free disk space and requires you',
     &	    ' to purge old file),',/,'    or 0 to append to existing',
     &	    ' file',/,9x,'(which is quicker but could make the file'
     &	    ,' unusable in case of error)')
	read(*,*)ifcopy
c
	write(*,'(1x,a,$)')
     &	    'Number of files to add onto end of this file: '
	read(*,*)nfilein
	if (nfilein.gt.lmfil) then
	  print *,'- TOO MANY FILES FOR NAME ARRAY'
	  stop
	endif
	print *,'Enter file names, one per line'
	do i=1,nfilein
	  read(*,101)filin(i)
	enddo
c
	if(ifcopy.ne.0)call copy_to_backup(filout)
c
	call imopen(2,filout,'old')
	call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
	call time(tim)
	call date(dat)
	call irtsiz(2,nxyz,mxyz,nxyzst)
	call irtcel(2,cell)
C   
	  IF ((NX*NY.GT.idim*idim)) THEN
	    PRINT *,' INPUT ARRAY TOO LARGE.'
	    STOP
	  ENDIF
	dsum=nz*dmean
	call imposn(2,nz,0)
	nzorig=nz
c	  
	do ifil=1,nfilein
	  call imopen(1,filin(ifil),'ro')
	  call irdhdr(1,nxyz2,mxyz2,mode2,dmin2,dmax2,dmean2)
	  if(nxyz2(1).ne.nx.or.nxyz2(2).ne.ny)then
	    print *,'FILE SIZE MISMATCH: APPEND OPERATION TRUNCATED'
	    go to 90
	  endif
	  do iz=1,nxyz2(3)
	    call irdsec(1,array,*99)
	    call iclden(array,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
	    dmin=min(dmin,tmin)
	    dmax=max(dmax,tmax)
	    dsum=dsum+tmean
	    nz=nz+1
	    call iwrsec(2,array)
	  enddo
	  call imclose(1)
	enddo
90	dmean=dsum/nz
c
c	  12/21/00 DNM: switch to unconditionally setting the header to the
c	  values for proper scaling
c
	call ialsiz_sam_cel(2,nx,ny,nz)
c
c	call ialsiz(2,nxyz,nxyzst)
c	  if mxyz=nxyz, keep this relationship
c	if(mxyz(1).eq.nx.and.mxyz(2).eq.ny.and.mxyz(3).eq.nzorig)then
c	  call ialsam(2,nxyz)
c	  cell(3)=(cell(3)*nz)/nzorig
c	  CALL IALCEL(2,CELL)
c	endif
c
c 7/7/00 CER: remove the encodes
c
C	ENCODE(80,302,TITLE)nz-nzorig,nfilein,dat,tim
	write(titlech,302) nz-nzorig,nfilein,dat,tim
302	FORMAT('ADDTOSTACK:',i4,' sections from',i3,' files appended'
     &	    ,t57,a9,2x,a8)
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
C	  
	CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
	CALL IMCLOSE(2)
C   
	STOP
99	WRITE(6,450)
450	FORMAT(' END OF IMAGE WHILE READING')
	go to 90
	END

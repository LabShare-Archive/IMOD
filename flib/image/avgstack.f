	Program AVGSTACK

c	-------------------------------------------------------------------
c	--- This program reads in an image file containing multiple	---
c	--- sections and averages the sections into a singles output	---
c	--- image.							---
c	--- Written: 27-Jan-1989 - sjm					---
c	--- Updates: 19-Dec-1989 - dnm: fixed cell setting in header	---
c	-------------------------------------------------------------------


	parameter (maxarr=2100)

	real	array(maxarr*maxarr), brray(maxarr*maxarr)
	real	dmax, dmin, dmean, title(20), cell(6)
	integer	nxyz(3), mxyz(3), nxyzst(3)
	equivalence (nxyz,nx)
	common//nx,ny,nz
	character*80 input, output, table
	character dat*9, tim*8
	data nxyzst / 0.,0.,0. /


c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
c
c	---------------------------------
c	--- initialize and input info ---

	write ( *, 1000 ) ' Input'
	read  ( *, 2000 ) input
	write ( *, 1000 ) 'Output'
	read  ( *, 2000 ) output

	call imopen(1,input,'ro')
	call imopen(2,output,'new')

	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean) !--- Get Image Header ---!

	if (nx*ny .gt. maxarr**2)stop '- IMAGE TOO LARGE FOR ARRAYS'
	nz = nz - 1	!--- first section is 0, so nz = total # sections - 1
	ifst = 0
	ilst = nz
	write ( *, * ) ' Sections to average [First,Last] :'
	read  ( *, * ) ifst, ilst

	if ( (ifst.eq.0) .and. (ilst.eq.0) ) ilst = nz !-Whole stack default
	ilst = min(ilst,nz)	!--- Check end limit ---!
	ifst = min(ifst,nz)	!--- Check beginning limit ---!
	ifst = min(ifst,ilst)	!--- Check beginning/end order ---!


c	-------------------------------
c	--- Clear the storage array ---

	do iy = 1 , ny*nx
	  array(iy) = 0.0
	end do

	call itrhdr(2,1)
	call ialmod(2,2)
	call ialnbsym(2,0)
	call ialsymtyp(2,0,0)

	nxyz(3) = 1
	mxyz(3) = 1
	call ialsam(2,mxyz)
	call ialsiz(2,nxyz,nxyzst)

	do i=1,3
	  cell(i)=nxyz(i)
	  cell(i+3)=90.
	enddo
	call ialcel(2,cell)

	call date(dat)
	call time(tim)
	numsec = ilst - ifst + 1

c 7/7/00 CER: remove the encodes
c
C       encode ( 80, 3000, title ) numsec, dat, tim
        write(titlech,3000) numsec,dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20)


c	------------------------------------------------------------
c	--- Add all requested sections from the input image file ---

	call imposn(1,ifst,0)	!--- Move to first section ---!
	do isec = ifst, ilst
	   write ( *, * ) ' Adding section ', isec
	   call irdsec(1,brray,*100) 
	   do iy = 1 , nx*ny
		 array(iy) = array(iy) + brray(iy)
	   end do
	end do

100	call imclose(1)


c	--------------------------------
c	--- Average the output image ---
	write ( *, * ) ' Averaging stack...'
	do iy = 1 , nx*ny
	      array(iy) = array(iy) / float(numsec)
	end do


c	-----------------------------
c	--- Write out the average ---


	call iclden(array,nx,ny,1,nx,1,ny,dmin,dmax,dmean)

	call iwrhdr(2,title,1,dmin,dmax,dmean)

	call iwrsec(2,array)

	call imclose(2)

	call exit(0)

1000	format ( 1x, a6, ' file : ', $ )
2000	format ( a50 )
3000	format ( 'AVGSTACK: ', i4, ' sections averaged.',
     &	    t57, a9, 2x, a8 )

	end

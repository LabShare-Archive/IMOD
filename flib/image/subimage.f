	Program SUBIMAGE

c	-----------------------------------------------------
c	--- This program subtracts one image from another ---
c	--- Written: 8-feb-1989 sjm			  ---
c	--- Updates: 31-may-1989, sjm			  ---
c       --- DNM 11/4/00: added multiple-section capability---
c       --- DNM 11/6/01: added ability to get statistics only
c	-----------------------------------------------------
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$


	parameter (maxarr=3100)
	parameter (maxlist=2000)

	real	array(maxarr*maxarr), brray(maxarr*maxarr),
     &	    max, min, title(20), cell(6)
	common /bigarr/ array,brray
	integer	nxyz(3), mxyz(3), nxyzst(3), asec, bsec, nxyz2(3)
	integer*4 listasec(maxlist),listbsec(maxlist)
	equivalence (nxyz,nx)
	common//nx,ny,nz
	character*80 afile, bfile, cfile
	character dat*9, tim*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
        data nxyzst / 0.,0.,0. /
	data cell /0.,0.,0.,90.,90.,90./


c	---------------------------------
c	--- initialize and input info ---

	write ( *, '( a )' ) ' This program will subtract sections of'
	write ( *, '( a )' ) ' file B from sections of file A.  The '
	write ( *, '( a )' ) ' resulting file (C) contains the difference'
	write ( *, '( a )' ) ' images of (A-B).'

	write ( *, '( a, $ )' ) ' Name of file A : '
	read  ( *, '( a )' )	afile
	print *,'Enter list of section numbers from file A (ranges OK)'
	call rdlist(5,listasec,nasec)
	if (nasec.gt.maxlist) stop 'TOO MANY SECTIONS FOR ARRAYS'
	write ( *, '( a, $ )' ) ' Name of file B : '
	read  ( *, '( a )' )	bfile
	write ( *, '( a,i5,a )' ) ' Enter list of',nasec,
     &	    ' corresponding sections from file B (ranges OK)'
	call rdlist(5,listbsec,nbsec)
	if (nasec.ne.nbsec)stop 'NUMBER OF SECTIONS DOES NOT MATCH'
	write ( *, '( a, $ )' )
     &	    ' Name of file C, or return for statistics only : '
	read  ( *, '( a )' )	cfile

	call imopen(1,afile,'ro')
	call imopen(2,bfile,'ro')

	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean) !--- Get Image Header 1--!
	call irdhdr(2,nxyz2,mxyz,mode,dmin,dmax,dmean) !--- Get Image Header 2--!

	if(nxyz2(1).ne.nx.or.nxyz(2).ne.ny)
     &	    stop 'IMAGE SIZES DO NOT MATCH'

	do i=1,nasec
	  asec=listasec(i)
	  bsec=listbsec(i)
	  if(asec.lt.0.or.asec.ge.nz.or.bsec.lt.0.or.bsec.ge.nxyz2(3))
     &	      stop 'ILLEGAL SECTION NUMBER'
	enddo

	if(nx*ny.gt.maxarr**2)stop 'IMAGES TOO LARGE FOR ARRAYS'

	if(cfile.ne.' ')then
	  call imopen(3,cfile,'new')
	  call itrhdr(3,1)
	endif
	dsum=0.
	sdsum=0.
	sdsumsq=0.
	print *,'Section      Min            Max            Mean'//
     &	    '           S.D.'
	do isec=1,nasec
	  asec=listasec(isec)
	  bsec=listbsec(isec)

	  call imposn(1,asec,0)
	  call imposn(2,bsec,0)
	  call irdsec(1,array,*100) 
	  call irdsec(2,brray,*100) 

c	    -----------------------------------------
c	    --- Subtract section B from section A ---

	  do i = 1 , nx*ny
	    array(i) = array(i) - brray(i)
	  end do


c	---------------------------------
c	--- Write out the difference  ---
	  if(cfile.ne.' ') call iwrsec(3,array)
c	    call iclden(array,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
	  call iclavgsd(array,nx,ny,1,nx,1,ny,tmin,tmax,sum,sumsq,tmean,sd)
	  write(6,4000)asec,tmin,tmax,tmean,sd
	  sdsum=sdsum+sum
	  sdsumsq=sdsumsq+sumsq
	  if(isec.eq.1)then
	    dmin=tmin
	    dmax=tmax
	  else
	    dmin=min(dmin,tmin)
	    dmax=max(dmax,tmax)
	  endif
	  dsum=dsum+tmean
	enddo

	call imclose(1)
	call imclose(2)
	dmean=dsum/nasec
	totpix=nx*ny*float(nasec)
	sd=sqrt(max(0.,(sdsumsq-totpix*dmean**2)/(totpix-1.)))
	if(nasec.gt.1)write(6,5000)dmin,dmax,dmean,sd

	if(cfile.eq.' ')call exit(0)

	nxyz(3) = nasec
	mxyz(3) = nasec
	cell(1) = float(nx)
	cell(2) = float(ny)
	cell(3) = nasec
	call ialsam(3,mxyz)
	call ialcel(3,cell)
	call ialsiz(3,nxyz,nxyzst)

	call date(dat)
	call time(tim)
c
c 7/7/00 CER: remove the encodes
c
c       encode ( 80, 3000, title ) dat, tim
        write(titlech,3000) dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20)

	call iwrhdr(3,title,1,dmin,dmax,dmean)

	call imclose(3)

	call exit(0)

1000	format ( 1x, a6, ' file : ', $ )
2000	format ( a30 )
3000	format ( 'SUBIMAGE: Subtract section B from section A.',
     &	    t57, a9, 2x, a8 )
4000	format(i5,4f15.4)
5000	format(' all ',4f15.4)

100	stop 'ERROR READING FROM FILE'
	end

************BOXAVG**************
*	  
*	  BOXAVG will average images at points specified in a file of
c	  coordinates.  The file consists of three integer index coordinates
c	  per point (x, y, z), such as is put out by the POINT routine in WIMP
c
c	  The inputs are:
c	  
c	  Input image file
c	  Name of piece list file if image is a montage; otherwise Return
c	  Name of file of point coordinates
c	  Name of output file for average
c	  
c	  0 if all coordinates in point file correspond to features; or 1 if
c	  .   some points are corners that must be omitted (a "teaching" file)
c	  
c	  Size of box for averaging, if a square box centered on the point
c	  .   coordinate is desired.  Otherwise, enter -1 to specify the box
c	  
c	  IF you entered -1, next enter the number of pixels in the box to the
c	  left, to the right, below and abovethe central pixel.
c	  
c	  0 to make the output image the same size as the box, 1 to make it
c	  .   the same size as the input image, and -1 to specify the size
c	  
c	  IF you entered -1, next enter the X and Y pixel dimensions of the
c	  .   output image
c	  
c	  Data mode of output file; the default is the same as the input
c	  
c	  0 to do one pass and simply average all of the boxes around the given
c	  .   point coordinates; or 1 to do a second pass, in which the
c	  .   central coordinate of each individual point will be adjusted to
c	  .   produce the best fit between the pixels around that point and the
c	  .   average from the first pass.
c	  
c	  IF the output file is larger than the box size, you must enter the
c	  .   next two lines:
c	  . Value to fill the rest of the output array with.  The default is
c	  .    the mean value of the edge of the box
c	  . 0 to leave the box in one piece in the lower-left corner of the
c	  .    output array, or 1 to shift the center of the box to the lower-
c	  .    left corner and split the box into 4 pieces in the 4 corners of
c	  .    output array
c	  
c	  IF the output data mode is 2 (real values), then you must enter one
c	  .   more line: a value to subtract from the entire output array.  The
c	  .   default value will make the mean of the output array be zero.
c	  
c	  David Mastronarde 4/26/89
*   
	parameter (ixdim=2100,iydim=2100,maxshift=3,idimbox=256)
	parameter (limpnt=10000,limpcl=50000)
	COMMON //NX,NY,NZ
C   
	DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),
     &      ARRAY(ixdim,iydim),BRRAY(ixdim,iydim),TITLE(20),
     &      NXYZ2(3),MXYZ2(3),CELL2(6),crray(idimbox,idimbox)
C	  
	integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
	CHARACTER*80 FILIN,FILOUT,filpoint,plfile
	character*9 dat
	character*8 tim
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
C   
	EQUIVALENCE (NX,NXYZ)
C   
        integer*4 ipx(limpnt),ipy(limpnt),ipz(limpnt),irx(limpnt),
     &	    iry(limpnt),irz(limpnt)
	dimension listsect(1000)
	parameter (lmzon=100)
	dimension ixzon0(lmzon),ixzon1(lmzon),nxzon(lmzon),izzon(lmzon)
     &	    ,iyzon0(lmzon),iyzon1(lmzon),nyzon(lmzon),indzon(lmzon)
        NX3=-1
        NY3=-1
        newmode=-1
	DATA NXYZST/0,0,0/
        write(*,'(1x,a,$)')'Image input file: '
	READ(5,101)FILIN
101     format(a)
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C   
        IF ((NX.GT.ixdim.or.ny.gt.iydim)) THEN
          PRINT *,' INPUT ARRAY TOO LARGE.'
          STOP
        ENDIF
c	  
	write(*,'(1x,a,$)')'Piece list file if image is montage,'//
     &	    ' otherwise Return: '
	read(*,101)plfile
	call read_piece_list(plfile,ixpclist,iypclist,izpclist,npclist)
c
        write(*,'(1x,a,$)')'File of points to be averaged: '
	read(5,101)filpoint
	call dopen(3,filpoint,'ro','f')
        write(*,'(1x,a,$)')'Output file for average: '
	READ(5,101)FILOUT
c
	write(*,'(1x,a,$)')'1 if points are a teaching file with'//
     &	    ' corners that should be omitted: '
	read(*,*)ifteach
c
c   read in the points in point file
c
	if(ifteach.eq.0)then
	  npoint=0
10	  j=npoint+1
	  read(3,*,end=15)ixtm,iytm,iztm
	  call lookup_piece(ixpclist,iypclist,izpclist,npclist,
     &	      nx,ny,ixtm,iytm,iztm,ipx(j),ipy(j),ipz(j))
	  npoint=j
	  go to 10
	else
	  call readteach(nx,ny,ixpclist,iypclist,izpclist,npclist,
     &	      npoint,nzone, nxzon,nyzon,izzon,indzon,indcur ,ixzon0,
     &	      ixzon1,iyzon0, iyzon1,irx,iry,irz,ipx,ipy,ipz,lmzon)
	endif
c	  
15	write(*,'(1x,a,$)')'Size of box for a centered square box,'//
     &	    ' or -1 to specify box offsets: '
	read(*,*)iboxsize
	if(iboxsize.gt.0)then
	  ixlo=-iboxsize/2
	  iylo=ixlo
	  ixhi=iboxsize+ixlo-1
	  iyhi=ixhi
	else
	  write(*,'(1x,a,$)')'# of pixels to left, right, below and'//
     &	      ' above central pixel: '
	  read(*,*)ixlo,ixhi,iylo,iyhi
	  ixlo=-ixlo
	  iylo=-iylo
	endif
        nxbox=ixhi+1-ixlo
        nybox=iyhi+1-iylo
	if (nxbox.gt.idimbox.or.nybox.gt.idimbox)then
	  print *,'Box too large for array, try again'
	  go to 15
	endif
c
	print *,'Enter 0 to make output image be same size as'//
     &	    ' box being averaged,'
	print *,'   or 1 to make output image same size as input images,'
	write(*,'(1x,a,$)')'   or -1 to specify size of output image: '
	read(*,*)isizeopt
c	  
	if(isizeopt.eq.0)then
	  nx3=nxbox
	  ny3=nybox
	elseif(isizeopt.gt.0)then
	  nx3=nx
	  ny3=ny
	else
	  write(*,'(1x,a,$)')'NX and NY of output image: '
	  read(*,*)nx3,ny3
	endif
c
17	newmode=mode
        write(*,'(1x,a,i2,a,$)')
     &      'Mode of output file [',newmode,'=default, same as input]: '
        read(*,*)newmode
	if(newmode.lt.0.or.newmode.gt.15.or.
     &	    (newmode.gt.2.and.newmode.lt.9))go to 17
c	  
	print *,'Enter 0 to take average in one pass without recentering,'
	write(*,'(1x,a,$)')' or 1 to do second pass, centering each'//
     &	    ' element on average from first pass: '
	read(*,*)ifpass
	npass=max(1,ifpass+1)
C   
C   Create output header.
C   
	CALL IMOPEN(2,FILOUT,'NEW')
	NXYZ2(1)=NX3
	NXYZ2(2)=NY3
	NXYZ2(3)=1
	MXYZ2(1)=NX3
	MXYZ2(2)=NY3
	MXYZ2(3)=1
	CELL2(1)=NX3
	CELL2(2)=NY3
	CELL2(3)=1
	CELL2(4)=90.
	CELL2(5)=90.
	CELL2(6)=90.
C   
	call time(tim)
	call date(dat)
c
c 7/7/00 CER: remove the encodes
c
C       ENCODE(80,301,TITLE)dat,tim
	write(titlech,301) dat,tim
301     FORMAT('BOXAVG: average of boxes',32x,a9,2x,a8)
	read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
	CALL ICRHDR(2,NXYZ2,MXYZ2,newmode,TITLE,0)
	call itrlab(2,1)
	CALL IALCEL(2,CELL2)
c
c	  make list of z values
c
	nzlist=0
        do j=1,npoint
          notonlist=1
          do list=1,nzlist
	    izpnt=ipz(j)
            if(izpnt.eq.listsect(list).or.izpnt.lt.0)notonlist=0
          enddo
          if(notonlist.eq.1)then
            nzlist=nzlist+1
            listsect(nzlist)=ipz(j)
          endif
        enddo
	do ipass=1,npass
c
c	    copy last sum into c array  and zero the sum array
c
	  nadded=0
	  do iy=1,nybox
	    do ix=1,nxbox
	      crray(ix,iy)=brray(ix,iy)
	      brray(ix,iy)=0.
	    enddo
	  enddo
c
c	    loop over the sections in the list
c
	  do list=1,nzlist
c
c	      read section if it exists
c
	    iz=listsect(list)
	    if(iz.ge.0.and.iz.lt.nz)then
	      call imposn(1,iz,0)
	      call irdpas(1,array,ixdim,iydim,0,nx-1,0,ny-1,*99)
c
c		look for points in that section and far enough from edge
c
	      do jpoint=1,npoint
		kx=ipx(jpoint)
		ky=ipy(jpoint)
		kz=ipz(jpoint)
		if(kz.eq.iz.and.
     &		    kx+ixlo.ge.0.and.kx+ixhi.lt.nx.and.
     &		    ky+iylo.ge.0.and.ky+iyhi.lt.ny)then
		  idxbest=0
		  idybest=0
		  if(ipass.gt.1)then
c
c		      for pass>1, find best offset in x and y for matching in
c		      to existing sum
c		      NOTE SD of difference works much better than convolution
c
c		    prodmax=-1.e20
		    sdmin=1.e10
		    do idx=-maxshift,maxshift
		      do idy=-maxshift,maxshift
c			prod=0
			sum=0.
			sumsq=0.
			kxofs=kx+idx+ixlo
			kyofs=ky+idy+iylo
			if(kxofs.ge.0.and.kxofs+nxbox.le.nx.and.
     &			    kyofs.ge.0.and.kyofs+nybox.le.ny)then
c
			  do iy=1,nybox
			    do ix=1,nxbox
c			      prod=prod+crray(ix,iy)*array(ix+kxofs,
c     &				  iy+kyofs)
			      diff=crray(ix,iy)-array(ix+kxofs,
     &				  iy+kyofs)
			      sum=sum+diff
			      sumsq=sumsq+diff**2
			    enddo
			  enddo
c
			  nsum=nxbox*nybox
			  sd=sqrt((sumsq-sum**2/nsum)/(nsum-1.))
			  if(sd.lt.sdmin)then
			    sdmin=sd
c			  if(prod.gt.prodmax)then
c			    prodmax=prod
			    idxbest=idx
			    idybest=idy
			  endif
			endif
		      enddo
		    enddo
c		    print *,idxbest,idybest
		  endif
c
c		    now add it into the sum with the best offsets
c
		  kxofs=kx+idxbest+ixlo
		  kyofs=ky+idybest+iylo
		  do iy=1,nybox
		    do ix=1,nxbox
		      brray(ix,iy)=brray(ix,iy)+array(ix+kxofs,iy+kyofs)
		    enddo
		  enddo
		  nadded=nadded+1
		endif
	      enddo
	    endif
	  enddo
c
c	    divide it by the number of points added to get average
c
	  do iy=1,nybox
	    do ix=1,nxbox
	      brray(ix,iy)=brray(ix,iy)/nadded
	    enddo
	  enddo
	enddo
	print *,nadded,' points averaged'
c
c	  find mean of edge of box and total mean
c
        sum=0.
        do ix=1,nxbox
          sum=sum+brray(ix,1)+brray(ix,nybox)
        enddo
        do iy=2,nybox-1
          sum=sum+brray(1,iy)+brray(nxbox,iy)
        enddo
        edge=sum/(2*nxbox+2*(nybox-1))
        call iclden(brray,ixdim,iydim,1,nxbox,1,nybox,dmin,dmax,dmean)
c	
c   if box is not equal to output array, dflt fill the rest with the edge mean
c
        write(*,'(a,4f10.2)')' min, max, mean, edge:'
     &      ,dmin,dmax,dmean,edge
        fill=edge
        bias=edge+(dmean-edge)*float(nxbox*nybox)/float(nx3*ny3)
        ifsplit=1
c
        if(nx3.ne.nxbox.or.ny3.ne.nybox)then
          write(*,'(1x,a,f10.3,a,$)') 'Value to fill rest of array'//
     &	      ' [default' ,fill,' = edge]: '
	  read(*,*)fill
	  write(*,'(1x,a,$)')'1 to move center of box to lower-left '//
     &	      'corner of output image, 0 not to: '
	  read(*,*)ifsplit
c
          if(ifsplit.eq.0)then
c
c	      if not splitting, just fill rest of array
c
            do iy=1,ny3
              do ix=1,nx3
                if(ix.gt.nxbox.or.iy.gt.nybox)brray(ix,iy)=fill
              enddo
            enddo
          else
c	      
c	      otherwise copy into array, then fill brray, then split
c
            do iy=1,nybox
              do ix=1,nxbox
                array(ix,iy)=brray(ix,iy)
              enddo
            enddo
c
            do iy=1,ny3
              do ix=1,nx3
                brray(ix,iy)=fill
              enddo
            enddo
c
	    do iy=1,nybox
              do ix=1,nxbox
                ixnew=ix+ixlo
                iynew=iy+iylo
                if(ixnew.le.0)ixnew=ixnew+nx3
                if(iynew.le.0)iynew=iynew+ny3
                brray(ixnew,iynew)=array(ix,iy)
              enddo
            enddo
          endif
        endif
c
c	  if new mode=2, default is shift to make mean zero
c
        if(newmode.eq.2)then
          write(*,'(1x,a,f10.3,a,$)')
     &	      'Offset to subtract from whole image [dflt ='
     &	      ,bias,'will make mean 0]: '
          read(*,*)bias
c
          do iy=1,ny3
            do ix=1,nx3
              brray(ix,iy)=brray(ix,iy)-bias
            enddo
          enddo
        endif
c
        if(mode.eq.newmode.or.newmode.eq.2)then
c
c	    get the min, max and mean
c
          call iclden(brray,ixdim,iydim,1,nx3,1,ny3,dmin,dmax,dmean)
        else
c
c	    otherwise, if mode not = new mode, rescale
c
          call isetdn(brray,ixdim,iydim,newmode,
     &        1,nx3,1,ny3,dmin,dmax,dmean)
        endif
        call irepak(brray,brray,ixdim,iydim,0,nx3-1,0,ny3-1)
        call iwrsec(2,brray)
C   
	CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
	CALL IMCLOSE(2)
C   
	STOP
99	WRITE(6,450)
450	FORMAT(' END OF IMAGE WHILE READING')
	STOP
	END




c	  READTEACH reads in a file of teaching points, figures out which
c	  points are the corners of new zones, returns descriptive parameters
c	  of the zones and allocates indexes for each of the zones that will
c	  be read in.
c
	subroutine readteach(nxim,nyim,ixpclist,iypclist,izpclist,
     &	    npclist,nreal,nzone,nxzon,nyzon,izzon,indzon,indcur
     &	    ,ixzon0,ixzon1,iyzon0,iyzon1,ixreal,iyreal,izreal,ixpc,iypc,
     &	    izpc,lmzon)
c
	dimension ixzon0(*),ixzon1(*),nxzon(*),izzon(*)
     &	    ,iyzon0(*),iyzon1(*),nyzon(*),indzon(*)
     &	    ,ixpclist(*),iypclist(*),izpclist(*)
c
	integer*4 ixreal(*),iyreal(*),izreal(*),ixpc(*),iypc(*),izpc(*)
	logical error
c
	nreal=0
	nzone=0
	indcur=1
c
c	  read next point: loop back to here
c
5	read(3,'(3i6)',end=7)ixtmrl1,iytmrl1,iztmrl1
	call lookup_piece(ixpclist,iypclist,izpclist, npclist,nxim,nyim,
     &	    ixtmrl1,iytmrl1,iztmrl1,ixtmpc1,iytmpc1,iztmpc1)
c
c	  if no zones yet or it's outside any zone, start a new zone
	izon=nzone
	do while(izon.gt.0.and.(iztmpc1.ne.izzon(izon).or.
     &	    ixtmpc1.lt.ixzon0(izon).or.ixtmpc1.gt.ixzon1(izon).or.
     &	    iytmpc1.lt.iyzon0(izon).or.iytmpc1.gt.iyzon1(izon)))
	  izon=izon-1
	enddo
	if(izon.eq.0)then
c
c	    read other corner of zone
	  read(3,'(3i6)',end=7)ixtmrl2,iytmrl2,iztmrl2
	  call lookup_piece(ixpclist,iypclist,izpclist, npclist,nxim,
     &	      nyim, ixtmrl2,iytmrl2,iztmrl2,ixtmpc2,iytmpc2,iztmpc2)
c
c	    point must have same z value
	  error=iztmpc1.ne.iztmpc2
c
c	    check that point is not in any previous zone
	  do i=1,nzone
	    if(iztmpc2.eq.izzon(i)
     &		.and.ixtmpc2.ge.ixzon0(i).and.ixtmpc2.le.ixzon1(i)
     &		.and.iytmpc2.ge.iyzon0(i).and.iytmpc2.le.iyzon1(i))
     &		error=.true.
	  enddo
	  if(error)then
	    print *,'box definition error at point #',nreal+1
	    stop
	  endif
c
c	    OK, define new zone
c
	  nzone=nzone+1
	  if(nzone.gt.lmzon)go to 7
	  ixzon0(nzone)=min(ixtmpc1,ixtmpc2)	!corner coordinates
	  ixzon1(nzone)=max(ixtmpc1,ixtmpc2)
	  iyzon0(nzone)=min(iytmpc1,iytmpc2)
	  iyzon1(nzone)=max(iytmpc1,iytmpc2)
	  nxzon(nzone)=ixzon1(nzone)+1-ixzon0(nzone) !nx and ny
	  nyzon(nzone)=iyzon1(nzone)+1-iyzon0(nzone)
	  izzon(nzone)=iztmpc1
	  indzon(nzone)=indcur			!index to packed image array 
	  indcur=indcur+nxzon(nzone)*nyzon(nzone)
	else
c
c	    if in some zone, add point to list of real points
c
	  nreal=nreal+1
	  ixreal(nreal)=ixtmrl1
	  iyreal(nreal)=iytmrl1
	  izreal(nreal)=iztmrl1
	  ixpc(nreal)=ixtmpc1
	  iypc(nreal)=iytmpc1
	  izpc(nreal)=iztmpc1
	endif
	go to 5
7	return
	end

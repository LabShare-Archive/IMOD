* * * * * BOXSTARTEND * * * * *
c	  
c	  BOXSTARTEND will clip out volumes of image centered on the starting
c	  or ending points of model contours, or on all points in a model
c	  contour.  It can place each volume into a separate file, or stack all
c	  the extracted volumes into a single output file.  In the latter
c	  case, the program can generate two lists of piece coordinates to
c	  allow the volumes to be examined in two different ways.  One list
c	  will assemble all of the volumes into a single square array, with
c	  the extracted starting or ending points all appearing on the same
c	  section.  The other list will display each piece in its true
c	  position in the original volume of images.
c	  
c	  Entries to program:
c	  
c	  Image file name
c	  Piece list file name, if image file is a montage
c	  Model file name
c	  
c	  0 to take the coordinates in the model as they are, or 1 to transform
c	  .   the coordinates by a set of transformations, or -1 to
c	  .   back-transform the coordinates.  The latter would be used to
c	  .   take coordinates from an aligned model and clip out images from
c	  .   unaligned images.
c	  
c	  IF you entered -1 or 1 for transformed coordinates, next enter the
c	  .  name of the file with the transforms.  The first transform in the
c	  .  file should correspond to the first section in the file.
c
c	  Minimum and maximum X, and minimum and maximum Y index coordinates
c	  .  within which starting or ending points must be contained in order
c	  .  for them to be clipped out, or / for no limits on coordinates
c	  
c	  Minimum and maximum section numbers within which the clipped out
c	  .  volumes must be contained, or / for no limits on section number
c	  
c	  List of numbers of objects containing contours whose endpoints
c	  .  should be selected, or Return for all objects.  Ranges may be
c	  .  entered, e.g. 1-3,6.
c
c	  0 to clip out starting points, or 1 to clip out ending points, or -1
c	  .  to clip out all points.
c	  
c	  Size of box, in pixels
c	  
c	  Number of sections before endpoint to clip out, and number of
c	  .  sections after endpoint to clip out
c	  
c	  0 to output all images into a single file, or 1 to output each
c	  .  box into a separate, numbered file
c	  
c	  IF you entered 1 for separate files, next enter the root name for the
c	  .  numbered files.  This is then the last entry.  IF you entered 0
c	  .  for a single file, then make the following additional entries:
c
c	  Name of output file for images
c
c	  Name of output file in which to place a list of piece coordinates in
c	  .  order to view the pieces in a square array, or Return for no
c	  .  file of such coordinates
c	  
c	  IF you specify a file name, next enter the amount of blank space to
c	  .  leave between adjacent pieces, in pixels
c	  
c	  Name of output file in which to place a list of the true, original
c	  .  coordinates of each piece, or Return for no such file
c
c	  David Mastronarde 4/23/90 - revised 1/19/93, for IMOD 4/24/97
c	  for separate files and all point output 10/26/99
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2004/03/15 23:54:47  mast
c	  Redimensioned to 256x256x257 and put big arrays in common
c	
c	  Revision 3.2  2002/09/09 21:36:00  mast
c	  Eliminate stat_source: and nimp_source: from all includes
c	
c	  Revision 3.1  2002/07/12 22:17:44  mast
c	  Revised man page to contain instructions for extracting into separate
c	  files; added checks on array limits, and converted STOP to
c	  call exit(1)
c	
c
	include 'model.inc'
	parameter (idim=256,izdim=257,limpcl=50000)
	real*4 array(idim,idim),brray(idim,idim,izdim)
     &	    ,avgray(idim,idim,izdim)
	integer*4 nxyz(3),mxyz(3),nxyz2(3),nxyzst(3),ind(3)
	equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
	logical exist,readw_or_imod
	character*80 modelfile
	real*4 delt(3),orig(3),cell(6),title(20),offset(3)
	integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl),
     &	    listz(limpcl),icolclip(256)
	character*80 filin,filpcl,filpcl2
	character*80 concat,convnum,rootname
	data nxyzst/0,0,0/
	character*9 dat
	character*8 tim
	common /bigarr/brray,avgray
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	character*6 chstrnd
	real*4 g(2,3,1000),gtmp(2,3)

	write(*,'(1x,a,$)')'Enter input image file name: '
	read(5,50)filin
50	format(A)
	CALL IMOPEN(1,filin,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
	call irtdel(1,delt)
	call irtorg(1,orig(1),orig(2),orig(3))
C	    
C	Open other files
C
	write(*,'(1x,a,$)')
     &	    'Piece list file if image is a montage, otherwise Return: '
	read(*,50)filpcl
	call read_piece_list(filpcl,ixpclist,iypclist,izpclist,npclist)
c	  
c	  if no pieces, set up mocklist
	if(npclist.eq.0)then
	  do i=1,nz
	    ixpclist(i)=0
	    iypclist(i)=0
	    izpclist(i)=i-1
	  enddo
	  npclist=nz
	endif
c	  make ordered list of z values 
	call fill_listz(izpclist,npclist,listz,nlistz)

	izrange=listz(nlistz)+1-listz(1)
	call checklist(ixpclist,npclist,1,nx,minxpiece
     &	    ,nxpieces,nxoverlap)
	call checklist(iypclist,npclist,1,ny,minypiece
     &	    ,nypieces,nyoverlap)
	xcen=minxpiece+(nx+(nxpieces-1)*(nx-nxoverlap))/2.
	ycen=minypiece+(ny+(nypieces-1)*(ny-nyoverlap))/2.
	maxxpiece=minxpiece+(nx+(nxpieces-1)*(nx-nxoverlap))-1
	maxypiece=minypiece+(ny+(nypieces-1)*(ny-nyoverlap))-1
c	  
10	write(*,'(1x,a,$)')'Name of input model file: '
	read(*,'(a)')modelfile
15	exist=readw_or_imod(modelfile)
	if(.not.exist)go to 10
c	  
	write(*,'(1x,a,$)')'0 to take coordinates as they are, 1 to'//
     &	    ' transform, -1 to back-transform: '
	read(5,*)ifxform
	if(ifxform.ne.0)then
	  write(*,'(1x,a,$)')'Name of file with transforms: '
	  read(5,50)filpcl
	  call dopen(4,filpcl,'old','f')
	  call xfrdall(4,g,nxg,*99)
	  if(ifxform.lt.0)then
	    do i=1,nxg
	      call xfinvert(g(1,1,i),gtmp)
	      call xfcopy(gtmp,g(1,1,i))
	    enddo
	  endif
	  xceng=xcen*delt(1)-xorig
	  yceng=ycen*delt(2)-yorig
	endif
c
	indxmin=minxpiece
	indxmax=maxxpiece
	indymin=minypiece
	indymax=maxypiece
	write(*,'(1x,a,/,a,$)')'Enter minimum and maximum X and Y'//
     &	    ' index coordinates within which',
     &	    '    ends should be contained, or / for no limits: '
	read(*,*)indxmin,indxmax,indymin,indymax
c	  
	indxmin=max(indxmin,minxpiece)
	indymin=max(indymin,minypiece)
	indxmax=min(indxmax,maxxpiece)
	indymax=min(indymax,maxypiece)
c
	indzmin=listz(1)
	indzmax=listz(nlistz)
	write(*,'(1x,a,/,a,$)')'Enter minimum and maximum'//
     &	    ' section numbers within which',
     &	    '    boxes should be contained, or / for no limits: '
	read(*,*)indzmin,indzmax
c	indzmin=max(indzmin,listz(1))
c	indzmax=min(indzmax,listz(nlistz))
c	  
	print *,'Enter list of numbers of objects whose ends should '//
     &	    'be clipped (Return for all)'
	call rdlist(5,icolclip,ncolclip)
c
	write(*,'(1x,a,$)')
     &	    'Clip out starts (0) or ends (1) or all points (-1): '
	read(*,*)ifstrtend
c
	write(*,'(1x,a,$)')'Box size in pixels: '
	read(*,*)npixbox
	npleft=(npixbox-1)/2
	npright=npixbox-npleft-1
	offset(1) = -0.5 * (1 + npright - npleft)
	offset(2) = offset(1)
	if (npixbox.gt.idim)then
	  print *,'BOXSTARTEND ERROR: BOX SIZE TOO LARGE FOR ARRAYS'
	  call exit(1)
	endif
c	  
	write(*,'(1x,a,$)')'# of sections before and # of sections'//
     &	    ' after endpoint to clip out: '
	read(*,*)nzbefore,nzafter
	nzclip=nzbefore+nzafter+1
	offset(3) = -0.495 * (1 + nzafter - nzbefore)
	if (nzclip.gt.izdim)then
	  print *,'BOXSTARTEND ERROR: MUMBER OF SECTIONS',
     &	      ' TOO LARGE FOR ARRAYS'
	  call exit(1)
	endif
	
c	  
	write(*,'(1x,a,$)')'1 to output numbered series of files,'//
     &	    ' 0 for single output file: '
	read(5,*)ifseries

	if(ifseries.ne.0)then
	  write(*,'(1x,a,$)')'Root name for output files: '
	  read(5,50)rootname
	  numfile=0
	  filpcl=' '
	  filpcl2=' '
c	    
c	    precount the points
c	    
	  npredict=0
	  do iobj=1,max_mod_obj
	    if (npt_in_obj(iobj).gt.0)then
	      ifnotcol=ncolclip
	      do icol=1,ncolclip
		if(icolclip(icol).eq.256-obj_color(2,iobj))ifnotcol=0
	      enddo
	      if(ifnotcol.eq.0)then
		if(ifstrtend.lt.0)then
		  npredict=npredict+npt_in_obj(iobj)
		else
		  npredict=npredict+1
		endif
	      endif
	    endif
	  enddo
	else
	  write(*,'(1x,a,$)')'Output image file name: '
	  read(5,50)filin
c	  
	  call imopen(2,filin,'new')
	  nxyz2(1)=npixbox
	  nxyz2(2)=npixbox
	  nxyz2(3)=1
	  call itrhdr(2,1)
	  call ialsiz(2,nxyz2,nxyzst)
c	  
	  write(*,'(1x,a,$)') 'Output file for 2D array piece '//
     &	      'list, or Return for none: '
	  read(5,50)filpcl
c	    
	  if(filpcl.ne.' ')then
	    write(*,'(1x,a,$)')'Number of empty pixels between clips: '
	    read(*,*)ngutter
	    call dopen(3,filpcl,'new','f')
	  endif
c	  
	  print *, 'Output file for real coordinate piece list,'//
     &	      ' or Return for none: '
	  read(5,50)filpcl2
	  if(filpcl2.ne.' ')call dopen(4,filpcl2,'new','f')
	endif
c	  
c	  set up for loop on model objects
c
	nzout=0
	nclip=0
	dsum=0.
	tmin=1.e10
	tmax=-1.e10
	do iobj=1,max_mod_obj
	  if (npt_in_obj(iobj).gt.0)then
	    ifnotcol=ncolclip
	    do icol=1,ncolclip
	      if(icolclip(icol).eq.256-obj_color(2,iobj))ifnotcol=0
	    enddo
	    if(ifnotcol.eq.0)then
	      loopst = 1
	      loopnd = npt_in_obj(iobj)
	      if(ifstrtend.eq.0)then
		loopnd = 1
	      elseif(ifstrtend.gt.0)then
		loopst = loopnd
	      endif

	      do loop=loopst,loopnd
		ipnt=abs(object(loop+ibase_obj(iobj)))
c		
c		  convert to index coords
c		
		do i=1,3
		  ind(i)=nint((p_coord(i,ipnt)+orig(i))/delt(i) + offset(i))
		enddo
c		
c		  is it inside limits?
c		
		indleft=ind(1)-npleft
		indright=ind(1)+npright
		indbot=ind(2)-npleft
		indtop=ind(2)+npright
		indzlo=ind(3)-nzbefore
		indzhi=ind(3)+nzafter
		if(ind(1).ge.indxmin .and. ind(1).le.indxmax .and.
     &		    ind(2).ge.indymin .and. ind(2).le.indymax .and.
     &		    indzlo.ge.indzmin .and. indzhi.le.indzmax)then
c		  
c		    loop on sections
c		  
		  do indz=indzlo,indzhi
		    if(ifxform.ne.0)then
		      indg=0
		      do ilist=1,nlistz
			if(listz(ilist).eq.indz)indg=ilist
		      enddo
		      if(indg.ne.0)then
			call xfapply(g(1,1,indg),xceng,yceng,p_coord(1,ipnt),
     &			    p_coord(2,ipnt),xnew,ynew)
			ind(1)=nint((xnew+orig(1))/delt(1))
			ind(2)=nint((ynew+orig(2))/delt(2))
			indleft=ind(1)-npleft
			indright=ind(1)+npright
			indbot=ind(2)-npleft
			indtop=ind(2)+npright
		      endif
		    endif
		    ifany=0
		    indar=indz+1-indzlo
c		      
c		      zero out the box
c		      
		    do iy=1,npixbox
		      do ix=1,npixbox
			brray(ix,iy,indar)=0.
		      enddo
		    enddo
c		      
c		      loop on pieces, find intersection with each piece
c		      
		    do ipc=1,npclist
		      if(izpclist(ipc).eq.indz)then
			ixpc=ixpclist(ipc)
			iypc=iypclist(ipc)
			ipxstr=max(indleft,ixpc)
			ipystr=max(indbot,iypc)
			ipxend=min(indright,ixpc+nx-1)
			ipyend=min(indtop,iypc+ny-1)
			if(ipxstr.le.ipxend.and.ipystr.le.ipyend)then
c			    
c			    if it intersects, read in intersecting part, move it
c			    into appropriate part of array
c			    
			  call imposn(1,ipc-1,0)
			  call irdpas(1,array,idim,idim,ipxstr-ixpc,
     &			      ipxend-ixpc,ipystr-iypc,ipyend-iypc,*99)
			  ifany=1
			  do iy=1,ipyend+1-ipystr
			    do ix=1,ipxend+1-ipxstr
			      brray(ix+ipxstr-indleft,iy+ipystr-indbot,
     &				  indar)=array(ix,iy)
			    enddo
			  enddo
			endif
		      endif
		    enddo
c		    
c		    if no piece intersected, abort this end point
c		    NO - CHANGE SO THAT BLANK PIECES WILL BE MADE
c		    if(ifany.eq.0)go to 60
c		    
		    if(filpcl2.ne.' ')write(4,'(3i6)')indleft,indbot,indz
		  enddo
c		  
c		    all sections were found, so write each piece, add to average
c
		  if(ifseries.ne.0)then
		    numfile=numfile+1
		    if(npredict.lt.100)then
		      write(convnum,'(i2.2)')numfile
		    elseif(npredict.lt.1000)then
		      write(convnum,'(i3.3)')numfile
		    elseif(npredict.lt.10000)then
		      write(convnum,'(i4.4)')numfile
		    else
		      write(convnum,'(i5.5)')numfile
		    endif
		    filin=concat(concat(rootname,'.'),convnum)
		    call imopen(2,filin,'new')
		    nxyz2(1)=npixbox
		    nxyz2(2)=npixbox
		    nxyz2(3)=nzclip
		    call itrhdr(2,1)
		    call ialsiz(2,nxyz2,nxyzst)
		    dsum=0.
		    tmin=1.e10
		    tmax=-1.e10
		  endif
		  do indar=1,nzclip
		    do iy=1,npixbox
		      do ix=1,npixbox
			avgray(ix,iy,indar)=avgray(ix,iy,indar)+
     &			    brray(ix,iy,indar)
		      enddo
		    enddo
		    call iclden(brray(1,1,indar),idim,idim,1,npixbox,1,
     &			npixbox,tmin,tmax,tmean)
		    dmin2=min(dmin2,tmin)
		    dmax2=max(dmax2,tmax)
		    dsum=dsum+tmean
		    call irepak(array,brray(1,1,indar),idim,idim,0,
     &			npixbox-1,0,npixbox-1)
		    call iwrsec(2,array)
		    nzout=nzout+1
		  enddo
		  if(ifseries.ne.0)then
		    dmean2=dsum/nzclip
		    do i=1,3
		      cell(i)=nxyz2(i)
		      cell(i+3)=90.
		    enddo
		    call ialsiz(2,nxyz2,nxyzst)
		    call ialsam(2,nxyz2)
		    call ialcel(2,cell)
		    
		    call date(dat)
		    call time(tim)
c
c 7/7/00 CER: remove the encodes
c 
C                   encode(80,111,title)dat,tim
                    write(titlech,111) dat,tim
                    read(titlech,'(20a4)')(title(kti),kti=1,20)
111		    format('BOXSTARTEND: Individual box clipped out'
     &			,t57,a9,2x,a8)
		    call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
		    call imclose(2)
		  endif
		  nclip=nclip+1
		endif
60		continue
	      enddo
	    endif
	  endif
	enddo
c	  
c	  take care of averages
c
	if(ifseries.eq.0)then
	  do indar=1,nzclip
	    do iy=1,npixbox
	      do ix=1,npixbox
		avgray(ix,iy,indar)=avgray(ix,iy,indar)/nclip
	      enddo
	    enddo
	    call iclden(avgray(1,1,indar),idim,idim,1,npixbox,1,
     &		npixbox,tmin,tmax,tmean)
	    dmin2=min(dmin2,tmin)
	    dmax2=max(dmax2,tmax)
	    dsum=dsum+tmean
	    call irepak(array,avgray(1,1,indar),idim,idim,0,
     &		npixbox-1,0,npixbox-1)
	    call iwrsec(2,array)
	    nzout=nzout+1
	  enddo
	  dmean2=dsum/nzout
	  nxyz2(3)=nzout
	  do i=1,3
	    cell(i)=nxyz2(i)
	    cell(i+3)=90.
	  enddo
	  call ialsiz(2,nxyz2,nxyzst)
	  call ialsam(2,nxyz2)
	  call ialcel(2,cell)
	  
	  call date(dat)
	  call time(tim)
	  chstrnd='starts'
	  if(ifstrtend.gt.0)chstrnd='ends  '
	  if(ifstrtend.lt.0)chstrnd='points'
c
c 7/7/00 CER: remove the encodes
c
c         encode(80,101,title)nclip,chstrnd,dat,tim
	  write(titlech,101) nclip,chstrnd,dat,tim
	  read(titlech,'(20a4)')(title(kti),kti=1,20) 
101	  format('BOXSTARTEND: ',i4,1x,a6,' clipped out and averaged'
     &	      ,t57,a9,2x,a8)
	  call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
	  call imclose(2)
	  nclip=nclip+1
	endif
c	  
c	  now put out piece list if desired
c
	if(filpcl.ne.' ')then
	  newypiece=nint(sqrt(float(nclip)))
	  newxpiece=(nclip+newypiece-1)/newypiece
	  indx=0
	  indy=0
	  do i=1,nclip
	    ixpc=indx*(npixbox+ngutter)
	    iypc=indy*(npixbox+ngutter)
	    do iz=0,nzclip-1
	      write(3,'(3i6)')ixpc,iypc,iz
	    enddo
	    indx=indx+1
	    if(indx.ge.newxpiece)then
	      indx=0
	      indy=indy+1
	    endif
	  enddo
	  close(3)    
	endif
	call exit(0)
99	print *,'BOXSTARTEND: ERROR READING FILE'
	call exit(1)
	end

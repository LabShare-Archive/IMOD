* * * * * MATCHVOL * * * * *
c	  
c	  MATCHVOL will transform a volume using a general linear
c	  transformation.  Its main use is to transform one tomogram from
c	  a two-axis tilt series so that it matches the other tomogram.
c	  To do so, it can combine an initial alignment transformation and
c	  any number of successive refining transformations.  The program
c	  uses the same algorithm as ROTATEVOL for rotating large volumes.
c
c	  See man page for more information
c
c	  David Mastronarde, 1995
c	  DNM 2/26/01: add temporary directory entry and semi-unique filenames
c	  DNM 11/6/01: fixed problem with output array size not being respected
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2003/10/02 19:58:27  mast
c	  Changed method of writing sections to avoid having to fit output
c	  section into array all at once.
c	  Increased array size and put big array in common
c	
c	  Revision 3.2  2003/03/14 23:43:50  mast
c	  Replace stop statement with standard error output
c	
c	  Revision 3.1  2002/07/31 23:55:45  mast
c	  Made it preserve pixel size
c	
c
	implicit none
	include 'rotmatwarp.inc'
	real*4 cell(12),dxyzin(3)
	real*4 mxyzin(3),cenind(3)
	real*4 mfor(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
	real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3)
	real*4 atmp1(3,3),atmp2(3,3),dtmp1(3),dtmp2(3),delta(3)
c
	character*120 filein,fileout,tempdir,tempext
c
c	DNM 3/8/01: initialize the time in case time(tim) doesn't work
c
	character dat*9,tim*8/'00:00:00'/
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	integer*4 i, ix, iy, kti, interpOrder, ierr, numToGet, numXFiles
	integer*4 numXLines, j, ntrans, itrans
	real*4 dminin, dmaxin, devmx, xcen, ycen, zcen
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger, PipGetFloatArray, PipNumberOfEntries
	integer*4 PipGetString,PipGetThreeIntegers,PipGetThreeFloats
	integer*4 PipGetNonOptionArg, PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  matchvol
c
	integer numOptions
	parameter (numOptions = 10)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'inverse:InverseFile:FN:@tempdir:TemporaryDirectory:CH:@'//
     &      'size:OutputSizeXYZ:IT:@center:CenterXYZ:FT:@'//
     &      'xffile:TransformFile:FN:@3dxform:3DTransform:FN:@'//
     &      'order:InterpolationOrder:I:@help:usage:B:'
c	  
c	  set defaults here
c	  
	interpOrder = 2
	tempdir = ' '
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'matchvol',
     &	    'ERROR: MATCHVOL - ', .true., 3, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

	if (PipGetInOutFile('InputFile', 1, 'Name of input file', filein)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')

	call imopen(5,filein,'RO')
	call irdhdr(5,nxyzin,mxyzin,mode,dminin,dmaxin,dmeanin)
c
	do i=1,3
	  cenind(i)=nxyzin(i)/2
	  nxyzout(i) = nxyzin(i)
	enddo
c
	if (PipGetInOutFile('OutputFile', 2, 'Name of output file', fileout)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
c
	call imopen(6,fileout,'NEW')

	if (pipinput) then
	  ierr = PipGetString('TemporaryDirectory', tempdir)
	  ierr = PipGetThreeFloats('CenterXYZ', cenind(1),
     &	      cenind(2), cenind(3))
	  ierr = PipGetInteger('InterpolationOrder', interpOrder)
	  ierr = PipGetThreeIntegers('OutputSizeXYZ', nxout, nyout, nzout)
c	    
c	    transforms
c
	  ierr = PipNumberOfEntries('TransformFile', numXFiles)
	  ierr = PipNumberOfEntries('3DTransform', numXLines)
	  ntrans = numXFiles + numXLines
	  if (ntrans .eq. 0) call errorexit(
     &	      'NO TRANSFORMS SPECIFIED')
	  call PipAllowCommaDefaults(0)
	  do itrans=1,ntrans
	    if (itrans .le.numXFiles) then
c		
c		read the files in turn
c
	      ierr = PipGetString('TransformFile', fileout)
	      call dopen(1,fileout,'ro','f')
	      read(1,*)((mfor(i,j),j=1,3),dxyzin(i),i=1,3)
	      close(1)
	    else
c		
c		Then read the in-line transforms in turn
c
	      numToGet = 12
	      ierr = PipGetFloatArray('3DTransform', cell, numToGet, 12)
	      do i = 1, 3
		do j = 1, 3
		  mfor(i,j) = cell(j + 4 * (i - 1))
		enddo
		dxyzin(i) = cell(4 * i)
	      enddo
	    endif
	    if(itrans.gt.1)then
	      call xfcopy3d(mfor,dxyzin,atmp2,dtmp2)
	      call xfmult3d(atmp1,dtmp1,atmp2,dtmp2,mfor,dxyzin)
	    endif
	    call xfcopy3d(mfor,dxyzin,atmp1,dtmp1)
	  enddo
	else
c	    
c	    interactive input: transforms can alternate between coming
c	    from file or from input arbitrarily
c
	  write(*,'(1x,a,/,a,$)')'Enter path name of directory for '//
     &	      'temporary files, ',' or Return to use current directory: '
	  read(5,'(a)')tempdir
c	    
	  write(*,'(1x,a,3i5,a,$)')'X, Y, and Z dimensions of the '//
     &	      'output file (/ for',nxout,nyout,nzout,'): '
	  read(5,*)nxout,nyout,nzout
c	  
	  write(*,'(1x,a,$)')
     &	      'Number of successive transformations to apply: '
	  read(5,*)ntrans
	  do itrans=1,ntrans
	    print *,'For transformation matrix #',itrans,
     &		', either enter the name of a file',
     &		' with the transformation, or Return to enter the',
     &		' transformation directly'
	    read(5,'(a)')fileout
	    if(fileout.eq.' ')then
	      print *,'Enter transformation matrix #',itrans
	      read(5,*)((mfor(i,j),j=1,3),dxyzin(i),i=1,3)
	    else
	      call dopen(1,fileout,'ro','f')
	      read(1,*)((mfor(i,j),j=1,3),dxyzin(i),i=1,3)
	      close(1)
	    endif
	    if(itrans.gt.1)then
	      call xfcopy3d(mfor,dxyzin,atmp2,dtmp2)
	      call xfmult3d(atmp1,dtmp1,atmp2,dtmp2,mfor,dxyzin)
	    endif
	    call xfcopy3d(mfor,dxyzin,atmp1,dtmp1)
	  enddo
	endif
c
	print *,'Forward matrix:'
	write(*,102)((mfor(i,j),j=1,3),dxyzin(i),i=1,3)
102	format(3f10.6,f10.3)
c
c	  get matrix for inverse transform
c	  
	call xfinv3d(mfor,dxyzin,minv,cxyzin)
	print *,'Inverse matrix:'
	write(*,102)((minv(i,j),j=1,3),cxyzin(i),i=1,3)

	if (pipinput) then
	  fileout = ' '
	  ierr = PipGetString('InverseFile', fileout)
	else
	  print *,'Enter name of file to place inverse transformation in,'
     &	      ,' or Return for none'
	  read(5,'(a)')fileout
	endif
	if(fileout.ne.' ')then
	  call dopen(1,fileout,'new','f')
	  write(1,102)((minv(i,j),j=1,3),cxyzin(i),i=1,3)
	  close(1)
	endif
	call PipDone()
c	  
c	  DNM 7/26/02: transfer pixel spacing to same axes; could be weird
c	  if spacings are not isotropic
c
	call irtdel(5,delta)
	do i=1,3
	  cell(i)=nxyzout(i)*delta(i)
	  cell(i+3)=90.
c	    cxyzin(i)=cxyzin(i)+nxyzin(i)/2.
	  cxyzin(i)=cxyzin(i)+cenind(i)
	  cxyzout(i)=nxyzout(i)/2.
	enddo
c	  
	call icrhdr(6,nxyzout,nxyzout,mode,title,0)
	call ialcel(6,cell)
	call itrlab(6,5)
	call time(tim)
	call date(dat)
	tempext='mat      1'
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,302,TITLE)dat,tim
        write(titlech,302) dat,tim
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
302     FORMAT('MATCHVOL: 3-D transformation of tomogram:',t57,a9,2x,a8)
	call irttlt(5,tiltold)
	call ialtlt(6,tiltold)
c	  
c	  find maximum extent in input volume occupied by a back-transformed
c	  unit cube in output volume
c
	devmx=0.
	do ix=-1,1,2
	  do iy=-1,1,2
	    do i=1,3
	      devmx=max(devmx,abs(minv(i,1)*ix+minv(i,2)*iy+minv(i,3)))
	    enddo
	  enddo
	enddo

	call setup_cubes_scratch(devmx, filein, tempdir, tempext, tim)
	call transform_cubes(interpOrder)
	end


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: MATCHVOL - ',message
	call exit(1)
	end


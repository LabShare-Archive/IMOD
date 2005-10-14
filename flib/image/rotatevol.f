* * * * * ROTATEVOL * * * * *
c	  
c	  ROTATEVOL will rotate all or part of a three-dimension volume
c	  of data.  The rotations may be by any angles about the three axes.
c	  Tilt angles and origin information in the header are properly
c	  maintained so that the new data stack will have a coordinate system
c	  congruent with the old one.
c	  
c	  The program can work on an arbitrarily large volume.  It reconstructs
c	  a series of sub-cubes of the output volume (each cube is currently
c	  256x256x256).  For each cube, it reads into memory a cube from the
c	  input volume that contains all of the image area that rotates into
c	  that cube of output volume.  It then uses triquadratic interpolation
c	  to find each pixel of the output cube, and writes the cube to a
c	  scratch file.  When all of the cubes in one layer are done, it reads
c	  back data from the scratch files and assembles each section in that
c	  layer.
c	  
c	  See man page for more details
c
c	  David Mastronarde 7/25/91
c	  1995: switch to tri-quadratic interpolation, allow real center coords
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
c	  Revision 3.6  2004/11/10 02:06:27  mast
c	  Added argument to call to setup cubes
c	
c	  Revision 3.5  2004/10/29 20:00:07  mast
c	  Added and allowed defaults for output size and angles
c	
c	  Revision 3.4  2003/10/10 20:38:04  mast
c	  Changed to use subroutines in rotmatwarpsubs.f and include file.
c	  Converted to PIP/autodoc input and added linear interpolation option.
c	
c	  Revision 3.3  2003/10/02 19:41:21  mast
c	  Changed method of writing sections to avoid having to fit output
c	  section into array all at once.
c	  Increased array size and put big array in common
c	
c	  Revision 3.2  2003/03/15 01:04:15  mast
c	  Standardize error output
c	
	implicit none
	include 'rotmatwarp.inc'
	real*4 cell(6)
	integer*4 mxyzin(3)
	real*4 cenind(3)
	real*4 mfor(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
	real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3),delta(3)
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
	integer*4 i, ix, iy, kti, interpOrder, ierr
	real*4 dminin, dmaxin, devmx, xcen, ycen, zcen
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger
	integer*4 PipGetString,PipGetThreeIntegers,PipGetThreeFloats
	integer*4 PipGetNonOptionArg, PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  rotatevol
c
	integer numOptions
	parameter (numOptions = 8)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'tempdir:TemporaryDirectory:CH:@size:OutputSizeXYZ:IT:@'//
     &      'center:RotationCenterXYZ:FT:@'//
     &      'angles:RotationAnglesZYX:FT:@'//
     &      'order:InterpolationOrder:I:@help:usage:B:'
c	  
c	  set defaults here
c	  
	interpOrder = 2
	tempdir = ' '
c
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'rotatevol',
     &	    'ERROR: ROTATEVOL - ', .true., 3, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

	if (PipGetInOutFile('InputFile', 1, 'Name of input file', filein)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')

	call imopen(5,filein,'RO')
	call irdhdr(5,nxyzin,mxyzin,mode,dminin,dmaxin,dmeanin)
	do i=1,3
	  cenind(i)=nxyzin(i)/2
	  nxyzout(i) = nxyzin(i)
	  angles(i) = 0.
	enddo
c
	if (PipGetInOutFile('OutputFile', 2, 'Name of output file', fileout)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')

	if (pipinput) then
	  ierr = PipGetString('TemporaryDirectory', tempdir)
	  ierr = PipGetThreeFloats('RotationCenterXYZ', cenind(1),
     &	      cenind(2), cenind(3))
	  ierr = PipGetInteger('InterpolationOrder', interpOrder)
	  ierr = PipGetThreeIntegers('OutputSizeXYZ', nxout, nyout, nzout)
	  ierr = PipGetThreeFloats('RotationAnglesZYX', angles(3), angles(2),
     &	      angles(1))
	else
c	    
	  write(*,'(1x,a,/,a,$)')'Enter path name of directory for '//
     &	      'temporary files, ',' or Return to use current directory: '
	  read(5,'(a)')tempdir
c	    
	  write(*,'(1x,a,$)')'X, Y, and Z dimensions of the output file: '
	  read(5,*)nxout,nyout,nzout
c	    
	  write(*,'(1x,a,/,a,$)')'Enter X, Y, and Z index coordinates of '
     &	      //'the center of rotation',
     &	      '   in the input file (/ for center of file): '
	  read(5,*)(cenind(i),i=1,3)
	  print *,'Rotations are applied in the order that you',
     &	      ' enter them: rotation about the','   Z axis, then ',
     &	      'rotation about the Y axis, then rotation about the X axis'
	  write(*,'(1x,a,$)')
     &	      'Rotations about Z, Y, and X axes (gamma, beta, alpha): '
	  read(5,*)angles(3),angles(2),angles(1)
	endif
c
	call PipDone()
	call imopen(6,fileout,'NEW')
c	  
c	  get true centers of index coordinate systems
c
	call irtdel(5,delta)
	do i=1,3
	  cxyzin(i)=(nxyzin(i)-1)/2.+cenind(i)-nxyzin(i)/2
	  cxyzout(i)=(nxyzout(i)-1)/2.
	  cell(i)=nxyzout(i)*delta(i)
	  cell(i+3)=90.
	enddo
c
c	  
c	  get matrices for forward and inverse rotations
c	  
	call icalc_matrix(angles,mfor)
	call inv_matrix(mfor,minv)
c	  
	call icrhdr(6,nxyzout,nxyzout,mode,title,0)
	call ialcel(6,cell)
	call itrlab(6,5)
	call time(tim)
	call date(dat)
	tempext='rot      1'
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,302,TITLE)(angles(i),i=1,3),dat,tim
        write(titlech,302) (angles(i),i=1,3),dat,tim
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
302	FORMAT('ROTATEVOL: 3D rotation by angles:',3f7.1,t57,a9,2x,a8)
c	  
c	  calculate new tilt angles and origin information from old
c	  
	call irttlt(5,tiltold)
	call icalc_matrix(tiltold,mold)
	call inv_matrix(mold,moldinv)
	call ialtlt_orig(6,tiltold)
	call ialtlt(6,tiltold)
	call ialtlt_rot(6,angles)
	call irttlt(6,tiltnew)
	call icalc_matrix(tiltnew,mnew)
	call irtorg(5,orig(1),orig(2),orig(3))
	xcen=cxin-orig(1)/delta(1)
	ycen=cyin-orig(2)/delta(2)
	zcen=czin-orig(3)/delta(3)
	do i=1,3
	  xtmp(i)=moldinv(i,1)*xcen+moldinv(i,2)*ycen+moldinv(i,3)*zcen
	enddo
	do i=1,3
	  orig(i)=delta(i) * (cxyzout(i)-
     &	      (mnew(i,1)*xtmp(1)+mnew(i,2)*xtmp(2)+mnew(i,3)*xtmp(3)))
	enddo
	call ialorg(6,orig(1),orig(2),orig(3))
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
c
	call setup_cubes_scratch(devmx, 0, filein, tempdir, tempext, tim)
	call transform_cubes(interpOrder)

	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: ROTATEVOL - ',message
	call exit(1)
	end


*       * * * * ROTATEVOL * * * * *
c       
c       ROTATEVOL will rotate all or part of a three-dimension volume
c       of data.  The rotations may be by any angles about the three axes.
c       Tilt angles and origin information in the header are properly
c       maintained so that the new data stack will have a coordinate system
c       congruent with the old one.
c       
c       The program can work on an arbitrarily large volume.  It reconstructs
c       a series of sub-regions of the output volume, referred to as cubes
c       but actually rectangles. For each cube, it reads into memory a cube
c       from the input volume that contains all of the image area that rotates
c       into that cube of output volume.  It then uses linear or triquadrati
c       interpolation to find each pixel of the output cube, and writes the
c       cube to a scratch file.  When all of the cubes in one layer are done,
c       it reads back data from the scratch files and assembles each section
c       in that layer.
c       
c       See man page for more details
c       
c       $Id$
c       
      use rotmatwarp
      implicit none
      real*4 cell(6)
      integer*4 mxyzin(3),maxdim(3)
      real*4 cenind(3)
      real*4 mfor(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
      real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3),delta(3)
c       
      character*320 filein,fileout,tempdir,tempext
c       
c	DNM 3/8/01: initialize the time in case time(tim) doesn't work
c       
      character dat*9,tim*8/'00:00:00'/
      character*80 titlech
      integer*4 i, ix, iy, kti, interpOrder, ierr, idiry, idirz
      real*4 dminin, dmaxin, devmx, xcen, ycen, zcen
      logical*4 query, backRotate
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetFloat
      integer*4 PipGetString,PipGetThreeIntegers,PipGetThreeFloats
      integer*4 PipGetNonOptionArg, PipGetInOutFile, PipGetLogical
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  rotatevol
c       
      integer numOptions
      parameter (numOptions = 14)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@tempdir:TemporaryDirectory:CH:@'//
     &    'size:OutputSizeXYZ:IT:@center:RotationCenterXYZ:FT:@'//
     &    'angles:RotationAnglesZYX:FT:@back:BackRotate:B:@order:InterpolationOrder:I:@'//
     &    'query:QuerySizeNeeded:B:@fill:FillValue:F:@memory:MemoryLimit:I:@'//
     &    'verbose:VerboseOutput:I:@param:ParameterFile:PF:@help:usage:B:'
c       
c       set defaults here
c       
      interpOrder = 2
      tempdir = ' '
      query = .false.
      backRotate = .false.
c       
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'rotatevol',
     &    'ERROR: ROTATEVOL - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile', 1, 'Name of input file', filein)
     &    .ne. 0) call exiterror('NO INPUT FILE SPECIFIED')

      if (pipinput) then
        ierr = PipGetLogical('QuerySizeNeeded', query)
        if (query) call ialprt(.false.)
      endif

      call imopen(5,filein,'RO')
      call irdhdr(5,nxyzin,mxyzin,mode,dminin,dmaxin,dmeanin)
      do i=1,3
        cenind(i)=nxyzin(i)/2
        nxyzout(i) = nxyzin(i)
        angles(i) = 0.
      enddo
c       
      if (.not.query .and. PipGetInOutFile('OutputFile', 2, 
     &    'Name of output file', fileout) .ne. 0)
     &    call exiterror('NO OUTPUT FILE SPECIFIED')

      if (pipinput) then
        ierr = PipGetString('TemporaryDirectory', tempdir)
        ierr = PipGetThreeFloats('RotationCenterXYZ', cenind(1),
     &      cenind(2), cenind(3))
        ierr = PipGetInteger('InterpolationOrder', interpOrder)
        ierr = PipGetInteger('MemoryLimit', memoryLim)
        ierr = PipGetInteger('VerboseOutput', iVerbose)
        ierr = PipGetThreeIntegers('OutputSizeXYZ', nxout, nyout, nzout)
        ierr = PipGetThreeFloats('RotationAnglesZYX', angles(3), angles(2),
     &      angles(1))
        ierr = PipGetFloat('FillValue', dmeanin)
        ierr = PipGetLogical('BackRotate', backRotate)
      else
c         
        write(*,'(1x,a,/,a,$)')'Enter path name of directory for '//
     &      'temporary files, ',' or Return to use current directory: '
        read(5,'(a)')tempdir
c         
        write(*,'(1x,a,$)')'X, Y, and Z dimensions of the output file: '
        read(5,*)nxout,nyout,nzout
c         
        write(*,'(1x,a,/,a,$)')'Enter X, Y, and Z index coordinates of '
     &      //'the center of rotation',
     &      '   in the input file (/ for center of file): '
        read(5,*)(cenind(i),i=1,3)
        print *,'Rotations are applied in the order that you',
     &      ' enter them: rotation about the','   Z axis, then ',
     &      'rotation about the Y axis, then rotation about the X axis'
        write(*,'(1x,a,$)')
     &      'Rotations about Z, Y, and X axes (gamma, beta, alpha): '
        read(5,*)angles(3),angles(2),angles(1)
      endif
c       
      call PipDone()
      if (nxout < 1 .or. nyout < 1 .or. nzout < 1) call exitError(
     &    'A POSITIVE OUTPUT SIZE MUST BE ENTERED ON ALL AXES')
c       
c       get matrices for forward and inverse rotations
c       
      if (backRotate) then
        call icalc_matrix(angles,minv)
        call inv_matrix(minv,mfor)
        call icalc_angles(angles, mfor)
      else
        call icalc_matrix(angles,mfor)
        call inv_matrix(mfor,minv)
      endif
c       
c       Compute maximum dimensions required if requested
c       
      if (query) then
        do i = 1, 3
          maxdim(i) = 0
        enddo
        do idiry = -1,1,2
          do idirz = -1,1,2
            do i = 1, 3
              maxdim(i) = max(maxdim(i), nint(abs(mfor(i, 1) * nxin +
     &            idiry * mfor(i, 2) * nyin + idirz * mfor(i, 3) * nzin)))
            enddo
          enddo
        enddo
        write(*,'(3i8)')(maxdim(i), i=1,3)
        call exit(0)
      endif
c       
      call imopen(6,fileout,'NEW')
c       
c       get true centers of index coordinate systems
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
      call icrhdr(6,nxyzout,nxyzout,mode,title,0)
      call ialcel(6,cell)
      call itrlab(6,5)
      call time(tim)
      call date(dat)
      tempext='rot      1'
c       
      write(titlech,302) (angles(i),i=1,3),dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
302   FORMAT('ROTATEVOL: 3D rotation by angles:',3f7.1,t57,a9,2x,a8)
c       
c       calculate new tilt angles and origin information from old
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
c       
c       Need to add 0.5 back to all these center coordinates to get
c       it right for actual 90 degree rotation
c
      xcen=cxin+0.5-orig(1)/delta(1)
      ycen=cyin+0.5-orig(2)/delta(2)
      zcen=czin+0.5-orig(3)/delta(3)
      do i=1,3
        xtmp(i)=moldinv(i,1)*xcen+moldinv(i,2)*ycen+moldinv(i,3)*zcen
      enddo
      do i=1,3
        orig(i)=delta(i) * (cxyzout(i)+0.5-
     &      (mnew(i,1)*xtmp(1)+mnew(i,2)*xtmp(2)+mnew(i,3)*xtmp(3)))
      enddo
      call ialorg(6,orig(1),orig(2),orig(3))
c       
c       Set up the arrangement of input and output data
      call setup_cubes_scratch(mfor, minv, 1, 0, filein, tempdir, tempext, tim,
     &    .false.)
c       
c       Do all the work and exit
      call transform_cubes(interpOrder)
      end


c       !
c       GET_TILT_ANGLES will read in tilt angles by the user's method of
c       choice.  
c       ^ [nview] should be the number of views, if known, or 0 if it
c       not known, in which case the routine will return the number in this
c       variable.  
c       ^ [nunit] should contain the number of a free logical unit.
c       ^ Tilt angles are returned in [tilt].
c       ^ [limtilt] should contain the dimensions of [tilt].
c       ^ [ifpip] should be set to 0 for interactive input, or nonzero for
c       input via PIP.  In the later case, the program must define the
c       options FirstTiltAngle, TiltIncrement, TiltFile, and TiltAngles.
c       If a TiltFile is entered, or if TiltAngles are entered, they 
c       supercede entries of FirstTiltAngle and TiltIncrement.
c       !       
c       $Id$
c       $Log$
c       Revision 3.4  2009/08/24 22:10:25  mast
c       Upgrade filename length
c
c       Revision 3.3  2006/01/26 15:11:43  mast
c       Added function to read from file
c
c       Revision 3.2  2003/10/24 03:35:42  mast
c       fix problem with reading set of angles with PIP
c	
c       Revision 3.1  2003/06/21 00:45:49  mast
c       New version to use PIP as an option
c       
      subroutine get_tilt_angles(nview,nunit,tilt, limtilt, ifpip)
      implicit none
      integer*4 nview, nunit, ifpip, limtilt
      real*4 tilt(*)
      character*320 filename
      integer*4 nvin, i, ierr, ierr2, numLines, index, ninLine
      real*4 tiltstr, tiltinc
      logical startIncOK
      integer*4 PipGetFloat,PipGetString, PipNumberOfEntries
      integer*4 PipGetFloatArray
c       
      if (ifpip .ne. 0) then
        ierr = PipGetFloat('FirstTiltAngle', tiltstr)
        ierr2 = PipGetFloat('TiltIncrement', tiltinc)
        startIncOK = ierr .eq. 0 .and. ierr2 .eq. 0

        ierr = PipGetString('TiltFile', filename)
        ierr2 = PipNumberOfEntries('TiltAngles', numLines)

        if (ierr .gt. 0 .and. numLines .eq. 0) then
          if (.not. startIncOK) call gta_errorexit('NO TILT ANGLES'//
     &        ' SPECIFIED BY START AND INCREMENT, FILE, OR '//
     &        'INDIVIDUAL VALUES')
          do i=1,nview
            tilt(i)=tiltstr+(i-1)*tiltinc
          enddo
          return
        endif

        if (numLines .gt. 0) then
          if (ierr .eq. 0) call gta_errorexit('YOU CANNOT SPECIFY '//
     &        'BOTH A TILT ANGLE FILE AND INDIVIDUAL ENTRIES')
          index = 0
          do i = 1, numLines
            ninLine = 0
            ierr = PipGetFloatArray('TiltAngles', tilt(index + 1), ninLine,
     &          limtilt - index)
            index = index + ninLine
          enddo
          if (nview .eq. 0) nview = index
          if (index .ne. nview) then 
            print *
            print *,'ERROR: GET_TILT_ANGLES -',nview,
     &          ' ANGLES EXPECTED BUT ONLY', index,' ENTERED'
            call exit(1)
          endif
          return
        endif
      else
        if(nview.eq.0)then
          write(*,'(1x,a,/,a,/,a,$)')'Enter the # of tilt angles to '//
     &        'specify them by starting and increment angle,',
     &        '     - the # of angles to specify each individual value,'
     &        ,'    or 0 to read angles from a file: '
          read(5,*)nvin
          nview = abs(nvin)
        else
          write(*,'(1x,a,/,a,/,a,$)')'Enter 1 to '//
     &        'specify starting and increment angle,',
     &        '     -1 to specify each individual value,'
     &        ,'    or 0 to read angles from a file: '
          read(5,*)nvin
        endif
        if (nview .gt. limtilt) call gta_errorexit(
     &      'TOO MANY VIEWS FOR TILT ANGLE ARRAY')
        if(nvin.gt.0)then
          write(*,'(1x,a,$)')'Starting and increment angle: '
          read(5,*)tiltstr,tiltinc
          do i=1,nview
            tilt(i)=tiltstr+(i-1)*tiltinc
          enddo
          return
        elseif(nvin.lt.0)then
          print *,'Enter all tilt angles'
          read(5,*,err=40,end=30)(tilt(i),i=1,nview)
          return
        else
          write(*,'(1x,a,$)')'Name of file with tilt angles: '
          read(5,'(a)')filename
        endif
      endif

      call read_tilt_file(nview, nunit, filename, tilt, limtilt)
      return
30    call gta_errorexit('end of file or input reached reading tilt angles')
40    call gta_errorexit('error reading tilt angles')
      end


c       !
c       READ_TILT_FILE will read in tilt angles from a file.
c       ^ [nview] should be the number of views, if known, or 0 if it
c       not known, in which case the routine will return the number in this
c       variable.  
c       ^ [nunit] should contain the number of a free logical unit.
c       ^ [filename] should contain the filename.
c       ^ Tilt angles are returned in [tilt].
c       ^ [limtilt] should contain the dimensions of [tilt].
c       !       
      subroutine read_tilt_file(nview, nunit, filename, tilt, limtilt)
      implicit none
      integer*4 nview, nunit, limtilt, i
      real*4 tilt(*)
      character*(*) filename
      call dopen(nunit,filename,'ro','f')
      if(nview.eq.0)then
10      read(nunit,*,end=20,err=40)tilt(nview+1)
        nview=nview+1
        if (nview .gt. limtilt) call gta_errorexit(
     &      'TOO MANY VIEWS FOR TILT ANGLE ARRAY')
        go to 10
      else
        read(nunit,*,err=40,end=30)(tilt(i),i=1,nview)
      endif
20    close(nunit)
      return
30    call gta_errorexit('end of file reached reading tilt angles')
40    call gta_errorexit('error reading tilt angles')
      end
      

      subroutine gta_errorexit(message)
      character*(*) message
      write(*,'(/,a,a)')'ERROR: GET_TILT_ANGLES - ',message
      call exit(1)
      end
      

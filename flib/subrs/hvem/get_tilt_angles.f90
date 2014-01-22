! !
! GET_TILT_ANGLES will read in tilt angles by the user's method of
! choice.
! ^ [numViews] should be the number of views, if known, or 0 if it
! not known, in which case the routine will return the number in this
! variable.
! ^ [nunit] should contain the number of a free logical unit.
! ^ Tilt angles are returned in [tilt].
! ^ [limTilt] should contain the dimensions of [tilt].
! ^ [ifpip] should be set to 0 for interactive input, or nonzero for
! input via PIP.  In the later case, the program must define the
! options FirstTiltAngle, TiltIncrement, TiltFile, and TiltAngles.
! If a TiltFile is entered, or if TiltAngles are entered, they
! supercede entries of FirstTiltAngle and TiltIncrement.
! !
! $Id$
!
subroutine get_tilt_angles(numViews, nunit, tilt, limTilt, ifpip)
  implicit none
  integer*4 numViews, nunit, ifpip, limTilt
  real*4 tilt(*)
  character*320 filename
  integer*4 nViewIn, i, ierr, ierr2, numLines, index, ninLine
  real*4 tiltStart, tiltInc
  logical startIncOK
  integer*4 PipGetFloat, PipGetString, PipNumberOfEntries
  integer*4 PipGetFloatArray
  !
  if (ifpip .ne. 0) then
    ierr = PipGetFloat('FirstTiltAngle', tiltStart)
    ierr2 = PipGetFloat('TiltIncrement', tiltInc)
    startIncOK = ierr == 0 .and. ierr2 == 0

    ierr = PipGetString('TiltFile', filename)
    ierr2 = PipNumberOfEntries('TiltAngles', numLines)

    if (ierr > 0 .and. numLines == 0) then
      if (.not. startIncOK) call gta_errorexit('NO TILT ANGLES'// &
          ' SPECIFIED BY START AND INCREMENT, FILE, OR INDIVIDUAL VALUES')
      do i = 1, numViews
        tilt(i) = tiltStart + (i - 1) * tiltInc
      enddo
      return
    endif

    if (numLines > 0) then
      if (ierr == 0) call gta_errorexit('YOU CANNOT SPECIFY '// &
          'BOTH A TILT ANGLE FILE AND INDIVIDUAL ENTRIES')
      index = 0
      do i = 1, numLines
        ninLine = 0
        ierr = PipGetFloatArray('TiltAngles', tilt(index + 1), ninLine, limTilt - index)
        index = index + ninLine
      enddo
      if (numViews == 0) numViews = index
      if (index .ne. numViews) then
        print *
        print *,'ERROR: GET_TILT_ANGLES -', numViews, &
            ' ANGLES EXPECTED BUT ONLY', index, ' ENTERED'
        call exit(1)
      endif
      return
    endif
  else
    if (numViews == 0) then
      write(*,'(1x,a,/,a,/,a,$)') 'Enter the # of tilt angles to '// &
          'specify them by starting and increment angle,', &
          '     - the # of angles to specify each individual value,' &
          , '    or 0 to read angles from a file: '
      read(5,*) nViewIn
      numViews = abs(nViewIn)
    else
      write(*,'(1x,a,/,a,/,a,$)') 'Enter 1 to '// &
          'specify starting and increment angle,', &
          '     -1 to specify each individual value,' &
          , '    or 0 to read angles from a file: '
      read(5,*) nViewIn
    endif
    if (numViews > limTilt) call gta_errorexit('TOO MANY VIEWS FOR TILT ANGLE ARRAY')
    if (nViewIn > 0) then
      write(*,'(1x,a,$)') 'Starting and increment angle: '
      read(5,*) tiltStart, tiltInc
      do i = 1, numViews
        tilt(i) = tiltStart + (i - 1) * tiltInc
      enddo
      return
    elseif (nViewIn < 0) then
      print *,'Enter all tilt angles'
      read(5,*,err = 40, end = 30) (tilt(i), i = 1, numViews)
      return
    else
      write(*,'(1x,a,$)') 'Name of file with tilt angles: '
      read(5, '(a)') filename
    endif
  endif

  call read_tilt_file(numViews, nunit, filename, tilt, limTilt)
  return
30 call gta_errorexit('end of input reached reading tilt angles')
40 call gta_errorexit('error reading tilt angles')
end subroutine get_tilt_angles


! !
! READ_TILT_FILE will read in tilt angles from a file.
! ^ [numViews] should be the number of views, if known, or 0 if it
! not known, in which case the routine will return the number in this
! variable.
! ^ [nunit] should contain the number of a free logical unit.
! ^ [filename] should contain the filename.
! ^ Tilt angles are returned in [tilt].
! ^ [limTilt] should contain the dimensions of [tilt].
! !
subroutine read_tilt_file(numViews, nunit, filename, tilt, limTilt)
  implicit none
  integer*4 numViews, nunit, limTilt, ind
  real*4 tilt(*)
  character*(*) filename
  character*80 message
  call dopen(nunit, filename, 'ro', 'f')
  if (numViews == 0) then
10  ind = numViews + 1
    read(nunit,*,end = 20, err = 40) tilt(ind)
    numViews = ind
    if (numViews > limTilt) call gta_errorexit('TOO MANY VIEWS FOR TILT ANGLE ARRAY')
    go to 10
  else
    do ind = 1, numViews
      read(nunit,*,err = 40, end = 30) tilt(ind)
    enddo
  endif
20 close(nunit)
  return
30 write(message,'(a,i5,a,i5)')'end of file reached after reading',ind - 1,  &
       ' tilt angles, expected', numViews
  call gta_errorexit(message)
40 write(message,'(a,i5)')'error reading tilt angles on line', ind
  call gta_errorexit(message)
end subroutine read_tilt_file


subroutine gta_errorexit(message)
  character*(*) message
  write(*,'(/,a,a)') 'ERROR: GET_TILT_ANGLES - ', trim(message)
  call exit(1)
end subroutine gta_errorexit

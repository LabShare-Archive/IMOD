      implicit none
      integer limview
      parameter (limview = 720)
      real*4 tilt(limview), alpha(limview), xf(2,3,limview), fr(2,3), fp(2,3)
      character*320 filin, filout,alphain, alphaout,tiltout
      real*4 rotation, beamtilt, rot, xaxistilt
      integer*4 numOptArg, numNonOptArg, nview, i, ifanyalf, ierr
      integer*4 PipGetInOutFile, PipGetFloat, PipGetString, PipGetLogical
      logical*4 reverse
      real*4 dtor/0.0174532/
      real*4 cosd, sind
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfforfidless
c       
      integer numOptions
      parameter (numOptions = 11)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputTransforms:FN:@output:OutputTransforms:FN:@'//
     &    'rotation:RotationAngle:F:@beam:BeamTilt:F:@'//
     &    'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'//
     &    'tiltfile:TiltFile:FN:@angles:TiltAngles:FAM:@'//
     &    'newtilt:NewTiltFile:FN:@xtilt:XTiltFile:FN:@'//
     &    'newxtilt:NewXTiltfile:FN:'

      rotation = 0.
      beamtilt = 0.
      ifanyalf = 0
      alphain = ' '
      alphaout = ' '
      tiltout = ' '
      xaxistilt = 0.
      reverse = .false.

      call PipReadOrParseOptions(options, numOptions, 'xfforfidless',
     &    'ERROR: XFFORFIDLESS - ', .false., 4, 1, 1, numOptArg,
     &    numNonOptArg)

      if (PipGetInOutFile('InputTransforms', 1, ' ', filin)
     &    .ne. 0) call exitError('NO INPUT TRANSFORM FILE SPECIFIED')
      if (PipGetInOutFile('OutputTransforms', 2, ' ', filout)
     &    .ne. 0) call exitError('NO OUTPUT TRANSFORM FILE SPECIFIED')

      ierr = PipGetFloat('BeamTilt', beamtilt)
      ierr = PipGetFloat('RotationAngle', rotation)
      ierr = PipGetFloat('XAxisTilt', xaxistilt)
      ierr = PipGetString('NewTiltFile', tiltout)
      ierr = PipGetString('XTiltFile', alphain)
      ierr = PipGetString('NewXTiltFile', alphaout)
      ierr = PipGetLogical('FullToPostRotation', reverse)
c       
c       Read transforms to know how many views
      call dopen(1, filin, 'ro', 'f')
      call xfrdall(1, xf, nview, *95)
      close(1)
      print *,'Got input'
c       
c       Get the tilt angles then the X tilts
      call get_tilt_angles(nview, 1, tilt, limview, 1)
      call PipDone()
      print *,'got angles'
      if (alphain .ne. ' ') then
        call dopen(1, alphain, 'ro', 'f')
        read(1,*, err=96, end=96)(alpha(i), i = 1, nview)
        do i = 1, nview
          if (alpha(i) .ne. 0) ifanyalf = 1
        enddo
        close(1)
      else
        do i = 1, nview
          alpha(i) = 0.
        enddo
      endif
      if (xaxistilt .ne. 0) ifanyalf = 1
c       
      if ((ifanyalf .ne. 0 .or. beamtilt .ne. 0.) .and. alphaout .eq. ' ' .and.
     &    .not.reverse) call exitError('X-TILT OUTPUT FILE MUST BE ENTERED')
      call dopen(1, filout, 'new', 'f')
      if (.not. reverse .and. tiltout .eq. ' ')
     &    call exitError('TILT ANGLE OUTPUT FILE MUST BE ENTERED')
      if (tiltout .ne. ' ') call dopen(2, tiltout, 'new', 'f')
      if (alphaout .ne. ' ') call dopen(3, alphaout, 'new', 'f')
c       
c       convert angles to account for beam tilt
      do i = 1, nview
        rot = -rotation
        if (beamtilt .ne. 0.) call convert_for_beamtilt(alpha(i), tilt(i),
     &      rot, beamtilt * dtor, ifanyalf)
        fr(1,1) = cosd(rot)
        fr(1,2) = -sind(rot)
        if (reverse) fr(1,2) = -fr(1,2)
        fr(2,1) = -fr(1,2)
        fr(2,2) = fr(1,1)
        fr(1,3) = 0.
        fr(2,3) = 0.
        call xfmult(fr, xf(1,1,i), fp)
        call xfwrite(1, fp, *97)
        if (tiltout .ne. ' ') write(2, '(f8.2)', err=98)tilt(i)
        if (alphaout .ne. ' ') write(3, '(f8.2)', err=98)alpha(i) - xaxistilt
      enddo
      close(1)
      if (tiltout .ne. ' ') close(2)
      if (alphaout. ne. ' ') close(3)
      call exit(0)
95    call exitError('READING TRANSFORM FILE')
96    call exitError('NOT ENOUGH X-AXIS TILTS IN FILE')
97    call exitError('WRITING TO TRANSFORM FILE')
98    call exitError('WRITING TO TILT OR XTILT FILE')
      end

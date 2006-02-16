*************HEADER.FOR**********************************************
*       
*	A JIFFY TO READ THE HEADER ON AN IMAGE FILE
*       
************************************************************************
c       Version for unix can take the file name either on the command line
c       or as an entry to the program.  If here is no file name on the command
c       line, the program asks for the file name.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2004/06/10 22:40:50  mast
c       Made it rotate a reported tilt angle from FEI header by 180 degrees
c       if bigger than 90 degrees
c	
c       Revision 3.1  2003/06/05 00:10:19  mast
c       Make it output pixel size and tilt axis rotation angle for Agard style
c       extended header
c	
*       
      implicit none
      integer maxextra
      parameter (maxextra = 2000000)
      real*4 array(maxextra/4)
      integer*4 NXYZ(3),MXYZ(3), mode, nbsym, nint, nreal, ierr, i, j
      logical nbytes_and_flags
C       
      CHARACTER*120 FILIN
C       
      integer*4 numInputFiles, nfilein
      logical*4 doSize, doMode, doMin,doMax,doMean, silent, doPixel, doOrigin
      real*4 DMIN,DMAX,DMEAN, pixel,tiltaxis, delta(3)
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetLogical, PipGetString, PipGetNonOptionArg
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  header
c       
      integer numOptions
      parameter (numOptions = 9)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FNM:@size:Size:B:@mode:Mode:B:@'//
     &    'pixel:PixelSize:B:@origin:Origin:B:@minimum:Minimum:B:@'//
     &    'maximum:Maximum:B:@mean:Mean:B:@help:usage:B:'
c       
c       defaults
c
      doSize = .false.
      doMode = .false.
      doMin = .false.
      doMax = .false.
      doMean = .false.
      doPixel = .false.
      doOrigin = .false.
      nfilein = 1
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'header',
     &    'ERROR: HEADER - ', .true., 1, 2, 0, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
C       
      if (pipinput) then
        ierr = PipGetLogical('Size', doSize)
        ierr = PipGetLogical('Mode', doMode)
        ierr = PipGetLogical('Max', doMax)
        ierr = PipGetLogical('Min', doMin)
        ierr = PipGetLogical('Mean', doMean)
        ierr = PipGetLogical('PixelSize', doPixel)
        ierr = PipGetLogical('Origin', doOrigin)
        call PipNumberOfEntries('InputFile', numInputFiles)
        nfilein = numInputFiles + numNonOptArg
        if (nfilein .eq. 0) then
          print *,'ERROR: HEADER - No input file specified'
          call exit(1)
        endif
      endif

      silent = doSize .or. doMode .or. doMin .or. doMax .or. doMean .or.
     &    doPixel .or. doOrigin
      if (silent) call ialprt(.false.)

      do i=1,nfilein
c         
c         get the next filename
c         
        if (pipinput) then
          if (i .le. numInputFiles) then
            ierr = PipGetString('InputFile', filin)
          else
            ierr = PipGetNonOptionArg(i - numInputFiles, filin)
          endif
        else
          write(*,'(1x,a,$)')'Name of input file: '
          READ(5,'(a)')FILIN
        endif
c       
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C         
        if (silent) then
          if (doSize) write(*, '(3i8)')(nxyz(j),j=1,3)
          if (doMode) write(*, '(i4)')mode
          if (doPixel) then
            call irtdel(1, delta)
            write(*, '(3g11.4)')(delta(j), j= 1,3)
          endif
          if (doOrigin) then
            call irtorg(1,delta(1), delta(2), delta(3))
            write(*, '(3g11.4)')(delta(j), j= 1,3)
          endif
          if (doMin) write(*, '(g13.5)')dmin
          if (doMax) write(*, '(g13.5)')dmax
          if (doMean) write(*, '(g13.5)')dmean
        else
          call irtnbsym(1,nbsym)
          if(nbsym.gt.0.and.nbsym.le.maxextra) then
            call irtsym(1,nbsym,array)
            call irtsymtyp(1,nint,nreal)
            if (.not. nbytes_and_flags(nint, nreal) .and. nreal .ge. 12) then
              tiltaxis = array(nint + 11)
              if (tiltaxis .ge. -360. .and. tiltaxis .le. 360.) then
                if (tiltaxis .lt. -90.) tiltaxis = tiltaxis + 180.
                if (tiltaxis .gt. 90.) tiltaxis = tiltaxis - 180.
                write(*,101)tiltaxis
101             format(10x,'Tilt axis rotation angle =', f6.1)
              endif
              pixel = array(nint + 12) * 1.e9
              if (pixel .gt. 0.01 .and. pixel .lt. 10000.) write(*,102)pixel
102           format(10x,'Pixel size in nanometers =', g11.4) 
            endif
          endif
        endif

        CALL IMCLOSE(1)
      enddo
c       
      call exit(0)
      END



*************HEADER.F**********************************************
*       
*	A SMALL PROGRAM TO READ THE HEADER ON AN IMAGE FILE
*       
************************************************************************
c       
c       $Id$
c       Log at end of file
*       
      implicit none
      integer maxextra,ntypes
      parameter (maxextra = 2000000, ntypes = 6)
      real*4 array(maxextra/4)
      integer*4 NXYZ(3),MXYZ(3), mode, nbsym, nint, nreal, ierr, i, j
      integer*4 labels(20,10), nlabel
      character*4 feichar
      logical nbytes_and_flags
      integer*4 lnblnk
C       
      CHARACTER*320 FILIN
C       
      character*17 typeName(ntypes)/'Tilt angles', 'Piece coordinates',
     &    'Stage positions', 'Magnifications', 'Intensities', 'Exposure doses'/
      character*25 extractCom(ntypes)/'extracttilts', 'extractpieces',
     &    'extracttilts -stage', 'extracttilts -mag', 'extracttilts -int',
     &    'extracttilts -exp'/
      integer*4 numInputFiles, nfilein, ifBrief
      logical*4 doSize, doMode, doMin,doMax,doMean, silent, doPixel, doOrigin
      real*4 DMIN,DMAX,DMEAN, pixel,tiltaxis, delta(3)
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetLogical, PipGetString, PipGetNonOptionArg, PipGetInteger
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  header
c       
      integer numOptions
      parameter (numOptions = 10)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FNM:@size:Size:B:@mode:Mode:B:@'//
     &    'pixel:PixelSize:B:@origin:Origin:B:@minimum:Minimum:B:@'//
     &    'maximum:Maximum:B:@mean:Mean:B:@brief:Brief:B:@help:usage:B:'
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
      ifBrief = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       But turn off the entry printing first!
      call PipEnableEntryOutput(0)
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
        ierr = PipGetInteger('Brief', ifBrief)
        call PipNumberOfEntries('InputFile', numInputFiles)
        nfilein = numInputFiles + numNonOptArg
        if (nfilein .eq. 0)
     &      call exitError('No input file specified')
      endif

      silent = doSize .or. doMode .or. doMin .or. doMax .or. doMean .or.
     &    doPixel .or. doOrigin
      if (silent) call ialprt(.false.)
      call ialbrief(ifBrief)

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
                if (tiltaxis .lt. -180.) tiltaxis = tiltaxis + 360.
                if (tiltaxis .gt. 180.) tiltaxis = tiltaxis - 360.
                call irtlab(1,labels,nlabel)
                write(feichar, '(a4)')labels(1,1)
                if (feichar .eq. 'Fei ') then
                  write(*,101)-tiltaxis,' (Corrected sign)'
                else
                  write(*,101)tiltaxis
                endif
101             format(10x,'Tilt axis rotation angle =', f6.1, a)
              endif
              pixel = array(nint + 12) * 1.e9
              call irtdel(1, delta)
              if (pixel .gt. 0.01 .and. pixel .lt. 10000.) then
                if (delta(1) .eq. 1) then
                  write(*,102)pixel
                else
                  write(*,104)pixel
                endif
102             format(10x,'Pixel size in nanometers =', g11.4) 
104             format(10x,'Original pixel size in nanometers =', g11.4) 
              endif
            endif
            if (nbytes_and_flags(nint, nreal) .and. .not.silent .and. 
     &          ifBrief .eq. 0) then
              write(*,'(/,a)')'Extended header from SerialEM contains:'
              do j = 1, ntypes
                if (mod(nreal/2**(j-1), 2) .ne. 0)
     &              write(*,103)typeName(j),
     &              extractCom(j)(1:lnblnk(extractCom(j)))
103             format(2x, a, ' - Extract with "',a,'"')
              enddo
            endif
          endif
        endif

        CALL IMCLOSE(1)
      enddo
c       
      call exit(0)
      END

c       
c       $Log$
c       Revision 3.8  2009/03/27 16:25:29  mast
c       Invert the sign of rotation angle for FEI header
c
c       Revision 3.7  2009/02/16 06:27:28  mast
c       Added detail on extra header and directives for extracting
c
c       Revision 3.6  2008/01/31 22:43:36  mast
c       Redimension filename to 320
c
c       Revision 3.5  2006/10/05 19:48:59  mast
c       Don't rotate FEI angles by 180, it ruins polarity of tilt angles
c
c       Revision 3.4  2006/09/28 21:45:25  mast
c       Added brief output option and turned off brief output by default
c
c       Revision 3.3  2006/02/16 04:05:21  mast
c       Converted to PIP and added simple output options
c
c       Revision 3.2  2004/06/10 22:40:50  mast
c       Made it rotate a reported tilt angle from FEI header by 180 degrees
c       if bigger than 90 degrees
c	
c       Revision 3.1  2003/06/05 00:10:19  mast
c       Make it output pixel size and tilt axis rotation angle for Agard style
c       extended header
c	

*       XFINVERSE will take the inverse of each transformation in a file.
c       The only option is whether to set the translations to zero or retain
c       them in the output.  The latter would give the true inverse, but the
c       former has been useful in arcane realignment procedures.
c       
c       Inputs to the program:
c       
c       Input file with list of transformations
c       
c       Output file for inverse transformations
c       
c       0 to set the translations (shifts) to zero, or one to retain them
c       and output the true inverse transformation
c       
c       David Mastronarde, added to IMOD 4/21/00
c       
      implicit none
      integer nflimit
      parameter (nflimit=100000)
      real*4 f(2,3,nflimit),prod(2,3)
      real*4, allocatable :: xControl(:), yControl(:), dxGrid(:), dyGrid(:)
      real*4, allocatable :: dxInv(:), dyInv(:)
      character*320 filin, filout, errString
      integer*4 ifzero, i, nout, ierr, iflags, nx, ny, ibin, nxMax, nyMax
      integer*4 maxControl, nControl, j, nxGrid, nyGrid
      real*4 pixelSize, xcen, ycen, xnew, ynew, xcvinv, ycvinv
      real*4 xStart, yStart, xInterval, yInterval
      integer*4 readCheckWarpFile, getNumWarpPoints, getLinearTransform, getWarpPoints
      integer*4 setWarpPoints, getGridParameters, getWarpGrid, setWarpGrid
      integer*4 setLinearTransform, writeWarpFile, getWarpGridSize

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInOutFile, PipParseInput, PipGetBoolean,PipGetLogical
      logical*4 pipinput, control
      integer numOptions
      parameter (numOptions = 4)
      character*(120 * numOptions) options(1) 
      options(1) = 
     &    'input:InputFile:FN:Name of input transform file@'//
     &    'output:OutputFile:FN:Name of output file for inverse transforms@'//
     &    'zero:ZeroShifts:B:Set shifts to zero@'//
     &    'help:usage:B:Print help output'
c       
      ifzero = 0

      call PipExitOnError(0, 'ERROR: XFINVERSE -')
      call PipAllowCommaDefaults(1)
      ierr = PipParseInput(options, numOptions, '@', numOptArg, numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
      if (pipinput) then
        if (PipGetBoolean('help', ierr) .eq. 0) then
          call PipPrintHelp('xfinverse', 0, 1, 1)
          call exit(0)
        endif
        ierr = PipGetBoolean('ZeroShifts', ifzero)
      endif
      if (PipGetInOutFile('InputFile', 1, 'Input file of transforms', filin) .ne. 0)
     &    call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 2, 'Name of output piece list file',
     &    filout) .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')

      ierr = readCheckWarpFile(filin, 0, 0, nx, ny, nout, ibin, pixelSize,
     &    iflags, errString)
      if (ierr .lt. -1) call exitError(errString)
      if (.not. pipinput) then
        write(*,'(1x,a,$)') '0 to set shifts to zero, or 1 to retain them: '
        read(5,*)j
        if (j .eq. 0) ifzero = 1
      endif
      if (ierr .lt. 0) then
c
c         Ordinary transforms: read, invert and write
        call dopen(1,filin,'ro','f')
        call xfrdall2(1,f,nout,nflimit,ierr)
        if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
        if (ierr .eq. 1) call exitError('TOO MANY TRANSFORMS IN FILE FOR TRANSFORM ARRAY')
c       
        call dopen(3,filout,'new','f')
c       
c       
        do i=1,nout
          call xfinvert(f(1,1,i),prod)
          if(ifzero.ne.0)then
            prod(1,3)=0.
            prod(2,3)=0.
          endif
          call xfwrite(3,prod,*94)
        enddo
        print *,nout,' inverse transforms written'
        close(3)
      else
c         
c         Warping: first figure out how much memory to allocate
        write(*,'(a,a)')'Old warping file opened: ',trim(filin)
       if (iflags .eq. 0) call exitError('An inverse can be taken only if '//
     &      'warping grid is identified as an inverse warping')

        control = mod(iflags / 2, 2) .ne. 0
        if (control) then
          if (getNumWarpPoints(-1, maxControl) .ne. 0) call exitError(
     &        'Getting maximum number of control points')
          allocate(xControl(maxControl), yControl(maxControl), dxGrid(maxControl), 
     &        dyGrid(maxControl), stat = ierr)
          call memoryError(ierr, 'ARRAYS FOR CONTROL POINTS')
        else
          if (getWarpGridSize(-1, nxMax, nyMax, maxControl) .ne. 0) call exitError(
     &        'Getting maximum size of warp grids')
          allocate(dxInv(maxControl), dyInv(maxControl), dxGrid(maxControl), 
     &        dyGrid(maxControl), stat = ierr)
          call memoryError(ierr, 'ARRAYS FOR WARPING GRID')
        endif
        xcen = nx / 2.
        ycen = ny / 2.

        do i=1,nout
          if (getLinearTransform(i, f) .ne. 0) call exitError(
     &        'Getting linear transform from warping file')
          if (control) then
c             
c             Control points: get them, apply transformations, and store them back
            call xfinvert(f, prod)
            
            if (getNumWarpPoints(i, nControl) .ne. 0 .or.
     &          getWarpPoints(i, xControl, yControl, dxGrid, dyGrid) .ne. 0)
     &          call exitError('Getting control points')
            do j = 1, nControl
              call xfApply(prod, xcen, ycen, xControl(j), yControl(j), xnew, ynew)
              call xfApply(prod, xcen, ycen, xControl(j) + dxGrid(j),
     &            yControl(j) + dyGrid(j), xcvinv, ycvinv)
              xControl(j) = xnew
              yControl(j) = ynew
c               
c               Verified that this is the same as minus the 2x2 matrix times the vectors
              dxGrid(j) = xnew - xcvinv
              dyGrid(j) = ynew - ycvinv
            enddo
            if (setWarpPoints(i, nControl, xControl, yControl, dxGrid, dyGrid) .ne. 0)
     &          call exitError('Saving transformed control points')
          else
c             
c             Grid: get them, call inverse routine, save them back
            if (getGridParameters(i, nxGrid, nyGrid, xStart, yStart, xInterval, yInterval)
     &          .ne. 0) call exitError('Getting warp grid parameters')
            if (getWarpGrid(i, nxGrid, nyGrid, xStart, yStart, xInterval, yInterval,
     &          dxGrid, dyGrid, nxGrid) .ne. 0) call exitError('Getting a warp grid')
            call invertWarpGrid(dxGrid, dyGrid, nxGrid, nxGrid, nyGrid, xStart, yStart,
     &          xInterval, yInterval, f, xcen, ycen, dxInv, dyInv, prod)
            if (setWarpGrid(i, nxGrid, nyGrid, xStart, yStart, xInterval, yInterval,
     &          dxInv, dyInv, nxGrid) .ne. 0) call exitError('Saving a warp grid')
          endif
          if (setLinearTransform(i, prod) .ne. 0)
     &        call exitError('Saving linear transform to warping file')
        enddo

        if (writeWarpFile(filout, 0) .ne. 0) call exitError('Writing out warping file')
        print *,nout,' inverse transforms written to new warping file'
      endif
      call exit(0)
c       
92    call exitError('Reading old transform file')
94    call exitError('Writing out inverse transform file')
      end

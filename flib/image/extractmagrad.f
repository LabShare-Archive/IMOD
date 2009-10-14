*************EXTRACTMAGRAD.FOR********************************************
c       
c       Extractmaggrad will read the intensity values and tilt angles from
c       an image file header, and read a table of magnification gradients
c       as a function of intensity, and output a file with a list of
c       tilt angles and mag gradients for each section.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2006/02/06 21:46:09  mast
c       Read version 2 mgt file with crossover, use linearized intensity for
c       interpolation
c
c       Revision 3.1  2004/03/22 05:43:54  mast
c       Initial creation
c	
c	
c       
      implicit none
      integer maxtab
      parameter (maxtab = 1000)
      integer*4 NXYZ(3),MXYZ(3)
      integer*4 maxextra, maxtilts, maxpiece
      real*4 delta(3)
      real*4 tableC2(maxtab), tableGrad(maxtab), tableRot(maxtab)
      real*4 tableLin(maxtab)
      real*4, allocatable :: tilt(:), c2val(:), array(:)
      integer*4, allocatable :: ixpiece(:),iypiece(:),izpiece(:)
C       
      CHARACTER*320 FILIN,filout, gradTable,line
      character*16 typeText(5) /'tilt angle', ' ', 'stage position',
     &    'magnification', 'intensity value'/
C       
      integer*4 nz, ierr, npiece, i, nbyte, iflags, iVersion
      integer*4 maxz, iunout, lenText, mode, ntilt, nbsym
      integer*4 ntiltout, numInTable, j, magVersion, nField
      real*4 dmin, dmax, dmean, axisRot, pixelSize, rot, grad, frac
      real*4 deltaGrad, deltaRot, crossover, xnum(20)
      logical*4 looking
      EQUIVALENCE (Nz,NXYZ(3))
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString, PipGetFloat, PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  extractmagrad
c       
      integer numOptions
      parameter (numOptions = 8)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputImageFile:FN:@output:OutputFile:FN:@'//
     &    'gradient:GradientTable:FN:@rotation:RotationAngle:F:@'//
     &    'pixel:PixelSize:F:@dgrad:DeltaGradient:F:@'//
     &    'drot:DeltaRotation:F:@help:usage:B:'
c       
      pixelSize = 0.
      magVersion = 1
      deltaGrad = 0.
      deltaRot = 0.
C       
c       Pip startup: set error, parse options, check help
c       
      call PipReadOrParseOptions(options, numOptions, 'extractmagrad',
     &    'ERROR: EXTRACTMAGRAD - ', .false., 3, 1, 1, numOptArg,
     &    numNonOptArg)

      if (PipGetInOutFile('InputImageFile', 1, ' ', filin)
     &    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 2, ' ', filout)
     &    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
      ierr = PipGetInOutFile('OutputFile', 2,
     &    'Name of output file, or return to print out values', filout)
c       
      if (PipGetString('GradientTable', gradTable) .ne. 0)
     &    call errorexit('YOU MUST SPECIFY A GRADIENT TABLE')
      if (PipGetFloat('RotationAngle', axisRot) .ne. 0)
     &    call errorexit('YOU MUST ENTER A ROTATION ANGLE')
      ierr = PipGetFloat('PixelSize', pixelSize)
      ierr = PipGetFloat('DeltaGradient', deltaGrad)
      ierr = PipGetFloat('DeltaRotation', deltaRot)
      call PipDone()

      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
C       Get pixel size from header if it was not entered
C       
      pixelSize = pixelSize * 10.
      if (pixelSize .eq. 0) then
        call irtdel(1, delta)
        if (delta(1) .eq. 1.0) call errorexit('YOU MUST ENTER A '//
     &      'PIXEL SIZE SINCE THE PIXEL SPACING IN THE HEADER IS 1')
        pixelSize = delta(1)
      endif
c       
      call irtnbsym(1,nbsym)
      maxextra = nbsym + 1024
      maxpiece = nz + 1024
      maxtilts = maxpiece
      allocate(tilt(maxtilts), c2val(maxtilts), array(maxextra/4), 
     &    ixpiece(maxpiece),iypiece(maxpiece),izpiece(maxpiece), stat = ierr)
      if (ierr .ne. 0) call exitError(
     &    'ALLOCATING ARRAYS FOR EXTRA HEADER DATA')

      call irtsym(1,nbsym,array)
      call irtsymtyp(1,nbyte,iflags)
      call get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &    ixpiece,iypiece,izpiece,npiece,maxpiece)
      if(npiece.eq.0)then
        do i=1,nz
          izpiece(i)=i-1
        enddo
        maxz=nz
      else
        maxz=0
        do i=1,npiece
          maxz=max(maxz,izpiece(i))
        enddo
      endif
c       
c       set up a marker value for empty slots
c       
      do i=1,maxz
        tilt(i)=-999.
      enddo
c       
      call get_extra_header_tilts (array,nbsym,nbyte,iflags,nz,
     &    tilt,ntilt,maxtilts,izpiece)
      if (ntilt.eq.0) call errorexit(
     &    'THERE ARE NO TILT ANGLES IN THE IMAGE FILE HEADER')
      call get_extra_header_items (array,nbsym,nbyte,iflags,nz,5,
     &    c2val, c2val,ntilt,maxtilts,izpiece)
      if (ntilt.eq.0) call errorexit(
     &    'THERE ARE NO INTENSITIES IN THE IMAGE FILE HEADER')
c       
c       Open gradient table file and look for version and crossover
c       
      open(1, file=gradTable, status='OLD',form='FORMATTED',err=40)
      read(1, '(a)') line
      call frefor(line, xnum, nfield)
      if (nfield .eq. 1) then
        iVersion = nint(xnum(1))
        read(1, *) crossover
      else
        iVersion = 1
        close(1)
        open(1, file=gradTable, status='OLD',form='FORMATTED',err=40)
      endif
c       
c       read the gradient table
c       
      i = 1
10    read(1, *, err=30, end=20) tableC2(i), tableGrad(i), tableRot(i)
      if (iVersion .gt. 1) tableLin(i) = 1. / (tableC2(i) - crossover)
      numInTable = i
      i = i + 1
      go to 10

20    call dopen(2,filout,'new','f')
c       
c       pack the values down and get count
c       
      ntiltout=0
      do i=1,ntilt
        if(tilt(i).ne.-999.)then
          ntiltout=ntiltout+1
          tilt(ntiltout)=tilt(i)
          c2val(ntiltout)=c2val(i)
        endif
      enddo

      write(2, '(i8)')magVersion
      write(2, '(i6, f8.3, f9.2)') ntiltout, pixelSize, axisRot

      do i=1,ntiltout
        if(tilt(i).ne.-999.)then
          if (c2val(i) .lt. tableC2(1)) then
            grad = tableGrad(1)
            rot = tableRot(1)
          else if (c2val(i) .gt. tableC2(numInTable)) then
            grad = tableGrad(numInTable)
            rot = tableRot(numInTable)
          else
            looking = .true.
            j = 1
            do while (looking .and. j .lt. numInTable)
              frac = (c2val(i) - tableC2(j)) / (tableC2(j + 1) - tableC2(j))
              if (frac .ge. 0. .and. frac .le. 1.) then
                if (iVersion .gt. 1) frac = (1. / (c2val(i) - crossover) -
     &              tableLin(j)) / (tableLin(j + 1) - tableLin(j))
c                 
c                 For version 2, do not extrapolation closer to crossover
c                 It should already have been done when making the table
c
                if (iVersion .gt. 1. and. tableC2(j) .lt. crossover .and.
     &              tableC2(j + 1) .gt. crossover) then
                  if (c2val(i) .lt. crossover) then
                    grad = tableGrad(j)
                    rot = tableRot(j)
                  else
                    grad = tableGrad(j + 1)
                    rot = tableRot(j + 1)
                  endif
                else
                  grad = (1. - frac) * tableGrad(j) + frac * (tableGrad(j + 1))
                  rot = (1. - frac) * tableRot(j) + frac * (tableRot(j + 1))
                endif
                looking = .false.
              endif
              j = j + 1
            enddo
          endif
          write(2, '(f8.2, 2f8.3)')tilt(i), grad + deltaGrad, rot + deltaRot
        endif
      enddo

      print *,ntiltout,' gradients output to file'

      CALL IMCLOSE(1)
c       
      call exit(0)
30    call errorexit('READING MAG GRADIENT TABLE FILE')
40    call errorexit('OPENING MAG GRADIENT TABLE FILE')
      END

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: EXTRACTMAGRAD - ',message
      call exit(1)
      end

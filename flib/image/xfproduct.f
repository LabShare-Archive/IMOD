*       XFPRODUCT  is used to concatenate two lists of transforms, or to
c       multiply all of the transforms in one file by another transform.
c       
c       See man page for details.
c       
c       David Mastronarde 1989
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer nflimit
      parameter (nflimit=100000)
      real*4 f(2,3,nflimit,2),ftmp(2,3,3)
      character*320 gfile(2),outFile,errString
c       
      integer*4 nfirst, nsecond, nout, ierr, i, nsingle, ifs, indcopy, inds(2)
      real*4 scales(2), xAllStr(2), xAllEnd(2), yAllStr(2), yAllEnd(2)
      logical*4 warping(2), controlPts(3), linear(2), linearOnly(3), needGrid
      integer*4 indWarpFile(3), numXforms(2), iflags, ibin, nx(3), ny(3)
      equivalence (nfirst, numXforms(1)), (nsecond, numXforms(2))
      integer*4 j, iz, nxgDim, nygDim, nControl, nxGrTmp, nyGrTmp, maxControl(2)
      real*4 xAllInt, yAllInt, xIntTmp, yIntTmp, xStrTmp, yStrTmp, xcen, ycen
      real*4 xvecTmp(3), yvecTmp(3), xconTmp(3), yconTmp(3), pixelSize(3)
      real*4, allocatable :: xControl(:), yControl(:), xVector(:), yVector(:)
      real*4, allocatable :: dxGrid(:,:,:), dyGrid(:,:,:)
      integer*4 ifUse2nd, nxGrids(2), nyGrids(2), ifScales
      real*4 xNewCont, yNewCont, xNewCpv, yNewCpv, warpScale, xStarts(2), yStarts(2)
      real*4 xIntervals(2), yIntervals(2)
      integer*4 readCheckWarpFile, getNumWarpPoints, getLinearTransform
      integer*4 getGridParameters, getWarpGrid, setWarpGrid, expandAndExtrapGrid
      integer*4 setLinearTransform, writeWarpFile, multiplyWarpings, newWarpFile
      integer*4 getWarpPoints, setCurrentWarpFile, setWarpPoints, gridSizeFromSpacing
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetTwoFloats, PipGetInteger
      integer*4 PipGetInOutFile
c       
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfproduct
c       
      integer numOptions
      parameter (numOptions = 6)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'in1:InputFile1:FN:@in2:InputFile2:FN:@output:OutputFile:FN:@'//
     &    'scale:ScaleShifts:FP:@one:OneXformToMultiply:I:@help:usage:B:'
c       
      scales(1) = 1.
      scales(2) = 1.
      nsingle = -1
c       
      call PipReadOrParseOptions(options, numOptions, 'xfproduct', 'ERROR: XFPRODUCT - ',
     &    .true., 3, 2, 1, numOptArg, numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
c       Get all the filenames
      if (PipGetInOutFile('InputFile1', 1, 'File of transforms applied first', gfile(1))
     &    .ne. 0) call exitError('NO FIRST INPUT FILE SPECIFIED')
      if (PipGetInOutFile('InputFile2', 2, 'File of transforms applied second', gfile(2))
     &    .ne. 0) call exitError('NO SECOND INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 3, 'New file for product transforms', outfile)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
c
c       Open the input files and read transforms if linear files
      do ifs = 1, 2
        indWarpFile(ifs) = readCheckWarpFile(gfile(ifs), 0, 1, nx(ifs),
     &      ny(ifs), numXforms(ifs), ibin, pixelSize(ifs), iflags, errString)
        if (indWarpFile(ifs) .lt. -1) call exitError(errString)
        warping(ifs) = indWarpFile(ifs) .ge. 0
        controlPts(ifs) = .false.
        if (warping(ifs)) then
          controlPts(ifs) = mod(iflags / 2, 2) .ne. 0
          write(*,'(a,a)')'Old warping file opened: ',trim(gfile(ifs))
        else
c
c           read regular linear xforms
          call dopen(1,gfile(ifs),'ro','f')
          call xfrdall2(1,f(1,1,1,ifs),numXforms(ifs),nflimit,ierr)
          if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
          if (ierr .eq. 1) call exitError('TOO MANY TRANSFORMS IN FILE FOR ARRAY')
          close(1)
        endif
      enddo
c
      if (nfirst .eq. 0) call exitError('NO TRANSFORMS IN FIRST INPUT FILE')
      print *,nfirst,' transforms in first file'
      if (nsecond .eq. 0) call exitError('NO TRANSFORMS IN SECOND INPUT FILE')
      print *,nsecond,' transforms in second file'
      if (nfirst.gt.nflimit .or. nsecond.gt.nflimit)
     &    call exitError('TOO MANY TRANSFORMS FOR ARRAY SIZE')
      if (pipinput) then
        ierr = PipGetInteger('OneXformToMultiply', nsingle)
        ifScales = 1 - PipGetTwoFloats('ScaleShifts', scales(1), scales(2))
      endif
      call PipDone()
c       
c       Check that warpings are the same size except for scaling
      if (warping(1) .and. warping(2)) then
        warpScale = pixelSize(1) / pixelSize(2)
        if (nint(warpScale * nx(1)) .ne. nx(2) .or. nint(warpScale * ny(1)) .ne. ny(2))
     &      call exitError(
     &      'WARPINGS THAT ARE NOT OF THE SAME SIZE IMAGE AREA CANNOT BE MULTIPLIED')
        if (ifScales .ne. 0) then
          if (abs(scales(1) / warpScale - scales(2)) .gt. 1.e-5) call exitError
     &        ('YOU MUST ENTER SCALES THAT KEEP THE WARPED IMAGE AREA THE SAME SIZE')
        else
          scales(1) = warpScale
        endif
      endif
c       
c       Determine maximum number of control points and array sizes
c       Use preliminary assessment of whether there is control point output
      controlPts(3) = (warping(1) .neqv. warping(2)) .and.
     &    (controlPts(1) .or. controlPts(2))
      nxgDim = 0
      nygDim = 0
      do ifs = 1, 2
        maxControl(ifs) = 0
        linearOnly(ifs) = .not. warping(ifs)
        if (warping(ifs)) then
          ierr = setCurrentWarpFile(indWarpFile(ifs))
          xAllStr(ifs) = nx(ifs)
          yAllStr(ifs) = ny(ifs)
          xAllEnd(ifs) = 0.
          yAllEnd(ifs) = 0.
          xAllInt = nx(ifs)
          yAllInt = ny(ifs)
          do iz = 1, numXforms(ifs)
            if (getLinearTransform(iz, f(1,1,iz,ifs)) .ne. 0) call exitError
     &          ('GETTING LINEAR TRANSFORM FROM WARP FILE')
            needGrid = .not. controlPts(ifs)
            if (controlPts(ifs)) then
              if (getNumWarpPoints(iz, nControl) .ne. 0)
     &            call exitError('GETTING NUMBER OF CONTROL POINTS')
              maxControl(ifs) = max(maxControl(ifs), nControl)
              if (nControl .le. 3) then
                if (getWarpPoints(iz, xconTmp, yconTmp, xvecTmp, yvecTmp) .ne. 0)
     &              call exitError('GETTING CONTROL POINTS')
                do i = 1, nControl
                  if (abs(xvecTmp(i)) .gt. 1.e-3 .or. abs(yvecTmp(i)) .gt. 1.e-3)
     &                call exitError('CANNOT WORK WITH 3 OR FEWER CONTROL POINTS THAT'//
     &                ' HAVE NON-ZERO VECTORS')
                enddo
              else if (.not. controlPts(3)) then
                if (gridSizeFromSpacing(iz, -1., -1., 1) .ne. 0) call exitError
     &              ('SETTING GRID SIZE FROM SPACING OF CONTROL POINTS')
                needGrid = .true.
              endif
            endif
            if (needGrid) then
              if (getGridParameters(iz, nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp,
     &            yIntTmp) .ne. 0) call exitError('GETTING GRID PARAMETERS')
              nxgDim = max(nxgDim, nxGrTmp)
              nygDim = max(nygDim, nyGrTmp)
              xAllStr(ifs) = min(xAllStr(ifs), xStrTmp)
              xAllInt = min(xAllInt, xIntTmp)
              xAllEnd(ifs) = max(xAllEnd(ifs), (nxGrTmp - 1) * xIntTmp)
              yAllStr(ifs) = min(yAllStr(ifs), yStrTmp)
              yAllInt = min(yAllInt, yIntTmp)
              yAllEnd(ifs) = max(yAllEnd(ifs), (nyGrTmp - 1) * yIntTmp)
c               if (xAllStr(ifs) .le. 0. .or. xAllEnd(ifs) .ge. nx(ifs) .or. yAllStr(ifs)
c     &            .le. 0. .or. yAllEnd(ifs) .ge. ny(ifs)) print *,'OUTSIDE NOW',
c     &            xAllStr(ifs),xAllEnd(ifs), yAllStr(ifs),yAllEnd(ifs)
            endif
          enddo
          
          if (xAllStr(ifs) .le. 0. .or. xAllEnd(ifs) .ge. nx(ifs) .or.
     &        yAllStr(ifs) .le. 0. .or. yAllEnd(ifs) .ge. ny(ifs)) call exitError(
     &        'CANNOT WORK WITH GRIDS THAT EXTEND OUTSIDE THE DEFINED IMAGE AREA')
          if (xAllStr(ifs) .lt. nx(ifs)) then
            xAllStr(ifs) = min(xAllStr(ifs), xAllInt / 2.)
            xAllEnd(ifs) = max(xAllEnd(ifs), nx(ifs) - xAllInt / 2.)
            yAllStr(ifs) = min(yAllStr(ifs), yAllInt / 2.)
            yAllEnd(ifs) = max(yAllEnd(ifs), ny(ifs) - yAllInt / 2.)
            iz = max(2., (xAllEnd(ifs) - xAllStr(ifs)) / xAllInt + 1.05)
            nxgDim = max(nxgDim, iz)
            iz = max(2., (yAllEnd(ifs) - yAllStr(ifs)) / yAllInt + 1.05)
            nygDim = max(nygDim, iz)
          endif
          linearOnly(ifs) = controlPts(ifs) .and. maxControl(ifs) .le. 3
        endif
      enddo
      linearOnly(3) = linearOnly(1) .and. linearOnly(2)
c       
c       set up output file and allocate arrays
      if (.not. linearOnly(3)) then
        ifs = 2
        if (linearOnly(2)) ifs = 1
        nx(3) = nint(nx(ifs) * scales(ifs))
        ny(3) = nint(ny(ifs) * scales(ifs))
        xcen = nx(3) / 2.
        ycen = ny(3) / 2.
        pixelSize(3) = pixelSize(ifs) / scales(ifs)
        controlPts(3) = controlPts(ifs) .and. (linearOnly(1) .or. linearOnly(2))
        iflags = 1
        if (controlPts(3)) iflags = 3
        indWarpFile(3) = newWarpFile(nx(3), ny(3), ibin, pixelSize(3), iflags)
        if (indWarpFile(3) .lt. 0) call exitError('OPENING A NEW WARPING FILE')

        if (controlPts(3)) then
          iz = max(maxControl(1), maxControl(2))
          allocate(xControl(iz), yControl(iz), xVector(iz), yVector(iz), stat = ierr)
        else
          allocate(dxGrid(nxgDim,nygDim,3), dyGrid(nxgDim,nygDim,3), stat = ierr)
        endif
        call memoryError(ierr, 'ARRAYS FOR WARPING DATA')
      else
        call dopen(3,outfile,'new','f')
      endif

c       
c       Determine how to sample the transforms when multiplying
      nout=min(nfirst,nsecond)
      if (nsecond.ne.nfirst)then
        if(nsecond.eq.1)then
          if (nsingle .ge. 0 .and. nsingle .lt. nfirst) then
            print *,'Single second transform applied to first transform #', nsingle
          else
            print *,'Single second transform applied to all first transforms'
          endif
          nout=nfirst
        elseif(nfirst.eq.1)then
          if (nsingle .ge. 0 .and. nsingle .lt. nsecond) then
            print *,'Single first transform applied to second transform #', nsingle
          else
            print *,'Single first transform applied to all second transforms'
          endif
          nout=nsecond
        else
          print *,'WARNING: XFPRODUCT - Number of transforms does not match'
        endif
      endif
c
c       Loop on the output
      do iz = 1, nout
c         
c         Set up indexes to take first and second from and copy index for copy
        inds(1) = iz
        inds(2) = iz
        indcopy = 0
        if (nsecond .ne. nfirst) then
          if(nsecond.eq.1)then
            inds(2) = 1
            if (nsingle .ge. 0 .and. nsingle .lt. nfirst .and. nsingle .ne. iz - 1)
     &          indcopy = 1
          elseif(nfirst.eq.1)then
            inds(1) = 1
            if (nsingle .ge. 0 .and. nsingle .lt. nsecond .and. nsingle .ne. iz - 1)
     &          indcopy = 2
          endif
        endif
c         
c         Get the transforms and scale them to the output scale
        do ifs = 1, 2
          call xfcopy(f(1,1,inds(ifs),ifs), ftmp(1,1,ifs))
          ftmp(1,3,ifs) = ftmp(1,3,ifs) * scales(ifs)
          ftmp(2,3,ifs) = ftmp(2,3,ifs) * scales(ifs)
          linear(ifs) = linearOnly(ifs)
          if (.not. linear(ifs)) then
            if (controlPts(ifs)) then
              ierr = setCurrentWarpFile(indWarpFile(ifs))
              ierr = getNumWarpPoints(inds(ifs), nControl)
              linear(ifs) = nControl .le. 3
c              print *,iz,ifs,inds(ifs),nControl, linear(ifs)
            endif
          endif
        enddo
        if (linear(1) .and. linear(2)) then
c           
c           If both are linear, do ordinary product or copy, then write or put in file
          if (indcopy .gt. 0) then
            call xfcopy(ftmp(1,1,indcopy), ftmp(1,1,3))
          else
            call xfmult(ftmp(1,1,1),ftmp(1,1,2),ftmp(1,1,3))
          endif
          if (linearOnly(3)) then
            call xfwrite(3,ftmp(1,1,3),*94)
          else
            ierr = setCurrentWarpFile(indWarpFile(3))
            if (setLinearTransform(iz, ftmp(1,1,3)) .ne. 0) call exitError(
     &          'ADDING LINEAR TRANSFORM TO NEW WARPING')
            if (.not. controlPts(3)) then
c               
c               Output a zero grid
              nxGrids(2) = min(9, nxgDim)
              nyGrids(2) = min(9, nygDim)
              xIntervals(2) = nx(3) / nxGrids(2)
              xStarts(2) = xIntervals(2) / 2.
              yIntervals(2) = ny(3) / nyGrids(2)
              yStarts(2) = yIntervals(2) / 2.
              dxGrid(1:nxGrids(2),1:nyGrids(2),2) = 0.
              dyGrid(1:nxGrids(2),1:nyGrids(2),2) = 0.
              if (setWarpGrid(iz, nxGrids(2), nyGrids(2), xStarts(2), yStarts(2),
     &            xIntervals(2), yIntervals(2), dxGrid(1,1,2), dyGrid(1,1,2),
     &            nxgDim) .ne. 0) call exitError('STORING ZERO WARPING GRID')
            endif
          endif
        else
c           
c           Warping involved somewhere: get data for first and second in turn
          do ifs = 1, 2
            if (linear(ifs) .or. (indcopy .gt. 0 .and. indcopy .ne. ifs)) cycle
            ierr = setCurrentWarpFile(indWarpFile(ifs))
            if (controlPts(ifs)) then
c               
c               If there are control points and we are writing control points, get them
              if (controlPts(3)) then
                ierr = getNumWarpPoints(inds(ifs), nControl)
c                print *,'Getting control', ierr, nControl,inds(ifs), indWarpFile(ifs)
                if (getWarpPoints(inds(ifs), xControl, yControl, xVector, yVector) .ne. 0)
     &              call exitError('GETTING WARP CONTROL POINTS')
c                 
c                 Scale all components
                xControl(1:nControl) = xControl(1:nControl) * scales(ifs)
                yControl(1:nControl) = yControl(1:nControl) * scales(ifs)
                xVector(1:nControl) = xVector(1:nControl) * scales(ifs)
                yVector(1:nControl) = yVector(1:nControl) * scales(ifs)
              else
c                 
c                 Otherwise set up to get a grid
                if (gridSizeFromSpacing(inds(ifs), -1., -1., 1) .ne. 0) call exitError
     &              ('SETTING GRID SIZE FROM SPACING OF CONTROL POINTS')
              endif
            endif
            if (.not. controlPts(3)) then
c               
c               Now get a grid and scale it
              if (getWarpGrid(inds(ifs), nxGrids(ifs), nyGrids(ifs), xStarts(ifs),
     &            yStarts(ifs), xIntervals(ifs), yIntervals(ifs), dxGrid(1,1,ifs),
     &            dyGrid(1,1,ifs), nxgDim) .ne. 0) call exitError('GETTING A WARP GRID')
              if (.not. (linear(1) .or. controlPts(ifs) .or. indcopy .ne. 0)) then
                if (expandAndExtrapGrid(dxGrid(1,1,ifs), dyGrid(1,1,ifs), nxgDim, nygDim,
     &              nxGrids(ifs), nyGrids(ifs), xStarts(ifs),yStarts(ifs),
     &              xIntervals(ifs), yIntervals(ifs),  xAllStr(ifs), yAllStr(ifs),
     &              xAllEnd(ifs), yAllEnd(ifs), 0, nx(ifs), 0, ny(ifs)) .ne. 0)
     &              call exitError('EXPANDING A WARP GRID')
              endif
              dxGrid(1:nxGrids(ifs), 1:nyGrids(ifs), ifs) =
     &            dxGrid(1:nxGrids(ifs), 1:nyGrids(ifs), ifs) * scales(ifs)
              dyGrid(1:nxGrids(ifs), 1:nyGrids(ifs), ifs) =
     &            dyGrid(1:nxGrids(ifs), 1:nyGrids(ifs), ifs) * scales(ifs)
              xStarts(ifs) = xStarts(ifs) * scales(ifs)
              yStarts(ifs) = yStarts(ifs) * scales(ifs)
              xIntervals(ifs) = xIntervals(ifs) * scales(ifs)
              yIntervals(ifs) = yIntervals(ifs) * scales(ifs)
            endif
          enddo
c           
c           Now do something unless copying
          ifUse2nd = -1
          if (indcopy .eq. 0) then
            if (linear(1)) then
c             
c               If first one is linear, multiply transforms and set up to copy the warp
              call xfmult(ftmp(1,1,1),ftmp(1,1,2),ftmp(1,1,2))
              indcopy = 2
            elseif (linear(2) .and. controlPts(3)) then
c               
c               If second one is linear and first one is still control points, transform
c               them 
              do j = 1, nControl
                call xfapply(ftmp(1,1,2), xcen, ycen, xControl(j), yControl(j), xNewCont,
     &              yNewCont)
                call xfapply(ftmp(1,1,2), xcen, ycen, xControl(j) + xVector(j),
     &              yControl(j) + yVector(j), xNewCpv, yNewCpv)
                xControl(j) = xNewCont
                yControl(j) = yNewCont
                xVector(j) = xNewCpv - xNewCont
                yVector(j) = yNewCpv - yNewCont
              enddo
              call xfmult(ftmp(1,1,1),ftmp(1,1,2),ftmp(1,1,2))
              indcopy = 2
            else
c               
c               multiplication of two grids: make new grid the one with smaller interval
              ifUse2nd = 0
              if (linear(2)) then
                nxGrids(2) = 0
                nyGrids(2) = 0
              else
                if (xIntervals(2) * yIntervals(2) .lt. xIntervals(1) * yIntervals(1))
     &              ifUse2nd = 1
              endif
              if (multiplyWarpings(dxGrid(1,1,1), dyGrid(1,1,1), nxgDim, nxGrids(1),
     &            nyGrids(1), xStarts(1), yStarts(1), xIntervals(1), yIntervals(1), 
     &            ftmp(1,1,1), xcen, ycen, dxGrid(1,1,2), dyGrid(1,1,2), nxgDim,
     &            nxGrids(2), nyGrids(2), xStarts(2), yStarts(2), xIntervals(2), 
     &            yIntervals(2), ftmp(1,1,2), dxGrid(1,1,3), dyGrid(1,1,3), ftmp(1,1,3),
     &            ifUse2nd) .ne. 0) call exitError('MULTIPLYING WARPINGS')
              indcopy = 3
            endif
          endif
c           
c           Put out the warping now indicated by indcopy
          ierr = setCurrentWarpFile(indWarpFile(3))
          if (setLinearTransform(iz, ftmp(1,1,indcopy)) .ne. 0) call exitError(
     &        'ADDING LINEAR TRANSFORM TO NEW WARPING')
c          print *,'putting out',controlPts(3), nControl
          if (controlPts(3)) then
            if (setWarpPoints(iz, nControl, xControl, yControl, xVector, yVector) .ne. 0)
     &          call exitError('STORING NEW WARP CONTROL POINTS')
          else
            i = ifUse2nd + 1
            if (i .eq. 0) i = indcopy
            if (setWarpGrid(iz, nxGrids(i), nyGrids(i), xStarts(i), yStarts(i),
     &          xIntervals(i), yIntervals(i), dxGrid(1,1,indcopy), dyGrid(1,1,indcopy),
     &          nxgDim) .ne. 0) call exitError('STORING NEW WARPING GRID')
          endif
        endif
      enddo
      if (linearOnly(3)) then
        close(3)
        print *,nout,' new transforms written'
      else
        if (writeWarpFile(outfile, 0) .ne. 0) call exitError('WRITING NEW WARPING FILE')
        write(*,'(i5,a,a)')nout,' new transforms written to warping file: ', trim(outfile)
      endif
      call exit(0)
c       
94    call exitError('WRITING OUT NEW TRANSFORM FILE')
      end

c       
c       $Log$
c       Revision 3.8  2011/06/27 01:47:58  mast
c       Make sure maximum dimension is big enough for actual grids
c
c       Revision 3.7  2011/06/23 14:57:05  mast
c       Reorder arguments to calls for consistency
c
c       Revision 3.6  2011/06/17 04:08:21  mast
c       Switched to new routines for warping
c
c       Revision 3.5  2011/06/10 04:10:11  mast
c       Added warping
c
c       Revision 3.4  2008/11/02 14:03:20  mast
c       Added option to multiply by one transform in multiple transform list
c
c       Revision 3.3  2005/10/13 00:54:03  mast
c       Generate error when files are empty and WARNING if they don't match
c	
c       Revision 3.2  2004/01/27 03:34:04  mast
c       Convert to PIP input and add option for scaling translations
c	
c       Revision 3.1  2003/08/02 22:32:14  mast
c       Standardize error output and exit
c	

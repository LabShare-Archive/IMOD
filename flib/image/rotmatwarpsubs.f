c       rotmatwarpsubs.f  - contains subroutines for Rotatevol, Matchvol, and
c       Warpvol
c       
c       $Id$
c
c       SETUP_CUBES_SCRATCH determines the needed size of the scratch files
c       opens the scratch files, and sets up tables for which cubes use
c       which file
c       
      subroutine setup_cubes_scratch(mfor, aloc, nloctot, nExtra, filein,
     &    tempdir, tempext, tim, warping)
      use rotmatwarp
      implicit none
      real*4 mfor(3,3), aloc(3,3,*)
      character*(*) filein
      character*(*) tempdir
      character*(*) tempext
      character*(*) tim
      logical*4 warping
      integer*4 nxyzcubas(3),nxyzscr(3),nbigcube(3),nExtra, nloctot
      integer*4 i, ind, j, ix, iy,iz,inmin(3),inmax(3),ival,nzExtra
      character*320 temp_filename
      character*320 tempname
      real*8 memSize8
      integer*4 iyAxis, izAxis, minYZsize
      real*4 aspect, efficiency, cubeEfficiency,xcen,ycen,zcen
      real*4 aspectMax,relEfficiencyCrit, delAspect
c       
c       Parameters controlling aspect ratio limit and search
      aspectMax = 4.
      relEfficiencyCrit = 0.9
      minYZsize = 200
      delAspect = 0.2

      memSize8 = memoryLim
      memSize8 = memSize8 * 1024. * 1024. / 4.
c       
c       Find dominant axis for output from elongation of input along X 
c       Direction turns out to be counterproductive, so it is not used
      idirXaxis = 1
      if (abs(mfor(2,1)) .ge. abs(mfor(1,1)) .and. abs(mfor(2,1)) .ge.
     &    abs(mfor(3,1))) then
        ioutXaxis = 2
        if (mfor(2,1) .lt. 0) idirXaxis = -1
        if (abs(mfor(1,2)) .ge. abs(mfor(2,2)) .and. abs(mfor(1,2)) .ge.
     &      abs(mfor(3,2))) then
          ioutYaxis = 1
          ioutZaxis = 3
        else
          ioutYaxis = 3
          ioutZaxis = 1
        endif
      else if(abs(mfor(3,1)) .ge. abs(mfor(1,1)) .and. abs(mfor(3,1)) .ge.
     &    abs(mfor(2,1))) then
        ioutXaxis = 3
        if (mfor(3,1) .lt. 0) idirXaxis = -1
        if (abs(mfor(2,2)) .ge. abs(mfor(1,2)) .and. abs(mfor(2,2)) .ge.
     &      abs(mfor(3,2))) then
          ioutYaxis = 2
          ioutZaxis = 1
        else
          ioutYaxis = 1
          ioutZaxis = 2
        endif
      else
        ioutXaxis = 1
        if (mfor(1,1) .lt. 0) idirXaxis = -1
        if (abs(mfor(2,2)) .ge. abs(mfor(1,2)) .and. abs(mfor(2,2)) .ge.
     &      abs(mfor(3,2))) then
          ioutYaxis = 2
          ioutZaxis = 3
        else
          ioutYaxis = 3
          ioutZaxis = 2
        endif
      endif
      iyAxis = mod(ioutXaxis, 3) + 1
      izAxis = mod(ioutXaxis + 1, 3) + 1
c       
c       Do not use multiple slices if 1) Not warping and outer axis = Z or
c       2) warping and inner axis is not Z
      if ((.not.warping .and. ioutZaxis .eq. 3) .or.
     &    (warping .and. ioutXaxis .ne. 3)) maxZout = 1
c       
c       Loop on increasing aspect ratios along that axis
      aspect = 1.
      do
        call findSizesForAspect()
c         
c         first time, record efficiency of cube 
        if (aspect .eq. 1) cubeEfficiency = efficiency
c
c         If there is only one cube on the output of the X axis, use this ratio
        if (ncubes(ioutXaxis) .eq. 1) exit
c
c         If the efficiency has fallen too far, or if the aspect ratio is above
c         a limit AND one of the other axes has become too small, back up to
c         last aspect ratio (if any) and use that one
        if (efficiency / cubeEfficiency .lt. relEfficiencyCrit .or.
     &      (aspect .gt. aspectMax .and.
     &      ((ncubes(iyAxis) .gt. 1 .and. nxyzcubas(iyAxis) .lt. minYZsize).or.
     &      (ncubes(izAxis) .gt. 1 .and. nxyzcubas(izAxis) .lt. minYZsize))))
     &      then
          if (iVerbose .gt. 0) print *,'Efficiency ratio', efficiency / cubeEfficiency
          if (aspect .gt. 1.) then
            aspect = aspect - delAspect
            call findSizesForAspect()
          endif
          exit
        endif
c         
c         Or, loop on next value of aspect ratio
        aspect = aspect + delAspect
      enddo
c       
c       Compute true needed input dimensions now that cube sizes are known
c       REPLACE inpDim rather than adjusting it downward, it can be too small
c       But don't let it get any bigger than actual file size
      inmin=1000000
      inmax=-1000000
      do j = 1, nloctot
        do ix=0,1
          do iy=0,1
            do iz=0,1
              xcen=ix*(nxyzcubas(1) + 1.)
              ycen=iy*(nxyzcubas(2) + 1.)
              zcen=iz*(nxyzcubas(3) + 1.)
              do i=1,3
                ival=nint(aloc(i,1,j)*xcen+aloc(i,2,j)*ycen+ aloc(i,3,j)*zcen)
c                 Adding 2 instead of 1 may be overkill but gives extra safety
                inmin(i)=min(inmin(i), ival - 2)
                inmax(i)=max(inmax(i), ival + 2)
              enddo
            enddo
          enddo
        enddo
      enddo
      do i = 1, 3
        inpDim(i) = min(nxyzin(i), nExtra + inmax(i) + 1 - inmin(i))
      enddo
      if (iVerbose .gt. 0)
     &    write(*,'(a,3i6)')'Input size adjusted for actual cubes:', inpDim
      arraySize8 = inpDim(1)
      arraySize8 = (arraySize8 * inpDim(2)) * inpDim(3)
      nzExtra = 0
c       
c       now compute sizes of nearly equal sized near cubes to fill output
c       volume, store the starting index coordinates
c       
      do i=1,3
        if(ncubes(i).gt.lmcube) then
          if (filein .ne. ' ' .or. nExtra .lt. 100) call exiterror(
     &        'TOO MANY CUBES IN LONGEST DIRECTION TO FIT IN ARRAYS')
          call exiterror('TOO MANY CUBES FOR ARRAYS, CHECK FOR WILD '//
     &        'VECTORS/BIG JUMPS BETWEEN TRANSFORMS')
        endif

        nbigcube(i)=mod(nxyzout(i),ncubes(i))
        ind=0
        do j=1,ncubes(i)
          ixyzcube(i,j)=ind
          nxyzcube(i,j)=nxyzcubas(i)
          if(j.le.nbigcube(i))nxyzcube(i,j)=nxyzcube(i,j)+1
          ind=ind+nxyzcube(i,j)
        enddo
        nxyzscr(i)=nxyzcubas(i)+1
      enddo
c       
c       This was originally a test that could fail - and it failed for nz = 1, so
c       increase the number of planes allocated as needed to hold output row
      do while (nxout * (nxyzcubas(2) + 1.) .ge. arraySize8)
        nzExtra = nzExtra + 1
        arraySize8 = inpDim(1)
        arraySize8 = (arraySize8 * inpDim(2)) * (inpDim(3) + nzExtra)
        if (arraySize8 .gt. 2 * memSize8) then
          print *,inpdim, nxout, nxyzcubas(2), nxout * (nxyzcubas(2) + 1.),
     &        arraySize8,nzExtra,memSize8
          call exiterror('SOMETHING IS WRONG TRYING TO MAKE ARRAY BIG ENOUGH FOR OUTPUT')
        endif
      enddo
c
      if (filein .eq. ' ') return
c       
c       Allocate memory
      allocate (array(inpdim(1),inpdim(2),inpdim(3)+nzExtra),
     &    brray(idimOut(1)*idimOut(2)*maxZout),
     &    ifile(ncubes(1), ncubes(2)),
     &    izinfile(ncubes(1), ncubes(2), nxyzscr(3)), stat = j)
      if (j .ne. 0) call exitError('FAILED TO ALLOCATE MEMORY FOR ARRAYS')
c       
c       Set limits of inner and outer loops depending on axis
      if (ioutXaxis .eq. 1) then
        limInner = ncubes(1)
        limOuter = ncubes(2)
      else
        limInner = ncubes(2)
        limOuter = ncubes(1)
      endif
c       
c       get an array of file numbers for each cube in X/Y plane
c       If only one cube in plane, set to final output file
      needScratch = .false.
      do ix=1,ncubes(1)
        do iy=1,ncubes(2)
          ifile(ix,iy)=1
          if(ix.gt.nbigcube(1))ifile(ix,iy)=ifile(ix,iy)+1
          if(iy.gt.nbigcube(2))ifile(ix,iy)=ifile(ix,iy)+2
          if (ncubes(1) * ncubes(2) .eq. 1) then
            ifile(ix,iy)=6
            print *,'Writing directly to output file'
          else
            needScratch(ifile(ix,iy)) = .true.
          endif
        enddo
      enddo

      write(*,103)ncubes(3),ncubes(1),ncubes(2)
103   format(' Rotations done in',i3,' layers, with',i3,' by',i3,
     &    ' cubes in each layer')
c       
c       If needed, open scratch files with 4 different sizes.
c       compose temporary filenames from the time
c       
      tempext(4:5)=tim(1:2)
      tempext(6:7)=tim(4:5)
      tempext(8:9)=tim(7:8)
      tempname=temp_filename(filein,tempdir,tempext)
c       
      nxyzscr(3)=nxyzscr(3)*ncubes(1)*ncubes(2)
      call ialprt(.false.)
      if (needScratch(1)) then
        call imopen(1,tempname,'scratch')
        call icrhdr(1,nxyzscr,nxyzscr,mode,title,0)
      endif
c       
      tempext(10:10)='2'
      tempname=temp_filename(filein,tempdir,tempext)
      nxyzscr(1)=nxyzscr(1)-1
      if (needScratch(2)) then
        call imopen(2,tempname,'scratch')
        call icrhdr(2,nxyzscr,nxyzscr,mode,title,0)
      endif
c       
      tempext(10:10)='4'
      tempname=temp_filename(filein,tempdir,tempext)
      nxyzscr(2)=nxyzscr(2)-1
      if (needScratch(4)) then
        call imopen(4,tempname,'scratch')
        call icrhdr(4,nxyzscr,nxyzscr,mode,title,0)
      endif
c       
      tempext(10:10)='3'
      tempname=temp_filename(filein,tempdir,tempext)
      nxyzscr(1)=nxyzscr(1)+1
      if (needScratch(3)) then
        call imopen(3,tempname,'scratch')
        call icrhdr(3,nxyzscr,nxyzscr,mode,title,0)
      endif
      return

c       
c       INTERNAL routine FINDSIZESFORASPECT to find sizes of input/output
c       given an aspect ratio, adjusting memory as needed for output slices

      contains
      subroutine findSizesForAspect()
      real*4 devmax(3), devfac(3), expandFac
      integer*4 numOver, indUnder, indOver, numIterReduce, iterReduce
      real*8 memReduced
c       
c       Find the maximum deviation in each direction of input required to
c       output an elongated cube
      devmax = 0.
      devfac = 1.
      devfac(ioutXaxis) = aspect
      do j=1,nloctot
        do ix=-1,1,2
          do iy=-1,1,2
            do i=1,3
              devmax(i)=max(devmax(i), abs(aloc(i,1,j) * ix * devfac(1) +
     &            aloc(i,2,j) * iy * devfac(2) + aloc(i,3,j) * devfac(3)))
            enddo
          enddo
        enddo
      enddo

c       Get the fraction of input data used in output
      efficiency = aspect / (devmax(1) * devmax(2) * devmax(3))
      if (iVerbose .gt. 0) 
     &    write(*,'(a,f6.2,a,f7.4,a,3f10.5)')'Aspect:',aspect,' efficiency'
     &    ,efficiency,' deviations:',devmax
c       
c       Find factor by which input can be expanded to use up the memory
c       But first get an initial estimate of the output size to allow a 
c       reduction for memory needed for the multiple slices
c       of output to be stored; set up to iterate with better output size
      numIterReduce = 1
      memReduced = memSize8
      if (maxZout .gt. 1) then
        expandFac = (memSize8 / (devmax(1) * devmax(2) * devmax(3)))**0.333333
        idimOut(1) = min(nxyzout(1), int(devFac(1) * expandFac))
        idimOut(2) = min(nxyzout(2), int(devFac(2) * expandFac))
        numIterReduce = 3
      endif
c       
c       Iterate to stabilize memory division between input and output
      do iterReduce = 1, numIterReduce
        if (maxZout .gt. 1) then
c           
c           Reduce by the amount needed for output, but do not decrease the
c           reduction from previous time, and keep it to half the total
          memReduced = max(memSize8 / 2., min(memReduced,
     &        memSize8 - float(maxZout) * idimOut(1) * idimOut(2)))
          if (iVerbose .gt. 0) write(*,'(a,i3,a,i7,a)')'Iteration',iterReduce,
     &        ': for multiple output slices, input memory reduced to',
     &        nint(memReduced/(1024.*256.)), ' MB'
        endif
c       
c         get expansion factor that would use up the memory
c         and get trial size for equal expansion
        expandFac = (memReduced / (devmax(1) *devmax(2) * devmax(3)))**0.333333
        inpDim = devmax * expandFac + 1.
c         
c         Count axes where this is big enough for whole volume, and keep track
c         of at least one axis where it is and is not
        numOver = 0
        indUnder = 1
        indOver = 1
        do i = 1,3
          if (inpDim(i) .ge. nxyzin(i)) then
            numOver = numOver + 1
            indOver = i
          else
            indUnder = i
          endif
        enddo
        if (iVerbose .gt. 0)  write(*,'(a,f9.2,3i8,a,i3)')'Initial fac',
     &      expandFac,inpdim,' numover', numOver
c         
c         If this is oversized in one axis, expand the other two axes and
c         re-evaluate if they are over
        if (numOver .eq. 1 .and. inpDim(indOver) .gt. nxyzin(indOver)) then
          expandFac = expandFac * sqrt(real(inpDim(indOver)) /nxyzin(indOver))
          inpDim(indOver) = nxyzin(indOver)
          do i = 0, 1
            ix = mod(indOver + i, 3) + 1
            inpDim(ix) = devmax(ix) * expandFac + 1.
            if (inpDim(ix) .ge. nxyzin(ix)) then
              numOver = numOver + 1
            else
              indUnder = ix
            endif
          enddo
          if (iVerbose .gt. 0) write(*,'(a,f9.2,3i8,a,i3)')'Expand 2 axes',
     &        expandFac,inpdim, ' numover', numOver
        endif
c       
c         Now if it is oversized in two axes, expand the remaining axis
        if (numOver .eq. 2) then
          ix = mod(indUnder, 3) + 1
          iy = mod(indUnder + 1, 3) + 1
          inpDim(ix) = nxyzin(ix)
          inpDim(iy) = nxyzin(iy)
          expandFac = ((memReduced / nxyzin(ix)) / nxyzin(iy)) / devmax(indUnder)
          inpDim(indUnder) = devmax(indUnder) * expandFac + 1.
          if (inpDim(indUnder) .ge. nxyzin(indUnder)) numOver = 3
          if (iVerbose .gt. 0) write(*,'(a,f9.2,3i8,a,i3)')'Expand 1 axis',
     &        expandFac,inpdim,' numover', numOver
        endif
c         
c         If over in all axes, set up input and output sizes
        if (numOver .eq. 3) then
          inpDim = nxyzin
          idimOut = nxyzOut
        else
c         
c           Otherwise, set up the output sizes by expanding the elongated cube
          do ix = 1, 3
            idimOut(ix) = min(nxyzout(ix), int(devFac(ix) * expandFac) - 2 *
     &          nExtra - 2)
          enddo
        endif
c       
c         Get number of cubes and basic cube size on each axis, adjust out size
        ncubes = (nxyzout - 1) / idimOut + 1
        nxyzcubas = nxyzout / ncubes
        if (iVerbose .gt. 0) write(*,'(a,3i6,a,3i4,a,3i6)')'Initial out',
     &      idimOut,'  # cubes',ncubes, '  size',nxyzcubas
        idimOut = nxyzcubas + 1
      enddo
      end subroutine findSizesForAspect

      end subroutine setup_cubes_scratch


c       
c       TRANSFORM_CUBES does the complete work of looping on all of the
c       cubes, transforming them, writing them to files, and calling the
c       routine to reassemble the output file.
c       It is used by Rotatevol and Matchvol
c       
      subroutine transform_cubes(interpOrder)
      use rotmatwarp
      implicit none
      integer*4 interpOrder
      integer*4 izsec(6)
      integer*4 inmin(3),inmax(3), icube(3)
      integer*4 ixcube, iycube, izcube, i, ifx, ify, ifz, ival, ifempty
      real*4 xcen, ycen, zcen, xofsout, xofsin, yofsin, zofsin, xp, yp, zp
      integer*4 ixlim, iylim, izlim, ixp, iyp, izp, ixpp1, ixpm1
      real*4 bval, dx, dy, dz, v2, v4, v5, v6, v8, vu, vd, vmin, vmax
      integer*4 iypp1, iypm1, izpp1, izpm1, iunit, iy, ix, iz, numDone
      real*4 a, b, c, d, e, f, dmin, dmax, tsum, dmean, tmin,tmax,tmean
      real*4 xpofs, ypofs, zpofs, d11, d12, d21, d22
      integer*4 indInner, indOuter, izStart, numZleft, numZtoDo, izEnd
      integer*4 innerStart,innerEnd,middleStart,middleEnd,iouterStart,iouterEnd
      integer*4 istride(3), innerStride, middleStride, iouterStride,indBase
      integer*4 iouter,middle, inner
      real*4 cenouter, cenMid, cenInner
      real*8 walltime, wallStart, wallCum
c       
      dmin=1.e20
      dmax=-dmin
      tsum=0.
      numDone = 0
      wallCum = 0.
      iStride(1) = 1
      istride(2) = idimOut(1)
      istride(3) = idimOut(1) * idimOut(2)
      innerStride = istride(ioutXaxis)
      middleStride = istride(ioutYaxis)
      iouterStride = istride(ioutZaxis)
c      print *,'strides',istride
c       
c       loop on layers of cubes in Z, do all I/O to complete layer
c       
      izsec(6) = 0
      do izcube=1,ncubes(3)
        icube(3) = izcube
c         
c         initialize files and counters
c         
        do i=1,4
          if (needScratch(i)) call imposn(i,0,0)
          izsec(i)=0
        enddo
c         
c         loop on the cubes in the layer
c         
        do indOuter = 1, limOuter
          do indInner = 1, limInner
            call cubeIndexes(ioutXaxis, indInner, indOuter, ixcube, iycube)
            icube(1) = ixcube
            icube(2) = iycube
c             
c             back-transform the corner coordinates of the output cube to
c             find the limiting index coordinates of the input cube
c             
            do i=1,3
              inmin(i)=1000000
              inmax(i)=-1000000
            enddo
            do ifx=0,1
              do ify=0,1
                do ifz=0,1
                  xcen=ixyzcube(1,ixcube)+ifx*nxyzcube(1,ixcube)-cxout
                  ycen=ixyzcube(2,iycube)+ify*nxyzcube(2,iycube)-cyout
                  zcen=ixyzcube(3,izcube)+ifz*nxyzcube(3,izcube)-czout
                  do i=1,3
                    ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &                  minv(i,3)*zcen+cxyzin(i))
                    inmin(i)=max(0,min(inmin(i),ival-1))
                    inmax(i)=min(nxyzin(i)-1,max(inmax(i),ival+1))
                  enddo
                enddo
              enddo
            enddo
            ifempty=0
            do i=1,3
              if(inmin(i).gt.inmax(i))ifempty=1
              if (inmax(i) + 1 - inmin(i) .gt. inpDim(i)) then
                write(*,'(/,a,i2,3i6)')'ERROR: TRANSFORM_CUBES - INPUT '//
     &              'DATA LARGER THAN CALCULATED SIZE:', i, inpDim(i),
     &              inmin(i),inmax(i)
                call exit(1)
              endif
            enddo
c             
c             load the input cube
c             
            if(ifempty.eq.0)then
              do iz=inmin(3),inmax(3)
                call imposn(5,iz,0)
                if (iVerbose .gt. 1) 
     &              write(*,'(10i5)')ixcube,iycube,izcube,iz,iz+1-inmin(3)
     &              ,inmin(1), inmax(1),inmin(2),inmax(2)
                call irdpas(5,array(1,1,iz+1-inmin(3)),inpdim(1),inpdim(2),
     &              inmin(1),inmax(1),inmin(2),inmax(2),*99)
              enddo
            endif
c             
c             prepare offsets and limits
c             
            xofsout=ixyzcube(ioutXaxis, icube(ioutXaxis))-1- cxyzout(ioutXaxis)
            xofsin=cxin+1-inmin(1)
            yofsin=cyin+1-inmin(2)
            zofsin=czin+1-inmin(3)
            ixlim=inmax(1)+1-inmin(1)
            iylim=inmax(2)+1-inmin(2)
            izlim=inmax(3)+1-inmin(3)
c             
c             loop over Z segments of the output cube
            izStart = 1
            numZleft = nxyzcube(3,izcube)
            do while (numZleft .gt. 0)
              numZtoDo = min(numZleft, maxZout)
              izEnd = izStart + numZtoDo - 1
c              print *,ixcube,iycube,izStart,izEnd,numZleft
              if(ifempty.eq.0)then
                
c                 Set up index limits for inner loop
                if (ioutXaxis .eq. 3) then
                  innerStart = izStart
                  innerEnd = izEnd
                else
                  innerStart = 1
                  innerEnd = nxyzcube(ioutXaxis, icube(ioutXaxis))
                endif
c                 
c                 Set up limits for middle loop
                if (ioutYaxis .eq. 3) then
                  middleStart = izStart
                  middleEnd = izEnd
                else
                  middleStart = 1
                  middleEnd = nxyzcube(ioutYaxis, icube(ioutYaxis))
                endif
c                 
c                 Set up limits for outer loop
                if (ioutZaxis .eq. 3) then
                  iouterStart = izStart
                  iouterEnd = izEnd
                else
                  iouterStart = 1
                  iouterEnd = nxyzcube(ioutZaxis, icube(ioutZaxis))
                endif
                indBase = -idimOut(1) - izStart * idimOut(1) * idimOut(2)
c                print *,indBase
c                print *,iouterStart, iouterEnd,middleStart, middleEnd,innerStart,innerEnd
                wallStart = walltime()
c                 
c                 Start looping over outer axis of this Z segment; parallelize
c                 loop over middle and inner axes
                do iouter = iouterStart, iouterEnd
                  cenouter=ixyzcube(ioutZaxis,icube(ioutZaxis))+iouter-1-
     &                cxyzout(ioutZaxis)
C$OMP PARALLEL DO
C$OMP& SHARED(nxyzcube, ixyzcube, minv, xofsin, yofsin, zofsin)
C$OMP& SHARED(icube,cxyzout,cenouter,iouter,indBase,middleStart, middleEnd)
C$OMP& SHARED(interporder, DMEANIN, array, brray,ixlim,iylim,izlim)
C$OMP& SHARED(xofsout,innerStart,innerEnd,ioutXaxis,ioutYaxis)
C$OMP& SHARED(ioutZaxis,innerStride,middleStride,iouterStride)
c$omp& DEFAULT(PRIVATE)
                  do middle = middleStart, middleEnd
                    cenmid = ixyzcube(ioutYaxis, icube(ioutYaxis)) + middle - 1
     &                  - cxyzout(ioutYaxis)
                    xpofs = minv(1,ioutYaxis)*cenmid+minv(1,ioutZaxis)*cenouter
     &                  + xofsin
                    ypofs = minv(2,ioutYaxis)*cenmid+minv(2,ioutZaxis)*cenouter
     &                  + yofsin
                    zpofs = minv(3,ioutYaxis)*cenmid+minv(3,ioutZaxis)*cenouter
     &                  + zofsin
                    if (interpOrder .ge. 2) then
                      do inner=innerStart,innerEnd
                        cenInner=inner+xofsout
c                       
c                         get indices in array of input data
c                         
                        xp=minv(1,ioutXaxis)*cenInner+xpofs
                        yp=minv(2,ioutXaxis)*cenInner+ypofs
                        zp=minv(3,ioutXaxis)*cenInner+zpofs
                        bval = DMEANIN
c                       
c                         do triquadratic interpolation with higher-order
c                         terms omitted
c                         
                        ixp=nint(xp)
                        iyp=nint(yp)
                        izp=nint(zp)
                        IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &                      IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &                      IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
                          dx=xp-ixp
                          dy=yp-iyp
                          dz=zp-izp
                          ixpp1=min(ixlim,ixp+1)
                          iypp1=min(iylim,iyp+1)
                          izpp1=min(izlim,izp+1)
                          ixpm1=max(1,ixp-1)
                          iypm1=max(1,iyp-1)
                          izpm1=max(1,izp-1)
C                           
C                           Set up terms for quadratic interpolation
C                           
                          V2 = ARRAY(IXP, IYPM1, IZP)
                          V4 = ARRAY(IXPM1, IYP, IZP)
                          V5 = ARRAY(IXP, IYP, IZP)
                          V6 = ARRAY(IXPP1, IYP, IZP)
                          V8 = ARRAY(IXP, IYPP1, IZP)
                          VU = ARRAY(IXP, IYP, IZPP1)
                          VD = ARRAY(IXP, IYP, IZPM1)
                          vmax=max(v2,v4,v5,v6,v8,vu,vd)
                          vmin=min(v2,v4,v5,v6,v8,vu,vd)
C                           
                          C = (V6 - V4)*.5
                          A = C + V4 - V5
                          D = (V8 - V2)*.5
                          B = D + V2 - V5
                          F = (VU - VD)*.5
                          E = F + VD - V5
                          bval = (a*dx+c)*dx + (b*dy+d)*dy + (e*dz+f)*dz + v5
                          bval=min(vmax,max(vmin,bval))
                        endif
                        brray(indBase + inner * innerStride + middle *
     &                      middleStride + iouter * iouterStride) = bval
                      enddo
                    else
c                     
c                       linear interpolation
c                       
                      do inner=innerStart,innerEnd
                        cenInner=inner+xofsout
c                         
c                         get indices in array of input data
c                         
                        xp=minv(1,ioutXaxis)*cenInner+xpofs
                        yp=minv(2,ioutXaxis)*cenInner+ypofs
                        zp=minv(3,ioutXaxis)*cenInner+zpofs
                        bval = DMEANIN
                        ixp=int(xp)
                        iyp=int(yp)
                        izp=int(zp)
                        IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &                      IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &                      IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
                          dx=xp-ixp
                          dy=yp-iyp
                          dz=zp-izp
                          ixpp1=min(ixlim,ixp+1)
                          iypp1=min(iylim,iyp+1)
                          izpp1=min(izlim,izp+1)
C                         
C                           Set up terms for linear interpolation
C                           
                          d11 = (1. - dx) * (1. - dy)
                          d12 = (1. - dx) * dy
                          d21 = dx * (1. - dy)
                          d22 = dx * dy
                          bval = (1. - dz) * (d11 * array(ixp, iyp, izp)
     &                        + d12 * array(ixp, iypp1, izp)
     &                        + d21 * array(ixpp1, iyp, izp)
     &                        + d22 * array(ixpp1, iypp1, izp)) +
     &                        dz * (d11 * array(ixp, iyp, izpp1)
     &                        + d12 * array(ixp, iypp1, izpp1)
     &                        + d21 * array(ixpp1, iyp, izpp1)
     &                        + d22 * array(ixpp1, iypp1, izpp1))
                        endif
                        brray(indBase + inner * innerStride + middle *
     &                      middleStride + iouter * iouterStride) = bval
                      enddo
                    endif
                  enddo
C$OMP END PARALLEL DO
                enddo
              else
                brray(1 : iStride(3) * numZtoDo) = dmeanin
              endif
              wallCum = wallCum + walltime() - wallStart
              iunit=ifile(ixcube,iycube)
              do iz = 0, numZtoDo - 1
                ix = iz * idimOut(1)*idimOut(2) + 1
                if (iunit .eq. 6) then
                  call iclden(brray(ix),idimOut(1),idimOut(2),1,
     &                nxyzcube(1,ixcube),1, nxyzcube(2,iycube),tmin,tmax,tmean)
                  dmin=min(dmin,tmin)
                  dmax=max(dmax,tmax)
                  tsum = tsum + tmean
                endif
                call irepak(brray(ix),brray(ix),idimOut(1),idimOut(2),
     &              0,nxyzcube(1,ixcube)-1,0,nxyzcube(2,iycube)-1)
                call iwrsec(iunit,brray(ix))
                izinfile(ixcube, iycube, iz + izStart) = izsec(iunit)
                izsec(iunit)=izsec(iunit)+1
              enddo
c
c               End of do while: update start and number left to do
              izStart = izEnd + 1
              numZleft = numZleft - numZtoDo
            enddo
c	      print *,ixcube,iycube,izcube
            numDone = numDone + 1
            write(*,'(a,i6,a,i6)')'Finished',numDone,' of',
     &          ncubes(1)*ncubes(2)*ncubes(3)
            call flush(6)
          enddo
        enddo
c         
c         whole layer of cubes in z is done.  now reread and compose one
c         row of the output section at a time in array
c         
        call recompose_cubes(izcube, dmin, dmax, tsum)
      enddo
      if (iVerbose .gt. 0) write(*,'(a,f10.4)')'Wall time ', wallCum
c       
      dmean=tsum/nzout
      call iwrhdr(6,title,1,dmin,dmax,dmean)
      do i=1,4
        if (needScratch(i))call imclose(i)
      enddo
      call imclose(5)
      call imclose(6)
      call exit(0)
99    call exiterror('READING FILE')
      end


c       CUBEINDEXES Gets X and Y cube indexes from inner and outer loop indexes
c       If X becomes X on output, then make X the inner loop
c       If X becomes Y, then make X outer and Y inner, of course
c       If X becomes Z, it helps a little to make X outer and Y inner
c
      subroutine cubeIndexes(ioutXaxis, indInner, indOuter, ixCube, iyCube)
      implicit none
      integer*4 ioutXaxis, indOuter, indInner, ixCube, iyCube
      if (ioutXaxis .eq. 1) then
        ixCube = indInner
        iyCube = indOuter
      else
        iyCube = indInner
        ixCube = indOuter
      endif
      return
      end


c       RECOMPOSE_CUBES takes a layer of cubes in Z out of the scratch files
c       and writes it to the output file
c       
      subroutine recompose_cubes(izcube, dmin, dmax, tsum)
      use rotmatwarp
      implicit none
      real*4 dmin, dmax, tsum,tmin,tmax,tmean,tmpsum
      integer*4 iz, iycube, ixcube, izcube, nLinesOut, izsec, iunit
      integer*4 maxPlanes, numLeft, numToRead, izStart
      integer*4 indInner, indOuter
c       
c       whole layer of cubes in z is done.  Find our how many planes can be
c       recomposed at once
      if (ncubes(1) * ncubes(2) .eq. 1) return
      maxPlanes = (arraySize8 / nxout) / nyout
      if (maxPlanes .lt. 2) then
c
c         If only one plane fits, there is no advantage to doing it all at
c         once, so reread and compose one row of the output section at a time 
c         in array
c       
        do iz=1,nxyzcube(3,izcube)
          tmpsum = 0.
          do iycube=1,ncubes(2)
            nLinesOut = nxyzcube(2,iycube)
            do ixcube=1,ncubes(1)
              iunit=ifile(ixcube,iycube)
              izsec=izinfile(ixcube, iycube, iz)
              call imposn(iunit,izsec,0)
              call irdsec(iunit,brray,*99)
              call pack_piece(array,nxout,nyout,ixyzcube(1,ixcube),
     &            0,brray,nxyzcube(1,ixcube), nLinesOut)
            enddo
            call iclden(array,nxout,nLinesOut,1,nxout,1,nLinesOut,tmin,tmax,
     &          tmean)
            dmin=min(dmin,tmin)
            dmax=max(dmax,tmax)
            tmpsum=tmpsum+tmean*nLinesOut
            call iwrsecl(6,array, nLinesOut)
          enddo
          tsum = tsum + tmpsum / nyout
        enddo
      else
c         
c         Otherwise, do entire planes so that multiple successive slices can
c         be read from the cubes
        numLeft = nxyzcube(3,izcube)
        izStart = 1
        do while (numLeft .gt. 0)
          numToRead = min(maxPlanes, numLeft)
          numLeft = numLeft - numToRead
c           
c           read this set of planes from each cube in turn
          do indOuter = 1, limOuter
            do indInner = 1, limInner
              call cubeIndexes(ioutXaxis, indInner, indOuter, ixcube, iycube)
              iunit=ifile(ixcube,iycube)
              do iz = 1, numToRead
                izsec=izinfile(ixcube, iycube, iz + izStart - 1)
                call imposn(iunit,izsec,0)
                call irdsec(iunit,brray,*99)
                call pack_piece3D(array,nxout,nyout,maxPlanes,
     &              ixyzcube(1,ixcube), ixyzcube(2,iycube),
     &              iz, brray,nxyzcube(1,ixcube), nxyzcube(2,iycube))
              enddo
            enddo
          enddo
c           
c           Write the planes and maintain MMM
          do iz = 1, numToRead
            call writeFullPlane(array, nxout,nyout,maxPlanes, iz, dmin, dmax,
     &          tsum)
          enddo
          izStart = izStart + numToRead
        enddo
      endif
      return
99    call exiterror('READING FILE')
      end subroutine recompose_cubes

c       
c       PACK_PIECE packs the contents of brray into appropriate region of array
c
      subroutine pack_piece (array,ixdout,iydout,ixofs,iyofs,
     &    brray,nxin,nyin)
      implicit none
      integer*4 ixdout,iydout,ixofs,iyofs,nxin,nyin
      real*4 array(ixdout,iydout),brray(nxin,nyin)
      integer*4 ix, iy
      do iy=1,nyin
        do ix=1,nxin
          array(ix+ixofs,iy+iyofs)=brray(ix,iy)
        enddo
      enddo
      return
      end subroutine pack_piece

c       
c       PACK_PIECE3D packs the contents of brray into appropriate region and
c       slice of the 3D array
c
      subroutine pack_piece3D (array,ixdout,iydout,izdout,ixofs,iyofs,iz,
     &    brray,nxin,nyin)
      implicit none
      integer*4 ixdout,iydout,ixofs,iyofs,nxin,nyin,izdout,iz
      real*4 array(ixdout,iydout,izdout),brray(nxin,nyin)
      integer*4 ix, iy
      do iy=1,nyin
        do ix=1,nxin
          array(ix+ixofs,iy+iyofs,iz)=brray(ix,iy)
        enddo
      enddo
      return
      end subroutine pack_piece3D

c       
c       WRITEFULLPLANE calculates and maintains density MMM and writes a plane
c
      subroutine writeFullPlane(array, nxout,nyout,maxPlanes, iz, dmin, dmax,
     &          tsum)
      implicit none
      integer*4 nxout,nyout,maxPlanes,iz
      real*4 array(nxout,nyout,maxPlanes), dmin, dmax,tsum,tmin,tmax,tmean
      call iclden(array(1,1,iz),nxout,nyout,1,nxout,1,nyout,tmin,tmax,
     &          tmean)
      dmin=min(dmin,tmin)
      dmax=max(dmax,tmax)
      tsum=tsum+tmean
      call iwrsec(6,array(1,1,iz))
      return
      end

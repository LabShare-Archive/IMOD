*       * * * * WARPVOL * * * * *
c       
c       WARPVOL will transform a volume using a series of general linear
c       transformations. Its main use is to "warp" one tomogram from
c       a two-axis tilt series so that it matches the other tomogram.
c       For each position in the volume, it interpolates between adjacent
c       adjacent transformations to find the transformation appropriate for
c       that position.  Any initial alignment transformation (from
c       SOLVEMATCH) must be already contained in the transformations entered
c       into this program; this combining of transformations is accomplished
c       by FINDWARP.  It can work with either a 2-D matrix of
c       transformations (varying in X and Z) or with a general 3-D matrix,
c       as output by FINDWARP.  The program uses the same algorithm as
c       ROTATEVOL for rotating large volumes.
c       
c       See man page for more details
c       
c       $Id$
c       
      program warpvol
      use rotmatwarp
      implicit none
      real*4 cell(6),dxyzin(3)
      logical*4, allocatable :: solved(:),solvtmp(:)
      integer*4 mxyzin(3)
      real*4 mfor(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
      real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3)
      real*4 atmp1(3,3),atmp2(3,3),dtmp1(3),dtmp2(3),delta(3)
      real*4, allocatable :: aloc(:,:,:),dloc(:,:),aloctmp(:,:,:),dloctmp(:,:)
      real*4, allocatable :: ofsin(:,:,:),amx(:,:,:,:)

      integer*4 izsec(6)
      integer*4 inmin(3),inmax(3),icube(3)
      real*4 freinp(10),xyzcen(3),xcen,ycen,zcen
c       equivalence (xcen,xyzcen(1)), (ycen,xyzcen(2)), (zcen,xyzcen(3))
c       
      character*320 filein,fileout,fileinv,tempdir,tempext,patchfile,fillfile
c       
c       DNM 3/8/01: initialize the time in case time(tim) doesn't work
c       
      character dat*9,tim*8/'00:00:00'/
      character*80 titlech
      integer*4 nlocx,ninp,nlocy,nlocz,nloctot,i,j,kti,ja,ix,iy,ierr
      real*4 xlocst,ylocst,xlocmax,ylocmax,zlocst,zlocmax,cenlocx,cenlocy
      real*4 cenlocz,dxloc,dyloc,dzloc,dmin,dmax,tsum
      integer*4 ind,izcube,ixcube,iycube,ifx,ify,ifz,ival,ifempty
      integer*4 iz,ixlim,iylim,izlim,ixp,iyp,ixpp1,ixpm1,iypp1,iypm1,numDone
      real*4 xofsout,xp,yp,zp,bval,dx,dy,dz,v2,v4,v6,v5,v8,vu,vd,vmin,vmax
      real*4 a,b,c,d,e,f,tmin,tmax,tmean,dmean,dminin,dmaxin,d11,d12,d21,d22
      integer*4 iunit,longint,l,izp,izpp1,izpm1,nLinesOut,interpOrder, nExtra
      integer*4 newExtra,lForErr, newlocx, newlocy, newlocz
      integer*4 nxloAdd, nyloAdd,nzloAdd,nxhiAdd, nyhiAdd,nzhiAdd
      real*4 baseInt,outerCen, xfscale
      integer*4 indInner, indOuter, iyxf
      real*8 cputime, walltime, cpuStart, wallStart, wallCum, threadWall
      integer*4 innerStart, innerEnd, iouterStart, iouterEnd,innerAxis
      integer*4 iouterAxis,innerStride,iouterStride,indBase,iouter,numZtoDo
      integer*4 izStart, izEnd, numZleft
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger, PipGetFloatArray, PipNumberOfEntries
      integer*4 PipGetString,PipGetThreeIntegers,PipGetThreeFloats, PipGetFloat
      integer*4 PipGetNonOptionArg, PipGetInOutFile, PipGetBoolean
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  warpvol
c       
      integer numOptions
      parameter (numOptions = 11)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'xforms:TransformFile:FN:@tempdir:TemporaryDirectory:CH:@'//
     &    'size:OutputSizeXYZ:IT:@same:SameSizeAsInput:B:@'//
     &    'order:InterpolationOrder:I:@patch:PatchOutputFile:FN:@'//
     &    'filled:FilledInOutputFile:FN:@param:ParameterFile:PF:@help:usage:B:'
c       
c       set defaults here
c       
      interpOrder = 2
      xfscale = 1.
      baseInt = 0.5
      tempdir = ' '
      tempext='wrp      1'
      call time(tim)
      call date(dat)
      patchfile = ' '
      fillfile = ' '
      wallCum = 0.
c       
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'warpvol',
     &    'ERROR: WARPVOL - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile', 1, 'Name of input file', filein)
     &    .ne. 0) call exiterror('NO INPUT FILE SPECIFIED')
      call imopen(5,filein,'RO')
      call irdhdr(5,nxyzin,mxyzin,mode,dminin,dmaxin,dmeanin)
      nxout=nzin
      nyout=nyin
      nzout=nxin
c       
      if (pipinput) then
        ierr = PipGetString('PatchOutputFile', patchfile)
        ierr = PipGetString('FilledInOutputFile', fillfile)
      endif

      if (PipGetInOutFile('OutputFile', 2, 'Name of output file', fileout)
     &    .ne. 0 .and. patchfile .eq. ' ' .and. fillfile .eq. ' ')
     &    call exiterror('NO OUTPUT FILE SPECIFIED')
c       
      if (pipinput) then
        ierr = PipGetString('TemporaryDirectory', tempdir)
        ierr = PipGetInteger('InterpolationOrder', interpOrder)
        ierr = PipGetInteger('MemoryLimit', memoryLim)
        ierr = PipGetInteger('VerboseOutput', iVerbose)
        ierr = PipGetFloat('ScaleTransforms', xfScale)
        iz = 0
        ierr = PipGetBoolean('SameSizeAsInput', iz)
        if (iz .ne. 0) then
          nxout = nxin
          nzout = nzin
        endif
        ierr = PipGetThreeIntegers('OutputSizeXYZ', nxout, nyout, nzout)
        if (PipGetString('TransformFile', fileinv) .ne. 0) call exiterror(
     &      'NO FILE WITH INVERSE TRANSFORMS SPECIFIED')
      else
        write(*,'(1x,a,/,a,$)')'Enter path name of directory for '//
     &      'temporary files, ',' or Return to use current directory: '
        read(5,'(a)')tempdir
c         
        write(*,'(1x,a,3i5,a,$)')'X, Y, and Z dimensions of the '//
     &      'output file (/ for',nxout,nyout,nzout,'): '
        read(5,*)nxout,nyout,nzout
c         
c         get matrix for inverse transforms
c         
        print *,'Enter name of file with inverse transformations'
        read(5,'(a)')fileinv
      endif
      call PipDone()
      if (interpOrder .lt. 2) baseInt = 0.
c       
      call dopen(1,fileinv,'ro','f')
      read(1,'(a)')fileinv
      call frefor(fileinv,freinp,ninp)
      nlocx=nint(freinp(1))
      if(ninp.eq.2)then
        nlocy=1
        nlocz=nint(freinp(2))
      else
        nlocy=nint(freinp(2))
        nlocz=nint(freinp(3))
        if (ninp .eq. 9) then
          xlocst = freinp(4) * xfScale
          ylocst = freinp(5) * xfScale
          zlocst = freinp(6) * xfScale
          dxloc = freinp(7) * xfScale
          dyloc = freinp(8) * xfScale
          dzloc = freinp(9) * xfScale
        endif
      endif
      nloctot=nlocx*nlocz*nlocy
      if (nloctot .eq. 0) call exitError(
     &    'THE WARP FILE SPECIFIES 0 POSITIONS ON ONE AXIS')
      allocate (aloctmp(3,3,nloctot), dloctmp(3,nloctot), solvtmp(nloctot),
     &    stat = ierr)
      if (ierr .ne. 0)call exiterror(
     &    'ALLOCATING MEMORY FOR READING IN TRANSFORMATION ARRAYS')
      if (ninp .ne. 9) then
c         
c         Every position must be present, find min and max to deduce interval
c         
        xlocst=1.e10
        xlocmax=-1.e10
        ylocst=1.e10
        ylocmax=-1.e10
        zlocst=1.e10
        zlocmax=-1.e10
        
        do l=1,nloctot
          lForErr = l
          read(1,*,err=98,end=98)cenlocx,cenlocy,cenlocz
          read(1,*,err=98,end=98)((aloctmp(i,j,l),j=1,3),dloctmp(i,l),i=1,3)
          cenlocx = cenlocx * xfScale
          cenlocy = cenlocy * xfScale
          cenlocz = cenlocz * xfScale
          dloctmp(1:3,l) = dloctmp(1:3,l) * xfScale
          solvtmp(l) = .true.
          xlocst=min(cenlocx,xlocst)
          ylocst=min(cenlocy,ylocst)
          zlocst=min(cenlocz,zlocst)
          xlocmax=max(cenlocx,xlocmax)
          ylocmax=max(cenlocy,ylocmax)
          zlocmax=max(cenlocz,zlocmax)
        enddo
        dxloc=(xlocmax-xlocst)/max(1,nlocx-1)
        dyloc=(ylocmax-ylocst)/max(1,nlocy-1)
        dzloc=(zlocmax-zlocst)/max(1,nlocz-1)
      endif
c       
c       Now fix the dlocs if only one position, then read other way
      if (nlocx .eq. 1) dxloc = 1.
      if (nlocy .eq. 1) dyloc = 1.
      if (nlocz .eq. 1) dzloc = 1.
      if (ninp .eq. 9) then
c         
c         Know min and interval, so read each entry, find where it goes in
c         array and put it there
        do l=1,nloctot
          solvtmp(l) = .false.
        enddo
        lForErr = 1
10      read(1,*,err=98,end=12)cenlocx,cenlocy,cenlocz
        cenlocx = cenlocx * xfScale
        cenlocy = cenlocy * xfScale
        cenlocz = cenlocz * xfScale
        ix = min(nlocx, max(1, nint((cenlocx - xlocst) / dxloc + 1.)))
        iy = min(nlocy, max(1, nint((cenlocy - ylocst) / dyloc + 1.)))
        iz = min(nlocz, max(1, nint((cenlocz - zlocst) / dzloc + 1.)))
        l = ix + (iy - 1) * nlocx + (iz - 1) * nlocx * nlocy
        read(1,*,err=98,end=98)((aloctmp(i,j,l),j=1,3),dloctmp(i,l),i=1,3)
        dloctmp(1:3,l) = dloctmp(1:3,l) * xfScale
        solvtmp(l) = .true.
        lForErr = lForErr + 1
        go to 10
      endif
12    close(1)
c       
c       determine additional positions needed to cover plane of volume
      nxloAdd = 0
      nxhiAdd = 0
      nyloAdd = 0
      nyhiAdd = 0
      nzloAdd = 0
      nzhiAdd = 0
      if (nlocx .gt. 1) then
        nxloAdd = max(0, nint((xlocst + nxout / 2.) / dxloc))
        nxhiAdd = max(0, nint((nxout / 2. - (xlocst+(nlocx-1)*dxloc)) / dxloc))
      endif
      if (nlocy .gt. nlocz) then
        nyloAdd = max(0, nint((ylocst + nyout / 2.) / dyloc))
        nyhiAdd = max(0, nint((nyout / 2. - (ylocst+(nlocy-1)*dyloc)) / dyloc))
      elseif (nlocz .gt. 1) then
        nzloAdd = max(0, nint((zlocst + nzout / 2.) / dzloc))
        nzhiAdd = max(0, nint((nzout / 2. - (zlocst+(nlocz-1)*dzloc)) / dzloc))
      endif
c       
c       If any are being added, shift the transforms up in the array
c       and adjust the parameters
      newlocx = nlocx + nxloAdd + nxhiAdd
      newlocy = nlocy + nyloAdd + nyhiAdd
      newlocz = nlocz + nzloAdd + nzhiAdd
      nloctot = newlocx * newlocy * newlocz
      allocate (aloc(3,3,nloctot), dloc(3,nloctot), solved(nloctot), stat=ierr)
      if (ierr .ne. 0)call exiterror(
     &    'ALLOCATING MEMORY FOR TRANSFORMATION ARRAYS')

      do i = 1, nloctot
        solved(i) = .false.
      enddo
      call shiftTransforms(aloctmp, dloctmp, solvtmp, nlocx, nlocy, nlocz,
     &    aloc, dloc, solved, newlocx, newlocy, newlocz, nxloAdd, nyloAdd,
     &    nzloAdd)
      xlocst = xlocst - nxloAdd * dxloc
      ylocst = ylocst - nyloAdd * dyloc
      zlocst = zlocst - nzloAdd * dzloc
      nlocx = newlocx
      nlocy = newlocy
      nlocz = newlocz
      deallocate (aloctmp, dloctmp, solvtmp, stat = ierr);
c       
c       Fill in missing transforms by extrapolation and weighted averaging
      call fillInTransforms(aloc, dloc, solved, nlocx, nlocy, nlocz,
     &    dxloc, dyloc, dzloc)
      if (fillfile .ne. ' ') then
c         
c         Output file of filled in transforms
        call dopen(1, fillfile, 'new', 'f')
        write(1,104)nlocx,nlocy,nlocz,xlocst,ylocst,zlocst,dxloc,dyloc,dzloc
104     format(i5,2i6,3f11.2,3f10.4)
        do iz = 1, nlocz
          do iy = 1, nlocy
            do ix = 1, nlocx
              l = ix + (iy - 1) * nlocx + (iz - 1) * nlocx * nlocy
              write(1,103)xlocst + (ix - 1) * dxloc, ylocst + (iy - 1) * dyloc,
     &            zlocst + (iz - 1) * dzloc
103           format(3f9.1)
              write(1,102)((aloc(i,j,l),j=1,3),dloc(i,l),i=1,3)
102           format(3f10.6,f10.3)
            enddo
          enddo
        enddo
        close(1)
      endif
      if (patchfile .ne. ' ') then
c         
c         Output patch file of inverse vectors for positions in output volume
        call dopen(1, patchfile, 'new', 'f')
        write(1,'(i8,a)')nlocx*nlocy*nlocz, ' positions'
        do iz = 1, nlocz
          do iy = 1, nlocy
            do ix = 1, nlocx
              l = ix + (iy - 1) * nlocx + (iz - 1) * nlocx * nlocy
              xp = xlocst + (ix - 1) * dxloc
              yp = ylocst + (iy - 1) * dyloc
              zp = zlocst + (iz - 1) * dzloc
              dx = aloc(1,1,l) * xp + aloc(1,2,l) * yp + aloc(1,3,l) * zp +
     &            dloc(1,l) - xp
              dy = aloc(2,1,l) * xp + aloc(2,2,l) * yp + aloc(2,3,l) * zp +
     &            dloc(2,l) - yp
              dz = aloc(3,1,l) * xp + aloc(3,2,l) * yp + aloc(3,3,l) * zp +
     &            dloc(3,l) - zp
              ifx = nint(xp + nxout / 2.)
              ify = nint(yp + nyout / 2.)
              ifz = nint(zp + nzout / 2.)
              write(1, 105)ifx,ify,ifz,dx,dy,dz
105           format(3i8,3f12.2)
            enddo
          enddo
        enddo
        close(1)
      endif
c       
c       Exit if patch or warp output files
      if (patchfile .ne. ' ' .or. fillfile .ne. ' ') call exit(0)
c       
c       Get mean inverse transform
      mfor = 0.
      do ja=1,nloctot
        call inv_matrix(aloc(1,1,ja),minv)
        mfor = mfor + minv / nloctot
      enddo
c       
c       DNM 7/26/02: transfer pixel spacing to same axes
c       
      call irtdel(5,delta)
      do i=1,3
        cell(i)=nxyzout(i)*delta(i)
        cell(i+3)=90.
        cxyzout(i)=nxyzout(i)/2.
      enddo
c       
c       Unless one layer in Y, allow 5% of memory for transforms
      if (nlocy .gt. 1) memoryLim = 0.95 * memoryLim
c       
c       Get provisional setup of cubes then find actual limits of input
c       cubes with this setup - loop until no new extra pixels needed
c       
      nExtra = 0
      newExtra = 1
      do while (newExtra .gt. 0)

        call setup_cubes_scratch(mfor, aloc, nloctot, nExtra, ' ', tempdir,
     &      tempext, tim, .true.)
c         
        newExtra = 0
        do izcube=1,ncubes(3)
          do ixcube=1,ncubes(1)
            do iycube=1,ncubes(2)
              call testCubeFaces(aloc,dloc,xlocst,dxloc,ylocst,dyloc, zlocst,
     &            dzloc, nlocx, nlocy, nlocz, ixcube,iycube,izcube,newExtra,
     &            inmin, inmax)
            enddo
          enddo
        enddo
        nExtra = nExtra + newExtra
      enddo
c       
      print *,nextra,' extra pixels needed in cubes for final setup'
c       
c       Get setup again for real this time and open output file after
c       all potential errors are past
c       
      call setup_cubes_scratch(mfor, aloc, nloctot, nExtra, filein, tempdir,
     &    tempext, tim, .true.)
c       
c       Set up axes and strides and allocate the array for transforms
      iy = idimOut(2)
      if (nlocy .eq. 1) iy = 1
      if (ioutXaxis .eq. 3) then
        innerAxis = 3
        iouterAxis = 1
        innerStride = idimOut(1) * idimOut(2)
        iouterStride = 1
      else
        innerAxis = 1
        iouterAxis = 3
        innerStride = 1
        iouterStride = idimOut(1) * idimOut(2)
      endif
      ix = idimOut(innerAxis)
      allocate (ofsin(3,ix,iy), amx(3,3,ix,iy), stat = ierr)
c       print *,'allocated',ix,iy,innerAxis,iouterAxis, idimOut
      if (ierr .ne. 0) call exitError(
     &    'FAILED TO ALLOCATE ARRAYS FOR PRECOMPUTED TRANSFORMS')
c       
      call imopen(6,fileout,'NEW')
c       
      call icrhdr(6,nxyzout,nxyzout,mode,title,0)
      call ialcel(6,cell)
      call itrlab(6,5)
      write(titlech,302) dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
302   FORMAT('WARPVOL: 3-D warping of tomogram:',t57,a9,2x,a8)
      dmin=1.e20
      dmax=-dmin
      tsum=0.
      numDone = 0
      call irttlt(5,tiltold)
      call ialtlt(6,tiltold)
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
            cpuStart = cputime()
            wallStart = walltime()
            call cubeIndexes(ioutXaxis, indInner, indOuter, ixcube, iycube)
            icube(1) = ixcube
            icube(2) = iycube
            newExtra = -1
            call testCubeFaces(aloc,dloc,xlocst,dxloc,ylocst,dyloc, zlocst,
     &          dzloc, nlocx, nlocy, nlocz, ixcube,iycube,izcube,newExtra,
     &          inmin, inmax)
            ifempty=0
            do i=1,3
              if(inmin(i).gt.inmax(i))ifempty=1
            enddo
c             
c             load the input cube
            if(ifempty.eq.0)then
              if (iVerbose .gt. 1) print *,ixcube,iycube,izcube,inmin(3),
     &            inmax(3), inmax(3)+1-inmin(3)
              do iz=inmin(3),inmax(3)
                call imposn(5,iz,0)
                if (iVerbose .gt. 1) then
                  write(*,'(10i5)')ixcube,iycube,izcube,iz,iz+1-inmin(3)
     &                ,inmin(1), inmax(1),inmin(2),inmax(2)
                  call flush(6)
                endif
                call irdpas(5,array(1,1,iz+1-inmin(3)),inpdim(1),inpdim(2),
     &              inmin(1),inmax(1),inmin(2),inmax(2),*99)
              enddo
            endif
c             
c             prepare offsets and limits
            xofsout=ixyzcube(innerAxis, icube(innerAxis))-1- cxyzout(innerAxis)
            ixlim=inmax(1)+1-inmin(1)
            iylim=inmax(2)+1-inmin(2)
            izlim=inmax(3)+1-inmin(3)
c             
c             If one layer of cubes in Z, precompute all transforms because it
c             is invariant in Z
            if (ifempty .eq. 0 .and. nlocz .eq. 1 .and. ioutXaxis .ne. 3) then
              if (iVerbose .gt. 0) print *,'Precomputing for layer'
              zcen=ixyzcube(3,izcube)+czout
              do iy=1,nxyzcube(2,iycube)
                ycen=ixyzcube(2,iycube)+iy-1-cyout
                do ix=1,nxyzcube(1,ixcube)
                  xcen=ix+xofsout
                  call interpinv(aloc,dloc,xlocst,dxloc,ylocst,
     &                dyloc, zlocst,dzloc, nlocx, nlocy, nlocz,
     &                xcen,ycen, zcen, amx(1,1,ix,iy),ofsin(1,ix,iy))
                  do i=1,3
                    ofsin(i,ix,iy)=ofsin(i,ix,iy)+1+nxyzin(i)/2.-inmin(i)
                  enddo
                enddo
              enddo
            endif
c             
c             loop over Z segments of the output cube (multiple slices in case
c             where the X axis is being output as the Z axis)
            izStart = 1
            numZleft = nxyzcube(3,izcube)
            do while (numZleft .gt. 0)
              numZtoDo = min(numZleft, maxZout)
              izEnd = izStart + numZtoDo - 1
c               print *,ixcube,iycube,izStart,izEnd,numZleft
              if(ifempty.eq.0)then
                
c                 Set up index limits for inner and outer loops
                if (ioutXaxis .eq. 3) then
                  innerStart = izStart
                  innerEnd = izEnd
                  iouterStart = 1
                  iouterEnd = nxyzcube(1, ixcube)
                else
                  innerStart = 1
                  innerEnd = nxyzcube(1, ixcube)
                  iouterStart = izStart
                  iouterEnd = izEnd
                endif
                indBase = -idimOut(1) - izStart * idimOut(1) * idimOut(2)
c                 print *,iouterStart, iouterEnd,innerStart,innerEnd
c                 
c                 Loop on the outer axis in the Z segment
                do iouter = iouterStart, iouterEnd
                  xyzcen(iouterAxis)=ixyzcube(iouterAxis,icube(iouterAxis))+
     &                iouter-1- cxyzout(iouterAxis)
                  outerCen = xyzcen(iouterAxis)
c                   
c                   get matrices for each inner position either for the one
c                   position in Y or for every one
                  if(.not.(nlocz .eq. 1 .and. ioutXaxis .ne. 3)) then
                    iyxf = nxyzcube(2,iycube)
                    if (nlocy .eq. 1) iyxf = 1
                    do iy=1,iyxf
                      xyzcen(2)=ixyzcube(2,iycube)+iy-1-cyout
                      do ix = innerStart, innerEnd
                        xyzcen(innerAxis)=ix+xofsout
                        call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &                      zlocst,dzloc, nlocx, nlocy, nlocz, xyzcen(1),
     &                      xyzcen(2),xyzcen(3), amx(1,1,ix,iy),ofsin(1,ix,iy))
                        do i=1,3
                          ofsin(i,ix,iy)=ofsin(i,ix,iy)+1+nxyzin(i)/2.-inmin(i)
                        enddo
                      enddo
                    enddo
                  endif
                  threadWall = walltime()
c                   
c                   parallelize loop on Y and inner axes
c                   
C                   $OMP PARALLEL DO
C                   $OMP& SHARED(nxyzcube, ixyzcube, outerCen,innerStart,innerEnd)
C                   $OMP& SHARED(iycube,cyout, baseInt,indBase, innerStride,iouter)
C                   $OMP& SHARED(interporder, xofsout, DMEANIN, array, brray,ixlim,iylim,izlim)
C                   $OMP& SHARED(amx, ofsin, nlocy,iouterStride, idimOut,innerAxis,iouterAxis)
C                   $OMP& DEFAULT(PRIVATE)
                  do iy=1,nxyzcube(2,iycube)
                    ycen=ixyzcube(2,iycube)+iy-1-cyout
                    iyxf = iy
                    if (nlocy .eq. 1) iyxf = 1
                    do ix=innerStart, innerEnd
                      xcen=ix+xofsout
c                       
c                       get indices in array of input data
                      xp=amx(1,innerAxis,ix,iyxf)*xcen+amx(1,2,ix,iyxf)*ycen+
     &                    amx(1,iouterAxis,ix,iyxf)*outerCen+ ofsin(1,ix,iyxf)
                      yp=amx(2,innerAxis,ix,iyxf)*xcen+amx(2,2,ix,iyxf)*ycen+
     &                    amx(2,iouterAxis,ix,iyxf)*outerCen+ ofsin(2,ix,iyxf)
                      zp=amx(3,innerAxis,ix,iyxf)*xcen+amx(3,2,ix,iyxf)*ycen+
     &                    amx(3,iouterAxis,ix,iyxf)*outerCen+ ofsin(3,ix,iyxf)
                      bval = DMEANIN
c                       
c                       do generalized evaluation of whether pixel is doable
c                       
                      ixp=int(xp + baseInt)
                      iyp=int(yp + baseInt)
                      izp=int(zp + baseInt)
                      IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &                    IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &                    IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
                        dx=xp-ixp
                        dy=yp-iyp
                        dz=zp-izp
                        ixpp1=min(ixlim,ixp+1)
                        iypp1=min(iylim,iyp+1)
                        izpp1=min(izlim,izp+1)
c                         
                        if (interpOrder .ge. 2) then
                          ixpm1=max(1,ixp-1)
                          iypm1=max(1,iyp-1)
                          izpm1=max(1,izp-1)
C                           
C                           Set up terms for quadratic interpolation with
c                           higher-order terms omitted
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
                        else
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
                      endif
                      brray(indBase + ix * innerStride + iy * idimOut(1) +
     &                    iouter * iouterStride) = bval
                    enddo
                  enddo
C                   $OMP END PARALLEL DO
                  wallCum = wallCum + walltime() - threadWall
                enddo
              else
                brray(1 : idimOut(1)*idimOut(2)*numZtoDo) = dmeanin
              endif

              iunit=ifile(ixcube,iycube)
              do iz = 0, numZtoDo - 1
                ix = iz * idimOut(1)*idimOut(2) + 1
                if (iunit .eq. 6) then
                  call iclden(brray(ix),idimOut(1),idimOut(2),1,
     &                nxyzcube(1,ixcube),1,nxyzcube(2,iycube),tmin,tmax, tmean)
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
c             print *,ixcube,iycube,izcube
            numDone = numDone + 1
            if (iVerbose .gt. 0) write(*,'(a,f10.4,a,f10.4)')'Cube CPU time:',
     &          cputime()-cpuStart,'   Wall time:',walltime() - wallStart
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
c       
      if (iVerbose .gt. 0) write(*,'(a,f10.4)')'Thread wall time ', wallCum
      dmean=tsum/nzout
      call iwrhdr(6,title,1,dmin,dmax,dmean)
      do i=1,4
        if (needScratch(i))call imclose(i)
      enddo
      call imclose(5)
      call imclose(6)
      call exit(0)
98    write(*,'(/,a,i7)')'ERROR: WARPVOL - READING WARP FILE AT POSITION',
     &    lForErr
      call exit(1)
99    call exiterror('READING IMAGE FILE')
      end program warpvol

c       
c       TESTCUBEFACES tests the faces of an output cube at its corners and 
c       at all positions corresponding to transforms
c       This was a nice internal subroutine but gfortran would not compile it
c       with openmp enabled
c       
      subroutine testCubeFaces(aloc,dloc,xlocst,dxloc,ylocst,dyloc, zlocst,
     &    dzloc, nlocx, nlocy, nlocz, ixcube,iycube,izcube,newExtrarg, inmin,
     &    inmax)
      use rotmatwarp
      implicit none
      integer*4 newExtrarg,nlocx, nlocy, nlocz,inmin(3),inmax(3)
      real*4 aloc(3,3,*),dloc(3,*),xlocst,dxloc,ylocst,dyloc,zlocst,dzloc
      real*4 xcubelo,xcubehi,ycubelo,ycubehi,zcubelo,zcubehi
      integer*4 ixcube,iycube,izcube
      integer*4 ixlo,ixhi,iylo,iyhi,izlo,izhi
      real*4 dxUse,dyUse,dzUse, xlUse,ylUse,zlUse,dxyz(3),ainv(3,3)
      integer*4 jfx,jfy,jfz,jval,i
      real*4 xcen,ycen,zcen
c       
c       back-transform the faces of the output cube at the corners and
c       at positions corresponding to transforms to
c       find the limiting index coordinates of the input cube
c       
      do i=1,3
        inmin(i)=100000
        inmax(i)=-100000
      enddo
      call cubeTestLimits(ixyzcube(2,iycube), nxyzcube(2,iycube),cyout,
     &    nlocy, ylocst, dyloc, ycubelo,ycubehi,iylo,iyhi,ylUse, dyUse)
      call cubeTestLimits(ixyzcube(3,izcube), nxyzcube(3,izcube), czout,
     &    nlocz, zlocst, dzloc, zcubelo, zcubehi, izlo, izhi, zlUse, dzUse)
      call cubeTestLimits(ixyzcube(1,ixcube), nxyzcube(1,ixcube), cxout,
     &    nlocx, xlocst, dxloc, xcubelo, xcubehi, ixlo, ixhi,xlUse,dxUse)
      do jfx=ixlo, ixhi
        do jfy=iylo, iyhi
          do jfz=izlo, izhi
            if (jfx .eq. ixlo .or. jfx .eq. ixhi .or. jfy .eq. iylo.or.
     &          jfy .eq. iyhi .or. jfz .eq. izlo .or. jfz .eq. izhi)
     &          then
              xcen = max(xcubelo, min(xcubehi, xlUse + jfx * dxUse))
              ycen = max(ycubelo, min(ycubehi, ylUse + jfy * dyUse))
              zcen = max(zcubelo, min(zcubehi, zlUse + jfz * dzUse))
              call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &            zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &            zcen,ainv,dxyz) 
              do i=1,3
                jval=nint(ainv(i,1)*xcen+ainv(i,2)*ycen+
     &              ainv(i,3)*zcen+dxyz(i)+nxyzin(i)/2.)
                inmin(i)=max(0,min(inmin(i),jval-2))
                inmax(i)=min(nxyzin(i)-1,max(inmax(i),jval+2))
c                 
c                 See if any extra pixels are needed in input
c                 
c                 if (newExtrarg .ge. 0 .and. newExtrarg .lt.inmax(i) +
c                 &              1-inmin(i)-inpdim) then
c                 print *,ixcube,iycube,izcube,xcen,ycen,zcen,
c                 &                        inmax(i) + 1 - inmin(i) -
c                 &                        inpdim,i, jval, inmin(i), inmax(i)
c                 print *,ainv(i,1),ainv(i,2),ainv(i,3),dxyz(i)
c                 endif
                if (newExtrarg .ge. 0) newExtrarg = max(newExtrarg,
     &              inmax(i) + 1 - inmin(i) - inpdim(i))
              enddo
            endif
          enddo
        enddo
      enddo
      end subroutine testCubeFaces

c       
c       CUBETESTLIMITS determines limits for cube testing on one axis
c       
      subroutine cubeTestLimits(icube, ncube, cout, nloc, stloc, dloc, cubelo,
     &    cubehi, ilo, ihi, stluse, dluse)
      implicit none
      integer*4 icube, ncube, nloc, ilo, ihi
      real*4 cout, stloc, dloc, cubelo, cubehi, stluse, dluse
      cubelo = icube - cout
      cubehi = icube + ncube - cout
      if (nloc .gt. 1) then
        ilo = floor((cubelo - stloc) / dloc)
        ihi = ceiling((cubehi - stloc) / dloc)
        stluse = stloc
        dluse = dloc
      else
        stluse = cubelo
        dluse = (cubehi - cubelo) / 2.
        ilo = 0
        ihi = 2
      endif
      return
      end

c       
c       INTERPINV takes the array of transforms and a given position
c       xcen,ycen,zcen and determines the interpolated transform minv,dxyz
c       
      subroutine interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,zlocst,
     &    dzloc,nlocx, nlocy, nlocz, xcen,ycen,zcen,minv,dxyz)
      implicit none
      integer*4 nlocx, nlocy, nlocz
      real*4 aloc(3,3,nlocx,nlocy,nlocz),dloc(3,nlocx,nlocy,nlocz)
      real*4 minv(3,3),dxyz(3),xlocst,dxloc,ylocst,dyloc,zlocst,dzloc
      real*4  xcen,ycen,zcen
      real*4 x,fx,fx1,y,fy,fy1,z,fz,fz1
      integer*4 ix,ix1,iy,iy1,iz,iz1,i,j
c       
      if (nlocx .gt. 1) then
        x=min(float(nlocx),max(1.,1.+(xcen-xlocst)/dxloc))
        ix=x
        ix1=min(ix+1,nlocx)
        fx1=x-ix
        fx=1.-fx1
      else
        ix = 1
        ix1 = 1
        fx1 = 0.
        fx = 1.
      endif
c       
      if (nlocy .gt. 1) then
        y=min(float(nlocy),max(1.,1.+(ycen-ylocst)/dyloc))
        iy=y
        iy1=min(iy+1,nlocy)
        fy1=y-iy
        fy=1.-fy1
      else
        iy = 1
        iy1 = 1
        fy1 = 0.
        fy = 1.
      endif
c       
      if (nlocz .gt. 1) then
        z=min(float(nlocz),max(1.,1.+(zcen-zlocst)/dzloc))
        iz=z
        iz1=min(iz+1,nlocz)
        fz1=z-iz
        fz=1.-fz1
      else
        iz = 1
        iz1 = 1
        fz1 = 0.
        fz = 1.
      endif
c       
      do i=1,3
        do j=1,3
          minv(i,j)=fy*(fz*(fx*aloc(i,j,ix,iy,iz) +
     &        fx1*aloc(i,j,ix1,iy,iz)) +
     &        fz1*(fx*aloc(i,j,ix,iy,iz1) +
     &        fx1*aloc(i,j,ix1,iy,iz1))) +
     &        fy1*(fz*(fx*aloc(i,j,ix,iy1,iz) +
     &        fx1*aloc(i,j,ix1,iy1,iz)) +
     &        fz1*(fx*aloc(i,j,ix,iy1,iz1) +
     &        fx1*aloc(i,j,ix1,iy1,iz1)))
        enddo
        dxyz(i)=fy*(fz*(fx*dloc(i,ix,iy,iz) +
     &      fx1*dloc(i,ix1,iy,iz)) +
     &      fz1*(fx*dloc(i,ix,iy,iz1) +
     &      fx1*dloc(i,ix1,iy,iz1))) +
     &      fy1*(fz*(fx*dloc(i,ix,iy1,iz) +
     &      fx1*dloc(i,ix1,iy1,iz)) +
     &      fz1*(fx*dloc(i,ix,iy1,iz1) +
     &      fx1*dloc(i,ix1,iy1,iz1)))
      enddo
      return
      end

c       FILLINTRANSFORMS fills in a regular array of transforms by
c       extrapolation from the nearest transforms to each missing one, where
c       the extrapolation is a weighted mean with weights proportional to the
c       square of distance from each existing transform.
c       
      subroutine fillInTransforms(aloc, dloc, solved, nlocx, nlocy, nlocz,
     &    dxloc, dyloc, dzloc)
      implicit none
      integer*4 nlocx, nlocy, nlocz, ix, iy, iz, jx, jy, jz, minx, miny, minz
      real*4 aloc(3,3,nlocx,nlocy,nlocz), dloc(3,nlocx,nlocy,nlocz)
      logical*4 solved(nlocx,nlocy,nlocz), boundary
      real*4 dxloc, dyloc, dzloc, dist, dmin, angle
      real*4 dx, dy, dz, dxyz(3), wsum, distcrit
      equivalence (dx, dxyz(1)),(dy, dxyz(2)),(dz, dxyz(3))
      integer*4 kx, ky, nayx, nayy, nayz, indyz, iysfac, izsfac, is, indDom
      integer*4 jxmin, jxmax, jymin, jymax, jzmin, jzmax
      integer*4 ixstep(12)/1,1,1,0,-1,-1,-1,0,1,1,1,0/
      integer*4 iystep(12)/-1,0,1,1,1,0,-1,-1,-1,0,1,1/
      real*4 range/2./
      real*4 atan2d
c       
      do iz = 1, nlocz
        do iy = 1, nlocy
          do ix = 1, nlocx
            dmin = 1.e30
            if (.not.solved(ix,iy,iz)) then
c               
c               Find closest position with transform
              do jz = 1, nlocz
                do jy = 1, nlocy
                  do jx = 1, nlocx
                    if (solved(jx,jy,jz)) then
                      dist = ((jx - ix) * dxloc)**2 + ((jy - iy) * dyloc)**2 +
     &                    ((jz - iz) * dzloc)**2
                      if (dist .lt. dmin) then
                        dmin = dist
                        minx = jx
                        miny = jy
                        minz = jz
                      endif
                    endif
                  enddo
                enddo
              enddo
c               
c               zero out the sum
              do jx = 1, 3
                dloc(jx,ix,iy,iz) = 0.
                do jy = 1,3
                  aloc(jx,jy,ix,iy,iz) = 0.
                enddo
              enddo
              wsum = 0.
c               write(*,'(a,3i4,a,3i4,f8.2)')'Filling in',ix,iy,iz,'  nearest',
c               &            minx,miny,minz,sqrt(dmin)
c               
c               Get actual distance to look, range of indexes to search, and
c               the criterion which is square of maximum distance
              dist = range * sqrt(dmin)
              jxmin = max(1, nint(ix - dist / max(dxloc, 1.) - 1))
              jxmax = min(nlocx, nint(ix + dist / max(dxloc, 1.) + 1))
              jymin = max(1, nint(iy - dist / max(dyloc, 1.) - 1))
              jymax = min(nlocy, nint(iy + dist / max(dyloc, 1.) + 1))
              jzmin = max(1, nint(iz - dist / max(dzloc, 1.) - 1))
              jzmax = min(nlocz, nint(iz + dist / max(dzloc, 1.) + 1))
              distcrit = dist**2
c               write(*,'(3i6)')jxmin,jxmax,jymin,jymax,jzmin,jzmax
c               
c               Get dominant index for second dimension
              indyz = 2
              iysfac = 1
              izsfac = 0
              if (nlocz .gt. nlocy) then
                indyz = 3
                izsfac = 1
                iysfac = 0
              endif
c               
c               Loop in the neighborhood, find boundary points within range
              do jz = jzmin, jzmax
                do jy = jymin, jymax
                  do jx = jxmin, jxmax
                    if (solved(jx,jy,jz)) then
                      dx = (jx - ix) * dxloc
                      dy = (jy - iy) * dyloc
                      dz = (jz - iz) * dzloc
                      dist = dx**2 + dy**2 + dz**2
                      if (dist .le. distcrit) then
c                         
c                         Find dominant direction to the point
                        angle = atan2d(dxyz(indyz), dx) + 157.5
                        if (angle .lt. 0) angle = angle + 360.
                        indDom = angle / 45. + 1.
                        indDom = max(1, min(8, indDom))
c                         
c                         Check that this point is a boundary, i.e. does not
c                         have a neighbor in any one of the 5 directions toward
c                         or at right angles to the dominant direction
                        boundary = .false.
                        do is = indDom, indDom + 4
                          nayx = jx + ixstep(is)
                          nayy = jy + iysfac * iystep(is)
                          nayz = jz + izsfac * iystep(is)
                          if (nayx .ge. 1. and. nayx .le. nlocx .and.
     &                        nayy .ge. 1. and. nayy .le. nlocy .and.
     &                        nayz .ge. 1. and. nayz .le. nlocz) then
                            if (.not. solved(nayx,nayy,nayz)) boundary = .true.
                          endif
                        enddo
c                         
c                         For boundary or min point, add to weighted sum
                        if (boundary .or. (jx .eq. minx .and. jy .eq.
     &                      miny .and. jz .eq. minz)) then
c                           write(*,'(a,3i4,a,f7.1,i4,f12.8)')'Adding in',jx,jy,
c                           &                        jz,' angle, dir', angle, indDom, 1./dist
                          do kx = 1, 3
                            dloc(kx,ix,iy,iz) = dloc(kx,ix,iy,iz) +
     &                          dloc(kx,jx,jy,jz) / dist
                            do ky = 1,3
                              aloc(kx,ky,ix,iy,iz) = aloc(kx,ky,ix,iy,iz) +
     &                            aloc(kx,ky,jx,jy,jz) /dist
                            enddo
                          enddo
                          wsum = wsum + 1. / dist
                        endif
                      endif
                    endif
                  enddo
                enddo
              enddo
c               
c               divide by weight sum
c               write (*,'(a,f12.8)')'wsum=',wsum
              do jx = 1, 3
                dloc(jx,ix,iy,iz) = dloc(jx,ix,iy,iz) / wsum
                do jy = 1,3
                  aloc(jx,jy,ix,iy,iz) = aloc(jx,jy,ix,iy,iz) / wsum
                enddo
c                 write(*,'(3f9.5,f9.2)')(aloc(jx,jy,ix,iy,iz),jy = 1,3),
c                 &              dloc(jx,ix,iy,iz)
              enddo
            endif
          enddo
        enddo
      enddo
      return
      end

c       
c       SHIFTTRANSFORMS shifts the transforms to fill the center of a larger
c       array
c       
      subroutine shiftTransforms(alocIn, dlocIn, solvtmp, nlocx, nlocy, nlocz,
     &    aloc, dloc, solved, newlocx, newlocy, newlocz, nxadd, nyadd, nzadd)
      implicit none
      integer*4 nlocx, nlocy, nlocz, newlocx, newlocy, newlocz
      integer*4 nxadd, nyadd, nzadd
      logical*4 solvtmp(nlocx, nlocy, nlocz), solved(newlocx, newlocy, newlocz)
      real*4 alocIn(3,3,nlocx, nlocy, nlocz), dlocIn(3,nlocx, nlocy, nlocz)
      real*4 aloc(3,3,newlocx,newlocy,newlocz), dloc(3,newlocx,newlocy,newlocz)
      integer*4 ix, iy, iz, ixn, iyn, izn, jx, jy
      do iz = nlocz, 1, -1
        izn = iz + nzadd
        do iy = nlocy, 1, -1
          iyn = iy + nyadd
          do ix = nlocx, 1, -1
            if (solvtmp(ix,iy,iz)) then
              ixn = ix + nxadd
              solved(ixn, iyn, izn) = .true.
              do jy = 1,3
                dloc(jy,ixn,iyn,izn) = dlocIn(jy,ix,iy,iz)
                do jx = 1, 3
                  aloc(jx,jy,ixn,iyn,izn) = alocIn(jx,jy,ix,iy,iz)
                enddo
              enddo
            endif
          enddo
        enddo
      enddo
      return
      end

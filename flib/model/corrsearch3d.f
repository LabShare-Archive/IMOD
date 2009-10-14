*       * * * * * CORRSEARCH3D * * * * *
c       
c       CORRSEARCH3D will determine the 3D displacement between two image
c       volumes at a regular array of positions.  At each position, it
c       extracts a patch of a specified size from each volume, then
c       searches for a local maximum in the cross-correlation between the
c       two patches.  The starting point of the search is based upon the
c       displacements of adjacent patches that have already been determined.
c       If there are no such adjacent patches, or if no maximum is found for
c       displacements within a specified range, the program uses FFT-based
c       cross-correlation instead.
c       
c       See man page for details.
c       
c       David Mastronarde, 7/16/01
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer idim,limvert,limcont,limpat,limwork
      parameter (idim=140000000)
      parameter (limvert=100000,limcont=1000,limpat=100000,limwork=1000*1000)
      integer*4 nx,ny,nz,nx2,ny2,nz2
      real*4 dxInitial, dyInitial, dzInitial, dxyzInit(3)
      real*4 dxVolume, dyVolume, dzVolume, dxyzVol(3)
      COMMON //NX,NY,NZ,nx2,ny2,nz2,dxInitial, dyInitial, dzInitial, dxVolume,
     &    dyVolume, dzVolume
      EQUIVALENCE (NX,NXYZ),(nx2,nxyz2),(dxyzInit, dxInitial)
      equivalence (dxyzVol, dxVolume)
C       
      integer*4 NXYZ(3),MXYZ(3),nxyzbsrc(3),nxyz2(3)
      real*4 buf(idim),work(limwork),ctf(8193)
      common /bigarr/buf
c       
      logical inside,exist,readSmallMod,found, flipMessages
      integer getimodhead,getimodscales
      real*4 xvert(limvert),yvert(limvert),zcont(limcont)
      integer*4 indvert(limcont),nvert(limcont)
      real*4 xvertb(limvert),yvertb(limvert),zcontb(limcont)
      integer*4 indvertb(limcont),nvertb(limcont)
      character*160 filea,fileb,filout,modelfile,tempfile,xffile,bmodel
      real*4 dxpat(limpat),dypat(limpat),dzpat(limpat),directErr(limpat)
      integer*4 ifdone(limpat),ixSeq(limpat),iySeq(limpat),izSeq(limpat)
      integer*4 idirSeq(limpat)
      real*4 dxDirect(limpat),dyDirect(limpat),dzDirect(limpat)
      real*4 ccDirect(limpat), corrCoef(limpat)
      real*4 xvertbsrc(4),yvertbsrc(4),xvsort(4),yvsort(4)
c       
      integer*4 indpat,ix,iy,iz,nxpatch,nypatch,nzpatch,maxshift
      integer*4 ifdebug,nonepatch,ncorrs,mode,numxpat,numypat,numzpat
      integer*4 nbxlo,nbxhi,nbylo,nbyhi,nbzlo,nbzhi,nxtap,nytap,nztap
      integer*4 ixstart,iystart,izstart,ixdelta,iydelta,izdelta
      integer*4 nbsrcxlo,nbsrcxhi,nbsrczlo,nbsrczhi,indv
      real*4 dmin,dmax,dmean,dum,a11,a12,a21,a22,dxsrc,dzsrc,xx,zz
      real*4 tmp, radius2, sigma1, sigma2, sigmak, delta
      integer*4 i,j,indyb,numSeq,nxctf,nyctf,nzctf,nxptmp,nyptmp,nzptmp
      integer*4 nxpad,nypad,nzpad,npixpatch,indpatcha
      integer*4 indpatchb,indloada,indloadb,maxxload,ncont
      integer*4 ierr,ifflip,numtot,ncontb,ifflipb
      real*4 xcen,ycen,zcen,xpatlo,xpathi,zpatlo,zpathi,dz,dzmin
      integer*4 indp,ifuse,icont,icmin,ixspan,izspan, kernelSize
      integer*4 izpstr,izpend,izdir,izpat,iz0,iz1,izcen
      integer*4 iypstr,iypend,iydir,iypat,iy0,iy1,iycen,ixpstr,ixpend
      integer*4 ixdir,ixpat,ix0,ix1,ixcen,nadj,ixadj,iyadj,izadj,inda
      integer*4 loady0,loady1,loadz0,loadz1,loadx0,loadx1,nmore
      integer*4 nxload,nyload,nzload,iseq,nnear,loadex,nskip
      integer*4 loadyb0,loadyb1,loadzb0,loadzb1,loadxb0,loadxb1
      integer*4 nxloadb,nyloadb,nzloadb,ixb0,ixb1,iyb0,iyb1,izb0,izb1
      integer*4 ixmin,ixmax,iymin,iymax,izmin,izmax,ifShiftIn,nadjlook
      integer*4 nxXCpad, nyXCpad, nzXCpad, nxXCbord, nyXCbord, nzXCbord
      real*4 asrc(3,3), dxyzsrc(3),dxnew,dynew,dznew,peak,wsumAdj,wsumNear
      real*4 dxsum,dysum,dzsum,err,perpos,dxadj,dyadj,dzadj,zmod, ymod,distAdj
      real*4 dxsNear,dysNear,dzsNear, dxNear, dyNear, dzNear,distsq,distNear
      real*4 ccRatio,wcc, sizeSwitch
      integer*4 niceframe

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoIntegers,PipGetThreeIntegers
      integer*4 PipGetString,PipGetFloat,PipgetThreeFloats,PipgetTwoFloats
      integer*4 PipGetInOutFile,PipGetLogical
c      
c       fallbacks from ../../manpages/autodoc2man -2 2  corrsearch3d
c       
      integer numOptions
      parameter (numOptions = 27)
      character*(40 * numOptions) options(1)

      indpat(ix,iy,iz)=ix + (iy-1)*numxpat + (iz-1)*numxpat*numypat

      options(1) =
     &    'ref:ReferenceFile:FN:@align:FileToAlign:FN:@'//
     &    'output:OutputFile:FN:@region:RegionModel:FN:@'//
     &    'size:PatchSizeXYZ:IT:@number:NumberOfPatchesXYZ:IT:@'//
     &    'xminmax:XMinAndMax:IP:@yminmax:YMinAndMax:IP:@'//
     &    'zminmax:ZMinAndMax:IP:@taper:TapersInXYZ:IT:@pad:PadsInXYZ:IT:@'//
     &    'maxshift:MaximumShift:I:@volume:VolumeShiftXYZ:FT:@'//
     &    'initial:InitialShiftXYZ:FT:@bsource:BSourceOrSizeXYZ:CH:@'//
     &    'bxform:BSourceTransform:FN:@bxborder:BSourceBorderXLoHi:IP:@'//
     &    'byzborder:BSourceBorderYZLoHi:IP:@bregion:BRegionModel:FN:@'//
     &    'kernel:KernelSigma:F:@ksize:KernelSize:F:@'//
     &    'lowpass:LowPassRadiusSigma:FP:@sigma1:HighPassSigma:F:@'//
     &    'messages:FlipYZMessages:B:@debug:DebugMode:I:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       IFDEBUG 1 for debugging output, 2 for dummy patch output at all
c       positions, 3 to do both search and fourier cross-correlation everywhere
c       
      ifdebug=0
      nonepatch=0
      ncorrs=0
      nxyzbsrc(1)=0
      nbsrcxlo = 36
      nbsrcxhi = 36
      nbsrczlo = 36
      nbsrczhi = 36
      maxshift = 10
      do i = 1, 3
        dxyzVol(i) = 0.
        dxyzInit(i) = 0.
      enddo
      modelfile = ' '
      xffile = ' '
      bmodel = ' '
      radius2 = 0.
      sigma1 = 0.
      sigma2 = 0.
      sigmak = 0.
      kernelSize = 3
      sizeSwitch = 1.49
      delta = 0.
      ifShiftIn = 0
      distAdj = 1.5
      distNear = 4.
      ccRatio = 0.33
      flipMessages = .false.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'corrsearch3d',
     &    'ERROR: CORRSEARCH3D - ', .true., 3, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
C       
C       Open image files.
C       
      if (PipGetInOutFile('ReferenceFile', 1, 'Image file to align to', filea)
     &    .ne. 0) call exiterror('NO REFERENCE FILE SPECIFIED')
      if (PipGetInOutFile('FileToAlign', 2, 'Image file being aligned', fileb)
     &    .ne. 0) call exiterror('NO FILE TO ALIGN SPECIFIED')
      if (PipGetInOutFile('OutputFile', 3, 'Output file for displacements',
     &    filout) .ne. 0)
     &    call exiterror('NO OUTPUT FILE FOR DISPLACEMENTS SPECIFIED')

      if (pipinput) then
        ierr = PipGetString('RegionModel', modelfile)
        ierr = PipGetString('BRegionModel', bmodel)
      else
        write(*,'(1x,a,$)')'Enter blank line : '
        read(5,50)tempfile
        print *,'Enter model file with contours enclosing areas to ',
     &      'analyze, or Return for none'
        read(5,50)modelfile
50      format(A)
      endif
c       
      CALL IMOPEN(1,filea,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      CALL IMOPEN(2,fileb,'RO')
      CALL IRDHDR(2,NXYZ2,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      if (pipinput) then
        if (PipGetThreeIntegers('PatchSizeXYZ', nxpatch,nypatch,nzpatch) .ne.
     &      0) call exitError('NO PATCH SIZE SPECIFIED')
        if (PipGetThreeIntegers('NumberOfPatchesXYZ', numxpat, numypat,numzpat)
     &      .ne. 0) call exitError('NO NUMBER OF PATCHES SPECIFIED')
        if (PipGetTwoIntegers('XMinAndMax', ixmin, ixmax) +
     &      PipGetTwoIntegers('YMinAndMax', iymin, iymax) +
     &      PipGetTwoIntegers('ZMinAndMax', izmin, izmax) .ne. 0) call
     &      exitError('MIN AND MAX COORDINATES IN X, Y, AND Z MUST BE ENTERED')
        nbxlo = ixmin - 1
        nbxhi = nx - ixmax
        nbylo = iymin - 1
        nbyhi = ny - iymax
        nbzlo = izmin - 1
        nbzhi = nz - izmax
        nxtap = (nxpatch + 9) / 10
        nytap = (nypatch + 9) / 10
        nztap = (nzpatch + 9) / 10
        ierr = PipGetThreeIntegers('TapersInXYZ', nxtap,nytap,nztap)
        ierr = PipGetInteger('MaximumShift', maxshift)
        call get_nxyz(.true., 'BSourceOrSizeXYZ', ' ', 5,nxyzbsrc) 
        ierr = PipGetTwoIntegers('BSourceBorderXLoHi', nbsrcxlo,nbsrcxhi)
        ierr = PipGetTwoIntegers('BSourceBorderYZLoHi', nbsrczlo,nbsrczhi)
        ierr = PipGetString('BSourceTransform', xffile)
        ifShiftIn = 1 - PipGetThreeFloats('VolumeShiftXYZ', dxVolume,
     &      dyVolume, dzVolume)
        ierr = PipGetThreeFloats('InitialShiftXYZ', dxInitial,
     &      dyInitial, dzInitial)
        ierr = PipGetTwoFloats('LowPassRadiusSigma', radius2, sigma2)
        ierr = PipGetFloat('HighPassSigma', sigma1)
        ierr = PipGetFloat('KernelSigma', sigmak)
        if (sigmak .gt. sizeSwitch) kernelSize = 5
        ierr = PipGetInteger('KernelSize', kernelSize)
        if (kernelSize .ne. 3 .and. kernelSize .ne. 5) call exitError(
     &      'KERNEL SIZE MUST BE 3 or 5')
        ierr = PipGetLogical('FlipYZMessages', flipMessages)
        ierr = PipGetInteger('DebugMode', ifdebug)
      else
        write(*,'(1x,a,$)')'X, Y, and Z size of patches: '
        read(5,*)nxpatch,nypatch,nzpatch
        write(*,'(1x,a,$)')'Number of patches in X, Y and Z: '
        read(5,*)numxpat, numypat,numzpat
        write(*,'(1x,a,$)')'Border sizes on lower and upper sides in X,'
     &      //' Y, and Z: '
        read(5,*)nbxlo,nbxhi,nbylo,nbyhi,nbzlo,nbzhi
        write(*,'(1x,a,$)')'Number of pixels to taper in X, Y, Z: '
        read(5,*)nxtap,nytap,nztap
        write(*,'(1x,a,$)')'Maximum shift to analyze for by searching: '
        read(5,*)maxshift
c         
c         get inputs for analyzing existence of data in B file
c       
        print *,'Enter name or NX, NY, NZ of the untransformed source',
     &      ' for the image file being aligned, or Return for none'
        call get_nxyz(.false., ' ', ' ', 5,nxyzbsrc)
        print *,'Enter name of file used to transform the image file',
     &      ' being aligned, or Return for none'
        read(5,50)xffile
        write(*,'(1x,a,/,a,$)')'Border sizes on lower and upper sides'
     &      //' in X and in Y or Z in the untransformed',
     &      ' source for the image file being aligned: '
        read(5,*)nbsrcxlo,nbsrcxhi,nbsrczlo,nbsrczhi
      endif
c       
c       If there is no initial shift entered, enforce centered transform
c       by setting initial shift to half the difference in size
c       
      if (ifShiftIn .eq. 0) then
        dxVolume = (nxyz2(1) - nxyz(1)) / 2.
        dyVolume = (nxyz2(2) - nxyz(2)) / 2.
        dzVolume = (nxyz2(3) - nxyz(3)) / 2.
      endif
c       
c       get padding for cross correlations
c
      nxXCbord = (nxpatch + 4) / 5
      nyXCbord = (nypatch + 4) / 5
      nzXCbord = (nzpatch + 4) / 5
      if (pipinput)
     &    ierr = PipGetThreeIntegers('PadsInXYZ', nxXCbord, nyXCbord, nzXCbord)
      nxXCpad = niceframe(nxpatch + 2 * nxXCbord, 2, 19)
      nyXCpad = niceframe(nypatch + 2 * nyXCbord, 2, 19)
      nzXCpad = niceframe(nzpatch + 2 * nzXCbord, 2, 19)
      if ((nxXCpad + 2) * nzXCpad .gt. limwork) call exitError(
     &    'PATCHES TOO LARGE FOR WORKING ARRAY FOR 3D FFT')
      nxctf = nxXCpad
c       
c       Initialize transform: since this is a center-based transform, 
c       neutralize component of volume coordinate shift due to size difference
      do i = 1,3
        do j = 1,3
          asrc(i,j) = 0.
        enddo
        dxyzsrc(i) = (nxyz2(i) - nxyz(i)) / 2. - dxyzVol(i)
        asrc(i,i) = 1.
      enddo
c
c       require b source dimensions if one is entered or
c       fall back to current B volume size; read in transform and add shifts
      if (nxyzbsrc(1) .eq. 0) then 
        if (xffile .ne. ' ') call exitError(
     &      'B SOURCE FILE DIMENSIONS MUST BE ENTERED TO USE 3D TRANSFORMS')
        do i = 1, 3
          nxyzbsrc(i) = nxyz2(i)
        enddo
      endif
c       
c       Read transform, add volume-shift component of center-based transform
      if (xffile .ne. ' ') then
        call dopen(1, xffile, 'ro', 'f')
        do i = 1, 3
          read(1,*,err=95, end =95) (asrc(i,j),j=1,3), tmp
          dxyzSrc(i) = dxyzSrc(i) + tmp
        enddo
        close(1)
      endif        
c       
c       get model contours as primary source of flip information and fallback 
c       to Y/Z dimensions
c
      ncont=0
      if (modelfile.ne.' ') then
        call get_region_contours(modelfile, 'CORRSEARCH3D', xvert, yvert,
     &      nvert, indvert, zcont, ncont, ifflip, limcont, limvert)
        
        if (ifdebug .gt.0) write(*,'(5(f7.0,f8.0))')
     &        (xvert(i), yvert(i),i = indvert(1), indvert(1) + nvert(1)-1)
      else
        ifflip = 0
        if (nz .gt. ny) ifflip = 1
      endif
c       
c       now that flip is known, get patch starting positions and delta between
c       patches 
c       
      indyb = 2
      if (ifflip .ne. 0 .and. flipMessages) indyb = 3
      call checkAndSetPatches(nx, nbxlo, nbxhi, nxpatch, numxpat,
     &    ixstart, ixdelta, 1)
      call checkAndSetPatches(ny, nbylo, nbyhi, nypatch, numypat,
     &    iystart, iydelta, indyb)
      call checkAndSetPatches(nz, nbzlo, nbzhi, nzpatch, numzpat,
     &    izstart, izdelta, 5 - indyb)


      ncontb = 0
      if (bmodel .ne. ' ') then
c         
c         For b model, get its coordinates in the source native section plane
c         then transform to coordinates in A volume native plane
c         
        print *,'Processing model on source for second volume'
        call get_region_contours(bmodel, 'CORRSEARCH3D', xvertb, yvertb,
     &      nvertb, indvertb, zcontb, ncontb, ifflipb, limcont, limvert)
        do j = 1, ncontb
          if (ifdebug .gt.0) write(*,'(5(f7.0,f8.0))')
     &        (xvertb(i), yvertb(i),i = indvertb(j), indvertb(j) + nvertb(j)-1)
          do i = indvertb(j), indvertb(j) + nvertb(j) - 1
            call xformBsrcToA(xvertb(i), yvertb(i),
     &          nxyzbsrc, nxyz, ifflipb, ifflip, asrc, dxyzsrc,
     &          xvertb(i), yvertb(i))
          enddo
          if (ifdebug .gt.0) write(*,'(5(f7.0,f8.0))')
     &        (xvertb(i), yvertb(i),i = indvertb(j), indvertb(j) + nvertb(j)-1)
        enddo
      else
        ifflipb = 0
        if (nxyzbsrc(3) .gt. nxyzbsrc(2)) ifflipb = 1
      endif
      indyb = 2
      if (ifflipb .ne. 0) indyb = 3
      if (ifdebug .gt. 0) print *,'flips',ifflip, ifflipb
c       
c       compute transformed locations of corners of b source volume
c       10/14/09: This used to be done inside the if below, but it is needed
c       for all evaluations of the patches.
      call xformBsrcToA(float(nbsrcxlo), float(nbsrczlo),
     &    nxyzbsrc, nxyz, ifflipb, ifflip, asrc, dxyzsrc,
     &    xvertbsrc(1), yvertbsrc(1))
      call xformBsrcToA(float(nxyzbsrc(1) - nbsrcxhi), float(nbsrczlo),
     &    nxyzbsrc, nxyz, ifflipb, ifflip, asrc, dxyzsrc,
     &    xvertbsrc(2), yvertbsrc(2))
      call xformBsrcToA(float(nxyzbsrc(1) - nbsrcxhi),
     &    float(nxyzbsrc(indyb) - nbsrczhi),
     &    nxyzbsrc, nxyz, ifflipb, ifflip, asrc, dxyzsrc,
     &    xvertbsrc(3), yvertbsrc(3))
      call xformBsrcToA(float(nbsrcxlo), float(nxyzbsrc(indyb) - nbsrczhi),
     &    nxyzbsrc, nxyz, ifflipb, ifflip, asrc, dxyzsrc,
     &    xvertbsrc(4), yvertbsrc(4))
      if (ifdebug .gt. 0)
     &    print *,'bverts',(xvertbsrc(i), yvertbsrc(i), i = 1,4)
      if (ifShiftIn .ne. 0 .or. xffile .ne. ' ') then
c         
c         now order the coordinates to find middle values that can be
c         used to adjust the entered lower and upper limits, so that the
c         basic grid can be set up to span between those limits
c         
        do i=1,4
          xvsort(i) = xvertbsrc(i)
          yvsort(i) = yvertbsrc(i)
        enddo
        do i=1,3
          do j=i+1,4
            if (xvsort(i) .gt. xvsort(j)) then
              tmp = xvsort(i)
              xvsort(i) = xvsort(j)
              xvsort(j) = tmp
            endif
            if (yvsort(i) .gt. yvsort(j)) then
              tmp = yvsort(i)
              yvsort(i) = yvsort(j)
              yvsort(j) = tmp
            endif
          enddo
        enddo
c         
c         compute revised lower and upper limits for X and Y/Z
c         
        call revisePatchRange(nx, nbxlo, nbxhi, xvsort(2), xvsort(3), nxpatch,
     &      numxpat, ixstart, ixdelta)
        if (ifflip .ne. 0) then
          call revisePatchRange(nz, nbzlo, nbzhi, yvsort(2), yvsort(3),
     &        nzpatch, numzpat, izstart, izdelta)
        else
          call revisePatchRange(ny, nbylo, nbyhi, yvsort(2), yvsort(3),
     &        nypatch, numypat, iystart, iydelta)
        endif
      endif
      if(numxpat*numypat*numzpat.gt.limpat)call exitError(
     &    'TOO MANY PATCHES FOR ARRAYS')
c       
      if (ifdebug.ne.0) then
        print *,'Scan limits in X:',ixstart,ixstart+(numxpat-1)*ixdelta
     &      +nxpatch
        print *,'Scan limits in Y:',iystart,iystart+(numypat-1)*iydelta
     &      +nypatch
        print *,'Scan limits in Z:',izstart,izstart+(numzpat-1)*izdelta
     &      +nzpatch
      endif
c       
c       prescan for patches inside boundaries, to get total count and flags
c       for whether to do
c       
      numtot=0
      do iz=1,numzpat
        zcen=izstart+(iz-1)*izdelta+nzpatch/2
        do iy=1,numypat
          ycen=iystart+(iy-1)*iydelta+nypatch/2
          ymod = ycen
          zmod = zcen
          if (ifflip .ne. 0) then
            zmod = ycen
            ymod = zcen
          endif
c           
          do ix=1,numxpat
            indp=indpat(ix,iy,iz)
            xcen=ixstart+(ix-1)*ixdelta+nxpatch/2
            ifuse=1
c
c             If A model was entered, find nearest contour in Z and see if 
c             patch is inside it
c               
            if (ncont.gt.0)then
              ifuse=0
              dzmin=100000.
              do icont=1,ncont
                dz=abs(zmod-zcont(icont))
                if(dz.lt.dzmin)then
                  dzmin=dz
                  icmin=icont
                endif
              enddo 
              indv=indvert(icmin)
              if(inside(xvert(indv),yvert(indv),nvert(icmin),xcen,ymod))
     &            ifuse=1
              if (ifuse .eq. 0 .and. ifdebug .eq.2)
     &            print *,xcen,ymod,' eliminated by A model contour',icmin

            endif
c             
c             Do the same if still ok and B model was entered
c
            if (ncontb.gt.0 .and. ifuse .eq. 1)then
              ifuse=0
              dzmin=100000.
              do icont=1,ncontb
                dz=abs(zmod-zcontb(icont))
                if(dz.lt.dzmin)then
                  dzmin=dz
                  icmin=icont
                endif
              enddo
              indv=indvertb(icmin)
              if(inside(xvertb(indv),yvertb(indv),nvertb(icmin),xcen,ymod))
     &            ifuse=1
              if (ifuse .eq. 0 .and. ifdebug .eq.2)
     &            print *,xcen,ymod,' eliminated by B model'
            endif
c
            if(ifuse .eq. 1)then
              ifuse=0
c               
c               now make sure all corners of the patch are inside transformed
c               area from B borders
c               
              xpatlo = xcen - (nxpatch - nxtap) / 2
              xpathi = xcen + (nxpatch - nxtap) / 2
              if (ifflip .ne. 0) then
                zpatlo = zcen - (nzpatch - nztap) / 2
                zpathi = zcen + (nzpatch - nztap) / 2
              else
                zpatlo = ycen - (nypatch - nytap) / 2
                zpathi = ycen + (nypatch - nytap) / 2
              endif
              if (inside(xvertbsrc,yvertbsrc,4,xpatlo,zpatlo) .and.
     &            inside(xvertbsrc,yvertbsrc,4,xpatlo,zpathi) .and.
     &            inside(xvertbsrc,yvertbsrc,4,xpathi,zpatlo) .and.
     &            inside(xvertbsrc,yvertbsrc,4,xpathi,zpathi)) ifuse = 1
              if (ifuse .eq. 0 .and. ifdebug .eq.2)
     &            print *,xcen,ymod,' eliminated by B boundaries'
            endif
            if(ifuse.eq.0)then
              ifdone(indp)=-1
            else
              ifdone(indp)=0
              numtot=numtot+1
            endif
            directErr(indp) = -1.
          enddo
        enddo
      enddo
c       
      if (numtot .lt. 1) call exitError(
     &    'NO PATCHES FIT WITHIN ALL OF THE CONSTRAINTS')
c       
c       set indexes at which to load data and compose patches
c       Here is the padding for the B patch direct correlation
c
      nxpad=nxpatch+2*(maxshift+1)
      nypad=nypatch+2*(maxshift+1)
      nzpad=nzpatch+2*(maxshift+1)
      npixpatch=nxpatch*nypatch*nzpatch
c       
c       Patch A space is set by Fourier correlation padding, B space by
c       max of Fourier and direct padded volumes
c
      indpatcha=1
      indpatchb = indpatcha + (nxXCpad + 2) * nyXCpad * nzXCpad
      indloada = indpatchb + max(nxpad*nypad*nzpad,
     &    (nxXCpad + 2) * nyXCpad*nzXCpad)
c       
c       get maximum load size: exload could be a separate parameter
c       
      loadex = 2 * (maxshift + 1)
      maxxload = (idim - indloada - loadex *
     &    (nypatch + loadex) * (nzpatch + loadex)) /
     &    (nypatch * nzpatch + (nypatch + loadex) * (nzpatch + loadex))
      if (maxxload .lt. nxpatch) call exitError('PATCHES TOO LARGE FOR ARRAYS')
      indloadb = indloada + maxxload * nypatch * nzpatch
c       print *,npixpatch,indpatcha,indpatchb,indloada,indloadb,
c       &           maxxload
c       
      call dopen(1,filout,'new','f')
      write(1,'(i7,a)')numtot,' positions'
c       
c       loop from center out in all directions, X inner, then short dimension,
c       then long dimension of Y and Z
c       
      if (ifflip .ne. 0) then
        call sequencePatches(numxpat, numypat, numzpat, ixSeq, iySeq,
     &      izSeq, idirSeq, numSeq)
      else
        call sequencePatches(numxpat, numzpat, numypat, ixSeq, izSeq,
     &      iySeq, idirSeq, numSeq)
      endif

      loadx1=-1
      loadxb1=-1
      nskip = 0
      do iseq = 1, numSeq
        ixpat = ixSeq(iseq)
        iypat = iySeq(iseq)
        izpat = izSeq(iseq)
        ixdir = idirSeq(iseq)
        iz0=izstart+(izpat-1)*izdelta
        iz1=iz0+nzpatch-1
        izcen=iz0+nzpatch/2
        iy0=iystart+(iypat-1)*iydelta
        iy1=iy0+nypatch-1
        iycen=iy0+nypatch/2
        ix0=ixstart+(ixpat-1)*ixdelta
        ix1=ix0+nxpatch-1
        ixcen=ix0+nxpatch/2
c         print *,'doing',ixcen,iycen,izcen
        indp=indpat(ixpat,iypat,izpat)
        if(ifdebug.eq.2.and.ifdone(indp).eq.0)then
          write(1,105)ixcen,iycen,izcen,2.,2.,2.
          ifdone(indp)=1
        endif

        if(ifdone(indp).eq.0)then
c           
c           find and average adjacent and nearby patches, weighting by the
c           correlation coefficient
c           
          nadj=0
          nnear = 0
          nadjlook = distNear + 1.
          dxsum=0.
          dysum=0.
          dzsum=0.
          dxsNear = 0.
          dysNear = 0.
          dzsNear = 0.
          wsumAdj = 0.
          wsumNear = 0.
          do ix = -nadjlook,nadjlook
            do iy = -nadjlook,nadjlook
              do iz = -nadjlook,nadjlook
                distsq = ix**2 + iy**2 + iz**2
                if (distsq .gt. 0 .and. distsq .le. distNear**2) then
                  ixadj=ixpat+ix
                  iyadj=iypat+iy
                  izadj=izpat+iz
                  if(ixadj.gt.0.and.ixadj.le.numxpat.and.
     &                iyadj.gt.0.and.iyadj.le.numypat.and.
     &                izadj.gt.0.and.izadj.le.numzpat)then
                    inda=indpat(ixadj,iyadj,izadj)
                    if(ifdone(inda).gt.0)then
                      nnear = nnear + 1
                      wcc = max(0.01, corrCoef(inda))
                      dxsNear = dxsNear + dxpat(inda) * wcc
                      dysNear = dysNear + dypat(inda) * wcc
                      dzsNear = dzsNear + dzpat(inda) * wcc
                      wsumNear = wsumNear + wcc
                      if (distsq .le. distAdj**2) then
                        nadj=nadj+1
                        dxsum=dxsum+dxpat(inda) * wcc
                        dysum=dysum+dypat(inda) * wcc
                        dzsum=dzsum+dzpat(inda) * wcc
                        wsumAdj = wsumAdj + wcc
                      endif
                    endif
                  endif
                endif
              enddo
            enddo
          enddo
c           nadj=max(1,nadj)
          dxadj = dxsum / max(.01, wsumAdj)
          dyadj = dysum / max(.01, wsumAdj)
          dzadj = dzsum / max(.01, wsumAdj)
          dxNear = dxsNear / max(.01, wsumNear)
          dyNear = dysNear / max(.01, wsumNear)
          dzNear = dzsNear / max(.01, wsumNear)
c           
c           Do direct search if there is something adjacent and its correlation
c           coefficients are good enough and it is not very deviant
c           
          if (nadj.gt.0 .and. wsumAdj / nadj .gt. ccRatio * wsumNear / nnear
     &        .and. abs(dxNear - dxadj) .lt. maxshift
     &        .and. abs(dyNear - dyadj) .lt. maxshift
     &        .and. abs(dzNear - dzadj) .lt. maxshift) then

            call setBload(ix0, ix1, nx2, dxadj, dxVolume, ixb0, ixb1)
            call setBload(iy0, iy1, ny2, dyadj, dyVolume, iyb0, iyb1)
            call setBload(iz0, iz1, nz2, dzadj, dzVolume, izb0, izb1)
            if (ix1 .gt. ix0 .and. iy1 .gt. iy0 .and. iz1 .gt. iz0 .and.
     &          (ix1+1-ix0)*(iy1+1-iy0)*(iz1+1-iz0) .ge. npixpatch / 2) then
c               
c               revise parameters based on load
c
              nxptmp = ix1 + 1 - ix0
              nyptmp = iy1 + 1 - iy0
              nzptmp = iz1 + 1 - iz0
              nxpad=nxptmp+2*(maxshift+1)
              nypad=nyptmp+2*(maxshift+1)
              nzpad=nzptmp+2*(maxshift+1)
c              print *,'direct',ixcen,iycen,izcen,nxptmp,nyptmp,nzptmp
              call flush(6)
c               
c               get the a patch from the loaded data into an exact fit,
c               taper, pad, kernel filter optionally and set to zero mean
c               
              call loadExtractProcess(1, buf, indloada, indpatcha, indpatchb,
     &            ix0, ix1, iy0, iy1, iz0, iz1, 0, ixdir, ixdelta, maxxload,
     &            nxyz, loadx0, loadx1, nxload, loady0, loady1, nyload, loadz0,
     &            loadz1, nzload, nxptmp, nyptmp, nzptmp, nxptmp, nxptmp,
     &            nyptmp, nzptmp, nxtap, nytap, nztap, kernelSize, sigmak)
c               
c               get the b patch from the loaded data padded to maximum shift
c               
              call loadExtractProcess(2, buf, indloadb, indpatchb, indpatchb,
     &            ixb0, ixb1, iyb0, iyb1, izb0, izb1, loadex, ixdir, ixdelta,
     &            maxxload, nxyz2, loadxb0, loadxb1, nxloadb, loadyb0,
     &            loadyb1, nyloadb, loadzb0, loadzb1, nzloadb, nxptmp,
     &            nyptmp, nzptmp, nxpad, nxpad, nypad, nzpad, nxtap, nytap,
     &            nztap, 3, 0.)
c               
              dxnew = 0.
              dynew = 0.
              dznew = 0.
c              call dumpVolume(buf(indpatcha), nxptmp, nxptmp,nyptmp,nzptmp,
c     &            'dumpa.')
c              call dumpVolume(buf(indpatchb), nxpad, nxpad,nypad,nzpad,
c     &            'dumpb.')
              call find_best_corr(buf(indpatcha),nxptmp, nyptmp,nzptmp, ix0,
     &            iy0,iz0,buf(indpatchb), nxpad,nypad,nzpad,ix0-maxshift-1,
     &            iy0- maxshift-1, iz0-maxshift-1, ix0,ix1,iy0,iy1,
     &            iz0,iz1,dxnew,dynew,dznew,maxshift,found,ncorrs)
              if(found)then
                ifdone(indp)=1
                dxpat(indp) = dxnew + nint(dxadj)
                dypat(indp) = dynew + nint(dyadj)
                dzpat(indp) = dznew + nint(dzadj)
                dxDirect(indp) = dxpat(indp)
                dyDirect(indp) = dypat(indp)
                dzDirect(indp) = dzpat(indp)
c                 
c                 compute correlation coefficient
c                 
                call oneCorrCoeff(buf(indpatcha),nxptmp, nyptmp,nzptmp,
     &              buf(indpatchb), nxpad,nypad,nzpad, nxptmp, nyptmp,nzptmp,
     &              dxnew, dynew, dznew, corrCoef(indp))
                ccDirect(indp) = corrCoef(indp)
              endif
            else
              ifdone(indp)=-1
              nskip = nskip + 1
            endif
          endif
c           
c           If there are no adjacent patches, or something was fishy,
c           do a full cross corr
c           
          if(ifdone(indp).eq.0.or.ifdebug.eq.3)then

            if (loadx1 .lt. 0 .and. loadxb1 .lt. 0)then
              dxNear = dxInitial
              dyNear = dyInitial
              dzNear = dzInitial
            endif
            call setBload(ix0, ix1, nx2, dxNear, dxVolume, ixb0, ixb1)
            call setBload(iy0, iy1, ny2, dyNear, dyVolume, iyb0, iyb1)
            call setBload(iz0, iz1, nz2, dzNear, dzVolume, izb0, izb1)
            if (ix1 .gt. ix0 .and. iy1 .gt. iy0 .and. iz1 .gt. iz0 .and.
     &          (ix1+1-ix0)*(iy1+1-iy0)*(iz1+1-iz0) .ge. npixpatch / 2) then
c               
c               Adjust load sizes and make new ctf if needed
c
              nxptmp = ix1 + 1 - ix0
              nyptmp = iy1 + 1 - iy0
              nzptmp = iz1 + 1 - iz0
              nxXCpad = niceframe(nxptmp + 2 * nxXCbord, 2, 19)
              nyXCpad = niceframe(nyptmp + 2 * nyXCbord, 2, 19)
              nzXCpad = niceframe(nzptmp + 2 * nzXCbord, 2, 19)
c              print *,'XC',nxptmp,nyptmp,nzptmp,nxXCpad,nyXCpad,nzXCpad,
c     &            ix0, ix1,iy0, iy1,iz0, iz1,ixb0, ixb1,iyb0, iyb1,izb0, izb1
              call flush(6)
              if ((radius2 .gt. 0. .or. sigma1 .ne. 0. .or. sigma2 .ne. 0).and.
     &            (nxctf .ne. nxXCpad .or. nyctf .ne. nyXCpad .or.
     &            nzctf .ne. nzXCpad)) then
                call setctfwsr(sigma1, sigma2, 0., radius2, ctf, nxXCpad,
     &              max(nyXCpad, nzXCpad), delta)
                nxctf = nxXCpad
                nyctf = nyXCpad
                nzctf = nzXCpad
              endif

c               
c               get the both patches from the loaded data padded for XCorr
c               
              call loadExtractProcess(1, buf, indloada, indpatcha, indpatchb,
     &            ix0, ix1, iy0, iy1, iz0, iz1, 0, ixdir, ixdelta,
     &            maxxload, nxyz, loadx0, loadx1, nxload, loady0,
     &            loady1, nyload, loadz0, loadz1, nzload, nxptmp,
     &            nyptmp, nzptmp, nxXCpad + 2, nxXCpad, nyXCpad, nzXCpad,
     &            nxtap, nytap, nztap, kernelSize, sigmak)
c              call dumpVolume(buf(indpatcha), nxXCpad + 2, nxXCpad,
c     &            nyXCpad, nzXCpad, 'dumpa.')
              call loadExtractProcess(2, buf, indloadb, indpatchb, indpatchb,
     &            ixb0, ixb1, iyb0, iyb1, izb0, izb1, loadex, ixdir, ixdelta,
     &            maxxload, nxyz2, loadxb0, loadxb1, nxloadb, loadyb0,
     &            loadyb1, nyloadb, loadzb0, loadzb1, nzloadb, nxptmp,
     &            nyptmp, nzptmp, nxXCpad + 2, nxXCpad, nyXCpad, nzXCpad,
     &            nxtap, nytap, nztap, 3, 0.)

c              call dumpVolume(buf(indpatchb), nxXCpad + 2, nxXCpad,
c     &            nyXCpad, nzXCpad, 'dumpb.')
              call fourierCorr(buf(indpatcha), buf(indpatchb), (nxXCpad + 2)/2,
     &            nyXCpad, nzXCpad, work, ctf, delta)
c              call dumpVolume(buf(indpatcha), nxXCpad + 2, nxXCpad,
c     &            nyXCpad, nzXCpad, 'dumpcorr.')

              call findXcorrPeak(buf(indpatcha), nxXCpad + 2, nyXCpad, nzXCpad,
     &            dxnew, dynew, dznew, peak)
              dxpat(indp) = dxnew + nint(dxNear)
              dypat(indp) = dynew + nint(dyNear)
              dzpat(indp) = dznew + nint(dzNear)
c               
c               For coef, reload the patches without the 2-pixel X padding
c
              call loadExtractProcess(1, buf, indloada, indpatcha, indpatchb,
     &            ix0, ix1, iy0, iy1, iz0, iz1, 0, ixdir, ixdelta,
     &            maxxload, nxyz, loadx0, loadx1, nxload, loady0,
     &            loady1, nyload, loadz0, loadz1, nzload, nxptmp,
     &            nyptmp, nzptmp, nxXCpad, nxXCpad, nyXCpad, nzXCpad,
     &            nxtap, nytap, nztap, kernelSize, sigmak)
              call loadExtractProcess(2, buf, indloadb, indpatchb, indpatchb,
     &            ixb0, ixb1, iyb0, iyb1, izb0, izb1, loadex, ixdir, ixdelta,
     &            maxxload, nxyz2, loadxb0, loadxb1, nxloadb, loadyb0,
     &            loadyb1, nyloadb, loadzb0, loadzb1, nzloadb, nxptmp,
     &            nyptmp, nzptmp, nxXCpad, nxXCpad, nyXCpad, nzXCpad,
     &            nxtap, nytap, nztap, 3, 0.)
              call oneCorrCoeff(buf(indpatcha),nxXCpad, nyXCpad, nzXCpad,
     &            buf(indpatchb), nxXCpad, nyXCpad, nzXCpad, nxptmp,
     &            nyptmp,nzptmp, dxnew, dynew, dznew, corrCoef(indp))

              if(ifdone(indp).eq.0) nonepatch=nonepatch+1
              if (ifdebug .eq. 3 .and. ifdone(indp) .gt. 0)
     &            directErr(indp)= sqrt((dxDirect(indp)-dxpat(indp))**2+
     &            (dyDirect(indp)-dypat(indp))**2+
     &            (dzDirect(indp)-dzpat(indp))**2)
              ifdone(indp)=1
            else if (ifdone(indp) .eq. 0) then
              ifdone(indp) = -1
              nskip = nskip + 1
            endif
          endif
          if (ifdone(indp) .gt. 0) then
            write(1,105)ixcen,iycen,izcen,dxpat(indp),dypat(indp),
     &          dzpat(indp), corrCoef(indp)
105         format(3i6,3f9.2,f10.4,3f9.2,f10.4,f8.3)
            call flush(1)
          endif
        endif
      enddo
c       
c       If any ones were skipped, need to rewrite the file
c
      if ((nskip .gt. 0 .or. ifdebug .eq. 3).and. ifdebug .ne. 2) then
        rewind(1)
        write(1,'(i7,a)')numtot-nskip,' positions'
        do iseq = 1, numSeq
          ixpat = ixSeq(iseq)
          iypat = iySeq(iseq)
          izpat = izSeq(iseq)
          indp=indpat(ixpat,iypat,izpat)
          if (ifdone(indp) .gt. 0) then
            izcen=izstart+(izpat-1)*izdelta+nzpatch/2
            iycen=iystart+(iypat-1)*iydelta+nypatch/2
            ixcen=ixstart+(ixpat-1)*ixdelta+nxpatch/2
            if(ifdebug.eq.3)then
              if(directErr(indp) .lt. 0.)then
                write(1,105)ixcen,iycen,izcen,dxpat(indp), dypat(indp),
     &              dzpat(indp), corrCoef(indp)
              else
                write(1,105)ixcen,iycen,izcen, dxpat(indp), dypat(indp),
     &              dzpat(indp),corrCoef(indp),dxDirect(indp),dyDirect(indp),
     &              dzDirect(indp),ccDirect(indp), directErr(indp)
              endif
            else
              write(1,105)ixcen,iycen,izcen,dxpat(indp),dypat(indp),
     &            dzpat(indp), corrCoef(indp)
            endif
          endif
        enddo
      endif
      close(1)
      perpos=(3.*ncorrs)/(numtot-nonepatch)
      write(*,'(f8.2,a,i5,a)')perpos,' correlations per position,'//
     &    ' Fourier correlations computed',nonepatch,' times' 
      call exit(0)
95    call exitError('READING TRANSFORM FILE')
      end


c       FIND_BEST_CORR will search for the best 3-D displacement between
c       two volumes in ARRAY and BRRAY.  ARRAY is dimensioned to NXA by NYA
c       by NZA, and its starting index coordinates are LOADAX0, LOADAY0,
c       LOADAZ0.  BRRAY is dimensioned to NXB by NYB by NZB, and its
c       starting index coordinates are LOADBX0, LOADBY0, LOADBZ0.  The volume
c       to be correlated is specified by starting and ending index
c       coordinates IX0, IX1, IY0, IY1, IZ0, IZ1.  DXADJ, DYADJ, DZADJ
c       should contain a starting shift upon entry, and will return the
c       shift with the best correlation.  MAXSHIFT specifies the maximum
c       shift that is allowed.  FOUND is returned as TRUE if a correlation
c       peak is found within the maximum shift.  NCORRS is a variable that
c       allows the calling program to maintain a count of the total number
c       of correlations computed.
c       
c       This was originally written to work with patches embedded in larger
c       loaded volumes, with the two volumes potentially loaded separately,
c       hence the unnecessary complexity for the program at hand.
c       
      subroutine find_best_corr(array,nxa,nya,nza,
     &    loadax0,loaday0,loadaz0, brray,nxb,nyb,nzb,
     &    loadbx0,loadby0,loadbz0,ix0,ix1,iy0,iy1, iz0,iz1,dxadj,
     &    dyadj,dzadj,maxshift, found,ncorrs)
      implicit none

      integer*4 nxa,nxb,nya,nyb,nza,nzb
      real*4 array(nxa,nya,nza),brray(nxb,nyb,nzb)
      real*8 corrs(-1:1,-1:1,-1:1),corrtmp(-2:2,-2:2,-2:2)
      real*8 corrmax
      logical done(-1:1,-1:1,-1:1),donetmp(-2:2,-2:2,-2:2)
      integer*4 idyseq(9)/0,-1,1,0,0,-1,1,-1,1/
      integer*4 idzseq(9)/0,0,0,1,-1,-1,-1,1,1/
      logical found
      integer*4 ix0,ix1,iy0,iy1, iz0,iz1,maxshift,ncorrs
      integer*4 loadax0,loadbx0,loaday0,loadby0,loadaz0,loadbz0
      real*4 dxadj,dyadj,dzadj
c       
      integer*4 idxglb,idyglb,idzglb,ix,iy,iz,iseq,idy,idz
      integer*4 idycor,idzcor,ix0cor,ix1cor,iy0cor,iy1cor,iz0cor,iz1cor
      integer*4 indmax
      real*4 cx,cy,cz,y1,y2,y3
      real*8 parabolicFitPosition
c       
c       get global displacement of b, including the load offset
c       
c       print *,'findbest',nxa,nya,nza,
c       &           loadax0,loaday0,loadaz0, nxb,nyb,nzb,
c       &           loadbx0,loadby0,loadbz0,ix0,ix1,iy0,iy1, iz0,iz1,dxadj,
c       &           dyadj,dzadj,maxshift
      idxglb=nint(dxadj)+loadax0-loadbx0
      idyglb=nint(dyadj)+loaday0-loadby0
      idzglb=nint(dzadj)+loadaz0-loadbz0
c       
c       clear flags for existence of corr
c       
      do iz=-1,1
        do iy=-1,1
          do ix=-1,1
            done(ix,iy,iz)=.false.
          enddo
        enddo
      enddo

      corrmax=-1.e30
      iseq=1
      do while(iseq.le.9)
        idy=idyseq(iseq)
        idz=idzseq(iseq)
c         print *,iseq,idy,idz
        if(.not.(done(-1,idy,idz).and.done(0,idy,idz).and.
     &      done(1,idy,idz)))then
c           
c           if the whole row does not exist, do the correlations
c           limit the extent if b is displaced and near an edge
c           
          idycor=idyglb+idy
          idzcor=idzglb+idz
          ix0cor=max(ix0-loadax0,-(idxglb-1))
          ix1cor=min(ix1-loadax0,nxb-1-(idxglb+1))
          iy0cor=max(iy0-loaday0,-idyglb)
          iy1cor=min(iy1-loaday0,nyb-1-idyglb)
          iz0cor=max(iz0-loadaz0,-idzglb)
          iz1cor=min(iz1-loadaz0,nzb-1-idzglb)
          ncorrs=ncorrs+1
          call threecorrs(array,nxa,nya,brray,nxb,nyb,ix0cor,ix1cor,
     &        iy0cor,iy1cor,iz0cor,iz1cor,idxglb,idycor,idzcor,
     &        corrs(-1,idy,idz),corrs(0,idy,idz),corrs(1,idy,idz))
          done(-1,idy,idz)=.true.
          done(0,idy,idz)=.true.
          done(1,idy,idz)=.true.
        endif
        if(corrs(0,idy,idz).gt.corrs(-1,idy,idz).and.
     &      corrs(0,idy,idz).gt.corrs(1,idy,idz))then
          indmax=0
        elseif(corrs(-1,idy,idz).gt.corrs(0,idy,idz).and.
     &        corrs(-1,idy,idz).gt.corrs(1,idy,idz))then
          indmax=-1
        else
          indmax=1
        endif
        if(corrs(indmax,idy,idz).gt.corrmax)then
c           print *,'moving by',indmax,idy,idz
          corrmax=corrs(indmax,idy,idz)
c           
c           if there is a new maximum, shift the done flags and the existing
c           correlations, and reset the sequence
c           
          idxglb=idxglb+indmax
          idyglb=idyglb+idy
          idzglb=idzglb+idz
c           
c           but if beyond the limit, return failure
c           
          if(max(abs(idxglb+loadbx0-loadax0),abs(idyglb+loadby0-loaday0),
     &        abs(idzglb+loadbz0-loadaz0)).gt.maxshift)then
            found=.false.
            return
          endif
          do iz=-1,1
            do iy=-1,1
              do ix=-1,1
                donetmp(ix,iy,iz)=.false.
              enddo
            enddo
          enddo
          do iz=-1,1
            do iy=-1,1
              do ix=-1,1
                donetmp(ix-indmax,iy-idy,iz-idz)=done(ix,iy,iz)
                corrtmp(ix-indmax,iy-idy,iz-idz)=corrs(ix,iy,iz)
              enddo
            enddo
          enddo
          do iz=-1,1
            do iy=-1,1
              do ix=-1,1
                done(ix,iy,iz)=donetmp(ix,iy,iz)
                corrs(ix,iy,iz)=corrtmp(ix,iy,iz)
              enddo
            enddo
          enddo
          if(indmax.ne.0.or.idy.ne.0.or.idz.ne.0)iseq=0
        endif
        iseq=iseq+1
      enddo
c       
c       do independent parabolic fits in 3 dimensions
c       
      y1=corrs(-1,0,0)
      y2=corrs(0,0,0)
      y3=corrs(1,0,0)
      cx = parabolicFitPosition(y1, y2, y3)
      y1=corrs(0,-1,0)
      y3=corrs(0,1,0)
      cy = parabolicFitPosition(y1, y2, y3)
      y1=corrs(0,0,-1)
      y3=corrs(0,0,1)
      cz = parabolicFitPosition(y1, y2, y3)
c       
      dxadj=idxglb+cx+loadbx0-loadax0
      dyadj=idyglb+cy+loadby0-loaday0
      dzadj=idzglb+cz+loadbz0-loadaz0
      found=.true.
c       print *,'returning a peak'
      return
      end


c       THREECORRS computes three correlations between volumes in ARRAY
c       and BRRAY.  The volume in ARRAY is dimensioned to NXA by NYA, that
c       in BRRAY is dimensioned to NXB by NYB.  The starting and ending
c       index coordinates (numbered from 0) in ARRAY over which the 
c       correlations are to be computed are IX0, IX1, IY0, IY1, IZ0, IZ1.
c       The shift between coordinates in ARRAY and coordinates in B is given
c       by IDX, IDY, IDZ.  The three correlations are returned in CORR1 (for
c       IDX-1), CORR2 (for IDX), and CORR3 (for IDX+1).
c       
c       On both the SGI and the PC, three correlations can be computed at
c       once almost as fast as one can.  This technique thus speeds up the
c       overall search by almost a factor of 3.
c       
      subroutine threecorrs(array,nxa,nya,brray,nxb,nyb,ix0,ix1,
     &    iy0,iy1,iz0,iz1,idx,idy,idz,corr1,corr2,corr3)
      implicit none
      real*4 array(*),brray(*)
      real*8 sum1,sum2,sum3,corr1,corr2,corr3
      integer*4 ix0,ix1,iy0,iy1, iz0,iz1,idx,idy,idz,nxa,nxb,nya,nyb
      integer*4 iz,izb,iy,iyb,indbasea,inddelb,ix,ixb,nsum
      sum1=0.
      sum2=0.
      sum3=0.
      do iz=iz0,iz1
        izb=iz+idz
        do iy=iy0,iy1
          iyb=iy+idy
          indbasea=1+iy*nxa+iz*nxa*nya
          inddelb=1+iyb*nxb+izb*nxb*nyb+idx-indbasea
          
          do ix=indbasea+ix0,indbasea+ix1
            ixb=ix+inddelb
            sum1=sum1+array(ix)*brray(ixb-1)
            sum2=sum2+array(ix)*brray(ixb)
            sum3=sum3+array(ix)*brray(ixb+1)
          enddo
        enddo
      enddo     
      nsum=(iz1+1-iz0)*(iy1+1-iy0)*(ix1+1-ix0)
      corr1=sum1/nsum
      corr2=sum2/nsum
      corr3=sum3/nsum
c       print *,idx,idy,idz,corr1,corr2,corr3
      return
      end

c       This was a more straightforward version for testing THREECORRS
c       
c$$$    subroutine safecorrs(array,nxa,nya,brray,nxb,nyb,ix0,ix1,
c$$$    &           iy0,iy1,iz0,iz1,idx,idy,idz,corr1,corr2,corr3)
c$$$    real*4 array(nxa,nya,*),brray(nxb,nyb,*)
c$$$    real*8 sum1,sum2,sum3,corr1,corr2,corr3
c$$$    c         
c$$$    c       print *,nxa,nya,nxb,nyb,ix0,ix1, iy0,iy1,iz0,iz1
c$$$    sum1=0.
c$$$    sum2=0.
c$$$    sum3=0.
c$$$    do iz=iz0+1,iz1+1
c$$$    izb=iz+idz
c$$$    do iy=iy0+1,iy1+1
c$$$    iyb=iy+idy
c$$$    c             print *,array(ix0+1,iy,iz),brray(ix0+idx+1,iyb,izb)
c$$$    do ix=ix0+1,ix1+1
c$$$    ixb=ix+idx
c$$$    c             print *,array(ix,iy,iz),brray(ixb,iyb,izb)
c$$$    sum1=sum1+array(ix,iy,iz)*brray(ixb-1,iyb,izb)
c$$$    sum2=sum2+array(ix,iy,iz)*brray(ixb,iyb,izb)
c$$$    sum3=sum3+array(ix,iy,iz)*brray(ixb+1,iyb,izb)
c$$$    enddo
c$$$    enddo
c$$$    enddo   
c$$$    nsum=(iz1+1-iz0)*(iy1+1-iy0)*(ix1+1-ix0)
c$$$    corr1=sum1/nsum
c$$$    corr2=sum2/nsum
c$$$    corr3=sum3/nsum
c$$$    c       print *,idx,idy,idz,corr1,corr2,corr3
c$$$    return
c$$$    end


c       oneCorrCoeff computes one correlation coefficient between volumes in
c       ARRAY and BRRAY.  The volume in ARRAY is dimensioned to NXA by NYA by
c       NZA, that in BRRAY is dimensioned to NXB by NYB by NZB.  The size of
c       volume to be correlated is NXPATCH by NYPATCH by NZPATCH and it is
c       assumed to be centered in the arrays.  The shift between coordinates
c       in ARRAY and coordinates in B is given by DX, DY, DZ.  The correlation
c       coefficient is returned in CORR2.
c
      subroutine oneCorrCoeff(array, nxa, nya, nza, brray,nxb,nyb,nzb,
     &    nxpatch,nypatch,nzpatch,dx,dy,dz,corr2)
      implicit none
      real*4 array(*),brray(*),corr2,dx,dy,dz,denom
      real*8 asum, sum2, bsum2, asumsq, bsumsq2
      integer*4 ix0,ix1,iy0,iy1, iz0,iz1,idx,idy,idz,nxa,nxb,nya,nyb,nza,nzb
      integer*4 iz,izb,iy,iyb,indbasea,inddelb,ix,ixb,nsum
      integer*4 nxpatch,nypatch,nzpatch
      sum2=0.
      asum = 0.
      bsum2 = 0.
      asumsq = 0.
      bsumsq2 = 0.
      idx = nint(dx) + (nxb - nxa) / 2
      ix0 = (nxa - nxpatch) / 2
      ix1 = min(nxpatch + ix0 - 1, nxb - 1 - idx) 
      ix0 = max(ix0, -idx)
      idy = nint(dy) + (nyb - nya) / 2
      iy0 = (nya - nypatch) / 2
      iy1 = min(nypatch + iy0 - 1, nyb - 1 - idy) 
      iy0 = max(iy0, -idy)
      idz = nint(dz) + (nzb - nza) / 2
      iz0 = (nza - nzpatch) / 2
      iz1 = min(nzpatch + iz0 - 1, nzb - 1 - idz) 
      iz0 = max(iz0, -idz)
      do iz=iz0,iz1
        izb=iz+idz
        do iy=iy0,iy1
          iyb=iy+idy
          indbasea=1+iy*nxa+iz*nxa*nya
          inddelb=1+iyb*nxb+izb*nxb*nyb+idx-indbasea

          do ix=indbasea+ix0,indbasea+ix1
            ixb=ix+inddelb
            sum2=sum2+array(ix)*brray(ixb)
            asum = asum + array(ix)
            asumsq = asumsq + array(ix)**2
            bsum2 = bsum2 + brray(ixb)
            bsumsq2 = bsumsq2 + brray(ixb)**2
          enddo
        enddo
      enddo	
      nsum=(iz1+1-iz0)*(iy1+1-iy0)*(ix1+1-ix0)
      denom = (nsum * asumsq - asum**2) * (nsum * bsumsq2 - bsum2**2)
c       
c       Set the ccc to 0 if the denominator is illegal, otherwise limit it
c       to +/-1
      if (denom .le. 0.) then
        corr2 = 0.
      else
        denom = sqrt(denom)
        corr2 = (nsum * sum2 - asum * bsum2)
        if (denom .lt. corr2) then
          corr2 = sign(1., corr2)
        else
          corr2 = corr2 / denom
        endif
      endif
c	print *,idx,idy,idz,corr2
      return
      end

       
c       kernelSmooth applies a 3D gaussian kernel to the data in ARRAY and
c       places the result in BRRAY.  The image size is NX x NY x NZ and the
c       dimensions of the arrays are NXDIM x NY x NZ.  ISIZE specifies the
c       kernel size (3 or 5) and sigma is the standard deviation of the
c       Gaussian
c
      subroutine kernelSmooth(array, brray, nxdim, nx, ny, nz, isize, sigma)
      implicit none
      integer*4 nx, nxdim, ny, nz, ix, iy, iz, i, j, k, isize, mid, less
      real*4 array(nxdim, ny, nz), brray(nxdim, ny, nz), sigma
      real*4 w(5,5,5), wsum
c       
c       Make up the gaussian kernel
c       
      mid = (isize + 1) / 2
      wsum = 0.
      do i = 1,isize
        do j = 1,isize
          do k = 1,isize
            w(i,j,k) = exp(-((i - mid)**2 + (j - mid)**2 + (k - mid)**2) /
     &          sigma**2)
            wsum = wsum + w(i,j,k)
          enddo
        enddo
      enddo
      do i = 1,isize
        do j = 1,isize
          do k = 1,isize
            w(i,j,k) = w(i, j,k) / wsum
          enddo
        enddo
      enddo
c       
c       Form the weighted sums: this formulation is 3 times faster
c       than adding everything into one output pixel at a time
c
      do iz = 0, nz - isize
        do iy = 0, ny - isize
          do ix = 0, nx - isize
            brray(ix + mid, iy + mid, iz + mid) = 0.
          enddo
          do k = 1,isize
            do j = 1,isize
              if (isize .eq. 3) then
                do ix = 0, nx - 3
                  brray(ix + 2, iy + 2, iz + 2) =  brray(ix + 2, iy + 2, iz+2)+
     &                array(ix + 1, iy + j, iz + k) * w(1, j, k) +
     &                array(ix + 2, iy + j, iz + k) * w(2, j, k) +
     &                array(ix + 3, iy + j, iz + k) * w(3, j, k)
                enddo
              else
                do ix = 0, nx - 5
                  brray(ix + 3, iy + 3, iz + 3) =  brray(ix + 3, iy + 3, iz+3)+
     &                array(ix + 1, iy + j, iz + k) * w(1, j, k) +
     &                array(ix + 2, iy + j, iz + k) * w(2, j, k) +
     &                array(ix + 3, iy + j, iz + k) * w(3, j, k) +
     &                array(ix + 4, iy + j, iz + k) * w(4, j, k) +
     &                array(ix + 5, iy + j, iz + k) * w(5, j, k)
                enddo
              endif
            enddo
          enddo
        enddo
      enddo
c       
c       Copy the walls of the volume
c       
      less = (mid - 1) / 2
      do iz = 1, nz - less, nz - less -1
        do iy = 1, ny
          do ix = 1,nx
            brray(ix,iy,iz) = array(ix,iy,iz)
          enddo
          if (isize .gt. 3) then
            do ix = 1,nx
              brray(ix,iy,iz + 1) = array(ix,iy,iz + 1)
            enddo
          endif
        enddo
      enddo
      do iy = 1, ny - less, ny - less - 1
        do iz = 1, nz
          do ix = 1,nx
            brray(ix,iy,iz) = array(ix,iy,iz)
          enddo
          if (isize .gt. 3) then
            do ix = 1,nx
              brray(ix,iy + 1,iz) = array(ix,iy + 1,iz)
            enddo
          endif
        enddo
      enddo
      do ix = 1, nx - less, nx - less - 1
        do iz = 1, nz
          do iy = 1, ny
            brray(ix,iy,iz) = array(ix,iy,iz)
          enddo
          if (isize .gt. 3) then
            do iy = 1, ny
              brray(ix + 1,iy,iz) = array(ix + 1,iy,iz)
            enddo
          endif
        enddo
      enddo
      return
      end

       
c       Computes a cross-correlation between volumes in ARRAY and BRRAY 
c       via fourier transforms and filters by the values in CTF if DELTA,
c       the frequency spacing of the points in CTF, is nonzero.  NXDIM is
c       the X dimension of the FFT, NY and NZ are the Y and Z image dimensions.
c       WORK must be dimensioned at least NXDIM x NZ.
c
      subroutine fourierCorr(array, brray, nxdim, ny, nz, work, ctf, delta)
      implicit none
      integer*4 nxdim, ny, nz
      real*4 ctf(*), delta, work
      complex array(nxdim, ny, nz), brray(nxdim, ny, nz)
      integer*4 jx, jy, jz, nxReal, indf
      real*4 delx, dely,delz, xa, ya, za, s
c       
c       Get nx in real space and take 3D FFT's
c
      nxReal = 2 * (nxdim - 1)
      call thrdfft(array, work, nxReal, ny, nz, 0) 
      call thrdfft(brray, work, nxReal, ny, nz, 0)
c       
c       multiply complex conjugate of array by brray, put back in array
c       This is different from usual so that we will get the amount b is
c       displaced from A, not the amount to shift B to align to A
c
      do jz = 1, nz
        do jy = 1, ny
          do jx = 1, nxdim
            array(jx, jy, jz) = conjg(array(jx, jy, jz)) * brray(jx, jy, jz)
          enddo
        enddo
      enddo
c       
c       Filter if delta set
c
      if (delta .gt. 0.) then
	delx=0.5/(nxdim-1.)
	dely=1./ny
	delz=1./nz
        do jz = 1, nz
          za = (jz - 1) * delz
          if (za .gt. 0.5) za = 1. - za
          do jy = 1, ny
            ya = (jy - 1) * dely
            if (ya .gt. 0.5) ya = 1. - ya
            do jx = 1, nxdim
              xa = (jx - 1) * delx
              s = sqrt(xa**2 + ya**2 + za**2)
              indf = s / delta + 1.5
              array(jx, jy, jz) = array(jx, jy, jz) * ctf(indf)
            enddo
          enddo
        enddo
      endif
      call thrdfft(array, work, nxReal, ny, nz, -1) 
      return
      end


c       findXcorrPeak finds the peak in a cross-correlation in ARRAY,
c       dimensioned to NXDIM x NY x NZ and image size NXDIM-2, NY, NZ.
c       It fits a parabola in each dimension to get interpolated peak
c       positions in XPEAK, YPEAK, ZPEAK, and returns peak magnitude in PEAK.
c
      subroutine findXcorrPeak(array, nxdim, ny, nz, xpeak, ypeak, zpeak, peak)
      implicit none
      integer*4 nxdim, ny, nz
      real*4 array(nxdim, ny, nz), xpeak, ypeak, zpeak, peak
      integer*4 ix, iy, iz, ixpeak,iypeak, izpeak, nx
      real*4 cx, cy, cz, y1, y2, y3
      integer*4 indmap
      real*8 parabolicFitPosition
c       
      nx = nxdim - 2
      peak = -1.e30
      do iz = 1, nz
        do iy = 1, ny
          do ix = 1, nx
            if (array(ix, iy, iz) .gt. peak) then
              peak = array(ix, iy, iz)
              ixpeak = ix
              iypeak = iy
              izpeak = iz
            endif
          enddo
        enddo
      enddo
c       
c       simply fit a parabola to the two adjacent points in X or Y or Z
c       
      y1=array(indmap(ixpeak-1,nx),iypeak,izpeak)
      y2=peak
      y3=array(indmap(ixpeak+1,nx),iypeak,izpeak)
      cx = parabolicFitPosition(y1, y2, y3)

      y1=array(ixpeak,indmap(iypeak-1,ny),izpeak)
      y3=array(ixpeak,indmap(iypeak+1,ny),izpeak)
      cy = parabolicFitPosition(y1, y2, y3)

      y1=array(ixpeak,iypeak,indmap(izpeak-1,nz))
      y3=array(ixpeak,iypeak,indmap(izpeak+1,nz))
      cz = parabolicFitPosition(y1, y2, y3)
c       
c       return adjusted pixel coordinate minus 1
c       
      xpeak=ixpeak+cx-1.
      ypeak=iypeak+cy-1.
      zpeak=izpeak+cz-1.
      if(xpeak.gt.nx/2)xpeak=xpeak-nx
      if(ypeak.gt.ny/2)ypeak=ypeak-ny
      if(zpeak.gt.nz/2)zpeak=zpeak-nz
      return
      end

c       setBload takes the desired coordinates on an axis, IX0 and IX1, the
c       size in that dimension, NX2, the incremental and initial offsets
c       DXADJ and DXINITIAL, and computes the limits for data that need
c       to be loaded from B in IXB0, IXB1.  It adjusts IX0 and IX1 as
c       necessary to keep everything within limits
c
      subroutine setBload(ix0, ix1, nx2, dxadj, dxVolume, ixb0, ixb1)
      implicit none
      integer*4 ix0, ix1, nx2, ixb0, ixb1, idxadj, idxVolume
      real*4 dxadj, dxVolume
      idxadj = nint(dxadj)
      idxVolume = nint(dxVolume)
      ixb0 = max(0, ix0 + idxadj + idxVolume)
      ix0 = ixb0 - idxadj - idxVolume
      ixb1 = min(nx2 - 1, ix1 + idxadj + idxVolume)
      ix1 = ixb1 - idxadj - idxVolume
      return
      end

c       loadExtractProcess takes care of loading data as needed from the
c       given unit IUNIT into the right area of BUF, extracting the desired
c       patch from there, tapering it, and smoothing via a scratch patch
c       area if sepecified
c
      subroutine loadExtractProcess(iunit, buf, indloada, indpatcha,
     &    indScratch, ix0, ix1, iy0, iy1, iz0, iz1, loadex, ixdir, ixdelta,
     &    maxxload, nxyz, loadx0, loadx1, nxload, loady0, loady1, nyload,
     &    loadz0, loadz1, nzload, nxpatch,nypatch, nzpatch, nxpadDim, nxpad,
     &    nypad, nzpad, nxtap, nytap,nztap, kernelSize, sigmak)
      implicit none
      real*4 buf(*), sigmak
      integer*4 indloada, indpatcha, indScratch, ix0, ix1, iy0, iy1, iz0, iz1
      integer*4 loadex, ixdir, ixdelta, maxxload, nxyz, loadx0, loadx1, nxload
      integer*4 loady0, loady1, nyload, loadz0, loadz1, nzload, nxpatch,nypatch
      integer*4 nzpatch, nxpadDim, nxpad, nypad, nzpad, nxtap, nytap,nztap
      integer*4 indTaper, iunit, kernelSize

      call manageLoad(iunit, buf(indloada), ix0, ix1, iy0, iy1, iz0, iz1,
     &    loadex / 2, ixdir, ixdelta, maxxload, nxyz, loadx0, loadx1, nxload,
     &    loady0, loady1, nyload, loadz0, loadz1, nzload)
c       
c       get the patch from the loaded data; taper inside
c       and shift mean to zero
c               
      call extract_patch(buf(indloada),nxload, nyload, nzload, loadx0, loady0,
     &    loadz0,ix0,iy0, iz0, buf(indpatcha),nxpatch,nypatch, nzpatch)
      indtaper = indpatcha
      if (sigmak .gt. 0.) indtaper = indScratch
      call taperinvol(buf(indpatcha),nxpatch, nypatch, nzpatch, buf(indtaper),
     &    nxpadDim,nxpad, nypad, nzpad, nxtap, nytap,nztap)

      if (sigmak .gt. 0.) then
c         call dumpVolume(buf(indtaper), nxpadDim, nxpad, nypad, nzpad,
c     &      'taper.')
        call kernelSmooth(buf(indtaper), buf(indpatcha),
     &      nxpadDim, nxpad, nypad, nzpad, kernelSize, sigmak)
c         call dumpVolume(buf(indpatcha), nxpadDim, nxpad, nypad, nzpad,
c     &      'smooth.')
      endif
      call volmeanzero(buf(indpatcha),nxpadDim, nxpad, nypad, nzpad)
      return
      end

c       MANAGELOAD tests whether the desired volume specified by IX0, IX1,
c       IY0, IY1, IZ0, IZ1 is already loaded, given the loaded limits in
c       LOADX0, etc.  If not, it loads the data, with extra amounts specified
c       by LOADEXH and a maximum load in X specified by MAXXLOAD
c
      subroutine manageLoad(iunit,buf, ix0, ix1, iy0, iy1, iz0, iz1, loadexh,
     &    ixdir, ixdelta, maxxload, nxyz, loadx0, loadx1, nxload, loady0,
     &    loady1, nyload, loadz0, loadz1, nzload)
      implicit none
      real*4 buf(*)
      integer*4 ix0, ix1, iy0, iy1, iz0, iz1, loadexh, ixdir, ixdelta, maxxload
      integer*4 loadx0, loadx1, nxload, loady0, loady1, nyload, loadz0, loadz1
      integer*4 nzload, nmore, nxyz(3), iunit
c       
      if(ix0.ge.loadx0.and.ix1.le.loadx1.and.
     &    iy0.ge.loady0.and.iy1.le.loady1.and.
     &    iz0.ge.loadz0.and.iz1.le.loadz1) return
c       
c       need to load new data
c       
      loady0=max(0, iy0 - loadexh)
      loady1=min(nxyz(2) - 1, iy1 + loadexh)
      loadz0=max(0, iz0 - loadexh)
      loadz1=min(nxyz(3) - 1, iz1 + loadexh)
c       
c       compute limits in X, loading as much as possible
c       but limiting to edge of data and then truncating
c       to the end of a patch
c       
      if(ixdir.gt.0)then
        loadx0=max(0, ix0 - loadexh)
        loadx1=min(nxyz(1)-1,ix0+maxxload-1 + loadexh)
        nmore=(loadx1 - loadexh - ix1) / ixdelta
        loadx1=min(nxyz(1)-1, ix1+ixdelta*nmore + loadexh)
      else
        loadx1=min(nxyz(1)-1, ix1 + loadexh)
        loadx0=max(0,ix1+1-maxxload - loadexh)
        nmore=(ix0-loadx0 + loadexh)/ixdelta
        loadx0=max(0,ix0-ixdelta*nmore - loadexh)
      endif
c      print *,'loading data',iunit,ix0, ix1, iy0, iy1, iz0, iz1,loadx0,
c     &    loadx1,  loady0, loady1, loadz0, loadz1
      nxload=loadx1+1-loadx0
      nyload=loady1+1-loady0
      nzload=loadz1+1-loadz0
      call loadvol(iunit,buf,nxload, nyload,loadx0, loadx1, loady0, loady1,
     &    loadz0,loadz1)
      return
      end


c       LOADVOL loads a subset of the volume from unit IUNIT, into ARRAY
c       assuming dimensions of NXDIM by NYDIM, from index coordinates
c       IX0, IX1, IY0, IY1, IZ0, IZ1.
c       
      subroutine loadvol(iunit,array,nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1)
      implicit none
      integer*4 nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1,iunit,indz,iz
      real*4 array(nxdim,nydim,*)
c       
c       print *,iunit,nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1
      indz=0
      do iz=iz0,iz1
        indz=indz+1
        call imposn(iunit,iz,0)
        call irdpas(iunit,array(1,1,indz),nxdim,nydim,ix0,ix1,iy0,iy1,*99)
      enddo
      return
99    call exitError('ERROR READING FILE')
      end


c       EXTRACT_PATCH extracts a patch of dimensions NXPATCH by NYPATCH by
c       NZPATCH into ARRAY from the loaded volume in BUF, whose dimensions
c       are NXLOAD by NYLOAD by NZLOAD.  BUF is loaded from starting index
c       coordinates LOADX0, LOADY0, LOADZ0, and the starting index
c       coordinates of the patch are IX0, IY0, IZ0.
c       
      subroutine extract_patch(buf,nxload,nyload,nzload,
     &    loadx0,loady0,loadz0,ix0,iy0,
     &    iz0,array,nxpatch,nypatch,nzpatch)
      implicit none
      integer*4 nxload,nyload,nzload,loadx0,loady0,loadz0,ix0,iy0
      integer*4 iz0,nxpatch,nypatch,nzpatch
      real*4 buf(nxload,nyload,nzload),array(nxpatch,nypatch,nzpatch)
      integer*4 iz, izfrom,iy,iyfrom,ix
c       
      do iz=1,nzpatch
        izfrom=iz+iz0-loadz0
        do iy=1,nypatch
          iyfrom=iy+iy0-loady0
          do ix=1,nxpatch
            array(ix,iy,iz)=buf(ix+ix0-loadx0,iyfrom,izfrom)
          enddo
        enddo
      enddo
      return
      end

      
c       VOLMEANZERO shifts the mean to zero of the volume in ARRAY
c       dimensioned NXDIM by NY by NZ, image size NX by NY by NZ
c       
      subroutine volmeanzero(array,nxdim,nx,ny,nz)
      implicit none
      integer*4 nxdim,nx,ny,nz
      real*4 array(nxdim,ny,nz)
      real*8 sum
      integer*4 i,j,k
      real*4 dmean
      sum=0.
      do k = 1, nz
        do j = 1, ny
          do i=1,nx
            sum=sum+array(i,j,k)
          enddo
        enddo
      enddo  
      dmean=sum/(nx*ny*nz)
      do k = 1, nz
        do j = 1, ny
          do i=1,nx
            array(i,j,k)=array(i,j,k)-dmean
          enddo
        enddo
      enddo  
      return
      end


c       checkAndSetPatches does error checks and sets the basic start
c       and delta for the patches in one dimension.
c       
      subroutine checkAndSetPatches(nx, nbxlo, nbxhi, nxpatch, numxpat,
     &    ixstart, ixdelta, iaxis)
      implicit none
      integer*4 nx, nbxlo, nbxhi, nxpatch, numxpat, ixstart, ixdelta, iaxis
      character*6 axis
      character*320 concat
c       
c       check basic input properties
c       
      axis = char(ichar('W') + iaxis)//' AXIS'
      if (nbxlo .lt. 0 .or. nbxhi .lt. 0) call exitError(
     &    'A NEGATIVE BORDER WAS ENTERED FOR THE '//axis)
      if (nxpatch .le. 4) call exitError(
     &    'PATCH SIZE NEGATIVE OR TOO SMALL FOR THE '//axis)
      if (numxpat .le. 0) call exitError(
     &    'NUMBER OF PATCHES MUST BE POSITIVE FOR THE '//axis)
      if (nxpatch .gt. nx-(nbxlo+nbxhi)) then
        write(*, '(/,a,i4,a,i4,a,a)') 'ERROR: CORRSEARCH3D -  PATCH SIZE (',
     &      nxpatch,') IS BIGGER THAN SPECIFIED RANGE (', nx-(nbxlo+nbxhi),
     &      ') FOR THE ', axis
        call exit(1)
      endif
c       
c       If multiple patches, compute the delta and then adjust the number
c       of patches down to require a delta of at least 2
c       
      if(numxpat.gt.1)then
        ixstart=nbxlo
        ixdelta=(nx-(nbxlo+nbxhi+nxpatch))/(numxpat-1)
        do while (numxpat .gt. 1 .and. ixdelta .lt. 2)
          numxpat = numxpat - 1
          if (numxpat .gt. 1) ixdelta=(nx-(nbxlo+nbxhi+nxpatch))/(numxpat-1)
        enddo
      endif
c       
c       If only one patch originally or now, center it in range
c       
      if (numxpat .eq. 1) then
        ixstart=(nbxlo+nx-nbxhi)/2 - nxpatch/2
        ixdelta=1
      endif
      return
      end

c       revisePatchRange adjusts the starting position and number of
c       patches based on the vertex constraints in xvert2, xvert3
c
      subroutine revisePatchRange(nx, nbxlo, nbxhi, xvert2, xvert3, nxpatch,
     &    numxpat, ixstart, ixdelta)
      implicit none
      integer*4 nx, nbxlo, nbxhi, numxpat, ixstart, ixdelta, nxpatch
      real*4 xvert2, xvert3
      integer*4 ixlo2, ixhi2, ixspan, numx2, newxdelta
      ixlo2 = max(nbxlo, nint(xvert2))
      ixhi2 = min(nx - nbxhi, nint(xvert3))
      if (numxpat .gt. 1) then
c           
c         get new number of intervals inside the limits, and a new delta
c         to span the limits
c         
        ixspan = max(0, ixhi2 - ixlo2 - nxpatch)
        numx2 = ixspan / ixdelta + 1
        newxdelta = ixspan / numx2
        if (newxdelta .lt. 0.6 * ixdelta) then
c           
c           but if delta is too small, drop the number of intervals
c           
          numx2=numx2-1
          if (numx2 .gt. 0) then
            newxdelta = ixspan / numx2
          else
            newxdelta = ixdelta
          endif
        endif
c           
c         now adjust ixlo2 up by half of remainder to center the patches
c         in the span and find true start and end that fits inside the
c         original low-hi limits
c         
        ixlo2 = ixlo2 + mod(ixspan, newxdelta) / 2
        ixdelta = newxdelta
        ixstart = ixlo2 - ixdelta * ((ixlo2 - nbxlo) / ixdelta)
        numxpat = (nx - nbxhi - nxpatch - ixstart) / ixdelta + 1
      else
c         
c         if only one patch, put it in new middle
c         
        ixstart=(ixlo2+ixhi2)/2 - nxpatch/2
      endif
      return
      end

c       Transforms a position XB, YB in B source to XA, YA in A.  NXYZBSRC and
c       NXYZ are the dimensions of B and A, IFFLIPB and IFFLIP are 1 if the
c       long dimension is Z in B or A, ASRC(3,3) and DXYZSRC(3) have the
c       3D transformation
c
      subroutine xformBsrcToA(xb, yb, nxyzbsrc, nxyz, ifflipb, ifflip, asrc,
     &    dxyzsrc, xa, ya)
      implicit none
      real*4 xa, ya, xb, yb, asrc(3,3), dxyzsrc(3), tmpb(3), tmpa(3)
      integer*4 nxyzbsrc(3), nxyz(3), ifflipb, ifflip
      integer*4 i, j, indyb

      indyb = 2
      if (ifflipb .ne. 0) indyb = 3
      tmpb(1) = xb
      tmpb(indyb) = yb
      tmpb(5 - indyb) = nxyzbsrc(5 - indyb) / 2.
      do i = 1, 3
        tmpa(i) = dxyzsrc(i) + nxyz(i) / 2.
        do j = 1,3
          tmpa(i) = tmpa(i) + asrc(i, j) * (tmpb(j) - nxyzbsrc(j) / 2.)
        enddo
c        print *,(asrc(i, j), j = 1, 3), dxyzsrc(i), tmpb(i), tmpa(i)
      enddo
      xa = tmpa(1)
      ya = tmpa(2)
      if (ifflip .ne. 0) ya = tmpa(3)
      return
      end

c       Fills arrays IXSEQ, IYSEQ, IZSEQ with a sequence of patch numbers
c       starting from the center outward, progressing in X, then Y, then Z
c       NUMXPAT, NUMYPAT, NUMZPAT is number of patches in each direction;
c       NUMSEQ is returned with total number to loop on
c
      subroutine sequencePatches(numxpat, numypat, numzpat, ixSeq, iySeq,
     &    izSeq, idirSeq, numSeq)
      implicit none
      integer*4 numxpat, numypat, numzpat, ixSeq(*), iySeq(*), izSeq(*), numSeq
      integer*4 izpstr,izpend,izdir,idirSeq(*)
      integer*4 iypstr,iypend,iydir,ixpstr,ixpend,ixdir,ixpat,iypat,izpat
c      
      izpstr=numzpat/2+1
      izpend=numzpat
      numSeq = 0
      do izdir=1,-1,-2
        do izpat=izpstr,izpend,izdir
          iypstr=numypat/2+1
          iypend=numypat
          do iydir=1,-1,-2
            do iypat=iypstr,iypend,iydir
              ixpstr=numxpat/2+1
              ixpend=numxpat
              do ixdir=1,-1,-2
                do ixpat=ixpstr,ixpend,ixdir
                  numSeq = numSeq + 1
                  ixSeq(numSeq) = ixpat
                  iySeq(numSeq) = iypat
                  izSeq(numSeq) = izpat
                  idirSeq(numSeq) = ixdir
                enddo
                ixpstr=ixpstr-1
                ixpend=1
              enddo
            enddo
            iypstr=iypstr-1
            iypend=1
          enddo
        enddo
        izpstr=izpstr-1
        izpend=1
      enddo
      return
      end


c       Dumps a volume from the contents of CRRAY with the given rootname
c
      subroutine dumpVolume(crray,nxdim,nxpad,nypad,nzpad,rootname)
      implicit none
      integer*4 nxdim,nxpad,nypad,nzpad
      real*4 crray(nxdim,nypad,nzpad)
      real*4 title(20), scale, dmin, dmax, tmin, tmax, dmt, brray(1000), dsub
      integer*4 kxyz(3), ix, iy,iz,mode
      character*(*) rootname
      integer*4 nfile /1/
      save nfile
      character*4 buf
      character*320 concat
      character*160 filename
      
      mode = 0
      call int_iwrite(buf, nfile, iz)
      nfile = nfile + 1
      filename = concat(rootname, buf(1:iz))
      call imopen(4, filename, 'new')
c       
      kxyz(1) = nxpad
      kxyz(2) = nypad
      kxyz(3) = nzpad
      dmin = 1.e30
      dmax = -1.e30
      call icrhdr(4, kxyz, kxyz, mode, title, 0)
      call ialsiz_sam_cel(4, nxpad,nypad, nzpad)
      do iz = 1, nzpad
        call iclden(crray(1,1,iz),nxdim,nypad,1,nxpad,1,nypad,tmin,tmax,dmt)
        dmin = min(dmin, tmin)
        dmax = max(dmax, tmax)
      enddo
      if (mode .eq. 0) then
        scale = 255. / (dmax - dmin)
        dsub = dmin
        dmin = 0
        dmax = 255
      else
        scale = 1.
        dsub = 0.
      endif
      do iz = 1, nzpad
        do iy = 1, nypad
          do ix = 1, nxpad
            brray(ix) = scale *(crray(ix,iy,iz) - dsub)
          enddo
          call iwrlin(4, brray)
        enddo
      enddo
c       
      call iwrhdr(4,title,-1,dmin,dmax,128.)
      call imclose(4)
      return
      end



c       $Log$
c       Revision 3.20  2008/02/28 20:03:34  mast
c       Increased main array size 40%
c
c       Revision 3.19  2008/02/28 19:19:02  mast
c       Increased size of working array for 3D FFT 6-fold
c
c       Revision 3.18  2007/11/18 04:59:54  mast
c       Redeclared concat at 320
c
c       Revision 3.17  2007/10/10 19:51:55  mast
c       Handle volume shifts properly when volume sizes differ
c
c       Revision 3.16  2007/10/04 16:18:27  mast
c       Called new parabolic fit function and protected correlation coefficient
c       from numeric errors
c
c       Revision 3.15  2007/03/02 15:51:56  mast
c       Increased string sizes from 80 to 160
c
c       Revision 3.14  2006/08/21 16:49:22  mast
c       Changed initial offset to volume offset and provided for a separate
c       initial offset
c
c       Revision 3.13  2006/08/18 14:34:53  mast
c       Eliminated double declaration of indpat
c
c       Revision 3.12  2006/08/17 16:17:20  mast
c       SGI insists statement function be after all declarations
c
c       Revision 3.11  2006/08/16 23:44:54  mast
c       Converted to PIP, incorporated FFT correlations internally, added
c       filtering, made it extract the B patches based on the local shift
c       instead of using the same coordinates in both volumes, added model
c       file for B and initial displacement, handled volumes in both
c       orientations.
c
c       Revision 3.10  2005/10/19 16:43:17  mast
c       Doubled image array size, put in common, and made patch limit 40000
c       
c       Revision 3.9  2004/06/16 17:55:55  mast
c       Added some error checking on entries and logic to prevent patches
c       too large and patches too close together
c       
c       Revision 3.8  2003/12/24 19:05:08  mast
c       Changed to fit new form of get_nxyz
c       
c       Revision 3.7  2003/10/24 17:40:17  mast
c       Removed -e flag from tcsh command
c       
c       Revision 3.6  2003/10/24 03:48:13  mast
c       Use IMOD_DIR and IMOD_CSHELL to run onepatchcorr explicitly for 
c       Windows
c       
c       Revision 3.5  2002/09/06 00:41:07  mast
c       Needed to prevent negative spanning distances when transformed
c       corners are very close to each other
c       
c       Revision 3.4  2002/07/26 19:20:05  mast
c       Was taking min of an int and a real - Intel compiler caught it.
c       
c       Revision 3.3  2002/07/21 19:44:11  mast
c       Added declaration of lnblnk
c       
c       Revision 3.2  2002/07/20 23:56:52  mast
c       Added analysis of regions to exclude based on their positions in the
c       source file for the second volume.  Standardized error outputs and
c       added declarations for implicit none.
c       

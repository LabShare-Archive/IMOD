*       * * * * * SOLVEMATCH * * * * * *
c       
c       SOLVEMATCH will solve for a 3-dimensional linear transformation
c       relating the tomogram volumes resulting from tilt series around two
c       different axes.  It uses information from the 3-D coordinates of
c       fiducials found by TILTALIGN.  See man page for further information.
c       
c       $Id$
c       
      implicit none
      include 'statsize.inc'
      include 'model.inc'
      integer idim
      parameter (idim=10000)
      real*4 xr(msiz,idim)
      real*4 pnta(3,idim),pntb(3,idim),a(3,4),dxyz(3),devxyz(3)
      real*4 devxyzmax(3),orig(3,2),ptrot(3),cenloc(3),aloc(3,4),dxyzloc(3)
      integer*4 idrop(idim),iorig(idim),mapped(idim)
      integer*4 listcorra(idim),listcorrb(idim),icont2ptb(idim)
      integer*4 mapab(idim),nxyz(3,2),jxyz(3)/1,3,2/
      integer*4 iconta(idim),icontb(idim),icont2pta(idim)
      integer*4 icontab(idim,2), icont2ptab(idim,2),listcorrab(idim,2)
      equivalence (iconta, icontab), (icontb, icontab(1,2))
      equivalence (icont2pta, icont2ptab), (icont2ptb, icont2ptab(1,2))
      equivalence (listcorra, listcorrab), (listcorrb, listcorrab(1,2))
      integer*4 npntab(2),npnta,npntb, listUse(idim)
      equivalence (npnta, npntab(1)), (npntb, npntab(2))
      real*4 transferX(idim,2), transferY(idim,2), fidModX(idim,2)
      real*4 fidModY(idim,2)
      integer*4 modObj(idim,2), modCont(idim,2), izbest(2), numFid(2)
      integer*4 modObjFid(idim,2), modContFid(idim,2)
      character*320 filename
      character*1 abtext(2)/'A', 'B'/
      character*1 badaxis1, badaxis2
      integer*4 nstartmin,itry,nlist,nlista,nlistb,ndat,ifZshifts,ifAngleOfs
      integer*4 ia,nsurf,model,iftransp,nmodpt,ipt,ip,iobj,i,ndata
      integer*4 mod,j,ifadded,ipntmax,ipta,iptb,iptamin,iptbmin
      integer*4 maxdrop,ndrop,idum,iofs,ixyz, ierrFactor
      real*4 addratio,addcrit,distmin,devavg,devsd
      real*4 devmax,dx,dist,crit,elimmin,critabs,stoplim,xtilta,xtiltb
      real*4 dy,dz,aScale, bScale, absFidCrit
      integer*4 ierr,ifflip,ncolfit,maxconta,maxcontb,icolfix,k
      real*4 xyscal,zscale,xofs,yofs,zofs,ximscale, yimscale, zimscale
      real*4 loMaxAvgRatio, hiMaxAvgRatio, loMaxLimRatio, hiMaxLimRatio
      real*4 aDelta, bDelta, aPixelSize, bPixelSize, xcen, ycen, transTol
      real*4 angleOffsetA, angleOffsetB, zShiftA, zShiftB
      integer*4 nxFidA, nyFidA, nxFidB, nyFidB, iTransA, iTransB, ifTransBtoA
      integer*4 nTransCoord, izind, indA, indB, nListUse, localNum
      integer*4 numLocalX, numLocalY, numBig, ixl, iyl, iorigmax, limRaised
      real*4 xmin,xmax,ymin,ymax,size, sizeLast,targetSize, dxLocal, dyLocal
      real*4 sumMean, sumMax, shiftLimit, csdx, csdy, csdz, devavgLoc
      real*4 devmaxLoc, devallMax,globLocAvgRatio, axisCrit, reportDiff
      real*4 yzScaleDiff,sumsq,xyScaleDiff,xzScaleDiff,axisScale(3)
      real*4 transPixel,freinp(20)
      logical*4 relativeFids, matchAtoB

      logical readw_or_imod, b3dxor
      integer*4 getimodhead,getimodscales,imodGetenv
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger
      integer*4 PipGetString,PipGetTwoFloats,PipGetFloat
      integer*4 PipGetInOutFile,PipGetLogical
       
      character*6 modelOption(2)/'AMatch','BMatch'/
      character*9 tomoOption(2)/'ATomogram','BTomogram'/
      character*10240 listString
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  solvematch
c       
      integer numOptions
      parameter (numOptions = 24)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'output:OutputFile:FN:@afiducials:AFiducialFile:FN:@'//
     &    'bfiducials:BFiducialFile:FN:@alist:ACorrespondenceList:LI:@'//
     &    'blist:BCorrespondenceList:LI:@'//
     &    'transfer:TransferCoordinateFile:FN:@amodel:AFiducialModel:FN:@'//
     &    'bmodel:BFiducialModel:FN:@use:UsePoints:LI:@atob:MatchingAtoB:B:@'//
     &    'xtilts:XAxisTilts:FP:@angles:AngleOffsetsToTilt:FP:@'//
     &    'zshifts:ZShiftsToTilt:FP:@surfaces:SurfacesOrUseModels:I:@'//
     &    'maxresid:MaximumResidual:F:@local:LocalFitting:I:@'//
     &    'center:CenterShiftLimit:F:@amatch:AMatchingModel:FN:@'//
     &    'bmatch:BMatchingModel:FN:@atomogram:ATomogramOrSizeXYZ:CH:@'//
     &    'btomogram:BTomogramOrSizeXYZ:CH:@scales:ScaleFactors:FP:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       don't add a point if it's this much higher than the limit for
c       quitting
c       
      addratio=1.
c       
c       require at least this many points to start
c       
      nstartmin=4
c       
c       initialize these items here in case no fiducials are entered
c       
      npnta=0
      npntb=0
      nlista = 0
      nlistb = 0
      ndat=0
      nsurf = 0
      ncolfit = 3
      icolfix = 0
      filename = ' '
      xtilta = 0.
      xtiltb = 0.
      angleOffsetA = 0.
      angleOffsetB = 0.
      localNum = 0
      shiftLimit = 10.
      zShiftA = 0.
      zShiftB = 0.
      stoplim = 8.
      aScale = 1.
      bScale = 1.
      aDelta = 0.
      bDelta = 0.
      aPixelSize = 0.
      bPixelSize = 0.
      relativeFids = .true.
      indA = 1
      indB = 2
      transTol = 3.
      nTransCoord = 0
      matchAtoB = .false.
      axisCrit = 10.
      transPixel = 0.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'solvematch',
     &    'ERROR: SOLVEMATCH - ', .true., 3, 0, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
c       Get first fid filename if any
c       Also get the surfaces option; if it's -2, then force models only
c       even if there are fids
c       
      if (pipinput) then
        ierr = PipGetLogical('MatchingAtoB', matchAtoB)
        if (matchAtoB) indA = 2
        indB = 3 - indA
        ierr = PipGetString('AFiducialFile', filename)
        nsurf = 2
        ierr = PipGetInteger('SurfacesOrUseModels', nsurf)
        if (nsurf .eq. -2) then
          nsurf = 0
          filename = ' '
        endif
      else
        write(*,'(1x,a,/,a$)') 'Name of file with 3-D fiducial'//
     &      ' coordinates for first tilt series,',
     &      ' or Return to align with matching model files only: '
        read(*,'(a)')filename
      endif
c       
c       If no fiducials are to be used, skip down to maximum residual entry
c       after initializing some variables
      maxconta = 0
      maxcontb = 0
      if (filename .eq. ' ')go to 40
c       
      ncolfit = 4
      call getFiducials(filename, iconta, pnta, npnta, modObj(1,1),
     &    modCont(1,1), idim, 'first tomogram', aPixelSize,  nxFidA, nyFidA)
c       
      if (pipinput) then
        if (PipGetString('BFiducialFile', filename) .gt. 0) call exiterror
     &      ('NO FILE SPECIFIED FOR FIDUCIALS IN SECOND TILT SERIES')
      else
        write(*,'(1x,a,$)') 'Name of file with 3-D fiducial'//
     &      ' coordinates for second tilt series: '
        read(*,'(a)')filename
      endif
      call getFiducials(filename, icontb, pntb, npntb, modObj(1,2),
     &    modCont(1,2), idim, 'second tomogram', bPixelSize, nxFidB, nyFidB)
c       
c       fill listcorr with actual contour numbers, and find maximum contours
c       
      do i=1,npnta
        listcorra(i)=iconta(i)
        mapab(i)=0
        maxconta=max(maxconta,iconta(i))
      enddo
      do i=1,npntb
        listcorrb(i)=icontb(i)
        mapped(i)=0
        maxcontb=max(maxcontb,icontb(i))
      enddo
c       
c       build index from contour numbers to points in array
c       
      if (maxconta.gt.idim.or.maxcontb.gt.idim)call exiterror(
     &    'CONTOUR NUMBERS TOO HIGH FOR ARRAYS')
      do j = 1, 2
        do i=1,max(maxconta,maxcontb)
          icont2ptab(i,j)=0
        enddo
        do i=1,npntab(j)
          icont2ptab(icontab(i,j),j)=i
        enddo
      enddo
c       
      if (pipinput .and.
     &    PipGetString('TransferCoordinateFile', filename) .eq. 0) then
c           
c         Read transfer file, get best section and coordinates
c         
        call dopen(1, filename, 'ro', 'f')
        read(1,'(a)')listString
        call frefor(listString, freinp, j)
        izbest(1) = nint(freinp(1))
        izbest(2) = nint(freinp(2))
        ifTransBtoA = nint(freinp(3))
c         
c         If there is a fourth value
c         then get the scaling to apply to model coords to match transfer coord
        if (j .gt. 3) transPixel = freinp(4)
c           
        i = 0
11      read(1,*,err=12,end=12)(transferX(i+1, j), transferY(i+1, j), j=1,2)
        i = i + 1
        if (i .ge. idim) call exiterror(
     &      'TOO MANY TRANSFER COORDINATES FOR ARRAYS')
        goto 11
12      nTransCoord = i
c        print *,nTransCoord, ' transfer coords'
c         
c         Set up indexes to best z for the first and second model, and for
c         accessing the absolute A or B axis transfer coords and fid coords
c         
        izind = 1
        if (b3dxor(matchAtoB, ifTransBtoA .gt. 0)) izind = 2
        iTransA = 1
        if (ifTransBtoA .gt. 0) iTransA = 2
        iTransB = 3 - iTransA
        
c         
c         Read the two fiducial models and get coordinates at best Z and
c         their objects and contours
c         
        call readFidModelFile('AFiducialModel', izbest(izind), abtext(indA),
     &      fidModX(1,indA), fidModY(1,indA), modObjFid(1,indA),
     &      modContFid(1,indA), numFid(indA), idim, transPixel)
        call readFidModelFile('BFiducialModel', izbest(3-izind),
     &      abtext(indB), fidModX(1,indB), fidModY(1,indB),
     &      modObjFid(1,indB), modContFid(1,indB), numFid(indB), idim,
     &      transPixel)
c         
c         Get the list of points to use from the true A series
c         
        nListUse = npntab(indA)
        do i = 1, npntab(indA)
          listUse(i) = icontab(i, indA)
        enddo
        if (PipGetString('UsePoints', listString) .eq. 0)
     &      call parselist(listString, listUse, nlistUse)
c        print *,nListUse,' points:', (listUse(i), i = 1,nListUse)
c         
c         Build list of corresponding points
c         
        nlist = 0
        do i = 1, nListUse
          if (listUse(i) .gt. 0 .and. listUse(i) .le. max(maxconta,maxcontb)
     &        .and. icont2ptab(listUse(i), indA) .gt. 0) then
c             
c             IF the point in A is a legal point from fid file, find the
c             same object/contour in the fiducial model
c             
            ipta = icont2ptab(listUse(i), indA)
c            print *,'legal point',i,listUse(i),ipta
            ip = 0
            do j = 1, numFid(1)
              if (modObj(ipta, indA) .eq. modObjFid(j, 1) .and.
     &            modCont(ipta, indA) .eq. modContFid(j, 1)) ip = j
            enddo
            if (ip .gt. 0) then
c               
c               Now match the model coords to the transfer coords
c               
c              print *,'matches in a fid model:', ip
              ia = 0
              do j = 1, nTransCoord
                if (sqrt((fidModX(ip, 1) - transferX(j, iTransA))**2 +
     &              (fidModY(ip, 1) - transferY(j, iTransA))**2) .lt.
     &              transTol) ia = j
              enddo
              if (ia .gt. 0) then
c                print *,'matches transfer:', ia
c                 
c                 Found one, then look for a match to the B transfer coords
c                 in the B fiducial model
c                 
                ipt = 0
                do j = 1, nTransCoord
                  if (sqrt((fidModX(j, 2) - transferX(ia, iTransB))**2 +
     &                (fidModY(j, 2) - transferY(ia, iTransB))**2) .lt.
     &                transTol) ipt = j
                enddo
                if (ipt .gt. 0) then
c                  print *,'matches in B fid model:', ipt
c                   
c                   Find the obj/cont in the points of the fid file
c                   
                  iptb = 0
                  do j = 1, numFid(2)
                    if (modObj(j, indB) .eq. modObjFid(ipt, 2) .and.
     &                  modCont(j, indB) .eq. modContFid(ipt, 2)) iptb = j
                  enddo
                  if (iptb .gt. 0) then
c                     
c                     check for uniqueness
c
                    nlistb = 0
                    do j = 1, nlist
                      if (listCorrAB(j, indB) .eq. icontAB(iptb, indB))nlistb=1
                    enddo
                    if (nlistb .eq. 0) then
c                     
c                       Bingo: we have corresponding points
c                       
c                      print *,'Matching:', listUse(i),icontAB(iptb, indB)  
                      nlist = nlist + 1
                      listCorrAB(nlist, indA) = listUse(i)
                      listCorrAB(nlist, indB) = icontAB(iptb, indB)
                    endif
                  endif
                endif
              endif
            endif
          endif
        enddo
        if (nlist .eq. 0) call exiterror(
     &      'NO CORRESPONDING POINTS FOUND USING TRANSFER COORDS')
        nlista = nlist
        nlistb = nlist

      else
c         
c         No transfer coordinates, get corresponding lists the old way
c
        nlist=min(npnta,npntb)
        nlista=nlist
        if (pipinput) then
          if (PipGetString('ACorrespondenceList', listString) .eq. 0)
     &        call parselist(listString, listcorra,nlista)
        else
          write (*,113)nlist
113       format('Enter a list of points in the first series for which',
     &        ' you are sure of the',/,' corresponding point in the',
     &        ' second series (Ranges are OK;',/' enter / if the first',
     &        i3, ' points are in one-to-one correspondence between',
     &        ' the series')
          call rdlist(5,listcorra,nlista)
        endif
        if (nlista .gt. idim) call exiterror('TOO MANY POINTS FOR ARRAYS')
        if(nlista.gt.nlist)call exiterror(
     &      'YOU HAVE ENTERED MORE NUMBERS THAN THE'//
     &      ' MINIMUM NUMBER OF POINTS IN A AND B')
c       
        nlistb=nlista
        if (pipinput) then
          if (PipGetString('BCorrespondenceList', listString) .eq. 0)
     &        call parselist(listString, listcorrb,nlistb)
        else
          write (*,114)
114       format('Enter a list of the corresponding points in the',
     &        ' second series ',/,' - enter / for ', $)
          call wrlist(listcorrb, nlist)
          call rdlist(5,listcorrb,nlistb)
        endif
      endif
c
      if(nlista.lt.nstartmin)then
        print *
        print *,'ERROR: SOLVEMATCH - NEED AT LEAST',nstartmin,
     &      ' POINTS TO GET STARTED'
        call exit(1)
      endif
      if(nlistb.ne.nlista)then
        print *
        print *,'ERROR: SOLVEMATCH - YOU MUST HAVE THE SAME NUMBER '
     &      //' OF ENTRIES IN EACH LIST','YOU MADE',nlista,' AND',
     &      nlistb,' ENTRIES FOR LISTS '//abtext(indA)//' AND '//abtext(indB)
        call exit(1)
      endif
c       
c       check legality and build map lists
c       
c      call wrlist(listcorra,nlista)
c      call wrlist(listcorrb,nlistb)
      do i=1,nlista
        if(listcorra(i).le.0.or.listcorrb(i).le.0)call exiterror(
     &      'YOU ENTERED A POINT NUMBER LESS '//
     &      'THAN OR EQUAL TO ZERO')
        if(listcorra(i) .gt. maxconta)call exiterror(
     &      ' YOU ENTERED A POINT NUMBER HIGHER'//
     &      ' THAN THE NUMBER OF POINTS IN '//abtext(indA))
        ipta = icont2pta(listcorra(i))
        if(ipta .eq. 0)call exiterror(
     &      ' YOU ENTERED A POINT NUMBER THAT IS NOT INCLUDED'//
     &      ' IN THE POINTS FROM '//abtext(indA))
        if(listcorrb(i).gt. maxcontb)call exiterror(
     &      ' YOU ENTERED A POINT NUMBER HIGHER'//
     &      ' THAN THE NUMBER OF POINTS IN '//abtext(indB))
        iptb = icont2ptb(listcorrb(i))
        if(iptb .eq. 0)call exiterror(
     &      ' YOU ENTERED A POINT NUMBER THAT IS NOT INCLUDED'//
     &      ' IN THE POINTS FROM '//abtext(indB))
        if(mapab(ipta).ne.0)then
          print *
          print *,'ERROR: SOLVEMATCH - POINT #',listcorra(i),
     &        ' IN '//abtext(indA)//' REFERRED TO TWICE'
          call exit(1)
        elseif(mapped(iptb).ne.0)then
          print *
          print *,'ERROR: SOLVEMATCH - POINT #',listcorrb(i),
     &        ' IN '//abtext(indB)//' REFERRED TO TWICE'
          call exit(1)
        endif
        mapab(ipta)=iptb
        mapped(iptb)=ipta
      enddo
c      print *,(mapab(i),i=1,npnta)
c      print *,(mapped(i),i=1,npntb)
c       
c       Get tilt axis angle, plus try to get tomogram pixel size (delta)
c       and compute scaling factors, overriden by an entry
c       
      if (pipinput) then
        ierr = PipGetTwoFloats('XAxisTilts', xtilta, xtiltb)
        ifZshifts = 1 - PipGetTwoFloats('ZShiftsToTilt', zShiftA, zShiftB)
        ifAngleOfs = 1 - PipGetTwoFloats('AngleOffsetsToTilt', angleOffsetA,
     &      angleOffsetB)
        ierr = PipGetInteger('LocalFitting', localNum)
        ierr = PipGetFloat('CenterShiftLimit', shiftLimit)
        ierr = PipGetFloat('AnisotropicLimit', axisCrit)
        call getDelta('ATomogramOrSizeXYZ', aDelta, nxyz(1,1)) 
        call getDelta('BTomogramOrSizeXYZ', bDelta, nxyz(1,2))
        if (aDelta * aPixelSize .gt. 0.) aScale = aPixelSize / aDelta
        if (bDelta * bPixelSize .gt. 0.) bScale = bPixelSize / bDelta
        ierrFactor = PipGetTwoFloats('ScaleFactors', aScale, bScale)
c         print *,aDelta,bDelta,aPixelSize,bPixelSize,ierrFactor
c         
c         Conditions for absolute fiducials are that the pixel sizes be
c         available (meaning new absolute coordinates) and that either
c         the tomogram pixel sizes were available too or scale factors
c         were entered and tomogram sizes were provided by getDelta
c         
        if (nsurf .ne. 0 .and. aPixelSize .gt. 0. .and. bPixelSize .gt. 0.
     &      .and. (aDelta .gt. 0. .or. (ierrFactor .eq. 0 .and.
     &      aDelta .eq. 0.)) .and. (bDelta .gt. 0 .or.
     &      (ierrFactor .eq. 0 .and. bDelta .eq. 0.))) then
c           
c           Shift the original X and Y to the center of the volume
c           have to divide size by scale because points aren't scaled up yet
c           Use the actual fiducial size if it exists
c           
          xcen = 0.5 * nxyz(1, 1) / aScale
          ycen = 0.5 * nxyz(jxyz(2), 1) / aScale
          if (nxFidA .gt. 0 .and. nyFidA .gt. 0) then
            xcen = 0.5 * nxFidA
            ycen = 0.5 * nyFidA
          endif
          do i = 1, npnta
            pnta(1, i) = pnta(1, i) - xcen
            pnta(2, i) = pnta(2, i) - ycen
          enddo
c           
          xcen = 0.5 * nxyz(1, 2) / bScale
          ycen = 0.5 * nxyz(jxyz(2), 2) / bScale
          if (nxFidB .gt. 0 .and. nyFidB .gt. 0) then
            xcen = 0.5 * nxFidB
            ycen = 0.5 * nyFidB
          endif
          do i = 1, npntb
            pntb(1, i) = pntb(1, i) - xcen
            pntb(2, i) = pntb(2, i) - ycen
          enddo
          relativeFids = .false.
        endif
      else
        write(*,'(1x,a,$)')'Tilts around the X-axis applied in '//
     &      'generating tomograms A and B: '
        read(5,*)xtilta,xtiltb
      endif
c       
c       Adjust the positions of the points for x axis tilt, scaling, angle
c       offset and z shift when building the tomogram
c
      call rotateFids(pnta, npnta, xtilta, aScale, angleOffsetA, zShiftA)
      call rotateFids(pntb, npntb, xtiltb, bScale, angleOffsetB, zShiftB)
c       
40    if (pipinput) then
        ierr = PipGetFloat('MaximumResidual', stoplim)
      else
        write(*,'(1x,a,/,a,$)')'Maximum residual value above which '//
     &      'this program should',' exit with an error: '
        read(5,*)stoplim
      endif
c       
c       if no fiducials, now skip to model entry section
c       
      if (npnta.eq.0) go to 50
c       
c       fill array for regression
c       
      do ia=1,npnta
        if(mapab(ia).ne.0)then
          ndat=ndat+1
          do j=1,3
            xr(j,ndat)=pntb(jxyz(j),mapab(ia))
            xr(j+5,ndat)=pnta(jxyz(j),ia)
          enddo
          xr(4,ndat)=1.
          iorig(ndat)=iconta(ia)
c           print *,(xr(j,ndat),j=1,3),(xr(j,ndat),j=6,8)
        endif
      enddo
c       
      print *,ndat,' pairs of fiducial points originally specified'
      if (.not. pipinput) then
        nsurf = 2
        write(*,'(1x,a,/,a,/,a,/,a,$)')'Enter 0 to solve for '//
     &      'displacements using matching model files, or -1, 1 or 2'
     &      ,'  to solve only for 3x3 matrix (2 if fiducials are on 2'
     &      //' surfaces, 1 if they are',
     &      '  on one surface and tomograms are NOT inverted,'//
     &      ' or -1 if fiducials ARE on one','  surface'
     &      //' and tomograms are inverted relative to each other): '
        read(5,*)nsurf
      endif
c       
c       Enter matching models if nsurf is 0
c       
50    if(nsurf.eq.0)then
        iofs=ncolfit+1
        do model=1,2
c           
c           DNM 7/20/02: Changed to just get the nx,ny,nz and not the
c           origin from the image file; to use model header information
c           to scale back to image index coordinates; and to not use or mess
c           up the y-z transposition variable
c           
          if (.not.pipinput) write(*,'(1x,a,i2,a)')'Enter NX, NY, NZ of '//
     &        'tomogram', model, ', or name of tomogram file'
          call get_nxyz(pipinput, tomoOption(model), 'SOLVEMATCH', 1,
     &        nxyz(1,model))
c           print *,(nxyz(i,model),i=1,3)
          if (pipinput) then
            if (PipGetString(modelOption(model), filename) .gt. 0) then
              print *
              print *,'ERROR: SOLVEMATCH - NO MATCHING MODEL FOR TOMOGRAM',
     &            model
              call exit(1)
            endif
          else
            write(*,'(1x,a,i2,a,$)')
     &          'Name of model file from tomogram',model,': '
            read(*,'(a)')filename
          endif
          if(.not.readw_or_imod(filename))call exiterror(
     &        'READING MODEL FILE')

          ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
          ierr = getimodscales(ximscale, yimscale, zimscale)
          
          nmodpt=0
          do iobj=1,max_mod_obj
            do ip=1,npt_in_obj(iobj)
              ipt=abs(object(ibase_obj(iobj)+ip))
              nmodpt=nmodpt+1
              if (ndat + nmodpt .gt. idim) call exitError(
     &            'TOO MANY POINTS FOR ARRAYS')
              xr(1+iofs,ndat+nmodpt)=(p_coord(1,ipt)-xofs)/ximscale
     &            - 0.5*nxyz(1,model)
              xr(2+iofs,ndat+nmodpt)=(p_coord(2,ipt)-yofs)/yimscale
     &            - 0.5*nxyz(2,model)
              xr(3+iofs,ndat+nmodpt)=(p_coord(3,ipt)-zofs)/zimscale
     &            - 0.5*nxyz(3,model)
              xr(4,ndat+nmodpt)=0.
              iorig(ndat+nmodpt)=-nmodpt
              if (npnta .eq. 0)iorig(ndat+nmodpt)=nmodpt
            enddo
          enddo
          if(model.eq.1)ndata=nmodpt
          iofs=0
        enddo
        if(nmodpt.ne.ndata)call exiterror(
     &      '# OF POINTS DOES NOT MATCH BETWEEN MATCHING MODELS')
        print *,nmodpt,' point pairs from models'
        iofs = ncolfit + 1
      else
c         
c         If no model points
c         get rid of dummy column: fit 3 columns, pack dependent vars to
c         the left, and set the offset for adding more dependent var data
c         
        nmodpt=0
        ncolfit = 3
        do i = 1, ndat
          do j = 5,7
            xr(j, i) = xr(j + 1, i)
          enddo
        enddo
        iofs = 4
c         
c         if only one surface, "fix" column 2, encode sign in icolfix
c         
        if(abs(nsurf).eq.1) icolfix = sign(2, nsurf)
      endif
      ndat=ndat+nmodpt
c       
c       loop to add points that weren't initially indicated - as long as
c       there are any left to add and the last minimum distance was still
c       low enough
c       
      ifadded=0
      addcrit=addratio*stoplim
      distmin=0.
c       
      do while(ndat-nmodpt.lt.min(npnta,npntb).and.distmin.lt.addcrit)
        call do3multr(xr,ndat,ncolfit,ndat,icolfix,a,dxyz,cenloc,
     &      devavg,devsd, devmax,ipntmax, devxyzmax)
        distmin=1.e10
c         
c         apply to each point in B that is not mapped to
c         
        do iptb=1,npntb
          if(mapped(iptb).eq.0)then
            do ixyz=1,3
              ptrot(ixyz)=dxyz(ixyz)
              if (ncolfit .eq. 4) ptrot(ixyz)=dxyz(ixyz) + a(ixyz,4)
              do j=1,3
                ptrot(ixyz)=ptrot(ixyz)+
     &              a(ixyz,j)*pntb(jxyz(j),iptb)
              enddo
            enddo
c             
c             search through points in A that don't have map
c             
            do ipta=1,npnta
              if(mapab(ipta).eq.0)then
                dx=pnta(jxyz(1),ipta)-ptrot(1)
                if(abs(dx).lt.distmin)then
                  dy=pnta(jxyz(2),ipta)-ptrot(2)
                  if(abs(dy).lt.distmin)then
                    dz=pnta(jxyz(3),ipta)-ptrot(3)
                    if(abs(dz).lt.distmin)then
                      dist=sqrt(dx**2+dy**2+dz**2)
                      if(dist.lt.distmin)then
                        iptamin=ipta
                        iptbmin=iptb
                        distmin=dist
                      endif
                    endif
                  endif
                endif
              endif
            enddo
          endif
        enddo
c         
c         add the closest fitting point-pair
c         
c         print *,iptbmin,iptamin,distmin,devavg,devmax
        if(distmin.lt.addcrit)then
          ifadded=1
          ndat=ndat+1
          mapab(iptamin)=iptbmin
          mapped(iptbmin)=iptamin
          do j=1,3
            xr(j,ndat)=pntb(jxyz(j),iptbmin)
            xr(j+iofs,ndat)=pnta(jxyz(j),iptamin)
          enddo
          xr(4,ndat)=1.
          iorig(ndat)=iconta(iptamin)
        endif
      enddo
c
      print *,ndat, ' pairs of points are available for fitting'
      if(ifadded.ne.0 .or. nTransCoord .gt. 0)then
c         
c         rebuild lists of actual contour numbers
c         
        nlista=0
        do i=1,npnta
          if (mapab(i) .ne. 0)then
            nlista=nlista+1
            listcorra(nlista) = iconta(i)
            listcorrb(nlista) = icontb(mapab(i))
          endif
        enddo
        print *,'In the final list of correspondences used for',
     &      ' fits, points from '//abtext(indA)//' are:'
        call wrlist(listcorra,nlista)
        print *,'Points from '//abtext(indB)//' are:'
        call wrlist(listcorrb,nlista)
      endif

c       write(*,105)((xr(i,j),i=1,4),(xr(i,j),i=1+iofs,3+iofs),j=1,ndat)
105   format(7f9.2)
      maxdrop=nint(0.1*(ndat-1))
      crit=0.01
      elimmin=3.
      critabs=0.002
      call solve_wo_outliers(xr,ndat,ncolfit,icolfix,maxdrop,crit,critabs,
     &    elimmin, idrop,ndrop, a,dxyz,cenloc, devavg,devsd,devmax,
     &    ipntmax, devxyzmax)
c       
      if(ndrop.ne.0)then
        write(*,104)ndrop,devavg,devsd,abtext(indA),(iorig(idrop(i)),i=1,ndrop)
        write(*,115)(xr(ncolfit+1,i),i=ndat+1-ndrop,ndat)
104     format(/,i3,' points dropped by outlier elimination; ',
     &      'residual mean =',f7.2,', SD =',f7.2,/,
     &      ' point # in ',a1,':',(9i7))
115     format(' deviations  :',(9f7.1))
      endif
c       
      iorigmax = iorig(ipntmax)
      if (iorigmax .le. maxconta .and. modObj(1,1) .gt. 0) then
        ipta = icont2pta(iorigmax)
        write(*,1011)devavg,devmax,iorigmax,modObj(ipta, 1),
     &      modCont(ipta, 1),abtext(indA)
1011    format(//,' Mean residual',f8.3,',  maximum',f9.3,
     &      ' at point #',i4,' (Obj',i3,' cont',i4,' in ',a1,')')
      else
        write(*,1012)devavg,devmax,iorigmax,abtext(indA)
1012    format(//,' Mean residual',f8.3,',  maximum',f9.3,
     &      ' at point #',i4,' (in ',a1,')')
      endif
      write(*,1013)(devxyzmax(i),i=1,3)
1013  format('  Deviations:',3f9.3)
c       
      if (ncolfit .gt. 3) then
c         
c         fit to both: report the dummy variable offset, make sure that
c         the dxyz are non-zero for matchshifts scanning
c         
        write(*,103)(a(i,4),i=1,3)
103     format(/,' X, Y, Z offsets for fiducial dummy variable:',3f10.3)
        if (abs(dxyz(1)) .lt. 0.001 .and. abs(dxyz(2)) .lt. 0.001 .and.
     &      abs(dxyz(3)) .lt. 0.001) dxyz(1) = 0.0013
c         
      else if (nmodpt .eq. 0 .and. relativeFids) then
c         
c         No matching models and relative fiducials: set dxyz to zero
c         and report the offset from the fit
c         
        write(*,107)(dxyz(i),i=1,3)
107     format(/,' X, Y, Z offsets from fiducial fit:',3f10.3)
        dxyz(1) = 0.
        dxyz(2) = 0.
        dxyz(3) = 0.
        if (imodGetenv('SOLVEMATCH_TEST', filename) .ne. 0) call exitError(
     &      'RELATIVE FIDUCIAL COORDINATES ARE NO LONGER ALLOWED; RERUN '//
     &      'TILTALIGN FOR BOTH AXES TO GET ABSOLUTE COORDINATES')
      else
c         
c         Absolute fiducials: just make sure they are not exactly zero
c         
        if (abs(dxyz(1)) .lt. 0.001 .and. abs(dxyz(2)) .lt. 0.001 .and.
     &      abs(dxyz(3)) .lt. 0.001) dxyz(1) = 0.0013
      endif
c       
      print *
      print *,'Transformation matrix for matchvol:'
      write(*,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102   format(3f10.6,f10.3)
c       
c       Compute scaling of vectors and report it
c       Then give warnings of unequal scalings
      do j = 1, 3
        sumsq = 0.
        do i = 1,3
          sumsq = sumsq + a(i,j)**2
        enddo
        axisScale(j) = sqrt(sumsq)
      enddo
      write(*,118)(axisScale(i),i=1,3)
118   format(/, 'Scaling along the three axes - X:',f7.3,'  Y:',f7.3,'  Z:',f7.3)
      xyScaleDiff = 100. * abs((axisScale(1) - axisScale(2)) / axisScale(1))
      xzScaleDiff = 100. * abs((axisScale(1) - axisScale(3)) / axisScale(1))
      yzScaleDiff = 100. * abs((axisScale(2) - axisScale(3)) / axisScale(3))
      badAxis1 = ' '
      badAxis2 = ' '
      if (axisCrit .gt. 0) then
        if (xyScaleDiff .gt. axisCrit .and. xzScaleDiff .gt. axisCrit .and.
     &      yzScaleDiff .gt. axisCrit) then
          reportDiff = min(xyScaleDiff, xzScaleDiff, yzScaleDiff)
          write(*,'(/,a,f3.0,a)')'WARNING: The scalings along all three axes'//
     &        ' differ from each other by more than', reportDiff, '%'
          badAxis1 = 'A'
        elseif (xyScaleDiff .gt. axisCrit .and. xzScaleDiff .gt. axisCrit) then
          badaxis1 = 'X'
          reportDiff = 0.5 * (xyScaleDiff + xzScaleDiff)
        elseif (xyScaleDiff .gt. axisCrit .and. yzScaleDiff .gt. axisCrit) then
          badaxis1 = 'Y'
          reportDiff = 0.5 * (xyScaleDiff + yzScaleDiff)
        elseif (xzScaleDiff .gt. axisCrit .and. yzScaleDiff .gt. axisCrit) then
          badaxis1 = 'Z'
          reportDiff = 0.5 * (xzScaleDiff + yzScaleDiff)
        elseif (xyScaleDiff .gt. axisCrit) then
          badaxis1 = 'X'
          badaxis2 = 'Y'
          reportDiff = xyScaleDiff
        elseif (xzScaleDiff .gt. axisCrit) then
          badaxis1 = 'X'
          badaxis2 = 'Z'
          reportDiff = xzScaleDiff
        elseif (yzScaleDiff .gt. axisCrit) then
          badaxis1 = 'Y'
          badaxis2 = 'Z'
          reportDiff = yzScaleDiff
        endif
      endif
      if (badaxis2 .ne. ' ') then
        write(*,'(/,a,a,a,a,a,f4.0,a)')'WARNING: The scaling along the ',
     &      badaxis1, ' and ',badaxis2,' axes differ by',reportDiff,'%'
      elseif (badaxis1 .ne. 'A' .and. badaxis1 .ne. ' ') then
        write(*,'(/,a,a,a,f4.0,a)')'WARNING: The scaling along the ',badaxis1,
     &      ' axis differs from the other two axes by', reportDiff, '%'
      endif
c       
c       Issue specific dual-axis warning with advice
      if (badaxis1 .eq. 'Y' .and. nsurf .eq. 2 .and. (ifAngleOfs .ne. 0 .or.
     &    ifZshifts .ne. 0)) write(*,119)
119   format('WARNING: Y scaling is probably wrong because you specified that',
     &    ' points are on',/,'WARNING:    two surfaces but there are too few ',
     &    'points on one surface.',/,'WARNING:    Try specifying that points ',
     &    'are on one surface')

      filename = ' '
      if (pipinput) then
        if (PipGetInOutFile('OutputFile', 1, ' ', filename)
     &      .ne. 0) call exiterror('NO OUTPUT FILE SPECIFIED')
      else
        print *,'Enter name of file to place transformation in, or ',
     &      'Return for none'
        read(5,'(a)')filename
      endif
      if(filename.ne.' ')then
        call dopen(1,filename,'new','f')
        write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
        close(1)
      endif
c       
      ierr = 0
      limRaised = devmax + 1.2
      devAllMax = 0.
      if (devmax.gt.stoplim .and. nmodpt.eq.0 .and. localNum .gt. 0) then
        if (localNum .lt. 6 .or. ndat .le. localNum) then
          if (localNum .lt. 6) write(*,'(/,a)')'ERROR: SOLVEMATCH - LOCAL'//
     &        ' FITS MUST HAVE A MINIMUM OF 6 POINTS'
          if (ndat .le. localNum) write(*,'(/,a,/,a)')'Local fitting is not'//
     &        ' available because the number of matched points',
     &        ' is no bigger than the minimum for local fitting'
        else
c           
c           For local fits, get extent of the data
c           
          xmin = 1.e10
          xmax = -xmin
          ymin = 1.e10
          ymax = -ymin
          do ia = 1, npnta
            if (mapab(ia) .gt. 0) then
              xmin = min(xmin,pnta(1,ia))
              ymin = min(ymin,pnta(2,ia))
              xmax = max(xmax,pnta(1,ia))
              ymax = max(ymax,pnta(2,ia))
            endif            
          enddo
c          print *,'minmax',xmin,xmax,ymin,ymax
c           
c           Set up number and interval between local areas
c
          targetSize = sqrt(localNum * (xmax-xmin)*(ymax-ymin) / ndat)
          numLocalX = max(1., 2. * (xmax - xmin) / targetSize + 0.1)
          numLocalY = max(1., 2. * (ymax - ymin) / targetSize + 0.1)
          dxLocal = 0
          dyLocal = 0
          if (numLocalX .gt. 1)
     &        dxLocal = (xmax - xmin - targetSize) / (numLocalX - 1)
          if (numLocalY .gt. 1)
     &        dyLocal = (ymax - ymin - targetSize) / (numLocalY - 1)
c           
c           Loop on local areas, getting mean residual and number with max 
c           above limit
c           
          sumMean = 0.
          sumMax = 0.
          numBig = 0
          do ixl = 1, numLocalX
            xcen = xmin + targetSize / 2. + (ixl - 1) * dxLocal
            do iyl = 1, numLocalY
              ycen = ymin + targetSize / 2. + (iyl - 1) * dyLocal
              size = targetSize
5             call fillLocalData(xcen, ycen, size, localNum, pnta, pntb,
     &            npnta, mapab, jxyz, xr, msiz, ndat)
c               do j = 1,ndat
c               write(*,'(6f8.1)')(xr(i,j),i=1,3), (xr(i,j),i=5,7)
c               enddo
              maxdrop=nint(0.1*ndat)
              if (ndat .le. 6) maxdrop = 0
              call solve_wo_outliers(xr,ndat,ncolfit,icolfix,maxdrop,crit,
     &            critabs, elimmin, idrop,ndrop, aloc,dxyzloc,cenloc,
     &            devavgLoc, devsd,devmaxLoc, ipntmax, devxyzmax)
c               print *,xcen,ycen,size,ndat,devavgloc,devmaxloc
              sumMean = sumMean + devavgLoc
              sumMax = sumMax + devmaxLoc
              devAllMax = max(devAllMax, devmaxLoc)
              if (devmaxLoc .gt. stoplim) numBig = numBig + 1
            enddo
          enddo
          write(*,1015)localNum, sumMean/(numLocalX * numLocalY),
     &        sumMax/(numLocalX * numLocalY), devAllMax, numBig,
     &        numLocalX * numLocalY, stoplim
1015      format(/,'Local fits to a minimum of',i4,' points give:',/,
     &        '   Average mean residual',f8.2,/,'   Average max residual',f8.2,
     &        /,'   Biggest max residual',f9.2,/,i5,' of',i5,
     &        ' local fits with max residual above',f8.1)

          if (devAllMax .lt. 1.5 * stoplim .and.
     &        numBig .le. 0.05 * numLocalX * numLocalY) then
            write(*,'(/,a,/)') 'The local fits indicate that THIS '//
     &          'SOLUTION IS GOOD ENOUGH'
            devmax = stoplim - 1
          endif
          limRaised = devAllMax + 1.2

          if (shiftLimit .gt. 0.) then
c           
c             Get the data for the central area and transform the points by
c             the global transformation and measure shift in the points.
c             3/19/07: switched to this from doing a fit because the fit can
c             screw up the shifts if there are not points on both sides
            xcen = 0.
            ycen = 0.
            size = targetSize
            call fillLocalData(xcen, ycen, size, localNum, pnta, pntb, npnta,
     &          mapab, jxyz, xr, msiz, ndat)
            dxyzloc(1) = 0.
            dxyzloc(2) = 0.
            dxyzloc(3) = 0.
            do ip = 1, ndat
              do j = 1, 3
                xr(10+j,ip) = dxyz(j)
                do i = 1, 3
                  xr(10+j,ip) = xr(10+j,ip) + a(j,i) * xr(i,ip)
                enddo
                dxyzloc(j) = dxyzloc(j) + (xr(10+j,ip) - xr(ncolfit+1+j,ip)) /
     &              ndat
              enddo
c              write(*,132)(xr(j,ip),j=1,3),(xr(j,ip),j=5,7),(xr(j,ip),j=11,13)
c132           format(9f8.1)
            enddo
            csdx = dxyzloc(1)
            csdy = dxyzloc(2)
            csdz = dxyzloc(3)
            dist = sqrt(csdx**2 + csdy**2 + csdz**2)
c             print *,'distance',dist, csdx,csdy,csdz
            if (dist .ge. shiftLimit) then
              write(*, 1016)dist, nint(csdx),nint(csdy),nint(csdz),
     &            nint(csdx),nint(csdz),nint(csdy)
1016          format(/,'Center shift indicated by local fit is',f6.0,
     &            ', bigger than the specified limit',/,
     &            '   The InitialShiftXYZ for corrsearch3d needs to be',3i5,/,
     &            '   In eTomo, set Patchcorr Initial shifts in X, Y, Z to',3i5)
              if (nxyz(jxyz(3),2) .gt. nxyz(jxyz(3),1))
     &            write(*,1017) nxyz(2,jxyz(3))
1017          format('   You should also set thickness of initial ',
     &            'matching file to at least', i5,/,
     &            '     (In eTomo, Initial match size for Matchvol1)')
              write(*,1018)nint(dist) + 1
1018          format('   To avoid stopping with this error, set CenterShift',
     &            'Limit to',i4,/,
     &            '     (In eTomo, Limit on center shift for Solvematch)')
              if (devmax .lt. stoplim) then
                write(*,'(/,a)')'ERROR: SOLVEMATCH - INITIAL SHIFT NEEDS TO'//
     &              ' BE SET FOR PATCH CORRELATION (BUT SOLUTION IS OK)'
              else
                write(*,'(/,a)')'ERROR: SOLVEMATCH - INITIAL SHIFT NEEDS TO'//
     &              ' BE SET FOR PATCH CORRELATION'
              endif
              ierr = 1
            endif
          endif
        endif
      endif


      if (devmax.gt.stoplim) then
c         
c         Give some guidance based upon the ratios between max and mean
c         deviation and max deviation and stopping limit
c         
        write(*,106)devmax
106     format(/, 'The maximum residual is',f8.2,', too high to proceed')
        loMaxAvgRatio = 4.
        hiMaxAvgRatio = 12.
        loMaxLimRatio = 2.
        hiMaxLimRatio = 3.
        globLocAvgRatio = 3.
        if ((devmax .lt. loMaxAvgRatio * devavg .and.
     &      devmax .lt. loMaxLimRatio * stoplim) .or. (devAllMAx .gt. 0 .and.
     &      devavgLoc * globLocAvgRatio .lt. devavg .and.
     &      devAllMax .lt. loMaxLimRatio * stoplim)) then
          if (nTransCoord .gt. 0) then
            write(*,1115)limRaised
1115        format('Since corresponding points were picked using coordinates ',
     &          'from transferfid,',/,'this is almost certainly due to ',
     &          'distortion between the volumes,',/,
     &          ' and you should just raise the residual limit to',i4)
          else
            if (devAllMax .le. 0.)then
              write(*,111)loMaxAvgRatio, devavg, loMaxLimRatio,limRaised
            else
              write(*,1116)globLocAvgRatio, devAllMax,loMaxLimRatio,limRaised
            endif
111         format('Since the maximum residual is less than',f6.1,
     &          ' times the mean residual (', f8.2,')',/,' and less than',
     &          f6.1, ' times the specified residual limit,',/,
     &          ' this is probably due to distortion between the volumes,',/,
     &          'and you should probably just raise the residual limit to',i4)
1116        format('Since the local fits improved the mean residual by more',
     &          ' than a factor of',f6.1,/,' and the local maximum residual (',
     &          f8.2,') is less than', f6.1, ' times the',/,
     &          ' specified residual limit, this is probably due to ',
     &          'distortion between the',/, ' volumes, and you should ',
     &          'probably just raise the residual limit to',i4)
          endif
        elseif (nTransCoord .gt. 0) then
          write(*,112)limRaised,iorigmax
112       format('Since corresponding points were picked using coordinates ',
     &        'from transferfid,',/,'this is probably due to distortion ',
     &        'between the volumes.',/,'You could raise the residual limit to',
     &        i4,' or start with a subset of points.',/,'Bad correspondence ',
     &        'is unlikely but you could check points (especially',i4,').')
        elseif (devmax .gt. hiMaxAvgRatio * devavg .or.
     &        devmax .gt. hiMaxLimRatio * stoplim) then
          if (devmax .gt. hiMaxAvgRatio * devavg)
     &        write(*,108)hiMaxAvgRatio, devavg
108       format('The maximum residual is more than', f6.1,
     &        ' times the mean residual (', f8.2,')')
          if (devmax .gt. hiMaxLimRatio * stoplim)
     &        write(*,109)hiMaxLimRatio
109       format('The maximum residual is more than', f6.1,
     &        ' times the specified residual limit')
          print *,'This is probably due to a bad correspondence list.'
          write (*,110)iorigmax
110       format('Check the points (especially',i4,
     &        ') or start with a subset of the list')
        else
          print *,'The situation is ambiguous but could be due to a',
     &        ' bad correspondence list.'
          write (*,110)iorigmax
        endif
        call exiterror('MAXIMUM RESIDUAL IS TOO HIGH TO PROCEED')
      endif
      call exit(ierr)
      end


c       GETDELTA returns the "delta" or pixel size from a tomogram as well
c       as the nxyz
c       OPTION is the option that specifies the tomogram or nx,ny,nz
c       DELTA is returned with -1 if the option was not entered at all,
c       0 if sizes were entered, or the actual delta value from the file
c       
      subroutine getDelta(option, delta, nxyz)
      implicit none
      integer*4 nxyz(3),mxyz(3),mode,i, PipGetString
      character*(*) option
      character*80 line
      real*4 dmin,dmax,dmean,deltmp(3),delta
      logical line_is_filename
c       
      delta = -1.
      if (PipGetString(option, line) .gt. 0)  return
      delta = 0.
      if (line_is_filename(line)) go to 10
      read(line,*,err=10,end=10)(nxyz(i),i=1,3)
      return
c       
10    call ialprt(.false.)
      call imopen(5,line,'ro')
      call irdhdr(5,nxyz,mxyz,mode,dmin,dmax,dmean)
      call irtdel(5, deltmp)
      call imclose(5)
      call ialprt(.true.)
      delta = deltmp(1)
      return
      end


c       getFiducials reads the fiducials from FILENAME, storing the
c       sequential point number in ICONTA, the X,Y,Z coordinates in PNTA,
c       and the number of points in NPNTA
c       IDIM specifies the dimensions of the arrays
c       AXIS specifies the axis for a message
c       If a pixel size is found on the first line, it is returned in
c       PIXELSIZE, otherwise this is set to 0
c       
      subroutine getFiducials(filename, iconta, pnta, npnta, modobj,
     &    modcont, idim, axis, pixelSize, nxFid, nyFid)
      implicit none
      character*(*) filename
      character*(*) axis
      character*100 line
      integer*4 iconta(*),idim,npnta,len,i,j,nxFid, nyFid, nfields
      integer*4 modobj(*), modcont(*), numeric(10)
      real*4 pnta(3, idim), pixelSize, xnum(10)
c       
      npnta = 0
      pixelSize = 0.
      nxFid = 0
      nyFid = 0
      call dopen(1,filename,'old','f')
c       
c       get the first line and search for PixelSize: (first version)
c       or Pix: and Dim: (second version)
c       
      read(1,'(a)',err=10,end=15)line
      len = len_trim(line)
      i = 1
      do while (i .lt. len - 11)
        if (line(i:i+10) .eq. 'Pixel size:')
     &      read(line(i+11:len),*, err=8)pixelSize
        if (line(i:i+3) .eq. 'Pix:')
     &      read(line(i+4:len),*, err=8)pixelSize
        if (line(i:i+3) .eq. 'Dim:')
     &      read(line(i+4:len),*, err=8)nxFid,nyFid
        i = i + 1
      enddo
c       
c       process first line then loop until end
c       
      do while(.true.)
        npnta=npnta+1
        if (npnta.gt.idim)call exiterror('TOO MANY POINTS FOR ARRAYS')
        call frefor2(line, xnum, numeric, nfields, 6)
        if (nfields .eq. 6 .and. numeric(5) .eq. 1) then
          read(line, *)iconta(npnta),(pnta(j,npnta),j=1,3),modobj(npnta),
     &        modcont(npnta)
        else
          read(line, *)iconta(npnta),(pnta(j,npnta),j=1,3)
          modobj(npnta) = 0
          modcont(npnta) = 0
        endif
c       
c       invert the Z coordinates because the tomogram built by TILT is
c       inverted relative to the solution produced by TILTALIGN
c         12/1/09: THIS IS NOT TRUE AFTER ACCOUNTING FOR FLIPPING IN 3dMOD
        pnta(3,npnta) = -pnta(3,npnta)
        read(1,'(a)',end=15,err=10)line
      enddo
c
15    print *,npnta,' points from ', axis
      close(1)
      return
10    call exiterror('READING FIDUCIAL POINT FILE')
8     call exiterror('READING PIXEL SIZE OR DIMENSIONS FROM POINT FILE')
      end


c       readFidModel gets the filename with the given OPTION, reads fiducial
c       model, finds points with Z = IZBEST, and returns their X/Y coords
c       in FIDMODX, FIDMODY and their object and contour numbers in MODOBJFID
c       and MODCONTFID.  NUMFID is the number of fiducials returned; iDIM 
c       specifies the dimensions of the arrays; ABTEXT is A or B.  transPixel
c       is a pixel size for transfer coords if non-zero.
c
      subroutine readFidModelFile(option, izbest, abtext, fidModX,
     &    fidModY, modObjFid, modContFid, numFid, idim, transPixel)
      implicit none
      include 'model.inc'
      character*(*) option
      character*1 abtext
      integer*4 izbest, idim, numFid, modObjFid(*), modContFid(*)
      real*4 fidModX(*), fidModY(*), fidScale, transPixel
      real*4 ximscale, yimscale, zimscale
      character*160 filename
      integer*4 iobj, ip, ipt
      logical*4 looking
      logical readw_or_imod
      integer*4 PipGetString, getImodScales

      if (PipGetString(option, filename) .ne. 0) call exiterror('FIDUCIAL '//
     &    'MODELS FOR BOTH AXES MUST BE ENTERED TO USE TRANSFER COORDS')
      if (.not.readw_or_imod(filename)) call exiterror(
     &    'READING FIDUCIAL MODEL FOR AXIS '//abtext)
c       
c       If a pixel size was defined for transfer, scale coords to match that
      fidScale = 1.
      ip = getImodScales(ximscale, yimscale, zimscale)
      if (ip .eq. 0 .and. transPixel .gt. 0.) fidScale = ximscale / transPixel
      call scale_model(0)
      numFid = 0
      do iobj = 1, max_mod_obj
        looking = .true.
        ip = 1
c         
c         Find point at best z value, record it and its obj/cont
c
        do while (looking .and. ip .le. npt_in_obj(iobj))
          ipt=abs(object(ibase_obj(iobj)+ip))
          if (nint(p_coord(3, ipt)) .eq. izbest) then
            numFid = numFid + 1
            if (numFid .gt. idim) call exiterror('TOO MANY FIDUCIAL '//
     &          'CONTOURS FOR ARRAYS IN MODEL FOR AXIS '//abtext)
            fidModX(numFid) = p_coord(1, ipt) * fidScale
            fidModY(numFid) = p_coord(2, ipt) * fidScale
            call objtocont(iobj, obj_color, modObjFid(numFid),
     &          modContFid(numFid))
c            print *, modObjFid(numFid),modContFid(numFid),fidModX(numFid),
c     &          fidModY(numFid)
            looking = .false.
          endif
          ip = ip + 1
        enddo
      enddo
      return
      end


c       ROTATEFIDS rotates, scales and shifts the NPNTA fiducials in PNTA
c       XTILT is the x axis tilt, aScale is the scaling, angleOffset
c
      subroutine rotateFids(pnt, npnt, xtilt, scale, angleOffset, zShift)
      implicit none
      real*4 pnt(3,*), xtilt, scale, angleOffset, zShift
      real*4 cosa, cosb, sina, sinb, tmpy
      integer*4 npnt, i
      real*4 sind, cosd
c
c       use the negative of the angle to account for the inversion of the
c       tomogram; rotate the 3-d points about the Y axis first, then the Z
c       axis; add Z shift
c       12/1/09: THE NEGATIVE COMPENSATES FOR THE FLIPPING OF Z ABOVE, BUT
c       ARE THERE OTHER CONSEQUENCES?  IS ALL THIS RIGHT?
      cosa=cosd(-xtilt) * scale
      sina=sind(-xtilt) * scale
      sinb = sind(-angleOffset)
      cosb = cosd(-angleOffset)
      do i=1,npnt
        tmpy = cosb * pnt(1,i) - sinb * pnt(3,i)
        pnt(3,i) = sinb * pnt(1,i) + cosb * pnt(3,i)
        pnt(1,i) = tmpy
        tmpy=cosa*pnt(2,i)-sina*pnt(3,i)
        pnt(3,i)=sina*pnt(2,i)+cosa*pnt(3,i) + zShift
        pnt(2,i)=tmpy
        pnt(1,i) = pnt(1,i) *scale
      enddo
      return
      end


c       fillLocalData fills the data array XR with at least MINDAT points
c       from an area centered at XCEN, YCEN.  SIZE is called with an initial
c       trial size, and returned with the final size needed to include the
c       required points.  PNTA and PNTB have the points, NPNTA is the total
c       number in PNTA, MAPAB is the mapping to indices in PNTB, JXYZ is the
c       dimension mapping.  NDAT is returned with number of points.
c
      subroutine fillLocalData(xcen, ycen, size, mindat, pnta, pntb, npnta,
     &    mapab, jxyz, xr, msiz, ndat)
      implicit none
      integer*4 npnta, mapab(*), jxyz(3), msiz, ndat, ia, j, mindat
      real*4 xcen, ycen, size, pnta(3,*), pntb(3,*), xr(msiz,*)
      real*4 xmin, ymin, xmax, ymax, sizein
c
      sizein = size
      ndat = 0
      do while (ndat .lt. mindat)
        ndat = 0
        xmin = xcen - size / 2.
        xmax = xcen + size / 2.
        ymin = ycen - size / 2.
        ymax = ycen + size / 2.
        do ia = 1, npnta
          if (mapab(ia) .gt. 0 .and. pnta(1, ia) .ge. xmin .and.
     &        pnta(1, ia) .le. xmax .and. pnta(2, ia) .ge. ymin
     &        .and. pnta(2, ia) .le. ymax) then
            ndat = ndat + 1
            do j = 1, 3
              xr(j,ndat)=pntb(jxyz(j),mapab(ia))
              xr(j+4,ndat)=pnta(jxyz(j),ia)
            enddo
          endif
        enddo
        if (ndat .lt. mindat) size = size + 0.02 * sizein
      enddo
      return 
      end

*************EDMONT.F**********************************************
c       
c       A GENERAL MONTAGE EDITOR TO MOVE IMAGES INTO, OUT OF, OR BETWEEN
c       MONTAGES It can float the images to a common range or mean of density,
c       scaling all of the pieces in a section by the same amount. It can
c       output only the pieces that intersect a defined subset of the image
c       area. 
c
c       $Id$
c       
      implicit none
      include 'smallmodel.inc'
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3),NXYZ2(3),MXYZ2(3),NX,NY,NZ
      real*4 TITLE(20), CELL2(6),cell(6),delt(3), xorig, yorig, zorig
C       
      CHARACTER*320, allocatable :: FILIN(:),FILOUT(:), pifilin(:),pifilout(:)
C       
      EQUIVALENCE (NX,NXYZ(1)), (ny, nxyz(2)), (nz, nxyz(3))
      CHARACTER*320 modelfile
C       
      DATA NXYZST/0,0,0/
      character*20 floatxt/' '/,trunctxt/' '/
      integer*4, allocatable :: inlist(:),inlistTmp(:),nlist(:),listind(:)
     &    ,nsecout(:),listz(:) ,ixpclist(:),iypclist(:) ,izpclist(:)
     &    ,ixpcout(:),iypcout(:),izpcout(:), ixko(:),iyko(:),izko(:)
      real*4 optmax(16)
      real*4, allocatable :: dminsec(:),dmaxsec(:),avgsec(:), sdsec(:),array(:)
      real*4, allocatable :: templine(:)
      integer*1, allocatable :: extrain(:),extraout(:)
      data optmax/255.,32767.,4*255.,65535.,2*255.,511.,1023.,2047.,4095.,
     &    8191., 16383.,32767./
      logical rescale
      integer(kind =8) i8, npix
      character dat*9,tim*8
      integer*2 temp
      character*80 titlech
      integer*4 maxextra, maxpieces,limextra,limpiece,idim, limlist
      integer*4 ierr, nfilein, numInputFiles, numPieceFiles, numSecLists
      integer*4 listot,ifile,i,mode, nbsymin,nlistz, minxpiece ,nxpieces,j
      integer*4 mxoverlap, minypiece, nypieces, myoverlap,newmode,nxoverlap
      integer*4 nyoverlap, minAllXpiece, minAllYpiece, maxXpiece, maxYpiece
      integer*4 numOutValues, numToGet,nfileout,noutot,numOutEntries
      integer*4 numOutputFiles,noutXpiece,noutYpiece,minxinc,maxxinc,minyinc
      integer*4 maxyinc,minFrame,maxFrame,knocks,ifknock,iobj,ipt,ifmean
      integer*4 iffloat,ifrenumb,ifshiftxy,ibinning,maxBinning,nxbin
      integer*4 nxbOverlap, ixst, nybin, nybOverlap, iyst, lentemp,ilis,indsec
      real*4 fraczero, zmin, zmax, dmin2,dmax2, avg, sd,tmpmin, tmpmax,dmaxout
      real*8 sum8,sumsq8,tsum8, tsumsq8,dmean2
      real*4 optin,optout,bottomin,bottomout,sum,zminsec,zmaxsec,dminout
      real*4 denoutmin,sclfac, const,den,dmax,dmean,dmin,dminin
      integer*4 nsecred,ifanyout,ipc,isec,isecout,ifileout,ipcout,kti,npclist
      integer*4 nbytexout,nbsymout,indxout,minSubXpiece,minSubYpiece,nbcopy
      integer*4 nbclear,nbytexin,iflagxin,numPLfileOut,maxsecout,ind
      integer*4 maxexout, maxNumXpc, maxNumYpc, maxbytexin
      character*100000 listString
      
      logical readSmallMod,notKnock
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString,PipGetTwoIntegers
      integer*4 PipGetIntegerArray, PipGetNonOptionArg
c       
c       fallbacks from ../../manpages/autodoc2man -2 2 edmont
      integer numOptions
      parameter (numOptions = 18)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'imin:ImageInputFile:FNM:@plin:PieceListInput:FNM:@'//
     &    'imout:ImageOutputFile:FNM:@plout:PieceListOutput:FNM:@'//
     &    'secs:SectionsToRead:LIM:@numout:NumberToOutput:IAM:@'//
     &    'mode:ModeToOutput:I:@xminmax:XMinAndMax:IP:@'//
     &    'Yminmax:YMinAndMax:IP:@xframes:XFrameMinAndMax:IP:@'//
     &    'yframes:YFrameMinAndMax:IP:@float:FloatDensities:I:@'//
     &    'bin:BinByFactor:I:@exclude:ExclusionModel:FN:@'//
     &    'renumber:RenumberZFromZero:B:@shift:ShiftXYToZero:B:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       
      limextra = 20000000
      limpiece = 2500000
      limlist = 10000000
      maxBinning = 32
      fraczero=0.
      ifmean=0
      iffloat = 0
      ifrenumb = 0
      ifshiftxy = 0
      ibinning = 1
      minxinc=0
      maxxinc=0
      minyinc=0
      maxyinc=0

      call PipReadOrParseOptions(options, numOptions, 'edmont',
     &    'ERROR: EDMONT - ', .false., 2, 2, 1, numOptArg,
     &    numNonOptArg)
c       
c       Allocate oversized temporary arrays
      allocate(inlistTmp(limlist), listz(limpiece), ixpclist(limpiece),
     &    iypclist(limpiece) ,izpclist(limpiece), extrain(limextra),
     &    stat = ierr)
      call memoryError(ierr, 'INITIAL OVERSIZED ARRAYS')

C       
c       Get the input files
      call PipNumberOfEntries('ImageInputFile', numInputFiles)
      nfilein = numInputFiles + max(0, numNonOptArg - 1)
      if (nfilein .eq. 0) call exitError('NO INPUT IMAGE FILE NAME ENTERED')
      call PipNumberOfEntries('PieceListInput', numPieceFiles)
      if (numPieceFiles .gt. 0 .and. numPieceFiles .ne. nfilein) call exitError
     &    ('THERE MUST BE ONE PIECE LIST ENTRY FOR EACH IMAGE FILE IF '//
     &    'THERE ARE ANY PIECE LIST FILES')
      call PipNumberOfEntries('SectionsToRead', numSecLists)
      if (numsecLists .gt. nfilein) call exitError(
     &    'THERE ARE MORE SECTION LISTS THAN INPUT FILES')
      
      allocate(filin(nfilein), pifilin(nfilein), nlist(nfilein),
     &    listind(nfilein), stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR FILES')

      listot=0
      maxextra = 0
      maxpieces = 0
      maxbytexin = 0
      maxNumXpc = 0
      maxNumYpc = 0
      nxoverlap = -10000
      nyoverlap = -10000
      do ifile=1,nfilein
        if (ifile .le. numInputFiles) then
          ierr = PipGetString('ImageInputFile', filin(ifile))
        else
          ierr = PipGetNonOptionArg(ifile - numInputFiles, filin(ifile))
        endif
        pifilin(ifile) = ' '
        if (numPieceFiles .gt. 0) then
          ierr = PipGetString('PieceListInput', pifilin(ifile))
          if (pifilin(ifile) .eq. 'none') pifilin(ifile) = ' '
        endif
c         
c         open the files to get properties and check them
        call openAndAnalyzeFiles(filin(ifile), pifilin(ifile), mxyz, dminin,
     &      .false., mode, extrain, limextra, nbsymin, nbytexin, ixpclist,
     &      iypclist, izpclist, npclist, limpiece, listz, nlistz, minxpiece,
     &      nxpieces, mxoverlap, minypiece, nypieces, myoverlap)
        call imclose(1)
        maxextra = max(maxextra, nbsymin)
        maxpieces = max(maxpieces, npclist)
        maxbytexin = max(maxbytexin, nbytexin)
        maxNumXpc = max(maxNumXpc, nxpieces)
        maxNumYpc = max(maxNumYpc, nypieces)
c         
c         The first file with multiple pieces defines the overlap on an axis
        if (nxoverlap .eq. -10000 .and. nxpieces .gt. 1) nxoverlap = mxoverlap
        if (nyoverlap .eq. -10000 .and. nypieces .gt. 1) nyoverlap = myoverlap
c         
c         The first file defines output mode and size; initialize for mins/maxs
        if (ifile .eq. 1) then
          newmode = mode
          nxyz = mxyz
c           
c           Keep track of overall minimum and maximum piece coordinate
          minAllXpiece = minxpiece
          minAllYpiece = minypiece
          maxXpiece = minxpiece + (nxpieces - 1) * (nx - nxoverlap)
          maxYpiece = minypiece + (nypieces - 1) * (ny - nyoverlap)
        else
c           
c           if overlap still not defined on an axis and there is now a
c           disparity, try to set up an overlap that makes sense
          if (nxoverlap .eq. -10000 .and. minxpiece .ne. minAllXpiece) then
            i = 1
            do while (abs(minxpiece - minAllXpiece) / i .gt. nx)
              i = i + 1
            enddo
            nxoverlap = nx - abs(minxpiece - minAllXpiece) / i
          endif
          if (nyoverlap .eq. -10000 .and. minypiece .ne. minAllYpiece) then
            i = 1
            do while (abs(minypiece - minAllYpiece) / i .gt. ny)
              i = i + 1
            enddo
            nyoverlap = ny - abs(minypiece - minAllYpiece) / i
          endif
          if (nx .ne. mxyz(1) .or. ny .ne. mxyz(2))
     &        call exitError('ALL IMAGE FILES MUST HAVE THE SAME SIZE PIECES')
          if ((nxpieces .gt. 1 .and. nxoverlap .ne. mxoverlap) .or.
     &        (nypieces .gt. 1 .and. nyoverlap .ne. myoverlap))
     &        call exitError('ALL MONTAGES MUST HAVE THE SAME OVERLAPS')
          if (mod(abs(minxpiece - minAllXpiece), nx - nxoverlap) .gt. 0 .or.
     &        mod(abs(minypiece - minAllYpiece), ny - nyoverlap) .gt. 0)
     &        call exitError('ALL MONTAGES MUST HAVE PIECE COORDINATES ON'//
     &        ' THE SAME REGULAR GRID')
          minAllXpiece = min(minAllXpiece, minxpiece)
          minAllYpiece = min(minAllYpiece, minypiece)
          maxXpiece = max(maxXpiece, minxpiece + (nxpieces-1) * (nx-nxoverlap))
          maxYpiece = max(maxYpiece, minypiece + (nypieces-1) * (ny-nyoverlap))
        endif
c         
c         Initialize the section list then get it if there is one
        if (listot+nlistz .gt. limlist)
     &      call exitError('TOO MANY SECTIONS FOR INITIAL ARRAYS')
        inlistTmp(listot+1:listot+nlistz) = listz(1:nlistz)
        nlist(ifile) = nlistz
        if (ifile .le. numSecLists) then
          ierr = PipGetString('SectionsToRead', listString)
          call parselist2(listString, inlistTmp(listot+1),nlist(ifile),
     &        limlist-listot)
c           
c           Check for legality
          do i = 1, nlist(ifile)
            ierr = 1
            do j = 1, nlistz
              if (listz(j) .eq. inlistTmp(listot+i)) then
                ierr = 0
                exit
              endif
            enddo
            if (ierr .eq. 1) then
              write(*,'(/,a,i5,a,i6,a)')'ERROR: EDMONT - SECTION LIST #',
     &            ifile,' CONTAINS ',
     &            inlistTmp(listot+i),', WHICH IS NOT A SECTION IN THAT FILE'
              call exit(1)
            endif
          enddo
        endif
        listind(ifile)=listot+1
        listot=listot+nlist(ifile)
      enddo
c       
c       Get output files and numbers to output
      call PipNumberOfEntries('ImageOutputFile', numOutputFiles)
      nfileout = numOutputFiles + min(1, numNonOptArg)
      if (nfileout .eq. 0) call exitError('NO OUTPUT IMAGE FILE NAME ENTERED')
      allocate(filout(nfileout), pifilout(nfileout), nsecout(nfileout),
     &    stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR OUTPUT FILES')
      call PipNumberOfEntries('PieceListOutput', numPLfileOut)
      if ((numPLfileOut .eq. 0 .and. numPieceFiles .gt. 0) .or.
     &    (numPLfileOut .ne. 0 .and. numPLfileOut .ne. nfileout))
     &    call exitError('THERE MUST BE AN OUTPUT PIECE LIST FILE FOR EACH '//
     &    'OUTPUT IMAGE FILE IF THERE ARE INPUT PIECE LIST FILES')
c
c       Take care of output numbers first so they can be totaled
      if (nfileout .eq. 1) then
        nsecout(1)=listot
      elseif (nfileout .eq. listot) then
        do i = 1, nfileout
          nsecout(i) = 1
        enddo
      else
        call PipNumberOfEntries('NumberToOutput', numOutEntries)
        if (numOutEntries .eq. 0)
     &      call exitError('YOU MUST SPECIFY NUMBER OF SECTIONS '//
     &      'TO WRITE TO EACH OUTPUT FILE')
            
        numOutValues = 0
        do i = 1, numOutEntries
          numToGet = 0
          ierr = PipGetIntegerArray('NumberToOutput',
     &        nsecout(numOutValues + 1), numToGet, nfileout - numOutValues)
          numOutValues = numOutValues + numToGet
        enddo
        if (numOutValues .ne. nfileout) call exitError(
     &      'THE NUMBER OF VALUES FOR SECTIONS TO OUTPUT DOES'
     &      //' NOT EQUAL THE NUMBER OF OUTPUT FILES')
      endif
 
      noutot=0
      maxsecout = 0
      do ifile=1,nfileout
        if (ifile .le. numOutputFiles) then
          ierr = PipGetString('ImageOutputFile', filout(ifile))
        else
          ierr = PipGetNonOptionArg(numNonOptArg, filout(ifile))
        endif
        pifilout(ifile) = ' '
        if (numPLfileOut .gt. 0)
     &      ierr = PipGetString('PieceListOutput', pifilout(ifile))
        noutot=noutot+nsecout(ifile)
        maxsecout = max(maxsecout, nsecout(ifile))
      enddo
       if(noutot.ne.listot)call exitError(
     &    'NUMBER OF INPUT AND OUTPUT SECTIONS DOES NOT MATCH')
       noutXpiece = 1 + (maxXpiece - minAllXpiece) / (nx - nxoverlap) 
       noutYpiece = 1 + (maxYpiece - minAllYpiece) / (ny - nyoverlap) 
c       
c       get limits on output size
c       
      ierr = PipGetTwoIntegers('XMinAndMax', minxinc,maxxinc)
      if (PipGetTwoIntegers('XFrameMinAndMax', minFrame, maxFrame).eq. 0)then
        if (ierr .eq. 0) call exitError(
     &      'YOU CANNOT ENTER BOTH -xminmax AND -xframes')
        if (minFrame .lt. 1 .or. maxFrame .gt. noutXpiece .or.
     &      minFrame .gt. maxFrame) call exitError
     &      ('MINIMUM AND MAXIMUM FRAMES IN X OUT OF RANGE OR OUT OF ORDER')
        minxinc = minAllXpiece + (minFrame - 2) * (nx - nxoverlap) + nx
        maxxinc = minAllXpiece + maxFrame * (nx - nxoverlap) - 1
        maxNumXpc = maxFrame + 1 - minFrame
      endif
      ierr = PipGetTwoIntegers('YMinAndMax', minyinc,maxyinc)
      if (PipGetTwoIntegers('YFrameMinAndMax', minFrame, maxFrame).eq. 0)then
        if (ierr .eq. 0) call exitError(
     &      'YOU CANNOT ENTER BOTH -yminmax AND -yframes')
        if (minFrame .lt. 1 .or. maxFrame .gt. noutYpiece .or.
     &      minFrame .gt. maxFrame) call exitError
     &      ('MINIMUM AND MAXIMUM FRAMES IN Y OUT OF RANGE OR OUT OF ORDER')
        minyinc = minAllYpiece + (minFrame - 2) * (ny - nyoverlap) + ny
        maxyinc = minAllYpiece + maxFrame * (ny - nyoverlap) - 1
        maxNumYpc = maxFrame + 1 - minFrame
      endif
c       
c       Get model to knock out pieces
      knocks=0
      ifknock = 1 - PipGetString('ExclusionModel', modelfile)
      if(ifknock.ne.0)then
        if (.not.readSmallMod(modelfile)) call exitError(
     &      'READING PIECE EXCLUSION MODEL')
        call scale_model(0)
        do iobj = 1, max_mod_obj
          knocks = knocks + npt_in_obj(iobj)
        enddo
      endif
      allocate(ixko(knocks+1),iyko(knocks+1),izko(knocks+1), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR EXCLUDING PIECES')
      if (ifknock .ne. 0) then
        knocks=0
        do iobj = 1, max_mod_obj
          do ipt = 1, npt_in_obj(iobj)
            knocks = knocks + 1
            ixko(knocks) = nint(p_coord(1,ibase_obj(iobj)+ipt))
            iyko(knocks) = nint(p_coord(2,ibase_obj(iobj)+ipt))
            izko(knocks) = nint(p_coord(3,ibase_obj(iobj)+ipt))
          enddo
        enddo
      endif
c       
      ierr = PipGetInteger('ModeToOutput', newmode)
c       
      ierr = PipGetInteger('FloatDensities', iffloat)
      if(iffloat.gt.1)ifmean=1
c       
c       renumber sections starting at zero: and if not, check for duplicate Z
      ierr = PipGetBoolean('RenumberZFromZero', ifrenumb)
      ierr = PipGetBoolean('ShiftXYToZero', ifshiftxy)
      ierr = PipGetInteger('BinByFactor', ibinning)
      if (ibinning .lt. 1 .or. ibinning .gt. maxBinning)
     &    call exitError('BINNING IS OUTSIDE OF ALLOWED RANGE')
      minSubXpiece = minAllXpiece / ibinning
      minSubYpiece = minAllYpiece / ibinning
      call PipDone()
c       
c       Manage memory
      allocate(inlist(listot), stat=ierr)
      call memoryError(ierr, 'ARRAY FOR SECTIONS TO DO')
      inlist(1:listot) = inlistTmp(1:listot)
      deallocate(inlistTmp, listz, ixpclist, iypclist, izpclist, extrain)
      maxexout = maxsecout * maxNumXpc * maxNumYpc * maxbytexin
      i = maxsecout * maxNumXpc * maxNumYpc
      allocate(listz(maxpieces), ixpclist(maxpieces),
     &    iypclist(maxpieces) ,izpclist(maxpieces), extrain(maxextra),
     &    extraout(maxexout), dminsec(listot), dmaxsec(listot), avgsec(listot),
     &    sdsec(listot), ixpcout(i), iypcout(i), izpcout(i), stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR PIECES AND EXTRA HEADER')
      nxbin = nx / ibinning
      nxbOverlap = nint((nxoverlap - mod(nx, ibinning)) / float(ibinning))
      ixst = mod(nx, ibinning) / 2
      nybin = ny / ibinning
      nybOverlap = nint((nyoverlap - mod(ny, ibinning)) / float(ibinning))
      iyst = mod(ny, ibinning) / 2
      lentemp = 2 * ibinning * nx
      idim = nxbin * nybin
      allocate(templine(lentemp),array(idim), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR IMAGE DATA')
      if (ifrenumb .eq. 0 .or. iffloat.ne.0 .or. knocks .gt. 0 .or.
     &    minxinc .ne. 0 .or.
     &    maxxinc .ne. 0 .or. minyinc .ne. 0 .or. maxyinc .ne. 0) then
c       
c         need to go through all the files and check for unique output pieces
c         if not renumbering in Z; get the actual range of pieces and check
c         for existence of pieces on each section if any pieces are being
c         removed, and get means if floating
        isecout=1
        ifileout=1
        ipcout=0
        if(iffloat.ne.0)then
          floatxt=', floated to range'
          if (iffloat.lt.0) floatxt=', scaled to range'
          if(fraczero.ne.0.)
     &        write(trunctxt,'(a,f6.3)')', truncated by',fraczero
          if(ifmean.ne.0)floatxt=', floated to means'
        endif
        zmin=1.e30
        zmax=-1.e30
        dminout = 1.e30
        dmaxout = -1.e30
        minSubXpiece = maxXpiece + nx
        minSubYpiece = maxYpiece + ny
        do ifile=1,nfilein
          call openAndAnalyzeFiles(filin(ifile), pifilin(ifile), mxyz, dminin,
     &        .false., mode, extrain, maxextra, nbsymin, nbytexin, ixpclist,
     &        iypclist, izpclist, npclist, maxpieces, listz, nlistz,
     &        minxpiece , nxpieces, mxoverlap, minypiece, nypieces, myoverlap)

          do ilis=1,nlist(ifile)
            indsec=ilis+listind(ifile)-1
            nsecred=inlist(indsec)
            dmin2=1.e30
            dmax2=-1.e30
            sum8=0.
            sumsq8=0.
            ifanyout = 0
            do ipc=1,npclist
              if(izpclist(ipc).eq.nsecred .and.
     &            ((minxinc.eq.0.and.maxxinc.eq.0) .or.
     &            (max(ixpclist(ipc),minxinc).le.
     &            min(ixpclist(ipc)+nx-1,maxxinc))) .and.
     &            ((minyinc.eq.0.and.maxyinc.eq.0) .or.
     &            (max(iypclist(ipc),minyinc).le.
     &            min(iypclist(ipc)+ny-1,maxyinc))) .and.
     &            notknock(ixpclist(ipc),iypclist(ipc),nsecred,nx,ny,nxoverlap,
     &            nyoverlap,ixko,iyko,izko,knocks)) then
                minSubXpiece = min(minSubXpiece, ixpclist(ipc))
                minSubYpiece = min(minSubYpiece, iypclist(ipc))
                if (iffloat.ne.0) then
c
c                   if floating, need to read all sections to get stats
c                   find the minimum of the ratio (dmean-dmin)/(dmax-dmin)
                  call irdbinned(1,ipc-1, array, nxbin, nybin, ixst,
     &                iyst, ibinning, nxbin, nybin, tempLine, lenTemp, ierr)
                  if (ierr .ne. 0) call exitError('READING IMAGE FILE')
                  if(ifmean.eq.0)then
                    call iclden(array, nxbin, nybin, 1, nxbin, 1, nybin,
     &                  tmpmin, tmpmax, sum)
                  else
                    call iclavgsd(array, nxbin, nybin, 1, nxbin, 1, nybin,
     &                  tmpmin, tmpmax, tsum8, tsumsq8, avg, sd)
                    
                    sum8 = sum8 + tsum8
                    sumsq8 = sumsq8 + tsumsq8
                  endif
                  dmin2 = min(dmin2, tmpmin)
                  dmax2 = max(dmax2, tmpmax)
                endif
                ifanyout = ifanyout + 1
                if (ifrenumb .eq. 0) then
                  do i = 1, ipcout
                    if (ixpcout(i) .eq. ixpclist(ipc) .and. iypcout(i) .eq.
     &                  iypclist(ipc) .and. izpcout(i) .eq. izpclist(ipc)) then
                      write(*,'(/,a,i5,a,3i8)')'ERROR: EDMONT - YOU MUST '//
     &                    'RENUMBER Z; OUTPUT FILE #',ifile,
     &                    ' WOULD CONTAIN TWO SECTIONS WITH X,Y,Z OF',
     &                    ixpcout(i), iypcout(i),izpcout(i)
                      call exit(1)
                    endif
                  enddo
                endif
                ipcout = ipcout + 1
                ixpcout(ipcout) = ixpclist(ipc)
                iypcout(ipcout) = iypclist(ipc)
                izpcout(ipcout) = izpclist(ipc)
              endif
            enddo
c             
c             Insist that some pieces  be found on this section!
            if (ifanyout .eq. 0) then
              write(*,'(/,a,i6,a,i5)')'ERROR: EDMONT - NO PIECES ARE '//
     &            'INCLUDED FROM SECTION',nsecred,
     &            ' WHICH IS IN THE LIST OF SECTIONS TO USE FROM FILE #',ifile
              call exit(1)
            endif
            if (iffloat .ne. 0) then
              dminsec(indsec)=dmin2
              dmaxsec(indsec)=dmax2
              dminout = min(dminout, dmin2)
              dmaxout = max(dmaxout, dmax2)
              if (ifmean.ne.0) then
                call sums_to_avgsd8(sum8, sumsq8,nxbin, nybin*ifanyout,avg,sd)
                avgsec(indsec)=avg
                sdsec(indsec)=sd
                zmin=min(zmin,(dmin2-avg)/sd)
                zmax=max(zmax,(dmax2-avg)/sd)
              endif
            endif
            isecout = isecout + 1
            if (isecout .gt. nsecout(ifileout)) then
              isecout = 1
              ifileout = ifileout + 1
              ipcout = 0
            endif
          enddo
          call imclose(1)
        enddo
c         
c         Convert minimum coordinates to binned coordinates
        i = (minSubXpiece - minAllXpiece) / (nx - nxoverlap)
        minSubXpiece = i * (nxbin - nxbOverlap) + minAllXpiece / ibinning
        i = (minSubYpiece - minAllYpiece) / (ny - nyoverlap)
        minSubYpiece = i * (nybin - nybOverlap) + minAllYpiece / ibinning
      endif
c       
c       start looping over input images
c       
      call time(tim)
      call date(dat)
      isec=1
      isecout=1
      ifileout=1
      ipcout=0
      do ifile=1,nfilein
        call openAndAnalyzeFiles(filin(ifile), pifilin(ifile), mxyz, dminin,
     &      .true., mode, extrain, maxextra, nbsymin, nbytexin, ixpclist,
     &      iypclist, izpclist, npclist, maxpieces, listz, nlistz, minxpiece ,
     &      nxpieces, mxoverlap, minypiece, nypieces, myoverlap)
        call irtsiz(1,nxyz,mxyz,nxyzst)
        call irtcel(1,cell)
        call irtdel(1, delt)
        call irtorg(1, xorig, yorig, zorig)
c         
c         get extra header information if any
c         
        if(nbsymin.gt.0)then
          call irtsym(1,nbsymin,extrain)
          call irtsymtyp(1,nbytexin,iflagxin)
        endif
C         
c         get each section in input file
        do ilis=1,nlist(ifile)
          indsec=ilis+listind(ifile)-1
          nsecred=inlist(indsec)
          ifanyout=0
          do ipc=1,npclist
            if(izpclist(ipc).eq.nsecred .and.
     &          ((minxinc.eq.0.and.maxxinc.eq.0) .or.
     &          (max(ixpclist(ipc),minxinc).le.
     &          min(ixpclist(ipc)+nx-1,maxxinc))) .and.
     &          ((minyinc.eq.0.and.maxyinc.eq.0) .or.
     &          (max(iypclist(ipc),minyinc).le.
     &          min(iypclist(ipc)+ny-1,maxyinc))).and.
     &          notknock(ixpclist(ipc),iypclist(ipc),nsecred,nx,ny,nxoverlap,
     &          nyoverlap,ixko,iyko,izko,knocks)) then
              call irdbinned(1,ipc-1, array, nxbin, nybin, ixst,
     &            iyst, ibinning, nxbin, nybin, tempLine, lenTemp, ierr)
              if (ierr .ne. 0) call exitError('READING IMAGE FILE')
              npix=int(nxbin,kind=8)*nybin
c               
c               calculate new min and max after rescaling under various
c               possibilities
c               
              optin=optmax(mode+1)
              optout=optmax(newmode+1)
c               
c               set bottom of input range to 0 unless mode 1 or 2; set bottom
c               of output range to 0 unless not changing modes
c               
              bottomin=0.
              if(dminin.lt.0..and.(mode.eq.1.or.mode.eq.2)) bottomin=-optin
              bottomout=0.
              if(mode.eq.newmode)bottomout=bottomin
              rescale=.false.
              if(iffloat.le.0)then
c                 
c                 get min and max 
                call iclden(array, nxbin, nybin, 1, nxbin, 1, nybin,
     &              tmpmin, tmpmax, sum)
                dmin2=tmpmin
                dmax2=tmpmax
              endif
c               
c               for no float: if mode = 2, no rescale
c               
              if(iffloat.le.0.and.newmode.ne.2 .and.
     &            (mode .ne. 2 .or. iffloat .lt. 0))then
c                 
c                 if within proper input range, rescale from input range to
c                 output range only if mode is changing
c                 
                rescale=mode.ne.newmode .and. (mode .ne. 2 .or. iffloat .lt. 0)
                if (iffloat .lt. 0 .and. rescale) then
                  bottomin = dminout
                  optin = dmaxout
                endif
                if(tmpmin.ge.bottomin.and.tmpmax.le.optin)then
                  dmin2=(tmpmin-bottomin)*(optout-bottomout)/
     &                (optin-bottomin)+bottomout
                  dmax2=(tmpmax-bottomin)*(optout-bottomout)/
     &                (optin-bottomin)+bottomout
                elseif(rescale)then
c                   :if outside proper range, tell user to start over
                  call exitError('Input data outside expected range. '//
     &                'Start over, specifying float to range')
                endif
              elseif(iffloat.gt.0)then
c                 If floating: scale to a dmin2 that will knock out fraczero
c                 of the range after truncation to zero
                dmin2=-optout*fraczero/(1.-fraczero)
                rescale=.true.
                tmpmin=dminsec(indsec)
                tmpmax=dmaxsec(indsec)
                if(ifmean.eq.0)then
c                   :float to range, new dmax2 is the max of the range
                  dmax2=optout
                else
c                   :float to mean, it's very hairy, first need mean again
                  zminsec=(tmpmin-avgsec(indsec))/sdsec(indsec)
                  zmaxsec=(tmpmax-avgsec(indsec))/sdsec(indsec)
                  dmin2=(zminsec-zmin)*optout/(zmax-zmin)
                  dmax2=(zmaxsec-zmin)*optout/(zmax-zmin)
                  dmin2=max(0.,dmin2)
c                   but in case of problems, just limit dmax2 to the range
                  dmax2=min(dmax2,optout)
                endif
              endif
              dmean2=0.
c               set up minimum value to output based on mode
              if(newmode.eq.1)then
                denoutmin=-32767
              elseif(newmode.eq.2)then
                denoutmin=-1.e30
                optout=1.e30
              else
                denoutmin=0.
              endif
c               
              if(rescale)then
c                 if scaling, set up equation, scale and compute new mean
                sclfac=(dmax2-dmin2)/(tmpmax-tmpmin)
                const=dmin2-sclfac*tmpmin
                dmin2=1.e20
                dmax2=-1.e20
                do i8=1,npix
                  den=sclfac*array(i8)+const
                  if(den.lt.denoutmin)then
c                     ntrunclo=ntrunclo+1
                    den=denoutmin
                  elseif(den.gt.optout)then
c                     ntrunchi=ntrunchi+1
                    den=optout
                  endif
                  array(i8)=den
                  dmean2=dmean2+den
                  dmin2=min(dmin2,den)
                  dmax2=max(dmax2,den)
                enddo
              else
c                 if not scaling, just need new mean
                do i8=1,npix
                  dmean2=dmean2+array(i8)
                enddo
              endif
              dmean2=dmean2/npix
              print *,'frame',isec-1,': min&max before and after, mean:'
              write(*,'(5f10.2)')tmpmin,tmpmax,dmin2,dmax2,dmean2
c               see if need to open an output file
              if(ipcout.eq.0)then
C                 
C                 Create output file, transfer header from currently open
c                 file, fix it enough to get going
                CALL IMOPEN(2,FILOUT(ifileout),'NEW')
                call itrhdr(2,1)
                call ialmod(2,newmode)
                NXYZ2(1)=NXbin
                NXYZ2(2)=NYbin
                NXYZ2(3)=1
                call ialsiz(2,nxyz2,nxyzst)
c                   
c                   adjust extra header information if current file has it
c                   
                nbsymout=0
                if(nbsymin.gt.0)then
                  nbytexout=nbytexin
                  nbsymout=nsecout(ifileout)*maxbytexin*maxNumXpc * maxNumYpc
                  call ialnbsym(2,nbsymout)
                  call imposn(2,0,0)
                  indxout=0
                endif
                write(titlech,301) floatxt,dat,tim
301             FORMAT('EDMONT: Images transferred',a18,t57,a9,2x,a8)
                read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
                DMAX=-100000.
                DMIN=100000.
                DMEAN=0.
              endif
C               
              DMIN = MIN(DMIN,DMIN2)
              DMAX = MAX(DMAX,DMAX2)
              DMEAN = DMEAN + DMEAN2
C               
              call iwrsec(2,array)
              isec=isec+1
              ipcout=ipcout+1
              i = (ixpclist(ipc) - minAllXpiece) / (nx - nxoverlap)
              ixpcout(ipcout) = i * (nxbin-nxbOverlap) +minAllXpiece/ibinning
              i = (iypclist(ipc) - minAllYpiece) / (ny - nyoverlap)
              iypcout(ipcout) = i * (nybin-nybOverlap) +minAllYpiece/ibinning
              izpcout(ipcout)=izpclist(ipc)
              if(ifrenumb.ne.0) izpcout(ipcout) = isecout-1
              if (ifshiftxy .ne. 0) then
                ixpcout(ipcout) = ixpcout(ipcout) - minSubXpiece
                iypcout(ipcout) = iypcout(ipcout) - minSubYpiece
              endif
              ifanyout=ifanyout+1
c               
c               transfer extra header bytes if present
c               
              if(nbsymout.ne.0.and.indxout.lt.nbsymout)then
                nbcopy=min(nbytexout,nbytexin,nbsymin)
                nbclear=nbytexout-nbcopy
                do i=1,nbcopy
                  indxout=indxout+1
                  extraout(indxout)=extrain((ipc-1)*nbytexin+i)
                enddo
                do i=1,nbclear
                  indxout=indxout+1
                  extraout(indxout)=0
                enddo
c               
c                 Need to replace values if there is renumbering, shifting,
c                 binning, or the piece coordinates actually came from pl file
                if((ifrenumb.ne.0 .or. ifshiftxy .ne. 0 .or. ibinning .gt. 1
     &              .or. pifilin(ifile) .ne. ' ')
     &              .and.nbcopy.ge.6)then
                  temp=isecout-1
                  ind=indxout+5-nbytexout
                  if(mod(iflagxin,2).ne.0)ind=ind+2
                  if (ifrenumb.ne.0 .or. pifilin(ifile) .ne. ' ')
     &                call move(extraout(ind),temp,2)
                  if (ifshiftxy .ne. 0 .or. ibinning .gt. 1 .or.
     &                pifilin(ifile) .ne. ' ') then
                    temp = ixpcout(ipcout)
                    call move(extraout(ind-4),temp,2)
                    temp = iypcout(ipcout)
                    call move(extraout(ind-2),temp,2)
                  endif
                endif
              endif
            endif
          enddo
          if(ifanyout.gt.0)isecout=isecout+1
c           see if need to close stack file
          if(isecout.gt.nsecout(ifileout) .or.
     &        (ifile.eq.nfilein.and.ilis.eq.nlist(ifile)))then
c             set new size, keep old nxyzst
            if(pifilout(ifileout).ne.' ')then
              call dopen(3,pifilout(ifileout),'new','f')
              write(3,'(3i6)')(ixpcout(i),iypcout(i),izpcout(i),i=1, ipcout)
              close(3)
            endif
            NXYZ2(3)=ipcout
            call ialsiz(2,nxyz2,nxyzst)
c             if mxyz=nxyz, keep this relationship
            if(mxyz(1).eq.nx.and.mxyz(2).eq.ny.and.mxyz(3).eq.nz)
     &          then
              MXYZ2(1)=NXbin
              MXYZ2(2)=NYbin
              MXYZ2(3)=ipcout
              call ialsam(2,mxyz2)
            endif
c               keep delta the same by scaling cell size from change in mxyz
            do i=1,3
              CELL2(i)=mxyz2(i)*(cell(i)/mxyz(i))
              if (i .lt. 3) cell2(i) = cell2(i) * ibinning
              CELL2(i+3)=90.
            enddo
            CALL IALCEL(2,CELL2)
c             
c             adjust origin if shifting piece coords to 0
            if (ifshiftXY .ne. 0)
     &          call ialorg(2, xorig - ibinning * minSubXpiece * delt(1),
     &          yorig - ibinning * minSubYpiece * delt(2), zorig)
            if(nbsymout.gt.0)call ialsym(2,nbsymout,extraout)
            DMEAN=DMEAN/ipcout
C             
            CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
            CALL IMCLOSE(2)
            ipcout=0
            isecout=1
            ifileout=ifileout+1
          endif
        enddo
        call imclose(1)
      enddo
C       
      call exit(0)
      END


c       Tests for whether a piece should be included given its coordinates
c       and the set of points to knock out
c
      logical function notknock(ixpc,iypc,izpc,nx,ny, nxoverlap,nyoverlap,ixko,
     &    iyko,izko, knocks)
      implicit none
      integer*4 ixko(*),iyko(*),izko(*),ixpc,iypc,izpc,nx,ny,knocks,i
      integer*4 nxoverlap,nyoverlap,nxbord, nybord
      notknock=.false.
      nxbord = min(nxoverlap, nx/4)
      nybord = min(nyoverlap, ny/4)
      do i=1,knocks
        if (izko(i).eq.izpc .and. ixpc+nxbord.le.ixko(i) .and.
     &      ixpc+nx-nxbord.gt.ixko(i) .and. iypc+nybord.le.iyko(i) .and.
     &      iypc+ny-nybord.gt.iyko(i)) return
      enddo
      notknock=.true.
      return
      end


c       Given a piece list file, read the piece list; given no piece file,
c       attempt to read piece coordinates from image header
c
      subroutine read_pl_or_header(pifilin,filin,extrain,maxextra,nbsymin,
     &    nbytexin, ixpclist,iypclist,izpclist,npclist,limpcl)
      implicit none
      character*(*) pifilin,filin
      integer*1 extrain(*)
      integer*4 ixpclist(*),iypclist(*),izpclist(*),maxextra,nbsymin,npclist
      integer*4 nxyz(3),mxyz(3),limpcl, mode, nbytexin,iflagxin
      real*4 DMININ,DMAXIN,DMEANIN
c       
      nbsymin = 0
      if(pifilin.ne.' ' .and. pifilin .ne. 'none')then
        call read_piece_list(pifilin, ixpclist,iypclist,izpclist,
     &      npclist)
      else
        call ialprt(.false.)
        call imopen(4,filin,'RO')
        CALL IRDHDR(4,NXYZ,MXYZ,MODE,DMININ,DMAXIN,DMEANIN)
c         
c         get extra header information if any
c         
        call irtnbsym(4,nbsymin)
        if(nbsymin.gt.0)then
          if(nbsymin.gt.maxextra)then
            write(*,'(/,a,a,a)')'ERROR: EDMONT - NO PIECE LIST FILE WAS'//
     &          ' GIVEN FOR INPUT FILE ',trim(filin),
     &          ' AND THE EXTRA HEADER DATA ARE TOO LARGE FOR THE ARRAY'
            call exit(1)
          else
            call irtsym(4,nbsymin,extrain)
            call irtsymtyp(4,nbytexin,iflagxin)
            call get_extra_header_pieces(extrain,nbsymin,nbytexin,
     &          iflagxin,nxyz(3),ixpclist,iypclist,izpclist,npclist,
     &          limpcl)
          endif
        endif
        if (nbsymin.eq.0.or.npclist.eq.0)then
          write(*,'(/,a,a,a)')'ERROR: EDMONT - NO PIECE LIST FILE WAS'//
     &        ' GIVEN FOR INPUT FILE ',trim(filin),
     &        ' AND THE HEADER DOES NOT CONTAIN PIECE COORDINATES'
          call exit(1)
        endif
      endif
      return
      end


c       Open an image file, get piece coordinates one way or another, analyze
c       the list of Z values in the file, and determine the montage
c       characteristics in each dimension
c
      subroutine openAndAnalyzeFiles(imfile, plfile, nxyz, dmin2, printing, 
     &    mode, extrain, maxextra, nbsymin, nbytexin, ixpclist,iypclist,
     &    izpclist, npclist, limpcl, listz,nlistz, minxpiece ,nxpieces,
     &    nxoverlap, minypiece , nypieces, nyoverlap)
      implicit none
      character*(*) imfile, plfile
      logical*4 printing
      integer*4 nxyz(3), mode, maxextra, nbsymin, ixpclist(*),iypclist(*)
      integer*4 izpclist(*),npclist,limpcl, listz(*),nlistz, minxpiece 
      integer*4 nxpieces, nxoverlap, minypiece, nypieces, nyoverlap
      integer*1 extrain(*)
      real*4 DMIN2,DMAX2,DMEAN2
      integer*4 MXYZ(3),nbytexin
C       
c       Read the piece data first in case there is a problem opening the
c       same file on two channels
      call read_pl_or_header(plfile,imfile,extrain, maxextra, nbsymin,
     &    nbytexin, ixpclist,iypclist,izpclist,npclist,limpcl)
      call ialprt(printing)
      CALL IMOPEN(1,imfile,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
      if (nbsymin .eq. 0) call irtnbsym(1, nbsymin)
      call fill_listz(izpclist,npclist,listz,nlistz)
      call checklist(ixpclist,npclist, 1, nxyz(1), minxpiece ,nxpieces,
     &    nxoverlap)
      call checklist(iypclist,npclist, 1, nxyz(2), minypiece ,nypieces,
     &    nyoverlap)
      return
      end

*       
*       Early history
*       David Mastronarde for VAX       5/9/89
*       4/19/90 Added line-by-line prompts for files and section entry, added
*       time/date stamp, fixed bugs on rescaling in place and missing secs
*       1999: added ability to knock out pieces.
*       1/3/00: made it handle extra header data, made scaling logic more
*       like NEWSTACK and made sure it could handle negative integers. 
*       10/24/00: made it actually use coordinates in header and renumber
*       sections sequentially.
*       

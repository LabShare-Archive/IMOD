*****   XFMODEL.FOR*********************************************
*       Will take a model from wimp or imod, and either
*       a) use corresponding points in two sections to obtain a transformation
*       between the sections, or
*       b) transform the points in the model to match a new alignment of images
c       
c       For more details, see man page
*       
c       $Id$
c       Log at end of file
c       
      implicit none
      include 'model.inc'
      integer nflimit,limpcl,idim,lmGrid
      parameter (nflimit=10000,limpcl=100000)
      real*4 f(2,3,nflimit),g(2,3,nflimit),gtmp(2,3)
      integer*4 nxyz(3),mxyz(3),nx,ny,nz,mode
      equivalence (nxyz(1),nx),(nxyz(2),ny),(nxyz(3),nz)
c       real*4 delt(3)
      integer*4 nsec(nflimit),listz(nflimit),indfl(nflimit)
      integer*4 numInChunks(nflimit), numChunks
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      parameter (idim=1000)
      parameter (lmGrid = 200)
      include 'statsize.inc'
      real*4 xr(msiz,idim)
      character*320 modelfile,newmodel,oldxfgfile,oldxffile,newxffile,idfFile
      character*320 magGradFile
      logical gotthis,gotlast,exist,readw_or_imod
      integer*4 getimodmaxes
      integer*4 limpnts/4/                      !min # of points for regression
c       
      integer*4 i,nlistz,nfout,npclist,izrange,iffillgap,indf,indval
      integer*4 minxpiece,nxpieces,nxoverlap,minypiece,nypieces,nyoverlap
      real*4 xhaf,yhaf,xcen,ycen,critmean,critmax,dmin,dmax,dmean
      integer*4 ifxfmod,iftrans,ifrotrans,ifprealign,ntofind, ifmagrot
      integer*4 ifsingle,izsingle,iffullrpt,ierr,ierr2,ifflip,indg
      real*4 zz,zdex,zthis,zlast, shiftScale
      integer*4 nundefine,iobj,ipt,iz,nfgin,indind,izmin,izmax,ibase
      integer*4 lastsec,npnts,iobject,ninobj,ipnt,isol,ipntmax,j
      real*4 const,rsq,fra,theta,sinth,costh,gmag,devmax
      real*4 xdev,ydev,devpnt,devavg,devsd,xx,yy,xlast,ylast,xnew,ynew
      integer*4 loop,noldg,izsec,il,indobj,maxx,maxy,maxz, ifBack, iter
      integer*4 lineUse, lineToUse, ifMagGrad
      logical done
      real*4 atan2d
c       
      integer*4 ifDistort, idfBinning, iBinning, idfNx, idfNy
      integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
      real*4 xGridIntrv, yGridIntrv, pixelIdf, binRatio, dx, dy, dx1, dy1
      real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
      character*10240 stringList
      real*4 pixelMagGrad, axisRot
      real*4 tiltAngles(nflimit), dmagPerUm(nflimit), rotPerUm(nflimit)
      integer*4 numMagGrad, lnblnk
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean, PipNumberOfEntries
      integer*4 PipGetString,PipGetTwoIntegers, PipGetFloatArray, PipGetFloat
      integer*4 PipGetIntegerArray, PipGetNonOptionArg, PipGetTwoFloats
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfmodel
c       
      integer numOptions
      parameter (numOptions = 24)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@image:ImageFile:FN:@'//
     &    'piece:PieceListFile:FN:@allz:AllZhaveTransforms:B:@'//
     &    'center:CenterInXandY:FP:@transonly:TranslationOnly:B:@'//
     &    'rottrans:RotationTranslation:B:@magrot:MagRotTrans:B:@'//
     &    'sections:SectionsToAnalyze:LI:@single:SingleSection:I:@'//
     &    'full:FullReportMeanAndMax:FP:@prealign:PrealignTransforms:FN:@'//
     &    'edit:EditTransforms:FN:@xforms:XformsToApply:FN:@'//
     &    'useline:UseTransformLine:I:@chunks:ChunkSizes:LI:@'//
     &    'back:BackTransform:B:@scale:ScaleShifts:F:@'//
     &    'distort:DistortionField:FN:@binning:BinningOfImages:I:@'//
     &    'gradient:GradientFile:FN:@param:ParameterFile:PF:@help:usage:B:'
c       
c       set defaults
c       
      modelfile = ' '
      xcen = 0.
      ycen = 0.
      ifBack = 0
      iBinning = 1
      ifDistort = 0
      ifMagGrad = 0
      numChunks = 0
      shiftScale = 1.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'xfmodel',
     &    'ERROR: XFMODEL - ', .true., 2, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

c       
c       get parameters
c       
      if (pipinput) then
        ierr = PipGetString('ImageFile', modelfile)
      else
        write(*,'(1x,a,$)')
     &      'Image file (or Return to enter Xcen, Ycen directly): '
        read(*,'(a)')modelfile
      endif
c       
c       if no image file, get crucial info directly
c       DNM 8/28/02: just get xcen, ycen since origin and delta aren't needed
c       
      if(modelfile.eq.' ')then
        if (pipinput) then
          ierr = PipGetTwoFloats('CenterInXandY', xcen, ycen)
        else
          print *,char(7),
     &        'Be SURE to enter CENTER coordinates, NOT NX and NY'
          write(*,'(1x,a,$)')'Xcen, Ycen: '
          read(*,*)xcen,ycen
        endif
        do i=1,nflimit
          listz(i)=i-1
        enddo
        nlistz=nflimit
        nfout=0
      else
c         
c         otherwise get header info from image file
        call imopen(1,modelfile,'ro')
c         
c         get header info for proper coordinate usage
        call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
c         call irtdel(1,delt)
c         call irtorg(1,xorig,yorig,zorig)
c         write(*,'(/,a,a,/)')' This header info MUST be the'
c         &           ,' same as when model was built'
c         
        modelfile = ' '
        if (pipinput) then
          ierr = PipGetString('PieceListFile', modelfile)
        else
          write(*,'(1x,a,$)') 'Piece list file if image is a'//
     &        ' montage, otherwise Return: '
          read(*,'(a)')modelfile
        endif
        call read_piece_list(modelfile,ixpclist,iypclist,izpclist,
     &      npclist)
        if (npclist.gt.limpcl)call exitError(
     &      'too many piece coordinates for arrays')
c         
c         if no pieces, set up mocklist
        if(npclist.eq.0)then
          do i=1,nz
            ixpclist(i)=0
            iypclist(i)=0
            izpclist(i)=i-1
          enddo
          npclist=nz
        endif
c         get ordered list of z values 
        call fill_listz(izpclist,npclist,listz,nlistz)
        if (nlistz.gt.nflimit)call exitError(
     &      'too many Z values for arrays')
        
        call checklist(ixpclist,npclist,1,nx,minxpiece
     &      ,nxpieces,nxoverlap)
        call checklist(iypclist,npclist,1,ny,minypiece
     &      ,nypieces,nyoverlap)
        xhaf=(nx+(nxpieces-1)*(nx-nxoverlap))/2.
        yhaf=(ny+(nypieces-1)*(ny-nyoverlap))/2.

c         DNM 8/28/02: don't scale any more
c         but still add minxpiece - the index coordinates are montage
c         coordinates starting at minxpiece.
c         
c         [xy]cen is the amount to SUBTRACT from real model coordinates to
c         get coordinates centered on the center of the image.  Adding [xy]
c         orig shifts coordinates to the lower left corner of image.
c         Subtracting 0.5*nx*delt(1) (half of the image width in real
c         coordinates) then shifts coords to center of image.  Hence this:
c         
c         xcen=(minxpiece+xhaf)*delt(1)-xorig
c         ycen=(minypiece+yhaf)*delt(2)-yorig
        xcen=(minxpiece+xhaf)
        ycen=(minypiece+yhaf)
        nfout=nlistz

      endif
c       
c       find out if gaps in z and ask how to store f's if there are gaps
c       
      izrange=listz(nlistz)+1-listz(1)
      iffillgap=0
      if(izrange.gt.nlistz)then
        write(*,'(1x,a,i4,a)')'There are',izrange-nlistz,
     &      ' gaps in section Z values'
        if (pipinput) then
          ierr = PipGetBoolean('AllZhaveTransforms', iffillgap)
          if (iffillgap .eq. 0) then
            print *,'Xform lists will be assumed to have xforms ',
     &          'only for existing sections'
          else
            print *,'Xform lists will be assumed to have xforms ',
     &          'for all sections'
          endif
        else
          write(*,'(1x,a,/,a$)')'Enter 0 for xform lists'//
     &        ' that have xforms only for existing sections,',
     &        '    or 1 for xform lists that have xforms for'//
     &        ' all Z values in range: '
          read(*,*)iffillgap
        endif
      endif
c       
c       make index from z values to transform list: index=0 for non-existent
c       
      if (pipinput .and. PipGetString('ChunkSizes', stringList) .eq. 0)
     &    call parselist(stringList, numInChunks, numChunks)
      do i=1,nflimit
        indfl(i)=0
      enddo
      if (numChunks .eq. 0) then
        do i=1,nlistz
          indf=listz(i)+1-listz(1)
          indval=i
          if(iffillgap.ne.0)indval=indf
          indfl(indf)=indval
        enddo
        if(nfout.gt.0)nfout=indfl(listz(nlistz)+1-listz(1))
      else
c         
c         or fill index list 
c         
c        print *,numChunks,(numInChunks(j),j=1,numChunks)
        indf = 1
        do j = 1, numChunks
          do i = 1, numInChunks(j)
            indfl(indf) = j
            indf = indf + 1
            if (indf .ge. nflimit) call exitError(
     &          'TOO MANY SECTIONS IN CHUNKS FOR ARRAYS')
          enddo
        enddo
      endif
c       print *,(indfl(i), i=1,nlistz)
c       
      ifxfmod = 0
      ifprealign = 0
      if (PipGetInOutFile('InputFile', 1, 'Input model file', modelfile)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      oldxfgfile = ' '
      oldxffile = ' '
      idfFile = ' '
      magGradFile = ' '
      iftrans=0
      ifrotrans=0
      ifmagrot = 0
      lineUse = -1
      if (pipinput) then
        ierr = PipGetBoolean('TranslationOnly', iftrans)
        ierr = PipGetBoolean('RotationTranslation', ifrotrans)
        ierr = PipGetBoolean('MagRotTrans', ifmagrot)
        ierr = PipGetInteger('UseTransformLine', lineUse)
        ierr = PipGetFloat('ScaleShifts', shiftScale)

        if (PipGetString('XformsToApply', oldxffile) .eq. 0) ifxfmod = 1
        if (PipGetString('DistortionField', idfFile) .eq. 0) ifxfmod = 1
        if (PipGetString('GradientFile', magGradFile) .eq. 0) ifxfmod = 1
        if (PipGetString('PrealignTransforms', oldxfgfile) .eq. 0)
     &      ifprealign = 1
c         
c         if back-transform, first check for legality
c         
        if (PipGetBoolean('BackTransform', ifBack) .eq. 0) then
          if (oldxffile .ne. ' ' .and. oldxfgfile .ne. ' ') call exitError(
     &        'You cannot enter both -xform and -prealign with -back')
          if (ifxfmod + ifprealign .eq. 0)
     &        call exitError('You must enter -xform, -prealign,'//
     &        ' -distort or -gradient with -back')
c           
c           in any case, set filename for use in back-transform, clear out
c           transform filename, set flag for prealign back-transform if any
c           Set xfmodel -1 if xform is givem, or if preali with no
c           distortions; i.e. preali with distortion will retransform forward
c           to original (distorted) aligned stack
c           
          if (oldxffile .ne. ' ' .or. (idfFile .eq. ' ' .and.
     &        magGradFile .eq. ' ')) ifxfmod = -1
          if (oldxffile .ne. ' ') oldxfgfile = oldxffile
          oldxffile = ' '
          if (oldxfgfile .ne. ' ') ifprealign = 1
        endif
c         
        if (iftrans + ifrotrans + ifmagrot .gt. 1) call exitError
     &      ('Only one of -trans, -rottrans, -magrot may be entered')
        if (iftrans + ifrotrans + ifmagrot .gt. 0 .and. ifxfmod .ne. 0)
     &      call exitError('You cannot both find transforms '//
     &      'and transform a model')
        if (ifxfmod .eq. 0 .and. shiftScale .ne. 1.) call exitError
     &      ('You cannot enter -scale unless you are transforming a model')
        if (iftrans .ne. 0) ifxfmod = 2
        if (ifrotrans .ne. 0) ifxfmod = 3
        if (ifmagrot .ne. 0) ifxfmod = 4

        if (.not.(ifxfmod .eq. -1 .or. (ifxfmod .eq. 1 .and. 
     &      (oldxffile .ne. ' ' .or. oldxfgfile .ne. ' '))))  then
          if (lineUse .ge. 0) call exitError('You cannot enter -useline'//
     &        ' unless you are transforming a model')
          if (numChunks .gt. 0) call exitError('You cannot enter -chunks'//
     &        ' unless you are transforming a model')
        endif
        if (lineuse .ge. 0 .and. numChunks .gt. 0) call exitError('It is '//
     &      'meaningless to enter both -useline and -chunks')
      else
c         
        write(*,'(1x,a,/,a,/,a/,a,$)') 'Enter 0 to find '//
     &      'transformations, 2 to find X/Y translations only,',
     &      '    3 to find translations and rotations only,',
     &      '    4 to find translation, rotation and mag change only,',
     &      '    1 to transform model, or -1 to back-transform model: '
        read(*,*)ifxfmod
      endif
c       
c       set variables for restricted fits
c       
      if(ifxfmod.eq.2)then
        ifxfmod=0
        iftrans=1
        limpnts=1
      elseif(ifxfmod.eq.3)then
        ifrotrans=1
        ifxfmod=0
        limpnts=2
      elseif(ifxfmod.eq.4)then
        ifrotrans=2
        ifxfmod=0
        limpnts=3
      endif
c       
c       
      ifsingle=0
      iffullrpt = 0
      ntofind=0
      if (pipinput) then
        if (PipGetInOutFile('OutputFile', 2, ' ', newxffile) .ne. 0)
     &      call exitError('NO OUTPUT FILE SPECIFIED')
        if(ifxfmod.eq.0)then
          oldxffile = ' '
          ierr = PipGetString('EditTransforms', oldxffile)

          if (PipGetTwoFloats('FullReportMeanAndMax', critmean, critmax)
     &        .eq. 0) iffullrpt = 1
          if (PipGetInteger('SingleSection', izsingle) .eq. 0) ifsingle = 1
          if (PipGetString('SectionsToAnalyze', stringList) .eq. 0)
     &        call parselist(stringList, nsec, ntofind)
        else
          newmodel = newxffile
          if (idfFile .ne. ' ') then
            ifDistort = 1
            call readDistortions(idfFile, fieldDx, fieldDy, lmGrid, idfNx,
     &          idfNy, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &          iyGridStrt, yGridIntrv, nyGrid)
c             
c             if the center is not yet defined, need to get it now
c             
            if (xcen .eq. 0. .and. ycen .eq. 0.) then
              exist=readw_or_imod(modelfile)
              if(.not.exist)go to 91
              ierr = getimodmaxes(maxx, maxy, maxz)
              xcen = maxx / 2.
              ycen = maxy / 2.
              write(*,'(a,2f8.1)')'Using model header to determine '//
     &            'center coordinates:', xcen, ycen
            endif
c             
c             insist on binning unless situation is unambiguous, and convert
c             the distortion field by the difference in binning
c             
            if (PipGetInteger('BinningOfImages', iBinning) .ne. 0) then
              
              if (2. * xcen .le. idfNx * idfBinning / 2 .and.
     &            2. * ycen .le. idfNy * idfBinning / 2) call exitError
     &            ('YOU MUST SPECIFY BINNING OF IMAGES BECAUSE THEY '//
     &            'ARE NOT LARGER THAN HALF THE CAMERA SIZE')
            endif
            if (iBinning .le. 0) call exitError
     &          ('IMAGE BINNING MUST BE A POSITIVE NUMBER')
            binRatio = 1.
            if (iBinning .ne. idfBinning) then
              binRatio = idfBinning / float(iBinning)
              ixGridStrt = nint(ixGridStrt * binRatio)
              iyGridStrt = nint(iyGridStrt * binRatio)
              xGridIntrv = xGridIntrv * binRatio
              yGridIntrv = yGridIntrv * binRatio
              do j = 1, nyGrid
                do i = 1, nxGrid
                  fieldDx(i, j) = fieldDx(i, j) * binRatio
                  fieldDy(i, j) = fieldDy(i, j) * binRatio
                enddo
              enddo
            endif
c             
c             Need to shift field by difference between image and camera
c             centers
c             
            ixGridStrt = ixGridStrt - nint(idfNx * binRatio / 2. - xcen)
            iyGridStrt = iyGridStrt - nint(idfNy * binRatio / 2. - ycen)
c             print *,ixGridStrt,ixGridStrt,xcen,ycen,idfnx,idfny,binratio
          endif
        endif
c         
c         mag gradients now
c         
        if (magGradFile .ne. ' ') then
          ifMagGrad = 1
          call readMagGradients(magGradFile, nflimit, pixelMagGrad, axisRot,
     &        tiltAngles, dmagPerUm, rotPerUm, numMagGrad)
        endif
        
      else
c         
c         old input: get prealignment or back transforms
c         
        if(ifxfmod.lt.0)then
          ifprealign=1
        else
          write(*,'(1x,a,$)')
     &        'Model built on raw sections (0) or prealigned ones(1): '
          read(*,*)ifprealign
        endif
c         
        if(ifprealign.ne.0)then
          write(*,'(1x,a,$)')
     &        'File of old g transforms used in prealignment: '
          read(*,'(a)')oldxfgfile
        endif
c         
        if(ifxfmod.eq.0)then
          write(*,'(1x,a,$)') 'Name of old file of f transforms'//
     &        ' to edit (Return if none): '
          read(*,'(a)')oldxffile
c           
          write(*,'(1x,a,$)')'Name of new file of f transforms: '
          read(*,'(a)')newxffile
c           
          write(*,118)
118       format(' Enter / to find transforms for all section pairs,',/,
     &        '      or -999 to find transforms relative to a single',
     &        ' section,',/,
     &        '      or a list of the section numbers to find',
     &        ' transforms for',/,'         (enter number of second',
     &        ' section of each pair, ranges are ok)')
          call rdlist(5,nsec,ntofind)
          if(ntofind.eq.1.and.nsec(1).eq.-999)then
            write(*,'(1x,a,$)')'Number of single section to find'//
     &          ' transforms relative to: '
            read(*,*)izsingle
            ifsingle=1
            print *,'Now enter list of sections to find transforms',
     &          ' for (/ for all)'
            ntofind=0
            call rdlist(5,nsec,ntofind)
          endif
c           
          write(*,'(1x,a,$)')'1 for full reports of deviations for'//
     &        ' sections with bad fits, 0 for none: '
          read(*,*)iffullrpt
c           
          if(iffullrpt.ne.0)then
            write(*,'(1x,a,$)')'Enter criterion mean deviation and'//
     &          ' max deviation; full reports will be given',
     &          '     for sections with mean OR max greater than'//
     &          ' these criteria: '
            read(*,*)critmean,critmax
          endif
c           
        else
          write(*,'(1x,a,$)')'New model file name: '
          read(*,'(a)')newmodel
c           
          if(ifxfmod.gt.0)then
            write(*,'(1x,a,$)')
     &          'File for list of g transforms to apply: '
            read(*,'(a)')oldxffile
          endif
c           
        endif
      endif
      call PipDone()
c       
c       read in the model
c       
      exist=readw_or_imod(modelfile)
      if(.not.exist)go to 91
c       
c       
c       if the center is not yet defined, use the model header sizes
c       
      if (xcen .eq. 0. .and. ycen .eq. 0.) then
        ierr = getimodmaxes(maxx, maxy, maxz)
        xcen = maxx / 2.
        ycen = maxy / 2.
        write(*,'(a,2f8.1)')'Using model header to determine '//
     &      'center coordinates:', xcen, ycen
      endif
c       
c       shift the data to image coordinates before using
c       
      call scale_model(0)
c       
c       first fill array with unit transforms in case things get weird
c       
      do i=1,nflimit
        call xfunit(f(1,1,i),1.)
      enddo
c       
c       back-transform if necessary
      if(ifprealign.ne.0)then
        call dopen(3,oldxfgfile,'ro','f')
c         
c         get g transforms into g list
        call xfrdall(3,g,noldg,*92)
        if (noldg.gt.nflimit)call exitError('too many transforms for arrays')
        close(3)
c         
c         invert the g's into the g list
        do indg=1,noldg
          g(1,3,indg) = g(1,3,indg) * shiftScale
          g(2,3,indg) = g(2,3,indg) * shiftScale
          call xfinvert(g(1,1,indg),gtmp)
          call xfcopy(gtmp,g(1,1,indg))
        enddo
c         
c         apply inverse g's to all points in model
c         Set up to use a single section if back transforming and user 
c         specified it or there is only one transform
c         
        lineToUse = -1
        if (ifxfmod .lt. 0) then
          lineToUse = lineUse
          if (noldg .eq. 1 .and. numChunks .eq. 0) then
            lineToUse = 0
            print *,'There is only one transform and it is being',
     &          ' applied at all Z values'
          endif
        endif
        print *,'Back-transforming model with inverse of XGs from ',
     &      oldxfgfile(1:lnblnk(oldxfgfile))
        call transformModel(g, noldg, nflimit, xcen, ycen, indfl,
     &      listz, lineToUse, nundefine)
        
        if(ifxfmod.lt.0 .and. ifDistort + ifMagGrad .eq. 0)then
c           
c           write out back-transformed model
          call rescaleWriteModel(newmodel, nundefine)
          call exit(0)
        endif
      endif
c       
c       read in the old f's or g's for whatever purpose 
c       
      nfgin=0
      lineToUse = -1
      if(oldxffile.ne.' ')then
        call dopen(1,oldxffile,'ro','f')
        call xfrdall(1,f,nfgin,*92)
        if (nfgin.gt.nflimit)call exitError('too many transforms for arrays')
        close(1)
        do indg=1,nfgin
          f(1,3,indg) = f(1,3,indg) * shiftScale
          f(2,3,indg) = f(2,3,indg) * shiftScale
        enddo
        lineToUse = lineUse
        if (nfgin .eq. 1 .and. numChunks .eq. 0) then
          lineToUse = 0
          print *,'There is only one transform and it is being',
     &        ' applied at all Z values'
        endif
      endif
c       
c       TRANSFORMING/UNDISTORTING MODEL 
c       
      if(ifxfmod.ne.0)then
        if (ifDistort + ifMagGrad .ne. 0) then
          if (ifBack .ne. 0) then
            print *,'Redistorting model'
          else
            print *,'Undistorting model'
          endif

          do i=1,n_point
            if (ifMagGrad .ne. 0) 
     &          iz = max(1, min(nint(p_coord(3, i) + 1.), numMagGrad))

            if (ifBack .eq. 0) then
c               
c               undistort the model - find point that distorts to the 
c               given model point
c               
              iter = 1
              xlast = p_coord(1,i)
              ylast = p_coord(2,i)
              done = .false.
              do while (iter .lt. 10 .and. .not.done)
                dx1 = 0.
                dy1 = 0.
                dx = 0.
                dy = 0.
                if (IfMagGrad .ne. 0)
     &              call magGradientShift(xlast, ylast, nint(2. * xcen),
     &              nint(2. * ycen),
     &              xcen, ycen, pixelMagGrad, axisRot, tiltAngles(iz),
     &              dmagPerUm(iz), rotPerUm(iz), dx1, dy1)

                if (ifDistort .ne. 0)
     &              call interpolateGrid(xlast + dx1, ylast + dy1, fieldDx,
     &              fieldDy, lmGrid, idfNx, idfNy, ixGridstrt, xGridIntrv,
     &              iyGridStrt, yGridIntrv, dx, dy)
                xnew = p_coord(1,i) - (dx + dx1)
                ynew = p_coord(2,i) - (dy + dy1)
                done = abs(xnew - xlast) .lt. 0.01 .and.
     &              abs(ynew - ylast) .lt. 0.01
                xlast = xnew
                ylast = ynew
                iter = iter + 1
              enddo     
              p_coord(1,i) = xnew
              p_coord(2,i) = ynew
            else
c               
c               or redistort the model
c               
              dx1 = 0.
              dy1 = 0.
              dx = 0.
              dy = 0.
              if (IfMagGrad .ne. 0)
     &            call magGradientShift(p_coord(1,i), p_coord(2,i),
     &            nint(2. * xcen), nint(2. * ycen),
     &            xcen, ycen, pixelMagGrad, axisRot, tiltAngles(iz),
     &            dmagPerUm(iz), rotPerUm(iz), dx1, dy1)

              if (ifDistort .ne. 0)
     &            call interpolateGrid(p_coord(1,i) + dx1,
     &            p_coord(2,i) + dy1,
     &            fieldDx, fieldDy, lmGrid, idfNx, idfNy, ixGridstrt,
     &            xGridIntrv, iyGridStrt, yGridIntrv, dx, dy)
              p_coord(1,i) = p_coord(1,i) + dx1 + dx
              p_coord(2,i) = p_coord(2,i) + dy1 + dy
            endif
          enddo
c           
c           if there was prealignment and no new transforms, get the 
c           prealignment transforms back by inversion and set up to use them
c           
          if (nfgin .ne. 0) then
            print *,'Transforming model with XGs from ',
     &          oldxffile(1:lnblnk(oldxffile))
          else if (nfgin .eq. 0 .and. ifprealign .ne. 0 .and.
     &          ifxfmod .gt. 0) then
            print *,'Transforming model with reinverted XGs from ',
     &          oldxfgfile(1:lnblnk(oldxfgfile))
            do indg=1,noldg
              call xfinvert(g(1,1,indg), f(1,1,indg))
            enddo
            nfgin = noldg
          endif
        endif
c         
        nundefine = 0
c         
c         transform the model
c         
        if (nfgin .ne. 0) call transformModel(f, nfgin, nflimit,
     &      xcen, ycen, indfl, listz, lineToUse, nundefine)
        
        call rescaleWriteModel(newmodel, nundefine)
      else
c         
c         SEARCH FOR POINTS TO DERIVE XFORMS FROM
c         
c         first find min and max z in model
        izmin=100000
        izmax=-izmin
        do iobj=1,max_mod_obj
          ibase=ibase_obj(iobj)
          do ipt=1,npt_in_obj(iobj)
            i=abs(object(ipt+ibase))
            zz=p_coord(3,i)
c             zdex=(zz+zorig)/delt(3)
            zdex=zz
            iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
c             if(iz.lt.listz(1).or.iz.gt.listz(nlistz))go to 93
            izmin=min0(izmin,iz)
            izmax=max0(izmax,iz)
          enddo           
        enddo
c         
c         if didn't specify list of section #'s, make such a list from range
c         
        if(ntofind.le.0)then
          ntofind=ifsingle+izmax-izmin
          do i=1,ntofind
            nsec(i)=izmin+i-ifsingle
          enddo
        endif
        nfout=max(nfout,nfgin)
        write(*,122)
122     format(40x,'Deviations between transformed points on',/,
     &      41x,'section and points on previous section',/,
     &      32x,'Mean     Max  @object & point #   X-Y Position')
        do loop=1,ntofind
          izsec=nsec(loop)
c           
c           make sure this is a section in list and find previous z
c           
          lastsec=-100000
          do il=2,nlistz
            if(izsec.eq.listz(il))lastsec=listz(il-1)
          enddo
c           
c           but if doing to single section, allow section to be first in list
c           as well and set lastsec to z of single section
c           
          if(ifsingle.ne.0 .and. (lastsec.ne.-100000 .or.
     &        izsec.eq.listz(1)))lastsec=izsingle
          if(lastsec.ne.-100000)then
c             
c             get points out of objects with points in both this and previous
c             section
c             
            npnts=0
            do iobject=1,max_mod_obj
              gotlast=.false.
              gotthis=.false.
              ninobj=npt_in_obj(iobject)
              do indobj=1,ninobj
                ipnt=object(indobj+ibase_obj(iobject))
                if(ipnt.gt.0.and.ipnt.le.n_point)then
c                   zdex=(p_coord(3,ipnt)+zorig)/delt(3)
                  zdex=p_coord(3,ipnt)
                  iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
                  if((iz.eq.izsec).and.(.not.gotthis.or.
     &                (p_coord(3,ipnt).lt.zthis)))then
c                     
c                     if in second section, and either haven't gotten a point
c                     there before, or this point has a lower z than the
c                     previous point, put x and y into 1st and 2nd
c                     column: independent vars
c                     
                    xr(1,npnts+1)=p_coord(1,ipnt)-xcen
                    xr(2,npnts+1)=p_coord(2,ipnt)-ycen
                    gotthis=.true.
                    zthis=p_coord(3,ipnt)
                    xr(6,npnts+1)=iobject
                    xr(7,npnts+1)=indobj
                  elseif((iz.eq.lastsec).and.(.not.gotlast.or.
     &                  (p_coord(3,ipnt).gt.zlast)))then
c                     
c                     if in first section, and either haven't gotten a point
c                     there before, or this point is higher in z than the
c                     previous point, put x and y into 4th and 5th
c                     column: dependent vars
c                     
                    xr(4,npnts+1)=p_coord(1,ipnt)-xcen
                    xr(5,npnts+1)=p_coord(2,ipnt)-ycen
                    gotlast=.true.
                  endif
                endif
              enddo
              if(gotthis.and.gotlast)npnts=npnts+1
              if (npnts.ge.idim)then
                print *
                print *,'ERROR: XFMODEL - ',
     &              'too many points for arrays on section', iz
                call exit(1)
              endif
            enddo                               !done with looking at objects
            if(npnts.ge.limpnts)then
c               
c               now if there are at least limpnts points, do regressions:
c               first last section x then l.s. y as function of this section
c               x and y
c               save results in xform for izsec
c               
              indf=indfl(izsec+1-listz(1))
              if(indf.le.0)then
                print *
                print *,'ERROR: XFMODEL - z value out of range for ',
     &              'transforms: ',zz
                call exit(1)
              endif
c               
              call findxf(xr,npnts,xcen,ycen,iftrans,ifrotrans,2,f(1,1,indf),
     &            devavg, devsd,devmax,ipntmax)
C               
c               keep track of the highest & lowest transforms obtained
c               
              nfout=max0(nfout,indf)
              write(*,121)npnts,izsec,devavg,devmax,
     &            nint(xr(6,ipntmax)),nint(xr(7,ipntmax)),
     &            xr(8,ipntmax),xr(9,ipntmax)
121           format(i4,
     &            ' points, section #',i4,2x,2f9.2,2i6,4x,2f9.2)
              if(iffullrpt.ne.0.and.
     &            (devmax.ge.critmax.or.devavg.gt.critmean))
     &            write(*,124)((xr(i,j),i=6,13),j=1,npnts)
124           format('    Object  Point       position        ',
     &            'deviation vector   angle   magnitude',/,
     &            (f10.0,f6.0,4f10.2,f9.0,f10.2))
            else
              print *,'less than',limpnts,' points for section # ',
     &            izsec
            endif
          endif
        enddo                                   !end of loop the loop
c         
c         write out enough for whole file, and at least as many as were in
c         an input file if any
c         
        call dopen(2,newxffile,'new','f')
        do i=1,nfout
          call xfwrite(2,f(1,1,i),*94)
        enddo
        close(2)
      endif
      call exit(0)
91    call exitError('reading model file')
92    call exitError('reading old f/g file')
94    call exitError('writing out f file')
      end


      subroutine transformModel(f, nfgin, nflimit, xcen, ycen,
     &    indfl, listz, lineToUse, nundefine)
      implicit none
      include 'model.inc'
      real*4 f(2,3,*), xcen, ycen
      integer*4 nfgin, nflimit, nundefine, indfl(*), listz(*), lineToUse
      integer*4 iobj, ipt, i, iz, indind, indg
      real*4 zdex,zz
c       
      nundefine=0
      do iobj=1,max_mod_obj
        do ipt=1,npt_in_obj(iobj)
          i=abs(object(ibase_obj(iobj)+ipt))
          zz=p_coord(3,i)
c           zdex=(zz+zorig)/delt(3)
          zdex=zz
          iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
          indind=iz+1 -listz(1)
          if ((indind.lt.1.or.indind.gt.nflimit) .and. lineToUse .lt. 0)then
            nundefine=nundefine+1
          else
            if (lineToUse .lt. 0) then
              indg=indfl(indind)
            else
              indg = lineToUse + 1
            endif
            if(indg.lt.1.or.indg.gt.nfgin)then
              nundefine=nundefine+1
            else
              call xfapply(f(1,1,indg),xcen,ycen,p_coord(1,i),
     &            p_coord(2,i),p_coord(1,i),p_coord(2,i))
            endif
          endif
        enddo
      enddo
      return
      end

      subroutine rescaleWriteModel(newmodel, nundefine)
      implicit none
      include 'model.inc'
      integer*4 nundefine
      character*(*) newmodel
      
c       
      if(nundefine.gt.0)print *,nundefine,
     &    ' points with Z values out of range of transforms'
c       
c       write model out
c       shift the data back for saving
c       
      call scale_model(1)
      call write_wmod(newmodel)
      return
      end
c       
c       
c       $Log$
c       Revision 3.15  2008/11/21 22:05:24  mast
c       Increased filename size
c
c       Revision 3.14  2007/02/01 05:12:53  mast
c       Added option to specify scaling of shifts in transforms
c
c       Revision 3.13  2006/02/27 16:54:30  mast
c       Removed debug output of chunks
c
c       Revision 3.12  2005/12/09 04:43:27  mast
c       gfortran: .xor., continuation, format tab continuation or byte fixes
c
c       Revision 3.11  2005/10/24 18:32:33  mast
c       Added option to use one transform per chunk of specified size
c       
c       Revision 3.10  2004/06/21 23:06:17  mast
c       Removed code calling multr/correl and called library function findxf
c       
c       Revision 3.9  2004/03/22 15:33:30  mast
c       Had to declare lnblnk
c       
c       Revision 3.8  2004/03/22 05:37:17  mast
c       Added mag gradient corrections, resolved some problems with
c       back-transforming with distortion corrections.
c       
c       Revision 3.7  2004/01/27 05:37:22  mast
c       Left a ; on a line
c       
c       Revision 3.6  2004/01/20 00:07:37  mast
c       Added option to apply a single transform, fixed initialization and a
c       problem with back-transforming with -xf
c       
c       Revision 3.5  2004/01/16 18:08:04  mast
c       Fixed problem with how it decided if it needed image binning entry
c       
c       Revision 3.4  2003/12/27 19:42:45  mast
c       Work out some problems, finalized documentation and interface
c       
c       Revision 3.3  2003/12/12 20:37:52  mast
c       Preliminary checkin with PIP conversion and distortion correction
c       
c       Revision 3.2  2003/10/26 15:31:48  mast
c       switch from long prints to formatted writes for Intel compiler
c       
c       Revision 3.1  2002/09/05 05:36:45  mast
c       Changes to take scaling information from model header and to scale
c       coordinates properly to index coordinates.  Also put in error
c       checks, standardize error outputs and made declarations for implicit
c       none
c       
*       David Mastronarde 1988
c       DNM 7/20/89  changes for new model format
c       DNM 1/10/90  have it transform only existing model points, not all
c       points in p_coord array, to avoid bad Z values
c       DNM 5/28/90  fix bug in rounding 0.5 values, implement ability to
c       transform relative to a single section.
c       DNM 3/31/92  Implement translation only finding
c       DNM 5/1/92   Implement translation and rotation only finding
c       DNM 4/24/95 changed model reading/writing to be portable
c       DNM 9/23/97  Add translation, rotation, mag only finding
c       

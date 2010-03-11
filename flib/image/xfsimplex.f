******************XFSIMPLEX*******************************************
*       
*       This program searches for the best general linear transform between
*       a pair of images by varying either the six formal parameters of the
*       transform, the six "semi-natural" parameters underlying such a
c       transform, or restricted subsets of those semi-natural parameters.
*       
c       See man page for details
c
*       David Mastronarde 4/5/91 (adapted from XFSEARCH)
*       
************************************************************************
c
c       $Id$
c       Log at end of file
*       
      module xfsimplex
      implicit none
      integer idima, idimb, isub,limspir,isubp1,isubt2,isubt25
      parameter (idima=8200*8200, idimb = 4100*4100)
      parameter (isub=idimb/3,limspir=1000)
      parameter (isubp1=isub+1, isubt2=2*isub+1, isubt25=2.5*isub+2)
      integer*4 NX,NY,NZ, NXYZ(3)
      EQUIVALENCE (NX,NXYZ(1)), (NY,NXYZ(2)), (NZ,NXYZ(3))
c
c       array for second image if doing diffs, using same storage as
c       all the arrays for doing distances
      real*4, allocatable :: array(:)
      real*4 brray(idimb)
      integer*2 ixcomp(isub),iycomp(isub)
      real*4 denlo(isub),denhi(isub)
      equivalence (brray(1),denlo(1)),(brray(isubp1),denhi(1))
     &    ,(brray(isubt2),ixcomp(1)),(brray(isubt25),iycomp(1))
      integer*4 nx1,nx2,ny1,ny2, interp,natural,ncompare,nspiral,ifdist,iftrace
      real*4 rds, deltmin,sd1,distspir(limspir),acall(6),alimits(2,6)
      integer*4 ntrial, ivend,ifccc,idxspir(limspir),idyspir(limspir)
      real*8, allocatable :: sxa(:,:),sya(:,:),sxsqa(:,:),sysqa(:,:),sxya(:,:) 
      integer*4, allocatable :: npixa(:,:)
      integer*4 numXpatch, numYpatch, nxyPatch
      end module xfsimplex

c       MAIN PROGRAM
c
      use xfsimplex
      implicit none
      integer lenTemp
      parameter (lenTemp = 32000*64)
      integer*4 MXYZ(3),NXYZST(3), nxyz2(3)
C       
      CHARACTER*320 FILIN1,FILIN2,DATOUT,xfinfile
C       
      real*4 temp(lenTemp)
      data NXYZST/0,0,0/
      real*4 a(6),da(6),amat(2,2),anat(6),danat(6)
      real*4 pp(7,7),yy(7),ptmp(6),ptol(6), ctf(8193)
c       if doing formal params, the a(i) are DX, DY, a11,a12,a21, and a22
c       for natural paramas, the a(i) are DX and DY, Global rotation, global
c       stretch, difference between Y&X-axis stretch, and difference
c       between Y and X axis rotation.
c       the da are the step sizes for the respective a's
      logical trace
c       starting values of a, and da, for formal search
      data a/0.,0.,1.,0.,0.,1./
      data da/1.,1.,.025,.025,.025,.025/
c       starting values of a and da for natural search
      data anat/0.,0.,0.,1.,0.,0./
      data danat/1.,1.,2.,.02,.02,2./
      real*8 tsum, tsumsq
c       
      integer*4 ihist(0:1000)
      real*4 range(10,2),pmlim(6),ranlo(10),ranhi(10),percen(10,2),
     &    pclo(10),pchi(10)
      equivalence (ranlo(1),range(1,1)),(ranhi(1),range(1,2))
      equivalence (pclo(1),percen(1,1)),(pchi(1),percen(1,2))
      external func
C       
c       default values for potentially input parameters
      real*4 ftol1/5.e-4/,ptol1/.02/,ftol2/5.e-3/,ptol2/.2/
      real*4 delfac/2/
      integer*4 iflmean/1/,ibinning/2/,nrange/2/,idredun/0/
      real*4 radius/4/,difflim/0.05/
c       
      integer*4 mode,ierr,ivst,ii,jj,i,numLimit
      integer*4 npixel,ibase,mattx, matty,ixy,ind,iran,lohi
      real*4 fracmatt,DMIN,DMAX,DMEAN,pcrange,DMIN2,DMAX2,DMEAN2,sd2
      real*4 histscal,val,ix,iy,ixm,iym,ixp,ic,ixcm,iycm,DMIN1,DMAX1,DMEAN1
      integer*4 ncrit,limdxy,idx,idy,j,itmp,iter,jmin,izref,izali, npad, nxpad
      integer*4 nypad, nxorig, nyorig, ifSobel, ifFiltAfter, lineuse
      integer*4 ifXminmax, ifYminmax
      real*4 scale,dadd,window,valscl,dstnc,DELMIN,ptfac,tmp
      real*4 sigma1,sigma2,radius1,radius2, deltac
      integer*4 niceframe
c
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetFloat,PipGetTwoFloats
      integer*4 PipGetInOutFile,PipGetBoolean,PipGetFloatArray
      integer*4 PipGetTwoIntegers
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfsimplex
c       
      integer numOptions
      parameter (numOptions = 32)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'aimage:AImageFile:FN:@bimage:BImageFile:FN:@'//
     &    'output:OutputFile:FN:@initial:InitialTransformFile:FN:@'//
     &    'useline:UseTransformLine:I:@sections:SectionsToUse:IP:@'//
     &    'coarse:CoarseTolerances:FP:@final:FinalTolerances:FP:@'//
     &    'step:StepSizeFactor:F:@trace:TraceOutput:I:@'//
     &    'variables:VariablesToSearch:I:@limits:LimitsOnSearch:FA:@'//
     &    'edge:EdgeToIgnore:F:@xminmax:XMinAndMax:IP:@'//
     &    'yminmax:YMinAndMax:IP:@float:FloatOption:I:@'//
     &    'binning:BinningToApply:I:@distance:DistanceMeasure:B:@'//
     &    'linear:LinearInterpolation:B:@near:NearestDistance:I:@'//
     &    'radius:RadiusToSearch:F:@density:DensityDifference:F:@'//
     &    'percent:PercentileRanges:FA:@rad1:FilterRadius1:F:@'//
     &    'rad2:FilterRadius2:F:@sig1:FilterSigma1:F:@sig2:FilterSigma2:F:@'//
     &    'after:FilterAfterBinning:B:@sobel:SobelFilter:B:@'//
     &    'ccc:CorrelationCoefficient:B:@param:ParameterFile:PF:@help:usage:B:'
c       
c       default values for parameters in module and equivalenced arrays
c       
      pclo(1) = 0.
      pclo(2) = 92.
      pchi(1) = 8.
      pchi(2) = 100.
      iftrace=0
      fracmatt=0.05
      ifdist=0
      natural=0
      interp=0
      nxyPatch = 0
      izref = 0
      izali = 0
      do i = 1,6
        alimits(1,i) = 0
        alimits(2,i) = 0
      enddo
      ifFiltAfter = 0
      sigma1 = 0.
      sigma2 = 0.
      radius1 = 0.
      radius2 = 0.
      npad = 16
      ifSobel = 0
      ifccc = 0
      lineuse = 0
      ifXminmax = 0
      ifYminmax = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'xfsimplex',
     &    'ERROR: XFSIMPLEX - ', .true., 3, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
      if (PipGetInOutFile('AImageFile', 1, 'Enter first image file name',
     &    filin1) .ne. 0)call exitError('NO FIRST IMAGE FILE SPECIFIED')
      if (PipGetInOutFile('BImageFile', 2, 'Enter second image file name',
     &    filin2) .ne. 0)call exitError('NO SECOND IMAGE FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 3, 'Transform output file name',
     &    datout) .ne. 0)call exitError('NO TRANSFORM OUTPUT FILE SPECIFIED')
      if (pipinput) then
        xfinfile = ' '
        ierr = PipGetString('InitialTransformFile', xfinfile)
      else
        write(*,'(1x,a,$)')
     &      'File with starting transform, or Return if none: '
        READ (5,40)xfinfile
40      FORMAT (A)
      endif
c       
c       Allocate big array
      allocate(array(idima), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING LARGE IMAGE ARRAY')
c       
c       open file now to get sizes and try to adjust defaults
c       
      call ialprt(.false.)
      CALL IMOPEN(1,FILIN1,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
c       
      if(nx.le.128.and.ny.le.128)then
        ftol1=2.*ftol1
        ptol1=2.*ptol1
      endif
c       
      if (pipinput) then
        ierr = PipGetTwoFloats('CoarseTolerances', ftol2, ptol2)
        ierr = PipGetTwoFloats('FinalTolerances', ftol1, ptol1)
        ierr = PipGetFloat('StepSizeFactor', delfac)
        ierr = PipGetInteger('TraceOutput', iftrace)
        ierr = PipGetInteger('UseTransformLine', lineuse)
        if (lineuse .lt. 0) goto 95
      else
        write(*,'(1x,a,/,a,/,a,/,a,f6.4,f5.2,f7.4,f5.2,f4.1,i2,a,$)')
     &      'Enter fractional tolerances in difference measure and'//
     &      ' in parameter values','   for terminating final search,'//
     &      ' corresponding tolerances for initial search',
     &      '   (or 0,0 for only one search), factor for initial'//
     &      ' step size, and 1 or 2 for','    trace output ['
     &      ,ftol1,ptol1,ftol2,ptol2,delfac,iftrace,']: '
        read(5,*)ftol1,ptol1,ftol2,ptol2,delfac,iftrace
      endif
      trace=iftrace.gt.0
C       
      if (pipinput) then
        ierr = PipGetInteger('VariablesToSearch', natural)
        if(natural.lt.0.or.natural.gt.6)call exitError(
     &      'NUMBER OF VARIABLES TO SEARCH IS OUT OF RANGE')
      else
13      write(*,'(1x,a,i1,a,$)')'0 to search 6 formal variables,'//
     &      ' or # of natural variables to search [',natural,']: '
        read(*,*)natural
        if(natural.lt.0.or.natural.gt.6)go to 13
      endif
      ivst=1
      if(natural.eq.0)then			!all six formal params
        ivend=6
      else
        ivend=natural				!or selected # of natural
        do i=1,6
          a(i)=anat(i)
          da(i)=danat(i)
        enddo
      endif
c       
c       if reading in a transform, get it and convert to natural if necessary
c       
      if(xfinfile.ne.' ')then
        call dopen(1,xfinfile,'old','f')
        do i = 0, lineuse
          read(1,*,err=94,end=95)((amat(ii,jj),jj=1,2),ii=1,2),a(1),a(2)
        enddo
        if(natural.eq.0)then
          a(3)=amat(1,1)
          a(4)=amat(1,2)
          a(5)=amat(2,1)
          a(6)=amat(2,2)
        else
          call amat_to_rotmag(amat,a(3),a(6),a(4),a(5))
        endif
      endif
c       
      do i=1,6
        acall(i)=a(i)
      enddo
c       
      numLimit = 0
      if (pipinput) then
        ierr = PipGetFloat('EdgeToIgnore', fracmatt)
        ierr = PipGetInteger('FloatOption', iflmean)
        ierr = PipGetInteger('BinningToApply', ibinning)
        ierr = PipGetBoolean('DistanceMeasure', ifdist)
        ierr = PipGetTwoIntegers('SectionsToUse', izref, izali)
        ierr = PipGetFloat('FilterSigma1', sigma1)
        ierr = PipGetFloat('FilterSigma2', sigma2)
        ierr = PipGetFloat('FilterRadius1', radius1)
        ierr = PipGetFloat('FilterRadius2', radius2)
        ierr = PipGetBoolean('FilterAfterBinning', ifFiltAfter)
        ierr = PipGetBoolean('SobelFilter', ifSobel)
        ierr = PipGetBoolean('CorrelationCoefficient', ifCCC)
        ibinning = max(1, ibinning)
        ifXminmax = 1 - PipGetTwoIntegers('XMinAndMax', nx1, nx2)
        ifYminmax = 1 - PipGetTwoIntegers('YMinAndMax', ny1, ny2)
        ierr = PipGetInteger('LocalPatchSize', nxyPatch)
c         
c         Get search limits
        if (PipGetFloatArray('LimitsOnSearch', pmlim, numLimit, 6) .eq. 0) then
          if (natural .eq. 0 .and. numLimit .gt. 2) call exitError(
     &        'YOU CAN LIMIT ONLY X AND Y SHIFTS WHEN SEARCHING FOR FORMAL '//
     &        'PARAMETERS; TRY -variables 6')
          if (natural .gt. 0) numLimit = min(numLimit, natural)
          pmlim(1) = pmlim(1) / ibinning
          pmlim(2) = pmlim(2) / ibinning
        endif
      else
        write(*,'(1x,a,f4.2,a,$)')'edge fraction to ignore [' ,fracmatt,']: '
        read(*,*)fracmatt
c       
        write(*,'(1x,a,i1,a,$)')'0 to float to range,'//
     &      ' 1 to float to mean&sd, -1 no float [',iflmean,']: '
        read(*,*)iflmean
c       
        write(*,'(1x,a,i1,a,$)')'Binning to apply to image [' ,ibinning,']: '
        read(*,*)ibinning
        ibinning = max(1, ibinning)
c       
        write(*,'(1x,a,i1,a,$)')'0 for difference,'//
     &      ' 1 for distance measure [',ifdist,']: '
        read(*,*)ifdist
      endif
      if(ifdist.eq.0)then
        if (pipinput) then
          ierr = PipGetBoolean('LinearInterpolation', interp)
        else
          write(*,'(1x,a,i1,a,$)')'1 to use interpolation [' ,interp,']: '
          read(*,*)interp
        endif
      else
c         
c         change defaults based on image size and reduction by 2
c         
        npixel=nx*ny
        if(ibinning.gt.1)then
          npixel=npixel/(ibinning**2)
          radius=4.
        else
          radius=5.
        endif
c         
        if(npixel.gt.480*360)then
          idredun=2
        elseif(npixel.gt.240*180)then
          idredun=1
        else
          idredun=0
        endif
c         
c         run percentile range from 8 down to 5 as go from 320x240 to 640x480
c         
        pcrange=min(8.,max(5.,8.-3.*(npixel-320*240)/
     &      (640*480-320*240)))
        pchi(1)=pcrange
        pclo(2)=100.-pcrange
c         
        if (pipinput) then
          ierr = PipGetInteger('NearestDistance', idredun)
          ierr = PipGetFloat('RadiusToSearch', radius)
          ierr = PipGetFloat('DensityDifference', difflim)
          ibase = 0
          if (PipGetFloatArray('PercentileRanges', array, ibase, 20) .eq. 0)
     &        then
            if (mod(ibase,2) .ne. 0) call exitError(
     &          'YOU MUST ENTER AN EVEN NUMBER OF VALUES FOR PercentileRanges')
            nrange = ibase / 2
            do i = 1, nrange
              pclo(i) = array(2 * i - 1)
              pchi(i) = array(2 * i)
            enddo
          endif
        else
          write(*,'(1x,a,i1,a,$)')'distance to search for and'//
     &        ' eliminate redundancy, 0 not to [',idredun,']: '
          read(*,*)idredun
c         
c           get density window and search radius
          write(*,'(1x,a,f3.1,a,$)')'radius to search for match [',radius,']: '
          read(*,*)radius
c         
          write(*,'(1x,a,f4.2,a,$)')'max density difference for match'
     &        //' as fraction of range [',difflim,']: '
          read(*,*)difflim
c         
          write(*,'(1x,a,i1,a,$)')'number of percentile ranges [' ,nrange,']: '
          read(*,*)nrange
c         
c           get percentile ranges
          write(*,'(1x,a,6f6.1)')'lower and upper percentiles'//
     &        ' in ranges - defaults:',(pclo(i),pchi(i),i=1,nrange)
          read(*,*)(pclo(i),pchi(i),i=1,nrange)
        endif
      endif
      call PipDone()
C       
      call ialprt(.true.)
      call imclose(1)
      CALL IMOPEN(1,FILIN1,'RO')
C       NOTE: ABSOLUTELY NEED TO READ HEADER AGAIN
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      CALL IMOPEN(2,FILIN2,'RO')
      CALL IRDHDR(2,NXYZ2,MXYZ,MODE,DMIN,DMAX,DMEAN)
      if (nxyz2(1) .ne. nx .or. nxyz(2) .ne. ny) call exitError(
     &    'THE TWO IMAGES MUST BE THE SAME SIZE IN X AND Y')
      if (izref .lt. 0 .or. izref .ge. nz .or. izali .lt. 0 .or.
     &    izali .ge. nxyz2(3)) call exitError(
     &    'ONE OF THE SECTION NUMBERS IS OUT OF RANGE')
C       
C       Open data file for transform
C       
      CALL DOPEN(4,DATOUT,'NEW','F')
C       
c       Get possibly filtered sizes, and check dimensions
      nxorig = nx
      nyorig = ny
      nx = nx / ibinning
      ny = ny / ibinning
      if (ifFiltAfter .eq. 0) then
        nxpad = niceframe(nxorig + npad, 2, 19)
        nypad = niceframe(nyorig + npad, 2, 19)
      else
        nxpad = niceframe(nx + npad, 2, 19)
        nypad = niceframe(ny + npad, 2, 19)
      endif
      call setctfwsr(sigma1,sigma2,radius1,radius2,ctf,nxpad,nypad,deltac)

      if (nx*ny.gt.idimb .or. (deltac.ne.0. .and. (nxpad+2)*nypad.gt.idima))
     &    call exiterror('IMAGE TOO BIG FOR ARRAYS - USE HIGHER BINNING OR '//
     &    'FILTER AFTER BINNING')
c       
c       Reduce the shift parameters, and set the limits now that shift is set
      rds=ibinning
      da(1)=da(1)/rds
      da(2)=da(2)/rds
      a(1)=a(1)/rds
      a(2)=a(2)/rds
      do i = 1, numLimit
        if (pmlim(i) .ge. 0.) then
          alimits(1,i) = a(i) - pmlim(i)
          alimits(2,i) = a(i) + pmlim(i)
        endif
      enddo
c
C       just get second section to work with 
      call readFilterSection(2, array, izali, nxorig, nyorig, nx, ny,
     &    nxpad, nypad, ibinning, ctf, deltac, ifFiltAfter, ifSobel, temp,
     &    lenTemp)
c
c       Just deal with points in central portion
      if(fracmatt.ge.1.)then
        mattx=nint(fracmatt/rds)
        matty=mattx
      else
        mattx=max(0,NINT(FLOAT(NX)*fracmatt))
        matty=max(0,NINT(FLOAT(NY)*fracmatt))
      endif
      if (mattx .ge. 0.49 * nx .or. matty .ge. 0.49 * ny) call exitError(
     &    'FRACTION OR NUMBER OF PIXELS TO IGNORE IS TOO LARGE')
      if (ifXminmax .ne. 0) then
        nx1 = (nx1 + ibinning - 1) / ibinning
        nx2 = nx2 / ibinning
        if (nx1 .le. 0 .or. nx2 .gt. nx .or. nx1 .ge. nx2) call exitError
     &      ('STARTING AND ENDING X VALUES OUT OF RANGE OR REVERSED')
      else
        NX1 = 1+mattx 
        NX2 = NX-mattx
      endif
      if (ifYminmax .ne. 0) then
        ny1 = (ny1 + ibinning - 1) / ibinning
        ny2 = ny2 / ibinning
        if (ny1 .le. 0 .or. ny2 .gt. ny .or. ny1 .ge. ny2) call exitError
     &      ('STARTING AND ENDING Y VALUES OUT OF RANGE OR REVERSED')
      else
        NY1 = 1+matty 
        NY2 = NY-matty
      endif
c       
c       Adjust patch size for binning, set to one patch if none, and allocate
      nxyPatch = nxyPatch / ibinning
      if (nxyPatch .ne. 0 .and. nxyPatch .lt. 10) call exitError(
     &      'LOCAL PATCH SIZE MUST BE AT LEAST 10 BINNED PIXELS')
      if (nxyPatch .eq. 0) nxyPatch = max(nx2 + 1 - nx1, ny2 + 1 - ny1)
      numXpatch = (nx2 + 1 - nx1 + nxyPatch - 1) / nxyPatch
      numYpatch = (ny2 + 1 - ny1 + nxyPatch - 1) / nxyPatch
c      print *,nxyPatch, numXpatch,numYpatch
      allocate(sxa(numXpatch,numYpatch), sya(numXpatch,numYpatch),
     &    sxsqa(numXpatch,numYpatch), sysqa(numXpatch,numYpatch),
     &    sxya(numXpatch,numYpatch), npixa(numXpatch,numYpatch), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS FOR PATCHES')
c
      CALL ICLavgsd(ARRAY,nx,ny,NX1,NX2,NY1,NY2 ,DMIN2,DMAX2,tsum,tsumsq,
     &    DMEAN2,sd2)
C       
      if(ifdist.eq.0)then
c         if doing simple difference measure, move array into brray
        do ixy=1,ny*nx
          brray(ixy)=array(ixy)
        enddo
      else
c         if doing distance measure, make histogram of intensities
        do i=0,1000
          ihist(i)=0
        enddo
        histscal=1000./(dmax2-dmin2)
        DO J=NY1,NY2
          ibase=nx*(j-1)
          DO I=NX1,NX2
C             
            ind=(ARRAY(I+ibase)-dmin2)*histscal
            ihist(ind)=ihist(ind)+1
C             
          ENDDO
        ENDDO
c         convert to cumulative histogram
        DO i=1,1000
          ihist(i)=ihist(i)+ihist(i-1)
        enddo
c         find density corresponding to each percentile limit
        do iran=1,nrange
          do lohi=1,2
            ncrit=(nx2+1-nx1)*(ny2+1-ny1)*percen(iran,lohi)/100.
            i=0
            do while(i.le.1000.and.ncrit.gt.ihist(i))
              i=i+1
            enddo
            range(iran,lohi)=(i/histscal)+dmin2
          enddo
        enddo
c         find all points in central part of array within those density
c         limits
        ncompare=0
        do iy=ny1,ny2
          ibase=nx*(iy-1)
          do ix=nx1,nx2
            val=array(ix+ibase)
            do iran=1,nrange
              if(val.ge.ranlo(iran).and.val.le.ranhi(iran))then
c                 redundancy reduction: look for previous nearby points in
c                 list
                if(idredun.gt.0)then
                  ixm=ix-idredun
                  iym=iy-idredun
                  ixp=ix+idredun
                  do ic=ncompare,1,-1
                    ixcm=ixcomp(ic)
                    iycm=iycomp(ic)
c                     if point in list is nearby and within same density
c                     range, skip this one
                    if(ixcm.ge.ixm.and.ixcm.le.ixp.and.iycm.ge.iym
     &                  .and.denlo(ic).ge.ranlo(iran).and.denlo(ic)
     &                  .le.ranhi(iran))go to 80
c                     if gotten back far enough on list, take this point
                    if(iycm.lt.iym.or.(iycm.eq.iym.and.ixcm.lt.ixm))
     &                  go to 78
                  enddo
                endif
78              ncompare=ncompare+1
                ixcomp(ncompare)=ix
                iycomp(ncompare)=iy
c                 just store density temporarily here
                denlo(ncompare)=val
                go to 80
              endif
            enddo
80        enddo
        enddo
        print *,ncompare,' points for comparison'
      endif
c
C       Now get first section
      call readFilterSection(1, array, izref, nxorig, nyorig, nx, ny,
     &    nxpad, nypad, ibinning, ctf, deltac, ifFiltAfter, ifSobel, temp,
     &    lenTemp)
C       
      CALL ICLavgsd(ARRAY,nx,ny,NX1,NX2,NY1,NY2 ,DMIN1,DMAX1,tsum,tsumsq,
     &    DMEAN1,sd1)
C       
c       get scale factor for floating second array densities to match that
c       of first
      scale=1.
      dadd=0.
      if(iflmean.eq.0)then
        SCALE=(DMAX1-dmin1)/(DMAX2 - dmin2)
        dadd=dmin1-scale*dmin2
      elseif(iflmean.gt.0)then
        scale=sd1/sd2
        dadd=dmean1-scale*dmean2
      endif
      if(ifdist.eq.0)then
c         for simple difference, rescale whole array
        do ixy=1,nx*ny
          brray(ixy)=SCALE*brray(ixy)+dadd
        enddo
        CALL DIFF(DELMIN,ARRAY,BRRAY,A,NX,NY)
      else
c         otherwise, for distance,
c         rescale list of densities and add lower and upper window
        window=SCALE*0.5*difflim*(dmax2-dmin2)
        do i=1,ncompare
          valscl=scale*denlo(i)+dadd
          denlo(i)=valscl-window
          denhi(i)=valscl+window
        enddo
c         find points within search radius
        limdxy=radius+1
        nspiral=0
        do idx=-limdxy,limdxy
          do idy=-limdxy,limdxy
            dstnc=sqrt(float(idx**2+idy**2))
            if(dstnc.le.radius)then
              nspiral=nspiral+1
              distspir(nspiral)=dstnc
              idxspir(nspiral)=idx
              idyspir(nspiral)=idy
            endif
          enddo
        enddo
c         order them by distance
        do i=1,nspiral
          do j=i+1,nspiral
            if(distspir(i).gt.distspir(j))then
              tmp=distspir(i)
              distspir(i)=distspir(j)
              distspir(j)=tmp
              itmp=idxspir(i)
              idxspir(i)=idxspir(j)
              idxspir(j)=itmp
              itmp=idyspir(i)
              idyspir(i)=idyspir(j)
              idyspir(j)=itmp
            endif
          enddo
        enddo
C         
        CALL DIST(DELMIN,ARRAY,A,alimits,NX,NY,ixcomp,iycomp,
     &      denlo, denhi,ncompare,idxspir,idyspir,distspir,nspiral
     &      ,natural)
c         
      endif
      ntrial=0
      deltmin=1.e30
c       
c       DNM 4/29/02: search fails if images match perfectly, so skip if so
c       
      if (delmin.gt.0.) then
        ptfac=ptol1
        if(ftol2.gt.0.or.ptol2.gt.0)ptfac=ptol2
        call amoebaInit(pp, yy, 7, ivend, delfac, ptfac, a, da, func, ptol)
        if(ftol2.gt.0.or.ptol2.gt.0)then
          call amoeba(pp,yy,7,ivend,ftol2,func,iter,ptol,jmin)
          if(trace)print *,'restarting'
          deltmin=1.e30
          do i=1,ivend
            a(i)=pp(jmin,i)
          enddo
          call amoebaInit(pp, yy, 7, ivend, delfac, ptol1, a, da, func, ptol)
        endif
        call amoeba(pp,yy,7,ivend,ftol1,func,iter,ptol,jmin)
C         
        do i=1,ivend
          a(i)=pp(jmin,i)
        enddo
      endif
c       
c       DEPENDENCY: transferfid expects two lines with natural params
      PRINT *,' FINAL VALUES'
      deltmin = yy(jmin)
      if (ifCCC .ne. 0) deltmin = 1 - deltmin
      write(*,72)ntrial,deltmin,(a(ii),ii=3,6),rds*a(1),rds*a(2)
72    format(i5,f14.7,4f10.5,2f10.3)
C       
      if(natural.ne.0)then 
        call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
        WRITE(*,70)((amat(ii,jj),jj=1,2),ii=1,2),rds*a(1),rds*a(2)
        WRITE(4,70)((amat(ii,jj),jj=1,2),ii=1,2),rds*a(1),rds*a(2)
      else
        write(4,70)(a(ii),ii=3,6),rds*a(1),rds*a(2)
      endif
70    FORMAT(4f12.7,2f12.3)
C       
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
C       
      call exit(0)
C       
94    call exitError('READING INITIAL TRANSFORM FILE')
95    call exitError('INITIAL TRANSFORM NUMBER OUT OF RANGE')
      END
C       
*********************************************************************
*       
*       FIND MISFIT OF IMAGES
*       
*******************************************************************
c
c       Taking the difference or correlation between images
C       
      SUBROUTINE DIFF(DELTA,CRRAY,DRRAY,A,NXin,NYin)
C       
      use xfsimplex
      implicit none
      real*4 CRRAY(nxin,nyin),DRRAY(nxin,nyin),A(6),amat(2,2)
      real*4 DELTA
      integer*4 nxin, nyin
C       
      real*4 deltalast/0./
      save deltalast
      real*4 xcen,ycen, xadd,yadd,fj,fi,x,y,fx,fx1,fy,fy1,den, delfac,ccc
      real*4 delsum,dend
      integer*4 npix,j, i, ix, iy, ix1, iy1, ixp, iyp, idirx, idiry, idx, idy
      integer*4 numCrit, mindist, ixmin, iymin
      logical*4 oldDiff
      real*8 sx
      real*4 outsideMultiplier

      DELTA=0.

      sx = 0.
      npix = 0
      do iyp = 1, numYpatch
        do ixp = 1, numXpatch
          sxa(ixp,iyp) = 0.
          sya(ixp,iyp) = 0.
          sxya(ixp,iyp) = 0.
          sxsqa(ixp,iyp) = 0.
          sysqa(ixp,iyp) = 0.
          npixa(ixp,iyp) = 0
        enddo
      enddo
      oldDiff = numXpatch * numYpatch .eq. 1 .and. ifCCC .eq. 0
      XCEN=FLOAT(NX)*0.5+0.5			!use +0.5 to be consistent
      YCEN=FLOAT(NY)*0.5+0.5			!with new cubinterp usage
C       
      if(natural.eq.0)then
        amat(1,1)=a(3)
        amat(1,2)=a(4)
        amat(2,1)=a(5)
        amat(2,2)=a(6)
      else
        call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
      endif
c	print *,((amat(i,j),j=1,2),i=1,2)
      xadd=xcen+a(1)
      yadd=ycen+a(2)
      if(interp.eq.0)then
        xadd=xcen+a(1)+0.5			!the 0.5 here gets nearest int
        yadd=ycen+a(2)+0.5
      endif
      DO J=NY1,NY2
        FJ=FLOAT(J) - YCEN
        iyp = (j - ny1) / nxyPatch + 1
        if (interp .eq. 0) then
C           
          DO I=NX1,NX2
            FI=FLOAT(I) - XCEN
C	      
            IX=amat(1,1)*FI + amat(1,2)*FJ + xadd
            IY= amat(2,1)*FI + amat(2,2)*FJ + yadd
C             
            if(ix.ge.1.and.ix.le.nx.and.iy.ge.1.and.iy.le.ny)then
              den = crray(ix,iy)
C               
c               DUPLICATION FROM HERE (sorry, it saves 5% over an internal sub)
              dend = drray(i,j)
              if (oldDiff) then
                sx = sx + abs(den- dend)
                npix=npix+1
              else if (ifCCC .eq. 0) then
                ixp = (i - nx1) / nxyPatch + 1
                sxa(ixp,iyp) = sxa(ixp,iyp) + den - dend
                sxsqa(ixp,iyp) = sxsqa(ixp,iyp) + (den - dend)**2
                npixa(ixp,iyp) = npixa(ixp,iyp) + 1
              else
                ixp = (i - nx1) / nxyPatch + 1
                sxa(ixp,iyp) = sxa(ixp,iyp) + den
                sya(ixp,iyp) = sya(ixp,iyp) + dend
                sxya(ixp,iyp) = sxya(ixp,iyp) + den * dend
                sxsqa(ixp,iyp) = sxsqa(ixp,iyp) + den**2
                sysqa(ixp,iyp) = sysqa(ixp,iyp) + dend**2
                npixa(ixp,iyp) = npixa(ixp,iyp) + 1
              endif
c               DUPLICATION TO HERE
            endif
C             
          ENDDO
c           
        else  
          DO I=NX1,NX2
            FI=FLOAT(I) - XCEN
C	      
            X= amat(1,1)*FI + amat(1,2)*FJ + xadd
            Y= amat(2,1)*FI + amat(2,2)*FJ + yadd
C             
            ix=x
            iy=y
            if(ix.ge.1.and.ix.lt.nx.and.iy.ge.1.and.iy.lt.ny)then
c               IX = MIN(NX-1,MAX(IX,1))
c               IY = MIN(NY-1,MAX(IY,1))
              ix1=ix+1
              iy1=iy+1
              fx=1+ix-x
              fx1=1.-fx
              fy=1+iy-y
              fy1=1.-fy
              den=crray(ix,iy)*fx*fy+crray(ix1,iy)*fx1*fy+
     &            crray(ix,iy1)*fx*fy1+crray(ix1,iy1)*fx1*fy1
C               
c               DUPLICATED SECTION
              dend = drray(i,j)
              if (oldDiff) then
                sx = sx + abs(den- dend)
                npix=npix+1
              else if (ifCCC .eq. 0) then
                ixp = (i - nx1) / nxyPatch + 1
                sxa(ixp,iyp) = sxa(ixp,iyp) + den - dend
                sxsqa(ixp,iyp) = sxsqa(ixp,iyp) + (den - dend)**2
                npixa(ixp,iyp) = npixa(ixp,iyp) + 1
              else
                ixp = (i - nx1) / nxyPatch + 1
                sxa(ixp,iyp) = sxa(ixp,iyp) + den
                sya(ixp,iyp) = sya(ixp,iyp) + dend
                sxya(ixp,iyp) = sxya(ixp,iyp) + den * dend
                sxsqa(ixp,iyp) = sxsqa(ixp,iyp) + den**2
                sysqa(ixp,iyp) = sysqa(ixp,iyp) + dend**2
                npixa(ixp,iyp) = npixa(ixp,iyp) + 1
              endif
            endif
C             
          ENDDO
C	    
        endif
      ENDDO
      if (oldDiff) then
        if (npix .gt. 0) delta = sx / (npix * sd1)
      else
c         
c         Combine patches with less than half the pixels with nearest patch
c         with more than half
        numCrit = nxyPatch**2 / 2
        do iyp = 1, numYpatch
          do ixp = 1, numXpatch
            if (npixa(ixp,iyp) .gt. 0 .and. npixa(ixp,iyp) .lt. numCrit) then
              mindist = 1000000000
c               
c               Find nearest qualifying patch
              do idy = 0, max(iyp, numYpatch - iyp)
                if (idy**2 .le. mindist) then
                  do idiry = -1,1,2
                    iy = iyp + idiry * idy
                    if (iy .ge. 1 .and. iy .le. numYpatch) then
                      do idx = 0, max(ixp, numXpatch - ixp)
                        if (idx**2 .le. mindist) then
                          do idirx = -1,1,2
                            ix = ixp + idirx * idx
                            if (ix .ge. 1 .and. ix .le. numXpatch) then
                              if (npixa(ix,iy) .ge. numCrit .and.
     &                            idx**2 + idy**2 .lt. mindist) then
                                mindist = idx**2 + idy**2
                                ixmin = ix
                                iymin = iy
                              endif
                            endif
                          enddo
                        endif
                      enddo
                    endif
                  enddo
                endif
              enddo
c               
c               If found a qualifying closest patch, add the data into it
              if (mindist .lt. 100000000) then
c                write(*,'(a,6i6)')'Pooling',ixp,iyp,npixa(ixp,iyp),ixmin,
c     &              iymin, npixa(ixmin,iymin)
                npixa(ixmin,iymin) = npixa(ixmin,iymin) + npixa(ixp,iyp)
                sxa(ixmin,iymin) = sxa(ixmin,iymin) + sxa(ixp,iyp)
                sxsqa(ixmin,iymin) = sxsqa(ixmin,iymin) + sxsqa(ixp,iyp)
                sxya(ixmin,iymin) = sxya(ixmin,iymin) + sxya(ixp,iyp)
                sya(ixmin,iymin) = sya(ixmin,iymin) + sya(ixp,iyp)
                sysqa(ixmin,iymin) = sysqa(ixmin,iymin) + sysqa(ixp,iyp)
                npixa(ixp,iyp) = 0
              endif
            endif
          enddo
        enddo
c         
c         Now add up the delta values from the patches, weighted by number of
c         pixels
        delsum = 0.
        npix = 0
        do iyp = 1, numYpatch
          do ixp = 1, numXpatch
            if (npixa(ixp,iyp) .gt. 1) then
              if (ifCCC .eq. 0) then
c                 
c                 Get the SD of the diff and scale it by the sd of the data
                den = (sxsqa(ixp,iyp) - sxa(ixp,iyp)**2 / npixa(ixp,iyp)) /
     &              (npixa(ixp,iyp) - 1.)
                if (den .gt. 0) then
                  delsum = delsum + npixa(ixp,iyp) * sqrt(den) / sd1
                  npix = npix + npixa(ixp,iyp)
                endif
              else
c                  
c                 Or get the ccc and use 1 - ccc
                den = (npixa(ixp,iyp) * sxsqa(ixp,iyp) - sxa(ixp,iyp)**2) *
     &              (npixa(ixp,iyp) * sysqa(ixp,iyp) - sya(ixp,iyp)**2)
                if (den .gt. 0) then
                  ccc = (npixa(ixp,iyp) * sxya(ixp,iyp) -
     &                sxa(ixp,iyp) * sya(ixp,iyp)) / sqrt(den)
                  delsum = delsum + npixa(ixp,iyp) * (1. - ccc)
                  npix = npix + npixa(ixp,iyp)
                endif
              endif
            endif
          enddo
        enddo
c         
c         Take the weighted average
        if (npix .gt. 0) delta = delsum / npix
      endif
c       
      delfac = outsideMultiplier(a, alimits)
      delta = delta * delfac
C       
c       DNM 5/17/02: if the number of pixels falls below a small threshold
c       return a big delta; otherwise adjust for # of pixels and save value
c       5/13/06: Normalize to # of Sds difference per pixel
c       
      if (delfac .gt. 1. .or. npix.gt.0.02*(nx2+1-nx1)*(ny2+1-ny1))then
        deltalast=delta
      else
        delta = 10.*deltalast
      endif
      RETURN
C       
      END
      

c       Or measure distance between features
c
      subroutine DIST(DELTA,ARRAY,A,alimits,NX,NY, ixcomp,iycomp,denlo,denhi,
     &    ncompare, idxspir,idyspir,distspir,nspiral,natural)
      implicit none
C       
      integer*4 NX,NY,ncompare,nspiral,natural
      real*4 DELTA
      real*4 ARRAY(nx,ny),A(6),amat(2,2),alimits(2,6)
      integer*2 ixcomp(*),iycomp(*)
      real*4 denlo(*),denhi(*),distspir(*)
      integer*4 idxspir(*),idyspir(*)
      real*4 xcen, ycen, xadd, yadd,FJ,FI,dstnc,critlo,crithi,den1,distmax
      integer*4 ispir,ixa,iya,icomp,ix,iy
      real*4 delfac,outsideMultiplier
C       
      XCEN=FLOAT(NX)*0.5+0.5
      YCEN=FLOAT(NY)*0.5+0.5
C       
      DELTA = 0.
C       
      xadd=xcen+a(1)+0.5
      yadd=ycen+a(2)+0.5
      if(natural.eq.0)then
        amat(1,1)=a(3)
        amat(1,2)=a(4)
        amat(2,1)=a(5)
        amat(2,2)=a(6)
      else
        call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
      endif
c	print *,((amat(i,j),j=1,2),i=1,2)
      distmax=distspir(nspiral)+1.
      do icomp=1,ncompare
        FJ=iycomp(icomp) - YCEN
        FI=ixcomp(icomp) - XCEN
C         
        IX=amat(1,1)*FI + amat(1,2)*FJ + xadd
        IY= amat(2,1)*FI + amat(2,2)*FJ + yadd
C         
        dstnc=distmax
        critlo=denlo(icomp)
        crithi=denhi(icomp)
        do ispir=1,nspiral
          ixa=min(nx,max(1,ix+idxspir(ispir)))
          iya=min(ny,max(1,iy+idyspir(ispir)))
          den1=array(ixa,iya)
          if(den1.ge.critlo.and.den1.le.crithi)then
            dstnc=distspir(ispir)
            go to 40
          endif
        enddo
40      DELTA=DELTA + dstnc
C         
      ENDDO
C       
c       5/13/06: Normalize to per comparison point to match normalization
c       of difference measure
c       
      delfac = outsideMultiplier(a, alimits)
      delta=delfac * delta/ncompare
      RETURN
C       
      END
C       
c       
c       find a multiplier for the delta factor if outside limits
c
      real*4 function outsideMultiplier(a, alimits)
      implicit none
      real*4 a(6), alimits(2,6),delfac,outside
      integer*4 i
      delfac = 1.
      do i = 1, 6
        if (alimits(1,i) .lt. alimits(2,i)) then
          outside = max(a(i) - alimits(2,i), alimits(1,i) - a(i)) /
     &        (alimits(2,i) - alimits(1,i))
          if (outside .gt. 0.) delfac = max(delfac, 100.**min(5., outside))
        endif
      enddo
      outsideMultiplier = delfac
      return 
      end


c       Function to be called by minimization routine
c       
      subroutine func(x, error)
      use xfsimplex
      implicit none
      real*4 x(*),a(6), error
      real*4 delta,deltout
      character*1 starout
      integer*4 i,ii
c       
      do i=1,ivend
        a(i)=x(i)
      enddo
      do i = ivend + 1, 6
        a(i) = acall(i)
      enddo
c       
      if(ifdist.eq.0)then
        CALL DIFF(DELTA,ARRAY,BRRAY,A,NX,NY)
      else
        CALL DIST(DELTA,ARRAY,A,alimits,NX,NY,ixcomp,iycomp,denlo,denhi,
     &      ncompare,idxspir,idyspir,distspir,nspiral,natural)
      endif
      error=delta
      ntrial=ntrial+1
      if(iftrace.ne.0)then
        starout=' '
        if(delta.lt.deltmin)then
          starout='*'
          deltmin=delta
        endif
        deltout = delta
        if (ifCCC .ne. 0) deltout = 1. - delta
        if(iftrace.eq.1.or.starout.eq.'*') write(*,72)
     &      starout,ntrial,deltout,(a(ii),ii=3,6),rds*a(1),rds*a(2)
72      format(1x,a1,i3,f14.7,4f10.5,2f10.3)
      endif
      return
      end

c       
c       Read a section with optional fourier filtering before or after
c       binning, and with optional sobel filtering
c
      subroutine readFilterSection(iunit, array, iz, nxorig, nyorig, nx, ny,
     &    nxpad, nypad, ibinning, ctf, deltac, ifFiltAfter, ifSobel, temp,
     &    lenTemp)
      implicit none
      real*4 array(*), ctf(*), deltac, temp(*)
      integer*4 iunit, iz, nxorig, nyorig, nx, ny, nxpad, nypad
      integer*4 ibinning, ifFiltAfter, ifSobel, lenTemp
      integer*4 ierr, nxfilt, nyfilt, ixlo, iylo 
      real*4 xOffset, yOffset
      integer*4 scaledSobel
c       
c       Read in the section without or with binning, set size being filtered
      if (deltac .ne. 0 .and. ifFiltAfter .eq. 0) then
        call imposn(iunit, iz, 0)
        call irdsec(iunit, array, 99)
        nxfilt = nxorig
        nyfilt = nyorig
      else
        call irdbinned(iunit,iz, array,nx, ny, 0, 0, ibinning, nx, ny, temp,
     &      lentemp, ierr)
        if (ierr .ne. 0) goto 99
        nxfilt = nx
        nyfilt = ny
      endif
c       
c       Apply fourier filter
      if (deltac .ne. 0.) then
        call taperOutPad(array, nxfilt, nyfilt, array, nxpad + 2, nxpad, nypad,
     &      0, 0)
        call todfft(array, nxpad, nypad, 0)
        call filterpart(array, array, nxpad, nypad, ctf, deltac)
        call todfft(array, nxpad, nypad, 1)
        ixlo = (nxpad - nxfilt) / 2
        iylo = (nypad - nyfilt) / 2
        call irepak(array,array,nxpad+2,nypad,ixlo,ixlo+nxfilt-1,iylo,
     &      iylo+nyfilt-1)
c         
c         Now bin if necessary
        if (ifFiltAfter .eq. 0 .and. ibinning .gt. 1) call reduce_by_binning
     &      (array, nxorig, nyorig, ibinning, array, nxfilt, nyfilt)
      endif
c       
c       Do sobel if requested: NOTE THAT IT CAN BE DONE IN PLACE AS LONG AS
c       SCALING = 1 BUT THIS SHOULD BE DOCUMENTED
      if (ifSobel .ne. 0) then
        if (scaledSobel(array, nx, ny, 1., 1., 1, 2, array, nxfilt, nyfilt,
     &      xOffset, yOffset) .ne. 0)
     &      call exitError('GETTING MEMORY FOR SOBEL FILTERING')
      endif
      return
99    call exiterror('READING FILE')
      end
c       
c       $Log$
c       Revision 3.15  2009/10/14 17:48:48  mast
c       Allocated big array in module to avoid compile error in gfortran/Mac
c
c       Revision 3.14  2009/10/13 13:44:05  mast
c       bad if test
c
c       Revision 3.13  2009/10/12 17:54:42  mast
c       Added weighted averaging of local patch cross-correlation or SD of
c       difference
c
c       Revision 3.12  2009/09/07 17:24:20  mast
c       Switched centering of transform to be consistent with cubinterp
c
c       Revision 3.11  2008/07/01 14:26:27  mast
c       Converted to PIP, added fourier and sobel filtering, limits to search,
c       subarea specification, correlation coefficient, sections to use,
c       transform line to use (with an eye to stripping xfalign down).
c
c       Revision 3.10  2006/06/22 01:06:59  mast
c       Changed initialization to avoid big executable on Intel Mac
c
c       Revision 3.9  2006/06/18 19:37:59  mast
c       Changed to use new C function for amoeba
c
c       Revision 3.8  2006/06/08 18:31:15  mast
c       Changed to read in data binned and to handle 4x2K after binning
c
c       Revision 3.7  2006/05/14 03:13:15  mast
c       Output normalized difference and distance measures, standardize
c       error outputs
c
c       Revision 3.6  2005/05/26 04:34:52  mast
c       Made sums args for iclavgsd real*8
c	
c       Revision 3.5  2003/12/24 19:07:27  mast
c       Moved amoeba subroutine to a library
c	
c       Revision 3.4  2002/08/20 19:23:48  mast
c       Didn't change both calls to iclavgsd
c	
c       Revision 3.3  2002/08/18 23:13:05  mast
c       Changed to call iclavgsd in library
c	
c       Revision 3.2  2002/05/20 15:47:33  mast
c       Made the DIFF function put out a very high value when the number of
c       pixels evaluated falls below 1% of total pixels, to keep it from
c       running away into impossible shifts.  Also increased dimensions of
c       input array to allow 4Kx4K images.
c	
c       Revision 3.1  2002/04/29 16:18:37  mast
c       Added test to keep it from failing when images match perfectly
c	

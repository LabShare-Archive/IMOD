c       DENSNORM
c
c       Densnorm can normalize images by dividing by the incident
c       illumination, an operation usually referred to as mass normalization.
c       It can take the log of normalized data, or simply take the log of the
c       input data.  It can also output weighting factors that can be supplied
c       to Tilt to provide a relative normalization of the data, thus
c       avoiding the need to write a normalized file.
c       
c       See man page for details
c       
c       $Id$
c       Log at end of file
c
      implicit none
      integer idim, maxdose, maxpiece
      parameter (idim = 20000000, maxdose = 4000, maxpiece=100000)
      real*4 doses(maxdose),refdoses(maxdose)
      real*4 array(idim)
      integer*4 ixpiece(maxpiece),iypiece(maxpiece),izpiece(maxpiece)
      integer*4 izpcInput(maxpiece),izpcSrc(maxpiece)
      integer*4 nxyz(3),mxyz(3),nx,ny,nz,nxyz2(3)
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      integer*4 i,j,maxLines,numChunks, iChunk,numLines,ierr,ierr2,ierr3
      integer*4 ifRefMean,ifRefDose,ifLog,ifpack,ndose,maxzInput,npcInput
      integer*4 iunit, nzsrc,mode2,maxzSrc,npcSrc,nrefdose,maxzRef,npcRef
      integer*4 modeIn,modeOut,ind,iz,cospower
      real*4 refMean,refDose,dmin,dmax,dmean,dmin2,dmax2,dmean2
      real*4 tmin,tmax,tmean, sum, scale,rangeFrac,dosefac,valmin, baselog
      real*4 addback, resubval
      real*8 dsum8
      character*160 infile,outfile,expfile,tiltfile,wgtfile,reffile,otherfile
      character*80 titlech
      character dat*9, tim*8, reltext*8, logtext*11
      logical*4 relative,reverse, ignoreExp,subtracted,resubtract, divideby2
      real*4 cosd
      common /bigarr/ array

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetFloat, PipGetLogical
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  densnorm
c       
      integer numOptions
      parameter (numOptions = 21)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputImageFile:FN:@output:OutputImageFile:FN:@'//
     &    'weight:WeightOutputFile:FN:@images:ImagesWithExposures:FN:@'//
     &    'rifile:ReferenceImageFile:FN:@rimean:MeanOfReferenceImage:F:@'//
     &    'riexp:ExposureOfReferenceImage:F:@'//
     &    'subtracted:SubtractedIntegers:B:@resub:Resubtract:B:@'//
     &    'divide:DivideBy2:B:@log:LogOfOutput:F:@mode:ModeOfOutput:I:@'//
     &    'scale:ScalingFactor:F:@reverse:ReverseContrast:B:@'//
     &    'ignore:IgnoreExposures:B:@expfile:ExposureFile:FN:@'//
     &    'tiltfile:TiltFile:FN:@power:CosinePowerInverse:I:@'//
     &    'minlog:MinimumLogFactor:F:@param:ParameterFile:PF:@help:usage:B:'
c       
c       Defaults
      infile = ' '
      outfile = ' '
      expfile = ' '
      tiltfile = ' '
      wgtfile = ' '
      reffile = ' '
      otherfile = ' '
      cospower = 1.
      ignoreExp = .false.
      reverse = .false.
      relative = .true.
      rangeFrac = 0.001
      baselog = 0.
      subtracted = .false.
      resubtract = .false.
      divideby2 = .false.
c       
c	  Pip startup: set error, parse options, do help output
      call PipReadOrParseOptions(options, numOptions, 'densnorm',
     &    'ERROR: DENSNORM - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
c       
c       Get file options, check for consistency
      ierr = PipGetInOutFile('InputImageFile', 1, ' ', infile)
      ierr = PipGetInOutFile('OutputImageFile', 2, ' ', outfile)
      if (infile .eq. ' ' .and. outfile .ne. ' ') call exitError('YOU MUST'//
     &    'SPECIFY AN INPUT IMAGE FILE IF YOU HAVE AN OUTPUT IMAGE FILE')
      ierr = PipGetString('WeightOutputFile', wgtfile)
      ierr = 1 - PipGetString('ExposureFile', expfile)
      ierr2 = 1 - PipGetString('TiltFile', tiltfile)
      ierr3 = 1 - PipGetString('ImagesWithExposures', otherfile)
      if (outfile .eq. ' ' .and. wgtfile .eq. ' ') call exitError(
     &    'YOU MUST SPECIFY EITHER AN IMAGE OR WEIGHTING OUTPUT FILE')
c       
      if (ierr + ierr2 + ierr3 .gt. 1) call exitError('YOU SHOULD ENTER'//
     &    'ONLY ONE OF TILT FILE, EXPOSURE FILE, OR IMAGE FILE WITH EXPOSURES')
c       
c       Get reference image options
      ierr = 1 - PipGetString('ReferenceImageFile', reffile)
      ifRefMean = 1 - PipGetFloat('MeanOfReferenceImage', refMean)
      ifRefDose = 1 - PipGetFloat('ExposureOfReferenceImage', refDose)
      if (ierr + ifRefMean + ifRefDose .gt. 0 .and. ierr2 .gt. 1)
     &    call exitError('IT MAKES NO SENSE TO ENTER ANY REFERENCE '//
     &    'INFORMATION WITH A TILT FILE')
      if (ierr + ifRefMean .eq. 0 .and. ifRefDose .eq. 1) call exitError(
     &    'CANNOT USE A REFERENCE EXPOSURE WITHOUT A REFERENCE IMAGE OR MEAN')
      if (ierr + ifRefMean .eq. 2) call exitError(
     &    'IT MAKES NO SENSE TO ENTER BOTH A REFERENCE IMAGE AND A MEAN')
c       
c       Other options than can be gotten at this time
      ifLog = 1 - PipGetFloat('LogOfOutput', baselog)
      ierr = PipGetLogical('IgnoreExposures', ignoreExp)
      ierr = PipGetInteger('CosinePowerInverse', cosPower)
      ierr = PipGetLogical('ReverseContrast', reverse)
      ierr = PipGetFloat('MinimumLogFactor', rangeFrac)
      ierr = PipGetLogical('SubtractedIntegers', subtracted)
      ierr = PipGetLogical('Resubtract', resubtract)
      ierr = PipGetLogical('DivideBy2', divideby2)
      ifpack = 1
      ndose = 0
      if (infile .ne. ' ') then
c         
c         Open and read the header of the input file to figure out if it is
c         a montage; otherwise doses should be packed for existing Z values
        call imopen(1, infile, 'RO')
        call irdhdr(1,nxyz,mxyz,modein,dmin,dmax,dmean)
        call getHeaderPiecesDoses(1, array, idim*4, nz, 0, doses,  maxdose,
     &      ndose, ixpiece, iypiece, izpcInput, maxpiece, maxzInput,
     &      npcInput)
        if (npcInput .ne. 0) ifpack = 0
        modeOut = modeIn
        ierr = PipGetInteger('ModeOfOutput', modeOut)
      endif
c       
c       Get exposures from somewhere: exposure file
      if (expfile .ne. ' ') then
        call dopen(1, expfile, 'ro', 'f')
        ndose = 0
10      read(1, *,end=11,err=95)doses(ndose + 1)
        ndose = ndose + 1
        goto 10
11      close(1)
c         
c         Tilt file
      elseif (tiltfile .ne. ' ') then
        call dopen(1, tiltfile, 'ro', 'f')
        ndose = 0
20      read(1, *,end=21,err=96)doses(ndose + 1)
        ndose = ndose + 1
        goto 20
21      close(1)
        do i = 1, ndose
          doses(i) = 1. / cosd(doses(i))**(1./cosPower)
        enddo
c         
c         Image file unless ignoring
      elseif (.not. ignoreExp) then
        if (otherfile .ne. ' ') then
          call imopen(2, infile, 'RO')
          call irdhdr(2,nxyz2,mxyz,mode2,dmin2,dmax2,dmean2)
          iunit = 2
          nzsrc = nxyz2(3)
        else
          if (infile .eq. ' ') call exitError(
     &        'YOU MUST SPECIFY A SOURCE FOR THE NORMALIZING DATA')
          iunit = 1
          nzsrc = nz
        endif
        call getHeaderPiecesDoses(iunit, array, idim*4, nzSrc, 0, doses,
     &      maxdose, ndose, ixpiece, iypiece, izpcSrc, maxpiece, maxzSrc,
     &      npcSrc)
        if (iunit .eq. 2) call imclose(2)
      endif
c
      if (ndose .gt. 0) then
c         
c         Are there the right number of doses?
        if (infile .ne. ' ' .and. (npcInput .eq. 0 .and. ndose .ne. nz) .or.
     &      (npcInput .gt. 0 .and. ndose .ne. maxzInput)) call exitError(
     &      'NUMBER OF EXPOSURES DOES NOT APPEAR TO MATCH NUMBER OF IMAGES')
c       
c         Now evaluate whether there is reference data
        if (reffile .ne. ' ') then
          call imopen(2, reffile, 'RO')
          call irdhdr(2,nxyz2,mxyz,mode2,dmin2,dmax2,dmean2)
          if (subtracted .and. mode2 .ne. 1 .and. mode2 .ne. 2) call exitError(
     &        'REFERENCE IMAGE FILE IS NOT THE RIGHT MODE TO CONTAIN '//
     &        'SUBTRACTED VALUES')
          if (ifRefDose .eq. 0) then
c             
c             Get doses from reference image
            call getHeaderPiecesDoses(iunit, array, idim*4, nxyz2(3), 1,
     &          refdoses, maxdose, nrefdose, ixpiece, iypiece, izpiece,
     &          maxpiece, maxzRef, npcRef)         
            if (nrefdose .eq. 0) call exitError(
     &          'REFERENCE IMAGE FILE HAS NO EXPOSURE DOSE VALUES')
            ifRefDose = 1
            refDose = refdoses(1)
          endif
c           
c           Get mean of reference image
          dsum8 = 0.
          do j = 1, nxyz2(2)
            call irdlin(2, array, *99)
            do i = 1, nxyz2(1)
              dsum8 = dsum8 + array(i)
            enddo
          enddo
          refMean = (dsum8 / nxyz2(1)) / nxyz2(2)
          if (subtracted) refMean = refMean + 32768
          ifRefMean = 1
          call imclose(2)
        endif
c       
c         Determine if the scaling is relative
        if (ifRefMean .eq. 1 .and. ifRefDose .eq. 1) relative = .false.
c       
c       If doing output, check state of subtractions
      if (outfile .ne. ' ') then
        if (subtracted .and. modeIn .ne. 1 .and. modeIn .ne. 2) call exitError(
     &      'INPUT IMAGE FILE IS NOT THE RIGHT MODE TO CONTAIN '//
     &      'SUBTRACTED VALUES')
        if (resubtract .and. divideby2) call exitError(
     &      'YOU CANNOT ENTER BOTH -resubtract AND -divideby2')
        if ((resubtract .or. divideby2) .and. .not.subtracted) call exitError(
     &      'YOU CANNOT ENTER -resubtract OR -divideby2 UNLESS DATA WERE '//
     &      'SUBTRACTED')
        if ((resubtract .or. divideby2) .and. (modeOut .ne. 1 .or.
     &      .not.relative .or. ifLog .ne. 0)) call exitError(
     &      'YOU CANNOT ENTER -resubtract OR -divideby2 UNLESS DOING '//
     &      'RELATIVE NORMALIZATIONS WITH OUTPUT MODE OF 1 AND NO LOG')
        if (subtracted .and. baselog .gt. 30000) call exitError(
     &      'YOU SHOULD ENTER "-log 0" WITH -subtracted')
      endif
c       
c         Make the weights
        if (relative) then
c           
c           Relative weights have a mean of zero
          sum = 0.
          do i = 1, ndose
            doses(i) = 1. / doses(i)
            sum = sum + doses(i) / ndose
          enddo
          do i = 1, ndose
            doses(i) = doses(i) / sum
          enddo
        else
c           
c           Absolute weights
          do i = 1, ndose
            doses(i) = refDose / (doses(i) * refMean)
          enddo
        endif
c         
c         Write to file if called for
        if (wgtfile .ne. ' ') then
          call dopen(1, wgtfile, 'new', 'f')
          write(1,'(f17.10)')(doses(i),i=1,ndose)
          close(1)
        endif

      else
c         
c         No doses are available: require log output, make unit factors
        if (outfile .eq. ' ' .or. ifLog .eq. 0) call exitError
     &      ('YOU MUST ENTER SOME KIND OF EXPOSURE DATA OR PRODUCE LOG OUTPUT')
        ndose = nz
        if (npcInput .gt. 0) ndose = maxzInput
        do i = 1, ndose
          doses(i) = 1.
        enddo
      endif
c       
c       Done if not producing output
      if (outfile .eq. ' ') then
        call imclose(1)
        call exit(0)
      endif
c
      call imopen(2, outfile, 'new')
      call itrhdr(2, 1)
      call ialmod(2, modeOut)
c       
c       Set scaling to 1 for float output, then to 25000 for attenuations,
c       then to 5000 for logs
      scale = 1.
      if (PipGetFloat('ScalingFactor', scale) .ne. 0 .and. modeOut .ne. 2
     &    .and. (.not. relative .or. ifLog .ne. 0)) then
        scale = 25000.
        if (ifLog .ne. 0) scale = 5000.
        if (modeOut .eq. 6) scale = scale * 2.
        if (modeOut .eq. 0) scale = scale / 100.
        if (.not. relative .and. (modeOut .eq. 6 .or. modeOut .eq. 0)) then
          scale = -scale
          reverse = .false.
        endif
        print *,'Output will be scaled by default factor of', scale
      endif
      if (reverse) scale = -scale
      addback = 0.
      resubval = 0.
      if (subtracted) addback = 32768.
      if (divideby2) scale = scale * 0.5
      if (resubtract) resubval = 32768.
      call PipDone()
c       
c       Process the image file in chunks
      maxLines = idim / nx
      numChunks = (ny + maxLines - 1) / maxLines
      dsum8 = 0.
      dmin2 = 1.e37
      dmax2 = -dmin2
      call imposn(1, 0, 0)
      do iz = 1, nz
        ind = izpcInput(iz) + 1
        if (ind .lt. 1 .or. ind .gt. ndose) then
          write(*,'(/,a,i6,a,i6,a)')'ERROR: DENSNORM - SECTION',iz, ' HAS Z'//
     &        ' VALUE OF',ind - 1,', OUT OF RANGE OF NORMALIZATION FACTORS'
          call exit(1)
        endif
c         
c         Get the scaling and the scaled minimum value for logs
        dosefac = doses(ind)
        valmin = rangeFrac * (dmax - dmin) * dosefac
        do iChunk = 1, numChunks
          numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
          call irdsecl(1,array, numLines, *100) 
c           
c           Log scaling (relative or absolute)
          if (ifLog .ne. 0) then
            do i = 1 , nx*numLines
              array(i) = scale * alog10(max(valmin, (array(i) + addback +
     &            baselog) * doseFac))
            enddo
c             
c             Relative linear scaling
          elseif (relative) then
            do i = 1 , nx*numLines
              array(i) = (array(i) + addback) * scale * doseFac - resubval
            enddo
          else
c             
c             Absolute linear scaling
            do i = 1 , nx*numLines
              array(i) = scale * ((array(i) + addback) * doseFac - 1.)
            enddo
          endif
          call iclden(array,nx,numLines,1,nx,1,numLines,tmin,tmax,tmean)
          dsum8 = dsum8 + tmean * numLines
          dmin2 = min(dmin2, tmin)
          dmax2 = max(dmax2, tmax)
          call iwrsecl(2,array, numLines)
        enddo
      enddo
c
      dmean = (dsum8 / ny) / nz
      reltext = 'Absolute'
      if (relative) reltext = 'Relative'
      logtext = 'linear'
      if (ifLog .ne. 0) logtext = 'logarithmic'
c
      call date(dat)
      call time(tim)
      write(titlech,3000) reltext,logtext,dat,tim
3000  format('DENSNORM: ', a, ' density normalization, ', a, t57, a9, 2x, a8)
      call iwrhdrc(2,titlech,1,dmin2,dmax2,dmean)
      call imclose(1)
      call imclose(2)
      call exit(0)

95    call exitError('READING EXPOSURE FILE')
96    call exitError('READING TILT FILE')
99    call exitError('READING REFERENCE IMAGE FILE')
100   call exitError('READING INPUT IMAGE FILE')
      end
        

c       Gets piece list and doses from the header of unit IUNIT, with ARRAY
c       as scratch array with MAXARR bytes, NZ dimension of file, IFPACK to
c       pack dose data down for montage with missing images, NDOSE doses
c       returned in DOSES of size MAXDOSE, NPIECE piece coordinates returned
c       IXPIECE, IYPIECE, IZPIECE of size MAXPIECE, and MAXZ being the highest
c       Z index numbered from 1
c
      subroutine getHeaderPiecesDoses(iunit, array, maxarr, nz, ifpack, doses,
     &    maxdose, ndose, ixpiece, iypiece, izpiece, maxpiece, maxz, npiece)
      implicit none
      integer*4 iunit, maxarr, nz, ndose, maxdose, maxpiece, maxz
      integer*4 npiece, ixpiece(*), iypiece(*), izpiece(*), ifpack
      real*4 array(*), doses(*)
      integer*4 i, nbsym,nbyte,iflags,ndoseout

      call irtnbsym(iunit,nbsym)
      if(nbsym.gt.maxarr) call exitError(
     &    'ARRAYS NOT LARGE ENOUGH FOR EXTRA HEADER DATA')
      call irtsym(iunit,nbsym,array)
      call irtsymtyp(iunit,nbyte,iflags)
      call get_extra_header_pieces(array,nbsym,nbyte,iflags,nz,
     &    ixpiece,iypiece,izpiece,npiece,maxpiece)
      if(npiece.eq.0)then
        do i=1,nz
          izpiece(i)=i-1
        enddo
        maxz=nz
      else
        maxz=0
        do i=1,npiece
          maxz=max(maxz,izpiece(i)+1)
        enddo
      endif
      if (maxz .gt. maxdose) call exitError('Z VALUES TOO HIGH FOR ARRAYS')
c       
c       set up a marker value for empty slots
      do i=1,maxz
        doses(i)=-999.
      enddo
      call get_extra_header_items(array,nbsym,nbyte,iflags,nz,6,
     &    doses,array,ndose,maxdose,izpiece)
      if (ndose .eq. 0 .or. ifpack .eq. 0) return
c       
c       pack the values down
      ndoseout=0
      do i=1,ndose
        if(doses(i).ne.-999.)then
          ndoseout=ndoseout+1
          doses(ndoseout)=doses(i)
        endif
      enddo
      ndose = ndoseout
      return
      end

c       $Log$
c       Revision 3.2  2007/08/10 16:14:46  mast
c       Added options for dealing with integers where 32768 was subtracted
c
c       Revision 3.1  2007/07/17 03:36:39  mast
c       Added to package
c
c

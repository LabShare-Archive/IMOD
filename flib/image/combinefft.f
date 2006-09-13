*       * * * * * COMBINEFFT * * * * * *
c       
c       COMBINEFFT combines the FFTs from the two tomograms of a double-axis
c       tilt series, taking into account the tilt range of each tilt series
c       and the transformation used to match one tomogram to the other.  For
c       a location in Fourier space where there is data from one tilt series
c       but not the other, it takes the Fourier value from just the one
c       appropriate FFT; everywhere else it averages the Fourier values from
c       the two FFT files.
c       
c       See man page for more details.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.4  2004/10/22 00:41:30  mast
c       Changed to shells with interzone reduction as new default
c	
c       Revision 3.3  2004/08/22 14:58:48  mast
c       Used line_is_filename as workaround to Windows problem
c	
c       Revision 3.2  2004/07/22 23:45:58  mast
c       Activated interzone option and allowed reduction > 1
c	
c       Revision 3.1  2004/07/13 18:13:54  mast
c       Converted to PIP input, did declarations and error exits, 
c       incorporated old weighting stuff and added missing-wedge reduction
c       options
c	
c       
c       David Mastronarde, November 1995
c       
      implicit none
      integer idim, numlook, limview, limring,limslab
      parameter (idim=5000*400)
      parameter (numlook=16000, limview = 1440, limring = 100, limslab = 100)
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3),nxyz2(3),NX,NY,NZ
      complex ARRAY(idim),BRRAY(idim),crray(idim),aVal
C       
      real*4 minv(3,3)
      real*4 tilta(limview),tiltb(limview),densa(limview),densb(limview)
      integer*4 looka(-numlook:numlook),lookb(-numlook:numlook)
      integer*4 numInRing(limring,limslab,3)
      real*4 ringSum(limring,limslab,3)

      COMMON //NX,NY,NZ
      CHARACTER*120 FILIN,FILOUT
C       
      EQUIVALENCE (NX,NXYZ)
      character dat*9,tim*8
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      logical ina,inb,verbose,interZone,independent,jointZone
      real*4 dmin,dmax,dmean,acritlo,acrithi,bcritlo,bcrithi
      integer*4 ierr,mode,iunout,i,j,nviewa,nviewb,iz,ind,iy,ix,ilook
      real*4 tsum,tmin,tmax,delx,dely,delz,za,ya,yasq,xa,xasq,xp,yp,zp,ra,rb
      real*4 rata,ratb,wa,wb,tmean,weightPower,facLookA,facLookB,ratmagsq
      real*4 dra,drb,drsum,fRing,fSlab,ySlab,thisMean,sumTmp(3)
      integer*4 numSlabs, numRings,iSlab,iRing,iZone,minInRing,nextRing
      real*4 radiusMin,ringWidth,reduceFrac,bothMean, oneMean, target,ring
      integer*4 nextSlab, numInZero
      real*4 radsq, bothRad, bothRadSq, zasq
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetFloat, PipGetLogical
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  combinefft
c       
      integer numOptions
      parameter (numOptions = 20)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'ainput:AInputFFT:FN:@binput:BInputFFT:FN:@output:OutputFFT:FN:@'//
     &    'atiltfile:ATiltFile:FN:@btiltfile:BTiltFile:FN:@'//
     &    'ahighest:AHighestTilts:FP:@bhighest:BHighestTilts:FP:@'//
     &    'inverse:InverseTransformFile:FN:@reduce:ReductionFraction:F:@'//
     &    'separate:SeparateReduction:B:@joint:JointReduction:B:@'//
     &    'ring:RingWidth:F:@nslabs:NumberOfSlabsInY:I:@'//
     &    'radius:MinimumRadiusToReduce:F:@points:MinimumPointsInRing:I:@'//
     &    'both:LowFromBothRadius:F:@verbose:VerboseOutput:B:@'//
     &    'weight:WeightingPower:F:@param:ParameterFile:PF:@help:usage:B:'
c       
      weightPower = 0.
      reduceFrac = 0.
      numSlabs = 1
      ringWidth = 0.01
      radiusMin = 0.01
      minInRing = 30
      filout = ' '
      verbose = .false.
      jointZone = .false.
      independent = .false.
      bothRad = 0.0
      numInZero = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'combinefft',
     &    'ERROR: COMBINEFFT - ', .true., 1, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
c       Get input and output files
c       
      if (PipGetInOutFile('AInputFFT', 1, 'Name of first tomogram FFT file',
     &    filin) .ne. 0) call exitError('NO FIRST INPUT FFT FILE SPECIFIED')
      call imopen(1,filin,'ro')
c       
      if (PipGetInOutFile('BInputFFT', 2, 'Name of second tomogram FFT file',
     &    filin) .ne. 0) call exitError('NO SECOND INPUT FFT FILE SPECIFIED')
      call imopen(2,filin,'old')
c       
      ierr = PipGetInOutFile('OutputFFT', 3,
     &    'Name of output file, or Return to put in 2nd file', filout)
c       
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
      call irdhdr(2,nxyz2,mxyz,mode,dmin,dmax,dmean)
      if(nx*ny.ge.idim)call exitError('IMAGE TOO LARGE FOR ARRAYS')
      if (nx .ne. nxyz2(1) .or. ny .ne. nxyz2(2) .or. nz .ne. nxyz2(3))
     &    call exitError('THE TWO FILES ARE NOT THE SAME SIZE')
      
      iunout=2
      if(filout.ne.' ')then
        call imopen(3,filout,'new')
        iunout=3
        call itrhdr(3,1)
      endif

      filin = ' '
      if (pipinput) then
        ierr = PipGetString('InverseTransformFile', filin)
      else
        write(*,'(1x,a,$)')
     &      'File with inverse of matching transformation: '
        read(5,'(a)')filin
      endif
      if (filin .eq. ' ') call exitError(
     &    'NO FILE SPECIFIED WITH INVERSE TRANSFORMATION')
      call dopen(1,filin,'ro','f')
      do i=1,3
        read(1,*)(minv(i,j),j=1,3)
      enddo
      close(1)
c       
      call gettilts(pipinput, 'AHighestTilts', 'ATiltFile', 'first',
     &    nviewa, tilta, limview, looka, numlook, facLookA, densa,
     &    acritlo, acrithi)
      call gettilts(pipinput, 'BHighestTilts', 'BTiltFile', 'second',
     &    nviewb, tiltb, limview, lookb, numlook, facLookB, densb,
     &    bcritlo, bcrithi)

      if (pipinput) then
        ierr = PipGetFloat('WeightingPower', weightPower)
        ierr = PipGetFloat('ReductionFraction', reduceFrac)
        if (reduceFrac .gt. 10.) call exitError(
     &      'REDUCTION FRACTION MUST NOT BE BIGGER THAN 10')
        ierr = PipGetLogical('SeparateReduction', independent)
        ierr = PipGetLogical('JointReduction', jointZone)
        interZone = .not.(jointZone.or.independent)
        ierr = PipGetLogical('VerboseOutput', verbose)
        ierr = PipGetFloat('LowFromBothRadius', bothRad)
      endif
c       
c       set up reduction
c       
      if (reduceFrac .gt. 0.) then
        ierr = PipGetFloat('RingWidth', ringWidth)
        ierr = PipGetFloat('MinimumRadiusToReduce', radiusMin)
        ierr = PipGetInteger('NumberOfSlabsInY', numSlabs)
        ierr = PipGetInteger('MinimumPointsInRing', minInRing)
        if (numSlabs .gt. limslab) call exitError(
     &      'TOO MANY SLABS IN Y FOR ARRAYS')
        numRings = (0.9 - radiusMin) / ringWidth + 2
        if (numRings .gt. limring) call exitError(
     &      'TOO MANY RINGS FOR ARRAYS WITH THIS RING WIDTH')
        if (ringWidth .le. 0) call exitError('ILLEGAL ENTRY FOR RING WIDTH')
        if (numSlabs .le. 0) call exitError(
     &      'ILLEGAL ENTRY FOR NUMBER OF SLABS')
        if (minInRing .le. 5) call exitError(
     &      'MINIMUM NUMBER OF POINTS IN RING IS TOO SMALL TO USE')
        if (radiusMin .lt. 0) call exitError('MINIMUM RADIUS MUST BE POSITIVE')
        
        do iy = 1, numSlabs
          do ix = 1, numRings
            do iz = 1,3
              numInRing(ix,iy,iz) = 0
              ringSum(ix,iy,iz) = 0.
            enddo
          enddo
        enddo
      endif
c       
      call PipDone()
c       
      tsum=0.
      tmin=1.e30
      tmax=-1.e30
      delx=0.5/(nx-1.)
      dely=1./ny
      delz=1./nz
      bothRadSq = bothRad**2
c       
      do iz=1,nz
        call irdsec(1,array,*99)
        call imposn(2,iz-1,0)
        call irdsec(2,brray,*99)
        ind=1
c         
c         this assumes Z has been reordered to be sequential in 3-D FFT
c         
        za=delz*(iz-1.)-0.5
        zasq = za**2
        do iy=1,ny
c	    
c	    this assumes Y has been reordered to be sequential in 2-D FFT
c           
          ya=dely*(iy-1.)-0.5
          yasq=ya**2
          do ix=1,nx
            xa=delx*(ix-1.)
            xasq=xa**2
c             
c             back transform this position to get vector in fft b
c             
            xp=minv(1,1)*xa+minv(1,2)*ya+minv(1,3)*za
            yp=minv(2,1)*xa+minv(2,2)*ya+minv(2,3)*za
            if(xp.lt.0.)then
              xp=-xp
              yp=-yp
            endif
            radsq = zasq + xasq + yasq
c             
            rata=ya/max(xa,1.e-6)
            ratb=yp/max(xp,1.e-6)
            ina=(rata.ge.acritlo.and.rata.le.acrithi).or.radsq.lt.bothRadSq
            inb=(ratb.ge.bcritlo.and.ratb.le.bcrithi).or.radSq.lt.bothRadSq
c
c             For counting pixels 
c            ina=(rata.ge.acritlo.and.rata.le.acrithi)
c            inb=(ratb.ge.bcritlo.and.ratb.le.bcrithi)
c            if (.not.(ina.and.inb) .and. radSq.lt.bothRadSq) then
c              ina = .true.
c              inb = .true.
c              numInZero = numInZero + 1
c            endif
            aVal = array(ind)
c	      array(ind)=(2.,0.)
            if(.not.ina.and..not.inb)then
c               
c               if in neither, take simple mean
c               
              array(ind)=0.5*(array(ind)+brray(ind))
c		array(ind)=(2.5,0.)
            elseif(.not.ina.and.inb)then
c               
c               if in B alone, take b's value
c               
              array(ind)=brray(ind)
c		array(ind)=(3.,0.)
            elseif(ina.and.inb)then
c               
c               in both: need to mix in selected way
c               
              wa=0.5
              wb=0.5
              if (weightPower .gt. 0.001) then
c                 
c                 weighting by local density:  Find radius in A and B.
c                 get change in magnitude and make the magnitude change by
c                 the same amount in opposite direction, since stretching in
c                 real space is squeezing in Fourier space (the square is
c                 needed to get from stretch through null to squeeze).
c                 
                zp=minv(3,1)*xa+minv(3,2)*ya+minv(3,3)*za
                ratmagsq=(xasq+yasq+zasq)/(xp**2+yp**2+zp**2)
                ra=sqrt(xasq+yasq)
                rb=sqrt(xp**2+yp**2) * ratmagsq
c                 
c                 find tilt density and
c                 effectively divide by radius (multiply by other radius)
c                 
                ilook=nint(facLookA*rata)
                dra=(rb*densa(looka(ilook)))**weightPower
                ilook=nint(facLookB*ratb)
                drb=(ra*densb(lookb(ilook)))**weightPower
                drsum=dra+drb
                if(drsum.gt.1.e-4)then
                  wa=dra/drsum
                  wb=drb/drsum
                endif
              endif
c		array(ind)=(wa,0.)
              array(ind)=wa*array(ind)+wb*brray(ind)
c               
c               else if in A and not in B, leave A value as is
c               
            endif
c             
c             if reducing, compute ring, slab, and zone and add abs value
c             
            if (reduceFrac .gt. 0.) then
              iSlab = max(1, min(numSlabs, int((ya + 0.5) * numSlabs) + 1))
              ra=sqrt(xasq+yasq+zasq)
              iRing = int((ra - radiusMin) / ringWidth) + 1
              if (interZone) then
c                 
c                 If doing comparisons between zones, determine zone and
c                 sum specifically for the zones
c                 
                iZone = 0
                if (ina .and. .not.inb) iZone = 1
                if (ina .and. inb) iZone = 2
                if (.not.ina .and. inb) iZone = 3
                if (iRing .gt. 0 .and. iZone .gt. 0) then
                  numInRing(iRing, iSlab, iZone) =
     &                numInRing(iRing, iSlab, iZone) + 1
                  ringSum(iRing, iSlab, iZone) =
     &                ringSum(iRing, iSlab, iZone) + cabs(array(ind))
                endif
              else
c                 
c                 Otherwise, just do joint area and sum A, B, and average
c                 
                if (iRing .gt. 0 .and. ina .and. inb) then
                  numInRing(iRing, iSlab, 2) = numInRing(iRing, iSlab, 2) + 1
                  ringSum(iRing, iSlab, 1) = ringSum(iRing, iSlab, 1) +
     &                cabs(aVal)
                  ringSum(iRing, iSlab, 2) = ringSum(iRing, iSlab, 2) +
     &                cabs(array(ind))
                  ringSum(iRing, iSlab, 3) = ringSum(iRing, iSlab, 3) +
     &                cabs(brray(ind))
                endif
              endif
            endif
            ind=ind+1
          enddo
        enddo
        call iclcdn(array,nx,ny,1,nx,1,ny,dmin,dmax,dmean)
        tmin=min(tmin,dmin)
        tmax=max(tmax,dmax)
        tsum=tsum+dmean
        call imposn(iunout,iz-1,0)
        call iwrsec(iunout,array)
      enddo
c       
c       If reducing, first get the scaling factors for the rings
c       
      if (reduceFrac .gt. 0.) then
        if (verbose) print *,'Ring  Slab  Zone  Mixed mean  Single mean',
     &      '    Target   Reduction factor'
        do ix = 1, numRings
          do iy = 1, numSlabs
            if (.not.interZone) then
              numInRing(ix, iy, 1) = numInRing(ix, iy, 2)
              numInRing(ix, iy, 3) = numInRing(ix, iy, 2)
            endif
            sumTmp(1) = ringSum(ix,iy,1)
            sumTmp(3) = ringSum(ix,iy,3)
            do iz = 1,3,2
              if (numInRing(ix, iy, iz) .ge. minInRing .and.
     &            numInRing(ix, iy, 2) .ge. minInRing) then
                bothMean = ringSum(ix, iy, 2) / numInRing(ix, iy, 2)
                thisMean = sumTmp(iz) / numInRing(ix, iy, iz)
                oneMean = thisMean
                if (.not.independent .and. .not.interZone) oneMean = 0.5 *
     &              (sumTmp(1) + sumTmp(3)) /numInRing(ix,iy,2)
                target = max(0., (1. - reduceFrac) * oneMean +
     &              reduceFrac * bothMean)
                ringSum(ix, iy, iz) = min(1., target / oneMean)
                if (verbose) write(*,107)
     &              ix,iy,iz,bothMean, thisMean,target,ringSum(ix,iy,iz)
107             format(i4,2i6,1x,3f12.4,f8.4)
              else
                ringSum(ix,iy,iz) = 1.
              endif
            enddo
          enddo
        enddo
c         
c         Now run through the file again 
c         
        tsum=0.
        tmin=1.e30
        tmax=-1.e30
c         
        do iz=1,nz
          call imposn(iunout,iz-1,0)
          call irdsec(iunout,array,*99)
          ind=1
          za=delz*(iz-1.)-0.5
          do iy=1,ny
            ya=dely*(iy-1.)-0.5
            yasq=ya**2
            ySlab = (ya + 0.5) * numSlabs
            iSlab = int(ySlab + 0.5)
            fSlab = ySlab + 0.5 - iSlab
            nextSlab = min(numSlabs, iSlab + 1)
            iSlab = max(1, iSlab)
            do ix=1,nx
c               
c               Find out where pixel is again
c               
              xa=delx*(ix-1.)
              xasq=xa**2
              xp=minv(1,1)*xa+minv(1,2)*ya+minv(1,3)*za
              yp=minv(2,1)*xa+minv(2,2)*ya+minv(2,3)*za
              if(xp.lt.0.)then
                xp=-xp
                yp=-yp
              endif
c		
              rata=ya/max(xa,1.e-6)
              ratb=yp/max(xp,1.e-6)
              ina=rata.ge.acritlo.and.rata.le.acrithi
              inb=ratb.ge.bcritlo.and.ratb.le.bcrithi
              ra=sqrt(xasq+yasq+za**2)
              iZone = 0 
              if (ina .and. .not.inb) iZone = 1
              if (.not.ina .and. inb) iZone = 3
              if (iZone .gt. 0 .and. ra .ge. radiusMin) then
c                 
c                 find ring and slab and adjust when in A or B only
c                 
                ring = (ra - radiusMin) / ringWidth
                iRing = int(ring + 0.5)
                fRing = ring + 0.5 - iRing
                nextRing = min(numRings, iRing + 1)
                iRing = max(1, iRing)
                
                array(ind) = array(ind) *
     &              ((1 - fRing) * (1 - fSlab) * ringSum(iRing, iSlab, iZone)
     &              + (1 - fRing) * fSlab * ringSum(iRing, nextSlab, iZone) +
     &              fRing * (1 - fSlab) * ringSum(nextRing, iSlab, iZone) +
     &              fRing * fSlab * ringSum(nextRing, nextSlab, iZone))
              endif
              ind=ind+1
            enddo
          enddo
          call iclcdn(array,nx,ny,1,nx,1,ny,dmin,dmax,dmean)
          tmin=min(tmin,dmin)
          tmax=max(tmax,dmax)
          tsum=tsum+dmean
          call imposn(iunout,iz-1,0)
          call iwrsec(iunout,array)
        enddo
      endif

      tmean=tsum/nz
      call date(dat)
      call time(tim)
c       
c       7/7/00 CER: remove the encodes.  DNM 7/8/04 use iwrhdrc
c       
C       encode ( 80, 3000, title ) dat, tim
      write(titlech,3000) dat,tim
      call iwrhdrc(iunout,titlech,1,tmin,tmax,tmean)
      call imclose(iunout)
c      print *,numInZero,' points added to joint area near zero radius'
      call exit(0)
3000  format ( 'COMBINEFFT: Combined FFT from two tomograms',t57,a9,
     &    2x,a8)
99    call exitError( 'READING FFT FILE')
      end



c       GETTILTS gets tilt angles and/or cutoffs for tangent of high angles
c       PIPINPUT is a logical, true for PIP input
c       ANGLEOPT is the name of the option for entering highest angles
c       FILEOPT is the name of the option for entering a tilt angle file
c       WHICH has 'irst' or 'second'
c       NVIEW is returned with the number of angles read, or zero
c       TILT is returned with tilt angles
c       LIMVIEW is the size of the angle arrays
c       LOOK is a filled with a lookup table from tangents to densities,
c       it is dimensioned to -NUMLOOK:NUMLOOK,
c       FACLOOK is returned with a factor to use for the lookup
c       DENS is returned with relative tilt densities
c       CRITLO and CRITHI are returned with criterion tangents of highest
c       angles
c       
      subroutine gettilts(pipinput, angleOpt, fileOpt, which, nview,tilt,
     &    limview,look,numlook,facLook,dens,critlo,crithi)
      implicit none
      logical pipinput
      character*(*) angleOpt, fileOpt, which
      integer*4 numlook,look(-numlook:numlook),nview,limview
      real*4 dens(*),tilt(*),wincr(20),critlo,crithi,tiltlo, tilthi,facLook
      character*120 line
      integer*4 ierr, ierr2,nweight,i,j,iv,iw,minlook,maxlook,indlook
      integer*4 nextlook,mid,ilook
      real*4 tmp,sumint,wsum,avgint
      integer*4 PipGetSTring, PipGetTwoFloats
      real*4 tand
      logical line_is_filename
c       
      nweight=2
c       
      nview=0
      tilthi=-9999.
      line = ' '

      if (pipinput) then
        ierr = PipGetTwoFloats(angleOpt, tiltlo, tilthi)
        ierr2 = PipGetString(fileOpt, line)
        if (ierr .eq. 0 .and. ierr2 .eq. 0) call exitError('YOU '//
     &      'CANNOT ENTER BOTH HIGHEST ANGLES AND A TILT ANGLE FILE')
        if (ierr .ne. 0 .and. ierr2 .ne. 0) call exitError('YOU MUST'//
     &      'ENTER EITHER HIGHEST ANGLES OR A TILT ANGLE FILE')
        if (ierr .eq. 0) go to 20
      else
        print *,'For ',which,' tomogram file, enter either the ',
     &      'starting and ending tilt ',
     &      ' angles, or the name of a file with tilt angles in it'
        read(5,'(a)')line
        if (line_is_filename(line)) go to 10
        read(line,*,err=10,end=10)tiltlo,tilthi
        go to 20
      endif
c       
c       get here either way if line has a filename for tilt angle file
c       
10    if (line .eq. ' ')call exitError('NO TILT ANGLE FILENAME ENTERED')
      call dopen(1,line,'ro','f')
15    read(1,*,err=25,end=18)tilt(nview + 1)
      nview = nview + 1
      if (nview .ge. limview) call exitError(
     &    'TOO MANY TILT ANGLES FOR ARRAYS')
      go to 15
18    if (nview.lt.2) call exitError('TOO FEW TILT ANGLES IN FILE')
      close(1)
      tiltlo = tilt(1)
      tilthi = tilt(nview)
c       
c       INVERT TILT ANGLES BECAUSE "TILT" PROGRAM IS WEIRD
c       
      do i=1,nview
        tilt(i)=-tilt(i)
      enddo
c       
c       order tilt angles to make it easier to get densities
c       
      do i=1,nview-1
        do j=i+1,nview
          if(tilt(i).gt.tilt(j))then
            tmp=tilt(i)
            tilt(i)=tilt(j)
            tilt(j)=tmp
          endif
        enddo
      enddo
c       
c       compute densities just as in Tilt program
c       
      do i=1,nweight
        wincr(i)=1./(i-0.5)
      enddo
      avgint=(tilt(nview)-tilt(1))/(nview-1)
      do iv=1,nview
        sumint=0
        wsum=0.
        do iw=1,nweight
          if(iv-iw.gt.0)then
            wsum=wsum+wincr(iw)
            sumint=sumint+wincr(iw)*(tilt(iv+1-iw)-
     &          tilt(iv-iw))
          endif
          if(iv+iw.le.nview)then
            wsum=wsum+wincr(iw)
            sumint=sumint+wincr(iw)*(tilt(iv+iw)-
     &          tilt(iv+iw-1))
          endif
        enddo
        dens(iv)=avgint/(sumint/wsum)
      enddo
c       
c       build lookup table to tilt angles
c       
      facLook = (numlook - 10.) / tand(max(abs(tilt(1)), abs(tilt(nview))))
      do i=-numlook,numlook
        look(i)=0
      enddo
      minlook=numlook
      maxlook=-numlook
c       
c       fill in values at the angles
c       
      do i=1,nview
        indlook=nint(facLook*tand(tilt(i)))
        look(indlook)=i
        minlook=min(minlook,indlook)
        maxlook=max(maxlook,indlook)
      enddo
c       
c       extend endpoints
c       
      do i=-numlook,minlook-1
        look(i)=look(minlook)
      enddo
      do i=maxlook+1,numlook
        look(i)=look(maxlook)
      enddo
      ilook=minlook
c       
c       fill in each interval split between neighbors
c       
      do while(ilook.lt.maxlook)
        nextlook=ilook+1
        do while (look(nextlook).eq.0)
          nextlook=nextlook+1
        enddo
        mid=(nextlook+ilook)/2
        do i=ilook+1,mid
          look(i)=look(ilook)
        enddo
        do i=mid+1,nextlook-1
          look(i)=look(nextlook)
        enddo
        ilook=nextlook
      enddo
c       
20    if (tilthi.eq.-9999.)call exitError('HIGHEST TILT ANGLE NOT ENTERED')
c       
c       INVERT ANGLES HERE TOO
c       
      critlo=tand(min(-tiltlo,-tilthi))
      crithi=tand(max(-tiltlo,-tilthi))
c       
c       If there were no tilt angles, just set up for equal densities
c       
      if (nview .eq. 0) then
        facLook = (numlook - 10.) / max(abs(critlo), abs(crithi))
        do i=-numlook,numlook
          look(i)=1
        enddo
        dens(1) = 1.
      endif
      return
25    call exitError('READING TILT ANGLE FILE')
      end

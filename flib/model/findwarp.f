*       * * * * * FINDWARP * * * * * *
c       
c       FINDWARP will solve for a series of general 3-dimensional linear
c       transformations that can then be used by WARPVOL to align two
c       volumes to each other.  It performs a series of multiple linear
c       regression on subsets of the displacements between the volumes
c       determined at a matrix of positions (patches).  
c       
c       See man page for details
c
c       $Id$
c       
      implicit none
      include 'statsize.inc'
      integer idim,limpatch,limvert,limaxis,limtarg,limdiag
      parameter (idim=40000,limpatch=500000,limvert=500000)
      parameter (limaxis=1000, limtarg=100, limdiag = 2*limaxis)
      real*4 xr(msiz,idim)
      real*4 cx(limpatch,3),dx(limpatch,3)
      real*4 firstm(3,3),firstd(3),a(3,3),dxyz(3),devxyz(3)
      real*4 devxyzmax(3),cenloc(3),atmp(3,3),dtmp(3)
      real*4 asave(3,3,limpatch),censave(3,limpatch)
      real*4 dxyzsave(3,limpatch),xyzsum(3),resSum(limpatch)
      logical solved(limpatch),exists(limpatch)
      integer*4 nxyz(3),idrop(idim), numRes(limpatch)
      integer*4 inddrop(idim),ntimes(idim)
      real*4 dropsum(idim),debugxyz(3),dxloc,dyloc,dzloc
      logical inside
      real*4 xvert(limvert),yvert(limvert),zcont(idim),cenmin(3),cenmax(3)
      integer*4 indvert(idim),nvert(idim)
      integer*4 nfxauto(limpatch),nfyauto(limpatch),nfzauto(limpatch)
      integer*4 inrowx(limaxis),inrowy(limaxis),inrowz(limaxis)
      integer*4 inDiag1(0:limdiag), inDiag2(0:limdiag)
      character*320 filename, resfile
      real*4 cxyzin(3),dxyzin(3),targetres(limtarg),devavAuto(limpatch)
      integer*4 npatxyz(3),listpos(limaxis,3),indxyz(3)
      integer*4 nfitx,nfity,nfitz,nfitxyz(3),nxtot,nytot,nztot,ntotxyz(3)
      integer*4 nfitxin,nfityin,nfitzin,nfitxyzin(3)
      equivalence (npatx,npatxyz(1)),(npaty,npatxyz(2)), (npatz,npatxyz(3))
      equivalence (nfitx,nfitxyz(1)),(nfity,nfitxyz(2)), (nfitz,nfitxyz(3))
      equivalence (nxtot,ntotxyz(1)),(nytot,ntotxyz(2)), (nztot,ntotxyz(3))
      equivalence (nfitxin,nfitxyzin(1)),(nfityin,nfitxyzin(2)),
     &    (nfitzin,nfitxyzin(3))
      integer*4 ndat,nFileDat,npatx,npaty,npatz,i,j,ipos,inlist,k,itmp,ind
      integer*4 ncont,ierr,indy,indz,iobj,ip,indcur,ipt,numTarget,indTarget
      integer*4 nofsx,nofsy,nofsz,ifsubset,ifdoSlab, ifdebug
      real*4 xofs,yofs,zofs,ximscale,yimscale,zimscale
      real*4 ratmin,ratmax,fracdrop,crit,critabs,elimmin
      integer*4 ifauto,nauto,niny,ix,iy,iz,iauto
      integer*4 nlocdone,nexclxhi,nexclyhi,nexclzhi
      integer*4 ifdiddle,nlocx,nlocy,nlocz,numZero,numDevSum
      integer*4 induse,nlistd,ndroptot,locx,locy,locz,lx,ly,lz,ifuse
      real*4 ratio,devavmin,dist,distmin,devavsum,devmaxsum,devmaxmax
      real*4 dzmin,dz,devmax,devavg,devsd,zscal,xyscal, discount
      real*4 devavavg,devmaxavg,devavmax, determ, detmean
      integer*4 icmin,icont,indv,indlc,ipntmax,maxdrop, numLowDet
      integer*4 ifon,ndrop,ifflip,indpat,indloc,icolfix,nyDiag
      character*5 rowslab(2) /'rows ', 'slabs'/
      character*5 rowslabcap(2) /'ROWS ', 'SLABS'/
      character*1 yztext(2) /'Y', 'Z'/
      logical*4 debugHere
      common /bigarr/xr,cx,dx,asave,censave,dxyzsave,xvert,yvert
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoIntegers,PipGetThreeIntegers
      integer*4 PipGetString,PipGetFloat,PipGetFloatArray,PipgetTwoFloats
      integer*4 PipGetInOutFile,PipGetLogical,PipGetThreeFloats
c      
c       fallbacks from ../../manpages/autodoc2man -2 2  findwarp
c       
      integer numOptions
      parameter (numOptions = 19)
      character*(40 * numOptions) options(1)

      indpat(ix,iy,iz)=ix + (iy-1)*npatx + (iz-1)*npatx*npaty
      indloc(ix,iy,iz)=ix + (iy-1)*nlocx + (iz-1)*nlocx*nlocy

      options(1) =
     &    'patch:PatchFile:FN:@output:OutputFile:FN:@region:RegionModel:FN:@'//
     &    'volume:VolumeOrSizeXYZ:FN:@initial:InitialTransformFile:FN:@'//
     &    'residual:ResidualPatchOutput:FN:@target:TargetMeanResidual:FA:@'//
     &    'measured:MeasuredRatioMinAndMax:FP:@xskip:XSkipLeftAndRight:IP:@'//
     &    'yskip:YSkipLowerAndUpper:IP:@zskip:ZSkipLowerAndUpper:IP:@'//
     &    'rowcol:LocalRowsAndColumns:IP:@slabs:LocalSlabs:I:@'//
     &    'maxfrac:MaxFractionToDrop:F:@minresid:MinResidualToDrop:F:@'//
     &    'prob:CriterionProbabilities:FP:@'//
     &    'discount:DiscountIfZeroVectors:F:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'
c
      nofsx=0
      nofsy=0
      nofsz=0
      nexclxhi = 0
      nexclyhi = 0
      nexclzhi = 0
      ifsubset=0
      ifdebug = 0
      ratmin=4.0
      ratmax=20.0
      fracdrop=0.1
      crit=0.01
      critabs=0.002
      elimmin=0.5
      ifdoSlab=0
      resfile = ' '
      discount = 0.
c
      call PipReadOrParseOptions(options, numOptions, 'findwarp',
     &    'ERROR: FINDWARP - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
C       
C       Open patch file and figure out sampled positions
C       
      if (PipGetInOutFile('PatchFile', 1,
     &    'Name of file with correlation positions and results', filename)
     &    .ne. 0) call exiterror('NO INPUT PATCH FILE SPECIFIED')

      call dopen(1,filename,'old','f')
      read(1,*)ndat
      if(ndat.gt.limpatch)call exitError('TOO MANY PATCHES FOR ARRAYS')
      npatx=0
      npaty=0
      npatz=0
      do i=1,ndat
        read(1,*)(cxyzin(j),j=1,3)
        do j=1,3
          ipos=nint(cxyzin(j))
          inlist=0
          k=1
          do while(inlist.eq.0.and.k.le.npatxyz(j))
            if(ipos.eq.listpos(k,j))inlist=k
            k=k+1
          enddo
          if(inlist.eq.0)then
            if(npatxyz(j).gt.limaxis)then
              write(*,'(//,a,i2)')
     &            'ERROR: FINDWARP - TOO MANY POSITIONS ALONG AXIS',j
              call exit(1)
            endif
            npatxyz(j)=npatxyz(j)+1
            listpos(npatxyz(j),j)=ipos
          endif
        enddo
      enddo
c       
c       sort the position lists
c       
      do i=1,3
        do j=1,npatxyz(i)-1
          do k=j,npatxyz(i)
            if(listpos(j,i).gt.listpos(k,i))then
              itmp=listpos(j,i)
              listpos(j,i)=listpos(k,i)
              listpos(k,i)=itmp
            endif
          enddo
        enddo
      enddo
c       
      nxtot=npatx
      nytot=npaty
      nztot=npatz
      nfityin=nytot
      print *,'Number of patches in X, Y and Z is:',npatx,npaty,npatz
      rewind(1)
      read(1,*)ndat
c       
      if (.not. pipinput) print *,'Enter NX, NY, NZ or name of file ',
     &    'for tomogram being matched to'
      call get_nxyz(pipinput, 'VolumeOrSizeXYZ', 'FINDWARP', 1,nxyz)
c       
c       mark positions as nonexistent and fill cx from list positions
c       
      do k=1,npatz
        do j=1,npaty
          do i=1,npatx
            ind=indpat(i,j,k)
            exists(ind)=.false.       
            cx(ind,1)=listpos(i,1)
            cx(ind,2)=listpos(j,2)
            cx(ind,3)=listpos(k,3)
          enddo
        enddo
      enddo
c       
c       read each line, look up positions in list and store in right place
c       
      do i=1,ndat
c         
c         these are center coordinates and location of the second volume
c         relative to the first volume
c         
        read(1,*)(cxyzin(j),j=1,3),(dxyzin(j),j=1,3)
        do j=1,3
          ipos=nint(cxyzin(j))
          k=1
          do while(k.le.npatxyz(j).and.ipos.gt.listpos(k,j))
            k=k+1
          enddo
          indxyz(j)=k
        enddo
        ind=indpat(indxyz(1),indxyz(2),indxyz(3))
        exists(ind)=.true.
        do j=1,3
          cx(ind,j)=cxyzin(j)
          dx(ind,j)=dxyzin(j)
        enddo
      enddo
c       
      nFileDat = ndat
      close(1)
c       
c       get patch region model
c       
      if (pipinput) then
        filename = ' '
        ierr = PipGetString('RegionModel', filename)
      else
        write(*,'(1x,a,/,a,$)')
     &      'Enter name of model file with contour enclosing area to '
     &      //'use,',' or Return to use all patches: '
        read(*,'(a)')filename
      endif
      ncont=0
      if(filename.ne.' ') then
        call get_region_contours(filename, 'FINDWARP', xvert, yvert, nvert,
     &      indvert, zcont, ncont, ifflip, idim, limvert, 0)
      else
        ifflip = 0
        if (npaty .lt. npatz) ifflip = 1
      endif
      indy = 2
      if (ifflip .ne. 0) indy = 3
      indz = 5 - indy
c       
c       aspectmax=3.
c       
8     if (pipinput) then
        ifauto = PipGetTwoIntegers('LocalRowsAndColumns', nfitxin,
     &      nfitxyzin(indy))
      else
        write(*,'(1x,a,$)')'1 to find best warping automatically, 0 '//
     &      'to proceed interactively: '
        read(5,*)ifauto
      endif
c
      if(ifauto.ne.0)then
c         
c         initialize auto 1: get target and ratio parameters
c
        if (pipinput) then
          numTarget = 0
          if (PipGetFloatArray('TargetMeanResidual', targetres, numTarget,
     &        limtarg) .gt. 0) call exitError(
     &        'TARGET MEAN RESIDUAL MUST BE ENTERED FOR AUTOMATIC FITS')
          ierr = PipGetTwoFloats('MeasuredRatioMinAndMax', ratmin, ratmax)
        else
          write(*,'(1x,a,$)')'One or more mean residuals to achieve: '
          read(5,'(a)')filename
          call frefor(filename, targetRes, numTarget)
          write(*,'(1x,a,/,a,2f5.1,a,$)') 'Minimum and maximum ratio of '//
     &        'measurements to unknowns to test','  (/ for'
     &        ,ratmin,ratmax,'): '
          read(5,*)ratmin,ratmax
        endif
c         write(*,'(1x,a,f5.0,a,$)')
c         &           'Maximum aspect ratio to allow in an area to be fit (/ for'
c         &           ,aspectmax,'): '
c         read(5,*)aspectmax
      endif
c       
c       Get rows, columns, slabs to exclude and check entries
c       
      if (pipinput) then
        ierr = PipGetTwoIntegers('XSkipLeftAndRight', nofsx,nexclxhi) +
     &      PipGetTwoIntegers('YSkipLowerAndUpper', nofsy,nexclyhi) +
     &      PipGetTwoIntegers('ZSkipLowerAndUpper', nofsz,nexclzhi)
        ifsubset = 3 - ierr
        ierr = PipGetFloat('MaxFractionToDrop', fracdrop)
        ierr = PipGetFloat('MinResidualToDrop', elimmin)
        ierr = PipGetTwoFloats('CriterionProbabilities', crit, critabs)
        ierr = PipGetString('ResidualPatchOutput', resFile)
        ierr = PipGetFloat('DiscountIfZeroVectors', discount)
        ifdebug = 1 - PipGetThreeFloats('DebugAtXYZ', debugxyz(1), debugxyz(2),
     &      debugxyz(3))
      else
        write(*,'(1x,a,$)')'0 to include all positions, or 1 to '//
     &      'exclude rows or columns of patches: '
        read(5,*)ifsubset
      endif
c       
      if(ifsubset.ne.0)then
10      if (.not. pipinput) then
          nexclxhi = npatx-nofsx-nxtot
          write(*,'(1x,a,2i3,a,$)')'# of columns to exclude'
     &        //' on the left and right in X (/ for',nofsx, nexclxhi,'): '
          read(5,*)nofsx,nexclxhi
        endif
        nxtot=npatx-nofsx-nexclxhi
        if(nxtot+nofsx.gt.npatx.or.nxtot.lt.2)then
          if (ifauto .ne. 0) call exitError(
     &        'ILLEGAL ENTRY FOR NUMBER OF COLUMNS TO EXCLUDE IN X')
          print *,'Illegal entry'
          nofsx=0
          nxtot=npatx
          go to 10
        endif
        
12      if (.not. pipinput) then
          nexclyhi = npaty-nofsy-nytot
          write(*,'(1x,a,a,a,2i3,a,$)')'# of ',rowslab(1+ifflip),' to exclude'
     &        //' on the bottom and top in Y (/ for',nofsy, nexclyhi,'): '
          read(5,*)nofsy,nexclyhi
        endif
        nytot=npaty-nofsy-nexclyhi
        if(nytot+nofsy.gt.npaty.or.nytot.lt.1 )then
          if (ifauto .ne. 0) call exitError('ILLEGAL ENTRY FOR NUMBER OF '//
     &        rowslabcap(1+ifflip)//' TO EXCLUDE IN Y')
          print *,'Illegal entry'
          nofsy=0
          nytot=npaty
          go to 12
        endif

14      if (.not. pipinput) then
          nexclzhi = npatz-nofsz-nztot
          write(*,'(1x,a,a,a,2i3,a,$)')'# of ',rowslab(2-ifflip),' to exclude'
     &        //' on the bottom and top in Z (/ for',nofsz, nexclzhi,'): '
          read(5,*)nofsz,nexclzhi
        endif
        nztot=npatz-nofsz-nexclzhi
        if(nztot+nofsz.gt.npatz.or.nztot.lt.2)then
          if (ifauto .ne. 0) call exitError( 'ILLEGAL ENTRY FOR NUMBER OF '//
     &        rowslabcap(2-ifflip)//' TO EXCLUDE IN Z')
          print *,'Illegal entry'
          nofsz=0
          nztot=npatz
          go to 14
        endif
        print *,'Remaining # of patches in X, Y and Z is:',
     &      nxtot,nytot,nztot
      else
        nxtot=npatx
        nytot=npaty
        nztot=npatz
        nofsx=0
        nofsy=0
        nofsz=0
      endif
c
c       Initialize nfit for slabs before checking on slabs
      if (pipinput .and. ifauto .ne. 0) then
        nfitxyz(indz) = ntotxyz(indz)
        nfitxyzin(indz) = nfitxyz(indz)
      endif
c       
c       Figure out if doing subsets of slabs but not for interactive when
c       auto was already selected
      if (pipinput .or. ifauto .eq. 0) then
        if (ntotxyz(indz).gt.2) then
          if (pipinput) then
            ifdoSlab = 1 -PipGetInteger('LocalSlabs', nfitxyz(indz))
            if (ifdoSlab .gt. 0) then
              if (nfitxyz(indz) .lt. 2 .or. nfitxyz(indz) .gt. npatxyz(indz))
     &            call exitError('NUMBER OF LOCAL SLABS OUT OF ALLOWED RANGE')
              nfitxyzin(indz) = nfitxyz(indz)
            endif
          else if (ifauto .eq. 0) then
            write(*,'(1x,a,a,a,a,a,$)')'0 to fit to all patches in ',
     &          yztext(2-ifflip), ', or 1 to fit to subsets in ',
     &          yztext(2-ifflip),': '
            read(5,*)ifdoSlab
          endif
        endif
      endif

      if(ifauto.ne.0)then
c         
c         set up parameters for automatic finding: make list of possible
c         nxfit, nyfit, nzfit values
c         
        if (ifflip .ne. 0) then
          nfity = min(nfity, nytot)
          call setAutoFits(nxtot, nztot, nytot, ifdoSlab, nfity, ratmin,
     &        ratmax, nfxauto, nfzauto, nfyauto, nauto, limpatch)
        else
          nfitz = min(nfitz, nztot)
          call setAutoFits(nxtot, nytot, nztot, ifdoSlab, nfitz, ratmin,
     &        ratmax, nfxauto, nfyauto, nfzauto, nauto, limpatch)
        endif
        if(nauto.eq.0)then
          write(*,'(/,a,/,a)')'ERROR: FINDWARP - NO FITTING PARAMETERS GIVE '//
     &        'THE REQUIRED RATIO OF',' ERROR: MEASUREMENTS TO UNKNOWNS - '//
     &        'THERE ARE PROBABLY TOO FEW PATCHES'
          call exit(1)
        endif
c         
c         sort the list by size of area in inverted order
c         
        do i=1,nauto-1
          do j=i+1,nauto
            if(nfxauto(i)*nfyauto(i)*nfzauto(i) .lt.
     &          nfxauto(j)*nfyauto(j)*nfzauto(j))then
              itmp=nfxauto(i)
              nfxauto(i)=nfxauto(j)
              nfxauto(j)=itmp
              itmp=nfyauto(i)
              nfyauto(i)=nfyauto(j)
              nfyauto(j)=itmp
              itmp=nfzauto(i)
              nfzauto(i)=nfzauto(j)
              nfzauto(j)=itmp
            endif
          enddo
        enddo
c         write(*,'(3i5)')(i,nfxauto(i),nfyauto(i),nfzauto(i),i=1,nauto)
c         
c         set up for first round and skip to set up this round's patches
c
        iauto = 1
        nfitx=-100
        nfity=-100
        nfitz=-100
        nlocdone=0
        devavmin=10000.
        indTarget = 1
        write(*,112)targetres(1)
112     format(/,'Seeking a warping with mean residual below',f9.3)
        go to 20
      endif
c       
c       Get parameter to control outlier elimination
c
      if (.not.pipinput) then
        write(*,'(1x,a,$)')'1 to enter parameters to control outlier '
     &      //'elimination, 0 not to: '
        read(5,*)ifdiddle
        if(ifdiddle.ne.0)then
          write(*,'(1x,a,f5.2,a,$)')'Maximum fraction of patches to '//
     &        'eliminate (/ for',fracdrop,'): '
          read(5,*)fracdrop
          write(*,'(1x,a,f5.2,a,$)')'Minimum residual needed to do any'
     &        //' elimination (/ for',elimmin,'): '
          read(5,*)elimmin
          write(*,'(1x,a,f6.3,a,$)')'Criterion probability for ' //
     &        'candidates for elimination (/ for',crit,'): '
          read(5,*)crit
          write(*,'(1x,a,f6.3,a,$)')'Criterion probability for enforced'
     &        //' elimination (/ for',critabs,'): '
          read(5,*)critabs
        endif
      endif
c       
c       Initialize for first time in or back after parameter setting
c
      nfitx=-100
      nfity=-100
      nfitz=-100
      nlocdone=0
      if (.not.pipinput) write(*,111)
111   format(/,' Enter 0 for the number of patches in X to loop',
     &    ' back and find best fit ',/,
     &    '  automatically, include a different subset of patches ',
     &    /,'  or specify new outlier elimination parameters',/)
c       
c       Get the input number of local patches unless doing auto
c
20    if (ifauto.eq.0 .and. ifdoSlab.ne.0) then
        if (.not.pipinput) then
          if(nlocdone.eq.0)then
            write(*,'(1x,a,$)')
     &          'Number of local patches for fit in X, Y and Z: '
          else
            write(*,'(1x,a,/,a,$)')
     &          'Number of local patches for fit in X, Y and Z,',
     &          '    or / to redo and save last result: '
          endif
          read(5,*)nfitxin,nfityin,nfitzin
        endif
      elseif (ifauto.eq.0) then
        if (.not.pipinput) then
          if(nlocdone.eq.0)then
            write(*,'(1x,a,a,a,$)')
     &          'Number of local patches for fit in X and ',yztext(1+ifflip),': '
          else
            write(*,'(1x,a,a,a,/,a,$)')
     &          'Number of local patches for fit in X and ',yztext(1+ifflip),',',
     &          '    or / to redo and save last result: '
          endif
          read(5,*)nfitxin,nfitxyzin(indy)
        endif
        nfitxyzin(indz) = ntotxyz(indz)
      else
        nfitxin=nfxauto(iauto)
        nfityin=nfyauto(iauto)
        nfitzin=nfzauto(iauto)
      endif
c       
c       Loop back to earlier parameter entries on a zero entry
c
      if(nfitxin.eq.0) go to 8
      if(nfitxin.ne.nfitx.or.nfityin.ne.nfity.or.nfitzin.ne.nfitz
     &    .or.nlocdone.eq.0)then
c         
c         Set up number of locations to fit and check entries
c
        nlocx=nxtot+1-nfitxin
        nlocy=nytot+1-nfityin
        nlocz=nztot+1-nfitzin
        if(nfitxin.lt.2.or.nfitxyzin(indy).lt.2.or.nlocx.lt.1.or.nlocz.lt.1
     &      .or.nfitxyzin(indz).lt.1.or.nlocy.lt.1)then
          if (ifauto .ne. 0) call exitError(
     &        'IMPROPER NUMBER TO INCLUDE IN FIT')
          print *,'Illegal entry, try again'
          go to 20
        endif
        if(nfitxin*nfityin*nfitzin.gt.idim)then
          if (ifauto .ne. 0) call exitError(
     &        'TOO MANY PATCHES FOR ARRAY SIZES')
          print *,'Too many patches for array sizes, try again'
          go to 20
        endif
        nfitx=nfitxin
        nfity=nfityin
        nfitz=nfitzin
      else
c         
c         Or save data and terminate on a duplicate entry
c
        if(nlocx.gt.1.or.nlocy.gt.1.or.nlocz.gt.1)then
c           
c           Eliminate locations with low determinants.  This is very
c           conservative measure before observed failures were diagonal
c           degeneracies in 2x2 fits
          numLowDet = 0
          do locz=1,nlocz
            do locy=1,nlocy
              do locx=1,nlocx
                ind=indloc(locx,locy,locz)
                if (solved(ind)) then
                  if (abs(determ(asave(1,1,ind))) .lt. 0.01 * detmean) then
                    solved(ind) = .false.
                    numLowDet = numLowDet + 1
                  endif
                endif
              enddo
            enddo
          enddo
c           
          if (numLowDet .gt. 0) write(*,'(/,i4,a)')numLowDet,
     &        ' fits were eliminated due to low matrix determinant'
c
          if(ifauto.ne.0)write(*,*)
          if (pipinput) then
            ierr = PipGetString('InitialTransformFile', filename)
          else
            print *,'Enter name of file with initial '//
     &          'transformation, typically solve.xf',
     &          '   (Return if none)'
            read(*,'(a)')filename
          endif
          if(filename.ne.' ')then
            call dopen(1,filename,'old','f')
            read(1,*)((firstm(i,j),j=1,3),firstd(i),i=1,3)
            close(1)
          else
            do i=1,3
              do j=1,3
                firstm(i,j)=0.
              enddo
              firstm(i,i)=1.
              firstd(i)=0.
            enddo
          endif
c           
          if (PipGetInOutFile('OutputFile', 2,
     &        'Name of file to place warping transformations in', filename).eq.
     &        0) then
            call dopen(1,filename,'new','f')
c           
c             Output new style header to allow missing data
            dxloc = 1.
            dyloc = 1.
            dzloc = 1.
            if (nlocx .gt. 1) dxloc = (cenmax(1) - cenmin(1)) / (nlocx - 1)
            if (nlocy .gt. 1) dyloc = (cenmax(2) - cenmin(2)) / (nlocy - 1)
            if (nlocz .gt. 1) dzloc = (cenmax(3) - cenmin(3)) / (nlocz - 1)
            write(1,104)nlocx,nlocy,nlocz,(cenmin(i),i=1,3),dxloc,dyloc,dzloc
104         format(i5,2i6,3f11.2,3f10.4)
c             
            do locz=1,nlocz
              do locy=1,nlocy
                do locx=1,nlocx
                  ind=indloc(locx,locy,locz)
                  induse=ind
c                   
c                   if this location was solved, combine and invert
                  if(solved(ind))then
                    call xfmult3d(firstm,firstd,asave(1,1,induse),
     &                  dxyzsave(1,induse),atmp,dtmp)
                    call xfinv3d(atmp,dtmp,a,dxyz)
                    write(1,103)(censave(i,ind),i=1,3)
103                 format(3f9.1)
                    write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102                 format(3f10.6,f10.3)
                  endif
                enddo
              enddo
            enddo
            close(1)
          endif
        else
c           
c           Save a single transform if fit to whole area; won't happen if auto
c
          print *,'Enter name of file in which to place single ',
     &        'refining transformation'
          read(5,'(a)')filename
          call dopen(1,filename,'new','f')
          write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
          close(1)
        endif

        call outputPatchRes(resFile, nFileDat, npatx * npaty * npatz,
     &      exists, resSum, numRes, inddrop, ntimes, nlistd, cx, dx, limpatch)

        call exit(0)
      endif
c       
c       Do the fits for real now
c
      devavsum=0.
      devmaxsum=0.
      devmaxmax=0.
      devavmax=0.
      nlistd=0
      ndroptot=0
      nlocdone=0
      numDevSum = 0.
      detmean = 0.
      do i = 1,3
        cenmin(i) = 1.e30
        cenmax(i) = -1.e30
      enddo
      do ind = 1, npatx * npaty * npatz
        numRes(ind) = 0
        resSum(ind) = 0
      enddo
      if (max(nfitx,nfity,nfitz).gt.limaxis) call exitError(
     &    'TOO MANY POINTS IN FIT IN ONE DIMENSION FOR ARRAYS')

      do locz=1,nlocz
        do locy=1,nlocy
          do locx=1,nlocx
            ndat=0
            numZero = 0
            do i=1,3
              xyzsum(i)=0.
            enddo
c             
c             count up number in each row in each dimension and on each 
c             diagonal in the major dimensions
c             
            do i=1,max(nfitx,nfity,nfitz)
              inrowx(i)=0
              inrowy(i)=0
              inrowz(i)=0
            enddo
            nyDiag = nfity
            if (ifflip .eq. 1) nyDiag = nfitz
c             
c             Zero the parts of the arrays corresponding to corners, which
c             won't be tested
            do i = 0, nfitx + nyDiag - 2
              inDiag1(i) = 0
              inDiag2(i) = 0
            enddo
            do lz=locz+nofsz,locz+nofsz+nfitz-1
              do ly=locy+nofsy,locy+nofsy+nfity-1
                do lx=locx+nofsx,locx+nofsx+nfitx-1
                  ifuse=0
                  ind=indpat(lx,ly,lz)
                  if(exists(ind))ifuse=1
                  do i=1,3
                    xyzsum(i)=xyzsum(i)+cx(ind,i)-0.5*nxyz(i)
                  enddo
                  if(ncont.gt.0.and.ifuse.gt.0)then
                    ifuse=0
c                     
c                     find nearest contour in Z and see if patch is inside it
c                     
                    dzmin=100000.
                    do icont=1,ncont
                      dz=abs(cx(ind,indz)-zcont(icont))
                      if(dz.lt.dzmin)then
                        dzmin=dz
                        icmin=icont
                      endif
                    enddo
                    indv=indvert(icmin)
                    if(inside(xvert(indv),yvert(indv),nvert(icmin),
     &                  cx(ind,1),cx(ind,indy))) ifuse=1
                  endif
c                   
                  if(ifuse.gt.0)then
                    ndat=ndat+1
                    if (dx(ind,1) .eq. 0. .and. dx(ind,2) .eq. 0 .and.
     &                  dx(ind,3) .eq. 0) numZero = numZero + 1
                    do j=1,3
c                       
c                       the regression requires coordinates of second volume as
c                       independent variables (columns 1-3), those in first
c                       volume as dependent variables (stored in 5-7), to
c                       obtain transformation to get from second to first
c                       volume cx+dx in second volume matches cx in first
c                       volume
c                       
                      xr(j+4,ndat)=cx(ind,j)-0.5*nxyz(j)
                      xr(j,ndat)=xr(j+4,ndat)+dx(ind,j)
                    enddo
c                     
c                     Solve_wo_outliers uses columns 8-17; save indexi in 18
c                     Add to row counts and to diagonal counts
c                     
                    xr(18,ndat)=ind
                    ix = lx+1-locx-nofsx
                    iy = ly+1-locy-nofsy
                    iz = lz+1-locz-nofsz
                    inrowx(ix) = inrowx(ix) + 1
                    inrowy(iy) = inrowy(iy) + 1
                    inrowz(iz) = inrowz(iz) + 1
                    if (ifflip .eq. 1) iy = iz
                    iz = (ix - iy) + nyDiag - 1
                    inDiag1(iz) = inDiag1(iz) + 1
                    iz = ix + iy - 2
                    inDiag2(iz) = inDiag2(iz) + 1
                  endif
                enddo
              enddo
            enddo
            indlc=indloc(locx,locy,locz)
c             
c             Need regular array of positions, so use the xyzsum to get
c             censave, not the cenloc values from the regression
c             
            debugHere = ifdebug .ne. 0
            do i=1,3
              censave(i,indlc)=xyzsum(i)/(nfitx*nfity*nfitz)
              cenmin(i) = min(cenmin(i), censave(i,indlc))
              cenmax(i) = max(cenmax(i), censave(i,indlc))
              if (debugHere)
     &            debugHere = abs(censave(i,indlc) - debugxyz(i)) .lt. 1.
            enddo
c             
c             solve for this location if there are at least half of the
c             normal number of patches present and if there are guaranteed
c             to be at least 3 patches in a different row from the dominant
c             one, even if the max are dropped from other rows
c             But treat thickness differently: if there are not enough data
c             on another layer, or if there is only one layer being fit,
c             then set the appropriate column as fixed in the fits
c             
            solved(indlc)=ndat.ge.nfitx*nfity*nfitz/2
            maxdrop=nint(fracdrop*ndat)
            icolfix = 0
            do i=1,max(nfitx,nfity,nfitz)
              if(debugHere)print *,'in row',i,':',inrowx(i),inrowy(i),inrowz(i)
              if (ifflip .eq. 1) then
                if(inrowx(i).gt.ndat-3-maxdrop.or.
     &              inrowz(i).gt.ndat-3-maxdrop)solved(indlc)=.false.
                if (inrowy(i).gt.ndat-3-maxdrop .or. nfity.eq.1) icolfix=2
              else
                if(inrowx(i).gt.ndat-3-maxdrop.or.
     &              inrowy(i).gt.ndat-3-maxdrop)solved(indlc)=.false.
                if (inrowz(i).gt.ndat-3-maxdrop .or. nfitz.eq.1) icolfix=3
              endif
            enddo
            do i = 1, nfitx + nyDiag - 3
              if (inDiag1(i) .gt. ndat-3-maxdrop .or.
     &            inDiag2(i) .gt. ndat-3-maxdrop) solved(indlc)=.false.
            enddo
            if(solved(indlc))then
              call solve_wo_outliers(xr,ndat,3,icolfix,maxdrop,crit,
     &            critabs, elimmin,idrop, ndrop, a,dxyz, cenloc,
     &            devavg,devsd, devmax, ipntmax,devxyzmax)
c               
              if (debugHere)then
                do i=1,ndat
                  write(*,'(8f9.2)')(xr(j,i),j=1,7)
                enddo
                print *,'cenloc',(cenloc(i),i=1,3)
                print *,'censave',(censave(i,indlc),i=1,3)
                write(*,'(9f8.3)')((a(i,j),i=1,3),j=1,3)
              endif
c               
c               Accumulate information about dropped points
c
              do i=1,ndrop
                ifon=0
                do j=1,nlistd
                  if(nint(xr(18,idrop(i))).eq.inddrop(j))then
                    ifon=1
                    ntimes(j)=ntimes(j)+1
                    dropsum(j)=dropsum(j)+xr(4,ndat+i-ndrop)
                  endif
                enddo
                if(ifon.eq.0.and.ifon.lt.idim)then
                  nlistd=nlistd+1
                  inddrop(nlistd)=nint(xr(18,idrop(i)))
                  ntimes(nlistd)=1
                  dropsum(nlistd)=xr(4,ndat+i-ndrop)
                endif
              enddo
              ndroptot=ndroptot+ndrop
c               
c               if residual output asked for, accumulate info about all resids
c               
              if (resFile .ne. ' ') then
                do i = 1, ndat
                  ind = nint(xr(18,nint(xr(5,i))))
                  numRes(ind) = numRes(ind) + 1
                  resSum(ind) = resSum(ind) + xr(4,i)
                enddo
              endif
c               
              if (discount .eq. 0. .or. float(numZero) / ndat .le. discount)
     &            then
                devavsum=devavsum+devavg
                devmaxsum=devmaxsum+devmax
                numDevSum = numDevSum + 1
              endif
              devavmax=max(devavmax,devavg)
              devmaxmax=max(devmaxmax,devmax)
c               
c               mark this location as solved and save the solution. 
c               
              if (debugHere) write(*,'(6i4,3f8.1)')indlc,locx,locy,locz,ndat,
     &            ndrop,(dxyz(i),i=1,3)
              do i=1,3
                dxyzsave(i,indlc)=dxyz(i)
                do j=1,3
                  asave(i,j,indlc)=a(i,j)
                enddo
                if (debugHere) write(*,102)(a(i,j),j=1,3),dxyz(i)
              enddo
              nlocdone=nlocdone+1
              detmean = detmean + abs(determ(a))
            elseif (debugHere) then
              print *,'Not solved',ndat
            endif
          enddo
        enddo
      enddo
      detmean = detmean / max(1, nlocdone)
c       
c       check for auto control
c       
      if(ifauto.ne.0)then
        devavavg=10000.
        if(numDevSum.gt.0)devavavg=devavsum/numDevSum
        devavmin=min(devavmin,devavavg)
        devavAuto(iauto) = devavavg
        if(devavavg.le.targetres(indTarget))then
c           
c           done: set nauto to zero to allow printing of results
c           
          nauto=0
          write(*,107)nfitx,nfity,nfitz
107       format(/,'Desired residual achieved with fits to',i3,',',i3,
     &        ', and',i3,' patches in X, Y, Z',/)
        else
          iauto=iauto+1
c           print *,iauto,' Did fit to',nfitx,nfity,nfitz
          if(iauto.gt.nauto)then
c             
c             See if any other criteria have been met and loop back if so
c
            do j = indTarget + 1, numTarget
              write(*,112)targetres(j)
              do i = 1, nauto
                if (devavAuto(i) .le. targetres(j)) then
                  indTarget = j
                  iauto = i
                  nfitx=-100
                  nlocdone=0
                  go to 20
                endif
              enddo
            enddo
c             
c             write patch file if desired on last fit before error message
c
            call outputPatchRes(resFile, nFileDat, npatx * npaty * npatz,
     &          exists, resSum, numRes, inddrop, ntimes, nlistd, cx, dx,
     &          limpatch)
            if (discount .gt. 0. .and. nlocdone .gt. 0 .and. numDevSum .eq. 0)
     &          call exitError('ALL FITS HAD TOO MANY ZERO VECTORS: RAISE '//
     &          '-discount FRACTION OR SET IT TO ZERO')
            write(*,108)devavmin
108         format(/,'ERROR: FINDWARP - FAILED TO FIND A WARPING WITH A ',
     &          'MEAN RESIDUAL BELOW',f9.3)
            call exit(2)
          endif
        endif
      endif

      if(nlistd.gt.0.and.nauto.eq.0)then
        write(*,105)nlistd,ndroptot
105     format(i5,' separate patches eliminated as outliers a total'
     &      ,' of',i6,' times:')
        if (nlistd .le. 10) then
          print *,'    patch position   # of times   mean residual'
          do i=1,nlistd
            write(*,106)(cx(inddrop(i),j),j=1,3),ntimes(i),
     &          dropsum(i)/ntimes(i)
106         format(3f7.0,i8,f12.2)
          enddo
        else
          do i = 1, nlistd
            dropsum(i) = dropsum(i) / ntimes(i)
          enddo
          call summarizeDrops(dropsum, nlistd, 'mean ')
         endif
        write(*,*)
      endif
c       
      if(nlocdone.gt.0.and.nauto.eq.0)then
        devavavg=devavsum/max(1, numDevSum)
        devmaxavg=devmaxsum/max(1, numDevSum)
        write(*,101)devavavg,devavmax,devmaxavg,devmaxmax
101     format('Mean residual has an average of',f8.3,
     &      ' and a maximum of',f8.3,/ ,
     &      'Max  residual has an average of',f8.3,' and a maximum of'
     &      ,f8.3)
        if (discount .gt. 0.) write(*,'(/,a,i7,a,i7,a)')'These averages are '//
     &      'based on',numDevSum,' of', nlocdone,' fits'
      elseif(ifauto.eq.0)then
        print *,'No locations could be solved for'
      endif
      go to 20
      end


      subroutine setAutoFits(nxtot, nztot, nytot, ifdoy, nfity, ratmin, ratmax,
     &    nfxauto, nfzauto, nfyauto, nauto, limauto)
      implicit none
      integer*4 nxtot, nztot, nytot, ifdoy, nfity, nfxauto(*), nfzauto(*)
      integer*4 nfyauto(*), nauto, niny,ix,iz,iy,limauto
      real*4 ratmin, ratmax, ratio, ratioFac
      nauto = 0
      niny = nytot
      if (ifdoy.ne.0) niny = nfity
      ratioFac = 4.0
      if (nytot .eq. 1) ratioFac = 3.0
      do ix = nxtot, 2, -1
        do iz = nztot , 2, -1
          do iy = nytot, niny, -1
            ratio = ix * iz * iy / ratioFac
c           aspect = float(ix)/iz
c           if(aspect.lt.1.)aspect=1./aspect
            if((ix.ne.nxtot.or.iz.ne.nztot) .and. ratio.ge.ratmin .and.
     &          ratio.le.ratmax)then
c               &                 aspect.le.aspectmax)then
              nauto = nauto + 1
              if (nauto .gt. limauto) call exitError(
     &            'TOO MANY POSSIBLE FITS FOR ARRAYS, REDUCE MAX RATIO')
              nfxauto(nauto) = ix
              nfzauto(nauto) = iz
              nfyauto(nauto) = iy
            endif
          enddo
        enddo
      enddo
      return
      end

c         
c         Output new patch file with mean residuals if requested
c
      subroutine outputPatchRes(resFile, nFileDat, npatTot, exists, resSum,
     &    numRes, inddrop, ntimes, nlistd, cx, dx, limpatch)
      implicit none
      character*(*)resFile
      integer*4 nFileDat, npatTot,limpatch,numRes(*), ind, i,inddrop(*),nlistd
      integer*4 ntimes(*)
      real*4 cx(limpatch,3), dx(limpatch,3), resSum(*), dist, dropFrac
      logical exists(*)
      if (resFile .eq. ' ') return
      call dopen(1,resFile,'new','f')
      write(1,'(i7,a)')nFiledat,' positions'
      do ind = 1, npatTot
        if (exists(ind)) then
          dropFrac = 0.
          do i = 1, nlistd
            if (ind .eq. inddrop(i)) then
              dropFrac = float(ntimes(i)) / max(ntimes(i), 1, numRes(ind))
              exit
            endif
          enddo
          dist = resSum(ind) / max(1, numRes(ind))
          write(1, 110) (nint(cx(ind, i)), i = 1, 3), (dx(ind, i), i = 1,3)
     &        , dist, dropFrac
110       format(3i6,3f9.2,f10.2,f7.3)
        endif
      enddo
      close(1)
      return
      end

      real*4 function determ(a)
      implicit none
      real *4 a(3,3)
      determ = a(1,1)*a(2,2)*a(3,3)+a(2,1)*a(3,2)*a(1,3)+a(1,2)*a(2,3)*a(3,1)-
     &    a(1,3)*a(2,2)*a(3,1)-a(2,1)*a(1,2)*a(3,3)-a(1,1)*a(3,2)*a(2,3)
      return
      end

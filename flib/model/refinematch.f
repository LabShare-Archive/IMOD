*       * * * * * REFINEMATCH * * * * * *
c       
c       REFINEMATCH will solve for a general 3-dimensional linear
c       transformation to align two volumes to each other.  It performs
c       multiple linear regression on the  displacements between the volumes
c       determined at a matrix of positions.  The displacements must be
c       contained in a file with the following form:
c       
c       Number of displacements
c       One line for each displacement consisting of the X, Y, and Z
c       .  coordinates in the first volume, then the displacements in X, Y
c       .  and Z involved in moving from the first to the second volume
c       
c       See man page for details
c
c       David Mastronarde, 1995
c       
c       $Id$
c       
      implicit none
      include 'statsize.inc'
      integer idim,limvert
      parameter (idim=100000,limvert=100000)
      real*4 xr(msiz,idim),ccc(idim)
      real*4 cx(3),dx(3),a(3,3),dxyz(3),devxyz(3)
      real*4 devxyzmax(3),cenloc(3),cxlast(3),freinp(20)
      integer*4 nxyz(3),idrop(idim)
      logical inside,onelayer(3),haveCCC
      real*4 xvert(limvert),yvert(limvert),zcont(idim)
      integer*4 indvert(idim),nvert(idim)
      character*320 filename
c       
      integer*4 ncont,ierr,ifflip,i,indy,indz,indcur,iobj,ndat,nfill,ninp
      real*4 fracdrop,cccres
      integer*4 ifuse,icont,icmin,j,ind,maxdrop,ndrop,ipntmax,ip,ipt
      real*4 dzmin,crit,elimmin,critabs,devavg,devsd,devmax,stoplim,dz
      integer*4 icolfix

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger
      integer*4 PipGetString,PipGetFloat,PipgetTwoFloats
      integer*4 PipGetInOutFile
c      
c       fallbacks from ../../manpages/autodoc2man -2 2  refinematch
c       
      integer numOptions
      parameter (numOptions = 11)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'patch:PatchFile:FN:@region:RegionModel:FN:@'//
     &    'volume:VolumeOrSizeXYZ:FN:@output:OutputFile:FN:@'//
     &    'residual:ResidualPatchOutput:FN:@limit:MeanResidualLimit:F:@'//
     &    'maxfrac:MaxFractionToDrop:F:@minresid:MinResidualToDrop:F:@'//
     &    'prob:CriterionProbabilities:FP:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'
c       
      fracdrop = 0.1
      crit=0.01
      elimmin=0.5
      critabs=0.002
      haveCCC = .false.
c
      call PipReadOrParseOptions(options, numOptions, 'refinematch',
     &    'ERROR: REFINEMATCH - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
C       
C       Open patch file
C       
      if (PipGetInOutFile('PatchFile', 1,
     &    'Name of file with correlation positions and results', filename)
     &    .ne. 0) call exiterror('NO INPUT PATCH FILE SPECIFIED')
      call dopen(1,filename,'old','f')

      if (.not. pipinput) print *,'Enter file name or NX, NY, NZ of tomogram',
     &    ' being matched to'
      call get_nxyz(pipinput, 'VolumeOrSizeXYZ', 'REFINEMATCH', 1,nxyz)

      if (pipinput) then
        filename = ' '
        ierr = PipGetString('RegionModel', filename)
        ierr = PipGetFloat('MaxFractionToDrop', fracdrop)
        ierr = PipGetFloat('MinResidualToDrop', elimmin)
        ierr = PipGetTwoFloats('CriterionProbabilities', crit, critabs)
        if (PipGetFloat('MeanResidualLimit', stoplim) .gt. 0) call exitError(
     &      'YOU MUST ENTER A MEAN RESIDUAL LIMIT')
      else
        write(*,'(1x,a,/,a,$)')
     &      'Enter name of model file with contour enclosing area to '
     &      //'use,',' or Return to use all patches: '
        read(*,'(a)')filename
      endif

      ncont = 0
      if (filename .ne. ' ') then
        call get_region_contours(filename, 'REFINEMATCH', xvert, yvert, nvert,
     &    indvert, zcont, ncont, ifflip, idim, limvert, 0)
      else
        ifflip = 0
        if (nxyz(2) .lt. nxyz(3)) ifflip = 1
      endif
      indy = 2
      if (ifflip .ne. 0) indy = 3
      indz = 5 - indy
c       
      read(1,*)ndat
      if(ndat.gt.idim)call exitError('TOO MANY POINTS FOR DATA ARRAYS')

      do j=1,3
        onelayer(j)=.true.
        cxlast(j) = 0.
      enddo
      nfill=0
      do i=1,ndat
        ccc(i) = 0.
c         
c         these are center coordinates and location of the second volume
c         relative to the first volume
c         
        read(1,'(a)')filename
        call frefor(filename, freinp, ninp)
        do j = 1, 3
          cx(j) = freinp(j)
          dx(j) = freinp(j + 3)
c        read(1,*)(cx(j),j=1,3),(dx(j),j=1,3)
c        do j = 1,3
          if (i.gt.1 .and. cx(j).ne.cxlast(j)) onelayer(j)=.false.
          cxlast(j) = cx(j)
        enddo
        if (ninp .gt. 6) then
          haveCCC = .true.
          ccc(i) = freinp(7)
        endif

        ifuse=1
        if(ncont.gt.0)then
          ifuse=0
c           
c           find nearest contour in Z and see if patch is inside it
c           
          dzmin=100000.
          do icont=1,ncont
            dz=abs(cx(indz)-zcont(icont))
            if(dz.lt.dzmin)then
              dzmin=dz
              icmin=icont
            endif
          enddo
          ind=indvert(icmin)
          if(inside(xvert(ind),yvert(ind),nvert(icmin),cx(1),cx(indy)))
     &        ifuse=1
        endif
c         
        if(ifuse.gt.0)then
          nfill=nfill+1
c	    write(*,'(3f6.0)')cx(1),cx(3),cx(2)
          do j=1,3
c	      
c             the regression requires coordinates of second volume as
c             independent variables (columns 1-3), those in first volume
c             as dependent variables (stored in 5-7), to obtain
c             transformation to get from second to first volume
c             cx+dx in second volume matches cx in first volume
c             
            xr(j+4,nfill)=cx(j)-0.5*nxyz(j)
            xr(j,nfill)=xr(j+4,nfill)+dx(j)
          enddo
        endif
      enddo

      close(1)
      if (ndat .lt. 4) call exitError('TOO FEW DATA POINTS FOR FITTING')
      ndat=nfill
      print *,ndat,' data points will be used for fit'
c       
      if (.not.pipinput) then
        write(*,'(1x,a,$)')'Mean residual above which to STOP and '//
     &      'exit with an error: '
        read(5,*)stoplim
      endif
c       
      icolfix = 0
      do i = 1,3
        if (icolfix.ne.0 .and. onelayer(i)) call exitError(
     &      'CANNOT FIT TO PATCHES THAT EXTEND IN ONLY ONE DIMENSION')
        if (onelayer(i)) icolfix = i
      enddo
      if (icolfix.gt.0)print *,'There is only one layer of patches',
     &    ' in the ',char(ichar('W')+icolfix),' dimension'
c       
      maxdrop=nint(fracdrop*ndat)
      call solve_wo_outliers(xr,ndat,3,icolfix,maxdrop,crit,critabs,
     &    elimmin, idrop,ndrop, a,dxyz,cenloc, devavg,devsd,devmax,
     &    ipntmax, devxyzmax)
c       
      if(ndrop.ne.0)then
        write(*,104)ndrop
104     format(i3,' patches dropped by outlier elimination:')
        if (ndrop .le. 10) then
          print *,'    patch position     residual'
          do i = 1, ndrop
            write(*,106)(xr(j, ndat + i - ndrop),j=1,4)
106         format(3f7.0,f9.2)
          enddo
        else
          do i = 1, ndrop
            zcont(i) = xr(4, ndat + i - ndrop)
          enddo
          call summarizeDrops(zcont, ndrop, ' ')
        endif
      endif
c       
      write(*,101)devavg,devmax
101   format(/,' Mean residual',f8.3,',  maximum',f8.3,/)
c       
      print *,'Refining transformation:'
      write(*,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102   format(3f10.6,f10.3)
c
      if (pipinput) then
c
c         For patch residual output, painfully recompute the original data!
c         First make a proper index from original to ordered rows
        do i = 1, ndat
          xr(6, nint(xr(5,i))) = i
        enddo
        if (PipGetString('ResidualPatchOutput', filename) .eq. 0) then
          call dopen(1, filename, 'new', 'f')
          write(1,'(i7,a)')ndat,' positions'
          do i = 1, ndat
            write(1, 110) (nint(xr(j+11, i) + 0.5 * nxyz(j)), j = 1,3),
     &          ((xr(j,i)-xr(j+4,i)),j=8,10), xr(4, nint(xr(6,i)))
110         format(3i6,3f9.2,f10.2)
          enddo
          close(1)
        endif
c         
c         For reduced vectors, put out the residual vector stored in cols 15-17
c         or ordered data
        if (PipGetString('ReducedVectorOutput', filename) .eq. 0) then
          call dopen(1, filename, 'new', 'f')
          write(1,'(i7,a)')ndat,' positions'
          do i = 1, ndat
            ind = nint(xr(6,i))
            cccres = xr(4,ind)
            if (haveCCC) cccres = ccc(i)
            write(1, 111) (nint(xr(j+11, i) + 0.5 * nxyz(j)), j = 1,3),
     &          (xr(j,ind),j=15,17),cccres
111         format(3i6,3f9.2,f12.4)
          enddo
          close(1)
        endif
        filename = ' '
        ierr = PipGetString('OutputFile', filename)
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
      if(devavg.gt.stoplim)then
        write(*,'(/,a)')'REFINEMATCH - MEAN RESIDUAL TOO HIGH;'//
     &      ' EITHER RAISE THE LIMIT OR USE WARPING'
        call exit(2)
      endif
      call exit(0)
      end

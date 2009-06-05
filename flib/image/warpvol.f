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
c       Log at end
c
      implicit none
      include 'rotmatwarp.inc'
      integer maxloc
      parameter (maxloc=200000)
      real*4 cell(6),dxyzin(3)
      logical*4 solved(maxloc),solvtmp(maxloc)
      equivalence (array, solved), (array(maxloc+1), solvtmp)
      integer*4 mxyzin(3)
      real*4 mfor(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
      real*4 angles(3),tiltold(3),tiltnew(3),orig(3),xtmp(3)
      real*4 atmp1(3,3),atmp2(3,3),dtmp1(3),dtmp2(3),delta(3)
      real*4 aloc(3,3,maxloc),dloc(3,maxloc)
      real*4 ofsin(3,limout),amx(3,3,limout)
      integer*4 izsec(4)
      integer*4 inmin(3),inmax(3)
      real*4 freinp(10)
c       
      character*320 filein,fileout,fileinv,tempdir,tempext,patchfile,fillfile
c       
c	DNM 3/8/01: initialize the time in case time(tim) doesn't work
c       
      character dat*9,tim*8/'00:00:00'/
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      integer*4 nlocx,ninp,nlocy,nlocz,nloctot,i,j,kti,ja,ix,iy,ierr
      real*4 xlocst,ylocst,xlocmax,ylocmax,zlocst,zlocmax,cenlocx,cenlocy
      real*4 cenlocz,dxloc,dyloc,dzloc,dmin,dmax,tsum,devmx,xcen,ycen,zcen
      integer*4 idimout,ind,izcube,ixcube,iycube,ifx,ify,ifz,ival,ifempty
      integer*4 iz,ixlim,iylim,izlim,ixp,iyp,ixpp1,ixpm1,iypp1,iypm1,numDone
      real*4 xofsout,xp,yp,zp,bval,dx,dy,dz,v2,v4,v6,v5,v8,vu,vd,vmin,vmax
      real*4 a,b,c,d,e,f,tmin,tmax,tmean,dmean,dminin,dmaxin,d11,d12,d21,d22
      integer*4 iunit,longint,l,izp,izpp1,izpm1,nLinesOut,interpOrder, nExtra
      integer*4 newExtra,lForErr, newlocx, newlocy, newlocz
      integer*4 nxloAdd, nyloAdd,nzloAdd,nxhiAdd, nyhiAdd,nzhiAdd
      real*4 baseInt,xcubelo,xcubehi,ycubelo,ycubehi,zcubelo,zcubehi
      integer*4 ixlo,ixhi,iylo,iyhi,izlo,izhi
      real*4 dxUse,dyUse,dzUse, xlUse,ylUse,zlUse
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger, PipGetFloatArray, PipNumberOfEntries
      integer*4 PipGetString,PipGetThreeIntegers,PipGetThreeFloats
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
      baseInt = 0.5
      tempdir = ' '
      tempext='wrp      1'
      call time(tim)
      call date(dat)
      patchfile = ' '
      fillfile = ' '
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
      ierr = PipGetString('PatchOutputFile', patchfile)
      ierr = PipGetString('FilledInOutputFile', fillfile)

      if (PipGetInOutFile('OutputFile', 2, 'Name of output file', fileout)
     &    .ne. 0 .and. patchfile .eq. ' ' .and. fillfile .eq. ' ')
     &    call exiterror('NO OUTPUT FILE SPECIFIED')
c       
      if (pipinput) then
        ierr = PipGetString('TemporaryDirectory', tempdir)
        ierr = PipGetInteger('InterpolationOrder', interpOrder)
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
          xlocst = freinp(4)
          ylocst = freinp(5)
          zlocst = freinp(6)
          dxloc = freinp(7)
          dyloc = freinp(8)
          dzloc = freinp(9)
        endif
      endif
      nloctot=nlocx*nlocz*nlocy
      if(nloctot.gt.maxloc)call exiterror(
     &    'TOO MANY TRANSFORMATIONS TO FIT IN ARRAYS')
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
          read(1,*,err=98,end=98)((aloc(i,j,l),j=1,3),dloc(i,l),i=1,3)
          solved(l) = .true.
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
      else
c         
c         Know min and interval, so read each entry, find where it goes in
c         array and put it there, then 
        do l=1,nloctot
          solved(l) = .false.
        enddo
        lForErr = 1
10      read(1,*,err=98,end=12)cenlocx,cenlocy,cenlocz
        ix = min(nlocx, max(1, nint((cenlocx - xlocst) / dxloc + 1.)))
        iy = min(nlocy, max(1, nint((cenlocy - ylocst) / dyloc + 1.)))
        iz = min(nlocz, max(1, nint((cenlocz - zlocst) / dzloc + 1.)))
        l = ix + (iy - 1) * nlocx + (iz - 1) * nlocx * nlocy
        read(1,*,err=98,end=98)((aloc(i,j,l),j=1,3),dloc(i,l),i=1,3)
        solved(l) = .true.
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
      if (nxloAdd + nxhiAdd + nyloAdd + nyhiAdd + nzloAdd + nzhiAdd .gt. 0
     &    .and. newlocx * newlocy * newlocz .le. maxloc)then
        do i = 1, nloctot
          solvtmp(i) = solved(i)
        enddo
        nloctot = newlocx * newlocy * newlocz
        do i = 1, nloctot
          solved(i) = .false.
        enddo
        call shiftTransforms(aloc, dloc, solvtmp, nlocx, nlocy, nlocz,
     &      aloc, dloc, solved, newlocx, newlocy, newlocz, nxloAdd, nyloAdd,
     &      nzloAdd)
        xlocst = xlocst - nxloAdd * dxloc
        ylocst = ylocst - nyloAdd * dyloc
        zlocst = zlocst - nzloAdd * dzloc
        nlocx = newlocx
        nlocy = newlocy
        nlocz = newlocz
      endif
c       
c       Fill in missing transforms by extrapolation amd weighted averaging
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
c       find maximum extent in input volume occupied by a back-transformed
c       unit cube in output volume
c       
      devmx=0.
      do ja=1,nloctot
        do ix=-1,1,2
          do iy=-1,1,2
            do i=1,3
              devmx=max(devmx,abs(aloc(i,1,ja)*ix+aloc(i,2,ja)*iy+
     &            aloc(i,3,ja)))
            enddo
          enddo
        enddo
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
c       Get provisional setup of cubes then find actual limits of input
c       cubes with this setup - loop until no new extra pixels needed
c       
      nExtra = 0
      newExtra = 1
      do while (newExtra .gt. 0)

        call setup_cubes_scratch(devmx, nExtra, ' ', tempdir, tempext, tim)
c         
        newExtra = 0
        do izcube=1,ncubes(3)
          call cubeTestLimits(ixyzcube(3,izcube), nxyzcube(3,izcube), czout,
     &        nlocz, zlocst, dzloc, zcubelo, zcubehi, izlo, izhi, zlUse, dzUse)
          do ixcube=1,ncubes(1)
            call cubeTestLimits(ixyzcube(1,ixcube), nxyzcube(1,ixcube), cxout,
     &          nlocx, xlocst, dxloc, xcubelo, xcubehi, ixlo, ixhi,xlUse,dxUse)
            do iycube=1,ncubes(2)
c               
c               back-transform the faces of the output cube at the corners and
c               at positions corresponding to transforms to
c               find the limiting index coordinates of the input cube
c               
              do i=1,3
                inmin(i)=100000
                inmax(i)=-100000
              enddo
              call cubeTestLimits(ixyzcube(2,iycube), nxyzcube(2,iycube),cyout,
     &            nlocy, ylocst, dyloc, ycubelo,ycubehi,iylo,iyhi,ylUse, dyUse)
              do ifx=ixlo, ixhi
                do ify=iylo, iyhi
                  do ifz=izlo, izhi
                    if (ifx .eq. ixlo .or. ifx .eq. ixhi .or. ify .eq. iylo.or.
     &                  ify .eq. iyhi .or. ifz .eq. izlo .or. ifz .eq. izhi)
     &                  then
                      xcen = max(xcubelo, min(xcubehi, xlUse + ifx * dxUse))
                      ycen = max(ycubelo, min(ycubehi, ylUse + ify * dyUse))
                      zcen = max(zcubelo, min(zcubehi, zlUse + ifz * dzUse))
                      call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &                    zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &                    zcen,minv,cxyzin) 
                      do i=1,3
                        ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &                      minv(i,3)*zcen+cxyzin(i)+nxyzin(i)/2)
                        inmin(i)=max(0,min(inmin(i),ival-2))
                        inmax(i)=min(nxyzin(i)-1,max(inmax(i),ival+2))
c                         
c                         See if any extra pixels are needed in input
c                       
c                         if (newExtra .lt.inmax(i) + 1-inmin(i)-inpdim) then
c                          print *,ixcube,iycube,izcube,xcen,ycen,zcen,
c     &                        inmax(i) + 1 - inmin(i) -
c     &                        inpdim,i, ival, inmin(i), inmax(i)
c                          print *,minv(i,1),minv(i,2),minv(i,3),cxyzin(i)
c                        endif
                        newExtra = max(newExtra, inmax(i) + 1 - inmin(i) -
     &                      inpdim)
                      enddo
                    endif
                  enddo
                enddo
              enddo
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
      call setup_cubes_scratch(devmx, nExtra, filein, tempdir, tempext, tim)
c       
      call imopen(6,fileout,'NEW')
c       
      call icrhdr(6,nxyzout,nxyzout,mode,title,0)
      call ialcel(6,cell)
      call itrlab(6,5)
c       
c       7/7/00 CER: remove the encodes
c       
c       ENCODE(80,302,TITLE)dat,tim
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
      do izcube=1,ncubes(3)
c         
c         initialize files and counters
c         
        do i=1,4
          call imposn(i,0,0)
          izsec(i)=0
        enddo
c         
c         loop on the cubes in the layer
c         
        call cubeTestLimits(ixyzcube(3,izcube), nxyzcube(3,izcube), czout,
     &      nlocz, zlocst, dzloc, zcubelo, zcubehi, izlo, izhi, zlUse, dzUse)
        do ixcube=1,ncubes(1)
          call cubeTestLimits(ixyzcube(1,ixcube), nxyzcube(1,ixcube), cxout,
     &        nlocx, xlocst, dxloc, xcubelo, xcubehi, ixlo, ixhi,xlUse,dxUse)
          do iycube=1,ncubes(2)
            call cubeTestLimits(ixyzcube(2,iycube), nxyzcube(2,iycube),cyout,
     &          nlocy, ylocst, dyloc, ycubelo,ycubehi,iylo,iyhi,ylUse, dyUse)
c             
c             back-transform the faces of the output cube to
c             find the limiting index coordinates of the input cube
c             
            do i=1,3
              inmin(i)=100000
              inmax(i)=-100000
            enddo
            do ifx=ixlo, ixhi
              do ify=iylo, iyhi
                do ifz=izlo, izhi
                  if (ifx .eq. ixlo .or. ifx .eq. ixhi .or. ify .eq. iylo.or.
     &                ify .eq. iyhi .or. ifz .eq. izlo .or. ifz .eq. izhi)
     &                then
                    xcen = max(xcubelo, min(xcubehi, xlUse + ifx * dxUse))
                    ycen = max(ycubelo, min(ycubehi, ylUse + ify * dyUse))
                    zcen = max(zcubelo, min(zcubehi, zlUse + ifz * dzUse))
                    call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &                  zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &                  zcen,minv,cxyzin) 
                    do i=1,3
                      ival=nint(minv(i,1)*xcen+minv(i,2)*ycen+
     &                    minv(i,3)*zcen+cxyzin(i)+nxyzin(i)/2)
                      inmin(i)=max(0,min(inmin(i),ival-2))
                      inmax(i)=min(nxyzin(i)-1,max(inmax(i),ival+2))
                    enddo
                  endif
                enddo
              enddo
            enddo
            ifempty=0
            do i=1,3
              if(inmin(i).gt.inmax(i))ifempty=1
            enddo
c             
c             load the input cube
c             
            if(ifempty.eq.0)then
c              print *,ixcube,iycube,izcube,inmin(3),inmax(3),
c               inmax(3)+1-inmin(3)
              do iz=inmin(3),inmax(3)
                call imposn(5,iz,0)
c                write(*,'(10i5)')ixcube,iycube,izcube,iz,iz+1-inmin(3)
c     &              ,inmin(1), inmax(1),inmin(2),inmax(2)
c                call flush(6)
                call irdpas(5,array(1,1,iz+1-inmin(3)),inpdim,inpdim,
     &              inmin(1),inmax(1),inmin(2),inmax(2),*99)
              enddo
            endif
c             
c             prepare offsets and limits
c             
            xofsout=ixyzcube(1,ixcube)-1-cxout
c	      xofsin=cxin+1-inmin(1)
c	      yofsin=cyin+1-inmin(2)
c	      zofsin=czin+1-inmin(3)
            ixlim=inmax(1)+1-inmin(1)
            iylim=inmax(2)+1-inmin(2)
            izlim=inmax(3)+1-inmin(3)
c             
c             loop over the output cube, doing and saving one section at a
c             time
c             
c             
            do iz=1,nxyzcube(3,izcube)
              zcen=ixyzcube(3,izcube)+iz-1-czout
c               
c               if only one position in Y, get matrices for each X position
c               
              if(nlocy.eq.1)then
                do ix=1,nxyzcube(1,ixcube)
                  xcen=ix+xofsout
                  call interpinv(aloc,dloc,xlocst,dxloc,ylocst,dyloc,
     &                zlocst,dzloc, nlocx, nlocy, nlocz, xcen,ycen,
     &                zcen, amx(1,1,ix),ofsin(1,ix))
                  do i=1,3
                    ofsin(i,ix)=ofsin(i,ix)+1+nxyzin(i)/2-inmin(i)
                  enddo
                enddo
              endif
c               
              do iy=1,nxyzcube(2,iycube)
                ycen=ixyzcube(2,iycube)+iy-1-cyout
                if(ifempty.eq.0)then 
                  do ix=1,nxyzcube(1,ixcube)
                    xcen=ix+xofsout
                    if(nlocy.gt.1)then
                      call interpinv(aloc,dloc,xlocst,dxloc,ylocst,
     &                    dyloc, zlocst,dzloc, nlocx, nlocy, nlocz,
     &                    xcen,ycen, zcen, amx(1,1,ix),ofsin(1,ix))
                      do i=1,3
                        ofsin(i,ix)=ofsin(i,ix)+1+nxyzin(i)/2-inmin(i)
                      enddo
                    endif
c                     
c                     get indices in array of input data
c                     
                    xp=amx(1,1,ix)*xcen+amx(1,2,ix)*ycen+
     &                  amx(1,3,ix)*zcen+ ofsin(1,ix)
                    yp=amx(2,1,ix)*xcen+amx(2,2,ix)*ycen+
     &                  amx(2,3,ix)*zcen+ ofsin(2,ix)
                    zp=amx(3,1,ix)*xcen+amx(3,2,ix)*ycen+
     &                  amx(3,3,ix)*zcen+ ofsin(3,ix)
                    bval = DMEANIN
c                     
c                     do generalized evaluation of whether pixel is doable
c                     
                    ixp=int(xp + baseInt)
                    iyp=int(yp + baseInt)
                    izp=int(zp + baseInt)
                    IF (IXP.GE.1 .AND. IXP.Le.IXLIM .AND.
     &                  IYP.GE.1 .AND. IYP.Le.IYLIM .AND.
     &                  IZP.GE.1 .AND. IZP.Le.IZLIM) THEN
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
C                         Set up terms for quadratic interpolation with
c                         higher-order terms omitted
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
                        bval = (a*dx+c)*dx + (b*dy+d)*dy
     &                      + (e*dz+f)*dz + v5
                        if(bval.gt.vmax)bval=vmax
                        if(bval.lt.vmin)bval=vmin
                      else
C			  
C                         Set up terms for linear interpolation
C                         
                        d11 = (1. - dx) * (1. - dy)
                        d12 = (1. - dx) * dy
                        d21 = dx * (1. - dy)
                        d22 = dx * dy
                        bval = (1. - dz) * (d11 * array(ixp, iyp, izp)
     &                      + d12 * array(ixp, iypp1, izp)
     &                      + d21 * array(ixpp1, iyp, izp)
     &                      + d22 * array(ixpp1, iypp1, izp)) +
     &                      dz * (d11 * array(ixp, iyp, izpp1)
     &                      + d12 * array(ixp, iypp1, izpp1)
     &                      + d21 * array(ixpp1, iyp, izpp1)
     &                      + d22 * array(ixpp1, iypp1, izpp1))
                      endif
                    endif
                    brray(ix,iy)=bval
                  enddo
                else
                  do ix=1,nxyzcube(1,ixcube)
                    brray(ix,iy)=dmeanin
                  enddo
                endif
              enddo
              iunit=ifile(ixcube,iycube)
              call irepak(brray,brray,limout,limout,
     &            0,nxyzcube(1,ixcube)-1,0,nxyzcube(2,iycube)-1)
              call iwrsec(iunit,brray)
              izinfile(ixcube + (iycube-1) * ncubes(1) +
     &            (iz-1) * ncubes(1) * ncubes(2)) = izsec(iunit)
              izsec(iunit)=izsec(iunit)+1
            enddo
c	      print *,ixcube,iycube,izcube
            numDone = numDone + 1
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
      dmean=tsum/nzout
      call iwrhdr(6,title,1,dmin,dmax,dmean)
      do i=1,6
        call imclose(i)
      enddo
      call exit(0)
98    write(*,'(/,a,i7)')'ERROR: WARPVOL - READING WARP FILE AT POSITION',
     &    lForErr
      call exit(1)
99    call exiterror('READING IMAGE FILE')
      end

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
      x=1.
      if(nlocx.gt.0)then
        x=1.+(xcen-xlocst)/dxloc
        x=min(float(nlocx),max(1.,x))
      endif
      ix=x
      ix1=min(ix+1,nlocx)
      fx1=x-ix
      fx=1.-fx1
c       
      y=1.
      if(nlocy.gt.0)then
        y=1.+(ycen-ylocst)/dyloc
        y=min(float(nlocy),max(1.,y))
      endif
      iy=y
      iy1=min(iy+1,nlocy)
      fy1=y-iy
      fy=1.-fy1
c       
      z=1.
      if(nlocz.gt.0)then
        z=1.+(zcen-zlocst)/dzloc
        z=min(float(nlocz),max(1.,z))
      endif
      iz=z
      iz1=min(iz+1,nlocz)
      fz1=z-iz
      fz=1.-fz1
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

c       Fills in regular array of transforms by copying from the nearest
c       transform to each missing one.
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
c              write(*,'(a,3i4,a,3i4,f8.2)')'Filling in',ix,iy,iz,'  nearest',
c     &            minx,miny,minz,sqrt(dmin)
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
c              write(*,'(3i6)')jxmin,jxmax,jymin,jymax,jzmin,jzmax
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
c                        write(*,'(a,3i4,a,f7.1,i4,f12.8)')'Adding in',jx,jy,
c     &                        jz,' angle, dir', angle, indDom, 1./dist
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
c              write (*,'(a,f12.8)')'wsum=',wsum
              do jx = 1, 3
                dloc(jx,ix,iy,iz) = dloc(jx,ix,iy,iz) / wsum
                do jy = 1,3
                  aloc(jx,jy,ix,iy,iz) = aloc(jx,jy,ix,iy,iz) / wsum
                enddo
c                write(*,'(3f9.5,f9.2)')(aloc(jx,jy,ix,iy,iz),jy = 1,3),
c     &              dloc(jx,ix,iy,iz)
              enddo
            endif
          enddo
        enddo
      enddo
      return
      end

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

c       
c       $Log$
c       Revision 3.16  2009/06/05 19:36:29  mast
c       Implemented extrapolation by weighted average of nearest transforms
c       Assessed input limits for cube using all grid points of transform as well
c       as corner points
c
c       Revision 3.15  2009/03/31 23:43:54  mast
c       Give position number in error message when reading file
c
c       Revision 3.14  2008/12/31 21:34:34  mast
c       Added same size option
c
c       Revision 3.13  2007/11/18 04:53:46  mast
c       Increased filename limits to 320
c
c       Revision 3.12  2007/08/30 20:13:38  mast
c       Change izinfile from i*2 3d to i*4 1D array
c
c       Revision 3.11  2007/04/07 22:16:25  mast
c       Redimensioned and allowed for patchy warping file (not tested yet!)
c
c       Revision 3.10  2006/06/01 14:16:48  mast
c       Fixed bug created by deferring opening of output file, switched to
c       exiterror
c
c       Revision 3.9  2006/02/27 05:25:05  mast
c       Defer opening output file until all errors are assessed
c
c       Revision 3.8  2006/01/23 19:31:22  mast
c       Increased format for status output from i4 to i6
c	
c       Revision 3.7  2005/03/01 00:01:41  mast
c       Needed to iterate finding that extra pixels were needed for input
c	
c       Revision 3.6  2004/11/10 02:05:18  mast
c       Do two passes of allocating data into cubes to avoid overflow, and
c       also sample positions along edges of cubes to determine real range
c       needed for loading data
c	
c       Revision 3.5  2004/07/24 17:35:38  mast
c       Added progress output
c	
c       Revision 3.4  2003/10/10 20:37:06  mast
c       Changed to use subroutines in rotmatwarpsubs.f and include file.
c       Converted to PIP/autodoc input and added linear interpolation option.
c	
c       Revision 3.3  2003/10/02 19:59:05  mast
c       Changed method of writing sections to avoid having to fit output
c       section into array all at once.
c       Increased array size and put big array in common
c	
c       Revision 3.2  2003/03/14 22:54:09  mast
c       Standardized error outout and implemented implicit none
c	
c       David Mastronarde, 11/15/96; modified for 3-D matrix of
c       transformations, 7/23/97
c       DNM 2/26/01: add temporary directory entry and semi-unique filenames
c       DNM 11/6/01: fixed problem with output array size not being respected

************BOXAVG**************
*       
*       BOXAVG will average images at points specified in a model file.
c       
c       See man page for details
c
c       $Id$
c       Log at end of file
c
c       David Mastronarde 4/26/89
*       
      implicit none 
      include 'smallmodel.inc'
      integer idim,idimbox,limpnt,limpcl,limsec,limobj
      parameter (idim=8200*8200,idimbox=512*512)
      parameter (limpnt=200000,limpcl=50000,limsec=10000,limobj=10000)
      integer*4 NX,NY,NZ, nx3, ny3
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3),NXYZ2(3),MXYZ2(3)
      real*4 ARRAY(idim),BRRAY(idimbox),TITLE(20),CELL2(6)
      real*4 crray(idimbox),drray(idimbox)
      common /bigarr/array,brray,crray,drray
C       
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      CHARACTER*160 FILIN,FILOUT,filpoint,plfile
      character*9 dat
      character*8 tim
      character*80 titlech
C       
      EQUIVALENCE (NX,NXYZ),(ny,nxyz(2)),(nz,nxyz(3))
      EQUIVALENCE (NX3,NXYZ2),(ny3,nxyz2(2))
C       
      real*4 px(limpnt),py(limpnt)
      integer*4 ipz(limpnt),listsect(limsec),iobjUse(limobj),iobjFlags(limobj)
      logical*4 zeromean
      character*1024 listString
      integer*4 npass, linear, mode, newmode, ierr, ixlo, iylo, ixhi, iyhi
      integer*4 nxbox, nybox,npclist, nobjUse, iffill, iobj, imodobj,imodcont
      integer*4 ifuse,i,ibase,ipt,ipnt, ixtm,iytm, iztm, nzlist, j, notonlist
      real*4 DMIN2,DMAX2,DMEAN2, delta(3), dxbest, dybest
      integer*4 list, izpnt, ipass, nadded, margin, maxshift, iz, jpoint
      integer*4 kx, ky, kz, ixnew, iynew, ifsplit, npnts, kti, ix, iy
      real*4 amat(2,2), edge, bias, fill, DMIN,DMAX,DMEAN
      integer*4 ipxtmp, ipytmp, nobjTot
      integer*4 getimodflags,getImodObjSize
      logical readSmallMod, getModelObjectRange
      real*8 sliceEdgeMean
      DATA NXYZST/0,0,0/

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetFloat,PipGetLogical
      integer*4 PipGetInOutFile,PipGetBoolean, PipGetTwoIntegers
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  boxavg
c
      integer numOptions
      parameter (numOptions = 15)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'image:InputImageFile:FN:@points:PointModelFile:FN:@'//
     &    'output:OutputFile:FN:@:PieceListFile:FN:@'//
     &    'objects:ObjectsToAverage:LI:@box:BoxSizeXandY:IP:@'//
     &    'size:SizeOfOutputXandY:IP:@mode:ModeOfOutput:I:@'//
     &    'passes:PassesThroughData:I:@fill:FillValue:F:@'//
     &    'split:SplitIntoCorners:B:@zero:ZeroMean:B:@'//
     &    'linear:LinearInterpolation:B:@param:ParameterFile:PF:@help:usage:B:'
c
c       Some defaults and initializations
      plfile = ' '
      npass = 2
      linear = 0
      zeromean = .false.
      amat(1,1) = 1.
      amat(1,2) = 0.
      amat(2,1) = 0.
      amat(2,2) = 1.
      maxshift = 3
      ifsplit = 0
c
c       PIP startup
      call PipReadOrParseOptions(options, numOptions, 'boxavg',
     &    'ERROR: BOXAVG - ', .false., 4, 2, 1, numOptArg, numNonOptArg)
c
c       Get input/output files
      if (PipGetInOutFile('InputImageFile', 1, ' ', filin)
     &    .ne. 0) call exitError('NO INPUT IMAGE FILE SPECIFIED')
      if (PipGetInOutFile('PointModelFile', 2, ' ', filpoint)
     &    .ne. 0) call exitError('NO POINT MODEL FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 3, ' ', filout)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
c       
c       Open file, check size
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
      IF (nx*ny .gt.idim) call exitError('INPUT IMAGE TOO LARGE FOR ARRAY')
c       
c       open model in partial mode
      call imodPartialMode(1)
      if (.not.readSmallMod(filpoint)) call exitError('READING MODEL FILE')
      nobjTot = getImodObjSize()
      if (nobjTot .gt. limobj) call exitError(
     &    'TOO MANY MODEL OBJECTS FOR ARRAYS')
c       
c       Handle piece list if any
      ierr = PipGetString('PieceListFile', plfile)
      call read_piece_list(plfile,ixpclist,iypclist,izpclist,npclist)
c       
c       Define box size and offsets to edges
      if (PipGetTwoIntegers('BoxSizeXandY', nxbox, nybox) .ne. 0)
     &    call exitError('NO BOX SIZE SPECIFIED')
      ixlo = -nxbox / 2
      ixhi = nxbox + ixlo - 1
      iylo = -nybox / 2
      iyhi = nybox + iylo - 1
c
      if (nxbox*nybox.gt.idimbox) call exitError('BOX TOO LARGE FOR ARRAYS')
      if (nxbox .lt. 4 .or. nybox .lt. 4) call exitError('BOX SIZE TOO SMALL')
c       
c       Get other options
      newmode = mode
      ierr = PipGetInteger('ModeOfOutput', newmode)
      if (newmode .ne. mode .and. newmode .lt. 0 .or. (newmode .gt. 2 .and.
     &    newmode .ne. 6)) call exitError('INVALID MODE ENTERED')
      iffill = 1 - PipGetFloat('FillValue', fill)
      ierr = PipGetBoolean('SplitIntoCorners', ifsplit)
      ierr = PipGetBoolean('LinearInterpolation', linear)
      ierr = PipGetLogical('ZeroMean', zeromean)
      ierr = PipGetInteger('PassesThroughData', npass)
      npass = max(1, min(2, npass))
      nx3 = nxbox
      ny3 = nybox
      if (PipGetTwoIntegers('SizeOfOutputXandY', nx3, ny3) .eq. 0) then
        if (nx3 .eq. 0) nx3 = nx
        if (ny3 .eq. 0) ny3 = ny
        IF (nx3*ny3 .gt.idim) call exitError('OUTPUT SIZE TOO LARGE FOR ARRAY')
      endif

      if (iffill .gt. 0 .and. nx3 .eq. nxbox .and. ny3 .eq. nybox) print *,
     &    'WARNING: Fill entry has no effect with output size equal '//
     &    'to box size'
      if (zeromean .and. (newmode .eq. 0 .or. newmode .eq. 6)) print *,
     &    'WARNING: Byte and unsigned integer data cannot be shifted to '//
     &    'zero mean'
c       
c       Get object list and/or types
      nobjUse = 0
      if (PipGetString('ObjectsToAverage', listString) .eq. 0) then
        call parselist(listString, iobjUse, nobjUse)
        if (nobjUse .gt. limobj) call exitError(
     &      'OBJECT LIST TOO LARGE FOR ARRAYS')
      else
        ierr = getimodflags(iobjFlags, limobj)
      endif
c       
c       Loop on objects, find out if needed
      npnts = 0
      do imodObj = 1, nobjTot
        ifuse = 0
        if (nobjUse .gt. 0) then
          do i = 1, nobjUse
            if (iobjUse(i) .eq. imodobj) ifuse = 1
          enddo
        else
          ifuse = mod(iobjFlags(imodobj), 4)
        endif
        if (ifuse .ne. 0) then
c           
c           For needed object , load it, unscale, and get coordinates
          if (.not.getModelObjectRange(imodObj, imodObj)) then
            print *
            print *, 'ERROR: BOXAVG - LOADING DATA FOR OBJECT #',imodobj
            call exit(1)
          endif
          call scaleModelToImage(1, 0)

          do iobj = 1, max_mod_obj
        
            ibase=ibase_obj(iobj)
            do ipt = 1, npt_in_obj(iobj)
              npnts = npnts + 1
              if (npnts .gt. limpnt)
     &            call exitError('TOO MANY POINTS FOR ARRAYS')
              ipnt = object(ipt + ibase)
              ixtm = nint(p_coord(1, ipnt))
              iytm = nint(p_coord(2, ipnt))
              iztm = nint(p_coord(3, ipnt))
              call lookup_piece(ixpclist,iypclist,izpclist,npclist,
     &            nx,ny,ixtm,iytm,iztm,ipxtmp,ipytmp,ipz(npnts))
              px(npnts) = ipxtmp + p_coord(1, ipnt) - ixtm
              py(npnts) = ipytmp + p_coord(2, ipnt) - iytm
            enddo
          enddo
        endif
      enddo
c     print *,'npnts',npnts
C       
C       Create output header.
C       
      call irtdel(1, delta)
      CALL IMOPEN(2,FILOUT,'NEW')
      NXYZ2(3)=1
      MXYZ2(1)=NX3
      MXYZ2(2)=NY3
      MXYZ2(3)=1
      CELL2(1)=NX3*delta(1)
      CELL2(2)=NY3*delta(2)
      CELL2(3)=delta(3)
      CELL2(4)=90.
      CELL2(5)=90.
      CELL2(6)=90.
C       
      call time(tim)
      call date(dat)
c       
      write(titlech,301) dat,tim
301   FORMAT('BOXAVG: average of boxes',32x,a9,2x,a8)
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
      CALL ICRHDR(2,NXYZ2,MXYZ2,newmode,TITLE,0)
      call itrlab(2,1)
      CALL IALCEL(2,CELL2)
c       
c       make list of z values
c       
      nzlist=0
      do j=1,npnts
        notonlist=1
        do list=1,nzlist
          izpnt=ipz(j)
          if(izpnt.eq.listsect(list).or.izpnt.lt.0)notonlist=0
        enddo
        if(notonlist.eq.1)then
          nzlist=nzlist+1
          if (nzlist .gt. limsec) call exitError('TOO MANY Z VALUES FOR ARRAY')
          listsect(nzlist)=ipz(j)
        endif
      enddo
c      print *,nzlist

      do ipass=1,npass
c         
c         copy last sum into c array  and zero the sum array
c         
        nadded=0
        do i=1,nxbox*nybox
          if (npass .gt. 1) crray(i)=brray(i)
          brray(i)=0.
        enddo
c         
c         loop over the sections in the list
c         
        margin = 2
        if (npass .gt. 1) margin = 2 + maxshift
        do list=1,nzlist
c           
c           read section if it exists
c           
          iz=listsect(list)
          if(iz.ge.0.and.iz.lt.nz)then
c            print *,'loading', iz
            call imposn(1,iz,0)
            call irdsec(1,array,*99)
c             
c             look for points in that section and far enough from edge
c             
            do jpoint=1,npnts
              kx=nint(px(jpoint))
              ky=nint(py(jpoint))
              kz=ipz(jpoint)
              if(kz.eq.iz.and.
     &            kx+ixlo.ge.margin.and.kx+ixhi.lt.nx-margin.and.
     &            ky+iylo.ge.margin.and.ky+iyhi.lt.ny-margin)then
                dxbest=0.
                dybest=0.
                if(ipass.gt.1)then
c                   
c                   Second pass: do search
                  call shiftSearch(array, nx, ny, crray, drray, nxbox, nybox, 
     &                px(jpoint), py(jpoint), dxbest, dybest, float(maxshift),
     &                0.9, dmean2, linear)
                  call shiftSearch(array, nx, ny, crray, drray, nxbox, nybox, 
     &                px(jpoint), py(jpoint), dxbest, dybest, 0.9,
     &                0.3, dmean2, linear)
                  call shiftSearch(array, nx, ny, crray, drray, nxbox, nybox, 
     &                px(jpoint), py(jpoint), dxbest, dybest, 0.3,
     &                0.1, dmean2, linear)
c                   write (*,'(2f6.1)')dxbest,dybest
                endif
c                 
c                 now add it into the sum with the best offsets
c                 
                call cubinterp(array, drray, nx, ny, nxbox, nybox, amat,
     &              px(jpoint) + dxbest, py(jpoint) + dybest, 0., 0., 1.,
     &              dmean2, linear)
                do i=1,nxbox*nybox
                  brray(i)=brray(i)+drray(i)
                enddo
                nadded=nadded+1
              endif
            enddo
          endif
        enddo
c         
c         divide it by the number of points added to get average
c         
        if (nadded .eq. 0) call exitError('NO POINTS AVERAGED')
        do i=1,nxbox*nybox
          brray(i)=brray(i)/nadded
        enddo
      enddo
      print *,nadded,' points averaged'
c       
c       find mean of edge of box and total mean
c       
      edge = sliceEdgeMean(brray, nxbox, 1,nxbox,1,nybox)
      call iclden(brray,nxbox,nybox,1,nxbox,1,nybox,dmin,dmax,dmean)
c       
c       if box is not equal to output array, fill the rest with the edge mean
c       or the specified value
      write(*,'(a,4f10.2)')' min, max, mean, edge:' ,dmin,dmax,dmean,edge
      if (iffill .eq. 0) fill=edge
      bias=edge+(dmean-edge)*float(nxbox*nybox)/float(nx3*ny3)
c       
      do i = 1, nx3 * ny3
        array(i) = fill
      enddo 
      if (ifsplit .eq. 0) then
        ixlo = 0
        iylo = 0
      endif
c       
c       copy box, potentially splitting into corners 
c           
      do iy=1,nybox
        do ix=1,nxbox
          ixnew=ix+ixlo
          iynew=iy+iylo
          if(ixnew.le.0)ixnew=ixnew+nx3
          if(iynew.le.0)iynew=iynew+ny3
          array(ixnew + nx3 * (iynew - 1)) = brray(ix + nxbox * (iy - 1))
        enddo
      enddo
c       
c       if possible for mode and requested, shift mean to zero
c       
      if((newmode.eq.2 .or. newmode .eq. 1) .and. zeromean)then
c         
        do i=1,nx3*ny3
          array(i)=array(i)-bias
        enddo
      endif
c       
      if(mode.eq.newmode.or.newmode.eq.2)then
c         
c         get the min, max and mean
c         
        call iclden(array,nx3,ny3,1,nx3,1,ny3,dmin,dmax,dmean)
      else
c         
c         otherwise, if mode not = new mode, rescale
c         
        call isetdn(array,nx3,nx3,newmode, 1,nx3,1,ny3,dmin,dmax,dmean)
      endif
      call iwrsec(2,array)
C       
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
C       
      call exit(0)
99    call exitError(' END OF IMAGE WHILE READING')
      END


c       Searches for the best alignment between the average in CRRAY (size
c       NXBIX x NYBOX) and the image extracted from ARRAY (size NX by NY) at
c       center position PX, PY.  DXBEST and DYBEST provide starting offsets and
c       are returned with the new best offsets.  SHIFTMAX is the maximum shift
c       to search, and DELSHIFT is the interval of the search.  DMEAN2 and
c       LINEAR are passed to cubinterp.
c
      subroutine shiftSearch(array, nx, ny, crray, drray, nxbox, nybox, 
     &    px, py, dxbest, dybest, shiftmax, delshift, dmean2, linear)
      implicit none
      real*4 array(*),crray(*),drray(*),px, py, dxbest, dybest, shiftmax
      real*4 delshift, dmean2, diff, sum, sumsq, xc, yc, sd, sdmin, amat(2,2)
      integer*4 nx, ny, nxbox, nybox, linear, maxshift, idx, idy, idxbest
      integer*4 idybest, i, nsum

      amat(1,1) = 1.
      amat(1,2) = 0.
      amat(2,1) = 0.
      amat(2,2) = 1.
      sdmin = 1.e30
      maxshift = nint(shiftmax / delshift)
      xc = px + dxbest
      yc = py + dybest
      do idx=-maxshift,maxshift
        do idy=-maxshift,maxshift
          sum=0.
          sumsq=0.
          call cubinterp(array, drray, nx, ny, nxbox, nybox, amat,
     &        xc + idx * delshift, yc + idy * delshift, 0., 0., 1., dmean2,
     &        linear)
c           
c           There was an old comment that the SD of the difference was a
c           better measure than the cross-product.
c                         
          do i=1,nybox*nxbox
            diff=crray(i)-drray(i)
            sum=sum+diff
            sumsq=sumsq+diff**2
          enddo
c           
          nsum=nxbox*nybox
          sd=sqrt(max(0.,(sumsq-sum**2/nsum)/(nsum-1.)))
          if(sd.lt.sdmin)then
            sdmin=sd
            idxbest=idx
            idybest=idy
c            write(*,101)dxbest + idx*delshift,dybest + idy*delshift,sd,'*'
c101         format(2f8.1,f9.2,a1)
          else
c            write(*,101)dxbest + idx*delshift,dybest + idy*delshift,sd
          endif
        enddo
      enddo
      dxbest = dxbest + idxbest * delshift
      dybest = dybest + idybest * delshift
      return
      end

c       $Log$
c       Revision 3.3  2007/10/25 02:47:18  mast
c       fixed initialization of split option
c
c       Revision 3.2  2007/10/22 07:56:51  mast
c       Fix for intel compiler
c
c       Revision 3.1  2007/10/21 17:35:57  mast
c       Converted to PIP, made it use model input and operate on non-integer
c       coordinates
c
c       

*       * * * * BOXSTARTEND * * * * *
c       
c       BOXSTARTEND will clip out volumes of image centered on the starting
c       or ending points of model contours, or on all points in a model
c       contour.  It can place each volume into a separate file, or stack all
c       the extracted volumes into a single output file.  In the latter
c       case, the program can generate two lists of piece coordinates to
c       allow the volumes to be examined in two different ways.  One list
c       will assemble all of the volumes into a single square array, with
c       the extracted starting or ending points all appearing on the same
c       section.  The other list will display each piece in its true
c       position in the original volume of images.
c       
c       See man page for details
c
c       $Id$
c       Log at end of file
c
c       David Mastronarde 4/23/90
c       
      implicit none
      include 'smallmodel.inc'
      integer ixdim,idim,limpcl,limobj,limsec
      parameter (ixdim=500,idim=ixdim*ixdim*ixdim,limpcl=50000,limobj=10000)
      parameter (limsec=10000)
      real*4 array(idim),brray(idim),avgray(idim)
      equivalence (array, avgray),(brray,avgray)
      integer*4 nxyz(3),mxyz(3),nxyz2(3),nxyzst(3),ind(3),nx,ny,nz
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      logical exist
      character*160 modelfile
      real*4 delt(3),orig(3),cell(6),title(20),offset(3)
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      integer*4 listz(limpcl)
      integer*4 icolclip(limobj), iobjFlags(limobj), loadObjs(limobj)
      character*160 filin,filpcl,filpcl2,convnum,rootname
      character*320 concat
      character*10240 listString
      data nxyzst/0,0,0/
      character*9 dat
      character*8 tim
      common /bigarr/avgray
c       
      character*80 titlech
      character*6 chstrnd
      real*4 g(2,3,limsec),gtmp(2,3)
c       
      real*4 DMIN,DMAX,DMEAN,xcen,ycen,dsum,tmin,tmax,xnew, ynew, dmin2, dmax2
      integer*4 mode, ierr, izrange, npclist, nlistz, minxpiece, nxpieces, icol
      integer*4 nxoverlap, minypiece, nypieces, nyoverlap, maxxpiece, maxypiece
      integer*4 nobjTot, ifxform, nxg, i, ix, iy, indxmin, indxmax, indymin
      integer*4 indymax, indzmin, indzmax, ncolclip, numLoadObj, imodObj, ifuse
      integer*4 ifstrtend, npixbox, npixsq, npleft, npright, ibbase, ipnt,ilist
      integer*4 iabase, nzbefore, nzafter, nzclip, ifseries, ifaverage, numfile
      integer*4 npredict, iobj, ngutter, nzout, nclip, loopst, loopnd, loop
      integer*4 indleft, indright, indbot, indtop, indzlo, indzhi,indz, indg
      integer*4 indar, ipc, ixpc, iypc, ipxstr, ipystr, ipxend, ipyend, kti,jnd
      integer*4 newypiece, newxpiece, indx, indy,iz, npixby, npixbz,npbot,nptop
      real*4 tmean,dmean2
      logical*4 backxform
      integer*4 getImodObjSize, getimodflags
      logical*4 readSmallMod, getModelObjectList
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetLogical
      integer*4 PipGetInOutFile,PipGetTwoIntegers, PipGetThreeIntegers
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  boxstartend
c
      integer numOptions
      parameter (numOptions = 19)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'image:InputImageFile:FN:@model:ModelFile:FN:@'//
     &    'output:OutputFile:FN:@series:SeriesRootName:CH:@'//
     &    'piece:PieceListFile:FN:@array:ArrayPieceList:FN:@'//
     &    'true:TruePieceList:FN:@objects:ObjectsToUse:LI:@box:BoxSizeXY:I:@'//
     &    'slices:SlicesBelowAndAbove:IP:@which:WhichPointsToExtract:I:@'//
     &    'xminmax:XMinAndMax:IP:@yminmax:YMinAndMax:IP:@'//
     &    'zminmax:ZMinAndMax:IP:@xforms:XformsToApply:FN:@'//
     &    'back:BackTransform:B:@blank:BlankBetweenImages:I:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       defaults
      ifstrtend = -1
      ngutter = 5
      backxform = .false.
      ifxform = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
      call PipReadOrParseOptions(options, numOptions, 'boxstartend',
     &    'ERROR: BOXSTARTEND - ', .true., 4, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputImageFile', 1, 'Enter input image file name',
     &    filin) .ne. 0)call exitError('NO INPUT IMAGE FILE SPECIFIED')

      CALL IMOPEN(1,filin,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      call irtdel(1,delt)
      call irtorg(1,orig(1),orig(2),orig(3))
C       
C       Open other files
C       
      filpcl = ' '
      if (pipinput) then
        ierr = PipGetString('PieceListFile', filpcl)
      else
        write(*,'(1x,a,$)')
     &      'Piece list file if image is a montage, otherwise Return: '
        read(*,50)filpcl
      endif
      call read_piece_list(filpcl,ixpclist,iypclist,izpclist,npclist)
c       
c       if no pieces, set up mocklist
      if(npclist.eq.0)then
        do i=1,nz
          ixpclist(i)=0
          iypclist(i)=0
          izpclist(i)=i-1
        enddo
        npclist=nz
      endif
c       make ordered list of z values 
      call fill_listz(izpclist,npclist,listz,nlistz)

      izrange=listz(nlistz)+1-listz(1)
      call checklist(ixpclist,npclist,1,nx,minxpiece
     &    ,nxpieces,nxoverlap)
      call checklist(iypclist,npclist,1,ny,minypiece
     &    ,nypieces,nyoverlap)
      xcen=minxpiece+(nx+(nxpieces-1)*(nx-nxoverlap))/2.
      ycen=minypiece+(ny+(nypieces-1)*(ny-nyoverlap))/2.
      maxxpiece=minxpiece+(nx+(nxpieces-1)*(nx-nxoverlap))-1
      maxypiece=minypiece+(ny+(nypieces-1)*(ny-nyoverlap))-1
c       
      call imodPartialMode(1)

      if (PipGetInOutFile('ModelFile', 2, 'Name of input model file',
     &    modelfile) .ne. 0)call exitError('NO INPUT MODEL FILE SPECIFIED')
      exist=readSmallMod(modelfile)
      if(.not.exist) call exitError('OPENING MODEL FILE')
      nobjTot = getImodObjSize()
      if (nobjTot .gt. limobj) call exitError(
     &    'TOO MANY MODEL OBJECTS FOR ARRAYS')
c       
      if (pipinput) then
        ifxform = 1 - PipGetString('XformsToApply', filpcl)
        ierr = PipGetLogical('BackTransform', backxform)
        if (ifxform .ne. 0 .and. backxform) ifxform = -1
      else
        write(*,'(1x,a,$)')'0 to take coordinates as they are, 1 to'//
     &      ' transform, -1 to back-transform: '
        read(5,*)ifxform
        if(ifxform.ne.0)then
          write(*,'(1x,a,$)')'Name of file with transforms: '
          read(5,50)filpcl
        endif
      endif

        if(ifxform.ne.0)then
        call dopen(4,filpcl,'old','f')
        call xfrdall(4,g,nxg,*99)
        if(ifxform.lt.0)then
          do i=1,nxg
            call xfinvert(g(1,1,i),gtmp)
            call xfcopy(gtmp,g(1,1,i))
          enddo
        endif
      endif
c       
      indxmin=minxpiece
      indxmax=maxxpiece
      indymin=minypiece
      indymax=maxypiece
      indzmin=-99999
      indzmax=-99999
      ncolclip = 0
      if (pipinput) then
        ierr = PipGetTwoIntegers('XMinAndMax', indxmin, indxmax)
        ierr = PipGetTwoIntegers('YMinAndMax', indymin, indymax)
        ierr = PipGetTwoIntegers('ZMinAndMax', indzmin, indzmax)
        if (PipGetString('ObjectsToUse', listString) .eq. 0)
     &      call parselist(listString, icolclip,ncolclip)
      else
        write(*,'(1x,a,/,a,$)')'Enter minimum and maximum X and Y'//
     &      ' index coordinates within which',
     &      '    ends should be contained, or / for no limits: '
        read(*,*)indxmin,indxmax,indymin,indymax
        write(*,'(1x,a,/,a,$)')'Enter minimum and maximum'//
     &      ' section numbers within which',
     &      '    boxes should be contained, or / for no limits: '
        read(*,*)indzmin,indzmax
        print *,'Enter list of numbers of objects whose ends should '//
     &      'be clipped (Return for all)'
        call rdlist(5,icolclip,ncolclip)
      endif
c       
      indxmin=max(indxmin,minxpiece)
      indymin=max(indymin,minypiece)
      indxmax=min(indxmax,maxxpiece)
      indymax=min(indymax,maxypiece)
c       
c       indzmin=max(indzmin,listz(1))
c       indzmax=min(indzmax,listz(nlistz))
c       
c       
      numLoadObj = 0
      ierr = getimodflags(iobjFlags, limobj)

      do imodObj = 1, nobjTot
        ifuse = 0
        if (ncolclip .gt. 0) then
          do icol=1,ncolclip
            if(icolclip(icol) .eq. imodObj) ifuse = 1
          enddo
        else
          ifuse = mod(iobjFlags(imodobj), 4)
        endif
        if (ifuse .ne. 0) then
          numLoadObj = numLoadObj + 1
          loadObjs(numLoadObj) = imodObj
        endif
      enddo
      if (.not.getModelObjectList(loadObjs, numLoadObj))
     &    call exitError('LOADING MODEL DATA')
c             
c             convert to index coords in the current volume
c             
      call scaleModelToImage(1, 0)
c       
      if (pipinput) then
        ierr = PipGetInteger('WhichPointsToExtract', ifstrtend)
        if (PipGetInteger('BoxSizeXY', npixbox) .ne. 0) then
          if (PipGetThreeIntegers('VolumeSizeXYZ', npixbox, npixby, npixbz)
     &        .ne. 0) call exitError(
     &        'BOX SIZE MUST BE ENTERED WITH -box OR -volume')
        else
          npixby = npixbox
          npixbz = npixbox
        endif
      else
        write(*,'(1x,a,$)')
     &      'Clip out starts (0) or ends (1) or all points (-1): '
        read(*,*)ifstrtend
c       
        write(*,'(1x,a,$)')'Box size in pixels: '
        read(*,*)npixbox
        npixby = npixbox
        npixbz = npixbox
      endif
      npixsq = npixbox*npixby
      npleft=(npixbox-1)/2
      npright=npixbox-npleft-1
      npbot = (npixby-1) / 2
      nptop = npixby - npbot - 1
      offset(1) = -0.5 * (1 + npright - npleft)
      offset(2) = -0.5 * (1 + nptop - npbot)
      ibbase = idim - npixsq
      iabase = ibbase - npixsq
      if (iabase .lt. 0) call exitError('BOX SIZE TOO LARGE FOR ARRAYS')
c       
      nzbefore = npixbz / 2
      nzafter = npixbz - 1 - nzbefore
      if (pipinput) then
        ierr = PipGetTwoIntegers('SlicesBelowAndAbove', nzbefore, nzafter)
      else
        write(*,'(1x,a,$)')'# of sections before and # of sections'//
     &      ' after endpoint to clip out: '
        read(*,*)nzbefore,nzafter
      endif
      print *,nzbefore, nzafter
      nzclip=nzbefore+nzafter+1
      if (nzclip .lt. 1 .or. npixbox .lt. 1 .or. npixby .lt. 1)
     &    call exitError('BOX SIZE AND NUMBER OF SLICES MUST BE POSITIVE')
      offset(3) = 0.
      if (mod(nzbefore,2) .eq. mod(nzafter,2)) offset(3) = -0.495
c       
c       Manage the Z limits if none entered
      if (indzmin .eq. -99999) indzmin = listz(1) - nzbefore
      if (indzmax .eq. -99999) indzmax = listz(nlistz) + nzafter
c       
      if (pipinput) then
        ifseries = 1 - PipGetString('SeriesRootName', rootname)
      else
        write(*,'(1x,a,$)')'1 to output numbered series of files,'//
     &      ' 0 for single output file: '
        read(5,*)ifseries
        if(ifseries.ne.0)then
          write(*,'(1x,a,$)')'Root name for output files: '
          read(5,50)rootname
        endif
      endif

      if(ifseries.ne.0)then
        ifaverage = 0
        numfile=0
        filpcl=' '
        filpcl2=' '
c         
c         precount the points
c         
        npredict=0
        do iobj=1,max_mod_obj
          if (npt_in_obj(iobj).gt.0)then
            if(ifstrtend.lt.0)then
              npredict=npredict+npt_in_obj(iobj)
            else
              npredict=npredict+1
            endif
          endif
        enddo
      else
        ifaverage = 1
        if (iabase .lt. npixsq * nzclip) then
          ifaverage = 0
          print *,'WARNING: BOXSTARTEND - VOLUMES TOO LARGE TO COMPUTE AVERAGE'
        else
          do ix = 1, npixsq * nzclip
            avgray(ix) = 0.
          enddo
        endif
        if (PipGetInOutFile('OutputFile', 3, 'Output image file name',
     &      filin) .ne. 0)call exitError('NO OUTPUT IMAGE FILE SPECIFIED')
c         
        call imopen(2,filin,'new')
        nxyz2(1)=npixbox
        nxyz2(2)=npixby
        nxyz2(3)=1
        call itrhdr(2,1)
        call ialsiz(2,nxyz2,nxyzst)
c         
        filpcl = ' '
        filpcl2 = ' '
        if (pipinput) then
          ierr = PipGetString('ArrayPieceList', filpcl)
          ierr = PipGetString('TruePieceList', filpcl2)
          ierr = PipGetInteger('BlankBetweenImages', ngutter)
        else
          write(*,'(1x,a,$)') 'Output file for 2D array piece '//
     &        'list, or Return for none: '
          read(5,50)filpcl
50        format(A)
c         
          if(filpcl.ne.' ')then
            write(*,'(1x,a,$)')'Number of empty pixels between clips: '
            read(*,*)ngutter
          endif
          print *, 'Output file for real coordinate piece list,'//
     &        ' or Return for none: '
          read(5,50)filpcl2
        endif
c
        if(filpcl.ne.' ') call dopen(3,filpcl,'new','f')
        if(filpcl2.ne.' ')call dopen(4,filpcl2,'new','f')
      endif
c       
c       set up for loop on model objects
c       
      nzout=0
      nclip=0
      dsum=0.
      tmin=1.e10
      tmax=-1.e10
      do iobj=1,max_mod_obj
        if (npt_in_obj(iobj).gt.0)then
          loopst = 1
          loopnd = npt_in_obj(iobj)
          if(ifstrtend.eq.0)then
            loopnd = 1
          elseif(ifstrtend.gt.0)then
            loopst = loopnd
          endif

          do loop=loopst,loopnd
            ipnt=abs(object(loop+ibase_obj(iobj)))
            do i=1,3
              ind(i)=nint(p_coord(i,ipnt) + offset(i))
            enddo
c            write(*,'(3f12.5,3i7)')(p_coord(i,ipnt),i=1,3), (ind(i),i=1,3)
c             
c             is it inside limits?
c             
            indleft=ind(1)-npleft
            indright=ind(1)+npright
            indbot=ind(2)-npbot
            indtop=ind(2)+nptop
            indzlo=ind(3)-nzbefore
            indzhi=ind(3)+nzafter
            print *,ind(3),indzlo, indzhi
            if(ind(1).ge.indxmin .and. ind(1).le.indxmax .and.
     &          ind(2).ge.indymin .and. ind(2).le.indymax .and.
     &          indzlo.ge.indzmin .and. indzhi.le.indzmax)then
c               
c               set up the file if doing series, use this counter regardless
c               
              numfile=numfile+1
              if(ifseries.ne.0)then
                if(npredict.lt.100)then
                  write(convnum,'(i2.2)')numfile
                elseif(npredict.lt.1000)then
                  write(convnum,'(i3.3)')numfile
                elseif(npredict.lt.10000)then
                  write(convnum,'(i4.4)')numfile
                else
                  write(convnum,'(i5.5)')numfile
                endif
                filin=concat(concat(rootname,'.'),convnum)
                call imopen(2,filin,'new')
                nxyz2(1)=npixbox
                nxyz2(2)=npixby
                nxyz2(3)=nzclip
                call itrhdr(2,1)
                call ialsiz(2,nxyz2,nxyzst)
                dsum=0.
                tmin=1.e10
                tmax=-1.e10
              endif
c               
c               loop on sections
c               
              do indz=indzlo,indzhi
                if(ifxform.ne.0)then
                  indg=0
                  do ilist=1,nlistz
                    if(listz(ilist).eq.indz)indg=ilist
                  enddo
                  if(indg.ne.0)then
                    call xfapply(g(1,1,indg),xcen,ycen,p_coord(1,ipnt),
     &                  p_coord(2,ipnt),xnew,ynew)
                    ind(1)=nint(xnew)
                    ind(2)=nint(ynew)
                    indleft=ind(1)-npleft
                    indright=ind(1)+npright
                    indbot=ind(2)-npbot
                    indtop=ind(2)+nptop
                  endif
                endif
                indar=indz+1-indzlo
c                 
c                 zero out the box
c                 
                do iy=1,npixbox*npixbox
                  brray(iy + ibbase)=dmean
                enddo
c                 
c                 loop on pieces, find intersection with each piece
c                 
                do ipc=1,npclist
                  if(izpclist(ipc).eq.indz)then
                    ixpc=ixpclist(ipc)
                    iypc=iypclist(ipc)
                    ipxstr=max(indleft,ixpc)
                    ipystr=max(indbot,iypc)
                    ipxend=min(indright,ixpc+nx-1)
                    ipyend=min(indtop,iypc+ny-1)
                    if(ipxstr.le.ipxend.and.ipystr.le.ipyend)then
c                       
c                       if it intersects, read in intersecting part, move it
c                       into appropriate part of array
c                       
                      call imposn(1,ipc-1,0)
                      call irdpas(1,array(iabase+1),npixbox,npixby,
     &                    ipxstr-ixpc, ipxend-ixpc,ipystr-iypc,ipyend-iypc,*99)
                      do iy=1,ipyend+1-ipystr
                        do ix=1,ipxend+1-ipxstr
                          brray(ibbase+ix+ipxstr-indleft+(iy+ipystr-indbot-1) *
     &                        npixbox) = array(iabase + ix + (iy-1) * npixbox)
                        enddo
                      enddo
                    endif
                  endif
                enddo
c                 
c                 write piece list and slice
c
                if(filpcl2.ne.' ')write(4,'(3i6)')indleft,indbot,indz
                call iclden(brray(ibbase+1),npixbox,npixby,1,npixbox,1,
     &              npixby,tmin,tmax,tmean)
                dmin2=min(dmin2,tmin)
                dmax2=max(dmax2,tmax)
                dsum=dsum+tmean
                call iwrsec(2,brray(ibbase+1))
                nzout=nzout+1
c
c                 Add to average only if flag set
                if (ifaverage .ne. 0) then
                  do ix=1,npixsq
                    jnd = ix + (indar - 1) * npixsq
                    avgray(jnd)=avgray(jnd)+ brray(ibbase+ix)
                  enddo
                endif
              enddo
c               
c               Done with Z loop, close up particle file
              if(ifseries.ne.0)then
                dmean2=dsum/nzclip
                do i=1,3
                  cell(i)=nxyz2(i) * delt(i)
                  cell(i+3)=90.
                enddo
                call ialsiz(2,nxyz2,nxyzst)
                call ialsam(2,nxyz2)
                call ialcel(2,cell)
                
                call date(dat)
                call time(tim)
                write(titlech,111) dat,tim
                read(titlech,'(20a4)')(title(kti),kti=1,20)
111             format('BOXSTARTEND: Individual box clipped out'
     &              ,t57,a9,2x,a8)
                call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
                call imclose(2)
              endif
              nclip=nclip+1
            endif
          enddo
        endif
      enddo
c       
c       take care of averages and finish stack file
c       
      if (ifseries .eq. 0) then
        if(ifaverage.ne.0)then
          do indar=1,nzclip
            do ix=1,npixsq
              jnd = ix + (indar - 1) * npixsq
              avgray(jnd)=avgray(jnd)/nclip
            enddo
            jnd = 1 + (indar - 1) * npixsq
            call iclden(avgray(jnd),npixbox,npixby,1,
     &          npixbox,1, npixby,tmin,tmax,tmean)
            dmin2=min(dmin2,tmin)
            dmax2=max(dmax2,tmax)
            dsum=dsum+tmean
            call iwrsec(2,avgray(jnd))
            nzout=nzout+1
          enddo
        endif
        dmean2=dsum/nzout
        nxyz2(3)=nzout
        do i=1,3
          cell(i)=nxyz2(i)*delt(i)
          cell(i+3)=90.
        enddo
        call ialsiz(2,nxyz2,nxyzst)
        call ialsam(2,nxyz2)
        call ialcel(2,cell)
        
        call date(dat)
        call time(tim)
        chstrnd='starts'
        if(ifstrtend.gt.0)chstrnd='ends  '
        if(ifstrtend.lt.0)chstrnd='points'
        write(titlech,101) nclip,chstrnd,dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20) 
101     format('BOXSTARTEND: ',i4,1x,a6,' clipped out and averaged'
     &      ,t57,a9,2x,a8)
        call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
        call imclose(2)
        nclip=nclip+1
      endif
c       
c       now put out piece list if desired
c       
      if(filpcl.ne.' ')then
        newypiece=nint(sqrt(float(nclip)))
        newxpiece=(nclip+newypiece-1)/newypiece
        indx=0
        indy=0
        do i=1,nclip
          ixpc=indx*(npixbox+ngutter)
          iypc=indy*(npixby+ngutter)
          do iz=0,nzclip-1
            write(3,'(3i6)')ixpc,iypc,iz
          enddo
          indx=indx+1
          if(indx.ge.newxpiece)then
            indx=0
            indy=indy+1
          endif
        enddo
        close(3)    
      endif
      print *,numfile,' points boxed out'
      call exit(0)
99    call exitError('READING FILE')
      end

c       
c       $Log$
c       Revision 3.8  2008/03/06 23:56:23  mast
c       Switched to scale to current volume
c
c       Revision 3.7  2007/12/05 20:40:40  mast
c       Rewrote to avoid memory constraints and converted to PIP
c
c       Revision 3.6  2007/11/18 04:59:48  mast
c       Redeclared concat at 320, increased filename lengths
c
c       Revision 3.5  2005/12/09 04:43:27  mast
c       gfortran: .xor., continuation, format tab continuation or byte fixes
c
c       Revision 3.4  2004/10/29 22:05:25  mast
c       Added offsets to center extracted data properly
c       
c       Revision 3.3  2004/03/15 23:54:47  mast
c       Redimensioned to 256x256x257 and put big arrays in common
c       
c       Revision 3.2  2002/09/09 21:36:00  mast
c       Eliminate stat_source: and nimp_source: from all includes
c       
c       Revision 3.1  2002/07/12 22:17:44  mast
c       Revised man page to contain instructions for extracting into separate
c       files; added checks on array limits, and converted STOP to
c       call exit(1)
c       
c       revised 1/19/93, for IMOD 4/24/97
c       for separate files and all point output 10/26/99
c       

*       * * * * * EDPIECEPOINT * * * * *
*       
c       This program allows one to "edit" a list of piece coordinates
c       or make up a new piece list for single or multiple
c       frames.
c       
c       See man page for details
c       
c       David Mastronarde  5/8/89
c
c       $Id$
c       
      implicit none
      integer limpcl, limsec
      parameter (limpcl=10000000,limsec=10000000)
      character*320 pclfil
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      integer*4 listz(limsec),newlist(limsec),indxzlst(limsec)
      integer*4 npclist, nxframe, nyframe, nxdelta, nydelta,nzdummy, ix, iy, iz
      integer*4 i, nlistz,nnew,ixadd,iyadd,izadd,ipc,newzval,indmov, ifcol
      integer*4 ixbase, iybase, inner, iouter, ninner, nouter, nyoverlap
      integer*4  nx, minxpiece, nxpieces, nxoverlap, ny, minypiece, nypieces
      integer*4 iOutArg, ierr, newXoverlap, newYoverlap, ifbin, ifadjust, newnx, newny
      integer*4 newminx, newminy, ibinning
      character*10240 listString
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean, PipGetString
      integer*4 PipGetTwoIntegers,PipGetThreeIntegers, PipGetInOutFile
      logical*4 pipinput
c       
c       fallbacks from ../../manpages/autodoc2man -2 2 edpiecepoint
c       
      integer numOptions
      parameter (numOptions = 12)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'create:CreateForSections:I:@pieces:PiecesInXandY:IP:@'//
     &    'spacing:SpacingInXandY:IP:@columns:ColumnsOfPieces:B:@'//
     &    'overlap:NewOverlapInXandY:IP:@size:SizeInXandY:IP:@'//
     &    'new:NewZList:LI:@add:AddToAllCoordinates:FT:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c
      nxframe = 1
      nyframe = 1
      nxdelta = 0
      nydelta = 0
      ixadd = 0
      iyadd = 0
      izadd = 0
      ifcol = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'edpiecepoint',
     &    'ERROR: EDPIECEPOINT - ', .true., 1, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
      if (pipinput) then
        iOutArg = 2
        if (PipGetInteger('CreateForSections', nzdummy) .eq. 0) then
          pclfil = ' '
          iOutArg = 1
        else
          if (PipGetInOutFile('InputFile', 1, ' ', pclfil) .ne. 0) 
     &        call exitError('NO INPUT FILE SPECIFIED')
        endif
      else
        write(*,'(1x,a)')'Enter name of input piece or point'//
     &      ' list file (Return if none)'
        read(*,'(a)')pclfil
      endif
      call read_piece_list(pclfil,ixpclist,iypclist,izpclist,npclist)
c       
      if (npclist.eq.0) then
        if (pipinput) then
          ierr = PipGetTwoIntegers('PiecesInXandY', nxframe, nyframe)
          ierr = PipGetTwoIntegers('SpacingInXandY', nxdelta, nydelta)
          ierr = PipGetBoolean('ColumnsOfPieces', ifcol)
        else
          write(*,'(1x,a,$)')'# of sections to make new piece list for: '
          read(*,*)nzdummy
          write(*,'(1x,a,$)')'# of montage pieces in X and in Y (/ for 1,1): '
          read(5,*)nxframe, nyframe
          write(*,'(1x,a,$)') 'Spacing between pieces in X and Y '//'
     &        (- for pieces in inverse order): '
          read(5,*)nxdelta, nydelta
          write(*,'(1x,a,$)')'0 for pieces in rows, 1 for pieces in columns: '
          read(5,*)ifcol
        endif
        ninner = nxframe
        nouter = nyframe
        if (ifcol .ne. 0) then
          ninner = nyframe
          nouter = nxframe
        endif
        i = 0
        ixbase = 0
        iybase = 0
        if (nxdelta .lt. 0) ixbase = -(nxframe - 1) * nxdelta
        if (nydelta .lt. 0) iybase = -(nyframe - 1) * nydelta
        do iz=1,nzdummy
          do iouter = 1, nouter
            do inner = 1, ninner
              ix = inner
              iy = iouter
              if (ifcol .ne. 0) then
                ix = iouter
                iy = inner
              endif
              i = i + 1
              ixpclist(i) = ixbase + (ix - 1) * nxdelta
              iypclist(i) = iybase + (iy - 1) * nydelta
              izpclist(i)=iz-1
            enddo
          enddo
        enddo
        npclist = i
      endif
c       
      if (PipGetInOutFile('OutputFile', iOutArg, 'Name of output piece or '//
     &    'point list file', pclfil) .ne. 0)
     &    call exitError('NO OUTPUT FILE SPECIFIED')
      call fill_listz(izpclist,npclist,listz,nlistz)
      if (pipinput) then
        ibinning = 1
        ifbin = 1 - PipGetInteger('DivideByBinning', ibinning)
        ifadjust = 1 - PipGetTwoIntegers('NewOverlapInXandY', newXoverlap, newYoverlap)
        if (ifbin .gt. 0 .and. ifadjust .gt. 0) call exitError(
     &      'YOU CANNOT ENTER BOTH -divide AND -overlap')
        if (ifbin .gt. 0 .or. ifadjust .gt. 0) then
          if (PipGetTwoIntegers('SizeInXandY', nx, ny) .ne. 0) call exitError('YOU MUST'//
     &        ' ENTER IMAGE SIZE IN X AND Y TO ADJUST OVERLAPS OR DIVIDE BY BINNING')
          call checklist(ixpclist,npclist, 1, nx, minxpiece ,nxpieces, nxoverlap)
          call checklist(iypclist,npclist, 1, ny, minypiece ,nypieces, nyoverlap)
          if (nxpieces .lt. 1 .or. nypieces .lt. 1) call exitError(
     &        'PIECE COORDINATES ARE NOT REGULARLY SPACED APART')
          newnx = nx
          newny = ny
          if (ifbin .gt. 0) then
            if (ibinning .lt. 2) call exitError('BINNING FACTOR MUST BE 2 OR HIGHER')
            if (PipGetTwoIntegers('BinnedSizeInXandY', newnx, newny) .ne. 0)
     &          call exitError('YOU MUST ENTER THE BINNED IMAGE SIZE TOO')
c
c             pick overlap that matches the original extent binned
            newXoverlap = nxoverlap / ibinning
            ipc = ((nxpieces - 1) * (nx - nxoverlap)) / ibinning
            if (abs((nxpieces - 1) * (newnx - newXoverlap) - ipc) .gt.
     &          abs((nxpieces - 1) * (newnx - newXoverlap - 1) - ipc))
     &          newXoverlap = newXoverlap + 1
            newYoverlap = nyoverlap / ibinning
            ipc = ((nypieces - 1) * (ny - nyoverlap)) / ibinning
            if (abs((nypieces - 1) * (newny - newYoverlap) - ipc) .gt.
     &          abs((nypieces - 1) * (newny - newYoverlap - 1) - ipc))
     &          newYoverlap = newYoverlap + 1
          endif
c           
c           Adjust the piece coordinates
          newminx = minxpiece / ibinning
          newminy = minypiece / ibinning
          do i = 1, npclist
            ipc = (ixpclist(i) - minxpiece) / (nx - nxoverlap)
            ixpclist(i) = newminx + ipc * (newnx - newXoverlap)
            ipc = (iypclist(i) - minypiece) / (ny - nyoverlap)
            iypclist(i) = newminy + ipc * (newny - newYoverlap)
          enddo
        endif
      endif
c       
c       get list of ranges
c       
      print *,'The current list of Z values in input list is:'
      call wrlist(listz,nlistz)
c       
c       get new list of z values for remapping
c       
20    do i=1,nlistz
        newlist(i)=listz(i)
      enddo
      nnew=nlistz
      if (pipinput) then
        if (PipGetString('NewZList', listString) .eq. 0) 
     &      call parselist2(listString, newlist, nnew, limsec)
      else
        write(*,'(/,a,i4,a,/,a,/,a,/,a)')' Enter new list of',nlistz,
     &      ' Z values to remap these values to.',
     &      ' Enter ranges just as above, or / to leave list alone,',
     &      '   or -1 to replace each Z value with its negative.',
     &      ' Use numbers from -999 to -990 to remove points with a'//
     &      ' particular Z value.'
        call rdlist(5,newlist,nnew)
      endif
c       
      if(nnew.eq.1.and.newlist(1).eq.-1)then
        do i=1,nlistz
          newlist(i)=-listz(i)
        enddo
      elseif(nnew.ne.nlistz)then
        if (pipinput) call exitError('NUMBER OF Z VALUES DOES NOT CORRESPOND')
        print *,'Number of Z values does not correspond'
        go to 20
      endif
      do i=1,nlistz
        indxzlst(listz(i)+1-listz(1))=i
      enddo
c       
      if (pipinput) then
        ierr = PipGetThreeIntegers('AddToAllCoordinates', ixadd,iyadd,izadd)
      else
        write(*,'(1x,a,$)')
     &      'Amounts to ADD to ALL X, Y and Z coordinates (/ for 0,0,0): '
        read(*,*)ixadd,iyadd,izadd
      endif
c       
      ipc=1
      do while(ipc.le.npclist)
        newzval=newlist(indxzlst(izpclist(ipc)+1-listz(1)))
        if(newzval.lt.-999.or.newzval.gt.-990)then
          izpclist(ipc)=newzval+izadd
          ixpclist(ipc)=ixpclist(ipc)+ixadd
          iypclist(ipc)=iypclist(ipc)+iyadd
          ipc=ipc+1
        else
          npclist=npclist-1
          do indmov=ipc,npclist
            ixpclist(indmov)=ixpclist(indmov+1)
            iypclist(indmov)=iypclist(indmov+1)
            izpclist(indmov)=izpclist(indmov+1)
          enddo
        endif
      enddo
      call dopen(3,pclfil,'new','f')
      write(3,'(3i8)')(ixpclist(i),iypclist(i),izpclist(i) ,i=1,npclist)
      close(3)
      call exit(0)
      end

c       
c       $Log$
c       Revision 3.4  2010/12/28 19:41:35  mast
c       PIP conversion and overlap change option
c
c       Revision 3.3  2010/10/29 14:38:51  mast
c       Added ability to control order of pieces in dummy
c
c       Revision 3.2  2008/01/09 06:32:53  mast
c       Extended dummy list to make a list for a montage
c

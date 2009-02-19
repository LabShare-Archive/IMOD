*       * * * * * REMAPMODEL * * * * *
*       
c       This program allows one to remap coordinates in a model, in two ways:
c       1) The set of Z values may be mapped, one-to-one, to any arbitrary
c       new set of Z values; and
c       2) The X, Y or Z coordinates may be shifted by a constant.
c       
c       Also, a mapping can be set up easily for the case where serial section 
c       tomograms are being rejoined with different spacings.
c
c       See man page for details.
c       
c       David Mastronarde  5/8/89
c       
c       $Id$
c       Log at end of file.
c       
      implicit none
      integer limsec,limndx,limchunk
      parameter (limsec=10000,limndx=10*limsec,limchunk=1000)
      integer*4 listz(limsec),newlist(limsec),indxzlst(limndx)
      include 'model.inc'
      integer*4 izlist(max_pt),izseFrom(limchunk),izseTo(limchunk)
c       
      character*160 modelfile,newmodel
      character*1024 listString
      logical exist,readw_or_imod
      integer*4 getimodMaxes, putimodMaxes
c       
      integer*4 ninzlis,iobj,indobj,ipnt,nlistz,i,nnew
      integer*4 ifreorder,ninobj,ibase,izval,newzval,indmov,itmp,index,j
      integer*4 lastkeep,idir,idirfirst,inorder,maxx,maxy,maxz,minz,maxindex
      integer*4 ierr,numFrom,numTo,ifadd,ifEntered,izbase,ninsec,ich,izout
      integer*4 izstOld,izndOld,idirOld,idirNew,izstNew,izndNew,izChunk,iz
      real*4 xadd,yadd,zadd
      logical b3dxor
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString,PipNumberOfEntries
      integer*4 PipGetIntegerArray, PipGetThreeFloats
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  remapmodel
c       
      integer numOptions
      parameter (numOptions = 10)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@old:OldZList:LI:@'//
     &    'full:FullRangeInZ:B:@new:NewZList:LI:@add:AddToAllPoints:FT:@'//
     &    'reorder:ReorderPointsInZ:I:@fromchunks:FromChunkLimits:IA:@'//
     &    'tochunks:ToChunkLimits:IA:@help:usage:B:'
c       
      ifreorder = 0
      xadd = 0.
      yadd = 0.
      zadd = 0.
      ifEntered = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'remapmodel',
     &    'ERROR: REMAPMODEL - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
      if (PipGetInOutFile('InputFile', 1, 'Name of input model file',
     &    modelfile) .ne. 0) call exiterror('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('InputFile', 2, 'Name of output model file',
     &    newmodel) .ne. 0) call exiterror('NO OUTPUT FILE SPECIFIED')
c       
c       read in the model
c       
      exist=readw_or_imod(modelfile)
      if(.not.exist)call exitError('READING MODEL FILE')
c       
c       scale to index coordinates
c       
      call scale_model(0)
c       
c       make list of all z values in the model and set default output list
c       
      ninzlis=0
      do iobj = 1, max_mod_obj
        do indobj = 1, npt_in_obj(iobj)
          ipnt = abs(object(indobj + ibase_obj(iobj)))
          izval = nint(p_coord(3,ipnt))
          ifadd = 1
          i = 1
          do while (i .le. ninzlis .and. ifadd .eq. 1)
            if (izlist(i) .eq. izval) ifadd = 0
            i = i + 1
          enddo
          if (ifadd .eq. 1) then
            ninzlis = ninzlis + 1
            if (ninzlis .gt. limsec) call exitError(
     &          'TOO MANY Z VALUES FOR ARRAYS') 
            izlist(ninzlis) = izval
          endif
        enddo
      enddo
      call fill_listz(izlist,ninzlis,listz,nlistz)
      minz = listz(1)
      maxindex = listz(nlistz)
      do i=1,nlistz
        newlist(i)=listz(i)
      enddo
      nnew=nlistz
      ierr = getImodMaxes(maxx, maxy, maxz)
c       
c       get options from pip input
c
      if (pipinput) then
        ierr = PipGetInteger('ReorderPointsInZ', ifreorder)
        ifadd = 1 - PipGetThreeFloats('AddToAllPoints', xadd, yadd, zadd)
        numFrom = 0
        numTo = 0
        ierr = PipGetIntegerArray('FromChunkLimits', izseFrom, numFrom,
     &      limchunk)
        ierr = PipGetIntegerArray('ToChunkLimits', izseTo, numTo, limchunk)
        if (b3dxor(numFrom .gt. 0, numTo .gt. 0)) call exitError(
     &      'YOU MUST ENTER BOTH -fromchunks AND -tochunks OR NEITHER')
        if (numFrom .eq. 0) then
c           
c           No chunks: get old list if entered, or full one
c
          ierr = PipGetBoolean('FullRangeInZ', ifEntered)
          if (PipGetString('OldZList', listString) .eq. 0) then
            if (ifEntered .ne. 0) call exitError(
     &          'YOU CANNOT ENTER BOTH A NEW LIST AND -full')
            call parselist(listString, listz, nlistz)
            if (nlistz .gt. limsec) call exitError(
     &          'TOO MANY Z VALUES IN NEW LIST FOR ARRAYS')
            ifEntered = 1
            do i = 1, nlistz - 1
              if (listz(i) .ge. listz(i + 1)) call exitError(
     &            'INPUT LIST OF Z VALUES MUST BE IN INCREASING ORDER')
            enddo
          else if (ifEntered .ne. 0) then
            if (maxz .gt. limsec) call exitError(
     &          'MODEL RANGE IN Z TOO BIG FOR ARRAYS')
            do i = 1, maxz
              listz(i) = i - 1
            enddo
            nlistz = maxz
          endif
c           
c           Make sure input list is still default for output
c
          do i=1,nlistz
            newlist(i)=listz(i)
          enddo
          nnew=nlistz
c           
c           No chunks: get new list; it's required unless adding to coordinates
c
          if (PipGetString('NewZList', listString) .eq. 0) then
            call parselist(listString, newlist, nnew)
            if (nnew .gt. limsec) call exitError(
     &          'TOO MANY Z VALUES FOR ARRAYS')
          else if (ifadd .eq. 0) then
            call exitError( 'NO LIST OF NEW Z VALUES SPECIFIED')
          endif
        else
c           
c           Chunks: build up in and out lists
c           
          if (PipGetBoolean('FullRangeInZ', ifEntered) + PipGetString(
     &        'OldZList', listString) + PipGetString('NewZList', listString)
     &        .ne. 3) call exitError('YOU CANNOT ENTER -new, -old, OR -full '//
     &        'WITH CHUNK ENTRIES') 
          if (numFrom .ne. numTo) call exitError(
     &        'SAME NUMBER OF VALUES REQUIRED WITH -tochunk AND -fromchunk')
          nlistz = 0
          izbase = 0
          do ich = 1, numFrom / 2
            izstOld = izseFrom(2 * ich - 1)
            izndOld = izseFrom(2 * ich)
            ninsec = abs(izstOld -  izndOld) + 1
            idirOld = 1
            idirNew = 1
            izstNew = izseTo(2 * ich - 1)
            izndNew = izseTo(2 * ich)
            if (izstOld .gt. izndOld) idirOld = -1
            if (izstNew .gt. izndNew) idirNew = -1
            if (nlistz + ninsec .gt. limsec) call exitError(
     &          'CHUNK RANGES MAKE TOO MANY VALUES FOR ARRAYS')
c             
c             For each z value in a chunk, get z value inside original section
c             and figure out the fate of that z value in the new joined volume
c
            do i = 0, ninsec -1
              izChunk = izstOld + i * idirOld
              iz = i + nlistz
              listz(iz + 1) = iz
              izout = -999
              if (idirNew * (izChunk - izstNew) .ge. 0 .and.
     &            idirNew * (izChunk - izndNew) .le. 0)
     &            izout = izbase + (izChunk - izstNew) / idirNew
              newlist(iz + 1) = izout
            enddo
            nlistz = nlistz + ninsec
            izbase = izbase + abs(izstNew - izndNew) + 1
          enddo
          nnew = nlistz
          minz = min(minz, 0)
          ifEntered = 1
          print *,'Z values being mapped:'
          call wrlist(listz,nlistz)
          print *,'Values being mapped to:'
          call wrlist(newlist,nnew)
        endif
      endif
c       
      if (ifEntered .eq. 0) then
        print *,'The current list of Z values (nearest integers)'//
     &      ' in model is:'
        call wrlist(listz,nlistz)
      endif
c       
c       get new list of z values for remapping
c       
      if (.not. pipinput) then
        write(*,'(/,a,i4,a,/,a,/,a,/,a)')' Enter new list of',nlistz,
     &      ' Z values to remap these values to.',
     &      ' Enter ranges just as above, or / to leave list alone,',
     &      '   or -1 to replace each Z value with its negative.',
     &      ' Use numbers from -999 to -990 to remove points with a'//
     &      ' particular Z value.'
        call rdlist(5,newlist,nnew)
        if (nnew .gt. limsec) call exitError('TOO MANY Z VALUES FOR ARRAYS')
c         
c         
        write(*,'(1x,a,$)')
     &      'Amounts to ADD to ALL X, Y and Z coordinates: '
        read(*,*)xadd,yadd,zadd
      endif

      if(nnew.eq.1.and.newlist(1).eq.-1)then
        do i=1,nlistz
          newlist(i)=-listz(i)
        enddo
      elseif(nnew.ne.nlistz)then
        call exitError('NUMBER OF Z VALUES DOES NOT CORRESPOND BETWEEN OLD '//
     &      'AND NEW LISTS')
      endif
c       
c       Build index list from all Z's that are being mapped
c       Initialize list for range of anything in model or input z list
c
      minz = min(minz,listz(1))
      maxindex = max(maxindex, listz(nlistz))
      if (maxindex - minz + 1 .gt. limndx) call exitError(
     &    'TOO MANY Z VALUES FOR INDEX LIST ARRAY')
      do i = minz, maxindex
        indxzlst(i+1-minz) = 0
      enddo
      do i=1,nlistz
        indxzlst(listz(i)+1-minz)=i
      enddo
c       
c       see if there are any Z's out of order: find direction between
c       first retained Z's and make sure all other intervals match
c       
      inorder=1
      idirfirst=0
      lastkeep=0
      do i=1,nlistz
        if(newlist(i).lt.-999.or.newlist(i).gt.-990)then
c           
c           if this is a retained Z and there is a previous one
c           get the sign of the interval
c           
          if(lastkeep.gt.0)then
            idir=sign(1,newlist(i)-newlist(lastkeep))
c             
c             store the sign the first time, compare after that
c             
            if(idirfirst.eq.0)then
              idirfirst=idir
            elseif(idir.ne.idirfirst)then
              inorder=0
            endif
          endif
          lastkeep=i
        endif
      enddo
c       
      ifreorder=0
      if(inorder.eq.0 .and. .not. pipinput)then
        write(*,'(1x,a,/,a,/,a,$)')'Your new Z''s are not in'//
     &      ' monotonically increasing order.',
     &      ' Enter 1 or -1 to reorder points in each object'//
     &      ' by increasing or decreasing','   new Z value,'//
     &      ' or 0 not to: '
        read(*,*)ifreorder
      endif
c       
c       loop through objects, recompute # of objects
c       
      n_object=0
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        indobj=1
        ibase=ibase_obj(iobj)
        do while(indobj.le.ninobj)
          ipnt=object(indobj+ibase)
          if(ipnt.gt.0.and.ipnt.le.n_point)then
c             
c             for valid point, get new z value
c             
            izval=nint(p_coord(3,ipnt))
            index = indxzlst(izval+1-minz)
            newzval = izval
            if (index .gt. 0) newzval=newlist(index)
c             
c             just shift values if not -999
c             
            if(newzval.lt.-999.or.newzval.gt.-990)then
              p_coord(1,ipnt)=p_coord(1,ipnt)+xadd
              p_coord(2,ipnt)=p_coord(2,ipnt)+yadd
              p_coord(3,ipnt)=p_coord(3,ipnt)+zadd+newzval-izval
              indobj=indobj+1
              maxz = max(maxz, nint(p_coord(3,ipnt)))
            else
c               
c               or delete point by moving rest of pointers in object down
c               
              ninobj=ninobj-1
              do indmov=indobj,ninobj
                object(indmov+ibase)=object(indmov+1+ibase)
              enddo
            endif
          endif
        enddo
        npt_in_obj(iobj)=ninobj
        if(ninobj.gt.0)n_object=n_object+1
c         
c         if reordering, sort object array by Z value
c         
        if(ifreorder.ne.0)then
          do i=1,ninobj-1
            do j=i,ninobj
              if(ifreorder*(p_coord(3,abs(object(j+ibase)))-
     &            p_coord(3,abs(object(i+ibase)))).lt.0.)then
                itmp=object(i+ibase)
                object(i+ibase)=object(j+ibase)
                object(j+ibase)=itmp
              endif
            enddo
          enddo
        endif
c         
      enddo
c       
c       scale data back
c       
      call scale_model(1)
      ierr = putImodMaxes(maxx, maxy, maxz)
      call write_wmod(newmodel)
      call exit(0)
      end

c       DNM 7/20/89  changes for new model format
c       DNM 2/20/90  changes to negate Z and reorder by Z
c       
c       $Log$
c       Revision 3.6  2008/12/10 21:24:38  mast
c       Fixed bug in reading in shifts with -add option
c
c       Revision 3.5  2007/01/24 23:44:45  mast
c       Fixed code for making list of Z values
c
c       Revision 3.4  2006/10/27 20:49:37  mast
c       Converted to PIP, added options for serial tomogram chunks
c
c       Revision 3.3  2002/07/28 01:01:21  mast
c       Added scaling of points back to index coordinates so pixel sizes
c       would be ignored
c	
c       Revision 3.2  2002/05/22 23:44:10  mast
c       *** empty log message ***
c	
c       Revision 3.1  2002/05/20 15:51:09  mast
c       Made sure the elimination of Z values did not confuse it into 
c       thinking the new Z values were out of order.
c	

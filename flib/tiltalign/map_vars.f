c       MAP_VARS.F  -  Routines for variable mapping
c       
c       ANALYZE_MAPS
c       automap
c       inputGroupings
c       makeMapList
c       grouplist
c       groupminmax
c       setgrpsize
c       mapSeparateGroup
c
c       $Id$
c       Log at end
c       
c
c       ANALYZE_MAPS takes a map list and other information about how to set
c       up a mapping and allocates variables, and mappings for getting
c       values from variables
c       GMAG is the variable array, one per view
c       MAPGMAP is filled with mappings to solution variables in VAR
c       LINGMAG is filled with mapping to second variable for linear mapping
c       FRCGMAG is filled with the fractions for linear mapping
c       FIXEDGMAG is the fixed value that is ramped to in linear mappings
c       FIXEDGMAG2 is a second fixed value to be ramped to for second ref
c       IFLIN is 1 for linear mapping, 0 for block
c       MAPLIST is the list of relative numbers to indicate grouping
c       NVIEW is the # of views in the solution
c       IREFTILT is the reference view to be set to zero (0 if none)
c       IREF2 is a second reference view to be set to zero (0 if none)
c       DEFVAL is a default value to fill the array with, if not -999.
c       NAME is a 4 character string with variable name
c       VAR is the solution variable array for minimization
c       VARNAME is an array of 8-character strings filled with map names
c       NVARSRCH is input with an returns number of variables in VAR
c       MAPVIEWTOFILE is mapping from solution views to file views
c       
      subroutine analyze_maps(gmag,mapgmag,lingmag,frcgmag,fixedgmag,
     &    fixedgmag2,iflin,maplist,nview,ireftilt,iref2,
     &    defval,name,var,varname,nvarsrch,mapviewtofile)
      implicit none
      integer*4 mapgmag(*),maplist(*),lingmag(*),mapviewtofile(*)
      integer*4 mapvarno(720)
      real*4 gmag(*),var(*),frcgmag(*)
      character*(*) varname(*),name
      real*4 fixedgmag,fixedgmag2,defval
      integer*4 iflin,nview,ireftilt,iref2,nvarsrch
c       
      integer*4 nmagsrch,iv,mapref1,mapref2,imap,it,listmax,ivmin,ivmax
      integer*4 ningrp,nexvmin,nexvmax,ninnex,nadd,ivadd,ivfix,linval
      integer*4 ivvar,lastadded,iadd
c       
      nmagsrch=0
      do iv=1,nview
        if(defval.ne.-999.)gmag(iv)=defval
      enddo
      mapref1=-999
      if(ireftilt.gt.0)mapref1=maplist(ireftilt)
      mapref2=-999
      if(iref2.gt.0)mapref2=maplist(iref2)
      if(iflin.le.0)then
        do iv=1,nview
          mapgmag(iv)=0
          lingmag(iv)=0
          frcgmag(iv)=1.
          if(maplist(iv).ne.mapref1.and.maplist(iv).ne.mapref2)then
c             
c             if value is same as for minimum tilt, all set for fixed mag
c             otherwise see if this var # is already encountered
c             
            imap=0
            do it=1,nmagsrch
              if(maplist(iv).eq.mapvarno(it))imap=it
            enddo
            if(imap.eq.0)then
c               
c               if not, need to add another mag variable
c               
              nmagsrch=nmagsrch+1
              mapvarno(nmagsrch)=maplist(iv)
              var(nvarsrch+nmagsrch)=gmag(iv)
              write(varname(nvarsrch+nmagsrch),'(a4,i4)')name,
     &            mapviewtofile(iv)
              imap=nmagsrch
            endif
c             
c             now map the mag to the variable
c             
            mapgmag(iv)=nvarsrch+imap
          endif
        enddo
      else
c         
c         LINEAR MAPPING OPTION
c         
        listmax=0
        do iv=1,nview
          listmax=max(listmax,maplist(iv))
        enddo
c         
c         go through the variable groupings
c         
        do imap=1,listmax
          call groupminmax(maplist,nview,imap,ivmin,ivmax,ningrp)
c           
c           if any in this group:
c           
          if(ningrp.gt.0)then
            call groupminmax(maplist,nview,imap+1,nexvmin,nexvmax,
     &          ninnex)
c             
c             if any in next group, the min view in that group is the
c             endpoint for interpolation, otherwise the max view in this
c             group is the endpoint
c             
            nadd=2
            ivadd=ivmin
            ivfix=0
            linval=-1
            if(ninnex.eq.0)then
              nexvmin=ivmax
c               
c               ending and only two in group, add only one
c               
              if(ningrp.le.2)nadd=1
c               
c               but if this one is fixed group, add end to stack unless
c               only 2, in which case add nothing
c               
              if(imap.eq.mapref1.or.imap.eq.mapref2)then
                mapgmag(ivmin)=0
                ivadd=nexvmin
                ivfix=ivmin
                ivvar=nexvmin
                if(ningrp.le.2)nadd=0
                if(imap.eq.mapref2)linval=-2
              endif
            else
c               
c               if continuing and this one is fixed, add next start to list
c               
              if(imap.eq.mapref1.or.imap.eq.mapref2)then
                mapgmag(ivmin)=0
                ivadd=nexvmin
c                 
c                 but if next one is fixed also, use this one's end instead
c                 
                if(imap+1.eq.mapref1.or.imap+1.eq.mapref2)ivadd=ivmax
                nadd=1
                ivfix=ivmin
                ivvar=ivadd
                if(imap.eq.mapref2)linval=-2
c                 
c                 if continuing and next one is fixed, add this start to list
c                 
              elseif(imap+1.eq.mapref1.or.imap+1.eq.mapref2)then
                nadd=1
                ivfix=nexvmin
                ivvar=ivmin
                if(imap+1.eq.mapref2)linval=-2
              endif
            endif
c             
c             add the starting and ending views for interpolation as
c             variables, if they are not already on the list
c             
            do iadd=1,nadd
              if(nmagsrch.eq.0.or.lastadded.ne.ivadd)then
                nmagsrch=nmagsrch+1
                var(nvarsrch+nmagsrch)=gmag(ivadd)
                write(varname(nvarsrch+nmagsrch),'(a4,i4)')name,
     &              mapviewtofile(ivadd)
                lastadded=ivadd
                mapgmag(ivadd)=nvarsrch+nmagsrch
                lingmag(ivadd)=0
                frcgmag(ivadd)=1.
              endif
              ivadd=nexvmin
            enddo
c             
c             
c             now go through rest of views (excluding endpoints), and for
c             each one in the group, map to next to last variable and set
c             fraction to the fraction of the distance in view number from
c             the starting to the ending point
c             
            if(ivfix.eq.0.and.(ninnex.gt.0.or.ningrp.gt.2))then
              do iv=ivmin+1,ivmax
                if(iv.ne.nexvmin.and.maplist(iv).eq.imap)then
                  mapgmag(iv)=mapgmag(ivmin)
                  lingmag(iv)=mapgmag(nexvmin)
                  frcgmag(iv)=float(nexvmin-iv)/float(nexvmin-ivmin)
                endif
              enddo
            elseif(ivfix.eq.0)then
c               
c               ending, not fixed, and only two in group: map 2nd to first
c               
              mapgmag(ivmax)=mapgmag(ivmin)
              lingmag(ivmax)=0
              frcgmag(ivmax)=1.
            elseif(ninnex.eq.0.and.ningrp.le.2)then
c               
c               ending in a fixed group of 2
c               
              mapgmag(ivmax)=0
            else
c               
c               linear ramp between fixed and variable
c               
              do iv=ivmin,ivmax
                if(iv.ne.ivfix.and.iv.ne.ivvar.and.maplist(iv)
     &              .eq.imap)then
                  mapgmag(iv)=mapgmag(ivvar)
                  lingmag(iv)=linval
                  frcgmag(iv)=float(ivfix-iv)/float(ivfix-ivvar)
                endif
              enddo
            endif
c             
c             set the fixed value that is being ramped to
c             
            if(imap.eq.mapref1)fixedgmag=gmag(ivfix)
            if(imap.eq.mapref2)fixedgmag2=gmag(ivfix)
          endif
        enddo
        

      endif
      nvarsrch=nvarsrch+nmagsrch
      return
      end


      subroutine automap(nview,maplist,grpsize,mapfiletoview,nfileviews,
     &    ifpip,ifRequired,defaultOption, nonDefaultOption,ninview, ninThresh,
     &    iflocal,nmapDef, nRanSpecIn, ivSpecStrIn,ivSpecEndIn,nmapSpecIn)
      implicit none
      integer*4 maplist(*),nview,nfileviews,ifpip,ifRequired,ninThresh
      real*4 grpsize(*)
      integer*4 mapfiletoview(*),ninview(*)
      character*(*) defaultOption, nonDefaultOption
      integer maxgrp
      parameter (maxgrp=20)
      integer*4 ivspecstrIn(maxgrp),ivspecendIn(maxgrp),nmapspecIn(maxgrp)
      integer*4 nmapdef, nRanSpecIn,iflocal
c       
      if (iflocal .le. 1) call inputGroupings(nfileviews, ifpip,ifRequired,
     &    defaultOption, nonDefaultOption, nmapDef, ivSpecStrIn,
     &    ivSpecEndIn,nmapSpecIn,nRanSpecIn,maxgrp)
      call makeMapList(nview,maplist,grpsize,mapfiletoview,nfileviews,
     &    nmapDef, ivSpecStrIn, ivSpecEndIn,nmapSpecIn,nRanSpecIn,ninview,
     &    ninThresh)
      return
      end


      subroutine inputGroupings(nfileviews, ifpip,ifRequired,
     &    defaultOption, nonDefaultOption, nmapDef, ivSpecStrIn,
     &    ivSpecEndIn,nmapSpecIn,nRanSpecIn,maxgrp)
      implicit none
      integer*4 nfileviews,ifpip,ifRequired,nMapDef,maxgrp,nRanSpecIn
      character*(*) defaultOption, nonDefaultOption
      integer*4 ivSpecStrIn(*),ivSpecEndIn(*),nmapSpecIn(*)
c       
      integer*4 iran,ivstr,ivend,ierr
      integer*4 PipGetInteger, PipNumberOfEntries, PipGetThreeIntegers
c       
      nRanSpecIn = 0
      if (ifpip .eq. 0) then
        print *,'Enter the negative of a group size to NOT treat ',
     &      'any views separately here.'
        write(*,'(1x,a,$)') 'Default group size, # of ranges'//
     &      ' with special group sizes: '
        read(5,*)nmapdef,nranspecIn
      else
        if (PipGetInteger(defaultOption, nmapdef) .gt. 0) then
          if (ifRequired .eq. 0) return
          print *
          print *,'ERROR: AUTOMAP - OPTION ',trim(defaultOption),
     &        ' MUST BE ENTERED'
          call exit(1)
        endif
        ierr = PipNumberOfEntries(nonDefaultOption, nRanSpecIn)
      endif
c       
      if (nRanSpecIn .gt. maxgrp) then
        print *,'ERROR: AUTOMAP - TOO MANY NONDEFAULT GROUPINGS FOR ARRAYS'
        call exit(1)
      endif
c       
      do iran=1,nranspecIn
        if (ifpip .eq. 0) then
          write(*,'(1x,a,i3,a,$)')'Starting and ending views in range',
     &        iran, ', group size: '
          read(5,*)ivstr,ivend,nmapSpecIn(iran)
        else
          ierr = PipGetThreeIntegers(nonDefaultOption,ivstr,ivend,
     &        nmapSpecIn(iran))
        endif
        if (ivstr.gt.ivend)then
          print *,'ERROR: AUTOMAP - Start past end of range:',ivstr, ivend
          call exit(1)
        endif
        if(ivstr.lt.0.or.ivstr.gt.nFileViews.or.ivend.lt.0.or.
     &      ivend.gt.nfileviews)then
          print *,'ERROR: AUTOMAP - View number not in file: ',ivstr,ivend
          call exit(1)
        endif
        ivSpecStrIn(iran) = ivstr
        ivSpecEndIn(iran) = ivend
      enddo
      return
      end

c       Makes a mapping list, consisting of group number for each view returned
c       in MAPLIST.  NVIEW is the current number of views, GRPSIZE is an array
c       of relative group sizes for those views, MAPFILETOVIEW is a mapping
c       from file views to current views, NFILEVIEWS is the number if file
c       views.  NMAPDEF is the default group size, NRANSPECIN is the number of
c       special ranges, IVSPECSTRIN and IVSPECENDIN are arrays with the
c       starting and ending file views in each range, NMAPSPECIN is and array
c       with the group size in each range.  NINVIEW is an array with the number
c       of points in each view and NINTHRESH has a threshold number required
c       for counting a view toward the number needed to form a group.  If
c       NINTHRESH is zero, the contents of NINVIEW are ignored.

      subroutine makeMapList(nview,maplist,grpsize,mapfiletoview,nfileviews,
     &    nmapDef, ivSpecStrIn, ivSpecEndIn,nmapSpecIn,nRanSpecIn,ninview,
     &    ninThresh)

      implicit none
      integer*4 maplist(*),nview,nfileviews,nMapDef,nRanSpecIn,ninview(*)
      real*4 grpsize(*)
      integer*4 mapfiletoview(*), ninThresh
      integer*4 ivspecstrin(*),ivspecendin(*),nmapspecin(*)
      integer maxview,maxgrp
      parameter (maxview=720,maxgrp=20)
      integer*4 ivspecstr(maxgrp),ivspecend(maxgrp),nmapspec(maxgrp)
      integer*4 inran(maxview)
      integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),ngsep
      common /mapsep/ ivsep,nsepingrp,ngsep
c       
      integer*4 nranspec,iran,ivstr,ivend,nran,ir,ivar
      integer*4 ninran,iv,ig,ifsep,jj,ii,ierr
c       
c       First process special ranges
c       
      nranspec = 0
      do iran = 1, nRanSpecIn
        ivstr = ivSpecStrIn(iran)
        ivend = ivSpecEndIn(iran)
c         
c         convert and trim nonexistent views from range
c         
        do while(ivstr.le.ivend.and.mapfiletoview(ivstr).eq.0)
          ivstr=ivstr+1
        enddo
        do while(ivstr.le.ivend.and.mapfiletoview(ivend).eq.0)
          ivend=ivend-1
        enddo
c         
c         if there is still a range, add it to list
c         
        if(ivstr.le.ivend)then
          nranspec=nranspec+1
          ivspecstr(nranspec)=mapfiletoview(ivstr)
          ivspecend(nranspec)=mapfiletoview(ivend)
          nmapspec(nranspec)=nmapSpecIn(iran)
c           print *,ivspecstr(nranspec),ivspecend(nranspec)
        endif
      enddo
c       
c       build list of all uninterrupted ranges
c       
      nran=nranspec+1
      ivspecstr(nran)=1
      ivspecend(nran)=nview
      nmapspec(nran)=nmapdef
      do iran=1,nranspec
        ir=nranspec+1
c         
c         for each special range, scan rest of ranges and see if they overlap
c         
        do while (ir.le.nran)
          if(.not.(ivspecstr(iran).gt.ivspecend(ir).or.
     &        ivspecend(iran).lt.ivspecstr(ir)))then
c             
c             overlap: then look at three cases
c             
            if(ivspecstr(iran).le.ivspecstr(ir).and.
     &          ivspecend(iran).ge.ivspecend(ir))then
c               
c               CASE #1: complete overlap: wipe out the range from the list,
c               move rest of list down
c               
              nran=nran-1
              do ii=ir,nran
                ivspecstr(ii)=ivspecstr(ii+1)
                ivspecend(ii)=ivspecend(ii+1)
                nmapspec(ii)=nmapspec(ii+1)
              enddo
            elseif(ivspecstr(ir).lt.ivspecstr(iran).and.
     &            ivspecend(ir).gt.ivspecend(iran))then
c               
c               CASE #2: interior subset; need to split range in 2
c               
              nran=nran+1
              ivspecend(nran)=ivspecend(ir)
              ivspecstr(nran)=ivspecend(iran)+1
              nmapspec(nran)=nmapspec(ir)
              ivspecend(ir)=ivspecstr(iran)-1
            else
c               
c               CASE #3, subset at one end, need to truncate range
c               
              if(ivspecstr(iran).gt.ivspecstr(ir))then
                ivspecend(ir)=ivspecstr(iran)-1
              else
                ivspecstr(ir)=ivspecend(iran)+1
              endif
            endif
          endif
          ir=ir+1
        enddo
c         write(*,'(4i4)')(i,ivspecstr(i),ivspecend(i),nmapspec(i),i=1,
c         &     nran)
      enddo
      ivar=1
c       
c       for each range, make list of views in range; exclude the special
c       ones unless nmap is negative
c       
      do iran=1,nran
        ninran=0
        do iv=ivspecstr(iran),ivspecend(iran)
          ifsep=0
          if(nmapspec(iran).gt.0)then
            do ig=1,ngsep
              do jj=1,nsepingrp(ig)
                if(iv.eq.ivsep(jj,ig))ifsep=1
              enddo
            enddo
          endif
          if(ifsep.eq.0)then
            ninran=ninran+1
            inran(ninran)=iv
          endif
        enddo
c         
c         get the ones in range grouped; then group each special set
c         
        call grouplist(inran,ninran,abs(nmapspec(iran)),grpsize,ninview,
     &      ninThresh,ivar, maplist)
        if(nmapspec(iran).gt.0)then
          do ig=1,ngsep
            ninran=0
            do jj=1,nsepingrp(ig)
              if(ivsep(jj,ig).ge.ivspecstr(iran).and.
     &            ivsep(jj,ig).le.ivspecend(iran))then
                ninran=ninran+1
                inran(ninran)=ivsep(jj,ig)
              endif
            enddo
            call grouplist(inran,ninran,nmapspec(iran),grpsize,ninview,
     &          ninThresh,ivar, maplist)
          enddo
        endif
      enddo
c       write(*,'(26i3)')(maplist(i),i=1,nview)
      return
      end


c       Determines groupings for views in a range.  INRAN has the list of
c       views in the range, NINRAN is the number of views in the list.  NMAP
c       is the average group size, and GRPSIZE is an array with relative group
c       sizes for all views.  NINVIEW has the number of points in each view,
c       and NINTHRESH is the threshold number required to count a view toward
c       the total of views in a group.  IVAR comes in with the first free group
c       number and is returned at two past the last group number to avoid
c       connections. MAPLIST is an array indexed by view number and is filled
c       with the group numbers.  

      subroutine grouplist(inran,ninran,nmap,grpsize,ninview, ninthresh,ivar,
     &    maplist)
      implicit none
      integer*4 inran(*),maplist(*),ninview(*)
      real*4 grpsize(*)
      integer*4 ninran,nmap,ivar,ninThresh
c       
      integer*4 nsets,isetstr,isetend,lastmap,iset,ninvar,i, iran,iffew,nextRan
      real*4 setsum,setcum,settarg,cumNext
c       
      if(ninran.eq.0)return
c       
c       First determine if there are any views below theshold - this triggers
c       a priority toward having groups include a minimum # of views above
c       threshold, rather than a priority of maintaining a maximum group size
      iffew = 0
      if (ninThresh .gt. 0) then
        do iran = 1, ninran
          if (ninview(inran(iran)) .lt. ninThresh) iffew = 1
        enddo
      endif
c       
c       compute the expected number of sets, taking into account the relative
c       groupings
      setsum=0.
      if (iffew .eq. 0) then
c         
c         Count all views from start to end of range
        do i = inran(1), inran(ninran)
          setsum=setsum+1./(grpsize(i)*nmap)
        enddo
      else
c         
c         Or count only views in range and above threshold for # of points
        do iran = 1, ninran
          i=inran(iran)
          if (ninview(i) .ge. ninThresh) setsum=setsum+1./(grpsize(i)*nmap)
        enddo
      endif
      nsets=max(1,nint(setsum))

c      if (iffew .ne. 0) print *,'setsum',setsum,'  nsets',nsets
      isetstr=inran(1)
      isetend=isetstr-1
      lastmap=isetstr
      setcum=0.
      iran = 0
      cumnext = 0.
      do iset=1,nsets
c         
c         find the ending point that is at or before the next target sum
c         for a set
c         
        settarg=(iset*setsum)/nsets
        if (iffew .eq. 0) then
c           
c           Simple case: advance as long as the next one will be below or
c           just at the target
          do while(isetend.lt.inran(ninran).and.
     &        setcum+1./(grpsize(isetend+1)*nmap).le.settarg+0.01)
            isetend=isetend+1
            setcum=setcum+1./(grpsize(isetend)*nmap)
          enddo
        else
c          print *,'set',iset,'  settarg',settarg
c
c           Complex case: index on view in list and advance as long as cumul
c           for next above threshold on list is not past target
          do while (iran .lt. ninran .and. cumnext .le. settarg+0.01)
c             
c             Advance index to next, skipping ones below threshold
            iran = iran + 1
            do while (iran .lt. ninran .and. ninview(inran(iran)) .lt.
     &          ninThresh)
              iran = iran + 1
            enddo
c             
c             Set view number of end, and get cumulative number
            isetend = inran(iran)
            setcum=setcum+1./(grpsize(isetend)*nmap)
c             
c             if not at end, get next eligible one
            if (iran.lt. ninran) then
              nextran = iran + 1
              do while (nextran .lt. ninran .and. ninview(inran(nextran)) .lt.
     &            ninThresh)
                nextran = nextran + 1
              enddo
c               
c               If it is at end, set group to the end; otherwise get cum
              if (ninview(inran(nextran)) .ge. ninThresh) then 
                cumNext = setcum + 1./(grpsize(inran(nextran))*nmap)
              else
                iran = ninran
                isetend = inran(iran)
              endif
            endif
          enddo
c          print *,'isetstr',isetstr,'  isetend', isetend,'  setcum',setcum,
c     &        '  cumnext',cumnext
        endif
c         
c         If it's a valid set, assign the group numbers
c         Make sure the last set goes to the end (shouldn't be needed)
        if(isetend.ge.isetstr)then
          if (iset .eq. nsets) isetend = inran(ninran)
          ninvar=0
          do i=1,ninran
            if(inran(i).ge.isetstr.and.inran(i).le.isetend)then
c               
c               if it's a long way since last entry, skip a group number
c               
              if(inran(i)-lastmap.gt.nmap+1)ivar=ivar+1
              ninvar=ninvar+1
              maplist(inran(i))=ivar
              lastmap=inran(i)
            endif
          enddo
          if(ninvar.gt.0)ivar=ivar+1
          isetstr=isetend+1
        endif
      enddo
c       
c       skip a group number at the end to prevent connections between sets
c       
      ivar=ivar+1
      return
      end




      subroutine groupminmax(maplist,nview,imap,ivmin,ivmax,ningrp)
      implicit none
      integer*4 maplist(*),nview,imap,ivmin,ivmax,ningrp
      integer*4 iv
      ivmin=0
      ivmax=0
      ningrp=0
      do iv=1,nview
        if(maplist(iv).eq.imap)then
          if(ivmin.eq.0)ivmin=iv
          ivmax=iv
          ningrp=ningrp+1
        endif
      enddo
      return
      end


c       Computes relative group sizes for each view; group size is proportional
c       to cosine of the tilt angle to the given power.

      subroutine setgrpsize(tilt,nview,power,grpsize)
      implicit none
      real*4 grpsize(*),tilt(*),power
      integer*4 nview,i
      real*4 sum, angmax
c       
c       12/13/08: Originally meant the maximum angle to be 65 but it was not
c       in radians so it never had any effect.  So make it high so grouping
c       will be same for anything but 180 degree data
      angmax = 80. * 3.14159 / 180.
      if(power.eq.0.)then
        do i=1,nview
          grpsize(i)=1.
        enddo
      else
        sum=0.
        do i=1,nview
          grpsize(i)=cos(min(abs(tilt(i)), angmax))**power
          sum=sum+grpsize(i)
        enddo
        do i=1,nview
          grpsize(i)=nview*grpsize(i)/sum
        enddo
      endif
      return
      end

c       
c       Remap the view numbers in a separate group from file views
c       to internal views
c       
      subroutine mapSeparateGroup(ivSep, nsepInGrp, mapFileToView,
     &    nFileViews)
      implicit none
      integer*4 ivSep(*),nsepInGrp, mapFileToView(*), nFileViews, i, j
c       
      i=1
      do while(i.le.nsepingrp)
        if(ivsep(i).le.0.or.ivsep(i).gt.nfileviews)call errorexit(
     &      'View in separate group is outside known range of '//
     &      'image file', 0)
        ivsep(i)=mapfiletoview(ivsep(i))
        if(ivsep(i).eq.0)then
          nsepingrp=nsepingrp-1
          do j=i,nsepingrp
            ivsep(j)=ivsep(j+1)
          enddo
        else
          i=i+1
        endif
      enddo
      end
c
c       $Log$
c       Revision 3.5  2008/12/14 19:03:41  mast
c       Fixed limit on angle used to get group size, set limit to 80
c
c       Revision 3.4  2007/02/19 21:11:01  mast
c       Changes to make group size depend on # of points in views
c
c       Revision 3.3  2005/03/28 22:51:17  mast
c       Split automap into two functions for beadtrack and made function for
c       remapping separate groups
c       
c       Revision 3.2  2004/06/24 15:38:08  mast
c       Changed to work with pip input
c       
c       Revision 3.1  2002/05/07 02:06:54  mast
c       Changes to make things work well with a subset of views
c       

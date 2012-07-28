*       * * * * GENHSTPLT * * * * *
c       
c       GENHSTPLT is a general-purpose interface to the BSHST and BSPLT
c       histogram and 2-dimensional data plotting routines.  See the
c       documentation of those routines for instructions on operating them.  
c       See man page for details.
c       
c       $Id$
c       
      call plax_initialize('genhstplt')
      call exit(0)
      end

      subroutine realgraphicsmain()
      parameter (len=100000,leng=5000)
      dimension dmat(len*10),xx(len),ngx(len),zz(len)
     &    ,yy(len),itype(len),itgrp(50,50),nsymb(50),ntypg(50)
      dimension ngxa(leng),avgx(leng),avgy(leng),nnav(leng),sdy(len)
      real*4 selmin(50),selmax(50)
      integer*4 icolsel(50),ifselexcl(50)
      character*1024 name
      logical b3dxor
      character*80 keys(8), xaxisLabel
      common /pltp/ifgpl,ifNoTerm,ifConnect,numKeys, keys, xaxisLabel
c       
      iffil=-1
      iftypes=0
      ncol=-1
      nskip=0
      iflogxin=0
      nx = 0
      zadd=0
      nselect=0
5     continue
      if (ifNoTerm .eq. 0) then
        write(*,'(1x,a,$)')
     &      '0 for Plax screen plots, 1 for terminal only, -1 for always screen [-1]: '
        read(5,*)iffil
        if (iffil .lt. 0) ifNoTerm = 1
      endif
      if (ifNoTerm .ne. 0) iffil = 0
c       
      call grfopn(iffil)
c       
      write(*,'(1x,a,/,a,$)')'1 if there are types in first column, 0 if there are '//
     &    'no types,',
     &    ' -1 to make one column be X and make separate types for other columns [0]: '
      read(5,*)iftypes
c       
      write(*,'(1x,a,/,a,$)')'Number of columns of data values, 0 if # is on line '//
     &    'before data,',' or -1 if there is one line per row of data [-1]: '
      read(5,*)ncol
c       
      nskip = 0
      write(*,'(1x,a,$)') 'Number of lines to skip at start of file [0]: '
      read(5,*,err=10)nskip
c       
10    call flnam(name,1,'0')
      close(1)
      open(1,file=name,status='old',err=10)
c       
      do i=1,nskip
        read(1,'(a)')name
      enddo
      if (ncol < 0) then
        read(1,'(a)')name
        call frefor2(name, avgx, itype, nfields, leng)
        ncol = 0
        do i = 1, nfields
          if (itype(i) > 0) ncol = i
        enddo
        if (ncol == 0) then
          print *,'There are no numeric values at start of first line of data, try again'
          go to 5
        endif
        rewind(1)
        do i=1,nskip
          read(1,'(a)')name
        enddo
        write(*,'(i5,a)')ncol,' columns of data'
      endif
      if(ncol.eq.0) read(1,*)ncol
c       
      nd=0
12    i=nd+1
      jst=(i-1)*ncol+1
      jnd=i*ncol
      if(iftypes <= 0)then
        read(1,*,end=18)(dmat(j),j=jst,jnd)
        itype(i)=1
      else
        read(1,*,end=18)itype(i),(dmat(j),j=jst,jnd)
      endif
      nd=i
      go to 12
18    write(*,'(i5,a)')nd,' lines of data'
      if (iftypes < 0) 
     &    call convertColsToTypes(dmat, dmat, ncol, nd, itype, nx, ifTypes)

c       
20    if(iftypes.ne.0)then
        write(*,'('' # of groups (neg. if one Type/group): '',$)')
        read(5,*)ngrps
        ifotpg=0
        if(ngrps.lt.0)then
          ngrps=-ngrps
          ifotpg=1
          ntyps=1
        endif
        do j=1,ngrps
          write(*,*) 'for group #',j    
          if(ifotpg.ne.0)then
            write(*,'('' Type # and symbol #: '',$)')
            read(5,*)itgrp(1,j),nsymb(j)
          else
            write(*,'('' # of Types and symbol #: '',$)')
            read(5,*)ntyps,nsymb(j)
            write(*,'('' Types: '',$)')
            read(5,*)(itgrp(i,j),i=1,ntyps)
          endif
          ntypg(j)=ntyps
        enddo
      else
        ngrps=1
        itgrp(1,1)=1
        ntypg(1)=1
        write(*,'(1x,a,$)')'Symbol #: '
        read(5,*)nsymb(1)
      endif
c       
30    ifRetain = 0
31    if(ncol.gt.1)then
        write(*,'('' Column number: '',$)')
        read(5,*)icol
      else
        icol=1
      endif
      if(icol.le.0.or.icol.gt.ncol)go to 20
c       
      if (ifretain == 0 .and. nx > 0) then
        iflogy=iflogx
        do i=1,nx
          yy(i)=xx(i)
        enddo
      endif
c       
      write(*,'(1x,a,$)') '1 or 2 to take log or sqr root '//
     &    '(-1 if it already is log), base to add: '
      read(5,*)iflogxin,zadd
c       
      iflogx=iflogxin
      nx=0
      do igrp=1,ngrps
        do k=1,nd
          ingroup=0
          do j=1,ntypg(igrp)
            if(itype(k).eq.itgrp(j,igrp))ingroup=1
          enddo
c           
c           check that it passes all selections too
c           
          do isel=1,nselect
            selval=dmat((k-1)*ncol+icolsel(isel))
            if(b3dxor(ifselexcl(isel).ne.0, (selval.lt.selmin(isel).or.
     &          selval.gt.selmax(isel))))ingroup=0
          enddo
          if(ingroup.gt.0)then
            nx=nx+1
            ngx(nx)=igrp
            xx(nx)=dmat((k-1)*ncol+icol)
            if(iflogx.gt.0)then
              if(iflogx.eq.2)xx(nx)=sqrt(xx(nx)+zadd)
              if(iflogx.ne.2)xx(nx)=alog10(xx(nx)+zadd)
            endif
          endif
        enddo
      enddo
c       
      if(iflogx.eq.2)iflogx=0
      iflogx=iabs(iflogx)
      call gnhst(xx,ngx,nx,nsymb,ngrps,iflogx)
c       
50    write(*,104)
104   format(' Enter 1 for new column or 14 for new column to replace Y and retain X,',/
     &    '       2 for plot of this column versus previous column or 17 with connectors,'
     &    ,/,
     &    '       3 for X/Y plot of averages of groups of points',
     &    ' or 11 with column ratios',/,
     &    '       4 to define new groups/symbols,',
     &    '   5 to open a new file,',/,
     &    '       6 or 7 to plot metacode file on screen or printer,',
     &    '   8 to exit program,',/,
     &    '       9 for Tukey box plots,',
     &    '   10 for X/Y plot with error bars using S.D.''s,',/,
     &    '       12 to set columns to select on, 13 for X/Y plot dividing Y by a',
     &    ' column')
      if (iftypes == 0 .and. ncol > 2) then
        write(*, 105)
105     format('       15 to make a separate type from each column,   16 for ordinal ',
     &      'column')
      else
        write(*, 106)
106     format('       16 for ordinal column')
      endif
      read(5,*)iopt
      if(iopt.eq.-123)go to 99
      if (iopt .eq. -8) then
        call plax_wait_for_close()
        go to 99
      endif
      if (iopt == -2) go to 1155
      if (iopt == -3) then
        call reverseGraphContrast(1)
        go to 50
      endif
      if(iopt.eq.209)iopt=7
      if(iopt.le.0.or.iopt.gt.17)go to 50
      go to(30,60,70,20,5,90,90,99,110,70,70,130,1130,1140,1150,1160,60)iopt
c       
60    ifConnect = 0
      if (iopt .eq. 17) ifConnect = 1
      call gnplt(yy,xx,ngx,nx,nsymb,ngrps,iflogy,iflogx)
      go to 50
1140  ifRetain = 1
      go to 31
c       
70    write(*,'(1x,a,/,a,$)')'For the error bars, enter the # of '//
     &    'S.D.''s, or - the # of S.E.M.''s,',
     &    '   or a large # for confidence limits at that % level: '
      read(5,*)fsd
      if (iopt.eq.10)go to 120
      if(iopt.eq.11)then
        write(*,'(1x,a,$)')'Column to divide current column by: '
        read(5,*)icolden
      endif
      nplt=0
      igrpstr=1
c       
      do igrp=1,ngrps
        if(ngrps.gt.1)write(*,*)' Define groupings of points for group #',igrp
c         
        ningrp=0
        do i=igrpstr,nx
          if(ngx(i).ne.igrp)exit
          ningrp=ningrp+1
        enddo
        if(iopt.eq.11)then
          ix=0
          do k=1,nd
            ingroup=0
            do j=1,ntypg(igrp)
              if(itype(k).eq.itgrp(j,igrp))ingroup=1
            enddo
            if(ingroup.gt.0)then
              ix=ix+1
              zz(ix)=dmat((k-1)*ncol+icolden)
            endif
          enddo
        endif
c         
        if(ningrp.gt.0)then
          if(iopt.eq.11)then
            call grupnt3(yy(igrpstr),xx(igrpstr),zz,ningrp,
     &          avgx(nplt+1),avgy(nplt+1),sdy(nplt+1),nnav,ngav)
          else
            call grupnt(yy(igrpstr),xx(igrpstr),ningrp,avgx(nplt+1)
     &          ,avgy(nplt+1),sdy(nplt+1),nnav,ngav)
          endif
c           
          do i=1,ngav
            semval=sdy(nplt+i)/sqrt(float(nnav(i)))
            if(fsd.gt.30..and.nnav(i).gt.1)then
              ifail=0
c               tcrit=g01caf(dble((1.+0.01*fsd)/2.),nnav(i)-1,ifail)
              tcrit=tvalue((1.+0.01*fsd)/2.,nnav(i)-1)
              sdy(nplt+i)=tcrit*semval
            elseif(fsd.ge.0)then
              sdy(nplt+i)=fsd*sdy(nplt+i)
            elseif(nnav(i).gt.1)then
              sdy(nplt+i)=-fsd*semval
            endif
            ngxa(nplt+i)=igrp
          enddo
c           
          nplt=nplt+ngav
          igrpstr=igrpstr+ningrp
        endif
      enddo
      call errplt(avgx,avgy,ngxa,nplt,nsymb,ngrps,sdy,iflogy,iflogx)
      go to 50
c       
110   sdy(1)=-9999.
      print *,'Enter an X value for each group, or / to use X',
     &    ' values in previous column'
      read(5,*)(sdy(i),i=1,ngrps)
      if(sdy(1).ne.-9999.)then
        do i=1,nx
          yy(i)=sdy(ngx(i))
        enddo
      endif
      call boxplt(yy,xx,ngx,nx,nsymb,ngrps,sdy,iflogy,iflogx)
      go to 50
c       
120   if(fsd.ge.0.and.fsd.le.30.)then
        write(*,'(1x,a,$)')'Column number with S.D.''s: '
        read(5,*)icolsd
      else
        write(*,'(1x,a,$)')'Column numbers with S.D.''s and n''s: '
        read(5,*)icolsd,icolnn
      endif
      ix=0
      do igrp=1,ngrps
        do k=1,nd
          ingroup=0
          do j=1,ntypg(igrp)
            if(itype(k).eq.itgrp(j,igrp))ingroup=1
          enddo
          if(ingroup.gt.0)then
            ix=ix+1
            sdval=dmat((k-1)*ncol+icolsd)
            sdy(ix)=fsd*sdval
            if(fsd.lt.0..or.fsd.gt.30.)then
              nnval=dmat((k-1)*ncol+icolnn)
              semval=sdval/sqrt(float(nnval))
              if(nnval.le.1)then
                sdy(ix)=0.
              elseif(fsd.lt.0.)then
                sdy(ix)=-fsd*semval
              else
                ifail=0
c                 tcrit=g01caf(dble((1.+0.01*fsd)/2.),nnval-1,ifail)
                tcrit=tvalue((1.+0.01*fsd)/2.,nnval-1)
                sdy(ix)=tcrit*semval
              endif
            endif
          endif
        enddo
      enddo
      call errplt(yy,xx,ngx,nx,nsymb,ngrps,sdy,iflogy,iflogx)
      go to 50
c       
90    continue
      call pltout(7-iopt)
      go to 50
c       
130   write(*,'(1x,a,$)')
     &    'New column to select on, or 0 to clear selections: '
      read(5,*)icolin
      if(icolin.le.0)then
        nselect=0
        go to 50
      endif
      nselect=nselect+1
      icolsel(nselect)=icolin
      write(*,'(1x,a,/,a,$)')'Minimum and maximum values for that '
     &    //'column, and 0 to include or ',
     &    '  1 to exclude values in that range: '
      read(5,*)selmin(nselect),selmax(nselect),ifselexcl(nselect)
      go to 50
1130  write(*,'(1x,a,$)')'Column number to divide by: '
      read(5,*)icoldiv
      if (icoldiv .lt. 1 .or. icoldiv .gt. ncol) go to 50
      write(*,'(1x,a,$)')'Lower and upper limits for quotient (0,0 for none): '
      read(5,*)qlimlo, qlimhi
      nxt=0
      do igrp=1,ngrps
        do k=1,nd
          ingroup=0
          do j=1,ntypg(igrp)
            if(itype(k).eq.itgrp(j,igrp))ingroup=1
          enddo
c           
c           check that it passes all selections too
c           
          do isel=1,nselect
            selval=dmat((k-1)*ncol+icolsel(isel))
            if(b3dxor(ifselexcl(isel).ne.0, (selval.lt.selmin(isel).or.
     &          selval.gt.selmax(isel))))ingroup=0
          enddo
          if(ingroup.gt.0)then
            nxt=nxt+1
            zz(nxt) = 0.
            (abs(dmat((k-1)*ncol+icoldiv)) .gt. 1.e-10 * abs(xx(nxt))) then
              zz(nxt)=xx(nxt) / dmat((k-1)*ncol+icoldiv)
              if (qlimlo .ne. 0. .or. qlimhi .ne. 0)
     &            zz(nxt) = max(qlimlo,min(qlimhi,zz(nxt)))
            endif
          endif
        enddo
      enddo
      call gnplt(yy,zz,ngx,nx,nsymb,ngrps,iflogy,iflogx)
      go to 50
c       
1150  if (iftypes > 0 .or. ncol < 2) go to 50
      call convertColsToTypes(dmat, dmat, ncol, nd, itype, nx, ifTypes)
      if (iftypes > 0) go to 20
      go to 50
c       
1155  write(*,'(1x,a,$)')'X axis label (Return for none): '
      read(5,'(a)')xaxisLabel
      write(*,'(1x,a,$)')'Number of key strings for next graph: '
      read(5,*)numKeys
      numKeys = max(0, min(8, numKeys))
      if (numKeys == 0) go to 50
      write(*,'(a,i2,a)')'Enter the',numKeys,' strings one per line:'
      do i = 1, numKeys
        read(*, '(a)') keys(i)
      enddo
      go to 50
c
1160  iflogx = 0
      j = 0
      do i = 1, nx
        if (i > 1 .and. ngx(max(i - 1, 1)) .ne. ngx(i)) j = 0
        j = j + 1
        xx(i) = j
      enddo
      go to 50
c
99    call plxoff
      call imexit
      end

      subroutine grupnt3(xx,yy,zz,np,avgx,avgy,sdy,nn,ng)
      dimension xx(*),avgx(*),sdy(*),yy(*),nn(*),avgy(*),zz(*)
      integer*2 ind(10000)
c       given a list of np points (xx,yy), it asks how many contiguous
c       non-overlapping groups (ng) to divide into, and number of points in
c       each group (nn), orders the points by x value, finds the mean x value
c       (avgx), and the mean and sd y value (avgy and sdy)
      sz=0.
      do i=1,np
        ind(i)=i
        sz=sz+zz(i)
      enddo
c       build an index to order the points by xx
      do i=1,np-1
        do j=i+1,np
          if(xx(ind(i)).gt.xx(ind(j)))then
            itmp=ind(i)
            ind(i)=ind(j)
            ind(j)=itmp
          endif
        enddo
      enddo
      write(*,*) np,' points'
      write(*,'('' number of groups: '',$)')
      read(*,*)ng
      thrshint=sz/max(1,ng)
      thresh=thrshint
      ncum=0
      ig=1
      zcum=0.
      do ii=1,np
        i=ind(ii)
        if(zcum+zz(i).ge.thresh)then
          thresh=thresh+thrshint
          nn(ig)=ii-ncum
          ncum=ii
          ig=ig+1
        endif
        zcum=zcum+zz(i)
      enddo
      if(ig.eq.ng)nn(ig)=np-ncum
      write(*,'(1x,a,$)')'# of points in each group (/ for equal denominators): '
      read(5,*)(nn(i),i=1,ng)
      ii=1
      do ig=1,ng
        sx=0.
        sy=0.
        sz=0.
        sysq=0.
        n=nn(ig)
        do in=1,n
          i=ind(ii)
          ii=ii+1
          sx=sx+xx(i)
          sy=sy+yy(i)
          sz=sz+zz(i)
          sysq=sysq+yy(i)**2
        enddo
        avgx(ig)=sx/n
        avgyig=sy/n
        avgz=sz/n
        avgy(ig)=avgyig/avgz
        sdyig=0.
        if(n.gt.1)sdyig=sqrt((sysq-n*avgyig**2)/(n-1.))
        sdy(ig)=sdyig/avgz
        write(*,101)avgx(ig),avgyig,sdyig,avgz,avgy(ig),sdy(ig),nn(ig)
101     format(6f10.4,i5)
      enddo
      return
      end

      subroutine convertColsToTypes(dmatIn, dmatOut, numCol, numData, itype, nx, ifTypes)
      implicit none
      real*4 dmatIn(numCol, numData), dmatOut(2, numData, numCol)
      real*4 tmpMat(numCol, numData)
      integer*4 numCol, numData, itype(numData, numCol), ixCol, i, j, nx, ifTypes
      if (nx > 0) write(*,'(a)')'This operation cannot be reversed except by reloading '//
     &    'the data file'
      write(*,'(1x,a,/,a,$)') 'Enter column number to become X (first column), or 0 '//
     &    'to abort: '
      read(5,*)ixCol
      ifTypes = 0
      if (ixCol <= 0 .or. ixCol > numCol) return
      tmpMat = dmatIn
      do j = 1, numCol
        dmatOut(1, 1:numData, j) = tmpMat(ixCol, 1:numData)
        dmatOut(2, 1:numData, j) = tmpMat(j, 1:numData)
        itype(1:numData, j) = j
      enddo
      numData = numData * numCol
      numCol = 2
      nx = 0
      ifTypes = 1
      return 
      end
      

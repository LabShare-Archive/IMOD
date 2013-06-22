c       MTK analyzes the distribution of distances of closest approach
c       between objects in 3 dimensions.
c       
c       See the man page for details
c       
c       David Mastronarde  November 1991
c       General object version March 2000
c       
c       $Id$
c       
      call plax_initialize('mtk')
      call exit(0)
      end

      subroutine realgraphicsmain()
      parameter (limgraphs=50,limbins=1001,limwobj=100000,limxyz=10000000,
     &    limregion=200,itypall=999)
      parameter (limtyp=250,limrand=1000,limflag=512)
      parameter (limprobs=50,limprobsets=50)
      parameter (nOptNeedModel=10)
      real*4 graphs(limbins,limgraphs),areas(limbins,limgraphs)
      real*4 xmt(limxyz),ymt(limxyz),zmt(limxyz)
      integer*4 iobjflag(limflag)
      integer*4 indstrt(limwobj),npntobj(limwobj)
      integer*4 icolor(limwobj)                 !types of sample points
      integer*4 nreftyp(limgraphs),nneightyp(limgraphs) !# of types
      integer*4 itypref(limtyp,limgraphs),itypneigh(limtyp,limgraphs)
      integer*4 iwhichend(limtyp,limgraphs)
c       
      integer*4 ninclass(limtyp,limregion),nintmp(limregion)
      integer*4 iobjregion(limregion),npntsregion(limregion)
      integer*4 ifflipregion(limregion)
      integer*4 ngapsregion(limregion),indgapregion(limregion)
      integer*4 itypcrosind(-limflag:limflag)
      real*4 xysclregion(limregion),zsclregion(limregion)
      real*4 xofsregion(limregion),yofsregion(limregion)
      real*4 zstrtregion(limregion),zendregion(limregion)
      real*4 xymask(4),xymaskregion(4,limregion),zofsregion(limregion)
      real*4 zgapst(10*limregion),zgapnd(10*limregion)
c       
      character*120 modelfile,modelregion(limregion)
      character*120 tiltfile,tiltregion(limregion)
      logical forceload,grapheach,shuffled,onlyshifted, nearestOnly
      logical sampled,converted,checkgrf,checkextra
c       
      real*4 xmaxdsp(4),ymaxdsp(4),chngfrac(limtyp)
      real*4 probsave(limbins)
      integer*4 igrfdsp(100),ityfrom(limtyp),ityto(limtyp)
      integer*4 listextra(limgraphs),idum(limgraphs)
      integer*4 itypshift(limtyp),itypchck(limtyp)
c       
      integer*4 ibasestrt(limgraphs),ibasend(limgraphs),
     &    integstrt(limgraphs),integend(limgraphs),nrandabove(limrand)
      real*4 baseline(limgraphs),realinteg(limgraphs),
     &    suminteg(limrand),sumsqinteg(limrand)
      integer*4 nbingrf(limgraphs),iobjwin(limwobj),iobjmod(limwobj)
      real*4 delrgrf(limgraphs),powergrf(limgraphs),dum(limgraphs)
      real*4 endsep(limwobj),xyzend(3,limwobj)
      real*4 probnear(limprobs,limprobsets),delnear(limprobsets)
      integer*4 nrestrict(limprobsets),iuseprob(limprobsets)
      integer*4 ioptNeedModel(nOptNeedModel)
     &    /15,16,19,26,20,21,22,27,23,40/
      logical OptionNeedsModel
      character*40 objname
      integer*4 getimodobjname
      integer*4 in5
      common /nmsinput/ in5
      common /bigmts/ xmt, ymt, zmt
c       
      in5 = 5
      ifanyplot=0
      nextragrf=0
      winmin=0.
      winmax=0.
      manyrandom=0
      ifexcludeout=1
      onlyshifted=.false.
      nearestOnly = .false.
      nbinsave=0
      ibavst = 1
      ibavnd = 1
      ibinst = 1
      ibinnd = 1
      baseval = 0
      padbound = 0.
      iwin = 1

      call opencomfile
c       
      print *,'Enter name of output file to store density values in ',
     &    '(Return for none)'
      read(in5,'(a)')modelfile
      if(modelfile.eq.' ')then
        iout=6
      else
        iout=7
        call dopen(7,modelfile,'new','f')
      endif
      iopt=-1
c       
      write(*,'(1x,a,$)')
     &    '0 for graphs in Plax window, 1 to suppress graphs: '
      read(in5,*)iffil
      call scrnOpen(iffil)
c       
      write(*,'(1x,a,/,a,$)')'0 for 3D density/closest approach '//
     &    'analysis','  or 1 for ends versus bundle analysis: '
      read(in5,*)ifbundend
c       
      modelfile=' '
      call read_model(modelfile,tiltfile,xyscal,zscal,xofs,yofs,zofs,
     &    ifflip,iobjflag, limflag,zgapst,zgapnd,ngaps)
      if (modelfile .eq. ' ') go to 38
      indgap=1
c       
c       initialize for first region
c       
10    nregion=1
      ntypes=0
      do i=-limflag,limflag
        itypcrosind(i)=0
      enddo
      irefflag=-1
      neighflag=-1
c       
12    if (modelfile .ne. ' ') then
        write(*,'(1x,a,$)')'Starting and ending Z to include (0,0 '//
     &      'for all; 0,-1 for new model): '
        read(in5,*)zstart,zend
      else
        zstart = 0
        zend = -1
      endif
      if(zstart.eq.0..and.zend.eq.-1.)then
        if(nregion.eq.1)then
          indgap=1
        else
          indgap=indgap+ngaps
        endif
        modelfile=' '
        call read_model(modelfile,tiltfile,xyscal,zscal,xofs,yofs,
     &      zofs,ifflip, iobjflag, limflag,zgapst(indgap),
     &      zgapnd(indgap),ngaps)
        if (modelfile .eq. ' ') go to 40
        if(nregion.eq.1)go to 12
        call checkflags(ngraph,itypref, nreftyp, itypneigh,
     &      nneightyp, iobjflag,limflag,irefflag,neighflag)
        if(irefflag.ge.0)go to 12
        print *,'Previous regions are being discarded'
        go to 10
      endif
c       
      if(ifbundend.ne.0)then
        xymask(1)=-1.e10
        xymask(2)=1.e10
        xymask(3)=-1.e10
        xymask(4)=1.e10
        ifmskscl=1
        write(*,'(1x,a,/,a,/,a,$)')'Enter lower & upper limits of X, '
     &      //'and lower & upper limits of Y,','   within which to'//
     &      ' include points for analysis;','   and 0 if in pixels'//
     &      ' or 1 if in microns (/ for no limits): '
        read(in5,*)(xymask(i),i=1,4),ifmskscl
        do i=1,4
          if(ifmskscl.eq.0)xymask(i)=xyscal*xymask(i)
          xymaskregion(i,nregion)=xymask(i)
        enddo
      endif
c       
      call get_objects(zstart,zend,xmt,ymt,zmt, indstrt, npntobj,
     &    icolor, nmt,iobjmod,iobjflag,limxyz,limwobj)
      modelregion(nregion)=modelfile
      tiltregion(nregion)=tiltfile
      zsclregion(nregion)=zscal
      xysclregion(nregion)=xyscal
      xofsregion(nregion)=xofs
      yofsregion(nregion)=yofs
      zofsregion(nregion)=zofs
      ifflipregion(nregion)=ifflip
      npntsregion(nregion)=nmt
      zstrtregion(nregion)=zstart
      zendregion(nregion)=zend
      ngapsregion(nregion)=ngaps
      indgapregion(nregion)=indgap
c       
c       count the types
c       
      do i=1,limtyp
        ninclass(i,nregion)=0
      enddo
      do i=1,nmt
        ity=icolor(i)
        iclass=itypcrosind(ity)
        if(iclass.eq.0)then
          ntypes=ntypes+1
          iclass=ntypes
          itypcrosind(ity)=ntypes
        endif
        if(iobjflag(abs(ity)).eq.1)then
          ninclass(iclass,nregion)=ninclass(iclass,nregion)+1
        else
          ninclass(iclass,nregion)=ninclass(iclass,nregion)+npntobj(i)
        endif
      enddo
c	  
      write(*,*)'Object   kind    number     name'
      do ity=-limflag,limflag
        i=itypcrosind(ity)
        objname=' '
        if(i.gt.0)then
          ierr = getimodobjname(abs(ity), objname)
          if(iobjflag(abs(ity)).eq.1)then
            write(*,'(i5,2x,a,i8,4x,a)')ity,'  lines',ninclass(i,nregion),
     &          objname
          else
            write(*,'(i5,2x,a,i8,4x,a)')ity,' points',ninclass(i,nregion),
     &          objname
          endif
        endif
        if(ity.gt.0.and.iobjflag(max(1,ity)).eq.4) then
          ierr = getimodobjname(abs(ity), objname)
          write(*,'(i5,2x,a,12x,a)')ity,' meshes',objname
        endif
      enddo
      write(*,*)
c       
      if(nregion.gt.1)go to 35
c       
      call getbinspec(delr,nbins,power,limfit,padbound,fracomit,
     &    ifbundend,samplen,ifcloseg,ifscatsurf)
      if(padbound.lt.0.)padbound=-xyscal*padbound
c       
      call getgraphspec(ngraph,itypref,nreftyp, itypneigh, nneightyp,
     &    iwhichend,ifbundend,iobjflag,limflag,irefflag,neighflag)
      maxgraph=ngraph
c       
c       
35    if(ifbundend.eq.0)then
        call closedist(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &      delr,nbins, ngraph,nreftyp,nneightyp,itypref,
     &      itypneigh,power,limfit,winmin,winmax,ninwin,graphs,areas,
     &      iobjwin,nobjwin,iobjmod,xyzend,endsep,samplen,
     &      ifcloseg,ifscatsurf,irefflag,neighflag,xyscal,
     &      zscal,powergrf,zgapst(indgap),
     &      zgapnd(indgap),ngaps,manyrandom,onlyshifted, nearestOnly)
      else
        call bundledist(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &      delr,nbins, ngraph,nreftyp,nneightyp,itypref, itypneigh,
     &      iwhichend,limfit,padbound,fracomit,graphs,areas,zscal,
     &      zstart,zend,xymask,winmin,winmax,iobjwin,nobjwin,xyzend)
      endif
      do ii=1,maxgraph
c         powergrf(ii)=power
        nbingrf(ii)=nbins
        delrgrf(ii)=delr
      enddo
      sampled=.false.
      shuffled=.false.
      converted=.false.
c       
c       if doing multiple regions, average these graphs into the ones
c       for previous regions.
c       
      if(nregion.gt.1)
     &    call addtoavg(graphs,areas,limbins,ngraph,nbins,nregion)
c       
c       display up to the first 4 new graphs
c       
      call fourdsp(graphs,limbins,1,min(4,ngraph),nbingrf,delrgrf,
     &    xmaxdsp, ymaxdsp,igrfdsp)
c       
      if(iopt.ne.-1)go to 40
38    write(*,104)
104   format(' 1/2: Type/Average selected bins of the graph in a',
     &    ' specified window',/,' 3: Compute integrated number',
     &    ' of (excess/missing) items in selected bins',/,
     &    ' 4/5: Display one graph in a window/Enter list of graphs',
     &    ' to display',/,
     &    ' 6/7: Rescale X or Y axis of one window/Y axis of all',
     &    ' windows',/, ' 8/9: Plot one window/all windows to',
     &    ' PostScript graphics file',/,
     &    ' 10/11: Output PostScript file to screen/printer'
     &    ,/,' 12: Output single or average graph to file',/,
     &    ' 13: Loop back to specify new range of Z to analyze',
     &    ' (or new model)',/, ' 14: Change radial weighting of',
     &    ' a graph',/,
     &    ' 15: Analyze new region and average with ',
     &    'previous region(s)',/, ' 16: Redo current region(s) with',
     &    ' new bin size, # of bins, or types for graphs',/,
     &    ' 17: Set min & max distances at which to compute angles',
     &    ' and add lines to model ',/, ' 18: Save bins of a graph'
     &    ,' to specify rejection probabilities for random points',/,
     &    ' 19/26/20: Do current region(s) with shuffled/converted',
     &    ' types or random shifts',/,' 21: Save current set of ',
     &    'objects and their types as an IMOD model',/,
     &    ' 22/27/23: Do many sets & integrals with ',
     &    'shuffled/converted types/random shifts',
     &    /,' 24: Take command input from file        25: Exit',
     &    /,' 28/29/30 Save a graph/Average/Combine 2 graphs into',
     &    ' an extra graph location',/,' 31/32: Save graph in',
     &    ' file/Read from file into an extra graph location',/,
     &    ' 33: Replace some sets of bins by their averages'
     &    ,/,' 37/38/39 Add list of graphs/Read',
     &    ' list of graphs from file/Read&Add from file',/,
     &    ' 40: Unshift an object',/,
     &    ' 41: Toggle between including and excluding items that ',
     &    'failed to shift',/,
     &    ' 42: Export graph values or points for drawing to file',/,
     &    ' 43: List distances of close approach between min/max limits',/,
     &    ' 44: Toggle between recording distances to all and nearest neighbors')
c       
40    write(*,'(1x,a,$)')'Option, or -1 for list of choices: '
      read(in5,*,err=40, end=225)iopt
      if(iopt.eq.-1)go to 38
      if (OptionNeedsModel(modelfile, ioptNeedModel,
     &    nOptNeedModel, iopt)) go to 40
      go to(201,202,203,204,205,206,207,208,209,210, 210,212,213,214,
     &    215,216,217,218,219,220, 221,222,222,224,225,226,222,228,
     &    228,228,228,228,228,213,235,235,228,228,228,240,241,228,243,244),iopt
      go to 40
c       
c       type bins
c       
201   ibtyst=1
      ibtynd=-1
      write(*,'(1x,a,$)')'Window # (- for raw counts), starting,'
     &    //' ending bins to type (/ for all): '
      read(in5,*)iwin,ibtyst,ibtynd
      ifraw=iwin
      iwin=abs(iwin)
      if(iwin.le.0.or.iwin.gt.4)go to 40
      if(igrfdsp(iwin).eq.0)go to 40
      jgrf=igrfdsp(iwin)
      if(ibtynd.eq.-1)ibtynd=nbingrf(jgrf)
      ibtynd=min(ibtynd,nbingrf(jgrf))
      ibtyst=max(1,ibtyst)
      linst=1+(ibtyst-1)/5
      linnd=1+(ibtynd-1)/5
      write(*,107)
107   format(12x,'1',14x,'2',14x,'3',14x,'4',14x,'5')
      do ilin=linst,linnd
        ib1=5*(ilin-1)
        ib2=min(ibtynd,ib1+5)
        if(ifraw.gt.0)then
          write(*,108)ib1,(graphs(ib,jgrf),ib=ib1+1,ib2)
108       format(i4,'/',f14.8,4f15.8)
        else
          write(*,1108)ib1,(nint(graphs(ib,jgrf)*areas(ib,jgrf))
     &        ,ib=ib1+1,ib2)
1108      format(i4,'/',i7,4i15)
        endif
      enddo
      go to 40
c       
c       average bins
c       
202   write(*,'(1x,a,i2,2i4,a$)')'Window #, starting, ending bins'//
     &    ' to average [',iwin,ibavst,ibavnd,']: '
      read(in5,*)iwin,ibavst,ibavnd
      if(iwin.le.0.or.iwin.gt.4)go to 40
      if(igrfdsp(iwin).eq.0)go to 40
      jgrf=igrfdsp(iwin)
      ibavnd=min(ibavnd,nbingrf(jgrf))
      ibavst=max(1,ibavst)
      nbavg=ibavnd+1-ibavst
      call avgsd(graphs(ibavst,jgrf),nbavg,avgden,sdden,semden)
      write(*,109)nbavg,avgden,sdden,semden
109   format(i4,' bins, mean =',f15.8,', SD =',f15.9,', SEM =',f15.9)
      baseval=avgden
      areasum=0.
      countsum=0.
      do ibin=ibavst,ibavnd
        areasum=areasum+areas(ibin,jgrf)
        countsum=countsum+areas(ibin,jgrf)*graphs(ibin,jgrf)
      enddo
      trueavg=0.
      if(areasum.gt.0.)trueavg=countsum/areasum
      write(*,1109)trueavg
1109  format(' True area-weighted average =',f15.8)
      go to 40
c       
c       integrate bins
c       
203   write(*,'(1x,a,/,a,i2,2i4,f15.8,a,$)')'Enter window #, starting'
     &    //' & ending bins to integrate,','     and base value '//
     &    'to subtract [',iwin,ibinst,ibinnd,baseval,']: '
      read(in5,*)iwin,ibinst,ibinnd,baseval
      if(iwin.le.0.or.iwin.gt.4)go to 40
      if(igrfdsp(iwin).eq.0)go to 40
      jgrf=igrfdsp(iwin)
      ibinnd=min(ibinnd,nbingrf(jgrf))
      ibinst=max(1,ibinst)
      call integrate(graphs(1,jgrf),areas(1,jgrf),nbingrf(jgrf),
     &    delrgrf(jgrf),powergrf(jgrf),ibinst,ibinnd,0,0, baseval,sum,centroid)
      write(*,110)ibinnd+1-ibinst,(ibinst-1)*delrgrf(jgrf),
     &    ibinnd*delrgrf(jgrf),sum,centroid
110   format(' For the',i4,' bins from',
     &    f10.3,' to',f10.3,/,5x,
     &    'the integrated number of (excess/missing) items is',f10.5/,5x,
     &    'the centroid of their distances is', f11.4)
      go to 40
c       
c       Display graph in window
c       
204   write(*,'(1x,a,$)')
     &    'Display graph in window; Enter graph # and window #: '
      read(in5,*)jgrf,iwin
      if(iwin.le.0.or.iwin.gt.4.or.
     &    .not.checkgrf(jgrf,maxgraph,nextragrf,listextra)) go to 40
      xmaxdsp(iwin)=-1.
      ymaxdsp(iwin)=-1.
      igrfdsp(iwin)=jgrf
      call graphdsp(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),iwin,
     &    jgrf,xmaxdsp(iwin), ymaxdsp(iwin))
      go to 40
c       
c       display list of graphs in windows
c       
205   write(*,'(1x,a,$)')'List of graphs to display (ranges OK): '
      do jj=1,4
        igrfdsp(jj)=0
      enddo
      call rdlist(in5,igrfdsp,ndisp)
      if(ndisp.eq.0)go to 40
      do jj=1,min(4,ndisp)
        if(.not.checkgrf(igrfdsp(jj),maxgraph,nextragrf,listextra))
     &      go to 205
      enddo
      call somedsp(graphs,limbins,nbingrf,delrgrf,xmaxdsp,ymaxdsp,
     &    igrfdsp)
      go to 40
c       
c       Rescale X or Y axis of graph in window
c       
206   write(*,'(1x,a,$)')
     &    'Number of window to rescale; 0 or 1 to rescale X or Y: '
      read(in5,*)iwin,ifxy
      if(iwin.le.0.or.iwin.gt.4)go to 40
      jgrf=igrfdsp(iwin)
      if(jgrf.eq.0)go to 40
      if(ifxy.eq.0)then
        rmax=nbingrf(jgrf)*delrgrf(jgrf)
        write(*,106)' X',rmax,xmaxdsp(iwin)
106     format(a,' ranges up to',f13.6,' and current full scale',
     &      ' value is',f13.6,/,'   Enter new full scale value: ',$)
        read(in5,*)xmaxdsp(iwin)
      else
        ymax=0.
        do i=1,nbingrf(jgrf)
          ymax=max(ymax,graphs(i,igrfdsp(iwin)))
        enddo
        write(*,106)' Y',ymax,ymaxdsp(iwin)
        read(in5,*)ymaxdsp(iwin)
      endif
      call graphdsp(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),iwin,
     &    jgrf, xmaxdsp(iwin),ymaxdsp(iwin))
      go to 40
c       
c       rescale all graphs to same maximum
c       
207   yallmax=0
      do iwn=1,4
        jgrf=igrfdsp(iwn)
        if(jgrf.gt.0)then
          do ibin=1,nbingrf(jgrf)
            yallmax=max(yallmax,graphs(ibin,jgrf))
          enddo
        endif
        call scale(0,0.9999*yallmax,dytmp,ylotmp)
        yallmax=10.*dytmp
      enddo
      do iwn=1,4
        jgrf=igrfdsp(iwn)
        if(jgrf.gt.0)then
          ymaxdsp(iwn)=yallmax
          call graphdsp(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),
     &        iwn,jgrf, xmaxdsp(iwn),ymaxdsp(iwn))
        endif
      enddo
      go to 40
c       
c       Plot one window to metacode file
c       
208   write(*,'(1x,a,$)')
     &    'Window number to plot, plot number or 0 to specify plot: '
      read(in5,*)iwin,iplot
      if(iwin.le.0.or.iwin.gt.4)go to 40
      jgrf=igrfdsp(iwin)
      if(jgrf.eq.0)go to 40
      if(ifanyplot.ne.0)then
        write(*,2081)
2081    format(' 0 for plot on same page as previous plot(s),',
     &      ' 1 for new page: ',$)
        read(in5,*)ifpag
        call psSetup(1,c1,c2,c3,0)
        if(ifpag.ne.0)call psFrame
      endif
      call graphplt(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),iplot,
     &    jgrf, xmaxdsp(iwin),ymaxdsp(iwin))
      ifanyplot=1
      go to 40
c       
c       Quick plot all 4 windows
c       
209   if(ifanyplot.ne.0)then
        write(*,2081)
        read(in5,*)ifpag
        call psSetup(1,c1,c2,c3,0)
        if(ifpag.ne.0)call psFrame
      endif
      do iwn=1,4
        jgrf=igrfdsp(iwn)
        if(jgrf.gt.0)then
          call graphplt(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),
     &        iwn,jgrf, xmaxdsp(iwn),ymaxdsp(iwn))
        endif
      enddo
      ifanyplot=1
      go to 40
c       
c       Output plot file to workstation or printer
c       
210   call pltout(11-iopt)
      ifanyplot=0
      go to 40
c       
c       write graph to file with lots of info
c       
212   write(*,'(1x,a,$)')'Graph #: '
      read(in5,*)jgrf
      if(.not.checkgrf(jgrf,maxgraph,nextragrf,listextra))go to 40
      if(jgrf.le.ngraph)then
        iregst=nregion
        jgrbas=jgrf
      else
        iregst=1
        jgrbas=jgrf-ngraph
      endif
      write(iout,111)modelfile
111   format(' Model file:  ',a)
      nintot=0
      do ir=iregst,nregion
        nintmp(ir)=0
        do ity=1,nreftyp(jgrbas)
          if(itypref(ity,jgrbas).eq.itypall)then
            nintmp(ir)=npntsregion(ir)
            go to 362
          endif
          itcr=itypcrosind(itypref(ity,jgrbas))
          if(itcr.ne.0)nintmp(ir)=nintmp(ir)+ninclass(itcr,ir)
        enddo
362     nintot=nintot+nintmp(ir)
      enddo
      write(iout,112)' Reference items:',(nintmp(i),i=iregst,nregion)
112   format(a,(t20,6i10))
      if(iregst.ne.nregion)write(iout,112)'         Total:',nintot
c       
      nintot=0
      do ir=iregst,nregion
        nintmp(ir)=0
        do ity=1,nneightyp(jgrbas)
          if(itypneigh(ity,jgrbas).eq.itypall)then
            nintmp(ir)=npntsregion(ir)
            go to 364
          endif
          itcr=itypcrosind(itypneigh(ity,jgrbas))
          if(itcr.ne.0)nintmp(ir)=nintmp(ir)+ninclass(itcr,ir)
        enddo
364     nintot=nintot+nintmp(ir)
      enddo
      write(iout,112)' Neighboring items:'
     &    ,(nintmp(i),i=iregst,nregion)
      if(iregst.ne.nregion)write(iout,112)'         Total:',nintot
c       
      write(iout,114)' Reference objects:',
     &    (itypref(ity,jgrbas),ity=1,nreftyp(jgrbas))
114   format(a,(12i5))
      write(iout,114)' Neighboring objects:',
     &    (itypneigh(ity,jgrbas),ity=1,nneightyp(jgrbas))
      write(iout,'(a,f12.5)')' Delta r =',delrgrf(jgrf)
      write(iout,'(5g15.8)')(graphs(i,jgrf),i=1,nbingrf(jgrf))
      go to 40
c       
c       Specify new boundary object: but check first if averaging
c       8/5/03: remove confirmation to make it easier to do command files
c       
c$$$    213	if(nregion.gt.1)then
c$$$    write(*,'(1x,a,$)')'This will destroy the stored average.'//
c$$$    &	      '  Enter 1 to do so: '
c$$$    read(in5,*)ifreally
c$$$    if(ifreally.ne.1)go to 40
c$$$    endif
213   if(iopt.eq.13)go to 10
      if(nobjwin.gt.0.and.winmin.lt.winmax)then
        do iow=1,nobjwin
          if (iobjwin(iow).gt.0) iobjwin(iow)=iobjmod(iobjwin(iow))
        enddo
        nobjwin=-nobjwin
      endif
      if(winmin.lt.winmax)then
        write(*,'(1x,a,$)')'New min and max distances: '
        read(in5,*)winmin,winmax
      endif
      ifbundend=max(1,ifbundend)-ifbundend
      go to 10
c       
c       change power of a graph in a window
c       
214   write(*,'(1x,a,$)') 'Change power of graph; Enter window #'//
     &    ' and new power: '
      read(in5,*)iwin,pownew
      if(iwin.le.0.or.iwin.gt.4)go to 40
      jgrf=igrfdsp(iwin)
      if(jgrf.eq.0)go to 40
      xmaxdsp(iwin)=-1.
      ymaxdsp(iwin)=-1.
      do ibin=1,nbingrf(jgrf)
        rr2=2.*delrgrf(jgrf)*(ibin-0.5)
        facold=1.
        if(powergrf(jgrf).ne.0.)facold=rr2**powergrf(jgrf)
        facnew=1.
        if(pownew.ne.0.)facnew=rr2**pownew
        areas(ibin,jgrf)=areas(ibin,jgrf)*facnew/facold
        graphs(ibin,jgrf)=graphs(ibin,jgrf)*facold/facnew
      enddo
      powergrf(jgrf)=pownew
      call graphdsp(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),iwin,
     &    jgrf,xmaxdsp(iwin), ymaxdsp(iwin))
      go to 40
c       
c       Specify new region to analyze and average with previous region(s)
c       
215   if(2*ngraph.gt.limgraphs)then
        print *,'Too many graphs to accumulate averages'
        go to 40
      endif
      if(sampled.or.shuffled.or.converted)then
        print *,'Data have been altered; need to redo previous'//
     &      ' regions before proceeding...'
        forceload=.true.
        ifshuffle=0
        ifsample=0
        ifconvert=0
        grapheach=.true.
        call redoRegions()
      endif
      if(nregion.eq.1)then
        write(*,'(a,i2,a,i2)')' The averages will be'//
     &      ' stored in graphs ',ngraph+1,' to ',2*ngraph
        call addtoavg(graphs,areas,limbins,ngraph,nbins,nregion)
      endif
      maxgraph=2*ngraph
      nregion=nregion+1
      go to 12
c       
c       do other kind of histogram on region(s)
c       
217   write(*,'(1x,a,$)')'Min & max distance for adding connecting'
     &    //' lines and computing angles: '
      read(in5,*)winmin,winmax
c       
c       Redo region(s) with new bins and/or graphs
c       
216   call getbinspec(delr,nbins,power,limfit,padbound,fracomit,
     &    ifbundend,samplen,ifcloseg,ifscatsurf)
      if(padbound.lt.0.)padbound=-xyscal*padbound
c       
2161  call getgraphspec(ngraph,itypref,nreftyp, itypneigh, nneightyp,
     &    iwhichend,ifbundend,iobjflag,limflag,irefflag,neighflag)
c       
c       check legality with any other model files
c       
      do ireg=1,nregion
        if(modelfile.ne.modelregion(ireg))then
          modelfile=modelregion(ireg)
          tiltfile=tiltregion(ireg)
          zscal=zsclregion(ireg)
          xyscal=xysclregion(ireg)
          xofs=xofsregion(ireg)
          yofs=yofsregion(ireg)
          zofs=zofsregion(ireg)
          ifflip=ifflipregion(ireg)
          ngaps=ngapsregion(ireg)
          indgap=indgapregion(ireg)
          call read_model(modelfile,tiltfile,xyscal,zscal,xofs,yofs,
     &        zofs,ifflip, iobjflag, limflag,zgapst(indgap),
     &        zgapnd(indgap),ngaps)
          call checkflags(ngraph,itypref, nreftyp, itypneigh,
     &        nneightyp, iobjflag,limflag,irefflag,neighflag)
          if(irefflag.lt.0)then
            print *,'You need to enter graph specifications that ',
     &          'will work for all models'
            go to 2161
          endif
        endif
      enddo
      maxgraph=ngraph*min(2,nregion)
      forceload=nregion.eq.1.and.(shuffled.or.sampled.or.converted)
      ifshuffle=0
      ifsample=0
      ifconvert=0
c       
c       general place to redo regions,
c       
315   if(forceload)then
        write(*,'(1x,a,/,a,$)')'The data to be analyzed have been'//
     &      ' modified/randomized.',' Enter 1 to work with the'//
     &      ' altered data or 0 to reload original data: '
        read(in5,*)ifdorand
        forceload=ifdorand.eq.0
      endif
      grapheach=.true.
      call redoRegions()
      if(nregion.gt.1)call fourdsp(graphs,limbins,ngraph+1,ngraph
     &    +min(4,ngraph),nbingrf,delrgrf,xmaxdsp,ymaxdsp,igrfdsp)
      go to 40
c       
c       save bins of some graph to specify restriction on distances
c       
218   write(*,'(1x,a,$)')
     &    'Graph #, baseline level corresponding to probability 1.0: '
      read(in5,*)jgrf,base
      if(.not.checkgrf(jgrf,maxgraph,nextragrf,listextra))go to 40
      nbinsave=0
      delsave=delrgrf(jgrf)
      do while(nbinsave.lt.nbingrf(jgrf).and.
     &    graphs(nbinsave+1,jgrf).lt.base)
        nbinsave=nbinsave+1
        probsave(nbinsave)=graphs(nbinsave,jgrf)/base
      enddo
      if(nbinsave.gt.limprobs)then
        print *,nbinsave,
     &      ' bins would be saved - too many for the arrays'
        nbinsave=0
      else
        print *,nbinsave,' probability bins saved'
      endif
      go to 40
c       
c       shuffle types
c       
219   ifshuffle=1
      forceload=nregion.eq.1.and.(sampled.or.converted)
      ifsample=0
      ifconvert=0
      if(iopt.eq.19)go to 315
      go to 319
c       
c       convert types
c       
226   ifshuffle=0
      forceload=nregion.eq.1.and.(sampled.or.converted.or.shuffled)
      ifsample=0
      ifconvert=1
      if(nchange.gt.0)then
        write(*,'(1x,a,$)')
     &      '0 to use last conversions, 1 to specify new ones: '
        read(in5,*)ifnewconv
        if(ifnewconv.ne.0)nchange=0
      endif
      if(nchange.eq.0)then
        write(*,'(1x,a,$)')'Number of objects to convert: '
        read(in5,*)nchange
        print *,'For each conversion, enter the object to change',
     &      ' from, the object to change to,',
     &      ' and the fraction of contours of that object to convert.'
        do i=1,nchange
          write(*,'(1x,a,i3,a$)')'Conversion',i,': '
          read(in5,*)ityfrom(i),ityto(i),chngfrac(i)
        enddo
      endif
      if(iopt.eq.26)go to 315
      go to 319
c       
c       do series of control sets: first see if need to recompute graphs
c       
222   if(shuffled.or.sampled.or.converted)then
        print *,'Data were modified/randomized; must rebuild '//
     &      'actual graphs to get integrals...'
        ifshuffle=0
        ifsample=0
        ifconvert=0
        forceload=.true.
        grapheach=.false.
        call redoRegions()
      endif
      if(iopt.eq.22)go to 219
      if(iopt.eq.27)go to 226
c       
c       shift objects randomly in X/Y
c       
220   write(*,'(1x,a,$)')'Minimum and maximum distance to shift'//
     &    'in X/Y plane: '
      read(in5,*)ranmin,ranmax
c       
      write(*,'(1x,a,$)')'Maximum amount to shift in Z relative '
     &    //'to X/Y shift (0 for no Z shift): '
      read(in5,*)ranzrel
c       
      print *,'Enter list of objects to shift',
     &    ' (Return for all)'
      call rdlist(in5,itypshift,nshiftyp)
      if(nshiftyp.eq.0)then
        itypshift(1)=itypall
        do i=1,limflag
          if(iobjflag(i).ge.0)then
            nshiftyp=nshiftyp+1
            itypshift(nshiftyp)=i
          endif
        enddo
      endif
      ishiftflag=-1
      do i=1,nshiftyp
        ityp=abs(itypshift(i))
        if(iobjflag(ityp).ge.0)then
          if(ishiftflag.lt.0)then
            ishiftflag=iobjflag(ityp)
          elseif(iobjflag(ityp).ne.ishiftflag)then
            print *,'Object types do not all match'
            go to 40
          endif
        endif
      enddo
c       
      print *,'Enter list of other objects to check'//
     &    ' distances from' ,' (Return for all other)'
      call rdlist(in5,itypchck,nchcktyp)
      if(nchcktyp.eq.0)then
        itypchck(1)=itypall
        do i=1,limflag
          if(iobjflag(i).ge.0)then
            noton=1
            do j=1,nshiftyp
              if(itypshift(j).eq.i)noton=0
            enddo
            if(noton.eq.1)then
              nchcktyp=nchcktyp+1
              itypchck(nchcktyp)=i
            endif
          endif
        enddo
      endif
      do i=1,nshiftyp
        do j=1,nchcktyp
          if(itypshift(i).eq.itypchck(j))then
            print *,itypshift(i),' is in both lists'
            go to 40
          endif
        enddo
      enddo
c       
      write(*,'(1x,a,$)')'# of probability curves to use '//
     &    'for rejection of close spacings: '
      read(in5,*)nprobsets
      if(nprobsets.gt.limprobsets)then
        print *,'Too many curves for arrays'
        go to 40
      endif
      if(nprobsets.eq.0)then
        nrestrict(1)=0
        nprobsets=1
      else
        if(nprobsets.gt.1)print *,'The first set will be ',
     &      'applied to the distances between shifted items'
        do j=1,nprobsets

          ifusesave=0
          if(nbinsave.gt.0.and.j.eq.1)then
            write(*,'(1x,a,$)') '1 to use saved probability '//
     &          'bins for the first curve, 0 not to: '
            read(in5,*)ifusesave
          endif
c           
          if(ifusesave.ne.0)then
            nrestrict(j)=nbinsave
            delnear(j)=delsave
            do ii=1,nbinsave
              probnear(ii,j)=probsave(ii)
            enddo
          else
c             
            write(*,'(1x,a,i3,a,$)')'Curve #',j,
     &          ': Enter # of bins for rejection and  bin size: '
            read(in5,*)nrestrict(j),delnear(j)
            if(nrestrict(j).gt.limprobs)then
              print *,'Too many bins for arrays'
              go to 40
            endif
            if(nrestrict(j).gt.0)then
              write(*,'(1x,a,$)')'Probability values: '
              read(in5,*)(probnear(i,j),i=1,nrestrict(j))
            endif
          endif
        enddo
      endif
      if(nprobsets.gt.1.and.nchcktyp.gt.0)then
        write(*,'(1x,a,i3,/,a,$)')'Enter the # of the curve to '//
     &      'use for each of the',nchcktyp,
     &      'objects being checked against: '
        read(in5,*)(iuseprob(i),i=1,nchcktyp)
        do i=1,nchcktyp
          if(iuseprob(i).lt.1.or.iuseprob(i).gt.nprobsets)then
            print *,'Illegal curve number'
            go to 40
          endif
        enddo
      else
        do i=1,nchcktyp
          iuseprob(i)=1
        enddo
      endif
c       
      write(*,'(1x,a,$)')'Maximum distance to shift outside '//
     &    'bounding box of original data: '
      read(in5,*)boxtol
      write(*,'(1x,a,$)')
     &    'Object # of object with bounding contours, or 0 if none: '
      read(in5,*)iobjbound
      write(*,'(1x,a,/,a,$)')'1 to check shifted items against'//
     &    ' ones yet to be shifted, or 0 to check',
     &    ' only against ones that have been shifted already: '
      read(in5,*)ifcheckunshifted
      write(*,'(1x,a,$)')'Maximum number of trials: '
      read(in5,*)maxtrials
      write(*,'(1x,a,$)')'# of trials per cycle, factor to change '
     &    //'maximum shift by per cycle: '
      read(in5,*)ntrialcycle,cyclefac
      ifshuffle=0
      ifsample=1
      ifconvert=0
      forceload=nregion.eq.1.and.(sampled.or.converted.or.shuffled)
      if(iopt.eq.20)go to 315
c       
c       general setup to do series of control runs and gather statistics
c       
319   jgrfadd=0
      if(nregion.gt.1)jgrfadd=ngraph
      write(*,'(1x,a,$)')'0 to specify integral for each graph '//
     &    'separately, 1 to use same bins for all: '
      read(in5,*)ifallsame
c       
      do jj=1,ngraph
        if(ifallsame.eq.0)write(*,'(10x,a,i3)')
     &      'Specify bins and baseline for graph #',jj+jgrfadd
c         
        if(ifallsame.eq.0.or.jj.eq.1)then
321       write(*,'(1x,a,$)')'Starting and ending bins to integrate: '
          read(in5,*)integstrt(jj),integend(jj)
          if(integend(jj).lt.integstrt(jj).or.integstrt(jj).lt.1
     &        .or.integend(jj).gt.nbins)go to 321
c           
322       write(*,'(1x,a,$)')'Start & end bins to compute baseline'//
     &        ' from, or 0,0 to use fixed value: '
          read(in5,*)ibasestrt(jj),ibasend(jj)
          if((ibasestrt(jj).ne.0.or.ibasend(jj).ne.0) .and.
     &        (ibasend(jj).lt.ibasestrt(jj) .or. ibasend(jj).gt.nbins
     &        .or. ibasestrt(jj).lt.1)) go to 322
c           
          if(ibasestrt(jj).eq.0 .and. ibasend(jj).eq.0)then 
            write(*,'(1x,a,$)')'Fixed baseline value: '
            read(in5,*)baseline(jj)
          endif
c           
        else
          integstrt(jj)=integstrt(1)
          integend(jj)=integend(1)
          ibasestrt(jj)=ibasestrt(1)
          ibasend(jj)=ibasend(1)
          baseline(jj)=baseline(1)
        endif
c         
        nrandabove(jj)=0
        suminteg(jj)=0
        sumsqinteg(jj)=0
        call integrate(graphs(1,jj+jgrfadd),areas(1,jj+jgrfadd),nbins,
     &      delr, powergrf(jj+jgrfadd),integstrt(jj),integend(jj),
     &      ibasestrt(jj),ibasend(jj), baseline(jj),realinteg(jj),centroid)
        write(*,116)jj+jgrfadd,realinteg(jj)
116     format(' Graph #',i3,', real integral =',f10.5)
      enddo
c       
      write(*,'(1x,a,$)')'Enter 1 to accumulate mean and standard '
     &    //'deviation graphs or 0 not to: '
      read(in5,*)ifdomeansd

      maxtmp=jgrfadd+3*ngraph
      if (ifdomeansd.ne.0) then
        if(maxtmp.le.limgraphs)then
          write(*,'(1x,a,i3,a,i3)')'Mean and standard deviation'
     &        //' graphs will be stored in graphs',
     &        jgrfadd+ngraph+1,' to',maxtmp
        else
          ifdomeansd=0
          print *,'Too many graphs to accumulate mean and SD graphs'
        endif
      endif
      if(ifdomeansd.ne.0)maxgraph=maxtmp
      ntotcontrol=0
c       
324   write(*,'(1x,a,$)')
     &    'Number of control sets to run, or 0 to enter new option: '
      read(in5,*)ndocontrol
      if(ndocontrol.le.0)go to 40
      if(ntotcontrol.ne.0.and.ndocontrol.ne.0)then
        write(*,'(1x,a,$)')'Enter 1 if you really want to do more'//
     &      ' sets, or 0 if that was a mistake: '
        read(in5,*)ifreally
        if(ifreally.ne.1)go to 40
      endif
      print *,' '
c       
      icont=1
c       
c       Get graphs from all regions as needed
c       
424   ntotcontrol=ntotcontrol+1
      write(*,'(a,i4,$)')char(13)//'Working on set',ntotcontrol
      call flush(6)
c       if(sampled)forceload=nregion.eq.1
      manyrandom=1
      grapheach=.false.
      call redoRegions()
c       
c       Get integrals from graphs and accumulate statistics
c       
      manyrandom=0
      do jj=1,ngraph
        call integrate(graphs(1,jj+jgrfadd),areas(1,jj+jgrfadd),nbins,
     &      delr, powergrf(jj+jgrfadd),integstrt(jj),integend(jj),
     &      ibasestrt(jj),ibasend(jj), baseline(jj),randinteg,centroid)
        if(randinteg.gt.realinteg(jj))
     &      nrandabove(jj)=nrandabove(jj)+1
        suminteg(jj)=suminteg(jj)+randinteg
        sumsqinteg(jj)=sumsqinteg(jj)+randinteg**2
      enddo
c       
c       accumulate mean&SD if desired
c       
      if(ifdomeansd.ne.0)then
        do jj=1+jgrfadd,ngraph+jgrfadd
          jmean=jj+ngraph
          jsd=jmean+ngraph
          do ibin=1,nbins
            if(ntotcontrol.eq.1)then
              graphs(ibin,jmean)=graphs(ibin,jj)
              graphs(ibin,jsd)=0.
              areas(ibin,jmean)=areas(ibin,jj)
              areas(ibin,jsd)=areas(ibin,jj)
              powergrf(jmean) = powergrf(jj)
              powergrf(jsd) = powergrf(jj)
            else
              sumbin=graphs(ibin,jmean)*(ntotcontrol-1)
              sumsqbin=graphs(ibin,jsd)*sqrt(ntotcontrol-2.)
     &            +sumbin**2/(ntotcontrol-1)
              sumbin=sumbin+graphs(ibin,jj)
              sumsqbin=sumsqbin+graphs(ibin,jj)**2
              call sums_to_avgsd(sumbin,sumsqbin,ntotcontrol,
     &            graphs(ibin,jmean),graphs(ibin,jsd))
              areas(ibin,jmean)=areas(ibin,jmean)+areas(ibin,jj)
              areas(ibin,jsd)=areas(ibin,jmean)
            endif
          enddo
        enddo
      else
      endif
      icont=icont+1
      if(icont.le.ndocontrol)go to 424
c       
      do iun=6,iout
        write(iun,117)ntotcontrol
117     format(/,' Total # of controls:',i6,/,' Graph    real   ',
     &      '    random_integrals     higher_than_real',/,
     &      '   #    integral      mean     S.D.          #      %')
        do jj=1,ngraph
          call sums_to_avgsd(suminteg(jj),sumsqinteg(jj),ntotcontrol,
     &        avgint,sdint)
          pctabove=100.*nrandabove(jj)/ntotcontrol
          write(iun,118)jj+jgrfadd,realinteg(jj),avgint,sdint,
     &        nrandabove(jj), pctabove
118       format(i5,f10.5,f12.5,f10.6,i10,f8.2)
        enddo
      enddo
      go to 324
c       
c       save current objects as model file
c       
221   call save_model(xyscal,zscal,xofs,yofs,zofs,ifflip,iobjflag,xmt,
     &    ymt, zmt, indstrt, npntobj, icolor,nmt,ninwin,iobjwin,
     &    nobjwin,iobjmod, endsep)
      call read_model(modelfile,tiltfile,xyscal,zscal,xofs,yofs,zofs,
     &    ifflip,iobjflag, limflag,zgapst(indgap),
     &    zgapnd(indgap),ngaps)
      go to 40
c       
c       Switch to reading in a command file
c       
224   call opencomfile
      go to 40
c       
c       exit
c       
225   call scrnClose
      call psExit
c       
c       call to manipulate graphs
c       
228   do i=1,limgraphs
        dum(i)=0.
        idum(i)=0
      enddo
      call manipgraphs(iopt,'mtk',graphs,areas,nbingrf,delrgrf,
     &    idum,dum,powergrf,maxgraph,nextragrf,listextra,
     &    igrfdsp,xmaxdsp,ymaxdsp)
      go to 40
c       
c       process end separations
c       
235   if(nobjwin.ge.0)go to 40
      write(*,'(1x,a,$)')
     &    'Graph # to place histogram in, bin width, # of bins: '
      read(in5,*)igrfextra,delhist,nbhist
      if(nbhist.le.0.or.delhist.le.0.)go to 40
      if(.not.checkextra(igrfextra,limgraphs,listextra,
     &    nextragrf))go to 40
      do ibin=1,nbhist
        areas(ibin,igrfextra)=1.
        graphs(ibin,igrfextra)=0.
      enddo
      nbingrf(igrfextra)=nbhist
      delrgrf(igrfextra)=delhist
      powergrf(igrfextra)=1.
      if(iopt.eq.35)then
        nout=0
        nmax=0
        nin=0
        do iow=1,-nobjwin
          if(endsep(iow).lt.0.)then
            nout=nout+1
          elseif(endsep(iow).eq.1.e10)then
            nmax=nmax+1
          else
            nin=nin+1
            ibin=endsep(iow)/delhist+1.
            if(ibin.le.nbhist)
     &          graphs(ibin,igrfextra)=graphs(ibin,igrfextra)+1
          endif
        enddo
        write(*,1235)-nobjwin,nin,nmax,nout
1235    format(i5,' total ends,',i4,' with close approaches,',i4,
     &      ' without,',i4,' not in model')
      else
        indst=indstrt(nmt)+npntobj(nmt)
        do ind=indst,indst+ninwin-1
          ibin=xmt(ind)/delhist+1.
          if(ibin.le.nbhist)
     &        graphs(ibin,igrfextra)=graphs(ibin,igrfextra)+1
        enddo
        print *,ninwin,' total distances between close approaches',
     &      ' and ends'
      endif
      go to 40
c       
c       unshift selected object
c       
240   write(*,'(1x,a,$)')
     &    'Object # to unshift: '
      read(in5,*)iobjshift
      call unshift_object(iobjshift,iobjflag(iobjshift),
     &    xmt,ymt,zmt,icolor,nmt, indstrt,npntobj,xyscal,zscal,
     &    zgapst(indgap),zgapnd(indgap), ngaps)
      go to 40
241   onlyshifted=.not.onlyshifted
      if (onlyshifted)then
        print *,'Items that failed to shift will be EXCLUDED'
      else
        print *,'Items that failed to shift will be INCLUDED'
      endif
      go to 40

243   if (ninwin .le. 0) then
        print *,'No distances have been found in window'
        go to 40
      endif
      sum = 0.
      sumsq = 0.
      print *,'Length of connector lines:'
      do i = 1, ninwin
        ii = indstrt(nmt)+npntobj(nmt) + 3 * (i - 1)
        dist = sqrt((xmt(ii) - xmt(ii+2))**2 + (ymt(ii) - ymt(ii+2))**2 +
     &      (zmt(ii) - zmt(ii+2))**2)
        sum = sum + dist
        sumsq = sumsq + dist**2
        write(*,'(f12.5)')dist
      enddo
      call sums_to_avgsd(sum, sumsq, ninwin, connavg, connsd)
      write(*,1243)ninwin, connavg, connsd
1243  format('N = ',i7,'   mean = ',f12.5,'   SD = ',f12.5)
      go to 40

244   nearestOnly = .not. nearestOnly
      if (nearestOnly) then
        print *,'Only distances to nearest neighbor will be recorded; redoing regions'
      else
        print *,'Distances to all neighbors will be recorded; redoing regions'
      endif
      forceload=nregion.eq.1.and.(sampled.or.converted.or.shuffled)
      go to 315


      CONTAINS
c       
c       
c       
c       general code to redo regions, optionally shuffling points or randomly
c       sampling them.  It manages the logicals SHUFFLED and SAMPLED to
c       reflect the actual state of points in memory.
c       This is set up to be used like a subroutine; it's not a subroutine
c       because there would have been so many arguments.  It is expecting:
c       IFSAMPLE = 1 to randomly sample points
c       IFSHUFFLE =1 to shuffle types
c       IFCONVERT = 1 to convert fraction of some types to other types
c       FORCELOAD true to force model to get reloaded even if only one region
c       GRAPHEACH true to display graphs after each region
c       
      subroutine redoRegions()
      do ireg=1,nregion
        if(nregion.gt.1.or.forceload)then
          if(modelfile.ne.modelregion(ireg).or.forceload)then
            modelfile=modelregion(ireg)
            tiltfile=tiltregion(ireg)
            zscal=zsclregion(ireg)
            xyscal=xysclregion(ireg)
            xofs=xofsregion(ireg)
            yofs=yofsregion(ireg)
            zofs=zofsregion(ireg)
            ifflip=ifflipregion(ireg)
            ngaps=ngapsregion(ireg)
            indgap=indgapregion(ireg)
            call read_model(modelfile,tiltfile,xyscal,zscal,xofs,yofs,
     &          zofs,ifflip, iobjflag, limflag,zgapst(indgap),
     &          zgapnd(indgap),ngaps)
            onlyshifted=.false.
          endif
          zstart=zstrtregion(ireg)
          zend=zendregion(ireg)
          call get_objects(zstart,zend,xmt,ymt, zmt, indstrt,npntobj,
     &        icolor,nmt,iobjmod,iobjflag,limxyz,limwobj)
          shuffled=.false.
          sampled=.false.
          converted=.false.
        endif
        if(ifshuffle.ne.0)then
          call shuffle(icolor,nmt)
          shuffled=.true.
        endif
        if(ifsample.ne.0)then
          call random_shifts(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &        iobjflag,ranmin,ranmax,probnear,limprobs,delnear,
     &        nrestrict,nshiftyp, itypshift, ishiftflag,
     &        nchcktyp, itypchck,iuseprob,zgapst(indgap),
     &        zgapnd(indgap),ngaps,boxtol, iobjbound,
     &        ifcheckunshifted,ranzrel,
     &        maxtrials,ntrialcycle,cyclefac,ifscatsurf,xyscal,zscal,
     &        manyrandom,ifexcludeout)
          sampled=.true.
        endif
        if(ifconvert.ne.0)then
          call change_type(icolor,nmt,ityfrom,ityto,chngfrac,nchange)
          converted=.true.
        endif
c         
        if(ifbundend.eq.0)then
          call closedist(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &        delr,nbins, ngraph,nreftyp,nneightyp,itypref,
     &        itypneigh,power,limfit,winmin,winmax,ninwin,graphs,
     &        areas, iobjwin,nobjwin,iobjmod,xyzend,endsep,
     &        samplen,ifcloseg,ifscatsurf,irefflag,neighflag,
     &        xyscal,zscal,powergrf,zgapst(indgap),
     &        zgapnd(indgap),ngaps,manyrandom,onlyshifted, nearestOnly)
        else
          call bundledist(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &        delr,nbins,ngraph,nreftyp,nneightyp,itypref,itypneigh,
     &        iwhichend,limfit,padbound,fracomit,graphs,areas,zscal,
     &        zstart,zend,xymaskregion(1,ireg),winmin,winmax,iobjwin,
     &        nobjwin,xyzend)
        endif
        do ii=1,maxgraph
c           powergrf(ii)=power
          nbingrf(ii)=nbins
          delrgrf(ii)=delr
        enddo
c         
        if(nregion.gt.1)
     &      call addtoavg(graphs,areas,limbins,ngraph,nbins,ireg)
        if(grapheach)call fourdsp(graphs,limbins,1,min(4,ngraph),
     &      nbingrf,delrgrf, xmaxdsp, ymaxdsp,igrfdsp)
      enddo
      end subroutine redoRegions
c       
      end

c       NDA (Neighbor density analysis) is a program for analysis of spatial
c       point patterns where points may be of different types.  It produces
c       graphs of point density versus distance from a point and graphs of
c       point density as a function of angular separation between neighbors
c       to a reference point.  It can shuffle the types of points and create
c       random patterns of points within the boundary containing the real
c       points so as to evaluate the statistical significance of apparent
c       pattersn in the real points.  See the file NDA.DOC for instructions.
c       
c       Modules: NDA, NDMODEL, GRAPHDEN, ANGDIST, NDRANDOM, NDSUBS,
c       NDMTKRAND, NDMTKSUBS, CONVEXBOUND
c       
c       David Mastronarde  7/31/90
c       
c       $Id$
c       
      call plax_initialize('nda')
      call exit(0)
      end

      subroutine realgraphicsmain()
      parameter (limgraphs=50,limbins=1001,limpnts=50000,
     &    limvert=5000,limregion=200,itypall=999)
      parameter (limtyp=50,limrand=1000)
      parameter (nOptNeedModel=12)
      real*4 bx(limvert),by(limvert)            !boundary vertices
      real*4 sx(limpnts),sy(limpnts)            !sample points
      real*4 graphs(limbins,limgraphs),areas(limbins,limgraphs)
      integer*4 itype(limpnts)                  !types of sample points
      integer*4 nreftyp(limgraphs),nneightyp(limgraphs) !# of types
      integer*4 itypref(limtyp,limgraphs),itypneigh(limtyp,limgraphs)
      integer*4 nangtyp(limgraphs),itypang(limtyp,limgraphs)
      integer*4 itypshft(limtyp),itypbound(limtyp)
c       
      integer*4 ninclass(limtyp,limregion),nintmp(limregion)
      integer*4 iobjregion(limregion),npntsregion(limregion)
      integer*4 ifsclregion(limregion),ntypbndregion(limregion)
      integer*4 itypbndregion(limtyp,limregion)
      integer*4 itypcrosind(-256:256),ifconvregion(limregion)
      real*4 zzregion(limregion),arearegion(limregion)
      real*4 xysclregion(limregion)
      real*4 padbndregion(limregion),fracomregion(limregion)
c       
      character*120 modelfile,modelregion(limregion)
      logical inside,forceload,grapheach,shuffled
      logical sampled,converted,checkgrf
c       
      real*4 xmaxdsp(4),ymaxdsp(4),chngfrac(limtyp)
      real*4 probnear(limbins),probsave(limbins),probnearin(limbins)
      integer*4 igrfdsp(100),ityfrom(limtyp),ityto(limtyp)
      integer*4 listextra(limgraphs)
      character*9 distangtxt
c       
      integer*4 ibasestrt(limgraphs),ibasend(limgraphs),
     &    integstrt(limgraphs),integend(limgraphs),nrandabove(limrand)
      real*4 baseline(limgraphs),realinteg(limgraphs),
     &    suminteg(limrand),sumsqinteg(limrand)
      integer*4 nbingrf(limgraphs),ifadgrf(limgraphs)
      real*4 delrgrf(limgraphs),rmingrf(limgraphs),rmaxgrf(limgraphs)
      common /grfarr/nrow,ncol,irow,icol,ifbycol,nxtick, nytick,
     &    xgutter,ygutter,ymaxfix
c       
      integer*4 ioptNeedModel(nOptNeedModel)
     &    /14,15,16,17,19,26,20,21,22,27,23,40/
      logical OptionNeedsModel
      character*40 objname
      integer*4 getimodobjname
      integer*4 in5
      common /nmsinput/ in5
c       
      in5 = 5
      ifanyplot=0
      nextragrf=0
      ntypbound = 0
      ibavst = 1
      ibavnd = 1
      ibinst = 1
      ibinnd = 1
      baseval = 0
      nbinsave = 0
      iwin = 1
      zz = -1.
      ifspec = 0

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
     &    '0 for graphs in a Plax window, 1 to suppress graphs: '
      read(in5,*)iffil
      call scrnOpen(iffil)
c       
      modelfile=' '
      call read_model(modelfile,ifscale,xyscal)
      if (modelfile .eq. ' ') go to 38
c       
      lastangdiff=-9999
8     write(*,'(1x,a,/,a,$)')'Enter 0 for graphs of density versus'//
     &    ' radial distance','    or 1 for graphs of density versus'//
     &    ' angular difference within an annulus: '
      read(in5,*)ifangdiff
c       
c       initialize for first region
c       
10    nregion=1
      ntypes=0
      do i=-256,256
        itypcrosind(i)=0
      enddo
c       
c       get boundary of region
c       
12    if (modelfile .eq. ' ') then
        iobjboundin = -1
      else
        write(*,'(1x,a,/,a,/,a,$)') 'To specify region boundary, enter'
     &      //' IMOD object and contour number,',' or WIMP object '
     &      //'number AND 0, or 0,0 to take all points on one section,'
     &      ,'   ( or -1,0 for new model): ' 
        read(in5,*)iobjboundin,icontbound
      endif
      if(iobjboundin.lt.0)then
        modelfile=' '
        call read_model(modelfile,ifscale,xyscal)
        if (modelfile .eq. ' ') go to 40
        go to 12
      endif
      if(iobjboundin.eq.0)then
        zz=zz+1.
        write(*,'(1x,a,/,a,/,a,f7.1,i3,a,$)')'Enter section # (from 0); and 0 '
     &      //'for unpadded rectangle around all points,','     1 to'
     &      //' select special options (subsets of points, padding,'
     &      //' convex polygon),',' or -1 to use previously selected'
     &      //' special options (/ for',zz,ifspec,'): '
        read(in5,*)zz,ifspec
        if(ifspec.gt.0)then
          print *,'Enter list of types to consider in finding',
     &        ' boundary (Return for all types)'
          call rdlist(in5,itypbound,ntypbound)
          write(*,'(1x,a,$)')'Distance to pad boundary beyond '//
     &        'points (- if in pixels to be scaled): '
          read(in5,*)padboundin
          padbound=padboundin
          write(*,'(1x,a,$)')
     &        '0 for rectangle or 1 to find smallest convex polygon: '
          read(in5,*)ifconvex
          if(ifconvex.ne.0)then
            write(*,'(1x,a,$)')'Fraction of far out points '//
     &          'to omit from consideration: '
            read(in5,*)fracomit
          endif
        elseif(ifspec.lt.0)then
          padbound=padboundin
        else      
          ifconvex=0
          padbound=0.
        endif
        if(padbound.lt.0.)then
          padbound=-padbound
          if(ifscale.ne.0)padbound=xyscal*padbound
        endif
        if(ifspec.eq.0.or.ntypbound.eq.0)then
          ntypbound=1
          itypbound(1)=itypall
        endif
        iobjbound=0
      else
        iobjbound=iobjfromcont(iobjboundin,icontbound)
        if(iobjbound.eq.0)go to 12
      endif
      call get_boundary_obj(iobjbound,bx,by,nvert,zz,itypbound,
     &    ntypbound,padbound,ifconvex,fracomit,sx,sy,limvert)
      if(nvert.eq.0)go to 12
c       
      iobjregion(nregion)=iobjbound
      zzregion(nregion)=zz
      modelregion(nregion)=modelfile
      ifsclregion(nregion)=ifscale
      xysclregion(nregion)=xyscal
      ntypbndregion(nregion)=ntypbound
      do i=1,ntypbound
        itypbndregion(i,nregion)=itypbound(i)
      enddo
      padbndregion(nregion)=padbound
      ifconvregion(nregion)=ifconvex
      fracomregion(nregion)=fracomit
c       
c       get points in region
c       
      call get_points(bx,by,nvert,zz,itypcrosind,ntypes,sx,sy,itype,
     &    npnts,ninclass(1,nregion))
c       
c       compute area and extent
c       
      area=0.
      xmin=bx(1)
      ymin=by(1)
      xmax=bx(1)
      ymax=by(1)
      do iv=1,nvert
        area=area+0.5*(by(iv+1)+by(iv))*(bx(iv+1)-bx(iv))
        xmin=min(xmin,bx(iv))
        ymin=min(ymin,by(iv))
        xmax=max(xmax,bx(iv))
        ymax=max(ymax,by(iv))
      enddo
      area=abs(area)
      arearegion(nregion)=area
      npntsregion(nregion)=npnts
      density=npnts/area
      range=max(xmax-xmin,ymax-ymin)
      write(*,101)npnts,area,density,range
101   format(i5,' points, area =',f13.6,',  density =',f13.6,/,
     &    ' Maximum extent of region is',f12.5)
c       
      write(*,*)'Type   number    density'
      do ity=-256,256
        i=itypcrosind(ity)
        if(i.gt.0)then
          objname=' '
          ierr = getimodobjname(abs(ity), objname)
          dens=ninclass(i,nregion)/area
          write(*,'(i5,i8,f13.6,4x,a)')ity,ninclass(i,nregion),dens,objname
        endif
      enddo
      write(*,*)
c       
      if(nregion.gt.1)go to 35
c       
      call getbinspec(ifangdiff,delr,nbins,rmin,rmax)
c       
      call getgraphspec(ifangdiff,lastangdiff,ngraph,itypref,nreftyp,
     &    itypneigh, nneightyp,itypang,nangtyp)
      maxgraph=ngraph
c       
c       
35    if(ifangdiff.eq.0)then
        call dengraph(bx,by,nvert,sx,sy,itype,npnts,delr,nbins,
     &      ngraph,nreftyp,nneightyp,itypref,itypneigh,graphs,areas)
      else
        call angdist(bx,by,nvert,sx,sy,itype,npnts,rmin,rmax,nbins,
     &      ngraph,nreftyp,nneightyp,nangtyp,itypref,itypneigh,itypang
     &      ,graphs,areas)
      endif
      do ii=1,maxgraph
        nbingrf(ii)=nbins
        delrgrf(ii)=delr
        ifadgrf(ii)=ifangdiff
        rmingrf(ii)=rmin
        rmaxgrf(ii)=rmax
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
     &    ' PostScript graphics file',/, ' 10/11: Output PostScript',
     &    ' graphics file to screen window/printer'
     &    ,/,' 12: Output single or average graph to file',/,
     &    ' 13: Loop back to specify model contour defining new area',
     &    ' to analyze',/, ' 14: Loop back to specify radial or ',
     &    'angular graph and new boundary contour ',/,
     &    ' 15: Analyze new region and average with ',
     &    'previous region(s)',/, ' 16: Redo current region(s) with',
     &    ' new bin size, # of bins, or types for graphs',/,
     &    ' 17: Redo current region(s) with angular instead of ',
     &    'radial graph or vice versa',/, ' 18: Save bins of a graph'
     &    ,' to specify rejection probabilities for random points',/,
     &    ' 19/26/20: Do current region(s) with shuffled/converted',
     &    ' types or random points',/,' 21: Save current set of ',
     &    'points and their types as an IMOD model',/,
     &    ' 22/27/23: Do many sets with shuffled/converted',
     &    ' types/random pnts for integrals',
     &    /,' 24: Take command input from file        25: Exit',
     &    /,' 28/29/30 Save a graph/Average/Combine 2 graphs into',
     &    ' an extra graph location',/,' 31/32: Save graph in',
     &    ' file/Read from file into an extra graph location',/,
     &    ' 33: Replace some sets of bins by their averages',/,
     &    ' 34/35: Set up special big array for plots/Plot all ',
     &    'windows in array',/,' 37/38/39 Add list of graphs/Read',
     &    ' list of graphs from file/Read&Add from file',/,
     &    ' 42: Export graph values or points for drawing to file')
c       
40    write(*,'(1x,a,$)')'Option, or -1 for list of choices: '
      read(in5,*,err=40)iopt
      if(iopt.eq.-1)go to 38
      if (OptionNeedsModel(modelfile, ioptNeedModel,
     &    nOptNeedModel, iopt)) go to 40
      go to(201,202,203,204,205,206,207,208,209,210,
     &    210,212,213,213,215,216,217,218,219,220,
     &    221,222,222,224,225,226,222,228,228,228,
     &    228,228,228,234,235,40,228,228,228,40,40,228)
     &    ,iopt
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
      call integrate(graphs(1,jgrf),ifadgrf(jgrf),delrgrf(jgrf),
     &    rmingrf(jgrf),rmaxgrf(jgrf), nbingrf(jgrf),
     &    ibinst,ibinnd,0,0, baseval,sum, centroid)
      distangtxt='distances'
      if(ifangdiff.ne.0)distangtxt='angles'
      write(*,110)ibinnd+1-ibinst,distangtxt,(ibinst-1)*delrgrf(jgrf),
     &    ibinnd*delrgrf(jgrf),sum,distangtxt,centroid
110   format(' For the',i4,' bins covering ',a,' from',
     &    f10.3,' to',f10.3,/,5x,
     &    'the integrated number of (excess/missing) items is',f10.5,/,5x,
     &    'the centroid of their ',a,' is', f11.4)
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
c       Set up big array of graphs
c       
234   call setgrapharray
      go to 40
c       
c       Plot all windows to big graph array
c       
235   if(irow.le.0.or.icol.le.0)go to 40
      do iwn=1,4
        jgrf=igrfdsp(iwn)
        if(jgrf.gt.0)then
          call psSetup(1,c1,c2,c3,0)
          if(ifanyplot.ne.0.and.irow.eq.1.and.icol.eq.1)call psFrame
          if(ymaxfix.gt.0.and.ymaxfix.ne.ymaxdsp(iwn))then
            ymaxdsp(iwn)=ymaxfix
            call graphdsp(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),
     &          iwn,jgrf, xmaxdsp(iwn),ymaxdsp(iwn))
          endif
          write(*,2351)jgrf,irow,icol
2351      format(' Plotting graph #',i3,' to row',i3,', column',i3)
          call graphplt(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),
     &        -1,jgrf, xmaxdsp(iwn),ymaxdsp(iwn))
          ifanyplot=1
        endif
      enddo
      go to 40
c       
c       Output plot file to workstation or printer
c       
210   call pltout(11-iopt)
      ifanyplot=0
      if(irow.gt.0.and.icol.gt.0)then
        irow=1
        icol=1
      endif
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
      write(iout,112)' Boundary object #:',
     &    (iobjregion(i),i=iregst,nregion)
112   format(a,(t20,6i10))
      write(iout,113)' Z values',(zzregion(i),i=iregst,nregion)
113   format(a,(t20,6f10.3))
      write(iout,113)' Area:',(arearegion(i),i=iregst,nregion)
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
      if(ifangdiff.gt.0)then
        nintot=0
        do ir=iregst,nregion
          nintmp(ir)=0
          do ity=1,nangtyp(jgrbas)
            if(itypang(ity,jgrbas).eq.itypall)then
              nintmp(ir)=npntsregion(ir)
              go to 366
            endif
            itcr=itypcrosind(itypang(ity,jgrbas))
            if(itcr.ne.0)nintmp(ir)=nintmp(ir)+ninclass(itcr,ir)
          enddo
366       nintot=nintot+nintmp(ir)
        enddo
        write(iout,112)' Annular nbr items:'
     &      ,(nintmp(i),i=iregst,nregion)
        if(iregst.ne.nregion)write(iout,112)'         Total:',nintot
      endif
      write(iout,114)' Reference types:',
     &    (itypref(ity,jgrbas),ity=1,nreftyp(jgrbas))
114   format(a,(12i5))
      write(iout,114)' Neighboring types:',
     &    (itypneigh(ity,jgrbas),ity=1,nneightyp(jgrbas))
      if(ifangdiff.gt.0)then
        write(iout,114)' Annular nbr types:',
     &      (itypang(ity,jgrbas),ity=1,nangtyp(jgrbas))
        write(iout,115)rmingrf(jgrf),rmaxgrf(jgrf),delrgrf(jgrf)
115     format(' Rmin =',f10.3,', rmax =',f10.3,
     &      ', delta theta = ',f6.1)
      else
        write(iout,'(a,f12.5)')' Delta r =',delrgrf(jgrf)
      endif
      write(iout,'(5g15.8)')(graphs(i,jgrf),i=1,nbingrf(jgrf))
      go to 40
c       
c       Specify new boundary object: but check first if averaging
c       
213   if(nregion.gt.1)then
        write(*,'(1x,a,$)')'This will destroy the stored average.'//
     &      '  Enter 1 to do so: '
        read(in5,*)ifreally
        if(ifreally.ne.1)go to 40
      endif
      if(iopt.eq.13)go to 10
      go to 8
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
        ireturn=3
        go to 1000
      endif
1003  continue
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
217   if(ifangdiff.eq.0)then
        ifangdiff=1
      else
        ifangdiff=0
      endif
c       
c       Redo region(s) with new bins and/or graphs
c       
216   call getbinspec(ifangdiff,delr,nbins,rmin,rmax)
c       
      call getgraphspec(ifangdiff,lastangdiff,ngraph,itypref,nreftyp,
     &    itypneigh, nneightyp,itypang,nangtyp)
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
      ireturn=1
      go to 1000
1001  if(nregion.gt.1)call fourdsp(graphs,limbins,ngraph+1,ngraph
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
      print *,nbinsave,' probability bins saved'
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
        write(*,'(1x,a,$)')'Number of types to convert: '
        read(in5,*)nchange
        print *,'For each conversion, enter the type to change',
     &      ' from, the type to change to,',
     &      ' and the fraction of points of that type to convert.'
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
        ireturn=4
        go to 1000
      endif
1004  continue
      if(iopt.eq.22)go to 219
      if(iopt.eq.27)go to 226
c       
c       make up control points
c       
220   write(*,'(1x,a,$)')'Minimum distance from boundary (- if in'//
     &    ' pixels to be scaled): '
      read(in5,*)boundsep
      if(boundsep.lt.0.)then
        boundsep=-boundsep
        if(ifscale.ne.0)boundsep=xyscal*boundsep
      endif
c       
      print *,'Enter list of types of points to shift ',
     &    '(Return for all types)'
      call rdlist(in5,itypshft,ntypshft)
      if(ntypshft.eq.0)then
        ntypshft=1
        itypshft(1)=itypall
      endif
c       
      ifusesave=0
      if(nbinsave.gt.0)then
        write(*,'(1x,a,$)')
     &      '1 to use saved probability bins, 0 not to: '
        read(in5,*)ifusesave
      endif
c       
      if(ifusesave.ne.0)then
        nrestrict=nbinsave
        delnear=delsave
        do ii=1,nbinsave
          probnearin(ii)=probsave(ii)
        enddo
      else
c         
        write(*,'(1x,a,$)')'# of bins for rejection, bin size: '
        read(in5,*)nrestrict,delnear
        if(nrestrict.gt.0)then
          write(*,'(1x,a,$)')'Probability values: '
          read(in5,*)(probnearin(i),i=1,nrestrict)
        endif
      endif
c       
      if(nrestrict.gt.0)then
c         write(*,'(1x,a,$)')'Power to raise the probability values to'
c         &           //' for use (try about 3): '
c         read(in5,*)power
c         if(power.eq.0.)power=1.
        power=1.
        do ii=1,nrestrict
          probnear(ii)=probnearin(ii)**power
        enddo
      endif
c       
      ifshuffle=0
      ifsample=1
      ifconvert=0
      forceload=.false.
      if(iopt.eq.20)go to 315
c       
c       general setup to do series of control runs and gather statistics
c       
319   jrgfadd=0
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
     &        ' from, or 0,0 to used fixed value: '
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
        call integrate(graphs(1,jj+jgrfadd),ifangdiff,delr,rmin,rmax,
     &      nbins, integstrt(jj),integend(jj),ibasestrt(jj),
     &      ibasend(jj), baseline(jj),realinteg(jj), centroid)
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
      grapheach=.false.
      ireturn=2
      go to 1000
1002  continue
c       
c       Get integrals from graphs and accumulate statistics
c       
      do jj=1,ngraph
        call integrate(graphs(1,jj+jgrfadd),ifangdiff,delr,rmin,
     &      rmax, nbins, integstrt(jj),integend(jj),ibasestrt(jj),
     &      ibasend(jj), baseline(jj),randinteg, centroid)
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
c       save current points and boundary as model file
c       
221   call save_model(bx,by,nvert,sx,sy,itype,npnts,zz,ifscale,xyscal)
      call read_model(modelfile,ifscale,xyscal)
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
228   call manipgraphs(iopt,'nda',graphs,areas,nbingrf,delrgrf,
     &    ifadgrf,rmingrf,rmaxgrf,maxgraph,nextragrf,listextra,
     &    igrfdsp,xmaxdsp,ymaxdsp)
      go to 40
c       
c       
c       general code to redo regions, optionally shuffling points or randomly
c       sampling them.  It manages the logicals SHUFFLED and SAMPLED to
c       reflect the actual state of points in memory.
c       This is set up to be used like a subroutine; it's not a subroutine
c       because there would have been so many arguments.  It is expecting:
c       IRETURN = value to use directly in computed GOTO at end to return
c       IFSAMPLE = 1 to randomly sample points
c       IFSHUFFLE =1 to shuffle types
c       IFCONVERT = 1 to convert fraction of some types to other types
c       FORCELOAD true to force model to get reloaded even if only one region
c       GRAPHEACH true to display graphs after each region
c       
1000  do ireg=1,nregion
        if(nregion.gt.1.or.forceload)then
          if(modelfile.ne.modelregion(ireg))then
            call read_model(modelregion(ireg),ifsclregion(ireg),
     &          xysclregion(ireg))
            modelfile=modelregion(ireg)
            ifscale=ifsclregion(ireg)
            xyscal=xysclregion(ireg)
          endif
          zz=zzregion(ireg)
          call get_boundary_obj(iobjregion(ireg),bx,by,nvert,zz,
     &        itypbndregion(1,ireg), ntypbndregion(ireg),
     &        padbndregion(ireg),ifconvregion(ireg),
     &        fracomregion(ireg),sx,sy,limvert)
          call get_points(bx,by,nvert,zz,itypcrosind,ntypes,sx,sy,
     &        itype, npnts,ninclass(1,ireg))
          shuffled=.false.
          sampled=.false.
          converted=.false.
        endif
        if(ifshuffle.ne.0)then
          call shuffle(itype,npnts)
          shuffled=.true.
        endif
        if(ifsample.ne.0)then
          call random_points(bx,by,nvert,probnear,delnear,nrestrict,
     &        boundsep,npnts,itype,itypshft,ntypshft, sx,sy)
          sampled=.true.
        endif
        if(ifconvert.ne.0)then
          call change_type(itype,npnts,ityfrom,ityto,chngfrac,nchange)
          converted=.true.
        endif
c         
        if(ifangdiff.eq.0)then
          call dengraph(bx,by,nvert,sx,sy,itype,npnts,delr,nbins,
     &        ngraph,nreftyp,nneightyp,itypref,itypneigh,graphs,areas)
        else
          call angdist(bx,by,nvert,sx,sy,itype,npnts,rmin,rmax,nbins,
     &        ngraph,nreftyp,nneightyp,nangtyp,itypref,itypneigh,
     &        itypang ,graphs,areas)
        endif
        do ii=1,maxgraph
          nbingrf(ii)=nbins
          delrgrf(ii)=delr
          ifadgrf(ii)=ifangdiff
          rmingrf(ii)=rmin
          rmaxgrf(ii)=rmax
        enddo
c         
        if(nregion.gt.1)
     &      call addtoavg(graphs,areas,limbins,ngraph,nbins,ireg)
        if(grapheach)call fourdsp(graphs,limbins,1,min(4,ngraph),
     &      nbingrf,delrgrf, xmaxdsp, ymaxdsp,igrfdsp)
      enddo
      go to (1001,1002,1003,1004),ireturn
c       
      end

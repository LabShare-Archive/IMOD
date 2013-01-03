c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       

c       KNOWN BUG: It will not work on data with a zscale value modeled on
c       a data stack flipped inside IMOD.  Make a flipped volume BEFORE
c       modeling.
c       
      call plax_initialize('sda')
      call exit(0)
      end

      subroutine realgraphicsmain()
      include 'sda.inc'
      parameter (limgraphs=50,limbins=1001,limregion=200,itypall=999)
      parameter (limtyp=50,limrand=1000)
      real*4 graphs(limbins,limgraphs),areas(limbins,limgraphs)
      integer*4 itytmp(maxpore)
      integer*4 nreftyp(limgraphs),nneightyp(limgraphs) !# of types
      integer*4 itypref(limtyp,limgraphs),itypneigh(limtyp,limgraphs)
      integer*4 nangtyp(limgraphs),itypang(limtyp,limgraphs)
      integer*4 itypshft(limtyp,limregion),indlim(6),itypkern(limtyp)
      logical*1 poreinreg(maxpore),vertinreg(maxverts)
      real*4 xyzlimin(6),xyzlim(6),xyzregion(6,limregion)
      real*4 clipplanes(4,100),clipuse(4,100)
      integer*4 iclip(100),nclipregion(limregion)
      integer*4 iclipregion(100,limregion),ntypshft(limregion)
      real*4 porout(3,maxpore)
c       
      integer*4 ninclass(limtyp,limregion),nintmp(limregion)
      integer*4 iobjregion(limregion),npntsregion(limregion)
      integer*4 itypcrosind(256)
      real*4 arearegion(limregion)
c       
      character*50 modelfile,modelregion(limregion),modelout
      logical inside,forceload,grapheach,shuffled
      logical sampled,converted,checkgrf
      integer putimodscat
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
      integer*4 in5
      common /nmsinput/ in5
c       
      in5 = 5
      ndredo=1
      ndfirst=5
      ifanyplot=0
      nextragrf=0
      manyrandom=0
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
     &    '0 for graphs on plax, 1 to suppress graphs: '
      read(in5,*)iffil
      call scrnOpen(iffil)
c       
c       write(*,'(1x,a,$)')
c       &           'Minimum distance between pore and surface in um: '
c       read(in5,*)dropcrit
      modelfile='SDAFIRSTTIME'
      print *,'Just Return to skip reading a model . . .'
      call read_model(modelfile,iobjsurf,xyscal,zscal,clipplanes,
     &    nmodclip)
      shuffled=.false.
      sampled=.false.
      converted=.false.
c       
      do i=1,6
        xyzlimin(i)=0.
      enddo
8     ifangdiff=0
      lastangdiff=-9999
      nregion=0
      if(modelfile.eq.' ')go to 37
c       8       write(*,'(1x,a,/,a,$)')'Enter 0 for graphs of density versus'//
c       &           ' radial distance','    or 1 for graphs of density versus'//
c       &           ' angular difference within an annulus: '
c       read(in5,*)ifangdiff
c       
c       initialize for first region
c       
10    nregion=1
      ntypes=0
      do i=1,256
        itypcrosind(i)=0
      enddo
c       
c       get surface and pores on it
c       
12    print *,'Enter list of clipping planes to apply, Return for '//
     &    'none,','     / for same as last time, or 0 for new model'
      call rdlist(in5,iclip,nclip)
      if(nclip.gt.0.and.iclip(1).eq.0)then
        modelfile=' '
        call read_model(modelfile,iobjsurf,xyscal,zscal, clipplanes,
     &      nmodclip)
        shuffled=.false.
        sampled=.false.
        converted=.false.
        go to 12
      elseif(modelfile.eq.' ')then
        nregion=0
        go to 40
      endif
c       
      write(*,'(1x,a,/,a,6f7.1,a)')'Enter lower & upper X, then Y'
     &    //', then Z of region to analyze (in image index',
     &    'coords, 0,0 for no limits, / for',
     &    (xyzlimin(i),i=1,6),')'
      read(in5,*)(xyzlimin(i),i=1,6)
      do i=1,6
        xyzlim(i)=xyscal*xyzlimin(i)
        if(i.gt.4)xyzlim(i)=zscal*xyzlim(i)
        xyzregion(i,nregion)=xyzlim(i)
      enddo
      call clipcopy(clipplanes,nmodclip,iclip,nclip,clipuse,nuseclip,
     &    xyzlim,indlim,nlim)
c       
      nclipregion(nregion)=nclip
      do i=1,nclip
        iclipregion(i,nregion)=iclip(i)
      enddo
      iobjregion(nregion)=iobjsurf
      modelregion(nregion)=modelfile
c       
c       find points in region and area
c       
c       write(*,'(1x,a,$)')'Number of subdivisions for triangles: '
c       read(in5,*)ndiv
      call find_points_area(itypcrosind,ntypes,clipuse,nuseclip,
     &    xyzlim,indlim,nlim,ndfirst,poreinreg,vertinreg,npnts,
     &    ninclass(1,nregion),areainreg)
      npntsregion(nregion)=npnts
      density=npnts/areainreg
      write(*,101)npnts,areainreg,density
101   format(i5,' points, area =',f13.6,',  density =',f13.6)
c       
      write(*,*)'Object  number    density'
      do ity=1,256
        i=itypcrosind(ity)
        if(i.gt.0)then
          dens=ninclass(i,nregion)/areainreg
          write(*,'(i5,i8,f13.6)')ity,ninclass(i,nregion),dens
        endif
      enddo
      write(*,*)
c       
      if(nregion.eq.1)then
c         
        call getbinspec(ifangdiff,delr,nbins,rmin,rmax)
c         
        call getgraphspec(ifangdiff,lastangdiff,ngraph,itypref,nreftyp,
     &      itypneigh, nneightyp,itypang,nangtyp)
        maxgraph=ngraph
        if(ngraph.eq.0)go to 37
      else
        call getregtypes(ngraph,itypref,nreftyp,itypneigh,nneightyp,
     &      nregion)
      endif
c       
c       
35    if(ifangdiff.eq.0)then
        ndiv = min(19,max(1,nint(0.5+2.*trisize/delr)))
        write(*,1035)ndiv
1035    format('Triangles will be subdivided',i3,' times')
        nregbas=(nregion-1)*ngraph+1
        call surfden(vertinreg,poreinreg,ndiv,delr,nbins,
     &      ngraph,nreftyp(nregbas),nneightyp(nregbas),
     &      itypref(1,nregbas),itypneigh(1,nregbas),graphs,areas,
     &      manyrandom)
      else
c         call angdist(bx,by,nvert,sx,sy,itype,npnts,rmin,rmax,nbins,
c         &           ngraph,nreftyp,nneightyp,nangtyp,itypref,itypneigh,itypang
c         &           ,graphs,areas)
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
37    if(iopt.ne.-1)go to 40
38    write(*,104)
104   format(' 1/2: Type/Average selected bins of the graph in a',
     &    ' specified window',/,' 3: Compute integrated number',
     &    ' of (excess/missing) items in selected bins',/,
     &    ' 4/5: Display one graph in a window/Enter list of graphs',
     &    ' to display',/,
     &    ' 6/7: Rescale X or Y axis of one window/Y axis of all',
     &    ' windows',/, ' 8/9: Plot one window/all windows to',
     &    ' PostScript graphics file',/,
     &    ' 10/11: Output PostScript graphics file to screen/printer'
     &    ,/,' 12: Output single or average graph to file',/,
     &    ' 13: Loop back to specify limits of region',
     &    ' to analyze',/, ' 14: Loop back to specify radial or ',
     &    'angular graph and new region ',/,
     &    ' 15: Analyze new region and average with ',
     &    'previous region(s)',/, ' 16: Redo current region(s) with',
     &    ' new bin size, # of bins, or types for graphs',/,
     &    ' 17: Redo current region(s) with angular instead of ',
     &    'radial graph or vice versa',/, ' 18: Save bins of a graph'
     &    ,' to specify rejection probabilities for random pores',/,
     &    ' 19/26/20: Do current region(s) with shuffled/converted',
     &    ' types or random pores',/,' 21/36: Compute kernel density '
     &    ,'and save in new model file/do many random sets',/,
     &    ' 22/27/23: Do many sets & integrals with shuffled/',
     &    'converted types/random pores',
     &    /,' 24: Take command input from file        25: Exit',
     &    /,' 28/29/30: Save a graph/Average/Combine 2 graphs into',
     &    ' an extra graph location',/,' 31/32: Save graph in',
     &    ' file/Read from file into an extra graph location',/,
     &    ' 33: Replace some sets of bins by their averages',/,
     &    ' 34/35: Set up special big array for plots/Plot all ',
     &    'windows in array',/,' 37/38/39: Add list of graphs/Read',
     &    ' list of graphs from file/Read&Add from file',/,
     &    ' 40: Save model with pores on surface     41: Compute ',
     &    'maximum extent in 3-D',/,
     &    ' 42: Export graph values or points for drawing to file')
c       
40    write(*,'(1x,a,$)')'Option, or -1 for list of choices: '
      read(in5,*,err=40)iopt
      if(iopt.eq.-1)go to 38
      if(nregion.eq.0.and.((iopt.ge.14.and.iopt.lt.22).or.
     &    iopt.eq.26.or.iopt.eq.27))go to 40
      go to(201,202,203,204,205,206,207,208,209,210,
     &    210,212,213,213,215,216,217,218,219,220,
     &    221,222,222,224,225,226,222,228,228,228,
     &    228,228,228,234,235,222,228,228,228,240,
     &    241,228)
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
107   format(12x,'1',12x,'2',12x,'3',12x,'4',12x,'5')
      do ilin=linst,linnd
        ib1=5*(ilin-1)
        ib2=min(ibtynd,ib1+5)
        if(ifraw.gt.0)then
          write(*,108)ib1,(graphs(ib,jgrf),ib=ib1+1,ib2)
108       format(i4,'/',5f13.6)
        else
          write(*,1108)ib1,(nint(graphs(ib,jgrf)*areas(ib,jgrf))
     &        ,ib=ib1+1,ib2)
1108      format(i4,'/',i7,4i13)
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
      call sdintegrate(graphs(1,jgrf),areas(1,jgrf),ifadgrf(jgrf),
     &    delrgrf(jgrf),
     &    rmingrf(jgrf),rmaxgrf(jgrf), nbingrf(jgrf),
     &    ibinst,ibinnd,0,0, baseval,sum)
      distangtxt='distances'
      if(ifangdiff.ne.0)distangtxt='angles'
      write(*,110)ibinnd+1-ibinst,distangtxt,(ibinst-1)*delrgrf(jgrf),
     &    ibinnd*delrgrf(jgrf),sum
110   format(' For the',i4,' bins covering ',a,' from',
     &    f10.3,' to',f10.3,/,5x,
     &    'the integrated number of (excess/missing) items is',f10.5)
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
      write(iout,112)' Surface object #:',
     &    (iobjregion(i),i=iregst,nregion)
112   format(a,(t20,6i10))
      write(iout,113)' Area:',(arearegion(i),i=iregst,nregion)
113   format(a,(t20,6f10.3))
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
     &      ', delta theta =,'f6.1)
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
217   continue
c       if(ifangdiff.eq.0)then
c       ifangdiff=1
c       else
c       ifangdiff=0
c       endif
c       
c       Redo region(s) with new bins and/or graphs
c       
216   call getbinspec(ifangdiff,delr,nbins,rmin,rmax)
c       
      if(nregion.gt.1)print *,'Specify graphs and types for region 1:'
      call getgraphspec(ifangdiff,lastangdiff,ngraph,itypref,nreftyp,
     &    itypneigh, nneightyp,itypang,nangtyp)
      do ireg=2,nregion
        print *,'Specify types for region',ireg,':'
        call getregtypes(ngraph,itypref,nreftyp,itypneigh,nneightyp,
     &      ireg)
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
      ireturn=1
      go to 1000
1001  if(nregion.gt.1)call fourdsp(graphs,limbins,ngraph+1,ngraph
     &    +min(4,ngraph),nbingrf,delrgrf,xmaxdsp,ymaxdsp,igrfdsp)
c       
c       for randomized points, save positions in model, ask if want to
c       store model now.
c       
      if(iopt.eq.20)then
        call putpores(itypore,pore,porout,npore,xyscal,zscal)
      endif
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
      if(iopt.eq.36)then
        write(*,'(1x,a,$)')'Range value to use '//
     &      '(1=range, 2,3,4=95,90,80%ile, 5 = SD): '
        read(in5,*)kernret
        go to 2212
      endif
c       
c       make up control points
c       
220   do ireg=1,nregion
        if(nregion.gt.1)then
          write(*,'(1x,a,i3,a)')'Enter list of types to randomize'//
     &        ' for region #',ireg,' (Return for all types)'
        else
          print *,'Enter list of types of pores to randomize ',
     &        '(Return for all types)'
        endif
        call rdlist(in5,itypshft(1,ireg),ntypshft(ireg))
        if(ntypshft(ireg).eq.0)then
          ntypshft(ireg)=1
          itypshft(1,ireg)=itypall
        endif
      enddo
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
      if(iopt.eq.36)then
        realrange=rangekern
        ntotcontrol=0
        rankernsum=0.
        rankernsq=0.
        kernabove=0
        go to 324
      endif
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
        call sdintegrate(graphs(1,jj+jgrfadd),areas(1,jj+jgrfadd),
     &      ifangdiff,delr,rmin,rmax,
     &      nbins, integstrt(jj),integend(jj),ibasestrt(jj),
     &      ibasend(jj), baseline(jj),realinteg(jj))
        write(*,116)jj+jgrfadd,realinteg(jj)
116     format(' Graph #',i3,', real integral =',f10.5)
      enddo
c       
      maxtmp=jgrfadd+3*ngraph
      if(maxtmp.le.limgraphs)then
        write(*,'(1x,a,i3,a,i3,/,a,$)')'Mean and standard deviation'
     &      //' graphs would be stored in graphs',
     &      jgrfadd+ngraph+1,' to',maxtmp,
     &      ' Enter 1 to accumulate these or 0 not to: '
        read(in5,*)ifdomeansd
      else
        ifdomeansd=0
        print *,'Too many graphs to accumulate mean and SD graphs'
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
      if(iopt.eq.36)then
        call random_pores(probnear,delnear,nrestrict,
     &      itypshft,ntypshft(1),1)
        sampled=.true.
        call kerneldens(h,poreinreg,vertinreg,itypkern,ntypkern,
     &      kernret, rangekern)
        rankernsum=rankernsum+rangekern
        rankernsq=rankernsq+rangekern**2
        if(rangekern.gt.realrange)kernabove=kernabove+1
        icont=icont+1
        if(icont.le.ndocontrol)go to 424
c         
        do iun=6,iout
          call sums_to_avgsd(rankernsum,rankernsq,ntotcontrol,
     &        avgint,sdint)
          pctabove=100.*kernabove/ntotcontrol
          coefvar=(realrange-avgint)/sdint
          write(iun,3618)realrange,avgint,sdint,ntotcontrol,
     &        kernabove, pctabove,coefvar
3618      format(' Real range =',f9.4,', random mean =',f9.4,', SD ='
     &        ,f8.4,', n =',i4,/,i5,' random > real (',f5.1,
     &        '%), real is',f8.3,' SDs above random mean')
        enddo
        go to 324
      endif
      write(*,'(a,i4,$)')char(13)//'Working on set',ntotcontrol
      call flush(6)
      manyrandom=1
      grapheach=.false.
      ireturn=2
      go to 1000
1002  continue
      manyrandom=0
c       
c       Get integrals from graphs and accumulate statistics
c       
      do jj=1,ngraph
        call sdintegrate(graphs(1,jj+jgrfadd),areas(1,jj+jgrfadd),
     &      ifangdiff,delr,rmin,
     &      rmax, nbins, integstrt(jj),integend(jj),ibasestrt(jj),
     &      ibasend(jj), baseline(jj),randinteg)
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
c       save normals as local densities
c       
221   kernret=0
2212  write(*,'(1x,a,$)')'Mean # of pores to include in kernel area: '
      read(in5,*)desired
      h=sqrt(desired/(3.14159*density))
      write(*,1221)h
1221  format(' Half-width of kernel = ',f7.3)
      print *,'Enter list of types to find density of (Return for',
     &    ' all, / for same as before)'
      call rdlist(in5,itypkern,ntypkern)
c       
      call kerneldens(h,poreinreg,vertinreg,itypkern,ntypkern,kernret,
     &    rangekern)
      if(iopt.ne.36)go to 40
      go to 220
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
228   call manipgraphs(iopt,'sda',graphs,areas,nbingrf,delrgrf,
     &    ifadgrf,rmingrf,rmaxgrf,maxgraph,nextragrf,listextra,
     &    igrfdsp,xmaxdsp,ymaxdsp)
      go to 40
240   call putpores(itypore,pore,porout,npore,xyscal,zscal)
      go to 40
241   xsqmax=0.
      crit=0.
      do ivert=1,nvert-1
        iv=listvert(ivert)
        do jvert=ivert+1,nvert
          jv=listvert(jvert)
          dx=verts(1,iv)-verts(1,jv)
          dy=verts(2,iv)-verts(2,jv)
          dz=verts(3,iv)-verts(3,jv)
          if(abs(dx).ge.crit.or.abs(dy).ge.crit.or.abs(dz).ge.crit)then
            dsq=dx**2+dy**2+dz**2
            if(dsq.gt.xsqmax)then
              xsqmax=dsq
              crit=sqrt(dsq/3.)
            endif
          endif
        enddo
      enddo
      extent=sqrt(xsqmax)
      print *,'maximum extent =',extent
      call sdlength()
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
          if(modelfile.ne.modelregion(ireg).or.
     &        iobjsurf.ne.iobjregion(ireg))then
            call read_model(modelregion(ireg),iobjregion(ireg),xyscal,
     &          zscal,clipplanes,nmodclip)
            modelfile=modelregion(ireg)
            iobjsurf=iobjregion(ireg)
          endif
          call clipcopy(clipplanes,nmodclip,iclipregion(1,ireg),
     &        nclipregion(ireg),clipuse,nuseclip,
     &        xyzregion(1,ireg),indlim,nlim)
          call find_points_area(itypcrosind,ntypes,clipuse,nuseclip,
     &        xyzregion(1,ireg),indlim,nlim,ndredo,poreinreg,
     &        vertinreg,npnts, ninclass(1,ireg),areainreg)
          density=npnts/areainreg
          shuffled=.false.
          sampled=.false.
          converted=.false.
        endif
        if(ifshuffle+ifconvert.ne.0)then
          ntmp=0
          do i=1,npore
            if(poreinreg(i))then
              ntmp=ntmp+1
              itytmp(ntmp)=itype(i)
            endif
          enddo
        endif
        if(ifshuffle.ne.0)then
          call shuffle(itytmp,npnts)
          shuffled=.true.
        endif
        if(ifsample.ne.0)then
          call random_pores(probnear,delnear,nrestrict,
     &        itypshft(1,ireg),ntypshft(ireg),manyrandom)
          sampled=.true.
        endif
        if(ifconvert.ne.0)then
          call change_type(itytmp,npnts,ityfrom,ityto,chngfrac,nchange)
          converted=.true.
        endif
        if(ifshuffle+ifconvert.ne.0)then
          ntmp=0
          do i=1,npore
            if(poreinreg(i))then
              ntmp=ntmp+1
              itype(i)=itytmp(ntmp)
            endif
          enddo
        endif
c         
        if(ifangdiff.eq.0)then
          ndiv=min(19,max(1,nint(0.5+2.*trisize/delr)))
          irbas=(ireg-1)*ngraph+1
          call surfden(vertinreg,poreinreg,ndiv,delr,nbins,
     &        ngraph,nreftyp(irbas),nneightyp(irbas),itypref(1,irbas),
     &        itypneigh(1,irbas),graphs,areas, manyrandom)
        else
c           call angdist(bx,by,nvert,sx,sy,itype,npnts,rmin,rmax,nbins,
c           &           ngraph,nreftyp,nneightyp,nangtyp,itypref,itypneigh,
c           &           itypang ,graphs,areas)
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

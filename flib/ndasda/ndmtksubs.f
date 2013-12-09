c       $Id$
c	
c       
C       GRAPHDSP displays one graph in histogram format in one of 4 windows
c       on the parallax.
c       GRAPH is the array containing numbers to be graphed
c       NBINS is the number of bins (values)
c       DELR is the bin width in user's units
c       IWIN is the desired window # (1 to 4)
c       JGRF is the graph number, which is displayed on the screen
c       XMAX and YMAX are full-scale X and Y values: if they are 0 when the
c       subroutine is called, the routine will compute good maximum values
c       and return them in XMAX and YMAX; if either one is non-zero then that
c       value will be used for the full-scale value of that axis.
c       
      subroutine graphdsp(graph,nbins,delr,iwin,jgrf,xmax,ymax)
      use scrnvars
      real*4 graph(*)
      data ixwinsiz/640/,iywinsiz/510/
      integer*4 ixwin(4),iywin(4)
      data ixwin/0,640,0,640/,iywin/512,512,0,0/
      character*5 dumchar
      character*10 label
      character*8 formt
c       
c       parameters controlling numerical labels
c       
      ntxtchr=6
      itxtsiz=6
      marglft=70
      margbot=10
      margrt=30
      margtop=10
c       
c       find XMAX and YMAX if they are non-zero, get D[XY] = [XY]MAX/10
c       
      if(xmax.le.0.)then
        call scale(0.,0.9999*delr*nbins,dx,xlo)
        xmax=10.*dx
        if(abs(delr*nbins-180.).lt..01)then
          xmax=180.
          dx=18.
        endif
      else
        xlo=0.
        dx=xmax/10.
      endif
c       
      if(ymax.le.0.)then
        dmax=0.
        do i=1,nbins
          dmax=max(dmax,graph(i))
        enddo
        call scale(0.,dmax,dy,ylo)
        ymax=10.*dy
      else
        ylo=0.
        dy=ymax/10.
      endif
c       
      ixlo=ixwin(iwin)
      iylo=iywin(iwin)
      ixran=ixwinsiz-20-marglft-margrt
      iyran=iywinsiz-20-margbot-margtop
      if(ifPlaxOn.gt.0)then
c         
c         output window and graph number in top left (if parallax is on)
c         
        call plax_mapcolor(251,0,255,0)
        call plax_box(0,ixlo,iylo,ixlo+ixwinsiz-1,iylo+iywinsiz-1)
        write(dumchar,'(i1,'':'',i3)')iwin,jgrf
        call plax_sctext(1,12,12,251,ixlo+20+marglft,
     &      iylo+margbot+iyran-35, dumchar)
c         
c         output x then y axis labels
c         
        call makefmt(xmax,ntxtchr,formt)
        do ii=0,10,2
          xval=ii*dx
          write(label,fmt=formt,err=30)xval
30        ix=ixlo+max(0,ii*ixran/10+10+marglft-
     &        ifix(itxtsiz*(ntxtchr-.3)))
          call plax_sctext(1,itxtsiz,itxtsiz,251,ix,iylo,
     &        label(1:ntxtchr))
        enddo
c         
        call makefmt(ymax,ntxtchr,formt)
        do ii=0,10,2
          yval=ii*dy
          write(label,fmt=formt,err=40)yval
40        iy=iylo+3+margbot+ii*iyran/10
          call plax_sctext(1,itxtsiz,itxtsiz,251,ixlo,iy,
     &        label(1:ntxtchr))
        enddo
      endif
c       
c       set scaling and output grids
c       
      xscal=ixran/xmax
      yscal=iyran/ymax
      ixlo=ixlo+10+marglft
      iylo=iylo+10+margbot
      call scrnGridLine(ixlo,iylo,ixran/10,0,10)
      call scrnGridLine(ixlo,iylo,0,iyran/10,10)
      call scrnGridLine(ixlo,iylo+iyran,ixran/10,0,10)
      call scrnGridLine(ixlo+ixran,iylo,0,iyran/10,10)
      ixlas=ixlo
c       
c       draw bins
c       
      do i=1,nbins
        if(i*delr.le.xmax)then
          ix=ixlo+xscal*i*delr
          iy=iylo+yscal*graph(i)
          if(i.eq.1)then 
            call scrnMoveAbs(ixlas,iy)
          else
            call scrnVectAbs(ixlas,iy)
          endif
          call scrnVectAbs(ix,iy)
          ixlas=ix
        endif
      enddo
      call scrnUpdate(1)
c       
      write(*,321)'X',xmax,dx,'Y',ymax,dy
321   format(1x,a1,': 0 to',f13.5,', at',f13.6,'/div')
      return
      end



c       FOURDSP displays up to 4 graphs numbered from IGRSTRT to IGREND,
c       with automatic scaling by the graphing subroutine
c       GRAPHS is the array of graphs
c       LIMBINS is the dimension of its first array
c       NBINS is the number of bins (values)
c       DELR is bin width
c       XMAXDSP, YMAXDSP are arrays of XMAX and YMAX values returned for the
c       graphs
c       IGRFDSP is an array returned with the number of each displayed graph 
c       
      subroutine fourdsp(graphs,limbins,igrstrt,igrend,nbingrf,
     &    delrgrf, xmaxdsp, ymaxdsp,igrfdsp)
      real*4 graphs(limbins,*),xmaxdsp(*),ymaxdsp(*),delrgrf(*)
      integer*4 igrfdsp(*),nbingrf(*)
      jgrf=igrstrt
      do jj=1,4
        if(jgrf.le.igrend)then
          igrfdsp(jj)=jgrf
        else
          igrfdsp(jj)=0
        endif
        jgrf=jgrf+1
      enddo
      call somedsp(graphs,limbins,nbingrf,delrgrf,xmaxdsp,ymaxdsp,
     &    igrfdsp)
      return
      end



c       SOMEDSP displays up to 4 graphs whose numbers are passed in the
c       array IGRFDSP; all other arguments have the same meaning as for
c       FOURDSP
c       
      subroutine somedsp(graphs,limbins,nbingrf,delrgrf, xmaxdsp,
     &    ymaxdsp, igrfdsp)
      real*4 graphs(limbins,*),xmaxdsp(*),ymaxdsp(*),delrgrf(*)
      integer*4 igrfdsp(*),nbingrf(*)
      call scrnErase(-1)
      do jj=1,4
        jgrf=igrfdsp(jj)
        if(jgrf.gt.0)then
          write(*,105)jgrf,jj
105       format(' Graph',i3,' to window',i2,':')
          xmaxdsp(jj)=-1.
          ymaxdsp(jj)=-1.
          call graphdsp(graphs(1,jgrf),nbingrf(jgrf),delrgrf(jgrf),jj,
     &        jgrf,xmaxdsp(jj), ymaxdsp(jj))
        endif
      enddo
      return
      end



c       GRAPHPLT plots one graph to an NCAR metacode file
c       GRAPH is the array containing numbers to be graphed
c       NBINS is the number of bins (values)
c       DELR is the bin width in user's units
c       IPLOT is the desired plot # - 1 to 4 for standard plots or 0 to
c       set a special position or other characteristics, -1 for big array
c       JGRF is the graph number, which is printed on standard plots
c       XMAX and YMAX are the full-scale X and Y values
c       
      subroutine graphplt(graph,nbins,delr,iplot,jgrf,xmax,ymax)
      real*4 graph(*)
      real*4 xpltsiz/0.5/,ypltsiz/0.5/,xplt(4),yplt(4)
      data xplt/0,.5,0,.5/,yplt/.5,.5,0,0/
      character*2 dumchar
      character*10 label
      character*8 formt
      common /grfarr/nrow,ncol,irow,icol,ifbycol,nxtick, nytick,
     &    xgutter,ygutter,ymaxfix
      integer*4 in5
      common /nmsinput/ in5
c       
c       parameters controlling label output
c       
      ofsetlft=0.47
      ofsetrt=0.2
      ofsetbot=0.1
      ofsettop=0.1
      ntxtchr=7
      itxtsiz=8
c       
c       extra size of page
c       
      xextra=0.
      yextra=2.5
c       
c       defaults for standard graphs
c       
      ifbox=1
      ntx=10
      nty=10
      tiksiz=0.05
      iaxthick=1
      igrfthick=1
c       
      call psSetup(1,width,c2,c3,0)
      if(iplot.gt.0)then
c         
c         for standard graph, set size & location, output graphs #
c         
        xlo=width*xplt(min(4,iplot))+0.05+ofsetlft
        ylo=width*yplt(min(4,iplot))+0.05+ofsetbot
        xsize=xpltsiz*width-0.1-ofsetlft-ofsetrt
        ysize=ypltsiz*width-0.1-ofsettop-ofsetbot
        write(label,'(i2)')jgrf
c        call pwrit(xlo+0.2,ylo+ysize-0.3,label,2 ,itxtsiz*2,0,0)
        call psWriteText(xlo+0.2,ylo+ysize-0.3,label(1:2) ,itxtsiz*2,0,0)
c         
c         do X labels then y
c         
        call makefmt(xmax,ntxtchr,formt)
        do ii=0,10,2
          xval=ii*xmax/10.
          write(label,fmt=formt,err=30)xval
30        xlab=xlo+ii*xsize/10
c          call pwrit(xlab,ylo-0.12,label,ntxtchr,itxtsiz,0,0)
          call psWriteText(xlab,ylo-0.12,label(1:ntxtchr),itxtsiz,0,0)
        enddo
c         
        call makefmt(ymax,ntxtchr,formt)
        do ii=0,10,2
          yval=ii*ymax/10.
          write(label,fmt=formt,err=40)yval
40        ylab=ylo+ii*ysize/10
c          call pwrit(xlo-0.05,ylab,label,ntxtchr,itxtsiz,0,1)
          call psWriteText(xlo-0.05,ylab,label(1:ntxtchr),itxtsiz,0,1)
        enddo
c         
      elseif(iplot.lt.0.and.irow.gt.0.and.icol.gt.0)then
        xsize=(width-0.21-(ncol-1)*xgutter)/ncol
        ysize=(width-0.21-(nrow-1)*ygutter)/nrow
        xlo=(icol-1)*(xsize+xgutter)+0.1
        ylo=(nrow-irow)*(ysize+ygutter)+0.1
        if(ifbycol.ne.0)then
          irow=irow+1
          if(irow.gt.nrow)then
            irow=1
            icol=icol+1
            if(icol.gt.ncol)icol=1
          endif
        else
          icol=icol+1
          if(icol.gt.ncol)then
            icol=1
            irow=irow+1
            if(irow.gt.nrow)irow=1
          endif
        endif
        ntx=-nxtick
        nty=-nytick
        tiksiz=-.05
        iaxthick=1
        igrfthick=1
        ifbox=1
      else
c         
c         get characteristics for non-standard graph
c         
10      write(*,'(1x,a,$)')
     &      'X and Y size, lower left X and Y in inches: '
        read(in5,*)xsize,ysize,xlo,ylo
        xpls=xextra/2.
        ypls=yextra/2.
        if(xsize.le.0..or.xlo.lt.-xpls.or.xsize+xlo.gt.xpls+width-0.2
     &      .or. ysize.le.0..or.
     &      ylo.lt.-ypls.or.ysize+ylo.gt.ypls+width-0.2)goto 10
c         
        xlo=xlo+0.1
        ylo=ylo+0.1
        write(*,'(1x,a,/,a,$)')'# of ticks in X, in Y, tick size,'//
     &      ' axis and graph line', ' thickness, 1 for box: '
        read(in5,*)ntx,nty,tiksiz,iaxthick,igrfthick,ifbox
      endif
c       
c       set scaling, do grids
c       
      call psSetup(iaxthick,c1,c2,c3,0)
      xscal=xsize/xmax
      yscal=ysize/ymax
      call psGridLine(xlo,ylo,xsize,0.,ntx,tiksiz)
      call psGridLine(xlo,ylo,0.,ysize,nty,tiksiz)
      if(ifbox.ne.0)then
        call psGridLine(xlo,ylo+ysize,xsize,0.,ntx,-tiksiz)
        call psGridLine(xlo+xsize,ylo,0.,ysize,nty,-tiksiz)
      endif
      xlas=xlo
c       
c       draw graph
c       
      call psSetup(igrfthick,c1,c2,c3,0)
      do i=1,nbins
        if(i*delr .le. xmax)then
          x=xlo+xscal*i*delr
          y=ylo+yscal*graph(i)
          if(i.eq.1)then 
            call psMoveAbs(xlas,y)
          else
            call psVectAbs(xlas,y)
          endif
          call psVectAbs(x,y)
          xlas=x
        endif
      enddo
c       
c       label axes if doing special graph
c       
      if(iplot.eq.0)then
        call label_axis(xlo,ylo,xsize,xscal,abs(ntx),xtick,0,0)
        call label_axis(xlo,ylo,ysize,yscal,abs(nty),ytick,0,1)
        call psMiscItems(xscal,0.,xlo,xsize,yscal,0.,ylo,ysize)
      endif
      return
      end


c       SETGRAPHARRAY gets parameters for a special big array of graphs
c       
      subroutine setgrapharray
      common /grfarr/nrow,ncol,irow,icol,ifbycol,nxtick, nytick,
     &    xgutter,ygutter,ymaxfix
      data xgutter/0.2/,ygutter/0.2/
      integer*4 in5
      common /nmsinput/ in5
      irow=0
      icol=0
      write(*,'(1x,a,$)')'# of columns and rows: '
      read(in5,*)ncol,nrow
      if(nrow.le.0.or.ncol.le.0)return
      write(*,'(1x,a,$)')
     &    '0 to fill row by row, 1 to fill column by column: '
      read(in5,*)ifbycol
      write(*,'(1x,a,$)')'# of ticks along X and Y: '
      read(in5,*)nxtick,nytick
      write(*,'(1x,a,2f5.2,a,$)')'X and Y gutter size (/ for',xgutter
     &    ,ygutter,'): '
      read(in5,*)xgutter,ygutter
      write(*,'(1x,a,$)')
     &    'Value to scale Y to for ALL graphs, or 0 for no rescale: '
      read(in5,*)ymaxfix
      irow=1
      icol=1
      return
      end



c       OPENCOMFILE requests a command file name and either opens that file
c       for input instead of keyboard input, or restores keyboard input
c       if no file name is entered, or if end of file or error occurs.
c       
      subroutine opencomfile
      character*120 comfile
      integer*4 in5, inlast
      common /nmsinput/ in5
c       
      inlast = in5
      if (in5 .ne. 5) close(in5)
      in5 = 5
      write(*,*) 'Enter name of file with commands, or Return for input from keyboard'
      read(inlast,'(a)',err=10,end=10)comfile
      if(comfile.eq.' ')return
c       
c       7/20/00 CER remove readonly,shared for gnu
c       
      open(4,file=comfile,status='old',err=10)
      in5 = 4
10    return
      end



c       ADDTOAVG adds the set of graphs for the current region to the average
c       for previous regions (it works even if there are no previous regions)
c       It maintains the total sample area in the AREAS array and bases the
c       average density on the total count and total area.
c       
      subroutine addtoavg(graphs,areas,limbins,ngraph,nbins,nregion)
      real*4 graphs(limbins,*),areas(limbins,*)
      if(nregion.eq.1)then
        do jj=1,ngraph
          javg=jj+ngraph
          do ibin=1,nbins+1
            graphs(ibin,javg)=graphs(ibin,jj)
            areas(ibin,javg)=areas(ibin,jj)
          enddo
        enddo
      else
        do jj=1,ngraph
          javg=jj+ngraph
          do ibin=1,nbins+1
            areanew=areas(ibin,javg)+areas(ibin,jj)
            if(areanew.ne.0.)graphs(ibin,javg)=
     &          (areas(ibin,javg)*graphs(ibin,javg)
     &          +areas(ibin,jj)*graphs(ibin,jj))/areanew
            areas(ibin,javg)=areanew
          enddo
        enddo
      endif
      return
      end





c       MAKEFMT makes up a format appropriate for outputing values as large
c       as VALMAX in NCHAR places.
c       
      subroutine makefmt(valmax,nchar,formt)
      character*(*) formt
      log =0
      if(valmax.ne.0)log=min(nchar-2,max(0,int(alog10(abs(valmax)))))
      mdig=(nchar-2)-log
      if(valmax.lt.0.)mdig=min(mdig,nchar-3)
      write(formt,101)nchar,mdig
101   format('(f',i2,'.',i2,')')
      return
      end


c       CHECKGRF checks whether a graph number is legal
c       
      logical function checkgrf(igrf,maxgraph,nextragrf,listextra)
      integer*4 listextra(*)
      checkgrf=igrf.gt.0.and.igrf.le.maxgraph
      do i=1,nextragrf
        checkgrf=checkgrf.or.listextra(i).eq.igrf
      enddo
      return
      end


c       CHECKEXTRA checks whether an extra location is legal and allocates
c       it if necessary
c       
      logical function checkextra(igrfextra,limgraphs,listextra,
     &    nextragrf)
      integer*4 listextra(*)
      checkextra=.false.
      if(igrfextra.lt.1.or.igrfextra.gt.limgraphs)then
        print *,'Illegal extra graph location'
        return
      endif
      ifonlist=0
      do i=1,nextragrf
        if(listextra(i).eq.igrfextra)ifonlist=1
      enddo
      if(ifonlist.eq.0)then
        nextragrf=nextragrf+1
        listextra(nextragrf)=igrfextra
      endif
      checkextra=.true.
      return
      end


c       SAVE_GRAPH saves a graph to a file
c       
      subroutine save_graph(graphs,areas,nbins,delr,ifangdiff,rmin,
     &    rmax)
      real*4 graphs(*),areas(*)
      character*120 namein,filename
      save filename,nch
      integer*4 nch/0/
      integer*4 in5
      common /nmsinput/ in5
c       
      rminsav=rmin
      if(ifangdiff.eq.0)rminsav=areas(nbins+1)
      if(nch.eq.0)then
        write(*,'(1x,a,$)')'File name: '
      else
        write(*,'(1x,a,a,a,$)')'File name (Return for ',
     &      filename(1:nch),'): '
      endif
      read(in5,'(a)') namein
      nchin = len_trim(namein)
      if(nch.eq.0.and.nchin.eq.0)go to 40
      if(nchin.ne.0.and..not.(namein.eq.'/'))then
        nch=nchin
        filename=namein
      endif
C       7/14/00 CER remove carriagecontrol for gnu
      open(9,file=filename,err=40,status='unknown')
42    read(9,'(a4)',end=44)filein
      go to 42
44    write(9,101)nbins,delr,ifangdiff,rminsav,rmax
101   format(i5,e15.6,i3,2e15.6)
      write(9,102)(graphs(i),i=1,nbins)
102   format(5e15.6)
      write(9,102)(areas(i),i=1,nbins)
      close(9)  
      return
c       
c	DNM 3/7/01: moved this clause down here to stop warnings
c       
40    print *,'Nothing saved'
      return
      end


c       READ_GRAPH reads a graph from a file
c       
      subroutine read_graph(graphs,areas,nbins,delr,ifangdiff,rmin,
     &    rmax,irecget,needfile,iferr)
      real*4 graphs(*),areas(*)
      character*120 namein,filename
      save filename,nch
      integer*4 nch/0/
      integer*4 in5
      common /nmsinput/ in5
c       
      if(needfile.gt.0)then
        if(nch.eq.0)then
          write(*,'(1x,a,$)')'File name: '
        else
          write(*,'(1x,a,a,a,$)')'File name (Return for ',
     &        filename(1:nch),'): '
        endif
        read(in5,'(a)') namein
        nchin = len_trim(namein)
        irecgrf=irecget
        if(nch.eq.0.and.nchin.eq.0)go to 40
        if(nchin.ne.0.and..not.(namein.eq.'/'))then
          nch=nchin
          filename=namein
        endif
      endif
      irecgrf=irecget
      open(9,file=filename,err=40,status='old')
      if(irecgrf.lt.0)then
        write(*,'(1x,a,$)')'Number of graph in file: '
        read(in5,*)irecgrf
      endif
      if(irecgrf.le.0)go to 40
      do irec=1,irecgrf
        read(9,*,err=40,end=40)nbins,delr,ifangdiff,rmin,rmax
        read(9,*,err=40,end=40)(graphs(i),i=1,nbins)
        read(9,*,err=40,end=40)(areas(i),i=1,nbins)
        if(ifangdiff.eq.0)areas(nbins+1)=rmin
      enddo
      close(9)  
      iferr=0
      return
c       
c	DNM 3/7/01: moved this clause down here to stop warnings
c       
40    if(irecgrf.lt.0)then
        print *,'Nothing retrieved'
      else
        print *,'Nothing retrieved for file graph #',irecgrf
      endif
      close(9)
      iferr=1
      nbins=1
      delr=1.
      graphs(1)=0.
      areas(1)=0.
      return
      end

c       EXPORT_GRAPH exports a graph to a file for use elsewhere
c       
      subroutine export_graph(graphs,areas,nbins,delr)
      implicit none
      real*4 graphs(*),areas(*)
      character*120 filename
      integer*4 nbins
      real*4 delr
      integer*4 itype, ifcounts, i
      real*4 yval,xstart,xmid,xend
      integer*4 in5
      common /nmsinput/ in5
c       
      write(*,'(1x,a,$)')'0 to output densities, 1 to output counts: '
      read(in5,*)ifcounts

      write(*,'(a,/,a,/,a,/a,$)')
     &    ' Enter 1 to output points for drawing histogram graph,',
     &    '       2 to output bin starting distance and bin value,',
     &    '       3 to output bin middle distance and bin value,',
     &    '    or 4 to output starting and ending distance and value: '
      read(in5,*)itype

      write(*,'(1x,a,$)')'Output file name: '
      read(in5,'(a)') filename
      call dopen(9, filename, 'new', 'f')

      if (itype .eq. 1) write(9,101)0., 0.
101   format(3g15.6)

      do i = 1, nbins
        yval = graphs(i)
        if (ifcounts .ne. 0) yval = nint(graphs(i)*areas(i))
        xstart = (i-1)*delr
        xend = i * delr
        xmid = (i-0.5)*delr
        if (itype.eq.1) then
          write(9,101)xstart,yval
          write(9,101)xend,yval
        else if (itype.eq.2) then
          write(9,101)xstart,yval
        else if (itype.eq.3) then
          write(9,101)xmid,yval
        else 
          write(9,101)xstart,xend,yval
        endif
      enddo
      close(9)
      return
      end



      subroutine replacebins(ncomb,icstr,icend,nrepl,graphs,areas)
      parameter (limbins=1001)
      integer*4 mapbin(limbins)
      real*4 bincum(limbins)
      real*4 graphs(*),areas(*)
      toosmall=0.5
      if(ncomb.gt.0)then
        icombin=icstr
        do irep=1,nrepl
          areasum=0.
          countsum=0.
          do ibin=icombin,icombin+ncomb-1
            areasum=areasum+areas(ibin)
            countsum=countsum+areas(ibin)*graphs(ibin)
          enddo
          if(areasum.ne.0.)countsum=countsum/areasum
          areasum=areasum/ncomb
          do ibin=icombin,icombin+ncomb-1
            areas(ibin)=areasum
            graphs(ibin)=countsum
          enddo
          icombin=icombin+ncomb
        enddo
      else
c         
c         find total area to be divided up
c         
        areatot=0
        do ibin=icstr,icend
          areatot=areatot+areas(ibin)
        enddo
c         
c         initialize cumulate amount in each target bin
c         
        ntarg=-ncomb
        do it=1,ntarg
          bincum(it)=0.
        enddo
c         
c         find cumulative amount to midpoint of each bin and map it to a
c         target bin; add to cumulative amount for that bin
c         
        curcum=0.
        do ibin=icstr,icend
          binmid=curcum+areas(ibin)/2.
          itarg=min(ntarg,ifix(ntarg*binmid/areatot+1.))
          bincum(itarg)=bincum(itarg)+areas(ibin)
          mapbin(ibin)=itarg
          curcum=curcum+areas(ibin)
        enddo
c         
c         now repeatedly try to shift first and last bin in each target bin
c         into an adjacent bin if that will help even out cumulative amounts
c         on round 1, it will shift into empty bins if any
c         
        call shiftbins(mapbin,bincum,areas,icstr,icend,1)
c         
c         eliminate any target bins that are too small; stick in next
c         bin unless its the last one
c         
        do itarg=1,ntarg
          if(bincum(itarg).lt.toosmall*areatot/ntarg)then
            newtarg=itarg+1
            if(newtarg.gt.ntarg)newtarg=itarg-1
            do ibin=icstr,icend
              if(mapbin(ibin).eq.itarg)then
                mapbin(ibin)=newtarg
                bincum(itarg)=bincum(itarg)-areas(ibin)
                bincum(newtarg)=bincum(newtarg)+areas(ibin)
              endif
            enddo
          endif
        enddo
c         
c         again try to shift first and last bin in each target bin
c         but this time only into existing nonempty bins 
c         
        call shiftbins(mapbin,bincum,areas,icstr,icend,2)
c         
c         finally, shift the data
c         
        areamin=1.e10
        areamax=-1.e10
        narea=0
        do itarg=1,ntarg
          if(bincum(itarg).gt.1.e-6*areatot)then
            areasum=0.
            countsum=0.
            navg=0
            do ibin=icstr,icend
              if(mapbin(ibin).eq.itarg)then
                navg=navg+1
                countsum=countsum+areas(ibin)*graphs(ibin)
                areasum=areasum+areas(ibin)
              endif
            enddo
            areamin=min(areamin,areasum)
            areamax=max(areamax,areasum)
            narea=narea+1
            if(abs(areasum-bincum(itarg))/areasum.gt.1.e-6)
     &          print *,char(7)//'areas didnt add up'
            countsum=countsum/areasum
            areasum=areasum/navg
            do ibin=icstr,icend
              if(mapbin(ibin).eq.itarg)then
                graphs(ibin)=countsum
                areas(ibin)=areasum
              endif
            enddo
          endif
        enddo
        write(*,101)narea,areamin,areatot/narea,areamax
101     format(i4,' bins, area min, mean & max =',3f15.7)
      endif
      return
      end


      subroutine shiftbins(mapbin,bincum,areas,icstr,icend,nround)
      integer*4 mapbin(*)
      real*4 bincum(*),areas(*)
      itry=0
      do while(itry.lt.100.and.(nmove.gt.0.or.itry.eq.0))
        nmove=0
        do ibin=icstr,icend
          itbin=mapbin(ibin)
          ifmove=0
          if(ibin.gt.icstr)then
            itbinm1=mapbin(ibin-1)
            if(itbin.ne.itbinm1)then
              if(nround.eq.1)itbinm1=itbin-1
              if(abs(bincum(itbin)-bincum(itbinm1)).gt.
     &            abs((bincum(itbin)-areas(ibin))-
     &            (bincum(itbinm1)+areas(ibin))))then
                mapbin(ibin)=itbinm1
                bincum(itbinm1)=bincum(itbinm1)+areas(ibin)
                bincum(itbin)=bincum(itbin)-areas(ibin)
                ifmove=1
c		  write(*,'(4i4,3f7.2)')itry,ibin,itbin,itbinm1,
c                 &		      areas(ibin),bincum(itbinm1),bincum(itbin)
              endif
            endif
          endif
          if(ibin.lt.icend.and.ifmove.eq.0)then
            itbinp1=mapbin(ibin+1)
            if(itbin.ne.itbinp1)then
              if(nround.eq.1)itbinp1=itbin+1
              if(abs(bincum(itbin)-bincum(itbinp1)).gt.
     &            abs((bincum(itbin)-areas(ibin))-
     &            (bincum(itbinp1)+areas(ibin))))then
                mapbin(ibin)=itbinp1
                bincum(itbinp1)=bincum(itbinp1)+areas(ibin)
                bincum(itbin)=bincum(itbin)-areas(ibin)
                ifmove=1
c		  write(*,'(4i4,3f7.2)')itry,ibin,itbin,itbinp1,
c                 &		      areas(ibin),bincum(itbin),bincum(itbinp1)
              endif
            endif
          endif
          nmove=nmove+ifmove
        enddo
        itry=itry+1
      enddo
c	print *,nround,itry
      if(itry.eq.100)print *,char(7)//'Shiftbins did not converge'
      return
      end

c       OptionNeedsModel checks whether there is a modelfile defined,
c       and if not, it checks whether option IOPT is on the list
c       IOPTNEEDMODEL (size NOPTNEEDMODEL) of options that need a model

      logical function OptionNeedsModel(modelfile, ioptNeedModel,
     &    nOptNeedModel, iopt)
      implicit none
      character*(*) modelfile
      integer*4 iopt, ioptNeedModel(*), nOptNeedModel, i
      OptionNeedsModel = .false.
      if (modelfile .ne. ' ') return
      do i = 1, nOptNeedModel
        if (iopt .eq. ioptNeedModel(i)) then
          OptionNeedsModel = .true.
          print *,'Option not available - no model file defined'
          return
        endif
      enddo
      return
      end

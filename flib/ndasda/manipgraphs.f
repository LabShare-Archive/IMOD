c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2003/10/26 05:33:27  mast
c       change command files to use unit 4 instead reopening 5
c	
c       Revision 3.1  2003/08/08 16:38:55  mast
c       Add option to export graphs
c	
c       
      subroutine manipgraphs(iopt,prog,graphs,areas,nbingrf,delrgrf,
     &    ifadgrf,rmingrf,rmaxgrf,maxgraph,nextragrf,listextra,
     &    igrfdsp,xmaxdsp,ymaxdsp)
      parameter (limgraphs=50,limbins=1001)
      real*4 graphs(limbins,limgraphs),areas(limbins,limgraphs)
      real*4 delrgrf(*),rmingrf(*),rmaxgrf(*),xmaxdsp(*),ymaxdsp(*)
      character*(*) prog
      integer*4 nbingrf(*),ifadgrf(*),listextra(*),igrfdsp(*)
      integer*4 igrfrep(limgraphs),igrfadd(limgraphs)
      integer*4 lsrdextra(limgraphs),lsrdrec(limgraphs*2)
      logical checkextra,checkgrf,dumlog
      save igrfmov,movextra,igrfav1,igrfav2,iavgextra,coef1,coef2
      save iredextra,igrfstor,ncomb,icomstr,icomend,igrfrep,ngrfrep
      save igrfadd,nrgfadd,iaddextra,irdsmextra,itmpextra,nredextra
      save lsrdextra,lsrdrec,nredrec
      integer*4 in5
      common /nmsinput/ in5
c       
      go to(228,229,230,231,232,233,40,40,40,237,238,239,40,40,242)iopt-27
40    return
c       
c       Save or add graphs to extra graph locations
c       
228   write(*,'(1x,a,$)')
     &    '# of graph to save, # of graph location to save it into: '
      read(in5,*)igrfmov,movextra
      igrfsav=igrfmov
      igrfextra=movextra
      if(checkgrf(igrfsav,maxgraph,nextragrf,listextra))go to 2281
      print *,'Illegal graph # to save'
      go to 40
c       
229   write(*,'(1x,a,$)')'#''s of two graphs to average, # of'//
     &    ' graph location to place average: '
      read(in5,*)igrfav1,igrfav2,iavgextra
      igrfextra=iavgextra
      go to 2291
c       
230   write(*,'(1x,a,/,a,$)')'Linearly combine two graphs: Enter'//
     &    ' #''s of the two graphs to combine,','   the '//
     &    'coefficient for each, # of graph to place result in: '
      read(in5,*)igrfav1,igrfav2,coef1,coef2,iavgextra
      igrfextra=iavgextra
c       
2291  if(.not.(checkgrf(igrfav1,maxgraph,nextragrf,listextra).and.
     &    checkgrf(igrfav2,maxgraph,nextragrf,listextra)))then
        print *,'Illegal graph # to average'
        go to 40
      endif
      if(nbingrf(igrfav1).eq.nbingrf(igrfav2).and.
     &    delrgrf(igrfav1).eq.delrgrf(igrfav2))go to 2281
      print *,'Number of bins or bin size does not match'
      go to 40
c       
232   write(*,'(1x,a,$)')'Read graph from file: First enter # of '//
     &    'graph position to read it into: '
      read(in5,*)iredextra
      igrfextra=iredextra
c       
2281  if(.not.checkextra(igrfextra,limgraphs,listextra,
     &    nextragrf))go to 40
      if(iopt.eq.28)then
        call copygraph(graphs,areas,igrfsav,igrfextra,nbingrf,delrgrf,
     &      ifadgrf,rmingrf,rmaxgrf)
      elseif(iopt.eq.29)then
        call addgraphs(graphs,areas,igrfav1,igrfav2,igrfextra,
     &      nbingrf, delrgrf, ifadgrf,rmingrf,rmaxgrf,prog)
      elseif(iopt.eq.30)then
        do ibin=1,nbingrf(igrfav1)+1
          graphs(ibin,igrfextra)=max(0.,coef1*graphs(ibin,igrfav1)+
     &        coef2*graphs(ibin,igrfav2))
          areas(ibin,igrfextra)=areas(ibin,igrfav1)
        enddo
        nbingrf(igrfextra)=nbingrf(igrfav1)
        delrgrf(igrfextra)=delrgrf(igrfav1)
        ifadgrf(igrfextra)=ifadgrf(igrfav1)
        rmingrf(igrfextra)=rmingrf(igrfav1)
        rmaxgrf(igrfextra)=rmaxgrf(igrfav1)
      elseif(iopt.eq.32)then
        call read_graph(graphs(1,igrfextra),areas(1,igrfextra),
     &      nbingrf(igrfextra),delrgrf(igrfextra),ifadgrf(igrfextra),
     &      rmingrf(igrfextra),rmaxgrf(igrfextra),-1,1,iferr)
      endif
      go to 40
c       
c       save graph to file for rereading
c       
231   write(*,'(1x,a,$)')
     &    'Save graph to file: Enter number of graph to save: '
      read(in5,*)igrfstor
      igrfsav=igrfstor
      if(checkgrf(igrfsav,maxgraph,nextragrf,listextra))then
        call save_graph(graphs(1,igrfsav),areas(1,igrfsav),
     &      nbingrf(igrfsav),delrgrf(igrfsav),ifadgrf(igrfsav),
     &      rmingrf(igrfsav),rmaxgrf(igrfsav))
      else
        print *,'Illegal graph # to save'
      endif
      go to 40
c       
c       replace selected sets of bins with their averages
c       
233   write(*,'(1x,a,/,a,/,a,/,a,$)')'Replace bins by their averages:'
     &    //' Enter # of bins to average together','  in each'//
     &    ' replacement for regular combining,',' or - the desired'//
     &    ' # of final bins to get new bins based on nearly equal '//
     &    'areas;',' and enter starting and ending bins to replace: '
      read(in5,*)ncomb,icomstr,icomend
      if(abs(ncomb).le.1.or.icomstr.ge.icomend)go to 40
      print *,'Enter list of graphs to do replacement in (Return',
     &    ' for all graphs)'
      call rdlist(in5,igrfrep,ngrfrep)
      if(ngrfrep.eq.0)then
        ngrfrep=limgraphs
        do i=1,ngrfrep
          igrfrep(i)=i
        enddo
      endif
      do igrepl=1,ngrfrep
        jgrp=igrfrep(igrepl)
        if(checkgrf(jgrp,maxgraph,nextragrf,listextra))then
          icend=min(icomend,nbingrf(jgrp))
          icstr=max(1,icomstr)
          nrepl=(icend+1-icstr)/ncomb
          if((ncomb.gt.0.and.nrepl.gt.0).or.
     &        (ncomb.lt.0.and.-ncomb.le.icend-icstr))then
            call replacebins(ncomb,icstr,icend,nrepl,graphs(1,jgrp),
     &          areas(1,jgrp))
            do iwn=1,4
              if(igrfdsp(iwn).eq.jgrp)then
                xmaxdsp(iwn)=-1.
                ymaxdsp(iwn)=-1.
                call graphdsp(graphs(1,jgrp),nbingrf(jgrp),
     &              delrgrf(jgrp),iwn,jgrp,xmaxdsp(iwn),ymaxdsp(iwn))
              endif
            enddo
          endif
        endif
      enddo
      go to 40
c       
c       add list of graphs
c       
237   write(*,'(1x,a,$)')'List of graphs to average (ranges OK): '
      call rdlist(in5,igrfadd,ngrfadd)
      nadd=0
      do igadd=1,ngrfadd
        if(checkgrf(igrfadd(igadd),maxgraph,nextragrf,listextra))then
          nadd=nadd+1
        else
          igrfadd(igadd)=-1
        endif
      enddo
      if(nadd.eq.0)then
        print *,'No legal graphs specified'
        go to 40
      endif
      write(*,'(1x,a,$)')'Extra graph location in which to place sum:  '
      read(in5,*)iaddextra
      if(.not.checkextra(iaddextra,limgraphs,listextra,
     &    nextragrf))go to 40
c       
      nadd=0
      do igadd=1,ngrfadd
        jgrp=igrfadd(igadd)
        if(jgrp.gt.0)then
          if(nadd.eq.0)then
            call copygraph(graphs,areas,jgrp,iaddextra,nbingrf,
     &          delrgrf, ifadgrf,rmingrf,rmaxgrf)
          else
            call addgraphs(graphs,areas,jgrp,iaddextra,iaddextra,
     &          nbingrf, delrgrf, ifadgrf,rmingrf,rmaxgrf,prog)
          endif
          nadd=1
        endif
      enddo
      go to 40
c       
c       read list of graphs
c       
238   write(*,'(1x,a,/,a,$)')'Read list of graphs: first enter '//
     &    'list of extra graph locations in which to',
     &    '   place graphs (ranges OK): '
      call rdlist(in5,lsrdextra,nredextra)
      write(*,'(1x,a,$)')'List of graph #s in file: '
      call rdlist(in5,lsrdrec,nredrec)
      if(nredrec.ne.nredextra)then
        print *,'Number of graphs does not match'
        go to 40
      endif
      needname=1
      do ired=1,nredrec
        igrfextra=lsrdextra(ired)
        if(checkextra(igrfextra,limgraphs,listextra,nextragrf))
     &      then
          call read_graph(graphs(1,igrfextra),areas(1,igrfextra),
     &        nbingrf(igrfextra),delrgrf(igrfextra),ifadgrf(igrfextra),
     &        rmingrf(igrfextra),rmaxgrf(igrfextra),lsrdrec(ired),
     &        needname,iferr)
          needname=0
        else
          print *,'Illegal extra graph location for file graph #',
     &        lsrdrec(ired)
        endif
      enddo
      go to 40
c       
c       read and add series of graphs
c       
239   write(*,'(1x,a,/,a,$)')'Read and average series of graphs:'//
     &    ' First enter 2 extra graph locations,',
     &    '   one for the final average and one for temporary use: '
      read(in5,*)irdsmextra,itmpextra
      if(irdsmextra.le.0.or.irdsmextra.gt.limgraphs.or.
     &    itmpextra.le.0.or.itmpextra.gt.limgraphs.or.
     &    irdsmextra.eq.itmpextra)then
        print *,'Illegal extra graph locations'
        go to 40
      endif
      write(*,'(1x,a,$)')'List of graph #s in file: '
      call rdlist(in5,lsrdrec,nredrec)
      if(nredrec.le.0)go to 40
      igrfextra=irdsmextra
      needname=1
      do ired=1,nredrec
        dumlog=checkextra(igrfextra,limgraphs,listextra,nextragrf)
        call read_graph(graphs(1,igrfextra),areas(1,igrfextra),
     &      nbingrf(igrfextra),delrgrf(igrfextra),ifadgrf(igrfextra),
     &      rmingrf(igrfextra),rmaxgrf(igrfextra),lsrdrec(ired),
     &      needname,iferr)
        needname=0
        if(iferr.eq.0)then
          if(igrfextra.eq.irdsmextra)then
            igrfextra=itmpextra
          else
            call addgraphs(graphs,areas,itmpextra,irdsmextra,
     &          irdsmextra, nbingrf, delrgrf, ifadgrf,rmingrf,rmaxgrf,
     &          prog)
          endif
        endif
      enddo
      go to 40
c       
c       export graph to file for use in other programs
c       
242   write(*,'(1x,a,$)')
     &    'Export graph to file: Enter number of graph to export: '
      read(in5,*)igrfstor
      igrfsav=igrfstor
      if(checkgrf(igrfsav,maxgraph,nextragrf,listextra))then
        call export_graph(graphs(1,igrfsav),areas(1,igrfsav),
     &      nbingrf(igrfsav),delrgrf(igrfsav),ifadgrf(igrfsav),
     &      rmingrf(igrfsav),rmaxgrf(igrfsav))
      else
        print *,'Illegal graph # to export'
      endif
      go to 40
      end


      subroutine copygraph(graphs,areas,igrfsav,igrfextra,nbingrf,
     &    delrgrf, ifadgrf,rmingrf,rmaxgrf)
      parameter (limgraphs=50,limbins=1001)
      real*4 graphs(limbins,limgraphs),areas(limbins,limgraphs)
      real*4 delrgrf(*),rmingrf(*),rmaxgrf(*)
      integer*4 nbingrf(*),ifadgrf(*)
c       
      do ibin=1,nbingrf(igrfsav)+1
        graphs(ibin,igrfextra)=graphs(ibin,igrfsav)
        areas(ibin,igrfextra)=areas(ibin,igrfsav)
      enddo
      nbingrf(igrfextra)=nbingrf(igrfsav)
      delrgrf(igrfextra)=delrgrf(igrfsav)
      ifadgrf(igrfextra)=ifadgrf(igrfsav)
      rmingrf(igrfextra)=rmingrf(igrfsav)
      rmaxgrf(igrfextra)=rmaxgrf(igrfsav)
      return
      end


      subroutine addgraphs(graphs,areas,igrfav1,igrfav2,igrfextra,
     &    nbingrf, delrgrf, ifadgrf,rmingrf,rmaxgrf,prog)
      parameter (limgraphs=50,limbins=1001)
      real*4 graphs(limbins,limgraphs),areas(limbins,limgraphs)
      real*4 delrgrf(*),rmingrf(*),rmaxgrf(*)
      integer*4 nbingrf(*),ifadgrf(*)
      character*(*) prog
c       
      do ibin=1,nbingrf(igrfav1)+1
        areanew=areas(ibin,igrfav1)+areas(ibin,igrfav2)
        if(areanew.ne.0.)then
          graphs(ibin,igrfextra)=
     &        (areas(ibin,igrfav1)*graphs(ibin,igrfav1)
     &        +areas(ibin,igrfav2)*graphs(ibin,igrfav2))/areanew
        else
          graphs(ibin,igrfextra)=0.
        endif
        areas(ibin,igrfextra)=areanew
      enddo
      nbingrf(igrfextra)=nbingrf(igrfav1)
      delrgrf(igrfextra)=delrgrf(igrfav1)
      ifadgrf(igrfextra)=ifadgrf(igrfav1)
      if(prog.eq.'mtk')then
        rmingrf(igrfextra)=0.5*(rmingrf(igrfav1)+rmingrf(igrfav2))
      else
        rmingrf(igrfextra)=rmingrf(igrfav1)
      endif
      rmaxgrf(igrfextra)=rmaxgrf(igrfav1)
      return
      end

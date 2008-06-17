c       SUBROUTINES FOR NDA ONLY
c
c       $Id$
c
c       $Log$
c       Revision 3.3  2006/05/01 21:14:50  mast
c       Increased number of bins to 1001
c	
c
c       GETBINSPEC gets a bin specification appropriate for the type of
c       graphs being done
c       
      subroutine getbinspec(ifangdiff,delr,nbins,rmin,rmax)
      parameter (limbins=1001)
      integer*4 in5
      common /nmsinput/ in5
      if(ifangdiff.eq.0)then
        write(*,'(1x,a,$)')
     &      'Bin width (radial distance), number of bins: '
        read(in5,*)delr,nbins
        if(nbins.ge.limbins)print *,'# of bins truncated to',limbins-1
        nbins=min(nbins,limbins-1)
      else
        write(*,'(1x,a,$)')
     &      'Minimum and maximum radii of annulus, number of bins: '
        read(in5,*)rmin,rmax,nbins
        if(nbins.ge.limbins)print *,'# of bins truncated to',limbins-1
        nbins=min(nbins,limbins-1)
        delr=180./nbins
      endif
      return
      end
      


c       GETGRAPHSPEC gets specifications of the types involved in each
c       desired graph.
c       IFANGDIFF is 0 for radial graphs and 1 for annular graphs
c       LASTANGDIFF id the value of IFANGDIFF when graphs were last
c       specified: if it equals IFANGDIFF, then the program asks whether one
c       wants to use the same graphs
c       NGRAPH is returned with the number of graphs
c       NxxxTYP is returned with the number of types for reference items
c       (xxx=REF), neighboring items (xxx=NEIGH), or angular neighbors to the
c       neighboring points (xxx=ANG).
c       ITYPxxx is returned with the list of types for each case
c       
      subroutine getgraphspec(ifangdiff,lastangdiff,ngraph,itypref,
     &    nreftyp, itypneigh, nneightyp,itypang,nangtyp)
      parameter (limtyp=50,itypall=999)
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
      integer*4 nangtyp(*),itypang(limtyp,*)
      integer*4 in5
      common /nmsinput/ in5
c       
30    ifchange=1
      if(ifangdiff.eq.lastangdiff)then
        ifchange=0
        write(*,'(1x,a,$)')'0 to keep same graph specifications or'//
     &      ' 1 to specify new graphs: '
        read(in5,*)ifchange
      endif
      if(ifchange.ne.0)then
        write(*,'(1x,a,$)')'Number of different graphs to compute: '
        read(in5,*)ngraph
c         
        do ii=1,ngraph
          write(*,102)ii,'reference'
102       format(' For graph #',i3,', enter list of types for ',
     &        'points to be considered',/,5x,a,' points',
     &        ' (Return for all, ranges OK)')
          call rdlist(in5,itypref(1,ii),nreftyp(ii))
          if(nreftyp(ii).eq.0)then
            nreftyp(ii)=1
            itypref(1,ii)=itypall
          endif
c           
          write(*,102)ii,'neighboring'
          call rdlist(in5,itypneigh(1,ii),nneightyp(ii))
          if(nneightyp(ii).eq.0)then
            nneightyp(ii)=1
            itypneigh(1,ii)=itypall
          endif
c           
          if(ifangdiff.ne.0)then
            nangtyp(ii)=nneightyp(ii)
            do jj=1,nangtyp(ii)
              itypang(jj,ii)=itypneigh(jj,ii)
            enddo
            write(*,103)ii
103         format(' For graph #',i3,', enter list of types for ',
     &          'points to be considered angular',/,' neighbors ',
     &          'to those neighboring points (Return for all, / ',
     &          'for same as neighbors)')
            call rdlist(in5,itypang(1,ii),nangtyp(ii))
            if(nangtyp(ii).eq.0)then
              nangtyp(ii)=1
              itypang(1,ii)=itypall
            endif
          endif
        enddo
      endif
      lastangdiff=ifangdiff
      return
      end



c       INTEGRATE compute the integral of the items in some bins of a graph,
c       subtracting a baseline value as well.
c       GRAPHS has the values of the graph
c       IFANGDIFF is 0 for radial graphs and 1 for annular graphs
c       DELR is bin width (distance or degrees)
c       NBINS is number of bins
c       RMIN and RMAX are inner and outer radii for an annular graph
c       INTST, INTND are starting and ending bins to integrate
c       IBAST, IBAND are starting and ending bins to compute the baseline
c       value from, or 0,0 to use thevalue supplied in BASELINE
c       SUM is returned with the integral
c       CENTROID is retuned with the center of gravity of peak above baseline
c       
      subroutine integrate(graphs,ifangdiff,delr,rmin,rmax,nbins,
     &    intst,intnd, ibast,iband, baseval,sum, centroid)
      implicit none
      real*4 graphs(*),delr,rmin,rmax,baseval,sum,centroid
      integer*4 ifangdiff,nbins,intst,intnd, ibast,iband
      real*4 baseline,distsum,anularea,counts
      integer*4 ibin
      
      baseline=baseval
      if(ibast.le.iband.and.(ibast.ne.0.or.iband.ne.0))then
        baseline=0.
        do ibin=ibast,iband
          baseline=baseline+graphs(ibin)
        enddo
        baseline=baseline/(iband+1-ibast)
      endif
      sum=0.
      distsum = 0.
      centroid = 0.
      anularea=3.14159*(rmax**2-rmin**2)/nbins
      do ibin=intst,intnd
        if(ifangdiff.eq.0)anularea=3.14159*delr**2*(2*ibin-1)
        counts = anularea*(graphs(ibin)-baseline) 
        sum=sum+counts
        distsum = distsum + delr * (ibin - 0.5) * counts
      enddo
      if (sum .gt. 0.) centroid = distsum / sum
      return
      end


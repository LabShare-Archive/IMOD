c       ANGDIST computes a set of "graphs" of the density of items as a
c       function of angular separation, for items within a specified annulus
c       centered on a reference item.  The reference points, the
c       neighboring points and their angular neighbors may be of various
c       types or sets of types, as defined separately for each graph.  The
c       subroutine  corrects approximately for edge effects, so that the
c       points may be contained within an arbitrary, irregular boundary and
c       the routine will compute density based only on the area within the
c       boundary.
c       NVERT is the number of boundary points
c       BX, BY are the coordinates of the boundary points
c       NPOINT is the number of items
c       SX, SY, ITYPE are the coordinates and types of the items
c       RMIN, RMAX are the inner and outer radii of the annulus
c       NBINS is the number of bins for each graph
c       NGRAPH is the number of graphs
c       NREFTYP is the number of types for reference points for each graph
c       NNEIGHTYP is the number of types for neighbor points for each graph
c       NANGTYP is the number of types for angular neighbors to neighbor
c       points for each graph
c       ITYPREF(I,J) is the Ith reference type for the Jth graph
c       ITYPNEIGH(I,J) is the Ith neighbor type for the Jth graph
c       ITYPANG(I,J) is the Ith angluar neighbor type for the Jth graph
c       GRAPHS(I,J) is returned with the density at the Ith bin of Jth graph
c       FRACSUM(I,J) is returned with total area contributing to that bin
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       
      subroutine angdist(bx,by,nvert,sx,sy,itype,npoint,rmin,rmax,
     &    nbins, ngraph, nreftyp,nneightyp,nangtyp,itypref,itypneigh,
     &    itypang,graphs,fracsum)
      parameter (limgraphs=50,limbins=1001,limpnts=50000,limcross=20,
     &    limvert=500,limtyp=50,itypall=999,limnay=100,
     &    limptxgrf=100000)
      real*4 bx(*),by(*)                        !boundary vertices
      real*4 sx(*),sy(*)                        !sample points
      real*4 graphs(limbins,limgraphs)
      integer*4 itype(*)                        !types of sample points
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
      integer*4 nangtyp(*),itypang(limtyp,*)
c       
      real*4 fracsum(limbins,limgraphs),crossang(limcross)
      real*4 edgesq(limvert),cwccwmin(2),thetaneigh(limnay)
      integer*4 igraphref(limgraphs),neighbor(limnay)
     &    ,igraphnay(limnay)
      logical*1 neighpt(limptxgrf),angpt(limptxgrf)
c       
      rmaxsq=rmax**2
      rminsq=rmin**2
      rmidmaxsq=(0.5*(rmin+rmax))**2
      safexlo=rmax
      safexhi=-rmax
      safeylo=rmax
      safeyhi=-rmax
c       
c       Analyze which points are valid neighbor and angle points for each
c       graph
c       
      do ii=1,npoint
        do jj=1,ngraph
          neighpt((ii-1)*ngraph+jj)=.false.
          do kk=1,nneightyp(jj)
            if(itypneigh(kk,jj).eq.itypall .or.
     &          itypneigh(kk,jj).eq.itype(ii))
     &          neighpt((ii-1)*ngraph+jj)=.true.
          enddo
          angpt((ii-1)*ngraph+jj)=.false.
          do kk=1,nangtyp(jj)
            if(itypang(kk,jj).eq.itypall .or.
     &          itypang(kk,jj).eq.itype(ii))
     &          angpt((ii-1)*ngraph+jj)=.true.
          enddo
        enddo
      enddo
c       
c       compute squares of edge lengths
c       
      do i=1,nvert
        edgesq(i)=(bx(i+1)-bx(i))**2+(by(i+1)-by(i))**2
      enddo

c       
c       zero out the graphs and the fractional area tables
c       
      do ii=1,ngraph
        do jj=1,nbins
          graphs(jj,ii)=0.
          fracsum(jj,ii)=0.
        enddo
      enddo
c       
c       loop through each sample point considered as reference
c       
      t1sum=0.
      t2sum=0.
      do iref=1,npoint
c         
c         first make list of graphs the reference point is needed in
c         
        needref=0
        do jj=1,ngraph
          needed=0
          do kk=1,nreftyp(jj)
            if(itypref(kk,jj).eq.itypall .or.
     &          itypref(kk,jj).eq.itype(iref)) needed=1
          enddo
          if(needed.gt.0)then
            needref=needref+1
            igraphref(needref)=jj
          endif
        enddo
c         
        if(needref.gt.0)then
c           
c           if point is inside the safe box, mark it as contributing fully
c           to all of the graphs by adding 1 to fracsum
c           
c           t1=secnds(0.)
          x0=sx(iref)
          y0=sy(iref)
          ncross=0
          if(x0.lt.safexlo .or. x0.gt. safexhi .or.
     &        y0.lt.safeylo .and. y0.gt. safeyhi)then
c             
c             need to analyze number of boundary crossings by annulus
c             consider each edge of boundary in turn, find crossings
c             
            x1=bx(1)
            y1=by(1)
            d1sq=(x0-x1)**2+(y0-y1)**2
            do ivert=1,nvert
c               
c               get distance to nearest point within the edge segment
c               
              dbsq=edgesq(ivert)
              x2=bx(ivert+1)
              y2=by(ivert+1)
              d2sq=(x0-x2)**2+(y0-y2)**2
              if(d2sq.gt.d1sq+dbsq)then
                distsq=d1sq                     !if (x1,y1) is closest
              elseif(d1sq.gt.d2sq+dbsq)then
                distsq=d2sq                     !if (x2,y2) is closest
              else                              !if somewhere between is closer
                distsq=d2sq-(d2sq+dbsq-d1sq)**2/(4.*dbsq)
              endif
c               
c               if that distance is smaller than middle radius of annulus,
c               and max distance is higher, solve for crossing angles
c               
              if(distsq.lt.rmidmaxsq.and.max(d1sq,d2sq).ge.rmidmaxsq)
     &            then
                bov2a=-(d2sq-d1sq-dbsq)/(2.*dbsq)
                rad=bov2a**2+(rmidmaxsq-d1sq)/dbsq
                if(rad.ge.0)then
                  root=sqrt(rad)
                  nsol=0
                  do idir=-1,1,2
                    fsol=bov2a+idir*root
                    if(fsol.ge.0.and.fsol.le.1)then
                      nsol=nsol+1
                      xsol=fsol*(x2-x1)+x1-x0
                      ysol=fsol*(y2-y1)+y1-y0
                      ncross=ncross+1
                      crossang(ncross)=atan2d(ysol,xsol)
                    endif
                  enddo
                  if(nsol.eq.0)print *,'neither solution valid'
                else
                  print *,'imaginary solution'
                endif
              endif
              x1=x2
              y1=y2
              d1sq=d2sq
            enddo
c             
            if(mod(ncross,2).ne.0)print *,'odd number of crossings'
          endif
c           t1sum=t1sum+secnds(t1)
c           t2=secnds(0.)
c           
c           now make list of all neighboring points in annulus
c           
          ninannul=0
          do nay=1,npoint
            if(nay.ne.iref)then
              delx=x0-sx(nay)
              if(abs(delx).lt.rmax)then
                dely=y0-sy(nay)
                if(abs(dely).lt.rmax)then
                  rsq=delx**2+dely**2
                  if(rsq.eq.0.)then
                    print *,'Warning: duplicate point - run CHECKMTMOD'
                  elseif(rsq.lt.rmaxsq.and.rsq.ge.rminsq)then
                    ninannul=ninannul+1
                    neighbor(ninannul)=nay
                    thetaneigh(ninannul)=atan2d(dely,delx)
                  endif
                endif
              endif
            endif
          enddo
c           
c           loop through each neighbor point and consider it as reference
c           
          do nay=1,ninannul
c             
c             first make list of graphs this point is a valid neighbor for
c             
            neednay=0
            do jgrf=1,needref
              if(neighpt(igraphref(jgrf)+(neighbor(nay)-1)*ngraph))
     &            then
                neednay=neednay+1
                igraphnay(neednay)=igraphref(jgrf)
              endif
            enddo
c             
            if(neednay.gt.0)then
c               
c               if it's needed in any graphs, and there are no border
c               crossings, just increment every bin of fracsum twice
c               
              if(ncross.eq.0)then
                do jgrf=1,neednay
                  jj=igraphnay(jgrf)
                  do ibin=1,nbins
                    fracsum(ibin,jj)=fracsum(ibin,jj)+2.
                  enddo
                enddo
              else
c                 
c                 otherwise, need to determine which bins are inside border
c                 find minimum clockwise and countercw distance to a crossing
c                 
                cwccwmin(1)=400.
                cwccwmin(2)=400.
                do ic=1,ncross
                  cwang=crossang(ic)-thetaneigh(nay)
                  ccwang=-cwang
                  if(cwang.lt.0.)cwang=cwang+360.
                  if(ccwang.lt.0.)ccwang=ccwang+360.
                  cwccwmin(1)=min(cwccwmin(1),cwang)
                  cwccwmin(2)=min(cwccwmin(2),ccwang)
                enddo
c                 
c                 add 1 to bins within boundary on each side
c                 
                do icw=1,2
                  nbinad=nbins*cwccwmin(icw)/180.
                  nbextra=2*nbins+1-nbinad
                  do jgrf=1,neednay
                    jj=igraphnay(jgrf)
                    do ibin=1,min(nbins,nbinad)
                      fracsum(ibin,jj)=fracsum(ibin,jj)+1.
                    enddo
                    do ibin=nbextra,nbins
                      fracsum(ibin,jj)=fracsum(ibin,jj)+1.
                    enddo
                  enddo
                enddo
              endif
c               
c               now process other neighbors in annulus
c               
              
              do nang=1,ninannul
                if(nang.ne.nay)then
                  iptno=neighbor(nang)
                  angdif=abs(thetaneigh(nang)-thetaneigh(nay))
                  if(angdif.gt.180.)angdif=360.-angdif
                  ibin=min(nbins,ifix(nbins*angdif/180.+1.))
                  do jgrf=1,neednay
                    jj=igraphnay(jgrf)
                    if(angpt((iptno-1)*ngraph+jj))
     &                  graphs(ibin,jj)=graphs(ibin,jj)+1.
                  enddo
                endif
              enddo
            endif
          enddo
c           t2sum=t2sum+secnds(t2)
c           
        endif
      enddo
c       print *,t1sum,t2sum
c       
c       convert counts to densities
c       
c$$$    do jj=1,ngraph
c$$$    print *,'Fracsum #',jj
c$$$    write(*,'(7f11.4)')(fracsum(ii,jj),ii=1,nbins)
c$$$    print *,'Graph #',jj
c$$$    write(*,'(7f11.4)')(graphs(ii,jj),ii=1,nbins)
c$$$    if(jj.ne.ngraph)call mypause (' ')
c$$$    enddo
      binarea=3.14159*(rmaxsq-rminsq)/(2.*nbins)
      do ibin=1,nbins
        do jj=1,ngraph
          totarea=binarea*fracsum(ibin,jj)
          if(totarea.eq.0.)then
            graphs(ibin,jj)=0.
          else
            graphs(ibin,jj)=graphs(ibin,jj)/totarea
          endif
          fracsum(ibin,jj)=totarea
        enddo
      enddo
      return
      end


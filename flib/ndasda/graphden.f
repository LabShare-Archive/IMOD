c       GRAPHDEN produces a series of "graphs" of average density (per
c       unit area) of neighboring items as a function of radial distance
c       from an average reference point.  The reference points and the
c       neighboring points may be of various types or sets of types, as
c       defined separately for each graph.  The subroutine accurately
c       corrects for edge effects, so that the points may be contained
c       within an arbitrary, irregular boundary and the routine will compute
c       density based only on the area within the boundary.
c       NVERT is the number of boundary points
c       BX, BY are the coordinates of the boundary points
c       NPOINT is the number of items
c       SX, SY, ITYPE are the coordinates and types of the items
c       DELR is the bin width (in distance units) desired for the graphs
c       NBINS is the number of bins for each graph
c       NGRAPH is the number of graphs
c       NREFTYP is the number of types for reference points for each graph
c       NNEIGHTYP is the number of types for neighbor points for each graph
c       ITYPREF(I,J) is the Ith reference type for the Jth graph
c       ITYPNEIGH(I,J) is the Ith neighbor type for the Jth graph
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

      subroutine dengraph(bx,by,nvert,sx,sy,itype,npoint,delr,nbins,
     &    ngraph,nreftyp,nneightyp,itypref,itypneigh,graphs,fracsum)
      parameter (limgraphs=50,limbins=1001,limpnts=50000,limcross=20,
     &    limvert=500,limtyp=50,itypall=999,limptxgrf=100000)
      real*4 bx(*),by(*)                        !boundary vertices
      real*4 sx(*),sy(*)                        !sample points
      real*4 graphs(limbins,limgraphs)
      integer*4 itype(*)                        !types of sample points
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
c       
      real*4 fracsum(limbins,limgraphs),crossang(limcross,limbins)
      real*4 edgesq(limvert)
      integer*4 ncross(limbins),igraphref(limgraphs)
      logical inside
      logical*1 neighpt(limptxgrf)
      real *8 fsol,rad,rsq,distsq,d1sq,d2sq,dbsq,bov2a,radpart,
     &    distmaxsqr,root
c       
      rmax=nbins*delr
      rmaxsq=rmax**2
      rmidmaxsq=((nbins-0.5)*delr)**2
      safexlo=rmax                              !"Safe box" not implemented
      safexhi=-rmax                             !Sorry
      safeylo=rmax
      safeyhi=-rmax
c       
c       Analyze which points are valid neighbor points for each graph
c       
      do ii=1,npoint
        do jj=1,ngraph
          neighpt((ii-1)*ngraph+jj)=.false.
          do kk=1,nneightyp(jj)
            if(itypneigh(kk,jj).eq.itypall .or.
     &          itypneigh(kk,jj).eq.itype(ii))
     &          neighpt((ii-1)*ngraph+jj)=.true.
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
          if(x0.ge.safexlo .and. x0.le. safexhi .and.
     &        y0.ge.safeylo .and. y0.le. safeyhi)then
            do ineed=1,needref
              jj=igraphref(ineed)
              do ii=1,nbins
                fracsum(ii,jj)=fracsum(ii,jj)+1.
              enddo
            enddo
          else
c             
c             otherwise need to analyze number of crossings of annulus at
c             each bin;  start by zeroing list of crossings
c             
            do ii=1,nbins
              ncross(ii)=0
            enddo
c             
c             consider each edge of boundary in turn, find range of annuli
c             that cross it
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
c               if that distance is smaller than radius of highest bin,
c               get # of first bin that might cross edge segment
c               
              if(distsq.lt.rmidmaxsq)then
                distmin=sqrt(max(0.d0,distsq))
                ibmin=distmin/delr+1.5
                bov2a=-(d2sq-d1sq-dbsq)/(2.*dbsq)
                radpart=bov2a**2-d1sq/dbsq
                distmaxsq=max(d1sq,d2sq)
c                 
c                 solve for crossings of each bin and store angles
c                 
                do ibin=ibmin,nbins
                  r=(ibin-0.5)*delr
                  rsq=r**2
                  if(rsq.gt.distmaxsq)go to 20
                  rad=radpart+rsq/dbsq
                  if(rad.ge.0)then
                    root=sqrt(max(0.d0,rad))
                    nsol=0
                    do idir=-1,1,2
                      fsol=bov2a+idir*root
c                       
c                       make test open on one end to prevent double-counting
c                       of segment endpoint
c                       
                      if(fsol.gt.0.and.fsol.le.1)then
                        nsol=nsol+1
                        xsol=fsol*(x2-x1)+x1-x0
                        ysol=fsol*(y2-y1)+y1-y0
                        ncross(ibin)=ncross(ibin)+1
                        crossang(ncross(ibin),ibin)=atan2d(ysol,xsol)
                      endif
                    enddo
                    if(nsol.eq.0) print *,'neither solution valid'
                  else
                    print *,'imaginary solution'
                  endif
                enddo
20              continue
              endif
              x1=x2
              y1=y2
              d1sq=d2sq
            enddo
c             
c             find first and last bins with non-zero #'s of crossings
c             
            ibstrt=nbins+1
            ibend=nbins
            do i=1,nbins
              if(ncross(i).ne.0)then
                ibend=i
                if(ibstrt.eq.nbins+1)ibstrt=i
              endif
            enddo
c             
c             add 1 to fracsum for all bins up to the first, then analyze
c             crossings for the rest
c             
            fracadd=1.
            do ibin=1,ibend
              if(ibin.ge.ibstrt)then
                if(ncross(ibin).eq.0)then
                  print *,'no boundary crossings for bin between bins'
     &                ,' with some crossings'
                elseif(mod(ncross(ibin),2).ne.0)then
                  print *,'odd number of crossings'
                else
c                   
c                   order the crossing angles then add up difference between
c                   non-overlapping pairs
c                   
                  do ii=1,ncross(ibin)-1
                    do jj=ii+1,ncross(ibin)
                      if(crossang(ii,ibin).gt.crossang(jj,ibin))then
                        ctmp=crossang(ii,ibin)
                        crossang(ii,ibin)=crossang(jj,ibin)
                        crossang(jj,ibin)=ctmp
                      endif
                    enddo
                  enddo
c                   
                  diffsum=0.
                  do ii=2,ncross(ibin),2
                    diffsum=diffsum+crossang(ii,ibin)-
     &                  crossang(ii-1,ibin)
                  enddo
c                   
c                   if point at +/-180 degrees is inside, need the complement
c                   
                  if(inside(bx,by,nvert,x0-(ibin-0.5)*delr,y0))
     &                diffsum=360.-diffsum
                  fracadd=diffsum/360.
                endif
              endif
c               
              do ineed=1,needref
                jj=igraphref(ineed)
                fracsum(ibin,jj)=fracsum(ibin,jj)+fracadd
              enddo
            enddo             
          endif
c           t1sum=t1sum+secnds(t1)
c           t2=secnds(0.)
c           
c           now add neighboring points to bins as needed
c           
          do nay=1,npoint
            if(nay.ne.iref)then
              delx=abs(x0-sx(nay))
              if(delx.lt.rmax)then
                dely=abs(y0-sy(nay))
                if(dely.lt.rmax)then
                  rsq=delx**2+dely**2
                  if(rsq.lt.rmaxsq)then
                    ibin=sqrt(rsq)/delr + 1
                    do ineed=1,needref
                      jj=igraphref(ineed)
                      if(neighpt((nay-1)*ngraph+jj))
     &                    graphs(ibin,jj)=graphs(ibin,jj)+1.
                    enddo
                  endif
                endif
              endif
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
      do ibin=1,nbins
        anularea=3.14159*delr**2*(2*ibin-1)
        do jj=1,ngraph
          totarea=anularea*fracsum(ibin,jj)
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

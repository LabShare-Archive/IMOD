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
c       Revision 3.1  2002/06/05 21:12:39  mast
c       Passed remaining size of boundary vertex array to convexbound
c       
c       
      subroutine bundledist(xmt,ymt,zmt,indstrt,npntobj,icolor,nmt,
     &    delr,nbins,ngraph,nreftyp,nneightyp,itypref,itypneigh,
     &    iwhichend,ifnearest,pad,fracomit,graphs,fracsum,zscal,
     &    zstart,zend,xymask,winmin,winmax,iobjwin,nobjwin,xyzend)

      parameter (limgraphs=50,limbins=1001,limpnts=2000,limptbnd=10000,
     &    limtyp=50,itypall=999,limsec=200,limbund=20,minbund=4,
     &    limvert=10000)
      real*4 xmt(*),ymt(*),zmt(*)
      integer*4 indstrt(*),npntobj(*)
      real*4 graphs(limbins,*),xyzend(3,*)
      integer*4 icolor(*)                       !types of sample points
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
      integer*4 iwhichend(limtyp,*),iobjwin(*)
      real*4 fracsum(limbins,*),xymask(*)
c       
      integer*4 ibundxind(-255:255),itypbund(limbund),ninsec(limsec)
      real*4 sx(limptbnd),sy(limptbnd),dist(limbund)
      integer*4 indvert(limbund,limsec),nvert(limbund,limsec)
      real*4 xcen(limbund,limsec),ycen(limbund,limsec)
      real*4 bx(limvert),by(limvert),angle(limvert)
      integer*4 igraphobj(limgraphs),iwhichobj(limgraphs)
      integer*4 needbund(limbund)
      logical countend,inside
c       
c       make list of types in bundles and get cross-index
c       
      nobjwin=0
      zsum=0
      zsumsq=0
      nbund=0
      do i=1,ngraph
        do j=1,nreftyp(i)
          inlist=0
          do m=1,nbund
            if(itypbund(m).eq.itypref(j,i))inlist=1
          enddo
          if(inlist.eq.0)then
            nbund=nbund+1
            itypbund(nbund)=itypref(j,i)
            ibundxind(itypbund(nbund))=nbund
          endif
        enddo
      enddo
c       
c       for each section and each bundle, get the convex boundary
c       
      indbase=1
      izstrt=nint(zstart+1.)
      izend=nint(zend-1.)
      nsect=izend+1-izstrt
      do ibund=1,nbund
        ity=itypbund(ibund)
        nmax=0
c         
c         count the MT's in the bundle, then pack the bundle into an array
c         that can be passed directly to CONVEXBOUND for each section
c         
        do imt=1,nmt
          if(icolor(imt).eq.ity)nmax=nmax+1
        enddo
        do isec=1,nsect
          ninsec(isec)=0
        enddo
        do imt=1,nmt
          if(icolor(imt).eq.ity)then
            do ind=indstrt(imt),indstrt(imt)+npntobj(imt)-1
              iz=nint(zmt(ind)/zscal)
              if(iz.ge.izstrt.and.iz.le.izend)then
                isec=iz+1-izstrt
                ninsec(isec)=ninsec(isec)+1
                ipos=ninsec(isec)+(isec-1)*nmax
                sx(ipos)=xmt(ind)
                sy(ipos)=ymt(ind)
              endif
            enddo
          endif
        enddo
c         
c         for each section get the boundary, and compute angles of the
c         boundary points.  If too few points, set index to zero
c         
        do isec=1,nsect
          if(ninsec(isec).ge.minbund)then
            call convexbound(sx(1+(isec-1)*nmax),sy(1+(isec-1)*nmax),
     &          ninsec(isec),fracomit,pad,bx(indbase),by(indbase),
     &          nvert(ibund,isec),xcen(ibund,isec),ycen(ibund,isec),
     &          limvert-indbase)
            indvert(ibund,isec)=indbase
            if(.not.inside(bx(indbase),by(indbase),nvert(ibund,isec),
     &          xcen(ibund,isec),ycen(ibund,isec)))write(*,'(a,2i5)')
     &          ' Centroid not inside boundary, bundle, section:'
     &          //char(7),ibund,isec
            do ivert=1,nvert(ibund,isec)
              ind=indbase+ivert-1
              angle(ind)=atan2(by(ind)-ycen(ibund,isec),
     &            bx(ind)-xcen(ibund,isec))
              if(ivert.gt.1)then
                if(angle(ind).le.angle(ind-1))write(*,'(a,3i5)')
     &              ' Angle out of order, bundle, section, vertex:'
     &              //char(7),ibund,isec,ivert
              endif
            enddo
            indbase=ind+1
          else
            indvert(ibund,isec)=0
          endif
        enddo
      enddo
c       
c       zero out the graphs 
c       
      do ii=1,ngraph
        do jj=1,nbins
          graphs(jj,ii)=0.
          fracsum(jj,ii)=0.
        enddo
      enddo
      distlim=delr*nbins
c       
c       loop through each mt
c       
      do imt=1,nmt
c         
c         first make list of graphs the point is needed in, and list of
c         bundles to get distance from
c         
        needobj=0
        nbundneed=0
        do jj=1,ngraph
          needed=0
          do kk=1,nneightyp(jj)
            if(itypneigh(kk,jj).eq.itypall .or.
     &          itypneigh(kk,jj).eq.icolor(imt)) needed=kk
          enddo
          if(needed.gt.0)then
            needobj=needobj+1
            igraphobj(needobj)=jj
            iwhichobj(needobj)=iwhichend(needed,jj)
            do iref=1,nreftyp(jj)
              inlist=0
              ibund=ibundxind(itypref(iref,jj))
              do m=1,nbundneed
                if(needbund(m).eq.ibund)inlist=1
              enddo
              if(inlist.eq.0)then
                nbundneed=nbundneed+1
                needbund(nbundneed)=ibund
              endif
            enddo
          endif
        enddo
c         
        if(needobj.gt.0)then
c           
c           go through object finding distances from bundles
c           
          indlim=indstrt(imt)+npntobj(imt)-1
          do indmt=indstrt(imt),indlim
            iz=nint(zmt(indmt)/zscal)
            if(iz.ge.izstrt.and.iz.le.izend.and.
     &          xmt(indmt).ge.xymask(1).and.
     &          xmt(indmt).le.xymask(2).and.
     &          ymt(indmt).ge.xymask(3).and.
     &          ymt(indmt).le.xymask(4) )then
              isec=iz+1-izstrt
              do ineed=1,nbundneed
                ibund=needbund(ineed)
                centx=xcen(ibund,isec)
                centy=ycen(ibund,isec)
                dx=xmt(indmt)-centx
                dy=ymt(indmt)-centy
                distcen=dx**2+dy**2
                if(distcen.lt.1.e-6)then
                  dist(ibund)=0.
                else
c                   
c                   find vertices that this MT's angle is between
c                   
                  next=indvert(ibund,isec)
                  if(next.eq.0)then
                    dist(ibund)=-1.
                  else
                    anglemt=atan2(dy,dx)
                    ivend=next+nvert(ibund,isec)-1
                    last=ivend
                    if(anglemt.lt.angle(ivend))then
                      do while(next.lt.ivend.and.
     &                    anglemt.ge.angle(next))
                        next=next+1
                      enddo
                      if(next.gt.indvert(ibund,isec))last=next-1
                    endif
c                     
c                     compute relative distance from bundle: at center = 0,
c                     at edge = 1, beyond = >1
c                     
                    dist(ibund)=(-dy*(bx(next)-bx(last))+
     &                  dx*(by(next)-by(last)))/
     &                  ((bx(next)-bx(last))*(centy-by(last))
     &                  -(centx-bx(last))*(by(next)-by(last)))
                    if(dist(ibund).lt.0)then
                      print *,'negative distance:'//char(7),dist(ibund)
                      dist(ibund)=-dist(ibund)
                    endif
                  endif
                endif
              enddo
c               
c               now add counts to the graphs
c               
              do ineed=1,needobj
                igrf=igraphobj(ineed)
                distmin=1.e10
                countend=(indmt.eq.indstrt(imt).and.
     &              mod(iwhichobj(ineed),2).eq.1) .or.
     &              (indmt.eq.indlim.and.iwhichobj(ineed).ge.2)
                do j=1,nreftyp(igrf)
                  ibund=ibundxind(itypref(j,igrf))
                  if(dist(ibund).ge.0.)then
                    if(ifnearest.ne.0)then
                      distmin=min(distmin,dist(ibund))
                    else
                      ibin=dist(ibund)/delr+1.
                      if(ibin.le.nbins)then 
                        fracsum(ibin,igrf)=fracsum(ibin,igrf)+1.
                        if(countend)
     &                      graphs(ibin,igrf)=graphs(ibin,igrf)+1.
                      endif
                    endif
                  endif
                enddo
                if(ifnearest.ne.0.and.distmin.lt.1.e10)then
                  ibin=distmin/delr+1.
                  if(ibin.le.nbins)then 
                    fracsum(ibin,igrf)=fracsum(ibin,igrf)+1.
                    if(countend)graphs(ibin,igrf)=graphs(ibin,igrf)+1.
                  endif
                  if(countend.and.winmax.ne.winmin.and.
     &                distmin.ge.winmin.and.distmin.le.winmax)then
                    nobjwin=nobjwin+1
                    iobjwin(nobjwin)=imt
                    zsum=zsum+iz
                    zsumsq=zsumsq+iz**2
                    xyzend(1,nobjwin)=xmt(indmt)
                    xyzend(2,nobjwin)=ymt(indmt)
                    xyzend(3,nobjwin)=zmt(indmt)
                  endif
                endif
              enddo
            endif
          enddo
        endif
      enddo
c       
c       turn counts into probabilities per unit of Z
c       
      do ibin=1,nbins
        do jj=1,ngraph
          fracsum(ibin,jj)=fracsum(ibin,jj)*zscal
          if(fracsum(ibin,jj).gt.0.)graphs(ibin,jj)=
     &        graphs(ibin,jj)/fracsum(ibin,jj)
        enddo
      enddo
      if(nobjwin.ne.0)then
        call sums_to_avgsd(zsum,zsumsq,nobjwin,zav,zsd)
        write(*,106)nobjwin,zav,zsd
106     format(i4,' distances in window; Z mean=',f7.1,',  SD=',
     &      f7.1)
      endif

      return
      end

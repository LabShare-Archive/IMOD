c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       
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
      subroutine surfden(vertinreg,poreinreg,ndiv,delr,nbins,
     &    ngraph,nreftyp,nneightyp,itypref,itypneigh,graphs,fracsum,
     &    manyrandom)
      include 'sda.inc'
      parameter (limgraphs=50,limbins=1001,limcross=20,
     &    limvert=500,limtyp=50,itypall=999,limptxgrf=100000)
      real*4 graphs(limbins,limgraphs)
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
      logical*1 poreinreg(*),vertinreg(*)
c       
      real*4 fracsum(limbins,limgraphs)
      integer*4 igraphref(limgraphs)
      logical*1 neighpt(limptxgrf)
      real*4 distvert(maxverts),subdist(800),subvert(3,800)
      integer*4 indsub(3,800)
c       
      if(ngraph.eq.0)return
      rmax=nbins*delr
      rmaxsq=rmax**2
      rmidmaxsq=((nbins-0.5)*delr)**2
c       
c       Analyze which points are valid neighbor points for each graph
c       
      do ii=1,npore
        if(poreinreg(ii))then
          do jj=1,ngraph
            neighpt((ii-1)*ngraph+jj)=.false.
            do kk=1,nneightyp(jj)
              if(itypneigh(kk,jj).eq.itypall .or.
     &            itypneigh(kk,jj).eq.itype(ii))
     &            neighpt((ii-1)*ngraph+jj)=.true.
            enddo
          enddo
        endif
      enddo
c       
c       zero out the graphs and the fractional area tables
c       
      do ii=1,ngraph
        do jj=1,nbins
          graphs(jj,ii)=0.
          fracsum(jj,ii)=0.
        enddo
        fracsum(nbins+1,ii)=0.
      enddo
c       
c       loop through each sample point considered as reference
c       
      t1sum=0.
      t2sum=0.
      do iref=1,npore
        needref=0
        if(poreinreg(iref))then
          if(manyrandom.eq.0)then
            write(*,'(a,i5,$)')char(13)//'Doing pore #',iref
            call flush(6)
          endif
c           
c           first make list of graphs the reference point is needed in
c           
          do jj=1,ngraph
            needed=0
            do kk=1,nreftyp(jj)
              if(itypref(kk,jj).eq.itypall .or.
     &            itypref(kk,jj).eq.itype(iref)) needed=1
            enddo
            if(needed.gt.0)then
              needref=needref+1
              igraphref(needref)=jj
              fracsum(nbins+1,jj)=fracsum(nbins+1,jj)+1
            endif
          enddo
        endif
c         
        if(needref.gt.0)then
c           
c           check every triangle. Process each vertex and see if outside
c           range or get distance. Mark vertex distance as -2 initially,
c           then set to -1 if outside range or store actual distance
c           
          ncircle=0
          itriref=itripore(iref)
          do iv=1,nvert
            ind=listvert(iv)
            distvert(ind)=-2.
          enddo
          do itri=1,ntriang
            ninrange=0
            do ivert=1,3
              ind=indvert(ivert,itri)
              if(distvert(ind).eq.-2.)then
                distvert(ind)=-1.
                if(vertinreg(ind))then
                  dx=verts(1,ind)-pore(1,iref)
                  if(abs(dx).le.rmax)then
                    dy=verts(2,ind)-pore(2,iref)
                    if(abs(dy).le.rmax)then
                      dz=verts(3,ind)-pore(3,iref)
                      if(abs(dz).le.rmax)then
                        if(dx**2+dy**2+dz**2.le.rmaxsq)then
c			    print *,'3d dist =',sqrt(dx**2+dy**2+dz**2)
                          call sddist(pore(1,iref),itriref,
     &                        verts(1,ind),itri,distvert(ind))
                          if(distvert(ind).eq.-4.)ncircle=ncircle+1
                        endif
                      endif
                    endif
                  endif
                endif
              endif
              if(distvert(ind).ge.0..and.distvert(ind).le.rmax)
     &            ninrange=ninrange+1
            enddo
            if(ninrange.gt.0)then
c               
c               this triangle has at least one vertex in range, subdivide
c               and analyze each subtriangle
c               
              ind1=indvert(1,itri)
              ind2=indvert(2,itri)
              ind3=indvert(3,itri)
              call subtriangle(verts(1,ind1),
     &            verts(1,ind2),verts(1,ind3),
     &            ndiv, subvert,indsub,nsubv,nsubtr)
              do i=1,nsubv
                subdist(i)=-2.
              enddo
              subdist(1)=distvert(ind1)
              subdist(nsubv-ndiv)=distvert(ind2)
              subdist(nsubv)=distvert(ind3)
              areasub=triarea(itri)/nsubtr
c               
c               go through each subtriangle getting mean distance of 3
c               vertices
c               
              do isub=1,nsubtr
                distsum=0.
                ifout=0
                do ivert=1,3
                  ind=indsub(ivert,isub)
                  if(subdist(ind).lt.-1.)call sddist(pore(1,iref),
     &                itriref,subvert(1,ind),itri,subdist(ind))
                  if(subdist(ind).eq.-4.)ncircle=ncircle+1
                  distsum=distsum+subdist(ind)
                  if(subdist(ind).lt.0.)ifout=1
                enddo
                if(ifout.eq.0)then
                  distavg=distsum/3.
                  ibin=distavg/delr+1.
                  if(ibin.le.nbins)then
                    do ineed=1,needref
                      jj=igraphref(ineed)
                      fracsum(ibin,jj)=fracsum(ibin,jj)+areasub
                    enddo
                  endif
                endif
              enddo
            endif
          enddo
c	    t1sum=t1sum+secnds(t1)
c	    t2=secnds(0.)
c           
c           now add neighboring points to bins as needed
c           
          do nay=1,npore
            if(poreinreg(nay).and.nay.ne.iref)then
              neednay=0
              do ineed=1,needref
                jj=igraphref(ineed)
                if(neighpt((nay-1)*ngraph+jj))neednay=neednay+1
              enddo
              if(neednay.gt.0)then
                dx=pore(1,nay)-pore(1,iref)
                if(abs(dx).le.rmax)then
                  dy=pore(2,nay)-pore(2,iref)
                  if(abs(dy).le.rmax)then
                    dz=pore(3,nay)-pore(3,iref)
                    if(abs(dz).le.rmax)then
                      if(dx**2+dy**2+dz**2.le.rmaxsq)then
                        call sddist(pore(1,iref),itriref,
     &                      pore(1,nay),itripore(nay),distpore)
                        if(distpore.eq.-4.)ncircle=ncircle+1
                        ibin=distpore/delr + 1.
                        if(distpore.ge.0..and.ibin.le.nbins)then
c			    print *,iref,nay
                          do ineed=1,needref
                            jj=igraphref(ineed)
                            if(neighpt((nay-1)*ngraph+jj))
     &                          graphs(ibin,jj)=graphs(ibin,jj)+1.
                          enddo
                        endif
                      endif
                    endif
                  endif
                endif
              endif
            endif
          enddo
c	    t2sum=t2sum+secnds(t2)
c           
          if(ncircle.gt.0)write(*,104)ncircle
104       format('  WENT IN CIRCLES',i4,' times')
        endif
      enddo
c	print *,t1sum,t2sum
c       
c       convert counts to densities
c       
      do ibin=1,nbins
        do jj=1,ngraph
          totarea=fracsum(ibin,jj)
          if(totarea.eq.0.)then
            graphs(ibin,jj)=0.
          else
            graphs(ibin,jj)=graphs(ibin,jj)/totarea
          endif
        enddo
      enddo
      if(manyrandom.eq.0)write(*,*)
      return
      end

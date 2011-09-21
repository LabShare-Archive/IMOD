      function dist3d(ptcur,ptb)
      real*4 ptcur(3),ptb(3)
      dist3d=sqrt((ptcur(1)-ptb(1))**2+(ptcur(2)-ptb(2))**2
     &    +(ptcur(3)-ptb(3))**2)
      return
      end

      subroutine interpnorm(pt,coeff,pnorm)
      real*4 pt(3),pnorm(3),coeff(4,3)
      do i=1,3
c         pnorm(i)=coeff(1,i)+dotproduct(pt,coeff(2,i))
        pnorm(i)=coeff(1,i)+pt(1)*coeff(2,i)+pt(2)*coeff(3,i)+
     &      pt(3)*coeff(4,i)
      enddo
      return
      end

      function dotproduct(veca,vecb)
      real*4 veca(3),vecb(3)
      dotproduct=veca(1)*vecb(1)+veca(2)*vecb(2)+veca(3)*vecb(3)
      return
      end


      subroutine crossproduct(avec,bvec,cvec)
      real*4 avec(*),bvec(*),cvec(*)
      cvec(1)=avec(2)*bvec(3)-avec(3)*bvec(2)
      cvec(2)=avec(3)*bvec(1)-avec(1)*bvec(3)
      cvec(3)=avec(1)*bvec(2)-avec(2)*bvec(1)
      return
      end



      subroutine normalize(vect)
      real*4 vect(3)
      sum=0.
      do i=1,3
        sum=sum+vect(i)**2
      enddo
      fac=sqrt(sum)
      do i=1,3
        vect(i)=vect(i)/fac
      enddo
      return
      end

      subroutine copyvec(a,b)
      real*4 a(3),b(3)
      b(1)=a(1)
      b(2)=a(2)
      b(3)=a(3)
      return
      end

      subroutine clipcopy(clipplanes,nmodclip,iclip,nclip,clipuse,
     &    nuseclip,xyzlim,indlim,nlim)
      real*4 clipplanes(4,*),clipuse(4,*),xyzlim(6)
      integer*4 iclip(*),indlim(*)
      nuseclip=0
      do i=1,nclip
        if(iclip(i).eq.0.or.abs(iclip(i)).gt.nmodclip)then
          print *,'No clipping plane #',abs(iclip(i))
        else
          indclp=abs(iclip(i))
          nuseclip=nuseclip+1
          sign=1.
          if(iclip(i).lt.0)sign=-1.
          do j=1,4
            clipuse(j,nuseclip)=sign*clipplanes(j,indclp)
          enddo
        endif
      enddo
      nlim=0
      do i=1,3
        if(xyzlim(2*i).ne.0.or.xyzlim(2*i-1).ne.0)then
          nlim=nlim+1
          indlim(nlim)=i
        endif
      enddo
      return
      end


      logical function inregion(pt,xyzlim,nlim,indlim,clip,nclip)
      real*4 pt(3),clip(4,*),xyzlim(6)
      integer*4 indlim(3)
      inregion=.false.
      do ilim=1,nlim
        i=indlim(ilim)
        if(pt(i).lt.xyzlim(2*i-1).or.pt(i).gt.xyzlim(2*i))return
      enddo
      do i=1,nclip
        if(dotproduct(pt,clip(1,i))+clip(4,i).le.0)return
      enddo
      inregion=.true.
      return
      end


      subroutine getregtypes(ngraph,itypref,nreftyp,itypneigh,
     &    nneightyp, nregion)
      parameter (limtyp=50,itypall=999)
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
      integer*4 in5
      common /nmsinput/ in5
      do ii=1,ngraph
c         
c         copy last region's specifications
c         
        last=ngraph*(nregion-2)+ii
        next=last+ngraph
        nreftyp(next)=nreftyp(last)
        nneightyp(next)=nneightyp(last)
        do jj=1,nreftyp(next)
          itypref(jj,next)=itypref(jj,last)
        enddo
        do jj=1,nneightyp(next)
          itypneigh(jj,next)=itypneigh(jj,last)
        enddo   
        write(*,102)ii,'reference'
102     format(' For graph #',i3,', enter list of types for ',
     &      'points to be considered',/,5x,a,' points',
     &      ' (/ for same as last region, Return for all)')
        call rdlist(in5,itypref(1,next),nreftyp(next))
        if(nreftyp(next).eq.0)then
          nreftyp(next)=1
          itypref(1,next)=itypall
        endif
c         
        write(*,102)ii,'neighboring'
        call rdlist(in5,itypneigh(1,next),nneightyp(next))
        if(nneightyp(next).eq.0)then
          nneightyp(next)=1
          itypneigh(1,next)=itypall
        endif
c         
      enddo
      return
      end




c       SDINTEGRATE compute the integral of the items in some bins of a
c       graph, subtracting a baseline value as well.
c       GRAPHS has the values of the graph
c       AREAS has the areas for the bins
c       IFANGDIFF is 0 for radial graphs and 1 for annular graphs
c       DELR is bin width (distance or degrees)
c       NBINS is number of bins
c       RMIN and RMAX are inner and outer radii for an annular graph
c       INTST, INTND are starting and ending bins to integrate
c       IBAST, IBAND are starting and ending bins to compute the baseline
c       value from, or 0,0 to use thevalue supplied in BASELINE
c       SUM is returned with the integral
c       
      subroutine sdintegrate(graphs,areas,ifangdiff,delr,rmin,rmax,
     &    nbins, intst,intnd, ibast,iband, baseval,sum)
      real*4 graphs(*),areas(*)
      baseline=baseval
      if(ibast.le.iband.and.(ibast.ne.0.or.iband.ne.0))then
        baseline=0.
        do ibin=ibast,iband
          baseline=baseline+graphs(ibin)
        enddo
        baseline=baseline/(iband+1-ibast)
      endif
      sum=0.
      do ibin=intst,intnd
        anularea=3.14159*delr**2*(2*ibin-1)
        if(areas(nbins+1).gt.0.and.ifangdiff.eq.0)
     &      anularea=areas(ibin)/areas(nbins+1)
        sum=sum+anularea*(graphs(ibin)-baseline)
      enddo
      return
      end




c       RANDOM_POINTS returns the coordinates of NPNTS randomly located
c       points in the arrays SX and SY.  They are located inside the boundary
c       defined by the NVERT points whose coordinates are passed in BX and BY
c       If BOUNDSEP is non-zero, they are no closer than that value to the
c       boundary.  If NRESTRICT is non-zero, then PROBNEAR and DELNEAR should
c       specify restrictions on how close adjacent points may get to each
c       other.  NRESTRICT should specify the number of values in PROBNEAR,
c       DELNEAR is the range of distance for each such value (effectively a
c       bin width), and each value in PROBNEAR gives the probability of
c       accepting a point that is within that particular range of distances
c       from another point.
c       
      subroutine random_points(bx,by,nvert,probnear,delnear,nrestrict,
     &    boundsep,npnts,itype,itypshft,ntypshft,sx,sy)
c       
      real bx(*),by(*)                          !boundary vertices
      real*4 sx(*),sy(*)                        !sample points
      real*4 probnear(*)                        !rejection probability
      integer*4 itype(*),itypshft(*)
      parameter (limvert=500,itypall=999,limpnts=50000)
      real*4 edgesq(limvert)
      logical inside,stillok
      character*8 jtime
      logical*1 needshft(limpnts)
      data iffirst/1/
      save iffirst,iseed
c       
      if (iffirst.ne.0)then
        call time(jtime)
        iseed=2*((ichar(jtime(8:8))+128*(ichar(jtime(7:7))+
     &      128*(ichar(jtime(5:5))+128*ichar(jtime(4:4)))))/2)+1
        iffirst=0
      endif
c       
c       compute squares of edge lengths and min and max x and y
c       
      bsepsq=boundsep**2
      radmax=delnear*nrestrict
      radmaxsq=radmax**2
      xmin=bx(1)
      ymin=by(1)
      xmax=bx(1)
      ymax=by(1)
      do iv=1,nvert
        xmin=min(xmin,bx(iv))
        ymin=min(ymin,by(iv))
        xmax=max(xmax,bx(iv))
        ymax=max(ymax,by(iv))
        edgesq(iv)=(bx(iv+1)-bx(iv))**2+(by(iv+1)-by(iv))**2
      enddo
c       
c       find out which points need shifting
c       
      do ipnt=1,npnts
        needshft(ipnt)=.false.
        do ity=1,ntypshft
          if(itype(ipnt).eq.itypshft(ity).or.itypshft(ity).eq.itypall)
     &        needshft(ipnt)=.true.
        enddo
      enddo
c       
c       sample npnts points
c       
10    ntry=0
      noutside=0
      nnearbound=0
      ipnt=0
      nthistry=0
      do while (ipnt.lt.npnts)
        if(needshft(ipnt+1))then
c           
c           get point in rectangle enclosing boundary
c           
          x0=ran(iseed)
          x0=ran(iseed)
          x0=ran(iseed)
          x0=ran(iseed)
          x0=xmin+(xmax-xmin)*ran(iseed)
          y0=ran(iseed)
          y0=ran(iseed)
          y0=ran(iseed)
          y0=ran(iseed)
          y0=ymin+(ymax-ymin)*ran(iseed)
c           
c           reject if not inside boundary
c           
          stillok=inside(bx,by,nvert,x0,y0)
          if(.not.stillok)noutside=noutside+1
          if(boundsep.gt.0.)then
            ivert=1
            d1sq=(x0-bx(1))**2+(y0-by(1))**2
            distminsq=d1sq
c             
c             if boundsep is non-zero, find minimum distance from boundary
c             
            do while (stillok.and.ivert.le.nvert)
              dbsq=edgesq(ivert)
              d2sq=(x0-bx(ivert+1))**2+(y0-by(ivert+1))**2
              if(d2sq.gt.d1sq+dbsq)then
                distsq=d1sq                     !if (x1,y1) is closest
              elseif(d1sq.gt.d2sq+dbsq)then
                distsq=d2sq                     !if (x2,y2) is closest
              else                              !if somewhere between is closer
                distsq=d2sq-(d2sq+dbsq-d1sq)**2/(4.*dbsq)
              endif
              distminsq=min(distminsq,distsq)
              stillok=bsepsq.le.distminsq
              d1sq=d2sq
              ivert=ivert+1
            enddo
          endif
          if(.not.stillok)nnearbound=nnearbound+1
c           
c           now if any distance restrictions, need to check for near
c           neighbors among that points that don't still need shifting
c           
          if(nrestrict.gt.0)then
            ichk=1
            do while (stillok .and. ichk.le.npnts)
              if(.not.needshft(ichk))then
                delx=abs(x0-sx(ichk))
                if(delx.lt.radmax)then
                  dely=abs(y0-sy(ichk))
                  if(dely.lt.radmax)then
                    rsq=delx**2+dely**2
                    if(rsq.lt.radmaxsq)then
c                       
c                       accept point if random number is less than
c                       probability for the given distance
c                       
                      ibin=sqrt(rsq)/delnear + 1
                      stillok=probnear(ibin).gt.0.
                      if(stillok)stillok=ran(iseed).lt.probnear(ibin)
                    endif
                  endif
                endif
              endif
              ichk=ichk+1
            enddo
          endif
c           
c           if point is still ok, accept it
c           
          if(stillok)then
            ipnt=ipnt+1
            sx(ipnt)=x0
            sy(ipnt)=y0
            needshft(ipnt)=.false.
            nthistry=0
          endif
          ntry=ntry+1
          nthistry=nthistry+1
          if(nthistry.gt.1000)then
            print *,'Couldn''t place point',ipnt+1,' of',npnts,
     &          '; Starting over'
            go to 10
          endif
        else
          ipnt=ipnt+1
        endif
      enddo
      nnearbound=nnearbound-noutside
      nnearpnt=ntry-noutside-nnearbound-npnts
c       print *,ntry,' points sampled',noutside,' outside',nnearbound,
c       &           ' too near boundary',nnearpnt,' too near point',npnts,' ok'
      return
      end

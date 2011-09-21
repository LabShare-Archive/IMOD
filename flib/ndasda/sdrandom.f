


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
      subroutine random_pores(probnear,delnear,nrestrict,
     &    itypshft,ntypshft,manyrandom)
c       
      real*4 probnear(*)                        !rejection probability
      integer*4 itypshft(*)
      parameter (itypall=999,maxind=10000)
      include 'sda.inc'
      logical stillok
      character*8 jtime
      logical*1 needshft(maxpore)
      real*4 cumarea(maxtri)
      integer*4 indcum(0:maxind)
      real*4 vec2(3),vec3(3),vecsum(3),cvec(3)
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
c       build cumulative distribution of areas and partial cross-index
c       
      cum=0.
      do i=1,ntriang
        cum=cum+triarea(i)
        cumarea(i)=cum
      enddo
      indcur=0
      cumcur=0.
      do i=1,ntriang
        cumarea(i)=cumarea(i)/cum
        do while(cumcur.le.cumarea(i).and.indcur.le.maxind)
          indcum(indcur)=i
          indcur=indcur+1
          cumcur=float(indcur)/maxind
        enddo
      enddo
      indcum(maxind)=ntriang
c       
      radmax=delnear*nrestrict
      radmaxsq=radmax**2
c       
c       find out which points need shifting
c       
      do ipnt=1,npore
        needshft(ipnt)=.false.
        do ity=1,ntypshft
          if(itypore(ipnt).eq.itypshft(ity).or.itypshft(ity).eq.itypall)
     &        needshft(ipnt)=.true.
        enddo
      enddo
c       
c       sample npore points
c       
10    ntry=0
      noutside=0
      nnearbound=0
      ipnt=0
      nthistry=0
      do while (ipnt.lt.npore)
        if(needshft(ipnt+1))then
c           
c           get triangle based on cumulative area distribution: get first
c           triangle with cumulative area >/= random number
c           
          if(manyrandom.eq.0)then
            write(*,'(a,i4,$)')char(13)//'Placing pore #',ipnt
            call flush(6)
          endif
          x0=ran(iseed)
          x0=ran(iseed)
          x0=ran(iseed)
          x0=ran(iseed)
          rancum=ran(iseed)
          indran=rancum*indmax
          itri=indcum(indran)
          do while(itri.lt.ntriang.and.cumarea(itri).lt.rancum)
            itri=itri+1
          enddo
c           
c           get vector that is random sum of two sides of triangle
c           and has tip inside triangle, based on whether sum of areas of
c           the triangles formed with the two sides is </= triangle area
c           
          ifinside=0
          i1=indvert(1,itri)
          i2=indvert(2,itri)
          i3=indvert(3,itri)
          do while(ifinside.eq.0)
            f2=ran(iseed)
            f3=ran(iseed)
            do i=1,3
              vec2(i)=verts(i,i2)-verts(i,i1)
              vec3(i)=verts(i,i3)-verts(i,i1)
              vecsum(i)=f2*vec2(i)+f3*vec3(i)
            enddo
            call crossproduct(vecsum,vec2,cvec)
            area2=sqrt(cvec(1)**2+cvec(2)**2+cvec(3)**2)/2.
            call crossproduct(vecsum,vec3,cvec)
            area3=sqrt(cvec(1)**2+cvec(2)**2+cvec(3)**2)/2.
            if(area2+area3.le.triarea(itri))ifinside=1
          enddo
c           
c           add to first vertex to get coordinates in triangle
c           
          do i=1,3
            cvec(i)=vecsum(i)+verts(i,i1)
          enddo
c           
c           now if any distance restrictions, need to check for near
c           neighbors among that points that don't still need shifting
c           
          stillok=.true.
          if(nrestrict.gt.0)then
            ichk=1
            do while (stillok .and. ichk.le.npore)
              if(.not.needshft(ichk))then
                delx=abs(cvec(1)-pore(1,ichk))
                if(delx.lt.radmax)then
                  dely=abs(cvec(2)-pore(2,ichk))
                  if(dely.lt.radmax)then
                    delz=abs(cvec(3)-pore(3,ichk))
                    if(delz.lt.radmax)then
                      rsq=delx**2+dely**2+delz**2
                      if(rsq.lt.radmaxsq)then
                        call sddist(cvec,itri,pore(1,ichk),
     &                      itripore(ichk),dist)
                        if(dist.ge.0..and.dist.lt.radmax)then
c                           
c                           accept point if random number is less than
c                           probability for the given distance
c                           
                          ibin=dist/delnear + 1
                          stillok=probnear(ibin).gt.0.
                          if(stillok)stillok=
     &                        ran(iseed).lt.probnear(ibin)
                        endif
                      endif
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
            call copyvec(cvec,pore(1,ipnt))
            itripore(ipnt)=itri
            needshft(ipnt)=.false.
            nthistry=0
          endif
          ntry=ntry+1
          nthistry=nthistry+1
          if(nthistry.gt.100)then
            print *,'Couldn''t place point',ipnt+1,' of',npnts,
     &          '; Starting over'
            go to 10
          endif
        else
          ipnt=ipnt+1
        endif
      enddo
      if(manyrandom.eq.0)write(*,*)
      return
      end

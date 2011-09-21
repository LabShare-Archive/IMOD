      subroutine find_points_area(itypcrosind,ntypes,clip,
     &    nclip, xyzlim,indlim,nlim,ndiv,poreinreg,vertinreg,npnts,
     &    ninclass,areainreg)
      include 'sda.inc'
      integer*4 itypcrosind(*),ninclass(*),indlim(*),indsub(3,800)
      real*4 clip(4,*),xyzlim(*),subvert(3,800),centroid(3)
      logical*1 vertinreg(*),poreinreg(*),subinreg(800)
      parameter (limtyp=50,itypall=999)
      logical inregion
      real*8 areatmp
c       
c       see if each pore is in region, build type list
c       
      do ii=1,limtyp
        ninclass(ii)=0
      enddo
      npnts=0
      do i=1,npore
        itype(i)=itypore(i)
c         do j=1,nclip
c         print *,i,itype(i),dotproduct(pore(1,i),clip(1,j))+clip(4,j)
c         enddo
        poreinreg(i)=inregion(pore(1,i),xyzlim,nlim,indlim,clip,nclip)
        if(poreinreg(i))then
          npnts=npnts+1
          if(itypcrosind(itype(i)).eq.0)then
            ntypes=ntypes+1
            itypcrosind(itype(i))=ntypes
          endif
          indlist=itypcrosind(itype(i))
          ninclass(indlist)=ninclass(indlist)+1
        endif
      enddo
c       
c       see if each vertex is in region
c       
      do i=1,nvert
        ind=listvert(i)
        vertinreg(ind)=inregion(verts(1,ind),xyzlim,nlim,indlim,clip,
     &      nclip)
      enddo
c       
c       go through each triangle, see how many vertices in region
c       
      areatmp=0
      do itri=1,ntriang
        nin=0
        do i=1,3
          if(vertinreg(indvert(i,itri)))nin=nin+1
        enddo
        if(nin.eq.3)then
          areatmp=areatmp+triarea(itri)
        elseif(nin.gt.0)then
c           
c           subdivide triangle and check each subtriangle
c           
          call subtriangle(verts(1,indvert(1,itri)),
     &        verts(1,indvert(2,itri)),verts(1,indvert(3,itri)),ndiv,
     &        subvert,indsub,nsubv,nsubtr)
          do i=1,nsubv
            subinreg(i)=inregion(subvert(1,i),xyzlim,nlim,indlim,
     &          clip, nclip)
          enddo
          subarea=triarea(itri)/nsubtr
          do isub=1,nsubtr
            nin=0
            do i=1,3
              if(subinreg(indsub(i,isub)))nin=nin+1
            enddo
            if(nin.eq.3)then
              areatmp=areatmp+subarea
            elseif(nin.gt.0)then
              do i=1,3
                sum=0.
                do j=1,3
                  sum=sum+subvert(i,indvert(j,isub))
                enddo
                centroid(i)=sum
              enddo
              if(inregion(centroid,xyzlim,nlim,indlim,clip,
     &            nclip))areatmp=areatmp+subarea
            endif
          enddo
        endif
      enddo
      areainreg=areatmp
      return
      end



      subroutine subtriangle(p1,p2,p3,ndiv,subvert,indsub,nsubv,
     &    nsubtr)
      real*4 subvert(3,*),p1(3),p2(3),p3(3)
      integer*4 indsub(3,*)
      nsubv=1
      nsubtr=0
      call copyvec(p1,subvert)
      indlast=1
      indthis=2
c       
c       go one row at a time
c       
      do jdiv=1,ndiv
        t=float(jdiv)/ndiv
        do i=1,3
          p1part=(1.-t)*p1(i)
          subvert(i,indthis)=p1part+t*p2(i)
          subvert(i,jdiv+indthis)=p1part+t*p3(i)
c           
c           subdivide the line on this row
c           
          do j=1,jdiv-1
            s=float(j)/ndiv
            subvert(i,j+indthis)=(1.-s)*subvert(i,indthis)+
     &          s*subvert(i,jdiv+indthis)
          enddo
        enddo
c         
c         set up triangles with vertex pointing toward point 1
c         
        do j=1,jdiv
          nsubtr=nsubtr+1
          indsub(1,nsubtr)=indlast+j-1
          indsub(2,nsubtr)=indthis+j-1
          indsub(3,nsubtr)=indthis+j
        enddo
c         
c         set up triangles with vertices pointing away from p1
c         
        do j=1,jdiv-1
          nsubtr=nsubtr+1
          indsub(1,nsubtr)=indlast+j-1
          indsub(2,nsubtr)=indthis+j
          indsub(3,nsubtr)=indlast+j
        enddo
        indlast=indthis
        indthis=indthis+jdiv+1
        nsubv=nsubv+jdiv+1
      enddo
      return
      end

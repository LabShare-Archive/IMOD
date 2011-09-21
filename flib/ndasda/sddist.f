      subroutine sddist(ptain,itrain,ptb,itrb,dist)
      include 'sda.inc'
      real*4 pta(3),ptb(3),anorm(3),bnorm(3),abvec(3),ptcur(3)
      real*4 aplane(3),bplane(3),path(3),ptcross(3,4),tsav(3)
      real*4 ptain(3),prod(3),traject(3,200)
      integer*4 iedgecross(4)
c       
      farcrit=1.e-4*trisize
      critlong=5.*volsize
      extranmin=20.
      extranmax=90.
      extranfac=10.
      ncirclim=20
      extratio=1.
c       
      ncircle=0
c       
c       come back here to try new plane if going in circles
c       
2     ifother=0
      delfac=(extratio*((ncircle+1)/2))/ncirclim
      if(mod(ncircle,2).eq.0)then
        afac=0.5-delfac
        bfac=0.5+delfac
      else
        afac=0.5+delfac
        bfac=0.5-delfac
      endif
c       
c       come back to here to restart other direction
c       
3     dist=0.
      call copyvec(ptain,pta)
      itra=itrain
      nreset=0
      ntraj=0
      ntottri=0
c       
c       come back to here to reset to a new A-starting point
c       
5     do i=1,3
        abvec(i)=ptb(i)-pta(i)
        ptcur(i)=pta(i)
      enddo
      abdot=dotproduct(abvec,abvec)
      toblast=sqrt(abdot)
      totlast=dist+toblast
c       
c       if A-B distance is small enough, just take it and return.
c       
      if(toblast.lt.1.e-2*trisize)then
        dist=dist+toblast
        return
      endif
c       
c       interpolate to find normals at the points
c       
      call interpnorm(pta,coeff(1,1,itra),anorm)
      call interpnorm(ptb,coeff(1,1,itrb),bnorm)
c       
c       
c       take cross product of each normal with vector from A to B
c       
      call crossproduct(anorm,abvec,aplane)
      call crossproduct(bnorm,abvec,bplane)
      if(nreset.eq.0)then
        angnorm=vectang(anorm,bnorm)
        angplane=vectang(aplane,bplane)
      endif
c       print *,'A-B normal and plane angles:',vectang(anorm,bnorm)
c       &           ,vectang(aplane,bplane)
c       
c       normalize and average, get parameters of path plane
c       
      call normalize(aplane)
      call normalize(bplane)
      dpath=0.
      do i=1,3
        path(i)=afac*aplane(i)+bfac*bplane(i)
        dpath=dpath+path(i)*pta(i)
      enddo
c       
c       set up to walk from one triangle to next
c       
      itrcur=itra
      iedgecur=0
c       print *,'going to',(ptb(i),i=1,3)
c       print *,itrcur,dist,(ptcur(i),i=1,3)
      nrelax=0
      relax=0.0
      do while(itrcur.ne.itrb)
        do i=1,3
          ind=indvert(i,itrcur)
          prod(i)=dotproduct(path,verts(1,ind))
        enddo
10      ncross=0
        do iedge=1,3
          t=999.
          if(iedge.ne.iedgecur)then
c             
c             look for crossing in edge other than current edge
c             
            iv2=mod(iedge,3)+1
            if(abs(prod(iedge)-prod(iv2)).gt.1.e-8)then
              ind1=indvert(iedge,itrcur)
              ind2=indvert(iv2,itrcur)
              t=(dpath-prod(iedge))/(prod(iv2)-prod(iedge))
c               
c               it's a crossing if parameter for line is between 0 and 1, so
c               save it
c               
              if(t.ge.-relax.and.t.le.1+relax)then
                ncross=ncross+1
                tc=max(0.,min(1.,t))
                do i=1,3
                  ptcross(i,ncross)=(1.-tc)*verts(i,ind1)+
     &                tc*verts(i,ind2)
                enddo
                iedgecross(ncross)=iedge
c                 
c                 but if more than one crossing, eliminate if close to another
c                 
                if(ncross.gt.1)then
                  ifclose=0
                  do j=1,ncross-1
                    iffar=0
                    do i=1,3
                      if(abs(ptcross(i,j)-ptcross(i,ncross)).gt.
     &                    farcrit) iffar=1
                    enddo
                    if(iffar.eq.0)ifclose=1
                  enddo
                  if(ifclose.eq.1)ncross=ncross-1
                endif
              endif
            endif
          endif
          tsav(iedge)=t
        enddo
c         
        if(ncross.eq.0.and.nrelax.le.6)then
          nrelax=nrelax+1
          relax=.0000001*(3**nrelax)
          go to 10
        endif
        if(ncross.eq.0)then
          print *, ncross,' crossings, triangle',itrcur,' position',
     &        (ptcur(i),i=1,3),(tsav(i),i=1,3)
          dist=-1.
          go to 30
        endif
c         
c         if 2-3 crossings, find the one that advances the dot product
c         with A-B vector the most
c         
        icross=1
        dstmax=howfar(ptcross,pta,abvec)
        do j=2,ncross
          dstmp=howfar(ptcross(1,j),pta,abvec)
          if(dstmax.lt.dstmp)then
            icross=j
            dstmax=dstmp
          endif
        enddo
        if(ifother.eq.1.and.ncross.gt.1)then
          ioldcross=icross
          icross=1
          dstmax=howfar(ptcross,pta,abvec)
          do j=2,ncross
            dstmp=howfar(ptcross(1,j),pta,abvec)
            if(dstmax.gt.dstmp)then
              icross=j
              dstmax=dstmp
            endif
          enddo
          if(icross.eq.ioldcross)then
            if(ncross.eq.2)icross=3-ioldcross
            if(ncross.eq.3)icross=mod(icross,3)+1
          endif
          ifother=2
        endif
        if(ifother.eq.1.and.ntraj.gt.20)then
          print *,'Starting out same way, give up',dist
          dist=-1.
          go to 30
        endif

c         
c         Have we passed by B, as assessed by dot product of A to pt on A-B?
c         
        tob=dist3d(ptcross(1,icross),ptb)
        if(dstmax/abdot.gt.1..and.tob.gt.1.e-3*trisize.and.
     &      (tob.lt.trisize.or.toblast.lt.trisize))then
c           print *,'Progress value >1',tob,toblast,dist
          dist=totlast
          go to 30
        endif
c         
c         add in distance from current point to crossing, set up new
c         current point in edge of new triangle
c         BUT, if no next triangle, return distance of -1
c         
        dist=dist+dist3d(ptcur,ptcross(1,icross))
        itrnew=indedge(iedgecross(icross),itrcur)
        iedgecur=numedge(iedgecross(icross),itrcur)
        call copyvec(ptcross(1,icross),ptcur)
        ntraj=ntraj+1
        if(ntraj.lt.100)call copyvec(ptcur,traject(1,ntraj))
        itrcur=itrnew
        toblast=tob
        totlast=dist+tob
        if(tob.lt.1.e-3*trisize)then
          dist=totlast
          go to 30
        endif
c         
        if(itrcur.le.0)then
          dist=-1.
          go to 30
        endif
        if(ntraj.gt.10)then
          if((dist.gt.5*trisize.and.traject(1,2).eq.ptcur(1).and.
     &        traject(2,2).eq.ptcur(2).and.
     &        traject(3,2).eq.ptcur(3)).or.
     &        dist.le.1.e-4*trisize.and.itrcur.eq.itra)then
            dist=-4.
            if(ifother.ne.0)go to 30
            ncircle=ncircle+1
            extrange=max(angplane,min(extranmax,max(extranmin,
     &          extranfac*angplane)))
            extratio=extrange/angplane
            if(ncircle.le.ncirclim)go to 2
c             print *,'Going in CIRCLES at all planes',angnorm,angplane
c             write(*,107)dist,(ptain(i),i=1,3),(ptb(i),i=1,3)
            return
          endif
        endif
c         
c         if had to relax to get crossings, reset point A to this point
c         and restart the travel
c         
        if(nrelax.gt.0)then
          itra=itrcur
          call copyvec(ptcur,pta)
          nreset=nreset+1
          go to 5
        endif
c         write(*,101)itrcur,iedgecross(icross),dist,(ptcur(i),i=1,3),tsav
101     format(2i5,5f8.3)
c         
c         if this is other way and already longer than first way, quit
c         
        if(ifother.gt.0.and.distfirst.ge.0..and.dist.gt.distfirst)
     &      go to 30
c         
        if(dist.gt.critlong)then
c           print *,'LONG distance'
c$$$        print *,'Passing by, far away', tob,toblast,dist
c$$$        print *,ptain,' starting'
c$$$        print *,ptb,' ending'
c$$$        do i=1,ntraj
c$$$        print *,(traject(j,i),j=1,3)
c$$$        enddo
c$$$        print *,nreset,' resets'
c$$$        print *,'A internormals:',
c$$$        &       vectang(verts(1,indnorm(1,itra)),verts(1,indnorm(2,itra))),
c$$$        &         vectang(verts(1,indnorm(2,itra)),verts(1,indnorm(3,itra))),
c$$$        &         vectang(verts(1,indnorm(3,itra)),verts(1,indnorm(1,itra)))
c$$$        print *,'A interp:',
c$$$        &       vectang(anorm,verts(1,indnorm(1,itra))),
c$$$        &       vectang(anorm,verts(1,indnorm(2,itra))),
c$$$        &       vectang(anorm,verts(1,indnorm(3,itra)))
c$$$        print *,'B internormals:',
c$$$        &       vectang(verts(1,indnorm(1,itrb)),verts(1,indnorm(2,itrb))),
c$$$        &         vectang(verts(1,indnorm(2,itrb)),verts(1,indnorm(3,itrb))),
c$$$        &         vectang(verts(1,indnorm(3,itrb)),verts(1,indnorm(1,itrb)))
c$$$        print *,'B interp:',
c$$$        &       vectang(bnorm,verts(1,indnorm(1,itrb))),
c$$$        &       vectang(bnorm,verts(1,indnorm(2,itrb))),
c$$$        &       vectang(bnorm,verts(1,indnorm(3,itrb)))
c$$$        print *,'A-B normal and plane angles:',vectang(anorm,bnorm)
c$$$        &       ,vectang(aplane,bplane)
c$$$        call mypause('HA')
          dist=-2.
          go to 30
        endif
        if(ntottri.gt.ntriang)then
c           print *,'WAY TOO MANY steps'
          dist=-3.
          go to 30
        endif
        relax=0.0
        nrelax=0
        ntottri=ntottri+1
      enddo
c       
c       leave loop when got to triangle B - finish distance
c       
      dist=dist+dist3d(ptcur,ptb)
c       print *,'Path dist=',dist
c       
c       here when got a distance. If angle between normals is <60, don't try
c       other direction.
c       
30    if(angnorm.gt.60.)then
c         
c         otherwise, go in other direction first time, save this distance
c         
        if(ifother.eq.0)then
          distfirst=dist
          ifother=1
          go to 3
        else
c           print *,'Two distances:', distfirst,dist
c           
c           Got two distances.
c           If first distance no good, just take current one
c           
          if(distfirst.ge.0.)then
c             
c             if new distance no good, take first; otherwise take the
c             minimum
c             
            if(dist.lt.0.)then
              dist=distfirst
            else
              dist=min(dist,distfirst)
            endif
          endif
        endif
      endif
      if(dist.eq.-2.)print *,'LONG distance'
      if(dist.eq.-3.)print *,'WAY TOO MANY steps'
c       if(ncircle.gt.0)print *,'Circled until plane #',ncircle
c       if(dist.lt.0)write(*,107)dist,(ptain(i),i=1,3),(ptb(i),i=1,3)
c       107     format(f7.1,6f12.3)
      return
      end

      function vectang(a,b)
      real*4 a(3),b(3)
      vectang=acosd(min(1.,dotproduct(a,b)/
     &    sqrt(dotproduct(a,a)*dotproduct(b,b))))
      return
      end

      function howfar(pt,pta,abvec)
      real*4 pt(3),pta(3),abvec(3)
      howfar=0.
      do i=1,3
        howfar=howfar+(pt(i)-pta(i))*abvec(i)
      enddo
      return
      end

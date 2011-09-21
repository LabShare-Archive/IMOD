      subroutine kerneldens(h,poreinreg,vertinreg,itypkern,ntypkern
     &    ,kernret,rangekern)
      include 'sda.inc'
      integer*4 itypkern(*)
      real*4 sumvert(maxverts),sumord(maxverts)
      character*60 filename
      logical*1 poreinreg(*),vertinreg(*),poreonlist(maxpore)
      integer*4 in5
      common /nmsinput/ in5
c       
c       go by triangle, looking at each vertex
c       
      do iv=1,nvert
        ind=listvert(iv)
        sumvert(ind)=-1.
      enddo
      hsq=h**2
      summax=0.
      summin=1.e10
      sumsum=0.
      sumsq=0.
      nsum=0
      do ipore=1,npore
        poreonlist(ipore)=ntypkern.eq.0
        do i=1,ntypkern
          if(itype(ipore).eq.itypkern(i))poreonlist(ipore)=.true.
        enddo
      enddo
c       
      ncircle=0
      if(kernret.eq.0)write(*,'(a,i6,a)')
     &    'Out of',ntriang,' triangles, working on #:'
      do itri=1,ntriang
        if(mod(itri,10).eq.0.and.kernret.eq.0)then
          write(*,'(a,i6,$)')char(13),itri
          call flush(6)
        endif
        do ivert=1,3
          ind=indvert(ivert,itri)
          if(sumvert(ind).lt.0.)then
            sum=0.
            if(vertinreg(ind))then
c               
c               for each pore, find distance if < h, and add kernel to sum
c               
              do ipore=1,npore
                if(poreinreg(ipore).and.poreonlist(ipore))then
                  dx=verts(1,ind)-pore(1,ipore)
                  if(abs(dx).le.h)then
                    dy=verts(2,ind)-pore(2,ipore)
                    if(abs(dy).le.h)then
                      dz=verts(3,ind)-pore(3,ipore)
                      if(abs(dz).le.h)then
                        if(dx**2+dy**2+dz**2.le.hsq)then
                          call sddist(
     &                        pore(1,ipore),itripore(ipore),
     &                        verts(1,ind),itri,dist)
                          if(dist.ge.0..and.dist.lt.h)
     &                        sum=sum+(1.-dist**2/hsq)
                          if(dist.eq.-4.)ncircle=ncircle+1
                        endif
                      endif
                    endif
                  endif
                endif
              enddo
            endif
            if(sum.gt.0.)then
              summin=min(summin,sum)
              nsum=nsum+1
              sumsum=sumsum+sum
              sumsq=sumsq+sum**2
              sumord(nsum)=sum
            endif
            sum=max(0.001,sum)
            sumvert(ind)=sum
            summax=max(sum,summax)
          endif
        enddo
      enddo
      if(ncircle.gt.0)write(*,104)ncircle
104   format('  WENT IN CIRCLES',i4,' times')
      call sums_to_avgsd(sumsum,sumsq,nsum,avgden,sdden)
      do i=1,nsum-1
        do j=i+1,nsum
          if(sumord(i).gt.sumord(j))then
            sumtmp=sumord(i)
            sumord(i)=sumord(j)
            sumord(j)=sumtmp
          endif
        enddo
      enddo
      p025=pctile(sumord,nsum,0.025)
      p05=pctile(sumord,nsum,0.05)
      p10=pctile(sumord,nsum,0.1)
      p90=pctile(sumord,nsum,0.9)
      p95=pctile(sumord,nsum,0.95)
      p975=pctile(sumord,nsum,0.975)
c       write(*,106)p05,p10,p90,p95
c       106     format(' 5, 10, 90, 95 %iles:',4f8.3)
      if(kernret.eq.0)write(*,*)
      write(*,107)summax-summin,p975-p025,p95-p05,p90-p10,sdden
107   format(5f8.3,' ranges-sd')
      do itri=1,ntriang
        do ivert=1,3
          ind=indvert(ivert,itri)
          if(sumvert(ind).ge.0.)then
c             
c             copy and scale the normal, make it at most 1.
c             
            indn=indnorm(ivert,itri)
            do i=1,3
              vertout(i,indn)=sumvert(ind)*verts(i,indn)/summax
            enddo 
c             if(abs(nint(vertout(3,ind))-vertout(3,ind)).gt.0.01)print *,
c             &           vertout(3,ind),(vertout(i,indn),i=1,3)
            sumvert(ind)=-1.
          endif
        enddo
      enddo
c       print *,'normals scaled'
c       
      call putimodmesh(vertout)
c       
      if(kernret.eq.0)then
        write(*,105)summin,summax,summax-summin,avgden,sdden
105     format(/,' Min =',f8.3,', max =',f8.3,', range =',f8.3,
     &      ', mean =',f8.3,', sd =',f8.3)

        write(*,'(1x,a,$)')'Output model file name (Return for none): '
        read(in5,'(a)')filename
        if(filename.ne.' ')call writeimod(filename)
      else
        rangekern=sdden
        if(kernret.eq.1)rangekern=summax-summin
        if(kernret.eq.2)rangekern=p975-p025
        if(kernret.eq.3)rangekern=p95-p05
        if(kernret.eq.4)rangekern=p90-p10
      endif
      return
      end


      function pctile(x,n,p)
      real*4 x(*)
      v=n*p+0.5
      v=max(1.,min(float(n),v))
      iv=min(v,n-1.)
      f=v-iv
      pctile=(1.-f)*x(iv)+f*x(iv+1)
c       write(*,'(f6.0,f9.3)')100.*p,pctile
      return
      end

      subroutine sdlength()
      include 'sda.inc'
      real*4 cen(3,maxtri)
c       
      do itri=1,ntriang
        do ixyz=1,3
          sum=0.
          do i=1,3
            sum=sum+verts(ixyz,indvert(i,itri))
          enddo
          cen(ixyz,itri)=sum/3.
        enddo
      enddo
c       
      xsqmax=0.
      crit=0.
      do itri=1,ntriang-1
        do jtri=itri+1,ntriang
          dx=cen(1,itri)-cen(1,jtri)
          dy=cen(2,itri)-cen(2,jtri)
          dz=cen(3,itri)-cen(3,jtri)
          if(abs(dx).ge.crit.or.abs(dy).ge.crit.or.abs(dz).ge.crit)then
            dsq=dx**2+dy**2+dz**2
            if(dsq.gt.xsqmax)then
              xsqmax=dsq
              crit=sqrt(dsq/3.)
            endif
          endif
        enddo
      enddo
      extent=sqrt(xsqmax)
      print *,'Maximum 3-D extent of volume =',extent
      return
      end

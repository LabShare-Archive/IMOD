      subroutine grupnt(xx,yy,np,avgx,avgy,sdy,nn,ng)
      dimension xx(*),avgx(*),sdy(*),yy(*),nn(*),avgy(*)
      integer*2 ind(10000)
c       given a list of np points (xx,yy), it asks how many contiguous
c       non-overlapping groups (ng) to divide into, and number of points in
c       each group (nn), orders the points by x value, finds the mean x value
c       (avgx), and the mean and sd y value (avgy and sdy)
      do 5 i=1,np
        ind(i)=i
5     continue
c       build an index to order the points by xx
      do 8 i=1,np-1
        do 6 j=i+1,np
          if(xx(ind(i)).gt.xx(ind(j)))then
            itmp=ind(i)
            ind(i)=ind(j)
            ind(j)=itmp
          endif
6       continue
8     continue
      write(*,*) np,' points'
      write(*,'('' number of groups: '',$)')
      read(*,*)ng
      npgrp=np/max(1,ng)
      nextra=np-ng*npgrp
      do 9 ig=1,ng
        nn(ig)=npgrp
        if(ig.le.nextra)nn(ig)=npgrp+1
9     continue
      write(*,'('' # of points in each group (/ for equal #s): '',$)')
      read(*,*)(nn(i),i=1,ng)
      ii=1
      do 20 ig=1,ng
        sx=0.
        sy=0.
        sysq=0.
        n=nn(ig)
        do 10 in=1,n
          i=ind(ii)
          ii=ii+1
          sx=sx+xx(i)
          sy=sy+yy(i)
          sysq=sysq+yy(i)**2
10      continue
        avgx(ig)=sx/n
        avgy(ig)=sy/n
        sdy(ig)=0.
        if(n.gt.1)sdy(ig)=sqrt((sysq-n*avgy(ig)**2)/(n-1.))
        write(*,101)avgx(ig),avgy(ig),sdy(ig),nn(ig)
101     format(3f10.4,i5)
20    continue
      return
      end

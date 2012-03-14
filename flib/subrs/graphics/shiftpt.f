c       SHIFTPT will adjust the coordinates of successive points in a plot
c       so that they will be separated by a minimum diostance from previously
c       plotted points.  It is initialized by calling with a separation of
c       0.; on successive calls thereafter it builds a list of the
c       coordinates of points that have been plotted.  The arguments x,y are
c       modified by the subroutine.  The returned coordinates are constrained
c       to be within the limits of xlo to xhi and ylo to yhi.
c       
      subroutine shiftpt(x,y,sep,xlo,xhi,ylo,yhi)
      parameter (limpts=500)
      real*4 xst(limpts),yst(limpts)
      save nlist,xst,yst
      if(sep.eq.0.)then
        nlist=0
        return
      endif
      sepsq=sep**2
      do 30 irad=0,4
        nang=max(1,4*irad)
        do 28 iang=1,nang
          theta=(iang-1)*2*3.14159/nang
          xx=x+0.5*irad*sep*cos(theta)
          yy=y+0.5*irad*sep*sin(theta)
          if(xx.lt.xlo.or.xx.gt.xhi.or.yy.lt.ylo.or.yy.gt.yhi)go to 28
          do 20 list=1,nlist
            dx=abs(xst(list)-xx)
            if(dx.lt.sep)then
              dy=abs(yst(list)-yy)
              if(dy.lt.sep)then
                if(dx**2+dy**2.lt.sepsq)go to 28
              endif
            endif
20        continue
          go to 32
28      continue
30    continue
32    if(nlist.lt.limpts)then
        nlist=nlist+1
        xst(nlist)=xx
        yst(nlist)=yy
      endif
c       if(xx.ne.x.or.yy.ne.y)print *,x,y,xx,yy,xlo,xhi,ylo,yhi
      x=xx
      y=yy
      return
      end


      

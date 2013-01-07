c       FILTERPLOT - make graphs of filters based on 4 parameters
c       
c       $Id$
c       
      call plax_initialize('plotfilter')
      call exit(0)
      end
      
      subroutine realgraphicsmain()
      implicit none
      integer*4 ixwinsiz/1200/,iywinsiz/1000/
      integer*4 ntxtchr,itxtsiz,marglft,margbot,margrt,margtop,ixran,iyran
      integer*4 ixlo,iylo,ii,ix,iy,nsize2,i
      real*4 xmax,ymax,xval,yval,xscal,yscal,xx,delta
      real*4 sigma1,sigma2,radius1,radius2
      character*10 label
      real*4 ctf(8193)
c       
c       parameters controlling numerical labels
c       
      ntxtchr=3
      itxtsiz=8
      marglft=60
      margbot=30
      margrt=30
      margtop=10
      xmax = 0.5
      ymax = 1.0
      ixran=ixwinsiz-20-marglft-margrt
      iyran=iywinsiz-20-margbot-margtop

      print *,'This program plots attenuation versus spatial frequency'//
     &    ' in reciprocal pixels'
      call scrnOpen(0)
10    write(*,'(1x,a,$)')'Sigma1, sigma2, radius1, radius2: '
      read(5,*)sigma1, sigma2, radius1, radius2
      call setctfnoscl(sigma1,sigma2,radius1,radius2,ctf,1000,1000, delta,
     &    nsize2)
      if (sigma1 .eq. 0. .and. sigma2 .eq. 0. .and. radius1 .eq. 0. .and.
     &    radius2 .eq. 0.) call exit(0)

c       
      ixlo=20
      iylo=10
      call scrnErase(-1)
      call plax_mapcolor(251,0,255,0)
      call plax_box(0,ixlo,iylo,ixlo+ixwinsiz-1,iylo+iywinsiz-1)
c         
c       output x then y axis labels
c       
      do ii=0,10,2
        xval=ii*0.05
        write(label,fmt=20,err=30)xval
20      format(f3.1)
30      ix=ixlo+max(0,ii*ixran/10+10+marglft- ifix(itxtsiz*(ntxtchr-.3)))
        call plax_sctext(1,itxtsiz,itxtsiz,251,ix,iylo, label(1:ntxtchr))
      enddo
c         
      do ii=0,10,2
        yval=ii*0.1
        write(label,fmt=20,err=40)yval
40      iy=iylo+3+margbot+ii*iyran/10
        call plax_sctext(1,itxtsiz,itxtsiz,251,ixlo,iy, label(1:ntxtchr))
      enddo
c       
c       set scaling and output grids
c       
      xscal=ixran/xmax
      yscal=iyran/ymax
      ixlo=ixlo+10+marglft
      iylo=iylo+10+margbot
      call scrnGridLine(ixlo,iylo,ixran/10,0,10)
      call scrnGridLine(ixlo,iylo,0,iyran/10,10)
c      call dsgrd(ixlo,iylo+iyran,ixran/10,0,10)
c      call dsgrd(ixlo+ixran,iylo,0,iyran/10,10)
c       
c       draw lines
c       
      do i=1,nint(xmax / delta) + 1
        xx = (i - 0.5) * delta
        if (xx .lt. xmax) then
          ix = ixlo + xscal * xx
          iy = iylo + yscal * ctf(i)
          if(i.eq.1)then 
            call scrnMoveAbs(ix,iy)
          else
            call scrnVectAbs(ix,iy)
          endif
        endif
      enddo
      call scrnUpdate(1)
      go to 10
      end

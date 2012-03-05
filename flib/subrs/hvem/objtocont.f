c       OBJTOCONT converts an internal or WIMP-style object number to an
c       IMOD object and contour number

      subroutine objtocont(iobj,obj_color,imodobj,imodcont)
      integer*4 obj_color(2,*)
      icolor=obj_color(2,iobj)
      imodobj=256-icolor
      imodcont=0
      do i=1,iobj
        if(icolor.eq.obj_color(2,i))imodcont=imodcont+1
      enddo
      return
      end

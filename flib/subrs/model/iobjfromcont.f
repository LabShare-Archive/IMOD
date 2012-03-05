c       IOBJFROMCONT will convert IMOD object and contour numbers into an
c       absolute object number for use with the WIMP model format in Fortran
c       code.  IMODOBJ and IMODCONT are the object and contour numbers.  If
c       IMODCONT is 0, the function returns IMODOBJ.  If the contour does not
c       exist, the function returns 0.

      function iobjfromcont(imodobj,imodcont)
      include 'model.inc'
      iobjfromcont=0
      if(imodcont.eq.0)then
        iobjfromcont=imodobj
      else
        ntimes=0
        icol=256-imodobj
        do i=1,max_mod_obj
          if(obj_color(2,i).eq.icol)ntimes=ntimes+1
          if(ntimes.eq.imodcont)then
            iobjfromcont=i
            return
          endif
        enddo
      endif
      return
      end

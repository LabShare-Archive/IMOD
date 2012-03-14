c       WRLIST writes out the list of NLISTZ values in array LISTZ as a
c       series of ranges separated by commas
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
      subroutine wrlist(listz,nlistz)
      implicit none
      integer*4 listz(*),nlistz
      character*82 line
      integer*4 index, il, numstart, numend

      numstart = listz(1)
      line = ""
      index = 1
      do il = 2, nlistz
        if (listz(il) .ne. listz(il-1) + 1) then
          numend = listz(il-1)
          call add_range_to_line(numstart, numend, line, index)
          numstart = listz(il)
        endif
      enddo
      numend = listz(nlistz)
      call add_range_to_line(numstart, numend, line, index)
      write(*,'(a)') line(1 : index - 1)
      return
      end  


      subroutine add_range_to_line(numstart, numend, line, index)
      implicit none
      integer*4 numstart, numend, index
      character*(*) line
      integer*4 ncharrange, ncharend
      character*24 rangestring
      character*12 endstring
c       
c       convert the starting number, then add the ending number if it is
c       truly a range
c       
      call int_iwrite(rangestring, numstart, ncharrange)
      if (numend .gt. numstart) then
        call int_iwrite(endstring, numend, ncharend)
        rangestring(ncharrange+1 : ncharrange+1) = '-'
        rangestring(ncharrange+2 : ncharrange+1+ncharend) =
     &      endstring(1 : ncharend)
        ncharrange = ncharrange + 1 + ncharend
      endif
c       
c       dump line and start new one if this range will not fit
c       
      if (index + ncharrange .gt. 80) then
        write(*,'(a)') line(1 : index - 1)
        line = ""
        index = 1
      endif
c       
c       add , unless at start of line, then add range
c       
      if (index .gt. 1) then
        line(index : index) = ','
        index = index + 1
      endif
      line(index : index + ncharrange - 1) = rangestring(1 : ncharrange)
      index = index + ncharrange
      return
      end

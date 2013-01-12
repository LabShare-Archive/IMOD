c       RDLIST reads a line from unit IUNit specifying ranges of values and
c       returns a list of all of the values (NLIST values in array LIST).
c       E.g. 1-5,7,9,11,15-20.  Numbers separated by dashed are replaced by
c       all of the numbers in the range.  NUmbers need not be in any order,
c       and backward ranges (10-5) are handled.  Any characters besides
c       digits are valid separators. A / at the beginning of the line will
c       return an unmodified list. Negative numbers can be entered provided
c       that the minus sign immediately precedes the number.  E.g.: -3 - -1
c       or -3--1 will give -3,-2,-1; -3, -1,1 or -3,-1,1 will give -3,-1,1.
c       
c       $Id$
c       
c       
      subroutine rdlist(iunit,list,nlist)
      integer*4 list(*),iunit,nlist
      call rdlist2(iunit,list,nlist,0)
      return
      end

      subroutine rdlist2(iunit,list,nlist, limlist)
c       
      implicit none
      integer*4 list(*),iunit,nlist,limlist
      character*10240 line
      read(iunit,'(a)')line
      call parselist2(line,list,nlist,limlist)
      return
      end


      subroutine parselist(line,list,nlist)
      character*(*) line
      integer*4 list(*), nlist
      call parselist2(line,list,nlist,0)
      return
      end

      subroutine parselist2(line,list,nlist,limlist)
      implicit none
      character*(*) line
      integer*4 list(*),nlist,limlist, parselistfw, ierr
      ierr = parselistfw(line, list, nlist, limlist)
      if (ierr == 0) return
      if (ierr < 0) then
        write(*,'(/,a)')'ERROR: PARSELIST - TOO MANY LIST VALUES FOR ARRAY'
      elseif (ierr == 1) then
        write(*,'(/,a)')'ERROR: PARSELIST - FAILED TO ALLOCATE MEMORY FOR LIST'
      else
        write(*,'(/,a)')'ERROR: PARSELIST - BAD CHARACTER IN ENTRY'
      endif
      call exit(1)
      end


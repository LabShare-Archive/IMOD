c       A bad routine to read all elements in file into list
      subroutine xfrdall(nunit,flist,nlist,*)
      implicit none
      integer*4 nunit, nlist,nt
      real*4 flist(2,3,*)
      nlist=0
10    nt=nlist+1
      call xfread(nunit,flist(1,1,nt),*20,*96)
      nlist=nt
      go to 10
20    return
96    return 1
      end

c       A better routine
      subroutine xfrdall2(nunit,flist,nlist, limlist, ierr)
      implicit none
      integer*4 nunit, nlist,nt, limlist, ierr
      real*4 flist(2,3,*), ftmp(2,3)
      ierr = 1
      nlist = 0
10    call xfread(nunit,ftmp,*20,*96)
      nlist = nlist + 1
      if (nlist .gt. limlist) return
      call xfcopy(ftmp, flist(1,1,nlist))
      go to 10
20    ierr = 0
      return
96    ierr = 2
      return
      end

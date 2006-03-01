c       !
c       Computes polynomial terms from [x] and [y] of order [norder], and
c       places then in array [vect].  These terms can then be used to fit a
c       polynomial in to measurements as a function of X and Y.  The number of
c       terms returned is {norder * (norder + 3) / 2}.  The first set of terms
c       is [x] and [y].  Each next set is the previous set multipled by [x],
c       plus the last term of the previous set multiplied by [y].
c       !       
      subroutine polyterm(x,y,norder,vect)
      implicit none
      real*4 vect(*), x, y
      integer*4 norder, istr,iend, iorder, i
      vect(1)=x
      vect(2)=y
      istr=1
      iend=2
      do iorder=2,norder
        do i=istr,iend
          vect(i+iorder)=vect(i)*x
        enddo
        istr=istr+iorder
        vect(iend+iorder+1)=vect(iend)*y
        iend=iend+iorder+1
      enddo
      return
      end


c       subroutine matinv
c       Now calls gaussj to get determinant
c
      subroutine matinv(a,n,b,m,determ)
      include 'statsize.inc'
      real*4 a(msiz,msiz), b(*), determ
      integer*4 n, m
      call gaussjDet(a, n, msiz, b, m, m, determ)
      return
      end

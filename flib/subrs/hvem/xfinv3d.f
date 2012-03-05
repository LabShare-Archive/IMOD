      subroutine xfinv3d(a,d,ainv,dinv)
      real*4 a(3,3),ainv(3,3),d(3),dinv(3)
      call inv_matrix(a,ainv)
      do i=1,3
        dinv(i)=0.
        do j=1,3
          dinv(i)=dinv(i)-ainv(i,j)*d(j)
        enddo
      enddo
      return
      end

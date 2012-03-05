      subroutine xfmult3d(a1,d1,a2,d2,a3,d3)
      real*4 a1(3,3),a2(3,3),a3(3,3),d1(3),d2(3),d3(3)
      do i=1,3
        d3(i)=d2(i)
        do j=1,3
          d3(i)=d3(i)+a2(i,j)*d1(j)
          a3(i,j)=0.
          do k=1,3
            a3(i,j)=a3(i,j)+a2(i,k)*a1(k,j)
          enddo
        enddo
      enddo
      return
      end

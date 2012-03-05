      subroutine xfcopy3d(a1,d1,a2,d2)
      real*4 a1(3,3),a2(3,3),d1(3),d2(3)
      do i=1,3
        d2(i)=d1(i)
        do j=1,3
          a2(i,j)=a1(i,j)
        enddo
      enddo
      return
      end

c SUBROUTINE: slicecopy.f
c
c PURPOSE:    Take a slice from 3D volume (in the xz-plane) and copy
c             it into a 2D-matrix. 
c
c INPUT:      u3D      real*4(array)          Array where the input
c                                             volume stacked as x,z,y.
c             k        integer*4              Slice to be extracted.
c             N1,N2    integer*4              Sizes in x and z.
c
c OUTPUT:     u        real*4(array)          Array with the output slice.
c
c
c NOTE: 
c
c 010223
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       subroutine slicecopy(u,u3D,k,N1,N2)   


       implicit real *4 (a-h,o-z)



       real *4 u3D(*),u(N1,N2)

c
c ============================================================
c

       ipoint=(k-1)*N1*N2
       do l=1,N2
          do i=1,N1
             u(i,l)=u3D(ipoint+i)
          enddo
          ipoint=ipoint+N1
       enddo

c
c ============================================================
c

       return
       end

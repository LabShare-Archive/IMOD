c   routines to read/write a transform
        subroutine xfwrite(iunit,f,*)
        real*4  f(2,3)
        write(iunit,101,err=20)((f(i,j),j=1,2),i=1,2),f(1,3),f(2,3)
101     format(4f12.7,2f12.3)
        return
20      return 1
        end

        subroutine xfread(iunit,f,*,*)
        real*4  f(2,3)
        read(iunit,*,end=10,err=20)((f(i,j),j=1,2),i=1,2),f(1,3),f(2,3)
        return
10      return 1
20      return 2
        end

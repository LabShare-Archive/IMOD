c   forms linear combination of two xforms: h=a*f+b*g
        subroutine xflincom(f,a,g,b,h)
        real*4 f(2,3),g(2,3),h(2,3)
        h(1,3)=a*f(1,3)+b*g(1,3)
        h(2,3)=a*f(2,3)+b*g(2,3)
        do i=1,2
          do j=1,2
            h(i,j)=a*f(i,j)+b*g(i,j)
          enddo
        enddo
        return 
        end

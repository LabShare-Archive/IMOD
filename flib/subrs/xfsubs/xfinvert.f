c   finds inverse of a transform
        subroutine xfinvert(f,fp)
        real*4  f(2,3),fp(2,3)
	denom = f(1,1)*f(2,2) - f(1,2)*f(2,1)
	fp(1,1) =  f(2,2)/denom
	fp(1,2) = -f(1,2)/denom
	fp(2,1) = -f(2,1)/denom
	fp(2,2) =  f(1,1)/denom
        fp(1,3) = -(fp(1,1)*f(1,3) + fp(1,2)*f(2,3))
        fp(2,3) = -(fp(2,1)*f(1,3) + fp(2,2)*f(2,3))
        return
        end

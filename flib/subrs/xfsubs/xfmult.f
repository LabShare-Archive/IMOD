c   forms product of xform one followed by xform two
        subroutine xfmult(one,two,prod)
        real*4 one(2,3),two(2,3),prod(2,3)
        prod(1,1)=two(1,1)*one(1,1) + two(1,2)*one(2,1)
        prod(1,2)=two(1,1)*one(1,2) + two(1,2)*one(2,2)
        prod(2,1)=two(2,1)*one(1,1) + two(2,2)*one(2,1)
        prod(2,2)=two(2,1)*one(1,2) + two(2,2)*one(2,2)
        prod(1,3)=two(1,3) + two(1,1)*one(1,3) + two(1,2)*one(2,3)
        prod(2,3)=two(2,3) + two(2,1)*one(1,3) + two(2,2)*one(2,3)
        return
        end

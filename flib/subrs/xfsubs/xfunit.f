c   returns a "unit" xform with a(1,1) and a(2,2) set equal to VAL
        subroutine xfunit(f,VAL)
        real*4 f(2,3)
        f(1,1)=VAL
        f(1,2)=0.
        f(2,1)=0.
        f(2,2)=VAL
        f(1,3)=0.
        f(2,3)=0.
        return
        end

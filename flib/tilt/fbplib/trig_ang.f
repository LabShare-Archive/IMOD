c SUBROUTINE: 
c
c PURPOSE:     
c
c INPUT:      
c
c OUTPUT:    
c
c
c NOTE: 
c
c 001212
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine trig_ang(angles,N,c,s,t,ci)


       implicit real *4 (a-h,o-z)

       real *4 angles(*),c(*),s(*),t(*),ci(*)
 
      

c
c ==============================================================
c

       do l=1,N
          c(l)=cos(angles(l))     
          s(l)=sin(angles(l))
          t(l)=tan(angles(l))
          ci(l)=1./c(l)
       enddo
          
c
c ==============================================================
c

       return
       end

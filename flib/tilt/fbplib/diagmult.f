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

       subroutine  diagmult(zuin,ldzuin,nviews,filter,cutoff,ifh)  


       implicit real *4 (a-h,o-z)


       integer *4 cutoff(*)
      
       real *4 filter(ifh+1,nviews)

       complex *8 zuin(ldzuin,nviews)  

c
c ============================================================
c

       do l=1,nviews
          do ifr=1,cutoff(l)
             zuin(ifr,l)=filter(ifr,l)*zuin(ifr,l)
          enddo
       enddo

c
c ============================================================
c

       return
       end

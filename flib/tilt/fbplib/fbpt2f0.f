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

         subroutine fbpt2f0(uin,uout,NprjP,np,
     &                      x,diag,wini,wsave,wrk)



         implicit real *4 (a-h,o-z)
         


         real *4 x(*),wini(*)

         complex *8  uin(*),uout(*),diag(*),wsave(*),wrk(*)
         

c
c ==============================================================
c


       
     
     
c ____________ Computation of trig. sum (USFFT) ____________
c

         isign = 1

         call ufs1ao(NprjP,uin,np,x,uout,isign,wini,wsave,wrk)
 
c
c __________________________________________________________


       


c ___________ Post-multiplication by a factor ______________
c

         do ifr=1,np
            uout(ifr)=uout(ifr)*diag(ifr)
         enddo

c
c ___________________________________________________________


        

c
c ===================================================================
c

      return
      end



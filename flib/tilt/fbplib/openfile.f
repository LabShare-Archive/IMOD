      
        subroutine openfile(fname,nslice,nwide,nthick)

        implicit real *4 (a-h,o-z)



        character *12 fname

c
c ========================================================
c

        open (unit=3,file=fname,status='new',
     &        form='unformatted',iostat=ios)
         
        if (ios .ne. 0) print *, ios  
        write(3) real(nslice),real(nwide),real(nthick)

c
c ========================================================
c

        return
        end

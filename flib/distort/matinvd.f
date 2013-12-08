c       subroutine matinv
c       from Cooley and Lohnes - Multivariate Procedures for the
c       Behavioral Sciences, Wiley, 1962 (!).
c       inverts matrix a of order n, computes determinant of a
c       replaces b with a solution vector if called for by m>0
c       12/25/90: converted to fortran 77, indented, put in message
c       for singularity, verified basically same program as GAUSSJ from
c       Press et al 1986 for Gauss-Jordan elimination
c       
      subroutine matinvd(a,msiz,n,b,m,determ)
c       parameter (msiz = 1000)
      parameter (linsiz = 10000)
      implicit double precision (a-h,o-z)
      dimension a(msiz,msiz), b(msiz), pivot(linsiz), ipivot(linsiz),
     1    index(linsiz,2)
      equivalence (irow,jrow), (icolum,jcolum), (amax, t, swap)
      determ=1.0
      do 20 j=1,n
        ipivot(j)=0
20    continue 
      do 550 i=1,n
        amax=0.
        do 105 j=1,n
          if(ipivot(j).ne.1)then
            do 100 k=1,n
              if(ipivot(k).eq.0)then
                if(abs(amax).lt.abs(a(j,k)))then
                  irow=j
                  icolum=k
                  amax=a(j,k)
                endif
              elseif(ipivot(k).gt.1)then
                write(*,*) 'Singular matrix'
                return
              endif
100         continue
          endif
105     continue
        ipivot(icolum)=ipivot(icolum)+1
        if(irow.ne.icolum)then
          determ=-determ
          do 200 l=1,n
            swap=a(irow,l)
            a(irow,l)=a(icolum,l)
            a(icolum,l)=swap
200       continue 
          if(m.ne.0)then
            swap=b(irow)
            b(irow)=b(icolum)
            b(icolum)=swap
          endif
        endif
        index(i,1)=irow
        index(i,2)=icolum
        pivotmp=a(icolum,icolum)
        if(abs(pivotmp).lt.1.e-30) write(*,*) 'small pivot',pivotmp
        pivot(i)=pivotmp
        determ=determ*pivotmp
        a(icolum,icolum)=1.
c         worried about that step!
        do 350 l=1,n
          a(icolum,l)=a(icolum,l)/pivotmp
350     continue
        if(m.gt.0)b(icolum)=b(icolum)/pivotmp
        do 545 l1=1,n
          if(l1.ne.icolum)then
            t=a(l1,icolum)
            a(l1,icolum)=0.
            do 450 l=1,n
              a(l1,l)=a(l1,l)-a(icolum,l)*t
450         continue
            if(m.gt.0)b(l1)=b(l1)-b(icolum)*t
          endif
545     continue
550   continue
      do 710 i=1,n
        l=n+1-i
        if(index(l,1).ne.index(l,2))then
          jrow=index(l,1)
          jcolum=index(l,2)
          do 705 k=1,n
            swap=a(k,jrow)
            a(k,jrow)=a(k,jcolum)
            a(k,jcolum)=swap
705       continue
        endif
710   continue
      return
      end

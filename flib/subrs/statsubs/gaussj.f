c	subroutine GAUSSJ
c	inverts matrix A of order N, (dimensions NP)
c	replaces M columns of B (dimensions NP by MP) with solution vectors
c	originally from Cooley and Lohnes - Multivariate Procedures for the
c	Behavioral Sciences, Wiley, 1962 (!).
c	  12/25/90: converted to fortran 77, indented, put in message
c	  for singularity, verified basically same program as GAUSSJ from
c	  Press et al 1986 for Gauss-Jordan elimination, changed arguments,
c	  removed determinant calculation and augmented treatment of B
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2002/07/30 00:51:30  mast
c	  Added check for array bounds being exceeded
c	
c
	subroutine gaussj(a,n,np,b,m,mp)
        parameter (msiz=10000)
	dimension a(np,np), b(np,mp), pivot(msiz), ipivot(msiz),
     1 index(msiz,2)
	equivalence (irow,jrow), (icolum,jcolum), (amax, t, swap)

	if (n .gt. msiz) then
	  print *
	  print *,'ERROR: GAUSSJ - MATRIX TOO LARGE FOR INTERNAL ARRAYS'
	  call exit(1)
	endif
	do 20 j=1,n
	  ipivot(j)=0
20	continue 
	do 550 i=1,n
	  amax=0.
	  do 105 j=1,n
	    if(ipivot(j).ne.1)then
	      do 100 k=1,n
		if(ipivot(k).eq.0)then
		  if(amax.lt.abs(a(j,k)))then
		    irow=j
		    icolum=k
		    amax=abs(a(j,k))
		  endif
		elseif(ipivot(k).gt.1)then
		  write(*,*) 'GAUSSJ: Singular matrix'
		  return
		endif
100	      continue
	    endif
105	  continue
	  ipivot(icolum)=ipivot(icolum)+1
	  if(irow.ne.icolum)then
	    do 200 l=1,n
	      swap=a(irow,l)
	      a(irow,l)=a(icolum,l)
	      a(icolum,l)=swap
200	    continue 
	    do 15 l=1,m
	      swap=b(irow,l)
	      b(irow,l)=b(icolum,l)
	      b(icolum,l)=swap
15	    continue
	  endif
	  index(i,1)=irow
	  index(i,2)=icolum
	  pivotmp=a(icolum,icolum)
	  if(abs(pivotmp).lt.1.e-30) write(*,*) 'small pivot',pivotmp
	  pivot(i)=pivotmp
	  a(icolum,icolum)=1.
c	    worried about that step!
	  do 350 l=1,n
	    a(icolum,l)=a(icolum,l)/pivotmp
350	  continue
	  do 17 l=1,m
	    b(icolum,l)=b(icolum,l)/pivotmp
17	  continue 
	  do 545 l1=1,n
	    if(l1.ne.icolum)then
	      t=a(l1,icolum)
	      a(l1,icolum)=0.
	      do 450 l=1,n
		a(l1,l)=a(l1,l)-a(icolum,l)*t
450	      continue
	      do 19 l=1,m
		b(l1,l)=b(l1,l)-b(icolum,l)*t
19	      continue 
	    endif
545	  continue
550	continue
	do 710 i=1,n
	  l=n+1-i
	  if(index(l,1).ne.index(l,2))then
	    jrow=index(l,1)
	    jcolum=index(l,2)
	    do 705 k=1,n
	      swap=a(k,jrow)
	      a(k,jrow)=a(k,jcolum)
	      a(k,jcolum)=swap
705	    continue
	  endif
710	continue
	return
	end

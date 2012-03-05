c       subroutine dirnm
c       from Cooley and Lohnes - Multivariate Procedures for the
c       Behavioral Sciences, Wiley, 1962 (!).
c       diagonalizes a real non-symmetric matrix of the form b-inverse*a
c       a and b are m by m input matrices.  Upon return, vector xl contains
c       the eigenvalues of b-1*a, and matrix x contains the eigenvectors
c       in its columns, normalized.  hdiag is required.
c       matrix h, of order n.  It places eigenvalues in the diagonal elements
c       of h, eigenvectors in the columns of matrix u if iegen =0 not 1.
c       nr contains # of rotations done
        subroutine dirnm(a, m, b, x, xl)
        include 'statsize.inc'
        dimension a(msiz,msiz), b(msiz,msiz), x(msiz,msiz), xl(msiz)
        call hdiag(b,m,0,x,nr)
        do 1 i=1,m
1       xl(i)= 1.0/sqrt(abs(b(i,i)))
        do 2 i=1,m
        do 2 j=1,m
2       b(i,j) = x(i,j)*xl(j)
        do 3 i=1,m
        do 3 j=1,m
        x(i,j)=0.
        do 3 k=1,m
3       x(i,j)=x(i,j)+b(k,i)*a(k,j)
        do 4 i=1,m
        do 4 j=1,m
        a(i,j)=0.
        do 4 k=1,m
4       a(i,j)=a(i,j)+x(i,k)*b(k,j)
        trace=0.
        do 10 i = 1,m
10      trace=trace+a(i,i)
        call hdiag(a, m, 0, x, nr)
        sumr=0.
        do 12 i=1,m
        xl(i) = a(i,i)
12      sumr=sumr+xl(i)
        do 6 i=1,m
        do 6 j=1,m
        a(i,j)=0.
        do 6 k=1,m
6       a(i,j)=a(i,j)+b(i,k)*x(k,j)
        do 9 j=1,m
        sumv=0.
        do 7 i=1,m
c7      sumv=sumv+a(i,j)**2
7       sumv=sumv+abs(a(i,j))
c       den=sqrt(sumv)
        den=sumv
        do 8 i=1,m
8       x(i,j)=a(i,j)/den
9       continue
        return
        end

c	$Log$
c	Revision 1.1.1.1  2001/11/21 23:12:01  rickg
c	Import into repository
c	
c 
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
c                                                                           c
c        This file is a part of                                             c
c        Fast Transform Library                                             c
c        Contains proprietary information supplied by GB Consulting.        c
c        Copyright (C), 1998-99 GB Consulting. All rights reserved          c
c                                                                           c
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
c
c        Inverse FFT of columns of complex rectangular matrix
c        where the result is real matrix (reverse of t2f)
c
c        columns are paired to use complex FFT
c
c
c        Input:
c
c        ldz         - leading dimension of input matrix zz (complex *8)
c        ldr         - leading dimension of output matrix rr
c
c        nrow        - number of rows in the output matrix; 
c                      nrow/2 +1 <= ldz
c                      nrow      <= ldr
c
c        ncol        - number of columns  (both input and output)
c
c        zz          - (complex *8) contains the FFT (without normalization) 
c                      leading dimension of zz is ldz
c                      the actual size of the matrix is nrow/2+1 by ncol
c                      This matrix stores, coulumn by coulumn,
c                      distinct (nrow/2+1) frequencies for real signals
c
c        work        - work area real *4 of size 2*nrow
c
c        wsave       - work area real *4 of size 4*nrow+15
c                      (for FFT)
c
c        isign       - sign of the exponential of the Fourier transform, 
c                      1 or -1
c
c
c        Output:
c
c        rr          - (real *4 ) matrix of size ldr by ncol, 
c                      where ldr is the leading dimension
c                      only nrow by ncol portion of the matrix is used 
c
         subroutine f2t(rr,ldr,nrow,ncol,ldz,zz,wsave,work,isign)
c
	 implicit real *4 (a-h,o-z)

         real *4 rr(ldr,ncol),work(*)
c	 real *4 rr(ldr,ncol)
	 complex *8 zz(ldz,ncol),wsave(*),ima


         data ima/(0.0,1.0)/
c
c        initialize fft routine 
c

         call fftini(nrow,1,wsave)
c         call ffti(nrow,wsave)


c
         do 1000 j=1,ncol-1,2
c
         work(1)      = zz(1,j)
         work(2)      = zz(1,j+1)
c
         do 3000 l=2,nrow/2+1
         call cpc2r(zz(l,j)+zz(l,j+1)*ima,work(2*l-1))
         call cpc2r(conjg(zz(l,j)-zz(l,j+1)*ima),work(2*nrow-2*l+3))
 3000    continue
c
         call fft1s(nrow,work,wsave,isign)
c
         do 2000 l=1,nrow
         rr(l,j)   = work(2*l-1)/nrow 
         rr(l,j+1) = work(2*l)/nrow
 2000    continue
c
 1000    continue
c
         if(iand(ncol,1).ne.0) then
c
         work(1)      = zz(1,ncol)
         work(2)      = 0
c
         do 3001 l=2,nrow/2+1
         call cpc2r(zz(l,ncol),work(2*l-1))
         call cpc2r(conjg(zz(l,ncol)),work(2*nrow-2*l+3))
 3001    continue
c
         call fft1s(nrow,work,wsave,isign)
c
         do 2001 l=1,nrow
         rr(l,ncol)   = work(2*l-1)/nrow 
 2001    continue
c
c
         endif
c
         return
         end
c
c
         subroutine cpc2r(cc,rr)
c
	 implicit real *4 (a-h,o-z)
	 complex *8 cc,rr
c
         rr = cc
c
         return
         end

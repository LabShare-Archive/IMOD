#ifndef LINEARFITTING_H
#define LINEARFITTING_H
#include "imodconfig.h"

#ifdef F77FUNCAP
#define dgels DGELS
#define dgelss DGELSS
#else
#define dgels dgels_
#define dgelss dgelss_
#endif

extern "C" {
  //QR decomposition;
  void dgels( char* TRANS, int* M, int* N, int* NRHS, double* A, int* LDA, 
      double* B, int* LDB, double* WORK, int* LWORK, int* INFO ); 
  void dgelss(int*M, int*N, int* NRHS, double* A, int*LDA, double* B, 
      int* LDB,  double* S, double* RCOND, int* RANK, double* WORK, 
      int* LWORK, int* INFO); //SVD
}

/*Purpose
*  =======
*
*  DGELSS computes the minimum norm solution to a real linear least
*  squares problem:
*
*  Minimize 2-norm(| b - A*x |).
*
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A. N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the first min(m,n) rows of A are overwritten with
*          its right singular vectors, stored rowwise.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, B is overwritten by the N-by-NRHS solution
*          matrix X.  If m >= n and RANK = n, the residual
*          sum-of-squares for the solution in the i-th column is given
*          by the sum of squares of elements n+1:m in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,max(M,N)).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A in decreasing order.
*          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A.
*          Singular values S(i) <= RCOND*S(1) are treated as zero.
*          If RCOND < 0, machine precision is used instead.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the number of singular values
*          which are greater than RCOND*S(1).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1, and also:
*          LWORK >= 3*min(M,N) + max( 2*min(M,N), max(M,N), NRHS )
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  the algorithm for computing the SVD failed to converge;
*                if INFO = i, i off-diagonal elements of an intermediate
*                bidiagonal form did not converge to zero.
*/

class LinearFitting
{
   public:
     LinearFitting(int nRaw);
     ~LinearFitting();
     int computeFitting(double *result, double *model, int nModel, int index1,
         int index2);
     int getDim() {return nDim;}
     void setRaw(double *rawData);
   private:
    int nDim;
    double *raw;
};

#endif

/*
 *  lapackc.h -- C prototypes for lapack functions
 *
 *  $Id$
 */

#ifndef LAPACKC_H
#define LAPACKC_H
#include "imodconfig.h"

#ifdef F77FUNCAP
#define dgels DGELS
#define dgelss DGELSS
#define dsyev DSYEV
#define dlaev2 DLAEV2
#define dsysv DSYSV
#else
#define dgels dgels_
#define dgelss dgelss_
#define dsyev dsyev_
#define dlaev2 dlaev2_
#define dsysv dsysv_
#endif

#ifdef __cplusplus
extern "C" {
#endif

  /* QR decomposition; */
  void dgels(const char* TRANS, int* M, int* N, int* NRHS, double* A, int* LDA, 
             double* B, int* LDB, double* WORK, int* LWORK, int* INFO, int *transSize); 
  /* SVD */
  void dgelss(int*M, int*N, int* NRHS, double* A, int*LDA, double* B, 
              int* LDB,  double* S, double* RCOND, int* RANK, double* WORK, 
              int* LWORK, int* INFO);
  void dsyev(const char *jobz, const char *uplo, int *n, double *a, int *lda, double *w,
             double *work, int *lwork, int *info, int jobzSize, int uploSize);
  void dlaev2(double *a, double *b, double *c, double *rt1, double *rt2, double *cs1, 
              double *sn1);
  void dsysv(const char *uplo, int *n, int *nrhs, double *a, int *lda, int *ipiv,
             double *b, int *ldb, double *work, int *lwork, int *info, 
             int uploSize);


#ifdef __cplusplus
}
#endif

#endif

/* lsqr.h
   $Revision$ $Date$
*/
/*!
   \file
   Header file for ISO C version of LSQR.
*/
void
lsqr( int m,
      int n,
      void (*aprod)(int mode, int m, int n, double x[], double y[],
                    void *UsrWrk ),
      double damp,
      void   *UsrWrk,
      double u[],    // len = m
      double v[],    // len = n
      double w[],    // len = n
      double x[],    // len = n
      double se[],   // len = *
      double atol,
      double btol,
      double conlim,
      int    itnlim,
      FILE   *nout,
      // The remaining variables are output only.
      int    *istop_out,
      int    *itn_out,
      double *anorm_out,
      double *acond_out,
      double *rnorm_out,
      double *arnorm_out,
      double *xnorm_out
     );

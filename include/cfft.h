
/* Header file for C translation of FFT library and wrapper to FFTW */

/* $Id$ */
#ifndef CFFT_H
#define CFFT_H

#include "imodconfig.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef F77FUNCAP
#define odfft ODFFT
#define todfft TODFFT
#define thrdfft THRDFFT
#define usingFFTW USINGFFTW
#define niceFFTlimit NICEFFTLIMIT
#else
#define odfft odfft_
#define todfft todfft_
#define thrdfft thrdfft_
#define usingFFTW usingfftw_
#define niceFFTlimit nicefftlimit_
#endif

int usingFFTW(void);
int niceFFTlimit(void);
void odfft(float *array, int *nxp, int *nyp, int *idirp);
void odfftc(float *array, int nx, int ny, int idir);
void todfft(float *array, int *nxp, int *nyp, int *idirp);
void todfftc(float *array, int nx, int ny, int idir);
void thrdfft(float *array, float *brray, int *nxp, int *nyp, int *nzp, 
             int *idirp);
void thrdfftc(float *array, float *brray, int nx, int ny, int nz, int idir);
void srfp (int pts, int pmax, int twogrp, int *factor, int *sym, int *psym,
	   int *unsym, int *error);
void realft (float *even, float *odd, int n, int *dim);
void cmplft (float *x, float *y, int n, int *d);
void hermft(float *x, float *y, int n, int *dim);
void diprp (int pts, int *sym, int psym, int *unsym, int *dim,
	    float *x, float *y);
void mdftkd (int n, int *factor, int *dim, float *x, float *y);
void r2cftk (int n, int m, float *x0, float *y0, float *x1, float *y1,
	     int *dim);
void r3cftk (int n, int m, float *x0, float *y0, float *x1, float *y1, 
	     float *x2, float *y2, int *dim);
void r4cftk (int n, int m, float *x0, float *y0, float *x1, float *y1, 
	     float *x2, float *y2, float *x3, float *y3, int *dim);
void r5cftk (int n, int m, float *x0, float *y0, float *x1, float *y1,
	     float *x2, float *y2, float *x3, float *y3, float *x4, float *y4,
	     int *dim);
void r8cftk (int n, int m, float *x0, float *y0, float *x1, float *y1,
	     float *x2, float *y2, float *x3, float *y3, float *x4,
	     float *y4, float *x5, float *y5, float *x6, float *y6,
	     float *x7, float *y7, int *dim);
void rpcftk (int n, int m, int p, int r, float *x, float *y, int *dim);

#ifdef __cplusplus
}
#endif
#endif

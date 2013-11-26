/*
 * fftw_wrap.c - Wrapper to FFTW, to be compiled instead of all other modules in directory
 *
 * Author: David Mastronarde
 *
 * $Id$
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "b3dutil.h"
#include "fftw3.h"
#include "cfft.h"

static void report(const char *mess);
static int indexAndThreadsForPlan(double geomSize, int goodPrimes);
static void normalize(float *array, float scale, size_t dataSize);
static int goodSize(int nx);

/* just in case it helps FFTW know that a plan is the same as a previous one,
   hold on to plans in a ring buffer */
#define MAX_PLANS 20
static int sInitialized = 0;
static fftwf_plan sPlans[MAX_PLANS];
static int sRingIndex = 0;
static int sNumInRing = 0;
static int doTiming = 0;
static double sWallStart;

/* Find the next index for a new plan, destroy an existing plan, and set the number
   of threads to use */
static int indexAndThreadsForPlan(double geomSize, int goodPrimes)
{
  int numThreads, maxThreads;
  int retval = sRingIndex;

  if (!sInitialized) {
    fftwf_init_threads();
    doTiming = (getenv("IMOD_FFTW_TIMING") != NULL) ? 1 : 0;
  }
  sInitialized = 1;
  if (sNumInRing < MAX_PLANS)
    sNumInRing++;
  else
    fftwf_destroy_plan(sPlans[sRingIndex]);
  sRingIndex = (sRingIndex + 1) % MAX_PLANS;
  if (doTiming)
    sWallStart = wallTime();

  /* This goes up 2 threads with every doubling of size, starting with 2 at 128 
     Limit the threads to 4 when there are any bad factors */
  maxThreads = (int)((log(geomSize) / log(2.) - 5.95) * 2.);
  B3DCLAMP(maxThreads, 1, (goodPrimes ? 8 : 4));
  numThreads = numOMPthreads(maxThreads);
  if (doTiming)
    printf("Using %d threads\n", numThreads);
  fftwf_plan_with_nthreads(numThreads);
  return retval;
}

/* Output timing if desired */
static void report(const char *mess)
{
  double now;
  if (!doTiming)
    return;
  now = wallTime();
  printf("%s:  %.3f\n", mess, (now - sWallStart) * 1000.);
  fflush(stdout);
  sWallStart = now;
}

/* Normalize the given number of real elements by the scaling factor */
static void normalize(float *array, float scale, size_t dataSize)
{
  int i;
  for (i = 0; i < dataSize; i++)
    array[i] *= scale;
}

/* Returns 1 if there is no prime factor greater than 1, 0 otherwise */
static int goodSize(int nx)
{
  int i;
  for (i = 2; i <= 13; i++)
    while ((nx % i) == 0)
      nx /= i;
  return (nx > 1 ? 0 : 1);
}

/* The 1D FFT wrapper.  See odfft.c for conventions */
void odfft(float *array, int *nxp, int *nyp, int *idirp)
{
  int nx = *nxp;
  int ny = *nyp;
  int idir = *idirp;
  int nxpad = 2 * (nx / 2 + 1);
  float scale = (float)(1. / sqrt((double)nx));

  /* For 1D FFT, planning is proportional to thread number and depends only on NX,
     so averaging with ny itself penalizes for long X/short Y cases where planning
     could predominate */
  double geomSize = (sqrt((double)nx * ny) + ny) / 2.;
  int goodPrimes = goodSize(nx);
  int index = indexAndThreadsForPlan(geomSize, goodPrimes);
  if (idir == 0) {
    sPlans[index] = fftwf_plan_many_dft_r2c
      (1, nxp, ny, array, NULL, 1, nxpad, (fftwf_complex *)array, NULL, 1, nxpad / 2,
       FFTW_ESTIMATE);
  } else if (idir == 1) {
    sPlans[index] = fftwf_plan_many_dft_c2r
      (1, nxp, ny, (fftwf_complex *)array, NULL, 1, nxpad / 2, array, NULL, 1, nxpad, 
       FFTW_ESTIMATE);
  } else if (idir == -1 || idir == -2) {
    sPlans[index] = fftwf_plan_many_dft
      (1, nxp, ny, (fftwf_complex *)array, NULL, 1, nx, (fftwf_complex *)array,
      NULL, 1, nx, idir == -1 ? FFTW_FORWARD : FFTW_BACKWARD, FFTW_ESTIMATE);
    nxpad = nx * 2;
  } else {
    printf("\nERROR: odfft - direction must be between -2 and 1\n");
    exit(1);
  }
  report("odfft planning");
  fftwf_execute(sPlans[index]);
  report("odfft FFT");
  normalize(array, scale, (size_t)nxpad * ny);
}

/* The 2D FFT wrapper.  See todfft.c for conventions */
void todfft(float *array, int *nxp, int *nyp, int *idirp)
{
  int nx = *nxp;
  int ny = *nyp;
  int nxpad = 2 * (nx / 2 + 1);
  double geomSize = sqrt((double)nx * ny);
  int goodPrimes = goodSize(nx) * goodSize(ny);
  int index = indexAndThreadsForPlan(geomSize, goodPrimes);
  if (*idirp)
    sPlans[index] = fftwf_plan_dft_c2r_2d(ny, nx, (fftwf_complex *)array, array,
                                          FFTW_ESTIMATE);
  else
    sPlans[index] = fftwf_plan_dft_r2c_2d(ny, nx, array, (fftwf_complex *)array,
                                          FFTW_ESTIMATE);
  report("todfft planning");
  fftwf_execute(sPlans[index]);
  report("todfft FFT");
  normalize(array, (float)(1. / sqrt((double)nx * ny)), (size_t)nxpad * ny);
}

/* The 3D FFT wrapper.  See thrdfft.c for conventions */
void thrdfft(float *array, float *brray, int *nxp, int *nyp, int *nzp, int *idirp)
{
  int nx = *nxp;
  int ny = *nyp;
  int nz = *nzp;
  int nxpad = 2 * (nx / 2 + 1);
  double geomSize = pow((double)nx * ny * nz, 0.3333);
  int goodPrimes = goodSize(nx) * goodSize(ny) * goodSize(nz);
  int index = indexAndThreadsForPlan(geomSize, goodPrimes);
  if (*idirp)
    sPlans[index] = fftwf_plan_dft_c2r_3d(nz, ny, nx, (fftwf_complex *)array, array,
                                          FFTW_ESTIMATE);
  else
    sPlans[index] = fftwf_plan_dft_r2c_3d(nz, ny, nx, array, (fftwf_complex *)array,
                                          FFTW_ESTIMATE);
  report("thrdfft planning");
  fftwf_execute(sPlans[index]);
  report("thrdfft FFT");
  normalize(array,  (float)(1. / sqrt((double)nx * ny * nz)), (size_t)nxpad * ny * nz);
}

int usingFFTW(void)
{
  return 1;
}

int niceFFTlimit(void)
{
  return 13;
}

void todfftc(float *array, int nx, int ny, int idir)
{
  todfft(array, &nx, &ny, &idir);
}

void thrdfftc(float *array, float *brray, int nx, int ny, int nz, int idir)
{
  thrdfft(array, brray, &nx, &ny, &nz, &idir);
}

/* These have to be here to have a Windows def file that doesn't depend on whether
   FFT is available */
void srfp (int pts, int pmax, int twogrp, int *factor, int *sym, int *psym,
	   int *unsym, int *error){}
void realft (float *even, float *odd, int n, int *dim){}
void cmplft (float *x, float *y, int n, int *d){}
void hermft(float *x, float *y, int n, int *dim){}
void diprp (int pts, int *sym, int psym, int *unsym, int *dim,
	    float *x, float *y){}
void mdftkd (int n, int *factor, int *dim, float *x, float *y){}
void r2cftk (int n, int m, float *x0, float *y0, float *x1, float *y1,
	     int *dim){}
void r3cftk (int n, int m, float *x0, float *y0, float *x1, float *y1, 
	     float *x2, float *y2, int *dim){}
void r4cftk (int n, int m, float *x0, float *y0, float *x1, float *y1, 
	     float *x2, float *y2, float *x3, float *y3, int *dim){}
void r5cftk (int n, int m, float *x0, float *y0, float *x1, float *y1,
	     float *x2, float *y2, float *x3, float *y3, float *x4, float *y4,
	     int *dim){}
void r8cftk (int n, int m, float *x0, float *y0, float *x1, float *y1,
	     float *x2, float *y2, float *x3, float *y3, float *x4,
	     float *y4, float *x5, float *y5, float *x6, float *y6,
	     float *x7, float *y7, int *dim){}
void rpcftk (int n, int m, int p, int r, float *x, float *y, int *dim){}

/*
* simplexfitting.cpp - Fits Gaussian or CTF curve to segment of a 1D power 
*                      spectrum by the simplex method.
*
*  Authors: Quanren Xiong and David Mastronarde
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*/
#include <math.h>
#include <stdio.h>
#include "simplexfitting.h"
#include "myapp.h"

#define VARNUM 5
#define MAX_ITER 100
#define MIN_ERROR 0.1

int SimplexFitting::mDim=0;
int SimplexFitting::mIndex1=0;
int SimplexFitting::mIndex2=0;
double* SimplexFitting::mRaw=NULL;
DefocusFinder *SimplexFitting::mFinder = NULL;
int SimplexFitting::mNumVar = 0;
float SimplexFitting::mA[5] = {0., 0., 0., 0., 0.};
double SimplexFitting::mExpZero = 0.;

//SimplexFitting::SimplexFitting(double *rawData, int nRaw, int i_1, int i_2)
SimplexFitting::SimplexFitting(int nRaw, MyApp *app)
{
  mApp = app;
  mRaw = new double[nRaw];
  mDim=nRaw;
  mFinder = &mApp->defocusFinder;
}

SimplexFitting::~SimplexFitting()
{
  delete[] mRaw;
}

void SimplexFitting::setRaw(double *rawData)
{
  for (int i=0;i<mDim;i++) 
    mRaw[i]=rawData[i];
}

/*
 * Fit to a gaussian
 */
int SimplexFitting::fitGaussian(double* fitting, double &err, int
                                   howToInitParams)
// mIndex1 and mIndex2 are the
//starting index and endind index of the fitting range.
{
  float pp[VARNUM+1][VARNUM+1], yy[VARNUM+1];
  float da[VARNUM]={2.0, 2.0, 2.0, 2.0, 2.0};
  float a[VARNUM]={0.7, 0.0, 0.2, 0.0, 0.0};
  float min_a[VARNUM];
  float ptol[VARNUM];  
  int nvar=4;
  int iter, jmin, i,  iter_counter;
  float errmin;
  
  float delfac=2.0;
  float ftol2=5.0e-4;
  float ptol2=5.0e-4;
  float ftol1=1.0e-5;
  float ptol1=1.0e-5;
  
  //init params
  if (howToInitParams == 0) {
   a[0]=mRaw[mIndex1];
   a[2]=1.414*(mIndex2-mIndex1)/(mDim-1);
  } else {
   a[0]=mRaw[(mIndex1+mIndex2)/2-1];
   a[1]=0.5*(mIndex2-mIndex1)/(mDim-1);  //sigma=0.3*(mIndex2-mIndex1)/(mDim-1); 
   a[2]=1.414*0.3*(mIndex2-mIndex1)/(mDim-1);
  }
 
  //find the range of fitting data;
  double min, max;
  double range;
  if( (mIndex2-mIndex1)>0 ){
     if( mRaw[mIndex1]> mRaw[mIndex1+1] ){
       max=mRaw[mIndex1];
       min=mRaw[mIndex1+1];
     }else{
       min=mRaw[mIndex1];
       max=mRaw[mIndex1+1];
     }
     for(i=mIndex1+1;i<mIndex2;i++){
       if( mRaw[i]>max ) 
         max=mRaw[i];
       else if( mRaw[i]<min)
         min=mRaw[i];
     }
     range=max-min;
  } else 
    range=mRaw[mIndex1];
  
  iter_counter=0;
  err=100000.0;
  double scaling;
  
  while(iter_counter<MAX_ITER){
    amoebaInit(&pp[0][0], yy, VARNUM+1, nvar, delfac, ptol2, a, da, 
        &SimplexFitting::funk, ptol); 
    amoeba(&pp[0][0], yy, VARNUM+1, nvar, ftol2, &SimplexFitting::funk,
        &iter, ptol, &jmin);
    for (i = 0; i < nvar; i++) 
      a[i] = pp[i][jmin];
    amoebaInit(&pp[0][0], yy, VARNUM+1, nvar, delfac, ptol1, a, da, 
        &SimplexFitting::funk, ptol); 
    amoeba(&pp[0][0], yy, VARNUM+1, nvar, ftol1, &SimplexFitting::funk,
        &iter, ptol, &jmin);

    for (i = 0; i < nvar; i++) 
      a[i] = pp[i][jmin];
    funk(a, &errmin);
    if( (errmin<err || errmin<MIN_ERROR*range) && a[0]>0.0){
      err=errmin;
      for(i=0;i<nvar;i++) 
        min_a[i]=a[i];
    }

    iter_counter++; 
    if(errmin<MIN_ERROR*range && a[0]>0.0) 
      break;

    //re-initialize parameters
    if(iter_counter%2) 
      scaling=0.5*iter_counter/(MAX_ITER-1);
    else
      scaling=-0.5*iter_counter/(MAX_ITER-1);

    a[0]=(1+scaling)*mRaw[mIndex1];
    if(howToInitParams==0) 
      a[1]=0.0;
    else 
      a[1]=0.5*(mIndex2-mIndex1)/(mDim-1); 
    a[2]=(1+scaling)*1.414*(mIndex2-mIndex1)/(mDim-1);
    a[3]=0.0;
  }// iteration
  
  for (i=0; i < mDim; i++) { 
    fitting[i]=min_a[0]*exp( -((float)(i-mIndex1)/(mDim-1)-min_a[1])*
        ((float)(i-mIndex1)/(mDim-1)-min_a[1])/(min_a[2]*min_a[2])) +min_a[3];
  } 

  if( debugLevel>=3){
   printf("Iteration Num=%d threshold=%f Simplex fitting parameters for \
      range %d to %d are:\n", iter_counter, MIN_ERROR*range, mIndex1, mIndex2);
   printf("Fitting error=%f\t a[0]=%f\t a[1]=%f\t a[2]=%f\t a[3]=%f\n",err,
      min_a[0], min_a[1], min_a[2], min_a[3]); 
  }
  fflush(stdout);
  return 0; 
}

/*
 * Callback function for simplex search for a gaussian
 */
void SimplexFitting::funk(float* param, float* fValue)
{
  int i;
  double x;
  *fValue=0.0;
  // This is not a sum of squares!
  for (i = mIndex1; i < mIndex2; i++) {
    x = (double)(i-mIndex1)/(mDim-1.) - param[1];
    *fValue=*fValue + fabs(mRaw[i] - param[0] *
                           exp(-(x * x) / (param[2] * param[2])) - param[3]); 
  }
}

/*
 * Fit to a CTF-like curve
 */
int SimplexFitting::fitCTF(double* fitting, int nvar, double &err,
                           double &focus)
{
  float pp[VARNUM+1][VARNUM+1], yy[VARNUM+1];
  float da[VARNUM]={2.0, 2.0, 2.0, 2.0, 0.1};
  float ptol[VARNUM];  
  int iter, jmin, i;
  float errmin;
  float delfac=2.0;
  float ftol2=5.0e-4;
  float ptol2=5.0e-4;
  float ftol1=1.0e-5;
  float ptol1=1.0e-5;
  double x, x0, ctfval, startDef;
  double rawMin = 1.e30;
  double rawMax = -1.e30;

  // Get starting defocus depending on current option setting, and get 
  // the zero at that defocus
  if (mApp->getDefocusOption())
    startDef = mFinder->getDefocus();
  else
    startDef = mFinder->getExpDefocus();
  mFinder->getTwoZeros(startDef, mExpZero, x0);

  // Initialize values, or leave previous values for some kinds of 
  // restricted fits
  mNumVar = nvar;
  for (i = mIndex1; i <= mIndex2; i++) {
    rawMin = B3DMIN(rawMin, mRaw[i]);
    rawMax = B3DMAX(rawMax, mRaw[i]);
  }
  mA[0] = startDef;
  if (nvar > 1)
    mA[1] = rawMin - 0.1 * (rawMax - rawMin);
  if (nvar > 2)
    mA[2] = (mRaw[mIndex1] - rawMin) / 
      mFinder->CTFvalue(mIndex1/(mDim-1.), startDef);
  if (nvar > 3) {
    mA[3] = 10.;
    mA[4] = 1.;
  }
  
  amoebaInit(&pp[0][0], yy, VARNUM+1, nvar, delfac, ptol2, mA, da, 
             &SimplexFitting::funkCTF, ptol); 
  amoeba(&pp[0][0], yy, VARNUM+1, nvar, ftol2, &SimplexFitting::funkCTF,
         &iter, ptol, &jmin);
  for (i = 0; i < nvar; i++) 
    mA[i] = pp[i][jmin];
  amoebaInit(&pp[0][0], yy, VARNUM+1, nvar, delfac, ptol1, mA, da, 
             &SimplexFitting::funkCTF, ptol); 
  amoeba(&pp[0][0], yy, VARNUM+1, nvar, ftol1, &SimplexFitting::funkCTF,
         &iter, ptol, &jmin);

  for (i = 0; i < nvar; i++) 
    mA[i] = pp[i][jmin];
  funkCTF(mA, &errmin);
  err=sqrt(errmin);
  focus = mA[0];

  x0 = (double)(mIndex1) / (mDim - 1.);
  for (i=0; i < mDim; i++) { 
    x = (double)i / (mDim - 1.);
    ctfval = mFinder->CTFvalue(x, focus);
    fitting[i] = mA[1] + mA[2] * exp(-mA[3] * (x - x0)) * 
      pow(fabs(ctfval), (double)mA[4]);
  } 
  if( debugLevel >= 1){
   printf("CTF fitting parameters for range %d to %d are:\n", mIndex1, mIndex2);
   printf("Fitting error=%f\t def=%f\t base=%f\t scale=%g\t decay=%f\t pow=%f\n"
          ,err, mA[0], mA[1], mA[2], mA[3], mA[4]);
  }
  fflush(stdout);
  return 0;
}

/*
 * Recompute a previously fit CTF-like curve at given defocus
 */
void SimplexFitting::recomputeCTF(double* result, double defocus)
{
  int i;
  float err;
  double ctfval, x, x0;

  mA[0] = defocus;
  funkCTF(mA, &err);
  err = sqrt(err);
  x0 = (double)(mIndex1) / (mDim - 1.);
  for (i=0; i < mDim; i++) { 
    x = (double)i / (mDim - 1.);
    ctfval = mFinder->CTFvalue(x, defocus);
    result[i] = mA[1] + mA[2] * exp(-mA[3] * (x - x0)) * 
      pow(fabs(ctfval), (double)mA[4]);
  } 
  if( debugLevel >= 1){
   printf("CTF fitting parameters for range %d to %d are:\n", mIndex1, mIndex2);
   printf("Fitting error=%f\t def=%f\t base=%f\t scale=%g\t decay=%f\t pow=%f\n",
          err, mA[0], mA[1], mA[2], mA[3], mA[4]);
  }
  fflush(stdout);
}

/*
 * The callback function for the simplex search fitting to a CTF
 */
void SimplexFitting::funkCTF(float* param, float* fValue)
{
  double x, x0, y, err, zero1, zero2;
  int i;
  double parUse[5];
  for (i = 0; i < mNumVar; i++)
    parUse[i] = param[i];
  for (i = mNumVar; i < 5; i++)
    parUse[i] = mA[i];

  // Compute the error
  err = 0.;
  x0 = (double)(mIndex1) / (mDim - 1.);
  for (i = mIndex1; i <= mIndex2; i++) {
    x = (double)i / (mDim - 1.);
    y = parUse[1] + parUse[2] * exp(-parUse[3] * (x - x0)) * 
      pow(fabs(mFinder->CTFvalue(x, parUse[0])), parUse[4]) - mRaw[i];
    err += y * y;
  }

  // Make error much bigger if power gets too far from 1
  if (parUse[4] > 2. || parUse[4] < 0.5) {
    x = B3DMAX(parUse[4] - 2., 0.5 - parUse[4]);
    err *= 5 * (1 + 5 * x);
  }
  
  // If the defocus places the second zero closer to the expected defocus than
  // the first zero, make error bigger
  mFinder->getTwoZeros(parUse[0], zero1, zero2);
  if (fabs(mExpZero - zero2) < 0.33 * fabs(mExpZero - zero1))
    err *= 5. * (3. - fabs(mExpZero - zero2) / mExpZero);
  if (zero1 > 0.9)
    err *= 5. * (1. + 5. * (zero1 - 0.9));

  // Make error much bigger if defocus becomes negative (overfocus)
  if (parUse[0] < 0.0)
    err *= 5. * (1. - 5. * parUse[0]);

  *fValue = (float)err;
  /*printf("err=%f  def=%f  fc=%f  scale=%f  decay=%f\n", err, parUse[0], parUse[1],
    parUse[2], parUse[3]);*/
}


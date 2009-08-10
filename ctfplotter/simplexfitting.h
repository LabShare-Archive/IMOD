/*
 * simplexfitting.h - Header for SimplexFitting class
 *
 *  $Id$
 *
 *  $Log$
 *
 */
#ifndef CURVEFITTING_H
#define CURVEFITTING_H

#include "b3dutil.h"
#include "defocusfinder.h"

class SimplexFitting
{
 public:
  //SimplexFitting(double *rawData, int nRaw, int index1, int index2);
  SimplexFitting(int nRaw);
  ~SimplexFitting();
  static void funk(float *, float *);
  static void funkCTF(float *, float *);
  int fitGaussian(double* result, double &err, int howToInitParams);
  int fitCTF(double* result, int nvar, double &err, double &focus);
  void setRange(int n1 , int n2){ mIndex1=n1; mIndex2=n2;}
  void setRaw(double *rawData);
  int getDim() {return mDim;}

 private:
  static int mDim;
  static int mIndex1;
  static int mIndex2;
  static int mNumVar;
  static float mA[5];
  static double *mRaw;
  static DefocusFinder *mFinder;
};

#endif

#ifndef CURVEFITTING_H
#define CURVEFITTING_H

#include "b3dutil.h"


class SimplexFitting
{
   public:
     //SimplexFitting(double *rawData, int nRaw, int index1, int index2);
     SimplexFitting(int nRaw);
     ~SimplexFitting();
     static void funk(float *, float *);
     int computeFitting(double* result, double *err, int howToInitParams);
     void setRange(int n1 , int n2){ index1=n1; index2=n2;}
     void setRaw(double *rawData);
     int getDim() {return nDim;}

   private:
    static int nDim;
    static int index1;
    static int index2;
    static double *raw;
};

#endif

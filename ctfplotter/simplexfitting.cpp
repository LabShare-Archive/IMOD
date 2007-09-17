#include <math.h>
#include <stdio.h>
#include "simplexfitting.h"

#define VARNUM 4
#define MAX_ITER 100
#define MIN_ERROR 0.1

int SimplexFitting::nDim=0;
int SimplexFitting::index1=0;
int SimplexFitting::index2=0;
double* SimplexFitting::raw=NULL;

void SimplexFitting::funk(float* param, float* fValue)
{
   int i;
   *fValue=0.0;
   for(i=index1;i<index2;i++){
     *fValue=*fValue + fabs( raw[i]-param[0]*exp(
         -((float)(i-index1)/(nDim-1)-param[1])*((float)(i-index1)/(nDim-1)-
           param[1])/(param[2]*param[2]) ) -param[3] ); 
   }
}

//SimplexFitting::SimplexFitting(double *rawData, int nRaw, int i_1, int i_2)
SimplexFitting::SimplexFitting(int nRaw)
{
  raw=new double[nRaw];
  nDim=nRaw;
}

SimplexFitting::~SimplexFitting()
{
  delete[] raw;
}

void SimplexFitting::setRaw(double *rawData)
{
  for(int i=0;i<nDim;i++) raw[i]=rawData[i];
}

int SimplexFitting::computeFitting(double* fitting, double *err, int
    howToInitParams)
// index1 and index2 are the
//starting index and endind index of the fitting range.
{
  float pp[VARNUM+1][VARNUM+1], yy[VARNUM+1];
  float da[VARNUM]={2.0, 2.0, 2.0, 2.0};
  float a[VARNUM]={0.7, 0.0, 0.2, 0.0};
  float min_a[VARNUM];
  float ptol[VARNUM];  
  int nvar=VARNUM;
  int iter, jmin, i,  iter_counter;
  float errmin;
  
  float delfac=2.0;
  float ftol2=5.0e-4;
  float ptol2=5.0e-4;
  float ftol1=1.0e-5;
  float ptol1=1.0e-5;
  
  //init params
  if( howToInitParams==0 ){
   a[0]=raw[index1];
   a[2]=1.414*(index2-index1)/(nDim-1);
  }else{
   a[0]=raw[(index1+index2)/2-1];
   a[1]=0.5*(index2-index1)/(nDim-1);  //sigma=0.3*(index2-index1)/(nDim-1); 
   a[2]=1.414*0.3*(index2-index1)/(nDim-1);
  }
 
  //find the range of fitting data;
  double min, max;
  double range;
  if( (index2-index1)>0 ){
     if( raw[index1]> raw[index1+1] ){
       max=raw[index1];
       min=raw[index1+1];
     }else{
       min=raw[index1];
       max=raw[index1+1];
     }
     for(i=index1+1;i<index2;i++){
       if( raw[i]>max ) max=raw[i];
       else if( raw[i]<min) min=raw[i];
     }
     range=max-min;
  }else range=raw[index1];
  
  iter_counter=0;
  *err=100000.0;
  double scaling;
  
  while(iter_counter<MAX_ITER){
    amoebaInit(&pp[0][0], yy, VARNUM+1, nvar, delfac, ptol2, a, da, 
        &SimplexFitting::funk, ptol); 
    amoeba(&pp[0][0], yy, VARNUM+1, nvar, ftol2, &SimplexFitting::funk,
        &iter, ptol, &jmin);
    for(i=0;i<nvar;i++) a[i]=pp[i][jmin];
    amoebaInit(&pp[0][0], yy, VARNUM+1, nvar, delfac, ptol1, a, da, 
        &SimplexFitting::funk, ptol); 
    amoeba(&pp[0][0], yy, VARNUM+1, nvar, ftol1, &SimplexFitting::funk,
        &iter, ptol, &jmin);

    for(i=0;i<nvar;i++) a[i]=pp[i][jmin];
    funk(a, &errmin);
    if( (errmin<*err || errmin<MIN_ERROR*range) && a[0]>0.0){
      *err=errmin;
      for(i=0;i<nvar;i++) min_a[i]=a[i];
    }

    iter_counter++; 
    if(errmin<MIN_ERROR*range && a[0]>0.0) break;

    //re-initialize parameters
    if(iter_counter%2) scaling=0.5*iter_counter/(MAX_ITER-1);
    else scaling=-0.5*iter_counter/(MAX_ITER-1);

    a[0]=(1+scaling)*raw[index1];
    if(howToInitParams==0) a[1]=0.0;
    else a[1]=0.5*(index2-index1)/(nDim-1); 
    a[2]=(1+scaling)*1.414*(index2-index1)/(nDim-1);
    a[3]=0.0;
  }// iteration
  
  for(i=0;i<nDim;i++){ 
    fitting[i]=min_a[0]*exp( -((float)(i-index1)/(nDim-1)-min_a[1])*
        ((float)(i-index1)/(nDim-1)-min_a[1])/(min_a[2]*min_a[2])) +min_a[3];
  } 

  printf("Iteration Num=%d threshold=%f Simplex fitting parameters for \
      range %d to %d are:\n", iter_counter, MIN_ERROR*range, index1, index2);
  printf("Fitting error=%f\t a[0]=%f\t a[1]=%f\t a[2]=%f\t a[3]=%f\n",*err,
      min_a[0], min_a[1], min_a[2], min_a[3]); 
  return 0; 
}

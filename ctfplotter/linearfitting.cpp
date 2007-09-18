#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "linearfitting.h"

LinearFitting::LinearFitting(int nRaw): nDim(nRaw) 
{
  raw=new double[nRaw];
}

LinearFitting::~LinearFitting()
{
  delete[] raw;
}

void LinearFitting::setRaw(double *rawData)
{
  for(int i=0;i<nDim;i++) raw[i]=rawData[i];
}


int LinearFitting::computeFitting(double *fitting, double *model, int nModel,
    int index1, int index2)
//The model is represented by an array of real numbers that are the 
//exponentials. For example, if model={0, 0.5, 1.0, 2.0}, then, 
//nModel would be 4; and y=c_0*x^0 + c_1*x^0.5 + c_2*x + c_3*x^2.0 . 
//index1 and index2 are the starting index and endind index of the 
//fitting range.
{
  if( index1>index2 || index1<0 || index2>(nDim-1) ) return 1;

  int M=index2-index1+1;
  int N=nModel;
  int i, j;
  double inc=1.0/(nDim-1);

  double *a=(double *)malloc(N*M*sizeof(double));
  double *aa=(double *)malloc(N*nDim*sizeof(double));
  for( i=0; i<N; i++){
    for(j=0; j<M; j++){
      *(a+i*M+j)=pow( (index1+j)*inc, model[i] );
    }
  }

  for( i=0; i<N; i++)
    for(j=0; j<nDim; j++){
      *(aa+i*nDim+j)=pow( j*inc, model[i] );
    }


  double *b=(double *)malloc(M*sizeof(double));
  for(i=0;i<M;i++) {
    b[i]=raw[i+index1]; 
  }
  for(i=0;i<nDim;i++)  fitting[i]=0.0;

  int NRHS=1;
  int LWORK=5*M*N;
  double *work=(double*)malloc(LWORK*sizeof(double));
  /*  = 0:  successful exit
  *   < 0:  if INFO = -i, the i-th argument had an illegal value.
  *   > 0:  the algorithm for computing the SVD failed to converge;
  *   if INFO = i, i off-diagonal elements of an intermediate
  *   bidiagonal form did not converge to zero.
  */
  int INFO=1000;
  double *sv=(double*)malloc(N*sizeof(double)); // store singular values
  //negative: so machine precision is used as minimun singular value;
  double RCOND=-5.0; 
  int RANK=-3;

  //Solve the minimum norm problem || b- A^T * X ||, 
  //M is the number of rows of A^T, the solution is stored in b.  
  dgelss(&M, &N, &NRHS, a, &M, b, &M, sv, &RCOND, &RANK, work, &LWORK, 
          &INFO);
  for(i=0;i<nDim;i++)
    for(j=0;j<N;j++) fitting[i]+=b[j]*(*(aa+j*nDim+i));

  printf("Linear fitting parameters for range %d to %d are:\n", index1, index2);
  for(i=0;i<N;i++) printf("x[%d]=%f\t", i, b[i]);
  printf("RANK=%d INFO=%d \n", RANK, INFO);
  free(a);
  free(aa);
  free(b);
  free(work);
  free(sv);
  return INFO; 
}

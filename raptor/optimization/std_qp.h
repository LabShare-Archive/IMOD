#ifndef std_qp_H_
#define std_qp_H_

//#include "sparseMat.h"
#include "../suitesparse/cs.h"
#include "../opencv/cv.h"
#include "STDQPdata.h"

//
CvMat* ZeroMat(int, int);
CvMat* OnesMat(int, int);
double MinElem(CvMat*);


//
//sparseMat* HessDiag(CvMat*);
//sparseMat* MakeHessian(double, sparseMat*, sparseMat*, CvMat*);
//CvMat* GradientHelper(sparseMat*, CvMat*);
//sparseMat* MakeGradient(double, sparseMat*, CvMat*, sparseMat*, CvMat*, sparseMat*);
//sparseMat* CombineMatrices(sparseMat*, sparseMat* b, sparseMat* c, sparseMat* d);
//sparseMat* Combinedx1dx2(CvMat*, sparseMat*);
CvMat* Combinedx1dx2(CvMat*, CvMat*);
double sumLog(CvMat*);
STDQPdata* std_qp(cs* _Q, cs* _c, cs* _A, CvMat* b, CvMat* x0, int M, int T, string option);
void LPsolver(cs* _c, cs* _A, CvMat* b, CvMat* x0);//solves LP optimization problems with log-barrier method and Newton method inside. Based on std_qp.cpp
//addition to handle sparse matrices from cs_suite
cs * GetSubMatrix(const cs *A,int xs, int xe, int ys, int ye);
double * GetSubMatrixDiag(const cs *A,int xs, int xe, int ys, int ye);
CvMat* GetSubMatrix(int xs, int xe, int ys, int ye,CvMat *dense);
CvMat* SparseDenseMult(cs *S,CvMat* dense);//result=S*dense
double dotProduct(CvMat *dense,cs *sparse);

void print(cs *A,ostream &out);
void print(CvMat *matrix,ostream &out);
#endif

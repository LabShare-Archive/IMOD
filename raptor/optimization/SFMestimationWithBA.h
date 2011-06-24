#ifndef SFMestimationWithBA_H_
#define SFMestimationWithBA_H_

#include <vector>
#include "../opencv/cv.h"
#include "estimation3d.h"
#include "contour.h"
#include "SFMdata.h"
#include "../suitesparse/cs.h"



using namespace std;

CvMat* Stack(CvMat*);
CvMat* MakeDiag(CvMat*);
CvMat* ConcatenateMatrix(CvMat*, CvMat*);
CvMat* ConcatenateMatrixDown(CvMat*, CvMat*);
double norm(CvMat*, int);
CvMat* OnesMat(int, int);

SFMdata* SFMestimationWithBA(SFMdata *sfm_, vector<double>, int, int, float, double *, int,bool debugMode=false);//famatnote: write variables here even if it is not necessary. It helps understanding the code

SFMdata* residAnalysis(SFMdata* sfm,bool debugMode);//analysis of the contours to detect and remove possible outliers

void writeIMODfidModel(SFMdata *sfm, int width, int height, int num_frames, string basename,ostream &out,vector<frame> *frames);
void writeIMODfidModelReproj(SFMdata *sfm, int width, int height, int num_frames, string basename,ostream &out,vector<frame> *frames);
SFMdata* decideAlphaOption(SFMdata *sfm,int *optionAlpha,vector<frame> *frames);
void decideTiltAlignOptions(SFMdata *sfm,int *tiltOption,int *rotOption,int *magOption);
#endif

#ifndef FILLCONTOURS_H_INCLUDED
#define FILLCONTOURS_H_INCLUDED

#include "../optimization/SFMdata.h"
#include "../opencv/cv.h"
#include "../mainClasses/ioMRCVol.h"

struct centroid
{
    double x,y,score;
};

void fillContours(SFMdata *sfm,ioMRC *vol,float **templ,int *templSize,bool debugMode);
SFMdata* joinSimilarContours(SFMdata *sfm);//sfm gets deleted inisde this method.
double distanceTrajectory(double *x1,double *y1,double *x2,double *y2,int length);//computes distance between two trajectories
void beadCenter(IplImage *patchIpl,IplImage *perfectMarkerTemplIpl,float *xx,float *yy,int mType);
void findMatchingTranslation(IplImage *imageIpl,IplImage *templIpl,double *x,double *y,double *score,int mType);


//I/o operations
void print(IplImage *im,ostream &out);
#endif // FILLCONTOURS_H_INCLUDED

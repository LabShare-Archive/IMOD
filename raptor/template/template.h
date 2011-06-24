#ifndef TEMPLATE_H
#define TEMPLATE_H

#include <vector>
#include "../mainClasses/ioMRCVol.h"
#include "../mainClasses/frame.h"
#include "../mainClasses/point2d.h"
#include "../mainClasses/ioMRCVol.h"
#include "../opencv/cxcore.h"
#include "../opencv/cv.h"
#include "../opencv/cv.h"

struct simplePoint2D
{
    int x, y;
    double score;
    simplePoint2D(int x, int y, double score)
    {
        this->x = x;
        this->y = y;
        this->score = score;
    }
};

float* createSyntheticTemplate(vector<frame>* frames, int diameter, int frameNumber, ioMRC* vol,bool white,int mType);
void computeNCC(vector<frame>* frames, float* templ, ioMRC* vol,int mType);
float* diffVector(float* vector, int length);
//void cropBorders(float* image, float* fv, int width, int height, int template_side);
vector<Point2D*> findPeaks(float* fv, int fv_width, int fv_height, float threshold, unsigned int maxMarkers, unsigned int minMarkers, int template_side, int frameID,int mType);
int nnz(float* matrix, int size);
vector<Point2D*> mergePeaks(vector<Point2D*> peak1_, vector<Point2D*> peak2_, int maxNMarkers);
bool simplepointcmp(simplePoint2D a, simplePoint2D b);
void mldivide(double*, double*, int, int, double*, int, int);
void findAllPeaks(vector<frame>* frames, float** templ, ioMRC* vol,int numDiffMarkerSize,bool xRay=false);
int estimateNumberOfMarkers(vector<frame>* frames, float** templ, ioMRC* vol,int numDiffMarkerSize);
bool processSimplePoint2D(vector<simplePoint2D>* potential_peaks, simplePoint2D p,unsigned  int maxMarkers,int mType);
bool withinDiameter(float x1, float y1, float x2, float y2,int diamNum);
bool isPeakInFrame(frame *frames,Point2D *p);
void findMin(vector<simplePoint2D>* potential_peaks);
void removeLinesXray(IplImage *image,float *mask);
#endif

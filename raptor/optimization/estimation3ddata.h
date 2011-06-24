#include "../opencv/cv.h"

#ifndef estimation3ddata_H_
#define estimation3ddata_H_

using namespace std;

class estimation3ddata {
  public:
  estimation3ddata(int, int);
  estimation3ddata();
  ~estimation3ddata();

    double residMeanPerc;
    double residMean;
    CvMat* G;
    CvMat* P;
    CvMat* t;

};

#endif

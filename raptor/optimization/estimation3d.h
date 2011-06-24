#ifndef estimation3d_H_
#define estimation3d_H_

#include "../opencv/cv.h"
#include "contour.h"
#include "estimation3ddata.h"
#include "probData.h"

using namespace std;



CvMat* CreateRandMat(int, int, double, double);
CvMat* repMat(CvMat*, int, int);

estimation3ddata* estimation3D(contour&, contour&, double, vector<double>, int, int, double, probData*, CvMat*, CvMat*);

//estimate* estimation3D(contour, contour, double, vector<double>, int, int, double, probStruct, CvMat*, CvMat*, double, int);


#endif

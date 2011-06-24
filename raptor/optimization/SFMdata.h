#ifndef SFMdata_H_
#define SFMdata_H_

#include "../opencv/cv.h"
#include "contour.h"



using namespace std;

class SFMdata {
  public:
  SFMdata(contour c_x,contour c_y,int* mType);
  SFMdata(contour c_x,contour c_y,contour r_x,contour r_y,int* mType);
  SFMdata();
  SFMdata(const SFMdata& p);
  ~SFMdata();
  SFMdata operator=(const SFMdata other);
  void clearPointers();

  contour *reproj_x;
  contour *reproj_y;
  contour *contour_x;
  contour *contour_y;
  int *markerType;//when we have different markers with different sizes we need to know each trajectory
};

#endif



#ifndef STDQPdata_H_
#define STDQPdata_H_

#include "../opencv/cv.h"
#include <vector>

using namespace std;

class STDQPdata {
  public:
  STDQPdata();
  ~STDQPdata();
  void clear();

  CvMat* answerMat;
  int numItrs;
  int exit_flag;
  double gap;
};

#endif

#ifndef PAIRCORRESPONDENCE_H
#define PAIRCORRESPONDENCE_H
#include "frame.h"

using namespace std;
class frame;
// Correspondence between a pair of points in two frames
class pairCorrespondence {
 public:
  pairCorrespondence(int point1_index, int point2_index, frame* frame1, frame* frame2, double score) {
    this->point1_index = point1_index;
    this->point2_index = point2_index;
    this->frame1 = frame1;
    this->frame2 = frame2;
    this->score = score;
};
  int point1_index;
  int point2_index;
  frame* frame1;
  frame* frame2;
  double score; // The score of correspondence between a pair of points
};



#endif

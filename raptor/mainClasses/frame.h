#ifndef FRAME_H
#define FRAME_H
#include "point2d.h"
#include <vector>
#include "../mainClasses/ioMRCVol.h"
using namespace std;
class Point2D;
class frame
{
 public:
  frame();
  frame(ioMRC* vol, int frameID, int width, int height);
  ioMRC* vol;
  vector<Point2D*> p; // the markers found in this frame
  int frameID;
  int width, height;
  bool discard;//if true indicates we should not use this frame
};

#endif

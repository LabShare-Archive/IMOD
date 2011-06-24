#ifndef contour_H
#define contour_H

#include "../opencv/cv.h"
#include "../trajectory/trajectory.h"
#include "../mainClasses/frame.h"

using namespace std;

class contour {
 public:
  contour();
  contour(int rows,int cols,int _xory);//initialize contour to zero
  contour(vector<trajectory>, int, int); //x = 1, y = 2
  contour(vector<trajectory>, int, int,vector<frame> *frames);
  contour(const contour&);
  contour(CvMat* cc,int _xory);
  ~contour();
  contour operator=(const contour other);
  int getNumTraj();
  int getNumFrame();
  int calculateNNZ();//computes Number of Non-Zeros present in a contour (useful to calculate if we have enough points in trajectories)

  //I/0 operations
  void print(ostream &out);
  vector<trajectory> contour2trajectory(contour c_x,contour c_y,vector<frame> *frames);

  CvMat* scores;//contains the values: for example contour_x contains x coordinates
  int xory;
  int numTraj;
  int numFrame;

  static const double minContourValue;
};


inline int contour::calculateNNZ()
{
    if(scores==NULL)
        return 0;
    int nnz=0;
    double *data=scores->data.db;
    for(int kk=0;kk<numTraj*numFrame;kk++)
        if(data[kk]>minContourValue) nnz++;

    return nnz;
}
#endif

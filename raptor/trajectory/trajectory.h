#ifndef TRAJECTORY_H
#define TRAJECTORY_H
#include "../mainClasses/point2d.h"
#include <vector>

class trajectory
{
public:
    trajectory();
    trajectory(trajectory* parent);
    trajectory(const trajectory &t);//famatnote: we need copy constructor for vectors and pointers
    ~trajectory();

    vector<Point2D*> p;
    trajectory* parent;
    vector<trajectory*> child;

};

//famatnote: why are these functions outside the class?
vector<trajectory> findTrajectories(vector<frame>* frames, vector< vector<pairCorrespondence> >* correspondences, int numFrames, unsigned int maxMarkersPrevFrame);
trajectory buildTrajectoryForward(Point2D* point, int,unsigned int);
void buildTrajectoryBackward(vector<trajectory>* t, vector<frame>*, int, int, unsigned int,unsigned int);
Point2D* findMarkerInPrevFrame(Point2D* cur, int jump);
Point2D* findMarkerInNextFrame(Point2D* cur, int jump, int);
void writeIMODfidModel(vector<trajectory> T, int width, int height, int num_frames, string basename,ostream &out);
void writeIMODtiltalign(double alpha,string path,string basename,ostream &out,vector<frame> *frames,int tiltOption,int rotOption,int MagOption);
void writeIMODtiltScript(int W,int H,string pathTilt,string pathAli,string basename,int recoThickness,vector<frame> *frames,string endingRec);
void writeMATLABcontour(string, int);
void freeTrajectoryVector(vector<trajectory> T);
vector<trajectory> fiducialModel2Trajectory(string fidTxtFilename);
int * getMarkerTypeFromTrajectory(vector<trajectory> T);
#endif

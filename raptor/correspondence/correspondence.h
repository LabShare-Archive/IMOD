#ifndef CORRESPONDENCE_H
#define CORRESPONDENCE_H
#include <vector>
#include "../mainClasses/frame.h"
#include "../mainClasses/stat.h"
#include "../mainClasses/ioMRCVol.h"
using namespace std;
void singlePotentials(float*, frame* frame1, frame* frame2, int win, int dmax, int mode, int option, ioMRC* vol, unsigned int maxMarkersPrevFrame_,unsigned int maxMarkersNextFrame_, vector<Point2D*> referencePeaks);
bool correspondenceRegions(frame* frame1, frame* frame2, vector<pairCorrespondence>* correspondences, int Ntemplate, unsigned int M, unsigned int K, int maxNCliques, int dmax, int Q1, int W1, int maxPWTable, int minPWTable, string idxStr, int diameter, ioMRC* vol, vector<pairCorrespondence>*, string outputDir,string binPATH);
#endif

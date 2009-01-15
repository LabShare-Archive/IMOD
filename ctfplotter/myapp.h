#ifndef MYAPP_H
#define MYAPP_H

#include <QApplication>
#include "defocusfinder.h"
#include "mrcslice.h"
#include "slicecache.h"

class SimplexFitting;
class LinearFitting;
class Plotter;

class MyApp : public QApplication
{
  Q_OBJECT
  public:
    SimplexFitting* simplexEngine;
    LinearFitting*  linearEngine;
    DefocusFinder   defocusFinder;
    int getEndingSliceNum() { return endingSlice;}
    int getStartingSliceNum() {return startingSlice;}
    void plotFitPS(bool flagSetInitSetting );
    void setPlotter( Plotter *p){ plotter=p;}
    void setSlice(char *stackFile, char *angleFile);
    double getLowAngle() {return lowAngle;}
    char *getStackName() {return fnStack;}
    void setStackMean(double mean){ stackMean=mean;}
    double getStackMean(){ return stackMean;}
    double getHighAngle(){return highAngle;}
    double getDefocusTol(){return defocusTol;}
    double getLeftTol(){return leftDefTol;}
    double getRightTol(){return rightDefTol;}
    int getTileSize(){return tileSize;}
    double getAxisAngle(){return tiltAxisAngle;}
    void setLowAngle(double lAngle){ lowAngle=lAngle;}
    void setHighAngle(double hAngle){ highAngle=hAngle;}
    void setPS(double *rAvg){rAverage=rAvg;}
    double* getPS(){return rAverage;}
    //recompute rAverage for the central strips after calling setSlice();
    int computeInitPS();  
    void setNoisePS(double *high,  double *low) {  highPs=high; lowPs=low;   }
    void setNoiseMean(double high,  double low) {  highMean=high; lowMean=low; }
    void setX1Range(int n1, int n2){x1Idx1=n1;x1Idx2=n2;}
    int getX1RangeLow(){return x1Idx1;}
    int getX1RangeHigh(){return x1Idx2;}
    void setX2Range(int n1, int n2){x2Idx1=n1;x2Idx2=n2;}
    int getX2RangeLow(){return x2Idx1;}
    int getX2RangeHigh(){return x2Idx2;}
    int getDim(){return nDim;}
    char *getDefFn(){return fnDefocus;}
    void setSaveFp(FILE *fp){saveFp=fp;}
    void saveAllPs();
    FILE *getSaveFp(){return saveFp;}
    MyApp(int &argc, char *argv[], int volt, double pixelSize, 
        double ampContrast, float cs, char *fnDefocus, 
        int dim, double focusTol, int tSize, 
        double tiltAxisAngle, double lAngle, double hAngle, double expDefocus,
        double leftDefTol, double rightDefTol, int maxCacheSize);
    ~MyApp();
 public slots:
    void rangeChanged(double x1, double x2, double, double);
    void angleChanged(double lowAngle, double highAngle, double defocus, 
      double defTol,int tSize,double axisAngle,double leftTol,double rightTol);
    void moreTile(bool hasIncludedCentralTiles);
    void moreTileCenterIncluded();
    void setX1Method(int index){x1MethodIndex=index;}
    int getX1Method(){return x1MethodIndex;}
    void setX2Method(int index){x2MethodIndex=index;}
    int getX2Method(){return x2MethodIndex;}
    void setDefOption(int index){defocusOption=index;}
    void setInitTileOption(int index);
 private:
    //declare as static so member functions can use resizable
    // arrays of size 'nDim';
    static int nDim;  
    static int tileSize;
    Plotter *plotter;
    char *fnStack;
    char *fnAngle;
    char *fnDefocus;
    double lowAngle;  // in degrees;
    double highAngle;  // in degrees;
    double leftDefTol; // in nm;
    double rightDefTol; //in nm;
    int nxx;  //x dimension;
    int nyy; //y dimension;
    int nzz;
    double defocusTol; // in nm;
    double pixelSize;  //in nm;
    int voltage; // in Kv;
    SliceCache cache;
    // x-direction tile numumber already included in PS computation;
    int *tileIncluded; 
    int totalTileIncluded;
    double *rAverage; // is the signal PS;
    double *highPs;  
    double highMean;
    double *lowPs;
    double lowMean;
    double stackMean;
    double tiltAxisAngle; //in degrees
    int itrNum;
    int x1MethodIndex;
    int x2MethodIndex;
    int defocusOption;
    int initialTileOption;
    int x1Idx1;
    int x1Idx2;
    int x2Idx1;
    int x2Idx2;
    FILE *saveFp;
    int endingSlice;
    int startingSlice;
};

#endif

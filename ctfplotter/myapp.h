/*
 * myapp.h - the QApplication class for ctfplotter.
 *
 *  $Id$
 *
 *  $Log$
 *  Revision 1.6  2009/08/10 22:34:39  mast
 *  General reworking of program
 *
 *
 */
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
    int getEndingSliceNum() { return mEndingSlice;}
    int getStartingSliceNum() {return mStartingSlice;}
    void plotFitPS(bool flagSetInitSetting );
    void fitPsFindZero();
    void setPlotter( Plotter *p){ mPlotter=p;}
    void setSlice(const char *stackFile, char *angleFile);
    double getLowAngle() {return mLowAngle;}
    char *getStackName() {return mFnStack;}
    void setStackMean(double mean){ mStackMean=mean;}
    double getStackMean(){ return mStackMean;}
    double getHighAngle(){return mHighAngle;}
    double getDefocusTol(){return mDefocusTol;}
    double getLeftTol(){return mLeftDefTol;}
    double getRightTol(){return mRightDefTol;}
    int getTileSize(){return mTileSize;}
    double getAxisAngle(){return mTiltAxisAngle;}
    void setLowAngle(double lAngle){ mLowAngle=lAngle;}
    void setHighAngle(double hAngle){ mHighAngle=hAngle;}
    void setPS(double *rAvg){mRAverage=rAvg;}
    double* getPS(){return mRAverage;}
    //recompute mRAverage for the central strips after calling setSlice();
    int computeInitPS();  
    void setNoiseForMean(double mean);
    void setNumNoiseFiles(int num) {mNumNoiseFiles = num;};
    void setNoiseIndexes(int *indexes) {mNoiseIndexes = indexes;};
    void setNoiseMeans(double *means) {mNoiseMeans = means;};
    void setAllNoisePS(double *ps) {mAllNoisePS = ps;};
    void setX1Range(int n1, int n2){mX1Idx1=n1;mX1Idx2=n2;}
    int getX1RangeLow(){return mX1Idx1;}
    int getX1RangeHigh(){return mX1Idx2;}
    void setX2Range(int n1, int n2){mX2Idx1=n1;mX2Idx2=n2;}
    int getX2RangeLow(){return mX2Idx1;}
    int getX2RangeHigh(){return mX2Idx2;}
    int getDim(){return mDim;}
    char *getDefFn(){return mFnDefocus;}
    void setSaveFp(FILE *fp){mSaveFp=fp;}
    void saveAllPs();
    FILE *getSaveFp(){return mSaveFp;}
    int getX1Method(){return mX1MethodIndex;}
    int getX2Method(){return mX2MethodIndex;}
    int getZeroFitMethod(){return mZeroFitMethod;}
    int getPolynomialOrder(){return mPolynomialOrder;}
    bool getVaryCtfPowerInFit(){return mVaryCtfPowerInFit;}
    MyApp(int &argc, char *argv[], int volt, double pSize, 
          double ampRatio, float cs, char *defFn, int dim, int hyper, 
          double focusTol, int tSize, double tAxisAngle, double lAngle,
          double hAngle, double expDefocus, double leftTol, 
          double rightTol, int maxCacheSize, int invertAngles);
    ~MyApp();
 public slots:
    void rangeChanged(double x1, double x2, double, double);
    void angleChanged(double mLowAngle, double mHighAngle, double defocus, 
      double defTol,int tSize,double axisAngle,double leftTol,double rightTol);
    void moreTile(bool hasIncludedCentralTiles);
    void moreTileCenterIncluded();
    void setX1Method(int index){mX1MethodIndex=index;}
    void setX2Method(int index){mX2MethodIndex=index;}
    void setZeroFitMethod(int index){mZeroFitMethod=index;}
    void setPolynomialOrder(int index){mPolynomialOrder=index;}
    void setVaryCtfPowerInFit(bool value){mVaryCtfPowerInFit=value;}
    void setDefOption(int index){mDefocusOption=index;}
    void setInitTileOption(int index);
    void scaleAndAddStrip
      (double *psSum, double *stripAvg, int *stripCounter,
       int counter, double mean, double xScale, 
       double xAdd, float freqInc, double delFmin, double delFmax);
 private:
    //declare as static so member functions can use resizable
    // arrays of size 'mDim';
    static int mDim;  
    static int mTileSize;
    int mHyperRes;
    Plotter *mPlotter;
    char *mFnStack;
    char *mFnAngle;
    char *mFnDefocus;
    double mLowAngle;  // in degrees;
    double mHighAngle;  // in degrees;
    double mLeftDefTol; // in nm;
    double mRightDefTol; //in nm;
    int mNxx;  //x dimension;
    int mNyy; //y dimension;
    int mNzz;
    double mDefocusTol; // in nm;
    double mPixelSize;  //in nm;
    int mVoltage; // in Kv;
    SliceCache mCache;
    // x-direction tile numumber already included in PS computation;
    int *mTileIncluded; 
    int mTotalTileIncluded;
    double *mRAverage; // is the signal PS;
    int *mFreqTileCounter;  // Count of tiles * pixels in each bin of PS
    double mStackMean;
    double mTiltAxisAngle; //in degrees
    int mItrNum;
    int mX1MethodIndex;
    int mX2MethodIndex;
    int mZeroFitMethod;
    bool mVaryCtfPowerInFit;
    int mPolynomialOrder;
    int mDefocusOption;
    int mInitialTileOption;
    int mX1Idx1;
    int mX1Idx2;
    int mX2Idx1;
    int mX2Idx2;
    FILE *mSaveFp;
    int mEndingSlice;
    int mStartingSlice;
    int mNumSlicesDone;
    int mNumNoiseFiles;
    int *mNoiseIndexes;
    double *mNoiseMeans;
    double *mAllNoisePS;
    double *mNoisePS;
};

#endif

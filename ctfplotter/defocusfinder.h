/*
 * defocusfinder.h - Header for DefocusFinder class
 *
 *  $Id$
 *
 *  $Log$
 *
 */
#ifndef DEFOCUSFINDER_H
#define DEFOCUSFINDER_H

class DefocusFinder
{
 public:
  DefocusFinder(int volt, double pSize, double ampContrast, double inputCs, 
                int dim, double expDef);
  int findZero(const double* simplexFitting, const double* linearFitting, 
               int x1, int x2, double* zero);
  int findDefocus(double *focus);
  double defocusFromSecondZero(double zero);
  double getZero(){return mZeroCrossing;}
  double getExpZero(){return mExpZero;}
  void setZero( double zero){ mZeroCrossing=zero;}
  double getDefocus(){ return mDefocus;}
  double getAvgDefocus(){ return mAvgDefocus;}
  double getExpDefocus(){return mExpDefocus;}
  void setDefocus(double def){mDefocus=def;}
  void setAvgDefocus(double def){mAvgDefocus=def;}
  void setExpDefocus(double expDef);
  void getTwoZeros(double focus, double &firstZero, double &secondZero);
  double CTFvalue(double freq, double def);
  double mWavelength;
  double mCsOne;
  double mCsTwo;
  
 private:
  int mVoltage;
  double mPixelSize;
  double mAmpRatio;
  double mCs;
  double mZeroCrossing;
  double mExpZero;
  int mDim;
  double mDefocus;
  double mExpDefocus;
  double mAmpAngle;
  double mAvgDefocus;
};

#endif

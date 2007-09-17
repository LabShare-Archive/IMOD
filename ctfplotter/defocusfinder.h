#ifndef DEFOCUSFINDER_H
#define DEFOCUSFINDER_H

class DefocusFinder{
  public:
  DefocusFinder(int volt, double pixelSize, double ampContrast, double cs,
      int nDim, double expDefocus);
  int findZero(const double* simplexFitting, const double* linearFitting, 
      int x1, int x2, double* zeroCrossing);
  int findDefocus(double *defocus);
  double getZero(){return zeroCrossing;}
  double getExpZero(){return expZero;}
  void setZero( double zero){ zeroCrossing=zero;}
  double getDefocus(){ return defocus;}
  double getExpDefocus(){return expDefocus;}
  void setDefocus(double def){defocus=def;}
  void setExpDefocus(double expDef);
  double wavelength;
  double csOne;
  double csTwo;

  private:
      int voltage;
      double pixelSize;
      double ampRatio;
      double cs;
      double zeroCrossing;
      double expZero;
      int nDim;
      double defocus;
      double expDefocus;
};

#endif

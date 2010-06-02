/*   cfsemshare.h   - functions in multiple files, mostly shared between C and
 *                      Fortran and/or IMOD and SerialEM
 *
 *   Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 *   Log at end
 */                                                                           

#ifndef CFSEMSHARE_H
#define CFSEMSHARE_H

#include "mrcslice.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* parselist.c  - for parsing a list of integers */
  int *parselist (const char *line, int *nlist);

  /* amoeba.c - simplex minimization routine */
  void amoeba(float *p, float *y, int mp, int ndim, float ftol, 
              void (*funk)(float *, float *), int *iterP, float *ptol,
              int *iloP);
  void amoebaInit(float *p, float *y, int mp, int ndim, float delfac, 
                  float ptolFac, float *a, float *da, 
                  void (*funk)(float *, float *), float *ptol);

  /* samplemeansd.c - for computing mean and SD quickly by sampling */
  int sampleMeanSD(unsigned char **image, int type, int nx, int ny,
                   float sample, int nxMatt, int myMatt, int nxUse, int nyUse,
                   float *mean, float *sd);

  /* colormap.c */
  int *cmapStandardRamp(void);
  int *cmapInvertedRamp(void);
  int cmapConvertRamp(int *rampData, unsigned char table[3][256]);
  int cmapReadConvert(char *filename, unsigned char table[3][256]);

  /* cubinterp.c */
  void cubinterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
                 float amat[2][2], float xc, float yc, float xt, float yt,
                 float scale, float dmean, int linear);

  /* reduce_by_binning.c */
  int reduceByBinning(void *array, int type, int nxin, int nyin, int nbin, 
                      void *brray, int keepByte, int *nxr, int *nyr);

  /* filtxcorr.c */
  void XCorrSetCTF(float sigma1, float sigma2, float radius1, float radius2,
                   float *ctf, int nx, int ny, float *delta);
  void XCorrSetCTFnoScl(float sigma1, float sigma2, float radius1,
                        float radius2, float *ctf, int nx,int ny,
                        float *delta, int *nsizeOut);
  void XCorrFilterPart(float *fft, float *array, int nx, int ny, float *ctf, 
                       float delta);
  void XCorrMeanZero(float *array, int nxdim, int nx, int ny);
  void XCorrPeakFind(float *array, int nxdim, int ny, float  *xpeak,
                     float *ypeak, float *peak, int maxpeaks);
  double parabolicFitPosition(float y1, float y2, float y3);
  void conjugateProduct(float *array, float *brray, int nx, int ny);
  double XCorrCCCoefficient(float *array, float *brray, int nxdim, int nx,
                            int ny, float xpeak, float ypeak, int nxpad,
                            int nypad, int *nsum);

  /* taperpad.c */
  void sliceTaperOutPad(void *array, int type, int nxbox, int nybox, 
                        float *brray, int nxdim, int nx, int ny, int ifmean,
                        float dmeanin);
  void sliceTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                       int iy0, int iy1, float *brray, int nxdim, int nx,
                       int ny, int nxtap, int nytap);
  double sliceEdgeMean(float *array, int nxdim, int ixlo, int ixhi, int iylo,
                       int iyhi);
  void sliceSplitFill(float *array, int nxbox, int nybox, float *brray,
                      int nxdim, int nx, int ny, int iffill, float fillin);
  void sliceSmoothOutPad(void *array, int type, int nxbox, int nybox, 
                           float *brray, int nxdim, int nx, int ny);

  /* taperatfill.c */
  int sliceTaperAtFill(Islice *sl, int ntaper, int inside);

  /* circlefit.c */
  int circleThrough3Pts(float x1, float y1, float x2, float y2, float x3, 
                             float y3, float *rad, float *xc, float *yc);
  int fitSphere(float *xpt, float *ypt, float *zpt, int numPts,
                     float *rad, float *xcen, float *ycen, float *zcen,
                     float *rmsErr);

  /* insidecontour.c */
  int InsideContour(float *ptX, float *ptY, int np, float x, float y);

  /* scaledsobel.c */
  int scaledSobel(float *inImage, int nxin, int nyin, float scaleFac, 
                  float minInterp, int linear, float center, float *outImage,
                  int *nxout, int *nyout, float *xOffset, float *yOffset);

  /* histogram.c */
  void kernelHistogram(float *values, int numVals, float *bins, int numBins,
                       float firstVal, float lastVal, float h, int verbose);
  int scanHistogram(float *bins, int numBins, float firstVal, float lastVal,
                    float scanBot, float scanTop, int findPeaks, float *dip,
                    float *peakBelow, float *peakAbove);
  int findHistogramDip(float *values, int numVals, int minGuess, float *bins,
                       int numBins, float firstVal, float lastVal, 
                       float *histDip, float *peakBelow, float *peakAbove,
                       int verbose);
  /* simplestat.c */
  void avgSD(float *x, int n, float *avg, float *sd, float *sem);
  void sumsToAvgSD(float sx, float sxsq, int n, float *avg, float *sd);
  void sumsToAvgSDdbl(double sx8, double sxsq8, int n1, int n2, float *avg,
                      float *sd);
  void lsFit(float *x, float *y, int num, float *slope, float *intcp,
             float *ro);
  void lsFitPred(float *x, float *y, int n, float *slope, float *bint,
                 float *ro, float *sa, float *sb, float *se,
                 float xpred, float *ypred, float *prederr);
  void lsFit2(float *x1, float *x2, float *y, int n, float *a, float *b,
              float *c);
  void lsFit2Pred(float *x1, float *x2, float *y, int n, float *a, float *b, 
                  float *c, float x1pred, float x2pred, float *ypred,
                  float *prederr);
  void lsFit3(float *x1, float *x2, float *x3, float *y, int n, float *a1, 
              float *a2, float *a3, float *c);

  /* robuststat.c */
  void rsSortFloats(float *x, int n);
  void rsSortIndexedFloats(float *x, int *index, int n);
  void rsMedianOfSorted(float *x, int n, float *median);
  void rsMedian(float *x, int n, float *tmp, float *median);
  void rsMADN(float *x, int n, float median, float *tmp, float *MADN);
  void rsMadMedianOutliers(float *x, int n, float kcrit, float *out);

  /* amat_to_rotamgstr.c */
  void amatToRotmagstr(float a11, float a12, float a21, float a22, 
                         float *theta, float *smag, float *str, float *phi);

  /* percentile.c */
  float percentileFloat(int s, float *r, int num);
  int percentileInt(int s, int *r, int num);

  /* convexbound.c */
  void convexBound(float *sx, float *syin, int npnts, float fracomit,
                   float pad, float *bx, float *by, int *nvert, float *xcen,
                   float *ycen, int maxverts);

  /* beadfind.c */
  void makeModelBead(int boxSize, float beadSize, float *array);
  double beadIntegral(float *array, int nxdim, int nx, int ny, float rCenter,
                      float rInner, float rOuter, float xcen, float ycen,
                      float *cenmean, float *annmean, float *temp, 
                      float annPct, float *median);

  /* parallelwrite.c */
  int parWrtInitialize(char *filename, int nxin, int nyin);
  int parWrtProperties(int *allSec, int *linesBound, int *nfiles);
  int parWrtFindRegion(int secNum, int lineNum, int nlWrite, char **filename, 
                       int *sections, int *startLines);

  /* statfuncs.f */
  double tValue(double signif, int ndf);
  double fValue(double signif, int ndf1, int ndf2);
  double errFunc(double x);
  double incompBeta(double a, double b, double x);
  double betaFunc(double p, double q);
  double gammaFunc(double x);
  double lnGamma(double x);

  /* surfacesort.c */
  int surfaceSort(float *xyz, int numPts, float *xrot, float *yrot,
                  float *zrot, int *group);
  int setSurfSortParam(int which, float value);


#ifdef __cplusplus
}
#endif


#endif

/*

$Log$
Revision 3.17  2010/01/21 03:48:15  mast
Added statfuncs

Revision 3.16  2009/11/28 20:10:20  mast
Added convexbound and indexed sort

Revision 3.15  2009/11/21 21:16:24  mast
Added robuststat

Revision 3.14  2009/02/16 06:16:12  mast
Add parallel write routines

Revision 3.13  2009/01/02 05:19:19  mast
const char * for Qt 4 port

Revision 3.12  2008/12/22 23:02:30  mast
Added return of nsum to ccc function

Revision 3.11  2008/12/21 18:28:17  mast
Add ccc routine

Revision 3.10  2008/12/20 05:32:46  mast
Added smooth pad function

Revision 3.9  2008/12/01 15:31:01  mast
More functions

Revision 3.8  2008/11/18 22:44:06  mast
Added amat_to_rotmagstr

Revision 3.7  2008/11/14 19:59:23  mast
Added simplestat functions

Revision 3.6  2008/11/12 03:41:42  mast
Added histogram functions

Revision 3.5  2008/06/24 04:44:11  mast
Added taper function

Revision 3.4  2007/10/11 16:47:23  mast
Added edge mean function

Revision 3.3  2007/10/10 18:54:52  mast
Functions callable by fortran must return double not float!

Revision 3.2  2007/10/04 16:24:29  mast
Added parabolic fit function to do it right everywhere

Revision 3.1  2007/10/01 15:27:13  mast
Split b3dutil.h, this one goes to SerialEM


*/

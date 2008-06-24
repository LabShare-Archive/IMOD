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
  int *parselist (char *line, int *nlist);

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

  /* taperpad.c */
  void sliceTaperOutPad(void *array, int type, int nxbox, int nybox, 
                        float *brray, int nxdim, int nx, int ny, int ifmean,
                        float dmeanin);
  void sliceTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                       int iy0, int iy1, float *brray, int nxdim, int nx,
                       int ny, int nxtap, int nytap);
  double sliceEdgeMean(float *array, int nxdim, int ixlo, int ixhi, int iylo,
                       int iyhi);

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


#ifdef __cplusplus
}
#endif


#endif

/*
$Log$
Revision 3.4  2007/10/11 16:47:23  mast
Added edge mean function

Revision 3.3  2007/10/10 18:54:52  mast
Functions callable by fortran must return double not float!

Revision 3.2  2007/10/04 16:24:29  mast
Added parabolic fit function to do it right everywhere

Revision 3.1  2007/10/01 15:27:13  mast
Split b3dutil.h, this one goes to SerialEM


*/

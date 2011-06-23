/* warpfiles.h - include file for using libwarp routines
 * 
 * $Id$ 
 */

#ifndef WARPINGIO_H
#define WARPINGIO_H

#define WARP_INVERSE     1
#define WARP_CONTROL_PTS 2

#ifdef __cplusplus
extern "C" {
#endif

  int newWarpFile(int nx, int ny, int binning, float pixelSize, int flags);
  int setCurrentWarpFile(int index);
  int clearWarpFile(int index);
  void warpFilesDone();
  int getWarpFileSize(int *nx, int *ny, int *nz, int *ifControl);
  int setLinearTransform(int iz, float *xform, int rows);
  int setWarpGrid(int iz, int nxGrid, int nyGrid, float xStart, float yStart,
                  float xInterval, float yInterval, float *dxGrid, float *dyGrid,
                  int xdim);
  int setWarpPoints(int iz, int nControl, float *xControl, float *yControl, 
                    float *xVector, float *yVector);
  int addWarpPoint(int iz, float xControl, float yControl, float xVector, float yVector);
  int removeWarpPoint(int iz, int index);
  int getLinearTransform(int iz, float *xform, int rows);
  int getNumWarpPoints(int iz, int *nControl);
  int getWarpPoints(int iz, float *xControl, float *yControl, float *xVector,
                    float *yVector);
  int getWarpPointArrays(int iz, float **xControl, float **yControl, float **xVector,
                         float **yVector);
  int getWarpGridSize(int iz, int *nxMax, int *nyMax, int *prodMax);
  int setGridSizeToMake(int iz, int nxGrid, int nyGrid, float xStart, float yStart, 
                        float xInterval, float yInterval);
  int controlPointRange(int iz, float *xmin, float *xmax, float *ymin, float *ymax);
  int controlPointSpacing(int iz, float percentile, float *spacing);
  int gridSizeFromSpacing(int iz, float percentile, float factor, int fullExtent);
  int getGridParameters(int iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart,
                        float *xInterval, float *yInterval);
  int getWarpGrid(int iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart, 
                  float *xInterval, float *yInterval, float *dxGrid, float *dyGrid, 
                  int xdim);
  int separateLinearTransform(int iz);
  int readWarpFile(char *filename, int *nx, int *ny, int *nz, int *binning, 
                   float *pixelSize, int *version, int *flags);
  int writeWarpFile(const char *filename, int skipBackup);
  
  /* From warputils.c  */
  void interpolateGrid(float x, float y, float *dxGrid, float *dyGrid, int ixgDim,
                        int nxGrid, int nyGrid, float xGridStart, float yGridStart,
                        float xGridIntrv, float yGridIntrv, float *dx, float *dy);
  int extrapolateGrid(float *dxGrid, float *dyGrid, char *solved, int xdim,
                      int nxGrid, int nyGrid, float xInterval, float yInterval,
                      int reuse);
  void extrapolateDone();

  int extractLinearXform(float *xPos, float *yPos, float *xVector, float *yVector,
                         int nPoints, float xcen, float ycen, float *newXvec, 
                         float *newYvec, float *xform, int rows);
  int multiplyWarpings(float *dxGrid1, float *dyGrid1, int ixgDim1, int nxGrid1, 
                       int nyGrid1, float xStart1, float yStart1, float xIntrv1, 
                       float yIntrv1, float *xform1, float xcen, float ycen, 
                       float *dxGrid2, float *dyGrid2, int ixgDim2, int nxGrid2,
                       int nyGrid2, float xStart2, float yStart2, float xIntrv2, 
                       float yIntrv2, float *xform2, float *dxProd,
                       float *dyProd, float *xfProd, int useSecond, int rows);
  void invertWarpGrid(float *dxGrid, float *dyGrid, int ixgDim, int nxGrid, int nyGrid,
                      float xGridStart, float yGridStart, float xGridIntrv, 
                      float yGridIntrv, float *xform, float xcen, float ycen, 
                      float *dxInv, float *dyInv, float *xfInv, int rows);
  void findInversePoint(float x, float y, float *dxGrid, float *dyGrid, int ixgDim,
                        int nxGrid, int nyGrid, float xGridStart, float yGridStart, 
                        float xGridIntrv, float yGridIntrv, float *xnew, float *ynew,
                        float *dx, float *dy);
  int expandAndExtrapGrid(float *dxGrid, float *dyGrid, int xdim, int ydim, int *nxGrid,
                          int *nyGrid, float *xStart, float *yStart, float xInterval, 
                          float yInterval, float xBigStr, float yBigStr, float xBigEnd,
                          float yBigEnd, int ixmin, int ixmax, int iymin, int iymax);
  int readCheckWarpFile(char *filename, int needDist, int needInv, int *nx, int *ny, 
                        int *nz, int *ibinning, float *pixelSize, int *iflags, 
                        char *errString, int lenString);
  int findMaxGridSize(float xnbig, float ynbig, float xOffset, float yOffset,
                      int *nControl, int *maxNxg, int *maxNyg, char *errString,
                      int lenString);
  int getSizeAdjustedGrid(int iz, float xnbig, float ynbig, float xOffset, float yOffset,
                          int adjustStart, float warpScale, int iBinning, int *nxGrid,
                          int *nyGrid, float *xGridStrt, float *yGridStrt,
                          float *xGridIntrv, float *yGridIntrv, float *fieldDx,
                          float *fieldDy, int ixgdim, int iygdim, char *errString,
                          int lenString);

  /* warpinterp.c */
  void warpInterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
                  float amat[2][2], float xc, float yc, float xt, float yt, float scale,
                  float dmean, int linear, int linFirst, float *dxGrid, float *dyGrid,
                  int ixgDim, int nxGrid, int nyGrid, float xGridStrt, float yGridStrt,
                  float xGridIntrv, float yGridIntrv);

  /* maggradfield.c */
  void magGradientShift(float xx, float yy, int imageNx, int imageNy, float xcen, 
                        float ycen, float pixelSize, float axisRot, float tilt,
                        float dmagPerUm, float rotPerUm, float *dx, float *dy);
  void addMagGradField(float *idfDx, float *idfDy, float *gradDx, float *gradDy, 
                       int lmGrid, int imageNx, int imageNy, int nxGrid, int nyGrid,
                       float xGridStrt, float yGridStrt, float xGridIntrv, 
                       float yGridIntrv, float xcen, float ycen, float pixelSize,
                       float axisRot, float tilt, float dmagPerUm, float rotPerUm);
  void makeMagGradField(float *idfDx, float *idfDy, float *gradDx, float *gradDy, 
                        int lmGrid, int imageNx, int imageNy, int *nxGrid, int *nyGrid,
                        float *xGridStrt, float *yGridStrt, float *xGridIntrv, 
                        float *yGridIntrv, float xcen, float ycen, float pixelSize,
                        float axisRot, float tilt, float dmagPerUm, float rotPerUm);
    

#ifdef __cplusplus
}
#endif

#endif

#ifndef IMOD_XCORR_H
#define IMOD_XCORR_H
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.1  2004/11/07 22:59:09  mast
    Initial creation

*/

#include "mrcslice.h"

#ifdef __cplusplus
extern "C" {
#endif
  
float sliceByteBinnedFFT(Islice *sin, int binning, int ix0, int ix1, int iy0,
                         int iy1);
int sliceFourierFilter(Islice *sin, float sigma1, float sigma2, float radius1,
                       float radius2);
void XCorrTaperOutPad(void *array, int type, int nxbox, int nybox, 
                      float *brray, int nxdim, int nx, int ny);
void XCorrTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                     int iy0, int iy1, float *brray, int nxdim, int nx, int ny,
                     int nxtap, int nytap);
void XCorrExtractConvert(float *array, int nxdim, int ixlo, int iylo,
                         void *brray, int type, int nx, int ny,
                         float outMin, float outMax);
int XCorrNiceFrame(int num, int idnum, int limit);
void XCorrSetCTF(float sigma1, float sigma2, float radius1, float radius2,
                 float *ctf, int nx,int ny, float *delta);
void XCorrFilterPart(float *fft, float *array, int nx, int ny, float *ctf, 
                     float delta);

#ifdef __cplusplus
}
#endif

#endif

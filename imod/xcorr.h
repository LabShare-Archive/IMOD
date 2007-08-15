#ifndef IMOD_XCORR_H
#define IMOD_XCORR_H
/*  xcorr.h  -  Declarations for xcorr.cpp
 *
 *  $Id$
 *  Log at end
 */

#include "mrcslice.h"

#ifdef __cplusplus
extern "C" {
#endif
  
float sliceByteBinnedFFT(Islice *sin, int binning, int ix0, int ix1, int iy0,
                         int iy1, int *xcen, int *ycen);
int sliceFourierFilter(Islice *sin, float sigma1, float sigma2, float radius1,
                       float radius2);
void XCorrTaperOutPad(void *array, int type, int nxbox, int nybox, 
                      float *brray, int nxdim, int nx, int ny);
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
/*
$Log$
Revision 1.3  2006/06/24 16:03:33  mast
Added arguments to return center coordinate of FFT display

Revision 1.2  2004/11/11 15:55:34  mast
Changes to do FFT in a subarea

Revision 1.1  2004/11/07 22:59:09  mast
Initial creation

*/

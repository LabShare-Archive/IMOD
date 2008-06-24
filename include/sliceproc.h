/*  sliceproc.h: include file for slice processing functions
 *
 *  $Id$
 *  Log at end
 */
#ifndef IMOD_SLICEPROC_H
#define IMOD_SLICEPROC_H

#include "mrcslice.h"

enum {ANISO_CLEAR_AT_END, ANISO_CLEAR_ONLY, ANISO_LEAVE_OPEN};

#ifdef __cplusplus
extern "C" {
#endif

  int sliceByteConvolve(Islice *sin, int mask[3][3]);
  int sliceByteAdd(Islice *sin, int inVal);
  int sliceByteEdgeTwo(Islice *sin, int center);
  int sliceByteEdgeSobel(Islice *sin);
  int sliceByteEdgePrewitt(Islice *sin);
  int sliceByteEdgeLaplacian(Islice *sin);
  int sliceByteSharpen(Islice *sin);
  int sliceByteSmooth(Islice *sin);
  int sliceByteConvolve(Islice *sin, int mask[3][3]);
  int sliceByteThreshold(Islice *sin, int val);
  int sliceByteGrow(Islice *sin, int val);
  int sliceByteShrink(Islice *sin, int val);
  int sliceByteGraham(Islice *sin);
  int sliceMinMax(Islice *s);
  int sliceMedianFilter(Islice *sout, struct MRCvolume *v, int size);
  void updateMatrix(float **image, float **imageOld, int m, int n,
                    int CC, double k, double lambda);
  int sliceAnisoDiff(Islice *sl,  int outMode, int CC, double k, double lambda,
                     int iterations, int clearFlag);
  float **allocate2D_float(int m, int n );
  void sliceByteAnisoDiff(Islice *sl, float **image, float **image2, int CC,
                          double k, double lambda, int iterations, 
                          int *iterDone);
  int niceFrame(int num, int idnum, int limit);
  void sliceScaleAndFree(Islice *sout, Islice *sin);


#ifdef __cplusplus
}
#endif

#endif
/*
  $Log$
  Revision 3.11  2007/11/22 20:46:30  mast
  Add gaussian kernel functions

  Revision 3.10  2007/10/01 15:27:23  mast
  Rearranging

  Revision 3.9  2007/09/14 05:24:24  mast
  Added niceFrame

  Revision 3.8  2007/08/16 16:44:37  mast
  Eliminated comment marks here

  Revision 3.7  2007/08/15 00:06:46  mast
  Added taperinpad function, renamed from xcorr

  Revision 3.6  2007/05/27 21:18:24  mast
  Added taper routine
  
  Revision 3.5  2005/03/31 23:45:57  mast
  Fixed declaration of updateMatrix

  Revision 3.4  2005/03/09 21:17:25  mast
  Converted diffusion to float, removed processor argument
  
  Revision 3.3  2005/01/28 05:40:45  mast
  Needed separate byte routine for anisotropic diffusion
  
  Revision 3.2  2005/01/27 05:57:05  mast
  Added anisotropic diffusion
  
  Revision 3.1  2005/01/07 20:01:17  mast
  Moved to libiimod so putthis file in include
  
  Revision 3.4  2004/12/22 15:21:15  mast
  Fixed problems discovered with Visual C compiler
  
  Revision 3.3  2004/11/07 23:01:42  mast
  Added fast min-max routine
  
  Revision 3.2  2004/11/05 19:08:12  mast
  Include local files with quotes, not brackets
  
  Revision 3.1  2002/12/01 15:34:41  mast
  Changes to get clean compilation with g++

*/

#ifndef IMOD_SLICEPROC_H
#define IMOD_SLICEPROC_H
/*  $Author$

    $Date$

    $Revision$

    $Log$
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


#ifdef __cplusplus
}
#endif

#endif

#ifndef IMOD_SLICEPROC_H
#define IMOD_SLICEPROC_H
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.3  2004/11/07 23:01:42  mast
    Added fast min-max routine

    Revision 3.2  2004/11/05 19:08:12  mast
    Include local files with quotes, not brackets

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/

#include "mrcslice.h"

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
#ifdef __cplusplus
}
#endif

#endif

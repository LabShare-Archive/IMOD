#ifndef IMOD_SLICEPROC_H
#define IMOD_SLICEPROC_H

#include <mrcslice.h>

int sliceByteConvolve(Islice *sin, int mask[3][3]);
int sliceByteAdd(Islice *sin, int inVal);
int sliceByteEdgeTwo(Islice *sin, int center);
int sliceByteEdgeSobel(Islice *sin);
int sliceByteEdgePrewitt(Islice *sin);
int sliceByteEdgeLaplacian(Islice *sin);
int sliceByteSharpen(Islice *sin);
int *sliceByteSmooth(Islice *sin);
int sliceByteConvolve(Islice *sin, int mask[3][3]);
int sliceByteThreshold(Islice *sin, int val);
int sliceByteGrow(Islice *sin, int val);
int sliceByteShrink(Islice *sin, int val);
int sliceByteGraham(Islice *sin);

#endif

/* reduce_by_binning.c: has C function redcueByBinning and wrapper for
 *  Fortran subroutine reduce_by_binning
 *
 * $Id$
 *
 * $Log$
 * Revision 1.4  2010/07/07 22:38:51  mast
 * Added support for RGB images
 *
 * Revision 1.3  2010/06/26 18:01:32  mast
 * Fixed test for keepByte value
 *
 * Revision 1.2  2007/10/01 15:26:09  mast
 * *** empty log message ***
 *
 * Revision 1.1  2007/09/20 02:42:53  mast
 * Added C translation to new library
 *
 */

#include "imodconfig.h"
#include "mrcslice.h"

#ifdef F77FUNCAP
#define reduce_by_binning REDUCE_BY_BINNING
#else
#ifdef G77__HACK
#define reduce_by_binning reduce_by_binning__
#else
#define reduce_by_binning reduce_by_binning_
#endif
#endif

/*!
 * Reduces an array in size by summing pixels (binning) by the factor [nbin].
 * [array] has the input, with dimensions [nxin] by [nyin], and slice mode in
 * [type].  The slice mode can be byte, short, unsigned short, float, or RGB.
 * [brray] receives the output, with dimensions returned in [nxr] and [nyr].
 * The output will have the same mode as the input and will be the average of
 * binned values, except in two cases.  If the input is bytes or RGB and 
 * [keepByte] is 0, then the output will be signed short integers and will be 
 * the sum, not the average, of the binned values, and for RGB data the output
 * will be an equal sum of red, green, and blue values.  [brray] can be the 
 * same as [array], and [nxr], [nyr] can be the same variables as [nxin],
 * [nyin].  The output size is obtained by integer division of the input size 
 * by the binning.  If the remainder of this division is nonzero, the data are 
 * centered in the output array as nearly as possible.  Specifically, the
 * coordinates of the lower left corner of the output array are offset by 
 * ^  ((nx % nbin) / 2, (ny % nbin) / 2) ^
 * relative to the input array.  Returns 1 for an unsupported data type.
 */
int reduceByBinning(void *array, int type, int nxin, int nyin, int nbin, 
                    void *brray, int keepByte, int *nxr, int *nyr)
{
  int i, j;
  int nbinsq = nbin * nbin;
  int nxout = nxin / nbin;
  int nyout = nyin / nbin;
  int ixofs = (nxin % nbin) / 2;
  int iyofs = (nyin % nbin) / 2;
  int sum, ix, iy, red, green, blue;
  b3dFloat fsum;
  unsigned char *bdata = (unsigned char *)brray;
  b3dInt16 *sdata = (b3dInt16 *)brray;
  b3dUInt16 *usdata = (b3dUInt16 *)brray;
  b3dInt16 *sline1, *sline2, *sline3, *sline4;
  b3dUInt16 *usline1, *usline2, *usline3, *usline4;
  unsigned char *cline1, *cline2, *cline3, *cline4;
  b3dFloat *fdata = (b3dFloat *)brray;
  b3dFloat *fline1, *fline2, *fline3, *fline4;

  if (type != SLICE_MODE_BYTE && type != SLICE_MODE_SHORT && 
      type != SLICE_MODE_USHORT && type != SLICE_MODE_FLOAT && 
      type != SLICE_MODE_RGB)
    return 1;

  if (nbin == 2 && type != SLICE_MODE_RGB) {
    
    /* Binning by 2 */
    switch (type) {
    case SLICE_MODE_BYTE:
      for (iy = 0; iy < nyout; iy++) {
        cline1 = ((unsigned char *)array) + 2 * iy * nxin;
        cline2 = cline1 + nxin;
        if (keepByte) {
          for (ix = 0;   ix < nxout; ix++) {
            sum = 2 + *cline1 + *(cline1 + 1) + *cline2 + *(cline2 + 1);
            *bdata++ = (unsigned char)(sum / 4);
            cline1 += 2;
            cline2 += 2;
          }
        } else {
          for (ix = 0;   ix < nxout; ix++) {
            *sdata++ = (b3dInt16)(*cline1) + *(cline1 + 1) + *cline2 + 
              *(cline2 + 1);
            cline1 += 2;
            cline2 += 2;
          }
        }
      }
      break;
      
    case SLICE_MODE_SHORT:
      for (iy = 0; iy < nyout; iy++) {
        sline1 = ((b3dInt16 *)array) + 2 * iy * nxin;
        sline2 = sline1 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          sum = 2 + *sline1 + *(sline1 + 1) + *sline2 + *(sline2 + 1);
          sline1 += 2;
          sline2 += 2;
          *sdata++ = sum / 4;
        }
      }
      break;

    case SLICE_MODE_USHORT:
      for (iy = 0; iy < nyout; iy++) {
        usline1 = ((b3dUInt16 *)array) + 2 * iy * nxin;
        usline2 = usline1 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          sum = 2 + *usline1 + *(usline1 + 1) + *usline2 + *(usline2 + 1);
          usline1 += 2;
          usline2 += 2;
          *usdata++ = sum / 4;
        }
      }
      break;

    case SLICE_MODE_FLOAT:
      for (iy = 0; iy < nyout; iy++) {
        fline1 = ((b3dFloat *)array) + 2 * iy * nxin;
        fline2 = fline1 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          fsum = *fline1 + *(fline1 + 1) + *fline2 + *(fline2 + 1);
          fline1 += 2;
          fline2 += 2;
          *fdata++ = fsum / 4.f;
        }
      }
      break;
    }
  
  } else if (nbin == 3 && type != SLICE_MODE_RGB) {

    /* Binning by 3 */
    switch (type) {
    case SLICE_MODE_BYTE:
      for (iy = 0; iy < nyout; iy++) {
        cline1 = ((unsigned char *)array) + (3 * iy + iyofs) * nxin + ixofs;
        cline2 = cline1 + nxin;
        cline3 = cline2 + nxin;
        if (keepByte) {
          for (ix = 0; ix < nxout; ix++) {
            sum = 4 + *cline1 + *(cline1 + 1) + *(cline1 + 2) +
              *cline2 + *(cline2 + 1) + *(cline2 + 2) +
              *cline3 + *(cline3 + 1) + *(cline3 + 2);
            cline1 += 3;
            cline2 += 3;
            cline3 += 3;
            *bdata++ = sum / 9;
          }
        } else {
          for (ix = 0; ix < nxout; ix++) {
            *sdata++ = (b3dInt16)(*cline1) + *(cline1 + 1) + *(cline1 + 2) +
              *cline2 + *(cline2 + 1) + *(cline2 + 2) +
              *cline3 + *(cline3 + 1) + *(cline3 + 2);
            cline1 += 3;
            cline2 += 3;
            cline3 += 3;
          }
        }
      }
      break;
      
    case SLICE_MODE_SHORT:
      for (iy = 0; iy < nyout; iy++) {
        sline1 = ((b3dInt16 *)array) + (3 * iy + iyofs) * nxin + ixofs;
        sline2 = sline1 + nxin;
        sline3 = sline2 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          sum = 4 + *sline1 + *(sline1 + 1) + *(sline1 + 2) +
            *sline2 + *(sline2 + 1) + *(sline2 + 2) +
            *sline3 + *(sline3 + 1) + *(sline3 + 2);
          sline1 += 3;
          sline2 += 3;
          sline3 += 3;
          *sdata++ = sum / 9;
        }
      }
      break;
        
    case SLICE_MODE_USHORT:
      for (iy = 0; iy < nyout; iy++) {
        usline1 = ((b3dUInt16 *)array) + (3 * iy + iyofs) * nxin + ixofs;
        usline2 = usline1 + nxin;
        usline3 = usline2 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          sum = 4 + *usline1 + *(usline1 + 1) + *(usline1 + 2) +
            *usline2 + *(usline2 + 1) + *(usline2 + 2) +
            *usline3 + *(usline3 + 1) + *(usline3 + 2);
          usline1 += 3;
          usline2 += 3;
          usline3 += 3;
          *usdata++ = sum / 9;
        }
      }
      break;
        
    case SLICE_MODE_FLOAT:
      for (iy = 0; iy < nyout; iy++) {
        fline1 = ((b3dFloat *)array) + (3 * iy + iyofs) * nxin + ixofs;
        fline2 = fline1 + nxin;
        fline3 = fline2 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          fsum = *fline1 + *(fline1 + 1) + *(fline1 + 2) +
            *fline2 + *(fline2 + 1) + *(fline2 + 2) +
            *fline3 + *(fline3 + 1) + *(fline3 + 2);
          fline1 += 3;
          fline2 += 3;
          fline3 += 3;
          *fdata++ = fsum / 9.f;
        }
      }
      break;
    }

  } else if (nbin == 4 && type != SLICE_MODE_RGB) {

    /* Binning by 4 */
    switch (type) {
    case SLICE_MODE_BYTE:
      for (iy = 0; iy < nyout; iy++) {
        cline1 = ((unsigned char *)array) + (4 * iy + iyofs) * nxin + ixofs;
        cline2 = cline1 + nxin;
        cline3 = cline2 + nxin;
        cline4 = cline3 + nxin;
        if (keepByte) {
          for (ix = 0; ix < nxout; ix++) {
            sum = 8 + *cline1 + *(cline1 + 1) + *(cline1 + 2) + *(cline1 + 3) +
              *cline2 + *(cline2 + 1) + *(cline2 + 2) + *(cline2 + 3) +
              *cline3 + *(cline3 + 1) + *(cline3 + 2) + *(cline3 + 3) +
              *cline4 + *(cline4 + 1) + *(cline4 + 2) + *(cline4 + 3);
            cline1 += 4;
            cline2 += 4;
            cline3 += 4;
            cline4 += 4;
            *bdata++ = sum / 16;
          }
        } else {
          for (ix = 0; ix < nxout; ix++) {
            *sdata++ = (b3dInt16)(*cline1) + *(cline1 + 1) + *(cline1 + 2) + 
              *(cline1 + 3) +
              *cline2 + *(cline2 + 1) + *(cline2 + 2) + *(cline2 + 3) +
              *cline3 + *(cline3 + 1) + *(cline3 + 2) + *(cline3 + 3) +
              *cline4 + *(cline4 + 1) + *(cline4 + 2) + *(cline4 + 3);
            cline1 += 4;
            cline2 += 4;
            cline3 += 4;
            cline4 += 4;
          }
        }
      }
      break;
        
    case SLICE_MODE_SHORT:
      for (iy = 0; iy < nyout; iy++) {
        sline1 = ((b3dInt16 *)array) + (4 * iy + iyofs) * nxin + ixofs;
        sline2 = sline1 + nxin;
        sline3 = sline2 + nxin;
        sline4 = sline3 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          sum = 8 +  *sline1 + *(sline1 + 1) + *(sline1 + 2) + *(sline1 + 3) +
            *sline2 + *(sline2 + 1) + *(sline2 + 2) + *(sline2 + 3) +
            *sline3 + *(sline3 + 1) + *(sline3 + 2) + *(sline3 + 3) +
            *sline4 + *(sline4 + 1) + *(sline4 + 2) + *(sline4 + 3);
          sline1 += 4;
          sline2 += 4;
          sline3 += 4;
          sline4 += 4;
          *sdata++ = sum / 16;
        }
      }
      break;
        
    case SLICE_MODE_USHORT:
      for (iy = 0; iy < nyout; iy++) {
        usline1 = ((b3dUInt16 *)array) + (4 * iy + iyofs) * nxin + ixofs;
        usline2 = usline1 + nxin;
        usline3 = usline2 + nxin;
        usline4 = usline3 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          sum = 8 +  *usline1 + *(usline1 + 1) + *(usline1 + 2) + 
            *(usline1 + 3) +
            *usline2 + *(usline2 + 1) + *(usline2 + 2) + *(usline2 + 3) +
            *usline3 + *(usline3 + 1) + *(usline3 + 2) + *(usline3 + 3) +
            *usline4 + *(usline4 + 1) + *(usline4 + 2) + *(usline4 + 3);
          usline1 += 4;
          usline2 += 4;
          usline3 += 4;
          usline4 += 4;
          *usdata++ = sum / 16;
        }
      }
      break;
        
    case SLICE_MODE_FLOAT:
      for (iy = 0; iy < nyout; iy++) {
        fline1 = ((b3dFloat *)array) + (4 * iy + iyofs) * nxin + ixofs;
        fline2 = fline1 + nxin;
        fline3 = fline2 + nxin;
        fline4 = fline3 + nxin;
        for (ix = 0;   ix < nxout; ix++) {
          fsum = *fline1 + *(fline1 + 1) + *(fline1 + 2) + *(fline1 + 3) +
            *fline2 + *(fline2 + 1) + *(fline2 + 2) + *(fline2 + 3) +
            *fline3 + *(fline3 + 1) + *(fline3 + 2) + *(fline3 + 3) +
            *fline4 + *(fline4 + 1) + *(fline4 + 2) + *(fline4 + 3);
          fline1 += 4;
          fline2 += 4;
          fline3 += 4;
          fline4 += 4;
          *fdata++ = fsum / 16.f;
        }
      }
      break;
    }

  } else {

    /* Bin by arbitrary number or bin RGB */
    switch (type) {
    case SLICE_MODE_BYTE:
      for (iy = 0; iy < nyout; iy++) {
        cline1 = ((unsigned char *)array) + (nbin * iy + iyofs) * nxin + ixofs;
        for (ix = 0; ix < nxout; ix++) {
          sum = 0;
          cline2 = cline1;
          for (j = 0; j < nbin; j++) {
            for (i = 0; i < nbin; i++) 
              sum += cline2[i];
            cline2 += nxin;
          }
          if (keepByte)
            *bdata++ = (unsigned char)((sum + nbinsq / 2) / nbinsq);
          else
            *sdata++ = sum;
          cline1 += nbin;
        }
      }
      break;

    case SLICE_MODE_SHORT:
      for (iy = 0; iy < nyout; iy++) {
        sline1 = ((b3dInt16 *)array) + (nbin * iy + iyofs) * nxin+ ixofs;
        for (ix = 0; ix < nxout; ix++) {
          sum = nbinsq / 2;
          sline2 = sline1;
          for (j = 0; j < nbin; j++) {
            for (i = 0; i < nbin; i++) 
              sum += sline2[i];
            sline2 += nxin;
          }
          *sdata++ = sum / nbinsq;
          sline1 += nbin;
        }
      }
      break;

    case SLICE_MODE_USHORT:
      for (iy = 0; iy < nyout; iy++) {
        usline1 = ((b3dUInt16 *)array) + (nbin * iy + iyofs) * nxin+ ixofs;
        for (ix = 0; ix < nxout; ix++) {
          sum = nbinsq / 2;
          usline2 = usline1;
          for (j = 0; j < nbin; j++) {
            for (i = 0; i < nbin; i++) 
              sum += usline2[i];
            usline2 += nxin;
          }
          *usdata++ = sum / nbinsq;
          usline1 += nbin;
        }
      }
      break;

    case SLICE_MODE_FLOAT:
      for (iy = 0; iy < nyout; iy++) {
        fline1 = ((b3dFloat *)array) + (nbin * iy + iyofs) * nxin + ixofs;
        for (ix = 0; ix < nxout; ix++) {
          fsum = 0.;
          fline2 = fline1;
          for (j = 0; j < nbin; j++) {
            for (i = 0; i < nbin; i++) 
              fsum += fline2[i];
            fline2 += nxin;
          }
          *fdata++ = fsum / (b3dFloat)nbinsq;
          fline1 += nbin;
        }
      }
      break;

    case SLICE_MODE_RGB:
      for (iy = 0; iy < nyout; iy++) {
        cline1 = ((unsigned char *)array) + 3 * ((nbin * iy + iyofs) * nxin + 
                                                 ixofs);
        for (ix = 0; ix < nxout; ix++) {
          red = green = blue = 0;
          cline2 = cline1;
          for (j = 0; j < nbin; j++) {
            for (i = 0; i < nbin; i++) {
              red += cline2[3*i];
              green += cline2[3*i + 1];
              blue += cline2[3*i + 2];
            }
            cline2 += nxin * 3;
          }
          if (keepByte) {
            *bdata++ = (unsigned char)((red + nbinsq / 2) / nbinsq);
            *bdata++ = (unsigned char)((green + nbinsq / 2) / nbinsq);
            *bdata++ = (unsigned char)((blue + nbinsq / 2) / nbinsq);
          } else 
            *sdata++ = red + green + blue;
          cline1 += nbin * 3;
        }
      }
      break;

    }
  }
  *nxr = nxout;
  *nyr = nyout;
  return 0;
}

/*!
 * Fortran wrapper for @reduceByBinning, called as {reduce_by_binning}.  
 * Again, the output variables can safely be the same as the input variables.
 */
void reduce_by_binning(float *array, int *nx, int *ny, int *nbin,
                       float *brray, int *nxr, int *nyr)
{
  reduceByBinning(array, SLICE_MODE_FLOAT, *nx, *ny, *nbin, brray, 0, nxr, 
                  nyr);
}

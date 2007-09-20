/* reduce_by_binning.c: has C function redcueByBinning and wrapper for
 *  Fortran subroutine reduce_by_binning
 *
 * $Id$
 *
 * $Log$
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
 * [array] has the input, with dimensions [nx] by [ny], while [brray] receives
 * the output, with dimensions returned in [nxr] and [nyr].  [brray] can be
 * the same as [array], and [nxr], [nyr] can be the same variables as [nx], 
 * [ny].  The output size is obtained by integer division of the input size by
 * the binning.  If the remainder of this division is nonzero, the data are 
 * centered in the output array as nearly as possible.  Specifically, the
 * coordinates of the lower left corner of the output array are offset by 
 * ^  ((nx % nbin) / 2, (ny % nbin) / 2) ^
 * relative to the input array.
 */
int reduceByBinning(void *datain, int mode, int nx, int ny, int nbin, 
                    void *dataout, int *nxr, int *nyr)
{
  int ixofset,iyofset,iyout,ixout,ixin,iyin,ixbase;
  int linebase,ixoutbase,nxrtmp,nyrtmp;
  float sum,binsq;
  b3dFloat *fdin = (b3dFloat *)datain;
  b3dFloat *fdout = (b3dFloat *)dataout;
  unsigned char *bdin = (unsigned char *)datain;
  unsigned char *bdout = (unsigned char *)dataout;
  b3dInt16 *sdin = (b3dInt16 *)datain;
  b3dInt16 *sdout = (b3dInt16 *)dataout;
  b3dUInt16 *usdin = (b3dUInt16 *)datain;
  b3dUInt16 *usdout = (b3dUInt16 *)dataout;

	  
  /* get reduced size and split any remainder equally to left and right */
  nxrtmp = nx/nbin;
  nyrtmp = ny/nbin;
  ixofset = (nx % nbin) / 2;
  iyofset = (ny % nbin) / 2;
  binsq = nbin * nbin;

  for (iyout = 0; iyout < nyrtmp; iyout++) {
    linebase = nx*(nbin*iyout+iyofset)+ixofset;
    ixoutbase = nxrtmp*iyout;
    switch (mode) {
    case SLICE_MODE_BYTE:
      for (ixout = 0; ixout < nxrtmp; ixout++) {
        ixbase = linebase;
        sum = 0.;
        for (iyin = 0; iyin < nbin; iyin++) {
          for (ixin = 0; ixin < nbin; ixin++)
            sum = sum + bdin[ixbase+ixin];
          ixbase = ixbase+nx;
        }
        bdout[ixoutbase+ixout] = (unsigned char)(sum/binsq);
        linebase = linebase+nbin;
      }
      break;

    case SLICE_MODE_SHORT:
      for (ixout = 0; ixout < nxrtmp; ixout++) {
        ixbase = linebase;
        sum = 0.;
        for (iyin = 0; iyin < nbin; iyin++) {
          for (ixin = 0; ixin < nbin; ixin++)
            sum = sum + sdin[ixbase+ixin];
          ixbase = ixbase+nx;
        }
        sdout[ixoutbase+ixout] = (b3dInt16)(sum/binsq);
        linebase = linebase+nbin;
      }
      break;

    case SLICE_MODE_FLOAT:
      for (ixout = 0; ixout < nxrtmp; ixout++) {
        ixbase = linebase;
        sum = 0.;
        for (iyin = 0; iyin < nbin; iyin++) {
          for (ixin = 0; ixin < nbin; ixin++)
            sum = sum + fdin[ixbase+ixin];
          ixbase = ixbase+nx;
        }
        fdout[ixoutbase+ixout] = (b3dFloat)(sum/binsq);
        linebase = linebase+nbin;
      }
      break;

    case SLICE_MODE_USHORT:
      for (ixout = 0; ixout < nxrtmp; ixout++) {
        ixbase = linebase;
        sum = 0.;
        for (iyin = 0; iyin < nbin; iyin++) {
          for (ixin = 0; ixin < nbin; ixin++)
            sum = sum + usdin[ixbase+ixin];
          ixbase = ixbase+nx;
        }
        usdout[ixoutbase+ixout] = (b3dUInt16)(sum/binsq);
        linebase = linebase+nbin;
      }
      break;

    default:
      return 1;
      
    }
  }
  *nxr = nxrtmp;
  *nyr = nyrtmp;
  return 0;
}

/*!
 * Fortran wrapper for @reduceByBinning, called as {reduce_by_binning}.  
 * Again, the output variables can safely be the same as the input variables.
 */
void reduce_by_binning(float *array, int *nx, int *ny, int *nbin,
                       float *brray, int *nxr, int *nyr)
{
  reduceByBinning(array, SLICE_MODE_FLOAT, *nx, *ny, *nbin, brray, nxr, nyr);
}

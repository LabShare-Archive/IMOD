/*
 * sparselsqr.h - Declarations for using lsqr with sparse matrices
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef SPARSELSQR_H

#include "imodconfig.h"

#ifdef F77FUNCAP
#define lsqrfw LSQR
#define sparseProd SPARSEPROD
#define addvaluetorow ADDVALUETOROW
#define addrowtomatrix ADDROWTOMATRIX
#define normalizecolumns NORMALIZECOLUMNS
#else
#define lsqrfw lsqr_
#define sparseProd sparseprod_
#define addvaluetorow addvaluetorow_
#define addrowtomatrix addrowtomatrix_
#define normalizecolumns normalizecolumns_
#endif

#ifdef __cplusplus
extern "C" {
#endif

  void sparseProd(int mode, int m, int n, double x[], double y[], 
                  void *UsrWrk);
  void addValueToRow(float val, int icol, float *valRow, int *icolRow, 
                     int *numInRow);
  int addRowToMatrix(float *valRow, int *icolRow, int numInRow, float *rwrk, 
                     int *ia, int *ja, int *numRows, int maxRows, int maxVals);
  void normalizeColumns(float *rwrk, int *ia, int *ja, int numVars,
                        int numRows, float *sumEntries);

#ifdef __cplusplus
}
#endif

#define SPARSELSQR_H
#endif

/*    

$Log$

*/

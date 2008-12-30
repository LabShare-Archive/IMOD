/*
 * sparselsqr.c - Functions for using lsqr with sparse matrices
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <math.h>
#include "sparselsqr.h"
#include "lsqr.h"

/* Fortran wrapper for lsqr.  Note added argument ifse should be nonzero to
 * evaluate se, otherwise se can be a small array.  If nout is non-zero,
 * output will be to stdout.
 */
void lsqrfw(int *m, int *n, 
      void (*aprod)(int mode, int m, int n, double x[], double y[],
                    void *UsrWrk ),
            double *damp,
            int    *ifse,
            void   *UsrWrk,
            double u[],    // len = m
            double v[],    // len = n
            double w[],    // len = n
            double x[],    // len = n
            double *se,    // len = *
            double *atol,
            double *btol,
            double *conlim,
            int    *itnlim,
            int   *nout,
            // The remaining variables are output only.
            int    *istop_out,
            int    *itn_out,
            double *anorm_out,
            double *acond_out,
            double *rnorm_out,
            double *arnorm_out,
            double *xnorm_out
            )
{
  FILE *out = NULL;
  double *secall = NULL;
  if (*nout > 0) 
    out = stdout;
  if (*ifse > 0)
    secall = se;
  lsqr(*m, *n, aprod, *damp, UsrWrk, u, v, w, x, secall, *atol, *btol, *conlim,
       *itnlim, out, istop_out, itn_out, anorm_out, acond_out, rnorm_out, 
       arnorm_out, xnorm_out);
}


/*!  
 * Compute products in a sparse matrix for lsqr.  The matrix has [m] data rows
 * times [n] columns, one for each variable  ^
 * If [mode] = 1, compute  y = y + A*x.  ^
 * If [mode] = 2, compute  x = x + A(transpose)*y.  ^
 * [UsrWrk] is loaded as follows:  ^
 * Offset to JA values, the column number of each data value (from 1) ^
 * Offset to RW values, the data values themselves  ^
 * IA values: the starting index of each row.  Numbered from 1  ^
 */
void sparseProd(int mode, int m, int n, double x[], double y[], void *UsrWrk)
{
  int *iw = (int *)UsrWrk;
  int jaofs = iw[0];
  int rwofs = iw[1];
  float *rw = (float *)UsrWrk + rwofs;
  int irow, ind, icol;
  if (mode == 1) {
    for (irow = 0; irow < m; irow++) {
      for (ind = iw[irow + 2] - 1; ind < iw[irow + 3] - 1; ind++) {
        icol = iw[jaofs + ind] - 1;
        y[irow] += rw[ind] * x[icol];
      }
    }
  } else {
    for (irow = 0; irow < m; irow++) {
      for (ind = iw[irow + 2] - 1; ind < iw[irow + 3] - 1; ind++) {
        icol = iw[jaofs + ind] - 1;
        x[icol] += rw[ind] * y[irow];
      }
    }
  }
}

/*!
 * Adds one value [val] and its data column [icol] to arrays [valRow] and
 * [icolRow] for the current row, and maintains the number of values in 
 * [numInRow].
 */
void addValueToRow(float val, int icol, float *valRow, int *icolRow, 
                   int *numInRow)
{
  valRow[*numInRow] = val;
  icolRow[*numInRow] = icol;
  (*numInRow)++;
}

/*! Fortran wrapper for @addValueToRow */
void addvaluetorow(float *val, int *icol, float *valRow, int *icolRow, 
                   int *numInRow)
{
  addValueToRow(*val, *icol, valRow, icolRow, numInRow);
}

/*!
 * Adds one data row to a sparse matrix.  Values are in [valRow], column
 * numbers (numbered from 1) in [icolRow], number of values in [numInRow].
 * [rwrk] is the sparse matrix array of data values, [ja] is the corresponding 
 * array of column numbers, [ia] is an array of starting indexes into those
 * arrays for each row (indexes numbered from 1).  The number of rows is
 * maintained in [numRows], and [maxRows] and [maxVals] are the maximum
 * number of rows and values allowed.
 */
int addRowToMatrix(float *valRow, int *icolRow, int numInRow, float *rwrk, 
                   int *ia, int *ja, int *numRows, int maxRows, int maxVals)
{
  int i, ind, icol;
  ind = ia[*numRows] - 1;
  (*numRows)++;
  if (*numRows >= maxRows)
    return 1;
  for (i = 0; i < numInRow; i++) {
    rwrk[ind] = valRow[i];
    icol = icolRow[i];
    ja[ind++] = icol;
    if (ind >= maxVals)
      return 2;
  }
  ia[*numRows] = ind + 1;
  return 0;
}

/*! Fortran wrapper for @addRowToMatrix */
int addrowtomatrix(float *valRow, int *icolRow, int *numInRow, float *rwrk, 
                   int *ia, int *ja, int *numRows, int *maxRows, int *maxVals)
{
  return addRowToMatrix(valRow, icolRow, *numInRow, rwrk, ia, ja, numRows,
                        *maxRows, *maxVals);
}

/*!
 * Normalizes each column of data in the sparse matrix by dividing it by the 
 * square root of the sum of squares of values in that column, as recommended
 * for running lsqr.  [rwrk], [ja], and [ia] are as described for 
 * @addRowToMatrix; [numVars] is the number of variables (columns) and
 * [numRows] is the number of rows.  The normalizing factor for each column is
 * returned in [sumEntries].  The solution vector obtained from normalized
 * data needs to be divided by [sumEntries].
 */
void normalizeColumns(float *rwrk, int *ia, int *ja, int numVars, int numRows,
                      float *sumEntries)
{
  int icol, j;
  for (icol = 0; icol < numVars; icol++)
    sumEntries[icol] = 0.;
  for (j = 0; j < ia[numRows] - 1; j++) {
    icol = ja[j] - 1;
    sumEntries[icol] += rwrk[j] * rwrk[j];
  }
  for (icol = 0; icol < numVars; icol++)
    sumEntries[icol] = (float)sqrt((double)sumEntries[icol]);
  for (j = 0; j < ia[numRows] - 1; j++) {
    icol = ja[j] - 1;
    rwrk[j] /= sumEntries[icol];
  }
}

/*! Fortran wrapper for @normalizeColumns */
void normalizecolumns(float *rwrk, int *ia, int *ja, int *numVars, 
                      int *numRows, float *sumEntries)
{
  normalizeColumns(rwrk, ia, ja, *numVars, *numRows, sumEntries);
}

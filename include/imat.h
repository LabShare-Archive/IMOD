/*  IMOD VERSION 2.20
 *
 *  imat.h -- Image model matrix header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

#ifndef IMAT_H
#define IMAT_H

typedef struct imodel_matrix
{
     float *data;
     int   dim;   /* is 2D or 3D */
     int   size;
}Imat;


#ifdef __cplusplus
extern "C" {
#endif

Imat *imodMatNew(int dim);
void  imodMatDelete(Imat *mat);
void  imodMatId(Imat *mat);
void  imodMatCopy(Imat *fmat, Imat *tomat);
void  imodMatMult(Imat *mat2, Imat *mat1, Imat *matout);
void  imodMatTrans(Imat *mat, Ipoint *pt);
int   imodMatScale(Imat *mat, Ipoint *pt);
int   imodMatRot(Imat *mat, double angle, int axis);
void  imodMatTransform2D(Imat *mat, Ipoint *pt, Ipoint *rpt);
void  imodMatTransform3D(Imat *mat, Ipoint *pt, Ipoint *rpt);
void  imodMatTransform(Imat *mat, Ipoint *pt, Ipoint *rpt);

int imodMatRotateVector(Imat *mat, double angle, Ipoint *v);
int imodMatFindVector(Imat *mat, double *angle, Ipoint *v);
int imodMatGetNatAngles(Imat *mat, double *x, double *y, double *z);

/* works for dim = 2 only */
Imat *imodMatInverse(Imat *mat);

#ifdef __cplusplus
}
#endif
#endif

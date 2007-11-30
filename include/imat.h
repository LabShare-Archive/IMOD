/*
 *  imat.h -- Image model matrix header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

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
void imodMatPrint(Imat *mat);
int imodMatRotateVector(Imat *mat, double angle, Ipoint *v);
int imodMatFindVector(Imat *mat, double *angle, Ipoint *v);
int imodMatGetNatAngles(Imat *mat, double *x, double *y, double *z);
void imodMatUniqueAngles(double *x, double *y, double *z);
void imodMatUniqueRotationPt(Ipoint *pt);
Imat *imodMatInverse(Imat *mat);

#ifdef __cplusplus
}
#endif
#endif

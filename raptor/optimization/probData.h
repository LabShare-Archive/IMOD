
#ifndef probData_H_
#define probData_H_

#include "../opencv/cv.h"
#include "../suitesparse/cs.h"
#include <vector>

using namespace std;

class probData
{
public:
    probData();
    probData(const probData &pD);
    ~probData();
    cs* CopyCS(cs *A);

    CvMat* buc;
    cs *a;
    cs *c;
    cs *Q;
    CvMat* x0;
};

inline cs* probData::CopyCS(cs *A)
{
    if(A==NULL)
        return(NULL);

    cs *B;
    if(A->nz==-1)
        B=cs_spalloc(A->m,A->n,A->nzmax,1,0);
    else
        B=cs_spalloc(A->m,A->n,A->nzmax,1,1);

    for(int ii=0;ii<A->nzmax;ii++)
    {
        B->i[ii]=A->i[ii];
        B->x[ii]=A->x[ii];
    }
    if(A->nz==-1)//compressed colum format
    {
        for(int ii=0;ii<(A->n)+1;ii++)
            B->p[ii]=A->p[ii];
    }
    else{
        for(int ii=0;ii<A->nzmax;ii++)
            B->p[ii]=A->p[ii];
    }

    B->nz=A->nz;

    return(B);
}
#endif

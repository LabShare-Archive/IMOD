/*
 * std_qp.cpp - QP solver
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include <iostream>
#include <string>
#include <fstream>
#include "std_qp.h"
#include "../mainClasses/constants.h"
#include "../suitesparse/cs.h"



using namespace std;

CvMat* ZeroMat(int x, int y)
{
    CvMat* result = cvCreateMat(x,y,CV_64FC1);
    cvSetZero(result);
    return result;
}


double MinElem(CvMat* mat)
{
    double min = cvGetReal2D(mat, 0, 0);
    for (int i = 0; i < cvGetSize(mat).height; i++)
    {
        for (int j = 0; j < cvGetSize(mat).width; j++)
        {
            if (cvGetReal2D(mat, i, j) < min)
                min = cvGetReal2D(mat, i, j);
        }
    }
    return min;
}

CvMat* Combinedx1dx2(CvMat* dx1, CvMat* dx2)
{
    assert(cvGetSize(dx1).width == cvGetSize(dx2).width);
    int width = cvGetSize(dx2).width;
    int height = cvGetSize(dx1).height + cvGetSize(dx2).height;
    CvMat * result=cvCreateMat(height,width,CV_64FC1);
    for (int i = 0; i < cvGetSize(dx1).height; i++)
    {
        for (int j = 0; j < cvGetSize(dx1).width; j++)
        {
            cvSetReal2D(result,i,j,cvGetReal2D(dx1, i,j));
        }
    }

    for (int i = 0; i < cvGetSize(dx2).height; i++)
    {
        for (int j = 0; j < cvGetSize(dx2).width; j++)
        {
            cvSetReal2D(result,i+cvGetSize(dx1).height,j,cvGetReal2D(dx2, i,j));
        }
    }
    return result;
}


double sumLog(CvMat* mat)
{
    assert(cvGetSize(mat).width == 1);
    double answer = 0.0;
    for (int i = 0; i < cvGetSize(mat).height; i++)
    {
        for (int j = 0; j < cvGetSize(mat).width; j++)
        {
            double elem = cvGetReal2D(mat, i, j);
            elem = log(elem);
            answer += elem;
        }
    }

    return answer;
}


STDQPdata* std_qp(cs* _Q, cs* _c, cs* _A, CvMat* b, CvMat* x0, int M, int T, string option)
{

    //famatcsparse
    //copy matrices into cSparse structure
    /*
    cs *_Q,*_c,*_A,*_T;
    double *_b,*_x0;
    _T=cs_spalloc(Q->xMatSize,Q->yMatSize,Q->numElems,1,1);
    for (map<pair<int,int>, double>::iterator itr = (Q->array).begin(); itr != (Q->array).end(); itr++)
    {
        cs_entry(_T,(*itr).first.first, (*itr).first.second, (*itr).second);
    }
    _Q=cs_compress(_T);
    cs_spfree (_T);

    _T=cs_spalloc(A->xMatSize,A->yMatSize,A->numElems,1,1);
    for (map<pair<int,int>, double>::iterator itr = (A->array).begin(); itr != (A->array).end(); itr++)
    {
        cs_entry(_T,(*itr).first.first, (*itr).first.second, (*itr).second);
    }
    _A=cs_compress(_T);
    cs_spfree (_T);

    _T=cs_spalloc(c->xMatSize,c->yMatSize,c->numElems,1,1);
    for (map<pair<int,int>, double>::iterator itr = (c->array).begin(); itr != (c->array).end(); itr++)
    {
        cs_entry(_T,(*itr).first.first, (*itr).first.second, (*itr).second);
    }
    _c=cs_compress(_T);
    cs_spfree (_T);
    */
    /*
    _b=new double[cvGetSize(b).height];
    for(int ii=0;ii<cvGetSize(b).height;ii++)
        _b[ii]=cvGetReal2D(b,ii,0);

    _x0=new double[cvGetSize(x0).height];
    for(int ii=0;ii<cvGetSize(x0).height;ii++)
        _b[ii]=cvGetReal2D(x0,ii,0);
        */
    //---------------------------------------------------------------

    //make sure sparse matrices are column compress format
    if (_Q->nz!=-1) _Q=cs_compress(_Q);
    if (_c->nz!=-1) _c=cs_compress(_c);
    if (_A->nz!=-1) _A=cs_compress(_A);
    cs *_T;

    //cout << "In STD QP" << endl;
    //initialize result struct
    STDQPdata* theResult = new STDQPdata();

    // initialize constants
    theResult->answerMat = cvCloneMat(x0);
    //PrintCvMat(theResult->answerMat);
    double alpha = 0.25;
    double beta = 0.5;
    double tol1 = 1e-3;
    double tol2 = 1e-3;
    int NEWTON_MAXITERS = 20;
    int QP_MAXITERS = 200;
    int exit_flag = 0;
    double mu = 10.0;
    double t = 1.0/mu;
    double n = cvGetSize(theResult->answerMat).height;
    int iters = 1;

    /*
    CvMat* Ax = A->SparseDenseMult(theResult->answerMat);
    CvMat* bAx=cvCreateMat(cvGetSize(b).height,cvGetSize(b).width,CV_64FC1);
    cvSub(b, Ax, bAx);
    */

    CvMat *bAx=cvCloneMat(b);
    cvConvertScale(bAx,bAx,-1.0,0);
    cs_gaxpy(_A,theResult->answerMat->data.db,bAx->data.db);
    cvConvertScale(bAx,bAx,-1.0,0);

    //famatdebug
    /*
    ofstream outbAx("bAx_qp.txt");
    print(bAx,outbAx);
    outbAx.close();
    ofstream outA("A_qp.txt");
    print(_A,outA);
    outA.close();
    ofstream outb("b_qp.txt");
    print(b,outb);
    outb.close();
    ofstream outQ("Q_qp.txt");
    print(_Q,outQ);
    outQ.close();
    ofstream outx("x0_qp.txt");
    print(x0,outx);
    outx.close();
    ofstream outc("c_qp.txt");
    print(_c,outc);
    outc.close();
    std::cout<<"Fnished printing A,b,c,Q for std_qp"<<std::endl;
    //exit(-1);
    */
    //------------------

    //bAx
    double minValue = MinElem(bAx);
    // cout << "Min of b: " << MinElem(b) << endl;
    // cout << "MIN: " << minValue << endl;

    //check unfeasible starting point

    //PrintCvMat(bAx);
    if (minValue < 0)
    {
        if(option != "probU")
            cout << "WARNING: Infeasible starting point for QP solving "<<option << endl;
        //for(int kk=0;kk<bAx->rows;kk++) cout<<bAx->data.db[kk]<<endl;

        cvReleaseMat(&bAx);
        theResult->exit_flag = 2;
        theResult->gap = -1.0*(minValue);
        return theResult;
    }

    while ( (n/t > tol1) && (exit_flag == 0) && (iters<QP_MAXITERS) )
    {
        //cout << "Within WHILE loop" << endl;
        t *= mu;

        for (int iter = 1; iter <= NEWTON_MAXITERS; iter++)
        {
            //cout << "Within ITERS" << endl;

            iters++;
            theResult->numItrs = iters;

            /*-----------------------------------------------
            //famatnote: gradient and Hessian have some operations in common. We should shoin them
            cout<<"Making Hessian" <<endl;
            sparseMat* H = MakeHessian(t, Q, A, bAx);

            cout<<"Making Jacobian" << endl;
            sparseMat* g = MakeGradient(t, Q, theResult->answerMat, A, bAx, c);

            ------------------------------------*/
            //--------------------------------------
            //compute gradient and Hessian
            _T=cs_spalloc(cvGetSize(bAx).height,cvGetSize(bAx).height,cvGetSize(bAx).height,1,1);
            for (int ii=0;ii<cvGetSize(bAx).height;ii++)
                cs_entry(_T,ii,ii,1.0/(cvGetReal2D(bAx,ii,0)));
            cs *diagM=cs_compress(_T);
            cs_spfree (_T);

            _T=cs_multiply(diagM,_A);
            cs_spfree(diagM);
            cs *auxOp=cs_transpose(_T,1);
            cs *auxOp2=cs_multiply(auxOp,_T);
            cs *H=cs_add(auxOp2,_Q,1.0,t);
            cs_spfree(_T);
            cs_spfree(auxOp);
            cs_spfree(auxOp2);
            //drop small entries
            cs_droptol(H,sparseMatrixZeroVal);

            CvMat *g=cvCreateMat(H->m,1,CV_64FC1);
            cvSetZero(g);
            cs_gaxpy(_Q,theResult->answerMat->data.db,g->data.db);
            double auxC=1.0;
            cs_gaxpy(_c,&auxC,g->data.db);
            for (int ii=0;ii<g->rows;ii++)
                g->data.db[ii]*=t;


            double *bAxInv=new double[_A->m];
            for (int ii=0;ii<_A->m;ii++)
                bAxInv[ii]=1.0/cvGetReal2D(bAx,ii,0);

            _T=cs_transpose(_A,1);
            cs_gaxpy(_T,bAxInv,g->data.db);
            cs_spfree(_T);
            delete []bAxInv;

            //----------------------------------
            //

            cs* C;
            cs* R;
            double* D1;
            double* D2;
            double* D3;
            CvMat* g1;
            CvMat* g2;
            int Dsize;

            if (option == "prob")
            {

                //cout << "prob:" << endl;
                C = GetSubMatrix(H,0, 2*T+3*M-1, 0, 2*T+3*M-1);
                R = GetSubMatrix(H,2*T+3*M, H->m-1, 0, 2*T+3*M-1);
                D1 = GetSubMatrixDiag(H,2*T+3*M, 2*T+3*M+2*T*M-1,2*T+3*M,2*T+3*M+2*T*M-1);
                D2 = GetSubMatrixDiag(H,2*T+3*M, 2*T+3*M+2*T*M-1,2*T+3*M+2*T*M,H->n-1);
                D3 = GetSubMatrixDiag(H,2*T+3*M+2*T*M, H->m-1, 2*T+3*M+2*T*M, H->n-1);
                g1 = GetSubMatrix(0, 2*T+3*M-1, 0, 0,g);
                g2 = GetSubMatrix(2*T+3*M, cvGetSize(g).height-1, 0, 0,g);
                Dsize=2*T*M;
            }
            else if (option == "probV")
            {

                C = GetSubMatrix(H,0,2, 0, 2);
                R = GetSubMatrix(H,3, H->m-1, 0, 2);
                D1 = GetSubMatrixDiag(H,3,2*T+2,3,2*T+2);
                D2 = GetSubMatrixDiag(H,3,2*T+2,3+2*T,H->n-1);
                D3 = GetSubMatrixDiag(H,3+2*T, H->m-1, 3+2*T, H->n-1);
                g1 = GetSubMatrix(0, 2, 0, 0,g);
                g2 = GetSubMatrix(3, cvGetSize(g).height-1, 0, 0,g);
                tol1 = 1e-4;
                Dsize=2*T;
            }
            else if (option == "probU")
            {
                C = GetSubMatrix(H,0, 3, 0, 3);
                R = GetSubMatrix(H,4, H->m-1, 0, 3);
                D1 = GetSubMatrixDiag(H,4,4+M-1,4,4+M-1);
                D2 = GetSubMatrixDiag(H,4,4+M-1,4+M,H->n-1);
                D3 = GetSubMatrixDiag(H,4+M, H->m-1, 4+M, H->n-1);
                g1 = GetSubMatrix(0, 3, 0, 0,g);
                g2 = GetSubMatrix(4, cvGetSize(g).height-1, 0, 0,g);
                tol1 = 1e-4;
                Dsize=M;
            }
            else
            {
                cout << "Unknown option for std_qp";
                return theResult;
            }

            //int l = D1->getX();
            double* aux1 = new double[Dsize];

            for (int ii=0;ii<Dsize;ii++)
                aux1[ii]=D1[ii]*D3[ii]-D2[ii]*D2[ii];

            //(D1->elementMult(D3))->SparseMinus(D2->elementPower(2));
            //Dinv=[spdiags(D3./aux,0,l,l) -spdiags(D2./aux,0,l,l);-spdiags(D2./aux,0,l,l) spdiags(D1./aux,0,l,l)];

            /*
            sparseMat* upperLeft = (D3->elementDiv(aux1));
            sparseMat* upperRight = (D2->elementDiv(aux1))->SparseScalarMult(-1);
            sparseMat* lowerLeft = (D2->elementDiv(aux1))->SparseScalarMult(-1);
            sparseMat* lowerRight = (D1->elementDiv(aux1));
            sparseMat* Dinv = CombineMatrices(upperLeft, upperRight, lowerLeft, lowerRight);
            */

            _T=cs_spalloc(Dsize+Dsize,Dsize+Dsize,4*Dsize,1,1);
            for (int ii=0;ii<Dsize;++ii)
            {
                cs_entry(_T,ii,ii,D3[ii]/aux1[ii]);
                cs_entry(_T,ii+Dsize,ii+Dsize,D1[ii]/aux1[ii]);
                cs_entry(_T,ii,ii+Dsize,-D2[ii]/aux1[ii]);
                cs_entry(_T,ii+Dsize,ii,-D2[ii]/aux1[ii]);
            }
            cs *Dinv=cs_compress(_T);
            cs_spfree(_T);

            /*
            D3->elementDivIn(aux1);
            D2->elementDivIn(aux1);
            D2->SparseScalarMultIn(-1.0);
            D1->elementDivIn(aux1);
            sparseMat* Dinv = CombineMatrices(D3, D2,D2, D1);
            */

            //famatdebug-------------------------------------------
            /*
            ofstream outD1("D1_qp.txt");
            for(int ii=0;ii<Dsize;ii++)
                outD1<<D1[ii]<<" ";
            outD1.close();

            ofstream outD2("D2_qp.txt");
            for(int ii=0;ii<Dsize;ii++)
                outD2<<D2[ii]<<" ";
            outD2.close();

            ofstream outD3("D3_qp.txt");
            for(int ii=0;ii<Dsize;ii++)
                outD3<<D3[ii]<<" ";
            outD3.close();

            ofstream outDinv("Dinv_qp.txt");
            Q->print(Dinv,outDinv);
            outDinv.close();
            */
            //--- ---------------------------------------------------

            delete []D1;
            delete []D2;
            delete []D3;
            delete []aux1;
            //delete upperLeft;
            //delete upperRight;
            //delete lowerLeft;
            //delete lowerRight;

            //cout << " R x: "<< R->getX() << " R y: " << R->getY() << " dinv x: "<< Dinv->getX() << " dinv y: "<< Dinv->getY() << endl;
            _T=cs_transpose(R,1);
            cs* aux = cs_multiply(_T,Dinv);
            cs_spfree(_T);


            //turn sparse into dense for cvSolve()


            /*    aux=D1.*D3-D2.^2;
               Dinv=[spdiags(D3./aux,0,l,l) -spdiags(D2./aux,0,l,l);-spdiags(D2./aux,0,l,l) spdiags(D1./aux,0,l,l)];
               aux=R'*Dinv;
               dx1=(C-aux*R)\(-g1+aux*g2);
               dx2=-Dinv*(g2+R*dx1);
               dx=[dx1;dx2];
            */

            //sparseMat* SolveA = C->SparseMinus(aux->SparseSparseMult(R));
            _T=cs_multiply(aux,R);
            cs *DenseA=cs_add(C,_T,1.0,-1.0);
            cs_spfree(_T);

            //delete C;
            cs_spfree(C);
            //CvMat* DenseA = SolveA->ConvertSparseDense();
            //delete SolveA;

            //cout << "solvedA" << endl;
            //CvMat *SolveB=aux->SparseDenseMult(g2);
            CvMat *DenseB=cvCreateMat(aux->m,1,CV_64FC1);
            cvSetZero(DenseB);
            cs_gaxpy(aux,g2->data.db,DenseB->data.db);
            cvSub(DenseB,g1,DenseB);
            //cvSub(SolveB,g1,DenseB);
            //cvReleaseMat(&SolveB);
            //cout << "solvedB" << endl;


            //famatdebug
            //cout<<"DenseA nz="<<DenseA->nz<<endl;

            //ofstream outDenseA("DenseA_qp.txt");
            //Q->print(DenseA,outDenseA);
            //outDenseA.close();
            //-------------------------------

            //int cholFlag=cs_cholsol(1,DenseA,DenseB->data.db);//solving system using sparse Cholesky decomposition
            //cholFlag is not used by now
            cs_cholsol(1,DenseA,DenseB->data.db);//solving system using sparse Cholesky decomposition
            CvMat *dx1=cvCloneMat(DenseB);
            //CvMat* dx1 = cvCreateMat(aux->getX(), 1, CV_64FC1);
            //cvSolve(DenseA, DenseB, dx1, CV_LU);
            cs_spfree(aux);

            cs_spfree(DenseA);
            cvReleaseMat(&DenseB);
            //cout << "Solved!" << endl;


            //dx2 = -Dinv*(g2+R*dx1)
            /*
            sparseMat* negDinv = Dinv->SparseScalarMult(-1);
            CvMat* Rdx1 = R->SparseDenseMult(dx1);
            CvMat* g2Rdx1=cvCreateMat(cvGetSize(g2).height,cvGetSize(g2).width,CV_64FC1);
            cvAdd(g2,Rdx1,g2Rdx1);
            CvMat* dx2 = negDinv->SparseDenseMult(g2Rdx1);

            delete Dinv;
            cvReleaseMat(&Rdx1);
            cvReleaseMat(&g2Rdx1);
                */

            cs_gaxpy(R,dx1->data.db,g2->data.db);
            cvConvertScale(g2,g2,-1.0,0);
            CvMat *dx2=ZeroMat(g2->rows,g2->cols);
            cs_gaxpy(Dinv,g2->data.db,dx2->data.db);


            cs_spfree(Dinv);
            cs_spfree(R);
            cvReleaseMat(&g2);
            cvReleaseMat(&g1);

            //dx = [dx1;dx2]
            CvMat* dx = Combinedx1dx2(dx1,dx2);

            //famatdebug-------------------------------------------
            /*
            ofstream outdx1("dx1_qp.txt");
            Q->print(dx1,outdx1);
            outdx1.close();
            ofstream outdx2("dx2_qp.txt");
            Q->print(dx2,outdx2);
            outdx2.close();
            ofstream outdx("dx_qp.txt");
            Q->print(dx,outdx);
            outdx.close();
            */
            //--------------------------------

            cvReleaseMat(&dx1);
            cvReleaseMat(&dx2);

            double t2 = 1;

            // forcing A*x <= b

            CvMat* xdx=cvCreateMat(cvGetSize(dx).height,cvGetSize(dx).width,CV_64FC1);
            cvAdd(theResult->answerMat,dx,xdx);
            CvMat* whileLoopTest=SparseDenseMult(_A,xdx);
            cvReleaseMat(&xdx);
            cvSub(b,whileLoopTest,whileLoopTest);
            double minValue2 = MinElem(whileLoopTest);



            //cout << "STDQP checkpoint 1" << endl;

            //CvMat* Adx=A->SparseDenseMult(dx);
            CvMat* Adx=ZeroMat(_A->m,dx->cols);
            cs_gaxpy(_A,dx->data.db,Adx->data.db);
            while (minValue2 <= 0)
            {
                t2 = beta*t2;
                if (t2 < 1e-11)
                {
                    exit_flag = 1;
                    break;
                }
                //CvMat* whileLoopTest =  (A->SparseDenseMult((dx->SparseScalarMult(t2))->DenseAddSparse(theResult->answerMat, "add")))->DenseAddSparse(b,"minus");
                cvScaleAdd(Adx,cvScalar((1.0-beta)*t2/beta),whileLoopTest,whileLoopTest);
                minValue2 = MinElem(whileLoopTest);
            }
            cvReleaseMat(&Adx);
            cvReleaseMat(&whileLoopTest);
            // backtracking line search

            //      while(0.5*t*(x+t2*dx)'*Q*(x+t2*dx) + t*c'*(x+t2*dx) - sum(log(b-A*(x+t2*dx))) > 0.5*t*x'*Q*x + t*c'*x - sum(log(b-A*x)) + alpha*t2*g'*dx) {

            //0.5*t*(x+t2*dx)'*Q*(x+t2*dx)

            CvMat* xt2dx = cvCreateMat(cvGetSize(dx).height,cvGetSize(dx).width,CV_64FC1);
            cvScaleAdd(dx,cvScalar(t2),theResult->answerMat,xt2dx);
            //CvMat *Qxt2dx=Q->SparseDenseMult(xt2dx);
            CvMat *Qxt2dx=ZeroMat(_Q->m,1);
            cs_gaxpy(_Q,xt2dx->data.db,Qxt2dx->data.db);

            double thexdxQxdx = 0.5*t*cvDotProduct(Qxt2dx,xt2dx);
            cvReleaseMat(&Qxt2dx);

            //cout << "//0.5*t*x'*Q*x" << endl;
            //0.5*t*x'*Q*x
            Qxt2dx=SparseDenseMult(_Q,theResult->answerMat);
            double thet05xQx = 0.5*t*cvDotProduct(theResult->answerMat,Qxt2dx);
            cvReleaseMat(&Qxt2dx);


//cout <<  "//t*c'*(x+t2*dx)" << endl;
            //t*c'*(x+t2*dx)
            //double thetctxt2dx = t*(c->dotProduct(xt2dx));
            double thetctxt2dx = t*(dotProduct(xt2dx,_c));

            //       cout<< "//t*c'*x" << endl;
            //t*c'*x
            //double thetctx = t*(c->dotProduct(theResult->answerMat));
            double thetctx = t*(dotProduct(theResult->answerMat,_c));
            //cout << "//b-A*(x+t2*dx)" << endl;
            //b-A*(x+t2*dx)
            /*
            CvMat* Axt2dx = A->SparseDenseMult(xt2dx);
            CvMat* bAxt2dx = cvCreateMat(cvGetSize(b).height,cvGetSize(b).width,CV_64FC1);
            cvSub(b,Axt2dx,bAxt2dx);
            cvReleaseMat(&Axt2dx);
            cvReleaseMat(&xt2dx);
            */


            CvMat *bAxt2dx=cvCloneMat(b);
            cvConvertScale(bAxt2dx,bAxt2dx,-1.0,0);
            cs_gaxpy(_A,xt2dx->data.db,bAxt2dx->data.db);
            cvConvertScale(bAxt2dx,bAxt2dx,-1.0,0);
            cvReleaseMat(&xt2dx);
            //cout << "sum log" << endl;
            //sum log
            double SLbAxt2dx = sumLog(bAxt2dx);
            //cout << SLbAxt2dx << endl;
            double SLbAx = sumLog(bAx);
            //cout << SLbAx << endl;
            //delete bAx;
            cvReleaseMat(&bAxt2dx);

            //cout<<"//alpha*t2*g'*dx" << endl;
            //alpha*t2*g'*dx
            double theat2gTdx = alpha*t2*cvDotProduct(g,dx);

            //cout << "STDQP Checkpoint 2" << endl;

            //backtracking search
            while ( (thexdxQxdx+thetctxt2dx-SLbAxt2dx) > (thet05xQx+thetctx-SLbAx+theat2gTdx) )
            {
                t2 *= beta;
                if (t2 < 1e-11)
                {
                    exit_flag = 1;
                    break;
                }

                //check
                //0.5*t*(x+t2*dx)'*Q*(x+t2*dx)
                CvMat* xt2dx = cvCreateMat(cvGetSize(dx).height,cvGetSize(dx).width,CV_64FC1);
                cvScaleAdd(dx,cvScalar(t2),theResult->answerMat,xt2dx);
                //CvMat *Qxt2dx=Q->SparseDenseMult(xt2dx);
                CvMat *Qxt2dx=ZeroMat(_Q->m,xt2dx->cols);
                cs_gaxpy(_Q,xt2dx->data.db,Qxt2dx->data.db);

                thexdxQxdx = 0.5*t*cvDotProduct(Qxt2dx,xt2dx);
                cvReleaseMat(&Qxt2dx);

                thetctxt2dx = t*(dotProduct(xt2dx,_c));

                cvConvertScale(xt2dx,xt2dx,-1.0,0);
                CvMat *bAxt2dx=cvCloneMat(b);
                cs_gaxpy(_A,xt2dx->data.db,bAxt2dx->data.db);


                //sum log
                SLbAxt2dx = sumLog(bAxt2dx);
                //SLbAx = sumLog(bAx);
                //delete bAx;
                cvReleaseMat(&bAxt2dx);
                cvReleaseMat(&xt2dx);

                //alpha*t2*g'*dx
                theat2gTdx = alpha*t2*cvDotProduct(g,dx);
            }




            CvMat *Hdx=ZeroMat(H->m,1);
            cs_gaxpy(H,dx->data.db,Hdx->data.db);
            //double lambda2 = cvDotProduct(dx,H->SparseDenseMult(dx));
            double lambda2 = cvDotProduct(dx,Hdx);
            cvReleaseMat(&Hdx);
            //delete H;
            cs_spfree(H);
            cvReleaseMat(&g);
            if ((lambda2 <  tol2)||(exit_flag == 1))
            {
                cvReleaseMat(&dx);
                break;
            }

            cvScaleAdd(dx,cvScalar(t2),theResult->answerMat,theResult->answerMat);
            cvReleaseMat(&dx);
            //update bAx
            //cvReleaseMat(&Ax);
            //Ax = A->SparseDenseMult(theResult->answerMat);
            //cvSub(b, Ax, bAx);

            cvReleaseMat(&bAx);
            bAx=cvCloneMat(b);
            cvConvertScale(bAx,bAx,-1.0,0);
            cs_gaxpy(_A,theResult->answerMat->data.db,bAx->data.db);
            cvConvertScale(bAx,bAx,-1.0,0);
            //---------------------------
            //famatdebug
            /*
             ofstream outtR2("tR2_qp.txt");
            dx->print(theResult->answerMat,outtR2);
            outtR2.close();
            */
            /*if(iters>2)
            {
                cout<<"Profiling 3 newton iterations"<<endl;
                exit(-1);
            }
            */
            //-----------------------------------
        }
    }

    //famatdebug
    //cout<<"Newton method needed "<<iters<<" to converge with exit flag "<<exit_flag<<endl;
    //ofstream outxhat("xHat_qp.txt");
    //print(theResult->answerMat,outxhat);
    //outxhat.close();
    cvReleaseMat(&bAx);
    return theResult;
}

cs * GetSubMatrix(const cs *A,int xs, int xe, int ys, int ye)
{
    int xsize = xe - xs + 1; //+1 for inclusive bounds
    int ysize = ye - ys + 1;
    cs* T = cs_spalloc(xsize,ysize,A->nzmax,1,1);

    //cs sparse uses column compress data structure
    int auxRow;
    for (int j = ys; j <= ye; j++)
    {
        for (int rowP=A->p[j];rowP<A->p[j+1];++rowP)
        {
            auxRow=A->i[rowP];
            if (auxRow>=xs && auxRow<=xe)
                cs_entry(T,auxRow-xs,j-ys,A->x[rowP]);
        }
    }
    cs * result=cs_compress(T);
    cs_spfree(T);
    return result;
}
//extract just the main diagonal
double * GetSubMatrixDiag(const cs *A,int xs, int xe, int ys, int ye)
{
    int xsize = xe - xs + 1; //+1 for inclusive bounds
    int ysize = ye - ys + 1;

    assert(xsize==ysize);
    double *result=new double[xsize];

    //cs sparse uses column compress data structure
    for (int j = ys; j <= ye; j++)
    {
        for (int rowP=A->p[j];rowP<A->p[j+1];++rowP)
        {
            if (A->i[rowP]-xs==j-ys)
                result[j-ys]=A->x[rowP];
        }
    }
    return result;
}

CvMat* GetSubMatrix(int xs, int xe, int ys, int ye,CvMat *dense)
{
    int xsize = xe - xs + 1; //+1 for inclusive bounds
    int ysize = ye - ys + 1;
    CvMat *result=cvCreateMat(xsize, ysize,CV_64FC1);

    for (int i = xs; i <= xe; i++)
    {
        for (int j = ys;j<=ye;j++)
        {
            cvSetReal2D(result,i-xs,j-ys,cvGetReal2D(dense,i,j));
        }
    }
    return result;
}

//famatnote: sparse x dense returns a dense matrix!!
//inspired by the function cs_gaxpy
CvMat* SparseDenseMult(cs *S,CvMat* dense)
{
    assert(S->n == cvGetSize(dense).height);
    CvMat *result=ZeroMat(S->m, cvGetSize(dense).width);

    int p, n, *Ap, *Ai ;
    double *Ax ,*denseData,*resultData;
    n = S->n ;
    Ap = S->p ;
    Ai = S->i ;
    Ax = S->x ;
    denseData=dense->data.db;
    resultData=result->data.db;
    int nCols = result->cols;
    for (int cc=0;cc<nCols;cc++)
    {
        for (int j = 0 ; j < n ; j++)
        {
            for (p = Ap [j] ; p < Ap [j+1] ; p++)
            {
                resultData[nCols*Ai[p]+cc] += (Ax [p] * denseData[nCols*j+cc]);
            }
        }
    }
    return result;
}

double dotProduct(CvMat *dense,cs *sparse)
{
    double result=0.0;
    assert(cvGetSize(dense).width==1);
    assert(sparse->n==1);
    assert(sparse->nz==-1);//sparse matrix has to be in compressed form
    for (int jj=0;jj<sparse->p[1];jj++)
        result+=(dense->data.db[sparse->i[jj]]*sparse->x[jj]);
    return result;
}


void print(CvMat *matrix,ostream &out)
{
    for (int i = 0; i < cvGetSize(matrix).height; i++)
    {
        for (int j = 0; j < cvGetSize(matrix).width; j++)
        {
            out << cvGetReal2D(matrix,i,j) << "  ";
        }
        out << endl;
    }
    out << endl;
}


//copy of cs_print
void print(cs *A,ostream &out)
{
    assert(A->nz==-1);
    int p, j, m, n, nzmax, *Ap, *Ai ;
    double *Ax ;
    if (A==NULL)
    {
        printf ("(null)\n") ;
        return ;
    }
    m = A->m ;
    n = A->n ;
    Ap = A->p ;
    Ai = A->i ;
    Ax = A->x ;
    nzmax = A->nzmax ;

    for (j = 0 ; j < n ; j++)
    {
        for (p = Ap [j] ; p < Ap [j+1] ; p++)
        {
            //write out a triplet
            out<<Ai[p]<<" "<<j<<" "<<Ax[p]<<endl;
        }
    }

    return ;
}

//based on std_qp.cpp
//this one solves LP with sparse matrices using newton method
// min c'*x
//subject to Ax<=b
//resultis returned in x0
void LPsolver(cs* _c, cs* _A, CvMat* b, CvMat* x0)
{
    //make sure sparse matrices are column compress format
    if (_c->nz!=-1) _c=cs_compress(_c);
    if (_A->nz!=-1) _A=cs_compress(_A);
    cs *_T;

    //final result returned. Initialize it
    CvMat *xHat=cvCloneMat(x0);

    //PrintCvMat(theResult->answerMat);
    double alpha = 0.25;
    double beta = 0.5;
    double tol1 = 1e-3;
    double tol2 = 1e-3;
    int NEWTON_MAXITERS = 20;
    int QP_MAXITERS = 200;
    int exit_flag = 0;
    double mu = 10.0;
    double t = 1.0/mu;
    double n = cvGetSize(x0).height;
    int iters = 1;

    /*
    CvMat* Ax = A->SparseDenseMult(theResult->answerMat);
    CvMat* bAx=cvCreateMat(cvGetSize(b).height,cvGetSize(b).width,CV_64FC1);
    cvSub(b, Ax, bAx);
    */

    CvMat *bAx=cvCloneMat(b);
    cvConvertScale(bAx,bAx,-1.0,0);
    cs_gaxpy(_A,xHat->data.db,bAx->data.db);
    cvConvertScale(bAx,bAx,-1.0,0);

     //famatdebug
    /*
    ofstream outbAx("bAx_LP.txt");
    print(bAx,outbAx);
    outbAx.close();
    ofstream outA("A_LP.txt");
    print(_A,outA);
    outA.close();
    ofstream outb("b_LP.txt");
    print(b,outb);
    outb.close();
    ofstream outx("x0_LP.txt");
    print(x0,outx);
    outx.close();
    ofstream outc("c_LP.txt");
    print(_c,outc);
    outc.close();
    std::cout<<"Fnished printing A,b,c,Q for std_LP"<<std::endl;
    //exit(-1);
    */
    //------------------


    //bAx
    double minValue = MinElem(bAx);
    // cout << "Min of b: " << MinElem(b) << endl;
    // cout << "MIN: " << minValue << endl;

    //check unfeasible starting point

    //PrintCvMat(bAx);
    if (minValue < 0)
    {
        cout << "Infeasible starting point for LP" << endl;
        //theResult->exit_flag = 2;
        //theResult->gap = -1.0*(minValue);
    }

    while ( (n/t > tol1) && (exit_flag == 0) && (iters<QP_MAXITERS) )
    {
        //cout << "Within WHILE loop" << endl;
        t *= mu;

        for (int iter = 1; iter <= NEWTON_MAXITERS; iter++)
        {
            //cout << "Within ITERS" << endl;

            iters++;
            //theResult->numItrs = iters;


            //compute gradient and Hessian
            _T=cs_spalloc(cvGetSize(bAx).height,cvGetSize(bAx).height,cvGetSize(bAx).height,1,1);
            for (int ii=0;ii<cvGetSize(bAx).height;ii++)
                cs_entry(_T,ii,ii,1.0/(cvGetReal2D(bAx,ii,0)));
            cs *diagM=cs_compress(_T);
            cs_spfree (_T);

            _T=cs_multiply(diagM,_A);
            cs_spfree(diagM);
            cs *auxOp=cs_transpose(_T,1);
            cs *H=cs_multiply(auxOp,_T);
            cs_spfree(_T);
            cs_spfree(auxOp);
            //drop small entries
            cs_droptol(H,sparseMatrixZeroVal);

            CvMat *g=cvCreateMat(H->m,1,CV_64FC1);
            cvSetZero(g);
            for(int cc=_c->p[0];cc<_c->p[1];cc++)
                g->data.db[_c->i[cc]]=(_c->x[cc]*t);


            double *bAxInv=new double[_A->m];
            for (int ii=0;ii<_A->m;ii++)
                bAxInv[ii]=1.0/cvGetReal2D(bAx,ii,0);

            _T=cs_transpose(_A,1);
            cs_gaxpy(_T,bAxInv,g->data.db);
            cs_spfree(_T);
            delete []bAxInv;

            //-----------------------------------------


            //dx = [dx1;dx2]
            CvMat* dx = cvCreateMat(g->rows,g->cols,CV_64FC1);
            cvScale(g,dx,-1.0);
            //compute here dx=-H\g using cholesky
            cs_cholsol(1,H,dx->data.db);


            double t2 = 1;

            // forcing A*x <= b

            CvMat* xdx=cvCreateMat(cvGetSize(dx).height,cvGetSize(dx).width,CV_64FC1);
            cvAdd(xHat,dx,xdx);
            CvMat* whileLoopTest=SparseDenseMult(_A,xdx);
            cvReleaseMat(&xdx);
            cvSub(b,whileLoopTest,whileLoopTest);
            double minValue2 = MinElem(whileLoopTest);



            //cout << "STDQP checkpoint 1" << endl;

            CvMat* Adx=ZeroMat(_A->m,dx->cols);
            cs_gaxpy(_A,dx->data.db,Adx->data.db);
            while (minValue2 <= 0)
            {
                t2 = beta*t2;
                if (t2 < 1e-11)
                {
                    exit_flag = 1;
                    break;
                }
                //CvMat* whileLoopTest =  (A->SparseDenseMult((dx->SparseScalarMult(t2))->DenseAddSparse(theResult->answerMat, "add")))->DenseAddSparse(b,"minus");
                cvScaleAdd(Adx,cvScalar((1.0-beta)*t2/beta),whileLoopTest,whileLoopTest);
                minValue2 = MinElem(whileLoopTest);
            }
            cvReleaseMat(&Adx);
            cvReleaseMat(&whileLoopTest);
            // backtracking line search

            //      while(t*c'*(x+t2*dx) - sum(log(b-A*(x+t2*dx))) >  t*c'*x - sum(log(b-A*x)) + alpha*t2*g'*dx)


            CvMat* xt2dx = cvCreateMat(cvGetSize(dx).height,cvGetSize(dx).width,CV_64FC1);
            cvScaleAdd(dx,cvScalar(t2),xHat,xt2dx);


//cout <<  "//t*c'*(x+t2*dx)" << endl;
            //t*c'*(x+t2*dx)
            //double thetctxt2dx = t*(c->dotProduct(xt2dx));
            double thetctxt2dx = t*(dotProduct(xt2dx,_c));

            //       cout<< "//t*c'*x" << endl;
            //t*c'*x
            //double thetctx = t*(c->dotProduct(theResult->answerMat));
            double thetctx = t*(dotProduct(xHat,_c));
            //cout << "//b-A*(x+t2*dx)" << endl;
            //b-A*(x+t2*dx)
            /*
            CvMat* Axt2dx = A->SparseDenseMult(xt2dx);
            CvMat* bAxt2dx = cvCreateMat(cvGetSize(b).height,cvGetSize(b).width,CV_64FC1);
            cvSub(b,Axt2dx,bAxt2dx);
            cvReleaseMat(&Axt2dx);
            cvReleaseMat(&xt2dx);
            */


            CvMat *bAxt2dx=cvCloneMat(b);
            cvConvertScale(bAxt2dx,bAxt2dx,-1.0,0);
            cs_gaxpy(_A,xt2dx->data.db,bAxt2dx->data.db);
            cvConvertScale(bAxt2dx,bAxt2dx,-1.0,0);
            cvReleaseMat(&xt2dx);
            //cout << "sum log" << endl;
            //sum log
            double SLbAxt2dx = sumLog(bAxt2dx);
            //cout << SLbAxt2dx << endl;
            double SLbAx = sumLog(bAx);
            //cout << SLbAx << endl;
            //delete bAx;
            cvReleaseMat(&bAxt2dx);

            //cout<<"//alpha*t2*g'*dx" << endl;
            //alpha*t2*g'*dx
            double theat2gTdx = alpha*t2*cvDotProduct(g,dx);

            //cout << "STDQP Checkpoint 2" << endl;

            //backtracking search
            while ( (thetctxt2dx-SLbAxt2dx) > (thetctx-SLbAx+theat2gTdx) )
            {
                t2 *= beta;
                if (t2 < 1e-11)
                {
                    exit_flag = 1;
                    break;
                }

                //check
                //0.5*t*(x+t2*dx)'*Q*(x+t2*dx)
                CvMat* xt2dx = cvCreateMat(cvGetSize(dx).height,cvGetSize(dx).width,CV_64FC1);
                cvScaleAdd(dx,cvScalar(t2),xHat,xt2dx);

                //t*c'*(x+t2*dx)
                //thetctxt2dx = t*(c->dotProduct(xt2dx));
                thetctxt2dx = t*(dotProduct(xt2dx,_c));


                cvConvertScale(xt2dx,xt2dx,-1.0,0);
                CvMat *bAxt2dx=cvCloneMat(b);
                cs_gaxpy(_A,xt2dx->data.db,bAxt2dx->data.db);


                //sum log
                SLbAxt2dx = sumLog(bAxt2dx);
                //SLbAx = sumLog(bAx);
                //delete bAx;
                cvReleaseMat(&bAxt2dx);
                cvReleaseMat(&xt2dx);
                //alpha*t2*g'*dx
                theat2gTdx = alpha*t2*cvDotProduct(g,dx);
            }



            //famatdebug
            /*
            ofstream outdx("dx_qp.txt");
            H->print(dx,outdx);
            outdx.close();

            ofstream outdxt("dxt_qp.txt");
            dxt->print(outdxt);
            outdxt.close();
            ofstream outdxtH("dxtH_qp.txt");
            dxtH->print(outdxtH);
            outdxtH.close();
            ofstream outtR("tR_qp.txt");
            dx->print(theResult->answerMat,outtR);
            outtR.close();
            */
            //-----------------------------------------



            CvMat *Hdx=ZeroMat(H->m,1);
            cs_gaxpy(H,dx->data.db,Hdx->data.db);
            //double lambda2 = cvDotProduct(dx,H->SparseDenseMult(dx));
            double lambda2 = cvDotProduct(dx,Hdx);
            cvReleaseMat(&Hdx);
            //delete H;
            cs_spfree(H);
            cvReleaseMat(&g);
            if ((lambda2 <  tol2)||(exit_flag == 1))
            {
                cvReleaseMat(&dx);
                break;
            }


            cvScaleAdd(dx,cvScalar(t2),xHat,xHat);
            cvReleaseMat(&dx);
            //update bAx
            //cvReleaseMat(&Ax);
            //Ax = A->SparseDenseMult(theResult->answerMat);
            //cvSub(b, Ax, bAx);

            cvReleaseMat(&bAx);
            bAx=cvCloneMat(b);
            cvConvertScale(bAx,bAx,-1.0,0);
            cs_gaxpy(_A,xHat->data.db,bAx->data.db);
            cvConvertScale(bAx,bAx,-1.0,0);
            //---------------------------
            //famatdebug
            /*
             ofstream outtR2("tR2_qp.txt");
            dx->print(xHat,outtR2);
            outtR2.close();
            */
            /*if(iters>2)
            {
                cout<<"Profiling 3 newton iterations"<<endl;
                exit(-1);
            }
            */
            //-----------------------------------
        }
    }

    //famatdebug
    //cout<<"Newton method needed "<<iters<<" to converge with exit flag "<<exit_flag<<endl;
    //ofstream outxhat("xHat_qp.txt");
    //print(xHat,outxhat);
    //outxhat.close();
    memcpy(x0->data.db,xHat->data.db,sizeof(CV_64FC1)*xHat->rows);
    cvReleaseMat(&xHat);
    cvReleaseMat(&bAx);
}


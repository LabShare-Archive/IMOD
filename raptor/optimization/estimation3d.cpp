/*
 * estimation3d.cpp - fit 3D model
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include <iostream>
#include <string>
#include <cmath>
#include <vector>
#include <algorithm>
#include "estimation3d.h"
#include "contour.h"
#include "std_qp.h"

//#define PI 3.14159265358979 famatnote:In C++ this is not very nice

using namespace std;

CvMat* CreateRandMat(int x, int y, double scale, double shift)
{
    CvMat* result = cvCreateMat(x,y,CV_64FC1);
    for (int i = 0; i < x; i++)
    {
        for (int j = 0; j < y; j++)
        {
            cvSetReal2D(result, i, j, scale*(rand()/(double)RAND_MAX)+shift);
        }

    }
    return result;
}

CvMat* repMat(CvMat* mat, int m, int n)
{
    CvMat* result = cvCreateMat(m*cvGetSize(mat).height, n*cvGetSize(mat).width, CV_64FC1);
    for (int i = 0; i < m; i++)
    {
        for (int j = 0; j < n; j++)
        {
            for (int si = 0; si < cvGetSize(mat).height; si++)
            {
                for (int sj = 0; sj < cvGetSize(mat).width; sj++)
                {
                    cvSetReal2D(result, (i*cvGetSize(mat).height)+si, (j*cvGetSize(mat).width)+sj, cvGetReal2D(mat,si,sj));
                }
            }
        }
    }
    return result;
}

estimation3ddata* estimation3D(contour& contour_x, contour& contour_y, double alpha, vector<double> tiltAngles, int W, int H, double percentile, probData* prob, CvMat* Wf, CvMat* Mf)
{
    //cout << "In estimation3D" << endl;

    int T = contour_x.getNumFrame();
    int M = contour_x.getNumTraj();

    estimation3ddata* theEstimate = new estimation3ddata(T,M);

    //create 3d rotation matrix
    CvMat* R = ZeroMat(2*T, 3);
    for (int i = 0; i < cvGetSize(R).height; i = i+2)
    {
        cvSetReal2D(R, i, 0, 1);
    }
    for (int i = 1; i < cvGetSize(R).height; i = i+2)
    {
        cvSetReal2D(R, i, 1, cos(tiltAngles[(i-1)/2]*PI/180.0));
    }
    for (int i = 1; i < cvGetSize(R).height; i = i+2)
    {
        //cvSetReal2D(R, i, 1, sin(tiltAngles[(i-1)/2])*180/PI);//famatnote: angles are in degrees. You have to do the conversion before taking the cosine
        cvSetReal2D(R, i, 2, sin(tiltAngles[(i-1)/2]*PI/180.0));//famatnote: bug from copy and paste. It is column#2 not #1
    }

    CvMat* Ralpha = ZeroMat(2,2);

    cvSetReal2D(Ralpha, 0, 0, cos(alpha*PI/180.0));//famatnote: same as above(alpha is in degrees)
    cvSetReal2D(Ralpha, 0, 1, -sin(alpha*PI/180.0));
    cvSetReal2D(Ralpha, 1, 0, sin(alpha*PI/180.0));
    cvSetReal2D(Ralpha, 1, 1, cos(alpha*PI/180.0));
    //Ralpha=[cosd(alpha)  -sind(alpha);...
    //        sind(alpha)   cosd(alpha)];

    for (int j = 0; j < T; j++)
    {
        CvMat* Rsub = ZeroMat(2,3);
        CvMat* RalphaRsub = ZeroMat(2,3);
        cvGetRows(R, Rsub, 2*j, 2*j+1+1, 1);
        cvMatMul(Ralpha, Rsub, RalphaRsub);
        for (int kk = 0; kk < 3; kk++)
        {
            cvSetReal2D(theEstimate->G, 2*j, kk, cvGetReal2D(RalphaRsub, 0, kk));
            cvSetReal2D(theEstimate->G, 2*j+1, kk, cvGetReal2D(RalphaRsub, 1, kk));
        }
        cvReleaseMat(&Rsub);
        cvReleaseMat(&RalphaRsub);
    }
    //G(2*j-1:2*j,:)=Ralpha*R(2*j-1:2*j,:);
    cvReleaseMat(&R);
    cvReleaseMat(&Ralpha);

    //updating parts of prob.a that require G
    for (int j = 2*T; j < 2*T+3*M; j++)
    {
        int colG=(j-2*T)%3;
        for(int ii = prob->a->p[j];ii < prob->a->p[j+1];ii++)
        {
            //cout<<"i="<<(prob->a->i[ii])%(2*T)<<";j="<<colG<<std::endl;
            (prob->a->x[ii])*=(cvGetReal2D(theEstimate->G,(prob->a->i[ii])%(2*T),colG));
        }

        //prob.a(2*T*M+2*T*(j-1)+1:2*T*M+2*T*j,3*(j-1)+2*T+1:2*T+3*j)=prob.a(2*T*M+2*T*(j-1)+1:2*T*M+2*T*j,3*(j-1)+2*T+1:2*T+3*j).*G;
    }


    prob->x0 = CreateRandMat(4*T*M+2*T+3*M, 1, 2, -1);
    //prob.x0=2*rand(4*T*M+2*T+3*M,1)-1;

    for (int i = 0; i < 2*T; i++)
    {
        cvSetReal2D(prob->x0, i, 0, 10*cvGetReal2D(prob->x0, i, 0));
    }
    //prob.x0(1:2*T)=10*prob.x0(1:2*T);

    for (int i = 2*T+3*M; i < 2*T+3*M+2*T*M; i++)
    {
        cvSetReal2D(prob->x0, i, 0, (rand()/(double)RAND_MAX)*3+100);
    }
    //prob.x0(2*T+3*M+1:2*T+3*M+2*T*M)=100+3*rand(2*T*M,1);

    for (int i = 2*T*M+3*M+2*T; i < 4*T*M+2*T+3*M; i++)
    {
        //famatnote: here there is an error to generate initial point for optimization
        //cvSetReal2D(prob->x0, 2*T*M+3*M+2*T, 0, cvGetReal2D(prob->buc, (cvGetSize(prob->buc).width)/2, 0) + 2*(rand()/RAND_MAX)-1);
        cvSetReal2D(prob->x0, i, 0, cvGetReal2D(prob->buc,i-(2*T*M+3*M+2*T), 0) + 2*(rand()/RAND_MAX)-1);
    }
    //prob.x0(2*T*M+3*M+2*T+1:end)=prob.buc(1:end/2)+2*rand(2*T*M,1)-1;

    //regularization on the translation parameter
    cs *_T=cs_spalloc(prob->Q->m,prob->Q->n,2*T,1,1);
    for (int kk = 0; kk < 2*T; kk++)
    {
        cs_entry(_T,kk,kk, 1e-6);
    }
    cs *_TT=cs_compress(_T);
    prob->Q=cs_add(prob->Q,_TT,1.0,1.0);
    cs_spfree(_T);cs_spfree(_TT);

    //cout<< "before stdqp"  << endl;

    //famatdebug------------------------------------------------------
    /*
    char sac[256];
    if (alpha<0)
        sprintf (sac, "m%2.0f", -alpha);
    else
        sprintf (sac, "p%2.0f", alpha);
    string sa(sac);
    ofstream outPD(("probDataBeforeQP_alpha" + sa + ".m").c_str());
    prob->printMatlab(outPD);
    outPD.close();
    */
    //-------------------------------

    //[xx,itersQP,exit_flag]
    STDQPdata* theResult = std_qp(prob->Q,prob->c,prob->a,prob->buc,prob->x0,M,T,"prob");

    //famatdebug
    /*
    ofstream outXX("xx_qp.txt");
    print(theResult->answerMat,outXX);
    outXX.close();
    cout<<"Finished writing xx"<<endl;
    */
    //-----------------------------
    //cout<< "after stdqp"  << endl;

    if (theResult->numItrs>200)
    {
        theEstimate->residMeanPerc=1e6;
        theEstimate->residMean=1e6;
        delete theResult;
        return theEstimate;
    }

    //CvMat* t = ZeroMat(2*T, 1);
    for (int i = 0; i < 2*T; i++)
    {
        cvSetReal2D(theEstimate->t, i, 0, cvGetReal2D(theResult->answerMat, i, 0));
    }
    //t=theResult.xx(1:2*T);

    //CvMat* P = ZeroMat(3, M);
    for (int i = 2*T,posP = 0; i < 2*T+3*M; i+=3,posP++)
    {
        cvSetReal2D(theEstimate->P, 0, posP, cvGetReal2D(theResult->answerMat, i, 0));
        cvSetReal2D(theEstimate->P, 1, posP, cvGetReal2D(theResult->answerMat, i+1, 0));
        cvSetReal2D(theEstimate->P, 2, posP, cvGetReal2D(theResult->answerMat, i+2, 0));
    }
    //famatnote: the reshape in matlab is done by columns, not by rows. You were getting values of P wring
    //P=reshape(xx(2*T+1:2*T+3*M),[3 M]);

    CvMat* GP = ZeroMat(cvGetSize(theEstimate->G).height, cvGetSize(theEstimate->P).width);
    cvMatMul(theEstimate->G,theEstimate->P, GP);
    CvMat* rept = repMat(theEstimate->t, 1, M);

    CvMat* MfGP = ZeroMat(cvGetSize(Mf).height, cvGetSize(Mf).width);
    cvSub(Mf, GP, MfGP, NULL);
    CvMat* MfGPrept = ZeroMat(cvGetSize(MfGP).height, cvGetSize(MfGP).width);
    cvSub(MfGP, rept, MfGPrept, NULL);
    CvMat* resid = ZeroMat(cvGetSize(MfGPrept).height, cvGetSize(MfGPrept).width);
    cvMul(MfGPrept, MfGPrept,resid);
    //resid=(Mf-G*P-repmat(t,[1,M])).^2;

    //famatdebug
    /*
    ofstream outt("t.txt");
    print(theEstimate->t,outt);
    outt.close();
    ofstream outP("P.txt");
    print(theEstimate->P,outP);
    outP.close();
    ofstream outG("G.txt");
    print(theEstimate->G,outG);
    outG.close();
    ofstream outGP("GP.txt");
    print(GP,outGP);
    outGP.close();
    cout<<"Finished printing G,P,Mf"<<cout;
    */
    //------------------

    cvReleaseMat(&GP);
    cvReleaseMat(&rept);
    cvReleaseMat(&MfGP);
    cvReleaseMat(&MfGPrept);



    vector<double> residAux;//we compute residual here
    for (int i = 0; i < cvGetSize(Wf).height; i+=2)
    {
        for (int j = 0; j < cvGetSize(Wf).width; j++)
        {
            if (cvGetReal2D(Wf, i, j) > (minWeight*1.1))//famatnote: never use == with doubles
                residAux.push_back(sqrt(cvGetReal2D(resid, i, j)+cvGetReal2D(resid, i+1, j)));
        }
    }
    cvReleaseMat(&resid);
    //resid(find(Wf==minWeight))=0;


    sort(residAux.begin(), residAux.end());
//    residAux=sort(resid(find(resid~=0)));



    vector<double> finalResid;
    if (residAux.empty())
    {
        theEstimate->residMean=0.0;
        theEstimate->residMeanPerc=0.0;
    }
    else
    {
        int size = (int)ceil(residAux.size()*percentile);
        double sum=0.0;
        for (int i = 0; i < size; i++)
        {
            sum += residAux[i];
        }
        theEstimate->residMeanPerc = sum/(double)size;
        for (unsigned int i = size; i < residAux.size(); i++)
        {
            sum += residAux[i];
        }
        theEstimate->residMean = sum/(double)residAux.size();;
        //residMean=mean(residAux);
    }

    delete theResult;
    //cout<< "estimate done"  << endl;
    return theEstimate;
}



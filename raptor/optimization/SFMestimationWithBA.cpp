/*
 * SFMestimationWithBA.cpp - estimate SFM problem
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include "SFMestimationWithBA.h"
#include <list>
#include "estimation3d.h"
#include "std_qp.h"
#include <algorithm>


using namespace std;


CvMat* Stack(CvMat* mat)
{
    int x = cvGetSize(mat).height;
    int y = cvGetSize(mat).width;
//    int numElems = x*y;
    CvMat* result = cvCreateMat(x*y, 1, CV_64FC1);
    for  (int j = 0; j < y; j++)
    {
        for (int i = 0; i < x; i++)
        {
            cvSetReal2D(result, i + j*x, 0, cvGetReal2D(mat, i, j));
        }
    }
    return result;
}

CvMat* MakeDiag(CvMat* mat)
{
    assert(cvGetSize(mat).width == 1);
    CvMat* result = cvCreateMat(cvGetSize(mat).height, cvGetSize(mat).height, CV_64FC1);
    cvSetZero(result);
    for (int i = 0; i < cvGetSize(mat).height; i++)
    {
        cvSetReal2D(result, i, i, cvGetReal2D(mat, i, 0));
    }
    return result;
}

CvMat* ConcatenateMatrix(CvMat* one, CvMat* two)
{
    CvMat* dst = cvCreateMat(cvGetSize(one).height, cvGetSize(one).width + cvGetSize(two).width, CV_64FC1);
    for (int i = 0; i < cvGetSize(one).height; i++)
    {
        for (int j = 0; j < cvGetSize(one).width; j++)
        {
            cvSetReal2D(dst, i, j, cvGetReal2D(one, i, j));
        }
    }
    for (int i = 0; i < cvGetSize(two).height; i++)
    {
        for (int j = 0; j < cvGetSize(two).width; j++)
        {
            cvSetReal2D(dst, i, j+cvGetSize(one).width, cvGetReal2D(two, i, j));
        }
    }
    return dst;
}


CvMat* ConcatenateMatrixDown(CvMat* one, CvMat* two)
{
    CvMat* dst = cvCreateMat(cvGetSize(one).height+cvGetSize(two).height, cvGetSize(one).width, CV_64FC1);
    for (int i = 0; i < cvGetSize(one).height; i++)
    {
        for (int j = 0; j < cvGetSize(one).width; j++)
        {
            cvSetReal2D(dst, i, j, cvGetReal2D(one, i, j));
        }
    }
    for (int i = 0; i < cvGetSize(two).height; i++)
    {
        for (int j = 0; j < cvGetSize(two).width; j++)
        {
            cvSetReal2D(dst, i+cvGetSize(one).height, j, cvGetReal2D(two, i, j));
        }
    }
    return dst;
}

double norm(CvMat* mat, int L)
{
    double sum = 0;
    for (int i = 0; i < cvGetSize(mat).height; i++)
    {
        for (int j = 0; j < cvGetSize(mat).width; j++)
        {
            double subsum = cvGetReal2D(mat, i, j);
            subsum = pow(pow(subsum,2),.5);
        }
    }
    return sum;
}

CvMat* OnesMat(int x, int y)
{
    CvMat* result = cvCreateMat(x,y,CV_64FC1);
    cvSet(result, cvScalar(1), NULL);
    return result;
}

SFMdata* SFMestimationWithBA(SFMdata *sfm_, vector<double> tiltAngles, int W, int H, float percentile, double *alphaFinal, int option,bool debugMode)
{

    //to be able to debug with valgrind
    //cvSetErrMode(CV_ErrModeParent);
    cvSetErrMode(CV_ErrModeLeaf);

    //cout << "Starting SFMestimationWithBA" << endl;
    int T = sfm_->contour_x->getNumFrame();
    int M = sfm_->contour_x->getNumTraj();

//    cout << "W:" << W << " H: " << H << " T: " << T << " M: " << M << endl;



    SFMdata* sfm =  new SFMdata(*sfm_);
    probData* theProb = new probData();

    //----------------------------------------------------------------------
    CvMat* Mf = ZeroMat(2*T, M);
    CvMat* Wf = OnesMat(2*T, M);

    CvMat* b = cvCreateMat(2*T,M,CV_64FC1);//helps creating constrains later
    //---------------------------------------------------------------------
    CvMat* contourxrow=cvCreateMat(M,1,CV_64FC1);
    CvMat* contourxrowtr=cvCreateMat(1,M,CV_64FC1);
    CvMat* contouryrow=cvCreateMat(M,1,CV_64FC1);
    CvMat* contouryrowtr=cvCreateMat(1,M,CV_64FC1);;

    double W_2=0.5*(double)W;
    double H_2=0.5*(double)H;

    for (int j = 0; j < T; j++)
    {
        cvGetCol(sfm_->contour_x->scores,contourxrow, j);
        cvTranspose(contourxrow,contourxrowtr);
        cvConvertScale(contourxrowtr,contourxrowtr,1.0,-W_2);

        for (int i = 0; i < M; i++)
        {
            double elem = cvGetReal2D(contourxrowtr, 0, i);
            cvSetReal2D(Mf, j+j, i, elem);
            //famatnote: Wf is just weights (between 0 and 1). So setiing it to elem is not right
            /*
            if (elem == 0)//famatnote: elem is a double. Checking a double with == is always very dangerous
                cvSetReal2D(Wf, 2*j, i, minWeight);
            else
                cvSetReal2D(Wf, 2*j, i, elem);
            */
            if (fabs(elem+W_2)<0.1)
            {
                cvSetReal2D(Wf, j+j, i, minWeight);//otherwise we leave it as a one
                cvSetReal2D(b,j+j,i,minWeight*elem);
            }
            else
            {
                cvSetReal2D(b,j+j,i,elem);
            }
        }

        cvGetCol(sfm_->contour_y->scores,contouryrow, j);
        cvTranspose(contouryrow,contouryrowtr);
        cvConvertScale(contouryrowtr,contouryrowtr,1.0,-H_2);

        for (int i = 0; i < M; i++)
        {
            double elem = cvGetReal2D(contouryrowtr, 0, i);
            cvSetReal2D(Mf, j+j+1, i, cvGetReal2D(contouryrowtr, 0, i));
            if (fabs(elem+H_2)<0.1)
            {
                cvSetReal2D(Wf, j+j+1, i, minWeight);//otherwise we leave it as a one
                cvSetReal2D(b,j+j+1,i,minWeight*elem);
            }
            else
            {
                cvSetReal2D(b,j+j+1,i,elem);
            }
        }
    }

    cvReleaseMat(&contourxrow);
    cvReleaseMat(&contouryrow);
    cvReleaseMat(&contourxrowtr);
    cvReleaseMat(&contouryrowtr);
    //cout << "Done creating Mf and Wf" << endl;

    //---------------------------------------------
    //famatdebug
    /*
    ofstream outMf("Mf.txt");
    ofstream outWf("Wf.txt");
    for (int i = 0; i < cvGetSize(Mf).height; i++)
    {
        for (int j = 0; j < cvGetSize(Mf).width; j++)
        {
            outMf << cvGetReal2D(Mf,i,j) << "  ";
            outWf << cvGetReal2D(Wf,i,j) << "  ";
        }
        outMf << endl;
        outWf << endl;
    }
    outMf << endl;
    outWf << endl;
    outMf.close();
    outWf.close();
    exit(-1);
    */
    //-----------------------------------------------

    //---------------------------------------------------------------------------

    // create constraint matrix from scratch
    //famat note: we can synthetize this piece of code
    /*
    CvMat* iMat = ZeroMat(1,(4*T*M+2*T+3*M)-(2*T*M+2*T+3*M+1)+1);
    for (int i = 0; i < (4*T*M+2*T+3*M)-(2*T*M+2*T+3*M+1)+1; i++)
    {
        cvSetReal2D(iMat, 0, i, 2*T*M+2+3*M+1+i); //famatnote:bug. It should be +2*T
                                                //famatnote: Matlab indexing starts in 1 but C++ starts in 0.
    }

    CvMat* qosubi = iMat;
    CvMat* qosubj = iMat;
    CvMat* qoval = OnesMat(1, cvGetSize(iMat).width);

    // generate Q matrix
    theProb->Q = new sparseMat(2*T+3*M+4*T*M, 2*T+3*M+4*T*M);
    for (int i = 0; i < cvGetSize(iMat).width; i++)
    {
        int x = (int)cvGetReal2D(qosubi,0,i);
        int y = (int)cvGetReal2D(qosubj,0,i);
        double elem = cvGetReal2D(qoval, 0,i);
        theProb->Q->addElem(x,y,elem);
    }

    cvReleaseMat(&iMat);
    cvReleaseMat(&qoval);
    */

    //--------------------------
    //famatdebug
    /*
    ofstream outContourx("contour_x.txt");
    contour_x.print(outContourx);
    outContourx.close();
    ofstream outContoury("contour_y.txt");
    contour_y.print(outContoury);
    outContoury.close();
    */
    //---------------------------
    cs *_T;
    // generate Q matrix
    _T = cs_spalloc(2*T+3*M+4*T*M, 2*T+3*M+4*T*M,2*T*M,1,1);
    for (int i = 2*T*M+2*T+3*M; i < (4*T*M+2*T+3*M); i++)
    {
        cs_entry(_T,i,i,1.0);
    }
    theProb->Q=cs_compress(_T);
    cs_spfree(_T);
    //----------------------------------------------------


    //generate c matrix
    _T = cs_spalloc(4*T*M+2*T+3*M, 1,2*T*M,1,1);
    for (int i = 2*T+3*M; i<(2*T+3*M+2*T*M); i++)
    {
        //double elem = delta;
        cs_entry(_T,i,0,delta);
    }
    theProb->c=cs_compress(_T);
    cs_spfree(_T);
    //----------------------------------------
    //CvMat* b = cvCreateMat(2*T,M,CV_64FC1);
    //cvMul(Wf, Mf, b);  //famatnote: this can be done earlier when we crate Wf and Mf

    CvMat* stackb = Stack(b);
    //CvMat* negb = cvCloneMat(stackb); //famatnote: you don;t need to clone the matrix. Just allocate memory
    CvMat *negb = cvCreateMat(cvGetSize(stackb).height,cvGetSize(stackb).width,CV_64FC1);
    cvConvertScale(stackb,negb,-1,0);

    //theProb->buc = ConcatenateMatrixDown(stackb, negb);//famatnote: buc needs to be concatenated as [negb stackb]
    theProb->buc = ConcatenateMatrixDown(negb,stackb);

    cvReleaseMat(&b);
    cvReleaseMat(&stackb);


    //create part of matrix A
    _T=cs_spalloc(4*T*M, 2*T+3*M+4*T*M,24*T*M,1,1);
    for (int j = 1; j <= M; j++)
    {
        //
        //cout << "CYCLE " << j << ":" << endl;
        for (int k = 2*T*(j-1) , r = 0; k < 2*T*j; k++,r++)
        {
            cs_entry(_T,k,r,-cvGetReal2D(Wf,r,j-1));
        }

        for (int k = 2*T*(j-1), r = 3*M+2*T+(2*T*(j-1)); k < 2*T*j; k++, r++)
        {
            cs_entry(_T,k,r,-1.0);
        }
        for (int k = 2*T*(j-1),r = 3*M+2*T+2*T*M+2*T*(j-1); k < 2*T*j; k++, r++)
        {
            cs_entry(_T,k,r,1.0);
        }
        for (int k = 2*T*M+2*T*(j-1),r = 0; k < 2*T*M+2*T*j; k++, r++)
        {
            cs_entry(_T,k,r,cvGetReal2D(Wf,r,j-1));
        }
        //
        for (int k = 2*T*M+2*T*(j-1), r = 3*M+2*T+2*T*(j-1); k < 2*T*M+2*T*j; k++, r++)
        {
            cs_entry(_T,k,r,-1.0);
        }
        //
        for (int k = 2*T*M+2*T*(j-1), r = 3*M+2*T+2*T*M+2*T*(j-1); k < 2*T*M+2*T*j; k++, r++)
        {
            cs_entry(_T,k,r,-1.0);
        }
    }



    if (debugMode) cout << "Done generating first part of prob->a structure" << endl;

    for (int j = 1; j <= M; j++)
    {
        for (int m = 2*T*(j-1),cc=0; m< 2*T*j; m++,cc++)
        {
            double auxWf=cvGetReal2D(Wf,cc,j-1);
            for (int n = 3*(j-1)+2*T; n < 2*T+3*j; n++)
            {
                cs_entry(_T,m,n,-auxWf);
                cs_entry(_T,m+2*T*M,n,auxWf);
            }
        }
    }

    //  prob.blc=repmat(-inf,[4*T*M 1]);
    //prob.blx=[];
    //prob.bux=[];
    theProb->a=cs_compress(_T);//even if we add stuff to A later, it won't be in a new position
    cs_spfree(_T);


    //remove zero entries from sparse matrix after weighting
    cs_dropzeros(theProb->a);

//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
    if (debugMode) cout << "Done creating second part of structure prob-a" << endl;

    double residMeanFinal;
    //double alphaFinalC = (*alphaFinal);
    CvMat* alpha;

    //if (option == 0 || option == 4)
    if (option == 0)
    {

        //setting up alpha

        int countAlphaVec=0;
        alpha = ZeroMat(1,10);
        for (double i = -81; i < 87; i +=18)
            //alpha = ZeroMat(1,2);
            //cout<<"!!!!!!!!WARNING!!!!!!!!!!!!!!:alpha limits changed to test with valgrind"<<endl;
            //for (double i = -9; i < 12; i +=18)
        {
            cvSetReal2D(alpha, 0, countAlphaVec ,i);
            countAlphaVec++;
        }

        //coarse search for alpha
        residMeanFinal = 1e+20;
        for (int k = 0; k < cvGetSize(alpha).width; k++)
        {
            probData* copyProb = new probData(*theProb); //good move. Otherwise strcuture will change for each call to estimation3D
            double theAlpha = cvGetReal2D(alpha, 0, k);

            //famatdebug------------------------------
            /*
            char sac[256];
            if(theAlpha<0)
                sprintf (sac, "m%2.0f", -theAlpha);
            else
                sprintf (sac, "p%2.0f", theAlpha);
            string sa(sac);
            ofstream outPD(("probDataBeforeE3D_alpha" + sa + ".m").c_str());
            copyProb->printMatlab(outPD);
            outPD.close();
            */
            //-------------------------------
            time_t start,end;
            time(&start);
            estimation3ddata* tresidMeanStruct = estimation3D(*(sfm_->contour_x), *(sfm_->contour_y), theAlpha, tiltAngles, W, H, percentile, copyProb, Wf, Mf);
            time(&end);
            delete copyProb;
            if (debugMode)
            {
                cout<<"Estimation 3D for alpha took "<<difftime(end,start)<<" secs"<<endl;
                cout<<"Residual ="<< tresidMeanStruct->residMean <<" for alpha="<<theAlpha<<endl;
                cout<<"Residual ="<< tresidMeanStruct->residMeanPerc <<" for alpha="<<theAlpha<<" with percentile="<<percentile<<endl;
            }
            if (tresidMeanStruct->residMeanPerc < residMeanFinal)
            {
                residMeanFinal = tresidMeanStruct->residMeanPerc;
                (*alphaFinal) = cvGetReal2D(alpha, 0, k);
            }
            delete tresidMeanStruct;

            //exit(-1);//for valgrind memcheck
            //cout << k+1 << "th end loop" << endl;
        }
        if (debugMode) cout << "First coarse search for alpha" << endl;

        cvReleaseMat(&alpha);
        //fine search for alpha
        alpha = cvCreateMat(1, 6, CV_64FC1);
        countAlphaVec=0;
        for (double i = (*alphaFinal)-9.0; i <= (*alphaFinal)+10.0; i +=3.0)
        {
            if (fabs(i-(*alphaFinal))<1e-3)
                continue;//we don't need to optimize again for the oarse value found in previous search
            cvSetReal2D(alpha, 0, countAlphaVec , i);
            countAlphaVec++;
        }

        for (int k = 0; k < cvGetSize(alpha).width; k++)
        {
            probData* copyProb = new probData(*theProb);
            double theAlpha = cvGetReal2D(alpha, 0, k);
            estimation3ddata* tresidMeanStruct = estimation3D(*(sfm_->contour_x), *(sfm_->contour_y), theAlpha, tiltAngles, W, H, percentile, copyProb, Wf, Mf);
            delete copyProb;
            if (debugMode)
            {
                cout<<"Residual ="<< tresidMeanStruct->residMean <<" for alpha="<<theAlpha<<endl;
                cout<<"Residual ="<< tresidMeanStruct->residMeanPerc <<" for alpha="<<theAlpha<<" with percentile="<<percentile<<endl;
            }
            if (tresidMeanStruct->residMeanPerc < residMeanFinal)
            {
                residMeanFinal = tresidMeanStruct->residMeanPerc;
                (*alphaFinal) = cvGetReal2D(alpha, 0, k);
            }
            delete tresidMeanStruct;
        }
        if (debugMode) cout <<"Fine search for alpha finished" <<endl;
        cvReleaseMat(&alpha);

        //very fine search for alpha
        alpha = cvCreateMat(1,2,CV_64FC1);
        cvSetReal2D(alpha, 0,0, (*alphaFinal)-1.0);
        cvSetReal2D(alpha, 0,1, (*alphaFinal)+1.0);
        for (int k = 0; k < 2; k++)
        {
            probData* copyProb = new probData(*theProb);
            double theAlpha = cvGetReal2D(alpha, 0, k);
            estimation3ddata* tresidMeanStruct = estimation3D(*(sfm_->contour_x), *(sfm_->contour_y), theAlpha, tiltAngles, W, H, percentile, copyProb, Wf, Mf);
            delete copyProb;
            if (debugMode)
            {
                cout<<"Residual ="<< tresidMeanStruct->residMean <<" for alpha="<<theAlpha<<endl;
                cout<<"Residual ="<< tresidMeanStruct->residMeanPerc <<" for alpha="<<theAlpha<<" with percentile="<<percentile<<endl;
            }
            if (tresidMeanStruct->residMeanPerc < residMeanFinal)
            {
                residMeanFinal = tresidMeanStruct->residMeanPerc;
                (*alphaFinal) = cvGetReal2D(alpha, 0, k);
            }
            delete tresidMeanStruct;

        }
        cvReleaseMat(&alpha);
        if (debugMode) cout << "Very fine search for alpha finished" << endl;
    }
    else if (option == 2)
    {
        //very fine search for alpha
        alpha = cvCreateMat(1, 5, CV_64FC1);
        cvSetReal2D(alpha, 0, 0, (*alphaFinal)-0.5);
        cvSetReal2D(alpha, 0, 1, (*alphaFinal)-0.25);
        cvSetReal2D(alpha, 0, 2, (*alphaFinal));
        cvSetReal2D(alpha, 0, 3, (*alphaFinal)+0.25);
        cvSetReal2D(alpha, 0, 4, (*alphaFinal)+0.5);

        residMeanFinal = 1e20;
        for (int k = 0; k <  cvGetSize(alpha).width; k++)
        {
            probData* copyProb = new probData(*theProb);
            double theAlpha = cvGetReal2D(alpha, 0, k);
            estimation3ddata* tresidMeanStruct = estimation3D(*(sfm_->contour_x), *(sfm_->contour_y), theAlpha, tiltAngles, W, H, percentile, copyProb, Wf, Mf);
            delete copyProb;
            if (debugMode)
            {
                cout<<"Residual ="<< tresidMeanStruct->residMean <<" for alpha="<<theAlpha<<endl;
                cout<<"Residual ="<< tresidMeanStruct->residMeanPerc <<" for alpha="<<theAlpha<<" with percentile="<<percentile<<endl;
            }
            if (tresidMeanStruct->residMeanPerc < residMeanFinal)
            {
                residMeanFinal = tresidMeanStruct->residMeanPerc;
                (*alphaFinal) = cvGetReal2D(alpha, 0, k);
            }
            delete tresidMeanStruct;
        }
        cvReleaseMat(&alpha);
    }
    estimation3ddata* finalEstimation3d=NULL;
    if (option < 3)
    {
        probData* copyProb = new probData(*theProb);
        finalEstimation3d= estimation3D(*(sfm_->contour_x), *(sfm_->contour_y), *alphaFinal, tiltAngles, W, H, percentile, copyProb, Wf, Mf);
        delete copyProb;

        if (debugMode)
        {
            cout<<"Residual ="<< finalEstimation3d->residMean <<" for alpha="<<*alphaFinal<<endl;
            cout<<"Residual ="<< finalEstimation3d->residMeanPerc <<" for alpha="<<*alphaFinal<<" with percentile="<<percentile<<endl;
        }
        //compute reprojection matrix
        CvMat* GP = ZeroMat(cvGetSize(finalEstimation3d->G).height, cvGetSize(finalEstimation3d->P).width);
        cvMatMul(finalEstimation3d->G,finalEstimation3d->P, GP);
        CvMat* Mfhat = repMat(finalEstimation3d->t, 1, M);
        cvAdd(GP, Mfhat, Mfhat);

        cvReleaseMat(&GP);


        if (sfm->reproj_x!=NULL)
            delete sfm->reproj_x;
        sfm->reproj_x = new contour(*(sfm_->contour_x));//just to allocate memory
        if (sfm->reproj_y!=NULL)
            delete sfm->reproj_y;
        sfm->reproj_y = new contour(*(sfm_->contour_y));

        double W_2=(double)W/2.0;
        double H_2=(double)H/2.0;
        for (int ii = 0; ii < M; ii++)
        {
            // reproj_x(:,j)=Mfhat(2*j-1,:)'+W/2;
            //      reproj_y(:,j)=Mfhat(2*j,:)'+H/2;
            for (int jj=0;jj<T;jj++)
            {
                cvSetReal2D(sfm->reproj_x->scores,ii,jj,cvGetReal2D(Mfhat,jj+jj,ii)+W_2);
                cvSetReal2D(sfm->reproj_y->scores,ii,jj,cvGetReal2D(Mfhat,jj+jj+1,ii)+H_2);
            }
        }

        if (finalEstimation3d->residMeanPerc>5.0)
        {
            cout<<"WARNING: residual of percentile "<<percentile<<" is above 5."<<endl;
            cout<<"That is an indication there is most likely a problem with pairwise correspondence or the marker template."<<endl;
            cout<<"RAPTOR is unable to continue at "<<getDate()<<endl;
            exit(-1);
        }
    }
    if (finalEstimation3d==NULL)
        finalEstimation3d=new estimation3ddata();

    delete theProb;
    cout << "Finished block to estimate alpha (2D in-plane rotation)" << endl;

    //famatdebug
    /*
    ofstream outCx("cx_sfm.txt");
    print(sfm->contour_x->scores,outCx);
    outCx.close();
    ofstream outCy("cy_sfm.txt");
    print(sfm->contour_y->scores,outCy);
    outCy.close();
    ofstream outRx("rx_sfm.txt");
    print(sfm->reproj_x->scores,outRx);
    outRx.close();
    ofstream outRy("ry_sfm.txt");
    print(sfm->reproj_y->scores,outRy);
    outRy.close();
    cout<<"Finished printing out sfm results"<<endl;
    exit(-1);
    */
    //------------------------

//-----------------------------------------------------------------
//-------------------------------------------------------------------
//start factorization
    //--------------------------------------------------------------------------

    CvMat* MfWf = cvCreateMat(cvGetSize(Wf).height, cvGetSize(Wf).width, CV_64FC1);
    cvMul(Wf, Mf, MfWf);



    CvMat* Vt=NULL;
    CvMat* G=NULL;
    CvMat* t=NULL;

    int max_iter;
    double mu;
    int randIni;

    if (option < 3)
    {
        max_iter = 5;
        mu = 100000.0;
        Vt=cvCreateMat(finalEstimation3d->P->cols,finalEstimation3d->P->rows,CV_64FC1);
        cvTranspose(finalEstimation3d->P,Vt);
        G = finalEstimation3d->G;
        t = finalEstimation3d->t;
        randIni = 1;
    }
    else if (option >= 3)
    {
        max_iter = 30;
        mu = 0.0;
        G = CreateRandMat(2*T, 3, 0.01, -0.005);
        t = CreateRandMat(2*T, 1, 1, -0.5);
        Vt = CreateRandMat(M, 3, 500, 0);
        randIni=5;
    }

    CvMat* Ut = ConcatenateMatrix(G, t);//if option>=3 Ut will be created again randomly for each random Initialization
    //tol=tol*10;//famatnote: that generates an increase in tol and this is not what we want!
    CvMat* UtOld = cvCreateMat(cvGetSize(Ut).height, cvGetSize(Ut).width, CV_64FC1);
    cvAddS(Ut, cvScalar(tol*10), UtOld, NULL);

// define some of the matrices that don't change over factorization
// to solve for U
//aa=M+4+1:2*M+4;
//probU.qosubi=[1 2 3 4 aa];
//probU.qosubj=[1 2 3 4 aa];
//probU.qoval=[2*mu 2*mu 2*mu 2*mu/10 ones(1,length(aa))];

    probData* probU = new probData();


    //---------generate common probU->Q
    _T=cs_spalloc(4+2*M, 4+2*M,M+4,1,1);
    if (mu>0.0)
    {
        double _2mu=mu+mu;
        for (int ii=0;ii<3;ii++)
            cs_entry(_T,ii,ii,_2mu);

        cs_entry(_T,3,3,_2mu/10.0);
    }
    for (int ii=M+4;ii<2*M+4;ii++)
        cs_entry(_T,ii,ii,1.0);

    probU->Q=cs_compress(_T);
    cs_spfree(_T);

    //------------generate common probU->c
    _T=cs_spalloc(4+2*M, 1,M+4,1,1);
    for (int i = 4; i < 4+M; i++)
        cs_entry(_T,i,0,delta);

    for (int i = 0; i < 4; i++)
        cs_entry(_T,i,0,-1000);//to guarantee that it is there later
    probU->c=cs_compress(_T);
    cs_spfree(_T);


    _T=cs_spalloc(2*M, 4+2*M,12*M,1,1);
    for (int i = 0; i < M; i++)
    {
        cs_entry(_T,i, 4+i, -1.0);
        cs_entry(_T,i, 4+M+i, 1.0);
        cs_entry(_T,M+i, 4+i, -1.0);
        cs_entry(_T,M+i, 4+M+i, -1.0);

        //to guarantee we have the space later
        cs_entry(_T,i, 0, -1000.0);
        cs_entry(_T,M+i, 0, -1000.0);
        cs_entry(_T,i, 1, -1000.0);
        cs_entry(_T,M+i, 1, -1000.0);
        cs_entry(_T,i, 2, -1000.0);
        cs_entry(_T,M+i, 2, -1000.0);
        cs_entry(_T,i, 3, -1000.0);
        cs_entry(_T,M+i, 3, -1000.0);
    }
    probU->a=cs_compress(_T);
    cs_spfree(_T);

//-------------------------------------
//prepare matrices to solve for V

//  aa=3+2*T+1:3+4*T;
//probV.qosubi=aa;
//probV.qosubj=aa;
//probV.qoval=ones(1,2*T);

    probData* probV = new probData();

    _T=cs_spalloc(3+4*T, 3+4*T,2*T,1,1);
    for (int i = 3+2*T;i < 3+4*T ; i++)
        cs_entry(_T,i,i,1.0);

    probV->Q=cs_compress(_T);
    cs_spfree(_T);


    _T=cs_spalloc(3+4*T,1,2*T,1,1);
    for (int i = 3; i < 3+2*T; i++)
        cs_entry(_T,i,0,delta);
    probV->c=cs_compress(_T);
    cs_spfree(_T);
    //probV.c=sparse(3+1:3+2*T,ones(1,2*T),delta*ones(1,2*T),3+4*T,1,2*T);

    _T=cs_spalloc(4*T,3+4*T,20*T,1,1);
    for (int i = 0; i < 2*T; i++)
    {
        cs_entry(_T,i, 3+i, -1.0);
        cs_entry(_T,i, 3+2*T+i, 1.0);
        cs_entry(_T,2*T+i, 3+i, -1.0);
        cs_entry(_T,2*T+i, 3+2*T+i, -1.0);

        //to make sure those entries are found later
        cs_entry(_T,i, 0, -1000.0);
        cs_entry(_T,i, 1, -1000.0);
        cs_entry(_T,i, 2, -1000.0);
        cs_entry(_T,2*T+i, 0, -1000.0);
        cs_entry(_T,2*T+i, 1, -1000.0);
        cs_entry(_T,2*T+i, 2, -1000.0);
    }
    probV->a=cs_compress(_T);
    cs_spfree(_T);

    //probV.a=sparse([],[],[],4*T,3+4*T,20*T);
    //probV.a(1:2*T,3+1:3+2*T)=-eye(2*T);
    //probV.a(1:2*T,3+2*T+1:3+4*T)=eye(2*T);
    //probV.a(2*T+1:4*T,3+1:3+2*T)=-eye(2*T);
    //probV.a(2*T+1:4*T,3+2*T+1:3+4*T)=-eye(2*T);

    //finished setting up matrices for factorization method
    //-----------------------------------------------

    double residMeanOld = 1e11;

    CvMat* V = cvCreateMat(M, 3, CV_64FC1);
    CvMat *Vtt=NULL,*Utt=NULL;
    for (int kk = 1; kk <= randIni; kk++)
    {
        //random initialization
        if (option >= 3)
        {
            finalEstimation3d->residMean = 1e10;
            cvReleaseMat(&Ut);
            Ut = CreateRandMat(2*T, 4, .01, -0.005);
        }
        int count = 0;

        double estimation3DResidMeanOld=finalEstimation3d->residMean+10.0;
        while ( (cvNorm(Ut,UtOld,CV_L1)>tol) && (count < max_iter) && (fabs(finalEstimation3d->residMean-estimation3DResidMeanOld)>1e-2))
        {
            estimation3DResidMeanOld=finalEstimation3d->residMean;
            count++;

            for (int ii = 0; ii < M; ii++)
            {
                //probV->buc
                CvMat* Wfcol = cvCreateMat(cvGetSize(Wf).height, 1, CV_64FC1);
                CvMat* Utcol = cvCreateMat(cvGetSize(Ut).height, 1, CV_64FC1);
                CvMat* MfWfcol = cvCreateMat(cvGetSize(MfWf).height, 1, CV_64FC1);
                cvGetCol(Wf, Wfcol, ii);
                cvGetCol(Ut, Utcol, 3);
                cvGetCol(MfWf,MfWfcol,ii);

                CvMat *auxBuc1=cvCreateMat(Wfcol->rows,Wfcol->cols,CV_64FC1);
                CvMat *auxBuc2=cvCreateMat(Wfcol->rows,Wfcol->cols,CV_64FC1);

                cvMul(Wfcol, Utcol, auxBuc1);
                cvSub(auxBuc1,MfWfcol,auxBuc1);
                cvScale( auxBuc1, auxBuc2, -1.0);
                probV->buc = ConcatenateMatrixDown(auxBuc1,auxBuc2);
                cvReleaseMat(&Utcol);
                cvReleaseMat(&MfWfcol);
                cvReleaseMat(&auxBuc1);
                cvReleaseMat(&auxBuc2);
                //	probV.buc=[-MfWf(:,i)+Wf(:,i).*Ut(:,4); MfWf(:,i)-Wf(:,i).*Ut(:,4)];




                for (int jj=0;jj<3;jj++)
                {
                    for (int ss=probV->a->p[jj];ss<probV->a->p[jj+1];ss++)
                    {
                        int auxRow=probV->a->i[ss];
                        if (auxRow<2*T)
                            probV->a->x[ss]=-(cvGetReal2D(Wf,auxRow,ii)*cvGetReal2D(Ut,auxRow,jj));
                        else
                            probV->a->x[ss]=(cvGetReal2D(Wf,auxRow-2*T,ii)*cvGetReal2D(Ut,auxRow-2*T,jj));
                    }
                }
                cvReleaseMat(&Wfcol);
                //  probV.a(1:2*T,1:3)=-repmat(Wf(:,i),[1,3]).*Ut(:,1:3);
                //	probV.a(2*T+1:2*2*T,1:3)=-probV.a(1:2*T,1:3);


                //prob.x0 - generate feasible point
                probV->x0 = cvCreateMat(3+4*T, 1, CV_64FC1);
                for (int k = 0; k < 3; k++)
                {
                    cvSetReal2D(probV->x0, k, 0, 2*(rand()/(double)RAND_MAX)-1);
                }
                for (int k = 3; k < 3+2*T; k++)
                {
                    cvSetReal2D(probV->x0, k, 0, 100.0);
                }

                for (int k = 3+2*T; k < 3+4*T; k++)
                {
                    cvSetReal2D(probV->x0, k, 0, cvGetReal2D(probV->buc, k-2*T-3, 0));
                }

                //famatdebug
                /*
                ofstream outQ("probV_Q.txt");
                print(probV->Q,outQ);
                outQ.close();
                ofstream outc("probV_c.txt");
                print(probV->c,outc);
                outc.close();
                ofstream outa("probV_a.txt");
                print(probV->a,outa);
                outa.close();
                ofstream outbuc("probV_buc.txt");
                print(probV->buc,outbuc);
                cout<<"Finsh writing probV matrices.txt"<<endl;
                outbuc.close();
                */
                //-----------------------------
                STDQPdata* Vstd = std_qp(probV->Q, probV->c, probV->a, probV->buc, probV->x0, M,T, "probV");

                //--------------------------------------
                //ofstream outVx("probV_xhat.txt");
                //print(Vstd->answerMat,outVx);
                //cout<<"Finished writing probV matrices"<<endl;
                //outVx.close();
                //---------------------------------

                bool xxgreater1 = true;
                for (int k = 4; k < 3+2*T; k++)
                {
                    if (cvGetReal2D(Vstd->answerMat, k, 0) < -0.01)
                    {
                        xxgreater1 = false;
                        break;
                    }
                }


                if (xxgreater1 == true)
                {
                    for (int j = 0; j < 3; j++)
                    {
                        cvSetReal2D(V, ii, j, cvGetReal2D(Vstd->answerMat, j, 0));
                    }
                    // V(i,:)=xx(1:3);
                }
                else
                {
                    for (int j = 0; j < 3; j++)
                    {
                        cvSetReal2D(V, ii, j, cvGetReal2D(Vt, ii, j));
                    }
                    //    V(i,:)=Vt(i,:);
                }
                delete Vstd;
            }

            CvMat* Vtr = cvCreateMat(V->cols,V->rows,CV_64FC1);
            cvTranspose(V, Vtr);
            CvMat* VtrOnes = ConcatenateMatrixDown(Vtr, OnesMat(1, M));
            CvMat* UtVtrOnes = cvCreateMat(Ut->rows,VtrOnes->cols,CV_64FC1);
            cvMatMul(Ut, VtrOnes, UtVtrOnes);
            cvMul(Wf, UtVtrOnes, UtVtrOnes);
            cvSub(MfWf,UtVtrOnes,UtVtrOnes);
            CvMat* r2=cvCreateMat(UtVtrOnes->rows,UtVtrOnes->cols,CV_64FC1);
            cvMul(UtVtrOnes,UtVtrOnes, r2);
            cvReleaseMat(&UtVtrOnes);
            cvReleaseMat(&Vtr);
            cvReleaseMat(&VtrOnes);
            //r2=(Wf.*Mf-Wf.*(Ut*[V';ones(1,M)])).^2;

            CvMat* r2replace=cvCreateMat((r2->rows)/2,r2->cols,CV_64FC1);
            for (int i = 0; i < cvGetSize(r2).height; i+=2)
            {
                for (int j = 0; j < cvGetSize(r2).width; j++)
                {
                    cvSetReal2D(r2replace, (int)(i/2), j, sqrt(cvGetReal2D(r2,i,j)+cvGetReal2D(r2,i+1,j)));
                }
            }
            cvReleaseMat(&r2);

            //r2=sqrt(r2(1:2:end,:)+r2(2:2:end,:));

            double meanCounter = 0;
            int counter = 0;
            for (int i = 0; i < cvGetSize(r2replace).height; i++)
            {
                for (int j = 0; j < cvGetSize(r2replace).width; j++)
                {
                    if (cvGetReal2D(Wf, i+i, j) > minWeight*10)
                    {
                        meanCounter+=cvGetReal2D(r2replace,i,j);
                        counter++;
                    }
                }
            }
            cvReleaseMat(&r2replace);
            double aa = meanCounter/counter;
            //aa=mean(r2(find(r2~=0)));

            if (aa<=(finalEstimation3d->residMean+0.1))
            {
                cvReleaseMat(&Vt);
                Vt=cvCloneMat(V);
                finalEstimation3d->residMean=aa;
                if (debugMode)
                {
                    cout<<"iteration "<<count<<" of the first matrix factorization; MSE(with outliers)="<<aa<<endl;
                }
            }
            else
                break;

            CvMat* U = cvCreateMat(2*T, 4, CV_64FC1);    //????
            for (int jj=0; jj<2*T; jj++)
            {

                for (int ss=probU->c->p[0];ss<probU->c->p[1];ss++)
                {
                    int auxRow=probU->c->i[ss];
                    if (auxRow<3)
                        probU->c->x[ss]=-2.0*mu*cvGetReal2D(G,jj,auxRow);
                    else if (auxRow==3)
                        probU->c->x[ss]=-0.2*mu*cvGetReal2D(t,jj,0);
                }
                //probU.c(1:4)=[-2*mu*G(j,1:3)'; -2*mu*t(j)/10];

                //famatnote: you need to preallocate memory before calling cvGetRow
                CvMat* MfWfrow = cvCreateMat(1,cvGetSize(MfWf).width,CV_64FC1);
                //----------------------------------
                cvGetRow(MfWf,MfWfrow, jj);

                CvMat* MfWfrowT = cvCreateMat(cvGetSize(MfWfrow).width,cvGetSize(MfWfrow).height,CV_64FC1);
                CvMat* negMfWfrowT = cvCreateMat(cvGetSize(MfWfrow).width,cvGetSize(MfWfrow).height,CV_64FC1);;
                cvTranspose(MfWfrow, MfWfrowT);
                cvConvertScale(MfWfrowT, negMfWfrowT,-1,0);
                if (probU->buc!=NULL) cvReleaseMat(&(probU->buc));
                probU->buc = ConcatenateMatrixDown(negMfWfrowT, MfWfrowT);
                cvReleaseMat(&negMfWfrowT);
                cvReleaseMat(&MfWfrowT);
                cvReleaseMat(&MfWfrow);
                //probU.buc=[-MfWf(j,:)'; MfWf(j,:)'];

                for (int kk=0;kk<3;kk++)
                {
                    for (int ss=probU->a->p[kk];ss<probU->a->p[kk+1];ss++)
                    {
                        int auxRow=probU->a->i[ss];
                        if (auxRow<M)
                            probU->a->x[ss]=-(cvGetReal2D(Wf,jj,auxRow)*cvGetReal2D(Vt,auxRow,kk));
                        else
                            probU->a->x[ss]=(cvGetReal2D(Wf,jj,auxRow-M)*cvGetReal2D(Vt,auxRow-M,kk));
                    }
                }
                for (int ss=probU->a->p[3];ss<probU->a->p[4];ss++)
                {
                    int auxRow=probU->a->i[ss];
                    if (auxRow<M)
                        probU->a->x[ss]=-(cvGetReal2D(Wf,jj,auxRow));
                    else
                        probU->a->x[ss]=(cvGetReal2D(Wf,jj,auxRow-M));
                }

                //probU.a(1:M,1:4)=-repmat(Wf(j,:)',[1,4]).*[Vt ones(M,1)];
                //probU.a(M+1:2*M,1:4)=-probU.a(1:M,1:4);



                //%fabricate feasible point
                probU->x0 = cvCreateMat(4+2*M, 1, CV_64FC1);
                for (int i = 0; i < 3; i++)
                {
                    cvSetReal2D(probU->x0, i, 0, cvGetReal2D(Ut,jj,i)+(0.01*rand()/(double)RAND_MAX)-0.005);
                }
                cvSetReal2D(probU->x0, 3, 0, cvGetReal2D(Ut,jj,3)+(10.0*rand()/(double)RAND_MAX)-5.0);
                for (int i = 4; i < M+4; i++)
                {
                    cvSetReal2D(probU->x0, i, 0, (10.0*rand()/(double)RAND_MAX)+1e3);
                }
                for (int i = 4+M; i<4+2*M; i++)
                {
                    cvSetReal2D(probU->x0, i, 0, (2.0*rand()/(double)RAND_MAX)-1);
                }

                //famatdebug
                /*
                ofstream outQ("probU_Q.txt");
                print(probU->Q,outQ);
                outQ.close();
                ofstream outc("probU_c.txt");
                print(probU->c,outc);
                outc.close();
                ofstream outa("probU_a.txt");
                print(probU->a,outa);
                outa.close();
                ofstream outbuc("probU_buc.txt");
                print(probU->buc,outbuc);
                outa.close();
                ofstream outx0("probU_x0.txt");
                print(probU->x0,outx0);
                outa.close();
                ofstream outWf("probU_Wf.txt");
                print(Wf,outWf);
                cout<<"Finished writing probU matrices"<<endl;
                ofstream outVt("probU_Vt.txt");
                print(Vt,outVt);
                cout<<"Finished writing probU matrices"<<endl;
                outx0.close();
                */
                //-----------------------------

                STDQPdata* Ustd = std_qp(probU->Q,probU->c,probU->a,probU->buc,probU->x0,M,T,"probU");

                //--------------------------------------
                //ofstream outx("probU_xhat.txt");
                //print(Ustd->answerMat,outx);
                //cout<<"Finished writing probU matrices"<<endl;
                //outx.close();
                //---------------------------------

                if (Ustd->exit_flag==2)
                {
                    for (int i = 4; i < 4+M; i++)
                    {
                        cvSetReal2D(probU->x0, i, 0, cvGetReal2D(probU->x0, i, 0) + 3.0*(Ustd->gap));
                    }
                    delete Ustd;
                    Ustd = std_qp(probU->Q,probU->c,probU->a,probU->buc,probU->x0,M,T,"probU");
                }

                bool xxgreater1 = true;
                for (int k = 4; k < 4+M; k++)
                {
                    if (cvGetReal2D(Ustd->answerMat, k, 0) < -0.01)
                    {
                        xxgreater1 = false;
                        break;
                    }
                }
                if (xxgreater1 == true)
                {
                    for (int j = 0; j < 4; j++)
                    {
                        cvSetReal2D(U, jj, j, cvGetReal2D(Ustd->answerMat, j, 0));
                    }
                    // U(j,:)=xx(1:4);
                }
                else
                {
                    for (int j = 0; j < 4; j++)
                    {
                        cvSetReal2D(U, jj, j, cvGetReal2D(Ut, jj, j));
                    }
                    //   U(jj,:)=Ut(jj,:);
                }
                delete Ustd;
            }

            Vtr=cvCreateMat(V->cols,V->rows,CV_64FC1);
            cvTranspose(V, Vtr);
            VtrOnes = ConcatenateMatrixDown(Vtr, OnesMat(1, M));
            UtVtrOnes=cvCreateMat(U->rows,VtrOnes->cols,CV_64FC1);
            cvMatMul(U, VtrOnes, UtVtrOnes);
            cvMul(Wf, UtVtrOnes, UtVtrOnes);
            cvSub(MfWf,UtVtrOnes, UtVtrOnes);
            r2=cvCreateMat(UtVtrOnes->rows,UtVtrOnes->cols,CV_64FC1);
            cvMul(UtVtrOnes,UtVtrOnes, r2);
            cvReleaseMat(&Vtr);
            cvReleaseMat(&VtrOnes);
            cvReleaseMat(&UtVtrOnes);
            //r2=(Wf.*Mf-Wf.*(Ut*[V';ones(1,M)])).^2;


            r2replace=cvCreateMat((r2->rows)/2,r2->cols,CV_64FC1);
            for (int i = 0; i < cvGetSize(r2).height; i+=2)
            {
                for (int j = 0; j < cvGetSize(r2).width; j++)
                {
                    cvSetReal2D(r2replace, (int)(i/2), j, sqrt(cvGetReal2D(r2,i,j)+cvGetReal2D(r2,i+1,j)));
                }
            }
            cvReleaseMat(&r2);


            //r2=sqrt(r2(1:2:end,:)+r2(2:2:end,:));

            double meanCounter2 = 0;
            int counter2 = 0;
            for (int i = 0; i < cvGetSize(r2replace).height; i++)
            {
                for (int j = 0; j < cvGetSize(r2replace).width; j++)
                {
                    if (cvGetReal2D(Wf, i+i, j) > minWeight*10)
                    {
                        meanCounter2+=cvGetReal2D(r2replace,i,j);
                        counter2++;
                    }
                }
            }
            cvReleaseMat(&r2replace);
            aa = meanCounter2/counter2;
            //aa=mean(r2(find(r2~=0)));

            if (aa <= (finalEstimation3d->residMean+0.1))
            {
                cvReleaseMat(&UtOld);
                UtOld = cvCloneMat(Ut);
                cvReleaseMat(&Ut);
                Ut = cvCloneMat(U);
                cvReleaseMat(&U);
                finalEstimation3d->residMean = aa;
                if (debugMode)
                {
                    cout<<"iteration "<<count<<" of the second matrix factorization; MSE(with outliers)="<<aa<<endl;
                }
            }
            else
            {
                cvReleaseMat(&U);
                cvReleaseMat(&UtOld);
                //factorization finished
                break;
            }
        }
        if (finalEstimation3d->residMean<residMeanOld)
        {
            residMeanOld=finalEstimation3d->residMean;

            if (Utt!=NULL)
                cvReleaseMat(&Utt);
            Utt= cvCloneMat(Ut);
            if (Vtt!=NULL)
                cvReleaseMat(&Vtt);
            Vtt= cvCloneMat(Vt);
            //---------------------------------------------
            //          %to speed-up the process
            if (residMeanOld<1.0)
                break;
        }

    }

    finalEstimation3d->residMean=residMeanOld;
    delete probV;
    delete probU;
    cvReleaseMat(&UtOld);

    cout<<"Matrix factorization; MSE(with outliers)="<<finalEstimation3d->residMean<<endl;


    CvMat *VttT = cvCreateMat(Vtt->cols,Vtt->rows,CV_64FC1);
    cvTranspose(Vtt, VttT);
    CvMat *reproj=ConcatenateMatrixDown(VttT,OnesMat(1,M));
    cvReleaseMat(&VttT);
    CvMat *MfHat=cvCreateMat(Utt->rows,reproj->cols,CV_64FC1);
    cvMatMul(Utt,reproj,MfHat);
    cvReleaseMat(&reproj);

    if (sfm->reproj_x!=NULL)
        delete sfm->reproj_x;
    sfm->reproj_x = new contour(*(sfm_->contour_x));//just to allocate memory
    if (sfm->reproj_y!=NULL)
        delete sfm->reproj_y;
    sfm->reproj_y = new contour(*(sfm_->contour_y));

    int pos=0;
    for (int ii=0;ii<M;ii++)
        for (int jj=0;jj<T;jj++)
        {
            sfm->reproj_x->scores->data.db[pos]=cvGetReal2D(MfHat,jj+jj,ii)+W_2;
            sfm->reproj_y->scores->data.db[pos]=cvGetReal2D(MfHat,jj+jj+1,ii)+H_2;
            pos++;
        }

    cvReleaseMat(&Vtt);
    cvReleaseMat(&Utt);
    cvReleaseMat(&MfHat);
    cvReleaseMat(&MfWf);
    delete finalEstimation3d;
    delete sfm_;
    return sfm;
}

SFMdata* residAnalysis(SFMdata* sfm,bool debugMode)
{
    //value of the residual to find teh kink in the resid curve (in pixels)
    double kinkThr=3.0;
    //any value above maxResid is consider an Outlier automatically
    double maxResid=20.0;

    //useful variables
    int T=sfm->contour_x->scores->cols;
    int M=sfm->contour_x->scores->rows;

    //compute residuals
    CvMat *resid=cvCreateMat(M,T,CV_64FC1);
    CvMat *residAux=cvCreateMat(M,T,CV_64FC1);
    cvSub(sfm->contour_x->scores,sfm->reproj_x->scores,resid);
    cvMul(resid,resid,resid);
    cvSub(sfm->contour_y->scores,sfm->reproj_y->scores,residAux);
    cvMul(residAux,residAux,residAux);
    cvAdd(resid,residAux,resid);
    cvReleaseMat(&residAux);
    cvPow( resid, resid, 0.5);

    int totalNumOutliers=0;
    vector<double> auxR;
    auxR.reserve(T);
    vector<int> keepContour;//vector containing which contours need to be kept after detecting outliers
    //analyze each trajectory
    for (int ii=0;ii<M;ii++)
    {

        auxR.clear();
        for (int jj=0,pos=ii*T;jj<T;jj++,pos++)
        {
            //find positions where residual is too big and set them to negative
            //number so it won't affect the outlier detection in case there are too
            //many residuals

            if (sfm->contour_x->scores->data.db[pos]>1e-6 && resid->data.db[pos]<maxResid) auxR.push_back(resid->data.db[pos]);
        }
        /*
        pos=find(resid(i,:)>0);
        l=length(pos);
        auxR=sort(resid(i,pos));
        */

        //build linear programming to detect outliers
        double thr=1e20;
        int l=auxR.size();
        if (l>0)
        {
            sort(auxR.begin(),auxR.end());
            cs *_T;

            //create c
            _T=cs_spalloc(l+2,1,l,1,1);
            for (int kk=2;kk<l+2;kk++)
                cs_entry(_T,kk,0,1.0);
            cs *c=cs_compress(_T);
            cs_spfree(_T);

            //create A
            _T=cs_spalloc(l+l,l+2,6*l,1,1);
            for (int kk=0;kk<l;kk++)
            {
                cs_entry(_T,kk,0,-kk-1);
                cs_entry(_T,kk+l,0,kk+1);
                cs_entry(_T,kk,1,-1.0);
                cs_entry(_T,kk+l,1,1.0);
                cs_entry(_T,kk,kk+2,-1.0);
                cs_entry(_T,kk+l,kk+2,-1.0);
            }
            cs *A=cs_compress(_T);
            cs_spfree(_T);

            //create b
            CvMat *b=cvCreateMat(l+l,1,CV_64FC1);
            for (int kk=0;kk<l;kk++)
            {
                b->data.db[kk]=-auxR[kk];
                b->data.db[kk+l]=auxR[kk];
            }

            //create x0 as feasible point
            CvMat *xx=cvCreateMat(2+l,1,CV_64FC1);
            xx->data.db[0]=0.0;
            xx->data.db[1]=0.0;
            for (int kk=0;kk<l;kk++)
                xx->data.db[kk+2]=auxR[kk]+1.0;

            //solve robust linear regression using L1 norm to find outliers
            LPsolver(c, A, b, xx);

            //famatdebug
            //ofstream outXX("xHat_LP.txt");
            //print(xx,outXX);
            //outXX.close();
            //cout<<"Finished writing solution for LP"<<endl;
            //------------------

            //find threshold in residual
            for (int kk=0;kk<l;kk++)
            {
                if ((auxR[kk]-xx->data.db[0]*(kk+1)-xx->data.db[1])>kinkThr)
                    thr=min(thr,auxR[kk]);
            }

            //free memory
            cs_spfree(c);
            cs_spfree(A);
            cvReleaseMat(&b);
            cvReleaseMat(&xx);
        }
        //mark all the outliers in this trajectory
        int numOutliers=0;
        for (int jj=0,pos=ii*T;jj<T;jj++,pos++)
        {
            if (sfm->contour_x->scores->data.db[pos]>1e-6)
            {
                if (resid->data.db[pos]>maxResid || resid->data.db[pos]>=thr)
                {
                    //set contours to zero so they can be restimated using fillContours
                    sfm->contour_x->scores->data.db[pos]=0.0;
                    sfm->contour_y->scores->data.db[pos]=0.0;
                    numOutliers++;
                }
            }
        }
        if(debugMode==true)
            cout<<"Detected "<<numOutliers<<" outliers in trajectory "<<ii<<" with thr="<<thr<<endl;

        totalNumOutliers+=numOutliers;
        //if trajectory has 10% of outliers or more it should be eliminated
        if (((double)numOutliers)/((double)T)<0.1)
            keepContour.push_back(ii);
    }

    cout<<"There are "<<totalNumOutliers<<" outliers"<<endl;

    //redo sfm structure to delete trajectories with too many outliers
    if (((int)keepContour.size())!=M)
    {
        CvMat *c_x=cvCreateMat(keepContour.size(),T,CV_64FC1);
        CvMat *r_x=cvCreateMat(keepContour.size(),T,CV_64FC1);
        CvMat *c_y=cvCreateMat(keepContour.size(),T,CV_64FC1);
        CvMat *r_y=cvCreateMat(keepContour.size(),T,CV_64FC1);
        int *mType=new int[keepContour.size()];
        int pos=0;
        for (unsigned int ii=0;ii<keepContour.size();ii++)
        {
            int pos2=keepContour[ii]*T;
            for (int jj=0;jj<T;jj++)
            {
                c_x->data.db[pos]=sfm->contour_x->scores->data.db[pos2];
                r_x->data.db[pos]=sfm->reproj_x->scores->data.db[pos2];
                c_y->data.db[pos]=sfm->contour_y->scores->data.db[pos2];
                r_y->data.db[pos]=sfm->reproj_y->scores->data.db[pos2];
                pos2++;
                pos++;
            }
            mType[ii]=sfm->markerType[keepContour[ii]];
        }

        delete sfm;
        SFMdata *sfmReturn=new SFMdata(contour(c_x,1),contour(c_y,2),contour(r_x,1),contour(r_y,2),mType);
        cvReleaseMat(&c_x);
        cvReleaseMat(&r_x);
        cvReleaseMat(&c_y);
        cvReleaseMat(&r_y);
        delete []mType;
        return (sfmReturn);
    }
    else
    {
        SFMdata *sfmReturn=new SFMdata(*sfm);
        delete sfm;
        return sfmReturn;
    }
}

void decideTiltAlignOptions(SFMdata *sfm,int *tiltOption,int *rotOption,int *magOption)
{
    int M=sfm->contour_x->scores->rows;
    int T=sfm->contour_x->scores->cols;

    //decide if we have enough markers to adjust all parameters or we need to fix some parameters

    int minNumMarkers=10000;
    //we count number of markers per frame
    int pos,count;
    double *data=sfm->contour_x->scores->data.db;
    for (int jj=0;jj<T;jj++)
    {
        //count number of nonzeros per frame
        count=0;
        pos=jj;
        for (int ii=0;ii<M;ii++)
        {
            if (data[pos]>1e-6)
                count++;
            pos+=T;
        }
        minNumMarkers=min(minNumMarkers,count);
    }
    switch(minNumMarkers)
    {
        case 1://we fix everything except tilt angle
            *tiltOption=5;
            *rotOption=0;
            *magOption=0;
        break;

        case 2://we fix everything except tilt angle
            *tiltOption=5;
            *rotOption=0;
            *magOption=0;
        break;

        case 3://we fix everything except tilt angle
            *tiltOption=5;
            *rotOption=0;
            *magOption=0;
        break;

        case 4://we fix magnification option
            *tiltOption=5;
            *rotOption=3;
            *magOption=0;
        break;

        case 5://we fix magnification option
            *tiltOption=5;
            *rotOption=3;
            *magOption=0;
        break;

        default://everything is grouped
            *tiltOption=5;
            *rotOption=3;
            *magOption=3;
        break;
    }
}

SFMdata* decideAlphaOption(SFMdata *sfm,int *optionAlpha,vector<frame> *frames)//sfm is freed inside decideAlphaOption
{
    int M=sfm->contour_x->scores->rows;
    int T=sfm->contour_x->scores->cols;

    //---------------------------------------------
    //map between frames and contours
    list<int> discardColumn;
    int *map=new int[T];
    int count=0;
    for (unsigned int kk=0;kk<frames->size();kk++)
    {
        if (frames->at(kk).discard==false)
        {
            map[count]=kk;
            count++;
        }
    }
    //---------------------------------------------

    bool flagReturn=false;

    if (*optionAlpha==1 || *optionAlpha==2)
        flagReturn=true;

    //decide if we have enough markers to do just factorization without regularization
    //we also decide if we have enough markers to continue

    //last condition
    int pos;
    double *data=sfm->contour_x->scores->data.db;
    for (int jj=0;jj<T;jj++)
    {
        //count number of nonzeros per frame
        count=0;
        pos=jj;
        for (int ii=0;ii<M;ii++)
        {
            if (data[pos]>1e-6)
                count++;
            pos+=T;
        }
        //if (count<16 && flagReturn==false)
        if (count<100 && flagReturn==false)//we want to use optionAlpha=0 all the time (it is faster than factorization) UV
        {
            *optionAlpha=0;
            flagReturn=true;
        }
        //check if we have a minimum of contours to continue
        if (count<=2)//for each frame we need 8 parameters (G_j,t). However, we use regularization, so that reduces the demand
        {
            cout<<"WARNING: projection number "<<jj<<" (first projection is 0) contains only "<<count<<"detected markers"<<endl;
            //cout<<"It is not safe to continue; We have too few detected markers to fit parameters"<<endl;
            //cout<<"RAPTOR is stopping computation at "<<getDate()<<endl;
            //exit(-1);
            cout<<"RAPTOR is ignoring projections on this side until the end of the tilt series"<<endl;
            discardColumn.push_back(jj);
        }
    }
    //second condition
    for (int jj=0;jj<T-1;jj++)
    {
        //count number of nonzeros per pairs of frames
        count=0;
        pos=jj;
        for (int ii=0;ii<M;ii++)
        {
            if (data[pos]>1e-6 && data[pos+1]>1e-6)
                count++;
            pos+=T;
        }
        if (count<4 && flagReturn==false)
        {
            *optionAlpha=0;
            flagReturn=true;
        }
        //check if we have a minimum of contours to continue
        if (count<=1)
        {
            cout<<"WARNING: projections number "<<jj<<"->"<<jj+1<<" (first projection is 0) contains only "<<count<<"common detected markers"<<endl;
            //cout<<"It is not safe to continue; We have too few detected markers to fit parameters"<<endl;
            //cout<<"RAPTOR is stopping computation at "<<getDate()<<endl;
            //exit(-1);
            cout<<"RAPTOR is ignoring projections on this side until the end of the tilt series"<<endl;
            discardColumn.push_back(jj);
        }
    }

    //third condition
    if (sfm->contour_x->calculateNNZ()<3*(8*T+3*M) && flagReturn==false)
    {
        *optionAlpha=0;
        flagReturn=true;
    }

    //if the code made it here it means we can use just factorization without regularization
    if (*optionAlpha==4 && flagReturn==false)
        *optionAlpha=3;
    else if (flagReturn==false)
        *optionAlpha=4;


    //--------------------------
    //decide if we have to erase some columns
    SFMdata *newSfm;
    if (discardColumn.empty())
    {
        newSfm=new SFMdata(*sfm);
    }
    else
    {
        discardColumn.sort();
        discardColumn.unique();
        unsigned int ll=discardColumn.size();
        bool *mask=new bool[T];
        for (int kk=0;kk<T;kk++)
        {
            if (binary_search (discardColumn.begin(), discardColumn.end(), kk))
            {
                mask[kk]=false;//remove this element
            }
            else mask[kk]=true;
        }

        CvMat *c_x=cvCreateMat(M,T-ll,CV_64FC1);
        CvMat *r_x=cvCreateMat(M,T-ll,CV_64FC1);
        CvMat *c_y=cvCreateMat(M,T-ll,CV_64FC1);
        CvMat *r_y=cvCreateMat(M,T-ll,CV_64FC1);
        int pos=0,pos2=0;
        for (int ii=0;ii<M;ii++)
        {
            for (int jj=0;jj<T;jj++)
            {
                if (mask[jj])
                {
                    c_x->data.db[pos]=sfm->contour_x->scores->data.db[pos2];
                    r_x->data.db[pos]=sfm->reproj_x->scores->data.db[pos2];
                    c_y->data.db[pos]=sfm->contour_y->scores->data.db[pos2];
                    r_y->data.db[pos]=sfm->reproj_y->scores->data.db[pos2];
                    pos++;
                }
                pos2++;
            }
        }
        newSfm=new SFMdata(contour(c_x,1),contour(c_y,2),contour(r_x,1),contour(r_y,2),sfm->markerType);
        cvReleaseMat(&c_x);
        cvReleaseMat(&r_x);
        cvReleaseMat(&c_y);
        cvReleaseMat(&r_y);
        delete []mask;
        //update frames information
        for (list<int>::iterator iter=discardColumn.begin();iter!=discardColumn.end();iter++)
            frames->at(map[*iter]).discard=true;
    }
    //--------------------------

    delete sfm;
    delete []map;
    return newSfm;
}


void writeIMODfidModel(SFMdata* sfm, int width, int height, int num_frames, string basename,ostream &out,vector<frame> *frames)
{
    //create trajectory vector and then call the original function
    vector<trajectory> T=sfm->contour_x->contour2trajectory(*(sfm->contour_x),*(sfm->contour_y),frames);
    writeIMODfidModel(T, width, height, num_frames, basename,out);
    //free memory appropiately
    freeTrajectoryVector(T);
    T.clear();
}

void writeIMODfidModelReproj(SFMdata* sfm, int width, int height, int num_frames, string basename,ostream &out,vector<frame> *frames)
{
    //create trajectory vector and then call the original function
    vector<trajectory> T=sfm->contour_x->contour2trajectory(*(sfm->reproj_x),*(sfm->reproj_y),frames);
    writeIMODfidModel(T, width, height, num_frames, basename,out);
    //free memory appropiately
    freeTrajectoryVector(T);
    T.clear();
}







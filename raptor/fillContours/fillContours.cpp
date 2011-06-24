/*
 * fillContours.cpp - fills trajectories using reprojection model estimation 
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */


#include "fillContours.h"
#include "../template/template.h"
#include <string>
#include <algorithm>

void fillContours(SFMdata *sfm,ioMRC *vol,float **templ,int *templSize,bool debugMode)
{
    cout<<"Starting filling contours"<<endl;
    //important parameters
    //maximum number of images use to use as a template matching
    int memory=3;
    int count;
    int mType;//marker type for bead centering

    //some more parameters
    int M=sfm->contour_x->scores->rows;
    int T=sfm->contour_x->scores->cols;

    //-------------------------------------
    //we try to expand each trajectory
    //firs we left to right
    for (int ii=0;ii<M;ii++)
    {
        mType=sfm->markerType[ii];
        //copy templ to IplImage for beadCenter method
        IplImage *perfectMarkerTemplIpl=cvCreateImage(cvSize(templSize[mType], templSize[mType]), IPL_DEPTH_32F, 1);
        memcpy((float*)perfectMarkerTemplIpl->imageData,templ[mType],templSize[mType]*templSize[mType]*sizeof(float));
        for (int jj=memory,pos=(ii*T)+memory;jj<T;jj++,pos++)//we need at least "memory" previous neigbors to try to locate the gold bead
        {
            if (sfm->contour_x->scores->data.db[pos]>1e-6)
                continue;
            //we need at least two previous neigbors to try to locate the gold bead
            bool tryTracking=true;
            for (int kk=1;kk<=memory;kk++)
                if (sfm->contour_x->scores->data.db[pos-kk]<1e-6)
                {
                    tryTracking=false;
                    break;
                }
            if (!tryTracking)
                continue;
            //now apparently we have enough history to try to continue the track

            //create patch around around this contour
            IplImage *patchIpl=cvCreateImage(cvSize(2*templSize[mType], 2*templSize[mType]), IPL_DEPTH_32F, 1);
            tryTracking=vol->readMRCpatch(jj,(int)ceil(sfm->reproj_x->scores->data.db[pos]),(int)ceil(sfm->reproj_y->scores->data.db[pos]),templSize[mType],templSize[mType],(float*)patchIpl->imageData);
            if (!tryTracking)
            {
                cvReleaseImage(&patchIpl);
                continue;
            }

            //famatdebug
            //ofstream outPatch("patch_fc.txt");
            //print(patchIpl,outPatch);
            //outPatch.close();
            //cout<<"Finished writing patch"<<endl;
            //----------------
            //for each neighboring image find translation peak
            centroid* peak=new centroid[memory];
            IplImage *templIpl=cvCreateImage(cvSize(2*(templSize[mType]/2),2*(templSize[mType]/2)), IPL_DEPTH_32F, 1);
            for (int kk=1;kk<=memory;kk++)
            {
                tryTracking=vol->readMRCpatch(jj-kk,(int)ceil(sfm->contour_x->scores->data.db[pos-kk]),(int)ceil(sfm->contour_y->scores->data.db[pos-kk]),templSize[mType]/2,templSize[mType]/2,(float*)templIpl->imageData);
                if (!tryTracking)
                {
                    peak[kk-1].x=-1.0;//we can check variable used==false to see if we were here
                    continue;
                }
                findMatchingTranslation(patchIpl,templIpl,&(peak[kk-1].x),&(peak[kk-1].y),&(peak[kk-1].score),mType);
                if (peak[kk-1].score<peakThresholdFillContours)
                    peak[kk-1].x=-1.0;
                //famatdebug
                //ofstream outTempl("templ_fc.txt");
                //print(templIpl,outTempl);
                //outTempl.close();
                //cout<<"Finished writing templ"<<endl;
                //----------------
            }
            //see if they agree
            float xMean=0.0f,yMean=0.0f,scoreSum=0.0f;
            count=0;
            for (int kk=0;kk<memory;kk++)
            {
                if (peak[kk].x>=0)
                {
                    count++;
                    xMean+=peak[kk].x;
                    yMean+=peak[kk].y;
                    scoreSum+=peak[kk].score;
                }
            }
            if (count==0)//non o fteh peaks could be calculated
            {
                delete []peak;
                cvReleaseImage(&patchIpl);
                cvReleaseImage(&templIpl);
                continue;
            }
            xMean/=((float)count);
            yMean/=((float)count);

            //double check if NCC agrees
            float xx=0.0f,yy=0.0f;
            tryTracking=true;
            for (int kk=0;kk<memory;kk++)
            {
                if (peak[kk].x>=0)
                {
                    if (fabs(xMean-peak[kk].x)>3.0)
                    {
                        delete []peak;
                        cvReleaseImage(&patchIpl);
                        cvReleaseImage(&templIpl);
                        tryTracking=false;
                        break;
                    }
                    xx+=(peak[kk].score*peak[kk].x);
                    yy+=(peak[kk].score*peak[kk].y);
                }
            }
            if (!tryTracking) continue;
            xx/=scoreSum;//weighted average
            yy/=scoreSum;


            //peak search return (x,y) coordinates in array *result. We have to obtain the relative coordinate of displacement (delta_x,delta_y)
            xx-=0.5*(patchIpl->width-templIpl->width);//the center is 0.5*(result->width-1). The -1 is because array limits are from 0->width-1
            yy-=0.5*(patchIpl->height-templIpl->height);
            //improve centering of the bead
            beadCenter(patchIpl,perfectMarkerTemplIpl,&xx,&yy,mType);

            sfm->contour_x->scores->data.db[pos]=yy+ceil(sfm->reproj_x->scores->data.db[pos]);//delta_x and delta_y are switched
            sfm->contour_y->scores->data.db[pos]=xx+ceil(sfm->reproj_y->scores->data.db[pos]);

            if(debugMode==true)
                cout<<"Filled trajectory="<<ii<<" frame="<<jj<<" with scoreSum="<<scoreSum<<" and new (x,y)="<<
            sfm->contour_x->scores->data.db[pos]<<","<<sfm->contour_y->scores->data.db[pos]<<".Reproj_(x,y)="<<
            sfm->reproj_x->scores->data.db[pos]<<","<<sfm->reproj_y->scores->data.db[pos]<<endl;
            //release memory
            delete []peak;
            cvReleaseImage(&patchIpl);
            cvReleaseImage(&templIpl);
        }
        if(debugMode)
            cout<<"Finished checking trajectory="<<ii<<" for from left to right"<<endl;
        cvReleaseImage(&perfectMarkerTemplIpl);
    }

    //-------------------------------------
    //we try to expand each trajectory
    //SECOND FROM RIGHT TO LEFT
    for (int ii=0;ii<M;ii++)
    {
        mType=sfm->markerType[ii];
        //copy templ to IplImage for beadCenter method
        IplImage *perfectMarkerTemplIpl=cvCreateImage(cvSize(templSize[mType], templSize[mType]), IPL_DEPTH_32F, 1);
        memcpy((float*)perfectMarkerTemplIpl->imageData,templ[mType],templSize[mType]*templSize[mType]*sizeof(float));
        for (int jj=T-memory-1,pos=((ii+1)*T)-memory-1;jj>=0;jj--,pos--)//we need at least "memory" previous neigbors to try to locate the gold bead
        {
            if (sfm->contour_x->scores->data.db[pos]>1e-6)
                continue;
            //we need at least two previous neigbors to try to locate the gold bead
            bool tryTracking=true;
            for (int kk=1;kk<=memory;kk++)
                if (sfm->contour_x->scores->data.db[pos+kk]<1e-6)
                {
                    tryTracking=false;
                    break;
                }
            if (!tryTracking)
                continue;
            //now apparently we have enough history to try to continue the track

            //create patch around around this contour
            IplImage *patchIpl=cvCreateImage(cvSize(2*templSize[mType], 2*templSize[mType]), IPL_DEPTH_32F, 1);
            tryTracking=vol->readMRCpatch(jj,(int)ceil(sfm->reproj_x->scores->data.db[pos]),(int)ceil(sfm->reproj_y->scores->data.db[pos]),templSize[mType],templSize[mType],(float*)patchIpl->imageData);
            if (!tryTracking)
            {
                cvReleaseImage(&patchIpl);
                continue;
            }
            //famatdebug
            //ofstream outPatch("patch_fc.txt");
            //print(patchIpl,outPatch);
            //outPatch.close();
            //cout<<"Finished writing patch"<<endl;
            //----------------

            //for each neighboring image find translation peak
            centroid *peak=new centroid[memory];
            IplImage *templIpl=cvCreateImage(cvSize(2*(templSize[mType]/2),2*(templSize[mType]/2)), IPL_DEPTH_32F, 1);
            for (int kk=1;kk<=memory;kk++)
            {
                tryTracking=vol->readMRCpatch(jj+kk,(int)ceil(sfm->contour_x->scores->data.db[pos+kk]),(int)ceil(sfm->contour_y->scores->data.db[pos+kk]),templSize[mType]/2,templSize[mType]/2,(float*)templIpl->imageData);
                if (!tryTracking)
                {
                    peak[kk-1].x=-1.0;//we can check variable used==false to see if we were here
                    continue;
                }
                findMatchingTranslation(patchIpl,templIpl,&(peak[kk-1].x),&(peak[kk-1].y),&(peak[kk-1].score),mType);
                if (peak[kk-1].score<peakThresholdFillContours)
                    peak[kk-1].x=-1.0;
                //famatdebug
                //ofstream outTempl("templ_fc.txt");
                //print(templIpl,outTempl);
                //outTempl.close();
                //cout<<"Finished writing templ"<<endl;
                //----------------
            }
            //see if they agree
            float xMean=0.0f,yMean=0.0f,scoreSum=0.0f;
            count=0;
            for (int kk=0;kk<memory;kk++)
            {
                if (peak[kk].x>=0.0)
                {
                    count++;
                    xMean+=peak[kk].x;
                    yMean+=peak[kk].y;
                    scoreSum+=peak[kk].score;
                }
            }
            if (count==0)//non o fteh peaks could be calculated
            {
                delete []peak;
                cvReleaseImage(&patchIpl);
                cvReleaseImage(&templIpl);
                continue;
            }
            xMean/=((float)count);
            yMean/=((float)count);

            //double check if NCC agrees
            float xx=0.0f,yy=0.0f;
            tryTracking=true;
            for (int kk=0;kk<memory;kk++)
            {
                if (peak[kk].x>=0.0)
                {
                    if (fabs(xMean-peak[kk].x)>3.0)
                    {
                        delete []peak;
                        cvReleaseImage(&patchIpl);
                        cvReleaseImage(&templIpl);
                        tryTracking=false;
                        break;
                    }
                    xx+=(peak[kk].score*peak[kk].x);
                    yy+=(peak[kk].score*peak[kk].y);
                }
            }
            if (!tryTracking) continue;
            xx/=scoreSum;//weighted average
            yy/=scoreSum;

            //peak search return (x,y) coordinates in array *result. We have to obtain the relative coordinate of displacement (delta_x,delta_y)
            xx-=0.5*(patchIpl->width-templIpl->width);//the center is 0.5*(result->width-1). The -1 is because array limits are from 0->width-1
            yy-=0.5*(patchIpl->height-templIpl->height);
            //improve centering of the bead
            beadCenter(patchIpl,perfectMarkerTemplIpl,&xx,&yy,mType);

            sfm->contour_x->scores->data.db[pos]=yy+ceil(sfm->reproj_x->scores->data.db[pos]);
            sfm->contour_y->scores->data.db[pos]=xx+ceil(sfm->reproj_y->scores->data.db[pos]);
            if(debugMode==true)
                cout<<"Filled trajectory="<<ii<<" frame="<<jj<<" with scoreSum="<<scoreSum<<" and new (x,y)="<<

            sfm->contour_x->scores->data.db[pos]<<","<<sfm->contour_y->scores->data.db[pos]<<".Reproj_(x,y)="<<
            sfm->reproj_x->scores->data.db[pos]<<","<<sfm->reproj_y->scores->data.db[pos]<<endl;
            //release memory
            delete []peak;
            cvReleaseImage(&patchIpl);
            cvReleaseImage(&templIpl);
        }
        if(debugMode)
            cout<<"Finished checking trajectory="<<ii<<" for from right to left"<<endl;
        cvReleaseImage(&perfectMarkerTemplIpl);
    }

    cout<<"Finished filling contours"<<endl;

}


//given a large patch and a smaller patch, finds the peak of teh NCC value to compute translation between patches
void findMatchingTranslation(IplImage *imageIpl,IplImage *templIpl,double *x,double *y,double *score,int mType)
{
    int lP_width=imageIpl->width;
    int lP_height=imageIpl->height;
    int sP_width=templIpl->width;
    //int sP_height=templIpl->height;
    int result_width = lP_width - sP_width + 1;
    int result_height = lP_height - sP_width + 1;

    IplImage* resultIpl = cvCreateImage(cvSize(result_width, result_height), IPL_DEPTH_32F, 1);


    cvMatchTemplate(imageIpl, templIpl, resultIpl, CV_TM_CCOEFF_NORMED);


    //famattodo: crop borders. Is it necessary using NCC from OpenCV? I don't think so.
    vector<Point2D*> peaks = findPeaks((float*)resultIpl->imageData, result_width, result_height, peakThresholdFillContours, 1, 0, sP_width, 0,mType);


    //famatdebug
    //ofstream outfv("result_fc.txt");
    //print(resultIpl,outfv);
    //outfv.close();
    //cout<<"Finished writing translation matching with (x,y)="<<*peaks[0]<<endl;
    //cout<<"peaks size="<<peaks.size()<<endl;
    //--------------------------

    cvReleaseImage(&resultIpl);

    if (!peaks.empty())
    {
        *x=peaks[0]->x;
        *y=peaks[0]->y;
        *score=peaks[0]->score;
    }
    else
    {
        *x=-1.0;
        *y=-1.0;
        *score=-1.0;
    }


    //deallocate memory for peaks
    for (unsigned int kk=0;kk<peaks.size();kk++)
        delete peaks[kk];
    peaks.clear();

}


void print(IplImage *im,ostream &out)
{
    int ww=(im->width);
    float *ptr=(float*)im->imageData;
    for (int ii=0;ii<(im->height)*ww;ii++)
    {
        out<<ptr[ii]<<" ";
        if (((ii+1)%ww)==0)
            out<<endl;
    }

}

SFMdata* joinSimilarContours(SFMdata *sfm)//sfm gets deleted inisde this method.
{
    //useful variables
    int T=sfm->contour_x->scores->cols;
    int M=sfm->contour_x->scores->rows;
    double minDistContour=3.0;



    vector<int> eraseContour;//vector containing which contours need to be erased because we have merged them
    vector<int>::iterator itFind;
    double dist;
    for (int ii=0;ii<M-1;ii++)
    {
        itFind=find(eraseContour.begin(), eraseContour.end(), ii);
        if (itFind!=eraseContour.end())//we don't need to check this contour because it was merged already
            continue;
        double *x1=&(sfm->contour_x->scores->data.db[ii*T]);
        double *y1=&(sfm->contour_y->scores->data.db[ii*T]);
        for (int jj=ii+1;jj<M;jj++)
        {
            itFind=find(eraseContour.begin(), eraseContour.end(), jj);
            if (itFind!=eraseContour.end())//we don't need to check this contour because it was merged already
                continue;

            dist=distanceTrajectory(x1,y1,&(sfm->contour_x->scores->data.db[jj*T]),&(sfm->contour_y->scores->data.db[jj*T]),T);
            if (dist<minDistContour)//contoues need to be merged
            {
                double *x2=&(sfm->contour_x->scores->data.db[jj*T]);
                double *y2=&(sfm->contour_y->scores->data.db[jj*T]);

                for (int kk=0;kk<T;kk++)
                {
                    if (x1[kk]<1e-6)
                    {
                        if (x2[kk]>1e-6)
                        {
                            x1[kk]=x2[kk];
                            y1[kk]=y2[kk];
                            x2[kk]=0.0;
                            y2[kk]=0.0;
                        }
                    }
                    else if (x2[kk]>1e-6)
                    {
                        x2[kk]=0.0;
                        y2[kk]=0.0;
                    }
                }
                eraseContour.push_back(jj);
            }
        }
    }


    //redo sfm structure to delete trajectories that were merged
    if (!eraseContour.empty())
    {
        CvMat *c_x=cvCreateMat(M-eraseContour.size(),T,CV_64FC1);
        CvMat *r_x=cvCreateMat(M-eraseContour.size(),T,CV_64FC1);
        CvMat *c_y=cvCreateMat(M-eraseContour.size(),T,CV_64FC1);
        CvMat *r_y=cvCreateMat(M-eraseContour.size(),T,CV_64FC1);
        int *mType=new int[M-eraseContour.size()];

        int count=0;
        for (int ii=0;ii<M;ii++)
        {
            itFind=find(eraseContour.begin(), eraseContour.end(), ii);
            if (itFind!=eraseContour.end())//we don't need to copy this contour
                continue;
            for (int jj=0,pos=count*T,pos2=ii*T;jj<T;jj++,pos++,pos2++)
            {
                c_x->data.db[pos]=sfm->contour_x->scores->data.db[pos2];
                r_x->data.db[pos]=sfm->reproj_x->scores->data.db[pos2];
                c_y->data.db[pos]=sfm->contour_y->scores->data.db[pos2];
                r_y->data.db[pos]=sfm->reproj_y->scores->data.db[pos2];
            }
            mType[count]=sfm->markerType[ii];
            count++;
        }

        if (count!=(int)(M-eraseContour.size()))
        {
            cout<<"ERROR: joinSimilarContours. Count="<<count<<" does not agree with number of contours to keep "<<M-eraseContour.size()<<endl;
            exit(-1);
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


double distanceTrajectory(double *x1,double *y1,double *x2,double *y2,int length)
{
    vector<double> dist;

    for (int kk=0;kk<length;kk++)
    {
        if (x1[kk]>1e-6 && x2[kk]>1e-6)
            dist.push_back(fabs(x1[kk]-x2[kk])+fabs(y1[kk]-y2[kk]));
    }

    if (dist.empty())
        return 1e20;
    else //compute median
    {
        sort(dist.begin(),dist.end());
        return(*(dist.begin()+dist.size()/2));
    }
}

void beadCenter(IplImage *patchIpl,IplImage *perfectMarkerTemplIpl,float *xx,float *yy,int mType)
{
    int result_width = patchIpl->width - perfectMarkerTemplIpl->width + 1;
    int result_height = patchIpl->height - perfectMarkerTemplIpl->height + 1;

    IplImage *resultIpl = cvCreateImage(cvSize(result_width, result_height), IPL_DEPTH_32F, 1);
    cvMatchTemplate(patchIpl, perfectMarkerTemplIpl, resultIpl, CV_TM_CCOEFF_NORMED);

    //famatdebug
                //ofstream outTempl("resultBeadCenter_fc.txt");
                //print(resultIpl,outTempl);
                //outTempl.close();
                //cout<<"Finished writing result bead center"<<endl;
                //----------------

    //we find the closest peak to (xx,yy)
    //note:highest peak is not always the best option (for example, when two markers are together)

    //famattodo: crop borders. Is it necessary using NCC from OpenCV? I don't think so.
    int maxNumPeaks=5;
    vector<Point2D*> peaks = findPeaks((float*)resultIpl->imageData, result_width, result_height, peakThresholdFillContours, maxNumPeaks, 0, perfectMarkerTemplIpl->width, 0,mType);

    //find the closest peak
    //at the most we allow a move as largar as ther adius of a marker
    double minDist=0.25*(perfectMarkerTemplIpl->width-1);
    double dist=1e11;
    //the center is 0.5*(result->width-1). The -1 is because array limits are from 0->width-1
    float cx=0.5f*(result_width-1),cy=0.5f*(result_height-1);//findPeaks returns (x,y) coodinates in array result. We need (delta_x,delta_y)
    for(unsigned int kk=0;kk<peaks.size();kk++)
    {
		dist=min(dist,sqrt((double)(*xx-(peaks[kk]->x-cx))*(*xx-(peaks[kk]->x-cx))+(*yy-(peaks[kk]->y-cy))*(*yy-(peaks[kk]->y-cy))));
        if(dist<minDist)
        {
            (*xx)=peaks[kk]->x-cx;
            (*yy)=peaks[kk]->y-cy;
        }
        delete peaks[kk];//deallocate memory for peaks
    }

    //deallocate memory
    peaks.clear();
    cvReleaseImage(&resultIpl);
}


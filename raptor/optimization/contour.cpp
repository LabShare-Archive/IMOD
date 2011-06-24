/*
 * contour.cpp - class to save trajectories in X or Y coordinates
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include "contour.h"

using namespace std;

const double contour::minContourValue=1e-6;

contour::contour()
{
    scores=NULL;
}
contour::contour(int rows,int cols,int _xory)
{
    xory=_xory;
    numTraj=rows;
    numFrame=cols;
    scores = cvCreateMat(numTraj, numFrame, CV_64FC1);
    cvSetZero(scores);
}

contour::contour(CvMat* cc,int _xory)
{
    xory=_xory;
    numTraj=cc->rows;
    numFrame=cc->cols;
    scores=cvCloneMat(cc);
}

contour::contour(vector<trajectory> traj, int coordinate, int numFrames)
{
    xory = coordinate;
    numTraj = traj.size();
    numFrame = numFrames;
    //cout << "num traj: " << numTraj << " numFrames: "<< numFrames;
    scores = cvCreateMat(numTraj, numFrame, CV_64FC1);
    cvSetZero(scores);
    for (unsigned int i = 0; i < traj.size(); i++)   //vector<trajectory>
    {
        for ( unsigned int j = 0; j < traj[i].p.size(); j++)  //vector<Point2D*>
        {
            if (coordinate == 1)
            {
                cvSetReal2D(scores, i, ((*((traj[i]).p)[j]).frameID), (*((traj[i]).p)[j]).x);
            }
            else if (coordinate == 2)
                cvSetReal2D(scores, i, ((*((traj[i]).p)[j]).frameID), (*((traj[i]).p)[j]).y);

        }
    }
}

contour::contour(vector<trajectory> traj, int coordinate, int numFrames,vector<frame> *frames)
{

    //create a map between frameId and real position in the contour
    int *pos=new int[numFrames];
    int count=0;
    for (int ii=0;ii<numFrames;ii++)
    {
        if (frames->at(ii).discard==true)
            pos[ii]=-1;
        else
        {
            pos[ii]=count;
            count++;
        }
    }
    //---------------------------------------------------------------

    xory = coordinate;
    numTraj = traj.size();
    numFrame = count;
    //cout << "num traj: " << numTraj << " numFrames: "<< numFrames;
    scores = cvCreateMat(numTraj, numFrame, CV_64FC1);
    cvSetZero(scores);
    int posAux;
    for (unsigned int i = 0; i < traj.size(); i++)   //vector<trajectory>
    {
        for ( unsigned int j = 0; j < traj[i].p.size(); j++)  //vector<Point2D*>
        {
            posAux=((*((traj[i]).p)[j]).frameID);
            if (pos[posAux]>=0)
            {
                if (coordinate == 1)
                {
                    cvSetReal2D(scores, i, pos[posAux], (*((traj[i]).p)[j]).x);
                }
                else if (coordinate == 2)
                    cvSetReal2D(scores, i, pos[posAux], (*((traj[i]).p)[j]).y);
            }
        }
    }
    delete []pos;
}


contour::~contour()
{
    if (scores!=NULL)
    {
        cvReleaseMat(&scores);
        scores=NULL;
    }
}

contour::contour(const contour& orig)
{
    scores = cvCloneMat(orig.scores);
    xory = orig.xory;
    numTraj = orig.numTraj;
    numFrame = orig.numFrame;
}

contour  contour::operator=(const contour other)
{
    if (this!=&other)
    {
        if (scores!=NULL)
            cvReleaseMat(&scores);
        if (other.scores!=NULL)
            scores = cvCloneMat(other.scores);
        else scores=NULL;
        xory = other.xory;
        numTraj = other.numTraj;
        numFrame = other.numFrame;

    }
    return *this;
}

int contour::getNumTraj()
{
    return numTraj;
}

int contour::getNumFrame()
{
    return numFrame;
}

void contour::print(ostream &out)
{
    for (int i = 0; i < cvGetSize(scores).height; i++)
    {
        for (int j = 0; j < cvGetSize(scores).width; j++)
        {
            out << cvGetReal2D(scores,i,j) << "  ";
        }
        out << endl;
    }
    out << endl;
}


vector<trajectory> contour::contour2trajectory(contour c_x,contour c_y,vector<frame> *frames)
{
    //create a map from contours to frameID
    int *map=new int[c_x.scores->cols];
    int count=0;
    for(unsigned int kk=0;kk<frames->size();kk++)
    {
        if(frames->at(kk).discard==false)
        {
            map[count]=kk;
            count++;
        }
    }

    if(count!=c_x.scores->cols)
    {
        cout<<"ERROR: at contour2trajectory;Number of cols="<<c_x.scores->cols<<";Number of valid frames="<<count<<".They should be the same"<<endl;
        exit(-1);
    }

    vector<trajectory> T;
    for (int i = 0; i < c_x.scores->rows; i++)
    {
        trajectory* newTraj = new trajectory;
        for (int j = 0,pos=i*(c_x.scores->cols); j < c_x.scores->cols; j++,pos++)
        {
            if (c_x.scores->data.db[pos]>1e-6)
            {
                Point2D* newPoint = new Point2D;
                newPoint->x = c_x.scores->data.db[pos];
                newPoint->y = c_y.scores->data.db[pos];
                newPoint->frameID = map[j];
                (newTraj->p).push_back(newPoint);
            }
        }
        T.push_back(*newTraj);
        delete newTraj;
    }

    delete []map;
    return T;
}


/*
 * SFMdata.cpp - data structure to save structure from motion (SFM) information
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include "SFMdata.h"
#include "contour.h"

using namespace std;

SFMdata::SFMdata()
{
    contour_x = NULL;
    contour_y = NULL;
    reproj_x=NULL;
    reproj_y=NULL;
    markerType=NULL;
}
SFMdata::SFMdata(contour c_x,contour c_y,int *mType)
{
    contour_x = new contour(c_x);//famatnote: you need an equal operator for contour class since it contains a pointer
    contour_y = new contour(c_y);
    reproj_x=new contour(c_x.numTraj,c_x.numFrame,1);//initialize to zero
    reproj_y=new contour(c_y.numTraj,c_y.numFrame,2);//initialize to zero
    markerType=new int[c_x.numTraj];
    memcpy(markerType,mType,sizeof(int)*(c_x.numTraj));
}

SFMdata::SFMdata(contour c_x,contour c_y,contour r_x,contour r_y,int *mType)
{
    contour_x = new contour(c_x);//famatnote: you need an equal operator for contour class since it contains a pointer
    contour_y = new contour(c_y);
    reproj_x = new contour(r_x);
    reproj_y = new contour(r_y);
    markerType=new int[c_x.numTraj];
    memcpy(markerType,mType,sizeof(int)*(c_x.numTraj));
}

SFMdata::~SFMdata()
{
    clearPointers();

}

SFMdata  SFMdata::operator=(const SFMdata other)
{
    if (this!=&other)
    {
        clearPointers();
        contour_x=new contour(*(other.contour_x));
        reproj_x=new contour(*(other.reproj_x));

        contour_y=new contour(*(other.contour_y));
        reproj_y=new contour(*(other.reproj_y));
        if (other.markerType==NULL)
            markerType=NULL;
        else
        {
            markerType=new int[contour_x->numTraj];
            memcpy(markerType,other.markerType,sizeof(int)*(contour_x->numTraj));
        }
    }
    return *this;
}

SFMdata::SFMdata(const SFMdata& p)
{

    if (p.contour_x==NULL)
        contour_x=NULL;
    else
        contour_x=new contour((*p.contour_x));

    if (p.contour_y==NULL)
        contour_y=NULL;
    else
        contour_y=new contour((*p.contour_y));

    if (p.reproj_x==NULL)
        reproj_x=NULL;
    else
        reproj_x=new contour((*p.reproj_x));

    if (p.reproj_y==NULL)
        reproj_y=NULL;
    else
        reproj_y=new contour((*p.reproj_y));

    if (p.markerType==NULL)
        markerType=NULL;
    else
    {
        markerType=new int[contour_x->numTraj];
        memcpy(markerType,p.markerType,sizeof(int)*(contour_x->numTraj));
    }


}


void SFMdata::clearPointers()
{
    if (contour_x!=NULL)
    {
        delete contour_x;
        contour_x=NULL;
    }
    if (contour_y!=NULL)
    {
        delete contour_y;
        contour_y=NULL;
    }
    if (reproj_x!=NULL)
    {
        delete reproj_x;
        reproj_x=NULL;
    }
    if (reproj_y!=NULL)
    {
        delete reproj_y;
        reproj_y=NULL;
    }
    if(markerType!=NULL)
    {
        delete []markerType;
        markerType=NULL;
    }
}

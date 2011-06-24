/*
 * probData.cpp - data structure to save QP optimization data
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include "probData.h"
#include "contour.h"

using namespace std;

probData::probData()
{
    buc=NULL;
    a=NULL;
    c=NULL;
    Q=NULL;
    x0=NULL;
}

probData::probData(const probData& pD)
{
    if(pD.buc!=NULL)
        buc = cvCloneMat(pD.buc);//we don't need to allocate memory first. CloneMat does it for us
    else
        buc=NULL;
    if(pD.a!=NULL)
        a=CopyCS(pD.a);
    else a=NULL;

    if(pD.c!=NULL)
        c=CopyCS(pD.c);
    else c=NULL;
    if(pD.Q!=NULL)
        Q=CopyCS(pD.Q);
    else Q=NULL;

    if(pD.x0!=NULL)
        x0 = cvCloneMat(pD.x0);//we don't need to allocate memory first. CloneMat does it for us
    else x0=NULL;
}

probData::~probData()
{
  if(buc!=NULL)
  {
    cvReleaseMat(&buc);
    buc=NULL;
  }
  if(a!=NULL)
  {
    cs_spfree(a);
    a=NULL;
  }
  if(c!=NULL)
  {
    cs_spfree(c);
    c=NULL;
  }
  if(Q!=NULL)
  {
    cs_spfree(Q);
    Q=NULL;
  }

  if(x0!=NULL)
  {
    cvReleaseMat(&x0);
    x0=NULL;
  }
}

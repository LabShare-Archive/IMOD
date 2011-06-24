/*
 * estimation3ddata.cpp - data structure to save projection model
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include <iostream>
#include "estimation3ddata.h"
#include "std_qp.h"


using namespace std;

estimation3ddata::estimation3ddata()
{
    G = NULL;
    t = NULL;
    P = NULL;
}

estimation3ddata::estimation3ddata(int T, int M)
{
    G = ZeroMat(2*T, 3);
    t = ZeroMat(2*T, 1);
    P = ZeroMat(3, M);
}

estimation3ddata::~estimation3ddata()
{
    if(G != NULL)
    {
        cvReleaseMat(&G);
        G=NULL;
    }
    if(P!=NULL)
    {
        cvReleaseMat(&P);
        P=NULL;
    }
    if(t!=NULL)
    {
        cvReleaseMat(&t);
        t=NULL;
    }
}

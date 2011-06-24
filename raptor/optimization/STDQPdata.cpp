/*
 * STDQPdata.cpp - data structure to hold QP information
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */

#include "STDQPdata.h"


using namespace std;

STDQPdata::STDQPdata()
{
    answerMat=NULL;
}

STDQPdata::~STDQPdata()
{
    clear();
}

void STDQPdata::clear()
{
    if(answerMat!=NULL)//famatnote: we need this. Otherwise it gives segmentation fault if we try to release a pointer that has not been initialize
        cvReleaseMat(&answerMat);
}

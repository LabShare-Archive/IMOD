/*   imodview.h  -  header file for public imodview functions
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.2  2004/01/05 17:55:45  mast
Changes for binning

Revision 1.1  2003/10/01 05:01:01  mast
Initial creation; declarations pulled from imod.h

*/
#ifndef IMODVIEW_H
#define IMODVIEW_H

#ifndef IMODVIEWP_H
#include "imodviewP.h"
#endif

/* Define macro for export of functions under Windows */
#ifndef DLL_EX_IM
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllexport)
#else
#define DLL_EX_IM
#endif
#endif

extern "C" {

/*
 *  Send a message to all imod parts that an update is needed.
 */
int DLL_EX_IM ivwDraw(ImodView *inImodView, int inFlags);

/*
 *  The general draw update function. 
 */
int DLL_EX_IM ivwRedraw(ImodView *vw);

/* Get pixel values of the greyscale color ramp. */
void DLL_EX_IM ivwGetRamp(ImodView *inImodView, int *outRampBase, 
                          int *outRampSize);
int DLL_EX_IM  ivwGetObjectColor(ImodView *inImodView, int inObject);

void DLL_EX_IM ivwGetImageSize(ImodView *inImodView, int *outX, int *outY, 
                               int *outZ);
int DLL_EX_IM  ivwGetImageType(ImodView *view, unsigned int *otype, 
                               unsigned int *oformat);
void DLL_EX_IM ivwGetLocation(ImodView *inImodView, int *outX, int *outY,
                              int *outZ);
void DLL_EX_IM ivwGetLocationPoint(ImodView *inImodView, Ipoint *outPoint);
void DLL_EX_IM ivwSetLocation(ImodView *inImodView, int inX, int inY, int inZ);
void DLL_EX_IM ivwSetLocationPoint(ImodView *inImodView, Ipoint *inPoint);


/*
 * 4D data functions     
 */
/* returns max time index, the current time is retured via outTime. */
int DLL_EX_IM   ivwGetTime(ImodView *inImodView, int *outTime);
/* set the current time to inTime */
void DLL_EX_IM  ivwSetTime(ImodView *inImodView, int inTime);
/* get max time */
int DLL_EX_IM   ivwGetMaxTime(ImodView *inImodView);
/* get label for time. */
char DLL_EX_IM *ivwGetTimeLabel(ImodView *inImodView);
char DLL_EX_IM *ivwGetTimeIndexLabel(ImodView *inImodView, int inIndex);

/* 
 * Returns line pointers to raw grey scale image data for given z section. 
 */
unsigned DLL_EX_IM char **ivwGetZSection(ImodView *inImodView, int inSection);
unsigned DLL_EX_IM char **ivwGetCurrentZSection(ImodView *inImodView);
unsigned DLL_EX_IM char **ivwGetZSectionTime(ImodView *iv, int section, 
                                             int time);

/* Returns grey scale value for given image coordinate. */
int DLL_EX_IM ivwGetValue(ImodView *inImodView, int inX, int inY, int inZ);

/* Return value from image file. */
float DLL_EX_IM ivwGetFileValue(ImodView *inImodView, int inX, int inY, 
                                int inZ);


/**************************** Model data functions. ***************************
 *
 * See the libimod library functions for using the model structure.
 *
 */

/*
 *  Get the model associated with the view.
 */
Imod DLL_EX_IM *ivwGetModel(ImodView *inImodView);

/*
 *  Get the extra object
 */
Iobj DLL_EX_IM *ivwGetExtraObject(ImodView *inImodView);
}
#endif

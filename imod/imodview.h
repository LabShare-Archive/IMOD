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
*/
#ifndef IMODVIEW_H
#define IMODVIEW_H

#ifndef IMODVIEWP_H
#include "imodviewP.h"
#endif

extern "C" {

/*
 *  Send a message to all imod parts that an update is needed.
 */
int  ivwDraw(ImodView *inImodView, int inFlags);

/*
 *  The general draw update function. 
 */
int  ivwRedraw(ImodView *vw);

/* Get pixel values of the greyscale color ramp. */
void ivwGetRamp(ImodView *inImodView, int *outRampBase, int *outRampSize);
int  ivwGetObjectColor(ImodView *inImodView, int inObject);

void ivwGetImageSize(ImodView *inImodView, int *outX, int *outY, int *outZ);
int  ivwGetImageType(ImodView *view, unsigned int *otype, 
		     unsigned int *oformat);
void ivwGetLocation(ImodView *inImodView, int *outX, int *outY, int *outZ);
void ivwGetLocationPoint(ImodView *inImodView, Ipoint *outPoint);
void ivwSetLocation(ImodView *inImodView, int inX, int inY, int inZ);
void ivwSetLocationPoint(ImodView *inImodView, Ipoint *inPoint);


/*
 * 4D data functions     
 */
/* returns max time index, the current time is retured via outTime. */
int   ivwGetTime(ImodView *inImodView, int *outTime);
/* set the current time to inTime */
void  ivwSetTime(ImodView *inImodView, int inTime);
/* get max time */
int   ivwGetMaxTime(ImodView *inImodView);
/* get label for time. */
char *ivwGetTimeLabel(ImodView *inImodView);
char *ivwGetTimeIndexLabel(ImodView *inImodView, int inIndex);

/* 
 * Returns line pointers to raw grey scale image data for given z section. 
 */
unsigned char **ivwGetZSection(ImodView *inImodView, int inSection);
unsigned char **ivwGetCurrentZSection(ImodView *inImodView);
unsigned char **ivwGetZSectionTime(ImodView *iv, int section, int time);
     
/* Returns grey scale value for given image coordinate. */
int ivwGetValue(ImodView *inImodView, int inX, int inY, int inZ);

/* Return value from image file. */
float ivwGetFileValue(ImodView *inImodView, int inX, int inY, int inZ);


/**************************** Model data functions. ***************************
 *
 * See the libimod library functions for using the model structure.
 *
 */

/*
 *  Get the model associated with the view.
 */
Imod *ivwGetModel(ImodView *inImodView);

/*
 *  Get the extra object
 */
Iobj *ivwGetExtraObject(ImodView *inImodView);
}
#endif

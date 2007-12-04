/*   imodview.h  -  header file for public imodview functions
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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

/*!
 * Sends a message to all 3dmod image windows that a redraw is needed.
 * [inFlags] should contain an OR of the various IMOD_DRAW_ flags.
 */
int DLL_EX_IM ivwDraw(ImodView *inImodView, int inFlags);

/*!
 * The general draw update function.  If a current model point is defined,
 * calls for a draw with IMOD_DRAW_ALL; otherwise calls with IMOD_DRAW_MOD.
 */
int DLL_EX_IM ivwRedraw(ImodView *vw);

/* Gets pixel values of the greyscale color ramp. For index mode. */
void DLL_EX_IM ivwGetRamp(ImodView *inImodView, int *outRampBase, 
                          int *outRampSize);
/* Gets pixel value corresponding to an object color in index mode. */
int DLL_EX_IM  ivwGetObjectColor(ImodView *inImodView, int inObject);

/*! Gets the dimensions of the loaded image into [outX], [outY], [outZ] */
void DLL_EX_IM ivwGetImageSize(ImodView *inImodView, int *outX, int *outY, 
                               int *outZ);

/*! Gets the current marker point location into [outX], [outY], [outZ] */
void DLL_EX_IM ivwGetLocation(ImodView *inImodView, int *outX, int *outY,
                              int *outZ);

/*! Gets the current marker point location into [outPoint] */
void DLL_EX_IM ivwGetLocationPoint(ImodView *inImodView, Ipoint *outPoint);
void DLL_EX_IM ivwSetLocation(ImodView *inImodView, int inX, int inY, int inZ);
void DLL_EX_IM ivwSetLocationPoint(ImodView *inImodView, Ipoint *inPoint);


/*
 * 4D data functions     
 */
/*!
 * Returns the maximum time index, the current time is returned in [outTime].
 * When there is only a single image file, the maximum index and time index 
 * are both 0; when there are multiple times the index is numbered from 1.
 */
int DLL_EX_IM   ivwGetTime(ImodView *inImodView, int *outTime);

/*! Sets the current time to [inTime] */
void DLL_EX_IM  ivwSetTime(ImodView *inImodView, int inTime);

/* !Get the maximum time index */
int DLL_EX_IM   ivwGetMaxTime(ImodView *inImodView);

/*! Gets the label for the current time. */
char DLL_EX_IM *ivwGetTimeLabel(ImodView *inImodView);

/*! Gets the label for time [inIndex]. */
char DLL_EX_IM *ivwGetTimeIndexLabel(ImodView *inImodView, int inIndex);

/*!
 * Sets the time for a new contour [cont[ in object pobj] to match the 
 * current image time. 
 */
void DLL_EX_IM ivwSetNewContourTime(ImodView *vw, Iobj *obj, Icont *cont);

/*!
 * Returns line pointers to loaded image data for the given Z section, 
 * [inSection].  The line pointers are an array of pointers to the data on
 * each line in Y.
 */
unsigned char DLL_EX_IM **ivwGetZSection(ImodView *inImodView, int inSection);

/*!
 * Returns line pointers to loaded image data for the current Z section, 
 */
unsigned char DLL_EX_IM **ivwGetCurrentZSection(ImodView *inImodView);

/*!
 * Returns line pointers to loaded image data for the Z slice [section] and
 * time index [time].
 */
unsigned char DLL_EX_IM **ivwGetZSectionTime(ImodView *iv, int section, 
                                             int time);

/*!
 * Returns grey scale value in memory for given image coordinate [inX],
 * [inY], [inZ]. 
 */
int DLL_EX_IM ivwGetValue(ImodView *inImodView, int inX, int inY, int inZ);

/*! Return value from image file. */
float DLL_EX_IM ivwGetFileValue(ImodView *inImodView, int inX, int inY, 
                                int inZ);


/**************************** Model data functions. ***************************
 *
 * See the libimod library functions for using the model structure.
 *
 */

/*!
 * Gets the model associated with the view.
 */
Imod DLL_EX_IM *ivwGetModel(ImodView *inImodView);


/*! 
 * Gets the number of a free extra object; returns a number greater than 1 or 
 * -1 for an error.  A plugin could claim an object it opens and free it when
 * it closes.
 */
int ivwGetFreeExtraObjectNumber(ImodView *vi);

/*!
 * Clears out and releases the extra object specified by [objNum].  Returns 1 
 * for an object number out of bounds.
 */
int ivwFreeExtraObject(ImodView *vi, int objNum);

/*!
 * Gets pointer to the extra object specified by [objNum].  Do not save this 
 * in a static structure; always get a new pointer before working with it.
 * Returns NULL for an object number out of bounds.
 */
Iobj DLL_EX_IM *ivwGetAnExtraObject(ImodView *inImodView, int objNum);

/*!
 * Deletes all contours in the extra object specified by [objNum].
 */
void DLL_EX_IM ivwClearAnExtraObject(ImodView *inImodView, int objNum);

/*!
 *  Gets the original extra object.  
 *  Do not use this.  Get a free object number and use that instead.
 */
Iobj DLL_EX_IM *ivwGetExtraObject(ImodView *inImodView);

/*!
 * Delete all contours in the original extra object
 */
void DLL_EX_IM ivwClearExtraObject(ImodView *inImodView);

/*!
 * Enable or disable drawing of stippled contours
 */
void DLL_EX_IM ivwEnableStipple(ImodView *inImodView, int enable);

/*!
 * Enable or disable sending mouse moves with button up to plugins
 */
void DLL_EX_IM ivwTrackMouseForPlugs(ImodView *inImodView, int enable);

/*!
 * Gets the current contour or makes a new one if there is none in object 
 * [obj], at the current time or the time indicated by [timeLock].
 */
Icont DLL_EX_IM *ivwGetOrMakeContour(ImodView *vw, Iobj *obj, int timeLock);

/*!
 * Returns 1 if in model mode or 0 if in movie mode
 */
int DLL_EX_IM ivwGetMovieModelMode(ImodView *vw);

/*!
 * Return true if it is possible to display images in overlay mode in the Zap
 * window.
 */
int DLL_EX_IM ivwOverlayOK(ImodView *inImodView);

/*!
 * Sets the overlay mode section difference to [sec], reverse contrast if 
 * [reverse] is nonzero, and displays current or other section in green if 
 * [whichGreen] is 0 or 1.
 */
void DLL_EX_IM ivwSetOverlayMode(ImodView *inImodView, int sec, int reverse, 
                       int whichGreen);
}

/*
 * Selection list functions in imod_edit.cpp
 */

/*!
 * Adds an item to the selection list defined by the object, contour, and point
 * indices in [newIndex].
 */
void DLL_EX_IM imodSelectionListAdd(ImodView *vi, Iindex newIndex);

/*!
 * Clears the selection list and returns the number previously on the list.
 */
int DLL_EX_IM imodSelectionListClear(ImodView *vi);

/*!
 * If contour [co] in object [ob] is on the selection list, returns the point
 * number; otherwise returns -2.  If [co] < 0, it simply tests whether the 
 * object is on the selection list and returns nonzero if so.
 */
int DLL_EX_IM imodSelectionListQuery(ImodView *vi, int ob, int co);

/*!
 * Removes contour [co] in object [ob] from the selection list if it is on it.
 */
void DLL_EX_IM imodSelectionListRemove(ImodView *vi, int ob, int co);

/*!
 * Manages the selection list when there is a new current point selected, for
 * the model [imod].  The current point index is defined in [indSave], and 
 * [controlDown] should be nonzero if the control key is down.
 */
void DLL_EX_IM imodSelectionNewCurPoint(ImodView *vi, Imod *imod, 
                                        Iindex indSave, int controlDown);
/*!
 * Returns the number of selected objects, and returns the minimum and maximum 
 * selected object number in [minOb] and [maxOb].
 */
int DLL_EX_IM imodNumSelectedObjects(ImodView *vi, int &minOb, int &maxOb);


#endif

/* 

$Log$
Revision 1.10  2007/11/27 17:56:57  mast
Added function to enable stipple drawing

Revision 1.9  2006/07/05 04:16:50  mast
Added arguments for overlay mode

Revision 1.8  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 1.7  2006/02/13 05:11:32  mast
Added function to get movie/mouse mode

Revision 1.6  2005/02/19 01:29:38  mast
Added function to clear extra object

Revision 1.5  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 1.4  2004/07/11 18:19:38  mast
Functions to set time of new contour and get/make contour for adding points

Revision 1.3  2004/05/31 23:10:56  mast
Added macros for exporting/importing under Windows

Revision 1.2  2004/01/05 17:55:45  mast
Changes for binning

Revision 1.1  2003/10/01 05:01:01  mast
Initial creation; declarations pulled from imod.h

*/

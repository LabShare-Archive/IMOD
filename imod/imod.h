/*  IMOD VERSION 2.20
 *
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.2  2003/02/10 20:41:55  mast
    Merge Qt source

    Revision 3.1.2.4  2003/01/29 01:43:00  mast
    add colormap draw flag

    Revision 3.1.2.3  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 3.1.2.2  2003/01/13 01:15:42  mast
    changes for Qt version of info window

    Revision 3.1.2.1  2003/01/02 15:38:16  mast
    remove declarations for control.c functions

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/
#ifndef IMOD_H
#define IMOD_H

#ifndef IMODP_H
#include "imodP.h"
#endif

/* imod plugin */
/************************************* defines *******************************/

#define IMOD_PLUG_MENU  1      /* Add to special plugin menu. */
#define IMOD_PLUG_TOOL  2      /* Add to toolbar (future)     */
#define IMOD_PLUG_PROC  4      /* Add to image proc window. (future)  */
#define IMOD_PLUG_VIEW  8      /* Add to model view window. */
#define IMOD_PLUG_KEYS 16      /* Handle key events. */
#define IMOD_PLUG_FILE 32      /* Allow other image files to be loaded. */

#define IMOD_REASON_EXECUTE 1  /* Execute plugin command.                */
#define IMOD_REASON_CLEANUP 2  /* Imod is exiting. Clean up your mess.   */
#define IMOD_REASON_STARTUP 3  /* Imod has started. Initalize your plug. */


       // 1/12/03: Moved plugin stuff to imodplug.h

       // 1/1/03: Moved ivwControl stuff to control.h, until all files can
       // read c++

/*********************** Graphics functions. *********************************/

/* defines for imodDraw(); */
#define IMOD_DRAW_IMAGE       (1) /* image data has been changed.         */
#define IMOD_DRAW_XYZ      (1<<1) /* current point has been changed.      */
#define IMOD_DRAW_MOD      (1<<2) /* model data has been changed.         */
#define IMOD_DRAW_SLICE    (1<<3) /* A slice location has changed.        */
#define IMOD_DRAW_CMAP     (1<<4) /* Colormap has changed.                */
#define IMOD_DRAW_MODVIEW  (1<<5) /* model view changed. */
#define IMOD_DRAW_COLORMAP (1<<11)  /* Colormap has changed (index mode)  */
#define IMOD_DRAW_NOSYNC  (1<<12) /* do not resync image to model point.  */
#define IMOD_DRAW_RETHINK (1<<13) /* recalc cursor position               */
#define IMOD_DRAW_ACTIVE  (1<<14) /* current window is active.            */
#define IMOD_DRAW_TOP     (1<<15) /* current window has highest priority. */

#define IMOD_DRAW_ALL (IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ | IMOD_DRAW_MOD)

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
 * Returns pointer to raw grey scale image data for given z section. 
 */
unsigned char *ivwGetZSection(ImodView *inImodView, int inSection);
unsigned char *ivwGetCurrentZSection(ImodView *inImodView);
unsigned char *ivwGetZSectionTime(ImodView *iv, int section, int time);
     
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
 *  Draw the model using a 2D line renderer implemented with OpenGL functions.
 *  You will need to set up the view matrix yourself.
 */
void imodDrawModel(ImodView *inImodView, Imod *inModel);


/**************************** Application Data *******************************/
/*
Display      *imodDisplay(void);
XtAppContext  imodAppContext(void);
Widget        imodTopLevel(void);
*/
/* These values are the based on the imod global graphics rendering
 * colormap.
 */
/* 
Visual       *imodVisual(void);
XVisualInfo  *imodVisualInfo(void);
Colormap      imodColormap(void);
*/
int           imodDepth(void);


/* color defines. */
#define COLOR_BACKGROUND 1  /* background color */
#define COLOR_FOREGROUND 2  /* foreground color */
#define COLOR_SELECT     3  /* selection color  */
#define COLOR_SHADOW     4  /* shadowed selection color */
#define COLOR_END        5  /* endpoint color   */
#define COLOR_BEGIN      6  /* begin point color */
#define COLOR_POINT      7  /* point color */
#define COLOR_GHOST      8  /* ghost color, darker version of object. */
#define COLOR_MIN        9  /* The index of the minimum image value. */
#define COLOR_MAX       10  /* The index of the maximum image value. */

/*
 * return the pixal value of the given IMOD_COLOR value.
 */
int imodColorValue(int inColor);


/************************ utility functions. *********************************/

/* print text to the imod information window.
 * the usage of this function is similar to the usage of the
 * stdio function printf.
 */
void wprint(char *fmt, ...);

/* 1/13/02: removed inputDefaultKeys
 *  Call a keyevent, execute imod hot keys.
 */
     


/*****************************************************************************/

#endif /* IMOD_H */

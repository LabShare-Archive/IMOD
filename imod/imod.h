/*
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$
    Log at end of file
*/
#ifndef IMOD_H
#define IMOD_H

class QKeyEvent;

#ifndef IMODP_H
#include "imodP.h"
#endif

/* Define macro for export of functions under Windows */
#ifndef DLL_EX_IM
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllexport)
#else
#define DLL_EX_IM
#endif
#endif

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

#include "imodview.h"
extern "C" {

/*
 *  Draw the model using a 2D line renderer implemented with OpenGL functions.
 *  You will need to set up the view matrix yourself.
 */
void DLL_EX_IM imodDrawModel(ImodView *inImodView, Imod *inModel);

/* imod plugin */
/************************************* defines *******************************/

#define IMOD_PLUG_MENU     1   /* Add to special plugin menu. */
#define IMOD_PLUG_TOOL     2   /* Add to toolbar (future)     */
#define IMOD_PLUG_PROC     4   /* Add to image proc window. (future)  */
#define IMOD_PLUG_VIEW     8   /* Add to model view window. */
#define IMOD_PLUG_KEYS    16   /* Handle key events. */
#define IMOD_PLUG_FILE    32   /* Allow other image files to be loaded. */
#define IMOD_PLUG_MESSAGE 64   /* Execute messages */

#define IMOD_REASON_EXECUTE 1  /* Execute plugin command.                */
#define IMOD_REASON_CLEANUP 2  /* Imod is exiting. Clean up your mess.   */
#define IMOD_REASON_STARTUP 3  /* Imod has started. Initalize your plug. */


/**************************** Application Data *******************************/

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
 * return the pixel value of the given IMOD_COLOR value.
 */
int DLL_EX_IM imodColorValue(int inColor);


/************************ utility functions. *********************************/

/* print text to the imod information window.
 * the usage of this function is similar to the usage of the
 * stdio function printf.
 */
void DLL_EX_IM wprint(char *fmt, ...);

/* Print an error message with printf-type arguments
   This function will output a message box if "out" is NULL or under Windows;
   otherwise it will print to "out" */
void DLL_EX_IM imodError(FILE *out, const char *format, ...);

/* Print a message to standard error with printf-type arguments
   This function will flush stderr under Windows */
void DLL_EX_IM imodPrintStderr(const char *format, ...);

/* Print a message to standard out, or to a message box under Windows */
void DLL_EX_IM imodPrintInfo(const char *message);

/* Put a message to standard error like "puts", but flush under Windows */
void DLL_EX_IM imodPuts(const char *message);


/* Call with a keyevent to execute 3dmod hot keys */
void DLL_EX_IM imodDefaultKeys(QKeyEvent *event, ImodView *vw);

/* Show a help page in Qt Assistant; set absolute to true to provide a full
   path instead of a path relative to IMOD_DIR/html/3dmodHelp */
void DLL_EX_IM imodShowHelpPage(const char *page, bool absolute = false);


/*****************************************************************************/
}
#endif /* IMOD_H */

/*
    $Log$
    Revision 3.8  2004/09/24 18:10:04  mast
    Added new definition for plugins that can execute messages

    Revision 3.7  2004/05/31 23:10:56  mast
    Added macros for exporting/importing under Windows

    Revision 3.6  2003/10/01 05:04:44  mast
    Changes for creation of imodview.h

    Revision 3.5  2003/09/16 02:49:06  mast
    Changed declarations ofr functions that return image line pointers

    Revision 3.4  2003/06/27 19:24:45  mast
    Add function to get extra object

    Revision 3.3  2003/02/27 18:13:59  mast
    Add ImodView argument to imodDrawModel so that times can be detected

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

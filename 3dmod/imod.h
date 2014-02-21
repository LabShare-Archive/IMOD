/*  imod.h - public header file for 3dmod
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
#ifndef IMOD_H
#define IMOD_H

/* This include is needed for various other items to compile */
#include <QKeyEvent>

#ifndef IMODP_H
#include "imodP.h"
#include "utilities.h"
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
/* DOC_SECTION DEFINES */
/* DOC_CODE Draw flags */
/* 
 * Defines for imodDraw() and ivwDraw().  IMOD_DRAW_ACTIVE and IMOD_DRAW_TOP
 * are flags added by the control module when calling the active or top window
 * and would not be included in calls to the draw routines.
 */

/* image data or color map has changed; windows need to clear caches and draw
   all images */
#define IMOD_DRAW_IMAGE       (1) 

/* The current point x, y, z position has been changed.  This causes an update
   of the info window x/y/z, image scale dialog if multifile Z loaded, the 
   image processing window, and the isosurface window.  Model view will be 
   drawn if the image display is on, unless the IMOD_DRAW_SKIPMODV flag is 
   also set. */
#define IMOD_DRAW_XYZ      (1<<1) 

/* Model data or current model object/contour/point has been changed. 
   Prior to XYZ update, this causes update to info window obj/cont/pt, and
   to object edit, cont/surf/pt, contour move and fine grain dialogs and
   update of plugins with IMOD_REASON_MODUPDATE.  Model view will be drawn
   unless the IMOD_DRAW_SKIPMODV flag is also set. */
#define IMOD_DRAW_MOD      (1<<2)

/* A slicer location has changed; Zap and XYZ windows will draw slicer lines */
#define IMOD_DRAW_SLICE    (1<<3) 

/* Skip drawing model view.  Use this to cause a draw of image windows when
 a model view draw is already inevitable */
#define IMOD_DRAW_SKIPMODV (1<<5) 

/* Color index mode color map has changed.  Do not combine with other flags. */
#define IMOD_DRAW_COLORMAP (1<<11)

/* Do not resync image to model point.  This keeps Zap from recentering. */
#define IMOD_DRAW_NOSYNC  (1<<12)

/* Recalculate cursor position: adds the IMOD_DRAW_MOD flag, and if there is
   a current model point defined, sets current point x,y,z to it and adds the 
   IMOD_DRAW_XYZ flag */
#define IMOD_DRAW_RETHINK (1<<13)
#define IMOD_DRAW_ACTIVE  (1<<14) /* current window is active.            */
#define IMOD_DRAW_TOP     (1<<15) /* current window has highest priority. */

#define IMOD_DRAW_ALL (IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ | IMOD_DRAW_MOD)
/* END_CODE */
/* END_SECTION */

#include "imodview.h"

#define IMOD_MM_TOGGLE 0

extern "C" {

/*!
 * Draw [model] using a 2D line renderer implemented with OpenGL functions.
 * This is the function used to draw the slicer window.  You will need to set
 * up the view matrix yourself, including Z limits in the projection to avoid
 * getting the whole model drawn on a slice.  Set [drawCurrent] nonzero to
 * have current image or model point and current contour end symbols drawn too.
 * Provide the z-scaling used to render the image in [zscale].
 */
void DLL_EX_IM imodDrawModel(ImodView *inImodView, Imod *inModel, 
                             int drawCurrent, float zscale);
/* 3dmod plugin */
/************************************* defines *******************************/
/* DOC_SECTION DEFINES */
/* DOC_CODE Plugin flags */
/* The IMOD_PLUG_ flags are ones that a plugin sends back in response to
 * the ImodPlugInfo call.  The IMOD_REASON_ values are sent to the plugin in 
 * the imodPlugExecute call 
 */
#define IMOD_PLUG_MENU      1  /* Add to special plugin menu. */
#define IMOD_PLUG_TOOL      2  /* Add to toolbar (not implemented)     */
#define IMOD_PLUG_PROC      4  /* Add to image proc window. (not implemented) */
#define IMOD_PLUG_VIEW      8  /* Add to model view window. (not implemented) */
#define IMOD_PLUG_KEYS     16  /* Handle key events. */
#define IMOD_PLUG_FILE     32  /* Allow other image files to be loaded (?). */
#define IMOD_PLUG_MESSAGE  64  /* Execute messages */
#define IMOD_PLUG_MOUSE   128  /* Handle mouse events */
#define IMOD_PLUG_EVENT   256  /* Handle other events (wheel, enter, leave) */
#define IMOD_PLUG_CHOOSER 512  /* Handle file choosing operations */

#define IMOD_REASON_EXECUTE   1 /* Execute plugin after selection from menu  */
#define IMOD_REASON_STARTUP   3 /* 3dmod has started. Initialize plugin. */
#define IMOD_REASON_MODUPDATE 4 /* Update the plugin for model changes */
#define IMOD_REASON_NEWMODEL  5 /* Update the plugin for new model loaded */
/* END_CODE */

/* DOC_CODE Snapshot flags */
/* Codes for snapshot formats.  TIF and RGB are used exclusively within 3dmod, 
 * with RGB used for either the user's selected non-TIFF format or the second
 * non-TIFF format.
 */
#define SnapShot_Default 0
#define SnapShot_RGB     1
#define SnapShot_TIF     2
#define SnapShot_PNG     3
#define SnapShot_JPG     4


/**************************** Application Data *******************************/

/* DOC_CODE Color defines */
/* color defines. */
#define COLOR_BACKGROUND 1  /* background color */
#define COLOR_FOREGROUND 2  /* foreground color */
#define COLOR_SELECT     3  /* selection color  */
#define COLOR_SHADOW     4  /* shadowed selection color */
#define COLOR_END        5  /* endpoint color   */
#define COLOR_BEGIN      6  /* begin point color */
#define COLOR_POINT      7  /* point color */
#define COLOR_GHOST      8  /* ghost color, darker version of object. */
#define COLOR_ARROW      9  /* first arrow color */
#define COLOR_ARROW2    10  /* second arrow color */
#define COLOR_ARROW3    11  /* third arrow color */
#define COLOR_ARROW4    12  /* fourth arrow color */
#define COLOR_MIN       13  /* The index of the minimum image value. */
#define COLOR_MAX       14  /* The index of the maximum image value. */
/* END_CODE */
/* END_SECTION */

/*!
 * Gets the color depth; 24 on current systems with no color index mode *
 */
int DLL_EX_IM imodDepth(void);

/*!
 * Returns the pixel value of the given COLOR_ value in [inColor], used to 
 * call b3dColorIndex
 */
int DLL_EX_IM imodColorValue(int inColor);


/************************ utility functions. *********************************/

/*! Print text to the 3dmod information window.
 * The usage of this function is similar to the usage of the
 * stdio function printf.
 */
void DLL_EX_IM wprint(const char *fmt, ...);

/*! Prints an error message with printf-type arguments
   This function will output a message box if [out] is NULL or under Windows;
   otherwise it will print to [out], which should be {stdout} or {stderr} */
void DLL_EX_IM imodError(FILE *out, const char *format, ...);

/*! Prints a message to standard error with printf-type arguments
  This function will flush stderr under Windows */
void DLL_EX_IM imodPrintStderr(const char *format, ...);

/*! Prints a message using @imodPrintStderr if the letter [key] is entered as 
  a debug key letter with -D */
void DLL_EX_IM imodTrace(char key, const char *format, ...);

/*! Puts [message] to standard error like puts, but flushes under Windows */
void DLL_EX_IM imodPuts(const char *message);

/*! Prints [message] to standard out, or to a message box under Windows */
void DLL_EX_IM imodPrintInfo(const char *message);


/*! Passes [even] to the general key handler to execute 3dmod hot keys */
void DLL_EX_IM imodDefaultKeys(QKeyEvent *event, ImodView *vw);

/*! Shows a help page with filename [page] in Qt Assistant; provide a full
 * path if the path is not relative to IMOD_DIR/html/3dmodHelp.
 * Returns 1 for error, 0 for success 
 */
int DLL_EX_IM imodShowHelpPage(const char *page);


/*****************************************************************************/
}
#endif /* IMOD_H */


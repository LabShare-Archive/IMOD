/*  imodP.h -- Main header file for imod - private functions
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef IMODP_H
#define IMODP_H

#include "imodconfig.h"

#include <stdio.h>
#include <qstring.h>
//Added by qt3to4:
#include <QPixmap>
#include "imodel.h"
#include "imodi.h"

class QGLColormap;
class QCursor;
class QPixmap;
class UndoRedo;
class QTextEdit;
typedef struct ilist_struct Ilist;

/* DNM 12/22/02: eliminated multiple view structures */
typedef struct imod_application
{
     struct ViewInfo *cvi; /* current view                    */     

  QGLColormap *qColormap;
  int          depth;
  int          doublebuffer;
  int          rgba;
  int         qtEnableDepth;
  QCursor *modelCursor;
  QPixmap *iconPixmap;
  int      convertSnap;        // Convert RGB to gray scale

  
  /* Global color pixel values */
  int base;
  int objbase;
  int background;
  int foreground;
  int select;
  int shadow;
  int endpoint;
  int bgnpoint;
  int curpoint;
  int ghost;
  
  short wzoom;
  int closing;      // Flag that widows are being closed on exit
  int exiting;      // Flag that application exit has been called
  int listening;    // Flag that it was started with -L or -W

}ImodApp;

extern ImodApp *App;

typedef struct imod_control_list ImodControlList;
typedef struct imod_autox_struct Autox;
typedef struct xbldrcoloramp Cramp;
class ImodWorkproc;
class ImodAssistant;
class ImodClipboard;

typedef struct
{
  int     cz;
  int     ct;
  int     used;
  Islice *sec;
  
}ivwSlice;

/* used to show current slice */
struct imod_showslice_struct
{
  int zx1, zx2;
  int zy1, zy2;  
  int xy1, xy2;
  int xz1, xz2;
  int yx1, yx2;
  int yz1, yz2;     
};


/* 
 * The view that imod used to store all image and model data.
 */
typedef struct ViewInfo
{
  unsigned char **idata;  /* 8 bit 3-D grey scale data. */

  int   xsize, ysize, zsize;      /* Size of idata */
  unsigned int xysize;                   /* section size. */
  float  xmouse, ymouse, zmouse;   /* Current point in idata. */
  int   xUnbinSize, yUnbinSize, zUnbinSize;  /* Original size of data */

  int   nt, ct; /* number of time frames, current time.       */

  struct LoadInfo *li; 
  ImodImageFile   *image;
  ImodImageFile   *imageList;
  ImodImageFile   *hdr;

  int      modelViewVi;       /* This is a viewinfo from model view */
  int      vmSize;            /* virtual memory z-section size. */
  ivwSlice *vmCache;          /* the cache of z-section data.   */
  int      vmCount;           /* Use counter for cache */
  int      *cacheIndex;       /* Cross-index from sections to cache entries */
  int      vmTdim;            /* Time dimension of cross-index */
  int      vmTbase;           /* Base T value for lookups - 0 or 1 */
  int      fullCacheFlipped;  /* Flag that cache was full upon flipping */
  int      keepCacheFull;     /* Cache was not specified by user, fill it */
  int      loadingImage;      /* Flag that data is being loaded */
  int      doingInitialLoad;  /* and that the load is the initial one */
  int      xybin;             /* Binning in X and Y dimensions */
  int      zbin;              /* Binning in Z */

  /* Image data scaleing for gray scale images. */
  int    rampbase;
  int    rampsize;
  int    black;
  int    white;

  /* motion control */
  int movierate;
  int xmovie, ymovie, zmovie, tmovie;
  unsigned int movieInterval;  
  int movieRunning;
  ImodWorkproc *timers;       /* Class with QTimers */

  /* XYZ slice points. */
  struct imod_showslice_struct slice;
  struct imod_showslice_struct lslice;

  /* Grey Scale Ramp Data. */
  Cramp *cramp;

  /* THE MODEL and extra objects and the selection list */
  Imod  *imod;
  Iobj  *extraObj;        /* The general extra object array */
  int   numExtraObj;      /* Number of extra objects allocated */
  int   *extraObjInUse;   /* Flags for whether objects are in use */
  Ilist *selectionList;

  /* Tilt angles number and array */
  int numTiltAngles;
  float *tiltAngles;

  /* storage for list of line pointers and a blank line */
  unsigned char **linePtrs;
  int linePtrMax;
  unsigned char *blankLine;

  /* Extra Window Data. */
  /* 12/7/02: zap not needed; 12/10/02 xyz not needed either */
  Autox  *ax;
  ImodControlList *ctrlist;
  UndoRedo *undo;             /* Undo -redo class */

  /* Some Flags. */
  int    dim;             /* bits 0..4, x, y, z, t */
  int    obj_moveto;      /* default object to move contour to. */
  int    ghostmode;
  int    ghostlast;       /* last value of mode, when toggled by g */
  int    ghostdist;       /* Maximum distance for ghosts */
  int    insertmode;      /* insert points before/after current point. */
  int    fastdraw;    
  int    drawcursor;
  int    ifd;
  int    overlaySec;      /* Section to show in overlay color */
  int    overlayRamp;     /* Color ramp when first started overlay mode */
  int    whichGreen;      /* Whether main section (0) or other (1) is green */
  int    reverseOverlay;  /* Toggle reverse contrast when going in or out */
  int    drawStipple;     /* Flag to draw contours with stipple if flag set */
  int    trackMouseForPlugs; /* Number of plugins that want mouse tracking */
     
  int      flippable;     /* Flag that images can be y-z flipped */
  short    fakeImage;     /* No real image data. */
  short    rawImageStore; /* the MRC_MODE in which the raw image is stored. 
                           * if not 0, data will be cached.
                           * 0  = unsigned bytes.
                           * 16 = color rgb unsigned byte triplets.
                           */
  int     colormapImage;  /* Flag that byte images with colormaps are loaded */
  int     grayRGBs;       /* Flag to load MRC RGBs as gray scale */
  int     multiFileZ;     /* Flag that multiple single-image files are sections
                             in Z (if > 0) or to be treated as times (< 0) */
  int     reloadable;     /* Model file exists and can be reloaded */
  int     noReadableImage; /* Flag that image file is not readable (stdin) */

  FILE   *fp;                /* current image file pointer.    */

}ImodView;


/*****************************************************************************/
/* Global Variables */
extern struct Mod_Model *Model;
#define IMOD_FILENAME_SIZE 256
extern char   Imod_filename[IMOD_FILENAME_SIZE];

extern int ImodTrans;
extern int Imod_debug;

extern char *Imod_imagefile;
extern QString Imod_cwdpath;
extern QString Imod_IFDpath;
extern ImodAssistant *ImodHelp;
extern ImodClipboard *ClipHandler;

extern int Rampbase;
extern int (*ivwFastGetValue)(int x, int y, int z);

/*****************************************************************************/

#define IMOD_SELSIZE 15   /* Distance for selecting model points with mouse. */

#define LATIN1(a) ((const char *)a.toLatin1())

#define RADIANS_PER_DEGREE 0.017453293
#define MOVIE_DEFAULT 52965

#define IMOD_GHOST_NEXTSEC (1)
#define IMOD_GHOST_PREVSEC (2)
#define IMOD_GHOST_SECTION (3)
#define IMOD_GHOST_SURFACE (1<<2)
#define IMOD_GHOST_ALLOBJ  (1<<3)
#define IMOD_GHOST_LIGHTER (1<<4)

/* Colors for 12-bit colormap systems */
#define RAMPBASE 256
#define RAMPSIZE 256  
#define RAMPSTEP 1
#define RAMP_INTERVAL 480
#define MAXIMUM_RAMPS 8

/* colors for 8-bit systems */
#define IMOD_MAX_INDEX  236
#define IMOD_GHOST      (IMOD_MAX_INDEX - 2)
#define IMOD_CURPOINT   (IMOD_MAX_INDEX - 3)
#define IMOD_BGNPOINT   (IMOD_MAX_INDEX - 4)
#define IMOD_ENDPOINT   (IMOD_MAX_INDEX - 5)
#define IMOD_SHADOW     (IMOD_MAX_INDEX - 6)
#define IMOD_SELECT     (IMOD_MAX_INDEX - 7)
#define IMOD_FOREGROUND (IMOD_MAX_INDEX - 8)
#define IMOD_BACKGROUND (IMOD_MAX_INDEX - 9)
#define RAMPMAX         (IMOD_MAX_INDEX - 11)
#define RAMPMIN         101
#define IMOD_MIN_INDEX  16


/****************************************************************************/
/* Private functions for internal imod use.                                 */

int imodLoopStarted();
void imod_quit(void);
bool imodDebug(char key);
void wprintWidget(QTextEdit *edit);
void wprintWriteFile(void);

#endif     


/*
$Log$
Revision 3.47  2009/03/22 21:19:06  mast
Add flag for app closing windows

Revision 3.46  2009/02/27 23:47:05  mast
Added listening flag

Revision 3.45  2009/01/15 16:33:17  mast
Qt 4 port

Revision 3.44  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 3.43  2008/05/27 05:38:51  mast
Various new variables

Revision 3.42  2008/04/29 22:30:56  mast
Added array for keeping track of extra objects

Revision 3.41  2008/04/02 04:12:41  mast
Add flag for reading from stdin

Revision 3.40  2008/01/14 19:46:55  mast
moved toggle flag to public file

Revision 3.39  2007/12/04 22:04:22  mast
Changes for rearrangement of utilities, extra object, mouse tracking

Revision 3.38  2007/11/27 17:53:54  mast
Add element to enable stipple drawing

Revision 3.37  2007/09/20 22:05:37  mast
Defined RADIANS_PER_DEGREE once

Revision 3.36  2006/09/28 21:17:51  mast
Changed xysize to unsigned FWIW

Revision 3.35  2006/08/28 05:19:05  mast
Added variable for colormapped images

Revision 3.34  2006/07/05 04:16:32  mast
More members for overlay mode

Revision 3.33  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 3.32  2006/06/19 05:24:24  mast
Made clipboard object a global so it can be deleted

Revision 3.31  2005/10/14 22:02:19  mast
Added reloadable variable

Revision 3.30  2004/12/04 02:10:16  mast
Moved declaration of ImodHelp into here

Revision 3.29  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 3.28  2004/11/05 19:08:12  mast
Include local files with quotes, not brackets

Revision 3.27  2004/11/01 22:51:48  mast
Added selection list

Revision 3.26  2004/07/11 18:16:27  mast
Added new ghost mode flags

Revision 3.25  2004/05/31 23:09:37  mast
moved printing functions to imod.h

Revision 3.24  2004/05/28 23:30:23  mast
*** empty log message ***

Revision 3.23  2004/01/06 16:43:30  mast
Add flag for gray-scale RGB reading

Revision 3.22  2004/01/05 17:55:45  mast
Changes for binning

Revision 3.21  2003/12/30 06:37:03  mast
Add multifileZ flag

Revision 3.20  2003/11/01 18:12:54  mast
add error output function

Revision 3.19  2003/10/01 05:04:58  mast
Changes for creation of imodviewP.h

Revision 3.18  2003/09/16 02:47:41  mast
Added variables and changed declarations for accessing images from line
pointers

Revision 3.17  2003/09/13 04:31:08  mast
Add a define for the size of the global array with model filename

Revision 3.16  2003/06/27 19:25:02  mast
Add extra object

Revision 3.15  2003/06/04 23:43:20  mast
Move message defines to imod_client_message.h

Revision 3.14  2003/05/23 02:47:34  mast
Defined message for raising windows

Revision 3.13  2003/05/18 22:08:48  mast
Changes to add an application icon

Revision 3.12  2003/04/18 20:15:17  mast
Add flag to set if program is exiting

Revision 3.11  2003/03/03 22:10:27  mast
Added modeling cursor to App structure

Revision 3.10  2003/02/27 18:17:36  mast
Changes for using Qt directory functions

Revision 3.9  2003/02/10 20:36:28  mast
Merge Qt source

Revision 3.8.2.18  2003/01/29 01:33:37  mast
remove qtRgba and qtDoubleBuffer

Revision 3.8.2.17  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 3.8.2.16  2003/01/23 20:00:02  mast
Changes for Qt versions of final dialog boxes

Revision 3.8.2.15  2003/01/18 01:06:34  mast
remove imod_cachefill declarations

Revision 3.8.2.14  2003/01/14 21:43:17  mast
changes for Qt versions of autox and imod_moviecon

Revision 3.8.2.13  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 3.8.2.12  2003/01/10 23:50:09  mast
Changes for Qt version of tumbler an elimination of tilt window

Revision 3.8.2.11  2003/01/06 15:44:23  mast
changes for making Qt version of slicer

Revision 3.8.2.10  2003/01/02 15:38:16  mast
remove declarations for control.c functions

Revision 3.8.2.9  2002/12/23 04:55:43  mast
A little more cleanup

Revision 3.8.2.8  2002/12/19 04:37:13  mast
Cleanup of unused global variables and defines

Revision 3.8.2.7  2002/12/17 18:40:24  mast
Changes and new includes with Qt version of imodv

Revision 3.8.2.6  2002/12/14 05:40:43  mast
new visual-assessing code

Revision 3.8.2.5  2002/12/13 06:09:09  mast
include file changes

Revision 3.8.2.4  2002/12/12 01:21:09  mast
Changes for xyz window to become Qt

Revision 3.8.2.3  2002/12/09 17:49:19  mast
changes to get Zap as a Qt window

Revision 3.8.2.2  2002/12/07 01:23:50  mast
*** empty log message ***

Revision 3.8.2.1  2002/12/05 16:29:02  mast
declare ioew_sgicolor_cb

Revision 3.8  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.7  2002/11/25 19:19:06  mast
Eliminated conditional on USE_IMOD_CONTROL

Revision 3.6  2002/10/23 15:56:23  mast
Blew away part of file before previous checkin

Revision 3.5  2002/10/22 22:45:34  mast
Removed some declarations from old workproc routines

Revision 3.4  2002/10/22 22:39:24  mast
*** empty log message ***

Revision 3.3  2002/09/19 22:53:36  rickg
Added MESSAGE_QUIT define

Revision 3.2  2002/09/13 20:57:45  mast
Added defines for MESSAGES's, removed redundant declarations that are also
in imod_io.h

*/

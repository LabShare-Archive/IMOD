/*  IMOD VERSION 2.50
 *
 *  imod.h -- Main header file for imod.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
Log at end of file
*/

#ifndef IMODP_H
#define IMODP_H

#include <imodconfig.h>

#include <stdio.h>
#include <qstring.h>
#include <imodel.h> 
#include <imodi.h>

class QGLColormap;

/* DNM 12/22/02: eliminated multiple view structures */
typedef struct imod_application
{
     struct ViewInfo *cvi; /* current view                    */     

  QGLColormap *qColormap;
  int          depth;
  int          doublebuffer;
  int          rgba;
  int         qtEnableDepth;
  
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

}ImodApp;

extern ImodApp *App;

typedef struct imod_control_list ImodControlList;
typedef struct imod_autox_struct Autox;
typedef struct xbldrcoloramp Cramp;
class ImodWorkproc;


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
  int   xysize;                   /* section size. */
  float  xmouse, ymouse, zmouse;   /* Current point in idata. */

  int   nt, ct; /* number of time frames, current time.       */

  struct LoadInfo *li; 
  ImodImageFile   *image;
  ImodImageFile   *imageList;
  ImodImageFile   *hdr;

  int      vmSize;            /* virtual memory z-section size. */
  ivwSlice *vmCache;          /* the cache of z-section data.   */
  int      vmCount;           /* Use counter for cache */
  int      vmLastUsed;        /* Index of last accessed section */

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

  /* THE MODEL */
  Imod  *imod;

  /* Extra Window Data. */
  /* 12/7/02: zap not needed; 12/10/02 xyz not needed either */
  Autox  *ax;
  ImodControlList *ctrlist;

  /* Some Flags. */
  int    dim;         /* bits 0..4, x, y, z, t */
  int    obj_moveto;  /* default object to move contour to. */
  int    ghostmode;
  int    ghostlast;   /* last value of mode, when toggled by g */
  int    ghostdist;    /* Maximum distance for ghosts */
  int    insertmode;  /* insert points before/after current point. */
  int    fastdraw;    
  int    drawcursor;
  int    ifd;

     
  int      flippable;     /* Flag that images can be y-z flipped */
  short    fakeImage;     /* No real image data. */
  short    rawImageStore; /* the MRC_MODE in which the raw image is stored. 
                           * if not 0, data will be cached.
                           * 0  = unsigned bytes.
                           * 16 = color rgb unsigned byte triplets.
                           */

  int            imageSize;

  FILE   *fp;                /* current image file pointer.    */

}ImodView;


/*****************************************************************************/
/* Global Variables */
extern struct Mod_Model *Model;
extern char   Imod_filename[256];

extern int ImodTrans;
extern int Imod_debug;

extern char *Imod_imagefile;
extern QString Imod_cwdpath;
extern QString Imod_IFDpath;

extern int Rampbase;

/*****************************************************************************/

#define IMOD_SELSIZE 15   /* Distance for selecting model points with mouse. */

#define MOVIE_DEFAULT 52965
#define IMOD_MM_TOGGLE 0

#define IMOD_GHOST_NEXTSEC (1)
#define IMOD_GHOST_PREVSEC (2)
#define IMOD_GHOST_SECTION (3)
#define IMOD_GHOST_SURFACE (1<<2)

#define MESSAGE_NO_ACTION   0
#define MESSAGE_OPEN_MODEL  1
#define MESSAGE_SAVE_MODEL  2
#define MESSAGE_VIEW_MODEL  3
#define MESSAGE_QUIT  4

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
/* Public functions that can be used via plugin.                            */
#include <imod.h>

/****************************************************************************/
/* Private functions for internal imod use.                                 */

void imod_quit(void);
char *imodwfname(char *intro);
char *imodwEithername(char *intro, char *filein, int modelFirst);
char *imodwGivenName(char *intro, char *filein);
QString imodCaption(char *intro);

unsigned char *ivwGetCurrentSection(ImodView *iv);
int ivwInitCache(ImodView *vi);

void ivwBindMouse(ImodView *vw);

int  ivwScale(ImodView *vw);
int  ivwFlip(ImodView *vw);
void ivwInit(ImodView *vi);
int  ivwPointVisible(ImodView *vw, Ipoint *pnt);
float ivwGetFileValue(ImodView *vw, int cx, int cy, int cz);

int  imodImageFileDesc(FILE *fin);
int  ivwLoadImage(ImodView *iv);
void ivwFlushCache(ImodView *vi);
int  ivwSetScale(ImodView *vi);
void ivwMultipleFiles(ImodView *iv, char *argv[], int firstfile, 
		      int lastimage);

void ivwTransModel(ImodView *iv);
void ivwSetModelTrans(ImodView *iv);
void ivwFlipModel(ImodView *iv);
void ivwCheckWildFlag(Imod *imod);
void ivwScaleDepth8(ImodView *iv, ivwSlice *tempSlicePtr);
void ivwReadZ(ImodView *iv, unsigned char *buf, int cz);


#endif     


/*
$Log$
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

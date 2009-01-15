/* 
 *  imodv.h -- Main imodv include file.
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

#ifndef IMODV_H
#define IMODV_H

#include <limits.h>
#include "imodconfig.h"

#include "imodel.h"       /* imod library include. */
//Added by qt3to4:
#include <QPixmap>

/* used for finding bounding box. */
#ifndef FLT_MAX
#define FLT_MAX 5e+29f
#endif

#ifndef IMODV_WINDOW_H
class ImodvWindow;
#endif
class QColor;
class QPixmap;

/* Number of frames to determine frame rate over */
#define MAX_MOVIE_TIMES  10

typedef struct __imodv_struct
{
  /* model data.   */
  Imod **mod;     /* model array              */
  Imod *imod;     /* current model            */
  int  nm;        /* number of models         */
  int  cm;        /* current model number     */
     
  Iobj *obj;      /* Current object edit.     */
  int    ob;      /* Current obj number       */
  Iobj *dobj;     /* default obj=bounding box */
  struct Mod_Draw  *md;  /* transformations   */
  Imat  *mat;     /* Transformation matrix storage.   */
  Imat  *rmat;    /* rotation matrix. */

  /* windowing data */
  int          wpid;
  ImodvWindow  *mainWin;
  QPixmap *iconPixmap;

  char         *rbgname;
  QColor       *rbgcolor; /* background color for rendering.    */
  int          enableDepthSB;  /* Flags for if visuals exist and have depth */
  int          enableDepthDB;
  int          enableDepthSBst;
  int          enableDepthDBst;

  /* global viewing flags */
  int cnear;       /* clipping planes.                        */
  int cfar;
  int fovy;        /* field of view angle for perspective.    */
  int db;          /* use doublebuffer widget if true         */
  int winx, winy;  /* current drawing window size.            */
  int lmx, lmy;    /* last x,y mouse location.                */
  int lightx,
    lighty;

  int mousemove;   /* 0 = move model, 1 light, 2 clip.        */
  int stereo;      /* 0 = no stereo view.                     */
                   /* 1 = cross, 2 = wall, 3 = red/green      */
                   /* 4 = display hardware stereo.            */
  int clearAfterStereo;   /* Flag to clear right buffer after leaving stereo */
  float plax;      /* parallax for stereo separation.         */
  int movie;       /* allow movies.                           */
  int drawall;     /* draw all models at once.                */
  int alpha;       /* number of alpha planes.                 */
  int current_subset;  /* display subset of model (current element) */
                       /* 0 = all, 1 = obj, 2 = surf, 3 = cont */
  int drawExtraOnly;   /* Draw only extra objects, skip rest of model */
  int movieFrames;     /* Number of movie frames displayed     */
  int movieStart;      /* Starting time of movie           */
  int movieCurrent;    /* Current time of movie          */
  float movieSpeed;    /* Speed in degrees per second */
  float throwFactor;   /* Speed multiplier if throw occurs */
  int movieTimes[MAX_MOVIE_TIMES];  /* Ring buffer of times */
  int snap_fileno;      /* Snapshot file number */
  int drawClip;         /* Draw current clip plane */
  int drawLight;        /* Draw lighting vector */
  int linkToSlicer;     /* Flag to link to top slicer */
  int linkSlicerCenter; /* Flag to link center of rotation too */
  float scaleBarSize;   /* Size of scale bar that was last drawn */
  int boundBoxExtraObj; /* Number of extra object for bounding box */
  int curPointExtraObj; /* Number of extra object for current point */

  /* start-up flags */
  int  moveall;    /* move all models if true.                 */
  int  crosset;    /* edit all models if true.                 */
  int  fullscreen; /* open full sized screen with no border.   */
  int  standalone; /* type of program being used.              */
  Iview view;

  /* texture mapping */
  int    texMap;
  int    texTrans;
  struct ViewInfo *vi;

  /* picking */
  int doPick;
  int xPick, yPick;
  int wPick, hPick;

  int lighting;
  int depthcue;
  int wireframe;
  int lowres;
} ImodvApp;

extern ImodvApp *Imodv;
extern int ImodvClosed;

/* imodv.cpp functions (the only ones that belong here ) */
void imodv_open(void);
void imodv_draw(void);
void imodv_close(void);
void imodv_new_model(Imod *mod);
int  imodv_main(int argc, char **argv);
void imodvSetCaption();
void imodvDrawImodImages(int skipDraw = 0);
int imodvByteImagesExist();
void imodvRegisterModelChg();
void imodvRegisterObjectChg(int object);
void imodvFinishChgUnit();
void imodvQuit();
int imodvStandalone();
int imodvLinkedToSlicer();
int imodvRotCenterLinked();
void imodvNewModelAngles(Ipoint *rot);

/*

$Log$
Revision 3.25  2008/12/15 21:23:22  mast
Variables for seperate stereo widgets

Revision 3.24  2008/11/28 06:40:24  mast
Added extra object variables for bounding box and current point

Revision 3.23  2008/10/02 22:43:28  mast
Add flag for clearing after leaving stereo

Revision 3.22  2008/06/10 05:49:19  mast
Add flag for drawing the light vector

Revision 3.21  2008/05/27 05:48:41  mast
Changes for linking slicer center of rotation

Revision 3.20  2008/04/01 23:43:38  mast
Added flag for drawing only extra objects

Revision 3.19  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 3.18  2007/11/30 06:51:50  mast
Changes for linking slicer to model view

Revision 3.17  2007/09/20 22:06:55  mast
Changes for visualizing clipping plane

Revision 3.16  2004/11/21 06:07:49  mast
Changes for undo/redo

Revision 3.15  2004/11/05 19:08:12  mast
Include local files with quotes, not brackets

Revision 3.14  2004/06/06 21:27:21  mast
Eliminated stereo-command related items

Revision 3.13  2003/12/30 06:32:16  mast
Make snap_fileno be part of imodvApp structure

Revision 3.12  2003/11/26 18:15:49  mast
Add function to determine if byte images exist

Revision 3.11  2003/11/12 18:52:52  mast
Move quit function to imodv.cpp

Revision 3.10  2003/11/04 04:41:07  mast
Add new movie variables

Revision 3.9  2003/06/27 19:43:46  mast
Eliminate the fastdraw flag in favor of bits in the world flag

Revision 3.8  2003/05/18 22:08:48  mast
Changes to add an application icon

Revision 3.7  2003/03/26 23:22:20  mast
Change argument to main entry function

Revision 3.6  2003/03/04 21:42:37  mast
Add function to draw imod windows

Revision 3.5  2003/02/27 23:11:24  mast
Changes for using Qt timing functions

Revision 3.4  2003/02/10 20:41:55  mast
Merge Qt source

Revision 3.3.2.8  2003/01/29 01:32:01  mast
add close call

Revision 3.3.2.7  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 3.3.2.6  2003/01/18 01:00:01  mast
remove dia_qtutils include

Revision 3.3.2.5  2003/01/01 19:12:31  mast
changes to start Qt application in standalone mode

Revision 3.3.2.4  2003/01/01 05:46:29  mast
changes for qt version of stereo

Revision 3.3.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 3.3.2.2  2002/12/17 17:41:58  mast
Changes for Qt port of imodv

Revision 3.3.2.1  2002/12/09 17:49:19  mast
changes to get Zap as a Qt window

Revision 3.3  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.2  2002/09/04 00:26:25  mast
Added declarations for imodv_init and imodvGetVisuals

Revision 3.1  2002/07/18 20:19:38  rickg
Changed include of GLwMDrawA to rely upon -I compiler option

*/

#endif /* imodv.h */

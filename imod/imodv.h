/*  IMOD VERSION 2.50
 *
 *  imodv.h -- Main imodv include file.
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

#ifndef IMODV_H
#define IMODV_H

#include <limits.h>
#include <imodconfig.h>

#include <imodel.h>       /* imod library include. */

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
  int    dlist;   /* use display list           */
  int    update_dlist; /* update display list    */

  /* windowing data */
  int          wpid;
  ImodvWindow  *mainWin;
  QPixmap *iconPixmap;

  char         *rbgname;
  QColor       *rbgcolor; /* background color for rendering.    */
  int          enableDepthSB;
  int          enableDepthDB;
  int          stereoSB;   /* Flags for whether visuals have stereo */
  int          stereoDB;

  char * stereoCommand;
  char * restoreCommand;
  char * SGIStereoCommand;
  char * SGIRestoreCommand;

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
  float plax;      /* parallax for stereo separation.         */
  int movie;       /* allow movies.                           */
  int drawall;     /* draw all models at once.                */
  int alpha;       /* number of alpha planes.                 */
  int current_subset;  /* display subset of model (current element) */
                       /* 0 = all, 1 = obj, 2 = surf, 3 = cont */
  int movieFrames;     /* Number of movie frames displayed     */
  int movieStart;      /* Starting time of movie           */
  int movieCurrent;    /* Current time of movie          */
  float movieSpeed;    /* Speed in degrees per second */
  float throwFactor;   /* Speed multiplier if throw occurs */
  int movieTimes[MAX_MOVIE_TIMES];  /* Ring buffer of times */

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
void imodvDrawImodImages();

/*
$Log$
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

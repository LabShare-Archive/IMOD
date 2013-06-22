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
class VertBufManager;

/* Number of frames to determine frame rate over */
#define MAX_MOVIE_TIMES  10

typedef struct __imodv_struct
{
  /* model data.   */
  Imod **mod;     /* model array              */
  Imod *imod;     /* current model            */
  int  numMods;   /* number of models         */
  int  curMod;    /* current model number     */
     
  Iobj  *obj;     /* Current object edit.     */
  int   objNum;   /* Current obj number       */
  Imat  *mat;     /* Transformation matrix storage.   */
  Imat  *rmat;    /* rotation matrix. */

  /* windowing data */
  int          wpid;
  ImodvWindow  *mainWin;
  QPixmap *iconPixmap;

  const char   *rbgname;
  QColor       *rbgcolor; /* background color for rendering.    */
  int          enableDepthSB;  /* Flags for if visuals exist and have depth */
  int          enableDepthDB;
  int          enableDepthSBst;
  int          enableDepthDBst;
  int          enableDepthDBal;
  int          enableDepthDBstAl;

  /* global viewing flags */
  int cnear;       /* clipping planes.                        */
  int cfar;
  int fovy;        /* field of view angle for perspective.    */
  int dblBuf;      /* use doublebuffer widget if true         */
  int dbPossible;  /* Flag that double buffer is possible     */
  int winx, winy;  /* current drawing window size.            */
  int lastmx, lastmy;    /* last x,y mouse location.                */
  int lightx,
    lighty;

  int mousemove;   /* 0 = move model, 1 light, 2 clip.        */
  int stereo;      /* 0 = no stereo view.                     */
                   /* 1 = cross, 2 = wall, 3 = red/green      */
                   /* 4 = display hardware stereo.            */
  int alphaVisual; /* Flag that the current visual has alpha  */
  int transBkgd;   /* Flag that alpha visual is being used for transparent background */
  int clearAfterStereo;   /* Flag to clear right buffer after leaving stereo */
  float plax;      /* parallax for stereo separation.         */
  int imageStereo;    /* draw images as stereo pairs             */
  int imagesPerArea;  /* Number of images of same item         */
  int imageDeltaZ;    /* Step in Z between stereo pairs        */
  float deltaRot;     /* General rotation step */
  int movie;          /* allow movies.                           */
  float xrotMovie;    /* Movie rotations in X, Y, Z             */
  float yrotMovie;
  float zrotMovie;
  int drawall;        /* draw all models at once.                */
  int alpha;           /* number of alpha planes.                 */
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
  int vertBufOK;   /* Flag for whether vertex buffers are possible and enabled */
  int primRestartOK;  /* Flag for primitive restarts being possible (GL 3.1) */
  int glExtFlags;    /* Flags for GL extensions possible */
  VertBufManager *vbManager;  /* Class for managingthe VBO's */

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
  int invertZ;
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


#endif /* imodv.h */

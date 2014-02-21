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
  struct ViewInfo *cvi;  // current view     

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
  int arrow[4];
  
  short wzoom;
  int closing;      // Flag that windows are being closed on exit
  int exiting;      // Flag that application exit has been called
  int listening;    // Flag that it was started with -L or -W
  int glInitialized;  // Flag that OpenGL version was gotten, and extensions for windows
  int chooserPlugin;  // Flag that a file chooser plugin exists and should be used
}ImodApp;

extern ImodApp *App;

typedef struct imod_control_list ImodControlList;
typedef struct imod_autox_struct Autox;
typedef struct xbldrcoloramp Cramp;
class ImodWorkproc;
class ImodAssistant;
class ImodClipboard;
class PyramidCache;

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
  unsigned char **idata;  // 8 bit 3-D grey scale data. 

  int   xsize, ysize, zsize;      // Size of idata 
  size_t xysize;                   // section size. 
  int  fullXsize, fullYsize, fullZsize;  // Full size of image file(s) 
  float  xmouse, ymouse, zmouse;   // Current point in idata. 
  int   xUnbinSize, yUnbinSize, zUnbinSize;  // Original size of data 

  int   numTimes, curTime; // number of time frames, current time.       

  IloadInfo *li; 
  ImodImageFile   *image;
  ImodImageFile   *imageList;
  ImodImageFile   *hdr;

  int      modelViewVi;       // Flag that this is a viewinfo from model view 
  int      vmSize;            // virtual memory z-section size. 
  ivwSlice *vmCache;          // the cache of z-section data.   
  int      vmCount;           // Use counter for cache 
  int      *cacheIndex;       // Cross-index from sections to cache entries 
  int      vmTdim;            // Time dimension of cross-index 
  int      vmTbase;           // Base T value for lookups - 0 or 1 
  int      fullCacheFlipped;  // Flag that cache was full upon flipping 
  int      keepCacheFull;     // Cache was not specified by user, fill it 
  int      stripOrTileCache;  // Flag to organize cache as tiles or at least strips
  int      loadingImage;      // Flag that data is being loaded 
  int      doingInitialLoad;  // and that the load is the initial one 
  int      xybin;             // Binning in X and Y dimensions 
  int      zbin;              // Binning in Z 
  int      imagePyramid;      // Number of files in an image pyramid
  PyramidCache *pyrCache;     // Class for handling pyramid and tile cacheed data

  /* Image data scaling for gray scale images. */
  int    rampbase;
  int    rampsize;
  int    black;               // True values used for scaling (0-255 OR 0-65535
  int    white;
  int    rangeLow;            // If ints loaded, low and high range sliders (0-65535) 
  int    rangeHigh;
  int    whiteInRange;        // The black/white slider values if ints loaded, 0-255 
  int    blackInRange;

  /* motion control */
  int movierate;
  int xmovie, ymovie, zmovie, tmovie;
  unsigned int movieInterval;  
  int movieRunning;
  ImodWorkproc *timers;       // Class with QTimers 

  /* XYZ slice points. */
  struct imod_showslice_struct slice;
  struct imod_showslice_struct lslice;

  /* Grey Scale Ramp Data. */
  Cramp *cramp;

  /* THE MODEL and extra objects and the selection list */
  Imod  *imod;
  Iobj  *extraObj;        // The general extra object array 
  int   numExtraObj;      // Number of extra objects allocated 
  int   *extraObjInUse;   // Flags for whether objects are in use 
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
  UndoRedo *undo;             // Undo -redo class 

  /* Some Flags. */
  int    dim;             // bits 0..4, x, y, z, t 
  int    obj_moveto;      // default object to move contour to. 
  int    ghostmode;
  int    ghostlast;       // last value of mode, when toggled by g 
  int    ghostdist;       // Maximum distance for ghosts 
  int    insertmode;      // insert points before/after current point. 
  int    fastdraw;    
  int    drawcursor;
  int    ifd;
  int    overlaySec;      // Section to show in overlay color 
  int    overlayRamp;     // Color ramp when first started overlay mode 
  int    whichGreen;      // Whether main section (0) or other (1) is green 
  int    reverseOverlay;  // Toggle reverse contrast when going in or out 
  int    drawStipple;     // Flag to draw contours with stipple if flag set 
  int    trackMouseForPlugs; // Number of plugins that want mouse tracking 
     
  int      flippable;     // Flag that images can be y-z flipped 
  short    fakeImage;     // No real image data. 
  short    rawImageStore; // the MRC_MODE in which the raw image is stored. 
                          // if not 0, data will be cached.
                          // 0  = unsigned bytes.
                          // 6  = unsigned shorts.
                          // 16 = color rgb unsigned byte triplets.

  int     ushortStore;    // Convenience flags for these modes 
  int     rgbStore;
  int     colormapImage;  // Flag that byte images with colormaps are loaded 
  int     grayRGBs;       // Flag to load MRC RGBs as gray scale 
  int     multiFileZ;     // Flag that multiple single-image files are sections
                          // in Z (if > 0) or to be treated as times (< 0)
  int     reloadable;     // Model file exists and can be reloaded 
  int     noReadableImage; // Flag that image file is not readable (stdin) 
  int     equalScaling;    // Flag to set smin/smax equal to biggest range needed 

  FILE   *fp;                // current image file pointer.    

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

extern "C" int iiQImageCheck(ImodImageFile *inFile);

/*****************************************************************************/

#define IMOD_SELSIZE 15   /* Distance for selecting model points with mouse. */

#define LATIN1(a) ((const char *)a.toLatin1())

// Some favorite macros from SerialEM (note no ; on the member ones, and lower case)
#define setMember(a,b) void set##b(a inVal) {m##b = inVal;}
#define getMember(a,b) a get##b() {return m##b;}
#define getSetMember(a,b) getMember(a,b);       \
  setMember(a,b)
#define CLEAR_RESIZE(a,b,c) { a.clear(); \
  a.swap(std::vector<b>(a)); \
  a.resize(c); }

#define RADIANS_PER_DEGREE 0.017453293
#define MOVIE_DEFAULT 52965
#define HUGE_CACHE 2000000000

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
#define IMOD_ARROW      (IMOD_MAX_INDEX - 10)
#define IMOD_ARROW2     (IMOD_MAX_INDEX - 11)
#define IMOD_ARROW3     (IMOD_MAX_INDEX - 12)
#define IMOD_ARROW4     (IMOD_MAX_INDEX - 13)
#define RAMPMAX         (IMOD_MAX_INDEX - 14)
#define RAMPMIN         101
#define IMOD_MIN_INDEX  16


/****************************************************************************/
/* Private functions for internal imod use.                                 */

int imodLoopStarted();
void imod_quit(void);
bool imodDebug(char key);
void wprintWidget(QTextEdit *edit);
void wprintWriteFile(void);
void wprintPaste(void);
void wprintCopy(void);

#endif     

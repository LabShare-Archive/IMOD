#ifndef MIDAS_H
#define MIDAS_H
/*
 *  midas.h -- Header file for midas.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

class MidasWindow;
class MidasSlots;
class MidasGL;
class QDoubleSpinBox;
class ArrowButton;
#include <QKeyEvent>
#include <QCloseEvent>
#include <qlabel.h>
#include <qstring.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qslider.h>
#include <qcheckbox.h>
#include <qbuttongroup.h>
#include <qgroupbox.h>
#include <qmainwindow.h>
#include <qapplication.h>
#include <qsignalmapper.h>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <qspinbox.h>

#include "imodconfig.h"
#include "iimage.h"
#include "b3dutil.h"
#include "warpfiles.h"
#include <qgl.h>
#include "slots.h"
#include "graphics.h"


/* Midas 0.9a renamed to manali, 2.40 renamed to midas */
#define MIDAS_VERSION_STRING VERSION_NAME
#define MIDAS_VERSION VERSION

#define MIDAS_VIEW_SINGLE 0
#define MIDAS_VIEW_COLOR  1
#define MIDAS_VIEW_MULTI  2

#define RADIANS_PER_DEGREE 0.0174532925

/* transformation types */
#define XTYPE_XO   0  /* Use default setting == XTYPE_XF  */
#define XTYPE_XF   1  /* section-to-section origin.       */
#define XTYPE_XG   2  /* global origin at center of file. */
#define XTYPE_XREF 3  /* origin at reference section.     */
#define XTYPE_MONT 4  /* displacements between montage pieces */

/* Image slice return types for image cache. */
#define MIDAS_SLICE_CURRENT   1
#define MIDAS_SLICE_OCURRENT  11
#define MIDAS_SLICE_PREVIOUS  2
#define MIDAS_SLICE_OPREVIOUS 12
#define MIDAS_SLICE_REFERENCE 4

#define MAX_CACHE_MBYTES   1024
#define MAX_ZOOMIND        13
#define MAX_INCREMENTS      6
#define MAX_TOP_ERR        10

#define LATIN1(a) ((const char *)a.toLatin1())

enum MenuIDs {
  FILE_MENU_LOAD, FILE_MENU_SAVE, FILE_MENU_SAVE_AS, FILE_MENU_SAVE_IMAGE,
  FILE_MENU_TRANSFORM, FILE_MENU_QUIT, EDIT_MENU_STORE, EDIT_MENU_RESET,
  EDIT_MENU_REVERT, EDIT_MENU_MIRROR, EDIT_MENU_DELETEPT, HELP_MENU_ABOUT,
  HELP_MENU_CONTROLS, HELP_MENU_HOTKEYS, HELP_MENU_MOUSE, HELP_MENU_MANPAGE, LAST_MENU_ID
};

class MidasWindow : public QMainWindow
{
  Q_OBJECT

public:
  MidasWindow(bool doubleBuffer, QWidget * parent = 0,
              Qt::WFlags f = Qt::Window);
  ~MidasWindow();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

public slots:

 private:
 void makeSeparator(QVBoxLayout *parent, int width);
 void makeTwoArrows(QHBoxLayout *parent, int direction, int signal,
                    QSignalMapper *mapper, bool repeat, const char *tip1 = NULL,
                    const char *tip2 = NULL, ArrowButton **arrow1 = NULL, 
                    ArrowButton **arrow2 = NULL);
 QSignalMapper *makeLabeledArrows(QVBoxLayout *parent, QString textlabel, 
                                  QLabel **outLabel, bool repeat,
                                  const char *tip1 = NULL, const char *tip2 = NULL);
 QLabel *makeArrowRow(QVBoxLayout *parent, int direction, int signal, 
                      QSignalMapper *mapper, bool repeat, QString textlabel, 
                      int decimals, int digits, float value, const char *tip1 = NULL,
                      const char *tip2 = NULL, ArrowButton **arrow1 = NULL, 
                      ArrowButton **arrow2 = NULL, QLabel **textLabel = NULL);
 QSpinBox *makeSpinBoxRow(QHBoxLayout *row, const char *labText, int minz, int maxz);
 void createParameterDisplay(QVBoxLayout *parent);
 void createSectionControls(QVBoxLayout *parent);
 void createZoomBlock(QVBoxLayout *parent);
 void createViewToggle(QVBoxLayout *parent);
 void createContrastControls(QVBoxLayout *parent);


};

struct Midas_transform
{
  int black;    /* contrast settings */
  int white;
  float mat[9]; /* transformation matrix */
};

struct Midas_cache
{
  int zval;     /* Section number */
  int xformed;  /* transformed or not */
  int used;     /* counter when last used */
  float mat[9]; /* Transformation matrix */
  int nControl;   /* # of warp points if warped */
  int nxGrid, nyGrid;   /* Warp grid characteristics */
  float xStart, yStart;
  float xInterval, yInterval;
  float meanSDs[8];  /* Mean and Sd of control point arrays */
  Islice *sec;  /* pointer to data */
};

struct Midas_chunk
{
  int size;       /* Number in chunk */
  int start;      /* Starting Z of chunk */
  int curSec;     /* Section to view when chunk is current */
  int refSec;     /* Section to view when chunk is previous */
  int maxCurSec;  /* Maximum section to see when chunk is current */
  int minRefSec;  /* Minimum section to see when chunk is previous */
};

typedef struct Midas_view
{
  /* Size of input data */
  int zsize; 
  int xsize; 
  int ysize;
  int xysize;
  int binning; /* Binning applied to data */
  int cz;    /* current section */
  int refz;  /* reference section */
  float xcenter;  /* center coordinates for rotation, stretch, mag */
  float ycenter;
  float xfixed;   /* Coordinates of a second fixed point */
  float yfixed;
  int useFixed;  /* Flag to use the fixed point for stretching */
  int curChunk;  /* Current chunk */

  /* input option values */
  float sminin;  /* minimum scale */
  float smaxin;  /* maximum scale */
  int cachein;   /* cache size */
  int rotMode;   /* Flag for rotation mode */
  double globalRot; /* Global rotation value */
  int cosStretch;   /* Flag for cosine stretch */
  float tiltOffset; /* Offset to apply to tilt angles */
  int numChunks;    /* Number of chunks for chunk mode */
  int quiet;     /* Flag to avoid nag message when fixing edges */
  int imageForChannel[3];  /* Which image to send to each channel */
  Midas_chunk *chunk;
  float *tiltAngles;   /* Array for tilt angles */

  struct LoadInfo *li;
  MrcHeader *hin;

  /* cache data */
  int usecount;  /* use counter */
  int cachesize; /* size */
  Midas_cache *cache;
  unsigned char *unbinnedBuf;
	
  /* transformation data array */
  Midas_transform *tr;

  Islice *ref;  /* reference data */
  int showref;  /* flag to display reference sec */


  int sangle;       /* stretch angle for applied stretches */
  float phi;        /* actual stretch angle */

  /* data used for viewing */
  int sdatSize;
  b3dUInt32 *sdat; /* data written into display-sized buffer */
  b3dUInt32 *id;   /* image data, full size.  */

  /* viewing factors */
  float zoom;    /* Current zoom, can be negative for fractions */
  float truezoom; /* Actual zoom, can be fractional */
  int zoomind;   /* current zoom index */
  int xtrans;    /* translation of image in window */
  int ytrans;
  int xoffset;   /* offset to get from image to display coordinates */
  int yoffset;

  /* view mode: single or overlay */
  int vmode;

  int fastInterp;  /* flag for fast display, nearest neighbor interpolation */

  /* used for mouse translation */
  int lastmx;
  int lastmy;
  int firstmx;
  int firstmy;
  int mx;
  int my;
  int mousemoving;

  /* current drawing area window size */
  int width;	
  int height;

  int xtype;   /* transform type, section-to-section, global or ref. */
  char *xname; /* name of file containing transforms. */
  char *oname;  /* Name of output file after reading in from xname */
  char *refname; /* name of file containing a reference image. */
  int refzsize; /* z size of reference file */
  char *tiltname; /* Name of tilt angle file */
  int changed;  /* flag that transforms have changed */
  int didsave;  /* flag that file has been saved at least once */
  char *plname; /* name of piece list file */
     
  int xsec;    /* The section # of the reference image. */
  int corrBoxSize;  /* Size of box for correlation */
  int corrShiftLimit;  /* Shift limit for correlation */

  int *xpclist; /* Piece coordinates */
  int *ypclist;
  int *zpclist;
  int minxpiece, minypiece;   /* Minimum piece coordinate */
  int minzpiece, maxzpiece;   /* Actual limits of Z values */
  int nxpieces, nypieces;     /* Number of pieces in X and Y */
  int nxoverlap, nyoverlap;   /* Overlap between pieces */
  float *edgedx;    /* Edge displacements in X and Y */
  float *edgedy;
  int *skippedEdge;   /* Flag if edge was skipped in blendmont or here */
  int anySkipped;      /* Flag if any such edges exist */
  int robustFit;     /* Flag to do robust fitting of shifts */
  float robustCrit;  /* Criterion for outlier rejection */
  int *pathList;     /* List of pieces to look at when looking for path */
  unsigned char *leaveType;  /* Type 1 or 2 for pieces around left-out edge */
  int *montmap;   /* Map of piece numbers in 3-D array of positions */
  int *edgelower; /* indexes of edges below and above pieces */
  int *edgeupper;
  int *piecelower; /* Piece numbers below and above edges */
  int *pieceupper;
  int nedge[2];    /* total # of edges in X or Y */
  int maxedge[2];  /* Maximum # of edges in X or Y on a section */
  int xory;        /* Doing X or Y edge */
  int centerXory;  /* Whether center was set for X or Y edge */
  int montcz;      /* Current Z value ; cz refers to piece number */
  int curedge;     /* Current edge number in the section */
  int edgeind;     /* index into arrays of current edge */
  int *fbs_work;   /* Arrays needed by find_best_shifts */
  float *fbs_b;
  int *fbs_indvar;
  int *fbs_ivarpc;
  int excludeSkipped;  /* Flag to exclude the skipped edges */
  float curleavex, curleavey;  /* Leave-out error of current edge */
  int topind[MAX_TOP_ERR];   /* Index of edges with top errors */
  int numTopErr;    /* Number of top errors to display */
  int skipErr;      /* Flag to skip error computation and display */
		       
  int      depth;

  int      exposed; /* Flag, true if graphics have been inited. */
  QSlider  *wBlacklevel;  /* sliders and numeric labels */
  QSlider  *wWhitelevel;
  QLabel   *wBlackval;
  QLabel   *wWhiteval;
  int      blackstate;   /* current black and white slider values */
  int      whitestate;
  QCheckBox *reversetoggle;
  int      reversemap;   /* flag to reverse contrast */
  int      applytoone;   /* Flag to apply to only one section */
  int      drawCorrBox;  /* Flag to draw correlation box, indicating color */
  QSpinBox *curSpin;
  QSpinBox *refSpin;
  QSpinBox *chunkSpin;
  QCheckBox *difftoggle;
  int      keepsecdiff;  /* flag to keep Curr-Ref constant */
  QCheckBox *warpToggle;
  bool     editWarps;     /* Flag for editing warp points */
  bool     warpingOK;     /* Flag that warping is allowed */
  bool     drawVectors;   /* Flag to draw vectors */
  int      curControl;   /* Current control point */
  int      curWarpFile;  /* Index of current warping file */
  int      warpNz;       /* nz of warp file */
  float    warpScale;    /* Product of pixel size ratio and binning */
  int      maxWarpBackup;  /* Array size and number of backup elements */
  int      numWarpBackup;
  float    *backupXcontrol;  /* Backup values of warping */
  float    *backupYcontrol;
  float    *backupXvector;
  float    *backupYvector;
  float    *gridDx;          /* Grid for warping */
  float    *gridDy;
  int      gridSize;
  float    oldMat[9];       /* Transform before changing, needed to adjust control pts */
  float    *lastGridDx;     /* Grid of last transform */
  float    *lastGridDy;
  int      lastGridSize;
  int      lastWarpedZ;     /* Z at which grid was used */
  int      lastNxGrid;      /* All characteristics of grid to make sure they match */
  int      lastNyGrid;
  float    lastXstart;
  float    lastYstart;
  float    lastXinterv;
  float    lastYinterv;
  float    lastMat[9];       /* And last transformation for good measure */
  QButtonGroup *edgeGroup;
  QRadioButton *wXedge;
  QRadioButton *wYedge;
  
  QSpinBox *edgeSpin;
  QSpinBox *lowerXspin;
  QSpinBox *lowerYspin;
  QCheckBox *wSkipExcluded;
  QCheckBox *wExcludeEdge;
  QLabel   *zoomlabel;
  QCheckBox *overlaytoggle;
  QLabel   *wIncrement[3];
  int      incindex[3];    /* index from parameters to increments */
  float    increment[3];   /* Current increments */
  QLabel   *wParameter[5];
  QLabel   *wLinearTrans;
  ArrowButton *arrowsToGray[10];
  QLabel   *labelsToGray[7];
  QPushButton *wToperr[MAX_TOP_ERR];
  QLabel   *wMeanerr;
  QLabel   *wCurerr;
  QLabel   *wLeaverr;
  QPushButton *wApplyLeave;
  QCheckBox *wSkipErr;
  QLabel   *wSelectLabel;
  QPushButton *wSelectBiggest;
  ArrowButton *wSelectLess;
  ArrowButton *wSelectMore;
  QCheckBox *wDrawVectors;
  float    paramstate[5];  /* Current displayed values of parameters */
  float    backup_mat[9];  /* backup values for current section */
  float    backup_edgedx, backup_edgedy;
  QSlider  *anglescale;
  QLabel   *anglelabel;
  QSpinBox *corrBoxSpin;
  QSpinBox *corrLimitSpin;
  QDoubleSpinBox *robustSpin;
  QLabel   *mouseLabel[3];
  QDoubleSpinBox *globRotSpin;
  QDoubleSpinBox *tiltOffSpin;
  int      mouseXonly;    /* Flag to constrain mouse to X moves only */
  int      ctrlPressed;
  int      shiftPressed;
  MidasWindow *midasWindow;
  MidasSlots *midasSlots;
  MidasGL    *midasGL;
} MidasView;

/* global variables, just the two */
extern MidasView *VW;
extern int Midas_debug;

/****************************************************************************/
/* midas.cpp function prototypes.                                           */
void midas_error(const char *tmsg, const char *bmsg, int retval);
void printStderr(const char *format, ...);

/****************************************************************************/

/****************************************************************************/
 /* file_io.cpp function prototypes                                          */

int load_image(MidasView *vw, char *filename);
int load_refimage(MidasView *vw, char *filename);
int save_view(MidasView *vw, char *filename);
int write_transforms(MidasView *vw, char *filename);
int load_transforms(MidasView *vw, char *filename);
void load_angles(MidasView *vw);
int midasReadZByte(MidasView *vw, MRCheader *hin, LoadInfo *li, 
                   unsigned char *data, int sec);


/****************************************************************************/
/* transforms.cpp function prototypes                                       */

Islice *getRawSlice(MidasView *vw, int zval);
Islice *midasGetSlice(MidasView *vw, int sliceType);
unsigned char *midasGetPrevImage(MidasView *vw);
void flush_xformed(MidasView *vw);
void midasGetSize(MidasView *vw, int *xs, int *ys);
int fillWarpingGrid(int iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart,
                   float *xInterval, float *yInterval);
     
int new_view(MidasView *vw);
int load_view(MidasView *vw, char *fname);
int translate_slice(MidasView *vw, int xt, int yt);
int global_rot_transform(MidasView *vw, Islice *slin, Islice *slout, 
                          int zval);
int midas_transform(int zval, Islice *slin, Islice *sout, float *trmat, int izwarp);
float *tramat_create(void);
void tramat_free(float *mat);
int tramat_idmat(float *mat);
int tramat_copy(float *fmat, float *tomat);
int tramat_multiply(float *m1, float *m2, float *out);
int tramat_translate(float *mat, double x, double y);
int tramat_scale(float *mat, double x, double y);
int tramat_rot(float *mat, double angle);
float *tramat_inverse(float *mat);
void rotate_transform(float *mat, double angle);
void rotate_all_transforms(MidasView *vw, double angle);
void stretch_transform(MidasView *vw, float *mat, int index, 
                       int destretch);
void stretch_all_transforms(MidasView *vw, int destretch);
void transform_model(const char *infname, const char *outfname, MidasView *vw);
void reduceControlPoints(MidasView *vw);
void adjustControlPoints(MidasView *vw);
int includedEdge(int mapind, int xory);
int nearest_edge(MidasView *vw, int z, int xory, int edgeno, 
		 int direction, int *edgeind);
int nearest_section(MidasView *vw, int sect, int direction);
void set_mont_pieces(MidasView *vw);
void find_best_shifts(MidasView *vw, int leaveout, int ntoperr,
		      float *meanerr, float *amax, int *indmax,
		      float *curerrx, float *curerry, int localonly);
void find_local_errors(MidasView *vw, int leaveout, int ntoperr,
		       float *meanerr, float *amax, int *indmax,
		       float *curerrx, float *curerry, int localonly);
void crossCorrelate(MidasView *vw);

#endif  // MIDAS_H

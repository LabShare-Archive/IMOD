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
 *  Log at end of file
 */

class MidasWindow;
class MidasSlots;
class MidasGL;
class QDoubleSpinBox;
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
#include "mrcc.h"
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

#define INITIAL_BOX_SIZE   -1
#define MAX_CACHE_MBYTES   128
#define MAX_ZOOMIND        13
#define MAX_INCREMENTS      6
#define MAX_TOP_ERR        10

#define LATIN1(a) ((const char *)a.toLatin1())

enum MenuIDs {
  FILE_MENU_LOAD,
  FILE_MENU_SAVE,
  FILE_MENU_SAVE_AS,
  FILE_MENU_SAVE_IMAGE,
  FILE_MENU_TRANSFORM,
  FILE_MENU_QUIT,
  EDIT_MENU_STORE,
  EDIT_MENU_RESET,
  EDIT_MENU_REVERT,
  EDIT_MENU_MIRROR,
  HELP_MENU_ABOUT,
  HELP_MENU_CONTROLS,
  HELP_MENU_HOTKEYS,
  HELP_MENU_MOUSE,
  HELP_MENU_MANPAGE,
  LAST_MENU_ID
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
                    QSignalMapper *mapper, bool repeat, char *tip1 = NULL,
                    char *tip2 = NULL);
 QSignalMapper *makeLabeledArrows(QVBoxLayout *parent, QString textlabel, 
                                  QLabel **outLabel, bool repeat,
                                  char *tip1 = NULL, char *tip2 = NULL);
 QLabel *makeArrowRow(QVBoxLayout *parent, int direction, int signal, 
                      QSignalMapper *mapper, bool repeat, QString textlabel, 
                      int decimals, int digits, float value, char *tip1 = NULL,
                      char *tip2 = NULL);
 QSpinBox *makeSpinBoxRow(QHBoxLayout *row, char *labText, int minz, int maxz);
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
  Islice *sec;  /* pointer to data */
};

struct Midas_chunk
{
  int size;    /* Number in chunk */
  int start;   /* Starting Z of chunk */
  int curSec;  /* Section to view when chunk is current */
  int refSec;  /* Section to view when chunk is previous */
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
  struct Midas_chunk *chunk;
  float *tiltAngles;   /* Array for tilt angles */

  struct LoadInfo *li;
  struct MRCheader *hin;

  /* cache data */
  int usecount;  /* use counter */
  int cachesize; /* size */
  struct Midas_cache *cache;
  unsigned char *unbinnedBuf;
	
  /* transformation data array */
  struct Midas_transform *tr;

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

  int fastip;  /* flag for fast display, not interpolation */

  /* used for mouse translation */
  int lastmx;
  int lastmy;
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
  int *skippedEdge;   /* Flag if edge was skipped in blendmont */
  int *pathList;     /* List of pieces to look at when looking for path */
  unsigned char *leaveType;  /* Type 1 or 2 for pieces around left-out edge */
  int anySkipped;      /* Flags if any such edges exist */
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
  QButtonGroup *edgeGroup;
  QRadioButton *wXedge;
  QRadioButton *wYedge;
  
  QSpinBox *edgeSpin;
  QSpinBox *lowerXspin;
  QSpinBox *lowerYspin;
  QCheckBox *wSkipExcluded;
  QLabel   *zoomlabel;
  QLabel   *blocklabel;
  int      boxsize;      /* block size for transforms */
  QCheckBox *overlaytoggle;
  QLabel   *wIncrement[3];
  int      incindex[3];    /* index from parameters to increments */
  float    increment[3];   /* Current increments */
  QLabel   *wParameter[5];
  QPushButton *wToperr[MAX_TOP_ERR];
  QLabel   *wMeanerr;
  QLabel   *wCurerr;
  QLabel   *wLeaverr;
  QPushButton *wApplyLeave;
  QCheckBox *wSkipErr;
  float    paramstate[5];  /* Current displayed values of parameters */
  float    backup_mat[9];  /* backup values for current section */
  float    backup_edgedx, backup_edgedy;
  QSlider  *anglescale;
  QLabel   *anglelabel;
  QSpinBox *corrBoxSpin;
  QSpinBox *corrLimitSpin;
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
     
int new_view(MidasView *vw);
int load_view(MidasView *vw, char *fname);
int translate_slice(MidasView *vw, int xt, int yt);
int global_rot_transform(MidasView *vw, Islice *slin, Islice *slout, 
                          int zval);
int midas_transform(Islice *slin, Islice *sout, float *trmat);
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

/*

$Log$
Revision 3.17  2010/06/06 21:14:12  mast
Remove some declarations (like gaussj, amat_to_rotmagstr)

Revision 3.16  2009/01/15 16:30:19  mast
Qt 4 port

Revision 3.15  2008/10/13 04:36:23  mast
Added cosine stretching

Revision 3.14  2007/10/03 21:36:10  mast
Added ImodAssistant help object

Revision 3.13  2006/07/08 15:32:13  mast
Changes to implement second fixed point for stretching

Revision 3.12  2006/05/20 16:07:56  mast
Changes to allow mirroring around X axis

Revision 3.11  2006/05/13 22:52:52  mast
Changes to allow overlay colors to be specified

Revision 3.10  2005/03/10 21:04:14  mast
Added -q option for use from etomo

Revision 3.9  2004/11/05 18:53:22  mast
Include local files with quotes, not brackets

Revision 3.8  2004/10/25 18:51:52  mast
Added optoin to output to different file from input file

Revision 3.7  2004/08/04 22:35:13  mast
Changed unsigned long to b3dUInt32 for 64-bit use

Revision 3.6  2004/07/12 18:42:31  mast
Changes for chunk alignment and for switching to spin boxes

Revision 3.5  2003/12/17 21:43:59  mast
Changes to implement global rotations

Revision 3.4  2003/05/26 01:03:08  mast
Added label for mouse action

Revision 3.3  2003/02/10 20:49:57  mast
Merge Qt source

Revision 3.2.2.2  2003/01/26 23:20:33  mast
using new library

Revision 3.2.2.1  2002/12/05 03:13:47  mast
New Qt version

Revision 3.2  2002/08/19 04:44:54  mast
Added a flag that mouse is moving, to prevent repeated error updates in
montage-fixing mode when there are many pieces.

Revision 3.1  2002/07/18 20:21:12  rickg
Changed include of GLwMDrawA to rely upon -I compiler option

*/

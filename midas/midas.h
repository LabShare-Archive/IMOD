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
 */

/*  $Author$

$Date$

$Revision$

Log at end of file
*/

class MidasWindow;
class MidasSlots;
class MidasGL;
class FloatSpinBox;

#include <qlabel.h>
#include <qstring.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qslider.h>
#include <qcheckbox.h>
#include <qbuttongroup.h>
#include <qmainwindow.h>
#include <qapplication.h>
#include <qsignalmapper.h>
#include <qhbox.h>
#include <qvbox.h>
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
  HELP_MENU_ABOUT,
  HELP_MENU_CONTROLS,
  HELP_MENU_HOTKEYS,
  HELP_MENU_MOUSE
};

class MidasWindow : public QMainWindow
{
  Q_OBJECT

public:
  MidasWindow(bool doubleBuffer, QWidget * parent = 0, const char * name = 0, 
	      WFlags f = WType_TopLevel) ;
  ~MidasWindow();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

public slots:

 private:
 void makeSeparator(QVBox *parent, int width);
 void makeTwoArrows(QHBox *parent, int direction, int signal,
                    QSignalMapper *mapper, bool repeat);
 QSignalMapper *makeLabeledArrows(QVBox *parent, QString textlabel, 
				  QLabel **outLabel, bool repeat);
 QLabel *makeArrowRow(QVBox *parent, int direction, int signal, 
		      QSignalMapper *mapper, bool repeat, QString textlabel, 
		      int decimals, int digits, float value);
 QSpinBox *makeSpinBoxRow(QHBox *row, char *labText, int minz, int maxz);
 void createParameterDisplay(QVBox *parent);
 void createSectionControls(QVBox *parent);
 void createZoomBlock(QVBox *parent);
 void createViewToggle(QVBox *parent);
 void createContrastControls(QVBox *parent);


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

struct Midas_view
{
  /* Size of input data */
  int zsize; 
  int xsize; 
  int ysize;
  int xysize;
  int cz;    /* current section */
  int refz;  /* reference section */
  float xcenter;  /* center coordinates for rotation, stretch, mag */
  float ycenter;
  int curChunk;  /* Current chunk */

  /* input option values */
  float sminin;  /* minimum scale */
  float smaxin;  /* maximum scale */
  int cachein;   /* cache size */
  int rotMode;   /* Flag for rotation mode */
  double globalRot; /* Global rotation value */
  int numChunks;    /* Number of chunks for chunk mode */
  int quiet;     /* Flag to avoid nag message when fixing edges */
  struct Midas_chunk *chunk;

  struct LoadInfo *li;
  struct MRCheader *hin;

  /* cache data */
  int usecount;  /* use counter */
  int cachesize; /* size */
  struct Midas_cache *cache;
	
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
  int changed;  /* flag that transforms have changed */
  int didsave;  /* flag that file has been saved at least once */
  char *plname; /* name of piece list file */
     
  int xsec;    /* The section # of the reference image. */

  int *xpclist; /* Piece coordinates */
  int *ypclist;
  int *zpclist;
  int minxpiece, minypiece;   /* Minimum piece coordinate */
  int minzpiece, maxzpiece;   /* Actual limits of Z values */
  int nxpieces, nypieces;     /* Number of pieces in X and Y */
  int nxoverlap, nyoverlap;   /* Overlap between pieces */
  float *edgedx;    /* Edge displacements in X and Y */
  float *edgedy;
  int *montmap;   /* Map of piece numbers in 3-D array of positions */
  int *edgelower; /* indexes of edges below and above pieces */
  int *edgeupper;
  int *piecelower; /* Piece numbers below and above edges */
  int *pieceupper;
  int nedge[2];    /* total # of edges in X or Y */
  int maxedge[2];  /* Maximum # of edges in X or Y on a section */
  int xory;        /* Doing X or Y edge */
  int montcz;      /* Current Z value ; cz refers to piece number */
  int curedge;     /* Current edge number in the section */
  int edgeind;     /* index into arrays of current edge */
  float *fbs_a;    /* Arrays needed by find_best_shifts */
  float *fbs_b;
  int *fbs_indvar;
  int *fbs_ivarpc;
  float curleavex, curleavey;  /* Leave-out error of current edge */
  int topind[8];   /* Index of edges with top errors */
		       
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
  QSpinBox *curSpin;
  QSpinBox *refSpin;
  QSpinBox *chunkSpin;
  QCheckBox *difftoggle;
  int      keepsecdiff;  /* flag to keep Curr-Ref constant */
  QButtonGroup *edgeGroup;
  QRadioButton *wXedge;
  QRadioButton *wYedge;
  
  QSpinBox *edgeSpin;
  QLabel   *zoomlabel;
  QLabel   *blocklabel;
  int      boxsize;      /* block size for transforms */
  QCheckBox *overlaytoggle;
  QLabel   *wIncrement[3];
  int      incindex[3];    /* index from parameters to increments */
  float    increment[3];   /* Current increments */
  QLabel   *wParameter[5];
  QPushButton *wToperr[4];
  QLabel   *wMeanerr;
  QLabel   *wCurerr;
  QLabel   *wLeaverr;
  float    paramstate[5];  /* Current displayed values of parameters */
  float    backup_mat[9];  /* backup values for current section */
  float    backup_edgedx, backup_edgedy;
  QSlider  *anglescale;
  QLabel   *anglelabel;
  QLabel   *mouseLabel;
  FloatSpinBox *globRotSpin;
  int      mouseXonly;    /* Flag to constrain mouse to X moves only */
  int      ctrlPressed;
  int      shiftPressed;
  MidasWindow *midasWindow;
  MidasSlots *midasSlots;
  MidasGL    *midasGL;
};

/* global variables, just the two */
extern struct Midas_view *VW;
extern int Midas_debug;

/****************************************************************************/
/* midas.cpp function prototypes.                                           */
void midas_error(char *tmsg, char *bmsg, int retval);

/****************************************************************************/

/****************************************************************************/
/* file_io.cpp function prototypes                                          */

int load_image(struct Midas_view *vw, char *filename);
int load_refimage(struct Midas_view *vw, char *filename);
int save_view(struct Midas_view *vw, char *filename);
int write_transforms(struct Midas_view *vw, char *filename);
int load_transforms(struct Midas_view *vw, char *filename);


/****************************************************************************/
/* transforms.cpp function prototypes                                       */

Islice *getRawSlice(struct Midas_view *vw, int zval);
Islice *midasGetSlice(struct Midas_view *vw, int sliceType);
void flush_xformed(struct Midas_view *vw);
void midasGetSize(struct Midas_view *vw, int *xs, int *ys);
     
int new_view(struct Midas_view *vw);
int load_view(struct Midas_view *vw, char *fname);
int translate_slice(struct Midas_view *vw, int xt, int yt);
int global_rot_transform(struct Midas_view *vw, Islice *slin, Islice *slout, 
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
void rotate_all_transforms(struct Midas_view *vw, double angle);
void transform_model(char *infname, char *outfname, struct Midas_view *vw);
int nearest_edge(struct Midas_view *vw, int z, int xory, int edgeno, 
		 int direction, int *edgeind);
int nearest_section(struct Midas_view *vw, int sect, int direction);
void set_mont_pieces(struct Midas_view *vw);
void find_best_shifts(struct Midas_view *vw, int leaveout, int ntoperr,
		      float *meanerr, float *amax, int *indmax,
		      float *curerrx, float *curerry, int localonly);
void find_local_errors(struct Midas_view *vw, int leaveout, int ntoperr,
		       float *meanerr, float *amax, int *indmax,
		       float *curerrx, float *curerry, int localonly);
void amat_to_rotmagstr(float *amat, float *theta, float *smag, float *str,
		       float *phi);
#endif  // MIDAS_H

/*
$Log$
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

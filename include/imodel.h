/*
 *  imodel.h -- Image model header file.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#ifndef IMODEL_H
#define IMODEL_H
#define IMOD_MODEL_H

#include "imodconfig.h"
#include "hvemtypes.h"

#include <stdio.h>
#include <stdlib.h>
#include "ilist.h"

/* Mouse mode. */
#define IMOD_MMODEL 1   /* Mouse edits model points.  */
#define IMOD_MMOVIE 2   /* Mouse changes image views. */

/* Imodel Limits */
#define IMOD_STRSIZE 128
#define IMOD_OBJSTRSIZE 64
#define IOBJ_STRSIZE 64
#define IOBJ_EXSIZE 16
#define IMOD_CLIPSIZE  6

/* Units for pixel size. */
#define IMOD_UNIT_PIXEL 0
#define IMOD_UNIT_KILO   3
#define IMOD_UNIT_METER  1
#define IMOD_UNIT_CM    -2
#define IMOD_UNIT_MM    -3
#define IMOD_UNIT_UM    -6
#define IMOD_UNIT_NM    -9
#define IMOD_UNIT_ANGSTROM -10
#define IMOD_UNIT_PM    -12

/* Size of circles drawn on points., (Model - drawmode) */
#define IMOD_DRAWMODE_BIG   9
#define IMOD_DRAWMODE_MED   3
#define IMOD_DRAWMODE_SMALL 1

/* Used for reading and writing imod files. */
/*
  #ifdef __vms
  #define MakeID(a,b,c,d) ( (long)(d)<<24L | (long)(c)<<16L | (b)<<8 | (a))
  #else
*/
#define MakeID(a,b,c,d) ((b3dInt32)(a)<<24L | (b3dInt32)(b)<<16L | (c)<<8 | (d))


#define ID_IMOD MakeID('I', 'M', 'O', 'D')  /* imod file                     */
#define ID_OBJT MakeID('O', 'B', 'J', 'T')  /* object header                 */
#define ID_CONT MakeID('C', 'O', 'N', 'T')  /* contour data                  */
#define ID_PNTS MakeID('P', 'N', 'T', 'S')  /* Point data for v01 only */
#define ID_MESH MakeID('M', 'E', 'S', 'H')  /* Mesh data                     */
#define ID_VIEW MakeID('V', 'I', 'E', 'W')  /* 3D View info                  */
#define IMOD_01 MakeID('V', '0', '.', '1')  /* File version 01, imod 1.0     */
#define IMOD_02 MakeID('V', '0', '.', '2')  /* File version 02, imod 1.1     */
#define IMOD_V12 MakeID('V','1', '.', '2')  /* Imod file version 1.2         */
#define ID_VERSION IMOD_V12
#define ID_IEOF MakeID('I', 'E', 'O', 'F')  /* Imod end of file */

/* new data for imod version 1.2, this is an extension to V0.2 files */
#define ID_CLIP MakeID('C', 'L', 'I', 'P')  /* object clip planes. */
#define ID_MCLP MakeID('M', 'C', 'L', 'P') /* Model clip planes */
#define SIZE_CLIP 28

#define ID_IMAT MakeID('I', 'M', 'A', 'T')  /* object material. */
#define SIZE_IMAT 16

#define ID_IMNX MakeID('M', 'I', 'N', 'X')  /* image  nat. transform.*/
#define SIZE_IMNX 72

#define ID_LABL MakeID('L', 'A', 'B', 'L')   
#define ID_OLBL MakeID('O', 'L', 'B', 'L')   

#define ID_SIZE MakeID('S', 'I', 'Z', 'E')  /* size for scattered points */

/* future data defines. */
#define ID_BRCH MakeID('B', 'R', 'C', 'H')  /* Branch data */

#define IMOD_ERROR_READ    10
#define IMOD_ERROR_WRITE   11
#define IMOD_ERROR_VERSION 20
#define IMOD_ERROR_FORMAT  21
#define IMOD_ERROR_MEMORY  50

/* model flags. */
#define IMODF_FLIPYZ (1l << 16)  /* data is stored with y,z coords flipped. */
#define IMODF_TILTOK (1l << 15)  /* current tilt angles properly stored */
#define IMODF_OTRANS_ORIGIN (1l << 14)  /* otrans has image origin values */
#define IMODF_MAT1_IS_BYTES (1l << 13)  /* mat1 and mat3 are stored as bytes */
#define IMODF_MULTIPLE_CLIP (1l << 12)  /* multiple clip planes possible */


/****************************** Structures ***********************************/

typedef struct Mod_Point
{
  b3dFloat           x;
  b3dFloat           y;
  b3dFloat           z;
}Ipoint;

typedef struct {b3dFloat a, b, c, d;} Iplane;
typedef Ilist *Istore;

typedef struct Mod_Mesh
{
  struct Mod_Point *vert;   /* list of points */
  b3dInt32        *list;    /* index into vert array + instructions */
  b3dInt32        vsize;    /* size of arrays */
  b3dInt32        lsize;
  b3dUInt32       flag;     /* Tells how to draw mesh */
  b3dInt32        type;     /* Set to 0. time data.   */
  b3dInt32        pad;      /* Set to 0. surf data.   */
  Istore          store;
}Imesh;


/* Describes current object, contour and point. */
typedef struct Mod_Index
{
  b3dInt32  object;
  b3dInt32  contour;
  b3dInt32  point;
}Iindex;


/* A set of clip planes, in object, view, or object view */
typedef struct Mod_Planes
{
  b3dUByte count;        /* number of clip planes.            */
  b3dUByte flags;        /* Which clip planes are on.         */
  b3dUByte trans;        /* Transparency for clipped area     */
  b3dUByte plane;        /* Current clip plane.               */
  Ipoint normal[IMOD_CLIPSIZE];
  Ipoint point[IMOD_CLIPSIZE];
} IclipPlanes;


/* Describes a 3D view of a model. */
#define VIEW_WORLD_ON         1
#define VIEW_WORLD_LIGHT      (1l << 1)
#define VIEW_WORLD_DEPTH_CUE  (1l << 2)
#define VIEW_WORLD_WIREFRAME  (1l << 3)

#define VIEW_WORLD_STEREO   (1l << 4) /* Stereo flags */
#define VIEW_WORLD_HARDWARE (1l << 5)
#define VIEW_WORLD_UPDOWN   (1l << 6)
#define VIEW_WORLD_LOWRES  (1l << 7)

/* Shift and actual bits reserved for point quality (8, 9, 10) */
#define WORLD_QUALITY_SHIFT  8
#define WORLD_QUALITY_BITS   (7l << WORLD_QUALITY_SHIFT)

/* Properties of an object that are stored in a view */
typedef struct Mod_Object_View
{
  b3dUInt32              flags;               /* bit flags IMOD_OBJFLAG        */
  b3dFloat   red;                 /* Red (0 - 1.0)                 */
  b3dFloat   green;               /* Green (0 - 1.0)               */
  b3dFloat   blue;                /* Blue (0 - 1.0)                */
  b3dInt32     pdrawsize;           /* size to draw scattered objs   */

  b3dUByte     linewidth;           /* linewidth in 3-D              */
  b3dUByte     linesty;             /* line draw style               */
  b3dUByte     trans;               /* transperentcy                 */

  /* define clipping planes. max for each object is 7. */
  IclipPlanes clips;

  /* Added info IMAT */
  b3dUByte ambient;   /* Ambient multiplier to color */
  b3dUByte diffuse;   /* Diffuse multiplier to color */
  b3dUByte specular;  /* Specular property, added to color */
  b3dUByte shininess; /* shininess exponent */
  b3dUByte mat1;      /* Fill color red */
  b3dUByte mat1b1;    /* Fill color green */
  b3dUByte mat1b2;    /* Fill color blue */
  b3dUByte mat1b3;    /* Sphere quality */
  b3dUInt32  mat2;    /* set to 0, use as flags.  Unused */
  b3dUByte mat3;      /* Black level for showing normal magnitudes in mesh */
  b3dUByte mat3b1;    /* Black level for showing normal magnitudes in mesh */
  b3dUByte mat3b2;    /* Unused */
  b3dUByte mat3b3;    /* Unused */
}Iobjview;

typedef struct
{
  /* Set up the camera */
  b3dFloat  fovy;   /* field of view of camera, perspective in degrees.  */
  b3dFloat  rad;    /* viewing radius of sphere encloseing bounding box. */
  b3dFloat  aspect; /* aspect ratio */
  b3dFloat  cnear;  /* clip near range 0.0 to 1.0, default 0.0. */
  b3dFloat  cfar;   
     
  /* Model transformation values for model view.
   */
  Ipoint rot;
  Ipoint trans;
  Ipoint scale;

  /* World OpenGL transformation matrix. */
  b3dFloat mat[16];

  b3dInt32 world;    /* flags, true if we use world matrix. */
  b3dByte  label[32];

  b3dFloat dcstart, dcend;
  b3dFloat lightx, lighty;
  b3dFloat plax;
  IclipPlanes clips;
  Iobjview *objview;    /* properties of each object */
  b3dInt32 objvsize;       /* Number of objects properties exist for */
     
}Iview;

typedef struct
{
  Ipoint oscale;
  Ipoint otrans;
  Ipoint orot;

  Ipoint cscale;
  Ipoint ctrans;
  Ipoint crot;

}IrefImage;


/*
 *  Label Data for Contours.
 */
typedef struct
{
  b3dByte *name;
  b3dInt32  len;
  b3dInt32 index;
}IlabelItem;

typedef struct
{
  b3dByte       *name;
  b3dInt32         len;
  b3dInt32        nl;
  IlabelItem *label;
}Ilabel;

/*
 * Contours are an array of points describing an open or closed contour.
 * The points can also be unconnected or the points can be in pairs
 * describing scan lines.
 */
typedef struct Mod_Contour
{
  /* Run time data */
  Ipoint       *pts;    /* Points data.                      */
  b3dFloat     *sizes;  /* sizes for scattered points        */

  /* data below is written to file, except store is run-time */
  b3dInt32     psize;  /* Number of points.                 */
  b3dUInt32    flags;  /* Default 0 means use object flags. */
  b3dInt32     type;   /* Time index.                       */
  b3dInt32     surf;   /* Surface number.                   */
  Ilabel      *label;

  Istore       store;
}Icont;


/* An Object is an array of contours */
typedef struct Mod_Object
{
  /* Run time data */
  struct Mod_Contour *cont;              /* Contour data.                 */
  struct Mod_Mesh    *mesh;

  b3dUInt32 fgcolor;  /* colors used for colorindex rendering. */
  b3dUInt32 bgcolor;

  /* data below is written to file except store is runtime */
  b3dByte      name[IOBJ_STRSIZE];  /* Name of Object.              */
  b3dUInt32    extra[IOBJ_EXSIZE];  /* extra unused data = 0        */

  /* Use some EXSIZE data for light location. 
     b3dFloat            lightOrientation[3];
  */


  b3dInt32     contsize;            /* Number of Contours in object. */
  b3dUInt32    flags;               /* bit flags IMOD_OBJFLAG        */
  b3dInt32     axis;                /* Z = 0, X = 1, Y = 2. (future) */
  b3dInt32     drawmode;            /* Tells type of points to draw  */
  /* for scatterd objects.         */
  b3dFloat     red;                 /* Red (0 - 1.0)                 */
  b3dFloat     green;               /* Green (0 - 1.0)               */
  b3dFloat     blue;                /* Blue (0 - 1.0)                */
  b3dInt32     pdrawsize;           /* size to draw scattered objs   */

  b3dUByte     symbol;              /* default 0=circle              */
  b3dUByte     symsize;             /* size of symbol.               */
  b3dUByte     linewidth2;          /* linewidth in 2-D              */
  b3dUByte     linewidth;           /* linewidth in 3-D              */
  b3dUByte     linesty;             /* line draw style               */
  b3dUByte     symflags;
  b3dUByte     sympad;
  b3dUByte     trans;               /* transparency                  */

  b3dInt32     meshsize;            /* Number of meshes in object.   */
  b3dInt32     surfsize;            /* Max surfaces in object.       */

  /* new extended data imod 1.2 */
  /* chunk CLIP */
  /* define clipping planes. max for each object is 7. */
  IclipPlanes clips;

  /* Added info IMAT */
  b3dUByte ambient;   /* Ambient multiplier to color */
  b3dUByte diffuse;   /* Diffuse multiplier to color */
  b3dUByte specular;  /* Specular property, added to color */
  b3dUByte shininess; /* shininess exponent */
  b3dUByte mat1;      /* Fill color red */
  b3dUByte mat1b1;    /* Fill color green */
  b3dUByte mat1b2;    /* Fill color blue */
  b3dUByte mat1b3;    /* Sphere quality */
  b3dUInt32  mat2;      /* set to 0, use as flags.  Unused */
  b3dUByte mat3;      /* Black level for showing normal magnitudes in mesh */
  b3dUByte mat3b1;    /* Black level for showing normal magnitudes in mesh */
  b3dUByte mat3b2;    /* Unused */
  b3dUByte mat3b3;    /* Unused */

  Ilabel *label;      /* Labels for surfaces */
  Istore  store;
}Iobj;



/* A Model is an array of objects */
typedef struct Mod_Model
{
  /* Run time data */
  Iobj  *obj;          /* Object data.                       */
  FILE  *file;         /* Current file handle.               */
  int   lock;          /* Locks model data for shared procs. */
  int   redraw;        /* Used for shared procs.             */
  int   ctime;         /* current time index.                */

  /* data written to file except store is runtime    */
  b3dByte   name[IMOD_STRSIZE];        /* file name of model.                */
                                       /* Use labels for model name.         */
  b3dInt32    xmax, ymax, zmax;  
  b3dInt32    objsize;                 /* Number of objects in model.        */
  b3dUInt32   flags;                   /* IMODF_FLAG values                  */
  b3dInt32    drawmode;                /* 1 to draw model, -1 not to         */
  b3dInt32    mousemode;               /* Tells what mouse buttons do.       */
  b3dInt32    blacklevel, whitelevel;  /* Contrast adjustment.               */
  b3dFloat  xoffset, yoffset, zoffset; /* offsets used for 3-D display       */
  b3dFloat  xscale;                    /* pixel size zoom.                   */
  b3dFloat  yscale;                    /* y aspect for pixels.               */
  b3dFloat  zscale;                    /* z aspect for sections.             */
  Iindex cindex;              /* Current object, contour & point     */
  b3dInt32    res;            /* Number of pixels between points.    */
  b3dInt32    thresh;         /* Threshold level for auto cont.      */
  b3dFloat  pixsize;          /* size of unit 1.0 pixel              */
  b3dInt32    units;          /* units of given size.                */
  b3dInt32    csum;           /* checksum storage.                   */
  b3dInt32    cview;
  b3dInt32    viewsize;     
  b3dInt32    tmax;           /* max time */
     
  /* imod file version 0.2 data */
  b3dFloat  alpha, beta, gamma;        /* rotation of model in 3-D view. */

  /* more runtime data */
  b3dUInt16  color;      /* color of current object */
  Iview      *view;      /* Array of view data */
  int        editGlobalClip;   /* Flag that global clip is selected */

  IrefImage  *refImage;
  char       *fileName;
  int        xybin;       /* Binning in X and Y */
  int        zbin;        /* Binning in Z */

  Istore store;      /* Add support for easy addon of user configurable data */

}Imod;




/* Used for drawing models, imodv */
typedef struct Mod_Draw
{
  b3dInt32 llx;      /* Used for drawing 2d models to sub area */
  b3dInt32 lly;     
  b3dInt32 urx;
  b3dInt32 ury;
  b3dInt32 llz;
  b3dInt32 urz;
  b3dFloat xorg;     /* Offset for 2d, translation for 3d models. */
  b3dFloat yorg;
  b3dFloat zorg;
  b3dInt32 xrot;     /* Amount to rotate model */
  b3dInt32 yrot;
  b3dInt32 zrot;
  b3dFloat xtrans;   /* Amount to translate model */
  b3dFloat ytrans;
  b3dFloat ztrans;
  b3dInt32 axis;     /* Axis to rotate.         */
  b3dInt32 cmapbase; /* Where to start colormap */
  b3dFloat left,
    right,
    bottom,
    top;
  b3dInt32    cnear, /* Clipping planes */
    cfar;
  b3dInt32   dc;     /* min depth cue factor  range x/1 to x/16*/
  b3dFloat zoom;     /* Mag factor for displaying */
  b3dFloat cdist;    /* Distence for center of model to far corner */
  b3dFloat edist;    /* Distence from center of model to eye.      */
  b3dInt32 xrotm;    /* xyz movie amounts */
  b3dInt32 yrotm;
  b3dInt32 zrotm;
  b3dInt32 arot;     /* amount of change in rotation */
  b3dFloat azoom;
  b3dFloat atrans;
  b3dFloat azscale;
} Idraw;


typedef struct Mod_Transform
{
  b3dFloat x;       /* input x, y and z. */
  b3dFloat y;
  b3dFloat z;
  b3dFloat xout;    /* output values after transformation. */
  b3dFloat yout;
  b3dFloat zout;
  b3dFloat xtrans;  /* amount to translate x, y and z. */
  b3dFloat ytrans;
  b3dFloat ztrans;
  b3dFloat xrot;    /* amount to rotate x, y, and z. */
  b3dFloat yrot;
  b3dFloat zrot;
  b3dFloat xscale;  /* amount to scale x, y and z. */
  b3dFloat yscale;
  b3dFloat zscale;
}Itrans;


/* include functions in iobj.c, icont.c, imesh.c, ipoint.c */
#include "iobj.h"
#include "icont.h"
#include "imesh.h"
#include "ipoint.h"
#include "imat.h"
#include "iplane.h"
#include "iview.h"
#include "b3dutil.h"

/*****************************************************************************/
/* imodel.c functions                                                        */
/* Model structure is passed to functions.                                   */
/*****************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif

  Imod *imodNew       (void);
  void  imodDelete(Imod *imod);
  int   imodNewObject (Imod *imod);
  int   imodDeleteObject(Imod *imod, int index);
  int   imodNextObject(Imod *imod);
  int   imodPrevObject(Imod *imod);
  int   imodMoveObject(Imod *imod, int obOld, int obNew);
  int   imodDefault(Imod *imod);
  void  imodCleanSurf(Imod *imod);
  void  imodFlipYZ(Imod *imod);

  int   imodNewContour(Imod *imod);
  int   imodPrevContour(Imod *imod);
  int   imodNextContour(Imod *imod);
  int imodDeleteContour(Imod *imod, int index);
  void imodDelCurrentContour(Imod *imod);

  int   imodNewPoint(Imod *imod, Ipoint *pt);
  int   imodInsertPoint(Imod *imod, Ipoint *point, int index);
  int   imodDeletePoint(Imod *imod);
  int   imodPrevPoint(Imod *imod);
  int   imodNextPoint(Imod *imod);

  void  imodGetIndex(Imod *imod, int *object, int *contour, int *point);
  void  imodSetIndex(Imod *imod, int object, int contour, int point);
  void  imodGetBoundingBox(Imod *imod, Ipoint *min, Ipoint *max);
  int   imodGetMaxObject(Imod *imod);
  float imodGetZScale(Imod *imod);
  float imodGetPixelSize(Imod *imod);
  int   imodGetFlipped(Imod *imod);
  int   imodTransform(Imod *imod, Imat *mat);

  double  imodel_dist(Imod *imod);
  void    imodel_minpt(Imod *imod, Ipoint *pnt);
  void    imodel_maxpt(Imod *imod, Ipoint *pnt);
  int     imodGetMaxTime(Imod *imod);
  char   *imodUnits(Imod *mod);
  Ipoint *imodNearestPoint(Imod *imod, Ipoint *pt);
  int    imodChecksum(Imod *imod);
  int     imodel_lock(Imod *mod, int flag);
  int     imodel_unlock(Imod *mod);
  int     imodel_transform_slice(Imod *model, float *mat, 
                                 int slice);
  int     imodel_transform(struct Mod_Transform *tr);
  int     imodel_model_clean(Imod *mod, int keepEmptyObjs);

  /***************************************************************************/
  /* imodel_from.c functions                                                 */
  /***************************************************************************/
  Imod *imod_from_wmod(FILE *fin);
  Imod *imod_from_synu(FILE *fin);
  Imod *imod_from_srec(FILE *fin);
  int substr(char bs[], char ls[]);
  int imodel_from_fgetline(FILE *fp, char s[],int limit);

  /***************************************************************************/
  /* imodel_to.c functions                                                   */
  /***************************************************************************/
  int imod_to_nff(Imod  *mod, FILE *fout);
  int imod_to_wmod(Imod *mod, FILE *fout, char *filename);
  int imod_to_synu(Imod *mod);

  /***************************************************************************/
  /* imodel_files.c functions                                                */
  /***************************************************************************/
  Imod *imodFileRead(char *filename);
  int   imodFileWrite(Imod *imod, char *filename);

  int   imodOpenFile  (char *filename, char *mode, Imod *imod);
  int   imodCloseFile (Imod *imod);
  int   imodReadFile  (Imod *imod);
  Imod *imodRead      (char *filename);
  int   imodWriteFile (Imod *imod);
  int   imodWrite     (Imod *imod, FILE *fout);
  int   imodWriteAscii(Imod *imod);
  int   imodReadAscii (Imod *imod);
  int   imodFgetline(FILE *fp, char s[],int limit);


  float imodGetFloat(FILE *fp);
  int   imodGetFloats(FILE *fp, float *buf, int size);
  int   imodPutFloats(FILE *fp, float *buf, int size);
  int   imodPutScaledPoints(FILE *fp, Ipoint *buf, int size, Ipoint *scale);
  int   imodPutFloat(FILE *fp, float *dat);
  int   imodGetInt(FILE *fp);
  int   imodGetInts(FILE *fp, void *buf, int size);
  int   imodPutInts(FILE *fp, void *buf, int size);
  int   imodPutInt(FILE *fp,  void *dat);
  b3dInt16 imodGetShort(FILE *fp);
  int   imodPutShort(FILE *fp, void *buf);
  unsigned char  imodGetByte(FILE *fp);
  int   imodGetBytes(FILE *fp, unsigned char *buf, int size);
  int   imodPutBytes(FILE *fp, unsigned char *buf, int size);
  int   imodPutByte(FILE *fp,  unsigned char *dat);
  void  imodFromVmsFloats(unsigned char *data, int amt);

  /***************************************************************************/
  /* ilabel.c functions                                                      */
  /***************************************************************************/
  Ilabel *imodLabelNew(void);
  void    imodLabelDelete(Ilabel *label);
  Ilabel *imodLabelDup(Ilabel *label);
  void    imodLabelName(Ilabel *label, char *val);
  void    imodLabelItemAdd(Ilabel *label, char *val, int index);
  void    imodLabelItemMove(Ilabel *label, int to_index, int from_index);
  void    imodLabelItemDelete(Ilabel *label, int index);
  char   *imodLabelItemGet(Ilabel *label, int index);
  void    imodLabelPrint(Ilabel *lab, FILE *fout);
  int     imodLabelWrite(Ilabel *lab, b3dUInt32 tag, FILE *fout);
  Ilabel *imodLabelRead(FILE *fin, int *err);
  int     imodLabelMatch(Ilabel *label, char *tstr);
  int     imodLabelItemMatch(Ilabel *label, char *tstr, int index);
  int     ilabelMatchReg(char *exp, char *str);

  /* parselist.c  - for parsing a list of integers */
  int *parselist (char *line, int *nlist);

#ifdef __cplusplus
}
#endif



/*****************************************************************************/
/* Imod Binary File Format. Version 0.1                                      */
/*****************************************************************************/
/******************************************************************************

Bytes  Data
-----------------
4      IMOD                 
4      V0.1
184    Mod_Model structure data.
*          (repeat for each object in model)
4          OBJT
124        Mod_Object structure data.
*              (repeat for each contour in object )
4              CONT
16             Mod_Contour structure data.
4              PNTS 
12...          repeat for each Mod_Point array data.
               
Future support for optional extra data.
----------------------------------------------------------------------------
Optional chunks can be put at the end of any data structure that the
information is intended for, or at the end of the file.
If you can't understand the data just skip it.
(optional chunks)
4    (Chunk ID)   4 bytes
4    (Chunk Size) long
     (Chunk Data) lengh of (Chunk Size) bytes.
List of future reserved optional chunks.

MESH  Mesh data array in object.
DRAW  Model Draw structure.
VOXL  List of voxels.
******************************************************************************/

/*****************************************************************************/
/* imod ascii file format version 1.0                                        */
/*

# comments begin with '#', blank lines are skipped.
# () mean substitute numerical value.

# first data line
imod (number of objects)

# next lines optional, they change default model values.
# they can be in any order anywhere in the file, (exept inside of contour
# and mesh directives).
offsets (x) (y) (z)
max     (x) (y) (z)
scale   (x) (y) (z)
angle   (x) (y) (z)

# initialize and select current object
object (index) (number of contours in object) (number of meshes in object)

# optional object directives
color (red) (green) (blue) (trans)
open
closed
scattered
wild
fill
nodraw

# contour directive for each contour in current object.
# point data follows contour directive.
contour (index) (surface) (number of points in contour)
(x) (y) (z)
(x) (y) (z) ...

# mesh directive for each mesh in current object.
mesh (index) (vert size) (list size)
(x) (y) (z)
(x) (y) (z)
(x) (y) (z) ...
(list index 1)
(list index 2)
(list index 3) ...
*/
/*****************************************************************************/

/*    
    $Log$
    Revision 3.22  2005/03/20 19:55:48  mast
    Eliminating duplicate functions

    Revision 3.21  2004/12/06 22:00:08  mast
    Removed DelPoint

    Revision 3.20  2004/11/20 04:05:26  mast
    removed virtual stuff, added contour store and function to move object

    Revision 3.19  2004/11/05 18:52:53  mast
    Include local files with quotes, not brackets

    Revision 3.18  2004/09/28 22:21:59  mast
    Added parselist declaration

    Revision 3.17  2004/09/21 20:08:04  mast
    Defined new clipping structures

    Revision 3.16  2004/09/17 19:48:19  mast
    Document use of mat1b3 for point quality

    Revision 3.15  2004/04/28 05:30:00  mast
    defined world flags with bit shifts

    Revision 3.14  2004/01/05 17:22:25  mast
    Added binning run-time member of model structure

    Revision 3.13  2003/10/24 04:07:51  mast
    move some functions to new b3dutil

    Revision 3.12  2003/09/16 02:05:53  mast
    Add function to return flipping state of model

    Revision 3.11  2003/06/27 20:07:52  mast
    Defined bits in world flag for quality;
    changed imodChecksum return type from long to int

    Revision 3.10  2003/03/03 22:45:31  mast
    Changes sizes and a few other variables from unsigned to signed ints

    Revision 3.9  2003/02/21 22:17:47  mast
    Implement new b3d types

    Revision 3.8  2002/12/01 15:40:44  mast
    move ilabel declarations so they are inside the extern "C" construct

    Revision 3.7  2002/09/04 23:13:10  mast
    Add a flag to indicate mat and mat3 are stored as bytes

    Revision 3.6  2002/09/03 20:06:14  mast
    Changed mat1 and mat3 in object this time; last time was in object view

    Revision 3.5  2002/09/03 19:37:01  mast
    Redefine object materials mat1 and mat3 from UINT to 4 UBYTES each because
    code accesses them as individual bytes in numbered positions

    Revision 3.4  2002/08/03 22:46:56  mast
    Changed some comments

    Revision 3.3  2002/07/20 23:26:44  mast
    Define a flag to signify that refImage.otrans has image origin info

    Revision 3.2  2002/05/20 15:27:15  mast
    Fix documentation of contour type and surf

    Revision 3.1  2001/12/05 16:02:06  mast
    Add declaration of imodFromVmsFloats in imodel_files.c

*/
#endif /* imodel.h */

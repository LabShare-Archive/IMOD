/*  IMOD VERSION 2.50
 *
 *  imodel.h -- Image model header file.
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

    $Log$
    Revision 3.4  2002/08/03 22:46:56  mast
    Changed some comments

    Revision 3.3  2002/07/20 23:26:44  mast
    Define a flag to signify that refImage.otrans has image origin info

    Revision 3.2  2002/05/20 15:27:15  mast
    Fix documentation of contour type and surf

    Revision 3.1  2001/12/05 16:02:06  mast
    Add declaration of imodFromVmsFloats in imodel_files.c

*/

#ifndef IMODEL_H
#define IMODEL_H
#define IMOD_MODEL_H

#include <imodconfig.h>
#include <hvemtypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <ilist.h>

/* Mouse mode. */
#define IMOD_MMODEL 1   /* Mouse edits model points.  */
#define IMOD_MMOVIE 2   /* Mouse changes image views. */

/* Imodel Limits */
#define IMOD_STRSIZE 128
#define IMOD_OBJSTRSIZE 64
#define IOBJ_STRSIZE 64
#define IOBJ_EXSIZE 16

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
#define MakeID(a,b,c,d) ( (long)(a)<<24L | (long)(b)<<16L | (c)<<8 | (d))


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
#define ID_CLPL MakeID('C', 'L', 'P', 'L') /* clip list */
#define SIZE_CLIP 28

#define ID_IMAT MakeID('I', 'M', 'A', 'T')  /* object material. */
#define SIZE_IMAT 16

#define ID_IMNX MakeID('M', 'I', 'N', 'X')  /* image  nat. transform.*/
#define SIZE_IMNX 72

#define ID_LABL MakeID('L', 'A', 'B', 'L')   

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


/****************************** Structures ***********************************/

typedef struct Mod_Point
{
     float           x;
     float           y;
     float           z;
}Ipoint;

typedef struct {float a, b, c, d;} Iplane;
typedef Ilist IclipList;
typedef Ilist *Istore;

typedef struct Mod_Mesh
{
     struct Mod_Point *vert;   /* list of points */
     INT              *list;   /* index into vert array + instructions */
     INT        vsize;         /* size of arrays */
     INT        lsize;
     UINT       flag;          /* Tells how to draw mesh */
     INT        type;          /* Set to 0. time data.   */
     INT        pad;           /* Set to 0. surf data.   */
     Istore     store;
}Imesh;


/* Describes current object, contour and point. */
typedef struct Mod_Index
{
     INT  object;
     INT  contour;
     INT  point;
}Iindex;

/* Describes a 3D view of a model. */
#define VIEW_WORLD_ON         1
#define VIEW_WORLD_LIGHT      2
#define VIEW_WORLD_DEPTH_CUE  4
#define VIEW_WORLD_WIREFRAME  8

#define VIEW_WORLD_STEREO   16 /* Stereo flags */
#define VIEW_WORLD_HARDWARE 32
#define VIEW_WORLD_UPDOWN   64
#define VIEW_WORLD_LOWRES  128

/* Properties of an object that are stored in a view */
typedef struct Mod_Object_View
{
     UINT              flags;               /* bit flags IMOD_OBJFLAG        */
     float   red;                 /* Red (0 - 1.0)                 */
     float   green;               /* Green (0 - 1.0)               */
     float   blue;                /* Blue (0 - 1.0)                */
     INT     pdrawsize;           /* size to draw scattered objs   */

     UBYTE     linewidth;           /* linewidth in 3-D              */
     UBYTE     linesty;             /* line draw style               */
     UBYTE     trans;               /* transperentcy                 */

     /* new extended data imod 1.2 */
     /* chunk CLIP */
     /* define a clipping plane. max for each object is 8. */
     UBYTE clip;        /* number of additional clip planes. */
     UBYTE clip_flags;  /* Which clip planes are on.         */
     UBYTE clip_trans;  /* Transperentcy for clipped area    */
     UBYTE clip_plane;  /* Current clip plane.               */
     Ipoint clip_normal;
     Ipoint clip_point;

     /* Added info IMAT */
     UBYTE ambient;   /* Ambient multiplier to color */
     UBYTE diffuse;   /* Diffuse multiplier to color */
     UBYTE specular;  /* Specular property, added to color */
     UBYTE shininess; /* shininess exponent */
     UBYTE mat1;      /* Fill color red */
     UBYTE mat1b1;    /* Fill color green */
     UBYTE mat1b2;    /* Fill color blue */
     UBYTE mat1b3;    /* Unused */
     UINT  mat2;      /* set to 0, use as flags.  Unused */
     UBYTE mat3;      /* Black level for showing normal magnitudes in mesh */
     UBYTE mat3b1;    /* Black level for showing normal magnitudes in mesh */
     UBYTE mat3b2;    /* Unused */
     UBYTE mat3b3;    /* Unused */
}Iobjview;

typedef struct
{
     /* Set up the camera */
     float  fovy;   /* field of view of camera, perspective in degrees.  */
     float  rad;    /* viewing radius of sphere encloseing bounding box. */
     float  aspect; /* aspect ratio */
     float  cnear;  /* clip near range 0.0 to 1.0, default 0.0. */
     float  cfar;   
     
     /* Model transformation values for model view.
      */
     Ipoint rot;
     Ipoint trans;
     Ipoint scale;

     /* World OpenGL transformation matrix. */
     float mat[16];

     int   world;    /* flags, true if we use world matrix. */
     char  label[32];

     float dcstart, dcend;
     float lightx, lighty;
     float plax;
     Iobjview *objview;    /* properties of each object */
     int   objvsize;       /* Number of objects properties exist for */
     
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
     char *name;
     INT  len;
     UINT index;
}IlabelItem;

typedef struct
{
     char       *name;
     INT         len;
     UINT        nl;
     IlabelItem *label;
}Ilabel;

Ilabel *imodLabelNew(void);
void    imodLabelDelete(Ilabel *label);
void    imodLabelName(Ilabel *label, char *val);
void    imodLabelItemAdd(Ilabel *label, char *val, int index);
void    imodLabelItemMove(Ilabel *label, int to_index, int from_index);
void    imodLabelItemDelete(Ilabel *label, int index);
char   *imodLabelItemGet(Ilabel *label, int index);
void    imodLabelPrint(Ilabel *lab, FILE *fout);
int     imodLabelWrite(Ilabel *lab, FILE *fout);
Ilabel *imodLabelRead(FILE *fin, int *err);
int     imodLabelMatch(Ilabel *label, char *tstr);
int     imodLabelItemMatch(Ilabel *label, char *tstr, int index);
int     ilabelMatchReg(char *exp, char *str);

/*
 * Contours are an array of points describing an open or closed contour.
 * The points can also be unconnected or the points can be in pairs
 * describing scan lines.
 */
typedef struct Mod_Contour
{
     /* Run time data */
     Ipoint *pts;    /* Points data.                      */
     float  *sizes;  /* sizes for scattered points        */

     /* data below is written to file                     */
     UINT    psize;  /* Number of points.                 */
     UINT    flags;  /* Default 0 means use object flags. */
     INT     type;   /* Time index.                       */
     INT     surf;   /* Surface number.                   */

     Ilabel  *label;
/*     Istore store;*/

}Icont;


/* An Object is an array of contours */
typedef struct Mod_Object
{
     /* Run time data */
     struct Mod_Contour *cont;              /* Contour data.                 */
     struct Mod_Mesh    *mesh;

     struct Mod_Contour *vcont;             /* virtual contour */
     struct Mod_Mesh    *vmesh;

     UINT fgcolor;  /* colors used for colorindex rendering. */
     UINT bgcolor;

     /* data below is written to file */
     char              name[IOBJ_STRSIZE];  /* Name of Object.               */
     UINT              extra[IOBJ_EXSIZE];  /* extra unused data = 0         */

     /* Use some EXSIZE data for light location. 
	float            lightOrientation[3];
     */


     UINT              contsize;            /* Number of Contours in object. */
     UINT              flags;               /* bit flags IMOD_OBJFLAG        */
     INT               axis;                /* Z = 0, X = 1, Y = 2. (future) */
     INT               drawmode;            /* Tells type of points to draw  */
                                            /* for scatterd objects.         */
     float             red;                 /* Red (0 - 1.0)                 */
     float   green;               /* Green (0 - 1.0)               */
     float   blue;                /* Blue (0 - 1.0)                */
     INT     pdrawsize;           /* size to draw scattered objs   */

     UBYTE     symbol;              /* default 0=circle              */
     UBYTE     symsize;             /* size of symbol.               */
     UBYTE     linewidth2;          /* linewidth in 2-D              */
     UBYTE     linewidth;           /* linewidth in 3-D              */
     UBYTE     linesty;             /* line draw style               */
     UBYTE     symflags;
     UBYTE     sympad;
     UBYTE     trans;               /* transperentcy                 */

     INT       meshsize;            /* Number of meshes in object.   */
     INT       surfsize;            /* Max surfaces in object.       */

     /* new extended data imod 1.2 */
     /* chunk CLIP */
     /* define a clipping plane. max for each object is 8. */
     UBYTE clip;        /* number of additional clip planes. */
     UBYTE clip_flags;  /* Which clip planes are on.         */
     UBYTE clip_trans;  /* Transperentcy for clipped area    */
     UBYTE clip_plane;  /* Current clip plane.               */
     Ipoint clip_normal;
     Ipoint clip_point;

     /* Added info IMAT */
     UBYTE ambient;
     UBYTE diffuse;
     UBYTE specular;  /* specular IS shininess   */
     UBYTE shininess; /* shininess used as flags */
     UINT  mat1; /* set to 0, use as material color future. ABGR */
     UINT  mat2; /* set to 0, use as flags. */
     UINT  mat3; /* used  byte0 = bright, byte1 = contrast */

     Istore store;
}Iobj;



/* A Model is an array of objects */
typedef struct Mod_Model
{
     /* Run time data */
     Iobj  *obj;          /* Object data.                       */
     FILE  *file;         /* Current file handle.               */
     INT   lock;          /* Locks model data for shared procs. */
     INT   redraw;        /* Used for shared procs.             */
     INT   ctime;         /* current time index.                */

     /* data written to file    */
     char   name[IMOD_STRSIZE];        /* file name of model.                */
                                       /* Use labels for model name.         */
     INT    xmax, ymax, zmax;  
     UINT   objsize;                   /* Number of objects in model.        */
     UINT   flags;                     /* IMODF_FLAG values      */
     INT    drawmode;                  /* 1 to draw model, -1 not to         */
     INT    mousemode;                 /* Tells what mouse buttons do.       */
     INT    blacklevel, whitelevel;    /* Contrast adjustment.               */
     float  xoffset, yoffset, zoffset; /* offsets used for 3-D display       */
     float  xscale;                    /* pixel size zoom.                   */
     float  yscale;                    /* y aspect for pixels.               */
     float  zscale;                    /* z aspect for sections.             */
     Iindex cindex;       /* Current object, contour & point     */
     INT    res;          /* Number of pixels between points.    */
     INT    thresh;       /* Threshold level for auto cont.      */
     float  pixsize;      /* size of unit 1.0 pixel              */
     INT    units;        /* units of given size.                */
     INT    csum;         /* checksum storage.                   */
     INT    cview;
     INT    viewsize;     
     INT    tmax;          /* max time */
     
     /* imod file version 0.2 data */
     float  alpha, beta, gamma;        /* rotation of model in 3-D view. */

     /* more runtime data */
     USHORT   color; /* color of current object */
     Iview   *view;

     IclipList  *clipList;
     IrefImage  *refImage;
     char       *fileName;

     Istore store; /* Add support for easy addon of user configurable data */

}Imod;




/* Used for drawing models, imodv */
typedef struct Mod_Draw
{
     INT llx;     /* Used for drawing 2d models to sub area */
     INT lly;     
     INT urx;
     INT ury;
     INT llz;
     INT urz;
     float xorg;    /* Offset for 2d, translation for 3d models. */
     float yorg;
     float zorg;
     INT xrot;     /* Amount to rotate model */
     INT yrot;
     INT zrot;
     float xtrans;   /* Amount to translate model */
     float ytrans;
     float ztrans;
     INT axis;     /* Axis to rotate.         */
     INT cmapbase; /* Where to start colormap */
     float left,
           right,
           bottom,
           top;
     INT    cnear,  /* Clipping planes */
            cfar;
     INT   dc;     /* min depth cue factor  range x/1 to x/16*/
     float zoom;   /* Mag factor for displaying */
     float cdist;  /* Distence for center of model to far corner */
     float edist;  /* Distence from center of model to eye.      */
     INT xrotm;    /* xyz movie amounts */
     INT yrotm;
     INT zrotm;
     INT arot;     /* amount of change in rotation */
     float azoom;
     float atrans;
     float azscale;
} Idraw;


typedef struct Mod_Transform
{
     float x;       /* input x, y and z. */
     float y;
     float z;
     float xout;    /* output values after transformation. */
     float yout;
     float zout;
     float xtrans;  /* amount to translate x, y and z. */
     float ytrans;
     float ztrans;
     float xrot;    /* amount to rotate x, y, and z. */
     float yrot;
     float zrot;
     float xscale;  /* amount to scale x, y and z. */
     float yscale;
     float zscale;
}Itrans;


/* include functions in iobj.c, icont.c, imesh.c, ipoint.c */
#include <iobj.h>
#include <icont.h>
#include <imesh.h>
#include <ipoint.h> 
#include <imat.h>
#include <iplane.h>
#include <iview.h>

/*****************************************************************************/
/* imodel.c functions                                                        */
/* Model structure is passed to functions.                                   */
/*****************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif

int imodVersion(char *pname);
void imodCopyright(void);
Imod *imodNew       (void);
void  imodFree      (Imod *imod);
void  imodDelete(Imod *imod);
int   imodNewObject (Imod *imod);
void  imodDeleteObject(Imod *imod, int index);
int   imodFreeObject(Imod *imod, int index);
int   imodNextObject(Imod *imod);
int   imodPrevObject(Imod *imod);
int   imodDefault(Imod *imod);
void  imodCleanSurf(Imod *imod);
void  imodFlipYZ(Imod *imod);
int   imodVirtIn(Imod *imod);

int   imodNewContour(Imod *imod);
int   imodPrevContour(Imod *imod);
int   imodNextContour(Imod *imod);
int NewContour (Imod *imod);
int FreeContour (Imod *imod, int index);
int PrevContour(Imod *imod);
int NextContour(Imod *imod);
int DelContour(Imod *imod, int index);
void imodDeleteContour(Imod *imod);

int   imodNewPoint(Imod *imod, Ipoint *pt);
int   imodInsertPoint(Imod *imod, Ipoint *point, int index);
int   imodDeletePoint(Imod *imod);
int   imodPrevPoint(Imod *imod);
int   imodNextPoint(Imod *imod);
int NewPoint(struct Mod_Model *mod, struct Mod_Point *pt);
int InsertPoint(struct Mod_Model *mod, struct Mod_Point *pt, int index);
int DelPoint(struct Mod_Model *mod);
int PrevPoint(struct Mod_Model *mod);
int NextPoint(struct Mod_Model *mod);

void  imodGetIndex(Imod *imod, int *object, int *contour, int *point);
void  imodSetIndex(Imod *imod, int object, int contour, int point);
void  imodGetBoundingBox(Imod *imod, Ipoint *min, Ipoint *max);
int   imodGetMaxObject(Imod *imod);
float imodGetZScale(Imod *imod);
float imodGetPixelSize(Imod *imod);
int   imodTransform(Imod *imod, Imat *mat);

double  imodel_dist(Imod *imod);
void    imodel_minpt(Imod *imod, Ipoint *pnt);
void    imodel_maxpt(Imod *imod, Ipoint *pnt);
int     imodGetMaxTime(Imod *imod);
char   *imodUnits(Imod *mod);
Ipoint *imodNearestPoint(Imod *imod, Ipoint *pt);
long    imodChecksum(Imod *imod);
int     imodel_lock(struct Mod_Model *mod, int flag);
int     imodel_unlock(struct Mod_Model *mod);
int     imodel_transform_slice(struct Mod_Model *model, float *mat, int slice);
int     imodel_transform(struct Mod_Transform *tr);
int     imodel_model_clean(struct Mod_Model *mod, int keepEmptyObjs);

/*****************************************************************************/
/* imodel_from.c functions                                                   */
/*****************************************************************************/
Imod *imod_from_wmod(FILE *fin);
Imod *imod_from_synu(FILE *fin);
Imod *imod_from_srec(FILE *fin);
int substr(char bs[], char ls[]);
int imodel_from_fgetline(FILE *fp, char s[],int limit);

/*****************************************************************************/
/* imodel_to.c functions                                                     */
/*****************************************************************************/
int imod_to_nff(Imod  *mod, FILE *fout);
int imod_to_wmod(Imod *mod, FILE *fout, char *filename);
int imod_to_synu(struct Mod_Model *mod);

/*****************************************************************************/
/* imodel_files.c functions                                                  */
/*****************************************************************************/
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
int   imodPutFloat(FILE *fp, float *dat);
int   imodGetInt(FILE *fp);
int   imodGetInts(FILE *fp, void *buf, int size);
int   imodPutInts(FILE *fp, void *buf, int size);
int   imodPutInt(FILE *fp,  void *dat);
short imodGetShort(FILE *fp);
int   imodPutShort(FILE *fp, void *buf);
unsigned char  imodGetByte(FILE *fp);
int   imodGetBytes(FILE *fp, unsigned char *buf, int size);
int   imodPutBytes(FILE *fp, unsigned char *buf, int size);
int   imodPutByte(FILE *fp,  unsigned char *dat);
void  imodFromVmsFloats(unsigned char *data, int amt);

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

#endif /* imodel.h */

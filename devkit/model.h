/*  IMOD VERSION 2.20
 *
 *  2.00b7 added function imodObjectGetMesh.
 *
 *  2.00b6 added GetFirst and GetNext functions for many structures.
 *
 *  2.00b5 first imod release containing devkit.
 ****************************************************************************
 *  model.h - The imod model library.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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
#ifndef IMOD_MODEL_H
#define IMOD_MODEL_H

struct privateStruct
{
     int privateData;
};

typedef struct { float x, y, z; }    Ipoint;
typedef struct { float a, b, c, d; } Iplane;

typedef struct privateStruct Imod;    /* The entire main model structure. */
typedef struct privateStruct Iview;   /* The view or camera structure.    */
typedef struct privateStruct Iobj;    /* Object data structure.           */
typedef struct privateStruct Imesh;   /* Mesh data structure.             */ 
typedef struct privateStruct Icont;   /* Contour data structure.          */
typedef struct privateStruct Ilabel;  /* Point label structure.           */

#define IMOD_NoIndex -1

/*****************************************************************************/
/* model functions. */

#ifdef __cplusplus
     extern "C" {
#endif

/* create a new model with default settings. */
Imod *imodNew(void);

/* delete a model freeing all memory. */
void  imodDelete(Imod *imod);

/* read and write imod models with the given filenames.
 * if NULL is returned while reading the read failed.
 * Use the stdio library's errno and perror for the exact error.
 * If errno is zero, assume the file is not a model file.
 * if non-zero is returned with writing the write failed.
 */
Imod *imodFileRead(char *filename);
int   imodFileWrite(Imod *imod, char *filename);

/*
 *  Get and Set the current index.  You should check after setting
 *  that the values were set to the values you wanted.
 *  The functions below operate on the current object contour or point.
 */
void  imodGetIndex(Imod *imod, int *object, int *contour, int *point);
void  imodSetIndex(Imod *imod, int object, int contour, int point);

/**************************
 *  Model object functions.
 */

/* Create a new object in the model, return non-zero on error. */
int   imodNewObject (Imod *imod);

/* Try and delete the object at the given index. */
void  imodDeleteObject(Imod *imod, int index);

/* goto next or previous object, return the new object index. 
 * the object index wont be changed if the current object 
 * is the last or first object. 
 */
int   imodNextObject(Imod *imod);
int   imodPrevObject(Imod *imod);

/**************************************************************
 *  Model contour functions.  Similar to model object funtions.
 */
int   imodNewContour(Imod *imod);
void  imodDeleteContour(Imod *imod);
int   imodNextContour(Imod *imod);
int   imodPrevContour(Imod *imod);

/************************************************************
 *  Model point functions.  Similar to model object funtions.
 */
int   imodNewPoint(Imod *imod, Ipoint *pt);
void  imodDeletePoint(Imod *imod);
int   imodNextPoint(Imod *imod);
int   imodPrevPoint(Imod *imod);

/*************************
 *  Other model functions.
 */

/* return the number of objects in model */
int   imodGetMaxObject(Imod *imod); 

/* return z scale factor.                */
float imodGetZScale(Imod *imod);

/* return pixel size.                    */
float imodGetPixelSize(Imod *imod);

/* return units for pixels. ie "nm", "um", "mm"   */
char *imodUnits(Imod *mod);

/* return the Bounding Box for the entire model.
 * min will contain the smallest point value in the model.
 * and max will contain the largest point value in the model.
 */
void  imodGetBoundingBox(Imod *imod, Ipoint *min, Ipoint *max);


/*****************************************************************************/
/* object data/functions. */

/*
 *  inValueType defines used for imodObjectGetValue, imodObjectSetValue.
 */
#define IobjMaxContour    33  /* GetValue only. Number of contour in object. */
#define IobjLineWidth     34  /* Get or Put. Rendering line width in pixels. */
#define IobjPointSize     35  /* Get or Put. Rendering point size in pixels. */
#define IobjMaxMesh       36  /* GetValue only. Number of meshes in object.  */
#define IobjMaxSurface    37  /* GetValue only. Number of surfaces in object.*/

#define IobjFlagConnected 9  /* Connect points if set. */
#define IobjFlagClosed    3  /* Also connect the first-last points if set. */
#define IobjFlagFilled    8  /* Render contours as filled if set. */
#define IobjFlagDraw      1  /* Draw if set. */
#define IobjFlagMesh      10 /* Render mesh data instead of contours if set. */
#define IobjFlagLine      11 /* Draw lines in 3D view if set. */
#define IobjFlagTime      12 /* Draw lines in 3D view if set. */

/* get the current, first or next object from a model. */
Iobj *imodObjectGet(Imod *inModel);
Iobj *imodObjectGetFirst(Imod *inModel);
Iobj *imodObjectGetNext(Imod *inModel);

/* create a new stand-alone object that is not part of a model. */
Iobj *imodObjectNew(void);
int   imodObjectDelete(Iobj *inObject);

/* add and remove contours from the given object. */
int   imodObjectAddContour(Iobj *inObject, Icont *inContour);
int   imodObjectRemoveContour(Iobj *obj, int index);

/*
 * Use the value type flags above to get/set object values.
 */
int   imodObjectGetMaxContour(Iobj *inObject);

char *imodObjectGetName(Iobj *inObject);
int   imodObjectSetName(Iobj *inObject, char *inName);

int   imodObjectGetValue(Iobj *inObject, int inValueType);
void  imodObjectSetValue(Iobj *inObject, int inValueType, int inValue);

void  imodObjectGetColor(Iobj *inObject,
			 float *outRed, float *outGreen, float *outBlue);


/*
 *  Returns the mesh structure with the given index.
 *  To find the max index call imodObjectGetValue(inObject, IobjMaxMesh);
 *  Any error such as the inIndex value being out of bounds 
 *  causes a NULL to be returned.
 */
Imesh *imodObjectGetMesh(Iobj *inObject, int inIndex);

/*
 * Add new mesh to object, returns index of mesh.
 * If the return index is negative, an error occured and
 * no new data was added to the inObject.
 */
int    imodObjectAddMesh(Iobj *inObject, Imesh *inMesh);

/* The bounding box for inObject is returned through the min and max
 * point values.  The min value is the lower left front coordinate of the 
 * bounding box and the max value is the upper right back coordinate. 
 * If the return value is non-zero an error occured.
 */
int   imodObjectGetBBox(Iobj *inObject, Ipoint *min, Ipoint *max);

/*****************************************************************************/
/* contour functions                                                         */

#define IMOD_CONTOUR_CLOCKWISE -1
#define IMOD_CONTOUR_COUNTER_CLOCKWISE 1

/* Get the current contour form the model. */
Icont *imodContourGet(Imod *inModel);
/* Get first contour in current object. */
Icont *imodContourGetFirst(Imod *imod);  
/* Get the next contour in the model, NULL if already at last contour. */
Icont *imodContourGetNext(Imod *imod);

/* contour create, delete and move functions. */
Icont *imodContourNew(void);
Icont *imodContoursNew(int size);
void   imodContourDefault(Icont *cont);
int    imodContourDelete(Icont *cont);
void   imodContourSwap(Icont *c1, Icont *c2);
int    imodContourCopy(Icont *from, Icont *to);
Icont *imodContourDup(Icont *cont); 

char   *imodContourGetName(Icont *inContour);
Ilabel *imodContourGetLabel(Icont *inContour);
void    imodContourSetLabel(Icont *inContour, Ilabel *inLabel);
int     imodContourGetMaxPoint(Icont *inContour);
void    imodContourSetPointData(Icont *inContour, Ipoint *inPoint, int inMax);
Ipoint *imodContourGetPoints(Icont *inContour);
Ipoint *imodContourGetPoint(Icont *inContour, int inIndex);

int     imodContourGetTimeIndex(Icont *inContour);
void    imodContourSetTimeIndex(Icont *inContour, int inTime);
int     imodContourGetSurface(Icont *inContour);
void    imodContourSetSurface(Icont *inContour, int inSurface);
int     imodContourGetBBox(Icont *cont, Ipoint *min, Ipoint *max);

/*****************************************************************************/
/* label functions                                                           */
Ilabel *imodLabelNew();
void    imodLabelDelete(Ilabel *label);
void    imodLabelName(Ilabel *label, char *val);
void    imodLabelItemAdd(Ilabel *label, char *val, int index);
void    imodLabelItemMove(Ilabel *label, int to_index, int from_index);
void    imodLabelItemDelete(Ilabel *label, int index);
char   *imodLabelItemGet(Ilabel *label, int index);
int     imodLabelMatch(Ilabel *label, char *tstr);
int     imodLabelItemMatch(Ilabel *label, char *tstr, int index);
int     ilabelMatchReg(char *exp, char *str);


/*****************************************************************************/
/* point functions */

Ipoint *imodPointGet(Imod *imod);
Ipoint *imodPointGetFirst(Imod *imod);
Ipoint *imodPointGetNext(Imod *imod);

int     imodPointAppend(Icont *cont, Ipoint *pnt);
int     imodPointAdd(Icont *cont, Ipoint *pnt, int index);
int     imodPointDelete(Icont *cont, int index);

void    imodPointCross( Ipoint *inPoint1, Ipoint *inPoint2, Ipoint *outPoint);
void    imodPointNormalize(Ipoint *ioPoint);
float   imodPointDistance(Ipoint *inPoint1, Ipoint *inPoint2);
float   imodPointDot(Ipoint *inPoint1, Ipoint *inPoint2);
float   imodPointArea(Ipoint *inPoint1, Ipoint *inPoint2, Ipoint *inPoint3);

     
/*****************************************************************************/
/* mesh functions */

/*
 * Index values for mesh. Positive values are indexed into the vertex point
 * data. 
 */
#define IMOD_MESH_END         -1  /* end of list data.       */
#define IMOD_MESH_ENDPOLY     -22 /* end of polygon data.    */
#define IMOD_MESH_BGNBIGPOLY  -24 /* large concave polygon.  */
#define IMOD_MESH_BGNTRINORM -23  /* Triangles with normals. */ 

/*
 *  Create or delete a mesh that is not part of an object or model.
 *
 *  NOTE: There is no imodMeshGet, imodMeshGetFirst or imodMeshGetNext
 *        functions because the model doesn't store a current mesh 
 *        because mesh data is not editable directly by the user.
 */
Imesh *imodMeshNew(void);
int    imodMeshDelete(Imesh *mesh);

int    imodMeshAddIndex(Imesh *mesh, int index);
void   imodMeshInsertIndex(Imesh *mesh, int val, int place);
void   imodMeshDeleteIndex(Imesh *mesh, int index);
int    imodMeshGetIndex(Imesh *mesh, int index);
int    imodMeshGetMaxIndex(Imesh *mesh);

int     imodMeshAddVert(Imesh *mesh, Ipoint *vert);
int     imodMeshGetMaxVert(Imesh *mesh);
Ipoint *imodMeshGetVert(Imesh *mesh, int index);
Ipoint *imodMeshGetVerts(Imesh *mesh);

#ifdef __cplusplus
}
#endif

#endif /* MODEL_H */

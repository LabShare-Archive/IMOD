/*
 *  imodel.c -- Library functions for handling model structures.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include <math.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"
#include "objgroup.h"

static void flipClips(IclipPlanes *clips);
static void rotClips(float yconst, float yfac, float zconst, float zfac, 
                     IclipPlanes *clips);
static void invertClips(float zconst, IclipPlanes *clips);

/*!
 * Allocates a new model structure and returns a pointer to it, or NULL if 
 * there is an error.  Initializes the model with @imodDefault 
 */
Imod *imodNew(void)
{
  Imod *model;

  model = (Imod *)malloc(sizeof(Imod));
  if (model == NULL)
    return((Imod *)NULL);
  model->file     = NULL;
  imodDefault(model);
  return(model);
}

/*!
 * Initializes model structure pointed to by [model] to default values and
 * allocates an initial view (which will leak if this is called twice).  
 * Returns 0 (there are no errors) 
 */
int imodDefault(Imod *model)
{
  int  i;
  char *newmodname = "IMOD-NewModel";

  model->objsize  = 0;
  model->flags = IMODF_NEW_TO_3DMOD;
  for (i = 0; i < 13; i++)
    model->name[i] = newmodname[i];
  model->name[i]    = 0x00;
  model->drawmode   = 1;
  model->mousemode  = IMOD_MMOVIE;
  model->blacklevel = 0;
  model->whitelevel = 255;
  
  model->xoffset = 0;
  model->yoffset = 0;
  model->zoffset = 0;
     
  model->xscale = 1;
  model->yscale = 1;
  model->zscale = 1;
     
  model->cindex.object  = -1;
  model->cindex.contour = -1;
  model->cindex.point   = -1;
  model->ctime = 0;
  
  model->res = 3;
  model->thresh = 128;
  model->pixsize = 1.0;  
  model->units = 0;      /* if unit is 0, pixsize is undefined */
     
  model->csum  = 0;
  model->tmax  = 0;
  model->xmax = 1;
  model->ymax = 1;
  model->zmax = 1;

  model->alpha = 0.0f;
  model->beta  = 0.0f;
  model->gamma = 0.0f;
    
  model->view     = NULL; /* available views. */
  model->cview    = 0;    /* current view.    */
  model->viewsize = 0;    /* number of views. */
  imodViewModelNew(model); /* default view */
  model->editGlobalClip = 0;
  model->curObjGroup = -1;

  model->refImage = NULL;
  model->fileName = NULL;
  model->xybin = 1;
  model->zbin = 1;
  model->store    = NULL;
  model->slicerAng = NULL;
  model->groupList = NULL;
  return(0);
}

/*! 
 * Deletes a model; frees all memory used in objects, views, and object 
 * views and frees the model itself. 
 */
void imodDelete(Imod *imod)
{
  if (!imod)
    return;

  /* DNM 7/24/03: free the object view and view data also */
  imodObjviewsFree(imod);
  imodViewDelete(imod->view);
  imodObjectsDelete(imod->obj, imod->objsize);
  if (imod->fileName)
    free(imod->fileName);
  if (imod->refImage)
    free(imod->refImage);
  ilistDelete(imod->store);
  objGroupListDelete(imod->groupList);
  free(imod);
}

/*!
 * Returns the current object, contour, and point indexes of the model in
 * [object], [contour], and [point] 
 */
void imodGetIndex(Imod *imod, int *object, int *contour, int *point)
{
  *object  = imod->cindex.object;
  *contour = imod->cindex.contour;
  *point   = imod->cindex.point;
  return;
}

/*!
 * Sets the current object, contour, and point indexes of the model with the
 * values in [object], [contour], and [point].  Indexes are checked for 
 * legality and if one is out of bounds it is set to -1.  If an index is -1, 
 * any index at a lower level is set to -1 also (e.g., point if contour is -1).
 */
void imodSetIndex(Imod *imod, int object, int contour, int point)
{
  Iobj *obj;
  Icont *cont;
     
  if (object < 0)
    object = contour = point = -1;
  if (object >= imod->objsize)
    object = 0;

  imod->cindex.object  = object;
  obj = imodObjectGet(imod);
  if (!obj){
    imod->cindex.contour = imod->cindex.point = -1;
    return;
  }

  /* check for contour # out of bounds and set contour index. */
  if (contour < 0) 
    contour = point = -1;
  if (contour >= obj->contsize)
    contour = obj->contsize - 1;
  imod->cindex.contour = contour;
  cont = imodContourGet(imod);
  if (!cont){
    imod->cindex.point = -1;
    return;
  }

  /* check for point # out of bounds */
  if (point >= cont->psize) 
    point = cont->psize - 1;
  if (point < 0) 
    point = -1;
  if (cont->psize < 1)
    point = -1;
  imod->cindex.point   = point;
     
  return;
}

/* DNM 6/26/03: for these 3 functions, return -1, -1, -1 if no points in
   model; also actually stop on first point (3/18/05). */

/*!
 * Returns the maximum coordinates of contour points in model [imod] in [pnt], or -1,-1,-1
 * if there are no points 
 */
void imodel_maxpt(Imod *imod, Ipoint *pnt)
{
  int ob, co, pt;
  int gotOne = 0;
  Iobj *obj;
  Icont *cont;

  pnt->x = pnt->y = pnt->z = -1.;

  /* Get first point */
  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (cont->psize) {
        pnt->x = cont->pts->x;
        pnt->y = cont->pts->y;
        pnt->z = cont->pts->z;
        co = obj->contsize;
        ob = imod->objsize;
        gotOne = 1;
        break;
      }
    }
    if (gotOne)
      break;
  }

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++){
        if (cont->pts[pt].x > pnt->x)
          pnt->x = cont->pts[pt].x;
        if (cont->pts[pt].y > pnt->y)
          pnt->y = cont->pts[pt].y;
        if (cont->pts[pt].z > pnt->z)
          pnt->z = cont->pts[pt].z;
      }
    }
  }
  return;
}

/* DMN 11/29/11: Change this to return real limits of contourless models with meshes */
/*!
 * Returns the minimum coordinates of contour points in the model [imod] in [pnt], or 
 * -1,-1,-1 if there are no points
 */
void imodel_minpt(Imod *imod, Ipoint *pnt)
{
  int ob, co, pt;
  Iobj *obj;
  Icont *cont;
  int gotOne = 0;
     
  pnt->x = pnt->y = pnt->z = -1.;

  /* Get first point */
  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for (co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (cont->psize) {
        pnt->x = cont->pts->x;
        pnt->y = cont->pts->y;
        pnt->z = cont->pts->z;
        co = obj->contsize;
        ob = imod->objsize;
        gotOne = 1;
        break;
      }
    }
    if (gotOne)
      break;
  }

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++){
        if (cont->pts[pt].x < pnt->x)
          pnt->x = cont->pts[pt].x;
        if (cont->pts[pt].y < pnt->y)
          pnt->y = cont->pts[pt].y;
        if (cont->pts[pt].z < pnt->z)
          pnt->z = cont->pts[pt].z;
      }
    }
  }
  return;
}

/*!
 * Returns the minimum and maximum coordinates of the model [imod] in [min] 
 * and [max], based either on the coordinates of points in contours, or on mesh vertices 
 * for objects with no contours.  Returns -1,-1,-1 if there are no points or meshes.
 */
void imodGetBoundingBox(Imod *imod, Ipoint *min, Ipoint *max)
{
  int ob;
  Ipoint obmin, obmax;
  int gotOne = 0;

  min->x = min->y = min->z = -1.;
  max->x = max->y = max->z = -1.;
     
  for (ob = 0; ob < imod->objsize; ob++) {
    if (imodObjectGetBBox(&(imod->obj[ob]), &obmin, &obmax) >= 0) {
      if (gotOne) {
        min->x = B3DMIN(min->x, obmin.x);
        min->y = B3DMIN(min->y, obmin.y);
        min->z = B3DMIN(min->z, obmin.z);
        max->x = B3DMAX(max->x, obmax.x);
        max->y = B3DMAX(max->y, obmax.y);
        max->z = B3DMAX(max->z, obmax.z);
      } else {
        *min = obmin;
        *max = obmax;
        gotOne = 1;
      }
    }
  }
}

/* Returns distance of current point from origin - unused and unsafe */
double imodel_dist(Imod *imod)
{
  double x, y;

  if (imod->cindex.point < 1)
    return(0.0);
    
  x =  imod->obj[imod->cindex.object].cont[imod->cindex.contour].pts[imod->cindex.point].x;
  y =  imod->obj[imod->cindex.object].cont[imod->cindex.contour].pts[imod->cindex.point].y;
  x -= imod->obj[imod->cindex.object].cont[imod->cindex.contour].pts[imod->cindex.point - 1].x;
  y -= imod->obj[imod->cindex.object].cont[imod->cindex.contour].pts[imod->cindex.point - 1].y;

  x *= x;
  y *= y;

  return(sqrt(x+y));

}

/*****************************************************************************/
/* OBJECT-LEVEL FUNCTIONS
 * DOC_SECTION OBJECT 
 */

/* static float basered = 0.5, basegreen = 0.5, baseblue = 0.5; */
#define MAX_STOCK_COLORS  35

/*!
 * Adds new object to end of object array in model [mod].
 * Color is automatically assigned from the collection of 35 stock colors.
 * Returns 1 if error.
 */
int imodNewObject(Imod *mod)
{
  Iobj *obj;
  int colorInd;

  /* Default Colors for Objects */
  float colors[MAX_STOCK_COLORS][3]  = { 
    {0.0, 1.0, 0.0},   /* Green       */
    {0.0, 1.0, 1.0},   /* Cyan        */
    {1.0, 0.0, 1.0},   /* Magenta     */
    {1.0, 1.0, 0.0},   /* Yellow      */
    {0.0, 0.0, 1.0},   /* Blue        */
    {1.0, 0.0, 0.0},   /* Red         */
    {0.0, 1.0, 0.5},
    {0.2, 0.2, 0.8},
    {0.8, 0.2, 0.2},
    {0.9, 0.6, 0.4},
    {0.6, 0.4, 0.9},
    {0.1, 0.6, 0.4},
    {0.6, 0.1, 0.4},
    {0.2, 0.6, 0.8},
    {1.0, 0.5, 0.0},
    {0.4, 0.6, 0.1},
    {0.1, 0.1, 0.6},
    {0.9, 0.9, 0.4},
    {0.9, 0.4, 0.6},
    {0.4, 0.9, 0.9},
    {0.6, 0.2, 0.2},
    {0.2, 0.8, 0.6},
    {0.4, 0.6, 0.9},
    {0.1, 0.6, 0.1},
    {0.8, 0.5, 0.2},
    {1.0, 0.0, 0.5},
    {0.0, 0.5, 1.0},
    {0.6, 0.2, 0.8},
    {0.5, 1.0, 0.0},
    {0.1, 0.4, 0.6},
    {0.6, 0.4, 0.1},
    {0.8, 0.2, 0.6},
    {0.4, 0.1, 0.6},
    {0.2, 0.8, 0.2},
    {0.9, 0.4, 0.9}
  };

  if (mod->objsize == 0)
    obj = (Iobj *)malloc( sizeof(Iobj) );
  else
    obj = (Iobj *)realloc
      (mod->obj, sizeof(Iobj) * (mod->objsize + 1));

  if (obj == NULL){
    mod->cindex.object = -1;
    mod->cindex.contour = -1;
    mod->cindex.point = -1;
    return(1);
  }

  ++mod->objsize;
  mod->obj = obj;
  imodObjectDefault(&(obj[mod->objsize - 1]));
     
  /*  if (mod->objsize < 12){ */

  colorInd = (mod->objsize - 1) % MAX_STOCK_COLORS;
  obj[mod->objsize - 1].red      = colors[colorInd][0];
  obj[mod->objsize - 1].green    = colors[colorInd][1];
  obj[mod->objsize - 1].blue     = colors[colorInd][2];
    /*  }else{ */

    /* DNM: Cycle through colors at different rates; base it on an
       initially fixed constant rather than on the last object */
    /*    basered += 0.27;
    basegreen += 0.37;
    baseblue += 0.63;
    if (basered > 1.0)
      basered -= 1.0;
    if (basegreen > 1.0)
      basegreen -= 1.0;
    if (baseblue > 1.0)
      baseblue -= 1.0;

    obj[mod->objsize - 1].red      = basered;
    obj[mod->objsize - 1].green    = basegreen;
    obj[mod->objsize - 1].blue     = baseblue;

    } */
     
  mod->cindex.object  = mod->objsize - 1;
  mod->cindex.contour = -1;
  mod->cindex.point   = -1;

  return(0);
}

/*!
 * Removes the object at the given [index] from the model [mod].
 * All contour data is deleted and object views are deleted.  The object array 
 * is packed down and reallocated or freed if there are no more objects.
 * Returns -1 if there is an error.
 */
int imodDeleteObject(Imod *mod, int index)
{

  int i;
  Iobj *obj = NULL;
  Iobj *tobj = NULL;
     
  if (index < 0 || index >= mod->objsize)
    return(-1);
  obj = &(mod->obj[index]);

  mod->cindex.object = index;

  /* Delete all contours in object before we delete object. */
  imodContoursDelete(obj->cont, obj->contsize);
     
  /* Copy objects above deleted object down one. */
  if (index != mod->objsize - 1 )
    for (i = index; i < mod->objsize; i++)
      imodObjectCopy(&(mod->obj[i + 1]), &(mod->obj[i]));

  /* Delete all object views for this object */
  imodObjviewDelete(mod, index);

  if (mod->objsize > 1){

    tobj = (Iobj *)realloc (mod->obj, mod->objsize * sizeof(Iobj));
    if (!tobj)
      return(-1);
    mod->obj = tobj;
    if (index)
      mod->cindex.object   = index - 1;

    mod->objsize--;
    tobj = &(mod->obj[mod->cindex.object]);

    if (mod->cindex.contour >= mod->obj[mod->cindex.object].contsize)
      mod->cindex.contour = mod->obj[mod->cindex.object].contsize - 1;
      
    if ((!tobj->contsize) || (tobj->cont == NULL) || mod->cindex.contour < 0) {
      mod->cindex.point = -1;
      mod->cindex.contour = -1;
    } else if (mod->cindex.point >= tobj->cont[mod->cindex.contour].psize)
      mod->cindex.point  = tobj->cont[mod->cindex.contour].psize  - 1;

  } else {

    /* Delete last object in model */
    free(mod->obj);
    mod->cindex.point  = -1;
    mod->cindex.contour = -1;
    mod->cindex.object = -1;
    mod->objsize = 0;
      
  }
  return(0);
}

/*!
 * Moves an object from index [obOld] to index [obNew] in the model [imod].
 * Returns 1 if memory error.
 */
int imodMoveObject(Imod *imod, int obOld, int obNew)
{
  int idir, ob, iv;
  Iobj object;
  Iobj *objSave = &object;
  Iview *vw;
  Iobjview obvwSave;

  if (obOld == obNew)
    return 0;
  idir = obOld < obNew ? 1 : -1;
  
  /* complete object views */
  if (imodObjviewComplete(imod))
    return 1;
    
  /* Copy object structures */
  imodObjectCopy(&imod->obj[obOld], objSave);
  for (ob = obOld; ob != obNew; ob += idir)
    imodObjectCopy(&imod->obj[ob + idir], &imod->obj[ob]);
  imodObjectCopy(objSave, &imod->obj[obNew]);

  /* Copy object views */
  for (iv = 1; iv < imod->viewsize; iv++) {
    vw = &imod->view[iv];
    obvwSave  = vw->objview[obOld];
    for (ob = obOld; ob != obNew; ob += idir)
      vw->objview[ob] =  vw->objview[ob + idir];
    vw->objview[obNew] = obvwSave;
  }
  return 0;
}

/*!
 * Advances from the current object to the next object in the model [imod],
 * adjusting the index of the current contour and point to stay within bounds
 * or setting them to -1 if appropriate.  Returns new object index or -1
 * if there is no model or objects.
 */
int imodNextObject(Imod *imod)
{
  Iobj  *obj;
  Icont *cont;

  if ((imod == NULL) || (imod->objsize == 0))
    return(-1);
  if (imod->cindex.object >= imod->objsize - 1)
    return(imod->cindex.object);

  ++imod->cindex.object;
  if (imod->cindex.object < 0) imod->cindex.object = 0;
  obj = imodObjectGet(imod);
     
  /* check for contour # out of bounds. */
  if ( imod->cindex.contour >= obj->contsize)
    imod->cindex.contour = obj->contsize - 1;
  cont = imodContourGet(imod);

  /* check for point # out of bounds */
  if ((imod->cindex.contour >= 0) && (cont)){
    if (imod->cindex.point >= cont->psize)
      imod->cindex.point = cont->psize - 1;
    if (cont->psize < 1)
      imod->cindex.point = -1;
  }else
    imod->cindex.point = -1;

  return(imod->cindex.object);
}


/*!
 * Moves from the current object to the previous object in the model [mod],
 * adjusting the index of the current contour and point to stay within bounds
 * or setting them to -1 if appropriate.  Returns new object index or -1
 * if there is no model or objects.
 */
int imodPrevObject(Imod *mod)
{
  Iobj *obj;
  Icont *cont;

  if ((mod == NULL) || (mod->objsize == 0))
    return(-1);
  /* If no object selected or if object is # 0, leave index alone*/
  if (mod->cindex.object <= 0)
    return(mod->cindex.object);

  if (mod->cindex.object > mod->objsize)
    mod->cindex.object = mod->objsize;

  --mod->cindex.object;
  obj = imodObjectGet(mod);
  if ( mod->cindex.contour >= obj->contsize)
    mod->cindex.contour = obj->contsize - 1;
  cont = (Icont *)imodContourGet(mod);

  /* check for point # out of bounds */
  if ((mod->cindex.contour > -1) && (cont)){
    if (mod->cindex.point >= cont->psize)
      mod->cindex.point = cont->psize - 1;
    if (cont->psize < 1)
      mod->cindex.point = -1;
  }else
    mod->cindex.point = -1;

  return(mod->cindex.object);
}

/*!
 * Returns a pointer to the current object in model [imod], or NULL if no legal
 * object is selected.
 */
Iobj *imodObjectGet(Imod  *imod)
{
  if (!imod)
    return(NULL);
  if (imod->cindex.object < 0 || imod->cindex.object >= imod->objsize)
    return( (Iobj *)NULL);
  return( &(imod->obj[imod->cindex.object]));
}

/*!
 * Sets first object in model [imod] as current object and returns pointer to
 * it, or NULL if error.
 */
Iobj *imodObjectGetFirst(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  imodSetIndex(imod, 0, co, pt);
  return(imodObjectGet(imod));
}

/*!
 * Advances the current object index by one in model [imod] and returns pointer
 * to new current object, or NULL if error or if the existing current object is
 * the last one in the model.
 */
Iobj *imodObjectGetNext(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  ob++;
  if (ob >= imod->objsize)
    return(NULL);
  imodSetIndex(imod, ob, co, pt);
  return(imodObjectGet(imod));
}


/*****************************************************************************/
/* Contour-LEVEL FUNCTIONS
 * DOC_SECTION CONTOUR
 */

/*!
 * Adds a new contour to the current object of the model [mod].  It will 
 * inherit surface and open/closed properties from the current contour and
 * become the new current contour.  Returns -1 if error.
 */
int imodNewContour(Imod *mod)
{
  Iobj *obj;
  Icont *cont, *ocont;

  obj = imodObjectGet(mod);

  if (obj == NULL)
    return(-1);

  /* Allocate Memory for new contour. */
  if (obj->contsize == 0)
    cont = (Icont *)malloc(sizeof(Icont));
  else
    cont = (Icont *)realloc(obj->cont, sizeof(Icont) * (obj->contsize + 1));

  if (cont == NULL){
    b3dError(stderr,
            "IMOD: imodNewContour memory Error, "
            "Suggest you save and restart.");
    return(-1);
  }

  /* Update object. */
  ++obj->contsize;
  obj->cont = cont;

  cont = &(obj->cont[obj->contsize -1]);

  /* Default values for new contour */     
  cont->pts = NULL;
  cont->psize = 0;
  cont->flags = 0;
  cont->time = 0;
  cont->surf = 0;
  cont->label = NULL;
  cont->sizes = NULL;
  cont->store = NULL;

  /* Reset current index. */
  ocont = imodContourGet(mod);
  if (ocont) {
    cont->surf = ocont->surf;
    /* DNM 10/24/01: inherent the open flag from previous contour */
    if (ocont->flags & ICONT_OPEN)
      cont->flags = ICONT_OPEN;
  }
  mod->cindex.contour = obj->contsize - 1;
  mod->cindex.point = -1;

  return(0);
}

/*!
 * Deletes the current contour of the model [imod].
 */
void imodDelCurrentContour(Imod *imod)
{
  imodDeleteContour(imod, imod->cindex.contour);
  return;
}

/*!
 * Deletes the contour at [index] in the current object of the model [mod],
 * freeing all point, size, and label data and reducing the size of the contour
 * array.  Sets the current contour and point index to -1 and returns the size
 * of the current object or -1 if error.
 */
int imodDeleteContour(Imod *mod, int index)
{
  Iobj *obj;
  Icont *cont;
  int i;
     
  obj = imodObjectGet(mod);
  if (!obj || index < 0 || index >= obj->contsize)
    return(-1);

  /* If contour has any points, free them. */
  /* DNM: this was &(obj->cont[index].pts) but that seems wrong! */
  cont = &obj->cont[index];
  if ((cont->pts != NULL) && (cont->psize))
    free( cont->pts );

  if (cont->sizes)
    free (cont->sizes);

  /* DNM: need to delete labels if any */
  imodLabelDelete(cont->label);
  ilistDelete(cont->store);
  istoreDeleteContSurf(obj->store, index, 0);
     
  /* Push extra contours into hole */
  if (index != obj->contsize - 1)
    for(i = index; i < (obj->contsize - 1); i++) {
      imodContourCopy(&(obj->cont[i + 1]), &(obj->cont[i]));
    }

  /* Change contour array to new size */
  if (obj->contsize > 1) {
    obj->contsize--;
    obj->cont = (Icont *)realloc(obj->cont, obj->contsize * sizeof(Icont));
     
    /* 3/19/05: Deleted incorrect fixing of contour and point indexes */
      
  } else {
    free(obj->cont);
    obj->cont = NULL;
    obj->contsize = 0;
  }

  /* DMN 9/20/04: clean out labels for non-existing surfaces */
  imodObjectCleanSurf(obj);

  mod->cindex.contour = -1;
  mod->cindex.point   = -1;
  return(obj->contsize);
}

/*!
 * Moves the current contour index back by one in model [mod].  If there
 * is no current contour it selects the last one in the object.  Returns the
 * current contour index or -1 for error.
 */
int imodPrevContour(Imod *mod)
{
  Iobj *obj;

  if (mod == NULL)
    return(-1);

  obj = imodObjectGet(mod);

  /* no object selected. */
  if (!obj){
    mod->cindex.contour = -1;
    return(-1);
  }

  /* If object has no contours, return. */
  if (!obj->contsize)
    return(-1);

  /* if first contour selected exit. If no contour select last one */
  if (mod->cindex.contour == 0)
    return(mod->cindex.contour);
  if (mod->cindex.contour < 0)
    mod->cindex.contour = obj->contsize - 1;
  else
    /* move index to previous contour */
    mod->cindex.contour--;

  /* if point index is to high change it. */
  if (mod->cindex.point > obj->cont[mod->cindex.contour].psize - 1)
    mod->cindex.point = obj->cont[mod->cindex.contour].psize - 1;

  return(mod->cindex.contour);
}

/*!
 * Advances the current contour index forward by one in model [mod].  If there
 * is no current contour it selects the first one in the object.  Returns the
 * current contour index or -1 for error.
 */
int   imodNextContour(Imod *imod)
{
  Iobj *obj;

  if (imod == NULL)
    return(-1);

  obj = imodObjectGet(imod);

  /* if no object selected, forget it. */
  if (!obj)
    return(-1);

  /* If object has no contours, return. */
  if (!obj->contsize)
    return(-1);

  /* if contour is last one, return */
  if (imod->cindex.contour == obj->contsize - 1)
    return(imod->cindex.contour);

  /* if no contour selected, select the first one */
  if (imod->cindex.contour < 0)
    imod->cindex.contour = 0;
  else
    imod->cindex.contour++;

  /* if point index is to high change it. */
  if (imod->cindex.point >= obj->cont[imod->cindex.contour].psize)
    imod->cindex.point = obj->cont[imod->cindex.contour].psize - 1;

  /* DNM 3/19/05: changed return here from contsize to contour index */
  return(imod->cindex.contour);
}
     
/*!
 * Returns a pointer to the current contour in model [imod], or NULL if there
 * is no current contour or the contour index is not legal.
 */
Icont *imodContourGet(Imod *imod)
{
  Iobj *obj;
     
  obj = imodObjectGet(imod);
  if (obj == NULL)
    return((Icont *)NULL);

  /* DNM 3/9/01: need to test for index too high also in case of corrupt
     model */
  if (imod->cindex.contour < 0 || imod->cindex.contour >= obj->contsize)
    return( (Icont *)NULL);
     
  return( &(obj->cont[imod->cindex.contour]));
}

/*!
 * Sets the current contour index to 0 in the current object in model [imod]
 * and returns pointer to first contour, or NULL if there is no contour.
 */
Icont *imodContourGetFirst(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  imodSetIndex(imod, ob, 0, pt);
  return(imodContourGet(imod));
}

/*!
 * Advances to the next contour in the current object of model [imod] and
 * returns pointer to the contour, or NULL if there is none (error or end
 * of object).
 */
Icont *imodContourGetNext(Imod *imod)
{
  int ob, co, pt;
  Iobj *obj;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  obj = imodObjectGet(imod);
  if (!obj) return(NULL);
  if (!obj->contsize) return(NULL);
  co++;
  if (co >= obj->contsize) return(NULL);
  imodSetIndex(imod, ob, co, pt);
  return(imodContourGet(imod));
}


/*****************************************************************************/
/* POINT-LEVEL FUNCTIONS
 * DOC_SECTION POINT
 */

/*!
 * Adds [point] to the end of the current contour in model [imod].
 * Returns new number of points in contour, or 0 for an error.
 */
int imodNewPoint(Imod *imod, Ipoint *point)
{
  Icont *cont = imodContourGet(imod);
  if (!cont)
    return(0);
  imod->cindex.point++;
  return(imodPointAppend(cont, point));
}

/*!
 * Inserts [point] at position [index] into the current contour in model
 * [imod].  Returns new number of points in contour, or 0 for an error.
 */
int imodInsertPoint(Imod *imod, Ipoint *point, int index)
{
  Icont *cont;
  int retval;

  if (!imod)  return 0;
  if (!point) return 0;
  cont = imodContourGet(imod);
  if (!cont)  return(0);
    
  if (index < 0)
    index = 0;

  /* DNM 11/17/04: allow it to specify index of appended point */
  if (index > cont->psize)
    index = cont->psize;

  retval = imodPointAdd(cont, point, index);

  imod->cindex.point = index;
  return retval;
}
     
/*!
 * Deletes the current point in model [imod], or the current contour if it is
 * empty.  Returns size of current contour or -1 for error.
 */
int imodDeletePoint(Imod *imod)
{
  Icont *cont;
  int    index;

  cont = imodContourGet(imod);
  if (!cont || (cont->psize && imod->cindex.point < 0))
    return(-1);

  /* If there are no points, delete the whole contour (12/3/04 swicthed from
     deleting if one point) */
  if (!cont->psize){
    imodDeleteContour(imod, imod->cindex.contour);
    return(0);
  }

  index = imod->cindex.point;

  /* DNM 11/16/04: removed second test on size = 1 that just free points */

  if (index || cont->psize == 1)
    imod->cindex.point = index - 1;
  return(imodPointDelete(cont, index));

}

/*!
 * Moves the current point index back by one in model [mod].  Returns the
 * new current point index or -1 if no object or contour is selected.
 */
int   imodPrevPoint(Imod *mod)
{
  if (mod == NULL)
    return(-1);
  
  if (mod->cindex.object < 0)
    return(-1);
     
  if (mod->cindex.contour < 0)
    return(-1);
  
  if (mod->cindex.point <= 0)
    return(mod->cindex.point);
  
  mod->cindex.point--;
  
  return(mod->cindex.point);
}

/*!
 * Advances the current point index forward by one in model [mod].  Returns the
 * new current point index or -1 if no object or contour is selected.
 */
int   imodNextPoint(Imod *mod)
{

  Icont *cont = NULL;

  cont = imodContourGet(mod);

  if (cont == NULL)
    return(-1);

  mod->cindex.point++;
   
  if ( mod->cindex.point >= cont->psize)
    mod->cindex.point = cont->psize - 1;

  return(mod->cindex.point);
}

/*!
 * Returns a pointer to the current point in model [imod], or NULL if there
 * is no current contour or current point.
 */
Ipoint *imodPointGet(Imod *imod)
{
  Icont *cont;

  if (imod->cindex.point < 0)
    return((Ipoint *)NULL);

  cont = imodContourGet(imod);
  if (!cont)
    return((Ipoint *)NULL);

  if (imod->cindex.point >= cont->psize)
    imod->cindex.point = cont->psize - 1;
  if (imod->cindex.point < 0)
    return((Ipoint *)NULL);

  return( &(cont->pts[imod->cindex.point]));
}

/*!
 * Sets the current point index to 0 in the current contour in model [imod]
 * and returns pointer to first point, or NULL if there is no current contour
 * or point.
 */
Ipoint *imodPointGetFirst(Imod *imod)
{
  int ob, co, pt;

  if (!imod) 
    return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  imodSetIndex(imod, ob, co, 0);
  return(imodPointGet(imod));
}

/*!
 * Advances to the next point in the current contour of model [imod] and
 * returns pointer to the point , or NULL if there is none (error or end
 * of contour).
 */
Ipoint *imodPointGetNext(Imod *imod)
{
  int ob, co, pt;
  Icont *cont;

  if (!imod) 
    return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
     
  cont = imodContourGet(imod);
  if (!cont || !cont->psize)
    return(NULL);

  pt++;
  if (pt >= cont->psize) 
    return(NULL);
  imodSetIndex(imod, ob, co, pt);
  return(imodPointGet(imod));
}

/*****************************************************************************/
/* MORE GENERAL FUNCTIONS
 * END_SECTION
 */

/*!
 * Transforms all contour points in model [imod] with the 3D transform in 
 * [mat].  Returns -1 if error.
 */
int imodTransform(Imod *imod, Imat *mat)
{
  Iobj  *obj;
  Icont *cont;
  Ipoint pnt;
  int ob, co, pt;
     
  if ((!imod) || (!mat)) return -1;
  for(ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
        imodMatTransform(mat, &cont->pts[pt], &pnt);
        cont->pts[pt] = pnt;
      }
    }
  }
  return(0);
}

/* 5/3/12: eliminated unused, redundant model_transform function */

/*!
 * Transforms all contour points in [model] with Z values that round to 
 * [slice].  The 2D transformation in [mat] is applied to X and Y coordinates.
 * Returns 0.
 */
int imodel_transform_slice(Imod *model, float *mat, int slice)
{
  int ob, co, pt;
  Iobj *obj;
  Icont *cont;
  int zval;
  float x, y;
     
  for(ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
        zval = (int)floor(cont->pts[pt].z + 0.5);
        if (zval == slice){
          x = cont->pts[pt].x;
          y = cont->pts[pt].y;
          cont->pts[pt].x =
            (x * mat[0]) + (y * mat[3]) + mat[6];
          cont->pts[pt].y =
            (x * mat[1]) + (y * mat[4]) + mat[7];
        }
      }
    }
  }
  return(0);
}

/* 9/21/07: Removed model locking code (!?) */

/*!
 * Cleans model [mod] by removing empty contours, removing empty objects if
 * [keepEmptyObjs] is 0, constraining points within the model xmax, ymax, and
 * zmax values, and making all points have the same Z value as the first point
 * for non-wild closed contours.  Returns 0.
 */
int imodel_model_clean(Imod *mod, int keepEmptyObjs)
{
  int ob, co, pt;
  Iobj *obj;
  Icont *cont;

  /* push all points inside of boundries. */
  for(ob = 0 ; ob < mod->objsize; ob++){
    obj = &(mod->obj[ob]);
    /*    printf("ob = %d, size = %d\n", ob, mod->objsize); */
    if (!obj->contsize && !obj->meshsize && !keepEmptyObjs){
      mod->cindex.object = ob;
      imodDeleteObject(mod, ob);
      ob--;
      continue;
    }
           
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      /*         printf("ob %d: co = %d, size = %d\n", ob, co, obj->contsize); */
      if(!cont->psize){
        mod->cindex.object = ob;
        mod->cindex.contour = co;
        imodDeleteContour(mod, co);
        co--;
        continue;
      }
      for(pt = 0; pt < cont->psize; pt++){
            
        if (cont->pts[pt].x > mod->xmax)
          cont->pts[pt].x = mod->xmax;
        if (cont->pts[pt].y > mod->ymax)
          cont->pts[pt].y = mod->ymax;
        if (mod->zmax)
          if (cont->pts[pt].z > mod->zmax)
            cont->pts[pt].z = mod->zmax;

        if (!( (obj->flags & IMOD_OBJFLAG_OPEN)
               || (obj->flags & IMOD_OBJFLAG_WILD)
               || (obj->flags & IMOD_OBJFLAG_SCAT)))
          cont->pts[pt].z = cont->pts[0].z;
      }
    }
  }
  return(0);
}

/*!
 * Returns a string (e.g., "nm") for the pixel size units of model [mod].
 */
char *imodUnits(Imod *mod)
{
  int units = mod->units;
  char *retval = NULL;

  switch(units){
  case IMOD_UNIT_PIXEL:
    retval ="pixels";
    break;
  case IMOD_UNIT_KILO:
    retval = "km";
    break;
  case IMOD_UNIT_METER:
    retval = "m";
    break;
  case IMOD_UNIT_CM:
    retval = "cm";
    break;
  case IMOD_UNIT_MM:
    retval = "mm";
    break;
  case IMOD_UNIT_UM:
    retval = "um";
    break;
  case IMOD_UNIT_NM:
    retval = "nm";
    break;
  case IMOD_UNIT_ANGSTROM:
    retval = "A";
    break;
  case IMOD_UNIT_PM:
    retval = "pm";
    break;
  default:
    retval = "unknown units";
    break;
  }
  return(retval);
}

/*
  int imod_obj_nearest\(Iobj *obj, Iindex *ind, Ipoint *pt)
  {
  return(0);
  }
  Ipoint *imodNearestPoint(Imod *imod, Ipoint *pt)
  {
  Ipoint *spnt = NULL;
  Iindex index;
  int i;
  int distance = -1;
  int temp_distance;

  if ((!imod)||(!pt)) return(NULL);

  for (i = 0; i < imod->objsize; i++){
  index.object = i;
  temp_distance = imod_obj_nearest
  (&(imod->obj[i]), &index , pt);
  if (temp_distance == -1)
  continue;
  if (distance == -1){
  distance      = temp_distance;
  imod->cindex.object  = index.object;
  imod->cindex.contour = index.contour;
  imod->cindex.point   = index.point;
  spnt = imodPointGet(imod);
  }
  if (distance > temp_distance){
  distance      = temp_distance;
  imod->cindex.object  = index.object;
  imod->cindex.contour = index.contour;
  imod->cindex.point  = index.point;
  spnt = imodPointGet(imod);
  }
  }
  return(spnt);
  }

*/

/*!
 * Returns the maximum time (type) value of any contour in model [imod].
 */
int imodGetMaxTime(Imod *imod)
{
  int maxtime = 0;
  int ob,co;
  Iobj *obj;
  if (!imod)
    return maxtime;
  for(ob = 0; ob < imod->objsize; ob++){
    obj= &imod->obj[ob];
    for(co = 0; co < obj->contsize; co++){
      if (obj->cont[co].time > maxtime)
        maxtime = obj->cont[co].time;
    }
  }
  return maxtime;
}

/*!
 * Computes a checksum from the coordinates and most other features of model 
 * [mod] and returns the value.
 */
int imodChecksum(Imod *imod)
{
  int ob, co, isum, i;
  Iview *view;
  Iobjview *obv;
  IclipPlanes *clips;
  SlicerAngles *slanp;
  double sum = 0.;
  double osum;

  sum += imod->zscale;
  sum += imod->pixsize;
  sum += imod->objsize;
  sum += imod->blacklevel;
  sum += imod->whitelevel;
  sum += imod->res;
  sum += imod->thresh;
  sum += imod->units;
  sum += imod->pixsize;
  sum += imod->viewsize;
  sum += imod->flags & ~IMODF_FLIPYZ & ~IMODF_NEW_TO_3DMOD & ~IMODF_ROT90X;

  for (i = 0; i < ilistSize(imod->slicerAng); i++) {
    slanp = (SlicerAngles *)ilistItem(imod->slicerAng, i);
    sum += slanp->center.x + slanp->center.y + slanp->center.z + 
      slanp->angles[0] + slanp->angles[0] + slanp->angles[0] + 
      strlen(slanp->label);
  }
  sum += objGroupListChecksum(imod->groupList);

  for (ob = 0; ob < imod->objsize; ob++){
    sum += imodObjectChecksum(&(imod->obj[ob]), ob);
  }

  /* Add properties of views.  Do not add rad and trans because they are
     changed just by opening model view window.  
     34/27/04: Add the rotation angle only for added views, not for the basic
     view */
  for (co = 0; co < imod->viewsize; co++) {
    view = &imod->view[co];
    sum += view->fovy + view->cnear + view->cfar;
    if (co)
      sum += view->rot.x + view->rot.y + view->rot.z; 
    sum += view->scale.x + view->scale.y + view->scale.z; 
    sum += view->world + view->dcstart + view->dcend + view->plax;
    sum += view->lightx + view->lighty;
    if (co) {
      for (ob = 0; ob < view->objvsize; ob++) {
        obv = &view->objview[ob];
        osum = ob;
        osum += obv->red + obv->green + obv->blue;
        osum += obv->flags;
        osum += obv->pdrawsize;
        osum += obv->linewidth;
        osum += obv->trans;
        osum += obv->ambient + obv->diffuse + obv->specular + obv->shininess;
        clips = &obv->clips;
        osum += clips->count + clips->flags + clips->trans;
        osum += clips->plane + obv->mat2;
        for (i = 0; i < clips->count; i++) {
          osum += clips->normal[i].x + clips->normal[i].y + 
            clips->normal[i].z;
          osum += clips->point[i].x + clips->point[i].y + 
            clips->point[i].z;
        }
        osum += obv->fillred + obv->fillgreen + obv->fillblue + obv->quality;
        osum += obv->valblack + obv->valwhite + obv->matflags2 + obv->mat3b3;
        sum += osum;
      }
    }
  }
  
  /* This will catch fractional values - not perfect but probably good */
  isum = sum / 1000000.;
  isum = (int)(1000. * (sum - 1000000. * isum));
  /* fprintf(stderr, "checksum = %d\n", sum, isum); */
  return (isum);
}

/*!
 * Fixes the surface size information for all objects in model [imod] by
 * determining the maximum surface number for the contours in each object.
 */
void imodCleanSurf(Imod *imod)
{
  Iobj *obj;
  Icont *cont;
  int ob, co, maxsurf;
     
  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    maxsurf = 0;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (cont->surf > maxsurf)
        maxsurf = cont->surf;
    }
    obj->surfsize = maxsurf;
  }
}

/* DNM 11/15/04: removed virtual in call */

/*!
 * Flip the model [imod] by exchanging Y and Z in contours and meshes.
 */
void  imodFlipYZ(Imod *imod)
{
  Iobj  *obj;
  Icont *cont;
  Imesh *mesh;
  float tmp;
  int    ob, co, pt, i;

  /* fFip contours and meshes */
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &(imod->obj[ob]);
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++) {
        B3DSWAP(cont->pts[pt].y, cont->pts[pt].z, tmp);
      }
    }

    flipClips(&obj->clips);
    for (co = 0; co < obj->meshsize; co++) {
      mesh = &(obj->mesh[co]);
      for (pt = 0; pt < mesh->vsize; pt++) {
        B3DSWAP(mesh->vert[pt].y, mesh->vert[pt].z, tmp);
      }
    }
  }

  /* View clip planes */
  for (i = 0; i < imod->viewsize; i++) {
    flipClips(&imod->view[i].clips);
    for (ob = 0; ob < imod->view[i].objvsize; ob++) {
      flipClips(&imod->view[i].objview[ob].clips);
    }
  }

  B3DSWAP(imod->ymax, imod->zmax, i);
}

/*!
 * Rotates the model in [imod] by 90 degrees around the X axis and shifts along the
 * appropriate axis to retain positive values.  The rotation is by -90 degrees if
 * [toNative] is 0, corresponding to the Y-Z flip in 3dmod, and by +90 degrees if
 * [toNative] is non-zero, corresponding to flipping back to native orientation.
 * The incoming {ymax} and {zmax} values will be swapped and must be correct for the
 * current model coordinates.
 */
void imodRot90X(Imod *imod, int toNative)
{
  Iobj  *obj;
  Icont *cont;
  Imesh *mesh;
  float tmp;
  int    ob, co, pt, i;
  float yconst, yfac, zconst, zfac;

  /* Set up the constants and multipliers from getting between Y and Z */
  if (toNative) {
    yconst = 0.;
    yfac = 1.;
    zconst = imod->zmax - 1.;
    zfac = -1.;
  } else {
    zconst = 0.;
    zfac = 1.;
    yconst = imod->ymax - 1.;
    yfac = -1.;
  }
  
  /* Rotate the contours */
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &(imod->obj[ob]);
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++) {
        tmp = yconst + yfac * cont->pts[pt].y;
        cont->pts[pt].y = zconst + zfac * cont->pts[pt].z;
        cont->pts[pt].z = tmp;
      }
    }

    /* Rotate object clip planes and meshes */
    rotClips(yconst, yfac, zconst, zfac, &obj->clips);
    for (co = 0; co < obj->meshsize; co++) {
      mesh = &(obj->mesh[co]);
      for (pt = 0; pt < mesh->vsize; pt += 2) {
        tmp = yconst + yfac * mesh->vert[pt].y;
        mesh->vert[pt].y = zconst + zfac * mesh->vert[pt].z;
        mesh->vert[pt].z = tmp;
        tmp = yfac * mesh->vert[pt + 1].y;
        mesh->vert[pt + 1].y = zfac * mesh->vert[pt + 1].z;
        mesh->vert[pt + 1].z = tmp;
      }
    }
  }

  /* Rotate the view clip planes */
  for (i = 0; i < imod->viewsize; i++) {
    rotClips(yconst, yfac, zconst, zfac, &imod->view[i].clips);
    for (ob = 0; ob < imod->view[i].objvsize; ob++) {
      rotClips(yconst, yfac, zconst, zfac, &imod->view[i].objview[ob].clips);
    }
  }

  B3DSWAP(imod->ymax, imod->zmax, i);
}

/*!
 * Inverts the model in [imod] in the Z direction and shifts it in Z
 * to retain positive values.  The incoming model {zmax} value must be correct for the
 * current model coordinates.
 */
void imodInvertZ(Imod *imod)
{
  Iobj  *obj;
  Icont *cont;
  Imesh *mesh;
  int    ob, co, pt, i;
  float zconst = imod->zmax - 1.;
  
  /* Invert ocontours and meshes */
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &(imod->obj[ob]);
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++)
        cont->pts[pt].z = zconst - cont->pts[pt].z;
    }

    invertClips(zconst, &obj->clips);
    for (co = 0; co < obj->meshsize; co++) {
      mesh = &(obj->mesh[co]);
      for (pt = 0; pt < mesh->vsize; pt += 2) {
        mesh->vert[pt].z = zconst - mesh->vert[pt].z;
        mesh->vert[pt + 1].z = -mesh->vert[pt + 1].z;
      }
    }
  }

  /* Invert view clip planes */
  for (i = 0; i < imod->viewsize; i++) {
    invertClips(zconst, &imod->view[i].clips);
    for (ob = 0; ob < imod->view[i].objvsize; ob++)
      invertClips(zconst, &imod->view[i].objview[ob].clips);
  }
}


/*!
 * Sets the @@IrefImage structure@ {refImage} in [imod] with the image 
 * coordinate information in the MRC header [hdata].  Returns 1 for memory 
 * error allocating the structure.
 */
int imodSetRefImage(Imod *imod, MrcHeader *hdata)
{
  IrefImage *ref;
  if (!imod->refImage) {
    imod->refImage = (IrefImage *)malloc(sizeof(IrefImage));
    if (!imod->refImage)
      return 1;
  }
  ref = imod->refImage;
  ref->ctrans.x = hdata->xorg;
  ref->ctrans.y = hdata->yorg;
  ref->ctrans.z = hdata->zorg;
  ref->crot.x = hdata->tiltangles[3];
  ref->crot.y = hdata->tiltangles[4];
  ref->crot.z = hdata->tiltangles[5];
  ref->cscale.x = ref->cscale.y = ref->cscale.z = 1.;
  if (hdata->xlen && hdata->mx)
    ref->cscale.x = hdata->xlen/(float)hdata->mx;
  if (hdata->ylen && hdata->my)
    ref->cscale.y = hdata->ylen/(float)hdata->my;
  if (hdata->xlen && hdata->mz)
    ref->cscale.z = hdata->zlen/(float)hdata->mz;
  ref->oscale.x = ref->oscale.y = ref->oscale.z = 1.;
  ref->orot.x = ref->orot.y = ref->orot.z = 0.;
  ref->otrans.x = ref->otrans.y = ref->otrans.z = 0.;
  return 0;
}

/*!
 * Shifts the model [imod] back to full volume coordinates if it was saved after being 
 * loaded on a subset of the image whose header is in [hdata], and also shifts it to the
 * coordinates of a subset currently being loaded, using the {xmin}, {ymin}, and {zmin}
 * elements in [li], if [li] is not NULL.
 */
void imodTransForSubsetLoad(Imod *imod, MrcHeader *hdata, IloadInfo *li)
{
  int ob, co, pt;
  Ipoint *pts;
  float xload = 0., yload = 0., zload = 0.;
  IrefImage *ref = imod->refImage;
  
  if (ref && ref->cscale.x && ref->cscale.y && ref->cscale.z) {
    xload = (hdata->xorg - ref->ctrans.x) / ref->cscale.x;
    yload = (hdata->yorg - ref->ctrans.y) / ref->cscale.y;
    zload = (hdata->zorg - ref->ctrans.z) / ref->cscale.z;
  }
  if (li) {
    xload -= li->xmin;
    yload -= li->ymin;
    zload -= li->zmin;
  }
  if (xload || yload || zload) {
    for (ob = 0; ob < imod->objsize; ob++) {
      for (co = 0; co < imod->obj[ob].contsize; co++) {
        pts = imod->obj[ob].cont[co].pts;
        for (pt = 0; pt < imod->obj[ob].cont[co].psize; pt++) {
          pts[pt].x += xload;
          pts[pt].y += yload;
          pts[pt].z += zload;
        }
      }
    }
  }
}

/*!
 * Transforms the model in [imod] according to the old and current shift, scale,
 * and rotation in the @@IrefImage structure@ [iref], with the additional 
 * scaling required for binning in each dimension specified in [binScale].
 * Returns 1 for memory errors.
 */
int imodTransFromRefImage(Imod *imod, IrefImage *iref, Ipoint binScale)
{
  Ipoint pnt;
  Imat  *mat, *matNorm, *matClip;

  /* Before any transforming, unflip a flipped model */
  if (imod->flags & IMODF_FLIPYZ) {
    imodFlipYZ(imod);
    imod->flags &= ~IMODF_FLIPYZ;
  }

  /* First transform to "absolute" image coords using old reference 
     image data */
  /* Compute separate matrices for point and clip plane normal transforms */
  mat = imodMatNew(3);
  matClip = imodMatNew(3);
  matNorm = imodMatNew(3);
  if (!mat || !matClip || !matNorm)
    return 1;

  imodMatScale(mat, &iref->oscale);
  pnt.x = 1. / iref->oscale.x;
  pnt.y = 1. / iref->oscale.y;
  pnt.z = 1. / iref->oscale.z;
  imodMatScale(matClip, &pnt);
  
  pnt.x =  - iref->otrans.x;
  pnt.y =  - iref->otrans.y;
  pnt.z =  - iref->otrans.z;
  imodMatTrans(mat, &pnt);


  /* DNM 11/5/98: because tilt angles were not properly set into the
     model data when IrefImage was created, do rotations
     only if the flag is set that tilts have been stored properly */
  /* DNM 1/3/04: and skip it if there is no rotation change */
  if (imod->flags & IMODF_TILTOK && (iref->crot.x != iref->orot.x || 
                                     iref->crot.y != iref->orot.y || 
                                     iref->crot.z != iref->orot.z )) {
    imodMatRot(mat, - iref->orot.x, b3dX);
    imodMatRot(mat, - iref->orot.y, b3dY);
    imodMatRot(mat, - iref->orot.z, b3dZ);

    imodMatRot(matClip, - iref->orot.x, b3dX);
    imodMatRot(matClip, - iref->orot.y, b3dY);
    imodMatRot(matClip, - iref->orot.z, b3dZ);

    /* Next transform from these "absolute" coords to new reference
       image coords */

    imodMatRot(mat, iref->crot.z, b3dZ);
    imodMatRot(mat, iref->crot.y, b3dY);
    imodMatRot(mat, iref->crot.x, b3dX);

    imodMatRot(matClip, iref->crot.z, b3dZ);
    imodMatRot(matClip, iref->crot.y, b3dY);
    imodMatRot(matClip, iref->crot.x, b3dX);
  }

  imodMatTrans(mat, &iref->ctrans);

  pnt.x = 1. / (iref->cscale.x * binScale.x);
  pnt.y = 1. / (iref->cscale.y * binScale.y);
  pnt.z = 1. / (iref->cscale.z * binScale.z);
  imodMatScale(mat, &pnt);

  /* Mesh normals scaling does not need to include the binning scale because
   they already include Z-scaling and the model display will be adjusted to
   display with proper Z-scaling including the binning difference
   But clip normals do need the bin scaling included */
  imodMatCopy(matClip, matNorm);
  imodMatScale(matNorm, &iref->cscale);
  pnt.x = iref->cscale.x * binScale.x;
  pnt.y = iref->cscale.y * binScale.y;
  pnt.z = iref->cscale.z * binScale.z;
  imodMatScale(matClip, &pnt);

  imodTransFromMats(imod, mat, matNorm, matClip);
          
  imodMatDelete(mat);
  imodMatDelete(matClip);
  imodMatDelete(matNorm);
  return 0;
}

/*!
 * Transforms the model in [imod] given three matrices: [mat] for transforming
 * points, [matNorm] for transforming the mesh normals, and [matClip] for
 * transforming clip plane normals.
 */
void imodTransFromMats(Imod *imod, Imat *mat, Imat *matNorm, Imat *matClip)
{
  Iobj  *obj;
  Icont *cont;
  Ipoint pnt;
  int    ob, co, pt;
  int    me, i;
  Imesh *mesh;

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);

    /* Transform points in contours */
    for (co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++){
        imodMatTransform(mat, &cont->pts[pt], &pnt);
        cont->pts[pt] = pnt;
      }
    }

    imodClipsTrans(&obj->clips, mat, matClip);
    
    /* Transform the mesh points and the normals */
    for (me = 0; me < obj->meshsize; me++) {
      mesh = &obj->mesh[me];
      if (!mesh || !mesh->vsize)
        continue;
      for (i = 0; i < mesh->vsize; i += 2){
        imodMatTransform(mat, &mesh->vert[i], &pnt);
        mesh->vert[i] = pnt;
        imodMatTransform(matNorm, &mesh->vert[i + 1], &pnt);
        imodPointNormalize(&pnt);
        mesh->vert[i + 1] = pnt;
      }
    }
  }

  /* Now transform clip points in views and object views */
  for (i = 0; i < imod->viewsize; i++) {
    imodClipsTrans(&imod->view[i].clips, mat, matClip);
    for (ob = 0; ob < imod->view[i].objvsize; ob++) {
      imodClipsTrans(&imod->view[i].objview[ob].clips, mat, matClip);
    }
  }
}

/* Exchange floats */
static void exchangef(float *a, float *b)
{
  float tmp = *a;
  *a = *b;
  *b = tmp;
}

/*!
 * Transforms [model] with the 3D matrix in [mat], which includes translations. ^
 * [normMat] is a 3D matrix for normal transformations, or NULL if none are needed.  ^
 * [newCen] are the center coordinates of the volume being transformed to; the center
 * coordinates for the transformation are taken from the {xmax}, {ymax}, and {zmax} 
 * members of the model structure.  ^
 * [zscale] is the Z scale factor, or 1 to not apply any Z scaling.
 * [doflip] should be nonzero if the model is in Y-Z flipped coordinates
 */
void imodTransModel3D(Imod *model, Imat *mat, Imat *normMat, Ipoint newCen, float zscale,
                     int doflip)
{
  Imat *matWork = imodMatNew(3);
  Imat *matWork2 = imodMatNew(3);
  Imat *matUse = imodMatNew(3);
  Imat *clipMat = imodMatNew(3);
  Ipoint oldCen, tmpPt;
  oldCen.x = -model->xmax * 0.5f;
  oldCen.y = -model->ymax * 0.5f;
  oldCen.z = -model->zmax * 0.5f;

  /* If data are flipped, exchange Y and Z columns then Y and Z rows */
  if (doflip) {
    exchangef(&mat->data[4], &mat->data[8]);
    exchangef(&mat->data[1], &mat->data[2]);
    exchangef(&mat->data[6], &mat->data[9]);
    exchangef(&mat->data[5], &mat->data[10]);
    exchangef(&mat->data[13], &mat->data[14]);
    if (normMat) {
      exchangef(&normMat->data[4], &normMat->data[8]);
      exchangef(&normMat->data[1], &normMat->data[2]);
      exchangef(&normMat->data[6], &normMat->data[9]);
      exchangef(&normMat->data[5], &normMat->data[10]);
      exchangef(&normMat->data[13], &normMat->data[14]);
    }
  }

  /* Compute a transformation by translating to origin, applying the given
     transform, then translating back to new center.
     Apply the zscale after translating, then de-scale after applying the
     transform. */
  imodMatTrans(matWork, &oldCen);
  tmpPt.x = 1.;
  tmpPt.y = 1.;
  tmpPt.z = zscale;
  imodMatScale(matWork, &tmpPt);
  imodMatMult(matWork, mat, matUse);
  tmpPt.z = 1. / zscale;
  imodMatScale(matUse, &tmpPt);
  imodMatTrans(matUse, &newCen);

  /* If no normal transform supplied, set up transform for normals as copy of 
     original transform, no shifts, then take the inverse and transpose it. */
  if (!normMat) {
    imodMatCopy(mat, matWork);
    matWork->data[12] = 0.;
    matWork->data[13] = 0.;
    matWork->data[14] = 0.;
    matWork->data[15] = 1.;
    normMat = imodMatInverse(matWork);
    exchangef(&normMat->data[1], &normMat->data[4]);
    exchangef(&normMat->data[2], &normMat->data[8]);
    exchangef(&normMat->data[6], &normMat->data[9]);
  }

  /* The mesh normals already contain the Z scaling, but the clip normals 
     require a matrix that is prescaled by 1/zscale and post-scaled by 
     z-scale */
  imodMatScale(matWork2, &tmpPt);
  imodMatMult(matWork2, normMat, clipMat);
  tmpPt.z = zscale;
  imodMatScale(clipMat, &tmpPt);

  imodTransFromMats(model, matUse, normMat, clipMat);
}

/* Flip clipping planes */
static void flipClips(IclipPlanes *clips)
{
  int i;
  float tmp;
  for (i = 0; i < clips->count; i++) {
    B3DSWAP(clips->point[i].y, clips->point[i].z, tmp);
    B3DSWAP(clips->normal[i].y, clips->normal[i].z, tmp);
  }
}

/* Rotate clipping planes by 90 with given factors */
static void rotClips(float yconst, float yfac, float zconst, float zfac, 
                     IclipPlanes *clips)
{
  int i;
  float tmp;

  /* Clipping plane point is the negative of an actual point so need to take negative
     before and after */
  for (i = 0; i < clips->count; i++) {
    tmp = -yconst + yfac * clips->point[i].y;
    clips->point[i].y = -zconst + zfac * clips->point[i].z;
    clips->point[i].z = tmp;
    tmp = yfac * clips->normal[i].y;
    clips->normal[i].y = zfac * clips->normal[i].z;
    clips->normal[i].z = tmp;
  }
}

/* Invert clipping planes in Z */
static void invertClips(float zconst, IclipPlanes *clips)
{
  int i;
  for (i = 0; i < clips->count; i++) {
    clips->point[i].z = -zconst - clips->point[i].z;
    clips->normal[i].z = - clips->normal[i].z;
  }
}


/*! Get the number of objects in model [imod] (not the maximum object index).
 */
int   imodGetMaxObject(Imod *imod) 
{ return(imod->objsize); }

/*! Get the Z scale in model [imod] */
float imodGetZScale(Imod *imod)
{ return(imod->zscale); }

/*! Get the pixel size in model [imod] */
float imodGetPixelSize(Imod *imod)
{ return(imod->pixsize); }

/*! Get a pointer to the filename of model [imod] */
const char *imodGetFilename(Imod *imod)
{ return imod->fileName; }

/*! Returns non-zero if model [imod] is flipped */
int   imodGetFlipped(Imod *imod) 
{ return(imod->flags & IMODF_FLIPYZ); }

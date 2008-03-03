/*
 *  imodel.c -- Library funcions for handling model structures.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include <math.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"
#include "objgroup.h"

static void flipClips(IclipPlanes *clips);

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
  model->flags = 0;
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
 * Returns the maximum coordinates of the model [imod] in [pnt], or -1,-1,-1
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

/*!
 * Returns the minimum coordinates of the model [imod] in [pnt], or -1,-1,-1
 * if there are no points
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
 * and [max], or -1,-1,-1 if there are no points 
 */
void imodGetBoundingBox(Imod *imod, Ipoint *min, Ipoint *max)
{
  int ob, co, pt;
  Iobj *obj;
  Icont *cont;
  int gotOne = 0;

  min->x = min->y = min->z = -1.;
  max->x = max->y = max->z = -1.;
     
  /* Get first point */
  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (cont->psize) {
        min->x = max->x = cont->pts->x;
        min->y = max->y = cont->pts->y;
        min->z = max->z = cont->pts->z;
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
        if (cont->pts[pt].x < min->x)
          min->x = cont->pts[pt].x;
        if (cont->pts[pt].y < min->y)
          min->y = cont->pts[pt].y;
        if (cont->pts[pt].z < min->z)
          min->z = cont->pts[pt].z;
        if (cont->pts[pt].x > max->x)
          max->x = cont->pts[pt].x;
        if (cont->pts[pt].y > max->y)
          max->y = cont->pts[pt].y;
        if (cont->pts[pt].z > max->z)
          max->z = cont->pts[pt].z;
      }
    }
  }
  return;
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
    0.0, 1.0, 0.0,   /* Green       */
    0.0, 1.0, 1.0,   /* Cyan        */
    1.0, 0.0, 1.0,   /* Magenta     */
    1.0, 1.0, 0.0,   /* Yellow      */
    0.0, 0.0, 1.0,   /* Blue        */
    1.0, 0.0, 0.0,   /* Red         */
    0.0, 1.0, 0.5,
    0.2, 0.2, 0.8,
    0.8, 0.2, 0.2,
    0.9, 0.6, 0.4,
    0.6, 0.4, 0.9,
    0.1, 0.6, 0.4,
    0.6, 0.1, 0.4,
    0.2, 0.6, 0.8,
    1.0, 0.5, 0.0,
    0.4, 0.6, 0.1,
    0.1, 0.1, 0.6,
    0.9, 0.9, 0.4,
    0.9, 0.4, 0.6,
    0.4, 0.9, 0.9,
    0.6, 0.2, 0.2,
    0.2, 0.8, 0.6,
    0.4, 0.6, 0.9,
    0.1, 0.6, 0.1,
    0.8, 0.5, 0.2,
    1.0, 0.0, 0.5,
    0.0, 0.5, 1.0,
    0.6, 0.2, 0.8,
    0.5, 1.0, 0.0,
    0.1, 0.4, 0.6,
    0.6, 0.4, 0.1,
    0.8, 0.2, 0.6,
    0.4, 0.1, 0.6,
    0.2, 0.8, 0.2,
    0.9, 0.4, 0.9
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
      
    if ((!tobj->contsize) || (tobj->cont == NULL)){
      mod->cindex.point = -1;
      mod->cindex.contour = -1;
    }
    else
      if (mod->cindex.point >= tobj->cont[mod->cindex.contour].psize)
        mod->cindex.point  = tobj->cont[mod->cindex.contour].psize  - 1;

  } else{

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

/* Transforms the x,y,z value in the transform structure - unused 3/18/05 */
int imodel_transform(struct Mod_Transform *tr)
{
  double cosx, cosy, cosz;
  double sinx, siny, sinz;
  float x, y, z;
  double rad = 0.017453293;

  sinx = sin(tr->xrot * rad);
  siny = sin(tr->yrot * rad);
  sinz = sin(tr->zrot * rad);
  cosx = cos(tr->xrot * rad);
  cosy = cos(tr->yrot * rad);
  cosz = cos(tr->zrot * rad);

  /* translate */
  x = tr->x + tr->xtrans;
  y = tr->y + tr->ytrans;
  z = tr->z + tr->ztrans;
     
  /* scale */
  x *= tr->xscale;
  y *= tr->yscale;
  z *= tr->zscale;

  /* rotate x */
  tr->xout = x;
  tr->yout = (y * cosx) - (z * sinx);
  tr->zout = (y * sinx) + (z * cosx);
  x = tr->xout;
  y = tr->yout;
  z = tr->zout;

  /* rotate y */
  tr->xout = (x * cosy) + (z * siny);
  tr->yout = y;
  tr->zout = (z * cosy) - (x * siny);
  x = tr->xout;
  y = tr->yout;
  z = tr->zout;

  /* rotate z */
  tr->xout = (x * cosz) - (y * sinz);
  tr->yout = (x * sinz) + (y * cosz);
  tr->zout = z;
     
  return(0);
}

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
    if (!obj->contsize && !keepEmptyObjs){
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
  int ob, co, pt, isum, i;
  Iobj *obj;
  Icont *cont;
  Iview *view;
  IclipPlanes *clips;
  Iobjview *obv;
  SlicerAngles *slanp;
  double sum = 0.;
  double osum, psum;

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
  sum += imod->flags & ~IMODF_FLIPYZ;

  for (i = 0; i < ilistSize(imod->slicerAng); i++) {
    slanp = (SlicerAngles *)ilistItem(imod->slicerAng, i);
    sum += slanp->center.x + slanp->center.y + slanp->center.z + 
      slanp->angles[0] + slanp->angles[0] + slanp->angles[0] + 
      strlen(slanp->label);
  }
  sum += objGroupListChecksum(imod->groupList);

  /* DNM: add # of contours, # of points, and point sizes */

  for (ob = 0; ob < imod->objsize; ob++){
    osum = ob;
    psum = 0.;
    obj = &(imod->obj[ob]);
    osum += obj->red + obj->green + obj->blue;
    osum += obj->flags;
    osum += obj->pdrawsize;
    osum += obj->symbol;
    osum += obj->symsize;
    osum += obj->linewidth2;
    osum += obj->linewidth;
    osum += obj->symflags;
    osum += obj->trans;     
    osum += obj->contsize;      
    osum += obj->ambient + obj->diffuse + obj->specular + obj->shininess;
    clips = &obj->clips;
    osum += clips->count + clips->flags + clips->trans;
    osum += clips->plane + obj->mat2;
    for (i = 0; i < clips->count; i++) {
      osum += clips->normal[i].x + clips->normal[i].y + 
        clips->normal[i].z;
      osum += clips->point[i].x + clips->point[i].y + 
        clips->point[i].z;
    }
    osum += obj->fillred + obj->fillgreen + obj->fillblue + obj->quality;
    osum += obj->valblack + obj->valwhite + obj->matflags2 + obj->mat3b3;
    osum += istoreChecksum(obj->store);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      psum += cont->surf;
      psum += cont->psize;
      for(pt = 0; pt < cont->psize; pt++){
        psum += cont->pts[pt].x;
        psum += cont->pts[pt].y;
        psum += cont->pts[pt].z;
      }
      psum += istoreChecksum(cont->store);
      if (cont->sizes)
        for(pt = 0; pt < cont->psize; pt++)
          psum += cont->sizes[pt];
    }
   
    sum += osum + psum;
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
  return;
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

  for(ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
        tmp = cont->pts[pt].y;
        cont->pts[pt].y = cont->pts[pt].z;
        cont->pts[pt].z = tmp;
      }
    }

    flipClips(&obj->clips);
    for(co = 0; co < obj->meshsize; co++){
      mesh = &(obj->mesh[co]);
      for(pt = 0; pt < mesh->vsize; pt++){
        tmp = mesh->vert[pt].y;
        mesh->vert[pt].y = mesh->vert[pt].z;
        mesh->vert[pt].z = tmp;
      }
    }
  }

  for (i = 0; i < imod->viewsize; i++) {
    flipClips(&imod->view[i].clips);
    for (ob = 0; ob < imod->view[i].objvsize; ob++) {
      flipClips(&imod->view[i].objview[ob].clips);
    }
  }

  tmp = imod->ymax;
  imod->ymax = imod->zmax;
  imod->zmax = tmp;
  return;
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
  return 0;
}

/*!
 * Transforms the model in [imod] according the old and current shift, scale,
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

  // Mesh normals scaling does not need to include the binning scale because
  // they already include Z-scaling and the model display will be adjusted to
  // display with proper Z-scaling including the binning difference
  // But clip normals do need the bin scaling included
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
  IclipPlanes *clips;

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

/* Flip clipping planes */
static void flipClips(IclipPlanes *clips)
{
  int i;
  float tmp;
  for (i = 0; i < clips->count; i++) {
    tmp = clips->point[i].y;
    clips->point[i].y = clips->point[i].z;
    clips->point[i].z = tmp;
    tmp = clips->normal[i].y;
    clips->normal[i].y = clips->normal[i].z;
    clips->normal[i].z = tmp;
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

/*! Returns non-zero if model [imod] is flipped */
int   imodGetFlipped(Imod *imod) 
{ return(imod->flags & IMODF_FLIPYZ); }

/*
$Log$
Revision 3.30  2008/01/27 06:19:51  mast
Changes for object groups

Revision 3.29  2007/09/25 15:44:47  mast
Added function to set refimage from MRC header

Revision 3.28  2007/09/22 00:07:01  mast
Removed model locking code, changed mat3b2 to matflags2

Revision 3.27  2007/05/25 05:18:26  mast
Changes for slicer angle storage

Revision 3.26  2006/09/13 02:42:38  mast
Clarify documentation

Revision 3.25  2006/09/12 15:24:44  mast
Handled member renames

Revision 3.24  2006/08/31 21:11:29  mast
Changed mat1 and mt3 to real names

Revision 3.23  2005/10/16 20:26:04  mast
Split transformation function into two

Revision 3.22  2005/10/14 22:45:52  mast
Moved clip transformation to iplane.c

Revision 3.21  2005/10/13 20:04:00  mast
Added model transformation function from 3dmod, and proper flipping of
clipping planes

Revision 3.20  2005/06/29 05:35:32  mast
Changed a store function call

Revision 3.19  2005/06/26 19:21:09  mast
Added storage handling to contour deletion routine

Revision 3.18  2005/04/23 23:37:02  mast
Moved some functions here

Revision 3.17  2005/03/22 16:46:33  mast
Fixed return type of imodDeleteObject

Revision 3.16  2005/03/20 20:01:13  mast
Fixed a mod/imod problem from copying code

Revision 3.15  2005/03/20 19:56:43  mast
Documenting and eliminating duplicate functions

Revision 3.14  2004/12/03 17:26:00  mast
Changed behavior of imodDeletePoint, removed DelPoint

Revision 3.13  2004/11/20 04:17:43  mast
Added object move function and other changes for undo/redo structure

Revision 3.12  2004/09/21 20:13:40  mast
Changes for multiple and global clipping planes (i.e. checksum)

Revision 3.11  2004/09/10 21:33:46  mast
Eliminated long variables

Revision 3.10  2004/04/27 21:44:47  mast
Eliminated rotation angle fo first view from checksum

Revision 3.9  2004/01/05 17:27:17  mast
Added initialization of xybin and zbin

Revision 3.8  2003/11/01 16:41:56  mast
changed to use new error processing routine

Revision 3.7  2003/10/24 03:02:14  mast
move routines to new b3dutil file

Revision 3.6  2003/09/16 02:06:08  mast
*** empty log message ***

Revision 3.5  2003/07/31 21:41:25  mast
Delete object view and view data when deleting model, also delete
object view data when deleting a single object

Revision 3.4  2003/06/27 20:19:48  mast
Made min and max functions return (-1,-1,-1) if there are no model points,
and improved the checksum computation to include real and integer values
and include all view data.

Revision 3.3  2003/03/13 01:18:49  mast
Add new default object color scheme

Revision 3.2  2003/02/27 00:34:40  mast
add projects' *.dsw *.dsp

Revision 3.1  2002/11/05 23:28:23  mast
Changed imodCopyright to use defined lab name

*/

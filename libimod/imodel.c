/*  IMOD VERSION 2.50
 *
 *  imodel.c -- Library funcions for handeling model structures.
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


/* Library Functions */
/* Function
 * ---------------------------------------------------------------------------
 * int   imodVersion(char *pname)     Print the current version of imod.
 * Imod *imodNew(void)                Create a new model.
 * void  imodDelete(Imod *imod)       Delete a model.
 * 
 * void  imodGetIndex(Imod *imod, int *object, int *contour, int *point)
 * void  imodSetIndex(Imod *imod, int object, int contour, int point)
 *
 * int   imodNewObject(Imod *imod)
 * int   imodNextObject(Imod *imod)
 * int   imodPrevObject(Imod *imod)
 * int   imodFreeObject(Imod *imod, int index)
 * int   imodMoveObject(Imod *imod, int obOld, int obNew)
 * 
 * int   imodNewContour(Imod *imod)
 * void  imodDeleteContour(Imod *imod)
 * int   imodNextContour(Imod *imod)
 * int   imodPrevContour(Imod *imod)
 *
 * int   imodNewPoint(Imod *imod, Ipoint *point)
 * int   imodDeletePoint(Imod *imod)
 * int   imodInsertPoint(Imod *imod, Ipoint *point, int index)
 * int   imodNextPoint(Imod *imod)
 * int   imodPrevPoint(Imod *imod)
 */


/*************************** include files ***********************************/
#include <math.h>
#include "imodel.h"
#include "b3dutil.h"


Imod *imodNew(void)
{
  Imod *model;

  model = (struct Mod_Model *)malloc(sizeof(struct Mod_Model));
  if (model == NULL)
    return((struct Mod_Model *)NULL);
  model->file     = NULL;
  imodDefault(model);
  return(model);
}

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

  model->refImage = NULL;
  model->fileName = NULL;
  model->xybin = 1;
  model->zbin = 1;
  model->store    = NULL;
  return(0);
}

void imodDelete(Imod *imod)
{
  imodFree(imod);
}
void imodFree(Imod *imod)
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
  free(imod);
}

void imodGetIndex(Imod *imod, int *object, int *contour, int *point)
{
  *object  = imod->cindex.object;
  *contour = imod->cindex.contour;
  *point   = imod->cindex.point;
  return;
}

void imodSetIndex(Imod *imod, int object, int contour, int point)
{
  Iobj *obj;
  Icont *cont;
     
  if (object < 0)
    object = contour = point = -1;
  if (object >= imod->objsize)
    object = 0;

  imod->cindex.object  = object;
  obj = imodel_object_get(imod);
  if (!obj){
    imod->cindex.contour = imod->cindex.point = -1;
    return;
  }

  /* check for contour # out of bounds and set contour index. */
  if (contour < 0) contour = point = -1;
  if (contour >= obj->contsize) contour = obj->contsize - 1;
  imod->cindex.contour = contour;
  cont = imodContourGet(imod);
  if (!cont){
    imod->cindex.point = -1;
    return;
  }

  /* check for point # out of bounds */
  if (point >= cont->psize) point = cont->psize - 1;
  if (point < 0) point = -1;
  if (cont->psize < 1) point = -1;
  imod->cindex.point   = point;
     
  return;
}

/* DNM 6/26/03: for these 3 functions, return -1, -1, -1 if no points in
   model; also actually stop on first point. */
void imodel_maxpt(struct Mod_Model *imod, struct Mod_Point *pnt)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;

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
      }
    }
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

void imodel_minpt(struct Mod_Model *imod, struct Mod_Point *pnt)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
     
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
      }
    }
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

void imodGetBoundingBox(Imod *imod, Ipoint *min, Ipoint *max)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;

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
      }
    }
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


double imodel_dist(struct Mod_Model *imod)
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

/* static float basered = 0.5, basegreen = 0.5, baseblue = 0.5; */
#define MAX_STOCK_COLORS  35

/*****************************************************************************/
/* Function imodNewObject()                                                  */
/* Adds new object to end of obj array                                       */
/* Returns 1 if error.                                                       */
/*****************************************************************************/
int imodNewObject(struct Mod_Model *mod)
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
    obj = (struct Mod_Object *)malloc( sizeof(struct Mod_Object) );
  else
    obj = (struct Mod_Object *)realloc
      (mod->obj, sizeof(struct Mod_Object) * (mod->objsize + 1));

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


void   imodDeleteObject(Imod *imod, int index)
{
  imodFreeObject(imod, index);
}

int imodFreeObject(struct Mod_Model *mod, int index)
{

  int i;
  struct Mod_Object *obj = NULL;
  struct Mod_Object *tobj = NULL;
     
  obj = &(mod->obj[index]);
  if (!obj)
    return(-1);

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


/*****************************************************************************/
/* imodMoveObject - inputs: pointer to model, existing and new object number */
/*                  returns 1 if memory error                                */
/*****************************************************************************/
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


/*****************************************************************************/
/* imodNextObject - input, pointer to model.                                 */
/*              returns new object index.                                    */
/*****************************************************************************/
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
  obj = imodel_object_get(imod);
     
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


/*****************************************************************************/
/* imodPrevObject - input, pointer to model.                                 */
/*              returns new object index.                                    */
/*****************************************************************************/
int imodPrevObject(struct Mod_Model *mod)
{
  struct Mod_Object *obj;
  struct Mod_Contour *cont;

  if (mod == NULL)
    return(-1);
  /* If no object selected or if object is # 0, leave index alone*/
  if (mod->cindex.object <= 0)
    return(mod->cindex.object);

  if (mod->cindex.object > mod->objsize)
    mod->cindex.object = mod->objsize;

  --mod->cindex.object;
  obj = imodel_object_get(mod);
  if ( mod->cindex.contour >= obj->contsize)
    mod->cindex.contour = obj->contsize - 1;
  cont = (struct Mod_Contour *)imodContourGet(mod);

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


/*****************************************************************************/
/* Contour Functions                                                         */
/*****************************************************************************/
int imodNewContour(Imod *imod)
{
  return(NewContour(imod));
}

int NewContour(struct Mod_Model *mod)
{
  struct Mod_Object *obj;
  struct Mod_Contour *cont, *ocont;

  obj = imodel_object_get(mod);

  if (obj == NULL)
    return(-1);

  /* Allocate Memory for new contour. */
  if (obj->contsize == 0)
    cont = (struct Mod_Contour *)
      malloc(sizeof(struct Mod_Contour));
  else
    cont = (struct Mod_Contour *)
      realloc(obj->cont,
              sizeof(struct Mod_Contour) * (obj->contsize + 1));

  if (cont == NULL){
    b3dError(stderr,
            "IMOD: NewContour memory Error, "
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
  cont->type = 0;
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


void imodDeleteContour(Imod *imod)
{
  DelContour(imod, imod->cindex.contour);
  return;
}

int DelContour(struct Mod_Model *mod, int index)
{
  struct Mod_Object *obj;
  int i;
     

  obj = imodel_object_get(mod);
  if (!obj)
    return(0);


  /* If contour has any points, free them. */
  /* DNM: this was &(obj->cont[index].pts) but that seems wrong! */
  if ((  obj->cont[index].pts != NULL) && (obj->cont[index].psize))
    free( obj->cont[index].pts );

  if (obj->cont[index].sizes)
    free (obj->cont[index].sizes);

  /* DNM: need to delete labels if any */
  imodLabelDelete(obj->cont[index].label);
     
  /* Push extra contours into hole */
  if (index != obj->contsize - 1)
    for(i = index; i < (obj->contsize - 1); i++){
      imodContourCopy(&(obj->cont[i + 1]), &(obj->cont[i]));
    }

  /* Change contour array to new size */
  if (obj->contsize > 1){
    obj->contsize--;
    obj->cont = (struct Mod_Contour *)
      realloc(obj->cont, obj->contsize * sizeof(struct Mod_Contour));
     
    if (obj->contsize >= mod->cindex.contour)
      mod->cindex.contour = obj->contsize - 1;

    if (obj->cont[mod->cindex.contour].psize >= mod->cindex.point)
      mod->cindex.point= obj->cont[mod->cindex.contour].psize - 1;
      
  }else{
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

int imodPrevContour(Imod *imod)
{
  return(PrevContour(imod));
}

int PrevContour(struct Mod_Model *mod)
{
  struct Mod_Object *obj;

  if (mod == NULL)
    return(-1);

  /* no object selected. */
  if (mod->cindex.object < 0){
    mod->cindex.contour = -1;
    return(-1);
  }
  obj = &(mod->obj[mod->cindex.object]);
  if (!obj){
    mod->cindex.contour = -1;
    return(-1);
  }

  /* If object has no contours, return. */
  if (!obj->contsize)
    return(-1);

  /* if no contour or first contour selected exit. */
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

int   imodNextContour(Imod *imod)
{
  return(NextContour(imod));
}

int NextContour(Imod *imod)
{
  Iobj *obj;

  if (imod == NULL)
    return(-1);

  /* if no object selected, forget it. */
  if (imod->cindex.object < 0)
    return(-1);

  /* Selected Object. */
  obj = &(imod->obj[imod->cindex.object]);

  /* If object has no contours, return. */
  if (!obj->contsize)
    return(-1);

  /* if contour is last one, return */
  if (imod->cindex.contour == obj->contsize - 1)
    return(imod->cindex.contour);


  /* if no contour selected, select the first one */
  if (imod->cindex.contour < 0){
    imod->cindex.contour = 0;
    if (imod->cindex.point >= obj->cont[0].psize)
      imod->cindex.point = obj->cont[0].psize - 1;
    return(imod->cindex.contour);
  }
     
  imod->cindex.contour++;

  /* if point index is to high change it. */
  if (imod->cindex.point >= obj->cont[imod->cindex.contour].psize)
    imod->cindex.point = obj->cont[imod->cindex.contour].psize - 1;

  return(obj->contsize);
}
     


/*****************************************************************************
 * Point Functions
 *       NewPoint
 *       InsertPoint
 *       DelPoint
 */

int NewPoint(struct Mod_Model *mod, struct Mod_Point *pt)
{
  return(imodNewPoint(mod,pt));
}
 
int imodNewPoint(Imod *imod, Ipoint *pt)
{
  struct Mod_Contour *cont;

  cont = imodContourGet(imod);
  if (!cont) return(0);
  imod->cindex.point++;
  return(imodPointAppend(cont, pt));
}



/*****************************************************************************/
/*  InsertPoint - Add point to middle of array at index.                     */
/*  Returns object size.                                                     */
/*****************************************************************************/
int InsertPoint(struct Mod_Model *mod, struct Mod_Point *pt, int index)
{
  return(imodInsertPoint(mod, pt, index));
}

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
     



/*****************************************************************************/
/* DeletePoint - Deletes index point from point array in contour.            */
/* returns size of new array.                                                */
/*****************************************************************************/

/* Deletes the current point, or the current contour if it the last point in it
   Returns size of current contour or -1 if nothing was deleted */
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
    DelContour(imod, imod->cindex.contour);
    return(0);
  }

  index = imod->cindex.point;

  /* DNM 11/16/04: removed second test on size = 1 that just free points */

  if (index || cont->psize == 1)
    imod->cindex.point = index - 1;
  return(imodPointDelete(cont, index));

}

int   imodPrevPoint(Imod *imod){ 
  return(PrevPoint(imod)); 
}
int PrevPoint(struct Mod_Model *mod)
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

int   imodNextPoint(Imod *imod){ 
  return(NextPoint(imod));
}

int NextPoint(struct Mod_Model *mod)
{

  struct Mod_Contour *cont = NULL;

  cont = imodContourGet(mod);

  if (cont == NULL)
    return(0);

  mod->cindex.point++;
   
  if ( mod->cindex.point >= cont->psize)
    mod->cindex.point = cont->psize - 1;

  return(mod->cindex.point);
}

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

int imodel_transform_slice(struct Mod_Model *model, float *mat, int slice)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  int zval;
  float x, y;
     
  for(ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
        zval = (cont->pts[pt].z + 0.5f);
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

/*****************************************************************************/
/* Locks object data for shared groups. If flag is true and the object
 * data array is already locked 0 is returned.
 *  Otherwise the object array is locked and 1 is returned.
 */
int imodel_lock(struct Mod_Model *mod, int flag)
{
  if (mod->lock){
    if (flag)
      return(0);

#ifdef __sgi__oldlock
    else
      /* wait for data to become available. */
      /* To do: wait for signal instead of polling. */

      while (mod->lock)
        sginap(1);
#endif
  }
     
  mod->lock = TRUE;
  return(1);
}

/* Unlock data after locking. */
int imodel_unlock(struct Mod_Model *mod)
{
  mod->lock = FALSE;
  return(0);
}



int imodel_model_clean(struct Mod_Model *mod, int keepEmptyObjs)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;

  /* push all points inside of boundries. */
  for(ob = 0 ; ob < mod->objsize; ob++){
    obj = &(mod->obj[ob]);
    /*    printf("ob = %d, size = %d\n", ob, mod->objsize); */
    if (!obj->contsize && !keepEmptyObjs){
      mod->cindex.object = ob;
      imodFreeObject(mod, ob);
      ob--;
      continue;
    }
           
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      /*         printf("ob %d: co = %d, size = %d\n", ob, co, obj->contsize); */
      if(!cont->psize){
        mod->cindex.object = ob;
        mod->cindex.contour = co;
        DelContour(mod, co);
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
      if (obj->cont[co].type > maxtime)
        maxtime = obj->cont[co].type;
    }
  }
  return maxtime;
}

int imodChecksum(Imod *imod)
{
  int ob, co, pt, isum, i;
  Iobj *obj;
  Icont *cont;
  Iview *view;
  IclipPlanes *clips;
  Iobjview *obv;
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
    osum += obj->mat1 + obj->mat1b1 + obj->mat1b2 + obj->mat1b3;
    osum += obj->mat3 + obj->mat3b1 + obj->mat3b2 + obj->mat3b3;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      psum += cont->surf;
      psum += cont->psize;
      for(pt = 0; pt < cont->psize; pt++){
        psum += cont->pts[pt].x;
        psum += cont->pts[pt].y;
        psum += cont->pts[pt].z;
      }
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
        osum += obv->mat1 + obv->mat1b1 + obv->mat1b2 + obv->mat1b3;
        osum += obv->mat3 + obv->mat3b1 + obv->mat3b2 + obv->mat3b3;
        sum += osum;
      }
    }
  }
  
  /* This will catch fractional values - not perfect but probably good */
  isum = sum / 1000000.;
  isum = (int)(1000. * (sum - 1000000. * isum));
  /* fprintf(stderr, "checksum = %d\n", isum); */
  return (isum);
}

/* clean model file due to dirty surface info */
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

/* Flip a model - exchange Y and Z */
void  imodFlipYZ(Imod *imod)
{
  Iobj  *obj;
  Icont *cont;
  Imesh *mesh;
  float tmp;
  int    ob, co, pt;

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
    for(co = 0; co < obj->meshsize; co++){
      mesh = &(obj->mesh[co]);
      for(pt = 0; pt < mesh->vsize; pt++){
        tmp = mesh->vert[pt].y;
        mesh->vert[pt].y = mesh->vert[pt].z;
        mesh->vert[pt].z = tmp;
      }
    }
  }
  tmp = imod->ymax;
  imod->ymax = imod->zmax;
  imod->zmax = tmp;
  return;
}

int   imodGetMaxObject(Imod *imod) { return(imod->objsize); }
float imodGetZScale(Imod *imod){ return(imod->zscale); }
float imodGetPixelSize(Imod *imod){ return(imod->pixsize); }
int   imodGetFlipped(Imod *imod) { return(imod->flags & IMODF_FLIPYZ); }

/*  
 *  iview.c -- view or camera handleing functions.
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

#include <string.h>
#include <math.h>
#include "imodel.h"

/*!
 * Allocates a new array of @@Iview structure@ of the given size. 
 * Returns NULL for error.
 */
Iview *imodViewNew(int size)
{
  Iview *view = (Iview *)malloc(sizeof(Iview) * size);
  return view;
}

/*!
 * Frees the @@Iview structure@ or array of Iviews in [vw], without freeing
 * any object views.
 */
void imodViewDelete(Iview *vw)
{
  if (vw)
    free(vw);
  return;
}

/*!
 * Sets default entries for the view [vw] with no model information, i.e. sets
 * the rad to 1.0 and assumes no object views 
 */
void imodViewDefault(Iview *vw)
{
  int i;
    
  vw->fovy = 0.0f;
  vw->rad  = 1.0f;
  vw->aspect = 1.0f;
  vw->cnear  = 0.0f;
  vw->cfar   = 1.0f;
  vw->rot.x = vw->rot.y = vw->rot.z = 0.0f;
  vw->trans.x = vw->trans.y = vw->trans.z = 0.0f;
  vw->scale.x = vw->scale.y = vw->scale.z = 1.0;
    
  vw->world = VIEW_WORLD_LIGHT;
  for(i = 0; i < 16; i++){
    vw->mat[i] = 0.0f;
  }
  vw->mat[0] = vw->mat[5] = vw->mat[10] = vw->mat[15] = 1.0f;
  for(i = 0; i < VIEW_STRSIZE; i++)
    vw->label[i] = 0x00;
  vw->lightx  = 0.0f;
  vw->lighty  = 0.0f;
  vw->plax    = 5.0f; 
  vw->dcstart = 0.0f;
  vw->dcend   = 1.0f;
  vw->objview = NULL;
  vw->objvsize = 0;
  
  imodClipsInitialize(&vw->clips);
  return;
}

/*!
 * Sets the default scaling for a view [vw] based on current model dimensions
 * in model [imod].  If the model is empty and has no extent, it assumes the
 * image dimensions given in [imageMax].  The ratio of Z binning to X/Y binning
 * given in [binScale], as well as the model {zscale}, is used to scale the 
 * Z dimensions.
 */
void imodViewDefaultScale(Imod *imod, Iview *vw, Ipoint *imageMax, 
                          float binScale)
{
  Ipoint maxp, minp;
  int i;
  if (!imod || !vw)
    return;

  imodGetBoundingBox(imod, &minp, &maxp);

  /* If there is no extent and an image maximum has been supplied,
     then use image box as limiting coordinates */
  if (maxp.x == minp.x && maxp.y == minp.y && maxp.z == minp.z &&
      imageMax->x) {
    minp.x = minp.y = minp.z = 0.;
    maxp.x = imageMax->x;
    maxp.y = imageMax->y;
    maxp.z = imageMax->z;
  }

  maxp.z *= binScale * imod->zscale;
  minp.z *= binScale * imod->zscale;

  vw->trans.x = (minp.x + maxp.x)  * -0.5f;
  vw->trans.y = (minp.y + maxp.y)  * -0.5f;
  vw->trans.z = (minp.z + maxp.z)  * -0.5f;
  if (imod->zscale)
    vw->trans.z /= binScale * imod->zscale;

  maxp.x -= minp.x;
  maxp.y -= minp.y;
  maxp.z -= minp.z;
  vw->rad = sqrt((maxp.x*maxp.x)+(maxp.y*maxp.y)+(maxp.z*maxp.z))*0.5f;

  /* DNM: Make this happen once here and let all callers rely on it */
  vw->rad *= 0.85;

  /* If the rad is zero, make it one to avoid division by 0 */
  if (!vw->rad)
    vw->rad = 1.;
  return;
}

/*!
 * Sets up a default view in [vw] given model information in [imod] and image 
 * dimensions in [imageMax].
 * No longer used in 3dmod but kept for consistency and for RIB conversion 
 */
void imodViewModelDefault(Imod *imod, Iview *vw, Ipoint *imageMax)
{
  if (!imod || !vw)
    return; 
  imodViewDefault(vw);
  imodViewDefaultScale(imod, vw, imageMax, 1.);
}


/* First definition was 67
   9/19/04, added clip plane arrays */
#define BYTES_PER_OBJVIEW (43 + 24 * IMOD_CLIPSIZE)

/*!
 * Writes an @@Iview structure@ [vw], including object views and a model clip
 * plane chunk, to the file [fout].  Clip plane points are scaled by [scale],
 * and normals are scaled by the inverse.  Returns IMOD_ERROR_WRITE for error.
 */
/* For backward compatibility, need to add all new elements at end of write
   so the array elements after the first have to be written at end */
int imodViewWrite(Iview *vw, FILE *fout, Ipoint *scale)
{
  unsigned int id;
  int i, j;
  int nbwrite;
  Iobjview *ov;
  IclipPlanes *clips;
  b3dUByte clipOut;
  Ipoint normScale;

  id = ID_VIEW;
  imodPutInt(fout, &id);
  id = 176;
  nbwrite = vw->objvsize * BYTES_PER_OBJVIEW;
  if (vw->objvsize) 
    id += 8 + nbwrite; 
  imodPutInt(fout, &id);
  imodPutFloats(fout, &vw->fovy, 30);
  imodPutInt(fout, &vw->world);
  imodPutBytes(fout, (unsigned char *)vw->label, VIEW_STRSIZE);
  imodPutFloats(fout, &vw->dcstart, 5);

  normScale.x = 1. / scale->x;
  normScale.y = 1. / scale->y;
  normScale.z = 1. / scale->z;
  
  if (vw->objvsize) {
    imodPutInt(fout, &vw->objvsize);
    imodPutInt(fout, &nbwrite);
    for (i = 0; i < vw->objvsize; i++) {
      ov = &vw->objview[i];
      clips = &ov->clips;
      imodPutInts(fout, &ov->flags, 1);
      imodPutFloats(fout, &ov->red, 3);
      imodPutInts(fout, &ov->pdrawsize, 1);
      imodPutBytes(fout, &ov->linewidth, 3);

      /* For backward compatibility, if there is one clip plane and it is
         off, set clip to 0 */
      clipOut = clips->count;
      if (clipOut == 1 && (clips->flags & 1) == 0)
        clipOut = 0;
      imodPutBytes(fout, &clipOut, 1);
      imodPutBytes(fout, &clips->flags, 3);
      imodPutScaledPoints(fout, &clips->normal[0], 1, &normScale);
      imodPutScaledPoints(fout, &clips->point[0], 1, scale);
      imodPutBytes(fout, &ov->ambient, 4);
      imodPutBytes(fout, &ov->fillred, 4);
      imodPutInts(fout, &ov->mat2, 1);
      imodPutBytes(fout, &ov->valblack, 4);
      imodPutScaledPoints(fout,  &clips->normal[1], IMOD_CLIPSIZE - 1,
        &normScale);
      imodPutScaledPoints(fout, &clips->point[1], IMOD_CLIPSIZE - 1,
                          scale);
    }
  }

  /* Write the clip plane chunk */
  if (vw->clips.count) {
    id = ID_MCLP;
    imodPutInt(fout, &id);
    id = 4 + 24 * vw->clips.count;
    imodPutInt(fout, &id);
    imodPutBytes(fout, &vw->clips.count, 4);
    imodPutScaledPoints(fout, &vw->clips.normal[0], vw->clips.count,
                        &normScale);
    imodPutScaledPoints(fout, &vw->clips.point[0], vw->clips.count,
                        scale);
  }

  if (ferror(fout))
    return(IMOD_ERROR_WRITE);
  else
    return(0);
}

/*
 * Writes all actual views (1 through {viewsize}) in model [imod] to the model
 * element {file}, with clip planes scaled by [scale].  Returns 
 * IMOD_ERROR_WRITE for error.
 */
int imodViewModelWrite(Imod *imod, Ipoint *scale)
{
  int i, id, bsize;
     
  if (imod->viewsize < 2)
    return -1;

  id = ID_VIEW;
  imodPutInt(imod->file, &id);
  bsize = 4;
  imodPutInt(imod->file, &bsize);
  imodPutInt(imod->file, &(imod->cview));

  for(i = 1; i < imod->viewsize; i++){
    imodViewWrite(&(imod->view[i]), imod->file, scale);
  }
  return(0);
}

/*!
 * Reads one @@Iview structure@ into model [imod] from the model element 
 * {file}, and adds it to the model's view array.  Reads object views as well.
 * Returns IMOD_ERROR_READ for error.
 */
int imodViewModelRead(Imod *imod)
{
  FILE *fin = imod->file;
  int i, ni = imod->viewsize + 1;
  Iview *nvw;
  Iview *vw;
  int lbuf;
  IclipPlanes *clips;
  Iobjview *ov;
  int bytesMissing, bytesObjv;
  int bytesRead = 0;

  lbuf = imodGetInt(fin);

  /* only current value selected. */
  if (lbuf == 4){
    imod->cview = imodGetInt(fin);
    return(0);
  }

  nvw = imodViewNew(ni);
  if (!nvw)
    return(IMOD_ERROR_MEMORY);

  vw = &nvw[imod->viewsize];
  vw->objvsize = 0;
  vw->objview = NULL;

  /* Need to initialize clip planes for view and for all object views because
     they may not be read in (for view), or ones past the first may not be
     read in (for object views) */
  imodClipsInitialize(&vw->clips);
     
  if (lbuf >= 56){
    imodGetFloats(fin, &vw->fovy, 14);
    bytesRead += 56;
  }
  if (lbuf >= 156){
    imodGetFloats(fin, vw->mat, 16);
    vw->world = imodGetInt(fin);
    imodGetBytes(fin, (unsigned char *)vw->label, VIEW_STRSIZE);
    bytesRead += 100;
     
  }
  if (lbuf >= 176){
    imodGetFloats(fin, &vw->dcstart, 5);
    bytesRead += 20;
  }

  if (lbuf >= 180){
    vw->objvsize = imodGetInt(fin);
    bytesObjv = imodGetInt(fin);

    bytesRead += 8;
    if (vw->objvsize > 0) {
      bytesMissing = BYTES_PER_OBJVIEW - bytesObjv / vw->objvsize ;
      vw->objview = (Iobjview *)
        malloc(vw->objvsize * sizeof(Iobjview));
      for (i = 0; i < vw->objvsize; i++) {
        ov = &vw->objview[i];
        clips = &ov->clips;
        imodClipsInitialize(clips);
        imodGetInts(fin, &ov->flags, 1);
        imodGetFloats(fin, &ov->red, 3);
        imodGetInts(fin, &ov->pdrawsize, 1);
        imodGetBytes(fin, &ov->linewidth, 3);

        /* Get clip parameters and first clip plane, then fix the count */
        imodGetBytes(fin, &clips->count, 4);
        imodGetFloats(fin, (float *)&clips->normal[0], 3);
        imodGetFloats(fin, (float *)&clips->point[0], 3);

        imodClipsFixCount(clips, imod->flags);

        imodGetBytes(fin, &ov->ambient, 4);
            
        /* DNM 9/4/03: read mat1 and mat3 as bytes, or as ints 
           for old model */
        if (imod->flags & IMODF_MAT1_IS_BYTES) {
          imodGetBytes(fin, &ov->fillred, 4);     
          imodGetInts(fin, &ov->mat2, 1);     
          imodGetBytes(fin, &ov->valblack, 4);     
        } else
          imodGetInts(fin, (int *)&ov->fillred, 3);     

        bytesRead += 67;
            
        /* If more elements are added in future, will need to test
           bytesMissing before reading them in.  For each one, test against
           total bytes in elements beyond this one */

        /* Read additional clip planes */
        if (bytesMissing <= 0) {
          imodGetFloats(fin, (float *)&clips->normal[1], 
                        3 * (IMOD_CLIPSIZE - 1));
          imodGetFloats(fin, (float *)&clips->point[1], 
                        3 * (IMOD_CLIPSIZE - 1));
          bytesRead += 24 * (IMOD_CLIPSIZE - 1);
        }

        /* But if this code tries to read a future file, it needs
           to skip over the rest of the data per object */
        if (bytesMissing < 0) {
          fseek(fin, -bytesMissing, SEEK_CUR);
          bytesRead -= bytesMissing;
        }
      }
    }
  }    

  if ((lbuf - bytesRead) > 0)
    fseek(fin, lbuf - bytesRead, SEEK_CUR);

  for(i = 0; i < imod->viewsize; i++){
    nvw[i] = imod->view[i];
  }
  imodViewDelete(imod->view);
  imod->view = nvw;
  imod->viewsize++;

  if (ferror(fin))
    return(IMOD_ERROR_READ);
  else
    return(0);
}

/*! 
 * Reads the variable-sized model clip plane chunk.  Returns
 * IMOD_ERROR_READ for error.
 */
int imodViewClipRead(Imod *imod)
{
  Iview *vw = &imod->view[imod->viewsize - 1];
  return(imodClipsRead(&vw->clips, imod->file));
}

/*!
 * Adds a new view to {view} array in the model [imod] and assigns it default 
 * properties.  Returns IMOD_ERROR_MEMORY for memory error.
 */
int imodViewModelNew(Imod *imod)
{
  int i, ni = imod->viewsize + 1;
  Iview *nvw = imodViewNew(ni);

  if (!nvw) return(IMOD_ERROR_MEMORY);

  imodViewDefault(&nvw[imod->viewsize]);

  for(i = 0; i < imod->viewsize; i++){
    nvw[i] = imod->view[i];
  }
  imodViewDelete(imod->view);
  imod->view = nvw;
  imod->viewsize++;
  return(0);
}

/*!
 * Copies all of the characteristics from the @@Objview structure@ [objview]
 * to the object [obj]
 */
void imodObjviewToObject(Iobjview *objview, Iobj *obj)
{
  obj->flags = objview->flags;
  obj->red = objview->red;
  obj->green = objview->green;
  obj->blue = objview->blue;
  obj->pdrawsize = objview->pdrawsize;
  obj->linewidth = objview->linewidth;
  obj->linesty = objview->linesty;
  obj->trans = objview->trans;
  obj->clips = objview->clips;
  memcpy (&obj->ambient, &objview->ambient, 4);
  memcpy (&obj->fillred, &objview->fillred, 12);
}


/*!
 * Copies all of the characteristics from the object [obj] to the object view
 * [objview].
 */
void imodObjviewFromObject(Iobj *obj, Iobjview *objview)
{
  objview->flags = obj->flags;
  objview->red = obj->red;
  objview->green = obj->green;
  objview->blue = obj->blue;
  objview->pdrawsize = obj->pdrawsize;
  objview->linewidth = obj->linewidth;
  objview->linesty = obj->linesty;
  objview->trans = obj->trans;
  objview->clips = obj->clips;
  memcpy (&objview->ambient, &obj->ambient, 4);
  memcpy (&objview->fillred, &obj->fillred, 12);
}

/*!
 * Makes the view specified by the {cview} element of model [imod] be the 
 * current view by copying it to the default view (element 0 of the {view}
 * array) and setting object properties from any object views.
 */
void imodViewUse(Imod *imod)
{
  int nobj, i;
  char labsav[VIEW_STRSIZE];

  /* First copy the view structure, saving and restoring the object view
     count and pointer from the default view */
  Iview *vw = &imod->view[imod->cview];
  Iobjview *defviewsave = imod->view->objview;
  int  defsizesave = imod->view->objvsize;
  memcpy(labsav, imod->view->label, VIEW_STRSIZE);
  imod->view[0] = *vw;
  memcpy(imod->view->label, labsav, VIEW_STRSIZE);
  imod->view->objview = defviewsave;
  imod->view->objvsize = defsizesave;
     

  /* Now set object characteristics based on the saved values in the
     current view.  Use no more data than the current # of objects */
  nobj = imod->objsize;
  if (nobj > vw->objvsize)
    nobj = vw->objvsize;

  for (i = 0; i < nobj; i++)
    imodObjviewToObject(&vw->objview[i], &imod->obj[i]);
}

/*!
 * Stores the default view and information about all objects in the [cview]
 * element of the {view} array of model [imod].  Returns IMOD_ERROR_MEMORY for
 * memory error.
 */
int imodViewStore(Imod *imod, int cview)
{
  int i;
  char labsav[VIEW_STRSIZE];
  Iview *vw = &imod->view[cview];

  /* First delete any existing object view data */
  if (vw->objview)
    free(vw->objview);

  memcpy(labsav, vw->label, VIEW_STRSIZE);
  *vw = imod->view[0];
  memcpy(vw->label, labsav, VIEW_STRSIZE);
  vw->objvsize = imod->objsize;
  vw->objview = (Iobjview *)malloc(imod->objsize * sizeof(Iobjview));
  if(!vw->objview) {
    vw->objvsize = 0;
    return(IMOD_ERROR_MEMORY);
  }

  for (i = 0; i < imod->objsize; i++)
    imodObjviewFromObject(&imod->obj[i], &vw->objview[i]);
  return (0);
}

/*!
 * Makes the actual model views in [imod] have a complete set of object views,
 * copying from object properties into all views if any need to be created.
 * Returns IMOD_ERROR_MEMORY for error.
 */
int imodObjviewComplete(Imod *imod)
{
  int i, j;
  Iview *vw;

  /* Loop through all real views */
  for (i = 1; i < imod->viewsize; i++) {
    vw = &imod->view[i];
    if (vw->objvsize < imod->objsize) {

      /* If there are missing object views, first allocate enough objviews */
      if (vw->objvsize)
        vw->objview = (Iobjview *)realloc(vw->objview, 
                                          imod->objsize * sizeof(Iobjview));
      else
        vw->objview = (Iobjview *)malloc(imod->objsize * sizeof(Iobjview));
      if(!vw->objview) {
        vw->objvsize = 0;
        return(IMOD_ERROR_MEMORY);
      }
      
      /* Then copy the missing objects into the view */
      for (j = vw->objvsize; j < imod->objsize; j++)
        imodObjviewFromObject(&imod->obj[j], &vw->objview[j]);
      vw->objvsize = imod->objsize;
    }
  }
  return 0;
}

/*!
 * Deletes the object views for object [index] from all of the views in model
 * [imod].
 */
void imodObjviewDelete(Imod *imod, int index)
{
  int i, j;
  Iview *vw;
  for (i = 1; i < imod->viewsize; i++) {
    vw = &imod->view[i];

    /* If this set of object views includes this object, shift all of
     the ones above down */
    if (index < vw->objvsize) {
      for (j = index + 1; j < vw->objvsize; j++)
        vw->objview[j - 1] = vw->objview[j];

      /* reduce count, free array if they are all gone */
      vw->objvsize--;
      if (!vw->objvsize) {
        free(vw->objview);
        vw->objview = NULL;
      }
    }
  }
}

/*! 
 * Frees the object views from all views in model [imod].
 */
void imodObjviewsFree(Imod *imod)
{
  int i;
  Iview *vw;
  for (i = 1; i < imod->viewsize; i++) {
    vw = &imod->view[i];
    if (vw->objvsize) {
      free (vw->objview);
      vw->objview = NULL;
      vw->objvsize = 0;
    }
  }
}

/* Image File view functions. */
/*!
 * Allocates an @@IrefImage structure@ and reads the IMONX chunk for model
 * [imod].  Returns -1 for error.
 */
int imodIMNXRead(Imod *imod)
{
  FILE *fin  = imod->file;
  int  lbuf = imodGetInt(fin);

  IrefImage *ref = (IrefImage *)malloc(sizeof(IrefImage));
  if (!ref)
    return -1;

  /* 12/23/04: stopped reading/writing across structure boundaries */
  imodGetFloats(fin, &ref->oscale.x, 3);
  imodGetFloats(fin, &ref->otrans.x, 3);
  imodGetFloats(fin, &ref->orot.x, 3);
  imodGetFloats(fin, &ref->cscale.x, 3);
  imodGetFloats(fin, &ref->ctrans.x, 3);
  imodGetFloats(fin, &ref->crot.x, 3);
  imod->refImage = ref;
     
  /* 12/23/04: "Bug fix for models made from manali stacks pre V1.3" was bug */

  return(0);
}

/*!
 * Writes the @@IrefImage structure@ {refImage} in model [imod] if any.
 * Returns -1 if there is none.
 */
int imodIMNXWrite(Imod *imod)
{
  int id = ID_IMNX;
  if (!imod->refImage) 
    return -1;

  imodPutInt(imod->file, &id);
  id = SIZE_IMNX;
  imodPutInt(imod->file, &id);
  imodPutFloats(imod->file, &imod->refImage->oscale.x, 3);
  imodPutFloats(imod->file, &imod->refImage->otrans.x, 3);
  imodPutFloats(imod->file, &imod->refImage->orot.x, 3);
  imodPutFloats(imod->file, &imod->refImage->cscale.x, 3);
  imodPutFloats(imod->file, &imod->refImage->ctrans.x, 3);
  imodPutFloats(imod->file, &imod->refImage->crot.x, 3);
  return(0);
}

/*!
 * Allocates and returns a new @@IrefImage structure@ with default values
 * of 1 for {cscale} and {oscale} and 0 for {crot}, {ctrans}, {orot}, and 
 * {otrans}.
 */
IrefImage *imodIMNXNew()
{
  IrefImage *ref = (IrefImage *)malloc(sizeof(IrefImage));
  if (!ref)
    return NULL;
  ref->cscale.x = ref->cscale.y = ref->cscale.z = 1.;
  ref->ctrans.x = ref->ctrans.y = ref->ctrans.z = 0.;
  ref->crot = ref->ctrans;
  ref->oscale = ref->cscale;
  ref->otrans = ref->ctrans;
  ref->orot = ref->crot;
  return ref;
}

/*  IMOD VERSION 2.20
 *
 *  iview.c -- view or camera handleing functions.
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

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.5  2003/07/31 21:37:00  mast
Extracted the transfer of object data to and from an objview to functions
Added new functions for making the list of object views complete for a
view, and for deleting and freeing object views

Revision 3.4  2003/06/27 20:14:45  mast
Implemented new function to set default scaling of a view and added ability
to pass image size and have it provide fallback scaling when there is no
model extent yet.

Revision 3.3  2002/09/04 23:50:23  mast
FIxed bug in last change

Revision 3.2  2002/09/04 23:13:29  mast
Read mat1 and mat3 the old way if a flag is not set

Revision 3.1  2002/09/03 20:04:46  mast
Changed to read and write mat1 and mat3 in object view structures as
bytes to avoid endian problems

*/

#include <math.h>
#include "imodel.h"

Iview *imodViewNew(int size)
{
  return((Iview *)malloc(sizeof(Iview) * size));
}

void imodViewDelete(Iview *vw)
{
  if (vw) free(vw);
  return;
}

/* Set default entries for a view with no model information, i.e. set the
   rad to 1.0 and have no object views */
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
  for(i = 0; i < 32; i++)
    vw->label[i] = 0x00;
  vw->lightx  = 0.0f;
  vw->lighty  = 0.0f;
  vw->plax    = 5.0f; 
  vw->dcstart = 0.0f;
  vw->dcend   = 1.0f;
  vw->objview = NULL;
  vw->objvsize = 0;
  return;
}

/* Set the default scaling for a view based on current model dimensions */
void imodViewDefaultScale(Imod *imod, Iview *vw, Ipoint *imageMax)
{
  Ipoint maxp, minp;
  int i;
  if (!imod || !vw)
    return;

  imodel_maxpt(imod, &maxp);
  imodel_minpt(imod, &minp);

  /* If there is no extent and an image maximum has been supplied,
     then use image box as limiting coordinates */
  if (maxp.x == minp.x && maxp.y == minp.y && maxp.z == minp.z &&
      imageMax->x) {
    minp.x = minp.y = minp.z = 0.;
    maxp.x = imageMax->x;
    maxp.y = imageMax->y;
    maxp.z = imageMax->z;
  }

  maxp.z *= imod->zscale;
  minp.z *= imod->zscale;

  vw->trans.x = (minp.x + maxp.x)  * -0.5f;
  vw->trans.y = (minp.y + maxp.y)  * -0.5f;
  vw->trans.z = (minp.z + maxp.z)  * -0.5f;
  if (imod->zscale)
    vw->trans.z /= imod->zscale;

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

/* Set up a default view given model information that can give scale.
   No longer used in 3dmod but kept for consistency and for RIB conversion */
void imodViewModelDefault(Imod *imod, Iview *vw, Ipoint *imageMax)
{
  if (!imod || !vw)
    return; 
  imodViewDefault(vw);
  imodViewDefaultScale(imod, vw, imageMax);
}



#define BYTES_PER_OBJVIEW 67

int imodViewWrite(Iview *vw, FILE *fout)
{
  unsigned int id;
  int i;
  int nbwrite;
  Iobjview *ov;

  id = ID_VIEW;
  imodPutInt(fout, &id);
  id = 176;
  nbwrite = vw->objvsize * BYTES_PER_OBJVIEW;
  if (vw->objvsize) 
    id += 8 + nbwrite; 
  imodPutInt(fout, &id);
  imodPutFloats(fout, &vw->fovy, 30);
  imodPutInt(fout, &vw->world);
  imodPutBytes(fout, (unsigned char *)vw->label, 32);
  imodPutFloats(fout, &vw->dcstart, 5);
  if (vw->objvsize) {
    imodPutInt(fout, &vw->objvsize);
    imodPutInt(fout, &nbwrite);
    for (i = 0; i < vw->objvsize; i++) {
      ov = &vw->objview[i];
      imodPutInts(fout, &ov->flags, 1);
      imodPutFloats(fout, &ov->red, 3);
      imodPutInts(fout, &ov->pdrawsize, 1);
      imodPutBytes(fout, &ov->linewidth, 3);
      imodPutBytes(fout, &ov->clip, 4);
      imodPutFloats(fout, (float *)&ov->clip_normal, 6);
      imodPutBytes(fout, &ov->ambient, 4);
      imodPutBytes(fout, (unsigned char *)&ov->mat1, 4);
      imodPutInts(fout, (int *)&ov->mat2, 1);
      imodPutBytes(fout, (unsigned char *)&ov->mat3, 4);
    }
  }

  if (ferror(fout))
    return(IMOD_ERROR_WRITE);
  else
    return(0);
}

int imodViewModelWrite(Imod *imod)
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
    imodViewWrite(&(imod->view[i]), imod->file);
  }
  return(0);
}

int imodViewModelRead(Imod *imod)
{
  FILE *fin = imod->file;
  int i, ni = imod->viewsize + 1;
  Iview *nvw = imodViewNew(ni);
  Iview *vw;
  int lbuf;
  Iobjview *ov;
  int bytesMissing, bytesObjv;
  int bytesRead = 0;
  if (!nvw) return(IMOD_ERROR_MEMORY);

  lbuf = imodGetInt(fin);

  vw = &nvw[imod->viewsize];
  vw->objvsize = 0;
  vw->objview = NULL;
     
  /* only current value selected. */
  if (lbuf == 4){
    imod->cview = imodGetInt(fin);
    return(0);
  }

  if (lbuf >= 56){
    imodGetFloats(fin, &vw->fovy, 14);
    bytesRead += 56;
  }
  if (lbuf >= 156){
    imodGetFloats(fin, vw->mat, 16);
    vw->world = imodGetInt(fin);
    imodGetBytes(fin, (unsigned char *)vw->label, 32);
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
        imodGetInts(fin, &ov->flags, 1);
        imodGetFloats(fin, &ov->red, 3);
        imodGetInts(fin, &ov->pdrawsize, 1);
        imodGetBytes(fin, &ov->linewidth, 3);
        imodGetBytes(fin, &ov->clip, 4);
        imodGetFloats(fin, (float *)&ov->clip_normal, 6);
        imodGetBytes(fin, &ov->ambient, 4);
            
        /* DNM 9/4/03: read mat1 and mat3 as bytes, or as ints 
           for old model */
        if (imod->flags & IMODF_MAT1_IS_BYTES) {
          imodGetBytes(fin, (unsigned char *)&ov->mat1, 4);     
          imodGetInts(fin, (int *)&ov->mat2, 1);     
          imodGetBytes(fin, (unsigned char *)&ov->mat3, 4);     
        } else
          imodGetInts(fin, (int *)&ov->mat1, 3);     

        bytesRead += 67;
            
        /* If more elements are added in future, will need to test
           bytesMissing before reading them in */

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

/* Copy all of the characteristics from an object view to an object */
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
  memcpy (&obj->clip, &objview->clip, 4);
  obj->clip_normal = objview->clip_normal;
  obj->clip_point = objview->clip_point;
  memcpy (&obj->ambient, &objview->ambient, 4);
  memcpy (&obj->mat1, &objview->mat1, 12);
}


/* Copy all of the characteristics from an object to an object view */
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
  memcpy (&(objview->clip), &(obj->clip), 4);
  objview->clip_normal = obj->clip_normal;
  objview->clip_point = obj->clip_point;
  memcpy (&objview->ambient, &obj->ambient, 4);
  memcpy (&objview->mat1, &obj->mat1, 12);
}

void imodViewUse(Imod *imod)
{
  int nobj, i;
  char labsav[32];

  /* First copy the view structure, saving and restoring the object view
     count and pointer from the default view */
  Iview *vw = &imod->view[imod->cview];
  Iobjview *defviewsave = imod->view->objview;
  int  defsizesave = imod->view->objvsize;
  memcpy(labsav, imod->view->label, 32);
  imod->view[0] = *vw;
  memcpy(imod->view->label, labsav, 32);
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

int imodViewStore(Imod *imod, int cview)
{
  int i;
  char labsav[32];
  Iview *vw = &imod->view[cview];

  /* First delete any existing object view data */
  if (vw->objview)
    free(vw->objview);

  memcpy(labsav, vw->label, 32);
  *vw = imod->view[0];
  memcpy(vw->label, labsav, 32);
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

/* Make the model views have a complete set of objviews, copying from 
 object properties into all views if any need to be created */
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

/* Delete the object views for one object from all of the views */
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

/* Free the object views from all views */
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

int imodIMNXRead(Imod *imod)
{
  FILE *fin  = imod->file;
  int  lbuf = imodGetInt(fin);

  IrefImage *ref = (IrefImage *)malloc(sizeof(IrefImage));
  imodGetFloats(fin, (float *)ref, 18);
  imod->refImage = ref;
     
  /* Bug fix for models made from manali stacks pre V1.3 */
  if (ref->ctrans.z != 0.0f)
    if ( (ref->ctrans.z/ref->ctrans.z) != 1.0)
      ref->ctrans.z = 0.0f;

  return(0);
}

int imodIMNXWrite(Imod *imod)
{
  int id = ID_IMNX;
  if (!imod->refImage) return -1;

  imodPutInt(imod->file, &id);
  id = SIZE_IMNX;
  imodPutInt(imod->file, &id);

  imodPutFloats(imod->file, (float *)imod->refImage, 18);
  return(0);
}

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

void imodViewModelDefault(Imod *imod, Iview *vw)
{
     float fovytan;
     Ipoint maxp, minp;
     int i;
     if (!imod) return; if (!vw) return;

     imodel_maxpt(imod, &maxp);
     imodel_minpt(imod, &minp);
     maxp.z *= imod->zscale;
     minp.z *= imod->zscale;

     vw->world = VIEW_WORLD_LIGHT;     
     vw->rot.x = vw->rot.y = vw->rot.z = 0.0f;
     vw->trans.x = (minp.x + maxp.x)  * -0.5f;
     vw->trans.y = (minp.y + maxp.y)  * -0.5f;
     vw->trans.z = (minp.z + maxp.z)  * -0.5f;
     if (imod->zscale)
	 vw->trans.z /= imod->zscale;
     vw->scale.x = vw->scale.y = vw->scale.z = 1.0f;
     vw->fovy    = 0.0f;
     vw->aspect  = 1.0f;
     for(i = 0; i < 16; i++){
	 vw->mat[i] = 0.0f;
     }
     vw->mat[0]  = vw->mat[5] = vw->mat[10] = vw->mat[15] = 1.0f;
/*     vw->mat[12] = vw->trans.x;
     vw->mat[13] = vw->trans.y;
     vw->mat[14] = vw->trans.z;
*/
     vw->lightx = 0.0f;
     vw->lighty = 0.0f;
     vw->plax   = 5.0f;
     vw->dcstart = 0.0f;
     vw->dcend   = 1.0f;
     maxp.x -= minp.x;
     maxp.y -= minp.y;
     maxp.z -= minp.z;
     vw->rad = sqrt((maxp.x*maxp.x)+(maxp.y*maxp.y)+(maxp.z*maxp.z))*0.5f;

     /* DNM: Make this happen once here and let all callers rely on it */
     vw->rad *= 0.85;
     return;
}


#define BYTES_PER_OBJVIEW 67

int imodViewWrite(Iview *vw, FILE *fout)
{
     unsigned long id;
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
     long i, id, bsize;
     
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
     

     /* Now set object characteristics based on the the saved values in the
	current view.  Use no more data than the current # of objects */
     nobj = imod->objsize;
     if (nobj > vw->objvsize)
          nobj = vw->objvsize;

     for (i = 0; i < nobj; i++) {
          imod->obj[i].flags = vw->objview[i].flags;
          imod->obj[i].red = vw->objview[i].red;
          imod->obj[i].green = vw->objview[i].green;
          imod->obj[i].blue = vw->objview[i].blue;
          imod->obj[i].pdrawsize = vw->objview[i].pdrawsize;
          imod->obj[i].linewidth = vw->objview[i].linewidth;
          imod->obj[i].linesty = vw->objview[i].linesty;
          imod->obj[i].trans = vw->objview[i].trans;
	  memcpy (&imod->obj[i].clip, &vw->objview[i].clip, 4);
          imod->obj[i].clip_normal = vw->objview[i].clip_normal;
          imod->obj[i].clip_point = vw->objview[i].clip_point;
	  memcpy (&imod->obj[i].ambient, &vw->objview[i].ambient, 4);
	  memcpy (&imod->obj[i].mat1, &vw->objview[i].mat1, 12);
     }    
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
     if(!vw->objview)
          return(1);

     for (i = 0; i < imod->objsize; i++) {
          vw->objview[i].flags = imod->obj[i].flags;
          vw->objview[i].red = imod->obj[i].red;
          vw->objview[i].green = imod->obj[i].green;
          vw->objview[i].blue = imod->obj[i].blue;
          vw->objview[i].pdrawsize = imod->obj[i].pdrawsize;
          vw->objview[i].linewidth = imod->obj[i].linewidth;
          vw->objview[i].linesty = imod->obj[i].linesty;
          vw->objview[i].trans = imod->obj[i].trans;
	  memcpy (&(vw->objview[i].clip), &(imod->obj[i].clip), 4);
          vw->objview[i].clip_normal = imod->obj[i].clip_normal;
          vw->objview[i].clip_point = imod->obj[i].clip_point;
	  memcpy (&vw->objview[i].ambient, &imod->obj[i].ambient, 4);
	  memcpy (&vw->objview[i].mat1, &imod->obj[i].mat1, 12);

     }    
     return (0);
}



/* Image File view functions. */

int imodIMNXRead(Imod *imod)
{
     FILE *fin  = imod->file;
     long  lbuf = imodGetInt(fin);

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
     long id = ID_IMNX;
     if (!imod->refImage) return -1;

     imodPutInt(imod->file, &id);
     id = SIZE_IMNX;
     imodPutInt(imod->file, &id);

     imodPutFloats(imod->file, (float *)imod->refImage, 18);
     return(0);
}

/*  IMOD VERSION 2.30
 *
 *  moplib.c -- mop functions.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
*/

#include <stdio.h>
#include <math.h>
#include "imodel.h"
#include "mrcfiles.h"

extern int Mop_red;
extern int Mop_green;
extern int Mop_blue;
extern int Mop_help;
extern struct Mod_Contour *mop_buildcont(struct Mod_Contour *ocont);
extern struct Mod_Contour *mop_setcont(struct Mod_Contour *ocont);
extern struct Mod_Contour *mop_scan(struct Mod_Contour *ocont);
struct Mod_Contour Mop_cont;
void *mrc_readz(FILE *fin, struct MRCheader *hdata, int z);
int clearz(void *pdata, struct MRCheader *hdata);
int slice_paint(struct Mod_Model *mod,
                void *idata,
                void *pdata,
                struct MRCheader *hdata,
                int z);
int contour_paint_byte(struct Mod_Contour *cont,
                       unsigned char *idata,
                       unsigned char *pdata,
                       struct MRCheader *hdata,
                       struct Mod_Object *obj);

int modpaint(FILE *mfin, FILE *gfin, FILE *gfout, 
             struct Mod_Model *model,
             struct MRCheader *hdata, int invert)
{
  int k, j, i;
  int xmin, xmax, ymin, ymax, zmin, zmax;
  void *idata = NULL;
  void *pdata = NULL;
  unsigned char *bdata = NULL;
  short *sdata = NULL;
  float *fdata = NULL;
  unsigned char *bidata = NULL;
  short *sidata = NULL;
  float *fidata = NULL;

  int psize;
  int xypsize, xysize;
  struct Mod_Point maxpt, minpt;
  struct MRCheader hdout;

  imodel_maxpt(model, &maxpt);
  imodel_minpt(model, &minpt);

  switch (hdata->mode)
    {
    case MRC_MODE_BYTE :
      psize = 1;
      break;
    case MRC_MODE_SHORT :
      psize = 2;
      break ;
    case MRC_MODE_FLOAT :
      psize = 4;
      break ;
    case MRC_MODE_COMPLEX_SHORT :
      fprintf(stderr,"MOP: Complex Short not supported.\n") ;
      return(-1);
    case MRC_MODE_COMPLEX_FLOAT :
      fprintf(stderr, "MOP: Complex Float not supported.\n") ;
      return(-1);
    default:
      fprintf(stderr, "MOP: Unknown graphic data type.\n");
      return(-1);
    }
     
  xypsize = hdata->nx * hdata->ny * psize;
  xysize = hdata->nx * hdata->ny;
  pdata = (void *)malloc( xypsize);

  bdata = pdata;
  sdata = pdata;
  fdata = pdata;     

  zmax = hdata->nz - 1;
  zmin = 0;
  ymax = maxpt.y;
  ymin = minpt.y;
  xmax = maxpt.x;
  xmin = minpt.x;

  if ( (minpt.z < hdata->nz) && (minpt.z > 0))
    zmin = minpt.z;
  if ( (maxpt.z < (hdata->nz - 1))  &&  (maxpt.z > minpt.z) )
    zmax = maxpt.z;
  mrc_head_new(&hdout, xmax-xmin+1, ymax-ymin+1, zmax-zmin+1, hdata->mode);
  mrc_head_label(&hdout, "mop: painted image from model.");
  hdout.amin = hdata->amin;
  hdout.amax = hdata->amax;
  mrc_head_write(gfout, &hdout);     

  for( k = zmin; k <= zmax; k++){
      
    idata = (void *)mrc_readz(gfin, hdata, k);
    clearz(pdata, hdata);
    printf("Mopping section %d ", k);
    fflush(stdout);
    slice_paint(model, idata, pdata, hdata, k);
    slice_paint(model, NULL, pdata, hdata, k); 
    printf("\r");

    if (invert) {
      bidata = idata;
      sidata = idata;
      fidata = idata;     
      switch (hdata->mode)
        {
        case MRC_MODE_BYTE:
          for (i = 0; i < xysize; i++) {
            bdata[i] = bidata[i] - bdata[i];
            if (!bdata[i])
              bdata[i] = hdata->amean;
          }
          break;

        case MRC_MODE_SHORT:
          for (i = 0; i < xysize; i++)
            sdata[i] = sidata[i] - sdata[i];
          break;
             
        case MRC_MODE_FLOAT:
          for (i = 0; i < xysize; i++)
            fdata[i] = fidata[i] - fdata[i];
          break;
             
        defaut:
          break;
        }
    }
           

    for(j = ymin; j <= ymax; j++){
           
      switch (hdata->mode)
        {
        case MRC_MODE_BYTE:
          b3dFwrite( &(bdata[(j * hdata->nx) + xmin ]), 
                  psize, xmax-xmin+1, gfout);
          break;

        case MRC_MODE_SHORT:
          b3dFwrite( &(sdata[(j * hdata->nx) + xmin ]), 
                  psize, xmax-xmin+1, gfout);
          break;
             
        case MRC_MODE_FLOAT:
          b3dFwrite( &(fdata[(j * hdata->nx) + xmin ]), 
                  psize, xmax-xmin+1, gfout);
          break;
             
        default:
          break;
        }
    }

    free(idata);
  }

  return(0);
}

int clearz(void *pdata, struct MRCheader *hdata)
{

  int i;
  unsigned char *cdata;
  short *sdata;
  float *fdata;
  int xysize;

  xysize = hdata->nx * hdata->ny;
  switch (hdata->mode)
    {
    case MRC_MODE_BYTE:
      cdata = (unsigned char *)pdata;
      for (i = 0; i < xysize; i++)
        cdata[i] = 0;
      break;
    case MRC_MODE_SHORT:
      sdata = (short *)pdata;
      for (i = 0; i < xysize; i++)
        sdata[i] = hdata->amin;
      break ;
    case MRC_MODE_FLOAT:
      fdata = (float *)pdata;
      for (i = 0; i < xysize; i++)
        fdata[i] = hdata->amin;
      break;
    default:
      break;
    }
  return(0);
}



int slice_paint(struct Mod_Model *mod,
                void *idata,
                void *pdata,
                struct MRCheader *hdata,
                int z)
{

  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  struct Mod_Contour *fcont = NULL;
  int objnum, contnum;

  /* Fill Inside contour data*/
     
  if (pdata == NULL)
    return(-1);

  for (objnum = 0; objnum < mod->objsize; objnum++){
      
    obj = &(mod->obj[objnum]);
    if (obj->flags & IMOD_OBJFLAG_OPEN)
      continue;

    if (obj->flags & IMOD_OBJFLAG_OFF)
      continue;
 
    /*    if (obj->flags & IMOD_OBJFLAG_OUT) */
    /*         puts("inside out"); */

    if ((Mop_red) && (!Mop_green) && (!Mop_blue) &&
        (pdata) && (obj->red == 0.0))
      continue;

    if ((!Mop_red) && (Mop_green) && (!Mop_blue) &&
        (pdata) && (obj->green == 0.0))
      continue;

    if ((!Mop_red) && (!Mop_green) && (Mop_blue) &&
        (pdata) && (obj->blue == 0.0))
      continue;


    if ((obj->flags & IMOD_OBJFLAG_OUT) && (idata != NULL))
      continue;

    if (!idata){
      if (obj->flags & IMOD_OBJFLAG_OUT)
        printf(".");
      else
        continue;
    }
      
    if (idata)
      printf("@");
    fflush(stdout);

    for(contnum = 0; contnum < obj->contsize; contnum++){
      cont = &(obj->cont[contnum]);

      if ( cont->psize)
        if (cont->pts[0].z == z){
            
          /*            if (pdata)
                        printf(".");
                        else
                        printf("@");
                        fflush(stdout);
          */
            
          fcont = imodel_contour_scan(cont);
            
          if (fcont){
            switch(hdata->mode){
            case MRC_MODE_BYTE:
              contour_paint_byte(fcont, (unsigned char *)idata,
                                 (unsigned char *)pdata, 
                                 hdata, obj);
              break;
            case MRC_MODE_SHORT:
              printf("mop stub: Short is out of service\n");
              break;
            case MRC_MODE_FLOAT:
              printf("mop stub: Float is out of service\n");
              break;
            default:
              break;
            }
             
            if (fcont->pts)
              free(fcont->pts);
            fcont->psize = 0;
            free(fcont);
            fcont = NULL;
          }else
            printf("No paint!\n");
        }
           
    }
  }
  return(0);
}




void *mrc_readz(FILE *fin, struct MRCheader *hdata, int z)
{

  unsigned char *cdata;
  b3dInt16 *sdata;
  b3dInt32 *fdata;
  int xysize;
     
  /* DNM 1/17/04: remove rewind, seek to headerSize not 1024 */
  b3dFseek(fin, hdata->headerSize, SEEK_SET);
  xysize = hdata->nx * hdata->ny;
     

  switch (hdata->mode)
    {
    case MRC_MODE_BYTE:
      mrc_big_seek(fin, 0, z, xysize, SEEK_CUR);
      cdata = (unsigned char *)malloc(xysize);
      if (cdata == NULL)
        return(cdata);
      b3dFread(cdata, 1, xysize, fin);
      return(cdata);

    case MRC_MODE_SHORT:
      mrc_big_seek(fin, 0, z, xysize * 2, SEEK_CUR);
      sdata = (b3dInt16 *)malloc(xysize * 2);
      if (sdata == NULL)
        return(sdata);
      b3dFread(sdata, 2, xysize, fin);
      return(sdata);

    case MRC_MODE_FLOAT:
      mrc_big_seek(fin, 0, z, xysize * 4, SEEK_CUR);
      fdata = (b3dInt32 *)malloc(xysize * 4);
      if (fdata == NULL)
        return(fdata);
      b3dFread(fdata, 4, xysize, fin);
      return(fdata);

    default:
      return(NULL);
    }
}


int mop_contwhole( struct Mod_Contour *cont)
{
  int i;
  int x, y, z;

  for (i = 0; i < cont->psize; i++){
    x = cont->pts[i].x + 0.5;
    y = cont->pts[i].y + 0.5;
    cont->pts[i].x = x;
    cont->pts[i].y = y;

  }
  return(0);
}




/*****************************************************************************
 * contour_paint_byte - paints a contour with byte type data.
 *     INPUT:
 *            cont -  a contour modified by mop scan to contain pairs of
 *                    scanline points.
 *            idata - raw image data.
 *            pdata - data to paint to.
 *            hdata - MRC header struct.
 *            obj   - Model object, used for more info on contour.
 *****************************************************************************/
int contour_paint_byte(struct Mod_Contour *cont,
                       unsigned char *idata,
                       unsigned char *pdata,
                       struct MRCheader *hdata,
                       struct Mod_Object *obj)
{
  int next, lx;
  int i, x, y;
  int j, bgnpt,endpt;
  int xmin, xmax;

  if (pdata == NULL)
    return(0);


  for(i = 0; i < cont->psize - 1; i++){

    y = cont->pts[i].y;
    bgnpt = i;
    while (cont->pts[i].y == cont->pts[i+1].y){
      ++i;
      if (i >= cont->psize){
        i = cont->psize - 1;
        break;
      }
    }
    endpt = i;


    /* check for odd amount of scans, shouldn't happen! */
    if (! ( (endpt-bgnpt)% 2)){

      xmin = cont->pts[bgnpt].x;
      xmax = cont->pts[endpt].x;
     
      /* BUG FIX WORK AROUND */
      if (xmin >= cont->surf){
        for(x = xmin; 
            (x <= xmax) && (x < hdata->nx) && (y < hdata->ny);
            x++){
          if (idata != NULL)
            pdata[x + (y * hdata->nx)] = 
              idata[x + (y * hdata->nx)];
          else
            pdata[x + (y * hdata->nx)] = 0;
        }
      }
    }

    else{
      for(j = bgnpt; j < endpt; j++){
        xmin = cont->pts[j].x;
        xmax = cont->pts[j+1].x;
            
        if (xmin >= cont->surf)
          for(x = xmin; 
              (x <= xmax) && (x < hdata->nx) && (y < hdata->ny);
              x++){
            if (idata != NULL)
              pdata[x + (y * hdata->nx)] = 
                idata[x + (y * hdata->nx)];
            else
              pdata[x + (y * hdata->nx)] = 0;
          }
        j++;
      }
    }
  }

  return(0);
}

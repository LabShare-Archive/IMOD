/*  IMOD VERSION 2.50
 *
 *  imodview.c -- Handle the ImodView structure.
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
Log at end of file
*/
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <qdir.h>
#include "imod_cachefill.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_info_cb.h"
#include "imod_io.h"
#include "imod_edit.h"
#include "imod_moviecon.h"
#include "imod_iscale.h"
#include "iproc.h"
#include "autox.h"
#include "imod_workprocs.h"
#include "preferences.h"

static int ivwSetCacheFromList(ImodView *iv, Ilist *ilist);
static int ivwManageInitialFlips(ImodView *iv);
static int ivwCheckLinePtrAllocation(ImodView *iv, int ysize);
static int ivwGetPixelBytes(int mode);
static int  ivwLoadIMODifd(ImodView *iv);
static int  ivwPlistBlank(ImodView *iv, int cz);

static int loadingImage = 0;

/*
 *
 *  Image data service functions.
 *
 */


/* Get the current Section, could be X or Y in future. */     
unsigned char **ivwGetCurrentSection(ImodView *iv)
{
  int cz = (int)(iv->zmouse + 0.5f);
  return(ivwGetZSection(iv, cz));
}

unsigned char **ivwGetCurrentZSection(ImodView *iv)
{
  int cz = (int)(iv->zmouse + 0.5f);
  return(ivwGetZSection(iv, cz));
}

/* Get a section at potentially different time from current time */
unsigned char **ivwGetZSectionTime(ImodView *iv, int section, int time)
{
  int oldTime;
  unsigned char **imageData;

  if (!iv) return NULL;
  if (!iv->nt) return(ivwGetZSection(iv, section));
  if (time < 1) return(NULL);
  /* DNM: make test > instead of >= */
  if (time > iv->nt) return(NULL);

  ivwGetTime(iv, &oldTime);
  if (time == oldTime) return(ivwGetZSection(iv, section));

  iv->ct = time;
  iv->hdr = iv->image = &iv->imageList[time-1];
  /* DNM: need to change directory before reopening, and back afterwards */
  if (!Imod_IFDpath.isEmpty())
    QDir::setCurrent(Imod_IFDpath);
  iiReopen(iv->image);
  if (!Imod_IFDpath.isEmpty())
    QDir::setCurrent(Imod_cwdpath);
  imageData = ivwGetZSection(iv, section);
  iiClose(iv->image);
  iv->ct = oldTime;
  iv->hdr = iv->image = &iv->imageList[oldTime-1];
  return(imageData);
}

/* DNM 12/12/01: make this a function, do it only if not raw images
   This is what scales data into restricted range with 8-bit colormap */
void ivwScaleDepth8(ImodView *iv, ivwSlice *tempSlicePtr)
{
  int rbase = iv->rampbase;
  float scale = iv->rampsize/256.0f;
  int pix, i;
  int mi = tempSlicePtr->sec->xsize * tempSlicePtr->sec->ysize;
  unsigned char *id = tempSlicePtr->sec->data.b;
  if (App->depth == 8 && !iv->rawImageStore){
    for(i = 0; i < mi; i++){
      pix   = (int)(id[i] * scale + rbase);
      id[i] = (unsigned char)pix;
    }
  }
}

/* Returns pointer to raw image data for given z section. */
/* DNM 12/13/01: rewrote to use a use count for priority, instead of 
   rearranging the cache array every time */
/* DNM 9/15/03: rewrote to return line pointers */
unsigned char **ivwGetZSection(ImodView *iv, int section)
{
  ivwSlice *tempSlicePtr = NULL;
  int sl, slmin, minused, pixSize;
  int cz = section;

  if (section < 0 || section >= iv->zsize) 
    return(NULL);
  if (!iv->fp || iv->fakeImage)
    return(NULL);
  if (ivwPlistBlank(iv, section)) 
    return(NULL);

  /* Plain, uncached data: make line pointers if not flipped */
  if (!iv->vmSize) {
    if (iv->li->axis == 3) {
      return (ivwMakeLinePointers(iv, iv->idata[section], iv->xsize,
                                  iv->ysize, iv->rawImageStore));
    }
    else {
      /* If flipped, check the pointer allocation, get the */
      if (ivwCheckLinePtrAllocation(iv, iv->ysize))
        return(NULL);
      pixSize = ivwGetPixelBytes(iv->rawImageStore);
      for (sl = 0; sl < iv->ysize; sl++)
        iv->linePtrs[sl] = iv->idata[sl] + iv->xsize * pixSize * section;
      return (iv->linePtrs);
    }
  }

  /* Cached data: check last used one before searching */
  sl = iv->vmLastUsed;
  if ((iv->vmCache[sl].cz == cz) &&
      (iv->vmCache[sl].ct == iv->ct)) {
    tempSlicePtr = &(iv->vmCache[iv->vmLastUsed]);

  } else {

    /* search in cache for section and time */
    for (sl = 0; sl < iv->vmSize; sl++){
      if ((iv->vmCache[sl].cz == cz) &&
          (iv->vmCache[sl].ct == iv->ct)){
        tempSlicePtr = &(iv->vmCache[sl]);
        break;
      }
    }
  }

  /* Didn't find slice in cache, need to load it in. */
  if (!tempSlicePtr){

    /* DNM 12/12/01: add call to cache filler */
    if (icfGetAutofill())
      return(ivwMakeLinePointers(iv, icfDoAutofill(iv, cz), iv->xsize,
                                 iv->ysize, iv->rawImageStore));

    /* Find oldest slice to replace */
    minused = iv->vmCount + 1;
    for (sl = 0; sl < iv->vmSize; sl++)
      if (iv->vmCache[sl].used < minused) {
        minused = iv->vmCache[sl].used;
        slmin = sl;
      }

    sl = slmin;
    tempSlicePtr = &(iv->vmCache[sl]);
          
    /* Load in image */
    ivwReadZ(iv, tempSlicePtr->sec->data.b, section);

    ivwScaleDepth8(iv, tempSlicePtr);

    tempSlicePtr->cz = section;
    tempSlicePtr->ct = iv->ct;
  }

  /* Adjust use count, assign to slice, record it as last used */
  iv->vmCount++;
  tempSlicePtr->used = iv->vmCount;
  iv->vmLastUsed = sl;

  return (ivwMakeLinePointers(iv, tempSlicePtr->sec->data.b, 
                              tempSlicePtr->sec->xsize,
                              tempSlicePtr->sec->ysize, iv->rawImageStore));
}

/* Determine size of data unit */
static int ivwGetPixelBytes(int mode)
{
  switch (mode) {
  case MRC_MODE_BYTE:
    return 1;
  case MRC_MODE_SHORT:
    return 2;
  case MRC_MODE_FLOAT:
  case MRC_MODE_COMPLEX_SHORT:
    return 4;
  case MRC_MODE_COMPLEX_FLOAT:
    return 8;
  case MRC_MODE_RGB:
    return 3;
  }
  return 1;
}

/* Allocate or reallocate if necessary to get the array big enough */
static int ivwCheckLinePtrAllocation(ImodView *iv, int ysize)
{
  if (ysize > iv->linePtrMax) {
    if (iv->linePtrMax)
      free(iv->linePtrs);
    iv->linePtrs = (unsigned char **)malloc (ysize * sizeof(unsigned char *));
    if (!iv->linePtrs) {
      iv->linePtrMax = 0;
      return 1;
    }
    iv->linePtrMax = ysize;
  }
  return 0;
}

/* Make a list of line pointers from a contiguous data slice that is xsize by
   ysize, of the given mode */
unsigned char **ivwMakeLinePointers(ImodView *iv, unsigned char *data,
                                    int xsize, int ysize, int mode)
{
  int i;
  int pixSize = 1;

  if (!data)
    return (NULL);

  if (ivwCheckLinePtrAllocation(iv, ysize))
    return NULL;

  pixSize = ivwGetPixelBytes(mode);

  /* Make up the pointers */
  for (i = 0; i < ysize; i++)
    iv->linePtrs[i] = data + pixSize * xsize * i;
  return (iv->linePtrs);
}

/*
 * Routines for getting a value based on type of data 
 */

int (*best_ivwGetValue)(ImodView *iv, int x, int y, int z);

int ivwGetValue(ImodView *iv, int x, int y, int z)
{
  return((*best_ivwGetValue)(iv, x, y, z));
}

int idata_ivwGetValue(ImodView *iv, int x, int y, int z)
{
  /* DNM: calling routine is responsible for limit checks */
  if (iv->li->axis == 3)
    return(iv->idata[z][x + (y * iv->xsize)]);
  else
    return(iv->idata[y][x + (z * iv->xsize)]);
}

int fileScale_ivwGetValue(ImodView *iv, int x, int y, int z)
{
  int rval;
  float val = ivwGetFileValue(iv, x, y, z);
  val *= iv->li->slope;
  rval = (int)(val + iv->li->offset);
  if (rval < iv->li->imin)
    rval = iv->li->imin;
  else
    rval = iv->li->imax;
  return(rval);
}

int cache_ivwGetValue(ImodView *iv, int x, int y, int z)
{
  ivwSlice *tempSlicePtr = 0;
  unsigned char *image;
  int sl = iv->vmLastUsed;

  /* find pixel in cache */
  /* DNM 12/13/01: look first in last used slice */
  if (iv->vmCache[sl].cz == z && iv->vmCache[sl].ct == iv->ct) {
    tempSlicePtr = &iv->vmCache[sl];
  } else {
    for(sl = 0; sl < iv->vmSize; sl++){
      if (iv->vmCache[sl].cz == z && iv->vmCache[sl].ct == iv->ct){
        tempSlicePtr = &iv->vmCache[sl];
        break;
      }
    }
  }

  if (!tempSlicePtr)
    return(0);

  image = tempSlicePtr->sec->data.b;
  if (!image) return(0);
     
  /* DNM: calling routine is responsible for limit checks */

  return(image[(y * tempSlicePtr->sec->xsize) + x]);
}

int fake_ivwGetValue(ImodView *iv, int x, int y, int z)
{
  return(0);
}

/*
 * Routines for fast access from slicer, tumbler, and xyz
 */

/* Static variables to hold the essentials after it is set up */
static int imdataxsize;
static int *vmdataxsize;
static unsigned char **imdata;
static int imdataMax = 0;
static int vmnullvalue;

/* A global variable with the appropriate function - because I had trouble
   returning that from the setup function */
int (*ivwFastGetValue)(int x, int y, int z);

/* The access routines */
static int idata_GetValue(int x, int y, int z)
{
  return(imdata[z][x + (y * imdataxsize)]);
}

static int flipped_GetValue(int x, int y, int z)
{
  return(imdata[y][x + (z * imdataxsize)]);
}

static int cache_GetValue(int x, int y, int z)
{
  if (!imdata[z])
    return(vmnullvalue);
  return(imdata[z][x + (y * vmdataxsize[z])]);
}

static int fake_GetValue(int x, int y, int z)
{
  return(0);
}

/* To set up the tables and set the appropriate function */
int ivwSetupFastAccess(ImodView *vi, unsigned char ***outImdata,
                       int inNullvalue, int *cacheSum)
{
  int size = vi->zsize;
  int i, iz;

  *cacheSum = 0;

  if (!vi->vmSize && vi->li->axis == 2)
    size = vi->ysize;

  /* If array(s) are not big enough, get new ones */
  if (imdataMax < size) {
    if (imdataMax) {
      free(imdata);
      if (vi->vmSize)
        free(vmdataxsize);
    }
    imdata = (unsigned char **) malloc(sizeof(unsigned char *) * size);
    if (!imdata) {
      imdataMax = 0;
      return 1;
    }
    if (vi->vmSize) {
      vmdataxsize = (int *)malloc(sizeof(int) * size);
      if (!vmdataxsize) {
        free(imdata);
        imdataMax = 0;
        return 1;
      }
    }
    imdataMax = size;
  }

  if (vi->fakeImage) {
    ivwFastGetValue = fake_GetValue;

  } else if (vi->vmSize) {

    /* Cached data: fill up pointers that exist */
    for (i = 0; i < size; i++)
      imdata[i] = NULL;
    for (i = 0; i < vi->vmSize; i++) {
      iz = vi->vmCache[i].cz;
      if (iz < size && iz >= 0 &&
          vi->vmCache[i].ct == vi->ct){
        imdata[iz] = vi->vmCache[i].sec->data.b;
        vmdataxsize[iz] = vi->vmCache[i].sec->xsize;
	*cacheSum += iz;
      }
    }
    
    vmnullvalue = inNullvalue;
    ivwFastGetValue = cache_GetValue;

  } else {
    /* for loaded data, get pointers from idata */

    for (i = 0; i < size; i++)
      imdata[i] = vi->idata[i];
    imdataxsize = vi->xsize;
    if (vi->li->axis == 3)
      ivwFastGetValue = idata_GetValue;
    else
      ivwFastGetValue = flipped_GetValue;
  }
  *outImdata = imdata;
  return 0;
}



/****************************************************************************/


/* DNM 1/19/03: eliminated ivwShowstatus in favor of imod_imgcnt */

/* default settings for the view info structure. */
void ivwInit(ImodView *vi)
{
  if (!vi)
    return;
  vi->xmouse = vi->ymouse = vi->zmouse = 0.0f;
  /*     vi->xtrans = vi->ytrans = vi->ztrans = 0; */

  vi->xmovie = vi->ymovie = vi->zmovie = vi->tmovie = 0;
  vi->xsize  = vi->ysize  = vi->zsize  = 0;
  vi->xysize = 0;

  vi->nt = 0; vi->ct = 0;
     
  imcSetMovierate(vi, 0);

  vi->vmSize     = 0;
  vi->black      = 0;
  vi->white      = 255;
  vi->fastdraw   = 0;
  vi->dim        = 1+2+4;
  vi->ax         = NULL;
  vi->ctrlist    = NULL;

  vi->imod       = NULL;
  vi->idata      = NULL;
  vi->fp         = NULL;

  vi->imageList  = NULL;

  vi->movieInterval = 17L;
  vi->timers = new ImodWorkproc(vi);
  vi->movieRunning = 0;
  vi->ghostmode = 0;
  vi->ghostlast = IMOD_GHOST_SECTION;
  vi->ghostdist = 1;
  vi->obj_moveto = 1;
  vi->drawcursor = TRUE;
  vi->insertmode = 0;

  vi->fakeImage     = 0;
  vi->rawImageStore = 0;
  vi->extraObj = imodObjectNew();
  vi->linePtrs = NULL;
  vi->linePtrMax = 0;
}

/* After we are all done with the Cache free it.
 */
void ivwFreeCache(ImodView *vi)
{
  int i;
  for(i = 0; i < vi->vmSize; i++)
    if (vi->vmCache[i].sec)
      sliceFree(vi->vmCache[i].sec);
  free(vi->vmCache);
  return;
}

/*
 *  Sets the image Cache so that is contains no data. 
 */
void ivwFlushCache(ImodView *vi)
{
  int i;
  for(i = 0; i < vi->vmSize; i++){
    vi->vmCache[i].cz = -1;
    vi->vmCache[i].ct = 0;
    vi->vmCache[i].used = -1;
  }
  vi->vmLastUsed = 0;
  vi->vmCount = 0;
  return;
}

/* Initialize the cache to number of slices in vmSize */
int ivwInitCache(ImodView *vi)
{
  int i;
  int xsize = vi->li->xmax - vi->li->xmin + 1;
  int ysize = vi->li->ymax - vi->li->ymin + 1;

  if (vi->li->axis == 2)
    ysize = vi->li->zmax - vi->li->zmin + 1;

  vi->vmLastUsed = 0;
  vi->vmCount = 0;

  /* printf("xsize %d  ysize %d  vmsize %d\n", xsize, ysize, vi->vmSize); */
  /* get array of slice structures */
  vi->vmCache = (ivwSlice *)malloc(sizeof(ivwSlice) * vi->vmSize);
  if (!vi->vmCache)
    return(9);
   
  for(i = 0; i < vi->vmSize; i++)
    vi->vmCache[i].sec = NULL;
     
  /* get a slice array for each slice, mark each slice as empty */
  for(i = 0; i < vi->vmSize; i++){
    if (vi->rawImageStore)
      vi->vmCache[i].sec = sliceCreate
        (xsize, ysize, vi->hdr->mode);
    else
      vi->vmCache[i].sec = sliceCreate
        (xsize, ysize, MRC_MODE_BYTE);
 
    /*  printf("cache %d : %d x %d\n", i, xsize, ysize); */
 
    if (!vi->vmCache[i].sec){
      ivwFreeCache(vi);
      return(10);
    }
    vi->vmCache[i].cz = -1;
    vi->vmCache[i].ct = 0;
    vi->vmCache[i].used = -1;
  }
  return(0);
}

/* DNM 2/11/01: Once image sizes are set up, this routine interprets an entered
   cache size in megabytes, determines actual size needed for a montage and
   size needed for multiple files, and then sets the cache size from the
   eneterd value or the needed size, as appropriate */
static int ivwSetCacheSize(ImodView *vi)
{
  int xsize = vi->li->xmax - vi->li->xmin + 1;
  int ysize = vi->li->ymax - vi->li->ymin + 1;
  int zsize = vi->li->zmax - vi->li->zmin + 1;
  int dzsize = zsize;
  int pixSize = 1;
  int i;

  if (vi->rawImageStore){
    if (vi->hdr->mode == MRC_MODE_SHORT) pixSize = 2;
    else if (vi->hdr->mode == MRC_MODE_FLOAT) pixSize = 4;
    else if (vi->hdr->mode == MRC_MODE_RGB)   pixSize = 3;
  }
     
  if (!xsize || !ysize || !zsize)
    return(-1);

  /* If negative size, it is megabytes, convert to sections */
  if (vi->vmSize < 0) {
    vi->vmSize = (int)((-1000000. * vi->vmSize) / 
                       (xsize * ysize * pixSize));
    if (!vi->vmSize)
      vi->vmSize = 1;
  }

  if (vi->li->plist){
    /* For montage, make the maximum cache size be the minimum of the 
       number of sections with data and the number actually being 
       loaded */
    dzsize = vi->li->pdz;
    if (dzsize < 1) 
      dzsize = 1;
    if (zsize < dzsize)
      dzsize = zsize;

    /* find first actually existing data and set mouse there */
    vi->zmouse  = zsize;
    for(i = 0; i < vi->li->plist; i++)
      if (vi->zmouse > vi->li->pcoords[(3*i)+2] - vi->li->zmin)
        vi->zmouse = vi->li->pcoords[(3*i)+2] - vi->li->zmin;
  } else
    /* For non-montage, maximum cache is size * number of files */
    dzsize *= (vi->nt > 0) ? vi->nt : 1;

  /* If no entry, just take the maximum size */
  /* Otherwise, limit the entry to the maximum size needed */
  if (!vi->vmSize)
    vi->vmSize = dzsize;
  else if (vi->vmSize > dzsize)
    vi->vmSize = dzsize;

  return 0;
}

/* Load the mrc file: called with regular non-montaged data,
   cached regular single-file data, or montaged image that was entered with
   a piece list file or with piece coordinates in the file header */
int ivwLoadMrc(ImodView *vi)
{
  int xsize = vi->li->xmax - vi->li->xmin + 1;
  int ysize = vi->li->ymax - vi->li->ymin + 1;
  int zsize = vi->li->zmax - vi->li->zmin + 1;
  int i;
  int eret;
  int t;

  /* DNM 1/25/02: move the setting of these variable up from the bottom
     so that movie controller is OK if it's opened before loading is done,
     also simplify print statement */
  vi->xsize  = xsize;
  vi->ysize  = ysize;
  vi->zsize  = zsize;
  vi->xysize = xsize * ysize;

  // Set info window up now that size is known
  ImodPrefs->setInfoGeometry();

  /* Get a cache size set properly if piece list or -C entry was made */
  if (vi->li->plist || vi->vmSize)
    ivwSetCacheSize(vi);

  /* DNM: only one mode won't work now; just exit in either case */
  if (vi->vmSize)
    if (vi->hdr->mode == MRC_MODE_COMPLEX_SHORT){
      imodError(NULL, "3DMOD Error: "
              "Image cache and piece lists do not work with "
              "complex short data.\n");
      exit(-1);
    }

  if (vi->vmSize){
    vi->idata = NULL;
          
    /* initialize cache, make sure axis is set for all data structures 
       Set axis to 3 for first initialization because vmSize has been 
       computed based on unflipped Z dimensions */
    i = vi->li->axis;
    vi->li->axis = 3;
    eret = ivwInitCache(vi);
    if (eret) return(eret);
    vi->li->axis = i;
          
    if (vi->nt){
      for(t = 0; t < vi->nt; t++){
        vi->imageList[t].axis = vi->li->axis;
      }
    }else{
      if (vi->imageList)
        vi->imageList->axis = vi->li->axis;
    }
    if (vi->image)
      vi->image->axis = vi->li->axis;
    if (vi->hdr)
      vi->hdr->axis = vi->li->axis;

    vi->li->imin = 0;
    vi->li->imax = 255;
    vi->li->slope  = 1.0f;
    vi->li->offset = 0.0f;
          
    best_ivwGetValue = cache_ivwGetValue;
    ivwSetScale(vi);

  }else{

    /* Finally, here is what happens for regular, non-cached data */

    best_ivwGetValue = idata_ivwGetValue;
    vi->idata = (unsigned char **)imod_io_image_load
      (vi->image, vi->li, imod_imgcnt);
    if (!vi->idata){
      /* Let caller do error message */
      return(-1);
    }
  }

  return(0);
}

/* DNM 2/16/01: fairly sure that this routine is not needed, and possibly not
   correct, because with cached data the file's slope and offset is used, not
   vi->li's */
int ivwSetScale(ImodView *vi)
{
  /* DNM: calculate scaling, first from image min & max; then from li->smin
     and max if those are 0, then default 0 to 255 */
  float min   = vi->image->imin;
  float max   = vi->image->imax;
  int   black = vi->li->black;
  int   white = vi->li->white;
  float range, rscale, slope;
     
  if (min == max) {
    min = vi->li->smin;
    max = vi->li->smax;
  }
  if (min == max) {
    min = 0;
    max = 255;
  }

  /* Set these to the current min and max now */
  vi->li->smax = max;
  vi->li->smin = min;
  /* printf("min %f  max %f black %d white %d\n", min, max, black, white);*/
     
  range = white - black + 1;
  if (!range) range = 1;
  rscale = 256.0 / (float)range;
     
  slope = 255.0 / (max - min);
     
  vi->li->slope  = slope * rscale;
  vi->li->offset = -(( ((float)black / 255.0) *
                       (max - min)) + min) * slope;
  /* printf ("set slope %f  offset %f\n", vi->li->slope, vi->li->offset); */
  return(1);
}


/* increases the x scanline size so that the data is long word aligned. */
/* Outdated: used for IrisGL */
/*
  int ivwScanWordAlign(ImodView *vw)
  {
  unsigned char *buf;
  int i, j, k;
  int jo, jao;
  int nxs;
  int pad;

  pad = vw->xsize % 4;
  if (!pad)
  return(0);
  pad = (pad *  -1) + 4;
  nxs = pad + vw->xsize;

  if (vw->li->contig)
  return(-1);

  for(k = 0; k < vw->zsize; k++){
  buf = (unsigned char *)malloc(nxs * vw->ysize);
  for(j = 0; j < vw->ysize; j++){
  jo  = j * vw->xsize;
  jao = j * nxs;
  for(i = 0; i < vw->xsize; i++){
  buf[jao+i]=vw->idata[k][jo+i];
  }
  for(i = vw->xsize; i < nxs; i++){
  buf[jao+i]=0;
  }
  }
  free(vw->idata[k]);
  vw->idata[k] = buf;
  }
  vw->xsize += pad;
  vw->xysize = vw->xsize * vw->ysize;
  return(0);
  }
*/


/* flip a tomogram */
/* DNM 9/15/03: removed actual flipping code now that line pointers are
   used to access images */
int ivwFlip(ImodView *vw)
{
  unsigned char **idata;
  unsigned char *trow, *tflag;
  unsigned char *inrow, *outrow;
  int nx, ny, nz;
  int i, j, k, t;
  int kstore, nextk;
  unsigned int nyz;
  int oymouse, ozmouse;


  if (vw->li->plist){
    wprint("\nSorry, Image Data can't be flipped when using "
           "piece lists.");
    return(-1);
  }

  if (!vw->flippable){
    wprint("\nSorry, these image data can't be flipped.");
    return(-1);
  }

  /* DNM 12/10/02: if loading image, flip the axis but defer until done */
  if (loadingImage) {
    vw->li->axis =  (vw->li->axis == 2) ? 3 : 2;
    return (1);
  }

  oymouse = (int)(vw->ymouse + 0.5f);
  ozmouse = (int)(vw->zmouse + 0.5f);

  wprint("Flipping image data.\n");

  /* DNM: restore data before flipping, as well as resetting when done */
  iprocRethink(vw);
  nx = vw->xsize;
  ny = vw->zsize;
  nz = vw->ysize;
  nyz = ny * nz;

  vw->li->axis =  (vw->li->axis == 2) ? 3 : 2;

  if (vw->vmSize){

    /* If Image data is cached from disk */
    /* tell images to flipaxis */
          
    if ((vw->nt) && (vw->imageList)){
      for(t = 0; t < vw->nt; t++){
        vw->imageList[t].axis = vw->li->axis;
      }
    }
    if (vw->image){
      vw->image->axis = vw->li->axis;
               
    }

    ivwFreeCache(vw);
    /* DNM: if the cache size equalled old # of Z planes, set it to new
       number of planes, including ones for each file
       Otherwise, set it to occupy same amount of memory, rounding up
       to avoid erosion on repeated flips */
    t = vw->nt > 0 ? vw->nt : 1;
    if (vw->vmSize == t * vw->zsize)
      vw->vmSize = t * nz;
    else {
      vw->vmSize = (vw->vmSize * vw->ysize + ny / 2) / ny;
      if (!vw->vmSize)
        vw->vmSize = 1;
    }
    ivwInitCache(vw);
  }

  vw->xsize = nx;
  vw->ysize = ny;
  vw->zsize = nz;
  vw->xysize = vw->xsize * vw->ysize;
  vw->xmouse = 0;
  vw->ymouse = 0;
  vw->zmouse = 0;

  ivwFlipModel(vw);
  iprocRethink(vw);
  autox_newsize(vw);
  imod_info_float_clear(-1, -1);
     
  vw->ymouse = ozmouse;
  vw->zmouse = oymouse;

  /* Keep it in bounds */
  if (vw->zmouse > nz - 1)
    vw->zmouse = nz - 1;

  /* DNM: need to reset the movie controller because ny and nz changed */
  imcResetAll(vw);

  return(0);
}

/* Scale image data to fit in 8-bit colorramp. */
int ivwScale(ImodView *vw)
{
  int pix;
  float scale = 1.0;
  int rbase = 0;
  int ysize = vw->ysize;
  int xsize = vw->xsize;
  int i,j,k;

  if (vw->vmSize)
    return -1;
     
  rbase = vw->rampbase;
  scale = vw->rampsize/256.0f;

  for(k = 0; k < vw->zsize; k++)
    for(j = 0; j < ysize; j++)
      for(i = 0; i < xsize; i++){
        pix = (int)(vw->idata[k][i + (j * vw->xsize)] * scale);
        pix += rbase;
        vw->idata[k][i + (j * vw->xsize)] = pix;
      }

  return(0);
}

void ivwBindMouse(ImodView *vw)
{
  if (vw->xmouse < 0)
    vw->xmouse = 0;
  if (vw->ymouse < 0)
    vw->ymouse = 0;
  if (vw->zmouse < 0)
    vw->zmouse = 0;
  if (vw->xmouse >= vw->xsize)
    vw->xmouse = vw->xsize - 1;
  if (vw->ymouse >= vw->ysize)
    vw->ymouse = vw->ysize - 1;
  if (vw->zmouse > vw->zsize - 1)
    vw->zmouse = vw->zsize - 1;
  return;
}

void ivwGetLocation(ImodView *vw, int *x, int *y, int *z)
{
  *x = (int)(vw->xmouse);
  *y = (int)(vw->ymouse);
  *z = (int)(vw->zmouse + 0.5);
  return;
}

void ivwGetLocationPoint(ImodView *inImodView, Ipoint *outPoint)
{
  outPoint->x = inImodView->xmouse;
  outPoint->y = inImodView->ymouse;
  outPoint->z = inImodView->zmouse;
}

/* By using IMOD IFD files one can have a separete image for
 * several different time points.
 * returns number of time elements available or 0 if no time.
 * time is the current time index.
 */
int ivwGetTime(ImodView *vw, int *time)
{
  if (time){
    *time = vw->ct;
  }
  return(vw->nt);
}

/* Set the current time index.  Time index ranges from 1 to maxtime 
 * 0 is reserved for no time - meaning only one image stack is loaded. 
 */
void ivwSetTime(ImodView *vw, int time)
{
    
  if (!vw->nt){
    vw->ct = vw->imod->ctime = 0;
    return;
  }
     
  /* DNM 6/17/01: Don't do this */
  /* inputSetModelTime(vw, time); */  /* set model point to a good value. */

  if (vw->ct > 0 && !vw->fakeImage)
    iiClose(&vw->imageList[vw->ct-1]);

  vw->ct = time;
  if (vw->ct > vw->nt)
    vw->ct = vw->nt;
  if (vw->ct <= 0)
    vw->ct = 1;

  if (!vw->fakeImage){
    vw->hdr = vw->image = &vw->imageList[vw->ct-1];
    ivwSetScale(vw);

    /* DNM: need to change directory before reopening, and back after */
    if (!Imod_IFDpath.isEmpty())
      QDir::setCurrent(Imod_IFDpath);
    iiReopen(vw->image);
    if (!Imod_IFDpath.isEmpty())
      QDir::setCurrent(Imod_cwdpath);
   }
  /* DNM: update scale window */
  imodImageScaleUpdate(vw);
  vw->imod->ctime = vw->ct;
  return;
}

char *ivwGetTimeIndexLabel(ImodView *inImodView, int inIndex)
{
  if (!inImodView) return "";
  if (inIndex < 1) return "";
  if (inIndex > inImodView->nt) return "";
  if (inImodView->fakeImage) return "";
  return(inImodView->imageList[inIndex-1].description);
}

char *ivwGetTimeLabel(ImodView *inImodView)
{
  return (inImodView->image->description);
}

int  ivwGetMaxTime(ImodView *inImodView)
{
  return(inImodView->nt);
}

/* Set the global location in 3D space for all windows.
 */
void ivwSetLocation(ImodView *vw, int x, int y, int z)
{
  vw->xmouse = x;
  vw->ymouse = y;
  vw->zmouse = z;
  ivwBindMouse(vw);
  imodDraw(vw, IMOD_DRAW_ALL);
  return;
}

void ivwSetLocationPoint(ImodView *vw, Ipoint *pnt)
{
  int x,y,z;
  double fc = 0.5;

  x = (int)floor(pnt->x + fc);
  y = (int)floor(pnt->y + fc);
  z = (int)floor(pnt->z + fc);

  ivwSetLocation(vw, x, y, z);
  return;
}


/* Test whether point is on current section - need to use floor for - values */
int ivwPointVisible(ImodView *vw, Ipoint *pnt)
{
  if ((int)floor(vw->zmouse + 0.5) == (int)floor(pnt->z + 0.5))
    return(1);
  else
    return(0);
}


float ivwGetFileValue(ImodView *vw, int cx, int cy, int cz)
{
  /* cx, cy, cz are in model file coords. */
  /* fx, fy, fz are in image file coords. */
  /* px, py, pz are in piece list coords. */
  int fx, fy, fz, tmp;
  FILE *fp = NULL;
  struct MRCheader *mrcheader = NULL;

  if (!vw->image) return 0.0f;
  if (vw->image->file != IIFILE_MRC) return 0.0f;
  fp = vw->image->fp;
  mrcheader = (struct MRCheader *)vw->image->header;

  if (vw->li){

    /* get to index values in file from screen index values */
    fx = cx + vw->li->xmin;
    fy = cy + vw->li->ymin;
    fz = cz + vw->li->zmin;
    if (vw->li->axis){
      switch(vw->li->axis){
      case 1:
        tmp = fx;
        fx = fz;
        fz = tmp;
        break;
      case 2:
        tmp = fy;
        fy = fz;
        fz = tmp;
        break;
      case 3:
      default:
        break;
      }
    }

    if (vw->li->plist){

      /* montaged: find piece with coordinates in it and get data 
         there */
      int px, py, pz;
      int i, mi = vw->li->plist;
      px = fx; py = fy; pz = fz;
      for(i = 0; i < mi; i++){
        if (pz == vw->li->pcoords[(i*3)+2]){
                        
          if ((px >= vw->li->pcoords[(i*3)]) &&
              (px <= (vw->li->pcoords[(i*3)]+vw->hdr->nx)) &&
              (py >= vw->li->pcoords[(i*3)+1]) &&
              (py <= (vw->li->pcoords[(i*3)+1]+vw->hdr->ny)
               )){
            fz = i;
            fx = px - vw->li->pcoords[(i*3)];
            fy = py - vw->li->pcoords[(i*3)+1];
            return(mrc_read_point
                   (fp, mrcheader, fx, fy, fz));
          }
        }
      }
      return(vw->hdr->amean);
    }
    return(mrc_read_point(fp, mrcheader, fx, fy, fz));
  }
  return(mrc_read_point(fp, mrcheader, cx, cy, cz));
}


static unsigned char *plistBuf = 0;
static int plistBufSize=0;

int ivwPlistBlank(ImodView *iv, int cz)
{
  int i, mi = iv->li->plist;
  if (!mi) return(mi);
  cz += iv->li->zmin;
  for(i = 0; i < mi; i++)
    if (iv->li->pcoords[(i*3)+2] == cz)
      return(0);
  return(1);
}

void deletePlistBuf(void)
{
  if (plistBuf) free(plistBuf);
  plistBuf = NULL;
  plistBufSize = 0;
}

void memreccpy
(unsigned char *tb,             /* copy data to buffer */
 unsigned char *fb,             /* copy data from buffer */
 int xcpy, int ycpy, int psize, /* amount/size of data to copy. */
 int tskip, int tox, int toy,   /* to buffer offsets, skip. */
 int fskip, int fox, int foy)   /* from buffer offsets, skip. */
{
  register unsigned int y, my;

  /* initialize both to and from buffers. */
  tb += tox*psize; 
  fb += fox*psize;
  tb += ((xcpy+tskip) * toy) * psize;
  fb += ((xcpy+fskip) * foy) * psize;

  my = ycpy; /* set max y value */
  xcpy *= psize;
  tskip*=psize;
  fskip*=psize;
  tskip+=xcpy;
  fskip+=xcpy;
  for (y = 0; y < my; y++){
    memcpy(tb, fb, xcpy);
    tb+=tskip;
    fb+=fskip;
  }
  return;
}

/* Read a section of data into the cache */
void ivwReadZ(ImodView *iv, unsigned char *buf, int cz)
{

  /* Image in not a stack but loaded into pieces. */
  if (iv->li->plist){
  
    int mx, my; /* the size of the section buffer */
    int ox, oy; /* data offset/origin  */
    unsigned int i, mxy, bxy;
    int pixSize = 1;

    mx = iv->xsize;    my = iv->ysize;
    ox = iv->li->xmin; oy = iv->li->ymin;
    mxy = mx * my;
    /* DNM: make the buffer the size of input pieces */
    bxy = iv->hdr->nx * iv->hdr->ny;

    /* Future: 
     * Image data could be stored as 16bit, floats or rgb.
     */
    if (iv->rawImageStore){
      if (iv->hdr->mode == MRC_MODE_SHORT) pixSize = 2;
      else if (iv->hdr->mode == MRC_MODE_FLOAT) pixSize = 4;
      else if (iv->hdr->mode == MRC_MODE_RGB)   pixSize = 3;
    }

    /* Clear image buffer we will write to. */
    memset(buf, 0, mxy * pixSize);

    /* Setup load buffer. */
    if (plistBufSize == -1){
      atexit(deletePlistBuf);
      plistBufSize = bxy;
      plistBuf = (unsigned char *)malloc(plistBufSize * pixSize);
      if (!plistBuf) plistBufSize = 0;
      return;
    }
    if (plistBufSize < bxy){
      deletePlistBuf();
      plistBufSize = bxy;
      plistBuf = (unsigned char *)malloc(plistBufSize * pixSize);
      if (!plistBuf) plistBufSize = 0;
    }
    cz += iv->li->zmin;

    /* Check each piece and copy its parts into the section. */
    for(i = 0; i < iv->li->plist; i++){
      int iox, ioy, fox, foy;
      int xsize, ysize;
      int fskip, iskip;
      int llx, lly, urx, ury;

      if ( iv->li->pcoords[(i*3)+2] == cz){
        iox = iv->li->pcoords[(i*3)];
        ioy = iv->li->pcoords[(i*3)+1];

        /* DNM: compute the bounding coordinates to read in, and
           skip if there is nothing that overlaps the image */
        llx = ox - iox;
        if (llx < 0)
          llx = 0;
        urx = iv->li->xmax - iox;
        if (urx >= iv->hdr->nx)
          urx = iv->hdr->nx - 1;

        lly = oy - ioy;
        if (lly < 0)
          lly = 0;
        ury = iv->li->ymax - ioy;
        if (ury >= iv->hdr->ny)
          ury = iv->hdr->ny - 1;

        if (llx > urx || lly > ury)
          continue;
                    
        iv->image->llx = llx;
        iv->image->urx = urx;
        iv->image->lly = lly;
        iv->image->ury = ury;

        if (iv->rawImageStore)
          iiReadSection(iv->image, (char *)plistBuf, i);
        else
          iiReadSectionByte(iv->image, (char *)plistBuf, i);

        /* set up size of copy, offsets and skip on each line
           for copying into image buffer */
        xsize = urx + 1 - llx;
        ysize = ury + 1 - lly;
        iskip = mx - xsize;
        iox += llx - ox;
        ioy += lly - oy;
        fox = foy = 0;
        fskip = 0;

        /* Draw our piece into the image buffer. */
        memreccpy(buf, plistBuf, 
                  xsize, ysize, pixSize,
                  iskip, iox, ioy, 
                  fskip, fox, foy);
      }
    }
    return;
  }

  /* normal data, YZ flipped */
  if (iv->li->axis == 2){
    if (iv->rawImageStore)
      iiReadSection(iv->image, (char *)buf, cz + iv->li->ymin);
    else
      iiReadSectionByte(iv->image, (char *)buf, cz + iv->li->ymin);
    return;
  }

  /* normal data, unflipped */
  if (iv->rawImageStore) 
    iiReadSection(iv->image, (char *)buf, cz + iv->li->zmin);
  else
    iiReadSectionByte(iv->image, (char *) buf, cz + iv->li->zmin);

  return;
}

/* DNM: scan through all contours and set wild flag if Z is not the 
   same throughout */

void ivwCheckWildFlag(Imod *imod)
{
  int ob, co, pt;
  Iobj *obj;
  Icont *cont;
     
  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      cont->flags &= ~ICONT_WILD;
      for (pt = 1; pt < cont->psize; pt++){
        if (cont->pts[0].z != cont->pts[pt].z) {
          cont->flags |= ICONT_WILD;
          break;
        }
      }
    }
  }

}


/*
 * Return the current image coordinate system.
 * This will take into account the transformations
 * in the MRC header along with the command line
 * sub area options.
 *
 * Fixed 3-17-96 Transform for the -Y command line option.
 *
 */
IrefImage *ivwGetImageRef(ImodView *iv)
{
  IrefImage *ref = (IrefImage *) malloc (sizeof(IrefImage));
  float xscale, yscale, zscale;
  float xtrans, ytrans, ztrans;
  float xrot, yrot, zrot;
     
  if (!ref) return(NULL);

  xscale = iv->image->xscale;
  yscale = iv->image->yscale;
  zscale = iv->image->zscale;
  xtrans = iv->image->xtrans;
  ytrans = iv->image->ytrans;
  ztrans = iv->image->ztrans;
  xrot = iv->image->xrot;
  yrot = iv->image->yrot;
  zrot = iv->image->zrot;

  ref->oscale.x = xscale;
  ref->oscale.y = yscale;
  ref->oscale.z = zscale;

  /* DNM 11/5/98: need to scale the load-in offsets before adding them */
  ref->otrans.x = xtrans - xscale * iv->li->xmin;
  ref->otrans.y = ytrans - yscale * iv->li->ymin;
  ref->otrans.z = ztrans - zscale * iv->li->zmin;

  /* DNM 12/19/98: if using piece lists, need to subtract the minimum
     values as well */
  if(iv->li->plist) {
    ref->otrans.x -= xscale * iv->li->opx;
    ref->otrans.y -= yscale * iv->li->opy;
    ref->otrans.z -= zscale * iv->li->opz;
  }

  /* DNM 11/5/98: tilt angles were not being passed back.  Start passing
     them through so that they will start being saved in model IrefImage */
  ref->orot.x = xrot;
  ref->orot.y = yrot;
  ref->orot.z = zrot;
  return(ref);
}



void ivwSetModelTrans(ImodView *iv)
{
  Imod *imod = iv->imod;
  IrefImage *ref, *iref;

  if (!imod->refImage){
    imod->refImage = (IrefImage *) malloc (sizeof(IrefImage));
    if (!imod->refImage) return;
    ref  = imod->refImage;
    iref = ivwGetImageRef(iv);
    ref->oscale = iref->oscale;
    ref->otrans = iref->otrans;
    ref->orot   = iref->orot;
  }else{
    ref  = imod->refImage;
    iref = ivwGetImageRef(iv);
  }

  ref->cscale = iref->oscale;
  ref->ctrans = iref->otrans;
  ref->crot   = iref->orot;

  if (iv->li->axis == 2)
    imod->flags |= IMODF_FLIPYZ;
  else
    imod->flags &= ~IMODF_FLIPYZ;

  /* DNM 11/5/98: set this flag that tilt angles were properly saved */
  imod->flags |= IMODF_TILTOK;

  /* DNM 7/20/02: the old values in the model seem never to be used, so
     use otrans to store image origin information so programs can get
     back to full volume index coordinates from info in model header.
     Also set a new flag to indicate this info exists */
  ref->otrans.x = iv->image->xtrans;
  ref->otrans.y = iv->image->ytrans;
  ref->otrans.z = iv->image->ztrans;
  imod->flags |= IMODF_OTRANS_ORIGIN;

  free(iref);
}

/* Flips model IF it does not match current flip state of image */

void ivwFlipModel(ImodView *iv)
{
  /* flip model y and z */
  Imod  *imod = iv->imod;

  if (iv->li->axis == 2)
    if ((imod->flags & IMODF_FLIPYZ))
      return;

  if ((iv->li->axis == 3) || (iv->li->axis == 0))
    if (!(imod->flags & IMODF_FLIPYZ))
      return;

  if (imod->flags & IMODF_FLIPYZ)
    imod->flags &= ~IMODF_FLIPYZ;
  else
    imod->flags |= IMODF_FLIPYZ;

  imodFlipYZ(imod);
  return;
}

/* Transformes model so that it matches image coordinate system.
 */
void ivwTransModel(ImodView *iv)
{
  IrefImage *iref;


  /* If model doesn't have a reference coordinate system
   * from an image, then use this image's coordinate
   * system and return;
   */
  if ((!ImodTrans) || (!iv->imod->refImage)){
    ivwSetModelTrans(iv);
    return;
  }

  /* Try and get the coordinate system that we will
   * transform the model to match.
   */
  iref = ivwGetImageRef(iv);
  if (!iref) return;

  iref->crot   = iv->imod->refImage->crot;
  iref->ctrans = iv->imod->refImage->ctrans;
  iref->cscale = iv->imod->refImage->cscale;

  /* transform model to new coords */
  {
    Imod  *imod = iv->imod;
    Iobj  *obj;
    Icont *cont;
    Ipoint pnt;
    Imat  *mat = imodMatNew(3);
    int    ob, co, pt;
    int    me, i;
    float  meshdx, meshdy, meshdz;
    Imesh *mesh;

    if (imod->flags & IMODF_FLIPYZ) {
      imodFlipYZ(imod);
      imod->flags &= ~IMODF_FLIPYZ;
    }

    /* First transform to "absolute" image coords using old reference 
       image data */
    imodMatId(mat);
    imodMatScale(mat, &iref->cscale);

    pnt.x =  - iref->ctrans.x;
    pnt.y =  - iref->ctrans.y;
    pnt.z =  - iref->ctrans.z;
    imodMatTrans(mat, &pnt);


    /* DNM 11/5/98: because tilt angles were not properly set into the
       model data when IrefImage was created, do rotations
       only if the flag is set that tilts have been stored properly */

    if (imod->flags & IMODF_TILTOK) {
      imodMatRot(mat, - iref->crot.x, b3dX);
      imodMatRot(mat, - iref->crot.y, b3dY);
      imodMatRot(mat, - iref->crot.z, b3dZ);

      /* Next transform from these "absolute" coords to new reference
         image coords */

      imodMatRot(mat, iref->orot.z, b3dZ);
      imodMatRot(mat, iref->orot.y, b3dY);
      imodMatRot(mat, iref->orot.x, b3dX);
    }

    imodMatTrans(mat, &iref->otrans);

    pnt.x = 1. / iref->oscale.x;
    pnt.y = 1. / iref->oscale.y;
    pnt.z = 1. / iref->oscale.z;
    imodMatScale(mat, &pnt);
          
    meshdx = iref->otrans.x - iref->ctrans.x;
    meshdy = iref->otrans.y - iref->ctrans.y;
    meshdz = iref->otrans.z - iref->ctrans.z;
    for(ob = 0; ob < imod->objsize; ob++){
      obj = &(imod->obj[ob]);
      for(co = 0; co < obj->contsize; co++){
        cont = &(obj->cont[co]);
        for(pt = 0; pt < cont->psize; pt++){
          imodMatTransform(mat, &cont->pts[pt], &pnt);
          cont->pts[pt] = pnt;
        }
      }

      /* Just translate the meshes as necessary */
      for(me = 0; me < obj->meshsize; me++) {
        mesh = &obj->mesh[me];
        if (!mesh || !mesh->vsize)
          continue;
        for(i = 0; i < mesh->vsize; i += 2){
          mesh->vert[i].x += meshdx;
          mesh->vert[i].y += meshdy;
          mesh->vert[i].z += meshdz;
        }
      }
    }
    imodMatDelete(mat);
  }

  ivwFlipModel(iv);
  ivwSetModelTrans(iv);
  free(iref);
  return;
}

/* Take care of scaling the model, and flipping data and model as needed */
static int ivwManageInitialFlips(ImodView *iv)
{
  int flipit;
  int retcode;

  /* Transform model to match new image. */
  ivwTransModel(iv); 
     
  /* Unflip it if that didn't */
  if (iv->imod->flags & IMODF_FLIPYZ){
    imodFlipYZ(iv->imod);
    iv->imod->flags &= ~IMODF_FLIPYZ;
  }
     
  /* Flip data and model if called for, but only if not montage */
  flipit = (iv->li->axis == 2)?1:0;
  iv->li->axis = 3;
  if (flipit) {
    retcode = ivwFlip(iv);
    if (retcode)
      return (retcode);
  }

  /* set model max values */
  iv->imod->xmax = iv->xsize;
  iv->imod->ymax = iv->ysize;
  iv->imod->zmax = iv->zsize;

  iv->imod->csum = imodChecksum(iv->imod);

  /* DNM: check wild flag here, after all the flipping is done */
  ivwCheckWildFlag(iv->imod);
  return 0;
}    


/* Load images initially, use for all kinds of data */
int ivwLoadImage(ImodView *iv)
{
  unsigned int xsize, ysize, xysize, zsize;
  Ilist *ilist;

  if (iv->fakeImage){
    iv->xsize = iv->imod->xmax;
    iv->ysize = iv->imod->ymax;
    iv->zsize = iv->imod->zmax;
    wprint("Image size %d x %d, %d sections.\n", 
           iv->xsize, iv->ysize, iv->zsize);
    best_ivwGetValue = fake_ivwGetValue;

    /* DNM: set the axis flag based on the model flip flag */
    if (iv->li->axis == 2) 
      imodError(NULL, "The -Y flag is ignored when loading a model"
              " without an image.\nUse Edit-Image-Flip to flip the"
              " model if desired");
    iv->li->axis = 3;
    if (iv->imod->flags & IMODF_FLIPYZ)
      iv->li->axis = 2;
    return(0);
  }

  /* This is a version one Image File Descriptror, (IFD) file,
   * they contain a list of images to load.
   */
  if (iv->ifd == 1)
    return(ivwLoadIMODifd(iv));
  /*     
   *  This is not yet working.
   *  Version 2 IFD files need to be reworked anyway.
   *
   *    if (iv->ifd == 2)
   *        return(ifioLoadIMODifd(iv));
   */
  if (iv->ifd > 1) {
    b3dError(NULL, "3dmod: Image list file version too high.\n");
    return (-1);
  }

  /* multiple file option */
  if (iv->ifd < 0) {
    ilist = (Ilist *)iv->imageList;
    return(ivwSetCacheFromList(iv, ilist)); 
  }

  /*  DNM removed IFDEF here
   * In case we want to finish allowing plugins to
   * install their own image reading formats.
   */

  /*  if (imodPlugLoaded(IMOD_PLUG_FILE)){ */
  /* Let plugin load file. */
  
  /* }else { */

  /* DATA OTHER THAN IMAGE LISTS AND MULTIPLE FILE DATA */
 
  /* check load info is up to date for image header. */
  mrc_fix_li(iv->li, iv->image->nx, iv->image->ny, iv->image->nz);
  xsize = iv->li->xmax - iv->li->xmin + 1;
  ysize = iv->li->ymax - iv->li->ymin + 1;
  zsize = iv->li->zmax - iv->li->zmin + 1;
  xysize = xsize * ysize;
  
  /* DNM 9/25/03: it probably no longer matters if data are ever contiguous
     so switch to noncontiguous for anything above 1 GB */
  iv->li->contig = 1; 
  if (1000000000 / xysize < zsize)
    iv->li->contig = 0;
  /*printf("contig = %d  axis = %d  xysize %d  % dzsize %d \n", iv->li->contig,
    iv->li->axis, xysize, 4200000000 / xysize, zsize); */

  /* print load status */
  wprint("Image size %d x %d, %d sections.\n", xsize, ysize, zsize);
     
  /* copy limits to image structure, in case loading is via cache */
  iv->image->llx = iv->li->xmin;
  iv->image->lly = iv->li->ymin;
  iv->image->llz = iv->li->zmin;
  iv->image->urx = iv->li->xmax;
  iv->image->ury = iv->li->ymax;
  iv->image->urz = iv->li->zmax;

  loadingImage = 1;
  if (ivwLoadMrc(iv)){
    /* DNM 10/31/03: let top level report error */
    loadingImage = 0;
          
    return(-1);
  }
  loadingImage = 0;
     

  if (App->depth == 8)
    ivwScale(iv);
     
  return (ivwManageInitialFlips(iv));
}



/*****************************************************************************/
/*****************************************************************************/
/**** IMOD IFD Files. ****/
/*****************************************************************************/
/*****************************************************************************/
/*
 * Instead of loading an mrc image file imod can load in a text file that
 * gives a list of mrc images to load.
 *
 *****************************************************************************/

/* Returns the type of image file. 
 *   0 = Unknown, may be MRC,TIFF image file.
 *   1 = Is an IMOD image list version 0 or 1
 *   2 = Is an IMOD image list version 2
 */
int imodImageFileDesc(FILE *fin)
{
  int isifd = 0;
  char buf[128];

  if (!fin) return 0;

  rewind(fin);
  imodFgetline(fin, buf, 127);

  if (0 == strncmp("IMOD image list", buf, 15))
    isifd = 1;

  if (isifd){
    isifd = 0;

    while((imodFgetline(fin, buf, 127) > 0)){
      if (!strncmp("VERSION", buf, 7)){
        isifd = atoi(&buf[8]);
        if (!isifd)
          isifd = 1;
        rewind(fin);
        return(isifd);
      }
    }
  }
  rewind(fin);
  return(isifd);
}

/* Load the IMOD image list file description. */
#define IFDLINE_SIZE 255
static int ivwLoadIMODifd(ImodView *iv)
{
  Ilist *ilist = ilistNew(sizeof(ImodImageFile), 32);
  ImodImageFile *image;
  char line[IFDLINE_SIZE + 1];
  int retcode = 0, i;
  struct LoadInfo *li;
  int xsize = 0, ysize = 0, zsize;
  int version = 0;

  char *imgdir = NULL;
  QDir *curdir = new QDir();

  rewind(iv->fp);
  imodFgetline(iv->fp, line, IFDLINE_SIZE);

  while((imodFgetline(iv->fp, line, IFDLINE_SIZE) > 0)){

    wprint("%s\n\r",line);
    imod_info_input();

    /* clear the return from the line. */
    for(i = 0; line[i]; i++)
      if (line[i] == '\n'){
        line[i] = 0x00;
        break;
      }

    if (!strncmp("VERSION", line, 7)){
      version = atoi(&line[8]);
      continue;
    }

    /* supply size in case first file is not found. */
    if (!strncmp("SIZE", line, 4)){
      sscanf(line, "SIZE %d%*c%d%*c%d\n", &xsize, &ysize, &zsize);
      continue;
    }

    /* define a root pathname for all image files. */
    if (!strncmp("IMGDIR", line, 6)){
      if (imgdir) 
        free(imgdir);
      imgdir = strdup((curdir->cleanDirPath(QString(&line[7]))).latin1());
      continue;
    }

    /* DNM: XYZ label now supported; require one image file */
    if (!strncmp("XYZ", line, 3)){

      li = iv->li;            
      if (ilist->size == 1)
        image = (ImodImageFile *)ilistItem(ilist, ilist->size - 1);
      else {
        imodError(NULL, "3DMOD Error: " 
                "Image list file must specify one image file"
                " before the XYZ option.\n");
        exit(-1);
      }
      iiPlistLoadF(iv->fp, li, 
                   image->nx, image->ny, image->nz);

      if (li->xmin != -1)
        li->xmin -= (int)li->opx;
      if (li->xmax != -1)
        li->xmax -= (int)li->opx;
      if (li->ymin != -1)
        li->ymin -= (int)li->opy;
      if (li->ymax != -1)
        li->ymax -= (int)li->opy;
      if (li->zmin != -1)
        li->zmin -= (int)li->opz;
      if (li->zmax != -1)
        li->zmax -= (int)li->opz;
      xsize = (int)li->px;
      ysize = (int)li->py;
      zsize = (int)li->pz;
      mrc_fix_li(li, xsize, ysize, zsize);

      ivwSetCacheSize(iv);

      iv->li->axis = 3;
      iv->flippable = 0;
      continue;
    }

    if (!strncmp("TIME", line, 4)){
      if (ilist->size) {
        image = (ImodImageFile *)ilistItem(ilist, ilist->size - 1);
        if (image->description)
          free(image->description);
        image->description = strdup(&line[5]);
      }
      continue;
    }

    if (!strncmp("IMAGE", line, 5)){
      /* Load image file */
      int pathlen = strlen(&line[6]);
      char *filename = NULL;
               
      if (imgdir){
        pathlen += strlen(imgdir);
        filename = (char *)malloc(pathlen + 3);
        strcpy(filename, imgdir);
        strcat(filename, (curdir->cleanDirPath(QString(&line[6]))).latin1());
      }else{
        filename = strdup((curdir->cleanDirPath(QString(&line[6]))).latin1());
      }

      errno = 0;
      image = iiOpen((char *)
        (QDir::convertSeparators(QString(filename))).latin1(), "rb");
      if (!image){
        if (!xsize || !ysize) {
          imodError(NULL, "3DMOD Error: " 
                  "couldn't open %s, first file in image list,"
                  "\n and no SIZE specified before this.\n",
                  filename);
          exit(-1);
        }
        wprint("Warning: couldn't open %s\n\r",
               filename);
        imodError(stdout, "Warning: couldn't open %s\n%s%s", filename, 
                  errno ? "System error: " : "", errno ? strerror(errno): "");
        image = iiNew();
        image->nx = xsize;
        image->ny = ysize;
        image->nz = zsize;
        image->filename = strdup
                   ((QDir::convertSeparators(QString(filename))).latin1());
      }
      /* DNM: set up scaling for this image */
      iiSetMM(image, (double)iv->li->smin, (double)iv->li->smax);
      iiClose(image);

      /* DNM: Make filename with directory stripped be the default 
         descriptor */
      pathlen = strlen(filename);
      while (( pathlen > 0) && (filename[pathlen-1] != '/'))
        pathlen--;
      image->description = strdup(&filename[pathlen]);

      ilistAppend(ilist, image);
      /* set xsize etc from size of first file if not set */
      if (!xsize && !ysize) {
        xsize = image->nx;
        ysize = image->ny;
        zsize = image->nz;
      }

      /* DNM: set time and increment time counter here, not with
         the TIME label */
      image->time = iv->nt;
      iv->nt++;

      if (filename)
        free(filename);
      continue;
    }

    imodError(NULL, "3dmod warning: "
            "Unknown image list option (%s)\n", line);

  }
  rewind(iv->fp);
  /* end of while (getline) */

  retcode = ivwSetCacheFromList(iv, ilist);

  if (imgdir)free(imgdir);
  delete curdir;

  return(retcode);
}

void ivwMultipleFiles(ImodView *iv, char *argv[], int firstfile, int lastimage)
{
  Ilist *ilist = ilistNew(sizeof(ImodImageFile), 32);
  ImodImageFile *image;
  int pathlen, i;
  char *convarg;
  QDir *curdir = new QDir();

  for (i = firstfile; i <= lastimage; i++) {
    convarg = strdup((curdir->cleanDirPath(QString(argv[i]))).latin1());
    image = iiOpen((char *)
      (QDir::convertSeparators(QString(convarg))).latin1(), "rb");
    if (!image){
      imodError(NULL, "3DMOD Error: " 
              "couldn't open image file %s.\n", argv[i]);
      exit(-1);
    }

    /* set up scaling for this image */
    iiSetMM(image, (double)iv->li->smin, (double)iv->li->smax);

    /* Setting the fp keeps it from closing the file, so there is no need to
      save the filename too */
    iv->fp = image->fp;
    iiClose(image);

    image->time = iv->nt;
    iv->nt++;

    /* Copy filename with directory stripped to the descriptor */
    pathlen = strlen(convarg);
    while (( pathlen > 0) && (convarg[pathlen-1] != '/'))
      pathlen--;
    image->description = strdup(&convarg[pathlen]);
    ilistAppend(ilist, image);
    free(convarg);
  }
  delete curdir;

  /* save this in iv so it can be passed in call to ivwSetCacheFrom List */
  iv->imageList = (ImodImageFile *)ilist;
}

static int ivwSetCacheFromList(ImodView *iv, Ilist *ilist)
{
  ImodImageFile *image;
  int retcode = 0;
  int eret;
  int xsize, ysize, zsize, i;
  int rgbs = 0;

  if (!ilist->size)
    return -1;

  if (!iv->li->plist) {
     
    /* First get minimum x, y, z sizes of all the files */
    for (i = 0; i < ilist->size; i++) {
      image = (ImodImageFile *)ilistItem(ilist, i);
      if (!i || image->nx < xsize)
        xsize = image->nx;
      if (!i || image->ny < ysize)
        ysize = image->ny;
      if (!i || image->nz < zsize)
        zsize = image->nz;
    }     

    /* Use this to fix the load-in coordinates, then use those to set the
       lower left and upper right coords in each file */
    mrc_fix_li(iv->li, xsize, ysize, zsize);
    for (i = 0; i < ilist->size; i++) {
      image = (ImodImageFile *)ilistItem(ilist, i);
      image->llx = iv->li->xmin;
      image->lly = iv->li->ymin;
      image->llz = iv->li->zmin;
      image->urx = iv->li->xmax;
      image->ury = iv->li->ymax;
      image->urz = iv->li->zmax;
               
      /* If not an MRC file or color file, set to no flipping ever */
      if (image->file != IIFILE_MRC || 
          image->format == IIFORMAT_RGB) {
        iv->li->axis = 3;
        iv->flippable = 0;
      }

      /* Add to count if RGB or not, to see if all the same type */
      if (image->format == IIFORMAT_RGB)
        rgbs++;
    }     
  } else {

    /* For montage, see if it is rgb */
    image = (ImodImageFile *)ilistItem(ilist, 0);
    if (image->format == IIFORMAT_RGB)
      rgbs = 1;
  }

  if (rgbs) {
    if (rgbs < ilist->size) {
      imodError(NULL, "3DMOD Error: Only %d files out of %d are "
              "RGB type and all files must be.\n", rgbs, ilist->size);
      exit(-1);
    }
               
    if (!App->rgba) {
      imodError(NULL, "3DMOD Error: You must not start 3dmod with "
              "the -ci option to display RGB files.\n");
      exit(-1);
    }
        
    /* Set the flag for storing raw images with the mode, and set rgba to 
       indicate the number of bytes being stored */
    App->rgba = 3;
    iv->rawImageStore = MRC_MODE_RGB;
  }

  xsize = iv->li->xmax - iv->li->xmin + 1;
  ysize = iv->li->ymax - iv->li->ymin + 1;
  zsize = iv->li->zmax - iv->li->zmin + 1;

  if (ilist->size == 1){

    iv->hdr = iv->image = iiNew();
    if (!iv->image){
      imodError(NULL, "Not enough memory.\n"); 
      exit(-1);
    }
    memcpy(iv->image, ilist->data, sizeof(ImodImageFile));
    iiReopen(iv->image);
    iv->ct = iv->nt = 0;

  } else {
    iv->imageList = (ImodImageFile *)malloc
      (sizeof(ImodImageFile) * ilist->size);
    memcpy(iv->imageList, ilist->data,
           sizeof(ImodImageFile) * ilist->size);
    ivwSetTime(iv, 1);
    iv->hdr = iv->image = iv->imageList;
    /* mrc_init_li(iv->li, NULL);
       mrc_fix_li(iv->li, xsize, ysize, zsize); */
    iv->dim |= 8;
  }
          
  iv->xsize  = xsize;
  iv->ysize  = ysize;
  iv->zsize  = zsize;
  iv->xysize = xsize * ysize;
  
  // Set info window up now that size is known
  ImodPrefs->setInfoGeometry();

  ivwSetCacheSize(iv);
  ilistDelete(ilist);
        
  /* The first initialization of the cache has to be with unflipped 
     dimensions regardless of whether it's going to be flipped, so save
     and restore the axis flag */
  i = iv->li->axis;
  iv->li->axis = 3;
  eret = ivwInitCache(iv);
  iv->li->axis = i;

  if (eret){
    imodError(NULL, "3DMOD Fatal Error. init image cache. (%d)\n",
            eret);
    exit(-1);
  }
               
  iv->li->imin = 0;
  iv->li->imax = 255;
  /*     iv->li->slope  = 1.0f;
         iv->li->offset = 0.0f; */

  best_ivwGetValue = cache_ivwGetValue;
  ivwSetScale(iv);
  wprint("\r");

  retcode = ivwManageInitialFlips(iv);
  imod_info_input();
  return(retcode);
}

/* plugin utility functions.*/
void ivwGetImageSize(ImodView *inImodView, int *outX, int *outY, int *outZ)
{
  *outX = inImodView->xsize;
  *outY = inImodView->ysize;
  *outZ = inImodView->zsize;
}


Imod *ivwGetModel(ImodView *inImodView)
{
  if (inImodView == NULL) 
    return(NULL);
  return(inImodView->imod);
}

Iobj *ivwGetExtraObject(ImodView *inImodView)
{
  if (inImodView == NULL) 
    return(NULL);
  return(inImodView->extraObj);
}

int  ivwDraw(ImodView *inImodView, int inFlags)
{
  imodDraw(inImodView, inFlags);
  return(0);
}

int ivwRedraw(ImodView *vw)
{
  return (imod_redraw(vw));
}

void ivwGetRamp(ImodView *inImodView, int *outRampBase, int *outRampSize)
{
  *outRampBase = inImodView->rampbase;
  *outRampSize = inImodView->rampsize;
}

int  ivwGetObjectColor(ImodView *inImodView, int inObject)
{
  Iobj *obj;
  int objIndex = 0;

  /* check that inObject is within range. */
  if (inObject < 0)
    return(objIndex);
  if (inObject >= inImodView->imod->objsize)
    return(objIndex);
     
  obj = &(inImodView->imod->obj[inObject]);
     
  if (App->depth <= 8){
    obj->fgcolor = App->objbase - inObject;
  }else{
    obj->fgcolor = App->objbase + inObject;
  }
  objIndex = obj->fgcolor;
  return(objIndex);
}

/*
$Log$
Revision 4.15  2003/10/01 05:09:11  mast
Changes for recreation of plugin compilation capability

Revision 4.14  2003/09/26 00:07:36  mast
No longer ask for contiguous memory for more than 1 GB or image data

Revision 4.13  2003/09/24 17:33:31  mast
Add setting of info window geometry as soon as image size is known

Revision 4.12  2003/09/18 00:42:43  mast
Fixed an error message

Revision 4.11  2003/09/16 02:46:18  mast
Changed to return line pointers to images instead of actually flipping data
and consolidated fast pixel access routines from xyz, slicer, and tumbler.

Revision 4.10  2003/08/02 22:44:14  mast
Made it possible to kill program during flip operation

Revision 4.9  2003/06/27 19:28:04  mast
Made the extra object when initializing view, and added function to
pass the extra object.

Revision 4.8  2003/05/06 02:19:13  mast
Made ivwPointVisible use proper rounding in test

Revision 4.7  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.6  2003/03/26 01:52:39  mast
Make decision about whether to request contiguous data or not depending
on whether data are to be flipped, and catch and report errors when flipping

Revision 4.5  2003/03/13 01:19:23  mast
Make ivwGetTimeIndexLabel return empty string instead of NULL

Revision 4.4  2003/02/27 19:42:14  mast
Changes to filename and directory handling to work under windows

Revision 4.3  2003/02/22 00:00:29  mast
Open image files in binary mode

Revision 4.2  2003/02/14 01:15:44  mast
treat zmouse values more carefully, cleanup unused variables

Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.4  2003/01/29 17:54:18  mast
changed ivwGetLocation to get nearest intgere from zmouse

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/23 20:12:25  mast
initialize new ghostdist variable

Revision 1.1.2.1  2003/01/18 01:12:48  mast
convert to cpp

Revision 3.4.2.5  2003/01/14 21:52:38  mast
include new movie controller include file

Revision 3.4.2.4  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 3.4.2.3  2002/12/19 04:37:13  mast
Cleanup of unused global variables and defines

Revision 3.4.2.2  2002/12/12 01:21:53  mast
xyz no longer a member of ImodView structure

Revision 3.4.2.1  2002/12/11 00:39:45  mast
Kept it from flipping images while loading data

Revision 3.4  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.3  2002/07/20 23:28:14  mast
Store image origin in model's refImage.otrans so programs can get back
to index corrdinates of full-sized volume

Revision 3.2  2002/01/28 16:56:07  mast
Moved setting of vi->[xyz]size up before image loading so that movie
controller will have good sizes if it is opened while image is loading

Revision 3.1  2001/12/17 18:50:42  mast
Changed the way section usage in the cache is kept track of and added
logic for cache filling

*/

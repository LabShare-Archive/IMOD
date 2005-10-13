/*
 *  imodview.c -- Handle the ImodView structure.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

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
#ifdef __linux
#include <fcntl.h>
#endif
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
#include "undoredo.h"

static int ivwProcessImageList(ImodView *vi);
static int ivwManageInitialFlips(ImodView *vi);
static int ivwCheckLinePtrAllocation(ImodView *vi, int ysize);
static int ivwCheckBinning(ImodView *vi, int nx, int ny, int nz);
static void deletePlistBuf(void);

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
  vi->keepCacheFull = 1;
  vi->loadingImage = 0;
  vi->doingInitialLoad = 0;
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
  vi->selectionList = NULL;

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
  vi->multiFileZ = 0;
  vi->extraObj = imodObjectNew();
  vi->linePtrs = NULL;
  vi->linePtrMax = 0;
  vi->blankLine = NULL;
  vi->xybin = 1;
  vi->zbin = 1;
  vi->flippable = 1;
  vi->grayRGBs = 0;
  vi->undo = new UndoRedo(vi);
}

/*
 *
 *  Image data service functions.
 *
 */

/* Get the current Section, could be X or Y in future. */     
unsigned char **ivwGetCurrentSection(ImodView *vi)
{
  int cz = (int)(vi->zmouse + 0.5f);
  return(ivwGetZSection(vi, cz));
}

unsigned char **ivwGetCurrentZSection(ImodView *vi)
{
  int cz = (int)(vi->zmouse + 0.5f);
  return(ivwGetZSection(vi, cz));
}

/* Reopen an image file, setting current directory correctly before opening,
 and back afterwards */
int ivwReopen(ImodImageFile *inFile)
{
  int retval;
  if (!Imod_IFDpath.isEmpty())
    QDir::setCurrent(Imod_IFDpath);
  retval = iiReopen(inFile);
  if (!Imod_IFDpath.isEmpty())
    QDir::setCurrent(Imod_cwdpath);
  return retval;
}

/* Get a section at potentially different time from current time */
unsigned char **ivwGetZSectionTime(ImodView *vi, int section, int time)
{
  int oldTime;
  unsigned char **imageData;

  if (!vi) return NULL;
  if (!vi->nt) return(ivwGetZSection(vi, section));
  if (time < 1) return(NULL);
  /* DNM: make test > instead of >= */
  if (time > vi->nt) return(NULL);

  ivwGetTime(vi, &oldTime);
  if (time == oldTime) return(ivwGetZSection(vi, section));

  vi->ct = time;
  vi->hdr = vi->image = &vi->imageList[time-1];
  ivwReopen(vi->image);
  imageData = ivwGetZSection(vi, section);
  iiClose(vi->image);
  vi->ct = oldTime;
  vi->hdr = vi->image = &vi->imageList[oldTime-1];
  return(imageData);
}

/* DNM 12/12/01: make this a function, do it only if not raw images
   This is what scales data into restricted range with 8-bit colormap */
void ivwScaleDepth8(ImodView *vi, ivwSlice *tempSlicePtr)
{
  int rbase = vi->rampbase;
  float scale = vi->rampsize/256.0f;
  int pix, i;
  int mi = tempSlicePtr->sec->xsize * tempSlicePtr->sec->ysize;
  unsigned char *id = tempSlicePtr->sec->data.b;
  if (App->depth == 8 && !vi->rawImageStore){
    for (i = 0; i < mi; i++){
      pix   = (int)(id[i] * scale + rbase);
      id[i] = (unsigned char)pix;
    }
  }
}

/* Returns pointer to raw image data for given z section. */
/* DNM 12/13/01: rewrote to use a use count for priority, instead of 
   rearranging the cache array every time */
/* DNM 9/15/03: rewrote to return line pointers */
unsigned char **ivwGetZSection(ImodView *vi, int section)
{
  ivwSlice *tempSlicePtr = NULL;
  int sl, slmin, minused, pixSize, slice;
  int cz = section;

  if (section < 0 || section >= vi->zsize) 
    return(NULL);
  if (!vi->fp || vi->fakeImage || vi->loadingImage)
    return(NULL);
  if (!vi->fullCacheFlipped && ivwPlistBlank(vi, section)) 
    return(NULL);

  /* Plain, uncached data: make line pointers if not flipped */
  if (!vi->vmSize) {
    if (vi->li->axis == 3) {
      return (ivwMakeLinePointers(vi, vi->idata[section], vi->xsize,
                                  vi->ysize, vi->rawImageStore));
    }
    else {
      /* If flipped, check the pointer allocation, get the */
      if (ivwCheckLinePtrAllocation(vi, vi->ysize))
        return(NULL);
      pixSize = ivwGetPixelBytes(vi->rawImageStore);
      for (sl = 0; sl < vi->ysize; sl++)
        vi->linePtrs[sl] = vi->idata[sl] + vi->xsize * pixSize * section;
      return (vi->linePtrs);
    }
  }

  /* Cached data with a full cache upon flipping - make line pointers */
  if (vi->vmSize && vi->fullCacheFlipped) {
    if (ivwCheckLinePtrAllocation(vi, vi->ysize))
      return(NULL);
    pixSize = ivwGetPixelBytes(vi->rawImageStore);
    for (sl = 0; sl < vi->ysize; sl++) {
      slice = vi->cacheIndex[sl*vi->vmTdim + vi->ct - vi->vmTbase];
      if (slice < 0)
        vi->linePtrs[sl] = vi->blankLine;
      else
        vi->linePtrs[sl] = vi->vmCache[slice].sec->data.b + 
          vi->xsize * pixSize * section;
    }
    return (vi->linePtrs);
  }

  /* Cached data otherwise */
  sl = vi->cacheIndex[section * vi->vmTdim + vi->ct - vi->vmTbase];
  /* imodPrintStderr("sect %d slice %d\n", section, sl);*/
  if (sl >= 0)
    tempSlicePtr = &(vi->vmCache[sl]);
  else {

    /* Didn't find slice in cache, need to load it in. */

    /* DNM 12/12/01: add call to cache filler */
    if (icfGetAutofill())
      return(ivwMakeLinePointers(vi, icfDoAutofill(vi, cz), vi->xsize,
                                 vi->ysize, vi->rawImageStore));

    /* Find oldest slice to replace */
    minused = vi->vmCount + 1;
    for (sl = 0; sl < vi->vmSize; sl++)
      if (vi->vmCache[sl].used < minused) {
        minused = vi->vmCache[sl].used;
        slmin = sl;
      }

    sl = slmin;
    tempSlicePtr = &(vi->vmCache[sl]);
    if (tempSlicePtr->cz >= 0 && tempSlicePtr->ct >= vi->vmTbase)
      vi->cacheIndex[tempSlicePtr->cz * vi->vmTdim + tempSlicePtr->ct - 
                     vi->vmTbase] = -1;
          
    /* Load in image */
    ivwReadZ(vi, tempSlicePtr->sec->data.b, section);

    ivwScaleDepth8(vi, tempSlicePtr);

    tempSlicePtr->cz = section;
    tempSlicePtr->ct = vi->ct;
    vi->cacheIndex[section * vi->vmTdim + vi->ct - vi->vmTbase] = sl;
  }

  /* Adjust use count, assign to slice */
  vi->vmCount++;
  tempSlicePtr->used = vi->vmCount;

  return (ivwMakeLinePointers(vi, tempSlicePtr->sec->data.b, 
                              tempSlicePtr->sec->xsize,
                              tempSlicePtr->sec->ysize, vi->rawImageStore));
}

static unsigned char *plistBuf = 0;
static int plistBufSize=0;

int ivwPlistBlank(ImodView *vi, int cz)
{
  int i, mi = vi->li->plist;
  if (!mi) return(mi);
  cz += vi->li->zmin;
  for (i = 0; i < mi; i++)
    if (vi->li->pcoords[(i*3)+2] == cz) {
      return(0);
    }
  return(1);
}

static void deletePlistBuf(void)
{
  if (plistBuf) free(plistBuf);
  plistBuf = NULL;
  plistBufSize = 0;
}

/* Read a section of data into the cache */
void ivwReadZ(ImodView *vi, unsigned char *buf, int cz)
{
  int zread;

  /* Image in not a stack but loaded into pieces. */
  if (vi->li->plist){
  
    int mx, my; /* the size of the section buffer */
    int ox, oy; /* data offset/origin  */
    unsigned int i, mxy, bxy;
    int nxbin, nybin; 

    /* DNM 1/3/04: use function instead of explicit tests */
    int pixSize = ivwGetPixelBytes(vi->rawImageStore);

    mx = vi->xsize;    my = vi->ysize;
    ox = vi->li->xmin; oy = vi->li->ymin;
    mxy = mx * my;

    nxbin = vi->hdr->nx / vi->xybin;
    nybin = vi->hdr->ny / vi->xybin;

    /* DNM: make the buffer the size of input pieces */
    bxy = nxbin * nybin;

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
    cz += vi->li->zmin;

    /* Check each piece and copy its parts into the section. */
    for (i = 0; i < vi->li->plist; i++){
      int iox, ioy, fox, foy;
      int xsize, ysize;
      int fskip, iskip;
      int llx, lly, urx, ury;

      if ( vi->li->pcoords[(i*3)+2] == cz){
        iox = vi->li->pcoords[(i*3)];
        ioy = vi->li->pcoords[(i*3)+1];

        /* DNM: compute the bounding coordinates to read in, and
           skip if there is nothing that overlaps the image */
        llx = ox - iox;
        if (llx < 0)
          llx = 0;
        urx = vi->li->xmax - iox;
        if (urx >= nxbin)
          urx = nxbin - 1;

        lly = oy - ioy;
        if (lly < 0)
          lly = 0;
        ury = vi->li->ymax - ioy;
        if (ury >= nybin)
          ury = nybin - 1;

        if (llx > urx || lly > ury)
          continue;
                    
        vi->image->llx = llx;
        vi->image->urx = urx;
        vi->image->lly = lly;
        vi->image->ury = ury;

        ivwReadBinnedSection(vi, (char *)plistBuf, i);

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

  /* normal data - set up z to read based on axis, flipped or not */
  zread = cz + vi->li->zmin;
  if (vi->li->axis == 2)
    zread = cz + vi->li->ymin;

  /* For multi-file Z, close current file, open proper one,  read z = 0 */
  if (vi->multiFileZ > 0) {
    iiClose(vi->image);
    vi->hdr = vi->image = &vi->imageList[zread];
    ivwReopen(vi->image);
    zread = 0;
  }

  /* DNM 1/2/04: simplify to call one place for raw, regular, or binned read */
  ivwReadBinnedSection(vi, (char *)buf, zread);

  return;
}

/* Determine size of data unit */
int ivwGetPixelBytes(int mode)
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
static int ivwCheckLinePtrAllocation(ImodView *vi, int ysize)
{
  if (ysize > vi->linePtrMax) {
    if (vi->linePtrMax)
      free(vi->linePtrs);
    vi->linePtrs = (unsigned char **)malloc (ysize * sizeof(unsigned char *));
    if (!vi->linePtrs) {
      vi->linePtrMax = 0;
      return 1;
    }
    vi->linePtrMax = ysize;
  }
  return 0;
}

/* Make a list of line pointers from a contiguous data slice that is xsize by
   ysize, of the given mode */
unsigned char **ivwMakeLinePointers(ImodView *vi, unsigned char *data,
                                    int xsize, int ysize, int mode)
{
  int i;
  int pixSize = 1;

  if (!data)
    return (NULL);

  if (ivwCheckLinePtrAllocation(vi, ysize))
    return NULL;

  pixSize = ivwGetPixelBytes(mode);

  /* Make up the pointers */
  for (i = 0; i < ysize; i++)
    vi->linePtrs[i] = data + pixSize * xsize * i;
  return (vi->linePtrs);
}

/* Read a section, potentially with binning */
int ivwReadBinnedSection(ImodView *vi, char *buf, int section)
{
  int xsize, ysize, xbinned, ybinned, i, iz;
  unsigned char *unbinbuf = NULL;
  unsigned char *usbuf = (unsigned char *)buf;
  b3dInt16 *binbuf = NULL;
  ImodImageFile im;

  ivwGetFileStartPos(vi->image);

  // If there is no binning, just call the raw or byte routines
  if (vi->xybin * vi->zbin == 1) {
    if (vi->rawImageStore) 
      iiReadSection(vi->image, buf, section);
    else
      iiReadSectionByte(vi->image, buf, section);
    ivwDumpFileSysCache(vi->image);
    return 0;
  }

  // Copy image structure and adjust load-in coordinates
  im = *(vi->image);
  xbinned = im.urx + 1 - im.llx;
  ybinned = im.axis == 3 ? im.ury + 1 - im.lly : im.urz + 1 - im.llz;
  im.llx = vi->xybin * im.llx;
  im.urx = vi->xybin * im.urx + vi->xybin - 1;
  im.lly = vi->xybin * im.lly;
  im.ury = vi->xybin * im.ury + vi->xybin - 1;
  im.llz = vi->xybin * im.llz;
  im.urz = vi->xybin * im.urz + vi->xybin - 1;

  // Get unbinned size, and get buffers for unbinned data and for adding
  // up binned data if there is Z binning
  xsize = im.urx + 1 - im.llx;
  ysize = im.axis == 3 ? im.ury + 1 - im.lly : im.urz + 1 - im.llz;
  unbinbuf = (unsigned char *)malloc(xsize * ysize);
  if (!unbinbuf)
    return 1;
  if (vi->zbin > 1) {
    binbuf = (b3dInt16 *)malloc(xbinned * ybinned * sizeof(b3dInt16));
    if (!binbuf) {
      free (unbinbuf);
      return 1;
    }
  }

  // Loop through the unbinned sections to read and bin them into buf
  for (iz = 0; iz < vi->zbin; iz++) {
    iiReadSectionByte(&im, (char *)unbinbuf, vi->zbin * section + iz);
    ivwBinByN(unbinbuf, xsize, ysize, vi->xybin, (unsigned char *)buf);

    // For multiple sections, move or add to the binned buffer
    if (vi->zbin > 1) {
      if (!iz)
        for (i = 0; i < xbinned * ybinned; i++)
          binbuf[i] = usbuf[i];
      else
        for (i = 0; i < xbinned * ybinned; i++)
          binbuf[i] += usbuf[i];
    }
  }

  // And divide binned value into final buffer
  if (vi->zbin > 1)
    for (i = 0; i < xbinned * ybinned; i++)
      usbuf[i] = binbuf[i] / vi->zbin;

  free(unbinbuf);
  if (binbuf)
    free(binbuf);
  ivwDumpFileSysCache(vi->image);
  return 0;
}

#ifdef __linux
static fpos_t startPos;
#endif

void ivwGetFileStartPos(ImodImageFile *image)
{
#ifdef __linux
  if (!image->fp || (image->file != IIFILE_MRC && image->file != IIFILE_RAW))
    return;
  fgetpos(image->fp, &startPos);
#endif
}

void ivwDumpFileSysCache(ImodImageFile *image)
{
#ifdef __linux
  fpos_t endPos;
  int filedes;
  long long start, end;
  off_t diff;
  if (!image->fp || (image->file != IIFILE_MRC && image->file != IIFILE_RAW))
    return;
  filedes = fileno(image->fp);
  fgetpos(image->fp, &endPos);
  start = startPos.__pos;
  end = endPos.__pos;
  if (end <= start)
    return;
  diff = (off_t)(end - start);
  posix_fadvise(filedes, startPos.__pos, diff, POSIX_FADV_DONTNEED);
  //imodPrintStderr("start %llu end %llu len %llu\n",  start, end, diff);
#endif
}

/*
 * Routines for getting a value based on type of data 
 */

int (*best_ivwGetValue)(ImodView *vi, int x, int y, int z);

int ivwGetValue(ImodView *vi, int x, int y, int z)
{
  return((*best_ivwGetValue)(vi, x, y, z));
}

int idata_ivwGetValue(ImodView *vi, int x, int y, int z)
{
  /* DNM: calling routine is responsible for limit checks */
  if (vi->li->axis == 3)
    return(vi->idata[z][x + (y * vi->xsize)]);
  else
    return(vi->idata[y][x + (z * vi->xsize)]);
}

/* 1/3/04: eliminated fileScale_ivwGetValue which was unussed, incorrect,
   and the only user of li->slope and offset */

int cache_ivwGetValue(ImodView *vi, int x, int y, int z)
{
  ivwSlice *tempSlicePtr = 0;
  unsigned char *image;
  int sl;

  /* find pixel in cache */
  /* If full cache and flipped, swap y and z */
  if (vi->fullCacheFlipped) {
    sl = z;
    z = y;
    y = sl;
  }

  /* get slice if it is loaded */
  sl = vi->cacheIndex[z * vi->vmTdim + vi->ct - vi->vmTbase];
  if (sl < 0)
    return(0);

  tempSlicePtr = &vi->vmCache[sl];

  image = tempSlicePtr->sec->data.b;
  if (!image) return(0);
     
  /* DNM: calling routine is responsible for limit checks */

  return(image[(y * tempSlicePtr->sec->xsize) + x]);
}

int fake_ivwGetValue(ImodView *vi, int x, int y, int z)
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

static int cache_GetFlipped(int x, int y, int z)
{
  if (!imdata[y])
    return(vmnullvalue);
  return(imdata[y][x + (z * vmdataxsize[y])]);
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

  if ((!vi->vmSize || vi->fullCacheFlipped) && vi->li->axis == 2)
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
    for (iz = 0; iz < size; iz++) {
      i = vi->cacheIndex[iz * vi->vmTdim + vi->ct - vi->vmTbase];
      if (i < 0) {
        imdata[iz] = NULL;
      } else {
        imdata[iz] = vi->vmCache[i].sec->data.b;
        vmdataxsize[iz] = vi->vmCache[i].sec->xsize;
	*cacheSum += iz;
      }
    }
    
    vmnullvalue = inNullvalue;
    if (vi->fullCacheFlipped)
      ivwFastGetValue = cache_GetFlipped;
    else
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


/* DNM 1/19/03: eliminated ivwShowstatus in favor of imod_imgcnt */

/****************************************************************************/
/* CACHE INITIALIZATION ROUTINES */

/* After we are all done with the Cache free it.
 */
void ivwFreeCache(ImodView *vi)
{
  int i;
  for (i = 0; i < vi->vmSize; i++)
    if (vi->vmCache[i].sec)
      sliceFree(vi->vmCache[i].sec);
  free(vi->vmCache);
  free(vi->cacheIndex);
  free(vi->blankLine);
  return;
}

/*
 *  Sets the image Cache so that it contains no data. 
 */
void ivwFlushCache(ImodView *vi)
{
  int i;
  int zsize = vi->li->zmax - vi->li->zmin + 1;
  if (vi->li->axis == 2) 
    zsize = vi->li->ymax - vi->li->ymin + 1;

  for (i = 0; i < vi->vmSize; i++){
    vi->vmCache[i].cz = -1;
    vi->vmCache[i].ct = 0;
    vi->vmCache[i].used = -1;
  }
  vi->vmCount = 0;

  for (i = 0; i < vi->vmTdim * zsize; i++)
    vi->cacheIndex[i] = -1;
}

/* Initialize the cache to number of slices in vmSize */
int ivwInitCache(ImodView *vi)
{
  int i;
  int xsize = vi->li->xmax - vi->li->xmin + 1;
  int ysize = vi->li->ymax - vi->li->ymin + 1;
  int zsize = vi->li->zmax - vi->li->zmin + 1;

  vi->vmTdim = vi->nt ? vi->nt : 1;
  vi->vmTbase = vi->nt ? 1 : 0;
  vi->fullCacheFlipped = 0;

  if (vi->li->axis == 2) {
    ysize = vi->li->zmax - vi->li->zmin + 1;
    zsize = vi->li->ymax - vi->li->ymin + 1;
  }


  /* imodPrintStderr("xsize %d  ysize %d  vmsize %d\n", xsize, ysize, 
     vi->vmSize); */
  /* get array of slice structures */
  vi->vmCache = (ivwSlice *)malloc(sizeof(ivwSlice) * vi->vmSize);
  if (!vi->vmCache)
    return(9);

  i = xsize * ivwGetPixelBytes(vi->rawImageStore);
  vi->cacheIndex = (int *)malloc(vi->vmTdim * zsize * sizeof(int));
  vi->blankLine = (unsigned char *)malloc(i);
  if (!vi->cacheIndex || !vi->blankLine) {
    free(vi->vmCache);
    return(9);
  }

  memset(vi->blankLine, 0, i);

  for (i = 0; i < vi->vmSize; i++)
    vi->vmCache[i].sec = NULL;
     
  /* get a slice array for each slice, mark each slice as empty */
  for (i = 0; i < vi->vmSize; i++){
    if (vi->rawImageStore)
      vi->vmCache[i].sec = sliceCreate
        (xsize, ysize, vi->hdr->mode);
    else
      vi->vmCache[i].sec = sliceCreate
        (xsize, ysize, MRC_MODE_BYTE);
 
    /*  imodPrintStderr("cache %d : %d x %d\n", i, xsize, ysize); */
 
    if (!vi->vmCache[i].sec){
      ivwFreeCache(vi);
      return(10);
    }
  }
  ivwFlushCache(vi);
  return(0);
}

/* DNM 2/11/01: Once image sizes are set up, this routine interprets an entered
   cache size in megabytes, determines actual size needed for a montage and
   size needed for multiple files, and then sets the cache size from the
   entered value or the needed size, as appropriate */
static int ivwSetCacheSize(ImodView *vi)
{
  int xsize = vi->li->xmax - vi->li->xmin + 1;
  int ysize = vi->li->ymax - vi->li->ymin + 1;
  int zsize = vi->li->zmax - vi->li->zmin + 1;
  int dzsize = zsize;
  int pixSize = ivwGetPixelBytes(vi->hdr->mode);
  int i;

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
    for (i = 0; i < vi->li->plist; i++)
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

/* DNM 1/3/04; eliminated ivwSetScale as unneeded and confusing */

/* 
 * FLIP A TOMOGRAM 
 */
/* DNM 9/15/03: removed actual flipping code now that line pointers are
   used to access images */
int ivwFlip(ImodView *vi)
{
  int nx, ny, nz;
  int i, t, cacheFull;
  unsigned int nyz;
  int oymouse, ozmouse;

  /* DNM 12/10/02: if loading image, flip the axis but defer until done */
  if (vi->doingInitialLoad) {
    vi->li->axis =  (vi->li->axis == 2) ? 3 : 2;
    return (1);
  }

  /* but if it is not the inital load, it is just to be ignored */
  if (vi->loadingImage)
    return (2);

  /* find out if cache is full */
  cacheFull = 1;
  if (vi->vmSize && !vi->fullCacheFlipped) {
    for (i = 0; i < vi->vmTdim * vi->zsize; i++)
      if (vi->cacheIndex[i] < 0 && !ivwPlistBlank(vi, i)) {
        cacheFull = 0;
        break;
      }
  }

  /* Flipping is always allowed unless the cache is not full */
  if ((!vi->flippable || vi->li->plist) && !cacheFull){
    wprint("\aSorry, these image data can't be flipped unless they "
           "are completely loaded into memory.\n");
    return(-1);
  }

  oymouse = (int)(vi->ymouse + 0.5f);
  ozmouse = (int)(vi->zmouse + 0.5f);

  wprint("Flipping image data.\n");

  /* DNM: restore data before flipping, as well as resetting when done */
  iprocRethink(vi);
  nx = vi->xsize;
  ny = vi->zsize;
  nz = vi->ysize;
  nyz = ny * nz;

  vi->li->axis =  (vi->li->axis == 2) ? 3 : 2;

  if (vi->vmSize && cacheFull) {
    vi->fullCacheFlipped = 1 - vi->fullCacheFlipped;

  } else if (vi->vmSize){

    /* If Image data is cached from disk */
    /* tell images to flipaxis */
          
    if ((vi->nt) && (vi->imageList)){
      for (t = 0; t < vi->nt; t++){
        vi->imageList[t].axis = vi->li->axis;
      }
    }
    if (vi->image){
      vi->image->axis = vi->li->axis;
               
    }

    ivwFreeCache(vi);
    /* DNM: if the cache size equalled old # of Z planes, set it to new
       number of planes, including ones for each file
       Otherwise, set it to occupy same amount of memory, rounding up
       to avoid erosion on repeated flips */
    t = vi->nt > 0 ? vi->nt : 1;
    if (vi->vmSize == t * vi->zsize)
      vi->vmSize = t * nz;
    else {
      vi->vmSize = (vi->vmSize * vi->ysize + ny / 2) / ny;
      if (!vi->vmSize)
        vi->vmSize = 1;
    }
    ivwInitCache(vi);
  }

  //vi->xsize = nx;
  vi->ysize = ny;
  vi->zsize = nz;
  vi->xysize = vi->xsize * vi->ysize;
  //vi->xmouse = 0;
  vi->ymouse = 0;
  vi->zmouse = 0;

  nx = vi->yUnbinSize;
  vi->yUnbinSize = vi->zUnbinSize;
  vi->zUnbinSize = nx;

  ivwFlipModel(vi);
  iprocRethink(vi);
  autox_newsize(vi);
  imod_info_float_clear(-1, -1);

  vi->ymouse = ozmouse;
  vi->zmouse = oymouse;

  /* Keep it in bounds */
  if (vi->zmouse > nz - 1)
    vi->zmouse = nz - 1;

  /* DNM: need to reset the movie controller because ny and nz changed */
  imcResetAll(vi);

  return(0);
}

/* Scale image data to fit in 8-bit colorramp. */
int ivwScale(ImodView *vi)
{
  int pix;
  float scale = 1.0;
  int rbase = 0;
  int ysize = vi->ysize;
  int xsize = vi->xsize;
  int i,j,k;

  if (vi->vmSize)
    return -1;
     
  rbase = vi->rampbase;
  scale = vi->rampsize/256.0f;

  for (k = 0; k < vi->zsize; k++)
    for (j = 0; j < ysize; j++)
      for (i = 0; i < xsize; i++){
        pix = (int)(vi->idata[k][i + (j * vi->xsize)] * scale);
        pix += rbase;
        vi->idata[k][i + (j * vi->xsize)] = pix;
      }

  return(0);
}

void ivwBindMouse(ImodView *vi)
{
  if (vi->xmouse < 0)
    vi->xmouse = 0;
  if (vi->ymouse < 0)
    vi->ymouse = 0;
  if (vi->zmouse < 0)
    vi->zmouse = 0;
  if (vi->xmouse >= vi->xsize)
    vi->xmouse = vi->xsize - 1;
  if (vi->ymouse >= vi->ysize)
    vi->ymouse = vi->ysize - 1;
  if (vi->zmouse > vi->zsize - 1)
    vi->zmouse = vi->zsize - 1;
  return;
}

void ivwGetLocation(ImodView *vi, int *x, int *y, int *z)
{
  *x = (int)(vi->xmouse);
  *y = (int)(vi->ymouse);
  *z = (int)floor(vi->zmouse + 0.5);
  return;
}

void ivwGetLocationPoint(ImodView *inImodView, Ipoint *outPoint)
{
  outPoint->x = inImodView->xmouse;
  outPoint->y = inImodView->ymouse;
  outPoint->z = inImodView->zmouse;
}

/* By using IMOD IFD files one can have a separate image for
 * several different time points.
 * returns number of time elements available or 0 if no time.
 * time is the current time index.
 */
int ivwGetTime(ImodView *vi, int *time)
{
  if (time){
    *time = vi->ct;
  }
  return(vi->nt);
}

/* Set the current time index.  Time index ranges from 1 to maxtime 
 * 0 is reserved for no time - meaning only one image stack is loaded. 
 */
void ivwSetTime(ImodView *vi, int time)
{
    
  if (!vi->nt){
    vi->ct = vi->imod->ctime = 0;
    return;
  }
     
  /* DNM 6/17/01: Don't do this */
  /* inputSetModelTime(vi, time); */  /* set model point to a good value. */

  if (vi->ct > 0 && !vi->fakeImage)
    iiClose(&vi->imageList[vi->ct-1]);

  vi->ct = time;
  if (vi->ct > vi->nt)
    vi->ct = vi->nt;
  if (vi->ct <= 0)
    vi->ct = 1;

  if (!vi->fakeImage){
    vi->hdr = vi->image = &vi->imageList[vi->ct-1];
    // ivwSetScale(vi);

    ivwReopen(vi->image);
   }
  /* DNM: update scale window */
  imodImageScaleUpdate(vi);
  vi->imod->ctime = vi->ct;
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

// Set the time of a new contour - only if there are multiple times and the
// object flags indicate time is to be stored
void ivwSetNewContourTime(ImodView *vw, Iobj *obj, Icont *cont)
{
  if (vw->nt && obj && cont && iobjTime(obj->flags)) {
    cont->type = vw->ct;
    cont->flags |= ICONT_TYPEISTIME;
  }
}

/* Set the global location in 3D space for all windows.
 */
void ivwSetLocation(ImodView *vi, int x, int y, int z)
{
  vi->xmouse = x;
  vi->ymouse = y;
  vi->zmouse = z;
  ivwBindMouse(vi);
  imodDraw(vi, IMOD_DRAW_ALL);
  return;
}

void ivwSetLocationPoint(ImodView *vi, Ipoint *pnt)
{
  int x,y,z;
  double fc = 0.5;

  x = (int)floor(pnt->x + fc);
  y = (int)floor(pnt->y + fc);
  z = (int)floor(pnt->z + fc);

  ivwSetLocation(vi, x, y, z);
  return;
}


/* Test whether point is on current section - need to use floor for - values */
int ivwPointVisible(ImodView *vi, Ipoint *pnt)
{
  if ((int)floor(vi->zmouse + 0.5) == (int)floor(pnt->z + 0.5))
    return(1);
  else
    return(0);
}

/* Read a point from an MRC file, binning if necessary */
static float ivwReadBinnedPoint(ImodView *vi, FILE *fp, 
                                struct MRCheader *mrcheader, 
                                int cx, int cy, int cz)
{
  double sum = 0.;
  int nsum = 0;
  int ix, iy, iz, ubx, uby, ubz, mirx, miry;

  if (vi->xybin * vi->zbin == 1 && !vi->image->mirrorFFT)
    return (mrc_read_point(fp, mrcheader, cx, cy, cz));
  for (iz = 0; iz < vi->zbin; iz++) {
    ubz = cz * vi->zbin + iz;
    if (ubz < mrcheader->nz) {
      for (iy = 0; iy < vi->xybin; iy++) {
        uby = cy * vi->xybin + iy;
        if (uby < mrcheader->ny) {
          for (ix = 0; ix < vi->xybin; ix++) {
            ubx = cx * vi->xybin + ix;
            if (vi->image->mirrorFFT && ubx < vi->image->nx) {
                mrcMirrorSource(vi->image->nx, vi->image->ny, ubx, uby, &mirx,
                                &miry);
                sum += mrc_read_point(fp, mrcheader, mirx, miry, ubz);
                nsum++;
            } else if (ubx < mrcheader->nx) {
              sum += mrc_read_point(fp, mrcheader, ubx, uby, ubz);
              nsum++;
            }
          }
        }
      }
    }
  }
  if (!nsum)
    return 0;
  return ((float)(sum / nsum));
}

float ivwGetFileValue(ImodView *vi, int cx, int cy, int cz)
{
  /* cx, cy, cz are in model file coords. */
  /* fx, fy, fz are in image file coords. */
  /* px, py, pz are in piece list coords. */
  int fx, fy, fz;
  FILE *fp = NULL;
  struct MRCheader *mrcheader = NULL;

  if (!vi->image)
    return 0.0f;
  if (vi->image->file != IIFILE_MRC && vi->image->file != IIFILE_RAW)
    return 0.0f;
  fp = vi->image->fp;
  mrcheader = (struct MRCheader *)vi->image->header;

  if (vi->li){

    /* get to index values in file from screen index values */
    /* DNM 7/13/04: changed to apply ymin, zmin after switching y and z */
    fx = cx + vi->li->xmin;
    if (vi->li->axis == 3) {
      fy = cy + vi->li->ymin;
      fz = cz + vi->li->zmin;
    } else {
      fy = cz + vi->li->ymin;
      fz = cy + vi->li->zmin;
    }

    /* For multi-file sections in Z, make sure z is legal, reopen the right
       section if necessary, and set z to 0 */
    if (vi->multiFileZ > 0) {
      if (fz >= 0 && fz < vi->multiFileZ && vi->image != &vi->imageList[fz]) {
        /* Don't mess with image files while loading is going on */
        if (vi->loadingImage)
          return 0.;
        iiClose(vi->image);
        vi->hdr = vi->image = &vi->imageList[fz];
        ivwReopen(vi->image);
        fp = vi->image->fp;
        mrcheader = (struct MRCheader *)vi->image->header;
        if (vi->image->file != IIFILE_MRC && vi->image->file != IIFILE_RAW)
          return 0.0f;
      }
      fz = 0;
    }

    if (vi->li->plist){

      /* montaged: find piece with coordinates in it and get data 
         there */
      int px, py, pz;
      int i, mi = vi->li->plist;
      px = fx; py = fy; pz = fz;
      for (i = 0; i < mi; i++){
        if (pz == vi->li->pcoords[(i*3)+2]){
                        
          if ((px >= vi->li->pcoords[(i*3)]) &&
              (px < (vi->li->pcoords[(i*3)]+vi->hdr->nx/vi->xybin)) &&
              (py >= vi->li->pcoords[(i*3)+1]) &&
              (py < (vi->li->pcoords[(i*3)+1]+vi->hdr->ny/vi->xybin)
               )){
            fz = i;
            fx = px - vi->li->pcoords[(i*3)];
            fy = py - vi->li->pcoords[(i*3)+1];
            return(ivwReadBinnedPoint(vi, fp, mrcheader, fx, fy, fz));
          }
        }
      }
      return(vi->hdr->amean);
    }
    return(ivwReadBinnedPoint(vi, fp, mrcheader, fx, fy, fz));
  }
  return(ivwReadBinnedPoint(vi, fp, mrcheader, cx, cy, cz));
}


/* Routine to copy a portion of one buffer into another */
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

/* DNM: scan through all contours and set wild flag if Z is not the 
   same throughout */

void ivwCheckWildFlag(Imod *imod)
{
  int ob, co, pt;
  Iobj *obj;
  Icont *cont;
     
  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for (co = 0; co < obj->contsize; co++){
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
 * Return the current image coordinate system into members of an IrefImage
 * This will take into account the transformations
 * in the MRC header along with the command line
 * sub area options.
 *
 * Fixed 3-17-96 Transform for the -Y command line option.
 * 8/14/05: changed to fill the c... members instead of the o... members
 *
 */
IrefImage *ivwGetImageRef(ImodView *vi)
{
  IrefImage *ref = (IrefImage *) malloc (sizeof(IrefImage));
  float xscale, yscale, zscale;
  float xtrans, ytrans, ztrans;
  float xrot, yrot, zrot;
     
  if (!ref) 
    return(NULL);

  xscale = vi->image->xscale;
  yscale = vi->image->yscale;
  zscale = vi->image->zscale;
  xtrans = vi->image->xtrans;
  ytrans = vi->image->ytrans;
  ztrans = vi->image->ztrans;
  xrot = vi->image->xrot;
  yrot = vi->image->yrot;
  zrot = vi->image->zrot;

  ref->cscale.x = xscale;
  ref->cscale.y = yscale;
  ref->cscale.z = zscale;

  /* DNM 11/5/98: need to scale the load-in offsets before adding them */
  ref->ctrans.x = xtrans - xscale * vi->li->xmin * vi->xybin;
  ref->ctrans.y = ytrans - yscale * vi->li->ymin * vi->xybin;
  ref->ctrans.z = ztrans - zscale * vi->li->zmin * vi->zbin;

  /* DNM 12/19/98: if using piece lists, need to subtract the minimum
     values as well */
  if(vi->li->plist) {
    ref->ctrans.x -= xscale * vi->li->opx * vi->xybin;
    ref->ctrans.y -= yscale * vi->li->opy * vi->xybin;
    ref->ctrans.z -= zscale * vi->li->opz;
  }

  /* DNM 11/5/98: tilt angles were not being passed back.  Start passing
     them through so that they will start being saved in model IrefImage */
  ref->crot.x = xrot;
  ref->crot.y = yrot;
  ref->crot.z = zrot;
  return(ref);
}

/*
 * Set up the IrefImage structure specifying the model transformation
 * ctrans, cscale, crot are set to values based upon the current image load.
 * otrans is set with the image header origin values.
 * orot and oscale are irrelevant and not maintained
 */
void ivwSetModelTrans(ImodView *vi)
{
  Imod *imod = vi->imod;
  IrefImage *ref, *iref;

  // If there is not an existing refImage, get a new one
  if (!imod->refImage){
    imod->refImage = (IrefImage *) malloc (sizeof(IrefImage));
    if (!imod->refImage) 
      return;
  }

  // Get the current image transformation data and copy to model structure
  ref  = imod->refImage;
  iref = ivwGetImageRef(vi);
  if (!iref)
    return;

  ref->cscale = iref->cscale;
  ref->ctrans = iref->ctrans;
  ref->crot   = iref->crot;

  if (vi->li->axis == 2)
    imod->flags |= IMODF_FLIPYZ;
  else
    imod->flags &= ~IMODF_FLIPYZ;

  /* DNM 11/5/98: set this flag that tilt angles were properly saved */
  imod->flags |= IMODF_TILTOK;

  /* DNM 7/20/02: the old values in the model seem never to be used, so
     use otrans to store image origin information so programs can get
     back to full volume index coordinates from info in model header.
     Also set a new flag to indicate this info exists */
  ref->otrans.x = vi->image->xtrans;
  ref->otrans.y = vi->image->ytrans;
  ref->otrans.z = vi->image->ztrans;
  imod->flags |= IMODF_OTRANS_ORIGIN;

  free(iref);
}

/* Flips model IF it does not match current flip state of image */

void ivwFlipModel(ImodView *vi)
{
  /* flip model y and z */
  Imod  *imod = vi->imod;

  if (vi->li->axis == 2)
    if ((imod->flags & IMODF_FLIPYZ))
      return;

  if ((vi->li->axis == 3) || (vi->li->axis == 0))
    if (!(imod->flags & IMODF_FLIPYZ))
      return;

  if (imod->flags & IMODF_FLIPYZ)
    imod->flags &= ~IMODF_FLIPYZ;
  else
    imod->flags |= IMODF_FLIPYZ;

  imodFlipYZ(imod);
  return;
}


/* Transforms model so that it matches image coordinate system.
 */
void ivwTransModel(ImodView *vi)
{
  IrefImage *iref;
  Imod  *imod = vi->imod;
  Ipoint binScale;

  /* If model doesn't have a reference coordinate system
   * from an image, then use this image's coordinate
   * system and return, unless there is binning;
   */
  if ((!ImodTrans) || (!vi->imod->refImage)) {
    ivwSetModelTrans(vi);
    if ((!ImodTrans) || (!vi->imod->refImage) || (vi->xybin * vi->zbin == 1))
      return;
  }

  /* Try and get the coordinate system that we will
   * transform the model to match.
   * Set the old members if iref to the model's current members
   */
  iref = ivwGetImageRef(vi);
  if (!iref) 
    return;

  iref->orot   = vi->imod->refImage->crot;
  iref->otrans = vi->imod->refImage->ctrans;
  iref->oscale = vi->imod->refImage->cscale;
  binScale.x = binScale.y = vi->xybin;
  binScale.z = vi->zbin;

  /* transform model to new coords (it will be unflipped if necessary) */
  imodTransModel(imod, iref, binScale);

  ivwFlipModel(vi);
  ivwSetModelTrans(vi);
  free(iref);
  return;
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
int ivwLoadIMODifd(ImodView *vi)
{
  Ilist *ilist = ilistNew(sizeof(ImodImageFile), 32);
  ImodImageFile *image;
  char line[IFDLINE_SIZE + 1];
  int i;
  struct LoadInfo *li;
  int xsize = 0, ysize = 0, zsize;
  int version = 0;

  char *imgdir = NULL;
  QDir *curdir = new QDir();

  rewind(vi->fp);
  imodFgetline(vi->fp, line, IFDLINE_SIZE);

  while((imodFgetline(vi->fp, line, IFDLINE_SIZE) > 0)){

    wprint("%s\n\r",line);
    imod_info_input();

    /* clear the return from the line. */
    for (i = 0; line[i]; i++)
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

      li = vi->li;            
      if (ilist->size == 1)
        image = (ImodImageFile *)ilistItem(ilist, ilist->size - 1);
      else {
        imodError(NULL, "3DMOD Error: " 
                "Image list file must specify one image file"
                " before the XYZ option.\n");
        exit(3);
      }
      iiPlistLoadF(vi->fp, li, 
                   image->nx, image->ny, image->nz);

      /* DNM 1/2/04: move adjusting of loading coordinates to fix_li call,
         move that call into list processing, eliminate setting cache size,
         since it will happen later, and break instead of continuing */
      vi->flippable = 0;
      break;
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
        filename = (char *)malloc(pathlen + 4);
        strcpy(filename, imgdir);
        strcat(filename, "/");
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
          exit(3);
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
      /* DNM: set up scaling for this image, leave last file in hdr/image */
      iiSetMM(image, vi->li->smin, vi->li->smax);
      vi->hdr = vi->image = image;
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
      image->time = vi->nt;
      vi->nt++;

      if (filename)
        free(filename);
      continue;
    }

    imodError(NULL, "3dmod warning: "
            "Unknown image list option (%s)\n", line);

  }
  rewind(vi->fp);
  /* end of while (getline) */

  /* save this in iv although it is an Ilist so ImageFile */
  vi->imageList = (ImodImageFile *)ilist;

  if (imgdir)
    free(imgdir);
  delete curdir;

  return(0);
}

/* Take a list of multiple files from the argument list and compose an
   image list */
void ivwMultipleFiles(ImodView *vi, char *argv[], int firstfile, int lastimage)
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
      exit(3);
    }

    /* set up scaling for this image */
    iiSetMM(image, vi->li->smin, vi->li->smax);

    /* Setting the fp keeps it from closing the file, so there is no need to
      save the filename too */
    /* 1/4/04: that made no sense!  Anyway, leave last file in vi->hdr/image */
    vi->fp = image->fp;
    vi->hdr = vi->image = image;
    iiClose(image);

    image->time = vi->nt;
    vi->nt++;

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
  vi->imageList = (ImodImageFile *)ilist;
}


/* Load images initially, use for all kinds of data */
int ivwLoadImage(ImodView *vi)
{
  int axisSave;
  int eret;

  if (vi->fakeImage){
    vi->xsize = vi->imod->xmax;
    vi->ysize = vi->imod->ymax;
    vi->zsize = vi->imod->zmax;
    vi->xybin = 1;
    vi->zbin = 1;
    vi->xUnbinSize = vi->xsize;
    vi->yUnbinSize = vi->ysize;
    vi->zUnbinSize = vi->zsize;

    wprint("Image size %d x %d, %d sections.\n",
           vi->xsize, vi->ysize, vi->zsize);
    best_ivwGetValue = fake_ivwGetValue;

    /* DNM: set the axis flag based on the model flip flag */
    if (vi->li->axis == 2) 
      imodError(NULL, "The -Y flag is ignored when loading a model"
              " without an image.\nUse Edit-Image-Flip to flip the"
              " model if desired");
    vi->li->axis = 3;
    if (vi->imod->flags & IMODF_FLIPYZ)
      vi->li->axis = 2;
    ImodPrefs->setInfoGeometry();
    return(0);
  }

  ivwProcessImageList(vi); 

  // Set info window up now that size is known
  ImodPrefs->setInfoGeometry();
  vi->doingInitialLoad = 1;
     
  /* Set up the cache and load it for a variety of conditions */
  if (vi->li->plist || vi->nt || vi->multiFileZ || vi->vmSize) {

    /* DNM: only one mode won't work now; just exit in either case */
    if (vi->hdr->mode == MRC_MODE_COMPLEX_SHORT){
      imodError(NULL, "3DMOD Error: "
                "Image cache and piece lists do not work with "
                "complex short data.\n");
      exit(3);
    }

    vi->idata = NULL;

    /* print load status */
    wprint("Image size %d x %d, %d sections.\n", vi->xsize, vi->ysize, 
           vi->zsize);
          
    /* initialize cache, make sure axis is set for all data structures 
       Set axis to 3 for first initialization because vmSize has been 
       computed based on unflipped Z dimensions */
    ivwSetCacheSize(vi);
    axisSave = vi->li->axis;
    vi->li->axis = 3;
    eret = ivwInitCache(vi);
    if (eret)
      return(eret);

    best_ivwGetValue = cache_ivwGetValue;
    
    /* If we are to keep cache full, fill it now and restore axis for possible
       flip later, unless user flipped it via menu */
    if (vi->keepCacheFull) {
      eret = imodCacheFill(vi);
      if (eret)
        return eret;
    }
    if (vi->li->axis == 3) 
      vi->li->axis = axisSave;
    else
      vi->li->axis = (axisSave == 3) ? 2 : 3;

  } else {

    /* Finally, here is what happens for non-cached data of all kinds */

    /* DNM 9/25/03: it probably no longer matters if data are ever contiguous
       so switch to noncontiguous for anything above 1 GB */
    vi->li->contig = 1; 
    if (1000000000 / vi->xysize < vi->zsize)
      vi->li->contig = 0;
    /*imodPrintStderr("contig = %d  axis = %d  xysize %d  % dzsize %d \n",
      vi->li->contig, vi->li->axis, xysize, 4200000000 / xysize, zsize); */

    best_ivwGetValue = idata_ivwGetValue;
    vi->idata = imod_io_image_load(vi);
    if (!vi->idata){
      /* Let caller do error message */
      return(-1);
    }
  }

  vi->doingInitialLoad = 0;

  if (App->depth == 8)
    ivwScale(vi);
     
  return (ivwManageInitialFlips(vi));
}

/* Process an image list and set up the cache and do initial load */
static int ivwProcessImageList(ImodView *vi)
{
  ImodImageFile *image;
  Ilist *ilist = (Ilist *)vi->imageList;
  int xsize, ysize, zsize, i;
  int rgbs = 0;

  if (!ilist->size)
    return -1;

  /* First get minimum x, y, z sizes of all the files and count up rgbs */
  for (i = 0; i < ilist->size; i++) {
    image = (ImodImageFile *)ilistItem(ilist, i);

    // See if mirroring of an FFT is needed:
    // MRC complex float odd size and not forbidden by option
    // Set flags and increase the nx
    if ((image->file == IIFILE_MRC || image->file == IIFILE_RAW) && 
        image->format == IIFORMAT_COMPLEX && image->type == IITYPE_FLOAT && 
        image->nx % 2 && vi->li->mirrorFFT >= 0) {
      image->mirrorFFT = 1;
      vi->li->mirrorFFT = 1;
      image->nx = (image->nx - 1) * 2;
    }

    if (!i || image->nx < xsize)
      xsize = image->nx;
    if (!i || image->ny < ysize)
      ysize = image->ny;
    if (!i || image->nz < zsize)
      zsize = image->nz;

    /* Add to count if RGB or not, to see if all the same type */
    if (image->format == IIFORMAT_RGB && 
        !((image->file == IIFILE_MRC  || image->file == IIFILE_RAW) && 
          vi->grayRGBs))
      rgbs++;
  }     

  /* Deal with RGB files */
  if (rgbs) {
    if (rgbs < ilist->size) {
      imodError(NULL, "3DMOD Error: Only %d files out of %d are "
              "RGB type and all files must be.\n", rgbs, ilist->size);
      exit(3);
    }
               
    if (!App->rgba) {
      imodError(NULL, "3DMOD Error: You must not start 3dmod with "
              "the -ci option to display RGB files.\n");
      exit(3);
    }
        
    /* Set the flag for storing raw images with the mode, and set rgba to 
       indicate the number of bytes being stored */
    App->rgba = 3;
    vi->rawImageStore = MRC_MODE_RGB;
  }

  if (!vi->li->plist) {

    /* Deal with non-montage case */
    /* If maximum Z is 1 and multifile treatment in Z is allowed, set zsize
       to number of files, and cancel treatment as times */
    if (ilist->size > 1 && zsize == 1 && vi->multiFileZ >= 0) {
      zsize = ilist->size;
      vi->multiFileZ = ilist->size;
      vi->ct = vi->nt = 0;
    }

    /* Use this to fix the load-in coordinates, then use those to set the
       lower left and upper right coords in each file - except for Z in the
       multifile Z case, which is set to 0 - 0 */
    mrc_fix_li(vi->li, xsize, ysize, zsize);
    ivwCheckBinning(vi, xsize, ysize, zsize);
    for (i = 0; i < ilist->size; i++) {
      image = (ImodImageFile *)ilistItem(ilist, i);
      image->llx = vi->li->xmin;
      image->lly = vi->li->ymin;
      image->llz = vi->multiFileZ > 0 ? 0 : vi->li->zmin;
      image->urx = vi->li->xmax;
      image->ury = vi->li->ymax;
      image->urz = vi->multiFileZ > 0 ? 0 : vi->li->zmax;
               
      /* If not an MRC file, or if multifile in Z, set to no
         flipping unless cache full */
      if ((image->file != IIFILE_MRC  && image->file != IIFILE_RAW) || 
          vi->multiFileZ > 0)
        vi->flippable = 0;

    }     
  } else {

    /* For montage, do the fix_li and see if it is rgb */
    mrc_fix_li(vi->li, 0, 0, 0);
    image = (ImodImageFile *)ilistItem(ilist, 0);
    ivwCheckBinning(vi, image->nx, image->ny, image->nz);
  }

  if (ilist->size == 1){

    /* for single file, cancel times and copy "list" to vi->image */
    vi->hdr = vi->image = iiNew();
    if (!vi->image){
      imodError(NULL, "Not enough memory.\n"); 
      exit(3);
    }
    memcpy(vi->image, ilist->data, sizeof(ImodImageFile));
    iiReopen(vi->image);
    vi->ct = vi->nt = 0;
    vi->imageList = NULL;

  } else {

    /* For multiple files, copy the whole image list to vi->imageList */
    vi->imageList = (ImodImageFile *)malloc
      (sizeof(ImodImageFile) * ilist->size);
    if (!vi->imageList) {
      imodError(NULL, "Not enough memory.\n"); 
      exit(3);
    }
    memcpy(vi->imageList, ilist->data,
           sizeof(ImodImageFile) * ilist->size);
    vi->hdr = vi->image = &vi->imageList[0];

    /* for times, set up initial time; for multifile Z, reopen first image */
    if (vi->multiFileZ <= 0) {
      ivwSetTime(vi, 1);
      vi->dim |= 8;
    } else {
      ivwReopen(vi->image);
    }
  }
          
  ilistDelete(ilist);
  return 0;
}


/* Take care of scaling the model, and flipping data and model as needed */
static int ivwManageInitialFlips(ImodView *vi)
{
  int flipit;
  int retcode;

  /* Transform model to match new image. */
  ivwTransModel(vi); 
     
  /* Unflip it if that didn't */
  if (vi->imod->flags & IMODF_FLIPYZ){
    imodFlipYZ(vi->imod);
    vi->imod->flags &= ~IMODF_FLIPYZ;
  }
     
  /* Flip data and model if called for, but do not generate error if it is not 
     flippable */
  flipit = (vi->li->axis == 2) ? 1 : 0;
  vi->li->axis = 3;
  if (flipit) {
    retcode = ivwFlip(vi);
    if (retcode && retcode != -1)
      return (retcode);
  }

  /* set model max values */
  vi->imod->xmax = vi->xUnbinSize;
  vi->imod->ymax = vi->yUnbinSize;
  vi->imod->zmax = vi->zUnbinSize;
  vi->imod->xybin = vi->xybin;
  vi->imod->zbin = vi->zbin;

  vi->imod->csum = imodChecksum(vi->imod);

  /* DNM: check wild flag here, after all the flipping is done */
  ivwCheckWildFlag(vi->imod);
  return 0;
}    

/* Check for binning and modify all parameters as necessary */
static int ivwCheckBinning(ImodView *vi, int nx, int ny, int nz)
{
  int nxbin, nybin, nzbin;
  int xmax, ymax, i;

  // Save original sizes of image
  vi->xUnbinSize  = vi->li->xmax - vi->li->xmin + 1;
  vi->yUnbinSize  = vi->li->ymax - vi->li->ymin + 1;
  vi->zUnbinSize  = vi->li->zmax - vi->li->zmin + 1;

  // Adjust binning to be positive and not larger than dimensions
  if (vi->xybin < 1)
    vi->xybin = 1;
  if (vi->zbin < 1)
    vi->zbin = 1;
  if (vi->xybin > nx)
    vi->xybin = nx;
  if (vi->xybin > ny)
    vi->xybin = ny;
  if (vi->zbin > nz)
    vi->zbin = nz;

  // Forbid binning for raw images
  if (vi->rawImageStore && vi->xybin * vi->zbin > 1) {
    vi->xybin = 1;
    vi->zbin = 1;
    wprint("\a\nBinning cannot be used with RGB data.\n");
  }

  // forbid Z binning for multifile Z or montage
  if ((vi->multiFileZ || vi->li->plist) && vi->zbin > 1) {
    vi->zbin = 1;
    if (vi->li->plist)
      wprint("\a\nThe Z dimension cannot be binned with montaged data.\n");
    else
      wprint("\a\nThe Z dimension cannot be binned with multiple "
             "single-section files.\n");
  }

  if (vi->xybin * vi->zbin > 1) {
    
    // Forbid flipped loading for non-isotropic binning
    if (vi->xybin != vi->zbin)
      vi->flippable = 0;

    // Get binned size of image file
    nxbin = nx / vi->xybin;
    nybin = ny / vi->xybin;
    nzbin = nz / vi->zbin;

    // If montaged, adjust piece coordinates and compute new full size
    if (vi->li->plist) {
      xmax = -1;
      ymax = -1;
      for (i = 0; i < vi->li->plist; i++) {
        vi->li->pcoords[3 * i] = (int)(vi->li->pcoords[3 * i] / 
                                       vi->xybin + 0.5);
        if (xmax < vi->li->pcoords[3 * i])
          xmax = vi->li->pcoords[3 * i];
        vi->li->pcoords[3 * i + 1] = (int)(vi->li->pcoords[3 * i + 1] /
                                           vi->xybin + 0.5);
        if (ymax < vi->li->pcoords[3 * i + 1])
          ymax = vi->li->pcoords[3 * i + 1];
      }
      nxbin += xmax;
      nybin += ymax;
      vi->li->px = nxbin;
      vi->li->py = nybin;
      vi->li->opx /= vi->xybin;
      vi->li->opy /= vi->xybin;
    }

    // Adjust load-in coordinates
    vi->li->xmin /= vi->xybin;
    vi->li->ymin /= vi->xybin;
    vi->li->zmin /= vi->zbin;
    vi->li->xmax /= vi->xybin;
    vi->li->ymax /= vi->xybin;
    vi->li->zmax /= vi->zbin;
    if (vi->li->xmax >= nxbin)
      vi->li->xmax = nxbin - 1;
    if (vi->li->ymax >= nybin)
      vi->li->ymax = nybin - 1;
    if (vi->li->zmax >= nzbin)
      vi->li->zmax = nzbin - 1;

  }

  // Set loaded image size for current loadin
  vi->xsize  = vi->li->xmax - vi->li->xmin + 1;
  vi->ysize  = vi->li->ymax - vi->li->ymin + 1;
  vi->zsize  = vi->li->zmax - vi->li->zmin + 1;
  vi->xysize = vi->xsize * vi->ysize;
  
  return 0;
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

/* Delete all contours in extra object */
void ivwClearExtraObject(ImodView *inImodView)
{
  Iobj *obj;
  if (inImodView == NULL) 
    return;
  obj = inImodView->extraObj;
  if (!obj->contsize)
    return;
  imodContoursDelete(obj->cont, obj->contsize);
  obj->contsize = 0;
  obj->cont = NULL;
}

// Get the current contour, the last contour if it is empty, or a new contour
Icont *ivwGetOrMakeContour(ImodView *vw, Iobj *obj, int timeLock)
{
  Icont *cont = imodContourGet(vw->imod);
  int curTime = timeLock ? timeLock : vw->ct;
  if (!cont) {
  
    // Set index to last contour, both to use that contour if it is empty and
    // so that its properties (surface and open/closed) are inherited if a new
    // contour is made
    vw->imod->cindex.contour = obj->contsize - 1;
    cont = imodContourGet(vw->imod);
    if (!cont || cont->psize) {

      // Actually get a new contour now if last one is not empty
      vw->undo->contourAddition(obj->contsize);
      imodNewContour(vw->imod);
      cont = imodContourGet(vw->imod);
      if (!cont) {
        vw->undo->clearUnits();
        return NULL;
      }
      ivwSetNewContourTime(vw, obj, cont);
    }
  }

  // If contour is empty and time doesn't match, 
  // reassign it to the current or requested time
  if (ivwTimeMismatch(vw, timeLock, obj, cont) && !cont->psize) {
    vw->undo->contourPropChg();
    cont->type = curTime;
  }

  // If current point index is not set, set it to end of contour
  if (vw->imod->cindex.point < 0)
    vw->imod->cindex.point = cont->psize - 1;

  return cont;
}

/* Return true if there are multiple images, contours have times in this 
   object, this contour has a non-zero time, and this time does not match
   current display time, which is either global time or timelock time */
bool ivwTimeMismatch(ImodView *vi, int timelock, Iobj *obj, Icont *cont)
{
  int time = timelock ? timelock : vi->ct;
  return (vi->nt > 0 && iobjFlagTime(obj) && (cont->flags & ICONT_TYPEISTIME)
	  && cont->type && (time != cont->type));
}

/* Inserts a point in the current contour after checking whether it makes it
   wild, registers contour change if needed and point addition, then closes
   the open unit */
int ivwRegisterInsertPoint(ImodView *vi, Icont *cont, Ipoint *pt, int index)
{
  int ret;
  if (cont->psize && (int)floor(cont->pts->z + 0.5) != (int)floor(pt->z + 0.5)
      && !(cont->flags & ICONT_WILD))
    vi->undo->contourPropChg();
  vi->undo->pointAddition(index);
  ret = imodInsertPoint(vi->imod, pt, index);
  if (ret <= 0)
    vi->undo->flushUnit();
  else
    vi->undo->finishUnit();
  return ret;
}

int  ivwDraw(ImodView *inImodView, int inFlags)
{
  imodDraw(inImodView, inFlags);
  return(0);
}

int ivwRedraw(ImodView *vi)
{
  return (imod_redraw(vi));
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

/* Bin an array by the binning factor */
void ivwBinByN(unsigned char *array, int nxin, int nyin, int nbin, 
                      unsigned char *brray)
{
  int i, j;
  int nbinsq = nbin * nbin;
  int nxout = nxin / nbin;
  int nyout = nyin / nbin;
  int ixofs = (nxin % nbin) / 2;
  int iyofs = (nyin % nbin) / 2;
  int sum, ix, iy;
  unsigned char *bdata = brray;
  unsigned char *cline1, *cline2, *cline3, *cline4;


  switch (nbin) {
  case 2:
    for (iy = 0; iy < nyout; iy++) {
      cline1 = ((unsigned char *)array) + 2 * iy * nxin;
      cline2 = cline1 + nxin;
      for (ix = 0;   ix < nxout; ix++) {
        sum = *cline1 + *(cline1 + 1) + *cline2 + *(cline2 + 1);
        *bdata++ = sum / 4;
        cline1 += 2;
        cline2 += 2;
      }
    }
    break;
  
  case 3:
    for (iy = 0; iy < nyout; iy++) {
      cline1 = ((unsigned char *)array) + (3 * iy + iyofs) * nxin + ixofs;
      cline2 = cline1 + nxin;
      cline3 = cline2 + nxin;
      for (ix = 0; ix < nxout; ix++) {
        sum = *cline1 + *(cline1 + 1) + *(cline1 + 2) +
          *cline2 + *(cline2 + 1) + *(cline2 + 2) +
          *cline3 + *(cline3 + 1) + *(cline3 + 2);
        *bdata++ = sum / 9;
        cline1 += 3;
        cline2 += 3;
        cline3 += 3;
      }
    }
    break;
        

  case 4:
    for (iy = 0; iy < nyout; iy++) {
      cline1 = ((unsigned char *)array) + (4 * iy + iyofs) * nxin + ixofs;
      cline2 = cline1 + nxin;
      cline3 = cline2 + nxin;
      cline4 = cline3 + nxin;
      for (ix = 0; ix < nxout; ix++) {
        sum = *cline1 + *(cline1 + 1) + *(cline1 + 2) + *(cline1 + 3) +
          *cline2 + *(cline2 + 1) + *(cline2 + 2) + *(cline2 + 3) +
          *cline3 + *(cline3 + 1) + *(cline3 + 2) + *(cline3 + 3) +
          *cline4 + *(cline4 + 1) + *(cline4 + 2) + *(cline4 + 3);
        *bdata++ = sum / 16;
        cline1 += 4;
        cline2 += 4;
        cline3 += 4;
        cline4 += 4;
      }
    }
    break;

  default:
    for (iy = 0; iy < nyout; iy++) {
      cline1 = ((unsigned char *)array) + (nbin * iy + iyofs) * nxin + ixofs;
      for (ix = 0; ix < nxout; ix++) {
        sum = 0;
        cline2 = cline1;
        for (j = 0; j < nbin; j++) {
          for (i = 0; i < nbin; i++) 
            sum += cline2[i];
          cline2 += nxin;
        }
        *bdata++ = sum / nbinsq;
        cline1 += nbin;
      }
    }
    break;
  }
}

/*
$Log$
Revision 4.36  2005/08/15 02:07:58  mast
Fixed scaling when a model with no refImage is displayed on binned data

Revision 4.35  2005/03/20 19:55:36  mast
Eliminating duplicate functions

Revision 4.34  2005/02/19 01:29:38  mast
Added function to clear extra object

Revision 4.33  2004/12/02 21:42:23  mast
Changes for raw image loading

Revision 4.32  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.31  2004/11/07 22:59:52  mast
Make binning routine global

Revision 4.30  2004/11/04 17:01:31  mast
Changes for loading FFTs with internal mirroring

Revision 4.29  2004/11/01 23:34:56  mast
Initialized selection list

Revision 4.28  2004/10/27 20:37:39  mast
Changed cache dumper to take ImodImageFile and only dump for MRC file

Revision 4.27  2004/10/22 22:18:04  mast
Added functions for dumping file system cache after each section is
loaded, works in Linux only

Revision 4.26  2004/07/13 22:29:54  mast
Fixed bug in getting file values for flipped data leaded as a subset

Revision 4.25  2004/07/11 18:19:38  mast
Functions to set time of new contour and get/make contour for adding points

Revision 4.24  2004/07/07 19:25:29  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 4.23  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.22  2004/01/07 01:54:25  mast
Needed to add a '/' in using IMGDIR as prefix

Revision 4.21  2004/01/06 16:55:32  mast
Fixed handling of rgb files to do it first when processing image list

Revision 4.20  2004/01/05 17:21:39  mast
Added binning option, cleaned up file started, reorganized file

Revision 4.19  2003/12/31 05:31:30  mast
Fix problem in getFileValue when switching files

Revision 4.18  2003/12/30 06:27:37  mast
Implemented treatment of multiple single-image files as sections in Z

Revision 4.17  2003/12/04 22:57:17  mast
Set info window position for fake images too

Revision 4.16  2003/11/01 18:12:17  mast
changed to put out virtually all error messages to a window

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

/*
 *  imodview.cpp -- Handle the ImodView structure.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
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
#include "imod.h"
#include "cachefill.h"
#include "client_message.h"
#include "iirawimage.h"
#include "display.h"
#include "info_cb.h"
#include "form_info.h"
#include "info_setup.h"
#include "imod_io.h"
#include "imod_edit.h"
#include "moviecon.h"
#include "rescale.h"
#include "mv_objed.h"
#include "object_edit.h"
#include "iproc.h"
#include "autox.h"
#include "xzap.h"
#include "sslice.h"
#include "zap_classes.h"
#include "slicer_classes.h"
#include "xcramp.h"
#include "workprocs.h"
#include "vertexbuffer.h"
#include "utilities.h"
#include "pyramidcache.h"
#include "preferences.h"
#include "undoredo.h"

static int ivwProcessImageList(ImodView *vi);
static int initializeFlipAndModel(ImodView *vi);
static int ivwCheckLinePtrAllocation(ImodView *vi, int ysize);
static int ivwCheckBinning(ImodView *vi, int nx, int ny, int nz);
static int snapshotTopWindow(QString &name, int format, bool checkGrayConvert, 
                             int winType, bool fullArea);
static int getTopZapOrSlicer(int winType, ZapFuncs **zap, SlicerFuncs **slicer);
static float ivwReadBinnedPoint(ImodView *vi, ImodImageFile *image, int cx, int cy,
                                int cz);

/* default settings for the view info structure. */
void ivwInit(ImodView *vi, bool modview)
{
  if (!vi)
    return;
  vi->xmouse = vi->ymouse = vi->zmouse = 0.0f;
  /*     vi->xtrans = vi->ytrans = vi->ztrans = 0; */

  vi->xmovie = vi->ymovie = vi->zmovie = vi->tmovie = 0;
  vi->xsize  = vi->ysize  = vi->zsize  = 0;
  vi->fullXsize  = vi->fullYsize  = vi->fullZsize  = 0;
  vi->xysize = 0;

  vi->numTimes = 0; vi->curTime = 0;

  // Initialize things needed for model view and then stop if model view only
  vi->imod       = NULL;
  vi->selectionList = NULL;
  vi->extraObj = NULL;
  vi->extraObjInUse = NULL;
  startExtraObjectIfNone(vi);
  vi->undo = new UndoRedo(vi);
  vi->modelViewVi = modview ? 1 : 0;
  vi->xybin = 1;
  vi->zbin = 1;
  if (modview)
    return;
     
  imcSetMovierate(vi, 0);

  vi->vmSize     = 0;
  vi->keepCacheFull = 1;
  vi->fullCacheFlipped = 0;
  vi->imagePyramid = 0;
  vi->stripOrTileCache = 0;
  vi->pyrCache = NULL;
  vi->loadingImage = 0;
  vi->doingInitialLoad = 0;
  vi->black      = 0;
  vi->white      = 255;
  vi->blackInRange = 0;
  vi->whiteInRange = 255;
  vi->rangeLow   = 0;
  vi->rangeHigh  = 65535;
  vi->fastdraw   = 0;
  vi->dim        = 1+2+4;
  vi->ax         = NULL;
  vi->ctrlist    = NULL;

  vi->idata      = NULL;
  vi->fp         = NULL;

  vi->imageList  = NULL;
  vi->image = NULL;
  vi->numTiltAngles = 0;
  vi->tiltAngles = NULL;

  vi->movieInterval = 17L;
  vi->timers = new ImodWorkproc(vi);
  vi->movieRunning = 0;
  vi->ghostmode = 0;
  vi->ghostlast = IMOD_GHOST_SECTION;
  vi->ghostdist = 0;
  vi->obj_moveto = 1;
  vi->drawcursor = TRUE;
  vi->insertmode = 0;
  vi->overlaySec = 0;
  vi->overlayRamp = -1;
  vi->drawStipple = 0;
  vi->trackMouseForPlugs = 0;

  vi->fakeImage     = 0;
  vi->rawImageStore = 0;
  vi->ushortStore = 0;
  vi->rgbStore = 0;
  vi->multiFileZ = 0;
  vi->noReadableImage = 0;
  vi->linePtrs = NULL;
  vi->linePtrMax = 0;
  vi->blankLine = NULL;
  vi->flippable = 1;
  vi->grayRGBs = 0;
  vi->reloadable = 0;
  vi->colormapImage = 0;
  vi->equalScaling = 0;
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

/* Get the current Z section; with tile cache, this call will allocate a full section 
   array and load tiles to fill it */
unsigned char **ivwGetCurrentZSection(ImodView *vi)
{
  int cz = (int)(vi->zmouse + 0.5f);
  if (vi->pyrCache)
    return (vi->pyrCache->getFullSection(cz));
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

  if (!vi)
    return NULL;
  if (!vi->numTimes)
    return(ivwGetZSection(vi, section));

  /* DNM: make test > instead of >= */
  if (time < 1 || time > vi->numTimes)
    return(NULL);

  ivwGetTime(vi, &oldTime);
  if (time == oldTime) 
    return(ivwGetZSection(vi, section));

  vi->curTime = time;
  vi->hdr = vi->image = &vi->imageList[time-1];
  ivwReopen(vi->image);
  imageData = ivwGetZSection(vi, section);
  iiClose(vi->image);
  vi->curTime = oldTime;
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
  if (App->depth == 8 && !vi->rawImageStore) {
    for (i = 0; i < mi; i++) {
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

  if (section < 0 || section >= vi->zsize) 
    return(NULL);
  if (!vi->fp || vi->fakeImage || vi->loadingImage || vi->pyrCache)
    return(NULL);
  if (!vi->fullCacheFlipped && ivwPlistBlank(vi, section)) 
    return(NULL);

  // Flip -> rotation: invert Z if flipped
  if (vi->li->axis == 2)
    section = vi->zsize - 1 - section;

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
      slice = vi->cacheIndex[sl*vi->vmTdim + vi->curTime - vi->vmTbase];
      if (slice < 0)
        vi->linePtrs[sl] = vi->blankLine;
      else
        vi->linePtrs[sl] = vi->vmCache[slice].sec->data.b + 
          vi->xsize * pixSize * section;
    }
    return (vi->linePtrs);
  }

  /* Cached data otherwise */
  sl = vi->cacheIndex[section * vi->vmTdim + vi->curTime - vi->vmTbase];
  /* imodPrintStderr("sect %d slice %d\n", section, sl);*/
  if (sl >= 0)
    tempSlicePtr = &(vi->vmCache[sl]);
  else {

    /* Didn't find slice in cache, need to load it in. */

    /* DNM 12/12/01: add call to cache filler */
    if (icfGetAutofill())
      return(ivwMakeLinePointers(vi, icfDoAutofill(vi, section), vi->xsize,
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
    tempSlicePtr->ct = vi->curTime;
    vi->cacheIndex[section * vi->vmTdim + vi->curTime - vi->vmTbase] = sl;
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

/* Read a section of data into the cache */
void ivwReadZ(ImodView *vi, unsigned char *buf, int cz)
{
  int zread, fill;
  b3dUInt16 *usbuf = (b3dUInt16 *)buf;

  /* Image in not a stack but loaded into pieces. */
  if (vi->li->plist) {
  
    int mx, my; /* the size of the section buffer */
    int ox, oy; /* data offset/origin  */
    unsigned int i, mxy, bxy;
    int nxbin, nybin; 
    float pixLoaded = 0.;
    float checkCrit = 1.e7;
      
    /* DNM 1/3/04: use function instead of explicit tests */
    int pixSize = ivwGetPixelBytes(vi->rawImageStore);

    mx = vi->xsize;    my = vi->ysize;
    ox = vi->li->xmin; oy = vi->li->ymin;
    mxy = mx * my;

    nxbin = vi->hdr->nx / vi->xybin;
    nybin = vi->hdr->ny / vi->xybin;

    /* DNM: make the buffer the size of input pieces */
    bxy = nxbin * nybin;

    // Clear image buffer we will write to with scaled value of image mean for real data
    // Otherwise just set to midrange
    if (vi->image->format == IIFORMAT_LUMINANCE) {
      fill = (int)(vi->image->amean * vi->image->slope + vi->image->offset);
      if (vi->ushortStore) {
        B3DCLAMP(fill, 0, 65535);
        for (i = 0; i < mxy; i++)
          usbuf[i] = fill;
      } else {
        B3DCLAMP(fill, 0, 255);
        memset(buf, fill, mxy * pixSize);
      }
    } else
      memset(buf, 127, mxy * pixSize);

    /* Setup load buffer. */
    if (plistBufSize < bxy) {
      B3DFREE(plistBuf);
      plistBufSize = 0;
      plistBuf = (unsigned char *)malloc(bxy * pixSize);
      if (!plistBuf) 
        return;
      plistBufSize = bxy;
    }
    cz += vi->li->zmin;

    /* Check each piece and copy its parts into the section. */
    for (i = 0; i < vi->li->plist; i++) {
      int iox, ioy, fox, foy;
      int xsize, ysize;
      int fskip, iskip;
      int llx, lly, urx, ury;

      if ( vi->li->pcoords[(i*3)+2] == cz) {
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

        // Periodically check for a user exit
        pixLoaded += (float)xsize * ysize * vi->xybin * vi->xybin;
        if (pixLoaded > checkCrit) {
          imod_info_input();
          if (App->exiting)
            exit(0);
          pixLoaded = 0.;
        }
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
  case MRC_MODE_USHORT:
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
  vi->linePtrs[0] = data;
  for (i = 1; i < ysize; i++)
    vi->linePtrs[i] = vi->linePtrs[i - 1] + pixSize * xsize;
  return (vi->linePtrs);
}

/* Read a section, potentially with binning */
int ivwReadBinnedSection(ImodView *vi, char *buf, int section) 
{
  return ivwReadBinnedSection(vi, vi->image, buf, section);
}

int ivwReadBinnedSection(ImodView *vi, ImodImageFile *image, char *buf, int section)
{
  int xsize, ysize, ix, iy, iz, pixsize, xOffset, leftXpad = 0, rightXpad = 0;
  int yOffset, leftYpad = 0, rightYpad = 0, zOffset, leftZpad, rightZpad;
  bool blankX = false, blankY = false, blankZ = false;
  unsigned char *unbinbuf = NULL;
  unsigned char *ucbuf = (unsigned char *)buf;
  b3dUInt16 *usbuf = (b3dUInt16 *)buf;
  unsigned char *inbuf, *outbuf;
  b3dUInt16 *usin, *usout;
  unsigned short fill;
  int *binbuf = NULL;
  ImodImageFile im;
  size_t xbinned, ybinned, xybinned, i, numBytes, numPix;;
  double sum;

  pixsize = ivwGetPixelBytes(vi->rawImageStore);

  // Copy image structure and adjust load-in coordinates
  im = *image;
  if (!vi->li->plist && !vi->pyrCache) {
    blankX  = ivwFixUnderSizeCoords(vi->fullXsize, im.nx / vi->xybin, im.llx, im.urx, 
                                    xOffset, leftXpad, rightXpad);
    blankY  = ivwFixUnderSizeCoords(vi->fullYsize, im.ny / vi->xybin, im.lly, im.ury,
                                    yOffset, leftYpad, rightYpad);
    if (vi->multiFileZ <= 0)
      blankZ  = ivwFixUnderSizeCoords(vi->fullZsize, im.nz / vi->zbin, im.llz, im.urz,
                                    zOffset, leftZpad, rightZpad);

    // Adjust the section number appropriately for the axis and see if section exists
    if (vi->multiFileZ <= 0 && !(blankX || blankY || blankZ)) {
      if (im.axis == 3) {
        section -= zOffset;
        blankZ = section < 0 || section >= im.nz / vi->zbin;
      } else {
        section -= yOffset;
        blankY = section < 0 || section >= im.ny / vi->xybin;
        leftYpad = leftZpad;
        rightYpad = rightZpad;
      }
    }
  }

  // Fill a blank image if any axis is out of range
  numPix = (size_t)vi->xsize * (size_t)vi->ysize;
  numBytes = numPix * pixsize;
  if (blankX || blankY || blankZ) {
    if (vi->ushortStore)
      for (i = 0; i < numPix; i++)
        *usbuf++ = 32767;
    else
      for (i = 0; i < numPix; i++)
        *buf++ = 127;
    return 0;
  }

  // Now load the image normally with these adjusted coordinates
  ivwGetFileStartPos(image);
  xbinned = im.urx + 1 - im.llx;
  ybinned = im.axis == 3 ? im.ury + 1 - im.lly : im.urz + 1 - im.llz;
  xybinned = xbinned * ybinned;
  //imodPrintStderr("xbin %d  ybin %d\n", xbinned, ybinned);
  
  // If there is no binning, just call the raw or byte routines
  if (vi->xybin * vi->zbin == 1) {
    if (vi->rgbStore) 
      iiReadSection(&im, buf, section);
    else if (vi->ushortStore)
      iiReadSectionUShort(&im, buf, section);
    else
      iiReadSectionByte(&im, buf, section);
  } else {

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
    unbinbuf = (unsigned char *)malloc((size_t)xsize * (size_t)ysize * pixsize);
    if (!unbinbuf)
      return 1;
    if (vi->zbin > 1) {
      binbuf = B3DMALLOC(int, xybinned);
      if (!binbuf) {
        free (unbinbuf);
        return 1;
      }
      for (i = 0; i < xybinned; i++)
        binbuf[i] = 0;
    }

    // Loop through the unbinned sections to read and bin them into buf
    for (iz = 0; iz < vi->zbin; iz++) {
      if (vi->rgbStore) 
        iiReadSection(&im, (char *)unbinbuf, section);
      else if (vi->ushortStore)
        iiReadSectionUShort(&im, (char *)unbinbuf, vi->zbin * section + iz);
      else
        iiReadSectionByte(&im, (char *)unbinbuf, vi->zbin * section + iz);
      reduceByBinning(unbinbuf, vi->rawImageStore, xsize, ysize, vi->xybin, buf, 1, &ix,
                      &iy);

      // For multiple sections, move or add to the binned buffer
      if (vi->zbin > 1) {
        if (vi->ushortStore)
          for (i = 0; i < xybinned; i++)
            binbuf[i] += usbuf[i];
        else
          for (i = 0; i < xybinned; i++)
            binbuf[i] += ucbuf[i];
      }
    }

    // And divide binned value into final buffer
    if (vi->zbin > 1) {
      if (vi->ushortStore)
        for (i = 0; i < xybinned; i++)
          usbuf[i] = binbuf[i] / vi->zbin;
      else
        for (i = 0; i < xybinned; i++)
          buf[i] = binbuf[i] / vi->zbin;
    }

    free(unbinbuf);
    if (binbuf)
      free(binbuf);
  }
  ivwDumpFileSysCache(image);

  // If the image is at all undersized, now it needs to be copied up in array and
  // padding applied
  if (leftXpad || leftYpad || rightXpad || rightYpad) {
    /*imodPrintStderr("llx %d urx %d left %d right %d\n", im.llx, im.urx, leftXpad, 
      rightXpad); */

    // Get an edge mean for byte data
    fill = 127;
    if (!vi->rgbStore) {
      sum = 0.;
      if (vi->ushortStore) {
        for (i = 0; i < xbinned; i++)
          sum += (double)usbuf[i] + usbuf[i + (ybinned - 1) * xbinned];
        for (i = 1; i < ybinned - 1; i++)
          sum += (double)usbuf[i * xbinned] + usbuf[xbinned - 1 + i * xbinned];
      } else {
        for (i = 0; i < xbinned; i++)
          sum += (double)ucbuf[i] + ucbuf[i + (ybinned - 1) * xbinned];
        for (i = 1; i < ybinned - 1; i++)
          sum += (double)ucbuf[i * xbinned] + ucbuf[xbinned - 1 + i * xbinned];
      }
      fill = B3DNINT(sum / (2. * (xbinned + ybinned - 2)));
    }

    if (vi->ushortStore) {
      usout = usbuf + numPix - 1;
      usin = usbuf + xybinned - 1;

      // Do fill at end
      for (i = 0; i < (vi->xsize * rightYpad + rightXpad); i++)
        *usout-- = fill;

      // For each line, copy data and fill left side and right side of previous line
      for (iy = ybinned - 1; iy >= 0; iy--) {
        for (i = 0; i < xbinned; i++)
          *usout-- = *usin--;
        for (i = 0; i < (leftXpad + (iy ? rightXpad : 0)); i++)
          *usout-- = fill;
      }

      // Do fill at start
      for (i = 0; i < (vi->xsize * leftYpad); i++)
        *usout-- = fill;

    } else {
      outbuf = ucbuf + numBytes - 1;
      inbuf = ucbuf + xybinned * pixsize - 1;

      // Do fill at end
      for (i = 0; i < (vi->xsize * rightYpad + rightXpad) * pixsize; i++)
        *outbuf-- = fill;

      // For each line, copy data and fill left side and right side of previous line
      for (iy = ybinned - 1; iy >= 0; iy--) {
        for (i = 0; i < xbinned * pixsize; i++)
          *outbuf-- = *inbuf--;
        for (i = 0; i < (leftXpad + (iy ? rightXpad : 0)) * pixsize; i++)
          *outbuf-- = fill;
      }

      // Do fill at start
      for (i = 0; i < (vi->xsize * leftYpad) * pixsize; i++)
        *outbuf-- = fill;
    }
  }
  return 0;
}

/*
 * Sets up the load-in coordinates for a dimension where an image is not as large
 * as the full size
 */
bool ivwFixUnderSizeCoords(int size, int nx, int &llx, int &urx, int &offset, 
                           int &leftPad, int &rightPad)
{
  offset = (size - nx) / 2;
  leftPad = rightPad = 0;
  llx -= offset;
  urx -= offset;
  if (llx < 0) {
    leftPad = -llx;
    llx = 0;
  }
  if (urx >= nx) {
    rightPad = urx + 1 - nx;
    urx = nx - 1;
  }
  return urx < 0 || llx >= nx;
}

/*
 * Returns the load-in start and the padding around an image at the given section and 
 * time value.  The Y value is also needed for flipped data when multifile Z is loaded;
 * set this < 0 if the offsets are not specific to Y and undefined for multifile Z if 
 * flipped.  Returns -1 if the section is out of range for multifile case, or 1 if the 
 * image is blank.
 */
int ivwGetImagePadding(ImodView *vi, int cy, int section, int time, int &llX, 
                       int &leftXpad, int &rightXpad, int &llY, int &leftYpad,
                       int &rightYpad, int &llZ, int &leftZpad, int &rightZpad)
{
  ImodImageFile im;

  int fz;
  bool blankX, blankY, blankZ = false;

  if (vi->fakeImage) {
    llX = leftXpad = rightXpad = llY = leftYpad = rightYpad = llZ = leftZpad = 
      rightZpad = 0.;
    return 1;
  }

  // Copy the right image file structure for the situation
  leftZpad = rightZpad = llZ = 0;
  if (vi->multiFileZ > 0) {
    fz = vi->li->axis == 3 ? section : cy;
      if (fz < 0 || fz >= vi->multiFileZ)
        return -1;
      im = vi->imageList[fz];
  } else if (time > 0)
    im = vi->imageList[time - 1];
  else
    im = *(vi->image);

  if (vi->li->plist) {
    leftXpad = rightXpad = leftYpad = rightYpad = leftZpad = rightZpad = 0;
    llX = im.llx;
    llY = im.lly;
    llZ = im.llz;
    return 0;
  }

  // Get the padding on each axis
  blankX = ivwFixUnderSizeCoords(vi->fullXsize, im.nx / vi->xybin, im.llx, im.urx, fz,
                                 leftXpad, rightXpad);
  blankY = ivwFixUnderSizeCoords(vi->fullYsize, im.ny / vi->xybin, im.lly, im.ury, fz,
                                 leftYpad, rightYpad);
  llX = im.llx;
  llY = im.lly;
  if (vi->multiFileZ <= 0) {
    blankZ = ivwFixUnderSizeCoords(vi->fullZsize, im.nz / vi->zbin, im.llz, im.urz,  fz,
                                   leftZpad, rightZpad);
    llZ = im.llz;
  }
  
  // Swap Y and Z padding if flipped
  if (vi->li->axis == 2) {
    fz = leftYpad;
    leftYpad = leftZpad;
    leftZpad = fz;
    fz = rightYpad;
    rightYpad = rightZpad;
    rightZpad = fz;
  }
  return (blankX || blankY || blankZ) ? 1 : 0;
}


#ifdef __linux
static fpos_t startPos;
static bool skipDumping = false;
#endif

void ivwGetFileStartPos(ImodImageFile *image)
{
#ifdef __linux
  skipDumping = (getenv("IMOD_DUMP_FSCACHE") == NULL);
  if (skipDumping || !image->fp || 
      (image->file != IIFILE_MRC && image->file != IIFILE_RAW))
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
  if (skipDumping || !image->fp || 
      (image->file != IIFILE_MRC && image->file != IIFILE_RAW))
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

static int idata_ivwGetValue(ImodView *vi, int x, int y, int z)
{
  /* DNM: calling routine is responsible for limit checks */
  if (vi->li->axis == 3)
    return(vi->idata[z][x + (y * vi->xsize)]);
  else
    return(vi->idata[y][x + ((vi->zsize - 1 - z) * vi->xsize)]);
}

/* 1/3/04: eliminated fileScale_ivwGetValue which was unussed, incorrect,
   and the only user of li->slope and offset */

static int cache_ivwGetValue(ImodView *vi, int x, int y, int z)
{
  ivwSlice *tempSlicePtr = 0;
  unsigned char *image;
  b3dUInt16 *usimage;
  int sl;
  size_t index;

  /* find pixel in cache */
  // Flip -> rotation: invert Z
  if (vi->li->axis == 2)
    z = vi->zsize - 1 - z;

  /* If full cache and flipped, swap y and z */
  if (vi->fullCacheFlipped) {
    sl = z;
    z = y;
    y = sl;
  }

  /* get slice if it is loaded */
  sl = vi->cacheIndex[z * vi->vmTdim + vi->curTime - vi->vmTbase];
  if (sl < 0)
    return(0);

  tempSlicePtr = &vi->vmCache[sl];
  index = (size_t)y * (size_t)tempSlicePtr->sec->xsize + x;

  /* DNM: calling routine is responsible for limit checks */
  if (vi->ushortStore) {
    usimage = tempSlicePtr->sec->data.us;
    if (!usimage)
      return(0);
    return(usimage[index]);
  }
  image = tempSlicePtr->sec->data.b;
  if (!image)
    return(0);
  return(image[index]);
}

static int fake_ivwGetValue(ImodView *vi, int x, int y, int z)
{
  return(0);
}

static int tiles_ivwGetValue(ImodView *vi, int x, int y, int z)
{
  return vi->pyrCache->getValueFromBaseCache(x, y, z);
}


/* A map that can be used for returned unsigned shorts */
unsigned char *ivwUShortInRangeToByteMap(ImodView *vi)
{
  float slope = 255. / (vi->rangeHigh - vi->rangeLow);
  float offset = -slope * vi->rangeLow;
  return (get_short_map(slope, offset, 0, 255, MRC_RAMP_LIN, 0, 0));
}


/*
 * Routines for fast access from slicer, tumbler, and xyz
 */

/* Static variables to hold the essentials after it is set up */
static int imdataxsize;
static int *vmdataxsize;
static unsigned char **imdata;
static b3dUInt16 **usimdata;
static int imdataMax = 0;
static int vmnullvalue;
static int rgbChan;
static int zsizeFac;
static int xzsizeFac;
static size_t xzsizeBigFac;
static int numXfastTiles, numYfastTiles, fastTileXdelta, fastTileYdelta;
static int fastTileXoffset, fastTileYoffset;

/* A global variable with the appropriate function - because I had trouble
   returning that from the setup function */
int (*ivwFastGetValue)(int x, int y, int z);

/* The access routines */
static int idata_GetValue(int x, int y, int z)
{
  return(imdata[z][x + (y * imdataxsize)]);
}

static int idata_BigGetValue(int x, int y, int z)
{
  return(imdata[z][x + ((size_t)y * (size_t)imdataxsize)]);
}

static int flipped_GetValue(int x, int y, int z)
{
  return(imdata[y][x + xzsizeFac - (z * imdataxsize)]);
}

static int flipped_BigGetValue(int x, int y, int z)
{
  return(imdata[y][x + xzsizeBigFac - ((size_t)z * (size_t)imdataxsize)]);
}

static int cache_GetValue(int x, int y, int z)
{
  if (!imdata[z])
    return(vmnullvalue);
  return(imdata[z][x + (y * vmdataxsize[z])]);
}

static int cache_BigGetValue(int x, int y, int z)
{
  if (!imdata[z])
    return(vmnullvalue);
  return(imdata[z][x + ((size_t)y * (size_t)vmdataxsize[z])]);
}

static int cache_GetFlipped(int x, int y, int z)
{
  if (!imdata[y])
    return(vmnullvalue);
  return(imdata[y][x + ((zsizeFac - z) * vmdataxsize[y])]);
}

static int cache_BigGetFlipped(int x, int y, int z)
{
  if (!imdata[y])
    return(vmnullvalue);
  return(imdata[y][x + ((size_t)(zsizeFac - z) * (size_t)vmdataxsize[y])]);
}

static int idata_ChanValue(int x, int y, int z)
{
  return(imdata[z][3 * (x + (y * imdataxsize)) + rgbChan]);
}

static int idata_BigChanValue(int x, int y, int z)
{
  return(imdata[z][3 * (x + ((size_t)y * (size_t)imdataxsize)) + rgbChan]);
}

static int flipped_ChanValue(int x, int y, int z)
{
  return(imdata[y][3 * (x + xzsizeFac - (z * imdataxsize)) + rgbChan]);
}

static int flipped_BigChanValue(int x, int y, int z)
{
  return(imdata[y][3 * (x + xzsizeBigFac - ((size_t)z * (size_t)imdataxsize)) + rgbChan]);
}

static int cache_ChanValue(int x, int y, int z)
{
  if (!imdata[z])
    return(vmnullvalue);
  return(imdata[z][3 * (x + (y * vmdataxsize[z ])) + rgbChan]);
}

static int cache_BigChanValue(int x, int y, int z)
{
  if (!imdata[z])
    return(vmnullvalue);
  return(imdata[z][3 * (x + ((size_t)y * (size_t)vmdataxsize[z ])) + rgbChan]);
}

static int cache_ChanFlipped(int x, int y, int z)
{
  if (!imdata[y])
    return(vmnullvalue);
  return(imdata[y][3 * (x + ((zsizeFac - z) * vmdataxsize[y])) + rgbChan]);
}

static int cache_BigChanFlipped(int x, int y, int z)
{
  if (!imdata[y])
    return(vmnullvalue);
  return(imdata[y][3 * (x + (size_t)(zsizeFac - z) * (size_t)vmdataxsize[y]) + rgbChan]);
}

static int idata_GetUSValue(int x, int y, int z)
{
  return(usimdata[z][x + (y * imdataxsize)]);
}

static int idata_BigGetUSValue(int x, int y, int z)
{
  return(usimdata[z][x + ((size_t)y * (size_t)imdataxsize)]);
}

static int flipped_GetUSValue(int x, int y, int z)
{
  return(usimdata[y][x + xzsizeFac - (z * imdataxsize)]);
}

static int flipped_BigGetUSValue(int x, int y, int z)
{
  return(usimdata[y][x + xzsizeBigFac - ((size_t)z * (size_t)imdataxsize)]);
}

static int cache_GetUSValue(int x, int y, int z)
{
  if (!usimdata[z])
    return(vmnullvalue);
  return(usimdata[z][x + (y * vmdataxsize[z])]);
}

static int cache_BigGetUSValue(int x, int y, int z)
{
  if (!usimdata[z])
    return(vmnullvalue);
  return(usimdata[z][x + ((size_t)y * (size_t)vmdataxsize[z])]);
}

static int cache_GetUSFlipped(int x, int y, int z)
{
  if (!usimdata[y])
    return(vmnullvalue);
  return(usimdata[y][x + ((zsizeFac - z) * vmdataxsize[y])]);
}

static int cache_BigGetUSFlipped(int x, int y, int z)
{
  if (!usimdata[y])
    return(vmnullvalue);
  return(usimdata[y][x + ((size_t)(zsizeFac - z) * (size_t)vmdataxsize[y])]);
}

static int fake_GetValue(int x, int y, int z)
{
  return(0);
}

// Macro for tile cahce access
#define TILECACHE_VALUE(data) \
  int xtile, ytile, xInTile, yInTile, index; \
  xtile = (x + fastTileXoffset) / fastTileXdelta; \
  B3DCLAMP(xtile, 0, numXfastTiles - 1); \
  ytile = (y + fastTileYoffset) / fastTileYdelta; \
  B3DCLAMP(ytile, 0, numYfastTiles - 1); \
  index = xtile + (ytile + z * numYfastTiles) * numXfastTiles; \
  if (!data[index]) \
    return(vmnullvalue); \
  xInTile = xtile ? ((x + fastTileXoffset) - xtile * fastTileXdelta) : x; \
  yInTile = ytile ? ((y + fastTileYoffset) - ytile * fastTileYdelta) : y; \
  return(data[index][xInTile + yInTile * vmdataxsize[index]]);

static int tilecache_GetValue(int x, int y, int z)
{
  TILECACHE_VALUE(imdata);
}

static int tilecache_GetUSValue(int x, int y, int z)
{
  TILECACHE_VALUE(usimdata);
}

// Common function to get the arrays big enough
static int setupFastArrays(int size, int doXsize)
{
  /* If array(s) are not big enough, get new ones */
  if (imdataMax < size) {
    if (imdataMax) {
      free(imdata);
      if (doXsize)
        free(vmdataxsize);
    }
    imdata = B3DMALLOC(unsigned char *, size);
    if (!imdata) {
      imdataMax = 0;
      return 1;
    }
    if (doXsize) {
      vmdataxsize = B3DMALLOC(int, size);
      if (!vmdataxsize) {
        free(imdata);
        imdataMax = 0;
        return 1;
      }
    }
    imdataMax = size;
  }
  usimdata = (b3dUInt16 **)imdata;
  return 0;
}

/* To set up the tables and set the appropriate function */
int ivwSetupFastAccess(ImodView *vi, unsigned char ***outImdata,
                       int inNullvalue, int *cacheSum, int time)
{
  int size = vi->zsize;
  int bigGets;
  int i, iz;

  // Time is an optional argument with default of -1 for current time
  if (time < 0)
    time = vi->curTime;

  *cacheSum = 0;
  bigGets = ((double)vi->xsize) * vi->ysize > 2.e9 ? 1 : 0;
  rgbChan = 0;

  if ((!vi->vmSize || vi->fullCacheFlipped) && vi->li->axis == 2) {
    size = vi->ysize;
    bigGets = ((double)vi->xsize) * vi->zsize > 2.e9 ? 1 : 0;
  }

  if (setupFastArrays(size, vi->vmSize))
    return 1;

  if (vi->fakeImage) {
    ivwFastGetValue = fake_GetValue;

  } else if (vi->vmSize) {

    /* Cached data: fill up pointers that exist
       The cache section is the inverse of an internal section number, so take the
       inverse when looking up the index for a flipped cache */
    for (iz = 0; iz < size; iz++) {

      if (vi->li->axis == 2 && !vi->fullCacheFlipped)
        i = vi->cacheIndex[(vi->zsize -1 - iz) * vi->vmTdim + time - vi->vmTbase];
      else
        i = vi->cacheIndex[iz * vi->vmTdim + time - vi->vmTbase];
      if (i < 0) {
        imdata[iz] = NULL;
      } else {
        imdata[iz] = vi->vmCache[i].sec->data.b;
        vmdataxsize[iz] = vi->vmCache[i].sec->xsize;
        *cacheSum += iz;
      }
    }
    
    vmnullvalue = inNullvalue;
    zsizeFac = vi->zsize - 1;
    if (vi->ushortStore) {
      if (vi->fullCacheFlipped)
        ivwFastGetValue = bigGets ? cache_BigGetUSFlipped : cache_GetUSFlipped;
      else
        ivwFastGetValue = bigGets ? cache_BigGetUSValue : cache_GetUSValue;
    } else if (vi->rgbStore) {
      if (vi->fullCacheFlipped)
        ivwFastGetValue = bigGets ? cache_BigChanFlipped : cache_ChanFlipped;
      else
        ivwFastGetValue = bigGets ? cache_BigChanValue : cache_ChanValue;
    } else {
      if (vi->fullCacheFlipped)
        ivwFastGetValue = bigGets ? cache_BigGetFlipped : cache_GetFlipped;
      else
        ivwFastGetValue = bigGets ? cache_BigGetValue : cache_GetValue;
    }

  } else {
    /* for loaded data, get pointers from idata */

    for (i = 0; i < size; i++)
      imdata[i] = vi->idata[i];
    imdataxsize = vi->xsize;
    if (bigGets)
      xzsizeBigFac = (size_t)(vi->zsize - 1) * (size_t)imdataxsize;
    else
      xzsizeFac = (vi->zsize - 1) * imdataxsize;
    if (vi->ushortStore) {
      if (vi->li->axis == 3)
        ivwFastGetValue = bigGets ? idata_BigGetUSValue : idata_GetUSValue;
      else
        ivwFastGetValue = bigGets ? flipped_BigGetUSValue : flipped_GetUSValue;
    } else if (vi->rgbStore) {
      if (vi->li->axis == 3)
        ivwFastGetValue = bigGets ? idata_BigChanValue : idata_ChanValue;
      else
        ivwFastGetValue = bigGets ? flipped_BigChanValue : flipped_ChanValue;
    } else {
      if (vi->li->axis == 3)
        ivwFastGetValue = bigGets ? idata_BigGetValue : idata_GetValue;
      else
        ivwFastGetValue = bigGets ? flipped_BigGetValue : flipped_GetValue;
    }
  }
  *outImdata = imdata;
  return 0;
}

void ivwSetRGBChannel(int value)
{
  rgbChan = B3DMAX(0, B3DMIN(2, value));
}

/*
 * To set up the tables and set the appropriate function when doing tile caches
 */
int ivwSetupFastTileAccess(ImodView *vi, int cacheInd, int inNullvalue, int &cacheSum)
{
  int nz;
  if (vi->pyrCache->getCacheTileNumbers(cacheInd, numXfastTiles, numYfastTiles, nz))
    return 1;
  if (setupFastArrays(numXfastTiles * numYfastTiles * nz, 1))
    return 1;
  vi->pyrCache->setupFastAccess(cacheInd, imdata, vmdataxsize, cacheSum, fastTileXdelta,
                                fastTileYdelta, fastTileXoffset, fastTileYoffset);
  if (vi->ushortStore)
    ivwFastGetValue = tilecache_GetUSValue;
  else
    ivwFastGetValue = tilecache_GetValue;
  vmnullvalue = inNullvalue;
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
 *  Sets the image Cache so that it contains no data if time < 0, or clears
 * data from the cache for the given time only
 */
void ivwFlushCache(ImodView *vi, int time)
{
  int i, t, tst, tnd;
  int zsize = vi->li->zmax - vi->li->zmin + 1;
  if (vi->li->axis == 2) 
    zsize = vi->li->ymax - vi->li->ymin + 1;

  for (i = 0; i < vi->vmSize; i++) {
    if (time < 0 || vi->vmCache[i].ct == time) {
      vi->vmCache[i].cz = -1;
      vi->vmCache[i].ct = 0;
      vi->vmCache[i].used = -1;
    }
  }
  if (time < 0) {
    vi->vmCount = 0;
    tst = vi->numTimes ? 1 : 0;
    tnd = vi->numTimes ? vi->numTimes : 0;
  } else {
    tst = time;
    tnd = time;
  }

  for (t = tst; t <= tnd; t++) 
    for (i = 0; i < zsize; i++)
      vi->cacheIndex[i * vi->vmTdim + t - vi->vmTbase] = -1;
}

/* Initialize the cache to number of slices in vmSize */
int ivwInitCache(ImodView *vi)
{
  int i;
  int xsize = vi->li->xmax - vi->li->xmin + 1;
  int ysize = vi->li->ymax - vi->li->ymin + 1;
  int zsize = vi->li->zmax - vi->li->zmin + 1;

  vi->vmTdim = vi->numTimes ? vi->numTimes : 1;
  vi->vmTbase = vi->numTimes ? 1 : 0;

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
  for (i = 0; i < vi->vmSize; i++) {
    vi->vmCache[i].sec = sliceCreate(xsize, ysize, vi->rawImageStore);
 
    /*  imodPrintStderr("cache %d : %d x %d\n", i, xsize, ysize); */
 
    if (!vi->vmCache[i].sec) {
      ivwFreeCache(vi);
      return(10);
    }
  }
  ivwFlushCache(vi, -1);
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

  // 3/8/11: This was the size of the data in the file!  It needs to be loaded data
  int pixSize = ivwGetPixelBytes(vi->rawImageStore);
  int i;
  char *envLimit;
  double memLimit = 0.;

  if (!xsize || !ysize || !zsize)
    return(-1);

  /* If negative size, it is megabytes, convert to sections */
  if (vi->vmSize < 0) {
    if (vi->pyrCache)
      vi->pyrCache->setVMpixels(-1000000. * vi->vmSize / pixSize);
    vi->vmSize = (int)((-1000000. * vi->vmSize) / (((float)xsize * ysize) * pixSize));
    if (!vi->vmSize)
      vi->vmSize = 1;
  } else if (vi->pyrCache) {
    if (vi->vmSize) {
      vi->pyrCache->setVMpixels((vi->vmSize * (double)xsize) * ysize);
    } else {
      envLimit = getenv("TILECACHE_LIMIT_MB");
      if (envLimit)
        memLimit = atof(envLimit);
      if (!memLimit)
        memLimit = DEFAULT_TILE_CACHE_LIMIT;
      vi->pyrCache->setVMpixels(1000000. * memLimit / pixSize);
    }
  }

  if (vi->li->plist) {
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
    dzsize *= (vi->numTimes > 0) ? vi->numTimes : 1;

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
  int nx, newy, newz;
  int i, t, cacheFull;
  unsigned int nyz;
  int oldYmouse, oldZmouse;

  /* DNM 12/10/02: if loading image, flip the axis but defer until done */
  if (vi->doingInitialLoad > 0) {
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
  if ((!vi->flippable || vi->li->plist) && !cacheFull) {
    wprint("\aSorry, these image data can't be flipped unless they "
           "are completely loaded into memory.\n");
    return(-1);
  }

  oldYmouse = (int)(vi->ymouse + 0.5f);
  oldZmouse = (int)(vi->zmouse + 0.5f);

  wprint("Flipping image data.\n");

  /* DNM: restore data before flipping, as well as resetting when done */
  iprocRethink(vi);
  nx = vi->xsize;
  newy = vi->zsize;
  newz = vi->ysize;
  nyz = newy * newz;

  vi->li->axis =  (vi->li->axis == 2) ? 3 : 2;

  if (vi->vmSize && cacheFull) {
    vi->fullCacheFlipped = 1 - vi->fullCacheFlipped;

  } else if (vi->vmSize) {

    /* If Image data is cached from disk */
    /* tell images to flipaxis */
          
    if ((vi->numTimes) && (vi->imageList)) {
      for (t = 0; t < vi->numTimes; t++) {
        vi->imageList[t].axis = vi->li->axis;
      }
    }
    if (vi->image) {
      vi->image->axis = vi->li->axis;
               
    }

    ivwFreeCache(vi);
    /* DNM: if the cache size equalled old # of Z planes, set it to new
       number of planes, including ones for each file
       Otherwise, set it to occupy same amount of memory, rounding up
       to avoid erosion on repeated flips */
    t = vi->numTimes > 0 ? vi->numTimes : 1;
    if (vi->vmSize == t * vi->zsize)
      vi->vmSize = t * newz;
    else {
      vi->vmSize = (vi->vmSize * vi->ysize + newy / 2) / newy;
      if (!vi->vmSize)
        vi->vmSize = 1;
    }
    ivwInitCache(vi);
  }

  //vi->xsize = nx;
  vi->ysize = newy;
  vi->zsize = newz;
  vi->xysize = (size_t)vi->xsize * (size_t)vi->ysize;
  //vi->xmouse = 0;
  vi->ymouse = 0;
  vi->zmouse = 0;

  nx = vi->yUnbinSize;
  vi->yUnbinSize = vi->zUnbinSize;
  vi->zUnbinSize = nx;

  // Rotate here instead of flipping
  if (!vi->doingInitialLoad)
    ivwFlipModel(vi, true);
  iprocRethink(vi);
  autox_newsize(vi);
  imod_info_float_clear(-1, -1);

  if (vi->li->axis == 2) {
    vi->ymouse = oldZmouse;
    vi->zmouse = newz - 1 - oldYmouse;
  } else {
    vi->ymouse = newy - 1 - oldZmouse;
    vi->zmouse = oldYmouse;
  }

  /* Keep it in bounds */
  if (vi->zmouse > newz - 1)
    vi->zmouse = newz - 1;

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
      for (i = 0; i < xsize; i++) {
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
  if (time) {
    *time = vi->curTime;
  }
  return(vi->numTimes);
}

/* Set the current time index.  Time index ranges from 1 to maxtime 
 * 0 is reserved for no time - meaning only one image stack is loaded. 
 */
void ivwSetTime(ImodView *vi, int time)
{
    
  if (!vi->numTimes) {
    vi->curTime = 0;
    if (vi->imod)
      vi->imod->ctime = 0;
    return;
  }
     
  /* DNM 6/17/01: Don't do this */
  /* inputSetModelTime(vi, time); */  /* set model point to a good value. */

  if (vi->curTime > 0 && !vi->fakeImage)
    iiClose(&vi->imageList[vi->curTime-1]);

  vi->curTime = time;
  if (vi->curTime > vi->numTimes)
    vi->curTime = vi->numTimes;
  if (vi->curTime <= 0)
    vi->curTime = 1;

  if (!vi->fakeImage) {
    vi->hdr = vi->image = &vi->imageList[vi->curTime-1];
    if (vi->ushortStore)
      ImodInfoWidget->setLHSliders(vi->rangeLow, vi->rangeHigh, vi->image->smin, 
                                   vi->image->smax, vi-> image->type == IITYPE_FLOAT);
      
    // ivwSetScale(vi);

    ivwReopen(vi->image);
   }
  /* DNM: update scale window */
  imodImageScaleUpdate(vi);
  if (vi->imod)
    vi->imod->ctime = vi->curTime;
}

const char *ivwGetTimeIndexLabel(ImodView *inImodView, int inIndex)
{
  if (!inImodView) return "";
  if (inIndex < 1) return "";
  if (inIndex > inImodView->numTimes) return "";
  if (inImodView->fakeImage) return "";
  return(inImodView->imageList[inIndex-1].description);
}

const char *ivwGetTimeLabel(ImodView *inImodView)
{
  return (inImodView->image->description);
}

int  ivwGetMaxTime(ImodView *inImodView)
{
  return(inImodView->numTimes);
}

// Set the time of a new contour - only if there are multiple times and the
// object flags indicate time is to be stored
void ivwSetNewContourTime(ImodView *vw, Iobj *obj, Icont *cont)
{
  if (vw->numTimes && obj && cont && iobjTime(obj->flags)) {
    cont->time = vw->curTime;
  }
}

/* Set the global location in 3D space for all windows to integer values
 */
void ivwSetLocation(ImodView *vi, int x, int y, int z)
{
  vi->xmouse = x;
  vi->ymouse = y;
  vi->zmouse = z;
  ivwBindMouse(vi);
  imodDraw(vi, IMOD_DRAW_ALL);
}

/* Set the global point to floating point X/Y values */
void ivwSetLocationPoint(ImodView *vi, Ipoint *pnt)
{
  vi->xmouse = pnt->x;
  vi->ymouse = pnt->y;
  vi->zmouse = B3DNINT(pnt->z);
  ivwBindMouse(vi);
  imodDraw(vi, IMOD_DRAW_ALL);
}

/* sets the current graphics position to the same as the current model
 * position. 
 *
 * Sets current time also.
 */

int imod_setxyzmouse()
{
  return(imod_redraw(App->cvi));
}

int imod_redraw(ImodView *vw)
{
  Imod *imod = ivwGetModel(vw);
  Iobj  *obj = NULL;
  Icont *cont = NULL;
  int   index;

  if ( (index = imod->cindex.point) < 0) {
    imodDraw(vw, IMOD_DRAW_MOD);
    return(1);
  }

  cont = imodContourGet(imod);
  if (cont == NULL) {
    imodDraw(vw, IMOD_DRAW_MOD);
    return(1);
  }
  if ((cont->pts == NULL) || (cont->psize <= index)) {
    imodDraw(vw, IMOD_DRAW_MOD);
    return(1);
  }

  obj = imodObjectGet(imod);
  if (iobjFlagTime(obj))
    ivwSetTime(vw, cont->time);

  ivwSetLocationPoint(vw, &(cont->pts[index]));

  return(0);
}


void ivwSetMovieModelMode(ImodView *vi, int mode)
{
  imod_set_mmode(mode);
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
static float ivwReadBinnedPoint(ImodView *vi, ImodImageFile *image, int cx, int cy,
                                int cz)
{
  double sum = 0.;
  int nsum = 0;
  int ix, iy, iz, ubx, uby, ubz, mirx, miry;
  if (vi->xybin * vi->zbin == 1 && !vi->image->mirrorFFT)
    return (iiReadPoint(image, cx, cy, cz));
  for (iz = 0; iz < vi->zbin; iz++) {
    ubz = cz * vi->zbin + iz;
    if (ubz < image->nz) {
      for (iy = 0; iy < vi->xybin; iy++) {
        uby = cy * vi->xybin + iy;
        if (uby < image->ny) {
          for (ix = 0; ix < vi->xybin; ix++) {
            ubx = cx * vi->xybin + ix;
            if (vi->image->mirrorFFT && ubx < vi->image->nx) {
                mrcMirrorSource(vi->image->nx, vi->image->ny, ubx, uby, &mirx,
                                &miry);
                sum += iiReadPoint(image, mirx, miry, ubz);
                nsum++;
            } else if (ubx < image->nx) {
              sum += iiReadPoint(image, ubx, uby, ubz);
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
  int fx, fy, fz, llx, lly, llz, xpad, ypad, zpad;

  if (!vi->image || cz < 0 || cz >= vi->zsize)
    return 0.0f;
  if (vi->noReadableImage) {
    if (!vi->rawImageStore)
      return ivwGetValue(vi, cx, cy, cz);
    return 0.0f;
  }
  if (vi->li->axis == 2)
    cz = vi->zsize - 1 - cz;

  // For a tile cache, translate the coordinates and read it
  if (vi->pyrCache) {
    if (!vi->pyrCache->getBaseFileCoords(cx, cy, cz, fx, fy, fz))
    return(ivwReadBinnedPoint(vi, vi->image, fx, fy, fz));
      return 0.f;
  }

  if (vi->li) {

    /* get to index values in file from screen index values */
    if (ivwGetImagePadding(vi, cy, cz, vi->curTime, llx, xpad, fx, lly, ypad, fy, llz, 
                           zpad, fz) < 0)
        return 0.;

    /* DNM 7/13/04: changed to apply ymin, zmin after switching y and z */
    fx = cx + llx - xpad;
    if (vi->li->axis == 3) {
      fy = cy + lly - ypad;
      fz = cz + llz - zpad;
    } else {
      fy = cz + lly - zpad;
      fz = cy + llz - ypad;
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
      }
      fz = 0;
    }

    if (vi->li->plist) {

      /* montaged: find piece with coordinates in it and get data 
         there */
      int px, py, pz;
      int i, mi = vi->li->plist;
      px = fx; py = fy; pz = fz;
      for (i = 0; i < mi; i++) {
        if (pz == vi->li->pcoords[(i*3)+2]) {
                        
          if ((px >= vi->li->pcoords[(i*3)]) &&
              (px < (vi->li->pcoords[(i*3)]+vi->hdr->nx/vi->xybin)) &&
              (py >= vi->li->pcoords[(i*3)+1]) &&
              (py < (vi->li->pcoords[(i*3)+1]+vi->hdr->ny/vi->xybin)
               )) {
            fz = i;
            fx = px - vi->li->pcoords[(i*3)];
            fy = py - vi->li->pcoords[(i*3)+1];
            return(ivwReadBinnedPoint(vi, vi->image, fx, fy, fz));
          }
        }
      }
      return(vi->hdr->amean);
    }
    return(ivwReadBinnedPoint(vi, vi->image, fx, fy, fz));
  }
  return(ivwReadBinnedPoint(vi, vi->image, cx, cy, cz));
}


/* Routine to copy a portion of one buffer into another 
   tskip = to_xsize - xcpy, fskip = from_xsize - xcpy*/
void memreccpy
(unsigned char *tb,             /* copy data to buffer */
 unsigned char *fb,             /* copy data from buffer */
 int xcpy, int ycpy, int psize, /* number of pixels X&Y and byte size of data to copy. */
 int tskip, int tox, int toy,   /* to buffer initial offsets, skip between lines. */
 int fskip, int fox, int foy)   /* from buffer initial offsets, skip between lines. */
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
  for (y = 0; y < my; y++) {
    memcpy(tb, fb, xcpy);
    tb+=tskip;
    fb+=fskip;
  }
}

/* Copy a portion of one buffer into another described by line pointers */
void memLineCpy
(unsigned char **tlines,        /* line pointers to copy data to */
 unsigned char *fb,             /* copy data from buffer */
 int xcpy, int ycpy, int psize, /* amount / byte size of data to copy */
 int tox, int toy,              /* to buffer offsets */
 int fxsize, int fox, int foy)  /* from buffer X size and offsets */
{
  unsigned char *tb;
  int y;

  /* initialize from buffer pointer */
  fb += (fox + fxsize * foy) * psize;
  xcpy *= psize;
  for (y = 0; y < ycpy; y++) {
    tb = tlines[y + toy] + tox * psize;
    memcpy(tb, fb, xcpy);
    fb += fxsize * psize;
  }
}

/* Copies a loaded Z plane to a byte buffer */
int ivwCopyImageToByteBuffer(ImodView *vi, unsigned char **image, unsigned char *buf)
{
  unsigned char *bmap = NULL;
  b3dUInt16 **usimage = (b3dUInt16 **)image;
  int i, j;
  if (vi->ushortStore) {
    bmap = ivwUShortInRangeToByteMap(vi);
    if (!bmap)
      return 1;
  }
  for (j = 0; j < vi->ysize; j++) {
    if (bmap)
      for (i = 0; i < vi->xsize; i++)
        *buf++ = bmap[usimage[j][i]];
    else
      for (i = 0; i < vi->xsize; i++)
        *buf++ = image[j][i];
  }
  B3DFREE(bmap);
  return 0;
}


/* DNM: scan through all contours and set wild flag if Z is not the 
   same throughout.  But 4/20/06: base it on nearest integer. */

void ivwCheckWildFlag(Imod *imod)
{
  int ob, co, pt, iz;
  Iobj *obj;
  Icont *cont;
     
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &(imod->obj[ob]);
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      cont->flags &= ~ICONT_WILD;
      if (cont->pts) {
        iz =(int)floor(cont->pts->z + 0.5);
        for (pt = 1; pt < cont->psize; pt++) {
          if (iz != (int)floor(cont->pts[pt].z + 0.5)) {
            cont->flags |= ICONT_WILD;
            break;
          }
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
     
  if (!ref || !vi->image) 
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
  if (vi->image->mirrorFFT)
    ref->ctrans.x += xscale * vi->image->nx / 2.;

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

  // Unconditionally set the maxes in the model; this works for fakeimage because
  // size was set from model right away
  imod->xmax = vi->xsize;
  imod->ymax = vi->ysize;
  imod->zmax = vi->zsize;

  if (vi->fakeImage)
    return;

  // If there is not an existing refImage, get a new one
  if (!imod->refImage) {
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

/* Flips or rotates model IF it does not match current flip state of image */

void ivwFlipModel(ImodView *vi, bool rotate /* = false */)
{
  /* flip model y and z and manage the flag state */
  Imod  *imod = vi->imod;
  int flag = rotate ? IMODF_ROT90X : IMODF_FLIPYZ;
  int curState = (imod->flags & flag) ? 1 : 0;
  //imodPrintStderr("axis %d  curstate %d  flag %d\n", vi->li->axis, curState, 
  //  rotate?1:0);
  if (vi->li->axis == 2 && curState)
    return;

  if ((vi->li->axis == 3 || vi->li->axis == 0) && !curState)
    return;

  if (curState)
    imod->flags &= ~flag;
  else
    imod->flags |= flag;

  if (rotate) {
    //imodPrintStderr("curstate %d ymax %d zmax %d\n", curState, imod->ymax, imod->zmax);
    imodRot90X(imod, curState);
  } else
    imodFlipYZ(imod);
}


/* Transforms model so that it matches image coordinate system.
 */
void ivwTransModel(ImodView *vi)
{
  IrefImage *iref;
  Imod  *imod = vi->imod;
  Ipoint binScale;

  /* If model doesn't have a reference coordinate system from an image, then use this
   * image's coordinate system and return, unless there is binning;
   */
  if (!ImodTrans || !vi->imod->refImage) {

    // When loading with no trans, if the model is flipped, need to invert it to restore 
    // handedness and mark it as rotated if image is, to avoid further operations
    if (!ImodTrans) {
      utilExchangeFlipRotation(vi->imod, FLIP_TO_ROTATION);
      setOrClearFlags(&vi->imod->flags, IMODF_ROT90X, vi->li->axis == 2 ? 1 : 0);
    } else {

      // Otherwise is needs to be in the right flip state before setting the trans data
      ivwFlipModel(vi);
    }
    ivwSetModelTrans(vi);
    if (!ImodTrans || !vi->imod->refImage || vi->xybin * vi->zbin == 1)
      return;
  }

  /* Try and get the coordinate system that we will transform the model to match.
   * Set the old members if iref to the model's current members
   */
  iref = ivwGetImageRef(vi);
  if (iref) {

    iref->orot   = vi->imod->refImage->crot;
    iref->otrans = vi->imod->refImage->ctrans;
    iref->oscale = vi->imod->refImage->cscale;
    binScale.x = binScale.y = vi->xybin;
    binScale.z = vi->zbin;
    
    /* transform model to new coords (it will be unflipped if necessary) */
    imodTransFromRefImage(imod, iref, binScale);
  }

  ivwFlipModel(vi);
  ivwSetModelTrans(vi);
  B3DFREE(iref);
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

  if (isifd) {
    isifd = 0;

    while((imodFgetline(fin, buf, 127) > 0)) {
      if (!strncmp("VERSION", buf, 7)) {
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
int ivwLoadIMODifd(ImodView *vi, QStringList &plFileNames, bool &anyHavePieceList,
                   bool &anyImageFail)
{
  Ilist *ilist = ilistNew(sizeof(ImodImageFile), 32);
  ImodImageFile *image;
  char line[IFDLINE_SIZE + 1];
  int i;
  IloadInfo *li;
  int xsize = 0, ysize = 0, zsize;
  float smin = 0., smax = 0.;
  int version = 0;
  int needVersion = 1;
  char *imgdir = NULL;
  QDir *curdir = new QDir();
  QString qname;

  anyHavePieceList = false;
  anyImageFail = false;
  rewind(vi->fp);
  imodFgetline(vi->fp, line, IFDLINE_SIZE);

  while((imodFgetline(vi->fp, line, IFDLINE_SIZE) > 0)) {

    //wprint("%s\n\r",line);
    //imod_info_input();

    /* clear the return from the line. */
    for (i = 0; line[i]; i++)
      if (line[i] == '\n' || line[i] == '\r') {
        line[i] = 0x00;
        break;
      }

    if (line[0] == '#') {
      needVersion = 2;
      continue;
    }

    if (!strncmp("VERSION", line, 7)) {
      version = atoi(&line[8]);
      continue;
    }

    /* supply size in case first file is not found. */
    if (!strncmp("SIZE", line, 4)) {
      sscanf(line, "SIZE %d%*c%d%*c%d\n", &xsize, &ysize, &zsize);
      continue;
    }

    /* define a root pathname for all image files. */
    if (!strncmp("IMGDIR", line, 6)) {
      if (imgdir) 
        free(imgdir);
      imgdir = strdup(LATIN1(curdir->cleanPath(QString(&line[7]).
                                                  trimmed())));
      continue;
    }

    // Define a scale for images that follow; can be defined again
    if (!strncmp("SCALE", line, 4)) {
      sscanf(line, "SCALE %f%*c%f\n", &smin, &smax);
      needVersion = 2;
      continue;
    }

    if (!strncmp("PYRAMID", line, 7)) {
      vi->imagePyramid = 1;
      needVersion = 2;
      continue;
    }

    /* DNM: XYZ label now supported; require one image file */
    if (!strncmp("XYZ", line, 3)) {

      li = vi->li;            
      if (ilist->size == 1)
        image = (ImodImageFile *)ilistItem(ilist, ilist->size - 1);
      else {
        imodError(NULL, "3DMOD Error: " 
                "Image list file must specify one image file"
                " before the XYZ option.\n");
        exit(3);
      }
      iiPlistLoadF(vi->fp, li, image->nx, image->ny, image->nz);

      /* DNM 1/2/04: move adjusting of loading coordinates to fix_li call,
         move that call into list processing, eliminate setting cache size,
         since it will happen later, and break instead of continuing */
      vi->flippable = 0;
      break;
    }

    // TIME label replaces the filename in the description string
    if (!strncmp("TIME", line, 4)) {
      if (ilist->size) {
        image = (ImodImageFile *)ilistItem(ilist, ilist->size - 1);
        if (image->description)
          free(image->description);
        image->description = strdup(&line[5]);
      }
      continue;
    }

    // Origin is applied to the current image file
    if (!strncmp("ORIGIN", line, 6)) {
      if (ilist->size) {
        image = (ImodImageFile *)ilistItem(ilist, ilist->size - 1);
        sscanf(line, "ORIGIN %f%*c%f%*c%f\n", &image->xtrans, &image->ytrans, 
               &image->ztrans);
      }
      needVersion = 2;
      continue;
    }

    if (!strncmp("IMAGE", line, 5)) {
      /* Load image file */
      int pathlen = strlen(&line[6]);
      char *filename = NULL;
               
      if (imgdir) {
        pathlen += strlen(imgdir);
        filename = (char *)malloc(pathlen + 4);
        strcpy(filename, imgdir);
        strcat(filename, "/");
        strcat(filename, LATIN1(curdir->cleanPath(QString(&line[6]).trimmed())));
      }else{
        filename = strdup(LATIN1(curdir->cleanPath(QString(&line[6]).trimmed())));
      }

      errno = 0;
      image = iiOpen(LATIN1(QDir::convertSeparators(QString(filename))), "rb");
      if (!image) {
        if (!xsize || !ysize) {
          imodError(NULL, "3DMOD Error: " 
                  "couldn't open %s, first file in image list,"
                  "\n and no SIZE specified before this.\n",
                  filename);
          exit(3);
        }
        wprint("Warning: couldn't open %s\n\r", filename);
        imodError(stdout, "Warning: couldn't open %s\n%s%s", filename, 
                  errno ? "System error: " : "", errno ? strerror(errno): "");
        image = iiNew();
        image->nx = xsize;
        image->ny = ysize;
        image->nz = zsize;
        image->filename = strdup(LATIN1(QDir::convertSeparators(QString(filename))));
        anyImageFail = true;
      }

      /* DNM: set up scaling for this image, leave last file in hdr/image */
      if (image->file == IIFILE_RAW && !image->amin && !image->amax && smin >= smax)
        iiRawScan(image);
      if (smin < smax) {
        image->smin = smin;
        image->smax = smax;
      }
        
      vi->hdr = vi->image = image;
      iiClose(image);
      if (image->hasPieceCoords)
        anyHavePieceList = true;

      /* DNM: Make filename with directory stripped be the default descriptor */
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

      /* DNM: set time and increment time counter here, not with the TIME label */
      image->time = vi->numTimes;
      vi->numTimes++;

      if (filename)
        free(filename);
      continue;
    }

    if (!strncmp("PIECEFILE", line, 9)) {
      for (i = plFileNames.size(); i < ilist->size - 1; i++)
        plFileNames << "NONE";
      qname = QString(&line[10]).trimmed();
      if (imgdir)
        qname = QString(imgdir) + "/" + qname;
      plFileNames << QDir::convertSeparators(curdir->cleanPath(qname));
      needVersion = 2;
      continue;
    }

    imodError(NULL, "3dmod warning: "
            "Unknown image list option (%s)\n", line);

  }
  rewind(vi->fp);
  /* end of while (getline) */

  /* save this in iv although it is an Ilist so ImageFile */
  vi->imageList = (ImodImageFile *)ilist;
  if (version < needVersion) {
    imodError(NULL, "3DMOD Error: "
              "The image list file must specify version %d or higher\n", needVersion);
    exit(3);
  }

  if (imgdir)
    free(imgdir);
  delete curdir;

  return(0);
}

// Loads a piece list file listed in an IFD file, which may be located elsewhere
int ivwLoadIFDpieceList(const char *plName, IloadInfo *li, int nx, int ny, int nz)
{
  int retval;
  if (!Imod_IFDpath.isEmpty())
  QDir::setCurrent(Imod_IFDpath);
  retval = (iiPlistLoad(plName, li, nx, ny, nz));
  if (!Imod_IFDpath.isEmpty())
    QDir::setCurrent(Imod_cwdpath);
  return retval;
}

/* Take a list of multiple files from the argument list and compose an
   image list */
void ivwMultipleFiles(ImodView *vi, char *argv[], int firstfile, int lastimage,
                      bool &anyHavePieceList)
{
  Ilist *ilist = ilistNew(sizeof(ImodImageFile), 32);
  ImodImageFile *image;
  int pathlen, i;
  char *convarg;
  QDir *curdir = new QDir();
  QString str;
  anyHavePieceList = false;

  for (i = firstfile; i <= lastimage; i++) {
    if (argv[i][0]) {
      convarg = strdup(LATIN1(curdir->cleanPath(QString(argv[i]))));
      image = iiOpen(LATIN1(QDir::convertSeparators(QString(convarg))), "rb");
    } else
      image = iiOpen(argv[i], "rb");
    if (!image) {
      str.sprintf("3DMOD Error: couldn't open image file %s.\n", argv[i]);
      str = QString(b3dGetError()) + str;
      imodError(NULL, LATIN1(str));
      exit(3);
    }

    /* set up scaling for this image, scanning if needed */
    if (image->file == IIFILE_RAW && !image->amin && !image->amax)
      iiRawScan(image);

    /* Anyway, leave last file in vi->hdr/image */
    vi->fp = image->fp;
    vi->hdr = vi->image = image;
    
    image->time = vi->numTimes;
    vi->numTimes++;

    /* Copy filename with directory stripped to the descriptor */
    /* There was strange comment about "Setting the fp keeps it from closing 
       the file", but the file does get closed unless it is stdin */
    if (argv[i][0]) {
      iiClose(image);
      pathlen = strlen(convarg);
      while (( pathlen > 0) && (convarg[pathlen-1] != '/'))
        pathlen--;
      image->description = strdup(&convarg[pathlen]);
      free(convarg);
    } else
      image->description = strdup(argv[i]);
    if (image->hasPieceCoords)
      anyHavePieceList = true;
    ilistAppend(ilist, image);
  }
  delete curdir;

  if (anyHavePieceList && vi->numTimes > 1 && !vi->imagePyramid)
    vi->imagePyramid = -1;

  /* save this in iv so it can be passed in call to ivwSetCacheFrom List */
  vi->imageList = (ImodImageFile *)ilist;
}


/* Load images initially, use for all kinds of data */
int ivwLoadImage(ImodView *vi)
{
  int axisSave;
  int eret;

  if (vi->fakeImage) {

    // Initialize various things for no image; access Model not vi->imod
    vi->xsize = Model->xmax;
    vi->ysize = Model->ymax;
    vi->zsize = Model->zmax;
    vi->xybin = 1;
    vi->zbin = 1;
    vi->xUnbinSize = vi->xsize;
    vi->yUnbinSize = vi->ysize;
    vi->zUnbinSize = vi->zsize;
    if (vi->numTimes > 1)
      ivwSetTime(vi, 1);

    vi->rawImageStore = 0;
    ImodInfoWidget->hideLowHighGrid();
    imod_color_init(App);

    wprint("Image size %d x %d, %d sections.\n", vi->xsize, vi->ysize, vi->zsize);
    best_ivwGetValue = fake_ivwGetValue;

    /* DNM: set the axis flag based on the model flip flag */
    if (vi->li->axis == 2) 
      imodError(NULL, "The -Y flag is ignored when loading a model without an image.\n"
                "Use Edit-Image-Flip to flip the model if desired");
    vi->li->axis = 3;
    if (Model->flags & IMODF_FLIPYZ)
      vi->li->axis = 2;
    ImodPrefs->setInfoGeometry();
    return (initializeFlipAndModel(vi));
  }

  ivwProcessImageList(vi); 

  // Set info window up now that size is known
  ImodPrefs->setInfoGeometry();
  vi->doingInitialLoad = 1;
     
  /* Set up the cache and load it for a variety of conditions */
  if (vi->li->plist || vi->numTimes || vi->multiFileZ > 0 || vi->vmSize || 
      vi->ushortStore || vi->pyrCache) {

    /* DNM: only one mode won't work now; just exit in either case */
    if (vi->hdr->mode == MRC_MODE_COMPLEX_SHORT) {
      imodError(NULL, "3DMOD Error: "
                "Image cache and piece lists do not work with "
                "complex short data.\n");
      exit(3);
    }

    vi->idata = NULL;

    /* print load status */
    wprint("Image size %d x %d, %d sections.\n", vi->xsize, vi->ysize, vi->zsize);
          
    ivwSetCacheSize(vi);

    if (vi->pyrCache) {
      vi->pyrCache->initializeCaches();
      best_ivwGetValue = tiles_ivwGetValue;

    } else {

      /* initialize ordinary cache, make sure axis is set for all data structures 
         Set axis to 3 for first initialization because vmSize has been 
         computed based on unflipped Z dimensions */
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
    }

  } else {

    /* Finally, here is what happens for non-cached data of all kinds */

    /* DNM 9/25/03: it probably no longer matters if data are ever contiguous
       so switch to noncontiguous for anything above 1 GB */
    vi->li->contig = 1; 
    if ((1000000000 / vi->xsize) / vi->ysize < vi->zsize)
      vi->li->contig = 0;
    /*imodPrintStderr("contig = %d  axis = %d  xysize %d  % dzsize %d \n",
      vi->li->contig, vi->li->axis, xysize, 4200000000 / xysize, zsize); */

    best_ivwGetValue = idata_ivwGetValue;
    vi->idata = imod_io_image_load(vi);
    if (!vi->idata) {
      /* Let caller do error message */
      return(-1);
    }
  }

  if (App->depth == 8)
    ivwScale(vi);
     
  return (initializeFlipAndModel(vi));
}

/* Process an image list and determine sizes and types of files */
static int ivwProcessImageList(ImodView *vi)
{
  ImodImageFile *image;
  Ilist *ilist = (Ilist *)vi->imageList;
  MrcHeader *header;
  FILE *fp;
  int xsize, ysize, zsize, i, midy, midz;
  float naysum, zratio, mratio, smin, smax;
  int rgbs = 0, cmaps = 0, allByte = 1, allCanReadInt = 1;

  if (!ilist->size)
    return -1;

  /* First get minimum x, y, z sizes of all the files and count up rgbs */
  for (i = 0; i < ilist->size; i++) {
    image = (ImodImageFile *)ilistItem(ilist, i);
    if (i) {
      smin = B3DMIN(smin, image->smin);
      smax = B3DMAX(smax, image->smax);
    } else {
      smin = image->smin;
      smax = image->smax;
    }

    if (image->format != IIFORMAT_LUMINANCE || image->type != IITYPE_UBYTE) 
      allByte = 0;
    if (!image->readSectionUShort)
      allCanReadInt = 0;

    // See if mirroring of an FFT is needed: Not forbidden by option 
    // (MRC complex float odd size not reliable, eliminated 7/16/13)
    // Set flags and increase the nx
    if (!i && (image->file == IIFILE_MRC || image->file == IIFILE_RAW) && 
        image->format == IIFORMAT_COMPLEX && image->type == IITYPE_FLOAT && 
        vi->li->mirrorFFT >= 0) {
      image->mirrorFFT = 1;
      if (image->file == IIFILE_MRC) {

        // Analyze real MRC file for whether the hot pixel in the middle of
        // of the file is much higher than at the bottom, if so don't mirror
        ivwReopen(image);
        fp = image->fp;
        header = (MrcHeader *)image->header;
        midy = image->ny / 2;
        midz = image->nz / 2;
        naysum = iiReadPoint(image, 1, midy, 0) + 
          iiReadPoint(image, 0, midy - 1, 0) + 
          iiReadPoint(image, 1, midy + 1, 0);
        zratio = 0.;
        if (naysum > 0.)
          zratio = iiReadPoint(image, 0, midy, 0) / naysum;
        naysum = iiReadPoint(image, 1, midy, midz) + 
          iiReadPoint(image, 0, midy - 1, midz) + 
          iiReadPoint(image, 1, midy + 1, midz);
        mratio = 0.;
        if (naysum  > 0.)
          mratio = iiReadPoint(image, 0, midy, midz) / naysum;
        if (zratio && mratio && mratio > 10. * zratio)
          image->mirrorFFT = 0;
        iiClose(image);
      }
      if (image->mirrorFFT) {
        vi->li->mirrorFFT = 1;
        image->nx = (image->nx - 1) * 2;
      }
    }

    // Keep track of largest image size
    if (!i || image->nx > xsize)
      xsize = image->nx;
    if (!i || image->ny > ysize)
      ysize = image->ny;
    if (!i || image->nz > zsize)
      zsize = image->nz;

    /* Add to count if RGB or not, to see if all the same type.  Similarly
       for colormap images */
    if (image->format == IIFORMAT_RGB && 
        !((image->file == IIFILE_MRC  || image->file == IIFILE_RAW) && 
          vi->grayRGBs))
      rgbs++;

    if (image->format == IIFORMAT_COLORMAP)
      cmaps++;
    if (vi->stripOrTileCache && (image->format == IIFORMAT_COMPLEX || rgbs || cmaps)) {
      imodError(NULL, "3DMOD Error: You cannot cache tiles or strips with complex,\n"
                "colormap, or RGB data, except when loading RGB as grayscale");
      exit(3);
    }
  }     

  // Cancel integer loading if all files are bytes or there is color loading
  if (allByte || rgbs || cmaps)
    vi->rawImageStore = 0;
  if (vi->rawImageStore == MRC_MODE_USHORT && !App->rgba) {
    imodError(NULL, "3DMOD Error: You can not store data as integers with the "
              "-ci option.\n");
    exit(3);
  }
  if (vi->rawImageStore == MRC_MODE_USHORT && !allCanReadInt) {
    imodError(NULL, "3DMOD Error: -I option cannot be used because some files cannot be"
              " read from as integers.\n");
    exit(3);
  }
  
  // And now the color ramps can be initialized and other flags set
  if (vi->rawImageStore == MRC_MODE_USHORT) {
    App->rgba = 2;
    vi->ushortStore = 1;
    vi->white = 65535;
    vi->li->outmax = 65535;
  } else
    ImodInfoWidget->hideLowHighGrid();
  imod_color_init(App);

  /* Deal with color files */
  if (rgbs || cmaps) {
    if ((rgbs && rgbs < ilist->size) || (cmaps && cmaps < ilist->size)) {
      imodError(NULL, "3DMOD Error: Only %d files out of %d are "
                "%s type and all files must be.\n", rgbs ? rgbs : cmaps, 
                ilist->size, rgbs ? "RGB" : "colormap");
      exit(3);
    }
               
    if (!App->rgba) {
      imodError(NULL, "3DMOD Error: You must not start 3dmod with "
                "the -ci option to display %s files.\n", 
                rgbs ? "RGB" : "colormap");
      exit(3);
    }
        
    /* For RGB, set the flag for storing raw images with the mode, and set 
       rgba to indicate the number of bytes being stored */
    if (rgbs) {
      App->rgba = 3;
      vi->rawImageStore = MRC_MODE_RGB;
      vi->rgbStore = 1;
    } else {
      vi->colormapImage = 1;
      vi->cramp->falsecolor = 2;
      vi->li->axis = 3;
    }
  }

  /* Set the scaling including equal scaling of intensities */
  for (i = 0; i < ilist->size; i++) {
    image = (ImodImageFile *)ilistItem(ilist, i);
    if (vi->equalScaling || (vi->imagePyramid && vi->li->smin == vi->li->smax)) {
      vi->li->smin = smin;
      vi->li->smax = smax;
    }
    iiSetMM(image, vi->li->smin, vi->li->smax, vi->ushortStore ? 65535. : 255.);
  }

  if (!vi->li->plist) {

    /* Deal with non-montage case */
    if (vi->imagePyramid) {
      xsize = vi->xUnbinSize;
      ysize = vi->yUnbinSize;
      zsize = vi->zUnbinSize;

    } else if (ilist->size > 1 && zsize == 1 && vi->multiFileZ >= 0) {

      /* If maximum Z is 1 and multifile treatment in Z is allowed, set zsize
         to number of files, and cancel treatment as times */
      zsize = ilist->size;
      vi->multiFileZ = ilist->size;
      vi->curTime = vi->numTimes = 0;
    } 

    /* Use this to fix the load-in coordinates, then use those to set the
       lower left and upper right coords in each file - except for Z in the
       multifile Z case, which is set to 0 - 0 */
    mrc_fix_li(vi->li, xsize, ysize, zsize);
    ivwCheckBinning(vi, xsize, ysize, zsize);
    if (!vi->imagePyramid) {
      for (i = 0; i < ilist->size; i++) {
        image = (ImodImageFile *)ilistItem(ilist, i);
        image->llx = vi->li->xmin;
        image->lly = vi->li->ymin;
        image->llz = vi->multiFileZ > 0 ? 0 : vi->li->zmin;
        image->urx = vi->li->xmax;
        image->ury = vi->li->ymax;
        image->urz = vi->multiFileZ > 0 ? 0 : vi->li->zmax;
               
        // If not an MRC file, or if multifile in Z, set to no flipping unless cache full
        if ((image->file != IIFILE_MRC  && image->file != IIFILE_RAW) || 
            vi->multiFileZ > 0)
          vi->flippable = 0;
      }
    }     
  } else {

    /* For montage, do the fix_li and see if it is rgb */
    mrc_fix_li(vi->li, 0, 0, 0);
    image = (ImodImageFile *)ilistItem(ilist, 0);
    ivwCheckBinning(vi, image->nx, image->ny, image->nz);
  }

  // 3/17/11: This fixes bugs from various places not testing <= 0 vs > 0
  vi->multiFileZ = B3DMAX(0, vi->multiFileZ);

  if (ilist->size == 1) {

    /* for single file, cancel times and copy "list" to vi->image */
    vi->hdr = vi->image = iiNew();
    if (!vi->image) {
      imodError(NULL, "Not enough memory.\n"); 
      exit(3);
    }
    memcpy(vi->image, ilist->data, sizeof(ImodImageFile));
    ivwReopen(vi->image);
    vi->curTime = vi->numTimes = 0;
    vi->imageList = NULL;
    if (cmaps) {
      xcramp_copyfalsemap(&vi->image->colormap[768 * vi->li->zmin]);
      xcramp_ramp(vi->cramp);
    }

  } else {

    /* For multiple files, copy the whole image list to vi->imageList */
    vi->imageList = (ImodImageFile *)malloc
      (sizeof(ImodImageFile) * ilist->size);
    if (!vi->imageList) {
      imodError(NULL, "Not enough memory.\n"); 
      exit(3);
    }
    memcpy(vi->imageList, ilist->data, sizeof(ImodImageFile) * ilist->size);
    vi->hdr = vi->image = &vi->imageList[vi->imagePyramid ? 
                                         vi->pyrCache->getBaseIndex() : 0];

    /* for times, set up initial time; for multifile Z, reopen first image */
    if (vi->imagePyramid) {
      vi->curTime = vi->numTimes = 0;
      ivwReopen(vi->image);
 
    } else if (!vi->multiFileZ) {
      ivwSetTime(vi, 1);
      vi->dim |= 8;
    } else {
      ivwReopen(vi->image);
    }
  }
  if (vi->ushortStore)
    ImodInfoWidget->setLHSliders(vi->rangeLow, vi->rangeHigh, vi->image->smin, 
                                 vi->image->smax, vi-> image->type == IITYPE_FLOAT);
          
  ilistDelete(ilist);
  return 0;
}

/* Take care of initial flipping of data and initializing new or read-in model */
static int initializeFlipAndModel(ImodView *vi)
{
  int flipit;
  int retcode;

  // Set this to -1 so image flipping works, but model initializing can skip things */
  vi->doingInitialLoad = -1;

  /* Flip data if called for, but do not generate error if it is not flippable */
  if (!vi->fakeImage) {
    flipit = (vi->li->axis == 2) ? 1 : 0;
    vi->li->axis = 3;
    if (flipit) {
      retcode = ivwFlip(vi);

      // This leads to an exit so just returning is OK
      if (retcode && retcode != -1)
        return (retcode);
    }
  }

  // Now that flipping is done, set up read-in or new model by standard routes
  if (Model) {
    initReadInModelData(Model, false);
  } else {
    retcode = createNewModel(Imod_filename);
    if (retcode != IMOD_IO_SUCCESS)
      return retcode;
  }
  vi->doingInitialLoad = 0;
  if (ClipHandler)
    ClipHandler->doneWithLoad();
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
  if (((size_t)vi->xUnbinSize * (size_t)vi->yUnbinSize) / vi->xUnbinSize
      != vi->yUnbinSize) {
    imodError(NULL, "This image is too large in X and Y to load on a 32-bit "
              "computer.\n");
    exit(3);
  }

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

  // Forbid z binning for RGB images
  if (vi->rgbStore && vi->zbin > 1) {
    vi->zbin = 1;
    wprint("\a\nBinning in Z cannot be used with RGB data.\n");
  }

  // forbid Z binning for multifile Z or montage (need to test multiFileZ this way)
  if ((vi->multiFileZ > 0 || vi->li->plist || vi->imagePyramid || vi->stripOrTileCache) 
      && vi->zbin > 1) {
    vi->zbin = 1;
    if (vi->imagePyramid || vi->stripOrTileCache) 
      wprint("\a\nThe Z dimension cannot be binned with an image pyramid.\n");
    else if (vi->stripOrTileCache) 
      wprint("\a\nThe Z dimension cannot be binned with strip/tile caching.\n");
    else if (vi->li->plist)
      wprint("\a\nThe Z dimension cannot be binned with montaged data.\n");
    else
      wprint("\a\nThe Z dimension cannot be binned with multiple "
             "single-section files.\n");
  }

  // Get binned size of image file
  nxbin = nx / vi->xybin;
  nybin = ny / vi->xybin;
  nzbin = nz / vi->zbin;

  if (vi->xybin * vi->zbin > 1) {
    
    // Forbid flipped loading for non-isotropic binning
    if (vi->xybin != vi->zbin)
      vi->flippable = 0;

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
  vi->xysize = (size_t)vi->xsize * (size_t)vi->ysize;
  vi->fullXsize = nxbin;
  vi->fullYsize = nybin;
  vi->fullZsize = nzbin;
  if (vi->pyrCache)
    vi->pyrCache->adjustForBinning();
    
  return 0;
}

/* Tilt angle functions */
int ivwReadAngleFile(ImodView *vi, const char *fname)
{
  FILE *fin;
  Ilist *list;
  float angle;
  int scanret;
  fin = fopen(fname, "r");
  if (!fin) {
    imodError(NULL, "3dmod warning: could not open angle file %s", fname);
    return 1;
  }
  list = ilistNew(sizeof(float), 10);
  while (1) {
    scanret = fscanf(fin, "%f", &angle);
    if (scanret != 1)
      break;
    if (!list || ilistAppend(list, &angle)) {
      imodError(NULL, "3dmod warning: could not allocate memory for angles",
                fname);
      fclose(fin);
      return 2;
    }
  }
  if (vi->tiltAngles)
    free(vi->tiltAngles);
  vi->tiltAngles = (float *)list->data;
  vi->numTiltAngles = ilistSize(list);
  free(list);
  fclose(fin);
  return 0;
}

// Return number and pointer to tilt angles; start angles at zmin
float *ivwGetTiltAngles(ImodView *vi, int &numAngles)
{
  int zmin = B3DMAX(0, vi->li->zmin);
  numAngles = B3DMAX(0, vi->numTiltAngles - zmin);
  return (numAngles ? vi->tiltAngles + zmin : NULL);
}

/* plugin utility functions.*/
void ivwGetImageSize(ImodView *inImodView, int *outX, int *outY, int *outZ)
{
  *outX = inImodView->xsize;
  *outY = inImodView->ysize;
  *outZ = inImodView->zsize;
}

int ivwGetImageStoreMode(ImodView *vi)
{
  return vi->rawImageStore;
}

bool ivwDataInTileOrStripCache(ImodView *inImodView)
{
  return inImodView->pyrCache != NULL;
}

unsigned char **ivwGetTileCachedSection(ImodView *inImodView, int section)
{
  if (inImodView->pyrCache)
    return inImodView->pyrCache->getFullSection(section);
  return NULL;
}

void ivwFreeTileCachedSection(ImodView *inImodView)
{
  if (inImodView->pyrCache)
    inImodView->pyrCache->freeFullSection();
}

int ivwGetMovieModelMode(ImodView *vw)
{
  if (!vw || vw->imod->mousemode == IMOD_MMOVIE)
    return 0;
  return 1;
}

Imod *ivwGetModel(ImodView *inImodView)
{
  if (inImodView == NULL) 
    return(NULL);
  return(inImodView->imod);
}

void startExtraObjectIfNone(ImodView *vi)
{
  if (!vi->extraObj) {
    vi->extraObj = imodObjectNew();
    vi->extraObjInUse = (int *)malloc(sizeof(int));
    vi->numExtraObj = (vi->extraObj && vi->extraObjInUse) ? 1 : 0;
    if (!vi->numExtraObj) {
      if (vi->extraObj)
        free(vi->extraObj);
      if (vi->extraObjInUse)
        free(vi->extraObjInUse);
      vi->extraObj = NULL;
      vi->extraObjInUse = NULL;
    } else
      vi->extraObjInUse[0] = 1;
  }
}

Iobj *ivwGetExtraObject(ImodView *inImodView)
{
  return ivwGetAnExtraObject(inImodView, 0);
}

int ivwGetFreeExtraObjectNumber(ImodView *vi)
{
  int i;
  for (i = 1; i < vi->numExtraObj; i++) {
    if (!vi->extraObjInUse[i]) {
      vi->extraObjInUse[i] = 1;

      // Return object to default when reassigning it
      imodObjectDefault(&vi->extraObj[i]);
      //imodPrintStderr("Allocating free existing object # %d\n", i);
      return i;
    }
  }
  startExtraObjectIfNone(vi);
  if (!vi->extraObj)
    return -1;
  vi->numExtraObj++;
  vi->extraObj = (Iobj *)realloc(vi->extraObj, vi->numExtraObj * sizeof(Iobj));
  vi->extraObjInUse = (int *)realloc(vi->extraObjInUse, vi->numExtraObj * 
                                     sizeof(int));
  if (!vi->extraObj || !vi->extraObjInUse) {
    vi->numExtraObj = 0;
    vi->extraObj = NULL;
    vi->extraObjInUse = NULL;
    return -1;
  }
  imodObjectDefault(&vi->extraObj[vi->numExtraObj - 1]);
  vi->extraObjInUse[i] = 1;
  //imodPrintStderr("Allocating new extra object # %d\n", vi->numExtraObj - 1);
  return (vi->numExtraObj - 1);
}

int ivwFreeExtraObject(ImodView *vi, int objNum)
{
  if (objNum < 1 || objNum >= vi->numExtraObj || !vi->extraObjInUse[objNum])
    return 1;
  ivwClearAnExtraObject(vi, objNum);
  vi->extraObjInUse[objNum] = 0;
  //imodPrintStderr("Freed extra object # %d\n", objNum);
  return 0;
}

Iobj *ivwGetAnExtraObject(ImodView *inImodView, int objNum)
{
  if (inImodView == NULL || objNum < 0 || 
      objNum >= inImodView->numExtraObj || !inImodView->extraObjInUse[objNum])
    return(NULL);
  return &inImodView->extraObj[objNum];
}

/* Delete all contours in extra object */
void ivwClearExtraObject(ImodView *inImodView)
{
  ivwClearAnExtraObject(inImodView, 0);
}

void ivwClearAnExtraObject(ImodView *inImodView, int objNum)
{
  Iobj *obj = ivwGetAnExtraObject(inImodView, objNum);
  if (!obj)
    return;
  //imodPrintStderr("Clearing extra object # %d\n", objNum);
  if (obj->contsize)
    imodContoursDelete(obj->cont, obj->contsize);
  obj->contsize = 0;
  obj->cont = NULL;
  vbCleanupVBD(obj);
  if (obj->meshsize)
    imodMeshesDelete(obj->mesh, obj->meshsize);
  obj->meshsize = 0;
  obj->mesh = NULL;
  if (obj->store)
    ilistDelete(obj->store);
  obj->store = NULL;
}

void ivwEnableStipple(ImodView *inImodView, int enable)
{
  inImodView->drawStipple = enable;
}

void ivwTrackMouseForPlugs(ImodView *inImodView, int enable)
{
  inImodView->trackMouseForPlugs = B3DMAX
    (0, inImodView->trackMouseForPlugs + (enable ? 1 : -1));
  zapSetMouseTracking();
}

int ivwGetTopZapZslice(ImodView *inImodView, int *outZ)
{
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap)
    return 1;
  *outZ = zap->mSection;
  return 0;
}

int ivwGetTopZapZoom(ImodView *inImodView, float *outZoom)
{
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap)
    return 1;
  *outZoom = zap->mZoom;
  return 0;
}

int ivwGetTopSlicerZoom(ImodView *inImodView, float *outZoom)
{
  SlicerFuncs *ss = getTopSlicer();
  if (!ss)
    return 1;
  *outZoom = ss->mZoom;
  return 0;
}

int ivwSetTopZapZoom(ImodView *inImodView, float inZoom, bool draw)
{
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap || inZoom < 0.005 || inZoom > 200.)
    return 1;
  zap->mZoom = inZoom;
  if (draw)
    zap->draw();
  return 0;
}

int ivwSetTopZapZslice(ImodView *inImodView, int inZ)
{
  if (inZ < 0 || inZ >= App->cvi->zsize)
    return 1;
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap)
    return 1;
  if (zap->mLock)
    zap->mSection = inZ;
  else
    App->cvi->zmouse = inZ;
  return 0;
}

int ivwGetTopZapMouse(ImodView *inImodView, Ipoint *imagePt)
{
  return getTopZapMouse(imagePt);
}

int ivwGetTopZapCenter(ImodView *inImodView, float &imX, float &imY, int &imZ)
{
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap)
    return 1;
  if (zap->mLock)
    imZ = zap->mSection;
  else
    imZ = B3DNINT(inImodView->zmouse);
  imX = (float)(inImodView->xsize / 2. - zap->mXtrans);
  imY = (float)(inImodView->ysize / 2. - zap->mYtrans);
  return 0;
}

int ivwSetTopZapCenter(ImodView *inImodView, float imX, float imY, int imZ,
                       bool draw)
{
  if (imX < 0 || imX >= inImodView->xsize || imY < 0 || 
      imY >= inImodView->ysize || imZ < 0 || imZ >= inImodView->zsize)
    return 1;
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap)
    return 1;

  zap->mXtrans = B3DNINT(zap->mVi->xsize / 2. - imX);
  zap->mYtrans = B3DNINT(zap->mVi->ysize / 2. - imY);
  if (zap->mLock) {
    zap->mSection = imZ;
    if (draw)
      zap->draw();
  } else {
    inImodView->zmouse = imZ;
    if (draw)
      imodDraw(inImodView, IMOD_DRAW_XYZ);
  }
  return 0;
}

/* 
 * Do snapshot in top zap or slicer
 */
int ivwSnapshotTopZap(QString &name, int format, bool checkGrayConvert, bool fullArea)
{
  return snapshotTopWindow(name, format, checkGrayConvert, ZAP_WINDOW_TYPE, fullArea);
}

int ivwSnapshotTopSlicer(QString &name, int format, bool checkGrayConvert, bool fullArea)
{
  return snapshotTopWindow(name, format, checkGrayConvert, SLICER_WINDOW_TYPE, fullArea);
}

static int snapshotTopWindow(QString &name, int format, bool checkGrayConvert, 
                             int winType, bool fullArea)
{
  ZapFuncs *zap;
  SlicerFuncs *slicer;
  int retval;
  if ((retval = getTopZapOrSlicer(winType, &zap, &slicer)))
    return retval;
  if (format != SnapShot_TIF && format != SnapShot_JPG && format != SnapShot_PNG)
    return -2;
  int restore = 0;
  if (format != SnapShot_TIF) {
    b3dSetNonTiffSnapFormat(format);
    if (restore < 0)
      return -3;
    format = SnapShot_RGB;
  }
  if (winType == ZAP_WINDOW_TYPE)
    retval = zap->namedSnapshot(name, format, checkGrayConvert, fullArea);
  else
    retval = slicer->namedSnapshot(name, format, checkGrayConvert, fullArea);
  if (restore)
    ImodPrefs->restoreSnapFormat();
  return retval;
}

static int getTopZapOrSlicer(int winType, ZapFuncs **zap, SlicerFuncs **slicer)
{
  if (winType == ZAP_WINDOW_TYPE)
    *zap = getTopZapWindow(false);
  else if (winType == SLICER_WINDOW_TYPE)
    *slicer = getTopSlicer();
  else
    return -4;
  if ((winType == ZAP_WINDOW_TYPE && !*zap) || 
      (winType == SLICER_WINDOW_TYPE && !*slicer))
    return -1;
  return 0;
}

/*
 * Functions for starting new arrow or clearing all arrows
 */
int startAddedArrow(int windowType)
{
  ZapFuncs *zap;
  SlicerFuncs *slicer;
  int retval;
  if ((retval = getTopZapOrSlicer(windowType, &zap, &slicer)))
    return retval;
  if (windowType == ZAP_WINDOW_TYPE)
    zap->startAddedArrow();
  else
    slicer->startAddedArrow();
  return 0;
}

// Clear all arrows in top window or all windows of given type
int clearAllArrows(int windowType, bool allWindows)
{
  ZapFuncs *zap;
  SlicerFuncs *slicer;
  QObjectList objList;
  int i;

  // Get top window regardless, for easy error check
  if ((i = getTopZapOrSlicer(windowType, &zap, &slicer)))
    return i;

  // Top window
  if (!allWindows) {
    if (windowType == ZAP_WINDOW_TYPE)
      zap->clearArrows();
    else
      slicer->clearArrows();
  } else {

    // All windows
    imodDialogManager.windowList(&objList, -1, windowType);
    for (i = 0; i < objList.count(); i++) {
      if (windowType == ZAP_WINDOW_TYPE) {
        zap = ((ZapWindow *)objList.at(i))->mZap;
        zap->clearArrows();
      } else {
        slicer = ((SlicerWindow *)objList.at(i))->mFuncs;
      slicer->clearArrows();
      }
    }
  }
  return 0;
}

// Return the name of the current image file, without any path adjustments
QString ivwCurrentImageFile(ImodView *inImodView, bool asEntered)
{
  if (inImodView->fakeImage)
    return QString("");
  QDir curDir(Imod_IFDpath);
  QString file;
  if (inImodView->multiFileZ <= 0) {
    file =  QString(inImodView->image->filename); 
  } else {
    int cz = B3DNINT(inImodView->zmouse);
    B3DCLAMP(cz, 0, inImodView->multiFileZ - 1);
    file = QString(inImodView->imageList[cz].filename);
  }
  if (asEntered)
    return file;
  return QDir::cleanPath(curDir.absoluteFilePath(file));
}

// Open 3dmod dialogs based on key letters
void ivwOpen3dmodDialogs(const char *keys)
{
  ImodInfoWin->openSelectedWindows(keys, 0);
}

int prefSaveGenericSettings(char *key, int numVals, double *values)
{
  return ImodPrefs->saveGenericSettings(key, numVals, values);
}

int prefGetGenericSettings(char *key, double *values, int maxVals)
{
  return ImodPrefs->getGenericSettings(key, values, maxVals);
}

void imodUpdateObjectDialogs()
{
  imodvObjedNewView();
  imod_object_edit_draw();
  imod_info_setobjcolor();
}

int ivwGetContrastReversed(ImodView *inImodView)
{
  return (inImodView->cramp->reverse);
}

int ivwOverlayOK(ImodView *inImodView)
{
  return (App->rgba == 1 && !App->cvi->rawImageStore);
}

void ivwSetOverlayMode(ImodView *vw, int sec, int reverse, 
                       int whichGreen)
{

  // If changing state, change color ramps
  if ((vw->overlaySec && !sec) || (!vw->overlaySec && sec)) {
    if (vw->overlayRamp < 0) {

      // The first time, save the ramp index, and initialize the next ramp
      // to the same black-white levels
      vw->overlayRamp = vw->cramp->clevel;
      xcrampSelectIndex(vw->cramp, 
                        (vw->overlayRamp + 1) % vw->cramp->noflevels);
      xcramp_setlevels(vw->cramp, vw->black, vw->white);
    } else {
      
      // Otherwise, restore the other color ramp
      xcrampSelectIndex(vw->cramp, 
                        sec ? (vw->overlayRamp + 1) % vw->cramp->noflevels : 
                        vw->overlayRamp);
      xcramp_ramp(vw->cramp);
      xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
      imod_info_setbw(vw->black, vw->white);
    }

    // Reverse if flag set
    if (reverse)
      xcramp_reverse(vw->cramp, !(vw->cramp->reverse));

    // If state is staying on but reverse is changing, then reverse
  } else if (sec && reverse != vw->reverseOverlay)
    xcramp_reverse(vw->cramp, !(vw->cramp->reverse));
      
  vw->reverseOverlay = reverse;
  vw->overlaySec = sec;
  vw->whichGreen = whichGreen;
  imodDraw(vw, IMOD_DRAW_IMAGE | IMOD_DRAW_NOSYNC);
}

// Get the current contour, the last contour if it is empty, or a new contour
Icont *ivwGetOrMakeContour(ImodView *vw, Iobj *obj, int timeLock)
{
  Icont *cont = imodContourGet(vw->imod);
  int curTime = timeLock ? timeLock : vw->curTime;
  if (!cont || (obj->extra[IOBJ_EX_PNT_LIMIT] && 
                cont->psize >= obj->extra[IOBJ_EX_PNT_LIMIT])) {
  
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
    cont->time = curTime;
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
  int time = timelock ? timelock : vi->curTime;
  return (vi->numTimes > 0 && iobjFlagTime(obj) && cont->time && 
          (time != cont->time));
}

int ivwWindowTime(ImodView *vi, int timelock)
{
  return timelock ? timelock : vi->curTime;
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
     
  if (App->depth <= 8) {
    obj->fgcolor = App->objbase - inObject;
  }else{
    obj->fgcolor = App->objbase + inObject;
  }
  objIndex = obj->fgcolor;
  return(objIndex);
}

// Set the black and white levels from the values stored in a model, adjusting as needed
void ivwSetBlackWhiteFromModel(ImodView *vi)
{
  vi->black = vi->imod->blacklevel;
  vi->white = vi->imod->whitelevel;
  if (vi->ushortStore) {
    if (vi->black < 256 && vi->white < 256) {
      vi->black *= 256;
      vi->white *= 256;
    }
  } else if (vi->black > 255 || vi->white > 255) {
    vi->black /= 256;
    vi->white /= 256;
  }
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
  size_t nxins = nxin;
  unsigned char *bdata = brray;
  unsigned char *cline1, *cline2, *cline3, *cline4;


  switch (nbin) {
  case 2:
    for (iy = 0; iy < nyout; iy++) {
      cline1 = ((unsigned char *)array) + 2 * iy * nxins;
      cline2 = cline1 + nxins;
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
      cline1 = ((unsigned char *)array) + (3 * iy + iyofs) * nxins + ixofs;
      cline2 = cline1 + nxins;
      cline3 = cline2 + nxins;
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
      cline1 = ((unsigned char *)array) + (4 * iy + iyofs) * nxins + ixofs;
      cline2 = cline1 + nxins;
      cline3 = cline2 + nxins;
      cline4 = cline3 + nxins;
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
      cline1 = ((unsigned char *)array) + (nbin * iy + iyofs) * nxins + ixofs;
      for (ix = 0; ix < nxout; ix++) {
        sum = 0;
        cline2 = cline1;
        for (j = 0; j < nbin; j++) {
          for (i = 0; i < nbin; i++) 
            sum += cline2[i];
          cline2 += nxins;
        }
        *bdata++ = sum / nbinsq;
        cline1 += nbin;
      }
    }
    break;
  }
}

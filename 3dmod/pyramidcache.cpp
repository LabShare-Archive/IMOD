/*
 *  pyramidcache - a class for working with image pyramid or strip/tile cache
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include <algorithm>
#include "pyramidcache.h"
#include "info_cb.h"
#include "workprocs.h"
#include "control.h"
#include "display.h"
#include "xxyz.h"
#include "xzap.h"
#include "zap_classes.h"
#include "slicer_classes.h"
#include "sslice.h"
#include "isosurface.h"
#include "mv_image.h"

#define FULL_SEC_BUF 3

//////////////////////////////////////////////////////////////////////////////
// CACHE INITIALIZATION
//////////////////////////////////////////////////////////////////////////////

/*
 * The constructor: sets up the caches for either a pyramid or a basic tile cache,
 * determines the biggest file, and  checks as many things as possible about the files
 */
PyramidCache::PyramidCache(ViewInfo *vi, QStringList &plFileNames, int frames, 
                           bool anyImageFail)
{
  ImodImageFile *image;
  Ilist *ilist = (Ilist *)vi->imageList;
  IloadInfo li;
  int maxXsize, maxYsize, maxZsize, ind, xscale, yscale, jnd;
  float expected;
  TileCache *cache, *fullCache;

  mZoomUpLimit = 1.55;
  mZoomDownLimit = 0.7;
  mVi = vi;
  mUseCount = 0.;
  mPyrInd = -1;
  mTotalPixels = 0.;
  mBufXsize = mBufYsize = 0;
  mLastRequestSize = 0;
  for (ind = 0; ind < 3; ind++) {
    mSliceBufs[ind] = NULL;
    mBufLoadZ[ind] = -1;
  }
  mBufCacheInd = -1;
  mBufTotSize = 0;
  mFullSecBuf = NULL;
  mFullSecPtrs = NULL;

  if (vi->imagePyramid && anyImageFail)
    pyramidError(0);
  if (vi->imagePyramid && vi->numTimes == 1) {    
    imodError(NULL, "3DMOD Error: "
              "You specified an image pyramid with only one %s.\n", 
              vi->ifd ? "file in the image list" : "image file");
    exit(3);
  }

  // Start to set up the caches for pyramid or tile
  if (vi->imagePyramid || vi->stripOrTileCache) {
    mNumCaches = vi->imagePyramid ? vi->numTimes : 1;
    mTileCaches = B3DMALLOC(TileCache, mNumCaches);
    mOrderedList = B3DMALLOC(int, mNumCaches);
    if (!mTileCaches || !mOrderedList)
      pyramidError(2);
    for (ind = 0; ind < mNumCaches; ind++)
      mOrderedList[ind] = ind;
  }

  // Done if no pyramid
  if (!vi->imagePyramid)
    return;

  //if (vi->rawImageStore)
      //pyramidError(1);
  if (vi->li->axis == 2)
    pyramidError(11);

  maxXsize = 0;
  maxYsize = 0;
  maxZsize = 0;

  for (mPyrInd = 0; mPyrInd < vi->numTimes; mPyrInd++) {
    cache = &mTileCaches[mPyrInd];
    li.plist = 0;
    image = (ImodImageFile *)ilistItem(ilist, mPyrInd);
    if (mPyrInd < plFileNames.size() && plFileNames[mPyrInd] != "NONE") {
      if (ivwLoadIFDpieceList(LATIN1(plFileNames[mPyrInd]), &li, image->nx, image->ny, 
                              image->nz))
        pyramidError(3);
    } else if (image->hasPieceCoords && !frames) {
      if (ivwReopen(image) > 0)
        pyramidError(4);
      iiLoadPCoord(image, 0, &li, image->nx, image->ny, image->nz);
      if (!li.plist)
        pyramidError(5);
      iiClose(image);
    }

    if (image->format == IIFORMAT_COMPLEX)
      pyramidError(6);
    if (image->format == IIFORMAT_COLORMAP)
      pyramidError(7);
    if (image->format == IIFORMAT_RGB && 
        !((image->file == IIFILE_MRC  || image->file == IIFILE_RAW) && vi->grayRGBs))
      pyramidError(8);

    if ((ind = setupTileCache(image, &li, cache)) != 0)
      pyramidError(ind);

    maxXsize = B3DMAX(maxXsize, cache->fullXsize);
    maxYsize = B3DMAX(maxYsize, cache->fullYsize);
    maxZsize = B3DMAX(maxZsize, cache->fullZsize);

  }

  // Find the biggest image (it had better be biggest in all dimensions)
  mBaseIndex = -1;
  mPyrInd = -1;
  for (ind = 0; ind < vi->numTimes; ind++) {
    cache = &mTileCaches[ind];
    if (cache->fullXsize == maxXsize && cache->fullYsize == maxYsize && 
        cache->fullZsize == maxZsize)
      mBaseIndex = ind;
  }
  if (mBaseIndex < 0)
    pyramidError(10);

  // Find integral scale of each image relative to biggest
  fullCache = &mTileCaches[mBaseIndex];
  for (mPyrInd = 0; mPyrInd < vi->numTimes; mPyrInd++) {
    if (mPyrInd == mBaseIndex)
      continue;
    cache = &mTileCaches[mPyrInd];
    xscale = B3DNINT((double)maxXsize / cache->fullXsize);
    yscale = B3DNINT((double)maxYsize / cache->fullYsize);
    if (xscale != yscale)
      pyramidError(12);
    cache->xyScale = xscale;
    cache->zScale = B3DNINT((double)maxZsize / cache->fullZsize);
    
    // Adjust to get offset to be relative to biggest image
    cache->xOffset -= fullCache->xOffset / xscale;
    cache->yOffset -= fullCache->yOffset / yscale;
    cache->zOffset -= fullCache->zOffset / cache->zScale;
    
    // Add twice the offset to get an expected size, assuming centered data
    expected = cache->fullXsize + 2 * cache->xOffset;
    if (fabs(maxXsize / xscale - expected) > cache->numXtiles + 2.5)
      pyramidError(13);
    expected = cache->fullYsize + 2 * cache->yOffset;
    if (fabs(maxYsize / yscale - expected) > cache->numYtiles + 2.5)
      pyramidError(14);

    // But for Z, the offset should be purely to account for binning
    if (fabs((double)maxZsize / cache->zScale - cache->fullZsize) > 2.5)
      pyramidError(15);
  }
  mPyrInd = -1;

  // Fix the offset and scale of the full res one.
  fullCache->xOffset = fullCache->zOffset = fullCache->yOffset = 0.;
  fullCache->xyScale = fullCache->zScale = 1;
  vi->imagePyramid = vi->numTimes;
  vi->flippable = 0;
  vi->li->plist = 0;
  vi->stripOrTileCache = 0;

  // Order the list by XY scale
  for (ind = 0; ind < mNumCaches - 1; ind++)
    for (jnd = ind + 1; jnd < mNumCaches; jnd++)
      if (mTileCaches[mOrderedList[ind]].xyScale > 
          mTileCaches[mOrderedList[jnd]].xyScale)
        B3DSWAP(mOrderedList[ind], mOrderedList[jnd], xscale);

  // Set these so the full size is known
  vi->xUnbinSize = fullCache->fullXsize;
  vi->yUnbinSize = fullCache->fullYsize;
  vi->zUnbinSize = fullCache->fullZsize;
}

/*
 * This sets the cache members for one image and potential piece coordinates in li
 */
int PyramidCache::setupTileCache(ImodImageFile *image, IloadInfo *li, TileCache *cache)
{
  int minxpiece, minypiece, i, remnant, multSize, gcd;

  // Image offset is positive for positive start relative to largest image
  cache->xOffset = -image->xtrans / image->xscale;
  cache->yOffset = -image->ytrans / image->yscale;
  cache->zOffset = -image->ztrans / image->zscale;
  cache->fileYtileOffset = 0;
  if (li->plist) {

    // For a montage, set up the full size and make sure piece coords work
    cache->plistSize = li->plist;
    cache->pcoords = li->pcoords;
    cache->fullXsize = li->px;
    cache->fullYsize = li->py;
    cache->fullZsize = li->pz;
    cache->xOffset += li->opx;
    cache->yOffset += li->opy;
    cache->zOffset += li->opz;
    if (checkPieceList(cache->pcoords, 3, li->plist, 1, image->nx, &minxpiece, 
                       &cache->numXtiles, &cache->xOverlap) || 
        checkPieceList(cache->pcoords + 1, 3, li->plist, 1, image->ny, &minypiece,
                       &cache->numYtiles, &cache->yOverlap))
      return 9;
    cache->xTileSize = image->nx;
    cache->yTileSize = image->ny;

    for (i = 0; i < li->plist; i++) 
      if (li->pcoords[3 * i] % mVi->xybin || li->pcoords[3 * i + 1] % mVi->xybin)
        return 19;

  } else {

    // For a non-montage, the size is it, but there may be tiles in TIFF files
    cache->plistSize = 0;
    cache->fullXsize = image->nx;
    cache->fullYsize = image->ny;
    cache->fullZsize = image->nz;
    cache->xOverlap = 0;
    cache->yOverlap = 0;
    imodTrace('t', "file %d TIIF %d  tile %d %d", image->file, IIFILE_TIFF, 
              image->tileSizeX, image->tileSizeY);
    if (image->file == IIFILE_TIFF) {
      cache->xTileSize = image->tileSizeX;
      cache->yTileSize = image->tileSizeY;

      // Get the offset of the first strip or tiles in Y, because TIFF file is inverted
      // and the partial strip is at the start in our coordinates
      if (cache->yTileSize) {
        cache->numYtiles = (cache->fullYsize + cache->yTileSize - 1) / cache->yTileSize;
        cache->fileYtileOffset = cache->numYtiles * cache->yTileSize - image->ny;
      }
    } else {
      cache->xTileSize = 0;
      cache->yTileSize = 0;
    }

    // If there are strips, make the loaded strip size be a multiple of that
    if (!cache->xTileSize && cache->yTileSize) {

      // find ideal number of multiples of strip size
      multSize = B3DMAX(1, FAKE_STRIP_PIXELS / (cache->fullXsize * cache->yTileSize));

      // Find greatest common divisor or binning and strip size, and the factor of the
      // binning that is not in the strip size, and make the multiplier be a multiple of
      // this remanant factor
      for (gcd = mVi->xybin; gcd > 1; gcd--)
        if (cache->yTileSize % gcd == 0 && mVi->xybin % gcd == 0)
          break;
      remnant = mVi->xybin / gcd;
      multSize = remnant * ((multSize + remnant - 1) / remnant);
      cache->yTileSize *= multSize;
      imodTrace('t', "Strip size multiplied by %d to %d for loading\n", multSize, 
                cache->yTileSize);
    }

    // Assign a useful strip size if none exists; make it divisible by binning if any
    if (!cache->xTileSize && !cache->yTileSize) {
      cache->yTileSize = B3DMAX(4, FAKE_STRIP_PIXELS / cache->fullXsize);
      cache->yTileSize = mVi->xybin * ((cache->yTileSize + mVi->xybin - 1) / mVi->xybin);
      if (cache->yTileSize >= cache->fullYsize)
        cache->yTileSize = 0;
    }
    
    // Provisionally get the number of tiles, and get the first tile Y offset for a TIFF
    cache->numXtiles = 0;
    cache->numYtiles = 0;
    if (cache->xTileSize)
      cache->numXtiles = (cache->fullXsize + cache->xTileSize - 1) / cache->xTileSize;
    if (cache->yTileSize)
      cache->numYtiles = (cache->fullYsize + cache->yTileSize - 1) / cache->yTileSize;
  }
  imodTrace('t', "cache %d size %d %d overlap %d %d tiles %d %d of %d x %d", mPyrInd,
            cache->fullXsize, cache->fullYsize, cache->xOverlap, cache->yOverlap,
            cache->numXtiles, cache->numYtiles, cache->xTileSize, cache->yTileSize);

  // Check if binning will work by simple division
  if (cache->fullXsize % mVi->xybin || cache->fullYsize % mVi->xybin)
    return 16;
  if (cache->xTileSize % mVi->xybin || cache->yTileSize % mVi->xybin)
    return 17;
  if (cache->xOverlap % mVi->xybin || cache->yOverlap % mVi->xybin)
    return 18;
  return 0;
}

/*
 * Fatal error processing: adds the file number if mPyrInd >= 0 and adds a line
 * about pyramid implied from montage and other files
 */
void PyramidCache::pyramidError(int err)
{
  const char *mess[] = {
    "Must be able to open all files in the image list\n"
    "when using an image pyramid.", //0
    "You cannot use -I and load an image pyramid as integers", // 1
    "Memory error allocating cache structures", // 2
    "Reading piece list file", // 3
    "Reopening image file to read piece list", // 4
    "Reading or processing piece list from image file header", // 5
    "Complex files cannot be used for an image pyramid", // 6
    "Color map files cannot be used for an image pyramid", // 7
    "RGB (color) files cannot be used for an image pyramid\n"
    "unless loaded in grayscale", // 8
    "Piece coordinates do not fall on a regular grid", //9
    "For an image pyramid, one file must be the biggest in all dimensions", // 10
    "You cannot flip/rotate Y and Z when loading an image pyramid", // 11
    "The scaling relative to the biggest file is not the same in X and Y", // 12
    "The file is not close enough to the expected size in X", // 13
    "The file is not close enough to the expected size in Y", // 14
    "The file is not close enough to the expected size in Z", // 15
    "The full size in X or Y is not evenly divisible by the requested binning", // 16
    "The tile size in X or Y is not evenly divisible by the requested binning", // 17
    "The overlap between pieces in X or Y is not evenly divisible\n"
    "by the requested binning", // 18
    "Some of the piece coordinates in X or Y are not evenly divisible\n"
    "by the requested binning", // 19
    "Memory error allocating cache index", // 20
    "Reopening file for use", // 21
  };
  QString str = "3dmod Error: ";
  str += mess[err];
  if (mPyrInd >= 0)
    str += QString(" for image file #%1").arg(mPyrInd + 1);
  if (err && (err < 2 || err > 5) && err < 20 && mVi->imagePyramid < 0)
    str += "\n(You cannot load a montage with other image\n"
      "files unless using them for an image pyramid)";
  imodError(NULL, LATIN1(str));
  exit(3);
}

/*
 * Adjusts all the cache data for the X/Y binning
 */
void PyramidCache::adjustForBinning()
{
  int ind, i, bin = mVi->xybin;
  TileCache *cache;

  // Adjust all the dimensions and offsets
  for (ind = 0; ind < mNumCaches; ind++) {
    cache = &mTileCaches[ind];
    cache->fullXsize /= bin;
    cache->fullYsize /= bin;
    cache->xTileSize /= bin;
    cache->yTileSize /= bin;
    cache->xOverlap /= bin;
    cache->yOverlap /= bin;
    cache->xOffset /= bin;
    cache->yOffset /= bin;

    // Adjust piece coordinates for pyramid
    if (mVi->imagePyramid) {
      for (i = 0; i < cache->plistSize; i++) {
        cache->pcoords[3 * i] /= bin;
        cache->pcoords[3 * i + 1] /= bin;
      }
    }
  }
}

/*
 * Sets up the simple one-file cache
 */
void PyramidCache::setupStripOrTileCache()
{
  int err;
  if ((err = setupTileCache(mVi->image, mVi->li, &mTileCaches[0])) != 0)
    pyramidError(err);
  mTileCaches[0].xOffset = mTileCaches[0].yOffset = mTileCaches[0].zOffset = 0.;
  mTileCaches[0].xyScale = 1;
  mTileCaches[0].zScale = 1;
  mBaseIndex = 0;
}

/*
 * Final setup of caches including cacheIndex allocation
 */
void PyramidCache::initializeCaches()
{
  int i, ind, tileTot;
  TileCache *cache;
  ImodImageFile *image;
  for (ind = 0; ind < mNumCaches; ind++) {
    cache = &mTileCaches[ind];
  
    // Set up the load limits based on the scaling for each file, revising the offsets
    // This will simply put the vi load limits into the limits for biggest file
    setLoadLimits(cache->xyScale, cache->fullXsize, mVi->li->xmin, mVi->li->xmax,
                  cache->xOffset, cache->minXload, cache->maxXload);
    setLoadLimits(cache->xyScale, cache->fullYsize, mVi->li->ymin, mVi->li->ymax, 
                  cache->yOffset, cache->minYload, cache->maxYload);
    setLoadLimits(cache->zScale, cache->fullZsize, mVi->li->zmin, mVi->li->zmax,
                  cache->zOffset, cache->minZload, cache->maxZload);
    cache->loadXsize = cache->maxXload + 1 - cache->minXload;
    cache->loadYsize = cache->maxYload + 1 - cache->minYload;
    cache->loadZsize = cache->maxZload + 1 - cache->minZload;

    // Set up the tiling in X and Y
    setTileGeometry(cache->fullXsize, cache->minXload, cache->maxXload, cache->xOverlap, 
                    0, cache->xTileSize, cache->numXtiles, 
                    cache->startXtile, cache->firstXoffset);
    setTileGeometry(cache->fullYsize, cache->minYload, cache->maxYload, cache->yOverlap,
                    cache->fileYtileOffset, cache->yTileSize, cache->numYtiles,
                    cache->startYtile, cache->firstYoffset);
    cache->tileXdelta = cache->xTileSize - cache->xOverlap;
    cache->tileYdelta = cache->yTileSize - cache->yOverlap;

  imodTrace('t', "cache %d start tile %d %d fxoffset %d %d tiles %d %d  offsets %.2f %.2f",
            ind, cache->startXtile, cache->startYtile, cache->firstXoffset, 
            cache->firstYoffset, cache->numXtiles, cache->numYtiles,
            cache->xOffset, cache->yOffset);
  imodTrace('t', "    load x %d %d  y %d %d  z %d %d", cache->minXload, cache->maxXload,
            cache->minYload, cache->maxYload, cache->minZload, cache->maxZload);

    // Allocate the index
    tileTot = cache->numXtiles * cache->numYtiles * cache->loadZsize;
    cache->tileIndex = B3DMALLOC(int, tileTot);
    if (!cache->tileIndex)
      pyramidError(20);
    for (i = 0; i < tileTot; i++)
      cache->tileIndex[i] = -1;

    // Make sure the file is open
    if (mNumCaches > 1) {
      image = &mVi->imageList[ind];
      if (image->state != IISTATE_READY && ivwReopen(image) > 0) {
        mPyrInd = ind;
        perror(NULL);
        pyramidError(21);
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////////////
// TILE LOADING, COPYING, AND MANAGEMENT
//////////////////////////////////////////////////////////////////////////////////

/*
 * Frees oldest tile if necessary to make space for the given number of pixels.
 * cacheInd is the index of the cache where space is about to be needed, tiles at zval and
 * otherZ (if not -1) will be protected on the first loop, and numLoop should be 1 or 2
 */
int PyramidCache::freeForNeededPixels(int cacheInd, int zval, int otherZ, 
                                      double numPixels, int numLoops)
{
  CacheSlice *tile;
  std::map<double,int>::iterator iter;
  int loop, freeInd;

  // Loop twice through tiles from oldest to newest, first time protecting tiles on the 
  // same Z level and cache
  for (loop = 0; loop < numLoops; loop++) {
    while (numPixels + mTotalPixels > mVMpixels) {
      if (mUseMap.empty())
        return -1;
      for (iter = mUseMap.begin(); iter != mUseMap.end(); iter++) {
        freeInd = (*iter).second;
        tile = &mTiles[freeInd];
        if (loop || tile->cacheInd != cacheInd || 
            (tile->section != zval && tile->section != otherZ)) {

          // Remove this tile
          freeTile(tile, freeInd, true);
          mUseMap.erase(iter);
          break;
        }
      }
    }
    if (numPixels + mTotalPixels <= mVMpixels) 
      break;
  }
  return 0;
}

/*
 * Makes a new item in the given cache for the given tile and Z value of the given size
 * otherZ is another Z value to be protected if tiles need to be freed
 */
int PyramidCache::makeNewCacheItem(int cacheInd, int xtile, int ytile, int zval,
                                   int otherZ, int xsize, int ysize)
{
  CacheSlice newTile, *tile;
  TileCache *cache;
  std::map<double,int>::iterator iter;
  int useInd;
  double numPixels = (double)xsize * ysize;

  // Loop twice through tiles from oldest to newest, first time protecting tiles on the 
  // same Z level and cache
  if (freeForNeededPixels(cacheInd, zval, otherZ, numPixels, 2))
    return -1;

  // There is enough memory.  Get free array element or make one
  if (mFreeTiles.empty()) {
    mTiles.push_back(newTile);
    useInd = mTiles.size() - 1;
  } else {
    useInd = mFreeTiles.top();
    mFreeTiles.pop();
  }

  // Assign values to element
  cache = &mTileCaches[cacheInd];
  tile = &mTiles[useInd];
  tile->cacheInd = cacheInd;
  tile->xTileInd = xtile;
  tile->yTileInd = ytile;
  tile->section = zval;
  tile->usedAtCount = mUseCount;
  tile->slice = sliceCreate(xsize, ysize, mVi->rawImageStore);
  
  // If allocation failed, add to free list and mark as free
  if (!tile->slice) {
    mFreeTiles.push(useInd);
    useInd = -1;
    tile->cacheInd = -1;
  } else {

    // Otherwise add to use count map and the cache's index
    mUseMap[mUseCount] = useInd;
    cache->tileIndex[xtile + (ytile + zval * cache->numYtiles) * cache->numXtiles] = 
      useInd;
    mTotalPixels += numPixels;
  }
  mUseCount += 1.;
  return useInd;
}

/*
 * Frees the given tile at the given index, manages the total pixel count, cache index
 * free tile list, and, if haveUserIter is false, removes from use map
 */
void PyramidCache::freeTile(CacheSlice *tile, int freeInd, bool haveUserIter)
{
  TileCache *cache;
  mTotalPixels -= tile->slice->xsize * tile->slice->ysize;
  sliceFree(tile->slice);
  tile->slice = NULL;
  cache = &mTileCaches[tile->cacheInd];
  cache->tileIndex[tile->xTileInd + (tile->yTileInd + tile->section * 
                                     cache->numYtiles) * cache->numXtiles] = -1;
  
  // Mark the tile as not used, add to free list, and remove from use map
  tile->cacheInd = -1;
  mFreeTiles.push(freeInd);
  if (!haveUserIter)
    mUseMap.erase(tile->usedAtCount);
}

/*
 * Loads all the tiles on the request queue.  If asyncLoad is 0, it does it
 * without interruption, if it is 1, then it calls to have events processed after
 * each tile; if it is -1, it loads one tile and returns
 */
int PyramidCache::loadRequestedTiles(int asyncLoad)
{
  LoadTileRequest request;
  TileCache *cache;
  ImodImageFile *image;
  int zval, tileInd, retval = 0;

  while (!mRequests.empty()) {
    request = mRequests.front();
    mRequests.pop_front();
    cache = &mTileCaches[request.cacheInd];
    if (mNumCaches > 1)
      image = &mVi->imageList[request.cacheInd];
    else
      image = mVi->image;
    
    // Find the limits for loading this tile in coordinates of the full image file
    findLoadLimits(request.cacheInd, request.xTileInd, request.yTileInd, false,
                   image->llx, image->urx, image->lly, image->ury);

    // Section numbers are loaded values and so add load Z to get to file Z
    zval = request.section + cache->minZload;
    if (adjustLoadLimitsForMont(cache, image->nx, image->ny, image->llx, image->urx,
                                image->lly, image->ury, zval))
      continue;

    // Make a new cache item to hold data
    tileInd = makeNewCacheItem(request.cacheInd, request.xTileInd, request.yTileInd, 
                               request.section, -1,  image->urx + 1 - image->llx, 
                               image->ury + 1 - image->lly);
    if (tileInd < 0) 
      return 1;

    // Read the data in
    if (ivwReadBinnedSection(mVi, image, (char *)mTiles[tileInd].slice->data.b, zval))
      retval = 1;

    if (!retval) {
      request.indInCache = tileInd;
      copyTileIntoBuffer(request, false);
    }
    if (asyncLoad < 0)
      return retval;
    if (asyncLoad > 0)
      imod_info_input();
  }
  return retval;
}

int PyramidCache::adjustLoadLimitsForMont(TileCache *cache, int nx, int ny, int &llx,
                                          int &urx, int &lly, int &ury, int &zval)
{
  int i, xstart, ystart;
  if (!cache->plistSize)
    return 0;

  // For montage, find the piece Z that contains these and modify the min/maxes
  for (i = 0; i < cache->plistSize; i++) {
    if (cache->pcoords[3 * i + 2] == zval) {
      xstart = cache->pcoords[3 * i];
      ystart = cache->pcoords[3 * i + 1];
      if (llx >= xstart && urx < xstart + nx / mVi->xybin && 
          lly >= ystart && ury < ystart + ny / mVi->xybin) {
        zval = i;
        llx -= xstart;
        urx -= xstart;
        lly -= ystart;
        ury -= ystart;
        return 0;
      }
    }
  }
  return 1;
}

/*
 * Given a tile number in X and Y and a section, for a given cache, it will either copy
 * the tile into the given buffer index if it is loaded, or add it to the request queue
 * if it is not.  bufXstart, bufXsize are the starting loaded cache coordinates and size
 * in X of the buffer being copied to, similarly for Y.  Returns true if request queued.
 */
bool PyramidCache::copyOrQueueTile(int cacheInd, LoadTileRequest &request, int xtile, 
                                   int ytile, int zsec, int bufInd, int bufXstart,
                                   int bufXsize, int bufYstart, int bufYsize)
{
  int tileXmin, tileXmax, tileYmin, tileYmax;
  int useXmin, useXmax, useYmin, useYmax;
  TileCache *cache = &mTileCaches[cacheInd];
  request.indInCache = cache->tileIndex[xtile + (ytile + zsec * cache->numYtiles) * 
                                        cache->numXtiles];
  request.xTileInd = xtile;
  request.yTileInd = ytile;
  request.cacheInd = cacheInd;
  request.bufInd = bufInd;
  request.section = zsec;

  // Get coordinates of tile in cache loaded coordinate space and intersect
  // with the buffer limits
  findLoadLimits(cacheInd, xtile, ytile, true, tileXmin, tileXmax, tileYmin, tileYmax);
  useXmin = B3DMAX(tileXmin, bufXstart);
  useXmax = B3DMIN(tileXmax, bufXstart + bufXsize - 1);
  useYmin = B3DMAX(tileYmin, bufYstart);
  useYmax = B3DMIN(tileYmax, bufYstart + bufYsize - 1);

  // Put limits in request structure
  request.fromXstart = useXmin - tileXmin;
  request.toXstart = useXmin - bufXstart;
  request.numXcopy = useXmax + 1 - useXmin;
  request.fromYstart = useYmin - tileYmin;
  request.toYstart = useYmin - bufYstart;
  request.numYcopy = useYmax + 1 - useYmin;
  if (request.indInCache < 0) {
    imodTrace('T', "Queuing %d %d from start %d %d copy %d %d to %d %d",
              xtile, ytile, request.fromXstart, request.fromYstart, 
              request.numXcopy, request.numYcopy, request.toXstart,
              request.toYstart);
            
    // If tile is not loaded, add request to queue
    mRequests.push_back(request);
  } else {

    // If tile is loaded, send request to copy routine
    copyTileIntoBuffer(request, true);
  }
  return request.indInCache < 0;
}

/*
 * Copies the tile described in the request into the proper buffer and optionally 
 * updates use count
 */
void PyramidCache::copyTileIntoBuffer(LoadTileRequest &request, bool manageUseCount)
{
  CacheSlice *tile = &mTiles[request.indInCache];
  int fromSkip = tile->slice->xsize - request.numXcopy;
  int toSkip = mBufXsize - request.numXcopy;
  unsigned char *buf;

  // Do the copy
  if (request.numXcopy > 0 && request.numYcopy > 0) {
    if (request.bufInd == FULL_SEC_BUF) {
      buf = mFullSecBuf;
      toSkip = mVi->xsize - request.numXcopy;
    } else {
      buf = mSliceBufs[request.bufInd];
    }
    imodTrace('T', "copying %d %d from start %d %d copy %d %d to %d %d",
              request.xTileInd, request.yTileInd, request.fromXstart, request.fromYstart, 
              request.numXcopy, request.numYcopy, request.toXstart, request.toYstart);
    memreccpy(buf, tile->slice->data.b, 
              request.numXcopy, request.numYcopy, ivwGetPixelBytes(mVi->rawImageStore),
              toSkip, request.toXstart, request.toYstart,
              fromSkip, request.fromXstart, request.fromYstart);
  }

  // Update the use count unless it was brand new tile
  if (manageUseCount) {
    mUseMap.erase(tile->usedAtCount);
    tile->usedAtCount = mUseCount;
    mUseMap[mUseCount] = request.indInCache;
    mUseCount += 1.;
  }
}

//////////////////////////////////////////////////////////////////////////////
// EXTERNAL CALLS FOR LOADING DESIRED AREAS
//////////////////////////////////////////////////////////////////////////////

/*
 * Make sure that the tile in the base cache containing this point is loaded
 */
int PyramidCache::loadBaseTileWithPoint(int x, int y, int z)
{
  return loadTilesContainingArea(mBaseIndex, x, y, 1, 1, z);
}

/*
 * Make sure all the tiles containing this area are loaded, synchronously if possible
 * The X/Y are base coordinates but the z value is in cache loaded coordinates
 */
int PyramidCache::loadTilesContainingArea(int cacheInd, int baseXstart, int baseYstart,
                                          int baseXsize, int baseYsize, int z)
{
  LoadTileRequest request, queued;
  int xtmp, ytmp, tileInd, retval = 0;
  int startXtile, startYtile, endXtile, endYtile, xtile, ytile;
  int xstart, xsize, ystart, ysize;
  float xoffset, yoffset;
  TileCache *cache = &mTileCaches[cacheInd];
  std::deque<LoadTileRequest>::iterator iter;
  bool onQueue, loadQueue = false;
  
  // Convert to selected cache coordinates
  scaledAreaSize(cacheInd, baseXstart, baseYstart, baseXsize, baseYsize, xstart, ystart, 
                 xsize, ysize, xoffset, yoffset, false);

  // Get tiles needed for lower left and upper right
  getTileAndPositionInTile(cacheInd, xstart, ystart, startXtile, startYtile, xtmp, 
                           ytmp);
  getTileAndPositionInTile(cacheInd, xstart + xsize - 1, ystart + ysize - 1, endXtile,
                           endYtile, xtmp, ytmp);

  // Loop on the tiles
  for (ytile = startYtile; ytile <= endYtile; ytile++) {
    for (xtile = startXtile; xtile <= endXtile; xtile++) {
      tileInd = cache->tileIndex[xtile + (ytile + z * cache->numYtiles) * 
                                 cache->numXtiles];
      if (tileInd >= 0)
        continue;
      request.xTileInd = xtile;
      request.yTileInd = ytile;
      request.section = z;
      request.cacheInd = cacheInd;
      request.numXcopy = 0;
      request.numYcopy = 0;

      // If there is a queue being loaded, check if already on it
      if (mRequests.size() && !loadQueue) {
        onQueue = false;
        iter = mRequests.begin();
        while (iter != mRequests.end()) {
          queued = *iter++;
          if (queued.section == z && queued.cacheInd == cacheInd && 
              queued.xTileInd == xtile && queued.yTileInd == ytile) {
            onQueue = true;
            break;
          }
        }
        
        // If is not on it, add it to the queue, hope for the best, and return 1
        if (!onQueue)
          mRequests.push_back(request);
        retval = 1;
      } else {

        // If no queue, add to it and set flag that we are going to load it
        mRequests.push_back(request);
        loadQueue = true;
      }
    }
  }
  // If we are starting a queue, run it synchronously, return -1 if error
  if (loadQueue)
    imodTrace('t', "loadTilesContainingArea loading %d tiles", mRequests.size());
  if (loadQueue && loadRequestedTiles(0))
    return -1;
  return retval;
}

/*
 * Fill a cache for a displayed area around the given section.  The source should be
 * 0 for a general fill with default priorities, -1 for a fill for image in model view,
 * or 1 for a fill for an image window
 */
void PyramidCache::fillCacheForArea(int section, int source)
{
  int foundType;
  XyzWindow *xyz;
  SlicerFuncs *ss;
  TileCache *cache;
  double zoom, numPix = 0.;
  float xoffset, yoffset;
  int xstart, ystart, xsize, ysize, ind, baseXstart, baseYstart, nxUse, nyUse;
  int tileXmin, tileXmax, tileYmin, tileYmax, xtile, ytile, cacheInd, needed;
  int numSlices, middleZ, scale, dir, delz, iz;
  int startXtile, startYtile, xtmp, ytmp, endXtile, endYtile, zstart, zend;
  float zoomUpLimit = mZoomUpLimit;
  float zoomDownLimit = mZoomDownLimit;

  // Look at a sequence of image uses, starting with isosurface, then model view image
  // then the image display windows
  if (!source && imodvIsosurfaceBoxLimits(baseXstart, baseYstart, nxUse, nyUse)) {
    zoom = 1.;
    imodTrace('t', "Using isosurface limits");
  } else if (source <= 0 && mvImageSubsetLimits(zoom, zoomUpLimit, zoomDownLimit, 
                                                baseXstart, baseYstart, nxUse, nyUse)) {
    imodTrace('t', "Using mvImage limits");
  } else {
    
    // Go for the top slicer or xyz window, or failing that, get a Zap
    QObject *topWin = imodDialogManager.getTopWindow(XYZ_WINDOW_TYPE, SLICER_WINDOW_TYPE, 
                                                   foundType);
    if (foundType < 0)
      topWin = imodDialogManager.getTopWindow(ZAP_WINDOW_TYPE, ZAP_WINDOW_TYPE,
                                              foundType);
    if (foundType < 0) {
      wprint("\aFill Cache can be used only when isosurface or image is displayed in "
             "Model View or there is a Slicer, XYZ, or Zap window open\n");
      return;
    }

    // Get the area and zoom as appropriate for the window type
    if (foundType == ZAP_WINDOW_TYPE) {
      imodTrace('t', "Top is Zap");
      if (zapSubsetLimits(mVi, baseXstart, baseYstart, nxUse, nyUse)) {
        wprint("\aCannot get subarea limits from top Zap window\n");
        return;
      }
      zoom = ((ZapWindow *)topWin)->mZap->mZoom;
    } else if (foundType == XYZ_WINDOW_TYPE) {
      imodTrace('t', "Top is XYZ");
      xyz = (XyzWindow *)topWin;
      xyz->getSubsetLimits(baseXstart, baseYstart, nxUse, nyUse);
      zoom = xyz->mZoom;
    } else {
      imodTrace('t', "Top is Slicer");
      ss = ((SlicerWindow *)topWin)->mFuncs;
      ss->getSubsetLimits(baseXstart, baseYstart, nxUse, nyUse);
      zoom = 1.;
    }
  }

  // Find the cache being used there
  cacheInd = pickBestCache(zoom, zoomUpLimit, zoomDownLimit, scale);
  cache = &mTileCaches[cacheInd];
  imodTrace('t', "Cache fill zoom %f  limits %d %d %d %d  index %d", zoom, baseXstart, 
            baseYstart, nxUse, nyUse, cacheInd);

  // Find range of tiles needed to cover the area and total pixels in them
  scaledAreaSize(cacheInd, baseXstart, baseYstart, nxUse, nyUse, xstart, ystart, xsize,
                 ysize, xoffset, yoffset, false);
  scaledRangeInZ(cacheInd, section, section, middleZ, xtmp, scale, xoffset);
  getTileAndPositionInTile(cacheInd, xstart, ystart, startXtile, startYtile, xtmp, ytmp);
  getTileAndPositionInTile(cacheInd, xstart + xsize - 1, ystart + ysize - 1, endXtile,
                           endYtile, xtmp, ytmp);
  for (ytile = startYtile; ytile <= endYtile; ytile++) {
    for (xtile = startXtile; xtile <= endXtile; xtile++) {
      findLoadLimits(cacheInd, xtile, ytile, true, tileXmin, tileXmax, tileYmin,
                     tileYmax);
      numPix += (tileXmax + 1 - tileXmin) * (tileYmax + 1 - tileYmin);
    }
  }

  // Determine number of slices that can be loaded and range of Z
  numSlices = (int)((0.9 * mVMpixels - 3. * mBufTotSize) / numPix);
  if (numSlices < 3) {
    wprint("\aThe cache is not big enough to load more than 2 sections of this size\n");
    return;
  }
  zstart = B3DMAX(0, middleZ - numSlices / 2);
  zend = B3DMIN(cache->maxZload - cache->minZload, zstart + numSlices - 1);
  imodMovieXYZT(mVi, 0, 0, 0, 0);

  // Loop through slices and tiles and update their use counts if they are loaded
  needed = 0;
  for (iz = zstart; iz <= zend; iz++) {
    for (ytile = startYtile; ytile <= endYtile; ytile++) {
      for (xtile = startXtile; xtile <= endXtile; xtile++) {
        ind = cache->tileIndex[xtile + (ytile + iz * cache->numYtiles) *
                               cache->numXtiles];
        if (ind >= 0) {
          mUseMap.erase(mTiles[ind].usedAtCount);
          mTiles[ind].usedAtCount = mUseCount;
          mUseMap[mUseCount] = ind;
          mUseCount += 1.;
        } else
          needed++;
      }
    }
  }

  wprint("Loading tiles for sections %d to %d...", scale * zstart + 1, scale * zend + 1);

  if (needed) {
    mRequests.clear();
    
    // Load from the middle out
    for (delz = 0; delz <= numSlices; delz++) {
      for (dir = -1; dir <= 1; dir += 2) {
        iz = middleZ + dir * delz;
        if (iz < zstart || iz > zend)
          continue;
        loadTilesContainingArea(cacheInd, baseXstart, baseYstart, nxUse, nyUse, iz);
        imod_info_input();
      }
    }
  }

  wprint("DONE!\n");
}

//////////////////////////////////////////////////////////////////////////////
// ROUTINES FOR ACCESSING PLANES OF IMAGE DATA
//////////////////////////////////////////////////////////////////////////////

/*
 * Return the subarea of a section needed for display from a chosen cache.
 * Section is the Z value, fullXstart and fullYstart are the starting coordinates needed,
 * and xsizeIn and ysizeIn are the size of area needed, all loaded coordinates of the
 * base cache.  Zoom is the zoom for the display, asyncLoad indicates whether loading can
 * be asynchronous, xsizeOut and ysizeOut are the number of pixels actually available,
 * xoffset and yoffset are the offset, in scaled pixels, of those pixels relative to 
 * full-sized request, scale is the scale factor of the cache, and status is returned with
 * the number of tiles left to load, or -1 for errors.
 */
unsigned char **PyramidCache::getSectionArea(int section, int fullXstart, int fullYstart, 
                                             int xsizeIn, int ysizeIn, double zoom,
                                             bool asyncLoad, int &xsizeOut, int &ysizeOut,
                                             float &xoffset, float &yoffset, int &scale,
                                             int &status)
{
  int ind, which, pixSize, xstart, ystart, numBuf, zind, indStart, indEnd;
  int fillVal,xtile, ytile, bufInd, startXtile, startYtile, XinTile, YinTile;
  int endXtile, endYtile, tileXmin, tileXmax, tileYmin, tileYmax, maxPix, numPix;
  int skip, ix, iy, i, j, fac1, fac2;
  size_t sizeNeeded;
  float zfrac;
  bool inBufferXY, zScaled, needInterp, bufferOK, loadOK, oneUsable, usableZ, tooSmall;
  bool shrink, newTilesLoaded;
  int startXinner, endXinner, startYinner, endYinner, firstXtile, firstYtile;
  int otherZ[3], needLoadZ[3], needInBuf[2];
  TileCache *cache;
  LoadTileRequest request;
  unsigned char *bbuf, *bbuf1, *bbuf2;
  unsigned short *usbuf, *usbuf1, *usbuf2;
  std::vector<int> xTileLoad, yTileLoad;
  float shrinkTileCrit = 2.;
  float shrinkFreeCrit = 0.2f;

  status = -1;

  // Find the cache with the largest zoom up to a limit
  which = pickBestCache(zoom, mZoomUpLimit, mZoomDownLimit, scale);
  cache = &mTileCaches[which];
  pixSize = ivwGetPixelBytes(mVi->rawImageStore);

  // Translate the full coordinates to scaled ones
  scaledAreaSize(which, fullXstart, fullYstart, xsizeIn, ysizeIn, xstart, ystart, 
                 xsizeOut, ysizeOut, xoffset, yoffset, true);

  // Is the existing buffer enough?
  inBufferXY = which == mBufCacheInd && mBufTotSize &&
    xstart >= mBufXstart && xstart + xsizeOut <= mBufXstart + mBufXsize &&
    ystart >= mBufYstart && ystart + ysizeOut <= mBufYstart + mBufYsize;
  imodTrace('T', "out start %d %d size %d %d  off %.1f %.1f buf start %d %d  size %d %d",
    xstart, ystart, xsizeOut, ysizeOut, xoffset, yoffset, mBufXstart, mBufYstart,
    mBufXsize, mBufYsize);
  needInterp = false;
  otherZ[0] = -1;
  needLoadZ[0] = section;
  zScaled = cache->zScale > 1;
  if (zScaled) {
    needInterp = findInterpolatedZvals(cache, section, needLoadZ, otherZ, zfrac);
    for (i = 0; i < (needInterp ? 2 : 1); i++) {
      needInBuf[i] = -1;
      for (j = 0; j < 3; j++)
        if (needLoadZ[i + 1] == mBufLoadZ[j])
          needInBuf[i] = j;
    }
    imodTrace('T', "cache %d scale %d  interp %s  needz %d %d %d  frac %.2f  inbuf %d %d",
              which, scale, needInterp ? "y" : "n", needLoadZ[0], needLoadZ[1], 
              needLoadZ[2], needInterp ? zfrac : 0., needInBuf[0],needInBuf[1]);
          
    usableZ = !needInterp && needInBuf[0] >= 0;
  } else {
    imodTrace('T', "cache %d scale %d", which, scale) ;
  }

  // Everything is fine if the requested section matches
  // But not if there are pending requests and this load is supposed to be synchronous
  bufferOK = inBufferXY && (section == mBufSection) && (asyncLoad || !mRequests.size());
  loadOK = bufferOK;
  oneUsable = inBufferXY && needInterp && (needInBuf[0] >= 0 || needInBuf[1] >= 0);
  newTilesLoaded = mRequests.size() < mLastRequestSize;
  mBufCacheInd = which;

  // If we just need one Z slice and it is present in one of the 3 buffer, this is good
  if (!bufferOK && inBufferXY && zScaled && usableZ) {

    // Rearrange buffers so that the needed one is in 0
    if (needInBuf[0] > 0) {
      imodTrace('t', "Swapped one needed from %d into 0", needInBuf[0]);
      B3DSWAP(mBufLoadZ[0], mBufLoadZ[needInBuf[0]], i);
      B3DSWAP(mSliceBufs[0], mSliceBufs[needInBuf[0]], bbuf);
    }
    bufferOK = true;
    loadOK = true;
  }

  // If we need interpolation and either Z slice is present, restore it to where it
  // belongs in the interp buffers
  if (!bufferOK && oneUsable) {

    // If lower buffer exists and is not where it belongs, swap it into place, and
    // if the upper buffer just got swapped out of there, change its position
    if (needInBuf[0] >= 0 && needInBuf[0] != 1) {
      imodTrace('t', "Swapped lower buf from %d into 1", needInBuf[0]);
      B3DSWAP(mBufLoadZ[1], mBufLoadZ[needInBuf[0]], i);
      B3DSWAP(mSliceBufs[1], mSliceBufs[needInBuf[0]], bbuf);
      if (needInBuf[1] == 1)
        needInBuf[1] = needInBuf[0];
    }

    // Now swap the upper buffer into place if it exists
    if (needInBuf[1] >= 0 && needInBuf[1] != 2) {
      imodTrace('t', "Swapped upper buf from %d into 2", needInBuf[1]);
      B3DSWAP(mBufLoadZ[2], mBufLoadZ[needInBuf[1]], i);
      B3DSWAP(mSliceBufs[2], mSliceBufs[needInBuf[1]], bbuf);
    }
    loadOK = needInBuf[0] >= 0 && needInBuf[1] >= 0;
  }
  
  // If the current load has no use at all, then first manage the slice buffers
  if (!(loadOK || oneUsable)) {

    // Determine the area desired for the slice buffer and whether it is too big or small
    optimalBufferSize(xstart, xsizeOut, cache->loadXsize, mBufXstart, mBufXsize);
    optimalBufferSize(ystart, ysizeOut, cache->loadYsize, mBufYstart, mBufYsize);
    tooSmall = mBufXsize * mBufYsize > mBufTotSize;
    shrink = !tooSmall && (mBufTotSize - mBufXsize * mBufYsize > 
                           shrinkTileCrit * cache->xTileSize * cache->yTileSize && 
                           mVMpixels - mTotalPixels < mVMpixels * shrinkFreeCrit);
    imodTrace('t', "buf start %d %d size %d %d %d  small %d big %d", mBufXstart, 
              mBufYstart, mBufXsize, mBufYsize, mBufTotSize, tooSmall?1:0, shrink?1:0);

    // Clear out Z for new buffer area
    for (ind = 0; ind < 3; ind++)
      mBufLoadZ[ind] = -1;

    // If the buffer is not big enough or needs to be shrunk or we are switching between 
    // Z interp and not, redo the whole thing
    if (shrink || tooSmall || !mBufTotSize || (zScaled && !mSliceBufs[1]) ||
        (!zScaled && mSliceBufs[1])) {
    
      // Clear everything
      for (ind = 0; ind < 3; ind++) {
        if (mSliceBufs[ind])
          mTotalPixels -= mBufTotSize;
        B3DFREE(mSliceBufs[ind]);
      }
      mBufTotSize = 0;

      // Free up space if needed, protecting the section(s) needed
      sizeNeeded = mBufXsize * mBufYsize;
      numBuf = zScaled ? 3 : 1;
      zind = zScaled ? 1 : 0;
      if (freeForNeededPixels(which, needLoadZ[zind], otherZ[zind], 
                              (double)numBuf * sizeNeeded, 1)) {
        return NULL;
      }

      // Allocate
      for (ind = 0; ind < numBuf; ind++) {
        mSliceBufs[ind] = (unsigned char *)malloc(pixSize * sizeNeeded);
        if (mSliceBufs[ind])
          mTotalPixels += sizeNeeded;
        else
          return NULL;
      }
      mBufTotSize = sizeNeeded;
    }
  }
  
  if (!loadOK) {
    
    // Set up buffer indexes
    if (needInterp) {
      indStart = needLoadZ[1] == mBufLoadZ[1] ? 2 : 1;
      indEnd = needLoadZ[2] == mBufLoadZ[2] ? 1 : 2;
    } else {
      indStart = 0;
      indEnd = 0;
    }
    for (bufInd = indStart; bufInd <= indEnd; bufInd++)
      mBufLoadZ[bufInd] = needLoadZ[bufInd];

    // Set a fill value according to the current slider settings
    fillVal = (mVi->white + mVi->black) / 2;

    // Find the range of tiles that intersect the buffer
    // If doing async, start list with ones that cover the displayed area
    if (asyncLoad) {
      getTileAndPositionInTile(mBufCacheInd, xstart + xsizeOut / 10, ystart + 
                               ysizeOut / 10, startXinner, startYinner, XinTile, YinTile);
      getTileAndPositionInTile(mBufCacheInd, xstart + xsizeOut - xsizeOut / 10 - 1, 
                               ystart + ysizeOut - ysizeOut / 10 - 1, endXinner,
                               endYinner, XinTile, YinTile);

      // Find out which one covers the largest fraction of displayed area
      maxPix = -1;
      for (ytile = startYinner; ytile <= endYinner; ytile++) {
        for (xtile = startXinner; xtile <= endXinner; xtile++) {
          findLoadLimits(mBufCacheInd, xtile, ytile, true, tileXmin, tileXmax, tileYmin,
                         tileYmax);
          tileXmin = B3DMAX(tileXmin, xstart);
          tileXmax = B3DMIN(tileXmax, xstart + xsizeOut - 1);
          tileYmin = B3DMAX(tileYmin, ystart);
          tileYmax = B3DMIN(tileYmax, ystart + ysizeOut - 1);
          numPix = (tileXmax + 1 - tileXmin) * (tileYmax + 1 - tileYmin);
          if (numPix > maxPix) {
            maxPix = numPix;
            firstXtile = xtile;
            firstYtile = ytile;
          }
        }
      }

      // Put that on the list first, then put the rest of the inner ones on the list
      xTileLoad.push_back(firstXtile);
      yTileLoad.push_back(firstYtile);
      for (ytile = startYinner; ytile <= endYinner; ytile++) {
        for (xtile = startXinner; xtile <= endXinner; xtile++) {
          if (xtile != firstXtile || ytile != firstYtile) {
            xTileLoad.push_back(xtile);
            yTileLoad.push_back(ytile);
          }
        }
      }
    }

    // Then get the full range of tiles and complete the list
    getTileAndPositionInTile(mBufCacheInd, mBufXstart, mBufYstart, startXtile, startYtile,
                             XinTile, YinTile);
    getTileAndPositionInTile(mBufCacheInd, mBufXstart + mBufXsize - 1, 
                             mBufYstart + mBufYsize - 1, endXtile, endYtile,
                             XinTile, YinTile);
    for (ytile = startYtile; ytile <= endYtile; ytile++) {
      for (xtile = startXtile; xtile <= endXtile; xtile++) {
        if (!asyncLoad || xtile < startXinner || xtile > endXinner ||
            ytile < startYinner || ytile > endYinner) {
          xTileLoad.push_back(xtile);
          yTileLoad.push_back(ytile);
        }
      }
    }

    // Loop on the buffers and tiles and copy them or set up request for them
    mRequests.clear();
    for (ind = 0; ind < xTileLoad.size(); ind++) {
      xtile = xTileLoad[ind];
      ytile = yTileLoad[ind];
      for (bufInd = indStart; bufInd <= indEnd; bufInd++) {
        if (copyOrQueueTile(mBufCacheInd, request, xtile, ytile, needLoadZ[bufInd],
                            bufInd, mBufXstart, mBufXsize, mBufYstart, mBufYsize)) {
            
          // If tile is not loaded, fill the area
          bbuf = mSliceBufs[bufInd] + pixSize * 
            (request.toXstart + request.toYstart * mBufXsize);
          usbuf = (unsigned short *)bbuf;
          skip = mBufXsize - request.numXcopy;
          for (iy = 0; iy < request.numYcopy; iy++) {
            if (mVi->ushortStore) {
              for (ix = 0; ix < request.numXcopy; ix++)
                *usbuf++ = fillVal;
              usbuf += skip;
            } else {
              for (ix = 0; ix < request.numXcopy; ix++)
                *bbuf++ = fillVal;
              bbuf += skip;
            }
          }
        }
      }
    }

    // Load one tile regardless when doing async
    if (mRequests.size() && asyncLoad)
      loadRequestedTiles(-1);

    // Then either start the work proc to do the loads asynchronously, or load all
    if (mRequests.size()) {
      if (asyncLoad)
        mVi->timers->startTileLoading();
      else
        loadRequestedTiles(0);
    }
  }
  mBufSection = section;

  // Interpolate at last
  if (needInterp && (!bufferOK || newTilesLoaded)) {
    bbuf = mSliceBufs[0];
    bbuf1 = mSliceBufs[1];
    bbuf2 = mSliceBufs[2];
    usbuf = (unsigned short *)bbuf;
    usbuf1 = (unsigned short *)bbuf1;
    usbuf2 = (unsigned short *)bbuf2;
    fac2 = B3DNINT(256 * zfrac);
    fac1 = 256 - fac2;
    for (iy = 0; iy < mBufYsize; iy++) {
      if (mVi->ushortStore) {
        for (ix = 0; ix < mBufXsize; ix++)
          *usbuf++ = (fac1 * *usbuf1++ + fac2 * *usbuf2++) >> 8;
      } else {
        for (ix = 0; ix < mBufXsize; ix++)
          *bbuf++ = (fac1 * *bbuf1++ + fac2 * *bbuf2++) >> 8;
      }
    }
  }

  // Make line pointers
  bbuf = mSliceBufs[0] + pixSize * ((xstart - mBufXstart) + 
                                   (ystart - mBufYstart) * mBufXsize);
  status = mRequests.size();
  mLastRequestSize = status;
  return ivwMakeLinePointers(mVi, bbuf, mBufXsize, ysizeOut, mVi->rawImageStore);
}

/*
 * Gets a complete Z section for the base file, storing it in a buffer allocated just
 * for this purpose and return separately allocated line pointers
 */
unsigned char **PyramidCache::getFullSection(int z)
{
  int xtile, ytile;
  int pixSize = mVi->ushortStore ? 2 : 1;
  size_t sizeNeeded = (size_t)mVi->xsize * mVi->ysize;
  TileCache *cache = &mTileCaches[mBaseIndex];
  LoadTileRequest request;

  // If buffer is not allocated, try to allocate it under the total cache allowance
  if (!mFullSecBuf) {
    if (freeForNeededPixels(mBaseIndex, z, B3DNINT(mVi->zmouse), (double)sizeNeeded + 
                            cache->xTileSize * cache->yTileSize, 1)) {
      wprint("\aCache is not big enough to allow full section to be stored\n");
      return NULL;
    }
    mFullSecBuf = (unsigned char *)malloc(sizeNeeded * pixSize);
    if (!mFullSecBuf)
      return NULL;
    mTotalPixels += sizeNeeded;
    mFullSecPtrs = makeLinePointers(mFullSecBuf, mVi->xsize, mVi->ysize, pixSize);
    if (!mFullSecPtrs) {
      freeFullSection();
      return NULL;
    }

    // If buffer is already loaded for this section, pointers are all set
  } else if (z == mFullSecZ)
    return mFullSecPtrs;

  // Otherwise copy or queue each tile
  mRequests.clear();
  for (ytile = 0; ytile < cache->numYtiles; ytile++) {
    for (xtile = 0; xtile < cache->numXtiles; xtile++) {
      copyOrQueueTile(mBaseIndex, request, xtile, ytile, z, FULL_SEC_BUF, 0, mVi->xsize,
                      0, mVi->ysize);
    }
  }

  // Load synchronously
  if (mRequests.size())
    loadRequestedTiles(0);

  mFullSecZ = z;
  return mFullSecPtrs;
}

/*
 * Frees the full section data and returns space to the cache allowance
 */
void PyramidCache::freeFullSection()
{
  if (mFullSecBuf)
    mTotalPixels -= (double)mVi->xsize * mVi->ysize;
  B3DFREE(mFullSecBuf);
  B3DFREE(mFullSecPtrs);
}

//////////////////////////////////////////////////////////////////////////////
// ROUTINES FOR FAST ACCESS TO PIXELS OR LINES OF PIXELS
//////////////////////////////////////////////////////////////////////////////

/*
 * Return parameters needed to set up use of ivwFastGetValue with tile caches
 */
void PyramidCache::setupFastAccess(int cacheInd, unsigned char **imdata, int *vmdataxsize,
                                   int &cacheSum, int &tileXdelta, int &tileYdelta,
                                   int &firstXoffset, int &firstYoffset)
{
  TileCache *cache = &mTileCaches[cacheInd];
  int ind, index;
  cacheSum = 0;
  for (ind = 0; ind < cache->numXtiles * cache->numYtiles * cache->loadZsize; ind++) {
    index = cache->tileIndex[ind];
    if (index < 0) {
      imdata[ind] = NULL;
    } else {
      imdata[ind] = mTiles[index].slice->data.b;
      vmdataxsize[ind] = mTiles[index].slice->xsize;
      cacheSum += index;
    }
  }
  tileXdelta = cache->tileXdelta;
  tileYdelta = cache->tileYdelta;
  firstXoffset = cache->firstXoffset - cache->xOverlap / 2;
  firstYoffset = cache->firstYoffset - cache->yOverlap / 2;
}

int PyramidCache::loadedCacheSum(int cacheInd)
{
  TileCache *cache = &mTileCaches[cacheInd];
  int ind, index, cacheSum;
  for (ind = 0; ind < cache->numXtiles * cache->numYtiles * cache->loadZsize; ind++) {
    index = cache->tileIndex[ind];
    if (index >= 0)
      cacheSum += index;
  }
  return cacheSum;
}

/*
 * Analyze tiles loaded in the given cache and provide a list of segments that provide
 * data for filling in the given plane on the given axis.  StartInds is returned
 * with the starting index of segments at each Y line in the plane.
 */
int PyramidCache::fastPlaneAccess(int cacheInd, int plane, int axis,
                                  std::vector<FastSegment> &segments,
                                  std::vector<int> &startInds)
{
  TileCache *cache = &mTileCaches[cacheInd];
  int nz = cache->loadZsize;
  FastSegment seg;
  std::vector<FastSegment> rowSegs;
  int pixSize = ivwGetPixelBytes(mVi->rawImageStore);
  int xtile, ytile, xInTile, yInTile, tileXmin, tileXmax, tileYmin, tileYmax;
  int iseg, numSeg, iz, ind, iy;
  if (cacheInd < 0 || cacheInd >= mNumCaches)
    return 1;
  
  segments.clear();
  switch (axis) {
  case b3dX:
    startInds.resize(nz + 1);

    // For an X plane, first find the X tile and position in it that corresponds to plane
    getTileAndPositionInTile(cacheInd, plane, 0, xtile, ytile, xInTile, yInTile);
    for (iz = 0; iz < nz; iz++) {
      startInds[iz] = segments.size();

      // Loop on all the Y tiles in that column
      for (ytile = 0; ytile < cache->numYtiles; ytile++) {
        ind = cache->tileIndex[xtile + (ytile + iz * cache->numYtiles) * 
                               cache->numXtiles];
        if (ind >= 0) {

          // Get limits of this tile in Y in loaded coordinates and specify the segment
          // from the limits, indenting to the proper X position and striding by x size
          findLoadLimits(cache->yTileSize, cache->yOverlap, ytile, cache->numYtiles,
                         cache->startYtile, cache->fileYtileOffset, cache->minYload,
                         cache->maxYload, true, tileYmin, tileYmax);
          seg.XorY = tileYmin;
          seg.YorZ = iz;
          seg.length = tileYmax + 1 - tileYmin;
          seg.stride = mTiles[ind].slice->xsize;
          seg.line = mTiles[ind].slice->data.b + pixSize * xInTile;
          segments.push_back(seg);
        }
      }
    }
    startInds[nz] = segments.size();
    break;

  case b3dY:
    startInds.resize(nz + 1);

    // For a Y plane, find the y tile and position in it that correspond to plane
    getTileAndPositionInTile(cacheInd, 0, plane, xtile, ytile, xInTile, yInTile);
    for (iz = 0; iz < nz; iz++) {
      startInds[iz] = segments.size();

      // Loop on the X tiles in that row
      for (xtile = 0; xtile < cache->numXtiles; xtile++) {
        ind = cache->tileIndex[xtile + (ytile + iz * cache->numYtiles) * 
                               cache->numXtiles];
        if (ind >= 0) {

          // Get limits of tile in X in loaded coordinates and specify a segment at
          // the correct Y line with a stride of 1
          findLoadLimits(cache->xTileSize, cache->xOverlap, xtile, cache->numXtiles,
                         cache->startXtile, 0, cache->minXload,
                         cache->maxXload, true, tileXmin, tileXmax);
          seg.XorY = tileXmin;
          seg.YorZ = iz;
          seg.length = mTiles[ind].slice->xsize;
          seg.stride = 1;
          seg.line = mTiles[ind].slice->data.b + pixSize * yInTile * seg.length;
          segments.push_back(seg);
        }
      }
    }
    startInds[nz] = segments.size();
    break;

  case b3dZ:
    startInds.resize(cache->maxYload + 2 - cache->minYload);
    rowSegs.resize(cache->numXtiles);

    // For a Z plane, loop on the y tiles and find loaded Y limits for a row of tiles
    for (ytile = 0; ytile < cache->numYtiles; ytile++) {
      findLoadLimits(cache->yTileSize, cache->yOverlap, ytile, cache->numYtiles,
                     cache->startYtile, cache->fileYtileOffset, cache->minYload,
                     cache->maxYload, true, tileYmin, tileYmax);

      // For a row of tiles, compile a list of segments at the bottom of the tiles
      numSeg = 0;
      for (xtile = 0; xtile < cache->numXtiles; xtile++) {
        ind = cache->tileIndex[xtile + (ytile + plane * cache->numYtiles) * 
                               cache->numXtiles];
        if (ind >= 0) {
          findLoadLimits(cache->xTileSize, cache->xOverlap, xtile, cache->numXtiles,
                         cache->startXtile, 0, cache->minXload,
                         cache->maxXload, true, tileXmin, tileXmax);
          rowSegs[numSeg].XorY = tileXmin;
          rowSegs[numSeg].YorZ = tileYmin;
          rowSegs[numSeg].length = mTiles[ind].slice->xsize;
          rowSegs[numSeg].stride = 1;
          rowSegs[numSeg++].line = mTiles[ind].slice->data.b;
        }
      }

      // Replicate those segments for the extent in Y, increasing Y and pointer each time
      for (iy = 0; iy < tileYmax + 1 - tileYmin; iy++) {
        startInds[iy + tileYmin] = segments.size();
        for (iseg = 0; iseg < numSeg; iseg++) {
          segments.push_back(rowSegs[iseg]);
          rowSegs[iseg].YorZ++;
          rowSegs[iseg].line += pixSize * rowSegs[iseg].length;
        }
      }
    }
    startInds[tileYmax] = segments.size();
    break;

  default:
    return 1;
  }
  return 0;
}

//////////////////////////////////////////////////////////////////////////////
// GETTING STATISTICS BY SAMPLING ALL TILES
//////////////////////////////////////////////////////////////////////////////

/*
 * Computes the mean and SD of an area by sampling, given whatever tiles are loaded for 
 * a given cache.  Returns a cache sum that can be used to check if tiles have changed;
 * if cacheSum equals oldCacheSum, it will return without computing the mean/SD again.
 * When Z interpolation is used, it used a tile only if it is loaded at both Z values.
 * If the optional arguments pctLo, pctHi, scaleLo and scaleHi are supplied, then it
 * also computes the scale values for a percentile stretch with the given percentages,
 * using the median of values from the different tiles.
 */
int PyramidCache::loadedMeanSD(int cacheInd, int section, float sample, int ixStart,
                               int iyStart, int nxUse, int nyUse, float *mean, float *SD,
                               int &cacheSum, int oldCacheSum, float pctLo, float pctHi,
                               float *scaleLo, float *scaleHi)
{
  TileCache *cache;
  int sampleType = mVi->ushortStore ? 2 : 0;
  double numPix = 0., xsum = 0., xsqsum = 0.;
  float tmpMean, tmpSD, xoffset, yoffset, loTemp, hiTemp;
  int xstart, ystart, xsize, ysize, loop, nxtmp, index[3];
  int tileXmin, tileXmax, tileYmin, tileYmax, xtile, ytile, xend, yend, otherZ[3];
  int useXmin, useXmax, nxTileUse, useYmin, useYmax, nyTileUse, indz, neededZ[3];
  int startXtile, startYtile, xtmp, ytmp, endXtile, endYtile, indZstart, indZend;
  int nsum = 0, numTiles = 0;
  bool needInterp;
  float zweights[3] = {1., 1., 1.}, zfrac;
  Islice *slice;
  unsigned char **lines;
  std::vector<float> loScales, hiScales;

  // Find area that is needed for analysis
  if (cacheInd >= mNumCaches)
    return 1;
  if (cacheInd < 0)
    cacheInd = mBaseIndex;
  cache = &mTileCaches[cacheInd];
  scaledAreaSize(cacheInd, ixStart, iyStart, nxUse, nyUse, xstart, ystart, xsize, ysize,
                 xoffset, yoffset, false);
  xend = xstart + xsize - 1;
  yend = ystart + ysize - 1;
  needInterp = findInterpolatedZvals(cache, section, neededZ, otherZ, zfrac);
  if (needInterp) {
    zweights[1] = 1. - zfrac;
    zweights[2] = zfrac;
  }
  indZstart = needInterp ? 1 : 0;
  indZend = needInterp ? 3 : 1;

  // Get tiles needed for lower left and upper right
  getTileAndPositionInTile(cacheInd, xstart, ystart, startXtile, startYtile, xtmp, ytmp);
  getTileAndPositionInTile(cacheInd, xstart + xsize - 1, ystart + ysize - 1, endXtile,
                           endYtile, xtmp, ytmp);

  cacheSum = 0;
  for (loop = 0; loop < 2; loop++) {

    // second time through, revise the sample fraction upwards for number of pixels 
    // First, give up if nothing loaded; and just return if cache sum matches
    if (loop) {
      if (!numPix)
        return 1;
      if (cacheSum == oldCacheSum) {
        imodTrace('T', "Cache sum matches for %d tile%s", numTiles,
                  needInterp ? " pairs" : "s");
        return 0;
      }
      sample *= ((double)nxUse * nyUse) / B3DMAX(1, numPix);
      sample = B3DMIN(sample, 1.);
      imodTrace('t', "Cache sum for sec %d is %d, old %d", section, cacheSum,oldCacheSum);
    }

    // Loop on tiles in region
    for (ytile = startYtile; ytile <= endYtile; ytile++) {
      for (xtile = startXtile; xtile <= endXtile; xtile++) {
        
        // Find intersection with needed area
        findLoadLimits(cacheInd, xtile, ytile, true, tileXmin, tileXmax, tileYmin, 
                       tileYmax);
        useXmin = B3DMAX(tileXmin, xstart);
        useXmax = B3DMIN(tileXmax, xend);
        useYmin = B3DMAX(tileYmin, ystart);
        useYmax = B3DMIN(tileYmax, yend);
        nxTileUse = useXmax + 1 - useXmin;
        nyTileUse = useYmax + 1 - useYmin;
        
        for (indz = indZstart; indz < indZend; indz++)
          index[indz] = cache->tileIndex[xtile + (ytile + neededZ[indz] * 
                                                  cache->numYtiles) * cache->numXtiles];
        if ((!needInterp && index[0] >= 0) || (needInterp && index[1] >= 0 && 
                                               index[2] >= 0)) {
          if (loop) {
            for (indz = indZstart; indz < indZend; indz++) {
              slice = mTiles[index[indz]].slice;

              // Make line pointers here; cannot reuse vi line pointers since floating is
              // called by zap after image is gotten and before it is drawn
              lines = makeLinePointers(slice->data.b, slice->xsize, 
                                       slice->ysize, mVi->ushortStore ? 2 : 1);
              if (lines != NULL && 
                  !sampleMeanSD(lines, sampleType, slice->xsize, slice->ysize, 
                                sample, useXmin - tileXmin, useYmin - tileYmin,
                                nxTileUse, nyTileUse, &tmpMean, &tmpSD)) {
                
                // Accumulate sums and sums of squares, but weight the numbers by the
                // Z weighting
                nxtmp = B3DNINT(sample * nxTileUse * nyTileUse * zweights[indz]);
                xsum += tmpMean * nxtmp;
                nsum += nxtmp;
                xsqsum += tmpSD * tmpSD * (nxtmp - 1.) + nxtmp * tmpMean * tmpMean;
              }

              // If the percentile stretch is wanted too, then get it and add to arrays
              if (lines != NULL && scaleLo != NULL && scaleHi != NULL) {
                percentileStretch(lines,
                                  mVi->ushortStore ? SLICE_MODE_USHORT : SLICE_MODE_BYTE,
                                  slice->xsize, slice->ysize, sample,
                                  useXmin - tileXmin, useYmin - tileYmin,
                                  nxTileUse, nyTileUse, pctLo, pctHi, &loTemp, &hiTemp);
                loScales.push_back(loTemp);
                hiScales.push_back(hiTemp);
              }
              B3DFREE(lines);
            }
          } else {
            numPix += (double)nxTileUse * nyTileUse;
            numTiles++;
            for (indz = indZstart; indz < indZend; indz++)
              cacheSum += index[indz] + xtile + ytile + 1;
          }
        }
      }
    }
  }
  if (nsum < 30)
    return 1;
  sumsToAvgSD((float)xsum, (float)xsqsum, nsum, mean, SD);
  imodTrace('t', "Mean %.2f SD %.2f from %d tile%s cachesum %d", *mean, *SD, numTiles,
            needInterp ? " pairs" : "s", cacheSum);

  // For percentile stretch, take the median value
  if (scaleLo != NULL && scaleHi != NULL) {
    std::sort(loScales.begin(), loScales.end());
    nsum = loScales.size();
    if (nsum % 2)
      *scaleLo = loScales[nsum / 2];
    else
      *scaleLo = loScales[nsum / 2] + loScales[nsum / 2 - 1];
    std::sort(hiScales.begin(), hiScales.end());
    nsum = hiScales.size();
    if (nsum % 2)
      *scaleHi = hiScales[nsum / 2];
    else
      *scaleHi = hiScales[nsum / 2] + hiScales[nsum / 2 - 1];
  }
  return 0;
}

//////////////////////////////////////////////////////////////////////////////
// ROUTINES FOR COMPUTING COORDINATES, GETTING VALUES, GENERAL SUPPORT OF CACHE USE
//////////////////////////////////////////////////////////////////////////////

/*
 * Gets a single value at the given load coordinates from the base cache
 */
int PyramidCache::getValueFromBaseCache(int x, int y, int z)
{
  TileCache *cache = &mTileCaches[mBaseIndex];
  int xtile, ytile, xInTile, yInTile, tileInd, index;
  Islice *slice;

  // Can half the overlap be incorporated into first offset?
  xtile = (x + cache->firstXoffset - cache->xOverlap / 2) / cache->tileXdelta;
  B3DCLAMP(xtile, 0, cache->numXtiles - 1);
  ytile = (y + cache->firstYoffset - cache->yOverlap / 2) / cache->tileYdelta;
  B3DCLAMP(ytile, 0, cache->numYtiles - 1);
  tileInd = cache->tileIndex[xtile + (ytile + z * cache->numYtiles) * cache->numXtiles];
  if (tileInd < 0)
    return 0;
  xInTile = xtile ? 
    ((x + cache->firstXoffset - cache->xOverlap / 2) - xtile * cache->tileXdelta) : x;
  yInTile = ytile ? 
    ((y + cache->firstYoffset - cache->yOverlap / 2) - ytile * cache->tileYdelta) : y;
  slice = mTiles[tileInd].slice;
  index = yInTile * slice->xsize + xInTile;

  /* DNM: calling routine is responsible for limit checks */
  if (mVi->ushortStore)
    return slice->data.us[index];
  return slice->data.b[index];
}  

/*
 * Get the coordinates in the base file of the given loaded point
 */
int PyramidCache::getBaseFileCoords(int x, int y, int z, int &fileX, int &fileY, 
                                     int &fileZ)
{
  TileCache *cache = &mTileCaches[mBaseIndex];
  int xtile, ytile, xInTile, yInTile, tileXmin, tileXmax, tileYmin, tileYmax;
  getTileAndPositionInTile(mBaseIndex, x, y, xtile, ytile, xInTile, yInTile);
  findLoadLimits(mBaseIndex, xtile, ytile, false, tileXmin, tileXmax, tileYmin,
                 tileYmax);
  fileZ = z;
  if (adjustLoadLimitsForMont(cache, mVi->image->nx, mVi->image->ny, tileXmin, tileXmax,
                              tileYmin, tileYmax, fileZ))
    return 1;
  fileX = tileXmin + xInTile;
  fileY = tileYmin + yInTile;
  return 0;
}

/*
 * Find the best cache to use given a zoom.  zoomUpLimit is an absolute limit for
 * how much a non-base cache will be zoomed up.  zoomDownLimit is a lower limit for
 * how much a higher resolution cache will be zoomed down if there is a lower-resolution
 * one below the zoomUpLimit.
 */
int PyramidCache::pickBestCache(double zoom, float zoomUpLimit, float zoomDownLimit, 
                                int &scale)
{
  int ind, which;
  if (zoom >= 1.) {
    which = mBaseIndex;
  } else {

    // Find first cache with zoom greater than the inverse scale; this and the previous 
    //are the candidates
    for (ind = 1; ind < mNumCaches; ind++)
      if (zoom >= 1. / mTileCaches[mOrderedList[ind]].xyScale)
        break;
    if (ind >= mNumCaches) {
      which = mOrderedList[mNumCaches - 1];
    } else {

      // Use previous one if this one is above the limit for how much to zoom up a smaller
      // one or if previous one is above the limit for how much a bigger one should zoom 
      // down
      if (zoom * mTileCaches[mOrderedList[ind]].xyScale > zoomUpLimit ||
          zoom * mTileCaches[mOrderedList[ind - 1]].xyScale > zoomDownLimit)
        which = mOrderedList[ind - 1];
      else
        which = mOrderedList[ind];
    }
  }
  scale = mTileCaches[which].xyScale;
  return which;
}

/*
 * Given X and Y, loaded coordinates in the given cache, this computes the X and Y loaded
 * tile number and position with that tile
 */
void PyramidCache::getTileAndPositionInTile(int cacheInd, int x, int y, int &xtile,
                                            int &ytile, int &xInTile, int &yInTile)
{
  TileCache *cache = &mTileCaches[cacheInd];
  xtile = (x + cache->firstXoffset - cache->xOverlap / 2) / cache->tileXdelta;
  B3DCLAMP(xtile, 0, cache->numXtiles - 1);
  ytile = (y + cache->firstYoffset - cache->yOverlap / 2) / cache->tileYdelta;
  B3DCLAMP(ytile, 0, cache->numYtiles - 1);
  xInTile = xtile ? 
    ((x + cache->firstXoffset - cache->xOverlap / 2) - xtile * cache->tileXdelta) : x;
  yInTile = ytile ? 
    ((y + cache->firstYoffset - cache->yOverlap / 2) - ytile * cache->tileYdelta) : y;
}

/*
 * Determines the min and max loading coordinates for one file, in file coordinates of
 * that file, given the scale and size of that file and the limits for loading the
 * base file. Also revises the offset between this file and the base file to be the
 * offset between the first loaded pixel of each
 */
void PyramidCache::setLoadLimits(int scale, int size, int fullMin, int fullMax, 
                                 float &offset, int &minLoad, int &maxLoad)
{
  // get the closest corresponding pixel to the start and constrain to be positive
  minLoad = B3DNINT(fullMin / (float)scale - offset);
  minLoad = B3DMAX(0, minLoad);
  
  // Get the closest pixel to the end (right end of load pixel) and constrain
  maxLoad = B3DNINT((fullMax + 1.) / scale - offset) - 1;
  maxLoad = B3DMIN(size - 1, maxLoad);

  // Revise the offset to be between the first loaded pixel of subvol and base vol
  // and to be in base coordinates
  offset = scale * (minLoad + offset) - fullMin;
}

/*
 * Given the full image size, loading limits, and overlap, determines the starting
 * tile number and the offset to the beginning of the loaded data from the nominal start 
 * of the first loaded tile, and revises the number of tiles to be the number being
 * loaded.  If there is no tiling, this is the point at which the tile size is set to
 * the full image size and number of tiles to 1
 */
void PyramidCache::setTileGeometry(int fullSize, int minLoad, int maxLoad, int overlap, 
                                   int fileOffset, int &tileSize, int &numTiles,
                                   int &startTile, int &firstOffset)
{
  int endTile;
  if (!tileSize) {
    firstOffset = 0;
    numTiles = 1;
    tileSize = fullSize;
    startTile = 0;
    return;
  }
  
  // Loaded tiles start every (size - overlap) on boundaries that start at overlap / 2
  // for montage, fileOffset for TIFF in Y.
  // Get the actual file tile number for starting and ending coordinates
  startTile = (minLoad - overlap / 2 - fileOffset) / (tileSize - overlap);
  B3DCLAMP(startTile, 0, numTiles - 1);
  endTile = (maxLoad - overlap / 2 - fileOffset) / (tileSize - overlap);
  B3DCLAMP(endTile, 0, numTiles - 1);

  // Offset is the distance from the actual start of first tile to beginning of the load
  // Where actual start is the regular starting position minus file offset of first tile
  firstOffset = minLoad + fileOffset - startTile * (tileSize - overlap);

  // Revise numTiles to be the total in the range
  numTiles = endTile + 1 - startTile;
}

/*
 * Finds the min and max coordinates of a tile to be loaded,  The tile number and the
 * number of tiles must be must be in terms of the loaded tiles; startTile is used to
 * adjust both to the file tile numbering.  It returns file coordinates unless 
 * toLoadedCoords is true, in which case it returns coordinates in the loaded space.
 */
void PyramidCache::findLoadLimits(int size, int overlap, int tile, int numTiles, 
                                  int startTile, int fileOffset, int fullMin, int fullMax,
                                  bool toLoadedCoords, int &loadMin, int &loadMax)
{
  tile += startTile;
  numTiles += startTile;
  if (!tile) {
    loadMax = size - (overlap - overlap / 2);
    loadMin = 0;
  } else {
    loadMin = tile * (size - overlap) + overlap / 2;
    if (tile < numTiles - 1)
      loadMax = loadMin + size - overlap;
    else
      loadMax = loadMin + size - overlap/2;
  }
  loadMin = B3DMAX(loadMin - fileOffset, fullMin);
  loadMax = B3DMIN(loadMax - 1 - fileOffset, fullMax);
  if (toLoadedCoords) {
    loadMin -= fullMin;
    loadMax -= fullMin;
  }
}

/*
 * Version that finds the limits of a tile in the given cache, in file or loaded 
 * coordinates, using the above function
 */
void PyramidCache::findLoadLimits(int cacheInd, int xtile, int ytile, bool toLoadedCoords,
                                  int &loadXmin, int &loadXmax, int &loadYmin,
                                  int &loadYmax)
{
  TileCache *cache = &mTileCaches[cacheInd];
  findLoadLimits(cache->xTileSize, cache->xOverlap, xtile,
                   cache->numXtiles, cache->startXtile, 0,
                   cache->minXload, cache->maxXload, toLoadedCoords, loadXmin, loadXmax);
  findLoadLimits(cache->yTileSize, cache->yOverlap, ytile,
                 cache->numYtiles, cache->startYtile, cache->fileYtileOffset,
                 cache->minYload, cache->maxYload, toLoadedCoords, loadYmin, loadYmax);
}

/*
 * Given the starting coordinate and size desired in base image loaded coordinates,
 * and the size, scale, and offset of the cache image being used, this computes the
 * starting loaded coordinate and size in the cache image, plus the error in the returned
 * starting coordinate versus the requested one, in base image coordinates.
 */
void PyramidCache::scaledAreaSize(int scale, int fullStart, int sizeIn, int fullSize,
                                  int offset, int &start, int &sizeOut,
                                  float &errorOffset, bool inside)
{
  int end;
  start = fullStart;
  sizeOut = sizeIn;
  errorOffset = 0;
  if (scale == 1)
    return;
  if (inside) {
    start = (int)ceil(((double)fullStart - offset) / scale);
    end = (int)floor(((double)fullStart + sizeIn - offset) / scale);
  } else {
    start = (int)floor(((double)fullStart - offset) / scale);
    end = (int)ceil(((double)fullStart + sizeIn - offset) / scale);
  }
  start = B3DMAX(0, start);
  B3DCLAMP(end, start + 1, fullSize);
  sizeOut = end - start;
  while (inside && sizeOut * scale > sizeIn && sizeOut > 1)
    sizeOut--;
  errorOffset = start * scale + offset - fullStart;
}

/*
 * Takes a cache index and X and Y start and size in base image coordinates and 
 * returns the start, size, and offsets for X and Y using the previous function
 */
void PyramidCache::scaledAreaSize(int cacheInd, int baseXstart, int baseYstart, 
                                  int baseXsize, int baseYsize, 
                                  int &xstart, int &ystart, int &xsizeOut, int &ysizeOut,
                                  float &xErrorOffset, float &yErrorOffset, bool inside)
{
  TileCache *cache = &mTileCaches[cacheInd];
  scaledAreaSize(cache->xyScale, baseXstart, baseXsize, cache->fullXsize, cache->xOffset,
                 xstart, xsizeOut, xErrorOffset, inside);
  scaledAreaSize(cache->xyScale, baseYstart, baseYsize, cache->fullYsize, cache->yOffset,
                 ystart, ysizeOut, yErrorOffset, inside);
}

/*
 * For a given cache index and two Z values in full base image coordinates, returns
 * nearest integer Z values to use and scale and offset needed to get from cache Z
 * values back to real Z value.
 */
void PyramidCache::scaledRangeInZ(int cacheInd, int baseZstart, int baseZend, int &zstart,
                                  int &zend, int &scale, float &offset)
{
  TileCache *cache = &mTileCaches[cacheInd];
  double zreal;
  scale = cache->zScale;
  zreal = (baseZstart + 0.5 + mVi->li->zmin) / scale - 0.5 - cache->zOffset -
    cache->minZload;
  B3DCLAMP(zreal, 0, cache->loadZsize - 1);
  zstart = B3DNINT(zreal);
  offset = baseZstart - scale * zreal;
  zreal = (baseZend + 0.5 + mVi->li->zmin) / scale - 0.5 - cache->zOffset -
    cache->minZload;
  B3DCLAMP(zreal, 0, cache->loadZsize - 1);
  zend = B3DNINT(zreal);
}

/*
 * Determines the cache loaded Z values from which a given section is interpolated and
 * returns true if there is indeed interpolation needed
 */
bool PyramidCache::findInterpolatedZvals(TileCache *cache, int section, int needLoadZ[2],
                                         int otherZ[3], float &zfrac)
{
  double zreal;
  int zint;
  float minInterpFrac = 0.1;
  needLoadZ[0] = section;
  if (cache->zScale == 1)
    return false;

  // This treats pixels as having thickness.  We are sampling the middle of this 
  // full Z value, hence section + 0.5.  Division brings it to the equivalent floating
  // Z value in a binned volume, which should have the same Z origin because pixels
  // start at the same place in Z.  Then subtract 0.5 to get back to slice numbers,
  // and subtract the Z offset for an origin shift (if sampling instead of binning)
  zreal = (section + 0.5 + mVi->li->zmin) / cache->zScale - 0.5 - cache->zOffset;
  B3DCLAMP(zreal, cache->minZload, cache->maxZload);
  
  // Get it back to load Z coordinates
  zreal -= cache->minZload;
  zint = (int)zreal;
  zfrac = zreal - zint;
  needLoadZ[1] = zint;
  needLoadZ[2] = zint + 1;
  otherZ[1] = zint + 1;
  otherZ[2] = zint;
  if (zfrac > 1. - minInterpFrac) {
    needLoadZ[0] = zint + 1;
    return false;
  }
  if (zfrac < minInterpFrac) {
    needLoadZ[0] = zint;
    return false;
  }
  return true;
}

/*
 * Computes the optimal size of the buffer on one axis, given the needed starting 
 * coordinate and number of pixels, in loaded coordinates of the given cache, and the
 * total size being loaded for that file.
 */
void PyramidCache::optimalBufferSize(int start, int sizeOut, int loadSize, 
                                     int &bufStart, size_t &bufSize)
{
  double extraFac = 0.5, extraBaseSize = 1000., extraPower = 0.5;
  int extra, end;
  extra = B3DNINT(extraFac * sizeOut * 
                  pow(extraBaseSize / B3DMAX(sizeOut, extraBaseSize), extraPower));
  bufStart = B3DMAX(0, start - extra);
  end = B3DMIN(loadSize - 1, start + sizeOut + extra - 1);
  bufSize = end + 1 - bufStart;
}

/*
 * Tests whether a given zoom displayed at the given window size will require loading
 * more than 50 MB (mbLoadLim) from file.
 */
bool PyramidCache::zoomRequiresBigLoad(double zoom, int winXsize, int winYsize)
{
  int cacheInd, scale, sizeXout, xstart, bufXstart, startXtile, endXtile;
  int sizeYout, ystart, bufYstart, startYtile, endYtile, xtmp, ytmp;
  double winZoom, numPix;
  size_t bufXsize, bufYsize;
  TileCache *cache;
  ImodImageFile *image;
  int mbLoadLim = 50;
  cacheInd = pickBestCache(zoom, mZoomUpLimit, mZoomDownLimit, scale);
  cache = &mTileCaches[cacheInd];
  winZoom = zoom * scale;
  sizeXout = B3DMIN((int)(winXsize / winZoom), cache->loadXsize);
  xstart = (cache->loadXsize - sizeXout) / 2;
  optimalBufferSize(xstart, sizeXout, cache->loadXsize, bufXstart, bufXsize);
  sizeYout = B3DMIN((int)(winYsize / winZoom), cache->loadYsize);
  ystart = (cache->loadYsize - sizeYout) / 2;
  optimalBufferSize(ystart, sizeYout, cache->loadYsize, bufYstart, bufYsize);
  getTileAndPositionInTile(cacheInd, bufXstart, bufYstart, startXtile, startYtile,
                           xtmp, ytmp);
  getTileAndPositionInTile(cacheInd, bufXstart + bufXsize - 1, bufYstart + bufYsize - 1,
                           endXtile, endYtile, xtmp, ytmp);
  numPix = ((double)cache->xTileSize * cache->yTileSize) * (endXtile + 1 - startXtile) * 
    (endYtile + 1 - startYtile);
  imodTrace('t', "zoom %f  xtile %d %d  ytile %d %d  numpix %f", zoom, startXtile,
            endXtile, startYtile, endYtile, numPix);
  if (mNumCaches > 1)
    image = &mVi->imageList[cacheInd];
  else
    image = mVi->image;
  return numPix > 1024. * 1024 * mbLoadLim / ivwGetPixelBytes(image->mode);
}

/*
 * Return the number of tiles in X and Y and planes in Z for the given cache
 */
int PyramidCache::getCacheTileNumbers(int cacheInd, int &numXtiles, int &numYtiles,
                                      int &nz)
{
  if (cacheInd < 0 || cacheInd >= mNumCaches)
    return 1;
  TileCache *cache = &mTileCaches[cacheInd];
  numXtiles = cache->numXtiles;
  numYtiles = cache->numYtiles;
  nz = cache->loadZsize;
  return 0;
}


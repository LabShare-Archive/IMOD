/*   pyramidcache.h  -  declarations for pyramidcache.cpp
 *
 *  $Id$
 */                                                                           
#ifndef PYRAMIDCACHE_H
#define PYRAMIDCACHE_H

#include "imod.h"
#include <qstringlist.h>
#include <vector>
#include <map>
#include <stack>
#include <deque>

#define FAKE_STRIP_PIXELS 1000000
#define DEFAULT_TILE_CACHE_LIMIT 20000

// Structure for one tile, containing an Islice
typedef struct
{
  int     cacheInd;      // Index of cache this tile belongs to
  int     section;       // Section number in loaded Z coordinates for this file
  int     xTileInd;      // X and Y index of tile, in loaded tile indexes
  int     yTileInd;  
  double  usedAtCount;   // Count at which tile was last used
  Islice *slice;         // The tile data and size
} CacheSlice;

// Structure for one tile cache
typedef struct
{
  int xTileSize, yTileSize;             // Size of tiles in X and Y
  int xOverlap, yOverlap;               // Overlap between tiles in X and Y
  int numXtiles, numYtiles;             // Number of tiles in X and Y
  int xyScale;                          // Amount scaled down from base cache in X/Y (>= 1)
  int zScale;                           // Amount scaled down from base cache in Z
  int *tileIndex;                       // Index from tile number in X, Y, Z to slices
  int fullXsize, fullYsize, fullZsize;  // Size of data volume in X, Y, Z
  int plistSize;                        // Number of piece coordinates
  int *pcoords;                         // Piece coordinates
  // Amount the loaded subset of volume is indented relative to loaded base volume, 
  // in coordinates of this volume
  float xOffset, yOffset, zOffset;
  // Starting and ending file coordinates that get loaded from this volume
  int minXload, maxXload, minYload, maxYload, minZload, maxZload;
  int loadXsize, loadYsize, loadZsize;  // Size of data volume being loaded in X, Y, Z
  int tileXdelta, tileYdelta;           // Spacing between tiles: size - overlap          
  // Offset from start of first tile in file to start of load; used to get back and
  // forth between tiles and loaded coordinates
  int firstXoffset, firstYoffset;       
  int fileYtileOffset;                  // Amount missing from first Y strip/tile in file
  int startXtile, startYtile;           // Number of first actual tile in file loaded
} TileCache;

// Structure for a request to load a tile and/or copy it to the buffer
typedef struct
{
  int cacheInd;                // Cache that tile belongs to
  int section;                 // Section number in loaded volume
  int xTileInd;                // Tile index in X and Y in loaded volume
  int yTileInd;
  int indInCache;              // Index in cache, filled in by load
  int bufInd;                  // Buffer number to copy to
  int numXcopy, numYcopy;      // Number of pixels to copy in X and Y if > 0
  int fromXstart, fromYstart;  // Starting pixel in X and Y in tile
  int toXstart, toYstart;      // Starting pixel to copy to in buffer
} LoadTileRequest;

// Structure for line segment to provide fast access
typedef struct
{
  int XorY, YorZ;       // Starting coordinate
  int length;           // Number of points in segment
  int stride;           // stride between points
  unsigned char *line;  // Pointer to data
} FastSegment;

class PyramidCache
{
 public:
  PyramidCache(ImodView *vi, QStringList &plFileNames, int frames, bool anyImageFail);
  ~PyramidCache() {};
  void setupStripOrTileCache();
  getMember(int, BaseIndex);
  getMember(int, BufCacheInd);
  setMember(double, VMpixels);
  void adjustForBinning();
  void initializeCaches();
  unsigned char **getSectionArea(int section, int fullXstart, int fullYstart, 
                                 int xsizeIn, int ysizeIn, double zoom, bool asyncLoad,
                                 int &xsizeOut, int &ysizeOut, float &xoffset,
                                 float &yoffset, int &scale, int &status);

  int getValueFromBaseCache(int x, int y, int z);
  int loadBaseTileWithPoint(int x, int y, int z);
  int loadTilesContainingArea(int cacheInd, int baseXstart, int baseYstart,
                              int baseXsize, int baseYsize, int z);
  int loadRequestedTiles(int asyncLoad);
  int getCacheTileNumbers(int cacheInd, int &numXtiles, int &numYtiles, int &nz);
  int indexSizeForCache(int cacheInd, int &numXtiles, int &numYtiles);
  void setupFastAccess(int cacheInd, unsigned char **imdata, int *vmdataxsize, 
                       int &cacheSum, int &tileXdelta, int &tileYdelta,
                       int &firstXoffset, int &firstYoffset);
  int fastPlaneAccess(int cacheInd, int plane, int axis, 
                      std::vector<FastSegment> &segments, std::vector<int> &startInds);
  int pickBestCache(double zoom, float zoomUpLimit, float zoomDownLimit, int &scale);
  void scaledAreaSize(int cacheInd, int baseXstart, int baseYstart, 
                      int baseXsize, int baseYsize, 
                      int &xstart, int &ystart, int &xsizeOut, int &ysizeOut,
                      float &xErrorOffset, float &yErrorOffset, bool inside);
  void scaledRangeInZ(int cacheInd, int baseZstart, int baseZend, int &zstart, int &zend, 
                      int &scale, float &offset);
  int loadedCacheSum(int cacheInd);
  int loadedMeanSD(int cacheInd, int section, float sample, int ixStart, int iyStart, 
                   int nxUse, int nyUse, float *mean, float *SD,
                   int &cacheSum, int oldCacheSum, float pctLo = 0., float pctHi = 0.,
                   float *scaleLo = NULL, float *scaleHi = NULL);
  void fillCacheForArea(int section, int source);
  int getBaseFileCoords(int x, int y, int z, int &fileX, int &fileY, int &fileZ);
  bool zoomRequiresBigLoad(double zoom, int winXsize, int winYsize);
  unsigned char **getFullSection(int z);
  void freeFullSection();

 private:
  int setupTileCache(ImodImageFile *image, IloadInfo *li, TileCache *cache);
  void pyramidError(int err);
  void setLoadLimits(int scale, int size, int fullMin, int fullMax,
                     float &offset, int &minLoad, int &maxLoad);
  void setTileGeometry(int fullSize, int minLoad, int maxLoad, int overlap, 
                       int fileOffset, int &tileSize, int &numTiles, int &startTile,
                       int &firstOffset);
  void freeTile(CacheSlice *tile, int freeInd, bool haveUserIter);
  void getTileAndPositionInTile(int cacheInd, int x, int y, int &xtile, int &ytile,
                               int &xInTile, int &yInTile);
  void findLoadLimits(int size, int overlap, int tile, int numTiles, int startTile,
                      int fileOffset, int fullMin, int fullMax, bool toLoadedCoords,
                      int &loadMin, int &loadMax);
  void findLoadLimits(int cacheInd, int xtile, int ytile, bool toLoadedCoords,
                      int &loadXmin, int &loadXmax, int &loadYmin, int &loadYmax);
  int adjustLoadLimitsForMont(TileCache *cache, int nx, int ny, int &llx,
                              int &urx, int &lly, int &ury, int &zval);
  int freeForNeededPixels(int cacheInd, int zval, int otherZ, double numPixels,
                          int numLoops);
  void scaledAreaSize(int scale, int fullStart, int sizeIn, int fullSize, int offset,
                      int &start, int &sizeOut, float &errorOffset, bool inside);
  void optimalBufferSize(int start, int sizeOut, int loadSize, int &bufStart,
                         size_t &bufSize);
  void copyTileIntoBuffer(LoadTileRequest &request, bool manageUseCount);
  int makeNewCacheItem(int cacheNum, int xtile, int ytile, int zval, int otherZ,
                       int xsize, int ysize);
  bool findInterpolatedZvals(TileCache *cache, int section, int needLoadZ[3], 
                             int otherZ[3], float &zfrac);
  bool copyOrQueueTile(int cacheInd, LoadTileRequest &request, int xtile, 
                       int ytile, int zsec, int bufInd, int bufXstart,
                       int bufXsize, int bufYstart, int bufYsize);

  ImodView *mVi;
  int mBaseIndex;                   // Index of "base" cache, one at highest scale
  int mNumCaches;                   // Number of caches
  int mPyrInd;                      // File index used for error messages
  std::vector<CacheSlice> mTiles;   // Array of tiles from all caches
  double mVMpixels;                 // Limit on number of pixels to be allocated
  double mTotalPixels;              // Current total pixels allocated
  double mUseCount;                 // Use counter for tiles
  std::map<double,int> mUseMap;     // Map from use counter to tile (sorted)
  std::stack<int,std::vector<int> > mFreeTiles;  // Stack of free array elements
  std::deque<LoadTileRequest> mRequests;  // Queue of load requests
  TileCache *mTileCaches;           // Array of caches
  int *mOrderedList;                // Array of cache numbers in order by scale
  unsigned char *mSliceBufs[3];     // Buffers for final display and Z interpolation
  int mBufXstart, mBufYstart;       // Starting loaded cache coordinates of buffer
  size_t mBufXsize, mBufYsize;  // currently used size of buffer(s) in loaded cache coords
  size_t mBufTotSize;           // Allocated total # of pixels
  int mBufSection;             // Full image section number in slice buffer
  int mBufLoadZ[3];            // Load section numbers in slice and interpolation buffers
  int mBufCacheInd;            // Which cache the buffer is currently for
  int mLastRequestSize;       // Size of remaining request last time getSectionArea called
  float mZoomUpLimit;         // Limits for zoom up and zoom down in getSectionArea
  float mZoomDownLimit;
  unsigned char *mFullSecBuf;    // Buffer for loading a full base section
  unsigned char **mFullSecPtrs;  // Line pointers for the full base section
  int mFullSecZ;                 // Z value loaded
};

#endif

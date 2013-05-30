/*  warpfiles.c - functions using files and structures of warp transforms
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "warpfiles.h"
#include "b3dutil.h"
#include "parse_params.h"
#include "ilist.h"
#include "nn.h"
/*#include "nan.h" */
#include "config.h"

#define DEFAULT_PERCENTILE 0.5
#define DEFAULT_FACTOR  0.15
/*#define DO_DUMPS */

typedef struct {
  int nx, ny;
  int numFrames;
  int binning;
  float pixelSize;
  int flags;
  Ilist *warpings;
  int inUse;
} WarpFile;

typedef struct {
  float xform[6];
  int nxGrid, nyGrid;
  float xStart, yStart;
  float xInterval, yInterval;
  float *xVector;
  float *yVector;
  int nControl;
  float *xControl;
  float *yControl;
  int maxVectors;
} Warping;

static WarpFile *sWarpFiles = NULL;
static int sNumWarpFiles = 0;
static int sCurFileInd = -1;
static WarpFile *sCurWarpFile = NULL;
static double *sGridx = NULL;
static double *sGridy = NULL;
static char *sSolved = NULL;
static point *sDpoints = NULL;
static delaunay *sDelau = NULL;
static nnai *sNninterp = NULL;
static int sLastNumCont = 0;
static int sLastNxGrid = 0;
static int sLastNyGrid = 0;
static float sLastXStart = -1.;
static float sLastYStart = -1.;
static float sLastXinterval = 0.;
static float sLastYinterval = 0.;
static int sLastXdim = 0;
static int sLastZforSpacing = -1;
static float sLastPercentile;
static float sLastSpacing;
static int sTimeOutput = 0;
static double sHeightBaseCrit = 0.2;
static double sAreaFractionCrit = 0.1;
static int sMinNumForPruning = 2;

static int readLineOfValues(FILE *fp, char *line, int limit, void *values, int floats, 
                            int *numToGet, int maxVals);
static void initWarping(Warping *warp);
static int addWarpFile();
static int addWarpingsIfNeeded(int iz);
static void deleteWarpFile(WarpFile *warpFile);
static void freeStaticArrays();
static int pointsMatchStatArray(Warping *warp);
#ifdef DO_DUMPS
static void dumpGrid(float *dxGrid, float *dyGrid, char *solved, int xdim,
                     int nxGrid, int nyGrid, float xStart, float yStart, 
                     float xInterval, float yInterval, int doSolved, 
                     const char *fname);
static void dumpControls(float *xControl, float *yControl, float *xVector, float *yVector,
                         int nControl, const char *fname);
#endif


/* Initialize a warp */
static void initWarping(Warping *warp)
{
  xfUnit(warp->xform, 1.0, 2);
  warp->nxGrid = warp->nyGrid = warp->nControl = 0;
  warp->xVector = warp->yVector = warp->xControl = warp->yControl = NULL;
  warp->maxVectors = 0;
}

/* Add a new warp file to the array */
static int addWarpFile()
{
  WarpFile *wfile;
  int index = -1, i;
  
  /* Search for a free warp file in array */
  for (i = 0; i < sNumWarpFiles; i++) {
    if (!sWarpFiles[i].inUse) {
      index = i;
      break;
    }
  }

  if (index < 0) {

    /* Allocate just one at a time when needed */
    if (!sNumWarpFiles)
      sWarpFiles = B3DMALLOC(WarpFile, 1);
    else
      B3DREALLOC(sWarpFiles, WarpFile, sNumWarpFiles + 1);
    if (PipMemoryError(sWarpFiles, "addWarpFile"))
      return -1;
    index = sNumWarpFiles++;
  }

  wfile = &sWarpFiles[index];
  wfile->numFrames = 0;
  wfile->warpings = ilistNew(sizeof(Warping), 4);
  wfile->inUse = 1;
  if (PipMemoryError(wfile->warpings, "addWarpFile"))
    return -1;
  return index;
}

/*!
 * Initialize for a new warp file based on image dimensions [nx], [ny] at the given
 * [binning], with the pixel size in Angstroms in [pixelSize] and [flags] containing the
 * sum of 1 for inverse warping and 2 for control points.  Returns the index of the new
 * file, or -1 for a memory error.
 */
int newWarpFile(int nx, int ny, int binning, float pixelSize, int flags)
{
  int err;
  if ((err = addWarpFile()) < 0)
    return err;
  setCurrentWarpFile(err);
  sCurWarpFile->nx = nx;
  sCurWarpFile->ny = ny;
  sCurWarpFile->binning = binning;
  sCurWarpFile->pixelSize = pixelSize;
  sCurWarpFile->flags = flags;
  return err;
}

/*!
 * Reads in a warping file whose name is in [filename].  Returns the X and Y sizes given
 * in the file in [nx] and [ny], the number of sections of warpings in [nz], the binning
 * factor into [binning], the pixel size in Angstroms into [pixelSize], the original file
 * version into [version], and the sum of 1 for inverse warpings and 2 for control points
 * into [flags].  The return value is the index of the warp file, or
 * -1 for failure to open the file, -2 for an error reading values from the file, -3 for
 * the wrong number of values on the first line or nothing following that line, -4 for a
 * version number out of range or an error reading values or the wrong number of values 
 * on the header line, -5 for [nz], [ny], [nz], or [binning] out of range, -6 for memory
 * errors, or -7 for an error or inconsistency reading warpings.  [version] will be -1
 * for errors -1, -2, or -3, or 0 if the error is -3 and there are 6 values on the first
 * line, which is a signal that the file contains ordinary transforms.
 */
int readWarpFile(char *filename, int *nx, int *ny, int *nz, int *binning,
                 float *pixelSize, int *version, int *flags)
{
#define MAX_LINE 1024
#define MAX_VALS 50
  FILE *fp;
  char line[MAX_LINE];
  float values[MAX_VALS];
  int numVals, err, i, iz, numRead;
  Warping *warp;

  *version = -1;
  fp = fopen(filename, "r");
  if (!fp)
    return -1;
  numVals = 0;
  err = readLineOfValues(fp, line, MAX_LINE, values, 1, &numVals, MAX_VALS);
  if (err < 0) {
    fclose(fp);
    if (err != -2)
      return -2;

    /* An empty file gives -2 and this is a simple file */
    *version = 0;
    return -3;
  }
  if (numVals == 6)
    *version = 0;
  if (numVals != 1 || err > 0) {
    fclose(fp);
    return -3;
  }
  *version = B3DNINT(values[0]);
  if (*version < 1 || *version > 3) {
    fclose(fp);
    return -4;
  }

  numVals = 0;
  err = readLineOfValues(fp, line, MAX_LINE, values, 1, &numVals, MAX_VALS);
  if (numVals != *version + 3) {
    fclose(fp);
    return -4;
  }
  i = 0;
  *nx = B3DNINT(values[i++]);
  *ny = B3DNINT(values[i++]);
  *nz = 1;
  if (*version > 1)
    *nz = B3DNINT(values[i++]);
  *binning = B3DNINT(values[i++]);
  *pixelSize = values[i++];
  *flags = WARP_INVERSE;
  if (*version == 3)
    *flags = B3DNINT(values[i++]);
  if (*nx <= 0 || *ny <= 0 || *nz <= 0 || *binning <= 0) {
    fclose(fp);
    return -5;
  }
  
  /* Ready to create a structure */
  err = newWarpFile(*nx, *ny, *binning, *pixelSize, *flags);
  if (err < 0) {
    fclose(fp);
    return -6;
  }
  
  /* Loop on the frames */
  for (iz = 0; iz < *nz; iz++) {
    if (addWarpingsIfNeeded(iz)) {
      err = -6;
      break;
    }
    warp = ilistItem(sCurWarpFile->warpings, iz);
    numVals = 0;
    err = readLineOfValues(fp, line, MAX_LINE, values, 1, &numVals, MAX_VALS);
    if (err)
      break;

    /* Read the header line and allocate arrays */
    if (!(*flags & WARP_CONTROL_PTS)) {
      warp->nxGrid = B3DNINT(values[2]);
      warp->nyGrid = B3DNINT(values[5]);
      warp->xStart  = values[0];
      warp->yStart  = values[3];
      warp->xInterval = values[1];
      warp->yInterval = values[4];
      warp->xVector = B3DMALLOC(float, warp->nxGrid * warp->nyGrid);
      warp->yVector = B3DMALLOC(float, warp->nxGrid * warp->nyGrid);
      if (PipMemoryError(warp->xVector, "readWarpFile") || 
          PipMemoryError(warp->yVector, "readWarpFile"))
        return -6;
      warp->maxVectors = warp->nxGrid * warp->nyGrid;
    } else {
      warp->nControl = B3DNINT(values[0]);
      if (warp->nControl > 0) {
        warp->xVector = B3DMALLOC(float, warp->nControl);
        warp->yVector = B3DMALLOC(float, warp->nControl);
        warp->xControl = B3DMALLOC(float, warp->nControl);
        warp->yControl = B3DMALLOC(float, warp->nControl);
        if (PipMemoryError(warp->xVector, "readWarpFile") || 
            PipMemoryError(warp->yVector, "readWarpFile") ||
            PipMemoryError(warp->xControl, "readWarpFile") || 
            PipMemoryError(warp->yControl, "readWarpFile"))
          return -6;
        warp->maxVectors = warp->nControl;
      }
    }

    /* All version 3 files have a transform so read it */
    if (*version > 2) {
      numVals = 0;
      err = readLineOfValues(fp, line, MAX_LINE, values, 1, &numVals, MAX_VALS);
      if (err < 0 || warp->nControl < 0 || numVals != 6) {
        if (err >= 0)
          err = -7;
        break;
      }
      warp->xform[0] = values[0];
      warp->xform[2] = values[1];
      warp->xform[1] = values[2];
      warp->xform[3] = values[3];
      warp->xform[4] = values[4];
      warp->xform[5] = values[5];
    }

    /* Read the grid */
    err = 0;
    if (!(*flags & WARP_CONTROL_PTS)) {
      numRead = 0;
      while (numRead < warp->nxGrid * warp->nyGrid && !err) {
        numVals = 0;
        err = readLineOfValues(fp, line, MAX_LINE, values, 1, &numVals, MAX_VALS);
        if (err < 0)
          break;
        if (numVals % 2) {
          err = -7;
          break;
        }
        for (i = 0; i < numVals; i += 2) {
          warp->xVector[numRead] = values[i];
          warp->yVector[numRead++] = values[i+1];
        }
      }
      if (numRead < warp->nxGrid * warp->nyGrid)
        break;
    } else {

      /* OR read the control points */
      for (i = 0; i < warp->nControl; i++) {
        if (err)
          break;
        numVals = 4;
        err = readLineOfValues(fp, line, MAX_LINE, values, 1, &numVals, MAX_VALS);
        if (err < 0)
          break;
        warp->xControl[i] = values[0];
        warp->yControl[i] = values[1];
        warp->xVector[i] = values[2];
        warp->yVector[i] = values[3];
      }
      if (i < warp->nControl)
        break;
    }
    err = 0;
  }
  if (err) {
    if (err > 0)
      err = -7;
    deleteWarpFile(sCurWarpFile);
    setCurrentWarpFile(-1);
  }
  fclose(fp);
  return sCurFileInd;
}

/*!
 * Writes the current warping file as a version 3 file to [filename].  Backs up an
 * existing file to filename~ unless [skipBackup] is non-zero.  Returns -1 for
 * no filename or failure to open the file, or 1 for an error backing up an existing file.
 */
int writeWarpFile(const char *filename, int skipBackup)
{
  int i,j,backerr = 0, iz;
  FILE *fp;
  Warping *warp;
  if (!sCurWarpFile)
    return -1;
  if (!skipBackup)
    backerr = imodBackupFile(filename);
  fp = fopen(filename, "w");
  if (!fp)
    return -1;
  fprintf(fp, "3\n");
  fprintf(fp, "%d %d %d %d %f %d\n", sCurWarpFile->nx, sCurWarpFile->ny,
          sCurWarpFile->numFrames, sCurWarpFile->binning, sCurWarpFile->pixelSize,
          sCurWarpFile->flags);
  for (iz = 0; iz < sCurWarpFile->numFrames; iz++) {
    warp = (Warping *)ilistItem(sCurWarpFile->warpings, iz);
    if (sCurWarpFile->flags & WARP_CONTROL_PTS)
      fprintf(fp, "%d\n", warp->nControl);
    else
      fprintf(fp, "%f  %f  %d  %f  %f  %d\n", warp->xStart, warp->xInterval, 
              warp->nxGrid, warp->yStart, warp->yInterval, warp->nyGrid);
    fprintf(fp, "%.6f  %.6f  %.6f  %.6f  %.3f %.3f\n", warp->xform[0], warp->xform[2],
            warp->xform[1], warp->xform[3], warp->xform[4], warp->xform[5]);

    if (sCurWarpFile->flags & WARP_CONTROL_PTS) {
      for (i = 0; i < warp->nControl; i++)
        fprintf(fp, "%.3f %.3f %.3f %.3f\n", warp->xControl[i], warp->yControl[i], 
                warp->xVector[i], warp->yVector[i]);
    } else {
      for (j = 0; j < warp->nyGrid; j++) {
        for (i = 0; i < warp->nxGrid; i++) {
          fprintf(fp, "  %.3f  %.3f", warp->xVector[i + j * warp->nxGrid],
                 warp->yVector[i + j * warp->nxGrid]);
          if (i % 4 == 3 || i == warp->nxGrid - 1)
            fprintf(fp, "\n");
        }
      }
    }
  }
  fclose(fp);
  return backerr;
}

/* Add additional initialized warp structures to the current file so that all sections
   through iz exist. */
static int addWarpingsIfNeeded(int iz)
{
  Warping warp;
  int i;
  if (!sCurWarpFile || iz < 0)
    return 1;
  if (iz < sCurWarpFile->numFrames)
    return 0;
  initWarping(&warp);
  for (i = sCurWarpFile->numFrames; i <= iz + 1; i++)
    if (ilistAppend(sCurWarpFile->warpings, &warp))
      return 1;
  sCurWarpFile->numFrames = iz + 1;
  return 0;
}

/*!
 * Set the index of the current warping file to [index].  Returns -1 for error.
 */
int setCurrentWarpFile(int index)
{
  sCurWarpFile = NULL;
  sCurFileInd = -1;
  if (index < 0 || index >= sNumWarpFiles)
    return -1;
  sCurFileInd = index;
  sCurWarpFile = &sWarpFiles[sCurFileInd];
  return 0;
}

/*!
 * Deletes all data for the warp file at [index] and marks it as unused.  Returns 1 for
 * index out of range or 2 for a warp file not in use.
 */
int clearWarpFile(int index)
{
  if (index < 0 || index >= sNumWarpFiles)
    return 1;
  if (!sWarpFiles[index].inUse)
    return 2;
  deleteWarpFile(&sWarpFiles[index]);
  if (sCurFileInd == index) {
    sCurFileInd = -1;
    sCurWarpFile = NULL;
  }
  return 0;
}

/*!
 * Deletes all warp file data and resets pointers and indices; also calls 
 * @@extrapolateDone@.
 */
void warpFilesDone()
{
  int i;
  for (i = 0; i < sNumWarpFiles; i++) {
    deleteWarpFile(&sWarpFiles[i]);
    ilistDelete(sWarpFiles[i].warpings);
  }
  B3DFREE(sWarpFiles);
  freeStaticArrays();
  sCurWarpFile = NULL;
  sNumWarpFiles = 0;
  sCurFileInd = -1;
  extrapolateDone();
}

/* Deletes one warp file given in [warpFile]. */
static void deleteWarpFile(WarpFile *warpFile)
{
  int iz;
  Warping *warp;
  for (iz = 0; iz < warpFile->numFrames; iz++) {
    warp = ilistItem(warpFile->warpings, iz);
    B3DFREE(warp->xVector);
    B3DFREE(warp->yVector);
    B3DFREE(warp->xControl);
    B3DFREE(warp->yControl);
  }
  warpFile->numFrames = 0;
  warpFile->inUse = 0;
  ilistTruncate(warpFile->warpings, 0);
}

/*!
 * For the current warp file, returns the image size in [nx] and [ny], number of sections
 * in [nz], and 1 in [ifControl] if it has control points.  Return value is 1 if there is
 * no current warp file.
 */
int getWarpFileSize(int *nx, int *ny, int *nz, int *ifControl)
{
  if (!sCurWarpFile)
    return 1;
  *nx = sCurWarpFile->nx;
  *ny = sCurWarpFile->ny;
  *nz = sCurWarpFile->numFrames;
  *ifControl = (sCurWarpFile->flags & WARP_CONTROL_PTS) ? 1 : 0;
  return 0;
}

/*!
 * Sets the linear transform for section [iz] in the current warp file to [xform]; [rows]
 * specifies the number of rows (2 or 3) in the [xform] array.
 * When calling from Fortran, omit [rows]; it is assumed to be 2.
 */
int setLinearTransform(int iz, float *xform, int rows) 
{
  Warping *warp;
  if (addWarpingsIfNeeded(iz))
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  xfCopy(xform, rows, warp->xform, 2);
  return 0;
}

/*!
 * Sets a warping grid for section [iz] in the current warp file.  The grid consists of
 * [nxGrid] by [nyGrid] points, starting at [xStart], [yStart] and at intervals of
 * [xInterval], [yInterval].  The vectors are in 2D arrays [dxGrid], [dyGrid], and [xdim]
 * is the X dimension of the arrays.  This call can replace an existing warping grid
 * but not eliminate a grid.  Returns 1 for all errors.
 */
int setWarpGrid(int iz, int nxGrid, int nyGrid, float xStart, float yStart, 
                float xInterval, float yInterval, float *dxGrid, float *dyGrid, int xdim)
{
  Warping *warp;
  int ix, iy;
  if (nxGrid <= 0 || nyGrid <= 0)
    return 1;
  if (addWarpingsIfNeeded(iz))
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  warp->nxGrid = nxGrid;
  warp->nyGrid = nyGrid;
  warp->xStart  = xStart;
  warp->yStart = yStart;
  warp->xInterval = xInterval;
  warp->yInterval = yInterval;
  if (nxGrid * nyGrid > warp->maxVectors) {
    B3DFREE(warp->xVector);
    B3DFREE(warp->yVector);
    warp->xVector = B3DMALLOC(float, nxGrid * nyGrid);
    warp->yVector = B3DMALLOC(float, nxGrid * nyGrid);
    if (PipMemoryError(warp->xVector, "setWarpGrid") || 
        PipMemoryError(warp->yVector, "setWarpGrid")) {
      warp->maxVectors = 0;
      return 1;
    }
    warp->maxVectors = nxGrid * nyGrid;
  }
  for (iy = 0; iy < nyGrid; iy++) {
    for (ix = 0; ix < nxGrid; ix++) {
      warp->xVector[ix + iy * nxGrid] = dxGrid[ix + iy * xdim];
      warp->yVector[ix + iy * nxGrid] = dyGrid[ix + iy * xdim];
    }
  }
  return 0;
}

/*!
 * Sets control points for section [iz] of the current warp file.  The number of points
 * is in [nControl], their locations are in [xVector], [yVector], and the displacements
 * at the points are in [xVector], [yVector].  This call can replace an existing set of
 * control points or eliminate them.  Returns 1 for errors.
 * 
 */
int setWarpPoints(int iz, int nControl, float *xControl, float *yControl, float *xVector, 
                  float *yVector)
{
  Warping *warp;
  int i;
  if (nControl < 0)
    return 1;
  if (addWarpingsIfNeeded(iz))
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  warp->nControl = nControl;
  if (nControl > warp->maxVectors) {
    B3DFREE(warp->xVector);
    B3DFREE(warp->yVector);
    B3DFREE(warp->xControl);
    B3DFREE(warp->yControl);
    warp->xVector = warp->yVector = NULL;
    warp->xControl = warp->yControl = NULL;
    warp->maxVectors = 0;
    if (!nControl)
      return 0;
    warp->xVector = B3DMALLOC(float, nControl);
    warp->yVector = B3DMALLOC(float, nControl);
    warp->xControl = B3DMALLOC(float, nControl);
    warp->yControl = B3DMALLOC(float, nControl);
    if (PipMemoryError(warp->xVector, "setWarpPoints") || 
        PipMemoryError(warp->yVector, "setWarpPoints") ||
        PipMemoryError(warp->xControl, "setWarpPoints") || 
        PipMemoryError(warp->yControl, "setWarpPoints"))
      return 1;
    warp->maxVectors = nControl;
  }
  for (i = 0; i < nControl; i++) {
    warp->xVector[i] = xVector[i];
    warp->yVector[i] = yVector[i];
    warp->xControl[i] = xControl[i];
    warp->yControl[i] = yControl[i];
  }
  return 0;
}

/*!
 * Adds one control point specified by position [xControl], [yControl] and displacement
 * [xVector], [yVector] to section [iz] of the current warp file.  Returns -1 for no 
 * current warp file, not a control point file, [iz] out of range, or memory errors.
 */
int addWarpPoint(int iz, float xControl, float yControl, float xVector, float yVector)
{
  Warping *warp;
  int chunk = 32, newsize;
  if (addWarpingsIfNeeded(iz))
    return -1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  /*for (i = 0; i < warp->nControl; i++) 
    printf("%d %f %f %f %f\n", i, warp->xControl[i], warp->yControl[i], 
    warp->xVector[i], warp->yVector[i]); */
  if (warp->nControl + 1 > warp->maxVectors) {
    newsize = warp->nControl + chunk;
    if (warp->maxVectors) {
      B3DREALLOC(warp->xVector, float, newsize);
      B3DREALLOC(warp->yVector, float, newsize);
      B3DREALLOC(warp->xControl, float, newsize);
      B3DREALLOC(warp->yControl, float, newsize);
    } else {
      warp->xVector = B3DMALLOC(float, newsize);
      warp->yVector = B3DMALLOC(float, newsize);
      warp->xControl = B3DMALLOC(float, newsize);
      warp->yControl = B3DMALLOC(float, newsize);
    }
    if (PipMemoryError(warp->xVector, "addWarpPoint") || 
        PipMemoryError(warp->yVector, "addWarpPoint") ||
        PipMemoryError(warp->xControl, "addWarpPoint") || 
        PipMemoryError(warp->yControl, "addWarpPoint")) {
      warp->maxVectors = 0;
      return -1;
    }
    warp->maxVectors = newsize;
  }
  warp->xVector[warp->nControl] = xVector;
  warp->xControl[warp->nControl] = xControl;
  warp->yVector[warp->nControl] = yVector;
  warp->yControl[warp->nControl++] = yControl;
  return warp->nControl;
}

/*!
 * Removes the control point at [index] from section [iz] of the current warp file.
 * Returns 1 for [iz] or [index] out of range.
 */
int removeWarpPoint(int iz, int index)
{
  Warping *warp;
  int i;
  if (!sCurWarpFile || iz < 0 || iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  if (index < 0 || index >= warp->nControl)
    return 1;
  for (i = index + 1; i < warp->nControl; i++) {
    warp->xControl[i-1] = warp->xControl[i];
    warp->yControl[i-1] = warp->yControl[i];
    warp->xVector[i-1] = warp->xVector[i];
    warp->yVector[i-1] = warp->yVector[i];
  }
  warp->nControl--;
  return 0;
}


/*!
 * Returns the linear transform for section [iz] of the current warp file into [xform],
 * which has [rows] rows (2 or 3).  Returns 1 for [iz] out of range or no warp file.
 * When calling from Fortran, omit [rows]; it is assumed to be 2.
 */
int getLinearTransform(int iz, float *xform, int rows)
{
  Warping *warp;
  if (!sCurWarpFile || iz < 0 || iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  xfCopy(warp->xform, 2, xform, rows);
  return 0;
}

/*!
 * Returns in [nControl] either the number of warp points on section [iz] if [iz] >= 0,
 * or the maximum number of points over all sections, if [iz] < 0.  Returns 1 if there
 * is no current warp file or [iz] is out of range.
 */
int getNumWarpPoints(int iz, int *nControl)
{
  int izst, iznd;
  Warping *warp;
  if (!sCurWarpFile || iz >= sCurWarpFile->numFrames)
    return 1;
  *nControl = 0;
  izst = iznd = iz;
  if (iz < 0) {
    izst = 0;
    iznd = sCurWarpFile->numFrames - 1;
  }
  for (iz = izst; iz <= iznd; iz++) {
    warp = ilistItem(sCurWarpFile->warpings, iz);
    *nControl = B3DMAX(*nControl, warp->nControl);
  }
  return 0;
}

/*!
 * Returns control points for section [iz] in the current warp file, with positions placed
 * into [xControl], [yControl] and displacements placed into [xVector], [yVector].
 * Returns 1 for [iz] out if range or no warp file.
 */
int getWarpPoints(int iz, float *xControl, float *yControl, float *xVector,
                  float *yVector)
{
  Warping *warp;
  int i;
  
  if (!sCurWarpFile || iz < 0 || iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  for (i = 0; i < warp->nControl; i++) {
    xVector[i] = warp->xVector[i];
    yVector[i] = warp->yVector[i];
    xControl[i] = warp->xControl[i];
    yControl[i] = warp->yControl[i];
  }
  return 0;
}    

/*!
 * Returns pointers to control point arrays for section [iz] in the current warp file, 
 * with positions pointers placed into [xControl], [yControl] and displacement pointers
 * placed into [xVector], [yVector].  Returns 1 for [iz] out if range or no warp file.
 */
int getWarpPointArrays(int iz, float **xControl, float **yControl, float **xVector,
                  float **yVector)
{
  Warping *warp;
  
  if (!sCurWarpFile || iz < 0 || iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  *xControl = &warp->xControl[0];
  *yControl = &warp->yControl[0];
  *xVector = &warp->xVector[0];
  *yVector = &warp->yVector[0];
  return 0;
}

/*!
 * Returns the size of the warping grid for section [iz] of the current warp file, or the 
 * maximum size of all grids in the file if [iz] < 0.  Returns the size in X and Y into
 * [nxMax], [nyMax], and the total number of elements in the grid in [prodMax].  Returns
 * 1 for [iz] out of range or no warping file.
 */
int getWarpGridSize(int iz, int *nxMax, int *nyMax, int *prodMax)
{
  int izst, iznd;
  Warping *warp;
  if (!sCurWarpFile || iz >= sCurWarpFile->numFrames)
    return 1;
  *nxMax = *nyMax = *prodMax = 0;
  izst = iznd = iz;
  if (iz < 0) {
    izst = 0;
    iznd = sCurWarpFile->numFrames - 1;
  }
  for (iz = izst; iz <= iznd; iz++) {
    warp = ilistItem(sCurWarpFile->warpings, iz);
    *nxMax = B3DMAX(*nxMax, warp->nxGrid);
    *nyMax = B3DMAX(*nyMax, warp->nyGrid);
    *prodMax = B3DMAX(*prodMax, warp->nxGrid * warp->nyGrid);
  }
  return 0;
}

/*!
 * Returns all positional parameters of the warping grid for section [iz] of the current 
 * warp file: number of elements in [nxGrid] and [nyGrid], starting coordinate in
 * [xStart], [yStart], and spacing between points in [xInterval] and [yInterval].
 * Returns 1 for no warp file or [iz] out of range.
 */
int getGridParameters(int iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart,
                        float *xInterval, float *yInterval)
{
  Warping *warp;
  if (!sCurWarpFile || iz >= sCurWarpFile->numFrames || iz < 0)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  *nxGrid = warp->nxGrid;
  *nyGrid = warp->nyGrid;
  *xStart = warp->xStart;
  *yStart = warp->yStart;
  *xInterval = warp->xInterval;
  *yInterval = warp->yInterval;
  return 0;
}

/*!
 * Sets the parameters for a grid to be interpolated from a set of control points for
 * section [iz] of the current warp file.  [nxGrid], [nyGrid] set the number of elements
 * in X and Y, [xStart], [yStart] set the starting position, and [xInterval],
 * [yInterval] set the intervals between points.  Returns 1 if [iz] is out of range,
 * there is no warping file, or the warping file does not contain control points.
 */
int setGridSizeToMake(int iz, int nxGrid, int nyGrid, float xStart, float yStart, 
                      float xInterval, float yInterval)
{
  Warping *warp;
  if (!sCurWarpFile || !(sCurWarpFile->flags & WARP_CONTROL_PTS) || iz < 0 ||
      iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  warp->nxGrid = nxGrid;
  warp->nyGrid = nyGrid;
  warp->xStart = xStart;
  warp->yStart = yStart;
  warp->xInterval = xInterval;
  warp->yInterval = yInterval;
  return 0;
}

/*!
 * Returns the range of the control points for section [iz] of the current warp file,
 * with the X range in [xmin] and [xmax] and the Y range in [ymin] and [ymax].  Returns 1
 * if [iz] is out of range, there is no warping file, or the warping file does not 
 * contain control points.
 */
int controlPointRange(int iz, float *xmin, float *xmax, float *ymin, float *ymax)
{
  int i;
  Warping *warp;
  if (!sCurWarpFile || !(sCurWarpFile->flags & WARP_CONTROL_PTS) || iz < 0 || 
      iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  if (!warp->nControl)
    return 1;
  *xmin = *xmax = warp->xControl[0];
  *ymin = *ymax = warp->yControl[0];
  for (i = 1; i < warp->nControl; i++) {
    *xmin = B3DMIN(*xmin, warp->xControl[i]);
    *ymin = B3DMIN(*ymin, warp->yControl[i]);
    *xmax = B3DMAX(*xmax, warp->xControl[i]);
    *ymax = B3DMAX(*ymax, warp->yControl[i]);
  }
  return 0;
}

/*!
 * Computes a spacing between the control points on section [iz] of the current warp file.
 * Finds the distance to the nearest neighboring point for each point, then finds the
 * percentile point of this distribution requested in [percentile] (a value between 0 
 * and 1), and returns that distance in [spacing].  If [percentile] is < 0, a value of 
 * 0.5 will be used to obtain the median.  Returns 1 for no warp file, a warp
 * file not containing control points, [iz] or [percentile] out of range, or fewer than
 * 2 control points.
 */
int controlPointSpacing(int iz, float percentile, float *spacing)
{
  int i, j, sel, match;
  float *nearest;
  float minsq, dx, dy, dxsq, dysq;
  Warping *warp;
  if (percentile < 0)
    percentile = DEFAULT_PERCENTILE;
  if (!sCurWarpFile || !(sCurWarpFile->flags & WARP_CONTROL_PTS) || iz < 0 || 
      iz >= sCurWarpFile->numFrames || percentile > 1.0)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  if (warp->nControl < 2)
    return 1;
  match = pointsMatchStatArray(warp);
  if (match && iz == sLastZforSpacing && fabs((double)percentile - sLastPercentile) < 
      1.e-6) {
    *spacing = sLastSpacing;
    return 0;
  }

  nearest = B3DMALLOC(float, warp->nControl);
  if (PipMemoryError(nearest, "controlPointSpacing"))
    return 1;
  for (i = 0; i < warp->nControl; i++) {
    minsq = 1.e30;
    for (j = 0; j < warp->nControl; j++) {
      if (i == j)
        continue;
      dx = warp->xControl[i] - warp->xControl[j];
      dxsq = dx * dx;
      if (dxsq < minsq) {
        dy = warp->yControl[i] - warp->yControl[j];
        dysq = dy * dy;
        if (dysq + dxsq < minsq)
          minsq = dxsq + dysq;
      }
    }
    nearest[i] = minsq;
  }
  sel = B3DNINT(percentile * warp->nControl);
  sel = B3DMIN(warp->nControl - 1, sel);
  rsSortFloats(nearest, warp->nControl);
  *spacing = (float)sqrt((double)(nearest[sel]));
  free(nearest);
  if (match) {
    sLastZforSpacing = iz;
    sLastPercentile = percentile;
    sLastSpacing = *spacing;
  } else
    sLastZforSpacing = -1;
  return 0;
}

/*!
 * Sets the grid size for an interpolated grid from the spacing between control points
 * for section [iz] of the current warp file.  Calls @controlPointSpacing to find the
 * given [percentile] value of the spacing between nearest neighboring control points, and
 * multiplies this spacing by the value in [factor] to get the interval of the grid.
 * If [percentile] is < 0, a value of * 0.5 will be used to obtain the median.  If 
 * [factor] <0, a default value of 0.25 will be used.  The grid is set up over the range 
 * of the control points unless [fullExtent] is nonzero, * in which case it is set up 
 * over the full extent of the image.  Returns 1 for an error:
 * [iz] out of range, no current warp file or no control points, 
 * [percentile] > 1, or [factor] is not < 0 or between 0.05 and 2.
 */
int gridSizeFromSpacing(int iz, float percentile, float factor, int fullExtent)
{
  Warping *warp;
  float spacing, xmin, xmax, ymin, ymax;
  if (factor < 0)
    factor = DEFAULT_FACTOR;
  if (controlPointSpacing(iz, percentile, &spacing) || factor > 2. || factor < 0.05)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  spacing *= factor;
  if (fullExtent) {
    xmin = spacing / 10.;
    xmax = sCurWarpFile->nx - xmin;
    ymin = spacing / 10.;
    ymax = sCurWarpFile->ny - ymin;
  } else
    controlPointRange(iz, &xmin, &xmax, &ymin, &ymax);

  warp->nxGrid = (int)ceil(1 + (xmax - xmin) / spacing);
  warp->nxGrid = B3DMAX(2, warp->nxGrid);
  warp->xInterval = (xmax - xmin) / (warp->nxGrid - 1);
  warp->nyGrid = (int)ceil(1 + (ymax - ymin) / spacing);
  warp->nyGrid = B3DMAX(2, warp->nyGrid);
  warp->yInterval = (ymax - ymin) / (warp->nyGrid - 1);
  warp->xStart = xmin;
  warp->yStart = ymin;
  return 0;
}


static void freeStaticArrays()
{
  B3DFREE(sDpoints);
  B3DFREE(sGridx);
  B3DFREE(sGridy);
  B3DFREE(sSolved);
  if (sDelau)
    delaunay_destroy(sDelau);
  sDelau = NULL;
  if (sNninterp)
    nnai_destroy(sNninterp);
  sNninterp = NULL;
  sLastZforSpacing = -1;
}

static int pointsMatchStatArray(Warping *warp)
{
  int ix;
  if (!sDpoints || sLastNumCont != warp->nControl)
    return 0;
  for (ix = 0; ix < warp->nControl; ix++)
    if (fabs(sDpoints[ix].x - warp->xControl[ix]) > 1.e-3 || 
        fabs(sDpoints[ix].y - warp->yControl[ix]) > 1.e-3)
      return 0;
  return 1;  
}

/*!
 * Returns a warping grid for section [iz] of the current warp file.  The size of the
 * grid is returned into [nxGrid], [nyGrid], the starting position in [xStart], 
 * [yStart], and the intervals in [xInterval], [yInterval].  The vectors are returned 
 * into the 2D arrays [dxGrid], [dyGrid] using the X dimension given in [xdim].  If
 * [xdim] is <= 0, it will be set to [nxGrid] and data will be packed sequentially. For a 
 * file of warping grids, the grid is returned directly; for a file of control points,
 * the grid is found by interpolation.  Returns 1 for error if [iz] is out of range, there
 * is no warping file, or, for a file of control points, the grid parameters are not
 * defined, there are fewer than 3 control points, or a memory allocation fails.
 */
int getWarpGrid(int iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart, 
                float *xInterval, float *yInterval, float *dxGrid, float *dyGrid, 
                int xdim)
{
  int ix, iy, ngrid, nsolve, nxSolve, nySolve, offXsolve, offYsolve, last, ind, ind2;
  int needNew = 1;
  Warping *warp;
  double *dxyin, *dxyout;
  double pruneCrit[2];
  float xmin, xmax, ymin, ymax;
  double wallStart, wallTop = wallTime();
  pruneCrit[0] = sHeightBaseCrit;
  pruneCrit[1] = sAreaFractionCrit;
  
  if (!sCurWarpFile || iz < 0 || iz >= sCurWarpFile->numFrames)
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);
  if (!warp->nxGrid || !warp->nyGrid)
    return 1;
  if (xdim <= 0)
    xdim = warp->nxGrid;
  if (!(sCurWarpFile->flags & WARP_CONTROL_PTS)) {

    /* Return existing grid */
    for (iy = 0; iy < warp->nyGrid; iy++) {
      for (ix = 0; ix < warp->nxGrid; ix++) {
        dxGrid[ix + iy * xdim] = warp->xVector[ix + iy * warp->nxGrid];
        dyGrid[ix + iy * xdim] = warp->yVector[ix + iy * warp->nxGrid];
      }
    }
  } else {

    /* Return grid based on control points */
    if (warp->nControl < 3)
      return 1;

    /* See if the points and grid match previous analysis */
    if (sDpoints && sGridx && sGridy && sSolved && sDelau && sNninterp && 
        sLastNumCont == warp->nControl && warp->nxGrid == sLastNxGrid && 
        warp->nyGrid == sLastNyGrid && warp->xStart == sLastXStart &&
        warp->yStart == sLastYStart && warp->xInterval == sLastXinterval &&
        warp->yInterval == sLastYinterval && xdim == sLastXdim) {
      needNew = 0;
      if (!pointsMatchStatArray(warp))
        needNew = 1;
    }

    /* Determine size of grid within the range */
    ngrid = xdim * warp->nyGrid;
    controlPointRange(iz, &xmin, &xmax, &ymin, &ymax);
    offXsolve = (int)ceil((xmin - 0.1 - warp->xStart) / warp->xInterval);
    offXsolve = B3DMAX(0, offXsolve);
    last = (int)floor((xmax + 0.1 - warp->xStart) / warp->xInterval);
    last = B3DMIN(warp->nxGrid - 1, last);
    nxSolve = last + 1 - offXsolve;
    offYsolve = (int)ceil((ymin - 0.1 - warp->yStart) / warp->yInterval);
    offYsolve = B3DMAX(0, offYsolve);
    last = (int)floor((ymax + 0.1 - warp->yStart) / warp->yInterval);
    last = B3DMIN(warp->nyGrid - 1, last);
    nySolve = last + 1 - offYsolve;
    nsolve = nxSolve * nySolve;
    if (!nsolve)
      return 1;

    nn_verbose = 0;
    /*printf("grid  %d %d   solve %d %d offset %d %d\n", warp->nxGrid, warp->nyGrid,
      nxSolve, nySolve, offXsolve, offYsolve); */

    if (needNew) {
      freeStaticArrays();
      sDpoints = B3DMALLOC(point, warp->nControl);
      sGridx = B3DMALLOC(double, nsolve);
      sGridy = B3DMALLOC(double, nsolve);
      sSolved = B3DMALLOC(char, 6 * ngrid);
    }
    dxyout = B3DMALLOC(double, nsolve);
    dxyin = B3DMALLOC(double, warp->nControl);
    
    if (PipMemoryError(sDpoints, "getWarpGrid") || PipMemoryError(sGridx, "getWarpGrid")||
        PipMemoryError(sGridy, "getWarpGrid") || PipMemoryError(dxyin, "getWarpGrid")
        || PipMemoryError(dxyout, "getWarpGrid")) {
      freeStaticArrays();
      B3DFREE(dxyin);
      B3DFREE(dxyout);
      return 1;
    }
    sLastNumCont = warp->nControl;
    sLastNxGrid = warp->nxGrid;
    sLastNyGrid = warp->nyGrid;
    sLastXStart = warp->xStart;
    sLastYStart = warp->yStart;
    sLastXinterval = warp->xInterval;
    sLastYinterval = warp->yInterval;
    sLastXdim = xdim;

    /* Zero out the whole grid in case extrapolation fails and to keep valgrind happy */
    for (ix = 0; ix < ngrid; ix++)
      dxGrid[ix] = dyGrid[ix] = 0.;

    /* Load the control points and the grid of points to solve at */
    if (needNew) {
      for (ix = 0; ix < warp->nControl; ix++) {
        sDpoints[ix].x = warp->xControl[ix];
        sDpoints[ix].y = warp->yControl[ix];
        sDpoints[ix].z = 0.;
      }
      for (iy = 0; iy < nySolve; iy++) {
        for (ix = 0; ix < nxSolve; ix++) {
          sGridx[ix + iy * nxSolve] = warp->xStart + (ix + offXsolve) * warp->xInterval;
          sGridy[ix + iy * nxSolve] = warp->yStart + (iy + offYsolve) * warp->yInterval;
        }
      }
    }

    if (needNew) {
      wallStart = wallTime();
      sDelau = delaunay_build(warp->nControl, sDpoints, 0, NULL, sMinNumForPruning,
                              pruneCrit);
      if (sTimeOutput)
        printf("Delaunay time %.3f\n", 1000. * ( wallTime() - wallStart));
      wallStart = wallTime();
      if (sDelau)
        sNninterp = nnai_build(sDelau, nsolve, sGridx, sGridy);
      if (!sNninterp) {
        freeStaticArrays();
        free(dxyin);
        free(dxyout);
        return 1;
      }
      if (sTimeOutput)
        printf("nnai_build time %.3f\n", 1000. * ( wallTime() - wallStart));
    }
    wallStart = wallTime();
        
    /* This is needed to ensure Nan's outside the convex hull */
    nnai_setwmin(sNninterp, 0);

    /* Interpolate DX values and store them in dxGrid, set solved array */
    for (ix = 0; ix < warp->nControl; ix++)
      dxyin[ix] = warp->xVector[ix];
    nnai_interpolate(sNninterp, dxyin, dxyout);
    for (ix = 0; ix < ngrid; ix++)
      sSolved[ix] = 0;
    for (iy = 0; iy < nySolve; iy++) {
      for (ix = 0; ix < nxSolve; ix++) {
        ind = ix + iy * nxSolve;

        /* config.h defines isnan as _isnan for WIN32 */
        if (!isnan(dxyout[ind])) {
          ind2 = ix + offXsolve + (iy + offYsolve) * xdim;
          dxGrid[ind2] = dxyout[ind];
          sSolved[ind2] = 1;
          /* printf("%d %d dX %f\n", ix+ offXsolve,iy+ offYsolve, dxyout[ind]); */
        }
      }
    }

    /* Load and interpolate DY values and store them in dyGrid */
    for (ix = 0; ix < warp->nControl; ix++)
      dxyin[ix] = warp->yVector[ix];
    nnai_interpolate(sNninterp, dxyin, dxyout);
    if (sTimeOutput)
      printf("nnai_interpolate time %.3f\n", 1000. * (wallTime() - wallStart));
    wallStart = wallTime();
    for (iy = 0; iy < nySolve; iy++) {
      for (ix = 0; ix < nxSolve; ix++) {
        ind2 = ix + offXsolve + (iy + offYsolve) * xdim;
        if (sSolved[ind2]) {
          dyGrid[ind2] = dxyout[ix + iy * nxSolve];
          /* printf("%d %d dY %f\n", ix+ offXsolve,iy+ offYsolve, 
             dxyout[ix + iy * nxSolve]);*/
        }
      }
    }

    /* Fill in outside the hull. If it fails, mark as non-resumable */
    if (extrapolateGrid(dxGrid, dyGrid, sSolved, xdim, warp->nxGrid, warp->nyGrid, 
                        warp->xInterval, warp->yInterval, 1 - needNew))
      sLastNumCont = 0;
    if (sTimeOutput)
      printf("extrapolate time %.3f\n", 1000. * (wallTime() - wallStart));

#ifdef DO_DUMPS
    dumpControls(warp->xControl, warp->yControl, warp->xVector, warp->yVector, 
                 warp->nControl, "controlPts.patch");
    dumpGrid(dxGrid, dyGrid, sSolved, xdim, warp->nxGrid, warp->nyGrid, warp->xStart,
             warp->xStart, warp->xInterval, warp->yInterval, 1, "interp.patch");
    dumpGrid(dxGrid, dyGrid, sSolved, xdim, warp->nxGrid, warp->nyGrid, warp->xStart,
             warp->xStart, warp->xInterval, warp->yInterval, 0, "extrap.patch");
#endif
    free(dxyin);
    free(dxyout);
  }
  
  /* Return grid parameters in either case */
  *nxGrid = warp->nxGrid;
  *nyGrid = warp->nyGrid;
  *xStart = warp->xStart;
  *yStart = warp->yStart;
  *xInterval = warp->xInterval;
  *yInterval = warp->yInterval;
  if (sTimeOutput)
    printf("Total time in getWarpGrid %.3f\n", 1000. * (wallTime() - wallTop));
  return 0;
}

#ifdef DO_DUMPS
static void dumpGrid(float *dxGrid, float *dyGrid, char *solved, int xdim,
                          int nxGrid, int nyGrid, float xStart, float yStart, 
                          float xInterval, float yInterval, int doSolved, 
                          const char *fname)
{
  int num = 0, ix, iy, ind;
  FILE *fp = fopen(fname, "w");

  if (!fp)
    return;
  for (iy = 0; iy < nyGrid; iy++)
    for (ix = 0; ix < nxGrid; ix++)
      if (solved[ix + iy * xdim] == doSolved)
        num++;

  fprintf(fp, "%d residuals\n", num);
  for (iy = 0; iy < nyGrid; iy++) {
    for (ix = 0; ix < nxGrid; ix++) {
      ind = ix + iy * xdim;
      if (solved[ind] == doSolved) {
        fprintf(fp, "%.2f  %.2f  0  %.2f  %.2f\n", xStart + ix * xInterval,
                yStart + iy * yInterval, dxGrid[ind], dyGrid[ind]);
      }
    }
  }
  fclose(fp);
}

static void dumpControls(float *xControl, float *yControl, float *xVector, float *yVector,
                    int nControl, const char *fname)
{
  int i;
  FILE *fp = fopen(fname, "w");

  if (!fp)
    return;
  fprintf(fp, "%d residuals\n", nControl);
  for (i = 0; i < nControl; i++)
    fprintf(fp, "%.2f  %.2f  0  %.2f  %.2f\n", xControl[i], yControl[i], xVector[i],
            yVector[i]);
    
  fclose(fp);
}
#endif

/*!
 * Extracts the linear transform embedded in the warping for section [iz] of the current
 * warp file, modifies the vectors to remove this transform, and combines the transform
 * with the linear transform for this section.  Returns 1 for no warp file, section out of
 * range, not an inverse warp file, fewer than 3 control or grid points, or a memory 
 * error.
 */
int separateLinearTransform(int iz)
{
  float *xPoint = NULL, *yPoint = NULL;
  int ix, iy, nPoints;
  float xfinv[6];
  Warping *warp;

  if (!sCurWarpFile || iz < 0 || iz >= sCurWarpFile->numFrames || 
      !(sCurWarpFile->flags & WARP_INVERSE))
    return 1;
  warp = ilistItem(sCurWarpFile->warpings, iz);

  /* For control points, set the pointers */
  if (sCurWarpFile->flags & WARP_CONTROL_PTS) {
    xPoint = warp->xControl;
    yPoint = warp->yControl;
    if (warp->nControl < 3)
      return 1;
    nPoints = warp->nControl;
  } else {

    /* For a grid, create arrays and load them with the points */
    nPoints = warp->nxGrid * warp->nyGrid;
    if (nPoints < 3)
      return 1;
    xPoint = B3DMALLOC(float, nPoints);
    yPoint = B3DMALLOC(float, nPoints);
    if (!xPoint || !yPoint) {
      B3DFREE(xPoint);
      B3DFREE(yPoint);
      return 1;
    }
    for (iy = 0; iy < warp->nyGrid; iy++) {
      for (ix = 0; ix < warp->nxGrid; ix++) {
        xPoint[ix + iy * warp->nxGrid] = warp->xStart + ix * warp->xInterval;
        yPoint[ix + iy * warp->nxGrid] = warp->yStart + iy * warp->yInterval;
      }
    }
  }

  ix = extractLinearXform(xPoint, yPoint, warp->xVector, warp->yVector, nPoints,
                          sCurWarpFile->nx / 2., sCurWarpFile->ny / 2., warp->xVector, 
                          warp->yVector, xfinv, 2);
  if (!ix) {
    xfMult(warp->xform, xfinv, warp->xform, 2);
  }
  if (!(sCurWarpFile->flags & WARP_CONTROL_PTS)) {
    B3DFREE(xPoint);
    B3DFREE(yPoint);
  }
  return ix;
}

/* Reads a line of values as ints or floats */
static int readLineOfValues(FILE *fp, char *line, int limit, void *values, int floats, 
                     int *numToGet, int maxVals)
{
  int *iarray = (int *)values;
  float *farray = (float *)values;
  int length, numGot = 0;
  char *endPtr, *token;
  char *str = line;
  while (1) {
    length = fgetline(fp, line, limit);
    if (!length)
      continue;
    if (length == -1 || length == -2)
      return length;
    while ((token = strtok(str, ", \t")) != NULL) {
      str = NULL;
      if (!token)
        break;
      if (numGot >= maxVals)
        return -3;
      if (floats)
        farray[numGot++] = (float)strtod(token, &endPtr);
      else
        iarray[numGot++] = strtol(token, &endPtr, 10);
      if (*endPtr != 0x00)
        return -4;
    }    
    if (!numGot)
      continue;

    if (!*numToGet)
      *numToGet = numGot;
    if (numGot < *numToGet)
      return -5;
    return length < 0 ? 1 : 0;
  }
}

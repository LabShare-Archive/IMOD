/*  warpwrapfort.c - Fortran wrappers for warp file and utility functions
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include "b3dutil.h"
#include "warpfiles.h"
#include "imodconfig.h"

/* NOTE that all wrappers subtract 1 from iz and omit rows arguments */

#ifdef F77FUNCAP
#define newwarpfile NEWWARPFILE
#define setcurrentwarpfile SETCURRENTWARPFILE
#define clearwarpfile CLEARWARPFILE
#define warpfilesdone WARPFILESDONE
#define setlineartransform SETLINEARTRANSFORM
#define setwarpgrid SETWARPGRID
#define setwarppoints SETWARPPOINTS
#define getlineartransform GETLINEARTRANSFORM
#define getnumwarppoints GETNUMWARPPOINTS
#define getwarppoints GETWARPPOINTS
#define getwarpgridsize GETWARPGRIDSIZE
#define setgridsizetomake SETGRIDSIZETOMAKE
#define controlpointrange CONTROLPOINTRANGE
#define controlpointspacing CONTROLPOINTSPACING
#define gridsizefromspacing GRIDSIZEFROMSPACING
#define getgridparameters GETGRIDPARAMETERS
#define getwarpgrid GETWARPGRID
#define separatelineartransform SEPARATELINEARTRANSFORM
#define readwarpfile READWARPFILE
#define writewarpfile WRITEWARPFILE
#define interpolategridf INTERPOLATEGRIDF
#define findinversepoint FINDINVERSEPOINT
#define invertwarpgrid INVERTWARPGRID
#define multiplywarpings MULTIPLYWARPINGS
#else
#define newwarpfile newwarpfile_
#define setcurrentwarpfile setcurrentwarpfile_
#define clearwarpfile clearwarpfile_
#define warpfilesdone warpfilesdone_
#define setlineartransform setlineartransform_
#define setwarpgrid setwarpgrid_
#define setwarppoints setwarppoints_
#define getlineartransform getlineartransform_
#define getnumwarppoints getnumwarppoints_
#define getwarppoints getwarppoints_
#define getwarpgridsize getwarpgridsize_
#define setgridsizetomake setgridsizetomake_
#define controlpointrange controlpointrange_
#define controlpointspacing controlpointspacing_
#define gridsizefromspacing gridsizefromspacing_
#define getgridparameters getgridparameters_
#define getwarpgrid getwarpgrid_
#define separatelineartransform separatelineartransform_
#define readwarpfile readwarpfile_
#define writewarpfile writewarpfile_
#define interpolategridf interpolategridf_
#define findinversepoint findinversepoint_
#define invertwarpgrid invertwarpgrid_
#define multiplywarpings multiplywarpings_
#endif


int newwarpfile(int *nx, int *ny, int *binning, float *pixelSize, int *flags)
{
  return newWarpFile(*nx, *ny, *binning, *pixelSize, *flags);
}

int setcurrentwarpfile(int *index)
{
  return setCurrentWarpFile(*index);
}

int clearwarpfile(int *index)
{
  return clearWarpFile(*index);
}

void warpfilesdone()
{
  warpFilesDone();
}

int setlineartransform(int *iz, float *xform)
{
  return setLinearTransform(*iz-1, xform, 2);
}

int setwarpgrid(int *iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart,
                  float *xInterval, float *yInterval, float *dxGrid, float *dyGrid,
                  int *xdim)
{
  return setWarpGrid(*iz-1, *nxGrid, *nyGrid, *xStart, *yStart, *xInterval, *yInterval, 
                     dxGrid, dyGrid, *xdim);
}

int setwarppoints(int *iz, int *nControl, float *xControl, float *yControl, 
                    float *xVector, float *yVector)
{
  return setWarpPoints(*iz-1, *nControl, xControl, yControl, 
                    xVector, yVector);
}

int getlineartransform(int *iz, float *xform)
{
  return getLinearTransform(*iz-1, xform, 2);
}

int getnumwarppoints(int *iz, int *nControl)
{
  return getNumWarpPoints(*iz-1, nControl);
}

int getwarppoints(int *iz, float *xControl, float *yControl, float *xVector,
                    float *yVector)
{
  return getWarpPoints(*iz-1, xControl, yControl, xVector,
                    yVector);
}

int getwarpgridsize(int *iz, int *nxMax, int *nyMax, int *prodMax)
{
  return getWarpGridSize(*iz-1, nxMax, nyMax, prodMax);
}

int setgridsizetomake(int *iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart, 
                        float *xInterval, float *yInterval)
{
  return setGridSizeToMake(*iz-1, *nxGrid, *nyGrid, *xStart, *yStart, 
                        *xInterval, *yInterval);
}

int controlpointrange(int *iz, float *xmin, float *xmax, float *ymin, float *ymax)
{
  return controlPointRange(*iz-1, xmin, xmax, ymin, ymax);
}

int controlpointspacing(int *iz, float *percentile, float *spacing)
{
  return controlPointSpacing(*iz-1, *percentile, spacing);
}

int gridsizefromspacing(int *iz, float *percentile, float *factor, int *fullExtent)
{
  return gridSizeFromSpacing(*iz-1, *percentile, *factor, *fullExtent);
}

int getgridparameters(int *iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart,
                        float *xInterval, float *yInterval)
{
  return getGridParameters(*iz-1, nxGrid, nyGrid, xStart, yStart,
                        xInterval, yInterval);
}

int getwarpgrid(int *iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart, 
                  float *xInterval, float *yInterval, float *dxGrid, float *dyGrid, 
                  int *xdim)
{
  return getWarpGrid(*iz-1, nxGrid, nyGrid, xStart, yStart, xInterval, yInterval,
                     dxGrid, dyGrid, *xdim);
}

int separatelineartransform(int *iz)
{
  return separateLinearTransform(*iz-1);
}

int readwarpfile(char *filename, int *nx, int *ny, int *nz, int *binning, 
                 float *pixelSize, int *version, int *flags, int namelen)
{
  int err;
  char *cname = f2cString(filename, namelen);
  if (!cname)
    return 1;
  err = readWarpFile(cname, nx, ny, nz, binning, pixelSize, version, flags);
  free(cname);
  return err;
}

int writewarpfile(const char *filename, int *skipBackup, int namelen)
{  int err;
  char *cname = f2cString(filename, namelen);
  if (!cname)
    return 1;
  err = writeWarpFile(cname, *skipBackup);
  free(cname);
  return err;
}  


void interpolategridf(float *x, float *y, float *dxGrid, float *dyGrid, int *ixgDim,
                     int *nxGrid, int *nyGrid, float *xGridStart, float *xGridIntrv,
                     float *yGridStart, float *yGridIntrv, float *dx, float *dy)
{
  interpolateGridf(*x, *y, dxGrid, dyGrid, *ixgDim, *nxGrid, *nyGrid, *xGridStart,
                   *xGridIntrv, *yGridStart, *yGridIntrv, dx, dy);
}

void findinversepoint(float *x, float *y, float *dxGrid, float *dyGrid, int *ixgDim,
                      int *nxGrid, int *nyGrid, float *xGridStart, float *xGridIntrv,
                      float *yGridStart, float *yGridIntrv, float *xnew, float *ynew,
                      float *dx, float *dy)
{
  findInversePoint(*x, *y, dxGrid, dyGrid, *ixgDim, *nxGrid, *nyGrid, *xGridStart, 
                   *xGridIntrv, *yGridStart, *yGridIntrv, xnew, ynew, dx, dy);
}

void invertwarpgrid(float *dxGrid, float *dyGrid, int *ixgDim, int *nxGrid, int *nyGrid,
                    float *xGridStart, float *xGridIntrv, float *yGridStart, 
                    float *yGridIntrv, float *xform, float *xcen, float *ycen,
                    float *dxInv, float *dyInv, float *xfInv)
{
  invertWarpGrid(dxGrid, dyGrid, *ixgDim, *nxGrid, *nyGrid, *xGridStart, *xGridIntrv, 
                 *yGridStart, *yGridIntrv, xform, *xcen, *ycen, dxInv, dyInv, xfInv, 2);
}

int multiplywarpings(float *dxGrid1, float *dyGrid1, int *ixgDim1, int *nxGrid1, 
                      int *nyGrid1, float *xStart1, float *xIntrv1, float *yStart1,
                      float *yIntrv1, float *xform1, float *xcen, float *ycen, 
                      float *dxGrid2, float *dyGrid2, int *ixgDim2, int *nxGrid2,
                      int *nyGrid2, float *xStart2, float *xIntrv2, 
                      float *yStart2, float *yIntrv2, float *xform2, float *dxProd,
                      float *dyProd, float *xfProd, int *useSecond)
{
  return multiplyWarpings
    (dxGrid1, dyGrid1, *ixgDim1, *nxGrid1, *nyGrid1, *xStart1, *xIntrv1, *yStart1,
     *yIntrv1, xform1, *xcen, *ycen, dxGrid2, dyGrid2, *ixgDim2, *nxGrid2, *nyGrid2, 
     *xStart2, *xIntrv2, *yStart2, *yIntrv2, xform2, dxProd, dyProd, xfProd, *useSecond,
     2);
}



/* maggradfield.c - contains routines for computing distortions due to mag gradients
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <math.h>
#include <stdio.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "warpfiles.h"
     
/*!
 * Makes a distortion field array with the shifts based on a mag gradient.  ^
 * [gradDx], [gradDy] are the arrays returned with the X and Y shifts at each location ^
 * [idfDx], [idfDy] are arrays used for temporary storage ^
 * All of these are dimensioned [lmGrid] x [lmGrid] ^
 * [imageNx] and [imageNy] are the image size that gradients are being applied to. ^
 * [xGridStrt], [yGridStrt] are coordinates at which the grid starts  ^
 * [xGridIntrv], [yGridIntrv] are the spacing between grid points in X & Y ^
 * [nxGrid], [nyGrid] are the number of grid points in X & Y ^
 * [xcen], [ycen] is the center coordinate for the mag and rotation changes ^
 * [pixelSize] is the pixel size in Angstroms ^
 * [axisRot] is the rotation of the tilt axis from the vertical ^
 * [tilt] is the tilt angle ^
 * [dmagPerUm] is the percent change in magnification per micron of Z ^
 * [rotPerUm] is the rotation in degrees per micron of Z height
 */
void makeMagGradField(float *idfDx, float *idfDy, float *gradDx, float *gradDy, 
                      int lmGrid, int imageNx, int imageNy, int *nxGrid, int *nyGrid,
                      float *xGridStrt, float *yGridStrt, float *xGridIntrv, 
                      float *yGridIntrv, float xcen, float ycen, float pixelSize,
                      float axisRot, float tilt, float dmagPerUm, float rotPerUm)
{
  int i;
     
  /* Set up a grid that has maximum resolution for the array size */
  *nxGrid = lmGrid;
  *nyGrid = lmGrid;
  *xGridStrt = 1.;
  *yGridStrt = 1.;
  *xGridIntrv = (imageNx - 1.) / (lmGrid - 1.);
  *yGridIntrv = (imageNy - 1.) / (lmGrid - 1.);
  for (i = 0; i < lmGrid * lmGrid; i++) 
    idfDx[i] = idfDy[i] = 0.;
  addMagGradField(idfDx, idfDy, gradDx, gradDy, lmGrid, imageNx, imageNy, *nxGrid,
                  *nyGrid, *xGridStrt, *yGridStrt, *xGridIntrv, *yGridIntrv, xcen, ycen,
                  pixelSize, axisRot, tilt, dmagPerUm, rotPerUm);
}

/*
 * Adds the distortions from mag gradients to those in an existing distortion field.  
 * The arguments are identical to @makeMagGradField, except that 
 * [idfDx], [idfDy] are arrays with an existing image distortion field.
 */
void addMagGradField(float *idfDx, float *idfDy, float *gradDx, float *gradDy, 
                     int lmGrid, int imageNx, int imageNy, int nxGrid, int nyGrid,
                     float xGridStrt, float yGridStrt, float xGridIntrv, 
                     float yGridIntrv, float xcen, float ycen, float pixelSize,
                     float axisRot, float tilt, float dmagPerUm, float rotPerUm)
{

  int ix, iy;
  float dx, dy, xx, yy, dx2, dy2;

  for (ix = 0; ix < nxGrid; ix++) {
    for (iy = 0; iy < nyGrid; iy++) {
      xx = xGridStrt + ix * xGridIntrv;
      yy = yGridStrt + iy * yGridIntrv;
      
      /* Get the shift due to the mag gradient, then look up the distortion field at this
         point and add the two shifts to get the total field */
      magGradientShift(xx, yy, imageNx, imageNy, xcen, ycen, pixelSize, axisRot, tilt,
                       dmagPerUm, rotPerUm, &dx, &dy);
      interpolateGrid(xx + dx, yy + dy, idfDx, idfDy, lmGrid, nxGrid, nyGrid, xGridStrt,
                       yGridStrt, xGridIntrv, yGridIntrv, &dx2, &dy2);
      gradDx[ix + iy * lmGrid] = dx + dx2;
      gradDy[ix + iy * lmGrid] = dy + dy2;
    }
  }
}

#define DTOR 0.017453293
/*!
 * Computes the shifts from a mag gradient at one point. ^
 * [xx], [yy] is the coordinate of the point ^
 * [imageNx] and [imageNy] are the image size that gradients are being applied to. ^
 * [xcen], [ycen] is the center coordinate for the mag and rotation changes ^
 * [pixelSize] is the pixel size in Angstroms ^
 * [axisRot] is the rotation of the tilt axis from the vertical ^
 * [tilt] is the tilt angle ^
 * [dmagPerUm] is the percent change in magnification per micron of Z ^
 * [rotPerUm] is the rotation in degrees per micron of Z height ^
 * [dx], [dy] are returned with the shifts
 */
void magGradientShift(float xx, float yy, int imageNx, int imageNy, float xcen, 
                      float ycen, float pixelSize, float axisRot, float tilt,
                      float dmagPerUm, float rotPerUm, float *dx, float *dy)
{
  float cosphi, sinphi, tantheta, xrel, yrel;
  float zh, sinrz, cosrz, gmag;

  /* get trig values for the rotation to axis and tilt angle */
  cosphi = cos(DTOR * axisRot) * pixelSize / 10000.;
  sinphi = sin(DTOR * axisRot) * pixelSize / 10000.;
  tantheta = tan(DTOR * tilt);
         
  /* compute the location of this point relative to the mag center and its vertical 
     height and thus rotation and mag */
  xrel = xx - xcen;
  yrel = yy - ycen;
  zh = tantheta * (xrel * cosphi + yrel * sinphi);
  sinrz = sin(DTOR * rotPerUm * zh);
  cosrz = cos(DTOR * rotPerUm * zh);
  gmag = (1. + 0.01 * dmagPerUm * zh);
     
  /* have to mag around the center of this picture so do transform relative to that to
     get dx, dy */     
  xrel = xx - imageNx / 2.;
  yrel = yy - imageNy / 2.;
  *dx = (xrel * cosrz - yrel * sinrz) * gmag + imageNx / 2. - xx;
  *dy = (xrel * sinrz + yrel * cosrz) * gmag + imageNy / 2. - yy;
}
